c
C---  Calcoli legati all'equazione di stato (z, alfa, beta, cp, viscosità..)
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
      Subroutine par_comp_rk(o,n_ogg,compos_elem)
c***********************************************************************
c SUBRoutine per il calcolo dei coefficienti dipendenti dalla composizione
c Equazione di stato Redlich-Kwong. Tutti i coefficienti rk_par(2), av_weigth,
c cp_par(4), vis_par(3) sono nel common rk.inc
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      INTEGER*4 o                         ! I) OFFSET ELEMENTO TOPOLOGIA
      INTEGER*4 n_ogg                     ! I) numero oggetti
c      Real      compos_elem(m_ogg,m_comp)  ! I) composizione elementi
      Real      compos_elem(n_ogg,m_comp)  ! I) composizione elementi
C
      integer*4 ncomp
c      parameter (ncomp = 10)        ! Adesso ncomp e m_comp sono uguali !!!!!!
      parameter (ncomp = m_comp)

      Real      compos(NCOMP) ! COMPOSIZIONE UNITAG-LIKE
c Composizione SIRE  : met eta prop but pent esa azo co2
c Composizione UNITAG: met eta prop n-but i-but pent esa azo co2 h2
c
      REAL      PMOL(NCOMP),PC(NCOMP),TC(NCOMP),ZC(NCOMP),
     *          k(NCOMP,NCOMP),aa(NCOMP),cnew(4,NCOMP)
      REAL      pca(ncomp),tca(ncomp)
      integer*4 i,j
      real      omb,oma,xij
C
C Pesi molecolari e valori critici (Pc in atm, Tc in K) -NEW_VALUE- FONTE:Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025

c      
c peso molecolare in [g/mole] -Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025 -NEW_VALUE-
      DATA PMOL / 16.0425, 30.069, 44.0956, 58.1222, 58.1222,
     *            72.1488, 86.1754, 28.0134, 44.0095, 2.01588 /
c pressione critica in [atm] - Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025 -NEW_VALUE- 
      DATA PC   / 45.391, 48.085, 41.956, 37.464, 35.815, 33.235, 
     *            30.043 , 33.514 , 72.808 , 12.794  /
c temperatura critica in [K]-Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025 -NEW_VALUE-
      DATA TC   / 190.564, 305.322, 369.89, 425.125 , 407.81, 469.7 , 
     *            507.82 , 126.192, 304.1282, 33.145 /
c z critico -American Petroleum Institute Research Project 44*/
      DATA ZC   / 0.288, 0.285, 0.276, 0.274, 0.283, 0.262, 0.273,
     *            0.289, 0.274, 0.283 /
      
      !********* -NEW_VALUE- NUOVA FONTE Parametri di interazione binaria Evolutiva 2025 ********************************************
      ! Reliable Binary Interaction Parameters for SRK EOS for										
      ! Modeling CO2 and H2 Storage in Depleted Oil and Gas										
      ! Reservoirs										
      ! April 18th, 2023										
      ! da: Optimization of Peng-Robinson and Redlich-Kwong-Soave Equations of State parameters for H2 – CH4 mixtures										
      ! da: Modeling Study of High Pressure and High Temperature Reservoir Fluids - Varzandeh, F., Yan, W., & Stenby, E. H. (2017) 
      !******************************************************************************************************************************
c---->             -NEW_VALUE-           Parametri di interazione binaria
      data K 
     * / 0.0,  0.006516,  0.016733,  0.026889,  0.026889,
     *   0.036382,  0.045145,   0.019855, 0.103355 ,  -0.156,  
     
     *   0.006516,  0.0,  0.002428, 0.007166, 0.007166,  
     *   0.012582,  0.018123,   0.048892,  0.138282,  0.141755,
     
     *   0.016733,  0.002428, 0.0,  0.001265,  0.001265, 
     *   0.00401, 0.007406,  0.068319,  0.143751,  0.327689,  
     
     *   0.026889,  0.007166, 0.001265, 0.0,  0.0,
     *   0.000775,  0.002569, 0.083621,  0.144255,  0.0,  
     
     *   0.026889,  0.007166, 0.001265, 0.0,  0.0,
     *   0.000775,  0.002569, 0.083621,  0.143412,  0.0,  
     
     *   0.036382,  0.012582, 0.00401, 0.000775,  0.000775,
     *   0.0,  0.000524, 0.098346,  0.142091,  0.0,  
       
     *   0.045145,  0.018123, 0.007406, 0.002569,  0.002569,
     *   0.000524,  0.0, 0.113608,  0.140559,  0.0,  
       
     *   0.019855,  0.048892, 0.068319, 0.083621,  0.083621,
     *   0.098346,  0.11360813, 0.0,  -0.017985,  0.084175,  
     
     *   0.103355,  0.138282, 0.143751, 0.144255,  0.143412,
     *   0.142091,  0.140559, -0.017985,  0.0,  0.144419,    
       
     *   -0.156,  0.141755, 0.327689, 0.0,  0.0,
     *   0.0,  0.0, 0.084175,  0.144419,  0.0 /

c      DATA   K / 0.0,1.0,2.3,2.9,2.9,3.9,5.0,2.5,5.0,5.0,
c     *     1.0,0.0,0.3,0.43,0.43,0.55,0.7,7.0,4.8,4.5,
c     *     2.3,0.3,0.0,0.33,0.33,0.43,0.5,10.,4.5,4.0,
c     *     2.9,0.43,0.33,0.0,0.0,0.09,0.15,11.5,5.0,3.5,
c     *     2.9,0.43,0.33,0.0,0.0,0.09,0.15,11.5,5.0,3.5,
c     *     3.9,0.55,0.43,0.09,0.09,0.0,0.0,14.,5.0,2.4,
c     *     5.0,0.7,0.5,0.15,0.15,0.0,0.0,17.2,5.0,0.0,
c     *     2.5,7.0,10.,11.5,11.5,14.,17.2,0.0,0.0,0.0,
c     *     5.0,4.8,4.5,5.0,5.0,5.0,5.0,0.0,0.0,3.5,
c     *     5.0,4.5,4.0,3.5,3.5,2.4,0.0,0.0,3.5,0.0/
c
      DATA OMA/0.42748/ ,OMB/0.08664/
C
C.....Coefficienti C1,C2,C3,C4 per ogni composto
C	(per il calcolo di CP0 MOLARE)	
C
C 	1)METANO  2)ETANO  3)PROPANO  4)N_BUTANO  5)I_BUTANO
C    6)PENTANI 7)ESANI  8) AZOTO   9) CO2     10)H2

c Sono espressi direttamente in [joule]/[mole][k] -NEW_VALUE- FONTE : Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025
C 	1-METANO (Tmin[K]=220 Tmax[K]=520)  2-ETANO (Tmin[K]=220 Tmax[K]=520)  3-PROPANO (Tmin[K]=220 Tmax[K]=520)  4-N_BUTANO (Tmin[K]=220 Tmax[K]=520)  5-I_BUTANO (Tmin[K]=220 Tmax[K]=520)
C    6-PENTANI(Tmin[K]=250 Tmax[K]=520)  7-ESANI (Tmin[K]=280 Tmax[K]=520)  8-AZOTO   (Tmin[K]=220 Tmax[K]=520)  9-CO2      (Tmin[K]=220 Tmax[K]=520) 10-H2       (Tmin[K]=220 Tmax[K]=520)
      data cnew /43.0487577556766, -1.08557885289082e-1, 
     *            3.56627240668296e-4, -2.51316977906458e-07, !metano Tmin[K]=220 Tmax[K]=520
     
     *           37.0927191345004, -5.05173141389184e-2,
     *            4.59212182838362e-4, -3.89697702320458E-07, !etano Tmin[K]=220 Tmax[K]=520
     
     *           39.128189622842 , -1.02247320723235e-2,
     *            5.79205083326973e-4, -5.33037687895075e-07, !propa Tmin[K]=220 Tmax[K]=520
     
     *           59.5734196481854, -5.5997490655966e-2,
     *            8.63199754476815e-4, -7.89574834520724e-07, !n-but Tmin[K]=220 Tmax[K]=520
     
     *           43.9844631931356,  2.42510592915273e-2,
     *            7.19890669136228e-4, -6.94438675116392e-07, !i-but Tmin[K]=220 Tmax[K]=520
     
     *           59.5523569101971,  3.89300258982133e-2,
     *            7.60674748172611e-4, -6.92667780980629e-07, !penta Tmin[K]=250 Tmax[K]=520
     
     *           57.5762240024468, 1.04123242444337e-1,
     *            8.7579898137298e-4,  -8.78223117326366e-07, !esano Tmin[K]=280 Tmax[K]=520
     
     *           29.1463861243913, 6.02467767736795e-4,  
     *           -6.33303323260995e-06, 1.37310951911997e-08, !azoto Tmin[K]=220 Tmax[K]=520
     
     *           19.8128223971857,  7.18072092170482e-2, 
     *            -4.75892126870292e-5,  6.52569382265349e-09, !co2 Tmin[K]=220 Tmax[K]=520
     
     *           18.2629751384588,   7.44925108993445e-2,
     *           -1.69469145950031e-4, 1.28902740083975e-07 / !h2 Tmin[K]=220 Tmax[K]=520
      real CA(4,ncomp)

      real pcm,tcm,rocm,zcm,vcm
c-----------------------------------------------------------------------
c conversione composizione SIRE in composizione UNITAG-LIKE
c da togliere se e quando le due composizioni saranno descritte 
c coi medesimi componenti
      compos(1)  = compos_elem(o,1)  ! metano
      compos(2)  = compos_elem(o,2)  ! etano
      compos(3)  = compos_elem(o,3)  ! propano
      compos(4)  = compos_elem(o,4)  ! n-butano * butano
      compos(5)  = compos_elem(o,5)  ! i-butano
      compos(6)  = compos_elem(o,6)  ! pentano
      compos(7)  = compos_elem(o,7)  ! esano
      compos(8)  = compos_elem(o,8)  ! azoto
      compos(9)  = compos_elem(o,9)  ! c02
      compos(10) = compos_elem(o,10) ! h2
C---->                            Trasformazione PC in unita' MKS (N/m2)
C                                     Adimensionalizzazione dati critici
      
          
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',access='append')
C      write(LGU,*)'START calcolo dei coefficienti composizione'
c      write(LGU,*)'---------------------------'
      
c      write(LGU,*)'Composizione Unitag  
c     * CORRISPONDENZA indici I,J ->  1)met 2)eta 3)prop 4)n-but 5)i-but 
c     * 6)pent 7)esa 8)azo 9)co2 10)h2'
c      write(LGU,*)'---------------------------'
c      write(LGU,*)'Pressioni e Temperature (Pascal Kelvin)'
      DO I=1,ncomp
c        write(LGU,*)'VALORE I:',I 
        PCA(I)=PC(I)*1.01325E+5   ! in Pascal
c        write(LGU,*)'VALORE PCA(I):',PCA(I)
        TCA(I)=TC(I)              ! in kelvin
c        write(LGU,*)'VALORE TCA(I):',TCA(I)
      END DO
c      write(LGU,*)'---------------------------'
C---->               Calcolo coefficienti di miscela della Redlich-Kwong
c      write(LGU,*)'Calcolo coeff. di miscela della Redlich-Kwong'
      rk_par(o,1) = 0.
      rk_par(o,2) = 0.
      
      
      DO I=1,ncomp
c        write(LGU,*)'VALORE I:',I  
        rk_par(o,2) = rk_par(o,2) + OMB*r_mks*TCA(I)/PCA(I)*compos(I)
c        write(LGU,*)'VALORE rk_par(o,2):',rk_par(o,2)
        AA(I) = OMA * TCA(I)**2.5/PCA(I)
c        write(LGU,*)'VALORE AA(I):',AA(I)
      END DO
c      write(LGU,*)'--------------'
      DO I=1,ncomp
c        write(LGU,*)'VALORE I:',I   
        DO J=1,ncomp
c          write(LGU,*)'VALORE J:',J   
          XIJ = compos(I)*compos(J)*(1.-K(I,J)/100.)
c          write(LGU,*)'VALORE XIJ:',XIJ
          rk_par(o,1) = rk_par(o,1) + r_mks*r_mks*SQRT(AA(I)*AA(J))*XIJ
c          write(LGU,*)'VALORE rk_par(o,1):',rk_par(o,1)
        END DO
      END DO
c      write(LGU,*)'--------------'
C---->                                             Peso molecolare medio
c      write(LGU,*)'---Peso molecolare medio----'
      av_weight(o) = 0.
      DO J =1,ncomp
c        write(LGU,*)'VALORE J:',J  
        av_weight(o) = av_weight(o) + compos(J)*PMOL(J)
c        write(LGU,*)'VALORE av_weight(o):',av_weight(o)
      END DO
c      write(LGU,*)'--------------'
c---->                       calcolo coefficienti per il calore specifico
C                            Trasformazione coefficienti in unita' M.K.S.
c      write(LGU,*)'---Trasformazione coefficienti in unita M.K.S.----'
      DO I=1,4
c        write(LGU,*)'VALORE I:',I   
        DO J = 1,ncomp
c          CA(I,J)=C(I,J)*4.186*compos(J)   ! [joule]/[mole][k] riferiti a c1
c          write(LGU,*)'VALORE J:',J 
          CA(I,J)=Cnew(I,J)*compos(J)       ! [joule]/[mole][k] riferiti a c1
c          write(LGU,*)'VALORE CA(I,J):',CA(I,J)
        END DO
      END DO
c      write(LGU,*)'------'  
      DO I=1,4
c       write(LGU,*)'VALORE I:',I   
       cp_par(I,o)=0
       DO J = 1,ncomp
c        write(LGU,*)'VALORE J:',J   
        cp_par(I,o) = cp_par(I,o) + CA(I,J)
c         write(LGU,*)'VALORE cp_par(I,o):',cp_par(I,o)
       END DO
      END DO
c      write(LGU,*)'-------FINE Trasformazione-----------'
c---->          calcolo dei coefficienti per il calcolo della viscosita'
c si usa il metodo di Dean e Stiel (Perry, pag 3-278 e sgg.)
c
c vis_par(1) = ( M**1/2 pc**2/3)/(tc**1/6)
c con TC in kelvin, M peso molecolare in g/mole e PC in atm
c permette di calcolare la viscosita' in microPoise
c
c vis_par(2) = 1/tc
c
c vis_par(3) = 1/roc
c
c vis(p,T) = vis0(T) + deltavis(p,T)
c
c                          3.40 (T*vis_par(2))**8/9                     se T<= 1.5Tc
c vis0(T) = vis_par(1) * 
c                          16.68*(0.1338*T*vis_par(2)-0.0932)**5/9      se T> 1.5Tc
c
c 
c
c deltavis(p,T)=vis_par(1)*1.08*(exp(1.439*ro*vis_par(3)) -
c                                exp(-1.111(ro*vis_par(3))**1.858)
c
c-ace
c         i coefficienti per il calcolo della viscosità richiedono unità di misura speciali:
c         PC [atm], TC [K];  si utilizza la regola della miscelazione di Prausnitz per 
c         le proprietà critiche della miscela:
c
      TCM = 0.
      ZCM = 0.
      VCM = 0.
c      write(LGU,*)'---calcolo della viscosità----------------------'
      DO I=1,ncomp
c        write(LGU,*)'VALORE I:',I  
        TCM = TCM + compos(I)*TCA(I)
c        write(LGU,*)'VALORE TCM:',TCM
        ZCM = ZCM + compos(I)*ZC(I)
c        write(LGU,*)'VALORE ZCM:',ZCM
        VCM = VCM + compos(I)*(ZC(I)*TCA(I)/PC(I))  !Attenzione:PC in atmosfere
c        write(LGU,*)'VALORE VCM:',VCM
      END DO
c      write(LGU,*)'-------------'
      PCM = ZCM*TCM/VCM
c      write(LGU,*)'VALORE PCM:',PCM
      ROCM= 1./VCM
c      write(LGU,*)'VALORE ROCM:',ROCM
      vis_par(o,1) = (sqrt(av_weight(o)) * pcm**(2./3.))/tcm**(1./6.)
c      write(LGU,*)'VALORE vis_par(o,1):',vis_par(o,1)
c
      vis_par(o,2) = 1./tcm
c      write(LGU,*)'VALORE vis_par(o,2):',vis_par(o,2)
c
      vis_par(o,3) = 1./rocm
c      write(LGU,*)'VALORE vis_par(o,3):',vis_par(o,3)
c      write(LGU,*)'---FINE calcolo della viscosità----------------------'
c
      
c      write(LGU,*)'FINE'
c      write(LGU,*)'///\\\'
c      write(LGU,*)''
c      close(120)
      return
      end
c- ace
      SUBROUTINE ZROT (o,ro,t,z)
c***********************************************************************
c- ace
c  Calcola il coefficiente di comprimibilità del gas a fissate T e densità
c  ([g]/[litro]) utilizzando l'eq. di Redlich-Kwong. 
c  I coefficienti rk_par(2) sono passati
c  tramite common rk.inc
c-----------------------------------------------------------------------
c Parametri:
c o,	    !:IN   offset all'elemento in topologia
c ro,		!:IN   densita` ([g]/[litro]) 
c t,		!:IN   temperatura [K]
c z		!:OUT  coefficiente di comprimibilita`
c-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real Z        ! O) 
      real ro       ! I) densità ([g]/[litro])
      real t        ! I) temperatura dimensionale in [k]
c
      real ro_mol   ! densità molare ([mole]/[m3])
      real RK1,RK2  ! coefficienti della equazione
c-----------------------------------------------------------------------
      
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'ZROT- Coeff.comprimibilità gas a fissate T e densità'
c      write(LGU,*)'----------------------------------------------------'
      RK1    = rk_par(o,1)   ! in [J m3 K**0.5/mole**2]
      RK2    = rk_par(o,2)   ! in [m3]/[mole]
c
      ro_mol = romks * ro / av_weight(o)   ! in [mole]/[m3]
c      write(LGU,*)'VALORE ro_mol:',ro_mol
c        
      Z=1/(1-ro_mol*RK2)-(RK1*ro_mol*t**(-1.5))/(r_mks*(1+ro_mol*RK2))
c      write(LGU,*)'VALORE Z:',Z
c
c- ace
c      write(LGU,*)'FINE ZROT'
c      write(LGU,*)''
c      close(120)
      return ! ZROT
      END
c
      SUBROUTINE DERZROT (o,ro,t,alfa,beta)
c***********************************************************************
c- ace
c Calcola le derivate di z (alfa e beta) a fissate T [K] e densità 
c ([g]/[litro]) utilizzando l'eq. di Redlich-Kwong.
c I coefficienti rk_par(2) sono passati tramite il common rk.inc
c-----------------------------------------------------------------------
c Parametri:
c o,	    !:IN   offset all'elemento in topologia
c ro,		!:IN   densita` ([g]/[litro])
c t,		!:IN   temperatura [K]
c alfa	!:OUT   
c beta	!:OUT  
c-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real alfa     ! O) 
      real beta     ! O) 
      real ro       ! I) densità ([g]/[litro])
      real t        ! I) temperatura dimensionale in [k]
c
      real RK1,RK2  ! coefficienti della equazione di stato
      real RR,p,z,AA,BB
c-----------------------------------------------------------------------
      RK1    = rk_par(o,1) 
      RK2    = rk_par(o,2) 
      RR     = r_mks / av_weight(o) * romks / pmks
      call ZROT (o,ro,t,z)
      P      = RR * Z * T * ro * pmks     ! P in [Pascal]
      AA     = rk_par(o,1) * P/((r_mks**2)*(T**2.5))
      BB     = rk_par(o,2) * P/(r_mks*T)
c        
      alfa = ((AA-BB-2*BB**2)*z-2*AA*BB) /
     *        (3*Z*Z*Z-2*Z*Z+(AA-BB-BB*BB)*Z)
      beta = ((2.5*AA-BB-2*BB*BB)*Z-3.5*AA*BB) /
     *        (3*Z*Z*Z-2*Z*Z+(AA-BB-BB*BB)*Z)
c
c- ace
      return ! ZROT
      END
c======================================================================
      SUBROUTINE ZPT1 (o,p,t,z)
c***********************************************************************
c- ace	
c     calcola il coefficiente di comprimibilità Z a fissate T e P
c	utilizzando l'eq. di Redlich-Kwong. I coefficienti zk_par(2) sono 
c     nel common rk.inc
c-----------------------------------------------------------------------
c Parametri:
c o	    !:IN   offset all'elemento in topologia
c p		!:IN	pressione in [bar]
c t		!:IN	temperatura [K]
c z		!:OUT	coefficiente di comprimibilita`
C
      IMPLICIT NONE
C
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real Z        ! O) 
      real t        ! I) TEMPERATURA dimensionale in [k]
      real P        ! I) PRESSIONE dimensionale in [BAR]
c
      real t_mks     ! temperatura MKS in [K]
      real p_mks     ! pressione MKS in [Pascal]
      real C1, C2, C3, C4
      REAL QQ,RR,S1,S2,AUX,AA,BB,B2,AB,ABB2,RAD,FI,FI3,Y1,Y2,Y3
	real fid,fi3d,ppi
c
      DATA C1/0.333333333333/,C2/0.111111111111/,
     *     C3/0.166666666667/,C4/0.037037037037/
c-----------------------------------------------------------------------
      
c     Trasformazione in MKS
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'ZPT1- coefficiente di comprimibilità Z'
c      write(LGU,*)'----------------------------------------------------'

	p_mks = p * PMKS
      t_mks = t
c
      AA = rk_par(o,1) * P_MKS/((r_mks**2)*(T_MKS**2.5))
      BB = rk_par(o,2) * P_MKS/(r_mks*T_MKS)
      B2 = BB*BB
      AB = AA * BB	
      ABB2=AA-BB-B2
      QQ = C1 * ABB2 - C2
      RR = C3 * (-ABB2 + 3.*AB) + C4
      RAD= QQ*QQ*QQ+RR*RR
c      write(LGU,*)'VALORE RAD:',RAD
      IF(RAD.GE.0.) THEN
        RAD = SQRT(RAD)
        S1 = RR+RAD
        S2 = RR-RAD
        IF(S1.GE.0.) THEN
          S1 = S1**C1
        ELSE
          S1 = - (-S1)**C1
        ENDIF
        IF(S2.GT.0) THEN
          S2 = S2**C1
        ELSE
          S2 =-(-S2)**C1
        ENDIF
        Z = S1 + S2 + C1
c        write(LGU,*)'VALORE Z con rad>=0:',Z
      ELSE           
C---->      Presenza di una fase liquida (calcola lo Z della fase vapore)
c-ace --- da SIMCENT 21-7-93  
	   ppi = acos(-1.)
         FI = ACOS( SQRT(RR*RR/(-(QQ*QQ*QQ))))
	   fID  = fi*180./ppi                                        
         fi3d = fid*C1 
	   fi3  = fi3d*ppi/180.
         AUX  = SIGN(2.*SQRT(-QQ),RR)
         Y1   =  AUX*COS(FI3)                                                   
         Y2   =  AUX*COS(FI3+2.*ppi/3.)
         Y3   =  AUX*COS(FI3+4.*ppi/3.)
         Z    =  AMAX1(Y1,Y2,Y3) + C1 
c         write(LGU,*)'VALORE Z con rad<0:',Z
	ENDIF
c      write(LGU,*)'FINE ZPT1'
c      write(LGU,*)''
c      close(120)
      return ! ZPT1
c- ace
      END
!======================================================================
      SUBROUTINE zrotdp(o,ro,t,z,dpdr)
c- ace
c     calcola il coefficiente di comprimibilità in funzione della temperatura [K]
c     e della densità ([g]/[litro]); valuta inoltre la derivata della pressione in
c     funzione della densità.
c		o,	 !:IN   offset elemento
c		ro,  !:IN   densita` ([g]/[litro]) 
c		t,	 !:IN	temperatura
c		z,	 !:OUT  Coeff. comprimibilita
c		dpdr)!:OUT	derivata pressione su densita'
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real ro       ! I) densità in [g]/ [litro]
     *    ,t        ! I) temperatura in [kelvin]
     *    ,z        ! O) fattore di comprimibilità
      real dzdr     !    dz/dro in [litro]/[g] ovvero ([m3]/[Kg]) 
     *    ,dpdr     ! O) dp/dro in [bar]*[litro]/[g]
c- ace
c                     dpdr è in [bar]*[litro]/[g]=[J]/[Kg]/10**5
      real RR
c-----------------------------------------------------------------------
c  1)  calcolo z
      call zrot (o,ro,t,z)
c  2)  calcolo dz/dro
c- ace dz/dro è in [m3]/[Kg]
      call dzdro(o,ro,t,dzdr)
c  3)  calcolo dp/dro
c- ace dp/dro è in [J]/[Kg]/10**5
c- ace RR è in [J]/[K]/[Kg]/10**5
c- ace p[bar]=RR*z*T*ro
c
      RR    = r_mks / av_weight(o) * romks / pmks
      dpdr = RR * T  * ( z + ro * dzdr )
c- ace
      return 
      end
!======================================================================
      SUBROUTINE dzdro(o,ro,t,dzdr)
c- ace
c  I coefficienti della equazione di stato sono passati via common rk.inc
c  Calcolo dz/dro in [litro]/[g] ovvero [m3]/[Kg]      
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real ro       ! I) densità in [g]/ [litro]
     *    ,t        ! I) temperatura in [kelvin]
      real dzdr     ! O) dz/dro in [litro]/[g] ovvero [m3]/[Kg]
c
      real rk1,rk2,ro_mol,t_mks
      real f1,f2,f3
c-----------------------------------------------------------------------
      RK1    = rk_par(o,1) 
      RK2    = rk_par(o,2)                 ! [m3]/[mole]
      ro_mol = romks * ro / av_weight(o)   ! [mole]/[m3]
      t_mks  = t 
c        
      f1 = (RK2)/(1-ro_mol*RK2)**2 
      f2 = ((RK1*t_mks**(-1.5))/(r_mks*(1+ro_mol*RK2)))
      f3 = ((RK1*RK2*ro_mol*t_mks**(-1.5))/(r_mks*(1+ro_mol*RK2)**2))
c
c --- calcolo dzdr
c
      dzdr = f1 - f2 + f3       !dzdr in [m3]/[mole]
c
c --- conversione di dzdr in [litro]/[g] ovvero [m3]/[Kg]
c     dzdr rappresenta la derivata di z fatta rispetto a ro_mol;
c     quindi occorre moltiplicare dzdr per la derivata di ro_mol ripsetto
c     a ro ---> romks/av_weight(o)
c
c                 dzdr = dzdr * av_weight(o) / romks  !!!!!!!!!!!!!!!!!!!!!
c- ace
      dzdr = dzdr / av_weight(o) * romks
c- ace
      return
      end
!======================================================================
      SUBROUTINE dzdt(o,ro,t,dz_dt)
c- ace
c     Calcolo dz/dT in 1/[K]
c     I coefficienti dell'equazione di stato sono passati via common rk.inc
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real*4 ro       ! I) densità in [g]/ [litro]
      real*4 t        ! I) temperatura [kelvin]
      real*4 dz_dt    ! O) dz/dt in 1 / [K]
c
      real rk1,rk2,ro_mol,t_mks
c-----------------------------------------------------------------------
      RK1    = rk_par(o,1) 
      RK2    = rk_par(o,2)                   ! [m3]/[mole]
      ro_mol = romks * ro / av_weight(o)     ! [mole]/[m3]
      t_mks  = t 
c        
c --- calcolo dz_dt [1]/[kelvin]
c- ace RK1 in       [J]*[m3]*[K**0.5]/[mole**2]
c                    
c
      dz_dt=1.5 * (RK1*ro_mol*t_mks**(-2.5)) / (r_mks*(1+RK2*ro_mol))
c
c- ace
      return
      end

!======================================================================
      subroutine ZDRDT(z,dpdr,dpdt,o,ro,t)
c- ace
c     Calcola il coefficiente di comprimibilità mediante l'equazione R.K;
c     I coefficienti sono passati via common rk.inc;
c     Calcola la derivata della pressione sulla densità, noti ro e t:
c     call zrotdp ---> dpdr è in [bar]*[litro]/[g]=[J]/[Kg]/10**5;
c     Calcola la derivata della pressione sulla temperatura:
c     dp/dT=dp/dro*dro/dz*dz/dT  in [bar]/[K]
c
      implicit none     
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real ro       ! I) densità in [g]/ [litro]
     *    ,t        ! I) temperatura in [K]
     *    ,z        ! O) fattore di comprimibilità
      real dpdr     ! O) dp/dro in [bar]*[litro]/[g]=[J]/[Kg]/10**5
     *    ,dpdt     ! O) dp/dT  in [bar]/[K]
     *    ,dz_dt    !    dz/dT  in 1/[K]
      real RR
c-----------------------------------------------------------------------
      call zrotdp(o,ro,t,z,dpdr)
c
      call dzdt(o,ro,t,dz_dt)
c
c- ace RR è in [J]/[K]/[Kg]/10**5)
c- ace p[bar]=RR*z*T*ro
c
      RR   = r_mks / av_weight(o) * romks / pmks
c
      dpdt = RR * ro * ( z + T  * dz_dT )
c
c- ace
      return
      end
!======================================================================
      subroutine deltah1(o,ro,t,DH)
c- ace 
c     Calcola la differenza tra il valore reale dell'entalpia del gas e quella che esso
c     avrebbe se fosse ideale.
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o  ! I) offset all'elemento in topologia
      real*4 ro      ! I) densità in [g]/[litro]
     *      ,t       ! I) temperatura in [K]
     *      ,DH      ! O) scostamento entalpia in [kcal]/[Nm3]
c
      REAL*4 z,RR,P
      real*4 AA,BB,AUX
c-----------------------------------------------------------------------
      
c  1) calcolo z
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'Start deltah1- differenza valore reale entalpia'
c      write(LGU,*)'----------------------------------------------------'
      
      call zrot (o,ro,t,z)
      
c  2) calcolo i fattori:
c      write(LGU,*)'deltah1-VALORI romks e pmks:',romks,pmks
      RR  = r_mks / av_weight(o) * romks / pmks
c      write(LGU,*)'deltah1-VALORE RR=r_mks/av_weight(o)*romks/pmks:',P
      P   = RR * z * T * ro * pmks                ! P in [Pascal]
c      write(LGU,*)'deltah1-VALORE P=RR * z * T * ro * pmks:',P
      AA  = rk_par(o,1) * P/((r_mks**2)*(T**2.5))
c      write(LGU,*)'deltah1-VALORE AA=rk_par(o,1)
c     * *P/((r_mks**2)*(T**2.5)):',AA
      BB  = rk_par(o,2) * P/(r_mks*T)
c      write(LGU,*)'deltah1-VALORE BB  = rk_par(o,2) * P/(r_mks*T):',BB
      AUX = AA/BB*ALOG(Z/(Z+BB))
c      write(LGU,*)'deltah1-VALORE AUX = AA/BB*ALOG(Z/(Z+BB)):',AUX
      DH  = r_mks * T * ( 1.5*AUX + Z - 1)        ! in [joule]/[mole]
c      write(LGU,*)'deltah1-VALORE DH=r_mks*T*( 1.5*AUX + Z-1):',DH
c
c-ace
c     DH/4186/volume_molare è in [kcal]/[litro]=[kcal]/[dm3]
c     DH/4186/volume_molare*1000=DH/4.186/volume_molare è in [kcal]/[Nm3]
c     volume_molare in [litro]/[mole]
c     volume_molare = 22.414 (in condizioni normali: p=101325, t=273.15, z=1)
c
      DH  = DH / (CJOUccH2/1000) / volume_molare            ! in [kcal]/[Nm3]
c      write(LGU,*)'deltah1-VALORE DH/(CJOUccH2/1000)/volume_molare:',DH
c- ace
c      write(LGU,*)'FINE deltah1'
c      write(LGU,*)''

      RETURN
      END

c======================================================================
c- ace- new- calcolo politropica
c======================================================================
      subroutine D_rk(o,p,T,z,DH,DS)
c-     
c     Calcola gli scostamenti di ENTALPIA ed ENTROPIA utilizzando l'equazione
c     Redlich-Kwong. Questa routine serve per valutare il processo iterativo nel 
c     calcolo dell'esponente della politropica. 
c     Lo scostamento entalpico DH viene calcolato coerentemente alla routine DELTAH1.
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o  ! I) offset all'elemento in topologia
      real p       ! I) pressione in [Bar]
     *    ,t       ! I) temperatura in [K]
     *    ,z       ! O) coefficiente di comprimibilità
     *    ,DH      ! O) scostamento entalpia in [J/mole]
     *    ,DS      ! O) scostamento entropia in 
c
      REAL RR
      real AA,BB,AUX
      real p_mks,t_mks
c-----------------------------------------------------------------------
      
c  1) calcolo z
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c     write(LGU,*)'START D_rk - scostamenti di ENTALPIA ed ENTROPIA'
c      write(LGU,*)'----------------------------------------------------'

      call zpt1 (o,p,T,z)
c  2) calcolo i fattori:
c     Trasformazione in MKS
	p_mks = p * PMKS
      t_mks = T
c      write(LGU,*)'D_rk - Valori di p e PMKS(Param. Globale):',p,PMKS
c      write(LGU,*)'D_rk - Valore p_mks = p * PMKS:',p_mks
c      write(LGU,*)'D_rk - Valore t_mks = T:',t_mks
c
      AA  = rk_par(o,1) * p_mks/((r_mks**2)*(t_mks**2.5))
c      write(LGU,*)'D_rk - Valore AA  = rk_par(o,1) * 
c     * p_mks/((r_mks**2)*(t_mks**2.5)):',AA
      BB  = rk_par(o,2) * p_mks/(r_mks*t_mks)
c      write(LGU,*)'D_rk - Valore BB  = rk_par(o,2) * 
c     * p_mks/(r_mks*t_mks):',BB
      AUX = AA/BB*ALOG(Z/(Z+BB))
c      write(LGU,*)'D_rk - Valore AUX = AA/BB*ALOG(Z/(Z+BB)):',AUX
      DH  = r_mks * t_mks * ( 1.5*AUX + Z - 1)        ! in [joule]/[mole]
	DS  = r_mks *(ALOG(z-BB) + 0.5*AUX)
c      write(LGU,*)'D_rk - VALORE DH=r_mks*t_mks*(1.5*AUX + Z - 1):',DH
c      write(LGU,*)'D_rk - VALORE DS=r_mks *(ALOG(z-BB) + 0.5*AUX):',DS
c- ace
c-    DH e DS vengono utilizati con pressioni in Pascal e temperature in K
c      write(LGU,*)'FINE D_rk'
c      write(LGU,*)''
c      close(120)
      RETURN
      END
c------------------------------------------------------------

c- ace
	Function prop_a(p,T,coef)
c***********************************************************************
c	calcolo della proprieta' a P(adimensionale) e T(adimensionale) fissate
c	con formula biquadratica
c     (esp_pol=C1+C2*Pin+C3*Tin+C4*Pin*Tin+C5*Pin**2.+C6*Tin**2.)
c     Pressione e temperatura sono adimensionali perchè il calcolo dei
c     coefficienti è fatto in adimensionale per ragioni di precisione 
c     numerica.
c----------------------------------------------------------------------
	real*4 coef(*), p,T
	prop_a = coef(1)+(coef(2)+coef(4)*T+coef(5)*P)*P+
     *			(coef(3)+coef(6)*T)*T
	return
	end
c==========================================================================
c- ace
	Subroutine regre_exp(o)
c***********************************************************************
c	calcola i coefficienti di regressione per l'esponente della
c	politropica in funzione di Pin e Tin. I coefficienti sono calcolati
c     con valori di pressione e temperatura adimensionali per ragioni di
c     stabilità numerica. I coefficienti (exp_coef) sono memorizzati in 
c     common rk.inc.
c     (esp_pol=C1+C2*Pin+C3*Tin+C4*Pin*Tin+C5*Pin**2.+C6*Tin**2.)
c----------------------------------------------------------------------
        implicit none
c
      include '../inc/param.inc'	    !:IN
      include '../inc/rk.inc'		    !:IN
c
      integer*4 o         ! I) offset all'elemento in topologia
c
c
      integer*4 max_p, max_t, max_punti
	PARAMETER (max_p=10, max_t=10, max_punti=max_p*max_t)
	REAL*4    coef(6),exp_pol(max_punti),X(3,max_punti)
	integer*4 IP(20)
c
      integer*4 i,j,k,ier
	Real*4	  cesad
	external  cesad
      real*4    pmin,ppmin,pmax,ppmax,tmin,ttmin,tmax,ttmax,
     *          dp,dt,comp_ratio,tout,sum
        
c---->                     intervallo della griglia
c     pressione [bar], Temperatura [Pascal]
	DATA Pmin, Pmax, Tmin, Tmax/
cgpe     *      20., 120., 260.,  360./
c sono stati modificati il minimo e massimo in temperatura per motivi di convergenza
c e comunque sono stati posti nel range 0-50 °C coerentemente con il fatto che
c l'esponente della politropica viene calcolato seolo per le centrali
     *      20., 120., 273.,  323./
cgpe-end
c..........................................................................
	dp = (pmax-pmin)/(max_p-1)
	dt = (tmax-tmin)/(max_t-1)
c---->           costruzione della griglia
c     rapporto di compressione fisso perchè si è verificato che l'esponente della politropica
c     è indipendente dalla pressione di uscita
      comp_ratio= 1.5
	k = 1
	do i=1,max_p
	   do j=1,max_t
      	  X(1,k) = pmin + (i-1)*dp
	      X(2,k) = tmin + (j-1)*dt
	      CALL POLITROP(o,X(1,k),X(2,k),comp_ratio,
     *                    exp_pol(k),Tout,ier)
            if(ier.gt.0) then
			continue
            else
c                                   adimensionalizzazione per la regres
		    X(1,k) = X(1,k)/(PRIF+P0)
	        X(2,k) = X(2,k)/(TRIF+T0)

		    k = k + 1

            end if
	   end do
	end do
	k = k - 1
c---->            regressione per il calcolo dei coefficienti
	do i=1,6
	   IP(i)=1
	end do
	do i=7,20
	   IP(i)=0
	end do
	call regres(2,3,k,IP,X,exp_pol,coef,sum,ier)
	if(ier.gt.0) then	
		do i=2,6			! In mancata convergenxa esponenti recuperati 
							!      simulando l'esponente adiabatico
			exp_coef(i,o) = 0.0
		end do
		exp_coef(1,o) = cesad(o)
		return
      end if  
	do i=1,6
	  exp_coef(i,o) = coef(i)
	end do
	return
	end
C=======================================================================
	SUBROUTINE POLITROP_OK(o,pin,Tin,comp_ratio,
     *				    exp_pol,Tout,ier)
c***********************************************************************
C 	Calcola l'esponente della trasformazione politropica che meglio
C	approssima la compressione di un gas reale,  in funzione della
C	composizione, pressione di aspirazione e mandata,temperatura di
C	aspirazione.
C	Ipotesi: compressione isoentropica, proprieta' calcolate con
C	l'equazione di stato di Redlich-Kwong.
C     Pin e Tin sono dimensionali; tutta la routine lavora in dimensionale
c     pin        [bar]  I)
c     Tin        [K]    I)
c     comp_ratio [-]    I)
c     exp_pol    [-]    O)
c     Tout       [K]    O)
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/staznw_par.inc'
c
      integer*4 o  ! I) offset all'elemento in topologia
	real*4 cp0m,tin,esp,tout,comp_ratio,tm,pin,zin,dhin,dsin,
     *     pout,zout,dhout,dsout,tprec,w,aux1,aux2,ratio,f,df,
     *     espprec,exp_pol
      integer*4 idoc,ier
c---------------------------------------------------------------------
	ier = 0
C---->                                   Stima cp0 medio e Tout
	cp0m= CP_par(1,o)+Tin*(CP_par(2,o)+Tin*(CP_par(3,o)+
     *			Tin*CP_par(4,o)))                            !cp molare
c 
c                                                cp0m = [J/(mole K)]
c                                                r_mks= [J/(mole K)]
	esp  = r_mks/cp0m
c                                                esp  è [-]
	Tout = Tin * comp_ratio**esp
	Tm   = (Tin+Tout)/2.
	cp0m = cp_par(1,o)+Tm*(cp_par(2,o)+Tm*(cp_par(3,o)
     *	    		  +Tm*cp_par(4,o)))
	esp= r_mks/cp0m
c
C---->                 Calcolo Tout per una trasformaz. isoentropica
	CALL D_RK(o,pin,Tin,zin,DHin,DSin)
	pout=pin*comp_ratio  !      [bar]
c- ace
c     le pressioni possono essere lasciate in bar e non trasformate in
c     Pascal perchè si utilizza il rapporto di compressione)
c
c     processo iterativo per la Tout
c
      idoc=0
10    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10                !non converge il calcolo di Tout
        return
      end if
     	CALL D_RK(o,pout,Tout,zout,DHout,DSout)
	Tprec=Tout
c
	Tout = Tin * comp_ratio**esp * exp((DSin-DSout)/cp0m)
c- ace
c     richiedo minor precisione perchè si lavora con temperature in gradi Kelvin
c     pertanto è sufficiente una convergenza sulla seconda cifra decimale; 
c     ricordo che sono aumentati i limiti di  temperatura [-10°C,+86,85°C]
c
	IF(abs(Tout-Tprec).gt.5.e-1) GO TO 10
c
c     processo iterativo per il calcolo dell'esponente della politropica
c
c- ace W lavoro teorico
c
	W  =   cp0m * (Tout-Tin) + DHout - DHin
	aux1 = ALOG(comp_ratio)
	aux2 = W/(zin*r_mks*Tin)
c     Tout = Tin *(zin/zout)*(pout/pin)**esp
c     (pout/pin)**esp = (zout/zin)*(Tout/Tin)
c     esp = ALOG(zout*Tout/(zin*Tin))/ALOG(pout/pin)
	esp= ALOG((zout*Tout)/(zin*Tin))/aux1
      idoc=0
20    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10
        return
      end if
  	ratio=comp_ratio**esp
	F = ratio - 1. - aux2*esp
	DF= aux1*ratio - aux2
	espprec=esp
	esp = espprec - F/DF
	IF(abs(esp-espprec).gt.1.e-3) GO TO 20
	exp_pol = 1./(1.-esp)
C---->                Calcolo temperatura di uscita
	Tout = (zin/zout)*Tin*comp_ratio**esp
	RETURN
	END
c- ace--------------------------------------------------------------------

CMAR     start  prova pre la T_MAN


C=======================================================================
	SUBROUTINE POLITROP(o,pin,Tin,comp_ratio,
     *				    exp_pol,Tout_iso,ier)
c***********************************************************************
C 	Calcola l'esponente della trasformazione politropica che meglio
C	approssima la compressione di un gas reale,  in funzione della
C	composizione, pressione di aspirazione e mandata,temperatura di
C	aspirazione.
C	Ipotesi: compressione isoentropica, proprieta' calcolate con
C	l'equazione di stato di Redlich-Kwong.
C     Pin e Tin sono dimensionali; tutta la routine lavora in dimensionale
c     pin        [bar]  I)
c     Tin        [K]    I)
c     comp_ratio [-]    I)
c     exp_pol    [-]    O)
c     Tout       [K]    O)
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/staznw_par.inc'
c
      integer*4 o  ! I) offset all'elemento in topologia
	real*4 cp0m,tin,esp,tout,comp_ratio,tm,pin,zin,dhin,dsin,
     *     pout,zout,dhout,dsout,tprec,w,aux1,aux2,ratio,f,df,
     *     espprec,exp_pol
      integer*4 idoc,ier
cmar
      real*4 Tout_iso
      
c---------------------------------------------------------------------
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'POLITROP- trasformazione politropica'
c      write(LGU,*)'----------------------------------------------------'
c      write(LGU,*)'POLITROP-VALORE TIN:',Tin
	ier = 0
C---->                                   Stima cp0 medio e Tout
	cp0m= CP_par(1,o)+Tin*(CP_par(2,o)+Tin*(CP_par(3,o)+
     *			Tin*CP_par(4,o)))                            !cp molare
c 
c                                                cp0m = [J/(mole K)]
c                                                r_mks= [J/(mole K)]
	esp  = r_mks/cp0m
c                                                esp  è [-]
	Tout = Tin * comp_ratio**esp
	Tm   = (Tin+Tout)/2.


	cp0m = cp_par(1,o)+Tm*(cp_par(2,o)+Tm*(cp_par(3,o)
     *	    		  +Tm*cp_par(4,o)))


	esp= r_mks/cp0m
c
C---->                 Calcolo Tout per una trasformaz. isoentropica
	CALL D_RK(o,pin,Tin,zin,DHin,DSin)
	pout=pin*comp_ratio  !      [bar]
c- ace
c     le pressioni possono essere lasciate in bar e non trasformate in
c     Pascal perchè si utilizza il rapporto di compressione)
c
c     processo iterativo per la Tout
c
      idoc=0
10    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10                !non converge il calcolo di Tout
        return
      end if
     	CALL D_RK(o,pout,Tout,zout,DHout,DSout)
cmar
      Tm   = (Tin+Tout)/2.
      
      cp0m = cp_par(1,o)+Tm*(cp_par(2,o)+Tm*(cp_par(3,o)
     *	    		  +Tm*cp_par(4,o)))
	Tprec=Tout
c
	Tout = Tin * comp_ratio**esp * exp((DSin-DSout)/cp0m)

CMAR    ricaclolo il Tm, cp

      Tm   = (Tin+Tout)/2.
	cp0m = cp_par(1,o)+Tm*(cp_par(2,o)+Tm*(cp_par(3,o)
     *	    		  +Tm*cp_par(4,o)))

c- ace
c     richiedo minor precisione perchè si lavora con temperature in gradi Kelvin
c     pertanto è sufficiente una convergenza sulla seconda cifra decimale; 
c     ricordo che sono aumentati i limiti di  temperatura [-10°C,+86,85°C]
c
	IF(ABS(Tout-Tprec).gt.5.e-1) GO TO 10
	if (Tout.gt.Tprec)then
	Tout_iso = Tout

	else
	Tout_iso = Tprec
	end if
c
c     processo iterativo per il calcolo dell'esponente della politropica
c
      CALL D_RK(o,pout,Tout,zout,DHout,DSout)
c- ace W lavoro teorico
c
	W  =   cp0m * (Tout-Tin) + DHout - DHin
	aux1 = ALOG(comp_ratio)
	aux2 = W/(zin*r_mks*Tin)
c     Tout = Tin *(zin/zout)*(pout/pin)**esp
c     (pout/pin)**esp = (zout/zin)*(Tout/Tin)
c     esp = ALOG(zout*Tout/(zin*Tin))/ALOG(pout/pin)
	esp= ALOG((zout*Tout)/(zin*Tin))/aux1
      idoc=0
20    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10
        return
      end if
  	ratio=comp_ratio**esp
	F = ratio - 1. - aux2*esp
	DF= aux1*ratio - aux2
	espprec=esp
	esp = espprec - F/DF
	IF(abs(esp-espprec).gt.1.e-3) GO TO 20
	exp_pol = 1./(1.-esp)
C---->                Calcolo temperatura di uscita
	Tout = (zin/zout)*Tin*comp_ratio**esp
c      write(LGU,*)'POLITROP-VALORE TOUT:',Tout
c      write(LGU,*)'FINE POLITROP'
c      write(LGU,*)''

	RETURN
	END
c- ace--------------------------------------------------------------------

CMAR    end

CMAR-------------------------------------

      SUBROUTINE POLITROP_ESP(o,pin,Tin,comp_ratio,
     *				    exp_pol,ier)
c***********************************************************************
C 	Calcola l'esponente della trasformazione politropica che meglio
C	approssima la compressione di un gas reale,  in funzione della
C	composizione, pressione di aspirazione e mandata,temperatura di
C	aspirazione.
C	Ipotesi: compressione isoentropica, proprieta' calcolate con
C	l'equazione di stato di Redlich-Kwong.
C     Pin e Tin sono dimensionali; tutta la routine lavora in dimensionale
c     pin        [bar]  I)
c     Tin        [K]    I)
c     comp_ratio [-]    I)
c     exp_pol    [-]    O)
c     Tout       [K]    -----
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/staznw_par.inc'
c
      integer*4 o  ! I) offset all'elemento in topologia
	real*4 cp0m,tin,esp,tout,comp_ratio,tm,pin,zin,dhin,dsin,
     *     pout,zout,dhout,dsout,tprec,w,aux1,aux2,ratio,f,df,
     *     espprec,exp_pol
      integer*4 idoc,ier
c ---19/03/2026------Valore di default di uscita per exp_pol     
      exp_pol=1.5
      if(ABS(comp_ratio-1.0) .le. 0.0001) then
          
      end if    
c---------------------------------------------------------------------
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'POLITROP_ESP - trasformazione politropica ESP'
c      write(LGU,*)'----------------------------------------------------'
c      write(LGU,*)'POLITROP_ESP -VALORE TIN:',Tin
	ier = 0
C---->                                   Stima cp0 medio e Tout
	cp0m= CP_par(1,o)+Tin*(CP_par(2,o)+Tin*(CP_par(3,o)+
     *			Tin*CP_par(4,o)))                            !cp molare
c 
c                                                cp0m = [J/(mole K)]
c                                                r_mks= [J/(mole K)]
	esp  = r_mks/cp0m
c                                                esp  è [-]
	Tout = Tin * comp_ratio**esp
	Tm   = (Tin+Tout)/2.
	cp0m = cp_par(1,o)+Tm*(cp_par(2,o)+Tm*(cp_par(3,o)
     *	    		  +Tm*cp_par(4,o)))
	esp= r_mks/cp0m
c
C---->                 Calcolo Tout per una trasformaz. isoentropica
	CALL D_RK(o,pin,Tin,zin,DHin,DSin)
	pout=pin*comp_ratio  !      [bar]
c- ace
c     le pressioni possono essere lasciate in bar e non trasformate in
c     Pascal perchè si utilizza il rapporto di compressione)
c
c     processo iterativo per la Tout
c
      idoc=0
10    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10                !non converge il calcolo di Tout
        return
      end if
     	CALL D_RK(o,pout,Tout,zout,DHout,DSout)
	Tprec=Tout
c
	Tout = Tin * comp_ratio**esp * exp((DSin-DSout)/cp0m)
c- ace
c     richiedo minor precisione perchè si lavora con temperature in gradi Kelvin
c     pertanto è sufficiente una convergenza sulla seconda cifra decimale; 
c     ricordo che sono aumentati i limiti di  temperatura [-10°C,+86,85°C]
c
	IF(abs(Tout-Tprec).gt.5.e-1) GO TO 10
c
c     processo iterativo per il calcolo dell'esponente della politropica
c
c- ace W lavoro teorico
c
	W  =   cp0m * (Tout-Tin) + DHout - DHin
	aux1 = ALOG(comp_ratio)
	aux2 = W/(zin*r_mks*Tin)
c     Tout = Tin *(zin/zout)*(pout/pin)**esp
c     (pout/pin)**esp = (zout/zin)*(Tout/Tin)
c     esp = ALOG(zout*Tout/(zin*Tin))/ALOG(pout/pin)
	esp= ALOG((zout*Tout)/(zin*Tin))/aux1
      idoc=0
20    idoc=idoc+1
      if(idoc.gt.maxit) then
        ier=10
        return
      end if
  	ratio=comp_ratio**esp
	F = ratio - 1. - aux2*esp
	DF= aux1*ratio - aux2
	espprec=esp 
      if(DF .eq. 0.0) then
        esp = espprec - 0.001
      else
        esp = espprec - F/DF  
      end if    
	
	IF(abs(esp-espprec).gt.1.e-3) GO TO 20
	exp_pol = 1./(1.-esp)
c ---19/03/2026------Valore di default di uscita se exp_pol=1        
      if(exp_pol .eq. 1.0) then 
         exp_pol=1.5 
      end if    
CMAR
CMAR
CMAR  C---->                Calcolo temperatura di uscita
	Tout = (zin/zout)*Tin*comp_ratio**esp
c      write(LGU,*)'POLITROP_ESP - VALORE TOUT:',Tout
c      write(LGU,*)'FINE POLITROP_ESP'
c      write(LGU,*)''

	RETURN
	END


CMAR----------------------------------------------------------
      subroutine deroh(o,ro,T,ctvc,ctpc,dhdro)
c- ace
c 
c o    ! :I) offset all'elemento
c ro   ! :I) densita' in [g]/[litro]
c T    ! :I) temperatura in [K]
c ctvc ! :O) cap. ter. a vol. costante reale
c ctpc ! :O) cp0 + d(dh)/dT  in [kcal]/([Nm3]*K)
c dhdro! :O) d(dh)/dro       in ([Kcal]/[Nm3]) * ([m3]/[Kg])
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN


c
      integer*4 o   ! I) offset all'elemento in topologia
      real ro       ! I) densità in [g]/[litro]
      real t        ! I) temperatura in [k]
      real ctvc     ! O) [Kcal/kg/K]
      real ctpc     ! O) [Kcal/kg/K]
      real dhdro    ! O) [kcal/Nm3 * m3/kg]
c
      real ro_mol   ! densità molare ([mole]/[m3])
      real RK1,RK2  ! coefficienti della equazione
      real ro_cn,p_cn,t_cn,z_cn,f1,f2,f3
c-----------------------------------------------------------------------
      

      RK1    = rk_par(o,1) 
      RK2    = rk_par(o,2) 
      ro_mol = romks * ro / av_weight(o)   ! [mole]/[m3]
c
      dhdro = (-1.5*RK1*t**(-0.5))/(1+ro_mol*RK2)     ! [J]*m3/mole**2
c        
      f1 = (RK2)/(1-ro_mol*RK2)**2 
      f2 = ((RK1*t**(-1.5))/(r_mks*(1+ro_mol*RK2)))
      f3 = ((RK1*RK2*ro_mol*t**(-1.5))/(r_mks*(1+ro_mol*RK2)**2))
c- ace
c     questa è dh/d(ro_mol) ---> coerenza con l'analisi dimensionale
	dhdro = dhdro + r_mks*T*(f1 - f2 + f3 )         ![J]*m3/mole**2
c
c-ace dhdro = dhdro * d(ro_mol)/d(ro)
      dhdro = dhdro * romks / av_weight(o)  ! -->    [J]*[m3]/[mole]/[Kg]
c ATTENZIONE:
c     DHdro dovrebbe essere restituito in [kcal]/[Nm3]
c     ro_mol_cn = (pcn*PMKS)/r_mks/(tcn*TMKS)   ---> [mole]/[Nm3]
c     ro_cn = av_weight(o) * ro_mol_cn / romks  ---> [kg]/[Nm3]
c     dhdro = dhdro * (ro_mol/ro_mol_cn)  ---> [J]*[Nm3]/[mole]/[kg]
c calcolo cp
      dhdro = dhdro/(CJOUccH2/1000) /volume_molare

c
      call calccp(o,ro,t,ctpc,ctvc)





c                                    !trasformazione in [kcal/Nm3/K]
      ctpc = ctpc * av_weight(o) /volume_molare   
      ctvc = ctvc * av_weight(o) /volume_molare



      end
c----------------------------------------------------------
      subroutine calccp(o,ro,t,ctpc,ctvc)
c Calcola i calori specifici reali a p e a v costante
c
c o    !:in offset all'elemento
c ro   ! :in densita' in [g]/[litro]
c T    ! :in temperatura dimensionale in [k]
c ctvc ! :out cap. ter. a vol. costante reale
c ctpc ! :out cp0 + d(dh)/dT
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real ro       ! I) densità (verificare che sia in [g]/[litro])
      real t        ! I) temperatura dimensionale in [k]
      real ctvc     ! O) [kcal]/[K]/[kg]
      real ctpc     ! O) [kcal]/[K]/[kg]
c
      real z,RR,p,aa,bb,alfa,beta,dcp,cp0
c-----------------------------------------------------------------------
      
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'calccp- calori specifici reali a p e a v costante'
c      write(LGU,*)'----------------------------------------------------'
c      write(LGU,*)'START calccp......'
c      write(LGU,*)'----------------------------------------------------'
      cp0 = (cp_par(1,o)+cp_par(2,o)*t+cp_par(3,o)*t**2+
     *       cp_par(4,o)*t**3)
c      write(LGU,*)'Valore Cp0:',cp0
c cp0 in [joule]/[mole][K]
      call ZROT (o,ro,T,z)
c      write(LGU,*)'Valore Calcolo ZROT: ',z
      RR      = r_mks / av_weight(o) * romks / pmks
c      write(LGU,*)'Valore RR=r_mks / av_weight(o) * romks / pmks:',RR
      P       = RR * z * T * ro * pmks                   ! P [Pascal]
c      write(LGU,*)'Valore P = RR * z * T * ro * pmks: ',P
      AA      = rk_par(o,1) * P/((r_mks**2)*(T**2.5))
c      write(LGU,*)'Valore AA=rk_par(o,1) * P/((r_mks**2)*(T**2.5)): ',AA
      BB      = rk_par(o,2) * P/(r_mks*T)
c      write(LGU,*)'Valore BB= rk_par(o,2) * P/(r_mks*T): ',BB
      CALL DERZROT (o,ro,T,alfa,beta)
c      write(LGU,*)'Valore Derivate alfa,beta: ',alfa,beta
      dcp = r_mks*(0.75*AA/BB*ALOG((z+BB)/z)+
     *      (1+beta)*(1.5*(AA/(z+BB))+z)-1)             ! in [joule]/[mole][K]
c      write(LGU,*)'Valore dcp = r_mks*(0.75*AA/BB*ALOG((z+BB)/z)
c     * +(1+beta)*(1.5*(AA/(z+BB))+z)-1): ',dcp
      ctpc = cp0 + dcp                                  ! in [Joule]/[mole][K]
      ctpc = ctpc / (CJOUccH2/1000) / av_weight(o)                ! in [kcal]/[kg]/[K]
c
      ctvc = r_mks*z*(1+beta)*(1+beta)/(1+alfa)
      ctvc = ctvc / (CJOUccH2/1000) / av_weight(o)
      ctvc = ctpc - ctvc
c      write(LGU,*)'VALORE ctvc:',ctvc
c      write(LGU,*)'FINE calccp'
c      write(LGU,*)''

c
      return
      end
c- ace
c!======================================================================
      subroutine deltacv(o,ro,t,ctvc)
c
c Calcola il valore reale della capacita' termica a volume costante
c   o,	! :in offset all'elemento
c   ro,	! :in densita`
c   T,	! :in temperatura
c   ctvc  ! :out capacita' termica a V cost. [kcal]/[kg]/[K]
c
      implicit none
      integer*4 o   ! I) offset all'elemento in topologia
      real ro,t,ctvc,ctpc
c
      call calccp(o,ro,t,ctpc,ctvc)
c
      return
      END
c-------------------------------------------------
      SUBROUTINE viscosita_DS (o,p,t,vis)
c***********************************************************************
c Calcola la viscosita' del gas a fissate T e p
c utilizzando l'eq. di Dean e Stiel.
c I coefficienti della formula si riferiscono a ben precise unita' di misura:
c             p           in atmosfere
c             t           in kelvin
c             viscosita'  in centiPoise
c-----------------------------------------------------------------------
c Parametri:
c o,	    !:IN   offset all'elemento in topologia
c p,		!:IN   pressione in bar
c T,		!:IN   temperatura in kelvin
c vis		!:OUT  viscosita' in centipoise
c-----------------------------------------------------------------------
c- ace
      IMPLICIT NONE
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real p        ! I) pressione in bar
      real t        ! I) temperatura dimensionale in [k]
      real vis      ! O) viscosita' in centiPoise
c
      real vis0,deltavis
      real trid,rorid
      real z
      real p_atm
c-----------------------------------------------------------------------
      p_atm = p / 1.01325     ! trasformazione bar ---> atm
      trid = t*vis_par(o,2)
      call ZPT1(o,p,T,z)
c   
c     ricordiamo che vis_par(o,3) = 1./VCM= p[atm]/(z*t[K])
      rorid = vis_par(o,3) * p_atm / (z*t)
c
      if (trid.le.1.5) then
        vis0 = vis_par(o,1)*3.40*trid**0.8889
      else 
        vis0 = vis_par(o,1)*16.68*(0.1338*trid-0.0932)**0.5556
      endif
      deltavis = 
     *    vis_par(o,1)*1.08*(EXP(1.439*ROrid)-EXP(-1.111*ROrid**1.858))
      vis = vis0 + deltavis   ! in microPoise
      vis = vis / 10000       ! in centiPoise
c- ace
      return 
      END
c
	SUBROUTINE LAMBD1(	 ! stima di lambda indip dalla portata
				 ! (formula Colebrook_White semplificata)
	1		  o) !:in offset all'elemento nella topologia
				 ! TX :in_out
*&*
C
        IMPLICIT NONE
C
	include '../inc/param.inc'	!:costanti
        include '../inc/tI.inc'		!:costanti
        include '../inc/tJ.inc'		!:costanti
        include '../inc/th.inc'		!:costanti
        include '../inc/cc.inc'		!:costanti
	include '../inc/td.inc'		!:IN
	include '../inc/tx.inc'		!:IN
	include '../inc/ty.inc'		!:IN_OUT

        integer*4 o

!-----------------------------------------------------------------------
! cf. collana Schaum, Giles: Meccanica dei fluidi e idraulica, Cap. 7
!			 1.0/sqrt(blam)=-2.0 * log10(rugo/(3.7* di))
!				 il moto e' sempre turbolento
!-----------------------------------------------------------------------
        real*4 portat
        real*4 BL,AR,AL,COST
	data  BL,AR,AL,COST  / 3.71, 353.68 , 2.51 , 0.868589 /
!				cost = 2* log10(e)
        real*4 x

        real*4 rey,y,z,binv,t,rerr,deriv
        integer*4 i

        integer*4 im,iv
        real*4 pm,tm
        real*4 visc
        real*4 c1,c3,b,a7,a8,a9,f
        DATA  C1/2.51/,C3/3.7/
c
*&*
	x =   alog10 ( rugotx(o)/( BL*diamtx(o)) )
	blamty(o) = 0.25 /(x*x)
	RETURN ! LAMBD1

!======================================================================
	ENTRY LAMBD2_old(   	!calcolo lambda nota la portata
	1		  o	 !:in offset all'elemento nella topologia
	2	     ,portat)	 ! :IN portata
				 ! TX :in_out
*&*
!----------------------------------------------------------------------
! cf. collana Schaum, Giles: Meccanica dei fluidi e idraulica, Cap. 7
!	 formula Colebrook_White completa
!			1.0/sqrt(blam)= -2.0 * log10[ rugo/(3.7*di) 
!					+ 2.51/(rey * sqrt(blam))]
!				ove: rey = no.di reynolds =
!				 = 4.0/pgreca*ro0*abs(portat)/(di*vis)
!			 se rey < 2000 [2005?] il moto e` laminare :
!						 blam = 64/rey
!			 essendo velocita`=portat*ro0/(ro*di*di*pgreca/4)
!
!-----------------------------------------------------------------------
C INTERVENTO PER IL CALCOLO DELLA VISCOSITA
        im = opumto(O)
        iv = opuvto(O)
        PM = (PRESTJ(IM)+PRESTJ(IV))/2.
        TM = (TEMPTJ(IM)+TEMPTJ(IV))/2.
        CALL VISCOSITA_DS(O,PM,TM,VISC)
C
	REY = AR * ro0td(o) * MAX(ABS(portat),1.0) /( diamtx(o) * visc )

	IF ( REY .lt. 2000.0) THEN    ! moto laminare
			blamty(o) = 64.0 / rey
	ELSE 				! moto turbolento

			Y    =  rugotx(o) / ( BL*diamtx(o))
			Z    =  AL / REY
			BINV =  1.0 / SQRT( blamty(o))	

			DO i = 1,10
				T = Y + BINV * Z
	       			RERR = BINV + 2 * alog10 ( T )
				IF ( abs (RERR) .lt. RZLAcc ) THEN ! successo
						blamty(o) = 1.0/ (binv*binv)
						RETURN
				ENDIF
				DERIV = 1 + COST * ( Z/T )
				BINV = BINV  - RERR / DERIV
	 		ENDDO
!		--------------------------------------------------
c         il calcolo di lambda non converge in 10 iterazioni
c                call gest_error(2,0,'LAMBD2',
c     *           'lambda non converge',0)
		blamty(o) = 0
	ENDIF  ! rey
        Return
c gz intervento per introdurre la serghides
	ENTRY LAMBD2(   	 !calcolo lambda nota la portata
	1		  o	 !:in offset all'elemento nella topologia
	2	     ,portat)	 ! :IN portata
				 ! TX :in_out
*&*
!----------------------------------------------------------------------
!	 formula di Serghides
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
C INTERVENTO PER IL CALCOLO DELLA VISCOSITA
        im = opumto(O)
        iv = opuvto(O)
        PM = (PRESTJ(IM)+PRESTJ(IV))/2.
        TM = (TEMPTJ(IM)+TEMPTJ(IV))/2.
        CALL VISCOSITA_DS(O,PM,TM,VISC)
C
	REY = AR * ro0td(o) * MAX(ABS(portat),1.0) /
     *        ( diamtx(o) * visc )

        if(rey.lt.1000.) rey= 1000.
        B = rugotx(o) / (C3*diamtx(o))
        A7=-2.*ALOG10(B+12./Rey)
        A8=-2.*ALOG10(B+C1*A7/Rey)
        A9=-2.*ALOG10(B+C1*A8/Rey)
        F= A7 - (A8-A7)*(A8-A7)/(A9-2*A8+A7)
        blamty(o) = 1./(F*F)
c end gz
	END ! LAMBD2, LAMBD1
c
	SUBROUTINE GASTD(	 ! caratteristiche del gas per un 
				 ! elemento della rete
				 ! (peso molecolare,pressione critica,
				 !  temperatura critica,viscosita` cinematica,
				 !  viscosita` dinamica,potere calorifico
				 !  inferiore)	
	1		  of)	 ! :IN offset oggetto
				 ! :OUT in TD
	             ! Grandezze in condizioni nominali
*&*
        implicit none

	include '../inc/param.inc'	! :costante
	include '../inc/TH.inc'		! :costanti
	include '../inc/TI.inc'		! :costanti
	include '../inc/CC.inc'		! :costante
	include '../inc/TC.inc'	        ! :IN
	include '../inc/RK.inc'	        ! :IN
	include '../inc/TD.inc'	   	! :OUT
*&*
        integer*4 of,gas
*&*
!--------------------------------------------------------------------------
		PCLItd (of) = 0.0 ! potere calorifico inferiore
	    ctpctd(of) = 0.0  !  calori specifici cv e cp
	    ctvctd(of) = 0.0  !

		do gas = 1, M_COMP
		 PCLItd (of) = PCLItd (of) + 
     -                COMPtc(of ,gas) * PCLIcc (gas)
           ctpctd(of) = ctpctd(of) + 
     -                   COMPtc (of,gas)* ctpccc (gas)
           ctvctd(of) = ctvctd(of) + 
     -                   COMPtc (of,gas) * ctvccc (gas)
		enddo  ! gas

	call zpt1 (of, P0cc, T0cc, ZM0td (of))	!:out coef.comprimibilita` 
	RO0td  (of) = av_weight(of) / (volmcc *  ZM0td (of))  	! densita`
	ravotd (of) = p0cc/( t0cc * ro0td(of) * zm0td(of))
					!rapporto tra costante universale
					!gas perfetti e peso molecolare:
					! P = Ro * Z * Ravotd * T
!--------------------------------------------------------------------------
	RETURN
!----------------------------------------------------------------------
	END	!gastd
!======================================================================

	SUBROUTINE GASROT (             ! eq. di stato dei gas reali,
                                        ! calcola la PRESSIONE note
                                        ! densita` e temperatura
	1                  o,           !:in offset all'elemento in topologia
	1                  p,           !:OUT   pressione
	2                  ro,          !:IN    densita`
	3                  t,           !:IN    temperatura
	4                  z)           !:IN    coefficiente di comprimibilita`

*&*
        implicit none
        include '../inc/param.inc'     !:costanti
        include '../inc/cc.inc'                !:costanti
        include '../inc/td.inc'                !:IN
 
        integer*4 o
        real*4 p,ro,t,z
                 
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
c      write(LGU,*)'GASROT- eq. di stato dei gas reali'
c      write(LGU,*)'----------------------------------------------------'


       
!-----------------------------------------------------------------------
!        equazione di stato:  (p/p0) = (ro/ro0) * (z/z0) * (t/t0),
!                                       ove p0 e t0 sono costanti
!-----------------------------------------------------------------------
*&*
        P = P0cc * (RO/ro0td(o)) * (Z/zm0td(o)) * (T/T0cc)
c        write(LGU,*)'VALORE GASROT - RO:',RO
c        write(LGU,*)'VALORE GASROT - T:',T
c        write(LGU,*)'VALORE GASROT - Z:',Z
c        write(LGU,*)'VALORE RESTITUITO GASROT - P:',P
c        write(LGU,*)'FINE GASROT'
c        write(LGU,*)''
        
          
        RETURN ! GASROT

!======================================================================

	ENTRY    GASPT  (               ! eq. di stato dei gas reali
                                        ! calcola la DENSITA` note:
                                        !  pressione e temperatura
	1                  o,           !:in offset all'elemento in topologia
	2                  p,           !:IN    pressione
	1                  ro,          !:OUT   densita`
	3                  t,           !:IN    temperatura
	4                  z)           !:IN    coefficiente di comprimibilita`
*&*
!----------------------------------------------------------------------
        RO = ro0td(o) * (P/P0cc) * (zm0td(o)/Z) * (T0cc/T)

        RETURN !GASPT
        END ! GASPT GASROT

!======================================================================
      SUBROUTINE FERG1 (!Calcola i coefficienti di Fergusson
     1 o,		!IN  offset tronco
     2 afer,    	!OUT primo coef. Fergusson
     3 bfer,    	!OUT secondo coef. Fergusson
     4 t,    		!IN  temperatura 
     5 z)    		!IN  coef. di comprimibilita'
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/cc.inc'
      include '../inc/td.inc'
      include '../inc/tx.inc'
      include '../inc/ty.inc'

      integer*4 o
      real*4 afer,bfer,t,z
 
      real*4 AR,BR,afer0,bfer0,xs,xk,xse
*&*
      data  AR,BR/2.0E-5,1.25E-6/

! calcolo di afer0,bfer0 iniziali (indipendenti da P,T)
      afer0=Disltx(o)*(AR*AGRVcc)/ravotd(o)
      bfer0=(ro0td(o)*RLUNtx(o))/(zm0td(o)*diamtx(o)**5)*(BR*P0cc)/T0cc

! coef. S,K
      xs=afer0/(T*Z)
      xk=bfer0*T*blamty(o)*Z	
! calcolo di afer,bfer
      xse=exp(-xs)
      afer=xse
      if(abs(xs).gt.RFERcc)then !tronco in pendenza
        bfer=xk*(1.0-xse)/xs
      else  			!tronco piano
        bfer=xk
      endif

      RETURN
      END

!======================================================================
	real*4 FUNCTION  CESAD(	! Coeff. espansione adiabatica
				! dip. da temperatura e miscela
	4		ie)	! :IN offset elemento (centrale)
*&*
        implicit none
	INCLUDE '../inc/param.inc'	! :costante
	INCLUDE '../inc/TD.inc'	! :IN

        integer*4 ie
*&*
!----------------------------------------------------------------------
! coeff. espansione adiabatica
	cesad = ctpctd(ie) / ctvctd(ie)
	RETURN
	END ! cesad
!-------------------------------------------------------------------------------
	real*4 FUNCTION  PCLS_REAL(	! Potere calorifico reale
				! (calcolato dalla composizione)
	4		ie)	! :IN offset elemento ()
*&*
        implicit none
	INCLUDE '../inc/param.inc'	! :costante
	INCLUDE '../inc/default.inc'	! :IN
	INCLUDE '../inc/flag.inc'	! :IN
	INCLUDE '../inc/CC.inc'	! :IN
	INCLUDE '../inc/TC.inc'	! :IN
	INCLUDE '../inc/TD.inc'	! :IN

        integer*4 ie

        integer*4 igas
      
*&*
!----------------------------------------------------------------------
c      OPEN (unit = 120, file='C:\LF\RES_H2.txt',status='old',
c     *      access='append')
      write(LGU,*)'PCLS_REAL- Potere calorifico reale'
      write(LGU,*)'----------------------------------------------------'  
	PCLS_REAL = 0
	do igas = 1, M_COMP
	  PCLS_REAL = PCLS_REAL + 
     -                COMPtc(ie ,igas) * PCLScc (igas)
	enddo  ! gas

      if (fl_pcls_def) PCLS_REAL = pcls_def
      write(LGU,*)'VALORE PCLS_REAL:',PCLS_REAL
      write(LGU,*)'FINE PCLS_REAL'
      write(LGU,*)''

	RETURN
	END ! PCLS_REAL


      subroutine definisci_cost_cc

!-Inizializza le costanti chimico fisiche della miscela gas. 

!-occorre ricordare che tali costanti sono inizializzate anche nelle verifiche
!-una modifica in questa routine deve essere riportata nelle analoghe
!-routine in ssdd$for (inicc in pinicc.for) e viceversa

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/cc.inc'             !costanti gas

      integer*4 gas
!-----------------------------------------
      RZLAcc=1e-5     	! precizione nel calcolo di lambda e zeta
      RECOcc=0.8        ! rendimento adiabatico di compressione
      RFERcc=1e-6       ! coefficiente per Fergusson
      PGR4cc=atan(1.0)  ! pi_greca / 4
      PGREcc=4.0*PGR4cc ! pi greca
      AGRVcc=9.80655    ! accelerazione di gravita` (m./sec**2) -NEW_VALUE- FONTE: Masse molari da EN ISO 6976:2016-12
      T0cc=273.15       ! temperatura di riferimento (k.)
      P0cc=1.01325       ! pressione di riferimento (bar) -NEW_VALUE- FONTE: Masse molari da EN ISO 6976:2016-12
      CJOUcc=4186.8     ! costante di conversione joule/kCal -NEW_VALUE- FONTE: Masse molari da EN ISO 6976:2016-12
      VOLMcc=22.414     ! volume molare (litri/mole) = R * t0 / p0
      CTPVcc=1.985875   ! costante universale dei gas in Kcal/ (Kmole * K) -NEW_VALUE- FONTE: Masse molari da EN ISO 6976:2016-12
c      POCEcc=0.02778    ! fattore di conversione calcolo potenza centrali in MW
      POCEcc=27.78    ! fattore di conversione calcolo potenza centrali in kW
      
!----------------------Masse molari da EN ISO 6976:2016-12------------------------------------------------------------------
!       Metano		16.04246
!       Etano		    30.06904
!       Propano		44.09562
!       n-butano	    58.1222
!       i-butano      58.1222
!       pentano		72.14878
!       esano	    	86.17536
!       azoto		    28.0134
!       CO2			44.0095
!       Idrogeno	    2.01588     
!---------------------------------------------------------------------------------------------------------------------------      
!------------------------------------------
      cpwccc=(pocecc*p0cc)/t0cc
!------------------------------------------
!     capacita` termica componenti puri a p costante [Kcal/(Nmc*K)] -NEW_VALUE- FONTE : Thermophysical Properties of Fluid Systems (nist.gov) NIST Standard Reference Data 2025
!------------------------------------------
      CTPCcc (GAS_MET)  = 0.372
      CTPCcc (GAS_ET)   = 0.529
      CTPCcc (GAS_PROP) = 0.732
      CTPCcc (GAS_BUT)  = 0.987
      CTPCcc (GAS_IBUT) = 0.961
      CTPCcc (GAS_PENT) = 1.202
      CTPCcc (GAS_ES)   = 1.422
      CTPCcc (GAS_N2)   = 0.310
      CTPCcc (GAS_CO2)  = 0.384
      CTPCcc (GAS_H2S)  = 0.305 !valore idrogeno
!------------------------------------------
!     capacita` termica componenti puri a v costante [Kcal/(Nmc*K)]
!------------------------------------------
      do gas=1,m_comp
        CTVCcc(gas)=CTPCcc(gas)-CTPVcc/VOLMcc
      enddo
!------------------------------------------
!     potere calorifico inferiore kJ/kNm3 -NEW_VALUE- FONTE: Valori calcolati secondo EN ISO 6976:2016-12
!     Scelto valore a 0 °C in coerenza con i Nm3 a denominatore
!     Per il passaggio da PCS a PCI è stata usata l'entalpia di vaporizzazione dell'acqua a 0°C 
!------------------------------------------
      PCLIcc (GAS_MET)  = 35816594.1297169 
      PCLIcc (GAS_ET)   = 63761935.8853078
      PCLIcc (GAS_PROP) = 91183050.9657148
      PCLIcc (GAS_BUT)  = 118588104.633095
      PCLIcc (GAS_IBUT) = 118180323.202356
      PCLIcc (GAS_PENT) = 146003865.90916
      PCLIcc (GAS_ES)   = 173453980.763087
      PCLIcc (GAS_N2)   = 0.0
      PCLIcc (GAS_CO2)  = 0.0
      PCLIcc (GAS_H2S)  = 10777921.9816347 !valore idrogeno
!------------------------------------------
!     potere calorifico superiore kJ/kNm3 -NEW_VALUE- FONTE: Valori calcolati secondo EN ISO 6976:2016-12
!     Scelto valore a 0 °C in coerenza con i Nm3 a denominatore      
!------------------------------------------
      PCLScc (GAS_MET)  = 39837658.1110759
      PCLScc (GAS_ET)   = 69793531.8573462
      PCLScc (GAS_PROP) = 99225178.9284327
      PCLScc (GAS_BUT)  = 128640764.586492
      PCLScc (GAS_IBUT) = 128232983.155754
      PCLScc (GAS_PENT) = 158067057.853236
      PCLScc (GAS_ES)   = 187527704.697844
      PCLScc (GAS_N2)   = 0.0
      PCLScc (GAS_CO2)  = 0.0
      PCLScc (GAS_H2S)  = 12788453.9723142 !valore idrogeno
!------------------------------------------
      return
      end
!-------------------------------------------------------------------------------
	Subroutine regre_dh(o)
c***********************************************************************
c***********************************************************************
c	calcola i coefficienti dh_coef(*) per approssimare gli scostamenti 
c     entalpici mediante una formula lineare in pressione e temperatura,i.e.
c     DH = dh_coef(1) + dh_coef(2)*p + dh_coef(3)*T
c     I coefficienti (dh_coef(3,max_vert)) sono memorizzati in 
c     common rk.inc.
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'	    !:IN
      include '../inc/rk.inc'		    !:IN
c
      integer*4 o                     ! I) offset all'elemento in topologia
c
c
      integer*4 max_p, max_t, max_punti
	PARAMETER (max_p=10, max_T=10, max_punti=max_p*max_t)
	REAL*4    coef(3),dh_h(max_punti),dh_s,z(max_punti),
     *          X(3,max_punti)
	integer*4 IP(20)
c
      integer*4 i,j,k,ier
      real*4    pmin,pmax,tmin,tmax,dp,dt,
     *          tout,sum
c---->                     intervallo della griglia
c     pressione [bar], Temperatura [kelvin] 
	DATA Pmin, Pmax, Tmin, Tmax/
     *      20., 120., 273.,  323./	


c
c-----------------------------------------------------------------------
c---->                           determinazione dei valori sulla griglia
	dp = (pmax-pmin)/(max_p-1)
	dt = (tmax-tmin)/(max_t-1)
c---->           costruzione della griglia
      k=1
	do i=1,max_p
	   do j=1,max_T
      	  X(1,k) = pmin + (i-1)*dp
	      X(2,k) = tmin + (j-1)*dt
c
            call d_rk(o,x(1,k),x(2,k),z(k),dh_h(k),dh_s)
	      x(1,k) = x(1,k)*PMKS
	      k=k+1
c           non adimensionalizzo pressioni e temperature 
c           in quanto dh_h non è adimensionale, ma trasformo le pressioni 
c           in Pascal
	    end do
	end do
c---->
      k=k-1
	do i=1,3
	   IP(i)=1
	end do
	do i=4,20
	   IP(i)=0
	end do
	call regres(2,3,k,IP,X,dh_h,coef,sum,ier)
	if(ier.gt.0) then
	   ier=0
	   coef(1)=1.
	   coef(2)=0.
	   coef(3)=1.
      end if 
	do i=1,3
	  dh_coef(i,o) = coef(i)
	end do
 	return
	end
c-ace
	subroutine ro_real(	! Calcola la densità alle reali condizioni del gas
	                    ! per la corretta valutazione delle perdite di carico concentrate)
	1		  o,	! :in offset all'elemento nella struttura dati completa
	1		  pres,	! :in pressione in [bar]`
	1		  temp,	! :in temperatura in [K]
	1		  ro)	! :out [g]/[litro]=[kg]/[m3]
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   
      real*4 ro    
     *      ,temp     
     *      ,pres     
      REAL*4 zeta,rr
c-----------------------------------------------------------------------
      
c      write(LGU,*)'ro_real - densità alle reali condizioni del gas'
c      write(LGU,*)'----------------------------------------------------'

c  1) calcolo z
	call zpt1(o,pres,temp,zeta)
c  2) calcolo i fattori:
      rr=r_mks*romks/pmks/av_weight(o)
	ro=pres/(rr*temp*zeta)
      
c      write(LGU,*)'VALORE in ro_real zeta:',zeta
c      write(LGU,*)'VALORE in ro_real temp:',temp
c      write(LGU,*)'VALORE in ro_real pres:',pres
c      write(LGU,*)'VALORE RESTITUITO ro_real ro:',ro
c      write(LGU,*)'FINE ro_real'
c      write(LGU,*)''
      
	return
c- ace
	end 
c
c-ace-bilancio energetico
      subroutine dHdT_ro(o,ro,T,cp_new)
c
c Calcola la derivata dell'entalpia H(ro,T) fatta rispetto alla temperatura T 
c mantenendo ro costante.
c Questa derivata non rappresenta il calore specifico che è per definizione la 
c derivata dell'entalpia H(p.T) fatta rispetto alla temperatura T mantenendo la 
c pressione costante. Questa derivata viene utilizzata nell'equazione 
c di bilancio energetico.
c
c
c o      ! :in offset all'elemento
c ro     ! :in densita' in [g]/[litro]
c T      ! :in temperatura dimensionale in [k]
c cp_new ! :out [kcal/Nm3/k]
c
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 o   ! I) offset all'elemento in topologia
      real*4 ro     ! I) densità (verificare che sia in [g]/[litro])
      real*4 T      ! I) temperatura dimensionale in [k]
	real*4 cp_new ! O) (DH/DT)con ro costante [kcal/Nm3/k]
c
	real*4 ctpc,ctvc,dhdro,alfa,beta
c-----------------------------------------------------------------------
c
	call deroh  (o,ro,T,ctvc,ctpc,dhdro)
c                                              dhdro=[kcal/Nm3]*[m3/kg]
c                                              ctpc =[kcal/Nm3/k]
	call derzrot(o,ro,T,alfa,beta)
c                                              alfa =[-]
c                                              beta =[-]
      cp_new = ctpc+dhdro*ro/T*(1.+beta)
c                                              cp_new=[kcal/Nm3/k]
      return
      end
    