c------------------------------------------------------------------
      SUBROUTINE PWMAXC1(FLAG,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *           NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *           CLIM,CPW,CPWT,TIN,TAIR,POUT,
     *           EFF_CORR,POW_CORR,nom_power,
     *           na,nb,qa,ha,qb,hb,
     *           nta,ntb,pta,ptb,ptr,un_q,un_h,flag_ch,
     *           na_ch,nb_ch,qa_ch,ha_ch,qb_ch,hb_ch,
     *           nta_ch,ntb_ch,pta_ch,ptb_ch,
     *           ptr_ch,un_q_ch,un_h_ch)

c*******************************************************************
C     CURVE DI MASSIMA POTENZA DELLE TURBINE RIPORTATE SULLE CARATTERISTICHE
C     DEI COMPRESSORI A PRESSIONE DI USCITA ASSEGNATA.
C-----------------------------------------------------------------------------
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 nlim
c
      real*4 pow_corr,eff_corr,nom_power,
     *       max_rev,min_rev,CHN(*),CHC(*),CEN(*),CEC(*),
     *       alim(*),blim(*),lim_n(*),lim_q(*),lim_h(*),
     *       clim(*),CPW(*),CPWT(*),tin,tair,pin,pout
c
c     preparazione del common per la FUN(pin)
      real*4 qq,ppow_corr,eeff_corr,CCHN(6),CCHC(6),CCEN(5),CCEC(6),
     *       cnlim,calim(max_nlim),cblim(max_nlim),
     *       clim_n(max_nlim),clim_q(max_nlim),clim_h(max_nlim),
     *       cclim(4),CCPW(3),CCPWT(4),
     *       nnom_power,ppwt,ttin,ttair,ppin,ppout
c
      COMMON/PWAUX/QQ,CCEN,CCEC,CCHN,CCHC,CNLIM,CALIM,CBLIM,CCLIM,
     *             CLIM_N,CLIM_Q,CLIM_H,PPWT,TTIN,PPOUT,
     *             EEFF_CORR,PPIN,CCPW,CCPWT,
     *             TTAIR,PPOW_CORR,nnom_power
c
      real*4 FUN
      EXTERNAL FUN      
      LOGICAL*2  FLAG,INPW,flag_ch,flag_out
c     vettori e dimensioni 
c     per la rappresentazione grafica della curva di massima potenza
c
      real*4 qa(*),ha(*),qb(*),hb(*)
	real*4 un_q(*),un_h(*)
	integer*4 na,nb,ptr,nta,ntb,pta(*),ptb(*)
      real*4 qa_ch(*),ha_ch(*),qb_ch(*),hb_ch(*)
	real*4 un_q_ch(*),un_h_ch(*)
	integer*4 na_ch,nb_ch,ptr_ch,nta_ch,ntb_ch,pta_ch(*),ptb_ch(*)

c	
      integer*4 i
c----------------------------------------------------------------------------
      INPW  = .FALSE.
C
C----> DEFINIZIONE DELL'AREA COMMON PER LA FUNCTION FUN
      DO I=1,5
         CCEN(I) = CEN(I)
      end do
      DO I=1,6
         CCEC(I) = CEC(I)
      end do
      DO I=1,6
         CCHN(I) = CHN(I)
         CCHC(I) = CHC(I)
      enddo
      DO I=1,3
         CCPW(I) = CPW(I)
         CCPWT(I)= CPWT(I)
      end do
      CCPWT(4)=CPWT(4)

      DO I=1,NLIM
        CALIM(I) = ALIM(I)
        CBLIM(I) = BLIM(I)
        CLIM_N(I) = LIM_N(I)
        CLIM_Q(I) = LIM_Q(I)
        CLIM_H(I) = LIM_H(I)
      end do
      CCLIM(1) = CLIM(1)
      CCLIM(2) = CLIM(2)
      CCLIM(3) = CLIM(3)
      CCLIM(4) = CLIM(4)
      CNLIM = NLIM

      TTAIR = TAIR                ! valori adimensionali
      TTIN  = TIN
      PPOUT = POUT
      PPIN  = 0.
      EEFF_CORR = EFF_CORR
      PPOW_CORR = POW_CORR
	nnom_power=nom_power
c
        igrtype = 1 ! solo rappresentazione nel piano (H,QM)
C-ELENA-PROVA
C      CALL DESIGN(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
C     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
C     *            CLIM,TIN,POUT,FUN,
C     *            FLAG,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
C     *            ptr,un_q,un_h)
C      IF (CLIM(3).GT. CLIM(4)) THEN
C        CALL DESIGN_CH(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
C     *                 NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
C     *                 CLIM,TIN,POUT,FUN,
C     *                 FLAG_ch,na_ch,nb_ch,qa_ch,ha_ch,qb_ch,hb_ch,
C     *                 nta_ch,ntb_ch,pta_ch,ptb_ch,
C     *                 ptr_ch,un_q_ch,un_h_ch)
C      ENDIF
      call DESIGN_UNICA(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *            CLIM,TIN,POUT,FUN,
     *            FLAG,flag_out,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *            ptr,un_q,un_h)

      RETURN
      END
C=============================================================================
      SUBROUTINE PWMAXC2(FLAG,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *           NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *           CLIM,CPW,CPWT,TIN,TAIR,PIN,
     *           EFF_CORR,POW_CORR,nom_power,
     *           na,nb,qa,ha,qb,hb,
     *           nta,ntb,pta,ptb,ptr,un_q,un_h,flag_ch,
     *           na_ch,nb_ch,qa_ch,ha_ch,qb_ch,hb_ch,
     *           nta_ch,ntb_ch,pta_ch,ptb_ch,
     *           ptr_ch,un_q_ch,un_h_ch)

***************************************************************
C     CURVE DI MASSIMA POTENZA DELLE TURBINE RIPORTATE SULLE CARATTERISTICHE
C     DEI COMPRESSORI A PRESSIONE DI INGRESSO ASSEGNATA
C-----------------------------------------------------------------------------
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 nlim
c
      real*4 pow_corr,eff_corr,nom_power,
     *       max_rev,min_rev,CHN(*),CHC(*),CEN(*),CEC(*),
     *       alim(*),blim(*),lim_n(*),lim_q(*),lim_h(*),
     *       clim(*),CPW(*),CPWT(*),tin,tair,pin,pout
c
c     preparazione del common per la FUN2(H)
      real*4 qq,ppow_corr,eeff_corr,CCHN(6),CCHC(6),CCEN(5),CCEC(6),
     *       cnlim,calim(max_nlim),cblim(max_nlim),
     *       clim_n(max_nlim),clim_q(max_nlim),clim_h(max_nlim),
     *       cclim(4),CCPW(3),CCPWT(4),nnom_power,
     *       ppwt,ttin,ttair,ppin,ppout
c
      COMMON/PWAUX/QQ,CCEN,CCEC,CCHN,CCHC,CNLIM,CALIM,CBLIM,CCLIM,
     *             CLIM_N,CLIM_Q,CLIM_H,PPWT,TTIN,PPOUT,
     *             EEFF_CORR,PPIN,CCPW,CCPWT,
     *             TTAIR,PPOW_CORR,nnom_power
c
c     vettori e dimensioni 
c     per la rappresentazione grafica della curva di massima potenza
c
      real*4 qa(*),ha(*),qb(*),hb(*)
	real*4 un_q(*),un_h(*)
	integer*4 na,nb,ptr,nta,ntb,pta(*),ptb(*)
      real*4 qa_ch(*),ha_ch(*),qb_ch(*),hb_ch(*)
	real*4 un_q_ch(*),un_h_ch(*)
	integer*4 na_ch,nb_ch,ptr_ch,nta_ch,ntb_ch,pta_ch(*),ptb_ch(*)
c      
      LOGICAL*2  FLAG,INPW,flag_ch,flag_out
C
      integer*4 i
	real*4 FUN2
      EXTERNAL FUN2
C----------------------------------------------------------------------------
c                               FUN2(H)
      INPW  = .true.
C----> DEFINIZIONE DELL'AREA COMMON PER LA FUNCTION FUN2
C
      DO I=1,5
         CCEN(I) = CEN(I)
      end do
      DO I=1,6
         CCEC(I) = CEC(I)
      end do
      DO I=1,6
         CCHN(I) = CHN(I)
         CCHC(I) = CHC(I)
      enddo
      DO I=1,3
         CCPW(I)= CPW(I)
         CCPWT(I)=CPWT(I)
      end do
      CCPWT(4)=CPWT(4)

      DO I=1,NLIM
        CALIM(I) = ALIM(I)
        CBLIM(I) = BLIM(I)
        CLIM_N(I) = LIM_N(I)
        CLIM_Q(I) = LIM_Q(I)
        CLIM_H(I) = LIM_H(I)
      end do
      CCLIM(1) = CLIM(1)
      CCLIM(2) = CLIM(2)
      CCLIM(3) = CLIM(3)
      CCLIM(4) = CLIM(4)
      CNLIM = NLIM

      TTAIR = TAIR
      TTIN  = TIN
      EEFF_CORR = EFF_CORR
      PPOW_CORR = POW_CORR
      PPIN = PIN
      PPOUT = 0.
c
      igrtype = 1    ! rappresentazione assoluta nel piano (H,QM)
c
c      CALL DESIGN(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
c     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
c     *            CLIM,TIN,PIN,FUN2,
c     *            FLAG,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
c     *            ptr,un_q,un_h)
c      IF (CLIM(3).GT. CLIM(4)) THEN

c        CALL DESIGN_CH(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
c     *                 NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
c     *                 CLIM,TIN,PIN,FUN2,
c     *                 FLAG_ch,na_ch,nb_ch,qa_ch,ha_ch,qb_ch,hb_ch,
c     *                 nta_ch,ntb_ch,pta_ch,ptb_ch,
c     *                 ptr_ch,un_q_ch,un_h_ch)
c      ENDIF
      call DESIGN_UNICA(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *            CLIM,TIN,Pin,FUN2,
     *            FLAG,flag_out,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *            ptr,un_q,un_h)


      RETURN
      END
c-------------------------------------------------------------------------------
      SUBROUTINE PWMAXC1_new(FLAG,flag_out,MAX_REV,MIN_REV,CHN,CHC,CEN,
     *           cec,NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *           CLIM,CPW,CPWT,TIN,TAIR,POUT,
     *           EFF_CORR,POW_CORR,nom_power,
     *           na,nb,qa,ha,qb,hb,
     *           nta,ntb,pta,ptb,ptr,un_q,un_h)

c*******************************************************************
C     CURVE DI MASSIMA POTENZA DELLE TURBINE RIPORTATE SULLE CARATTERISTICHE
C     DEI COMPRESSORI A PRESSIONE DI USCITA ASSEGNATA.
C-----------------------------------------------------------------------------
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 nlim
c
      real*4 pow_corr,eff_corr,nom_power,
     *       max_rev,min_rev,CHN(*),CHC(*),CEN(*),CEC(*),
     *       alim(*),blim(*),lim_n(*),lim_q(*),lim_h(*),
     *       clim(*),CPW(*),CPWT(*),tin,tair,pin,pout
c
c     preparazione del common per la FUN(pin)
      real*4 qq,ppow_corr,eeff_corr,CCHN(6),CCHC(6),CCEN(5),CCEC(6),
     *       cnlim,calim(max_nlim),cblim(max_nlim),
     *       clim_n(max_nlim),clim_q(max_nlim),clim_h(max_nlim),
     *       cclim(4),CCPW(3),CCPWT(4),
     *       nnom_power,ppwt,ttin,ttair,ppin,ppout
c
      COMMON/PWAUX/QQ,CCEN,CCEC,CCHN,CCHC,CNLIM,CALIM,CBLIM,CCLIM,
     *             CLIM_N,CLIM_Q,CLIM_H,PPWT,TTIN,PPOUT,
     *             EEFF_CORR,PPIN,CCPW,CCPWT,
     *             TTAIR,PPOW_CORR,nnom_power
c
      real*4 FUN
      EXTERNAL FUN      
      LOGICAL*2  FLAG,INPW,flag_out
c     vettori e dimensioni 
c     per la rappresentazione grafica della curva di massima potenza
c
      real*4 qa(*),ha(*),qb(*),hb(*)
	real*4 un_q(*),un_h(*)
	integer*4 na,nb,ptr,nta,ntb,pta(*),ptb(*)

c	
      integer*4 i
c----------------------------------------------------------------------------
      INPW  = .FALSE.
C
C----> DEFINIZIONE DELL'AREA COMMON PER LA FUNCTION FUN
      DO I=1,5
         CCEN(I) = CEN(I)
      end do
      DO I=1,6
         CCEC(I) = CEC(I)
      end do
      DO I=1,6
         CCHN(I) = CHN(I)
         CCHC(I) = CHC(I)
      enddo
      DO I=1,3
         CCPW(I) = CPW(I)
         CCPWT(I)= CPWT(I)
      end do
      CCPWT(4)=CPWT(4)

      DO I=1,NLIM
        CALIM(I) = ALIM(I)
        CBLIM(I) = BLIM(I)
        CLIM_N(I) = LIM_N(I)
        CLIM_Q(I) = LIM_Q(I)
        CLIM_H(I) = LIM_H(I)
      end do
      CCLIM(1) = CLIM(1)
      CCLIM(2) = CLIM(2)
      CCLIM(3) = CLIM(3)
      CCLIM(4) = CLIM(4)
      CNLIM = NLIM

      TTAIR = TAIR                ! valori adimensionali
      TTIN  = TIN
      PPOUT = POUT
      PPIN  = 0.
      EEFF_CORR = EFF_CORR
      PPOW_CORR = POW_CORR
	nnom_power=nom_power
c
        igrtype = 1 ! solo rappresentazione nel piano (H,QM)

      call DESIGN_UNICA(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *            CLIM,TIN,POUT,FUN,
     *            FLAG,flag_out,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *            ptr,un_q,un_h)

      RETURN
      END
c------------------------------------------------------------------------------
      SUBROUTINE PWMAXC2_new(FLAG,flag_out,MAX_REV,MIN_REV,CHN,CHC,CEN,
     *           cec,NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *           CLIM,CPW,CPWT,TIN,TAIR,PIN,
     *           EFF_CORR,POW_CORR,nom_power,
     *           na,nb,qa,ha,qb,hb,
     *           nta,ntb,pta,ptb,ptr,un_q,un_h)

***************************************************************
C     CURVE DI MASSIMA POTENZA DELLE TURBINE RIPORTATE SULLE CARATTERISTICHE
C     DEI COMPRESSORI A PRESSIONE DI INGRESSO ASSEGNATA
C-----------------------------------------------------------------------------
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
      integer*4 nlim
c
      real*4 pow_corr,eff_corr,nom_power,
     *       max_rev,min_rev,CHN(*),CHC(*),CEN(*),CEC(*),
     *       alim(*),blim(*),lim_n(*),lim_q(*),lim_h(*),
     *       clim(*),CPW(*),CPWT(*),tin,tair,pin,pout
c
c     preparazione del common per la FUN2(H)
      real*4 qq,ppow_corr,eeff_corr,CCHN(6),CCHC(6),CCEN(5),CCEC(6),
     *       cnlim,calim(max_nlim),cblim(max_nlim),
     *       clim_n(max_nlim),clim_q(max_nlim),clim_h(max_nlim),
     *       cclim(4),CCPW(3),CCPWT(4),nnom_power,
     *       ppwt,ttin,ttair,ppin,ppout
c
      COMMON/PWAUX/QQ,CCEN,CCEC,CCHN,CCHC,CNLIM,CALIM,CBLIM,CCLIM,
     *             CLIM_N,CLIM_Q,CLIM_H,PPWT,TTIN,PPOUT,
     *             EEFF_CORR,PPIN,CCPW,CCPWT,
     *             TTAIR,PPOW_CORR,nnom_power
c
c     vettori e dimensioni 
c     per la rappresentazione grafica della curva di massima potenza
c
      real*4 qa(*),ha(*),qb(*),hb(*)
	real*4 un_q(*),un_h(*)
	integer*4 na,nb,ptr,nta,ntb,pta(*),ptb(*)
c      
      LOGICAL*2  FLAG,INPW,flag_out
C
      integer*4 i
	real*4 FUN2
      EXTERNAL FUN2
C----------------------------------------------------------------------------
c                               FUN2(H)
      INPW  = .true.
C----> DEFINIZIONE DELL'AREA COMMON PER LA FUNCTION FUN2
C
      DO I=1,5
         CCEN(I) = CEN(I)
      end do
      DO I=1,6
         CCEC(I) = CEC(I)
      end do
      DO I=1,6
         CCHN(I) = CHN(I)
         CCHC(I) = CHC(I)
      enddo
      DO I=1,3
         CCPW(I)= CPW(I)
         CCPWT(I)=CPWT(I)
      end do
      CCPWT(4)=CPWT(4)

      DO I=1,NLIM
        CALIM(I) = ALIM(I)
        CBLIM(I) = BLIM(I)
        CLIM_N(I) = LIM_N(I)
        CLIM_Q(I) = LIM_Q(I)
        CLIM_H(I) = LIM_H(I)
      end do
      CCLIM(1) = CLIM(1)
      CCLIM(2) = CLIM(2)
      CCLIM(3) = CLIM(3)
      CCLIM(4) = CLIM(4)
      CNLIM = NLIM

      TTAIR = TAIR
      TTIN  = TIN
      EEFF_CORR = EFF_CORR
      PPOW_CORR = POW_CORR
      PPIN = PIN
      PPOUT = 0.
c
      igrtype = 1    ! rappresentazione assoluta nel piano (H,QM)
c
      call DESIGN_UNICA(INPW,MAX_REV,MIN_REV,CHN,CHC,CEN,CEC,
     *            NLIM,ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *            CLIM,TIN,Pin,FUN2,
     *            FLAG,flag_out,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *            ptr,un_q,un_h)


      RETURN
      END
c-------------------------------------------------------------------------------
      Subroutine max_power_new(compos,altitude,power_loss,eff_corr,
     *                    max_rev,min_rev,CHN,CHC,CEN,CEC,
     *                    nlim,alim,blim,lim_n,lim_q,lim_h,
     *                    clim,CPW,CPWT,nom_rev,nom_power,
     *                    tin,tair,pin,pout,
     *                    flag,flag_out,na,nb,qa,ha,qb,hb,
     *                    nta,ntb,pta,ptb,ptr,un_q,un_h)
c**********************************************************************
c    calcola i punti per tracciare sulla mappa del compressore le curve 
c    di massima potenza della turbina ad esso accoppiata per fissata 
c    pressione di ingresso o di uscita
c*********************************************************************** 
      implicit none

      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/td.inc'		!:IN
      include '../inc/cc.inc'		!:IN
      include '../inc/staznw_par.inc'		!:IN
c
      real*4 compos(*),  
     *       altitude,   
     *       power_loss, 
     *       eff_corr,   
     *       max_rev,    
     *       min_rev,    
     *       CHN(*),     
     *       CHC(*),     
     *       CEN(*),     
     *       CEC(*)      
c
      integer*4 nlim     

      real*4 alim(*),    
     *	   blim(*),    
     *	   lim_n(*),   !I/O)
     *       lim_q(*),   !I/O)
     *       lim_h(*),   !I/O)
     *       clim(*),    
     *       CPW(*),
     *       CPWT(*),
     *       nom_rev,
     *       tin,     !I/O)
     *       tair,    !I/O)
     *       pin,     !I/O)
     *       pout     !I/O)
c                         per il grafico  --OUTPUT--
      LOGICAL*2   FLAG,flag_out
c------>punti curva max_power nella zona normale
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(*),ha(*), !coordinate HQ per disegnare il ramo A
	                    !(vettori di dim=na) dimensionali
     *       qb(*),hb(*)  !coordinate HQ per disegnare il ramo B 
	                    !(vettori di dim=nb) dimensionali
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(*),      !numero di punti su ogni tronco del ramo A
     *          ptb(*)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(*),un_q(*)  !coordinate dei pti di raccordo 
                              !(dim=ptr) dimensionali
                              !(dim=ptr) dimensionali
      real*4 nom_power
c------------------------->variabili locali

      real*4 power_corr,del
c*
      integer*4 ind           !offset elem.top.fittizio
c                            
	integer*4 i,j         
	real*4 comp_top(1,M_COMP)
!	real*4 comp_top(m_ogg,M_COMP)

      integer*4 nlim_app      
      real*4 lim_n_app(max_nlim),     
     *	 lim_q_app(max_nlim)     
      real*4 lim_h_app(max_nlim)      
      real*4 alim_app(max_nlim+1),      
     *     blim_app(max_nlim+1)       


c----------------------------------------------------------
!      call ini_par()
      maxit = 100
c     altitude (m)
c     eff_corr (%)
c     power_loss (%)
c     max_rev, min_rev (%N)
c     tin,tair (K)
c     pin,pout (bar)
c     compos (M_comp)
c     nom_rev (da maschera)
c     occorre adimensionalizzare i vettori lim_q,lim_h,lim_n perchè dalla dll vengono 
c     calcolati dimensionali
c-old
c     max_rev = max_rev/100. !ricevo i valori gia normalizzati
c	min_rev = min_rev/100.
c-old
	do j=1,nlim
	   lim_q(j)=lim_q(j)/qmrif/1000.   !ricevo in m^3/h e trasformo in km^3/h
	   lim_h(j)=lim_h(j)/headrif/1000. !ricevo in m e trasformo in km
	   lim_n(j)=lim_n(j)/nom_rev       !ricevo rpm e trasformo in %N
	end do

	do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_h_app(i)=lim_h(i)
	   alim_app(i)=alim(i)
	   blim_app(i)=blim(i)
	end do
	nlim_app=nlim

      call filtra_plim(nlim,lim_n,lim_q,lim_h,alim,blim)

c-17 gennaio
	alim(nlim+1)=alim(nlim)
	blim(nlim+1)=blim(nlim)
c-17 gennaio

c-old	eff_corr = eff_corr/100.
c-old power_loss = power_loss/100.
      DEL    = 1.-1.165E-4*ALTITUDE + 4.67E-9*ALTITUDE**2
c      POWER_CORR = DEL*(1.-POWER_LOSS)
c-elena-5/12/2005
c NOTA: in input viene letta la variazione di potenza, nelle simulazioni e'
c       utilizzata per variare la massima potenza e contiene la correzione
c       dovuta all'altitudine (delta) e la variazione (power_loss) data a
c       livello topologico
      POWER_CORR = DEL*(1.+POWER_LOSS)
c-elena-5/12/2005
	tin  = tin/(TRIF+T0)
	tair = tair/(TRIF+T0)
c
      ind = 1
      call definisci_cost_cc
	do i=1,M_COMP
c-old	  comp_top(ind,i) = compos(i)/100. !ricevo una %
	  comp_top(ind,i) = compos(i) !ricevo una %N
	end do
      call par_comp_rk(ind,1,comp_top)

		do i = 1, M_COMP
           ctpctd(ind) = ctpctd(ind) + 
     -                   comp_top (ind,i)* ctpccc (i)
           ctvctd(ind) = ctvctd(ind) + 
     -                   comp_top (ind,i) * ctvccc (i)
		enddo  ! i

	call regre_exp(ind)
c	if (ier.gt.0) then
c	  return
c	end if
      if (pout.GT.0) THEN
c 	 pout = pout/((PRIF+P0)*PMKS)   !ricevo bar 
 	 pout = pout/((PRIF+P0))  
       CALL PWMAXC1_new(FLAG,flag_out,MAX_REV,MIN_REV,
     *               CHN,CHC,CEN,CEC,NLIM,
     *               ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *               CLIM,CPW,CPWT,tin,tair,
     *               pout,EFF_CORR,POWER_CORR,nom_power,
     *               na,nb,qa,ha,qb,hb,
     *               nta,ntb,pta,ptb,ptr,un_q,un_h)

      ELSE
c-elena 	  pin = pin/((PRIF+P0)*PMKS)   
 	  pin = pin/((PRIF+P0))   !ricevo bar 
	  CALL PWMAXC2_new(FLAG,flag_out,MAX_REV,MIN_REV,
     *              CHN,CHC,CEN,CEC,NLIM,
     *              ALIM,BLIM,LIM_N,LIM_Q,LIM_H,
     *              CLIM,CPW,CPWT,tin,tair,
     *              pin,EFF_CORR,POWER_CORR,nom_power,
     *              na,nb,qa,ha,qb,hb,
     *              nta,ntb,pta,ptb,ptr,un_q,un_h)

      end if
c
c      flag = FALSE -->  then 'curva OUT RANGE'
c
      RETURN
      END