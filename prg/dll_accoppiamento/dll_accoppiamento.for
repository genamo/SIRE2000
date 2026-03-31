!  dll_accoppiamento.for
!
!  FUNCTIONS/SUBROUTINES exported from DLL_ACCOPPIAMENTO.dll:


c********************************************************************************
!	DLL_MAX_POWER      - subroutine 
!
      Subroutine dll_max_power(compos,altitude,power_loss,eff_corr,
     *                    max_rev,min_rev,chn,chc,cen,cec,
     *                    nlim,alim,blim,lim_n,lim_q,lim_h,
     *                    clim,cpw,cpwt,nom_rev,nom_power,
     *                    tin,tair,pin,pout,
     *                    flag,flag_out,na,nb,qa,ha,qb,hb,
     *                    nta,ntb,pta,ptb,ptr,un_q,un_h)
C
! Expose subroutine DLL_MAX_POWER to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_MAX_POWER

c     Variables declarations
      implicit none
C
      integer*4 max_np,max_nt,max_nlim
	parameter (max_np=100,max_nt=100,max_nlim=20)
c     
c-----------------------------> INPUT <----------------------------------------
C
c------------------>CONDIZIONI OPERATIVE
c
	real*4 compos(10),!composizione del gas:(%)
	                  !1)meth(matano) 2)eth(etano) 3)prop(propano)
	                  !4)n_but(n-butano) 5)i_but(i-butano)
	                  !6)pent(pentano) 7)hexa(esano) 8)n2(azoto)
	                  !9)co2(anidride carbonica) 10)h2s(acido solfidrico)
     *      altitude,   !altezza slm in cui si vuole studiare l'accoppiamento
     *      power_loss, !variazione di potenza associata all'accoppiamento (%)
     *      eff_corr    !variazione di efficienza associata all'accoppiamento(%)
c***************************************************************************
c   Le quattro variabili seguenti sono di INPUT/OUTPUT

      real*4 tin,    !temperatura d'ingresso del gas (K)    
     *       tair    !temperatura dell'aria (K)
c     
      real*4 pin,    !pressione d'ingresso del gas (Pa)
     *       pout    !pressione d'uscita del gas (Pa)
	               !(viene assegnata o l'una o l'altra di queste grandezze,
	               !che avrà quindi valore positivo mentre l'altra sarà nulla)
C*********************************************************************************
c---------------------------> VARIABILI PER COMPRESSORE    
C
      real*4 max_rev  !numero di giri massimo (%)
     *      ,min_rev  !numero di giri minimo (%)
     *      ,nom_rev  !numero di giri nominale (rpm)
     *	  ,chn(6)   !coefficienti di h=f(n,q) nella zona normale 
     *	  ,chc(6)   !coefficienti di h=f(n,q) nella zona di choking 
     *	  ,cen(5)   !coefficienti di eff=f(n,q) nella zona normale 
     *	  ,cec(6)   !coefficienti di eff=f(n,q) nella zona di choking 
c                   (i 4 set di coefficienti si ottengono per regressione)
c
      integer*4 nlim !numero di punti limite sulla spezzata anti-sourge
c
      real*4 alim(max_nlim+1) !vettore  termini noti della spezzata di surge
     *      ,blim(max_nlim+1) !vettore  coeff angolari della spezzata di surge
c***********************************************************************************
c I vettori lim_n,lim_q,lim_h sono di INPUT/OUTPUT

     *      ,lim_n(max_nlim)!vettore dei valori estremi del numero di giri   
     *      ,lim_q(max_nlim)!vettore dei valori estremi della portata        
     *      ,lim_h(max_nlim)!vettore dei valori estremi della prevalenza 
c***********************************************************************************          
c          (vettori di dimensione effettiva pari a nlim;grandezze dimensionali
c           lette in parte da maschera e in parte ottenute da regressione)
     *	  ,clim(4) !costanti delle curve limite del compressore
c	     (vettore ottenuto da regressione)
c
c---------------------------> VARIABILI PER TURBINA    
C
      real*4 cpw(3)    !coefficienti per il calcolo della potenza
     *      ,cpwt(4)   !coefficienti correttivi per la potenza 
c	     (vettori ottenuti da regressione)
     *      ,nom_power !potenza nominale della turbina (kW)
C
C-------------------------------->OUTPUT<-----------------------------------------
C
      logical*2 flag  !Indica se la curva è OUTofRANGE
	                !.TRUE. ho abb pti per disegnare la curva di accoppiamento
	                !.FALSE. non ho abb p.ti per disegnare la curva di accoppiamento
                      !Indica il tipo di OUTofRANGE 
     *         ,flag_out !.TRUE. :out_of_range negativo->accoppiamento non buono
     *                   !.FALSE.:out_of_range positivo->buon accoppiamento
c
c------>punti curva max_power nella zona normale
c
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(max_np),ha(max_np), !coordinate HQ per disegnare il ramo A
	                              !(vettori di dim=na)
     *       qb(max_np),hb(max_np)  !coordinate HQ per disegnare il ramo B 
	                              !(vettori di dim=nb)
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(max_nt),      !numero di punti su ogni tronco del ramo A
     *       ptb(max_nt)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(max_np),un_q(max_np) !coordinate dei pti di raccordo 
                                       !(dim=ptr)
c
c

      call max_power_new(compos,altitude,power_loss,eff_corr,
     *                    max_rev,min_rev,CHN,CHC,CEN,CEC,
     *                    nlim,alim,blim,lim_n,lim_q,lim_h,
     *                    clim,CPW,CPWT,nom_rev,nom_power,
     *                    tin,tair,pin,pout,
     *                    flag,flag_out,na,nb,qa,ha,qb,hb,
     *                    nta,ntb,pta,ptb,ptr,un_q,un_h)

      return
	end   !subroutine DLL_MAX_POWER
c
c*******************************************************************************
c
!	DLL_TRONCO      - subroutine 
!
      subroutine dll_tronco(npt,nt,pt,q,h,npoint,qt,ht)


C
! Expose subroutine DLL_TRONCO to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_TRONCO
c
      implicit none
c
      integer*4 max_nt,      !massimo numero di tronchi calcolabili
     *          max_np	
	parameter (max_nt=100,max_np=100)
c
c------>INPUT
c
	integer*4 npt        !numero punti sul ramo analizzato
	integer*4 pt(max_nt) !vettore punti calcolati sui vari tronchi
	real*4 q(max_np),    !ascisse punti sul ramo analizzato
     *       h(max_np)     !ordinate punti sul ramo analizzato
c
c------>OUTPUT
c
      integer*4 npoint(max_nt) !numero di punti sul singolo tronco (dim=nt)
      real*4 qt(max_nt,max_np),!ascisse punti sul singolo tronco
     *       ht(max_nt,max_np)!ordinate punti sul singolo tronco 
c------>INPUT/OUTPUT
	integer*4 nt         !numero tronchi sul ramo analizzato

c ***********************************************************************  

      call tronco(npt,nt,pt,q,h,npoint,qt,ht)

      return
	end    !dll_tronco