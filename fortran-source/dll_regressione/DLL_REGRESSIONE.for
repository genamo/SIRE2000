!  dll_regressione.for 
!
!  FUNCTIONS/SUBROUTINES exported from DLL_REGRESSIONE.dll:


!	DLL_C_COEF_COMPR      - subroutine 
!
	subroutine DLL_C_COEF_COMPR(nom_rev,max_rev,min_rev,chlim_q,
     *                    	chlim_h,n_char,rev,flow,head,eff,nlim,
     *                        lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                        alim,blim,clim,ier,iwr)

  ! Expose subroutine DLL_C_COEF_COMPR to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_C_COEF_COMPR

  ! Variables
	implicit none

      integer*4 max_n_char,max_nlim
	parameter (max_n_char=50,max_nlim=20)
c
	real*4 nom_rev        !valore nominale del numero di giri
c
      real*4 max_rev,       !numero di giri massimo rispetto al valore nominale
     *       min_rev        !numero di giri minimo rispetto al valore nominale
c
      real*4 chlim_q,       !portata del punto limite di choking
     *       chlim_h        !prevalenza del punto limite di choking
c
      integer*4 n_char      !punti assegnati per effettuare la regressione
c                   
                            !grandezze relative ai punti assegnati:
      real*4 rev(max_n_char),       !numero di giri (dim=n_char,max_n_char=50)
     *       flow(max_n_char),      !portata (dim=n_char)
     *       head(max_n_char),      !altezza adiabatica (dim=n_char)
     *       eff(max_n_char)        !efficienza (dim=n_char)
c
      integer*4 nlim       !punti limite assegnati per definire le spezzate 
	                     !di anti-surge
c----------------------->Input/Output  
c
c     caso 1- se nlim>0 le spezzate di surge vengono definite in input ----->
c             lim_n,lim_q sono input
c     caso 2- se nlim=0 le spezzate di surge non vengono definite in input e quindi 
c             le definisco da programma------>lim_n,lim_q sono output
c
                            !grandezze relative ai punti limite assegnati:
      real*4 lim_n(max_nlim),     !numero di giri(dim=nlim,max_n_lim=20)
     *	   lim_q(max_nlim)      !portata (dim=nlim)
c----------------------->Output
      real*4 chlim_n        !numero di giri corrispondente al pto limite di choking 
c  
      real*4 lim_h(max_nlim)      !altezza adiabatica dei punti limite
c
c                          !coefficienti per calcolare:
      real*4 chn(6),       !altezza adiabatica nella zona normale (dim=6)
     *       chc(6),       !altezza adiabatica nella zona di choking (dim=6)
     *       cen(5)        !efficienza nella zona normale (dim=5)
c
                         !coefficienti delle rette che descrivono la spezzata di 
					   !intervento anti pompaggio:
      real*4 alim(max_nlim+1),      !termine noto (dim=nlim+1)
     *       blim(max_nlim+1)       !coefficiente angolare(dim=nlim+1)
c
      real*4 clim(4)       !limiti anti-choke (dim=4)
c
	integer*4 ier         !codice d'errore
c-ele 30/8/2006
	integer*4 iwr         !codice di warning

c
c------------------->variabili locali
c
      character err_msg*(200)!messaggio che descrive l'esito della regressione e
	                     !gli eventuali errori commessi



  ! Body of DLL_C_COEF_COMPR
      call c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                  chlim_h,n_char,rev,flow,head,eff,nlim,
     *                  lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                   alim,blim,clim,ier,err_msg,iwr)


	return
	end !subroutine DLL_C_COEF_COMPR



!	DLL_COEF_TURB      - subroutine 
!
	subroutine DLL_C_COEF_TURB(nom_tair,nom_power,nom_hrate,
     *	                     ncorr,temp,fpw,fhr,
     *                         nmaxp,rev_pwmax,pwmax,airpwm,
     *                         nhrate,rev_hrate,power,airhrt,hrate,
     *                         cpw,cpwt,chr,chrt,jer)

  ! Expose subroutine DLL_C_COEF_TURB to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_C_COEF_TURB

  ! Variables
	implicit none

      integer*4 max_n_char
	parameter (max_n_char=50)
c
      real*4 nom_tair,         !valore nominale della temperatura dell'aria
     *       nom_power,        !valore nominale della potenza
     *	   nom_hrate         !valore nominale dell'heat-rate
c
c           Dati per la regressione dei coefficienti correttivi
c
      integer*4 ncorr       !punti assegnati,max_ncorr=50  
                            !grandezze relative a tali punti:
      real*4 temp(max_n_char),!temperatura;
     *       fpw(max_n_char), !fattore di correzione per la potenza fpw=f(temp)
     *       fhr(max_n_char)  !fattore di correzione per l'heat rate fhr=f(temp)
	                      
c
c           Dati per la regressione dei coefficienti CPW   
c
      integer*4 nmaxp        !punti assegnati
                             !grandezze relative a tali punti:
	real*4 rev_pwmax(max_n_char), !numero di giri; 
     *       pwmax(max_n_char),     !potenza massima
     *       airpwm(max_n_char)     !temperatura dell'aria
c
c           Dati per la regressione dei coefficienti CHR
c
      integer*4 nhrate        !punti assegnati
                              !grandezze relative a tali punti:
	real*4 rev_hrate(max_n_char), !numero di giri; 
     *       power(max_n_char),     !potenza massima;
     *       airhrt(max_n_char),    !temperatura dell'aria;
     *       hrate(max_n_char)      !heat rate

c------------------->Output
      real*4 cpw(3),      !coefficienti per il calcolo della potenza massima
     *       cpwt(4),     !coefficienti per il calcolo del fattore correttivo
	                    !per la potenza
     *       chr(6),      !coefficienti per il calcolo dell'heat rate
     *       chrt(4)      !coefficienti per il calcolo del fattore correttivo 
	                    !per l'heat rate
c
      integer*4 jer           !indice d'errore
c
c------------------------------------>variabili locali
c
      character*(200) error_msg  !messaggio che descrive l'esito della regressione
                             !e gli eventuali errori commessi
c   


  ! Body of DLL_C_COEF_TURB
      call c_coef_turb(nom_tair,nom_power,nom_hrate,
     *	             ncorr,temp,fpw,fhr,
     *                 nmaxp,rev_pwmax,pwmax,airpwm,
     *                 nhrate,rev_hrate,power,airhrt,hrate,
     *                 cpw,cpwt,chr,chrt,jer,error_msg)



	end !subroutine DLL_C_COEF_TURB


