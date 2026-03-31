!  dll_analisi_statistica.for 
!
!  FUNCTIONS/SUBROUTINES exported from DLL_ANALISI_STATISTICA.dll:


!	DLL_DATI_COMPR      - subroutine 
!
      subroutine dll_dati_compr(nom_rev,nom_flow,nom_head,max_rev,
     *                   	min_rev,min_eff,npoint,rev,flow,head,eff,
     *                      nlim,lim_n,lim_q,lim_h,alim,blim,clim,
     *                      chlim_n,chlim_q,chlim_h,chn,chc,cen,
     *                      cec,new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e,
     *                      delta,delta_eff,delta_max,delta_max_eff,
     *                      idelta_max,idelta_max_eff,codice) 

  ! Expose subroutine DLL_DATI_COMPR to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_DATI_COMPR

  ! Variables
      implicit none
      integer*4 max_npoint
	parameter (max_npoint=50)
c
      integer*4 max_nlim
	parameter (max_nlim=20)
c
      real*4 nom_rev,          !numero di giri
     *     nom_flow,         !portata
     *     nom_head          !altezza adiabatica
c
      real*4 max_rev,          !numero di giri massimo
     *     min_rev,          !numero di giri minimo
     *     min_eff           !efficienza minima
c
      integer*4 npoint         !punti di lavoro assegnati
c
                             !grandezze relative ai punti assegnati:
	real*4 rev(max_npoint),           !numero di giri
     *     flow(max_npoint),          !portata
     *     head(max_npoint),          !altezza adiabatica
     *     eff(max_npoint)            !efficienza
c                             !vettori di dimensione npoint
c
      integer*4 nlim           !punti limite assegnati
c
                             !grandezze relative ai punti limite assegnati
      real*4 lim_n(max_nlim),         !numero di giri
     *     lim_q(max_nlim),         !portata
     *     lim_h(max_nlim)          !altezza adiabatica
	                       !(vettori di dimensione nlim)
c
                             !grandezze relative alla spezzata anti-surge:
      real*4 alim(max_nlim+1),          !termini noti
     *     blim(max_nlim+1)           !coefficienti angolari
                             !(vettori di dimensione nlim+1)
c
      real*4 clim(4)           !costanti delle curve limite (dim=4)
	                       !zona di choking per l'efficienza:
	                       !(1)curva di choking,(2)curva operating limit
	                       !zona di choking per i giri:
	                       !(1)curva di choking,(2)curva operating limit
c
                             !grandezze relative al punto limite di choking:
	real*4 chlim_n,          !numero di giri
     *     chlim_q,          !portata
     *     chlim_h           !altezza adiabatica
c
      real*4 chn(6),           !coeff di h=f(n,q) nella zona normale (dim=6)
     *     chc(6),           !coeff di h=f(n,q) nella zona di choking (dim=6)
     *     cen(5)           !coeff di eff=f(n,q) nella zona normale (dim=5)
c-------------------->OUTPUT
      real*4 cec(6)            !coefficienti di eff=f(n,q) nella zona di choking
c                            !dim=6
	                       !grandezze relative ai punti calcolati:
      real*4 new_rev(max_npoint),       !numero di giri
     *     new_eff(max_npoint)        !efficienza      (dim=npoint)
c
                             !valori statistici:
	real*4 sq1_r,            !deviazione standard per i giri
     *     sq2_r,            !scostamento medio per i giri
     *     sq1_e,            !deviazione standard per l'efficienza
     *     sq2_e             !scostamento medio per l'efficienza
c
      real*4 delta(max_npoint),            !differenza giri assegnati e calcolati
     *     delta_eff(max_npoint)         !differenza efficienza assegnata e calcolata

      real*4 delta_max,        !max scostamento percentuale per i giri
     *     delta_max_eff     !max scostamento percentuale per l'efficienza
c
      integer*4 idelta_max,    !indice del punto col max scostamento per i giri
     *        idelta_max_eff !indice del punto col max scostamento per l'eff

      integer*4 codice
  ! Body of DLL_DATI_COMPR

     
      call dati_compr(nom_rev,nom_flow,nom_head,max_rev,min_rev,
     *                      min_eff,npoint,rev,flow,head,eff,
     *                      nlim,lim_n,lim_q,lim_h,alim,blim,clim,
     *                      chlim_n,chlim_q,chlim_h,chn,chc,cen,
     *                      cec,new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e,
     *                      delta,delta_eff,delta_max,delta_max_eff,
     *                      idelta_max,idelta_max_eff,codice) 


	return
	end !subroutine DLL_DATI_COMPR

c********************************************************************************

!	DLL_DATI_TURB      - subroutine 
!
      subroutine dll_dati_turb(nom_power,nom_hrate,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *            ncorr,temp_corr,fpw,fhr,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            new_fpw,new_fhr,
c-elena 21/12/05
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
     *            delta_fpw,delta_fhr)



  ! Expose subroutine DLL_DATI_TURB to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_DATI_TURB

  ! Variables
	implicit none
      integer*4 max_npoint
	parameter (max_npoint=50)

      real*4 nom_power,          !potenza massima
     *     nom_hrate           !heat rate
c
c                          !Tabella della potenza
c 
      integer*4 npwmax       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 rev_pwmax(max_npoint),   !numero di giri
     *     pwmax(max_npoint),       !massima potenza
     *     temp_pwmax(max_npoint)   !temperatura
	                     !(vettori di dimensione npwmax)
c
c                          !Tabella dello heat rate
c
      integer*4 nhrate       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 rev_hrate(max_npoint),   !numero di giri
     *     pw_hrate(max_npoint),    !massima potenza
     *     temp_hrate(max_npoint),  !temperatura
     *     hrate(max_npoint)        !heat rate
	                     !(vettori di dimensione nhrate)

	                     !tabella fattori correttivi
      integer*4 ncorr       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 temp_corr(max_npoint),    !temperatura
     *     fpw(max_npoint),            !correzione potenza
     *     fhr(max_npoint)             !correzione heat rate
	                     !(vettori di dimensione ncorr)

c
      real*4 cpw(3),      !coefficienti per il calcolo della potenza massima:
	                  !pw(n)=cpw(1)+cpw(2)*n+cpw(3)*n^2 (dim=3)
     *     cpwt(4),     !coefficienti per il calcolo del fattore correttivo
	                  !per la potenza (dim=4):
	                  !fpw(t)=cpwt(1)+cpwt(2)*t+cpwt(3)*t^2+cpwt(4)*t^3
     *     chr(6),      !coefficienti per il calcolo dell'heat rate:
	                  !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
	                  !         chr(5)*n^2+chr(6)*pw^2   (dim=6)
     *     chrt(4)      !coefficienti per il calcolo del fattore correttivo 
	                  !per l'heat rate (dim=4):
	                  !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
c------------>OUTPUT
      real*4 new_pwm(max_npoint),     !massima potenza 
	                          !(fisso temperatura e numero di giri)
     *     new_hrate(max_npoint),    !heat rate
	                          !(fisso temperatura,numero di giri e potenza)
     *     new_fpw(max_npoint),     !correzione potenza  
     *     new_fhr(max_npoint),      !correzione heat rate

     *     delta_p(max_npoint),
     *     delta_hr(max_npoint),
     *     delta_fpw(max_npoint),   !differenza valore di fpw calcolato e misurato
     *     delta_fhr(max_npoint)   !differenza valore di fhr calcolato e misurato

c
      real*4 st_dev_pwmax,        !deviazione standard per la massima potenza
     *     st_dev_hrate         !deviazione standard per lo heat rate
c--------------------------------------------------------------------------------


  ! Body of DLL_DATI_TURB
      call dati_turb(nom_power,nom_hrate,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *            ncorr,temp_corr,fpw,fhr,new_fpw,new_fhr,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
c-elena 21/12/05
     *            delta_fpw,delta_fhr)


      return
	end !subroutine DLL_DATI_TURB

c***********************************************************************************

!	DLL_PRINT_C      - subroutine 
!
      subroutine DLL_PRINT_C(unit,o_name_file,compr,comment,nom_rev,
     *            nom_flow,nom_head,max_rev,min_rev,min_eff,
     *            npoint,rev,flow,head,eff,nlim,lim_n,lim_q,lim_h,
     *            alim,blim,clim,chlim_n,chlim_q,chlim_h,chn,chc,cen)

  ! Expose subroutine DLL_PRINT_C to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_PRINT_C

  ! Variables
      implicit none
      
      integer*4 unit  !identifica il file da aprire in scrittura con 
	                !l'istruzione fortran OPEN
      character*(150) o_name_file !percorso assoluto del file di output
c
      integer*4 max_npoint
	parameter (max_npoint=50)
c
      integer*4 max_nlim
	parameter (max_nlim=20)
c
      character compr*(16),   !compressore analizzato
     *          comment*(60)  !commento sul compressore analizzato
c
      real*4 nom_rev,          !numero di giri nominale
     *       nom_flow,         !portata nominsle
     *       nom_head          !prevalenza nominale
c
      real*4 max_rev,          !numero di giri massimo
     *       min_rev,          !numero di giri minimo
     *       min_eff           !efficienza minima
c
      integer*4 npoint         !punti di lavoro assegnati
c
                                  !grandezze relative ai punti assegnati:
	real*4 rev(max_npoint),     !numero di giri
     *       flow(max_npoint),    !portata
     *       head(max_npoint),    !altezza adiabatica
     *       eff(max_npoint)      !efficienza
	                            !(vettori di dimensione effettiva npoint)
c
      integer*4 nlim           !punti limite assegnati
c
                               !grandezze relative ai punti limite assegnati
      real*4 lim_n(max_nlim),  !numero di giri
     *       lim_q(max_nlim),  !portata
     *       lim_h(max_nlim)   !altezza adiabatica
	                         !(vettori di dimensione effettiva nlim)
c
                               !grandezze relative alla spezzata anti-surge:
      real*4 alim(max_nlim+1), !termini noti
     *       blim(max_nlim+1)  !coefficienti angolari
                               !(vettori di dimensione nlim+1)
c
      real*4 clim(4)           !costanti delle curve limite 
	                         !zona di choking per l'efficienza:
	                         !(1)curva di choking,(2)curva operating limit
	                         !zona di choking per i giri:
	                         !(1)curva di choking,(2)curva operating limit
c
                               !grandezze relative al punto limite di choking:
	real*4 chlim_n,          !numero di giri
     *       chlim_q,          !portata
     *       chlim_h           !altezza adiabatica
c
      real*4 chn(6),           !coeff curve a rev costante
     *       chc(6),           !coeff curve a rev costante-zona di choking-
     *       cen(5)            !coeff curve a eff costante 

c------------------------------------------------------------------------------------
  ! Body of DLL_PRINT_C

      call print_c(unit,o_name_file,compr,comment,nom_rev,nom_flow,
     *            nom_head,max_rev,min_rev,min_eff,npoint,rev,flow,head,
     *            eff,nlim,lim_n,lim_q,lim_h,alim,blim,clim,
     *            chlim_n,chlim_q,chlim_h,chn,chc,cen)

      return

	end  !DLL_PRINT_C

C************************************************************************************

!	DLL_PRINT_T     - subroutine 
!

      subroutine DLL_PRINT_T(unit,o_name_file,turb,comment_t,
     *            nom_rev_t,nom_power,nom_hrate,nom_tair,
     *            ncorr,temp_cor,pw_cor,hrate_cor,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *            cpw,cpwt,chr,chrt)

  ! Expose subroutine DLL_PRINT_T to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_PRINT_T

  ! Variables
      implicit none
      
      integer*4 unit  !identifica il file da aprire in scrittura con 
	                !l'istruzione fortran OPEN
      character*(150) o_name_file !nome del file di output
c
      character turb*(16),      !turbina analizzata
     *          comment_t*(60)    !commento sulla turbina analizzata
c
      integer*4 max_npoint
	parameter (max_npoint=50)
c
c                                !valori nominali:
      real*4 nom_rev_t,          !numero di giri
     *       nom_power,          !potenza 
     *       nom_hrate,          !heat rate
     *       nom_tair            !temperatura dell'aria
c
                             !Tabella delle correzioni
      integer*4 ncorr        !numero di punti assegnati      
c                                    !grandezze relative ai punti assegnati:
      real*4 temp_cor(max_npoint),    !temperatura
     *       pw_cor(max_npoint),      !fattore di correzione per la potenza
     *       hrate_cor(max_npoint)    !fattore di correzione per l'hrate
      	                          !(vettori di dimensione effettiva ncorr)
c
                             !Tabella della potenza
      integer*4 npwmax       !numero di punti assegnati
c
                                      !grandezze relative ai punti assegnati:
      real*4 rev_pwmax(max_npoint),   !numero di giri
     *       pwmax(max_npoint),       !massima potenza
     *       temp_pwmax(max_npoint)   !temperatura
                                    !(vettori di dimensione effettiva npwmax)

                           !Tabella dello heat rate
c
      integer*4 nhrate       !numero di punti assegnati
                                      !grandezze relative ai punti assegnati
      real*4 rev_hrate(max_npoint),   !numero di giri
     *       pw_hrate(max_npoint),    !massima potenza
     *       temp_hrate(max_npoint),  !temperatura
     *       hrate(max_npoint)        !heat rate
                                    !(vettori di dimensione effettiva nhrate)

c
      real*4 cpw(3),    !coefficienti curva max_power 
     *       cpwt(4),   !coefficienti curva di correzione potenza
     *       chr(6),    !coefficienti curve ad heat rate costante
     *       chrt(4)    !coefficienti curva di correzione heat rate
c-------------------------------------------------------------------------------
  ! Body of DLL_PRINT_T

      call print_t(unit,o_name_file,turb,comment_t,nom_rev_t,nom_power,
     *            nom_hrate,nom_tair,ncorr,temp_cor,pw_cor,hrate_cor,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *            cpw,cpwt,chr,chrt)

      return

	end !DLL_PRINT_T

C***********************************************************************************