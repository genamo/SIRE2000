!  dll_curve_unita.for
!
!  FUNCTIONS/SUBROUTINES exported from DLL_CURVE_UNITA.dll:


!	DLL_COMPR_PT      - subroutine 
!
cmar	subroutine DLL_COMPR_PT(nom_rev,max_rev,min_rev,min_eff,chn,chc,
cmar     *              cen,flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
cmar     *              np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
cmar     *              qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
cmar     *              ef_q,eta,qmini,qmaxi,hmini,hmaxi)


	subroutine DLL_COMPR_PT(k_equi_min, k_equi_max,flag_equi,
     *                    nom_rev,max_rev,min_rev,min_eff,
     *                    chn,chc,cen,
     *                   flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
     *                   np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
     *                   qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
     *                   ef_q,eta,
     *                   qmini,qmaxi,hmini,hmaxi,
     *                     nse, qse, hse,nce, qce, hce)


! Expose subroutine DLL_COMPR_PT to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_COMPR_PT


! Variables
	implicit none

      integer*4 max_np,max_nlim,max_neff,max_ngiri
	parameter (max_np=50,max_nlim=20,max_neff=10,max_ngiri=12)

C------------------------>>>>>>>>>>>>INPUT<<<<<<<<<<<<<<<<<<--------------------------------
      real*4 max_rev  !numero di giri massimo   
     *    ,min_rev  !numero di giri minimo      
c
c---------------------------> VARIABILI PER COMPRESSORI    
c
      real*4 nom_rev  !valore nominale del numero di giri   
     *    ,min_eff  !efficienza minima                     
     *	,chn(6)   !coefficienti di h=f(n,q) nella zona normale     
     *	,chc(6)   !coefficienti di h=f(n,q) nella zona di choking  
     *	,cen(5)   !coefficienti di eff=f(n,q) nella zona normale   
c
      logical*2 flag_lim !curva limite SI/NO;se si disegno l'AS teorica  
      integer*4 nlim !numero di punti limite assegnati sulla spezzata AntiSourge 
c
      real*4 alim(max_nlim+1) !vettore dei termini noti della spezzata di surge  
     *    ,blim(max_nlim+1) !vettore dei coefficienti angolari della spezzata di surge
     *	,clim(4) !costanti delle curve limite del compressore          
     *    ,lim_n(max_nlim)!vettore dei valori estremi del numero di giri       
     *    ,lim_q(max_nlim)!vettore dei valori estremi della portata            
     *    ,lim_h(max_nlim)!vettore dei valori estremi della prevalenza         
c
      integer*4 np   !numero di punti da calcolare sulle singole curve   
C
C------------------->>>>>>>>>>>>OUTPUT<<<<<<<<<<<<<<<<<<-------------------------
C
     *       ,ip   !numero di punti sulla curva teorica di anti-surge    
c
      real*4 qs(max_nlim)  !ascisse dei pti sulla curva anti-surge
     *	,hs(max_nlim)  !ordinate dei pti sulla curva anti-surge
     *    ,qst(max_ngiri)   !ascisse dei pti sulla curva anti-surge teorica 
     *    ,hst(max_ngiri)   !ordinate dei pti sulla curva anti-surge teorica
     *    ,qc(max_np)  !ascisse dei pti sulla curva anti-choke
     *    ,hc(max_np)  !ordinate dei pti sulla curva anti-choke
     *    ,qol(max_np) !ascisse dei pti sulla curva operating limit
     *	,hol(max_np) !ordinate dei pti sulla curva operating limit
c
      integer*4 ngiri   !curve a numero di giri costante da disegnare
c
      real*4 giri(max_ngiri) !valori costanti del numero di giri da graficare 
c
      real*4 qrn(max_ngiri,max_np)!ascisse pti su curve a rev cost (zona normale) 
     *    ,hrn(max_ngiri,max_np)!ordinate pti su curve a rev cost (zona normale)
     *    ,qrc(max_ngiri,max_np)!ascisse pti su curve a rev cost (zona choking)
     *    ,hrc(max_ngiri,max_np)!ordinate pti su curve a rev cost (zona choking)
c
      integer*4 neff    !curve a efficienza costante da disegnare
c
      real*4 effic(max_neff) !valori dell'efficienza calcolati 
c
c      integer*4 ngiri_qe
c      real*4 giri_qe(12)
c
      real*4 qef1(max_neff,max_np)!ascisse dei pti su curve a eff cost(1°set pt)
     *    ,hef1(max_neff,max_np)!ordinate dei pti su curve a eff cost
     *    ,qef2(max_neff,max_np)!ascisse dei pti su curve a eff cost(2°set pt)
     *    ,hef2(max_neff,max_np)!ordinate dei pti su curve a eff cost
     *    ,ef_q(max_ngiri,max_np)!ascisse per l'eff nel piano Q-eta
     *    ,eta(max_ngiri,max_np) !ordinate per l'eff nel piano Q-eta
     *    ,qmini                !limite minimo di q sull'asse delle ascisse
     *	,qmaxi                !limite massimo di q sull'asse delle ascisse
     *    ,hmini                !limite minimo di h sull'asse delle ordinate
     *    ,hmaxi                !limite massimo di h sull'asse delle ordinate

cmar 
      real*4 k_equi_min, k_equi_max

	real*4 qse(max_np), hse(max_np), qce(max_np), hce(max_np)
      integer*4 nse, nce
  
      logical*2 flag_equi
C------->variabili locali
      integer i
c
cmar------------++++++++++++++++++++++++------------------------------|
cmar    nuovo calcolo per le curve dell'Equidistanza sui compressori  |
cmar------------++++++++++++++++++++++++------------------------------|
      

        call compr_pt     (k_equi_min, k_equi_max,flag_equi,
     *                    nom_rev,max_rev,min_rev,min_eff,
     *                    chn,chc,cen,
     *                   flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
     *                   np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
     *                   qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
     *                   ef_q,eta,
     *                   qmini,qmaxi,hmini,hmaxi,
     *                     nse,qse, hse, nce,qce, hce)



cmar      call compr_pt(nom_rev,max_rev,min_rev,min_eff,chn,chc,
cmar     *              cen,flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
cmar     *              np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
cmar     *              qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
cmar     *              ef_q,eta,qmini,qmaxi,hmini,hmaxi)


	return
	end !subroutine DLL_COMPR_PT
C-------------------------------------------------------------------------------
!	DLL_TRANS_COMPR      - subroutine 
!
	subroutine DLL_TRANS_COMPR(npoint,flow,head,eff)

! Expose subroutine DLL_TRANS_COMPR to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_TRANS_COMPR

      implicit none
      integer*4 max_npoint   !numero max di punti inseribili
	parameter (max_npoint=50)

c------------------------->>>>>>INPUT<<<<<<<<<<<<<<-----------------------------
      integer*4 npoint     !numero di punti assegnati (max_npoint=50)
                           !coordinate relative a tali punti:
c------------------------->>>>>>INPUT/OUTPUT<<<<<<<<<<<<<<-----------------------------
	real*4 flow(max_npoint)       !I/O) portata    (dim=npoint)
     *      ,head(max_npoint)       !I/O) prevalenza (dim=npoint)
     *      ,eff(max_npoint)        !I/O) efficienza (dim=npoint)



  ! Body of DLL_TRANS_COMPR

	call TRANS_COMPR(npoint,flow,head,eff)

      return
	end
c---------------------------------------------------------------------------------

C
!	DLL_TURB_POINT      - subroutine 
!
	subroutine DLL_TURB_POINT(npt,nom_power,nom_hrate,cpw,cpwt,
     *                          chr,chrt,
     *                          tmin,tmax, 
     *                          rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
     *                          pwup,pwdw,temp,fac_hr,fac_pw,hr)
C

! Expose subroutine DLL_TURB_POINT to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_TURB_POINT

! Variables
C
      implicit none
      integer*4 max_np,max_nhr
	parameter (max_np=50,max_nhr=10)
C
c------------------->>>>>>>>>>INPUT<<<<<<<<<<<<<---------------------------------
      integer*4 npt    !numero di punti da calcolare
c
c
      real*4 nom_power !valore nominale della potenza
     *    ,nom_hrate !valore nominale dell'hrate

      real*4 cpw(3)    !coefficienti per il calcolo della potenza
     *    ,chr(6)    !coefficienti per il calcolo dell'heat-rate
     *    ,cpwt(4)   !coefficienti correttivi per la potenza 
     *    ,chrt(4)   !coefficienti correttivi per l'hrate 
c
      real*4 tmin,   !temperatura minima per valutazione fattori correttivi
     *       tmax    !temperatura max per valutazione fattori correttivi

C-------------------------->>>>>>>OUTPUT<<<<<<<<<<<<<<--------------------------
      real*4 rev_pw(max_np)   !ascisse dei punti calcolati per disegnare la curva di
	                   !massima potenza e la curva ad heat rate costante
     *    ,ppwmax(max_np)!ordinate dei punti usati per disegnare la curva di 
                         !massima potenza
      integer*4 nhr   !numero curve ad hr costante calcolate

      real*4 rev_hr_up(max_nhr,max_np)  
     *    ,rev_hr_dw(max_nhr,max_np) 
     *    ,pwdw(max_nhr,max_np) !ordinate dei punti di intersezione tra una  
     *    ,pwup(max_nhr,max_np) !curva a numero di giri costante e una ad heat 
                                  !rate costante
c
c      integer*4 mp(10)
c
      real*4 temp(max_np) !ascisse dei punti calcolati per disegnare l'andamento  
	                  !dei fattori correttivi (temperature calcolate)
     *    ,fac_hr(max_np)!ordinate dei punti calcolati per disegnare l'andamento  
                        !del fattore correttivo per l'heat rate
     *    ,fac_pw(max_np) !ordinate dei punti calcolati per disegnare l'andamento 
                         !del fattore correttivo per la potenza
     *    ,hr(7) !valori costanti dell'hr da graficare

  ! Body of DLL_TURB_POINT
C
      call turb_point(npt,nom_power,nom_hrate,cpw,cpwt,chr,chrt,
     *                tmin,tmax,
     *                rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
     *                pwup,pwdw,temp,fac_hr,fac_pw,hr)
c
	return
	end !subroutine DLL_TURB_POINT
c
c------------------------------------------------------------------------------------------
!	DLL_TRANS_TURB      - subroutine 
!
	subroutine DLL_TRANS_TURB(nom_power,cpwt,nmaxp,rev_pwmax,
     *                  pwmax,airpwm,nhrate,rev_hrate,power,airhrt,
     *                  pwmax_t,power_t)
C

! Expose subroutine DLL_TRANS_TURB to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_TRANS_TURB

! Variables
      implicit none
      integer*4 max_npoint   !numero max di punti inseribili
	parameter (max_npoint=50)
c------->Input
      real*4 nom_power    !potenza nominale
c
      integer*4 nmaxp           ! punti a disposizione per il calcolo dei CPW          
c                               ! grandezze relative ai punti osservati a disposizione:
      real*4  pwmax(max_npoint)          ! potenza massima;
     *	   ,airpwm(max_npoint)         ! temperatura.
c
      integer*4 nhrate         ! punti a disposizione per il calcolo dei CHR                               
	                         ! grandezze relative ai punti osservati a disposizione:
	real*4 power(max_npoint)          ! potenza;
     *      ,airhrt(max_npoint)         ! temperatura;
c
      real*4 cpwt(4)           ! coeff per la correzione della potenza funzione 
                               ! della temperatura dell'aria
c------->Output                  potenza alla temperatura di graficazione
      real*4 pwmax_t(max_npoint),       ! punti assegnati per i cpw 
     *       power_t(max_npoint)        ! punti assegnati per i chr
c
c------->Input/Output
c
      real*4  rev_pwmax(max_npoint),  ! numero di giri punti assegnati per i cpw
     *        rev_hrate(max_npoint)   ! numero di giri punti assegnati per i chr
      integer*4 i
c
! Body of DLL_TRANS_TURB
c
      call trans_turb(nom_power,cpwt,nmaxp,pwmax,airpwm,nhrate,
     *                       power,airhrt,pwmax_t,power_t)

      do i=1,nhrate
        rev_hrate(i)=rev_hrate(i)*100.
	end do
      do i=1,nmaxp
        rev_pwmax(i)=rev_pwmax(i)*100.
	end do
	return
	end !subroutine DLL_TRANS_TURB

c-------------------------------------------------------------------------------------------
	subroutine DLL_TRANS_FCORR(ncorr,temp,fpw,fhr)

! Expose subroutine DLL_TRANS_FCORR to users of this DLL
!
!DEC$ ATTRIBUTES DLLEXPORT::DLL_TRANS_FCORR

      implicit none
      integer*4 max_npoint   !numero max di punti inseribili
	parameter (max_npoint=50)
c------------------------->>>>>>INPUT<<<<<<<<<<<<<<-----------------------------
	integer*4 ncorr     !numero di punti assegnati per la valutazione dei
	                    !fattori correttivi (max_ncorr=50)
                          !grandezze caratteristiche di tali punti:
c------------------------->>>>>>INPUT/OUTPUT<<<<<<<<<<<<<<-----------------------------
	real*4 temp(max_npoint)      !I/O) temperatura    (dim=ncorr)
     *      ,fpw(max_npoint)       !I/O) correzione potenza (dim=ncorr)
     *      ,fhr(max_npoint)       !I/O) correzione heat rate (dim=ncorr)


  ! Body of DLL_TRANS_FCORR

      call trans_fcorr(ncorr,temp,fpw,fhr)

      return
	end
c---------------------------------------------------------------------------------------