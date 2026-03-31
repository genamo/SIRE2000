c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Subroutine generiche per il calcolo del punto di lavoro sulle
C     curve dei compressori
cgpe  NOTA: queste subroutine ricevono in input parametri dimensionali
cgpe        e adimensionalizza le variabili prima di chiamare le
cgpe        analoghe routine che utilizzano variabili adimensionali
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


!======================================================================
      subroutine HDAQN(h,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (Q,n) assegnati.
c
      real*4     h      ! O) head calcolato
      real*4     q      ! I) portata 
      real*4     rev    ! I) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     qad,rad,had  ! variabili interne adimensionali
c
      qad = q   / qmrif
      rad = rev / revrif
      call HDAQN_A(had,qad,rad,chn)
      h = headrif * had
c
      return
      end

!======================================================================
      subroutine QDAHN(h,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (H,n) assegnati.
c
      real*4     h      ! I) head 
      real*4     q      ! O) portata 
      real*4     rev    ! I) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     had,rad,qad  ! variabili interne adimensionali
      real*4     qmin/1.e-3/
c
      had = h   / headrif
      rad = rev / revrif
      call QDAHN_A(had,qad,rad,chn)
      q = qmrif * qad
      if (q.lt.qmin) q=qmin
c
      return
      end
!======================================================================
      subroutine NDAHQ(h,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di n in funzione di (H,Q) assegnati.
c
      real*4     h      ! I) head 
      real*4     q      ! I) portata 
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     had,qad,rad  ! variabili interne adimensionali
c
      had = h   / headrif
      qad = q   / qmrif
      call NDAHQ_A(had,qad,rad,chn)
      rev = revrif * rad
c
      return
      end

!======================================================================
      subroutine QDAH_CH(k,h,q)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (H) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! I) costante della curva limite
      real*4     q      ! O) portata 
c
      real*4     had,qad  ! variabili interne adimensionali
c
      had = h   / headrif
      call QDAH_CH_A(k,had,qad)
      q = qmrif * qad
c
      return
      end
!======================================================================
      subroutine HDAQ_CH(k,h,q)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (Q) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! O) costante della curva limite
      real*4     q      ! I) portata 
c
      real*4     qad,had
c
      qad = q / qmrif
      call HDAQ_CH_A(k,had,qad)
      h = headrif * had
c
      return
      end
!======================================================================
      subroutine NDAH_CH(k,h,q,rev,chn)
cgpe corrisponde alla NQDAH_CH_A, per analogia dovrebbe essere rinominata
cgpe subroutine NQDAH_CH(k,h,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q,N in funzione di (H) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! I) costante della curva limite
      real*4     q      ! O) portata 
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     qad,had,rad  ! variabili interne adimensionali
c
      had = h / headrif
      call NQDAH_CH_A(k,had,qad,rad,chn)
      rev = revrif * rad
      q = qmrif * qad
c
      return
      end
!======================================================================
      subroutine NDAQ_CH(k,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di N in funzione di (Q) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     q      ! I) portata 
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     qad,rad  ! variabili interne adimensionali
c
      qad = q / qmrif
      call NDAQ_CH_A(k,qad,rad,chn)
      rev = revrif * rad
c
      return
      end
!======================================================================
      subroutine QDAN_CH(k,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (N) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     q      ! O) portata 
      real*4     rev    ! I) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     rad,qad  ! variabili interne adimensionali
c
      rad = rev / revrif
	call QDAN_CH_A(k,qad,rad,chn)
      q = qmrif * qad
c
      return
      end
!======================================================================
      subroutine HDAN_CH(k,h,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (N) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! O) head
      real*4     rev    ! I) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
      real*4     rad,had  ! variabili interne adimensionali
c
      rad = rev / revrif
      call HDAN_CH_A(k,had,rad,chn)
      h = headrif * had
c
      return
      end
!======================================================================
      subroutine QDAH_SU(alim,blim,nlim,lim_h,h,q,irange)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (H) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta Q = (h-alim)/blim
c
      real*4     alim(*)   ! I) vettore dei termini noti a
      real*4     blim(*)   ! I) vettore dei coefficienti angolari b
      INTEGER*4    nlim    ! I) numero di rette
      real*4     lim_h(*)  ! I) vettore dei valori estremi di h
      real*4     h         ! I) costante della curva limite
      real*4     q         ! O) portata 
      INTEGER*4    irange  ! O) indice dell'intervallo trovato trovato
c
      real*4     had,qad   ! variabili interne adimensionali
      real*4     qmin/1.e-3/
c
      had = h / headrif
      CALL QDAH_SU_A(alim,blim,nlim,lim_h,had,qad,irange)
      q = qmrif * qad
      if (q.lt.qmin) q=qmin
c
      return
      end
!======================================================================
      subroutine NDAQ_SU(alim,blim,nlim,lim_q,q,chn,n,irange)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di N in funzione di (Q) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = qblim + alim
c
      real*4     alim(*)   ! I) vettore dei termini noti a
      real*4     blim(*)   ! I) vettore dei coefficienti angolari b
      INTEGER*4  nlim      ! I) numero di rette
      real*4     lim_q(*)  ! I) vettore dei valori estremi di q
      real*4     q         ! I) portata 
      real*4     chn(*)    ! I) vettore dei coefficienti
      real*4     n         ! O) giri calcolati
      INTEGER*4  irange    ! O) indice dell'intervallo trovato trovato
c
      real*4     qad,rad   ! variabili interne adimensionali
c
      qad = q / qmrif
      call NDAQ_SU_A(alim,blim,nlim,lim_q,qad,chn,rad,irange)
	n = revrif * rad
c
      return
      end
!======================================================================
      subroutine HDAQ_SU(alim,blim,nlim,lim_q,q,chn,h,irange)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (Q) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = qblim + alim
c
      real*4     alim(*)   ! I) vettore dei termini noti a
      real*4     blim(*)   ! I) vettore dei coefficienti angolari b
      INTEGER*4  nlim      ! I) numero di rette
      real*4     lim_q(*)  ! I) vettore dei valori estremi di q
      real*4     q         ! I) portata 
      real*4     chn(*)    ! I) vettore dei coefficienti
      real*4     h         ! O) prevalenza calcolata
      INTEGER*4  irange    ! O) indice dell'intervallo trovato trovato
c
      real*4     qad,had   ! variabili interne adimensionali
c
      qad = q / qmrif
      call HDAQ_SU_A(nlim,alim,blim,qad,lim_q,had,irange)
      h = headrif * had
c
      return
      end
!======================================================================
      subroutine HDAN_SU(chn,alim,blim,nlim,lim_rev,rev,h,q,irange)
cgpe corrisponde alla HQDAN_SU_A, per analogia dovrebbe essere rnominata
cgpe subroutine HQDAN_SU(chn,alim,blim,nlim,lim_rev,rev,h,q,irange)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H e Q in funzione di (rev) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = alim + Q * blim
c
      real*4     chn(*)     ! I) coefficienti curva caratteristica
      real*4     alim(*)    ! I) vettore dei termini noti a
      real*4     blim(*)    ! I) vettore dei coefficienti angolari b
      INTEGER*4    nlim       ! I) numero di rette
      real*4     lim_rev(*) ! I) vettore dei valori estremi di rev
      real*4     rev        ! I) rev num
      real*4     h          ! O) head
      real*4     q          ! O) portata 
      INTEGER*4    irange     ! O) indice dell'intervallo trovato trovato
c
      real*4     rad,qad,had  ! variabili interne adimensionali
c
      rad = rev / revrif
      call HQDAN_SU_A(chn,alim,blim,nlim,lim_rev,rad,had,qad,irange)
      h = headrif * had
      q = qad * qmrif
c
      return
      end
!======================================================================
      subroutine QDAN_SU(chn,alim,blim,nlim,lim_rev,rev,Q,h,irange)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (rev) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = alim + Q * blim
c
      real*4     chn(*)     ! I) coefficienti curva caratteristica
      real*4     alim(*)    ! I) vettore dei termini noti a
      real*4     blim(*)    ! I) vettore dei coefficienti angolari b
      INTEGER*4  nlim       ! I) numero di rette
      real*4     lim_rev(*) ! I) vettore dei valori estremi di rev
      real*4     rev        ! I) rev num
      real*4     q          ! O) portata 
      real*4     h          ! O) head
      INTEGER*4  irange     ! O) indice dell'intervallo trovato trovato
      real*4     qmin/1.e-3/
c
      call HDAN_SU(chn,alim,blim,nlim,lim_rev,rev,h,q,irange)
      if (q.lt.qmin) q=qmin
c
      return
      end
!======================================================================
      subroutine NDAH_SU(chn,alim,blim,nlim,lim_h,rev,h)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di N in funzione di (H) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = alim + Q * blim
c
      real*4     chn(*)     ! I) coefficienti curva caratteristica
      real*4     alim(*)    ! I) vettore dei termini noti a
      real*4     blim(*)    ! I) vettore dei coefficienti angolari b
      INTEGER*4    nlim     ! I) numero di rette
      real*4     lim_h(*)   ! I) vettore dei valori estremi di head
      real*4     rev        ! O) rev num
      real*4     h          ! I) head
c
      INTEGER*4  irange     ! O) indice dell'intervallo trovato trovato
cgpe      real*4     q          ! O) portata 
c
      real*4     had,rad,qad ! variabili interne adimensionali
c
      had = h / headrif
cgpe	call NDAH_SU_A(chn,alim,blim,nlim,lim_h,rad,had)
	call NDAH_SU_A(chn,alim,blim,nlim,lim_h,rad,had,qad)
      rev = rad * revrif
cgpe      q = qmrif * qad
c
      return
      end
!======================================================================
      subroutine PWDAN(Pow,rev,temp,cpwm,cpwt)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di PWMAX in funzione di (rev,temp) assegnati.
c
      real*4     Pow     ! O) pwmax calcolato (in Kwatt)
      real*4     rev     ! I) revolution rate
      real*4     temp    ! I) temperatura dell'aria
      real*4     cpwm(*) ! coefficienti curva caratteristica
      real*4     cpwt(*) ! coefficienti curva correzione temp
c
      real*4     tadim,rad,powad
c
      tadim = temp/(trif+t0)
      rad   = rev / revrif
      call PWDAN_A(Powad,rad,tadim,cpwm,cpwt)
      pow = powerif * powad
c
      return
      end
!======================================================================
      subroutine HRDAPN(Hr,Pow,rev,temp,chr,chrt)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di PWMAX in funzione di (rev,temp) assegnati.
c
      real*4     Hr      ! O) hrate calcolato (in KJ/Kwatth)
      real*4     Pow     ! I) pwmax (in Kwatt)
      real*4     rev     ! I) revolution rate
      real*4     temp    ! I) temperatura dell'aria
      real*4     chr(*)  ! coefficienti curva caratteristica
      real*4     chrt(*) ! coefficienti curva correzione temp
c
      real*4     tadim,rad,pwad,Hrad
c
      tadim = temp/(trif+t0)
      rad   = rev / revrif
      pwad  = pow / powerif
      call HRDAPN_A(Hrad,Pwad,rad,tadim,chr,chrt)
      hr  = hraterif*hrad
c
      return
      end
!======================================================================
      subroutine EFFDAQN(eff,q,rev,cen,cec,clim)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di eff in funzione di (rev,temp) assegnati.
c
      real*4     eff     ! O) efficienza calcolata (valore da 0 a 1)
      real*4     Q       ! I) portata
      real*4     rev     ! I) revolution rate
      real*4     cen(*)  ! coefficienti curva 
      real*4     cec(*)  ! coefficienti curva choking zone
      real*4     clim(*) ! coefficienti costanti curve limite
c
      real*4     qad,rad,qr
c
      qad   = q / qmrif
      rad   = rev / revrif
      call EFFDAQN_A(eff,qad,rad,cen,cec,clim)
c
      return
      end
!======================================================================
      subroutine qvert(chn,q,rev)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo del vertice Q della parabola rev=costante.
c
      real*4     chn(*) ! I) coefficienti curva caratteristica
      real*4     q      ! O) portata 
      real*4     rev    ! I) revolution rate
c
      real*4     qad,rad  ! variabili interne adimensionali
c
      rad = rev / revrif
c
      qad = -(chn(2)+chn(4)*rad)/(2*chn(5))
c
      q = qad * qmrif
c
      return
      end
!======================================================================
      subroutine QDAN_SU_MF(k,q,rev,chn)
c
c PROTOTIPO: Per ora si simula la curva di surge come su una parabola
c analoga alla choking
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (N) sulla curva di surge, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     q      ! I) portata
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
c-----------------------------------------------------------------------
      call QDAN_CH(k,q,rev,chn)
c
      return
      end
!======================================================================
      subroutine NDAQ_SU_MF(k,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di N in funzione di (Q) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     q      ! I) portata
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c-----------------------------------------------------------------------
      call NDAQ_CH(k,q,rev,chn)
c
      return
      end
!======================================================================
      subroutine HDAQ_SU_MF(k,h,q)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (Q) sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! O) costante della curva limite
      real*4     q      ! I) portata
c-----------------------------------------------------------------------
      call HDAQ_CH(k,h,q)
c
      return
      end
!======================================================================
      subroutine HDAN_SU_MF(k,h,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (N) sulla curva clim = K la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! O) costante della curva limite
      real*4     rev    ! I) giri
      real*4     chn(*) ! I) coefficienti
c-----------------------------------------------------------------------
      call HDAN_CH(k,h,rev,chn)
c
      return
      end
!======================================================================
      subroutine QDAH_SU_MF(k,h,q)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di H sulla curva clim = K la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! O) costante della curva limite
      real*4     q      ! I)
c-----------------------------------------------------------------------
      call QDAH_CH(k,h,q)
c
      return
      end
!======================================================================
      subroutine NDAH_SU_MF(k,h,q,rev,chn)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di N in funzione di H sulla curva clim = K la cui costante
c   e' k (H=K*Q**2)
c
      real*4     k      ! I) costante della curva limite
      real*4     h      ! I) costante della curva limite
      real*4     q      ! O) portata
      real*4     rev    ! O) revolution rate
      real*4     chn(*) ! coefficienti curva caratteristica
c
c-----------------------------------------------------------------------
      call NDAH_CH(k,h,q,rev,chn)
c
      return
      end