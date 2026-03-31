*** 77PNEWTON.FOR *** metodo delle tangenti ********************************
!-----------------------------------------------------------------------
cgpe	subroutine newton_nc_rk( ! metodo delle tangenti per la soluz di un sistema non lineare
cgpe	1                 n,     ! :in ordine del sistema
cgpe	1                 ia,    ! :in vettore indici riga della matrice del sistema
cgpe	1                 ja,    ! :in vettore indici colonna ""  ""
cgpe	1                  a,    ! :in vettore elementi       ""  ""
cgpe	1                kr0,    ! :in vettore ordinamento righe "" ""
cgpe	1                kc0,    ! :in vetore ordinamento colonne "" ""
cgpe	1                ic0,    ! :in vettore inverso di kc0
cgpe	1                  x,    ! :out vettore soluzioni
cgpe	1                info)   ! :out codice di errore

	subroutine newton_nc_rk(! metodo delle tangenti per la soluz di un sist non lineare
	1                n,      ! :in ordine del sistema
	1                ia,     ! :in vettore indici riga della matrice del sistema
	1                ja,     ! :in vettore indici colonna ""  ""
	1                a,      ! :in vettore elementi       ""  ""
	1                x,      ! :out vettore soluzioni
	1                info)   ! :out codice di errore
*&*
      implicit none
	include '../inc/param.inc'   	!:costanti
	include '../inc/dimension.inc'	!:costanti
	include '../inc/staznw_par.inc'	        !:in
	include '../inc/tv.inc'	        !:in
	include '../inc/nw.inc'	        !:in
*&*
!-----------------------------------------------------------------------
      integer*4 n,ia,ja,INFO
      real*4    x,a
	dimension x(*),
	1          ia(*),
	1          ja(*),
	1          a(*)
      integer*4 IDMAX
	parameter (IDMAX = 10*m_vard+2*m_jac)

      real*4    dx_dummy,deltax,rfvec,wk
      integer*4 iwk,k_rango
	dimension deltax(m_vard),
	1	      rfvec(m_vard),
	1          dx_dummy(m_vard),
	1          wk(IDMAX),
	1          iwk(IDMAX)

      integer*4 NFEV,NIER6,NIER7,NIER8
      real*4    sqREPS,RFNORM,DIFIT,RFNORM1,DIFIT1,XNORM,MOPT
      integer*4 I,IERR,M
      real*4    DELTA,RFAC
      logical*2 LCIT,LCONV

      INTEGER*4 i_fnorm_max
      integer*4 el_rec
      INTEGER*4 itop_from_ivar  !function
      external  itop_from_ivar
!-------------------------inizializzazioni---------------------------
	sqreps = sqrt(repsmch)
	i_fnorm_max = 0
	info = 0
	nfev = 0
	nier6 = -1
	nier7 = -1
	nier8 = 0
	rfnorm = zero
	difit = zero
	rfnorm1 = zero
	difit1 = zero
	xnorm = zero
!	call cmopt(n,mopt)
	mopt = 1  !?
	
	do i=1,n
	   xnorm = max(xnorm,abs(x(i)))
!  per il recupero del caso di rango < n
!  con 0 conferma la soluzione precedente
	   dx_dummy(i) = 0.0
	enddo

	delta = rscale*xnorm
	if(xnorm.eq.zero) delta = rscale
	
cgpe	call fcnev(n,ia,ja,x,rfvec)
	call fcnev(x,rfvec)

	do i =1,n
	   rfnorm = max(rfnorm,abs(rfvec(i)))
	enddo


!------------------ iterazione principale -------------------------

10	continue

cgpe	call jacob(n,ia,ja,x,a)
      call jacob (ia,ja,x,a)

      do i =1,n
        deltax(i) = rfvec(i)
	enddo
cgpe-prv	call decspa (n,ia,ja,a,kr0,kc0,ic0,ierr)
      call gauss(n,dx_dummy,ia,ja,a,deltax,wk,iwk,
     *           IDMAX,m_jac,K_rango,el_rec,ierr)

cgpe	if(ierr.le.0) then
cace
	if(ierr.ne.0) then
cgpe-end
	   info = 5
	   go to 100
	endif

      if(el_rec.gt.0) then
	   info = 9
	   go to 100
	endif
cgpe-prv	call solspa(n,rfvec,deltax)

	difit1 = difit
	difit = zero
	
	do i=1,n
	   difit = max(difit,abs(deltax(i)))
	enddo

	if(difit.gt.delta) then
	   rfac = delta/difit
	   do i=1,n
	      deltax(i) = deltax(i)*rfac
	   enddo
	   difit = delta
	endif


	xnorm = zero
	do i = 1,n
	   x(i) = x(i) - deltax(i)
	   xnorm = max(xnorm,abs(x(i)))
	enddo
	delta = max(delta,rscale*xnorm)

!----------------------------------------------------------------------
!	Controllo di convergenza 
!----------------------------------------------------------------------

	lcit = .true.
	do i = 1, n
cgpe		lcit = lcit .and. (abs(deltax(i)) .lt. tolxwd*abs(x(i)))
		lcit = lcit .and. (abs(deltax(i)) .le. tolxwd*abs(x(i)))
	enddo


	rfnorm1 = rfnorm
	rfnorm = zero
	
cgpe	call fcnev(n,ia,ja,x,rfvec)
	call fcnev(x,rfvec)

	nfev = nfev+1
	
	do i =1,n
c	   rfnorm = max(rfnorm,abs(rfvec(i)))
	   if (abs(rfvec(i)).gt.rfnorm) then
	     rfnorm = abs(rfvec(i))
	     i_fnorm_max = i
	   endif
	enddo


!------ determina il progredire delle iterazioni----------------------



	lconv=(rfnorm.le.rfnorm1 .and. difit.le.difit1 )
	nier6 = nier6 +1
	nier7 = nier7+1
	nier8 = nier8+1
	if(lconv) nier6 = 0
	if(rfnorm.le.rfnorm1 .or. difit.le.difit1) nier7 = 0
	if(difit.gt.sqreps*xnorm) nier8 = 0
	
	if(rfnorm.le.tolfwd) info = 1
	if(lcit .and. lconv) info = 2
	if(rfnorm.le.tolfwd .and. info.eq.2) info = 3
	if(info.ne.0) go to 100
	
	if(nfev.ge.mitnwd) info = 4
	if(nier6.eq.10) info = 6
	if(nier7.eq.3) info = 7
	if(nier8.eq.4) info = 8
	if(info.ne.0) go to 100

!----------------- raffinamento iterativo -----------------------------

	if(lconv .and. difit.le.p05*xnorm .and. mopt.gt.1) then
	   do m = 2,mopt

cgpe-prv		call solspa(n,rfvec,deltax)
          do i =1,n
            deltax(i) = rfvec(i)
	    enddo
cgpe-prv	call decspa (n,ia,ja,a,kr0,kc0,ic0,ierr)
          call gauss(n,dx_dummy,ia,ja,a,deltax,wk,iwk,
     *               IDMAX,m_jac,K_rango,el_rec,ierr)

cgpe	         if(ierr.le.0) then

	    if(ierr.ne.0)then
cgpe-end
	      info = 5
	      go to 100
	    endif

          if(el_rec.gt.0) then
      	  info = 9
	      go to 100
	    endif

		xnorm = zero
		difit = zero
		do i = 1,n
		   difit = max(difit,abs(deltax(i)))
		   x(i) = x(i) - deltax(i)
		   xnorm = max(xnorm,abs(x(i)))
		enddo
!----------------------------------------------------------------------
!	Controllo di convergenza 
!----------------------------------------------------------------------

	lcit = .true.
	do i = 1, n
		lcit = lcit .and. (abs(deltax(i)) .lt. tolxwd*abs(x(i)))
	enddo

		rfnorm1 = rfnorm
		rfnorm = zero
cgpe		call fcnev(n,ia,ja,x,rfvec)
		call fcnev(x,rfvec)
		do i = 1,n
		   rfnorm = max(rfnorm,abs(rfvec(i)))
		enddo


		if(rfnorm .ge.rfnorm1)then
		   rfnorm = rfnorm1
		   go to 80
		endif


		if(rfnorm.le.tolfwd) info = 1
		if(lcit) info = 2
		if(rfnorm.le.tolfwd .and. info.eq.2) info = 3
		if(nfev.ge.mitnwd .and. info.eq.0) info = 4
		if(info.ne.0) go to 100
	   enddo
	endif
80	continue

!    Vengono aggiornate le variabili in tj

cgpe	Call aggvars_rk(n,x)
	call aggvars_ssdd(x)
cgpec gz --- new routine aggjac_nc
cgpe	Call aggjac_nc(ia,ja)

	go to 10

100	continue

	if     (info .eq. 4)then
cgpe	   call err('new_01')
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'Superato no max di iter nel Newton',0)
       if (i_fnorm_max.gt.0) then
	   i_elem_ko = itop_from_ivar(i_fnorm_max,nw_topvar)
	   if (i_elem_ko.gt.m_ogg+max_ce) then
	      i_elem_ko = i_elem_ko + oics
         elseif (i_elem_ko.gt.m_ogg) then
	      i_elem_ko = i_elem_ko + oice
         else
	      continue
         end if
	 endif
	elseif (info .eq. 5)then
cgpe	   call err('new_02')
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'Errore nella sol del sist jacob',0)
	elseif (info .eq. 6)then
cgpe	   call err('new_03')
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'Troppe iter non conv in Newton',0)
       if (i_fnorm_max.gt.0) then
	   i_elem_ko = itop_from_ivar(i_fnorm_max,nw_topvar)
	   if (i_elem_ko.gt.m_ogg+max_ce) then
	      i_elem_ko = i_elem_ko + oics
         elseif (i_elem_ko.gt.m_ogg) then
	      i_elem_ko = i_elem_ko + oice
         else
	      continue
         end if
	 endif
	elseif (info .eq. 7)then
cgpe	   call err('new_04')
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'Le iter di Newton divergono',0)
       if (i_fnorm_max.gt.0) then
	   i_elem_ko = itop_from_ivar(i_fnorm_max,nw_topvar)
	   if (i_elem_ko.gt.m_ogg+max_ce) then
	      i_elem_ko = i_elem_ko + oics
         elseif (i_elem_ko.gt.m_ogg) then
	      i_elem_ko = i_elem_ko + oice
         else
	      continue
         end if
	 endif
	elseif (info .eq. 8)then
cgpe	   call err('new_05')
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'La toll richiesta e'' eccessiva',0)
       if (i_fnorm_max.gt.0) then
	   i_elem_ko = itop_from_ivar(i_fnorm_max,nw_topvar)
	   if (i_elem_ko.gt.m_ogg+max_ce) then
	      i_elem_ko = i_elem_ko + oics
         elseif (i_elem_ko.gt.m_ogg) then
	      i_elem_ko = i_elem_ko + oice
         else
	      continue
         end if
	 endif
	elseif (info .eq. 9)then
D        call gest_error(1,0,'Newton_nc_rk',
D     *       'Elementi linearmente dipendenti',0)
	  i_elem_ko = itop_from_ivar(el_rec,nw_topvar)
	   if (i_elem_ko.gt.m_ogg+max_ce) then
	      i_elem_ko = i_elem_ko + oics
         elseif (i_elem_ko.gt.m_ogg) then
	      i_elem_ko = i_elem_ko + oice
         else
	      continue
         end if
D        call gest_error(1,0,'Newton_nc_rk',
D     *       snomtv(i_elem_ko),i_elem_ko)
	else
cgpe	   call aggvars_rk(n,x)
	   call aggvars_ssdd(x)
	endif

	return
	end

      subroutine aggvars_ssdd (x)

cgpe   Nota coincide con la subroutine ssdd$src:aggvars
!     Aggiorna i valori di pressione e portata in tj e calcola densita',
!     lambda e Z medio.

*&* 
      implicit none
      include '../inc/param.inc'    !:costanti
      include '../inc/default.inc'    !:costanti
      include '../inc/nw.inc'      !:costanti
      include '../inc/ti.inc'	      !:IN
      include '../inc/tj.inc'	      !:INOUT
      include '../inc/ty.inc'	      !:out
      include '../inc/jc.inc'	      !:out
c-elena
      include '../inc/staznw_par.inc'	      !:out
*&*
      real*4    X(*)         !  :in  variabili calcolate con Newton
*&*
      integer*4 I,J,IM,IV
      real*4    XP

!      dimension x(n)
c-elena      parameter pzero = 0.01
      integer*4 oicen
!----------------------------------------------------------------------
!  PRESSIONE e Z dei PUNTI
!----------------------------------------------------------------------
      do i = otipti(e_pu), etipti(e_pu)
        prestj(i) = sqrt(max(x(nw_topvar(i)),0.0))
        prestj(i) = max(prestj(i),pmin_sys)
        call agg_punto(i)
      enddo

!----------------------------------------------------------------------
!  Portata ,lambda  in ogni TRONCO
!     pressione,  Z medio e coeff. di Fergusson
!----------------------------------------------------------------------
      do i = otipti(e_tr), etipti(e_tr)
        im = opumto(i)
        iv = opuvto(i)
        porttj(i) = x(nw_topvar(i))
        call agg_tronco(i)
      enddo

!--------------------------------------------------- -------------------
!  PORTATA degli ELEMENTI SPECIALI
!----------------------------------------------------------------------
      do j = 1, ne_spe
        do i = otipti(te_spe(j)), etipti(te_spe(j))
          porttj(i) = x(nw_topvar(i))
        end do
      end do

!----------------------------------------------------------------------
!  Consumi delle centrali
!----------------------------------------------------------------------
      do j=1,ne_cen
         if (te_cen(j).eq.e_ce) then
            oicen=oice
         else
            oicen=oics
         end if
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
c      oicen=oice
c      do i = otipti(e_ce), etipti(e_ce)
cgpe-corr           im = opumto(i)
           consty(i) = x(nw_topvar(i-oicen))
c           valsjc(i-oicen) = consty(i)     ! aggiornamento valsjc in modo che
	                                 ! quando si passa da consumo calcolato (lcent_firt [T])
									 ! a consumo imposto (lcent_firt [F]) 
									 ! sia memorizzato l'ultimo valore calcolato.
									 ! N.B. Nel caso lcent_firt [F] valsjc e consty 
									 !      dovrebbero essere uguali
          enddo
      enddo
      return
      end   ! aggvars

!----------------------------------------------------------------------
*** 12PNEWTON.FOR *** metodo delle tangenti ********************************
