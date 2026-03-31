*** 12PNEWTON.FOR *** metodo delle tangenti ********************************
!-----------------------------------------------------------------------
cgpe	subroutine fcnev(       !  Calcola il valore dei residui delle equa-
cgpe	                        !  zioni del sistema in un dato punto  x
cgpe	1                n,     ! :in ordine del sistema
cgpe	1                ia,    ! :in vettore indici riga della matrice del sistema
cgpe	1                ja,    ! :in vettore indici colonna  "" ""
cgpe	1                 x,    ! :in vettore variabili del sistema
cgpe	1                 rfvec)! :out vettore contenente i valori dei residui delle equazioni

      subroutine fcnev (
     *                  x,    ! :in vettore variabili del sistema
     *                  rfvec)! :out vettore contenente i valori dei residui delle equazioni

cgpe  Nota: coincide con la subroutine pse_model (x,rfvec)

!     Calcola il valore dei residui delle equazioni del sistema
!     in un dato punto  x.
!     Questa routine calcola i coefficienti riordinati coerentemente
!     con la topologia.

*&*
      implicit none
      include '../inc/param.inc'    !:costanti
      include '../inc/default.inc'  !:costanti
      include '../inc/nw.inc'       !:costanti
      include '../inc/TH.inc'       !:costanti
      include '../inc/jc.inc'       !:in
      include '../inc/ti.inc'       !:INOUT
      include '../inc/tj.inc'       !:INOUT
      include '../inc/td.inc'       !:INOUT
      include '../inc/ty.inc'       !:INOUT
      include '../inc/tx.inc'       !:INOUT
      include '../inc/cc.inc'       !:INOUT
      include '../inc/rk.inc'       !:INOUT

      include '../inc/stazione.inc'  !:IN
      include '../inc/stations.inc'  !:IN
      include '../inc/cap_bon_prv.inc'      !:costanti
*&*
      real*4    x(*)          ! :in vettore variabili del sistema
     *         ,rfvec(*)      ! :out vettore contenente i valori dei residui
*&*
      integer*4 i,ieq,im,iv,i_t,k,iele,j
      real*4    prop

      real*4 XQ,XM,XV,XEEA,XC,XP
      real*4 coefxq,coefxm,coefxv,coefxqs,coefxs,coefxp,coefxc,COEFQ
crsaconce
c      real*4 dq,dpa2,dpv2 !qconce
crsaconce

      logical*2 elem_bin,elem_uni   ! Function
	external  elem_bin,elem_uni
	integer*4 oicen
c-elena
      logical*2 elem_cen
	external  elem_cen
cgpe Nota: è stato tolto l'uso del vettore valsjc e sono stati usati 
cgpe       i valori di set (qsettj,pmsetj,pvsetj) e porttj(e_pu) per
cgpe       gli off_take
!-----------------------------------------------------------------------
      ieq = 1
!      oice = otipti(e_ce) - (M_OGG + 1)


!  Ciclo sui tronchi di condotta (equazioni di Fergusson)

      do i = otipti(e_tr), etipti(e_tr)
        rfvec(ieq) = 0.0

        im = opumto(i)
        iv = opuvto(i)

        xq = x(nw_topvar(i))
        xm = x(nw_topvar(im))
        xv = x(nw_topvar(iv))

        coefxq = bferty(i)
        coefxm = -aferty(i)
        coefxv = 1

        rfvec(ieq) = coefxq*xq*abs(xq) + coefxm*xm + coefxv*xv
        ieq = ieq + 1
      enddo


!	Ciclo sugli elementi speciali

      do k = 1, ne_spe
        do i = otipti(te_spe(k)), etipti(te_spe(k))
          rfvec(ieq) = 0.0

          if (hstatj(i) .eq. sta_on .or.
     *        hstatj(i). eq. sta_fr     ) then
!  P di monte = P di valle
            im = opumto(i)
            iv = opuvto(i)

            xm = x(nw_topvar(im))
            xv = x(nw_topvar(iv))

            coefxm = 1
            coefxv = -1

            rfvec(ieq) = coefxm*xm + coefxv*xv
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_off .or.
     *            hstatj(i) .eq. sta_nr      ) then
!  Se la regolazione e' in off: Q = 0

            xq = x(nw_topvar(i))

            coefxq = 1

            rfvec(ieq) = coefxq*xq
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_q .or.
     *            hstatj(i) .eq. sta_qm .or.
     *            hstatj(i) .eq. sta_qc     ) then

!  Se la regolazione e' in portata: Q = Qs

            xq = x(nw_topvar(i))

            coefxq = 1
            coefxqs = -valsjc(i)

            rfvec(ieq) = coefxq*xq + coefxqs
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_pm) then
!  Se la regolazione e' in pressione a monte: Pm = Ps

            im = opumto(i)
            xp = x(nw_topvar(im))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_pv .or.
     *            hstatj(i) .eq. sta_pw .or.
     *            hstatj(i) .eq. sta_rom    ) then
!  se la regolazione e' in pressione a valle o in Pw max o ro max: Pv = Ps

            iv = opuvto(i)
            xp = x(nw_topvar(iv))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1
!  questa aggiunta rispecchia l'aggiunta nella subroutine creajac_ssdd
!  (la gestione di elementi speciali per i quali si scrivono più equazioni)

	    elseif(hstatj(i) .eq. sta_p ) then
!	se la regolazione e' in pressione (valle e monte)

!  Pv = Ps
            iv = opuvto(i)
            xp = x(nw_topvar(iv))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

!  Pm = Ps
            im = opumto(i)
            xp = x(nw_topvar(im))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pvq ) then
!	se la regolazione e' in pressione a valle e in portata

!  Pv = Ps
            iv = opuvto(i)
            xp = x(nw_topvar(iv))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

!  Q = Qs
            xq = x(nw_topvar(i))

            coefxq = 1
            coefxqs = -valsjc(i)

            rfvec(ieq) = coefxq*xq + coefxqs
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pmq ) then
!	se la regolazione e' in pressione a monte e in portata

!  Pm = Ps
            im = opumto(i)
            xp = x(nw_topvar(im))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

!  Q = Qs
            xq = x(nw_topvar(i))

            coefxq = 1
            coefxqs = -valsjc(i)

            rfvec(ieq) = coefxq*xq + coefxqs
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pq ) then
!	se la regolazione e' in pressione (valle e monte) e in portata

!  Pv = Ps
            iv = opuvto(i)
            xp = x(nw_topvar(iv))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

!  Pm = Ps
            im = opumto(i)
            xp = x(nw_topvar(im))

            coefxp = 1
            coefxs = -valsjc(i)**2

            rfvec(ieq) = coefxp*xp + coefxs
            ieq = ieq + 1

!  Q = Qs
            xq = x(nw_topvar(i))

            coefxq = 1
            coefxqs = -valsjc(i)

            rfvec(ieq) = coefxq*xq + coefxqs
            ieq = ieq + 1
          end if
        end do
      end do


!	Equazioni di bilancio ai nodi

      do i = otipti(e_pu), etipti(e_pu)
        rfvec(ieq) = 0.0

        iele = oseeto(i)
        do while (iele.ne.i)
          xq = x(nw_topvar(iele))
          coefxc = 0
          coefxq = 1           ! entrante

          rfvec(ieq) = rfvec(ieq) + coefxq*xq + coefxc*xc

          iele = oseeto(iele)
        end do

        iele = oseuto(i)

!  Ciclo sugli elementi uscenti
        do while (iele.ne.i)
          xq = x(nw_topvar(iele))
          coefxc = 0
          coefxq = -1          ! uscente
c-elena ---->tengo conto delle centrali semplificate
          if (elem_cen(iele)) then    ! centrali bin/sempl o una. in stoc
             if (htipto(iele).eq.e_ce) then
                oicen=oice
             else
                oicen=oics
             end if              
c          if (htipto(iele).eq.e_ce) then
            xc = x(nw_topvar(iele-oicen))
            coefxc = -1
          end if

          rfvec(ieq) = rfvec(ieq) + coefxq*xq + coefxc*xc
          iele = oseuto(iele)
        end do

        coefq = -valsjc(i)         ! OFF-TAKE
        rfvec(ieq) = rfvec(ieq) + coefq
        ieq = ieq + 1
      enddo


!	Equazioni dei consumi per le centrali
c-elena --->considero anche le centrali semplificate
      do j=1,ne_cen
         if (te_cen(j).eq.e_ce) then
            oicen=oice
         else
            oicen=oics
         end if
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
c      do i = otipti(e_ce), etipti(e_ce)
          rfvec(ieq) = 0.0

crsaconce
c          if (lcent_first) then
c            im = opumto(i)
c            iv = opuvto(i)
c            if (elem_bin(i)) then
c              xv = x(nw_topvar(iv))
c              xm = x(nw_topvar(im))
c            else if (htipto(im) .eq. e_pu) then
c              xm = x(nw_topvar(im))
c            else
c              xv = x(nw_topvar(iv))
c            endif
c            xq = x(nw_topvar(i))
c
c            xc = x(nw_topvar(i-oicen))
c
c            xm = sqrt(max(xm,pmin_def))
c            xv = sqrt(max(xv,pmin_def))
c            call qconce(xq,xm,xv,i,coefxc,dq,dpa2,dpv2)
c
c
c            rfvec(ieq) = xc - coefxc
c
c	    else
crsaconce
!   consumo = consumo imposto

            xc = x(nw_topvar(i-oicen))
            coefxc = valsjc(i-oicen)
            rfvec(ieq) = xc - coefxc

crsaconce
c	    endif

crsaconce
          ieq = ieq + 1

      enddo
	end do
c
      return
      end ! pse_model

!-----------------------------------------------------------------------
*&*
	subroutine jacob(
	1                ia,   ! :in vettore indici riga della matr. jacobiana
	1                ja,   ! :in vettore indici colonna     ""	""
	1                 x,   ! :in vettore variabili del sistema
	1                 a)   ! :out vettore elementi  matrice jacobiana

cgpe  Nota: coincide con la subroutine pse_evajac_pse (ia,ja,x,a) e rispetto alla subroutine
cgpe        jacob è stato tolto il parametro n

!     Calcola il valore della matrice jacobiana in un dato punto x
*&*
      implicit none
      include '../inc/param.inc'    !:costanti
      include '../inc/default.inc'       !:costanti
      include '../inc/nw.inc'       !:costanti
      include '../inc/TH.inc'       !:costanti
      include '../inc/ti.inc'       !:INOUT
      include '../inc/tj.inc'       !:INOUT
      include '../inc/ty.inc'       !:INOUT

*&*
      integer*4 ia(*)        ! :in vettore indici riga della matr. jacobiana
     *         ,ja(*)        ! :in vettore indici colonna     ""      ""
      real*4    x(*)         ! :in vettore variabili del sistema
      real*4    a(*)         ! :out vettore elementi  matrice jacobiana
*&*
      integer*4 I,J,JAJ,im,iv,i_t,k,ieq,jj
      real*4 coefxq,coefxm,coefxv,coefxqs,coefxs,coefxp,coefxc,COEFQ
crsaconce
c      real*4 XQ,XM,XV,XC
c      real*4 dqconce,dpasp2conce,dpman2conce
crsaconce

      INTEGER*4 itop_from_ivar
      external  itop_from_ivar

	integer*4 oicen
!-----------------------------------------------------------------------
!      oice = otipti(e_ce) - (M_OGG + 1)

!  Ciclo sui tronchi di condotta (equazioni di Fergusson)

      ieq = 1
      do i = otipti(e_tr), etipti(e_tr)
        im = opumto(i)
        iv = opuvto(i)
        do j = ia(ieq),ia(ieq+1)-1
          jaj = ja(j)
          i_t = itop_from_ivar(jaj,nw_topvar)
          if (i_t.eq.i) then
crsaconce
c            xq = x(jaj)
c            a(j) = 2*bferty(i)* abs(xq)
            a(j) = 2*bferty(i)* abs(x(jaj))
crsaconce
          else if (i_t.eq.im) then
            a(j) = -aferty(i)
          else
            a(j) = 1
          end if
        end do
        ieq = ieq + 1

      enddo


!	Ciclo sugli elementi speciali

      do k = 1, ne_spe
        do i = otipti(te_spe(k)), etipti(te_spe(k))

          if (hstatj(i) .eq. sta_on .or.
     *        hstatj(i). eq. sta_fr     ) then
!  P di monte = P di valle
            im = opumto(i)
            iv = opuvto(i)
            coefxm = 1
            coefxv = -1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              i_t = itop_from_ivar(jaj,nw_topvar)
              if (i_t.eq.im) then
                a(j) = coefxm
              else
                a(j) = coefxv
              endif
            end do
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_off .or.
     *            hstatj(i) .eq. sta_nr      ) then
!  Se la regolazione e' in off: Q = 0
            coefxq = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxq
            end do
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_q .or.
     *            hstatj(i) .eq. sta_qm .or.
     *            hstatj(i) .eq. sta_qc     ) then

!  Se la regolazione e' in portata: Q = Qs
            coefxq = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxq
            end do
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_pm) then
!  Se la regolazione e' in pressione a monte: Pm = Ps
            im = opumto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

          elseif (hstatj(i) .eq. sta_pv .or.
     *            hstatj(i) .eq. sta_pw .or.
     *            hstatj(i) .eq. sta_rom    ) then
!  se la regolazione e' in pressione a valle o in Pw max o ro max: Pv = Ps
            iv = opuvto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

cgpe-new
!  questa aggiunta rispecchia l'aggiunta nella subroutine creajac_ssdd
!  (la gestione di elementi speciali per i quali si scrivono più equazioni)

	    elseif(hstatj(i) .eq. sta_p ) then
!	se la regolazione e' in pressione (valle e monte)

!  Pv = Ps
            iv = opuvto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

!  Pm = Ps
            im = opumto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pvq ) then
!	se la regolazione e' in pressione a valle e in portata

!  Pv = Ps
            iv = opuvto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

!  Q = Qs
            coefxq = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxq
            end do
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pmq ) then
!	se la regolazione e' in pressione a monte e in portata

!  Pm = Ps
            im = opumto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

!  Q = Qs
            coefxq = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxq
            end do
            ieq = ieq + 1

	    elseif(hstatj(i) .eq. sta_pq ) then
!	se la regolazione e' in pressione (valle e monte) e in portata

!  Pv = Ps
            iv = opuvto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

!  Pm = Ps
            im = opumto(i)
            coefxp = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxp
            end do
            ieq = ieq + 1

!  Q = Qs
            coefxq = 1

            do j = ia(ieq),ia(ieq+1)-1
              jaj = ja(j)
              a(j) = coefxq
            end do
            ieq = ieq + 1
cgpe-new-end
          end if
cgpe          ieq = ieq + 1
        end do
      end do


!	Equazioni di bilancio ai nodi

      do i = otipti(e_pu), etipti(e_pu)

        do j = ia(ieq),ia(ieq+1)-1
          jaj = ja(j)
          i_t = itop_from_ivar(jaj,nw_topvar)
          if (i_t.lt.M_OGG) then
            if (opumto(i_t).eq.i) then
              a(j) = -1
            else
              a(j) = +1
            endif
          else
            a(j) = -1
          end if
        end do

        ieq = ieq + 1
      enddo

!	Equazioni dei consumi per le centrali
c      do i = otipti(e_ce), etipti(e_ce)
c-elena --->considero anche le centrali semplificate
      do jj=1,ne_cen
       if (te_cen(jj).eq.e_ce) then
          oicen=oice
       else
          oicen=oics
       end if
	 do i=otipti(te_cen(jj)),etipti(te_cen(jj))

crsaconce
c       if (lcent_first) then
c        im = opumto(i)
c        iv = opuvto(i)
c        do j = ia(ieq),ia(ieq+1)-1
c          jaj = ja(j)
c          i_t = itop_from_ivar(jaj,nw_topvar)
c          if (i_t.eq.i) then
c            xq = x(nw_topvar(i))
c          else if(i_t.eq.im) then
c            xm = x(nw_topvar(im))
c          else if(i_t.eq.iv) then
c            xv = x(nw_topvar(iv))
c          else
c            xc = x(nw_topvar(i-oicen))
c          end if
c        end do
c
c        xm = sqrt(max(xm,pmin_def))
c        xv = sqrt(max(xv,pmin_def))
c        call qconce(xq,xm,xv,i,coefq,dqconce,dpasp2conce,dpman2conce)
c
c
c        do j = ia(ieq),ia(ieq+1)-1
c          jaj = ja(j)
c          i_t = itop_from_ivar(jaj,nw_topvar)
c          if (i_t.eq.i) then
c            a(j) = -dqconce
c          else if(i_t.eq.im) then
c            a(j) = dpasp2conce
c          else if(i_t.eq.iv) then
c            a(j) = -dpman2conce
c          else
c            a(j) = 1.0
c          end if
c        end do
c
c	 else
crsaconce

        coefxc = 1
        do j = ia(ieq),ia(ieq+1)-1
          jaj = ja(j)
          a(j) = coefxc
        end do

crsaconce
c       endif
crsaconce

       ieq = ieq + 1
      enddo
	end do
c
      return
      end ! pse_evajac

      subroutine inizpq_rk (n,x)

cgpe viene creata dalla subroutine ssdd$src:pse_ini_var e ssdd$src:ott_inizpq

! Inizializza i valori delle variabili incognite con i valori iniziali forniti:
!     n  : I    numero equazioni e incognite
!     x  : I/O  vettore variabili incognite
! Definisce la corrispondenza tra indice topologico e indice vettore di stato
! per pgni elemento topologico (nw_topvar (ind_top) = ind_var)
*&*
      implicit none
*&*
      include '../inc/param.inc'    !: costante
      include '../inc/default.inc'    !: costante
      include '../inc/NW.inc'       !: costante
      include '../inc/ti.inc'       !: in
      include '../inc/th.inc'       !: in
      include '../inc/tj.inc'       !: in
      include '../inc/ty.inc'       !: in
      include '../inc/td.inc'       !: in
      include '../inc/jc.inc'       !: in
c-elena
      include '../inc/staznw_par.inc'       !: in
*&*
      real*4    X
      integer*4 N
      dimension x(n)

      integer*4 I,J
crsaconce
c      real*4    xq,xm,xv,dq,dpa2,dpv2
crsaconce

      integer*4 ind_var
	real*4 qset_real
	external qset_real
!---------------------------------------------------------------------------
!
!       Il vettore x contiene tutte le incognite, in pressione e portata.
!       Per le pressioni dei punti, si lavora in realta`  con i quadrati
!       dei valori di pressione, in modo da avere una maggiore omogeneita`
!       negli ordini di grandezza e un minor grado di non-linearita` nelle
!       equazioni del sistema.
!       Le variabili in x sono ordinate in modo da rispettare l'ordinamento
!       degli elementi della rete nel vettore osorti.
!
!---------------------------------------------------------------------------

crsa$$   basta la lib_validazione ????
!    Calcola la stima iniziale delle portate con l'equazione di fergusson
!    e i valori di temperatura,pressione e zmedio sui tronchi e coefficienti
!    aferty e bferty coerenti solo in caso di distribuzione sufficientemente buona
C    di pressione (telemisure....)
c      do i = otipti(e_tr), etipti(e_tr)
c        call agg_tronco_q(i)
c      enddo

c      do i = otipti(e_ce), etipti(e_ce)
c-elena
C le portate sono sempre zero !!!!!!!!!!!
c      do j=1,ne_cen
c	   do i=otipti(te_cen(j)),etipti(te_cen(j))
c           if (htipto(opumto(i)).eq.e_pu) pmon = prestj(opumto(i))
c           if (htipto(opuvto(i)).eq.e_pu) pval = prestj(opuvto(i))
c           call qconce(porttj(i),pmon,pval,i,consty(i),dq,dpa2,dpv2)
c         end do
c	end do
c-elena
crsa$$   basta la lib_validazione ????

!    Assegna alle portate incognite x i valori precedentemente calcolati o
!    se questi risultano 0,il valore di trigger rq0
      ind_var = 0

      do i = otipti(e_tr), etipti(e_tr)
        ind_var = ind_var + 1
        nw_topvar(i) = ind_var
c_ecol
        if (abs(porttj(i)).gt.0) then
            x(nw_topvar(i)) = porttj(i)
	  else
            x(nw_topvar(i)) = rq0
	  end if
        call agg_tronco(i)
      enddo


!    Inizializza alla portata telemisurata le portate incognite x
!    degli elementi speciali

      do j = 1, ne_spe
        do i = otipti(te_spe(j)), etipti(te_spe(j))
          ind_var = ind_var + 1
          nw_topvar(i) = ind_var
cgpe
          if (hstatj(i).eq.sta_q) then
            x(nw_topvar(i)) = qset_real(i)
	    elseif (hstatj(i).eq.sta_off) then
            x(nw_topvar(i)) = 0.

	    else
            x(nw_topvar(i)) = rq0

	    endif
cgpe-end
        end do
      enddo


!   Inizializza il valore delle pressioni ai nodi incognite x

      do i = otipti(e_pu), etipti(e_pu)
        ind_var = ind_var + 1
        nw_topvar(i) = ind_var
        x(nw_topvar(i)) = prestj(i)**2
      enddo


!   Inizializza il valore dei consumi delle centrali

      oice = otipti(e_ce) - (M_OGG+1)
      do i = otipti(e_ce), etipti(e_ce)
crsaconce
c          xq = x(nw_topvar(i))
c          xm = x(nw_topvar(opumto(i)))
c          xv = x(nw_topvar(opuvto(i)))
c          xm = sqrt(max(xm,pmin_def))
c          xv = sqrt(max(xv,pmin_def))
c          call qconce(xq,xm,xv,
c     -                i,consty(i),dq,dpa2,dpv2)
          consty(i) = 0
crsaconce
		ind_var = ind_var + 1
          nw_topvar(i-oice) = ind_var

		x(nw_topvar(i-oice)) = consty(i)
      enddo
c-elena
!   Inizializza il valore dei consumi delle centrali semplificate
      oics = otipti(e_cs) - (M_OGG+max_ce+1)
      do i = otipti(e_cs), etipti(e_cs)
crsaconce
c          xq = x(nw_topvar(i))
c          xm = x(nw_topvar(opumto(i)))
c          xv = x(nw_topvar(opuvto(i)))
c          xm = sqrt(max(xm,pmin_def))
c          xv = sqrt(max(xv,pmin_def))
c          call qconce(xq,xm,xv,
c     -                i,consty(i),dq,dpa2,dpv2)
          consty(i) = 0
crsaconce
		ind_var = ind_var + 1
          nw_topvar(i-oics) = ind_var
		x(nw_topvar(i-oics)) = consty(i)
      enddo
c-elena
      return
      end

*** 12PNEWTON.FOR *** metodo delle tangenti ********************************