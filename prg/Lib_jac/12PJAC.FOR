*** 12PJAC.FOR *** calcolo dello jacobiano per il metodo di newton *******

!---------------------------------------------------------------------------
cgpe	subroutine ordjac(      !  Costruisce i vettori che indicano 
cgpe	                        !  l'ordinamento con il quale vanno eseguite
cgpe	                        !  le operazioni sulle righe e colonne della
cgpe	                        !  matrice jacobiana per la sua fattorizzazione
cgpe	1                 n,    !  :in ordine della matrice
cgpe	1                 ia,   !  :in vettore indici riga dello jacobiano
cgpe	1                 ja,   !  :in vettore indici colonna dello jacobiano
cgpe	1                 kr0,  !  :out vettore ordinamento righe
cgpe	1                 kc0,  !  :out vettore ordinamento colonne
cgpe	1                 ic0)  !  :out vettore inverso di kc0
*&*
cgpe	include '../inc/param.inc'	!: costante
cgpe
cgpe	dimension
cgpe	1	kr0(n),
cgpe	1	kc0(n),
cgpe	1	ic0(n),
cgpe	1	ia(*),
cgpe	1       ja(*)
*&*
!---------------------------------------------------------------------------
!
!	Per il momento, la matrice Jacobiana non viene riordinata prima
!	della soluzione del sistema
!
!---------------------------------------------------------------------------
!   Inizializza i vettori ordinamento righe e colonne per la
!   matrice jacobiana
cgpe	do j = 1, n
cgpe	    kr0(j) = j
cgpe	    kc0(j) = j
cgpe	    ic0(j) = j
cgpe	enddo

cgpe	return
cgpe	end  ! ORDJAC
      subroutine inista_ssdd()

cgpe  viene presa dalla subroutine ssdd$src:inista()

!     Inizializza gli stati di regolazione e il vettore di appoggio valsjc

*&*
      implicit none
      include '../inc/param.inc'    !:costante
      include '../inc/default.inc'    !:costante
      include '../inc/th.inc'       !:costante
      include '../inc/ti.inc'       !:in
      include '../inc/ty.inc'       !:in
      include '../inc/tj.inc'       !:in
      include '../inc/td.inc'       !:in
      include '../inc/nw.inc'       !:in
      include '../inc/jc.inc'       !:out
*&*
      integer*4 I,J
	real*4 qset_real
	external qset_real

*&*
      do j = 1, ne_spe
        do i = otipti(te_spe(j)), etipti(te_spe(j))
          if (hstatj(i) .eq. sta_pv) then
            valsjc(i) = pvsetj(i)
          elseif (hstatj(i) .eq. sta_pm) then
            valsjc(i) = pmsetj(i)
          elseif (hstatj(i) .eq. sta_q) then
crsapcls_con set fissati nel primo giro completo
crsapcls            valsjc(i) = qsettj(i)
            valsjc(i) = qset_real(i)
          endif
        end do
      enddo
      




      do i = otipti(e_pu), etipti(e_pu)
crsapcls_con prelievi fissi nel primo giro completo
crsapcls        valsjc(i) = porttj(i)
cccmar        valsjc(i) = porttj(i)*pcls_def/pclstd(i)

cmar------------------
      IF (FLAG_CONVERGENZA.EQ.1)THEN
           
	    valsjc(i) = porttj(i)*pcls_def/pclstd(i)

          ELSEIF (FLAG_CONVERGENZA.EQ.2)THEN


	    valsjc(i) = porttj(i)


	    ELSEIF (FLAG_CONVERGENZA.EQ.3)THEN
          


                valsjc(i) = porttj(i)*pcls_def/pclstd(i)


	    ELSEIF (FLAG_CONVERGENZA.EQ.4)THEN



                valsjc(i) = porttj(i)




	ENDIF


cmar------------------
       
      enddo



c
!   Consumi delle centrali

      do i = otipti(e_ce), etipti(e_ce)
          valsjc(i-oice) = consty(i)
      enddo
c-elena
!   Consumi delle centrali semplificate

      do i = otipti(e_cs), etipti(e_cs)
          valsjc(i-oics) = consty(i)
      enddo
c-elena
c
      return
      end
c
