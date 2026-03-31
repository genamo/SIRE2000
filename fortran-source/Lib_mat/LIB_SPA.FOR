!# Versione 3.01.0000
!-----------------------------------------------------------------------
**** operazioni su matrici *******************
!-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE GAUSS(N_N,Y,IA,JA,A_A,B_B,WK,IWK,IDMAX,IDM,
!#ifdef NOINT_2012    !rsa
!     *                 K_RANGO,ier)
!#else
     *                 K_RANGO,EL_REC,ier)
!#endif
! Questa subroutine esegue la fattorizzazione di Gauss per una 
! matrice quadrata. Questa routine considera anche il caso in cui il rango 
! non e' pieno, rimandando il controllo alla routine chiamante affinchè 
! (in particolari condizioni) possa imporre il valore di recupero per la 
! variabile rilevata come linearmente dipendente. 
! Quando il rango è pieno la soluzione 
! e' memorizzata in B_B
        implicit none
                                                
C 
C                    SPECIFICATIONS FOR ARGUMENTS
c	   N_N = dimensione della matrice quadrata A_A
c        Y   = vettore contenente valori di default delle variabili
c	   IA  = indice di riga della matrice A_A
c	   JA  = indice di colonna della matrice A_A
c	   A_A = matrice A_A
c 	   B_B = input  Termine noto del sistema (vettore di dimensione N_N)
c                       output Soluzione
c        WK  = area di lavoro reale  di dimensioni IDMAX
c        IWK = area di lavoro intera di dimensioni IDMAX
c        IDMAX = dimensione delle aree di lavoro
c        IDM   = dimensione massima delle matrici decpmposte
c	k_rango= rango
!#ifdef NOINT_2012    !rsa
!#else
c	el_rec = primo elemento recuperato nel caso k_rango < N_N
c                0 nessun recupero
!#endif
c       ier   = indice di errore
c       ier <0   errore ko
c       ier = 0  soluzione trovata (el_rec=0)
c       ier = 0  rango non pieno (el_rec diverso da 0)

      integer*4    N_N,IA(*),JA(*)
      real*4       A_A(*),B_B(*),WK(*),y(*)
      integer*4    IWK(*)
      integer*4    IDMAX,IDM,ier,k_rango
!#ifdef NOINT_2012    !rsa
!#else
      integer*4    el_rec
!#endif

      integer*4    nkr,nkc,nic,ir,jr,iu,ju,ipiv
      save         nkr,nkc,nic,ir,jr,iu,ju,ipiv
      integer*4    r_r,u_u,diag,aux
      save         r_r,u_u,diag,aux

      integer*4    k

C------------------------------------------------------------------------
!#ifdef NOINT_2012    !rsa
!#else
        el_rec = 0
!#endif
!                    vettore di interi   IWK
        nkr=1
        nkc=nkr+n_n
        nic=nkc+n_n
        ir=nic+n_n
        jr=ir+n_n+1
        iu=jr+idm
        ju=iu+n_n+1
	  ipiv=ju+idm
        if(ipiv+n_n+1.gt.idmax) then
           IER=-101
           call gest_error(1,0,'Gauss',
     *         'Overflow di IntWorkArea',IER)
           goto 1111
        END IF
C
C                                 VETTORE di reali
        r_r=1
        u_u=r_r+idm
        diag=u_u+idm
	  aux=diag+n_n
        IF(aux+n_n.GT.IDMAX) THEN
           IER=-102
           call gest_error(1,0,'Gauss',
     *         'Overflow di RealWorkArea',IER)
           goto 1111
        END IF
c
        call ordspa(n_n,iwk(nkr),iwk(nkc),iwk(nic))
        call decspa(n_n,ia,ja,a_a,idm,iwk(nkr),iwk(nkc),iwk(nic),
     *              iwk(ir),iwk(jr),wk(r_r),iwk(iu),iwk(ju),
     *              wk(u_u),wk(diag),iwk(ipiv),wk(aux),k_rango)
        if(k_rango.lt.0) then
          IER=-103
          call gest_error(1,0,'Gauss',
     *         'Errore nella decomposizione',IER)
          goto 1111
        end if
!                      qui si arriva con k_rango>0 e IER = 0
        if(k_rango.ne.n_n) then
             k = k_rango+1
             if (el_rec.eq.0) el_rec = iwk(nkc-1+k)
	       return
        end if

        call agg_dec(k_rango,n_n,iwk(nkr),
     *               iwk(iu),iwk(ir),iwk(jr),
     *               wk(r_r),wk(diag),
     *               b_b,wk(aux))
        call solspa(n_n,iwk(nkc),iwk(nic),
     *                  iwk(iu),iwk(ju),wk(u_u),b_b,wk(aux))
C      
1111    return
        end
C--------------------------------------------------------------
	subroutine decspa (    ! Esegue la fattorizzazione RU della 
	                           ! matrice sparsa
	1		   n,      ! :in ordine della matrice
	2                  ia,     ! :in vettore indici riga
	3                  ja,     ! :in vettore indici colonna
	3                   a,     ! :in elementi della matrice
	3                  idm,    ! :dimensione massima per le matr. decomp.
	4                  kr,     ! :in/out vettore ordinamento righe
	5                  kc,     ! :in/out vettore ordinamento colonne
	6                  ic,     ! :in/out inverso del vettore kc
	7                  irkm,
	8                  ilkm,
	9                  rkm,
	9                  iu,
	9                  ju,
	9                  u,
	9                  rkn,
	9                  ip,
	9                  xt,
	7		   ier )   ! :out codice di errore
				   !       > 0  rango della matrice
				   !       =0	n.equazioni = 0 
				   !       -1	la riga k  e' nulla
				   !       -2	la riga k ha un elem. duplicato
				   !       -3	pivot = 0 per la riga k
				   !       -4	memoria riservata a u
				   !		 superata alla riga k
*&*
        implicit none	
*&*
c-elena
      include '../inc/staznw_par.inc'    ! :parametri
c-elena
        integer*4 N,idm
        integer*4 KR(*),IC(*),KC(*)
        integer*4 IA(*),JA(*),ier
        real*4    a(*)
c----------------

        CHARACTER*80 msg_err
!-----------------------------------------------------------------------
	integer*4   iu(*),
	1	    ju(*),
	1	    irkm(*),
	1	    ilkm(*),
	1	    ip(*)
c
        real*4      u(*),
	1	    rkm(*),
	1 	    rkn(*),
	1	    xt(*)


        integer*4 j,k,JUPTR,vk,nlkm
        integer*4 JMIN,JMAX,jaj,vj,ppk,pk,vi,i,juj
        integer*4 pv,v,MAXCL,MAXC,NZCNT
        integer*4 ck

c-elena        real*4    UNO,ZERO
        real*4    RLKI
        real*4    xpvmax,xpv,dk,eps
cgpe        real*4     zerom
      real*4 base

*&*
	if (n.eq.0) go to 1001
        ier = 0

!  Controllo sulla precisione 
!  ---> Norma di Frobenius  25-10-200
! per tenere conto delle precisioni di macchina
cgpe
cgpe!        eps = zerom()
cgpe	call zerom(eps,base)
cgpe-end

!        dk=0.
!       do k=1,n
!           do j=ia(k),ia(k+1)-1
!              dk = dk+a(j)*a(j)
!           end do
!        end do
!        dk = sqrt(dk)    ! Norma Frobenius
!        eps = eps*dk
!  ---> Norma di Frobenius  25-10-200

!#ifdef NOINT_2012    !rsa
!        eps = 0.0         ! valore ideale
!#else
! per tenere conto delle precisioni di macchina
cgpe        eps = zerom()
	call zerom(eps,base)
cgpe-end
!#endif
!----------------------------------------------
!  inizializza area di lavoro e puntatori a ju

	do j = 1, n
	  xt(j) = zero
	enddo

	iu(1) = 1
	juptr = 0
	nlkm  = 0
!      
!      l'indice k indica la riga della matrice R ed U, per sapere a quale riga/colonna
!      di A corrisponde si utilizzano i vettori kr(k) e kc(k). 
!      Se invece si conosce la colonna di A il vettore ic() individua la corrispondente
!      colonna nella matrice R e U

! Esegue la fattorizzazione simbolica e numerica riga per riga

	do k =1, n

	!  Inizializza la lista per questa riga. La r(k)esima riga di a
	!  diventa la kesima riga di u 

	   ip(n+1) = n + 1
	   vk   = kr(k)			!??era krjc
	   irkm(k) = nlkm + 1

	!  Genera la lista delle adiacenze per questa riga, secondo l'ordine
	!  corrente di colonna di u

	   jmin = ia(vk)
	   jmax = ia(vk+1) - 1
	   if(jmin.gt.jmax) go to 1002
	   j = jmax
20	   jaj = ja(j)
	   vj = ic(jaj)
	
	!  memorizza a(k,j) nel vettore di lavoro

	   xt(vj) = a(j)

	!  Inserisce vj nella lista delle adiacenze di vk

	   ppk = n + 1
30	   pk = ppk
	   ppk = ip(pk)
	   if(ppk - vj) 30,1003,40
40	   ip(vj) = ppk
	   ip(pk) = vj
	   j = j - 1
	   if(j.ge.jmin) go to 20

	!  Calcola la k-esima riga di u

	   vi = n + 1
50	   vi = ip(vi)
	   if(vi .ge. k) go to 110

	!  vi minore di k -- Calcola l'elemento l(k,i) e lo memorizza. Unisce
	!  la lista delle adiacenze di vi con quella di vk

	   nlkm = nlkm + 1
	   if(nlkm .gt. idm) go to 1005
	   ilkm(nlkm) = vi
	   rlki = -xt(vi)
	   rkm(nlkm) = rlki
	   xt(vi) = zero
	   ppk = vi
	   jmin = iu(vi)
	   jmax = iu(vi+1) - 1
	   if(jmin .gt. jmax) go to 50
	
	   do j = jmin, jmax
		juj = ju(j)
		vj = ic(juj)

	   !	se vj e' gia' tra le adiacenze di vk, salta l'inserzione

		if(xt(vj) .ne. zero) go to 90

	   !	Inserisce vj nella lista di adiacenze di vk. Riporta ppk a vi
	   !	se e' stato oltrepassato il punto corretto di inserimento.

		if(vj - ppk) 60,90,70
60		ppk = vi
70		pk = ppk
		ppk = ip(pk)
		if(ppk - vj) 70,90,80
80		ip(vj) = ppk
		ip(pk) = vj
		ppk = vj

	   !	Calcola u(k,j) = u(k,j) - l(k,i)*u(i,j)

90		xt(vj) = xt(vj) + rlki*u(j)
	   enddo

	   go to 50

	!  Pivot-- Scambia l'elemento di modulo maggiore della k-esima
	!  riga di u con l'elemento diagonale

	!  Trova l'elemento di modulo maggiore

110	   if(vi .gt. n) go to 1004
	   xpvmax = abs(xt(vi))
	   maxc = vi
	   nzcnt = 0
	   pv = vi

120	      v = pv
	      pv = ip(pv)
	      if(pv .gt. n) go to 130
	      nzcnt = nzcnt + 1
	      xpv = abs(xt(pv))
	      if(xpv .le. xpvmax) go to 120
	      xpvmax = xpv
	      maxc = pv
	      maxcl = v
	      go to 120

130	   if (abs(xpvmax) .le. eps) go to 1004

	!  Se vi = k, c'e' un elemento diagonale che dev'essere cancellato,
	!  altrimenti cancella l'elemento che diverra' quello diagonale

	   if (vi .eq. k) go to 140
	   if (vi .eq. maxc) go to 140
	   ip(maxcl) = ip(maxc)
	   go to 150
140	   vi = ip(vi)

	!  Calcola e memorizza d(k) = 1/l(k,k) ed esegue lo scambio

150	   dk = uno/xt(maxc)
           ier = ier + 1
	   rkn(k) = dk
	   xt(maxc) = xt(k)
	   i = kc(k)
	   kc(k) = kc(maxc)
	   kc(maxc) = i
	   ck = kc(k)
	   ic(ck) = k
	   ic(i) = maxc
	   xt(k) = zero

	!  Calcola il valore di iu(k+1) e controlla se c'e' un overflow

	   if (iu(k) + nzcnt .gt. idm) go to 1005
	!  Trasferisce gli indici di colonna dalla lista a ju. Le colonne
	!  sono memorizzate nell'ordine corrente con il numero di colonna
	!  originale c(j) memorizzato per la corrente colonna j.

	   j = vi

	   do while (j .le. n)
		if(xt(j) .ne. zero)then
		  juptr = juptr + 1
		  ju(juptr) = kc(j)
		  u(juptr) = xt(j)*dk
	 	  xt(j) = zero
		endif
		j = ip(j)
	   enddo
	   iu(k + 1) = juptr + 1

	enddo
	irkm(n+1) = nlkm + 1

	return

!	ritorni con errore

1001	ier = -5
C	call err('pms_01')
        msg_err = 'Ordine della matrice nullo '
        call gest_error(1,0,'DECSPA',msg_err,ier)

	return

!	la riga della matrice A tutta nulla

1002	ier = -1
C	call erri('pms_02',k)
        msg_err = 'La riga k di a e'' nulla '
        call gest_error(1,0,'DECSPA',msg_err,k)

	return

!	la riga k-sima della matrice A ha un elemento duplicato

1003	ier = -2
C	call erri('pms_03',k)
        msg_err = 'La riga k di a ha un elemento duplicato '
        call gest_error(1,0,'DECSPA',msg_err,k)

	return

!	La matrice A non ha rango pieno: rango(A)=k
!       (la decomposizione si e' fermata con pivot nullo)

1004	continue                                         
	return

!	sfondamento area di memoria

1005	ier = -4
C	call erri('pms_05',k)
        msg_err = 'Memoria riservata a u superata ' //
     -            'alla riga k'
        call gest_error(1,0,'DECSPA',msg_err,k)


	return 
        end ! decspa
!-----------------------------------------------------------------------
      subroutine solspa(n,kc,ic,iu,ju,u,b,x)
!                              Risolve il sistema con matrice decomposta
!                              dalla decspa; la soluzione e' 
!                              memorizzata in b;
!                              x e' output della routine AGG_DEC
!---------------------------------------------------------------------
      implicit none
      integer*4   n,kc(*),ic(*),iu(*),ju(*)
      real*4      u(*),b(*),x(*)

      real*4  yk
      integer*4   k,vk,jmin,jmax,j,vi,juj,ck,i
!       
!-----------------------------------------------------------------------

!	Risolve il sistema ux = y e riordina x

	k = n

	do i = 1, n
	   yk = x(k)
	   jmin = iu(k)
	   jmax = iu(k+1) - 1
	   if (jmin .LE. jmax) THEN       !l'elemento diag. è memorizzato a 

		   do j = jmin, jmax
			juj = ju(j)
			juj = ic(juj)
			yk = yk - u(j)*x(juj)
		   enddo
	   endif

	   x(k) = yk
	   ck = kc(k)
	   b(ck) = yk
	   k = k - 1
	enddo

	RETURN
	END ! SOLSPA
!-----------------------------------------------------------------------
        Subroutine agg_dec (k_rango
     *               ,n  !ordine della matrice
     *               ,kr !vettore che indica l'ordine delle righe di A
     *               ,iu   !indici di riga della matrice U
     *               ,irkm !indici di riga della matrice R
     *               ,ilkm !indici di colonna della matrice R
     *               ,rkm  !matrice R
     *               ,rkn  !termini diagonali 
     *               ,b_b  !termine noto
     *               ,x)   !termine noto aggiornato con la decomposizione
!----------------------------------------------------------------------
!completa la decomposizione LU operata dalla routine decspa, qualora 
!il rango della matrice decomposta non sia pieno.
!-----------------------------------------------------------------------
!
        implicit   none
        integer*4  k_rango,n
        integer*4  kr(*),iu(*),irkm(*),ilkm(*)
        real*4     rkm(*),rkn(*),b_b(*),x(*)

        real*4  yk
        integer*4  k
        integer*4  vk,jmin,jmax,vi,j
!
!completo il termine noto

	do k = 1, k_rango

!	esegue sul vettore b_b le stesse operazioni eseguite sulle righe
!	di a, ottenendo un vettore x.

	    vk = kr(k)
	    yk = b_b(vk)
	    jmin = irkm(k)
	    jmax = irkm(k+1) - 1
	    if (jmin .LE. jmax) THEN
		    do j = jmin, jmax
			vi = ilkm(j)
			yk = yk + rkm(j)*x(vi)
		    enddo
	    ENDIF
	    x(k) = yk*rkn(k)
	
	enddo
	RETURN
	END ! agg_dec
c---------------------------------------------------------------------
         subroutine ordspa( !  Costruisce i vettori che indicano 
	                        !  l'ordinamento con il quale vanno eseguite
	                        !  le operazioni sulle righe e colonne della
	                        !  matrice jacobiana per la sua fattorizzazione
	1                 n,    !  :in ordine della matrice
	1                 kr0,  !  :out vettore ordinamento righe
	1                 kc0,  !  :out vettore ordinamento colonne
	1                 ic0)  !  :out vettore inverso di kc0


*&*
        implicit none
*&*
        integer*4 n,j,kr0,kc0,ic0

	dimension
	1	kr0(*),
	1	kc0(*),
	1	ic0(*)
*&*
!---------------------------------------------------------------------------
!
!	Per il momento, la matrice Jacobiana non viene riordinata prima
!	della soluzione del sistema
!
!---------------------------------------------------------------------------

!   Inizializza i vettori ordinamento righe e colonne per la
!   matrice jacobiana



	do j = 1, n
	    kr0(j) = j
	    kc0(j) = j
	    ic0(j) = j
	enddo


	return
	end  ! ordspa
c--------------------------------------------------------------------------
C---------------------------------------------------------------
