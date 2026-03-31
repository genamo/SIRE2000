**** LIB_GAS_TC.FOR ***** PACKAGE CALCOLO TEMPERATURE E COMPOSIZIONI ****
!---------------------------------------------------------------------------
**** Aggiornamento della Temp e Comp ****

      subroutine agg_tc (*)    !  I/O error
*&*
!      noti i valori di pressioni e portate procede all'aggiornamento 
!      dei valori delle composizioni e delle temperature tramite soluzione
!      di due sistemi lineari (Newton).
!      Raggiunta la convergenza, la soluzione viene salvata nel vettore di
!      stato.

      implicit none
      include '../inc/param.inc'
!
      include '../inc/ti.inc'	!: in

      integer*4 N_PUN

!
!
!   Calcolo il numero dei nodi 
      n_pun = etipti(e_pu) + 1 - otipti(e_pu)

!   La composizione e la temperatura vengono calcolate a partire dalle p e q
!   presenti nel common TJ.
!   Successivamente vengono aggiornate le variabili dipendenti dalle 
!   composizioni e dalle temperature

      call compos(n_pun,*100)

      call ntemp(n_pun,*100)

      return  ! agg_tc
100   return  1

      end
!-----------------------------------------------------------------------
      subroutine compos (n,*)

!      Questa subroutine calcola il valore delle composizioni ai nodi
!      risolvendo il sistema lineare che viene costruito a partire dai
!      bilanci di massa sui nodi e dai valori noti delle composizioni
!      in alcuni punti.

*&*
      implicit none
*&*
      include '../inc/param.inc'	! : costanti
      include '../inc/th.inc'	! : costanti
      include '../inc/ti.inc'	! : in
      include '../inc/tc.inc'	! : in/out
      include '../inc/cc.inc'	! : costanti
      include '../inc/tj.inc'	! : in
      include '../inc/err.inc'	! : in
ctemprsa
      include '../inc/default.inc'
      include '../inc/tv.inc'	! : in
ctemprsa
c-elena
      include '../inc/staznw_par.inc'	! : parametri
c-elena
      integer*4 n       !  :in numero di equazioni (nodi)

      integer*4 OINODI,JACNT,NEQ,IA,I,GAS
      integer*4 IE,IM,ONM,IND,JA,IV,ONV
      real*4    A,B,QTOT
      integer*4 K_RANGO
      integer*4 cp_m_var
      parameter (cp_m_var=max_pu)
      integer*4 cp_m_jac
      parameter (cp_m_jac=5*cp_m_var)
      integer*4 IDMAX
      parameter (IDMAX=2*cp_m_jac+10*cp_m_var)
      real*4    wk
      integer*4 iwk
      integer*4 ier
	character*(l_D_ERR) msgerr


      dimension
     -          ia(cp_m_var)   ! vettore indici riga della matrice del sistema
     -         ,ja(cp_m_jac)   ! vettore indici colonna  ""        ""
     -         ,a(cp_m_jac)    ! vettore contenente la matrice del sistema
     -         ,iwk(IDMAX)      ! Area di lavoro per la decomposizione di GAUSS
     -         ,wk(IDMAX)       !   "        "                "             "
     -         ,b(cp_m_var,m_comp) ! vettore termini noti del sistema
*&*
      real*4 compapp(max_pu,m_comp)
      logical*2 elem_uni
      external  elem_uni
      integer*4 el_rec,ini_e_pu,igas
!---------------------------------------------------------------------------
      oinodi = otipti(e_pu) - 1
      ini_e_pu = otipti(e_pu)
!   Per ogni componente del gas, costruisce la matrice (in forma sparsa)
!   del sistema di equazioni ai nodi, e chiama il package che lo risolve


!   Inizializza i puntatori alle righe e agli elementi della matrice
      jacnt = 1	
      neq = 1
      ia(1) = 1
!   Inizializza vettore
      do i = 1, cp_m_jac
        a(i) = 0.0
      enddo
!----------Per ogni punto, -----------------------------------------------

1000  continue
      do i = ini_e_pu, etipti(e_pu)
        do gas = 1, m_comp
          b(neq,gas) = 0.0
        enddo
        qtot = 0.0	! quantita` di gas entrante nel nodo

         ie = oseeto(i)

!------- Ciclo sugli elementi entranti ------------------------------------

        do while (ie .ne. i)
          if(porttj(ie) .gt. zeroTC) then  ! Entrante come flusso          
!	Se si tratta di elementi unari la composizione del gas
!	entrante nel nodo e` nota

              if (elem_uni(ie)) then
                do gas = 1, m_comp
                  b(neq,gas) = b(neq,gas) +
crsacomp
c     *                         porttj(ie)*comptc(ie,gas)
     *                         porttj(ie)*comptc_set(ie,gas)  ! impongo set composizione
crsacomp
                enddo
              else
!	altrimenti e` anch'essa un'incognita dell'equazione

                im = opumto(ie)
                onm = im - oinodi
                do ind = ia(neq), jacnt - 1
                  if(ja(ind) .eq. onm) then
                    a(ind) = a(ind) - porttj(ie)
                    go to 50
                  endif
                enddo
                ja(jacnt) = onm
                a(jacnt) = -porttj(ie)
                jacnt = jacnt + 1
              endif
50            qtot = qtot + porttj(ie)
          endif
!!!        endif
          ie = oseeto(ie)
        enddo		    	
!	Ciclo sugli elementi uscenti-------------------------------------------

        ie = oseuto(i)
        do while (ie .ne. i)
          if(porttj(ie) .lt. -zeroTC) then    ! Entrante come flusso
!	Se si tratta di elementi unari la composizione del gas
!	entrante nel nodo e` nota

              if (elem_uni(ie)) then
                do gas = 1, m_comp
                  b(neq,gas) = b(neq,gas) -
crsacomp
c     *                         porttj(ie)*comptc(ie,gas)
     *                         porttj(ie)*comptc_set(ie,gas)  ! impongo set composizione
crsacomp
                enddo
              else
                iv = opuvto(ie)
                onv = iv - oinodi
                do ind = ia(neq), jacnt - 1
                  if(ja(ind) .eq. onv) then
                    a(ind) = a(ind) + porttj(ie)
                    go to 60
                  endif
                enddo
                ja(jacnt) = onv
                a(jacnt) = porttj(ie)
                jacnt = jacnt + 1
              endif
60            qtot = qtot - porttj(ie)
!              endif
          endif
!!!        endif
          ie = oseuto(ie)
        enddo

        ja(jacnt) = neq
        if(qtot .ne. 0.0) then
          a(jacnt) = qtot
          jacnt = jacnt + 1
        else
          a(jacnt) = 1.0
          jacnt = jacnt + 1
          do gas = 1, m_comp
            b(neq,gas) = comptc(i,gas)
          enddo
        endif

        neq = neq + 1
        ia(neq) = jacnt
			
      enddo ! -- fine del ciclo sui nodi

! Soluzione del sistema per ogni componente di gas
      do gas = 1, m_comp
        call gauss (n,comptc(otipti(e_pu),gas),ia,ja,a,b(1,gas),
     *              WK,IWK,IDMAX,Cp_m_JAC,K_RANGO,el_rec,ier)
        if (ier.ne.0) goto 100
cace
!        nel caso di rango non pieno, si impone un valore di default alla 
!        composizione relativa al primo elemento linearmente dipendente; i.e.
!        si impone l'equazione c(el_rec)= valore di default
cace 
        if (el_rec.ne.0) then
	   ini_e_pu = otipti(e_pu)-1+el_rec
         neq = el_rec
         jacnt = ia(neq)

         ja(jacnt) = neq
     	   a(jacnt) = 1.0
         jacnt = jacnt + 1
         do igas = 1, m_comp
            b(neq,igas) = comptc(ini_e_pu,igas)
         enddo

         neq = neq + 1
         ia(neq) = jacnt

	   ini_e_pu = ini_e_pu+1

	   goto 1000
        endif

        neq = 1
        do i = otipti(e_pu), etipti(e_pu)
          compapp(pgrato(i),gas) = b(neq,gas)
          neq = neq + 1
        enddo
      enddo
      ie = 0                               !per evitare composizioni tutte nulle
      do i = otipti(e_pu), etipti(e_pu)
        qtot = 0.0
        do gas = 1, m_comp
          qtot = qtot + compapp(pgrato(i),gas)
        enddo
        if (qtot.gt. 0.9999) then
          do gas = 1, m_comp
            comptc(i,gas) = compapp(pgrato(i),gas)
          enddo
        else
	    ie = ie+1                ! contatore delle composizioni nulle
        endif
      enddo

      call aggvarc(*200)
      return

100   continue 
		call gest_error(3,0,' ',' ',0)
          call tipo_err (1,err_ko,msgerr)
		call gest_error(3,0,' ',msgerr,0)
	    call gest_error(3,0,' ',
     *' Errore nel GAUSS delle COMPOSIZIONI',0)
200   continue
      return 1 ! uscita KO
      end ! COMPOS

      subroutine ntemp (n,*)
*&*
!     calcola le temperature dei nodi iterando piu` volte la routine ntemp1
*&*
      implicit none	
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'	!: in/out
	include '../inc/err.inc'
c-elena
      include '../inc/staznw_par.inc'	!parametri
c-elena
*&*
      integer*4 n      !  :in numero di equazioni (nodi)

      integer*4 J,OP,ITER
      real*4    RSAVET,DTMAX

      dimension rsavet(m_ogg)

	character*(l_D_ERR) msgerr
!----------------------------------------------------------------------
      j = 0
      do op = otipti(e_pu), etipti(e_pu)
        j = j + 1
        rsavet(j) = temptj(op)
      enddo

c      call initz()
      call aggvart()
      do iter = 1, maxit
        call ntemp1(n,*100)

        call aggvart()

        dtmax = 0.0
        j = 0
        do op = otipti(e_pu), etipti(e_pu)
          j = j + 1
          dtmax = max(dtmax,abs(temptj(op)-rsavet(j)))
          rsavet(j) = temptj(op)
        enddo
        if (dtmax .lt. repsTC) goto 50
      enddo
 
	call gest_error(3,0,' ',' ',0)
      call tipo_err (1,err_ko,msgerr)
	call gest_error(3,0,' ',msgerr,0)
	call gest_error(3,0,' ',
     *' Superato numero max di iterazioni nel calcolo Temperatura'
     *                                                      ,0)
      return1  ! uscita con errore


50    return

100   continue

D     print * , 'Le iterazioni in temperatura non convergono',dtmax

      return1  ! uscita con errore
      end ! ntemp

!----------------------------------------------------------------------
!----------------------------------------------------------------------
      subroutine ntemp1 (n,*)

!     Questa subroutine calcola i valori delle temperature dei nodi
!     risolvendo il sistema lineare ottenuto a partire dai bilanci
!     entalpici sui nodi.
*&*
      implicit none	
*&*
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'	!: in
      include '../inc/cc.inc'
      include '../inc/td.inc'	!: in
      include '../inc/tx.inc'	!: in
      include '../inc/top_app_ele.inc'	!: in
      include '../inc/ty.inc'	!: in
      include '../inc/tj.inc'	!: in/out
      include '../inc/tz.inc'	!: in/out
      INCLUDE '../inc/stations.inc'
      include '../inc/err.inc'	! : in
ctemprsa
      include '../inc/tv.inc'	!: in/out
ctemprsa
      include '../inc/cap_bon_prv.inc'	!: in/out
      include '../inc/rk.inc'	!: in/out
c-elena
      include '../inc/staznw_par.inc'	!: parametri
c-elena
cgpe
      include '../inc/nw.inc'	!:lcent_first
cgpe-end
      integer*4  n     !  :in numero di equazioni (nodi)

      integer*4 JACNT,NEQ,IA,I,IE,IM,ONM,IND,JA,IV,ONV
      real*4    TCOEF,BVAL,CTI,CN
      real*4    A,B,DHM,DHV,DELTAT,TOUT,PA,PM,XX,XEEA
      logical*2 LINSER

      real*4    prop
      external  prop

      logical*2 elem_uni,elem_bin
      external  elem_uni,elem_bin

      integer*4 K_RANGO
      integer*4 tp_m_var
      parameter (tp_m_var=max_pu)
      integer*4 tp_m_jac
      parameter (tp_m_jac=5*tp_m_var)
      integer*4 IDMAX
      parameter (IDMAX=2*tp_m_jac+10*tp_m_var)
      real*4    wk
      integer*4 iwk
      integer*4 ier
	character*(l_D_ERR) msgerr

ctemprsa      logical*2 l_ko

      dimension
     -          ia(tp_m_var)   ! vettore indici riga della matrice del sistema
     -         ,ja(tp_m_jac)   ! vettore indici colonna  ""        ""
     -         ,a(tp_m_jac)    ! vettore contenente la matrice del sistema
     -         ,iwk(IDMAX)      ! Area di lavoro per la decomposizione di GAUSS
     -         ,wk(IDMAX)       !   "        "                "             "
     -         ,b(tp_m_var)    ! vettore termini noti del sistema

*&*
      integer*4 el_rec,ini_e_pu

      real*4    flow

      logical*2 elem_vlv
	external  elem_vlv

      logical*2 elem_cen
	external  elem_cen
!---------------------------------------------------------------------------
ctemprsa      l_ko = .false.
      jacnt = 1
      neq = 1
      ia(1) = 1
!   Inizializza vettore
      do i = 1, tp_m_jac
        a(i) = 0.0
      enddo

!  Ciclo sui punti della rete, per la costruzione del sistema di equazioni

1000  continue
	do i = otipti(e_pu), etipti(e_pu)

    		tcoef = 0.0
    		bval =  0.0

    		ie = oseeto(i)

!--------------  Ciclo sugli elementi entranti -------------

    		do while (ie .ne. i)

		  im = opumto(ie)
		  onm = im - otipti(e_pu) + 1

! ------------------------  Tronco di condotta ----------------------

		  if(htipto(ie) .eq. e_tr) then	

		 	if(porttj(ie) .gt. zeroTC) then

				call tutronc(ie,im,i,cti,cn)

!  controlla che il nodo di monte non sia gia' stato inserito 
!  nell'equazione (caso di tubi paralleli)

				linser = .false.
				do ind = ia(neq), jacnt - 1
					if(onm .eq. ja(ind)) then
						linser = .true.
						go to 50
					endif
				enddo
50				continue


				if(linser) then
					a(ind) = a(ind) + cti*porttj(ie)
			    	else
					ja(jacnt) = onm
			        	a(jacnt) = cti*porttj(ie)
					jacnt = jacnt + 1
				endif
				tcoef = tcoef - porttj(ie)
				bval = bval - cn*porttj(ie)

			endif		

		  elseif(elem_vlv(ie)) then

!	--------- Valvola-------------------------------

		 	if(porttj(ie) .gt. zeroTC) then

			    if(tsettx(ie). eq. R_UND	 ! senza riscaldatore
	1			.or. touttz(ie) .gt. tsettx(ie))then
                    deltat = touttz(ie) - temptj(im)

				  ja(jacnt) = onm
				  a(jacnt) = porttj(ie)*ctvctz(ie)
	 	          jacnt = jacnt + 1
				  bval = bval - porttj(ie)*deltat*ctvctz(ie)

			    else ! con riscaldatore
				  bval = bval - porttj(ie)*tsettx(ie)*ctvctz(ie)
			    endif
			    tcoef = tcoef - porttj(ie)

			endif
!----------------------------------------------------------------------
!	Centrale
!----------------------------------------------------------------------
            elseif(elem_cen(ie)) then

              if(porttj(ie) .gt. zeroTC) then
                if(hstatj(ie) .eq. sta_on .or.
     *             hstatj(ie) .eq. sta_fr) then
                  ja(jacnt) = onm
                  a(jacnt) = porttj(ie)*ctvctz(im)
                  jacnt = jacnt + 1
                else

                  if (lcent_first) then

cgpe-tout                    if(temptj(ie) .lt. tarctx(ie)) then
                    if(touttz(ie) .lt. tarctx(ie)) then
                      ja(jacnt) = onm
c?                      a(jacnt) = porttj(ie)*(1.0 + ((pm/pa)**xeea
c?     *                           - 1.0)/recocc )*ctvctz(ie)
cgpe-tout                      a(jacnt) = porttj(ie)*(temptj(ie)/temptj(im))*
                      a(jacnt) = porttj(ie)*(touttz(ie)/temptj(im))*
     *                           ctvctz(ie)

                      jacnt = jacnt + 1
                    else
c Viene fatta la semplificazione che si riesca a riportare la temperatura
c della centrale alla temperatura massima della centrale
c                      temptj(ie) = tarctx(ie)
cgpe-tout                      bval = bval-porttj(ie)*temptj(ie)*ctvctz(ie)
                      bval = bval-porttj(ie)*touttz(ie)*ctvctz(ie)
                    endif
                  else
cgpe-tout                    bval= bval-porttj(ie)*temptj(ie)*ctvctz(ie)
                    bval= bval-porttj(ie)*touttz(ie)*ctvctz(ie)
                  endif
                endif

                tcoef = tcoef - porttj(ie)
              endif ! if(porttj(ie) .gt. zeroTC)
!----------------------------------------------------------------------
! Importazioni, pozzi, stoccaggi
!----------------------------------------------------------------------
		  elseif(elem_uni(ie)) then


		 	if(porttj(ie) .gt. zeroTC) then
		        bval = bval - porttj(ie)*tsettj(ie)*ctvctz(ie)
				tcoef = tcoef - porttj(ie)
		    endif

		  endif ! htipto

		  ie = oseeto(ie)

		enddo  ! ie - Fine ciclo sugli elementi entranti

!   Ciclo sugli elementi uscenti----------------------
	
          ie = oseuto(i)
          do while (ie .ne. i)
            iv = opuvto(ie)
            onv = iv - otipti(e_pu) + 1
		
! ----- ------ Tronco di condotta ---------------------------------

		    if(htipto(ie) .eq. e_tr) then

		 	if(porttj(ie) .lt. -zeroTC) then
                 flow = abs(porttj(ie))
cverso				call tutronc(ie,i,iv,cti,cn)
				call tutronc(ie,iv,i,cti,cn)
!   Controlla che il nodo di valle non sia gia' stato inserito nell'equazione

				linser = .false.
				do ind = ia(neq), jacnt - 1
					if(onv .eq. ja(ind)) then
						linser = .true.
						go to 60
					endif
				enddo
60				continue

				if(linser) then
c				    a(ind) = a(ind) - cti*porttj(ie)
				    a(ind) = a(ind) + cti*flow
				else
				    ja(jacnt) = onv
c				    a(jacnt) = - cti*porttj(ie)
				    a(jacnt) = cti*flow
				    jacnt = jacnt + 1
				endif
c				bval = bval + cn*porttj(ie)
c				tcoef = tcoef + porttj(ie)
				bval = bval - cn*flow
				tcoef = tcoef - flow
			endif
!----------------------------------------------------------------------
!	Valvole 
!----------------------------------------------------------------------
c-elena		    elseif(htipto(ie) .eq. e_vl) then
		    elseif(elem_vlv(ie)) then

		 	if(porttj(ie) .lt. -zeroTC) then
                  flow = abs(porttj(ie))
			    if(tsettx(ie). eq. R_UND	 ! senza riscaldatore
	1			.or. touttz(ie) .gt. tsettx(ie))then
                    deltat = touttz(ie) - temptj(iv)

				  ja(jacnt) = onv
				  a(jacnt) = flow*ctvctz(ie)
	 	          jacnt = jacnt + 1
				  bval = bval - flow*deltat*ctvctz(ie)

			    else ! con riscaldatore
cT				bval = bval - porttj(ie)*tsettj(ie)*ctvctz(ie)
				  bval = bval - flow*tsettx(ie)*ctvctz(ie)
			    endif
			    tcoef = tcoef - flow

			endif


!----------------------------------------------------------------------
!	Centrali 
!----------------------------------------------------------------------
c		    elseif(htipto(ie) .eq. e_ce) then
c-elena --->considero anche la centrale semplificata
            elseif(elem_cen(ie)) then

              if(porttj(ie) .lt. -zeroTC) then
                  ja(jacnt) = onv 
                  a(jacnt) = - porttj(ie)*ctvctz(ie)
                  jacnt = jacnt + 1
                  tcoef = tcoef + porttj(ie)
              endif

            endif  ! if (htipto
		
            ie = oseuto(ie)	      

          enddo ! ie - Fine ciclo sugli elementi uscenti
!----------------------------------------------------------------------
		if(tcoef .ne. 0.0) then
		    	a(jacnt) = tcoef*ctvctz(i)
			b(neq) = bval 
		else
			a(jacnt) = 1.0
			b(neq) = temptj(i)
		endif
	    	ja(jacnt) = neq
	 	jacnt = jacnt + 1

	    neq = neq + 1
	    ia(neq) = jacnt

	enddo ! j - Fine ciclo sui nodi

        call gauss (n,temptj(otipti(e_pu)),ia,ja,a,b,
     *              WK,IWK,IDMAX,Tp_m_JAC,K_RANGO,el_rec,ier)

        if (ier.ne.0) goto 100
cace
!        nel caso di rango non pieno, si impone un valore di default alla 
!        temperatura relativa al primo elemento linearmente dipendente; i.e.
!        si impone l'equazione T(el_rec)= valore di default
cace 

        if (el_rec.ne.0) then
	   ini_e_pu = otipti(e_pu)-1+el_rec
         neq = el_rec
         jacnt = ia(neq)

         ja(jacnt) = neq
     	   a(jacnt) = 1.0
         jacnt = jacnt + 1
	   b(neq) = temptj(ini_e_pu)

         neq = neq + 1
         ia(neq) = jacnt

	   ini_e_pu = ini_e_pu+1

	   goto 1000
        endif

      neq = 1
      do i = otipti(e_pu), etipti(e_pu)
        temptj(i) = b(neq)
ctemp        temptj(i) = max(b(neq),tmin_sys)
        if (temptj(i).lt.tmin_sys) then
           continue
	     temptj(i)=tmin_sys
	  endif
        neq = neq + 1
      enddo

      
      return

100   continue 
		call gest_error(3,0,' ',' ',0)
          call tipo_err (1,err_ko,msgerr)
		call gest_error(3,0,' ',msgerr,0)
	    call gest_error(3,0,' ',
     *' Errore nel GAUSS delle TEMPERATURE',0)
      return 1 ! uscita KO
      end ! ntemp1

      subroutine tutronc(               ! calcola la temperatura del
                                        ! gas all'uscita di un tronco
     *                   ie,            ! :in offset tronco
     *                   im,            ! :in offset punto di monte
     *                   iv,            ! :in offset punto di valle
     *                   cti,           ! :out coeff. per la T del p. di monte
     *                   cn)            ! :out termine noto
                                        ! Tout = cti*Tm + cn(Tm,Tout,Tamb)
*&*
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/tz.inc'	!: in/out
*&*
      integer*4 IM, IV, IE
      real*4    CTI, CN

      real*4    DHROX, REXP, TLIM
!----------------------------------------------------------------------
      call coef_tutronc(ie,im,iv,tlim,rexp)
      cti = exp(-rexp)*ctvctz(ie)
      cn = tlim*(1 - 	exp(-rexp))*ctvctz(ie)

      return
      end	! tutronc

      subroutine coef_tutronc(    ! calcola coefficienti per la temperatura del
                                  ! gas all'uscita di un tronco
     *                   ie,      !:in offset tronco
     *                   oi,      !:in offset punto di ingresso (rispetto al flusso)
     *                   of,      !:in offset punto di uscita (rispetto al flusso)
     *                   tlim,    !:out t_amb - dislivello - joule-thompson
     *                   rexp)    !:out stanton
*&*
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/tx.inc'	!: in
      include '../inc/tj.inc'	!: in/out
      include '../inc/tz.inc'	!: in/out
*&*
      integer*4 OI, OF, IE

      real*4    DHROX, REXP, TLIM
      real*4    ccfact
!----------------------------------------------------------------------
c-elena
C ccfact contiene la conversione di conntx da w/mK a Kcal/hmC
c-elena      ccfact = (conntx(ie)*rluntx(ie))/1162.0
c
c     in SIRE conntx assume il significato di coeff. di conducibilità termica
c     e non di coeff. di scambio termico come in SSDD
c
c      ccfact = conntx(ie)
c-elena
c-ace-bilancio termico
c                                        conntx(ie) = [W/(m K)]
      ccfact = (conntx(ie)*rluntx(ie))/1162.0
c-ace

      dhrox = dhmetz(ie)*(rocitj(of) - rocitj(oi))
cace      dhrox = (dhrotz(of) + dhrotz(oi))/2.0*(rocitj(of) - rocitj(oi))  !
      tlim = tambtz(ie) - abs(porttj(ie))*(dhrox + depxtz(ie))/ccfact
      rexp = ccfact/(abs(porttj(ie))*cpmetz(ie))


      return
      end	! coef_tutronc

      subroutine aggvarc(      !  Aggiorna i valori dei parametri
     *                     *)  !  dipendenti dalle composizioni :
                               !  composizione % in ogni elemento
                               !  caratteristche del gas in ogni oggetto
                               !  parti costanti dei coeff.di Fergusson e Lambda
*&*
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/err.inc'
      include '../inc/TH.inc'
      include '../inc/TI.inc'
      include '../inc/TY.inc'    ! :OUT
      include '../inc/TC.inc'    ! :IN
      include '../inc/tj.inc'    ! :IN
      include '../inc/tv.inc'    ! :IN
c-elena
      include '../inc/staznw_par.inc'    ! :parametri
*&*
      integer*4 I,J,IM,IV,GAS,OF
      character*(l_D_ERR) msgerr
	logical*2 err_zmci
!----------------------------------------------------------------------
!	Composizione % del gas in ogni elemento BINARIO
!----------------------------------------------------------------------
      err_zmci = .true.
	do j = 1, ne_bin
        do i = otipti(te_bin(j)), etipti(te_bin(j))

          im = opumto(i)
          iv = opuvto(i)
          do gas = 1, m_comp
            if (abs(porttj(i)) .le. zeroTC) then
              if (htipto(im) .eq. E_PU) then
                comptc(i,gas) = comptc(im,gas)  ! Gas del punto di monte
              else
                comptc(i,gas) = comptc(iv,gas)  ! Gas del punto di valle
              endif
            else if (porttj(i) .gt. zeroTC) then
!              comptc(i,gas) = comptc(im,gas)  ! Gas del punto di monte
              if (htipto(im) .eq. E_PU) then
                comptc(i,gas) = comptc(im,gas)  ! Gas del punto di monte
              endif
            else
!              comptc(i,gas) = comptc(iv,gas)  ! Gas del punto di valle
              if (htipto(iv) .eq. E_PU) then
                comptc(i,gas) = comptc(iv,gas)  ! Gas del punto di valle
              endif
            endif
          enddo
        end do
      enddo
!----------------------------------------------------------------------
!	Composizione % del gas in ogni elemento UNARIO
!----------------------------------------------------------------------
      do j = 1, ne_uni
        do i = otipti(te_uni(j)), etipti(te_uni(j))
crsacomp
          if (porttj(i) .lt. -zeroTC) then   !Per le portate negative impongo
		                                  ! compos del punto valle 
            iv = opuvto(i)
            do gas = 1, m_comp
              comptc(i,gas) = comptc(iv,gas)
              if (diretj(i) .eq. -1) ! per gli unari uscenti e portata negativa
	                               ! aggiorno set compos
     -            comptc_set(i,gas) = comptc(i,gas)
            enddo
          else                      !Per le portate non negative impongo set compos
            do gas = 1, m_comp
              comptc(i,gas) = comptc_set(i,gas)
            enddo
          end if
crsacomp
        end do
      enddo
!----------------------------------------------------------------------
!	caratteristiche del GAS in ogni OGGETTO
!       aggiorna i coefficienti dell'equazione di stato R.K. dipendenti
!       dalle composizioni.
!       Aggiorna il common TD
!----------------------------------------------------------------------
      do j = 1, ne_gen
        do of = otipti(te_gen(j)), etipti(te_gen(j))
          CALL par_comp_rk (of,m_ogg,comptc)
          CALL GASTD (of)
        end do
      enddo
!----------------------------------------------------------------------
!	Aggiornamento del  coefficiente di comprimibilita' (z) e
!       delle densita' in ogni PUNTO
!----------------------------------------------------------------------
      do i = otipti(e_pu), etipti(E_pu)
        call agg_punto(i)
        if (zmcitj(i).lt.zmci_sys) then
	    if (err_zmci) then
	      err_zmci = .false.
		  call gest_error(3,0,' ',' ',0)
            call tipo_err (1,err_ko,msgerr)
		  call gest_error(3,0,' ',msgerr,0)
	      msgerr = 'coefficiente di comprimibilità ' //
     *               'inferiore al valore limite '
		  call gest_error(3,0,' ',msgerr,0)
	      msgerr = 'controllare composizioni inserite ' //
     *               'che interessano i punti:'
		  call gest_error(3,0,' ',msgerr,0)
	    endif
	    msgerr = '  -  ' // snomtv(i)
	    call gest_error(3,0,' ',msgerr,0)
	  endif
      enddo
      if (.not.err_zmci) return1
!----------------------------------------------------------------------
!	Per ogni Tronco aggiorna il coefficiente di comprimibilita'
!       medio, temperatura media, viscosita' e Coeff.di Fergusson
!----------------------------------------------------------------------
      do i = otipti(e_tr), etipti(e_tr)
        call agg_tronco(i)
      enddo
cgpe
!-------------------------------------------------------------------------
!	Per ogni Centrale aggiorna i coefficienti z_coef, exp_coef e dh_coef
!-------------------------------------------------------------------------
c--->Tengo conto anche delle centrali semplificate
      do j=1,ne_cen
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
c      do i = otipti(e_ce), etipti(e_ce)
        call agg_centrale(i)
      enddo
      enddo
c-elena
cgpe-end

      return
      end ! aggvarc

      subroutine aggvart    (  !  aggiorna i parametri dipendenti 
     *                      )  !  dalla temperatura

!	Coeff. comprimibilita` in ogni PUNTO
!	Temperatura e coeff.Comprimibilita' in ogni TRONCO
*&*
      implicit none	
*&*
      include '../inc/param.inc'
c     include '../inc/TH.inc'
      include '../inc/TI.inc'
c     include '../inc/ty.inc'        ! :out
      include '../inc/tj.inc'        ! :out
      include '../inc/tz.inc'        ! :out
c     include '../inc/staznw_par.inc'    ! :parametri
*&*
      integer*4 I,J,IM,IV
      real*4    DTTRON
      logical*2 elem_bin

!----------------------------------------------------------------------
!	Coeff. comprimibilita` in ogni PUNTO
!----------------------------------------------------------------------
      do i = otipti(e_pu), etipti(e_pu)
        call agg_punto(i)
        call deroh(i,rocitj(i),temptj(i),ctvctz(i),ctpctz(i),dhrotz(i))
      enddo
!----------------------------------------------------------------------
!	Temperatura, coeff.Comprimibilita'  MEDIO e coefficienti
!	di Fergusson in ogni TRONCO
!       Ricordarsi l'ordine di chiamata: dttron + pipent
!----------------------------------------------------------------------
      do i = otipti(e_tr), etipti(e_tr)
        call agg_tronco(i)
        im = opumto(i)
        iv = opuvto(i)
        touttz(i) = dttron(i)
        call pipent(i,im,iv)
      enddo

      do j=1,ne_vlv
	 do i=otipti(te_vlv(j)),etipti(te_vlv(j))
        call dtvalv(i)
       enddo
	end do
      do j=1,ne_cen
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
            call dtcent_gen(i)
      enddo
      end do
      do j = 1, ne_uni
        do i = otipti(te_uni(j)), etipti(te_uni(j))
            call dtuno(i)
        enddo
      enddo

      return
      end   !aggvart

      subroutine initz()		! calcola temperatura media sui tronchi
					! e la loro conducibilita` termica
*&*
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/td.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tz.inc'
c-elena
      include '../inc/staznw_par.inc'    ! :parametri
*&*
      integer*4 I,J,IM,IV
      logical*2 elem_bin
*&*
      do i = otipti(e_pu), etipti(e_pu)
        call agg_punto(i)
        call deroh(i,rocitj(i),temptj(i),ctvctz(i),ctpctz(i),dhrotz(i))
      enddo

      do i = otipti(e_tr), etipti(e_tr)
        call agg_tronco(i)
        im = opumto(i)
        iv = opuvto(i)
cT        touttz(i) = temptj(i)                   ! Inizializzazione touttz
        touttz(i) = (temptj(im)+temptj(iv)) /2.0  ! Inizializzazione touttz
cT        tambtz(i) = (tambtx(im) + tambtx(iv))/2.0
        depxtz(i) = ro0td(i)*(renptx(iv) - renptx(im))
        call pipent(i,im,iv)
      enddo

      do j=1,ne_vlv
	 do i=otipti(te_vlv(j)),etipti(te_vlv(j))
        iv = opuvto(i)
        touttz(i) = temptj(iv)                  ! Inizializzazione touttz
        call dtvalv(i)
       enddo
      end do
      do j=1,ne_cen
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
            call dtcent_gen(i)
         enddo
      end do
      do j = 1, ne_uni
        do i = otipti(te_uni(j)), etipti(te_uni(j))
crsatemp
            touttz(i) = tsettj(i)
crsatemp
          if (porttj(i) .gt. zeroTC) then
crsatemp
c            touttz(i) = tsettj(i)
crsatemp
            call dtuno(i)
          end if
        enddo
      enddo

      return
      end	! initz
!---------------------------------------------------------------------------