**** 77STAZNTC.FOR **** stazionario col metodo di newton con T e C *******
****                    e con il nuovo modello di centrale di compressione ***
!-----------------------------------------------------------------------
	subroutine stazio_nc_rk (*)        !  I/O error
*&*
!  Esegue un ciclo di iterazioni comprendente la soluzione del sistema non lineare in 
!  pressioni e portate col metodo di Newton e l'aggiornamento dei valori delle
!  composizioni e delle temperature tramite soluzione di due sistemi lineari.
!  Raggiunta la convergenza, la soluzione viene salvata nel vettore di stato.
*&*   
	implicit none
      include '../inc/param.inc'
   	include '../inc/dimension.inc'
   	include '../inc/scenario.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
   	include '../inc/ti.inc'
   	include '../inc/tj.inc'
   	include '../inc/tv.inc'
   	include '../inc/nw.inc'
c-elena
   	include '../inc/staznw_par.inc'      !parametri tolleranza/convergenza
c-elena
c
      integer*4 j ,ier
	integer*4 n,n_ce,n_el,n_pun
	real*4    x,a
	integer*4 ia,ja
	integer*4 i,info
      integer*4 n_cs
	character*(l_D_ERR) msgerr
!----------------------------------------------------------------------
      logical*2 l_cor_ce           ! I) = .F. verifica della stazione
                                   !    = .T. viene corretto il punto di lavoro
     *         ,l_cons_ce          ! I) = .F. verifica del consumo
                                   !    = .T. viene corretto il consumo
     *         ,l_rec_ce           ! O) risultato della simulazione globale
								 !      .T. richiesta di recupero     
     *		 ,l_conv_ce          ! O) risultato della convergenza sul consumo
								 !      .T. convergenza raggiunta     
cgpe      integer*4 fl_fatal(max_vert) ! O) risultato della simulazione per ogni centrale

c_rsa_ce	logical*2 l_cor_ce_imp

	dimension 
     *           x(m_vard),	  ! vettore variabili incognite
     *           ia(m_vard),	  ! vettore indici riga jacobiano
     *           ja(m_jac),	  ! vettore indici colonna jacobiano
     *           a(m_jac) 	  ! vettore contenente lo jacobiano
      integer*4 icount
	logical*2 l_rec			! recupero

							!Blocchi di controllo  1-5
      integer*4 fl_blk		! determina il blocco su cui arrestare i controlli
     *         ,iel_mod		! informa su quale elemento è avvenuta la modifica


	
c_rsa_ce	logical*2 provaci_ancora
	logical*2 fl_rete_ko_old, fl_rete_ko
	logical*2 rete_ko
      external rete_ko
!----------------------------------------------------------------------

c_rsa_ce	provaci_ancora = .true.

      if (fl_nwrec.eq.0) then
	  fl_blk=0
	else
	  fl_blk=100
	endif
	
cgpe-prv
      call gest_error(3,0," ","",0)
      call gest_error(3,0," ","----------------------------",0)
      if (fl_blk .le. 0) then
        call gest_error(3,0," ","Stazionario senza recupero",0)
        fl_recupero = .false.
      else
        call gest_error(3,0," ","Stazionario con recupero",0)
        fl_recupero = .true.
      endif
      call gest_error(3,0," ","----------------------------",0)
cgpe-prv-end
*&*
!--------------------------------------------------------------------------
      fl_rete_ko_old = .true.
      lcent_last = .false.
      lcent_first=.true.
c_rsa_ce
      l_cons_ce = .true.

	if (fl_blk.ge.2) then
c_rsa_ce	  l_cor_ce_imp = .TRUE.
	  l_cor_ce = .TRUE.
	else
c_rsa_ce	  l_cor_ce_imp = .FALSE.
	  l_cor_ce = .FALSE.
	endif
! calcolo del numero di equazioni e incognite
	n_el = 0
	do i = 1, ne_gen
	  n_el = n_el + etipti(te_gen(i)) - otipti(te_gen(i)) + 1
	enddo

      n_ce = etipti(e_ce) - otipti(e_ce) + 1
c-elena
c Nella valutazione dei consumi devo tener conto anche delle centrali semplificate
c
      n_cs = etipti(e_cs) - otipti(e_cs) + 1
c-elena     
! numero di equazioni compresivo dei consumi delle centrali
	n = n_el + n_ce + n_cs
c-elena
!	Il numero di equazioni e` superiore al massimo previsto
	if(n. gt. m_vard) then
        call tipo_err (1,err_ko,msgerr)
	  call gest_error(3,0,' ',msgerr,0)
        call gest_error(1,0,'Stazio_nc_rk',
     *       'Superato numero max di equazioni',0)
        goto 100
	endif

! calcolo del numero dei nodi della rete
      n_pun = etipti(e_pu) - otipti(e_pu) + 1

cgpec gz  ---  inizializzazione valori delle nuove stazioni
cgpe      call set_initial_state (ier)   !??????????????

	l_rec = .false.

        icount=0
! inizializza le incognite in pressione e portata
	call inizpq_RK (n,x)
! inizializza i termini noti
	call inista_ssdd()



40	continue
        icount=icount+1
        if(icount.gt.max_count) then
          goto 200
        endif


	call creajac_ssdd(ia,ja,*200)
	if (lcent_first) then
         call gest_qconce(n,ia,ja,a,x,info)
      else
  	   call newton_nc_rk(n,ia,ja,a,x,info)
      endif

	if (info .gt. 3) go to 200
	
1001	continue
!   Ciclo di iterazioni per il calcolo di p e q insieme a composizioni e
!   temperature. Queste due ultime vengono calcolate a partire dalle p e q
		call s_stazio_nc_rk (n_pun,n,ia,ja,a,x,info)
	    if (info .gt. 3) go to 200

		! verifico soluzione ed eventualmente salvo
          fl_rete_ko = rete_ko()
          if (l_rec.and.
     -        fl_rete_ko.and.
     -        .not.fl_rete_ko_old) then   ! se peggioro recupero soluzione 
		                                               ! evito modifica su elemento problematico
	                                                   ! e vado avanti
             call recupera_soluzione(n,x,*110)
 	       if (iel_mod.ne.0) n_agg(iel_mod)=n_agg_max+1
	    else
		  fl_rete_ko_old = fl_rete_ko
		  l_rec = .true.
		  call SAV_RIP_NW(0,n,x)
	    endif

c_rsa_ce          l_cor_ce = l_cor_ce_imp	! ripristino controllo sulle centrali 
c_rsa_ce          l_cons_ce = .true.      ! ripristino controllo sui consumi sulle centrali

crsa_pcls
!          call agg_port_pcls_def()
crsa_pcls
		call aggstel(fl_blk,iel_mod) !verifica lo stato degli elementi speciali
crsa_pcls
!          call agg_port_pcls_cor()
crsa_pcls
c          if (iel_mod.gt.0) go to 40
          if (iel_mod.ne.0) go to 40
c	endif


5005	continue

! Simulazione centrali
		lcent_first=.false.
cgpe          call simula_nc(l_cor_ce,l_cons_ce,l_rec_ce,l_conv_ce,fl_fatal)
           call simula_nc(l_cor_ce,l_cons_ce,l_rec_ce,l_conv_ce)
c_rsa_ce
c         if (l_cor_ce) then
c            l_cor_ce = .false.  ! controllo già effettuato
c            if (l_rec_ce) goto 40
c          endif
c	      if (.not.l_cons_ce) provaci_ancora = .false.
c            l_cons_ce = .false.  ! controllo già effettuato
c            if (.not.l_conv_ce) goto 40
c          endif
          if ((l_cor_ce.and.l_rec_ce) .or.
     -        (l_cons_ce.and..not.l_conv_ce)) goto 40
c_rsa_ce


      call agg_tc(*110)
cgpe-prv	call savetz()
      l_cor_ce  = .false.
      l_cons_ce = .false.
      lcent_last = .true.
      call simula_nc(l_cor_ce,l_cons_ce,l_rec_ce,l_conv_ce)
crsa_pcls
      call agg_pcls()
!      call agg_port_pcls_def()
crsa_pcls

      call verstel_nw()

crsa_pcls
!      call agg_port_pcls_cor()
      call agg_prel()
crsa_pcls

      call agg_setdaris()

	return

200	continue   ! recupero soluzione precedente se esiste
	if(l_rec) then
        call recupera_soluzione(n,x,*110)
	  fl_blk=-1                   !No recupero su rete
c_rsa_ce	  l_cor_ce_imp = . false.
c_rsa_ce        l_cor_ce = l_cor_ce_imp
c_rsa_ce          provaci_ancora = .false.
	  l_cor_ce = . false.         !No recupero su centrali
        if (.not.lcent_first) then     !se ho già eseguito un controllo centrali
	                                 !significa che la soluzione è definitiva
	                                 !per cui elimino anche:
	    l_rec = .false.              !  recupero soluzione 
          l_cons_ce = .false.          !  controllo consumi
        endif
	  icount = 0               ! Viene data la possibilità di ricominciare le prove 
                                 ! (nelle condizioni sopra definite)
	  goto 5005      


	else
		call gest_error(3,0,' ',' ',0)
          call tipo_err (1,err_ko,msgerr)
		call gest_error(3,0,' ',msgerr,0)
		if     (info .eq. 4)then
			call gest_error(3,0,' ',
     *' Superato numero massimo di iterazioni nel Newton',0)
		elseif (info .eq. 5)then
			call gest_error(3,0,' ',
     *' Errore nella soluzione del sistema jacobiano',0)
		elseif (info .eq. 6)then
			call gest_error(3,0,' ',
     *' Troppe iterazioni non convergenti nel Newton',0)
		elseif (info .eq. 7)then
			call gest_error(3,0,' ',
     *' Iterazioni che divergono nel Newton',0)
		elseif (info .eq. 8)then
			call gest_error(3,0,' ',
     *'La tolleranza richiesta e'' eccessiva',0)
		elseif (info .eq. 9)then
			call gest_error(3,0,' ',
     *' Elementi linearmente dipendenti',0)
		elseif (info .eq. 101)then
			call gest_error(3,0,' ',
     *' Iterazioni che divergono nel calcolo di Comp. e Temp.',0)
		elseif (info .eq. 102)then
			call gest_error(3,0,' ',
     *' Superato numero max di iterazioni nel calcolo di Comp. e Temp.'
     *                                                      ,0)
		elseif (info .eq. 103)then
			call gest_error(3,0,' ',
     *' Errore in aggiornamento delle Comp. e Temp.',0)
		elseif (info .eq. 104)then
			call gest_error(3,0,' ',
     *' Superato numero max di iterazioni nel calcolo del PCS',0)
		elseif (info .eq. 105)then
			call gest_error(3,0,' ',
     *' Iterazioni che divergono nel calcolo del consumo centrale',0)
		else
			continue
		endif
		if (i_elem_ko.gt.0) then
	        call gest_error(3,0,' ',
     *' Controllare l''elemento ' //
     *trim(te_stri(htipto(i_elem_ko)))//' : '//
     *trim(snomtv(i_elem_ko)),0)
		endif
	endif

100	continue
	return  1
110   continue
	return  1
120   continue
	return  1
	end
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
	subroutine s_stazio_nc_rk (n_pun,n,ia,ja,a,x,info)  ! I/O error
*&*
!  Esegue un ciclo di iterazioni comprendente la soluzione del sistema non lineare
!  in pressioni e portate col metodo di Newton e l'aggiornamento dei valori delle
!  composizioni e delle temperature tramite soluzione di due sistemi lineari.
*&*   
	implicit none
      include '../inc/param.inc'
   	include '../inc/stazione.inc'
   	include '../inc/dimension.inc'
	include '../inc/STATIONS.INC'
   	include '../inc/ti.inc'
   	include '../inc/tj.inc'
   	include '../inc/jc.inc'
   	include '../inc/ty.inc'
      include '../inc/nw.inc'
      include '../inc/staznw_par.inc'
	integer*4 n_pun,n,ia,ja,info
	real*4    a,x

	integer*4 n_iter,n_conv,n_conv_pcls,jp
      real*4    pres1,difpre,difpre1
      real*4    pmax
      logical*2 l_conv_pcls
c *********** endGZ *********************
	dimension 
     *           x(m_vard),	  ! vettore variabili incognite
     *           ia(m_vard),	  ! vettore indici riga jacobiano
     *           ja(m_jac),	  ! vettore indici colonna jacobiano
     *           a(m_jac),	  ! vettore contenente lo jacobiano
     *           pres1(m_vard)  ! vettore save delle pressioni tra le iterazioni
*&*
!--------------------------------------------------------------------------
 


10	continue

!   Inizializza  flag e variabili usate per controllare la convergenza
      info = 0
	n_iter = 0     ! numero iterazioni
      n_conv = 0     ! numero iterazioni divergenti
      n_conv_pcls = 0     ! numero iterazioni divergenti PCS
	difpre = zero  ! max diff tra i valori (al quadrato) delle p tra due iteraz successive

!   Ciclo di iterazioni per il calcolo di p e q insieme a composizioni e
!   temperature. Queste due ultime vengono calcolate a partire dalle p e q
!   ottenute dalla routine Newton, che viene ancora chiamata per ricalcolare
!   p e q in funzione dei nuovi valori di T e Composizioni, e cosi via.
!   Si esce dal ciclo se la differenza tra i valori di P tra due successive
!   iterazioni e' minore di p_tol, oppure se le iterazioni divergono o
!   superano un numero massimo max_iter.

50	continue

!   Salva i vecchi valori di pressioni e differenze tra iterazioni
	difpre1 = difpre
	difpre = zero
	pmax = zero

	do jp = otipti(e_pu), etipti(e_pu)
	  pres1(jp) = x(nw_topvar(jp))
	enddo

!   Ricalcola composizioni, temperature, P e Q:
!   aggiornamento delle variabili dipendenti dalle temperature e dalle composizioni
      call agg_tc(*222)  !  setta errore calcolo temp e comp

cpcls      call agg_pcls()
cpcls call agg_pcls_conv(l_conv_pcls)


cgpe!   Aggiorna i coeff per il calcolo dello jacobiano
cgpe questo aggiornamento viene fatto tramite la routine aggvars
cgpe      call aggjac_nc(ia,ja)

!   Risolve nuovamente il sistema di equazioni non lineari con Newton
  	call newton_nc_rk(n,ia,ja,a,x,info)

      if(info .gt. 3) goto 100

!   Calcola la differenza tra le iterazioni e controlla se si e' raggiunta
!   la precisione desiderata



	do jp = otipti(e_pu), etipti(e_pu)
        difpre = max(difpre,abs(pres1(jp)-x(nw_topvar(jp))))
        pmax = max(pmax,abs(x(nw_topvar(jp))))

      enddo

      if( difpre .le. TOLCwd*pmax) then

	  n_iter = 0
        n_conv_pcls = n_conv_pcls + 1
        if(n_conv_pcls .gt. MIT_pcls) then

       if (TOLCwd.ge.TOLCwd_max) then

	    info = 104
          goto 100
	else   ! provo ad allargare la tolleranza

           TOLCwd = TOLCwd*10
	    toll_pcls = TOLCwd

	call mess_param(2)
	    goto 10


       endif
       endif



        call agg_pcls_conv(l_conv_pcls)

        if (l_conv_pcls) then



	    n_conv_pcls = 0
          go to 1111
        else
  	   call newton_nc_rk(n,ia,ja,a,x,info)
         if(info .gt. 3) goto 100
        endif
      endif


!   Controlla se le iterazioni divergono o se viene superato il
!   numero massimo
      n_iter = n_iter + 1
      n_conv = n_conv + 1

      if(difpre .lt. difpre1) n_conv = 0
	    


      if(n_conv .gt. m_it_nconv) then
	  info = 101
        goto 100
      endif

      if((n_iter .gt. MITCwd).OR.(n_conv_pcls .gt. MIT_pcls)) then
        if (TOLCwd.ge.TOLCwd_max) then
	    info = 102
          goto 100
	  else   ! provo ad allargare la tolleranza
	    TOLCwd = TOLCwd*10
	    toll_pcls = TOLCwd
          call mess_param(2)
	    goto 10
	  endif
      endif

      go to 50

!   Fine del ciclo di iterazioni

1111	continue

100	continue

	return

222   continue
      info = 103



	goto 100

	end

!-----------------------------------------------------------------------
**** STAZNTC.FOR **** stazionario col metodo di newton con T e C *******

!======================================================================

!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE SAV_RIP_NW (fl_op         ! Salva o Ripristina valori 
     -                       ,n_eq,x)      ! per Newton

      implicit none
*&*
      include '../inc/param.inc '      !: costante
   	include '../inc/dimension.inc'
      include '../inc/TI.inc'
      include '../inc/TY.inc'
      include '../inc/TJ.inc'
   	include '../inc/nw.inc'
   	include '../inc/jc.inc'
*&*

	integer*4 fl_op	! flag operazione : 0 = salva
					!                   1 = ripristina
	integer*4 n_eq	! numero equazioni
	real*4 x(*)		! soluzione
********
      integer*4 i_elem,oicen,j
      integer*4 vv
	
	real*4 save_x(m_vard)
c	real*4 save_val_tn(m_ogg+max_ce)
c-elena---->tengo conto anche delle e_cs
	real*4 save_val_tn(m_ogg+max_ce+max_cs)
	integer save_st(m_ogg)
	save save_x,save_val_tn,save_st

c     integer*4 j
!-------------------------------------------------------
	if (fl_op .eq. 0) then
        do vv = 1, ne_gen
          do i_elem = otipti(te_gen(vv)), etipti(te_gen(vv))
		  save_st(i_elem) = hstatj(i_elem)
		  save_val_tn(i_elem) = valsjc(i_elem)
          enddo
        enddo
        do j=1,ne_cen
          if (te_cen(j).eq.e_ce) then
            oicen=oice
          else
            oicen=oics
          end if
	    do i_elem=otipti(te_cen(j)),etipti(te_cen(j))
c	     oicen=oice
c          do i_elem = otipti(e_ce), etipti(e_ce)
             save_val_tn(i_elem-oicen) = valsjc(i_elem-oicen)
          enddo
	  end do
	  do vv = 1, n_eq
		save_x(vv) = x(vv)
	  enddo
	
	else
        do vv = 1, ne_gen
          do i_elem = otipti(te_gen(vv)), etipti(te_gen(vv))
		  hstatj(i_elem) = save_st(i_elem)
		  valsjc(i_elem) = save_val_tn(i_elem)
          enddo
        enddo
        do j=1,ne_cen
          if (te_cen(j).eq.e_ce) then
            oicen=oice
          else
            oicen=oics
          end if
	    do i_elem=otipti(te_cen(j)),etipti(te_cen(j))
c	  oicen=oice
c        do i_elem = otipti(e_ce), etipti(e_ce)
            valsjc(i_elem-oicen) = save_val_tn(i_elem-oicen)
          enddo
	  end do
	  do vv = 1, n_eq
		x(vv) = save_x(vv)
	  enddo
	  call aggvars_ssdd(x)
	endif
      return
      end
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE recupera_soluzione (n,x,*)      ! recupera soluzione salvata

      implicit none
*&*
      include '../inc/param.inc '      !: costante
   	include '../inc/dimension.inc'
*&*

	integer*4 n	! numero equazioni
	real*4 x(*)		! soluzione
********
	
!-------------------------------------------------------

      call SAV_RIP_NW(1,n,x)
      call agg_tc(*110)
      call agg_pcls()
      return
110   continue
      return1
      end
!----------------------------------------------------------------------


      SUBROUTINE agg_pcls_conv_orig (l_conv_pcls)      ! aggiorna pcs 

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TH.inc'
      include '../inc/TD.inc'
   	include '../inc/jc.inc'
   	include '../inc/staznw_par.inc'
*&*

	logical*2 l_conv_pcls	! convergenza dei pcs : .T. = OK
					!                               .F. = KO
********
      integer*4 i_elem
      integer*4 vv
	
	real*4 pcls_app
	real*4 pcls_real
      external pcls_real

!-------------------------------------------------------
      l_conv_pcls = .true.
      do vv = 1, ne_spe
        do i_elem = otipti(te_spe(vv)), etipti(te_spe(vv))
          if (hstatj(i_elem) .eq. sta_q .and.
     *        abs(valsjc(i_elem)).gt.zeroTC) then
            pcls_app = pcls_real (i_elem)
            if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *             toll_pcls) then
              l_conv_pcls = .false.

      

cmar_pcs 
             valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app
              pclstd(i_elem) = pcls_app
            endif
          endif
        end do
      enddo

      do i_elem = otipti(e_pu), etipti(e_pu)
	 if (abs(valsjc(i_elem)).gt.zeroTC) then
        pcls_app = pcls_real (i_elem)
        if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *         toll_pcls) then
          l_conv_pcls = .false.
      
cmar_pcs 
          valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app
          pclstd(i_elem) = pcls_app
        endif
       endif
      enddo

      return
      end
!----------------------------------------------------------------------
!----------------------------------------------------------------------
c**********************************************************************



      SUBROUTINE agg_pcls_conv (l_conv_pcls)      ! aggiorna pcs 

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TH.inc'
      include '../inc/TD.inc'
   	include '../inc/jc.inc'
   	include '../inc/staznw_par.inc'
cmar_pcs
ccc      include  '../inc/FLAG_CONV.INC'
cmar_pcs
*&*

	logical*2 l_conv_pcls	! convergenza dei pcs : .T. = OK
					!                               .F. = KO
********
      integer*4 i_elem
      integer*4 vv
	
	real*4 pcls_app
	real*4 pcls_real
      external pcls_real

!-------------------------------------------------------
ccc      FLAG_CONVERGENZA=1
      l_conv_pcls = .true.
      do vv = 1, ne_spe
        do i_elem = otipti(te_spe(vv)), etipti(te_spe(vv))
          if (hstatj(i_elem) .eq. sta_q .and.
     *        abs(valsjc(i_elem)).gt.zeroTC) then
            pcls_app = pcls_real (i_elem)
            if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *             toll_pcls) then
              l_conv_pcls = .false.

cmar_pcs     
               IF (FLAG_CONVERGENZA.EQ.1)THEN
           
	    valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

          ELSEIF (FLAG_CONVERGENZA.EQ.2)THEN


	    valsjc(i_elem) = valsjc(i_elem)


	    ELSEIF (FLAG_CONVERGENZA.EQ.3)THEN
          
	          if (diretj(i_elem).lt.0)then

                valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

                else
                valsjc(i_elem) = valsjc(i_elem)    
                endif

	    ELSEIF (FLAG_CONVERGENZA.EQ.4)THEN

	          if (diretj(i_elem).lt.0)then

                valsjc(i_elem) = valsjc(i_elem)

                else
                valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

                endif


	ENDIF


cmar_pcs             valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app



              pclstd(i_elem) = pcls_app
            endif
          endif
        end do
      enddo







      do i_elem = otipti(e_pu), etipti(e_pu)
	 if (abs(valsjc(i_elem)).gt.zeroTC) then
        pcls_app = pcls_real (i_elem)
        if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *         toll_pcls) then
          l_conv_pcls = .false.
cmar_pcs     
               IF (FLAG_CONVERGENZA.EQ.1)THEN
           
	    valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

          ELSEIF (FLAG_CONVERGENZA.EQ.2)THEN


	    valsjc(i_elem) = valsjc(i_elem)


	    ELSEIF (FLAG_CONVERGENZA.EQ.3)THEN
          
c	          if (diretj(i_elem).lt.0)then

                valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

c                else
c                valsjc(i_elem) = valsjc(i_elem)    
c                endif

	    ELSEIF (FLAG_CONVERGENZA.EQ.4)THEN

c	          if (diretj(i_elem).lt.0)then

                valsjc(i_elem) = valsjc(i_elem)

c                else
c                valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app

c                endif


	ENDIF

 
cmar_pcs             valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app
          pclstd(i_elem) = pcls_app
        endif
       endif
      enddo
     



      return
      end


c**********************************************************************
      SUBROUTINE agg_pcls_conv_ (l_conv_pcls)      ! aggiorna pcs 

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TH.inc'
      include '../inc/TD.inc'
   	include '../inc/jc.inc'
   	include '../inc/staznw_par.inc'
*&*

	logical*2 l_conv_pcls	! convergenza dei pcs : .T. = OK
					!                               .F. = KO
********
      integer*4 i_elem
      integer*4 vv
	
	real*4 pcls_app
	real*4 pcls_real
      external pcls_real

!-------------------------------------------------------

      l_conv_pcls = .true.
      do vv = 1, ne_spe

        do i_elem = otipti(te_spe(vv)), etipti(te_spe(vv))


          if (hstatj(i_elem) .eq. sta_q .and.
     *        abs(valsjc(i_elem)).gt.zeroTC) then


            pcls_app = pcls_real (i_elem)

            if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *             toll_pcls) then

		  

              l_conv_pcls = .false.

   

              valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app
              pclstd(i_elem) = pcls_app
            endif
          endif
        end do
      enddo

      do i_elem = otipti(e_pu), etipti(e_pu)
	
	



	 if (abs(valsjc(i_elem)).gt.zeroTC) then

       pcls_app = pcls_real (i_elem)

        if (abs((pcls_app - pclstd(i_elem))/pclstd(i_elem)) .gt.
     *         toll_pcls) then

          l_conv_pcls = .false.
	



          
          valsjc(i_elem) = valsjc(i_elem)*pclstd(i_elem)/pcls_app
          pclstd(i_elem) = pcls_app
        endif
       endif
      enddo

      close(11)


      return
      end
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE gest_qconce(n,ia,ja,a,x,info)     ! converge su conce 

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TH.inc'
      include '../inc/TD.inc'
   	include '../inc/jc.inc'
   	include '../inc/staznw_par.inc'
*&*

      integer*4 n,ia,ja,INFO
      real*4    x,a
	dimension x(*),
	1          ia(*),
	1          ja(*),
	1          a(*)
********
	logical*2 l_conv_conce	! convergenza dei conce : .T. = OK
					!                               .F. = KO
      integer*4 n_conv_conce
********

!-------------------------------------------------------
        n_conv_conce = 0
40    continue
        n_conv_conce = n_conv_conce + 1
        if(n_conv_conce .gt. MIT_conce) then
	    info = 105
          goto 100
        endif
  	  call newton_nc_rk(n,ia,ja,a,x,info)
        if(info .gt. 3) goto 100
        call agg_conce_conv (l_conv_conce)
        if (.not.l_conv_conce) goto 40

100   continue
      return
      end
!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE agg_conce_conv (l_conv_conce)      ! aggiorna conce 

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/Ty.inc'
      include '../inc/TJ.inc'
      include '../inc/Jc.inc'
c      include '../inc/TH.inc'
c      include '../inc/TD.inc'
c   	include '../inc/jc.inc'
   	include '../inc/staznw_par.inc'
   	include '../inc/nw.inc'
*&*

	logical*2 l_conv_conce	! convergenza dei conce : .T. = OK
				   	  !                               .F. = KO
********
      integer*4 i_elem
      integer*4 vv

	real*4 consumo,dq,dpa2,dpv2
	

!-------------------------------------------------------
      l_conv_conce = .true.
      do vv = 1, ne_cen
        do i_elem = otipti(te_cen(vv)), etipti(te_cen(vv))
           call qconce(porttj(i_elem),
     -            prestj(opumto(i_elem)),prestj(opuvto(i_elem)),
     -            i_elem,consumo,dq,dpa2,dpv2)
            if (abs(consumo - consty(i_elem)) .gt.
     *             toll_conce) then
              l_conv_conce = .false.
              if (te_cen(vv).eq.e_ce) then
                 valsjc(i_elem-oice) = consumo
              else
                 valsjc(i_elem-oics) = consumo
              end if
c              consty(i_elem) = consumo
            endif
        end do
      enddo

      return
      end
!----------------------------------------------------------------------
      SUBROUTINE agg_setdaris ()      ! aggiorna setta da risultati stazionario
									! prima della scrittura per la dinamica

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/default.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TH.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
      include '../inc/jc.inc'       !:out
*&*

********
      integer*4 i_type, i_elem
	
      logical*2 elem_vlv,elem_uni
      external  elem_vlv,elem_uni
!-------------------------------------------------------
      do i_type = 1, ne_spe
        do i_elem = otipti(te_spe(i_type)), etipti(te_spe(i_type))
	     if (hstatj(i_elem).eq.STA_OFF) then
	       htcitj(i_elem) = TC_OFF
	     elseif (hstatj(i_elem).eq.STA_ON) then
	       if(.not.(elem_vlv(i_elem).and.
     -          htcitj(i_elem).eq.tc_npv))
     -        htcitj(i_elem) = TC_NR
	     elseif (hstatj(i_elem).eq.STA_FR) then
	       if(.not.(elem_vlv(i_elem).and.
     -          htcitj(i_elem).eq.tc_pv))
     -	    htcitj(i_elem) = TC_ON
	     elseif (hstatj(i_elem).eq.STA_Q) then
             if (elem_uni(i_elem)) then    ! Come qset_real
	         qsettj(i_elem) = porttj(i_elem)*pclstd(i_elem)/pcls_def
	       else
	         qsettj(i_elem) = porttj(i_elem)
             endif
	       htcitj(i_elem) = TC_Q
	     elseif (hstatj(i_elem).eq.STA_PV) then
	       pvsetj(i_elem) = prestj(opuvto(i_elem))
	       if(htcitj(i_elem).ne.tc_pq) htcitj(i_elem) = TC_PV
	     elseif (hstatj(i_elem).eq.STA_PM) then
	       pmsetj(i_elem) = prestj(opumto(i_elem))
	       if(htcitj(i_elem).ne.tc_pq) htcitj(i_elem) = TC_PM
	     else
	       continue
	     endif

        end do
      end do

      return
      end
