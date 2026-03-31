*** LIB_OTC_SERIE.FOR *** gestione della centrale in serie ********************
! Moduli per la gestione delle centrali in serie di:
! - Cap Bon
! - Cortemaggiore (servizio di Booster)
!
! L'intervento si snoda seguendo i seguenti punti:
! - riconoscimento della configurazione serie: esistenza della seconda colonna
!   di puntatori
! - modulo per la simulazione:
!   - la prima fase e' propedeutica alla seconda fase
!   - azzeramento della funzione consumo (differenza fra il massimo consumo
!     della prima fase e il massimo consumo della seconda fase)
! - modulo per l'ottimizzazione:
!   - ricerca di tutte le possibili configurazioni da provare
!   - simulazione di ciascuna configurazione nel rispetto del criterio di UC
!   - scelta della configurazione con associato il consumo minimo nel rispetto
!     del criterio di UC fra le due fasi
!   
! NOta. Criterio di uguaglianza di consumo tra le due fasi:
!      determinazione della pressione intermedia in modo da annullare la
!      differenza tra il massimo consumo della prima fase e il massimo
!      consumo della seconda fase
*******************************************************************************

      Subroutine simula_cent_serie (istaz_fase1,jstaz_fase2,ivert_fase1,
     *           jvert_fase2,cons_tot,pint,ier)
C
C   Determina la pressione intermedia nel rispetto del criterio di
C   uguaglianza di consumo tra le due fasi.
C   Sono note:
C   - pressione di aspirazione della prima fase e di mandata della seconda
C   - portata di entrambe le fasi (coincidono)
C   - temperatura di aspirazione della prima fase e la temperatura
C     dell'aria di entrambe le fasi
C   - stato delle unita' di entrambe le fasi (una unita' non puo' essere
C     dichiarata accesa in entrambe le fasi)
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/UNITS.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
      include '../inc/messag.inc'
      include '../inc/cap_bon_ind.inc'
cgpe      include '../inc/cap_bon_limiti.inc'
      include '../inc/cap_bon.inc'

      INTEGER*4 istaz_fase1,jstaz_fase2    ! I) indice centrale [otipti(e_ce),etipti(e_ce)]
      INTEGER*4 ivert_fase1,jvert_fase2    ! I) risp. pgrato(istaz),pgrato_app(jstaz)
      real*4    cons_tot
      INTEGER*4 ier
      real*4    pint

      real*4    pmin,pmax,delta_cons
      real*4    fa,fb,ra,rb,rd
c-din-corr      real*4    eps_cons/1.e-3/
      real*4    eps_cons/5.e-2/
c-din-corr-end
      real*4    zero_cons
      EXTERNAL  zero_cons
c-------------------------------------------------------------------------
      ier = 0

!-via common sono note la pressione minima e massima fattibile dalla
!-configurazione corrente
      pmin = pint_min
      pmax = pint_max

!-inizializzazione area common CAP_BON_IND.INC
      istaz = istaz_fase1
      jstaz = jstaz_fase2
      ivert = ivert_fase1
      jvert = jvert_fase2

c-bpcorr
c      if (.not.iopt_last) then
c-bpcorr-end
        iopt = .true.
        pint = R_UND
C determinazione dello zero della funzione
        fa = zero_cons(pmin)
        fb = zero_cons(pmax)
        if (abs(fa) .le. eps_cons) then
          pint = pmin
        elseif (abs(fb) .le. eps_cons) then
          pint = pmax
        elseif ((fa*fb) .lt. 0) then
          ra = pmin
          rb = pmax
          rd = (pmin+pmax)/2.
!        call zero_fun_cons(ra,rb,rd,fa,fb,pint,eps_flow,eps_cons,
!     *       zero_cons)
          call zero_fun1 (ra,rb,rd,fa,fb,pint,epsma,pmach,zero_cons)
        else
          if ((fa*fb) .gt. 0) then
            if (abs(fa) .le. abs(fb)) then
              pint = pmin
            else
              pint = pmax
            endif
          endif
        endif
c-bpcorr
c      endif !.not.iopt_last
c-bpcorr-end
c
      if (pint .ne. R_UND) then
        if (pint .ge. pint_min .and. pint .le. pint_max) then
          ier = 0
          call ini_centrale (ivert,jvert)
c-bpcorr
          if (iopt_last) iopt_mess = .false.
c-bpcorr-end
          delta_cons = zero_cons (pint)
          if (abs(delta_cons) .le. eps_cons) then
cgpe-corr-new
cgpe-corr-new            cons_tot = tot_cons(ivert) + tot_cons(jvert)
            if (err(ivert).eq.0 .and. err(jvert).eq.0) then
              cons_tot = tot_cons(ivert) + tot_cons(jvert)
            else
cgpe dare un nuovo indice di errore
              ier = 64
            endif
cgpe-corr-new-end
          else
            ier = 64 ! ier = 12
          endif
        else
          ier = 63 ! ier = 11
        endif
      else
        ier = 63 ! ier = 11
      endif
c
cgpe-new
      if (ier .ne. 0) then
        err(ivert) = ier
        err(jvert) = ier
      endif
cgpe-new-end
c
      return ! simula_cent_serie
      end

      Subroutine calc_ottimo_serie (istaz,jstaz,ivert,jvert,ier)
c
C   Determina lo stato delle unita' di ciascuna fase e la pressione
C   intermedia nel rispetto del criterio di uguaglianza di consumo
C   tra le due fasi in modo tale da minimizzare il consumo.
C   Sono note:
C   - pressione di aspirazione della prima fase e di mandata della seconda
C   - portata di entrambe le fasi (coincidono)
C   - temperatura di aspirazione della prima fase e la temperatura
C     dell'aria di entrambe le fasi
C   - la disponibilita' delle unita' di entrambe le fasi (una unita' non
C     puo' essere dichiarata disponibile o necessariamente accesa in
C     entrambe le fasi)
C
C   Schema della routine:
C   - salvataggio dello stato delle unita'
C   - inizializzazione delle variabili necessarie per il calcolo delle
C     combinazioni da provare (subroutine set_tipi)
C   - determinazione del vettore con le possibili combinazioni di stati
C     delle unita' sulla prima e sulla seconda fase (vettore di interi
C     istatus)
C   - calcolo della configurazione ottima:
C     per ciascuna configurazione sulla prima e sulla seconda fase, calcola
C     la pressione intermedia nel rispetto del criterio di UC tra le due
C     fasi e in modo da minimizzare il consumo (a cui vengono aggiunti
C     eventuali costi di accensione e spegnimento)
C   - simulazione della configurazione ottima (a pressione intermedia e
C     stato delle unita' determinate precedentemente)
C   - ripristino lo stato iniziale delle unita'
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/flag.inc'
      include '../inc/UNITS.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
      include '../inc/cap_bon.inc'
      include '../inc/simpstat_aux.inc'
c
      INTEGER*4 istaz,jstaz    ! I) indice centrale [otipti(e_ce),etipti(e_ce)]
      INTEGER*4 ivert,jvert    ! I) risp. pgrato(istaz),pgrato_app(jstaz)
      INTEGER*4 ier            ! O) indice di errore

      INTEGER*4 jtest,ntest
      INTEGER*4 unit_sim,iu,ju,i
      INTEGER*4 istatus(maxunits*max_ntest)
      character*3 status_app1(maxunits),status_app2(maxunits)

! memorizzazione dei puntatori alla configurazione ottima
      INTEGER*4 jtest_min
cgpe      real*4    cons_tot_min,pint_min_save,diff_cons
      real*4    pint_min_save,diff_cons
      real*4    pint,cons_tot
      REAL*4    pmin_save,pmax_save
      character*(tit_len) msg_err
c-------------------------------------------------------------------------
      msg_err = ' '
c-bpcorr
      iopt_last = .false.
c-bpcorr-end
      unit_sim = unit_num(ivert)
      iu = first_unit(ivert)
      ju = first_unit(jvert)

!-salvataggio dello stato delle unita'
      do i = 1,unit_sim
        status_app1(i) = status(iu+i-1)
        status_app2(i) = status(ju+i-1)
      enddo

!-inizializza le variabili necessarie per il calcolo delle combinazioni
ccorr-din
      do i = 1, maxunits*max_ntest
	 istatus(i) = 0
      enddo
ccorr-din-end
cgpe-corr
cgpe-corr      if (jstaz .ne. I_UND) then
cgpe-corr        call init_loc_var_s (istaz,jstaz,ivert,jvert)
cgpe-corr      else
cgpe-corr        call init_loc_var_s (istaz,istaz,ivert,jvert)
cgpe-corr      endif
      if (jstaz .ne. I_UND) then
        call init_loc_var_s (istaz,jstaz,ivert,jvert)
      else
        call init_loc_var_s (istaz,istaz,ivert,jvert)
      endif
cgpe-corr-end

C Determinazione del vettore con le possibili combinazioni di stati delle
C unita' sulla prima e sulla seconda fase (vettore di interi istatus)
      call set_tipi (ivert,jvert,istatus,ntest)

! viene settata l'area common FLAG_CORR
      num_test_tot = ntest

!-inizializza il flag per non visualizzare i messaggi
      iopt_mess = .true.

C calcolo della configurazione ottima
      jtest_min = I_UND
cgpe      cons_tot_min = R_MAX
      diff_cons = R_MAX
      do jtest = 1, ntest
! viene settato lo stato delle unita' della prima e della seconda fase
          do i = 1,unit_sim
            status(iu+i-1) = funz_off
            status(ju+i-1) = funz_off
            if (istatus((unit_sim*(jtest-1))+i) .eq. fase_1) then
              status(iu+i-1) = funz_on
            elseif (istatus((unit_sim*(jtest-1))+i) .eq. fase_2) then
              status(ju+i-1) = funz_on
            endif
          enddo
!          fl_exit = .true.
!- gpe inizio
! viene settata l'area common FLAG_CORR
          num_test = jtest
!- gpe fine
          call calc_int (istaz,ivert,jstaz,jvert,ier)
!          call calc_serie_limiti (istaz,ivert,jstaz,jvert,ier)
!          fl_exit = .false.
          if (ier.eq. 0) then
           fl_opt = .true.
           call simula_cent_serie (istaz,jstaz,ivert,jvert,cons_tot,
     *          pint,ier)
           if (ier .eq. 0) then
!-aggiungo gli eventuali costi di accensione e/o spegnimento
             call add_cost (unit_num,status(iu),status(ju),status_app1,
     *status_app2,unit_start_cost(iu),unit_shut_cost(iu),
     *unit_start_cost(ju),unit_shut_cost(ju),cons_tot)

!-confronto con lo stato ottimo "precedente"
cgpe            if (cons_tot .lt. cons_tot_min) then
c gestione della scelta della soluzione come minimo del consumo o minimo della differenza
c del consumo con il consumo da rispettare
            if (abs(cons_tot-cons_imp) .lt. diff_cons) then
              jtest_min = jtest
              pint_min_save = pint
              diff_cons = abs(cons_tot-cons_imp)
cgpe              cons_tot_min = cons_tot
cgpe
              pmin_save = pint_min
			pmax_save = pint_max
cgpe-end
            endif
           endif
          endif
      enddo
c
!-Simulazione della configurazione ottima
cgpe      if (jtest_min .ne. I_UND .and. cons_tot_min .ne. R_MAX) then
      if (jtest_min .ne. I_UND) then
! viene settato lo stato delle unita' della prima e della seconda fase per
! la simulazione della configurazione che realizza il minimo consumo
        do i = 1,unit_sim
          if (istatus((unit_sim*(jtest_min-1))+i) .eq. fase_1) then
            status(iu+i-1) = funz_on
          elseif (istatus((unit_sim*(jtest_min-1))+i) .eq. fase_2) then
            status(ju+i-1) = funz_on
          else
            status(iu+i-1) = funz_off
            status(ju+i-1) = funz_off
          endif
        enddo
        call ini_centrale (ivert,jvert)

!-inizializza il flag per non visualizzare i messaggi
        iopt_mess = .true.
c-bpcorr
        iopt_last = .true.
c-bpcorr        call simula_cent_serie (istaz, jstaz,ivert,jvert,cons_tot,
c-bpcorr     *       pint,ier)
cgpe
        pint_min = pmin_save
        pint_max = pmax_save
cgpe-end
        call simula_cent_serie (istaz, jstaz,ivert,jvert,cons_tot,
     *       pint_min_save,ier)
c-bpcorr-end
      else
!-non esiste punto di lavoro ottimo
        ier = 29
!-18/05/2000
        if (.not. fl_sim) return
!-18/05/2000-end
cgpe-new
        if (iopt.or.iopt_mess) return
cgpe-new-end
cgpe-mess        call app_mess(ivert,ier,msg_err)
cgpe-mess        call app_mess(jvert,ier,msg_err)

        ier = 1 ! ier = 11
cgpe-new        if (fl_sim) then
cgpe-new          call gest_error (2,0,'OTTIMO_CENT_S',
cgpe-new     *           'Non esiste punto ottimo',0)
cgpe-new        endif
      endif
c
!-ripristino lo stato iniziale delle unita'
      do i = 1,unit_sim
        status(iu+i-1) = status_app1(i)
        status(ju+i-1) = status_app2(i)
      enddo

      return ! calc_ottimo_serie
      end

      Subroutine add_cost (unit_num,status_fase1,status_fase2,
     *           status_app_fase1,status_app_fase2,
     *           start_cost_fase1,shut_cost_fase1,
     *           start_cost_fase2,shut_cost_fase2,tot_cons)
C
C   Aggiunge l'eventuale costo di accensione e spegnimento solo nel caso
C   in cui opt_time e' diverso da zero 
C
      implicit none
      include '../inc/param.inc'
      include '../inc/otc_time.inc'
C
      INTEGER*4   unit_num             ! I) numero di unita'
      REAL*4      start_cost_fase1(*), ! I) costo accensione unita' I fase
     *            shut_cost_fase1(*),  ! I) costo spegnimento unita' I fase
     *            start_cost_fase2(*), ! I) costo accensione unita' II fase
     *            shut_cost_fase2(*)   ! I) costo spegnimento unita' II fase
      character*3 status_app_fase1(*), ! I) stato unita' attuale configurazione
     *            status_app_fase2(*), ! I) stato unita' attuale configurazione
     *            status_fase1(*),     ! I) stato unita' da CIN
     *            status_fase2(*)      ! I) stato unita' da CIN
      REAL*4      tot_cons             ! O) consumo totale compreso eventuali
                                       !     costi di accensione / spegnimento

      INTEGER*4   i
c----------------------------------------------------------------------
      if (opt_time .ne. 0) then
        do i = 1, unit_num
          call calc_cons_tot (status_fase1(i),status_app_fase1(i),
     *start_cost_fase1(i),shut_cost_fase1(i),opt_time,tot_cons)
          call calc_cons_tot (status_fase2(i),status_app_fase2(i),
     *start_cost_fase2(i),shut_cost_fase2(i),opt_time,tot_cons)

        end do
      end if
c
      return
      end

      Subroutine calc_max_cons (ivert,cons_max)
C
C   Calcola il consumo massimo delle unita' attive di una fase
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/UNITS.INC'
      include '../inc/stations.inc'
c-mf03
      include '../inc/flag.inc'
c-mf03-end

      INTEGER*4 ivert        ! I) pgrato(istaz)
      real*4    cons_max     ! O) massimo consumo tra le unita' della fase

      INTEGER*4 iu,i
c-------------------------------------------------------------------------
      cons_max = -R_MAX
      iu = first_unit(ivert)
      do i = 1, unit_num(ivert)
        if (unit_cons(iu+i-1) .gt. cons_max) then
          cons_max = unit_cons(iu+i-1)
c-mf03
          if (cons_max .gt. soglia_cons) then
            cons_max = cons_max - soglia_cons
cgpe            do while (cons_max .ge. soglia_cons)
cgpe              cons_max = cons_max - soglia_cons
cgpe            enddo
          endif
c-mf03-end
        endif
      enddo
c
      return
      end

      real*4 Function zero_cons (pint)
C
C  Funzione da azzerare per il calcolo della pressione intermedia
C  in modo da avere uguaglianza di consumo tra la prima fase e la
C  seconda fase per una centrale in serie (Cap Bon e servizio con
C  Booster)
C
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.INC'
      include '../inc/cap_bon_par.INC'
      include '../inc/cap_bon_ind.INC'
c
      real*4   pint     ! I) pressione intermedia tra le due fasi

!      integer*4 istaz,jstaz,ivert,jvert
      real*4    cons1_max,cons2_max
      real*4    tint
c----------------------------------------------------------------------
! - calcolo del consumo di ciascun compressore della prima fase e del
!   massimo consumo (cons1_max)
      call calc_fase_serie (istaz,ivert,fase_1,pint,tint)

! se non si e' verificato alcun errore nella simulazione
      call calc_max_cons (ivert,cons1_max)

! - calcolo del consumo di ciascun compressore della seconda fase e del
!   massimo consumo (cons2_max)
      call calc_fase_serie (istaz,jvert,fase_2,pint,tmand_stat(ivert))

! se non si e' verificato alcun errore nella simulazione
      call calc_max_cons (jvert,cons2_max)

!-funzione da azzerare
      zero_cons = cons1_max - cons2_max
c
      return
      end

      Subroutine ini_centrale (ivert,jvert)
C
C   Inizializzazione dei messaggi di errore associati alle centrali
C   istaz e jstaz
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/messag.inc'

      INTEGER*4 ivert,jvert   ! I)
      INTEGER*4 i,j
c-------------------------------------------------------------------------
!-prima fase
      mess_num(ivert) = 0
      do i = 1, mess_max
        mess_stri(i,ivert) = ' '
        do j = 1, max_rig_mess
          mess_descr(j,i) = ' '
        enddo
      enddo
!-seconda fase
      mess_num(jvert) = 0
      do i = 1, mess_max
        mess_stri(i,jvert) = ' '
        do j = 1, max_rig_mess
          mess_descr(j,i) = ' '
        enddo
      enddo
c
      return
      end

      Subroutine ini_ind_serie (ivert)

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/otc.inc'
      include '../inc/cmf_assetti.inc'
c
      INTEGER*4   ivert

      INTEGER*4 i,j,i1,ii1,k,iu
c-------------------------------------------------------------------------
      if (.not. lmf) then
        ind_opt = 1
        i1 = 0
        iu = first_unit(ivert)
        do i = 1,type_num(ivert)
          ind_s(ind_opt,i) = 0
          j = first_type(ivert)+i-1
          do k = 1,type_quant(j)
            ii1 = iu+k-1
            if (status(ii1) .eq. funz_on) then
              ind_s(ind_opt,i) = ind_s(ind_opt,i) + 1
            endif
          enddo
          i1 = i1+type_quant(j)
          iu = iu+type_quant(j)
        enddo
      else
!        num_ass_mf(i_serv(ivert)) = assetto_cm(ivert)
cgpe        num_ass_mf(i_serv(ivert)) = numrig_ass_mf(i_serv(ivert))
        iu = first_unit(ivert)
        do i = 1,unit_num(ivert)
          ind_u(i) = 0
          if (status(iu+i-1) .eq. funz_on) then
            ind_u(i) = 1
          endif
        enddo
      endif

      return ! ini_ind_serie
      end

      Subroutine calc_int (istaz,ivert,jstaz,jvert,ier)
C
C  Determina l'intervallo entro il quale operare la ricerca di zero
C  della funzione zero_cons
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/messag.inc'
      include '../inc/units.inc'
      include '../inc/simpstat.inc'
      include '../inc/cap_bon.inc'
      include '../inc/flag.inc'
      integer*4 ivert,jvert,istaz,jstaz     ! I) jstaz = I_UND
     *         ,ier                         ! O) codice di errore
      real*4    delta_p,pint,tint,delta_p_old,eps_p
      integer*4 i,iu,ju
      character*3 status_ivert(maxunits),status_jvert(maxunits)
      real*4    pint_min_save,pint_max_save
      integer*4 npint_ok,npint_ok_min
      integer*4 npint_ok_fase1,npint_ko_p
c-mf03
      integer*4 npint_ok_fase2
c-mf03-end
      parameter (npint_ok_min = 5)
C------------------------------------------------------------------------------
!-inizializzazione pint_min e pint_max
      pint_min = R_UND
      pint_max = R_UND

      ier = 0
      l_corr = .true.

!-salvataggio dei dati intermedi in pressione e temperatura
      pmand_fase1 = pmand_stat(ivert)
      tmand_fase1 = tmand_stat(ivert)
      pmand_fase2 = pmand_stat(jvert)
      tmand_fase2 = tmand_stat(jvert)

!#ifdef NOINT_0075   ! gp
!#else
      eps_p = 1.
!#endif
!      delta_p = 0.5
      delta_p = 1.

!-salvataggio degli stati delle unita'
      iu = first_unit(ivert)
      ju = first_unit(jvert)
      do i = 1, unit_num(ivert)
        status_ivert(i) = status(iu+i-1)
        status_jvert(i) = status(ju+i-1)
      enddo

!-inizializza il flag per non visualizzare i messaggi
      iopt_mess = .true.

      npint_ok = 0
      npint_ok_fase1 = 0
      npint_ko_p = 0
c-mf03
      npint_ok_fase2 = 0
c-mf03-end
!#ifdef NOINT_0075   ! gp
!      do pint = pasp_stat(ivert),pmand_stat(jvert),delta_p
!#else
c-check_pint
      do pint = pasp_stat(ivert)+delta_p+eps_p,pmand_stat(jvert),delta_p
c-check_pint      do pint = pasp_stat(ivert)+delta_p+eps_p,stat_vars(6,jvert),
c-check_pint     *          delta_p
c-check_pint
!#endif

!-calcolo la fattibilita' della prima fase
        call calc_fase_serie (istaz,ivert,fase_1,pint,tint)
        if (err(ivert) .eq. 0) then

          npint_ok_fase1 = npint_ok_fase1 + 1
!-calcolo la fattibilita' della seconda fase
          call calc_fase_serie (istaz,jvert,fase_2,pint,
     *           tmand_stat(ivert))

          if (err(jvert) .eq. 0) then
            npint_ok = npint_ok + 1
c-mf03
            npint_ok_fase2 = npint_ok_fase2 + 1
c-mf03-end
            pint_min = pint
            if (npint_ok .eq. 1) then
              pint_min_save = pint
            endif
            if (npint_ok .gt. npint_ok_min) then
              pint_min = pint_min_save
              goto 10
            endif

!-20/11/2000
          elseif((err(jvert).eq.5 .and. vert_typ(jvert).eq.tipo_semp)
     *                            .or.
cgpe     *        (err(jvert).eq.21 .and. vert_typ(jvert).eq.tipo_comp))then
     *        (err(jvert).eq.71 .and. vert_typ(jvert).eq.tipo_comp))then
            npint_ko_p = npint_ko_p + 1
!-20/11/2000-end

          endif
!-20/11/2000
        elseif ((err(ivert).eq.5 .and. vert_typ(ivert).eq.tipo_semp)
     *                            .or.
cgpe     *        (err(ivert).eq.21 .and. vert_typ(ivert).eq.tipo_comp))then
     *        (err(ivert).eq.71 .and. vert_typ(ivert).eq.tipo_comp))then
          pint_min = R_UND
          npint_ok = 0
          goto 10
!-20/11/2000-end
        endif

        if (err(ivert) .ne. 0 .or. err(jvert) .ne. 0) then
          npint_ok = 0
        endif

!-ripristino lo stato delle unita'
        do i = 1, unit_num(ivert)
          status(iu+i-1) = status_ivert(i)
          status(ju+i-1) = status_jvert(i)
        enddo
      enddo

10    continue
c-mf03
      if (npint_ok_fase2 .gt. 0 .and.
     *    npint_ok_fase2 .lt. npint_ok_min) then
        pint_min = pint_min_save
      endif
c-mf03-end

!-ripristino lo stato delle unita'
      do i = 1, unit_num(ivert)
        status(iu+i-1) = status_ivert(i)
        status(ju+i-1) = status_jvert(i)
      enddo

!-20/11/2000
!- gpe inizio
      if (fl_opt .and. num_test .eq. num_test_tot .and.
     *     npint_ok_fase1 .eq. npint_ko_p) then
!- gpe fine
!-viene settata una variabile logica per evitare di correggere la pressione
!-di mandata
        l_corr = .false.
      endif

      if (npint_ok .eq. 0 .and. pint_min .eq. R_UND) then
        pint_max = R_UND
        goto 11
      endif
!-20/11/2000-end

      npint_ok = 0
c-mf03
      npint_ok_fase2 = 0
c-mf03-end
!#ifdef NOINT_0075   ! gp
!      do pint = pmand_stat(jvert),pasp_stat(ivert),-delta_p
!#else
      do pint = pmand_stat(jvert)-delta_p-eps_p,pasp_stat(ivert),
     *          -delta_p
!#endif

!-calcolo la fattibilita' della prima fase
        call calc_fase_serie (istaz,ivert,fase_1,pint,tint)
        if (err(ivert) .eq. 0) then

!-calcolo la fattibilita' della seconda fase
          call calc_fase_serie (istaz,jvert,fase_2,pint,
     *         tmand_stat(ivert))

          if (err(jvert) .eq. 0) then
            npint_ok = npint_ok + 1
c-mf03
            npint_ok_fase2 = npint_ok_fase2 + 1
c-mf03-end
            pint_max = pint
            if (npint_ok .eq. 1) then
              pint_max_save = pint
            endif
            if (npint_ok .gt. npint_ok_min) then
              pint_max = pint_max_save
              goto 11
            endif
          endif
        endif
        if (err(ivert) .ne. 0 .or. err(jvert) .ne. 0) then
          npint_ok = 0
        endif

!-ripristino lo stato delle unita'
        do i = 1, unit_num(ivert)
          status(iu+i-1) = status_ivert(i)
          status(ju+i-1) = status_jvert(i)
        enddo
      enddo
c
11    continue
c
c-mf03
      if (npint_ok_fase2 .gt. 0 .and.
     *    npint_ok_fase2 .lt. npint_ok_min) then
        pint_max = pint_max_save
        goto 21
      endif
c-mf03-end
!-20/09/2000
!      if (pint_min .eq. R_UND .or. pint_max .eq. R_UND) then
      if (pint_min .eq. R_UND .or.
     *    pint_max .eq. R_UND .or.
     *    pint_max .lt. pint_min  ) then
!-20/09/2000-end
        ier = -11
        return
      endif

!-raffinamento degli estremi dell'intervallo
!#ifdef NOINT_0075   ! gp
!#else
      delta_p_old = delta_p
!#endif
      delta_p = 0.2

      pint_min_save = pint_min
      pint_max_save = pint_max
c
!-inizializza il flag per non visualizzare i messaggi
      iopt_mess = .true.

!#ifdef NOINT_0075   ! gp
!      do pint = pint_min_save-delta_p,pint_min_save-1,-delta_p
!#else
      do pint = pint_min_save-delta_p,pint_min_save-delta_p_old,-delta_p
!#endif

!-calcolo la fattibilita' della prima fase
        call calc_fase_serie (istaz,ivert,fase_1,pint,tint)
        if (err(ivert) .eq. 0) then

!-calcolo la fattibilita' della seconda fase
          call calc_fase_serie (istaz,jvert,fase_2,pint,
     *         tmand_stat(ivert))

          if (err(jvert) .eq. 0) then
            pint_min = pint
          endif
        else
          goto 20
        endif

!-ripristino lo stato delle unita'
        do i = 1, unit_num(ivert)
          status(iu+i-1) = status_ivert(i)
          status(ju+i-1) = status_jvert(i)
        enddo
      enddo

20    continue

!-ripristino lo stato delle unita'
      do i = 1, unit_num(ivert)
        status(iu+i-1) = status_ivert(i)
        status(ju+i-1) = status_jvert(i)
      enddo

!#ifdef NOINT_0075   ! gp
!      do pint = pint_max_save+delta_p,pint_max_save+1,delta_p
!#else
      do pint = pint_max_save+delta_p,pint_max_save+delta_p_old,delta_p
!#endif

!-calcolo la fattibilita' della prima fase
        call calc_fase_serie (istaz,ivert,fase_1,pint,tint)
        if (err(ivert) .eq. 0) then

!-calcolo la fattibilita' della seconda fase
          call calc_fase_serie (istaz,jvert,fase_2,pint,
     *         tmand_stat(ivert))

          if (err(jvert) .eq. 0) then
            pint_max = pint
          endif
        else
          goto 21
        endif

!-ripristino lo stato delle unita'
        do i = 1, unit_num(ivert)
          status(iu+i-1) = status_ivert(i)
          status(ju+i-1) = status_jvert(i)
        enddo
      enddo
c
21    continue
c
!#ifdef NOINT_0075   ! gp
!#else
!-ripristino lo stato delle unita'
      iu = first_unit(ivert)
      ju = first_unit(jvert)
      do i = 1, unit_num(ivert)
        status(iu+i-1) = status_ivert(i)
        status(ju+i-1) = status_jvert(i)
      enddo
!#endif
      return ! calc_int
      end

      Subroutine calc_fase_serie (istaz,ivert,fase,pint,tint)
C
C    Calcola i dati di una fase della centrale in serie
C
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.INC'
      include '../inc/cap_bon_par.INC'
c
      integer*4 istaz,    ! I) 
     *          ivert,    ! I) pgrato(istaz)
     *          fase      ! I) indica se si tratta della I o della II fase
      real*4    pint,     ! I) pressione intermedia tra le due fasi
     *          tint      ! I) temperatura intermedia tra le due fasi

      integer*4 i_appo
c----------------------------------------------------------------------
      i_appo = I_UND
      call init_loc_var_s (istaz,i_appo,ivert,i_appo)
      if (fase .eq. fase_1) then
        pmand_stat(ivert) = pint
      else
        pasp_stat(ivert) = pint
!-aggiornamento con la temperatura intermedia
        tasp_stat(ivert) = tint
      endif
cgpe      call check_vert (ivert,i_appo)
      call ini_ind_serie (ivert)
      call simula_cent ()

      return  ! calc_fase_serie
      end

      Subroutine init_loc_var_s (istaz,jstaz,ivert,jvert)
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/ai.inc'
      include '../inc/rk_param.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/simpstat.inc'
      include '../inc/FLAG.INC'

      INTEGER*4 istaz       ! I)
      INTEGER*4 jstaz       ! I) off_set assetto coniugato a istaz
      INTEGER*4 ivert,jvert ! I)

      INTEGER*4 pg,iserv
      integer*4 j,i1
!------------------------------------------------------------
!-inizializzazioni
      lmf = .false.
      iserv = 1
!      pg = pgrato(istaz)
      pg = ivert
      fl_simu(pg) = .true.
      iopt = .false.

!-viene settato iopt
      if (fl_opt) then
        iopt = .true.
      endif

      i_staz(iserv) = istaz
      i_vert(iserv) = pg
      i_serv(pg) = iserv

cgpe-staz      l_simu(iserv) = .true.
      l_simu(iserv) = .false.
      if (set_flag(pg).ne.0) then
        l_simu(iserv) = .true.
      endif
cgpe-staz-end

!-numero di assetti dati in input (cmf_assetti.inc)
      num_servizi = iserv

!-indice dell'assetto dato in input (cmf_assetti.inc)
      numrig_ass_mf(iserv) = assetto_cm(pg)

      if (assetto_cm(pg) .gt. 0) then
!-flag tipo di ottimizzazione (.T. ==> MF)
        lmf = .true.
      end if

!-imposizione del tipo e stato di funzionamento iniziali (queste condizioni
!-iniziali possono venire modificate nel corso della simulazione)
      htelec(pg) = htcitj(istaz)
      hstato(pg) = STATai(hstatj(istaz))

!-inizializzazione di l_act
      i1 = first_unit(pg)
      do j = i1 ,i1+unit_num(pg)-1
        l_act(j) = .false.
      enddo

      call init_state(istaz,pg)

!-inizializzazione del consumo imposto
      if (fl_cons) then
cgpe        if (lmf) then
cgpe          cons_serv_imp(iserv) = 0.
cgpe        else
          cons_imp = 0.
cgpe        endif
      else
cgpe        if (lmf) then
cgpe          cons_serv_imp(iserv) = consty(istaz)
cgpe        else
        cons_imp = consty(istaz)
cgpe        endif
      endif

!-eventuale inizializzazione delle variabili relative alla centrale jstaz
      if (jstaz .ne. I_UND) then
!        pg = pgrato(jstaz)
        pg = jvert
        iserv = iserv + 1
        i_staz(iserv) = jstaz
        i_vert(iserv) = pg
        i_serv(pg) = iserv
        numrig_ass_mf(iserv) = assetto_cm(pg)
        fl_simu(pg) = .true.
        num_servizi = iserv

cgpe-staz      l_simu(iserv) = .true.
        l_simu(iserv) = .false.
        if (set_flag(pg).ne.0) then
          l_simu(iserv) = .true.
        endif
cgpe-staz-end

!-imposizione del tipo e stato di funzionamento iniziali (queste condizioni
!-iniziali possono venire modificate nel corso della simulazione)
        htelec(pg) = htcitj(jstaz)
        hstato(pg) = STATai(hstatj(jstaz))

!-inizializzazione di l_act
        i1 = first_unit(pg)
        do j = i1 ,i1+unit_num(pg)-1
          l_act(j) = .false.
        enddo

        call init_state(jstaz,pg)

cgpe!-inizializzazione del consumo imposto
cgpe        cons_imp = 0.
cgpe        cons_serv_imp(iserv) = 0.
cgpe        if (fl_cons) then
cgpe          if (lmf) then
cgpe            cons_serv_imp(iserv) = 0.
cgpe          else
cgpe            cons_imp = 0.
cgpe          endif
cgpe        else
cgpe          if (lmf) then
cgpe            cons_serv_imp(iserv) = consty(jstaz)
cgpe          else
cgpe            cons_imp = consty(jstaz)
cgpe          endif
cgpe        endif

      endif
c
      return
      END ! init_loc_var_s

      Subroutine set_tipi (ivert,jvert,istatus,ntest)
C
C   Determina il vettore con le possibili combinazioni di stati
C   delle unita' sulla prima e sulla seconda fase
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/cap_bon_par.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
c
      INTEGER*4 ivert,jvert    ! I) indice delle centrali
      INTEGER*4 istatus(*)     ! O) vettore contenente lo stato delle unita''
! istatus((unit_sim*(jtest-1))+i) , con: jtest=1,max_ntest e i=1,unit_sim
      INTEGER*4 ntest          ! O) numero di configurazioni da provare

      INTEGER*4 iu,i,it,i1,k

      INTEGER*4 type_act_fase1(maxunits)
      INTEGER*4 type_act_fase2(maxunits)
      INTEGER*4 inum,num,unit_sim,type_sim
      INTEGER*4 ki1,ki2,ki3,ki4,ki5,ki6,ki7,ki8,ki9,ki10,ki11,ki12
c-------------------------------------------------------------------------
      iu = first_unit(ivert)
      it = first_type(ivert)
      unit_sim = unit_num(ivert)
      type_sim = type_num(ivert)

      ntest = 0
!      do i = 1, type_sim
!        type_act_fase1 (i) = 0
!      enddo

!-02/06/2000
c---->          calcolo le unita' necessariamente da accendere
      num = 0
      i1 = iu
      do i=1,type_sim
        type_act_fase1(i) = 0
c???????????        do k=1,type_quant(i)
        do k=1,type_quant(it+i-1)

c???????????-end
          if(unit_avail(i1+k-1).gt.1) then
            num = num+1
            type_act_fase1(i) = type_act_fase1(i)+1
          end if
        end do
c???????????        i1 = i1+type_quant(i)
        i1 = i1+type_quant(it+i-1)
c???????????-end
      end do
!-02/06/2000-end

!-determinazione delle combinazioni con la configurazione con le unita'
!-da accendere necessariamente
!-20/09/2000
      if (num .gt. 0) then
       if (ntest.lt.max_ntest) then
        call set_tipi_fase2(ivert,jvert,type_act_fase1,istatus,ntest)
       end if
      end if
!-20/09/2000-end

      do inum=1,12
       do ki1=1,type_sim
        type_act_fase1(ki1) = type_act_fase1(ki1)+1
        if(type_avail(it+ki1-1).ge.type_act_fase1(ki1)) then
         if(inum.gt.1) then
c---->    +++  111 -----> piu' di un compressore inserito
          do ki2=ki1,type_sim
           type_act_fase1(ki2) = type_act_fase1(ki2)+1
           if(type_avail(it+ki2-1).ge.type_act_fase1(ki2)) then
            if(inum.gt.2) then
c---->    +++  222 ----> piu' di due compressori da inserire
             do ki3=ki2,type_sim
              type_act_fase1(ki3) = type_act_fase1(ki3)+1
              if(type_avail(it+ki3-1).ge.type_act_fase1(ki3)) then
               if(inum.gt.3) then
c---->    +++  333 ---->  piu' di tre compressori da inserire
                do ki4=ki3,type_sim
                 type_act_fase1(ki4) = type_act_fase1(ki4)+1
                 if(type_avail(it+ki4-1).ge.type_act_fase1(ki4)) then
                  if(inum.gt.4) then
c---->    +++  444 ---->  piu' di quattro compressori da inserire
                   do ki5=ki4,type_sim
                    type_act_fase1(ki5) = type_act_fase1(ki5)+1
                    if(type_avail(it+ki5-1).ge.type_act_fase1(ki5)) then
                     if(inum.gt.5) then
c---->    +++  555 ---->  piu' di cinque compressori da inserire
                      do ki6=ki5,type_sim
                       type_act_fase1(ki6) = type_act_fase1(ki6)+1
                       if(type_avail(it+ki6-1).ge.
     *                    type_act_fase1(ki6)) then
                        if(inum.gt.6) then
c---->    +++  666 ---->  piu' di sei compressori da inserire
                         do ki7=ki6,type_sim
                          type_act_fase1(ki7) = type_act_fase1(ki7)+1
                          if(type_avail(it+ki7-1).ge.
     *                       type_act_fase1(ki7)) then
                           if(inum.gt.7) then
c---->    +++  777 ---->  piu' di sette compressori da inserire
                            do ki8=ki7,type_sim
                             type_act_fase1(ki8)=type_act_fase1(ki8)+1
                             if(type_avail(it+ki8-1).ge.
     *                          type_act_fase1(ki8)) then
                              if(inum.gt.8) then
c---->    +++  888 ---->  piu' di otto compressori da inserire
                               do ki9=ki8,type_sim
                                type_act_fase1(ki9)=
     *                               type_act_fase1(ki9)+1
                                if(type_avail(it+ki9).ge.
     *                             type_act_fase1(ki9)) then
                                 if(inum.gt.9) then
c---->    +++  999 ---->  piu' di nove compressori da inserire
                                  do ki10=ki9,type_sim
                                   type_act_fase1(ki10)=
     *                                 type_act_fase1(ki10)+1
                                   if(type_avail(it+ki10-1).ge.
     *                                type_act_fase1(ki10)) then
                                    if(inum.gt.10) then
c---->    +++  10 ---->  piu' di dieci compressori da inserire
                                     do ki11=ki10,type_sim
                                      type_act_fase1(ki11)=
     *                                    type_act_fase1(ki11)+1
                                      if(type_avail(it+ki11-1).ge.
     *                                   type_act_fase1(ki11))then
                                       if(inum.gt.11) then
c---->    +++  11 ---->  piu' di undici compressori da inserire
                                        do ki12=ki11,type_sim
                                         type_act_fase1(ki12)=
     *                                           type_act_fase1(ki12)+1 
                                         if(type_avail(it+ki12-1).ge.
     *                                       type_act_fase2(ki12)) then
c---->    +++  12 ---->  simulazione con dodici  compressori inseriti
                                          if((num+12).ge.numin) then
                                           if (ntest.lt.max_ntest) then
                                            call 
     *set_tipi_fase2(ivert,jvert,type_act_fase1,istatus,ntest)
                                           end if

                                          end if
                                         end if
                                         type_act_fase1(ki12)=
     *                                      type_act_fase1(ki12)-1 
                                        end do
                                       else
c---->                     simulazione con 11 compressori inseriti
                                        if((num+11).ge.numin) then
                                         if (ntest.lt.max_ntest) then
                                          call 
     *set_tipi_fase2(ivert,jvert,type_act_fase1,istatus,ntest)
                                         end if

                                        end if
                                       end if
                                      end if
                                      type_act_fase1(ki11) =
     *                                     type_act_fase1(ki11)-1
                                     end do
                                    else
c---->                     simulazione con dieci compressori inseriti
                                     if((num+10).ge.numin) then
                                      if (ntest.lt.max_ntest) then
                                       call set_tipi_fase2
     *(ivert,jvert,type_act_fase1,istatus,ntest)
                                      end if

                                     end if
                                    end if
                                   end if
                                   type_act_fase1(ki10)=
     *                                   type_act_fase1(ki10)-1
                                  end do
                                 else
c---->                     simulazione con nove compressori inseriti
                                  if((num+9).ge.numin) then
                                   if (ntest.lt.max_ntest) then
                                    call set_tipi_fase2
     *(ivert,jvert,type_act_fase1,istatus,ntest)
                                   end if
                                  end if
                                 end if
                                end if
                                type_act_fase1(ki9)=
     *                              type_act_fase1(ki9)-1
                               end do
                              else
c---->                     simulazione con otto compressori inseriti
                               if((num+8).ge.numin) then
                                if (ntest.lt.max_ntest) then
                                 call set_tipi_fase2
     *(ivert,jvert,type_act_fase1,istatus,ntest)
                                end if
                               end if
                              end if
                             end if
                             type_act_fase1(ki8) = type_act_fase1(ki8)-1
                            end do
                           else
c---->                     simulazione con sette compressori inseriti
                            if((num+7).ge.numin) then
                             if (ntest.lt.max_ntest) then
                              call set_tipi_fase2(ivert,jvert,
     *type_act_fase1,istatus,ntest)
                             end if

                            end if
                           end if
                          end if
                          type_act_fase1(ki7) = type_act_fase1(ki7)-1
                         end do
                        else
c---->                     simulazione con sei compressori inseriti
                         if((num+6).ge.numin) then
                          if (ntest.lt.max_ntest) then
                           call set_tipi_fase2(ivert,jvert,
     *type_act_fase1,istatus,ntest)
                          end if

                         end if
                        end if
                       end if
                       type_act_fase1(ki6) = type_act_fase1(ki6)-1
                      end do
                     else
c---->                     simulazione con cinque compressori inseriti
                      if((num+5).ge.numin) then
                       if (ntest.lt.max_ntest) then
                        call set_tipi_fase2(ivert,jvert,
     *type_act_fase1,istatus,ntest)
                       end if

                      end if
                     end if
                    end if
                    type_act_fase1(ki5) = type_act_fase1(ki5)-1
                   end do
                  else
c---->                  simulazione con quattro compressori inseriti
                   if((num+4).ge.numin) then
                    if (ntest.lt.max_ntest) then
                     call set_tipi_fase2(ivert,jvert,
     *type_act_fase1,istatus,ntest)
                    end if

                   end if
                  end if
                 end if
                 type_act_fase1(ki4) = type_act_fase1(ki4)-1
                end do
               else
c---->                  simulazione con tre compressori inseriti
                if((num+3).ge.numin) then
                 call set_tipi_fase2(ivert,jvert,
     *type_act_fase1,istatus,ntest)
                 if (ntest.lt.max_ntest) then
                 end if

                end if
               end if
              end if
              type_act_fase1(ki3) = type_act_fase1(ki3)-1
             end do


            else
c---->                  simulazione con due compressori inseriti
             if((num+2).ge.numin) then
              if (ntest.lt.max_ntest) then
               call set_tipi_fase2(ivert,jvert,type_act_fase1,
     *istatus,ntest)
              end if

             end if
            end if
           end if
           type_act_fase1(ki2) = type_act_fase1(ki2)-1
          end do
         else
c---->                  simulazione con un solo compressore inserito
          if((num+1).ge.numin) then
           if (ntest.lt.max_ntest) then
            call set_tipi_fase2(ivert,jvert,type_act_fase1,
     *istatus,ntest)
           end if

          end if
         end if
        end if
        type_act_fase1(ki1) = type_act_fase1(ki1)-1
       end do
      end do
c
      return ! set_tipi
      end

      Subroutine set_tipi_fase2 (ivert,jvert,type_act_fase1,
     *           istatus,ntest)

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/cap_bon_par.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
c
      INTEGER*4 ivert,jvert    ! I) risp. pgrato(istaz),pgrato_app(jstaz)
      INTEGER*4 istatus(*)     ! O) vettore contenente lo stato delle unita''
! istatus((unit_sim*(jtest-1))+i) , con: jtest=1,max_ntest e i=1,unit_sim
      INTEGER*4 ntest          ! O) numero di configurazioni da provare

      INTEGER*4 i,jt,iu,ju,i1,k

      INTEGER*4 type_act_fase1(*)
      INTEGER*4 type_act_fase2(maxunits)
      INTEGER*4 inum,num,unit_sim,type_sim
      INTEGER*4 ki1,ki2,ki3,ki4,ki5,ki6,ki7,ki8,ki9,ki10,ki11,ki12
c-------------------------------------------------------------------------
      iu = first_unit(ivert)
      ju = first_unit(jvert)
      jt = first_type(jvert)
      unit_sim = unit_num(jvert)
      type_sim = type_num(jvert)
!      do i=1,type_sim
!        type_act_fase2(i) = 0
!      enddo

!-02/06/2000
c---->          calcolo le unita' necessariamente da accendere
      num = 0
      i1 = ju
      do i=1,type_sim
        type_act_fase2(i) = 0
c???????????        do k=1,type_quant(i)
        do k=1,type_quant(jt+i-1)
c???????????-end
          if(unit_avail(i1+k-1).gt.1) then
            num = num+1
            type_act_fase2(i) = type_act_fase2(i)+1
          end if
        end do
c???????????        i1 = i1+type_quant(i)
        i1 = i1+type_quant(jt+i-1)
c???????????-end
      end do
!-02/06/2000-end

!-determinazione delle combinazioni con la configurazione con le unita'
!-da accendere necessariamente
!-20/09/2000
      if (num .gt. 0) then
!-20/09/2000-end
       if (ntest.lt.max_ntest) then
        call gen_stat_serie (unit_sim,type_sim,type_quant(jt),
     *unit_avail(iu),unit_avail(ju),type_act_fase1,type_act_fase2,
     *ntest,istatus)
       endif
      endif

      do inum=1,12
       do ki1=1,type_sim
        type_act_fase2(ki1) = type_act_fase2(ki1)+1
        if(type_avail(jt+ki1-1).ge.type_act_fase2(ki1)) then
         if(inum.gt.1) then
c---->    +++  111 -----> piu' di un compressore inserito
          do ki2=ki1,type_sim
           type_act_fase2(ki2) = type_act_fase2(ki2)+1
           if(type_avail(jt+ki2-1).ge.type_act_fase2(ki2)) then
            if(inum.gt.2) then
c---->    +++  222 ----> piu' di due compressori da inserire
             do ki3=ki2,type_sim
              type_act_fase2(ki3) = type_act_fase2(ki3)+1
              if(type_avail(jt+ki3-1).ge.type_act_fase2(ki3)) then
               if(inum.gt.3) then
c---->    +++  333 ---->  piu' di tre compressori da inserire
                do ki4=ki3,type_sim
                 type_act_fase2(ki4) = type_act_fase2(ki4)+1
                 if(type_avail(jt+ki4-1).ge.type_act_fase2(ki4)) then
                  if(inum.gt.4) then
c---->    +++  444 ---->  piu' di quattro compressori da inserire
                   do ki5=ki4,type_sim
                    type_act_fase2(ki5) = type_act_fase2(ki5)+1
                    if(type_avail(jt+ki5-1).ge.type_act_fase2(ki5)) then
                     if(inum.gt.5) then
c---->    +++  555 ---->  piu' di cinque compressori da inserire
                      do ki6=ki5,type_sim
                       type_act_fase2(ki6) = type_act_fase2(ki6)+1
                       if(type_avail(jt+ki6-1).ge.
     *                    type_act_fase2(ki6))then
                        if(inum.gt.6) then
c---->    +++  666 ---->  piu' di sei compressori da inserire
                         do ki7=ki6,type_sim
                          type_act_fase2(ki7) = type_act_fase2(ki7)+1
                          if(type_avail(jt+ki7-1).ge.
     *                       type_act_fase2(ki7)) then
                           if(inum.gt.7) then
c---->    +++  777 ---->  piu' di sette compressori da inserire
                            do ki8=ki7,type_sim
                             type_act_fase2(ki8)=type_act_fase2(ki8)+1
                             if(type_avail(jt+ki8-1).ge.
     *                          type_act_fase2(ki8)) then
                              if(inum.gt.8) then
c---->    +++  888 ---->  piu' di otto compressori da inserire
                               do ki9=ki8,type_sim
                                type_act_fase2(ki9)=
     *                               type_act_fase2(ki9)+1
                                if(type_avail(jt+ki9).ge.
     *                             type_act_fase2(ki9)) then
                                 if(inum.gt.9) then
c---->    +++  999 ---->  piu' di nove compressori da inserire
                                  do ki10=ki9,type_sim
                                   type_act_fase2(ki10)=
     *                                 type_act_fase2(ki10)+1
                                   if(type_avail(jt+ki10-1).ge.
     *                                type_act_fase2(ki10)) then
                                    if(inum.gt.10) then
c---->    +++  10 ---->  piu' di dieci compressori da inserire
                                     do ki11=ki10,type_sim
                                      type_act_fase2(ki11)=
     *                                    type_act_fase2(ki11)+1
                                      if(type_avail(jt+ki11-1).ge.
     *                                   type_act_fase2(ki11))then
                                       if(inum.gt.11) then
c---->    +++  11 ---->  piu' di undici compressori da inserire
                                        do ki12=ki11,type_sim
                                         type_act_fase2(ki12)=
     *                                           type_act_fase2(ki12)+1 
                                         if(type_avail(jt+ki12-1).ge.
     *                                       type_act_fase2(ki11)) then
c---->    +++  12 ---->  simulazione con dodici  compressori inseriti
                                          if((num+12).ge.numin) then
                                           if (ntest.lt.max_ntest) then
                                            call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),
     *unit_avail(iu),unit_avail(ju),type_act_fase1,type_act_fase2,
     *ntest,istatus)
                                           end if

                                          end if
                                         end if
                                         type_act_fase2(ki12)=
     *                                      type_act_fase2(ki12)-1 
                                        end do
                                       else
c---->                     simulazione con 11 compressori inseriti
                                        if((num+11).ge.numin) then
                                         if (ntest.lt.max_ntest) then
                                          call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                                         end if

                                        end if
                                       end if
                                      end if
                                      type_act_fase2(ki11) =
     *                                     type_act_fase2(ki11)-1
                                     end do
                                    else
c---->                     simulazione con dieci compressori inseriti
                                     if((num+10).ge.numin) then
                                      if (ntest.lt.max_ntest) then
                                       call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                                      end if

                                     end if
                                    end if
                                   end if
                                   type_act_fase2(ki10)=
     *                                   type_act_fase2(ki10)-1
                                  end do
                                 else
c---->                     simulazione con nove compressori inseriti
                                  if((num+9).ge.numin) then
                                   if (ntest.lt.max_ntest) then
                                    call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                                   end if
                                  end if
                                 end if
                                end if
                                type_act_fase2(ki9)=
     *                              type_act_fase2(ki9)-1
                               end do
                              else
c---->                     simulazione con otto compressori inseriti
                               if((num+8).ge.numin) then
                                if (ntest.lt.max_ntest) then
                                 call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                                end if
                               end if
                              end if
                             end if
                             type_act_fase2(ki8) = type_act_fase2(ki8)-1
                            end do
                           else
c---->                     simulazione con sette compressori inseriti
                            if((num+7).ge.numin) then
                             if (ntest.lt.max_ntest) then
                              call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                             end if

                            end if
                           end if
                          end if
                          type_act_fase2(ki7) = type_act_fase2(ki7)-1
                         end do
                        else
c---->                     simulazione con sei compressori inseriti
                         if((num+6).ge.numin) then
                          if (ntest.lt.max_ntest) then
                           call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                          end if

                         end if
                        end if
                       end if
                       type_act_fase2(ki6) = type_act_fase2(ki6)-1
                      end do
                     else
c---->                     simulazione con cinque compressori inseriti
                      if((num+5).ge.numin) then
                       if (ntest.lt.max_ntest) then
                        call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)

                       end if

                      end if
                     end if
                    end if
                    type_act_fase2(ki5) = type_act_fase2(ki5)-1
                   end do
                  else
c---->                  simulazione con quattro compressori inseriti
                   if((num+4).ge.numin) then
                    if (ntest.lt.max_ntest) then
                     call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
                    end if

                   end if
                  end if
                 end if
                 type_act_fase2(ki4) = type_act_fase2(ki4)-1
                end do
               else
c---->                  simulazione con tre compressori inseriti
                if((num+3).ge.numin) then
                 if (ntest.lt.max_ntest) then
                  call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)

                 end if

                end if
               end if
              end if
              type_act_fase2(ki3) = type_act_fase2(ki3)-1
             end do


            else
c---->                  simulazione con due compressori inseriti
             if((num+2).ge.numin) then
              if (ntest.lt.max_ntest) then
               call gen_stat_serie (
     *unit_sim,type_sim,type_quant(jt),unit_avail(iu),unit_avail(ju),
     *type_act_fase1,type_act_fase2,ntest,istatus)
              end if

             end if
            end if
           end if
           type_act_fase2(ki2) = type_act_fase2(ki2)-1
          end do
         else
c---->                  simulazione con un solo compressore inserito
          if((num+1).ge.numin) then
           if (ntest.lt.max_ntest) then
            call gen_stat_serie (unit_sim,type_sim,type_quant(jt),
     *unit_avail(iu),unit_avail(ju),type_act_fase1,type_act_fase2,
     *ntest,istatus)
           end if

          end if
         end if
        end if
        type_act_fase2(ki1) = type_act_fase2(ki1)-1
       end do
      end do
c
      return ! set_tipi_fase2
      end

      Subroutine gen_stat_serie (unit_num,type_num,type_quant,
     *           unit_avail_fase1,unit_avail_fase2,
     *           type_act_fase1,type_act_fase2,ntest,istatus)
C
C	Genera il vettore delle unita' accese per ogni centrale
C       in serie (I e II fase)
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/cap_bon_par.inc'

      INTEGER*4 unit_num            ! numero di unita'
     *         ,type_num            ! numero di tipi diversi di unita'
     *         ,type_quant(*)       ! numero di unita' per ciascun tipo
     *         ,unit_avail_fase1(*) ! disponibilta' delle unita' prima fase
     *         ,unit_avail_fase2(*) ! disponibilta' delle unita' seconda fase
     *         ,type_act_fase1(*)   ! numero di unita' attive per ciascun tipo
                                    ! per la configurazione sulla prima fase
     *         ,type_act_fase2(*)   ! numero di unita' attive per ciascun tipo
                                    ! per la configurazione sulla seconda fase
     *         ,ntest               ! indice configurazione
     *         ,istatus(*)          ! vettore per assegnare una unita' sulla
                                    ! prima o sulla seconda fase
      INTEGER*4 I,i1,k
      INTEGER*4 type_act_fase1_app(maxunits)
     *         ,type_act_fase2_app(maxunits)
c----------------------------------------------------------------------
!-inizializzazione di type_act_fase1_app, type_act_fase2_app
      do i = 1,type_num
        type_act_fase1_app(i) = type_act_fase1(i)
        type_act_fase2_app(i) = type_act_fase2(i)
      end do

      ntest = ntest + 1
!-unita' prima fase
!      i1 = 0
!      do i = 1,type_num
!        do k=1,type_act_fase1(i)
!          istatus(unit_num*(ntest-1)+k+i1) = fase_1
!        end do
!        i1 = i1+type_quant(i)
!      end do
c
      i1 = 0
      do i = 1, type_num
        do k = 1, type_quant(i)
          if (unit_avail_fase1(k+i1) .gt. 0) then
            if (type_act_fase1_app(i) .gt. 0) then
              istatus(unit_num*(ntest-1)+k+i1) = fase_1
              type_act_fase1_app(i) = type_act_fase1_app(i) - 1
            endif
          endif
        end do
        i1 = i1+type_quant(i)
      end do
c
!-unita' seconda fase
!      i1 = 0
!      do i = 1,type_num
!        do k=1,type_act_fase2(i)
!          istatus(unit_num*(ntest-1)+k+i1) = fase_2
!        end do
!        i1 = i1+type_quant(i)
!      end do
c
      i1 = 0
      do i = 1,type_num
        do k = 1,type_quant(i)
          if (unit_avail_fase2(k+i1) .gt. 0) then
            if (type_act_fase2_app(i) .gt. 0) then
              istatus(unit_num*(ntest-1)+k+i1) = fase_2
              type_act_fase2_app(i) = type_act_fase2_app(i) - 1
            endif
          endif
        end do
        i1 = i1+type_quant(i)
      end do
c
      return  ! gen_stat_serie
      end

*** LIB_OTC_SERIE.FOR *** gestione della centrale in serie ********************
