	!Simcent.for

      Subroutine s_cnt (*)

!- Per ogni centrale da trattare:
!  - viene calcolato il punto di lavoro simulato, nel caso di ripartizione
!    manuale del carico
!  - viene calcolato il punto di lavoro ottimo
!- Viene restituito un indice associato al risultato della simulazione per
!  ogni centrale

*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/flag.INC'
      include '../inc/messag.INC'
      include '../inc/cap_bon_prv.INC'
      include '../inc/nw.inc'
      include '../inc/err.inc'

      real*4     sum_cons
      integer*4  kflag
      logical*2  l_rec_ce ! e' valorizzato per la rete
c----------------------------------------------------------------
c
      lcent_last = .true.
c-din
c-din      call sim_cent (sum_cons,l_rec_ce)
      kflag = 0
      call sim_cent (sum_cons,l_rec_ce,kflag)
c-din-end
c
      call verstel_nw ()
crsa	if (fl_ris .eq. -1) goto 101
	if (fl_ris_gen .eq. err_ko) goto 101
      return

101   continue
      return 1 ! uscita KO

      end ! s_cnt

c-din
c-din      Subroutine sim_cent (sum_cons,l_rec_ce)
      Subroutine sim_cent (sum_cons,l_rec_ce,kflag)
c-din-end

!- Per ogni centrale da trattare:
!  - viene calcolato il punto di lavoro simulato, nel caso di ripartizione manuale
!    del carico 
!  o in alternativa
!  - viene calcolato il punto di lavoro ottimo
!- Viene restituito:
!  - un flag per la simulazione di rete
!  - un indice di errore associato a ciascuna centrale

*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/tz.inc'
      include '../inc/jc.inc'
      include '../inc/nw.inc'
      include '../inc/stations.inc'
      include '../inc/flag.INC'
      include '../inc/messag.INC'
      include '../inc/cap_bon_prv.INC'

	include '../inc/ASS_COND.INC'

CMAR
      COMMON/MAR/istaz


      real*4     sum_cons  ! O) per la rete, massima differenza fra il consumo
                           !    calcolato e quello imposto
      logical*2  l_rec_ce  ! O) per la rete, risultato della simulazione globale
                           !    = .T. richiesta di recupero
                           !          (almeno una cen ha errore recuperabile)
      integer*4  kflag     ! O) risultato della simulazione per ogni centrale
           				 !    = 0  => tutte le centrale ok
						 !    = 1  => almeno una centrale con errore di tipo warning
						 !    = -1 => almeno una centrale con errore di tipo fatal

      integer*4  istaz,jstaz,pg,pg_ass,pg_app
      integer*4  flag_fatal
      INTEGER*4  fl_sim_istaz,fl_sim_jstaz
c-din
      integer*4  setflag
c-din-end
      real*4     app_cons
      logical*2  l_simula,fl_ass,flag_serie,fl_goto
      logical*2  ce_serie
      external   ce_serie
      logical*2  simula_ce
      external   simula_ce
      real*4     soglia_vio_vinc
      external   soglia_vio_vinc
c----------------------------------------------------------------
c-din
c-din      try_ass_par = .true.
c-din      try_rec = .true.
     
      
      kflag = 0
      if (fl_din) then
        try_ass_par = .false.
        try_rec = .false.
      else
        try_ass_par = .true.
        try_rec = .true.
      endif
c-din-end

10    continue

! inizializzazione del flag FL_SIMU per tutte le centrali
      do istaz = otipti(e_ce), etipti(e_ce)
        pg = pgrato(istaz)
c flag per indicare il risultato della simulazione di ciascuna centrale
        fl_fatal(pg) = 0
c flag per indicare se una centrale è già stata simulata
        fl_simu(pg) = .false.
        fl_cen_punto_ko(pg) = .false.
c e' necessario inizializzare a zero il numero di messaggi delle iterazioni precedenti
        call deapp_mess (pg)

        if (ce_serie(istaz)) then
          fl_fatal(pgrato_app(istaz)) = 0
          fl_simu(pgrato_app(istaz)) = .false.
          fl_cen_punto_ko(pgrato_app(istaz)) = .false.
          call deapp_mess (pgrato_app(istaz))
        endif
      enddo
c
      do istaz = otipti(e_ce), etipti(e_ce)
        pg = pgrato(istaz)
cgpe-29-08-06
        if (.not.fl_simu(pg)) then
c Nota: per una centrale in serie è sufficiente questo test
cgpe-29-08-06-end
c-ele 10/8 in caso di 2 assetti paralleli al secondo passaggio, con tale assegnazione
c          si può perdere traccia di eventuali situazioni bloccanti individuate dalla
c          check_ce al primo passaggio e tracciate dal valore fl_fatal=-1
c        fl_fatal(pg) = 0
c-din
        try_rec = .true.
c        if (.not.fl_din) try_rec = .true.
c-din-end

        jstaz = I_UND
cstaz-corr
c        if (fl_sim) then
        if (fl_sim .or. .not.fl_sim .and.try_ass_par) then
cstaz-corr-end
          call find_ce_ass(istaz,jstaz)
cstaz-corr
        endif
cstaz-corr-end
c        if (.not.fl_sim .and. try_ass_par) then
        if (.not.fl_sim .and. try_ass_par .and. jstaz .ne. I_UND) then
          try_rec = .false.
        endif
c fl_ass = .T. se esiste un assetto associato a istaz
        if (jstaz .eq. I_UND) then
          fl_ass = .false.
        else
          fl_ass = .true.
          pg_ass = pgrato(jstaz)
c-ele 10/8          fl_fatal(pg_ass) = 0
        endif

c flag_serie = .T. se istaz è una centrale in serie
        flag_serie = ce_serie(istaz)
        pg_app = pgrato_app(istaz)
c-ele 10/8        if (flag_serie) fl_fatal(pg_app) = 0

cgpe-29-08-06        if (.not.fl_simu(pg)) then
c Nota: per una centrale in serie è sufficiente questo test
cgpe-29-08-06-end
          if (.not. fl_sim) then
c simulazione della centrale in rete e dunque vengono settate
c alla centrale le condizioni imposte dalla rete
c anche per l'eventuale centrale jstaz associate a istaz (assetto proveniente
c dalla stessa centrale MF o centrale in serie)
            call setta_cin (istaz,jstaz)

            if (fl_din) then
              call setta_stato_tbg (istaz,jstaz,
     *             fl_sim_istaz,fl_sim_jstaz)
              fl_goto = .false.
              if (istaz.ne.I_UND .and. fl_sim_istaz .eq. 0) then
                if (simula_ce(istaz)) then
                  fl_fatal(pg) = -100001
                  tot_cons(pg) = 0.
                  set_flag(pg) = 0
                  fl_simu(pg) = .true.
                  fl_goto = .true.
                endif
              endif
              if (jstaz.ne.I_UND .and. fl_sim_jstaz .eq. 0) then
                if (simula_ce(jstaz)) then
                  fl_fatal(pgrato(jstaz)) = -100001
                  tot_cons(pgrato(jstaz)) = 0.
                  set_flag(pgrato(jstaz)) = 0
                  fl_simu(pgrato(jstaz)) = .true.
                  fl_goto = .true.
                endif
              endif
              if (fl_goto) goto 100
            endif
          endif

c controllo la simulabilità della centrale (ovvero che non sia spenta o con
c condizioni non fattibili)
            call check_ce (istaz,jstaz,l_simula)
c l_simula=.T. se almeno uno degli assetti ha condizioni ok per la simulazione
c Nella 
            if (l_simula) then
              call s_cnt_new(istaz,jstaz)
c In uscita dalla subroutine ottimo_cent fl_ris_ce assume i seguenti valori:
c = 0, se è stata trovata una configurazione ottima dalla calc_ottimo
c = 1, se è stata trovata una configurazione ottima in seguito a recupero
c = -1, se non è stata trovata alcuna soluzione
              if (fl_ris_ce(pg).eq.0) then
c simulazione della centrale ok
c nella variabile consumo viene memorizzato il consumo di una centrale binaria o di
c un assetto o di una centrale in serie
cmod_rete2
                if (.not. fl_sim) then

C GZ LA SIMULAZIONE DI STAZIONE CAMBIA SOLO LA TEMPTJ(IND) DI STAZIONE E
C GZ NON LA TEMPTJ(IV). QUESTO PERCHE' LA TEMPTJ(IV) IN REALTA' PUO' 
C GZ ESSERE DOVUTA AL CONTRIBUTO DI PIU' FLUSSO AFFERENTI NEL NODO 
C GZ (IV), DI CUI (IND) E' SOLO UNO.
                  valsjc(istaz-oice) = tot_cons(pg)
cgpe-tout                  temptj(istaz) = tmand_stat(pg)
                  touttz(istaz) = tmand_stat(pg)
                  if (flag_serie) then
                    valsjc(istaz-oice) = valsjc(istaz-oice) +
     *                                   tot_cons(pg_app)
                    temptj(istaz) = tmand_stat(pg_app)
                  endif
                  if (fl_ass) then
cgpe-tout                    temptj(jstaz) = tmand_stat(pg_ass)
                    touttz(jstaz) = tmand_stat(pg_ass)
                    valsjc(jstaz-oice) = tot_cons(pg_ass)
cstaz-corr
                    if (fl_ris_ce(pg_ass) .eq. 1) then
                      l_rec_ce = .true.
                    endif
cstaz-corr-end
                  endif

                endif
cgpe-29-08-06                fl_fatal(pg) = fl_ris_ce(pg)
                if (set_flag(pg) .eq. 1) then
                  fl_fatal(pg) = fl_ris_ce(pg)
c-11-06-2007
                  if (fl_cen_punto_ko(pg)) then
c ottimo senza soluzione, ma con il calcolo del punto ko
                    fl_fatal(pg) = -10001
                  endif
c-11-06-2007-end
                endif
cgpe-29-08-06-end
                if (fl_ass) then
cgpe-29-08-06
                 if (set_flag(pg_ass) .eq. 1) then
cgpe-29-08-06-end
                  fl_fatal(pg_ass) = fl_ris_ce(pg_ass)
                  if (fl_cen_punto_ko(pg_ass)) then
c ottimo senza soluzione, ma con il calcolo del punto ko
                    fl_fatal(pg_ass) = -10001
                  endif
cgpe-corr-staz
                 else
                  if (.not.fl_sim) then
                    fl_fatal(pg_ass) = fl_ris_ce(pg_ass)
                    if (fl_cen_punto_ko(pg_ass)) then
c ottimo senza soluzione, ma con il calcolo del punto ko
                      fl_fatal(pg_ass) = -10001
                    endif
                  endif
cgpe-corr-staz-end
cgpe-29-08-06
                 endif
cgpe-29-08-06-end
                else
                  if (flag_serie) fl_fatal(pg_app) = fl_ris_ce(pg_app)
                endif
              else
c-staz-corr
c                if (try_ass_par) then
c                if (try_ass_par .and. fl_ass) then
                if (.not.fl_sim .and. try_ass_par .and. fl_ass) then
                  try_ass_par = .false.
                  goto 10
                endif
c-staz-corr-end
c gestione dell'errore della centrale
                if (fl_ris_ce(pg) .eq. 1) then
c deriva dal recupero con modifica dei valori di set per l'assetto pg
                  fl_fatal(pg) = hstatj(istaz)
CMAR_ASS_COND WARNING
c	 IF(FLAG_ASS_COND_P(PG))THEN
c       fl_fatal(pg+1)=fl_fatal(pg)
c	 ENDIF
CMAR_ASS_COND WARNING


                  if (fl_ass) then
cgpe-29-08-06
                   if (set_flag(pg_ass) .eq. 1) then
cgpe-29-08-06-end
                    if (tipo_criterio(pg_ass).eq.crit_man) then
                      fl_fatal(pg_ass) = fl_ris_ce(pg_ass)
                    else
c deriva dal recupero con modifica dei valori di set per l'assetto pg_ass
				    fl_fatal(pg_ass) = hstatj(jstaz)
                    endif
cgpe-29-08-06
                   endif
cgpe-29-08-06-end
                  else
                    if (flag_serie) fl_fatal(pg_app) = hstatj(istaz)
                  endif
                  if (.not. fl_sim) then
                    l_rec_ce = .true.
cgpe                    valsjc(istaz-oice) = tot_cons(pg)
cgpe                    if (fl_ass) valsjc(jstaz-oice) = tot_cons(pg_ass)
cgpe                    if (flag_serie) valsjc(istaz-oice) = valsjc(istaz-oice) + tot_cons(pg_app)
C GZ LA SIMULAZIONE DI STAZIONE CAMBIA SOLO LA TEMPTJ(IND) DI STAZIONE E
C GZ NON LA TEMPTJ(IV). QUESTO PERCHE' LA TEMPTJ(IV) IN REALTA' PUO' 
C GZ ESSERE DOVUTA AL CONTRIBUTO DI PIU' FLUSSO AFFERENTI NEL NODO 
C GZ (IV), DI CUI (IND) E' SOLO UNO.
cgpe-tout                    temptj(istaz) = tmand_stat(pg)
                    touttz(istaz) = tmand_stat(pg)
                    if (flag_serie) then
cgpe-tout                      temptj(istaz) = tmand_stat(pg_app)
                      touttz(istaz) = tmand_stat(pg_app)
                    endif
                    if (fl_ass) then
cgpe-tout                      temptj(jstaz) = tmand_stat(pg_ass)
                      touttz(jstaz) = tmand_stat(pg_ass)
                    endif
                  endif
                elseif (fl_ris_ce(pg) .eq. -1) then
c Viene settato il flag fl_fatal con l'errore in uscita dalla simulazione
                  if (tipo_criterio(pg).eq.crit_man) then
                    fl_fatal(pg) = fl_ris_ce(pg)
                  else
                    fl_fatal(pg) = -err(pg)
                  endif
                  if (fl_ass) then
cgpe-29-08-06
                   if (set_flag(pg_ass) .eq. 1) then
cgpe-29-08-06-end
                    if (tipo_criterio(pg_ass).eq.crit_man) then
                      fl_fatal(pg_ass) = fl_ris_ce(pg_ass)
                    else
                      fl_fatal(pg_ass) = -err(pg_ass)
                    endif
cgpe-29-08-06
                   endif
cgpe-29-08-06-end
                  else
                    if (flag_serie) then
                      if (tipo_criterio(pg_app).eq.crit_man) then
                        fl_fatal(pg_app) = fl_fatal(pg)
                      else
                        fl_fatal(pg_app) = -err(pg_app)
                      endif
                    endif
                  endif
                  if (fl_cen_punto_ko(pg)) then
c ottimo senza soluzione, ma con il calcolo del punto ko
cgpe-29-08-06
                   if (set_flag(pg) .eq. 1) then
cgpe-29-08-06-end
                    fl_fatal(pg) = -10001
                    if (flag_serie) fl_fatal(pg_app) = fl_fatal(pg)
cgpe-29-08-06
                   endif
cgpe-29-08-06-end
                  endif
                  if (fl_ass) then
cgpe-29-08-06
                   if (set_flag(pg_ass) .eq. 1) then
cgpe-29-08-06-end
                    if (fl_cen_punto_ko(pg_ass)) then
                      fl_fatal(pg_ass) = -10001
                    endif
cgpe-29-08-06
                   endif
cgpe-29-08-06-end
                  endif
                endif
              endif
              if (fl_ris_ce(pg) .ge. 0) then
cgpe-29-08-06
               if (set_flag(pg) .eq. 1) then
cgpe-29-08-06-end
                fl_fatal(pg) = fl_fatal(pg) + soglia_vio_vinc(pg)
cgpe-29-08-06
               endif
cgpe-29-08-06-end
              endif
              if (fl_ass) then
cgpe-29-08-06
               if (set_flag(pg_ass) .eq. 1) then
cgpe-29-08-06-end
                if (fl_ris_ce(pg_ass) .ge. 0) then
                  fl_fatal(pg_ass) = fl_fatal(pg_ass) +
     *                               soglia_vio_vinc(pg_ass)
                endif
cgpe-29-08-06
               endif
cgpe-29-08-06-end
cmar_ass_cond_01_07_2013
      if (flag_ass_cond_p(pg) ) then
cgpe-29-08-06-end
                

            	if (fl_ris_ce(pg) .ge. 0) then
cgpe-29-08-06
                   if (set_flag(pg) .eq. 1) then

                   fl_fatal(pg_ass) = fl_fatal(pg_ass) +
     *                               soglia_vio_vinc(pg_ass)
                   endif
cgpe-29-08-06
              endif
      endif


cmar_ass_cond_01_07_2013

              else
                if (flag_serie) then
                    fl_fatal(pg_app) = fl_fatal(pg_app) +
     *                                 soglia_vio_vinc(pg_app)
                endif
              endif
            else
c
            endif   ! if (l_simula)

c-din
            if (fl_din) then
              if (l_simula) then
                if (kflag .ge.0) then
                  if (fl_ris_ce(pg) .gt. 0) kflag = 1
                  if (fl_ris_ce(pg) .lt. 0) kflag = -1
                endif
                if (flag_serie .and. kflag .ge.0) then
c                  kflag = fl_ris_ce(pg_app)
                  if (fl_ris_ce(pg_app) .gt. 0) kflag = 1
                  if (fl_ris_ce(pg_app) .lt. 0) kflag = -1
                endif
              else
                if (kflag .ge.0) then
                  call set_stato_stazione(setflag,istaz)
                  if (set_flag(pg) .eq. setflag) then
c centrale spenta ok
                    kflag = 0
c?                    if (flag_serie) then
c?                      if (set_flag(pg_app) .eq. 0) then
c?                        kflag = 0
c?                      endif
c?                    endif
                  else
c centrale spenta con condizioni ko
                    kflag = -1
                  endif
                endif
              endif
            endif
c-din-end
cgpe-new
c per la rete: calcolo della temperatura di uscita della centrale
c Si può eliminare tale calcolo nei sottocasi sopra?
            if (.not. fl_sim) then
              if ((l_simula .and. fl_ris_ce(pg).ge.0) .or.
     *            .not.l_simula                           ) then
cgpe-tout                temptj(istaz) = tmand_stat(pg)
                touttz(istaz) = tmand_stat(pg)
                if (flag_serie) then
cgpe-tout                  temptj(istaz) = tmand_stat(pg_app)
                  touttz(istaz) = tmand_stat(pg_app)
                endif
                if (fl_ass) then
cgpe-tout                  temptj(jstaz) = tmand_stat(pg_ass)
                  touttz(jstaz) = tmand_stat(pg_ass)
                endif
              endif
            endif
cgpe-new-end

c          else    ! errore dalla check_stat
cc flag_fatal <> 0 => fl_fatal <> 0
c            if (flag_fatal .gt. 0) then
cc caso di errore recuperabile
c              fl_ris_ce(pg) = 0
c            elseif (flag_fatal .lt. 0) then
cc caso di errore non recuperabile
c              fl_ris_ce(pg) = -1
c            endif
c          endif
          if (.not. fl_sim) then
c memorizzazione della differenza valsjc e consty per la convergenza del consumo
c Per le centrali con flag ok, viene fatta la differenza effettiva
c mentre per quelle ko, i due valori dovrebbe coincidere
            app_cons = abs(valsjc(istaz-oice)-consty(istaz))
            sum_cons = max(sum_cons,app_cons)
      
            if (fl_ass) then
              app_cons = abs(valsjc(jstaz-oice)-consty(jstaz))
              sum_cons = max(sum_cons,app_cons)
            endif
          endif
c flag per indicare che la centrale istaz e l'eventuale jstaz sono state già trattate
          fl_simu(pg) = .true.
          if (fl_ass) then
            fl_simu(pg_ass) = .true.
          else
            if (flag_serie) fl_simu(pg_app) = .true.
          endif
cstaz-corr-rec
c-old          if (l_rec_ce) return
          if (l_rec_ce) then
            goto 1000
          endif
cstaz-corr-rec-end
        endif  ! if (.not.fl_simu(pg))

100     continue

      enddo  ! ciclo su tutte le centrali

c ripristino i valori dello unit_avail da cin
      if (fl_din) then
        do istaz = otipti(e_ce), etipti(e_ce)
          call ripristina_cin (istaz)
        enddo
      endif

1000  return

      return
      END ! sim_cent

      Subroutine s_cnt_new(istaz,jstaz)

!- Per la centrale con indice istaz:
!  - viene calcolato il punto di lavoro simulato, nel caso di ripartizione manuale del carico
!  oppure
!  - viene calcolato il punto di lavoro ottimo
!- Viene restituito un indice di errore

*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/flag.INC'
       	include '../inc/ASS_COND.INC'

	

      integer*4   istaz,jstaz    ! I) jstaz può essere anche I_UND

      integer*4   ind_man,ind_ott
      logical*2   fl_cor_save



	

cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN


!---------------------------------------------------------


cmar_DIN
      am = istaz
cmar_DIN

cgpe      if (tipo_criterio(pgrato(istaz)).eq.crit_man) then
cgpec gestione manuale della portata
cgpe        if (fl_sim) then
cgpe          call gest_error (2,0,'S_CNT_NEW',
cgpe     *                   'Ripartizione manuale della portata',0)
cgpe        endif
cgpe        fl_opt = .false.

cgpe        fl_cor_save = fl_cor
cgpe        fl_cor = .false.
cgpevvvv        call simula_cent_all (istaz,jstaz)
cgpe        fl_cor = fl_cor_save

cgpe      else
cgpec simulazione della centrale fatta tramite l'algoritmo dell'ottimo
cgpe        if (fl_sim) then
cgpe          call gest_error (3,0,'S_CNT_NEW','Simulazione di centrale',0)
cgpe        endif
cgpe        fl_opt = .true.
cgpe        if (fl_sim) then
cgpe          fl_cor = .true.
cgpe        endif
cgpe        call ottimo_cent_all (istaz,jstaz)
cgpe      endif

      if (jstaz .eq. I_UND) then
        if (tipo_criterio(pgrato(istaz)).eq.crit_man) then
c gestione manuale della portata
          fl_opt = .false.
          fl_cor_save = fl_cor
          fl_cor = .false.
          call simula_cent_all (istaz,jstaz)
          fl_cor = fl_cor_save

        else
c simulazione della centrale fatta tramite l'algoritmo dell'ottimo
          fl_opt = .true.
          if (fl_sim) fl_cor = .true.
          call ottimo_cent_all (istaz,jstaz)
        endif
      else
        if (tipo_criterio(pgrato(istaz)) .eq.
     *      tipo_criterio(pgrato(jstaz))     ) then
          if (  (tipo_criterio(pgrato(istaz)).eq.crit_man     .AND.
     *      (  .not. CRIT_MAN_(ISTAZ))  )  ) then
c gestione manuale della portata per entrambi gli assetti
            fl_opt = .false.
            fl_cor_save = fl_cor
            fl_cor = .false.
            call simula_cent_all (istaz,jstaz)
            fl_cor = fl_cor_save

          else
c simulazione della centrale fatta tramite l'algoritmo dell'ottimo per entrambi gli assetti
            fl_opt = .true.
            if (fl_sim) fl_cor = .true.
            call ottimo_cent_all (istaz,jstaz)
          endif
        else
c caso di criterio misto
CMAR_CRIT_MAN
CMAR_CRIT_MAN          if (tipo_criterio(pgrato(istaz)).eq.crit_man) then
           if (      (tipo_criterio(pgrato(istaz)).eq.crit_man)   .AND.
     *              (  .not. CRIT_MAN_(ISTAZ)) )then
            ind_man = istaz
            ind_ott = jstaz
          else
            ind_man = jstaz
            ind_ott = istaz
          endif

          fl_opt = .true.
          if (fl_sim) fl_cor = .true.
          call ottimo_cent_all (ind_ott,I_UND)

          fl_opt = .false.
          fl_cor_save = fl_cor
          fl_cor = .false.
          call simula_cent_all (ind_man,I_UND)
          fl_cor = fl_cor_save

        endif

      endif
c
      return
      END ! S_CNT_NEW

      Subroutine setta_cin (istaz,jstaz)
C********************************************************************************
C
C   Assegnazione delle condizioni iniziali necessarie per la simulazione
C   
C   Nel caso della rete, inizializza le condizioni iniziali necessarie
C   per la centrale:
C   - portata uscente:              porttj(ind)
C   - pressione entrante e uscente: prestj(im)/pasp_stat, prestj(iv)/pmand_stat
C   - temperatura di aspirazione:   temptj(im)/tasp_stat
C   - temperatura dell'aria:        taria(k)
C
C   In particolare per una centrale:
C
C   - cen binaria:              prestj(im)  ==> pasp_stat(ivert)
C                               prestj(iv)  ==> pmand_stat(ivert)
C                               temptj(im)  ==> tasp_stat(ivert)
C                               porttj(ind) ==> flow_stat(ivert)
C
C   - cen unaria in stoccaggio: prestj(im)  ==> pasp_stat(ivert)
C                                               pmand_stat(ivert)
C                               temptj(im)  ==> tasp_stat(ivert)
C                               porttj(ind) ==> flow_stat(ivert)
C
C   - cen unaria in estrazione:                 pasp_stat(ivert)
C                               prestj(iv)  ==> pmand_stat(ivert)
C                                               tasp_stat(ivert)
C                               porttj(ind) ==> flow_stat(ivert)
C
C   Le grandezze della centrale calcolate dalla simulazione sono:
C   - il consumo (tot_cons(ivert))
C   - la temperatura di uscita (tmand_stat(ivert))
c***********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/cap_bon_prv.inc'

      INTEGER*4 istaz,jstaz     ! I)

      INTEGER*4 pg,pg_app
      logical*2 ce_serie
      external  ce_serie
!------------------------------------------------------------
!-centrale istaz
      pg = pgrato(istaz)
      tot_cons(pg) = 0.
      if (.not.ce_serie(istaz)) then
        if (htipto(opumto(istaz)) .eq. e_pu) then
          pasp_stat(pg) = prestj(opumto(istaz))
          tasp_stat(pg) = temptj(opumto(istaz))
        endif
        if (htipto(opuvto(istaz)) .eq. e_pu) then
          pmand_stat(pg) = prestj(opuvto(istaz))
        endif
        flow_stat(pg) = porttj(istaz)
      else
c servizio di prima fase
        pasp_stat(pg) = prestj(opumto(istaz))
        tasp_stat(pg) = temptj(opumto(istaz))
        flow_stat(pg) = porttj(istaz)

cgpe-corr
        pmand_stat(pg) = prestj(opumto(istaz)) +
     *            abs(prestj(opuvto(istaz))-prestj(opumto(istaz)))/2.
        tmand_stat(pg) = temptj(opumto(istaz)) +
     *            abs(temptj(opuvto(istaz))-temptj(opumto(istaz)))/2.
cgpe-corr-end

c servizio di seconda fase
        pg_app = pgrato_app(istaz)
        pmand_stat(pg_app) = prestj(opuvto(istaz))
        flow_stat(pg_app) = porttj(istaz)
cgpe-corr
        pasp_stat(pg_app) = pmand_stat(pg)
        tasp_stat(pg_app) = tmand_stat(pg)
cgpe-corr-end
      endif

      if (jstaz .ne. i_und) then
!-eventuale centrale jstaz.
!-Nota: per le centrali in serie, jstaz = I_UND
        pg = pgrato(jstaz)
        tot_cons(pg) = 0.
        if (htipto(opumto(jstaz)) .eq. e_pu) then
          pasp_stat(pg) = prestj(opumto(jstaz))
          tasp_stat(pg) = temptj(opumto(jstaz))
        endif
        if (htipto(opuvto(jstaz)) .eq. e_pu) then
          pmand_stat(pg) = prestj(opuvto(jstaz))
        endif
        flow_stat(pg) = porttj(jstaz)
      endif
c
      return
      END ! setta_cin

      Subroutine setta_stato_tbg (istaz,jstaz,fl_sim_istaz,fl_sim_jstaz)
C********************************************************************************
C
C   Nel caso della rete in dinamica, inizializza lo stato dei tbg (unit_avail)
C   in base al valore dell'attivazione dei tbg
C
c***********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/units.inc'

      INTEGER*4 istaz,jstaz     ! I)

      INTEGER*4 pg,pg_app
      INTEGER*4 i,iu,ju
      INTEGER*4 kk

      INTEGER*4 fl_sim_istaz,fl_sim_jstaz

      logical*2 ce_serie,ce_avail
      external  ce_serie,ce_avail
!------------------------------------------------------------
!-centrale istaz
      pg = pgrato(istaz)
      iu = first_unit(pg)
      fl_sim_istaz = 1
      fl_sim_jstaz = 1

c      if (.not.ce_serie(istaz)) then
c centrale bipolare o assetto parallelo o stadio1 di un assetto serie
      do i = 1, unit_num(pg)

        lstatus(iu+i-1) = OFF

        unit_avail(iu+i-1) = 0
        if (unit_attiv_tlc(iu+i-1) .eq. on) then
          unit_avail(iu+i-1) = 2
        endif
      enddo

      if (.not. ce_avail(pg)) then
c non ci sono unità accese (per la dinamica la routine viene chiamata solo
c per delta_p effettivi)
        fl_sim_istaz = 0
      endif
      if (ce_serie(istaz)) then
c stadio2
        pg_app = pgrato_app(istaz)
        ju = first_unit(pg_app)
        do i = 1, unit_num(pg_app)

          lstatus(ju+i-1) = OFF

          unit_avail(ju+i-1) = 0
          if (unit_attiv_tlc(ju+i-1) .eq. on) then
            unit_avail(ju+i-1) = 2
          endif
        enddo
        if (.not. ce_avail(pg_app)) then
c non ci sono unità accese (per la dinamica la routine viene chiamata solo
c per delta_p effettivi)
          fl_sim_istaz = 0
        endif

      endif

      if (jstaz .ne. i_und) then
!-eventuale centrale jstaz. Nota: per le centrali in serie, jstaz = I_UND
        pg = pgrato(jstaz)
        ju = first_unit(pg)
        do i = 1, unit_num(pg)

          lstatus(ju+i-1) = OFF

          unit_avail(ju+i-1) = 0
          if (unit_attiv_tlc(ju+i-1) .eq. on) then
            unit_avail(ju+i-1) = 2
          endif
        enddo
        if (.not. ce_avail(pg)) then
c non ci sono unità accese (per la dinamica la routine viene chiamata solo
c per delta_p effettivi)
          fl_sim_jstaz = 0
        endif

      endif
c
      return
      END ! setta_stato_tbg

      Subroutine ripristina_cin (istaz)
C********************************************************************************
C
C   Nel caso della rete in dinamica, ripristina lo stato dei tbg (unit_avail)
C
c***********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/units.inc'

      INTEGER*4 istaz        ! I)

      INTEGER*4 pg,pg_app
      INTEGER*4 i,iu

      logical*2 ce_serie
      external  ce_serie
!------------------------------------------------------------
      pg = pgrato(istaz)
      iu = first_unit(pg)
      do i = 1, unit_num(pg)
        unit_avail(iu+i-1) = unit_avail_cin(iu+i-1)
      enddo

      if (ce_serie(istaz)) then
        pg_app = pgrato_app(istaz)
        iu = first_unit(pg_app)
        do i = 1, unit_num(pg_app)
          unit_avail(iu+i-1) = unit_avail_cin(iu+i-1)
        enddo
      endif
c
      return
      END ! ripristina_cin

      Subroutine verifica_limiti_ce (i_rec,istaz,
     *           pasp_rich,pman_rich,flow_rich,cons_rich,ivar)

!- Viene trattata solo una centrale / assetto:
!  - viene calcolato il punto di lavoro ottimo, con il recupero della/e variabile/i
!    che modificano meno le
!- Viene restituito:
!  - un flag associato alla centrale:
!            = 0  => centrale ok
!            = 1  => errore di tipo warning
!            = -1 => errore di tipo fatal
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/tz.inc'
      include '../inc/jc.inc'
      include '../inc/nw.inc'
      include '../inc/stations.inc'
      include '../inc/flag.INC'
      include '../inc/messag.INC'
      include '../inc/cap_bon_prv.INC'

      integer*4  i_rec,istaz       ! I)
                  ! i_rec = 0 non viene chiamata la calc_limiti_ce
      
      real*4     pasp_rich   ! O)
     *          ,pman_rich   ! O)
     *          ,flow_rich   ! O)
     *          ,cons_rich   ! O)
      integer*4  ivar        ! O)

      integer*4  jstaz,pg,pg_app,kflag,ncorr,ier,setflag
     *          ,ivert      
      INTEGER*4  fl_sim_istaz,fl_sim_jstaz

      logical*2  l_simula,flag_serie
      logical*2  ce_serie
      external   ce_serie
      real*4     soglia_vio_vinc
      external   soglia_vio_vinc

      logical*2  check_pman_ok
      external   check_pman_ok
c----------------------------------------------------------------
      pg = pgrato(istaz)
      pg_app = pgrato_app(istaz)

c Inizializzazioni
c-din-corr      ncorr= 30
      ncorr= 20
      call deapp_mess (pg)
      fl_simu(pg) = .false.
      fl_cen_punto_ko(pg) = .false.
      pasp_rich = R_MAX
      pman_rich = R_MAX
      flow_rich= R_MAX
      cons_rich= R_MAX
      ivar = 0

c flag_serie = .T. se istaz è una centrale in serie
      flag_serie = ce_serie(istaz)
      if (flag_serie) then
        call deapp_mess (pg_app)
        fl_simu(pg_app) = .false.
        fl_cen_punto_ko(pg_app) = .false.

      endif

c simulo solo la centrale istaz, anche se esiste l'assetto coniugato
      jstaz = I_UND 

c simulazione della centrale in rete, vengono settate le condizioni imposte dalla rete
      call setta_cin (istaz,jstaz)
      if (fl_din) then
        call setta_stato_tbg (istaz,jstaz,fl_sim_istaz,fl_sim_jstaz)
        if (istaz.ne.I_UND .and. fl_sim_istaz .eq. 0 .or.
     *      jstaz.ne.I_UND .and. fl_sim_jstaz .eq. 0    ) then
          cons_rich = R_MAX
          ivar = -1
          return
        endif
      endif

c controllo la simulabilità della centrale (ovvero che non sia spenta o con
c condizioni non fattibili)
      call check_ce (istaz,jstaz,l_simula)
c l_simula=.T. se almeno uno degli assetti ha condizioni ok per la simulazione
      if (l_simula) then
        call s_cnt_new(istaz,jstaz)
c In uscita dalla subroutine ottimo_cent fl_ris_ce assume i seguenti valori:
c = 0, se è stata trovata una configurazione ottima dalla calc_ottimo
c = 1, se è stata trovata una configurazione ottima in seguito a recupero
c = -1, se non è stata trovata alcuna soluzione
c-din-corr
        kflag = 0
c-din-corr-end
        if (fl_ris_ce(pg) .ne. 0) then
c-din-corr          kflag = 0
          kflag = fl_ris_ce(pg)
          if (flag_serie .and. kflag .ge.0) then
            kflag = fl_ris_ce(pg_app)
          endif
        else
c simulazione ok
          cons_rich = tot_cons(pg)
          if (flag_serie) then
            cons_rich = cons_rich + tot_cons(pg_app)
          endif
        endif
      else
       if (check_pman_ok (istaz,pg,pg_app)) then
        kflag = 0
        call spegni_tbg (istaz)
        call set_stato_stazione (setflag,istaz)
        if (set_flag(pg) .eq. setflag) then
c centrale spenta ok
          cons_rich= 0.
        endif
       else
        kflag = -1
        call set_pman_max(istaz,pg,pg_app)
       endif
      endif
      if (kflag .ne. 0) then
       if (i_rec .eq. 1) then
        call calc_limiti_ce (istaz,pg,ncorr,
     *       pasp_rich,pman_rich,flow_rich,cons_rich,ivar,ier)
       endif
      endif

c gestione dell'errore ier ???

c ripristino i valori dello unit_avail da cin
      if (fl_din) call ripristina_cin (istaz)

      return
      END ! verifica_limiti_ce

      Subroutine spegni_tbg (istaz)
C********************************************************************************
C
C   Nel caso della rete in dinamica, inizializza lo stato dei tbg (unit_avail)
C   in base al valore dell'attivazione dei tbg
C
c***********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/units.inc'

      INTEGER*4 istaz     ! I)

      INTEGER*4 pg,pg_app,i,iu,ju

      logical*2 ce_serie
      external  ce_serie
!------------------------------------------------------------
!-centrale istaz
      pg = pgrato(istaz)
      iu = first_unit(pg)
c      if (.not.ce_serie(istaz)) then
c centrale bipolare o assetto parallelo o stadio1 di un assetto serie
      do i = 1, unit_num(pg)
        lstatus(iu+i-1) = OFF
c        unit_attiv_tlc(iu+i-1) = off
      enddo
      if (ce_serie(istaz)) then
c stadio2
        pg_app = pgrato_app(istaz)
        ju = first_unit(pg_app)
        do i = 1, unit_num(pg_app)
          lstatus(ju+i-1) = OFF
c          unit_attiv_tlc(ju+i-1) = off
        enddo
      endif
c
      return
      END ! spegni_tbg

      real*4 function cons_cen (istaz,pasp,pman,flow)
!------------------------------------------------------------
c   Calcolo del consumo di una centrale semplificata e di una
c   centrale completa
!------------------------------------------------------------
      implicit none

	include '../inc/param.inc'
	include '../inc/ti.inc'
	include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/flag.inc'

      integer*4 istaz            ! I)
      real*4    pasp,pval,flow   ! I)

      integer*4 setflag,pg,pg_app,jstaz
      INTEGER*4 fl_sim_istaz,fl_sim_jstaz

      real*4    pmon,pman,consumo,dq,dpa2,dpv2
     *         ,pasp_save,pman_save,flow_save
      logical*2 flag_serie,l_simula
      logical*2 ce_serie
      external  ce_serie
!------------------------------------------------------------
c salvataggio
      pasp_save = prestj(opumto(istaz))
      pman_save = prestj(opuvto(istaz))
      flow_save = porttj(istaz)

      prestj(opumto(istaz)) = pasp
      prestj(opuvto(istaz)) = pman
      porttj(istaz) = flow

      cons_cen = R_MAX
      consumo = 0
      if (htipto(istaz) .eq. e_cs) then
        if (htipto(opumto(istaz)).eq.e_pu) pmon = prestj(opumto(istaz))
        if (htipto(opuvto(istaz)).eq.e_pu) pval = prestj(opuvto(istaz))
        if (pmon .le. pval) then
          call qconce(porttj(istaz),pmon,pval,istaz,
     *         consumo,dq,dpa2,dpv2)
        endif
        cons_cen = consumo
      else
c Inizializzazioni
        pg = pgrato(istaz)
        call deapp_mess (pg)
        fl_simu(pg) = .false.
        fl_cen_punto_ko(pg) = .false.

c flag_serie = .T. se istaz è una centrale in serie
        flag_serie = ce_serie(istaz)
        if (flag_serie) then
          pg_app = pgrato_app(istaz)
          call deapp_mess (pg_app)
          fl_simu(pg_app) = .false.
          fl_cen_punto_ko(pg_app) = .false.

        endif

c simulo solo la centrale istaz, anche se esiste l'assetto coniugato
        jstaz = I_UND

c simulazione della centrale in rete, vengono settate le condizioni imposte dalla rete
        call setta_cin (istaz,jstaz)
        if (fl_din) then
          call setta_stato_tbg (istaz,jstaz,fl_sim_istaz,fl_sim_jstaz)
        endif

c controllo la simulabilità della centrale (ovvero che non sia spenta o con
c condizioni non fattibili)
        call check_ce (istaz,jstaz,l_simula)
c l_simula=.T. se almeno uno degli assetti ha condizioni ok per la simulazione
        if (l_simula) then
          call s_cnt_new(istaz,jstaz)
          if (fl_ris_ce(pg) .eq. 0) then
            cons_cen = tot_cons(pg)
            if (flag_serie .and. fl_ris_ce(pg_app) .eq. 0) then
              cons_cen = cons_cen + tot_cons(pg_app)
            endif
          endif
        else
          call set_stato_stazione (setflag,istaz)
          if (set_flag(pg) .eq. setflag) then
c centrale spenta ok
            cons_cen = 0.
          endif
        endif
      endif

c ripristino dei valori
      prestj(opumto(istaz)) = pasp_save
      prestj(opuvto(istaz)) = pman_save
      porttj(istaz) = flow_save

      return
      END ! cons_cen

      logical*2 function check_pman_ok (istaz,ivert,jvert)

      implicit none
      include '../inc/param.inc'   ! parametri
c      include '../inc/th.inc'      ! common rete
c      include '../inc/tj.inc'      ! common rete
      include '../inc/staznw_par.inc'         !common centrali
      include '../inc/stazione.inc'         !common centrali
      include '../inc/stations.inc'         !common centrali

c
      integer*4 istaz    ! I) indice delle stazioni da considerare
     *         ,ivert,jvert
      logical*2 flag_serie,ce_serie
      external  ce_serie
!------------------------------------------------------------
      check_pman_ok = .true.
      flag_serie = ce_serie(istaz)
c
      if (flag_serie) then
        if (pmand_stat(jvert) .gt. stat_vars(6,jvert)+repsP) then
          check_pman_ok = .false.
        endif
      else
        if (pmand_stat(ivert) .gt. stat_vars(6,ivert)+repsP) then
          check_pman_ok = .false.
        endif
      endif

      return
      end ! check_pman_ok
c
      Subroutine set_pman_max (istaz,ivert,jvert)

      implicit none
      include '../inc/param.inc'        ! parametri
      include '../inc/staznw_par.inc'   !common centrali
      include '../inc/stazione.inc'     !common centrali
      include '../inc/stations.inc'     !common centrali
c
      integer*4 istaz    ! I) indice delle stazioni da considerare
     *         ,ivert,jvert

      real*4    delta_p
      logical*2 flag_serie,ce_serie
      external  ce_serie
!------------------------------------------------------------
      flag_serie = ce_serie(istaz)
      delta_p = 1.
c
      if (flag_serie) then
        pmand_stat(jvert) = stat_vars(6,jvert) - delta_p
      else
        pmand_stat(ivert) = stat_vars(6,ivert) - delta_p
      endif

      return
      end ! set_pman_max

      logical*2 function ce_avail_tlc (ivert)
c
      implicit none
      include '../inc/param.inc'     !parametri
      include '../inc/ti.inc'        !top connessioni
      include '../inc/stations.inc'  !centrali
      include '../inc/units.inc'     !unita' centrali

      integer*4 ivert      ! I)
      integer*4 i,iu
!------------------------------------------------------------
      ce_avail_tlc = .false.

!-una centrale viene definita disponibile se ha almeno una unità disponibile
      iu = first_unit(ivert)-1
      do i = 1, unit_num(ivert)
        if (unit_attiv_tlc(iu+i) .ge. 1) then
          ce_avail_tlc = .true.
          return
        endif
      enddo

      return
      end ! ce_avail_tlc