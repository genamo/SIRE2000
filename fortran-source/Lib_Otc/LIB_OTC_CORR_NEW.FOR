cgpe
cgpe  Creazione dell'algoritmo per il recupero delle centrali in base al tipo
cgpe  di regolazione
cgpe-end
c-bp21   Aggiunto il parametro in input ncorr, che assume valore diverso se chiamata
c        per un assetto serie (tipo Cap Bon)
cgpe      Subroutine calc_corr (istaz,pg,ier)
      Subroutine calc_corr (istaz,pg,ncorr,ier)
c-bp21-end
*&*
cgpe
cgpe Correzione del punto di lavoro: RECUPERO DEL SECONDO TIPO
cgpe CASI da prevedere:
cgpe c1) centrale binaria
cgpe c2) un assetto
cgpe c3) due assetti (in parallelo)
cgpe     c3a) non condividono collettori nè in asp nè in mand
cgpe     c3b) condividono il collettore in asp
cgpe          c3b1) hanno lo stesso tipo di regolazione
cgpe          c3b2) non hanno lo stesso tipo di regolazione
cgpe     c3c) condividono il collettore in mand
cgpe          c3c1) hanno lo stesso tipo di regolazione
cgpe          c3c2) non hanno lo stesso tipo di regolazione
cgpe c4) due assetti in serie (tipo Cap Bon)
cgpe
cgpe IMPOSTAZIONE DELL'ALGORITMO:
cgpe a) Riconoscimento del caso in cui non viene fatto il recupero (si esce):
cgpe    a1) criterio di ripartizione manuale del carico
cgpe    a2) due assetti attivi con unità disponibili in entrambi gli assetti
cgpe b) viene chiamata la simulazione di centrale settando tutte le unità ad ON
cgpe    quelle necessariamente accese e disponibili.
cgpe    Settare il valore della variabili IOPT = F così si hanno le opportune
cgpe    correzioni variando pasp_stat, pmand_stat (==> ITER = 1) e la messagistica
cgpe    si adegua.
crsa
crsa  Il recupero consiste nel modificare le condizione iniziali prima di rilanciare
crsa  l'ottimo. Gli assetti compatibili saranno "recuperati" in modo separato e
crsa  quindi non sarà possibile trattare assetti con macchine disponibili in
crsa  comune nel qual caso sarà emesso un messaggio di errore; comunque nel caso
crsa  particolare di due assetti compatibili [senza unità in comune] con un collettore
crsa  in comune sul quale vengono imposte contemporaneamente le due grandezze da
crsa  "grigliare", sarà considerata una sola griglia (quella con il passo più grande)
crsa  e la soluzione trovata dovrà essere fattibile per entrambi gli assetti.
crsa Il recupero viene effettuato modificando una sola tra le grandezze imposte
crsa  scegliendola secondo il seguente ordine: Pressione valle, Pressione monte,
crsa  Portata. Ex: SetQ->Portata, SetPvQ->Pressione valle, SetPm->Pressione monte.
crsa  La modifica della grandezza avviene in un intorno del valore imposto nel rispetto
crsa  dei vincoli; la griglia proposta si compone al massimo di 10 valori, 5 al di
crsa  sopra e 5 al di sotto del valore imposto, con passo uguale al 3% del delta di
crsa  pressione iniziale per le pressioni oppure al 5% della portata iniziale per
crsa  le portate. Quando non esistono grandezze imposte il recupero non viene effettuato.
cgpe-end
cgpe  Viene chiamata la imp_new_sta per passare alla rete i valori modificati
cgpe  hstatj e valsjc.

*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/flag.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/otc.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/messag.inc'
c-UDM
      include '../inc/conv.inc'
c-UDM-end
c
      INTEGER*4 istaz(max_serv)   ! indice della stazione
c istaz(2) ! indice della eventuale stazione da trattare, può essere I_UND
      INTEGER*4 ier               ! O) indice di errore


      INTEGER*4 ncorr         ! numero di tentativi
cgpe      parameter (ncorr = 24)  ! (ncorr = 10)
	real*4    val_corr(ncorr),delta_var_corr,val_corr_app(ncorr)
      REAL*4    val_save,p_app,valore_corr,val_ini
	real*4    delpr_in,delpr_out
      INTEGER*4 pg(max_serv),pgi,pgj,i,iu,ju,ii1,jj1,ker,unit_sim,ic,
     *          ind_c,iserv
      INTEGER*4 j,jk,kk,i1
      logical*2 ce_serie
      external  ce_serie

	real*4    val_corr_int(ncorr),val_save_int
      INTEGER*4 i_est,i_int,ind_c_int,ncorr_int
      logical*2 punto_com,l_punto_com_asp,punto_com_asp,punto_com_man
      external  punto_com,l_punto_com_asp,punto_com_asp,punto_com_man
      REAL*4    tot_cons_min,cons_ass(2)
      REAL*4    pasp_stat_save(max_serv),pmand_stat_save(max_serv),
     *          flow_stat_save(max_serv)
cgpe-staz      REAL*4    pasp_stat_save(max_vert),pmand_stat_save(max_vert),
cgpe-staz     *          flow_stat_save(max_vert)
      REAL*4    max_pout_app,flow_prog_app
      character*2  tip_corr,tip_corr_int
      character*(max_len) mess/' '/
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end
cgpe-corr
      INTEGER*4 ier1,ier2
cgpe-corr-end
      real*4    dp_in,dp_out
      external  dp_in,dp_out
cgpe-corr-staz
      INTEGER*4 num_ass_attivi
cgpe-corr-staz-end
c-------------------------------------------------------------------------
!-inizializzazioni
      ier = 0
      ncorr_int = ncorr

c-bp08
c salvataggio dei dati di input (comprende il trattamento della centrale in serie ???)
      if (.not.ce_serie(istaz(1))) then
c per una centrale bipolare e un assetto di MF o due assetti
        do iserv = 1, num_servizi
          pgi = pg(iserv)
cgpe-staz          flow_stat_save(pgi)  = flow_stat(pgi)
cgpe-staz          pasp_stat_save(pgi)  = pasp_stat(pgi)
cgpe-staz          pmand_stat_save(pgi) = pmand_stat(pgi)
          flow_stat_save(iserv)  = flow_stat(pgi)
          pasp_stat_save(iserv)  = pasp_stat(pgi)
          pmand_stat_save(iserv) = pmand_stat(pgi)
        enddo
      else
c per un assetto serie di MF
        iserv = 1
        pgi = pg(iserv)
        flow_stat_save(iserv)  = flow_stat(pgi)
        pasp_stat_save(iserv)  = pasp_stat(pgi)
        pmand_stat_save(iserv) = pmand_stat(pgi)

        iserv = 2
        pgj = pg(iserv)
        flow_stat_save(iserv)  = flow_stat(pgj)
        pasp_stat_save(iserv)  = pasp_stat(pgj)
        pmand_stat_save(iserv) = pmand_stat(pgj)
      endif
c-bp08-end

c Riconoscimento dei casi in cui non viene fatto il recupero per cui si esce:
      if (hstatj(istaz(1)).eq.sta_und) then
        ier = -1
      elseif (istaz(2).ne.I_UND) then
        if (hstatj(istaz(2)).eq.sta_und) ier = -1
      endif
      if (ier .lt. 0) then
c        msg_err = ' Tipo di regolazione indefinito'
        return
      endif


c ALGORITMO DI CORREZIONE
      if (pg(2).ne.0) then
        if (.not.ce_serie(istaz(1))) then
          iserv = 1
          pgi = pg(1)
c definizione di delta_var_corr
          pgj = pg(2)
          if (num_servizi .eq. max_serv) then
c controllo che due assetti attivi non abbiano unità disponibili in
c entrambi gli assetti
cgpe-corr-staz
c numero di assetti effettivamente attivi
           num_ass_attivi = 0
           if (set_flag(pgi) .eq. 1) num_ass_attivi = num_ass_attivi + 1
           if (set_flag(pgj) .eq. 1) num_ass_attivi = num_ass_attivi + 1
           if (num_ass_attivi .eq. max_serv) then
c il controllo sulle unità condivise non ha senso se un assetto ha set_flag=0
cgpe-corr-staz-end
            iu = first_unit(pg(1))
            ju = first_unit(pg(2))
            do i = 1, unit_num(pg(1))
              ii1=iu+i-1
              jj1=ju+i-1
              if (unit_avail(ii1).gt.0 .and. unit_avail(jj1).gt.0) then
                ier = 111
                return
              endif
            enddo
cgpe-corr-staz
           endif
cgpe-corr-staz-end
          endif

          if (punto_com(istaz,pg)) then
c le centrali istaz e jstaz hanno il collettore in comune o in aspirazione o in mandata
c e tutte e due hanno lo stesso tipo di regolazione in pressione o in aspirazione o in
c mandata (risp sta_pm o sta_pv)
c UNICO caso in cui si opera le correzione con una griglia comune per i due assetti:
c assetti compatibili, senza unità disp in comune, con un collettore in comune (asp. o mand.)
c sul quale vengono imposte contemporaneamente la grandezza da grigliare (sta_pm o
c rispettivamente sta_pv/sta_pq).
c Pertanto si ha una correzione unica solo correggendo o la pressione monte o
c la pressione valle.
c Viene chiamata la optcent_mf e la soluzione viene memorizzata in ind_u(1:unit_num) = 1 opp 2
            max_pout_app = min(stat_vars(6,pgi),stat_vars(6,pgj))
c-din-corr            flow_prog_app = min(stat_vars(2,pgi),stat_vars(2,pgj))
            flow_prog_app = R_MAX
c-din-corr-end
            if (.not.l_punto_com_asp(istaz,pg)) then
              p_app = min(pmand_stat(pgi),pmand_stat(pgj))
              call calc_range_corr (max_pout_app,flow_prog_app,
     *             hstatj(istaz(iserv)),ncorr,val_corr,
     *             delta_var_corr,pasp_stat(pgi),
     *             p_app,flow_stat(pgi),val_save,tip_corr)
            else
              p_app = min(pasp_stat(pgi),pasp_stat(pgj))
              call calc_range_corr (max_pout_app,flow_prog_app,
     *             hstatj(istaz(iserv)),ncorr,val_corr,
     *             delta_var_corr,p_app,
     *             pmand_stat(pgi),flow_stat(pgi),val_save,tip_corr)
            endif
c vengono provati i valori corretti e si esce al primo tentativo OK
            do ind_c = 1, ncorr
              if (l_punto_com_asp(istaz,pg)) then
                pasp_stat(pgi) = val_corr(ind_c)
                pasp_stat(pgj) = val_corr(ind_c)

c pmand_stat(iserv) < max_pout(iserv) da input per ogni iserv. Avendo calcolato il
c delta_var_corr prendendo il minimo fra le pressioni di mandata, la pasp_stat
c corretta è sicuramente minore delle max_pout. Se si cambierà tale filosofia,
c bisogna introdurre questi test
              else
                pmand_stat(pgi) = val_corr(ind_c)
                pmand_stat(pgj) = val_corr(ind_c)
              endif
              ier = 0
              call optcent_mf(ier)
              if (ier .eq. 0) then
                call imp_new_sta(istaz(1),pgi,val_corr(ind_c),tip_corr)
                call imp_new_sta(istaz(2),pgj,val_corr(ind_c),tip_corr)
c-ele-9/5/06 Spostata la gestione dei messaggi nella subroutine 'verstel_stat_nw'
c            caso 1 : centrale MF con 2 assetti recuperati con griglia comune
                if (fl_sim) then
                  ker = 65
cgpe-mess                  write(mess,556) val_save,val_corr(ind_c)
c                  if (hstatj(istaz(1)) .eq. sta_q) then
c-UDM                    write(mess,560) val_save,val_corr(ind_c),UDM_INT_Q
c                    write(mess,530) udm_out_str(udm_Q,val_save,0),
c     *                              udm_out_str(udm_Q,val_corr(ind_c),1)
c                  elseif (hstatj(istaz(1)) .eq. sta_pm) then
c-UDM                    write(mess,570) val_save,val_corr(ind_c),UDM_INT_P
c                    write(mess,540) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                  elseif (hstatj(istaz(1)) .eq. sta_pv) then
c-UDM                    write(mess,580) val_save,val_corr(ind_c),UDM_INT_P
c                    write(mess,550) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                  endif
c                  call app_mess(pgi,ker,mess)
c                  call app_mess(pgj,ker,mess)
                endif
                goto 11
              endif
            enddo

          elseif (punto_com_asp(istaz,pg) .or.
     *            punto_com_man(istaz,pg)     ) then
c le centrali istaz e jstaz hanno il collettore in comune in aspirazione o in mandata
c Viene fatto un ciclo esterno per la centrale che ha come tipo di regolazione la
c pressione nel collettore in comune
c Internamente viene fatto un ciclo sull'altra centrale
C Dunque per ogni valore in pressione, vengono provati n valori nell'altra variabile da
c correggere (vengono fatte tutte le combinazioni e 
c Viene chiamata la optcent_mf e la soluzione viene memorizzata in ind_u(1:unit_num) = 1 opp 2
            if (punto_com_asp(istaz,pg) .and.
     *             hstatj(istaz(1)) .eq. sta_pm .or.
     *          .not.punto_com_asp(istaz,pg) .and.
     *             hstatj(istaz(1)) .eq. sta_pv      ) then
              i_est = istaz(1)
              i_int = istaz(2)
              pgi = pg(1)
              pgj = pg(2)
            else
              i_est = istaz(2)
              i_int = istaz(1)
              pgi = pg(2)
              pgj = pg(1)
            endif
            max_pout_app  = stat_vars(6,pgi)
c-din-corr            flow_prog_app = stat_vars(2,pgi)
            flow_prog_app = R_MAX
c-din-corr-end
            call calc_range_corr (max_pout_app,flow_prog_app,
     *           hstatj(i_est),ncorr,val_corr,
     *           delta_var_corr,pasp_stat(pgi),pmand_stat(pgi),
     *           flow_stat(pgi),val_save,tip_corr)
            if (set_flag(pgj) .ne. 0) then
              max_pout_app  = stat_vars(6,pgj)
c-din-corr              flow_prog_app = stat_vars(2,pgj)
              flow_prog_app = R_MAX
c-din-corr-end
              call calc_range_corr (max_pout_app,flow_prog_app,
     *             hstatj(i_int),ncorr_int,
     *             val_corr_int,
     *             delta_var_corr,pasp_stat(pgj),pmand_stat(pgj),
     *             flow_stat(pgj),val_save_int,tip_corr_int)
            else
              ncorr_int = 1
              if (hstatj(i_est) .eq. sta_pm) then
                val_save_int = pasp_stat(pgj)
              elseif (hstatj(i_est) .eq. sta_pv) then
                val_save_int = pmand_stat(pgj)
              endif

            endif
c vengono provati i valori corretti e si esce al primo tentativo OK
            do ind_c = 1, ncorr
! ciclo esterno con correzione in pressione sul collettore in comune
              if (punto_com_asp(istaz,pg)) then
                pasp_stat(pgi) = val_corr(ind_c)
                pasp_stat(pgj) = pasp_stat(pgi)
c                pasp_stat(pgj) = val_corr_int(ind_c)
              else
                pmand_stat(pgi) = val_corr(ind_c)
                pmand_stat(pgj) = pmand_stat(pgi)
              endif

              do ind_c_int = 1, ncorr_int
! ciclo interno
                if (hstatj(i_int) .eq. sta_q) then
                  flow_stat(pgj) = val_corr_int(ind_c_int)
                elseif (hstatj(i_int) .eq. sta_pm) then
                  pasp_stat(pgj) = val_corr_int(ind_c_int)
                elseif (hstatj(i_int) .eq. sta_pv) then
                  pmand_stat(pgj) = val_corr_int(ind_c_int)
                else
                  if (set_flag(pgj) .eq. 0) then
                    if (hstatj(i_est) .eq. sta_pm) then
                      pmand_stat(pgj) = pasp_stat(pgj)
                    elseif (hstatj(i_est) .eq. sta_pv) then
                      pasp_stat(pgj) = pmand_stat(pgj)
                    endif
                  endif
                endif

                ier = 0
                call optcent_mf(ier)
                if (ier .eq. 0) then
                  call imp_new_sta(i_est,pgi,val_corr(ind_c),
     *                 tip_corr)
                  if (set_flag(pgj) .ne. 0) then
                    call imp_new_sta(i_int,pgj,val_corr_int(ind_c_int),
     *                   tip_corr_int)
                  else
                    call imp_new_sta(i_int,pgj,val_corr(ind_c),
     *                   tip_corr)
                  endif
c-ele-9/5/06 Spostata la gestione dei messaggi nella subroutine 'verstel_stat_nw'
c            caso 2 : centrale MF con 2 assetti con collettore comune ma diversa 
c                     regolazione
                  if (fl_sim) then
                    ker = 65
cgpe-mess                    write(mess,556) val_save,val_corr(ind_c)
c                    if (hstatj(i_est) .eq. sta_q) then
c-UDM                      write(mess,560) val_save,val_corr(ind_c),UDM_INT_Q
c                      write(mess,530) udm_out_str(udm_Q,val_save,0),
c     *                              udm_out_str(udm_Q,val_corr(ind_c),1)
c                    elseif (hstatj(i_est) .eq. sta_pm) then
c-UDM                      write(mess,570) val_save,val_corr(ind_c),UDM_INT_P
c                      write(mess,540) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                    elseif (hstatj(i_est) .eq. sta_pv) then
c-UDM                      write(mess,580) val_save,val_corr(ind_c),UDM_INT_P
c                      write(mess,550) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                    endif
c                    call app_mess(pgi,ker,mess)
c                    if (set_flag(pgj) .ne. 0) then
cgpe-mess                      write(mess,556) val_save_int,val_corr_int(ind_c_int)
c                      if (hstatj(i_int) .eq. sta_q) then
c-UDM                       write(mess,560) val_save_int,val_corr_int(ind_c),UDM_INT_Q
c                        write(mess,530)
c     *                        udm_out_str(udm_Q,val_save_int,0),
c     *                        udm_out_str(udm_Q,val_corr_int(ind_c),1)
c                      elseif (hstatj(i_int) .eq. sta_pm) then
c-UDM                       write(mess,570) val_save_int,val_corr_int(ind_c),UDM_INT_P
c                        write(mess,540)
c     *                        udm_out_str(udm_P,val_save_int,0),
c     *                        udm_out_str(udm_P,val_corr_int(ind_c),1)
c                      elseif (hstatj(i_int) .eq. sta_pv) then
c-UDM                       write(mess,580) val_save_int,val_corr_int(ind_c),UDM_INT_P
c                        write(mess,550)
c     *                        udm_out_str(udm_P,val_save_int,0),
c     *                        udm_out_str(udm_P,val_corr_int(ind_c),1)
c                      endif
c                    else
cgpe-mess                      write(mess,556) val_save_int,val_corr(ind_c)
c                      if (hstatj(i_int) .eq. sta_q) then
c-UDM                        write(mess,560) val_save_int,val_corr(ind_c),UDM_INT_Q
c                        write(mess,530)
c     *                        udm_out_str(udm_Q,val_save_int,0),
c     *                        udm_out_str(udm_Q,val_corr(ind_c),1)
c                      elseif (hstatj(i_int) .eq. sta_pm) then
c-UDM                        write(mess,570) val_save_int,val_corr(ind_c),UDM_INT_P
c                        write(mess,540)
c     *                        udm_out_str(udm_P,val_save_int,0),
c     *                        udm_out_str(udm_P,val_corr(ind_c),1)
c                      elseif (hstatj(i_int) .eq. sta_pv) then
c-UDM                        write(mess,580) val_save_int,val_corr(ind_c),UDM_INT_P
c                        write(mess,550)
c     *                        udm_out_str(udm_P,val_save_int,0),
c     *                        udm_out_str(udm_P,val_corr(ind_c),1)
c                      endif
c                    endif
c                    call app_mess(pgj,ker,mess)
                  endif
                  goto 11
                endif
              enddo
            enddo

          endif
        endif
      endif

c definizione del vettore val_corr della grandezza da grigliare
c x es. se pasp_stat = 20 e pmand_stat = 60,
c allora: delta_var_corr = (60-30)/100*perc_p=0.3*3=0.9
c e:      val_corr=(1:10)= (60+0.9,60+0.9*2,...,60+0.9*5,60-0.9,60-0.9*2,...,60-0.9*5)
c
! centrale in serie
      iserv = 1
      if (ce_serie(istaz(iserv))) then
        pgi = pg(iserv)
        pgj = pg(iserv+1)
        max_pout_app  = stat_vars(6,pgj)
c-din-corr        flow_prog_app = stat_vars(2,pgi)
        flow_prog_app = R_MAX
c-din-corr-end
        call calc_range_corr (max_pout_app,flow_prog_app,
     *       hstatj(istaz(iserv)),ncorr,val_corr,
     *       delta_var_corr,pasp_stat(pgi),
     *       pmand_stat(pgj),flow_stat(pgi),val_save,tip_corr)
c vengono provati i valori corretti e si esce al primo tentativo OK
        do ind_c = 1, ncorr
          if (hstatj(istaz(iserv)) .eq. sta_q) then
            flow_stat(pgi) = val_corr(ind_c)
            delpr1(pgi) = dp_in(istaz(iserv),pasp_stat(pgi),
     *                    tasp_stat(pgi),flow_stat(pgi))
            delpr2(pgi) = dp_out(istaz(iserv),pmand_stat(pgi),
     *                    stat_vars(7,pgi),flow_stat(pgi))
cgpe-corr-new
            flow_stat(pgj) = val_corr(ind_c)
c viene usata tasp_stat(pgi) perché tasp_stat(pgj) forse non è validata
            delpr1(pgj) = dp_in(istaz(iserv),pasp_stat(pgj),
     *                    tasp_stat(pgi),flow_stat(pgj))
            delpr2(pgj) = dp_out(istaz(iserv),pmand_stat(pgj),
     *                    stat_vars(7,pgj),flow_stat(pgj))
cgpe-corr-new-end
          elseif (hstatj(istaz(iserv)) .eq. sta_pm)  then
c viene modificata la pressione di aspirazione della (prima) centrale istaz
            pasp_stat(pgi) = val_corr(ind_c)

          elseif (hstatj(istaz(iserv)) .eq. sta_pv) then
c viene modificata la pressione di mandata della (seconda) centrale jstaz
            pmand_stat(pgj) = val_corr(ind_c)
          endif
          ier = 0
          call calc_ottimo_serie (istaz(iserv),istaz(iserv+1),pgi,pgj,
     *         ier)

           if (ier .eq. 0) then
c-ele 9/5/06 Imposizione del valore corretto alla valsjc:
c           - sullo stadio 1 se sta_pm
c           - sullo stadio 2 se sta_pv
c           - sullo stadio 1 e 2 se sta_q

c-ele            call imp_new_sta(istaz(iserv),pgi,val_corr(ind_c),tip_corr)
c-staz_corr
c-staz_corr            if (fl_sim) then
c-staz_corr              ker = 65
c-staz_corr-end
cgpe-mess              write(mess,556) val_save,val_corr(ind_c)
              if (hstatj(istaz(iserv)) .eq. sta_q) then
c-UDM                write(mess,560) val_save,val_corr(ind_c),UDM_INT_Q
c                write(mess,530) udm_out_str(udm_Q,val_save,0),
c     *                          udm_out_str(udm_Q,val_corr(ind_c),1)
cgpe-corr-new
c                call app_mess(pgi,ker,mess)
c                call app_mess(pgj,ker,mess)
                call imp_new_sta(istaz(iserv),pgi,val_corr(ind_c),
     *               tip_corr)
                call imp_new_sta(istaz(iserv),pgj,val_corr(ind_c),
     *               tip_corr)
c      Spostata la gestione dei messaggi nella subroutine 'verstel_stat_nw'
c      caso 3 : macroassetto serie

cgpe-corr-new-end
              elseif (hstatj(istaz(iserv)) .eq. sta_pm)  then
                call imp_new_sta(istaz(iserv),pgi,val_corr(ind_c),
     *               tip_corr)
c-UDM                write(mess,570) val_save,val_corr(ind_c),UDM_INT_P
c                write(mess,540) udm_out_str(udm_P,val_save,0),
c     *                          udm_out_str(udm_P,val_corr(ind_c),1)
cgpe-corr-new
c                call app_mess(pgi,ker,mess)
cgpe-corr-new-end
              elseif (hstatj(istaz(iserv)) .eq. sta_pv) then
                call imp_new_sta(istaz(iserv),pgj,val_corr(ind_c),
     *               tip_corr)
cc-UDM                write(mess,580) val_save,val_corr(ind_c),UDM_INT_P
c                write(mess,550) udm_out_str(udm_P,val_save,0),
c     *                          udm_out_str(udm_P,val_corr(ind_c),1)
ccgpe-corr-new
c                call app_mess(pgj,ker,mess)
ccgpe-corr-new-end
              endif
cgpe-corr              call app_mess(pgi,ker,mess)
c-staz_corr
            if (fl_sim) then
              ker = 65
c-staz_corr-end
            endif
            goto 11
          endif
        enddo
      else
! centrale non in serie
        if (lmf) then
          fl_corr_mf = .true.
         do iserv = 1,num_servizi
          do i = 1,unit_num(pg(iserv))
            ind_u(i) = 0
          enddo
         enddo
        endif
        if (num_servizi .eq. max_serv) then
c due assetti attivi con unità disponibili in entrambi gli assetti
cgpe-corr-staz
         num_ass_attivi = 0
         if (set_flag(pg(1)) .eq. 1) num_ass_attivi = num_ass_attivi + 1
         if (set_flag(pg(2)) .eq. 1) num_ass_attivi = num_ass_attivi + 1
         if (num_ass_attivi .eq. max_serv) then
c il controllo sulle unità condivise non ha senso se un assetto ha set_flag=0
cgpe-corr-staz-end
          iu = first_unit(pg(1))
          ju = first_unit(pg(2))
          do i = 1, unit_num(pg(1))
            ii1=iu+i-1
            jj1=ju+i-1
            if (unit_avail(ii1).gt.0 .and. unit_avail(jj1).gt.0) then
              ier = 111
              return
            endif
          enddo

cgpe-corr-staz
         endif
cgpe-corr-staz-end

        endif
cgpe-corr
        ier1 = 0
        ier2 = 0
cgpe-corr-end
        do iserv = 1, num_servizi
          pgi = pg(iserv)
          if (set_flag(pgi) .ne. 0) then
c definizione di delta_var_corr
            max_pout_app  = stat_vars(6,pgi)
c-din-corr            flow_prog_app = stat_vars(2,pgi)
            flow_prog_app = R_MAX
c-din-corr-end
            call calc_range_corr (max_pout_app,flow_prog_app,
     *           hstatj(istaz(iserv)),ncorr,val_corr,
     *           delta_var_corr,pasp_stat(pgi),
     *           pmand_stat(pgi),flow_stat(pgi),val_save,tip_corr)

c vengono provati i valori corretti e si esce al primo tentativo OK
            do ind_c = 1, ncorr
              if (hstatj(istaz(iserv)) .eq. sta_q) then
                flow_stat(pgi) = val_corr(ind_c)
                delpr1(pgi) = dp_in(istaz(iserv),pasp_stat(pgi),
     *                        tasp_stat(pgi),flow_stat(pgi))
                delpr2(pgi) = dp_out(istaz(iserv),pmand_stat(pgi),
     *                        stat_vars(7,pgi),flow_stat(pgi))

              elseif (hstatj(istaz(iserv)) .eq. sta_pm)  then
                pasp_stat(pgi) = val_corr(ind_c)
              elseif (hstatj(istaz(iserv)) .eq. sta_pv) then
                pmand_stat(pgi) = val_corr(ind_c)
              endif
              ier = 0
              unit_sim = unit_num(pgi)
c settaggio di type_avail
              iu = first_unit(pgi)
              do j = 1,type_num(pgi)
                jk = first_type(pgi)+j-1
                type_avail(jk) = 0
                do kk = 1,type_quant(jk)
                  ii1 = iu+kk-1
                  if (unit_avail(ii1).ge.1) 
     *                type_avail(jk) = type_avail(jk)+1
               enddo
               iu = iu+type_quant(jk)
              enddo
cgpe La sub.ine optcent riempie il vettore ind_s(ind_opt,1:unit_num). Quindi:
cgpe - se la centrale da correggere è binaria, ok.
cgpe - se si corregge un solo assetto, o due assetti separatamente si deve passare dal vettore
cgpe   di appoggio iunit_corr al vettore ind_u(1:unit_num)
              call optcent(istaz,vert_typ(pgi),tipo_criterio(pgi),
     *pgi,flow_stat(pgi),pasp_stat(pgi),pmand_stat(pgi),
     *delpr1(pgi),delpr2(pgi),
     *tasp_stat(pgi),TARIA(pgi),unit_sim,stat_ord(pgi),first_type(pgi),
     *type_num(pgi),first_unit(pgi),type_quant(first_type(pgi)),
     *type_avail(first_type(pgi)),unit_avail(first_unit(pgi)),
     *status(first_unit(pgi)),stat_vars(1,pgi),
     *unit_start_cost(first_unit(pgi)),unit_shut_cost(first_unit(pgi)),
     *tot_cons(pgi),ier)
cgpe-corr
              if (iserv .eq. 1) then
                ier1 = ier
              elseif (iserv .eq. 2) then
                ier2 = ier
              endif
cgpe-corr-end
              if (ier .eq. 0) then
c - ele 10/5/06
c                call imp_new_sta(istaz,pgi,val_corr(ind_c),tip_corr)
                call imp_new_sta(istaz(iserv),pgi,val_corr(ind_c),
     *                           tip_corr)
                if (fl_corr_mf) then
c salvataggio nel caso di correzione del punto di due assetti separatamente
                  i1 = 0
                  iu = first_unit(pgi)
                  do j = 1,type_num(pgi)
                    jk = first_type(pgi)+j-1
                    do kk = 1,type_quant(jk)
                      ii1 = iu+kk-1
                      if (iunit_corr(j).gt.0 .and.
     *                    unit_avail(ii1).gt.0    ) then
                        ind_u(i1+kk) = iserv
                        iunit_corr(j) = iunit_corr(j) - 1
                      endif
                    enddo
                    i1 = i1+type_quant(jk)
                    iu = iu+type_quant(jk)
                  end do
                endif
                if (fl_sim) then
cgpe-corr
                  if (num_servizi .gt. 1) then
                    ker = 66
cgpe-corr
                    mess = ' '
cgpe-corr-end
                    call app_mess(pgi,ker,mess)
                  endif
cgpe-corr-end
c-ele-9/5/06 Spostata la gestione dei messaggi nella subroutine 'verstel_stat_nw'
c            caso 4 : centrale binaria ocentrale MF con 2 assetti recuperati separatamente
                  ker = 65
cgpe-mess                  write(mess,556) val_save,val_corr(ind_c)
c                  if (hstatj(istaz(iserv)) .eq. sta_q) then
c-UDM                    write(mess,560) val_save,val_corr(ind_c),UDM_INT_Q
c                    write(mess,530) udm_out_str(udm_Q,val_save,0),
c     *                              udm_out_str(udm_Q,val_corr(ind_c),1)
c                  elseif (hstatj(istaz(iserv)) .eq. sta_pm)  then
c-UDM                    write(mess,570) val_save,val_corr(ind_c),UDM_INT_P
c                    write(mess,540) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                  elseif (hstatj(istaz(iserv)) .eq. sta_pv) then
c-UDM                    write(mess,580) val_save,val_corr(ind_c),UDM_INT_P
c                    write(mess,550) udm_out_str(udm_P,val_save,0),
c     *                              udm_out_str(udm_P,val_corr(ind_c),1)
c                  endif
cgpe-corr
c                  if (iserv .eq. 1) then
c                    ier1 = ier
c                  elseif (iserv .eq. 2) then
c                    ier2 = ier
c                  endif
cgpe-corr-end
c                  call app_mess(pgi,ker,mess)
                endif
                goto 10
              endif
            enddo
            if (ier .ne. 0) then
cgpe-corr
c              if (iserv .eq. 1) then
c                ier1 = ier
c              elseif (iserv .eq. 2) then
c                ier2 = ier
c              endif
              if (num_servizi.eq.2) ier = max(ier1,ier2)
cgpe-corr-end
              goto 11
            endif
          endif

10    continue

        enddo

      endif

11    continue

c-bp08
      if (ier .ne. 0) then
c in caso di uscita con errore, ripristino dei dati di input
        if (.not.ce_serie(istaz(1))) then
          do iserv = 1, num_servizi
            pgi = pg(iserv)
cgpe-staz            pasp_stat(pgi) = pasp_stat_save(pgi)
cgpe-staz            pmand_stat(pgi) = pmand_stat_save(pgi)
cgpe-staz            flow_stat(pgi) = flow_stat_save(pgi)
            pasp_stat(pgi) = pasp_stat_save(iserv)
            pmand_stat(pgi) = pmand_stat_save(iserv)
            flow_stat(pgi) = flow_stat_save(iserv)
cgpe-corr
            if (mess_num(pgi) .gt. 0) then
              call deapp_mess(pgi)
            endif
cgpe-corr-end
          enddo 
        else
          iserv = 1
          pgi = pg(iserv)
          pasp_stat(pgi) = pasp_stat_save(iserv)
          pmand_stat(pgi) = pmand_stat_save(iserv)
          flow_stat(pgi) = flow_stat_save(iserv)
cgpe-corr
          if (mess_num(pgi) .gt. 0) then
            call deapp_mess(pgi)
          endif
cgpe-corr-end
          iserv = 2
          pgj = pg(iserv)
          flow_stat(pgj)  = flow_stat_save(iserv)
          pasp_stat(pgj)  = pasp_stat_save(iserv)
          pmand_stat(pgj) = pmand_stat_save(iserv)
cgpe-corr
          if (mess_num(pgj) .gt. 0) then
            call deapp_mess(pgj)
          endif
cgpe-corr-end
        endif
      endif
c-bp08-end

cgpe-mess556   format('Val ric: ',f8.3,' - Val proposto:',f8.3)
cgpe-corr560   format('Q: ',f8.3,' - Q proposto:',f8.3,'  ',A9)
c-UDM560   format('Q: ',f8.3,' - Q proposto: ',f8.3,'  ',A12)
c-UDM570   format('P asp: ',f8.3,' - P asp proposto: ',f8.3,'  ',A9)
c-UDM580   format('P man: ',f8.3,' - P man proposto: ',f8.3,'  ',A9)

c-UDM
530   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('p asp richiesta: ',A10,' - p asp proposta: ',A25)
550   format('p man richiesta: ',A10,' - p man proposta: ',A25)
c-UDM-end
      return
      end ! calc_corr
c
c-bp21
c-bp21      Subroutine calc_range_corr (hstatj,ncorr,val_corr,delta_var_corr,
c-bp21     *           pasp,pman,flow,val_save,tip_corr)
      Subroutine calc_range_corr (max_pout,nom_flow,
     *           hstatj,ncorr,val_corr,delta_var_corr,
     *           pasp,pman,flow,val_save,tip_corr)
c-bp21-end
c
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
c-din
      include '../inc/staznw_par.inc'
c-din-end
c
cgpe      INTEGER*4 htcitj
c-bp21
      real*4    max_pout,nom_flow
c-bp21-end
      INTEGER*4 hstatj
	INTEGER*4 ncorr   ! I/O) numero di valori entro i range da provare
	real*4    val_corr(*),delta_var_corr,pasp,pman,flow,val_save
	character*(*) tip_corr
      integer*4 ic,ind_c,num_corr
c
cgpe      if (htcitj .eq. TC_Q) then
      if (hstatj .eq. sta_q) then
c-bp21
c e' da mettere il test sul fatto che la portata da provare può essere in un range
c intorno alla portata di progetto nom_flow
c-bp21-end
        delta_var_corr = flow/100*perc_q
        delta_var_corr = max(delta_var_corr,max_deltaq)
        val_corr(1) = flow
        val_save = flow
	  tip_corr = 'Q'
cgpe      elseif (htcitj .eq. TC_PM)  then
      elseif (hstatj .eq. sta_pm)  then
        delta_var_corr = (pman-pasp)/100*perc_p
cgpe-da-vedere
        delta_var_corr = max(delta_var_corr,max_deltap)
        val_corr(1) = pasp
        val_save = pasp
	  tip_corr = 'PM'
cgpe      elseif (htcitj .eq. TC_PV  .or.
cgpe     *        htcitj .eq. TC_PQ       ) then
      elseif (hstatj .eq. sta_pv) then
        delta_var_corr = (pman-pasp)/100*perc_p
cgpe-da-vedere
        delta_var_corr = max(delta_var_corr,max_deltap)
        val_corr(1) = pman
        val_save = pman
	  tip_corr = 'PV'
      endif

      ic = ncorr/2
      val_corr(ic+1) = val_corr(1)
c-bp21
      num_corr = ncorr/2
c      do ind_c = 2, ncorr/2
      do ind_c = 2, num_corr
        if (hstatj .eq. sta_pv) then
c         val_corr(ind_c) = val_corr(1)+delta_var_corr*ind_c
          if (val_corr(1)+delta_var_corr*ind_c .le. max_pout) then
            val_corr(ind_c) = val_corr(1)+delta_var_corr*ind_c
          else
            num_corr = ncorr/2 + (ncorr/2 - ind_c + 1)
            ic = ind_c - 1
            val_corr(ic+1) = val_corr(1)
            goto 10
          endif
        elseif (hstatj .eq. sta_q) then
c controllo con nom_flow
          if (val_corr(1)+delta_var_corr*ind_c .lt. nom_flow) then
            val_corr(ind_c) = val_corr(1)+delta_var_corr*ind_c
          else
            num_corr = ncorr/2 + (ncorr/2 - ind_c + 1)
            ic = ind_c - 1
            val_corr(ic+1) = val_corr(1)
            goto 10
          endif
c-staz-corr
        elseif (hstatj .eq. sta_pm) then
c          if (val_corr(1)+delta_var_corr*ind_c .lt. pman+dp_on_ce) then
          if (val_corr(1)+delta_var_corr*ind_c .lt. pman) then
            val_corr(ind_c) = val_corr(1)+delta_var_corr*ind_c
          else
            num_corr = ncorr/2 + (ncorr/2 - ind_c + 1)
            ic = ind_c - 1
            val_corr(ic+1) = val_corr(1)
            goto 10
          endif
c-staz-corr-end
        else
          val_corr(ind_c) = val_corr(1)+delta_var_corr*ind_c
        endif
c-bp21-end
      enddo

10    continue

      val_corr(1) = val_corr(1) + delta_var_corr
c-bp21      do ind_c = 2, ncorr/2
      do ind_c = 2, num_corr
        if (hstatj .eq. sta_pv) then
c        val_corr(ic+ind_c) = val_corr(ic+1)-delta_var_corr*ind_c
          if (val_corr(ic+1)-delta_var_corr*ind_c .gt. pasp) then
            val_corr(ic+ind_c) = val_corr(ic+1)-delta_var_corr*ind_c
          else
            ncorr = ic + ind_c - 1
            goto 11
          endif
c        elseif (hstatj .eq. sta_q) then
c controllo con nom_flow
        else
          val_corr(ic+ind_c) = val_corr(ic+1)-delta_var_corr*ind_c
        endif
c-bp21-end
      enddo

11    continue

      val_corr(ic+1) = val_corr(ic+1)-delta_var_corr
c
      return
	end

      logical*2 function punto_com (istaz,ivert)

!     function punto_com = .true. se le centrali-assetti istaz e jstaz
!     aspirano o mandano sullo stesso collettore e su questo hanno
!     regolazione in pressione (risp. sta_pm o sta_pv)

      implicit none
      include '../inc/param.inc'   ! parametri
c      include '../inc/ti.inc'      ! common rete
      include '../inc/th.inc'      ! common rete
      include '../inc/tj.inc'      ! common rete
      include '../inc/top_app_ele.inc'      ! common centrale
c
      integer*4 istaz(max_serv)    ! I) indice delle stazioni da considerare
      integer*4 ivert(max_serv)    ! I) indice delle stazioni da considerare

      integer*4 iserv,im(max_serv),iv(max_serv)
      logical*2 l_punto_com
      external  l_punto_com
c
      punto_com = .false.
      do iserv = 1,max_serv
c-punto        im(iserv) = opumto(istaz(iserv))
c-bpcorr
c-bpcorr                  im(iserv) = ass_coll_asp(pgrato(istaz(iserv)))
        im(iserv) = ass_coll_asp(ivert(iserv))
c-bpcorr-end
      enddo
      if (l_punto_com(istaz,im)) then
        if (hstatj(istaz(1)) .eq. sta_pm .and.
     *      hstatj(istaz(2)) .eq. sta_pm     ) then
          punto_com = .true.
        endif
      endif

      if (.not. punto_com) then
        do iserv = 1,max_serv
c-punto          iv(iserv) = opuvto(istaz(iserv))
c-bpcorr
c-bpcorr          iv(iserv) = ass_coll_man(pgrato(istaz(iserv)))
          iv(iserv) = ass_coll_man(ivert(iserv))
c-bpcorr-end
        enddo
        if (l_punto_com(istaz,iv)) then
          if (hstatj(istaz(1)) .eq. sta_pv .and.
     *        hstatj(istaz(2)) .eq. sta_pv     ) then
            punto_com = .true.
          endif
        endif
      endif
c
      return
      end ! function punto_com

      logical*2 function l_punto_com (istaz,ipunto)

      implicit none
      include '../inc/param.inc'      !parametri
c      include '../inc/ti.inc'         !common rete
c
      integer*4 istaz(*)    ! I) indice delle stazioni da considerare
     *         ,ipunto(*)   ! I) indice del punto (monte o valle)
c
      l_punto_com = .false.
      if (ipunto(1) .eq. ipunto(2)) then
        l_punto_com = .true.
      endif
c
      return
      end ! function l_punto_com

c-bpcorr
c-bpcorr      logical*2 function l_punto_com_asp (istaz)
      logical*2 function l_punto_com_asp (istaz,ivert)
c-bpcorr-end

! l_punto_com_asp = .T. se condividono il punto in aspirazione

      implicit none
      include '../inc/param.inc'      !parametri
c      include '../inc/ti.inc'         !common rete
      include '../inc/top_app_ele.inc'         !common centrali
c
      integer*4 istaz(*)    ! I) indice delle stazioni da considerare
     *         ,ivert(*)    ! I) indice delle stazioni da considerare
c
      integer*4 im(max_serv),iserv
      logical*2 l_punto_com
      external  l_punto_com

      l_punto_com_asp = .false.
c-punto      im(1) = opumto(istaz(1))
c-punto      im(2) = opumto(istaz(2))
      do iserv = 1,max_serv
c-bpcorr
c-bpcorr        im(iserv) = ass_coll_asp(pgrato(istaz(iserv)))
        im(iserv) = ass_coll_asp(ivert(iserv))
c-bpcorr-end
      enddo

      l_punto_com_asp = l_punto_com(istaz,im)
c
      return
      end ! function l_punto_com_asp

      logical*2 function punto_com_asp (istaz,ivert)

!     punto_com_asp = .true. se le centrali-assetti istaz e jstaz
!     aspirano dallo stesso punto di rete e su questo hanno
!     regolazione in pressione sta_pm

      implicit none
      include '../inc/param.inc'   ! parametri
c      include '../inc/ti.inc'      ! common rete
      include '../inc/th.inc'      ! common rete
      include '../inc/tj.inc'      ! common rete
      include '../inc/top_app_ele.inc'         !common centrali
c
      integer*4 istaz(max_serv)    ! I) indice delle stazioni da considerare
      integer*4 ivert(max_serv)    ! I) indice delle stazioni da considerare

      integer*4 iserv,im(max_serv),iv(max_serv)
      logical*2 l_punto_com
      external  l_punto_com
c
      punto_com_asp = .false.
      do iserv = 1,max_serv
c-punto        im(iserv) = opumto(istaz(iserv))
c-bpcorr
c-bpcorr        im(iserv) = ass_coll_asp(pgrato(istaz(iserv)))
        im(iserv) = ass_coll_asp(ivert(iserv))
c-bpcorr-end
      enddo

      if (l_punto_com(istaz,im)) then
        if (hstatj(istaz(1)) .eq. sta_pm .or.
     *      hstatj(istaz(2)) .eq. sta_pm     ) then
          punto_com_asp = .true.
        endif
      endif

      return
      end ! function punto_com_asp

      logical*2 function punto_com_man (istaz,ivert)

!     punto_com_man = .true. se le centrali-assetti istaz e jstaz
!     mandano sullo stesso punto di rete e su questo hanno
!     regolazione in pressione sta_pv

      implicit none
      include '../inc/param.inc'   ! parametri
c      include '../inc/ti.inc'      ! common rete
      include '../inc/th.inc'      ! common rete
      include '../inc/tj.inc'      ! common rete
      include '../inc/top_app_ele.inc'  !common centrali
c
      integer*4 istaz(max_serv)    ! I) indice delle stazioni da considerare
      integer*4 ivert(max_serv)    ! I) indice delle stazioni da considerare

      integer*4 iserv,iv(max_serv)
      logical*2 l_punto_com
      external  l_punto_com
c
      punto_com_man = .false.
      do iserv = 1,max_serv
cgpe        iv(iserv) = opuvto(istaz(iserv))
c-bpcorr
c-bpcorr        iv(iserv) = ass_coll_man(pgrato(istaz(iserv)))
        iv(iserv) = ass_coll_man(ivert(iserv))
c-bpcorr-end
      enddo
      if (l_punto_com(istaz,iv)) then
        if (hstatj(istaz(1)) .eq. sta_pv .or.
     *      hstatj(istaz(2)) .eq. sta_pv     ) then
          punto_com_man = .true.
        endif
      endif
c
      return
      end ! function punto_com_man

c-bp08
      Subroutine calc_ottimo_ko (istaz,pg,ier)
*&*
C   Creazione dell'algoritmo per la graficazione del punto in caso di ottimo
C   ed eventuale recupero dell'ottimo falliti
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tv.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/flag.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/stations.inc'
      include '../inc/otc.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/messag.inc'
c-UDM
      include '../inc/conv.inc'
c-UDM-end


      include '../inc/ass_cond.inc'
c
      INTEGER*4 istaz(max_serv)  ! I) indice della stazione NOTA: istaz(2) può essere I_UND
      INTEGER*4 pg(max_serv)     ! I)
      INTEGER*4 ier              ! O) indice di errore

      INTEGER*4 pgi,pgj,i,iu,ju,ii1,jj1,ker,unit_sim,ic,ind_c,iserv
      INTEGER*4 j,jk,kk,i1
      logical*2 ce_serie
      external  ce_serie
      logical*2 flag_serie

      INTEGER*4 num_mono,num_bifa

      REAL*4    tot_cons_min,cons_ass(2)
      logical*2 fl_ko_global
      character*2  tip_corr,tip_corr_int
      character*(max_len) mess/' '/
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end
c-------------------------------------------------------------------------
!-inizializzazioni
      ier = 0
      flag_serie = .false.
      fl_ko_global = .false.

c Viene settata la variabile fl_punto_ko = .F. se non e' stato calcolato il punto ko
      fl_punto_ko = .false.

      iserv = 1


CMAR_ASS_COND------------------------------------------------------------

            IF(FLAG_ASS_COND_S) THEN
CMAR_ASS_COND------------------------------------------------------------
      
      if (ce_serie(istaz(iserv))) then
! centrale in serie
        pgi = pg(iserv)
        pgj = pg(iserv+1)
        ier = 0
      else
! centrale bipolare oppure assetto/i non in parallelo:
c Riconoscimento dei casi in cui non viene fatto il recupero per cui si esce:
        if (num_servizi .eq. max_serv) then
c due assetti attivi con unità disponibili in entrambi gli assetti (punto 1)
          iu = first_unit(pg(1))
          ju = first_unit(pg(2))
          do i = 1, unit_num(pg(1))
            ii1=iu+i-1
            jj1=ju+i-1



      if (unit_avail_AS(ii1).gt.0 .and. unit_avail_AS(jj1).gt.0) then
              ier = 1
c              return
              goto 20
            endif
          enddo
        endif
c



c se la configurazione dei TBG e' serie, non possono essere considerati insieme tbg bifase
c e tbg monofase
        do iserv = 1, num_servizi
          pgi = pg(iserv)
          if (stat_ord(pgi) .eq. f_serie) then
            iu = first_unit(pgi)
            num_mono = 0
            num_bifa = 0
            do i = 1, unit_num(pgi)
              if (unit_avail(iu+i-1) .gt. 0) then
                if (unit_bifase(iu+i-1)) then
                  num_bifa = num_bifa + 1
                else
                  num_mono = num_mono + 1
                endif
              endif
            enddo
            if (num_bifa.gt.0 .and. num_mono.gt.0) then
              ier = 1
c              return
              goto 20
            endif
          endif
        enddo

        do iserv = 1, num_servizi
          pgi = pg(iserv)
          if (set_flag(pgi) .ne. 0) then
cgpe-corr
            if (err(pgi).ne.0) then
cgpe-corr-end
            ier = 0
            call calc_punto_ko (istaz(iserv),pgi,ier)
            fl_cen_punto_ko(pgi) = fl_punto_ko
            fl_ko_global = fl_cen_punto_ko(pgi)
            if (iserv .eq. max_serv) then
              fl_ko_global = fl_ko_global .and.
     *                       fl_cen_punto_ko(pg(1))
            endif
cgpe-corr
            endif ! err(pgi).ne.0
cgpe-corr-end
          endif
        enddo
      endif

20    continue

c      if (.not.fl_punto_ko) then
      if (.not.fl_ko_global) then
c se non e' stato calcolato il punto ko, vengono scritti comunque su file di log:
c - altazza adiabatica di stazione (in km)
c - portata di stazione (in kNm3/h flow_stat)
c - portata di stazione (in km3/h  flowm)
        iserv = 1
        if (ce_serie(istaz(iserv))) then
! centrale in serie
           flag_serie = .true.
cgpe-staz-corr
           if (num_servizi .eq. max_serv) num_servizi = 1
cgpe-staz-corr-end
        endif

        do iserv = 1, num_servizi

          pgi = pg(iserv)
cgpe-corr          mess = trim(snomtv(istaz(iserv)))
cgpe-corr          if (trim(sigla_ass(pgi)) .ne.' ') then
cgpe-corr	      mess = trim(mess) //' - ASSETTO:'//trim(sigla_ass(pgi))
cgpe-corr          end if
cgpe-corr          call gest_error (1,0,'',mess,0)
cnew?
          if (.not.fl_cen_punto_ko(pgi)) then
cnew?
          if (flag_serie) then
            pgj = pg(iserv+1)

c            fl_cen_punto_ko(pgi) = fl_punto_ko
c            fl_cen_punto_ko(pgj) = fl_punto_ko
            fl_cen_punto_ko(pgi) = fl_ko_global
            fl_cen_punto_ko(pgj) = fl_ko_global
            call calc_head (istaz(iserv),pgi,tasp_stat(pgi),
     *           pasp_stat(pgi),pmand_stat(pgj),delpr1(pgi),
     *           delpr2(pgj),flow_stat(pgi),head(pgi),flowm(pgi))
c valorizzazione dei dati intermedi
            pmand_stat(pgi) = pasp_stat(pgi) +
cgpe     *                       (pasp_stat(pgj) - pasp_stat(pgi))/2.
     *                       (pmand_stat(pgj) - pasp_stat(pgi))/2.
            pasp_stat(pgj) = pmand_stat(pgi)
cgpe-corr
cgpe            tmand_stat(pgi) = (tasp_stat(pgi) + tmand_stat(pgj))/2.
            tmand_stat(pgi) = stat_vars(7,pgi)
            tasp_stat(pgj)  = tmand_stat(pgi)
            tmand_stat(pgj) = stat_vars(7,pgj)
cgpe-corr-end
          else
c            fl_cen_punto_ko(pgi) = fl_punto_ko
            fl_cen_punto_ko(pgi) = fl_ko_global
            call calc_head (istaz(iserv),pgi,tasp_stat(pgi),
     *           pasp_stat(pgi),pmand_stat(pgi),delpr1(pgi),
     *           delpr2(pgi),flow_stat(pgi),head(pgi),flowm(pgi))
          endif
cgpe-corr-staz
          if (set_flag(pgi) .eq. 1) then
cgpe-corr-staz-end
c Altezza adiabatica di stazione
          ker = 55
c-UDM          write(mess,557) head(pgi),UDM_INT_H ! write(mess,557) head(pgi)
cgpe-mess          call gest_error (1,0,'',mess,0)
          write(mess,557) udm_out_str(udm_H,head(pgi),1)
          call app_mess(pgi,ker,mess)

c portata di stazione
c          write(mess,560) flow_stat(pgi)
c          call gest_error (1,0,'',mess,0)

c Portata di stazione in condizioni macchina
          ker = 55
c-UDM          write(mess,561) flowm(pgi),UDM_INT_QM ! write(mess,561) flowm(pgi)
cgpe-mess          call gest_error (1,0,'',mess,0)
          write(mess,560) udm_out_str(udm_QM,flowm(pgi),1)
          call app_mess(pgi,ker,mess)
cgpe-corr-staz
          endif !(set_flag(pgi) .eq. 1) then
cgpe-corr-staz-end

cnew?
          endif
cnew?
        enddo
      endif
c
c-UDM557   format('Prevalenza ad richiesta: ',f8.3,'  ',A4)
c-UDM560   format('Q richiesta:             ',f8.3,'  ',A12)
c-UDM561   format('Q richiesta:             ',f8.3,'  ',A11)



  



CMAR_ASS_COND------------------------------------------------------------

      ELSE


CMAR_ASS_COND------------------------------------------------------------



      if (ce_serie(istaz(iserv))) then
! centrale in serie
        pgi = pg(iserv)
        pgj = pg(iserv+1)
        ier = 0
      else
! centrale bipolare oppure assetto/i non in parallelo:
c Riconoscimento dei casi in cui non viene fatto il recupero per cui si esce:
        if (num_servizi .eq. max_serv) then
c due assetti attivi con unità disponibili in entrambi gli assetti (punto 1)
          iu = first_unit(pg(1))
          ju = first_unit(pg(2))
          do i = 1, unit_num(pg(1))
            ii1=iu+i-1
            jj1=ju+i-1



            if (unit_avail(ii1).gt.0 .and. unit_avail(jj1).gt.0) then
              ier = 1
c              return
              goto 10
            endif
          enddo
        endif
c



c se la configurazione dei TBG e' serie, non possono essere considerati insieme tbg bifase
c e tbg monofase
        do iserv = 1, num_servizi
          pgi = pg(iserv)
          if (stat_ord(pgi) .eq. f_serie) then
            iu = first_unit(pgi)
            num_mono = 0
            num_bifa = 0
            do i = 1, unit_num(pgi)
              if (unit_avail(iu+i-1) .gt. 0) then
                if (unit_bifase(iu+i-1)) then
                  num_bifa = num_bifa + 1
                else
                  num_mono = num_mono + 1
                endif
              endif
            enddo
            if (num_bifa.gt.0 .and. num_mono.gt.0) then
              ier = 1
c              return
              goto 10
            endif
          endif
        enddo

        do iserv = 1, num_servizi
          pgi = pg(iserv)
          if (set_flag(pgi) .ne. 0) then
cgpe-corr
            if (err(pgi).ne.0) then
cgpe-corr-end
            ier = 0
            call calc_punto_ko (istaz(iserv),pgi,ier)
            fl_cen_punto_ko(pgi) = fl_punto_ko
            fl_ko_global = fl_cen_punto_ko(pgi)
            if (iserv .eq. max_serv) then
              fl_ko_global = fl_ko_global .and.
     *                       fl_cen_punto_ko(pg(1))
            endif
cgpe-corr
            endif ! err(pgi).ne.0
cgpe-corr-end
          endif
        enddo
      endif

10    continue

c      if (.not.fl_punto_ko) then
      if (.not.fl_ko_global) then
c se non e' stato calcolato il punto ko, vengono scritti comunque su file di log:
c - altazza adiabatica di stazione (in km)
c - portata di stazione (in kNm3/h flow_stat)
c - portata di stazione (in km3/h  flowm)
        iserv = 1
        if (ce_serie(istaz(iserv))) then
! centrale in serie
           flag_serie = .true.
cgpe-staz-corr
           if (num_servizi .eq. max_serv) num_servizi = 1
cgpe-staz-corr-end
        endif

        do iserv = 1, num_servizi

          pgi = pg(iserv)
cgpe-corr          mess = trim(snomtv(istaz(iserv)))
cgpe-corr          if (trim(sigla_ass(pgi)) .ne.' ') then
cgpe-corr	      mess = trim(mess) //' - ASSETTO:'//trim(sigla_ass(pgi))
cgpe-corr          end if
cgpe-corr          call gest_error (1,0,'',mess,0)
cnew?
          if (.not.fl_cen_punto_ko(pgi)) then
cnew?
          if (flag_serie) then
            pgj = pg(iserv+1)

c            fl_cen_punto_ko(pgi) = fl_punto_ko
c            fl_cen_punto_ko(pgj) = fl_punto_ko
            fl_cen_punto_ko(pgi) = fl_ko_global
            fl_cen_punto_ko(pgj) = fl_ko_global
            call calc_head (istaz(iserv),pgi,tasp_stat(pgi),
     *           pasp_stat(pgi),pmand_stat(pgj),delpr1(pgi),
     *           delpr2(pgj),flow_stat(pgi),head(pgi),flowm(pgi))
c valorizzazione dei dati intermedi
            pmand_stat(pgi) = pasp_stat(pgi) +
cgpe     *                       (pasp_stat(pgj) - pasp_stat(pgi))/2.
     *                       (pmand_stat(pgj) - pasp_stat(pgi))/2.
            pasp_stat(pgj) = pmand_stat(pgi)
cgpe-corr
cgpe            tmand_stat(pgi) = (tasp_stat(pgi) + tmand_stat(pgj))/2.
            tmand_stat(pgi) = stat_vars(7,pgi)
            tasp_stat(pgj)  = tmand_stat(pgi)
            tmand_stat(pgj) = stat_vars(7,pgj)
cgpe-corr-end
          else
c            fl_cen_punto_ko(pgi) = fl_punto_ko
            fl_cen_punto_ko(pgi) = fl_ko_global
            call calc_head (istaz(iserv),pgi,tasp_stat(pgi),
     *           pasp_stat(pgi),pmand_stat(pgi),delpr1(pgi),
     *           delpr2(pgi),flow_stat(pgi),head(pgi),flowm(pgi))
          endif
cgpe-corr-staz
          if (set_flag(pgi) .eq. 1) then
cgpe-corr-staz-end
c Altezza adiabatica di stazione
          ker = 55
c-UDM          write(mess,557) head(pgi),UDM_INT_H ! write(mess,557) head(pgi)
cgpe-mess          call gest_error (1,0,'',mess,0)
          write(mess,557) udm_out_str(udm_H,head(pgi),1)
          call app_mess(pgi,ker,mess)

c portata di stazione
c          write(mess,560) flow_stat(pgi)
c          call gest_error (1,0,'',mess,0)

c Portata di stazione in condizioni macchina
          ker = 55
c-UDM          write(mess,561) flowm(pgi),UDM_INT_QM ! write(mess,561) flowm(pgi)
cgpe-mess          call gest_error (1,0,'',mess,0)
          write(mess,560) udm_out_str(udm_QM,flowm(pgi),1)
          call app_mess(pgi,ker,mess)
cgpe-corr-staz
          endif !(set_flag(pgi) .eq. 1) then
cgpe-corr-staz-end

cnew?
          endif
cnew?
        enddo
      endif
c
c-UDM557   format('Prevalenza ad richiesta: ',f8.3,'  ',A4)
c-UDM560   format('Q richiesta:             ',f8.3,'  ',A12)
c-UDM561   format('Q richiesta:             ',f8.3,'  ',A11)


CMAR_ASS_COND------------------------------------------------------------

      ENDIF


CMAR_ASS_COND------------------------------------------------------------

557   format('Prevalenza ad richiesta: ',A25)
560   format('Q richiesta:             ',A25)
     
      
      return
      end ! calc_ottimo_ko
c
      Subroutine calc_punto_ko (istaz,ivert,ier)
*&*
C   Creazione dell'algoritmo per la graficazione del punto ko 
C   
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/flag.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
c
      INTEGER*4 istaz,ivert ! I)
      INTEGER*4 ier         ! O) indice di errore

      INTEGER*4 iu,i,k
c-------------------------------------------------------------------------
!-inizializzazioni
      ier = 0
      fl_punto_ko = .false.
c
      if (stat_ord(ivert) .eq. f_serie) then
c trattamento dei TBG bifase in serie
c settaggio dello stato (si considerano solo i TBG bifase)
        iu = first_unit(ivert)
        do i = 1 , unit_num(ivert)
          lstatus(iu+i-1) = OFF
          l_act(iu+i-1) = .false.
c?          if (unit_bifase(iu+i-1) .and. unit_avail(iu+i-1).gt.0) then
c?c            unit_avail(iu+i-1) = 0
c?c Viene settata la vaiabile fl_punto_ko = .T. se e' stato calcolato il punto ko
c?            lstatus(iu+i-1) = ON
c?            l_act(iu+i-1) = .true.
c?          endif
c-bp08          if (unit_avail(iu+i-1).gt.0) then
c-bp08            if (unit_bifase(iu+i-1)) then
c-bp08              fl_punto_ko = .true.
c-bp08              lstatus(iu+i-1) = ON
c-bp08              l_act(iu+i-1) = .true.
c-bp08            else
c-bp08              fl_punto_ko = .false.
c-bp08              goto 10
c-bp08            endif
c-bp08          endif
        enddo

      elseif (stat_ord(ivert) .eq. f_paral) then
c trattamento di TBG  monofase e/o dei bifase in parallelo

c settaggio dello stato (si considerano solo i TBG monofase, per ora)
c NOTA settare la variabile lstatus dovrebbe bastare per poter scrivere anche
c      i risultati
        iu = first_unit(ivert)
        do i = 1,unit_num(ivert)
          lstatus(iu+i-1) = OFF
          l_act(iu+i-1) = .false.
cc Viene settata la vaiabile fl_punto_ko = .T. se e' stato calcolato il punto ko
cc-bp08-prv            fl_punto_ko = .true.
c            fl_punto_ko = .true.
c            lstatus(iu+i-1) = ON
c            l_act(iu+i-1) = .true.
ccgpe-corr
c          else
cc se ci sono compressori bifase e monofase disponibili per ora si è scelto di
cc non rappresentare il punto ko
ccgpe-corr            fl_punto_ko = .false.
c            if (unit_bifase(iu+i-1)) fl_punto_ko = .false.
ccgpe-corr-end
c          endif
          if (unit_avail(iu+i-1).gt.0) then
            if (unit_bifase(iu+i-1)) then
c se ci sono compressori bifase e monofase disponibili per ora si è scelto di
c non rappresentare il punto ko
              fl_punto_ko = .false.
              goto 10
            else
              fl_punto_ko = .true.
              lstatus(iu+i-1) = ON
              l_act(iu+i-1) = .true.
            endif
          endif
        enddo

10    continue

        if (fl_punto_ko) then
	    call single_station_ko(istaz,ivert,tipo_criterio(ivert),
     *         flow_stat(ivert),pasp_stat(ivert),pmand_stat(ivert),
     *         tasp_stat(ivert),tmand_stat(ivert),taria(ivert),
     *         comp_ratio(ivert),tot_cons(ivert),head(ivert),
     *         delpr1(ivert),delpr2(ivert),flowm(ivert),
     *         flow_ric(ivert),type_num(ivert),unit_num(ivert),
     *         first_unit(ivert),first_type(ivert),
     *         stat_vars(1,ivert),stat_ord(ivert),ier)

        endif
      endif

      return
      end ! calc_punto_ko

      Subroutine calc_head (istaz,ivert,tasp,pasp,pman,
     *           delpr1,delpr2,flow,
     *           head,flowm)

C->CALCOLO L'ALTEZZA ADIABATICA DELLA STAZIONE

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'

      integer*4 istaz,ivert      ! I)
      real*4    pasp,pman,tasp   ! I)
      real*4    delpr1,delpr2    ! I)
      real*4    flow             ! I)
      real*4    head,flowm       ! O)

      real*4    z,exp_pol,esp,aux1,aux2,aux3,comp_ratio,
     *          uno_su_ro_attuale,pin,pout
      real*4    prop
      EXTERNAL  prop
	integer*4 ier 
c-----------------------------------------------------------------------
c              CADUTA DI PRESSIONE DOVUTA AL PIPING
      pin  = pasp - delpr1
      pout = pman + delpr2

cgpe      exp_pol = prop(pin,tasp,exp_coef(1,ind))

CMAR  
      comp_ratio = pout/pin
	call politrop_esp(istaz,pin,tasp,comp_ratio,exp_pol,ier)

CMAR      exp_pol = prop(pin,tasp,exp_coef(1,istaz))
      esp = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c---->             adiabatic head
CMAR      z = prop(pin,tasp,z_coef(1,ivert))

      call zpt1(istaz,pin,tasp,z)
      aux1 = (erre / agrvcc / pmol) * Z * tasp/esp
      aux2 = comp_ratio**esp
      head = aux1 * (aux2-1.)                        ! in km
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pin
c coeff per passare da normal a machine conditions (flow_mach=flow_norm*aux3)
      aux3 = ROCN*uno_su_ro_attuale
      flowm = flow*aux3

      return ! calc_head
      end
c-bp08-end

      Subroutine deapp_mess (ivert)

!-Vengono puliti i messaggi associati alla simulazione della centrale ivert

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/messag.inc'

      integer*4 ivert      ! I)

      integer*4 i,j
c-----------------------------------------------------------------------
!      INTEGER*4           mess_num, mess_type
!      character*(max_len) mess_stri
      do i = 1,mess_num(IVERT)
        do j = 1,num_rig_mess(i)
!-j-esima riga che forma la descrizione dell'i-esimo messaggio
          mess_descr(j,i) = ' '
        end do
      end do
      do i = 1,mess_num(IVERT)
!-numero di righe per ciascun messaggio
        num_rig_mess(i) = 0
      end do
!-numero di messaggi
      mess_num(IVERT) = 0

      return
      end

      Subroutine calc_limiti_ce (istaz,ivert,ncorr,
     *           pasp_rich,pman_rich,flow_rich,cons_rich,
     *           ivar,ier)
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/tx.inc'
      include '../inc/flag.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/otc.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/messag.inc'
c
      integer*4  istaz       ! I)
     *          ,ivert       ! I)
     *          ,ncorr       ! I)
      real*4     pasp_rich   ! O)
     *          ,pman_rich   ! O)
     *          ,flow_rich   ! O)
     *          ,cons_rich   ! O)
      integer*4  ivar        ! O)
     *          ,ier         ! O) indice di errore

      integer*4 jvert
      integer*4 i_estset ! vecchio ind_c
     *         ,i_medset ! vecchio ind_c_int
     *         ,i_intset ! nuovo loop

      integer*4 hstatj_est,hstatj_med,hstatj_int
     *         ,ncorr_est,ncorr_med,ncorr_int
      integer*4 ivar_save,ier_save
      integer*4 jstaz
      INTEGER*4 fl_sim_istaz,fl_sim_jstaz

      real*4    max_pout_app,flow_prog_app

	real*4    val_corr_est(ncorr+1),delta_var_est,val_save_est

	real*4    val_corr_med(ncorr+1),delta_var_med,val_save_med
     *         ,val_corr_int(ncorr+1),delta_var_int,val_save_int

      real*4    pasp_save,pman_save,flow_save,delta_rich
	real*4    pasp_rich_save,pman_rich_save,flow_rich_save
     *         ,cons_rich_save
     *         ,delta_rich_save

	real*4    p_flow,p_pasp,p_pman

      real*4    pmin_asp,pmax_asp,pmin_man,pmax_man

	real*4    delpr_in,delpr_out
      real*4    dp_in,dp_out
      external  dp_in,dp_out

      logical*2 flag_serie
     *         ,ce_serie
      external  ce_serie

      logical*2 l_simula

      character*2  tip_corr_est,tip_corr_med,tip_corr_int
c
c definizione del vettore val_corr della grandezza da grigliare
c x es. se pasp_stat = 20 e pmand_stat = 60,
c allora: delta_var = (60-30)/100*perc_p=0.3*3=0.9
c e:      val_corr=(1:10)= (60+0.9,60+0.9*2,...,60+0.9*5,60-0.9,60-0.9*2,...,60-0.9*5)
c-------------------------------------------------------------------------
!-inizializzazioni
      ier = 0
      ier_save = 0
      ivar = 0
      ivar_save = ivar

c simulo solo la centrale istaz, anche se esiste l'assetto coniugato
      jstaz = I_UND

      ncorr_est = 2
      ncorr_med = 2
      ncorr_int = 4

      jvert = ivert
      flag_serie = ce_serie(istaz)
      if (flag_serie) jvert = pgrato_app(istaz)
c da definire in base alle tipologie di casi:
      p_flow = 1.
      p_pasp = 1.
      p_pman = 1.
      delta_rich_save = R_MAX

c salvataggio dei dati di input
      flow_save = flow_stat(ivert)
      pasp_save = pasp_stat(ivert)
      pman_save = pmand_stat(ivert)
      if (flag_serie) then
        pman_save = pmand_stat(jvert)
      endif
      pasp_rich_save = R_MAX
      pman_rich_save = R_MAX
      flow_rich_save = R_MAX
      cons_rich_save = R_MAX

c Riconoscimento dei casi in cui non viene fatto il recupero per cui si esce:
      if (hstatj(istaz).eq.sta_und) then
        ier = -1
c        msg_err = ' Tipo di regolazione indefinito'
        return
      endif

c gestione limiti della centrale
      max_pout_app  = stat_vars(6,jvert)
c-din-corr      flow_prog_app = stat_vars(2,ivert)
      flow_prog_app = qmaxtx(istaz)
c-din-corr-end
c gestione limiti dei punti in aspirazione e mandata
      pmin_asp = pmintx(opumto(istaz))
      pmax_asp = pmaxtx(opumto(istaz))
      pmin_man = pmintx(opuvto(istaz))
      pmax_man = pmaxtx(opuvto(istaz))

c il ciclo esterno viene fatto sulla variabile di set:
c scelta della variabile per il ciclo medio ed interno
      if (hstatj(istaz) .eq. sta_pm) then
        hstatj_est = sta_pm
        val_save_est = pasp_stat(ivert)
        hstatj_med = sta_pv
        val_save_med = pmand_stat(jvert)
        hstatj_int = sta_q
        val_save_int = flow_stat(ivert)
        val_corr_est(1) = pasp_stat(ivert)
        val_corr_med(1) = pmand_stat(jvert)
        val_corr_int(1) = flow_stat(ivert)

        p_flow = 0.001
        p_pasp = 100.
        p_pman = 100.

      elseif (hstatj(istaz) .eq. sta_pv) then
        hstatj_est = sta_pv
        val_save_est = pmand_stat(jvert)
        hstatj_med = sta_pm
        val_save_med = pasp_stat(ivert)
        hstatj_int = sta_q
        val_save_int = flow_stat(ivert)
        val_corr_est(1) = pmand_stat(jvert)
        val_corr_med(1) = pasp_stat(ivert)
        val_corr_int(1) = flow_stat(ivert)
        p_flow = 0.001
        p_pasp = 100.
        p_pman = 100.

      elseif (hstatj(istaz) .eq. sta_q) then
        hstatj_est = sta_q
        val_save_est = flow_stat(ivert)
        hstatj_med = sta_pv
        val_save_med = pmand_stat(jvert)
        hstatj_int = sta_pm
        val_save_int = pasp_stat(ivert)
        val_corr_est(1) = flow_stat(ivert)
        val_corr_med(1) = pmand_stat(jvert)
        val_corr_int(1) = pasp_stat(ivert)
        p_flow = 1.
        p_pasp = 1.
        p_pman = 1.

      else
        ier = -1
        return
      endif

      if (flag_serie) then
        pmand_stat(ivert) = (pmand_stat(jvert)+pasp_stat(ivert))/2.
      endif
c calcolo dei valori da provare per il ciclo esterno
      call calc_range_corr (max_pout_app,flow_prog_app,
     *     hstatj_est,ncorr_est,val_corr_est(2),
     *     delta_var_est,pasp_stat(ivert),pmand_stat(jvert),
     *     flow_stat(ivert),val_save_est,tip_corr_est)

c calcolo dei valori da provare per il ciclo medio
      call calc_range_corr (max_pout_app,flow_prog_app,
     *     hstatj_med,ncorr_med,val_corr_med(2),
     *     delta_var_med,pasp_stat(ivert),pmand_stat(jvert),
     *     flow_stat(ivert),val_save_med,tip_corr_med)

c calcolo dei valori da provare per il ciclo interno
      call calc_range_corr (max_pout_app,flow_prog_app,
     *     hstatj_int,ncorr_int,val_corr_int(2),
c-din-corr     *     delta_var_med,pasp_stat(ivert),pmand_stat(jvert),
     *     delta_var_int,pasp_stat(ivert),pmand_stat(jvert),
     *     flow_stat(ivert),val_save_int,tip_corr_int)

c vengono provati i valori corretti e si esce al primo tentativo OK
      do i_estset = 1, ncorr_est + 1
! ciclo esterno: viene settato il valore corrispondente alla variabile di set
        if (hstatj_est .eq. sta_q) then
          flow_stat(ivert) = val_corr_est(i_estset)
          if (flag_serie) then
            flow_stat(jvert) = val_corr_est(i_estset)
          endif
        elseif (hstatj_est .eq. sta_pm) then
          pasp_stat(ivert) = val_corr_est(i_estset)
        elseif (hstatj_est .eq. sta_pv) then
          pmand_stat(jvert) = val_corr_est(i_estset)
        endif

        do i_medset = 1, ncorr_med + 1
! ciclo medio: viene settato il valore corrispondente alla variabile hstatj_med
          if (hstatj_med .eq. sta_q) then
            flow_stat(ivert) = val_corr_med(i_medset)
            if (flag_serie) then
              flow_stat(jvert) = val_corr_med(i_medset)
            endif
          elseif (hstatj_med .eq. sta_pm) then
            pasp_stat(ivert) = val_corr_med(i_medset)
          elseif (hstatj_med .eq. sta_pv) then
            pmand_stat(jvert) = val_corr_med(i_medset)
          endif

          do i_intset = 1, ncorr_int + 1
! ciclo interno: viene settato il valore corrispondente alla variabile hstatj_int
            if (hstatj_int .eq. sta_q) then
              flow_stat(ivert) = val_corr_int(i_intset)
              if (flag_serie) then
                flow_stat(jvert) = val_corr_int(i_intset)
              endif
            elseif (hstatj_int .eq. sta_pm) then
              pasp_stat(ivert) = val_corr_int(i_intset)
            elseif (hstatj_int .eq. sta_pv) then
              pmand_stat(jvert) = val_corr_int(i_intset)
            endif

            if (.not.flag_serie) then
c calcolo dei dp dovuti alle nuove portate
              delpr1(ivert) = dp_in(istaz,pasp_stat(ivert),
     *                        tasp_stat(ivert),flow_stat(ivert))
              delpr2(ivert) = dp_out(istaz,pmand_stat(jvert),
     *                        stat_vars(7,jvert),flow_stat(ivert))
            else
c calcolo dei dp dovuti alle nuove portate
              delpr1(ivert) = dp_in(istaz,pasp_stat(ivert),
     *                        tasp_stat(ivert),flow_stat(ivert))
              delpr2(ivert) = dp_out(istaz,pmand_stat(ivert),
     *                        stat_vars(7,ivert),flow_stat(ivert))
c viene usata tasp_stat(ivert) perché tasp_stat(jvert) forse non è validata
              delpr1(jvert) = dp_in(istaz,pasp_stat(jvert),
     *                        tasp_stat(ivert),flow_stat(jvert))
              delpr2(jvert) = dp_out(istaz,pmand_stat(jvert),
     *                        stat_vars(7,jvert),flow_stat(jvert))
            endif

c forse è da mettere la chiamata alla subroutine check_ce
            if (pasp_stat(ivert) .lt. pmand_stat(jvert) .and.
     *          flow_stat(ivert) .gt. 0.                     ) then
             if (pasp_stat(ivert) .gt. pmin_asp .and.
     *           pasp_stat(ivert) .lt. pmax_asp .and.
     *           pmand_stat(jvert) .gt. pmin_man .and.
     *           pmand_stat(jvert) .lt. pmax_man      ) then
              ier = -1
c-din-corr
c-din-corr              if (.not.flag_serie) then
c-din-corr                call optcent_mf(ier)
c-din-corr              else
c-din-corr                call calc_ottimo_serie (istaz,istaz,ivert,jvert,ier)
c-din-corr              endif
              call deapp_mess (ivert)
              fl_simu(ivert) = .false.
              if (flag_serie) then
                call deapp_mess (jvert)
                fl_simu(jvert) = .false.
              endif

c simulazione della centrale in rete, vengono settate le condizioni imposte dalla rete
c      call setta_cin (istaz,jstaz)
              if (fl_din) then
                call setta_stato_tbg (istaz,jstaz,
     *               fl_sim_istaz,fl_sim_jstaz)
              endif

c controllo la simulabilità della centrale (ovvero che non sia spenta o con
c condizioni non fattibili)
              call check_ce (istaz,jstaz,l_simula)
c l_simula=.T. se almeno uno degli assetti ha condizioni ok per la simulazione
              if (l_simula) then
                call s_cnt_new(istaz,jstaz)
                if (fl_ris_ce(ivert) .eq. 0) then
                  ier = 0
                  if (flag_serie .and. fl_ris_ce(jvert) .ne. 0) then
                    ier = 1
                  endif
                endif
              endif
c-din-corr-end
             endif
            endif
            if (ier .eq. 0) then
              ivar = 0
c              if (hstatj_est .eq. sta_pm) then
c                pasp_rich = val_corr_est(i_estset)
c              elseif (hstatj_est .eq. sta_pv) then
c                pman_rich = val_corr_est(i_estset)
c              elseif (hstatj_est .eq. sta_q) then
c                flow_rich = val_corr_est(i_estset)
c              endif
c              if (hstatj_med .eq. sta_pm) then
c                pasp_rich = val_corr_med(i_medset)
c              elseif (hstatj_med .eq. sta_pv) then
c                pman_rich = val_corr_med(i_medset)
c              elseif (hstatj_med .eq. sta_q) then
c                flow_rich = val_corr_med(i_medset)
c              endif
c              if (hstatj_int .eq. sta_pm) then
c                pasp_rich = val_corr_int(i_intset)
c              elseif (hstatj_int .eq. sta_pv) then
c                pman_rich = val_corr_int(i_intset)
c              elseif (hstatj_int .eq. sta_q) then
c                flow_rich = val_corr_int(i_intset)
c              endif

              pasp_rich = pasp_stat(ivert)
              pman_rich = pmand_stat(jvert)
              flow_rich = flow_stat(ivert)
              cons_rich = tot_cons(ivert)
              if (flag_serie) then
                cons_rich = cons_rich + tot_cons(jvert)
              endif

              delta_rich = p_flow*(abs(flow_rich - flow_save)) +
     *                     p_pasp*(abs(pasp_rich - pasp_save)) +
     *                     p_pman*(abs(pman_rich - pman_save))

              if (delta_rich .lt. delta_rich_save) then
                ier_save = 0
                pasp_rich_save  = pasp_rich
                pman_rich_save  = pman_rich
                flow_rich_save  = flow_rich
                cons_rich_save  = cons_rich
                delta_rich_save = delta_rich
c calcolo di ivar
                if (pasp_rich .ne. pasp_save) then
                  ivar = 1
                endif
                if (pman_rich .ne. pman_save) then
                  ivar = ivar+1*10
                endif
                if (flow_rich .ne. flow_save) then
                  ivar = ivar+1*100
                endif
                ivar_save = ivar
c-din-corr
                goto 10
c-din-corr-end
              endif

            endif
          enddo
        enddo
      enddo

10    continue

c salvataggio dei dati di input
      flow_stat(ivert)  = flow_save
      pasp_stat(ivert)  = pasp_save
      pmand_stat(ivert) = pman_save
      if (mess_num(ivert) .gt. 0) call deapp_mess(ivert)
      if (flag_serie) then
        pmand_stat(jvert) = pman_save
        flow_stat(jvert)  = flow_save
        if (mess_num(jvert) .gt. 0) call deapp_mess(jvert)
      endif
      if (ier_save .eq. 0) then
        pasp_rich  = pasp_rich_save
        pman_rich  = pman_rich_save
        flow_rich  = flow_rich_save
        cons_rich  = cons_rich_save
        delta_rich = delta_rich_save
        ivar = ivar_save
        ier = 0
      endif

      return
      end ! calc_limiti_ce
