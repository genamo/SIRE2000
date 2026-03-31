cgpe	SUBROUTINE verstel_nc_rk(	! Verifica che siano rispettati i vincoli

C----------------->NUOVE ROUTINE AGGSTEL_NW,VERSTEL_NW,INISTA_NW<---------------------

c***********************************************************************************
	subroutine messtel(fl_int,fl_end,msg_int,msg)
c*****************************************************************************
c    Messaggi stato elementi.
c    Stampa il messaggio che descrive la violazione riscontrata nell'elemento
c    ed eventualmente l'intestazione che ne riporta il nome.
c    fl_end :se TRUE viene stampata una riga vuota dopo il blocco di messaggi
c    stampati.
c    Subroutine usata unicamente nella verstel_nw
c*****************************************************************************
      implicit none
	
	logical*2 fl_int,
     *          fl_end
	 
	character*(*) msg_int,
     *              msg
c------------------------------------------------------------------
	if (.not.fl_int) then
	   call gest_error(3,0,' ',' ',0)
	   call gest_error(3,0,' ',msg_int,0)
	   fl_int=.true.
	   fl_end=.true.
      end if
	call gest_error(3,0,' ',msg,0)

      return 
	end   !messtel 

cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr



      subroutine verstel_stat_nw(istaz,pg,pg_app,flag_mf)
c-----------------------------------------------------------------------------------
c     Controllo relativi ad una centrale da eseguire dopo la simulazione
c-----------------------------------------------------------------------------------
      implicit none

      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/jc.inc'
   	include '../inc/err.inc'
   	include '../inc/flag.inc'
   	include '../inc/stazione.inc'
   	include '../inc/top_app_ele.inc'
   	include '../inc/stations.inc'
   	include '../inc/units.inc'
   	include '../inc/mf_units.inc'
   	include '../inc/types.inc'
   	include '../inc/messag.inc'
   	include '../inc/conv.inc'
   	include '../inc/staznw_par.inc'

	include '../inc/ASS_COND.INC'
cmar_rev_log_sem

      include '../inc/allarmi.inc'
   
cmar_rev_log_sem

      integer*4 istaz       ! I) indice stazione nella struttura dati completa
     *         ,pg          ! I) pgrato(istaz)
     *         ,pg_app      ! I) pgrato_app(istaz)
      logical*2 flag_mf     ! I) flag multifunzione

c l_ko       !flag violazioni KO 'soluzione non trovata
c l_fat      !flag violazioni fatal soluzione non trovata ma con risultati
c l_info     !flag violazioni warning soluzione non accettabile
c l_warn     !flag violazioni informational soluzione non accettabile con variz.SET

      integer*4 iu,i,j,im,iv,setflag
      integer*4 vert_type,type_act_loc(maxunits),
     *          ntipi_act,nunit_act,max_nunitact,k,ii1
      real*4    pasp_stat_dp,pmand_stat_dp,p_app

      character*(max_len_str) udm_out_str
      external                udm_out_str
	character*100 msg_int !intestazione messaggi di errore
     *             ,msg     !messaggio associato alla violazione riscontrata  

	logical*2 l_crit_ko,ltest_crit_giri,ltest_crit
	logical*2 fl_vio_temp
c nel caso di assetto serie senza punto ko, evita di mettere l'intestazione relativa
c ai due stadi per il messaggio relativo ad head e flowm
	logical*2 fl_mess_stadio
      logical*2 ce_accesa,ce_off,ce_avail,ce_serie
      external  ce_accesa,ce_off,ce_avail,ce_serie
	logical*2 fl_int      !flag intestazione: TRUE (già stampata)
	logical*2 fl_end
	real*4 qset_real
	external qset_real
	logical*2 fl_skip_mess
c-----------------------------------------------------------------------------------
c---> Prepara l'intestazione dell'elenco di messaggi relativi alla centrale


      fl_cmf_ass(istaz)=.false.
      if (flag_mf) then
CMRA_ASS_COND
      
	IF(FLAG_ASS_COND(ISTAZ-1))THEN
      
	if (.not.no_acc(istaz)) then
	hstatj(istaz)=hstatj(istaz-1)
      endif

      RETURN
	ENDIF
CMAR_ASS_COND
        IF(FLAG_ASS_COND(ISTAZ))THEN
cmar_Ass_conddddddddddddd
	pg_app=pg+1
cmar_ass_cond

      IF(FLOWM(PG).EQ.0)THEN
	FL_FATAL(PG)=8
	ELSEIF(FLOWM(PG+1).EQ.0)THEN
	FL_FATAL(PG+1)=8
	ENDIF

cmar_ass_con


cmar_ass_cond

c      IF(FLAG_ASS_COND(ISTAZ))THEN
c      ce_serie(istaz)=.false.   
c	endif

cmar_ass_cond

        msg_int='CENTRALE MULTIFUNZIONE ad assetti condivisi'// '  : 
     *'//trim(snomtv(istaz) )

cmar_log_sem
c	    IF( 

c     *            (
c     *(port_min_all(istaz).NE. R_UND).OR.(port_max_all(istaz).NE. R_UND 
c     * )           )

c     *.AND.   (

c     *(port_min_all(istaz+1).NE.R_UND).OR.
c     *(port_max_all(istaz+1).NE.R_UND)  
c     *         )

c     *       ) THEN

c	  fl_cmf_ass(istaz)=.true.
c        msg_int='CENTRALE MULTIFUNZIONE ad assetti condivisi'// '  : 
c     *'//trim(snomtv(istaz) )
c     *          ' :ASSETTO:'//trim(sigla_ass(pg))


c      	ENDIF

      IF( 

     *(port_min_all(istaz).NE. R_UND).OR.
     *(port_max_all(istaz).NE. R_UND).OR.
     *(port_min_all(istaz+1).NE.R_UND).OR.
     *(port_max_all(istaz+1).NE.R_UND)  
     *         ) THEN

	  fl_cmf_ass(istaz)=.true.
c        msg_int='CENTRALE MULTIFUNZIONE ad assetti condivisi'// '  : 
c     *'//trim(snomtv(istaz) )
c     *          ' :ASSETTO:'//trim(sigla_ass(pg))


      	ENDIF




cmar_log_sem

        ELSE
        msg_int='CENTRALE MULTIFUNZIONE'// ' : '//trim(snomtv(istaz))//
     *          '         ASSETTO:'//trim(sigla_ass(pg))

        ENDIF
     	else
        msg_int=trim(te_stri(e_ce))//' : '//trim(snomtv(istaz)) 
      end if

c inizializzazioni
	fl_mess_stadio = .true.
	fl_skip_mess = .false.

c confronta lo stato finale della centrale con quello richiesto per essa dall'utente
c Tale messaggio non deve comparire in Simcent perchè in caso di recupero viene modificata
c la variabile di set ma non il tipo di set.
c Il messaggio può invece comparire nella simulazione di rete, poichè come accade per gli 
c altri elementi, in questo caso la centrale può anche subire una modifica del tipo di set
c_bin
c-simcent-corr      call  verst_bin(istaz,msg_int,fl_int,fl_end)
c ALTRIMENTI: si può chiamare la verst_bin e uscire dopo l'inizializzazione di
c             flertj
      if (.not. fl_sim) then
        call verst_bin(istaz,msg_int,fl_int,fl_end)
      else
c per simcent vengono inizializzate queste variabili il cui valore veniva
c inizializzato nella verst_bin
        fl_int = .false.
        fl_end = .false.
        flertj(istaz) = err_ok
      endif
c-simcent-corr-end

      call set_stato_stazione(setflag,istaz)
      if (set_flag(pg) .ne. setflag) then
        if (setflag .eq. 1) then
c la centrale ha condizioni che non ne permettono la simulabilità, pertanto è stato
c cambiato il set_flag da 1 a 0
          call tipo_err(istaz,err_ff,msg)
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'La centrale non ha condizioni che ne permettano la '//
     *          'simulazione.'
         call messtel(fl_int,fl_end,msg_int,msg)
c viene definito il fl_fatal per avere comunque la messaggistica relativa alla centrale
c-din-corr          if (.not.fl_sim) fl_fatal(pg) = -1
          if (.not.fl_sim) then
            if (fl_fatal(pg) .ge. 0) fl_fatal(pg) = -1
          endif
c-din-corr-end
        endif
      endif

c TRATTAMENTO DEL FL_FATAL  = 0 => centrale ok
c                           = sta_pm/sta_pv/sta_q => centrale ok, ma con recupero
c                           = -k => centrale ko, con errore fatale k
      if (fl_fatal(pg) .lt. 0) then
c caso 1) la centrale non è stata simulata non avendo superato i controlli della check_ce
c caso 2) la simulazione non ha trovato soluzione
c-din-corr
       if (fl_fatal(pg) .lt. -100000) then
         call tipo_err(istaz,err_ff,msg)
         call messtel(fl_int,fl_end,msg_int,msg)
         msg = 'Attivato con tlc un tbg che ha spento l''unico tbg'//
     *         ' acceso nell''assetto coniugato.'
         call messtel(fl_int,fl_end,msg_int,msg)

c-din-corr       if (fl_fatal(pg) .lt. -10000) then
       elseif (fl_fatal(pg) .lt. -10000) then
c-din-corr-end
c rappresentazione del punto ko (lfat = T)
         call tipo_err(istaz,err_ff,msg)
         call messtel(fl_int,fl_end,msg_int,msg)
         if (tipo_criterio(pg) .ne. crit_man) then
           msg = 'La simulazione non ha trovato una configurazione'//
     *           ' ottima.'
         else
	     msg = 'Le macchine accese dall''utente (criterio manuale)'//
     *           ' non soddisfano il punto di lavoro.' 
         endif
         call messtel(fl_int,fl_end,msg_int,msg)

       else
c senza rappresentazione del punto ko
        if (set_flag(pg) .eq. setflag) then
           call tipo_err(istaz,err_ff,msg)
	     call messtel(fl_int,fl_end,msg_int,msg)
           if (fl_sim) then !simcent
             call tipo_err(istaz,err_ko,msg)
           endif
           if (tipo_criterio(pg) .ne. crit_man) then
             msg = 'La simulazione non ha trovato una configurazione '//
     * 	         'ottima.'
           else
             msg = 'Le macchine accese dall''utente (criterio '// 
     *             'manuale) non soddisfano il punto di lavoro.'
           endif
	     call messtel(fl_int,fl_end,msg_int,msg)
	  endif

        if (fl_fatal(pg) .lt. -1000) then
          call tipo_err(istaz,err_ff,msg)
          if (fl_sim) then !simcent
            call tipo_err(istaz,err_ko,msg)
          endif
	    call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Modulo di simulazione non previsto.'
	    call messtel(fl_int,fl_end,msg_int,msg)

        elseif (fl_fatal(pg) .lt. -100) then
          call tipo_err(istaz,err_ff,msg)
          if (fl_sim) then !simcent
            call tipo_err(istaz,err_ko,msg)
          endif
	    call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Impossibile effettuare il recupero: unita'' condivise.'
	    call messtel(fl_int,fl_end,msg_int,msg)
        else
          if (ce_serie(istaz)) then
            fl_mess_stadio = .false.
cmar-dp
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,p_app)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,p_app)
            call calc_deltapTOT_eff(istaz,pg_app,p_app,pmand_stat_dp)
cmar-dp          call calc_deltap_eff(istaz,pg_app,p_app,pmand_stat_dp)
          else
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
	    endif
cmar-dp
c          if (set_flag(pg).eq.0) then  !sta_fr,sta_on,sta_off,sta_nr
          if (setflag .eq. 0) then
c stazione spenta (sta_fr,sta_on,sta_off,sta_nr): associo un messaggio
c alle violazioni individuate dalla check_ce
c controllo sui vincoli fisici (non si dovrebbe passare se lo spegnimento è OK)
		  if (ce_accesa(pg)) then
c non ha senso in SIMCENT ma potrebbe essere restituito dallo stazionario
              call tipo_err(istaz,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Stazione spenta con unità necessariamente accese.'
	        call messtel(fl_int,fl_end,msg_int,msg)
            endif

c test ridondante???
	      if (.not.ce_off(pasp_stat_dp,pmand_stat_dp)) then
              call tipo_err(istaz,err_ff,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Richiesta una compressione effettiva'//
     *              ' ad una stazione spenta:'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
		  endif 
c test ridondante???-end

          else
c  tale messaggio non dovrebbe mai comparire sul log dato che il controllo sulla
c  disponibilità dei turbogruppi viene eseguito in maschera prima di lanciare la
c  simulazione;in particolare almeno un tbg deve essere dichiarato disponibile o 
c  acceso           
c PER LO STAZIONARIO PUO' COMPARIRE, perché il test viene fatto su setflag, che
c è lo stato della centrale trovato dalla rete
            if (ce_serie(istaz)) then
              if (.not.ce_avail(pg) .or. .not.ce_avail(pg_app)) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Centrale accesa ma nessuna unità di '//
     *                'compressione disponibile.'
	          call messtel(fl_int,fl_end,msg_int,msg)
              endif
            else
              if (.not.ce_avail(pg)) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Centrale accesa ma nessuna unità di '//
     *                'compressione disponibile.'
	          call messtel(fl_int,fl_end,msg_int,msg)
              endif
		  endif
            if (ce_serie(istaz)) then
c              if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
cc controllo all'uscita del primo stadio
c                call tipo_err(istaz,err_ii,msg)
c                call messtel(fl_int,fl_end,msg_int,msg)
c	          msg = 'Pressione di uscita al primo stadio superiore'//
c     *                ' alla massima consentita: '
c	          call messtel(fl_int,fl_end,msg_int,msg)
c                write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
c     *                         udm_out_str(udm_P,stat_vars(6,pg),1)
c	          call messtel(fl_int,fl_end,msg_int,msg)
c	        endif
              if (pmand_stat(pg_app).gt.stat_vars(6,pg_app)+repsP) then
cmar_log_sem                call tipo_err(istaz,err_ff,msg)
                call tipo_err(istaz,err_ww,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Pressione di mandata superiore alla '//
     *                'massima consentita: '
	          call messtel(fl_int,fl_end,msg_int,msg)
                write(msg,630) udm_out_str(udm_P,pmand_stat(pg_app),0),
     *                         udm_out_str(udm_P,stat_vars(6,pg_app),1)
	          call messtel(fl_int,fl_end,msg_int,msg)
	        endif
	      else
              if (pmand_stat(pg) .gt. stat_vars(6,pg)+repsP) then
cmar_rev_log_sem call tipo_err(istaz,err_ff,msg)
                 call tipo_err(istaz,err_ww,msg)
                 if (fl_sim) then !simcent
                   call tipo_err(istaz,err_ko,msg)
                 endif
                 call messtel(fl_int,fl_end,msg_int,msg)
	           msg = 'Pressione di mandata superiore alla '//
     *                 'massima consentita: '
	           call messtel(fl_int,fl_end,msg_int,msg)
                 write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                          udm_out_str(udm_P,stat_vars(6,pg),1)
	           call messtel(fl_int,fl_end,msg_int,msg)
	        endif
	      endif
 	      if (pasp_stat_dp .gt. pmand_stat_dp) then
c Viene restituito sia da SIMCENT sia dallo STAZIONARIO nel caso in cui i delta p
c dovuti ai piping/filtri/air-cooler
c (il controllo sul segno delle pressioni viene eseguito in maschera prima di
c lanciare la simulazione, ma riguarda le pressioni iniziali pasp_stat e pmand_stat)
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Pressione di mandata inferiore alla'//
     *              ' pressione di aspirazione:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

 	      if (pasp_stat_dp .le. 0) then
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Delta pressione asp. non corretto che rende la'
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'pressione di aspirazione al compressore negativa:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,640) udm_out_str(udm_P,pasp_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

 	      if(pmand_stat_dp .le. 0) then
c Questo messaggio dovrebbe comparire solo sullo stazionario dove non c'è
c alcun controllo sulle pressioni di aspirazione e mandata, in quanto
c calcolate dal sistema che risolve la rete
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Delta pressione man. non corretto che rende la'
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'pressione di mandata al compressore negativa:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,650) udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

	      if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
c non ha senso in SIMCENT ma potrebbe essere restituito dallo stazionario
c err_ko???
              call tipo_err(istaz,err_ii,msg)
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Le pressioni di aspirazione e di mandata'
	        call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'calcolate rendono spenta la centrale'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
		  end if 

c Controllo sulla corretta definizione delle parabole limite per l'applicazione del
c criterio di equidistanza:errore bloccante se la parabola massima cade a sinistra
c della parabola minima (per l'assetto in serie i tbg del primo stadio sono i
c medesimi del secondo stadio)
            if (tipo_criterio(pg) .eq. crit_equi) then
c controllo parabole per il criterio di equidistanza
              l_crit_ko = .false.
              iu = first_unit(pg)
              do j = 1,unit_num(pg)
                 i = iu + j - 1
	           if (unit_crit_max(i).gt.unit_crit_min(i)) then
                    l_crit_ko = .true.
                 end if
  	        end do
              if (l_crit_ko) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Risultano invertite le parabole necessarie'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'al criterio di equidistanza:'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'controllare le percentuali inserite.'
                call messtel(fl_int,fl_end,msg_int,msg)
              endif
            endif
            l_crit_ko = .false.
            ltest_crit_giri = .false.
            if (tipo_criterio(pg) .eq. crit_giri) then
c calcolo il tipo di centrale (semplice, composta o mista) in base al valore dello
c stato dei tbg per unità necessariamente accese
c definisco type_act_loc in base al valore di unit_avail/unit_attiv_tlc rispettivamente
c per stazionario e dinamica
              iu = first_unit(pg)
              do i = 1,type_num(pg)
                j = first_type(pg)+i-1
                type_act_loc(i) = 0
                do k = 1,type_quant(j)
                  ii1 = iu+k-1
                  if (unit_avail(ii1).gt.1) then
c fare il test anche su unit_attiv_tlc????
                    type_act_loc(i) = type_act_loc(i) + 1
                    ltest_crit_giri = .true.
                  end if
                end do
                iu = iu+type_quant(j)
              end do
              if (ltest_crit_giri) then
                call init_vert_type(pg,type_act_loc,vert_type)
                if (vert_type .eq. tipo_mist) then
c centrale mista e tipo di configurazione dei turbogruppi è in serie
                  l_crit_ko = .true.
                elseif (vert_type .eq. tipo_comp) then
c per una centrale di tipo composto, non ci devono essere attivi tipi diversi
c calcolo di ntipi_act e nunit_act
                  ntipi_act = 0
                  nunit_act = 0
                  do i = 1, type_num(pg)
                    if (type_act_loc(i) .gt. 0) then
                      ntipi_act = ntipi_act + 1
                      nunit_act = nunit_act + 1
                    endif
                  enddo
                  if (ntipi_act .gt. 1) then
                    l_crit_ko = .true.
                  endif
c controllo che non venga superato il massimo numero di tbg bifase
                  max_nunitact = maxunits/2
                  if (nunit_act .gt. max_nunitact) then
                    l_crit_ko = .true.
                  endif
                end if
                if (l_crit_ko) then
                  call tipo_err(istaz,err_ff,msg)
                  if (fl_sim) then !simcent
                    call tipo_err(istaz,err_ko,msg)
                  endif
                  call messtel(fl_int,fl_end,msg_int,msg)
                  msg = 'Stato dei turbogruppi non consistente'
                  call messtel(fl_int,fl_end,msg_int,msg)
                  msg = 'per il criterio dei giri'
                  call messtel(fl_int,fl_end,msg_int,msg)
                endif
              endif
            else
c criterio delle portate e dell'equidistanza
              iu = first_unit(pg)
              do i = 1,type_num(pg)
                j = first_type(pg)+i-1
                type_act_loc(i) = 0
                do k = 1,type_quant(j)
                  ii1 = iu+k-1
                  if (unit_avail(ii1).gt.1) then
                    type_act_loc(i) = type_act_loc(i) + 1
                    ltest_crit = .true.
                  end if
                end do
                iu = iu+type_quant(j)
              end do
              if (ltest_crit) then
                call init_vert_type(pg,type_act_loc,vert_type)
                if (vert_type .eq. tipo_mist .and.
     *              stat_ord(pg) .eq. f_serie         ) then
                  l_crit_ko =  .true.
                  if (l_crit_ko) then
                    call tipo_err(istaz,err_ff,msg)
                    if (fl_sim) then !simcent
                      call tipo_err(istaz,err_ko,msg)
                    endif
                    call messtel(fl_int,fl_end,msg_int,msg)
                    msg = 'Centrale mista con configurazione dei'
                    call messtel(fl_int,fl_end,msg_int,msg)
                    msg = 'turbogruppi in serie'
                    call messtel(fl_int,fl_end,msg_int,msg)
                  endif
                endif
              endif
            endif
	    endif
        endif
       endif

      else if (fl_fatal(pg).gt.0 .or. fl_fatal(pg_app).gt.0) then
        if (fl_fatal(pg) .ge. soglia_cons .or.
     *      fl_fatal(pg_app) .ge. soglia_cons ) then
c la simulazione ha trovato una configurazione ottima a seguito di recupero
c-sire2_0020          call tipo_err(istaz,err_ii,msg)
          call tipo_err(istaz,err_ww,msg)
c-sire2_0020-end
          call messtel(fl_int,fl_end,msg_int,msg)
          if (tipo_criterio(pg) .ne. crit_man) then
            msg = 'La simulazione ha trovato una configurazione'//
     *            ' fattibile'
          else
            msg = 'Il criterio manuale ha trovato soluzione '
          endif
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'rilasciando i vincoli operativi.'
          call messtel(fl_int,fl_end,msg_int,msg)

	    if (fl_fatal(pg) .ge. soglia_cons) then
            fl_fatal(pg) = fl_fatal(pg) - soglia_cons
          endif
	    if (fl_fatal(pg_app) .ge. soglia_cons) then
            fl_fatal(pg_app) = fl_fatal(pg_app) - soglia_cons
	    endif
        endif


        if (tipo_criterio(pg) .ne. crit_man) then
c test ridondante???
c_bin
cstaz-corr
c         if (fl_fatal(pg) .gt. 0) then
         if (fl_fatal(pg) .gt. 0 .and. fl_sim) then
cstaz-corr-end
!  la simulazione ha trovato una configurazione ottima a seguito di recupero
           call tipo_err(istaz,err_ii,msg)
           call messtel(fl_int,fl_end,msg_int,msg)
	     msg = 'La simulazione ha trovato una configurazione ottima'//
     *           ' in seguito a recupero.'
	     call messtel(fl_int,fl_end,msg_int,msg)
!  variazione di una delle variabili di set
           if(fl_fatal(pg) .eq. sta_pv) then
	       msg = 'Modificata la PRESSIONE DI MANDATA:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,550) udm_out_str(udm_P,pvsetj(istaz),0),
     *                      udm_out_str(udm_P,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
           else if(fl_fatal(pg) .eq. sta_pm)then
	       msg = 'Modificata la PRESSIONE DI ASPIRAZIONE:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,540) udm_out_str(udm_P,pmsetj(istaz),0),
     *                      udm_out_str(udm_P,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
 	     else if(fl_fatal(pg) .eq. sta_q)then
	       msg = 'Modificata la PORTATA:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,530) udm_out_str(udm_Q,qset_real(istaz),0),
     *                      udm_out_str(udm_Q,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
	     end if
	   end if
c_bin-end
c test ridondante???-end

         if (ce_serie(istaz)) then
c controllo all'uscita del primo stadio:
           if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
             call tipo_err(istaz,err_ii,msg)
             call messtel(fl_int,fl_end,msg_int,msg)
	       msg = 'Pressione di uscita al primo stadio superiore'//
     *             ' alla massima consentita: '
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                      udm_out_str(udm_P,stat_vars(6,pg),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
	     endif
         endif

        else
! NON SI DOVREBBE MAI PASSARE DA QUI
         if (fl_fatal(pg) .lt. 0) then
           call tipo_err(istaz,err_ww,msg)
           call messtel(fl_int,fl_end,msg_int,msg)
           msg = 'Le macchine accese dall''utente (criterio manuale) '//
     *           'non soddisfano il punto di lavoro.' 
	     call messtel(fl_int,fl_end,msg_int,msg)
         endif
        endif

      else
c la simulazione ha trovato una configurazione ottima

cmar_log_sem        if (tipo_criterio(pg) .ne. crit_man) then
c         if (.not.fl_skip_mess) then
cmar_log_sem
cmar_log_sem          if (setflag .eq. 0) then
cmar_log_sem            call tipo_err(istaz,err_ok,msg)
cmar_log_sem            call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem            if (flag_mf) then
cmar_log_sem              msg = 'Assetto spento ok.'
cmar_log_sem            else
cmar_log_sem              msg = 'Centrale spenta ok.'
cmar_log_sem            endif
cmar_log_sem            goto 10
cmar_log_sem          endif


cmar_log_sem 

        if (tipo_criterio(pg) .ne. crit_man) then
c         if (.not.fl_skip_mess) then

          if (setflag .eq. 0) then

            
            if (flag_mf) then
              msg = 'Assetto spento ok.'
            	msg=''
          	msg_int=''
	return
            else
              msg = 'Centrale spenta ok.'
	    	msg=''
          	msg_int=''
	return
            endif
c	msg=''
c	msg_int=''
c            call tipo_err(istaz,err_ok,msg)
c            call messtel(fl_int,fl_end,msg_int,msg)
c            goto 10
          endif
cmar_log_sem 

c         endif ! fl_skip_mess

          if (ce_serie(istaz)) then
cmar-dp
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,p_app)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,p_app)
            call calc_deltapTOT_eff(istaz,pg_app,p_app,pmand_stat_dp)
cmar-dp             call calc_deltap_eff(istaz,pg_app,p_app,pmand_stat_dp)

          else
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
cmar-dp            call calc_deltap_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)

          endif
cmar-dp
          if(.not.fl_sim .and. ce_off(pasp_stat_dp,pmand_stat_dp)) then
            call tipo_err(istaz,err_ii,msg)
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Le pressioni di aspirazione e di mandata calcolate'
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'non determinano una effettiva compressione:'
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                     udm_out_str(udm_P,pmand_stat_dp,1)
            if (ce_accesa(pg)) then
c Centrale spenta con unità necessariamente accese
              call tipo_err(istaz,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Alla centrale che risulta spenta sono'
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = ' state richieste unita'' necessariamente accese.'
              call messtel(fl_int,fl_end,msg_int,msg)
            endif
            else
cevo_2024
              fl_vio_temp = .false.
c intervento per il warning di temperatura
      !do i=1,1
      !    if (mess_num(i).eq.1) then
      !        msg =''
      !        call tipo_err(istaz,err_ww,msg)
      !        call messtel(fl_int,fl_end,msg_int,msg)
      !        call write_mess_stat_cen (pg,fl_vio_temp)
      !        call write_mess_log (pg)
      !    endif
      !enddo
cevo_2024
c           if (.not.fl_skip_mess) then
            iu = first_unit(pg)
	      do i = 1,unit_num(pg)
              if (lstatus(iu+i-1).eq.1.and.unit_avail(iu+i-1).eq.1.or.
     *            lstatus(iu+i-1).eq.0.and.unit_avail(iu+i-1).eq.1 )then
cmar_log_sem                call tipo_err(istaz,err_ok,msg)
cmar_log_sem                call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	          msg = 'La simulazione ha trovato una '//
cmar_log_sem     *                'configurazione ottima.'
c
cmar_log_sem                call tipo_err(istaz,err_ok,msg)
cmar_log_sem                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'La simulazione ha trovato una '//
     *                'configurazione ottima.'

	          	msg=''
          	msg_int=''

c           	return

cmar_log_sem 
                goto 10
	        endif
            enddo
cmar_log_sem            call tipo_err(istaz,err_ok,msg)
cmar_log_sem            call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	      msg = 'Le macchine accese dall''utente soddisfano '//
cmar_log_sem     *            'il punto di lavoro.' 

cmar_log_sem            

	      msg = 'Le macchine accese dall''utente soddisfano '//
     *            'il punto di lavoro.' 
	
	          	msg=''
          	msg_int=''
              
           	return

c           endif ! fl_skip_mess

		endif
cmar_log_sem	  else
c         if (.not.fl_skip_mess) then

cmar_log_sem          call tipo_err(istaz,err_ok,msg)
cmar_log_sem          call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	    msg = 'Le macchine accese dall''utente (criterio manuale) '//
cmar_log_sem     *          'soddisfano il punto di lavoro.' 
cmar_log_sem 
         msg = 'Le macchine accese dall''utente (criterio manuale) '//
     *          'soddisfano il punto di lavoro.' 
      	msg=''
          	msg_int=''
           	return

c         endif ! fl_skip_mess
cmar_log_sem	  endif
 	  endif

10	  continue
        if (.not.fl_skip_mess) then
          call messtel(fl_int,fl_end,msg_int,msg)
        endif
        if (ce_serie(istaz)) then
c controllo all'uscita del primo stadio:
          if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
            call tipo_err(istaz,err_ii,msg)
            call messtel(fl_int,fl_end,msg_int,msg)
	      msg = 'Pressione di uscita al primo stadio superiore'//
     *            ' alla massima consentita: '
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                     udm_out_str(udm_P,stat_vars(6,pg),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	    endif
        endif
	endif

c Vengono scritti sul file di log i messaggi relativi alla simulazione della
c centrale / assetto/stadio1

CMAR_ASS_COND

	IF(.NOT.FLAG_ASS_COND(ISTAZ))THEN
CMAR_ASS_COND


      if (pg_app .ne. pg) then
        if (mess_num(pg) .gt. 0) then
          if (fl_mess_stadio) then
            msg = ''
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Messaggi relativi allo stadio1 del'//
     *            ' macroassetto serie:'
            call messtel(fl_int,fl_end,msg_int,msg)
          endif
c        else
cc controllo all'uscita del primo stadio:
c          if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
c            if (fl_mess_stadio) then
c              msg = ''
c              call messtel(fl_int,fl_end,msg_int,msg)
c              msg = 'Messaggi relativi allo stadio1 del'//
c     *              ' macroassetto serie:'
c              call messtel(fl_int,fl_end,msg_int,msg)
c            endif
c            call tipo_err(istaz,err_ii,msg)
c            call messtel(fl_int,fl_end,msg_int,msg)
c 	      msg = 'Pressione di uscita al primo stadio superiore'//
c     *            ' alla massima consentita: '
c	      call messtel(fl_int,fl_end,msg_int,msg)
c            write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
c     *                     udm_out_str(udm_P,stat_vars(6,pg),1)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c          endif
        endif
      endif

CMAR_ASS_COND

	ENDIF
CMAR_ASS_COND


      fl_vio_temp = .false.
c intervento per il warning di temperatura
	call write_mess_stat_cen (pg,fl_vio_temp)
      
	IF(FLAG_ASS_COND(ISTAZ))THEN

c       IF ( ( ASS1T(ISTAZ).OR.ASS2T(ISTAZ) ) .AND. ASS1T(ISTAZ) )THEN 
        IF ( ASS1T(ISTAZ) )THEN 
      if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
      endif
      call write_mess_log (pg)
        ENDIF

C	 ELSEIF(( ASS1T(ISTAZ).OR.ASS2T(ISTAZ) ) .AND. ASS2T(ISTAZ))THEN

        IF(  ASS2T(ISTAZ)  )THEN

	 if (fl_fatal(pg+1).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg+1) = fl_fatal(pg+1) + soglia_vio_temp
      endif
cmar_ass_cond per evitare di scrivere 2 vv sul log testuale:

      if(.not.ASS1T(ISTAZ)  )then
      call write_mess_log (pg)
	endif

	  ENDIF
    
C       ELSEIF (( ASS1T(ISTAZ).AND.ASS2T(ISTAZ) ))THEN
C
C      if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
C        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
C      endif
     
C
C	if (fl_fatal(pg+1).ge.0 .and. fl_vio_temp) then
C        fl_fatal(pg+1) = fl_fatal(pg+1) + soglia_vio_temp
C      endif
C      call write_mess_log (pg)


C       ENDIF


	ELSE
CMAR_ SE NN è AD ASSETTI CONDIVISI

        if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
      endif
      call write_mess_log (pg)

	ENDIF



CMAR_ASS_COND

      if (pg_app .ne. pg) then
c Vengono scritti sul file di log i messaggi relativi alla simulazione
c dello stadio2
        if (mess_num(pg_app) .gt. 0) then
          msg = ''
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Messaggi relativi allo stadio2 del macroassetto serie:'
          call messtel(fl_int,fl_end,msg_int,msg)
        endif
        fl_vio_temp = .false. 
        call write_mess_stat_cen (pg_app,fl_vio_temp)
        if (fl_fatal(pg_app).ge.0 .and. fl_vio_temp) then
          fl_fatal(pg_app) = fl_fatal(pg_app) + soglia_vio_temp
          fl_fatal(pg) = fl_fatal(pg_app)
        endif
        call write_mess_log (pg_app)
      endif

      return

530   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('p asp richiesta: ',A10,' - p asp proposta: ',A25)
550   format('p man richiesta: ',A10,' - p man proposta: ',A25)
610   format('Q: ',A25)
620   format('p asp.: ',A10,' - p man.: ',A25)
630   format('p man.: ',A10,' - p man.max: ',A25)
640   format('p asp.: ',A25)
650   format('p man.: ',A25)
710   format('k min: ',A10,' - k max: ',A10)

	end





cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
c
c------------------Subroutine per controlli sulle centrali--------------------------

      subroutine verstel_stat_nw_mod(istaz,pg,pg_app,flag_mf)
c-----------------------------------------------------------------------------------
c     Controllo relativi ad una centrale da eseguire dopo la simulazione
c-----------------------------------------------------------------------------------
      implicit none

      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/jc.inc'
   	include '../inc/err.inc'
   	include '../inc/flag.inc'
   	include '../inc/stazione.inc'
   	include '../inc/top_app_ele.inc'
   	include '../inc/stations.inc'
   	include '../inc/units.inc'
   	include '../inc/mf_units.inc'
   	include '../inc/types.inc'
   	include '../inc/messag.inc'
   	include '../inc/conv.inc'
   	include '../inc/staznw_par.inc'

	include '../inc/ASS_COND.INC'


      integer*4 istaz       ! I) indice stazione nella struttura dati completa
     *         ,pg          ! I) pgrato(istaz)
     *         ,pg_app      ! I) pgrato_app(istaz)
      logical*2 flag_mf     ! I) flag multifunzione

c l_ko       !flag violazioni KO 'soluzione non trovata
c l_fat      !flag violazioni fatal soluzione non trovata ma con risultati
c l_info     !flag violazioni warning soluzione non accettabile
c l_warn     !flag violazioni informational soluzione non accettabile con variz.SET

      integer*4 iu,i,j,im,iv,setflag
      integer*4 vert_type,type_act_loc(maxunits),
     *          ntipi_act,nunit_act,max_nunitact,k,ii1
      real*4    pasp_stat_dp,pmand_stat_dp,p_app

      character*(max_len_str) udm_out_str
      external                udm_out_str
	character*100 msg_int !intestazione messaggi di errore
     *             ,msg     !messaggio associato alla violazione riscontrata  

	logical*2 l_crit_ko,ltest_crit_giri,ltest_crit
	logical*2 fl_vio_temp
c nel caso di assetto serie senza punto ko, evita di mettere l'intestazione relativa
c ai due stadi per il messaggio relativo ad head e flowm
	logical*2 fl_mess_stadio
      logical*2 ce_accesa,ce_off,ce_avail,ce_serie
      external  ce_accesa,ce_off,ce_avail,ce_serie
	logical*2 fl_int      !flag intestazione: TRUE (già stampata)
	logical*2 fl_end
	real*4 qset_real
	external qset_real
	logical*2 fl_skip_mess
c-----------------------------------------------------------------------------------
c---> Prepara l'intestazione dell'elenco di messaggi relativi alla centrale
      if (flag_mf) then
CMRA_ASS_COND
      
	IF(FLAG_ASS_COND(ISTAZ-1))THEN
      
	if (.not.no_acc(istaz)) then
	hstatj(istaz)=hstatj(istaz-1)
      endif

cmarrrrrrrrrrrrrrrrrrrrrrrr      RETURN
	ENDIF
CMAR_ASS_COND
        IF(FLAG_ASS_COND(ISTAZ))THEN
cmar_ass_cond
      IF(FLOWM(PG).EQ.0)THEN
	FL_FATAL(PG)=8
	ELSEIF(FLOWM(PG+1).EQ.0)THEN
	FL_FATAL(PG+1)=8
	ENDIF

cmar_ass_con


        msg_int='CENTRALE MULTIFUNZIONE ad assetti condivisi'// '  : 
     *'//trim(snomtv(istaz) )
        ELSE
        msg_int='CENTRALE MULTIFUNZIONE'// ' : '//trim(snomtv(istaz))//
     *          '         ASSETTO:'//trim(sigla_ass(pg))

c      Pg_app=pg+1
        ENDIF
     	else
        msg_int=trim(te_stri(e_ce))//' : '//trim(snomtv(istaz)) 
      end if

c inizializzazioni
	fl_mess_stadio = .true.
	fl_skip_mess = .false.

c confronta lo stato finale della centrale con quello richiesto per essa dall'utente
c Tale messaggio non deve comparire in Simcent perchè in caso di recupero viene modificata
c la variabile di set ma non il tipo di set.
c Il messaggio può invece comparire nella simulazione di rete, poichè come accade per gli 
c altri elementi, in questo caso la centrale può anche subire una modifica del tipo di set
c_bin
c-simcent-corr      call  verst_bin(istaz,msg_int,fl_int,fl_end)
c ALTRIMENTI: si può chiamare la verst_bin e uscire dopo l'inizializzazione di
c             flertj
      if (.not. fl_sim) then
        call verst_bin(istaz,msg_int,fl_int,fl_end)
      else
c per simcent vengono inizializzate queste variabili il cui valore veniva
c inizializzato nella verst_bin
        fl_int = .false.
        fl_end = .false.
        flertj(istaz) = err_ok
      endif
c-simcent-corr-end

      call set_stato_stazione(setflag,istaz)
      if (set_flag(pg) .ne. setflag) then
        if (setflag .eq. 1) then
c la centrale ha condizioni che non ne permettono la simulabilità, pertanto è stato
c cambiato il set_flag da 1 a 0
          call tipo_err(istaz,err_ff,msg)
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'La centrale non ha condizioni che ne permettano la '//
     *          'simulazione.'
         call messtel(fl_int,fl_end,msg_int,msg)
c viene definito il fl_fatal per avere comunque la messaggistica relativa alla centrale
c-din-corr          if (.not.fl_sim) fl_fatal(pg) = -1
          if (.not.fl_sim) then
            if (fl_fatal(pg) .ge. 0) fl_fatal(pg) = -1
          endif
c-din-corr-end
        endif
      endif

c TRATTAMENTO DEL FL_FATAL  = 0 => centrale ok
c                           = sta_pm/sta_pv/sta_q => centrale ok, ma con recupero
c                           = -k => centrale ko, con errore fatale k
      if (fl_fatal(pg) .lt. 0) then
c caso 1) la centrale non è stata simulata non avendo superato i controlli della check_ce
c caso 2) la simulazione non ha trovato soluzione
c-din-corr
       if (fl_fatal(pg) .lt. -100000) then
         call tipo_err(istaz,err_ff,msg)
         call messtel(fl_int,fl_end,msg_int,msg)
         msg = 'Attivato con tlc un tbg che ha spento l''unico tbg'//
     *         ' acceso nell''assetto coniugato.'
         call messtel(fl_int,fl_end,msg_int,msg)

c-din-corr       if (fl_fatal(pg) .lt. -10000) then
       elseif (fl_fatal(pg) .lt. -10000) then
c-din-corr-end
c rappresentazione del punto ko (lfat = T)
         call tipo_err(istaz,err_ff,msg)
         call messtel(fl_int,fl_end,msg_int,msg)
         if (tipo_criterio(pg) .ne. crit_man) then
           msg = 'La simulazione non ha trovato una configurazione'//
     *           ' ottima.'
         else
	     msg = 'Le macchine accese dall''utente (criterio manuale)'//
     *           ' non soddisfano il punto di lavoro.' 
         endif
         call messtel(fl_int,fl_end,msg_int,msg)

       else
c senza rappresentazione del punto ko
        if (set_flag(pg) .eq. setflag) then
           call tipo_err(istaz,err_ff,msg)
	     call messtel(fl_int,fl_end,msg_int,msg)
           if (fl_sim) then !simcent
             call tipo_err(istaz,err_ko,msg)
           endif
           if (tipo_criterio(pg) .ne. crit_man) then
             msg = 'La simulazione non ha trovato una configurazione '//
     * 	         'ottima.'
           else
             msg = 'Le macchine accese dall''utente (criterio '// 
     *             'manuale) non soddisfano il punto di lavoro.'
           endif
	     call messtel(fl_int,fl_end,msg_int,msg)
	  endif

        if (fl_fatal(pg) .lt. -1000) then
          call tipo_err(istaz,err_ff,msg)
          if (fl_sim) then !simcent
            call tipo_err(istaz,err_ko,msg)
          endif
	    call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Modulo di simulazione non previsto.'
	    call messtel(fl_int,fl_end,msg_int,msg)

        elseif (fl_fatal(pg) .lt. -100) then
          call tipo_err(istaz,err_ff,msg)
          if (fl_sim) then !simcent
            call tipo_err(istaz,err_ko,msg)
          endif
	    call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Impossibile effettuare il recupero: unita'' condivise.'
	    call messtel(fl_int,fl_end,msg_int,msg)
        else
          if (ce_serie(istaz)) then
            fl_mess_stadio = .false.
cmar-dp
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,p_app)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,p_app)
            call calc_deltapTOT_eff(istaz,pg_app,p_app,pmand_stat_dp)
cmar-dp          call calc_deltap_eff(istaz,pg_app,p_app,pmand_stat_dp)
          else
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
	    endif
cmar-dp
c          if (set_flag(pg).eq.0) then  !sta_fr,sta_on,sta_off,sta_nr
          if (setflag .eq. 0) then
c stazione spenta (sta_fr,sta_on,sta_off,sta_nr): associo un messaggio
c alle violazioni individuate dalla check_ce
c controllo sui vincoli fisici (non si dovrebbe passare se lo spegnimento è OK)
		  if (ce_accesa(pg)) then
c non ha senso in SIMCENT ma potrebbe essere restituito dallo stazionario
              call tipo_err(istaz,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Stazione spenta con unità necessariamente accese.'
	        call messtel(fl_int,fl_end,msg_int,msg)
            endif

c test ridondante???
	      if (.not.ce_off(pasp_stat_dp,pmand_stat_dp)) then
              call tipo_err(istaz,err_ff,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Richiesta una compressione effettiva'//
     *              ' ad una stazione spenta:'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
		  endif 
c test ridondante???-end

          else
c  tale messaggio non dovrebbe mai comparire sul log dato che il controllo sulla
c  disponibilità dei turbogruppi viene eseguito in maschera prima di lanciare la
c  simulazione;in particolare almeno un tbg deve essere dichiarato disponibile o 
c  acceso           
c PER LO STAZIONARIO PUO' COMPARIRE, perché il test viene fatto su setflag, che
c è lo stato della centrale trovato dalla rete
            if (ce_serie(istaz)) then
              if (.not.ce_avail(pg) .or. .not.ce_avail(pg_app)) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Centrale accesa ma nessuna unità di '//
     *                'compressione disponibile.'
	          call messtel(fl_int,fl_end,msg_int,msg)
              endif
            else
              if (.not.ce_avail(pg)) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Centrale accesa ma nessuna unità di '//
     *                'compressione disponibile.'
	          call messtel(fl_int,fl_end,msg_int,msg)
              endif
		  endif
            if (ce_serie(istaz)) then
c              if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
cc controllo all'uscita del primo stadio
c                call tipo_err(istaz,err_ii,msg)
c                call messtel(fl_int,fl_end,msg_int,msg)
c	          msg = 'Pressione di uscita al primo stadio superiore'//
c     *                ' alla massima consentita: '
c	          call messtel(fl_int,fl_end,msg_int,msg)
c                write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
c     *                         udm_out_str(udm_P,stat_vars(6,pg),1)
c	          call messtel(fl_int,fl_end,msg_int,msg)
c	        endif
              if (pmand_stat(pg_app).gt.stat_vars(6,pg_app)+repsP) then
cmar_rev_log_sem                call tipo_err(istaz,err_ff,msg)
                call tipo_err(istaz,err_ww,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
	          msg = 'Pressione di mandata superiore alla '//
     *                'massima consentita: '
	          call messtel(fl_int,fl_end,msg_int,msg)
                write(msg,630) udm_out_str(udm_P,pmand_stat(pg_app),0),
     *                         udm_out_str(udm_P,stat_vars(6,pg_app),1)
	          call messtel(fl_int,fl_end,msg_int,msg)
	        endif
	      else
              if (pmand_stat(pg) .gt. stat_vars(6,pg)+repsP) then
cmar_rev_log_sem                 call tipo_err(istaz,err_ff,msg)
                  call tipo_err(istaz,err_ww,msg)
                 if (fl_sim) then !simcent
                   call tipo_err(istaz,err_ko,msg)
                 endif
                 call messtel(fl_int,fl_end,msg_int,msg)
	           msg = 'Pressione di mandata superiore alla '//
     *                 'massima consentita: '
	           call messtel(fl_int,fl_end,msg_int,msg)
                 write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                          udm_out_str(udm_P,stat_vars(6,pg),1)
	           call messtel(fl_int,fl_end,msg_int,msg)
	        endif
	      endif
 	      if (pasp_stat_dp .gt. pmand_stat_dp) then
c Viene restituito sia da SIMCENT sia dallo STAZIONARIO nel caso in cui i delta p
c dovuti ai piping/filtri/air-cooler
c (il controllo sul segno delle pressioni viene eseguito in maschera prima di
c lanciare la simulazione, ma riguarda le pressioni iniziali pasp_stat e pmand_stat)
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Pressione di mandata inferiore alla'//
     *              ' pressione di aspirazione:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

 	      if (pasp_stat_dp .le. 0) then
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Delta pressione asp. non corretto che rende la'
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'pressione di aspirazione al compressore negativa:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,640) udm_out_str(udm_P,pasp_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

 	      if(pmand_stat_dp .le. 0) then
c Questo messaggio dovrebbe comparire solo sullo stazionario dove non c'è
c alcun controllo sulle pressioni di aspirazione e mandata, in quanto
c calcolate dal sistema che risolve la rete
              call tipo_err(istaz,err_ff,msg)
              if (fl_sim) then !simcent
                call tipo_err(istaz,err_ko,msg)
              endif
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Delta pressione man. non corretto che rende la'
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'pressione di mandata al compressore negativa:'
              call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,650) udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            end if

	      if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
c non ha senso in SIMCENT ma potrebbe essere restituito dallo stazionario
c err_ko???
              call tipo_err(istaz,err_ii,msg)
              call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'Le pressioni di aspirazione e di mandata'
	        call messtel(fl_int,fl_end,msg_int,msg)
              msg = 'calcolate rendono spenta la centrale'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                       udm_out_str(udm_P,pmand_stat_dp,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
		  end if 

c Controllo sulla corretta definizione delle parabole limite per l'applicazione del
c criterio di equidistanza:errore bloccante se la parabola massima cade a sinistra
c della parabola minima (per l'assetto in serie i tbg del primo stadio sono i
c medesimi del secondo stadio)
            if (tipo_criterio(pg) .eq. crit_equi) then
c controllo parabole per il criterio di equidistanza
              l_crit_ko = .false.
              iu = first_unit(pg)
              do j = 1,unit_num(pg)
                 i = iu + j - 1
	           if (unit_crit_max(i).gt.unit_crit_min(i)) then
                    l_crit_ko = .true.
                 end if
  	        end do
              if (l_crit_ko) then
                call tipo_err(istaz,err_ff,msg)
                if (fl_sim) then !simcent
                  call tipo_err(istaz,err_ko,msg)
                endif
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Risultano invertite le parabole necessarie'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'al criterio di equidistanza:'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'controllare le percentuali inserite.'
                call messtel(fl_int,fl_end,msg_int,msg)
              endif
            endif
            l_crit_ko = .false.
            ltest_crit_giri = .false.
            if (tipo_criterio(pg) .eq. crit_giri) then
c calcolo il tipo di centrale (semplice, composta o mista) in base al valore dello
c stato dei tbg per unità necessariamente accese
c definisco type_act_loc in base al valore di unit_avail/unit_attiv_tlc rispettivamente
c per stazionario e dinamica
              iu = first_unit(pg)
              do i = 1,type_num(pg)
                j = first_type(pg)+i-1
                type_act_loc(i) = 0
                do k = 1,type_quant(j)
                  ii1 = iu+k-1
                  if (unit_avail(ii1).gt.1) then
c fare il test anche su unit_attiv_tlc????
                    type_act_loc(i) = type_act_loc(i) + 1
                    ltest_crit_giri = .true.
                  end if
                end do
                iu = iu+type_quant(j)
              end do
              if (ltest_crit_giri) then
                call init_vert_type(pg,type_act_loc,vert_type)
                if (vert_type .eq. tipo_mist) then
c centrale mista e tipo di configurazione dei turbogruppi è in serie
                  l_crit_ko = .true.
                elseif (vert_type .eq. tipo_comp) then
c per una centrale di tipo composto, non ci devono essere attivi tipi diversi
c calcolo di ntipi_act e nunit_act
                  ntipi_act = 0
                  nunit_act = 0
                  do i = 1, type_num(pg)
                    if (type_act_loc(i) .gt. 0) then
                      ntipi_act = ntipi_act + 1
                      nunit_act = nunit_act + 1
                    endif
                  enddo
                  if (ntipi_act .gt. 1) then
                    l_crit_ko = .true.
                  endif
c controllo che non venga superato il massimo numero di tbg bifase
                  max_nunitact = maxunits/2
                  if (nunit_act .gt. max_nunitact) then
                    l_crit_ko = .true.
                  endif
                end if
                if (l_crit_ko) then
                  call tipo_err(istaz,err_ff,msg)
                  if (fl_sim) then !simcent
                    call tipo_err(istaz,err_ko,msg)
                  endif
                  call messtel(fl_int,fl_end,msg_int,msg)
                  msg = 'Stato dei turbogruppi non consistente'
                  call messtel(fl_int,fl_end,msg_int,msg)
                  msg = 'per il criterio dei giri'
                  call messtel(fl_int,fl_end,msg_int,msg)
                endif
              endif
            else
c criterio delle portate e dell'equidistanza
              iu = first_unit(pg)
              do i = 1,type_num(pg)
                j = first_type(pg)+i-1
                type_act_loc(i) = 0
                do k = 1,type_quant(j)
                  ii1 = iu+k-1
                  if (unit_avail(ii1).gt.1) then
                    type_act_loc(i) = type_act_loc(i) + 1
                    ltest_crit = .true.
                  end if
                end do
                iu = iu+type_quant(j)
              end do
              if (ltest_crit) then
                call init_vert_type(pg,type_act_loc,vert_type)
                if (vert_type .eq. tipo_mist .and.
     *              stat_ord(pg) .eq. f_serie         ) then
                  l_crit_ko =  .true.
                  if (l_crit_ko) then
                    call tipo_err(istaz,err_ff,msg)
                    if (fl_sim) then !simcent
                      call tipo_err(istaz,err_ko,msg)
                    endif
                    call messtel(fl_int,fl_end,msg_int,msg)
                    msg = 'Centrale mista con configurazione dei'
                    call messtel(fl_int,fl_end,msg_int,msg)
                    msg = 'turbogruppi in serie'
                    call messtel(fl_int,fl_end,msg_int,msg)
                  endif
                endif
              endif
            endif
	    endif
        endif
       endif

      else if (fl_fatal(pg).gt.0 .or. fl_fatal(pg_app).gt.0) then
        if (fl_fatal(pg) .ge. soglia_cons .or.
     *      fl_fatal(pg_app) .ge. soglia_cons ) then
c la simulazione ha trovato una configurazione ottima a seguito di recupero
c-sire2_0020          call tipo_err(istaz,err_ii,msg)
          call tipo_err(istaz,err_ww,msg)
c-sire2_0020-end
          call messtel(fl_int,fl_end,msg_int,msg)
          if (tipo_criterio(pg) .ne. crit_man) then
            msg = 'La simulazione ha trovato una configurazione'//
     *            ' fattibile'
          else
            msg = 'Il criterio manuale ha trovato soluzione '
          endif
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'rilasciando i vincoli operativi.'
          call messtel(fl_int,fl_end,msg_int,msg)

	    if (fl_fatal(pg) .ge. soglia_cons) then
            fl_fatal(pg) = fl_fatal(pg) - soglia_cons
          endif
	    if (fl_fatal(pg_app) .ge. soglia_cons) then
            fl_fatal(pg_app) = fl_fatal(pg_app) - soglia_cons
	    endif
        endif
        if (tipo_criterio(pg) .ne. crit_man) then
c test ridondante???
c_bin
cstaz-corr
c         if (fl_fatal(pg) .gt. 0) then
         if (fl_fatal(pg) .gt. 0 .and. fl_sim) then
cstaz-corr-end
!  la simulazione ha trovato una configurazione ottima a seguito di recupero
           call tipo_err(istaz,err_ii,msg)
           call messtel(fl_int,fl_end,msg_int,msg)
	     msg = 'La simulazione ha trovato una configurazione ottima'//
     *           ' in seguito a recupero.'
	     call messtel(fl_int,fl_end,msg_int,msg)
!  variazione di una delle variabili di set
           if(fl_fatal(pg) .eq. sta_pv) then
	       msg = 'Modificata la PRESSIONE DI MANDATA:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,550) udm_out_str(udm_P,pvsetj(istaz),0),
     *                      udm_out_str(udm_P,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
           else if(fl_fatal(pg) .eq. sta_pm)then
	       msg = 'Modificata la PRESSIONE DI ASPIRAZIONE:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,540) udm_out_str(udm_P,pmsetj(istaz),0),
     *                      udm_out_str(udm_P,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
 	     else if(fl_fatal(pg) .eq. sta_q)then
	       msg = 'Modificata la PORTATA:'
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,530) udm_out_str(udm_Q,qset_real(istaz),0),
     *                      udm_out_str(udm_Q,valsjc(istaz),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
	     end if
	   end if
c_bin-end
c test ridondante???-end

         if (ce_serie(istaz)) then
c controllo all'uscita del primo stadio:
           if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
             call tipo_err(istaz,err_ii,msg)
             call messtel(fl_int,fl_end,msg_int,msg)
	       msg = 'Pressione di uscita al primo stadio superiore'//
     *             ' alla massima consentita: '
	       call messtel(fl_int,fl_end,msg_int,msg)
             write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                      udm_out_str(udm_P,stat_vars(6,pg),1)
	       call messtel(fl_int,fl_end,msg_int,msg)
	     endif
         endif

        else
! NON SI DOVREBBE MAI PASSARE DA QUI
         if (fl_fatal(pg) .lt. 0) then
           call tipo_err(istaz,err_ww,msg)
           call messtel(fl_int,fl_end,msg_int,msg)
           msg = 'Le macchine accese dall''utente (criterio manuale) '//
     *           'non soddisfano il punto di lavoro.' 
	     call messtel(fl_int,fl_end,msg_int,msg)
         endif
        endif

      else
c la simulazione ha trovato una configurazione ottima
        if (tipo_criterio(pg) .ne. crit_man) then
c         if (.not.fl_skip_mess) then
          if (setflag .eq. 0) then
cmar_log_sem            call tipo_err(istaz,err_ok,msg)
cmar_log_sem            call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem            if (flag_mf) then
cmar_log_sem              msg = 'Assetto spento ok.'
cmar_log_sem            else
cmar_log_sem              msg = 'Centrale spenta ok.'




            endif
            goto 10
          endif
c         endif ! fl_skip_mess

          if (ce_serie(istaz)) then
cmar-dp
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,p_app)
cmar-dp          call calc_deltap_eff(istaz,pg,pasp_stat_dp,p_app)
            call calc_deltapTOT_eff(istaz,pg_app,p_app,pmand_stat_dp)
cmar-dp             call calc_deltap_eff(istaz,pg_app,p_app,pmand_stat_dp)

          else
            call calc_deltapTOT_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)
cmar-dp            call calc_deltap_eff(istaz,pg,pasp_stat_dp,pmand_stat_dp)

          endif
cmar-dp
          if(.not.fl_sim .and. ce_off(pasp_stat_dp,pmand_stat_dp)) then
            call tipo_err(istaz,err_ii,msg)
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Le pressioni di aspirazione e di mandata calcolate'
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'non determinano una effettiva compressione:'
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,620) udm_out_str(udm_P,pasp_stat_dp,0),
     *                     udm_out_str(udm_P,pmand_stat_dp,1)
            if (ce_accesa(pg)) then
c Centrale spenta con unità necessariamente accese
              call tipo_err(istaz,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = 'Alla centrale che risulta spenta sono'
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg = ' state richieste unita'' necessariamente accese.'
              call messtel(fl_int,fl_end,msg_int,msg)
            endif
          else
c           if (.not.fl_skip_mess) then
            iu = first_unit(pg)
	      do i = 1,unit_num(pg)
              if (lstatus(iu+i-1).eq.1.and.unit_avail(iu+i-1).eq.1.or.
     *            lstatus(iu+i-1).eq.0.and.unit_avail(iu+i-1).eq.1 )then
cmar_log_sem                call tipo_err(istaz,err_ok,msg)
cmar_log_sem                call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	          msg = 'La simulazione ha trovato una '//
cmar_log_sem     *                'configurazione ottima.'
                goto 10
	        endif
            enddo
cmar_log_sem            call tipo_err(istaz,err_ok,msg)
cmar_log_sem            call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	      msg = 'Le macchine accese dall''utente soddisfano '//
cmar_log_sem     *            'il punto di lavoro.' 
c           endif ! fl_skip_mess

		endif
cmar_log_sem	  else
c         if (.not.fl_skip_mess) then

cmar_log_sem          call tipo_err(istaz,err_ok,msg)
cmar_log_sem          call messtel(fl_int,fl_end,msg_int,msg)
cmar_log_sem	    msg = 'Le macchine accese dall''utente (criterio manuale) '//
cmar_log_sem     *          'soddisfano il punto di lavoro.' 
c         endif ! fl_skip_mess
cmar_log_sem	  endif

10	  continue
        if (.not.fl_skip_mess) then
          call messtel(fl_int,fl_end,msg_int,msg)
        endif
        if (ce_serie(istaz)) then
c controllo all'uscita del primo stadio:
          if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
            call tipo_err(istaz,err_ii,msg)
            call messtel(fl_int,fl_end,msg_int,msg)
	      msg = 'Pressione di uscita al primo stadio superiore'//
     *            ' alla massima consentita: '
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
     *                     udm_out_str(udm_P,stat_vars(6,pg),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	    endif
        endif
	endif

c Vengono scritti sul file di log i messaggi relativi alla simulazione della
c centrale / assetto/stadio1
      if (pg_app .ne. pg) then
        if (mess_num(pg) .gt. 0) then
          if (fl_mess_stadio) then
            msg = ''
            call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Messaggi relativi allo stadio1 del'//
     *            ' macroassetto serie:'
            call messtel(fl_int,fl_end,msg_int,msg)
          endif
c        else
cc controllo all'uscita del primo stadio:
c          if (pmand_stat(pg) .gt. stat_vars(6,pg)) then
c            if (fl_mess_stadio) then
c              msg = ''
c              call messtel(fl_int,fl_end,msg_int,msg)
c              msg = 'Messaggi relativi allo stadio1 del'//
c     *              ' macroassetto serie:'
c              call messtel(fl_int,fl_end,msg_int,msg)
c            endif
c            call tipo_err(istaz,err_ii,msg)
c            call messtel(fl_int,fl_end,msg_int,msg)
c 	      msg = 'Pressione di uscita al primo stadio superiore'//
c     *            ' alla massima consentita: '
c	      call messtel(fl_int,fl_end,msg_int,msg)
c            write(msg,630) udm_out_str(udm_P,pmand_stat(pg),0),
c     *                     udm_out_str(udm_P,stat_vars(6,pg),1)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c          endif
        endif
      endif
      fl_vio_temp = .false.
c intervento per il warning di temperatura
	call write_mess_stat_cen (pg,fl_vio_temp)
      
	IF(FLAG_ASS_COND(ISTAZ))THEN

c       IF ( ( ASS1T(ISTAZ).OR.ASS2T(ISTAZ) ) .AND. ASS1T(ISTAZ) )THEN 
        IF ( ASS1T(ISTAZ) )THEN 
      if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
      endif
      call write_mess_log (pg)
        ENDIF

C	 ELSEIF(( ASS1T(ISTAZ).OR.ASS2T(ISTAZ) ) .AND. ASS2T(ISTAZ))THEN

        IF(  ASS2T(ISTAZ)  )THEN

	 if (fl_fatal(pg+1).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg+1) = fl_fatal(pg+1) + soglia_vio_temp
      endif
cmar_ass_cond per evitare di scrivere 2 vv sul log testuale:

      if(.not.ASS1T(ISTAZ)  )then
      call write_mess_log (pg)
	endif

	  ENDIF
    
C       ELSEIF (( ASS1T(ISTAZ).AND.ASS2T(ISTAZ) ))THEN
C
C      if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
C        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
C      endif
     
C
C	if (fl_fatal(pg+1).ge.0 .and. fl_vio_temp) then
C        fl_fatal(pg+1) = fl_fatal(pg+1) + soglia_vio_temp
C      endif
C      call write_mess_log (pg)


C       ENDIF


	ELSE
CMAR_ SE NN è AD ASSETTI CONDIVISI

        if (fl_fatal(pg).ge.0 .and. fl_vio_temp) then
        fl_fatal(pg) = fl_fatal(pg) + soglia_vio_temp
      endif
      call write_mess_log (pg)

	ENDIF



CMAR_ASS_COND

      if (pg_app .ne. pg) then
c Vengono scritti sul file di log i messaggi relativi alla simulazione
c dello stadio2
        if (mess_num(pg_app) .gt. 0) then
          msg = ''
          call messtel(fl_int,fl_end,msg_int,msg)
          msg = 'Messaggi relativi allo stadio2 del macroassetto serie:'
          call messtel(fl_int,fl_end,msg_int,msg)
        endif
        fl_vio_temp = .false. 
        call write_mess_stat_cen (pg_app,fl_vio_temp)
        if (fl_fatal(pg_app).ge.0 .and. fl_vio_temp) then
          fl_fatal(pg_app) = fl_fatal(pg_app) + soglia_vio_temp
          fl_fatal(pg) = fl_fatal(pg_app)
        endif
        call write_mess_log (pg_app)
      endif

      return

530   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('p asp richiesta: ',A10,' - p asp proposta: ',A25)
550   format('p man richiesta: ',A10,' - p man proposta: ',A25)
610   format('Q: ',A25)
620   format('p asp.: ',A10,' - p man.: ',A25)
630   format('p man.: ',A10,' - p man.max: ',A25)
640   format('p asp.: ',A25)
650   format('p man.: ',A25)
710   format('k min: ',A10,' - k max: ',A10)

	end




c****************************************************************************************
      subroutine write_mess_log (ivert)
c--------------------------------------------------------------------------------
c   Scrive sul file di log i messaggi preparati in area common dall subroutine
c   write_mess_stat_cen
c--------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/messag.inc'

      integer*4 ivert  ! I) indice della stazione nel vettore delle centrali

      integer*4 i,j
c--------------------------------------------------------------------------------
      do i = 1,mess_num(IVERT)
        do j = 1,num_rig_mess(i)
!-j-esima riga che forma la descrizione dell'i-esimo messaggio
          call gest_error (3,0,'',mess_descr(j,i),0)
        end do
      end do

	return 
	end   !write_mess_log


c------------------------------------------------------------
c-intervento_temp
c      Subroutine write_mess_stat_cen (vert_ind)
      Subroutine write_mess_stat_cen (vert_ind,fl_vio_temp)
c-intervento_temp-end
c
*****************************************************************************
c Modificati i codici di errore in modo da unire in una unica subroutine
c i messaggi di errore contenuti in:
c - mess_stat:       Messaggi per una stazione elementare semplice, composta in
c                    parallelo e serie con criterio di uguaglianza dei giri
c - mess_stat_multi: Messaggi per una stazione elementare composta in serie
c                    con il criterio di equidistanza o di uguaglianza di portata
c - mess_stat_mixed: Messaggi per una stazione elementare mista (semplice, 
c                    composta in parallelo e mista) con criterio di equidistanza
c                    o di uguaglianza di portata
c I messaggi della mess_stat erano unicamente quelli del criterio UG.
c Ora i messaggi di errore sono stati organizzati in modo tale da racchiudere
c tra 1 e 49 quelli del criterio UG, ad eccezione di alcuni messaggi che
c valgono anche per gli altri criteri, fatto segnalato con un commento opportuno.
c Tra 50 e 98 sono i messaggi per il criterio UP, EQ, MAN e KO.
c Viene indicato anche se alcuni messaggi non vengono più utilizzati.
cgpe-prv
c Dopo aver chiamato la write_mess_stat_cen che prepara i messaggi in area
c common e' necessario scrivere tali messaggi su file di log:
c!-scrittura sul file di log dei messaggi associati alla centrale
c        do i = 1,mess_num(IVERT)
c          do j = 1,num_rig_mess(i)
c!-j-esima riga che forma la descrizione dell'i-esimo messaggio
c            call gest_error (2,0,' ',mess_descr(j,i),*9999)
c          enddo
c        enddo
c E' da commentare poi la scrittura sul file MESSAGGI_OUT???
cgpe-prv-end
*****************************************************************************
c-----------------------------------------------------------------------
        implicit none
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/messag.inc'
c
	INTEGER*4  vert_ind     ! I) indice della stazione

	INTEGER*4  i,num_iter
c-intervento_temp
      logical*2 fl_vio_temp
c-intervento_temp-end
c--------------------------------------------------------------------
	do i=1,mess_num(vert_ind)
c          num_iter=int(mess_type(i,vert_ind)/100)
c          mess_type(i,vert_ind)=mess_type(i,vert_ind)-num_iter*100

********* messaggio in comune a tutti i criteri *********
	  if(mess_type(i,vert_ind).eq.1) then  ! NON VIENE SCRITTO
cgpe            write(mess_descr(1,i),100) num_iter
cgpe	    write(mess_descr(2,i),200)
cgpe     *' Simulazione terminata con errore'
cgpe	    write(mess_descr(3,i),200)
cgpe     *' La stazione non riesce a lavorare tutto il gas'
cgpe!	    write(mess_descr(4,i),200)
cgpe!     *' e non vi sono piu'' compressori disponibili.'
cgpe!            num_rig_mess(i) = 4
cgpe            num_rig_mess(i) = 3
	    write(mess_descr(1,i),200)
     *'Simulazione terminata con errore.'
	    write(mess_descr(2,i),200)
     *'La stazione non riesce a lavorare tutto il gas.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
cgpe	  else if(mess_type(i,vert_ind).eq.2) then ! NU
cgpe	    write(mess_descr(1,i),200)
cgpe     *'Il riciclo puo'' essere evitato spegnendo un compressore.'
cgpe            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
cgpe	  else if(mess_type(i,vert_ind).eq.3) then ! NU
cgpe	    write(mess_descr(1,i),200)
cgpe     *'Piu'' di un compressore sulla curva di riciclo.'
cgpe            num_rig_mess(i) = 1
*********************************************************

******** messaggio reso comune x tutti i criteri ********
	  else if(mess_type(i,vert_ind).eq.4) then ! da 2 a 4 x paral ! RETE
	    write(mess_descr(1,i),200)
     *'La portata richiesta e'' minore o uguale a zero.'
	    write(mess_descr(2,i),200)
     *'La stazione viene spenta.'
            num_rig_mess(i) = 2
*********************************************************

******** messaggio reso comune x tutti i criteri ********
	  else if(mess_type(i,vert_ind).eq.5) then ! da 4 a 5 x paral ! RETE
	    write(mess_descr(1,i),200)
     *'Pressione di ingresso maggiore della pressione di uscita.'
	    write(mess_descr(2,i),200)
     *'La stazione viene spenta.'
            num_rig_mess(i) = 2
*********************************************************

******** messaggio reso comune x tutti i criteri ********
	  else if(mess_type(i,vert_ind).eq.6) then ! da 5 a 6 x paral
	    write(mess_descr(1,i),200)
     *'Altezza adiabatica eccessiva.'
c	    write(mess_descr(2,i),200)
c     *'Viene imposto un nuovo set di pressione'
            num_rig_mess(i) = 1
*********************************************************

******** messaggio reso comune x tutti i criteri ********
	  else if(mess_type(i,vert_ind).eq.7) then ! da 6 a 7 x paral
	    write(mess_descr(1,i),200)
     *'Altezza adiabatica insufficiente.'
c	    write(mess_descr(2,i),200)
c     *'Viene imposto un nuovo set di pressione'
            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.8) then
	    write(mess_descr(1,i),200)
     *'La stazione non riesce a lavorare tutto il gas.'
!	    write(mess_descr(3,i),200)
!     *'e non vi sono piu'' compressori disponibili.'
!	    write(mess_descr(4,i),200)
!     *'Viene variato il set di portata.'
!            num_rig_mess(i) = 4
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.9) then
	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore: '
     *'La pressione di mandata della centrale e'' maggiore '
	    write(mess_descr(2,i),200)
     *'della pressione massima definita in topologia.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.10) then
	    write(mess_descr(1,i),200)
cgpe-mess
     *'Turbogruppo vincolato al minimo:'
	    write(mess_descr(2,i),200)
cgpe-mess-end
     *'compressore sulla curva anti surge.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.11) then
	    write(mess_descr(1,i),200)
cgpe-mess
     *'Turbogruppo vincolato al minimo:'
	    write(mess_descr(2,i),200)
cgpe-mess-end
     *'compressore al minimo revolution number.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.12) then
	    write(mess_descr(1,i),200)
cgpe-mess
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
cgpe-mess-end
     *'compressore al massimo revolution number.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.13) then
	    write(mess_descr(1,i),200)
cgpe-mess
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
cgpe-mess-end
     *'compressore sulla curva di choking.'
            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.14) then ! NU => 15
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
cc     *'Simulazione terminata con errore: '
cc	    write(mess_descr(2,i),200) 
c     *'Turbina alla massima potenza.'
c            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.15) then
	    write(mess_descr(1,i),200)
     *'Turbina alla massima potenza.'
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.16) then ! NON VIENE SCRITTO
	    write(mess_descr(1,i),200)
     *'La potenza richiesta e'' maggiore della massima fornibile'
	    write(mess_descr(2,i),200)
     *'dalla turbina. Viene imposto un nuovo valore di set.'
            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.17) then
	    write(mess_descr(1,i),200)
     *'La stazione non riesce a lavorare tutto il gas.'
!	    write(mess_descr(2,i),200)
!     *'Si prova ad accendere un compressore.'
!            num_rig_mess(i) = 3
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.18) then
	    write(mess_descr(1,i),200)
     *'Al minimo numero di giri viene violata la massima potenza.'
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.19) then  ! NU RETE
	    write(mess_descr(1,i),200)
     *'I valori di pressione alla stazione non sono significativi.'
	    write(mess_descr(2,i),200)
     *'La stazione viene spenta.'
            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  elseif(mess_type(i,vert_ind).eq.20) then
	    write(mess_descr(1,i),200)
     *'Portata richiesta inferiore alla minima'
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit UG ******************
	  else if(mess_type(i,vert_ind).eq.21) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al minimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla curva anti surge.'
            num_rig_mess(i) = 2
*********************************************************

c****************** messaggio x crit UG ******************
********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.22) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al minimo:'
	    write(mess_descr(2,i),200)
     *'compressore al minimo numero di giri.'
            num_rig_mess(i) = 2
*********************************************************

c****************** messaggio x crit UG ******************
********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.23) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
     *'compressore al massimo numero di giri.'
            num_rig_mess(i) = 2
*********************************************************

c****************** messaggio x crit UG ******************
********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.24) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla curva di choking.'
            num_rig_mess(i) = 2
*********************************************************

c********* messaggio in comune a tutti i criteri *********
********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.25) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla curva di massima potenza.'
            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  else if(mess_type(i,vert_ind).eq.25) then ! coincide con il 9 ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore '
c	    write(mess_descr(2,i),200)
c     *'La pressione richiesta in mandata dalla stazione '
c	    write(mess_descr(3,i),200)
c     *'e'' maggiore della pressione massima definita in topologia.'
c            num_rig_mess(i) = 3
*********************************************************

***************** messaggio criterio KO ****************
	  else if(mess_type(i,vert_ind).eq.26) then
	    write(mess_descr(1,i),200)
     *'Punto KO - turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla curva di massima potenza.'
            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  elseif(mess_type(i,vert_ind).eq.27) then
	    write(mess_descr(1,i),200)
     *'La centrale e'' in riciclo:'
            num_rig_mess(i) = 1
*********************************************************
********* messaggio in comune a tutti i criteri 2026/01/27*********
	  elseif(mess_type(i,vert_ind).eq.33) then
	    write(mess_descr(1,i),200)
     *'La pressione di uscita di fase 2 supera quella di mandata 
     * richiesta:'
            num_rig_mess(i) = 1
*********************************************************                        
cgpe-ric
****************** messaggio x crit EQ ******************
	  else if(mess_type(i,vert_ind).eq.28) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al minimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla parabola del criterio di equidistanza.'
            num_rig_mess(i) = 2
*********************************************************

c****************** messaggio x crit EQ ******************
	  else if(mess_type(i,vert_ind).eq.29) then
	    write(mess_descr(1,i),200)
     *'Turbogruppo vincolato al massimo:'
	    write(mess_descr(2,i),200)
     *'compressore sulla parabola del criterio di equidistanza.'
            num_rig_mess(i) = 2
*********************************************************

c****************** messaggio x crit EQ ******************
c	  else if(mess_type(i,vert_ind).eq.30) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Tutti i turbogruppi sono nella zona tra il riciclo'
c	    write(mess_descr(2,i),200)
c     *'e le parabole minime del criterio EQ.'
c            num_rig_mess(i) = 2
c*********************************************************

c****************** messaggio x crit EQ ******************
c	  else if(mess_type(i,vert_ind).eq.31) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Tutti i turbogruppi sono nella zona tra il massimo'
c	    write(mess_descr(2,i),200)
c     *'e le parabole massime del criterio EQ.'
c            num_rig_mess(i) = 2
*********************************************************
cgpe-ric-end

********* messaggio in comune a tutti i criteri *********
c	  elseif(mess_type(i,vert_ind).eq.28) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore:'
c            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  elseif(mess_type(i,vert_ind).eq.29) then
c	    write(mess_descr(1,i),200)
c     *'Ottimizzazione terminata con errore:'
c	    write(mess_descr(2,i),200)
c     *'non e'' stata trovata una configurazione ottima'
c            num_rig_mess(i) = 2
*********************************************************

****************** messaggio x crit UG ******************
c	  else if(mess_type(i,vert_ind).eq.30) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Modificato il numero di compressori attivi'
c            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  ELSE if(mess_type(i,vert_ind).eq.31) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore '
c	    write(mess_descr(2,i),200)
c     *'L''altezza adiabatica e'' eccessiva'
c	    write(mess_descr(3,i),200)
c     *'e il set imposto e'' in portata.'
c            num_rig_mess(i) = 3
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  ELSE if(mess_type(i,vert_ind).eq.32) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore '
c	    write(mess_descr(2,i),200)
c     *'L''altezza adiabatica e'' insufficiente'
c	    write(mess_descr(3,i),200)
c     *'e il set imposto e'' in portata.'
c            num_rig_mess(i) = 3
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  else if(mess_type(i,vert_ind).eq.33) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore '
c	    write(mess_descr(2,i),200)
c     *'Raggiunto il numero massimo di iterazioni richiesto.'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.34) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Al minimo numero di giri viene violata la massima potenza.'
c            num_rig_mess(i) = 1
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.35) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Situazione inconsistente nella calc_qmax:'
c	    write(mess_descr(2,i),200)
c     *'deve esistere un punto in cui si supera max_power.'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.36) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'La pressione richiesta non e'' raggiungibile dalla centrale.'
c            num_rig_mess(i) = 1
*********************************************************

*************** messaggio x crit UP ed EQ ***************
	  else if(mess_type(i,vert_ind).eq.37) then
c            write(mess_descr(1,i),100) num_iter
	    write(mess_descr(1,i),200)
     *'Sulla choking non si raggiunge pout:'
	    write(mess_descr(2,i),200)
     *'al max_rev si resta sempre al di sopra'
            num_rig_mess(i) = 2
*********************************************************

* messaggio con indice di errore modificato x UP ed EQ **
* con una valenza del messaggio leggermente diversa
        elseif (mess_type(i,vert_ind).eq.38) then ! da 3 a 38 RETE
c            write(mess_descr(1,i),100) num_iter
	    write(mess_descr(1,i),200) 
     *'Delta pressione asp. o man. non corretto che'
	    write(mess_descr(2,i),200) 
     *'rende non valide le condizioni in pressione.'
            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  elseif(mess_type(i,vert_ind).eq.39) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore:'
c	    write(mess_descr(2,i),200)
c     *'richiesta compressione con tutte le unita'' spente'
c            num_rig_mess(i) = 2
*********************************************************

** messaggio con indice di errore modificato x crit UG **
	  else if(mess_type(i,vert_ind).eq.40) then ! da 61 a 40
	    write(mess_descr(1,i),200)
c     *'SIMULAZIONE TERMINATA CON ERRORE '
c	    write(mess_descr(2,i),200)
     *'La stazione non riesce a lavorare rispettando'
	    write(mess_descr(2,i),200)
     *'il rapporto dei giri fra i compressori.'
            num_rig_mess(i) = 2
*********************************************************

******* messaggio con indice di errore modificato *******
	  else if(mess_type(i,vert_ind).eq.41) then ! da 51 a 41 x crit UG
                                                  ! da 10 a 41 x UP ed EQ
	    write(mess_descr(1,i),200)
     *'Viene variata la pressione di aspirazione.'
            num_rig_mess(i) = 1
*********************************************************

******* messaggio con indice di errore modificato *******
	  else if(mess_type(i,vert_ind).eq.42) then ! da 52 a 42 x crit UG ! NU
                                                  ! da 11 a 42 x UP ed EQ
	    write(mess_descr(1,i),200)
     *'Viene variata la pressione di mandata.'
            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
          elseif (mess_type(i,vert_ind).eq.43) then ! NU
c            write(mess_descr(1,i),100) num_iter
	    write(mess_descr(1,i),200) 
     *'Richiesta una pressione negativa.'
            write(mess_descr(2,i),200)
     *'La stazione viene spenta.'
            num_rig_mess(i) = 2
*********************************************************

** messaggio con indice di errore modificato x crit UG **
	  else if(mess_type(i,vert_ind).eq.46) then ! da 56 a 46
	    write(mess_descr(1,i),200)
     *'Turbina alla massima potenza.'
            num_rig_mess(i) = 1
*********************************************************

** messaggio con indice di errore modificato x crit UG **
	  else if(mess_type(i,vert_ind).eq.47) then ! da 57 a 47 ! assimilato al 46
	    write(mess_descr(1,i),200)
     *'Turbina alla massima potenza.'
!    write(mess_descr(2,i),200)
!     *'la stazione non riesce a lavorare tutto il gas.'
!	    write(mess_descr(4,i),200)
!     *'Si prova ad accendere un compressore.'
!            num_rig_mess(i) = 4
            num_rig_mess(i) = 1
*********************************************************

** messaggio con indice di errore modificato x crit UG **
	  else if(mess_type(i,vert_ind).eq.48) then ! da 58 a 48
	    write(mess_descr(1,i),200)
     *'Il secondo compressore della serie e'' inutile:'
	    write(mess_descr(2,i),200)
     *'l''altezza adiabatica e'' uguale a zero.'
            num_rig_mess(i) = 2
*********************************************************

** messaggio con indice di errore modificato x crit UG **
c	  else if(mess_type(i,vert_ind).eq.49) then ! da 59 a 49 ! NU
c	    write(mess_descr(1,i),200)
c     *'La stazione e'' in riciclo.'
c            num_rig_mess(i) = 1
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.50) then ! da 9 a 50 ! NU
c	    write(mess_descr(1,i),200)
c     *'Tutte le unita'' sono in riciclo'
c            num_rig_mess(i) = 1
*********************************************************

******************* messaggio x paral *******************
c	  else if(mess_type(i,vert_ind).eq.51) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Errore nella routine calc_hpow: situazione non prevista.'
c            num_rig_mess(i) = 1
*********************************************************

* messaggio con indice di errore modificato x UP ed EQ **
	  else if(mess_type(i,vert_ind).eq.52) then ! da 12 a 52
c            write(mess_descr(1,i),100) num_iter
	    write(mess_descr(1,i),200)
cgpe     *'Simulazione terminata con errore '
cgpe	    write(mess_descr(2,i),200)
!     *'Non esistono punti fattibili'
     *'Compressione richiesta non fattibile.'
            num_rig_mess(i) = 1
*********************************************************

****************** messaggio x crit EQ ******************
c	  else if(mess_type(i,vert_ind).eq.53) then ! da 6 a 53 ! NU
c	    write(mess_descr(1,i),200)
c     *'Situazione anomala: Vminimo > Vmassimo.'
c            num_rig_mess(i) = 1
*********************************************************

*** messaggio con indice di errore modificato x serie ***
c	  else if(mess_type(i,vert_ind).eq.54) then ! da 7 a 54 ! NU
c	    write(mess_descr(1,i),200)
c     *'Situazione anomala: flag diverso da 0.'
c            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.55) then
	    write(mess_descr(1,i),200)
     *'Non viene rappresentato il punto ko.'
            num_rig_mess(i) = 1
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.61) then ! da 61 a 37 già esistente per crit UG
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Simulazione terminata con errore '
c	    write(mess_descr(2,i),200)
c     *'Al min_rev viene superata la massima potenza'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.62) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200) 
c     *'Compressione richiesta non fattibile.'
c            num_rig_mess(i) = 1
*********************************************************

************* messaggio x cen serie tipo CB *************
	  else if(mess_type(i,vert_ind).eq.63) then ! da 11 a 63 ! NON VIENE SCRITTO
	    write(mess_descr(1,i),200)
     *'Non si rispetta il criterio di UC tra le due fasi.'
	    write(mess_descr(2,i),200)
     *'Viene comunque calcolato un punto fattibile'
	    write(mess_descr(3,i),200)
     *'su entrambe le fasi'
          num_rig_mess(i) = 3
*********************************************************

************* messaggio x cen serie tipo CB *************
	  else if(mess_type(i,vert_ind).eq.64) then ! da 11 a 64 ! NON VIENE SCRITTO
	    write(mess_descr(1,i),200)
     *'Non si rispetta il criterio di UC tra le due fasi:'
	    write(mess_descr(2,i),200)
     *'non esistono punti fattibili comunque si faccia'
	    write(mess_descr(3,i),200)
     *'variare la pressione intermedia fra le due fasi.'
          num_rig_mess(i) = 3
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.65) then
	    write(mess_descr(1,i),200)
cgpe-mess     *'Ottimizzazione terminata con errore:'
cgpe-mess	    write(mess_descr(2,i),200)
cgpe-mess     *'modificando il valore di set e'' possibile'
cgpe-mess	    write(mess_descr(3,i),200)
cgpe-mess     *'trovare una configurazione ottima per la centrale.'
     *'Valore proposto della varibile di set:'
            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
      else if(mess_type(i,vert_ind).eq.66) then
	    write(mess_descr(1,i),200)
     *'Non e'' stata trovata una soluzione ottima per entrambi'
	    write(mess_descr(2,i),200)
     *'gli assetti attivi, ma solamente per un assetto.'
	    write(mess_descr(3,i),200)
     *'La soluzione viene calcolata indipendentemente'
	    write(mess_descr(4,i),200)
     *'dalle condizioni dell''altro assetto attivo.'
            num_rig_mess(i) = 4
*********************************************************

* messaggio con indice di errore modificato x UP ed EQ **
	  else if(mess_type(i,vert_ind).eq.70) then ! da 41 a 70 ! NON VIENE SCRITTO
c            write(mess_descr(1,i),100) num_iter
            write(mess_descr(1,i),200)
     *'Errore: vmin assoluto maggiore di vmax assoluto.'
            num_rig_mess(i) = 1
*********************************************************

*** messaggio con indice di errore modificato x paral ***
c	  else if(mess_type(i,vert_ind).eq.71) then ! da 21 a 71 ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Pressione richiesta superiore alla massima fattibile'
c!	    write(mess_descr(3,i),200)
c!     *'Viene corretto il punto di lavoro'
c!            num_rig_mess(i) = 3
c            num_rig_mess(i) = 1
*********************************************************

*** messaggio con indice di errore modificato x paral ***
c	  else if(mess_type(i,vert_ind).eq.72) then ! da 22 a 72 ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Pressione richiesta inferiore alla mimima fattibile'
c            num_rig_mess(i) = 1
*********************************************************

*** messaggio con indice di errore modificato x paral ***
c	  else if(mess_type(i,vert_ind).eq.73) then ! da 23 a 73 ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'La pressione richiesta viene sempre superata.'
c!	    write(mess_descr(3,i),200)
c!     *'Viene corretto il punto di lavoro'
c!            num_rig_mess(i) = 3
c            num_rig_mess(i) = 1
*********************************************************

*** messaggio con indice di errore modificato x paral ***
c	  else if(mess_type(i,vert_ind).eq.74) then ! da 24 a 74  ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Non esistono punti fattibili sulle due fasi.'
c            num_rig_mess(i) = 1
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.79) then
c superamento della temperatura massima di mandata : tout>max_tout(=stat_vars(7))
	    write(mess_descr(1,i),200)
     *'Superata la massima temperatura di mandata '
            num_rig_mess(i) = 1
c-intervento_temp
          fl_vio_temp = .true.
c-intervento_temp-end
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.80) then
c superamento della massima temperatura intermedia: temp_int>type_maxtint
	    write(mess_descr(1,i),200)
     *'Superata la massima temperatura intermedia '
            num_rig_mess(i) = 1
c-intervento_temp
          fl_vio_temp = .true.
c-intervento_temp-end
*********************************************************

********* messaggio in comune a tutti i criteri *********
c	  else if(mess_type(i,vert_ind).eq.81) then ! NU
c	    write(mess_descr(1,i),200)
c     *'Violata la minima potenza della turbina '
c            num_rig_mess(i) = 1
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.91) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Warning: Non si riesce a rispettare il criterio:'
c	    write(mess_descr(2,i),200)
c     *'la portata richiesta e'' inferiore alla minima.'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.92) then ! NU
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Warning: Non si riesce a rispettare il criterio:'
c	    write(mess_descr(2,i),200)
c     *'la portata richiesta e'' superiore alla massima.'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.93) then ! NU - sostituito da 21/22
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Warning: Non si riesce a rispettare il criterio.'
c	    write(mess_descr(2,i),200)
c     *'Turbogruppo vincolato al minimo.'
c            num_rig_mess(i) = 2
*********************************************************

*************** messaggio x crit UP ed EQ ***************
c	  else if(mess_type(i,vert_ind).eq.94) then ! NU - sostituito da 23/24
cc            write(mess_descr(1,i),100) num_iter
c	    write(mess_descr(1,i),200)
c     *'Warning: Non si riesce a rispettare il criterio.'
c	    write(mess_descr(2,i),200)
c     *'Turbogruppo vincolato al massimo.'
c            num_rig_mess(i) = 2
*********************************************************

********* messaggio in comune a tutti i criteri *********
	  else if(mess_type(i,vert_ind).eq.99) then
	    write(mess_descr(1,i),200) 
     *'Non c''e'' spazio per altri messaggi !!!'
            num_rig_mess(i) = 1
*********************************************************

	  end if
!!!          if (mess_stri(i).ne.' ') write(funit,100) mess_stri(i)
          if (mess_stri(i,vert_ind).ne.' ') then
            num_rig_mess(i) = num_rig_mess(i) + 1
            write(mess_descr(num_rig_mess(i),i),200)
     *            mess_stri(i,vert_ind)
          end if

 100	format('ITER =',i2)
 200    format(a)
	end do
	return
c------------------------------------------------------------
	end

c***********************************************************************************
      subroutine verstel_nw_OKKK_orig_()
c***********************************************************************************
c  Questa subroutine viene chiamata dopo la risoluzione stazionaria della rete col
c  metodo di Newton per verificare la fattibilità della soluzione trovata negli
c  elementi speciali.Nasce dall'unione di tre routines:VERSTEL(Sire,Pseudo),OUTSTAIL
c  (Sire,Pseudo),STAMSTA(Sire).
c  Si possono distinguere 2 tipi di controllo:
c---->1:confronta lo stato finale dell'elemento con quello richiesto per esso
c       dall'utente(cioè col valore di hstatj corrispondente ad htcitj) e 
c       segnala cambiamenti.Tali controlli sono eseguiti su tutti gli
c       elementi speciali. 
c---->2:verifica che siano rispettati i vincoli fisici sugli elementi della rete
c       e segnala con un messaggio eventuali violazioni.Tali controlli sono 
c       eseguiti sugli elementi speciali e sui punti
c***********************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/top_app_ele.inc'
c-elena
   	include '../inc/staznw_par.inc'
c-elena
c-UDM
   	include '../inc/conv.inc'
c-UDM-end
      integer*4 i,j,im,iv,ind1,ind2,k,jstaz
	character*100 msg,msg_int
c     dimensioni:snomtv--->30,statai(i)--->80

      logical*2 fl_int,   !flag per stampa intestazione
     *          fl_end    !flag per stampa riga bianca alla fine dei messaggi

      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno

      logical*2 elem_bin,elem_uni
      external elem_bin,elem_uni

	integer*4 pg,pg_app,pg_ass
      logical*2 fl_sim_ok(max_ce),
     *          flag_mf   !flag se multifunzione

	real*4 qset_real
	external qset_real
c-udm 17/5/06
	real*4 pres_appo
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end

c------------------------------------------------------------------------------
      fl_end=.false.
c------------------------------------------------------------------------------
c    VALVOLE
c------------------------------------------------------------------------------
      do j=1,ne_vlv
	 do i=otipti(te_vlv(j)),etipti(te_vlv(j))
	   msg_int=trim(te_stri(te_vlv(j)))//' : '//trim(snomtv(i)) 
         call  verst_bin(i,msg_int,fl_int,fl_end)
	 end do
	end do
c------------------------------------------------------------------------------
c    CENTRALI SEMPLIFICATE
c------------------------------------------------------------------------------
	do i = otipti(e_cs),etipti(e_cs)
	   msg_int = trim(te_stri(e_cs))//' : '//trim(snomtv(i))
         call verst_bin(i,msg_int,fl_int,fl_end)
	end do
c------------------------------------------------------------------------------
c    CENTRALI
c------------------------------------------------------------------------------
	do i=otipti(e_ce),etipti(e_ce)
	   pg=pgrato(i)
         fl_sim_ok(pg)=.false.
      end do

	do i=otipti(e_ce),etipti(e_ce)
	   flag_mf=.false.
	   pg=pgrato(i)
	   pg_app=pgrato_app(i)
         if (.not.fl_sim_ok(pg)) then
	     if (assetto_cm(pg).gt.0) then
	        flag_mf=.true.
              call verstel_stat_nw(i,pg,pg_app,flag_mf)
	        jstaz=I_UND
	        call find_ce_ass(i,jstaz)
              if (jstaz.ne.I_UND)then
	           pg_ass=pgrato(jstaz)
	           fl_sim_ok(pg_ass)=.true.
                 pg_app=pgrato_app(jstaz)
c     Nella chiamata per l'assetto associato veniva passato in input istaz invece che jstaz
                 call verstel_stat_nw(jstaz,pg_ass,pg_app,flag_mf)
	        end if
	     else 
              call verstel_stat_nw(i,pg,pg_app,flag_mf)
           end if
	   end if


	end do


c------------------------------------------------------------------------------
c    ELEMENTI UNARI ENTRANTI:
c       controlla se vengono violate la pressione massima e la portata massima.
c    ELEMENTI UNARI USCENTI:
c       controlla se vengono violate la pressione minima e la portata massima.
c------------------------------------------------------------------------------
      do j=1,ne_uni
         do i=otipti(te_uni(j)),etipti(te_uni(j))
	      msg_int=trim(te_stri(te_uni(j)))//' : '//trim(snomtv(i))  
	      fl_int=.false.
	      iv=opuvto(i)
c-elena
c  confronta lo stato finale dell'unario con quello richiesto per essa dall'utente
            flertj(i) = err_ok
            if((stadatc(htcitj(i)).ne.hstatj(i)))then
	        if (stadatc(htcitj(i)).eq.sta_pv) then
		        ind1 = sta_pu
		    else
			    ind1 = stadatc(htcitj(i))
	        endif
	        if (hstatj(i).eq.sta_pv) then
		        ind2 = sta_pu
		    else
			    ind2 = hstatj(i)
	        endif
              call tipo_err (i,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='Lo stato e''cambiato '
	        call messtel(fl_int,fl_end,msg_int,msg)
		    msg='    da '// trim(statai(ind1))
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='    a ' // trim(statai(ind2)) //'.'
	        call messtel(fl_int,fl_end,msg_int,msg)
c-elena
            else
	        if (hstatj(i).eq.sta_q) then
cqpcls
	           if (abs(porttj(i)-qset_real(i)).gt.repsQ) then
                    call tipo_err (i,err_ii,msg)
	              call messtel(fl_int,fl_end,msg_int,msg)
	              msg = 'Modificata la PORTATA di set:'
          	      call messtel(fl_int,fl_end,msg_int,msg)
                    write(msg,510) udm_out_str(udm_Q,qset_real(i),0),
     *                             udm_out_str(udm_Q,porttj(i),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	           end if
	        else if (hstatj(i).eq.sta_pv) then
	           if (abs(prestj(iv)-pvsetj(i)).gt.repsP) then
                    call tipo_err (i,err_ii,msg)
	              call messtel(fl_int,fl_end,msg_int,msg)
	              msg = 'Modificata la PRESSIONE di set:'
          	      call messtel(fl_int,fl_end,msg_int,msg)
                    write(msg,560) udm_out_str(udm_P,pvsetj(i),0),
     *                             udm_out_str(udm_P,prestj(iv),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	           end if
              end if
	      end if
c-elena
c
	        if(prestj(iv).lt.(pmintx(i)-repsP)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Pressione inferiore al minimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,580) udm_out_str(udm_P,prestj(iv),0),
     *                               udm_out_str(udm_P,pmintx(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
              end if
 	        if(prestj(iv).gt.(pmaxtx(i)+repsP)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Pressione superiore al massimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                      write(msg,570) udm_out_str(udm_P,prestj(iv),0),
cmar_rev_log_sem     *                               udm_out_str(udm_P,pmaxtx(i),1)
cmar_rev_log_sem	                call messtel(fl_int,fl_end,msg_int,msg)
              end if
	        if (abs(porttj(i)).gt.(qmaxtx(i)+repsQ)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Portata superiore al massimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                        write(msg,540) udm_out_str(udm_Q,
cmar_rev_log_sem       *                                       diretj(i)*porttj(i),0),
cmar_rev_log_sem       *                               udm_out_str(udm_Q,qmaxtx(i),1)
cmar_rev_log_sem  	                call messtel(fl_int,fl_end,msg_int,msg)
	        end if
	        if (abs(porttj(i)).gt.repsQ .and.
     *            hstatj(i).eq.sta_off) then
                      call tipo_err (i,err_ff,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Portata diversa da zero.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
	        end if
		    if(diretj(i)*porttj(i).lt.-repsQ) then
                 if (set_non_ritorno(i)) then
                      call tipo_err (i,err_ff,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg = 'Violata la direzionalita'' '//
     *	                  'dell''elemento. Portata negativa.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
                 else
                      call tipo_err (i,err_ii,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg = 'Violata la direzionalita'' '//
     *	                  'dell''elemento. Portata negativa.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
                 end if
              end if
         end do 
	end do
c----------------------------------------------------------------------
c     PUNTI:controlla se viene violata la pressione minima 
c----------------------------------------------------------------------
      if (.not.fl_sim) then
      do i = otipti(e_pu), etipti(e_pu)
c-e_pu: escludo il trattamento dei punti nati dallaa scompattazione dei tronchi
c       con lunghezza superiore alla lunghezza massima di trattabilità
        if (elem_id(i) .gt. 0) then
c-e_pu-end
	   msg_int=trim(te_stri(e_pu))//' : '//trim(snomtv(i)) 
	   fl_int=.false.
c
         if (prestj(i) .gt. pmaxtx(i)) then
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione superiore al massimo consentito.'
	      call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem            write(msg,570) udm_out_str(udm_P,prestj(i),0),
cmar_rev_log_sem     *                     udm_out_str(udm_P,pmaxtx(i),1)
cmar_rev_log_sem	      call messtel(fl_int,fl_end,msg_int,msg)
	   end if
         if(prestj(i) .lt. pmin_def) then
            call tipo_err (i,err_ff,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione inferiore al minimo di sistema.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,620) udm_out_str(udm_P,prestj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   else if (prestj(i) .lt. pmintx(i)) then                        
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione inferiore al minimo consentito.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,580) udm_out_str(udm_P,prestj(i),0),
     *                     udm_out_str(udm_P,pmintx(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   endif
         if(temptj(i) .lt. tmin_def) then
            call tipo_err (i,err_ff,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Temperatura inferiore al minimo di sistema.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_T,temptj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
         elseif(temptj(i) .lt. tmin_fnz) then
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem	      msg='Temperatura inferiore al minimo consentito.'
            msg='Temperatura inferiore alla minima consentita.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_T,temptj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   endif
        endif ! e_pu

      enddo
      endif !.not.fl_sim

c-17/5/06
c-UDM
510   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('Q: ',A10,' - Q max: ',A25)
560   format('p richiesta: ',A10,' - p proposta: ',A25)
570   format('p: ',A10,' - p max: ',A25)
580   format('p: ',A10,' - p min: ',A25)
610   format('Q: ',A25)
620   format('p: ',A25)
630   format('T: ',A25)

c-UDM-end

	return 
	end   !verstel_nw
c****************************************************************************************
cmar-tmin-30-06-09

      subroutine verstel_nw()
c***********************************************************************************
c  Questa subroutine viene chiamata dopo la risoluzione stazionaria della rete col
c  metodo di Newton per verificare la fattibilità della soluzione trovata negli
c  elementi speciali.Nasce dall'unione di tre routines:VERSTEL(Sire,Pseudo),OUTSTAIL
c  (Sire,Pseudo),STAMSTA(Sire).
c  Si possono distinguere 2 tipi di controllo:
c---->1:confronta lo stato finale dell'elemento con quello richiesto per esso
c       dall'utente(cioè col valore di hstatj corrispondente ad htcitj) e 
c       segnala cambiamenti.Tali controlli sono eseguiti su tutti gli
c       elementi speciali. 
c---->2:verifica che siano rispettati i vincoli fisici sugli elementi della rete
c       e segnala con un messaggio eventuali violazioni.Tali controlli sono 
c       eseguiti sugli elementi speciali e sui punti
c***********************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/top_app_ele.inc'
c-elena
   	include '../inc/staznw_par.inc'
c-elena
c-UDM
   	include '../inc/conv.inc'
c-UDM-end

cmar_rev_log_sem
c  ???????????????????

      include '../inc/td.inc'

cmar_rev_log_sem
cmar_rev_log_sem

      include '../inc/allarmi.inc'
    

c       real*4 v_all(m_ogg)

cmar_rev_log_sem


      integer*4 i,j,im,iv,ind1,ind2,k,jstaz
	character*100 msg,msg_int
c     dimensioni:snomtv--->30,statai(i)--->80

      logical*2 fl_int,   !flag per stampa intestazione
     *          fl_end    !flag per stampa riga bianca alla fine dei messaggi

      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno

      logical*2 elem_bin,elem_uni
      external elem_bin,elem_uni

	integer*4 pg,pg_app,pg_ass
      logical*2 fl_sim_ok(max_ce),
     *          flag_mf   !flag se multifunzione

	real*4 qset_real
	external qset_real
c-udm 17/5/06
	real*4 pres_appo
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end
cmar_rev_log_sem  ++++++++++++++++++++++++++++++++++++++

       real*4 vel(m_ogg), r, am, vel_m(m_ogg),vel_v(m_ogg)
	   logical*2 lro_ok
c	real*4   v_all
c       character*(100) msg_int


cmar_rev_log_sem  ++++++++++++++++++++++++++++++++++++++


c------------------------------------------------------------------------------
      fl_end=.false.

cmar_rev_log_sem 
c      v_all=15.0
cmar_rev_log_sem 
c------------------------------------------------------------------------------




cmar_rev_log_sem  ++++++++++++++++++++++++++++++++++++++
cmar   velocità media 
c      do i= otipti(e_tr),etipti(e_tr)
       do i= otipti(e_tr),fine_tr
      vel(i)=0

	 msg_int=trim(te_stri(e_tr))//' : '//trim(snomtv(i)) 
	fl_int=.false.





ccc	r   = (rocitj(opumto(i))+rocitj(opuvto(i)))/2

      

       lro_ok=((rocitj(opumto(i)).ne.0.  .and.
     -            rocitj(opuvto(i)).ne.0.) .and.
     -           (ro0td(i).ne.0.))
c-amount: volume gas presente nel tronco
c         if(lro_ok)then
c            am = ro*(volntx(i)*2)/ro0td(i)
c         else
c            am = 0.
c         endif






ccc	vel(i) = ((diamtx(i)*diamtx(i)*r)*atan(1.)*3.6)

ccc	if((lro_ok).and.(abs(vel(i)).gt.0.))then
ccc             vel(i) = (abs(porttj(i))*ro0td(i))/vel(i)
ccc         else
ccc             vel(i) = 0.
ccc         endif


CMAR_TR_VELOCITà_MONTE

c      (m/sec)
c-densità media ro definita sopra
         vel_m(i)=((diamtx(i)*diamtx(i)*rocitj(opumto(i))
     *   )*atan(1.)*3.6)
c-velocità gas
         if((lro_ok).and.(abs(vel_m(i)).gt.0.))then
             vel_m(i) = (abs(porttj(i))*ro0td(i))/vel_m(i)
         else
             vel_m(i)= 0.
         endif
         
CMAR_TR_VELOCITà_MONTE

      vel(i)=MAX(vel(i),vel_m(i))

CMAR_TR_VELOCITà_VALLE

!25-TROUT_VELOCITA -r*4  (m/sec)
      vel_v(i)=((diamtx(i)*diamtx(i)*rocitj(opuvto(i))
     *   )*atan(1.)*3.6)
c-velocità gas
         if((lro_ok).and.(abs(vel_v(i)).gt.0.))then
             vel_v(i) = (abs(porttj(i))*ro0td(i))/vel_v(i)
         else
             vel_v(i)= 0.
         endif
CMAR_TR_VELOCITà_VALLE

      vel(i)=MAX(vel(i),vel_v(i))
c      vel(i)=MIN(vel(i),vel_v(i))

   
      if (v_all(I) .ne. R_UND) then


       IF( vel(I).GT.v_all(I) )THEN

	      call tipo_err (i,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Velocità '
            call messtel(fl_int,fl_end,msg_int,msg)
      
       ELSE
	 continue
	 ENDIF

	endif



ccc      call messtel(fl_int,fl_end,msg_int,msg)
c        write(msg,640)  udm_out_str(udm_Q,pres_max_all(i),1)
c	      call messtel(fl_int,fl_end,msg_int,msg)

c       msg_int=trim(te_stri(e_tr))//' : '//trim(snomtv(i)) 

c	 call messtel(fl_int,fl_end,msg_int,msg)
ccccccccc
c         call tipo_err (i,err_aa,msg)
c         call messtel(fl_int,fl_end,msg_int,msg)
ccccccccc

c	          msg = 'superata velocità !!!!!!!'

c	         call messtel(fl_int,fl_end,msg_int,msg)
      
c            msg = '    '

c	         call messtel(fl_int,fl_end,msg_int,msg)
      

ccc	endif

     
	enddo


cmar_rev_log_sem  ++++++++++++++++++++++++++++++++++++++
c    VALVOLE
c------------------------------------------------------------------------------
      do j=1,ne_vlv
	 do i=otipti(te_vlv(j)),etipti(te_vlv(j))
	   msg_int=trim(te_stri(te_vlv(j)))//' : '//trim(snomtv(i)) 
         call  verst_bin(i,msg_int,fl_int,fl_end)
	 end do
	end do
c------------------------------------------------------------------------------
c    CENTRALI SEMPLIFICATE
c------------------------------------------------------------------------------
	do i = otipti(e_cs),etipti(e_cs)
	   msg_int = trim(te_stri(e_cs))//' : '//trim(snomtv(i))
         call verst_bin(i,msg_int,fl_int,fl_end)
	end do
c------------------------------------------------------------------------------
c    CENTRALI
c------------------------------------------------------------------------------
	do i=otipti(e_ce),etipti(e_ce)
	   pg=pgrato(i)
         fl_sim_ok(pg)=.false.
      end do

	do i=otipti(e_ce),etipti(e_ce)
	   flag_mf=.false.
	   pg=pgrato(i)
	   pg_app=pgrato_app(i)
         if (.not.fl_sim_ok(pg)) then
	     if (assetto_cm(pg).gt.0) then
	        flag_mf=.true.
              call verstel_stat_nw(i,pg,pg_app,flag_mf)
	        jstaz=I_UND
	        call find_ce_ass(i,jstaz)
              if (jstaz.ne.I_UND)then
	           pg_ass=pgrato(jstaz)
	           fl_sim_ok(pg_ass)=.true.
                 pg_app=pgrato_app(jstaz)
c     Nella chiamata per l'assetto associato veniva passato in input istaz invece che jstaz
                 call verstel_stat_nw(jstaz,pg_ass,pg_app,flag_mf)
	        end if
	     else 
              call verstel_stat_nw(i,pg,pg_app,flag_mf)
           end if
	   end if


	end do


c------------------------------------------------------------------------------
c    ELEMENTI UNARI ENTRANTI:
c       controlla se vengono violate la pressione massima e la portata massima.
c    ELEMENTI UNARI USCENTI:
c       controlla se vengono violate la pressione minima e la portata massima.
c------------------------------------------------------------------------------
      do j=1,ne_uni
         do i=otipti(te_uni(j)),etipti(te_uni(j))
	      msg_int=trim(te_stri(te_uni(j)))//' : '//trim(snomtv(i))  
	      fl_int=.false.
	      iv=opuvto(i)
c-elena
c  confronta lo stato finale dell'unario con quello richiesto per essa dall'utente
            flertj(i) = err_ok
            if((stadatc(htcitj(i)).ne.hstatj(i)))then
	        if (stadatc(htcitj(i)).eq.sta_pv) then
		        ind1 = sta_pu
		    else
			    ind1 = stadatc(htcitj(i))
	        endif
	        if (hstatj(i).eq.sta_pv) then
		        ind2 = sta_pu
		    else
			    ind2 = hstatj(i)
	        endif
              call tipo_err (i,err_ii,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='Lo stato e''cambiato '
	        call messtel(fl_int,fl_end,msg_int,msg)
		    msg='    da '// trim(statai(ind1))
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='    a ' // trim(statai(ind2)) //'.'
	        call messtel(fl_int,fl_end,msg_int,msg)
c-elena
            else
	        if (hstatj(i).eq.sta_q) then
cqpcls
	           if (abs(porttj(i)-qset_real(i)).gt.repsQ) then
                    call tipo_err (i,err_ii,msg)
	              call messtel(fl_int,fl_end,msg_int,msg)
	              msg = 'Modificata la PORTATA di set:'
          	      call messtel(fl_int,fl_end,msg_int,msg)
                    write(msg,510) udm_out_str(udm_Q,qset_real(i),0),
     *                             udm_out_str(udm_Q,porttj(i),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	           end if
	        else if (hstatj(i).eq.sta_pv) then
	           if (abs(prestj(iv)-pvsetj(i)).gt.repsP) then
                    call tipo_err (i,err_ii,msg)
	              call messtel(fl_int,fl_end,msg_int,msg)
	              msg = 'Modificata la PRESSIONE di set:'
          	      call messtel(fl_int,fl_end,msg_int,msg)
                    write(msg,560) udm_out_str(udm_P,pvsetj(i),0),
     *                             udm_out_str(udm_P,prestj(iv),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	           end if
              end if
	      end if
c-elena
c
	        if(prestj(iv).lt.(pmintx(i)-repsP)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Pressione inferiore al minimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,580) udm_out_str(udm_P,prestj(iv),0),
     *                               udm_out_str(udm_P,pmintx(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
              end if
 	        if(prestj(iv).gt.(pmaxtx(i)+repsP)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Pressione superiore al massimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                      write(msg,570) udm_out_str(udm_P,prestj(iv),0),
cmar_rev_log_sem     *                               udm_out_str(udm_P,pmaxtx(i),1)
cmar_rev_log_sem	                call messtel(fl_int,fl_end,msg_int,msg)
              end if
	        if (abs(porttj(i)).gt.(qmaxtx(i)+repsQ)) then
                      call tipo_err (i,err_ww,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Portata superiore al massimo consentito.'
	                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                        write(msg,540) udm_out_str(udm_Q,
cmar_rev_log_sem       *                                       diretj(i)*porttj(i),0),
cmar_rev_log_sem       *                               udm_out_str(udm_Q,qmaxtx(i),1)
cmar_rev_log_sem  	                call messtel(fl_int,fl_end,msg_int,msg)
	        end if


C  ALLARMI  unari


      if (port_max_all(I) .ne. R_UND) then

         IF( PORTTJ(I).GT.port_max_all(I) )THEN

	      call tipo_err (i,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata massima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,641)  udm_out_str(udm_Q,port_max_all(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
      endif 
	   ENDIF

      if (port_min_all(I) .ne. R_UND) then

	     IF( PORTTJ(I).LT.port_min_all(I) )THEN

	      call tipo_err (i,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata minima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,651)  udm_out_str(udm_Q,port_min_all(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      	ENDIF
      endif


CMAR    ALLARMI



	        if (abs(porttj(i)).gt.repsQ .and.
     *            hstatj(i).eq.sta_off) then
                      call tipo_err (i,err_ff,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg='Portata diversa da zero.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
	        end if
		    if(diretj(i)*porttj(i).lt.-repsQ) then
                 if (set_non_ritorno(i)) then
                      call tipo_err (i,err_ff,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg = 'Violata la direzionalita'' '//
     *	                  'dell''elemento. Portata negativa.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
                 else
                      call tipo_err (i,err_ii,msg)
	                call messtel(fl_int,fl_end,msg_int,msg)
	                msg = 'Violata la direzionalita'' '//
     *	                  'dell''elemento. Portata negativa.'
	                call messtel(fl_int,fl_end,msg_int,msg)
                      write(msg,610) udm_out_str(udm_Q,
     -                                  diretj(i)*porttj(i),1)
	                call messtel(fl_int,fl_end,msg_int,msg)
                 end if







              end if
         end do 
	end do
c----------------------------------------------------------------------
c     PUNTI:controlla se viene violata la pressione minima 
c----------------------------------------------------------------------
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
cccccccccccccccccccccccccccccccccccccccccccccc       call  P_alarm_conn()
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr

      if (.not.fl_sim) then
c      do i = otipti(e_pu), etipti(e_pu)
cmar_rev_log_sem 
       do i = otipti(e_pu), fine_pti

ccccccccccccccccccccccccccccccccccc	call  P_alarm_conn(i,pmaxtx(i))
c-e_pu: escludo il trattamento dei punti nati dallaa scompattazione dei tronchi
c       con lunghezza superiore alla lunghezza massima di trattabilità
        if (elem_id(i) .gt. 0) then
c-e_pu-end
	   msg_int=trim(te_stri(e_pu))//' : '//trim(snomtv(i)) 
	   fl_int=.false.
c
         if (prestj(i) .gt. pmaxtx(i)) then
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione superiore al massimo consentito.'
	      call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem            write(msg,570) udm_out_str(udm_P,prestj(i),0),
cmar_rev_log_sem     *                     udm_out_str(udm_P,pmaxtx(i),1)
cmar_rev_log_sem	      call messtel(fl_int,fl_end,msg_int,msg)
	   end if
         if(prestj(i) .lt. pmin_def) then
            call tipo_err (i,err_ff,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione inferiore al minimo di sistema.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,620) udm_out_str(udm_P,prestj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   else if (prestj(i) .lt. pmintx(i)) then                        
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Pressione inferiore al minimo consentito.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,580) udm_out_str(udm_P,prestj(i),0),
     *                     udm_out_str(udm_P,pmintx(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   endif
         if(temptj(i) .lt. tmin_def) then
            call tipo_err (i,err_ff,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      msg='Temperatura inferiore al minimo di sistema.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_T,temptj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
         elseif(temptj(i) .lt. tmin_fnz) then
            call tipo_err (i,err_ww,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem	      msg='Temperatura inferiore al minimo consentito.'
            msg='Temperatura inferiore alla minima consentita.'
	      call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,630) udm_out_str(udm_T,temptj(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
	   endif
        endif ! e_pu


C  ALLARMI
      if (pres_max_all(I) .ne. R_UND) then

         IF( PRESTJ(I).GT.pres_max_all(I) )THEN

	      call tipo_err (i,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Pressione massima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,640)  udm_out_str(udm_P,pres_max_all(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)

         ENDIF
	endif


       if (pres_min_all(I) .ne. R_UND) then
	     IF( PRESTJ(I).LT.pres_min_all(I) )THEN

	      call tipo_err (i,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Pressione minima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,650)  udm_out_str(udm_P,pres_min_all(i),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      	ENDIF
      endif

CMAR    ALLARMI

      enddo
      endif !.not.fl_sim

c	do i = otipti(e_tr), etipti(e_tr)
c	 msg_int=trim(te_stri(e_tr))//' : '//trim(snomtv(i)) 
c	fl_int=.false.


c       IF( vel(I).GT.v_all(I) )THEN

c	      call tipo_err (i,err_aa,msg)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c      msg='Superata la soglia di allarme '
c            call messtel(fl_int,fl_end,msg_int,msg)
      
c       else
c	 continue
c	 endif
c	       

c	enddo


c-17/5/06
c-UDM
510   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('Q: ',A10,' - Q max: ',A25)
560   format('p richiesta: ',A10,' - p proposta: ',A25)
570   format('p: ',A10,' - p max: ',A25)
580   format('p: ',A10,' - p min: ',A25)
610   format('Q: ',A25)
620   format('p: ',A25)
630   format('T: ',A25)
cmar_log_sem
640   format('P max: ',A25)
650   format('P min: ',A25)


cmar_log_sem
641   format('Q max: ',A25)
651   format('Q min: ',A25)


c-UDM-end


      close(102)
	return 
	end   !verstel_nw
c****************************************************************************************
cmar-tmin-30-06-09
      subroutine  verst_bin_okk_orig(i_ele,msg_int,fl_int,fl_end)
c************************************************************************************
c  Verifica la fattibilità di una condizione di esercizio di elemento binario
c  descritta dalla terna  (Pm,Pv,Q,T); 
c  solo per gli impianti di regolazione
c    verifica anche che la velocità gas non superi la velocità limite nel piping
c  .
c************************************************************************************
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/err.inc' !parametri costanti
	include '../inc/th.inc' !stati di regolazione
	include '../inc/ti.inc' !identificatori elementi
	include '../inc/tx.inc' !dati topologici elementi (qmaxtx)
	include '../inc/tj.inc' !cin (pressioni,portate...)
	include '../inc/tz.inc' 
   	include '../inc/ai.inc'
	include '../inc/jc.inc' !variabili per calcolo jacobiano nello stazionario
	include '../inc/top_app_ele.inc' 
   	include '../inc/staznw_par.inc'
   	include '../inc/conv.inc'
   	include '../inc/flag.inc'
   	include '../inc/stations.inc'

c
      integer*4 i_ele    !indice dell'elemento nella struttura dati completa
      character*(*)msg_int
	logical*2 fl_int,fl_end

      integer*4 im,iv, ipg, ind1,ind2
      real*4    qmax,pvmax,dp_on
      character*150 msg
	logical*2 flag_vlim

      real*4    qpwmce
	external  qpwmce

	logical*2 centrale

	logical*2 set_non_ritorno
	external set_non_ritorno

	logical*2 l_ok

      logical*2 ltest_qmax

	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external               udm_out_str

      character(80) format_p

	real*4 qset_real
	external qset_real
c-------------------------------------------------------------------------------------
      fl_int = .false.
      fl_end = .false.
      im = opumto(i_ele)
      iv = opuvto(i_ele)
      ipg = pgrato(i_ele)

	centrale = (htipto(i_ele).eq.e_ce.or.htipto(i_ele).eq.e_cs)

c inizializzazione
      flertj(i_ele) = err_ok
      l_ok = .true.

      if (centrale.or.tipoScenario.eq.TIPOSCEN_RETE_din) then
	  dp_on = dp_on_ce
	else
	  dp_on = dp_on_vl
	endif
      if (centrale) then
	  format_p = "('p aspirazione: ',A10,' - p mandata: ',A25)"
	else
	  format_p = "('p monte: ',A10,' - p valle: ',A25)"
	endif

   	if(htipto(i_ele) .eq. e_vg) then
         call qmaxvg(i_ele,qmaxtx(i_ele),flag_vlim)
	else
	   flag_vlim = .false.
      end if

c  confronta lo stato finale dell'elemento con quello richiesto per essa dall'utente
      if((stadatc(htcitj(i_ele)).ne.hstatj(i_ele)))then
C------->SET NON RISPETTATO

	  ind1 = stadatc(htcitj(i_ele))
        if (centrale) then
	    if (ind1.eq.sta_pv) then
		  ind1 = sta_pman
		elseif (ind1.eq.sta_pm) then
		  ind1 = sta_pasp
		elseif (ind1.eq.sta_pq) then
		  ind1 = sta_pamq
	    endif
        endif
	  ind2 = hstatj(i_ele)
        if (centrale) then
	    if (ind2.eq.sta_pv) then
		  ind2 = sta_pman
		elseif (ind2.eq.sta_pm) then
		  ind2 = sta_pasp
		elseif (ind2.eq.sta_pq) then
		  ind2 = sta_pamq
	    endif
        endif

        call tipo_err (i_ele,err_ii,msg)
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='Lo stato e''cambiato '
	  call messtel(fl_int,fl_end,msg_int,msg)
        msg='   da '// trim(statai(ind1)) 
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='   a ' // trim(statai(ind2)) //'.'
	  call messtel(fl_int,fl_end,msg_int,msg)

      else
       if (.not.fl_sim) then
        if (hstatj(i_ele).eq.sta_q) then
          if (abs(porttj(i_ele)-qset_real(i_ele)).gt.repsQ) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Modificata la PORTATA di set:'
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0),
     *                     udm_out_str(udm_Q,porttj(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_q
            endif
cstaz-corr-end
           end if
        else if (hstatj(i_ele).eq.sta_pm) then
          if (abs(prestj(im)-pmsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE ASPIRAZIONE di set:'
	      else
               msg = 'Modificata la PRESSIONE MONTE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(im),1)
            call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_pm
            endif

          end if
        else if (hstatj(i_ele).eq.sta_pv) then
          if (abs(prestj(iv)-pvsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE MANDATA di set:'
	      else
               msg = 'Modificata la PRESSIONE VALLE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(iv),1)
            call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_pv
            endif

          end if
        end if
       end if
      end if

c  Verifica che siano rispettati i vincoli fisici
	if(htcitj(i_ele).ne.tc_off) then
c SIRE2000<>NUOVO SIRE
c--------------------------------------------------------------------------------
cc  se la portata della valvola è negativa si ha uno stato legale per i tipi di
cc  funzionamento che ammettono un flusso bidirezionale e per i quali si impongono 
cc  equazioni in pressione,purchè il valore assoluto della portata non superi la 
cc  portata massima consentita alla valvola
c  se la portata della valvola è negativa si ha uno stato legale per il
c  funzionamento TC_ON (bidirazionale) purchè il valore assoluto della
c  portata non superi la portata massima consentita alla valvola (cfr. modello
c  di centrale semplificata)
c--------------------------------------------------------------------------------
c SIRE2000<>NUOVO SIRE-end

            ltest_qmax = .true.
   	      if(htipto(i_ele).eq.e_vd) then   
c-------->NUOVO CONTROLLO	SOLO PER IMPIANTI RIDUZIONE
               if (hstatj(i_ele).ne.sta_on. and.
     *             hstatj(i_ele).ne.sta_fr .and.
     *             hstatj(i_ele).ne.sta_off .and.
     *             hstatj(i_ele).ne.sta_nr .and.
     *             (prestj(im)-prestj(iv)) .lt. dpmin(ipg))then
c                  ltest_qmax = .false.   ! il controllo sulla qmax viene fatto sempre
c dpmin deve essere in BAR ASSOLUTI
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Salto di pressione inferiore al minimo'// 
     *                  ' consentito per il corretto funzionamento'//
     *                  ' dell''impianto.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,150) udm_out_str(udm_P,
     *                           prestj(im)-prestj(iv),0),
     *                           udm_out_str(udm_P,dpmin(ipg),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
               endif

	      end if
	   
cs            if(htipto(i_ele).eq.e_cs) then   
csc----->verifica che siano rispettati i vincoli fisici
cs              pvmax = prestj(im)*romax(ipg)
cs              if (prestj(iv) .gt. pvmax +repsP) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Pressione di mandata superiore alla massima'//
cs     *                ' consentita.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Violazione massimo rapporto di compressione.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
cs     *                         udm_out_str(udm_P,pvmax,1)
cs	          call messtel(fl_int,fl_end,msg_int,msg)
csc                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
csc                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif 

cs              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Portata superiore al massimo'//
cs     *                ' consentito quando la potenza e'' massima.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cs     *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif
cs            endif 

          if (ltest_qmax) then
            if(abs(porttj(i_ele)) .gt. (qmaxtx(i_ele)+repsQ)) then
              if (htipto(i_ele) .ne. e_ce) then
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            if (flag_vlim) then
 	              msg = 'Portata superiore al massimo consentito'//
     *                     ' dalla velocita'' limite.'
	            else
                    msg = 'Portata superiore al massimo consentito.'
	            endif
	            call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                    write(msg,220) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                           udm_out_str(udm_Q,qmaxtx(i_ele),1)
cmar_rev_log_sem  	            call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem    
	            if (flag_vlim) then
cmar_rev_log_sem                      write(msg,410) udm_out_str(udm_V,vlim_out(ipg),1)
cmar_rev_log_sem  	              call messtel(fl_int,fl_end,msg_int,msg)
	            endif
              endif
            endif
          endif

c--------->NUOVO CONTROLLO
            if (.not.centrale .and.
     -          touttz(i_ele) .lt. tsettx(i_ele)) then
               call tipo_err (i_ele,err_ww,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
	         msg = 'Temperatura inferiore alla minima consentita.'
	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,310) udm_out_str(udm_T,touttz(i_ele),0),
     *                        udm_out_str(udm_T,tsettx(i_ele),1)
	         call messtel(fl_int,fl_end,msg_int,msg)

	      end if

            if (htcitj(i_ele) .eq. tc_pq) then
c------------------------------------------------------------------------------
c regolazione TC_PQ: la valvola cerca di mantenere il valore della pressione di 
c valle uguale a PVset, purchè il valore della pressione di monte non scenda al
c di sotto di PMset e la portata non superi Qset
c------------------------------------------------------------------------------
                if (prestj(im) .lt. (pmsetj(i_ele)-repsP) .or.
     *              prestj(iv) .gt. (pvsetj(i_ele)+repsP) .or.
     *              porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                  call tipo_err (i_ele,err_ww,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Regolazione in pressione e portata non '//
     *                  'rispettata.'
	            call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr                  if(prestj(im) .lt. pmsetj(i_ele)) then
                  if(prestj(im) .lt. (pmsetj(i_ele)-repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(im),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
                  if(prestj(iv) .gt. (pvsetj(i_ele)+repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(iv),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
	            if (porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                    write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0)
     *                            ,udm_out_str(udm_Q,porttj(i_ele),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	            end if

                end if
            endif


		  if(diretj(i_ele)*porttj(i_ele).lt.-repsQ .and.
     -         set_non_ritorno(i_ele)) then
               call tipo_err (i_ele,err_ff,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
	         msg = 'Violata la direzionalita'' dell''elemento. '//
     *	           'Portata negativa:'
	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
               call messtel(fl_int,fl_end,msg_int,msg)
               l_ok = .false.
	      end if

	      if (abs(prestj(im)-prestj(iv)) .gt. dp_on) then
                if (abs(porttj(i_ele)).gt.repsQ .and.
     -              (hstatj(i_ele) .eq. sta_on .or.
     -               hstatj(i_ele) .eq. sta_fr)) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Delta pressione in stato di libero '//
     *                  ' flusso.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                           udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).lt.-repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).lt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).gt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Portata negativa '
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).gt.repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).gt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).lt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

                endif
            end if

cs-controlli spostati dopo il check sulle p e q
   	      if(htipto(i_ele).eq.e_cs .and. l_ok) then   
c----->verifica che siano rispettati i vincoli fisici
              pvmax = prestj(im)*romax(ipg)
              if (prestj(iv) .gt. pvmax +repsP) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Pressione di mandata superiore alla massima'//
     *                ' consentita.'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Violazione massimo rapporto di compressione.'
                call messtel(fl_int,fl_end,msg_int,msg)
                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
     *                         udm_out_str(udm_P,pvmax,1)
	          call messtel(fl_int,fl_end,msg_int,msg)
c                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
c                call messtel(fl_int,fl_end,msg_int,msg)
              endif 

              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Portata superiore al massimo'//
     *                ' consentito quando la potenza e'' massima.'
                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cmar_rev_log_sem                  call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cmar_rev_log_sem                 call messtel(fl_int,fl_end,msg_int,msg)
              endif
            endif
cs-end

      end if


110   format('p richiesta: ',A10,' - p proposta: ',A25)
!140   format('p monte: ',A10,' - p valle: ',A25)
!141   format('p aspirazione: ',A10,' - p mandata: ',A25)
150	format('delta p: ',A10,' - delta p min: ',A25)
210   format('Q richiesta: ',A10,' - Q proposta: ',A25)
220   format('Q: ',A10,' - Q max: ',A25)
230   format('Q: ',A25)
310	format('T: ',A10,' - T min: ',A25)
410   format('V lim: ',A25)
510   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('Q: ',A10,' - Q max: ',A25)
550   format('p man: ',A10,' - p man max: ',A25)
610   format('Q: ',A25)
630   format('P max: ',A25)

      return
	end
c--------------------------------------------------------------------------------------
      subroutine  verst_bin(i_ele,msg_int,fl_int,fl_end)
c************************************************************************************
c  Verifica la fattibilità di una condizione di esercizio di elemento binario
c  descritta dalla terna  (Pm,Pv,Q,T); 
c  solo per gli impianti di regolazione
c    verifica anche che la velocità gas non superi la velocità limite nel piping
c  .
c************************************************************************************
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/err.inc' !parametri costanti
	include '../inc/th.inc' !stati di regolazione
	include '../inc/ti.inc' !identificatori elementi
	include '../inc/tx.inc' !dati topologici elementi (qmaxtx)
	include '../inc/tj.inc' !cin (pressioni,portate...)
	include '../inc/tz.inc' 
   	include '../inc/ai.inc'
	include '../inc/jc.inc' !variabili per calcolo jacobiano nello stazionario
	include '../inc/top_app_ele.inc' 
   	include '../inc/staznw_par.inc'
   	include '../inc/conv.inc'
   	include '../inc/flag.inc'
   	include '../inc/stations.inc'

cmar_rev_log_sem

      include '../inc/allarmi.inc'
   
cmar_rev_log_sem

c
      integer*4 i_ele    !indice dell'elemento nella struttura dati completa
      character*(*)msg_int
	logical*2 fl_int,fl_end

      integer*4 im,iv, ipg, ind1,ind2
      real*4    qmax,pvmax,dp_on
      character*150 msg
	logical*2 flag_vlim

      real*4    qpwmce
	external  qpwmce

	logical*2 centrale

	logical*2 set_non_ritorno
	external set_non_ritorno

	logical*2 l_ok

      logical*2 ltest_qmax

	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external               udm_out_str

      character(80) format_p

	real*4 qset_real
	external qset_real

	real*4 a
c-------------------------------------------------------------------------------------

cmar-tmin


cmar-tmin

      fl_int = .false.
      fl_end = .false.
      im = opumto(i_ele)
      iv = opuvto(i_ele)
      ipg = pgrato(i_ele)

	centrale = (htipto(i_ele).eq.e_ce.or.htipto(i_ele).eq.e_cs)

c inizializzazione
      flertj(i_ele) = err_ok
      l_ok = .true.
CMF++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
CMAR   solo per la centrale ad assetti condivisi 


	IF (      fl_cmf_ass(i_ele)    )  then

c	 msg_int='CENTRALE MULTIFUNZIONE'// ' : '//trim(snomtv(istaz))//
      msg='ASSETTO:'//trim(sigla_ass(ipg))

        call messtel(fl_int,fl_end,msg_int,msg)
	
c        ALLARMI

	 if (port_max_all(I_ELE) .ne. R_UND) then

cmar         IF( PORTTJ(I_ELE).GT.port_max_all(I_ELE) )THEN
  
         IF( flow_stat(ipg).GT.port_max_all(I_ELE) )THEN


	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata massima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,640)  udm_out_str(udm_Q,port_max_all(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
      endif 
	   ENDIF
c+++++++
      if (port_min_all(I_ELE) .ne. R_UND) then

	     IF( flow_stat(ipg).LT.port_min_all(I_ELE) )THEN

	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata minima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,650)  udm_out_str(udm_Q,port_min_all(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      	ENDIF
      endif


CMAR    ALLARMI
      msg='ASSETTO:'//trim(sigla_ass(ipg+1))

        call messtel(fl_int,fl_end,msg_int,msg)
	
cmar_        ALLARMI

	 if (port_max_all(I_ELE+1) .ne. R_UND) then

         IF( flow_stat(ipg+1).GT.port_max_all(I_ELE+1) )THEN

	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata massima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,640)  udm_out_str(udm_Q,port_max_all(i_ele+1),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
      endif 
	   ENDIF

c+++++++

      if (port_min_all(I_ELE) .ne. R_UND) then

	     IF( flow_stat(ipg+1).LT.port_min_all(I_ELE+1) )THEN

	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata minima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,650)  udm_out_str(udm_Q,port_min_all(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      	ENDIF
      endif



CMF
      ELSE





cmar_        ALLARMI

	 if (port_max_all(I_ELE) .ne. R_UND) then

         IF( PORTTJ(I_ELE).GT.port_max_all(I_ELE) )THEN


cmar_        ALLARMI


  
cmar_        ALLARMI

	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata massima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,640)  udm_out_str(udm_Q,port_max_all(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
      endif 
	   ENDIF

      if (port_min_all(I_ELE) .ne. R_UND) then

	     IF( PORTTJ(I_ELE).LT.port_min_all(I_ELE) )THEN

	      call tipo_err (i_ele,err_aa,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
      msg='Superata la soglia di allarme sulla Portata minima'
            call messtel(fl_int,fl_end,msg_int,msg)
      
        write(msg,650)  udm_out_str(udm_Q,port_min_all(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      	ENDIF
      endif


CMAR    ALLARMI

CMF
	ENDIF
cmar_log_sem+++++++++++++++++++++++++++++++



      if (centrale.or.tipoScenario.eq.TIPOSCEN_RETE_din) then
	  dp_on = dp_on_ce
	else
	  dp_on = dp_on_vl
	endif
      if (centrale) then
	  format_p = "('p aspirazione: ',A10,' - p mandata: ',A25)"
	else
	  format_p = "('p monte: ',A10,' - p valle: ',A25)"
	endif

   	if(htipto(i_ele) .eq. e_vg) then
         call qmaxvg(i_ele,qmaxtx(i_ele),flag_vlim)
cmar-tmin-22-12
c      temptj(iv)=touttz(i_ele)
cmar-tmin-22-12
	else
	   flag_vlim = .false.
      end if

c  confronta lo stato finale dell'elemento con quello richiesto per essa dall'utente
      if((stadatc(htcitj(i_ele)).ne.hstatj(i_ele)))then
C------->SET NON RISPETTATO

	  ind1 = stadatc(htcitj(i_ele))
        if (centrale) then
	    if (ind1.eq.sta_pv) then
		  ind1 = sta_pman
		elseif (ind1.eq.sta_pm) then
		  ind1 = sta_pasp
		elseif (ind1.eq.sta_pq) then
		  ind1 = sta_pamq
	    endif
        endif
	  ind2 = hstatj(i_ele)
        if (centrale) then
	    if (ind2.eq.sta_pv) then
		  ind2 = sta_pman
		elseif (ind2.eq.sta_pm) then
		  ind2 = sta_pasp
		elseif (ind2.eq.sta_pq) then
		  ind2 = sta_pamq
	    endif
        endif

        call tipo_err (i_ele,err_ii,msg)
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='Lo stato e''cambiato '
	  call messtel(fl_int,fl_end,msg_int,msg)
        msg='   da '// trim(statai(ind1)) 
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='   a ' // trim(statai(ind2)) //'.'
	  call messtel(fl_int,fl_end,msg_int,msg)

      else
       if (.not.fl_sim) then
        if (hstatj(i_ele).eq.sta_q) then
          if (abs(porttj(i_ele)-qset_real(i_ele)).gt.repsQ) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Modificata la PORTATA di set:'
            call messtel(fl_int,fl_end,msg_int,msg)
cmar_q_mees_b
      if (htipto(i_ele).eq.e_ce ) then

        write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0),
     *                     udm_out_str(udm_Q,porttj(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


c
	a= (porttj(i_ele)-qset_real(i_ele))/qset_real(i_ele) 
	a=a*100.0


        write(msg,397)'Scostamento: ',abs(a),'%'
	      call messtel(fl_int,fl_end,msg_int,msg)

      else



cmar_q_mees_b

            write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0),
     *                     udm_out_str(udm_Q,porttj(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


      endif

cmar_q_mees_b

cmar
c      endif

cmar






cstaz-corr
c            if (htipto(_ele) .eq. e_ce) then
            
cstaz-corr-end
           end if
cmar_q_mess
      
cmar-modify
       if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
cmar             fl_fatal(ipg) = fl_fatal(ipg) + sta_q
       fl_fatal(ipg) = fl_fatal(ipg) 
            endif
cmarmodify
c	endif


        else if (hstatj(i_ele).eq.sta_pm) then
          if (abs(prestj(im)-pmsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE ASPIRAZIONE di set:'
	      else
               msg = 'Modificata la PRESSIONE MONTE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)

cmar_p_mess

cmar_p_set
cmar_p_set passa di qua
cmar_p_set
       if (htipto(i_ele).eq.e_ce ) then

          write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(im),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


c
      a= (pmsetj(i_ele)-prestj(im))/prestj(im)
      a=a*100.
c       a= 17.32

        write(msg,*)'Scostamento : ',abs(a),'%'
c       write(msg,397)'Scostamento : ',a,'%'
	      call messtel(fl_int,fl_end,msg_int,msg)

      else



cmar_p_mess

            write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(im),1)
            call messtel(fl_int,fl_end,msg_int,msg)

cmar_p_mess

      endif
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then

c             fl_fatal(ipg) = fl_fatal(ipg) + sta_pm
cmar_p_mess
          fl_fatal(ipg) = fl_fatal(ipg) + 1
cmar_p_mess
            endif

          end if
        else if (hstatj(i_ele).eq.sta_pv) then
          if (abs(prestj(iv)-pvsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE MANDATA di set:'
	      else
               msg = 'Modificata la PRESSIONE VALLE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)

cmar_p_mess


       if (htipto(i_ele).eq.e_ce ) then

          write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(im),1)
	      call messtel(fl_int,fl_end,msg_int,msg)


c
	a= (pmsetj(i_ele)-prestj(im))/prestj(im)

      a=a*100.
        write(msg,397)'Scostamento : ',abs(a),'%'
	      call messtel(fl_int,fl_end,msg_int,msg)

      else



cmar_p_mess


            write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(iv),1)
            call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
cmar_p_mess              fl_fatal(ipg) = fl_fatal(ipg) + sta_pv
cmar_p_mess
                             fl_fatal(ipg) = fl_fatal(ipg) + 1

cmar_p_mess
        endif
cmarrrrrrrrrrrrrrrrrr
            endif

          end if
        end if
       end if
      end if

c  Verifica che siano rispettati i vincoli fisici
	if(htcitj(i_ele).ne.tc_off) then
c SIRE2000<>NUOVO SIRE
c--------------------------------------------------------------------------------
cc  se la portata della valvola è negativa si ha uno stato legale per i tipi di
cc  funzionamento che ammettono un flusso bidirezionale e per i quali si impongono 
cc  equazioni in pressione,purchè il valore assoluto della portata non superi la 
cc  portata massima consentita alla valvola
c  se la portata della valvola è negativa si ha uno stato legale per il
c  funzionamento TC_ON (bidirazionale) purchè il valore assoluto della
c  portata non superi la portata massima consentita alla valvola (cfr. modello
c  di centrale semplificata)
c--------------------------------------------------------------------------------
c SIRE2000<>NUOVO SIRE-end

            ltest_qmax = .true.
   	      if(htipto(i_ele).eq.e_vd) then   
c-------->NUOVO CONTROLLO	SOLO PER IMPIANTI RIDUZIONE
               if (hstatj(i_ele).ne.sta_on. and.
     *             hstatj(i_ele).ne.sta_fr .and.
     *             hstatj(i_ele).ne.sta_off .and.
     *             hstatj(i_ele).ne.sta_nr .and.
     *             (prestj(im)-prestj(iv)) .lt. dpmin(ipg))then
c                  ltest_qmax = .false.   ! il controllo sulla qmax viene fatto sempre
c dpmin deve essere in BAR ASSOLUTI
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Salto di pressione inferiore al minimo'// 
     *                  ' consentito per il corretto funzionamento'//
     *                  ' dell''impianto.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,150) udm_out_str(udm_P,
     *                           prestj(im)-prestj(iv),0),
     *                           udm_out_str(udm_P,dpmin(ipg),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
               endif

	      end if
	   
cs            if(htipto(i_ele).eq.e_cs) then   
csc----->verifica che siano rispettati i vincoli fisici
cs              pvmax = prestj(im)*romax(ipg)
cs              if (prestj(iv) .gt. pvmax +repsP) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Pressione di mandata superiore alla massima'//
cs     *                ' consentita.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Violazione massimo rapporto di compressione.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
cs     *                         udm_out_str(udm_P,pvmax,1)
cs	          call messtel(fl_int,fl_end,msg_int,msg)
csc                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
csc                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif 

cs              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Portata superiore al massimo'//
cs     *                ' consentito quando la potenza e'' massima.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cs     *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif
cs            endif 

          if (ltest_qmax) then
            if(abs(porttj(i_ele)) .gt. (qmaxtx(i_ele)+repsQ)) then
              if (htipto(i_ele) .ne. e_ce) then
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            if (flag_vlim) then
 	              msg = 'Portata superiore al massimo consentito'//
     *                     ' dalla velocita'' limite.'
	            else
                    msg = 'Portata superiore al massimo consentito.'
	            endif
	            call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                    write(msg,220) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                           udm_out_str(udm_Q,qmaxtx(i_ele),1)
cmar_rev_log_sem  	            call messtel(fl_int,fl_end,msg_int,msg)
	            if (flag_vlim) then
cmar_rev_log_sem                      write(msg,410) udm_out_str(udm_V,vlim_out(ipg),1)
cmar_rev_log_sem  	              call messtel(fl_int,fl_end,msg_int,msg)
	            endif
              endif
            endif
          endif

c--------->NUOVO CONTROLLO
            if (.not.centrale .and.
     -          touttz(i_ele) .lt. tsettx(i_ele)) then
               call tipo_err (i_ele,err_ww,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
	         msg = 'Temperatura inferiore alla minima consentita.'
	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,310) udm_out_str(udm_T,touttz(i_ele),0),
     *                        udm_out_str(udm_T,tsettx(i_ele),1)
	         call messtel(fl_int,fl_end,msg_int,msg)

	      end if
cmar-tmin
cmar-tmin!!!!!!!!!!!!!!!!valvola!!!!!!!!!!!!!!!!
          IF ( HTIPTO(i_ele) .eq. E_VL ) THEN

	if(touttz(i_ele) . lt. 273.15)then

cmar_log_sem 	 call tipo_err (i_ele,err_ok,msg)
       call tipo_err (i_ele,err_ww,msg)


               call messtel(fl_int,fl_end,msg_int,msg)

	          msg = 'Temperatura negativa.'

	         call messtel(fl_int,fl_end,msg_int,msg)

cm 
      	   if(touttz(i_ele) . gt. R_UND )then

	write(msg,310) udm_out_str(udm_T,touttz(i_ele),0)
	call messtel(fl_int,fl_end,msg_int,msg)

	endif
cm


	endif


	    ENDIF

cmar-tmin!!!!!!!!!!!!!!!!Imp.riduz.!!!!!!!!!!!!!!!!
      IF ( HTIPTO(i_ele) .eq. E_VD ) THEN

		if(touttz(i_ele) . lt. 273.15)then

	 call tipo_err (i_ele,err_ww,msg)
               call messtel(fl_int,fl_end,msg_int,msg)

	          msg = 'Temperatura negativa.'

	         call messtel(fl_int,fl_end,msg_int,msg)
cm
	if(touttz(i_ele) . gt. R_UND )then

	write(msg,310) udm_out_str(udm_T,touttz(i_ele),0)
	call messtel(fl_int,fl_end,msg_int,msg)

	endif
cm
	endif

	    ENDIF

cmar-tmin!!!!!!!!!!!!!!!!Imp.regol.!!!!!!!!!!!!!!!!
      IF ( HTIPTO(i_ele) .eq. E_VG ) THEN

		if(touttz(i_ele) . lt. 273.15)then

	 call tipo_err (i_ele,err_ww,msg)
               call messtel(fl_int,fl_end,msg_int,msg)

	          msg = 'Temperatura negativa.'

	         call messtel(fl_int,fl_end,msg_int,msg)

cm
      if(touttz(i_ele) . gt. R_UND )then

	write(msg,310) udm_out_str(udm_T,touttz(i_ele),0)
	call messtel(fl_int,fl_end,msg_int,msg)

	endif
cm
	endif

	    ENDIF

cmar-tmin

            if (htcitj(i_ele) .eq. tc_pq) then
c------------------------------------------------------------------------------
c regolazione TC_PQ: la valvola cerca di mantenere il valore della pressione di 
c valle uguale a PVset, purchè il valore della pressione di monte non scenda al
c di sotto di PMset e la portata non superi Qset
c------------------------------------------------------------------------------
                if (prestj(im) .lt. (pmsetj(i_ele)-repsP) .or.
     *              prestj(iv) .gt. (pvsetj(i_ele)+repsP) .or.
     *              porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                  call tipo_err (i_ele,err_ww,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Regolazione in pressione e portata non '//
     *                  'rispettata.'
	            call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr                  if(prestj(im) .lt. pmsetj(i_ele)) then
                  if(prestj(im) .lt. (pmsetj(i_ele)-repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(im),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
                  if(prestj(iv) .gt. (pvsetj(i_ele)+repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(iv),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
	            if (porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                    write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0)
     *                            ,udm_out_str(udm_Q,porttj(i_ele),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	            end if

                end if
            endif


		  if(diretj(i_ele)*porttj(i_ele).lt.-repsQ .and.
     -         set_non_ritorno(i_ele)) then
               call tipo_err (i_ele,err_ff,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
	         msg = 'Violata la direzionalita'' dell''elemento. '//
     *	           'Portata negativa:'
	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
               call messtel(fl_int,fl_end,msg_int,msg)
               l_ok = .false.
	      end if

	      if (abs(prestj(im)-prestj(iv)) .gt. dp_on) then
                if (abs(porttj(i_ele)).gt.repsQ .and.
     -              (hstatj(i_ele) .eq. sta_on .or.
     -               hstatj(i_ele) .eq. sta_fr)) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Delta pressione in stato di libero '//
     *                  ' flusso.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                           udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).lt.-repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).lt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).gt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Portata negativa '
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).gt.repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).gt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).lt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

                endif
            end if

cs-controlli spostati dopo il check sulle p e q
   	      if(htipto(i_ele).eq.e_cs .and. l_ok) then   
c----->verifica che siano rispettati i vincoli fisici
              pvmax = prestj(im)*romax(ipg)
              if (prestj(iv) .gt. pvmax +repsP) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Pressione di mandata superiore alla massima'//
     *                ' consentita.'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Violazione massimo rapporto di compressione.'
                call messtel(fl_int,fl_end,msg_int,msg)
                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
     *                         udm_out_str(udm_P,pvmax,1)
	          call messtel(fl_int,fl_end,msg_int,msg)
c                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
c                call messtel(fl_int,fl_end,msg_int,msg)
              endif 

              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Portata superiore al massimo'//
     *                ' consentito quando la potenza e'' massima.'
                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cmar_rev_log_sem                  call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cmar_rev_log_sem                  call messtel(fl_int,fl_end,msg_int,msg)
              endif
            endif
cs-end

      end if

C  ALLARMI
c      if (port_max_all(I_ELE) .ne. R_UND) then

c        IF( PORTTJ(I_ELE).GT.port_max_all(I_ELE) )THEN

c	      call tipo_err (i_ele,err_aa,msg)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c      msg='Superata la soglia di allarme sulla Portata massima'
c            call messtel(fl_int,fl_end,msg_int,msg)
      
c        write(msg,640)  udm_out_str(udm_Q,port_max_all(i_ele),1)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c      endif 
c	   ENDIF

c      if (port_min_all(I_ELE) .ne. R_UND) then
c
c	     IF( PORTTJ(I_ELE).LT.port_min_all(I_ELE) )THEN
c
c	      call tipo_err (i_ele,err_aa,msg)
c	      call messtel(fl_int,fl_end,msg_int,msg)
c      msg='Superata la soglia di allarme sulla Portata minima'
c            call messtel(fl_int,fl_end,msg_int,msg)
c      
c        write(msg,650)  udm_out_str(udm_Q,port_min_all(i_ele),1)
c	      call messtel(fl_int,fl_end,msg_int,msg)


c      	ENDIF
c      endif


CMAR    ALLARMI


110   format('p richiesta: ',A10,' - p proposta: ',A25)
!140   format('p monte: ',A10,' - p valle: ',A25)
!141   format('p aspirazione: ',A10,' - p mandata: ',A25)
150	format('delta p: ',A10,' - delta p min: ',A25)
210   format('Q richiesta: ',A10,' - Q proposta: ',A25)
c
397   format(A14,F4.2,A3)
c
220   format('Q: ',A10,' - Q max: ',A25)
230   format('Q: ',A25)
310	format('T: ',A10,' - T min: ',A25)
410   format('V lim: ',A25)
510   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('Q: ',A10,' - Q max: ',A25)
550   format('p man: ',A10,' - p man max: ',A25)
610   format('Q: ',A25)
630   format('P max: ',A25)
cmar_log_sem
640   format('Q max: ',A25)
650   format('Q min: ',A25)
      return
	end
c--------------------------------------------------------------------------------------
      subroutine  verst_bin_(i_ele,msg_int,fl_int,fl_end)
c************************************************************************************
c  Verifica la fattibilità di una condizione di esercizio di elemento binario
c  descritta dalla terna  (Pm,Pv,Q,T); 
c  solo per gli impianti di regolazione
c    verifica anche che la velocità gas non superi la velocità limite nel piping
c  .
c************************************************************************************
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/err.inc' !parametri costanti
	include '../inc/th.inc' !stati di regolazione
	include '../inc/ti.inc' !identificatori elementi
	include '../inc/tx.inc' !dati topologici elementi (qmaxtx)
	include '../inc/tj.inc' !cin (pressioni,portate...)
	include '../inc/tz.inc' 
   	include '../inc/ai.inc'
	include '../inc/jc.inc' !variabili per calcolo jacobiano nello stazionario
	include '../inc/top_app_ele.inc' 
   	include '../inc/staznw_par.inc'
   	include '../inc/conv.inc'
   	include '../inc/flag.inc'
   	include '../inc/stations.inc'

c
      integer*4 i_ele    !indice dell'elemento nella struttura dati completa
      character*(*)msg_int
	logical*2 fl_int,fl_end

      integer*4 im,iv, ipg, ind1,ind2
      real*4    qmax,pvmax,dp_on
      character*150 msg
	logical*2 flag_vlim

      real*4    qpwmce
	external  qpwmce

	logical*2 centrale

	logical*2 set_non_ritorno
	external set_non_ritorno

	logical*2 l_ok

      logical*2 ltest_qmax

	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external               udm_out_str

      character(80) format_p

	real*4 qset_real
	external qset_real
c-------------------------------------------------------------------------------------
cmar-----------
c        touttz(7411)=274
c         touttz(6001)=270
c        touttz(8011)=270
cmar-----------
      fl_int = .false.
      fl_end = .false.
      im = opumto(i_ele)
      iv = opuvto(i_ele)
      ipg = pgrato(i_ele)

	centrale = (htipto(i_ele).eq.e_ce.or.htipto(i_ele).eq.e_cs)

c inizializzazione
      flertj(i_ele) = err_ok
      l_ok = .true.

      if (centrale.or.tipoScenario.eq.TIPOSCEN_RETE_din) then
	  dp_on = dp_on_ce
	else
	  dp_on = dp_on_vl
	endif
      if (centrale) then
	  format_p = "('p aspirazione: ',A10,' - p mandata: ',A25)"
	else
	  format_p = "('p monte: ',A10,' - p valle: ',A25)"
	endif

   	if(htipto(i_ele) .eq. e_vg) then
         call qmaxvg(i_ele,qmaxtx(i_ele),flag_vlim)
	else
	   flag_vlim = .false.
      end if

c  confronta lo stato finale dell'elemento con quello richiesto per essa dall'utente
      if((stadatc(htcitj(i_ele)).ne.hstatj(i_ele)))then
C------->SET NON RISPETTATO

	  ind1 = stadatc(htcitj(i_ele))
        if (centrale) then
	    if (ind1.eq.sta_pv) then
		  ind1 = sta_pman
		elseif (ind1.eq.sta_pm) then
		  ind1 = sta_pasp
		elseif (ind1.eq.sta_pq) then
		  ind1 = sta_pamq
	    endif
        endif
	  ind2 = hstatj(i_ele)
        if (centrale) then
	    if (ind2.eq.sta_pv) then
		  ind2 = sta_pman
		elseif (ind2.eq.sta_pm) then
		  ind2 = sta_pasp
		elseif (ind2.eq.sta_pq) then
		  ind2 = sta_pamq
	    endif
        endif

        call tipo_err (i_ele,err_ii,msg)
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='Lo stato e''cambiato '
	  call messtel(fl_int,fl_end,msg_int,msg)
        msg='   da '// trim(statai(ind1)) 
	  call messtel(fl_int,fl_end,msg_int,msg)
	  msg='   a ' // trim(statai(ind2)) //'.'
	  call messtel(fl_int,fl_end,msg_int,msg)

      else
       if (.not.fl_sim) then
        if (hstatj(i_ele).eq.sta_q) then
          if (abs(porttj(i_ele)-qset_real(i_ele)).gt.repsQ) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
            msg = 'Modificata la PORTATA di set:'
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0),
     *                     udm_out_str(udm_Q,porttj(i_ele),1)
	      call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_q
            endif
cstaz-corr-end
           end if
        else if (hstatj(i_ele).eq.sta_pm) then
          if (abs(prestj(im)-pmsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE ASPIRAZIONE di set:'
	      else
               msg = 'Modificata la PRESSIONE MONTE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(im),1)
            call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_pm
            endif

          end if
        else if (hstatj(i_ele).eq.sta_pv) then
          if (abs(prestj(iv)-pvsetj(i_ele)).gt.repsP) then
C------->VALORE DEL SET NON RISPETTATO
            call tipo_err (i_ele,err_ii,msg)
	      call messtel(fl_int,fl_end,msg_int,msg)
	      if (centrale) then
               msg = 'Modificata la PRESSIONE MANDATA di set:'
	      else
               msg = 'Modificata la PRESSIONE VALLE di set:'
	      endif
            call messtel(fl_int,fl_end,msg_int,msg)
            write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                     udm_out_str(udm_P,prestj(iv),1)
            call messtel(fl_int,fl_end,msg_int,msg)
cstaz-corr
c            if (htipto(i_ele) .eq. e_ce) then
            if (htipto(i_ele).eq.e_ce .and. fl_fatal(ipg).ge.0) then
              fl_fatal(ipg) = fl_fatal(ipg) + sta_pv
            endif

          end if
        end if
       end if
      end if

c  Verifica che siano rispettati i vincoli fisici
	if(htcitj(i_ele).ne.tc_off) then
c SIRE2000<>NUOVO SIRE
c--------------------------------------------------------------------------------
cc  se la portata della valvola è negativa si ha uno stato legale per i tipi di
cc  funzionamento che ammettono un flusso bidirezionale e per i quali si impongono 
cc  equazioni in pressione,purchè il valore assoluto della portata non superi la 
cc  portata massima consentita alla valvola
c  se la portata della valvola è negativa si ha uno stato legale per il
c  funzionamento TC_ON (bidirazionale) purchè il valore assoluto della
c  portata non superi la portata massima consentita alla valvola (cfr. modello
c  di centrale semplificata)
c--------------------------------------------------------------------------------
c SIRE2000<>NUOVO SIRE-end

            ltest_qmax = .true.
   	      if(htipto(i_ele).eq.e_vd) then   
c-------->NUOVO CONTROLLO	SOLO PER IMPIANTI RIDUZIONE
               if (hstatj(i_ele).ne.sta_on. and.
     *             hstatj(i_ele).ne.sta_fr .and.
     *             hstatj(i_ele).ne.sta_off .and.
     *             hstatj(i_ele).ne.sta_nr .and.
     *             (prestj(im)-prestj(iv)) .lt. dpmin(ipg))then
c                  ltest_qmax = .false.   ! il controllo sulla qmax viene fatto sempre
c dpmin deve essere in BAR ASSOLUTI
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Salto di pressione inferiore al minimo'// 
     *                  ' consentito per il corretto funzionamento'//
     *                  ' dell''impianto.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,150) udm_out_str(udm_P,
     *                           prestj(im)-prestj(iv),0),
     *                           udm_out_str(udm_P,dpmin(ipg),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
               endif
cmar-tmin-2009-------------------------------------------------------------
              

C            	 if(touttz(i_ele) .lt. 273.15 ) then 

C	            call tipo_err (i_ele,err_ww,msg)
 
C 	            call messtel(fl_int,fl_end,msg_int,msg)
C	            msg = 'temperatura negativa!!!!!!!!!'
C	            call messtel(fl_int,fl_end,msg_int,msg)
cm                  write(msg,150) udm_out_str(udm_P,
cm     *                           prestj(im)-prestj(iv),0),
cm     *                           udm_out_str(udm_P,dpmin(ipg),1)
cm	            call messtel(fl_int,fl_end,msg_int,msg)

               
	
C              endif

cmar-tmin-2009-------------------------------------------------------------           

	      end if
cmar!!!!!!!!!!!!!!-----VALVOLE  SEMPLICI----------!!!!!!!!!!!!!!!!
cmar-tmin-2009

C      IF ( HTIPTO(i_ele) .eq. E_VL ) THEN

C          if(touttz(i_ele) .lt. 273.15 ) then 

C	            call tipo_err (i_ele,err_ww,msg)
C  	            call messtel(fl_int,fl_end,msg_int,msg)
C	            msg = 'temperatura negativa!!!!!!!!!'
C	            call messtel(fl_int,fl_end,msg_int,msg)      
C
C          endif

C	ENDIF

cmar-tmin-2009


CMAR!!!!!!!!!!!!!------IMPIANTI DI REGOLAZIONE----!!!!!!!!!!!!!!!!
cmar-tmin-2009

c      IF ( HTIPTO(i_ele) .eq. E_VG ) THEN

c      if(touttz(i_ele) .lt. 273.15 ) then 

c	            call tipo_err (i_ele,err_ww,msg)
c  	            call messtel(fl_int,fl_end,msg_int,msg)
c	            msg = 'temperatura negativa!!!!!!!!!'
c	            call messtel(fl_int,fl_end,msg_int,msg)      
c
c          endif

c	ENDIF

cmar-tmin-2009

	   
cs            if(htipto(i_ele).eq.e_cs) then   
csc----->verifica che siano rispettati i vincoli fisici
cs              pvmax = prestj(im)*romax(ipg)
cs              if (prestj(iv) .gt. pvmax +repsP) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Pressione di mandata superiore alla massima'//
cs     *                ' consentita.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Violazione massimo rapporto di compressione.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
cs     *                         udm_out_str(udm_P,pvmax,1)
cs	          call messtel(fl_int,fl_end,msg_int,msg)
csc                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
csc                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif 

cs              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
cs                call tipo_err (i_ele,err_ww,msg)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                msg = 'Portata superiore al massimo'//
cs     *                ' consentito quando la potenza e'' massima.'
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cs     *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cs                call messtel(fl_int,fl_end,msg_int,msg)
cs              endif
cs            endif 

          if (ltest_qmax) then
            if(abs(porttj(i_ele)) .gt. (qmaxtx(i_ele)+repsQ)) then
              if (htipto(i_ele) .ne. e_ce) then
                  call tipo_err (i_ele,err_ww,msg)
  	            call messtel(fl_int,fl_end,msg_int,msg)
	            if (flag_vlim) then
 	              msg = 'Portata superiore al massimo consentito'//
     *                     ' dalla velocita'' limite.'
	            else
                    msg = 'Portata superiore al massimo consentito.'
	            endif
	            call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                   write(msg,220) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                           udm_out_str(udm_Q,qmaxtx(i_ele),1)
cmar_rev_log_sem  	            call messtel(fl_int,fl_end,msg_int,msg)
	            if (flag_vlim) then
cmar_rev_log_sem                      write(msg,410) udm_out_str(udm_V,vlim_out(ipg),1)
cmar_rev_log_sem  	              call messtel(fl_int,fl_end,msg_int,msg)
	            endif
              endif
            endif
          endif

c--------->NUOVO CONTROLLO
            if (.not.centrale .and.
     -     touttz(i_ele) .lt. 273.15) then

c     -    touttz(i_ele) .lt. tsettx(i_ele)) then
  
cmar-tmin     -     touttz(i_ele) .lt. 273.15) then


               call tipo_err (i_ele,err_ww,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
cmar-tmin	         msg = 'Temperatura inferiore alla minima consentita.'
C1   
c       IF(tsettx(i_ele) .LE. 273.15) THEN  
	          msg = 'Temperatura negativa.'


	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,310) udm_out_str(udm_T,touttz(i_ele),0)
cmar-tmin			 ,

cmar-tmin     *                    udm_out_str(udm_T,tsettx(i_ele),1)
ccccccccccccccccc	         call messtel(fl_int,fl_end,msg_int,msg)
C1
c1	else
      IF (touttz(i_ele) .lt. tsettx(i_ele)) THEN
	 msg = 'Temperatura inferiore alla minima consentita.'


	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,310) udm_out_str(udm_T,touttz(i_ele),0),
     *                        udm_out_str(udm_T,tsettx(i_ele),1)
	         call messtel(fl_int,fl_end,msg_int,msg)
C1
	ENDIF

	      end if

            if (htcitj(i_ele) .eq. tc_pq) then
c------------------------------------------------------------------------------
c regolazione TC_PQ: la valvola cerca di mantenere il valore della pressione di 
c valle uguale a PVset, purchè il valore della pressione di monte non scenda al
c di sotto di PMset e la portata non superi Qset
c------------------------------------------------------------------------------
                if (prestj(im) .lt. (pmsetj(i_ele)-repsP) .or.
     *              prestj(iv) .gt. (pvsetj(i_ele)+repsP) .or.
     *              porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                  call tipo_err (i_ele,err_ww,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Regolazione in pressione e portata non '//
     *                  'rispettata.'
	            call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr                  if(prestj(im) .lt. pmsetj(i_ele)) then
                  if(prestj(im) .lt. (pmsetj(i_ele)-repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pmsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(im),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
                  if(prestj(iv) .gt. (pvsetj(i_ele)+repsP)) then
cgpe-corr-end
                    write(msg,110) udm_out_str(udm_P,pvsetj(i_ele),0),
     *                             udm_out_str(udm_P,prestj(iv),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
cgpe-corr	            else
                  endif
	            if (porttj(i_ele) .gt. (qset_real(i_ele)+repsQ)) then
cgpe-corr-end
                    write(msg,210) udm_out_str(udm_Q,qset_real(i_ele),0)
     *                            ,udm_out_str(udm_Q,porttj(i_ele),1)
	              call messtel(fl_int,fl_end,msg_int,msg)
	            end if

                end if
            endif


		  if(diretj(i_ele)*porttj(i_ele).lt.-repsQ .and.
     -         set_non_ritorno(i_ele)) then
               call tipo_err (i_ele,err_ff,msg)
               call messtel(fl_int,fl_end,msg_int,msg)
	         msg = 'Violata la direzionalita'' dell''elemento. '//
     *	           'Portata negativa:'
	         call messtel(fl_int,fl_end,msg_int,msg)
               write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
               call messtel(fl_int,fl_end,msg_int,msg)
               l_ok = .false.
	      end if

	      if (abs(prestj(im)-prestj(iv)) .gt. dp_on) then
                if (abs(porttj(i_ele)).gt.repsQ .and.
     -              (hstatj(i_ele) .eq. sta_on .or.
     -               hstatj(i_ele) .eq. sta_fr)) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
		        msg = 'Delta pressione in stato di libero '//
     *                  ' flusso.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                           udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).lt.-repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).lt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).gt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Portata negativa '
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,230) udm_out_str(udm_Q,porttj(i_ele),1)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

		      elseif((porttj(i_ele).gt.repsQ) .and.
     -                  ((centrale .and.
     -                    prestj(im).gt.prestj(iv)).or.
     -                   (.not.centrale .and.
     -                    prestj(im).lt.prestj(iv)))) then
                  call tipo_err (i_ele,err_ff,msg)
                  call messtel(fl_int,fl_end,msg_int,msg)
	            msg = 'Pressioni invertite rispetto al flusso del gas.'
	            call messtel(fl_int,fl_end,msg_int,msg)
                  write(msg,format_p) udm_out_str(udm_P,prestj(im),0),
     *                        udm_out_str(udm_P,prestj(iv),1)
	            call messtel(fl_int,fl_end,msg_int,msg)
                  l_ok = .false.

                endif
            end if

cs-controlli spostati dopo il check sulle p e q
   	      if(htipto(i_ele).eq.e_cs .and. l_ok) then   
c----->verifica che siano rispettati i vincoli fisici
              pvmax = prestj(im)*romax(ipg)
              if (prestj(iv) .gt. pvmax +repsP) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Pressione di mandata superiore alla massima'//
     *                ' consentita.'
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Violazione massimo rapporto di compressione.'
                call messtel(fl_int,fl_end,msg_int,msg)
                write(msg,550) udm_out_str(udm_P,prestj(iv),0),
     *                         udm_out_str(udm_P,pvmax,1)
	          call messtel(fl_int,fl_end,msg_int,msg)
c                write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
c                call messtel(fl_int,fl_end,msg_int,msg)
              endif 

              if (porttj(i_ele) .gt. qpwmce(i_ele)) then
                call tipo_err (i_ele,err_ww,msg)
                call messtel(fl_int,fl_end,msg_int,msg)
                msg = 'Portata superiore al massimo'//
     *                ' consentito quando la potenza e'' massima.'
                call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,540) udm_out_str(udm_Q,porttj(i_ele),0),
cmar_rev_log_sem       *                         udm_out_str(udm_Q,qpwmce(i_ele),1)
cmar_rev_log_sem                  call messtel(fl_int,fl_end,msg_int,msg)
cmar_rev_log_sem                  write(msg,630) udm_out_str(udm_PW,powmax(ipg),1)
cmar_rev_log_sem                  call messtel(fl_int,fl_end,msg_int,msg)
              endif
            endif
cs-end

      end if


110   format('p richiesta: ',A10,' - p proposta: ',A25)
!140   format('p monte: ',A10,' - p valle: ',A25)
!141   format('p aspirazione: ',A10,' - p mandata: ',A25)
150	format('delta p: ',A10,' - delta p min: ',A25)
210   format('Q richiesta: ',A10,' - Q proposta: ',A25)
220   format('Q: ',A10,' - Q max: ',A25)
230   format('Q: ',A25)
310	format('T: ',A10,' - T min: ',A25)
410   format('V lim: ',A25)
510   format('Q richiesta: ',A10,' - Q proposta: ',A25)
540   format('Q: ',A10,' - Q max: ',A25)
550   format('p man: ',A10,' - p man max: ',A25)
610   format('Q: ',A25)
630   format('P max: ',A25)

      return
	end

c-------------------------------------------------------
c-------------------------------------------------------
      subroutine tipo_err(           ! Gestisce il tipo di errore
     *                    i_elem,    ! I)Indice variabile
     *                    tip_err,   ! I)Tipo di errore
     *                    msg        ! O)Messsaggio da riportare
     *                   )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'
      include '../inc/flag.inc'
      include '../inc/tj.inc'

      integer*4      i_elem
	integer*4      tip_err
      character*(*)  msg


c---------------------------------------------------------------

      if (tip_err.gt.flertj(i_elem)) flertj(i_elem) = tip_err
      if (tip_err.gt.fl_ris_gen) fl_ris_gen = tip_err

     
       
       
      if (tip_err.ge.err_ko) then
	   msg='*******  rosso&1  ****** '
	   
c     *        *******  1  *******'
	else if (tip_err.ge.err_ff) then
	 msg='*******  rosso&2  *******'
c	   msg='*******  2  *******'

	else if (tip_err.ge.err_ii) then
	msg='*******  giallo&2  *******'
c	   msg='*******  2  *******'

	else if (tip_err.ge.err_ww) then
	msg='*******  giallo&1  *******'
c	   msg='*******  1  *******'

      else if (tip_err.ge.err_aa) then
	   msg='*****    verde&allarme    *****'
c	   msg='*****    allarme    *****'
	else if (tip_err.ge.err_ok) then
       msg='*******  verde& *******'
c	   msg='*******  1  *******'

	else
         msg=''
	   continue
	endif

      RETURN
      END
c-------------------------------------------------------



c-------------------------------------------------------
      subroutine tipo_err_orig(           ! Gestisce il tipo di errore
     *                    i_elem,    ! I)Indice variabile
     *                    tip_err,   ! I)Tipo di errore
     *                    msg        ! O)Messsaggio da riportare
     *                   )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'
      include '../inc/flag.inc'
      include '../inc/tj.inc'

      integer*4      i_elem
	integer*4      tip_err
      character*(*)  msg


c---------------------------------------------------------------

      if (tip_err.gt.flertj(i_elem)) flertj(i_elem) = tip_err
      if (tip_err.gt.fl_ris_gen) fl_ris_gen = tip_err

      if (tip_err.ge.err_ko) then
	   msg='*******  System error  *******'
	else if (tip_err.ge.err_ff) then
	   msg='*******  Fatal error   *******'
	else if (tip_err.ge.err_ww) then
	   msg='*******  Warning error *******'
	else if (tip_err.ge.err_ii) then
	   msg='*****  Information error *****'
	else if (tip_err.ge.err_ok) then
	   msg='*****    Information     *****'
	else
         msg=''
	   continue
	endif

      RETURN
      END
c-------------------------------------------------------
c-------------------------------------------------------
      logical*2 function rete_ko()

      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/th.inc'
	include '../inc/ti.inc' !identificatori elementi
      include '../inc/tj.inc'
      include '../inc/staznw_par.inc'

	integer itp,iel,im,iv
	logical*2 e_pmpnt,e_tmpnt,e_druni
     -         ,e_drvlv,e_onvlv,e_dpvlv
     -         ,e_drcen,e_oncen,e_dpcen

	logical*2 set_non_ritorno
	external set_non_ritorno


      e_pmpnt = .false.
      e_tmpnt = .false.
      e_druni = .false.
      e_drvlv = .false.
      e_onvlv = .false.
      e_dpvlv = .false.
      e_drcen = .false.
      e_oncen = .false.
      e_dpcen = .false.

         do iel=otipti(e_pu),etipti(e_pu)
              if(prestj(iel) .lt. pmin_def) then
	          e_pmpnt = .true.
	        endif
              if(temptj(iel) .lt. tmin_def) then
	          e_tmpnt = .true.
	        endif
         enddo

      do itp=1,ne_uni
         do iel=otipti(te_uni(itp)),etipti(te_uni(itp))
		    if(diretj(iel)*porttj(iel).lt.-repsQ .and.
     -           set_non_ritorno(iel)) then
	          e_druni = .true.
	        endif
         enddo
	enddo
      do itp=1,ne_vlv
         do iel=otipti(te_vlv(itp)),etipti(te_vlv(itp))
	        im = opumto(iel)
	        iv = opuvto(iel)
		    if(diretj(iel)*porttj(iel).lt.-repsQ .and.
     -           set_non_ritorno(iel)) then
	          e_drvlv = .true.
	        endif
	        if (abs(prestj(im)-prestj(iv)) .gt. dp_on_vl) then
                if (abs(porttj(iel)).gt.repsQ .and.
     -              (hstatj(iel) .eq. sta_on .or.
     -               hstatj(iel) .eq. sta_fr)) then
	             e_onvlv = .true.
		      elseif(porttj(iel).lt.-repsQ .and.
     -                 prestj(im).gt.prestj(iv)) then
	             e_dpvlv = .true.
		      elseif(porttj(iel).gt.repsQ .and.
     -                 prestj(im).lt.prestj(iv)) then
	             e_dpvlv = .true.
                endif
              end if
         enddo
	enddo

      do itp=1,ne_cen
         do iel=otipti(te_cen(itp)),etipti(te_cen(itp))
	        im = opumto(iel)
	        iv = opuvto(iel)
		    if(diretj(iel)*porttj(iel).lt.-repsQ .and.
     -           set_non_ritorno(iel)) then
	          e_drcen = .true.
	        endif
	        if (abs(prestj(im)-prestj(iv)) .gt. dp_on_ce) then
                if (abs(porttj(iel)).gt.repsQ .and.
     -              (hstatj(iel) .eq. sta_on .or.
     -               hstatj(iel) .eq. sta_fr)) then
	             e_oncen = .true.
		      elseif(porttj(iel).lt.-repsQ .and.
     -                 prestj(im).lt.prestj(iv)) then
	             e_dpcen = .true.
		      elseif(porttj(iel).gt.repsQ .and.
     -                 prestj(im).gt.prestj(iv)) then
	             e_dpcen = .true.
                endif
              end if
         enddo
	enddo

      rete_ko = e_pmpnt .or. e_tmpnt .or. e_druni .or.
     -          e_drvlv .or. e_onvlv .or. e_dpvlv .or. 
     -          e_drcen .or. e_oncen .or. e_dpcen


      RETURN
      END

c-------------------------------------------------------
      logical*2 function simula_ce(iel)     ! true è simulabile


      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'
	include '../inc/ti.inc' !identificatori elementi
      include '../inc/tj.inc'
      include '../inc/staznw_par.inc'

	integer iel


      simula_ce =	(porttj(iel).gt.repsQ .and.
     -   prestj(opuvto(iel))-prestj(opumto(iel)) .gt. dp_on_ce)


      RETURN
      END


	      subroutine P_alarm_conn_ko()
c***********************************************************************************
c  Questa subroutine viene chiamata dopo la risoluzione stazionaria della rete col
c  metodo di Newton per verificare la fattibilità della soluzione trovata negli
c  elementi speciali.Nasce dall'unione di tre routines:VERSTEL(Sire,Pseudo),OUTSTAIL
c  (Sire,Pseudo),STAMSTA(Sire).
c  Si possono distinguere 2 tipi di controllo:
c---->1:confronta lo stato finale dell'elemento con quello richiesto per esso
c       dall'utente(cioè col valore di hstatj corrispondente ad htcitj) e 
c       segnala cambiamenti.Tali controlli sono eseguiti su tutti gli
c       elementi speciali. 
c---->2:verifica che siano rispettati i vincoli fisici sugli elementi della rete
c       e segnala con un messaggio eventuali violazioni.Tali controlli sono 
c       eseguiti sugli elementi speciali e sui punti
c***********************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/top_app_ele.inc'
c-elena
   	include '../inc/staznw_par.inc'
c-elena
c-UDM
   	include '../inc/conv.inc'
c-UDM-end






      integer*4 i,j,im,iv,ind1,ind2,k,jstaz
	character*100 msg,msg_int
c     dimensioni:snomtv--->30,statai(i)--->80

      logical*2 fl_int,   !flag per stampa intestazione
     *          fl_end    !flag per stampa riga bianca alla fine dei messaggi

      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno

      logical*2 elem_bin,elem_uni
      external elem_bin,elem_uni

	integer*4 pg,pg_app,pg_ass
      logical*2 fl_sim_ok(max_ce),
     *          flag_mf   !flag se multifunzione

	real*4 qset_real
	external qset_real
c-udm 17/5/06
	real*4 pres_appo
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end

c---------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      integer*4 iel,ipu,last_e,last_u,ivet
      integer*4 el_primo_en,el_primo_us
      logical*2 primo_en,primo_us

cmar_press_%

      integer*4 pto_m, pto, pto_v, pu, a, p, b, ie
	real*4 pres_c

cmar_press_%

!-------------------------------------------
      primo_en=.false.
      primo_us=.false.
!-------------------------------------------

    

!-PUNTI
    
      do ipu=otipti(e_pu),etipti(e_pu)
 
         oseeto(ipu)=ipu
         oseuto(ipu)=ipu
         last_e=ipu
         last_u=ipu
	  

	
!-BINARI 
   

         do ivet=1,ne_bin
          do iel=otipti(te_bin(ivet)),etipti(te_bin(ivet))


c
c	        do while (htipto(iel).ne.1)
c	
             if(ipu.eq.opuvto(iel))then
                oseeto(last_e)=iel
                oseeto(iel)=ipu

                last_e=iel
cmar_press_%
            	pto=ipu
          	pres_c=max(pres_c,prestj(pto))


               pto_m=oseuto(iel)

               pres_c=max(pres_c,prestj(pto))




cmar_press_%

             endif
c		ipu=pto_m

             if(ipu.eq.opumto(iel))then
                oseuto(last_u)=iel
                oseuto(iel)=ipu

                last_u=iel

cmar_press_%

  	pto=ipu
          	pres_c=max(pres_c,prestj(pto))


               pto=oseeto(iel)

               pres_c=max(pres_c,prestj(pto))

cmar_press_%
             endif



c
c	enddo
c


          enddo
         enddo
   
!-UNARI
         do ivet=1,ne_uni
          do iel=otipti(te_uni(ivet)),etipti(te_uni(ivet))
             if(ipu.eq.opuvto(iel))then

                oseeto(last_e)=iel
                oseeto(iel)=ipu



                last_e=iel
             endif
          enddo
         enddo
      enddo
!-------------------------------------------
     '
      do ivet=ne_uni,1,-1
         do iel=etipti(te_uni(ivet)),otipti(te_uni(ivet))-1
            oseuto(iel)=off_oi
            if(primo_en)then
               oseuto(last_e)=iel
            else
               primo_en=.true.
               el_primo_en=iel
            endif
            last_e=iel
	
          enddo
      enddo
!-------------------------------------------
!-poiche' tutti unari entranti
      el_primo_us=off_dp
!-------------------------------------------
      oseeto(off_oi)=i_und
      if(primo_us)then
        oseeto(off_dp)=el_primo_us
      else
        oseeto(off_dp)=off_dp
      endif
      oseeto(off_re)=off_re

      if(primo_en)then
        oseuto(off_oi)=el_primo_en
      else
        oseuto(off_oi)=off_oi
      endif
      oseuto(off_dp)=i_und
      oseuto(off_re)=off_re
!-------------------------------------------
       pres_c=0

      
cccccccccc      do pu=otipti(e_pu),etipti(e_pu)

c	ipu=pu	
cccccccccc        a = oseeto (pu)
cccccccccc  	p=pu

c      do while ( htipto (a) .ne. 1 )
      
c	a = oseeto (pu)
	

c      p=oseuto(a)

c	enddo


  
c       if(ipu.eq.opuvto(iel))then

c	 endif

cccccccccc            b = oseuto (pu)
cccccccccc  		p = pu

cccccccccc        do while ( htipto (b) .ne. 1 )
      
cccccccccc  	b = oseuto (p)
	

cccccccccc        p = oseeto(b)

cccccccccc  	enddo

cccccccccc  	enddo

cccc
        
      do iel=otipti(e_pu),etipti(e_pu)

        
         ie = oseuto(iel)
	if (htipto(ie).NE.e_tr)  then

	pres_c=max(pres_c,prestj(iel))
		 
      ie = oseuto(ie)

      endif


	   ie = oseeto(iel)
	   do while (ie .ne. iel .and. fl_nocap(pgrato(iel)))
		  if(htipto(ie) .eq. e_tr) fl_nocap(pgrato(iel)) = .false.
      	  ie = oseeto(ie)
         enddo
	   if (fl_nocap(pgrato(iel))) then
           volntx(iel) = 1
	   else
           volntx(iel) = 0
	   endif
      enddo

cccc



      RETURN
      END


	      subroutine P_alarm_conn(iel,pres_c)
c***********************************************************************************
c  Questa subroutine viene chiamata dopo la risoluzione stazionaria della rete col
c  metodo di Newton per verificare la fattibilità della soluzione trovata negli
c  elementi speciali.Nasce dall'unione di tre routines:VERSTEL(Sire,Pseudo),OUTSTAIL
c  (Sire,Pseudo),STAMSTA(Sire).
c  Si possono distinguere 2 tipi di controllo:
c---->1:confronta lo stato finale dell'elemento con quello richiesto per esso
c       dall'utente(cioè col valore di hstatj corrispondente ad htcitj) e 
c       segnala cambiamenti.Tali controlli sono eseguiti su tutti gli
c       elementi speciali. 
c---->2:verifica che siano rispettati i vincoli fisici sugli elementi della rete
c       e segnala con un messaggio eventuali violazioni.Tali controlli sono 
c       eseguiti sugli elementi speciali e sui punti
c***********************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/top_app_ele.inc'
c-elena
   	include '../inc/staznw_par.inc'
c-elena
c-UDM
   	include '../inc/conv.inc'
c-UDM-end






      integer*4 i,j,im,iv,ind1,ind2,k,jstaz
	character*100 msg,msg_int
c     dimensioni:snomtv--->30,statai(i)--->80

      logical*2 fl_int,   !flag per stampa intestazione
     *          fl_end    !flag per stampa riga bianca alla fine dei messaggi

      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno

      logical*2 elem_bin,elem_uni
      external elem_bin,elem_uni

	integer*4 pg,pg_app,pg_ass
      logical*2 fl_sim_ok(max_ce),
     *          flag_mf   !flag se multifunzione

	real*4 qset_real
	external qset_real
c-udm 17/5/06
	real*4 pres_appo
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end

c---------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      integer*4 iel,ipu,last_e,last_u,ivet
      integer*4 el_primo_en,el_primo_us
      logical*2 primo_en,primo_us

cmar_press_%

      integer*4 pto_m, pto, pto_v, pu, a, p, b, ie
	real*4 pres_c

cmar_press_%

!-------------------------------------------
      primo_en=.false.
      primo_us=.false.
!-------------------------------------------


  

       pres_c=prestj(iel)


      
        
c      do iel=otipti(e_pu),etipti(e_pu)

        
         
100   continue
       ie = oseuto(iel)


	if (htipto(ie).NE.e_tr)  then

	pres_c=max(pres_c,prestj(ie))
		 
c      ie = oseuto(ie)
	else
      iel = oseuto(iel)
	goto 100
    


      endif


	   ie = oseeto(iel)
	  	if (htipto(ie).NE.e_tr)  then

	pres_c=max(pres_c,prestj(ie))
		 
c      ie = oseuto(ie)
	else
	
      iel = oseuto(iel)
	goto 100


      endif

	  
c      enddo

cccc



      RETURN
      END








         subroutine P_alarm_conn_orig()
c***********************************************************************************
c  Questa subroutine viene chiamata dopo la risoluzione stazionaria della rete col
c  metodo di Newton per verificare la fattibilità della soluzione trovata negli
c  elementi speciali.Nasce dall'unione di tre routines:VERSTEL(Sire,Pseudo),OUTSTAIL
c  (Sire,Pseudo),STAMSTA(Sire).
c  Si possono distinguere 2 tipi di controllo:
c---->1:confronta lo stato finale dell'elemento con quello richiesto per esso
c       dall'utente(cioè col valore di hstatj corrispondente ad htcitj) e 
c       segnala cambiamenti.Tali controlli sono eseguiti su tutti gli
c       elementi speciali. 
c---->2:verifica che siano rispettati i vincoli fisici sugli elementi della rete
c       e segnala con un messaggio eventuali violazioni.Tali controlli sono 
c       eseguiti sugli elementi speciali e sui punti
c***********************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tv.inc'
   	include '../inc/ai.inc'
   	include '../inc/top_app_ele.inc'
c-elena
   	include '../inc/staznw_par.inc'
c-elena
c-UDM
   	include '../inc/conv.inc'
c-UDM-end
      integer*4 i,j,im,iv,ind1,ind2,k,jstaz
	character*100 msg,msg_int
c     dimensioni:snomtv--->30,statai(i)--->80

      logical*2 fl_int,   !flag per stampa intestazione
     *          fl_end    !flag per stampa riga bianca alla fine dei messaggi

      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno

      logical*2 elem_bin,elem_uni
      external elem_bin,elem_uni

	integer*4 pg,pg_app,pg_ass
      logical*2 fl_sim_ok(max_ce),
     *          flag_mf   !flag se multifunzione

	real*4 qset_real
	external qset_real
c-udm 17/5/06
	real*4 pres_appo
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end

c---------\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

      integer*4 iel,ipu,last_e,last_u,ivet
      integer*4 el_primo_en,el_primo_us
      logical*2 primo_en,primo_us
!-------------------------------------------
      primo_en=.false.
      primo_us=.false.
!-------------------------------------------


!-PUNTI
    
      do ipu=otipti(e_pu),etipti(e_pu)
       
         oseeto(ipu)=ipu
         oseuto(ipu)=ipu
         last_e=ipu
         last_u=ipu
	  

!-BINARI 


         do ivet=1,ne_bin
          do iel=otipti(te_bin(ivet)),etipti(te_bin(ivet))

             if(ipu.eq.opuvto(iel))then
                oseeto(last_e)=iel
                oseeto(iel)=ipu
	

                last_e=iel
             endif

             if(ipu.eq.opumto(iel))then
                oseuto(last_u)=iel
                oseuto(iel)=ipu
	
	
                last_u=iel
             endif
          enddo
         enddo
     
!-UNARI
         do ivet=1,ne_uni
          do iel=otipti(te_uni(ivet)),etipti(te_uni(ivet))
             if(ipu.eq.opuvto(iel))then

                oseeto(last_e)=iel
                oseeto(iel)=ipu

		


                last_e=iel
             endif
          enddo
         enddo
      enddo
!-------------------------------------------
      
      do ivet=ne_uni,1,-1
         do iel=etipti(te_uni(ivet)),otipti(te_uni(ivet))-1
            oseuto(iel)=off_oi
            if(primo_en)then
               oseuto(last_e)=iel
            else
               primo_en=.true.
               el_primo_en=iel
            endif
            last_e=iel
	
          enddo
      enddo
!-------------------------------------------
!-poiche' tutti unari entranti
      el_primo_us=off_dp
!-------------------------------------------
      oseeto(off_oi)=i_und
      if(primo_us)then
        oseeto(off_dp)=el_primo_us
      else
        oseeto(off_dp)=off_dp
      endif
      oseeto(off_re)=off_re

      if(primo_en)then
        oseuto(off_oi)=el_primo_en
      else
        oseuto(off_oi)=off_oi
      endif
      oseuto(off_dp)=i_und
      oseuto(off_re)=off_re
!-------------------------------------------



  
      RETURN
      END