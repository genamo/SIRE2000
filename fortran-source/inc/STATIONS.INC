
      INTEGER*4  first_type, type_num, first_unit, unit_num
      INTEGER*4  vert_poi
      INTEGER*4  stat_ord  ! =1 compressori in parallelo; 2 in serie
      INTEGER*4  set_flag
      integer*4   htelec
      integer*4  tipo_criterio
      INTEGER*4  vert_typ       ! tipo di stazione
c
      INTEGER*4  first_assetto  ! puntatore al primo assetto
      INTEGER*4  num_assetti    ! numero effettivo di assetti
      INTEGER*4  num_coll       ! numero effettivo di collettori di centrale
      INTEGER*4  coll_num_ce    ! numero effettivo di collettori di centrale

      INTEGER*4  num_unari      ! numero di unari presenti nella centrale MF
cgpe      INTEGER*4  ind_ass_att    ! indice assetto attivo in rete
c
c-tbg_attivi
      INTEGER*4  num_tbg_attivi   ! numero di tbg attivi nella centrale/assetto
c-tbg_attivi-end

      REAL*4     pasp_stat,pmand_stat,tasp_stat,flow_stat,TARIA
     *          ,flow_stat_in
cgpe      REAL*4     pasp_stat_dp,pmand_stat_dp

      real*4     comp_ratio,tmand_stat,tot_cons,
     *           stat_vars,delpr1,delpr2,head,flowm,flow_ric,
     *           pres_int,temp_int,delprint,deltrint,
     *           q_riciclo
c-bp05
     *          ,power_perc_der
     *          ,PERC_POW_STAZIONE
c-bp05-end
c-report
     *          ,power_tot
     *          ,nom_max_pow
     *          ,nom_max_pow_der
     *          ,nom_max_pow_vinc
     *          ,nom_min_pow
c-report-end

      REAL*4     coll_cen_as,coll_cen_ma
      INTEGER*4  fl_ris_ce    ! flag relativo alla fattibilità del punto di lavoro
      integer*4  fl_fatal     ! O) risultato della simulazione per ogni centrale
cgpe					      !    = 0  => centrale ok
cgpe						  !    = 1  => centrale ok, con recupero
cgpe						  !    = -k => centrale ko, con errore fatale k
c
c-bp08
c per due assetti provenienti dalla stessa MF o capbon hanno lo stesso valore per il flag
      logical*2  fl_cen_punto_ko ! = .T. => e' stato calcolato il punto ko
c-bp08-end
      character*2 hstato
	  logical*2   fl_simu
      character*(tit_len) sigla_ass  ! sigla centrale-assetto

      common/STATIONS/ first_unit(MAX_VERT), unit_num(MAX_VERT),
     *       first_type(MAX_VERT), type_num(MAX_VERT),
     *       vert_poi(max_vert),
     *       stat_ord(max_vert), set_flag(max_vert),
     *       htelec(MAX_VERT), tipo_criterio(max_vert),
     *       vert_typ(MAX_VERT),
     *       first_assetto(MAX_VERT), num_assetti(MAX_VERT),
     *       num_coll(MAX_VERT),num_unari(MAX_VERT),
c-tbg_attivi
     *       num_tbg_attivi(MAX_VERT),
c-tbg_attivi-end
     *       coll_cen_as(MAX_VERT),coll_cen_ma(MAX_VERT),
     *       fl_ris_ce(MAX_VERT),
     *       fl_fatal(MAX_VERT),
cgpe     *       ind_ass_att(max_vert),
     *	     flow_stat_in(max_vert),
     *	     flow_stat(max_vert),TARIA(max_vert),
     *       pasp_stat(max_vert),pmand_stat(max_vert),
cgpe     *       pasp_stat_dp(max_vert),pmand_stat_dp(max_vert),
     *       tasp_stat(max_vert),
     *	     comp_ratio(max_vert),tot_cons(max_vert),
     *       tmand_stat(max_vert),stat_vars(max_stat_vars,MAX_VERT),
     *       pres_int(max_vert),temp_int(max_vert),
     *       delprint(max_vert),deltrint(max_vert),
     *       delpr1(MAX_VERT),delpr2(MAX_VERT),head(MAX_VERT),
     *       flowm(MAX_VERT),flow_ric(MAX_VERT),
     *       q_riciclo(MAX_VERT),
c-bp05
     *       power_perc_der(max_vert),
     *       PERC_POW_STAZIONE(max_vert),
c-bp05-end
c-report
     *       power_tot(max_vert),
     *       nom_max_pow(max_vert),
     *       nom_max_pow_der(max_vert),
     *       nom_max_pow_vinc(max_vert),
     *       nom_min_pow(max_vert),
c-report-end
     *       hstato(max_vert),
     *       fl_simu(max_vert),
c-bp08
     *       fl_cen_punto_ko(max_vert),
c-bp08-end
     *       sigla_ass(max_vert)

c*******************************************************************************************
c-elena
c
c   Dati di centrale, da visualizzare sui report di scenario ,ricavabili dai
c   risultati della simulazione
c

      real*4 cons_su_qout,
     *       fuel_su_ptt,
     *       ptt_su_pft


      common /report_scenario/cons_su_qout(max_vert),
     *       fuel_su_ptt(max_vert),
     *       ptt_su_pft(max_vert)

c*******************************************************************************************
c-elena
c
c  Area per appoggio dati topologici usati nella definizione del vettore STAT_VARS
c
      real*4 flow_prog,       !portata di progetto
     *       delpin,          !delta pressione in aspirazione
     *       delpout,         !delta pressione in mandata
     *       dtout,           !delta temperatura in mandata
     *       max_pout,        !massima pressione in mandata
     *       max_tout         !massima temperatura in mandata

      common /stations_appo/ flow_prog(max_vert),       
     *       delpin(max_vert),          
     *       delpout(max_vert),         
     *       dtout(max_vert),           
     *       max_pout(max_vert),        
     *       max_tout(max_vert)
c-------------------------------------------------------------------------------------------         