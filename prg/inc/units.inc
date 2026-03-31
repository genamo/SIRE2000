c
      INTEGER*4 unit_ind, unit_avail
     *         ,unit_attiv_tlc

      INTEGER*4 unit_avail_cin

      REAL*4    unit_perc, unit_rev, unit_eff_corr, unit_prior,
     *          unit_eff, unit_power, unit_hrate, unit_cons,
     *          unit_temp, unit_flow, unit_head, unit_max,
     *          unit_power_corr, unit_rev_rat
	  real*4    unit_flow_rat !cport
      real*4    unit_crit_min, unit_crit_max
      real*4    unit_min_head, unit_max_head 
      real*4    unit_min_flow, unit_max_flow 
      real*4    unit_delta_pasp, unit_delta_pman 
      real*4    unit_flow_prog 

      real*4    unit_start_cost   ! costo di accensione
      real*4    unit_shut_cost    ! costo di spegnimento
cgpe
! variazione percentuale del consumo specifico della turbina
      real*4    unit_hrate_corr   
      real*4    fl_flow_man
cgpe-end
	  integer*4 tg_comp_id ! sergio

      common/UNITS/ unit_ind(max_unit), unit_avail(max_unit),
     *       unit_attiv_tlc(max_unit),
     *       unit_avail_cin(max_unit),
     *       unit_prior(max_unit), unit_perc(max_unit),
     *       unit_eff_corr(max_unit), unit_power_corr(max_unit),
     *	     unit_rev(max_unit), unit_flow(max_unit),
     *       unit_head(max_unit), unit_eff(max_unit),
     *       unit_power(max_unit), unit_hrate(max_unit),
     *       unit_cons(max_unit), unit_temp(max_unit),
     *       unit_max(max_unit), unit_rev_rat(max_unit),
     *       unit_flow_rat(max_unit),
     *       unit_crit_min(max_unit), unit_crit_max(max_unit),
     *       unit_min_head(max_unit), unit_max_head(max_unit),
     *       unit_min_flow(max_unit), unit_max_flow(max_unit),
     *       unit_delta_pasp(max_unit), unit_delta_pman(max_unit),
     *       unit_flow_prog(max_unit),
     *       unit_start_cost(max_unit),unit_shut_cost(max_unit),
     *       unit_hrate_corr(max_unit),fl_flow_man(max_unit)
     *      ,tg_comp_id(max_unit)
c
      INTEGER*4 lstatus             !-stato delle unita'
      character*3 status

      COMMON /UNITC/ lstatus(max_unit)
     *              ,status(MAX_UNIT)

c***********************************************************************************
c_ecol
c
      real*4 unit_vinc_maxpow    !vincolo operativo sulla potenza massima
     *      ,unit_vinc_minpow    !vincolo operativo sulla potenza minima
     *      ,unit_vinc_maxrev    !vincolo operativo sul massimo numero di giri
     *      ,unit_vinc_maxpow_iso !vincolo operativo sulla potenza massima (valore ISO)
     *      ,unit_vinc_minpow_iso !vincolo operativo sulla potenza minima (valore ISO)

c
c------------------------------------------------------------------------------------
c I vincoli operativi sulla potenza sono assegnati tra i CIN di centrale come 
c percentuali che vanno riferite alla potenza nominale delle singole turbine attive 
c all'interno della centrale analizzata.
c Nella common il valore da memorizzare è quello calcolato
c per ogni turbogruppo a partire da tali dati,cioè per ogni turbogruppo:
c      unit_vinc_maxpow=nom_power*unit_vinc_maxpow(%)/100.
c      unit_vinc_minpow=nom_power*unit_vinc_minpow(%)/100.
c Si deve inoltre verificare che il valore così ottenuto sia in KWatt.
c Per quanto invece riguarda il vincolo operativo sul massimo numero di giri,il dato
c da memorizzare è quello che si ottiene dal numero di giri nominale del compressore 
c in questo modo:
c      unit_vinc_maxrev=unit_vinc_maxrev(%)/100.      
c------------------------------------------------------------------------------------
c
c
	  common /vincoli/ unit_vinc_maxpow(max_unit)
     *                ,unit_vinc_minpow(max_unit)
     *                ,unit_vinc_maxrev(max_unit)
     *                ,unit_vinc_maxpow_iso(max_unit)
     *                ,unit_vinc_minpow_iso(max_unit)
c
c************************************************************************************
c-elena
c   Dati di unità, da visualizzare sui report dello scenario di centrale,ricavabili dai
c   risultati della simulazione
c


      real*4 unit_rev_rpm,
     *       unit_qout,
     *       unit_qpeso,
     *       unit_ptt,
     *       unit_cons_su_qout,
     *       unit_hrate_perc,
     *       unit_eff_turb
c-report_new
     *      ,unit_hrate_onsite
     *      ,unit_power_iso
     *      ,power_onsite
c-report_new-end

	  common /report_unita/ unit_rev_rpm(max_unit),
     *       unit_qout(max_unit),
     *       unit_qpeso(max_unit),
     *       unit_ptt(max_unit),
     *       unit_cons_su_qout(max_unit),
     *       unit_hrate_perc(max_unit),
     *       unit_eff_turb(max_unit)
c-report_new
     *      ,unit_hrate_onsite(max_unit)
     *      ,unit_power_iso(max_unit)
     *      ,power_onsite(max_unit)
c-report_new-end

*****************************************************************************************
c  La nuova implementazione del criterio dell'equidistanza prevede l'inserimemto di due 
c  percentuali per ogni tbg inserito in centrale, che rappresentano lo scostamento che la
c  curva minima del criterio e la curva massima del criterio devono avere rispetto la curva 
c  di surge sperimentale e la curva operating limit.
c*******************************************************************************************

      real*4 unit_perc_min_equi,
     *       unit_perc_max_equi

	  common /perc_crit_equi/ unit_perc_min_equi(max_unit),
     *       unit_perc_max_equi(max_unit)
