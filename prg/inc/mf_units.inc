c
c Integrazione del common units.cmn per i dati della seconda fase
c
c
      REAL*4      unit2_perc,unit2_rev,unit2_eff_corr,unit2_eff,
     *            unit2_temp,unit2_flow,unit2_head,unit2_max,
     *            unit2_rev_rat,unit2_power_corr,unit2_power,
     *            unit2_min
c
      common/MF_UNITS/unit2_perc(max_unit),unit2_eff_corr(max_unit),
     *			unit2_power_corr(max_unit),
     *			unit2_rev(max_unit),unit2_flow(max_unit),
     *			unit2_head(max_unit),unit2_eff(max_unit),
     *			unit2_temp(max_unit),unit2_power(max_unit),
     *                  unit2_max(max_unit),unit2_rev_rat(max_unit),
     *                  unit2_min(max_unit)

      integer*4 unit_vinc
      REAL*4    unit_min
      REAL*4    unit_vcrit
      real*4    unit_pres_int, unit_temp_int, unit_delprint,
     *          unit_deltrint, unit_maxtint,unit_delpr_ac
      logical*2 unit_bifase
      character*(TIT_LEN) UNIT_COMM

      common/app_units/ unit_vinc(max_unit),
     *       unit_min(max_unit), unit_vcrit(max_unit),
     *       unit_pres_int(max_unit), unit_temp_int(max_unit),
     *       unit_delprint(max_unit), unit_deltrint(max_unit),
     *       unit_maxtint(max_unit),unit_delpr_ac(max_unit),
     *       unit_bifase(max_unit),
     *       UNIT_COMM(max_unit)
c
c************************************************************************************
c-elena
c   Dati di unità(fase2), da visualizzare sui report dello scenario di centrale,ricavabili dai
c   risultati della simulazione
c
      real*4 unit2_rev_rpm,
     *       unit2_qout,
     *       unit2_qpeso,
     *       unit2_ptt,
     *       unit2_cons_su_qout
c-report_new
     *      ,unit2_power_iso
c-report_new-end



	  common /report_unita_2f/ unit2_rev_rpm(max_unit),
     *       unit2_qout(max_unit),
     *       unit2_qpeso(max_unit),
     *       unit2_ptt(max_unit),
     *       unit2_cons_su_qout(max_unit)
c-report_new
     *      ,unit2_power_iso(max_unit)
c-report_new-end
*****************************************************************************************
