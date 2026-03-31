
	INTEGER*4 lvinc2
        INTEGER*4 nlim2
        INTEGER*4 tipo_surge
        INTEGER*4 tipo_crit
        INTEGER*4 unit_act

        real*4    type2_ratio,clim2,a_coef2,b_coef2,lim_h2,lim_n2,
     *            lim_q2,cen2,cec2,chn2,chc2,eff_corr2,power_corr2,
     *            effmi2
        real*4    type_kcrit_eff
        real*4    type_delpint
        real*4    type_deltint
        real*4    type_maxtint
        real*4    flow_unit ! portata che ciascuna unita' attiva deve lavorare
                            ! per il criterio di uguaglianza delle portate
        logical*2 type_bifase
c_ecol
c --------------->Trattamento per l'air cooler 2
      integer*4 ac2_stat        !stato dell'air_cooler di fase 2
      real*4 ac2_k              !coefficiente caratteristico dell'air cooler
     *      ,ac2_q_prog         !calore scambiato nell'unità di tempo
     *      ,ac2_taria_prog     !temperatura di riferimento dell'aria

cmar_AC_spento	  
         integer*4 ac2_stat_M
		 real*4 ac2_k_M
cmar_AC_spento	
c
	common/MF_SIMPSTAT/ lvinc2(maxunits), nlim2(maxunits),
     *         tipo_surge, tipo_crit, unit_act,
     *         ac2_stat(maxunits),
     *         type2_ratio(maxunits),
     *         clim2(4,maxunits),a_coef2(max_lim,maxunits),
     *	       b_coef2(max_lim,maxunits),lim_h2(max_lim,maxunits),
     *	       lim_n2(max_lim,maxunits),lim_q2(max_lim,maxunits),
     *	       chn2(6,maxunits),chc2(6,maxunits),
     *         cen2(5,maxunits),cec2(6,maxunits),eff_corr2(maxunits),
     *         power_corr2(maxunits),
     *         effmi2,type_kcrit_eff(2,maxunits),
     *         type_delpint(maxunits),type_deltint(maxunits),
     *         type_maxtint(maxunits),
     *         flow_unit,
c_ecol
     *         ac2_k(maxunits),
     *         ac2_q_prog(maxunits),
     *         ac2_taria_prog(maxunits),
c_ecol
     *         type_bifase(maxunits),ac2_stat_M,ac2_k_M(maxunits)

  
c_ecol
cmar_AC_spento	
