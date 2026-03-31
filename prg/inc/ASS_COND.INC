

       integer*4 d, ss_(2),as1,as2
       INTEGER*4 EE(240),CC(240),DD(240)
       logical*2 flag_ass_cond(M_OGG),flag_ass_cond_s,
     *     FLAG_ASS_COND_P(M_OGG),flag_dati_s(m_ogg),flag_dati,
     *     FLAG_S,SIM(M_OGG),SIM_S,no_ACC(M_OGG),a_err(m_ogg),
     *     ass1t(m_ogg),ass2t(m_ogg)
     
       INTEGER*4 FIRST_UNIT_(m_ogg)
       real*4 TOT_NOM(M_OGG)
       
       CHARACTER*3 status_as(m_ogg)
   
       common/aa/d, flag_ass_cond,FLAG_ASS_COND_P,flag_ass_cond_s,
     *  ss_,flag_dati_s,flag_dati,as1,as2,FLAG_S,SIM,SIM_S,no_ACC,
     *  status_as,a_err,ass1t,ass2t,TOT_NOM
       
       	INTEGER*4 type_quant_mf_as(maxunits,max_serv),
     *         type_avail_mf_as(maxunits,max_serv),
     *          unit_avail_mf_as(maxunits,max_serv),
     *          type_num_as(maxunits*max_serv),
! *          unit_avail_as(maxunits*max_serv),
     *           unit_avail_as(250),
     *          type_quant_as(max_type),
     *          unit_avail_as_o(250)

	 INTEGER*4 UNIT_1,UNIT_2
	 INTEGER*4 NTP_1, NTP_2
	 INTEGER*4 UNIT_1_(m_ogg),UNIT_2_(m_ogg)
	 INTEGER*4 NTP_1_(m_ogg), NTP_2_(m_ogg)
	 REAL*4 temp2_1,temp2_2
	 REAL*4 H1, H2 , FL_FLOW_ASS(M_OGG)  
	 real*4  stat_vars_2(10)
	 logical*2 crit_man_(m_ogg)
  

	         

	
     
    
        COMMON/AS/type_quant_mf_as,type_avail_mf_as,unit_avail_mf_as,
     * type_num_as,unit_avail_as,type_quant_as,EE,CC,DD,UNIT_1,
     * NTP_1, NTP_2,UNIT_2,FIRST_UNIT_,stat_vars_2,unit_avail_as_o,
     * NTP_1_, NTP_2_,UNIT_1_,UNIT_2_
     
     
        real*4 aux3_type(maxunits),head_type(maxunits),COMP_RATIO_2,
     * DP_F_1,DP_F_2,DP_AC_1,DP_AC_2,flowm_1,flowm_2,flow_ric_1,
     * flow_ric_2,dp_out_1,dp_out_2,dp_in_1,dp_in_2,
     * tot_cons1,tot_cons2, f1, f2, flow1, flow2,
c     * stvr3_1,stvr4_1,stvr3_2,stvr4_2
     * ac_ce_stat_1,ac_ce_k_1,ac_q_prog_1,ac_taria_prog_1,
     * ac_ce_stat_2,ac_ce_k_2,ac_q_prog_2,ac_taria_prog_2
        
	
	    common/stat_mf_as/aux3_type,head_type,COMP_RATIO_2,flowm_1,
     * flowm_2,flow_ric_1,flow_ric_2,dp_out_1,dp_out_2,dp_in_1,dp_in_2,
     * tot_cons1,tot_cons2, f1, f2,H1,H2
c     * stvr3_1,stvr4_1,stvr3_2,stvr4_2
  	 
	    COMMON/DP/DP_F_1,DP_F_2,DP_AC_1,DP_AC_2,temp2_1,temp2_2
c	 * ac_ce_stat_1,ac_ce_k_1,ac_q_prog_1,ac_taria_prog_1,
c     * ac_ce_stat_2,ac_ce_k_2,ac_q_prog_2,ac_taria_prog_2,   
	  
	   
	    
	    COMMON/AC/ac_ce_stat_1,ac_ce_k_1,ac_q_prog_1,ac_taria_prog_1,
     * ac_ce_stat_2,ac_ce_k_2,ac_q_prog_2,ac_taria_prog_2
     
        COMMON/crman/crit_man_,FL_FLOW_ASS
	  
     
       
       