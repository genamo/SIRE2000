
      INTEGER*4  type_quant,type_avail,type_act
      INTEGER*4  type_nlim
      REAL*4     min_rev,max_rev,min_eff,nom_rev,t_nom_rev,nom_power,
     *           nom_hrate
      real*4     type_a_coef,type_b_coef,type_c_coef,type_lim_n,
     *           type_lim_q,type_lim_h,type_cen,type_chn,type_chc,
     *           type_cpwm,type_cpwt,type_chr,type_chrt
     *          ,type_cec
      REAL*4     nom_flow, nom_head, nom_tair
c
      CHARACTER*(TIT_LEN) C_NAME, T_NAME, C_COMM, T_COMM
c
      common/TYPES/ type_quant(MAX_TYPE),type_avail(MAX_TYPE),
     *      type_act(MAX_TYPE),type_nlim(max_unit),
     *      max_rev(MAX_unit),min_rev(MAX_unit),
     *      min_eff(max_unit),nom_rev(max_unit),t_nom_rev(max_unit),
     *      nom_power(max_unit),nom_hrate(max_unit),
c-elena     *      type_a_coef(max_lim,max_unit),type_b_coef(max_lim,max_unit),
     *      type_a_coef(max_lim+1,max_unit),
     *      type_b_coef(max_lim+1,max_unit),
     *      type_c_coef(4,max_unit),type_lim_n(max_lim,max_unit),
     *      type_lim_q(max_lim,max_unit),type_lim_h(max_lim,max_unit),
     *      type_cen(5,max_unit),type_chn(6,max_unit),
     *      type_chc(6,max_unit),
     *      type_cpwm(3,max_unit),type_cpwt(4,max_unit),
     *      type_chr(6,max_unit),type_chrt(4,max_unit),
     *      type_cec(6,max_unit),
     *      nom_flow(MAX_TYPE),nom_head(MAX_TYPE),
     *      nom_tair(MAX_TYPE),
     *      C_NAME(MAX_UNIT),T_NAME(MAX_UNIT),
     *      C_COMM(MAX_UNIT),T_COMM(MAX_UNIT)
