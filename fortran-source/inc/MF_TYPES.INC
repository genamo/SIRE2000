c
c Integrazione del common types.cmn per i dati della seconda fase
c
	INTEGER*4  type2_nlim

	REAL*4     min2_rev,max2_rev,min2_eff,nom2_rev
        real*4     type2_a_coef,type2_b_coef,type2_c_coef,type2_lim_n,
     *             type2_lim_q,type2_lim_h,type2_cen,type2_chn,
     *             type2_chc,type2_cec
        REAL*4     nom2_flow, nom2_head

        CHARACTER*(TIT_LEN) C2_NAME, C2_COMM
c
	common/MF_TYPES/ type2_nlim(max_unit),
     *      max2_rev(MAX_unit), min2_rev(MAX_unit),
c-elena     *      type2_a_coef(max_lim,max_unit),
c-elena     *      type2_b_coef(max_lim,max_unit),
     *      type2_a_coef(max_lim+1,max_unit),
     *      type2_b_coef(max_lim+1,max_unit),
     *      type2_c_coef(4,max_unit), type2_lim_n(max_lim,max_unit),
     *      type2_lim_q(max_lim,max_unit),
     *      type2_lim_h(max_lim,max_unit),
     *      type2_cen(5,max_unit), type2_chn(6,max_unit),
     *      type2_chc(6,max_unit),type2_cec(6,max_unit),
     *      min2_eff(max_unit), nom2_rev(max_unit),
     *      nom2_flow(MAX_TYPE), nom2_head(MAX_TYPE),
     *      C2_NAME(MAX_UNIT), C2_COMM(MAX_UNIT)

