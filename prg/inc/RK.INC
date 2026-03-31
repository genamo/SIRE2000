
      INCLUDE 'rk_param.inc'     ! dimensioni e parametri

c coefficienti
      real rk_par(m_ogg,2)
      real av_weight(m_ogg)
      real cp_par(4,m_ogg)
      real vis_par(m_ogg,3)
      real exp_coef(6,m_ogg)
	  real dh_coef(3,m_ogg)
      common /rkcom/ rk_par,av_weight,cp_par,vis_par,exp_coef,
     *               dh_coef
