!------------------------------------------------------------------------
!*** TQ.INC *** coefficienti dei polinomi interpolanti i prelievi
!------------------------------------------------------------------------
      real*4
     -  qsp1tq(max_modul,max_pu)
     - ,qsp2tq(max_modul,max_pu)
     - ,qsp3tq(max_modul,max_pu)

      common/Rtq/qsp1tq,qsp2tq,qsp3tq
!------------------------------------------------------------------------
!*qsp1tq = coef.termine di grado 0 (noto)
!- pr_step -> portata nel periodo attuale
!- pr_line -> primo coef.retta
!- pr_spli -> primo coef.spline

!*qsp2tq = coef.termine di grado 1 (lineare)
!- pr_step -> non usato
!- pr_line -> secondo coef.retta
!- pr_spli -> secondo coef.spline

!*qsp3tq = coef.termine di grado 2 (quadratico)
!- pr_step -> non usato
!- pr_line -> non usato
!- pr_spli -> terzo coef.spline
!------------------------------------------------------------------------
