********* TD.INC *** caratteristiche del gas per ciascun elemento ******
      real*4
     -  zm0td (m_ogg) !coef.comprimibilita` nominale (adimensionale)
     - ,ro0td (m_ogg) !densita` nominale
     - ,ravotd(m_ogg) !rapporto fra costante gas perfetti e peso molecolare
     - ,pclstd(m_ogg) !potere calorifico superiore (kJ/kNm3)
! solo per bilancio entalpico
     - ,pclitd(m_ogg) !potere calorifico inferiore  (kJ/kNm3)
     - ,ctvctd(m_ogg) !capacita` termica specifica a volume cost.(Kcal/Kmole)
     - ,ctpctd(m_ogg) !capacita` termica specifica a pressione cost.(Kcal/Kmole)

      common/rTD/
     -  zm0td,ro0td,ravotd,pclstd,pclitd,ctvctd,ctpctd
!-----------------------------------------------------------------------
! NOTA: per tradurre le capacita` termiche specifiche da Kcal/Kmole a Kcal/Nmc
! occorre moltiplicarle per il volume "VOLMcc" occupato da una Kmole
!------------------------------------------------------------------------
********* TD.INC *** caratteristiche del gas per ciascun elemento ******
