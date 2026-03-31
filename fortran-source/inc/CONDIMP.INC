*** condimp.INC *** condizioni imposte************

!      real*4  prestj_imp(m_ogg)      ! pressione da imporre per i punti
!     -       ,porttj_imp(m_ogg)      ! portata agli elementi speciali

      integer*4 pimpo(m_ogg)         ! flag pressione da imporre per i punti
     -         ,qimpo(m_ogg)         ! flag portata agli elementi speciali

!      real*4  TJ0_IMP(M_OGG)
      real*4  TJ0_FL_IMP(M_OGG)

!      common/cond_imp/ TJ0_IMP,TJ0_FL_IMP
      common/cond_imp/ TJ0_FL_IMP

!                       EL. SP.          PU
      EQUIVALENCE
!     -  (TJ0_IMP(1), porttj_imp(1), prestj_imp(1)),
     -  (TJ0_FL_IMP(1), qimpo(1), pimpo(1)) 
