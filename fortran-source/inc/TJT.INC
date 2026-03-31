*** TJT.INC *** condizioni iniziali da Telemisure ************

      real*4
     -  prestjt(m_ogg) !pressione                 TR,PU
!     - ,temptjt(m_ogg) !temperatura               TR,PU 
!     - ,zmcitjt(m_ogg) !coef. di comprimibilita`  TR,PU
     - ,rocitjt(m_ogg) !densita`                  TR,PU
     - ,porttjt(m_ogg) !portata                   tutti
!     - ,qsettjt(m_ogg) !set portata               solo elementi speciali
!     - ,pmsetjt(m_ogg) !set pressione monte       solo elementi speciali binari
!     - ,pvsetjt(m_ogg) !set pressione valle       solo elementi speciali
!     - ,tsettjt(m_ogg) !set temperatura           solo elementi speciali ??

      real*4
     -  TJT0(m_ogg)
!     - ,TJT1(m_ogg)
     - ,TJT2(m_ogg)
!     - ,TJT3(m_ogg)
     - ,TJT4(m_ogg)
      common /TJT/
     -  TJT0,TJT2,TJT4
!    -  TJT0,TJT1,TJT2,TJT3,TJT4
!======================================================================
!      EQUIVALENCE     	! parte reale
!     -  (TJT0 (1), PREStjt(1),   PVSEtjt(1)),
!     -  (TJT1 (1), TEMPtjt(1),   TSETtjt(1)),
!     -  (TJT2 (1), ROCItjt(1),   PMSEtjt(1)),
!     -  (TJT3 (1), ZMCItjt(1),   QSETtjt(1)),
!     -  (TJT4 (1),      PORTtjt(1))
      EQUIVALENCE     	! parte reale
     -  (TJT0 (1), PREStjt(1)),
!     -  (TJT1 (1), TEMPtjt(1),   TSETtjt(1)),
     -  (TJT2 (1), ROCItjt(1)),
!     -  (TJT3 (1), ZMCItjt(1),   QSETtjt(1)),
     -  (TJT4 (1),      PORTtjt(1))
!-----------------------------------------------------------------------

*** TJT.INC *** condizioni iniziali ************
