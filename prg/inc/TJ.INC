!-----------------------------------------------------------------------
*** TJ.INC *** condizioni iniziali ************
!-----------------------------------------------------------------------
      real*4
     -  prestj(m_ogg) !pressione                TR,PU
     - ,temptj(m_ogg) !temperatura              TR,PU 
     - ,zmcitj(m_ogg) !coef. di comprimibilita  TR,PU
     - ,rocitj(m_ogg) !densita`                 TR,PU
     - ,porttj(m_ogg) !portata                  tutti
     - ,qsettj(m_ogg) !set portata              elementi speciali
     - ,pmsetj(m_ogg) !set pressione monte      elementi speciali binari
     - ,pvsetj(m_ogg) !set pressione valle      elementi speciali
     - ,tsettj(m_ogg) !set temperatura          elementi speciali
c     - ,dqsetj(m_ogg) !Dq(t)/dt all'attivazione comando  elementi speciali
     - ,tfintj(m_ogg) !tempo finale comando     elementi speciali
     - ,diretj(m_ogg) !direzionalità elemento:  elementi speciali unari
	                  !(entrante = +1  uscente = -1)
c     - ,FL_FLOW_ASS(M_OGG) !CMAR_EVO_CRIT_MAN PERC PER ASSETTO CONDIVISO

!-rocitj(off_re)=massa presente in rete
!-rocitj(off_oi)=massa importata, rocitj(off_dp)=massa esportata

      integer*4
     -  htcitj(m_ogg)    !tipo di regolazione   elementi speciali
     - ,hstatj(m_ogg)    !stato dell'elemento   solo elementi speciali
     - ,n_agg(m_ogg)     !numero aggiustamenti  per tutti

      real*4
     -  TJ0(m_ogg)
     - ,TJ1(m_ogg)
     - ,TJ2(m_ogg)
     - ,TJ3(m_ogg)
     - ,TJ4(m_ogg)
c     - ,TJ5(m_ogg)
     - ,TJ6(m_ogg)
      integer*4
     -  iTJ0(m_ogg)
     - ,iTJ1(m_ogg)
     - ,iTJ2(m_ogg)
     - ,iTJ3(m_ogg)
      common /TJ/
c     -  TJ0,TJ1,TJ2,TJ3,TJ4,TJ5,TJ6,iTJ0,iTJ1,iTj2,iTJ3
     -  TJ0,TJ1,TJ2,TJ3,TJ4,TJ6,iTJ0,iTJ1,iTj2,iTJ3
!======================================================================
      EQUIVALENCE   !parte reale
     -  (TJ0 (1), PREStj(1),   PVSEtj(1)),
     -  (TJ1 (1), TEMPtj(1),   TSETtj(1)),
     -  (TJ2 (1), ROCItj(1),   PMSEtj(1)),
     -  (TJ3 (1), ZMCItj(1),   QSETtj(1)),
     -  (TJ4 (1),      PORTtj(1)),
c     -  (TJ5 (1),              DQSEtj(1)),
     -  (TJ6 (1),              TFINtj(1))
      EQUIVALENCE	!parte intera
     -  (iTJ0 (1),      HSTAtj(1)),
     -  (iTJ1 (1),      HTCItj(1)),
     -  (iTJ2 (1),      N_AGG(1)),
     -  (iTJ3 (1),      DIREtj(1))
!======================================================================
!-OUTPUT
      integer*4 flertj  !flag di bontà di elemento
      common/dati_ele_out/flertj (m_ogg)
!======================================================================
