!------------------------------------------------------------------------
** TX.INC ** valori allarmi degli elementi
!------------------------------------------------------------------------

!------------------------------------------------------------------------
      real*4
     - v_all(m_ogg),         ! velocità allarme tronco   (m/s) TR
     - pres_min_all(m_ogg),   ! pressione allarmi minima  (bar) Pto
     - pres_max_all(m_ogg),  ! pressione allarmi massima (bar) Pto
     - port_min_all(m_ogg),  ! portata allarmi minima 
     - port_max_all(m_ogg)   ! portata allarmi massima
      logical*2 
     - fl_cmf_ass (m_ogg)   !flag se cle ass cond .AND. esiste allarme 
       integer*4
     - fine_tr, fine_pti
      common/ALL/
     -  v_all, pres_min_all, pres_max_all,port_min_all,
     -  port_max_all, fine_tr, fine_pti,fl_cmf_ass
!-----------------------------------------------------------------------
   