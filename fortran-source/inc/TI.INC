!------------------------------------------------------------------------
** TI.INC ** identificatori elementi e loro connessioni **
!------------------------------------------------------------------------
      integer*4
     -  htipto(m_ogg) !tipo elemento
     - ,opumto(m_ogg) !punto monte
     - ,opuvto(m_ogg) !punto valle
     - ,oseeto(m_ogg) !successivo elemento entrante
     - ,oseuto(m_ogg) !successivo elemento uscente
      integer*4
     -  pgrato(m_ogg) !puntatore alle grandezze elemento
     - ,elem_id(m_ogg) ! campo elem_id della tabella ELEMENTI
      common/Oto/htipto,opumto,opuvto,oseeto,oseuto,pgrato
     -          ,elem_id
!----------------------------------------------------------------------
      integer*4 
     -  otipti(top_maxtipi) !puntatori alle zone inizio tipi di elemento
     - ,etipti(top_maxtipi) !puntatori alle zone fine tipi di elemento
      common/inizio_tipi/otipti,etipti
!------------------------------------------------------------------------
      integer*4
     -  off_oi       !alfa:  origine gas
     - ,off_dp       !omega: destinazione gas
     - ,off_re       !rete:  area globale
      common/sire_ele/off_oi,off_dp,off_re
!------------------------------------------------------------------------
