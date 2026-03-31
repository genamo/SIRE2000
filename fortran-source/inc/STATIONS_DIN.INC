
       real*4    p_low, p_high, q_high
       parameter (p_low    = 10.,
     *            p_high   = 200.,
     *            q_high   = 10000.)

       integer*4 MAXSET
       PARAMETER(MAXSET=100)

       integer*4 FLAG_STATO
       integer*4 stato_sta
!       LOGICAL*1 LMESS_21,LMESS_29,lblock
       LOGICAL*2 LMESS_21,LMESS_29,lblock
       COMMON/STAZDIN/FLAG_STATO(MAX_VERT),stato_sta(max_vert),
     *                LMESS_21(MAX_VERT), LMESS_29(MAX_VERT),
     *                lblock(max_vert)
C
       integer*4   stat_ord_set(max_vert)
       character*3 status_set(max_unit)
c
       common/SET_POINT/stat_ord_set, status_set
