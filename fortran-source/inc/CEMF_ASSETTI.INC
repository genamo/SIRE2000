
*----------------------------------------------------------------------
c   Informazioni relative agli assetti definiti per le centrali MF
c   (tutte le variabili legate agli assetti della centrale sono
c   dimensionate a MAX_NUM_ASS = MAX_VERT_CMF*MAX_NUM_ASSETTI)
*----------------------------------------------------------------------
        integer*4 stmo_cemf             ! codice stmo assetto
        integer*4 p_coll_cen_as_cemf    ! punt.collet.centrale asp.
        integer*4 p_coll_cen_ma_cemf    ! punt.collet.centrale mandata
        integer*4 i_coll_est_as_cemf    ! id.collet.esterno(nodo/unario) asp.
        integer*4 i_coll_est_ma_cemf    ! id.collet.esterno(N/U) mandata
        integer*4 i_coll_est_ma2_cemf   ! id.altro collet.esterno mandata
c
        real*4    max_pout_cemf         ! massima pressione coll. cent. mand
        real*4    max_tout_cemf         ! massima temperatura coll. cent. mand
        real*4    delpin_cemf           ! delta pressione coll. cent. asp
        real*4    delpout_cemf          ! delta pressione coll. cent. mand
        real*4    dtout_cemf            ! massima temperatura coll. cent. mand
c
        logical*2 abi_unita_cemf        ! abilitazione unita' (T/F)
        logical*2 compa_ass_cemf        ! compatibilita' assetti (T/F)
c
        character*(tit_len) sigla_cemf  ! sigla assetto
        character*10 coll_cen_as_cemf   ! collettore centrale aspirazione
        character*1  flag_est_as_cemf   ! flag col.est.asp.(N=nodo/U=unario)
        character*10 coll_cen_ma_cemf   ! collettore centrale mandata
        character*1  flag_est_ma_cemf   ! flag col.est.man.(N=nodo/U=unario)
c
cgpe MAX_NUM_ASSETTI e' diventato MAX_SERV, MAX_NUM_ASS = MAX_NUM_ASSETTI*MAX_VERT_CMF
cgpe ma temporaneamente sostituisco MAX_NUM_ASS con MAX_SERV
cgpe-prv ripristino MAX_NUM_ASS
        common/cemf_ass/ 
     *         stmo_cemf(MAX_NUM_ASS),
     *         p_coll_cen_as_cemf(MAX_NUM_ASS),
     *         p_coll_cen_ma_cemf(MAX_NUM_ASS),
     *         i_coll_est_as_cemf(MAX_NUM_ASS),
     *         i_coll_est_ma_cemf(MAX_NUM_ASS),
     *         i_coll_est_ma2_cemf(MAX_NUM_ASS),
     *         max_pout_cemf(MAX_NUM_ASS),
     *         max_tout_cemf(MAX_NUM_ASS),
     *         delpin_cemf(MAX_NUM_ASS),
     *         delpout_cemf(MAX_NUM_ASS),
     *         dtout_cemf(MAX_NUM_ASS),
     *         abi_unita_cemf(MAX_NUM_ASS,MAXUNITS),
     *         compa_ass_cemf(MAX_NUM_ASS,MAX_NUM_ASSETTI),
     *         sigla_cemf(MAX_NUM_ASS),
     *         coll_cen_as_cemf(MAX_NUM_ASS),
     *         flag_est_as_cemf(MAX_NUM_ASS),
     *         coll_cen_ma_cemf(MAX_NUM_ASS),
     *         flag_est_ma_cemf(MAX_NUM_ASS)
