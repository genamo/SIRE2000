*-------------------------------------------------------------------------------
*                         TOP_NSM.INC
*
*       Definizione area dati topologici per il NODO COMPLETO
*-------------------------------------------------------------------------------
!===============================================================================
!                       NODO COMPLETO:  e_nsm
!===============================================================================
!-numero effettivo nodi di smistamento
      integer*4 num_nsm           !n.ro nodi presenti in rete (nsm)
*-------------------------------------------------------------------------------
!-dati componenti per ogni nodo:
*-------------------------------------------------------------------------------
!-numero elementi componenti globali
      integer*4 num_cl_nsm        !n.ro collettori            (cl)
      integer*4 num_lg_nsm        !n.ro linee grigliato       (lg)   
      integer*4 num_rt_nsm        !n.ro linee su anchor point (rt)

!-numero elementi su linee grigliato (lg)
      integer*4 num_vr_lg_nsm     !n.ro riduttori    su lg/rt
      integer*4 num_vs_lg_nsm     !n.ro valvole sez. su lg/rt
      integer*4 num_pz_nsm        !n.ro pozzi        su lg
      integer*4 num_st_nsm        !n.ro stoccaggi    su lg
      integer*4 num_rg_nsm        !n.ro regolatori   su lg

!-numero elementi tra collettori (cl)
      integer*4 num_vr_cl_nsm     !n.ro riduttori    tra cl
      integer*4 num_vs_cl_nsm     !n.ro valvole sez. tra cl
      integer*4 num_ce_nsm        !n.ro centrali     tra cl

!-identificatori collettori (cl)                    !20
      integer*4 id_cl_nsm         !ident. collettore (cl)

!-connessioni linee esterne con anchor point (rt)   !20
      integer*4 id_prete_nsm      !ident. punto rete (rt)

!-connessioni linee grigliato (lg) ai punti di rete !20
      integer*4 p_id_prete_nsm    !puntatore a punto rete

!-connessioni vr riduttori con linea (lg)    !5
      integer*4 p_vr_lg_nsm       !puntatore a linea lg /a punto rt
      integer*4 tipo_vr_lg_nsm    !0/1 = su linea/punto

! connessioni vs valvole sez. con linea (lg) !5
      integer*4 p_vs_lg_nsm       !puntatore a linea lg /a punto rt
      integer*4 tipo_vs_lg_nsm    !0/1 = su linea/punto

! connessioni pz pozzi con linea (lg)        !5
      integer*4 p_pz_lg_nsm       !puntatore a linea lg

! connessioni st stoccaggi con linea (lg)    !5
      integer*4 p_st_lg_nsm       !puntatore a linea lg

! connessioni rg regolatori con linea (lg)   !5
      integer*4 p_rg_lg_nsm       !puntatore a linea lg

! connessioni vr riduttori tra collettori (cl)    !1
      integer*4 p_vr_clm_nsm      !puntatore cl monte
      integer*4 p_vr_clv_nsm      !puntatore cl valle

! connessioni vs valvole sez. tra collettori (cl) !5
      integer*4 p_vs_clm_nsm      !puntatore cl monte
      integer*4 p_vs_clv_nsm      !puntatore cl valle

! connessioni ce/cm centrali tra collettori (cl)  !1
      integer*4 p_ce_clm_nsm      !puntatore cl monte
      integer*4 p_ce_clv_nsm      !puntatore cl valle

!-flag argo (0/1 -> trattamento rete/argo)
      integer*4 flag_argo         !flag argo
!-ritardo
      real*4 rit_nsm              !45 ritardo
!-codice
      integer*4 cod_nsm           !codice nsm
!-descrizione
      character*(tit_len) des_nsm !descrizione
*-------------------------------------------------------------------------------
!===============================================================================
!                        COMMON NODO COMPLETO
!===============================================================================
      common /dati_nsm_com/
     -   num_nsm
     -  ,flag_argo
     -  ,num_cl_nsm   (max_nsm)
     -  ,num_lg_nsm   (max_nsm)
     -  ,num_rt_nsm   (max_nsm)
     -  ,num_vr_lg_nsm(max_nsm)
     -  ,num_vs_lg_nsm(max_nsm)
     -  ,num_pz_nsm   (max_nsm)
     -  ,num_st_nsm   (max_nsm)
     -  ,num_rg_nsm   (max_nsm)
     -  ,num_vr_cl_nsm(max_nsm)
     -  ,num_vs_cl_nsm(max_nsm)
     -  ,num_ce_nsm   (max_nsm)
     -  ,id_cl_nsm     (max_nsm,max_cl_nsm)
     -  ,id_prete_nsm  (max_nsm,max_rt_nsm)
     -  ,p_id_prete_nsm(max_nsm,max_lg_nsm)
     -  ,p_vr_lg_nsm   (max_nsm,max_vr_lg_nsm)
     -  ,tipo_vr_lg_nsm(max_nsm,max_vr_lg_nsm)
     -  ,p_vs_lg_nsm   (max_nsm,max_vs_lg_nsm)
     -  ,tipo_vs_lg_nsm(max_nsm,max_vs_lg_nsm)
     -  ,p_pz_lg_nsm   (max_nsm,max_pz_nsm)
     -  ,p_st_lg_nsm   (max_nsm,max_st_nsm)
     -  ,p_rg_lg_nsm   (max_nsm,max_rg_nsm)
     -  ,p_vr_clm_nsm  (max_nsm,max_vr_cl_nsm)
     -  ,p_vr_clv_nsm  (max_nsm,max_vr_cl_nsm)
     -  ,p_vs_clm_nsm  (max_nsm,max_vs_cl_nsm)
     -  ,p_vs_clv_nsm  (max_nsm,max_vs_cl_nsm)
     -  ,p_ce_clm_nsm  (max_nsm,max_ce_nsm)
     -  ,p_ce_clv_nsm  (max_nsm,max_ce_nsm)
     -  ,rit_nsm (max_nsm)
     -  ,cod_nsm (max_nsm)
     -  ,des_nsm (max_nsm)
*-------------------------------------------------------------------------------
!===============================================================================
!                   COMMON ELEMENTI NODO COMPLETO
!===============================================================================
! Pozzo:  e_pz

      real*4 qmax_pz_nsm              !30 portata max 
      real*4 rit_pz_nsm               !45 ritardo
      integer*4 cod_pz_nsm            !00 codice
      character*(tit_len) des_pz_nsm  !00 descrizione

      common /dati_nsm_pz/
     -      qmax_pz_nsm(max_nsm,max_pz_nsm)
     -     ,rit_pz_nsm (max_nsm,max_pz_nsm)
     -     ,cod_pz_nsm (max_nsm,max_pz_nsm)
     -     ,des_pz_nsm (max_nsm,max_pz_nsm)
*-------------------------------------------------------------------------------
! Stoccaggio:  e_st

      real*4 qmax_st_nsm              !30 portata max 
      real*4 rit_st_nsm               !45 ritardo
      integer*4 cod_st_nsm            !00 codice
      character*(tit_len) des_st_nsm  !00 descrizione

      common /dati_nsm_st/
     -      qmax_st_nsm(max_nsm,max_st_nsm)
     -     ,rit_st_nsm (max_nsm,max_st_nsm)
     -     ,cod_st_nsm (max_nsm,max_st_nsm)
     -     ,des_st_nsm (max_nsm,max_st_nsm)
*-------------------------------------------------------------------------------
! Regolatore:  e_rg

      real*4 qmax_rg_nsm              !30 portata max 
      real*4 rit_rg_nsm               !45 ritardo
      integer*4 cod_rg_nsm            !00 codice
      character*(tit_len) des_rg_nsm  !00 descrizione

      common /dati_nsm_rg/
     -      qmax_rg_nsm(max_nsm,max_rg_nsm)
     -     ,rit_rg_nsm (max_nsm,max_rg_nsm)
     -     ,cod_rg_nsm (max_nsm,max_rg_nsm)
     -     ,des_rg_nsm (max_nsm,max_rg_nsm)
*-------------------------------------------------------------------------------
! Centrali:  e_ce/e_cm

      integer*4 pgrato_ce_nsm         !puntatore elemento area common (ce)
!#ifdef NOINT_0131   !sm
!#else
      integer*4 prec_ce_nsm           !puntatore record nel file CE.TOP 
!#endif
      real*4 rit_ce_nsm               !45 ritardo
      integer*4 cod_ce_nsm            !00 codice
      character*(tit_len) des_ce_nsm  !00 descrizione

      common /dati_nsm_ce/
     -      pgrato_ce_nsm(max_nsm,max_ce_nsm)
!#ifdef NOINT_0131   !sm
!#else
     -     ,prec_ce_nsm  (max_nsm,max_ce_nsm)
!#endif
     -     ,rit_ce_nsm   (max_nsm,max_ce_nsm)
     -     ,cod_ce_nsm   (max_nsm,max_ce_nsm)
     -     ,des_ce_nsm   (max_nsm,max_ce_nsm)
*-------------------------------------------------------------------------------
!===============================================================================
