!--------------------------------------------------------------------
! *** FILES.INC *** parametri per i File di lettura e scrittura ***
!--------------------------------------------------------------------
!-File INPUT per SIMCENT e RETE 
!--------------------------------------------------------------------
      character*(*) 
     -        rf_udm
     -       ,rf_centrali
     -       ,rf_assetti
     -       ,rf_turbogruppi
     -       ,rf_punti_lim
      parameter (
     -        rf_udm         = 'udm_in.txt'
     -       ,rf_centrali    = 'centrali_in.txt'
     -       ,rf_assetti     = 'assetti_in.txt'
     -       ,rf_turbogruppi = 'turbogruppi_in.txt'
     -       ,rf_punti_lim   = 'punti_lim_in.txt'
     -)
!--------------------------------------------------------------------
!-File INPUT per RETE
!--------------------------------------------------------------------
      character*(*) 
     -        rf_infoscen
     -       ,rf_punti
     -       ,rf_tronchi
     -       ,rf_regolazioni
     -       ,rf_riduzioni
     -       ,rf_centrali_s
     -       ,rf_valvole
     -       ,rf_unari
     -       ,rf_stacco
      parameter (
     -        rf_infoscen    = 'infoscen.txt'
     -       ,rf_punti       = 'punti_in.txt'
     -       ,rf_tronchi     = 'tronchi_in.txt'
     -       ,rf_regolazioni = 'regolazioni_in.txt'
     -       ,rf_riduzioni   = 'riduzioni_in.txt'
     -       ,rf_centrali_s  = 'centrali_s_in.txt'
     -       ,rf_valvole     = 'valvole_in.txt'
     -       ,rf_unari       = 'unari_in.txt'
     -       ,rf_stacco      = 'stacco_in.txt'
     -)
!--------------------------------------------------------------------
!-File INPUT per Ripartenza Dinamico RETE e Telecomandi RETE
!--------------------------------------------------------------------
      character*(*) 
     -        df_centrali
     -       ,df_assetti
     -       ,df_turbogruppi
     -       ,df_punti
     -       ,df_tronchi
     -       ,df_regolazioni
     -       ,df_riduzioni
     -       ,df_centrali_s
     -       ,df_valvole
     -       ,df_unari
     -       ,df_stacco
     -       ,df_telecomandi
     -       ,df_interrupt
      parameter (
     -        df_centrali    = 'centrali_din.txt'
     -       ,df_assetti     = 'assetti_din.txt'
     -       ,df_turbogruppi = 'turbogruppi_din.txt'
     -       ,df_punti       = 'punti_din.txt'
     -       ,df_tronchi     = 'tronchi_din.txt'
     -       ,df_regolazioni = 'regolazioni_din.txt'
     -       ,df_riduzioni   = 'riduzioni_din.txt'
     -       ,df_centrali_s  = 'centrali_s_din.txt'
     -       ,df_valvole     = 'valvole_din.txt'
     -       ,df_unari       = 'unari_din.txt'
     -       ,df_stacco      = 'stacco_din.txt'
     -       ,df_telecomandi = 'telecomandi.txt'
     -       ,df_interrupt   = 'interrupt.txt'
     -)
!--------------------------------------------------------------------
!-File INPUT Parametri di Simulazione
!--------------------------------------------------------------------
      character*(*) 
     -        rf_parasim
      parameter (
     -        rf_parasim    = 'parasim.config'
     -)
!--------------------------------------------------------------------
!-File OUTPUT per SIMCENT e RETE
!--------------------------------------------------------------------
      character*(*) 
     -        wf_centrali
     -       ,wf_assetti
     -       ,wf_turbogruppi
      parameter (
     -        wf_centrali    = 'centrali_out.txt'
     -       ,wf_assetti     = 'assetti_out.txt'
     -       ,wf_turbogruppi = 'turbogruppi_out.txt'
     -)
!--------------------------------------------------------------------
!-File OUTPUT per RETE
!--------------------------------------------------------------------
      character*(*) 
     -        wf_punti
     -       ,wf_tronchi
     -       ,wf_regolazioni
     -       ,wf_riduzioni
     -       ,wf_centrali_s
     -       ,wf_valvole
     -       ,wf_unari
     -       ,wf_stacco
      parameter (
     -        wf_punti       = 'punti_out.txt'
     -       ,wf_tronchi     = 'tronchi_out.txt'
     -       ,wf_regolazioni = 'regolazioni_out.txt'
     -       ,wf_riduzioni   = 'riduzioni_out.txt'
     -       ,wf_centrali_s  = 'centrali_s_out.txt'
     -       ,wf_valvole     = 'valvole_out.txt'
     -       ,wf_unari       = 'unari_out.txt'
     -       ,wf_stacco      = 'stacco_out.txt'
     -)
!--------------------------------------------------------------------
!-Unità File Lettura e Scrittura
!--------------------------------------------------------------------
!-Lettura: Simcent, Rete, Dinamico
      integer*4
     -        urf_file
     -       ,udf_file
     -       ,udf_tlc
      parameter (
     -        urf_file = 51
     -       ,udf_file = 52
     -       ,udf_tlc  = 53
     -)

!-Scrittura: Simcent, Rete
      integer*4
     -        uwf_file
     -       ,uwf_centrali
     -       ,uwf_assetti
     -       ,uwf_turbogruppi
      parameter (
     -        uwf_file        = 61
     -       ,uwf_centrali    = 62
     -       ,uwf_assetti     = 63
     -       ,uwf_turbogruppi = 64
     -)
!--------------------------------------------------------------------
!-Path Cartelle File
!--------------------------------------------------------------------
      character*1024 c_path_dirdati,
     -               c_path_dirlog,c_path_dirute
      common/exe_param/c_path_dirdati,
     -                 c_path_dirlog,c_path_dirute
!--------------------------------------------------------------------
