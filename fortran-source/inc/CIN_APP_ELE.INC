*-------------------------------------------------------------------------------
*                          CIN_APP_ELE.INC
*
*  Definizione grandezze condizioni iniziali AGGIUNTIVE per gli elementi
*-------------------------------------------------------------------------------
!===============================================================================
!                 IMPIANTI DI REGOLAZIONE:e_vg
!===============================================================================
      integer*4 line_vg_on     !numero linee attive impianto di regolazione

      common/cin_app_vg/
     -       line_vg_on (max_vg)

