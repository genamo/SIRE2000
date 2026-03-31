!-------------------------------------------------------------------------------
!                         SCENARIO.INC
!-------------------------------------------------------------------------------
!-Informazioni generali per la simulazione relativi ad uno scenario di rete
!-------------------------------------------------------------------------------
      integer*4 stagione    !1=Primavera,2=Estate,3=Autunno,4=Inverno
      integer*4 interpol    !1=Gradino,2=Lineare,3=Biquadratica
      integer*4 tipo_prel   !1=Medio,2=Istante                               (Stazionario)
      integer*4 fl_nwrec    !0=No recupero,1=Si recupero (Newton sem,Newton) (Stazionario)
      integer*4 fl_tlc      !0=No telecomandi,1=Si telecomandi       (Dinamica)
      integer*4 tipo_tlc    !0=No ciclici,1=Si ciclici (se fl_tlc=1) (Dinamica)
      integer*4 fl_modul    !0=No modulazioni,1=Si modulazioni       (Dinamica)
      integer*4 ultimo_camp !Ultimo campionamento                    (Dinamica)

      real*4 ist_prel       !Istante se tipo_prel=2 (Stazionario)
      real*4 ist_iniz       !Istante iniziale       (Dinamica)
      real*4 ist_fine       !Istante finale         (Dinamica) 
      real*4 passo_camp     !Passo di campionamento (Dinamica)

      real*4 corr_ist       !Istante corrente       (Stazionario,Dinamica)

      integer*4 fl_est      !Flag estensione file   0:NO  1:SI

      common/info_scen/stagione,interpol,tipo_prel,fl_nwrec,fl_tlc
     -      ,tipo_tlc,fl_modul,ultimo_camp,ist_prel,ist_iniz,ist_fine
     -      ,passo_camp,corr_ist,fl_est
!-------------------------------------------------------------------------------
