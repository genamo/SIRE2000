!-----------------------------------------------------------------------
!*** PARAM.INC *** parametri di uso generale ***
!-----------------------------------------------------------------------
      include 'PARAM_VAL.INC' !ELEMENTI TOPOLOGICI DA DATA BASE
      include 'FILES.INC'     !nomi files
!-----------------------------------------------------------------------
! numero di Bytes:
      integer*4 tit_len,data_len
      parameter(
     -         tit_len  = 30 !n.ro bytes titoli                    [DB]
     -        ,data_len = 19 !n.ro bytes data  GG/MM/AAAA HH:MM:SS [DB]
     -)
!-----------------------------------------------------------------------
!      INDEFINITO-INFINITO
!-----------------------------------------------------------------------
      integer*4  I_UND,I_MAX
      parameter (I_UND  =  -2**30   !INDEFINITO INTEGER*4
     -          ,I_MAX  =  -I_UND-1)
      integer*4  G_UND
      parameter (G_UND  = 0) 		!INDEFINITO ENUMERAZIONE
      real*4     R_UND,R_MAX
      parameter (R_UND  = -1.7e38   !INDEFINITO REAL*4
     -          ,R_MAX  = -R_UND)
!-----------------------------------------------------------------------
!***   TOPOLOGIA   ***
!-----------------------------------------------------------------------
! Mnemonici tipi di elementi topologici
! L'ordinamento di tutti i parametri non puo` essere modificato!!!
!-----------------------------------------------------------------------
      integer*4  E_TR   !Tronco
     -          ,E_VL   !Valvola semplificata
     -          ,E_CE   !Centrale binaria

     -          ,E_IM   !Importazione
     -          ,E_PZ   !Pozzo
     -          ,E_ST   !Stoccaggio
     -          ,E_RG   !Regolatore esterno
     -          ,E_PR   !Prelievo interrompibile

     -          ,E_PU   !Punto di calcolo (utenza)

     -          ,E_NSM  !Nodo di smistamento
     -          ,E_OI   !Origine alfa       1-virtuale
     -          ,E_DP   !Destinazione omega 2-virtuale
     -          ,E_RE   !Rete per bilanci   3-virtuale

     -          ,E_VS   !Valvola di sezionamento (semplificata)
     -          ,E_VR   !Valvola di riduzione (semplificata)
     -          ,E_CM   !Centrale Multifunzione
     -          ,E_VD   !Impianto di riduzione
     -          ,E_VG   !Impianto di regolazione
     -          ,E_CS   !Centrale semplificata
     -          ,E_VC   !Valvola di collegamento

     -          ,E_VT   !Valvola di stacco
     -          ,E_UT   !Unario di stacco

      parameter (E_TR = 1
     -          ,E_VL = 2
     -          ,E_CE = 3
     -          ,E_IM = 4
     -          ,E_PZ = 5
     -          ,E_ST = 6
     -         	,E_RG = 7
     -          ,E_PR = 8
     -          ,E_PU = 9
     -          ,E_NSM= 10
     -          ,E_OI = 11
     -          ,E_DP = 12
     -          ,E_RE = 13
c
     -          ,E_VS = 14
     -          ,E_VR = 15
     -          ,E_CM = 16
     -          ,E_VD = 18
     -          ,E_VG = 19
     -          ,E_CS = 20
     -          ,E_VC = 21
     -          ,E_VT = 22
     -          ,E_UT = 23
     -)
!----------------------------------------------------------
      integer*4  top_maxtipi
      parameter (top_maxtipi = 60) !nro. tipi di elementi

! tipologia dei tipi
      integer*4
     -           TE_BIN(top_maxtipi)  !Vettore tipi binari
     -          ,TE_UNI(top_maxtipi)  !Vettore tipi unari
     -          ,TE_SPE(top_maxtipi)  !Vettore tipi speciali
     -          ,TE_GEN(top_maxtipi)  !Vettore tipi
     -          ,TE_VLV(top_maxtipi)  !Vettore tipi valvola
     -          ,TE_CEN(top_maxtipi)  !Vettore tipi centrale (nella rete)
c
      integer*4
     -           NE_BIN  !N.ro elementi nel vettore tipi binari
     -          ,NE_UNI  !N.ro elementi nel vettore tipi unari
     -          ,NE_SPE  !N.ro elementi nel vettore tipi speciali
     -          ,NE_GEN  !N.ro elementi nel vettore tipi
     -          ,NE_VLV  !N.ro elementi nel vettore tipi valvola
     -          ,NE_CEN  !N.ro elementi nel vettore tipi centrale 
c
      common/v_te/te_bin,te_uni,te_spe,te_gen,te_vlv,te_cen

      character*35 te_stri(top_maxtipi) 
      common/te_stri/te_stri
!----------------------------------------------------------
      parameter (ne_bin = 10)

      parameter (ne_uni = 6)

      parameter (ne_spe = 15)

      parameter (ne_gen = 17)

      parameter (ne_vlv = 7)

      parameter (ne_cen = 2)

!-----------------------------------------------------------------------
c* Dimensionamento dei numeri massimi di elementi topologici

      integer*4   max_tr  !Tronco
     -           ,max_vl  !Valvola semplificata
     -           ,max_ce  !Centrale binaria
     -           ,max_im  !Importazione
     -           ,max_pz  !Pozzo
     -           ,max_st  !Stoccaggio
     -           ,max_rg  !Regolatore esterno
     -           ,max_pr  !Prelievo interrompibile
     -           ,max_pu  !Punto
     -           ,max_nsm !Nodo smistamento
c
     -           ,max_vs  !Valvola di sezionamento (semplificata)
     -           ,max_vr  !Valvola di regolazione (semplificata)
     -           ,max_vd  !Impianto di riduzione
     -           ,max_vg  !Impianto di regolazione
     -           ,max_cs  !Centrale semplificata
     -           ,max_vc  !Valvola di collegamento
     -           ,max_vt  !Valvola di stacco
     -           ,max_ut  !Unario di stacco
c
      parameter ( max_tr = max_tr_db
     -           ,max_vl = max_vl_db
     -           ,max_ce = max_ce_db
     -           ,max_im = max_im_db
     -           ,max_pz = max_pz_db
     -           ,max_st = max_st_db
     -           ,max_rg = max_rg_db
     -           ,max_pr = max_pr_db
     -           ,max_pu = max_pu_db    
     -           ,max_nsm= max_nsm_db
c
     -           ,max_vs = max_vs_db
     -           ,max_vr = max_vr_db
     -           ,max_vd = max_vd_db
     -           ,max_vg = max_vg_db
     -           ,max_cs = max_cs_db
     -           ,max_vc = max_vc_db    !max_vc_nsm*max_nsm
     -           ,max_vt = max_vt_db
     -           ,max_ut = max_ut_db
     -)
c
      integer*4   max_ev  !elementi virtuali (alfa,omega,rete)
      parameter ( max_ev = 3)
c
      integer*4  max_vert
     -          ,maxunits
     -          ,max_unit
      parameter (max_vert = max_ce + max_ces_db !max_ce_nsm*max_nsm_db
     -          ,maxunits = 12
     -          ,max_unit = maxunits*max_vert
     -)

c*  Massimo numero punti da assegnare per la regressione
      integer*4 max_point
	  parameter (max_point=50)

c*  Dimensionamento del numero massimo assetti definiti per le centrali MF
      integer*4  max_num_assetti !max numero assetti per una CE MF
     -          ,max_num_ass     !max numero assetti nella rete
      parameter (max_num_assetti = 2
     -          ,max_num_ass = max_ce*max_num_assetti
     -)

      integer*4  max_serv        !max numero servizi-assetti per una CE MF
      parameter (max_serv = 2)

      integer*4  max_stat_vars   !max numero dati memorizzati nel vettore stat_vars
      parameter (max_stat_vars = 10)
!-----------------------------------------------------------------------
      integer*4  M_OGG  !dimensionamento massimo della topologia
                        !per le strutture dati delle simulazioni
      parameter (M_OGG = max_tr
     -                 + max_vl
     -                 + max_ce
     -                 + max_im
     -                 + max_pz
     -                 + max_st
     -                 + max_rg
     -                 + max_pr
     -                 + max_pu

     -                 + max_vs
     -                 + max_vr
     -                 + max_vd
     -                 + max_vg
     -                 + max_cs
     -                 + max_vc
     -                 + max_vt
     -                 + max_ut

     -                 + max_ev  !elementi virtuali
     -)
!-----------------------------------------------------------------------
!	COMPOSIZIONI GAS
!-----------------------------------------------------------------------
        integer*4  M_COMP      !max componenti del gas
        parameter (M_COMP   = 10)
! componenti della miscela
      integer*4  GAS_MET    !metano  (C  H4)
     -          ,GAS_ET     !etano   (C2 H6)
     -          ,GAS_PROP   !propano (C3 H8)
     -          ,GAS_BUT    !butano  (C4 H10)
     -          ,GAS_IBUT   !isobutano  (C4 H10)
     -          ,GAS_PENT   !pentano (C5 H12)
     -          ,GAS_ES     !esano   (C6 H14)
     -          ,GAS_N2     !azoto   (N2)
     -          ,GAS_CO2    !anidride carbonica (C O2)
     -          ,GAS_H2S    !
      parameter (GAS_MET  = 1
     -          ,GAS_ET   = 2
     -          ,GAS_PROP = 3
     -          ,GAS_BUT  = 4
     -          ,GAS_IBUT = 5
     -          ,GAS_PENT = 6
     -          ,GAS_ES   = 7
     -          ,GAS_N2   = 8
     -          ,GAS_CO2  = 9
     -          ,GAS_H2S  = 10
     -)
!-----------------------------------------------------------------------
!	UNITA' LOGICHE
!-----------------------------------------------------------------------
      integer*4  f_log
      parameter (f_log = 7)  !File output (log)
  
      integer*4  u_tem       !unità file temporanea
      parameter (u_tem = 15)
!-----------------------------------------------------------------------
!     RECORD FILE TOP-CIN-TLM
!-----------------------------------------------------------------------
      integer*4  max_len_rec
      parameter (max_len_rec = 5000)

!term_campi = car. separatore per le stringhe
      character*1 term_campi
      parameter ( term_campi = char(165) )
!-----------------------------------------------------------------------
!	Parametri di Temperature stagionali
!-----------------------------------------------------------------------
      integer*4  max_stag
      parameter (max_stag = 4) !1)primavera,2)estate,3)autunno,4)inverno
!------------------------------------------------------------------------
! Numero massimo di aggiustamenti per singolo elemento nel recupero Newton
!-----------------------------------------------------------------------
      integer*4 n_agg_max
	  parameter (n_agg_max = 5)
!------------------------------------------------------------------------
! Tipo di Scenario e Identificativo di Scenario 
!-----------------------------------------------------------------------
      integer*4 TIPOSCEN_CE
      integer*4 TIPOSCEN_RETE_sta
      integer*4 TIPOSCEN_RETE_din
      parameter(TIPOSCEN_CE       = 1
     -         ,TIPOSCEN_RETE_sta = 2
     -         ,TIPOSCEN_RETE_din = 3
     -)
      integer*4 tipoScenario !=TIPOSCEN_CE,TIPOSCEN_RETE_sta,TIPOSCEN_RETE_din
      integer*4 scenario_id  !=identificativo

      common/scenario/tipoScenario,scenario_id
!-----------------------------------------------------------------------
c-sire2_0060
      real*4 taria_def   !temperatura dell'aria stagionale
      real*4 cpwt_def    !coefficienti per la correzione dovuta all taria
      
      common/def_cs/taria_def(4),cpwt_def(4)
c-sire2_0060-end
!-----------------------------------------------------------------------
      integer*4 FLAG_CONVERGENZA
cmar--------------------------DEFINIIONE COSTANTE PER H2------------------------------------  
	  real CJOUccH2
	  parameter (CJOUccH2=4186.8)
cmar--------------------------FINE DEFINIIONE COSTANTE PER H2------------------------------------
      common/f_c/FLAG_CONVERGENZA
cmar-------------------------------------------------------------- 
cmar-------------------------DEFINIZIONE LOG CENTRALIZZATO--------------
	  integer*4 LGU
	  common /LOGFILE/LGU
cmar--------------------------------------------------------------------    



