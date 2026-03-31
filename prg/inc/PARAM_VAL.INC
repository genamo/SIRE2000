!-----------------------------------------------------------------------
!           ELEMENTI TOPOLOGICI DA DATA BASE
!-----------------------------------------------------------------------
      integer*4  xmx
      parameter (xmx  = 3)     
	  integer*4  max_tr_db
     -          ,max_vl_db
     -          ,max_ce_db
     -          ,max_im_db
     -          ,max_pz_db
     -          ,max_st_db
     -          ,max_rg_db
     -          ,max_pr_db
     -          ,max_pu_db
     -          ,max_vs_db
     -          ,max_vr_db
     -          ,max_vd_db
     -          ,max_vg_db
     -          ,max_cs_db
     -          ,max_vt_db
     -          ,max_ut_db
     -          ,max_nsm_db
     -          ,max_vc_db
     -          ,max_ces_db

      parameter (max_tr_db  = xmx*2000	!1700	!1500	tronchi
     -          ,max_vl_db  = xmx*100	!150	!250	valvole generiche
     -          ,max_ce_db  = xmx*100	!150	!150	centrali e assetti
     -          ,max_im_db  = xmx*20	!20		!15		importazioni
     -          ,max_pz_db  = xmx*100	!100	!100	pozzi
     -          ,max_st_db  = xmx*50	!50		!25		stoccaggi
     -          ,max_rg_db  = xmx*0     !0		!-		regolatori esterni
     -          ,max_pr_db  = xmx*100	!100	!100	prelievi interrompibili
     -          ,max_pu_db  = xmx*2000	!1700	!1500	punti
     -          ,max_vs_db  = xmx*0     !0		!-		valvole sezionamento
     -          ,max_vr_db  = xmx*0     !0		!-		valvole riduttori
     -          ,max_vd_db  = xmx*200	!150	!-		impianti di riduzione
     -          ,max_vg_db  = xmx*200	!150	!-		impianti di regolazioni
     -          ,max_cs_db  = xmx*100	!50		!-		centrali semplificate
     -          ,max_vt_db  = xmx*50	!100	!-		valvole di stacco
     -          ,max_ut_db  = xmx*50	!100	!-		unari di stacco
     -          ,max_nsm_db = xmx*50	!70		!-		nodi
     -          ,max_vc_db  = xmx*100	!200	!-      valvole di connessione nei nodi
     -          ,max_ces_db = xmx*10	!10		!-      centrali in serie
     -)
!-----------------------------------------------------------------------