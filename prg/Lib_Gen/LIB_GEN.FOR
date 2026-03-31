!----------------------------------------------------------------------
	subroutine legge_data (str_data)
  
c[|   Legge data e ora di sistema
c	e la assegna al parametro str_data, nel formato gg/mm/aaaa hh:mm:ss

	implicit none
	INCLUDE '../inc/param.inc'

	character*(*) str_data !O
 
	integer*4 giorno,mese,anno
	integer*4 ore,minuti,secondi
	character*8 sdata
	character*10 stempo
		
	call date_and_time (sdata,stempo)
       ! Returns character data on the real-time clock and date in a form
       ! compatible with the representations defined in Standard ISO 8601:1988.
	
	str_data(1:2)=sdata(7:8) ! giorno
	str_data(3:5)='/'//sdata(5:6) ! mese
	str_data(6:10)='/' // sdata(1:4) ! anno
	str_data(11:13)=' ' // stempo(1:2) ! ore
	str_data(14:16)=':' // stempo(3:4) ! minuti
	str_data(17:19)=':' // stempo(5:6) ! secondi

      return
      end
!======================================================================
      SUBROUTINE apre_conn(name_appl)   !Apre Connessioni
                                        !Video,File_log
      implicit none
      include '../inc/param.inc'
      character*(*) name_appl  ! Nome applicazione
!------------------------------------------------------------
c$dafare      call LIB$INIT_TIMER
!------------------------------------------------------------
      call get_DatLog(c_path_dirdati,c_path_dirlog,c_path_dirute)

      call open_error(f_log,' ',name_appl)

      return
      end
!======================================================================
      SUBROUTINE CHIUDE_CONN(f_err)     !Chiude Connessioni
                                        !Video,File_log
      implicit none
      include '../inc/param.inc'
      integer*4  f_err          !Flag errore (0:ok, 1:ko)
!------------------------------------------------------------
c-elena      character*32 elapsed_time
c-elena      character*22 cpu_time
c$dafare      logical*2 action_rtn
c$dafare      external  action_rtn
!------------------------------------------------------------
c-elena	ELAPSED_TIME=' ' !cport
c-elena	cpu_time=' ' !cport
c$dafare		call LIB$SHOW_TIMER(,1,action_rtn,elapsed_time,)  !ELAPSED TIME
c$dafare      call LIB$SHOW_TIMER(,2,action_rtn,cpu_time,)      !CPU TIME
!-scrittura su file di log
c-elena      call gest_error (3,0,'Chiude_conn',ELAPSED_TIME,0)
c-elena      call gest_error (3,0,'Chiude_conn',cpu_time,0)
!------------------------------------------------------------
      call close_error(f_err)

      return
      end
!======================================================================
      logical*2 function action_rtn (TIME,tipo_t)

      character*(*) TIME,tipo_t

      tipo_t = TIME
      action_rtn = .false.

      return
      end
!======================================================================
	subroutine risultati_pronti()

      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      integer*4     stato
      character*(312)nome_file

	nome_file=trim(c_path_dirlog)//'/'//trim(nome_app)//'.RIS'
      open (unit=u_tem,
     -     file=nome_file,
     -     recordtype='stream_lf',recl= len_rec_log,
     -     form='formatted',shared,err=100,iostat=stato,
     -     status='replace')
      close (u_tem)

100   continue
      return
      end
!======================================================================
      integer*4 function tc_da_sta(iel)

c Associa allo stato finale di un elemento il corrispondente telecomando;
c se lo stato è illegale non modifico il valore del telecomando.

      implicit none
      include '../inc/param.inc'
      include '../inc/tj.inc'   
      include '../inc/th.inc'   

      integer*4 iel !offset elemento
c
      tc_da_sta = htcitj(iel)

	if     (hstatj(iel).eq.sta_off) then
	        tc_da_sta = tc_off

	elseif (hstatj(iel).eq.sta_fr) then
	        tc_da_sta = tc_on
	
	elseif (hstatj(iel).eq.sta_on) then
	        tc_da_sta = tc_nr

	elseif (hstatj(iel).eq.sta_q) then
	        tc_da_sta = tc_q
	
	elseif (hstatj(iel).eq.sta_pm) then
	    if (htcitj(iel).ne.tc_npm) then
  	        tc_da_sta = tc_pm
	    endif

	elseif (hstatj(iel).eq.sta_pv) then
	    if (htcitj(iel).ne.tc_npv.and.
     *        htcitj(iel).ne.tc_p.and.
     *        htcitj(iel).ne.tc_pq) then
              tc_da_sta = tc_pv
	    endif
	endif

      return
	end
c**********************************************************************
c Subroutine e function della vecchia LIB_TOP che vanno mantenute
c**********************************************************************
      subroutine ini_common

      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/err.inc'
      include '../inc/stazione.inc'
      include '../inc/th.inc'
      include '../inc/ai.inc'
      include '../inc/scenario.inc'
      include '../inc/simpstat_aux.inc'
      include '../inc/OTC_TIME.inc'
      include '../inc/nw.inc'
      include '../inc/conv.inc'
      include '../inc/flag.inc'
      include '../inc/ric.inc'
      integer*4 i
c------------------------------------------------------------------
c common udm  -----------------------------------
c      temperatura
      i = udm_T  !=1
c                        valori imposti
      udm_grd_id_db(i)=15
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=2
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="K"

c      pressione
      i = udm_P
c                        valori imposti
      udm_grd_id_db(i)=7
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=1
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="bar ass"

c      potenza
      i = udm_PW
c                        valori imposti
      udm_grd_id_db(i)=20
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=1
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="kW"

c      prevalenza
      i = udm_H
c                        valori imposti
      udm_grd_id_db(i)=35
      udm_fact_md(i)=1000
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=0
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="m"

c      portata volumetrica in Condizioni Normali
      i = udm_Q
c                        valori imposti
      udm_grd_id_db(i)=52
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=2
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="1000*Nm3/h"

c      portata volumetrica in Condizioni Macchina
      i = udm_QM
c                        valori imposti
      udm_grd_id_db(i)=1
      udm_fact_md(i)=1000
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=0
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="m3/h"

c      numero giri in percentuale
      i = udm_NG_perc
c                        valori imposti
      udm_grd_id_db(i)=29
      udm_fact_md(i)=1/100.
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=0
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="%N"

c      heat rate
      i = udm_HR
c                        valori imposti
      udm_grd_id_db(i)=25
      udm_fact_md(i)=1000
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=1
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="kJ/kWh"
c-ele 13/7/06
c      velocità
      i = udm_V
c                        valori imposti
      udm_grd_id_db(i)=77
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=1
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="m/s"
c-ele 26/7/06
c      coefficienti
      i = udm_coeff
c                        valori imposti
      udm_grd_id_db(i)=31
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=4
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)=""

c      PCS
      i = udm_pcs
c                        valori imposti
      udm_grd_id_db(i)=80
      udm_fact_md(i)=Conv_da_Sm3_a_Nm3/1.e+6
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=0
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="MJ/sm3"

c      Lunghezza
      i = udm_lun
c                        valori imposti
      udm_grd_id_db(i)=38
      udm_fact_md(i)=1
      udm_plus_md(i)=0
c                        valori inizializzati come udm interna
      udm_grd_prec_db(i)=0
      udm_fact_db(i)=1
      udm_plus_db(i)=0
      udm_sgl_db(i)="m"

c common udm  -----------------------------------

c common scenario.inc
      stagione = 1 !1=Primavera
	interpol = 1 !1=Gradino
	tipo_prel= 1 !1=Medio
	fl_nwrec = 0 !0=No recupero (Newton sem)
	fl_tlc   = 0 !0=No telecomandi
	tipo_tlc = 0 !0=No ciclici
      fl_modul = 0 !0=No modulazioni
	ist_prel = 0
	ist_iniz = 0
	ist_fine = 1
	passo_camp  = 1
	ultimo_camp = 0
      corr_ist = 0
      fl_est = 0

c common err.inc
      unit_filelog = 0   ! Unita' file log (0:NO)

c common flag.inc
      fl_pcls_def = .False.

!-----------------------------------------------
c common ric.inc
!-Parametri di simulazione (modificabili in Parasim.config)
      rc_weit_p     = 10.0    !Peso per P
      rc_weit_q     = 1.0     !Peso per Q
      rc_weit_on    = 0.1     !10.0  Peso per pmon=pval
      rc_atol_cvP   = 1.e-1   !1.e-4 Tolleranza ass.conv.P(punti)
      rc_rtol_cvP   = 1.e-4   !Tolleranza rel.conv.P(punti)
      rc_atol_cvQ   = 1.e0    !1.e-3 Tolleranza ass.conv.Q(tronchi,el.spec)
      rc_rtol_cvQ   = 1.e-3   !Tolleranza rel.conv.Q(tronchi,el.spec)
      rc_rtol_cvS   = 1.e-4   !Tolleranza rel.conv.increm.somm.(P e Q)**2
      rc_atol_cnB   = 1.0     !Tolleranza controllo equaz.Bilancio
      rc_atol_cnF   = 10.0    !Tolleranza controllo equaz.Fergusson
      rc_atol_cnQ   = 10.0    !Tolleranza controllo Q imposte
      rc_atol_cnP   = 1.0     !Tolleranza controllo P imposte
      rc_atol_cnOn  = 10.0    !Tolleranza controllo pmon=pval
      t_d_passo_min = 1.e-6   !1.e-7 Delta min tempo per passo dinamica
      t_d_passo_max = 1.e-1   !Delta max tempo per passo dinamica
      t_passo_input = 0.9/60. !Delta min tempo fra input (meno di un minuto)
      tol_gear      = 1.e-1   !1.e-2 Tolleranza accuratezza GEAR 
!-----------------------------------------------

c common param.inc
      data te_bin(1),te_bin(2),te_bin(3),te_bin(4),te_bin(5)
     -    ,te_bin(6),te_bin(7),te_bin(8),te_bin(9),te_bin(10)
     -    /E_TR,E_VL,E_CE,E_VS,E_VR,E_VD,E_VG,E_CS,E_VC,E_VT/

      data te_uni(1),te_uni(2),te_uni(3),te_uni(4),te_uni(5),te_uni(6)
     -    /E_IM,E_PZ,E_ST,E_RG,E_PR,E_UT/

      data te_spe(1),te_spe(2),te_spe(3),te_spe(4),te_spe(5)
     -    ,te_spe(6),te_spe(7),te_spe(8),te_spe(9),te_spe(10)
     -    ,te_spe(11),te_spe(12),te_spe(13),te_spe(14),te_spe(15)
     -    /E_VL,E_CE,E_IM,E_PZ,E_ST,E_RG,E_PR,
     -     E_VS,E_VR,E_VD,E_VG,E_CS,E_VC,E_VT,E_UT/

      data te_gen(1),te_gen(2),te_gen(3),te_gen(4),te_gen(5)
     -    ,te_gen(6),te_gen(7),te_gen(8),te_gen(9),te_gen(10)
     -    ,te_gen(11),te_gen(12),te_gen(13),te_gen(14),te_gen(15)
     -    ,te_gen(16),te_gen(17)
     -    /E_TR,E_VL,E_CE,E_IM,E_PZ,E_ST,E_RG,E_PR,E_PU,
     -     E_VS,E_VR,E_VD,E_VG,E_CS,E_VC,E_VT,E_UT/

      data te_vlv(1),te_vlv(2),te_vlv(3),te_vlv(4),te_vlv(5),te_vlv(6)
     -    ,te_vlv(7)
     -    /E_VL,E_VS,E_VR,E_VD,E_VG,E_VC,E_VT/

      data te_cen(1),te_cen(2)
     -    /E_CE,E_CS/

C      common PARAM.INC
      data te_stri(e_tr)/'TRONCO'/
      data te_stri(e_vl)/'VALVOLA'/
      data te_stri(e_ce)/'CENTRALE'/
      data te_stri(e_im)/'IMPORTAZIONE'/
      data te_stri(e_pz)/'POZZO'/
      data te_stri(e_st)/'STOCCAGGIO'/
      data te_stri(e_rg)/'REGOLATORE ESTERNO'/
      data te_stri(e_pu)/'PUNTO'/
      data te_stri(e_nsm)/'NODO DI SMISTAMENTO'/
      data te_stri(e_vs)/'VALVOLA DI SEZIONAMENTO'/
      data te_stri(e_vr)/'VALVOLA DI RIDUZIONE'/
      data te_stri(e_cm)/'CENTRALE MULTIFUNZIONE'/
      data te_stri(e_pr)/'PRELIEVO SPECIALE'/
      data te_stri(e_vd)/'IMPIANTI DI RIDUZIONE'/
      data te_stri(e_vg)/'IMPIANTI DI REGOLAZIONE'/
      data te_stri(e_cs)/'CENTRALE SEMPLIFICATA'/
      data te_stri(e_vc)/'VALVOLA DI COLLEGAMENTO'/
      data te_stri(e_vt)/'VALVOLA DI STACCO'/
      data te_stri(e_ut)/'UNARIO DI STACCO'/

c common stazione.inc
c      dp_off = 0.0  ! inizializzazione per il check di centrale spenta (simcent)

c common/Sai/    (file AI.INC)
c  Non sono stati definiti i messaggi corrispondenti agli stati STA_NQM(portata
c  massima negativa),STA_QA(portata massima pozzi consentita da Agip),STA_PA
c  (pressione massima pozzi consentita da Agip) in quanto non più utilizzati,
c  visto che questi due dati sono stati sostituiti da una generica pmaxtx e qmaxtx
c-elena
	data STATai (STA_OFF) / 'OFF'/ 	
	data STATai (STA_NR ) /	'NON RITORNO illegale' /	

	data STATai (STA_QM) /
     *'stato illegale con violazione della PORTATA max elemento' /

c  Tale stato veniva assegnato in caso di violazione in importazioni e pozzi,
c  non viene più assegnato in quanto per il calcolo della Q non si utilizza
c  più un modello matematico (e quindi curve caratteristiche).La qmax è assegnata
c  da utente
c	 
	data STATai(STA_QC )/
     *'stato illegale con violazione PORTATA max curva caratteristica'/	 	
c-elena
	data STATai(STA_Q  ) /'stato regolato con set in PORTATA'/

	data STATai(STA_PM ) /'stato regolato con set in PRESSIONE MONTE'/
	data STATai(STA_PV ) /'stato regolato con set in PRESSIONE VALLE'/
	data STATai(STA_PN ) /
     *'stato illegale con violazione della PRESSIONE minima' /
	data STATai(STA_PW ) /	
     *'stato illegale con violazione della POTENZA massima' /	 

	data STATai(STA_ON ) /	'flusso libero UNIDIREZIONALE'/
	data STATai(STA_FR ) /	'flusso libero BIDIREZIONALE'/
	data STATai(STA_IL ) / 'stato ILLEGALE'/

	data STATai(STA_ROM ) /
     *'stato illegale con violazione massimo RAPPORTO di COMPRESSIONE' /	 	

	data STATai(STA_P ) /
     *'stato regolato con set in PRESSIONE MONTE e PRESSIONE VALLE'/
	data STATai(STA_PMQ ) /
     *'stato regolato con set in PRESSIONE MONTE e PORTATA'/
	data STATai(STA_PVQ) /
     *'stato regolato con set in PRESSIONE VALLE e PORTATA'/
	data STATai(STA_PQ )/
     *'stato regolato con set in PRESSIONE MONTE E VALLE e in PORTATA'/

	data STATai(STA_PMMIN ) /
     *'stato illegale con violazione della PRESSIONE minima' /
	data STATai(STA_PVMIN ) /
     *'stato illegale con violazione della PRESSIONE minima' /
	data STATai(STA_PMMAX ) /
     *'stato illegale con violazione della PRESSIONE massima' /
	data STATai(STA_PVMAX ) /
     *'stato illegale con violazione della PRESSIONE massima' /

	data STATai(STA_PU ) /'stato regolato con set in PRESSIONE'  /	
	data STATai(STA_PASP )/
     *'stato regolato con set in PRESSIONE ASPIRAZIONE'/
	data STATai(STA_PMAN )/
     *'stato regolato con set in PRESSIONE MANDATA'/
	data STATai(STA_PAMQ )/
     *'stato regolato con set in PRESSIONE ASP. E MANDATA e in PORTATA'/

c common /sta_tc/ (file TH.inc)
	data STADATC (TC_PV) /STA_PV/   !regolazione in pressione valle
	data STADATC (TC_PM ) /STA_PM/  !regolazione in pressione monte
	data STADATC (TC_Q) /STA_Q/	    !regolazione in portata 
	data STADATC (TC_ON) /STA_FR/	!flusso libero bidirezionale(bypass)
	data STADATC (TC_OFF) /STA_OFF/ !regolazione off(el.spento)
	data STADATC (TC_NR) /STA_ON/	!flusso libero unidirezionale(non ritorno)
	data STADATC (TC_PQ) /STA_PV/	!regolazione in Pvalle,Pmonte e Portata
	data STADATC (TC_NPV) /STA_PV/  !regolazione in Pvalle con non ritorno
	data STADATC (TC_NPM) /STA_PM/  !regolazione in Pmonte con non ritorno

c common /cmf/ (file STAZIONE.INC)
      data nit/0/, nitmax/1/
     *         ,tipo_corr/1/

c common/SIMPSTAT_AUX/  (file simpstat_aux.INC)
      data fl_exit /.false./

c COMMON /ARGO_TIME/ (file OTC_TIME.INC)
      data    opt_time /24/           ! I) durata dell'ottimizzazione

c-sire2_0060
	data taria_def(1) /taria_pri/ ! 1=Primavera
	data taria_def(2) /taria_est/ ! 2=Estate
	data taria_def(3) /taria_aut/ ! 3=Autunno
	data taria_def(4) /taria_inv/ ! 4=Inverno
      cpwt_def(1) = 14.4652
      cpwt_def(2) = -40.0177
      cpwt_def(3) = 40.8789
      cpwt_def(4) = -14.3274
c-sire2_0060-end

c------> inizializzo lo stato di filtri e aircooler
      call ini_fil_cool

c-ele 26/7/06
c------>inizializzo le percentuali minima e massima del criterio di equidistanza
c       (nuova implementazione)
      call ini_perc_equi

	return
	end
c********************************************************************************
      subroutine ini_fil_cool

c    Inizializza a 0 lo stato di filtri e air cooler
c    (lo stato è la variabile di test)

	implicit none
      include '../inc/param.INC'
      include '../inc/filtri.INC'
      include '../inc/air_cooler.INC'

	integer*4 i

c common filtri e aircooler
      do i=1,max_vert
	   c_f_ce_stat(i)=0
	   c_ac_ce_stat(i)=0
	end do

      do i=1,max_unit
	   c_f_tg_stat(i)=0
	   c_ac1_stat(i)=0
	   c_ac2_stat(i)=0
	end do
	
	return 
	end
c********************************************************************************
      subroutine ini_perc_equi
	
	implicit none
      include '../inc/param.INC'
      include '../inc/units.INC'

	integer*4 i

      do i=1,max_unit
	   unit_perc_min_equi(i)=0
	   unit_perc_max_equi(i)=0
	end do
c-prova specifica relativa allo scenario MALB_OK
c	unit_perc_min_equi(1)=0.03
c	unit_perc_max_equi(1)=0.05
c	unit_perc_min_equi(2)=0.03
c	unit_perc_max_equi(2)=0.05
c	unit_perc_min_equi(3)=0.01
c	unit_perc_max_equi(3)=0.03
c	unit_perc_min_equi(4)=0
c	unit_perc_max_equi(4)=0

	return
	end
c********************************************************************************
      character*(*) function udm_out_str(grd,val,l_sigla)
c------------------------------------------------------------------
c    Restituisce la stringa str con 
c       il valore (val) secondo la udm interna per la grandezza (grd)
c       con la sigla della udm stessa
c----------------------------------------------------------------
	implicit none
      include '../inc/param.INC'
      include '../inc/conv.INC'

	integer*4 grd  ! grandezza
	real*4 val     ! valore
	integer*4 l_sigla  ! sigla : 0 NO 1 SI

	character*(50)  str ! stringa di output
	integer*4 lun  ! lunghezza stringa

	real*4 val_app, arr_dec
	integer*4 c_val,n_dec

	real*4 udm_out   ! function

510   format(F<c_val>.<n_dec>)
520   format(I<c_val>)

	if (udm_fact_db(grd).eq.0) then
	  n_dec = 0
	else
	  n_dec = log10(abs(udm_fact_db(grd)))
	endif
c-ele 20/10/2006 modificata la formula per il calcolo dei decimali secondo quanto
c indicato nelle specifiche
c	n_dec = udm_grd_prec_db(grd) - n_dec
	n_dec = udm_grd_prec_db(grd) + n_dec

      arr_dec = 5. * 10.**(-(n_dec+1))

      val_app = udm_out(grd,val)
	if (val_app.eq.0) then
	  c_val = 0
	else
	  c_val = log10(abs(val_app)+arr_dec) + 1
	endif
	if (c_val.le.0) c_val = 1
	if (val_app.lt.0) c_val = c_val +1
	if (n_dec.gt.0) then
	 c_val = c_val + 1 + n_dec
       write (str,510) val_app
	else
       write (str,520) int(val_app)
	endif
	
	if (l_sigla.eq.1) then
	  str = str(1:len_trim(str)) // 
     *    " [" // udm_sgl_db(grd)(1:len_trim(udm_sgl_db(grd))) // "]"
	endif

      lun = min(len(udm_out_str),len_trim(str))      
	udm_out_str = str(1:lun)

	return 
	end
c********************************************************************************
      real*4 function udm_mod(grd,val)
c------------------------------------------------------------------
c    Restituisce il valore (val) secondo la udm modello da udm interna DB 
c    per la grandezza (grd)
c----------------------------------------------------------------
	implicit none
      include '../inc/param.INC'
      include '../inc/conv.INC'

	integer*4 grd  ! grandezza
	real*4 val     ! valore

      udm_mod = (val - udm_plus_db(grd)) / udm_fact_db(grd)
		
	return 
	end
c********************************************************************************
      real*4 function udm_int(grd,val)
c------------------------------------------------------------------
c    Restituisce il valore (val) secondo la udm interna DB da udm modello 
c    per la grandezza (grd)
c----------------------------------------------------------------
	implicit none
      include '../inc/param.INC'
      include '../inc/conv.INC'

	integer*4 grd  ! grandezza
	real*4 val     ! valore

	udm_int = val*udm_fact_md(grd) + udm_plus_md(grd)
		
	return 
	end
c********************************************************************************
      real*4 function udm_out(grd,val)
c------------------------------------------------------------------
c    Restituisce il valore (val) secondo la udm di output da udm interna DB 
c    per la grandezza (grd)
c---------------------------------------------------------------
	implicit none
      include '../inc/param.INC'
      include '../inc/conv.INC'

	integer*4 grd  ! grandezza
	real*4 val     ! valore

	real*4 udm_int   ! functione

      udm_out = (udm_int(grd,val) - udm_plus_db(grd)) / udm_fact_db(grd)
	
	return 
	end
c------------------------------------------------------------------
c Moduli per le operazioni sulle stringhe.
c------------------------------------------------------------------
      integer*4 function s_trim_s(STRINGA)
 
c-Trova la posizione che elimina i bianchi a sinistra
      implicit none
      character*(*) STRINGA   ! I)
      integer*4 pos
 
      do pos = 1,len(STRINGA)
         if (STRINGA(POS:POS) .ne. ' ') goto 100
      enddo
      pos = len(STRINGA)
100   continue
      s_trim_s = pos

      return
      end
c------------------------------------------------------------------
      integer*4 function s_trim_d(STRINGA)
 
c-Trova la posizione che elimina i bianchi a destra
      implicit none
      character*(*) STRINGA   ! I)
      integer*4 pos

      do pos = len(STRINGA),1,-1
         if (STRINGA(POS:POS) .ne. ' ') goto 100
      enddo
      pos = 0
100   continue
      s_trim_d = pos

      return
      end
c------------------------------------------------------------------
      logical*2 function set_non_ritorno (iel)
     
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel

      if (htcitj(iel).eq.tc_nr  .or.
     *    htcitj(iel).eq.tc_npv .or.
     *    htcitj(iel).eq.tc_npm ) then
	  set_non_ritorno = .true.
	else
	  set_non_ritorno = .false.
	endif

      return
      end
c------------------------------------------------------------------
      logical*2 function set_condir (iel)  ! connessione diretta
     
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel

      if (htcitj(iel).eq.tc_nr  .or.
     *    htcitj(iel).eq.tc_on ) then
	  set_condir = .true.
	else
	  set_condir = .false.
	endif

      return
      end
c------------------------------------------------------------------
      logical*2 function set_stapres (iel)  ! hsta setta in pressione PM e PV
     
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel

      if (hstatj(iel).eq.sta_pm  .or.
     *    hstatj(iel).eq.sta_pv  .or.
     *    hstatj(iel).eq.sta_on  .or.
     *    hstatj(iel).eq.sta_fr ) then
	  set_stapres = .true.
	else
	  set_stapres = .false.
	endif

      return
      end
c------------------------------------------------------------------
      logical*2 function set_stapm (iel)  ! hsta setta in pressione PM
     
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel

      if (hstatj(iel).eq.sta_pm  .or.
     *    hstatj(iel).eq.sta_on  .or.
     *    hstatj(iel).eq.sta_fr ) then
	  set_stapm = .true.
	else
	  set_stapm = .false.
	endif

      return
      end
c------------------------------------------------------------------
      logical*2 function set_stapv (iel)  ! hsta setta in pressione PV
     
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel

      if (hstatj(iel).eq.sta_pv  .or.
     *    hstatj(iel).eq.sta_on  .or.
     *    hstatj(iel).eq.sta_fr ) then
	  set_stapv = .true.
	else
	  set_stapv = .false.
	endif

      return
      end
c------------------------------------------------------------------
c------------------------------------------------------------------
      subroutine ini_par ()
*&*
!-Inizializzazione dei parametri necessari 
*&*
      implicit none
      include '../inc/staznw_par.inc'
!-----------------------------------------------------------------------
!-NOTA: !* indica parametro modificabile in Parasim.config
!-----------------------------------------------------------------------
      MITNwd = 50       !* Max num.iter.Newton (20:50)
      TOLXwd = 0.001    !* Diff.relativa max iter.Newton (0.01:0.0001)
      TOLFwd = 1.       !* Max Residui (0.01:1)
      mitcwd = 50       !* Max num.iter.in comp.temp.
      TOLCwd = 0.001    !* Tolleranza cicli in comp.temp.
      mit_pcls  = 50    !* Max num.iter.in pcls
      toll_conce = 0.01 !* Tolleranza cicli in conce
      mit_conce  = 50   !* Max num.iter.in conce
	max_count = 100   !* Max num.iter.risol.sistema in PQ
	m_it_nconv = 3    !* Max num.iter.diverg.risol.sistema in PQ,TC 
!-----------------------------------------------------------------------
      rq0 = 100.0         !Trigger per Q
      dp_on_vl = 0.01     !DeltaP Min esercizio Valvole
      dp_on_ce = 0.5      !DeltaP Min esercizio Centrali
      repsmch = 0.138e-16 !eps di macchina
	rscale = 1.0        !range newton
	p05 = 0.05          !Precisione controlli convergenza
	zero = 0.0          !zero
	uno = 1.0           !uno
      maxit = 100         !Num.iter.calcolo T (20)
!-----------------------------------------------------------------------
      repsQ = 0.5       !* Precisione controlli Q
	repsP = 0.005     !* Precisione controlli P
	repsTC = 0.1      !* Precisione controlli T
	zeroTC = 0.05     !* Precisione test 0
      perc_p = 1.       !* Correz.P punti centrale
      perc_q = 1.       !* Correz.Q punti centrale
      max_deltap = 0.2  !* Range P punti centrale
      max_deltaq = 20.  !* Range Q punti centrale
!-----------------------------------------------------------------------

      return
      end
c------------------------------------------------------------------
      subroutine ini_par_dep ()
*&*
!-Inizializzazione dei parametri necessari 
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/staznw_par.inc'
      include '../inc/stazione.inc'
      include '../inc/flag.inc'
!-----------------------------------------------------------------------
      TOLCwd_max = TOLCwd*100 !Max Tolleranza sui cicli in comp. e temp.
      toll_pcls = TOLCwd      !Tolleranza sui cicli in pcls (conviene = TOLCwd)

      if(fl_sim)then
c per simcent viene settato dp_off pari 0.0
         dp_off = 0.
	elseif (fl_din) then
c per la dinamica viene settato dp_off pari a deu volte dp_on_ce
         dp_off = dp_on_ce*2
	else
c per lo stazionario viene settato dp_off pari a dp_on_ce
         dp_off = dp_on_ce
	endif

      return
      end
c------------------------------------------------------------------
c********************************************************************************
c  Controllo tempo
c********************************************************************************
      subroutine time_ini (t_ini,t_fine)
	
	implicit none
      include '../inc/param.INC'

      real*4 t_ini,t_fine

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      real*4 t_simu_total,t_simu_ini
     -      ,t_time_total,t_time_ini
     -      ,t_time_clock

	common /time_area/
     -       t_simu_total,t_simu_ini
     -      ,t_time_total,t_time_ini
     -      ,t_time_clock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

	integer*4 hh,mm,ss
      character*(data_len)data_time

1000  format(I)

      t_simu_ini = t_ini
      t_simu_total = t_fine - t_ini

	call legge_data(data_time)

      read(data_time(12:13),1000) hh
      read(data_time(15:16),1000) mm
      read(data_time(18:19),1000) ss

	t_time_ini   = hh+mm/60.+ss/3600.
      t_time_total  = max(15.0,t_simu_total) !15 minuti sono accettabili
      t_time_total  = min(60.0,t_simu_total) !60 minuti sono un limite 
      t_time_total  = t_time_total/60.0

      t_time_clock = 2.0/60.

	return
	end
c------------------------------------------------------------------
      logical*2 function time_mod (t_simu_cur)
	
	implicit none
      include '../inc/param.INC'

      real*4 t_simu_cur

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      real*4 t_simu_total,t_simu_ini
     -      ,t_time_total,t_time_ini
     -      ,t_time_clock

	common /time_area/
     -       t_simu_total,t_simu_ini
     -      ,t_time_total,t_time_ini
     -      ,t_time_clock
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

      real*4 t_time_cur
      real*4 t_time_dt,t_simu_dt,dt_simu, dt_time
	integer*4 hh,mm,ss
      character*(data_len)data_time

1000  format(I2)

	call legge_data(data_time)

      read(data_time(12:13),1000) hh
      read(data_time(15:16),1000) mm
      read(data_time(18:19),1000) ss

	t_time_cur = hh+mm/60.0+ss/3600.0

	t_time_dt = t_time_cur - t_time_ini

      time_mod = .False.
	if (t_time_dt.ge.t_time_clock) then
	  t_simu_dt = t_simu_cur - t_simu_ini
	  dt_simu = t_simu_dt/t_simu_total
	  dt_time = t_time_dt/t_time_total
	  t_simu_ini = t_simu_cur
	  t_time_ini = t_time_cur
	  if (dt_simu .lt. dt_time) then
	     time_mod = .True.
	  endif
	endif

	return
	end
c------------------------------------------------------------------
c------------------------------------------------------------------
      character*(*) function str_tempo (tempo)
	
	implicit none
      include '../inc/param.INC'

      real*4 tempo

	integer*4  mm,hh

!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
	  mm = int(mod(tempo,1.0)*60+0.5)
        if (mm.eq.60) then
	    mm = 0
	    hh = int(tempo) + 1
	  else
	    hh = int(tempo)
	  endif
        write (str_tempo,'(i4,":",i2.2)')  hh, mm
	return
	end
c------------------------------------------------------------------
c********************************************************************************

ccccccccccc      real*4 function qset_real(iel)
c------------------------------------------------------------------
c    Restituisce il valore di qset effettivo da settare
c    Si ipotizza che il set in portata debba tenere conto dell'energia
c      solo per gli unari
c
c----------------------------------------------------------------
ccccccccccc	implicit none
ccccccccccc      include '../inc/param.INC'
ccccccccccc      include '../inc/default.inc '      !: costante
ccccccccccc      include '../inc/td.INC'
ccccccccccc      include '../inc/tj.INC'

ccccccccccc	integer*4 iel  ! elemento

ccccccccccc       logical*2 elem_uni
ccccccccccc       external elem_uni
C------------------------------------

ccccccccccc      if (elem_uni(iel)) then


ccccccccccc	   qset_real = qsettj(iel)*pcls_def/pclstd(iel)
ccccccccccc	else
ccccccccccc	   qset_real = qsettj(iel)
ccccccccccc	endif
		
ccccccccccc	return 
ccccccccccc	end



      real*4 function qset_real(iel)
c------------------------------------------------------------------
c    Restituisce il valore di qset effettivo da settare
c    Si ipotizza che il set in portata debba tenere conto dell'energia
c      solo per gli unari
c
c----------------------------------------------------------------
	implicit none
      include '../inc/param.INC'
      include '../inc/default.inc '      !: costante
      include '../inc/td.INC'
      include '../inc/tj.INC'
cmar_pcs
cccc      include  '../inc/FLAG_CONV.INC'
cmar_pcs

	integer*4 iel  ! elemento

       logical*2 elem_uni
       external elem_uni
C------------------------------------
      
ccc       FLAG_CONVERGENZA=1

      if (elem_uni(iel)) then

cmar_pcs
          IF (FLAG_CONVERGENZA.EQ.1)THEN
           
	    qset_real = qsettj(iel)*pcls_def/pclstd(iel)

          ELSEIF (FLAG_CONVERGENZA.EQ.2)THEN


	    qset_real = qsettj(iel)


	    ELSEIF (FLAG_CONVERGENZA.EQ.3)THEN
          
	          if (diretj(iel).eq.1)then

                    qset_real = qsettj(iel)

                else
                    qset_real = qsettj(iel)*pcls_def/pclstd(iel)

                endif

	    ELSEIF (FLAG_CONVERGENZA.EQ.4)THEN

	          if (diretj(iel).eq.1)then

                    qset_real = qsettj(iel)*pcls_def/pclstd(iel)

                else
                    qset_real = qsettj(iel)

                endif


	ENDIF
cmar_pcs

cmar_pcs	   qset_real = qsettj(iel)*pcls_def/pclstd(iel)
	else
	   qset_real = qsettj(iel)
	endif
		
	return 
	end
!=============================================================================

!=============================================================================
	SUBROUTINE out_TIMCOST() 

	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/tv.inc'		!:IN
	INCLUDE '../inc/err.inc'		!:IN

	Real*4 t_tr_min,t_tr_max
	Real*4 t_pu_min,t_pu_max
	Real*4 dg_sf_tr,dg_sf_pu
	Real*4 t_min,t_max
c
	INTEGER*4 o_tr_min,o_tr_max
	INTEGER*4 o_pu_min,o_pu_max
	character*(l_D_ERR) msgerr
	character*(7) str_tempo
	external str_tempo

!----------------------------------------------------------------------
	 call TIMCOST(t_tr_min,t_tr_max,o_tr_min,o_tr_max,
     *                   t_pu_min,t_pu_max,o_pu_min,o_pu_max,
     *                   dg_sf_tr,dg_sf_pu,t_min,t_max) 

       call gest_error (3,0,""," ",0)
       call gest_error (3,0,"",
!     * "==========  Informazioni sui tempi ",0)
     * "==========  Informazioni sui tempi di risposta della rete",0)
       write (msgerr,'(" Tronco piu'' lento : ",a)') snomtv(o_tr_min)
       call gest_error (3,0,"",msgerr,0)
       write (msgerr,'(" Tronco piu'' veloce: ",a)') snomtv(o_tr_max)
       call gest_error (3,0,"",msgerr,0)
       write (msgerr,'(" Punto piu'' lento : ",a)') snomtv(o_pu_min)
       call gest_error (3,0,"",msgerr,0)
       write (msgerr,'(" Punto piu'' veloce: ",a)') snomtv(o_pu_max)
       call gest_error (3,0,"",msgerr,0)
c       write (msgerr,'(" Tronco piu'' lento: ",a,
c     -         " col tempo",a7)') snomtv(o_tr_min), str_tempo(t_tr_min)
c       call gest_error (3,0,"",msgerr,0)
c       write (msgerr,'(" Tronco piu'' veloce: ",a,
c     -         " col tempo",a7)') snomtv(o_tr_max), str_tempo(t_tr_max)
c       call gest_error (3,0,"",msgerr,0)
c       write (msgerr,'(" Punto piu'' lento: ",a,
c     -         " col tempo",a7)') snomtv(o_pu_min), str_tempo(t_pu_min)
c       call gest_error (3,0,"",msgerr,0)
c       write (msgerr,'(" Punto piu'' veloce: ",a,
c     -         " col tempo",a7)') snomtv(o_pu_max), str_tempo(t_pu_max)
c       call gest_error (3,0,"",msgerr,0)

	RETURN !out_TIMCOST
	END	
cace
cace
!=============================================================================
	SUBROUTINE TIMCOST(t_tr_min,t_tr_max,o_tr_min,o_tr_max,
     *                   t_pu_min,t_pu_max,o_pu_min,o_pu_max,
     *                   dg_sf_tr,dg_sf_pu,t_min,t_max) 


c          costanti di tempo per i tronchi e punti (dovuti alla topologia):
c
c          t_tr_min     costante di tempo per il tr + lento
c          t_tr_max     costante di tempo per il tr + veloce
c          o_tr_min     indice topologico tr + lento
c          o_tr_max     indice topologico tr + veloce
c          t_pu_min     costante di tempo per il pu + lento
c          t_pu_max     costante di tempo per il pu + veloce
c          o_pu_min     indice topologico pu + lento
c          o_pu_max     indice topologico pu + veloce
c          dg_sf_tr     grado di stiffnes tronco
c          dg_sf_pu     grado di stiffnes punto
c          t_min        tempo minimo del sistema
c          t_max        tempo massimo
*&*------------------------------------------------------------
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tx.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:IN
	include '../inc/staznw_par.inc'!:parametri 
	INCLUDE '../inc/ty.inc'		!:INOUT
c
	Real*4 t_tr_min,t_tr_max
	Real*4 t_pu_min,t_pu_max
	Real*4 dg_sf_tr,dg_sf_pu
	Real*4 t_min,t_max
c
	INTEGER*4 o_tr_min,o_tr_max
	INTEGER*4 o_pu_min,o_pu_max
c
c
      INTEGER*4 je,im,iv,ipg
	Real*4 timc_pu(max_pu),timc_tr
c
	Real*4   q_mas,b_b,pm,r_tr
	Real*4   alfa,beta
	Real*4   x_app,dp_ro 
!----------------------------------------------------------------------
!	inizializzazione
!----------------------------------------------------------------------
	o_tr_min = otipti(e_tr)
	o_tr_max = otipti(e_tr)
	o_pu_min = otipti(e_pu)
	o_pu_max = otipti(e_pu)
	
	DO je = 1,max_pu
		timc_pu(je) = 0.0
	ENDDO

	timc_tr = 0.0

      t_tr_min = 1000.
	t_tr_max = -1.
	t_pu_min = 1000.
	t_pu_max = -1.
!----------------------------------------------------------------------
!	costanti di tempo dei TRONCHI
!----------------------------------------------------------------------
	DO je = otipti(e_tr),etipti(e_tr)
	 if(abs(porttj(je)) .gt. zeroTC) then
		im = opumto(je)		 	      ! offset punto monte in TOPO
		iv = opuvto(je)		       	  ! offset punto valle in TOPO
	    q_mas = porttj(je)*ro0td(je)
	    b_b = bferty(je)/(ro0td(je)*ro0td(je))
c                             b_b serve per scrivere la Fergusson con Q(in massa)
          pm = (prestj(im)+prestj(iv))/2.

          r_tr = ABS(b_b*q_mas/pm)
c         calcolo la resistenza totale nel punto partendo da quella 
c         dei tronchi incidenti:sum = (1/r1+1/r2  ) da cui 1/rtot=sum  
c         da cui rtot = 1/sum
	    timc_pu(pgrato(im)) = timc_pu(pgrato(im))+1./r_tr 
	    timc_pu(pgrato(iv)) = timc_pu(pgrato(iv))+1./r_tr 
c
	    call DERZROT (im,rocitj(im),temptj(im),alfa,beta)
	    CALL zrotdp(im,rocitj(im),temptj(im),x_app,dp_ro)
	    a1 = (1+alfa)*volntx(im)/dp_ro
		call DERZROT (iv,rocitj(iv),temptj(iv),alfa,beta)
	    CALL zrotdp(iv,rocitj(iv),temptj(iv),x_app,dp_ro)
	    a2 =(1+alfa)*volntx(iv)/dp_ro
c          c_tr = ABS(volntx(je)*(a1+a2))
          c_tr = ABS(a1+a2)
	    timc_tr = r_tr * c_tr
c
          if(timc_tr.lt. t_tr_min) then
           t_tr_min = timc_tr
           o_tr_min = je
          endif
          if (timc_tr.gt. t_tr_max) then
           t_tr_max = timc_tr
           o_tr_max = je
          endif
	 end if
	ENDDO
!----------------------------------------------------------------------
!	costanti di tempo dei PUNTI
!----------------------------------------------------------------------
	DO je = otipti(e_pu),etipti(e_pu)
	  ipg = pgrato(je)
        if(timc_pu(ipg).ne.0.0) then
	    call DERZROT(je,rocitj(je),temptj(je),alfa,beta)
	    cALL zrotdp (je,rocitj(je),temptj(je),x_app,dp_ro)

          c_tr  = volntx(je)/dp_ro*(1+alfa)
c
          timc_pu(ipg) = ABS(c_tr / timc_pu(ipg))
!-------     costanti di tempo MIN e MAX dei Punti
          if(timc_pu(ipg).lt. t_pu_min) then
           t_pu_min = timc_pu(ipg)
           o_pu_min = je
          endif
          if (timc_pu(ipg).gt. t_pu_max) then
           t_pu_max = timc_pu(ipg)
           o_pu_max = je
          endif
        end if
	ENDDO
!----------------------------------------------------------------------
      dg_sf_tr = 0.0
	dg_sf_pu = 0.0
	if (t_tr_min.ne.0.0) then
	   dg_sf_tr = t_tr_max/t_tr_min   ! grado STIFFNESS dei tronchi
	end if
	if (t_pu_min.ne.0.0) then
	   dg_sf_pu = t_pu_max/t_pu_min   ! grado STIFFNESS dei punti
	end if
	t_min = MIN(t_tr_min,t_pu_min)
	t_max = MAX(t_tr_max,t_pu_max)

	RETURN !timcost
	END	
!======================================================================
cace
cace
c------------------------------------------------------------------
      character*(*) function str_reg (iel)
	
	implicit none
      include '../inc/param.INC'
      include '../inc/tj.INC'
      include '../inc/td.INC'
      include '../inc/th.INC'

	integer*4  iel

       logical*2 elem_cen,elem_uni
       external  elem_cen,elem_uni
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
      if (htcitj(iel).eq.tc_off) then
	   str_reg = "OFF"
      elseif (htcitj(iel).eq.tc_on) then
	   str_reg = "ON"
	elseif (htcitj(iel).eq.tc_nr) then
	   str_reg = "NR"
	elseif (htcitj(iel).eq.tc_pv) then
	   if (elem_uni(iel)) then
	     str_reg = "PSET"
	   elseif(elem_cen(iel)) then
	     str_reg = "PMAN"
	   else
	     str_reg = "PVSET"
	   endif
	elseif (htcitj(iel).eq.tc_pm) then
	   if (elem_uni(iel)) then
	     str_reg = "PSET"
	   elseif(elem_cen(iel)) then
	     str_reg = "PASP"
	   else
	     str_reg = "PMSET"
	   endif
	elseif (htcitj(iel).eq.tc_npv) then
	   if (elem_uni(iel)) then
	     str_reg = "NPSET"
	   elseif(elem_cen(iel)) then
	     str_reg = "NPMAN"
	   else
	     str_reg = "NPVSET"
	   endif
	elseif (htcitj(iel).eq.tc_npm) then
	   if (elem_uni(iel)) then
	     str_reg = "NPSET"
	   elseif(elem_cen(iel)) then
	     str_reg = "NPASP"
	   else
	     str_reg = "NPMSET"
	   endif
	elseif (htcitj(iel).eq.tc_q) then
	   str_reg = "QSET"
	elseif (htcitj(iel).eq.tc_pq) then
	   str_reg = "PQSET"
	else
	   str_reg = " "
	endif

	return
	end
c------------------------------------------------------------------
