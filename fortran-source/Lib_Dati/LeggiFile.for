!===================================================================================
! L'input per la simulazione è costituito da un insieme di file  corrispondenti a tabelle db.
! L'output della simulazione è costituito da un file di risultati e da un file di log.
!
! I file sono organizzati nelle seguenti cartelle:
	! \c_path_dirdati\	Dati scenario
	!     \<tabella>      (dati di input)
	!     \<tabella>      (dati di output)
	! \c_path_dirlog\	Log scenario
	!     \<log file>
	! \c_path_dirute\	Dati generici utente
	!     \<tabella>      (dati di input)
! c_path_dirdati, c_path_dirlog e c_path_dirute sono variabili definite in area common,
! che debbono essere già valorizzate prima di chiamare la presente subroutine.
! La loro valorizzazione è effettuata dal main Fortran, che riceve le corrispondenti
! informazioni come parametri dal programma chiamante (quello in ambiente .NET).
!  Sul lato client sarà presente un file di configurazione (criptato) contenente:
!  - il nome di sharing corrispondente alla root \c_path_dirdati\
!  - il nome di sharing corrispondente alla root \c_path_dirlog\
!  - il nome di sharing corrispondente alla root \c_path_dirute\
!  - il nome della cartella sul lato server contenente l'eseguibile
!===================================================================================
      Subroutine leggi_scenario (msgerr,*)

!-Legge i file di INPUT per gli elementi di uno scenario.
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'

	character*(*) msgerr       !IO)
!----------------------------------------------------
!-Definizione otipti di inizio classe elemento
!----------------------------------------------------
      otipti(e_tr) = 1
      otipti(e_vl) = otipti(e_tr)+max_tr
      otipti(e_ce) = otipti(e_vl)+max_vl
      otipti(e_im) = otipti(e_ce)+max_ce
      otipti(e_pz) = otipti(e_im)+max_im
      otipti(e_st) = otipti(e_pz)+max_pz
      otipti(e_rg) = otipti(e_st)+max_st
      otipti(e_pr) = otipti(e_rg)+max_rg
!-
      otipti(e_vs) = otipti(e_pr)+max_pr
      otipti(e_vr) = otipti(e_vs)+max_vs
      otipti(e_vd) = otipti(e_vr)+max_vr
      otipti(e_vg) = otipti(e_vd)+max_vd
      otipti(e_cs) = otipti(e_vg)+max_vg
      otipti(e_vc) = otipti(e_cs)+max_cs
!-
      otipti(e_vt) = otipti(e_vc)+max_vc
      otipti(e_ut) = otipti(e_vt)+max_vt
!-
      otipti(e_pu) = otipti(e_ut)+max_ut
!-3 elementi virtuali:
      otipti(e_oi) = otipti(e_pu)+max_pu
      otipti(e_dp) = otipti(e_oi)+1
      otipti(e_re) = otipti(e_dp)+1
!----------------------------------------------------
!-Definizione etipti di fine classe elemento
!----------------------------------------------------
      etipti(e_tr) = otipti(e_tr) - 1
      etipti(e_vl) = otipti(e_vl) - 1
      etipti(e_ce) = otipti(e_ce) - 1
      etipti(e_im) = otipti(e_im) - 1
      etipti(e_pz) = otipti(e_pz) - 1
      etipti(e_st) = otipti(e_st) - 1
      etipti(e_rg) = otipti(e_rg) - 1
      etipti(e_pr) = otipti(e_pr) - 1
!-
      etipti(e_vs) = otipti(e_vs) - 1
      etipti(e_vr) = otipti(e_vr) - 1
      etipti(e_vd) = otipti(e_vd) - 1
      etipti(e_vg) = otipti(e_vg) - 1
      etipti(e_cs) = otipti(e_cs) - 1
      etipti(e_vc) = otipti(e_vc) - 1
!-
      etipti(e_vt) = otipti(e_vt) - 1
      etipti(e_ut) = otipti(e_ut) - 1
!-
      etipti(e_pu) = otipti(e_pu) - 1
      etipti(e_oi) = otipti(e_oi)
      etipti(e_dp) = otipti(e_dp)
      etipti(e_re) = otipti(e_re)
!---------------------------------------
!-Definizioni per i 3 elementi virtuali
!---------------------------------------
      off_oi = otipti(e_oi) !alfa
      off_dp = otipti(e_dp) !omega
      off_re = otipti(e_re) !rete
      htipto(off_oi)=e_oi
      htipto(off_dp)=e_dp
      htipto(off_re)=e_re
      opumto(off_oi)=off_oi
      opumto(off_dp)=off_dp
      opumto(off_re)=off_re
      opuvto(off_oi)=off_oi
      opuvto(off_dp)=off_dp
      opuvto(off_re)=off_re
!------------------------------------------------------
!-Lettura Valori UDM per Output
!------------------------------------------------------
      call leggi_udm_out (msgerr,*9999)

!----------------------------------------------------------------------
!-Definizione Default Tipi Regolazione 
!----------------------------------------------------------------------
      call tabella_tipi_regolazione ()

!----------------------------------------------------------------------
!-Lettura parametri caratteristici per la Simulazione
!----------------------------------------------------------------------
      call leggi_parasim ()

!----------------------------------------------------------------------
!-Lettura Input Elementi Rete (NO SIMCENT, SI RETE)
!----------------------------------------------------------------------
      if(tipoScenario.ne.TIPOSCEN_CE)then

!-    Lettura dati generali di simulazione scenario
         call leggi_infoscen (msgerr,*9999)

!-    Lettura TOP e CIN per elementi RETE
         call leggi_elemrete (msgerr,*9999)

!-    Eventuale Scomposizione tronchi con lunghezza > max (50 km) 
         call scomponi_lun_tr (msgerr,*9999)

      endif
!----------------------------------------------------------------------

!----------------------------------------------------------------------
!-Lettura Input Centrali bipolari e multifunzione (SI SIMCENT, SI RETE)
!----------------------------------------------------------------------
      call leggi_centrali (msgerr,*9999)

!----------------------------------------------------------------------
      return

!----------------------------------------------------------------------
!->Errori
9999  continue
      return 1
      end
!===================================================================================
      subroutine leggi_elemrete (str_err,*)

!*-Legge i file input (TOP,CIN) per gli Elementi di RETE:
!1)punti_in.txt       (DB: v_punti)        e_pu
!2)tronchi_in.txt     (DB: v_tronchi)      e_tr
!3)regolazioni_in.txt (DB: v_regolazioni)  e_vg
!4)riduzioni_in.txt   (DB: v_riduzioni)    e_vd
!5)centrali_s_in.txt  (DB: v_centrali_s)   e_cs
!6)valvole_in.txt     (DB: v_valvole)      e_vl,e_vc (no: e_vs,e_vr)
!7)unari_in.txt       (DB: v_unari)        e_im,e_pz,e_st,e_pr
!8)stacco_in.txt      (DB: v_punti_stacco) e_vt,e_ut
      implicit none
      include '../inc/param.inc'
      include '../inc/dimension.inc'
      include '../inc/default.inc'
      include '../inc/scenario.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/tv.inc'
      include '../inc/tx.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/tj.inc'
      include '../inc/tc.inc'
      include '../inc/cin_app_ele.inc'
      include '../inc/tipo_reg.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/stations.inc'
      include '../inc/tq.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/conv.inc'

cmar_rev_log_sem

      include '../inc/allarmi.inc'
    

c       real*4 v_all(m_ogg)

cmar_rev_log_sem



      character*(*) str_err  !IO

      integer*4 pos,pos_pu,p_i,stato,i_val,iel,jel,elem_tipo
      integer*4 punto,punto_conn,sta_id,pf1,pf2,s_trim_d
      integer*4 pos_mod,ier_interp
      real*4 temp_stag(max_stag) !temp.medie stagione (pri,est,aut,inv)
      real*4 modul(24)           !coef.modulazione prelievi
      real*4 eps,base  !precisione macchina e base numerazione
      real*4 direz,disl,disl_min
      logical*2 eof,nullo,pmax_ancora,pmin_ancora
      character*14 lab_tipo           !**
      character*30 nomefile           !**
      character*(max_len_rec) str_rec !**
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external udm_out_str
!---------------------------------
540   format('Lung.: ',A10,' - Disl.: ',A25)
550   format('Disl.: ',A25)
560   format('Pres.min: ',A10,' - Pres.max: ',A25)
10000 format(A)
!---------------------------------
!-Definizione zero di macchina
!      eps=2.2e-16
      call zerom(eps,base)
!---------------------------------
!-Definizione dislivello minimo accettabile per elementi binari, esclusi tronchi
      disl_min = 0.045 !metri
!---------------------------------
!-Definizione Eof lettura
      eof = .false.
!---------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**1)-PUNTI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      elem_tipo = e_pu
      nomefile = rf_punti
!---------------------------------
!-definizione puntatore istante modulazione
!-per calcolo portata richiesta attraverso le modulazioni
	pos_mod= int(ist_iniz - 
     -        (int(ist_iniz/PRL_PERIODO)*PRL_PERIODO)) + 1
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_punti,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
cmar-tmin

      do iel=1,max_pu

         read (urf_file,10000,end=1000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

         prestj(pos) = pres_def  !**default pressione
         temptj(pos) = temp_def  !**default temperatura

!**-Inizio Elenco dati-**
!-
!01-pu_elem_scen_id -i*4
         call read_i(str_rec,p_i,scenario_id,nullo,*9994)

!02-pu_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-pu_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-pu_altitudine -r*4
         call read_r(str_rec,p_i,altetx(pos),nullo,*9994)

!05-pu_pmin -r*4
         call read_r(str_rec,p_i,pmintx(pos),nullo,*9994)

!06-pu_pmax -r*4
         call read_r(str_rec,p_i,pmaxtx(pos),nullo,*9994)

!07-pu_temp_pri -r*4
!08-pu_temp_est
!09-pu_temp_aut
!10-pu_temp_inv 
         do jel=1,4
            call read_r(str_rec,p_i,temp_stag(jel),nullo,*9994)
         enddo
         tambtx(pos) = temp_stag(stagione)  !*temperatura ambiente
cmar: Atenzione spostare nel tracciato record alle pos 11 e 12
!36-pu_p_min -r*4

         call read_r(str_rec,p_i,pres_min_all(pos),nullo,*9994)

	    if(nullo)then
            pres_min_all(pos) = r_und
         endif
!37-pu_p_min -r*4

         call read_r(str_rec,p_i,pres_max_all(pos),nullo,*9994)

	    if(nullo)then
            pres_max_all(pos) = r_und
         endif



!11-pucin_prelievo -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)



!*si lettura modulazioni: se fl_modul=1 (Si modulazione) e portata non zero
         if((fl_modul.eq.1).and.(porttj(pos).ne.0.))then
!12-pucin_mod_1  -r*4
!....................
!35-pucin_mod_24
            do jel=1,24
               call read_r(str_rec,p_i,modul(jel),nullo,*9994)
            enddo
         else
!*no lettura
            do jel=1,24
               modul(jel)=0.
            enddo
         endif
!--------------------------------------------------------------------
!=>Calcolo coefficienti interpolanti il prelievo 
!-ove tipo interpolazione (interpol): 1=Gradino,2=Lineare,3=Biquadratica
!--------------------------------------------------------------------
         call interp (prl_periodo,max_modul,interpol,porttj(pos),
     -        modul,eps,qsp3tq(1,pgrato(pos)),qsp2tq(1,pgrato(pos)),
     -        qsp1tq(1,pgrato(pos)),ier_interp)

         if(ier_interp.ne.0)then
            goto 9989  !*errore interpolazione
         endif

!=>Calcolo portata con modulazione
         porttj(pos) = qsp3tq(pos_mod,pgrato(pos))*ist_iniz*ist_iniz
     -               + qsp2tq(pos_mod,pgrato(pos))*ist_iniz
     -               + qsp1tq(pos_mod,pgrato(pos))



!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================


!-Lettura test per overflow max_pu 
      read (urf_file,10000,end=1000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore

!=>EOF finale
1000  continue
      close (urf_file,err=9996,iostat=stato)




cmar_log_sem_etipti

      fine_pti=etipti(e_pu)

cmar_log_sem_etipti

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**2)-TRONCHI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      elem_tipo = e_tr
      nomefile = rf_tronchi
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_tronchi,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      do iel=1,max_tr

         read (urf_file,10000,end=2000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

!**-Inizio Elenco dati-**
!-
!01-tro_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-tro_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-tro_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = pos_pu

!05-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opuvto(pos) = pos_pu

!06-tro_diametro -r*4
         call read_r(str_rec,p_i,diamtx(pos),nullo,*9994)

!07-tro_lunghezza -r*4
         call read_r(str_rec,p_i,rluntx(pos),nullo,*9994)

cmar_rev_log_sem

c          call read_r(str_rec,p_i,v_all(pos),nullo,*9994)

cmar_rev_log_sem

!08-tro_rugosita -r*4
         call read_r(str_rec,p_i,rugotx(pos),nullo,*9994)

!09-tro_cond_termica -r*4
         call read_r(str_rec,p_i,conntx(pos),nullo,*9994)

cmar_rev_log_sem

          call read_r(str_rec,p_i,v_all(pos),nullo,*9994)

	 if(nullo)then
            v_all(pos) = r_und
         endif

cmar_rev_log_sem


!-
!**-Fine Elenco dati-**

!-------------------------------------------
!->Test errore se lunghezza < dislivello
         disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
         if(rluntx(pos).lt.disl)then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
            call tipo_err(1,err_ko,str_err)
            call gest_error(3,0,' ',str_err,0)
            str_err = 'Lunghezza minore del dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,540) udm_out_str(udm_lun,rluntx(pos),0),
     *                         udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
         endif
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!-Lettura test per overflow max_tr
      read (urf_file,10000,end=2000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore

!=>EOF finale
2000  continue
      close (urf_file,err=9996,iostat=stato)

	

cmar_log_sem_etipti

      fine_tr=etipti(e_tr)
cmar_log_sem_etipti
    


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**3)-REGOLAZIONI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      elem_tipo = e_vg
      nomefile = rf_regolazioni
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_regolazioni,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      do iel=1,max_vg

         read (urf_file,10000,end=3000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

!**-Inizio Elenco dati-**
!-
!01-rg_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-rg_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-rg_elem_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = pos_pu

!05-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opuvto(pos) = pos_pu

!06-rg_num_linee -i*4
         call read_i(str_rec,p_i,line_vg(pgrato(pos)),nullo,*9994)

!07-rg_tmin_valle -r*4
         call read_r(str_rec,p_i,tsettx(pos),nullo,*9994)
cmar-tmin
       if(nullo)then
            tsettx(pos) = r_und
        endif
cmar-tmin

!08-rg_coeff_efl_m -r*4
         call read_r(str_rec,p_i,cvmnt(pgrato(pos)),nullo,*9994)

!09-rg_coeff_efl_r -r*4
         call read_r(str_rec,p_i,cvreg(pgrato(pos)),nullo,*9994)

!10-rg_pdiff_lim -r*4
         call read_r(str_rec,p_i,xtreg(pgrato(pos)),nullo,*9994)

!11-rg_diam_nom -r*4
         call read_r(str_rec,p_i,dnom_vg(pgrato(pos)),nullo,*9994)

!12-rg_vel_lim -r*4
         call read_r(str_rec,p_i,vlim_out(pgrato(pos)),nullo,*9994)

!13-rg_diam_valle_lin -r*4
         call read_r(str_rec,p_i,diam_out(pgrato(pos)),nullo,*9994)

!14-rg_diam_monte_lin -r*4
         call read_r(str_rec,p_i,diam_in(pgrato(pos)),nullo,*9994)
c Segnalazione SIRE2_0119 - Correttiva 
c In assenza dato 'Dmonte' da maschera, deve prendere 'Dvalle'
c	
         if(nullo)then
            diam_in(pgrato(pos)) = diam_out(pgrato(pos))
         endif
c
c - By Crs
c
!15-rgcin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!16-rgcin_set_pmonte -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!17-rgcin_set_pvalle -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!18-rgcin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!19-rgcin_num_linee_attive -i*4
         call read_i(str_rec,p_i,line_vg_on(pgrato(pos)),nullo,*9994)
cmar_rev_log_sem
!20-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	
	    if(nullo)then
            port_min_all(pos) = r_und
         endif
!21-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	
	    if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem
!-
!**-Fine Elenco dati-**

!-------------------------------------------
!->Test errore se presente dislivello
         disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
         if(disl.gt.disl_min)then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_log_sem

            call tipo_err(1,err_ww,str_err)

            call gest_error(3,0,' ',str_err,0)
            str_err = 'Elemento con dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,550) udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
         endif
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!-Lettura test per overflow max_vg
      read (urf_file,10000,end=3000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore

!=>EOF finale
3000  continue
      close (urf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**4)-RIDUZIONI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      elem_tipo = e_vd
      nomefile = rf_riduzioni
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_riduzioni,
     -    'OLD',stato,*9990)



!===Inizio_Loop_lettura================================
      do iel=1,max_vd

         read (urf_file,10000,end=4000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

!**-Inizio Elenco dati-**
!-
!01-rd_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-rd_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-rd_elem_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = pos_pu

!05-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opuvto(pos) = pos_pu

!06-rd_portata -r*4
         call read_r(str_rec,p_i,qprog(pgrato(pos)),nullo,*9994)

!07-rd_deltap_min -r*4
         call read_r(str_rec,p_i,dpmin(pgrato(pos)),nullo,*9994)

!08-rd_tmin_valle -r*4
         call read_r(str_rec,p_i,tsettx(pos),nullo,*9994)
cmar-tmin
        if(nullo)then
            tsettx(pos) = r_und
        endif
cmar-tmin

!09-rd_portata_max -r*4
         call read_r(str_rec,p_i,qmaxtx(pos),nullo,*9994)
!         if(nullo)then
!            qmaxtx(pos) = qprog(pgrato(pos))
!         endif

!10-rdcin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!11-rdcin_set_pmonte -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!12-rdcin_set_pvalle -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!13-rdcin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)
cmar_rev_log_sem
!14-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)

	
	    if(nullo)then
            port_min_all(pos) = r_und
         endif
!15-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	
	    if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem
!-
!**-Fine Elenco dati-**



!-------------------------------------------
!->Test errore se presente dislivello
         disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
         if(disl.gt.disl_min)then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_log_sem
            call tipo_err(1,err_ww,str_err)


            call gest_error(3,0,' ',str_err,0)
            str_err = 'Elemento con dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,550) udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
         endif
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!-Lettura test per overflow max_vd
      read (urf_file,10000,end=4000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore

!=>EOF finale
4000  continue
      close (urf_file,err=9996,iostat=stato)




!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**5)-CENTRALI_S_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      elem_tipo = e_cs
      nomefile = rf_centrali_s
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_centrali_s,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      do iel=1,max_cs

         read (urf_file,10000,end=5000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         pgrato_app(pos) = pgrato(pos)           !**
         htipto(pos) = elem_tipo                 !**

!**-Inizio Elenco dati-**
!-
!01-ces_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-ces_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-ces_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = pos_pu

!05-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opuvto(pos) = pos_pu

!06-ces_portata_max -r*4
         call read_r(str_rec,p_i,qmaxtx(pos),nullo,*9994)

!07-ces_potenza_max -r*4
         call read_r(str_rec,p_i,powmax(pgrato(pos)),nullo,*9994)

!08-ces_rendimento -r*4
         call read_r(str_rec,p_i,rendtx(pos),nullo,*9994)

!09-ces_rapp_comp_max -r*4
         call read_r(str_rec,p_i,romax(pgrato(pos)),nullo,*9994)

!10-ces_temp_ac -r*4
         call read_r(str_rec,p_i,tarctx(pos),nullo,*9994)

!11-ces_consumo -r*4
         call read_r(str_rec,p_i,ceuptx(pos),nullo,*9994)

!12-ces_perdita_asp -r*4
         call read_r(str_rec,p_i,dpin(pgrato(pos)),nullo,*9994)
cmar-dp-cs
cmar_31_07_09
	dpin_s(pgrato(pos))=dpin(pgrato(pos))/(qmaxtx(pos)**2)
cmar_31_07_09
cmar-dp-cs

!13-ces_perdita_man -r*4
         call read_r(str_rec,p_i,dpout(pgrato(pos)),nullo,*9994)

cmar-dp-cs
cmar_31_07_09
      dpout_s(pgrato(pos))=dpout(pgrato(pos))/(qmaxtx(pos)**2)
cmar_31_07_09
cmar-dp-cs


!14-ces_deltap_asp -r*4
         call read_r(str_rec,p_i,dpasp(pgrato(pos)),nullo,*9994)

!15-ces_deltap_man -r*4
         call read_r(str_rec,p_i,dpman(pgrato(pos)),nullo,*9994)

!16-cescin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!17-cescin_set_p_asp -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!18-cescin_set_p_man -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!19-cescin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)
cmar_Taria
!20-cescin_set_Taria -r*4
         call read_r(str_rec,p_i,Taria_s(pgrato(pos)),nullo,*9994)
cmar_Taria
cmar_rev_log_sem
!21-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	 if(nullo)then
            port_min_all(pos) = r_und
         endif
!22-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem
!-
!**-Fine Elenco dati-**

!-------------------------------------------
!->Test errore se presente dislivello
         disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
         if(disl.gt.disl_min)then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_log_sem
            call tipo_err(1,err_ww,str_err)


            call gest_error(3,0,' ',str_err,0)
            str_err = 'Elemento con dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,550) udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
         endif
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!-Lettura test per overflow max_cs
      read (urf_file,10000,end=5000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore

!=>EOF finale
5000  continue
      close (urf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**6)-VALVOLE_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-elem_tipo: e_vl=2, e_vc=21 (no: e_vs=14, e_vr=15) 
!---------------------------------
      iel = 0 !Contatore record
      nomefile = rf_valvole
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_valvole,
     -    'OLD',stato,*9990)



!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=6000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-vl_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-vl_tipo_elem_id -i*4
         call read_i(str_rec,p_i,elem_tipo,nullo,*9994)

!---------------------------------
!-Test su tipo elemento e su max valvole
         if(elem_tipo.eq.e_vl)then
            if((etipti(e_vl)-otipti(e_vl)+1).ge.max_vl)then
               lab_tipo='Valvole_Sem'
               goto 9992 !*errore
            endif
         elseif(elem_tipo.eq.e_vc)then
            if((etipti(e_vc)-otipti(e_vc)+1).ge.max_vc)then
               lab_tipo='Valvole_Col'
               goto 9992 !*errore
            endif
!         elseif(elem_tipo.eq.e_vs)then
!            if((etipti(e_vs)-otipti(e_vs)+1).ge.max_vs)then
!               lab_tipo='Valvole_Sez'
!               goto 9992 !*errore
!            endif
!         elseif(elem_tipo.eq.e_vr)then
!            if((etipti(e_vr)-otipti(e_vr)+1).ge.max_vr)then
!               lab_tipo='Valvole_Rid'
!               goto 9992 !*errore
!            endif
         else
            goto 9991  !*Tipo elemento errato
         endif
!---------------------------------
!-OK si procede

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

!03-vl_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!04-vl_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!05-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = pos_pu

!06-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opuvto(pos) = pos_pu

!07-vl_portata_max -r*4
         call read_r(str_rec,p_i,qmaxtx(pos),nullo,*9994)

!08-vl_temp_risc -r*4
         call read_r(str_rec,p_i,tsettx(pos),nullo,*9994)
!-Test campo nullo: se nullo -> NO RISCALDATORE
         if(nullo)then
            tsettx(pos) = r_und
         endif

!09-vlcin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!10-vlcin_set_pmonte -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!11-vlcin_set_pvalle -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!12-vlcin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)
!13-vlcin_set_portata -r*4
c         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
!14-vlcin_set_portata -r*4
c         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
cmar_rev_log_sem
!13-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	!-Test campo nullo: se nullo 
         if(nullo)then
            port_min_all(pos) = r_und
         endif

!14-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem
!-
!**-Fine Elenco dati-**


!-------------------------------------------
!->Test errore se presente dislivello
         disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
         if(disl.gt.disl_min)then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_log_sem
            call tipo_err(1,err_ww,str_err)


            call gest_error(3,0,' ',str_err,0)
            str_err = 'Elemento con dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,550) udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
         endif	    
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
6000  continue
      close (urf_file,err=9996,iostat=stato)



!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**7)-UNARI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-elem_tipo: e_im=4, e_pz=5, e_st=6, e_pr=8
!-im,pz(ent. dir.=+1) st(ent./usc. dir.=+/-1) pr(usc.dir.=-1) 
!---------------------------------
      iel = 0 !Contatore record
      nomefile = rf_unari
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_unari,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=7000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-una_elem_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-una_tipo_elem_id -i*4
         call read_i(str_rec,p_i,elem_tipo,nullo,*9994)

!---------------------------------
!-Test sul tipo elemento e sui relativi max unari
         if(elem_tipo.eq.e_im)then
            direz = 1   !**default entrante
            if((etipti(e_im)-otipti(e_im)+1).ge.max_im)then
               lab_tipo='Importazioni'
               goto 9992 !*errore
            endif
         elseif(elem_tipo.eq.e_pz)then
            direz = 1   !**default entrante
            if((etipti(e_pz)-otipti(e_pz)+1).ge.max_pz)then
               lab_tipo='Pozzi'
               goto 9992 !*errore
            endif
         elseif(elem_tipo.eq.e_st)then
            direz = 1   !**default entrante
            if((etipti(e_st)-otipti(e_st)+1).ge.max_st)then
               lab_tipo='Stoccaggi'
               goto 9992 !*errore
            endif
         elseif(elem_tipo.eq.e_pr)then
            direz = -1  !**default uscente
            if((etipti(e_pr)-otipti(e_pr)+1).ge.max_pr)then
               lab_tipo='Prelievi'
               goto 9992 !*errore
            endif
         else
            goto 9991  !*Tipo elemento errato
         endif
!---------------------------------
!-OK si procede

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**

!03-una_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!04-una_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!05-punto -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos punto
         call cerca_connessione(i_val,pos_pu,*9993)
         opumto(pos) = off_oi
         opuvto(pos) = pos_pu

!06-una_portata_max_in -r*4
         call read_r(str_rec,p_i,qmintx(pos),nullo,*9994)

!07-una_portata_max_out -r*4
         call read_r(str_rec,p_i,qmoutx(pos),nullo,*9994)

!08-una_pmax -r*4
         call read_r(str_rec,p_i,pmaxtx(pos),nullo,*9994)
         if(nullo)then
            pmaxtx(pos) = pmaxtx(opuvto(pos)) !**pmax da punto ancoraggio
            pmax_ancora = .true.
         else
            pmax_ancora = .false.
         endif

!09-una_pmin -r*4
         call read_r(str_rec,p_i,pmintx(pos),nullo,*9994)
         if(nullo)then
            pmintx(pos) = pmintx(opuvto(pos)) !**pmin da punto ancoraggio
            pmin_ancora = .true.
         else
            pmin_ancora = .false.
         endif
!-------------------------------------------
!->Test errore se pmin > pmax
         if(pmintx(pos).gt.pmaxtx(pos))then
            call gest_error(3,0,' ',' ',0)
	      str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_rev_log_sem            call tipo_err(1,err_ko,str_err)
             call tipo_err(1,err_ww,str_err)
            call gest_error(3,0,' ',str_err,0)
            if(pmax_ancora)then
               str_err = 'Pressione minima di elemento > '//
     *                   'Pressione massima su punto ancoraggio'
            else
               str_err = 'Pressione minima su punto ancoraggio > '//
     *                   'Pressione massima di elemento'
            endif            
            call gest_error(3,0,' ',str_err,0)
            write(str_err,560) udm_out_str(udm_P,pmintx(pos),0),
     *                         udm_out_str(udm_P,pmaxtx(pos),1)
            call gest_error(3,0,' ',str_err,0)
         endif
!-------------------------------------------

!10-uncin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!11-uncin_set_pressione -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!12-uncin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!13-uncin_temp -r*4
         call read_r(str_rec,p_i,tsettj(pos),nullo,*9994)
         if(nullo)then
            tsettj(pos) = temptj(opuvto(pos)) !**default temperatura punto
         endif

!14-uncin_direz -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!---------------------------------
!=>Definizione direzione
         diretj(pos) = direz
         if(elem_tipo.eq.e_st)then
            if(i_val.eq.-1)then
               diretj(pos) = -1 !**default uscente
            endif
         endif
!---------------------------------

!15-uncin_metano -r*4
!16-uncin_etano
!17-uncin_propano
!18-uncin_nbutano
!19-uncin_ibutano
!20-uncin_pentano
!21-uncin_esano
!22-uncin_azoto
!23-uncin_co2
!24-uncin_h2s
         do jel=1,10
            call read_r(str_rec,p_i,comptc(pos,jel),nullo,*9994)
         enddo

cmar_rev_log_sem
!25-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	 if(nullo)then
            port_min_all(pos) = r_und
         endif
!26-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
7000  continue
      close (urf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**8)-STACCO_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-2 elem_tipo: e_vt(valvola stacco), e_ut(unario stacco) direzione=1/-1 entrante/uscente 
!---------------------------------
      iel = 0 !Contatore record
      nomefile = rf_stacco
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_stacco,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=8000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-sta_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-sta_id -i*4
         call read_i(str_rec,p_i,sta_id,nullo,*9994)

!03-punto -i*4 (vale opumto se valvola o opuvto se unario)
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!-cerca pos di punto
         call cerca_connessione(i_val,pos_pu,*9993)
         punto = pos_pu

!04-punto_conn -i*4 (vale opuvto se valvola o nullo se unario)
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!-definizione del tipo elemento di stacco
         elem_tipo = e_ut  !*pongo unario stacco

!-cerca pos di punto_conn (no esiste -> unario)
         call cerca_connessione(i_val,pos_pu,*7200)
!-si esiste punto valle -> valvola stacco
         punto_conn = pos_pu
         elem_tipo = e_vt  !*è valvola stacco

7200     continue
!-Test su max valvole stacco
         if(elem_tipo.eq.e_vt)then
            if((etipti(e_vt)-otipti(e_vt)+1).ge.max_vt)then
               lab_tipo='Valvole stacco'
               goto 9992 !*errore
            endif
!-Test su max unari stacco
         else
            if((etipti(e_ut)-otipti(e_ut)+1).ge.max_ut)then
               lab_tipo='Unari stacco'
               goto 9992 !*errore
            endif
         endif
!---------------------------------
!-OK si procede

         etipti(elem_tipo) = etipti(elem_tipo)+1 !**
         pos = etipti(elem_tipo)                 !**
         pgrato(pos) = pos-otipti(elem_tipo)+1   !**
         htipto(pos) = elem_tipo                 !**
         elem_id(pos)= sta_id                    !**
         snomtv(pos) = snomtv(punto)             !**

         if(elem_tipo.eq.e_vt)then
            opumto(pos) = punto
            opuvto(pos) = punto_conn
            htcitj(pos) = tc_on   !*valore Sire
            qmaxtx(pos) = 30000.  !*
            tsettx(pos) = r_und   !*
            pmsetj(pos) = 0       !*
            pvsetj(pos) = 0       !*
            qsettj(pos) = 0       !*
            goto 7500
         else
            opumto(pos) = off_oi
            opuvto(pos) = punto
         endif

!05-sta_portata_max_in -r*4
         call read_r(str_rec,p_i,qmintx(pos),nullo,*9994)

!06-sta_portata_max_out -r*4
         call read_r(str_rec,p_i,qmoutx(pos),nullo,*9994)

!07-sta_pmax -r*4
         call read_r(str_rec,p_i,pmaxtx(pos),nullo,*9994)

!08-sta_pmin -r*4
         call read_r(str_rec,p_i,pmintx(pos),nullo,*9994)

!09-stacin_tipo_reg_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!10-stacin_set_pressione -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!11-stacin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!12-stacin_temp -r*4
         call read_r(str_rec,p_i,tsettj(pos),nullo,*9994)
         if(nullo)then
            tsettj(pos) = temptj(opuvto(pos)) !**default temperatura punto
         endif

!13-stacin_direz -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!---------------------------------
!=>Definizione direzione
         if(i_val.eq.-1)then
            diretj(pos) = -1 !**default uscente
         else
            diretj(pos) =  1 !**default entrante
         endif
!---------------------------------

!14-stacin_metano -r*4
!15-stacin_etano
!16-stacin_propano
!17-stacin_nbutano
!18-stacin_ibutano
!19-stacin_pentano
!20-stacin_esano
!21-stacin_azoto
!22-stacin_co2
!23-stacin_h2s
         do jel=1,10
            call read_r(str_rec,p_i,comptc(pos,jel),nullo,*9994)
         enddo
cmar_rev_log_sem
!24-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	 if(nullo)then
            port_min_all(pos) = r_und
         endif
!25-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif

cmar_rev_log_sem

7500  continue
!-
!**-Fine Elenco dati-**

!-------------------------------------------
!->Test errore se presente dislivello
         if(elem_tipo.eq.e_vt)then
          disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
          if(disl.gt.disl_min)then
            call gest_error(3,0,' ',' ',0)
            str_err=trim(te_stri(htipto(pos)))//' : '//trim(snomtv(pos))  
            call gest_error(3,0,' ',str_err,0)
cmar_log_sem
            call tipo_err(1,err_ww,str_err)

            call gest_error(3,0,' ',str_err,0)
            str_err = 'Elemento con dislivello'
            call gest_error(3,0,' ',str_err,0)
            write(str_err,550) udm_out_str(udm_lun,disl,1)
            call gest_error(3,0,' ',str_err,0)
          endif
         endif
!-------------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
8000  continue
      close (urf_file,err=9996,iostat=stato)

!-------------------------------------------
!->Test uscita per errore riscontrato prima 
	if(fl_ris_gen.eq.err_ko)then
         str_err=' '
         goto 9999
      endif
!-------------------------------------------
!->Uscita OK
      return
!-------------------------------------------
!->Errori
9989  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Interpolazione errata : '//snomtv(pos)(1:pf2)
      goto 9998
9990  continue
      str_err = 'Open errata'
      goto 9998
9991  continue
      stato=0
      call msgerr_key (iel,'Tipo Elemento errato in Record:',str_err)
      goto 9998
9992  continue
      stato=0
      str_err = 'Superato Max '//lab_tipo
      goto 9998
9993  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Connessione errata : '//snomtv(pos)(1:pf2)
      goto 9998
9994  continue
      stato=0
9995  continue
      call msgerr_key (iel,'Record errato:',str_err)
      goto 9998
9996  continue
      str_err = 'Close errata'
      goto 9998
9997  continue
      stato=0
      str_err = 'Superato Max elementi'
9998  continue
      pf1 = s_trim_d(nomefile)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_elemrete ',
     - nomefile(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (urf_file,err=9999)
9999  continue 
      return 1
      end
!==============================================================
      subroutine leggi_centrali (str_err,*)

!*-Legge TOP,CIN per le Centrali (SI SIMCENT, SI RETE):
!1)centrali_in.txt    (DB: v_centrali_simcent) e_ce
!2)assetti_in.txt     (DB: v_assetti_simcent)  e_ce
!3)turbogruppi_in.txt (DB: v_turbogruppi)
!4)punti_lim_in.txt   (DB: v_punti_lim)
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tv.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/tj.inc'
      include '../inc/tc.inc'
      include '../inc/tipo_reg.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/filtri.inc'
      include '../inc/air_cooler.inc'
      include '../inc/default.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_types.inc'
      include '../inc/mf_units.inc'
      include '../inc/err.inc'
      include '../inc/conv.inc'
	include '../inc/ASS_COND.INC'


cmar_pow_stat
	 include '../inc/power.INC'
cmar_pow_stat
cmar_rev_log_sem

      include '../inc/allarmi.INC'
cmar_rev_log_sem
c

      character*(*) str_err  !IO

      integer*4 pos,p_i,stato,iel,jel,i_val,pos_pu
      integer*4 kel,prec_pos,etipti_ini,pf1,pf2,s_trim_d
      integer*4 ce_fil_un_id,ce_cool_un_id,cecin_fl_cri_manuale
      integer*4 ass_fil_un_id,ass_cool_un_id
      integer*4 ass_fil_un_id2,ass_cool_un_id2,asscin_fl_cri_manuale
	integer*4 fccin_filtro2_stato,fccin_cool2_stato
      integer*4 elemId,compId,tgcompId,kun,akun,pkun,skun
      integer*4 tbg_tipo_uso,tbgcin_stadio,tbg_fil_un_id,tbg_cool_un_id
      integer*4 tbg_cool_un_id2,cmp_id,cmp_id2,plim_cmp_id,plim_prg
      real*4 asscin_p_asp,asscin_p_man,asscin_t_asp
      real*4 ass_pmax_man2,ass_tmax_man2,ass_portata2,ass_deltap_asp2
      real*4 ass_deltap_man2,ass_deltat_man2
	real*4 fil_portata2,fil_perdita_p2,cool_portata2
      real*4 cool_caduta_p2,cool_temp_rif2,cool_calore_scambiato2
      real*4 plim_num_giri,plim_portata,plim_prevalenza
      real*4 plim_alim,plim_blim
      logical*2 bInsMonte,bInsValle
      logical*2 eof,nullo,presente_ass
      character*30 nomefile           !**
      character*(max_len_rec) str_rec !**
!------------------------------------------
!-Dati_uso_interno: turbogruppi e punti_lim
      real*4 cin_num_giri_max(max_vert),
     -       cin_potenza_min(max_vert),cin_potenza_max(max_vert)
      integer*4 ass_comp_tipo_uso(max_vert),ass1p_pos(max_vert)
      integer*4 tbg_num,tbg_kun(max_unit),tbg_akun(max_unit),
     -          tbg_cmp_id(max_unit),tbg_cmp_id2(max_unit)

CCCC	LOGICAL*2 FLAG_ASS_COND(M_OGG)

      integer*4 a_a, a_m(20), a_n, j_m
!------------------------------------------
      real*4 disl,disl_min
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external udm_out_str

      REAL*4 FP




!---------------------------------
550   format('Disl.: ',A25)
10000 format(A)
!------------------------------------------
!-Definizione dislivello minimo accettabile per centrali
      disl_min = 0.045 !metri
!---------------------------------
!-Definizione Eof lettura
      eof = .false.









C-----------------------------------------------------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**1)-CENTRALI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = rf_centrali
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_centrali,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      do iel=1,max_ce

         read (urf_file,10000,end=1000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

         etipti(e_ce) = etipti(e_ce)+1    !**
         pos = etipti(e_ce)               !**
         pgrato(pos) = pos-otipti(e_ce)+1 !**
         pgrato_app(pos) = pgrato(pos)    !**
         htipto(pos) = e_ce               !**

         ass_comp_id(pgrato(pos)) = 0       !**bipolare
         assetto_cm (pgrato(pos)) = 0       !** "
         ass_comp_tipo_uso(pgrato(pos)) = 0 !** "
         ass1p_pos(pgrato(pos)) = 0         !** "
         tipo_fase  (pgrato(pos)) = fase_1  !**
         unit_num   (pgrato(pos)) = 0       !**
         first_unit (pgrato(pos)) = (pgrato(pos)-1)*maxunits+1 !**
         first_type (pgrato(pos)) = first_unit(pgrato(pos))    !**
     
!**-Inizio Elenco dati-**
!-
!01-cecin_scen_id -i*4 per Simcent
!01-ce_elem_scen_id -i*4 per Rete
         call read_i(str_rec,p_i,scenario_id,nullo,*9994)

!02-ce_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-ce_elem_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-ce_elem_altitudine -r*4
         call read_r(str_rec,p_i,altit(pgrato(pos)),nullo,*9994)

!05-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!->Se Rete: cerca pos punto (per Simcent fittizio)
         if(tipoScenario.ne.TIPOSCEN_CE)then
            call cerca_connessione(i_val,pos_pu,*9993)
            opumto(pos) = pos_pu
         endif

!06-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!->Se Rete: cerca pos punto (per Simcent fittizio)
         if(tipoScenario.ne.TIPOSCEN_CE)then
            call cerca_connessione(i_val,pos_pu,*9993)
            opuvto(pos) = pos_pu
         endif

!07-ce_criterio -i*4
         call read_i(str_rec,p_i,tipo_criterio(pgrato(pos)),nullo,*9994)

!08-ce_pmax_man -r*4
         call read_r(str_rec,p_i,max_pout(pgrato(pos)),nullo,*9994)

!09-ce_tmax_man -r*4
         call read_r(str_rec,p_i,max_tout(pgrato(pos)),nullo,*9994)

!10-ce_portata -r*4
         call read_r(str_rec,p_i,flow_prog(pgrato(pos)),nullo,*9994)

!11-ce_deltap_asp -r*4
         call read_r(str_rec,p_i,delpin(pgrato(pos)),nullo,*9994)

!12-ce_deltap_man -r*4
         call read_r(str_rec,p_i,delpout(pgrato(pos)),nullo,*9994)

!13-ce_deltat_man -r*4
         call read_r(str_rec,p_i,dtout(pgrato(pos)),nullo,*9994)

!14-ce_fil_un_id -i*4
         call read_i(str_rec,p_i,ce_fil_un_id,nullo,*9994)

!15-ce_cool_un_id -i*4
         call read_i(str_rec,p_i,ce_cool_un_id,nullo,*9994)

!16-cod_reg -htcitj i*4 per Simcent
!16-cecin_tipo_set  i*4 per Rete
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         if(tipoScenario.eq.TIPOSCEN_CE)then
            htcitj(pos) = i_val                           !se Simcent coincide
         else
            call conv_tiporeg_db_sire (i_val,htcitj(pos)) !se Rete converte 
         endif

!17-cecin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!18-cecin_set_p_asp -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!19-cecin_set_p_man -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!20-cecin_t_aria -r*4
         call read_r(str_rec,p_i,taria(pgrato(pos)),nullo,*9994)

!21-cecin_t_asp -r*4
         call read_r(str_rec,p_i,tasp_stat(pgrato(pos)),nullo,*9994)

!22-cecin_tipo_uso -i*4
         call read_i(str_rec,p_i,stat_ord(pgrato(pos)),nullo,*9994)

!23-cecin_num_giri_max -r*4
         call read_r(str_rec,p_i,cin_num_giri_max(pgrato(pos)),
     -   nullo,*9994)

!24-cecin_potenza_min -r*4
         call read_r(str_rec,p_i,cin_potenza_min(pgrato(pos)),
     -   nullo,*9994)


!25-cecin_potenza_max su -r*4
         call read_r(str_rec,p_i,cin_potenza_max(pgrato(pos)),
     -   nullo,*9994)

!25-bis-cecin_potenza_min -r*4
         call read_r(str_rec,p_i,FP,nullo,*9994)


	IF(FP.EQ.0)THEN
      FLAG_POW(POS)=.FALSE.
	ELSEIF (FP.EQ.1)THEN
      FLAG_POW(POS)=.TRUE.
      ENDIF


!26-fccin_filtro1_stato -i*4
         call read_i(str_rec,p_i,c_f_ce_stat(pgrato(pos)),nullo,*9994)

!27-fccin_cool1_stato -i*4
         call read_i(str_rec,p_i,c_ac_ce_stat(pgrato(pos)),nullo,*9994)

!28-fil_portata -r*4
         call read_r(str_rec,p_i,c_f_ce_flow_prog(pgrato(pos)),
     -   nullo,*9994)

!29-fil_perdita_p -r*4
         call read_r(str_rec,p_i,c_f_ce_deltap(pgrato(pos)),
     -   nullo,*9994)

!30-cool_portata -r*4
         call read_r(str_rec,p_i,c_ac_ce_flow_prog(pgrato(pos)),
     -   nullo,*9994)

!31-cool_caduta_p -r*4
         call read_r(str_rec,p_i,c_ac_ce_deltap(pgrato(pos)),
     -   nullo,*9994)

!32-cool_temp_rif -r*4
         call read_r(str_rec,p_i,c_ac_taria_prog(pgrato(pos)),
     -   nullo,*9994)


cmar_rev_log_sem


      if(tipoScenario.NE.TIPOSCEN_CE)then
!34-pu_p_min -r*4

         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994) 
	   if(nullo)then
            port_min_all(pos) = r_und
         endif

!35-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif



       ENDIF
cmar_rev_log_sem

c--------------------------- spostato 
	
!33-cool_calore_scambiato -r*4
         call read_r(str_rec,p_i,c_ac_q_prog(pgrato(pos)),
     -   nullo,*9994)


CMAR-CRIT_MAN SPOSTATO PER EVO
!33-BIS -cecin_fl_cri_manuale -i*4
            call read_i(str_rec,p_i,cecin_fl_cri_manuale,nullo,*9994)
CMAR-CRIT_MAN

c---------------------------
!->Se Simcent
         if(tipoScenario.eq.TIPOSCEN_CE)then

!34-cecin_p_asp -r*4
            call read_r(str_rec,p_i,pasp_stat(pgrato(pos)),nullo,*9994)

!35-cecin_p_man -r*4
            call read_r(str_rec,p_i,pmand_stat(pgrato(pos)),nullo,*9994)

!36-cecin_portata -r*4
            call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

CMAR-CRIT_MAN !37-cecin_fl_cri_manuale -i*4
CMAR-CRIT_MAN            call read_i(str_rec,p_i,cecin_fl_cri_manuale,nullo,*9994)

!38-cecin_metano -r*4
!39-cecin_etano
!40-cecin_propano
!41-cecin_nbutano
!42-cecin_ibutano
!43-cecin_pentano
!44-cecin_esano
!45-cecin_azoto
!46-cecin_co2
!47-cecin_h2s
            do jel=1,10
               call read_r(str_rec,p_i,comptc(pos,jel),nullo,*9994)
            enddo



!->Se Rete
         else

            pasp_stat(pgrato(pos))=0.  !34-cecin_p_asp
            pmand_stat(pgrato(pos))=0. !35-cecin_p_man
            porttj(pos)=0             !36-cecin_portata


            do jel=1,10                !38-47 cecin-composizioni
               comptc(pos,jel)=0.
            enddo

         endif
!-
!**-Fine Elenco dati-**

!--------------------------------------
!=>Definizione altri Dati
!--------------------------------------
         flow_stat (pgrato(pos)) = porttj(pos)
         tmand_stat(pgrato(pos)) = temp_def

         if(cecin_fl_cri_manuale.gt.0)then
            tipo_criterio(pgrato(pos)) = crit_man
         endif

!-Se manca filtro: Pulizia campi filtro
         if(ce_fil_un_id.eq.0)then
            c_f_ce_stat(pgrato(pos))=0
            c_f_ce_flow_prog(pgrato(pos))=0.
            c_f_ce_deltap(pgrato(pos))=0.
         endif
!-Se manca air_cooler: Pulizia campi cool
         if(ce_cool_un_id.eq.0)then
            c_ac_ce_stat(pgrato(pos))=0
            c_ac_ce_flow_prog(pgrato(pos))=0.
            c_ac_ce_deltap(pgrato(pos))=0.
            c_ac_taria_prog(pgrato(pos))=0.
            c_ac_q_prog(pgrato(pos))=0.
         endif

!=>Se_Simcent
         if(tipoScenario.eq.TIPOSCEN_CE)then
            call connPuntoFittizio(.true.,.true. ,pos,pgrato(pos))
            call connPuntoFittizio(.true.,.false.,pos,pgrato(pos))
            close (urf_file,err=9996,iostat=stato)
!*Una sola centrale ammessa per Simcent
            goto 2050 !*Si va direttamente ai dati di Turbogruppi
         else
!cccccccccccccccccccccccccccccccccccccc
!=>Se Rete
!->Test errore se presente dislivello
            disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
            if(disl.gt.disl_min)then
              call gest_error(3,0,' ',' ',0)
	        str_err=trim(te_stri(htipto(pos)))
     -        //' : '//trim(snomtv(pos))  
              call gest_error(3,0,' ',str_err,0)
cmar_log_sem 
             call tipo_err(1,err_ww,str_err)

              call gest_error(3,0,' ',str_err,0)
              str_err = 'Elemento con dislivello'
              call gest_error(3,0,' ',str_err,0)
              write(str_err,550) udm_out_str(udm_lun,disl,1)
              call gest_error(3,0,' ',str_err,0)
            endif
!cccccccccccccccccccccccccccccccccccccc
         endif
!--------------------------------------
      
      enddo
!===Fine_Loop_lettura==================================

!-Lettura per overflow
      read (urf_file,10000,end=1000,err=9997,iostat=stato) str_rec
      goto 9997 !*errore




!=>EOF finale
1000  continue
      close (urf_file,err=9996,iostat=stato)








!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**2)-ASSETTI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
ccc      ASSCIN_ASS_COMP_COND_ID(13)=77
      
      iel = 0 !Contatore record
      nomefile = rf_assetti
!---------------------------------
!-Definizione etipti iniziale per le multifunzioni
      etipti_ini = etipti(e_ce)+1
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_assetti,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=2000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!---------------------------------
!-Test su max (centrali+multifunzioni), essendo lette prima le bipolari
         if((etipti(e_ce)-otipti(e_ce)+1).ge.max_ce)then
            goto 9997 !*errore
         endif
!---------------------------------

         etipti(e_ce) = etipti(e_ce)+1        !**
         pos = etipti(e_ce)                   !**
         pgrato(pos) = pos-otipti(e_ce)+1     !**
         if(pgrato(pos).ne.1)then
            pgrato(pos) = pgrato_app(pos-1)+1 !**
         endif
         pgrato_app(pos) = pgrato(pos)        !**
         htipto(pos) = e_ce                   !**

         unit_num  (pgrato(pos)) = 0          !**
         first_unit(pgrato(pos)) = (pgrato(pos)-1)*maxunits+1 !**
         first_type(pgrato(pos)) = first_unit(pgrato(pos))    !**

!**-Inizio Elenco dati-**
!-
!01-asscin_scen_id -i*4 per Simcent
!01-ass_scen_id    -i*4 per Rete
         call read_i(str_rec,p_i,scenario_id,nullo,*9994)

!02-ce_elem_id -i*4
         call read_i(str_rec,p_i,elem_id(pos),nullo,*9994)

!03-ce_elem_nome -c*(tit_len)
         call read_c(str_rec,p_i,snomtv(pos),*9994)

!04-ce_elem_altitudine -r*4
         call read_r(str_rec,p_i,altit(pgrato(pos)),nullo,*9994)

!05-ass_comp_id -i*4
         call read_i(str_rec,p_i,ass_comp_id(pgrato(pos)),
     -   nullo,*9994)

!06-ass_comp_tipo_uso -i*4 (0/1=Parallelo/Serie)
         call read_i(str_rec,p_i,ass_comp_tipo_uso(pgrato(pos)),
     -   nullo,*9994)

!07-ass_comp_nome -c*(tit_len)
         call read_c(str_rec,p_i,sigla_ass(pgrato(pos)),*9994)

!08-punto_monte -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!->Se Rete: cerca pos punto (per Simcent fittizio)
         if(tipoScenario.ne.TIPOSCEN_CE)then
            call cerca_connessione(i_val,pos_pu,*9993)
            opumto(pos) = pos_pu
         endif

!09-punto_valle -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
!->Se Rete: cerca pos punto (per Simcent fittizio)
         if(tipoScenario.ne.TIPOSCEN_CE)then
            call cerca_connessione(i_val,pos_pu,*9993)
            opuvto(pos) = pos_pu
         endif

!10-ce_criterio -i*4
         call read_i(str_rec,p_i,tipo_criterio(pgrato(pos)),
     -   nullo,*9994)

!11-ass_pmax_man -r*4
         call read_r(str_rec,p_i,max_pout(pgrato(pos)),nullo,*9994)

!12-ass_tmax_man -r*4
         call read_r(str_rec,p_i,max_tout(pgrato(pos)),nullo,*9994)

!13-ass_portata -r*4
         call read_r(str_rec,p_i,flow_prog(pgrato(pos)),nullo,*9994)

!14-ass_deltap_asp -r*4
         call read_r(str_rec,p_i,delpin(pgrato(pos)),nullo,*9994)

!15-ass_deltap_man -r*4
         call read_r(str_rec,p_i,delpout(pgrato(pos)),nullo,*9994)

!16-ass_deltat_man -r*4
         call read_r(str_rec,p_i,dtout(pgrato(pos)),nullo,*9994)

!17-ass_fil_un_id -i*4
         call read_i(str_rec,p_i,ass_fil_un_id,nullo,*9994)

!18-ass_cool_un_id -i*4
         call read_i(str_rec,p_i,ass_cool_un_id,nullo,*9994)

!19-ass_pmax_man2 -r*4
         call read_r(str_rec,p_i,ass_pmax_man2,nullo,*9994)

!20-ass_tmax_man2 -r*4
         call read_r(str_rec,p_i,ass_tmax_man2,nullo,*9994)

!21-ass_portata2 -r*4
         call read_r(str_rec,p_i,ass_portata2,nullo,*9994)

!22-ass_deltap_asp2 -r*4
         call read_r(str_rec,p_i,ass_deltap_asp2,nullo,*9994)

!23-ass_deltap_man2 -r*4
         call read_r(str_rec,p_i,ass_deltap_man2,nullo,*9994)

!24-ass_deltat_man2 -r*4
         call read_r(str_rec,p_i,ass_deltat_man2,nullo,*9994)

!25-ass_fil_un_id2 -i*4
         call read_i(str_rec,p_i,ass_fil_un_id2,nullo,*9994)

!26-ass_cool_un_id2 -i*4
         call read_i(str_rec,p_i,ass_cool_un_id2,nullo,*9994)

!27-ascl_coll_comp_id_asp -i*4
         call read_i(str_rec,p_i,ass_coll_asp(pgrato(pos)),
     -   nullo,*9994)


!28-ascl_coll_comp_id_man -i*4
         call read_i(str_rec,p_i,ass_coll_man(pgrato(pos)),
     -   nullo,*9994)

!29-cod_reg -htcitj i*4 per Simcent
!29-asscin_tipo_set i*4 per Rete
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         if(tipoScenario.eq.TIPOSCEN_CE)then
            htcitj(pos) = i_val                           !se Simcent coincide
         else
            call conv_tiporeg_db_sire (i_val,htcitj(pos)) !se Rete converte
         endif

     
!30-asscin_set_portata -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!31-asscin_set_p_asp -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!32-asscin_set_p_man -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!33-asscin_t_aria -r*4
         call read_r(str_rec,p_i,taria(pgrato(pos)),nullo,*9994)

!34-asscin_t_asp -r*4
         call read_r(str_rec,p_i,asscin_t_asp,nullo,*9994)

!35-asscin_tipo_uso -i*4
         call read_i(str_rec,p_i,stat_ord(pgrato(pos)),nullo,*9994)

!36-asscin_num_giri_max -r*4
         call read_r(str_rec,p_i,cin_num_giri_max(pgrato(pos)),
     -   nullo,*9994)

!37-asscin_potenza_min -r*4
         call read_r(str_rec,p_i,cin_potenza_min(pgrato(pos)),
     -   nullo,*9994)

!38-asscin_potenza_max -r*4
         call read_r(str_rec,p_i,cin_potenza_max(pgrato(pos)),
     -   nullo,*9994)

!38-bis-cecin_potenza_min -r*4
          call read_r(str_rec,p_i,FP,nullo,*9994)


	IF(FP.EQ.0)THEN
      FLAG_POW(POS)=.FALSE.
	ELSEIF (FP.EQ.1)THEN
      FLAG_POW(POS)=.TRUE.
      ENDIF

!39-fccin_filtro1_stato -i*4
         call read_i(str_rec,p_i,c_f_ce_stat(pgrato(pos)),
     -   nullo,*9994)

!40-fccin_filtro2_stato -i*4
         call read_i(str_rec,p_i,fccin_filtro2_stato,nullo,*9994)

!41-fccin_cool1_stato -i*4
         call read_i(str_rec,p_i,c_ac_ce_stat(pgrato(pos)),
     -   nullo,*9994)

!42-fccin_cool2_stato -i*4
         call read_i(str_rec,p_i,fccin_cool2_stato,nullo,*9994)

!43-fil_portata -r*4
         call read_r(str_rec,p_i,c_f_ce_flow_prog(pgrato(pos)),
     -   nullo,*9994)

!44-fil_perdita_p -r*4
         call read_r(str_rec,p_i,c_f_ce_deltap(pgrato(pos)),
     -   nullo,*9994)

!45-cool_portata -r*4
         call read_r(str_rec,p_i,c_ac_ce_flow_prog(pgrato(pos)),
     -   nullo,*9994)

!46-cool_caduta_p -r*4
         call read_r(str_rec,p_i,c_ac_ce_deltap(pgrato(pos)),
     -   nullo,*9994)

!47-cool_temp_rif -r*4
         call read_r(str_rec,p_i,c_ac_taria_prog(pgrato(pos)),
     -   nullo,*9994)

!48-cool_calore_scambiato -r*4
         call read_r(str_rec,p_i,c_ac_q_prog(pgrato(pos)),
     -   nullo,*9994)

!49-fil_portata2 -r*4
         call read_r(str_rec,p_i,fil_portata2,nullo,*9994)

!50-fil_perdita_p2 -r*4
         call read_r(str_rec,p_i,fil_perdita_p2,nullo,*9994)

!51-cool_portata2 -r*4
         call read_r(str_rec,p_i,cool_portata2,nullo,*9994)

!52-cool_caduta_p2 -r*4
         call read_r(str_rec,p_i,cool_caduta_p2,nullo,*9994)

!53-cool_temp_rif2 -r*4
         call read_r(str_rec,p_i,cool_temp_rif2,nullo,*9994)

!54-cool_calore_scambiato2 -r*4
         call read_r(str_rec,p_i,cool_calore_scambiato2,nullo,*9994)
cmar_rev_log_sem
!55-pu_p_min -r*4
ccc
          if(tipoScenario.ne.TIPOSCEN_CE)then


         call read_r(str_rec,p_i,port_min_all(pos),nullo,*9994)
	 if(nullo)then
            port_min_all(pos) = r_und
         endif
!56-pu_p_min -r*4

         call read_r(str_rec,p_i,port_max_all(pos),nullo,*9994)
	 if(nullo)then
            port_max_all(pos) = r_und
         endif

	

         endif
ccc

cmar_rev_log_sem



c55
cccmar_____      call read_i(str_rec,p_i,a_n,nullo,*9994)



cccmar_____	call read_i(str_rec,p_i,a_n,nullo,*9994)


cccmar_____	call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	call read_i(str_rec,p_i,a_n,nullo,*9994)

	

cccmar_____	call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

!61	
cccmar_____	        call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)


cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)


cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	call read_i(str_rec,p_i,a_n,nullo,*9994)

	
cccmar_____	      call read_i(str_rec,p_i,a_n,nullo,*9994)

	

CMAR-CRIT_MAN SPOSTATO PER EVO
!54-BIS-asscin_fl_cri_manuale -i*4
            call read_i(str_rec,p_i,asscin_fl_cri_manuale,nullo,*9994)


!->Se Simcent
         if(tipoScenario.eq.TIPOSCEN_CE)then

!55-asscin_p_asp -r*4
            call read_r(str_rec,p_i,asscin_p_asp,nullo,*9994)

!56-asscin_p_man -r*4
            call read_r(str_rec,p_i,asscin_p_man,nullo,*9994)

!57-asscin_portata -r*4
            call read_r(str_rec,p_i,porttj(pos),nullo,*9994)
CMAR-CRIT_MAN
!58-asscin_fl_cri_manuale -i*4
CMAR-CRIT_MAN            call read_i(str_rec,p_i,asscin_fl_cri_manuale,nullo,*9994)
CMAR-CRIT_MAN

!59-asscin_metano -r*4
!60-asscin_etano
!61-asscin_propano
!62-asscin_nbutano
!63-asscin_ibutano
!64-asscin_pentano
!65-asscin_esano
!66-asscin_azoto
!67-asscin_co2
!68-asscin_h2s
            do jel=1,10
               call read_r(str_rec,p_i,comptc(pos,jel),nullo,*9994)
            enddo
!69
 	call read_i(str_rec,p_i,a_n,nullo,*9994)
	

!70
cccmar____
	      call read_i(str_rec,p_i,ASSCIN_ASS_COMP_COND_ID(pgrato(pos))
     *                   ,nullo,*9994)
	

!->Se Rete
         else

            asscin_p_asp=0.         !55-asscin_p_asp
            asscin_p_man=0.         !56-asscin_p_man
            porttj(pos)=0.          !57-asscin_portata
CMAR-CRIT_MAN            asscin_fl_cri_manuale=0 !58-asscin_fl_cri_manuale
            do jel=1,10             !59-68 cecin-composizioni
               comptc(pos,jel)=0.
            enddo
	      do j_m=1,14
c            
             call read_r(str_rec,p_i,a_m(j_m),nullo,*9994)
c	       write(117,*)j_m,a_m(j_m)
            enddo
            
	call read_i(str_rec,p_i,ASSCIN_ASS_COMP_COND_ID(pgrato(pos))
     *                   ,nullo,*9994)


            
         endif
cmar_ass_cond
!69-ASSCIN_ASS_COMP_COND_ID -i*4
c            call read_i(str_rec,p_i,ASSCIN_ASS_COMP_COND_ID(pgrato(pos))
c     *                   ,nullo,*9994)



	




cmar_ass_cond
!-
!**-Fine Elenco dati-**

!cccccccccccccccccccccccccccccccccccccc
!=>Se Rete
         if(tipoScenario.ne.TIPOSCEN_CE)then
!->Test errore se presente dislivello
            disl = abs(altetx(opuvto(pos))-altetx(opumto(pos)))
            if(disl.gt.disl_min)then
              call gest_error(3,0,' ',' ',0)
	        str_err=trim(te_stri(htipto(pos)))
     -        //' : '//trim(snomtv(pos))  
              call gest_error(3,0,' ',str_err,0)
cmar_log_sem
            call tipo_err(1,err_ww,str_err)


              call gest_error(3,0,' ',str_err,0)
              str_err = 'Elemento con dislivello'
              call gest_error(3,0,' ',str_err,0)
              write(str_err,550) udm_out_str(udm_lun,disl,1)
              call gest_error(3,0,' ',str_err,0)
            endif
         endif
!cccccccccccccccccccccccccccccccccccccc

!--------------------------------------
!=>Definizione altri Dati
!--------------------------------------
!-Se manca filtro: Pulizia campi filtro
         if(ass_fil_un_id.eq.0)then
            c_f_ce_stat(pgrato(pos))=0
            c_f_ce_flow_prog(pgrato(pos))=0.
            c_f_ce_deltap(pgrato(pos))=0.
         endif
!-Se manca air_cooler: Pulizia campi cool
         if(ass_cool_un_id.eq.0)then
            c_ac_ce_stat(pgrato(pos))=0
            c_ac_ce_flow_prog(pgrato(pos))=0.
            c_ac_ce_deltap(pgrato(pos))=0.
            c_ac_taria_prog(pgrato(pos))=0.
            c_ac_q_prog(pgrato(pos))=0.
         endif








!->Controllo se presente assetto della stessa centrale
         presente_ass = .false.

         do kel = etipti_ini,etipti(e_ce)-1
            if(elem_id(kel).eq.elem_id(pos))then
               presente_ass = .true. !*trovata stessa centrale
               prec_pos = kel        !*pos centrale trovata
CMAR_ASS_COND
   


CMAR_ASS_COND
      IF ( PRESENTE_ASS ) THEN
	






      IF(   ( 
     *ass_comp_id(pgrato(KEL)).EQ.ASSCIN_ASS_COMP_COND_ID(pgrato(POS)))
     *          .AND.
     *(ass_comp_id(pgrato(POS)).EQ.ASSCIN_ASS_COMP_COND_ID(pgrato(KEL)))
     * ) THEN




     

      FLAG_ASS_COND(KEL)= .true.
	FLAG_ASS_COND_P(pgrato(KEL))= .true.


      

      SIM_S=.FALSE.
CMAR_14_05_2013
      SIM(KEL)=.FALSE.
CMAR_14_05_2013

      htcitj(pos)=6

	no_acc(pos)=.true.



      flag_dati=.false.


CMAR_ASS_COND

CMAR_ASS_COND      ELSE 
	

	
CMAR_ASS_COND      FLAG_ASS_COND(KEL)= .false.
CMAR_ASS_COND	FLAG_ASS_COND_P(pgrato(KEL))= .false.

CMAR_CRIT_MAN 
       IF(asscin_fl_cri_manuale.eq.1)THEN
CMAR_CRIT_MAN

	crit_man_(kel)=.true.
	crit_man_(pos)=.true.

	
      ENDIF
CMAR_CRIT_MAN

c	goto 117



      ENDIF





	ENDIF 

CMAR_CRIT_MAN	      ENDIF







C-----------------


CMAR_ASS_COND
!---------------------------------
!-Test se OK per una stessa centrale
!-1)No max assetti raggiunto
               if(assetto_cm(pgrato(prec_pos)).eq.max_serv)then
                  goto 9992 !*errore
               endif
!-2)No parallelo(=0) con serie(=1)
               if((ass_comp_tipo_uso(pgrato(prec_pos)).ne.1) .and.
     -            (ass_comp_tipo_uso(pgrato(pos)).eq.1))then
                  goto 9992 !*errore
               endif
!-3)No serie(=1)
               if(ass_comp_tipo_uso(pgrato(prec_pos)).eq.1)then
                  goto 9992 !*errore
               endif
!---------------------------------

            endif
         enddo

         if(.not.presente_ass)then
            assetto_cm(pgrato(pos)) = 1 !**
            ass1p_pos (pgrato(pos)) = 0 !*uso_interno
         else
            assetto_cm(pgrato(pos)) = assetto_cm(pgrato(prec_pos))+1 !**
            ass1p_pos (pgrato(pos)) = prec_pos  !*uso_interno
            ass1p_pos (pgrato(prec_pos)) = pos  !*uso_interno
         endif

         first_assetto(pgrato(pos)) = (pgrato(pos)-1)*max_serv+1 !**
         tipo_fase    (pgrato(pos)) = fase_1                     !**

         if(asscin_fl_cri_manuale.gt.0)then
            tipo_criterio(pgrato(pos)) = crit_man
         endif

         flow_stat(pgrato(pos)) = porttj(pos)
         pasp_stat(pgrato(pos)) = asscin_p_asp
         tasp_stat(pgrato(pos)) = asscin_t_asp

!=>Se_Parallelo(=0)
         if(ass_comp_tipo_uso(pgrato(pos)).ne.1)then
            pmand_stat(pgrato(pos)) = asscin_p_man
            tmand_stat(pgrato(pos)) = temp_def

!->Se_Simcent
            if(tipoScenario.eq.TIPOSCEN_CE)then
               if(assetto_cm(pgrato(pos)).eq.1)then
                  if(etipti(e_ce).gt.otipti(e_ce))then
!-No presenza altra centrale 
                     etipti(e_ce) = etipti(e_ce)-1
                     goto 2000 !*Una sola centrale ammessa per Simcent
                  endif
!-Si centrale unica con 1 assetto_parallelo
                  call connPuntoFittizio(.true.,.true. ,pos,
     -            pgrato(pos))
                  call connPuntoFittizio(.true.,.false.,pos,
     -            pgrato(pos))
               else
!-Si centrale unica con 2 assetti_paralleli
                  bInsMonte = ass_coll_asp(pgrato(pos)) .ne.
     -                        ass_coll_asp(pgrato(prec_pos))
                  bInsValle = ass_coll_man(pgrato(pos)) .ne.
     -                        ass_coll_man(pgrato(prec_pos))
                  call connPuntoFittizio(bInsMonte,.true.,pos,
     -            pgrato(pos))
                  call connPuntoFittizio(bInsValle,.false.,pos,
     -            pgrato(pos))
                  goto 2000 !*Una sola centrale ammessa per Simcent
               endif
            endif

!=>Se_Serie(=1)
         else
            pmand_stat(pgrato(pos)) = (asscin_p_asp+asscin_p_man)/2.
            tmand_stat(pgrato(pos)) = (asscin_t_asp+temp_def)/2.

!=>Stadio_2
            pgrato_app(pos) = pgrato(pos)+1      !**

            assetto_cm(pgrato_app(pos)) = assetto_cm(pgrato(pos))+1 !**
            tipo_fase (pgrato_app(pos)) = fase_2 !**
            first_unit(pgrato_app(pos)) = 
     -           (pgrato_app(pos)-1)*maxunits+1  !**
            first_type(pgrato_app(pos)) = 
     -           first_unit(pgrato_app(pos))     !**
            first_assetto(pgrato_app(pos)) =
     -           (pgrato_app(pos)-1)*max_serv+1  !**
            unit_num(pgrato_app(pos)) = 0        !**

            sigla_ass    (pgrato_app(pos)) = sigla_ass(pgrato(pos))
            ass_comp_id  (pgrato_app(pos)) = ass_comp_id(pgrato(pos))
            altit        (pgrato_app(pos)) = altit(pgrato(pos))
            flow_stat    (pgrato_app(pos)) = flow_stat(pgrato(pos))
            taria        (pgrato_app(pos)) = taria(pgrato(pos))
            stat_ord     (pgrato_app(pos)) = stat_ord(pgrato(pos))
            tipo_criterio(pgrato_app(pos)) = tipo_criterio(pgrato(pos))

            pasp_stat (pgrato_app(pos)) =
     -                (asscin_p_asp+asscin_p_man)/2.
            tasp_stat (pgrato_app(pos)) =
     -                (asscin_t_asp+temp_def)/2.
            pmand_stat(pgrato_app(pos)) = asscin_p_man
            tmand_stat(pgrato_app(pos)) = temp_def


            max_pout (pgrato_app(pos)) = ass_pmax_man2
            max_tout (pgrato_app(pos)) = ass_tmax_man2
            flow_prog(pgrato_app(pos)) = ass_portata2
            delpin   (pgrato_app(pos)) = ass_deltap_asp2
            delpout  (pgrato_app(pos)) = ass_deltap_man2
            dtout    (pgrato_app(pos)) = ass_deltat_man2

            if(ass_fil_un_id2.ne.0)then
               c_f_ce_stat(pgrato_app(pos)) = fccin_filtro2_stato
               c_f_ce_flow_prog(pgrato_app(pos)) = fil_portata2
               c_f_ce_deltap(pgrato_app(pos)) = fil_perdita_p2
            else
               c_f_ce_stat(pgrato_app(pos)) = 0
               c_f_ce_flow_prog(pgrato_app(pos)) = 0.
               c_f_ce_deltap(pgrato_app(pos)) = 0.
            endif

            if(ass_cool_un_id2.ne.0)then
               c_ac_ce_stat(pgrato_app(pos)) = fccin_cool2_stato
               c_ac_ce_flow_prog(pgrato_app(pos)) = cool_portata2
               c_ac_ce_deltap(pgrato_app(pos)) = cool_caduta_p2
               c_ac_taria_prog(pgrato_app(pos)) = cool_temp_rif2
               c_ac_q_prog(pgrato_app(pos)) = cool_calore_scambiato2
            else
               c_ac_ce_stat(pgrato_app(pos)) = 0
               c_ac_ce_flow_prog(pgrato_app(pos)) = 0.
               c_ac_ce_deltap(pgrato_app(pos)) = 0.
               c_ac_taria_prog(pgrato_app(pos)) = 0.
               c_ac_q_prog(pgrato_app(pos)) = 0.
            endif

!->Se_Simcent
            if(tipoScenario.eq.TIPOSCEN_CE)then
               if(etipti(e_ce).gt.otipti(e_ce))then
!-No presenza altra centrale
                  etipti(e_ce) = etipti(e_ce)-1
                  goto 2000 !*Una sola centrale ammessa per Simcent
               endif
!-Si centrale unica con 1 assetto_serie
               call connPuntoFittizio(.true.,.true. ,pos,
     -         pgrato(pos))
               call connPuntoFittizio(.true.,.false.,pos,
     -         pgrato_app(pos))
               goto 2000 !*Una sola centrale ammessa per Simcent
            endif

         endif
!--------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
2000  continue
      close (urf_file,err=9996,iostat=stato)

!----------------------------------------------
!-Test presenza almeno una centrale se Simcent
      if(tipoScenario.eq.TIPOSCEN_CE)then
         if(etipti(e_ce).lt.otipti(e_ce))then
            goto 9991 !*errore
         endif
      endif
!----------------------------------------------
!-Inizio lettura dati Turbogruppi
2050  continue
!----------------------------------------------






CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C-----------------------------------------------------------------------------------------------------------------------------------






!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**3)-TURBOGRUPPI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      iel = 0 !Contatore record
      nomefile = rf_turbogruppi
!---------------------------------
!-Contatore turbogruppi letti
      tbg_num = 0
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_turbogruppi,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=3000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!001-tbgcin_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!002-ce_elem_id -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!003-tbg_comp_id -i*4 (id_turbogruppo)
         call read_i(str_rec,p_i,tgcompId,nullo,*9994)

!004-tbgcin_ass_comp_id -i*4 (id_assetto) 
         call read_i(str_rec,p_i,compId,nullo,*9994)

!005-tbgcin_stadio -i*4
         call read_i(str_rec,p_i,tbgcin_stadio,nullo,*9994)

!---------------------------------------------------------
!*Ricerca pos di elemId e compId tra le centrali presenti
         do pos = otipti(e_ce),etipti(e_ce)
            if(elem_id(pos).eq.elemId)then
               if(ass_comp_id(pgrato(pos)).eq.compId)then
                  goto 2100 !*OK se trovato
               endif
            endif
         enddo
!*Se non trovato:
        if(tipoScenario.eq.TIPOSCEN_CE)then
           goto 9983 !**errore incongruenza se Simcent
        else
           goto 2500 !**scarto se Rete
        endif
!---------------------------------------------------------

2100     continue
!-kun:  puntatore corrente unità
!-akun: eventuale puntatore unità inesistente che è in parallelo o in serie
!-su cui occorre poi copiare i dati del corrispondente kun esistente

         pkun = 0  !*puntatore altra unità kun in parallelo
         skun = 0  !*puntatore altra unità kun in serie
!=>Se_assetto_Parallelo(=0) o Bipolare(=0)
         if(ass_comp_tipo_uso(pgrato(pos)).ne.1)then
            prec_pos = ass1p_pos(pgrato(pos))  !*pos altra unità in parallelo

            if(prec_pos.gt.0)then
               do jel=1,unit_num(pgrato(pos))
                  kun = first_unit(pgrato(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     akun = 0
                     goto 9992 !*errore doppione
                  endif
               enddo
               do jel=1,unit_num(pgrato(prec_pos))
                  kun = first_unit(pgrato(prec_pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     pkun = kun
                     kun = first_unit(pgrato(pos)) + jel - 1
                     akun = 0

                     do kel=1,tbg_num
                        if(tbg_akun(kel).eq.kun)then
!-Pongo a zero akun di quello parallelo 
                           tbg_akun(kel) = 0
                        endif
                     enddo

                     goto 2200
                  endif
               enddo

               if(unit_num(pgrato(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato(pos)) = unit_num(pgrato(pos)) + 1 !**
               type_num(pgrato(pos)) = unit_num(pgrato(pos))     !**
               kun = first_unit(pgrato(pos)) +
     -             unit_num(pgrato(pos)) - 1                    

               if(unit_num(pgrato(prec_pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato(prec_pos)) =
     -             unit_num(pgrato(prec_pos)) + 1                      !**
               type_num(pgrato(prec_pos)) = unit_num(pgrato(prec_pos)) !**
               akun = first_unit(pgrato(prec_pos)) +
     -              unit_num(pgrato(prec_pos)) - 1
               tg_comp_id(akun) = 0  !**

2200           continue

            else
               do jel=1,unit_num(pgrato(pos))
                  kun = first_unit(pgrato(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     akun = 0
                     goto 9992 !*errore doppione
                  endif
               enddo

               if(unit_num(pgrato(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato(pos)) = unit_num(pgrato(pos)) + 1 !**
               type_num(pgrato(pos)) = unit_num(pgrato(pos))     !**
               kun = first_unit(pgrato(pos)) +
     -             unit_num(pgrato(pos)) - 1
               akun = 0

            endif

!=>Se_assetto_Serie(=1)
         else

!=>Se_fase_2
            if(tbgcin_stadio.eq.fase_2) then
               do jel=1,unit_num(pgrato(pos))
                  kun = first_unit(pgrato(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     skun = kun
                     kun = first_unit(pgrato_app(pos)) + jel - 1
                     akun = 0
!-
                     goto 9992 !*errore doppione tra i 2 stadi
!-
!-                     do kel=1,tbg_num
!-                        if(tbg_akun(kel).eq.kun)then
!-Pongo a zero akun di quello serie 
!-                           tbg_akun(kel) = 0
!-                        endif
!-                     enddo
!-                     goto 2300
!-
                  endif
               enddo
               do jel=1,unit_num(pgrato_app(pos))
                  kun = first_unit(pgrato_app(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     akun = 0
                     goto 9992 !*errore doppione
                  endif
               enddo

               if(unit_num(pgrato_app(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato_app(pos)) =
     -             unit_num(pgrato_app(pos)) + 1                     !**
               type_num(pgrato_app(pos)) = unit_num(pgrato_app(pos)) !**
               kun = first_unit(pgrato_app(pos)) +
     -             unit_num(pgrato_app(pos)) - 1

               if(unit_num(pgrato(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato(pos)) = unit_num(pgrato(pos)) + 1 !**
               type_num(pgrato(pos)) = unit_num(pgrato(pos))     !**
               akun = first_unit(pgrato(pos)) +
     -              unit_num(pgrato(pos)) - 1
               tg_comp_id(akun) = 0  !**
!-2300           continue

!=>Se_fase_1
            else
               do jel=1,unit_num(pgrato(pos))
                  kun = first_unit(pgrato(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     akun = 0
                     goto 9992 !*errore doppione
                  endif
               enddo
               do jel=1,unit_num(pgrato_app(pos))
                  kun = first_unit(pgrato_app(pos)) + jel - 1
                  if(tg_comp_id(kun).eq.tgcompId)then
                     skun = kun
                     kun = first_unit(pgrato(pos)) + jel - 1
                     akun = 0
!-
                     goto 9992 !*errore doppione tra i 2 stadi
!-
!-                     do kel=1,tbg_num
!-                        if(tbg_akun(kel).eq.kun)then
!-Pongo a zero akun di quello serie 
!-                           tbg_akun(kel) = 0
!-                        endif
!-                     enddo
!-                     goto 2400
!-
                  endif
               enddo

               if(unit_num(pgrato(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato(pos)) = unit_num(pgrato(pos)) + 1 !**
               type_num(pgrato(pos)) = unit_num(pgrato(pos))     !**
               kun = first_unit(pgrato(pos)) +
     -             unit_num(pgrato(pos)) - 1

               if(unit_num(pgrato_app(pos)).eq.maxunits)then
                  goto 9987 !*errore 
               endif
               unit_num(pgrato_app(pos)) =
     -             unit_num(pgrato_app(pos)) + 1                     !**
               type_num(pgrato_app(pos)) = unit_num(pgrato_app(pos)) !**
               akun = first_unit(pgrato_app(pos)) +
     -              unit_num(pgrato_app(pos)) - 1
               tg_comp_id(akun) = 0  !**
!-2400           continue

            endif
         endif
!------------------------------------------------------

!006-tbg_tipo_uso -i*4 (1/2=Monofase/Bifase)
         call read_i(str_rec,p_i,tbg_tipo_uso,nullo,*9994)

!007-cmp_id -i*4
         call read_i(str_rec,p_i,cmp_id,nullo,*9994)

!008-cmp_id2 -i*4
         call read_i(str_rec,p_i,cmp_id2,nullo,*9994)

!009-tbg_fil_un_id -i*4
         call read_i(str_rec,p_i,tbg_fil_un_id,nullo,*9994)

!010-tbg_cool_un_id -i*4
         call read_i(str_rec,p_i,tbg_cool_un_id,nullo,*9994)

!011-tbg_cool_un_id2 -i*4
         call read_i(str_rec,p_i,tbg_cool_un_id2,nullo,*9994)

!012-tbg_perdita_potenza -r*4
         call read_r(str_rec,p_i,unit_power_corr(kun),nullo,*9994)

!013-tbg_varia_consumo -r*4
         call read_r(str_rec,p_i,unit_hrate_corr(kun),nullo,*9994)

!014-tbg_corr_eff -r*4
         call read_r(str_rec,p_i,unit_eff_corr(kun),nullo,*9994)

!015-tbg_corr_eff2 -r*4
         call read_r(str_rec,p_i,unit2_eff_corr(kun),nullo,*9994)

!016-tbg_deltap_int -r*4
         call read_r(str_rec,p_i,unit_delprint(kun),nullo,*9994)

!017-tbg_deltat_int -r*4
         call read_r(str_rec,p_i,unit_deltrint(kun),nullo,*9994)

!018-tbg_tmax_int -r*4
         call read_r(str_rec,p_i,unit_maxtint(kun),nullo,*9994)

!019-cri_fatrip_portata -r*4
         call read_r(str_rec,p_i,unit_flow_rat(kun),nullo,*9994)

!020-cri_fatrip_num_giri -r*4
         call read_r(str_rec,p_i,unit_rev_rat(kun),nullo,*9994)

!021-cri_perc_min_equi -r*4
         call read_r(str_rec,p_i,unit_perc_min_equi(kun),nullo,*9994)

!022-cri_perc_max_equi -r*4
         call read_r(str_rec,p_i,unit_perc_max_equi(kun),nullo,*9994)

!023-tbgcin_stato -i*4
         call read_i(str_rec,p_i,unit_avail(kun),nullo,*9994)

!----------------------------------------
!-Cambio stato per tbg_uguali di centrale
!-per 2 paralleli uguali
         if(pkun.gt.0)then
            if(unit_avail(kun).eq.2)then
               unit_avail(pkun) = 0
            endif
            if(unit_avail(pkun).eq.2)then
               unit_avail(kun) = 0
            endif
         endif
!-per 2 stadi serie uguali
!-         if(skun.gt.0)then
!-            if(unit_avail(kun).gt.0)then
!-               unit_avail(skun) = 0
!-            endif
!-            if(unit_avail(skun).gt.0)then
!-               unit_avail(kun) = 0
!-            endif
!-         endif
!----------------------------------------

!024-tbgcin_portata_man -r*4
         call read_r(str_rec,p_i,fl_flow_man(kun),nullo,*9994)


CMAR_EVO_CRIT_MAN

	IF  ( crit_man_(pos) )THEN
	FL_FLOW_ASS(POS)=FL_FLOW_ASS(POS)+fl_flow_man(kun)
	ENDIF


CMAR_EVO_CRIT_MAN
!025-fccin_filtro1_stato -i*4
         call read_i(str_rec,p_i,c_f_tg_stat(kun),nullo,*9994)

!026-fccin_cool1_stato -i*4
         call read_i(str_rec,p_i,c_ac1_stat(kun),nullo,*9994)

!027-fccin_cool2_stato -i*4
         call read_i(str_rec,p_i,c_ac2_stat(kun),nullo,*9994)

!028-trb_num_giri -r*4
         call read_r(str_rec,p_i,t_nom_rev(kun),nullo,*9994)

!029-trb_potenza -r*4
         call read_r(str_rec,p_i,nom_power(kun),nullo,*9994)

!030-trb_heat_rate -r*4
         call read_r(str_rec,p_i,nom_hrate(kun),nullo,*9994)

!031-trb_temp -r*4
         call read_r(str_rec,p_i,nom_tair(kun),nullo,*9994)

!032-coet_cpw1 -r*4
!033-coet_cpw2
!034-coet_cpw3
         do jel=1,3
            call read_r(str_rec,p_i,type_cpwm(jel,kun),nullo,*9994)
         enddo

!035-coet_cpwt1 -r*4
!036-coet_cpwt2
!037-coet_cpwt3
!038-coet_cpwt4
         do jel=1,4
            call read_r(str_rec,p_i,type_cpwt(jel,kun),nullo,*9994)
         enddo

!039-coet_chrt1 -r*4
!040-coet_chrt2
!041-coet_chrt3
!042-coet_chrt4
         do jel=1,4
            call read_r(str_rec,p_i,type_chrt(jel,kun),nullo,*9994)
         enddo

!043-coet_chr1 -r*4
!044-coet_chr2
!045-coet_chr3
!046-coet_chr4
!047-coet_chr5
!048-coet_chr6
         do jel=1,6
            call read_r(str_rec,p_i,type_chr(jel,kun),nullo,*9994)
         enddo

!049-cmp_num_giri -r*4
         call read_r(str_rec,p_i,nom_rev(kun),nullo,*9994)

!050-cmp_portata -r*4
         call read_r(str_rec,p_i,nom_flow(kun),nullo,*9994)

!051-cmp_prevalenza -r*4
         call read_r(str_rec,p_i,nom_head(kun),nullo,*9994)

!052-cmp_num_giri_min -r*4
         call read_r(str_rec,p_i,min_rev(kun),nullo,*9994)

!053-cmp_num_giri_max -r*4
         call read_r(str_rec,p_i,max_rev(kun),nullo,*9994)

!054-cmp_efficienza_min -r*4
         call read_r(str_rec,p_i,min_eff(kun),nullo,*9994)

!055-coec_clim1 -r*4
!056-coec_clim2
!057-coec_clim3
!058-coec_clim4
         do jel=1,4
            call read_r(str_rec,p_i,type_c_coef(jel,kun),nullo,*9994)
         enddo

!059-coec_cen1 -r*4
!060-coec_cen2
!061-coec_cen3
!062-coec_cen4
!063-coec_cen5
         do jel=1,5
            call read_r(str_rec,p_i,type_cen(jel,kun),nullo,*9994)
         enddo

!064-coec_cec1 -r*4
!065-coec_cec2
!066-coec_cec3
!067-coec_cec4
!068-coec_cec5
!069-coec_cec6
         do jel=1,6
            call read_r(str_rec,p_i,type_cec(jel,kun),nullo,*9994)
         enddo

!070-coec_chn1 -r*4
!071-coec_chn2
!072-coec_chn3
!073-coec_chn4
!074-coec_chn5
!075-coec_chn6
         do jel=1,6
            call read_r(str_rec,p_i,type_chn(jel,kun),nullo,*9994)
         enddo

!076-coec_chc1 -r*4
!077-coec_chc2
!078-coec_chc3
!079-coec_chc4
!080-coec_chc5
!081-coec_chc6
         do jel=1,6
            call read_r(str_rec,p_i,type_chc(jel,kun),nullo,*9994)
         enddo

!082-cmp_num_giri_2 -r*4
        call read_r(str_rec,p_i,nom2_rev(kun),nullo,*9994)

!083-cmp_portata_2 -r*4
         call read_r(str_rec,p_i,nom2_flow(kun),nullo,*9994)

!084-cmp_prevalenza_2 -r*4
         call read_r(str_rec,p_i,nom2_head(kun),nullo,*9994)

!085-cmp_num_giri_min_2 -r*4
         call read_r(str_rec,p_i,min2_rev(kun),nullo,*9994)

!086-cmp_num_giri_max_2 -r*4
         call read_r(str_rec,p_i,max2_rev(kun),nullo,*9994)

!087-cmp_efficienza_min_2 -r*4
         call read_r(str_rec,p_i,min2_eff(kun),nullo,*9994)

!088-coec_clim1_2 -r*4
!089-coec_clim2_2
!090-coec_clim3_2
!091-coec_clim4_2
         do jel=1,4
            call read_r(str_rec,p_i,type2_c_coef(jel,kun),
     -      nullo,*9994)
         enddo

!092-coec_cen1_2 -r*4
!093-coec_cen2_2
!094-coec_cen3_2
!095-coec_cen4_2
!096-coec_cen5_2
         do jel=1,5
            call read_r(str_rec,p_i,type2_cen(jel,kun),
     -      nullo,*9994)
         enddo

!097-coec_cec1_2 -r*4
!098-coec_cec2_2
!099-coec_cec3_2
!100-coec_cec4_2
!101-coec_cec5_2
!102-coec_cec6_2
         do jel=1,6
            call read_r(str_rec,p_i,type2_cec(jel,kun),
     -      nullo,*9994)
         enddo

!103-coec_chn1_2 -r*4
!104-coec_chn2_2
!105-coec_chn3_2
!106-coec_chn4_2
!107-coec_chn5_2
!108-coec_chn6_2
         do jel=1,6
            call read_r(str_rec,p_i,type2_chn(jel,kun),
     -      nullo,*9994)
         enddo

!109-coec_chc1_2 -r*4
!110-coec_chc2_2
!111-coec_chc3_2
!112-coec_chc4_2
!113-coec_chc5_2
!114-coec_chc6_2
         do jel=1,6
            call read_r(str_rec,p_i,type2_chc(jel,kun),
     -      nullo,*9994)
         enddo

!115-fil_portata -r*4
         call read_r(str_rec,p_i,c_f_tg_flow_prog(kun),nullo,*9994)

!116-fil_perdita_p -r*4
         call read_r(str_rec,p_i,c_f_tg_deltap(kun),nullo,*9994)

!117-cool_portata -r*4
         call read_r(str_rec,p_i,c_ac1_flow_prog(kun),nullo,*9994)

!118-cool_caduta_p -r*4
         call read_r(str_rec,p_i,c_ac1_deltap(kun),nullo,*9994)

!119-cool_temp_rif -r*4
         call read_r(str_rec,p_i,c_ac1_taria_prog(kun),nullo,*9994)

!120-cool_calore_scambiato -r*4
         call read_r(str_rec,p_i,c_ac1_q_prog(kun),nullo,*9994)

!121-cool_portata_2 -r*4
         call read_r(str_rec,p_i,c_ac2_flow_prog(kun),nullo,*9994)

!122-cool_caduta_p_2 -r*4
         call read_r(str_rec,p_i,c_ac2_deltap(kun),nullo,*9994)

!123-cool_temp_rif_2 -r*4
         call read_r(str_rec,p_i,c_ac2_taria_prog(kun),nullo,*9994)

!124-cool_calore_scambiato_2 -r*4
         call read_r(str_rec,p_i,c_ac2_q_prog(kun),nullo,*9994)
!-
!**-Fine Elenco dati-**

!--------------------------------------
!=>Definizione altri Dati
!--------------------------------------
!-Se manca filtro: Pulizia campi filtro
         if(tbg_fil_un_id.eq.0)then
            c_f_tg_stat(kun)=0
            c_f_tg_flow_prog(kun)=0.
            c_f_tg_deltap(kun)=0.
         endif

!-Se manca air_cooler: Pulizia campi cool
         if(tbg_cool_un_id.eq.0)then
            c_ac1_stat(kun)=0
            c_ac1_flow_prog(kun)=0.
            c_ac1_deltap(kun)=0.
            c_ac1_taria_prog(kun)=0.
            c_ac1_q_prog(kun)=0.
         endif

!-Se manca air_cooler2: Pulizia campi cool2
         if(tbg_cool_un_id2.eq.0)then
            c_ac2_stat(kun)=0
            c_ac2_flow_prog(kun)=0.
            c_ac2_deltap(kun)=0.
            c_ac2_taria_prog(kun)=0.
            c_ac2_q_prog(kun)=0.
!-Se turbogruppo Bifase e configurazione Parallelo: anche Pulizia campi cool
            if(tbg_tipo_uso.eq.2)then
               if(stat_ord(pgrato(pos)).eq.1)then
                  c_ac1_stat(kun)=0
                  c_ac1_flow_prog(kun)=0.
                  c_ac1_deltap(kun)=0.
                  c_ac1_taria_prog(kun)=0.
                  c_ac1_q_prog(kun)=0.
               endif
            endif
         endif

!-Definizione Id Turbogruppo
         tg_comp_id(kun) = tgcompId  !**

!-Valori tbg_tipo_uso: 1=Monofase, 2=Bifase
         unit_bifase(kun) = tbg_tipo_uso .ne. 1
         type_quant (kun) = 1

         unit_vinc_maxrev(kun) = cin_num_giri_max(pgrato(pos))
         unit_vinc_minpow(kun) = cin_potenza_min(pgrato(pos))
         unit_vinc_maxpow(kun) = cin_potenza_max(pgrato(pos))

!-se_assetto_Parallelo(=0) o Bipolare
         if(ass_comp_tipo_uso(pgrato(pos)).ne.1)then
            if((fl_flow_man(kun).gt.0.) .and.
     -         (tipo_criterio(pgrato(pos)).eq.crit_man) .and.
     -         (unit_avail(kun).eq.2))then
	        status(kun) = 'ON'    !**
            else
	        status(kun) = 'OFF'   !**
            endif

!-se assetto_Serie(=1)
         else
            if(tipo_criterio(pgrato(pos)).eq.crit_man)then
               if(fl_flow_man (kun).gt.0)then
                  status(kun) = 'ON'    !**
               else
                  status(kun) = 'OFF'   !**
               endif
            else
               status(kun) = 'OFF'      !**
            endif
         endif

         type_nlim(kun)  = 0
         type2_nlim(kun) = 0

!===========================================
!-Raccolta finale dati per:
!-1)assegnare punti_lim a compressori
!-2)copia unità tra 2 assetti paralleli
!-3)copia unità tra 2 stadi di assetto serie
!===========================================
         if(tbg_num.eq.max_unit)then
            goto 9997 !*errore
         endif
         tbg_num = tbg_num + 1
         tbg_kun (tbg_num) = kun
         tbg_akun(tbg_num) = akun
         tbg_cmp_id(tbg_num) = cmp_id
!-se_bifase
         if(tbg_tipo_uso.eq.2)then
            tbg_cmp_id2(tbg_num) = cmp_id2
         else
            tbg_cmp_id2(tbg_num) = 0
         endif
!============================================

2500  continue
      enddo


!===Fine_Loop_lettura==================================

!=>EOF finale
3000  continue
      close (urf_file,err=9996,iostat=stato)

!--------------------------------------
!-Test presenza turbogruppi se Simcent
      if(tipoScenario.eq.TIPOSCEN_CE)then
        if(tbg_num.eq.0)then
           goto 9982 !*errore 
        endif
      endif




!--------------------------------------

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**4)-PUNTI_LIM_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      iel = 0 !Contatore record
      nomefile = rf_punti_lim
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_punti_lim,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,10000,end=4000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-plim_scen_id -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

!02-plim_cmp_id -i*4
         call read_i(str_rec,p_i,plim_cmp_id,nullo,*9994)

!03-plim_prg -i*4 su -type_nlim
         call read_i(str_rec,p_i,plim_prg,nullo,*9994)

!04-plim_num_giri -r*4 su -type_lim_n
         call read_r(str_rec,p_i,plim_num_giri,nullo,*9994)

!05-plim_portata -r*4 su -type_lim_q
         call read_r(str_rec,p_i,plim_portata,nullo,*9994)

!06-plim_prevalenza -r*4 su -type_lim_h
         call read_r(str_rec,p_i,plim_prevalenza,nullo,*9994)

!07-plim_alim -r*4 su -type_a_coef
         call read_r(str_rec,p_i,plim_alim,nullo,*9994)

!08-plim_blim -r*4 su -type_b_coef
         call read_r(str_rec,p_i,plim_blim,nullo,*9994)
!-
!**-Fine Elenco dati-**

!--------------------------------------
!=>Definizione altri Dati
!--------------------------------------
!*Ricerca compressori per turbogruppo
!-compressore_1
         do jel=1,tbg_num
            if(tbg_cmp_id(jel).eq.plim_cmp_id)then
               kun = tbg_kun(jel)

               if(type_nlim(kun).eq.max_lim)then
                  goto 9997 !*errore
               endif

               type_nlim(kun) = type_nlim(kun)+1
               type_lim_n (type_nlim(kun),kun) = plim_num_giri
               type_lim_q (type_nlim(kun),kun) = plim_portata
               type_lim_h (type_nlim(kun),kun) = plim_prevalenza
               type_a_coef(type_nlim(kun),kun) = plim_alim
               type_b_coef(type_nlim(kun),kun) = plim_blim
            endif

!-compressore_2 per bifase
            if(tbg_cmp_id2(jel).eq.plim_cmp_id)then
               kun = tbg_kun(jel)

               if(type2_nlim(kun).eq.max_lim)then
                  goto 9997 !*errore
               endif

               type2_nlim(kun) = type2_nlim(kun)+1
               type2_lim_n (type2_nlim(kun),kun) = plim_num_giri
               type2_lim_q (type2_nlim(kun),kun) = plim_portata
               type2_lim_h (type2_nlim(kun),kun) = plim_prevalenza
               type2_a_coef(type2_nlim(kun),kun) = plim_alim
               type2_b_coef(type2_nlim(kun),kun) = plim_blim

            endif
         enddo
!--------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
4000  continue
      close (urf_file,err=9996,iostat=stato)

!===================================================
!-Operazione di Copia finale turbogruppi per i casi:
!-1) 2 assetti paralleli
!-2) 2 stadi serie
!-Copia dati turbogruppi da kun solo su akun marcati 
!===================================================
      do jel=1,tbg_num
         if(tbg_akun(jel).eq.0) goto 5000

         akun = tbg_akun(jel)
         kun  = tbg_kun(jel)
         tg_comp_id (akun) = tg_comp_id(kun)
         unit_avail (akun) = 0
         unit_bifase(akun) = unit_bifase(kun)
         type_quant (akun) = type_quant(kun)
cgpe-corr         status     (akun) = status(kun)
         status     (akun) = 'OFF'
cgpe-corr-end
         unit_vinc_maxrev(akun) = unit_vinc_maxrev(kun)
         unit_vinc_minpow(akun) = unit_vinc_minpow(kun)
         unit_vinc_maxpow(akun) = unit_vinc_maxpow(kun)

         unit_power_corr(akun) = unit_power_corr(kun)
         unit_hrate_corr(akun) = unit_hrate_corr(kun)
         unit_eff_corr  (akun) = unit_eff_corr(kun)
         unit2_eff_corr (akun) = unit2_eff_corr(kun)
         unit_delprint  (akun) = unit_delprint(kun)
         unit_deltrint  (akun) = unit_deltrint(kun)
         unit_maxtint   (akun) = unit_maxtint(kun)
         unit_flow_rat  (akun) = unit_flow_rat(kun)
         unit_rev_rat   (akun) = unit_rev_rat(kun)
         unit_perc_min_equi(akun) = unit_perc_min_equi(kun)
         unit_perc_max_equi(akun) = unit_perc_max_equi(kun)

cgpe-corr         fl_flow_man(akun) = fl_flow_man(kun)
         fl_flow_man(akun) = 0.
cgpe-corr-end

CMAR_CRIT_MAN 
       fl_flow_man(akun) = fl_flow_man(kun)
CMAR_CRIT_MAN 
         t_nom_rev  (akun) = t_nom_rev(kun)
         nom_power  (akun) = nom_power(kun)
         nom_hrate  (akun) = nom_hrate(kun)
         nom_tair   (akun) = nom_tair(kun)
         nom_rev    (akun) = nom_rev(kun)
         nom_flow   (akun) = nom_flow(kun)
         nom_head   (akun) = nom_head(kun)
         min_rev    (akun) = min_rev(kun)
         max_rev    (akun) = max_rev(kun)
         min_eff    (akun) = min_eff(kun)
         nom2_rev   (akun) = nom2_rev(kun)
         nom2_flow  (akun) = nom2_flow(kun)
         nom2_head  (akun) = nom2_head(kun)
         min2_rev   (akun) = min2_rev(kun)
         max2_rev   (akun) = max2_rev(kun)
         min2_eff   (akun) = min2_eff(kun)

         c_f_tg_stat(akun) = c_f_tg_stat(kun)
         c_ac1_stat (akun) = c_ac1_stat(kun)
         c_ac2_stat (akun) = c_ac2_stat(kun)

         c_f_tg_flow_prog(akun) = c_f_tg_flow_prog(kun)
         c_f_tg_deltap   (akun) = c_f_tg_deltap(kun)
         c_ac1_flow_prog (akun) = c_ac1_flow_prog(kun)
         c_ac1_deltap    (akun) = c_ac1_deltap(kun)
         c_ac1_taria_prog(akun) = c_ac1_taria_prog(kun)
         c_ac1_q_prog    (akun) = c_ac1_q_prog(kun)
         c_ac2_flow_prog (akun) = c_ac2_flow_prog(kun)
         c_ac2_deltap    (akun) = c_ac2_deltap(kun)
         c_ac2_taria_prog(akun) = c_ac2_taria_prog(kun)
         c_ac2_q_prog    (akun) = c_ac2_q_prog(kun)

         do iel=1,3
            type_cpwm(iel,akun) = type_cpwm(iel,kun)
         enddo
         do iel=1,4
            type_cpwt(iel,akun) = type_cpwt(iel,kun)
         enddo
         do iel=1,4
            type_chrt(iel,akun) = type_chrt(iel,kun)
         enddo
         do iel=1,6
            type_chr(iel,akun) = type_chr(iel,kun)
         enddo
         do iel=1,4
            type_c_coef(iel,akun) = type_c_coef(iel,kun)
         enddo
         do iel=1,5
            type_cen(iel,akun) = type_cen(iel,kun)
         enddo
         do iel=1,6
            type_cec(iel,akun) = type_cec(iel,kun)
         enddo
         do iel=1,6
            type_chn(iel,akun) = type_chn(iel,kun)
         enddo
         do iel=1,6
            type_chc(iel,akun) = type_chc(iel,kun)
         enddo
         do iel=1,4
            type2_c_coef(iel,akun) = type2_c_coef(iel,kun)
         enddo
         do iel=1,5
            type2_cen(iel,akun) = type2_cen(iel,kun)
         enddo
         do iel=1,6
            type2_cec(iel,akun) = type2_cec(iel,kun)
         enddo
         do iel=1,6
            type2_chn(iel,akun) = type2_chn(iel,kun)
         enddo
         do iel=1,6
            type2_chc(iel,akun) = type2_chc(iel,kun)
         enddo

         type_nlim(akun) = type_nlim(kun)
         do iel=1,type_nlim(akun)
            type_lim_n (iel,akun) = type_lim_n (iel,kun)
            type_lim_q (iel,akun) = type_lim_q (iel,kun)
            type_lim_h (iel,akun) = type_lim_h (iel,kun)
            type_a_coef(iel,akun) = type_a_coef(iel,kun)
            type_b_coef(iel,akun) = type_b_coef(iel,kun)
         enddo

         type2_nlim(akun) = type2_nlim(kun)
         do iel=1,type2_nlim(akun)
            type2_lim_n (iel,akun) = type2_lim_n (iel,kun)
            type2_lim_q (iel,akun) = type2_lim_q (iel,kun)
            type2_lim_h (iel,akun) = type2_lim_h (iel,kun)
            type2_a_coef(iel,akun) = type2_a_coef(iel,kun)
            type2_b_coef(iel,akun) = type2_b_coef(iel,kun)
         enddo

5000  continue
      enddo
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9982  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_centrali ',
     - 'Attenzione!  Mancano Turbogruppi',0)
      goto 9999
9983  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Turbogruppo errato: '//snomtv(pos)(1:pf2)
      goto 9998
9987  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Turbogruppi > Max : '//snomtv(pos)(1:pf2)
      goto 9998
9990  continue
      str_err = 'Open errata'
      goto 9998
9991  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_centrali ',
     - 'Attenzione!  Mancano Centrali',0)
      goto 9999
9992  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Assetti errati : '//snomtv(pos)(1:pf2)
      goto 9998
9993  continue
      stato=0
      pf2 = s_trim_d(snomtv(pos))
      str_err = 'Connessione errata : '//snomtv(pos)(1:pf2)
      goto 9998
9994  continue
      stato=0
9995  continue
      call msgerr_key (iel,'Record errato:',str_err)
      goto 9998
9996  continue
      str_err = 'Close errata'
      goto 9998
9997  continue
      stato=0
      str_err = 'Superato Max elementi'
9998  continue
      pf1 = s_trim_d(nomefile)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_centrali ',
     - nomefile(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (urf_file,err=9999)
9999  continue 
      return 1
      end
!==============================================================
      subroutine leggi_udm_out (str_err,*)

!*Lettura UDM per OUTPUT (udm_in.txt)
!-sequenza dati diversa tra SIMCENT (v_udm_output) e RETE (v_udm_output_scen)
!-Definisce per udm_grd_id_db:
!-udm_grd_prec_db, udm_fact_db, udm_plus_db, udm_sgl_db
      implicit none
      include '../inc/param.inc'
      include '../inc/conv.inc'  !udm conversioni

      character*(*) str_err      !O

      integer*4 p_i,iel,jel,stato,pf1,pf2,s_trim_d
      integer*4 io_scen_id,io_grd_id,grd_prec
      real*4 udm_fact,udm_plus
      logical*2 esiste_udm,eof,nullo
      logical*2 esiste_udmId(udm_dim)
      character*(tit_len) udm_sgl     !**
      character*(max_len_rec) str_rec !**
!---------------------------------
5000  format(A)
!---------------------------------
!-Definizione Eof lettura
      eof = .false.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**-UDM_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-Test presenza file
      inquire(file=trim(c_path_dirdati)//'_'//trim(rf_udm),
     - Exist=esiste_udm)
      if(.not.esiste_udm)then
         goto 2000
      endif
!---------------------------------
!-Contatore record
      iel = 0
!-File non esiste o vuoto
      esiste_udm = .false.
!-Flag no/si presenza chiavi nel file
      do jel=1,udm_dim
         esiste_udmId(jel) = .false.
      enddo
!---------------------------------
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_udm,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel +1
         read (urf_file,5000,end=1000,err=9995,iostat=stato) str_rec

         esiste_udm = .true.
         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!-NO SIMCENT, SI RETE
         if(tipoScenario.ne.TIPOSCEN_CE)then
!-         io_scen_id -i*4
            call read_i(str_rec,p_i,io_scen_id,nullo,*9994)
         endif

!01-io_grd_id -i*4
         call read_i(str_rec,p_i,io_grd_id,nullo,*9994)

!02-grd_prec -i*4
         call read_i(str_rec,p_i,grd_prec,nullo,*9994)

!03-udm_fact -r*4
         call read_r(str_rec,p_i,udm_fact,nullo,*9994)

!04-udm_plus -r*4
         call read_r(str_rec,p_i,udm_plus,nullo,*9994)

!05-udm_sgl -c*(tit_len)
         call read_c(str_rec,p_i,udm_sgl,*9994)
!-
!**-Fine Elenco dati-**

!--------------------------------------
!-Cerca io_grd_id letta nelle udm_grd_id_db definite
!--------------------------------------
         do jel=1,udm_dim
            if(udm_grd_id_db(jel).eq.io_grd_id)then
               esiste_udmId(jel) = .true.
               udm_grd_prec_db(jel) = grd_prec
               udm_fact_db    (jel) = udm_fact
               udm_plus_db    (jel) = udm_plus
               udm_sgl_db     (jel) = udm_sgl
               goto 200
            endif
         enddo
200      continue

!-Controllo se trovate tutte le chiavi
         do jel=1,udm_dim
            if(.not.esiste_udmId(jel)) goto 300
         enddo
!-Tutte trovate -> chiudere prima lettura
         close (urf_file,err=9996,iostat=stato)
         return
300      continue
!--------------------------------------

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
1000  continue
      close (urf_file,err=9996,iostat=stato)

2000  continue
      if(.not.esiste_udm)then
         call gest_error(3,0,' ',' ',0)
         call gest_error(3,0,' ',
     -   'Non sono state definite le UDM di output.  '//
     -   'Le UDM visualizzate sono quelle interne',0)
         return
      endif

!-Test se tutti gli udm_grd_id_db sono stati trovati nel file
      do jel=1,udm_dim
         if(.not.esiste_udmId(jel))then
            call gest_error(3,0,' ',' ',0)
            call gest_error(3,0,' ',
     -      'Non sono tutte definite le UDM di output.  '//
     -      'Per le mancanti sono usate le UDM interne',0)
            return
         endif
      enddo
      return
!---------------------------------

!---------------------------------
!->Errori
9990  continue
      str_err = 'Open errata'
      goto 9998
9994  continue
      stato=0
9995  continue
      call msgerr_key (iel,'Record errato:',str_err)
      goto 9998
9996  continue
      str_err = 'Close errata'
9998  continue
      pf1 = s_trim_d(rf_udm)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_udm_out ',
     - rf_udm(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (urf_file,err=9999)
9999  continue 
      return 1
      end
      subroutine leggi_infoscen (str_err,*)

!*Lettura dati generali di simulazione scenario di Rete
!-Legge: scenari_in.txt (v_scenari_rete)
      implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'
cmar_pcs
ccc      include  '../inc/FLAG_CONV.INC'
cmar_pcs

      character*(*) str_err      !O

      integer*4 p_i,stato,scer_scen_id,pf1,pf2,s_trim_d
      logical*2 esiste_scen,nullo
      character*(max_len_rec) str_rec !**
!---------------------------------
5000  format(A)
!---------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**-SCENARI_IN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-Test presenza file
      inquire(file=trim(c_path_dirdati)//'_'//trim(rf_infoscen),
     - Exist=esiste_scen)
      if(.not.esiste_scen)then
         goto 2000
      endif
!---------------------------------
!-File non esiste o vuoto
      esiste_scen = .false.
!=>Open file
      call open_vfile (urf_file,c_path_dirdati,'_'//rf_infoscen,
     -    'OLD',stato,*9990)

      read (urf_file,5000,end=1000,err=9995,iostat=stato) str_rec

      esiste_scen=.true.
      p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-scer_scen_id -i*4
      call read_i(str_rec,p_i,scer_scen_id,nullo,*9994)

!02-scer_stagione -i*4
!1=Primavera,2=Estate,3=Autunno,4=Inverno
      call read_i(str_rec,p_i,stagione,nullo,*9994)
      if(stagione.le.0.or.stagione.gt.4)then
         esiste_scen = .false.
         goto 1000
      endif

!03-scer_interpolazione -i*4
!1=Gradino,2=Lineare,3=Biquadratica
      call read_i(str_rec,p_i,interpol,nullo,*9994)
      if(interpol.le.0.or.interpol.gt.3)then 
         esiste_scen = .false.
         goto 1000
      endif

!04-scer_tipo_prelievo -i*4
!1=Medio,2=Istante
      call read_i(str_rec,p_i,tipo_prel,nullo,*9994)
      if(tipo_prel.le.0.or.tipo_prel.gt.2)then 
         esiste_scen = .false.
         goto 1000
      endif

!05-scer_istante_prelievo -r*4
!Istante se tipo_prel=2
      call read_r(str_rec,p_i,ist_prel,nullo,*9994)

!06-scer_fl_recupero -i*4
!0=No recupero,1=Si recupero (Newton sem,Newton)
      call read_i(str_rec,p_i,fl_nwrec,nullo,*9994)
      if(fl_nwrec.lt.0.or.fl_nwrec.gt.1)then 
         esiste_scen = .false.
         goto 1000
      endif
cmar_pcs
c            call read_i(str_rec,p_i,interpol,nullo,*9994)
cmar_pcs

!07-scer_fl_tlc -i*4
!0=No telecomandi,1=Si telecomandi
      call read_i(str_rec,p_i,fl_tlc,nullo,*9994)
      if(fl_tlc.lt.0.or.fl_tlc.gt.1)then 
         esiste_scen = .false.
         goto 1000
      endif

!08-scer_tipo_tlc -i*4
!0=No ciclici,1=Si ciclici (se fl_tlc=1)
      call read_i(str_rec,p_i,tipo_tlc,nullo,*9994)
      if(tipo_tlc.lt.0.or.tipo_tlc.gt.1)then 
         esiste_scen = .false.
         goto 1000
      endif

!09-scer_fl_modulazioni -i*4
!0=No modulazioni,1=Si modulazioni
      call read_i(str_rec,p_i,fl_modul,nullo,*9994)
      if(fl_modul.lt.0.or.fl_modul.gt.1)then 
         esiste_scen = .false.
         goto 1000
      endif

      
!10-scer_istante_iniziale -r*4
      call read_r(str_rec,p_i,ist_iniz,nullo,*9994)

!11-scer_istante_finale -r*4
      call read_r(str_rec,p_i,ist_fine,nullo,*9994)

!12-scer_passo_campionamento -r*4
      call read_r(str_rec,p_i,passo_camp,nullo,*9994)

!13-scer_ultimo_campionamento -i*4
      call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)
!14-tipo di convergenza stazionario
      call read_i(str_rec,p_i,FLAG_CONVERGENZA,nullo,*9994)
ccc      FLAG_CONVERGENZA=1
!-
!**-Fine Elenco dati-**

!=>Close file
1000  continue
      close (urf_file,err=9996,iostat=stato)

2000  continue
      if(.not.esiste_scen)then
         call gest_error(3,0,' ',' ',0)
         call gest_error(3,0,' ',
     -  'Non sono definiti i dati generali di scenario.  '//
     -  'Vengono utilizzati i valori interni',0)
         return
      endif

!---------------------------------
!*Considerazioni finali per Stazionario
!-(si riporta su flag dinamica scelte stazionario)
      if(tipoScenario.eq.TIPOSCEN_RETE_sta)then
!-se tipo_prel: 1=Medio
         if(tipo_prel.eq.1)then 
            fl_modul = 0     !No modulazioni
            ist_prel = 0.    !istante_prelievo=0
         else
            fl_modul = 1     !Si modulazioni
         endif
!-si pone istante_iniziale = istante_prelievo
         ist_iniz = ist_prel
      endif
!---------------------------------
     
      return
!---------------------------------

!---------------------------------
!->Errori
9990  continue
      str_err = 'Open errata'
      goto 9998
9994  continue
      stato=0
9995  continue
      str_err = 'Record errato'
      goto 9998
9996  continue
      str_err = 'Close errata'
9998  continue
      pf1 = s_trim_d(rf_infoscen)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_infoscen',
     - rf_infoscen(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (urf_file,err=9999)
9999  continue 
      return 1
      end
!==============================================================
      subroutine leggi_parasim ()

!*Lettura parametri caratteristici per la Simulazione
!-Legge: 'parasim.txt' (direttamente nella cartella exe di simulazione)
!-Se il file non esiste o è vuoto o errato, vengono considerati
!-i valori definiti in ini_common
      implicit none
      include '../inc/param.inc'
      include '../inc/ric.inc'
      include '../inc/staznw_par.inc'

      integer*4 stato,pos,pos1,pos2,pos3,pos4,pos_i,pos_f,iel,s_trim_d
	integer*4 max_label
      parameter (max_label=35)
	integer*4 f_label(max_label)
      real*4 r_val
      logical*2 eof,esiste_parasim
      character*(25) label(max_label)
      character*(80) str_rec
!---------------------------------
10000 format(A)
!---------------------------------
!-Definizione Eof lettura
      eof = .false.
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**-PARASIM.CONFIG
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-Test presenza file
      inquire(file=trim(rf_parasim),Exist=esiste_parasim)
      if(.not.esiste_parasim)then
         return
      endif

c      call gest_error (3,0,"LeggiFile","Parametri Aggiornati",0)
!---------------------------------
!-Definizione label ammesse per identificazione valore
!01-Peso per P
      label(1) = 'rc_weit_p'
!02-Peso per Q
      label(2) = 'rc_weit_q'
!03-Peso per pmon=pval
      label(3) = 'rc_weit_on'
!04-Tolleranza ass.conv.P(punti)
      label(4) = 'rc_atol_cvp'
!05-Tolleranza rel.conv.P(punti)
      label(5) = 'rc_rtol_cvp'
!06-Tolleranza ass.conv.Q(tronchi,el.spec)
      label(6) = 'rc_atol_cvq'
!07-Tolleranza rel.conv.Q(tronchi,el.spec)
      label(7) = 'rc_rtol_cvq'
!08-Tolleranza rel.conv.increm.somm.(P e Q)**2
      label(8) = 'rc_rtol_cvs'
!09-Tolleranza controllo equaz.Bilancio
      label(9) = 'rc_atol_cnb'
!10-Tolleranza controllo equaz.Fergusson
      label(10)= 'rc_atol_cnf'
!11-Tolleranza controllo Q Imposte
      label(11)= 'rc_atol_cnq'
!12-Tolleranza controllo P Imposte
      label(12)= 'rc_atol_cnp'
!13-Tolleranza controllo pmon=pval
      label(13)= 'rc_atol_cnon'
!14-Delta min tempo per passo dinamica
      label(14)= 't_d_passo_min'
!15-Delta max tempo per passo dinamica
      label(15)= 't_d_passo_max'
!16-Delta min tempo fra input (meno di un minuto)
      label(16)= 't_passo_input'
!17-Tolleranza accuratezza GEAR
      label(17)= 'tol_gear'
!18-Max num.iter.Newton
      label(18)= 'mitnwd'
!19-Diff.relativa max iter.Newton
      label(19)= 'tolxwd'
!20-Max Residui
      label(20)= 'tolfwd'
!21-Max num.iter.in comp.temp.
      label(21)= 'mitcwd'
!22-Tolleranza cicli in comp.temp.
      label(22)= 'tolcwd'
!23-Max num.iter.in pcls
      label(23)= 'mit_pcls'
!24-Tolleranza cicli in conce
      label(24)= 'toll_conce'
!25-Max num.iter.in conce
      label(25)= 'mit_conce'
!26-Max num.iter.risol.sistema in PQ
      label(26)= 'max_count'
!27-Max num.iter.diverg.risol.sistema in PQ,TC 
      label(27)= 'm_it_nconv'
!28-Precisione controlli Q
      label(28)= 'repsq'
!29-Precisione controlli P
      label(29)= 'repsp'
!30-Precisione controlli T
      label(30)= 'repstc'
!31-Precisione controlli 0
      label(31)= 'zerotc'
!32-Correz.P punti centrale
      label(32)= 'perc_p'
!33-Correz.Q punti centrale
      label(33)= 'perc_q'
!34-Range P punti centrale
      label(34)= 'max_deltap'
!35-Range Q punti centrale
      label(35)= 'max_deltaq'
!---------------------------------
!-Posizione finale label ammesse
      do iel =1,max_label
         f_label(iel) = s_trim_d(label(iel)) !pos finale label
      enddo
!---------------------------------
!=>Open file
      call open_vfile (urf_file,'',trim(rf_parasim),'OLD',stato,*7000)

!====================================================
      dowhile (.not.eof)

!-Lettura record
         read(urf_file,10000,end=6000,err=6000) str_rec

!-Ricerca valore numerico
         pos_f = s_trim_d(str_rec) !pos_f fine stringa da esaminare
         if(pos_f.eq.0) goto 5000  !scarto stringa vuota

         do pos = 1,pos_f
            if((str_rec(pos:pos).ne.' ').and.
     -         (str_rec(pos:pos).ne.char(9)))then
               pos1 = pos !pos1 inizio valore
               pos2 = pos !pos2 fine valore
               goto 100
            endif
         enddo
         goto 5000  !scarto stringa con solo tab
100      continue
         do pos = pos1+1,pos_f
            if((str_rec(pos:pos).eq.' ').or.
     -         (str_rec(pos:pos).eq.char(9))) goto 200
            pos2 = pos !pos2 fine valore
         enddo
200      continue

!-Estrazione valore da pos1 a pos2 (scarto se non numerico)
         read(str_rec(pos1:pos2),*,err=5000) r_val !*estrazione valore
!----------------------------------------------------
!-Ricerca label del valore (se esiste -> ok assegnazione valore alla grandezza)
!----------------------------------------------------
         pos_i = pos2+2 !pos iniziale stringa dopo valore e un balnk o tab
         if(pos_i.gt.pos_f) goto 5000  !scarto se label a blank

!-Estrazione label da pos3 a pos4
         do pos = pos_i,pos_f
            if((str_rec(pos:pos).ne.' ').and.
     -         (str_rec(pos:pos).ne.char(9)))then
               pos3 = pos !pos3 inizio label
               pos4 = pos !pos4 fine label
               goto 300
            endif
         enddo
         goto 5000  !scarto stringa con solo tab
300      continue
         do pos = pos3+1,pos_f
            if((str_rec(pos:pos).eq.' ').or.
     -         (str_rec(pos:pos).eq.char(9))) goto 400
            pos4 = pos !pos4 fine label
         enddo
400      continue

!-Conversione label caratteri Maiuscolo in minuscolo
         do pos = pos3,pos4
            if(str_rec(pos:pos).ge.'A'.and.str_rec(pos:pos).le.'Z')then
               str_rec(pos:pos)=char(ichar(str_rec(pos:pos))+32)
            endif
         enddo

!-Ricerca label fra quelle ammesse
         do iel =1,max_label
            if(str_rec(pos3:pos4).eq.label(iel)(1:f_label(iel)))then
               if    (iel.eq.1)then
                  rc_weit_p = r_val
               elseif(iel.eq.2)then
                  rc_weit_q = r_val
               elseif(iel.eq.3)then
                  rc_weit_on = r_val
               elseif(iel.eq.4)then
                  rc_atol_cvp = r_val
               elseif(iel.eq.5)then
                  rc_rtol_cvp = r_val
               elseif(iel.eq.6)then
                  rc_atol_cvq = r_val
               elseif(iel.eq.7)then
                  rc_rtol_cvq = r_val
               elseif(iel.eq.8)then
                  rc_rtol_cvs = r_val
               elseif(iel.eq.9)then
                  rc_atol_cnb = r_val
               elseif(iel.eq.10)then
                  rc_atol_cnf = r_val
               elseif(iel.eq.11)then
                  rc_atol_cnq = r_val
               elseif(iel.eq.12)then
                  rc_atol_cnp = r_val
               elseif(iel.eq.13)then
                  rc_atol_cnon = r_val
               elseif(iel.eq.14)then
                  t_d_passo_min = r_val
               elseif(iel.eq.15)then
                  t_d_passo_max = r_val
               elseif(iel.eq.16)then
                  t_passo_input = r_val
               elseif(iel.eq.17)then
                  tol_gear = r_val
               elseif(iel.eq.18)then
                  mitnwd = r_val
               elseif(iel.eq.19)then
                  tolxwd = r_val
               elseif(iel.eq.20)then
                  tolfwd = r_val
               elseif(iel.eq.21)then
                  mitcwd = r_val
               elseif(iel.eq.22)then
                  tolcwd = r_val
               elseif(iel.eq.23)then
                  mit_pcls = r_val
               elseif(iel.eq.24)then
                  toll_conce = r_val
               elseif(iel.eq.25)then
                  mit_conce = r_val
               elseif(iel.eq.26)then
                  max_count = r_val
               elseif(iel.eq.27)then
                  m_it_nconv = r_val
               elseif(iel.eq.28)then
                  repsq = r_val
               elseif(iel.eq.29)then
                  repsp = r_val
               elseif(iel.eq.30)then
                  repstc = r_val
               elseif(iel.eq.31)then
                  zerotc = r_val
               elseif(iel.eq.32)then
                  perc_p = r_val
               elseif(iel.eq.33)then
                  perc_q = r_val
               elseif(iel.eq.34)then
                  max_deltap = r_val
               elseif(iel.eq.35)then
                  max_deltaq = r_val
               endif
               goto 5000
            endif
         enddo
!----------------------------------------------------

5000  continue
      enddo
!====================================================
6000  continue
!=>Close file
      close (urf_file)
7000  continue

      return
      end
!==============================================================
