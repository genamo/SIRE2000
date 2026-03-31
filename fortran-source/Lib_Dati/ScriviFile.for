!===================================================================================
      Subroutine scrivi_scenario (msgerr,*)

!*-Scrive i file di OUTPUT per tutti gli elementi di uno scenario
      implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'

      character*(*) msgerr  !IO
!-------------------------------------------------------------------------
!-Scrittura output elementi Rete (NO SIMCENT, SI RETE)
!-------------------------------------------------------------------------
      if(tipoScenario.ne.TIPOSCEN_CE)then

!-      Eventuale Ricomposizione originale tronchi con lunghezza > max 
         call attiva_tr_originali ()

!-      Scrittura output elementi
         call scrivi_Ris_Ele (msgerr,*9999)

!-      Eventuale Scomposizione tronchi con lunghezza > max 
         call attiva_tr_scomposti ()

      endif
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
!-Scrittura output centrali bipolari e multifunzione (SI SIMCENT, SI RETE)
!-------------------------------------------------------------------------
      call scrivi_Ris_Cen (msgerr,*9999)

!-------------------------------------------------------------------------
      return

9999  continue
      return 1
      end
!===================================================================================
      subroutine scrivi_Ris_Cen (str_err,*)

!*-Scrive i risultati di centrali bipolari e multifunzione
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc' !*cm_serie (tipo_fase,pgrato_app)
      include '../inc/top_app_ele.inc'

      character*(*) str_err  !IO

      integer*4 iel,stato,pgr,js,assCompId
      integer*4 pf1,pf2,s_trim_d
      logical*2 bMF,bSerie
      character*30 nomefile
!--------------------------------------------------
!=>Open file di output
      nomefile = wf_centrali
      call open_vfile(uwf_centrali,c_path_dirlog,'_'//wf_centrali,
     - 'REPLACE',stato,*9990)

      nomefile = wf_assetti
      call open_vfile(uwf_assetti,c_path_dirlog,'_'//wf_assetti,
     - 'REPLACE',stato,*9990)

      nomefile = wf_turbogruppi
      call open_vfile(uwf_turbogruppi,c_path_dirlog,'_'//wf_turbogruppi,
     - 'REPLACE',stato,*9990)
!--------------------------------------------------
!-Inizio Loop Centrali-----------------------
      do iel = otipti(e_ce),etipti(e_ce)

         assCompId = 0
         bMF = assetto_cm (pgrato(iel)) .gt. 0
         bSerie = pgrato(iel) .ne. pgrato_app(iel)

         if(bMF)then
!-Se_Multifunzione

            if(tipoScenario.eq.TIPOSCEN_CE)then
!-per SIMCENT
               call scrivi_assetti (iel,
     -         pgrato(iel),pgrato_app(iel),bSerie,
     -         uwf_assetti,wf_assetti,str_err,*9999)
            else
!-per RETE
               call scrivi_ass_rete (iel,
     -         pgrato(iel),pgrato_app(iel),bSerie,
     -         uwf_assetti,wf_assetti,str_err,*9999)
            endif
	
            do js=1,2
               if(js.eq.1 .or. (js.eq.2 .and. bSerie))then
                  if(js.eq.1)then
                     pgr = pgrato(iel)
                     assCompId = ass_comp_id(pgr)
                  else
                     pgr = pgrato_app(iel)
                  endif
               endif
            enddo

         else
!-Se_Bipolare
            pgr = pgrato(iel)

            if(tipoScenario.eq.TIPOSCEN_CE)then
!-per SIMCENT
               call scrivi_centrali (iel,pgr,
     -         uwf_centrali,wf_centrali,str_err,*9999)
            else
!-per RETE
               call scrivi_cen_rete (iel,pgr,
     -         uwf_centrali,wf_centrali,str_err,*9999)
            endif

         endif

         call scrivi_RisTurbogruppi (pgrato(iel),pgrato_app(iel),
     -        bMF.and.bSerie,assCompId,str_err,*9999)

      enddo
!-Fine Loop Centrali-------------------------
!--------------------------------------------------
!=>Close file di output
      nomefile = wf_centrali
      close (uwf_centrali,err=9996,iostat=stato)
      nomefile = wf_assetti
      close (uwf_assetti,err=9996,iostat=stato)
      nomefile = wf_turbogruppi
      close (uwf_turbogruppi,err=9996,iostat=stato)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9990  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Open errata: ' // nomefile(1:pf1)
      goto 9998
9996  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Close errata: ' // nomefile(1:pf1)
9998  continue
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_Ris_Cen',str_err(1:pf2),stato)
      str_err=' '
      close (uwf_centrali,err=8001)
8001  close (uwf_assetti,err=8002)
8002  close (uwf_turbogruppi,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_RisTurbogruppi (pgr,pgrApp,bMF_serie,assCompId,
     -           str_err,*)

!*-Scrive i risultati di turbogruppi per centrali bipolari e multifunzione
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'

      integer*4     pgr       !I
      integer*4     pgrApp    !I
      logical*2     bMF_serie !I
      integer*4     assCompId !I significativo solo se tbg di assetto
      character*(*) str_err   !IO

      integer*4 j1,jtg,kun,kunApp,j1App,stadio
      logical*2 bAcceso,bAccesoApp
      character*3 statoAccensione
!--------------------------------------------------
      j1    = first_unit(pgr)
      j1App = first_unit(pgrApp)

!-NOTA: nel caso di MF serie unit_num(pgr) coincide con unit_num(pgrApp)
      do jtg=1,unit_num(pgr) 

          kun = j1 + jtg -1
          stadio = 0
          bAcceso = .false.
          bAccesoApp = .false.
          call getStatoAcc (pgr,kun,statoAccensione,bAcceso)

          if(bMF_serie)then
             if(bAcceso)then
                stadio = fase_1
             else
                kunApp = j1App + jtg -1
                call getStatoAcc (pgrApp,kunApp,statoAccensione,
     -               bAccesoApp)
                if(bAccesoApp)then
                   stadio = fase_2
                   kun = kunApp
                endif
             endif
          endif

          if(bAcceso.or.bAccesoApp)then

             if(tipoScenario.eq.TIPOSCEN_CE)then
!-per SIMCENT
                call scrivi_turbogr (kun,assCompId,stadio,
     -          statoAccensione,uwf_turbogruppi,wf_turbogruppi,
     -          str_err,*9999)
             else
!-per RETE
                call scrivi_turb_rete (kun,assCompId,stadio,
     -          statoAccensione,uwf_turbogruppi,wf_turbogruppi,
     -          str_err,*9999)
             endif

          endif

      enddo

      return

!--------------------------------------------------
!->Errori
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_centrali (iel,pgr,unita,nomefile,str_err,*)

!*-Per SIMCENT: Scrive il file output 'centrali_out.txt' (DB: centrali_out_scen_simcent)
      implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/ti.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 iel          !I
      integer*4 pgr          !I
      integer*4 unita        !I
      character*(*) nomefile !I
      character*(*) str_err  !IO
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-CEOUT_SCEN_ID -i*4 
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-CEOUT_CE_ELEM_ID -i*4
      call write_i(str_rec,p_i,elem_id(iel),*9997)
      
!03-CEOUT_FL_FATAL -i*4
      call write_i(str_rec,p_i,fl_fatal(pgr),*9997)
      
!04-CEOUT_METANO -r*4
      call write_r(str_rec,p_i,comptc(iel,1),*9997)

!05-CEOUT_ETANO -r*4
      call write_r(str_rec,p_i,comptc(iel,2),*9997)

!06-CEOUT_PROPANO -r*4
      call write_r(str_rec,p_i,comptc(iel,3),*9997)

!07-CEOUT_NBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,4),*9997)

!08-CEOUT_IBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,5),*9997)

!09-CEOUT_PENTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,6),*9997)

!10-CEOUT_ESANO -r*4
      call write_r(str_rec,p_i,comptc(iel,7),*9997)

!11-CEOUT_AZOTO -r*4
      call write_r(str_rec,p_i,comptc(iel,8),*9997)

!12-CEOUT_CO2 -r*4
      call write_r(str_rec,p_i,comptc(iel,9),*9997)

!13-CEOUT_H2S -r*4
      call write_r(str_rec,p_i,comptc(iel,10),*9997)

!14-CEOUT_PORTATA -r*4
      call write_r(str_rec,p_i,porttj(iel),*9997)

!15-CEOUT_PORTATA_IN -r*4
      call write_r(str_rec,p_i,flow_stat_in(pgr),*9997)

!16-CEOUT_PORTATA_OUT -r*4
      call write_r(str_rec,p_i,flow_stat(pgr),*9997)
!17-CEOUT_P_ASP -r*4      
      call write_r(str_rec,p_i,pasp_stat(pgr),*9997)
!18-CEOUT_P_MAN -r*4
      if(press_mand_calc(pgr) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgr),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgr),*9997)        
      end if
      


!19-CEOUT_T_ASP -r*4
      call write_r(str_rec,p_i,tasp_stat(pgr),*9997)

!20-CEOUT_T_MAN -r*4
      call write_r(str_rec,p_i,tmand_stat(pgr),*9997)

!21-CEOUT_RCOMPR -r*4
      call write_r(str_rec,p_i,comp_ratio(pgr),*9997)

!22-CEOUT_TOTCONS -r*4
      call write_r(str_rec,p_i,tot_cons(pgr),*9997)

!23-CEOUT_DELTAP_IN -r*4
      call write_r(str_rec,p_i,delpr1(pgr),*9997)

!24-CEOUT_DELTAP_OUT -r*4
      call write_r(str_rec,p_i,delpr2(pgr),*9997)

!25-CEOUT_HEAD -r*4
      call write_r(str_rec,p_i,head(pgr),*9997)

!26-CEOUT_FLOWM -r*4
      call write_r(str_rec,p_i,flowm(pgr),*9997)

!27-CEOUT_FLOWRIC -r*4
      call write_r(str_rec,p_i,q_riciclo(pgr),*9997)

!28-CEOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,cons_su_qout(pgr),*9997)

!29-CEOUT_FUEL_SU_PTT -r*4
      call write_r(str_rec,p_i,fuel_su_ptt(pgr),*9997)

!30-CEOUT_PTT_SU_PFT -r*4
      call write_r(str_rec,p_i,ptt_su_pft(pgr),*9997)

!31-CEOUT_POWER -r*4
      r_val=0
      call write_r(str_rec,p_i,r_val,*9997)

!32-CEOUT_POWER_PERC_DER -r*4
      call write_r(str_rec,p_i,power_perc_der(pgr),*9997)

!33-CEOUT_CE_ELEM_UFF_ID -i*4
      i_val = 0
      call write_i(str_rec,p_i,i_val,*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_centrali',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_cen_rete (iel,pgr,unita,nomefile,str_err,*)

!*-Per RETE: Scrive file output 'centrali_out.txt' (DB: centrali_out_scen)
      implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/ti.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'

      integer*4 iel          !I
      integer*4 pgr          !I
      integer*4 unita        !I
      character*(*) nomefile !I
      character*(*) str_err  !IO
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      real*4 udm_int ! function
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-CEOUT_SCEN_ID -i*4 
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-CEOUT_CE_ELEM_ID -i*4
      call write_i(str_rec,p_i,elem_id(iel),*9997)
      
!03-CEOUT_CAMPIONAMENTO -i*4
      call write_i(str_rec,p_i,ultimo_camp,*9997)

!04-CEOUT_ISTANTE -r*4
      call write_r(str_rec,p_i,corr_ist,*9997)

!05-CEOUT_METANO -r*4
      call write_r(str_rec,p_i,comptc(iel,1),*9997)

!06-CEOUT_ETANO -r*4
      call write_r(str_rec,p_i,comptc(iel,2),*9997)

!07-CEOUT_PROPANO -r*4
      call write_r(str_rec,p_i,comptc(iel,3),*9997)

!08-CEOUT_NBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,4),*9997)

!09-CEOUT_IBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,5),*9997)

!10-CEOUT_PENTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,6),*9997)

!11-CEOUT_ESANO -r*4
      call write_r(str_rec,p_i,comptc(iel,7),*9997)

!12-CEOUT_AZOTO -r*4
      call write_r(str_rec,p_i,comptc(iel,8),*9997)

!13-CEOUT_CO2 -r*4
      call write_r(str_rec,p_i,comptc(iel,9),*9997)

!14-CEOUT_H2S -r*4
      call write_r(str_rec,p_i,comptc(iel,10),*9997)

!15-CEOUT_PORTATA -r*4
      call write_r(str_rec,p_i,porttj(iel),*9997)

!16-CEOUT_PORTATA_IN -r*4
      call write_r(str_rec,p_i,flow_stat_in(pgr),*9997)

!17-CEOUT_PORTATA_OUT -r*4
      call write_r(str_rec,p_i,flow_stat(pgr),*9997)

!18-CEOUT_P_ASP -r*4      
      call write_r(str_rec,p_i,pasp_stat(pgr),*9997)
!19-CEOUT_P_MAN -r*4
      do itip= 1, 100
         if(press_mand_calc(itip) .gt. 0.0) then
             press_mand_calc(pgr) = press_mand_calc(itip)
         end if    
      end do    
      
      if(press_mand_calc(pgr) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgr),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgr),*9997)        
      end if

!20-CEOUT_T_ASP -r*4
      call write_r(str_rec,p_i,tasp_stat(pgr),*9997)

!21-CEOUT_T_MAN -r*4
      call write_r(str_rec,p_i,tmand_stat(pgr),*9997)

!22-CEOUT_RCOMPR -r*4
      call write_r(str_rec,p_i,comp_ratio(pgr),*9997)

!23-CEOUT_TOTCONS -r*4
      call write_r(str_rec,p_i,tot_cons(pgr),*9997)

!24-CEOUT_DELTAP_IN -r*4
      call write_r(str_rec,p_i,delpr1(pgr),*9997)

!25-CEOUT_DELTAP_OUT -r*4
      call write_r(str_rec,p_i,delpr2(pgr),*9997)

!26-CEOUT_HEAD -r*4
      call write_r(str_rec,p_i,head(pgr),*9997)

!27-CEOUT_FLOWM -r*4
      call write_r(str_rec,p_i,flowm(pgr),*9997)

!28-CEOUT_FLOWRIC -r*4
      call write_r(str_rec,p_i,q_riciclo(pgr),*9997)

!29-CEOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,cons_su_qout(pgr),*9997)

!30-CEOUT_FUEL_SU_PTT -r*4
      call write_r(str_rec,p_i,fuel_su_ptt(pgr),*9997)

!31-CEOUT_PTT_SU_PFT -r*4
      call write_r(str_rec,p_i,ptt_su_pft(pgr),*9997)

!32-CEOUT_POWER -r*4
c      r_val = 0.
c      call write_r(str_rec,p_i,r_val,*9997)
      call write_r(str_rec,p_i,power_tot(pgr),*9997)

!33-CEOUT_POWER_PERC_DER -r*4
      call write_r(str_rec,p_i,power_perc_der(pgr),*9997)

!34-CEOUT_TIPO_SET -i*4
      call conv_tiporeg_sire_db (e_ce,iel,i_val)
      call write_i(str_rec,p_i,i_val,*9997)

!35-CEOUT_SET_P_ASP -r*4
      call write_r(str_rec,p_i,pmsetj(iel),*9997)

!36-CEOUT_SET_P_MAN -r*4
      call write_r(str_rec,p_i,pvsetj(iel),*9997)

!37-CEOUT_SET_PORTATA -r*4
      call write_r(str_rec,p_i,qsettj(iel),*9997)

!38-CEOUT_STATO_FUNZ_ID -i*4
      if    (hstatj(iel).eq.sta_pm)then
             i_val = sta_pasp
      elseif(hstatj(iel).eq.sta_pv)then
             i_val = sta_pman
      else
             i_val = hstatj(iel)
      endif
      call write_i(str_rec,p_i,i_val,*9997)

!39-CEOUT_NUM_TBG_ATTIVI -i*4
      call write_i(str_rec,p_i,num_tbg_attivi(pgr),*9997)

!40-CEOUT_PCS -r*4
      r_val = udm_int(udm_pcs,pclstd(iel))
      call write_r(str_rec,p_i,r_val,*9997)

!41-CEOUT_POWER_MAX_NOM -r*4
      call write_r(str_rec,p_i,nom_max_pow(pgr),*9997)

!42-CEOUT_POWER_MAX_NOM_DER -r*4
      call write_r(str_rec,p_i,nom_max_pow_der(pgr),*9997)

!43-CEOUT_POWER_MAX_NOM_RID -r*4
      call write_r(str_rec,p_i,nom_max_pow_vinc(pgr),*9997)

!44-CEOUT_POWER_MIN -r*4
      call write_r(str_rec,p_i,nom_min_pow(pgr),*9997)

!45-CEOUT_FL_ERR -i*4
      call write_i(str_rec,p_i,flertj(iel),*9997)

!46-CEOUT_FL_FATAL -i*4
      call write_i(str_rec,p_i,fl_fatal(pgr),*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_cen_rete',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_assetti (iel,pgr,pgrApp,bSerie,unita,
     -                           nomefile,str_err,*)

!*-Per SIMCENT: Scrive il file output 'assetti_out.txt' (DB: assetti_out_scen_simcent)
      implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/top_app_ele.inc'


      integer*4 iel          !I
      integer*4 pgr          !I
      integer*4 pgrApp       !I
      logical*2 bSerie       !I
      integer*4 unita        !I
      character*(*) nomefile !I
      character*(*) str_err  !IO
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

      
      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-ASSOUT_SCEN_ID -i*4
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-ASSOUT_ASS_COMP_ID -i*4
      call write_i(str_rec,p_i,ass_comp_id(pgr),*9997)

!03-ASSOUT_FL_FATAL -i*4
      call write_i(str_rec,p_i,fl_fatal(pgr),*9997)

!04-ASSOUT_METANO -r*4
      call write_r(str_rec,p_i,comptc(iel,1),*9997)

!05-ASSOUT_ETANO -r*4
      call write_r(str_rec,p_i,comptc(iel,2),*9997)

!06-ASSOUT_PROPANO -r*4
      call write_r(str_rec,p_i,comptc(iel,3),*9997)

!07-ASSOUT_NBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,4),*9997)

!08-ASSOUT_IBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,5),*9997)

!09-ASSOUT_PENTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,6),*9997)

!10-ASSOUT_ESANO -r*4
      call write_r(str_rec,p_i,comptc(iel,7),*9997)

!11-ASSOUT_AZOTO -r*4
      call write_r(str_rec,p_i,comptc(iel,8),*9997)

!12-ASSOUT_CO2 -r*4
      call write_r(str_rec,p_i,comptc(iel,9),*9997)

!13-ASSOUT_H2S -r*4
      call write_r(str_rec,p_i,comptc(iel,10),*9997)

!14-ASSOUT_PORTATA -r*4
      call write_r(str_rec,p_i,porttj(iel),*9997)

!15-ASSOUT_PORTATA_IN -r*4
      call write_r(str_rec,p_i,flow_stat_in(pgr),*9997)

!16-ASSOUT_PORTATA_OUT -r*4
      call write_r(str_rec,p_i,flow_stat(pgr),*9997)

!17-ASSOUT_P_ASP -r*4      
      call write_r(str_rec,p_i,pasp_stat(pgr),*9997)
!18-ASSOUT_P_MAN -r*4
      if(press_mand_calc(pgr) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgr),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgr),*9997)        
      end if

!19-ASSOUT_T_ASP -r*4
      call write_r(str_rec,p_i,tasp_stat(pgr),*9997)

!20-ASSOUT_T_MAN -r*4
      call write_r(str_rec,p_i,tmand_stat(pgr),*9997)

!21-ASSOUT_RCOMPR -r*4
      call write_r(str_rec,p_i,comp_ratio(pgr),*9997)

!22-ASSOUT_TOTCONS -r*4
      call write_r(str_rec,p_i,tot_cons(pgr),*9997)

!23-ASSOUT_DELTAP_IN -r*4
      call write_r(str_rec,p_i,delpr1(pgr),*9997)

!24-ASSOUT_DELTAP_OUT -r*4
      call write_r(str_rec,p_i,delpr2(pgr),*9997)

!25-ASSOUT_HEAD -r*4
      call write_r(str_rec,p_i,head(pgr),*9997)

!26-ASSOUT_FLOWM -r*4
      call write_r(str_rec,p_i,flowm(pgr),*9997)

!27-ASSOUT_FLOWRIC -r*4
      call write_r(str_rec,p_i,q_riciclo(pgr),*9997)

!28-ASSOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,cons_su_qout(pgr),*9997)

!29-ASSOUT_FUEL_SU_PTT -r*4
      call write_r(str_rec,p_i,fuel_su_ptt(pgr),*9997)

!30-ASSOUT_PTT_SU_PFT -r*4
      call write_r(str_rec,p_i,ptt_su_pft(pgr),*9997)

!31-ASSOUT_POWER -r*4
      r_val=0
      call write_r(str_rec,p_i,r_val,*9997)

!32-ASSOUT_POWER_PERC_DER -r*4
      call write_r(str_rec,p_i,power_perc_der(pgr),*9997)
cmar
!32BIS-PERC_POW_STAZIONE -r*4
cmar_
      if(power_perc_der(pgr).eq.0)then
      PERC_POW_STAZIONE(pgr)=power_perc_der(pgr)
	endif
cmar_
      call write_r(str_rec,p_i,PERC_POW_STAZIONE(pgr),*9997)
cmar
!**-SI SERIE
      if(bSerie)then

!33-ASSOUT_2_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9997)

!34-ASSOUT_2_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9997)

!35-ASSOUT_2_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9997)

!36-ASSOUT_2_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9997)

!37-ASSOUT_2_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9997)

!38-ASSOUT_2_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9997)

!39-ASSOUT_2_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9997)

!40-ASSOUT_2_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9997)

!41-ASSOUT_2_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9997)

!42-ASSOUT_2_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9997)

!43-ASSOUT_2_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9997)

!44-ASSOUT_2_PORTATA_IN -r*4
         call write_r(str_rec,p_i,flow_stat_in(pgrApp),*9997)

!45-ASSOUT_2_PORTATA_OUT -r*4
         call write_r(str_rec,p_i,flow_stat(pgrApp),*9997)

!46-ASSOUT_2_P_ASP -r*4
         call write_r(str_rec,p_i,pasp_stat(pgrApp),*9997)

!47-ASSOUT_2_P_MAN -r*4
      if(press_mand_calc(pgrApp) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgrApp),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgrApp),*9997)        
      end if
!48-ASSOUT_2_T_ASP -r*4
         call write_r(str_rec,p_i,tasp_stat(pgrApp),*9997)

!49-ASSOUT_2_T_MAN -r*4
         call write_r(str_rec,p_i,tmand_stat(pgrApp),*9997)

!50-ASSOUT_2_RCOMPR -r*4
         call write_r(str_rec,p_i,comp_ratio(pgrApp),*9997)

!51-ASSOUT_2_TOTCONS -r*4
         call write_r(str_rec,p_i,tot_cons(pgrApp),*9997)

!52-ASSOUT_2_DELTAP_IN -r*4
         call write_r(str_rec,p_i,delpr1(pgrApp),*9997)

!53-ASSOUT_2_DELTAP_OUT -r*4
         call write_r(str_rec,p_i,delpr2(pgrApp),*9997)

!54-ASSOUT_2_HEAD -r*4
         call write_r(str_rec,p_i,head(pgrApp),*9997)

!55-ASSOUT_2_FLOWM -r*4
         call write_r(str_rec,p_i,flowm(pgrApp),*9997)

!56-ASSOUT_2_FLOWRIC -r*4
         call write_r(str_rec,p_i,q_riciclo(pgrApp),*9997)

!57-ASSOUT_2_CONS_SU_QOUT -r*4
         call write_r(str_rec,p_i,cons_su_qout(pgrApp),*9997)

!58-ASSOUT_2_FUEL_SU_PTT -r*4
         call write_r(str_rec,p_i,fuel_su_ptt(pgrApp),*9997)

!59-ASSOUT_2_PTT_SU_PFT -r*4
         call write_r(str_rec,p_i,ptt_su_pft(pgrApp),*9997)

!60-ASSOUT_2_POWER -r*4
         r_val=0
         call write_r(str_rec,p_i,r_val,*9997)

!61-ASSOUT_2_POWER_PERC_DER -r*4
         call write_r(str_rec,p_i,power_perc_der(pgrApp),*9997)

!**-NO SERIE
      else

         r_val=0
!33-ASSOUT_2_METANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!34-ASSOUT_2_ETANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!35-ASSOUT_2_PROPANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!36-ASSOUT_2_NBUTANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!37-ASSOUT_2_IBUTANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!38-ASSOUT_2_PENTANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!39-ASSOUT_2_ESANO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!40-ASSOUT_2_AZOTO -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!41-ASSOUT_2_CO2 -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!42-ASSOUT_2_H2S -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!43-ASSOUT_2_PORTATA -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!44-ASSOUT_2_PORTATA_IN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!45-ASSOUT_2_PORTATA_OUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!46-ASSOUT_2_P_ASP -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!47-ASSOUT_2_P_MAN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!48-ASSOUT_2_T_ASP -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!49-ASSOUT_2_T_MAN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!50-ASSOUT_2_RCOMPR -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!51-ASSOUT_2_TOTCONS -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!52-ASSOUT_2_DELTAP_IN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!53-ASSOUT_2_DELTAP_OUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!54-ASSOUT_2_HEAD -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!55-ASSOUT_2_FLOWM -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!56-ASSOUT_2_FLOWRIC -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!57-ASSOUT_2_CONS_SU_QOUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!58-ASSOUT_2_FUEL_SU_PTT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!59-ASSOUT_2_PTT_SU_PFT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!60-ASSOUT_2_POWER -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!61-ASSOUT_2_POWER_PERC_DER -r*4
         call write_r(str_rec,p_i,r_val,*9997)

      endif
!**-

!62-ASSOUT_ASS_COMP_UFF_ID -i*4
      i_val = 0
      call write_i(str_rec,p_i,i_val,*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_assetti ',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_ass_rete (iel,pgr,pgrApp,bSerie,unita,
     -                           nomefile,str_err,*)

!*-Per RETE: Scrive il file output 'assetti_out.txt' (DB: assetti_out_scen)
      implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'

      integer*4 iel          !I
      integer*4 pgr          !I
      integer*4 pgrApp       !I
      logical*2 bSerie       !I
      integer*4 unita        !I
      character*(*) nomefile !I
      character*(*) str_err  !IO
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      real*4 udm_int ! function
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-ASSOUT_SCEN_ID -i*4
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-ASSOUT_ASS_COMP_ID -i*4
      call write_i(str_rec,p_i,ass_comp_id(pgr),*9997)

!03-ASSOUT_CAMPIONAMENTO -i*4
      call write_i(str_rec,p_i,ultimo_camp,*9997)

!04-ASSOUT_ISTANTE -r*4
      call write_r(str_rec,p_i,corr_ist,*9997)

!05-ASSOUT_METANO -r*4
      call write_r(str_rec,p_i,comptc(iel,1),*9997)

!06-ASSOUT_ETANO -r*4
      call write_r(str_rec,p_i,comptc(iel,2),*9997)

!07-ASSOUT_PROPANO -r*4
      call write_r(str_rec,p_i,comptc(iel,3),*9997)

!08-ASSOUT_NBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,4),*9997)

!09-ASSOUT_IBUTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,5),*9997)

!10-ASSOUT_PENTANO -r*4
      call write_r(str_rec,p_i,comptc(iel,6),*9997)

!11-ASSOUT_ESANO -r*4
      call write_r(str_rec,p_i,comptc(iel,7),*9997)

!12-ASSOUT_AZOTO -r*4
      call write_r(str_rec,p_i,comptc(iel,8),*9997)

!13-ASSOUT_CO2 -r*4
      call write_r(str_rec,p_i,comptc(iel,9),*9997)

!14-ASSOUT_H2S -r*4
      call write_r(str_rec,p_i,comptc(iel,10),*9997)

!15-ASSOUT_PORTATA -r*4
      call write_r(str_rec,p_i,porttj(iel),*9997)

!16-ASSOUT_PORTATA_IN -r*4
      call write_r(str_rec,p_i,flow_stat_in(pgr),*9997)

!17-ASSOUT_PORTATA_OUT -r*4
      call write_r(str_rec,p_i,flow_stat(pgr),*9997)

!18-ASSOUT_P_ASP -r*4
      call write_r(str_rec,p_i,pasp_stat(pgr),*9997)

!19-ASSOUT_P_MAN -r*4
      if(press_mand_calc(pgr) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgr),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgr),*9997)        
      end if
!20-ASSOUT_T_ASP -r*4
      call write_r(str_rec,p_i,tasp_stat(pgr),*9997)

!21-ASSOUT_T_MAN -r*4
      call write_r(str_rec,p_i,tmand_stat(pgr),*9997)

!22-ASSOUT_RCOMPR -r*4
      call write_r(str_rec,p_i,comp_ratio(pgr),*9997)

!23-ASSOUT_TOTCONS -r*4
      call write_r(str_rec,p_i,tot_cons(pgr),*9997)

!24-ASSOUT_DELTAP_IN -r*4
      call write_r(str_rec,p_i,delpr1(pgr),*9997)

!25-ASSOUT_DELTAP_OUT -r*4
      call write_r(str_rec,p_i,delpr2(pgr),*9997)

!26-ASSOUT_HEAD -r*4
      call write_r(str_rec,p_i,head(pgr),*9997)

!27-ASSOUT_FLOWM -r*4
      call write_r(str_rec,p_i,flowm(pgr),*9997)

!28-ASSOUT_FLOWRIC -r*4
      call write_r(str_rec,p_i,q_riciclo(pgr),*9997)

!29-ASSOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,cons_su_qout(pgr),*9997)

!30-ASSOUT_FUEL_SU_PTT -r*4
      call write_r(str_rec,p_i,fuel_su_ptt(pgr),*9997)

!31-ASSOUT_PTT_SU_PFT -r*4
      call write_r(str_rec,p_i,ptt_su_pft(pgr),*9997)

!32-ASSOUT_POWER -r*4
c      r_val=0
c      call write_r(str_rec,p_i,r_val,*9997)
      call write_r(str_rec,p_i,power_tot(pgr),*9997)

!33-ASSOUT_POWER_PERC_DER -r*4
      call write_r(str_rec,p_i,power_perc_der(pgr),*9997)

cmar
cmar_
      if(power_perc_der(pgr).eq.0)then
      PERC_POW_STAZIONE(pgr)=power_perc_der(pgr)
	endif
cmar_

!33_BIS-PERC_POW_STAZIONE -r*4
      call write_r(str_rec,p_i,PERC_POW_STAZIONE(pgr),*9997)
cmar

!**-SI SERIE
      if(bSerie)then

!34-ASSOUT_2_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9997)

!35-ASSOUT_2_PORTATA_IN -r*4
         call write_r(str_rec,p_i,flow_stat_in(pgrApp),*9997)

!36-ASSOUT_2_PORTATA_OUT -r*4
         call write_r(str_rec,p_i,flow_stat(pgrApp),*9997)

!37-ASSOUT_2_P_ASP -r*4
         call write_r(str_rec,p_i,pasp_stat(pgrApp),*9997)

!38-ASSOUT_2_P_MAN -r*4
      if(press_mand_calc(pgrApp) .gt. 0.0) then
          call write_r(str_rec,p_i,press_mand_calc(pgrApp),*9997)
      else
          call write_r(str_rec,p_i,pmand_stat(pgrApp),*9997)        
      end if
!39-ASSOUT_2_T_ASP -r*4
         call write_r(str_rec,p_i,tasp_stat(pgrApp),*9997)

!40-ASSOUT_2_T_MAN -r*4
         call write_r(str_rec,p_i,tmand_stat(pgrApp),*9997)

!41-ASSOUT_2_RCOMPR -r*4
         call write_r(str_rec,p_i,comp_ratio(pgrApp),*9997)

!42-ASSOUT_2_TOTCONS -r*4
         call write_r(str_rec,p_i,tot_cons(pgrApp),*9997)

!43-ASSOUT_2_DELTAP_IN -r*4
         call write_r(str_rec,p_i,delpr1(pgrApp),*9997)

!44-ASSOUT_2_DELTAP_OUT -r*4
         call write_r(str_rec,p_i,delpr2(pgrApp),*9997)

!45-ASSOUT_2_HEAD -r*4
         call write_r(str_rec,p_i,head(pgrApp),*9997)

!46-ASSOUT_2_FLOWM -r*4
         call write_r(str_rec,p_i,flowm(pgrApp),*9997)

!47-ASSOUT_2_FLOWRIC -r*4
         call write_r(str_rec,p_i,q_riciclo(pgrApp),*9997)

!48-ASSOUT_2_CONS_SU_QOUT -r*4
         call write_r(str_rec,p_i,cons_su_qout(pgrApp),*9997)

!49-ASSOUT_2_FUEL_SU_PTT -r*4
         call write_r(str_rec,p_i,fuel_su_ptt(pgrApp),*9997)

!50-ASSOUT_2_PTT_SU_PFT -r*4
         call write_r(str_rec,p_i,ptt_su_pft(pgrApp),*9997)

!51-ASSOUT_2_POWER -r*4
c         r_val=0
c         call write_r(str_rec,p_i,r_val,*9997)
         call write_r(str_rec,p_i,power_tot(pgrApp),*9997)

!52-ASSOUT_2_POWER_PERC_DER -r*4
         call write_r(str_rec,p_i,power_perc_der(pgrApp),*9997)

!**-NO SERIE
      else

         r_val=0
!34-ASSOUT_2_PORTATA -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!35-ASSOUT_2_PORTATA_IN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!36-ASSOUT_2_PORTATA_OUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!37-ASSOUT_2_P_ASP -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!38-ASSOUT_2_P_MAN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!39-ASSOUT_2_T_ASP -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!40-ASSOUT_2_T_MAN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!41-ASSOUT_2_RCOMPR -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!42-ASSOUT_2_TOTCONS -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!43-ASSOUT_2_DELTAP_IN -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!44-ASSOUT_2_DELTAP_OUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!45-ASSOUT_2_HEAD -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!46-ASSOUT_2_FLOWM -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!47-ASSOUT_2_FLOWRIC -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!48-ASSOUT_2_CONS_SU_QOUT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!49-ASSOUT_2_FUEL_SU_PTT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!50-ASSOUT_2_PTT_SU_PFT -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!51-ASSOUT_2_POWER -r*4
         call write_r(str_rec,p_i,r_val,*9997)
!52-ASSOUT_2_POWER_PERC_DER -r*4
         call write_r(str_rec,p_i,r_val,*9997)

      endif
!**-

!53-ASSOUT_TIPO_SET -i*4
      call conv_tiporeg_sire_db (e_ce,iel,i_val)
      call write_i(str_rec,p_i,i_val,*9997)

!54-ASSOUT_SET_P_ASP -r*4
      call write_r(str_rec,p_i,pmsetj(iel),*9997)

!55-ASSOUT_SET_P_MAN -r*4
      call write_r(str_rec,p_i,pvsetj(iel),*9997)

!56-ASSOUT_SET_PORTATA -r*4
      call write_r(str_rec,p_i,qsettj(iel),*9997)

!57-ASSOUT_STATO_FUNZ_ID -i*4
      if    (hstatj(iel).eq.sta_pm)then
             i_val = sta_pasp
      elseif(hstatj(iel).eq.sta_pv)then
             i_val = sta_pman
      else
             i_val = hstatj(iel)
      endif
      call write_i(str_rec,p_i,i_val,*9997)

!58-ASSOUT_NUM_TBG_ATTIVI -i*4
      call write_i(str_rec,p_i,num_tbg_attivi(pgr),*9997)

!59-ASSOUT_2_NUM_TBG_ATTIVI -i*4
      call write_i(str_rec,p_i,num_tbg_attivi(pgrApp),*9997)

!60-ASSOUT_PCS -r*4
      r_val = udm_int(udm_pcs,pclstd(iel))
      call write_r(str_rec,p_i,r_val,*9997)

!61-ASSOUT_POWER_MAX_NOM -r*4
      call write_r(str_rec,p_i,nom_max_pow(pgr),*9997)

!62-ASSOUT_POWER_MAX_NOM_DER -r*4
      call write_r(str_rec,p_i,nom_max_pow_der(pgr),*9997)

!63-ASSOUT_POWER_MAX_NOM_RID -r*4
      call write_r(str_rec,p_i,nom_max_pow_vinc(pgr),*9997)

!64-ASSOUT_POWER_MIN -r*4
      call write_r(str_rec,p_i,nom_min_pow(pgr),*9997)

!65-ASSOUT_FL_ERR -i*4
      call write_i(str_rec,p_i,flertj(iel),*9997)

!66-ASSOUT_FL_FATAL -i*4
      call write_i(str_rec,p_i,fl_fatal(pgr),*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_ass_rete',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_turbogr (kun,assCompId,stadio,statoAccensione,
     -           unita,nomefile,str_err,*)

!*-Per SIMCENT: Scrive il file output 'turbogruppi_out.txt' (DB: turbogruppi_out_scen_simcent)
      implicit none
      include '../inc/param.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
      include '../inc/argo_reale_ris.inc'

      integer*4  kun                !I
      integer*4  assCompId          !I
      integer*4  stadio             !I
      character*(*) statoAccensione !I
      integer*4  unita              !I
      character*(*) nomefile        !I
      character*(*) str_err         !IO
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-TBGOUT_SCEN_ID -i*4
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-TBGOUT_TBG_COMP_ID -i*4
      call write_i(str_rec,p_i,tg_comp_id(kun),*9997)

!03-TBGOUT_ASS_COMP_ID -i*4
      call write_i(str_rec,p_i,assCompId,*9997)

!04-TBGOUT_STADIO -i*4
      call write_i(str_rec,p_i,stadio,*9997)

!05-TBGOUT_STATOACCENSIONE -c*3
      call write_c(str_rec,p_i,statoAccensione,*9997)

!06-TBGOUT_POWER -r*4
      call write_r(str_rec,p_i,power(kun),*9997)

!07-TBGOUT_POWER_ONSITE -r*4
      call write_r(str_rec,p_i,power_onsite(kun),*9997)

!08-TBGOUT_POWER_PERC -r*4
      call write_r(str_rec,p_i,power_perc(kun),*9997)

!09-TBGOUT_UNIT_POWER_ONSITE -r*4
c-sire2_0059      r_val=0
c-sire2_0059      call write_r(str_rec,p_i,r_val,*9997)
      call write_r(str_rec,p_i,unit_power(kun),*9997)
c-sire2_0059-end

!10-TBGOUT_POWER_PERC_VAR -r*4
      call write_r(str_rec,p_i,power_perc_var(kun),*9997)

!11-TBGOUT_POWER_PERC_REV -r*4
      call write_r(str_rec,p_i,power_perc_rev(kun),*9997)

!12-TBGOUT_HRATE_ONSITE -r*4
      call write_r(str_rec,p_i,unit_hrate_onsite(kun),*9997)

!13-TBGOUT_HRATE -r*4
      call write_r(str_rec,p_i,unit_hrate(kun),*9997)

!14-TBGOUT_HRATE_PERC -r*4
      call write_r(str_rec,p_i,unit_hrate_perc(kun),*9997)

!15-TBGOUT_EFF_TURB -r*4
      call write_r(str_rec,p_i,unit_eff_turb(kun),*9997)

!16-TBGOUT_CONS -r*4
      call write_r(str_rec,p_i,unit_cons(kun),*9997)

!17-TBGOUT_VCRIT -r*4
      call write_r(str_rec,p_i,unit_vcrit(kun),*9997)

!18-TBGOUT_VINC -i*4
      call write_i(str_rec,p_i,unit_vinc(kun),*9997)

!19-TBGOUT_UNIT_REV -r*4
      call write_r(str_rec,p_i,unit_rev(kun),*9997)

!20-TBGOUT_UNIT_REV_RPM -r*4
      call write_r(str_rec,p_i,unit_rev_rpm(kun),*9997)

!21-TBGOUT_UNIT_FLOW -r*4
      call write_r(str_rec,p_i,unit_flow(kun),*9997)

!22-TBGOUT_UNIT_HEAD -r*4
      call write_r(str_rec,p_i,unit_head(kun),*9997)

!23-TBGOUT_UNIT_EFF -r*4
      call write_r(str_rec,p_i,unit_eff(kun),*9997)

!24-TBGOUT_TEMP -r*4
      call write_r(str_rec,p_i,unit_temp(kun),*9997)

!25-TBGOUT_UNIT_POWER -r*4
c-sire2_0059      call write_r(str_rec,p_i,unit_power(kun),*9997)
      call write_r(str_rec,p_i,unit_power_iso(kun),*9997)
c-sire2_0059-end

!26-TBGOUT_POWER_RIC -r*4
      call write_r(str_rec,p_i,power_ric(kun),*9997)

!27-TBGOUT_POWER_RIC_PERC -r*4
      call write_r(str_rec,p_i,power_ric_perc(kun),*9997)

!28-TBGOUT_UNIT_MAX -r*4
      call write_r(str_rec,p_i,unit_max(kun),*9997)

!29-TBGOUT_UNIT_MIN -r*4
      call write_r(str_rec,p_i,unit_min(kun),*9997)

!30-TBGOUT_UNIT_PERC -r*4
      call write_r(str_rec,p_i,unit_perc(kun),*9997)

!31-TBGOUT_QOUT -r*4
      call write_r(str_rec,p_i,unit_qout(kun),*9997)

!32-TBGOUT_QPESO -r*4
      call write_r(str_rec,p_i,unit_qpeso(kun),*9997)

!33-TBGOUT_PTT -r*4
      call write_r(str_rec,p_i,unit_ptt(kun),*9997)

!34-TBGOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,unit_cons_su_qout(kun),*9997)

!35-TBGOUT_UNIT_PRES_INT -r*4
      call write_r(str_rec,p_i,unit_pres_int(kun),*9997)

!36-TBGOUT_UNIT_TEMP_INT -r*4
      call write_r(str_rec,p_i,unit_temp_int(kun),*9997)

!37-TBGOUT_UNIT_DELPRINT -r*4
      call write_r(str_rec,p_i,unit_delprint(kun),*9997)

!38-TBGOUT_UNIT_DELTRINT -r*4
      call write_r(str_rec,p_i,unit_deltrint(kun),*9997)

!39-TBGOUT_2_UNIT_REV -r*4
      call write_r(str_rec,p_i,unit2_rev(kun),*9997)

!40-TBGOUT_2_UNIT_REV_RPM -r*4
      call write_r(str_rec,p_i,unit2_rev_rpm(kun),*9997)

!41-TBGOUT_2_UNIT_FLOW -r*4
      call write_r(str_rec,p_i,unit2_flow(kun),*9997)

!42-TBGOUT_2_UNIT_HEAD -r*4
      call write_r(str_rec,p_i,unit2_head(kun),*9997)

!43-TBGOUT_2_UNIT_EFF -r*4
      call write_r(str_rec,p_i,unit2_eff(kun),*9997)

!44-TBGOUT_2_UNIT_POWER_ONSITE -r*4
c-sire2_0059      r_val=0
c-sire2_0059      call write_r(str_rec,p_i,r_val,*9997)
      call write_r(str_rec,p_i,unit2_power(kun),*9997)
c-sire2_0059-end

!45-TBGOUT_2_TEMP -r*4
      call write_r(str_rec,p_i,unit2_temp(kun),*9997)

!46-TBGOUT_2_UNIT_POWER -r*4
c-sire2_0059      call write_r(str_rec,p_i,unit2_power(kun),*9997)
      call write_r(str_rec,p_i,unit2_power_iso(kun),*9997)
c-sire2_0059-end

!47-TBGOUT_2_POWER_RIC -r*4
      call write_r(str_rec,p_i,power2_ric(kun),*9997)

!48-TBGOUT_2_POWER_RIC_PERC -r*4
      call write_r(str_rec,p_i,power2_ric_perc(kun),*9997)

!49-TBGOUT_2_UNIT_MAX -r*4
      call write_r(str_rec,p_i,unit2_max(kun),*9997)

!50-TBGOUT_2_UNIT_MIN -r*4
      call write_r(str_rec,p_i,unit2_min(kun),*9997)

!51-TBGOUT_2_UNIT_PERC -r*4
      call write_r(str_rec,p_i,unit2_perc(kun),*9997)

!52-TBGOUT_2_QOUT -r*4
      call write_r(str_rec,p_i,unit2_qout(kun),*9997)

!53-TBGOUT_2_QPESO -r*4
      call write_r(str_rec,p_i,unit2_qpeso(kun),*9997)

!54-TBGOUT_2_PTT -r*4
      call write_r(str_rec,p_i,unit2_ptt(kun),*9997)

!55-TBGOUT_2_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,unit2_cons_su_qout(kun),*9997)

!56-TBGOUT_TBG_COMP_UFF_ID -i*4
      i_val = 0
      call write_i(str_rec,p_i,i_val,*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_turbogr ',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine scrivi_turb_rete (kun,assCompId,stadio,statoAccensione,
     -           unita,nomefile,str_err,*)

!*-Per RETE: Scrive il file output 'turbogruppi_out.txt' (DB: turbogruppi_out_scen)
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
      include '../inc/argo_reale_ris.inc'
      include '../inc/scenario.inc'

      integer*4  kun                !I
      integer*4  assCompId          !I
      integer*4  stadio             !I
      character*(*) statoAccensione !I
      integer*4  unita              !I
      character*(*) nomefile        !I
      character*(*) str_err         !IO

      integer*4 p_i,stato,i_val
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val
      character*(max_len_rec) str_rec !**
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
      str_rec = ' '
      p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-TBGOUT_SCEN_ID -i*4
      call write_i(str_rec,p_i,scenario_id,*9997)
      
!02-TBGOUT_TBG_COMP_ID -i*4
      call write_i(str_rec,p_i,tg_comp_id(kun),*9997)

!03-TBGOUT_CAMPIONAMENTO -i*4
      call write_i(str_rec,p_i,ultimo_camp,*9997)

!04-TBGOUT_ISTANTE -r*4
      call write_r(str_rec,p_i,corr_ist,*9997)

!05-TBGOUT_ASS_COMP_ID -i*4
      call write_i(str_rec,p_i,assCompId,*9997)

!06-TBGOUT_STADIO -i*4
      call write_i(str_rec,p_i,stadio,*9997)

!07-TBGOUT_STATO -i*4
      call write_i(str_rec,p_i,unit_avail(kun),*9997)

!08-TBGOUT_ATTIVAZIONE -i*4
      if(tipoScenario.eq.TIPOSCEN_RETE_sta)then
         if(statoAccensione.eq.funz_on)then
            unit_attiv_tlc(kun)=on
         else
            unit_attiv_tlc(kun)=off
         endif
      endif
      call write_i(str_rec,p_i,unit_attiv_tlc(kun),*9997)

!09-TBGOUT_STATOACCENSIONE -c*3
      call write_c(str_rec,p_i,statoAccensione,*9997)

!10-TBGOUT_POTENZA_CORR -r*4
      call write_r(str_rec,p_i,nom_pow_der(kun),*9997)

!11-TBGOUT_POWER -r*4
      call write_r(str_rec,p_i,power(kun),*9997)

!12-TBGOUT_POWER_ONSITE -r*4
      call write_r(str_rec,p_i,power_onsite(kun),*9997)

!13-TBGOUT_POWER_PERC -r*4
      call write_r(str_rec,p_i,power_perc(kun),*9997)

!14-TBGOUT_POWER_PERC_VAR -r*4
      call write_r(str_rec,p_i,power_perc_var(kun),*9997)

!15-TBGOUT_POWER_PERC_REV -r*4
      call write_r(str_rec,p_i,power_perc_rev(kun),*9997)

!16-TBGOUT_HRATE_ONSITE -r*4
      call write_r(str_rec,p_i,unit_hrate_onsite(kun),*9997)

!17-TBGOUT_HRATE -r*4
      call write_r(str_rec,p_i,unit_hrate(kun),*9997)

!18-TBGOUT_HRATE_PERC -r*4
      call write_r(str_rec,p_i,unit_hrate_perc(kun),*9997)

!19-TBGOUT_EFF_TURB -r*4
      call write_r(str_rec,p_i,unit_eff_turb(kun),*9997)

!20-TBGOUT_CONS -r*4
      call write_r(str_rec,p_i,unit_cons(kun),*9997)

!21-TBGOUT_VCRIT -r*4
      call write_r(str_rec,p_i,unit_vcrit(kun),*9997)

!22-TBGOUT_VINC -i*4
      call write_i(str_rec,p_i,unit_vinc(kun),*9997)

!23-TBGOUT_UNIT_REV -r*4
      call write_r(str_rec,p_i,unit_rev(kun),*9997)

!24-TBGOUT_UNIT_REV_RPM -r*4
      call write_r(str_rec,p_i,unit_rev_rpm(kun),*9997)

!25-TBGOUT_UNIT_FLOW -r*4
      call write_r(str_rec,p_i,unit_flow(kun),*9997)

!26-TBGOUT_UNIT_HEAD -r*4
      call write_r(str_rec,p_i,unit_head(kun),*9997)

!27-TBGOUT_UNIT_EFF -r*4
      call write_r(str_rec,p_i,unit_eff(kun),*9997)

!28-TBGOUT_TEMP -r*4
      call write_r(str_rec,p_i,unit_temp(kun),*9997)

!29-TBGOUT_UNIT_POWER -r*4
      call write_r(str_rec,p_i,unit_power(kun),*9997)

!30-TBGOUT_UNIT_POWER_ISO -r*4
      call write_r(str_rec,p_i,unit_power_iso(kun),*9997)

!31-TBGOUT_POWER_RIC -r*4
      call write_r(str_rec,p_i,power_ric(kun),*9997)

!32-TBGOUT_POWER_RIC_PERC -r*4
      call write_r(str_rec,p_i,power_ric_perc(kun),*9997)

!33-TBGOUT_UNIT_MAX -r*4
      call write_r(str_rec,p_i,unit_max(kun),*9997)

!34-TBGOUT_UNIT_MIN -r*4
      call write_r(str_rec,p_i,unit_min(kun),*9997)

!35-TBGOUT_UNIT_PERC -r*4
      call write_r(str_rec,p_i,unit_perc(kun),*9997)

!36-TBGOUT_QOUT -r*4
      call write_r(str_rec,p_i,unit_qout(kun),*9997)

!37-TBGOUT_QPESO -r*4
      call write_r(str_rec,p_i,unit_qpeso(kun),*9997)

!38-TBGOUT_PTT -r*4
      call write_r(str_rec,p_i,unit_ptt(kun),*9997)

!39-TBGOUT_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,unit_cons_su_qout(kun),*9997)

!40-TBGOUT_UNIT_PRES_INT -r*4
      call write_r(str_rec,p_i,unit_pres_int(kun),*9997)

!41-TBGOUT_UNIT_TEMP_INT -r*4
      call write_r(str_rec,p_i,unit_temp_int(kun),*9997)

!42-TBGOUT_UNIT_DELPRINT -r*4
      call write_r(str_rec,p_i,unit_delprint(kun),*9997)

!43-TBGOUT_UNIT_DELTRINT -r*4
      call write_r(str_rec,p_i,unit_deltrint(kun),*9997)

!44-TBGOUT_2_UNIT_REV -r*4
      call write_r(str_rec,p_i,unit2_rev(kun),*9997)

!45-TBGOUT_2_UNIT_REV_RPM -r*4
      call write_r(str_rec,p_i,unit2_rev_rpm(kun),*9997)

!46-TBGOUT_2_UNIT_FLOW -r*4
      call write_r(str_rec,p_i,unit2_flow(kun),*9997)

!47-TBGOUT_2_UNIT_HEAD -r*4
      call write_r(str_rec,p_i,unit2_head(kun),*9997)

!48-TBGOUT_2_UNIT_EFF -r*4
      call write_r(str_rec,p_i,unit2_eff(kun),*9997)

!49-TBGOUT_2_TEMP -r*4
      call write_r(str_rec,p_i,unit2_temp(kun),*9997)

!50-TBGOUT_2_UNIT_POWER -r*4
      call write_r(str_rec,p_i,unit2_power(kun),*9997)

!51-TBGOUT_2_UNIT_POWER_ISO -r*4
      call write_r(str_rec,p_i,unit2_power_iso(kun),*9997)

!52-TBGOUT_2_POWER_RIC -r*4
      call write_r(str_rec,p_i,power2_ric(kun),*9997)

!53-TBGOUT_2_POWER_RIC_PERC -r*4
      call write_r(str_rec,p_i,power2_ric_perc(kun),*9997)

!54-TBGOUT_2_UNIT_MAX -r*4
      call write_r(str_rec,p_i,unit2_max(kun),*9997)

!55-TBGOUT_2_UNIT_MIN -r*4
      call write_r(str_rec,p_i,unit2_min(kun),*9997)

!56-TBGOUT_2_UNIT_PERC -r*4
      call write_r(str_rec,p_i,unit2_perc(kun),*9997)

!57-TBGOUT_2_QOUT -r*4
      call write_r(str_rec,p_i,unit2_qout(kun),*9997)

!58-TBGOUT_2_QPESO -r*4
      call write_r(str_rec,p_i,unit2_qpeso(kun),*9997)

!59-TBGOUT_2_PTT -r*4
      call write_r(str_rec,p_i,unit2_ptt(kun),*9997)

!60-TBGOUT_2_CONS_SU_QOUT -r*4
      call write_r(str_rec,p_i,unit2_cons_su_qout(kun),*9997)
!-
!**-Fine Elenco dati-**

!-Scrittura record
      write (unita,10000,err=9998,iostat=stato) str_rec(1:p_i-1)
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9997  continue
      stato=0
9998  continue
      pf1 = s_trim_d(nomefile)
      str_err = 'Errore scrittura: ' // nomefile(1:pf1)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_turb_rete',str_err(1:pf2),stato)
      str_err=' '
      close (unit=unita,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
	subroutine getStatoAcc (pgr,kun,statoAccensione,bAcceso)

	implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'

	integer*4 pgr                 !I)
	integer*4 kun                 !I)
	character*(*) statoAccensione !O)
	logical*2 bAcceso             !O)

	if (tipo_criterio(pgr).eq.crit_man) then
		!stato rilevato dalla simulazione
		statoAccensione = status (kun)
	else
		!stato rilevato dall'ottimo
		if (lstatus (kun) .eq. ON) then
			statoAccensione = 'ON'
		else
			statoAccensione = 'OFF'
		endif
	endif

	bAcceso = statoAccensione(1:2) .eq. 'ON'

	return
	end
!===================================================================================
	subroutine scrivi_Ris_Ele (str_err,*)

!*-Per RETE: Scrive i file output:
!-1)'punti_out.txt'       e_pu                     (DB: punti_out_scen)
!-2)'tronchi_out.txt'     e_tr                     (DB: tronchi_out_scen)
!-3)'regolazioni_out.txt' e_vg                     (DB: regolazioni_out_scen)
!-4)'riduzioni_out.txt'   e_vd                     (DB: riduzioni_out_scen)
!-5)'centrali_s_out.txt'  e_cs                     (DB: centrali_s_out_scen)
!-6)'valvole_out.txt'     e_vl,e_vc (e_vs,e_vr)    (DB: valvole_out_scen)
!-7)'unari_out.txt'       e_im,e_pz,e_st,e_pr      (DB: unari_out_scen)
!-8)'stacco_out.txt'      e_vt,e_ut                (DB: punti_stacco_out_scen)

      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/cin_app_ele.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'
      include '../inc/flag.inc'
      include '../inc/ric.inc'

      character*(*) str_err  !IO

      integer*4 iel,stato,p_i,i_val,tipo
      integer*4 pf1,pf2,s_trim_d
      real*4 r_val,ro,amount,x,vm,vv,qmon,qval
      real*4 udm_int ! function
c-din
      real*4 dq,dpa2,dpv2
c-din-end
      logical*2 lro_ok
      character*30 nomefile
      character*(max_len_rec) str_rec
!--------------------------------------------------
10000 format(A)
!--------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**1)-PUNTI_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_punti
!---------------------------------
!=>Open file
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_punti,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_pu),etipti(e_pu)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-PUOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-PUOUT_PU_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-PUOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-PUOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-PUOUT_PRESSIONE -r*4
         call write_r(str_rec,p_i,prestj(iel),*9992)

!06-PUOUT_TEMP_GAS -r*4
         call write_r(str_rec,p_i,temptj(iel),*9992)

!07-PUOUT_CONSUMO_ISTANTANEO -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!08-PUOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!09-PUOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!10-PUOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!11-PUOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!12-PUOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!13-PUOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!14-PUOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!15-PUOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!16-PUOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!17-PUOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!18-PUOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!19-PUOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**

!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**2)-TRONCHI_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
cmar-----------------
cmar-----------------

      nomefile = wf_tronchi
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_tronchi,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_tr),etipti(e_tr)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-TROUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-TROUT_TRO_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-TROUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-TROUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-TROUT_SCAMBIO_TERMICO -r*4  (W/m2K) ove conntx in W/mK
         r_val = conntx(iel)/rluntx(iel)
         call write_r(str_rec,p_i,r_val,*9992)

!06-TROUT_DISLIVELLO -r*4 
         call write_r(str_rec,p_i,disltx(iel),*9992)

!07-TROUT_VOLUME_TOT -r*4  (m3)
         r_val = (volntx(iel)*2)*1000
         call write_r(str_rec,p_i,r_val,*9992)

!08-TROUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!09-TROUT_QUANTITA_GAS -r*4 =amount (knm3)
c-densità media ro



         ro=(rocitj(opumto(iel))+rocitj(opuvto(iel)))/2.



         lro_ok=((rocitj(opumto(iel)).ne.0.  .and.
     -            rocitj(opuvto(iel)).ne.0.) .and.
     -           (ro0td(iel).ne.0.))
c-amount: volume gas presente nel tronco
         if(lro_ok)then
            amount = ro*(volntx(iel)*2)/ro0td(iel)
         else
            amount = 0.
         endif
         call write_r(str_rec,p_i,amount,*9992)

!10-TROUT_VELOCITA -r*4  (m/sec)
c-densità media ro definita sopra



         r_val=((diamtx(iel)*diamtx(iel)*ro)*atan(1.)*3.6)


c-velocità gas
         if((lro_ok).and.(abs(r_val).gt.0.))then
             r_val = (abs(porttj(iel))*ro0td(iel))/r_val
         else
             r_val = 0.
         endif
         call write_r(str_rec,p_i,r_val,*9992)

!11-TROUT_INVASO -r*4  portata volumetrica (knm3/h)
         x = porttj(iel)*ro0td(iel)
         vm = volntx(iel)/volntx(opumto(iel))
         vv = volntx(iel)/volntx(opuvto(iel))
c-droty: variazione della densità nel tempo (bilancio di portata nei punti)
         if (f_densita.eq.1) then
           qmon = (x+droty(opumto(iel))*vm)/ro0td(opumto(iel))
           qval = (x-droty(opuvto(iel))*vv)/ro0td(opuvto(iel))
	   else
           qmon = (x+droty(opumto(iel))*vm)
           qval = (x-droty(opuvto(iel))*vv)
	   endif
c-invaso: bilancio gas come differenza tra volume entrante e uscente
         r_val = qmon - qval
         call write_r(str_rec,p_i,r_val,*9992)

!12-TROUT_PCS -r*4  (kJ/kNm3)
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!13-TROUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!14-TROUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!15-TROUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!16-TROUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!17-TROUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!18-TROUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!19-TROUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!20-TROUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!21-TROUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!22-TROUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!23-TROUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**
CMAR_TR_VELOCITà_MONTE

!24-TROUT_VELOCITA -r*4  (m/sec)
c-densità media ro definita sopra
         r_val=((diamtx(iel)*diamtx(iel)*rocitj(opumto(iel))
     *   )*atan(1.)*3.6)
c-velocità gas
         if((lro_ok).and.(abs(r_val).gt.0.))then
             r_val = (abs(porttj(iel))*ro0td(iel))/r_val
         else
             r_val = 0.
         endif
         call write_r(str_rec,p_i,r_val,*9992)
CMAR_TR_VELOCITà_MONTE



CMAR_TR_VELOCITà_VALLE

!25-TROUT_VELOCITA -r*4  (m/sec)
c-densità media ro definita sopra
         r_val=((diamtx(iel)*diamtx(iel)*rocitj(opuvto(iel))
     *   )*atan(1.)*3.6)
c-velocità gas
         if((lro_ok).and.(abs(r_val).gt.0.))then
             r_val = (abs(porttj(iel))*ro0td(iel))/r_val
         else
             r_val = 0.
         endif
         call write_r(str_rec,p_i,r_val,*9992)
CMAR_TR_VELOCITà_VALLE


!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo




!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**3)-REGOLAZIONI_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_regolazioni
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_regolazioni,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_vg),etipti(e_vg)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-RGOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-RGOUT_RG_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-RGOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-RGOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-RGOUT_ALTITUDINE -r*4
         r_val= (altetx(opumto(iel))+altetx(opuvto(iel)))/2.
         call write_r(str_rec,p_i,r_val,*9992)

!06-RGOUT_PORTATA_MAX -r*4
         call write_r(str_rec,p_i,qmaxtx(iel),*9992)

!07-RGOUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!08-RGOUT_NUM_LINEE_ATTIVE -i*4
         call write_i(str_rec,p_i,line_vg_on(pgrato(iel)),*9992)

!09-RGOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (e_vg,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!10-RGOUT_SET_PMONTE -r*4
         call write_r(str_rec,p_i,pmsetj(iel),*9992)

!11-RGOUT_SET_PVALLE -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!12-RGOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!13-RGOUT_STATO_FUNZ_ID -i*4
         call write_i(str_rec,p_i,hstatj(iel),*9992)

!14-RGOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!15-RGOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!16-RGOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!17-RGOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!18-RGOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!19-RGOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!20-RGOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!21-RGOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!22-RGOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!23-RGOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!24-RGOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!25-RGOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**

!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**4)-RIDUZIONI_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_riduzioni
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_riduzioni,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_vd),etipti(e_vd)

         str_rec = ' '
         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-RDOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-RDOUT_RD_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-RDOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-RDOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-RDOUT_ALTITUDINE -r*4
         r_val= (altetx(opumto(iel))+altetx(opuvto(iel)))/2.
         call write_r(str_rec,p_i,r_val,*9992)

!06-RDOUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!07-RDOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (e_vd,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!08-RDOUT_SET_PMONTE -r*4
         call write_r(str_rec,p_i,pmsetj(iel),*9992)

!09-RDOUT_SET_PVALLE -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!10-RDOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!11-RDOUT_STATO_FUNZ_ID -i*4
         call write_i(str_rec,p_i,hstatj(iel),*9992)

!12-RDOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!13-RDOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!14-RDOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!15-RDOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!16-RDOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!17-RDOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!18-RDOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!19-RDOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!20-RDOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!21-RDOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!22-RDOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!23-RDOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**

!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**5)-CENTRALI_S_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_centrali_s
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_centrali_s,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_cs),etipti(e_cs)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 

!**-Inizio Elenco dati-**
!-
!01-CESOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-CESOUT_CES_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-CESOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-CESOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-CESOUT_ALTITUDINE -r*4
         r_val= (altetx(opumto(iel))+altetx(opuvto(iel)))/2.
         call write_r(str_rec,p_i,r_val,*9992)

!06-CESOUT_PORTATA -r*4 (in mandata)
         call write_r(str_rec,p_i,porttj(iel),*9992)

!07-CESOUT_PORTATA_MACCHINA -r*4
         call write_r(str_rec,p_i,flowm_cs(pgrato(iel)),*9992)

!08-CESOUT_PREVALENZA -r*4
         call write_r(str_rec,p_i,head_cs(pgrato(iel)),*9992)

!09-CESOUT_CONSUMO -r*4
c-din-corr
c         if (fl_din) then
c           dq = 0.
c           dpa2 = 0.
c           dpv2 = 0.
c           call qconce(porttj(iel),prestj(opumto(iel)),
c     -          prestj(opuvto(iel)),iel,consty(iel),dq,dpa2,dpv2)
c         endif
c-din-corr-end
         call write_r(str_rec,p_i,consty(iel),*9992)

!10-CESOUT_POTENZA -r*4
!-potenza totale = consumo*potere cal_inf/consumo energetico 
c         if(abs(ceuptx(iel)).gt.0.)then
c            r_val = ((consty(iel)*pclitd(opuvto(iel)))/ceuptx(iel))
ccout     -              /3600.
c         else
c            r_val = r_und
c         endif
c         call write_r(str_rec,p_i,r_val,*9992)
         call write_r(str_rec,p_i,power_cs(pgrato(iel)),*9992)

!11-CESOUT_RAPP_COMP -r*4
!-rapporto compressione = p2+perd_car2/p1-perd_car1
         r_val= prestj(opumto(iel))-dpin(pgrato(iel))
         if(abs(r_val).gt.0.)then
            r_val = (prestj(opuvto(iel))+dpout(pgrato(iel)))/r_val
         else
            r_val = r_und
         endif
         call write_r(str_rec,p_i,r_val,*9992)

!12-CESOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (e_cs,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!13-CESOUT_SET_P_ASP -r*4
         call write_r(str_rec,p_i,pmsetj(iel),*9992)

!14-CESOUT_SET_P_MAN -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!15-CESOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!16-CESOUT_STATO_FUNZ_ID -i*4
         if    (hstatj(iel).eq.sta_pm)then
               i_val = sta_pasp
         elseif(hstatj(iel).eq.sta_pv)then
               i_val = sta_pman
         else
               i_val = hstatj(iel)
         endif
         call write_i(str_rec,p_i,i_val,*9992)

!17-CESOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!18-CESOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!19-CESOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!20-CESOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!21-CESOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!22-CESOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!23-CESOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!24-CESOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!25-CESOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!26-CESOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!27-CESOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!28-CESOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)

cmar_add_CS_OUT

!29-CESOUT_DP_asp
      call write_r(str_rec,p_i,dp_asp_cs(pgrato(iel)),*9992)

!30-CESOUT_DP_asp
      call write_r(str_rec,p_i,dp_man_cs(pgrato(iel)),*9992)

!31-CESOUT_Taria
      call write_r(str_rec,p_i,taria_cs(pgrato(iel)),*9992)

!32-CESOUT_Pn_corr
      call write_r(str_rec,p_i,pn_corr(pgrato(iel)),*9992)

cmar_add_CS_OUT
!-
!**-Fine Elenco dati-**

!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**6)-VALVOLE_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_valvole
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_valvole,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_vl),etipti(e_vl)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_valvole (e_vl,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

      do iel = otipti(e_vc),etipti(e_vc)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_valvole (e_vc,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!      do iel = otipti(e_vs),etipti(e_vs)
!         str_rec = ' '
!         p_i = 1 !posizione inizio dati 
!!**-Inizio Elenco dati-**
!         call	definisci_out_valvole (e_vs,iel,p_i,str_rec,*9992)
!!-Scrittura record
!         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)
!      enddo
!
!      do iel = otipti(e_vr),etipti(e_vr)
!        str_rec = ' '
!         p_i = 1 !posizione inizio dati 
!!**-Inizio Elenco dati-**
!         call	definisci_out_valvole (e_vr,iel,p_i,str_rec,*9992)
!!-Scrittura record
!         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)
!
!      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**7)-UNARI_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_unari
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_unari,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_im),etipti(e_im)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_unari (e_im,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

      do iel = otipti(e_pz),etipti(e_pz)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_unari (e_pz,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

      do iel = otipti(e_st),etipti(e_st)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_unari (e_st,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

      do iel = otipti(e_pr),etipti(e_pr)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_unari (e_pr,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**8)-STACCO_OUT.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = wf_stacco
!---------------------------------
!=>Open file di output
      call open_vfile(uwf_file,c_path_dirlog,'_'//wf_stacco,
     - 'REPLACE',stato,*9990)

      do iel = otipti(e_vt),etipti(e_vt)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_stacco (e_vt,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

      do iel = otipti(e_ut),etipti(e_ut)

         str_rec = ' '
         p_i = 1 !posizione inizio dati 
!**-Inizio Elenco dati-**
         call	definisci_out_stacco (e_ut,iel,p_i,str_rec,*9992)
!-Scrittura record
         write (uwf_file,10000,err=9994,iostat=stato) str_rec(1:p_i-1)

      enddo

!=>Close file di output
      close (uwf_file,err=9996,iostat=stato)

!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9990  continue
      str_err = 'Open errata'
      goto 9998
9992  continue
      stato=0
9994  continue
      str_err = 'Errore scrittura'
      goto 9998
9996  continue
      str_err = 'Close errata'
9998  continue
      pf1 = s_trim_d(nomefile)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Scrivi_Ris_Ele',
     - nomefile(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (uwf_file,err=9999)
9999  continue
      return 1
      end
!===================================================================================
      subroutine definisci_out_valvole (tipoel,iel,p_i,str_rec,*)

!-Definisce i campi per il file output di valvole: e_vl,e_vc (e_vs,e_vr)
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/tj.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'

      integer*4     tipoel  !I tipo elemento
      integer*4     iel     !I
      integer*4     p_i     !I/O
      character*(*) str_rec !I/O

      integer*4 i_val
      real*4 r_val
      real*4 udm_int ! function
!--------------------------------------------------
!**-Inizio Elenco dati-**
!-
!01-VLOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-VLOUT_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-VLOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-VLOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-VLOUT_ALTITUDINE -r*4
         r_val= (altetx(opumto(iel))+altetx(opuvto(iel)))/2.
         call write_r(str_rec,p_i,r_val,*9992)

!06-VLOUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!07-VLOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (tipoel,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!08-VLOUT_SET_PMONTE -r*4
         call write_r(str_rec,p_i,pmsetj(iel),*9992)

!09-VLOUT_SET_PVALLE -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!10-VLOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!11-VLOUT_STATO_FUNZ_ID -i*4
         call write_i(str_rec,p_i,hstatj(iel),*9992)

!12-VLOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!13-VLOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!14-VLOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!15-VLOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!16-VLOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!17-VLOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!18-VLOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!19-VLOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!20-VLOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!21-VLOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!22-VLOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!23-VLOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9992  continue
      return 1
      end
!===================================================================================
      subroutine definisci_out_unari (tipoel,iel,p_i,str_rec,*)

!-Definisce i campi per il file output di unari (e_im,e_pz,e_st,e_pr)
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'

      integer*4     tipoel  !I tipo elemento
      integer*4     iel     !I
      integer*4     p_i     !I/O
      character*(*) str_rec !I/O

      integer*4 i_val
      real*4 r_val
      real*4 udm_int ! function
!--------------------------------------------------
!**-Inizio Elenco dati-**
!-
!01-UNOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-UNOUT_ELEM_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-UNOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-UNOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-UNOUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!06-UNOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (tipoel,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!07-UNOUT_SET_PRESSIONE -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!08-UNOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!09-UNOUT_TEMP -r*4
         call write_r(str_rec,p_i,tsettj(iel),*9992)

!10-UNOUT_STATO_FUNZ_ID -i*4
         if(hstatj(iel).eq.sta_pv)then
            i_val = sta_pu
         else
            i_val = hstatj(iel)
         endif
         call write_i(str_rec,p_i,i_val,*9992)

!11-UNOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!12-UNOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!13-UNOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!14-UNOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!15-UNOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!16-UNOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!17-UNOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!18-UNOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!19-UNOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!20-UNOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!21-UNOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!22-UNOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9992  continue
      return 1
      end
!===================================================================================
      subroutine definisci_out_stacco (tipoel,iel,p_i,str_rec,*)

!-Definisce i campi per il file output di stacco (e_vt,e_ut)
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tc.inc'
      include '../inc/td.inc'
      include '../inc/tx.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/conv.inc'
      include '../inc/scenario.inc'

      integer*4     tipoel  !I tipo elemento
      integer*4     iel     !I
      integer*4     p_i     !I/O
      character*(*) str_rec !I/O

      integer*4 i_val
      real*4 r_val
      real*4 udm_int ! function
!--------------------------------------------------
!**-Inizio Elenco dati-**
!-
!01-STAOUT_SCEN_ID -i*4 
         call write_i(str_rec,p_i,scenario_id,*9992)
      
!02-STAOUT_STA_ID -i*4
         call write_i(str_rec,p_i,elem_id(iel),*9992)
            
!03-STAOUT_CAMPIONAMENTO -i*4
         call write_i(str_rec,p_i,ultimo_camp,*9992)

!04-STAOUT_ISTANTE -r*4
         call write_r(str_rec,p_i,corr_ist,*9992)

!05-STAOUT_ALTITUDINE -r*4
         if(tipoel.eq.e_vt)then
            r_val= (altetx(opumto(iel))+altetx(opuvto(iel)))/2.
         else
            r_val= r_und !per e_ut
         endif   
         call write_r(str_rec,p_i,r_val,*9992)

!06-STAOUT_PORTATA -r*4
         call write_r(str_rec,p_i,porttj(iel),*9992)

!07-STAOUT_TIPO_REG_ID -i*4
         call conv_tiporeg_sire_db (tipoel,iel,i_val)
         call write_i(str_rec,p_i,i_val,*9992)

!08-STAOUT_SET_PRESSIONE -r*4
         call write_r(str_rec,p_i,pvsetj(iel),*9992)

!09-STAOUT_SET_PORTATA -r*4
         call write_r(str_rec,p_i,qsettj(iel),*9992)

!10-STAOUT_TEMP -r*4
         if(tipoel.eq.e_vt)then
            r_val= r_und
         else
            r_val= tsettj(iel) !per e_ut
         endif
         call write_r(str_rec,p_i,r_val,*9992)

!11-STAOUT_STATO_FUNZ_ID -i*4
         if(tipoel.eq.e_vt)then
            i_val = hstatj(iel)
         else
            if(hstatj(iel).eq.sta_pv)then
               i_val = sta_pu      !per e_ut
            else
               i_val = hstatj(iel) !per e_ut
            endif
         endif   
         call write_i(str_rec,p_i,i_val,*9992)

!12-STAOUT_PCS -r*4
         r_val = udm_int(udm_pcs,pclstd(iel))
         call write_r(str_rec,p_i,r_val,*9992)

!13-STAOUT_METANO -r*4
         call write_r(str_rec,p_i,comptc(iel,1),*9992)

!14-STAOUT_ETANO -r*4
         call write_r(str_rec,p_i,comptc(iel,2),*9992)

!15-STAOUT_PROPANO -r*4
         call write_r(str_rec,p_i,comptc(iel,3),*9992)

!16-STAOUT_NBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,4),*9992)

!17-STAOUT_IBUTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,5),*9992)

!18-STAOUT_PENTANO -r*4
         call write_r(str_rec,p_i,comptc(iel,6),*9992)

!19-STAOUT_ESANO -r*4
         call write_r(str_rec,p_i,comptc(iel,7),*9992)

!20-STAOUT_AZOTO -r*4
         call write_r(str_rec,p_i,comptc(iel,8),*9992)

!21-STAOUT_CO2 -r*4
         call write_r(str_rec,p_i,comptc(iel,9),*9992)

!22-STAOUT_H2S -r*4
         call write_r(str_rec,p_i,comptc(iel,10),*9992)

!23-STAOUT_FL_ERR -i*4
         call write_i(str_rec,p_i,flertj(iel),*9992)
!-
!**-Fine Elenco dati-**
!--------------------------------------------------
      return

!--------------------------------------------------
!->Errori
9992  continue
      return 1
      end
!===================================================================================
