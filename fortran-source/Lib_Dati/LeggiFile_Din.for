!===================================================================================
      subroutine ripartenza (str_err,*)

!*-Legge i file input di Ripartenza Dinamico per:
!  1)Elementi RETE (ripart_elemrete) con solo tronchi e punti effettivi
!  2)Centrali RETE (ripart_centrali)
!*-Aggiorna i valori per eventuali tronchi e punti fittizi
      implicit none
      character*(*) str_err  !IO
!---------------------------------
      call ripart_elemrete (str_err,*9999)

      call ripart_centrali (str_err,*9999)

      call ripart_ele_fitt ()

      return

9999  continue 
      return 1
      end
!===================================================================================
      subroutine ripart_ele_fitt ()

!*-In Ripartenza Dinamico distribuisce i valori letti dalla ripartenza rete reale
!  sui punti e tronchi fittizi dei tronchi scomposti.
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/tc.inc'
      include '../inc/scomp_tronchi.inc'

      integer*4 pos_ini_fitt,pos_fitt,pos_ori,jel
      integer*4 pos_app,pnt_ori,pnt_fitt
      real*4 lung_prog,delta_pres
!===========================================
!-Test presenza scomposizioni: False=No scomposizioni
      if(.not.tr_flag_scomp)then
         return
      endif
!===========================================
!-I tronchi fittizi sono memorizzati dopo tr_ori_etipti (etipti tronchi originali)
      pos_ini_fitt = tr_ori_etipti+1  !pos inizio tronchi fittizi
      pos_app = 0

      do pos_fitt=pos_ini_fitt,etipti(e_tr) !loop fino a etipti con fittizi

!-Recupero puntatore a tronco padre da tr_ori_pmo (inutilizzato per tronchi originali)
         pos_ori = tr_ori_pmo(pgrato(pos_fitt))

!-portata tronco fittizio
         porttj(pos_fitt)=porttj(pos_ori)

	   if(porttj(pos_ori).lt.0.) then
	      pnt_ori = opuvto(pos_ori)
	   else
	      pnt_ori = tr_ori_pmo(pgrato(pos_ori))
	   endif
	   pnt_fitt = opuvto(pos_fitt)

!-temperatura,composizione e pressione punto valle fittizio del tronco fittizio
         temptj(pnt_fitt)=temptj(pnt_ori)
         do jel=1,10
            comptc(pnt_fitt,jel)=comptc(pnt_ori,jel)
         enddo
               
         if(pos_app.eq.pos_ori)then
	      lung_prog = lung_prog + rluntx(pos_fitt)
         else
	      pos_app = pos_ori
	      lung_prog = rluntx(pos_fitt)			   
         endif

         delta_pres = (prestj(opuvto(pos_ori)) -
     -                  prestj(tr_ori_pmo(pgrato(pos_ori))))
     -                * lung_prog/tr_ori_lun(pgrato(pos_ori))
         prestj(pnt_fitt) = prestj(tr_ori_pmo(pgrato(pos_ori))) 
     -                      + delta_pres
     
      enddo
!===========================================
      return
      end
!===================================================================================
      subroutine ripart_elemrete (str_err,*)

!*-Legge i file input di Ripartenza Dinamico per gli Elementi di RETE:
!1)punti_din.txt       (DB: v_punti_dinamica)        e_pu
!2)tronchi_din.txt     (DB: v_tronchi_dinamica)      e_tr
!3)regolazioni_din.txt (DB: v_regolazioni_dinamica)  e_vg
!4)riduzioni_din.txt   (DB: v_riduzioni_dinamica)    e_vd
!5)centrali_s_din.txt  (DB: v_centrali_s_dinamica)   e_cs
!6)valvole_din.txt     (DB: v_valvole_dinamica)      e_vl,e_vc (no: e_vs,e_vr)
!7)unari_din.txt       (DB: v_unari_dinamica)        e_im,e_pz,e_st,e_pr
!8)stacco_din.txt      (DB: v_punti_stacco_dinamica) e_vt,e_ut
      implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/tc.inc'
      include '../inc/ty.inc'

      character*(*) str_err  !IO

      integer*4 pos,p_i,stato,i_val,iel,jel,elem_tipo,elemId
      integer*4 pf1,pf2,s_trim_d
      logical*2 eof,nullo
      character*30 nomefile           !**
      character*(max_len_rec) str_rec !**
!---------------------------------
10000 format(A)
!---------------------------------
	eof=.false.  !eof gestione lettura dei file
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**1)-PUNTI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_punti
      iel = 0                !*contatore record
      pos = otipti(e_pu) - 1 !*pos partenza tabellone
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_punti,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=1000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-PUOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-PUOUT_PU_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         pos = pos + 1  !*pos tabellone
         call pos_elem_tipo (e_pu,elemId,pos,*9997)
            
!03-PUOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-PUOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-PUOUT_PRESSIONE -r*4
         call read_r(str_rec,p_i,prestj(pos),nullo,*9994)

!06-PUOUT_TEMP_GAS -r*4
         call read_r(str_rec,p_i,temptj(pos),nullo,*9994)

!07-PUOUT_CONSUMO_ISTANTANEO -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!08-PUOUT_METANO -r*4
!....................
!17-PUOUT_H2S    -r*4
         do jel=1,10
            call read_r(str_rec,p_i,comptc(pos,jel),nullo,*9994)
         enddo
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
1000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**2)-TRONCHI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_tronchi
      iel = 0                !*contatore record
      pos = otipti(e_tr) - 1 !*pos partenza tabellone
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_tronchi,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=2000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-TROUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-TROUT_TRO_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         pos = pos + 1  !*pos tabellone
         call pos_elem_tipo (e_tr,elemId,pos,*9997)
            
!03-TROUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-TROUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-TROUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
2000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**3)-REGOLAZIONI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_regolazioni
      iel = 0                !*contatore record
      pos = otipti(e_vg) - 1 !*pos partenza tabellone
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_regolazioni,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=3000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-RGOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-RGOUT_RG_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         pos = pos + 1  !*pos tabellone
         call pos_elem_tipo (e_vg,elemId,pos,*9997)
            
!03-RGOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-RGOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-RGOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!06-RGOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!07-RGOUT_SET_PMONTE -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!08-RGOUT_SET_PVALLE -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!09-RGOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!10-RGOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,hstatj(pos),nullo,*9994)
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
3000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**4)-RIDUZIONI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_riduzioni
      iel = 0                !*contatore record
      pos = otipti(e_vd) - 1 !*pos partenza tabellone
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_riduzioni,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=4000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-RDOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-RDOUT_RD_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         pos = pos + 1  !*pos tabellone
         call pos_elem_tipo (e_vd,elemId,pos,*9997)
            
!03-RDOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-RDOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-RDOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!06-RDOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!07-RDOUT_SET_PMONTE -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!08-RDOUT_SET_PVALLE -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!09-RDOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!10-RDOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,hstatj(pos),nullo,*9994)
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
4000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**5)-CENTRALI_S_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_centrali_s
      iel = 0                !*contatore record
      pos = otipti(e_cs) - 1 !*pos partenza tabellone
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_centrali_s,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=5000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-CESOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-CESOUT_CES_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         pos = pos + 1  !*pos tabellone
         call pos_elem_tipo (e_cs,elemId,pos,*9997)
            
!03-CESOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-CESOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-CESOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!06-CESOUT_CONSUMO -r*4
         call read_r(str_rec,p_i,consty(pos),nullo,*9994)

!07-CESOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!08-CESOUT_SET_P_ASP -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!09-CESOUT_SET_P_MAN -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!10-CESOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!11-CESOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if(i_val.eq.sta_pasp)then
            hstatj(pos) = sta_pm
         elseif(i_val.eq.sta_pman)then
            hstatj(pos) = sta_pv
         else
            hstatj(pos) = i_val
         endif
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
5000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**6)-VALVOLE_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-elem_tipo: e_vl=2, e_vc=21 (no: e_vs=14, e_vr=15) 
!---------------------------------
      nomefile = df_valvole
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_valvole,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=6000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-VLOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-VL_TIPO_ELEM_ID -i*4 
         call read_i(str_rec,p_i,elem_tipo,nullo,*9994)

!*Test tipo elemento ammesso
         if((elem_tipo.ne.e_vl) .and.
     -      (elem_tipo.ne.e_vc))then
            goto 9993  !*Tipo elemento errato
         endif
      
!03-VLOUT_VL_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         do pos=otipti(elem_tipo),etipti(elem_tipo)
            if(elem_id(pos).eq.elemId)then
               goto 5500 !*OK trovato
            endif
         enddo
         goto 9997  !*KO non trovato
5500     continue
            
!04-VLOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!05-VLOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!06-VLOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!07-VLOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!08-VLOUT_SET_PMONTE -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!09-VLOUT_SET_PVALLE -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!10-VLOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!11-VLOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,hstatj(pos),nullo,*9994)
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
6000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**7)-UNARI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-elem_tipo: e_im=4, e_pz=5, e_st=6, e_pr=8
!-im,pz (ent.dir.=+1), st(ent./usc. dir.=+/-1), pr(usc.dir.=-1) 
!---------------------------------
      nomefile = df_unari
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_unari,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=7000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-UNOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-UNA_TIPO_ELEM_ID -i*4 
         call read_i(str_rec,p_i,elem_tipo,nullo,*9994)

!*Test tipo elemento ammesso
         if((elem_tipo.ne.e_im) .and.
     -      (elem_tipo.ne.e_pz) .and.
     -      (elem_tipo.ne.e_st) .and.
     -      (elem_tipo.ne.e_pr))then
            goto 9993  !*Tipo elemento errato
         endif
      
!03-UNOUT_UNA_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         do pos=otipti(elem_tipo),etipti(elem_tipo)
            if(elem_id(pos).eq.elemId)then
               goto 6500 !*OK trovato
            endif
         enddo
         goto 9997  !*KO non trovato
6500     continue
            
!04-UNOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!05-UNOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!06-UNOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!07-UNOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!08-UNOUT_SET_PRESSIONE -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!09-UNOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!10-UNOUT_TEMP -r*4
         call read_r(str_rec,p_i,tsettj(pos),nullo,*9994)
         if(nullo)then
            tsettj(pos) = temptj(opuvto(pos)) !**default temperatura punto
         endif

!11-UNOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if(i_val.eq.sta_pu)then
            hstatj(pos) = sta_pv
         else
            hstatj(pos) = i_val
         endif
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
7000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**8)-STACCO_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-2 tipi: e_vt valvola stacco e e_ut unario stacco
      nomefile = df_stacco
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_stacco,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=8000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-STAOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
            
!02-STAOUT_STA_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca prima elemId nel tabellone e_vt
         do pos=otipti(e_vt),etipti(e_vt)
            if(elem_id(pos).eq.elemId)then
               goto 7500 !*OK trovato
            endif
         enddo
!*Ricerca elemId nel tabellone e_ut
         do pos=otipti(e_ut),etipti(e_ut)
            if(elem_id(pos).eq.elemId)then
               goto 7500 !*OK trovato
            endif
         enddo
         goto 9997  !*KO non trovato
7500     continue
            
!03-STAOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-STAOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-STAOUT_PORTATA -r*4
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)

!06-STAOUT_TIPO_REG_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!07-STAOUT_SET_PRESSIONE -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!08-STAOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!09-STAOUT_TEMP -r*4
         call read_r(str_rec,p_i,tsettj(pos),nullo,*9994)
         if(nullo)then
            tsettj(pos) = temptj(opuvto(pos)) !**default temperatura punto
         endif

!10-STAOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if(i_val.eq.sta_pu)then
            hstatj(pos) = sta_pv
         else
            hstatj(pos) = i_val
         endif
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
8000  continue
      close (udf_file,err=9996,iostat=stato)

      return
!---------------------------------
!->Errori
9990  continue
      str_err = 'Open errata'
      goto 9998
9993  continue
      stato=0
      call msgerr_key (iel,'Tipo Elemento errato in Record:',str_err)
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
      call msgerr_key (iel,'Id.Elemento errato in Record:',str_err)
      goto 9998
9998  continue
      pf1 = s_trim_d(nomefile)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Ripart_elemrete',
     -     nomefile(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (udf_file,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine ripart_centrali (str_err,*)

!*-Legge i file input di Ripartenza Dinamico per le Centrali di RETE:
!1)centrali_din.txt    (DB: v_centrali_rete_dinamica) e_ce
!2)assetti_din.txt     (DB: v_assetti_rete_dinamica)  e_ce
!3)turbogruppi_din.txt (DB: v_turbogruppi_dinamica)   e_ce
      implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'
      include '../inc/th.inc'
      include '../inc/ti.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/tj.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/stations.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'

      character*(*) str_err  !IO

      integer*4 pos,p_i,stato,i_val,iel,jel,elemId,asscompId,tgcompId
      integer*4 kun,stadio,pgr,pf1,pf2,s_trim_d
      logical*2 eof,nullo
      character*3 statoAccensione
      character*30 nomefile           !**
      character*(max_len_rec) str_rec !**
!---------------------------------
10000 format(A)
!---------------------------------
	eof=.false.  !eof gestione lettura dei file
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**1)-CENTRALI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_centrali
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_centrali,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=1000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-CEOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-CEOUT_CE_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)

!*Ricerca elemId nel tabellone
         do pos=otipti(e_ce),etipti(e_ce)
            if(elem_id(pos).eq.elemId)then
               goto 500 !*OK trovato
            endif
         enddo
         goto 9997  !*KO non trovato
500      continue
            
!03-CEOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-CEOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-CEOUT_PORTATA_OUT -r*4
c-ripartenza-corr         call read_r(str_rec,p_i,flow_stat(pgrato(pos)),nullo,*9994)
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)
c-ripartenza-corr-end

!06-CEOUT_TOTCONS -r*4
         call read_r(str_rec,p_i,tot_cons(pgrato(pos)),nullo,*9994)

!07-CEOUT_TIPO_SET -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!08-CEOUT_SET_P_ASP -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!09-CEOUT_SET_P_MAN -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!10-CEOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!11-CEOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if    (i_val.eq.sta_pasp)then
            hstatj(pos) = sta_pm
         elseif(i_val.eq.sta_pman)then
            hstatj(pos) = sta_pv
         else
            hstatj(pos) = i_val
         endif
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
1000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**2)-ASSETTI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_assetti
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_assetti,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=2000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-ASSOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-ASSOUT_ASS_COMP_ID -i*4
         call read_i(str_rec,p_i,asscompId,nullo,*9994)

!*Ricerca asscompId
         do pos = otipti(e_ce),etipti(e_ce)
            if(ass_comp_id(pgrato(pos)).eq.asscompId)then
               goto 1500 !*OK trovato
            endif
         enddo
         goto 9992 !*KO non trovato
1500     continue
            
!03-ASSOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!04-ASSOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!05-ASSOUT_PORTATA_OUT -r*4
c-ripartenza-corr         call read_r(str_rec,p_i,flow_stat(pgrato(pos)),nullo,*9994)
         call read_r(str_rec,p_i,porttj(pos),nullo,*9994)
c-ripartenza-corr-end

!06-ASSOUT_TOTCONS -r*4
         call read_r(str_rec,p_i,tot_cons(pgrato(pos)),nullo,*9994)

!07-ASSOUT_TIPO_SET -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)
         call conv_tiporeg_db_sire (i_val,htcitj(pos))

!08-ASSOUT_SET_P_ASP -r*4
         call read_r(str_rec,p_i,pmsetj(pos),nullo,*9994)

!09-ASSOUT_SET_P_MAN -r*4
         call read_r(str_rec,p_i,pvsetj(pos),nullo,*9994)

!10-ASSOUT_SET_PORTATA -r*4
         call read_r(str_rec,p_i,qsettj(pos),nullo,*9994)

!11-ASSOUT_STATO_FUNZ_ID -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if    (i_val.eq.sta_pasp)then
            hstatj(pos) = sta_pm
         elseif(i_val.eq.sta_pman)then
            hstatj(pos) = sta_pv
         else
            hstatj(pos) = i_val
         endif
!-
!**-Fine Elenco dati-**

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
2000  continue
      close (udf_file,err=9996,iostat=stato)

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!**3)-TURBOGRUPPI_DIN.TXT
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      nomefile = df_turbogruppi
      iel = 0 !*contatore record
!---------------------------------
!=>Open file
      call open_vfile (udf_file,c_path_dirdati,'_'//df_turbogruppi,
     -    'OLD',stato,*9990)

!===Inizio_Loop_lettura================================
      dowhile (.not.eof)

         iel = iel + 1
         read (udf_file,10000,end=3000,err=9995,iostat=stato) str_rec

         p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-TBGOUT_SCEN_ID -i*4 
         call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-TBG_COMP_CE_ELEM_ID -i*4
         call read_i(str_rec,p_i,elemId,nullo,*9994)
            
!03-TBGOUT_TBG_COMP_ID -i*4
         call read_i(str_rec,p_i,tgcompId,nullo,*9994)

!04-TBGOUT_ASS_COMP_ID -i*4
         call read_i(str_rec,p_i,asscompId,nullo,*9994)

!*Ricerca elemId e asscompId
         do pos = otipti(e_ce),etipti(e_ce)
            if(elem_id(pos).eq.elemId)then
               if(ass_comp_id(pgrato(pos)).eq.asscompId)then
                  goto 2300 !*OK trovati elemId e asscompId
               endif
            endif
         enddo
!*KO non trovato o elemId o asscompId
         do pos=otipti(e_ce),etipti(e_ce)
            if(elem_id(pos).eq.elemId)then
               goto 9992 !*KO asscompId non trovato pur trovando elemId
            endif
         enddo
         goto 9997  !*KO elemId non trovato
2300     continue

!05-TBGOUT_CAMPIONAMENTO -i*4
         call read_i(str_rec,p_i,ultimo_camp,nullo,*9994)

!06-TBGOUT_ISTANTE -r*4
         call read_r(str_rec,p_i,corr_ist,nullo,*9994)

!07-TBGOUT_STADIO -i*4
         call read_i(str_rec,p_i,stadio,nullo,*9994)

         if(stadio.ne.fase_2)then
            pgr = pgrato(pos)
         else
            pgr = pgrato_app(pos)
         endif
!*Ricerca tgcompId
         do jel=1,unit_num(pgr)
            kun = first_unit(pgr) + jel - 1
            if(tg_comp_id(kun).eq.tgcompId)then
               goto 2500 !*OK turbogruppo trovato
            endif
         enddo

ctest_tbg
ctest_tbg         goto 9993 !*KO turbogruppo non trovato
         goto 2600
ctest_tbg-end

2500     continue

!08-TBGOUT_STATO -i*4
         call read_i(str_rec,p_i,unit_avail(kun),nullo,*9994)

!09-TBGOUT_ATTIVAZIONE -i*4
         call read_i(str_rec,p_i,i_val,nullo,*9994)

         if(i_val.eq.on)then
            unit_attiv_tlc(kun) = on
         else
            unit_attiv_tlc(kun) = off
         endif

!10-TBGOUT_STATOACCENSIONE -c*3
         call read_c(str_rec,p_i,statoAccensione,*9994)

         if(statoAccensione.eq.funz_on)then
		  status(kun) = funz_on 
c		  lstatus(kun) = ON
         else
		  status(kun) = funz_off 
c		  lstatus(kun) = OFF
         endif
!-
!**-Fine Elenco dati-**

ctest_tbg
2600     continue
ctest_tbg-end

      enddo
!===Fine_Loop_lettura==================================

!=>EOF finale
3000  continue
      close (udf_file,err=9996,iostat=stato)

      return
!---------------------------------
!->Errori
9990  continue
      str_err = 'Open errata'
      goto 9998
9992  continue
      stato=0
      call msgerr_key (iel,'Id.Assetto errato in Record:',str_err)
      goto 9998
9993  continue
      stato=0
      call msgerr_key (iel,'Id.Turbogr. errato in Record:',str_err)
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
      call msgerr_key (iel,'Id.Centrale errato in Record:',str_err)
      goto 9998
9998  continue
      pf1 = s_trim_d(nomefile)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Ripart_centrali',
     -     nomefile(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (udf_file,err=9999)
9999  continue 
      return 1
      end
!===================================================================================
      subroutine open_file_tlc (*)

!*-Apre il file Telecomandi ('xxx'_telecomandi.txt)
      implicit none
      include '../inc/param.inc'
	include '../inc/err.inc'

      integer*4 stato,pf1,pf2,s_trim_d
	character*(l_D_ERR) str_err
!---------------------------------
      call open_vfile (udf_tlc,
     -     c_path_dirdati,'_'//df_telecomandi,'OLD',stato,*9990)

      return
!---------------------------------
!->Errore
9990  continue 
      str_err = 'Open errata'
      pf1 = s_trim_d(df_telecomandi)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Open_file_tlc',
     -     df_telecomandi(1:pf1)//' - '//str_err(1:pf2),stato)

      return 1
      end
!===================================================================================
      subroutine close_file_tlc (*)

!*-Chiude il file Telecomandi ('xxx'_telecomandi.txt)
      implicit none
      include '../inc/param.inc'
	include '../inc/err.inc'

      integer*4 stato,pf1,pf2,s_trim_d
	character*(l_D_ERR) str_err
!---------------------------------
      close (udf_tlc,err=9990,iostat=stato)

      return
!---------------------------------
!->Errore
9990  continue
      str_err = 'Close errata'
      pf1 = s_trim_d(df_telecomandi)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Close_file_tlc',
     -     df_telecomandi(1:pf1)//' - '//str_err(1:pf2),stato)

      return 1
      end
!===================================================================================
      subroutine rewind_file_tlc (*)

!*-Riposiziona ad inizio lettura il file Telecomandi ('xxx'_telecomandi.txt)
      implicit none
      include '../inc/param.inc'
	include '../inc/err.inc'

      integer*4 stato,pf1,pf2,s_trim_d
	character*(l_D_ERR) str_err
!---------------------------------
      rewind (udf_tlc,err=9990,iostat=stato)

      return
!---------------------------------
!->Errore
9990  continue
      str_err = 'Rewind errata'
      pf1 = s_trim_d(df_telecomandi)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Rewind_file_tlc',
     -     df_telecomandi(1:pf1)//' - '//str_err(1:pf2),stato)

      return 1
      end
!===================================================================================
      subroutine leggi_file_tlc (ier,*)

!*-Legge il file Telecomandi ('xxx'_telecomandi.txt)
!*-Aggiorna l'area wb.inc
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/stations.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'
	include '../inc/wb.inc'
	include '../inc/err.inc'

      integer*4 ier  !O (0=lettura record effettuata, 1=eof) 

      integer*4 p_i,stato,elem_tipo,jel,i_val,attivaz
      integer*4 elemId,asscompId,tgcompId,pos,kun
      integer*4 pf1,pf2,s_trim_d
      logical*2 nullo
      character*(max_len_rec) str_rec
	character*(l_D_ERR) str_err
!---------------------------------
10000 format(A)
!---------------------------------
!-Elementi Rete con Telecomandi
!---------------------------------
!-e_ce  3 centrale bipolare
!-e_cs 20 centrale semplificata
!-e_vd 18 impianto di riduzione
!-e_vg 19 impianto di regolazione
!-e_vl  2 valvola semplificata
!-e_vc 21 valvola collegamento nodo ridotto
!-e_im  4 unario importazione
!-e_pz  5 unario pozzo
!-e_st  6 unario stoccaggio
!-e_pr  8 unario prelievo interrompibile
!-e_ut 23 unario di stacco
!---------------------------------
      ier = 1 !*esito lettura con eof
!---------------------------------
!->Lettura record
      read (udf_tlc,10000,end=3000,err=9995,iostat=stato) str_rec

      p_i = 1 !posizione inizio dati

!**-Inizio Elenco dati-**
!-
!01-SCENARIO -i*4
      call read_i(str_rec,p_i,i_val,nullo,*9994)
      
!02-ISTANTE -r*4
      call read_r(str_rec,p_i,timeWB,nullo,*9994) !**timeWB

!03-DURATA -r*4
      call read_r(str_rec,p_i,tritWB,nullo,*9994) !**tritWB

!04-TIPO_ID -i*4
      call read_i(str_rec,p_i,elem_tipo,nullo,*9994)
c 
      if (elem_tipo .eq. e_cm) elem_tipo = e_ce

!------------------------------------------------------
!**Test sul tipo elemento
      if((elem_tipo.ne.e_ce) .and.
     -   (elem_tipo.ne.e_cs) .and.
     -   (elem_tipo.ne.e_vd) .and.
     -   (elem_tipo.ne.e_vg) .and.
     -   (elem_tipo.ne.e_vl) .and.
     -   (elem_tipo.ne.e_vc) .and.
     -   (elem_tipo.ne.e_im) .and.
     -   (elem_tipo.ne.e_pz) .and.
     -   (elem_tipo.ne.e_st) .and.
     -   (elem_tipo.ne.e_pr) .and.
     -   (elem_tipo.ne.e_ut))then
          goto 9991  !*Tipo elemento errato
      endif
!------------------------------------------------------
      
!05-ELEM_ID -i*4
      call read_i(str_rec,p_i,elemId,nullo,*9994)

!------------------------------------------------------
!**Ricerca se elemId presente in Rete
      do pos=otipti(elem_tipo),etipti(elem_tipo)
         if(elem_id(pos).eq.elemId)then
            oftoWB = pos                        !**oftoWB
            goto 500 !*OK trovato
         endif
      enddo
      goto 9997 !*KO non trovato
500   continue
!------------------------------------------------------
            
!06-ASS_COMP_ID -i*4 (solo per e_ce)
      call read_i(str_rec,p_i,asscompId,nullo,*9994)

!------------------------------------------------------
!**Ricerca asscompId solo nel caso di centrale bipolare
      if(elem_tipo.eq.e_ce)then

         do pos=otipti(elem_tipo),etipti(elem_tipo)
            if(elem_id(pos).eq.elemId)then
               if(ass_comp_id(pgrato(pos)).eq.asscompId)then
                  oftoWB = pos                  !**oftoWB
                  goto 1000 !*OK trovato
               endif
            endif
         enddo
         goto 9992 !*KO non trovato
1000     continue

      endif
!------------------------------------------------------

!07-TIPO_REG_ID -i*4 (se nullo: lasciato = 0 per validare tc_und)
      call read_i(str_rec,p_i,i_val,nullo,*9994)
      call conv_tiporeg_db_sire (i_val,hregWB)    !**hregWB

!08-SET_PMONTE -r*4 (solo per Binari)
      call read_r(str_rec,p_i,pmonWB,nullo,*9994) !**pmonWB

!09-SET_PVALLE -r*4
      call read_r(str_rec,p_i,pvalWB,nullo,*9994) !**pvalWB

!10-SET_PORTATA -r*4
      call read_r(str_rec,p_i,qsetWB,nullo,*9994) !**qsetWB

!11-TEMP -r*4 (solo per Unari)
      call read_r(str_rec,p_i,tsetWB,nullo,*9994) !**tsetWB
      if(nullo)then
         tsetWB = r_und
      endif

!12-TBG_COMP_ID -i*4 (solo per e_ce)
      call read_i(str_rec,p_i,tgcompId,nullo,*9994)

!------------------------------------------------------
!**Ricerca tgcompId solo nel caso di centrale bipolare e se tgcompId.ne.NULLO
      if(.not.nullo.and.elem_tipo.eq.e_ce)then

         do jel=1,unit_num(pgrato(pos))
            kun = first_unit(pgrato(pos))+jel-1
            if(tg_comp_id(kun).eq.tgcompId)then
               otbgWB = kun                       !**otbgWB
               goto 1500  !*OK trovato
            endif
         enddo
ctest_tbg
ctest_tbg         goto 9993 !*KO non trovato
         goto 1600
ctest_tbg-end

1500     continue

!*se con assetti in Serie verifica disponibilità stadio_1 o stadio_2          
	   if(pgrato_app(pos).ne.pgrato(pos))then
            if(unit_avail(kun).eq.0)then
!*stadio_1 non disponibile: definisce puntatore stadio_2
               otbgWB = first_unit(pgrato_app(pos))+jel-1 !**otbgWB
            endif
         endif

!13-ATTIVAZIONE -i*4 (solo per e_ce)
         call read_i(str_rec,p_i,attivaz,nullo,*9994)

!**Assegna attivazione
         if(attivaz.eq.on)then
            if(unit_avail(otbgWB).gt.0)then
               atbgWB = on  !**atbgWB
            else
               atbgWB = off !**atbgWB
            endif
         else
            atbgWB = off    !**atbgWB
         endif

      else
         otbgWB = i_und
         atbgWB = off
      endif

ctest_tbg
1600  continue
ctest_tbg-end

!------------------------------------------------------
!-
!**-Fine Elenco dati-**

      ier = 0 !*esito OK lettura senza eof

!=>EOF finale
3000  continue

      return
!---------------------------------
!->Errori
9991  continue
      stato=0
      str_err = 'Tipo elemento errato'
      goto 9998
9992  continue
      stato=0
      str_err = 'Id.Assetto errato'
      goto 9998
9993  continue
      stato=0
      str_err = 'Id.Turbogr. errato'
      goto 9998
9994  continue
      stato=0
9995  continue
      str_err = 'Lettura errata'
      goto 9998
9997  continue
      stato=0
      str_err = 'Id.Elemento errato'
      goto 9998
9998  continue
      pf1 = s_trim_d(df_telecomandi)
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error (1,0,'Leggi_file_tlc',
     -     df_telecomandi(1:pf1)//' - '//str_err(1:pf2),stato)
      str_err=' '
      close (udf_tlc,err=9999)
9999  continue 
      ier = 0 !*esito KO lettura senza eof
      return 1
      end
!===================================================================================
      subroutine interrupt_utente (fl_interrupt)

!*Controlla la presenza di un file interrupt generato da utente
!*durante una simulazione dinamica, per ottenere una interruzione.
      implicit none
      include '../inc/param.inc'

      logical*2 fl_interrupt  !O
!---------------------------------
!-Test presenza del file di interrupt
      inquire(file=trim(c_path_dirdati)//'_'//trim(df_interrupt),
     -  Exist=fl_interrupt)
     
      return
      end
!===================================================================================
