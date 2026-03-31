c*************************************************************
c   subroutines per la preparazione dei dati di input
c*************************************************************
      subroutine tabella_valvole_grigliato(num_cl_ns,num_lg_ns,
     *                                     id_cl,id_lg,id_vlg,
     *                                     vlg_id_lg,vlg_id_cl,
     *                                     vlg_stato,
     *                                     vlg_stato_griglia)

	implicit none

	include '../inc/nodo_ridotto.inc '

	integer*4 num_cl_ns,
     *          num_lg_ns
      integer*1 id_cl(*),
     *          id_lg(*),
     *          id_vlg (*),
     *          vlg_id_lg (*),
     *          vlg_id_cl (*)
      integer*4 vlg_stato(*)  

      integer*4 vlg_stato_griglia(max_cl_nsm,max_lg_nsm)   !(O)

      logical*2 confronta_Guid
      external  confronta_Guid
     

      integer*4 num_vlg,i_riga,i_colo,i_cl,i_lg,iel,i_vlg
c------------------------------------------------------------------------
      num_vlg=num_cl_ns*num_lg_ns
c---------------->inizializzo la tabella 	
      do i_riga=1,num_cl_ns
         do i_colo=1,num_lg_ns
            vlg_stato_griglia(i_riga,i_colo) = STATO_IND
	   end do
	end do
c---------------->definisco la tabella

      do i_riga=1,num_cl_ns
	   do i_colo=1,num_lg_ns
	      loop:do iel=1,num_vlg
               if(confronta_Guid(iel,i_riga,vlg_id_cl,id_cl).and.
     *            confronta_Guid(iel,i_colo,vlg_id_lg,id_lg))then
     	            vlg_stato_griglia(i_riga,i_colo)=vlg_stato(iel)
	            !vlg_stato: 0 -->OFF   1-->ON   2-->LIBERO
	            exit loop
	         end if
	      end do loop
	   end do
      end do

	return
	end
c***************************************************************************************************

      subroutine link_collettori (id_pu,id_cl,vg_tab,
     *                             n_cl,n_pt,cl_link_vg)
!*************************************************************************************
!  Questa subroutine serve per creare la matrice CL_LINK_VG che fornisce informazioni
!  sulla connessione dei collettori data dalle valvole di grigliato.
!************************************************************************************* 
 
      implicit none
	include '../inc/nodo_ridotto.inc '	
	
	integer*4 n_cl    !numero collettori   (I)
     *         ,n_pt    !numero punti griglia   (I)
      integer*1 id_cl(*)
     *         ,id_pu(*) 

	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)   !(I)

      logical*2 cl_link_vg(max_cl_nsm,max_cl_nsm)     !(O)

      integer*4 k,kk,i,ind_k,ind_kk,ind_i 

	logical*2 flag    
c----------------------------------------------------------------------------------------
c     INIZIALIZZO LE VARIABILI DI OUTPUT
c
	do k = 1,n_cl
         do  kk = 1,n_cl
	       cl_link_vg (k,kk) = .false.
         end do
	end do

	do k = 1,n_cl
         do  kk = 1,n_cl
             if (kk.eq.k) then                   !termini diagonali
	           cl_link_vg(k,k) = .true.
	       else
	           flag = .false.
	           do i=1,n_pt
	              if(vg_tab(k,i).eq.STATO_ON.and.    !collettori in bypass
     *                 vg_tab(kk,i).eq.STATO_ON) then
	                 flag = .true.
	                 exit
	              else if (vg_tab(k,i).eq.STATO_RID_CL_PT.and.
     *                 vg_tab(kk,i).eq.STATO_RID_CL_PT) then !riduzione tra cl
	             	 flag = .true.
	                 exit
	              end if
			    end do
	            cl_link_vg (k,kk) = flag
	        end if
         end do
      end do

	return
	end
c*******************************************************************************************
      subroutine tabella_collettori
     *                             (num_cl_ns,num_vr_cl_ns,num_vd_cl_ns,
     *                              num_vg_cl_ns,num_ce_cl_ns,
     *                              num_mf_cl_ns,num_ass_cl_ns,
     *                              num_cs_cl_ns,
     *                              id_cl,id_vr_cl,id_vd_cl,id_vg_cl,
     *                              id_ce_cl,id_ass_cl,id_cs_cl,
     *                              vr_id_clm,vr_id_clv,
     *                              vd_id_clm,vd_id_clv,
     *                              vg_id_clm,vg_id_clv,
     *                              ce_id_cl_asp,ce_id_cl_man,
     *                              ass_id_cl_asp,ass_id_cl_man,
     *                              cs_id_cl_asp,cs_id_cl_man,
     *                              assetto_attivo,
     *                              cl_tab,elem_id_cl)

	implicit none
	include '../inc/nodo_ridotto.inc '	

c---------------------------------------->numero elementi
      integer*4 num_cl_ns        !num.collettori
     *         ,num_vr_cl_ns     !num.valvole tra collettori
     *         ,num_vd_cl_ns     !num.impianti rid.tra collettori
     *         ,num_vg_cl_ns     !num.impianti reg.tra collettori
     *         ,num_ce_cl_ns     !num.centrali binarie
     *         ,num_mf_cl_ns     !num.centrali mf
     *         ,num_ass_cl_ns    !num.assetti attivi mf
     *         ,num_cs_cl_ns     !num.centrali semplificate
c---------------------------------------->id. elementi
      integer*1 id_cl(*)          !id collettori
     *         ,id_vr_cl(*)    !id riduzioni tra cl
     *         ,id_vd_cl(*)    !id impianti rid.tra cl
     *         ,id_vg_cl(*)    !id impianti reg.tra cl
     *         ,id_ce_cl(*)    !id ce binaria tra cl
     *         ,id_ass_cl(*)   !id assetti tra cl
     *         ,id_cs_cl(*)    !id ce semplificate tra cl
c---------------------------------------->connessioni ai collettori
     *         ,vr_id_clm(*)  !id coll monte della vr
     *         ,vr_id_clv(*)  !id coll monte della vr
     *         ,vd_id_clm(*)  !id coll monte della vd
     *         ,vd_id_clv(*)  !id coll monte della vd
     *         ,vg_id_clm(*)  !id coll monte della vg
     *         ,vg_id_clv(*)  !id coll monte della vg
     *         ,ce_id_cl_asp(*)  !id coll monte della ce
     *         ,ce_id_cl_man(*)  !id coll monte della ce
     *         ,ass_id_cl_asp(*) !id coll monte dell'ass
     *         ,ass_id_cl_man(*) !id coll monte dell'ass
     *         ,cs_id_cl_asp(*)  !id coll monte della cs
     *         ,cs_id_cl_man(*)  !id coll monte della cs

	logical*2 assetto_attivo(max_assetti_nsm)   !(I)

 	integer*4 cl_tab(max_cl_nsm,max_cl_nsm)   !link diretti tra collettori(O)
      integer*1 elem_id_cl(max_cl_nsm,max_cl_nsm*max_lunguid)               (O)

      integer*4  iel,jel,iel_m,iel_v,id_coll,ind_vr,ind_vd,ind_vg,
     *           ind_ce,ind_ass,ind_cs,lun

	logical*2 confronta_Guid
      external  confronta_Guid
c-----------------------------------------------------------------------------------------
c  Inizializzo i vettori di output
  
      lun=num_cl_ns*lunguid  
      do iel=1,num_cl_ns
         do jel=1,num_cl_ns
            cl_tab(iel,jel) = NESSUN_LINK    !0=nessun collegamento diretto 
         end do
	   do jel=1,lun
       	  elem_id_cl(iel,jel) = 0
	   end do
	end do

c-----------------------------------------------------------------------------------------
c    Ricerca di valvole di riduzione tra collettori   (VR_COL)
c-----------------------------------------------------------------------------------------
      if (num_vr_cl_ns.gt.0) then
         do jel=1,num_vr_cl_ns    !jel -valvole di riduzione
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,vr_id_clm,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,vr_id_clv,id_cl))then
	               iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
	           cl_tab(iel_m,iel_v)=VR_COL
                 call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_vr_cl,elem_id_cl)
            end if
         end do !jel
      end if
c-----------------------------------------------------------------------------------------
c    Ricerca di impianti di riduzione tra collettori (VD_COL)
c-----------------------------------------------------------------------------------------
      if (num_vd_cl_ns.gt.0) then
         do jel=1,num_vd_cl_ns    !jel -valvole di riduzione
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,vd_id_clm,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,vd_id_clv,id_cl))then
	               iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
	           cl_tab(iel_m,iel_v)=VD_COL
                 call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_vd_cl,elem_id_cl)
            end if
         end do !jel
      end if
c-----------------------------------------------------------------------------------------
c    Ricerca impianti di regolazione tra collettori (VG_COL)
c-----------------------------------------------------------------------------------------
      if (num_vg_cl_ns.gt.0) then
         do jel=1,num_vg_cl_ns    !jel -valvole di riduzione
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,vg_id_clm,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,vg_id_clv,id_cl))then
	               iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
	          cl_tab(iel_m,iel_v)=VG_COL
               call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_vg_cl,elem_id_cl)
            end if
         end do !jel
      end if
c-----------------------------------------------------------------------------------------
c    Ricerca centrali/assetti tra collettori (centrale bipolare CE_BIN
c                                             assetto mf- ASS_MF
c                                             centrale semplificata-CE_SEMP)
c-----------------------------------------------------------------------------------------
      if (num_ce_cl_ns.gt.0) then
         do jel=1,num_ce_cl_ns    !jel -valvole di riduzione
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,ce_id_cl_asp,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,ce_id_cl_man,id_cl))then
	              iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
	          cl_tab(iel_m,iel_v)=CE_BIN
                 call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_ce_cl,elem_id_cl)
            end if
         end do !jel
c	   go to  100
      end if

      if (num_mf_cl_ns.gt.0) then
        do jel=1,num_ass_cl_ns    !jel -valvole di riduzione
	    if (assetto_attivo(jel)) then
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,ass_id_cl_asp,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,ass_id_cl_man,id_cl))then
	              iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
	          cl_tab(iel_m,iel_v)=ASS_MF
                call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_ass_cl,elem_id_cl)
            end if
	    end if
        end do !jel
c	  go to  100
      end if

      if (num_cs_cl_ns.gt.0) then
         do jel=1,num_cs_cl_ns    !jel -valvole di riduzione
            iel_m=0
	      iel_v=0
            do iel=1,num_cl_ns    !iel -collettori
c---------------->ricerca punto monte
               if (iel_m.eq.0) then
                  if(confronta_Guid(jel,iel,cs_id_cl_asp,id_cl))then
	              iel_m=iel
	            end if
	         endif
c---------------->ricerca punto valle
               if (iel_v.eq.0) then
                  if(confronta_Guid(jel,iel,cs_id_cl_man,id_cl))then
	              iel_v=iel
	            end if
	         endif
            end do   !iel

            if ((iel_m.gt.0).and.(iel_v.gt.0).and.(iel_m.ne.iel_v))then
                cl_tab(iel_m,iel_v)=CE_SEMP
                call copia_Guid_m(jel,max_cl_nsm,iel_m,iel_v,
     *                                id_cs_cl,elem_id_cl)
            end if
         end do !jel
c	   go to  100
      end if



100	continue

	return
	end
c*********************************************************************************************