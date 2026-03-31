c******************************************************************************
      subroutine verifica_collettori(num_cl_ns,num_lg_ns,
     *                        num_mf_cl_ns,num_ass_cl_ns,
     *                        id_cl,id_ass_cl,
     *                        ass_id_cl_asp,ass_id_cl_man,
     *                        num_mf_nsrid,
     *                        num_ass_nsrid,
     *                        id_ass_nsrid,
     *                        assetto_attivo,
     *                        vlg_stato_griglia)
c****************************************************************************************
c     Definisce il vettore ASSETTO_ATTIVO che informa sull'attivazione (=inserimento nel
c     nodo ridotto) degli assetti topologici della eventuale centrale mf inserita nel nodo.
c     Inoltre implementa il seguente controllo:
c     le valvole di grigliato sui collettori di aspirazione e mandata di assetti 
c     non attivati, vengono posti in stato OFF (operazione equivalente all'eliminazione 
c     dei collettori) 
c*****************************************************************************************
	implicit none

	include '../inc/nodo_ridotto.inc '	


c---------------------------------------->TOPOLOGIA
c---------------------------------------->numero elementi
      integer*4 num_cl_ns        !num.collettori
     *         ,num_lg_ns        !numero linee di grigliato
     *         ,num_mf_cl_ns     !num.centrali mf
     *         ,num_ass_cl_ns    !num.assetti attivi mf
c---------------------------------------->id. elementi
      integer*1 id_cl(*)          !id collettori
     *         ,id_ass_cl(*) !id assetti tra cl
c---------------------------------------->connessioni ai collettori
     *         ,ass_id_cl_asp(*) !id coll monte dell'ass
     *         ,ass_id_cl_man(*) !id coll monte dell'ass

c---------------------------------------->CIN
c---------------------------------------->numero elementi
      integer*4 num_mf_nsrid,
     *          num_ass_nsrid

      integer*1 id_ass_nsrid(*)

      integer*4 vlg_stato_griglia(max_cl_nsm,max_lg_nsm) !stato vl grigliato I/O
      logical*2 assetto_attivo(max_assetti_nsm)          !O

      logical*2 confronta_Guid
      external  confronta_Guid
 
c------------->variabili locali
      integer*4  iel,jel,iel_m,iel_v,id_coll,ind_ass,ii

	logical*2 flag_coll(max_cl_nsm)
c-------------------------------------------------------------------------------------
c-------->inizializza variabili di output e locali  
      do ii=1,num_cl_ns
	   flag_coll(ii)=.false.
	end do

      do ii=1,num_ass_cl_ns
         assetto_attivo(ii)=.false.
      end do



      if (num_mf_cl_ns.gt.0) then
	   if (num_mf_nsrid.gt.0) then   
            do jel=1,num_ass_nsrid   
               do iel=1,num_ass_cl_ns
	           if(confronta_Guid(iel,jel,id_ass_cl,id_ass_nsrid))then
                      iel_m=iel
	                iel_v=iel
                      assetto_attivo(iel)=.true.
	                exit
	             end if
               end do   
               do iel=1,num_cl_ns    !iel -collettori
                  if(confronta_Guid(iel_m,iel,ass_id_cl_asp,id_cl).or.
     *               confronta_Guid(iel_v,iel,ass_id_cl_man,id_cl))then
	               flag_coll(iel)=.true.
	            end if
	         end do
	      end do
	   end if
	end if


      if (num_mf_cl_ns.gt.0) then
	   if (num_mf_nsrid.eq.0) then   !nessun mf nel nodo ridotto
            do jel=1,num_ass_cl_ns    
               iel_m=jel
	         iel_v=jel
               do iel=1,num_cl_ns    !iel -collettori
                   if(confronta_Guid(iel_m,iel,ass_id_cl_asp,id_cl).and.
     *        	    (.not.flag_coll(iel))) then
	                flag_coll(iel)=.true.
	                do ii=1,num_lg_ns
	                   vlg_stato_griglia(iel,ii)=VLG_STATO_OFF
	                end do
	             end if
                   if(confronta_Guid(iel_v,iel,ass_id_cl_man,id_cl).and.
     *				 (.not.flag_coll(iel))) then
	                 flag_coll(iel)=.true.
	                 do ii=1,num_lg_ns
	                      vlg_stato_griglia(iel,ii)=VLG_STATO_OFF
	                 end do
	             end if
	         end do
             end do !jel
	   else                     !alcuni assetti attivi
            do jel=1,num_ass_cl_ns    
	         if (.not.assetto_attivo(jel))then
                  iel_m=jel
	            iel_v=jel
                  do iel=1,num_cl_ns    !iel -collettori
                     if(confronta_Guid(iel_m,iel,ass_id_cl_asp,id_cl)
     *				   .and.(.not.flag_coll(iel))) then
	                   flag_coll(iel)=.true.
	                   do ii=1,num_lg_ns
	                      vlg_stato_griglia(iel,ii)=VLG_STATO_OFF
	                   end do
	               end if
                     if(confronta_Guid(iel_v,iel,ass_id_cl_man,id_cl)
     *				  .and.(.not.flag_coll(iel))) then
	                   flag_coll(iel)=.true.
	                   do ii=1,num_lg_ns
	                      vlg_stato_griglia(iel,ii)=VLG_STATO_OFF
	                   end do
	               end if
	            end do
	          end if
             end do !jel
           
         end if                         !IF2
      end if
      
    
	return
	end
c*************************************************************************
      subroutine prepara_grigliato(num_cl_ns,num_lg_ns,
     *                      num_mf_cl_ns,num_ass_cl_ns,
     *                      id_cl,id_lg,id_vlg,
     *                      vlg_id_lg,vlg_id_cl,vlg_stato,
     *                      id_ass_cl,ass_id_cl_asp,ass_id_cl_man,
     *                      num_mf_nsrid,num_ass_nsrid,id_ass,
     *                      assetto_attivo,
     *                      vlg_stato_griglia)

	implicit none

	include '../inc/nodo_ridotto.inc '	  

	integer*4 num_lg_ns,           !num.linee di grigliato
     *          num_cl_ns,           !num.collettori
     *          num_mf_cl_ns,        !num.centrali mf
     *          num_ass_cl_ns        !num.assetti attivi mf

      integer*1 id_lg(*),            !id.linee grigliato
     *          id_cl(*),            !id.collettori
     *          id_vlg(*),           !id.valvole di grigliato
     *          vlg_id_lg(*),        !id.lg cui è associata la vlg
     *          vlg_id_cl(*)         !id.cl cui è associata la vlg

      integer*4 vlg_stato(*),
     *          num_mf_nsrid,
     *          num_ass_nsrid
 
	  
      integer*1 id_ass_cl(*),        !id assetti tra cl
     *          ass_id_cl_asp(*),    !id coll monte dell'ass
     *          ass_id_cl_man(*),    !id coll monte dell'ass
     *          id_ass(*)

c----------------------------------->output
      integer*4 vlg_stato_griglia(max_cl_nsm,max_lg_nsm)  
      logical*2 assetto_attivo(max_assetti_nsm)
c************************************************************************************************************

c------->creo la tabella degli stati delle valvole di grigliato, come assegnati nel nodo completo:
c        OFF --> 0  ; ON --> 1  ;  LIBERO --> 2
      call tabella_valvole_grigliato(num_cl_ns,num_lg_ns,
     *                               id_cl,id_lg,id_vlg,
     *                               vlg_id_lg,vlg_id_cl,
     *                               vlg_stato,
     *                               vlg_stato_griglia)

c------->gestione centrali tra collettori (pongo in stato OFF tutte le valvole di grigliato
c        sui collettori in aspirazione e mandata di assetti MF non inseriti nella riduzione)
      call verifica_collettori(num_cl_ns,num_lg_ns,
     *                        num_mf_cl_ns,num_ass_cl_ns,
     *                        id_cl,id_ass_cl,
     *                        ass_id_cl_asp,ass_id_cl_man,
     *                        num_mf_nsrid,
     *                        num_ass_nsrid,
     *                        id_ass,assetto_attivo,
     *                        vlg_stato_griglia)
	return
	end

c*******************************************************************************************************
      subroutine crea_connessioni(num_pu_ns,num_lg_ns,num_cl_ns,
     *             num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *             num_vr_cl_ns,num_vd_cl_ns,num_vg_cl_ns,
     *             num_ce_cl_ns,num_mf_cl_ns,num_ass_cl_ns,num_cs_cl_ns,
     *             id_pu,id_lg,id_cl,id_vlg,id_vr_lg,id_vd_lg,id_vg_lg,
     *             id_vr_cl,id_vd_cl,id_vg_cl,id_ce_cl,id_ass_cl,
     *             id_cs_cl,vlg_id_lg,vlg_id_cl,lg_id_pu,
     *             vr_id_lg,vd_id_lg,vg_id_lg,vr_id_clm,vr_id_clv,
     *             vd_id_clm,vd_id_clv,vg_id_clm,vg_id_clv,
     *             ce_id_cl_asp,ce_id_cl_man,ass_id_cl_asp,
     *             ass_id_cl_man,cs_id_cl_asp,cs_id_cl_man,
     *             dir_vr_lg,dir_vd_lg,dir_vg_lg,
     *             vlg_stato_griglia,assetto_attivo,
     *             num_vr_nsrid,num_vd_nsrid,num_vg_nsrid,
     *             num_ce_nsrid,num_mf_nsrid,
     *             num_ass_nsrid,num_cs_nsrid,
     *             id_vr,id_vd,id_vg,id_ce,id_ass,id_cs,
     *             stato_free,
     *             conn,numero_link,tipo_link,warn,elem_id)
c--------------------------------------------------------------------------------------------
	implicit none
	include '../inc/nodo_ridotto.inc '	

c---------------------------------------------------------->numero elementi nodo
c---------------------------------------------------------->topologia
	integer*4 num_pu_ns,           !num.punti rete
     *          num_lg_ns,           !num.linee di grigliato
     *          num_cl_ns,           !num.collettori
c---------------------------------------------------------->elementi nel grigliato
     *          num_vr_lg_ns,        !num.valvole su linee di grigliato (vr)
     *          num_vd_lg_ns,        !num.impianti rid su linee grigliato(vd)
     *          num_vg_lg_ns,        !num.impianti reg su linee grigliato (vg)
c---------------------------------------------------------->elementi tra collettori
     *          num_vr_cl_ns,        !num.valvole tra collettori
     *          num_vd_cl_ns,        !num.impianti rid.tra collettori
     *          num_vg_cl_ns,        !num.impianti reg.tra collettori
     *          num_ce_cl_ns,        !num.centrali binarie
     *          num_mf_cl_ns,        !num.centrali mf
     *          num_ass_cl_ns,       !num.assetti attivi mf
     *          num_cs_cl_ns         !num.centrali semplificate

c---------------------------------------------------------->id. elementi
      integer*1 id_pu(*),            !id.punti rete
     *          id_lg(*),            !id.linee grigliato
     *          id_cl(*),            !id.collettori
     *          id_vlg(*),           !id.valvole di grigliato
     *          id_vr_lg(*),         !id riduzioni su grigliato
     *          id_vd_lg(*),         !id impianti rid.su grigliato
     *          id_vg_lg(*),         !id impianti reg.su grigliato
     *          id_vr_cl(*),         !id riduzioni tra cl
     *          id_vd_cl(*),         !id impianti rid.tra cl
     *          id_vg_cl(*),         !id impianti reg.tra cl
     *          id_ce_cl(*),         !id ce binaria tra cl
     *          id_ass_cl(*),        !id assetti tra cl
     *          id_cs_cl(*),         !id ce semplificate tra cl
c---------------------------------------------------------->associazioni
     *          vlg_id_lg(*),        !id.lg cui è associata la vlg
     *          vlg_id_cl(*),        !id.cl cui è associata la vlg
     *          lg_id_pu(*),         !id.pu cui è associata la lg
     *          vr_id_lg(*),         !id.lg cui è associata la vr
     *          vd_id_lg (*),        !id.lg cui è associata la vd
     *          vg_id_lg (*),        !id.lg cui è associata la vg
     *          vr_id_clm(*),        !id coll monte della vr
     *          vr_id_clv(*),        !id coll monte della vr
     *          vd_id_clm(*),        !id coll monte della vd
     *          vd_id_clv(*),        !id coll monte della vd
     *          vg_id_clm(*),        !id coll monte della vg
     *          vg_id_clv(*),        !id coll monte della vg
     *          ce_id_cl_asp(*),     !id coll monte della ce
     *          ce_id_cl_man(*),     !id coll monte della ce
     *          ass_id_cl_asp(*),    !id coll monte dell'ass
     *          ass_id_cl_man(*),    !id coll monte dell'ass
     *          cs_id_cl_asp(*),     !id coll monte della cs
     *          cs_id_cl_man(*)      !id coll monte della cs
c---------------------------------------------------------->direzione riduzioni
      integer*4 dir_vr_lg(*),    !direzione connessione vr
	                                       !0->da lg a pu
	                                       !1->da pu a lg
     *          dir_vd_lg(*),    !direzione connessione vd
     *          dir_vg_lg(*),    !direzione connessione vg
c---------------------------------------------------------->stato elementi
     *          vlg_stato_griglia(max_cl_nsm,max_lg_nsm) 

      logical*2 assetto_attivo(max_assetti_nsm)

      integer*4 num_ce_nsrid,
     *          num_mf_nsrid,
     *          num_ass_nsrid,
     *          num_cs_nsrid

      integer*1 id_ce(*),
     *          id_ass(*),
     *          id_cs(*)

      integer*4 num_vr_nsrid,
     *          num_vd_nsrid,
     *          num_vg_nsrid

      integer*1 id_vr(*),
     *          id_vd(*),
     *          id_vg(*)
    
c---------------------------------------------------------->stato free
      integer*4 stato_free                   !0-->OFF   1-->ON

c------------>output
	logical*2 conn(max_lg_nsm,max_lg_nsm)
      integer*4 tipo_link(max_lg_nsm,max_lg_nsm,max_link),
     *          warn(max_lg_nsm,max_lg_nsm),
     *          numero_link(max_lg_nsm,max_lg_nsm)
      integer*1 elem_id(max_lg_nsm,max_lg_nsm,max_link*max_lunguid)
c------------>variabili locali
      integer*4 vlg_tab(max_cl_nsm,max_lg_nsm),
     *          vlg_tab_t(max_lg_nsm,max_cl_nsm),
     *          cl_tab(max_cl_nsm,max_cl_nsm),
     *          tipo_rid_gri(max_cl_nsm,max_lg_nsm),
     *          tipo_rid_gri_t(max_lg_nsm,max_cl_nsm)
      integer*1 elem_id_gri(max_cl_nsm,max_lg_nsm*max_lunguid),
     *          elem_id_gri_t(max_lg_nsm*max_lunguid,max_cl_nsm),
     *          elem_id_cl(max_cl_nsm,max_cl_nsm*max_lunguid)

      logical*2 cl_link_vg(max_cl_nsm,max_cl_nsm)  

c***************************************************************************************
c------->creo la tabella degli stati collettore-punto

      call tabella_stati_forc(num_lg_ns,num_cl_ns,
     *             num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *             id_lg,id_cl,id_vr_lg,id_vd_lg,id_vg_lg,
     *             lg_id_pu,vr_id_lg,vd_id_lg,vg_id_lg,
     *             dir_vr_lg,dir_vd_lg,dir_vg_lg,
     *             stato_free,vlg_stato_griglia,
     *             tipo_rid_gri,elem_id_gri,vlg_tab)
c------->traspongo la matrice degli stati
      call stato_pt_cl (num_cl_ns,num_lg_ns,vlg_tab,vlg_tab_t)
c      call stato_pt_cl (num_cl_ns,num_pu_ns,vlg_tab,vlg_tab_t)

c------->matrice connessioni dirette tra collettori
      call tabella_collettori(num_cl_ns,num_vr_cl_ns,num_vd_cl_ns,
     *                            num_vg_cl_ns,num_ce_cl_ns,
     *                            num_mf_cl_ns,num_ass_cl_ns,
     *                            num_cs_cl_ns,
     *                            id_cl,id_vr_cl,id_vd_cl,id_vg_cl,
     *                            id_ce_cl,id_ass_cl,id_cs_cl,
     *                            vr_id_clm,vr_id_clv,
     *                            vd_id_clm,vd_id_clv,
     *                            vg_id_clm,vg_id_clv,
     *                            ce_id_cl_asp,ce_id_cl_man,
     *                            ass_id_cl_asp,ass_id_cl_man,
     *                            cs_id_cl_asp,cs_id_cl_man,
     *                            assetto_attivo,
     *                            cl_tab,elem_id_cl)

c------->matrice connessioni tra collettori date dalle valvole di grigliato
      call link_collettori (id_pu,id_cl,vlg_tab,
     *                      num_cl_ns,num_pu_ns,cl_link_vg)
c------->crea la matrice delle connessioni punto punto

      call connessione(num_cl_ns,num_lg_ns,id_pu,id_cl,vlg_tab,
c      call connessione(num_cl_ns,num_pu_ns,id_pu,id_cl,vlg_tab,
     *                  vlg_tab_t,tipo_rid_gri,elem_id_gri,
     *                  cl_tab,cl_link_vg,elem_id_cl,
     *                  tipo_rid_gri_t,elem_id_gri_t,
     *                  conn,numero_link,tipo_link,warn,elem_id)
      return
	end

c**********************************************************************************************************************
      subroutine riduzioni(num_pu_ns,id_pu,num_cl_ns,id_cl,num_pu_gri,
     *                     num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *                     num_vr_cl_ns,num_vd_cl_ns,num_vg_cl_ns,
     *                     num_ce_cl_ns,num_ass_cl_ns,num_cs_cl_ns, 
     *                     id_vr_lg,id_vd_lg,id_vg_lg,
     *                     id_vr_cl,id_vd_cl,id_vg_cl,
     *                     id_ce_cl,id_ass_cl,id_cs_cl,
     *                     ass_id_cl_asp,ass_id_cl_man,
     *                     vg_tab,pu_equivalenti_asp,pu_equivalenti_man,
     *                     num_vc_nsrid,num_vr_nsrid,num_vd_nsrid,
     *                     num_vg_nsrid,num_ce_nsrid,num_mf_nsrid,
     *                     num_ass_nsrid,num_cs_nsrid,id_vc,
     *                     id_vr,id_vd,id_vg,id_ce,id_ass,id_cs,
     *                     id_pu_vc_m,id_pu_vc_v,
     *                     id_pu_vr_m,id_pu_vr_v,id_pu_vd_m,id_pu_vd_v,
     *                     id_pu_vg_m,id_pu_vg_v,
     *                     id_pu_ce_asp,id_pu_ce_man,
     *                     id_pu_ass_asp,id_pu_ass_man,
     *                     id_pu_cs_asp,id_pu_cs_man,
     *                     stato_vc,flag_vc,ptrt_eq_ptgri,
     *                     nsrid,elem_id,nsrid_warn)
c------------------------------------------------------------------------
c    Crea la matrice che descrive la riduzione eseguita dall'utente
c------------------------------------------------------------------------
      implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 linea,collettori
	parameter (linea=0, collettori=1)

      integer*4 num_pu_ns,num_pu_gri
      integer*1 id_pu(*),id_cl(*)
      integer*4 num_cl_ns,
     *          num_vr_lg_ns,        !num.valvole su linee di grigliato (vr)
     *          num_vd_lg_ns,        !num.impianti rid su linee grigliato(vd)
     *          num_vg_lg_ns,        !num.impianti reg su linee grigliato (vg)
c---------------------------------------------------------->elementi tra collettori
     *          num_vr_cl_ns,     !num.valvole tra collettori
     *          num_vd_cl_ns,     !num.impianti rid.tra collettori
     *          num_vg_cl_ns,     !num.impianti reg.tra collettori
     *          num_ce_cl_ns,
     *          num_ass_cl_ns,
     *          num_cs_cl_ns
         
      integer*1 id_vr_lg(*),        !id riduzioni su grigliato
     *          id_vd_lg(*),        !id impianti rid.su grigliato
     *          id_vg_lg(*),        !id impianti reg.su grigliato
     *          id_vr_cl(*),        !id riduzioni tra cl
     *          id_vd_cl(*),        !id impianti rid.tra cl
     *          id_vg_cl(*),        !id impianti reg.tra cl
     *          id_ce_cl(*),        !id impianti reg.tra cl
     *          id_ass_cl(*),        !id impianti reg.tra cl
     *          id_cs_cl(*),        !id impianti reg.tra cl
     *          ass_id_cl_asp(*),    !id coll monte dell'ass
     *          ass_id_cl_man(*)    !id coll monte dell'ass

      integer*4 vg_tab(max_cl_nsm,max_lg_nsm)
	integer*4 pu_equivalenti_asp(max_cl_nsm,max_lg_nsm),
     *          pu_equivalenti_man(max_cl_nsm,max_lg_nsm)

      integer*4 num_vc_nsrid,
     *          num_vr_nsrid,
     *          num_vd_nsrid,
     *          num_vg_nsrid,
     *          num_ce_nsrid,
     *          num_mf_nsrid,
     *          num_ass_nsrid,
     *          num_cs_nsrid

      integer*1 id_vc(*),
     *          id_vr(*),
     *          id_vd(*),
     *          id_vg(*),
     *          id_ce(*),
     *          id_ass(*),
     *          id_cs(*),
     *          id_pu_vc_m(*),
     *          id_pu_vc_v(*),
     *          id_pu_vr_m(*),
     *          id_pu_vr_v(*),
     *          id_pu_vd_m(*),
     *          id_pu_vd_v(*),
     *          id_pu_vg_m(*),
     *          id_pu_vg_v(*),
     *          id_pu_ce_asp(*),
     *          id_pu_ce_man(*),
     *          id_pu_ass_asp(*),
     *          id_pu_ass_man(*),
     *          id_pu_cs_asp(*),
     *          id_pu_cs_man(*)

      integer*4 stato_vc(*),
     *          flag_vc(max_rt_nsm,max_rt_nsm)

      integer*4 vr_loc  !valvole di riduzione
     *         ,vd_loc  !impianti di riduzione
     *         ,vg_loc  !impianti di regolazione

	
	integer*4 nsrid(max_rt_nsm,max_rt_nsm)

      integer*1 elem_id(max_rt_nsm,max_rt_nsm*max_lunguid)


	integer*4 i,j,i_vc,i_vr,i_vd,i_vg,i_ce,i_ass,i_cs,pu1,pu2,stato,
     *          i_pu,ii,icl_asp,icl_man,i_pu1,i_pu2,count_asp,count_man,
     *          i_asp,i_man,jj,i_pu1_rt,i_pu2_rt,lun

	logical*2 confronta_Guid,ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	external  confronta_Guid

	integer*4 punto_rete_equi
	external punto_rete_equi

      logical*2 flag1,flag2
c-prova
      logical*2 nsrid_warn(max_rt_nsm,max_rt_nsm)  !(O)
c***********************************************************************************************************    

c------>Inizializzazione variabili di output
      lun = num_pu_ns*lunguid
	do i=1,num_pu_ns
	   do j=1,num_pu_ns
	      nsrid(i,j)=NESSUN_LINK
	      nsrid_warn(i,j)=.false.
            flag_vc(i,j)=STATO_IND
	   end do
	   do j=1,lun
	      elem_id(i,j)=0
	   end do
	end do
c------>Inizializzazione variabili locali
      do i=1,num_cl_ns
	   do j=1,num_pu_gri
            pu_equivalenti_asp(i,j)=0
            pu_equivalenti_man(i,j)=0
	   end do
	end do

c------>valvola di collegamento tra punti :
c       se stato ON rappresenta il collegamento con valvole di sezionamento
c       se se stato PV rappresenta laminazione
c       se stato OFF rappresenta una disgiunzione
c       se stato NR disgiunzione direzionata  
 
      do i=1,num_vc_nsrid
	   do i_pu=1,num_pu_ns
            if(confronta_Guid(i,i_pu,id_pu_vc_m,id_pu))then
	         pu1=i_pu
	      end if
            if(confronta_Guid(i,i_pu,id_pu_vc_v,id_pu))then
	         pu2=i_pu
	      end if
	   end do

	   if (stato_vc(i).eq.sempre_off) then
            flag_vc(pu1,pu2)=sempre_off
	   else if (stato_vc(i).eq.mai_off) then
	       nsrid(pu1,pu2)=VL_SEZ
cmes	       nsrid(pu2,pu1)=VL_SEZ
             call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vc,elem_id)
             flag_vc(pu1,pu2)=mai_off
	   else if (stato_vc(i).eq.cambia_stato) then
	       nsrid(pu1,pu2)=VL_SEZ
cmes       nsrid(pu2,pu1)=VL_SEZ
             call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vc,elem_id)
             flag_vc(pu1,pu2)=cambia_stato
        end if


	end do

c------>valvole nel nodo ridotto
      do i=1,num_vr_nsrid
	   do i_pu=1,num_pu_ns
            if(confronta_Guid(i,i_pu,id_pu_vr_m,id_pu))then
	         pu1=i_pu
	      end if
            if(confronta_Guid(i,i_pu,id_pu_vr_v,id_pu))then
	         pu2=i_pu
	      end if
	   end do

         do ii=1,num_vr_lg_ns
            if(confronta_Guid(ii,i,id_vr_lg,id_vr))then
	          nsrid(pu1,pu2)=VR_GRI   !rid su linea
                call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vr,elem_id)
	          exit
	      end if
	   end do

         do ii=1,num_vr_cl_ns
            if(confronta_Guid(ii,i,id_vr_cl,id_vr))then
	          nsrid(pu1,pu2)=VR_COL  !rid tra collettori
                call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vr,elem_id)
	          exit
	      end if
	   end do

	end do
c------>impianti di riduzione nel nodo ridotto
      do i=1,num_vd_nsrid
	   do i_pu=1,num_pu_ns
            if(confronta_Guid(i,i_pu,id_pu_vd_m,id_pu))then
	         pu1=i_pu
	      end if
            if(confronta_Guid(i,i_pu,id_pu_vd_v,id_pu))then
	         pu2=i_pu
	      end if
	   end do

         do ii=1,num_vd_lg_ns
            if(confronta_Guid(ii,i,id_vd_lg,id_vd))then
	         nsrid(pu1,pu2)=VD_GRI   !vd su linea
               call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vd,elem_id)
	         exit
	      end if
	   end do
         do ii=1,num_vd_cl_ns
            if(confronta_Guid(ii,i,id_vd_cl,id_vd))then
	          nsrid(pu1,pu2)=VD_COL  !vd tra collettori
                call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vd,elem_id)
 	          exit
	      end if
	   end do
	end do
c------>impianti di regolazione nel nodo ridotto
      do i=1,num_vg_nsrid
	   do i_pu=1,num_pu_ns
            if(confronta_Guid(i,i_pu,id_pu_vg_m,id_pu))then
	         pu1=i_pu
	      end if
            if(confronta_Guid(i,i_pu,id_pu_vg_v,id_pu))then
	         pu2=i_pu
	      end if
	   end do

         do ii=1,num_vg_lg_ns
            if(confronta_Guid(ii,i,id_vg_lg,id_vg))then
	         nsrid(pu1,pu2)=VG_GRI   !vg su linea
               call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vg,elem_id)
	         exit
	      end if
	   end do

         do ii=1,num_vg_cl_ns
            if(confronta_Guid(ii,i,id_vg_cl,id_vg))then
	          nsrid(pu1,pu2)=VG_COL  !vg tra collettori
                call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_vg,elem_id)
	          exit
	      end if
	   end do

	end do
c------>centrali binarie nel nodo ridotto
      if (num_ce_nsrid.gt.0) then
	   do i=1,num_ce_nsrid
	      do i_pu=1,num_pu_ns
               if(confronta_Guid(i,i_pu,id_pu_ce_asp,id_pu))then
	            pu1=i_pu
	         end if
               if(confronta_Guid(i,i_pu,id_pu_ce_man,id_pu))then
	            pu2=i_pu
	         end if
	      end do

	          nsrid(pu1,pu2)=CE_BIN   !ce
                call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_ce,elem_id)

	   end do
	   go to 100
	end if
c------>centrali mf nel nodo ridotto 
      if (num_mf_nsrid.gt.0) then
	   do i=1,num_ass_nsrid

	      do i_pu=1,num_pu_ns
               if(confronta_Guid(i,i_pu,id_pu_ass_asp,id_pu))then
	            pu1=i_pu
	         end if
               if(confronta_Guid(i,i_pu,id_pu_ass_man,id_pu))then
	            pu2=i_pu
	         end if
	      end do

	
	      nsrid(pu1,pu2)=ASS_MF   !rid
            call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_ass,elem_id)


            do ii=1,num_ass_cl_ns
               if(confronta_Guid(ii,i,id_ass_cl,id_ass))then
	             do jj=1,num_cl_ns
	               if(confronta_Guid(ii,jj,ass_id_cl_asp,id_cl))then
                        icl_asp=jj
	               end if
	               if(confronta_Guid(ii,jj,ass_id_cl_man,id_cl))then
                        icl_man=jj
	               end if
	             end do
	            exit
	         end if
	      end do
c-punti griglia
            call punti_equivalenti(num_pu_gri,num_cl_ns,icl_asp,vg_tab,
     *                             count_asp,pu_equivalenti_asp)
            call punti_equivalenti(num_pu_gri,num_cl_ns,icl_man,vg_tab,
     *                             count_man,pu_equivalenti_man)
c-prova 17   
            flag1=.false.
            flag2=.false.
	      if (count_asp.gt.1.or.count_man.gt.1) then
	         do i_asp=1,count_asp
	               i_pu1=pu_equivalenti_asp(icl_asp,i_asp)
                     i_pu1_rt=punto_rete_equi(num_pu_ns,i_pu1,
     *                                           ptrt_eq_ptgri)
	               if (i_pu1_rt.eq.pu1) then
                        flag1=.true.
	                  exit
	               end if
	         end do
			 do i_man=1,count_man
	               i_pu2=pu_equivalenti_man(icl_man,i_man)
                     i_pu2_rt=punto_rete_equi(num_pu_ns,i_pu2,
     *                                           ptrt_eq_ptgri)
	               if (i_pu2_rt.eq.pu2) then
                        flag2=.true.
	                  exit
	               end if
               end do
	     end if
	     if (flag1.and.flag2)then
c-prova  17   Quando inserisco un'assetto e questo ha più aspirazioni e più mandate estendo
c             l'inserimento a tutte le combinazioni di aspirazione e mandata, purchè l'inserimento 
c             iniziale sia corretto (se l'assegnamento viene fatto anche ad inserimento non corretto
c             non viene colta l'imposizione non assegnata)
	      if (count_asp.gt.1.or.count_man.gt.1) then
	         do i_asp=1,count_asp
	            do i_man=1,count_man
	               i_pu1=pu_equivalenti_asp(icl_asp,i_asp)
	               i_pu2=pu_equivalenti_man(icl_man,i_man)
                     i_pu1_rt=punto_rete_equi(num_pu_ns,i_pu1,
     *                                           ptrt_eq_ptgri)
                     i_pu2_rt=punto_rete_equi(num_pu_ns,i_pu2,
     *                                           ptrt_eq_ptgri)
	               if (i_pu1_rt.ne.pu1.or.i_pu2_rt.ne.pu2) then
c-prova 
                        if (nsrid(i_pu1_rt,i_pu2_rt).gt.0) then
	                      nsrid_warn(i_pu1_rt,i_pu2_rt)=.true.   
                        end if
c-prova
	                  nsrid(i_pu1_rt,i_pu2_rt)=ASS_MF   !ass
                       call copia_Guid_m(i,max_rt_nsm,i_pu1_rt,i_pu2_rt,
     *                                id_ass,elem_id)
                     end if
	            end do
	         end do
	       end if
           end if

	   end do
	   go to 100
	end if

c------>centrali semplificate nel nodo ridotto
      if (num_cs_nsrid.gt.0) then
	   do i=1,num_cs_nsrid
	      do i_pu=1,num_pu_ns
               if(confronta_Guid(i,i_pu,id_pu_cs_asp,id_pu))then
	            pu1=i_pu
	         end if
               if(confronta_Guid(i,i_pu,id_pu_cs_man,id_pu))then
	            pu2=i_pu
	         end if
	      end do

	      nsrid(pu1,pu2)=CE_SEMP   !cs
               call copia_Guid_m(i,max_rt_nsm,pu1,pu2,
     *                                id_cs,elem_id)

	   end do
	   go to 100
	end if

100   continue

      return
	end

c*******************************************************************************************************
      subroutine verifica_nodo_ridotto(                    
     *           lun_GUID,								 !lunghezza 	
     *           num_pu_ns,id_pu,                          !punti rete
     *           num_lg_ns,id_lg,lg_id_pu,                 !linee di grigliato
     *           num_cl_ns,id_cl,                          !collettori
     *           id_vlg,vlg_id_lg,vlg_id_cl,vlg_stato,     !valvole di grigliato
     *           num_vr_lg_ns,id_vr_lg,vr_id_lg,dir_vr_lg, !valvole su linee
     *           num_vd_lg_ns,id_vd_lg,vd_id_lg,dir_vd_lg, !impianti rid linee
     *           num_vg_lg_ns,id_vg_lg,vg_id_lg,dir_vg_lg, !impianti reg linee
     *           num_vr_cl_ns,id_vr_cl,vr_id_clm,vr_id_clv,!valvole collettori
     *           num_vd_cl_ns,id_vd_cl,vd_id_clm,vd_id_clv,!impianti rid cl
     *           num_vg_cl_ns,id_vg_cl,vg_id_clm,vg_id_clv,!impianti reg cl
     *           num_ce_cl_ns,id_ce_cl,ce_id_cl_asp,ce_id_cl_man,     !centrali 
     *           num_ass_cl_ns,id_ass_cl,ass_id_cl_asp,ass_id_cl_man, !assetti
     *           num_cs_cl_ns,id_cs_cl,cs_id_cl_asp,cs_id_cl_man,     !ce sempli
c                                                          !nodo ridotto
     *           num_vc_nsrid,id_vc,id_pu_vc_m,id_pu_vc_v,stato_vc,
     *           num_vr_nsrid,id_vr,id_pu_vr_m,id_pu_vr_v,
     *           num_vd_nsrid,id_vd,id_pu_vd_m,id_pu_vd_v,
     *           num_vg_nsrid,id_vg,id_pu_vg_m,id_pu_vg_v,
     *           num_ce_nsrid,id_ce,id_pu_ce_asp,id_pu_ce_man,
     *           num_ass_nsrid,id_ass,id_pu_ass_asp,id_pu_ass_man,
     *           num_cs_nsrid,id_cs,id_pu_cs_asp,id_pu_cs_man,
c-------------------------------------------------------------------------->output
     *           n_err,cod_err,cod_elem,type_elem,pt_err_m,pt_err_v,
     *           n_warn,cod_warn,pt_warn_m,pt_warn_v) 

      implicit none

      integer*4 stato_imposto,stato_possibile
	parameter (stato_imposto=0,stato_possibile=1)

	include '../inc/nodo_ridotto.inc '	! :in

c---------------------------------------------------------->numero elementi nodo
c---------------------------------------------------------->topologia
	integer*4 lun_GUID              !lunghezza GUID

      integer*4 num_pu_ns 			!num.punti rete
      integer*1 id_pu(*)              !id.punti rete

      integer*4 num_lg_ns 			!num.linee di grigliato
      integer*1 id_lg(*),             !id.linee di grigliato
     *          lg_id_pu(*)           !id.pu cui è associata la lg

      integer*4 num_cl_ns 			!num.collettori
      integer*1 id_cl(*)              !id.collettori

      integer*1 id_vlg(*),            !id.valvole di grigliato
     *          vlg_id_lg(*),         !id.lg cui è associata la vlg
     *          vlg_id_cl(*)         !id.cl cui è associata la vlg
      integer*4 vlg_stato(*)          !stato valvole di grigliato

      integer*4 num_vr_lg_ns 			!num.valvole su linee di grigliato (vr)
      integer*1 id_vr_lg(*),           !id riduzioni su grigliato
     *          vr_id_lg(*)           !id.lg cui è associata la vr
      integer*4 dir_vr_lg(*)          !direzione connessione vr
	                                            !0->da lg a pu
										        !1->da pu a lg

      integer*4 num_vd_lg_ns 			!!num.impianti rid su linee grigliato(vd)
      integer*1 id_vd_lg(*),           !id impianti rid.su grigliato
     *          vd_id_lg(*)           !id.lg cui è associata la vd
      integer*4 dir_vd_lg(*)          !direzione connessione vd
	                                            !0->da lg a pu
										        !1->da pu a lg

      integer*4 num_vg_lg_ns 			!!num.impianti reg su linee grigliato(vg)
      integer*1 id_vg_lg(*),           !id impianti rid.su grigliato
     *          vg_id_lg(*)           !id.lg cui è associata la vg
      integer*4 dir_vg_lg(*)          !direzione connessione vg
	                                            !0->da lg a pu
										        !1->da pu a lg
      integer*4 num_vr_cl_ns 			!num.valvole tra collettori
      integer*1 id_vr_cl(*),          !id riduzioni tra cl
     *          vr_id_clm(*),         !id coll monte della vr
     *          vr_id_clv(*)         !id coll monte della vr

      integer*4 num_vd_cl_ns 			!num.impianti rid.tra collettori
      integer*1 id_vd_cl(*),          !id impianti rid.tra cl
     *          vd_id_clm(*),         !id coll monte della vd
     *          vd_id_clv(*)         !id coll monte della vd

      integer*4 num_vg_cl_ns 			!num.impianti reg.tra collettori
      integer*1 id_vg_cl(*),          !id impianti reg.tra cl
     *          vg_id_clm(*),         !id coll monte della vg
     *          vg_id_clv(*)         !id coll monte della vg
 
      integer*4 num_ce_cl_ns 			!num.centrali binarie
      integer*1 id_ce_cl(*),          !id centrali binarie
     *          ce_id_cl_asp(*),      !id coll monte della ce
     *          ce_id_cl_man(*)       !id coll monte della ce

      integer*4 num_ass_cl_ns 		 !num.assetti mf
      integer*1 id_ass_cl(*),          !id assetti mf
     *          ass_id_cl_asp(*),      !id coll monte dell'ass
     *          ass_id_cl_man(*)       !id coll monte dell'ass

      integer*4 num_cs_cl_ns 			! num.centrali semplificate
      integer*1 id_cs_cl(*),           !id centrali semplificate
     *          cs_id_cl_asp(*),       !id coll monte della cs
     *          cs_id_cl_man(*)        !id coll monte della cs

      integer*4 num_mf_cl_ns,num_mf_nsrid

c---------------------------------------------------->nodo ridotto
      integer*4 num_vc_nsrid         !numero valvole di collegamento
      integer*1 id_vc(*),            !id.valvole di collegamento
     *          id_pu_vc_m(*),	   !id. punto di connesione monte
     *          id_pu_vc_v(*)		   !id. punto di connesione valle
      integer*4 stato_vc(*)          !flag di stato valvole di collegamento

      integer*4 num_vr_nsrid         !numero valvole 
      integer*1 id_vr(*),            !id.valvole 
     *          id_pu_vr_m(*),	   !id. punto di connesione monte
     *          id_pu_vr_v(*)  	   !id. punto di connesione valle

      integer*4 num_vd_nsrid         !numero impianti di riduzione
      integer*1 id_vd(*),            !id.impianti di riduzione 
     *          id_pu_vd_m(*),	   !id. punto di connesione monte
     *          id_pu_vd_v(*)	       !id. punto di connesione valle

      integer*4 num_vg_nsrid         !numero impianti di regolazione
      integer*1 id_vg(*),            !id.impianti di regolazione 
     *          id_pu_vg_m(*),	   !id. punto di connesione monte
     *          id_pu_vg_v(*)	       !id. punto di connesione valle

      integer*4 num_ce_nsrid         !numero centrali binarie
      integer*1 id_ce(*),            !id.centrali binarie
     *          id_pu_ce_asp(*),     !id. punto di connesione aspirazione
     *          id_pu_ce_man(*)      !id. punto di connesione mandata

      integer*4 num_ass_nsrid         !numero assetti
      integer*1 id_ass(*),            !id.assetti 
     *          id_pu_ass_asp(*),     !id. punto di connesione aspirazione
     *          id_pu_ass_man(*)      !id. punto di connesione mandata

      integer*4 num_cs_nsrid         !numero centrali semplificate
      integer*1 id_cs(*),            !id.centrali semplificate
     *          id_pu_cs_asp(*),     !id. punto di connesione aspirazione
     *          id_pu_cs_man(*)      !id. punto di connesione mandata


c------------->output
      integer*4 n_err          !numero errori individuati nella riduzione
     *         ,cod_err(*)     !codice d'errore 
     *         ,type_elem(*)   !codice d'errore 
      integer*1 cod_elem(*)    !codice elemento 
     *         ,pt_err_m(*)    !pu monte coinvolto nel collegamento errato                                     
     *         ,pt_err_v(*)    !pu monte coinvolto nel collegamento errato                                     

      integer*4 n_warn                 !numero errori individuati nella riduzione
     *         ,cod_warn(*)   !codice d'errore 
      integer*1 pt_warn_m(*)!punti coinvolti nel collegamento errato                                     
     *         ,pt_warn_v(*)!punti coinvolti nel collegamento errato                                     
   
c------------->variabili locali

      logical*2 flag,no_link
      integer*4 i,j,ii,k,ind_i,ind_ii,ind_k,index,n_iter
	real*4 log2,log_arg

      integer*4 vlg_stato_griglia(max_cl_nsm,max_lg_nsm)  
	   
	integer*4 nsrid(max_rt_nsm,max_rt_nsm) 

	integer*1 elem_id(max_rt_nsm,max_rt_nsm*max_lunguid) 

      integer*4 tipo_link_pos(max_lg_nsm,max_lg_nsm,max_link),
     *          tipo_link_imp(max_lg_nsm,max_lg_nsm,max_link),
     *          warn_pos(max_lg_nsm,max_lg_nsm),
     *          warn_imp(max_lg_nsm,max_lg_nsm),
     *          numero_link_pos(max_lg_nsm,max_lg_nsm),
     *          numero_link_imp(max_lg_nsm,max_lg_nsm)

      integer*1 elem_id_pos(max_lg_nsm,max_lg_nsm,max_link*max_lunguid), 
     *          elem_id_imp(max_lg_nsm,max_lg_nsm,max_link*max_lunguid) 

      logical*2 link_ok(max_rt_nsm,max_rt_nsm),
     *          conn_pos(max_lg_nsm,max_lg_nsm),
     *          conn_imp(max_lg_nsm,max_lg_nsm),
     *          assetto_attivo(max_assetti_nsm)


      integer*4 flag_vc(max_rt_nsm,max_rt_nsm)

	integer*4 pu_equivalenti_asp(max_cl_nsm,max_lg_nsm),
     *          pu_equivalenti_man(max_cl_nsm,max_lg_nsm)

celena
      logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)  
	integer*4 punto_rete_equi ,num_pu_gri  
	external punto_rete_equi   
      logical*2 nsrid_warn(max_rt_nsm,max_rt_nsm)  

c------------------------------------------------------------------------------------------
	
      lunGUID=lun_GUID		!valorizzo l'area common parametri del file
							!                          nodo_ridotto.inc
      n_err=0
	do i=1,max_err
         cod_err(i)=0
         type_elem(i)=0
	end do

	do i=1,max_err*lunguid
         cod_elem(i)=0
         pt_err_m(i)=0
         pt_err_v(i)=0
	end do

      n_warn=0
	do i=1,max_err
         cod_warn(i)=0
	end do

	do i=1,max_err*lunguid
         pt_warn_m(i)=0
         pt_warn_v(i)=0
	end do

	num_mf_cl_ns = 0
	num_mf_nsrid = 0
      
      if (num_ass_cl_ns.gt.0) then
	    num_mf_cl_ns = 1
	    if(num_ass_nsrid.gt.0)then
	       num_mf_nsrid = 1
		end if
	end if



      call prepara_grigliato(num_cl_ns,num_lg_ns,
     *                      num_mf_cl_ns,num_ass_cl_ns,
     *                      id_cl,id_lg,id_vlg,
     *                      vlg_id_lg,vlg_id_cl,vlg_stato,
     *                      id_ass_cl,ass_id_cl_asp,ass_id_cl_man,
     *                      num_mf_nsrid,num_ass_nsrid,id_ass,
     *                      assetto_attivo,
     *                      vlg_stato_griglia)

	call equivalenza_punti(num_pu_ns,num_lg_ns,id_pu,lg_id_pu,
     *                       ptrt_eq_ptgri)


      num_pu_gri=num_lg_ns    !numero punti grigliato

c-------->matrice dei collegamenti possibili
c      call crea_connessioni(num_pu_ns,num_lg_ns,num_cl_ns,
      call crea_connessioni(num_pu_gri,num_lg_ns,num_cl_ns,
     *             num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *             num_vr_cl_ns,num_vd_cl_ns,num_vg_cl_ns,
     *             num_ce_cl_ns,num_mf_cl_ns,num_ass_cl_ns,num_cs_cl_ns,
     *             id_pu,id_lg,id_cl,id_vlg,id_vr_lg,id_vd_lg,id_vg_lg,
     *             id_vr_cl,id_vd_cl,id_vg_cl,id_ce_cl,id_ass_cl,
     *             id_cs_cl,vlg_id_lg,vlg_id_cl,lg_id_pu,
     *             vr_id_lg,vd_id_lg,vg_id_lg,vr_id_clm,vr_id_clv,
     *             vd_id_clm,vd_id_clv,vg_id_clm,vg_id_clv,
     *             ce_id_cl_asp,ce_id_cl_man,ass_id_cl_asp,
     *             ass_id_cl_man,cs_id_cl_asp,cs_id_cl_man,
     *             dir_vr_lg,dir_vd_lg,dir_vg_lg,
     *             vlg_stato_griglia,assetto_attivo,
     *             num_vr_nsrid,num_vd_nsrid,num_vg_nsrid,
     *             num_ce_nsrid,num_mf_nsrid,
     *             num_ass_nsrid,num_cs_nsrid,
     *             id_vr,id_vd,id_vg,id_ce,id_ass,id_cs,
     *             stato_possibile,
     *             conn_pos,numero_link_pos,
     *             tipo_link_pos,warn_pos,elem_id_pos)

c-------->matrice dei collegamenti imposti
c      call crea_connessioni(num_pu_ns,num_lg_ns,num_cl_ns,
      call crea_connessioni(num_pu_gri,num_lg_ns,num_cl_ns,
     *             num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *             num_vr_cl_ns,num_vd_cl_ns,num_vg_cl_ns,
     *             num_ce_cl_ns,num_mf_cl_ns,num_ass_cl_ns,num_cs_cl_ns,
     *             id_pu,id_lg,id_cl,id_vlg,id_vr_lg,id_vd_lg,id_vg_lg,
     *             id_vr_cl,id_vd_cl,id_vg_cl,id_ce_cl,id_ass_cl,
     *             id_cs_cl,vlg_id_lg,vlg_id_cl,lg_id_pu,
     *             vr_id_lg,vd_id_lg,vg_id_lg,vr_id_clm,vr_id_clv,
     *             vd_id_clm,vd_id_clv,vg_id_clm,vg_id_clv,
     *             ce_id_cl_asp,ce_id_cl_man,ass_id_cl_asp,
     *             ass_id_cl_man,cs_id_cl_asp,cs_id_cl_man,
     *             dir_vr_lg,dir_vd_lg,dir_vg_lg,
     *             vlg_stato_griglia,assetto_attivo,
     *             num_vr_nsrid,num_vd_nsrid,num_vg_nsrid,
     *             num_ce_nsrid,num_mf_nsrid,
     *             num_ass_nsrid,num_cs_nsrid,
     *             id_vr,id_vd,id_vg,id_ce,id_ass,id_cs,
     *             stato_imposto,
     *             conn_imp,numero_link_imp,tipo_link_imp,
     *             warn_imp,elem_id_imp)

c      call riduzioni(num_pu_ns,id_pu,num_cl_ns,id_cl,
      call riduzioni(num_pu_ns,id_pu,num_cl_ns,id_cl,num_pu_gri,
     *              num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *              num_vr_cl_ns,num_vd_cl_ns,num_vg_cl_ns,
     *              num_ce_cl_ns,num_ass_cl_ns,num_cs_cl_ns, 
     *              id_vr_lg,id_vd_lg,id_vg_lg,
     *              id_vr_cl,id_vd_cl,id_vg_cl,
     *              id_ce_cl,id_ass_cl,id_cs_cl,
     *              ass_id_cl_asp,ass_id_cl_man,
     *              vlg_stato_griglia,
     *              pu_equivalenti_asp,pu_equivalenti_man,
     *              num_vc_nsrid,num_vr_nsrid,num_vd_nsrid,
     *              num_vg_nsrid,num_ce_nsrid,num_mf_nsrid,
     *              num_ass_nsrid,num_cs_nsrid,id_vc,
     *              id_vr,id_vd,id_vg,id_ce,id_ass,id_cs,
     *              id_pu_vc_m,id_pu_vc_v,
     *              id_pu_vr_m,id_pu_vr_v,id_pu_vd_m,id_pu_vd_v,
     *              id_pu_vg_m,id_pu_vg_v,
     *              id_pu_ce_asp,id_pu_ce_man,
     *              id_pu_ass_asp,id_pu_ass_man,
     *              id_pu_cs_asp,id_pu_cs_man,
     *              stato_vc,flag_vc,ptrt_eq_ptgri,
     *              nsrid,elem_id,nsrid_warn)



c----------->controllo sulle connessioni!
c------------>verifico i collegamenti diretti assegnati da utente nella riduzione
c             (linl_ok viene definito a partire sa NSRID) e utilizza gli stessi per
c             definire i collegamenti indiretti

      call controlla_connessioni_forc(num_pu_gri,num_pu_ns,id_pu,
     *                             nsrid,conn_pos,ptrt_eq_ptgri,link_ok)

c----------------------------->Definisco le situazioni di errore e di warning
      call trova_errori_forc(num_pu_gri,num_pu_ns,id_pu,nsrid,
     *                  flag_vc,
     *                  conn_pos,conn_imp,
     *                  numero_link_pos,numero_link_imp,
     *                  tipo_link_pos,tipo_link_imp,
     *                  link_ok,warn_pos,warn_imp,
     *                  elem_id,elem_id_pos,elem_id_imp,
     *                  ptrt_eq_ptgri,nsrid_warn,      
     *                  n_err,cod_err,cod_elem,type_elem,
     *                  pt_err_m,pt_err_v,
     *                  n_warn,cod_warn,pt_warn_m,pt_warn_v)


      return
	end