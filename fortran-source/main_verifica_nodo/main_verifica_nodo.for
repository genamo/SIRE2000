c*****************************************************************************************************
      program check_nodoridotto
c-----------------------------------------------------------------------------------------------------
c  verifica la riduzione del nodo assegnata da utente
c-----------------------------------------------------------------------------------------------------

      implicit none
	include '../inc/nodo_ridotto.inc '	


c---------------------------------------------------------->topologia
	integer*4 lun_GUID              !lunghezza GUID

      integer*4 num_pu_ns 			!num.punti rete
      integer*1 id_pu(max_rt_nsm*max_lunGuid)             !id.punti rete

      integer*4 num_lg_ns 			!num.linee di grigliato
      integer*1 id_lg(max_lg_nsm*max_lunguid),    !id.linee di grigliato
     *          lg_id_pu(max_lg_nsm*max_lunguid)  !id.pu cui è associata la lg

      integer*4 num_cl_ns 		                    	!num.collettori
      integer*1 id_cl(max_cl_nsm*max_lunguid)             !id.collettori

      integer*1 id_vlg(max_vlg_nsm*max_lunguid),       !id.valvole di grigliato
     *          vlg_id_lg(max_vlg_nsm*max_lunguid),    !id.lg associato alla vlg
     *          vlg_id_cl(max_vlg_nsm*max_lunguid)     !id.cl associato alla vlg
      integer*4 vlg_stato(max_vlg_nsm)               !stato valvole di grigliato

      integer*4 num_vr_lg_ns 			!num.valvole su linee di grigliato (vr)
      integer*1 id_vr_lg(max_vr_lg_nsm*max_lunguid),   !id rid su grigliato
     *          vr_id_lg(max_vr_lg_nsm*max_lunguid)    !id.lg associato alla vr
      integer*4 dir_vr_lg(max_vr_lg_nsm)          !direzione connessione vr
	                                            !0->da lg a pu
										        !1->da pu a lg

      integer*4 num_vd_lg_ns 			!!num.impianti rid su linee grigliato(vd)
      integer*1 id_vd_lg(max_vd_lg_nsm*max_lunguid),   !id impianti rid.su gri
     *          vd_id_lg(max_vd_lg_nsm*max_lunguid)    !id.lg associato alla vd
      integer*4 dir_vd_lg(max_vd_lg_nsm)          !direzione connessione vd
	                                            !0->da lg a pu
										        !1->da pu a lg

      integer*4 num_vg_lg_ns 			!!num.impianti reg su linee grigliato(vg)
      integer*1 id_vg_lg(max_vg_lg_nsm*max_lunguid),    !id impianti rid.su gri
     *          vg_id_lg(max_vg_lg_nsm*max_lunguid)     !id.lg associato alla vg
      integer*4 dir_vg_lg(max_vg_lg_nsm)          !direzione connessione vg
	                                            !0->da lg a pu
										        !1->da pu a lg
      integer*4 num_vr_cl_ns 			!num.valvole tra collettori
      integer*1 id_vr_cl(max_vr_cl_nsm*max_lunguid),    !id riduzioni tra cl
     *          vr_id_clm(max_vr_cl_nsm*max_lunguid),   !id coll monte della vr
     *          vr_id_clv(max_vr_cl_nsm*max_lunguid)    !id coll monte della vr

      integer*4 num_vd_cl_ns 			!num.impianti rid.tra collettori
      integer*1 id_vd_cl(max_vd_cl_nsm*max_lunguid),    !id impianti rid.tra cl
     *          vd_id_clm(max_vd_cl_nsm*max_lunguid),   !id coll monte della vd
     *          vd_id_clv(max_vd_cl_nsm*max_lunguid)    !id coll monte della vd

      integer*4 num_vg_cl_ns 			!num.impianti reg.tra collettori
      integer*1 id_vg_cl(max_vg_cl_nsm*max_lunGUid),    !id impianti reg.tra cl
     *          vg_id_clm(max_vg_cl_nsm*max_lunguid),   !id coll monte della vg
     *          vg_id_clv(max_vg_cl_nsm*max_lunguid)    !id coll monte della vg
 
      integer*4 num_ce_cl_ns 			!num.centrali binarie
      integer*1 id_ce_cl(max_ce_nsm*max_lunguid),       !id centrali binarie
     *          ce_id_cl_asp(max_ce_nsm*max_lunguid),   !id coll monte della ce
     *          ce_id_cl_man(max_ce_nsm*max_lunguid)    !id coll monte della ce

      integer*4 num_ass_cl_ns 		 !num.assetti mf
      integer*1 id_ass_cl(max_assetti_nsm*max_lunguid),    !id assetti mf
     *          ass_id_cl_asp(max_assetti_nsm*max_lunguid),!id coll monte ass
     *          ass_id_cl_man(max_assetti_nsm*max_lunguid) !id coll monte ass

      integer*4 num_cs_cl_ns 			! num.centrali semplificate
      integer*1 id_cs_cl(max_cs_nsm*max_lunguid),           !id cs
     *          cs_id_cl_asp(max_cs_nsm*max_lunguid),       !id coll monte cs
     *          cs_id_cl_man(max_cs_nsm*max_lunguid)        !id coll monte cs


c---------------------------------------------------->nodo ridotto
      integer*4 num_vc_nsrid         !numero valvole di collegamento
      integer*1 id_vc(max_vc_nsm*max_lunguid),          !id.vl di collegamento
     *          id_pu_vc_m(max_vc_nsm*max_lunguid),	  !id. punto di conn monte
     *          id_pu_vc_v(max_vc_nsm*max_lunguid)      !id. punto di conn valle
      integer*4 stato_vc(max_vc_nsm)      !flag di stato valvole di collegamento

      integer*4 num_vr_nsrid         !numero valvole 
      integer*1 id_vr(max_vr_nsm*max_lunguid),          !id.valvole 
     *          id_pu_vr_m(max_vr_nsm*max_lunguid),	  !id. punto di conn monte
     *          id_pu_vr_v(max_vr_nsm*max_lunguid)  	  !id. punto di conn valle

      integer*4 num_vd_nsrid         !numero impianti di riduzione
      integer*1 id_vd(max_vd_nsm*max_lunguid),         !id.impianti di riduzione 
     *          id_pu_vd_m(max_vd_nsm*max_lunguid),	 !id. punto di conn monte
     *          id_pu_vd_v(max_vd_nsm*max_lunguid)	 !id. punto di conn valle

      integer*4 num_vg_nsrid         !numero impianti di regolazione
      integer*1 id_vg(max_vg_nsm*max_lunguid),       !id.impianti di regolazione 
     *          id_pu_vg_m(max_vg_nsm*max_lunguid),  !id. punto di conn monte
     *          id_pu_vg_v(max_vg_nsm*max_lunguid)   !id. punto di conn valle

      integer*4 num_ce_nsrid         !numero centrali binarie
      integer*1 id_ce(max_ce_nsm*max_lunguid),            !id.centrali binarie
     *          id_pu_ce_asp(max_ce_nsm*max_lunguid),     !id. punto di conn asp
     *          id_pu_ce_man(max_ce_nsm*max_lunguid)      !id. punto di conn man

      integer*4 num_ass_nsrid         !numero assetti
      integer*1 id_ass(2*max_lunguid),            !id.assetti 
     *          id_pu_ass_asp(2*max_lunguid),     !id. punto di conn asp
     *          id_pu_ass_man(2*max_lunguid)      !id. punto di conn man

      integer*4 num_cs_nsrid         !numero centrali semplificate
      integer*1 id_cs(max_cs_nsm*max_lunguid),            !id.cs
     *          id_pu_cs_asp(max_cs_nsm*max_lunguid),     !id. punto di conn asp
     *          id_pu_cs_man(max_cs_nsm*max_lunguid)      !id. punto di conn man


c------------->output
      integer*4 n_err                 !numero errori individuati nella riduzione
     *         ,cod_err(max_err)     !codice d'errore 
     *         ,type_elem(max_err)     !codice d'errore 
      integer*1 cod_elem(max_err*max_lunguid)    !codice elemento 
     *         ,pt_err_m(max_err*max_lunguid)    !pu monte nel link errato                                     
     *         ,pt_err_v(max_err*max_lunguid)    !pu valle nel link errato
      integer*4 n_warn               !numero warning individuati nella riduzione
     *         ,cod_warn(max_err)   !codice di warning 
      integer*1 pt_warn_m(max_err*max_lunguid)!pu monte nel link errato                                     
     *         ,pt_warn_v(max_err*max_lunguid)!pu valle nel link errato      
      character*(120) ute,dat,log
	character*(100) i_name_file,o_name_file,file_input,input

      integer*4 status,i,ii,num_vlg_ns,j,last_i,jj,count

c-24 luglio-PREPARO I PUNTATORI PER L'ALLOCAZIONE DI MEMORIA
C***********************************************************
c      integer*4 num_pu_ns 			!num.punti rete
      integer*1, allocatable :: id_pu_LOC(:)              !id.punti rete

c     integer*4 num_lg_ns 			   !num.linee di grigliato
      integer*1, allocatable :: id_lg_LOC(:), !id.linee di grigliato
     *          lg_id_pu_LOC(:)              !id.pu associato alla lg

c      integer*4 num_cl_ns 			    !num.collettori
      integer*1, allocatable :: id_cl_LOC(:)   !id.collettori

      integer*1, allocatable :: id_vlg_LOC(:),   !id.valvole di grigliato
     *          vlg_id_lg_LOC(:),               !id.lg cui è associata la vlg
     *          vlg_id_cl_LOC(:)                !id.cl cui è associata la vlg
      integer*4, allocatable ::vlg_stato_LOC(:)  !stato valvole di grigliato

c      integer*4 num_vr_lg_ns 			!num.valvole su linee di grigliato (vr)
      integer*1, allocatable :: id_vr_lg_LOC(:), !id riduzioni su grigliato
     *          vr_id_lg_LOC(:)           !id.lg cui è associata la vr
      integer*4, allocatable :: dir_vr_lg_LOC(:)       !direzione connessione vr
	                                            !0->da lg a pu
										        !1->da pu a lg

c      integer*4 num_vd_lg_ns 			!!num.impianti rid su linee grigliato(vd)
      integer*1, allocatable :: id_vd_lg_LOC(:),    !id impianti rid.su grigliato
     *          vd_id_lg_LOC(:)           !id.lg cui è associata la vd
      integer*4, allocatable :: dir_vd_lg_LOC(:)       !direzione connessione vd
	                                            !0->da lg a pu
										        !1->da pu a lg

c      integer*4 num_vg_lg_ns 			!!num.impianti reg su linee grigliato(vg)
      integer*1, allocatable :: id_vg_lg_LOC(:),    !id impianti rid.su grigliato
     *          vg_id_lg_LOC(:)           !id.lg cui è associata la vg
      integer*4, allocatable :: dir_vg_lg_LOC(:)        !direzione connessione vg
	                                            !0->da lg a pu
										        !1->da pu a lg
c      integer*4 num_vr_cl_ns 			!num.valvole tra collettori
      integer*1, allocatable :: id_vr_cl_LOC(:),          !id riduzioni tra cl
     *          vr_id_clm_LOC(:),         !id coll monte della vr
     *          vr_id_clv_LOC(:)         !id coll monte della vr

c      integer*4 num_vd_cl_ns 			!num.impianti rid.tra collettori
      integer*1, allocatable :: id_vd_cl_LOC(:),          !id impianti rid.tra cl
     *          vd_id_clm_LOC(:),         !id coll monte della vd
     *          vd_id_clv_LOC(:)         !id coll monte della vd

c      integer*4 num_vg_cl_ns 			!num.impianti reg.tra collettori
      integer*1, allocatable :: id_vg_cl_LOC(:),          !id impianti reg.tra cl
     *          vg_id_clm_LOC(:),         !id coll monte della vg
     *          vg_id_clv_LOC(:)         !id coll monte della vg
 
c      integer*4 num_ce_cl_ns 			!num.centrali binarie
      integer*1, allocatable :: id_ce_cl_LOC(:),          !id centrali binarie
     *          ce_id_cl_asp_LOC(:),      !id coll monte della ce
     *          ce_id_cl_man_LOC(:)       !id coll monte della ce

c      integer*4 num_ass_cl_ns 		 !num.assetti mf
      integer*1, allocatable :: id_ass_cl_LOC(:),          !id assetti mf
     *          ass_id_cl_asp_LOC(:),      !id coll monte dell'ass
     *          ass_id_cl_man_LOC(:)       !id coll monte dell'ass

c      integer*4 num_cs_cl_ns 			! num.centrali semplificate
      integer*1, allocatable :: id_cs_cl_LOC(:),       !id centrali semplificate
     *          cs_id_cl_asp_LOC(:),       !id coll monte della cs
     *          cs_id_cl_man_LOC(:)        !id coll monte della cs
c---------------------------------------------------->nodo ridotto
c      integer*4 num_vc_nsrid         !numero valvole di collegamento
      integer*1, allocatable :: id_vc_LOC(:),        !id.valvole di collegamento
     *          id_pu_vc_m_LOC(:),	   !id. punto di connesione monte
     *          id_pu_vc_v_LOC(:)		   !id. punto di connesione valle
      integer*4, allocatable :: stato_vc_LOC(:)  !flag stato valvole collegamento

c      integer*4 num_vr_nsrid         !numero valvole 
      integer*1, allocatable :: id_vr_LOC(:),            !id.valvole 
     *          id_pu_vr_m_LOC(:),	   !id. punto di connesione monte
     *          id_pu_vr_v_LOC(:)  	   !id. punto di connesione valle

c      integer*4 num_vd_nsrid         !numero impianti di riduzione
      integer*1, allocatable :: id_vd_LOC(:),        !id.impianti di riduzione 
     *          id_pu_vd_m_LOC(:),	   !id. punto di connesione monte
     *          id_pu_vd_v_LOC(:)	       !id. punto di connesione valle

c      integer*4 num_vg_nsrid         !numero impianti di regolazione
      integer*1, allocatable :: id_vg_LOC(:),       !id.impianti di regolazione 
     *          id_pu_vg_m_LOC(:),	   !id. punto di connesione monte
     *          id_pu_vg_v_LOC(:)	       !id. punto di connesione valle

c      integer*4 num_ce_nsrid         !numero centrali binarie
      integer*1, allocatable :: id_ce_LOC(:),            !id.centrali binarie
     *          id_pu_ce_asp_LOC(:),     !id. punto di connesione aspirazione
     *          id_pu_ce_man_LOC(:)      !id. punto di connesione mandata

c      integer*4 num_ass_nsrid         !numero assetti
      integer*1, allocatable :: id_ass_LOC(:),            !id.assetti 
     *          id_pu_ass_asp_LOC(:),     !id. punto di connesione aspirazione
     *          id_pu_ass_man_LOC(:)      !id. punto di connesione mandata

c      integer*4 num_cs_nsrid         !numero centrali semplificate
      integer*1, allocatable :: id_cs_LOC(:),          !id.centrali semplificate
     *          id_pu_cs_asp_LOC(:),     !id. punto di connesione aspirazione
     *          id_pu_cs_man_LOC(:)      !id. punto di connesione mandata


c------------->output
c      integer*4 n_err                 !numero errori individuati nella riduzione
      integer*4, allocatable :: cod_err_LOC(:)     !codice d'errore 
     *         ,type_elem_LOC(:)     !codice d'errore 
      integer*1, allocatable :: cod_elem_LOC(:)    !codice elemento 
     *         ,pt_err_m_LOC(:)    !pu monte coinvolto nel collegamento errato                                     
     *         ,pt_err_v_LOC(:)    !pu monte coinvolto nel collegamento errato                                     

c      integer*4 n_warn        !numero errori individuati nella riduzione
      integer*4, allocatable :: cod_warn_LOC(:)   !codice d'errore 
      integer*1, allocatable :: pt_warn_m_LOC(:)!punti nel collegamento errato                                     
     *         ,pt_warn_v_LOC(:)!punti coinvolti nel collegamento errato 

c----------------------------------------------------------------------------------------------------
c----------------------------------------------------------------------------------------------------
c----------------------------------------------------------------------------------------------------
      count=0

	call get_DatLog(dat,log,ute)
	i_name_file = trim(dat) //'/'// 'nsm_input.txt' 
	o_name_file = trim(dat) //'/'// 'nsm_output.txt' 

      open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status)

c    
c     Apre il file di input   
      open(UNIT=30,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

100   continue  
      read(30,*,END=200) input      !num. punti rete

	file_input = '\\edsv10iz\UTENTI\elena\'//trim(input)
	count=count+1


      open(UNIT=10,FILE=file_input,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

      read(10,*) lun_guid      !num. punti rete

      read(10,*) num_pu_ns      !num. punti rete
	last_i=lun_guid*num_pu_ns
	read(10,*) id_pu(1:last_i) !id. punti rete

	read(10,*) num_lg_ns      !num. linne di grigliato
	last_i=lun_guid*num_lg_ns
	read(10,*) id_lg(1:last_i)
	read(10,*) lg_id_pu(1:last_i) 

	read(10,*) num_cl_ns      !num. collettori
	last_i=lun_guid*num_cl_ns
	read(10,*) id_cl(1:last_i) !id. collettori

	num_vlg_ns=num_lg_ns*num_cl_ns !numero valvole di grigliato
	last_i=lun_guid*num_vlg_ns
	read(10,*) id_vlg(1:last_i)
	read(10,*) vlg_id_lg(1:last_i)
	read(10,*) vlg_id_cl(1:last_i)
	read(10,*) vlg_stato(1:num_vlg_ns)    

	read(10,*) num_vr_lg_ns   !num. valvole su linee di grigliato
	if (num_vr_lg_ns.gt.0)then
	  last_i=lun_guid*num_vr_lg_ns
	  read(10,*) id_vr_lg(1:last_i)
	  read(10,*) vr_id_lg(1:last_i)
	  read(10,*) dir_vr_lg(1:num_vr_lg_ns)   !stato_vr_lg(ii)   
	end if 

	read(10,*) num_vd_lg_ns   !num. impianti rid su linee i grigliato
	if (num_vd_lg_ns.gt.0)then
	  last_i=lun_guid*num_vd_lg_ns
	  read(10,*) id_vd_lg(1:last_i)
	  read(10,*) vd_id_lg(1:last_i)
	  read(10,*) dir_vd_lg(1:num_vd_lg_ns)   !stato_vr_lg(ii)    
      end if

	read(10,*) num_vg_lg_ns   !num. impianti reg su linee i grigliato
	if (num_vg_lg_ns.gt.0)then
	  last_i=lun_guid*num_vg_lg_ns
	  read(10,*) id_vg_lg(1:last_i)
	  read(10,*) vg_id_lg(1:last_i)
	  read(10,*) dir_vg_lg(1:num_vg_lg_ns)   !stato_vr_lg(ii)    
      end if

	read(10,*) num_vr_cl_ns   !num. valvole tra collettori
	if (num_vr_cl_ns.gt.0)then
	  last_i=lun_guid*num_vr_cl_ns
	  read(10,*) id_vr_cl(1:last_i)
	  read(10,*) vr_id_clm(1:last_i)
	  read(10,*) vr_id_clv(1:last_i)
	end if

	read(10,*) num_vd_cl_ns   !num. impianti rid tra collettori
	if (num_vd_cl_ns.gt.0)then
	  last_i=lun_guid*num_vd_cl_ns
	  read(10,*) id_vd_cl(1:last_i)
	  read(10,*) vd_id_clm(1:last_i)
	  read(10,*) vd_id_clv(1:last_i)
      end if

	read(10,*) num_vg_cl_ns   !num. impianti reg tra collettori
	if (num_vg_cl_ns.gt.0)then
	  last_i=lun_guid*num_vg_cl_ns
	  read(10,*) id_vg_cl(1:last_i)
	  read(10,*) vg_id_clm(1:last_i)
	  read(10,*) vg_id_clv(1:last_i)
      end if

	read(10,*) num_ce_cl_ns   !num ce binarie tra cl
      if (num_ce_cl_ns.gt.0)then
        last_i=lun_guid*num_ce_cl_ns
	  read(10,*) id_ce_cl(1:last_i)
	  read(10,*) ce_id_cl_asp(1:last_i)
	  read(10,*) ce_id_cl_man(1:last_i)
      end if


	read(10,*) num_ass_cl_ns  !num assetti tra cl
      if (num_ass_cl_ns.gt.0)then
	  last_i=lun_guid*num_ass_cl_ns
	  read(10,*) id_ass_cl(1:last_i)
	  read(10,*) ass_id_cl_asp(1:last_i)
	  read(10,*) ass_id_cl_man(1:last_i)
      end if

	read(10,*) num_cs_cl_ns   !num ce semplificate tra cl
      if (num_cs_cl_ns.gt.0)then
	  last_i=lun_guid*num_cs_cl_ns
	  read(10,*) id_cs_cl(1:last_i)
	  read(10,*) cs_id_cl_asp(1:last_i)
	  read(10,*) cs_id_cl_man(1:last_i)
	end if                           
c---------------------------------------------------------------->riduzione nodo

      read(10,*) num_vc_nsrid   !num. vr su nodo ridotto 
      if (num_vc_nsrid.gt.0)then
	  last_i=lun_guid*num_vc_nsrid
	  read(10,*) id_vc(1:last_i)
	  read(10,*) id_pu_vc_m(1:last_i)
	  read(10,*) id_pu_vc_v(1:last_i)
	  read(10,*) stato_vc(1:num_vc_nsrid)
	end if

      read(10,*) num_vr_nsrid   !num. vr su nodo ridotto 
      if (num_vr_nsrid.gt.0)then
	  last_i=lun_guid*num_vr_nsrid
	  read(10,*) id_vr(1:last_i)
	  read(10,*) id_pu_vr_m(1:last_i)
	  read(10,*) id_pu_vr_v(1:last_i)
	end if
	
	read(10,*) num_vd_nsrid   !num. vd su nodo ridotto 
      if (num_vd_nsrid.gt.0)then
	  last_i=lun_guid*num_vd_nsrid
	  read(10,*) id_vd(1:last_i)
	  read(10,*) id_pu_vd_m(1:last_i)
	  read(10,*) id_pu_vd_v(1:last_i)
	end if
	
	read(10,*) num_vg_nsrid   !num. vg du nodo ridotto
      if (num_vg_nsrid.gt.0)then
	  last_i=lun_guid*num_vg_nsrid
	  read(10,*) id_vg(1:last_i)
	  read(10,*) id_pu_vg_m(1:last_i)
	  read(10,*) id_pu_vg_v(1:last_i)
	end if
	
	read(10,*) num_ce_nsrid   !num. ce su nodo ridotto
      if (num_ce_nsrid.gt.0)then
	  last_i=lun_guid*num_ce_nsrid
	  read(10,*) id_ce(1:last_i)
	  read(10,*) id_pu_ce_asp(1:last_i)
	  read(10,*) id_pu_ce_man(1:last_i)
	end if
		
	read(10,*) num_ass_nsrid  !num. ass su nodo ridotto
      if (num_ass_nsrid.gt.0)then
	  last_i=lun_guid*num_ass_nsrid
	  read(10,*) id_ass(1:last_i)
	  read(10,*) id_pu_ass_asp(1:last_i)
	  read(10,*) id_pu_ass_man(1:last_i)
	end if
	
	read(10,*) num_cs_nsrid   !num. cs su nodo ridotto
      if (num_cs_nsrid.gt.0)then
	  last_i=lun_guid*num_cs_nsrid
	  read(10,*) id_cs(1:last_i)
	  read(10,*) id_pu_cs_asp(1:last_i)
	  read(10,*) id_pu_cs_man(1:last_i)
	end if
	
      close (unit=10)	

c      call scrivi_dati(                    
c     *           lun_GUID,								 !lunghezza 	
c     *           num_pu_ns,id_pu,                          !punti rete
c     *           num_lg_ns,id_lg,lg_id_pu,                 !linee di grigliato
c     *           num_cl_ns,id_cl,                          !collettori
c     *           id_vlg,vlg_id_lg,vlg_id_cl,vlg_stato,     !valvole di grigliato
c     *           num_vr_lg_ns,id_vr_lg,vr_id_lg,dir_vr_lg, !valvole su linee
c     *           num_vd_lg_ns,id_vd_lg,vd_id_lg,dir_vd_lg, !impianti rid linee
c     *           num_vg_lg_ns,id_vg_lg,vg_id_lg,dir_vg_lg, !impianti reg linee
c     *           num_vr_cl_ns,id_vr_cl,vr_id_clm,vr_id_clv,!valvole collettori
c     *           num_vd_cl_ns,id_vd_cl,vd_id_clm,vd_id_clv,!impianti rid cl
c     *           num_vg_cl_ns,id_vg_cl,vg_id_clm,vg_id_clv,!impianti reg cl
c     *           num_ce_cl_ns,id_ce_cl,ce_id_cl_asp,ce_id_cl_man,     !centrali 
c     *           num_ass_cl_ns,id_ass_cl,ass_id_cl_asp,ass_id_cl_man, !assetti
c     *           num_cs_cl_ns,id_cs_cl,cs_id_cl_asp,cs_id_cl_man,     !ce sempli
c                                                          !nodo ridotto
c     *           num_vc_nsrid,id_vc,id_pu_vc_m,id_pu_vc_v,stato_vc,
c     *           num_vr_nsrid,id_vr,id_pu_vr_m,id_pu_vr_v,
c     *           num_vd_nsrid,id_vd,id_pu_vd_m,id_pu_vd_v,
c     *           num_vg_nsrid,id_vg,id_pu_vg_m,id_pu_vg_v,
c     *           num_ce_nsrid,id_ce,id_pu_ce_asp,id_pu_ce_man,
c     *           num_ass_nsrid,id_ass,id_pu_ass_asp,id_pu_ass_man,
c     *           num_cs_nsrid,id_cs,id_pu_cs_asp,id_pu_cs_man) 
C***********************************
c    ALLOCO LA MEMORIA
C***********************************
      allocate(id_pu_LOC(num_pu_ns*lun_guid),            
     *         id_lg_LOC(num_lg_ns*lun_guid),
     *         lg_id_pu_LOC(num_lg_ns*lun_guid),            
     *         id_cl_LOC(num_cl_ns*lun_guid),  
     *         id_vlg_LOC(num_vlg_ns*lun_guid),   
     *         vlg_id_lg_LOC(num_vlg_ns*lun_guid),               
     *         vlg_id_cl_LOC(num_vlg_ns*lun_guid),               
     *         vlg_stato_LOC(num_vlg_ns),  
     *         id_vr_lg_LOC(num_vr_lg_ns*lun_guid), 
     *         vr_id_lg_LOC(num_vr_lg_ns*lun_guid), 
     *         dir_vr_lg_LOC(num_vr_lg_ns),       
     *         id_vd_lg_LOC(num_vd_lg_ns*lun_guid),    
     *         vd_id_lg_LOC(num_vd_lg_ns*lun_guid),           
     *         dir_vd_lg_LOC(num_vd_lg_ns),       
     *         id_vg_lg_LOC(num_vg_lg_ns*lun_guid),    
     *         vg_id_lg_LOC(num_vg_lg_ns*lun_guid),           
     *         dir_vg_lg_LOC(num_vg_lg_ns),        
     *         id_vr_cl_LOC(num_vr_cl_ns*lun_guid),         
     *         vr_id_clm_LOC(num_vr_cl_ns*lun_guid),         
     *         vr_id_clv_LOC(num_vr_cl_ns*lun_guid),        
     *         id_vd_cl_LOC(num_vd_cl_ns*lun_guid),          
     *         vd_id_clm_LOC(num_vd_cl_ns*lun_guid),         
     *         vd_id_clv_LOC(num_vd_cl_ns*lun_guid),        
     *         id_vg_cl_LOC(num_vg_cl_ns*lun_guid),         
     *         vg_id_clm_LOC(num_vg_cl_ns*lun_guid),         
     *         vg_id_clv_LOC(num_vg_cl_ns*lun_guid),          
     *         id_ce_cl_LOC(num_ce_cl_ns*lun_guid),          
     *         ce_id_cl_asp_LOC(num_ce_cl_ns*lun_guid),     
     *         ce_id_cl_man_LOC(num_ce_cl_ns*lun_guid),      
     *         id_ass_cl_LOC(num_ass_cl_ns*lun_guid),          
     *         ass_id_cl_asp_LOC(num_ass_cl_ns*lun_guid),      
     *         ass_id_cl_man_LOC(num_ass_cl_ns*lun_guid),       
     *         id_cs_cl_LOC(num_cs_cl_ns*lun_guid),      
     *         cs_id_cl_asp_LOC(num_cs_cl_ns*lun_guid),       
     *         cs_id_cl_man_LOC(num_cs_cl_ns*lun_guid),        
c---------------------------------------------------->nodo ridotto
     *         id_vc_LOC(num_vc_nsrid*lun_guid),        
     *         id_pu_vc_m_LOC(num_vc_nsrid*lun_guid),	   
     *         id_pu_vc_v_LOC(num_vc_nsrid*lun_guid),		   
     *         stato_vc_LOC(num_vc_nsrid),    
     *         id_vr_LOC(num_vr_nsrid*lun_guid),            
     *         id_pu_vr_m_LOC(num_vr_nsrid*lun_guid),	   
     *         id_pu_vr_v_LOC(num_vr_nsrid*lun_guid),  	   
     *         id_vd_LOC(num_vd_nsrid*lun_guid),        
     *         id_pu_vd_m_LOC(num_vd_nsrid*lun_guid),	  
     *         id_pu_vd_v_LOC(num_vd_nsrid*lun_guid),	     
     *         id_vg_LOC(num_vg_nsrid*lun_guid),       
     *         id_pu_vg_m_LOC(num_vg_nsrid*lun_guid),	   
     *         id_pu_vg_v_LOC(num_vg_nsrid*lun_guid),	     
     *         id_ce_LOC(num_ce_nsrid*lun_guid),            
     *         id_pu_ce_asp_LOC(num_ce_nsrid*lun_guid),     
     *         id_pu_ce_man_LOC(num_ce_nsrid*lun_guid),      
     *         id_ass_LOC(num_ass_nsrid*lun_guid),            
     *         id_pu_ass_asp_LOC(num_ass_nsrid*lun_guid),     
     *         id_pu_ass_man_LOC(num_ass_nsrid*lun_guid),      
     *         id_cs_LOC(num_cs_nsrid*lun_guid),          
     *         id_pu_cs_asp_LOC(num_cs_nsrid*lun_guid),     
     *         id_pu_cs_man_LOC(num_cs_nsrid*lun_guid),      
c---------------------------------------------------->output
     *         cod_err_LOC(max_err),     
     *         type_elem_LOC(max_err),     
     *         cod_elem_LOC(max_err*lun_guid),    
     *         pt_err_m_LOC(max_err*lun_guid),                                       
     *         pt_err_v_LOC(max_err*lun_guid),                                        
     *         cod_warn_LOC(max_err),   
     *         pt_warn_m_LOC(max_err*lun_guid),                                    
     *         pt_warn_v_LOC(max_err*lun_guid)) 

c******************copio i valori letti nelle aree allocate
	last_i=lun_guid*num_pu_ns
	do i=1,last_i
	   id_pu_loc(i)=id_pu(i) 
      end do

	last_i=lun_guid*num_lg_ns
	do i=1,last_i
	   id_lg_loc(i)=id_lg(i) 
	   lg_id_pu_loc(i)=lg_id_pu(i) 
      end do

	last_i=lun_guid*num_cl_ns
	do i=1,last_i
	   id_cl_loc(i)=id_cl(i) 
      end do

	last_i=lun_guid*num_vlg_ns
	do i=1,last_i
	   id_vlg_loc(i)=id_vlg(i)
	   vlg_id_lg_loc(i)=vlg_id_lg(i)
	   vlg_id_cl_loc(i)=vlg_id_cl(i)
	end do
	do i=1,num_vlg_ns
         vlg_stato_loc(i)=vlg_stato(i)  
	end do  

	if (num_vr_lg_ns.gt.0)then
	  last_i=lun_guid*num_vr_lg_ns
	  do i=1,last_i
	     id_vr_lg_loc(i)=id_vr_lg(i)
	     vr_id_lg_loc(i)=vr_id_lg(i)
	  end do
	  do i=1,num_vr_lg_ns
	     dir_vr_lg_loc(i)=dir_vr_lg(i)      
	  end do
	end if 

	if (num_vd_lg_ns.gt.0)then
	  last_i=lun_guid*num_vd_lg_ns
	  do i=1,last_i
	     id_vd_lg_loc(i)=id_vd_lg(i)
	     vd_id_lg_loc(i)=vd_id_lg(i)
	  end do
	  do i=1,num_vd_lg_ns
	     dir_vd_lg_loc(i)=dir_vd_lg(i)      
	  end do
	end if 

	if (num_vg_lg_ns.gt.0)then
	  last_i=lun_guid*num_vg_lg_ns
	  do i=1,last_i
	     id_vg_lg_loc(i)=id_vg_lg(i)
	     vg_id_lg_loc(i)=vg_id_lg(i)
	  end do
	  do i=1,num_vg_lg_ns
	     dir_vg_lg_loc(i)=dir_vg_lg(i)      
	  end do
	end if 

	if (num_vr_cl_ns.gt.0)then
	  last_i=lun_guid*num_vr_cl_ns
	  do i=1,last_i
	     id_vr_cl_loc(i)=id_vr_cl(i)
	     vr_id_clm_loc(i)=vr_id_clm(i)
	     vr_id_clv_loc(i)=vr_id_clv(i)
	  end do
	end if

	if (num_vd_cl_ns.gt.0)then
	  last_i=lun_guid*num_vd_cl_ns
	  do i=1,last_i
	     id_vd_cl_loc(i)=id_vd_cl(i)
	     vd_id_clm_loc(i)=vd_id_clm(i)
	     vd_id_clv_loc(i)=vd_id_clv(i)
	  end do
	end if

	if (num_vg_cl_ns.gt.0)then
	  last_i=lun_guid*num_vg_cl_ns
	  do i=1,last_i
	     id_vg_cl_loc(i)=id_vg_cl(i)
	     vg_id_clm_loc(i)=vg_id_clm(i)
	     vg_id_clv_loc(i)=vg_id_clv(i)
	  end do
	end if

	if (num_ce_cl_ns.gt.0)then
	  last_i=lun_guid*num_ce_cl_ns
	  do i=1,last_i
	     id_ce_cl_loc(i)=id_ce_cl(i)
	     ce_id_cl_asp_loc(i)=ce_id_cl_asp(i)
	     ce_id_cl_man_loc(i)=ce_id_cl_man(i)
	  end do
	end if


	if (num_ass_cl_ns.gt.0)then
	  last_i=lun_guid*num_ass_cl_ns
	  do i=1,last_i
	     id_ass_cl_loc(i)=id_ass_cl(i)
	     ass_id_cl_asp_loc(i)=ass_id_cl_asp(i)
	     ass_id_cl_man_loc(i)=ass_id_cl_man(i)
	  end do
	end if

	if (num_cs_cl_ns.gt.0)then
	  last_i=lun_guid*num_cs_cl_ns
	  do i=1,last_i
	     id_cs_cl_loc(i)=id_cs_cl(i)
	     cs_id_cl_asp_loc(i)=cs_id_cl_asp(i)
	     cs_id_cl_man_loc(i)=cs_id_cl_man(i)
	  end do
	end if
c---------------------------------------------------------------->riduzione nodo

      if (num_vc_nsrid.gt.0)then
	  last_i=lun_guid*num_vc_nsrid
	  do i=1,last_i
	     id_vc_loc(i)=id_vc(i)
	     id_pu_vc_m_loc(i)=id_pu_vc_m(i)
	     id_pu_vc_v_loc(i)=id_pu_vc_v(i)
	  end do
	  do i=1,num_vc_nsrid
	     stato_vc_loc(i)=stato_vc(i)
	  end do
	end if

      if (num_vr_nsrid.gt.0)then
	  last_i=lun_guid*num_vr_nsrid
	  do i=1,last_i
	     id_vr_loc(i)=id_vr(i)
	     id_pu_vr_m_loc(i)=id_pu_vr_m(i)
	     id_pu_vr_v_loc(i)=id_pu_vr_v(i)
	  end do
	end if
	
      if (num_vd_nsrid.gt.0)then
	  last_i=lun_guid*num_vd_nsrid
	  do i=1,last_i
	     id_vd_loc(i)=id_vd(i)
	     id_pu_vd_m_loc(i)=id_pu_vd_m(i)
	     id_pu_vd_v_loc(i)=id_pu_vd_v(i)
	  end do
	end if
	
      if (num_vg_nsrid.gt.0)then
	  last_i=lun_guid*num_vg_nsrid
	  do i=1,last_i
	     id_vg_loc(i)=id_vg(i)
	     id_pu_vg_m_loc(i)=id_pu_vg_m(i)
	     id_pu_vg_v_loc(i)=id_pu_vg_v(i)
	  end do
	end if
	
      if (num_ce_nsrid.gt.0)then
	  last_i=lun_guid*num_ce_nsrid
	  do i=1,last_i
	     id_ce_loc(i)=id_ce(i)
	     id_pu_ce_asp_loc(i)=id_pu_ce_asp(i)
	     id_pu_ce_man_loc(i)=id_pu_ce_man(i)
	  end do
	end if
		
      if (num_ass_nsrid.gt.0)then
	  last_i=lun_guid*num_ass_nsrid
	  do i=1,last_i
	     id_ass_loc(i)=id_ass(i)
	     id_pu_ass_asp_loc(i)=id_pu_ass_asp(i)
	     id_pu_ass_man_loc(i)=id_pu_ass_man(i)
	  end do
	end if
      if (num_cs_nsrid.gt.0)then
	  last_i=lun_guid*num_cs_nsrid
	  do i=1,last_i
	     id_cs_loc(i)=id_cs(i)
	     id_pu_cs_asp_loc(i)=id_pu_cs_asp(i)
	     id_pu_cs_man_loc(i)=id_pu_cs_man(i)
	  end do
	end if

c      call verifica_nodo_ridotto(                    
c     *           lun_GUID,								 !lunghezza 	
c     *           num_pu_ns,id_pu,                          !punti rete
c     *           num_lg_ns,id_lg,lg_id_pu,                 !linee di grigliato
c     *           num_cl_ns,id_cl,                          !collettori
c     *           id_vlg,vlg_id_lg,vlg_id_cl,vlg_stato,     !valvole di grigliato
c     *           num_vr_lg_ns,id_vr_lg,vr_id_lg,dir_vr_lg, !valvole su linee
c     *           num_vd_lg_ns,id_vd_lg,vd_id_lg,dir_vd_lg, !impianti rid linee
c     *           num_vg_lg_ns,id_vg_lg,vg_id_lg,dir_vg_lg, !impianti reg linee
c     *           num_vr_cl_ns,id_vr_cl,vr_id_clm,vr_id_clv,!valvole collettori
c     *           num_vd_cl_ns,id_vd_cl,vd_id_clm,vd_id_clv,!impianti rid cl
c     *           num_vg_cl_ns,id_vg_cl,vg_id_clm,vg_id_clv,!impianti reg cl
c     *           num_ce_cl_ns,id_ce_cl,ce_id_cl_asp,ce_id_cl_man,     !centrali 
c     *           num_ass_cl_ns,id_ass_cl,ass_id_cl_asp,ass_id_cl_man, !assetti
c     *           num_cs_cl_ns,id_cs_cl,cs_id_cl_asp,cs_id_cl_man,     !ce sempli
c                                                          !nodo ridotto
c     *           num_vc_nsrid,id_vc,id_pu_vc_m,id_pu_vc_v,stato_vc,
c     *           num_vr_nsrid,id_vr,id_pu_vr_m,id_pu_vr_v,
c     *           num_vd_nsrid,id_vd,id_pu_vd_m,id_pu_vd_v,
c     *           num_vg_nsrid,id_vg,id_pu_vg_m,id_pu_vg_v,
c     *           num_ce_nsrid,id_ce,id_pu_ce_asp,id_pu_ce_man,
c     *           num_ass_nsrid,id_ass,id_pu_ass_asp,id_pu_ass_man,
c     *           num_cs_nsrid,id_cs,id_pu_cs_asp,id_pu_cs_man,
c-------------------------------------------------------------------------->output
c     *            n_err,cod_err,cod_elem,type_elem,pt_err_m,pt_err_v,
c     *            n_warn,cod_warn,pt_warn_m,pt_warn_v) 

      call verifica_nodo_ridotto(                    
     *           lun_GUID,								 	
     *           num_pu_ns,id_pu_LOC,                      
     *           num_lg_ns,id_lg_LOC,lg_id_pu_LOC,         
     *           num_cl_ns,id_cl_LOC,                         
     *           id_vlg_LOC,vlg_id_lg_LOC,vlg_id_cl_LOC,vlg_stato_LOC,     
     *           num_vr_lg_ns,id_vr_lg_LOC,vr_id_lg_LOC,dir_vr_lg_LOC, 
     *           num_vd_lg_ns,id_vd_lg_LOC,vd_id_lg_LOC,dir_vd_lg_LOC, 
     *           num_vg_lg_ns,id_vg_lg_LOC,vg_id_lg_LOC,dir_vg_lg_LOC, 
     *           num_vr_cl_ns,id_vr_cl_LOC,vr_id_clm_LOC,vr_id_clv_LOC,
     *           num_vd_cl_ns,id_vd_cl_LOC,vd_id_clm_LOC,vd_id_clv_LOC,
     *           num_vg_cl_ns,id_vg_cl_LOC,vg_id_clm_LOC,vg_id_clv_LOC,
     *           num_ce_cl_ns,id_ce_cl_LOC,
     *                               ce_id_cl_asp_LOC,ce_id_cl_man_LOC,      
     *           num_ass_cl_ns,id_ass_cl_LOC,
     *                             ass_id_cl_asp_LOC,ass_id_cl_man_LOC, 
     *           num_cs_cl_ns,id_cs_cl_LOC,
     *                               cs_id_cl_asp_LOC,cs_id_cl_man_LOC,    
     *           num_vc_nsrid,id_vc_LOC,
     *                      id_pu_vc_m_LOC,id_pu_vc_v_LOC,stato_vc_LOC,
     *           num_vr_nsrid,id_vr_LOC,id_pu_vr_m_LOC,id_pu_vr_v_LOC,
     *           num_vd_nsrid,id_vd_LOC,id_pu_vd_m_LOC,id_pu_vd_v_LOC,
     *           num_vg_nsrid,id_vg_LOC,id_pu_vg_m_LOC,id_pu_vg_v_LOC,
     *           num_ce_nsrid,id_ce_LOC,
     *                              id_pu_ce_asp_LOC,id_pu_ce_man_LOC,
     *           num_ass_nsrid,id_ass_LOC,
     *                            id_pu_ass_asp_LOC,id_pu_ass_man_LOC,
     *           num_cs_nsrid,id_cs_LOC,
     *                              id_pu_cs_asp_LOC,id_pu_cs_man_LOC,
     *           n_err,cod_err_LOC,cod_elem_LOC,
     *                        type_elem_LOC,pt_err_m_LOC,pt_err_v_LOC,
     *           n_warn,cod_warn_LOC,pt_warn_m_LOC,pt_warn_v_LOC) 

c
	write(20,*) 'Verifica numero:'
	write(20,*) count
	write(20,*) 'Esito:'

	if(n_err.eq.0 .and. n_warn.eq.0)then
         write(20,*) 'Riduzione nodo corretta'
         write(20,*) '*********************************'
	else if (n_err.eq.0 .and. n_warn.gt.0)then
         write(20,*) ' '	
	else
         write(20,*) 'Riduzione nodo non corretta'
	end if
	if(n_err.gt.0) then
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.1) then
	      if (cod_err_loc(i).eq.1) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'collegare'
	         write(20,*) 'il punto  '
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.2) then
	      if (cod_err_loc(i).eq.2) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire una valvola di collegamento:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.3) then
	      if (cod_err_loc(i).eq.3) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire la valvola:'
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.4) then
	      if (cod_err_loc(i).eq.4) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire una centrale bipolare:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.5) then
	      if (cod_err_loc(i).eq.5) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire un assetto:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.6) then
	      if (cod_err_loc(i).eq.6) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire una centrale semplificata:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.7) then
	      if (cod_err_loc(i).eq.7) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire un impianto di riduzione:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.8) then
	      if (cod_err_loc(i).eq.8) then
	         write(20,*) ''
	         write(20,*) 'NON E'' POSSIBILE'
	         write(20,*) 'inserire un impianto di regolazione:'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do

	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
	      if (cod_err_loc(i).eq.13) then
	         write(20,*) ''
	         write(20,*) ' E'' POSSIBILE'
	         write(20,*) 'inserire la valvola:'
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.20) then
	      if (cod_err_loc(i).eq.20) then
	         write(20,*) ''
	         write(20,*) 'Imposta piu'' di una connessione'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do

	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.21) then
	      if (cod_err_loc(i).eq.21) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE collegare' 
	         write(20,*) 'il punto'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.22) then
	      if (cod_err_loc(i).eq.22) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE inserire la valvola di riduzione:' 
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.23) then
	      if (cod_err_loc(i).eq.23) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE' 
	         write(20,*) 'inserire la centrale bipolare:'
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.24) then
	      if (cod_err_loc(i).eq.24) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE' 
	         write(20,*) 'inserire l''assetto:'
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.25) then
	      if (cod_err_loc(i).eq.25) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE' 
	         write(20,*) 'inserire la centrale semplificata:'
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.26) then
	      if (cod_err_loc(i).eq.26) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE' 
	         write(20,*) 'inserire l''impianto di riduzione:'
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.27) then
	      if (cod_err_loc(i).eq.27) then
	         write(20,*) ''
	         write(20,*) 'SI DEVE' 
	         write(20,*) 'inserire l''impianto di regolazione:'
c	         write(20,*) cod_elem(j:jj)
	         write(20,*) cod_elem_loc(j:jj)
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do

	   do i=1,n_err
	      j=(i-1)*lun_guid+1
	      jj=j+lun_guid-1
c	      if (cod_err(i).eq.30) then
	      if (cod_err_loc(i).eq.30) then
	         write(20,*) '' 
	         write(20,*) 'I collegamenti imposti nel nodo completo' 
	         write(20,*) 'inducono la presenza di 2 elementi in serie'
	         write(20,*) 'tra il punto:'
c	         write(20,*) pt_err_m(j:jj)
	         write(20,*) pt_err_m_loc(j:jj)
	         write(20,*) 'e il punto  '
c	         write(20,*) pt_err_v(j:jj)
	         write(20,*) pt_err_v_loc(j:jj)
	      end if
	   end do
         write(20,*) '*********************************************'

      end if




c***************************************************
C          LIBERO LA MEMORIA ALLOCATA 
C***************************************************
      deallocate(id_pu_LOC,            
     *         id_lg_LOC,
     *         lg_id_pu_loc,            
     *         id_cl_LOC,  
     *         id_vlg_LOC,   
     *         vlg_id_lg_LOC,               
     *         vlg_id_cl_LOC,               
     *         vlg_stato_loc,  
     *         id_vr_lg_LOC, 
     *         vr_id_lg_LOC, 
     *         dir_vr_lg_LOC,       
     *         id_vd_lg_LOC,    
     *         vd_id_lg_LOC,           
     *         dir_vd_lg_LOC,       
     *         id_vg_lg_LOC,    
     *         vg_id_lg_LOC,           
     *         dir_vg_lg_LOC,        
     *         id_vr_cl_LOC,         
     *         vr_id_clm_LOC,         
     *         vr_id_clv_LOC,        
     *         id_vd_cl_LOC,          
     *         vd_id_clm_LOC,         
     *         vd_id_clv_LOC,        
     *         id_vg_cl_LOC,         
     *         vg_id_clm_LOC,         
     *         vg_id_clv_LOC,          
     *         id_ce_cl_LOC,          
     *         ce_id_cl_asp_LOC,     
     *         ce_id_cl_man_LOC,      
     *         id_ass_cl_LOC,          
     *         ass_id_cl_asp_LOC,      
     *         ass_id_cl_man_LOC,       
     *         id_cs_cl_LOC,      
     *         cs_id_cl_asp_LOC,       
     *         cs_id_cl_man_LOC,        
c---------------------------------------------------->nodo ridotto
     *         id_vc_LOC,        
     *         id_pu_vc_m_LOC,	   
     *         id_pu_vc_v_LOC,		   
     *         stato_vc_LOC,    
     *         id_vr_LOC,            
     *         id_pu_vr_m_LOC,	   
     *         id_pu_vr_v_LOC,  	   
     *         id_vd_LOC,        
     *         id_pu_vd_m_LOC,	  
     *         id_pu_vd_v_LOC,	     
     *         id_vg_LOC,       
     *         id_pu_vg_m_LOC,	   
     *         id_pu_vg_v_LOC,	     
     *         id_ce_LOC,            
     *         id_pu_ce_asp_LOC,     
     *         id_pu_ce_man_LOC,      
     *         id_ass_LOC,            
     *         id_pu_ass_asp_LOC,     
     *         id_pu_ass_man_LOC,      
     *         id_cs_LOC,          
     *         id_pu_cs_asp_LOC,     
     *         id_pu_cs_man_LOC,      
c---------------------------------------------------->output
     *         cod_err_LOC,     
     *         type_elem_LOC,     
     *         cod_elem_LOC,    
     *         pt_err_m_LOC,                                       
     *         pt_err_v_LOC,                                        
     *         cod_warn_LOC,   
     *         pt_warn_m_LOC,                                    
     *         pt_warn_v_LOC) 

c********************************************************************
      go to 100
200	continue

      close (unit=30)	
      close (unit=20)	

9999  continue



	end program