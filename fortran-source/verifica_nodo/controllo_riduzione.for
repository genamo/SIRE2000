c*********************************************************************************

      subroutine scrivi_dati(                    
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
     *           num_cs_nsrid,id_cs,id_pu_cs_asp,id_pu_cs_man) 

C****************************************************************************************************
  ! Expose subroutine DLL_VERIFICA_NODO_RIDOTTO to users of this DLL
  !
  !DEC$ ATTRIBUTES DLLEXPORT::DLL_VERIFICA_NODO_RIDOTTO

  ! Variables
	implicit none
	include '../inc/nodo_ridotto.inc '	

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


      integer*4 last_i,status,num_vlg_ns
	character*(100) i_name_file

      character*(8) real_date
      character*(10) real_time
      character*(24) nsm_input

c********************************************************************************
      call DATE_AND_TIME (real_date,real_time)
      nsm_input=trim(real_date)//'_'//trim(real_time)//'.txt'
	i_name_file = '\\edsv10iz\UTENTI\elena\'//trim(nsm_input)

c	i_name_file = '\\edsv10iz\UTENTI\elena\nsm_input.txt'
c    
c     Apre il file di input   
      open(UNIT=10,FILE=i_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)

      write(10,*) lun_guid      !num. punti rete

      write(10,*) num_pu_ns      !num. punti rete
	last_i=lun_guid*num_pu_ns
	write(10,*) id_pu(1:last_i) !id. punti rete

	write(10,*) num_lg_ns      !num. linne di grigliato
	last_i=lun_guid*num_lg_ns
	write(10,*) id_lg(1:last_i)
	write(10,*) lg_id_pu(1:last_i) 

	write(10,*) num_cl_ns      !num. collettori
	last_i=lun_guid*num_cl_ns
	write(10,*) id_cl(1:last_i) !id. collettori

	num_vlg_ns=num_lg_ns*num_cl_ns !numero valvole di grigliato
	last_i=lun_guid*num_vlg_ns
	write(10,*) id_vlg(1:last_i)
	write(10,*) vlg_id_lg(1:last_i)
	write(10,*) vlg_id_cl(1:last_i)
	write(10,*) vlg_stato(1:num_vlg_ns)    

	write(10,*) num_vr_lg_ns   !num. valvole su linee di grigliato
	if (num_vr_lg_ns.gt.0)then
	  last_i=lun_guid*num_vr_lg_ns
	  write(10,*) id_vr_lg(1:last_i)
	  write(10,*) vr_id_lg(1:last_i)
	  write(10,*) dir_vr_lg(1:num_vr_lg_ns)   !stato_vr_lg(ii)   
	end if 

	write(10,*) num_vd_lg_ns   !num. impianti rid su linee i grigliato
	if (num_vd_lg_ns.gt.0)then
	  last_i=lun_guid*num_vd_lg_ns
	  write(10,*) id_vd_lg(1:last_i)
	  write(10,*) vd_id_lg(1:last_i)
	  write(10,*) dir_vd_lg(1:num_vd_lg_ns)   !stato_vr_lg(ii)    
      end if

	write(10,*) num_vg_lg_ns   !num. impianti reg su linee i grigliato
	if (num_vg_lg_ns.gt.0)then
	  last_i=lun_guid*num_vg_lg_ns
	  write(10,*) id_vg_lg(1:last_i)
	  write(10,*) vg_id_lg(1:last_i)
	  write(10,*) dir_vg_lg(1:num_vg_lg_ns)   !stato_vr_lg(ii)    
      end if

	write(10,*) num_vr_cl_ns   !num. valvole tra collettori
	if (num_vr_cl_ns.gt.0)then
	  last_i=lun_guid*num_vr_cl_ns
	  write(10,*) id_vr_cl(1:last_i)
	  write(10,*) vr_id_clm(1:last_i)
	  write(10,*) vr_id_clv(1:last_i)
	end if

	write(10,*) num_vd_cl_ns   !num. impianti rid tra collettori
	if (num_vd_cl_ns.gt.0)then
	  last_i=lun_guid*num_vd_cl_ns
	  write(10,*) id_vd_cl(1:last_i)
	  write(10,*) vd_id_clm(1:last_i)
	  write(10,*) vd_id_clv(1:last_i)
      end if

	write(10,*) num_vg_cl_ns   !num. impianti reg tra collettori
	if (num_vg_cl_ns.gt.0)then
	  last_i=lun_guid*num_vg_cl_ns
	  write(10,*) id_vg_cl(1:last_i)
	  write(10,*) vg_id_clm(1:last_i)
	  write(10,*) vg_id_clv(1:last_i)
      end if

	write(10,*) num_ce_cl_ns   !num ce binarie tra cl
      if (num_ce_cl_ns.gt.0)then
        last_i=lun_guid*num_ce_cl_ns
	  write(10,*) id_ce_cl(1:last_i)
	  write(10,*) ce_id_cl_asp(1:last_i)
	  write(10,*) ce_id_cl_man(1:last_i)
      end if

	write(10,*) num_ass_cl_ns  !num assetti tra cl
      if (num_ass_cl_ns.gt.0)then
	  last_i=lun_guid*num_ass_cl_ns
	  write(10,*) id_ass_cl(1:last_i)
	  write(10,*) ass_id_cl_asp(1:last_i)
	  write(10,*) ass_id_cl_man(1:last_i)
      end if

	write(10,*) num_cs_cl_ns   !num ce semplificate tra cl
      if (num_cs_cl_ns.gt.0)then
	  last_i=lun_guid*num_cs_cl_ns
	  write(10,*) id_cs_cl(1:last_i)
	  write(10,*) cs_id_cl_asp(1:last_i)
	  write(10,*) cs_id_cl_man(1:last_i)
	end if                           
c---------------------------------------------------------------->riduzione nodo

      write(10,*) num_vc_nsrid   !num. vr su nodo ridotto 
      if (num_vc_nsrid.gt.0)then
	  last_i=lun_guid*num_vc_nsrid
	  write(10,*) id_vc(1:last_i)
	  write(10,*) id_pu_vc_m(1:last_i)
	  write(10,*) id_pu_vc_v(1:last_i)
	  write(10,*) stato_vc(1:num_vc_nsrid)
	end if

      write(10,*) num_vr_nsrid   !num. vr su nodo ridotto 
      if (num_vr_nsrid.gt.0)then
	  last_i=lun_guid*num_vr_nsrid
	  write(10,*) id_vr(1:last_i)
	  write(10,*) id_pu_vr_m(1:last_i)
	  write(10,*) id_pu_vr_v(1:last_i)
	end if
	
	write(10,*) num_vd_nsrid   !num. vd su nodo ridotto 
      if (num_vd_nsrid.gt.0)then
	  last_i=lun_guid*num_vd_nsrid
	  write(10,*) id_vd(1:last_i)
	  write(10,*) id_pu_vd_m(1:last_i)
	  write(10,*) id_pu_vd_v(1:last_i)
	end if
	
	write(10,*) num_vg_nsrid   !num. vg du nodo ridotto
      if (num_vg_nsrid.gt.0)then
	  last_i=lun_guid*num_vg_nsrid
	  write(10,*) id_vg(1:last_i)
	  write(10,*) id_pu_vg_m(1:last_i)
	  write(10,*) id_pu_vg_v(1:last_i)
	end if
	
	write(10,*) num_ce_nsrid   !num. ce su nodo ridotto
      if (num_ce_nsrid.gt.0)then
	  last_i=lun_guid*num_ce_nsrid
	  write(10,*) id_ce(1:last_i)
	  write(10,*) id_pu_ce_asp(1:last_i)
	  write(10,*) id_pu_ce_man(1:last_i)
	end if
		
	write(10,*) num_ass_nsrid  !num. ass su nodo ridotto
      if (num_ass_nsrid.gt.0)then
	  last_i=lun_guid*num_ass_nsrid
	  write(10,*) id_ass(1:last_i)
	  write(10,*) id_pu_ass_asp(1:last_i)
	  write(10,*) id_pu_ass_man(1:last_i)
	end if
	
	write(10,*) num_cs_nsrid   !num. cs su nodo ridotto
      if (num_cs_nsrid.gt.0)then
	  last_i=lun_guid*num_cs_nsrid
	  write(10,*) id_cs(1:last_i)
	  write(10,*) id_pu_cs_asp(1:last_i)
	  write(10,*) id_pu_cs_man(1:last_i)
	end if

      close (unit=10)	

9999  continue

	return
	end

c***************************************************************************************************
      subroutine controlla_dimensioni
     *             (num_pu_ns,num_cl_ns,num_lg_ns,num_ass_cl_ns,ier)


      implicit none

	include '../inc/nodo_ridotto.inc '	

     
      integer*4 num_pu_ns,       !(I)
     *          num_lg_ns,       !(I)
     *          num_cl_ns,       !(I)
     *          num_ass_cl_ns,   !(I)
     *          ier              !(O)

      ier=0

	if (num_pu_ns.gt.max_rt_nsm)  ier =1000

	if (num_lg_ns.gt.max_lg_nsm)  ier =2000

	if (num_cl_ns.gt.max_cl_nsm)  ier =3000

	if (num_ass_cl_ns.gt.max_assetti_nsm)  ier =4000

	return
	end
c***************************************************************************************************




