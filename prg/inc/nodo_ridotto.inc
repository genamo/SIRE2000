c*************************************************************c
c     PARAMETRI USATI NELLA VERIFICA DEL NODO RIDOTTO         c
c*************************************************************c
!-----------------------------------------------------------------------
!                       NODO COMPLETO:  e_nsm
!-----------------------------------------------------------------------
      integer*4 max_rt_nsm    !max rt punti rete (anchor point)
     -         ,max_lg_nsm    !max lg linee grigliato
     -         ,max_cl_nsm    !max cl collettori
     -         ,max_vr_lg_nsm !max vr riduttori linea/punto
     -         ,max_vs_lg_nsm !max vs valvole sez. linea/punto
     -         ,max_vd_lg_nsm !max impianti rid. linea/punto
     -         ,max_vg_lg_nsm !max impianti reg. linea/punto
     -         ,max_pz_nsm    !max pz pozzi
     -         ,max_st_nsm    !max st stoccaggi
     -         ,max_rg_nsm    !max rg regolatori
     -         ,max_vr_cl_nsm !max vr riduttori tra collettori
     -         ,max_vs_cl_nsm !max vs valvole sez. tra collettori
     -         ,max_vd_cl_nsm !max vr riduttori tra collettori
     -         ,max_vg_cl_nsm !max vr riduttori tra collettori
     -         ,max_ce_nsm    !max ce centrali ce/cm tra collettori
     -         ,max_cs_nsm    !max ce centrali ce/cm tra collettori
     -         ,max_vc_nsm    !max vc valvole di collegamento
     -         ,max_assetti_nsm    !max vc valvole di collegamento

      parameter(max_rt_nsm    = 20
     -         ,max_lg_nsm    = 30
     -         ,max_cl_nsm    = 30
     -         ,max_vr_lg_nsm = 30
     -         ,max_vs_lg_nsm = 30
     -         ,max_vd_lg_nsm = 30
     -         ,max_vg_lg_nsm = 30
     -         ,max_vr_cl_nsm = 15
     -         ,max_vs_cl_nsm = 15
     -         ,max_vd_cl_nsm = 15
     -         ,max_vg_cl_nsm = 15
     -         ,max_ce_nsm    =  1
     -         ,max_cs_nsm    =  2
     -         ,max_vc_nsm    = 50
     -         ,max_assetti_nsm    = 30
     -)

      integer*4 max_vr_nsm    !max valvole
     -         ,max_vd_nsm    !max impianti rid
     -         ,max_vg_nsm    !max impianti reg
     -         ,max_vlg_nsm   !max impianti reg

      parameter(max_vr_nsm  =  max_vr_lg_nsm + max_vr_cl_nsm
     -         ,max_vd_nsm  =  max_vd_lg_nsm + max_vd_cl_nsm
     -         ,max_vg_nsm  =  max_vg_lg_nsm + max_vg_cl_nsm
     -         ,max_vlg_nsm =  max_cl_nsm*max_lg_nsm
     -)

      integer*4   max_link
      parameter ( max_link=2*max_cl_nsm*max_cl_nsm)
      integer*4   max_err
      parameter ( max_err=max_rt_nsm*max_rt_nsm)

      integer*4  STATO_IND
      parameter (STATO_IND  = 0) 		!INDEFINITO ENUMERAZIONE

c-----> stati valvole di grigliato in input (rif. tabella codici)

      integer*4 vlg_stato_off,          !Off
     *          vlg_stato_on,           !On
     *          vlg_stato_libero,        !libero
     *          vlg_stato_riduce        !libero

      parameter (vlg_stato_off=0,
     *           vlg_stato_on=1,
     *           vlg_stato_libero=2,
     *           vlg_stato_riduce=3)
 
c-----> stati valvole di grigliato dopo elaborazione

      integer*4 stato_off,          !Off
     *          stato_on,           !On
     *          stato_rid_cl_pt,    !Riduzione da collettore a punto
     *          stato_rid_pt_cl     !Riduzione da punto a collettore

      parameter (stato_off=0,
     *           stato_on=1,
     *           stato_rid_cl_pt=2,
     *           stato_rid_pt_cl=-2)
     

c-----> tipi di collegamento possibile

      integer*4   link_ko,
     *            nessun_link,
     *            vl_sez,
     *            vr_gri,
     *            vd_gri,
     *            vg_gri,
     *            ce_bin,
     *            ass_mf,
     *            ce_semp,
     *            vr_col,
     *            vd_col,
     *            vg_col

      parameter ( link_ko=-1,
     *            nessun_link=0,
     *            vl_sez=1,
     *            vr_gri=2,
     *            vd_gri=3,
     *            vg_gri=4,
     *            vr_col=5,
     *            vd_col=6,
     *            vg_col=7,
     *            ce_bin=8,
     *            ass_mf=9,
     *            ce_semp=10)


      integer*4 sempre_off,          !Off
     *          mai_off,           !On
     *          cambia_stato    !Riduzione da collettore a punto

      parameter (sempre_off=1,
     *           mai_off=2,
     *           cambia_stato=3)
c----------------------------------------------------------------------------------
      integer*4   lunGuid			!lunghezza GUID elementi

      common /parametri/ lunGuid		
      integer*4   max_lunGuid
      parameter ( max_lunGuid=36)

c-----------------------------------------------------------------------------------
c
c	SireElementType: tipo restituito al programma C#
c

      integer*4   undefined,
     *            valve,
     *            bipolar_plant,
     *            mf_plant,
     *            mf_plant_array,
     *            reduction_plant,
     *            regulation_plant,
     *            simplified_plant,
     *            connection_valve

      parameter ( undefined=0,
     *            valve=2,
     *            bipolar_plant=3,
     *            mf_plant=16,
     *            mf_plant_array=17,
     *            reduction_plant=18,
     *            regulation_plant=19,
     *            simplified_plant=20,
     *            connection_valve=21)


