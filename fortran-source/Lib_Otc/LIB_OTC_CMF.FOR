c-------------------------------------------------------------------------

      Subroutine optcent_mf_MOD(ier)
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/OTC.INC'
	include '../inc/ASS_COND.INC'
c

c    i_staz(MAX_SERV)  I  indici delle centrali
c    start_cost/shut_cost  I  costo di accensione/ spegnimento
c    opt_time         I  tempo di ottimizzazione
c    dhs/dqs/tab_grid   I  
c    ndisp(i)            numero di unita' disponibili per il servizio i-esimo
c    type_num         I  massimo numero di tipi della centrale multifunzione
c    unit_avail_mf    I  disponibilita' dell'unita' j assegnata al servizio i
c                        = 0, se l'unita' non и abilitata
c                        = 2, se l'unita' и necessariamente accesa
c                        = 1, se l'unita' и disponibile
c    type_avail_mf(i,j)  disponibilita' del tipo j per il servizio i-esimo:
c    num_servizi      I  numero di servizi da ottimizzare
c    ind_u(j)         O  assegnazione delle unita' ai due servizi:
c                          -  0, se l'unita' j non viene assegnata
c                          -  1, se l'unita' j viene assegnata al servizio s1
c                          -  2, se l'unita' j viene assegnata al servizio s2
c    ier              O  indice di errore:
c                        - 11, non e' stata trovata una configurazione ottima
c                        - 12, tipo di funzionamento non corretto
c                        - 13, non esistono assetti compatibili
c                        - 14, gli assetti da provare violano la massima
c                              pressione di uscita
c
      INTEGER*4 ier    ! O)
c
      INTEGER*4 ndisp(max_serv)
      INTEGER*4 nserv

!-informazioni topologiche relative agli assetti
      INTEGER*4 vert_type_mf(max_serv)
      character*3 status_mf(maxunits,max_serv)
      REAL*4    stat_vars_mf(12,max_serv)
      INTEGER*4 first_u_mf(max_serv),first_t_mf(max_serv),
     *          istaz_mf(max_serv),vert_poi_mf(max_serv)
      INTEGER*4 type_quant_mf(maxunits,max_serv)
      INTEGER*4 type_avail_mf(maxunits,max_serv)
      INTEGER*4 unit_avail_mf(maxunits,max_serv)
      INTEGER*4 set_flag_mf(max_serv)






!-condizioni iniziali necessarie per simulare gli assetti
      REAL*4    flow2_mf(max_serv),temp1_mf(max_serv),
     *          pres1_mf(max_serv),pres2_mf(max_serv),
cgpe
     *          delpr1_mf(max_serv),delpr2_mf(max_serv),
cgpe-end
     *          air_mf(max_serv)
      INTEGER*4 stat_ord_mf(max_serv)
      REAL*4    start_cost_mf(maxunits,max_serv)
     *         ,shut_cost_mf(maxunits,max_serv)

cgpe      INTEGER*4 unit_avail_old(max_unit)
cgpe      REAL*4    stat_vars_old(12,max_vert)
      LOGICAL*2 linv
      INTEGER*4 j,iserv,IU,IT,I,K,II1
c-------------------------------------------------------------------------
      ier = 0





cmar_ass_cond
cmar_ass_cond-25-feb-2012  if(.not.sim_s)then

      do iserv=1,num_servizi
        i_serv(i_vert(iserv)) = iserv
      end do


      nserv = num_servizi

cgpe      call calc_avail_unita(nserv,unit_avail_old,ndisp,linv)
      call calc_avail_unita(nserv,unit_avail,ndisp,linv)

c la routine agg_dati_stazione и stata slegat a dal concetto di assetto,
c ma и legata solamente al concetto di servizio attivo
      call agg_dati_stazione_as(nserv,first_u_mf,first_t_mf,istaz_mf,
     *     vert_poi_mf,type_quant_mf,type_avail_mf,unit_avail_mf,
     *     vert_type_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
cgpe
     *     delpr1_mf,delpr2_mf,
cgpe-end
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      IF(FLAG_ASS_COND_S) THEN 
      nserv=1
      ENDIF

cmar_ass_cond
      call opt_cent_mf(istaz_mf,vert_type_mf,
     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
     *     unit_num(i_vert(1)),type_num(i_vert(1)),
     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
     *     unit_avail_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf,ier)



C      call opt_cent_mf(istaz_mf,vert_type_mf,
C     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
C     *     unit_num(i_vert(1)),type_num(i_vert(1)),
C     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
C     *     unit_avail_mf,
cgpe-new
C     *     set_flag_mf,
cgpe-new-end
C     *     flow2_mf,pres1_mf,pres2_mf,
C     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
C     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
C     *     start_cost_mf,shut_cost_mf,ier)

cmar_ass_cond-25-feb-2012 	endif

cmar_ass_cond
c      call ripristina_dati_stazione_as(nserv,first_u_mf,first_t_mf,
c     *     istaz_mf,vert_poi_mf,type_quant_mf,type_avail_mf,
c     *     unit_avail_mf,vert_type_mf,
c     *     set_flag_mf,
c     *     flow2_mf,pres1_mf,pres2_mf,
c     *     delpr1_mf,delpr2_mf,
c     *     temp1_mf,air_mf,stat_ord_mf,
c     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      if (ier.eq.0) then
        do i=1,maxunits
          ind_u(i) = ind_s(ind_opt,i)
        end do
      end if ! if (ier.eq.0)
c



cmar_ass_cond
ccccccccc      else
cmar_ass_cond
 

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccc


c-------------------------------------------------------------------------

      Subroutine optcent_mf(ier)
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/OTC.INC'
	include '../inc/ASS_COND.INC'
c

c    i_staz(MAX_SERV)  I  indici delle centrali
c    start_cost/shut_cost  I  costo di accensione/ spegnimento
c    opt_time         I  tempo di ottimizzazione
c    dhs/dqs/tab_grid   I  
c    ndisp(i)            numero di unita' disponibili per il servizio i-esimo
c    type_num         I  massimo numero di tipi della centrale multifunzione
c    unit_avail_mf    I  disponibilita' dell'unita' j assegnata al servizio i
c                        = 0, se l'unita' non и abilitata
c                        = 2, se l'unita' и necessariamente accesa
c                        = 1, se l'unita' и disponibile
c    type_avail_mf(i,j)  disponibilita' del tipo j per il servizio i-esimo:
c    num_servizi      I  numero di servizi da ottimizzare
c    ind_u(j)         O  assegnazione delle unita' ai due servizi:
c                          -  0, se l'unita' j non viene assegnata
c                          -  1, se l'unita' j viene assegnata al servizio s1
c                          -  2, se l'unita' j viene assegnata al servizio s2
c    ier              O  indice di errore:
c                        - 11, non e' stata trovata una configurazione ottima
c                        - 12, tipo di funzionamento non corretto
c                        - 13, non esistono assetti compatibili
c                        - 14, gli assetti da provare violano la massima
c                              pressione di uscita
c
      INTEGER*4 ier    ! O)
c
      INTEGER*4 ndisp(max_serv)
      INTEGER*4 nserv

!-informazioni topologiche relative agli assetti
      INTEGER*4 vert_type_mf(max_serv)
      character*3 status_mf(maxunits,max_serv)
      REAL*4    stat_vars_mf(12,max_serv)
      INTEGER*4 first_u_mf(max_serv),first_t_mf(max_serv),
     *          istaz_mf(max_serv),vert_poi_mf(max_serv)
      INTEGER*4 type_quant_mf(maxunits,max_serv)
      INTEGER*4 type_avail_mf(maxunits,max_serv)
      INTEGER*4 unit_avail_mf(maxunits,max_serv)
      INTEGER*4 set_flag_mf(max_serv)






!-condizioni iniziali necessarie per simulare gli assetti
      REAL*4    flow2_mf(max_serv),temp1_mf(max_serv),
     *          pres1_mf(max_serv),pres2_mf(max_serv),
cgpe
     *          delpr1_mf(max_serv),delpr2_mf(max_serv),
cgpe-end
     *          air_mf(max_serv)
      INTEGER*4 stat_ord_mf(max_serv)
      REAL*4    start_cost_mf(maxunits,max_serv)
     *         ,shut_cost_mf(maxunits,max_serv)

cgpe      INTEGER*4 unit_avail_old(max_unit)
cgpe      REAL*4    stat_vars_old(12,max_vert)
      LOGICAL*2 linv
      INTEGER*4 j,iserv,IU,IT,I,K,II1
c-------------------------------------------------------------------------


      ier = 0


cmar_ass_cond
CMAR     if(.not.sim_s)then



	 if(.not.sim (I_STAZ(1))  )then



      do iserv=1,num_servizi
        i_serv(i_vert(iserv)) = iserv
      end do


      nserv = num_servizi

cgpe      call calc_avail_unita(nserv,unit_avail_old,ndisp,linv)
      call calc_avail_unita(nserv,unit_avail,ndisp,linv)

c la routine agg_dati_stazione и stata slegat a dal concetto di assetto,
c ma и legata solamente al concetto di servizio attivo
      call agg_dati_stazione_as(nserv,first_u_mf,first_t_mf,istaz_mf,
     *     vert_poi_mf,type_quant_mf,type_avail_mf,unit_avail_mf,
     *     vert_type_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
cgpe
     *     delpr1_mf,delpr2_mf,
cgpe-end
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      IF(FLAG_ASS_COND_S) THEN 
      nserv=1
      ENDIF

cmar_ass_cond
      call opt_cent_mf(istaz_mf,vert_type_mf,
     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
     *     unit_num(i_vert(1)),type_num(i_vert(1)),
     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
     *     unit_avail_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf,ier)
	else

	
      
CMAR                    ****************************************
      call agg_dati_stazione_as(nserv,first_u_mf,first_t_mf,istaz_mf,
     *     vert_poi_mf,type_quant_mf,type_avail_mf,unit_avail_mf,
     *     vert_type_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
cgpe
     *     delpr1_mf,delpr2_mf,
cgpe-end
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)


CMAR                    ****************************************      
	     IF(FLAG_ASS_COND_S) THEN 
            nserv=1
           ENDIF

cmar_ass_cond
      call opt_cent_mf(istaz_mf,vert_type_mf,
     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
     *     unit_num(i_vert(1)),type_num(i_vert(1)),
     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
     *     unit_avail_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf,ier)

	endif
cmar_ass_cond
c      call ripristina_dati_stazione_as(nserv,first_u_mf,first_t_mf,
c     *     istaz_mf,vert_poi_mf,type_quant_mf,type_avail_mf,
c     *     unit_avail_mf,vert_type_mf,
c     *     set_flag_mf,
c     *     flow2_mf,pres1_mf,pres2_mf,
c     *     delpr1_mf,delpr2_mf,
c     *     temp1_mf,air_mf,stat_ord_mf,
c     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      if (ier.eq.0) then
        do i=1,maxunits
          ind_u(i) = ind_s(ind_opt,i)
        end do
      end if ! if (ier.eq.0)
c



cmar_ass_cond
ccccccccc      else
cmar_ass_cond
    
    

      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccc



cmar_ass_cond



      Subroutine optcent_mf_OR(ier)
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/OTC.INC'
	include '../inc/ASS_COND.INC'
c

c    i_staz(MAX_SERV)  I  indici delle centrali
c    start_cost/shut_cost  I  costo di accensione/ spegnimento
c    opt_time         I  tempo di ottimizzazione
c    dhs/dqs/tab_grid   I  
c    ndisp(i)            numero di unita' disponibili per il servizio i-esimo
c    type_num         I  massimo numero di tipi della centrale multifunzione
c    unit_avail_mf    I  disponibilita' dell'unita' j assegnata al servizio i
c                        = 0, se l'unita' non и abilitata
c                        = 2, se l'unita' и necessariamente accesa
c                        = 1, se l'unita' и disponibile
c    type_avail_mf(i,j)  disponibilita' del tipo j per il servizio i-esimo:
c    num_servizi      I  numero di servizi da ottimizzare
c    ind_u(j)         O  assegnazione delle unita' ai due servizi:
c                          -  0, se l'unita' j non viene assegnata
c                          -  1, se l'unita' j viene assegnata al servizio s1
c                          -  2, se l'unita' j viene assegnata al servizio s2
c    ier              O  indice di errore:
c                        - 11, non e' stata trovata una configurazione ottima
c                        - 12, tipo di funzionamento non corretto
c                        - 13, non esistono assetti compatibili
c                        - 14, gli assetti da provare violano la massima
c                              pressione di uscita
c
      INTEGER*4 ier    ! O)
c
      INTEGER*4 ndisp(max_serv)
      INTEGER*4 nserv

!-informazioni topologiche relative agli assetti
      INTEGER*4 vert_type_mf(max_serv)
      character*3 status_mf(maxunits,max_serv)
      REAL*4    stat_vars_mf(12,max_serv)
      INTEGER*4 first_u_mf(max_serv),first_t_mf(max_serv),
     *          istaz_mf(max_serv),vert_poi_mf(max_serv)
      INTEGER*4 type_quant_mf(maxunits,max_serv)
      INTEGER*4 type_avail_mf(maxunits,max_serv)
      INTEGER*4 unit_avail_mf(maxunits,max_serv)
      INTEGER*4 set_flag_mf(max_serv)






!-condizioni iniziali necessarie per simulare gli assetti
      REAL*4    flow2_mf(max_serv),temp1_mf(max_serv),
     *          pres1_mf(max_serv),pres2_mf(max_serv),
cgpe
     *          delpr1_mf(max_serv),delpr2_mf(max_serv),
cgpe-end
     *          air_mf(max_serv)
      INTEGER*4 stat_ord_mf(max_serv)
      REAL*4    start_cost_mf(maxunits,max_serv)
     *         ,shut_cost_mf(maxunits,max_serv)

cgpe      INTEGER*4 unit_avail_old(max_unit)
cgpe      REAL*4    stat_vars_old(12,max_vert)
      LOGICAL*2 linv
      INTEGER*4 j,iserv,IU,IT,I,K,II1
c-------------------------------------------------------------------------
      ier = 0



cmar_ass_cond
      if(.not.sim_s)then



CMAR	 if(.not.sim (I_STAZ(1))  )then

      do iserv=1,num_servizi
        i_serv(i_vert(iserv)) = iserv
      end do


      nserv = num_servizi

cgpe      call calc_avail_unita(nserv,unit_avail_old,ndisp,linv)
      call calc_avail_unita(nserv,unit_avail,ndisp,linv)

c la routine agg_dati_stazione и stata slegat a dal concetto di assetto,
c ma и legata solamente al concetto di servizio attivo
      call agg_dati_stazione_as(nserv,first_u_mf,first_t_mf,istaz_mf,
     *     vert_poi_mf,type_quant_mf,type_avail_mf,unit_avail_mf,
     *     vert_type_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
cgpe
     *     delpr1_mf,delpr2_mf,
cgpe-end
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      IF(FLAG_ASS_COND_S) THEN 
      nserv=1
      ENDIF

cmar_ass_cond
      call opt_cent_mf(istaz_mf,vert_type_mf,
     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
     *     unit_num(i_vert(1)),type_num(i_vert(1)),
     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
     *     unit_avail_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf,ier)
	else

	     IF(FLAG_ASS_COND_S) THEN 
            nserv=1
           ENDIF

cmar_ass_cond
      call opt_cent_mf(istaz_mf,vert_type_mf,
     *     tipo_criterio(i_vert(1)),vert_poi_mf,nserv,linv,
     *     unit_num(i_vert(1)),type_num(i_vert(1)),
     *     first_u_mf,first_t_mf,type_quant_mf,type_avail_mf,
cgpe     *     unit_avail_mf,flow2_mf,pres1_mf,pres2_mf,temp1_mf,
     *     unit_avail_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,temp1_mf,
cgpe-end
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf,ier)

	endif
cmar_ass_cond
c      call ripristina_dati_stazione_as(nserv,first_u_mf,first_t_mf,
c     *     istaz_mf,vert_poi_mf,type_quant_mf,type_avail_mf,
c     *     unit_avail_mf,vert_type_mf,
c     *     set_flag_mf,
c     *     flow2_mf,pres1_mf,pres2_mf,
c     *     delpr1_mf,delpr2_mf,
c     *     temp1_mf,air_mf,stat_ord_mf,
c     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      if (ier.eq.0) then
        do i=1,maxunits
          ind_u(i) = ind_s(ind_opt,i)
        end do
      end if ! if (ier.eq.0)
c



cmar_ass_cond
ccccccccc      else
cmar_ass_cond


      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccc







c-------------------------------------------------------------------------
        Subroutine tbg_ass(ier)
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/OTC.INC'
	include '../inc/ASS_COND.INC'
c

c    i_staz(MAX_SERV)  I  indici delle centrali
c    start_cost/shut_cost  I  costo di accensione/ spegnimento
c    opt_time         I  tempo di ottimizzazione
c    dhs/dqs/tab_grid   I  
c    ndisp(i)            numero di unita' disponibili per il servizio i-esimo
c    type_num         I  massimo numero di tipi della centrale multifunzione
c    unit_avail_mf    I  disponibilita' dell'unita' j assegnata al servizio i
c                        = 0, se l'unita' non и abilitata
c                        = 2, se l'unita' и necessariamente accesa
c                        = 1, se l'unita' и disponibile
c    type_avail_mf(i,j)  disponibilita' del tipo j per il servizio i-esimo:
c    num_servizi      I  numero di servizi da ottimizzare
c    ind_u(j)         O  assegnazione delle unita' ai due servizi:
c                          -  0, se l'unita' j non viene assegnata
c                          -  1, se l'unita' j viene assegnata al servizio s1
c                          -  2, se l'unita' j viene assegnata al servizio s2
c    ier              O  indice di errore:
c                        - 11, non e' stata trovata una configurazione ottima
c                        - 12, tipo di funzionamento non corretto
c                        - 13, non esistono assetti compatibili
c                        - 14, gli assetti da provare violano la massima
c                              pressione di uscita
c
      INTEGER*4 ier    ! O)
c
      INTEGER*4 ndisp(max_serv)
      INTEGER*4 nserv

!-informazioni topologiche relative agli assetti
      INTEGER*4 vert_type_mf(max_serv)
      character*3 status_mf(maxunits,max_serv)
      REAL*4    stat_vars_mf(12,max_serv)
      INTEGER*4 first_u_mf(max_serv),first_t_mf(max_serv),
     *          istaz_mf(max_serv),vert_poi_mf(max_serv)
      INTEGER*4 type_quant_mf(maxunits,max_serv)
      INTEGER*4 type_avail_mf(maxunits,max_serv)
      INTEGER*4 unit_avail_mf(maxunits,max_serv)
      INTEGER*4 set_flag_mf(max_serv)






!-condizioni iniziali necessarie per simulare gli assetti
      REAL*4    flow2_mf(max_serv),temp1_mf(max_serv),
     *          pres1_mf(max_serv),pres2_mf(max_serv),
cgpe
     *          delpr1_mf(max_serv),delpr2_mf(max_serv),
cgpe-end
     *          air_mf(max_serv)
      INTEGER*4 stat_ord_mf(max_serv)
      REAL*4    start_cost_mf(maxunits,max_serv)
     *         ,shut_cost_mf(maxunits,max_serv)

cgpe      INTEGER*4 unit_avail_old(max_unit)
cgpe      REAL*4    stat_vars_old(12,max_vert)
      LOGICAL*2 linv
      INTEGER*4 j,iserv,IU,IT,I,K,II1
c-------------------------------------------------------------------------
      ier = 0
	
cmar_ass_cond
ccccccc      if(flag_ass_cond_s)then
      num_servizi=2
cmar_ass_cond
      do iserv=1,num_servizi
        i_serv(i_vert(iserv)) = iserv
      end do


      nserv = num_servizi



      call ripristina_dati_stazione_as(nserv,first_u_mf,first_t_mf,
     *     istaz_mf,vert_poi_mf,type_quant_mf,type_avail_mf,
     *     unit_avail_mf,vert_type_mf,
     *     set_flag_mf,
     *     flow2_mf,pres1_mf,pres2_mf,
     *     delpr1_mf,delpr2_mf,
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond





      if (ier.eq.0) then
        do i=1,maxunits
          ind_u(i) = ind_s(ind_opt,i)
        end do
      end if ! if (ier.eq.0)
c



      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccc


c-------------------------------------------------------------------------
        Subroutine tbgV()
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/tv.inc'
      include '../inc/rk_param.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/MF_TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
      include '../inc/messag.inc'
      include '../inc/cmf_assetti.inc'

	include '../inc/ASS_COND.INC'
      INTEGER*4 ier,iserv
      INTEGER*4 I,J,K,iu,ia
      LOGICAL*2 iopt_aux
      character*(tit_len) msg_err
      INTEGER*4 I_ERR
      INTEGER*4 ker
      character*(l_N_RTN) nome_routine
	logical*2 l_act_(m_ogg)
	integer*4 iserv_,h(m_ogg)
c-------------------------------------------------------------------------
      ier = 0




cmar_ass_cond
ccccccc      if(flag_ass_cond_s)then
      num_servizi=2
cmar_ass_cond
CMAR_0806      	i=1
CMAR_0806      I=first_unit(i_vert(1))

c	do j=1,24
      do j = first_unit(i_vert(1)),first_unit(i_vert(1))+23
	l_act_(j)=.false.
	enddo

c      do j=1,24
       do j = first_unit(i_vert(1)),first_unit(i_vert(1))+23

	if(unit_avail_as(j).ne.0)then
C	l_act_(j)=l_act(i)  
C	i=i+1
      l_act_(j)=l_act(J)  
	endif
      
       
	enddo

      

c	do j=1,24
      do j = first_unit(i_vert(1)),first_unit(i_vert(1))+23

	l_act(j)=.false.


	l_act(j)=l_act_(j)


	enddo

c      do j=1,24
       do j = first_unit(i_vert(1)),first_unit(i_vert(1))+23
              lstatus(j) = OFF
              if (l_act(j)) then
c	h(j)=1
                lstatus(j) = ON
              endif
           enddo
c


    

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccc
c-------------------------------------------------------------------------
        Subroutine tbg_bck(ier)
c
c     Ottimizzazione di una centrale multifunzione:
c     la subroutine opt_cent_mf trova lo stato ottimo che minimizza
c     o il consumo o la differenza con un consumo da rispettare
c     (con associate le unita' attive per ciascun servizio e il costo).
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/OTC.INC'
	include '../inc/ASS_COND.INC'
c

c    i_staz(MAX_SERV)  I  indici delle centrali
c    start_cost/shut_cost  I  costo di accensione/ spegnimento
c    opt_time         I  tempo di ottimizzazione
c    dhs/dqs/tab_grid   I  
c    ndisp(i)            numero di unita' disponibili per il servizio i-esimo
c    type_num         I  massimo numero di tipi della centrale multifunzione
c    unit_avail_mf    I  disponibilita' dell'unita' j assegnata al servizio i
c                        = 0, se l'unita' non и abilitata
c                        = 2, se l'unita' и necessariamente accesa
c                        = 1, se l'unita' и disponibile
c    type_avail_mf(i,j)  disponibilita' del tipo j per il servizio i-esimo:
c    num_servizi      I  numero di servizi da ottimizzare
c    ind_u(j)         O  assegnazione delle unita' ai due servizi:
c                          -  0, se l'unita' j non viene assegnata
c                          -  1, se l'unita' j viene assegnata al servizio s1
c                          -  2, se l'unita' j viene assegnata al servizio s2
c    ier              O  indice di errore:
c                        - 11, non e' stata trovata una configurazione ottima
c                        - 12, tipo di funzionamento non corretto
c                        - 13, non esistono assetti compatibili
c                        - 14, gli assetti da provare violano la massima
c                              pressione di uscita
c
      INTEGER*4 ier    ! O)
c
      INTEGER*4 ndisp(max_serv)
      INTEGER*4 nserv

!-informazioni topologiche relative agli assetti
      INTEGER*4 vert_type_mf(max_serv)
      character*3 status_mf(maxunits,max_serv)
      REAL*4    stat_vars_mf(12,max_serv)
      INTEGER*4 first_u_mf(max_serv),first_t_mf(max_serv),
     *          istaz_mf(max_serv),vert_poi_mf(max_serv)
      INTEGER*4 type_quant_mf(maxunits,max_serv)
      INTEGER*4 type_avail_mf(maxunits,max_serv)
      INTEGER*4 unit_avail_mf(maxunits,max_serv)
      INTEGER*4 set_flag_mf(max_serv)






!-condizioni iniziali necessarie per simulare gli assetti
      REAL*4    flow2_mf(max_serv),temp1_mf(max_serv),
     *          pres1_mf(max_serv),pres2_mf(max_serv),
cgpe
     *          delpr1_mf(max_serv),delpr2_mf(max_serv),
cgpe-end
     *          air_mf(max_serv)
      INTEGER*4 stat_ord_mf(max_serv)
      REAL*4    start_cost_mf(maxunits,max_serv)
     *         ,shut_cost_mf(maxunits,max_serv)

cgpe      INTEGER*4 unit_avail_old(max_unit)
cgpe      REAL*4    stat_vars_old(12,max_vert)
      LOGICAL*2 linv
      INTEGER*4 j,iserv,IU,IT,I,K,II1
c-------------------------------------------------------------------------
      ier = 0
	
cmar_ass_cond
ccccccc      if(flag_ass_cond_s)then
      num_servizi=2
cmar_ass_cond
      do iserv=1,num_servizi
        i_serv(i_vert(iserv)) = iserv
      end do


      nserv = num_servizi




cmar_ass_cond



cgpe      call calc_avail_unita(nserv,unit_avail_old,ndisp,linv)
      call calc_avail_unita(nserv,unit_avail,ndisp,linv)

c la routine agg_dati_stazione и stata slegat a dal concetto di assetto,
c ma и legata solamente al concetto di servizio attivo
      call agg_dati_stazione_as(nserv,first_u_mf,first_t_mf,istaz_mf,
     *     vert_poi_mf,type_quant_mf,type_avail_mf,unit_avail_mf,
     *     vert_type_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,
cgpe
     *     delpr1_mf,delpr2_mf,
cgpe-end
     *     temp1_mf,air_mf,stat_ord_mf,
     *     status_mf,stat_vars_mf,start_cost_mf,shut_cost_mf)
cmar_ass_cond
      nserv=1
cmar_ass_cond

cmar_ass_cond

      if (ier.eq.0) then
        do i=1,maxunits
          ind_u(i) = ind_s(ind_opt,i)
        end do
      end if ! if (ier.eq.0)
c



cmar_ass_cond
ccccccccc      else
cmar_ass_cond

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccc
CMAR_ASS_ACOND-------------------------------------------------------------------

      Subroutine agg_dati_stazione(nserv,
     *     first_u_mf,first_t_mf,istaz_mf,vert_poi_mf,
     *     type_quant_mf,type_avail_mf,unit_avail_mf,vert_type_mf,
cgpe     *     flow2_mf,pres1_mf,pres2_mf,temp1_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,delpr1_mf,delpr2_mf,temp1_mf,
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf)
c
c  Genera tutte le condizioni di input per l'ottimizzazione della
c  centrale multifunzione, assegnando alle variabili con indice 1
c  i valori delle variabili del servizio con il numero minore di
c  unita'.Aggiorna i dati di centrale (stat_vars_mf)
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/cmf_assetti.inc'
c
      INTEGER*4 nserv
      INTEGER*4 first_u_mf(*),first_t_mf(*),
     *          istaz_mf(*),vert_poi_mf(*)
      INTEGER*4 type_quant_mf(maxunits,max_serv),
     *          type_avail_mf(maxunits,max_serv),
     *          unit_avail_mf(maxunits,max_serv)
      integer*4 vert_type_mf(*),set_flag_mf(*)
      REAL*4  flow2_mf(*),pres1_mf(*),pres2_mf(*),
cgpe
     *        delpr1_mf(*),delpr2_mf(*),
cgpe-end
     *        temp1_mf(*),air_mf(*)
      INTEGER*4 stat_ord_mf(*)
      CHARACTER*3  status_mf(maxunits,max_serv)

      REAL*4  stat_vars_mf(12,max_serv)
      REAL*4  start_cost_mf(maxunits,max_serv)
     *       ,shut_cost_mf(maxunits,max_serv)

      INTEGER*4 i1,ii1,i,iu,it,i_a
      INTEGER*4 iserv

      integer*4 J,K
c-------------------------------------------------------------------------
      do iserv = 1,nserv
        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
        air_mf(iserv)        = taria(i_vert(iserv))
        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
        first_u_mf(iserv)    = first_unit(i_vert(iserv))
        first_t_mf(iserv)    = first_type(i_vert(iserv))
        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
        istaz_mf(iserv)      = i_staz(iserv)
        vert_poi_mf(iserv)   = i_vert(iserv)

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_quant_mf(i,iserv) = type_quant(it)
        end do

        i1 = 0
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            unit_avail_mf(k+i1,iserv) = unit_avail(ii1)
          end do
          iu=iu+type_quant(j)
          i1=i1+type_quant(j)
        end do

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_avail_mf(i,iserv) = type_avail(it)
        end do

        iu = first_unit(i_vert(iserv))
        do i = 1,unit_num(i_vert(iserv))
          status_mf(i,iserv) = status(iu+i-1)
          start_cost_mf(i,iserv) = unit_start_cost(iu+i-1)
          shut_cost_mf(i,iserv)  = unit_shut_cost(iu+i-1)
        end do

        do k = 1,10
	     stat_vars_mf(k,iserv)= stat_vars(k,i_vert(iserv))
        end do
      end do
c
      return
      end
cmar*******************************************************************************************
cmar_ass_cond-----ORIGINALE--------------------------------------------------------------------
!	      Subroutine agg_dati_stazione(nserv,
!     *     first_u_mf,first_t_mf,istaz_mf,vert_poi_mf,
!     *     type_quant_mf,type_avail_mf,unit_avail_mf,vert_type_mf,
cgpe     *     flow2_mf,pres1_mf,pres2_mf,temp1_mf,
cgpe-new
!     *     set_flag_mf,
cgpe-new-end
!     *     flow2_mf,pres1_mf,pres2_mf,delpr1_mf,delpr2_mf,temp1_mf,
!     *     air_mf,stat_ord_mf,status_mf,stat_vars_mfc
!     *     start_cost_mf,shut_cost_mf)
c
c  Genera tutte le condizioni di input per l'ottimizzazione della
c  centrale multifunzione, assegnando alle variabili con indice 1
c  i valori delle variabili del servizio con il numero minore di
c  unita'.Aggiorna i dati di centrale (stat_vars_mf)
c-------------------------------------------------------------------------
!      implicit none
*&*
!      include '../inc/param.inc'
!      include '../inc/stazione.inc'
!      include '../inc/rk_param.inc'
!      include '../inc/stations.inc'
!      include '../inc/units.inc'
!      include '../inc/types.inc'
!      include '../inc/cmf_assetti.inc'
!	include '../inc/ass_cond.inc'
c
!      INTEGER*4 nserv
!      INTEGER*4 first_u_mf(*),first_t_mf(*),
!     *          istaz_mf(*),vert_poi_mf(*)
!      INTEGER*4 type_quant_mf(maxunits,max_serv),
!     *          type_avail_mf(maxunits,max_serv),
!     *          unit_avail_mf(maxunits,max_serv)
!      integer*4 vert_type_mf(*),set_flag_mf(*)
!      REAL*4  flow2_mf(*),pres1_mf(*),pres2_mf(*),
cgpe
!     *        delpr1_mf(*),delpr2_mf(*),
cgpe-end
!     *        temp1_mf(*),air_mf(*)
!      INTEGER*4 stat_ord_mf(*)
!      CHARACTER*3  status_mf(maxunits,max_serv)
!
!      REAL*4  stat_vars_mf(12,max_serv)
!      REAL*4  start_cost_mf(maxunits,max_serv)
!     *       ,shut_cost_mf(maxunits,max_serv)

!      INTEGER*4 i1,ii1,i,iu,it,i_a
!      INTEGER*4 iserv

!     integer*4 J,K
c-------------------------------------------------------------------------
!      do iserv = 1,nserv
!        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
!        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
!        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
!        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
!        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
!        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
!        air_mf(iserv)        = taria(i_vert(iserv))
!        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
!        first_u_mf(iserv)    = first_unit(i_vert(iserv))
!        first_t_mf(iserv)    = first_type(i_vert(iserv))
!        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
!        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
!        istaz_mf(iserv)      = i_staz(iserv)
!        vert_poi_mf(iserv)   = i_vert(iserv)

!        do i = 1,type_num(i_vert(iserv))
!          it = first_type(i_vert(iserv))+i-1
!          type_quant_mf(i,iserv) = type_quant(it)
!        end do
   

!        i1 = 0
!        iu = first_unit(i_vert(iserv))
!        it = first_type(i_vert(iserv))
!        do i=1,type_num(i_vert(iserv))
!          j=it+i-1
!          do k=1,type_quant(j)
!            ii1=iu+k-1
!            unit_avail_mf(k+i1,iserv) = unit_avail(ii1)
!          end do
!          iu=iu+type_quant(j)
!          i1=i1+type_quant(j)
!        end do

!        do i = 1,type_num(i_vert(iserv))
!          it = first_type(i_vert(iserv))+i-1
!          type_avail_mf(i,iserv) = type_avail(it)
!        end do

!        iu = first_unit(i_vert(iserv))
!        do i = 1,unit_num(i_vert(iserv))
!          status_mf(i,iserv) = status(iu+i-1)
!          start_cost_mf(i,iserv) = unit_start_cost(iu+i-1)
!          shut_cost_mf(i,iserv)  = unit_shut_cost(iu+i-1)
!        end do

!        do k = 1,10
!	     stat_vars_mf(k,iserv)= stat_vars(k,i_vert(iserv))
!        end do
!      end do
c
!      return
!      end
cmar_ass_cond----------------------------------------------------------------------------------
cmar*******************************************************************************************


	Subroutine agg_dati_stazione_as(nserv,
     *     first_u_mf,first_t_mf,istaz_mf,vert_poi_mf,
     *     type_quant_mf,type_avail_mf,unit_avail_mf,vert_type_mf,
cgpe     *     flow2_mf,pres1_mf,pres2_mf,temp1_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,delpr1_mf,delpr2_mf,temp1_mf,
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf)
c
c  Genera tutte le condizioni di input per l'ottimizzazione della
c  centrale multifunzione, assegnando alle variabili con indice 1
c  i valori delle variabili del servizio con il numero minore di
c  unita'.Aggiorna i dati di centrale (stat_vars_mf)
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/cmf_assetti.inc'
	include '../inc/ass_cond.inc'



	 include '../inc/filtri.inc'
      include '../inc/air_cooler.inc'
c
    
ccccccc   da verificare la dimensione       ccccccccc

      INTEGER*4 ordine(max_type)
cmar_ass_cond    scarico
c      COMMON/ASS_COND1/ordine
cmar_ass_cond
c
      INTEGER*4 nserv
      INTEGER*4 first_u_mf(*),first_t_mf(*),
     *          istaz_mf(*),vert_poi_mf(*)
      INTEGER*4 type_quant_mf(maxunits,max_serv),
     *          type_avail_mf(maxunits,max_serv),
     *          unit_avail_mf(maxunits,max_serv)
      integer*4 vert_type_mf(*),set_flag_mf(*)
      REAL*4  flow2_mf(*),pres1_mf(*),pres2_mf(*),
cgpe
     *        delpr1_mf(*),delpr2_mf(*),
cgpe-end
     *        temp1_mf(*),air_mf(*)
      INTEGER*4 stat_ord_mf(*)
      CHARACTER*3  status_mf(maxunits,max_serv)



      REAL*4  stat_vars_mf(12,max_serv)
      REAL*4  start_cost_mf(maxunits,max_serv)
     *       ,shut_cost_mf(maxunits,max_serv)

      INTEGER*4 i1,ii1,i,iu,it,i_a
      INTEGER*4 iserv

      integer*4 J,K

cmar_ass_cond

	integer*4 ii, jj, kk, primo, ll, lll, iii, iv,
      
	
	       
     *          ordine1(maxunits,max_serv), ordine_(maxunits), 
     
     *          type_nun_as(10000)
      
	
cmar_ass_cond	CHARACTER*3 status_as(maxunits)


c      INTEGER*4 ordine(maxunits)     !OUTPUT: ordine(posizione)=indice tbg
C      INTEGER*4 NTP_1, NTP_2


c-------------------------------------------------------------------------



cmarrrrr_23_04       
       if(flag_ass_cond_s)then

CCC	IF(sim_s)THEN
C  FA SOLO L'AGGIORNAMENTO CIN:
ccc      IF(FLAG_DATI)THEN

cmarrrrr_23_04          IF(  flag_dati_s( istaz_mf(1) )  ) THEN


CMAR_14_05_2013      	IF(sim_s)THEN
      IF(sim (I_STAZ(1)) )THEN

		


      do iserv = 1,2
	first_unit_(i_vert(iserv))=first_unit(i_vert(iserv))

        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
        first_u_mf(iserv)    = first_unit(i_vert(iserv))
        first_t_mf(iserv)    = first_type(i_vert(iserv))
        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
       istaz_mf(iserv)      = i_staz(iserv)
        vert_poi_mf(iserv)   = i_vert(iserv)

      enddo






       nserv=2
      do iserv = 1,nserv

CCC	first_unit_(i_vert(iserv))=first_unit(i_vert(iserv))


        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
        air_mf(iserv)        = taria(i_vert(iserv))
CCC        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
CCC        first_u_mf(iserv)    = first_unit(i_vert(iserv))
CCC        first_t_mf(iserv)    = first_type(i_vert(iserv))
CCC        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
CCC        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
CCC       istaz_mf(iserv)      = i_staz(iserv)
CCC        vert_poi_mf(iserv)   = i_vert(iserv)

CMAR_ASS_COND: NECESSARIO PER VETTORE DI MULTIFNI:
          do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_quant_mf(i,iserv) = type_quant(it)

	type_quant_as(it)=type_quant(it)
	type_quant_mf_as(i,iserv)=type_quant_mf(i,iserv)




      type_avail_mf(i,iserv) = type_avail(it)



        end do


CMAR_ASS_COND	

	


      ENDDO




	ELSE
	
      UNIT_1_(I_STAZ(1))=0
	UNIT_2_(I_STAZ(1))=0
	NTP_1_(I_STAZ(1))=0
	NTP_2_(I_STAZ(1))=0


    
	ll=1
	iii=1


      nserv=2
      do iserv = 1,nserv

	first_unit_(i_vert(iserv))=first_unit(i_vert(iserv))


        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
        air_mf(iserv)        = taria(i_vert(iserv))
        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
        first_u_mf(iserv)    = first_unit(i_vert(iserv))
        first_t_mf(iserv)    = first_type(i_vert(iserv))
        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
        istaz_mf(iserv)      = i_staz(iserv)
        vert_poi_mf(iserv)   = i_vert(iserv)

       

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_quant_mf(i,iserv) = type_quant(it)

	type_quant_as(it)=type_quant(it)
	type_quant_mf_as(i,iserv)=type_quant_mf(i,iserv)

        end do
   

        i1 = 0
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            unit_avail_mf(k+i1,iserv) = unit_avail(ii1)
		  
		  
cmar_ass_cond
      IF(ISERV.EQ.1)THEN

	IF(unit_avail_mf(k+i1,iserv).NE.0)THEN

	UNIT_1_(I_STAZ(1))=UNIT_1_(I_STAZ(1))+1
		
	ENDIF

	ELSE

	IF(unit_avail_mf(k+i1,iserv).NE.0)THEN

	UNIT_2_(I_STAZ(1))=UNIT_2_(I_STAZ(1))+1
	ENDIF

      ENDIF

cmar_ass_cond

      
	unit_avail_as(ii1)=unit_avail(ii1)
	
	unit_avail_mf_as(k+i1,iserv)=unit_avail_mf(k+i1,iserv)

	ordine1(k+i1,iserv)=ordine_(k+i1)
	ordine1(k+i1,iserv)=(ordine1(k+i1,iserv))*
     *                           (unit_avail_mf(k+i1,iserv))

	 if(ordine1(k+i1,iserv).ne.0)then

	 ordine1(ll,iserv)=ordine1(k+i1,iserv)
	 ll=ll+1
     
	 endif
      
	  
          end do
   

        do lll=ll, maxunits
        ordine1(lll,iserv)=0
	  enddo

          iu=iu+type_quant(j)
          i1=i1+type_quant(j)
        end do

 
   
	do iv=1, maxunits
       if(ordine1(iv,iserv).ne.0)then
	ordine(iii)=ordine1(iv,iserv)
	iii=iii+1
      end if
	enddo
      iii=iii

        do i = 1,type_num(i_vert(iserv))
         it = first_type(i_vert(iserv))+i-1
          type_avail_mf(i,iserv) = type_avail(it)
cmar_ass_cond
      IF(ISERV.EQ.1)THEN

	IF(type_avail_mf(i,iserv).NE.0)THEN
		

	NTP_1_(I_STAZ(1))=NTP_1_(I_STAZ(1))+1

	

	ENDIF

	ELSE

	IF(type_avail_mf(i,iserv).NE.0)THEN
      	

	NTP_2_(I_STAZ(1))=NTP_2_(I_STAZ(1))+1

	


	ENDIF



      ENDIF

cmar_ass_cond

	type_avail_mf_as(i,iserv)=type_avail_mf(i,iserv)





        end do

        iu = first_unit(i_vert(iserv))
        do i = 1,unit_num(i_vert(iserv))
          status_mf(i,iserv) = status(iu+i-1)

ccccc	 
      status_as(iu+i-1) = status(iu+i-1)





          start_cost_mf(i,iserv) = unit_start_cost(iu+i-1)
          shut_cost_mf(i,iserv)  = unit_shut_cost(iu+i-1)
        end do

        do k = 1,10
	     stat_vars_mf(k,iserv)= stat_vars(k,i_vert(iserv))
        end do
      end do


c
     
cmar_Ass_cond_rpt_a   ii=1
cmar_Ass_cond_rpt_a	jj=1
cmar_Ass_cond_rpt_a	kk=1
      ii=first_unit(i_vert(1))
	jj=first_unit(i_vert(1))	
      kk=first_unit(i_vert(1))


cmar_Ass_cond_rpt_a	primo=0 
      primo=first_unit(i_vert(1))-1


cmar_Ass_cond_rpt_a       

      do i=  first_unit(i_vert(1)),first_unit(i_vert(1))+11
      if(unit_avail(i).ne.0)then

          
	primo=primo+1

	endif

      enddo
cmar_Ass_cond_rpt_a     

c      ordine(i) = (unit_avail(i))*ordine(i)

      do i=  first_unit(i_vert(1)),first_unit(i_vert(1))+23

      if(unit_avail(i).ne.0)then

          unit_avail(jj)=unit_avail(i)

         
	jj=jj+1

 
      endif


      enddo


cmmmmmmmmmmmmmmmmm 
	
	     do i=  first_unit(i_vert(1)),first_unit(i_vert(1))+23

	if(type_avail (i).ne.0)then
           type_avail(kk)=type_avail (i)

	kk=kk+1


	endif

	enddo

cиииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииии
cиииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииииии

	     do i= first_unit(i_vert(1)),first_unit(i_vert(1))+23
     
c	if(type_quant(i).ne.0)then
	
      	type_quant(i)=type_avail(i)
	CC(I)=type_quant(i)
c	ii=ii+1

c	endif
	 end do
cmar_ass_cond_rpt_
      
	type_num(i_vert(1))=0
	type_num(i_vert(2))=0
c assetto 1


	do i=first_unit(i_vert(1)),first_unit(i_vert(1))+11
	


      if(type_avail(i) .ne. 0) then
	 

	 type_num(i_vert(1))= type_num(i_vert(1))+1
	endif
C     	D(I)=type_num(I)
	enddo
c assetto 2

		do i=first_unit(i_vert(2)),first_unit(i_vert(2))+11




cmar_01_lug	type_num_as(i)=type_num(i)
      if(type_avail(i) .ne. 0) then

	 type_num(i_vert(2))= type_num(i_vert(2))+1
	endif
C		D(I)=type_num(I)
	enddo






cmar_Ass_cond_rpt_a      	do i=13,24
c	type_avail_mf_as(i, 2)=0
c	type_avail_mf(i, 2)=0
c	type_avail(i)=0
c	type_quant_mf(i,2)=0
c	unit_avail_mf(i,2)=0
c      type_avail_mf(i,2)=0
      
      do i=first_unit(i_vert(2)),first_unit(i_vert(2))+11
	unit_avail(i)=0

	type_avail(i)=0

	type_quant(i)=0

	end do






	   


      do iserv = 1,nserv

        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
        air_mf(iserv)        = taria(i_vert(iserv))
        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
        first_u_mf(iserv)    = first_unit(i_vert(iserv))
        first_t_mf(iserv)    = first_type(i_vert(iserv))
        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
        istaz_mf(iserv)      = i_staz(iserv)
        vert_poi_mf(iserv)   = i_vert(iserv)

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_quant_mf(i,iserv) = type_quant(it)
        end do
   

        i1 = 0
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            unit_avail_mf(k+i1,iserv) = unit_avail(ii1)
	EE(II1)=unit_avail(ii1)
          end do
          iu=iu+type_quant(j)
          i1=i1+type_quant(j)
        end do

        do i = 1,type_num(i_vert(iserv))
         it = first_type(i_vert(iserv))+i-1
          type_avail_mf(i,iserv) = type_avail(it)
cc	BB(IT)=type_avail(it)
        end do

        iu = first_unit(i_vert(iserv))
        do i = 1,unit_num(i_vert(iserv))
          status_mf(i,iserv) = status(iu+i-1)
          start_cost_mf(i,iserv) = unit_start_cost(iu+i-1)
          shut_cost_mf(i,iserv)  = unit_shut_cost(iu+i-1)
        end do

        do k = 1,10
	     stat_vars_mf(k,iserv)= stat_vars(k,i_vert(iserv))
        end do
      end do
  
     

cmar  ass_cond

cmar  ass_cond      do iserv= 1, nserv   
      


cmar_ass_cond_ordina
    	do i=  first_unit(i_vert(1)),first_unit(i_vert(1))+23

     
       if(unit_avail_as(i).ne.0)then
         unit_avail_as_o(i)=1
       endif

      enddo
cmar_ass_cond_ordina   
      
cmar_ass_cond_nom_flow	
	call ordina_as(unit_avail_as_o,nom_flow,
     *                       first_unit(i_vert(1)))  

		call ordina_as(unit_avail_as_o,unit_eff_corr,
     *                       first_unit(i_vert(1)))  
     
		call ordina_as(unit_avail_as_o,nom_power,
     *                       first_unit(i_vert(1)))  
     

		call ordina_as(unit_avail_as_o,unit_power_corr,
     *                       first_unit(i_vert(1)))  
     
     
     	call ordina_as(unit_avail_as_o,nom_hrate,
     *                       first_unit(i_vert(1)))   
 

		call ordina_as(unit_avail_as_o,c_f_tg_k,
     *                       first_unit(i_vert(1)))   

		call ordina_as(unit_avail_as_o,c_ac1_k,
     *                       first_unit(i_vert(1)))   
cmar_ass_cond_nom_flow

      call ordina_as(unit_avail_as_o,unit_crit_min,
     *                       first_unit(i_vert(1)))

	 




	call ordina_as(unit_avail_as_o,unit_crit_max,
     *                       first_unit(i_vert(1)))







	call ordina_type_as(unit_avail_as_o,type_a_coef,
     *                       first_unit(i_vert(1)))






	call ordina_type_as(unit_avail_as_o,type_b_coef,
     *                       first_unit(i_vert(1)))


    



	call ordina_type_as_ch(unit_avail_as_o,type_chn,
     *                       first_unit(i_vert(1)))



	call ordina_type_as_ch(unit_avail_as_o,type_chc,
     *                       first_unit(i_vert(1)))





      call ordina_type_c_as(unit_avail_as_o,type_c_coef,
     *                       first_unit(i_vert(1)))


 


	call ordina_type_as_cen(unit_avail_as_o,type_cen,    
     *                       first_unit(i_vert(1)))





	call ordina_type_as_ch_p(unit_avail_as_o,type_cec,
     *                       first_unit(i_vert(1)))





      call ordina_type_3(unit_avail_as_o,type_cpwm,
     *                       first_unit(i_vert(1)))







	call ordina_type_c_as(unit_avail_as_o,type_cpwt,
     *                       first_unit(i_vert(1)))
   

      call ordina_type_as_ch(unit_avail_as_o,type_chr,   
     *                       first_unit(i_vert(1)))

                 



      call ordina_type_c_as(unit_avail_as_o,type_chrt,
     *                       first_unit(i_vert(1)))


       call ordina_as(unit_avail_as_o,unit_vinc_maxpow,
     *                       first_unit(i_vert(1)))



       call ordina_as(unit_avail_as_o,unit_vinc_minpow,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,unit_eff_corr,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,unit_power_corr,
     *                       first_unit(i_vert(1)))

      call ordina_as(unit_avail_as_o,unit_hrate_corr,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,unit_vinc_maxrev,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,unit_rev_rat,
     *                       first_unit(i_vert(1)))

c	call ordina_as(unit_avail_as,unit_delprint)

c	call ordina_as(unit_avail_as,unit_deltrint)

c	call ordina_as(unit_avail_as,unit_maxtint)

      call ordina_as(unit_avail_as_o,min_rev,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,max_rev,
     *                       first_unit(i_vert(1)))

	call ordina_as(unit_avail_as_o,min_eff,
     *                       first_unit(i_vert(1)))

      



cmar_ass_cond_temp
      do i=1,10
	stat_vars_2(i)=stat_vars(i,i_vert(2))
      enddo
cmar_ass_cond_temp

CCC      CHIUDO IL CICLO SUI CIN
      ENDIF
CCC      CHIUDO IL CICLO SUI CIN



      ELSE

            do iserv = 1,nserv
        flow2_mf(iserv)      = flow_stat(i_vert(iserv))
        pres1_mf(iserv)      = pasp_stat(i_vert(iserv))
cgpe        pres1_mf(iserv)      = pasp_stat_dp(i_vert(iserv))
        pres2_mf(iserv)      = pmand_stat(i_vert(iserv))
cgpe        pres2_mf(iserv)      = pmand_stat_dp(i_vert(iserv))
        delpr1_mf(iserv)     = delpr1(i_vert(iserv))
        delpr2_mf(iserv)     = delpr2(i_vert(iserv))
cgpe-end
        temp1_mf(iserv)      = tasp_stat(i_vert(iserv))
        air_mf(iserv)        = taria(i_vert(iserv))
        stat_ord_mf(iserv)   = stat_ord(i_vert(iserv))
        first_u_mf(iserv)    = first_unit(i_vert(iserv))
        first_t_mf(iserv)    = first_type(i_vert(iserv))
        vert_type_mf(iserv)  = vert_typ(i_vert(iserv))
cgpe-new
        set_flag_mf(iserv)   = set_flag(i_vert(iserv))
cgpe-new-end
        istaz_mf(iserv)      = i_staz(iserv)
        vert_poi_mf(iserv)   = i_vert(iserv)

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_quant_mf(i,iserv) = type_quant(it)
        end do

        i1 = 0
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            unit_avail_mf(k+i1,iserv) = unit_avail(ii1)
          end do
          iu=iu+type_quant(j)
          i1=i1+type_quant(j)
        end do

        do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1
          type_avail_mf(i,iserv) = type_avail(it)
        end do

        iu = first_unit(i_vert(iserv))
        do i = 1,unit_num(i_vert(iserv))
          status_mf(i,iserv) = status(iu+i-1)
          start_cost_mf(i,iserv) = unit_start_cost(iu+i-1)
          shut_cost_mf(i,iserv)  = unit_shut_cost(iu+i-1)
        end do

        do k = 1,10
	     stat_vars_mf(k,iserv)= stat_vars(k,i_vert(iserv))
        end do
      end do
c
           

  
	ENDIF


      sim (I_STAZ(1))=.true.
     


     
      return
      end
cmar_ass_cond++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cmar_ass_cond******************************************************************


	Subroutine ripristina_dati_stazione_as(nserv,
     *     first_u_mf,first_t_mf,istaz_mf,vert_poi_mf,
     *     type_quant_mf,type_avail_mf,unit_avail_mf,vert_type_mf,
cgpe     *     flow2_mf,pres1_mf,pres2_mf,temp1_mf,
cgpe-new
     *     set_flag_mf,
cgpe-new-end
     *     flow2_mf,pres1_mf,pres2_mf,delpr1_mf,delpr2_mf,temp1_mf,
     *     air_mf,stat_ord_mf,status_mf,stat_vars_mf,
     *     start_cost_mf,shut_cost_mf)
c
c  Genera tutte le condizioni di input per l'ottimizzazione della
c  centrale multifunzione, assegnando alle variabili con indice 1
c  i valori delle variabili del servizio con il numero minore di
c  unita'.Aggiorna i dati di centrale (stat_vars_mf)
c-------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/cmf_assetti.inc'
	include '../inc/ass_cond.inc'
    
ccccccc   da verificare la dimensione       ccccccccc

      INTEGER*4 ordine(max_type)
cmar_ass_cond    scarico
c      COMMON/ASS_COND1/ordine
cmar_ass_cond
c
      INTEGER*4 nserv
      INTEGER*4 first_u_mf(*),first_t_mf(*),
     *          istaz_mf(*),vert_poi_mf(*)
      INTEGER*4 type_quant_mf(maxunits,max_serv),
     *          type_avail_mf(maxunits,max_serv),
     *          unit_avail_mf(maxunits,max_serv)
      integer*4 vert_type_mf(*),set_flag_mf(*)
      REAL*4  flow2_mf(*),pres1_mf(*),pres2_mf(*),
cgpe
     *        delpr1_mf(*),delpr2_mf(*),
cgpe-end
     *        temp1_mf(*),air_mf(*)
      INTEGER*4 stat_ord_mf(*)
      CHARACTER*3  status_mf(maxunits,max_serv)

      REAL*4  stat_vars_mf(12,max_serv)
      REAL*4  start_cost_mf(maxunits,max_serv)
     *       ,shut_cost_mf(maxunits,max_serv)

      INTEGER*4 i1,ii1,i,iu,it,i_a
      INTEGER*4 iserv

      integer*4 J,K

cmar_ass_cond

	integer*4 ii, jj, kk, primo, ll, lll, iii, iv,
      

	       
     *          ordine1(maxunits,max_serv), ordine_(maxunits)

cmar_ass_cond

c      INTEGER*4 ordine(maxunits)     !OUTPUT: ordine(posizione)=indice tbg



c-------------------------------------------------------------------------
     


cmar_ass_cond: restore del bck  
      do iserv = 1,nserv
	  do i = 1,type_num(i_vert(iserv))
          it = first_type(i_vert(iserv))+i-1

          
	type_quant(it)=type_quant_as(it)
	type_quant_mf(i,iserv)=type_quant_mf_as(i,iserv)

        end do
   

        i1 = 0
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            


	unit_avail(ii1)=unit_avail_as(ii1)
	unit_avail_mf(k+i1,iserv)=unit_avail_mf_as(k+i1,iserv)


       enddo
	enddo

      enddo
    

     
      return
      end
cmar_ass_cond+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cmar++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      Subroutine calc_avail_unita(nserv,unit_avail_old,ndisp,linv)

      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/stations.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/cmf_assetti.inc'
	include '../inc/ass_cond.inc'
c
      INTEGER*4 nserv
      INTEGER*4 unit_avail_old(*)
      INTEGER*4 ndisp(*)
      LOGICAL*2 linv

      integer*4 J,ISERV,IU,IT,I1,JU,I,K,II1,JJ1
c-------------------------------------------------------------------------
cgpe      do iserv = 1,nserv
cgpe        iu = first_unit(i_vert(iserv))
cgpe        it = first_type(i_vert(iserv))
cgpe        i1=0
cgpe        if (iserv.eq.2) then
cgpe          ju = first_unit(i_vert(iserv-1))
cgpe        end if
cgpe        do i=1,type_num(i_vert(iserv))
cgpe          j=it+i-1
cgpe          do k=1,type_quant(j)
cgpe            ii1=iu+k-1
cgpe            unit_avail(ii1) = 0
cgpe            if(.not.(unit_avail_old(ii1).eq.0))then
cgpe              jj1 = ii1
cgpe              if (iserv.eq.2) then
cgpe                jj1 = ju+ii1-first_unit(i_vert(iserv))
cgpe              end if
cgpe              if (unit_avail_old(ii1).eq.2) then
cgpe                unit_avail(ii1) = 2
cgpe                if (iserv.eq.2) then
cgpe                  unit_avail(jj1) = 0
cgpe                end if
cgpe              else if (unit_avail_old(jj1).eq.1) then
cgpe                unit_avail(ii1) = 1
cgpe              else
cgpe                unit_avail(ii1) = 0
cgpe              end if
cgpe            end if

cgpec se l'unita' i-esima e' disponibile ma necessariamente spenta per il servizio
cgpec iserv=2, deve risultare non disponibile anche per il servizio iserv=1
cgpe            if (iserv.eq.2) then
cgpe             if (unit_avail_old(ii1).eq.0) then
cgpe               jj1 = ju+ii1-first_unit(i_vert(iserv))
cgpe               unit_avail(jj1) = 0
cgpe             end if
cgpe            end if
cgpe          end do
cgpe          iu=iu+type_quant(j)
cgpe          i1=i1+type_quant(j)
cgpe        end do ! end ciclo su i
cgpe      end do   ! end ciclo su iserv
      ss_(1)=0
c determinazione di type_avail in base al valore di unit_avail per i
c servizi presenti
      do iserv=1,nserv
        iu = first_unit(i_vert(iserv))
        it = first_type(i_vert(iserv))
        ndisp(iserv) = 0
        do i=1,type_num(i_vert(iserv))
          j=it+i-1
          type_avail(j) = 0
          do k=1,type_quant(j)
            ii1=iu+k-1
            if (unit_avail(ii1).gt.0) then
              ndisp(iserv)  = ndisp(iserv) + 1
              type_avail(j) = type_avail(j) + 1
            end if
          end do
          iu = iu+type_quant(j)
        end do
      end do

cmar_ass_cond
cmar      do i=121,121+24

      do i =first_unit(i_vert(1)),first_unit(i_vert(1))+23
      if(type_avail(i).ne.0)then
	ss_(1)=ss_(1)+type_avail(i)
	endif
	enddo

cmar_ass_cond


c se il numero di unita' disponibili per il servizio iserv=2 e' minore del
c numero di unita' disponibili per il servizio iserv=1, vengono scambiate
c le condizioni di input mentre non dovrebbe essere necessario scambiare
c la disponibilita' delle unita' e dei tipi (non condividono la stessa
c area di memoria)

cmar_ass_cond
              if(1) then
      linv = .false.
      if ((nserv.gt.1).and.(ndisp(2).gt.ndisp(1))) then
        linv = .false.
      end if
	        endif
c
cmar_ass_cond

c      linv = .false.
c      if ((nserv.gt.1).and.(ndisp(2).lt.ndisp(1))) then
c        linv = .true.
c      end if
c
      return
      end