
c.......................................................................
c  pacchetto contenente le routines per la simulazione di una stazione
c  semplice (estensione alle stazioni elementari introdotte dalla
c  multifunzione)
c.......................................................................
       Subroutine optim_vertex(istaz,iopt,vert_type,tipo_criterio,
     *              vert_pointer,flow1,flow2,pres1,pres2,
     *              temp1,temp2,air,delpr1,delpr2,
     *              comp_ratio,tot_cons,flowm,flow_ric,
     *              head,unit_num,stat_ord,jtfir,type_num,jufir,
     *              stat_vars,pres_int,temp_int,
     *              delprint,deltrint,ier)
c***********************************************************************
c	simula un vertice in stazionario
c-----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/flag.INC'
	include '../inc/ASS_COND.INC'
c
c	istaz        I  indice della stazione nella struttura dati completa
c                       (common stazione.inc, variabile ind)
c	vert_type    I	tipo di stazione (1=semplice, 2=composta)
c	vert_pointer I	indice della stazione nella struttura interna di
c                       dati di stazione
cgpec	set_flag     I	flag di stato (=0 stazione spenta, 
cgpec                                      =1 stazione accesa)
c	flow1        I/O portata entrante in stazione
c	flow2        I	portata uscente (lavorata dalla stazione)
c	pres1        I	pressione di aspirazione
c	pres2        I	pressione di mandata
c	temp1        I	temperatura di aspirazione
c	temp2        O	temperaura di mandata
c	air          I	temperatura dell'aria
c	delpr1       I	perdita di pressione effettiva in ingresso
c	delpr2       I	perdita di pressione effettiva in uscita
c	comp_ratio   O	rapporto di compressione effettivo
c	tot_cons     I/O consumo della stazione
c	flowm        O  portata di macchina totale della stazione
c	flow_ric     O  portata normal condition totale della stazione
c                       (con eventuale riciclo)
c	head         O  adiabatic head della stazione
c	unit_num     I	numero totale di macchine della stazione
c       stat_ord     I  tipo di funzionamento della stazione, se di tipo
c                       composto
c	jtfir        I	puntatore al primo tipo della stazione
c	type_num     I	numero di tipi della stazione
c	jufir        I	puntatore alla prima unita' della stazione
c	stat_vars    I	variabili topologiche di stazione
c	nit          I	numero di iterazione (per i messaggi)
c	nitmax       I	numero di iterazione massimo richiesto dall'esterno
c	tipo_corr    I  tipo di correzione da apportare nei casi estremi
c                       (se il set e' in pressione e la correzione e' 
c                        in portata, oppure se il set e' in portata e la
c                        correzione e' in pressione)
c                        1=correzione
c                        0=nessuna correzione
c	ier          O	error index
c       iopt         I  index optimization (iopt=T optimization), parametro
c                       necessario per la gestione degli errori
c       status       I  lo stato delle unita' viene letto nel common units.inc
c
CMAR
      COMMON/pre/ppres2
cmarmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

	 COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
	common/equi_k/k_min(max_unit), k_max(max_unit)

cmar
cmar
      real*4 perc_equi_min, perc_equi_max
	real*4 perc_equi_min_(max_unit), perc_equi_max_(max_unit)



		COMMON/equi_flag/flag_equi_s(max_unit)
cmar-equi-05-08-09

		COMMON/equi_cho/kmax_cho(max_unit) 

	COMMON/equi_sur/kmin_sur(max_unit) 

	real*4 kmin_sur
	real*4 kmin_sur_(max_unit)


	real*4 kmax_cho
	real*4 kmax_cho_(max_unit)


cmar-equi-05-08-09
 
      logical*2 flag_equi_s
      logical*2 flag_equi_s_(max_unit)

	real*4 k_min, k_max
	integer*4 mm, jj, m
cmarmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm

      real*4 ppres2
      INTEGER*4  istaz,vert_type,vert_pointer
      INTEGER*4  tipo_criterio
      real*4   pres1,pres2,temp1,temp2,air,delpr1,delpr2,
     *         comp_ratio,tot_cons,flowm,flow_ric,head,pres_int,
     *         temp_int,delprint,deltrint
      INTEGER*4  unit_num,stat_ord,jtfir,type_num,jufir
c
      INTEGER*4  i1,iu,i,j,k,ii1
      INTEGER*4 ntipi_act
c
      real*4     stat_vars(*)
      INTEGER*4  ier
c
!      INTEGER*4  iset,istat
      INTEGER*4  iw
      character*(max_len) mess/' '/
      logical*2  iopt
      INTEGER*4  vert_type_old
      INTEGER*4  num_mono,num_bifa,nunit_act,max_unitact

c-----------------------------------------------------------------------
CMAR  
      ppres2=pres2       
	ier = 0
      nit = 0
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
cmar

      do jj= 1, unit_num
      perc_equi_min_(jj)=perc_equi_min(jj)
	perc_equi_max_(jj)=perc_equi_max(jj)
      flag_equi_s_(jj)=flag_equi_s(jj)
	kmax_cho_(jj) = kmax_cho(jj) 
	kmin_sur_(jj) = kmin_sur(jj)


      enddo
      
cmar
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      vert_type_old = vert_type
cmar_EQ2
	call init_vert_EQ2(vert_pointer,type_act(jtfir),vert_type)

cmar_EQ2

c viene calcolato 'dinamicamicamente' il vert_type, ogni volta che si prova una
c determinata configurazione di turbogruppi
cmar_18 ottobre      
      call init_vert_type(vert_pointer,type_act(jtfir),vert_type)


      if (tipo_criterio.eq.crit_giri) then
        if (vert_type .eq. tipo_mist) then
          ier = 1 ! ier = 11
          vert_type = vert_type_old
          return
cgpe-corr	  elseif (vert_type.eq.tipo_comp .and. stat_ord.eq.f_serie) then
	  elseif (vert_type.eq.tipo_comp) then
c controllo che non ci siano unità attive di tipo diverso in base al valore di type_act
	    ntipi_act = 0
	    nunit_act = 0
          do i=1,type_num
	      if (type_act(jtfir+i-1).gt.0) then
              ntipi_act = ntipi_act + 1
              nunit_act = nunit_act + type_act(jtfir+i-1)
	      endif
          end do
          if (ntipi_act.gt.1) then
            ier = 1 ! ier = 11
            vert_type = vert_type_old
            return
          end if
cgpe-corr
c controllo che per la limitazione dovuta alle aree common e al fatto che ntp= 2*type_act
c nella init_station e init_multi_stadio si possono attivare al più 6 turbogruppi
c contemporaneamente
          max_unitact = maxunits/2.
          if (nunit_act .gt. max_unitact) then
            ier = 1 ! ier = 11
            vert_type = vert_type_old
            return
          endif
cgpe-corr-end
        end if
      else
        if (vert_type .eq. tipo_mist .and.
     *      stat_ord .eq. f_serie         ) then
          ier = 1 ! ier = 11
          vert_type = vert_type_old
          return
        end if
      end if
c
cgpe  La gestione di una centrale spenta viene fatta a monte, non viene chiamato
cgpe l'algoritmo di ottimizzazione
cgpe      if (set_flag.eq.0) then
cgpec---->  gestione stazione off
cgpe        flow1 = flow2
cgpe        pres2 = pres1
cgpe        temp2 = temp1
cgpe        comp_ratio = 1.
cgpe        tot_cons = 0.
cgpe        call spegni_staz(unit_num,jufir)

cgpe      else
c---->  gestione stazione on
        if(
     * ((vert_type.eq.tipo_semp).and.(tipo_criterio.eq.crit_giri)) .or.
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_giri).and.
     *                                (stat_ord.eq.f_paral)      )) then
c MODULO B: simulazione del criterio dei giri per centrali semplici e per
c           centrali composte in parallelo (1 tipo di unita').
	    call single_station(istaz,vert_pointer,vert_type,
     *         tipo_criterio,flow2,
     *         pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,
     *         flow_ric,type_num,unit_num,jufir,jtfir,
     *	     stat_vars,stat_ord,ier)
       elseif(
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_giri).and.
     *                                (stat_ord.eq.f_serie)   )  ) then
c MODULO E: simulazione del criterio dei giri per centrali composte 
c           (1 tipo di unita') in serie
          write(LGU,*)'CHIAMATA comp_Station.....'
          call comp_Station(istaz,vert_pointer,tipo_criterio,
     *         stat_ord,flow2,pres1,pres2,temp1,temp2,
     *         air,comp_ratio,tot_cons,
     *         delpr1,delpr2,pres_int,temp_int,delprint,deltrint,
     *         flowm,flow_ric,type_num,unit_num,jufir,jtfir,
     *         stat_vars,ier)
       elseif(
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_equi).and.
     *                                (stat_ord.eq.f_serie)   )  ) then
c MODULO F: simulazione del criterio di Equidistanza per centrali composte 
c           in serie
          write(LGU,*)'CHIAMATA comp_Station_mf.....'
          write(LGU,*)'INPUT istaz:',istaz
          write(LGU,*)'INPUT vert_pointer:',vert_pointer
          write(LGU,*)'INPUT tipo_criterio:',tipo_criterio
          write(LGU,*)'INPUT flow2:',flow2
          write(LGU,*)'INPUT pres1:',pres1
          write(LGU,*)'INPUT pres2:',pres2
          write(LGU,*)'INPUT temp1:',temp1
          write(LGU,*)'INPUT temp2:',temp2
          write(LGU,*)'INPUT air:',air
          write(LGU,*)'INPUT comp_ratio:',comp_ratio
          write(LGU,*)'INPUT tot_cons:',tot_cons
          write(LGU,*)'INPUT delpr1:',delpr1
          write(LGU,*)'INPUT delpr2:',delpr2
c         write(LGU,*)'INPUT unit_delprint:',(unit_delprint(i), i=1,6)
          write(LGU,*)'INPUT flowm:',flowm
          write(LGU,*)'INPUT flow_ric:',flow_ric
          write(LGU,*)'INPUT type_num:',type_num
          write(LGU,*)'INPUT unit_num:',unit_num
          write(LGU,*)'INPUT jufir:',jufir
          write(LGU,*)'INPUT jtfir:',jtfir
c        write(LGU,*)'INPUT stat_vars:',stat_vars
          write(LGU,*)'INPUT stat_ord:',stat_ord
          call comp_Station_mf(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,tot_cons,
     *         delpr1,delpr2,flowm,flow_ric,type_num,unit_num,jufir,
     *         jtfir,stat_vars,stat_ord,ier)
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      do jj= 1, unit_num
      perc_equi_min(jj)=perc_equi_min_(jj)
	perc_equi_max(jj)=perc_equi_max_(jj)
	flag_equi_s(jj)=flag_equi_s_(jj)
	kmax_cho(jj) = kmax_cho_(jj) 
	kmin_sur(jj) = kmin_sur_(jj)
      enddo
      
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
     

      else if(
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_port).and.
     *                                (stat_ord.eq.f_serie)      )) then
c MODULO D: simulazione del criterio di Uguaglianza portate per 
c           centrali composte in serie
          write(LGU,*)'CHIAMATA comp_Station_UP'
          call comp_Station_UP(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,
     *         comp_ratio,tot_cons,delpr1,delpr2,flowm,
     *         flow_ric,type_num,unit_num,jufir,jtfir,
     *         stat_vars,stat_ord,ier)
	  else if(
     * ((vert_type.eq.tipo_semp).and.(tipo_criterio.eq.crit_port)).or.
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_port) .and.
     *                               (stat_ord.eq.f_paral)       ).or.
     * ((vert_type.eq.tipo_mist).and.(tipo_criterio.eq.crit_port))) then
c MODULO A: simulazione del criterio di Uguaglianza portate per centrali
c           miste
	    call single_station_UP(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	     type_num,unit_num,jufir,jtfir,stat_vars,
     *         stat_ord,ier)
 	  else if(
     * ((vert_type.eq.tipo_semp).and.(tipo_criterio.eq.crit_equi)).or.
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_equi) .and.
     *                               (stat_ord.eq.f_paral)       ).or.
     * ((vert_type.eq.tipo_mist).and.(tipo_criterio.eq.crit_equi))) then
c MODULO C: simulazione del criterio di Equidistanza per centrali miste
cmar_ass_cond      if(flag_ass_cond(istaz).OR.flag_ass_cond(istaz+1))then
      if(flag_ass_cond(istaz))then
      as1=istaz
	as2=istaz+1
cmar_as_cond
           call single_station_mf_as(istaz,vert_pointer,tipo_criterio,
cmar_as_cond      call single_station_mf(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	     type_num,unit_num,jufir,jtfir,stat_vars,
     *         stat_ord,ier)
	else
	    call single_station_mf(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	     type_num,unit_num,jufir,jtfir,stat_vars,
     *         stat_ord,ier)
      endif
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      do jj= 1, unit_num
      perc_equi_min(jj)=perc_equi_min_(jj)
	perc_equi_max(jj)=perc_equi_max_(jj)
	flag_equi_s(jj)=flag_equi_s_(jj)
	kmax_cho(jj) = kmax_cho_(jj) 
	kmin_sur(jj) = kmin_sur_(jj)
      enddo
      
cmarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
      elseif(
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_man).and.
     *  (stat_ord.eq.f_serie)      )                             ) then
c MODULO G: simulazione della ripartizione manuale della portata per 
c           centrali composte con compressori bifase in serie
          write(LGU,*)'CHIAMATA comp_Station_man'
          call comp_Station_man(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,
     *         comp_ratio,tot_cons,delpr1,delpr2,flowm,
     *         flow_ric,type_num,unit_num,jufir,jtfir,
     *         stat_vars,stat_ord,ier)
	  elseif(
     * ((vert_type.eq.tipo_semp).and.(tipo_criterio.eq.crit_man)).or.
     * ((vert_type.eq.tipo_comp).and.(tipo_criterio.eq.crit_man) .and.
     *                               (stat_ord.eq.f_paral)       ).or.
     * ((vert_type.eq.tipo_mist).and.(tipo_criterio.eq.crit_man))) then
c MODULO H: simulazione della ripartizione manuale della portata per
c           centrali miste (con compressori bifase in parallelo)

cmar_crit_man

      if(flag_ass_cond(istaz))then
      as1=istaz
	as2=istaz+1
cmar_as_cond
           call single_station_man_as(istaz,vert_pointer,tipo_criterio,
cmar_as_cond      call single_station_mf(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	     type_num,unit_num,jufir,jtfir,stat_vars,
     *         stat_ord,ier)
	else
	    call single_station_man(istaz,vert_pointer,tipo_criterio,
     *         flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *         tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	     type_num,unit_num,jufir,jtfir,stat_vars,
     *         stat_ord,ier)
	endif
cmar_crit_man

        else
!!!!!!!! MODULO DI CALCOLO NON PREVISTO !!!!!!!!
cgpe-new
          ier = -1111
cgpe-new          if (fl_sim) then
cgpe-new            call gest_error (1,0,'OPTIM_VERTEX',
cgpe-new     *          'Modulo di calcolo non previsto',0)
cgpe-new          endif


  	  end if
        if (iopt) return
        flow1 = flow2+tot_cons
!        if (ier.gt.0 .and. nit.eq.nitmax) then
!c segnalo che e' stato raggiunto il massimo numero di iterazioni richiesto
!          iw=33
!          mess = ' '
!          call app_mess(vert_pointer,iw,mess)
!        endif
cgpe      endif
c


	return
	end

c-----------------------------------------**********************************

cmar_ass_cond
      Subroutine Single_Station_man_as(istaz,vert_pointer,tipo_criterio,
     *                  flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *                  tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	                type_num,unit_num,jufir,jtfir,stat_vars,
     *                  stat_ord,ier)
c
c***********************************************************************
c simulazione di una stazione elementare di tipo misto con criterio
c di isorendimento
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/mf_UNITS.INC'






cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess



cgpe      include '../inc/cmf.INC'
c
c  ind  : indice della stazione nella struttura dati SIRE del
c         common stazione.inc
 
      INTEGER*4  istaz  ! I indice della stazione nella struttura dati
      INTEGER*4  tipo_criterio
      INTEGER*4  vert_pointer
      INTEGER*4  stat_ord
      real*4     flow2, ! I flusso di uscita
     *           pres1,pres2,  ! pressioni di ingresso e uscita
     *           temp1,temp2,  ! 
     *           air,comp_ratio,tot_cons,
     *           head,
     *           delpr1,delpr2,
c_ecol
cgpe     *           delpr_f,delpr_ac,
c_ecol
     *           flowm,flow_ric
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_vars(*)
      INTEGER*4  ier
c
      real*4     mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4     mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2  lstat(maxunits)

	real*4 a
CMAR_AC_AUTO
CMAR:  SOLO PER LA MULTIFNE AD ASSETTI CONDIVISI:

      REAL*4 DP_AC_1_M,DP_AC_2_M

CMAR_AC_AUTO


      real*4      dp1_M,dp2_M

	

cmar_ac_gestione mess
	integer*4 i
cmar_ac_gestione mess


	
c
cvar      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
cvar     *          VALSJC_OLD
cvar      integer*4 I, J
cvar      character*3 status_old(maxunits)
cvar      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD,
cvar     *          unit_eff_corr_old(maxunits),
cvar     *          unit_power_corr_old(maxunits)
cvar      integer*4 type_quant_old(maxunits)
cvar      real*4    pres_int,temp_int,delprint,delt_int
c-----------------------------------------------------------------------
C---->  salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
csave10    continue
c---->              assign zero all the station variable





      call init_simpstat_mf(istaz,vert_pointer,tipo_criterio,pres1,
     *          pres2,air,tot_cons,
cgpe     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,nit,mirev,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,mirev,
     *          marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars(1))



CMAR_AC
C       QUI CAMBIO IL MODELLO, SIMULO E VEDO SE è IL CASO DI ACCENDERE L'AC PER RAFFREDDARE IL GAS

CMAR     IF1


      IF( ac_ce_stat.eq.1)	THEN



cmar_1809
      type_actr_M=type_actr
cmar_1809
	ac_ce_k_M=ac_ce_k
	ac1_k_M=ac1_k
cmar        a=stat_vars(5)

	ac_ce_stat_M=ac_ce_stat

	ac1_stat_M=ac1_stat


CMAR   SPENGO L'AC
      ac_ce_k=0
      ac1_k=0
cmar      stat_vars(5)=0

      ac_ce_stat=0

	ac1_stat=0

	
      FLAG_AC=.TRUE.

	

cmar_ac_02_10_13
       dp1_M=delpr1
       
	 dp2_M=delpr2

cmar_ac_02_10_13


c---->                                       simulazione stazione
ccccccc 	call Station_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
      call Station_man_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)
	




CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARS(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  
cmar      stat_vars(5)=a

      ac_ce_stat=ac_ce_stat_M

	ac1_stat=ac1_stat_M


	FLAG_AC=.FALSE.

cmar_1809
      type_actr=type_actr_M
cmar_1809





cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_13







cmar_ac_gestione mess
c      mess=' '

      do i=1,mess_num(vert_pointer)
      mess_stri(i,vert_pointer)=''
	mess_type(i,vert_pointer)=0

	enddo


cmar_ac_gestione mess


	  call Station_man_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)
	


      ENDIF


	

CMAR     IF1
       ELSE
CMAR     IF1


	  call Station_man_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)



     

CMAR     IF1
      ENDIF
CMAR     IF1


!  
      return
      end
cmar_ass_cond



cmar_ass_cond
      Subroutine Single_Station_mf_as(istaz,vert_pointer,tipo_criterio,
     *                  flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *                  tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	                type_num,unit_num,jufir,jtfir,stat_vars,
     *                  stat_ord,ier)
c
c***********************************************************************
c simulazione di una stazione elementare di tipo misto con criterio
c di isorendimento
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/mf_UNITS.INC'






cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess



cgpe      include '../inc/cmf.INC'
c
c  ind  : indice della stazione nella struttura dati SIRE del
c         common stazione.inc
 
      INTEGER*4  istaz  ! I indice della stazione nella struttura dati
      INTEGER*4  tipo_criterio
      INTEGER*4  vert_pointer
      INTEGER*4  stat_ord
      real*4     flow2, ! I flusso di uscita
     *           pres1,pres2,  ! pressioni di ingresso e uscita
     *           temp1,temp2,  ! 
     *           air,comp_ratio,tot_cons,
     *           head,
     *           delpr1,delpr2,
c_ecol
cgpe     *           delpr_f,delpr_ac,
c_ecol
     *           flowm,flow_ric
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_vars(*)
      INTEGER*4  ier
c
      real*4     mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4     mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2  lstat(maxunits)

	real*4 a
CMAR_AC_AUTO
CMAR:  SOLO PER LA MULTIFNE AD ASSETTI CONDIVISI:

      REAL*4 DP_AC_1_M,DP_AC_2_M

CMAR_AC_AUTO


      real*4      dp1_M,dp2_M

	

cmar_ac_gestione mess
	integer*4 i
cmar_ac_gestione mess


	
c
cvar      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
cvar     *          VALSJC_OLD
cvar      integer*4 I, J
cvar      character*3 status_old(maxunits)
cvar      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD,
cvar     *          unit_eff_corr_old(maxunits),
cvar     *          unit_power_corr_old(maxunits)
cvar      integer*4 type_quant_old(maxunits)
cvar      real*4    pres_int,temp_int,delprint,delt_int
c-----------------------------------------------------------------------
C---->  salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
csave10    continue
c---->              assign zero all the station variable





      call init_simpstat_mf(istaz,vert_pointer,tipo_criterio,pres1,
     *          pres2,air,tot_cons,
cgpe     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,nit,mirev,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,mirev,
     *          marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars(1))



CMAR_AC
C       QUI CAMBIO IL MODELLO, SIMULO E VEDO SE è IL CASO DI ACCENDERE L'AC PER RAFFREDDARE IL GAS

CMAR     IF1


      IF( ac_ce_stat.eq.1)	THEN



cmar_1809
      type_actr_M=type_actr
cmar_1809
	ac_ce_k_M=ac_ce_k
	ac1_k_M=ac1_k
cmar        a=stat_vars(5)

	ac_ce_stat_M=ac_ce_stat

	ac1_stat_M=ac1_stat


CMAR   SPENGO L'AC
      ac_ce_k=0
      ac1_k=0
cmar      stat_vars(5)=0

      ac_ce_stat=0

	ac1_stat=0

	
      FLAG_AC=.TRUE.

	

cmar_ac_02_10_13
       dp1_M=delpr1
       
	 dp2_M=delpr2

cmar_ac_02_10_13


c---->                                       simulazione stazione
ccccccc 	call Station_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
      call Station_mf_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)
	




CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARS(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  
cmar      stat_vars(5)=a

      ac_ce_stat=ac_ce_stat_M

	ac1_stat=ac1_stat_M


	FLAG_AC=.FALSE.

cmar_1809
      type_actr=type_actr_M
cmar_1809





cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_13







cmar_ac_gestione mess
c      mess=' '

      do i=1,mess_num(vert_pointer)
      mess_stri(i,vert_pointer)=''
	mess_type(i,vert_pointer)=0

	enddo


cmar_ac_gestione mess


	  call Station_mf_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)
	


      ENDIF


	

CMAR     IF1
       ELSE
CMAR     IF1


	  call Station_mf_as(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)



     

CMAR     IF1
      ENDIF
CMAR     IF1


!  
      return
      end
cmar_ass_cond


	Subroutine Single_Station_mf(istaz,vert_pointer,tipo_criterio,
     *                  flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *                  tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *	                type_num,unit_num,jufir,jtfir,stat_vars,
     *                  stat_ord,ier)
c
c***********************************************************************
c simulazione di una stazione elementare di tipo misto con criterio
c di isorendimento
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/mf_UNITS.INC'
cgpe      include '../inc/cmf.INC'

cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess

c
c  ind  : indice della stazione nella struttura dati SIRE del
c         common stazione.inc
 
      INTEGER*4  istaz  ! I indice della stazione nella struttura dati
      INTEGER*4  tipo_criterio
      INTEGER*4  vert_pointer
      INTEGER*4  stat_ord
      real*4     flow2, ! I flusso di uscita
     *           pres1,pres2,  ! pressioni di ingresso e uscita
     *           temp1,temp2,  ! 
     *           air,comp_ratio,tot_cons,
     *           head,
     *           delpr1,delpr2,
c_ecol
cgpe     *           delpr_f,delpr_ac,
c_ecol
     *           flowm,flow_ric
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_vars(*)
      INTEGER*4  ier
c
      real*4     mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4     mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2  lstat(maxunits)

	
	real*4 a

	real*4      dp1_M,dp2_M

cmar_ac_gestione mess
	integer*4 i
cmar_ac_gestione mess

cmar_ac_gestione mess
c	character*(200) mess

cmar_ac_gestione mess

c
cvar      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
cvar     *          VALSJC_OLD
cvar      integer*4 I, J
cvar      character*3 status_old(maxunits)
cvar      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD,
cvar     *          unit_eff_corr_old(maxunits),
cvar     *          unit_power_corr_old(maxunits)
cvar      integer*4 type_quant_old(maxunits)
cvar      real*4    pres_int,temp_int,delprint,delt_int
c-----------------------------------------------------------------------
C---->  salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
csave10    continue
c---->              assign zero all the station variable
      call init_simpstat_mf(istaz,vert_pointer,tipo_criterio,pres1,
     *          pres2,air,tot_cons,
cgpe     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,nit,mirev,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,mirev,
     *          marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars(1))


	


CMAR_AC
CMAR     IF1
      IF( ac_ce_stat.eq.1)	THEN

C       QUI CAMBIO IL MODELLO, SIMULO E VEDO SE è IL CASO DI ACCENDERE L'AC PER RAFFREDDARE IL GAS

cmar_1809
      type_actr_M=type_actr
cmar_1809
      
	ac_ce_k_M=ac_ce_k
	ac1_k_M=ac1_k
c        a=stat_vars(5)

	ac_ce_stat_M=ac_ce_stat

	ac1_stat_M=ac1_stat




c      ac2_stat_M=ac2_stat
c	ac2_k_M=ac2_k

CMAR   SPENGO L'AC
      ac_ce_k=0
      ac1_k=0
c      stat_vars(5)=0

      ac_ce_stat=0


      ac1_stat=0

      FLAG_AC=.TRUE.

	
cmar_ac_02_10_13
       dp1_M=delpr1
       
	 dp2_M=delpr2

cmar_ac_02_10_13



c---->                                       simulazione stazione
      call Station_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,delpr1,
     *     delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)



	
CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARS(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  
c      stat_vars(5)=a


      



	ac_ce_stat=ac_ce_stat_M

      ac1_stat	= ac1_stat_M

	FLAG_AC=.FALSE.


cmar_1809
      type_actr=type_actr_M
cmar_1809





cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_13

cmar_ac_gestione mess
c      mess=' '

      do i=1,mess_num(vert_pointer)
      mess_stri(i,vert_pointer)=''
	mess_type(i,vert_pointer)=0

	enddo


cmar_ac_gestione mess

	 call Station_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,delpr1,
     *     delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)


       ENDIF


CMAR     IF1
       ELSE
CMAR     IF1

      call Station_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,delpr1,
     *     delpr2,
cgpe     *     delpr_f,delpr_ac,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
cgpe     *     unit_power_corr(jufir),unit_power(jufir),unit_hrate(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),unit_hrate(jufir),
     *     unit_cons(jufir),unit_temp(jufir),unit_max(jufir),
     *     unit_min(jufir),unit_vcrit(jufir),unit_vinc(jufir),
     *     stat_vars,unit2_perc(jufir),unit2_rev(jufir),
     *     unit2_flow(jufir),unit2_head(jufir),unit2_eff_corr(jufir),
     *     unit2_eff(jufir),unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),unit2_max(jufir),unit2_min(jufir),
     *     unit_bifase(jufir),IER)




CMAR     IF1
      ENDIF
CMAR     IF1

!      if (iopt) goto 11
cerr      if (iopt .or. iopt_mess) goto 11

cerrC---> gestione errore
cerr      if (ier .gt. 0) then
cerr        if (ier .le. 5 .or. ier .eq. 38) then
!- ier=1: non esistono punti fattibili sulle mappe dei compressori
!- ier=4: flow negativo
!- ier=38: pressione di ingresso o di uscita negativa
!- ier=5: pressione di ingresso superiore alla pressione di uscita
!- Non e' possibile portare il punto dentro le mappe, ma e' stato imposto
!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cerr          goto 11

cerr        else if (ier.eq.6 .or. ier.eq.7) then
!- ier=6: head supera l'altezza adiabatica massima
!- ier=7: head e' inferiore all'altezza adiabatica minima
cerr          if (nit .lt. nitmax) then
!- viene portato il punto dentro le mappe
!- le nuove condizioni di input alla station_mf sono rappresentate da
!- PRES1/ PRES2/ FLOW2
cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if

cerr        else if (ier.eq.8 .or. ier.eq.20) then
!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
!- (sono stati calcolati i dati di unita')
cerr          if (nit .lt. nitmax) then
!- se l'errore deriva  , non sono state modificate le condizioni iniziali
!            return
cerr            goto 11
cerr          else
!- nel caso in cui l'errore derivi dalla correzione del punto, sono state
!- modificate le condizioni iniziali
cerr            goto 11
cerr          end if

cerr        end if
cerr      end if
c
cerr11    continue
c
C--->  ripristino delle condizioni iniziali
cerr      call restore_cond_ini (istaz,vert_pointer,
cerr     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
cerr     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
cerr     *        ier,
cerr     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
cerr     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
cerr     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
      return
      end
cmar--------------------------------------------------------------
	Subroutine init_simpstat_mf(istaz,vert_pointer,tipo_criterio,
     *          pres1,pres2,air,tot_cons,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,
     *          mirev,marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars)
c
c***********************************************************************
c       inizializzazione del common simpstat
c-----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'

      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/mf_TYPES.INC'
      include '../inc/mf_UNITS.INC'
      include '../inc/mf_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
c_ecol
      include '../inc/filtri.INC'
      include '../inc/air_cooler.INC'
c_ecol
c
cc
      include '../inc/ASS_COND.INC'

      

      integer*4 istaz,vert_pointer
      integer*4 stat_ord
cgpe      integer*4 type_num,unit_num,jufir,nit,jtfir
      integer*4 type_num,unit_num,jufir,jtfir
c
      real*4    pres1,pres2,air,tot_cons,
cgpe     *          delpr1,delpr2,
     *          flowm,flow_ric
      real*4    mirev(*),marev(*),mieff(*)
      real*4    mirev2(*),marev2(*),mieff2(*)
      real*4    stat_vars(*)

      LOGICAL*2 lstat(*)

      integer*4 i,j,k,nu
c_ecol
     *         ,ij,ac_nu
c_ecol
      integer*4 tipo_criterio
      real*4    aa,bb,denom,faux
      real*4    haux,qaux,qauxv
      INTEGER*4 iu,ii1
c-----------------------------------------------------------------------




      IF(FLAG_ASS_COND(istaz) .and. FLAG_ASS_COND_S)THEN
      DP_F_1=C_F_CE_K(vert_pointer)
	DP_F_2=C_F_CE_K(vert_pointer+1)

	DP_AC_1=C_AC_CE_K(vert_pointer)
	DP_AC_2=C_AC_CE_K(vert_pointer+1)

	

cmar_ass_06_09
	ac_ce_stat_1=c_ac_ce_stat(vert_pointer)
	ac_ce_k_1=c_ac_ce_k(vert_pointer)
	ac_q_prog_1=c_ac_q_prog(vert_pointer)
	ac_taria_prog_1=c_ac_taria_prog(vert_pointer)
      
      ac_ce_stat_2=c_ac_ce_stat(vert_pointer+1)
	ac_ce_k_2=c_ac_ce_k(vert_pointer+1)
	ac_q_prog_2=c_ac_q_prog(vert_pointer+1)
	ac_taria_prog_2=c_ac_taria_prog(vert_pointer+1)
      
cmar_ass_06_09

	ENDIF
c---->        inizializzazione
      do i=1,unit_num
        j = jufir+i-1
        unit_flow(j) = 0.
        unit_head(j) = 0.
        unit_perc(j) = 0.
        unit_rev(j) = 0.
        unit_eff(j) = 0.
        unit_power(j) = 0.
        unit_hrate(j) = 0.
        unit_cons(j) = 0.
cgoe        unit_temp(j) = 0.
        unit_temp(j) = t0
        unit_max(j) = 0.
        unit_vinc(j) = 0
        unit_vcrit(j) = 0.
c---------------------------->filtri
	  f_tg_stat(i)=0.
	  f_tg_k(i)=0.
c---------------------------->ac1
        ac1_stat(i)=0.
        ac1_k(i)=0.
	  ac1_q_prog(i)=0.
	  ac1_taria_prog(i)=0.
        if (unit_bifase(j)) then
          unit2_flow(j) = 0.
          unit2_head(j) = 0.
          unit2_perc(j) = 0.
          unit2_rev(j) = 0.
          unit2_eff(j) = 0.
          unit2_power(j) = 0.
          unit2_temp(j) = 0.
          unit2_max(j) = 0.
          unit2_min(j) = 0.
c---------------------------->ac2
          ac2_stat(i)=0.
	    ac2_k(i)=0.
	    ac2_q_prog(i)=0.
	    ac2_taria_prog(i)=0.
        endif
      end do
      tot_cons = 0.
cgpe      delpr1   = 0.
cgpe      delpr2   = 0.
      flowm    = 0.
      flow_ric = 0.
cgpe      PRES1_RIC=PRES1
cgpe      PRES2_RIC=PRES2
c_ecol
      do i=1,maxunits
         type_ac_k_int(i)=0.
      end do
c_ecol
c---->               preparazione common stazione.inc
      ind    = istaz
      ver_poi = vert_pointer
      delta = stat_vars(1)
c---->                                  generazione common SIMPSTAT
      num_iter = nit
!      if (.not.iopt) then
        ntp  = type_num
!      end if
      tair = air
      tipo_crit = tipo_criterio
      unit_act = 0
c
c deve essere calcolato il numero di unita' necessariamente accese:
c viene fatto in base al valore della variabile  unit_avail(i)

        iu=jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            if (unit_avail(ii1).gt.1) then
              unit_act = unit_act + 1
            end if
          end do
          iu=iu+type_quant(j)
        end do
      nu = 0
      do i=1,ntp
        type_actr(i) = type_act(i+jtfir-1)
        j = nu+jufir
        eff_corr(i) = unit_eff_corr(j)
        power_corr(i) = unit_power_corr(j)
        hrate_corr(i) = unit_hrate_corr(j)
        type_vinc_maxpow(i)=unit_vinc_maxpow(j)
        type_vinc_minpow(i)=unit_vinc_minpow(j)
        type_flag_maxpow(i)=.false.
cgpe	  if (unit_vinc_maxrev(j) .lt. 1.) then
cgpe	    max_rev(j) = max_rev(j) * unit_vinc_maxrev(j)
c-mf03	  if (unit_vinc_maxrev(j).lt.max_rev(j).and.
c-mf03     *      unit_vinc_maxrev(j).gt.min_rev(j)     ) then
c-mf03          type_flag_maxrev(i) = .true.
c-mf03	    max_rev(j) = unit_vinc_maxrev(j)
c-mf03	  end if
        type_vinc_maxrev(i) = unit_vinc_maxrev(j)
c-mf03-end
        type_ratio(i) = unit_rev_rat(j)
        type_bifase(i) = unit_bifase(j)
        type_delpint(i) = unit_delprint(j)
        type_deltint(i) = unit_deltrint(j)
        type_maxtint(i) = unit_maxtint(j)
        mirev(i)  = min_rev(j) * revrif
        marev(i)  = max_rev(j) * revrif
        mieff(i)  = min_eff(j)
        type_min_rev(i)  = min_rev(j) * revrif
        type_max_rev(i)  = max_rev(j) * revrif
c-seconda fase
        eff_corr2(i)   = unit2_eff_corr(j)
        power_corr2(i) = unit2_power_corr(j)
        type2_ratio(i) = unit2_rev_rat(j)
        mirev2(i)      = min2_rev(j) * revrif
        marev2(i)      = max2_rev(j) * revrif
        mieff2(i)      = min2_eff(j)
        type2_min_rev(i)  = min2_rev(j) * revrif
        type2_max_rev(i)  = max2_rev(j) * revrif
c-seconda fase
        do k=1,max_lim
          a_coef(k,i) = type_a_coef(k,j)
          b_coef(k,i) = type_b_coef(k,j)
          lim_h(k,i) = type_lim_h(k,j)
          lim_n(k,i) = type_lim_n(k,j)
          lim_q(k,i) = type_lim_q(k,j)
c-seconda fase
          a_coef2(k,i) = type2_a_coef(k,j)
          b_coef2(k,i) = type2_b_coef(k,j)
          lim_h2(k,i)  = type2_lim_h(k,j)
          lim_n2(k,i)  = type2_lim_n(k,j)
          lim_q2(k,i)  = type2_lim_q(k,j)
        enddo
        clim(1,i) = type_c_coef(1,j)
        clim(2,i) = type_c_coef(2,j)
        clim(3,i) = type_c_coef(3,j)
        clim(4,i) = type_c_coef(4,j)
c-seconda fase
        clim2(1,i) = type2_c_coef(1,j)
        clim2(2,i) = type2_c_coef(2,j)
        clim2(3,i) = type2_c_coef(3,j)
        clim2(4,i) = type2_c_coef(4,j)
c
        do k=1,5
          chn(k,i) = type_chn(k,j)
          cen(k,i) = type_cen(k,j)
c-seconda fase
          chn2(k,i) = type2_chn(k,j)
          cen2(k,i) = type2_cen(k,j)
        end do
        chn(6,i) = type_chn(6,j)
c-seconda fase
        chn2(6,i) = type2_chn(6,j)
c attenzione !!! i chc sono uguali ai chn se non e' definita la zona di choking
        if (clim(3,i).ne.clim(4,i)) then
          do k=1,6
            chc(k,i) = type_chc(k,j)
          end do
        else
          do k=1,6
            chc(k,i) = chn(k,i)
          end do
        endif
c-seconda fase
        if (clim2(3,i).ne.clim2(4,i)) then
          do k=1,6
            chc2(k,i) = type2_chc(k,j)
          end do
        else
          do k=1,6
            chc2(k,i) = chn2(k,i)
          end do
        endif
c
        if (clim(1,i).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim(1,i)
          bb=1/clim(2,i)
          denom=aa-bb
          faux=cen(1,i)+cen(2,i)/aa+cen(3,i)/aa/aa
          cec(1,i)=(aa*faux-mieff(i)*bb)/denom
          cec(2,i)=(aa*cen(4,i))/denom
          cec(3,i)=(aa*cen(5,i))/denom
          cec(4,i)=(aa*bb*(mieff(i)-faux))/denom
          cec(5,i)=-(aa*bb*cen(4,i))/denom
          cec(6,i)=-(aa*bb*cen(5,i))/denom
        endif
c-seconda fase
        if (clim2(1,i).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim2(1,i)
          bb=1/clim2(2,i)
          denom=aa-bb
          faux=cen2(1,i)+cen2(2,i)/aa+cen2(3,i)/aa/aa
          cec2(1,i)=(aa*faux-mieff2(i)*bb)/denom
          cec2(2,i)=(aa*cen2(4,i))/denom
          cec2(3,i)=(aa*cen2(5,i))/denom
          cec2(4,i)=(aa*bb*(mieff2(i)-faux))/denom
          cec2(5,i)=-(aa*bb*cen2(4,i))/denom
          cec2(6,i)=-(aa*bb*cen2(5,i))/denom
        endif
c
        nlim(i)= type_nlim(j)
c-seconda fase
        nlim2(i)= type2_nlim(j)
c
        do k=1,3
          cpwm(k,i) = type_cpwm(k,j)
          cpwt(k,i) = type_cpwt(k,j)
          chr(k,i)  = type_chr(k,j)
          chrt(k,i) = type_chrt(k,j)
        end do
        cpwt(4,i) = type_cpwt(4,j)
        chrt(4,i) = type_chrt(4,j)
        chr(4,i) = type_chr(4,j)
        chr(5,i) = type_chr(5,j)
        chr(6,i) = type_chr(6,j)
c criterio
        if (tipo_criterio.eq.crit_equi) then
          kcrit_min(i) = unit_crit_min(j)
          kcrit_max(i) = unit_crit_max(j)
c-ele 26/7/06 Questo confronto e l'eventuale ridefinizione di unit_crit_max
c             viene gia eseguito in fase di validazione
c          if (clim(4,i).gt.unit_crit_max(j) ) then
c            type_kcrit_eff(2,i) = clim(4,i)
c-prec-???
c            kcrit_max(i) = type_kcrit_eff(2,i)
c-prec-???-end
c          else
c-ele
          type_kcrit_eff(2,i) = unit_crit_max(j)
c          endif
c- gz- prototipo
c per ora tratto la curva limite minimo del criterio come se fosse anche
c la curva di riciclo
          type_kcrit_eff(1,i) = unit_crit_min(j)
          clim_new(i) = type_kcrit_eff(1,i)
          if (type_bifase(i).and.(stat_ord.eq.f_paral)) then
            call hdan_su_mf(clim_new(i),haux,type_max_rev(i),chn(1,i))
            call qdahn(haux,qaux,type2_max_rev(i),chn2(1,i))
c verifica che il punto non sia oltre il vertice della parabola
            call qvert(chn2(1,i),qauxv,type2_max_rev(i))
            if (qaux.lt.qauxv) then
cgpe-new              call gest_error (2,0,'INIT_SIMPSTAT_MF',
cgpe-new     *             'SurgeII oltre curva teorica',0)

              qaux = qauxv
              call HDAQN(haux,qaux,type2_max_rev(i),chn2(1,i))
            endif
            haux = haux / headrif
            qaux = qaux / qmrif
            clim2_new(i) = haux/(qaux*qaux)
c-ele posso arrivare a questo livello solo se il criterio è l'equidistanza, quindi la 
c     condizione testata non può mai verificarsi; sposto il controllo ad un livello 
c     superiore in modo che risulti essere l'alternativa per il test su 'tipo_criterio'
c          else if (tipo_criterio.eq.crit_port) then
c            clim_new(i)=0.
c          endif
c-ele 26/7/06
           end if
        else if (tipo_criterio.eq.crit_port) then
            clim_new(i)=0.
        endif
c tipo di surge
c        if (tipo_criterio.eq.crit_equi) then
c	  tipo_surge = parabola
cgpe	else if (tipo_criterio.eq.crit_port) then
c	else
c	  tipo_surge = spezzata
c	end if
c
        nu = nu+type_quant(i+jtfir-1)
      end do
c
c-ele 26/7/06 'tipo_criterio' e 'tipo_surge' sono variabili definite a livello di
c             centrale e quindi il controllo sul loro valore è indipendente da i e j,
c             cioè da tipo e tbg;il controllo è stato quindi portato fuori dal ciclo do
c             per evitare che la stessa assegnazione sia fatta più volte
      if (tipo_criterio.eq.crit_equi) then
	  tipo_surge = parabola
	else
	  tipo_surge = spezzata
	end if
c-ele
c---->            set unit status as logical variable

     

      if (iopt) then

        do i=1,unit_num
        
          if(index(stat(i),funz_on).gt.0) then
       

            lstat(i) = .true.
          else
            lstat(i) = .false.
          end if
        end do
      else


	


        do i=1,unit_num
          j = jufir+i-1
          if(index(status(j),funz_on).gt.0) then
	 
            lstat(i) = .true.
          else
            lstat(i) = .false.
          end if
        end do
      end if
c_ecol
c_ecol --------------------------->Filtri
	f_ce_stat=c_f_ce_stat(vert_pointer)
	f_ce_k=c_f_ce_k(vert_pointer)

	do i=1,unit_num
	   j=jufir+i-1
	   if (lstat(i)) then
	      f_tg_stat(i)=c_f_tg_stat(j)
	      f_tg_k(i)=c_f_tg_k(j)
	   end if
	end do
c_ecol --------------------------->AirCooler
c------------>gestione air_cooler di centrale
	ac_ce_stat=c_ac_ce_stat(vert_pointer)
	ac_ce_k=c_ac_ce_k(vert_pointer)
	ac_q_prog=c_ac_q_prog(vert_pointer)
	ac_taria_prog=c_ac_taria_prog(vert_pointer)
c------------>gestione air_cooler di turbogruppo

	if(stat_ord.eq.f_serie) then    !bifase serie
	   do i=1,unit_num
	      j=jufir+i-1
	      if (lstat(i)) then
               ac1_stat(i)=c_ac1_stat(j)
 	         ac1_k(i)=c_ac1_k(j)
	         ac1_q_prog(i)=c_ac1_q_prog(j)
	         ac1_taria_prog(i)=c_ac1_taria_prog(j)
               ac2_stat(i)=c_ac2_stat(j)
	         ac2_k(i)=c_ac2_k(j)
	         ac2_q_prog(i)=c_ac2_q_prog(j)
	         ac2_taria_prog(i)=c_ac2_taria_prog(j)
	       end if
	   end do
c---------------------------------------------------------------------------------------
c  Definisco per ogni tipologia di turbogruppi un valore unico della costante
c  k caratteristica dell'air cooler di fase 1, in modo da poter calcolare la
c  caduta di pressione ad esso associata nelle routine di simulazione dove i
c  cicli sono eseguiti non sulle singole unità ma sui tipi.La costante è definita  
c  come media dei k delle unità appartenenti ad uno stesso tipo.Tale scelta 
c  è corretta sia quando unità dello stesso tipo hanno tutte lo stesso air cooler
c  sia nel caso più generale di unità appartenenti allo stesso tipo ma con cooler
c  diversi
c----------------------------------------------------------------------------------------
         nu=0
         do i=1,type_num
            j=jtfir+i-1
	      ac_nu=0
            do k=1,type_quant(j)
	         ij=nu+k
               if(lstat(ij)) then
                 type_ac_k_int(i)=type_ac_k_int(i)+ac1_k(ij)
	           ac_nu=ac_nu+1
               end if
            end do
	      if (ac_nu.gt.0) then
	         type_ac_k_int(i)=type_ac_k_int(i)/ac_nu
	      else
	         type_ac_k_int(i)=0.
	      end if
            nu=nu+type_quant(j)
         end do
      else                       !bifase parallelo
	   do i=1,unit_num
	      j=jufir+i-1
	      if (lstat(i)) then
               ac1_stat(i)=c_ac1_stat(j)
		     ac1_k(i)=c_ac1_k(j)
	         ac1_q_prog(i)=c_ac1_q_prog(j)
	         ac1_taria_prog(i)=c_ac1_taria_prog(j)
	         if (unit_bifase(j)) then
                  ac2_stat(i)=c_ac2_stat(j)
	            ac2_k(i)=c_ac2_k(j)
	            ac2_q_prog(i)=c_ac2_q_prog(j)
	            ac2_taria_prog(i)=c_ac2_taria_prog(j)
			 end if
	       end if
	    end do
      end if
c i bifase hanno una sola turbina
      una_turb = .true.
c

    

      return
      end

cmar ----------------------------------------------------------
	Subroutine init_simpstat_mf_MAR(istaz,vert_pointer,tipo_criterio,
     *          pres1,pres2,air,tot_cons,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,
     *          mirev,marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars)
c
c***********************************************************************
c       inizializzazione del common simpstat
c-----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'

      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/mf_TYPES.INC'
      include '../inc/mf_UNITS.INC'
      include '../inc/mf_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
c_ecol
      include '../inc/filtri.INC'
      include '../inc/air_cooler.INC'
c_ecol
c
      
	integer*4 istaz,vert_pointer
      integer*4 stat_ord
cgpe      integer*4 type_num,unit_num,jufir,nit,jtfir
      integer*4 type_num,unit_num,jufir,jtfir
c
      real*4    pres1,pres2,air,tot_cons,
cgpe     *          delpr1,delpr2,
     *          flowm,flow_ric
      real*4    mirev(*),marev(*),mieff(*)
      real*4    mirev2(*),marev2(*),mieff2(*)
      real*4    stat_vars(*)

      LOGICAL*2 lstat(*)

      integer*4 i,j,k,nu
c_ecol
     *         ,ij,ac_nu
c_ecol
      integer*4 tipo_criterio
      real*4    aa,bb,denom,faux
      real*4    haux,qaux,qauxv
      INTEGER*4 iu,ii1

cmar
      real*4 aa_m(2,2), aa_m_(2)
   
c-----------------------------------------------------------------------
c---->        inizializzazione
      do i=1,unit_num
        j = jufir+i-1
        unit_flow(j) = 0.
        unit_head(j) = 0.
        unit_perc(j) = 0.
        unit_rev(j) = 0.
        unit_eff(j) = 0.
        unit_power(j) = 0.
        unit_hrate(j) = 0.
        unit_cons(j) = 0.
cgoe        unit_temp(j) = 0.
        unit_temp(j) = t0
        unit_max(j) = 0.
        unit_vinc(j) = 0
        unit_vcrit(j) = 0.
c---------------------------->filtri
	  f_tg_stat(i)=0.
	  f_tg_k(i)=0.
c---------------------------->ac1
        ac1_stat(i)=0.
        ac1_k(i)=0.
	  ac1_q_prog(i)=0.
	  ac1_taria_prog(i)=0.
        if (unit_bifase(j)) then
          unit2_flow(j) = 0.
          unit2_head(j) = 0.
          unit2_perc(j) = 0.
          unit2_rev(j) = 0.
          unit2_eff(j) = 0.
          unit2_power(j) = 0.
          unit2_temp(j) = 0.
          unit2_max(j) = 0.
          unit2_min(j) = 0.
c---------------------------->ac2
          ac2_stat(i)=0.
	    ac2_k(i)=0.
	    ac2_q_prog(i)=0.
	    ac2_taria_prog(i)=0.
        endif
      end do
      tot_cons = 0.
cgpe      delpr1   = 0.
cgpe      delpr2   = 0.
      flowm    = 0.
      flow_ric = 0.
cgpe      PRES1_RIC=PRES1
cgpe      PRES2_RIC=PRES2
c_ecol
      do i=1,maxunits
         type_ac_k_int(i)=0.
      end do
c_ecol
c---->               preparazione common stazione.inc
      ind    = istaz
      ver_poi = vert_pointer
      delta = stat_vars(1)
c---->                                  generazione common SIMPSTAT
      num_iter = nit
!      if (.not.iopt) then
        ntp  = type_num
!      end if
      tair = air
      tipo_crit = tipo_criterio
      unit_act = 0
c
c deve essere calcolato il numero di unita' necessariamente accese:
c viene fatto in base al valore della variabile  unit_avail(i)

        iu=jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant(j)
            ii1=iu+k-1
            if (unit_avail(ii1).gt.1) then
              unit_act = unit_act + 1
            end if
          end do
          iu=iu+type_quant(j)
        end do
      nu = 0
      do i=1,ntp
        type_actr(i) = type_act(i+jtfir-1)
        j = nu+jufir
        eff_corr(i) = unit_eff_corr(j)
        power_corr(i) = unit_power_corr(j)
        hrate_corr(i) = unit_hrate_corr(j)
        type_vinc_maxpow(i)=unit_vinc_maxpow(j)
        type_vinc_minpow(i)=unit_vinc_minpow(j)
        type_flag_maxpow(i)=.false.
cgpe	  if (unit_vinc_maxrev(j) .lt. 1.) then
cgpe	    max_rev(j) = max_rev(j) * unit_vinc_maxrev(j)
c-mf03	  if (unit_vinc_maxrev(j).lt.max_rev(j).and.
c-mf03     *      unit_vinc_maxrev(j).gt.min_rev(j)     ) then
c-mf03          type_flag_maxrev(i) = .true.
c-mf03	    max_rev(j) = unit_vinc_maxrev(j)
c-mf03	  end if
        type_vinc_maxrev(i) = unit_vinc_maxrev(j)
c-mf03-end
        type_ratio(i) = unit_rev_rat(j)
        type_bifase(i) = unit_bifase(j)
        type_delpint(i) = unit_delprint(j)
        type_deltint(i) = unit_deltrint(j)
        type_maxtint(i) = unit_maxtint(j)
        mirev(i)  = min_rev(j) * revrif
        marev(i)  = max_rev(j) * revrif
        mieff(i)  = min_eff(j)
        type_min_rev(i)  = min_rev(j) * revrif
        type_max_rev(i)  = max_rev(j) * revrif
c-seconda fase
        eff_corr2(i)   = unit2_eff_corr(j)
        power_corr2(i) = unit2_power_corr(j)
        type2_ratio(i) = unit2_rev_rat(j)
        mirev2(i)      = min2_rev(j) * revrif
        marev2(i)      = max2_rev(j) * revrif
        mieff2(i)      = min2_eff(j)
        type2_min_rev(i)  = min2_rev(j) * revrif
        type2_max_rev(i)  = max2_rev(j) * revrif
c-seconda fase
        do k=1,max_lim
          a_coef(k,i) = type_a_coef(k,j)
          b_coef(k,i) = type_b_coef(k,j)
cmar

      aa_m(k,i)= a_coef(k,i) + b_coef(k,i)
	 
cm	   do kk = 1,max_lim
cm	   aa_m_(i) = aa_m(k)

cmar

c      if (tipo_criterio.eq.crit_equi) then


c               if  (max_lim . ne. 0 ) then
c                if  (a_coef(k,i).ne. 0 .and. b_coef(k,i).ne.0) then
			        
c		       tipo_surge = spezzata

c	          else

c	           tipo_surge = parabola

c	          end if
      
c	   else 
c	   	   tipo_surge = spezzata
	 
c       end if

cmar


          lim_h(k,i) = type_lim_h(k,j)
          lim_n(k,i) = type_lim_n(k,j)
          lim_q(k,i) = type_lim_q(k,j)
c-seconda fase
          a_coef2(k,i) = type2_a_coef(k,j)
          b_coef2(k,i) = type2_b_coef(k,j)
          lim_h2(k,i)  = type2_lim_h(k,j)
          lim_n2(k,i)  = type2_lim_n(k,j)
          lim_q2(k,i)  = type2_lim_q(k,j)
        enddo
cmar
      
	do k= 1,max_lim
	   aa_m_(i) = aa_m_(i)+ aa_m(k,i)
      enddo

cmar

        clim(1,i) = type_c_coef(1,j)
        clim(2,i) = type_c_coef(2,j)
        clim(3,i) = type_c_coef(3,j)
        clim(4,i) = type_c_coef(4,j)
c-seconda fase
        clim2(1,i) = type2_c_coef(1,j)
        clim2(2,i) = type2_c_coef(2,j)
        clim2(3,i) = type2_c_coef(3,j)
        clim2(4,i) = type2_c_coef(4,j)
c
        do k=1,5
          chn(k,i) = type_chn(k,j)
          cen(k,i) = type_cen(k,j)
c-seconda fase
          chn2(k,i) = type2_chn(k,j)
          cen2(k,i) = type2_cen(k,j)
        end do
        chn(6,i) = type_chn(6,j)
c-seconda fase
        chn2(6,i) = type2_chn(6,j)
c attenzione !!! i chc sono uguali ai chn se non e' definita la zona di choking
        if (clim(3,i).ne.clim(4,i)) then
          do k=1,6
            chc(k,i) = type_chc(k,j)
          end do
        else
          do k=1,6
            chc(k,i) = chn(k,i)
          end do
        endif
c-seconda fase
        if (clim2(3,i).ne.clim2(4,i)) then
          do k=1,6
            chc2(k,i) = type2_chc(k,j)
          end do
        else
          do k=1,6
            chc2(k,i) = chn2(k,i)
          end do
        endif
c
        if (clim(1,i).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim(1,i)
          bb=1/clim(2,i)
          denom=aa-bb
          faux=cen(1,i)+cen(2,i)/aa+cen(3,i)/aa/aa
          cec(1,i)=(aa*faux-mieff(i)*bb)/denom
          cec(2,i)=(aa*cen(4,i))/denom
          cec(3,i)=(aa*cen(5,i))/denom
          cec(4,i)=(aa*bb*(mieff(i)-faux))/denom
          cec(5,i)=-(aa*bb*cen(4,i))/denom
          cec(6,i)=-(aa*bb*cen(5,i))/denom
        endif
c-seconda fase
        if (clim2(1,i).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim2(1,i)
          bb=1/clim2(2,i)
          denom=aa-bb
          faux=cen2(1,i)+cen2(2,i)/aa+cen2(3,i)/aa/aa
          cec2(1,i)=(aa*faux-mieff2(i)*bb)/denom
          cec2(2,i)=(aa*cen2(4,i))/denom
          cec2(3,i)=(aa*cen2(5,i))/denom
          cec2(4,i)=(aa*bb*(mieff2(i)-faux))/denom
          cec2(5,i)=-(aa*bb*cen2(4,i))/denom
          cec2(6,i)=-(aa*bb*cen2(5,i))/denom
        endif
c
        nlim(i)= type_nlim(j)
c-seconda fase
        nlim2(i)= type2_nlim(j)
c
        do k=1,3
          cpwm(k,i) = type_cpwm(k,j)
          cpwt(k,i) = type_cpwt(k,j)
          chr(k,i)  = type_chr(k,j)
          chrt(k,i) = type_chrt(k,j)
        end do
        cpwt(4,i) = type_cpwt(4,j)
        chrt(4,i) = type_chrt(4,j)
        chr(4,i) = type_chr(4,j)
        chr(5,i) = type_chr(5,j)
        chr(6,i) = type_chr(6,j)
c criterio
        if (tipo_criterio.eq.crit_equi) then
          kcrit_min(i) = unit_crit_min(j)
          kcrit_max(i) = unit_crit_max(j)
c-ele 26/7/06 Questo confronto e l'eventuale ridefinizione di unit_crit_max
c             viene gia eseguito in fase di validazione
c          if (clim(4,i).gt.unit_crit_max(j) ) then
c            type_kcrit_eff(2,i) = clim(4,i)
c-prec-???
c            kcrit_max(i) = type_kcrit_eff(2,i)
c-prec-???-end
c          else
c-ele
          type_kcrit_eff(2,i) = unit_crit_max(j)
c          endif
c- gz- prototipo
c per ora tratto la curva limite minimo del criterio come se fosse anche
c la curva di riciclo
          type_kcrit_eff(1,i) = unit_crit_min(j)
          clim_new(i) = type_kcrit_eff(1,i)
          if (type_bifase(i).and.(stat_ord.eq.f_paral)) then
            call hdan_su_mf(clim_new(i),haux,type_max_rev(i),chn(1,i))
            call qdahn(haux,qaux,type2_max_rev(i),chn2(1,i))
c verifica che il punto non sia oltre il vertice della parabola
            call qvert(chn2(1,i),qauxv,type2_max_rev(i))
            if (qaux.lt.qauxv) then
cgpe-new              call gest_error (2,0,'INIT_SIMPSTAT_MF',
cgpe-new     *             'SurgeII oltre curva teorica',0)

              qaux = qauxv
              call HDAQN(haux,qaux,type2_max_rev(i),chn2(1,i))
            endif
            haux = haux / headrif
            qaux = qaux / qmrif
            clim2_new(i) = haux/(qaux*qaux)
c-ele posso arrivare a questo livello solo se il criterio è l'equidistanza, quindi la 
c     condizione testata non può mai verificarsi; sposto il controllo ad un livello 
c     superiore in modo che risulti essere l'alternativa per il test su 'tipo_criterio'
c          else if (tipo_criterio.eq.crit_port) then
c            clim_new(i)=0.
c          endif
c-ele 26/7/06
           end if
        else if (tipo_criterio.eq.crit_port) then
            clim_new(i)=0.
        endif
c tipo di surge
cmar        if (tipo_criterio.eq.crit_equi) then
cmar	  tipo_surge = parabola
cgpe	else if (tipo_criterio.eq.crit_port) then
cmar	else
cmar	  tipo_surge = spezzata
cmar	end if

cmar  Marzo 2009 -------------------------------------------------------------
cmar  Nuova definizione della curva limite per il criterio dell' Equidistanza
cmar   -----------------------------------------------------------------------

      if (tipo_criterio.eq.crit_equi) then

      
c               if  (max_lim . ne. 0 ) then
c           do k=1,max_lim
c                 if  (a_coef(k,i).ne. 0 .OR. b_coef(k,i).ne.0) the



c	   aa= a_coef(k,i) + b_coef(k,i)
c	   aa_ = aa_+aa
         if  (aa_m_(i) .ne.0) then
                 
		       tipo_surge = spezzata
                  
	          else

	           tipo_surge = parabola

	          end if
c            end do


	   else 
	   	   tipo_surge = spezzata
	 
       end if
c
        nu = nu+type_quant(i+jtfir-1)
      end do
c
c-ele 26/7/06 'tipo_criterio' e 'tipo_surge' sono variabili definite a livello di
c             centrale e quindi il controllo sul loro valore è indipendente da i e j,
c             cioè da tipo e tbg;il controllo è stato quindi portato fuori dal ciclo do
c             per evitare che la stessa assegnazione sia fatta più volte
cmar      if (tipo_criterio.eq.crit_equi) then
cmar	  tipo_surge = parabola
cmar	else
cmar	  tipo_surge = spezzata
cmar	end if

c - mar ------->>>>>>>

cmar      if (tipo_criterio.eq.crit_equi) then



c               if  (max_lim . ne. 0 ) then
c                       tipo_surge = spezzata
c	          else
c	                  tipo_surge = parabola
c	          end if
      
c	    else tipo_surge = spezzata
	  
cmar      end if
c-ele
c---->            set unit status as logical variable
      if (iopt) then
        do i=1,unit_num
          if(index(stat(i),funz_on).gt.0) then
            lstat(i) = .true.
          else
            lstat(i) = .false.
          end if
        end do
      else
        do i=1,unit_num
          j = jufir+i-1
          if(index(status(j),funz_on).gt.0) then
            lstat(i) = .true.
          else
            lstat(i) = .false.
          end if
        end do
      end if
c_ecol
c_ecol --------------------------->Filtri
	f_ce_stat=c_f_ce_stat(vert_pointer)
	f_ce_k=c_f_ce_k(vert_pointer)

	do i=1,unit_num
	   j=jufir+i-1
	   if (lstat(i)) then
	      f_tg_stat(i)=c_f_tg_stat(j)
	      f_tg_k(i)=c_f_tg_k(j)
	   end if
	end do
c_ecol --------------------------->AirCooler
c------------>gestione air_cooler di centrale
	ac_ce_stat=c_ac_ce_stat(vert_pointer)
	ac_ce_k=c_ac_ce_k(vert_pointer)
	ac_q_prog=c_ac_q_prog(vert_pointer)
	ac_taria_prog=c_ac_taria_prog(vert_pointer)
c------------>gestione air_cooler di turbogruppo

	if(stat_ord.eq.f_serie) then    !bifase serie
	   do i=1,unit_num
	      j=jufir+i-1
	      if (lstat(i)) then
               ac1_stat(i)=c_ac1_stat(j)
 	         ac1_k(i)=c_ac1_k(j)
	         ac1_q_prog(i)=c_ac1_q_prog(j)
	         ac1_taria_prog(i)=c_ac1_taria_prog(j)
               ac2_stat(i)=c_ac2_stat(j)
	         ac2_k(i)=c_ac2_k(j)
	         ac2_q_prog(i)=c_ac2_q_prog(j)
	         ac2_taria_prog(i)=c_ac2_taria_prog(j)
	       end if
	   end do
c---------------------------------------------------------------------------------------
c  Definisco per ogni tipologia di turbogruppi un valore unico della costante
c  k caratteristica dell'air cooler di fase 1, in modo da poter calcolare la
c  caduta di pressione ad esso associata nelle routine di simulazione dove i
c  cicli sono eseguiti non sulle singole unità ma sui tipi.La costante è definita  
c  come media dei k delle unità appartenenti ad uno stesso tipo.Tale scelta 
c  è corretta sia quando unità dello stesso tipo hanno tutte lo stesso air cooler
c  sia nel caso più generale di unità appartenenti allo stesso tipo ma con cooler
c  diversi
c----------------------------------------------------------------------------------------
         nu=0
         do i=1,type_num
            j=jtfir+i-1
	      ac_nu=0
            do k=1,type_quant(j)
	         ij=nu+k
               if(lstat(ij)) then
                 type_ac_k_int(i)=type_ac_k_int(i)+ac1_k(ij)
	           ac_nu=ac_nu+1
               end if
            end do
	      if (ac_nu.gt.0) then
	         type_ac_k_int(i)=type_ac_k_int(i)/ac_nu
	      else
	         type_ac_k_int(i)=0.
	      end if
            nu=nu+type_quant(j)
         end do
      else                       !bifase parallelo
	   do i=1,unit_num
	      j=jufir+i-1
	      if (lstat(i)) then
               ac1_stat(i)=c_ac1_stat(j)
		     ac1_k(i)=c_ac1_k(j)
	         ac1_q_prog(i)=c_ac1_q_prog(j)
	         ac1_taria_prog(i)=c_ac1_taria_prog(j)
	         if (unit_bifase(j)) then
                  ac2_stat(i)=c_ac2_stat(j)
	            ac2_k(i)=c_ac2_k(j)
	            ac2_q_prog(i)=c_ac2_q_prog(j)
	            ac2_taria_prog(i)=c_ac2_taria_prog(j)
			 end if
	       end if
	    end do
      end if
c i bifase hanno una sola turbina
      una_turb = .true.
c
      return
      end
c----------------------------------------------------------------------------------




      subroutine Station_mf(vert_pointer,flow,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,nom_flow,
     *     unit_perc,unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c	simulazione di stazione con il nuovo criterio di controllo
c
c--------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
    
CMAR
      COMMON/MAR/istaz,ppmol
      INTEGER*4 istaz
	real*4 ppmol
c	integer*4 ier
CMAR
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
	common/equi_k/k_min(max_unit), k_max(max_unit)

cmar
cmar
      real*4 perc_equi_min, perc_equi_max
	real*4 k_min, k_max
cmar-eq2



cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN
      include '../inc/power.INC'

	INTEGER*4  lsim_n(maxunits) ,ii
	real*4 qmin_0(maxunits), qmin_0_tot, qmin_0_type(maxunits)
      real*4 qmin_0_2(maxunits), qmin_0_2_tot, qmin_0_2_type(maxunits)

cmar-eq2
cmar
c
      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4    nom_flow(*)
      real*4 flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),unit_rev(*),
     *	     unit_max(*),unit_min(*),unit_flow(*),unit_eff_corr(*),
     *	     unit_eff(*),unit_head(*),unit_power_corr(*),unit_power(*),
     *	     unit_hrate(*),unit_cons(*),unit_temp(*),stat_vars(*),
     *       delpr1,delpr2,flowm,head,unit_vcrit(*),
     *       unit_hrate_corr(*)
      integer*4 unit_vinc(*)
      real*4 unit2_perc(*),unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*),unit2_max(*),unit2_min(*)

      LOGICAL*2  unit_bifase(maxunits)
      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   pin,pout,exp_pol,esp,aux1
      real*4   flow_ass
c
      INTEGER*4  i,ier,itip,ker
c
      real*4   ra,rb,rd
      real*4   vcrit
      real*4   haux, raux
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   FLOW_APP
c --- new gz
      real*4    qmin_tot
      real*4    qmax_tot
      real*4    qmin(maxunits)
      real*4    qmin2(maxunits)
      real*4    qmax(maxunits)
      real*4    qmax2(maxunits)
      real*4    head_assoluto(maxunits)
      real*4    qmax_assoluto(maxunits)
      real*4    qmax_assoluto2(maxunits)
      real*4    qmin_assoluto(maxunits)
      real*4    qmin_assoluto2(maxunits)
      integer*4 vinc_min(maxunits)
      integer*4 vinc_max(maxunits)
      real*4    hmin,hmax
      real*4    qass(maxunits)
      real*4    qass2(maxunits)
      real*4    caux(maxunits)
      real*4    fa,fb
      real*4    climite
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 vinc_crit_min, vinc_crit_max
      real*4    vmin, vmax
      real*4    cmin(maxunits), cmax(maxunits)
      integer*4 icj,ij,nutot
      real*4    tot_flow
      real*4    zero_perc
      EXTERNAL  zero_perc
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      logical*2 l_ric
      real*4 tout_ac,delpr_f,delpr_ac
c-mf03
      real*4    pow_app
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18 
      real*4 ro_in,ro_out
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str

	
cmar_08_05
      real*4 n_sim, n_calc
cmar_08_05


cmar_power_stat
c      real*4 max_p,min_p,power_sta
cmar_power_stat      
c-UDM-end
c-----------------------------------------------------------------------

cmar_ac_auto
c      if(ker.eq.79)then
c	ker=0
c	endif
cmar_ac_auto

c      IF ( .NOT. FLAG_AC ) THEN
c	MESS= ' ' 
c	ENDIF
cmar_ac_auto



	max_p=0
	min_p=0
	power_tot_staz=0
	

cmar_pow_stat
      flag_pow_cle = flag_pow(istaz)
cmar_pow_stat
cmar_08_05
	FLAG_LAST =.FALSE.

	A_P=0

CMAR    A_A è CONTENUTA NELLA POWER.INC ED è UTILIZZATA NELLA 
CMAR    FUNCTION IND_VIO_VINC


      n_sim=0
	N_CALC=0
cmar_08_05
cmar_pow_stat

cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312





cmar_DIN
      istaz=am
cmar_DIN


 

      ier = 0

	qmin_0_tot = 0

	qmin_0 = 0

	ii=1

	qmin_0_2_tot = 0

	qmin_0_2 = 0
   
      ppmol = av_weight(ind)
      pin  = pres1 - delpr1
      pout = pres2 + delpr2


c---->             
c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
   


      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,flow,delpr_f)

c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p 
      ro_in=1.
	delpr_f=delpr_f/ro_in


      pin = pin - delpr_f
c      pin  = pres1-delpr1
c             CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER

     





      call delpr_ac_p (ntp,type_quant,status,unit_bifase,nom_flow,
     *                 ac_ce_k,ac1_k,ac2_k,flow,delpr_ac)



      call ro_real(ind,pout,temp1,ro_out)
cmar-p 
      ro_out=1.
c      pout = pres2+delpr2
      delpr_ac=delpr_ac/ro_out




      pout = pout + delpr_ac


cgpe assegnazione di pin e pout. Coincidono con pres1 e pres2.
cgpe Per la centrale composta, è necessario riempire il common compstat.inc
cgpe      pin = pres1
cgpe      pout = pres2
cgpe NOTA: delpr1 e delpr2 comprendono sia il dp dovuto al piping e il dp dovuto
cgpe       alla presenza di eventuali filtri ed aircooler
cgpe-corr
      if (pin .le. 0. .or. pout .le. 0.) ier = 38
      if (pout .lt. pin) ier = 5
      if (ier .gt. 0) then
        if (.not.(iopt .or. iopt_mess)) then
          call app_mess(vert_pointer,ier,mess)
        endif
        return
      endif
cgpe-corr-end
c-bpcorr
      if (.not.(iopt .or. iopt_mess)) then
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac
      endif


c-bpcorr-end
cgpe-end
CMAR
   


      comp_ratio = pout/pin

CMAR      PIN = Pin/(PRIF+P0)
CMAR      Temp1 = Temp1/(trif+t0)   

CMAR      call POLITROP_ESP(istaz,pin,temp1,comp_ratio,exp_pol,ier)

CMAR      comp_ratio = pout/pin

      call POLITROP_ESP(istaz,pin,temp1,comp_ratio,exp_pol,ier)
      esp = (exp_pol-1.)/exp_pol

	
CMAR


CMAR      exp_pol = prop(pin,temp1,exp_coef(1,ind))
CMAR      esp = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c---->             adiabatic head
CMAR

CMAR      z = prop(pin,temp1,z_coef(1,vert_pointer))
       call zpt1(istaz,pin,temp1,z)
      aux1 = (erre / agrvcc / pmol) * Z * temp1/esp
      aux2 = comp_ratio**esp
      head = aux1 * (aux2-1.)    ! in km  



         
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp1/pin
      aux3 = ROCN*uno_su_ro_attuale
      flowm = flow*aux3


      flow_ass = 0.
c carico headm nel common simpstat.INC per alcune routine zero_***
      headm = head
      do i=1,ntp
        head_assoluto(i) = head      
      enddo

      do i=1,unit_num
        lsim(i) = status(i)




cmar-eq2   vettore di interi che mi dice chi è acceso o spento per fare i conti

      if(lsim(i)) then
      lsim_n(i)= 1
cmar_08_05
	n_sim=n_sim+lsim_n(i)
cmar_08_05
	else
      lsim_n(i)= 0
      endif





cmar-eq2
      end do
      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo
      do i=1,ntp

        cmin_eff(i) = clim_new(i)
        caux(i) = clim(4,i)
      enddo


     
c---> calcolo gli estremi, in prevalenza (hmax,hmin), della stazione
      call limiti_h(hmin,hmax)
      if (head.gt.hmax) then
c errore: prevalenza richiesta superiore alla massima fattibile
        ier = 6 ! ier = 5
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        if (fl_sim) then
c-UDM          write(mess,557) head,hmax,UDM_INT_H ! write(mess,557) head,hmax
c-UDM557       format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
557       format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
            write(mess,557) udm_out_str(udm_H,head,0),
     *                      udm_out_str(udm_H,hmax,1)
          call app_mess(vert_pointer,ier,mess)
        endif




      else if (head.lt.hmin) then
c errore: prevalenza richiesta inferiore alla mimima fattibile
        ier = 7 ! ier = 6
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        if (fl_sim) then
c-UDM          write(mess,559) head,hmin,UDM_INT_H ! write(mess,559) head,hmin
c-UDM559       format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
559       format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
          write(mess,557) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmin,1)
          call app_mess(vert_pointer,ier,mess)
        endif


      endif
c
10    flow_tot = (flowm-flow_ass)

      call calc_limiti(type_min_rev, type_max_rev, clim_new, caux)
c-preparazione common per il calcolo dei limiti estremi
c entro le mappe 
      
      do i=1,ntp
        cmin_eff(i) = clim_new(i)
        cmax_eff(i) = clim(4,i)
     

      enddo

c--->
      call limiti_q_bifase(head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)



    
      do itip= 1, ntp



	if (type_bifase(itip)) then


      	do ij=1, type_quant(itip)


	qmin_0_2(ii)=(qmin_assoluto2(itip)*lsim_n(ii))/(1+perc_equi_min(itip))
	qmin_0(ii) = (qmin_assoluto(itip)*lsim_n(ii))/(1+perc_equi_min(itip))



	            qmin_0_tot=qmin_0_tot+qmin_0_2(ii)+qmin_0(ii)





      ii=ii+1


	    enddo

	qmin_0_2_type(itip)=(qmin_assoluto2(itip))/(1+perc_equi_min(itip))
      qmin_0_type(itip)=(qmin_assoluto(itip))/(1+perc_equi_min(itip))


      

	else

     

	       do ij=1, type_quant(itip)


	qmin_0(ii) = (qmin_assoluto(itip)*lsim_n(ii))/(1+perc_equi_min(itip))

	qmin_0_tot=qmin_0_tot+qmin_0(ii)



      ii=ii+1


	       enddo
	qmin_0_type(itip)=(qmin_assoluto(itip))/(1+perc_equi_min(itip))




	endif

      enddo

      
      climite = flow_tot/qmin_0_tot


      

      
cmar-eq2

	
cmar -----------------------
      do itip=1,ntp
      kcrit_min(itip)=k_min(itip)
	kcrit_max(itip)=k_max(itip)
c	type_kcrit_eff(1,itip)=k_min(itip)
c      type_kcrit_eff(2,itip)=k_max(itip)

	end do
cmar  ------ ------ --------
      if(ier.ne.0) then
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1
        return
      endif

      l_ric = .true.
   

      if (flow_tot.lt.qmin_tot) then

        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)     = qmin_assoluto(itip)
            qass2(itip)    = qmin_assoluto2(itip)
          endif
        enddo
        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            if (vinc_min(itip) .ne. sur_1f .and.
     *          vinc_min(itip) .ne. sur_2f     ) then
              l_ric = .false.
            end if
          end if
        end do
	  
	  goto 77


      else if (flow_tot.gt.qmax_tot) then
c	
      do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_max(itip)
            qass(itip)     = qmax_assoluto(itip)
            qass2(itip)    = qmax_assoluto2(itip)
          endif
        enddo
        goto 77
      endif

c simulazione normale
c calcolo la portata massima nei limiti del rispetto del criterio


cmar-eq2      call calc_clim(vinc_min, vinc_max, qmax_assoluto,
cmar-eq2     *               head_assoluto, qmin_assoluto, head_assoluto,
cmar-eq2     *               vinc_crit_min, vinc_crit_max, vmin, vmax,
cmar-eq2     *               cmin, cmax, ier)
cmar-eq2      if (ier.ne.0) then
!        if (iopt) return
cmar-eq2        if (iopt .or. iopt_mess) return
cmar-eq2        call app_mess(vert_pointer,ier,mess)
cmar-eq2        ier = 1
cmar-eq2        return
cmar-eq2      endif
cmar-eq2      call calc_limiti(type_min_rev, type_max_rev, cmin, cmax)



c
c-preparazione common per il calcolo dei limiti effettivi
      do i=1,ntp
        cmin_eff(i) = cmin(i)
        cmax_eff(i) = cmax(i)
      enddo
c
      call limiti_q_bifase(head,qmin_tot,qmin,qmin2,qmax_tot,qmax,
     *             qmax2,vinc_max,vinc_min,ier)
      	
		
		

c


      if(ier.ne.0) then
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1
        return
      endif




      if (flow_tot.lt.qmin_tot) then
c una macchina va imposta al minimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_min(vinc_crit_min).eq.sur_1f)  .or.
     *        (vinc_min(vinc_crit_min).eq.sur_2f))  then
cgpe-ric            ker = 21
            ker = 28
cgpe-ric-end
          else
            ker = 22
          endif

          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if




c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmin(vinc_crit_min)*type_actr(vinc_crit_min)
        if (type_bifase(vinc_crit_min)) then
         flow_ass=flow_ass+qmin2(vinc_crit_min)*type_actr(vinc_crit_min)
        endif
        type_actr(vinc_crit_min) = 0
        tip_vinc(vinc_crit_min)  = vinc_min(vinc_crit_min)
        qass(vinc_crit_min)      = qmin(vinc_crit_min)
        qass2(vinc_crit_min)     = qmin2(vinc_crit_min)
        goto 10





      else if (flow_tot.gt.qmax_tot) then
c una macchina va imposta al massimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_max(vinc_crit_max).eq.cho_1f)  .or.
     *        (vinc_max(vinc_crit_max).eq.cho_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Parabola'
cgpe-ric            ker = 24
            ker = 29
cgpe-ric-end
          elseif ((vinc_max(vinc_crit_max).eq.mar_1f)  .or.
     *            (vinc_max(vinc_crit_max).eq.mar_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Giri'
            ker = 23
          else

            ker = 25
          endif
cgpe-mess          ker = 94
          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if




c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmax(vinc_crit_max)*type_actr(vinc_crit_max)
        if (type_bifase(vinc_crit_max)) then
         flow_ass=flow_ass+qmax2(vinc_crit_max)*type_actr(vinc_crit_max)
        endif
        type_actr(vinc_crit_max) = 0
        tip_vinc(vinc_crit_max)  = vinc_max(vinc_crit_max)
        qass(vinc_crit_max)      = qmax(vinc_crit_max)
        qass2(vinc_crit_max)     = qmax2(vinc_crit_max)
        goto 10
      else
c simulazione normale delle macchine residue
c ricerca il clim per cui Qlavorato = flow_tot
c ricerca nell'intervallo vmin-vmax
c che diventa per ogni unita'' un intervallo clim_min-clim_max



cmar-eq2



          do itip=1,ntp    

            if (type_actr(itip).gt.0) then
              type_actr(itip) = 0
              tip_vinc(itip)  = 0

              
	qass(itip)=climite*qmin_0_type(itip)


              


              if (type_bifase(itip)) then

	qass2(itip)=climite*qmin_0_2_type(itip)


c                call hdaq_ch(clim(3,itip),haux,qass(itip))


c                if(head.gt.haux) then
c                  call ndahq(head,qass(itip),raux,chn(1,itip))
c                else
c                  call ndahq(head,qass(itip),raux,chc(1,itip))
c                end if

c                raux = raux*type_ratio(itip)/type2_ratio(itip)

c                call hdan_ch(clim2(3,itip),haux,raux,chn2(1,itip))

c                if(head.gt.haux) then
c                  call qdahn(head,qass2(itip),raux,chn2(1,itip))
c                else
c                  call qdahn(head,qass2(itip),raux,chc2(1,itip))
c                end if



 


cmar_pow_stat
c
              endif

cmar_pow_stat
ccccc_312      min_p=min_p+type_vinc_minpow(itip)
ccccc_312      max_p=max_p+type_vinc_maxpow(itip)
cmar_pow_stat	
      
            endif
          enddo

        endif

c
c
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, qass (o qass2), vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old

77    continue

c problema: dati head e qass per ogni tipo, calcolare le altre grandezze
c-bpcorr
c-l_ric      l_ric = .true.
c-bpcorr-end




      nutot = 0
      do itip=1,ntp
        if(type_act_old(itip).gt.0) then
          do ij =1,type_quant(itip)
            icj = nutot+ij
            if(status(icj)) then

cmar_08_05      
          n_calc=n_calc+1
cmar_08_05 
           
      unit_vcrit(icj)= climite


              call dati_compressore(head,qass(itip),qass2(itip),
     *                  qmax_assoluto(itip),qmax_assoluto2(itip),
     *                  qmin_assoluto(itip),qmin_assoluto2(itip),itip,
     *                  unit_bifase(icj),unit_head(icj),unit2_head(icj),
     *                  unit_rev(icj),unit2_rev(icj),unit_flow(icj),
     *                  unit2_flow(icj),unit_eff(icj),unit2_eff(icj),
     *                  unit_max(icj),unit_min(icj),unit2_max(icj),
     *                  unit2_min(icj),unit_eff_corr(icj),
     *			unit2_eff_corr(icj),unit_power_corr(icj),
     *                  unit2_power_corr(icj),unit_power(icj),
cgpe     *			unit2_power(icj),unit_hrate(icj),unit_cons(icj),
     *			unit2_power(icj),unit_hrate_corr(icj),
     *                  unit_hrate(icj),unit_cons(icj),
     *                  unit_vcrit(icj),unit_vinc(icj),tip_vinc(itip),
     *                  ier)

cmar-eq2
	 if(unit_flow(icj).gt.qmax_assoluto(itip)  .OR.
     *        unit_flow(icj).lt.qmin_assoluto(itip)) then

	ier=1   
      endif







cmar-eq2


cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)+unit2_power(icj)
cmar_pow_stat



c controllo dell'errore in uscita dalla dati_compressore
              if (ier.gt.0) then
!                if (.not.iopt) then
                if (.not.iopt .and. .not.iopt_mess) then

                  mess = ' '

                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                end if
!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                ier = 1



                return
c-mf03
              else

cmar_08_05   

      IF ( N_CALC .EQ. N_SIM ) THEN

	FLAG_LAST =.TRUE.
      ENDIF
cmar_08_05   

c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
                pow_app = unit_power(icj) + unit2_power(icj)
                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))

cmar_pow_stat
      




	  
                if (fl_vinc .and. ier.gt.0) then
                  ier = 1

                  return
                else

cgpe                  if (ier .gt. 0) then
                  if (ier .gt. 0 .and. iopt) then
c                  if (ier .gt. 0 .and. iopt .and..not.iopt_last) then
                    unit_cons(icj) = unit_cons(icj) + soglia_cons




                  endif
                  if (ier .gt. 0) unit_vinc(icj) = ier
                  ier = 0
                endif
c-mf03-end
              end if

            endif
          enddo
        endif
        nutot = nutot+type_quant(itip)
      enddo

      
c
      tot_cons = 0.
      tot_flow = 0.

c--------------------------------------------------------------------------
c	max_p=0
c	min_p=0
c	power_tot=0


c--------------------------------------------------------------------------      
      do icj=1,unit_num
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)
c------------------------------------------------

c-------------------------------------------------



        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do
    

      flow_ric = tot_flow/aux3 ! portata compressa (contenente eventuale riciclo)
      

      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow.lt.flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8

  

          if (iopt .or. iopt_mess) return

560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
          write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                    udm_out_str(udm_Q,flowm/aux3,1)
          call app_mess(vert_pointer,ier,mess)
        else if (tot_flow.gt.flowm) then
!!          if (iopt .or. iopt_mess) return
!          if (iopt) return
          if (iopt) then
            if (.not.l_ric) ier = 20
            return
          endif
c-l_ric          if (.not.iopt_mess) then
            if (l_ric) then
              ier = 27
            else
cstaz-corr
cstaz-corr-end
              ier = 20
            endif
cgpe-new
            if (l_ric) then
c-l_ric
             if (.not.iopt_mess) then
c-l_ric-end
c-bpcorr		    ier = 0
cgpec portata di riciclo
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM              write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3, (flowm*conv_ns)/aux3
              write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                        udm_out_str(udm_Q,flowm/aux3,1)
              call app_mess(vert_pointer,ier,mess)
c-l_ric
             endif
c-l_ric-end
c-bpcorr
		    ier = 0
c-bpcorr-end
            endif

        endif
      endif
cgpe-new
cgpe      if (iopt .or. iopt_mess) return
      if (ier .ne. 0) then
	  if (iopt .or. iopt_mess) return
      endif
cgpe-new-end
      do icj=1,unit_num
        if(status(icj)) then
          unit_perc(icj) = unit_flow(icj)/tot_flow
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/tot_flow
          endif
        endif
      enddo

c calcolo della temperatura di uscita della centrale
      temp2 = 0.

    
      call out_temper_mf(
     *         tair,
     *         temp2,temp1,Z,pin,pout,aux2,
     *         z_coef(1,vert_pointer),
c-ace*         eff_coef,unit_num,status,
     *         dh_coef(1,ind),cp_par(1,ind),unit_num,status,
     *         unit_perc,unit_eff,
     *         unit_flow,
     *         unit_temp,unit2_perc,unit2_eff,
     *         unit2_flow,
     *         unit2_temp,unit_bifase,stat_vars(7))


c-ele--->intervento su deltat aircooler
      temp2 = temp2 - stat_vars(5)

cgpe	call out_temper_ac_ce(tair,pout,tot_flow,temp2,stat_vars(7))


	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7)
     * ,pin,temp1,temp2)
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning

      if (iopt .or. iopt_mess) return
cgpe-end
cmar_AC_auto
      IF (.not.FLAG_AC)THEN

cmar_AC_auto


      if (temp2.gt.stat_vars(7)) then
        ker = 79




        write(mess,570) udm_out_str(udm_T,temp2,0),
     *                  udm_out_str(udm_T,stat_vars(7),1)
570   format('T man: ',A10,' - T man max: ',A25)
        call app_mess(vert_pointer,ker,mess)
	end if
cmar_AC_auto
	ENDIF
cmar_AC_auto
      



      return
      end




c..............................................................................


cmar*******************************************************************************
      subroutine Station_mf_as(vert_pointer,flow,pres1,pres2,temp1,
     *     temp2,delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,nom_flow,
     *     unit_perc,unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c	simulazione di stazione con il nuovo criterio di controllo
c
c--------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
CMAR_ASS_COND
      	include '../inc/ASS_COND.INC'
CMAR_ASS_COND    




cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat
c

CMAR
      COMMON/MAR/istaz,ppmol
      INTEGER*4 istaz
	real*4 ppmol
c	integer*4 ier
CMAR
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
	common/equi_k/k_min(max_unit), k_max(max_unit)

cmar
cmar

      real*4 perc_equi_min, perc_equi_max
	real*4 k_min, k_max
cmar-eq2
cc
      COMMON/DP/dp


	

cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN
cc      
	INTEGER*4  lsim_n(maxunits) ,ii
	real*4 qmin_0(maxunits), qmin_0_tot, qmin_0_type(maxunits)
      real*4 qmin_0_2(maxunits), qmin_0_2_tot, qmin_0_2_type(maxunits)

cmar-eq2
cmar
c
      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4    nom_flow(*)
      real*4 flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),unit_rev(*),
     *	     unit_max(*),unit_min(*),unit_flow(*),unit_eff_corr(*),
     *	     unit_eff(*),unit_head(*),unit_power_corr(*),unit_power(*),
     *	     unit_hrate(*),unit_cons(*),unit_temp(*),stat_vars(*),
     *       delpr1,delpr2,flowm,head,unit_vcrit(*),
     *       unit_hrate_corr(*)
      integer*4 unit_vinc(*)
      real*4 unit2_perc(*),unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*),unit2_max(*),unit2_min(*)

      LOGICAL*2  unit_bifase(maxunits)
      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   pin,pout,exp_pol,esp,aux1
      real*4   flow_ass
c
      INTEGER*4  i,ier,itip,ker
c
      real*4   ra,rb,rd
      real*4   vcrit
      real*4   haux, raux
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   FLOW_APP
c --- new gz
      real*4    qmin_tot
      real*4    qmax_tot
      real*4    qmin(maxunits)
      real*4    qmin2(maxunits)
      real*4    qmax(maxunits)
      real*4    qmax2(maxunits)
      real*4    head_assoluto(maxunits)
      real*4    qmax_assoluto(maxunits)
      real*4    qmax_assoluto2(maxunits)
      real*4    qmin_assoluto(maxunits)
      real*4    qmin_assoluto2(maxunits)
      integer*4 vinc_min(maxunits)
      integer*4 vinc_max(maxunits)
      real*4    hmin,hmax
      real*4    qass(maxunits)
      real*4    qass2(maxunits)
      real*4    caux(maxunits)
      real*4    fa,fb
      real*4    climite
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 vinc_crit_min, vinc_crit_max
      real*4    vmin, vmax
      real*4    cmin(maxunits), cmax(maxunits)
      integer*4 icj,ij,nutot
      real*4    tot_flow
      real*4    zero_perc
      EXTERNAL  zero_perc
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      logical*2 l_ric
      real*4 tout_ac,delpr_f,delpr_ac
c-mf03
      real*4    pow_app
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18 
      real*4 ro_in,ro_out
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
      




      real*4 pin_(2), pout_(2), comp_ratio_(2),head_(2),exp_pol_(2),
     *  esp_(2),aux1_(2),aux3_(2),z_(2),uno_su_ro_attuale_(2),aux2_(2),
     *  ntp_(2)
cc     * aux3_type(MAXUNITS),head_type(maxunits)
      real*4 pin_s, exp_pol_s,comp_ratio_s,z_s,qmin_tot_(2),qmax_tot_(2)

      real*4 unit_flow_(240),unit_bifase_(240),unit_head_(240),
     * unit_rev_(240),unit_eff_(240),unit_max_(240),unit_min_(240),
     *unit_eff_corr_(240),unit_power_corr_(240),unit_power_(240),
     *unit_hrate_corr_(240),unit_hrate_(240),unit_cons_(240),
     *unit_vcrit_(240), unit_vinc_(240),unit_temp_(240),unit_perc_(240),
     *STATUS_(240)
	
c	integer*4 j,m,vect(maxunits),k,nn
      integer*4 j,m,vect(24),k,nn

	real*4 aa(maxunits),  ss_primo, rr_primo

	integer*4 ss(maxunits),rr(maxunits)
	integer*4 SS_TYPE(maxunits),RR_TYPE(maxunits)
c-UDM-end

      real*4 qmin_tot_p,qmax_tot_p,qmin_assoluto_1(maxunits),
     * qmax_assoluto_1(maxunits),A,type_actr_b(maxunits),
cmar_12_01_12
     * qmin_assoluto_2(maxunits),qmax_assoluto_2(maxunits)
cmar_12_01_12
	REAL*4 NOM_1,NOM_2,NOM_TOT,p1,p2,q1,q2,df_1,df_2,dac_1,dac_2,
     * q_icj(maxunits),tot_flow_n
	logical*2 b,dp
c      logical*2 ass1, ass2
	real*4 qq1, qq2, qx1, qx2

cmar    temporaneamente:
       external  dp_in,dp_out
	real*4    dp_in,dp_out

	real*4 delpr1_1,delpr2_1,delpr1_2,delpr2_2
	real*4 delprf_1,delprf_2,delprac_1,delprac_2

CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI
	REAL*4 DP_AC_1_M,DP_AC_2_M
CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI

cmar_Ass_26

	logical*2 pp

	


cmar_08_05
      real*4 n_sim, n_calc
cmar_08_05

cmar_ass_26
cmar    temporaneamente:
cmar_ass_cond_temp

c      REAL*4 temp2_1,temp2_2


      COMMON/Tou_ass/T_staz

	real*4  T_staz

c-----------------------------------------------------------------------
      


	max_p=0
	min_p=0
	power_tot_staz=0
cmar_pow_stat
      flag_pow_cle = flag_pow(istaz)

cmar_08_05
	FLAG_LAST =.FALSE.

	A_P=0

CMAR    A_A è CONTENUTA NELLA POWER.INC ED è UTILIZZATA NELLA 
CMAR    FUNCTION IND_VIO_VINC


      n_sim=0
	N_CALC=0
cmar_08_05

cmar_pow_stat



cmar_DIN
      istaz=am
cmar_DIN


c--->
cmar_warning    
      ass1t= .false.
	ass2t= .false.
cmar_warning

cmar_Ass_26
      pp=.false.
cmar_Ass_26



cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312



cmar    temporaneamente:

      delpr1=0.
	delpr2=0.
cmar    temporaneamente:

cmar_ass_25
      flag_dati_s(ISTAZ)=.TRUE.
	flag_dati=flag_dati_s(ISTAZ)
cmar_ass_25

      a_err(istaz)=.false.
      


      dp=.true.
      b=.false.
      
      ier = 0

CMAR_ASS13	qmin_0_tot = 0

	qmin_0 = 0

cmar_ass13	ii=1

	qmin_0_2_tot = 0

	qmin_0_2 = 0
cc
      qmin_tot_p=0
	qmax_tot_p=0

cc
	flow1=0
	flow2=0
	flowm_1=0
	flowm_2=0
cc
cmar_ass_cond_rpt	 
       tot_cons1=0
       tot_cons2=0

cmar_ass_cond_rpt	 



      NTP_1=NTP_1_(ISTAZ)
	NTP_2=NTP_2_(ISTAZ)
      UNIT_1=UNIT_1_(ISTAZ)
	UNIT_2=UNIT_2_(ISTAZ)


c	flowm_1=0
c	flowm_2=0

cmar_ass_cond
      do i=1,24
	vect(i)=0
	enddo
      

cmar_ass_cond



      SS_TYPE(1:12)=0
	RR_TYPE(1:12)=0


       lsim_n(1:12)=0
       unit_flow(1:24)=0
      
cmar_ass_cond

      
 
	


      pin  = pres1
      pout = pres2


      

      DO Itip=1, NTP_1
      call aaa_aaa (itip,type_min_rev,type_max_rev,clim_new,caux,a)
	nom_1=nom_1+(a*type_quant(itip))
      
      enddo     
	
	DO Itip=NTP_1+1,NTP_1+NTP_2
      call aaa_aaa (itip,type_min_rev,type_max_rev,clim_new,caux,a)
	nom_2=nom_2+(a*type_quant(itip))	
	
      enddo    
      	

	nom_tot=nom_1+nom_2

	p1=nom_1/nom_tot
	p2=1-p1
	
	q1=flow*p1
	q2=flow*p2

      



      
cc      q1=3179.221    
cc      q2=1573.934   

cmar_Ass_26   pp= .t. solo al primo giro di do, qnd ho le portate di primo tentativo per i deltaP
      pp=.true.
cmar_Ass_26

     


cc      inizio il do di convergenza    
 
      do nn=1,10
cc
      




100	 flowm_1=0
	flowm_2=0
cmar_pow_stat_corr enna
	power_tot_staz=0
cmar_pow_stat_corr enna
cmar_ass13
cmar    temporaneamente Piping:

         delpr1_1 = dp_in(istaz,pres1,temp1,q1)

	
	   delpr2_1 = dp_out(istaz,pres2,stat_vars(7),q1)
cmar   sul secondo assetto
	    delpr1_2 = dp_in(istaz+1,pres1,temp1,q2)

	
	   delpr2_2 = dp_out(istaz+1,pres2,stat_vars(7),q2)
cmar    
   
   
cccccccccccccccccccccccccccccccccccccccccccc




     
c                       CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro1 (ntp_1,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,q1,delprf_1)

      call delpr_filtro2 (ntp_1,ntp_2,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,q2,delprf_2)
     
   
CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI

	
      IF( FLAG_AC )	THEN
      DP_AC_1_M=DP_AC_1
      DP_AC_2_M=DP_AC_2

      DP_AC_1=0
      DP_AC_2=0

      ENDIF

CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI


      call delpr_ac_p1 (ntp_1,type_quant,status,unit_bifase,nom_flow,
     *                 ac_ce_k,ac1_k,ac2_k,q1,delprac_1)

	  
      call delpr_ac_p2 (ntp_1,ntp_2,type_quant,status,unit_bifase,
     *                 nom_flow,ac_ce_k,ac1_k,ac2_k,q2,delprac_2)

CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI
      
      IF( FLAG_AC )	THEN
      DP_AC_1=DP_AC_1_M
      DP_AC_2=DP_AC_2_M
      ENDIF

CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI




		ii=1
      
      qmin_0_tot = 0

      


c      df_1= q1*q1*dp_f_1
c	dac_1= q1*q1*dp_ac_1
c	dac_2= q2*q2*dp_ac_2
c      df_2= q2*q2*dp_f_2

C      


cc	                      
      dp_in_1=delpr1_1+delprf_1

      dp_in_2=delpr1_2+delprf_2

	dp_out_1=delpr2_1+delprac_1

	dp_out_2=delpr2_2+delprac_2



     
     
      if(b)then
	do itip=1,ntp
		type_actr(itip)=type_actr_b(itip)
          type_actr_b(itip)=0
      enddo
      endif
      do itip=1,ntp
		
          type_actr_b(itip)=0
      enddo





cgpe assegnazione di pin e pout. Coincidono con pres1 e pres2.
cgpe Per la centrale composta, è necessario riempire il common compstat.inc
cgpe      pin = pres1
cgpe      pout = pres2
cgpe NOTA: delpr1 e delpr2 comprendono sia il dp dovuto al piping e il dp dovuto
cgpe       alla presenza di eventuali filtri ed aircooler
cgpe-corr
      



c      pin_(1)=pin-df_1
c	pin_(2)=pin-df_2
c	pout_(1)=pout+dac_1
c	pout_(2)=pout+dac_2
      


      

	pin_(1)=pin-dp_in_1

	pin_(2)=pin-dp_in_2

	pout_(1)=pout+dp_out_1

	pout_(2)=pout+dp_out_2
      
 

      if(q1.eq.0)then
	m=2
	goto 17
	elseif(q2.eq.0)then
	m=1
	goto 17
	endif

      
        do m=1, 2
      
      
 17      if ((pin_(m) .le. 0.).or. (pout_(m) .le. 0.)) ier = 38



      if (pout_(m) .lt. pin_(m)) ier = 5

      

      if (ier .gt. 0) then
        if (.not.(iopt .or. iopt_mess)) then
          call app_mess(vert_pointer,ier,mess)
        endif
cmar_Asss_san    
	a_err(istaz)=.true.
      
    
      return

     
cmar_Asss_san  goto 111



      endif
cgpe-corr-end
c-bpcorr

CMAR_ASS_COND !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     

      if (.not.(iopt .or. iopt_mess)) then
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac	
      endif

c-bpcorr-end
cgpe-end
CMAR
      
      
      comp_ratio_(m) = pout_(m)/pin_(m)
	

      pin_s=pin_(m)
      comp_ratio_s=comp_ratio_(m)
      call POLITROP_ESP(istaz,pin_s,temp1,comp_ratio_s,exp_pol_s,ier)
      exp_pol_(m)=exp_pol_s

      esp_(m) = (exp_pol_(m)-1.)/exp_pol_(m)
      call zpt1(istaz,pin_s,temp1,z_s)
	z_(m)=z_s


      aux1_(m) = (erre / agrvcc / pmol) * Z_(m) * temp1/esp_(m)
      aux2_(m) = comp_ratio_(m)**esp_(m)
      head_(m) = aux1_(m) * (aux2_(m)-1.)    ! in km      
      uno_su_ro_attuale_(m)=(1/1.e5)*(erre/pmol)*Z_(m)*temp1/pin_(m)
      aux3_(m) = ROCN*uno_su_ro_attuale_(m)
      flowm = flow*aux3_(m)
      


      head=head_(m)
     

cc      CHIUDO IL DO su m
      ENDDO
      
cc 


CMAR_ASS_COND_10_10_2013
      H1=head_(1)
	H2=head_(2)
CMAR_ASS_COND_10_10_2013
      
     
      DO I=1, NTP_1
      aux3_type(i)=AUX3_(1)
	head_type(i)=head_(1)

	ENDDO

      DO I= NTP_1+1, NTP_1+NTP_2
	aux3_type(i)=AUX3_(2)
	head_type(i)=head_(2)
	ENDDO

cc

      flow_ass = 0.
c carico headm nel common simpstat.INC per alcune routine zero_***
      headm = head
      do i=1,ntp
        head_assoluto(i) = head      
      enddo

      do i=1,unit_num
        lsim(i) = status(i)

     
cmar-eq2   vettore di interi che mi dice chi è acceso o spento per fare i conti

      
      if(lsim(i)) then
      lsim_n(i)= 1
	
cmar_08_05
	n_sim=n_sim+lsim_n(i)
cmar_08_05

	else
      lsim_n(i)= 0

      endif
      
cmar-eq2
      end do
      do i=1,ntp
        type_act_old(i)=type_actr(i)

      enddo
        
      do i=1,ntp

        cmin_eff(i) = clim_new(i)
        caux(i) = clim(4,i)

      enddo
     

CC    check di head
     
	


c---> calcolo gli estremi, in prevalenza (hmax,hmin), della stazione
      call limiti_h(hmin,hmax)

	
cmar_ass_cond      if (head.gt.hmax) then
     
      IF((head_(1).gt.hmax).OR.(head_(2).gt.hmax)) then
	 


c errore: prevalenza richiesta superiore alla massima fattibile
        ier = 6 ! ier = 5
       
cmar_ass_26
 


cmar_ass_26      
      if(pp)then
cmar_ass_26	
    
c	IER=0
cmar_ass_26 	
      goto 117
cmar_ass_26 
 	endif

cmar_ass_26

		a_err(istaz)=.true.
 
CMAR_ASS_COND21


c      SIM(ISTAZ)=.TRUE.
c	SIM_S=SIM(ISTAZ)

CMAR_ASS_COND21

!        if (iopt) return
       if (iopt .or. iopt_mess) return
		

cmar_ass_Cond_san       if (iopt .or. iopt_mess) goto 111

        if (fl_sim) then


c-UDM          write(mess,557) head,hmax,UDM_INT_H ! write(mess,557) head,hmax
c-UDM557       format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
557       format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
            write(mess,557) udm_out_str(udm_H,head,0),
     *                      udm_out_str(udm_H,hmax,1)
          call app_mess(vert_pointer,ier,mess)
        endif


cmar_ass_cond       else if (head.lt.hmin) then
      else if ((head_(1).lt.hmin).OR.(head_(2).lt.hmin)) then

c errore: prevalenza richiesta inferiore alla mimima fattibile
        ier = 7 ! ier = 6

cmar_ass_26
c     
   

cmar_ass_26
       if(pp)then
cmar_ass_26
	
c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26


	 	a_err(istaz)=.true.
	
	


!        if (iopt) return
         if (iopt .or. iopt_mess) return
cmar_ass_cond:_san       if (iopt .or. iopt_mess) goto 111

        if (fl_sim) then
c-UDM          write(mess,559) head,hmin,UDM_INT_H ! write(mess,559) head,hmin
c-UDM559       format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
559       format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
          write(mess,557) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmin,1)
          call app_mess(vert_pointer,ier,mess)
        endif


      endif

c
c10    flow_tot = (flowm-flow_ass)
 10      flow_tot = (flow-flow_ass)

 


      call calc_limiti(type_min_rev, type_max_rev, clim_new, caux)
c-preparazione common per il calcolo dei limiti estremi
c entro le mappe 
      do i=1,ntp
        cmin_eff(i) = clim_new(i)
        cmax_eff(i) = clim(4,i)
      
      enddo
c--->
cc        

	do i=1, UNIT_1
	ss(i)=lsim_n(i)     
	enddo


	DO I= 1, NTP_1
	SS_TYPE(I)=1

	ENDDO
c DA CONTROLLARE: SE è DA UNIT_1+1 A UNIT_2
	do i = UNIT_1,UNIT_2
	rr(i)=lsim_n(i)
	enddo
	DO I= NTP_1+1, NTP_1+NTP_2
	RR_TYPE(I)=1
	ENDDO

       
	 
cc
      if(q1.eq.0)then
	m=2
	goto 18
	elseif(q2.eq.0)then
	m=1
	goto 18
	endif


      

      do m=1, 2
	

cmar_ass_cond
18    qmin_tot_(m)=0
	qmin_tot=0
	
      qmax_tot_(m)=0
	qmax_tot=0

	



	comp_ratio= comp_ratio_(m)  	   
      exp_pol=exp_pol_(m)
      esp=esp_(m)
      z=z_(m)


      aux1=aux1_(m) 
      aux2=aux2_(m) 
      
      uno_su_ro_attuale_=uno_su_ro_attuale_(m)
      aux3=aux3_(m) 
      
	    
      head= head_(m)
      
      

cccccccccccccccccc	ntp=ntp_(m)
cmar_ass_cond  PRIMO 1111111111111111111111111111111111111111

	  if(m.eq.1)then

	      do i=1, maxunits
	
	      vect(i)= ss_type(i)
	
     



	      enddo
	  else
	
            do i=1, maxunits

	      vect(i)=rr_type(i)




	      enddo
        endif
      
c   primo
c   primo 
  

    
      call limiti_q_bifase_as(vect,head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)

		

      qmin_tot_p=qmin_tot_p+qmin_tot/aux3
      qmax_tot_p=qmax_tot_p+qmax_tot/aux3
     

		

	if(m.eq.1)then
		
cc      qq1=qmin_tot_p
cc	qx1=qmax_tot_p
      do k=1, ntp_1
      qmin_assoluto_1(k)=qmin_assoluto(k)
	qmax_assoluto_1(k)=qmax_assoluto(k)
      
	enddo
cmar_12_01_12
      else
     		
	do k=ntp_1+1,ntp_1+ntp_2
      qmin_assoluto_2(k)=qmin_assoluto(k)
	qmax_assoluto_2(k)=qmax_assoluto(k)

	enddo



	endif
		

chiudo il do grande sulle m      
	enddo
      

cc	qmin_tot_p=qq1+qq2
cc      qmax_tot_p=qx1+qx2

      		
	do k=1, ntp_1
      qmin_assoluto(k)=qmin_assoluto_1(k)
	qmax_assoluto(k)=qmax_assoluto_1(k)
    
	enddo
cc	       
cmar_12_01_12
    
	do k=ntp_1+1,ntp_1+ntp_2
      qmin_assoluto(k)=qmin_assoluto_2(k)
	qmax_assoluto(k)=qmax_assoluto_2(k)

	enddo

cmar_12_01_12

    
      do itip= 1, ntp
			
     

	if (type_bifase(itip)) then


      	do ij=1, type_quant(itip)

      
      
	qmin_0_2(ii)=(qmin_assoluto2(itip)*lsim_n(ii))/(1+perc_equi_min(itip))
	qmin_0(ii) = (qmin_assoluto(itip)*lsim_n(ii))/(1+perc_equi_min(itip))



	            qmin_0_tot=qmin_0_tot+qmin_0_2(ii)+qmin_0(ii)





      ii=ii+1


	    enddo

	qmin_0_2_type(itip)=(qmin_assoluto2(itip))/(1+perc_equi_min(itip))
      qmin_0_type(itip)=(qmin_assoluto(itip))/(1+perc_equi_min(itip))


      

	else

     		

	       do ij=1, type_quant(itip)

     
    
	qmin_0(ii) = (qmin_assoluto(itip)*lsim_n(ii))/(1+perc_equi_min(itip))

     

      qmin_0(ii)=qmin_0(ii)/aux3_type(itip)

   

	qmin_0_tot=qmin_0_tot+qmin_0(ii)



      ii=ii+1


	       enddo
	qmin_0_type(itip)=(qmin_assoluto(itip))/(1+perc_equi_min(itip))

      	
	endif

      enddo
     
     
      climite = flow/qmin_0_tot
		

      
cmar -----------------------
      do itip=1,ntp
      kcrit_min(itip)=k_min(itip)
	kcrit_max(itip)=k_max(itip)

	end do
cmar  ------ ------ --------

      if(ier.ne.0) then
	

!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1

cmar_ass_26

cmar_ass_26
      if(pp)then
cmar_ass_26
	 
c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26


	  a_err(istaz)=.true.
     


        return
      endif


      l_ric = .true.



        
      if (flow_tot.lt.qmin_tot_p) then

     

        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)     = qmin_assoluto(itip)
            qass2(itip)    = qmin_assoluto2(itip)
          endif
        enddo
        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            if (vinc_min(itip) .ne. sur_1f .and.
     *          vinc_min(itip) .ne. sur_2f     ) then
              l_ric = .false.
            end if
          end if
        end do
	  
	  goto 77

      else if (flow_tot.gt.qmax_tot_p) then
	
      do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_max(itip)
            qass(itip)     = qmax_assoluto(itip)
            qass2(itip)    = qmax_assoluto2(itip)
          endif
        enddo
        goto 77
      endif


c
c-preparazione common per il calcolo dei limiti effettivi
      do i=1,ntp
cmar_attenzione modifica 

	if(cmin(i).eq.0)then
cc        cmin_eff(i) = cmin(i)
      cmin_eff(i) = clim_new(i)
	else
      cmin_eff(i) = clim_new(i)
	endif
      

	if(cmax(i).eq.0)then
cc        cmax_eff(i) = cmax(i)
       cmax_eff(i) = caux(i)
	else
      cmax_eff(i) = cmax(i)
	endif
cccccc       cmax_eff(i) = caux(i)

	 


      enddo

c
      
      
cc
      qmin_tot_p=0
	qmax_tot_p=0

cc
      if(q1.eq.0)then
	
	m=2
	goto 19
	elseif(q2.eq.0)then
	
	m=1
	goto 19
	endif

cc
      
           do m=1, 2
cmar_ass_cond
      qmin_tot_(m)=0
	qmin_tot=0
	
      qmax_tot_(m)=0
	qmax_tot=0
      

19	comp_ratio= comp_ratio_(m)  	   
      exp_pol=exp_pol_(m)
      esp=esp_(m)
      z=z_(m)


      aux1=aux1_(m) 
      aux2=aux2_(m) 
      
      uno_su_ro_attuale_=uno_su_ro_attuale_(m)
      aux3=aux3_(m) 
      
	    
      head= head_(m)
	
     

cccccccccccccccccc	ntp=ntp_(m)

	  if(m.eq.1)then
	
	      do i=1, maxunits
	      vect(i)= ss_type(i)
     



	      enddo
	  else
            do i=1, maxunits
	      vect(i)=rr_type(i)


	      enddo
        endif
      
c           secondo
     

      

      call limiti_q_bifase_as(vect,head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)

	

     


      qmin_tot_p=qmin_tot_p+qmin_tot/aux3
      qmax_tot_p=qmax_tot_p+qmax_tot/aux3

   



	if(m.eq.1)then

      do k=1, ntp_1
      qmin_assoluto_1(k)=qmin_assoluto(k)
	qmax_assoluto_1(k)=qmax_assoluto(k)
	enddo
cmar_26_04_12
      else

	do k=ntp_1+1,ntp_1+ntp_2
	qmin_assoluto_2(k)=qmin_assoluto(k)
	qmax_assoluto_2(k)=qmax_assoluto(k)
	enddo
cmar_26_04_12

	endif
chiudo il do grande sulle m      
	enddo
    



	do k=1, ntp_1
      qmin_assoluto(k)=qmin_assoluto_1(k)
	qmax_assoluto(k)=qmax_assoluto_1(k)
      
	enddo
cc
cmar_26_04_12
      	do k=ntp_1+1,ntp_1+ntp_2
	qmin_assoluto(k)=qmin_assoluto_2(k)
	qmax_assoluto(k)=qmax_assoluto_2(k)
	enddo

cmar_26_04_12



     

      if(ier.ne.0) then
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1
cmar_ass_26

cmar_ass_26
      if(pp)then
cmar_ass_26

c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26

		a_err(istaz)=.true.
      

cmar_ass_cond_san  goto 111  
      return
      

      endif

	
			 

      if (flow_tot.lt.qmin_tot_p) then

		
c una macchina va imposta al minimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_min(vinc_crit_min).eq.sur_1f)  .or.
     *        (vinc_min(vinc_crit_min).eq.sur_2f))  then
cgpe-ric            ker = 21
            ker = 28
cgpe-ric-end
          else
            ker = 22
          endif

          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmin(vinc_crit_min)*type_actr(vinc_crit_min)
        if (type_bifase(vinc_crit_min)) then
         flow_ass=flow_ass+qmin2(vinc_crit_min)*type_actr(vinc_crit_min)
        endif
        type_actr(vinc_crit_min) = 0
        tip_vinc(vinc_crit_min)  = vinc_min(vinc_crit_min)
        qass(vinc_crit_min)      = qmin(vinc_crit_min)
        qass2(vinc_crit_min)     = qmin2(vinc_crit_min)
        goto 10
      else if (flow_tot.gt.qmax_tot_p) then
c una macchina va imposta al massimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_max(vinc_crit_max).eq.cho_1f)  .or.
     *        (vinc_max(vinc_crit_max).eq.cho_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Parabola'
cgpe-ric            ker = 24
            ker = 29
cgpe-ric-end
          elseif ((vinc_max(vinc_crit_max).eq.mar_1f)  .or.
     *            (vinc_max(vinc_crit_max).eq.mar_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Giri'
            ker = 23
          else

            ker = 25
          endif
cgpe-mess          ker = 94
          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmax(vinc_crit_max)*type_actr(vinc_crit_max)
        if (type_bifase(vinc_crit_max)) then
         flow_ass=flow_ass+qmax2(vinc_crit_max)*type_actr(vinc_crit_max)
        endif
        type_actr(vinc_crit_max) = 0
        tip_vinc(vinc_crit_max)  = vinc_max(vinc_crit_max)
        qass(vinc_crit_max)      = qmax(vinc_crit_max)
        qass2(vinc_crit_max)     = qmax2(vinc_crit_max)
        goto 10
      else
c simulazione normale delle macchine residue
c ricerca il clim per cui Qlavorato = flow_tot
c ricerca nell'intervallo vmin-vmax
c che diventa per ogni unita'' un intervallo clim_min-clim_max
      


cmar-eq2
          do itip=1,ntp

            if (type_actr(itip).gt.0) then
cc
      b=.true.
	type_actr_b(itip)=type_actr(itip)
cc 
              type_actr(itip) = 0
              tip_vinc(itip)  = 0

 
	   
	qass(itip)=climite*qmin_0_type(itip)

cmar_pow_stat
 
ccccccccccccccccccccc_312      min_p=min_p+type_vinc_minpow(itip)
ccccccccccccccccccccc_312     max_p=max_p+type_vinc_maxpow(itip)

cmar_pow_stat     


              if (type_bifase(itip)) then

	qass2(itip)=climite*qmin_0_2_type(itip)



c
              endif
            endif
          enddo

        endif

c
c
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, qass (o qass2), vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old

77    continue
     

c problema: dati head e qass per ogni tipo, calcolare le altre grandezze
c-bpcorr
c-l_ric      l_ric = .true.
c-bpcorr-end
      
cc_18_02 attenzione commento la pulizia sulle portate
      do i=1,unit_num
       q_icj(i)=0.
      enddo


      nutot = 0

       tot_cons1=0
       tot_cons2=0

      flow1=0
  	flow2=0

      f1=0.
      f2=0.

      do itip=1,ntp


     
cc
      if(itip.le.ntp_1)then

      comp_ratio= comp_ratio_(1)  	   
      exp_pol=exp_pol_(1)
      esp=esp_(1)
      z=z_(1)      
	aux1=aux1_(1) 
      aux2=aux2_(1)       
      uno_su_ro_attuale_=uno_su_ro_attuale_(1)
      aux3=aux3_(1) 	    
      head= head_(1)
      else
	
      comp_ratio= comp_ratio_(2)  	   
      exp_pol=exp_pol_(2)
      esp=esp_(2)
      z=z_(2)
      aux1=aux1_(2) 
      aux2=aux2_(2) 
      uno_su_ro_attuale_=uno_su_ro_attuale_(2)
      aux3=aux3_(2) 	    
      head= head_(2)
	
      endif
    



        if(type_act_old(itip).gt.0) then

          do ij =1,type_quant(itip)
            icj = nutot+ij
     
            if(status(icj)) then


cmar_08_05      
          n_calc=n_calc+1
cmar_08_05   
        
      unit_vcrit(icj)= climite

      
              call dati_compressore(head,qass(itip),qass2(itip),
     *                  qmax_assoluto(itip),qmax_assoluto2(itip),
     *                  qmin_assoluto(itip),qmin_assoluto2(itip),itip,
     *                  unit_bifase(icj),unit_head(icj),unit2_head(icj),
     *                  unit_rev(icj),unit2_rev(icj),unit_flow(icj),
     *                  unit2_flow(icj),unit_eff(icj),unit2_eff(icj),
     *                  unit_max(icj),unit_min(icj),unit2_max(icj),
     *                  unit2_min(icj),unit_eff_corr(icj),
     *			unit2_eff_corr(icj),unit_power_corr(icj),
     *                  unit2_power_corr(icj),unit_power(icj),
cgpe     *			unit2_power(icj),unit_hrate(icj),unit_cons(icj),
     *			unit2_power(icj),unit_hrate_corr(icj),
     *                  unit_hrate(icj),unit_cons(icj),
     *                  unit_vcrit(icj),unit_vinc(icj),tip_vinc(itip),
     *                  ier)

      


      q_icj(icj)=unit_flow(icj)/aux3_type(itip)
      
      
cc     

	if(icj.le.unit_1)then
	
    

c	flow1=flow1+q_icj(icj)  
      flow1=flow1+q_icj(icj)
	

     
	
cmar_ass_cond_rpt	   
	tot_cons1 = tot_cons1+unit_cons(icj)
   
      flowm_1=flowm_1+unit_flow(icj)
      
     
	else
	
      
cc      do i=unit_1,unit_2
      flow2=flow2+q_icj(icj)
      
	
	
cmar_ass_cond_rpt
	tot_cons2 = tot_cons2+unit_cons(icj)





	flowm_2=flowm_2+unit_flow(icj)
CMAR_ASS_COND
      IF(FLOWM_2.NE.0)THEN
	no_ACC (ISTAZ+1)=.false.
	ENDIF

      
     

	endif
  
cmar-eq2
    
    

	 if(unit_flow(icj).gt.qmax_assoluto(itip)  .OR.
     *        unit_flow(icj).lt.qmin_assoluto(itip)) then

	ier=1   

	
       
      
      endif
      

c controllo dell'errore in uscita dalla dati_compressore
    
              if (ier.gt.0) then
	  
!                if (.not.iopt) then
                if (.not.iopt .and. .not.iopt_mess) then

                  mess = ' '

                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                end if
!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                ier = 1

      	a_err(istaz)=.true.

 
cmar_ass_26
    

cmar_ass_26
      if(pp)then
cmar_ass_26

c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif
 
cmar_ass_26
ccccccccccccccccccccccccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      

      
                return
c-mf03
              else
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
     

cmar_08_05   

      IF ( N_CALC .EQ. N_SIM ) THEN

	FLAG_LAST =.TRUE.
      ENDIF
cmar_08_05 



cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)
cmar_pow_stat

                
			  pow_app = unit_power(icj) + unit2_power(icj)
     

                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))

   
	  
                if (fl_vinc .and. ier.gt.0) then
                  ier = 1

	
cmar_ass_01072013       	a_err(istaz)=.true.

  
cmar_ass_26

cmar_ass_26
cmar_ass_01072013       if(pp)then
cmar_ass_26


c	IER=0
cmar_ass_26
cmar_ass_01072013 	goto 117
cmar_ass_26
cmar_ass_01072013 	endif

cmar_ass_26
ccccccccccccccccccccccccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      
      


                  return
                else

cgpe                  if (ier .gt. 0) then
     
                  if (ier .gt. 0 .and. iopt) then
c                  if (ier .gt. 0 .and. iopt .and..not.iopt_last) then
                    unit_cons(icj) = unit_cons(icj) + soglia_cons

        

                  endif
                  if (ier .gt. 0) unit_vinc(icj) = ier
CMAR_ASS_COND
cmar_ass_01072013           
cmar_ass_01072013     
                         ier = 0
	 
cmar_ass_01072013 
                endif
c-mf03-end
              end if

            endif
          enddo
        endif
        nutot = nutot+type_quant(itip)
      enddo

            
      
      f1=flow1
 	f2=flow2
c
      tot_cons = 0.
      tot_flow = 0.
	tot_flow_n=0.
      

     

      do icj=1,unit_num
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)

	  tot_flow_n=tot_flow_n+q_icj(icj)





   

        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do

          
    

     
  
cc      flow_ric = tot_flow/aux3 ! portata compressa (contenente eventuale riciclo)
      flow_ric=flow1+flow2

       


      if (abs(flow-tot_flow_n).gt.eps_q) then
        if (tot_flow_n.lt.flow) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8

      	a_err(istaz)=.true.
    
cmar_ass_26

cmar_ass_26
      if(pp)then

cmar_ass_26


c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
		endif

cmar_ass_26

          if (iopt .or. iopt_mess) return

560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
          write(mess,560) udm_out_str(udm_Q,tot_flow_n,0),
     *                    udm_out_str(udm_Q,flow,1)
          call app_mess(vert_pointer,ier,mess)
        else if (tot_flow_n.gt.flow) then
!!          if (iopt .or. iopt_mess) return
!          if (iopt) return
          if (iopt) then
            if (.not.l_ric) ier = 20
		a_err(istaz)=.true.
      
ccccccccccccccccccccccccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      
      
	
            return
          endif
c-l_ric          if (.not.iopt_mess) then
            if (l_ric) then
              ier = 27

cmar_ass_26

      if(pp)then

c	IER=0
	goto 117
	endif

cmar_ass_26



		a_err(istaz)=.true.

      

	
            else
cstaz-corr
cstaz-corr-end
              ier = 20

cmar_ass_26

      if(pp)then



c	IER=0
	goto 117
	endif

cmar_ass_26



		a_err(istaz)=.true.
	


	
            endif
cgpe-new
            if (l_ric) then
c-l_ric
             if (.not.iopt_mess) then
c-l_ric-end
c-bpcorr		    ier = 0
cgpec portata di riciclo
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM              write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3, (flowm*conv_ns)/aux3
              write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                        udm_out_str(udm_Q,flowm/aux3,1)
              call app_mess(vert_pointer,ier,mess)
c-l_ric
             endif
c-l_ric-end
c-bpcorr
		    ier = 0
cmar_ass_cond_26 aprile



CMAR_03_MAGGIO    
       a_err(istaz)=.false.
cmar_ass_cond_26 aprile


c-bpcorr-end
            endif

        endif
      endif
cgpe-new
cgpe      if (iopt .or. iopt_mess) return
      if (ier .ne. 0) then

cmar_ass_26

      if(pp)then

c	IER=0
	goto 117
	endif

cmar_ass_26


	  if (iopt .or. iopt_mess) return
      endif
cgpe-new-end
cmar_ass_cond      do icj=1,unit_num
cmar_ass_cond        if(status(icj)) then
cmar_ass_cond          unit_perc(icj) = unit_flow(icj)/tot_flow
cmar_ass_cond           if (unit_bifase(icj)) then
cmar_ass_cond             unit2_perc(icj) = unit2_flow(icj)/tot_flow
cmar_ass_cond           endif
cmar_ass_cond         endif
cmar_ass_cond       enddo



      do icj=1,unit_1
        if(status(icj)) then
          unit_perc(icj) = unit_flow(icj)/flowm_1
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/flowm_1
          endif
        endif
      enddo

       do icj=unit_1+1, unit_1+unit_2
        if(status(icj)) then
          unit_perc(icj) = unit_flow(icj)/flowm_2
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/flowm_2
          endif
        endif
      enddo
               

c calcolo della temperatura di uscita della centrale
c      temp2 = 0.

    
cmar_ass_cond      call out_temper_mf_as(
cmar_ass_cond     *         tair,
cmar_ass_cond     *         temp2,temp1,Z,pin,pout,aux2,
cmar_ass_cond    *         z_coef(1,vert_pointer),
cmar_ass_condc-ace*         eff_coef,unit_num,status,
cmar_ass_cond     *         dh_coef(1,ind),cp_par(1,ind),unit_num,status,
cmar_ass_cond     *         unit_perc,unit_eff,
cmar_ass_cond     *         unit_flow,
cmar_ass_cond     *         unit_temp,unit2_perc,unit2_eff,
cmar_ass_cond     *         unit2_flow,
cmar_ass_cond     *         unit2_temp,unit_bifase,stat_vars(7))

cmar_ass_cond
cmar_ass_cond   assetto_1      

c      call out_temper_mf_as_OKK(
c     *         tair,
c     *         temp2,temp1,Z,pin,pout,aux2,
c     *         z_coef(1,vert_pointer),
c-ace*         eff_coef,unit_num,status,
c     *         dh_coef(1,ind),cp_par(1,ind),unit_num,status,
c     *         unit_perc,unit_eff,
c     *         unit_flow,
c     *         unit_temp,unit2_perc,unit2_eff,
c     *         unit2_flow,
c     *         unit2_temp,unit_bifase,stat_vars(7))
cmar_ass_cond
       temp2 = 0.
	 temp2_1=0.
	 temp2_2=0.
c-ele--->intervento su deltat aircooler
      CALL out_temper_mf_as(
     *         tair,
     *         temp2_1,temp1,Z_(1),pin_(1),pout_(1),aux2_(1),
     *         z_coef(1,vert_pointer),
     *         dh_coef(1,ind),cp_par(1,ind),unit_1,status,
     *         unit_perc,unit_eff,
     *         flowm_1,
     *         unit_temp,
     *         temp2_2,temp1,Z_(2),pin_(2),pout_(2),aux2_(2),
cmar     *         z_coef(1,vert_pointer),
cc     *         unit_perc,unit_eff,
     *         unit_2,flowm_2,
cmar     *         unit_temp_2,
     *         unit2_perc,unit2_eff,     
     *         unit2_flow,
     *         unit2_temp,unit_bifase,stat_vars(7),stat_vars_2(7))

cmar      temp2 = temp2 - stat_vars(5)
      
      

cmar_ass_cond_09052013

      IF  ( temp2_1.GT.0) THEN

      temp2_1=temp2_1- stat_vars(5)

	ENDIF


      IF  ( temp2_2.GT.0) THEN

      temp2_2=temp2_2- stat_vars_2(5)

	ENDIF


      
	temp2=temp2_1

    

cmar_ass_cond_1305213







C SE i tbg dell'assetto 1 non vengono utilizzati:

c  hp: metto comE Tout quella dell'assetto 2:
     




cmar_ass_cond_1305213      temp2_1=temp2_1- stat_vars(5)

cmar_ass_cond_1305213      temp2_2=temp2_2- stat_vars_2(5)

cmar_ass_cond_1305213	temp2=temp2_1

   

cgpe	call out_temper_ac_ce(tair,pout,tot_flow,temp2,stat_vars(7))


	call out_temper_ac_ce_as1_new(tair,pout_(1),flowm_1,stat_vars(7),
     *                           pin_(1),temp1,temp2_1)

	call out_temper_ac_ce_as2_new(tair,pout_(2),flowm_2,stat_vars_2(7),
     *                           pin_(2),temp1,temp2_2)

      	temp2=temp2_1
cmarrrrrrrrrrrrrrrrrrrrrrrrrrr
	IF ( temp2 .eq.0 ) then
	
	temp2=temp2_2
	
	ENDIF

	IF ( temp2_2 .eq.0 ) then
	
	temp2_2=temp2_1
	
	ENDIF
      
cmarrrrrrrrrrrrrrrrrrrrrrrrrr
     

	
cc      call out_temper_ac_ce(tair,pout,tot_flow,stat_vars(7),temp2)
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning

cmar_ass_cond_sposto la parte della temperatura


   
cmar_ass_cond_temp

cc qui c'era la fine del do di convergenza!

cc      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc	      enddo 


  
cmar_ass_cond
cmar_ass_cond_rpt      flowm_1=flow*aux3_type(ntp_1)
cmar_ass_cond_rpt  	flowm_2=flow*aux3_type(ntp_1+ntp_2)
     
CC 23-GEN
      
      comp_ratio=comp_ratio_(1)
	COMP_RATIO_2=comp_ratio_(2)




      flow_ric_1=flow1
      flow_ric_2=flow2
cmar_ass_cond_rpt
      if(flowm_1.eq.0)then
	comp_ratio=0
      flow_ric_1=0

	elseif(flowm_2.eq.0)then
	COMP_RATIO_2=0
	 flow_ric_2=0

      endif


cmar_ass_cond_rpt

	
CC 23-GEN
cmar_ass_cond_rpt_prova:
    

      i=1.

cccc      do j=1,24

     

      do j=first_unit_(vert_pointer),first_unit_(vert_pointer)+23
cmar_san      if(unit_avail_as(j).ne.0)then


       if(unit_avail_as(j).ne.0)then
       
      	
      
      
     

	unit_flow_(j-first_unit_(vert_pointer)+1)=unit_flow(i)
	
	

cc      unit_flow_(j)=unit_flow(i)
    

      unit_head_(j-first_unit_(vert_pointer)+1)=unit_head(i)
      unit_rev_(j-first_unit_(vert_pointer)+1)=unit_rev(i)
      unit_max_(j-first_unit_(vert_pointer)+1)=unit_max(i)
      unit_min_(j-first_unit_(vert_pointer)+1)=unit_min(i)
      unit_power_(j-first_unit_(vert_pointer)+1)=unit_power(i)
      unit_hrate_(j-first_unit_(vert_pointer)+1)=unit_hrate(i)
	unit_cons_(j-first_unit_(vert_pointer)+1)=unit_cons(i)
      unit_vcrit_(j-first_unit_(vert_pointer)+1)=unit_vcrit(i)
	unit_vinc_(j-first_unit_(vert_pointer)+1)=unit_vinc(i)
cc-23-01-11
      unit_temp_(j-first_unit_(vert_pointer)+1)=unit_temp(i)
      unit_perc_(j-first_unit_(vert_pointer)+1)=unit_perc(i)

cc-23-01-11
      unit_eff_(j-first_unit_(vert_pointer)+1)=unit_eff(i)

cmar_ass_cond: restano fuori=	 tip_vinc(itip),    ier)
      

     
CMAR
cmar_ass_Cond_14       STATUS_(j-first_unit_(vert_pointer)+1)=STATUS(i)

CMAR
	   i=i+1
	endif


      

	enddo

	


ccccc	
      do j=1,24
          

cccccccccc   	do j=first_unit_(vert_pointer),first_unit_(vert_pointer)+23
ccc      do j=first_unit_(vert_pointer) ,first_unit_(vert_pointer)+23

cmar_temp_prova     do j=13,36


         

cmar_15_05_2013        do j= 25,48

c       do j=first_unit_(vert_pointer),first_unit_(vert_pointer)+23

	unit_flow(j)=0
ccc      unit_bifase(j)=0
      unit_head(j)=0
      unit_rev(j)=0
      unit_eff(j)=0
      unit_max(j)=0
      unit_min(j)=0

      unit_power(j)=0

      unit_hrate(j)=0
      unit_cons(j)=0
      unit_vcrit(j)=0
	unit_vinc(j)=0
cc-23-01-11
      unit_temp(j)=0
	unit_perc(j)=0
cc-23-01-11

cmar_ass_Cond_14       STATUS(J)=0
    
	unit_flow(j)=unit_flow_(j)

	

  

ccc      unit_flow(j+first_unit_(vert_pointer))=unit_flow_(j)
     
      unit_head(j)=unit_head_(j)
      unit_rev(j)=unit_rev_(j)
      unit_eff(j)=unit_eff_(j)
      unit_max(j)=unit_max_(j)
      unit_min(j)=unit_min_(j)

      unit_power(j)=unit_power_(j)

      unit_hrate(j)=unit_hrate_(j)
	unit_cons(j)=unit_cons_(j)
      unit_vcrit(j)=unit_vcrit_(j)
	unit_vinc(j)=unit_vinc_(j)
cc-23-01-11
      unit_temp(j)=unit_temp_(j)
      unit_perc(j)=unit_perc_(j)
cc-23-01-11

cmar_ass_Cond_14      STATUS(J)=STATUS_(J)

	enddo



   
117     SIM(ISTAZ)=.TRUE.
ccc      SIM(ISTAZ)=.false.
	SIM_S=SIM(ISTAZ)
	



      

      if(  ( (flow1 - q1)/q1 .ge. 0.01).OR.
     *                  ( (flow2 - q2)/q2 .ge. 0.01) )then
	

      if(pp)then
	
	IER=0
	endif

	
      

	q1=flow1
	q2=flow2





	qmin_tot_p=0
	qmax_tot_p=0
cmar_ass_26 se passa di qua nn sarà almeno il giro iniziale +1
	pp=.false.


cmar_ass_26



cmar_ass_cond
cmar_ass_Cond_14       unit_num=unit_1+unit_2

cmar_ass_cond


      goto 100
	else 
	goto 20
	endif
      enddo
     


cmar_aggiunto  L'IF SUL CONSUMO

 	        
C            IF (TOT_CONS .GE. soglia_cons)THEN  
C              IER=1
C	        ENDIF
cmar_aggiunto : 


     
cmar_AC_auto
20       IF (.not.FLAG_AC)THEN
cmar_AC_auto

 
      if (iopt .or. iopt_mess) return
cgpe-end 

       if (temp2_1.gt.stat_vars(7)) then
cmar_ass_warning

      ass1t=.true.

cmar_ass_warning
        ker = 79




        write(mess,570) udm_out_str(udm_T,temp2_1,0),
     *                  udm_out_str(udm_T,stat_vars(7),1)
570   format('T1 man: ',A10,' - T man max: ',A25)
        call app_mess(vert_pointer,ker,mess)
	end if
      
cmar_ass_cond_temp
	   if (iopt .or. iopt_mess) return
cgpe-end
      if (temp2_2.gt.stat_vars_2(7)) then
cmar_ass_warning

      ass2t=.true.

cmar_ass_warning
        ker = 79




        write(mess,571) udm_out_str(udm_T,temp2_2,0),
     *                  udm_out_str(udm_T,stat_vars_2(7),1)
571   format('T2 man: ',A10,' - T man max: ',A25)
        call app_mess(vert_pointer,ker,mess)
	end if
      
    
cmar_AC_auto
	ENDIF
cmar_AC_auto
  

CMAR_OTT 2013
   
      IF (F1.EQ.0)THEN
	H1=0
	ENDIF

	IF(F2.EQ.0)THEN
	H2=0
	ENDIF

    


      return
      end





c...................................................................................







cmar*******************************************************************************
      subroutine Station_man_as(vert_pointer,flow,pres1,pres2,temp1,
     *     temp2,delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,nom_flow,
     *     fl_flow_man,
     *     unit_perc,unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c	simulazione di stazione con il nuovo criterio di controllo
c
c--------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
CMAR_ASS_COND
      	include '../inc/ASS_COND.INC'
CMAR_ASS_COND    




cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat
c

CMAR
      COMMON/MAR/istaz,ppmol
      INTEGER*4 istaz
	real*4 ppmol
c	integer*4 ier
CMAR
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
	common/equi_k/k_min(max_unit), k_max(max_unit)

cmar
cmar

      real*4 perc_equi_min, perc_equi_max
	real*4 k_min, k_max
cmar-eq2
cc
      COMMON/DP/dp


	

cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN
cc      
	INTEGER*4  lsim_n(maxunits) ,ii
	real*4 qmin_0(maxunits), qmin_0_tot, qmin_0_type(maxunits)
      real*4 qmin_0_2(maxunits), qmin_0_2_tot, qmin_0_2_type(maxunits)

cmar-eq2
cmar
c
      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4    nom_flow(*)
      real*4 flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),unit_rev(*),
     *	     unit_max(*),unit_min(*),unit_flow(*),unit_eff_corr(*),
     *	     unit_eff(*),unit_head(*),unit_power_corr(*),unit_power(*),
     *	     unit_hrate(*),unit_cons(*),unit_temp(*),stat_vars(*),
     *       delpr1,delpr2,flowm,head,unit_vcrit(*),
     *       unit_hrate_corr(*)
      integer*4 unit_vinc(*)
      real*4 unit2_perc(*),unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*),unit2_max(*),unit2_min(*)

      LOGICAL*2  unit_bifase(maxunits)
      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   pin,pout,exp_pol,esp,aux1
      real*4   flow_ass
c
      INTEGER*4  i,ier,itip,ker
c
      real*4   ra,rb,rd
      real*4   vcrit
      real*4   haux, raux
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   FLOW_APP
c --- new gz
      real*4    qmin_tot
      real*4    qmax_tot
      real*4    qmin(maxunits)
      real*4    qmin2(maxunits)
      real*4    qmax(maxunits)
      real*4    qmax2(maxunits)
      real*4    head_assoluto(maxunits)
      real*4    qmax_assoluto(maxunits)
      real*4    qmax_assoluto2(maxunits)
      real*4    qmin_assoluto(maxunits)
      real*4    qmin_assoluto2(maxunits)
      integer*4 vinc_min(maxunits)
      integer*4 vinc_max(maxunits)
      real*4    hmin,hmax
      real*4    qass(maxunits)
      real*4    qass2(maxunits)
      real*4    caux(maxunits)
      real*4    fa,fb
      real*4    climite
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 vinc_crit_min, vinc_crit_max
      real*4    vmin, vmax
      real*4    cmin(maxunits), cmax(maxunits)
      integer*4 icj,ij,nutot
      real*4    tot_flow
      real*4    zero_perc
      EXTERNAL  zero_perc
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      logical*2 l_ric
      real*4 tout_ac,delpr_f,delpr_ac
c-mf03
      real*4    pow_app
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18 
      real*4 ro_in,ro_out
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
      




      real*4 pin_(2), pout_(2), comp_ratio_(2),head_(2),exp_pol_(2),
     *  esp_(2),aux1_(2),aux3_(2),z_(2),uno_su_ro_attuale_(2),aux2_(2),
     *  ntp_(2)

	
      real*4 flowm_(2)

cc     * aux3_type(MAXUNITS),head_type(maxunits)
      real*4 pin_s, exp_pol_s,comp_ratio_s,z_s,qmin_tot_(2),qmax_tot_(2)

      real*4 unit_flow_(240),unit_bifase_(240),unit_head_(240),
     * unit_rev_(240),unit_eff_(240),unit_max_(240),unit_min_(240),
     *unit_eff_corr_(240),unit_power_corr_(240),unit_power_(240),
     *unit_hrate_corr_(240),unit_hrate_(240),unit_cons_(240),
     *unit_vcrit_(240), unit_vinc_(240),unit_temp_(240),unit_perc_(240),
     *STATUS_(240)
	
c	integer*4 j,m,vect(maxunits),k,nn
      integer*4 j,m,vect(24),k,nn

	real*4 aa(maxunits),  ss_primo, rr_primo

	integer*4 ss(maxunits),rr(maxunits)
	integer*4 SS_TYPE(maxunits),RR_TYPE(maxunits)
c-UDM-end

      real*4 qmin_tot_p,qmax_tot_p,qmin_assoluto_1(maxunits),
     * qmax_assoluto_1(maxunits),A,type_actr_b(maxunits),
cmar_12_01_12
     * qmin_assoluto_2(maxunits),qmax_assoluto_2(maxunits)
cmar_12_01_12
	REAL*4 NOM_1,NOM_2,NOM_TOT,p1,p2,q1,q2,df_1,df_2,dac_1,dac_2,
     * q_icj(maxunits),tot_flow_n
	logical*2 b,dp
c      logical*2 ass1, ass2
	real*4 qq1, qq2, qx1, qx2

cmar    temporaneamente:
       external  dp_in,dp_out
	real*4    dp_in,dp_out

	real*4 delpr1_1,delpr2_1,delpr1_2,delpr2_2
	real*4 delprf_1,delprf_2,delprac_1,delprac_2

CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI
	REAL*4 DP_AC_1_M,DP_AC_2_M
CMAR_AC_SOLO PER LA MFNE AD ASSETTI CONDI

cmar_Ass_26

	logical*2 pp

cmar_crit_man
      real*4 fl_flow_man(100)

      real*4 flow_man(maxunits),tot_flow_man
cmar_crit_man
	


cmar_08_05
      real*4 n_sim, n_calc
cmar_08_05

cmar_ass_26
cmar    temporaneamente:
cmar_ass_cond_temp

c      REAL*4 temp2_1,temp2_2


      COMMON/Tou_ass/T_staz

	real*4  T_staz

c-----------------------------------------------------------------------
      



	max_p=0
	min_p=0
	power_tot_staz=0
cmar_pow_stat
      flag_pow_cle = flag_pow(istaz)

cmar_08_05
	FLAG_LAST =.FALSE.

	A_P=0

CMAR    A_A è CONTENUTA NELLA POWER.INC ED è UTILIZZATA NELLA 
CMAR    FUNCTION IND_VIO_VINC


      n_sim=0
	N_CALC=0
cmar_08_05

cmar_pow_stat



cmar_DIN
      istaz=am
cmar_DIN


c--->
cmar_warning    
      ass1t= .false.
	ass2t= .false.
cmar_warning

cmar_Ass_26
      pp=.false.
cmar_Ass_26



cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312



cmar    

      delpr1=0.
	delpr2=0.
cmar    

cmar_ass_25
      flag_dati_s(ISTAZ)=.TRUE.
	flag_dati=flag_dati_s(ISTAZ)
cmar_ass_25

      a_err(istaz)=.false.
      


      dp=.true.
      b=.false.
      
      ier = 0

CMAR_ASS13	qmin_0_tot = 0

	qmin_0 = 0

cmar_ass13	ii=1

	qmin_0_2_tot = 0

	qmin_0_2 = 0
cc
      qmin_tot_p=0
	qmax_tot_p=0

cc
	flow1=0
	flow2=0
	flowm_1=0
	flowm_2=0
cc
cmar_ass_cond_rpt	 
       tot_cons1=0
       tot_cons2=0

cmar_ass_cond_rpt	 



      NTP_1=NTP_1_(ISTAZ)
	NTP_2=NTP_2_(ISTAZ)
      UNIT_1=UNIT_1_(ISTAZ)
	UNIT_2=UNIT_2_(ISTAZ)


c	flowm_1=0
c	flowm_2=0

cmar_ass_cond______21_maggio_13
      do i=1,24
	vect(i)=0
	enddo
      

cmar_ass_cond______21_maggio_13
cmar_ass_cond______23_maggio_13


      SS_TYPE(1:12)=0
	RR_TYPE(1:12)=0


       lsim_n(1:12)=0
       unit_flow(1:24)=0
      
cmar_ass_cond______23_maggio_13

      
 
	


      pin  = pres1
      pout = pres2




      	

	nom_tot=nom_1+nom_2

      
	p1=FL_FLOW_ASS(ISTAZ)

	p2=1-p1

      q1=flow*p1
	q2=flow*p2

      





cmar_Ass_26   pp= .t. solo al primo giro di do, qnd ho le portate di primo tentativo per i deltaP
      pp=.true.
cmar_Ass_26

     




100	 flowm_1=0
	flowm_2=0


cmar_ass13


         delpr1_1 = dp_in(istaz,pres1,temp1,q1)

	
	   delpr2_1 = dp_out(istaz,pres2,stat_vars(7),q1)
cmar   sul secondo assetto
	    delpr1_2 = dp_in(istaz+1,pres1,temp1,q2)

	
	   delpr2_2 = dp_out(istaz+1,pres2,stat_vars(7),q2)
   
   

     
c                       CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro1 (ntp_1,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,q1,delprf_1)

      call delpr_filtro2 (ntp_1,ntp_2,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,q2,delprf_2)
     
  

	
      IF( FLAG_AC )	THEN
      DP_AC_1_M=DP_AC_1
      DP_AC_2_M=DP_AC_2

      DP_AC_1=0
      DP_AC_2=0

      ENDIF



      call delpr_ac_p1 (ntp_1,type_quant,status,unit_bifase,nom_flow,
     *                 ac_ce_k,ac1_k,ac2_k,q1,delprac_1)

	  
      call delpr_ac_p2 (ntp_1,ntp_2,type_quant,status,unit_bifase,
     *                 nom_flow,ac_ce_k,ac1_k,ac2_k,q2,delprac_2)

      
      IF( FLAG_AC )	THEN
      DP_AC_1=DP_AC_1_M
      DP_AC_2=DP_AC_2_M
      ENDIF





		ii=1
      
      qmin_0_tot = 0

      



cc	                      
      dp_in_1=delpr1_1+delprf_1

      dp_in_2=delpr1_2+delprf_2

	dp_out_1=delpr2_1+delprac_1

	dp_out_2=delpr2_2+delprac_2



     
     
      if(b)then
	do itip=1,ntp
		type_actr(itip)=type_actr_b(itip)
          type_actr_b(itip)=0
      enddo
      endif
      do itip=1,ntp
		
          type_actr_b(itip)=0
      enddo




      

	pin_(1)=pin-dp_in_1

	pin_(2)=pin-dp_in_2

	pout_(1)=pout+dp_out_1

	pout_(2)=pout+dp_out_2
      
      


      if(q1.eq.0)then
	m=2
	goto 17
	elseif(q2.eq.0)then
	m=1
	goto 17
	endif

      
        do m=1, 2
      
      
 17      if ((pin_(m) .le. 0.).or. (pout_(m) .le. 0.)) ier = 38

      if (pout_(m) .lt. pin_(m)) ier = 5


      if (ier .gt. 0) then
        if (.not.(iopt .or. iopt_mess)) then
          call app_mess(vert_pointer,ier,mess)
        endif
cmar_Asss_san    
	a_err(istaz)=.true.
      
    
      return

     
cmar_Asss_san  goto 111



      endif
cgpe-corr-end
c-bpcorr

     

      if (.not.(iopt .or. iopt_mess)) then
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac	
      endif

c-bpcorr-end
cgpe-end
CMAR
      
      
      comp_ratio_(m) = pout_(m)/pin_(m)
	

      pin_s=pin_(m)
      comp_ratio_s=comp_ratio_(m)
      call POLITROP_ESP(istaz,pin_s,temp1,comp_ratio_s,exp_pol_s,ier)
      exp_pol_(m)=exp_pol_s

      esp_(m) = (exp_pol_(m)-1.)/exp_pol_(m)
      call zpt1(istaz,pin_s,temp1,z_s)
	z_(m)=z_s


      aux1_(m) = (erre / agrvcc / pmol) * Z_(m) * temp1/esp_(m)
      aux2_(m) = comp_ratio_(m)**esp_(m)
      head_(m) = aux1_(m) * (aux2_(m)-1.)    ! in km      
      uno_su_ro_attuale_(m)=(1/1.e5)*(erre/pmol)*Z_(m)*temp1/pin_(m)
      aux3_(m) = ROCN*uno_su_ro_attuale_(m)
      
      flowm = flow*aux3_(m)
      flowm_(m) = flow*aux3_(m)
      


      head=head_(m)
     

      ENDDO


C NUTOT DA SPOSTARE SUUUU

      nutot = 0
      do itip=1,ntp
        

	
      do ij =1,type_quant(itip)
          icj = nutot+ij
c           flow_man(icj) = flowm_(1)*fl_flow_man(icj)
CMAR_EVO_CRIT_MAN       flow_man(icj) = flow*fl_flow_man(icj)
CMAR_EVO_CRIT_MAN 
        

      if((nutot+1) .le. unit_1) then
        flowm = flow*aux3_(1)
	else
       flowm = flow*aux3_(2)

	endif

    


        flow_man(icj) = flowm*fl_flow_man(icj)


		

        enddo

        nutot = nutot+type_quant(itip)

        
      enddo


CMAR_ASS_COND_10_10_2013
      H1=head_(1)
	H2=head_(2)
CMAR_ASS_COND_10_10_2013

     

      
     
      DO I=1, NTP_1
      aux3_type(i)=AUX3_(1)
	head_type(i)=head_(1)


	ENDDO

      DO I= NTP_1+1, NTP_1+NTP_2
	aux3_type(i)=AUX3_(2)
	head_type(i)=head_(2)




	ENDDO

cc

      flow_ass = 0.
c carico headm nel common simpstat.INC per alcune routine zero_***
      headm = head
      do i=1,ntp
        head_assoluto(i) = head      
      enddo

      do i=1,unit_num
        lsim(i) = status(i)

     
cmar-eq2   vettore di interi che mi dice chi è acceso o spento per fare i conti

      
      if(lsim(i)) then
      lsim_n(i)= 1
	
cmar_08_05
	n_sim=n_sim+lsim_n(i)
cmar_08_05

	else
      lsim_n(i)= 0

      endif
      
cmar-eq2
      end do
      do i=1,ntp
        type_act_old(i)=type_actr(i)

      enddo
        
      do i=1,ntp

        cmin_eff(i) = clim_new(i)
        caux(i) = clim(4,i)

      enddo
     

CC    check di head
     
	


c---> calcolo gli estremi, in prevalenza (hmax,hmin), della stazione
      call limiti_h(hmin,hmax)

	
cmar_ass_cond      if (head.gt.hmax) then
     
      IF((head_(1).gt.hmax).OR.(head_(2).gt.hmax)) then
	 


c errore: prevalenza richiesta superiore alla massima fattibile
        ier = 6 ! ier = 5
       



cmar_ass_26      
      if(pp)then
cmar_ass_26	
    
c	IER=0
cmar_ass_26 	
      goto 117
cmar_ass_26 
 	endif

cmar_ass_26

		a_err(istaz)=.true.
 

!        if (iopt) return
       if (iopt .or. iopt_mess) return
		

cmar_ass_Cond_san       if (iopt .or. iopt_mess) goto 111

        if (fl_sim) then


c-UDM          write(mess,557) head,hmax,UDM_INT_H ! write(mess,557) head,hmax
c-UDM557       format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
557       format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
            write(mess,557) udm_out_str(udm_H,head,0),
     *                      udm_out_str(udm_H,hmax,1)
          call app_mess(vert_pointer,ier,mess)
        endif


cmar_ass_cond       else if (head.lt.hmin) then
      else if ((head_(1).lt.hmin).OR.(head_(2).lt.hmin)) then

c errore: prevalenza richiesta inferiore alla mimima fattibile
        ier = 7 ! ier = 6

cmar_ass_26
c     
   

cmar_ass_26
       if(pp)then
cmar_ass_26
	
c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26


	 	a_err(istaz)=.true.
	
	


!        if (iopt) return
         if (iopt .or. iopt_mess) return
cmar_ass_cond:_san       if (iopt .or. iopt_mess) goto 111

        if (fl_sim) then
c-UDM          write(mess,559) head,hmin,UDM_INT_H ! write(mess,559) head,hmin
c-UDM559       format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
559       format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
          write(mess,557) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmin,1)
          call app_mess(vert_pointer,ier,mess)
        endif


      endif

c
c10    flow_tot = (flowm-flow_ass)
 10      flow_tot = (flow-flow_ass)

 


      call calc_limiti(type_min_rev, type_max_rev, clim_new, caux)
c-preparazione common per il calcolo dei limiti estremi
c entro le mappe 
      do i=1,ntp
        cmin_eff(i) = clim_new(i)
        cmax_eff(i) = clim(4,i)
      
      enddo
c--->
cc        

	do i=1, UNIT_1
	ss(i)=lsim_n(i)     
	enddo


	DO I= 1, NTP_1
	SS_TYPE(I)=1

	ENDDO
c DA CONTROLLARE: SE è DA UNIT_1+1 A UNIT_2
	do i = UNIT_1,UNIT_2
	rr(i)=lsim_n(i)
	enddo
	DO I= NTP_1+1, NTP_1+NTP_2
	RR_TYPE(I)=1
	ENDDO

       
	 
cc
      if(q1.eq.0)then
	m=2
	goto 18
	elseif(q2.eq.0)then
	m=1
	goto 18
	endif


      

      do m=1, 2
	

cmar_ass_cond
18    qmin_tot_(m)=0
	qmin_tot=0
	
      qmax_tot_(m)=0
	qmax_tot=0

	



	comp_ratio= comp_ratio_(m)  	   
      exp_pol=exp_pol_(m)
      esp=esp_(m)
      z=z_(m)


      aux1=aux1_(m) 
      aux2=aux2_(m) 
      
      uno_su_ro_attuale_=uno_su_ro_attuale_(m)
      aux3=aux3_(m) 
      
	    
      head= head_(m)
      
      


	  if(m.eq.1)then

	      do i=1, maxunits
	
	      vect(i)= ss_type(i)
	
     



	      enddo
	  else
	
            do i=1, maxunits

	      vect(i)=rr_type(i)




	      enddo
        endif
      
c   primo
c   primo 
  

    
      call limiti_q_bifase_as(vect,head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)

		

      qmin_tot_p=qmin_tot_p+qmin_tot/aux3
      qmax_tot_p=qmax_tot_p+qmax_tot/aux3
     

		

	if(m.eq.1)then
		
cc      qq1=qmin_tot_p
cc	qx1=qmax_tot_p
      do k=1, ntp_1
      qmin_assoluto_1(k)=qmin_assoluto(k)
	qmax_assoluto_1(k)=qmax_assoluto(k)
      
	enddo
cmar_12_01_12
      else
     		
	do k=ntp_1+1,ntp_1+ntp_2
      qmin_assoluto_2(k)=qmin_assoluto(k)
	qmax_assoluto_2(k)=qmax_assoluto(k)

	enddo



	endif
		

chiudo il do grande sulle m      
	enddo
      

cc	qmin_tot_p=qq1+qq2
cc      qmax_tot_p=qx1+qx2

      		
	do k=1, ntp_1
      qmin_assoluto(k)=qmin_assoluto_1(k)
	qmax_assoluto(k)=qmax_assoluto_1(k)
    
	enddo
cc	       
cmar_12_01_12
    
	do k=ntp_1+1,ntp_1+ntp_2
      qmin_assoluto(k)=qmin_assoluto_2(k)
	qmax_assoluto(k)=qmax_assoluto_2(k)

	enddo

      do itip=1,ntp
      kcrit_min(itip)=k_min(itip)
	kcrit_max(itip)=k_max(itip)

	end do
cmar  ------ ------ --------

      if(ier.ne.0) then
	
C		write(117,*)'ier=',ier

!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1

cmar_ass_26

cmar_ass_26
      if(pp)then
cmar_ass_26
	 
c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26


	  a_err(istaz)=.true.
     


        return
      endif


      l_ric = .true.


C	write(117,*)'flow_tot=',flow_tot
C		write(117,*)'qmin_tot_p=',qmin_tot_p

        
      if (flow_tot.lt.qmin_tot_p) then

     


        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)     = qmin_assoluto(itip)
            qass2(itip)    = qmin_assoluto2(itip)
          endif
        enddo
        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            if (vinc_min(itip) .ne. sur_1f .and.
     *          vinc_min(itip) .ne. sur_2f     ) then
              l_ric = .false.
            end if
          end if
        end do
	  
	  goto 77

      else if (flow_tot.gt.qmax_tot_p) then
	
      do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_max(itip)
            qass(itip)     = qmax_assoluto(itip)
            qass2(itip)    = qmax_assoluto2(itip)
          endif
        enddo
        goto 77
      endif


c
c-preparazione common per il calcolo dei limiti effettivi
      do i=1,ntp
cmar_attenzione modifica del 26_04_12  - condizione all'aggiornamneto delle curve 
      
	if(cmin(i).eq.0)then
cc        cmin_eff(i) = cmin(i)
      cmin_eff(i) = clim_new(i)
	else
      cmin_eff(i) = clim_new(i)
	endif
      

	if(cmax(i).eq.0)then
cc        cmax_eff(i) = cmax(i)
       cmax_eff(i) = caux(i)
	else
      cmax_eff(i) = cmax(i)
	endif
cccccc       cmax_eff(i) = caux(i)

	 


      enddo

c
      
      
cc
      qmin_tot_p=0
	qmax_tot_p=0

cc
      if(q1.eq.0)then
	
	m=2
	goto 19
	elseif(q2.eq.0)then
	
	m=1
	goto 19
	endif

cc
      
           do m=1, 2
cmar_ass_cond
      qmin_tot_(m)=0
	qmin_tot=0
	
      qmax_tot_(m)=0
	qmax_tot=0
      

19	comp_ratio= comp_ratio_(m)  	   
      exp_pol=exp_pol_(m)
      esp=esp_(m)
      z=z_(m)


      aux1=aux1_(m) 
      aux2=aux2_(m) 
      
      uno_su_ro_attuale_=uno_su_ro_attuale_(m)
      aux3=aux3_(m) 
      
	    
      head= head_(m)
	
     

cccccccccccccccccc	ntp=ntp_(m)

	  if(m.eq.1)then
	
	      do i=1, maxunits
	      vect(i)= ss_type(i)
     



	      enddo
	  else
            do i=1, maxunits
	      vect(i)=rr_type(i)


	      enddo
        endif
      
c           secondo
     

      

      call limiti_q_bifase_as(vect,head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)

	

     


      qmin_tot_p=qmin_tot_p+qmin_tot/aux3
      qmax_tot_p=qmax_tot_p+qmax_tot/aux3

   



	if(m.eq.1)then

      do k=1, ntp_1
      qmin_assoluto_1(k)=qmin_assoluto(k)
	qmax_assoluto_1(k)=qmax_assoluto(k)
	enddo
cmar_26_04_12
      else

	do k=ntp_1+1,ntp_1+ntp_2
	qmin_assoluto_2(k)=qmin_assoluto(k)
	qmax_assoluto_2(k)=qmax_assoluto(k)
	enddo
cmar_26_04_12

	endif
chiudo il do grande sulle m      
	enddo
    



	do k=1, ntp_1
      qmin_assoluto(k)=qmin_assoluto_1(k)
	qmax_assoluto(k)=qmax_assoluto_1(k)
      
	enddo
cc
cmar_26_04_12
      	do k=ntp_1+1,ntp_1+ntp_2
	qmin_assoluto(k)=qmin_assoluto_2(k)
	qmax_assoluto(k)=qmax_assoluto_2(k)
	enddo

cmar_26_04_12



     

      if(ier.ne.0) then
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1
cmar_ass_26

cmar_ass_26
      if(pp)then
cmar_ass_26

c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif

cmar_ass_26

		a_err(istaz)=.true.
      

cmar_ass_cond_san  goto 111  
      return
      

      endif

	
			 

      if (flow_tot.lt.qmin_tot_p) then

		
c una macchina va imposta al minimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_min(vinc_crit_min).eq.sur_1f)  .or.
     *        (vinc_min(vinc_crit_min).eq.sur_2f))  then
cgpe-ric            ker = 21
            ker = 28
cgpe-ric-end
          else
            ker = 22
          endif

          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmin(vinc_crit_min)*type_actr(vinc_crit_min)
        if (type_bifase(vinc_crit_min)) then
         flow_ass=flow_ass+qmin2(vinc_crit_min)*type_actr(vinc_crit_min)
        endif
        type_actr(vinc_crit_min) = 0
        tip_vinc(vinc_crit_min)  = vinc_min(vinc_crit_min)
        qass(vinc_crit_min)      = qmin(vinc_crit_min)
        qass2(vinc_crit_min)     = qmin2(vinc_crit_min)
        goto 10
      else if (flow_tot.gt.qmax_tot_p) then
c una macchina va imposta al massimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_max(vinc_crit_max).eq.cho_1f)  .or.
     *        (vinc_max(vinc_crit_max).eq.cho_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Parabola'
cgpe-ric            ker = 24
            ker = 29
cgpe-ric-end
          elseif ((vinc_max(vinc_crit_max).eq.mar_1f)  .or.
     *            (vinc_max(vinc_crit_max).eq.mar_2f))  then
cgpe-mess            write(mess,558) vinc_crit_max, 'Giri'
            ker = 23
          else

            ker = 25
          endif
cgpe-mess          ker = 94
          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)

        end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmax(vinc_crit_max)*type_actr(vinc_crit_max)
        if (type_bifase(vinc_crit_max)) then
         flow_ass=flow_ass+qmax2(vinc_crit_max)*type_actr(vinc_crit_max)
        endif
        type_actr(vinc_crit_max) = 0
        tip_vinc(vinc_crit_max)  = vinc_max(vinc_crit_max)
        qass(vinc_crit_max)      = qmax(vinc_crit_max)
        qass2(vinc_crit_max)     = qmax2(vinc_crit_max)
        goto 10
      else
c simulazione normale delle macchine residue
c ricerca il clim per cui Qlavorato = flow_tot
c ricerca nell'intervallo vmin-vmax
c che diventa per ogni unita'' un intervallo clim_min-clim_max
      


cmar-eq2
          do itip=1,ntp

            if (type_actr(itip).gt.0) then
cc
      b=.true.
	type_actr_b(itip)=type_actr(itip)
cc 
              type_actr(itip) = 0
              tip_vinc(itip)  = 0

 




              if (type_bifase(itip)) then

	qass2(itip)=climite*qmin_0_2_type(itip)



c
              endif
            endif
          enddo

        endif

c
c
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, qass (o qass2), vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old

77    continue
     

c problema: dati head e qass per ogni tipo, calcolare le altre grandezze
c-bpcorr
c-l_ric      l_ric = .true.
c-bpcorr-end
      
cc_18_02 attenzione commento la pulizia sulle portate
      do i=1,unit_num
       q_icj(i)=0.
      enddo


      nutot = 0

       tot_cons1=0
       tot_cons2=0

      flow1=0
  	flow2=0

      f1=0.
      f2=0.

      do itip=1,ntp


     
cc
      if(itip.le.ntp_1)then

      comp_ratio= comp_ratio_(1)  	   
      exp_pol=exp_pol_(1)
      esp=esp_(1)
      z=z_(1)      
	aux1=aux1_(1) 
      aux2=aux2_(1)       
      uno_su_ro_attuale_=uno_su_ro_attuale_(1)
      aux3=aux3_(1) 	    
      head= head_(1)
      else
	
      comp_ratio= comp_ratio_(2)  	   
      exp_pol=exp_pol_(2)
      esp=esp_(2)
      z=z_(2)
      aux1=aux1_(2) 
      aux2=aux2_(2) 
      uno_su_ro_attuale_=uno_su_ro_attuale_(2)
      aux3=aux3_(2) 	    
      head= head_(2)
	
      endif
    

    
        

        if(type_act_old(itip).gt.0) then

	
          do ij =1,type_quant(itip)
            icj = nutot+ij
cmar_crit_man
     	qass(icj)=flow_man(icj)
cmar_crit_man
            if(status(icj)) then
 
      

cmar_08_05      
          n_calc=n_calc+1


      
                      call dati_compressore(head,qass(icj),qass2(icj),
     *             qmax_assoluto(itip),qmax_assoluto2(itip),
     *             qmin_assoluto(itip),qmin_assoluto2(itip),itip,
     *             unit_bifase(icj),unit_head(icj),unit2_head(icj),
     *             unit_rev(icj),unit2_rev(icj),unit_flow(icj),
     *             unit2_flow(icj),unit_eff(icj),unit2_eff(icj),
     *             unit_max(icj),unit_min(icj),unit2_max(icj),
     *             unit2_min(icj),unit_eff_corr(icj),
     *			 unit2_eff_corr(icj),unit_power_corr(icj),
     *             unit2_power_corr(icj),unit_power(icj),
     *			 unit2_power(icj),unit_hrate_corr(icj),
     *             unit_hrate(icj),unit_cons(icj),
     *             unit_vcrit(icj),unit_vinc(icj),tip_vinc(icj),
     *             ier)




      q_icj(icj)=unit_flow(icj)/aux3_type(itip)
      	

      
cc     

	if(icj.le.unit_1)then
	
    

c	flow1=flow1+q_icj(icj)  
      flow1=flow1+q_icj(icj)
	

     
	
cmar_ass_cond_rpt	   
	tot_cons1 = tot_cons1+unit_cons(icj)
   
      flowm_1=flowm_1+unit_flow(icj)
      
     
	else
	
      
cc      do i=unit_1,unit_2
      flow2=flow2+q_icj(icj)
      
	
	
cmar_ass_cond_rpt
	tot_cons2 = tot_cons2+unit_cons(icj)





	flowm_2=flowm_2+unit_flow(icj)
CMAR_ASS_COND
      IF(FLOWM_2.NE.0)THEN
	no_ACC (ISTAZ+1)=.false.
	ENDIF

      
     

	endif
  
cmar-eq2
    
    
      

c controllo dell'errore in uscita dalla dati_compressore
    
              if (ier.gt.0) then
	  
!                if (.not.iopt) then
                if (.not.iopt .and. .not.iopt_mess) then

                  mess = ' '

                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                end if
!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                ier = 1

      	a_err(istaz)=.true.

 
cmar_ass_26
    

cmar_ass_26
      if(pp)then
cmar_ass_26

c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
	endif
 
cmar_ass_26
ccccccccccccccccccccccccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      

      
                return
c-mf03
              else
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
     

cmar_08_05   

      IF ( N_CALC .EQ. N_SIM ) THEN

	FLAG_LAST =.TRUE.
      ENDIF
cmar_08_05 



cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)
cmar_pow_stat

                
			  pow_app = unit_power(icj) + unit2_power(icj)
     

                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))

   
	  
                if (fl_vinc .and. ier.gt.0) then
                  ier = 1


                  return
                else

cgpe                  if (ier .gt. 0) then
     
                  if (ier .gt. 0 .and. iopt) then
c                  if (ier .gt. 0 .and. iopt .and..not.iopt_last) then
                    unit_cons(icj) = unit_cons(icj) + soglia_cons

        

                  endif
                  if (ier .gt. 0) unit_vinc(icj) = ier


                         ier = 0
	 
cmar_ass_01072013 
                endif
c-mf03-end
              end if

            endif
          enddo
        endif
        nutot = nutot+type_quant(itip)
      enddo


	
	      
      
      f1=flow1
 	f2=flow2
c
      tot_cons = 0.
      tot_flow = 0.
	tot_flow_n=0.
      

     

      do icj=1,unit_num
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)

	  tot_flow_n=tot_flow_n+q_icj(icj)





   

        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do

          
    
C         	write(110,*)'TOT_CONS=',TOT_CONS

     
  
cc      flow_ric = tot_flow/aux3 ! portata compressa (contenente eventuale riciclo)
      flow_ric=flow1+flow2


       


      if (abs(flow-tot_flow_n).gt.eps_q) then
        if (tot_flow_n.lt.flow) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
C	write(117,*)'flow, tot_flow_n, ier=',flow, tot_flow_n, ier
      	a_err(istaz)=.true.
    
cmar_ass_26

cmar_ass_26
      if(pp)then

cmar_ass_26


c	IER=0
cmar_ass_26
	goto 117
cmar_ass_26
		endif

cmar_ass_26

          if (iopt .or. iopt_mess) return

560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
          write(mess,560) udm_out_str(udm_Q,tot_flow_n,0),
     *                    udm_out_str(udm_Q,flow,1)
          call app_mess(vert_pointer,ier,mess)
        else if (tot_flow_n.gt.flow) then
!!          if (iopt .or. iopt_mess) return
!          if (iopt) return
          if (iopt) then
            if (.not.l_ric) ier = 20
		a_err(istaz)=.true.

C		write(117,*)'ier=',ier


      
ccccccccccccccccccccccccxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx      
      
	
            return
          endif
c-l_ric          if (.not.iopt_mess) then



            if (l_ric) then
              ier = 27

cmar_ass_26

      if(pp)then

c	IER=0
	goto 117
	endif

cmar_ass_26



		a_err(istaz)=.true.

      

	
            else
cstaz-corr
cstaz-corr-end
              ier = 20



cmar_ass_26

      if(pp)then



c	IER=0
	goto 117
	endif

cmar_ass_26



		a_err(istaz)=.true.
	


	
            endif
cgpe-new
            if (l_ric) then
c-l_ric
             if (.not.iopt_mess) then
c-l_ric-end
c-bpcorr		    ier = 0
cgpec portata di riciclo
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM              write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3, (flowm*conv_ns)/aux3
              write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                        udm_out_str(udm_Q,flowm/aux3,1)
              call app_mess(vert_pointer,ier,mess)
c-l_ric
             endif
c-l_ric-end
c-bpcorr
		    ier = 0
cmar_ass_cond_26 aprile



CMAR_03_MAGGIO    
       a_err(istaz)=.false.
cmar_ass_cond_26 aprile


c-bpcorr-end
            endif

        endif
      endif
cgpe-new
cgpe      if (iopt .or. iopt_mess) return
      if (ier .ne. 0) then

cmar_ass_26

      if(pp)then

c	IER=0
	goto 117
	endif

cmar_ass_26


	  if (iopt .or. iopt_mess) return
      endif




      do icj=1,unit_1
        if(status(icj)) then
          unit_perc(icj) = unit_flow(icj)/flowm_1
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/flowm_1
          endif
        endif
      enddo

       do icj=unit_1+1, unit_1+unit_2
        if(status(icj)) then
          unit_perc(icj) = unit_flow(icj)/flowm_2
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/flowm_2
          endif
        endif
      enddo
               




       temp2 = 0.
	 temp2_1=0.
	 temp2_2=0.
c-ele--->intervento su deltat aircooler
      CALL out_temper_mf_as(
     *         tair,
     *         temp2_1,temp1,Z_(1),pin_(1),pout_(1),aux2_(1),
     *         z_coef(1,vert_pointer),
     *         dh_coef(1,ind),cp_par(1,ind),unit_1,status,
     *         unit_perc,unit_eff,
     *         flowm_1,
     *         unit_temp,
     *         temp2_2,temp1,Z_(2),pin_(2),pout_(2),aux2_(2),
cmar     *         z_coef(1,vert_pointer),
cc     *         unit_perc,unit_eff,
     *         unit_2,flowm_2,
cmar     *         unit_temp_2,
     *         unit2_perc,unit2_eff,     
     *         unit2_flow,
     *         unit2_temp,unit_bifase,stat_vars(7),stat_vars_2(7))

      
      

cmar_ass_cond_09052013

      IF  ( temp2_1.GT.0) THEN

      temp2_1=temp2_1- stat_vars(5)

	ENDIF


      IF  ( temp2_2.GT.0) THEN

      temp2_2=temp2_2- stat_vars_2(5)

	ENDIF


      
	temp2=temp2_1

    

cmar_ass_cond_1305213







   

cgpe	call out_temper_ac_ce(tair,pout,tot_flow,temp2,stat_vars(7))


	call out_temper_ac_ce_as1_new(tair,pout_(1),flowm_1,stat_vars(7)
     *                           ,pin_(1),temp1,temp2_1)

	call out_temper_ac_ce_as2_new(tair,pout_(2),flowm_2,stat_vars_2(7)
     *                           ,pin_(2),temp1,temp2_2)

      	temp2=temp2_1
cmarrrrrrrrrrrrrrrrrrrrrrrrrrr
	IF ( temp2 .eq.0 ) then
	
	temp2=temp2_2
	
	ENDIF

	IF ( temp2_2 .eq.0 ) then
	
	temp2_2=temp2_1
	
	ENDIF
      
cmarrrrrrrrrrrrrrrrrrrrrrrrrr
     

	
cc      call out_temper_ac_ce(tair,pout,tot_flow,stat_vars(7),temp2)
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning

      
      comp_ratio=comp_ratio_(1)
	COMP_RATIO_2=comp_ratio_(2)



      flow_ric_1=flow1
      flow_ric_2=flow2
cmar_ass_cond_rpt
      if(flowm_1.eq.0)then
	comp_ratio=0
      flow_ric_1=0

	elseif(flowm_2.eq.0)then
	COMP_RATIO_2=0
	 flow_ric_2=0

      endif


cmar_ass_cond_rpt

	
CC 23-GEN
cmar_ass_cond_rpt_prova:
    

      i=1.


  

      do j=first_unit_(vert_pointer),first_unit_(vert_pointer)+23


       if(unit_avail_as(j).ne.0)then
       
      	
      
      
     

	unit_flow_(j-first_unit_(vert_pointer)+1)=unit_flow(i)
	
	

    

      unit_head_(j-first_unit_(vert_pointer)+1)=unit_head(i)
      unit_rev_(j-first_unit_(vert_pointer)+1)=unit_rev(i)
      unit_max_(j-first_unit_(vert_pointer)+1)=unit_max(i)
      unit_min_(j-first_unit_(vert_pointer)+1)=unit_min(i)
      unit_power_(j-first_unit_(vert_pointer)+1)=unit_power(i)
      unit_hrate_(j-first_unit_(vert_pointer)+1)=unit_hrate(i)
	unit_cons_(j-first_unit_(vert_pointer)+1)=unit_cons(i)
      unit_vcrit_(j-first_unit_(vert_pointer)+1)=unit_vcrit(i)
	unit_vinc_(j-first_unit_(vert_pointer)+1)=unit_vinc(i)
cc-23-01-11
      unit_temp_(j-first_unit_(vert_pointer)+1)=unit_temp(i)
      unit_perc_(j-first_unit_(vert_pointer)+1)=unit_perc(i)

cc-23-01-11
      unit_eff_(j-first_unit_(vert_pointer)+1)=unit_eff(i)


CMAR
	   i=i+1
	endif


      

	enddo

	


ccccc	
      do j=1,24
          




c       do j=first_unit_(vert_pointer),first_unit_(vert_pointer)+23

	unit_flow(j)=0
ccc      unit_bifase(j)=0
      unit_head(j)=0
      unit_rev(j)=0
      unit_eff(j)=0
      unit_max(j)=0
      unit_min(j)=0

      unit_power(j)=0

      unit_hrate(j)=0
      unit_cons(j)=0
      unit_vcrit(j)=0
	unit_vinc(j)=0
cc-23-01-11
      unit_temp(j)=0
	unit_perc(j)=0
cc-23-01-11

    
	unit_flow(j)=unit_flow_(j)

c	write(111,*)'  3 do'
	

  

ccc      unit_flow(j+first_unit_(vert_pointer))=unit_flow_(j)
     
      unit_head(j)=unit_head_(j)
      unit_rev(j)=unit_rev_(j)
      unit_eff(j)=unit_eff_(j)
      unit_max(j)=unit_max_(j)
      unit_min(j)=unit_min_(j)

      unit_power(j)=unit_power_(j)

      unit_hrate(j)=unit_hrate_(j)
	unit_cons(j)=unit_cons_(j)
      unit_vcrit(j)=unit_vcrit_(j)
	unit_vinc(j)=unit_vinc_(j)
cc-23-01-11
      unit_temp(j)=unit_temp_(j)
      unit_perc(j)=unit_perc_(j)
cc-23-01-11

cmar_ass_Cond_14      STATUS(J)=STATUS_(J)

	enddo



   
117     SIM(ISTAZ)=.TRUE.
ccc      SIM(ISTAZ)=.false.
	SIM_S=SIM(ISTAZ)
	



      

      if(  ( (flow1 - q1)/q1 .ge. 0.01).OR.
     *                  ( (flow2 - q2)/q2 .ge. 0.01) )then
	

      if(pp)then
	
	IER=0
	endif

	
      

	q1=flow1
	q2=flow2



	qmin_tot_p=0
	qmax_tot_p=0
cmar_ass_26 se passa di qua nn sarà almeno il giro iniziale +1
	pp=.false.





      goto 100
	else 
	goto 20
	endif
c      enddo
     





     
cmar_AC_auto
20       IF (.not.FLAG_AC)THEN
cmar_AC_auto

 
      if (iopt .or. iopt_mess) return
cgpe-end 

       if (temp2_1.gt.stat_vars(7)) then
cmar_ass_warning

      ass1t=.true.

cmar_ass_warning
        ker = 79




        write(mess,570) udm_out_str(udm_T,temp2_1,0),
     *                  udm_out_str(udm_T,stat_vars(7),1)
570   format('T1 man: ',A10,' - T man max: ',A25)
        call app_mess(vert_pointer,ker,mess)
	end if
      
cmar_ass_cond_temp
	   if (iopt .or. iopt_mess) return
cgpe-end
      if (temp2_2.gt.stat_vars_2(7)) then
cmar_ass_warning

      ass2t=.true.

cmar_ass_warning
        ker = 79




        write(mess,571) udm_out_str(udm_T,temp2_2,0),
     *                  udm_out_str(udm_T,stat_vars_2(7),1)
571   format('T2 man: ',A10,' - T man max: ',A25)
        call app_mess(vert_pointer,ker,mess)
	end if
      
    
cmar_AC_auto
	ENDIF
cmar_AC_auto
  

CMAR_OTT 2013
   
      IF (F1.EQ.0)THEN
	H1=0
	ENDIF

	IF(F2.EQ.0)THEN
	H2=0
	ENDIF


 
c         enddo





      close(117)


    


      return
      end





c...................................................................................



cmar------------------------------------------



cmar---------------------------------------------------------------------------



		Subroutine out_temper_mf(
     *                  tair,
     *			      tout,tin,zin,pin,pout,aux2,z_coef,
     *        dh_coef,cp_par,unit_num,status,unit_perc,unit_eff,
     *                  unit_flow,
     *                  unit_temp,unit2_perc,unit2_eff,
     *                  unit2_flow,
     *                  unit2_temp,unit_bifase,
     *                  tmax_man)  !new

c***********************************************************************************
        implicit none
c
c	Calcola la temperatura di uscita di ogni compressore e 
c       della stazione
c**********************************************************************************
C
        include '../inc/RK_PARAM.inc'
        include '../inc/PARAM.inc'
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_num  I	total number of unit in the station
c	status    I	status of each unit
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
      real*4    tair
      REAL*4  dh_coef(*),cp_par(*)
c-ace REAL*4  eff_coef
	REAL*4	tout,tin,pin,zin,pout,aux2,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*),
     *          unit2_perc(*),unit2_eff(*),unit2_temp(*)
     *         ,unit_flow(*),unit2_flow(*),tmax_man
	INTEGER*4   unit_num
	LOGICAL*2   status(*)
c
        INTEGER*4   nu,i,nt
        real*4    temp,temp1,delta,deltat
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
        logical*2 unit_bifase(*)
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp
c---------------------------------------------------------------------
	nu = 0
	do i =1,unit_num
	  if(status(i)) then
c-ace
c-acec---->                         outlet theoretical temperature extimation
	      call single_out_temp_new(unit_temp(i),tin,zin,pin,
     *           pout,aux2,
     *           z_coef,dh_coef,cp_par,unit_eff(i))

c_ele 28/4/06
c	      call single_out_temp_ac(tair,pout,i,1,
c     *                        unit_flow(i),tmax_man,unit_temp(i))
cEvo2024	     
              write(LGU,*)'AC CALL 04'
            call single_out_temp_ac_new(tair,pout,i,1,
     *                unit_flow(i),tmax_man,unit_temp(i),pin,tin,deltat)
c_ele 28/4/06
c            tout = tout + unit_perc(i)*unit_temp(i)
            tout = tout + unit_perc(i)*(unit_temp(i)-deltat)
            if (unit_bifase(i)) then
c---->                         outlet theoretical temperature extimation
	        call single_out_temp_new(unit2_temp(i),tin,zin,pin,
     *           pout,aux2,
     *           z_coef,dh_coef,cp_par,unit2_eff(i))
c-ace
c_ele 28/4/06
c	         call single_out_temp_ac(tair,pout,i,2,
c     *                   unit2_flow(i),tmax_man,unit2_temp(i))
               write(LGU,*)'AC CALL 05'
	         call single_out_temp_ac_new(tair,pout,i,2,
     *            unit2_flow(i),tmax_man,unit2_temp(i),pin,tin,deltat)
c_ecol
c
c---->               station outlet temperature
c-ele 28/4               tout = tout + unit2_perc(i)*unit2_temp(i)
               tout = tout + unit2_perc(i)*(unit2_temp(i)-deltat)
            endif
	  end if
	end do
	return
	end

		Subroutine out_temper_mf_as_OKK(
     *                  tair,
     *			      tout,tin,zin,pin,pout,aux2,z_coef,
     *        dh_coef,cp_par,unit_num,status,unit_perc,unit_eff,
     *                  unit_flow,
     *                  unit_temp,unit2_perc,unit2_eff,
     *                  unit2_flow,
     *                  unit2_temp,unit_bifase,
     *                  tmax_man)  !new

c***********************************************************************************
        implicit none
c
c	Calcola la temperatura di uscita di ogni compressore e 
c       della stazione
c**********************************************************************************
C
        include '../inc/RK_PARAM.inc'
        include '../inc/PARAM.inc'
  
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_num  I	total number of unit in the station
c	status    I	status of each unit
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
      real*4    tair
      REAL*4  dh_coef(*),cp_par(*)
c-ace REAL*4  eff_coef
	REAL*4	tout,tin,pin,zin,pout,aux2,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*),
     *          unit2_perc(*),unit2_eff(*),unit2_temp(*)
     *         ,unit_flow(*),unit2_flow(*),tmax_man
	INTEGER*4   unit_num
	LOGICAL*2   status(*)
c
        INTEGER*4   nu,i,nt
        real*4    temp,temp1,delta,deltat
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
        logical*2 unit_bifase(*)
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp
c---------------------------------------------------------------------
	nu = 0
	do i =1,unit_num
	  if(status(i)) then
c-ace
c-acec---->                         outlet theoretical temperature extimation
	      call single_out_temp_new(unit_temp(i),tin,zin,pin,
     *           pout,aux2,
     *           z_coef,dh_coef,cp_par,unit_eff(i))

c_ele 28/4/06
c	      call single_out_temp_ac(tair,pout,i,1,
c     *                        unit_flow(i),tmax_man,unit_temp(i))
              write(LGU,*)'AC CALL 06'
	      call single_out_temp_ac_new(tair,pout,i,1,
     *                unit_flow(i),tmax_man,unit_temp(i),pin,tin,deltat)
c_ele 28/4/06
c            tout = tout + unit_perc(i)*unit_temp(i)
            tout = tout + unit_perc(i)*(unit_temp(i)-deltat)
            if (unit_bifase(i)) then
c---->                         outlet theoretical temperature extimation
	        call single_out_temp_new(unit2_temp(i),tin,zin,pin,
     *           pout,aux2,
     *           z_coef,dh_coef,cp_par,unit2_eff(i))
c-ace
c_ele 28/4/06
c	         call single_out_temp_ac(tair,pout,i,2,
c     *                   unit2_flow(i),tmax_man,unit2_temp(i))
               write(LGU,*)'AC CALL 07'
	         call single_out_temp_ac_new(tair,pout,i,2,
     *            unit2_flow(i),tmax_man,unit2_temp(i),pin,tin,deltat)
c_ecol
c
c---->               station outlet temperature
c-ele 28/4               tout = tout + unit2_perc(i)*unit2_temp(i)
               tout = tout + unit2_perc(i)*(unit2_temp(i)-deltat)
            endif
	  end if
	end do



	return
	end


			Subroutine out_temper_mf_as(
     *                  tair,
     *			      tout_1,tin_1,zin_1,pin_1,pout_1,aux2_1,z_coef,
     *        dh_coef,cp_par,unit_num1,status,unit_perc,unit_eff,
     *                  unit_flow_1,unit_temp,                  
     *			      tout_2,tin_2,zin_2,pin_2,pout_2,aux2_2,
     *                  unit_num2,
     *                  unit_flow_2,
cmar     *                  unit_temp_2,
     *         unit2_perc,unit2_eff,
     *                  unit2_flow,
     *                  unit2_temp,unit_bifase,
     *                  tmax_man,tmax_man2)  !new)


c***********************************************************************************
        implicit none
c
c	Calcola la temperatura di uscita di ogni compressore e 
c       della stazione
c**********************************************************************************
C
        include '../inc/RK_PARAM.inc'
        include '../inc/PARAM.inc'
  
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_num  I	total number of unit in the station
c	status    I	status of each unit
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
      real*4    tair
      REAL*4  dh_coef(*),cp_par(*)
c-ace REAL*4  eff_coef
	REAL*4	tout_1,tin_1,pin_1,zin_1,pout_1,aux2_1,z_coef(*),
     *        tout_2,tin_2,pin_2,zin_2,pout_2,aux2_2,
     *        unit_perc(*),unit_eff(*),unit_temp(*),
     *        unit2_perc(*),unit2_eff(*),unit2_temp(*),
c     *        unit_flow(*),
     *        unit2_flow(*),tmax_man,tmax_man2

c	real*4 unit_temp1(*),unit_temp2(*),
	
	real*4 unit_flow_1(*),unit_flow_2(*)

      INTEGER*4   unit_num1,unit_num2
	INTEGER*4   unit_num
	LOGICAL*2   status(*)
c
        INTEGER*4   nu,i,nt
        real*4    temp,temp1,delta,deltat
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
        logical*2 unit_bifase(*)
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp
c---------------------------------------------------------------------
	nu = 0

	do i =1,unit_num1
	  if(status(i)) then
c-ace
c-acec---->                         outlet theoretical temperature extimation
	      call single_out_temp_new(unit_temp(i),tin_1,zin_1,pin_1,
     *           pout_1,aux2_1,
     *           z_coef,dh_coef,cp_par,unit_eff(i))

c_ele 28/4/06
c	      call single_out_temp_ac(tair,pout,i,1,
c     *                        unit_flow(i),tmax_man,unit_temp(i))
              write(LGU,*)'AC CALL 08'
	      call single_out_temp_ac_new(tair,pout_1,i,1,
     *                        unit_flow_1(i),tmax_man,unit_temp(i),
     *                        pin_1,tin_1,deltat)
c_ele 28/4/06
c            tout = tout + unit_perc(i)*unit_temp(i)
            tout_1 = tout_1 + unit_perc(i)*(unit_temp(i)-deltat)



            if (unit_bifase(i)) then
c---->                         outlet theoretical temperature extimation
	        call single_out_temp_new(unit2_temp(i),tin_1,zin_1,pin_1,
     *           pout_1,aux2_1,
     *           z_coef,dh_coef,cp_par,unit2_eff(i))
c-ace
c_ele 28/4/06
c	         call single_out_temp_ac(tair,pout,i,2,
c     *                   unit2_flow(i),tmax_man,unit2_temp(i))
              write(LGU,*)'AC CALL 09'
              call single_out_temp_ac_new(tair,pout_1,i,2,
     *        unit2_flow(i),tmax_man,unit2_temp(i),pin_1,tin_1,deltat)
c_ecol
c
c---->               station outlet temperature
c-ele 28/4               tout = tout + unit2_perc(i)*unit2_temp(i)
               tout_1 = tout_1 + unit2_perc(i)*(unit2_temp(i)-deltat)
            endif
	  end if
	end do

cmar_ass_cond_rpt_temp

     	do i =unit_num1+1,unit_num1+unit_num2

	  if(status(i)) then
c-ace
c-acec---->                         outlet theoretical temperature extimation
	      call single_out_temp_new(unit_temp(i),tin_2,zin_2,pin_2,
     *           pout_2,aux2_2,
     *           z_coef,dh_coef,cp_par,unit_eff(i))

c_ele 28/4/06
c	      call single_out_temp_ac(tair,pout,i,1,
c     *                        unit_flow(i),tmax_man,unit_temp(i))
              write(LGU,*)'AC CALL 10'
	      call single_out_temp_ac_new(tair,pout_2,i,2,
     *                        unit_flow_2(i),tmax_man2,unit_temp(i),
     *                        pin_2,tin_2,deltat)
c_ele 28/4/06
c            tout = tout + unit_perc(i)*unit_temp(i)
            tout_2 = tout_2 + unit_perc(i)*(unit_temp(i)-deltat)

            if (unit_bifase(i)) then
c---->                         outlet theoretical temperature extimation
	        call single_out_temp_new(unit2_temp(i),tin_2,zin_2,pin_2,
     *           pout_2,aux2_2,
     *           z_coef,dh_coef,cp_par,unit2_eff(i))
c-ace
c_ele 28/4/06
c	         call single_out_temp_ac(tair,pout,i,2,
c     *                   unit2_flow(i),tmax_man,unit2_temp(i))
              write(LGU,*)'AC CALL 11'
	         call single_out_temp_ac_new(tair,pout_2,i,2,
     *        unit2_flow(i),tmax_man2,unit2_temp(i),pin_2,tin_2,deltat)
c_ecol
c
c---->               station outlet temperature
c-ele 28/4               tout = tout + unit2_perc(i)*unit2_temp(i)
               tout_2 = tout_2 + unit2_perc(i)*(unit2_temp(i)-deltat)
            endif
	  end if
	end do



	return
	end

      Real*4 Function zero_perc(vcrit)
c
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
      include '../inc/MF_SIMPSTAT.INC'
c
      integer*4 itip
      real*4   vcrit
      real*4   qaux,raux,haux
      real*4   qtot
      real*4   climite
c----------------------------------------------------------------------
      qtot = 0
      do itip=1,ntp
        if(type_actr(itip).gt.0) then
          climite = 1./((1./kcrit_min(itip)) + vcrit *
     *          ((1./kcrit_max(itip))-(1./kcrit_min(itip)) ) )
          call qdah_ch(climite,headm,qaux) 
          qtot = qtot + qaux*type_actr(itip)
          if (type_bifase(itip)) then
            call hdaq_ch(clim(3,itip),haux,qaux)
            if(headm.gt.haux) then
              call ndahq(headm,qaux,raux,chn(1,itip))
            else
              call ndahq(headm,qaux,raux,chc(1,itip))
            end if
            raux = raux*type_ratio(itip)/type2_ratio(itip)
            call hdan_ch(clim2(3,itip),haux,raux,chn2(1,itip))
            if(headm.gt.haux) then
              call qdahn(headm,qaux,raux,chn2(1,itip))
            else
              call qdahn(headm,qaux,raux,chc2(1,itip))
            end if
            qtot = qtot + qaux*type_actr(itip)
          endif
        end if
      end do
      zero_perc = flow_tot-qtot
      return
      end
      subroutine dati_compressore_ok_or(head,qass,qass2,qmax_assoluto,
     *                  qmax_assoluto2,qmin,qmin2,itip,unit_bifase,
     *                  unit_head,unit2_head,unit_rev,unit2_rev,
     *                  unit_flow,unit2_flow,unit_eff,unit2_eff,
     *                  unit_max,unit_min,unit2_max,unit2_min,
     *			unit_eff_corr,unit2_eff_corr,unit_power_corr,
     *			unit2_power_corr,unit_power,unit2_power,
     *                  unit_hrate_corr,
     *                  unit_hrate,unit_cons,unit_vcrit,unit_vinc,
     *                  tip_vinc,ier)
c
      implicit none
c
c  Dati la prevalenza e la portata, calcola tutte le altre grandezze
c  di stazione
c  per le unita' bifase, calcola anche i dati della seconda fase
c 
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/td.inc'
      include '../inc/cc.inc'

cmar 
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
cmar
     	common/equi_k/k_min(max_unit), k_max(max_unit)
cmar
cmar
      real*4 perc_equi_min, perc_equi_max

	real*4 k_min, k_max
cmar
c
      real*4    head,qass,qmax_assoluto
      real*4    unit_head,unit_rev,unit_flow,unit_eff,
     *          unit_eff_corr,unit_power_corr,unit_power,unit_hrate,
     *          unit_cons,unit_max,unit_min,
     *          unit_hrate_corr
      logical*2 unit_bifase
      real*4    qass2,qmax_assoluto2,qmin,qmin2
      real*4    unit2_head,unit2_rev,unit2_flow,unit2_eff,
     *          unit2_eff_corr,unit2_power_corr,unit2_power,
     *          unit2_max,unit2_min,unit_vcrit
      integer*4 unit_vinc,tip_vinc
      INTEGER*4 itip
      INTEGER*4 ier
      real*4    qaux
      real*4    qf,dpw
      logical*2 lhr
      real*4    pow_app, qf2
      real*4    fi0, fi1, fie, qad, had

cmar
      integer*4 i
c  	real*4 kcrit_max_e, kcrit_min_e
c-----------------------------------------------------------------------
      unit_flow = qass
      unit_head = head
      unit_max  = qass/qmax_assoluto
      unit_min  = qass/qmin
c
      if ( ((abs(qass-qmax_assoluto)).lt.eps_flow)    .or.
     *     ((abs(qass-qmin)).lt.eps_flow)           ) then
        if (tip_vinc.eq.3) then
          tip_vinc = 1
        else if (tip_vinc.eq.4) then
          tip_vinc = 2
        else if (tip_vinc.eq.8) then
          tip_vinc = 5
        else if (tip_vinc.eq.9) then
          tip_vinc = 6
        else if (tip_vinc.eq.10) then
          tip_vinc = 7
        end if
        unit_vinc = tip_vinc
      end if
c
cmar-------------------------------------

c      if (tipo_crit .eq. crit_equi) then
cmar   calcolare le parabole passanti dai punti min e max per l'Equidistanza

c        qmin = qmin/qmrif
c	  qmax_assoluto = qmax_assoluto/qmrif

c	  head = head / headrif
      
c        qmin= qmin*(1+perc_equi_min(itip) )
c	  qmax_assoluto= qmax_assoluto*(1-perc_equi_max(itip) )
c        kcrit_min_e=head/( qmin**2 )
c        kcrit_max_e=head/( qmax_assoluto**2 )

c        fi0 = 1/kcrit_min_e
c        fi1 = 1/kcrit_max_e
c        qad = qass / qmrif
c        had = head / headrif
c        fie = (qad*qad)/had
c        unit_vcrit = (fie-fi0)/(fi1-fi0)
c      end if

c        qmin = qmin*qmrif
c	  qmax_assoluto = qmax_assoluto*qmrif

c	  head = head*headrif
cmar-----------------------------------------------------
cmar -----------------------
c      do itip=1,ntp
c      kcrit_min(itip)=k_min(itip)
c	kcrit_max(itip)=k_max(itip)
c	end do
cmar  ------ ------ --------
cmar
      do i=1,ntp
	kcrit_min(i)=k_min(i)
	kcrit_max(i)=k_max(i)
      end do
cmar

      if (tipo_crit .eq. crit_equi) then
        fi0 = 1/kcrit_min(itip)
        fi1 = 1/kcrit_max(itip)
        qad = qass / qmrif
        had = head / headrif
        fie = (qad*qad)/had
        unit_vcrit = (fie-fi0)/(fi1-fi0)
      end if
cmar

      if (unit_bifase) then
        unit2_flow = qass2
        unit2_head = head
        unit2_max  = qass2/qmax_assoluto2
        unit2_min  = qass2/qmin2
      endif
c --1-- numero di giri
      call qdah_ch(clim(3,itip),head,qaux)
      if(qass.le.qaux) then
c zona normale
        call ndahq(head,qass,unit_rev,chn(1,itip))
      else
c zona di choking
        call ndahq(head,qass,unit_rev,chc(1,itip))
      endif
      call effdaqn(unit_eff,unit_flow,unit_rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
      unit_eff = unit_eff*unit_eff_corr
cgpe
      if (unit_eff .lt. 0.01) unit_eff = 0.01
cgpe-end
      qf = unit_flow/aux3
cgpe      unit_power= AGRVcc * rocn * head*qf/(unit_eff*unit_power_corr)
      unit_power= AGRVcc * rocn * head*qf/unit_eff
cgpe-end
c gz unit_power e' ora in Kjoule/ora. Conversione in KWatt:
      unit_power=unit_power/(3.6*1.e3)
      unit2_power = 0.
      if (unit_bifase) then
        call qdah_ch(clim2(3,itip),head,qaux)
        if(qass2.le.qaux) then
c zona normale
          call ndahq(head,qass2,unit2_rev,chn2(1,itip))
        else
c zona di choking
          call ndahq(head,qass2,unit2_rev,chc2(1,itip))
        endif
        call effdaqn(unit2_eff,unit2_flow,unit2_rev,cen2(1,itip),
     *               cec2(1,itip),clim2(1,itip))
        unit2_eff = unit2_eff*unit2_eff_corr
cgpe
        if (unit2_eff .lt. 0.01) unit2_eff = 0.01
cgpe-end
        qf2 = unit2_flow/aux3
cgpe        unit2_power= AGRVcc*rocn*head*qf2/(unit2_eff*unit2_power_corr)
        unit2_power= AGRVcc*rocn*head*qf2/unit2_eff
cgpe-end
        unit2_power=unit2_power/(3.6*1.e3)
      endif
c
      pow_app=(unit_power+unit2_power)
cc---->                                turbine simulation
      lhr = .true.
      ier = 0
cgpe      call turb(unit_rev,pow_app,unit_hrate,tair,cpwm(1,itip),
      call turb(itip,unit_rev,unit_power_corr,pow_app,unit_hrate_corr,
     *          unit_hrate,tair,cpwm(1,itip),
     *		  cpwt(1,itip),chr(1,itip),chrt(1,itip),dpw,delta,lhr,ier)
c   se unit_power e' in kW
c      unit_hrate e' in [kJ]/[kWh]
c      pclitd     e' in kJ/kNm3
cgpe      unit_cons = unit_hrate*pow_app/pclitd(ind)/4186.
      unit_cons = unit_hrate*(pow_app/unit_power_corr)/pclitd(ind)
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
cgpe      unit_power=unit_power*unit_power_corr
cgpe      unit2_power=unit2_power*unit2_power_corr

      return
      end
cmar-eq2-----------------------------------------------------------------
      subroutine dati_compressore(head,qass,qass2,qmax_assoluto,
     *                  qmax_assoluto2,qmin,qmin2,itip,unit_bifase,
     *                  unit_head,unit2_head,unit_rev,unit2_rev,
     *                  unit_flow,unit2_flow,unit_eff,unit2_eff,
     *                  unit_max,unit_min,unit2_max,unit2_min,
     *			unit_eff_corr,unit2_eff_corr,unit_power_corr,
     *			unit2_power_corr,unit_power,unit2_power,
     *                  unit_hrate_corr,
     *                  unit_hrate,unit_cons,unit_vcrit,unit_vinc,
     *                  tip_vinc,ier)
c
      implicit none
c
c  Dati la prevalenza e la portata, calcola tutte le altre grandezze
c  di stazione
c  per le unita' bifase, calcola anche i dati della seconda fase
c 
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/td.inc'
      include '../inc/cc.inc'

cmar 
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
cmar
     	common/equi_k/k_min(max_unit), k_max(max_unit)
cmar
cmar
      real*4 perc_equi_min, perc_equi_max

	real*4 k_min, k_max
cmar
c
      real*4    head,qass,qmax_assoluto
      real*4    unit_head,unit_rev,unit_flow,unit_eff,
     *          unit_eff_corr,unit_power_corr,unit_power,unit_hrate,
     *          unit_cons,unit_max,unit_min,
     *          unit_hrate_corr
      logical*2 unit_bifase
      real*4    qass2,qmax_assoluto2,qmin,qmin2
      real*4    unit2_head,unit2_rev,unit2_flow,unit2_eff,
     *          unit2_eff_corr,unit2_power_corr,unit2_power,
     *          unit2_max,unit2_min,unit_vcrit
      integer*4 unit_vinc,tip_vinc
      INTEGER*4 itip
      INTEGER*4 ier
      real*4    qaux
      real*4    qf,dpw
      logical*2 lhr
      real*4    pow_app, qf2
      real*4    fi0, fi1, fie, qad, had

cmar
      integer*4 i
c  	real*4 kcrit_max_e, kcrit_min_e
c-----------------------------------------------------------------------
      unit_flow = qass
      unit_head = head
      unit_max  = qass/qmax_assoluto
      unit_min  = qass/qmin
c
      if ( ((abs(qass-qmax_assoluto)).lt.eps_flow)    .or.
     *     ((abs(qass-qmin)).lt.eps_flow)           ) then
        if (tip_vinc.eq.3) then
          tip_vinc = 1
        else if (tip_vinc.eq.4) then
          tip_vinc = 2
        else if (tip_vinc.eq.8) then
          tip_vinc = 5
        else if (tip_vinc.eq.9) then
          tip_vinc = 6
        else if (tip_vinc.eq.10) then
          tip_vinc = 7
        end if
        unit_vinc = tip_vinc
      end if
c
cmar-eq2-------------------------------------


cmar  ------ ------ --------
cmar
      do i=1,ntp
	kcrit_min(i)=k_min(i)
	kcrit_max(i)=k_max(i)
      end do
cmar-eq2


      if (unit_bifase) then
        unit2_flow = qass2
        unit2_head = head
        unit2_max  = qass2/qmax_assoluto2
        unit2_min  = qass2/qmin2
      endif
c --1-- numero di giri
      call qdah_ch(clim(3,itip),head,qaux)
      if(qass.le.qaux) then
c zona normale
        call ndahq(head,qass,unit_rev,chn(1,itip))
      else
c zona di choking
        call ndahq(head,qass,unit_rev,chc(1,itip))
      endif
      call effdaqn(unit_eff,unit_flow,unit_rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
      unit_eff = unit_eff*unit_eff_corr
cgpe
      if (unit_eff .lt. 0.01) unit_eff = 0.01
cgpe-end
      qf = unit_flow/aux3
cgpe      unit_power= AGRVcc * rocn * head*qf/(unit_eff*unit_power_corr)
      unit_power= AGRVcc * rocn * head*qf/unit_eff
cgpe-end
c gz unit_power e' ora in Kjoule/ora. Conversione in KWatt:
      unit_power=unit_power/(3.6*1.e3)
      unit2_power = 0.
      if (unit_bifase) then
        call qdah_ch(clim2(3,itip),head,qaux)
        if(qass2.le.qaux) then
c zona normale
          call ndahq(head,qass2,unit2_rev,chn2(1,itip))
        else
c zona di choking
          call ndahq(head,qass2,unit2_rev,chc2(1,itip))
        endif
        call effdaqn(unit2_eff,unit2_flow,unit2_rev,cen2(1,itip),
     *               cec2(1,itip),clim2(1,itip))
        unit2_eff = unit2_eff*unit2_eff_corr
cgpe
        if (unit2_eff .lt. 0.01) unit2_eff = 0.01
cgpe-end
        qf2 = unit2_flow/aux3
cgpe        unit2_power= AGRVcc*rocn*head*qf2/(unit2_eff*unit2_power_corr)
        unit2_power= AGRVcc*rocn*head*qf2/unit2_eff
cgpe-end
        unit2_power=unit2_power/(3.6*1.e3)
      endif
c
      pow_app=(unit_power+unit2_power)
cc---->                                turbine simulation
      lhr = .true.
      ier = 0
cgpe      call turb(unit_rev,pow_app,unit_hrate,tair,cpwm(1,itip),
      
      call turb(itip,unit_rev,unit_power_corr,pow_app,unit_hrate_corr,
     *          unit_hrate,tair,cpwm(1,itip),
     *		  cpwt(1,itip),chr(1,itip),chrt(1,itip),dpw,delta,lhr,ier)
c   se unit_power e' in kW
c      unit_hrate e' in [kJ]/[kWh]
c      pclitd     e' in kJ/kNm3
cgpe      unit_cons = unit_hrate*pow_app/pclitd(ind)/4186.
      unit_cons = unit_hrate*(pow_app/unit_power_corr)/pclitd(ind)
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
cgpe      unit_power=unit_power*unit_power_corr
cgpe      unit2_power=unit2_power*unit2_power_corr

      return
      end

cmar-eq2-----------------------------------------------------------------

	REAL*4 Function zero_pow_mf(rev)
c
c  Calcola la differenza fra la potenza richiesta dal compressore e
c  la potenza fornita dalla turbina al numero di giri dato in input.
c
c  Tutti gli altri parametri sono forniti dal common "simpstat"
c----------------------------------------------------------------------
        implicit none
c 
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
	include '../inc/MF_SIMPSTAT.INC'
	include '../inc/cc.inc'
c
	LOGICAL*2 lhr
        real*4    rev,un_flow,un_eff,qf,un_power,hrate,dpw,haux
        real*4    rev2,un2_flow,un2_eff,qf2,un2_power
        INTEGER*4 ier
        real*4    pow_app
c-----------------------------------------------------------------
     



        call hdan_ch(clim(3,itipo),haux,rev,chn(1,itipo))
    
        if(headm.ge.haux) then

          call qdahn(headm,un_flow,rev,chn(1,itipo))
        else
          call qdahn(headm,un_flow,rev,chc(1,itipo))
        endif
c---->           efficiency
        call effdaqn(un_eff,un_flow,rev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))

     '

	un_eff = un_eff*eff_corr(itipo)
cgpe
      if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
	qf = un_flow/aux3
c---->          power required
cgpe        un_power= AGRVcc * rocn * headm*qf/(un_eff*power_corr(itipo))
        un_power= AGRVcc * rocn * headm*qf/un_eff
        un_power=un_power/(3.6*1.e3)
        un2_power=0.
        if (type_bifase(itipo)) then
          rev2 = rev*type_ratio(itipo)/type2_ratio(itipo)
          call hdan_ch(clim2(3,itipo),haux,rev2,chn2(1,itipo))
          if(headm.ge.haux) then
            call qdahn(headm,un2_flow,rev2,chn2(1,itipo))
          else
            call qdahn(headm,un2_flow,rev2,chc2(1,itipo))
          endif
          call effdaqn(un2_eff,un2_flow,rev2,cen2(1,itipo),
     *               cec2(1,itipo),clim2(1,itipo))
          un2_eff = un2_eff*eff_corr2(itipo)
cgpe
          if (un2_eff .lt. 0.01) un2_eff = 0.01
cgpe-end
          qf2 = un2_flow/aux3
cgpe          un2_power= AGRVcc * rocn * headm*qf2/
cgpe     *               (un2_eff*power_corr2(itipo))
          un2_power= AGRVcc * rocn * headm*qf2/un2_eff
          un2_power=un2_power/(3.6*1.e3)
        endif
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
        pow_app=(un_power+un2_power)
c
	lhr = .false.
cgpe	call turb(rev,pow_app,hrate,tair,

	call turb(itipo,rev,power_corr(itipo),pow_app,hrate_corr(itipo),
     *     hrate,tair,cpwm(1,itipo),cpwt(1,itipo),
     *	 chr(1,itipo),chrt(1,itipo),dpw,delta,lhr,ier)
	zero_pow_mf = dpw
	return
	end

	Subroutine comp_Station_mf(istaz,vert_pointer,tipo_criterio,
     *                        flow2,pres1,pres2,temp1,temp2,air,
     *                        comp_ratio,tot_cons,delpr1,delpr2,flowm,
     *                        flow_ric,type_num,unit_num,jufir,jtfir,
     *	                      stat_varsp,stat_ord,ier)
c
c***********************************************************************
c simulazione e recover di una stazione elementare tra quelle
c della multifunzione
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'

      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/mf_UNITS.INC'
c

cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess
c  ind  : indice della stazione nella struttura dati (common stazione.inc)
 
      INTEGER*4  istaz
      INTEGER*4  vert_pointer
      INTEGER*4  tipo_criterio
      INTEGER*4  stat_ord
      real*4   flow2, ! I flusso di uscita
     *         pres1,pres2,  ! pressioni di ingresso e uscita
     *         temp1,temp2,  ! 
     *         air,comp_ratio,tot_cons,
     *         delpr1,delpr2,flowm,flow_ric,delprint
c_ecol
cgpe     *         ,delpr_f,delpr_ac,unit_delpr_ac(maxunits)
c_ecol
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_varsp(*)
      INTEGER*4  ier
c
      real*4   mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4   mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2  lstat(maxunits)
c
      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
     *          VALSJC_OLD
      integer*4 I, J
      character*3 status_old(maxunits)
      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD,
     *          unit_eff_corr_old(maxunits),
     *          unit_power_corr_old(maxunits)
      integer*4 type_quant_old(maxunits)
      real*4    pres_int,temp_int,delt_int,deltat

	
	real*4 a

	 real*4      dp1_M,dp2_M



cmar_ac_gestione mess
c	integer*4 i
cmar_ac_gestione mess


c-----------------------------------------------------------------------
C---->  salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
cerr10    continue
c---->              assign zero all the station variable
      call init_simpstat_mf(istaz,vert_pointer,tipo_criterio,pres1,
     *          pres2,air,tot_cons,
cgpe     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,nit,mirev,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,mirev,
     *          marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_varsp(1))

      call init_compstat(istaz,vert_pointer,flow2,stat_varsp)
csave     *                   ,delprint)




CMAR_AC
C       QUI CAMBIO IL MODELLO, SIMULO E VEDO SE è IL CASO DI ACCENDERE L'AC PER RAFFREDDARE IL GAS

CMAR     IF1


      IF( ac_ce_stat.eq.1)	THEN



cmar_1809
      type_actr_M=type_actr
cmar_1809
      
	ac_ce_k_M=ac_ce_k
	ac1_k_M=ac1_k
c      a=stat_varsp(5)

	ac_ce_stat_M=ac_ce_stat

	ac1_stat_M=ac1_stat


CMAR   SPENGO L'AC
      ac_ce_k=0
      ac1_k=0
c      stat_varsp(5)=0


	ac_ce_stat=0

	ac1_stat=0

      FLAG_AC=.TRUE.




cmar_ac_02_10_13
       dp1_M=delpr1
       
	 dp2_M=delpr2

cmar_ac_02_10_13


c---->                                       simulazione stazione
      write(LGU,*)'CHIAMATA compstat_mf.....'
      call compstat_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),marev,mirev,
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
     *     unit_power(jufir),unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),unit_min(jufir),
     *     unit_vcrit(jufir),unit_vinc(jufir),stat_varsp,
     *     unit2_rev(jufir),unit2_flow(jufir),unit2_head(jufir),
     *     unit2_eff_corr(jufir),unit2_eff(jufir),
     *     unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),
c_ecol
     *     unit_delpr_ac(jufir),
c_ecol
     *     IER)


	


CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARSP(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M
 

       ac_ce_stat = ac_ce_stat_M

      ac1_stat	= ac1_stat_M
  
c      stat_varsp(5)=a

	FLAG_AC=.FALSE.


cmar_1809
      type_actr=type_actr_M
cmar_1809



cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_13







cmar_ac_gestione mess
c      mess=' '

      do i=1,mess_num(vert_pointer)
      mess_stri(i,vert_pointer)=''
	mess_type(i,vert_pointer)=0

	enddo


cmar_ac_gestione mess

	
c---->                                       simulazione stazione
      write(LGU,*)'compstat_mf : if TEMP2.GT.STAT_VARSP(7)'
      call compstat_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),marev,mirev,
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
     *     unit_power(jufir),unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),unit_min(jufir),
     *     unit_vcrit(jufir),unit_vinc(jufir),stat_varsp,
     *     unit2_rev(jufir),unit2_flow(jufir),unit2_head(jufir),
     *     unit2_eff_corr(jufir),unit2_eff(jufir),
     *     unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),
c_ecol
     *     unit_delpr_ac(jufir),
c_ecol
     *     IER)


            ENDIF
     




CMAR     IF1
       ELSE
CMAR     IF1


c---->                                       simulazione stazione
      write(LGU,*)'compstat_mf : else TEMP2.GT.STAT_VARSP(7)'
      call compstat_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),marev,mirev,
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
     *     unit_power(jufir),unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),unit_min(jufir),
     *     unit_vcrit(jufir),unit_vinc(jufir),stat_varsp,
     *     unit2_rev(jufir),unit2_flow(jufir),unit2_head(jufir),
     *     unit2_eff_corr(jufir),unit2_eff(jufir),
     *     unit2_power_corr(jufir),unit2_power(jufir),
     *     unit2_temp(jufir),
c_ecol
     *     unit_delpr_ac(jufir),
c_ecol
     *     IER)




CMAR     IF1
      ENDIF
CMAR     IF1





!      if (iopt) goto 11
cerr      if (iopt .or. iopt_mess) goto 11

cerrC---> gestione dell'errore
cerr      if (ier .gt. 0) then
!-simulazione KO
cerr        if (ier .le. 5 .or. ier .eq. 38) then
!-ier=1: non esistono punti fattibili sulle mappe dei compressori
!-ier=38: pressione di ingresso o di uscita nulle o negative
!-ier=4: flow negativo
!-ier=5: pressione di ingresso superiore alla pressione di uscita
!- Non e' possibile dare il punto dentro la mappa, ma e' stato imposto
!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cerr          goto 11

cgpe        else if (ier.eq.21 .or. ier.eq.23) then
cerr        else if (ier.eq.71 .or. ier.eq.73) then
cerr          if (nit . lt. nitmax) then
!- viene portato il punto dentro le mappe
!- le nuove condizioni di input alla station sono rappresentate da
!- PRES1/ PRES2/ FLOW2
cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if

cerr        else if (ier .eq. 8 .or. ier .eq. 20) then
!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
!- (sono stati calcolati i dati di unita')
cerr          if (nit .lt. nitmax) then
!- se l'errore deriva  , non sono state modificate le condizioni iniziali
!            return
cerr            goto 11

cerr          elseine compStat_mf
!- nel caso in cui l'errore derivi dalla correzione del punto, sono state
!- modificate le condizioni iniziali
cerr            goto 11
cerr          end if
cerr        endif
cerr      end if
c
cerr11    continue
c
C--->  ripristino delle condizioni iniziali
csave      call restore_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        ier,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
      return
      end
      Subroutine compStat_mf(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int,unit_temp_int,unit_delprint,
     *     unit_deltrint,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     type_quant,unit_num,status,nom_flow,max_rev,min_rev,
     *     unit_perc,
     *     unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_varsp,
     *     unit2_rev,unit2_flow,
     *     unit2_head,unit2_eff_corr,unit2_eff,unit2_power_corr,
     *     unit2_power,unit2_temp,
c_ecol
     *     unit_delpr_ac,
c_ecol
     *     IER) 
c
c	simulazione di stazione con il nuovo criterio di controllo
c
c--------------------------------------------------------------------------
      
      implicit none


c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/th.inc '	! :in
      include '../inc/tj.inc '	! :in/out
c
      include '../inc/simpstat.INC '	
      include '../inc/mf_simpstat.INC '	
      include '../inc/compstat.INC '	
      include '../inc/mf_limiti.INC '	
      include '../inc/PROTOTIPO.INC '	
      include '../inc/flag.INC '	
c-UDM
      include '../inc/conv.INC'
      include '../inc/err.INC'
c-UDM-end
c
cmar

cmar-eq2

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

      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
      real*4 delta_ac1(100)
      COMMON /DELTA/ delta_ac1
      real*4 delta_ac2(100)
      COMMON /DELTA/ delta_ac2
	INTEGER*4  lsim_n(maxunits) ,ii
	real*4 qmin_0(maxunits), qmin_0_tot, qmin_0_type(maxunits)
      real*4 qmin_0_2(maxunits), qmin_0_2_tot, qmin_0_2_type(maxunits)

cmar-eq2

      common/equi/perc_equi_min(max_unit),perc_equi_max(max_unit)
  	common/equi_k/k_min(max_unit), k_max(max_unit)

	real*4 k_min, k_max
	real*4 perc_equi_min, perc_equi_max

c	integer ii
cmar
      COMMON/MAR/istaz
      integer*4 istaz
      INTEGER CONT_ITER
      COMMON /CNTBLK/ CONT_ITER
      
      
	

cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat


      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4 presMandataCalc
      real*4 nom_flow(*)
      real*4 flow2,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       max_rev(*),min_rev(*),flow_ric,
     *       unit_perc(*),
     *       unit_rev(*),unit_max(*),unit_min(*),unit_vcrit(*),
     *	     unit_flow(*),unit_eff_corr(*),unit_eff(*),unit_head(*),
     *	     unit_power_corr(*),unit_power(*),unit_hrate(*),
     *	     unit_cons(*),unit_temp(*),stat_varsp(*),
     *       delpr1,delpr2,flowm,
     *       unit_hrate_corr(*)
     *       ,unit_delpr_ac(*)
c_ecol
     *       ,tout_ac
c_ecol
      integer*4 unit_vinc(*)
      real*4 unit_pres_int(*),unit_temp_int(*),unit_delprint(*)
      real*4 unit_deltrint(*)
      real*4 unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*)

      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   exp_pol,aux1
      real*4   flow_ass
c
      INTEGER*4  i,ier,itip,ker
c
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   FLOW_APP
c --- new gz
      real*4 qmin_tot, qmax_tot
      real*4 qmin(maxunits), qmax(maxunits)
      real*4 hmin(maxunits), hmax(maxunits)
      real*4 qmax_assoluto(maxunits), qmin_assoluto(maxunits)
      real*4 hmax_assoluto(maxunits), hmin_assoluto(maxunits)
      integer*4 vinc_min(maxunits), vinc_max(maxunits)
      real*4 qass(maxunits), hass(maxunits)
      integer*4 vinc_crit_min, vinc_crit_max
      real*4 vmin, vmax
      real*4 cmin(maxunits), cmax(maxunits)
      real*4 caux(maxunits)
      real*4 vcrit
!      real*4 percent
      real*4 qmin_tot_crit, qmax_tot_crit
      integer*4 flag
c
      real*4    qa,qb
      real*4    ra,rb,rd
      real*4    fa,fb
      real*4    climite
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 icj,ij,nutot
      real*4    tot_flow
      real*4    old_aux3
      real*4    zero_crit
      external  zero_crit
      real*4    eps_q/1.e-3/
      integer*4 nact   ! rappresenta il numero delle unita' attive meno
c      unit_act, che rappresenta il numero delle unita' quelle tali che
c      unit_avail = 2

      real*4    pres_old
      real*4    eps_p/0.05/

      real*4    EESP,QAUX
      real*4    p_min, p_max
      real*4    delpr_f,delpr_ac

!      real*4    h_min, h_max, q_min, q_max
!      integer*4 itip_min, itip_max
      real*4    pin_save, pout_save
C      integer*4 ier_save
      logical*2 l_ric
c-mf03
      real*4    pow_app
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18
      real*4 ro_in,ro_out,
c-ele 1/6/6
     *       deltat(maxunits)
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end
c-----------------------------------------------------------------------
        write(LGU,*)'INPUT vert_pointer:',vert_pointer
        write(LGU,*)'INPUT flow2:',flow2
        write(LGU,*)'INPUT pres1:',pres1
        write(LGU,*)'INPUT pres2:',pres2
        write(LGU,*)'INPUT temp1:',temp1
        write(LGU,*)'INPUT temp2:',temp2
        write(LGU,*)'INPUT delpr1:',delpr1
        write(LGU,*)'INPUT delpr2:',delpr2
c        write(LGU,*)'INPUT unit_delprint:',(unit_delprint(i), i=1,6)
         


	max_p=0
	min_p=0
	power_tot_staz=0


cmar_pow_stat
      flag_pow_cle = flag_pow(istaz)
cmar_pow_stat




cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312

cmar_pow_stat


      ier = 0

      qmin_0_tot = 0

	qmin_0 = 0

	ii=1

	qmin_0_2_tot = 0

	qmin_0_2 = 0
cmar-eq2
      tin = temp1
cmar-eq2
      pin  = pres1 - delpr1
      pout = pres2 + delpr2
cgpe-end
c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,flow,delpr_f)
c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p
      ro_in=1.
	delpr_f=delpr_f/ro_in
      pin = pin - delpr_f
c      pin  = pres1-delpr1

c             CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_s (ntp,type_quant,status,nom_flow,
     *                 ac_ce_k,ac2_k,flow,delpr_ac)
c-ace18
      call ro_real(ind,pout,temp1,ro_out)
cmar-p
      ro_out=1.
c      pout = pres2+delpr2
      delpr_ac=delpr_ac/ro_out
      pout = pout + delpr_ac



cgpe-corr
      if (pin .le. 0. .or. pout .le. 0.) ier = 38
      if (pout .lt. pin) ier = 5
      if (ier .gt. 0) then
        if (.not.(iopt .or. iopt_mess)) then
          call app_mess(vert_pointer,ier,mess)
        endif
        return
      endif
cgpe-corr-end

c-bpcorr
      if (.not.(iopt .or. iopt_mess)) then
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac
      endif
c-bpcorr-end



CMAR 
      comp_ratio = pout/pin
      call politrop_esp(istaz,pin,temp1,comp_ratio,exp_pol,ier)

CMAR      exp_pol = prop(pin,temp1,exp_coef(1,ind))
c esp1 e' nel common compstat.INC
      esp1 = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c zin e' nel common compstat.INC
CMAR      zin = prop(pin,temp1,z_coef(1,vert_pointer))
   
      
	call zpt1(istaz,pin,temp1,zin)
      aux1 = (erre / agrvcc / pmol) * Zin * temp1/esp1
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*temp1/pin
      aux3 = ROCN*uno_su_ro_attuale
      old_aux3 = aux3
      flowm = flow2*aux3
      flow_ass = 0.


c 
      do i=1,unit_num
        lsim(i) = status(i)
cmar-eq2   vettore di interi che mi dice chi è acceso o spento per fare i conti

c 

      if(lsim(i)) then
      lsim_n(i)= 1
	else
      lsim_n(i)= 0
      endif

cmar-eq2
      end do




      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo
c
      do i=1,ntp
        if (type_actr(i).gt.0) then
          caux(i) = clim(4,i)
        end if
      enddo
c
      call calc_limiti(type_min_rev, type_max_rev, clim_new, caux)
c-preparazione common per il calcolo dei limiti estremi
      do i=1,ntp
        if (type_actr(i).gt.0) then
          cmin_eff(i) = clim_new(i)
          cmax_eff(i) = clim(4,i)
        end if
      enddo
c
c calcolo i limiti minimi e massimi in portata per ciascun compressore e
c la portata minima e massima totale fattibile
C      ier_save = 0
C      if (.not.iopt .and. .not.iopt_mess) then
        qmin_tot = 0.
        qmax_tot = 0.
        do i = 1,ntp
          if (type_actr(i).gt.0) then
            write(LGU,*)'CSMF-Chiamata calc_punti_bifase......'
            call calc_punti_bifase(qmin_assoluto(i),qmax_assoluto(i),
     *           hmin_assoluto(i),hmax_assoluto(i),vinc_min(i),
     *           vinc_max(i),i,ier)
            write(LGU,*)'CSMF-cpb - qmin_assoluto(i): ',qmin_assoluto(i)  
            write(LGU,*)'CSMF-cpb - qmax_assoluto(i): ',qmax_assoluto(i)
            write(LGU,*)'CSMF-cpb - hmin_assoluto(i): ',hmin_assoluto(i)  
            write(LGU,*)'CSMF-cpb - hmax_assoluto(i):',hmax_assoluto(i)
            write(LGU,*)'CSMF-cpb - vinc_min(i): ',vinc_min(i)  
            write(LGU,*)'CSMF-cpb - vinc_max(i): ',vinc_max(i)

      do itip= 1, ntp


	       do ij=1, type_quant(itip)



	qmin_0(ii) = (qmin_assoluto(itip)*lsim_n(ii))/(1+perc_equi_min(itip))



	            qmin_0_tot=qmin_0_tot+qmin_0(ii)


      ii=ii+1


	    enddo
	qmin_0_type(itip)=(qmin_assoluto(itip))/(1+perc_equi_min(itip))
      write(LGU,*)'CSMF- qmin_assoluto(itip): ',qmin_assoluto(itip)
      write(LGU,*)'CSMF- perc_equi_min(itip): ',perc_equi_min(itip)
      write(LGU,*)'CSMF- qmin_0_type(itip): ',qmin_0_type(itip)

c	endif

      enddo

      write(LGU,*)'CSMF-VALORE INIZIALE flowm: ',flowm
      write(LGU,*)'CSMF-VALORE INIZIALE qmin_0_tot: ',qmin_0_tot
      climite = flowm/qmin_0_tot
      write(LGU,*)'CSMF-VALORE INIZIALE climite: ',climite
cmar-eq2
cmar-eq2-------------------------------------------------------------------------

cmar
c      do i=1,ntp        
c          cmin_eff(i) = clim_new(i)
c          cmax_eff(i) = clim(4,i)
c      enddo
cmar

       do ii=1,ntp
	kcrit_min(i)=k_min(i)
	kcrit_max(i)=k_max(i)
      end do

c - gestione errori:
            if (ier.ne.0) then
C              ier_save = ier
cgpe              if (ier.eq.12) then
              if (ier.eq.52) then
                ier = 1
                if (iopt .or. iopt_mess) return
                write(mess,561) i
C                call app_mess(vert_pointer,ier_save,mess)
                call app_mess(vert_pointer,ier,mess)
                return
              end if
561   format('Tipo di compressore: ',i3)
            endif
            qmin_tot = qmin_tot + qmin_assoluto(i)*type_actr(i)
            qmax_tot = qmax_tot + qmax_assoluto(i)*type_actr(i)
          end if
        end do



c
      CONT_ITER=0
c      write(LGU,*)'CSMF-VALORE CONT_ITER: ',CONT_ITER
10    flow_tot = (flowm-flow_ass)
      CONT_ITER = CONT_ITER + 1
      write(LGU,*)'CSMF-VALORE CONT_ITER: ',CONT_ITER
      l_ric = .true.
      write(LGU,*)'CSMF-VALORE flowm: ',flowm
      write(LGU,*)'CSMF-VALORE flow_ass: ',flow_ass
      write(LGU,*)'CSMF-VALORE flow_tot: ',flow_tot
      write(LGU,*)'CSMF-VALORE qmin_tot: ',qmin_tot
      write(LGU,*)'CSMF-VALORE qmax_tot: ',qmax_tot
      if (flow_tot.lt.qmin_tot) then



        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)     = qmin_assoluto(itip)
            hass(itip)     = hmin_assoluto(itip)
            write(LGU,*)'CSMF-VALORE MINASS qass(itip): ',qass(itip)
            write(LGU,*)'CSMF-VALORE MINASS hass(itip): ',hass(itip)
          endif
        enddo
        do itip=1,ntp
!        if (type_actr(itip).gt.0 .and. vinc_min(itip).ne.sur_1f) then
          if (type_actr(itip).gt.0) then
            if (vinc_min(itip) .ne. sur_1f .and.
     *          vinc_min(itip) .ne. sur_2f     ) then
              l_ric = .false.
            end if
          end if
        end do
        goto 77
      else if (flow_tot.gt.qmax_tot) then

        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_max(itip)
            qass(itip)     = qmax_assoluto(itip)
            hass(itip)     = hmax_assoluto(itip)
            write(LGU,*)'CSMF-VALORE MAXASS qass(itip): ',qass(itip)
            write(LGU,*)'CSMF-VALORE MAXASS hass(itip): ',hass(itip)
          endif
        enddo
        goto 77
      else
          l_ric = .false.
      endif
c
c calcolo delle portate minime e massime di ciascun compressore per il
c rispetto del criterio
c
      call calc_clim(vinc_min, vinc_max, qmax_assoluto, 
     *               hmax_assoluto, qmin_assoluto, hmin_assoluto,
     *               vinc_crit_min, vinc_crit_max, vmin, vmax,
     *               cmin, cmax, ier)
      if (ier.ne.0) then
!        if (iopt) return
        if (iopt .or. iopt_mess) return
        call app_mess(vert_pointer,ier,mess)
        ier = 1
        return
      endif
      write(LGU,*)'CSMF-DOPO calc_clim...'
c
c calcolo delle portate minime e massime totali per il rispetto del criterio
c
      call calc_limiti(type_min_rev,type_max_rev,cmin,cmax)
      write(LGU,*)'CSMF-DOPO calc_limiti...'
c
      do i = 1,ntp
        if (type_actr(i).gt.0) then
          qmin(i) = 0
          qmax(i) = 0
          hmin(i) = 0
          hmax(i) = 0
        end if
      end do
c
      qmin_tot_crit = 0.
      qmax_tot_crit = 0.
      do i = 1,ntp
        if (type_actr(i).gt.0) then
          itipo = i
          write(LGU,*)'CSMF-Chiamata q_clim per minimo...',i
          call q_clim(qsur_mar(i),qsur_mir(i),cmin(i),
     *         flag,qmin(i),hmin(i))
c          write(LGU,*)'CSMF-q_clim - qsur_mar(i): ',qsur_mar(i)
c          write(LGU,*)'CSMF-q_clim - qsur_mir(i): ',qsur_mir(i)
c          write(LGU,*)'CSMF-q_clim - cmin(i): ',cmin(i)
c          write(LGU,*)'CSMF-q_clim - flag: ',flag
c          write(LGU,*)'CSMF-q_clim - qmin(i): ',qmin(i)
c          write(LGU,*)'CSMF-q_clim - hmin(i): ',hmin(i)
          qass(i)=qmin(i)
          hass(i)=hmin(i)
          if ((flag.ne.0).and.(iopt)) then
              ier = 1
              return
          else if (flag.ne.0) then
!            if (.not.iopt) then
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_min(i).eq.sur_1f) .or.
     *            (vinc_min(i).eq.sur_2f))   then
cgpe-mess                write(mess,557) i, 'Parabola'
cgpe-ric            ker = 21
                ker = 28
cgpe-ric-end
              else
cgpe-mess                  write(mess,557) i, 'Giri'
                ker = 22
              endif
cgpe-mess557     format('Tipo vincolato:',i2,' - Tipo vincolo:',a)
cgpe-mess              ker = 93
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *           'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *           'Unita'' vincolata al minimo',0)
            end if
c Aggiorno flow_ass, type_actr e torno a 10
            flow_ass=flow_ass+
     *               qmin_assoluto(i)*type_actr(i)
c            write(LGU,*)'CSMF-min - flow_ass: ',flow_ass
c            write(LGU,*)'CSMF-min - qmin_assoluto(i): ',qmin_assoluto(i)
c            write(LGU,*)'CSMF-min - type_actr(i): ',type_actr(i)
c            write(LGU,*)'CSMF-min - vinc_min(i)=tip_vinc: ',vinc_min(i)
            tip_vinc(i) = vinc_min(i)
            qass(i)     = qmin_assoluto(i)
            hass(i)     = hmin_assoluto(i)
c            write(LGU,*)'CSMF-min - qass(i)=qmin_assoluto: ',qass(i)
c            write(LGU,*)'CSMF-min - hass(i)=hmin_assoluto: ',hass(i)
            qmin_tot = qmin_tot-qmin_assoluto(i)*type_actr(i)
            qmax_tot = qmax_tot-qmax_assoluto(i)*type_actr(i)
c            write(LGU,*)'CSMF-min - qmin_tot: ',qmin_tot
c            write(LGU,*)'CSMF-min - qmax_tot: ',qmax_tot
            type_actr(i) = 0
            if (CONT_ITER.lt.10000) goto 10
      end if
          write(LGU,*)'CSMF-Chiamata q_clim per massimo...',i
          call q_clim(qcho_mar(i),qcho_mir(i),cmax(i),
     *         flag,qmax(i),hmax(i))
c          write(LGU,*)'CSMF-q_clim - qcho_mar(i): ',qcho_mar(i)
c          write(LGU,*)'CSMF-q_clim - qcho_mir(i): ',qcho_mir(i)
c          write(LGU,*)'CSMF-q_clim - cmax(i): ',cmax(i)
c          write(LGU,*)'CSMF-q_clim - flag: ',flag
c          write(LGU,*)'CSMF-q_clim - qmax(i): ',qmax(i)
c          write(LGU,*)'CSMF-q_clim - hmax(i): ',hmax(i)
          qass(i)=qmax(i)
          hass(i)=hmax(i)
          if ((flag.ne.0).and.(iopt)) then
            ier = 1
            return
          else if (flag.ne.0) then
!            if (.not.iopt) then
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_max(i).eq.cho_1f)   .or.
     *            (vinc_max(i).eq.cho_2f))    then
cgpe-mess                write(mess,557) i, 'Parabola'
cgpe-ric            ker = 24
                ker = 29
cgpe-ric-end
              elseif ((vinc_max(i).eq.mar_1f)   .or.
     *                 (vinc_max(i).eq.mar_2f))   then
cgpe-mess                write(mess,557) i, 'Giri'
                ker = 23
              else
cgpe-mess                write(mess,557) i, 'Potenza'
                ker = 25
              endif
cgpe-mess              ker = 94
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *             'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *             'Unita'' vincolata al massimo',0)
            end if
c Aggiorno flow_ass, type_actr e torno a 10
            flow_ass=flow_ass+
     *               qmax_assoluto(i)*type_actr(i)
c           write(LGU,*)'CSMF-max - flow_ass: ',flow_ass
c            write(LGU,*)'CSMF-max - qmax_assoluto(i): ',qmax_assoluto(i)
c            write(LGU,*)'CSMF-max - type_actr(i): ',type_actr(i)
c            write(LGU,*)'CSMF-max - vinc_max(i)=tip_vinc: ',vinc_max(i)
            tip_vinc(i) = vinc_max(i)
            qass(i)     = qmax_assoluto(i)
            hass(i)     = hmax_assoluto(i)
c            write(LGU,*)'CSMF-max - qass(i)=qmax_assoluto: ',qass(i)
c            write(LGU,*)'CSMF-max - hass(i)=qmax_assoluto: ',hass(i)
            qmin_tot = qmin_tot-qmin_assoluto(i)*type_actr(i)
            qmax_tot = qmax_tot-qmax_assoluto(i)*type_actr(i)
c            write(LGU,*)'CSMF-max - qmin_tot: ',qmin_tot
c            write(LGU,*)'CSMF-max - qmax_tot: ',qmax_tot
            type_actr(i) = 0
            if (CONT_ITER.lt.10000) goto 10
          end if
          qmin_tot_crit = qmin_tot_crit + qmin(i)*type_actr(i)
          qmax_tot_crit = qmax_tot_crit + qmax(i)*type_actr(i)
c          write(LGU,*)'CSMF-qmin_tot_crit: ',qmin_tot_crit
c          write(LGU,*)'CSMF-qmax_tot_crit: ',qmax_tot_crit
        end if
      end do

c
      
      write(LGU,*)'CSMF-Dopo min-max - flow_tot: ',flow_tot
      if (flow_tot.lt.qmin_tot_crit) then
c una macchina va imposta al minimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        write(LGU,*)'CSMF-flow_tot. < qmin_tot_crit......' 
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_min(vinc_crit_min).eq.sur_1f) .or.
     *        (vinc_min(vinc_crit_min).eq.sur_2f))   then
cgpe-mess            write(mess,557) vinc_crit_min, 'Parabola'
cgpe-ric            ker = 21
            ker = 28
cgpe-ric-end
          else
cgpe-mess            write(mess,557) vinc_crit_min, 'Giri'
            ker = 22
          endif
cgpe-mess          ker = 93
          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)
cgpe-new          call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *         'Non si rispetta il criterio',0)
cgpe-new          call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *         'Unita'' vincolata al minimo',0)
        end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmin(vinc_crit_min)*type_actr(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - vinc_crit_min: ',vinc_crit_min
c        write(LGU,*)'CSMF-critMin - flow_ass: ',flow_ass
c        write(LGU,*)'CSMF-critMin - qmin(vinc_crit_min): '
c     *   ,qmin(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - type_actr(vinc_crit_min): '
c     *   ,type_actr(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - vinc_min(vinc_crit_min)=tip_vinc: '
c     *   ,vinc_min(vinc_crit_min)
        tip_vinc(vinc_crit_min)  = vinc_min(vinc_crit_min)
        qass(vinc_crit_min)      = qmin(vinc_crit_min)
        hass(vinc_crit_min)      = hmin(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - qass(vinc_crit_min)=qmin: '
c     *   ,qass(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - hass(vinc_crit_min)=hmin: '
c     *   ,hass(vinc_crit_min)
        qmin_tot = qmin_tot-qmin(vinc_crit_min)*
     *             type_actr(vinc_crit_min)
        qmax_tot = qmax_tot-qmax(vinc_crit_min)*
     *             type_actr(vinc_crit_min)
c        write(LGU,*)'CSMF-critMin - qmin_tot: ',qmin_tot
c        write(LGU,*)'CSMF-critMin - qmax_tot: ',qmax_tot
        type_actr(vinc_crit_min) = 0
        if (CONT_ITER.lt.10000) goto 10
      else if (flow_tot.gt.qmax_tot_crit) then
          write(LGU,*)'CSMF-flow_tot. > qmax_tot_crit......'
c una macchina va imposta al massimo (la piu' limitante secondo il criterio)
c si riprova con le altre
!        if (.not.iopt) then
        if (.not.iopt .and. .not.iopt_mess) then
          if ((vinc_max(vinc_crit_max).eq.cho_1f)   .or.
     *        (vinc_max(vinc_crit_max).eq.cho_2f))    then
cgpe-mess            write(mess,557) vinc_crit_max, 'Parabola'
cgpe-ric            ker = 24
            ker = 29
cgpe-ric-end
          elseif ((vinc_max(vinc_crit_max).eq.mar_1f)   .or.
     *            (vinc_max(vinc_crit_max).eq.mar_2f))   then
cgpe-mess            write(mess,557) vinc_crit_max, 'Giri'
            ker = 23
          else
cgpe-mess            write(mess,557) vinc_crit_max, 'Potenza'
            ker = 25
          endif
cgpe-mess          ker = 94
          mess = ' '
          ier = 0
          call app_mess(vert_pointer,ker,mess)
cgpe-new          call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *         'Non si rispetta il criterio',0)
cgpe-new          call gest_error (3,0,'COMPSTAT_MF',
cgpe-new     *         'Unita'' vincolata al massimo',0)
         end if
c Aggiorno flow_ass, type_actr e torno a 10
        flow_ass=flow_ass+qmax(vinc_crit_max)*type_actr(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - vinc_crit_max: ',vinc_crit_max
c        write(LGU,*)'CSMF-critMAX - flow_ass: ',flow_ass
c        write(LGU,*)'CSMF-critMAX - qmax(vinc_crit_max): '
c     *   ,qmax(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - type_actr(vinc_crit_max): '
c     *   ,type_actr(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - vinc_max(vinc_crit_max)=tip_vinc: '
c     *   ,vinc_max(vinc_crit_max)
        tip_vinc(vinc_crit_max)  = vinc_max(vinc_crit_max)
        qass(vinc_crit_max)      = qmax(vinc_crit_max)
        hass(vinc_crit_max)      = hmax(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - qass(vinc_crit_max)=qmax: '
c     *   ,qass(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - hass(vinc_crit_max)=hmax: '
c     *   ,hass(vinc_crit_max)
        qmin_tot = qmin_tot-qmin(vinc_crit_max)*
     *             type_actr(vinc_crit_max)
        qmax_tot = qmax_tot-qmax(vinc_crit_max)*
     *             type_actr(vinc_crit_max)
c        write(LGU,*)'CSMF-critMAX - qmin_tot: ',qmin_tot
c        write(LGU,*)'CSMF-critMAX - qmax_tot: ',qmax_tot
        type_actr(vinc_crit_max) = 0
        if (CONT_ITER.lt.10000) goto 10
      else
c simulazione normale delle macchine residue
c ricerca il valore di v per cui Qlavorato = flow_tot
c ricerca nell'intervallo vmin-vmax
c che diventa per ogni unita'' un intervallo clim_min-clim_max
c


cmar-eq2

        write(LGU,*)'CSMF-ELSE-NOCRIT....'
          do itip=1,ntp
            if (type_actr(itip).gt.0) then
              type_actr(itip) = 0
              tip_vinc(itip)  = 0

              
c ****************************************2026/02/04********************************************
c L'assegnazione di qass ed hass viene commentata a seguito delle Fix sui delta di pressione 
c e sul calcolo della pressione di uscita nella zero_giri. In questo modo qass ed hass risulteranno
c valorizzate con l'uscita dall'ultima iterazione dell'algoritmo di simulazione
c *******************************************************************************************
	qass(itip)=climite*qmin_0_type(itip)
c	hass(itip)=hmin_assoluto(itip)
c *******************************************************************************************
      write(LGU,*)'CSMF-NOCRIT - climite: ',climite
      write(LGU,*)'CSMF-NOCRIT - qmin_0_type(itip):',qmin_0_type(itip)
      write(LGU,*)'CSMF-NOCRIT - qass(itip):',qass(itip)
      write(LGU,*)'CSMF-NOCRIT - hass(itip):',hass(itip)     
            endif
          enddo

        endif
      if(CONT_ITER.ge.10000) then
          qass(itip)=climite*qmin_0_type(itip)
          call tipo_err(istaz,err_ww,'')
      end if
cmar-eq2      endif
c
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, flow (prima fase) vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old
77    continue
c problema: dati hass e qass per la prima fase di ogni tipo, calcolare 
c le altre grandezze
c-bpcorr
c-l_ric      l_ric = .true.
c-bpcorr-end
      write(LGU,*)'****** CSMF-FINE SIMULAZIONE ***** '

cmar_pow_stat
      nutot = 0
      do itip=1,ntp
        if(type_act_old(itip).gt.0) then
          do ij =1,type_quant(itip)
            icj = nutot+ij
            if(status(icj)) then
cmar_pow_stat
ccccc_312      min_p=min_p+type_vinc_minpow(itip)
ccccc_312      max_p=max_p+type_vinc_maxpow(itip)
cmar_pow_stat
            endif
	     enddo
	    endif
      enddo

cmar_pow_stat


      nutot = 0
      do itip=1,ntp
        if(type_act_old(itip).gt.0) then
          do ij =1,type_quant(itip)
            icj = nutot+ij


	unit_vcrit(icj) = climite


            if(status(icj)) then
              write(LGU,*)'VALORE ntp: ',ntp
              write(LGU,*)'VALORE type_quant(itip):',type_quant(itip)
              write(LGU,*)'ITER itip: ',itip
              write(LGU,*)'INPUT hass : ',hass(itip)
              write(LGU,*)'INPUT qass : ',qass(itip)
              write(LGU,*)'INPUT dp_int_aux : ', delta_in_pressione(icj)             
              write(LGU,*)'INPUT dt_int_aux : ',unit_deltrint(icj)
              write(LGU,*)'VALORE unit_num:',unit_num
 
             call dati_compress2(hass(itip),qass(itip),
     *                qmax_assoluto(itip),qmin_assoluto(itip),
     *                tip_vinc(itip),itip,
c_ecol
     *                icj,
c_ecol
     *                unit_head(icj),
     *                unit2_head(icj),unit_rev(icj),unit2_rev(icj),
     *                unit_flow(icj),unit2_flow(icj),unit_eff(icj),
     *                unit2_eff(icj),unit_temp(icj),unit2_temp(icj),
     *                unit_max(icj),unit_min(icj),unit_vcrit(icj),
     *                unit_vinc(icj),unit_eff_corr(icj),
     *                unit2_eff_corr(icj),unit_power_corr(icj),
     *                unit2_power_corr(icj),unit_power(icj),
cgpe     *	              unit2_power(icj),unit_hrate(icj),unit_cons(icj),
     *	              unit2_power(icj),unit_hrate_corr(icj),
     *                unit_hrate(icj),unit_cons(icj),
c_ecol
     *                ac1_k(icj),
c_ecol
     *                unit_pres_int(icj),unit_temp_int(icj), 
     *                delta_in_pressione(icj), unit_deltrint(icj), 
     *                unit_delpr_ac(icj),
     *                stat_varsp, ier)

                      write(LGU,*)'INPUT dp_ac1 : ', delta_ac1(icj)

cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)+unit2_power(icj)  
cmar_pow_stat



c-ele 28/4/06
c                      write(LGU,*)'AC CALL 12'
                   if(pres_for_fase2 .eq.0.0) then
                      pres_for_fase2 = press_out_calc(itip)
                      temp_for_fase2 = unit2_temp(icj)
                  end if
                   call single_out_temp_ac_new(tair,press_out_calc(itip)
     *              ,icj,2,
     *                  unit2_flow(icj),stat_varsp(7),unit2_temp(icj),
c-ele 1/6/6     *                  ,deltat)
     *    pres_for_fase2 - delta_in_pressione(icj) - delta_ac1(icj),
     *             temp_for_fase2,deltat(icj))
c 01/12/2025
c ******** ATTENZIONE SOSTITUZIONE PRESS_FOR_FASE2 *********
c-bpcorr
c-l_ric              if (unit_vinc(icj) .ne. sur_1f .and.
c-l_ric     *            unit_vinc(icj) .ne. sur_2f     ) then
c-l_ric                l_ric = .false.
c-l_ric              end if
c-bpcorr-end

c controllo dell'errore in uscita dalla dati_compress2
              if (ier.gt.0) then
!                if (.not.iopt) then
                if (.not.iopt .and. .not.iopt_mess) then
cgpe-new                  call gest_error (2,0,'COMPSTAT_MF',
cgpe-new     *             ' Violata la max_power',0)
                  mess = ' '
cgpe-mess                  ker = 14
                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                end if

!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                ier = 1
                return
c-mf03
              else
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
                pow_app = unit_power(icj) + unit2_power(icj)
                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))
                if (fl_vinc .and. ier.gt.0) then
                  ier = 1
                  return                  
                else
c                  unit_cons(icj) = unit_cons(icj) + soglia_cons
                  if (ier .gt. 0 .and. iopt) then
                    unit_cons(icj) = unit_cons(icj) + soglia_cons
                  endif
                  if (ier .gt. 0) unit_vinc(icj) = ier
                  ier = 0
                endif
c-mf03-end
              end if
c-bpcorr
cgpe              if (unit_vinc(icj) .ne. sur_1f .and.
cgpe     *            unit_vinc(icj) .ne. sur_2f     ) then
cgpe                l_ric = .false.
cgpe              end if
c-bpcorr-end
            endif
          enddo
        endif
        nutot = nutot+type_quant(itip)
      enddo
c
      tot_cons = 0.
      tot_flow = 0.
      do icj=1,unit_num
c          write(LGU,*)'VALORE unit_flow - ICJ:',icj
c          write(LGU,*)'VALORE unit_flow(icj):',unit_flow(icj)
          tot_flow = tot_flow+unit_flow(icj)
          tot_cons = tot_cons+unit_cons(icj)

      end do




      if (.not.iopt) then
!-26/05/2000
        temp2 = 0.
!-26/05/2000-end
        do icj=1,unit_num
!          percent = unit_flow(icj)/tot_flow
!          temp2 = temp2 + percent * unit2_temp(icj)
c          write(LGU,*)'VALORE ICJ:',icj
          unit_perc(icj) = unit_flow(icj)/tot_flow
c          write(LGU,*)'VALORE unit_perc:',unit_perc(icj)
c          write(LGU,*)'VALORE unit2_temp:',unit2_temp(icj)
c          write(LGU,*)'VALORE deltat:',deltat(icj)
c-ele 28/4/06
c          temp2 = temp2 + unit_perc(icj) * unit2_temp(icj)
          temp2 = temp2 + unit_perc(icj) * (unit2_temp(icj)-deltat(icj))
          write(LGU,*)'VALORE temp2:',temp2
	  end do
c---->intervento su deltat aircooler
        temp2 = temp2 - stat_vars(5)
      write(LGU,*)'VALORE FINALE temp2:',temp2
	  call out_temper_ac_ce_new(tair,press_out_calc(icj),
     *   tot_flow,stat_vars(7)
     *   ,pres_for_fase2 - delta_in_pressione(icj) - delta_ac1(icj)
     *   ,temp_for_fase2,temp2)


c_ecol 23/9/2004---------------------------------------------------------------
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning
cmar_AC_auto
      IF (.not.FLAG_AC)THEN
cmar_AC_auto
c_ecol
        if (temp2.gt.stat_vars(7)) then
	     ker=79
	     if (iopt.or.iopt_mess) return
cgpe-mess           call gest_error (2,0,'','Superata la massima temperatura di mandata',0)
c-UDM	     write (mess,570) temp2,stat_vars(7),UDM_INT_T ! write (mess,570) temp2,stat_vars(7)
c-UDM570        format('T man: ',f7.2,' - T man max: ',f7.2,'  ',A3)
           write(mess,570) udm_out_str(udm_T,temp2,0),
     *                     udm_out_str(udm_T,stat_vars(7),1)
570        format('T man: ',A10,' - T man max: ',A25)

           call app_mess(vert_pointer,ker,mess)
	  end if
cmar_AC_auto
	ENDIF
cmar_AC_auto
c_ecol------------------------------------------------------------------------
        end if
c
cgpe      flow_ric = 0
c    ********* 2026/01/27 NUOVO CONTROLLO SU PRESSIONE DI USCITA FASE 2 MAGGIORE DELLA PRESSIONE DI MANDATA***********
      do itip= 1, ntp
          write(LGU,*)'Valore pres_mand input:',pres2
          write(LGU,*)'Valore press_out:', press_out_calc(itip) 
          presMandataCalc = press_out_calc(itip) - delpr2
          write(LGU,*)'Valore pres_mand calc:',presMandataCalc
         if(abs(pres2 - presMandataCalc) .gt. 0.01 
     *       .and. press_out_calc(itip) .gt. 0.0) then
          ier = 0
          call messtel(.true.,.true.,' ',
     *        '*******  giallo&1  *******')          
559       format('Pressione Uscita: ',A10,' - Pressione Mandata: ',A25)
          write(mess,559) udm_out_str(udm_P,presMandataCalc,0),
     *                    udm_out_str(udm_P,pres2,1)
          call app_mess(vert_pointer,ier,mess)
          call tipo_err(istaz,err_ww,'')
          press_mand_calc(itip) = presMandataCalc 
         end if    
      end do    
c    ********* FINE CONTROLLO SU PRESSIONE DI USCITA FASE 2 MAGGIORE DELLA PRESSIONE DI MANDATA ***********
 
      flow_ric = tot_flow/old_aux3
      write(LGU,*)'Controllo portata - Valore flowm:',flowm
      write(LGU,*)'Controllo portata - Valore tot_flow:',tot_flow
      write(LGU,*)'Controllo portata - Valore flow_tot:',flow_tot      
      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow.lt.flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
!          if (iopt) return
          if (iopt .or. iopt_mess) return
c-UDM          write(mess,558) tot_flow/old_aux3,flowm/old_aux3,UDM_INT_Q ! write(mess,558) (tot_flow*conv_ns)/old_aux3, (flowm*conv_ns)/old_aux3
c-UDM558     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12) ! format(' Portata lavorata: ',f7.2,' *** Portata richiesta: ',f7.2)
558     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
          write(mess,558) udm_out_str(udm_Q,tot_flow/old_aux3,0),
     *                    udm_out_str(udm_Q,flowm/old_aux3,1)
          call app_mess(vert_pointer,ier,mess)
        else if (tot_flow.gt.flowm) then
!!          if (iopt .or. iopt_mess) return
!          if (iopt) return
          if (iopt) then
            if (.not.l_ric) ier = 0
            return
          endif
c-l_ric          if (.not.iopt_mess) then
            if (l_ric) then
              ier = 27
            else
              ier = 20
            endif
cgpe            if (l_ric) ier = 0
            if (l_ric) then
c-bpcorr	        ier = 0
cgpe              flow_ric = tot_flow/old_aux3
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
cgpe-mess              write(mess,558) (tot_flow*conv_ns)/old_aux3, (flowm*conv_ns)/old_aux3
c-UDM              write(mess,558) tot_flow/old_aux3,flowm/old_aux3,UDM_INT_Q
             if (.not.iopt_mess) then
              write(mess,558) udm_out_str(udm_Q,tot_flow/old_aux3,0),
     *                        udm_out_str(udm_Q,flowm/old_aux3,1)
              call app_mess(vert_pointer,ier,mess)
             endif
c-bpcorr
		    ier = 0
c-bpcorr-end
            endif
!            flow_ric = (tot_flow-flow_tot)/old_aux3
c-l_ric          endif
        endif
      endif




c
      return
      end


cmar-eq2*****************************************************************
      subroutine dati_compress2(hass,qass,qmax_assoluto,qmin_assoluto,
     *                tip_vinc,itip,
c_ecol
     *                itg,
c_ecol
     *                unit_head,unit2_head,unit_rev,
     *                unit2_rev,unit_flow,unit2_flow,unit_eff,unit2_eff,
     *                unit_temp,unit2_temp,unit_max,unit_min,unit_vcrit,
     *                unit_vinc,unit_eff_corr,unit2_eff_corr,
     *                unit_power_corr,unit2_power_corr,
cgpe     *                unit_power,unit2_power,unit_hrate,unit_cons,
     *                unit_power,unit2_power,unit_hrate_corr,
     *                unit_hrate,unit_cons,
c_ecol
     *                ac_k_int,
c_ecol
     *                pres_int, temp_int, dp_int, dt_int, 
     *                dp_int_ac,
     *                stat_varsp, ier)
c
      implicit none
c
c  Dati la prevalenza e la portata, calcola tutte le altre grandezze
c  di stazione
c  per le unita' bifase, calcola anche i dati della seconda fase
c 
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/flag.inc'
      include '../inc/simpstat.INC'
      include '../inc/mf_simpstat.INC'
      include '../inc/compstat.INC'
      include '../inc/td.inc'
      include '../inc/cc.inc'

cmar
      
cmar

c
      include '../inc/prototipo.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
c
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
cmar
     	common/equi_k/k_min(max_unit), k_max(max_unit)
cmar
cmar
      real*4 perc_equi_min, perc_equi_max

	real*4 k_min, k_max

	integer*4 i
cmar
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

CMAR
      COMMON/pre/ppres2
	real*4 ppres2

      COMMON/MAR/istaz
      integer*4 istaz

      real*4    hass,qass,qmax_assoluto,qmin_assoluto
      real*4    unit_head,unit_rev,unit_flow,unit_eff,
     *          unit_eff_corr,unit_power_corr,unit_power,unit_hrate,
     *          unit_cons,unit_max,unit_min,unit_vcrit,unit_temp,
     *          unit2_temp,unit_hrate_corr
      integer*4 unit_vinc,tip_vinc
      real*4    unit2_head,unit2_rev,unit2_flow,unit2_eff,
     *          unit2_eff_corr,unit2_power_corr,unit2_power
      real*4    pres_int, temp_int, dp_int, dt_int, pres_out
c_ecol
     *          ,dp_int_ac,ac_k_int,deltat
c_ecol
      real*4    zeta
c
      INTEGER*4 itip
      INTEGER*4 ier,ker
      real*4    qaux
      real*4    flow_norm,dpw
      logical*2 lhr
      real*4    pow_app 
      real*4    stat_varsp(*)
      real*4    eesp,aux1,tout,esp2
      real*4    exp_pol
      real*4    uno_su_ro_attuale
      real*4    aux3_int
      real*4    fi0, fi1, fie, qad, had
      real*4    prop
      EXTERNAL  prop
c
      real*4    dp_int_aux, dt_int_aux,
     *          ro_int
c_ecol
      integer*4 itg     !indice del turbogruppo analizzato
	character*(max_len) mess
c_ecol
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end

CMAR
      real*4 comp_ratio2
      real*4 delta_ac1(100)
      COMMON /DELTA/ delta_ac1
      INTEGER CONT_DC2
      COMMON /CNTBLK/ CONT_DC2
      CONT_DC2=CONT_DC2 + 1
c      write(LGU,*)'VALORE CONT_DC2: ',CONT_DC2
c-----------------------------------------------------------------------
      dp_int_aux = dp_int
      dt_int_aux = dt_int
      write(LGU,*)'DC2-itip: ',itip
      write(LGU,*)'DC2-itg: ',itg

 
      unit_flow = qass
      unit_head = hass
      unit_max  = qass/qmax_assoluto
      unit_min  = qass/qmin_assoluto
c 
      if ( ((abs(qass-qmax_assoluto)).lt.eps_flow)    .or.
     *     ((abs(qass-qmin_assoluto)).lt.eps_flow)    ) then
        if (tip_vinc.eq.3) then
          tip_vinc = 1
        else if (tip_vinc.eq.4) then
          tip_vinc = 2
        else if (tip_vinc.eq.8) then
          tip_vinc = 5
        else if (tip_vinc.eq.9) then
          tip_vinc = 6
        else if (tip_vinc.eq.10) then
          tip_vinc = 7
        end if
        unit_vinc = tip_vinc
      end if
c

cmar
c      do i=1,ntp
c	kcrit_min(i)=k_min(i)
c	kcrit_max(i)=k_max(i)
c     end do

cmak_min
c      if (tipo_crit.eq.crit_equi) then
c        fi0 = 1/kcrit_min(itip)
c        fi1 = 1/kcrit_max(itip)
c        qad = qass / qmrif
c        had = hass / headrif
c        fie = (qad*qad)/had
c        unit_vcrit = (fie-fi0)/(fi1-fi0)
c      end if
c --1-- numero di giri 
      call qdah_ch(clim(3,itip),hass,qaux)
      if(qass.le.qaux) then
c zona normale
        call ndahq(hass,qass,unit_rev,chn(1,itip))
      else
c zona di choking
        call ndahq(hass,qass,unit_rev,chc(1,itip))
      endif
      call effdaqn(unit_eff,unit_flow,unit_rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
      unit_eff = unit_eff*unit_eff_corr
cgpe
      if (unit_eff .lt. 0.01) unit_eff = 0.01
cgpe-end
      flow_norm = unit_flow/aux3
cgpe      unit_power= AGRVcc * rocn * unit_head*flow_norm/
cgpe *            (unit_eff*unit_power_corr)
      unit_power= AGRVcc * rocn * unit_head*flow_norm/unit_eff
cgpe-end
      unit_power=unit_power/(3.6*1.e3)

c fase intermedia
c---->  calcolo efficienza pressione intermedia e temperatura intermedia
      eesp=1/esp1
c---->                calcolo altezza adiabatica e portata
      aux1     = (erre / agrvcc / pmol) * Zin * tin/esp1
      write(LGU,*)'DC2-pin: ',pin
      write(LGU,*)'DC2-aux1     = (erre / agrvcc / pmol) * Zin * 
     * tin/esp1',aux1
      write(LGU,*)'DC2-unit_head: ',unit_head
      write(LGU,*)'DC2-eesp: ',eesp
      pres_int = pin*(((unit_head/aux1)+1)**eesp)
      write(LGU,*)'DC2-pres_int: ',pres_int
      tout     = 0.
      aux2     = (pres_int/pin)**esp1
      write(LGU,*)'DC2-aux2 = (pres_int/pin)**esp1: ',aux2
 
c
c-ace
	call single_out_temp_new(unit_temp,tin,zin,pin,pres_int,aux2,
     *           z_coef(1,ver_poi),dh_coef(1,ind),
     *           cp_par(1,ind),unit_eff)

c------>tolgo la caduta di pressione associata al piping
c-ele 28/4/06
c      unit_temp=unit_temp-type_deltint(itip)
      temp_int=unit_temp-type_deltint(itip)

c      call single_out_temp_ac(tair,pres_int,itg,1,
c     *                        unit_flow,type_maxtint(itip),unit_temp)
c      write(LGU,*)'AC CALL 13'
      call single_out_temp_ac_new(tair,pres_int,itg,1,
     *  unit_flow,type_maxtint(itip),temp_int,pin,tin,deltat)

c      temp_int=unit_temp
      temp_int=temp_int-deltat

c-ace18
      call ro_real(ind,pres_int,temp_int,ro_int)
c      type_delpint(itip)=type_delpint(itip)/ro_int
c	 ac_k_int=ac_k_int/ro_int
c      dp_int_aux  = type_delpint(itip)*flow_norm*flow_norm/1
c      write(LGU,*)'Valore type_delpint(itip) : ',type_delpint(itip)
      write(LGU,*)'DC2-Valore flow_norm : ',flow_norm
c      write(LGU,*)'Valore ro_int : ',ro_int
c      write(LGU,*)'Valore dp_int_aux = 
c     * type_delpint(itip)*flow_norm*flow_norm/ro_int : ',dp_int_aux
c_ecol
c_ecol Calcolo la caduta di pressione associata al piping e all'air cooler intermedio
      dp_int_ac = ac_k_int*flow_norm*flow_norm/1
      delta_ac1(itg) = dp_int_ac
      write(LGU,*)'DC2-Valore ac_k_int : ',ac_k_int
      write(LGU,*)'DC2-Valore dp_int_ac = 
     * ac_k_int*flow_norm*flow_norm/ro_int : ',dp_int_ac
c_ecol
c_ecol
c_ecol      pres_int = pres_int-dp_int_aux
      pres_int = pres_int-dp_int_aux-dp_int_ac
      write(LGU,*)'DC2-Valore pres_int DOPO DELTA : ',pres_int
c      dt_int_aux   = type_deltint(itip)
c      temp_int = unit_temp-dt_int_aux
c_ecol      if(temp_int.gt.type_maxtint(itip)) temp_int = type_maxtint(itip)
      if (fl_sim .and. .not.(iopt .or. iopt_mess)) then
        if(temp_int.gt.type_maxtint(itip)) then
	     ker=80
c-UDM	     write (mess,570) temp_int,type_maxtint(itip),UDM_INT_T ! write (mess,570) temp_int,type_maxtint(itip)
c-UDM570        format('T int: ',f5.2,' - T int max: ',f5.2,'  ',A3)
           write(mess,570) udm_out_str(udm_T,temp_int,0),
     *                     udm_out_str(udm_T,type_maxtint(itip),1)
570        format('T int: ',A10,' - T int max: ',A25)

           call app_mess(ver_poi,ker,mess)
        end if
      end if
c-c---->                   secondo stadio
cgpe      exp_pol = cesad(ind)
CMAR
c  2026/01/30 ( comp_ratio2=ppres2/pres_int)  ppres2 viene sostituita con pout che tiene conto del delta del refrigerante di fase 2 e del deltap della stazione/assetto
      comp_ratio2=pout/pres_int
      write(LGU,*)'DC2-Valore ppres2 : ',ppres2
      write(LGU,*)'DC2-Valore pout : ',pout
      write(LGU,*)'DC2-Valore pres_int : ',pres_int
      write(LGU,*)'DC2-Valore comp_ratio2 : ',comp_ratio2      
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio2,exp_pol,ier)

CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))
  
      call zpt1(istaz,pres_int,temp_int,z)
c---->                calcolo altezza adiabatica e portata
      write(LGU,*)'--INIT CALCOLO PREVALENZA FASE 2--'
      write(LGU,*)'DC2-Valore pres_int,temp_int : ',pres_int,temp_int
      write(LGU,*)'DC2-Valore exp_pol : ',exp_pol
      write(LGU,*)'DC2-Valore esp2 : ',esp2
      write(LGU,*)'DC2-Valore Z da call zpt1 : ',z
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
      write(LGU,*)'DC2-Valore erre : ',erre
      write(LGU,*)'DC2-Valore agrvcc : ',agrvcc
      write(LGU,*)'DC2-Valore pmol : ',pmol
      write(LGU,*)'DC2-Valore Z : ',Z
      write(LGU,*)'DC2-Valore temp_int : ',temp_int
      write(LGU,*)'DC2-Valore esp2 : ',esp2
      write(LGU,*)'DC2-Valore aux1 = 
     * (erre / agrvcc / pmol) * Z * temp_int/esp2 : ',aux1
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      write(LGU,*)'DC2-Valore uno_su_ro_attuale=
     * (1/1.e5)*(erre/pmol)*Z*temp_int/pres_int : ',uno_su_ro_attuale
      aux3_int    = ROCN*uno_su_ro_attuale
      write(LGU,*)'DC2-Valore ROCN : ',ROCN
      write(LGU,*)'Valore aux3_int=ROCN*uno_su_ro_attuale : ',aux3_int
      unit2_flow  = flow_norm*aux3_int
      write(LGU,*)'Valore unit2_flow=flow_norm*aux3_int : ',unit2_flow
      unit2_rev   = unit_rev*type_ratio(itip)/type2_ratio(itip)
      write(LGU,*)'DC2-Valore unit_rev : ',unit_rev
      write(LGU,*)'DC2-Valore type_ratio(itip) : ',type_ratio(itip)
      write(LGU,*)'DC2-Valore type2_ratio(itip) : ',type2_ratio(itip)
      write(LGU,*)'DC2-Valore  unit2_rev=
     *  unit_rev*type_ratio(itip)/type2_ratio(itip) : ',unit2_rev
      unit2_power = 0.
      call QDAN_CH(clim2(3,itip),qaux,unit2_rev,chn2(1,itip))
      write(LGU,*)'DC2-Valore qaux=
     * call QDAN_CH(clim2(3,itip),qaux,unit2_rev,chn2(1,itip)):',qaux
      if(unit2_flow.le.qaux) then
c zona normale
        call hdaqn(unit2_head,unit2_flow,unit2_rev,chn2(1,itip))
        write(LGU,*)'--DC2-INPUT PREVALENZA FASE 2 
     *         (unit2_flow,unit2_rev,chn2(1,itip)):',
     *          unit2_flow,unit2_rev,chn2(1,itip)
        write(LGU,*)'--DC2-VALORE PREVALENZA FASE 2:',unit2_head
        write(LGU,*)'--DC2-END CALCOLO PREVALENZA FASE 2--'
      else
        call hdaqn(unit2_head,unit2_flow,unit2_rev,chc2(1,itip))
        write(LGU,*)'--DC2-INPUT PREVALENZA FASE 2 
     *     (unit2_flow,unit2_rev,chc2(1,itip)):',
     *     unit2_flow,unit2_rev,chc2(1,itip)
        write(LGU,*)'--DC2-VALORE PREVALENZA FASE 2:',unit2_head
        write(LGU,*)'--DC2-END CALCOLO PREVALENZA FASE 2--'
      endif
c      unit2_head = head_for_fase2
      write(LGU,*)'--DC2-VALORE PREVALENZA FASE 2:',unit2_head      
      eesp = 1/esp2
      pres_out = pres_int*(((unit2_head/aux1)+1)**eesp)
      write(LGU,*)'--DC2-PRESSIONE USCITA FASE 2:',pres_out
!-01/10/99
!      unit2_eff = unit2_eff*unit2_eff_corr
      call effdaqn(unit2_eff,unit2_flow,unit2_rev,cen2(1,itip),
     *     cec2(1,itip),clim2(1,itip))
      write(LGU,*)'--DC2-unit2_eff:',unit2_eff
      write(LGU,*)'--DC2-unit2_eff_corr:',unit2_eff_corr
      unit2_eff = unit2_eff*unit2_eff_corr
      write(LGU,*)'--DC2-unit2_eff FINALE:',unit2_eff
cgpe
      if (unit2_eff .lt. 0.01) unit2_eff = 0.01
cgpe-end
!-01/10/99-end
      eesp = 1/esp2
cgpe      unit2_power= AGRVcc*rocn*unit2_head*flow_norm/
cgpe     *             (unit2_eff*unit2_power_corr)
      unit2_power= AGRVcc*rocn*unit2_head*flow_norm/unit2_eff
cgpe-end
      unit2_power=unit2_power/(3.6*1.e3)
c gz unit_power e' ora in Kjoule/ora. Conversione in KWatt:
      pow_app=(unit_power+unit2_power)
c---->  simulazione turbina
      lhr = .true.
      ier = 0
cgpe      call turb(unit_rev,pow_app,unit_hrate,tair,cpwm(1,itip),
      call turb(itip,unit_rev,unit_power_corr,pow_app,unit_hrate_corr,
     *     unit_hrate,tair,cpwm(1,itip),
     *	 cpwt(1,itip),chr(1,itip),chrt(1,itip),dpw,delta,lhr,ier)
c   se unit_power e' in kW
c      unit_hrate e' in [kJ]/[kWh]
c      pclitd     e' in kJ/kNm3
cgpe      unit_cons = unit_hrate*pow_app/pclitd(ind)/4186.
      unit_cons = unit_hrate*(pow_app/unit_power_corr)/pclitd(ind)
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
      if (.not.iopt) then
cgpe        unit_power=unit_power*unit_power_corr
cgpe        unit2_power=unit2_power*unit2_power_corr
c -prot calcolo temperatura di uscita finale
c      unit2_temp = ????

CMAR        zeta = prop(pres_int,temp_int,z_coef(1,ver_poi))

      call zpt1(istaz,pres_int,temp_int,zeta)
        aux2 = (pres_out/pres_int)**esp2
c-ace
	call single_out_temp_new(unit2_temp,temp_int,zeta,pres_int,pres_out,
     *           aux2,z_coef(1,ver_poi),dh_coef(1,ind),
     *           cp_par(1,ind),unit2_eff)
      write(LGU,*)'--DC2-TEMP.USCITA FASE 2:',unit2_temp
      press_out_calc(itip) = pres_out
      write(LGU,*)'--DC2-press_out_calc(itip):',pres_out
c-ace
c-ace call single_out_temp(unit2_temp,temp_int,zeta,pres_int,pout,
c-ace     *               aux2,z_coef(1,ver_poi),1.,unit2_eff)
c-ace
c_ele 28/4/06
c      call single_out_temp_ac_new(tair,pout,itg,2,
c     *        unit2_flow,stat_varsp(7),unit2_temp,pres_int,tin,deltat)
c_ecol

      end if
      return
      end

cmar-eq2*****************************************************************
      subroutine dati_compress2_ok(hass,qass,qmax_assoluto,
     *                qmin_assoluto,tip_vinc,itip,
c_ecol
     *                itg,
c_ecol
     *                unit_head,unit2_head,unit_rev,
     *                unit2_rev,unit_flow,unit2_flow,unit_eff,unit2_eff,
     *                unit_temp,unit2_temp,unit_max,unit_min,unit_vcrit,
     *                unit_vinc,unit_eff_corr,unit2_eff_corr,
     *                unit_power_corr,unit2_power_corr,
cgpe     *                unit_power,unit2_power,unit_hrate,unit_cons,
     *                unit_power,unit2_power,unit_hrate_corr,
     *                unit_hrate,unit_cons,
c_ecol
     *                ac_k_int,
c_ecol
     *                pres_int, temp_int, dp_int, dt_int, 
     *                dp_int_ac,
     *                stat_varsp, ier)
c
      implicit none
c
c  Dati la prevalenza e la portata, calcola tutte le altre grandezze
c  di stazione
c  per le unita' bifase, calcola anche i dati della seconda fase
c 
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/flag.inc'
      include '../inc/simpstat.INC'
      include '../inc/mf_simpstat.INC'
      include '../inc/compstat.INC'
      include '../inc/td.inc'
      include '../inc/cc.inc'

cmar
      
cmar

c
      include '../inc/prototipo.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
c
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
cmar
     	common/equi_k/k_min(max_unit), k_max(max_unit)
cmar
cmar
      real*4 perc_equi_min, perc_equi_max

	real*4 k_min, k_max

	integer*4 i
cmar


CMAR
      COMMON/pre/ppres2
	real*4 ppres2

      COMMON/MAR/istaz
      integer*4 istaz

      real*4    hass,qass,qmax_assoluto,qmin_assoluto
      real*4    unit_head,unit_rev,unit_flow,unit_eff,
     *          unit_eff_corr,unit_power_corr,unit_power,unit_hrate,
     *          unit_cons,unit_max,unit_min,unit_vcrit,unit_temp,
     *          unit2_temp,unit_hrate_corr
      integer*4 unit_vinc,tip_vinc
      real*4    unit2_head,unit2_rev,unit2_flow,unit2_eff,
     *          unit2_eff_corr,unit2_power_corr,unit2_power
      real*4    pres_int, temp_int, dp_int, dt_int, pres_out
c_ecol
     *          ,dp_int_ac,ac_k_int,deltat
c_ecol
      real*4    zeta
c
      INTEGER*4 itip
      INTEGER*4 ier,ker
      real*4    qaux
      real*4    flow_norm,dpw
      logical*2 lhr
      real*4    pow_app 
      real*4    stat_varsp(*)
      real*4    eesp,aux1,tout,esp2
      real*4    exp_pol
      real*4    uno_su_ro_attuale
      real*4    aux3_int
      real*4    fi0, fi1, fie, qad, had
      real*4    prop
      EXTERNAL  prop
c
      real*4    dp_int_aux, dt_int_aux,
     *          ro_int
c_ecol
      integer*4 itg     !indice del turbogruppo analizzato
	character*(max_len) mess
c_ecol
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end

CMAR
      real*4 comp_ratio2
c-----------------------------------------------------------------------
      dp_int_aux = dp_int
      dt_int_aux = dt_int
      unit_flow = qass
      unit_head = hass
      unit_max  = qass/qmax_assoluto
      unit_min  = qass/qmin_assoluto
c 
      if ( ((abs(qass-qmax_assoluto)).lt.eps_flow)    .or.
     *     ((abs(qass-qmin_assoluto)).lt.eps_flow)    ) then
        if (tip_vinc.eq.3) then
          tip_vinc = 1
        else if (tip_vinc.eq.4) then
          tip_vinc = 2
        else if (tip_vinc.eq.8) then
          tip_vinc = 5
        else if (tip_vinc.eq.9) then
          tip_vinc = 6
        else if (tip_vinc.eq.10) then
          tip_vinc = 7
        end if
        unit_vinc = tip_vinc
      end if
c

cmar
c      do i=1,ntp
c	kcrit_min(i)=k_min(i)
c	kcrit_max(i)=k_max(i)
c     end do

cmak_min
      if (tipo_crit.eq.crit_equi) then
        fi0 = 1/kcrit_min(itip)
        fi1 = 1/kcrit_max(itip)
        qad = qass / qmrif
        had = hass / headrif
        fie = (qad*qad)/had
        unit_vcrit = (fie-fi0)/(fi1-fi0)
      end if
c --1-- numero di giri 
      call qdah_ch(clim(3,itip),hass,qaux)
      if(qass.le.qaux) then
c zona normale
        call ndahq(hass,qass,unit_rev,chn(1,itip))
      else
c zona di choking
        call ndahq(hass,qass,unit_rev,chc(1,itip))
      endif
      call effdaqn(unit_eff,unit_flow,unit_rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
      unit_eff = unit_eff*unit_eff_corr
cgpe
      if (unit_eff .lt. 0.01) unit_eff = 0.01
cgpe-end
      flow_norm = unit_flow/aux3
cgpe      unit_power= AGRVcc * rocn * unit_head*flow_norm/
cgpe *            (unit_eff*unit_power_corr)
      unit_power= AGRVcc * rocn * unit_head*flow_norm/unit_eff
cgpe-end
      unit_power=unit_power/(3.6*1.e3)

c fase intermedia
c---->  calcolo efficienza pressione intermedia e temperatura intermedia
      eesp=1/esp1
c---->                calcolo altezza adiabatica e portata
      aux1     = (erre / agrvcc / pmol) * Zin * tin/esp1
      pres_int = pin*(((unit_head/aux1)+1)**eesp)
      tout     = 0.
      aux2     = (pres_int/pin)**esp1
c
c-ace
	call single_out_temp_new(unit_temp,tin,zin,pin,pres_int,aux2,
     *           z_coef(1,ver_poi),dh_coef(1,ind),
     *           cp_par(1,ind),unit_eff)

c------>tolgo la caduta di pressione associata al piping
c-ele 28/4/06
c      unit_temp=unit_temp-type_deltint(itip)
      temp_int=unit_temp-type_deltint(itip)

c      call single_out_temp_ac(tair,pres_int,itg,1,
c     *                        unit_flow,type_maxtint(itip),unit_temp)
      write(LGU,*)'AC CALL 14'
      call single_out_temp_ac_new(tair,pres_int,itg,1,
     *        unit_flow,type_maxtint(itip),temp_int,pin,tin,deltat)

c      temp_int=unit_temp
      temp_int=temp_int-deltat

c-ace18
      call ro_real(ind,pres_int,temp_int,ro_int)
c      type_delpint(itip)=type_delpint(itip)/ro_int
c	 ac_k_int=ac_k_int/ro_int

      dp_int_aux  = type_delpint(itip)*flow_norm*flow_norm/ro_int
c_ecol
c_ecol Calcolo la caduta di pressione associata al piping e all'air cooler intermedio
      dp_int_ac = ac_k_int*flow_norm*flow_norm/ro_int
c_ecol
c_ecol
c_ecol      pres_int = pres_int-dp_int_aux
      pres_int = pres_int-dp_int_aux-dp_int_ac
c      dt_int_aux   = type_deltint(itip)
c      temp_int = unit_temp-dt_int_aux
c_ecol      if(temp_int.gt.type_maxtint(itip)) temp_int = type_maxtint(itip)
      if (fl_sim .and. .not.(iopt .or. iopt_mess)) then
        if(temp_int.gt.type_maxtint(itip)) then
	     ker=80
c-UDM	     write (mess,570) temp_int,type_maxtint(itip),UDM_INT_T ! write (mess,570) temp_int,type_maxtint(itip)
c-UDM570        format('T int: ',f5.2,' - T int max: ',f5.2,'  ',A3)
           write(mess,570) udm_out_str(udm_T,temp_int,0),
     *                     udm_out_str(udm_T,type_maxtint(itip),1)
570        format('T int: ',A10,' - T int max: ',A25)

           call app_mess(ver_poi,ker,mess)
        end if
      end if
c-c---->                   secondo stadio
cgpe      exp_pol = cesad(ind)
CMAR
      comp_ratio2=ppres2/pres_int
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio2,exp_pol,ier)

CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))
  
      call zpt1(istaz,pres_int,temp_int,z)
c---->                calcolo altezza adiabatica e portata
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      aux3_int    = ROCN*uno_su_ro_attuale
      unit2_flow  = flow_norm*aux3_int
      unit2_rev   = unit_rev*type_ratio(itip)/type2_ratio(itip)
      unit2_power = 0.
      call QDAN_CH(clim2(3,itip),qaux,unit2_rev,chc2(1,itip))
      if(unit2_flow.le.qaux) then
c zona normale
        call hdaqn(unit2_head,unit2_flow,unit2_rev,chn2(1,itip))
      else
        call hdaqn(unit2_head,unit2_flow,unit2_rev,chc2(1,itip))
      endif
      eesp = 1/esp2
      pres_out = pres_int*(((unit2_head/aux1)+1)**eesp)
!-01/10/99
!      unit2_eff = unit2_eff*unit2_eff_corr
      call effdaqn(unit2_eff,unit2_flow,unit2_rev,cen2(1,itip),
     *     cec2(1,itip),clim2(1,itip))
      unit2_eff = unit2_eff*unit2_eff_corr
cgpe
      if (unit2_eff .lt. 0.01) unit2_eff = 0.01
cgpe-end
!-01/10/99-end
      eesp = 1/esp2
cgpe      unit2_power= AGRVcc*rocn*unit2_head*flow_norm/
cgpe     *             (unit2_eff*unit2_power_corr)
      unit2_power= AGRVcc*rocn*unit2_head*flow_norm/unit2_eff
cgpe-end
      unit2_power=unit2_power/(3.6*1.e3)
c gz unit_power e' ora in Kjoule/ora. Conversione in KWatt:
      pow_app=(unit_power+unit2_power)
c---->  simulazione turbina
      lhr = .true.
      ier = 0
cgpe      call turb(unit_rev,pow_app,unit_hrate,tair,cpwm(1,itip),
      call turb(itip,unit_rev,unit_power_corr,pow_app,unit_hrate_corr,
     *     unit_hrate,tair,cpwm(1,itip),
     *	 cpwt(1,itip),chr(1,itip),chrt(1,itip),dpw,delta,lhr,ier)
c   se unit_power e' in kW
c      unit_hrate e' in [kJ]/[kWh]
c      pclitd     e' in kJ/kNm3
cgpe      unit_cons = unit_hrate*pow_app/pclitd(ind)/4186.
      unit_cons = unit_hrate*(pow_app/unit_power_corr)/pclitd(ind)
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
      if (.not.iopt) then
cgpe        unit_power=unit_power*unit_power_corr
cgpe        unit2_power=unit2_power*unit2_power_corr
c -prot calcolo temperatura di uscita finale
c      unit2_temp = ????

CMAR        zeta = prop(pres_int,temp_int,z_coef(1,ver_poi))

      call zpt1(istaz,pres_int,temp_int,zeta)
        aux2 = (pout/pres_int)**esp2
c-ace
	call single_out_temp_new(unit2_temp,temp_int,zeta,pres_int,pout,
     *           aux2,z_coef(1,ver_poi),dh_coef(1,ind),
     *           cp_par(1,ind),unit2_eff)
c-ace
c-ace call single_out_temp(unit2_temp,temp_int,zeta,pres_int,pout,
c-ace     *               aux2,z_coef(1,ver_poi),1.,unit2_eff)
c-ace
c_ele 28/4/06
c      call single_out_temp_ac(tair,pout,itg,2,
c     *                        unit2_flow,stat_varsp(7),unit2_temp)
c_ecol

      end if
      return
      end
cmar-eq2---------------------------------------------------------------------------------

      real*4 Function zero_crit(vcrit)
c
c***********************************************************************
c Funzione da azzerare per il calcolo del valore della V del criterio
c per garantire il raggiungimento della portata totale
c sono fissati dall'esterno:
c  	-	flow_tot
c	-	tutte le variabili da fissare per zero_clim
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
c
      real*4    vcrit
c
      integer*4 itip
      integer*4 flag
      real*4    climite
      real*4    q, h, qtot
      real*4    qa, qb
      real*4    fi0, fi1
c-------------------------------------------------------------------
      qtot = 0
      do itip = 1, ntp
        if (type_actr(itip).gt.0) then
          itipo = itip


cmar  




          fi0=1./kcrit_min(itip)
          fi1=1./kcrit_max(itip)
          climite = 1./( fi0 + vcrit*(fi1-fi0) )
          if (climite.ge.clim(3,itip)) then
            call QDAN_CH(climite,qa,type_max_rev(itip),chn(1,itip))
            call QDAN_CH(climite,qb,type_min_rev(itip),chn(1,itip))
          else
            call QDAN_CH(climite,qa,type_max_rev(itip),chc(1,itip))
            call QDAN_CH(climite,qb,type_min_rev(itip),chc(1,itip))
          endif
          call q_clim (qa,qb,climite,flag,q,h)
          if (flag.ne.0) then
cgpe-new            call gest_error (2,0,'ZERO_CRIT',
cgpe-new     *           'Situazione non prevista. Flag.ne.0',0)
          endif
          qtot = qtot + q * type_actr(itip)
        endif
      enddo
c
      zero_crit = flow_tot - qtot
c
      return
      end
      real*4 Function zero_clim(flowt)
c
c***********************************************************************
c Funzione da azzerare per il calcolo della portata per garantire il 
c raggiungimento della pressione di uscita e imponendo il punto di lavoro 
c su una generica curva h/Q2 = clim
c sono fissati dall'esterno:
c 	-	climt
c	-	tutte le variabili da fissare per zero_giri
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
      include '../inc/MF_COMPSTAT.INC'
c
      real*4    flowt
c
      real*4    rev1,head1
      real*4    zero_giri
      external  zero_giri
      real*4    uno_su_ro_attuale
c-------------------------------------------------------------------
c---->                      ricerca numero minimo di giri
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3     = ROCN*uno_su_ro_attuale
c
      call hdaq_ch(climt,head1,flowt)
      if (climt.ge.clim(3,itipo)) then
c zona normale
        call ndahq(head1,flowt,rev1,chn(1,itipo))
      else 
        call ndahq(head1,flowt,rev1,chc(1,itipo))
      end if
c---->      calcolo portata e numero di giri sulla curva anti surge
      un_flow1 = flowt
      flow     = flowt/aux3
      zero_clim = zero_giri(rev1)
c
      return
      end
      real*4 Function zero_giri(rev)
c
c***********************************************************************
c Funzione da azzerare per il calcolo del numero di giri comune ai due
c compressori per garantire il raggiungimento della pressione di uscita
c Via common devono essere noti:
c A) Parametri generali di simulazione riferiti alla centrale
c    Settaggio a livello di main routine chiamante:
c pin, pout       Pressioni di ingresso e uscita
c zin
c tin
c ver_poi         ????
c esp1
c B) Settaggio a livello di unita' per singola simulazione:
c flow            Portata in CN dell'unita' di compressione
c un_flow1        Portata in C macchina dell'unita' di compressione
c itipo

c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/flag.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/MF_COMPSTAT.INC'
      include '../inc/PROTOTIPO.INC'
      
C
CMAR     
      COMMON/pre/ppres2
	real*4 ppres2, comp_ratio2
      COMMON/MAR/istaz
      integer*4 istaz
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

      real*4    haux,eesp
      real*4    rev
      real*4    rrev,flow_min,head1,hmax,pres_int,un_eff1,tout,
     *          un_temp1,temp_int,exp_pol,esp2,uno_su_ro_attuale,
     *          un_flow2,head2,pres_out,qaux
      real*4    prop
      external  prop
      real*4    aux1,aux3_int,deltat
      integer*4 irange
	character*(max_len) mess
	integer*4 ier,ker
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
 
c-ace18
      real*4 ro_int
      INTEGER CONT_ZG
      COMMON /CNTBLK/ CONT_ZG
      
      CONT_ZG=CONT_ZG + 1
      write(LGU,*)'VALORE CONT_ZG: ',CONT_ZG
c-----------------------------------------------------------------------
      if(rev.le.0) then
c        zero_giri = pin-pout
        zero_giri = pout-pin
        return
      end if
c---->         calcolo altezza adiabatica e flusso nel primo stadio
c-prototipo
      if (tipo_surge.eq.spezzata) then
        call QDAN_SU(chn(1,itipo),a_coef(1,itipo),b_coef(1,itipo),
     *             nlim(itipo),lim_n(1,itipo),rev,flow_min,haux,irange)
      else
        call QDAN_SU_MF(cmin_eff(itipo),flow_min,rev,chn(1,itipo))
      end if
      if(un_flow1.ge.flow_min) then
        call QDAN_CH(clim(3,itipo),qaux,rev,chn(1,itipo))
        if (un_flow1.le.qaux) then
          call hdaqn(head1,un_flow1,rev,chn(1,itipo))   
            write(LGU,*)'--INPUT PREVALENZA ZG 
     *         (un_flow1,rev,chn(1,itipo)):',
     *          un_flow1,rev,chn(1,itipo)
            write(LGU,*)'--VALORE PREVALENZA ZG:',head1
        else
          call hdaqn(head1,un_flow1,rev,chc(1,itipo))
          
            write(LGU,*)'--INPUT PREVALENZA ZG 
     *         (un_flow1,rev,chc(1,itipo)):',
     *          un_flow1,rev,chc(1,itipo)
            write(LGU,*)'--VALORE PREVALENZA ZG:',head1 
          
        endif
      else
c-prototipo = sistemare il trattamento di questo punto quando clim_eff
c             non coincide con clim_new
        if (tipo_surge.eq.parabola) then
          call hdaq_su_mf(cmin_eff(itipo),haux,un_flow1)
        else
          call hdaq_su(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *         lim_q(1,itipo),un_flow1,chn(1,itipo),haux,irange)
        end if
        hmax = haux
        head1 = hmax*(1.-0.05*(un_flow1-flow_min)/flow_min)
        write(LGU,*)'--VALORE ELSE PREVALENZA ZG:',head1 
       
      end if
c      hassZg1 = head1
c      head_for_fase2 = un_flow1

c---->  calcolo efficienza pressione intermedia e temperatura intermedia
      eesp=1/esp1
c---->                calcolo altezza adiabatica e portata
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
c      write(LGU,*)'ZG-pin: ',pin
c      write(LGU,*)'ZG-aux1     = (erre / agrvcc / pmol) * Zin * 
c     * tin/esp1',aux1
c      write(LGU,*)'ZG-head1: ',head1
c      write(LGU,*)'ZG-eesp: ',eesp
      pres_int=pin*(((head1/aux1)+1)**eesp)
      write(LGU,*)'ZG-pres_int: ',pres_int
      call effdaqn(un_eff1,un_flow1,rev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))
      un_eff1 = un_eff1*eff_corr(itipo)
cgpe
      if (un_eff1 .lt. 0.01) un_eff1 = 0.01
cgpe-end
      tout = 0.
      aux2 = (pres_int/pin)**esp1
c      write(LGU,*)'ZG-aux2 = (pres_int/pin)**esp1: ',aux2
c-ace
	call single_out_temp_new(un_temp1,tin,zin,pin,pres_int,aux2,
     *      z_coef(1,ver_poi),dh_coef(1,ind),
     *      cp_par(1,ind),un_eff1)

c--------------->caduta di temperatura dovuta al piping
c-ele 28/4/06
c      un_temp1 = un_temp1-type_deltint(itipo)
      temp_int = un_temp1-type_deltint(itipo)

c-ele 28/4/06
c      call single_out_temp_ac(tair,pres_int,itipo,1,
c     *                        un_flow1,type_maxtint(itipo),un_temp1)
c      write(LGU,*)'AC CALL 15'
      call single_out_temp_ac_new(tair,pres_int,itipo,1,
     *        un_flow1,type_maxtint(itipo),temp_int,pin,tin,deltat)

c      temp_int = un_temp1
      temp_int = temp_int-deltat
c-ace18
      call ro_real(ind,pres_int,temp_int,ro_int)
cmar-p
      ro_int=1.
cmar-p
c      type_delpint(itipo)=type_delpint(itipo)/ro_int
c	 type_ac_k_int(itipo)=type_ac_k_int(itipo)/ro_int

c      pres_int = pres_int-type_delpint(itipo)*flow*flow
c_ecol Ricalcolo la pressione intermedia tenendo conto delle cadute di pressione
c-ecol dovute al piping e alla presenza dell'air cooler intermedio
c      pres_int = pres_int-
c     *       (type_delpint(itipo)+type_ac_k_int(itipo))*flow*flow/ro_int
c      write(LGU,*)'ZG dp_int:',delta_in_pressione(itipo)

c      write(LGU,*)'ZG type_ac_k_int:',type_ac_k_int(itipo)

c      write(LGU,*)'ZG flow:',flow

c      write(LGU,*)'ZG pres_int prima del delta:',pres_int

      pres_int  = pres_int-delta_in_pressione(itipo)-
     * (type_ac_k_int(itipo))*flow*flow/ro_int 
      
      write(LGU,*)'ZG pres_int dopo il delta:',pres_int
c  2026/01/30 ( comp_ratio2=ppres2/pres_int)  ppres2 viene sostituita con pout che tiene conto del delta del refrigerante di fase 2 e del deltap della stazione/assetto      
      comp_ratio2=pout/pres_int
c      IF (pres_int .GE. pout .OR. ABS(comp_ratio2 - 1.0) .LT. 0.002) 
c     *    THEN
c     Condizione di uscita per evitare division by zero su calcolo pressione di uscita di seconda fase          
c          write(LGU,*)'ZG Condizione Uscita pres_int>=pout:'
c     *     ,pout-pres_int
c          zero_giri = pout-pres_int
c          if(pres_int .eq. pout) then
c              write(LGU,*)'ZG Impostata Uscita ZERO_GIRI=-0.1:'
c              zero_giri = -0.1
c          end if    
c          return
c      end if
c-ele      temp_int = un_temp1-type_deltint(itipo)
c_ecol      if(temp_int.gt.type_maxtint(itipo)) temp_int=type_maxtint(itipo)
cgpe      if (fl_sim .and. .not.(iopt .or. iopt_mess)) then
cgpe        if(temp_int.gt.type_maxtint(itipo)) then
cgpe	     ker=80
cgpe	     write (mess,570) temp_int,type_maxtint(itipo)
cgpe570        format('T int: ',f7.2,' - T int max: ',f7.2)
cgpe           call app_mess(ver_poi,ker,mess)
cgpe        end if
cgpe	end if
c---->                   secondo stadio
cgpe      exp_pol = cesad(ind)

CMAR

      write(LGU,*)'ZG-pol pres_int:',pres_int
      write(LGU,*)'ZG-pol temp_int:',temp_int
      write(LGU,*)'ZG-pol comp_ratio2:',comp_ratio2
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio2,exp_pol,ier)
      write(LGU,*)'ZG exp_pol:',exp_pol
CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))
   
      call zpt1(istaz,pres_int,temp_int,z)

c---->                calcolo altezza adiabatica e portata
c      write(LGU,*)'ZG erre / agrvcc / pmol:',erre,agrvcc,pmol

c      write(LGU,*)'ZG Z,temp_int,esp2:',Z,temp_int,esp2
 
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
c      write(LGU,*)'ZG aux1:',aux1
      aux2 = (pout/pin)**esp2
c      write(LGU,*)'ZG aux2:',aux2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
c      write(LGU,*)'ZG uno_su_ro_attuale:',uno_su_ro_attuale
 
      aux3_int = ROCN*uno_su_ro_attuale
c      write(LGU,*)'ZG aux3_int:',aux3_int
      un_flow2 = flow*aux3_int
c      write(LGU,*)'ZG un_flow2:',un_flow2
c      write(LGU,*)'ZG rev:',rev
c      write(LGU,*)'ZG type_ratio(itipo):',type_ratio(itipo)
c      write(LGU,*)'ZG type2_ratio(itipo):',type2_ratio(itipo)
      rrev = rev*type_ratio(itipo)/type2_ratio(itipo)
c      write(LGU,*)'ZG rrev = rev*type_ratio(itipo)/type2_ratio(itipo):',
c     * rrev
c - prototipo: la seconda fase e' totalmente dipendente dalla prima
      if (tipo_surge.eq.parabola) then
        call qvert(chn2(1,itipo),flow_min,rrev)
      else
        call QDAN_SU(chn2(1,itipo),a_coef2(1,itipo),
     *               b_coef2(1,itipo),nlim2(itipo),
     *               lim_n2(1,itipo),rrev,flow_min,haux,irange)
      end if
      if(un_flow2.ge.flow_min) then
        call QDAN_CH(clim2(3,itipo),qaux,rrev,chn2(1,itipo))
        if (un_flow2.le.qaux) then
          call hdaqn(head2,un_flow2,rrev,chn2(1,itipo))
         
            write(LGU,*)'--INPUT PREVALENZA ZG2 
     *         (un_flow2,rrev,chn2(1,itipo)):',
     *          un_flow2,rrev,chn2(1,itipo)
            write(LGU,*)'--VALORE PREVALENZA ZG2:',head2 
          
        else
          call hdaqn(head2,un_flow2,rrev,chc2(1,itipo))
          
            write(LGU,*)'--INPUT PREVALENZA ZG2 
     *         (un_flow2,rrev,chc2(1,itipo)):',
     *          un_flow2,rrev,chc2(1,itipo)
            write(LGU,*)'--VALORE PREVALENZA ZG2:',head2 
                    
        endif
        if(head2.lt.0.) head2 = 0.
      else
        call hdaqn(haux,un_flow2,rrev,chn2(1,itipo))
        hmax = haux
        head2 = hmax*(1.-0.05*(un_flow2-flow_min)/flow_min)
        write(LGU,*)'--VALORE ELSE PREVALENZA ZG2:',head2 

      end if
c---->                calcolo pressione di uscita
c      head_for_fase2 = head2
      eesp=1/esp2
      write(LGU,*)'ZG aux1:',aux1
      write(LGU,*)'ZG eesp:',eesp
      pres_out=pres_int*(((head2/aux1)+1)**eesp)
c      zero_giri = pres_out-pout
      zero_giri = pout-pres_out
      write(LGU,*)'ZG ppres2,pres_out e pout:',ppres2,pres_out,pout
      return
      end

      subroutine q_clim (qa,qb,cli mite,flag,q,h)
c---
c     Calcola la portata q, nell'intervallo [qa,qb], con qa > qb, su una curva 
c     H/Q2=climite, tale da raggiungere la pout richiesta. 
c     Il FLAG specifica se e' stata raggiunta la soluzione nell'intervallo 
c     richiesto. Flag assume i valori:
c				0 - ok
c				1 - Pout non raggiunta nell'intervallo
c				2 - Pout sempre superata nell'intervallo
c---
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_COMPSTAT.INC'
c
      real*4     qa,qb   ! I) portate estreme dell'intervallo di ricerca
      real*4     climite ! I) costante della parabola H/Q2
      integer*4  flag    ! O) risultato della ricerca
      real*4     h,q     ! O) soluzione trovata
c
      real*4     qsol
      real*4     fa,fb
      real*4     qd
      real*4     zero_clim
      external   zero_clim
c-----------------------------------------------------------------------
      flag = 0
c completamento assegnazione valori in common da passare
      climt = climite    
      fa = zero_clim(qa)
      fb = zero_clim(qb)
      if (abs(fa).le.eps_flow) then
        q = qa 
        call hdaq_ch(climite,h,q)
      else if (abs(fb).le.eps_flow) then
        q = qb
        call hdaq_ch(climite,h,q)
      else if (fa.gt.0) then
c la pout richiesta non viene raggiunta neanche nel punto piu' alto dell'intervallo
        flag = 1
        return
      else if (fb .gt. 0) then
        qd=(qa+qb)/2.
        call zero_fun(qb,qa,qd,fb,fa,qsol,epsma,pmach,zero_clim)
        q = qsol
        call hdaq_ch(climite,h,q)
      else
c la pout richiesta e' sempre superata, anche nel punto piu' basso dell'intervallo
        flag = 2
        return
      endif
      return
      end
      subroutine verifica_pwmax(itip,flow1,head1,dpw)
c***********************************************************************
c Verifica che il punto richiesto non superi la massima potenza
c Il valore e' negativo se si supera la max_power
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/flag.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c

CMAR      COMMON/pre/ppres2
CMAR	real*4 ppres2
	real*4 comp_ratio2
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
      COMMON/MAR/istaz
      integer*4 istaz
      integer*4 itip
      real*4    flow1,head1
      real*4    flow2,head2
      real*4    qaux,rev1,rev2
      real*4    flow_norm
      real*4    dpw,deltat
      real*4    uno_su_ro_attuale,eesp,aux1,pres_int,temp_int,
     *          un_eff1,tout,un_temp1,exp_pol,esp2,aux3_int
      real*4    prop
      external  prop
      real*4    FLOW_MIN,haux,un_eff2
      real*4    pwmax, un_power1, un_power2
      integer*4 IRANGE
      INTEGER*4 HMAX     ! per l'implicit, dovrebbe essere I*2,
                         ! dovrebbe essere corretto real*4
      character*(max_len) mess
	integer*4 ier,ker
c-ace18
      real*4 ro_int
c---------------------------------------------------------------------
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3      = ROCN*uno_su_ro_attuale
      flow_norm = flow1/aux3
c
      call qdah_ch(clim(3,itip),head1,qaux)
      if (flow1.le.qaux) then
c zona normale
        call ndahq(head1,flow1,rev1,chn(1,itip))
      else
        call ndahq(head1,flow1,rev1,chc(1,itip))
      endif
c---->  calcolo efficienza pressione intermedia e temperatura intermedia
      eesp=1/esp1
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      pres_int=pin*(((head1/aux1)+1)**eesp)
      call effdaqn(un_eff1,flow1,rev1,cen(1,itip),cec(1,itip),
     *             clim(1,itip))
c 
      un_eff1 = un_eff1*eff_corr(itip)
cgpe
      if (un_eff1 .lt. 0.01) un_eff1 = 0.01
cgpe-end
      tout = 0.
      aux2 = (pres_int/pin)**esp1
c-ace
	call single_out_temp_new(un_temp1,tin,zin,pin,pres_int,aux2,
     *      z_coef(1,ver_poi),dh_coef(1,ind),
     *      cp_par(1,ind),un_eff1)

c---->caduta di temperatura dovuta al piping
c-ele 28/4/06
c      un_temp1 = un_temp1-type_deltint(itip)
      temp_int = un_temp1-type_deltint(itip)

c      call single_out_temp_ac(tair,pres_int,1,
c     *                          itip,flow1,type_maxtint(itip),un_temp1)
cgpe-corr      call single_out_temp_ac(tair,pres_int,1,
cgpe-corr     *                  itip,flow1,type_maxtint(itip),temp_int,deltat)
      write(LGU,*)'AC CALL 16'
      call single_out_temp_ac_new(tair,pres_int,itip,
     *            1,flow1,type_maxtint(itip),temp_int,pin,tin,deltat)

c      temp_int =un_temp1
      temp_int =temp_int-deltat

c-ace18
      call ro_real(ind,pres_int,temp_int,ro_int)
c      type_delpint(itip)=type_delpint(itip)/ro_int
c	 type_ac_k_int(itip)=type_ac_k_int(itip)/ro_int
      ro_int = 1.0
c      pres_int = pres_int-type_delpint(itip)*flow_norm*flow_norm
c_ecol Ricalcolo la pressione intermedia tenendo conto delle cadute di pressione
c-ecol dovute al piping e alla presenza dell'air cooler intermedio
c      pres_int = pres_int-(type_delpint(itip)+
c     *                 type_ac_k_int(itip))*flow_norm*flow_norm/ro_int
      pres_int  = pres_int-delta_in_pressione(itip)-
     * (type_ac_k_int(itip))*flow_norm*flow_norm/ro_int
c      temp_int = un_temp1-type_deltint(itip)
c_ecol      if(temp_int.gt.type_maxtint(itip)) temp_int=type_maxtint(itip)
cgpe      if (fl_sim .and. .not.(iopt .or. iopt_mess)) then
cgpe        if(temp_int.gt.type_maxtint(itip)) then
cgpe	     ker=80
cgpe	     write (mess,570) temp_int,type_maxtint(itip)
cgpe570        format('T int: ',f7.2,' - T int max: ',f7.2)
cgpe           call app_mess(ver_poi,ker,mess)
cgpe        end if
cgpe	end if
c---->                   secondo stadio
cgpe      exp_pol = cesad(ind)
CMAR
      
CMAR      comp_ratio2=ppres2/pres_int
CMAR da verificare per capire se nel comp_ratio va o no la p_int

      comp_ratio2= (pout/pin) 
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio2,exp_pol,ier)

CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))

      call zpt1(istaz,pres_int,temp_int,z)
c---->                calcolo altezza adiabatica e portata
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
      aux2 = (pout/pin)**esp2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      aux3_int = ROCN*uno_su_ro_attuale
      flow2 = flow_norm*aux3_int
      rev2 = rev1*type_ratio(itip)/type2_ratio(itip)
c - prototipo: la seconda fase e' totalmente dipendente dalla prima
      if (tipo_surge.eq.parabola) then
        call qvert(chn2(1,itip),flow_min,rev2) 
      else
        call QDAN_SU(chn2(1,itip),a_coef2(1,itip),
     *               b_coef2(1,itip),nlim2(itip),
     *               lim_n2(1,itip),rev2,flow_min,haux,irange)
      end if
      if(flow2.ge.flow_min) then
        call QDAN_CH(clim2(3,itip),qaux,rev2,chn2(1,itip))
        if (flow2.le.qaux) then
          call hdaqn(head2,flow2,rev2,chn2(1,itip))
        else
          call hdaqn(head2,flow2,rev2,chc2(1,itip))
        endif
        if(head2.lt.0.) head2 = 0.
      else
        call hdaqn(head2,flow2,rev2,chn2(1,itip))
        hmax  = haux
        head2 = hmax*(1.-0.05*(flow2-flow_min)/flow_min)
      end if
      call effdaqn(un_eff2,flow2,rev2,cen2(1,itip),cec2(1,itip),
     *               clim2(1,itip))
c
      un_eff2 = un_eff2*eff_corr2(itip)
      if (un_eff2 .lt. 0.01) un_eff2 = 0.01

c --- calcolo delta-potenza
      call pwdan(pwmax,rev1,tair,cpwm(1,itip),cpwt(1,itip))
c-mf03      call real_maxpower(itip,pwmax)

c --- potenza richiesta prima fase
cgpe      un_power1= AGRVcc * rocn * head1*flow_norm/(un_eff1*
cgpe     *           power_corr(itip))
      un_power1= AGRVcc * rocn * head1*flow_norm/un_eff1
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power1=un_power1/(3.6*1.e3)
c --- potenza richiesta seconda fase
cgpe      un_power2= AGRVcc * rocn * head2*flow_norm/(un_eff2*
cgpe     *           power_corr2(itip))
      un_power2= AGRVcc * rocn * head2*flow_norm/un_eff2
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power2=un_power2/(3.6*1.e3)

c-mf03      call real_minpower(itip,un_power2+un_power1)
      dpw = un_power2+un_power1-pwmax*power_corr(itip)
c
      return
      end
 
      subroutine calc_clim(vinc_min, vinc_max, qmax_assoluto, 
     *                     hmax_assoluto, qmin_assoluto, hmin_assoluto,
     *                     vinc_crit_min, vinc_crit_max, vmin_ass, 
     *                     vmax_ass, cmin, cmax, ier)
c ---
c Calcola il clim minimo e massimo per ogni unita'' entro la zona operativa
c e il clim limitante a destra e sinistra fra tutte le unita' attive per il
c rispetto del criterio
C
C NB:   !!!!!!!!!!!!!!!!!!!!!!
c type_kcrit sono i valori di clim corrispondenti agli estremi assoluti del
c criterio, ovvero V=0 e V=98.
c type_kcrit_eff sono invece i valori di clim effettivi entro cui puo' muoversi la
c macchina. I type_kcrit_eff sono gia' quelli estremi per ogni macchina, tenendo 
c conto anche del riciclo e del choking. Questo settaggio deve essere 
c fatto al caricamento della centrale, assegnando a 
c	type_kcrit_eff(2) il massimo fra 
c	-	clim corrispondente a V=98 .vs. clim di choking
c	type_kcrit_eff(1) il minimo fra 
c	-	clim corrispondente a V=0 .vs. clim di surge
c ---
c                     2      2
c                    Qeff - Qsur  
c Si ricorda che V = ------------ 
c                     2      2
c                    Qcho - Qsur
c
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'

cmar

      common/equi_k/k_min(max_unit), k_max(max_unit)
cmar
cmar
	real*4 k_min, k_max
c
      integer*4 vinc_min(*)       ! I) tipo di vincolo di minima
      integer*4 vinc_max(*)       ! I) tipo di vincolo di massima
      real*4    qmax_assoluto(*)  ! I) portata massima assoluta
      real*4    hmax_assoluto(*)  ! I) prevalenza massima assoluta
      real*4    qmin_assoluto(*)  ! I) portata minima assoluta
      real*4    hmin_assoluto(*)  ! I) prevalenza minima assoluta
      integer*4 vinc_crit_min  ! O) tipo che vincola il valor minimo del criterio
      integer*4 vinc_crit_max  ! O) tipo che vincola il valor massimodel criterio
      real*4    cmin(maxunits) ! O) valore di clim minimo per ogni tipo 
                               !    per il rispetto del criterio
      real*4    cmax(maxunits) ! O) valore di clim massimo per ogni tipo 
                               !    per il rispetto del criterio
      real*4    vmin_ass       ! O) valore di v limitante al minimo per
                               !    il rispetto del criterio
      real*4    vmax_ass       ! O) valore di v limitante al massimo per
                               !    il rispetto del criterio
      integer*4 ier            ! O) Errore bloccante: le curve limite delle
                               !    unita' non consentono un funzionamento 
                               !    contemporaneo
c
      real*4    vmin, vmax
      integer*4 itip
      real*4    fi0, fi1, fie
      real*4    had, qad
c-----------------------------------------------------------------------
      vmin_ass = -1.
      vmax_ass = 100.
c      
      do itip = 1,ntp
cmar
cmar	kcrit_min(itip)=k_min(itip)
cmar      kcrit_max(itip)=k_max(itip)
cmar


        if (type_actr(itip).gt.0) then
          fi0 = 1/kcrit_min(itip)
          fi1 = 1/kcrit_max(itip)
          fie = 1/type_kcrit_eff(1,itip)
          vmin = (fie-fi0)/(fi1-fi0)
          fie = 1/type_kcrit_eff(2,itip)
          vmax = (fie-fi0)/(fi1-fi0)
          if (vmin .gt. vmin_ass) then
            vmin_ass = vmin
            vinc_crit_min = itip
          endif          
          if (vmax .lt. vmax_ass) then
            vmax_ass = vmax
            vinc_crit_max = itip
          endif          
c
          if (vinc_max(itip).ne.cho_1f) then
            fi0 = 1/kcrit_min(itip)
            fi1 = 1/kcrit_max(itip)
            had = hmax_assoluto(itip)/headrif
            qad = qmax_assoluto(itip)/qmrif
            fie = (qad*qad)/had
            vmax = (fie-fi0)/(fi1-fi0)
            if (vmax .lt. vmax_ass) then
              vmax_ass = vmax
              vinc_crit_max = itip
            endif          
          endif
c
          if (vinc_min(itip).ne.sur_1f) then
            fi0 = 1/kcrit_min(itip)
            fi1 = 1/kcrit_max(itip)
            had = hmin_assoluto(itip)/headrif
            qad = qmin_assoluto(itip)/qmrif
            fie = (qad*qad)/had
            vmin = (fie-fi0)/(fi1-fi0)
            if (vmin .gt. vmin_ass) then
              vmin_ass = vmin
              vinc_crit_min = itip
            endif          
          endif
        endif
      enddo
c
      if (vmin_ass .gt. vmax_ass) then
c errore: le unita' non possono rispettare il criterio
cgpe        ier = 41
        ier = 70
!        if (iopt) return
        if (iopt .or. iopt_mess) return
cgpe-new          call gest_error (2,0,'CALC_CLIM',
cgpe-new     *         'Vmin maggiore di vmax',0)
        return
      endif
c
c calcolo il valori di clim per tutti i tipi perche' rispettino il criterio
c
      do itip = 1,ntp
        if (type_actr(itip).gt.0) then
          fi0=1./kcrit_min(itip)
          fi1=1./kcrit_max(itip)
          cmin(itip) = 1./( fi0 + vmin_ass*(fi1-fi0) )
          cmax(itip) = 1./( fi0 + vmax_ass*(fi1-fi0) )
        endif
      enddo
c 
      return
      end

 	Subroutine single_out_temp(tout,tin,zin,pin,pout,aux2,z_coef,
     *                             eff_coef,effic)



c
c	Calcola la temperatura di uscita per un tipo di compressore
C
        implicit none
      include '../inc/RK_PARAM.INC'
	
	
C
c	tout      O	temperatura di uscita
c	tin       I	temperatura di ingresso
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	z_coef    I	coefficienti per il calcolo di z
c	eff_coef  I	coefficient fo outlet temperature computation
c	effic     I	efficiency of the compressor
c-----------------------------------------------------------------------
c
	REAL	    tout,tin,pin,zin,pout,aux2,eff_coef,z_coef(*),effic
c	REAL 

c
        INTEGER*4   nt
        real*4      temp,temp1,delta
        real*4      zout
        real*4      dt,dtmin,dz,df
        real*4      T_ADIM,P_ADIM
c
        real*4      prop
        external    prop
c
        DATA dtmin /0.01/
        real*4      dtp
      COMMON/MAR/istaz,ppmol
      INTEGER*4 istaz
	real*4 ppmol
	real ro_attuale
c---------------------------------------------------------------------


	  if (effic.gt.0) then
          temp = tin * aux2
          do nt=1,10
CMAR            zout = prop(pout,temp,z_coef)
      call zpt1(istaz,pout,temp,zout)

            temp1 = tin * zin/zout*aux2
            dt = temp-temp1
            dtp = dt/temp
            if (abs(dtp).lt.dtmin) go to 30
CMAR            T_ADIM=TEMP/(TRIF+T0)
CMAR            P_ADIM=POUT/(PRIF+P0)

CMAR            dz = z_coef(4)*P_ADIM+z_coef(3)+
CMAR     *				2.*z_coef(6)*T_ADIM
CMAR   calcolo di dz/dt a p costante

      ro_attuale = (pout*ppmol*1E05)/(erre*zout*temp)
CMAR   dz
      call dzdt(istaz, ro_attuale,temp,dz)

            df = 1.+tin*zin*aux2*dz/(zout*zout)
            temp = temp-dt/df
          end do
c---->              correction for the compression ratio
 30       delta = eff_coef*(pout-pin)
          tout = (Tin+delta)+(Temp1-Tin-delta)/effic
	end if
	return
	end
c-ace


cmar_________________________________________________________
cmar vecchio metodo di calcolo della Tman del compressore
Cmar_________________________________________________________


c-ace
cmar	Subroutine single_out_temp_new(tout,tin,zin,pin,pout,aux2,
cmar     *           z_coef,dh_coef,cp_par,effic)
c
c	Calcola la temperatura di uscita per un tipo di compressore, nel caso 
c     di gas reale; i.e.  l'efficienza è espressa come rapporto di 
c     variazioni entalpiche.
C
cmar        implicit none

cmar	include '../inc/RK_PARAM.INC'
C
c	tout      O	temperatura di uscita del compressore[K]
c	tin       I	temperatura di ingresso [K]
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure [bar]
c	pout      I	outlet pressure[bar]
c	aux2      I	cofficient function of the compr_ratio
c	z_coef    I	coefficienti per il calcolo di z
c	dh_coef   I	coefficienti per il calcolo degli scostamenti entalpici
c     cp_par    I coefficienti per il calore specifico
c	effic     I	efficienza del compressore
c-----------------------------------------------------------------------
c
cmar	REAL*4	     tout,tin,pin,zin,pout,aux2,
cmar     *             z_coef(*),dh_coef(*),cp_par(*),effic
cmar	REAL*4       eff_coef,cp0m,tm,df,dfdT
c
cmar        INTEGER*4   nt
cmar        real*4      temp,delta
cmar        real*4      zout
cmar        real*4      dtmin,dz
cmar        real*4      T_ADIM,P_ADIM
c
cmar        real*4      prop
cmar        external    prop
c
cmar        DATA dtmin /0.01/
c---------------------------------------------------------------------
c       pressioni in [bar]
c       temperature in [K]
c       i coefficienti dh_coef lavorano con t[K],p[Pascal] 
cmar        if (effic.gt.0) then
c---->     calcolo della temperatura teorica di uscita del compressore
c-ace
c          aux2 = (pout/pin)**exp
cmar	     temp = tin * aux2
cmar	     do nt=1,10
c---->                   iterazioni per il calcolo di tout
c                        (temp,pout)

cmar	        zout = prop(pout,temp,z_coef)
cmar		    Tm   = (Tin+Temp)/2.
c                        calcolo del cp medio
cmar	        cp0m = cp_par(1)+Tm*(cp_par(2)+Tm*(cp_par(3)
cmar     *	    		  +Tm*cp_par(4)))
cmar	        eff_coef = dh_coef(2)/(cp0m+dh_coef(3))
cmar	        eff_coef = eff_coef*(1.-effic)/effic
cmar	        df = temp - tin*(1.+ (zin/zout*aux2-1.)/effic) -
cmar     *               eff_coef*(pout-pin)*PMKS
c
cmar              T_ADIM=temp/(TRIF+T0)   ! [K]/[K]
cmar              P_ADIM=pout/(PRIF+P0)   ! [bar]/[bar]
c             calcolo dz/dT
cmar	        dz = z_coef(4)*P_ADIM+z_coef(3)+2.*z_coef(6)*T_ADIM
c             calcolo la derivata della funzione df rispetto a Tout
cmar	        dfdT = 1.+tin*zin*aux2*dz/(zout*zout*effic)
cmar	        delta = - df/dfdT
cmar              temp  = temp + delta
cmar	        delta = delta/temp
cmar	        if (abs(delta).lt.dtmin) go to 30
cmar              if(nt.gt.10) then
cmar              if(nt.gt.10) then
cmar                call gest_error(1,0,'Single_out_temp_new',
cmar     *          'la temperatura di uscita del compr. diverge',0)
cmar                 goto 30
cmar              endif
cmar	       end do
c---->
c---->              correction for the compression ratio
cmar 30         tout = temp
cmar	end if
cmar	return
cmar	end
c-ace






CMAR_________________-------------------------------_______________________


C-    nuova implementazione del calcolo della temperatura di mandata del compressore

CMAR_________________-------------------------------_______________________

C*****************************


      Subroutine single_out_temp_new(tout,tin,zin,pin,

     *          pout,aux2, z_coef,dh_coef,cp_par,effic)

 



 

            implicit none

 

 

            include '../inc/RK_PARAM.INC'

 

      COMMON/MAR/istaz,ppmol
      COMMON/pres/pout_c

      real*4 pout_c
            integer*4 istaz

            real*4 ppmol

            

            real*4 pin, pout, aux2, tin, temp, tout

      real*4 dh_in, dh_out, ds_in, ds_out

      real*4 tm, cp0m, df, tideale

            real*4 tiso, delta, zin, zout 

            real*4 z_coef(*),dh_coef(*),cp_par(*), effic

            real*4 ro_attuale, dfdt, dz, H

 

      integer*4 nt, a

 

            real*4 H0_in, H0_out, H_in, H_out

            real*4 S0_in, S0_out, S_in, S_out          

      real*4 Hr, diff, tempIso

            real*4 dtmin, deltaS

      real*4 esp,k, tcon, pin_p, pout_p

      real*4 comp_ratio, exp_pol, ier

            real*4 effic_ce, effic_star, cp0in, cp0out, temp_iso

 

            DATA dtmin /0.001/
           
           
 

cmar   calcolo di H-altezza- nelle condizioni isoentropiche




      pout_c = pout

      comp_ratio=pout/pin

      call POLITROP (istaz, pin,tin, comp_ratio, exp_pol, temp, ier)
	temp_iso = temp
	

      Tm   = (temp+tin)/2

c                       calcolo del cp medio

                    cp0m = cp_par(1)+tm*(cp_par(2)+tm*(cp_par(3)

     *                                +tm*cp_par(4)))


      cp0in = cp_par(1)+tin*(cp_par(2)+tin*(cp_par(3)

     *                                +tin*cp_par(4)))

	cp0out = cp_par(1)+temp*(cp_par(2)+temp*(cp_par(3)

     *                                +temp*cp_par(4)))


      call d_rk(istaz,pin,tin,zin,dh_in,ds_in)

   

      call d_rk(istaz,pout,temp,zout,dh_out,ds_out)

 

     

            H0_in=cp0m*Tin

            H_in=dh_in+H0_in

 

            H0_out=cp0m*temp

            H_out=dh_out+H0_out

 

cmar   calcolo entropia totale

      pin_p= pin*1E5

      S0_in = cp0m * ALOG(tin) - erre * ALOG(pin_p)
      S_in = S0_in + ds_in


      pout_p= pout*1E5

      S0_out =  cp0m * ALOG(temp) - erre * ALOG(pout_p)
      S_out = S0_out + ds_out

 

      deltaS = S_out - S_in 



      H = H_out - H_in



      Hr = H/effic

	
            
      tcon = temp

            do nt = 1,100

cmar    è una compressione --> aumento Tisoentropica, per trovare Tout


      temp= temp*1.002



      call d_rk(istaz,pout,temp,zout,dh_out,ds_out)


 

            H0_out=cp0m*temp

            H_out=dh_out+H0_out

       diff = H_out-H_in 

       delta=(Hr - diff)

            delta=delta/Hr 

         
              pout_p= pout*1E5

       S0_out =  cp0m * ALOG(temp) - erre * ALOG(pout_p)

       S_out = S0_out + ds_out

 

      deltaS = S_out - S_in 



     

      cp0in = cp_par(1)+tin*(cp_par(2)+tin*(cp_par(3)

     *                                +tin*cp_par(4)))

	cp0out = cp_par(1)+temp*(cp_par(2)+temp*(cp_par(3)

     *                                +temp*cp_par(4)))





                    if (delta.lt.dtmin) goto 30

 

              if(nt.gt.100) then

                call gest_error(1,0,'Single_out_temp_new',

     *          'la temperatura di uscita del compr. diverge',0)

                 goto 30

              endif

      end do

30    tout = temp


 

      return

            end 




cmar-p-30-07-09 cle semplificata



      Subroutine single_out_temp_new_cs(istaz,tout,tin,zin,pin,

     *          pout,aux2,dh_coef,cp_par,effic)

 



 

            implicit none

 

 

            include '../inc/RK_PARAM.INC'

 

c      COMMON/MAR/istaz,ppmol
      COMMON/pres/pout_c

      real*4 pout_c
            integer*4 istaz

c            real*4 ppmol

            

            real*4 pin, pout, aux2, tin, temp, tout

      real*4 dh_in, dh_out, ds_in, ds_out

      real*4 tm, cp0m, df, tideale

            real*4 tiso, delta, zin, zout 

            real*4 dh_coef(*),cp_par(*), effic

            real*4 ro_attuale, dfdt, dz, H

 

      integer*4 nt, a

 

            real*4 H0_in, H0_out, H_in, H_out

            real*4 S0_in, S0_out, S_in, S_out          

      real*4 Hr, diff, tempIso

            real*4 dtmin, deltaS

      real*4 esp,k, tcon, pin_p, pout_p

      real*4 comp_ratio, exp_pol, ier

            real*4 effic_ce, effic_star, cp0in, cp0out, temp_iso

 

            DATA dtmin /0.001/
           
           
 

cmar   calcolo di H-altezza- nelle condizioni isoentropiche




      pout_c = pout

      comp_ratio=pout/pin

      call POLITROP (istaz, pin,tin, comp_ratio, exp_pol, temp, ier)
	temp_iso = temp
	

      Tm   = (temp+tin)/2

c                       calcolo del cp medio

                    cp0m = cp_par(1)+tm*(cp_par(2)+tm*(cp_par(3)

     *                                +tm*cp_par(4)))


      cp0in = cp_par(1)+tin*(cp_par(2)+tin*(cp_par(3)

     *                                +tin*cp_par(4)))

	cp0out = cp_par(1)+temp*(cp_par(2)+temp*(cp_par(3)

     *                                +temp*cp_par(4)))


      call d_rk(istaz,pin,tin,zin,dh_in,ds_in)

   

      call d_rk(istaz,pout,temp,zout,dh_out,ds_out)

 

     

            H0_in=cp0m*Tin

            H_in=dh_in+H0_in

 

            H0_out=cp0m*temp

            H_out=dh_out+H0_out

 

cmar   calcolo entropia totale

      pin_p= pin*1E5

      S0_in = cp0m * ALOG(tin) - erre * ALOG(pin_p)
      S_in = S0_in + ds_in


      pout_p= pout*1E5

      S0_out =  cp0m * ALOG(temp) - erre * ALOG(pout_p)
      S_out = S0_out + ds_out

 

      deltaS = S_out - S_in 



      H = H_out - H_in



      Hr = H/effic

	
            
      tcon = temp

            do nt = 1,100

cmar    è una compressione --> aumento Tisoentropica, per trovare Tout


      temp= temp*1.002



      call d_rk(istaz,pout,temp,zout,dh_out,ds_out)


 

            H0_out=cp0m*temp

            H_out=dh_out+H0_out

       diff = H_out-H_in 

       delta=(Hr - diff)

            delta=delta/Hr 

         
              pout_p= pout*1E5

       S0_out =  cp0m * ALOG(temp) - erre * ALOG(pout_p)

       S_out = S0_out + ds_out

 

      deltaS = S_out - S_in 



     

      cp0in = cp_par(1)+tin*(cp_par(2)+tin*(cp_par(3)

     *                                +tin*cp_par(4)))

	cp0out = cp_par(1)+temp*(cp_par(2)+temp*(cp_par(3)

     *                                +temp*cp_par(4)))





                    if (delta.lt.dtmin) goto 30

 

              if(nt.gt.100) then

                call gest_error(1,0,'Single_out_temp_new',

     *          'la temperatura di uscita del compr. diverge',0)

                 goto 30

              endif

      end do

30    tout = temp


 

      return

            end 





     
cmar--------------------------------------------------------------
      subroutine ordina_as(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(10000), vector(10000)
	integer*4 unit_avail_as_o(10000), a

	integer*4 kk, l1, l2, ii
c------------------------------------      
      l1=a
	l2=1



       do ii= a, a+11

    



      IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF
       vector_(ii)=vector(ii)*unit_avail_as_o(ii)

      
	 enddo


	
       do ii= a+12, a+23


	

	IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF

       vector_(ii)=vector(ii)*unit_avail_as_o(ii)


	 enddo

     
      do kk = a, a+23
     





        if(vector_(kk).ne.0) then
	IF(vector_(KK).EQ.-1000.)THEN
	vector_(KK)=0
	ENDIF

	  vector(l1)=vector_(kk)
      
	 
C	  l1=l1+1
        l1=l1+1

	  endif  
      enddo 



	do kk=a+12,a+23	   

	vector(kk)=vector(kk-12)

	enddo


	return
	end


cmar--------------------------------------------------------------
      subroutine ordina_as_bkp(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(10000), vector(10000)
	integer*4 unit_avail_as_o(10000), a

	integer*4 kk, l1, l2, ii
c------------------------------------      
      l1=1
	l2=1



       do ii= a, a+12



      IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF
       vector_(ii)=vector(ii)*unit_avail_as_o(ii)



	 enddo



	
       do ii= a+12, a+24

	IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF

       vector_(ii)=vector(ii)*unit_avail_as_o(ii)
	

	

	 enddo

     
      do kk = a+1, a+24

        if(vector_(kk).ne.0) then
	IF(vector_(KK).EQ.-1000.)THEN
	vector_(KK)=0
	ENDIF

	  vector(l1)=vector_(kk)

	 
	 
C	  l1=l1+1
        l1=a+1
	  endif  
      enddo 




	do kk=a+12,a+24	   
      
	vector(kk)=vector(kk-12)


	enddo



	return
	end
     
cmar--------------------------------------------------------------
      subroutine ordina_as_(unit_avail_as_o,vector)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(24), vector(24)
	integer*4 unit_avail_as_o(24), a

	integer*4 kk, l1, l2, ii
c------------------------------------      
      l1=1
	l2=1
       do ii= 1, 12

	     
      IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF
       vector_(ii)=vector(ii)*unit_avail_as_o(ii)

	 enddo


       do ii= 13, 24
	IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF

       vector_(ii)=vector(ii)*unit_avail_as_o(ii)

	 enddo

     
      do kk = 1, 24
        if(vector_(kk).ne.0) then
	IF(vector_(KK).EQ.-1000.)THEN
	vector_(KK)=0
	ENDIF

	  vector(l1)=vector_(kk)
	 
	  l1=l1+1
	  endif  
      enddo 

	do kk=13,24	   
	vector(kk)=vector(kk-12)
	enddo
          


       
	return
	end
      subroutine ordina_as_old_w(unit_avail_as_o,vector)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(24), vector(24)
	integer*4 unit_avail_as_o(24), a

	integer*4 kk, l1, l2, ii
c------------------------------------      
      l1=1
	l2=1


       do ii= 1, 12
       
	     
      IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF
       vector_(ii)=vector(ii)*unit_avail_as_o(ii)

	 enddo

    


       do ii= 13, 24
      

	IF((vector(ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(ii)=-1000.
	ENDIF

       vector_(ii)=vector(ii)*unit_avail_as_o(ii)

	 enddo

     

      do kk = 1, 24
      
        if(vector_(kk).ne.0) then
	IF(vector_(KK).EQ.-1000.)THEN
	vector_(KK)=0
	ENDIF

	  vector(l1)=vector_(kk)
	 
	  l1=l1+1



	  endif  
      enddo 



	do kk=13,24	   
	vector(kk)=vector(kk-12)
	enddo
          


       
	return
	end

      subroutine ordina_type_as(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(21,10000), vector(21,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1, a
	logical*2 flag
c------------------------------------      
      l1=a
	l2=1

cc      kk1=1
        kk1=a
cc  	do i= 1,21

ccc	do ii = 1,12 
cc      vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
     

      

cc	do ii = 13,24 

cc       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
cc
cc	enddo



	do ii = a,a+11 

	do i= 1,21

	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

c      do i= 1,21
c	if(vector_(i,ii).ne.0) then
c	  vector(i,l1)=vector_(i,ii)
	 
c	  l1=l1+1
c	  endif  
     
c      enddo

	enddo

      

	do ii = a+12,a+23 
	do i= 1,21
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = a, a+23
      do i= 1,21
      
        if(vector_(i,kk).ne.0) then

      IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
	ENDIF

	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          

     
cmar
      
	do kk = a, a+11
      do i= 1,21


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo
cmar
       
	return
	end


cmar_ass_cond
      subroutine ordina_type_as_old(unit_avail_as_o,vector)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(21,48), vector(21,48)
	integer*4 unit_avail_as_o(24)

	integer*4 kk, l1, l2, ii, i, kk1
	logical*2 flag
c------------------------------------      
      l1=1
	l2=1

      kk1=1
cc  	do i= 1,21

ccc	do ii = 1,12 
cc      vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
     

      

cc	do ii = 13,24 

cc       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
cc
cc	enddo



	do ii = 1,12 

	do i= 1,21

	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

c      do i= 1,21
c	if(vector_(i,ii).ne.0) then
c	  vector(i,l1)=vector_(i,ii)
	 
c	  l1=l1+1
c	  endif  
     
c      enddo

	enddo

      

	do ii = 13,24 
	do i= 1,21
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = 1, 24
      do i= 1,21
      
        if(vector_(i,kk).ne.0) then

      IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
	ENDIF

	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          

     

       
	return
	end
cmar_ass_cond
      
      subroutine ordina_type_c_as(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(4,10000), vector(4,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1,a
	logical*2 flag
c------------------------------------   


      l1=a
	l2=1

      kk1=a
cc  	do i= 1,21

ccc	do ii = 1,12 
cc      vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
     

      

cc	do ii = 13,24 

cc       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

cc	 enddo
cc
cc	enddo

	
      


  

	do ii = a,a+11

	do i= 1,4



     


	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)
      
     

	 enddo



c      do i= 1,21
c	if(vector_(i,ii).ne.0) then
c	  vector(i,l1)=vector_(i,ii)
	 
c	  l1=l1+1
c	  endif  
     
c      enddo

	enddo


      


	do ii = a+12,a+23 
	do i= 1,4


  


      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)
        
	  
	 enddo

	enddo
      



	do kk = a, a+23
      do i= 1,4
      
        if(vector_(i,kk).ne.0) then
      
      IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
      ENDIF

	  vector(i,kk1)=vector_(i,kk)

	  
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          

cmar
      
	do kk = a, a+11
      do i= 1,4


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo


     

       
	return
	end


cmar_ass_cond


cmar ----------------------------------------------------------
      subroutine ordina_type_as_ch_p(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(6,10000), vector(6,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1,a
	logical*2 flag
c------------------------------------      
      l1=a
	l2=1

      kk1=a




	do ii = a, a+11

	do i= 1,6
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo



	enddo

      

	do ii = a+12,a+23
	do i= 1,6
	 IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF

       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = a, a+23
      do i= 1,6
      
        if(vector_(i,kk).ne.0) then
	IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
	ENDIF
	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

	   
c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          

cmar
      
	do kk = a, a+11
      do i= 1,6


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo
cmar
     

       
	return
	end

cmar ----------------------------------------------------------

      subroutine ordina_type_as_cen(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(5,10000), vector(5,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1,a
	logical*2 flag
c------------------------------------      
      l1=a
	l2=1

      kk1=a




	do ii = a, a+11 

	do i= 1,5
	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo



	enddo

      

	do ii = a+12,a+23
	do i= 1,5
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = a, a+23
      do i= 1,5
      
        if(vector_(i,kk).ne.0) then

	IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
      ENDIF

	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          

cmar
      
	do kk = a, a+11
      do i= 1,5


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo
cmar

       
	return
	end

cmar ----------------------------------------------------------
      subroutine ordina_type_as_ch(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(6,10000), vector(6,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1,a
	logical*2 flag
c------------------------------------      
      l1=a
	l2=1

      kk1=a




	do ii = a,a+11 

	do i= 1,6
	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo



	enddo

      

	do ii = a+12,a+23 
	do i= 1,6
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = a, a+23
      do i= 1,6
      
        if(vector_(i,kk).ne.0) then

	IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
	ENDIF
	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

c	do kk=13,24	   
c	vector(i,kk)=vector(i,kk-12)
c	enddo
          
cmar
      
	do kk = a, a+11
      do i= 1,6


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo
cmar
     

       
	return
	end


	 subroutine ordina_type_3(unit_avail_as_o,vector,a)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
c------------------------------------
      real*4 vector_(3,10000), vector(3,10000)
	integer*4 unit_avail_as_o(10000)

	integer*4 kk, l1, l2, ii, i, kk1,a
	logical*2 flag
c------------------------------------      
      l1=a
	l2=1

      kk1=a

	do ii = a, a+11

	do i= 1,3

	IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo

      

	do ii = a+12,a+23
	do i= 1,3
      IF((vector(i,ii).EQ.0).AND.unit_avail_as_o(ii).EQ.1)THEN
	vector(i,ii)=-1000.
	ENDIF
       vector_(i,ii)=vector(i,ii)*unit_avail_as_o(ii)

	 enddo

	enddo
      


	do kk = a, a+23
      do i= 1,3
      
        if(vector_(i,kk).ne.0) then

      IF(vector_(i,KK).EQ.-1000.)THEN
	vector_(i,KK)=0
	ENDIF

	  vector(i,kk1)=vector_(i,kk)
	  flag= .true.
		 	 	  
	  endif 
	   
      enddo  
       if(flag)then
       kk1=kk1+1 
	endif
      flag=.false.
      enddo

cmar
      
	do kk = a, a+11
      do i= 1,3


      vector(i,kk+12)=  vector(i,kk)

    

	enddo

	enddo
cmar

       
	return
	end

