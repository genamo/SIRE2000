
	Subroutine Single_Station(istaz,vert_pointer,vert_type,
     *             tipo_criterio,flow2,pres1,pres2,temp1,temp2,
     *             air,comp_ratio,tot_cons,head,delpr1,delpr2,
     *             flowm,flow_ric,type_num,unit_num,jufir,jtfir,
     *	           stat_vars,stat_ord,ier)
c
c***********************************************************************
c	simulazione e recover di una stazione semplice
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
c
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
c-mf03-end
      include '../inc/MF_UNITS.INC'
c-mf03-end



cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess
c
c  ind  : indice della stazione nella struttura dati (common stazione.inc)
 
      INTEGER*4  istaz
      INTEGER*4  vert_pointer,vert_type
      real*4   flow2,          ! I flusso di uscita
     *         pres1,pres2,    ! pressioni di ingresso e uscita
     *         pres1_RIC,pres2_RIC,  ! pressioni di ingresso e uscita
     *         temp1,temp2,    ! 
     *         air,comp_ratio,tot_cons,
     *         head,
     *         delpr1,delpr2,  ! I)
     *         flowm,flow_ric
c_ecol
cgpe     *         ,delpr_f,delpr_ac
c_ecol
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_vars(*)
      INTEGER*4  ier
      INTEGER*4  i,j,k
c
      real*4   mirev(maxunits),marev(maxunits),mieff(maxunits)
c
      INTEGER*4 stat_ord,tipo_criterio
c
      LOGICAL*2  lstat(maxunits)
cvar      real*4   ff1
cvar      real*4   eps_q/0.001/
c
cvar      integer*4 idummy

cvar      real*4    HTCITJ_OLD, PORTTJ_OLD, PMSETJ_OLD, PVSETJ_OLD,
cvar     *          VALSJC_OLD, QSETTJ_OLD
cvar      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD
cvar      character*3 status_old(maxunits)

cvar      real*4    pres_int,temp_int,delprint,delt_int
      integer*4 type_quant_old(maxunits)
cvar      real*4    unit_eff_corr_old(maxunits),
cvar     *          unit_power_corr_old(maxunits)
      real*4 a
		 real*4      dp1_M,dp2_M

	

cmar_ac_gestione mess
c	integer*4 i
cmar_ac_gestione mess

c-----------------------------------------------------------------------
c---->   salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
csave      call save_cond_ini (type_num,jtfir,pres_int,temp_int,
csave     *                    delprint,delt_int,type_quant_old)
      do i = 1, type_num
        j = jtfir+i-1
        type_quant_old(i) = type_quant(j)
      end do

cgaia      qsettj(istaz) = porttj(istaz)
c
10    continue

c---->   assign zero all the station variable
      call init_station (istaz,vert_pointer,tipo_criterio,vert_type,
     *     pres1,pres2,pres1_ric,pres2_ric,air,tot_cons,
cgpe     *     delpr1,delpr2,flowm,flow_ric,unit_num,type_num,type_quant_old,
     *     flowm,flow_ric,unit_num,type_num,type_quant_old,
cgpe     *     jufir,jtfir,nit,mirev,marev,mieff,lstat)
     *     jufir,jtfir,mirev,marev,mieff,lstat,
     *     stat_vars(1))


CMAR_AC
C       QUI CAMBIO IL MODELLO, SIMULO E VEDO SE è IL CASO DI ACCENDERE L'AC PER RAFFREDDARE IL GAS
CMAR     IF1


      IF( ac_ce_stat.eq.1)	THEN


cmar_1809
      type_actr_M=type_actr
cmar_1809

        
	ac_ce_k_M=ac_ce_k
	ac1_k_M=ac1_k
c        a=stat_vars(5)

	ac_ce_stat_M=ac_ce_stat

	ac1_stat_M=ac1_stat


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


c
c---->   simulazione stazione
      call Station(vert_pointer,flow2,pres1,pres2,temp1,temp2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     delpr1,delpr2,comp_ratio,tot_cons,flowm,flow_ric,PRES1_RIC,
     *     PRES2_RIC,head,type_quant(jtfir),type_num,unit_num,lstat,
     *     nom_flow(jufir),
     *     marev,mirev,mieff,unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),stat_vars,
c-mf03
     *     unit_vinc(jufir),
c-mf03-end
     *     IER)


	


CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARS(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M
    
      ac_ce_stat = ac_ce_stat_M

      ac1_stat	= ac1_stat_M
  
c      stat_vars(5)=a

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


       call Station(vert_pointer,flow2,pres1,pres2,temp1,temp2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     delpr1,delpr2,comp_ratio,tot_cons,flowm,flow_ric,PRES1_RIC,
     *     PRES2_RIC,head,type_quant(jtfir),type_num,unit_num,lstat,
     *     nom_flow(jufir),
     *     marev,mirev,mieff,unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),stat_vars,
c-mf03
     *     unit_vinc(jufir),
c-mf03-end
     *     IER)

        ENDIF


CMAR     IF1
       ELSE
CMAR     IF1


      call Station(vert_pointer,flow2,pres1,pres2,temp1,temp2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     delpr1,delpr2,comp_ratio,tot_cons,flowm,flow_ric,PRES1_RIC,
     *     PRES2_RIC,head,type_quant(jtfir),type_num,unit_num,lstat,
     *     nom_flow(jufir),
     *     marev,mirev,mieff,unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),
     *     unit_power_corr(jufir),unit_power(jufir),
     *     unit_hrate_corr(jufir),
     *     unit_hrate(jufir),unit_cons(jufir),
     *     unit_temp(jufir),unit_max(jufir),stat_vars,
c-mf03
     *     unit_vinc(jufir),
c-mf03-end
     *     IER)


	

CMAR     IF1
      ENDIF
CMAR     IF1

!      if (iopt) goto 11
cerr      if (iopt .or. iopt_mess) goto 11

c---->  gestione dell'errore

cerr      if (ier .gt. 0) then
cerr        if (ier .le. 5) then
!- ier=1: errore grave (pressioni alla stazione negative / i compressori
!-        non riescono a lavorare allo stesso numero di giri, qualunque
!-        siano le condizioni imposte)
!- ier=4: flow negativo
!- ier=5: pressione di ingresso superiore alla pressione di uscita
!- Non e' possibile portare il punto dentro le mappe, ma e' stato imposto
!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cerr          goto 11

cerr        else if (ier.eq.6 .or. ier.eq.7) then
!-ier=6: head troppo elevato
!-ier=7: head troppo basso
cerr          if (nit . lt. nitmax) then
!- viene portato il punto dentro le mappe
!-29/08/2000
!!- le nuove condizioni di input alla station sono rappresentate da
!!- PRES1/ PRES2/ FLOW2
!            ff1 = flow2
!            ff1 = ff1 - (ff1*eps_q)
!!            PRES1=PRES1_RIC
!!            PRES2=PRES2_RIC
!            call last_recover_station(vert_pointer,ff1,tipo_corr,ier)
!            flow2 = ff1
!- le nuove condizioni di input alla station sono rappresentate da
!- PRES1/ PRES2
!-29/08/2000-end

cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if
cerr        else if (ier.eq.17) then
!-ier=17: portata troppo alta
!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
!- (sono stati calcolati i dati di unita')
cerr          if (nit .lt. nitmax) then
!- se non e' stato corretto il punto, non sono state modificate le
!- condizioni iniziali
!            return
cerr            goto 11

cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if

cerr        else if ((ier.ge.10) .and. (ier.lt.20)) then
cerr          if (nit .lt. nitmax) then
!- viene portato il punto dentro le mappe
!- le nuove condizioni di input alla station sono rappresentate da
!- FLOW_RIC (PRES1_RIC/ PRES2_RIC)
cerr            pres1 = pres1_ric
cerr            pres2 = pres2_ric
cerr            ff1 = flow_ric
cerr            ff1 = ff1 - (ff1*eps_q)
cgpe            call last_recover_station(vert_pointer,ff1,tipo_corr,ier)
cerr            call last_recover_station(vert_pointer,ff1,ier)

cerr            flow2 = ff1
cerr            nit = nit + 1

cerr            goto 10
cerr          else
!- nel caso in cui l'errore derivi dalla correzione del punto, sono state
!- modificate le condizioni iniziali
cerr            goto 11

cerr          end if
cerr        end if
cerr      end if

!- simulazione OK oppure correzione OK: aggiornamento dei dati dei
!- compressori
      if (vert_type .eq. tipo_comp) then
csave        call agg_station (stat_ord,type_num,type_quant_old,unit_num,
csave     *       jtfir,jufir,pres_int,temp_int,delprint,delt_int,lstat)
        call agg_station(unit_num,jufir,lstat)
      endif
cgpe-new
cerr      if (ier.eq.0) then
cerr      endif
cgpe-new-end
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
csave      call restore_cond_ini (type_num,jtfir,type_quant_old)
      do i = 1, type_num
        j = jtfir+i-1
        type_quant(j) = type_quant_old(i)
      end do
c
      return
      end

      Subroutine Station(vert_pointer,flow,pres1,pres2,temp1,temp2,
c_ecol
cgpe     *            delpr_f,delpr_ac,
c_ecol 
     *     	    delpr1,delpr2,comp_ratio,tot_cons,flowm,flow_ric,
     *            PRES1_RIC,PRES2_RIC,head,type_quant,type_num,unit_num,
     *            status,nom_flow,
     *            max_rev,min_rev,min_eff,unit_perc,unit_rev,
     *            unit_flow,unit_head,unit_eff_corr,
     *            unit_eff,unit_power_corr,unit_power,
     *            unit_hrate_corr,unit_hrate,
     *            unit_cons,unit_temp,unit_max,stat_vars,
c-mf03
     *            unit_vinc,
c-mf03-end
     *            IER) 

c_ecol  I valori delpr_f e delpr_ac vengono visualizzati nel file contenente
c_ecol  i risultati della simulazione,quindi devo restituirli come output 
c
c	simulazione di stazione
c
c	Tutte le variabili delle unita' (rev, flow power ecc..) sono
c	        gia' definite uguali a zero.
c--------------------------------------------------------------------------
c	vert_pointer I	indice della stazione (da vert_pointer)
c	flow         I	outlet flow (worked by the station)
c	pres1        I	inlet pressure
c	pres2        I	outlet pressure
c	temp1        I	inlet temperature
c	temp2        I	outlet temperaure
c	delpr1       I	inlet pressure drop
c	delpr2       I	outlet pressure drop
c_ecol     delpr_f      O  caduta di pressione in ingresso dovuta al filtro
c_ecol     delpr_ac     O  caduta di pressione in uscita dovuta all'air cooler
c	comp_ratio   O	effective compression ratio
c	tot_cons     O	station consumption
c	flowm        O  total machine flow
c	flow_ric     O  total machine flow effective (recirculation)
c	head         O  station adiabatic head
c	type_quant   I  number of unit of each type
c	unit_num     I	total number of unit on the station
c	status       I  status of each unit (T=active , F=not active)
c	max_rev      I	max revolution rate (for each type)
c	min_rev      I	min revolution rate (for each type)
c	min_eff      I  min compressor efficiency (of each tupe)
c	unit_perc    O	flow rate percentage worked by each unit
c	unit_rev     O	revolution rate of each unit
c	unit_flow    O	machine flow of each unit
c	unit_eff_corr I	efficiency correction of each compressor
c	unit_eff     O	efficiency of each compressor
c	unit_power_corr I	turbine power correction
c	unit_power   O	power furnished by each turbine
c	unit_hrate_corr I heat rate correction of each turbine
c	unit_hrate   O heat rate of each turbine
c	unit_cons    O	consumption of each turbine
c	unit_temp    O	outlet gas temperature from each compressor
c	unit_max     O	percentage of the maximum flow workable
c	stat_vars    I	station variable
c	ier          O	error index
c========>>>>>>>>  local variable
c	hlim         station extreme (or unit extreme)
c	rev_lim      effective revolution rate limits
c	clim_eff     effective limit curve coefficients
c	lsim         unit to simulate (T= not simulate  F=already simulate)
c	lhr          turbine computation index (=T require heat rate comput. )
c	lric         recirculation index (=T recirculation is determined)
c--------------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/td.inc'
      include '../inc/cc.inc'
      include '../inc/th.inc '	! :in
      include '../inc/tj.inc '	! :in/out
      include '../inc/jc.inc '	! :out
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
c-mf03
      include '../inc/flag.INC'
c-mf03-end
c-UDM
      include '../inc/conv.INC'
c-UDM-end
cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat
c

cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN


      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num,type_num
c
      real*4    nom_flow(*)
      real*4    flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *          max_rev(*),min_rev(*),min_eff(*),flow_ric,PRES1_RIC,
     *	      PRES2_RIC,unit_perc(*),unit_rev(*),unit_max(*),
     *	      unit_flow(*)
      real*4    unit_head(*)
c-mf03
      integer*4 unit_vinc(*)
c-mf03-end
      real*4    unit_eff_corr(*),unit_eff(*),
cgpe     *	       unit_power_corr(*),unit_power(*),unit_hrate(*),
     *	       unit_power_corr(*),unit_power(*),
     *           unit_hrate_corr(*),unit_hrate(*),
     *	       unit_cons(*),unit_temp(*),stat_vars(*),
     *	       hlim(4),qlim(maxunits),rev_lim(2),clim_eff(maxunits),
     *           delpr1,delpr2,flowm,head,flow_lim(4)
     *          ,delpr_f,delpr_ac
      LOGICAL*2 lhr,lric,lsim(maxunits),status(*),LFLOW,lturb
c
      real*4   qq,pin,pout,exp_pol,esp,aux1
      real*4   flow_ass
      real*4   f1,f2
c
CMAR
      COMMON/MAR/istaz,ppmol
	integer*4 istaz
	real*4 ppmol
      INTEGER*4  i,ier,ker,nu,nutot,itip,ij,icj,imin,imax,ic
      INTEGER*4  ivinc
      INTEGER*4  irmax
      INTEGER*4  itip_max
      INTEGER*4  ntp_eff
      real*4   flow_work,tot_flow
      real*4   rev,uni_rev,tot_power,app_hrate
c
      real*4   dpw
c
      real*4   zero_pow,prop
      EXTERNAL zero_pow,prop
      real*4   eps_q/1.e-3/
      real*4   eps_p/0.05/
      real*4   eesp
      real*4   uno_su_ro_attuale
      INTEGER*4  iw
      real*4   qaux
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   pres_old,FLOW_APP
      real*4   hmax,hmin

      integer*4  i_icj
      LOGICAL*2 l_ric,bifase(maxunits)
      real*4 tout_ac

c      real*4   cesad
c      external cesad
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
c-UDM-end
cgpe-new
	integer*4 nrecover,max_nrecover
cgpe-new-end
CMAR
CMAR      integer*4 ier

cmar_08_05
      real*4 n_sim, n_calc
cmar_08_05

c-----------------------------------------------------------------------



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






cmar_DIN
      istaz=am
cmar_DIN


cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312


 
      ier = 0
      ker = 0
cgpe-new
	nrecover = 0
	max_nrecover = 20
cgpe-new-end
c_ecol
	do i=1,maxunits
	   bifase(i)=.false.
	end do
c_ecol
cgpe      if (flow.le.0.) then
cgpe        ier = 4
cgpe        if (iopt .or. iopt_mess) return
cgpec flow negativo ==> imposizione di pout=pin e stato=OFF.
cgpe        pres2 = pres1
cgpe        temp2 = temp1
cgpe        call imp_new_sta(vert_pointer,pres2,'FR')
cgpe        FLOW_APP=FLOW*CONV_NS
cgpe        write(mess,555) flow_APP
cgpe555     format(' Q richiesta ',f9.3)
cgpe        call app_mess(vert_pointer,ier,mess)
cgpe        return
cgpe      end if
c---->             
c          CADUTE DI PRESSIONE DOVUTE AL PIPING
      pin  = pres1 - delpr1
      pout = pres2 + delpr2



c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,flow,delpr_f)

	
c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p
      ro_in = 1.
	delpr_f=delpr_f/ro_in
      pin = pin - delpr_f


c-ace18
      if (una_turb) then
         do i=1,maxunits
            bifase(i)=.true.
         end do
      end if

c             CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_p (ntp,type_quant,status,bifase,nom_flow,
     *          ac_ce_k,ac1_k,ac2_k,flow,delpr_ac)




c-ace18
      call ro_real(ind,pout,temp1,ro_out)
cmar-p
      ro_out=1.
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
c NOTA: delpr1 e delpr2 comprendono sia il dp dovuto al piping e il dp dovuto
c       alla presenza di eventuali filtri ed aircooler
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac
      endif



c-bpcorr-end

cgpe      if(pin.le.0.or.pout.le.0) then
cgpec pressioni alla stazione negative ==> imposizione di pout=pin e stato=OFF.
cgpe        ier = 19
cgpe!        if (iopt) return
cgpe        if (iopt .or. iopt_mess) return
cgpe        temp2 = temp1
cgpe        pres2 = pres1
cgpe        call imp_new_sta(vert_pointer,pres2,'FR')
cgpe        write(mess,556) pin,pout
cgpe556     format('P asp: ',f7.2,' - P man:',f7.2)
cgpe        call app_mess(vert_pointer,ier,mess)
cgpe        ier = 1
cgpe        return
cgpe      endif
cgpe      if(pin.gt.pout) then
cgpec richiesta una pressione in uscita minore di quella di ingresso
cgpe        ier = 5
cgpe!        if (iopt) return
cgpe        if (iopt .or. iopt_mess) return
cgpec pin>pout ==> imposizione di pout=pin e stato=NR.
cgpe        pout=pin
cgpe        pres2 = pout-delpr2
cgpe        temp2 = temp1
cgpe        flow = 0.
cgpec          call imp_new_sta(vert_pointer,pres2,'NR')
cgpe        call imp_new_sta(vert_pointer,pres2,'FR')
cgpe        write(mess,556) pin,pout
cgpe        call app_mess(vert_pointer,ier,mess)
cgpe        return
cgpe      end if




CMAR
      comp_ratio = pout/pin 
	

      call politrop_esp(istaz,pin,temp1,comp_ratio,exp_pol,ier)

CMAR      exp_pol = prop(pin,temp1,exp_coef(1,ind))
      esp = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c---->             adiabatic head
! gz - ho cambiato la formule di aux1 e aux3 per togliere l'adimensionalizzazione
CMAR
CMAR      z = prop(pin,temp1,z_coef(1,vert_pointer))
      
	call zpt1(istaz, pin,temp1,z)
CMAR
c	aux1 = (ravotd(ind) / AGRVcc) * Z * temp1/esp
      aux1 = (erre / agrvcc / pmol) * Z * temp1/esp
      aux2 = comp_ratio**esp
      head = aux1 * (aux2-1.)         
	

	
	               ! in km
c	aux3 = ravotd(ind)*ro0td(ind)*Z*temp1/pin
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp1/pin
      aux3 = ROCN*uno_su_ro_attuale
      flowm = flow*aux3
      flow_ass = 0.
c---->               assign the unit to simulate
      nu = 0
      do i=1,ntp
        do ij=1,type_quant(i)
          icj=nu+ij
          lsim(icj) = status(icj)


cmar_08_05
       if(lsim(icj)) then  
	n_sim=n_sim+1
      endif
cmar_08_05


        end do
        nu = nu+type_quant(i)
      end do
c
      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo

c---->               compressor simulation (compute uni_rev)
10    flow_tot = (flow-flow_ass)*aux3
      ivinc = 0
      if (una_turb) then
        call limit(ntp,type_act_old,type_ratio,max_rev,
     *		min_rev,rev_lim,itip_max,irmax)
      else
        call limit(ntp,type_actr,type_ratio,max_rev,
     *		min_rev,rev_lim,itip_max,irmax)
      endif
      if(irmax.gt.0) then
        if(una_turb) then
c i compressori non riescono a lavorare allo stesso numero di giri, 
c qualunque siano le condizioni imposte.
!          if (.not.iopt) then
          if (.not.iopt .and. .not.iopt_mess) then
cgpe            iw = 61
            iw = 40
            mess = ' '
            call app_mess(vert_pointer,iw,mess)
          end if
          ier = 1
          return
        else
          ivinc = 1
        endif
      endif
      if (una_turb) then
        call surge_eff(ntp,type_act_old,type_ratio,head,
     *                 max_rev,min_rev,qlim,IVINC,IRMAX,
     *                 ITIP_MAX)
        if(irmax.gt.0) then
c i compressori non riescono a lavorare allo stesso numero di giri,
c qualunque siano le condizioni imposte.
!          if (.not.iopt) then
          if (.not.iopt .and. .not.iopt_mess) then
cgpe            iw = 61
            iw = 40
            mess = ' '
            call app_mess(vert_pointer,iw,mess)
          endif
          ier = 1
          return
        endif
      else
        call surge_eff(ntp,type_actr,type_ratio,head,
     *                 max_rev,min_rev,qlim,IVINC,IRMAX,
     *                 ITIP_MAX)
      endif
c---->             compute the station or compressor extremes
15    itipo = irmax
      call fact_lim(itip_max,hlim,flow_lim,clim_eff,rev_lim,
     *                max_rev,min_rev,irmax,ivinc)
c ricalcolo i clim_eff, perche' il vecchio modo non va piu' bene,
c dal momento che non ho piu' la similitudine
      if (una_turb) then
        call choke_eff(ntp,type_act_old,type_ratio,head,clim_eff)
      else
        call choke_eff(ntp,type_actr,type_ratio,head,clim_eff)
      endif
c---->        effective number of types
      ntp_eff = 0
      nu = 1
      if(irmax.gt.0) then
        ntp_eff = 1
        itipo = irmax
        imin = irmax
        imax = irmax
      ELSE
        imin = 1
        imax = ntp
        do ic=1,ntp
          if(type_actr(ic).gt.0) then
            itipo  = ic
            ntp_eff= ntp_eff+ 1
          end if
          nu = nu+type_quant(ic)
        end do
      end if
c---->                         compressors simulation
      lric = .false.
      headm = head
      call Compressors(vert_pointer,ntp_eff,max_rev,min_rev,
     *			hlim,qlim,rev_lim,clim_eff,irmax,ivinc,
     *                  uni_rev,IER) 
c&&&&&&&&&&&
      if((ier.gt.0).and.(ier.lt.20)) then
c---->               simulation ok or with warning
        if(ier.ge.10) then
          if (ier.ge.12.and.ier.ne.14) THEN
c gz  ier=12  max_rev       ==> subr compressors
c gz  ier=13  choking limit ==> subr compressors
c gz  ier=15  max power     ==> subr turb
            if (ier.EQ.12 .OR. IER.EQ.13) THEN
!              if (iopt) return
              if (iopt .or. iopt_mess) return
              LFLOW=.TRUE.
              if (una_turb) then
                call new_flusso_cp(LFLOW,rev_lim,type_ratio,clim_eff,
     *                flow_work,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
              else
                call new_flusso(LFLOW,max_rev,min_rev,
     *                flow_work,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
              endif
              do i=1,ntp
                type_actr(i)=type_act_old(i)
              enddo
            ENDIF
            RETURN
          ENDIF
          call recover_Compr(vert_pointer,min_rev,max_rev,rev_lim,
     *			clim_eff,qlim,type_quant,status,lsim,
     *			ivinc,irmax,lric,uni_rev,
     *			flow_ass,flow_work,unit_flow,ier)
cgpe-new
          nrecover = nrecover + 1
cgpe-new-end
          if(ier.eq.0) then
            imin = irmax
            imax = irmax
            ier = 0
          end if
        else
c  GZ   errori compresi fra 1 e 9: trattamento per recover degli errori
c  gz   6 7.
!          if (iopt) return
cgpe-corr          if (iopt .or. iopt_mess) return
          if (ier.eq.6) then
            if(irmax.eq.0 .and. ntp_eff.gt.1.and.(.not.una_turb)) then
              call fact_lim_reale(max_rev,min_rev,hmax,hmin)
              if (headm.le.hmax) then
c e' possibile il recover sganciando le macchine dal rapporto dei giri
                ier=23
                call recover_Compr(vert_pointer,min_rev,max_rev,rev_lim,
     *			clim_eff,qlim,type_quant,status,lsim,
     *			ivinc,irmax,lric,uni_rev,
     *			flow_ass,flow_work,unit_flow,ier)
cgpe-new
                nrecover = nrecover + 1
cgpe-new-end
                goto 15
              else
                hlim(1)=hmax
              endif
            endif
cgpe-corr
            if (iopt .or. iopt_mess) return
            if (fl_sim) then
c-UDM              write(mess,557) headm,hmax,UDM_INT_H
c-UDM557       format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,
              write(mess,557) udm_out_str(udm_H,headm,0),
     *                        udm_out_str(udm_H,hmax,1)
557   format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
              call app_mess(vert_pointer,ier,mess)
            endif
cgpe-corr            eesp=1/esp
cgpe-corr            if (htcitj(ind).eq.tc_pm) then
cgpe-corrc gz questo e' da fare come la trans di 77subunir.for
cgpe-corr              pres_old = pres1
cgpe-corr              call trans(pout,temp1,hlim(1),qaux,pin,qaux,1,ind,
cgpe-corr     *                   vert_pointer)
cgpe-corr              pin=pin+ eps_p
cgpe-corr              pres1 = pin+delpr1
cgpe-corrcgpe              call imp_new_sta(vert_pointer,pres1,'PM')
cgpe-corr              call imp_new_sta(ind,vert_pointer,pres1,'PM')
cgpe-corr              write(mess,557) pres_old,pres1
cgpe-corr557           format('P asp: richiesta=',f7.2,' - proposta=',f7.2)
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr            ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ .or.
cgpe-corr     *                 tipo_corr.eq.1) then
cgpe-corr              pres_old = pres2
cgpe-corr              pout=pin*(((hlim(1)/aux1)+1)**eesp)-eps_p
cgpe-corr              pres2 = pout-delpr2
cgpe-corr              if (pres2.gt.stat_vars(6)) then
cgpe-corr                 ier = 9
cgpe-corr!                 if (.not.iopt) then
cgpe-corr                 if (.not.iopt .and. .not.iopt_mess) then
cgpe-corr                   write(mess,558) pres2,stat_vars(6)
cgpe-corr558              format('P man: ',f7.2,
cgpe-corr     *                  ' - P man max: ',f7.2)
cgpe-corr                   call app_mess(vert_pointer,ier,mess)
cgpe-corr                   ier = 1
cgpe-corr                 end if
cgpe-corr                 return
cgpe-corr              endif
cgpe-corrcgpe              call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr              call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr              write(mess,559) pres_old,pres2
cgpe-corr559           format('P man: richiesta=',f7.2,' - proposta=',
cgpe-corr     *                f7.2)
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr            ELSE
cgpe-corrC IL SET E' IN PORTATA. CHE FARE ???
cgpe-corr              IER = 31
cgpe-corr              mess = ' '
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr              IER = 1
cgpe-corr            endif
            return
          elseif (ier.eq.7) then
            if(irmax.eq.0 .and. ntp_eff.gt.1.and.(.not.una_turb)) then
              call fact_lim_reale(max_rev,min_rev,hmax,hmin)
              if (headm.ge.hmin) then
c e' possibile il recover sganciando le macchine dal rapporto dei giri
                ier=22
                call recover_Compr(vert_pointer,min_rev,max_rev,rev_lim,
     *			clim_eff,qlim,type_quant,status,lsim,
     *			ivinc,irmax,lric,uni_rev,
     *			flow_ass,flow_work,unit_flow,ier)
cgpe-new
                nrecover = nrecover + 1
cgpe-new-end
                goto 15
              else
                hlim(4)=hmin
              endif
            endif
cgpe-corr
            if (iopt .or. iopt_mess) return
            if (fl_sim) then
c-UDM              write(mess,559) head,hmin,UDM_INT_H
c-UDM559     format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
              write(mess,559) udm_out_str(udm_H,headm,0),
     *                        udm_out_str(udm_H,hmin,1)
559   format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
              call app_mess(vert_pointer,ier,mess)
            endif
cgpe-corr            eesp=1/esp
cgpe-corr            if (htcitj(ind).eq.tc_pm) then
cgpe-corr              pres_old = pres1
cgpe-corr              call trans(pout,temp1,hlim(4),qaux,pin,qaux,1,ind,
cgpe-corr     *                   vert_pointer)
cgpe-corr              pin=pin - eps_p
cgpe-corr              pres1 = pin+delpr1
cgpe-corrcgpe              call imp_new_sta(vert_pointer,pres1,'PM')
cgpe-corr              call imp_new_sta(ind,vert_pointer,pres1,'PM')
cgpe-corr              write(mess,557) pres_old,pres1
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr            ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ .or.
cgpe-corr     *                 tipo_corr.eq.1) then
cgpe-corr              pres_old = pres2
cgpe-corr              pout=pin*(((hlim(4)/aux1)+1)**eesp)+eps_p
cgpe-corr              pres2 = pout-delpr2
cgpe-corr              if (pres2.gt.stat_vars(6)) then
cgpe-corr                ier = 9
cgpe-corr                write(mess,558) pres2,stat_vars(6)
cgpe-corr                call app_mess(vert_pointer,ier,mess)
cgpe-corr                ier = 1
cgpe-corr                return
cgpe-corr              endif
cgpe-corrcgpe              call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr              call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr              write(mess,559) pres_old,pres2
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr            else
cgpe-corrC IL SET E' IN PORTATA. CHE FARE ???
cgpe-corr              IER = 32
cgpe-corr              mess =' '
cgpe-corr              call app_mess(vert_pointer,ier,mess)
cgpe-corr              IER = 1
cgpe-corr            endif
          endif
          return
        end if
      else if(ier.ge.20) then
c----->      simulation with warning and more than one type active
!-27/07/2000-iopt
!!        if (iopt) return
!        if (iopt .or. iopt_mess) return
!-27/07/2000-iopt-end
        if (una_turb) then
!-27/07/2000-iopt
!!          if (iopt) return
!          if (iopt .or. iopt_mess) return
!-27/07/2000-iopt-end
          if (ier.eq.21.or.ier.eq.22) then
c recover delle situazioni di riciclo
            call recover_Compr_cp(vert_pointer,min_rev,max_rev,
     *                  rev_lim,qlim,type_quant,ivinc,irmax,ier)
cgpe-new
            nrecover = nrecover + 1
cgpe-new-end
            goto 15
          endif
          LFLOW=.TRUE.
          call new_flusso_cp(LFLOW,rev_lim,type_ratio,clim_eff,
     *                flow_work,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
          ier = 17
!          f1 = flow*aux3
!          f2 = flow_ric*aux3
!          write(mess,561) f1,f2
!          call app_mess(vert_pointer,ier,mess)
          do i=1,ntp
            type_actr(i)=type_act_old(i)
          enddo
          return
        else
          call recover_Compr(vert_pointer,min_rev,max_rev,rev_lim,
     *			clim_eff,qlim,type_quant,status,lsim,
     *			ivinc,irmax,lric,uni_rev,
     *			flow_ass,flow_work,unit_flow,ier)
cgpe-new
          nrecover = nrecover + 1
cgpe-new-end

          goto 15
        endif
      end if
c---->                  turbine computation
      nutot = 0
      do itip=1,imin-1
        nutot = nutot+type_quant(itip)
      end do
      if (una_turb) then
	do itip=imin,imax
	  if(type_actr(itip).gt.0) then
	    do ij =1,type_quant(itip)
	      icj = nutot+ij


	      if(lsim(icj)) then
                rev = uni_rev*type_ratio(itip)
                lturb=.false.
                call compr_data(lturb,rev,itip,
     *			unit_rev(icj),unit_flow(icj),unit_eff(icj),
     *			min_eff,unit_eff_corr(icj),
     *			unit_power_corr(icj),unit_power(icj),
cgpe
     *			unit_hrate_corr(icj),
     *			unit_hrate(icj),unit_cons(icj),ier)
                lsim(icj) = .false.
                type_actr(itip) = type_actr(itip)-1
                lvinc(icj) = ivinc
                if(lric) then
                  flow_ass = flow_work/aux3+flow_ass
                else
                  flow_ass = unit_flow(icj)/aux3+flow_ass
                end if
  	      end if
	    end do
	  end if
	  nutot = nutot+type_quant(itip)
	end do
      else
	do itip=imin,imax
	  if(type_actr(itip).gt.0) then
	    do ij =1,type_quant(itip)
	      icj = nutot+ij
	      if(lsim(icj)) then
c----->               compute station parameter
	        rev = uni_rev*type_ratio(itip)
                lturb=.true.
	        call compr_data(lturb,rev,itip,
     *			unit_rev(icj),unit_flow(icj),unit_eff(icj),
     *			min_eff,unit_eff_corr(icj),
     *			unit_power_corr(icj),unit_power(icj),
cgpe
     *			unit_hrate_corr(icj),
     *			unit_hrate(icj),unit_cons(icj),
     *			ier)
	        if(ier.gt.0) then
c---->                   turbine recover
	          itipo = itip
	          call recover_turb(vert_pointer,icj,
     *			unit_rev(icj),min_rev,min_eff,rev,
     *			unit_eff_corr,unit_power_corr,unit_flow,
     *			flow_ass,status,lsim,type_quant,ier)
cgpe-new
                nrecover = nrecover + 1
cgpe-new-end
c	          if(ier.gt.0) ier = 0
	          if(ier.gt.0) then
                  if (iopt .or. iopt_mess) return
cgpe-corr                    if (ier.ne.1) then
cgpe-corrc assimilo all'errore 6 per un trattamento uniforme in uscita
cgpe-corr                      ier = 6
cgpe-corr                      eesp=1/esp
cgpe-corr                      if (htcitj(ind).eq.tc_pm) then
cgpe-corr                        pres_old = pres1
cgpe-corr                        call trans(pout,temp1,headm,qaux,pin,qaux,
cgpe-corr     *                             1,ind,vert_pointer)
cgpe-corr                        pin=pin+ eps_p
cgpe-corr                        pres1 = pin+delpr1
cgpe-corrcgpe                        call imp_new_sta(vert_pointer,pres1,'PM')
cgpe-corr                        call imp_new_sta(ind,vert_pointer,pres1,'PM')
cgpe-corr                        write(mess,557) pres_old,pres1
cgpe-corr                        call app_mess(vert_pointer,ier,mess)
cgpe-corr                      ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                         htcitj(ind).eq.tc_pQ .or.
cgpe-corr     *                         tipo_corr.eq.1) then
cgpe-corr                        pres_old = pres2
cgpe-corr                        pout=pin*(((headm/aux1)+1)**eesp)-eps_p
cgpe-corr                        pres2 = pout-delpr2
cgpe-corr                        if (pres2.gt.stat_vars(6)) then
cgpe-corr                          ier = 9
cgpe-corr                          write(mess,558) pres2,stat_vars(6)
cgpe-corr                          call app_mess(vert_pointer,ier,mess)
cgpe-corr                          ier = 1
cgpe-corr                          return
cgpe-corr                        endif
cgpe-corrcgpe                        call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr                        call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr                        write(mess,559) pres_old,pres2
cgpe-corr                        call app_mess(vert_pointer,ier,mess)
cgpe-corr                      ELSE
cgpe-corrC IL SET E' IN PORTATA. CHE FARE ???
cgpe-corr                        IER = 31
cgpe-corr                        mess = ' '
cgpe-corr                        call app_mess(vert_pointer,ier,mess)
cgpe-corr                        IER = 1
cgpe-corr                      endif
cgpe-corr                    else
cgpe-corrc caso di ier = 1 ==> la turbina, al min_rev assoluto, lavora oltre la
cgpe-corrc                     max_power
cgpe-corr                    endif
                  if (ier.ne.1) ier = 1
cgpe-corr-end
                  return
                endif
                  lturb=.true.
	          call compr_data(lturb,rev,itip,
     *			unit_rev(icj),unit_flow(icj),unit_eff(icj),
     *			min_eff,unit_eff_corr(icj),
     *			unit_power_corr(icj),unit_power(icj),
     *			unit_hrate_corr(icj),
     *			unit_hrate(icj),unit_cons(icj),
     *			ker)
	          flow_ass = unit_flow(icj)/aux3+flow_ass
cgpe-new
               if (nrecover .ge. max_nrecover) then
                  ier = 1
                  return
                endif
cgpe-new-end
	          do i=1,ntp
	            if(type_actr(i).gt.0) go to 10
	          end do
	          goto 20
c-mf03
cgpe-corr              else ! ier = 0
cgpe-corrc check se il punto di lavoro trovato viola un limite operativo in potenza o giri
cgpe-corrc                pow_app = unit_power(icj) + unit2_power(icj)
cgpe-corr                pow_app = unit_power(icj)
cgpe-corr                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))
cgpe-corr                if (fl_vinc .and. ier.gt.0) then
cgpe-corr                  ier = 1
cgpe-corr                  return                  
cgpe-corr                else
cgpe-corrc                  unit_cons(icj) = unit_cons(icj) + soglia_cons
cgpe-corr                  if (ier .gt. 0 .and. iopt) then
cgpe-corr                    unit_cons(icj) = unit_cons(icj) + soglia_cons
cgpe-corr                  endif
cgpe-corr                  if (ier .gt. 0) unit_vinc(icj) = ier
cgpe-corr                  ier = 0
cgpe-corr                endif
c-mf03-end
	        end if ! end recover di turbina
cgpe-corr
c                pow_app = unit_power(icj) + unit2_power(icj)

cmar_pow_stat



	power_tot_staz=power_tot_staz+unit_power(icj)
    
cmar_pow_stat
              pow_app = unit_power(icj)
              ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))
              if (fl_vinc .and. ier.gt.0) then
                ier = 1
                return
              else
cgpe-c                  unit_cons(icj) = unit_cons(icj) + soglia_cons
                if (ier .gt. 0 .and. iopt) then
                  unit_cons(icj) = unit_cons(icj) + soglia_cons
                endif
                if (ier .gt. 0) unit_vinc(icj) = ier
                ier = 0
              endif
cgpe-corr-end
c---->              set unit already simulated
	        lsim(icj) = .false.
	        type_actr(itip) = type_actr(itip)-1
	        lvinc(icj) = ivinc
c---->                flow already worked by compressors
	        if(lric) then
	          flow_ass = flow_work/aux3+flow_ass
	        else
	          flow_ass = unit_flow(icj)/aux3+flow_ass
	        end if
	      end if
	    end do
	  end if
	  nutot = nutot+type_quant(itip)
	end do
      endif
c---->      verify if all the compressors are already simulated
!-16/06/99
!      do i=1,type_num
      do i=1,ntp
        if(type_actr(i).gt.0) go to 10
      end do
!-16/06/99-end
20    continue
c anzitutto, se una_turb e' true, devo verificare la power della stazione
      if (una_turb) then
c cerco la prima coppia attiva
        nu = 0
        do itip = 1,type_num
          lhr = .true.
          do ij =1,type_quant(itip)
            icj=nu+ij





c cerco la prima coppia attiva
            if(status(icj)) then
              tot_power=unit_power(icj)+unit_power(icj+unit_num)
              goto 21
            endif
          enddo
          nu = nu+type_quant(itip)
        enddo

21      continue
cgpe        call turb(unit_rev(icj),tot_power,app_hrate,tair,
        call turb(itip,unit_rev(icj),unit_power_corr(icj),tot_power,
     *            unit_hrate_corr(icj),app_hrate,tair,
     *            cpwm(1,itip),cpwt(1,itip),chr(1,itip),chrt(1,itip),
     *            dpw,delta,lhr,ier)
        if(ier.gt.0) then
!          if (iopt) return
          if (iopt .or. iopt_mess) return
cgpe          iw = 56
          iw = 46
          mess = ' '
          call app_mess(vert_pointer,iw,mess)
          LFLOW=.TRUE.
          call new_flusso_cp(LFLOW,rev_lim,type_ratio,clim_eff,
     *                flow_work,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
          ier = 15
          return
cgpe-corr
        else
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
c                pow_app = unit_power(icj) + unit2_power(icj)

	power_tot_staz=power_tot_staz+unit_power(icj)
          
cmar_pow_stat
          pow_app = tot_power
          ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))
          if (fl_vinc .and. ier.gt.0) then
            ier = 1
            return                  
          else
c            unit_cons(icj) = unit_cons(icj) + soglia_cons
            if (ier .gt. 0 .and. iopt) then
              unit_cons(icj) = unit_cons(icj) + soglia_cons
            endif
            if (ier .gt. 0) unit_vinc(icj) = ier
            ier = 0
          endif
cgpe-corr-end

        endif
c assegnazione dei valori a tutte le unita' in funzione del power richiesto
        nutot = 0
        do itip=1,type_num
          do ij =1,type_quant(itip)
            icj = nutot+ij
            if(status(icj)) then
              unit_hrate(icj) = app_hrate
cgpe              unit_cons(icj) = app_hrate*unit_power(icj)/
cgpe     *                        pclitd(ind)/4186.
cgpe              unit_cons(icj+unit_num) = app_hrate*
cgpe     *                      unit_power(icj+unit_num)/pclitd(ind)/4186.
c pclitd è stato modificato da kcal/Nm3 a kJ/kNm3
              unit_cons(icj) = app_hrate*
     *                        (unit_power(icj)/unit_power_corr(icj))/
     *                        pclitd(ind)
              unit_cons(icj+unit_num) = app_hrate*
     *         (unit_power(icj+unit_num)/unit_power_corr(icj+unit_num))
     *                      /pclitd(ind)
cgpe-corr
              unit_vinc(icj) = unit_vinc(itip)
cgpe-corr-end
            end if
          end do
          nutot = nutot+type_quant(itip)
        end do
      endif
      if(((flow-flow_ass)/flow_ass).gt.1.e-3) then
        ier = 17
!        if (iopt) return
        if (iopt .or. iopt_mess) return
c non riesce a lavorare tutto il gas.
!        f1 = flow*aux3
!        f2 = flow_ass*aux3
!        write(mess,561) f1,f2
!561     format(' Q di macchina (totale): richiesta=',f8.3,
!     *          '  -  massima=',f8.3)
!        call app_mess(vert_pointer,ier,mess)

c-UDM        f1 = flow*CONV_NS
c-UDM        f2 = flow_ass*CONV_NS
c-UDM        write(mess,562) f1,f2
        write(mess,559) udm_out_str(udm_Q,flow,0),
     *                  udm_out_str(udm_Q,flow_ass,1)
c-UDM562     format('Q richiesta: ',f9.3,' - Q proposta: ',f9.3)
562     format('Q richiesta: ',A10,' - Q proposta: ',A25)
        call app_mess(vert_pointer,ier,mess)
c gz attenzione!!! commento il return per andare avanti e calcolare 
c    flow_ric che serve nel last_recover_station. Attenzione a percmax
c    che, se chiamato con ier, puo' cambiare il valore del codice di
c    errore.
c          return
      else
        ier = 0
      end if
c----->           flow rate partition and total consumption
      tot_cons = 0.
      tot_flow = 0.
c
      nu = 0
      do i=1,ntp
        do ij=1,type_quant(i)
          icj=nu+ij
c-din_corr          tot_flow = tot_flow+unit_flow(icj)
c-din_corr          tot_cons = tot_cons+unit_cons(icj)
          if (status(icj)) then
            tot_flow = tot_flow+unit_flow(icj)
            tot_cons = tot_cons+unit_cons(icj)
          endif
c-din_corr-end
        end do
        nu = nu+type_quant(i)
      end do
c
      if (ier.gt.0) then
        LFLOW=.FALSE.
        if (una_turb) then
          call new_flusso_cp(LFLOW,rev_lim,type_ratio,clim_eff,
     *                TOT_FLOW,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
        else
          call new_flusso(LFLOW,max_rev,min_rev,
     *                TOT_FLOW,stat_vars,pin,pout,pres1_RIC,
     *                PRES2_RIC,type_act_old,UNIT_EFF_CORR,
     *                UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18  
     *                temp1,vert_pointer)
        endif
        do i=1,ntp
          type_actr(i)=type_act_old(i)
        enddo
      else
        flow_ric = tot_flow/aux3
      endif
c
      nu = 0
      do i=1,ntp
        do ij=1,type_quant(i)
          icj=nu+ij
          unit_perc(icj) = unit_flow(icj)/tot_flow
        end do
        nu = nu+type_quant(i)
      end do
c---->                           station outlet temperature
      temp2 = 0.
c gz !!!! attenzione !!!!
c per ora non si calcola il fattore eff_coef (cioe' k)
c-ace      eff_coef = 0
c gz
c_ecol      if (una_turb) then
	call out_temper_new(
     *                  tair,temp2,temp1,z,pin,pout,aux2,
     *			      z_coef(1,vert_pointer),
c-ace*                  eff_coef,unit_num,
     *                  dh_coef(1,ind),cp_par(1,ind),unit_num,
     *                  status,unit_perc,unit_eff,
     *                  unit_flow,
     *                  unit_temp,bifase,stat_vars(7))
c-------->modifica relativa al delta dell'ac
c
c ---> sottraggo la caduta di temperatura dovuta al piping

      temp2 =temp2 -stat_vars(5)
	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7),pin,temp1
     * ,temp2)  



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
c-27/07/2000-correzione dovuta al fatto che non c'e' riciclo sul minimo
c            numero di giri
!!      if (flow.lt.flow_ass) then
!      if (flow_ass.lt.flow_ric) then

      if ((flow_ric-flow_ass) .gt. eps_q) then
c la portata richiesta e' inferiore alla minima fattibile dalla stazione
!!        if (iopt .or. iopt_mess) return
!        if (iopt) return
!-viene settato il flag l_ric
        l_ric = .true.
cgpe-riciclo
        if (.not. fl_ric_rev) then
cgpe per simcent
          nu = 0
          do i=1,ntp
            do ij=1,type_quant(i)
              icj=nu+ij
              if (unit_rev(icj) .eq. min_rev(i) .and. status(icj)) then
                l_ric = .false.
              endif
            end do
            nu = nu+type_quant(i)
          end do
        endif
cgpe-riciclo-end
c
        if (.not.iopt .and. .not.iopt_mess) then
cgpe-new          if (l_ric) then
cgpe-newc la centrale e' in riciclo
cgpe-new            call gest_error (3,0,'STATION',
cgpe-new     *           'La centrale e'' in riciclo',0)
cgpe-new          else
cgpe-new            call gest_error (2,0,'STATION',
cgpe-new     *           'Portata inferiore alla minima',0)
cgpe-new          end if
        end if
c
        if (iopt) then
          ier = 0
          if (.not.l_ric) ier = 20
          return
        endif
c-l_ric        if (.not.iopt_mess) then
          if (l_ric) then
            ier = 27
c-bpcorr		  ier = 0
cgpe-mess            call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM            write(mess,560) flow_ric,flow_ass,UDM_INT_Q ! write(mess,560) (flow_ric*conv_ns),(flow_ass*conv_ns)
           if (.not.iopt_mess) then
            write(mess,560) udm_out_str(udm_Q,flow_ric,0),
     *                      udm_out_str(udm_Q,flow_ass,1)
            call app_mess(vert_pointer,ier,mess)
           endif
c-bpcorr
		  ier = 0
c-bpcorr-end
          else
            ier = 20
c-UDM            write(mess,560) flow_ric,flow_ass,UDM_INT_Q ! write(mess,560) (flow_ric*conv_ns),(flow_ass*conv_ns)
           if (.not.iopt_mess) then
            write(mess,560) udm_out_str(udm_Q,flow_ric,0),
     *                      udm_out_str(udm_Q,flow_ass,1)
            call app_mess(vert_pointer,ier,mess)
           endif
          endif
cgpe-mess560     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2)
c-UDM560     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
!          write(mess,560) (flow_ass*conv_ns)/aux3,
!     *                  (flow*conv_ns)/aux3
c-bpcorr          write(mess,560) (flow_ric*conv_ns),
c-bpcorr     *                  (flow_ass*conv_ns)
c-bpcorr          call app_mess(vert_pointer,ier,mess)
c-bpcorr          if (l_ric) then
c-bpcorr		  ier = 0
c-bpcorr            call gest_error (2,0,'',
c-bpcorr     *           'La centrale e'' in riciclo',0)
c-bpcorr            write(mess,560) (flow_ric*conv_ns),
c-bpcorr     *                      (flow_ass*conv_ns)
c-bpcorr            call app_mess(vert_pointer,ier,mess)
cgpe          else
cgpe	      flow_ric = 0.
c-bpcorr          endif
cgpe-new-end
c-l_ric        endif
cgpe-new
cgpe      else
cgpe        flow_ric = 0.
cgpe-new-end
      endif
c-27/07/2000-end
c 
!      if (iopt) return
      if (iopt .or. iopt_mess) return
c----->                compute the percentage of the maximum
      if (una_turb) then
c 
        call percmax_cp(vert_pointer,flowm,head,
     *		    type_quant,2*unit_num,status,
     *		    rev_lim,type_ratio,clim_eff,min_eff,
     *	            unit_rev,unit_flow,unit_head,unit_eff_corr,
     *              unit_power_corr,unit_max,ker)
      else
        call percmax(vert_pointer,flowm,head,
     *		    type_quant,unit_num,status,
     *		    max_rev,min_rev,min_eff,
     *	            unit_rev,unit_flow,unit_head,unit_eff_corr,
cgpe     *              unit_power_corr,unit_max,ker)
     *              unit_power_corr,unit_hrate_corr,unit_max,ker)
      endif
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
c correzione gz --- 20-luglio-1993
c
cgpe      nu = 0
cgpe      do i=1,ntp
cgpe        do ij=1,type_quant(i)
cgpe          icj = nu+ij
cgpe          if(status(icj)) then
cgpe            unit_power(icj)=unit_power(icj)*unit_power_corr(icj)
cgpe          end if
cgpe        end do
cgpe        nu = nu+type_quant(i)
cgpe      end do
c

 


      return
      end

	subroutine  percmax(vert_pointer,flowm,head,
     *		    type_quant,unit_num,status,
     *		    max_rev,min_rev,min_eff,
     *	            unit_rev,unit_flow,unit_head,unit_eff_corr,
cgpe     *              unit_power_corr,unit_max,ier)
     *              unit_power_corr,unit_hrate_corr,unit_max,ier)
c
c-----------------------------------------------------------------------
c	Calcola la percentuale del massimo
c-----------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
      include '../inc/SIMPSTAT.INC'
c
	INTEGER*4 vert_pointer,ier,type_quant(*),unit_num
	real*4  max_rev(*),min_rev(*),min_eff(*),unit_rev(*),
     *		unit_flow(*),unit_eff_corr(*),unit_power_corr(*),
     *		unit_max(*),unit_head(*)
     *        ,unit_hrate_corr(*)
	LOGICAL*2 status(*)
	LOGICAL*2 lturb
        real*4  head
        real*4  flsr
        INTEGER*4 nutot,itip,ic,i,ker
        real*4  uflow,urev,ueff,upower,uhrate,ucons
        real*4  h,rev,rev0,rev_min,rev_max,flowm
        real*4  fa,fb
        INTEGER*4 nrc
        character*(max_len) mess /' '/
        real*4   ZERO_POW
        EXTERNAL ZERO_POW
        real*4   eps_q/1.e-5/
        real*4   rdummy
c
        rdummy = head ! serve per evitare %FORT-I-VARUNUSED
c
	flsr = 0.
	nutot = 0
	do itip=1,ntp
	  do ic=1,type_quant(itip)
	    i = nutot+ic
	    if(status(i)) then
              unit_head(i)=headm
	      if(lvinc(i).eq.1) then
	        flsr = flsr+unit_flow(i)
	        unit_max(i) = 1.
c---->                    verify message for the maximum
	        if(unit_rev(i).lt.max_rev(itip)) then
c---->    verify if the turbine is at the maximum power
                  call qdah_ch(clim(4,itip),headm,uflow)
	          if(unit_flow(i).lt.(uflow-eps_q)) then
	            ier = 15
	          else
	            ier = 13
	          end if
!                  if (.not.iopt) then
                  if (.not.iopt .and. .not.iopt_mess) then
	            call app_mess(vert_pointer,ier,mess)
                  end if
	          ier = 0
	        end if
	      else
	        if(lvinc(i).eq.2) then
c---->                write messag for recirculation
	          if(unit_rev(i).gt.min_rev(itip)) then
	            ier = 10
	          else
	            ier = 11
	          end if
!                  if (.not.iopt) then
                  if (.not.iopt .and. .not.iopt_mess) then
	            call app_mess(vert_pointer,ier,mess)
                  end if
	        end if
c---->               compressor not at the maximum
c gz   calcolo la q corrispondente al max_rev ammissibile per lo head usato.
c      quindi calcolo a quale percento di questo flusso ha lavorato la macchina
                call QDAN_CH(clim(4,itip),uflow,max_rev(itip),
     *                       chc(1,itip))
	        call hdaq_ch(clim(4,itip),h,uflow)
	        if(headm.gt.h) then
	          rev = max_rev(itip)
	        else
  	          call qdah_ch(clim(4,itip),headm,uflow)
                  call NDAHQ(headm,uflow,rev,chc(1,itip))
	        end if
c---->              compute flow at maximum revolution rate
	        ker = 0
                lturb=.true.
	        call compr_data(lturb,rev,itip,
     *			urev,uflow,ueff,min_eff,
     *			unit_eff_corr(i),unit_power_corr(i),
cgpe     *			upower,uhrate,ucons,ker)
     *			upower,unit_hrate_corr(i),uhrate,ucons,ker)
	        if(ker.eq.0) then
c---->                      max flow at max revolution rate
	          unit_max(i) = unit_flow(i)/uflow
	        else
c---->             max flow at maximum turbine power
	          un_eff_corr(1) = unit_eff_corr(i)
	          un_pow_corr(1) = unit_power_corr(i)
	          effmi = min_eff(i)
c---->	 interval of research 
	          rev_min = unit_rev(i)
	          rev_max = rev
	          itipo = itip
	          Fa = zero_pow(rev_min)
	          fb = zero_pow(rev_max)
	          if(fa*fb.lt.0.) then
c---->              e' possibile trovare una soluzione
	            rev0 = (rev_min+rev_max)/2.
	            call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *			rev,epsmi,pmach,zero_pow)
	            rev = rev-epsmi
                    lturb=.true.
	            call compr_data(lturb,rev,itip,
     *			urev,uflow,ueff,min_eff,
     *			unit_eff_corr(i),unit_power_corr(i),
cgpe     *			upower,uhrate,ucons,ker)
     *			upower,unit_hrate_corr(i),uhrate,ucons,ker)
	            unit_max(i) = unit_flow(i)/uflow
	          else
c Al minimo numero di giri la potenza richiesta e' maggiore della massima
c fornibile dalla turbina.
c------>!!!!!! Questa situazione in teoria non dovrebbe mai verificarsi
	            unit_max(i) = 1.
	          end if
	        end if
	      end if
	      if(lvinc(i).ne.2) flsr = flsr+unit_flow(i)/unit_max(i)
	    end if
	  end do
	  nutot = nutot+type_quant(itip)
	end do
cgpe-mess
c----->             Verifica se puo' essere evitato il riciclo
cgpe	nrc = 0
cgpe	do i=1,unit_num
cgpe	  if(status(i)) then
cgpe	    if(lvinc(i).eq.2) then
cgpe	      nrc = nrc+1
cgpe	      if(flsr.gt.0.) then
cgpe	        if((flsr-flowm)/flsr.gt.1.e-3) then
cgpe	          ier = 2
cgpe!                  if (.not.iopt) then
cgpe                  if (.not.iopt .and. .not.iopt_mess) then
cgpe	            call app_mess(vert_pointer,ier,mess)
cgpe                  end if
cgpe	        end if
cgpe	      end if
cgpe	    end if
cgpe	  end if
cgpe	end do
cgpe-mess-end
!-28/08/2000-intervento sul riciclo
!	if(nrc.gt.1) then
!	  ier = 3
!!          if (.not.iopt) then
!          if (.not.iopt .and. .not.iopt_mess) then
!            call app_mess(vert_pointer,ier,mess)
!          end if
!	end if
!-28/08/2000-end
	ier = 0
   	return
	end

	subroutine  percmax_cp(vert_pointer,flowm,head,type_quant,
     *		    unit_num,status,rev_lim,tipi_ratio,clim_eff,min_eff,
     *              unit_rev,unit_flow,unit_head,unit_eff_corr,
     *              unit_power_corr,unit_max,ier)
c
c-----------------------------------------------------------------------
c	Calcola la percentuale del massimo 
c       (stazione composta, in parallelo, con una turbina 
c        per entrambe le fasi)
c-----------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
      include '../inc/SIMPSTAT.INC'
c
	INTEGER*4 vert_pointer,ier,type_quant(*),unit_num
        real*4  tipi_ratio(*)
        real*4  clim_eff(*)
	real*4  rev_lim(*),min_eff(*),unit_rev(*),
     *		unit_flow(*),unit_eff_corr(*),unit_power_corr(*),
     *		unit_max(*),unit_head(*)
	LOGICAL*2 status(*)
        real*4  head
        real*4  flsr
        INTEGER*4 nutot,itip,ic,i
        real*4  uflow,uflow1,haux
        real*4  rev,rev0,rev_min,rev_max,flowm
        real*4  fa,fb
        INTEGER*4 nrc
        character*(max_len) mess /' '/
        real*4   raux1,raux2
        real*4   ZERO_POW_cp
        EXTERNAL ZERO_POW_cp
        integer*4  fase_uno,fase_due
        real*4   eps_q/1.e-5/
        real*4   rev_save
        real*4   rdummy
c
        rdummy = head ! serve per evitare %FORT-I-VARUNUSED
c
	flsr = 0.
	nutot = 0
        fase_uno=1
        fase_due=1+type_quant(1)
	do itip=1,ntp
          raux1=rev_lim(1)*tipi_ratio(itip)
          raux2=rev_lim(2)*tipi_ratio(itip)
	  do ic=1,type_quant(itip)
	    i = nutot+ic
	    if(status(i)) then
              unit_head(i)=headm
	      if(lvinc(i).eq.1) then
	        flsr = flsr+unit_flow(i)
	        unit_max(i) = 1.
c---->                    verify message for the maximum
	        if(unit_rev(i).lt.raux2) then
c---->    verify if the turbine is at the maximum power
                  call qdah_ch(clim_eff(itip),headm,uflow)
	          if(unit_flow(i).lt.(uflow-eps_q)) then
	            ier = 15
	          else
	            ier = 13
	          end if
!                  if (.not.iopt) then
                  if (.not.iopt .and. .not.iopt_mess) then
	            call app_mess(vert_pointer,ier,mess)
                  end if
	          ier = 0
	        end if
	      else
	        if(lvinc(i).eq.2) then
c---->                write messag for recirculation
	          if(unit_rev(i).gt.raux1) then
	            ier = 10
	          else
	            ier = 11
	          end if
!                  if (.not.iopt) then
                  if (.not.iopt .and. .not.iopt_mess) then
	            call app_mess(vert_pointer,ier,mess)
                  end if
	        end if
c---->               compressor not at the maximum
c gz   calcolo la q corrispondente al max_rev ammissibile per lo head usato.
c      quindi calcolo a quale percento di questo flusso ha lavorato la macchina
c --- calcolo portata max sulla choke effettiva
                call QDAH_CH(clim_eff(itip),headm,uflow)
c --- calcolo portata max sul max_rev effettivo
                call hdan_ch(clim(3,itip),haux,raux2,chn(1,itip))
	        if(headm.gt.haux) then
                  call qdahn(headm,uflow1,raux2,chn(1,itip))
                else
                  call qdahn(headm,uflow1,raux2,chc(1,itip))
                endif
c --- il valore piu' limitante e' il massimo di riferimento:
                if (uflow1.lt.uflow) uflow=uflow1
c --- calcolo il rev corrispondente al punto massimo trovato
	        if(headm.gt.haux) then
                  call ndahq(headm,uflow,rev,chn(1,itip))
                else
                  call ndahq(headm,uflow,rev,chc(1,itip))
                endif
c
                unit_max(i) = unit_flow(i)/uflow
c---->             verifica per max_power
                if (itip.eq.1) then
                  un_eff_corr(1) = unit_eff_corr(fase_uno)
                  un_pow_corr(1) = unit_power_corr(fase_uno)
                  un_eff_corr(2) = unit_eff_corr(fase_due)
                  un_pow_corr(2) = unit_power_corr(fase_due)
                  effmi = min_eff(i)
c---->	 interval of research 
                  rev_min = unit_rev(i)
                  rev_max = rev
                  itipo = itip
                  rapp_giri=tipi_ratio(2)/tipi_ratio(1)
                  Fa = zero_pow_cp(rev_min)
                  fb = zero_pow_cp(rev_max)
                  rev_save=0.
                  if(fa.gt.0.) then
c---->              e' possibile trovare una soluzione
                    if (fb.lt.0) then
                      rev0 = (rev_min+rev_max)/2.
                      call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *			rev,epsmi,pmach,zero_pow_cp)
                      rev = rev-epsmi
                      rev_save=rev
                      call hdan_ch(clim(3,itip),haux,rev,chn(1,itip))
                      if(headm.ge.haux) then
                        call qdahn(headm,uflow,rev,chn(1,itip))
                      else
                        call qdahn(headm,uflow,rev,chc(1,itip))
                      endif
                      if ((unit_flow(i)/uflow) .gt.unit_max(i)) then
                        unit_max(i) = unit_flow(i)/uflow
                      endif
                    endif
                  else
c Al minimo numero di giri la potenza richiesta e' maggiore della massima
c fornibile dalla turbina.
c------>!!!!!! Questa situazione in teoria non dovrebbe mai verificarsi
                    unit_max(i) = 1.
                  end if
                else
                  if (unit_max(i-type_quant(1)).eq.1.) then
                    unit_max(i) = 1.
                  else if (rev_save.gt.0.) then
                    rev_save=rev_save*(tipi_ratio(2)/tipi_ratio(1))
                    call hdan_ch(clim(3,itip),haux,rev_save,chn(1,itip))
                    if(headm.ge.haux) then
                      call qdahn(headm,uflow,rev_save,chn(1,itip))
                    else
                      call qdahn(headm,uflow,rev_save,chc(1,itip))
                    endif
                    if ((unit_flow(i)/uflow) .gt.unit_max(i)) then
                      unit_max(i) = unit_flow(i)/uflow
                    endif
                  endif
                endif
	      end if
	      if(lvinc(i).ne.2) flsr = flsr+unit_flow(i)/unit_max(i)
	    end if
	  end do
	  nutot = nutot+type_quant(itip)
	end do
cgpe-mess
cgpec----->             Verifica se puo' essere evitato il riciclo
cgpe	nrc = 0
cgpe	do i=1,unit_num
cgpe	  if(status(i)) then
cgpe	    if(lvinc(i).eq.2) then
cgpe	      nrc = nrc+1
cgpe	      if(flsr.gt.0.) then
cgpe	        if((flsr-flowm)/flsr.gt.1.e-3) then
cgpe	          ier = 2
cgpe!                  if (.not.iopt) then
cgpe                  if (.not.iopt .and. .not.iopt_mess) then
cgpe	            call app_mess(vert_pointer,ier,mess)
cgpe                  end if
cgpe	        end if
cgpe	      end if
cgpe	    end if
cgpe	  end if
cgpe	end do
cgpe-mess-end
!-28/08/2000-intervento sul riciclo
!	if(nrc.gt.1) then
!	  ier = 3
!!          if (.not.iopt) then
!          if (.not.iopt .and. .not.iopt_mess) then
!	    call app_mess(vert_pointer,ier,mess)
!          end if
!	end if
!-28/08/2000-end
	ier = 0
   	return
	end

	Subroutine Compressors(vert_pointer,ntp_eff,
     *		    max_rev,min_rev,hlim,qlim,rev_lim,
     *		    clim_eff,irmax,ivinc,uni_rev,IER)
c
c	Simulazione del compressore
c
c	Questa routine calcola il numero di giri del compressore
c
c	ntp_eff      I	number of active types
c	max_rev      I	maximum revolution rate for each type
c	min_rev      I	minimum revolution rate for each type
c	hlim         I	extreme points for the station or compressor
c	qlim         I	estremi di surge per ogni compressore
c	rev_lim      I	effective revolution rate limits
c	clim_eff     I	effective limit curve
c	irmax        I	type of compressor with a constraints
c	ivinc        I	kind of constraints (=1 max flow,  =2 min flow)
c	uni_rev       O	revolution rate determined
c	ier          O	error index
c----------------------------------------------------------------------
        implicit none
        include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
	INTEGER*4 vert_pointer,ntp_eff,irmax,ivinc
        real*4 hlim(*),qlim(*),min_rev(*),max_rev(*),
     *			rev_lim(*),clim_eff(*)
c
        INTEGER*4    ier,itip
        real*4     uni_rev,qaux,flow_max,un_flow,ad_flow,ad_head,qh,
     *             flow_min,rev_min,rev,rev_max,rev0,fa,fb,haux
c
        character*(max_len) mess/' '/
	real*4     zero_rev
	EXTERNAL   zero_rev
        real*4     rdummy
c-----------------------------------------------------------------------
        rdummy = min_rev(1) ! serve per evitare %FORT-I-VARUNUSED
        rdummy = max_rev(1) ! serve per evitare %FORT-I-VARUNUSED
c
	ier = 0
c----->                 verifica dei vincoli
	if(headm.gt.hlim(1)) then
c----->   situazione non ammissibile: altezza adiabatica troppo elevata
          ier = 6
          return
	else if(headm.lt.hlim(4)) then
c----->   situazione non ammissibile: altezza adiabatica troppo bassa
          ier = 7
          return
	end if
	if(irmax.gt.0) then
c----->     simulazione con vincoli
c----------->            questo tipo di simulazione e' la prima effettuata
c	                 in ogni caso
c
	  if(ivinc.eq.1) then
c----->           compressore al massimo flusso
	    if(headm.gt.hlim(2)) then
c----->               massimo flusso al massimo revolution rate
cgz	      uni_rev = max_rev(irmax)
	      uni_rev = rev_lim(2)*type_ratio(irmax)
c---->           verifica se il flusso e' sufficiente
              call hdan_ch(clim(3,irmax),haux,uni_rev,chn(1,irmax))
  	      if(headm.ge.haux) then
                call qdahn(headm,qaux,uni_rev,chn(1,irmax))
              else
                call qdahn(headm,qaux,uni_rev,chc(1,irmax))
              endif
              flow_max = type_actr(irmax)*qaux
	    else
c----->               massimo flusso sulla choking curve
cgz              call qdah_ch(clim(4,irmax),headm,un_flow)
cgz              call ndaq_ch(clim(4,irmax),un_flow,uni_rev,chc(1,irmax))
              call qdah_ch(clim_eff(irmax),headm,un_flow)
              call ndaq_ch(clim_eff(irmax),un_flow,uni_rev,chc(1,irmax))
	      flow_max = un_flow*type_actr(irmax)
	    end if
	    if(flow_max.gt.flow_tot) then
c---->         il compressore lavora piu' gas del totale, percio' il numero di
c              giri deve essere ridotto
	      ier = 14
c
	      un_flow=flow_tot/type_actr(irmax)
              if (un_flow.lt.qlim(irmax)) then
c---->                recirculation zone 
	        ier = 10
	        un_flow = qlim(irmax)
                qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
                if (qh.lt.clim(3,irmax) ) then
                  call ndahq(headm,un_flow,uni_rev,chc(1,irmax))
                else
                  call ndahq(headm,un_flow,uni_rev,chn(1,irmax))
                endif
	        ivinc = 2
cgz	        if(uni_rev.lt.min_rev(irmax)) then
	        if(uni_rev.lt.(rev_lim(1)*type_ratio(irmax))) then
	          ier = 11
cgz	          uni_rev= min_rev(irmax)
	          uni_rev= rev_lim(1)*type_ratio(irmax)
                end if
	      else
                qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
                if (qh.lt.clim(3,irmax) ) then
                  call ndahq(headm,un_flow,uni_rev,chc(1,irmax))
                else
                  call ndahq(headm,un_flow,uni_rev,chn(1,irmax))
                endif
cgz	        if(uni_rev.lt.min_rev(irmax)) then
	        if(uni_rev.lt.(rev_lim(1)*type_ratio(irmax))) then
	          ier = 11
cgz	          uni_rev = min_rev(irmax)
	          uni_rev= rev_lim(1)*type_ratio(irmax)
	          ivinc = 2
	        else
	          ivinc = 0
	        end if
	      end if
	    end if
	  else
c----->           compressore al minimo del flusso
	    if(headm.gt.hlim(3)) then
c---->               compressore sulla sourge curve
              un_flow=qlim(irmax)
              qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,irmax) ) then
                call ndahq(headm,un_flow,uni_rev,chc(1,irmax))
              else
                call ndahq(headm,un_flow,uni_rev,chn(1,irmax))
              endif
c---->            compressore al minimo revolution rate
	    else
cgz	      uni_rev = min_rev(irmax)
              uni_rev = rev_lim(1)*type_ratio(irmax)
	    end if
	  end if
	  uni_rev = uni_rev/type_ratio(irmax)
	else
c---->                simulazione senza vincoli
	  if(ntp_eff.eq.1) then
c---->                  compressori di un unico tipo (type = itipo)
	    un_flow=flow_tot/type_actr(itipo)
            qaux=qlim(itipo)
            ad_flow = un_flow/qmrif
            ad_head = headm  /headrif
            qh=ad_flow*ad_flow/ad_head
      	    if (un_flow.lt.qaux) then
	      ier = 10
	      un_flow = qaux
              qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,itipo) ) then
                call ndahq(headm,un_flow,uni_rev,chc(1,itipo))
              else
                call ndahq(headm,un_flow,uni_rev,chn(1,itipo))
              endif
	      ivinc = 2
cgz	      if(uni_rev.lt.min_rev(itipo)) then
              if(uni_rev.lt.(rev_lim(1)*type_ratio(itipo))) then
	        ier = 11
cgz	        uni_rev= min_rev(itipo)
                uni_rev= rev_lim(1)*type_ratio(itipo)
	      end if
cgz	    else if (qh.gt.1./clim(4,itipo)) then
	    else if (qh.gt.1./clim_eff(itipo)) then
c---->                      punto oltre la choking curve
	      ier = 13
!              if (.not.iopt) then
              if (.not.iopt .and. .not.iopt_mess) then
	        call app_mess(vert_pointer,ier,mess)
              end if
              return
c gz ???
c	      un_flow = sqrt(headm/clim(3,itipo))
c	      if(crn(1,itipo).lt.0.) then
c	        uni_rev = un_flow* (crn(1,itipo)+
c     *			SQRT(crn(2,itipo)+crn(3,itipo)*
c     *			(headm/(un_flow*un_flow))))
c	      else
c	        uni_rev = un_flow* (crn(1,itipo)-
c     *			SQRT(crn(2,itipo)+crn(3,itipo)*
c     *			(headm/(un_flow*un_flow))))
c	      end if
c	      if(uni_rev.gt.max_rev(itipo)) uni_rev = max_rev(itipo)
c	      ivinc = 1
	    else
c---->                          calcolo del numero di giri
              qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,itipo) ) then
                call ndahq(headm,un_flow,uni_rev,chc(1,itipo))
              else
                call ndahq(headm,un_flow,uni_rev,chn(1,itipo))
              endif
cgz	      if (uni_rev.gt.max_rev(itipo)) then
	      if (uni_rev.gt.rev_lim(2)*type_ratio(itipo)) then
c---->                  revolution rate maggiore di max_rev
	        ier = 12
!                if (.not.iopt) then 
                if (.not.iopt .and. .not.iopt_mess) then
	          call app_mess(vert_pointer,ier,mess)
                end if
                return
c gz ???
c	        uni_rev = max_rev(itipo)
c	        ivinc = 1
cgz	      else if(uni_rev.lt.min_rev(itipo)) then
	      else if(uni_rev.lt.rev_lim(1)*type_ratio(itipo)) then
c---->          riciclo sul minimo numero di giri
	        ier = 11
cgz	        uni_rev= min_rev(itipo)
	        uni_rev= rev_lim(1)*type_ratio(itipo)
	        ivinc = 2
	      end if
	    end if
	    uni_rev = uni_rev/type_ratio(itipo)
	  else
c---->                                 compressori di tipo differente
	    if(headm.gt.hlim(3)) then
c---->         flusso sulla anti_surge curve
	      flow_min = 0.
	      do itip=1,ntp
	        if(type_actr(itip).gt.0) then
                  qaux=qlim(itip)
	          flow_min = flow_min+ type_actr(itip)*qaux
	        end if
	      end do
	      if(flow_tot.lt.flow_min) then
c---->                      stazione in riciclo
c---->         only one type on recirculation which one ?
	        ier = 21
	        return
	      end if
c---->                     revolution rate sulla anti_surge curve
              un_flow=qlim(itipo)
              qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,itipo) ) then
                call ndahq(headm,un_flow,rev_min,chc(1,itipo))
              else
                call ndahq(headm,un_flow,rev_min,chn(1,itipo))
              endif
	      rev_min = rev_min/type_ratio(itipo)
	    else
c---->             flow rate al minimo revolution rate
	      flow_min = 0.
	      do itip=1,ntp
	        if(type_actr(itip).gt.0) then
	          rev = rev_lim(1)*type_ratio(itip)
                  call hdan_ch(clim(3,itip),haux,rev,chn(1,itip))
                  if(headm.ge.haux) then
                    call qdahn(headm,qaux,rev,chn(1,itip))
                  else
                    call qdahn(headm,qaux,rev,chc(1,itip))
                  endif
                  flow_min = flow_min+ type_actr(itip)*qaux
	        end if
	      end do
	      if(flow_tot.lt.flow_min) then
c---->                      stazione in riciclo 
c---->         only one type on recirculation which one ?
	        ier = 22
	        return
	      else
	        rev_min = rev_lim(1)
	      end if
	    end if
c---->       massimo flusso alla stazione
	    if(headm.gt.hlim(2)) then
c---->            maximum flow at max_rev
	      flow_max = 0.
	      do itip=1,ntp
	        if(type_actr(itip).gt.0) then
	          rev = rev_lim(2)*type_ratio(itip)
                  call hdan_ch(clim(3,itip),haux,rev,chn(1,itip))
                  if(headm.ge.haux) then
                    call qdahn(headm,qaux,rev,chn(1,itip))
                  else
                    call qdahn(headm,qaux,rev,chc(1,itip))
                  endif
                  flow_max = flow_max+ type_actr(itip)*qaux
	        end if
	      end do
	      if(flow_tot.gt.flow_max) then
c---->               stazione oltre il massimo revolution rate
	        ier = 23
	        return
	      else
	        rev_max = rev_lim(2)
	      end if
	    else
c---->         flusso sulla anti_choke curve
	      flow_max = 0.
	      do itip=1,ntp
	        if(type_actr(itip).gt.0) then
                  call qdah_ch(clim_eff(itip),headm,qaux)
	          flow_max = flow_max + type_actr(itip)*qaux
	        end if
	      end do
	      if(flow_tot.gt.flow_max) then
c---->                      stazione oltre il massimo
	        ier = 24
	        return
	      end if
c---->                     revolution rate sulla anti_choke curve
              call qdah_ch(clim_eff(itipo),headm,un_flow)
              qh=( headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,itipo) ) then
                call ndahq(headm,un_flow,rev_max,chc(1,itipo))
              else
                call ndahq(headm,un_flow,rev_max,chn(1,itipo))
              endif
              rev_max = rev_max/type_ratio(itipo)
	    end if

c---->                 calcola il numero di giri
	    rev0 = rev_min
	    fa = zero_rev(rev_min)
	    fb = zero_rev(rev_max)
	    call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *			uni_rev,epsmi,pmach,zero_rev)
	  end if
	end if
	return
	end

	Subroutine compr_data(lturb,rev,itip,
     *			unit_rev,unit_flow,unit_eff,
     *			min_eff,unit_eff_corr,unit_power_corr,
cgpe     *			unit_power,unit_hrate,unit_cons,ier)
     *			unit_power,unit_hrate_corr,unit_hrate,unit_cons,ier)
c
c Calcola e assegna i parametri di una unita'
c
c	lturb        I	flag: calcolo o no dei dati di turbina
c	rev          I	revolution rate of compressor
c	itip         I	unit type
c	unit_rev     O	revolution rate of compressor
c	unit_flow    O	machine flow of compressor
c	unit_eff     O	efficiency of compressor
c	min_eff      I  minimum efficiency for compressor
c	unit_eff_corr I	efficiency correction of compressor
c	unit_power_corr I	turbine power correction
c	unit_power   O	power furnished by turbine
c	unit_hrate_corr I heat rate correction of turbine
c	unit_hrate   O	heat rate of turbine
c	unit_cons    O	consumption of turbine
c	ier          O	error index
c---------------------------------------------------------------------
        implicit none
c 
        include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
	include '../inc/td.inc'
	include '../inc/cc.inc'
c

        logical*2  lturb
        INTEGER*4  itip
	real*4   rev,unit_rev,unit_flow,unit_eff,min_eff(*),
     *		 unit_eff_corr,unit_power_corr,
     *		 unit_power,unit_hrate_corr,unit_hrate,unit_cons
        logical*2  lhr
        INTEGER*4  ier
c
        real*4   qf,dpw,haux
        real*4   rdummy
c
        rdummy = min_eff(1)  ! serve per evitare %FORT-I-VARUNUSED
c
	unit_rev = rev
        call hdan_ch(clim(3,itip),haux,unit_rev,chn(1,itip))
        if(headm.ge.haux) then
          call qdahn(headm,unit_flow,rev,chn(1,itip))
        else
          call qdahn(headm,unit_flow,rev,chc(1,itip))
        endif
c---->           efficiency
        call effdaqn(unit_eff,unit_flow,rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
	unit_eff = unit_eff*unit_eff_corr
cgpe
      if (unit_eff .lt. 0.01) unit_eff = 0.01
cgpe-end
	qf = unit_flow/aux3
cgpe        unit_power= AGRVcc * rocn * headm*qf/(unit_eff*unit_power_corr)
        unit_power= AGRVcc * rocn * headm*qf/unit_eff
c gz unit_power e' ora in Kjoule/ora. Conversione in KWatt:
        unit_power=unit_power/(3.6*1.e3)
c---->                                turbine simulation
	lhr = .true.
	ier = 0
      if (lturb) then
cgpe          call turb(rev,unit_power,unit_hrate,tair,cpwm(1,itip),
        call turb(itip,rev,unit_power_corr,unit_power,unit_hrate_corr,
     *       unit_hrate,tair,cpwm(1,itip),
     *       cpwt(1,itip),chr(1,itip),chrt(1,itip),dpw,delta,lhr,ier)
c   se unit_power e' in kWatt
c      unit_hrate e' in [kJ]/[kWh]
c      pclitd     e' in kJ/kNm3
cgpe  	  unit_cons = unit_hrate*unit_power/pclitd(ind)/4186.
  	  unit_cons = unit_hrate*(unit_power/unit_power_corr)/
     *              pclitd(ind)
  	  if(ier.gt.0) return
      endif

	return
	end

cgpe	Subroutine last_recover_station(vert_pointer,flow1,tipo_corr,
cgpe     *                                  ier)
csave	Subroutine last_recover_station(vert_pointer,flow1,ier)
c
c***********************************************************************
c	In caso di portata totale di macchina insufficiente,
c       e di mancanza di ulteriori macchine disponibili, impone
c       il valore di set sulla base dell'ultima verifica, compatibilmente
c       con il set richiesto dall'utente.
c  NB: se il set richiesto e' in portata, si prova ad imporre semplicemente la
c      portata massima lavorabile fornita in output dall'ultima verifica.
c      se il set e' in pressione,  si esce con errore e si termina la
c      verifica (spiegazione: violazione in portata non risolvibile con
c      aumento del numero di unita').

c------------------------------------------------------------------
csave      implicit none
c
csave      include '../inc/param.inc'
csave      include '../inc/stazione.inc'
csave      include '../inc/rk_param.inc'
csave      include '../inc/th.inc '	! :in
csave      include '../inc/tj.inc '	! :in/out
csave      include '../inc/jc.inc '	! :out
csave      include '../inc/stations.inc'
csave      include '../inc/units.inc'
c-UDM
csave      include '../inc/conv.inc'
c-UDM-end

csave      INTEGER*4   vert_pointer,ier
cgpe      INTEGER*4   tipo_corr
csave      real*4    flow1
csave      real*4    FF1,FF2
csave      logical*2     lvar,lnsta
csave      external      lnsta
csave      character*(max_len)  mess/' '/
c-UDM
csave      character*(max_len_str) udm_out_str
csave      external                udm_out_str
c-UDM-end
c
cgpe      if (htcitj(ind).eq.tc_q .or. htcitj(ind).eq.tc_pq .or.
cgpe     *    tipo_corr.eq.1) then
csave      if (hstatj(ind) .eq. sta_q) then
csave        ier = 8
c-UDM        FF1=FLOW1*CONV_NS
c-UDM        FF2=QSETTJ(IND)*CONV_NS
c-UDM        write(mess,555) FF2,FF1
csave        write(mess,555) udm_out_str(udm_Q,FLOW1,0),
csave     *                  udm_out_str(udm_Q,QSETTJ(IND),1)

!555     format(' Q di set: richiesta=',f9.3,'  -  proposta=',f9.3)
c-UDM555     format('Q: richiesta=',f9.3,' - proposta=',f9.3)
csave555     format('Q richiesta: ',A10,' - Q proposta: ',A25)
csave        call app_mess(vert_pointer,ier,mess)
cgpe        htcitj(ind)=tc_q
cgpe        qsettj(ind) = flow1
cgpe        lvar = lnsta(ind,sta_q,'Q')
cgpe        valsjc(ind) = qsettj(ind)
cgpe        call imp_new_sta(vert_pointer,flow1,'Q')
csave        call imp_new_sta(ind,vert_pointer,flow1,'Q')
cgpe-end
csave      else
csave        ier = 1
csave        mess = ' '
csave        call app_mess(vert_pointer,ier,mess)
csave      endif
c
csave      return
csave      end

      subroutine new_flusso(LFLOW,max_rev,min_rev,flow_work,stat_vars,
     *              pin,pout,PRES1_RIC,PRES2_ric,ACT_NUM,UNIT_EFF_CORR,
     *              UNIT_POWER_CORR,TYPE_QUANT,flow_ric,
c-ace18
     *              tin,vert_pointer)
c----------------------------------------------------------------------
c	lflow        I	flag per specificare se calcolare il primo valore
c                       della portata totale
c	max_rev      I	maximum revolution rate for each type
c	min_rev      I	minimum revolution rate for each type
c	flow_work    I/O portata (di macchina) totale richiesta dalla stazione
c                       nelle condizioni di massima
c	stat_vars    I	vettore di variabili di stazione
c	pin          I  pressione di ingresso ai compressori
c	pout         I  pressione di uscita dai compressori
c	PRES1_RIC    O  pressione di ingresso alla stazione richiesta
c	PRES2_RIC    O  pressione di mandata della stazione richiesta
c	act_num      I  numero di unita' attive per ogni tipo
c	flow_ric     O  portata in normal condition richiesta 
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
c
      LOGICAL*2  LFLOW
      real*4     min_rev(*),max_rev(*)
      real*4     flow_work,stat_vars(*),PRES1_RIC,PRES2_ric,flow_ric,
     *           UNIT_EFF_CORR(*),UNIT_POWER_CORR(*),pin,pout
      integer*4  ACT_NUM(*),TYPE_QUANT(*)
c
      real*4     uni_rev,qaux
c ricalcolo le nuove condizioni
      real*4     qq,delpr1,DELPR2,QAUX1
      real*4     rev0
      real*4     fa,fb

      real*4     ZERO_POW
      EXTERNAL   ZERO_POW
c
      integer*4  itip,IUN
      real*4     haux
c-ace18
      real*4 tin,ro_in,ro_out
	integer*4 vert_pointer
c-----------------------------------------------------------------------
      IF (LFLOW) THEN
        flow_work = 0.
        IUN=1
	do itip=1,ntp
          if(act_num(itip).gt.0) then
            call hdan_ch(clim(4,itip),haux,max_rev(itip),
     *                     chc(1,itip))
            if(headm.gt.haux) then
c---->           portata massima su max_rev
              uni_rev = max_rev(itip)
              call hdan_ch(clim(3,itip),haux,uni_rev,chn(1,itip))
              if(headm.ge.haux) then
                call qdahn(headm,qaux,uni_rev,chn(1,itip))
              else
                call qdahn(headm,qaux,uni_rev,chc(1,itip))
              endif
            else
c----->               massimo flusso sulla choking curve
              call qdah_ch(clim(4,itip),headm,QAUX)
              call ndaq_ch(clim(4,itip),QAUX,uni_rev,chc(1,itip))
            end if
C VERIFICA PER MASSIMA POTENZA
            un_eff_corr(1) = unit_eff_corr(IUN)
            un_pow_corr(1) = unit_power_corr(IUN)
            itipo = itip
            Fa = zero_pow(MIN_REV(itip))
            fb = zero_pow(MAX_REV(itip))
            if(fa.gt.0.) then
              if (fb.lt.0) then
c---->              e' possibile trovare una soluzione
                rev0 = (min_REV(itip)+max_REV(itip))/2.
                call zero_fun(min_REV(itip),max_REV(itip),rev0,fa,fb,
     *               uni_rev,epsmi,pmach,zero_pow)
                uni_rev = uni_rev-epsmi
                call hdan_ch(clim(3,itip),haux,uni_rev,chn(1,itip))
                if(headm.ge.haux) then
                  CALL QDAHN(headm,QAUX1,uni_rev,chn(1,itip))
                else
                  CALL QDAHN(headm,QAUX1,uni_rev,chc(1,itip))
                endif
              else
                qaux1 = 0
              endif
            ENDIF
            IF (QAUX1.LT.QAUX .and. qaux1.gt.0) QAUX=QAUX1              
            flow_WORK = flow_work + ACT_NUM(itip)*qaux
          ENDIF
          IUN = IUN + TYPE_QUANT(ITIP)
        ENDdo
      ENDif
C
      flow_ric = FLOW_WORK/aux3
      qq = flow_ric*flow_ric
c-ace18
      call ro_real(ind,pin,tin,ro_in)
      call ro_real(ind,pout,tin,ro_out)
cmar_p
      ro_in=1.
	ro_out=1.
cmar_p
      delpr1 = stat_vars(3)*qq/ro_in
      delpr2 = stat_vars(4)*qq/ro_out
      PRES1_RIC=PIN + DELPR1
      PRES2_RIC=POUT - DELPR2
      if (pres2_ric.gt.stat_vars(6)) PRES2_RIC=STAT_VARS(6)
      return
      end

      subroutine new_flusso_cp(LFLOW,rev_lim,tipi_ratio,clim_eff,
     *              flow_work,stat_vars,pin,pout,PRES1_RIC,PRES2_ric,
     *              ACT_NUM,UNIT_EFF_CORR,UNIT_POWER_CORR,TYPE_QUANT,
     *              flow_ric,
c-ace18
     *               tin,vert_pointer)
c----------------------------------------------------------------------
c calcolo del nuovo flusso in condizioni di massima, nel rispetto del
c rapporto dei giri (stazione composta, in parallelo, con una turbina
c per entrambe le fasi)
c----------------------------------------------------------------------
c	lflow        I	flag per specificare se calcolare il primo valore
c                       della portata totale
c	rev_lim      I	minimo e massimo rev_num nel rispetto del
c                       rapporto relativo
c	tipi_ratio   I	rapporto dei giri fra tipi
c	flow_work    I/O portata (di macchina) totale richiesta dalla stazione
c                       nelle condizioni di massima
c	stat_vars    I	vettore di variabili di stazione
c	pin          I  pressione di ingresso ai compressori
c	pout         I  pressione di uscita dai compressori
c	PRES1_RIC    O  pressione di ingresso alla stazione richiesta
c	PRES2_RIC    O  pressione di mandata della stazione richiesta
c	act_num      I  numero di unita' attive per ogni tipo
c	flow_ric     O  portata in normal condition richiesta 
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
c
      LOGICAL*2  LFLOW
      real*4     rev_lim(*)
      real*4     clim_eff(*)
      real*4     flow_work,stat_vars(*),PRES1_RIC,PRES2_ric,flow_ric,
     *           UNIT_EFF_CORR(*),UNIT_POWER_CORR(*),pin,pout
      real*4     tipi_ratio(*)
      integer*4  ACT_NUM(*),TYPE_QUANT(*)
c
      real*4     uni_rev,qaux
c ricalcolo le nuove condizioni
      real*4     qq,delpr1,DELPR2,QAUX1
      real*4     rev0
      real*4     fa,fb

      real*4     ZERO_POW_cp
      EXTERNAL   ZERO_POW_cp
c
      integer*4  itip,fase_uno,fase_due
      real*4     haux,raux1,raux2,haux1,qh
c-ace18
      real*4 tin,ro_in,ro_out
	integer*4 vert_pointer
c-----------------------------------------------------------------------


 
      IF (LFLOW) THEN
        fase_uno=1
        fase_due=1+type_quant(1)
        flow_work = 0.
        do itip=1,ntp
          if(act_num(itip).gt.0) then
            raux2 = rev_lim(2)*tipi_ratio(itip)
            call hdan_ch(clim(3,itip),haux1,raux2,chn(1,itip))
            call hdan_ch(clim_eff(itip),haux,raux2,chn(1,itip))
            if (haux.lt.haux1) then
              call hdan_ch(clim_eff(itip),haux,raux2,chc(1,itip))
            endif
            if(headm.gt.haux) then
c--->           portata massima su max_rev
              uni_rev = rev_lim(2)*tipi_ratio(itip)
              call hdan_ch(clim(3,itip),haux,uni_rev,chn(1,itip))
              if(headm.ge.haux) then
                call qdahn(headm,qaux,uni_rev,chn(1,itip))
              else
                call qdahn(headm,qaux,uni_rev,chc(1,itip))
              endif
            else
c----->               massimo flusso sulla choking curve
              call qdah_ch(clim_eff(itip),headm,QAUX)
              qh=( headm/(qaux*qaux))*((qmrif*qmrif)/headrif)
              if (qh.lt.clim(3,itip)) then
                call ndaq_ch(clim_eff(itip),QAUX,uni_rev,chc(1,itip))
              else
                call ndaq_ch(clim_eff(itip),QAUX,uni_rev,chn(1,itip))
              end if
            end if
C VERIFICA PER MASSIMA POTENZA
            un_eff_corr(1) = unit_eff_corr(fase_uno)
            un_pow_corr(1) = unit_power_corr(fase_uno)
            un_eff_corr(2) = unit_eff_corr(fase_due)
            un_pow_corr(2) = unit_power_corr(fase_due)
            rapp_giri=tipi_ratio(2)/tipi_ratio(1)
            itipo = itip
            raux1 = rev_lim(1)*tipi_ratio(itip)
            Fa = zero_pow_cp(raux1)
            Fb = zero_pow_cp(raux2)
            if(fa.gt.0.) then
              if (fb.lt.0) then
c---->              e' possibile trovare una soluzione
                rev0 = (raux1+raux2)/2.
                call zero_fun(raux1,raux2,rev0,fa,fb,
     *               uni_rev,epsmi,pmach,zero_pow_cp)
                uni_rev = uni_rev-epsmi
                call hdan_ch(clim(3,itip),haux,uni_rev,chn(1,itip))
                if(headm.ge.haux) then
                  CALL QDAHN(headm,QAUX1,uni_rev,chn(1,itip))
                else
                  CALL QDAHN(headm,QAUX1,uni_rev,chc(1,itip))
                endif
              else
                qaux1 = 0
              endif
            ENDIF
            IF (QAUX1.LT.QAUX .and. qaux1.gt.0) QAUX=QAUX1              
            flow_WORK = flow_work + ACT_NUM(itip)*qaux
          ENDIF
        ENDdo
      ENDif
C
      flow_ric = FLOW_WORK/aux3
      qq = flow_ric*flow_ric
c-ace18
      call ro_real(ind,pin,tin,ro_in)
      call ro_real(ind,pout,tin,ro_out)
cmar_p
      ro_in=1.
	ro_out=1.
cmar_p
      delpr1 = stat_vars(3)*qq/ro_in
      delpr2 = stat_vars(4)*qq/ro_out
       
     

      PRES1_RIC=PIN + DELPR1
      PRES2_RIC=POUT - DELPR2
      if (pres2_ric.gt.stat_vars(6)) PRES2_ric=STAT_VARS(6)


      

      return
      end

	Subroutine recover_Compr(vert_pointer,min_rev,max_rev,rev_lim,
     *			clim_eff,qlim,type_quant,status,lsim,
     *			ivinc,irmax,lric,uni_rev,
     *			flow_ass,flow_work,unit_flow,ier)
c
c       Tentativo di recover del compressore.
c
c	min_rev    I	minimum revolution rate for each type
c	max_rev    I	maximum revolution rate for each type
c	rev_lim    I	effective station revolution rate limits
c	clim_eff   I	effective limit curve coefficients
c	qlim       I	limiti in portata dei compressori sulla surge effettiva
c	type_quant I	number of unit of each type
c	status     I	status of each unit (T=active)
c	lsim       I/O	status of each unit (T=not simulated)
c	ivinc      I/O	type of constraint
c	irmax      I/O  constraint active or not (=0 not active)
c	lric       I    flag for recirculation condition
c	uni_rev    I/O  revolution rate
c	flow_ass   I/O  flow rate already assigned at simulated units
c	flow_work  O    flow worked by the unit recovered
c	unit_flow  I/O  flow worked by each unit
c	ier        I/O  error index
c-----------------------------------------------------------------------
        implicit none

	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
        real*4  min_rev(*),max_rev(*),rev_lim(*),clim_eff(*),
     *		unit_flow(*),qlim(*)
	INTEGER*4 vert_pointer
	INTEGER*4 type_quant(*)
	LOGICAL*2 lric,lsim(*),status(*)
        INTEGER*4 ier
        INTEGER*4 irmax
        INTEGER*4 ivinc
        real*4  flow_ass,flow_work

        real*4  rmin,rmax
        INTEGER*4 itip,nutot,ic,i
        real*4  un_flow,qh,uni_rev
c
        real*4  q_effe,q_real,diff
        INTEGER*4 irange
        character*(max_len) mess/' '/
c----------------------------------------------------------------------
	if(ier.gt.20) then
!          if (.not.iopt) then
cgpe-corr          if (.not.iopt .and. .not.iopt_mess) then
cgpe-corr            mess = ' '
cgpe-corr            call app_mess(vert_pointer,ier,mess)
cgpe-corr          end if
c---->      e' necessario fissare un vincolo su un tipo di compressore
	  irmax = 0
	  if(ier.eq.21) then
c    Un compressore in riciclo sulla anti-surge curve
c    Il compressore con la curva effettiva di riciclo piu' vicina alla
c    reale curva di riciclo, per il valore headm, viene posto al minimo
	    rmin = 1000000.
	    do itip=1,ntp
	      if(type_actr(itip).gt.0) then
                call QDAH_SU(a_coef(1,itip),b_coef(1,itip),
     *               nlim(itip),lim_h(1,itip),headm,q_effe,irange)
                q_real=qlim(itip)
                diff=abs(q_effe-q_real)
	        if(diff.lt.rmin) then
	          rmin = diff
	          irmax = itip
	        end if
	      end if
	    end do
	    ivinc = 2
	  else if(ier.eq.22) then
c    Un compressore in riciclo sul minimo numero di giri
c    Il compressore con il minimo type-ratio viene posto al minimo
	    rmin = 10000.
	    do itip=1,ntp
	      if(type_actr(itip).gt.0) then
cgz	        if((type_ratio(itip)*min_rev(itip)).lt.rmin) then
	        if((type_ratio(itip)*rev_lim(1)).lt.rmin) then
	          rmin = type_ratio(itip)*rev_lim(1)
	          irmax = itip
	        end if
	      end if
	    end do
	    ivinc = 2
	  else if(ier.eq.23) then
c    Un compressore sul massimo numero di giri
c    Il compressore con il massimo type-ratio viene posto al massimo
	    rmax = 0.
	    do itip=1,ntp
	      if(type_actr(itip).gt.0) then
	        if((type_ratio(itip)*rev_lim(2)).gt.rmax) then
	          rmax = type_ratio(itip)*rev_lim(2)
	          irmax = itip
	        end if
	      end if
	    end do
	    ivinc = 1
	  else
c    Un compressore sulla anti choke curve.
c    Il compressore con l'effettiva choking curve piu' vicina alla reale
c    choking curve viene posto al massimo
	    rmax = 1000.
	    do itip=1,ntp
	      if(type_actr(itip).gt.0) then
	        if((clim_eff(itip)-clim(4,itip)).le.rmax) then
	          rmax = clim_eff(itip)-clim(4,itip)
	          irmax = itip
	        end if
	      end if
	    end do
	    ivinc = 1
	  end if
	  ier = 0
	  rev_lim(1) = min_rev(irmax)/type_ratio(irmax)
	  rev_lim(2) = max_rev(irmax)/type_ratio(irmax)
	else
c---->             compressori di un solo tipo
	  if(ier.le.11) then
c---->          Compressore in riciclo
c---->          Verifico se esiste un compressore al massimo.
	    nutot = 0
	    do itip=1,ntp
	      do ic=1,type_quant(itip)
	        i = nutot+ic
	        if(status(i).and.(.not.lsim(i))) then
c---->            Unita' attiva e gia' simulata
	          if(lvinc(i).ne.2) then
c---->               Stazione al massimo
	            lsim(i) = .true.
	            flow_ass = flow_ass-unit_flow(i)/aux3
	            unit_flow(i) = 0.
	            type_actr(itip) = type_actr(itip)+1
	            ier = 0
	          end if
	        end if
	      end do
	      nutot = nutot+type_quant(itip)
	    end do
c---->              Verifico se il recover ha successo.
	    if(ier.eq.0) then
	      lric = .false.
	      irmax = itipo
	    else
	      lric = .true.
	      flow_work = flow_tot/type_actr(itipo)
	    end if
	    ivinc = 2
	  else if(ier.eq.14) then
c---->   Cerco il revolution rate per tutto il gas
	    un_flow = flow_tot/type_actr(irmax)
	    qh = (headm/(un_flow*un_flow))*((qmrif*qmrif)/headrif)
	    if (qh.lt.clim(3,irmax)) then
c choking zone
              call ndahq(headm,un_flow,uni_rev,chc(1,irmax))
            else
              call ndahq(headm,un_flow,uni_rev,chn(1,irmax))
            endif
	    uni_rev = uni_rev/type_ratio(irmax)
	    ier = 0
	    ivinc = 0
	  end if
	end if
	return
	end

	Subroutine recover_Compr_cp(vert_pointer,min_rev,max_rev,
     *			rev_lim,qlim,type_quant,ivinc,irmax,ier)
c
c       Tentativo di recover del compressore (stazione composta, in parallelo)
c
c	min_rev    I	minimum revolution rate for each type
c	max_rev    I	maximum revolution rate for each type
c	rev_lim    I	effective station revolution rate limits
c	qlim       I	limiti in portata dei compressori sulla surge effettiva
c	type_quant I	number of unit of each type
c	ivinc      I/O	type of constraint
c	irmax      I/O  constraint active or not (=0 not active)
c	ier        I/O  error index
c-----------------------------------------------------------------------
        implicit none
c 
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
        real*4 min_rev(*),max_rev(*),rev_lim(*),qlim(*)
	INTEGER*4 vert_pointer
	INTEGER*4 type_quant(*)
        INTEGER*4 ier
        INTEGER*4 irmax
        INTEGER*4 ivinc

        real*4  rmin
        INTEGER*4 itip
c
        real*4  q_effe,q_real,diff
        INTEGER*4 irange,iw
        character*(max_len) mess/' '/
        real*4    rdummy
        integer*4 idummy
c----------------------------------------------------------------------
      rdummy = min_rev(1)    !  serve per evitare %FORT-I-VARUNUSED
      rdummy = max_rev(1)    !  serve per evitare %FORT-I-VARUNUSED
      idummy = type_quant(1) !  serve per evitare %FORT-I-VARUNUSED
c
!-28/08/2000-intervento sul riciclo
!!      if (.not.iopt) then
!      if (.not.iopt .and. .not.iopt_mess) then
!        mess = ' '
!cgpe        iw = 59
!        iw = 49
!        call app_mess(vert_pointer,iw,mess)
!      end if
!-28/08/2000-end
c---->      e' necessario fissare un vincolo su un tipo di compressore
      irmax = 0
      if(ier.eq.21) then
c    Un compressore in riciclo sulla anti-surge curve
c    Il compressore con la curva effettiva di riciclo piu' lontana dalla
c    reale curva di riciclo, per il valore headm, viene posto al minimo
c    Poiche' e' una stazione composta, l'altro compressore verra' simulato in
c    riciclo sulla sua propria curva di riciclo.
        rmin = -1.
        do itip=1,ntp
          if(type_actr(itip).gt.0) then
            call QDAH_SU(a_coef(1,itip),b_coef(1,itip),
     *               nlim(itip),lim_h(1,itip),headm,q_effe,irange)
            q_real=qlim(itip)
            diff=abs(q_effe-q_real)
            if(diff.gt.rmin) then
              rmin = diff
              irmax = itip
            end if
          end if
        end do
        ivinc = 2
      else if(ier.eq.22) then
c    Un compressore in riciclo sul minimo numero di giri
c    Il compressore con il massimo type-ratio viene posto al minimo
        rmin = -1.
        do itip=1,ntp
          if(type_actr(itip).gt.0) then
c            if((type_ratio(itip)*min_rev(itip)).gt.rmin) then
            if((type_ratio(itip)*rev_lim(1)).gt.rmin) then
              rmin = type_ratio(itip)*rev_lim(1)
              irmax = itip
            end if
          end if
        end do
        ivinc = 2
      endif
      ier = 0
cgz      rev_lim(1) = min_rev(irmax)/type_ratio(irmax)
cgz      rev_lim(2) = max_rev(irmax)/type_ratio(irmax)
      return
      end

	Subroutine recover_turb(vert_pointer,icomp,
     *			rev_max,min_rev,min_eff,rev,
     *			unit_eff_corr,unit_power_corr,unit_flow,
     *			flow_ass,status,lsim,type_quant,ier)
c
c	Calcola il massimo flusso necessario per rispettare la
c	massima potenza fornibile dalla turbina
c
c	icomp      I	compressor index
c	rev_max    I	revolution rate where pwmax<power required
c	min_rev    I	minimum revolution rate for each compressor type
c	min_eff    I	minimum efficiency for each compressor type
c	rev        I/O	revolution rate determined
c	unit_eff_corr I	efficiency correction of each compressor
c	unit_power_corr I power correction of each turbine
c	unit_flow  I	flow worked by each compressor
c	flow_ass   I/O  flow rate already assigned at simulated units
c	status     I	status of each unit (T=active)
c	lsim       I/O	status of each unit (T=not simulated)
c	type_quant I    number of units of each type
c	ier        I/O  error index
c
c=====>    sul common simpstat
c	itipo	 compressor type
c
c----------------------------------------------------------------------
        implicit none
c 
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
        INTEGER*4   vert_pointer,icomp
	real*4    min_rev(*),unit_eff_corr(*),unit_power_corr(*),
     *			unit_flow(*),min_eff(*)
	INTEGER*4   type_quant(*)
	LOGICAL*2 status(*),lsim(*)
        real*4    flow_ass
        INTEGER*4   ier,iw

        INTEGER*4   nutot,itip,ic,i
        real*4    qa,qb
        real*4    fa,fb
        real*4    rev0,rev_max,rev,rev_min
        real*4    qaux,raux
        character*(max_len) mess/' '/
        INTEGER*4   irange
	real*4    zero_pow
	EXTERNAL  zero_pow
	real*4    zero_pow_su
	EXTERNAL  zero_pow_su
c----------------------------------------------------------------------
c verifica sulle altre unita'
	nutot = 0
	do itip=1,ntp
	  do ic=1,type_quant(itip)
	    i = nutot+ic
	    if(status(i).and.(i.ne.icomp)) then
	      if(.not.lsim(i)) then
c---->           Unita' attiva e gia' simulata
c	        if(lvinc(i).eq.0) then
c gz correction 27-5-93
	        if(lvinc(i).ne.1) then
c---->               stazione non al massimo 
	          lsim(i) = .true.
	          flow_ass = flow_ass-unit_flow(i)/aux3
	          unit_flow(i) = 0.
	          type_actr(itip) = type_actr(itip)+1
	          ier = 0
	        end if
	      else
c---->            Unita' attiva e non simulata
	        ier = 0
	      end if
	    end if
	  end do
	  nutot = nutot+type_quant(itip)
	end do
c---->             definizione completa del common simpstat
	un_eff_corr(1) = unit_eff_corr(icomp)
	un_pow_corr(1) = unit_power_corr(icomp)
	effmi = min_eff(itipo)
c---->	 intervallo di ricerca
        call QDAH_SU(a_coef(1,itipo),b_coef(1,itipo),
     *               nlim(itipo),lim_h(1,itipo),headm,qaux,irange)
c uso chn perche' sono sicuramente sulla surge
        call ndahq(headm,qaux,rev_min,chn(1,itipo))
	if(rev_min.lt.min_rev(itipo)) rev_min = min_rev(itipo)
	Fa = zero_pow(rev_min)
	if(fa.gt.0.) then
c---->               e' possibile trovare una soluzione
	  rev0 = (rev_min+rev_max)/2.
	  fb = zero_pow(rev_max)
	  call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *			rev,epsmi,pmach,zero_pow)
	  ier = 0
	  rev = rev-epsmi
	else
c---->       al minimo numero di giri la potenza richiesta e' maggiore della
c            massima fornibile dalla turbina
	  rev = rev_min
c
c---->             definizione completa del common simpstat
          un_eff_corr(1) = unit_eff_corr(icomp)
	  un_pow_corr(1) = unit_power_corr(icomp)
	  effmi = min_eff(itipo)
          Fa = zero_pow(min_rev(itipo))
          if(fa.gt.0.) then
c---->               e' possibile trovare una soluzione
            qa=lim_q(1,itipo)*qmrif
            qb=lim_q(nlim(itipo),itipo)*qmrif
            rev0 = (qa+qb)/2.
	    fa = zero_pow_su(qa)
	    fb = zero_pow_su(qb)
	    call zero_fun(qa,qb,rev0,fa,fb,qaux,epsmi,pmach,zero_pow_su)
            call NDAQ_SU(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *               lim_q(1,itipo),qaux,chn(1,itipo),raux,irange)
            call hdaqn(headm,qaux,raux,chn(1,itipo))
            ier = 16
          else  
!            if (.not.iopt) then
            if (.not.iopt .and. .not.iopt_mess) then
              iw = 18
              mess = ' '
              call app_mess(vert_pointer,iw,mess)
            end if
            ier = 1
          endif
          return
	end if
c---->    flag per la condizione operativa
	lvinc(icomp) = 1
	lsim(icomp) = .false.
	type_actr(itipo) = type_actr(itipo)-1
	return
	end

	Subroutine limit(ntp,type_actr,type_ratio,max_rev,
     *			min_rev,rev_lim,itip_max,ier)
c
c Calcola il massimo e il minimo numero di giri ammissibili
c
c	ntp         I	number of type
c	type_actr   I	number of active compressor to use
c	type_ratio  I	parameter for the flow rate distribution
c	max_rev     I	maximum revolution rate for each unit type
c	min_rev     I	minimum revolution rate for each unit type
c	rev_lim     O	effective revolution rate limits
c	itip_max    O	tipo avente il massimo valore del ratio
c	ier         O	error index
c
c	ier not equal 0 the unit of type ier must be at the maximum
c                       revolution rate
c       ier = 0 then rev_lim(1) = minimum revolution rate
c                    rev_lim(2) = maximum revolution rate
c----------------------------------------------------------------------
        implicit none
c
	REAL*4	type_ratio(*),rev_lim(2),max_rev(*),min_rev(*),rmax
	INTEGER*4 ntp,type_actr(*),itip_max
c
        INTEGER*4 itm,itip,ier
        real*4  rm
c---------------------------------------------------------------------
	rev_lim(1) = 0.
	rev_lim(2) = 1000.
	rmax     = 0.
	itm      = 0
	itip_max = 0
c---->     compute the minimum and maximum 
	do itip=1,ntp
	  if(type_actr(itip).gt.0) then
	    if(type_ratio(itip).gt.rmax) then
	      rmax     = type_ratio(itip)
	      itm      = itip
	      itip_max = itip
	    end if
	    rm = max_rev(itip)/type_ratio(itip)
	    if(rm.lt.rev_lim(2)) rev_lim(2) = rm
	    rm = min_rev(itip)/type_ratio(itip)
	    if(rm.gt.rev_lim(1)) rev_lim(1) = rm
	  end if
	end do
c---->      verify the result
	if(rev_lim(2).le.rev_lim(1)) then
	  ier = itm
	  rev_lim(2) = max_rev(itm)/type_ratio(itm)
	  rev_lim(1) = min_rev(itm)/type_ratio(itm)
	else
	  ier = 0
	end if
	return
	end

	Subroutine fact_lim(itip_max,hlim,flow_lim,clim_eff,rev_lim,
     *                      max_rev,min_rev,irmax,ivinc)
c
c	Calcola i punti estremi della stazione
c
c	itip_max    I	tipo avente il massimo valore del ratio
c	hlim        O	station or unit limits
c		1  max rev and recirculation
c		2  max rev and anti-choke
c		3  min rev and recirculation
c		4  min rev and anti-choke
c	flow_lim    O	station or unit limits for flow rate
c		1  max rev and recirculation
c		2  max rev and anti-choke
c		3  min rev and recirculation
c		4  min rev and anti-choke
c	clim_eff    O	effective limit curve for each compressor on
c		        anti choke curve
c	rev_lim     I/O	effective revolution rate limits
c	max_rev     I	maximum revolution rate for each unit type
c	min_rev     I	minimum revolution rate for each unit type
c	irmax         O   the unit of type irmax must be at the maximum
c                       revolution rate
c	ivinc      I/O	type of constraint
c----------------------------------------------------------------------
        implicit none
c
	include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
	REAL*4	hlim(*),clim_eff(*),rev_lim(2),max_rev(*),min_rev(*),
     *          flow_lim(*)
c
        INTEGER*4 imin,imax,itip,irange,i,itip_max,irmax
        INTEGER*4 ivinc
        real*4  rev,h,qaux,haux
c---------------------------------------------------------------------
10      continue
	hlim(1) = 10000.
	hlim(2) = 0.
	if(itipo.gt.0) then
	  imin = itipo
	  imax = itipo
	else
	  imin = 1
	  imax = ntp
	end if
	do itip = imin,imax
	   if(type_actr(itip).gt.0) then
c---->      massima altezza adiabatica ammissibile per il compressore
	      rev = rev_lim(2)*type_ratio(itip)
c uso chn perche' sono sulla surge
              call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *                  nlim(itip),lim_n(1,itip),rev,h,qaux,irange)
	      if(h.lt.hlim(1)) then
c---->      ricerca del minimo fra i massimi
		 hlim(1) = h
c                 if(hlim(1).lt.hlim(2)) hlim(2) = 0.
	      end if
c---->      calcola il minimo h al massimo rev
              call hdan_ch(clim(4,itip),h,rev,chc(1,itip))
c---->      calcola il massimo fra i minimi
c              if((h.gt.hlim(2)).and.(h.lt.hlim(1))) hlim(2) = h
	      if(h.gt.hlim(2)) hlim(2) = h
	  end if
	end do
        if(hlim(1).lt.hlim(2) .and. itipo .eq.0) then
c in questa situazione assumo che non possa essere rispettato il rapporto
c dei giri richiesto, e quindi impongo subito una simulazione vincolata al
c massimo sul tipo avente il massimo valore del ratio
	  rev_lim(2) = max_rev(itip_max)/type_ratio(itip_max)
	  rev_lim(1) = min_rev(itip_max)/type_ratio(itip_max)
          irmax = itip_max
          itipo = itip_max
          ivinc = 1
          goto 10
        endif
	do itip=imin,imax
	  if(type_actr(itip).gt.0) then
c---->         flusso al massimo numero di giri
c---------->         SullA choking curve
	    rev = rev_lim(2)*type_ratio(itip)
            call hdan_ch(clim(3,itip),h,rev,chn(1,itip))
	    if(hlim(2).ge.h) then
c normal
              call qdahn(hlim(2),qaux,rev,chn(1,itip))
            else
              call qdahn(hlim(2),qaux,rev,chc(1,itip))
            endif
            qaux=qaux/qmrif
            haux=hlim(2)/headrif
	    clim_eff(itip) = haux/(qaux)**2
	  else
	    clim_eff(itip) = 1.
	  end if
	end do
c---->       Calcolo del minimo numero di giri
	hlim(3) = 10000.
	hlim(4) = 0.
	do itip = imin,imax
	   if(type_actr(itip).gt.0) then
c---->       massima altezza adiabatica
	      rev = rev_lim(1)*type_ratio(itip)
              call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *                  nlim(itip),lim_n(1,itip),rev,h,qaux,irange)
	      if(h.lt.hlim(3)) hlim(3) = h
c---->      calcola il minimo h al minimo rev
              call hdan_ch(clim(4,itip),h,rev,chc(1,itip))
c---->        massimo fra i minimi
c	      if((h.gt.hlim(4)).and.(h.lt.hlim(3))) hlim(4) = h
	      if (h.gt.hlim(4)) hlim(4) = h
	  end if
	end do
        if(hlim(4).gt.hlim(3) .and. itipo .eq.0) then
c in questa situazione assumo che non possa essere rispettato il rapporto
c dei giri richiesto, e quindi impongo subito una simulazione vincolata al
c massimo sul tipo avente il massimo valore del ratio
	  rev_lim(2) = max_rev(itip_max)/type_ratio(itip_max)
	  rev_lim(1) = min_rev(itip_max)/type_ratio(itip_max)
          irmax = itip_max
          itipo = itip_max
          ivinc = 1
          goto 10
        endif
c
c---->                       determinazione delle portate estreme
c                                        e curve limite effettive
        do i=1,4
           flow_lim(i) = 0.
        end do
	do itip = imin,imax
          if(type_actr(itip).gt.0) then
c---->                          portate al numero massimo di giri
            rev = rev_lim(2)*type_ratio(itip)
            call hdan_ch(clim(3,itip),h,rev,chn(1,itip))
	    if(hlim(1).ge.h) then
              call qdahn(hlim(1),qaux,rev,chn(1,itip))
            else
              call qdahn(hlim(1),qaux,rev,chc(1,itip))
            endif
            flow_lim(1) = flow_lim(1)+type_actr(itip)*qaux
	    if(hlim(2).ge.h) then
              call qdahn(hlim(2),qaux,rev,chn(1,itip))
            else
              call qdahn(hlim(2),qaux,rev,chc(1,itip))
            endif
            flow_lim(2) = flow_lim(2)+type_actr(itip)*qaux
c---->                          portate al numero minimo di giri
            rev = rev_lim(1)*type_ratio(itip)
            call hdan_ch(clim(3,itip),h,rev,chn(1,itip))
	    if(hlim(3).ge.h) then
              call qdahn(hlim(3),qaux,rev,chn(1,itip))
            else
              call qdahn(hlim(3),qaux,rev,chc(1,itip))
            endif
            flow_lim(3) = flow_lim(3)+type_actr(itip)*qaux
	    if(hlim(4).ge.h) then
              call qdahn(hlim(4),qaux,rev,chn(1,itip))
            else
              call qdahn(hlim(4),qaux,rev,chc(1,itip))
            endif
            flow_lim(4) = flow_lim(4)+type_actr(itip)*qaux
          end if
        end do
c
	return
	end

	real*4 Function zero_rev(rev)
c
c       Funzione per la definizione del revolution rate
c
c	Tutti i parametri sono passati tramite il common "simpstat"
c----------------------------------------------------------------------
        implicit none
c
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
        real*4   rev
c
        real*4   flow_ext,rrev,qaux,haux
        INTEGER*4  itip
c----------------------------------------------------------------------
	flow_ext = 0.
	do itip=1,ntp
	  if(type_actr(itip).gt.0) then
	    rrev = rev*type_ratio(itip)
            call hdan_ch(clim(3,itip),haux,rrev,chn(1,itip))
	    if(HEADM.ge.haux) then
              call qdahn(headm,qaux,rrev,chn(1,itip))
            else
              call qdahn(headm,qaux,rrev,chc(1,itip))
            endif
            flow_ext = flow_ext+type_actr(itip)*qaux
	  end if
	end do
	zero_rev = flow_tot-flow_ext
	return
	end

	subroutine turb(itip,rev,power_corr,power,hrate_corr,hrate,tair,
CGPE     *			cpwm,cpwt,chr,chrt,dpw,lhr,ier)
     *			cpwm,cpwt,chr,chrt,dpw,delta,lhr,ier)
c
c	Simulazione della turbina
c
c     itip    I   indice del tipo
c	rev     I	revolution rate
c	power_corr I power correction
c	power   I	power required
c	hrate_corr I heat rate correction
c	hrate   O	heat rate determined
c	tair    I	air temperature
c	cpwm    I	maximum power coefficients
c	cpwt    I	correction of maximum power coefficients
c	chr     I	heat rate coefficients
c	chrt    I	correction of heat rate coefficients
c	dpw     O	difference max_power-power
c	lhr     I       logical for the heat rate computation (T)
c	ier     O	error index
c-----------------------------------------------------------------
      implicit none
c
      integer*4 itip
      real*4    rev,tair,power,hrate
      real*4    power_corr,hrate_corr
      real*4    dpw
	real*4    cpwm(*),cpwt(*),chr(*),chrt(*)
      real*4    pow_app ! potenza pw(0)
      real*4    delta,fc,tadim
      INTEGER*4 ier     
c
      real*4    pwmax
	LOGICAL*2 lhr
      real*4    eps_pow/0.1/
c*******************************************************************************
c-mf03      call real_minpower(itip,power)
c---->     calcolo della massima potenza:
c Il valore di pwmax calcolato dalla pwdan è una potenza all'altitudine h=0
c e alla temperatura tair (in generale diverso da 15°)
      call pwdan(pwmax,rev,tair,cpwm,cpwt)
c-mf03      call real_maxpower(itip,pwmax)

cgpe	dpw = pwmax-power
c Viene corretta la massima potenza pwmax con power_corr che contiene il fattore
c di derating della turbina e il fattore delta di correzione dovuto all'altitudine.
c Pertanto il termine pwmax*power_corr e' a condizioni reali, paragonabile al power
c (ON SITE e in KW)
	dpw = pwmax*power_corr-power

	if(lhr) then
c---->     calcolo dell'heat rate
c-old        pow_app = power/power_corr
        pow_app = power ! potenza ON SITE
c-da-manuale        pow_app = pow_app*(2. - (power_corr/delta))
c correzione dovuta all'altitudine
        pow_app = pow_app/delta
c correzione dovuta alla tair
c        fc = (700./tair-1)/(700./(15.+273.15)-1)
        tadim = tair/(15.+273.15) 
        fc    = cpwt(1)+tadim*(cpwt(2)+tadim*
     *          (cpwt(3)+cpwt(4)*tadim))
        pow_app = pow_app/fc
c pertanto pow_app passato alla subroutine hrdapn è una potenza ISO
        call hrdapn(hrate,pow_app,rev,tair,chr,chrt)
c Il valore hrate calcolato dalla subroutine hrdapn e' ON SITE (alla temperatura
c reale tair)
c Viene corretto hrate con la variazione percentuale del consumo specifico della turbina.
        hrate = hrate * hrate_corr
cgpe        hrate = hrate + hrate_corr*fc
	  if (dpw.le.0.) then
	    IER = 15
	    RETURN
	  end if  
	end if
	RETURN                                             
	END                                                

	REAL*4 Function zero_pow(rev)
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
	include '../inc/cc.inc'
c
	LOGICAL*2 lhr
        real*4  rev,un_flow,un_eff,qf,un_power,hrate,dpw,haux
        INTEGER*4 ier
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
	un_eff = un_eff*un_eff_corr(1)
cgpe
      if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
	qf = un_flow/aux3
c---->          power required
cgpe        un_power= AGRVcc * rocn * headm*qf/(un_eff*un_pow_corr(1))
        un_power= AGRVcc * rocn * headm*qf/un_eff
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
        un_power=un_power/(3.6*1.e3)
c
	lhr = .false.
cgpe	call turb(rev,un_power,hrate,tair,
	call turb(itipo,rev,un_pow_corr(1),un_power,hrate_corr(itipo),
     *          hrate,tair,cpwm(1,itipo),cpwt(1,itipo),
     *		  chr(1,itipo),chrt(1,itipo),dpw,delta,lhr,ier)
	zero_pow = dpw
	return
	end

	REAL*4 Function zero_pow_su(flow)
c
c  Calcola la differenza fra la potenza richiesta dal compressore e
c  la potenza fornita dalla turbina alla portata assegnata sulla curva
c  di anti surge
c
c  Tutti gli altri parametri sono forniti dal common "simpstat"
c----------------------------------------------------------------------
        implicit none
c 
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
	include '../inc/cc.inc'
c
	LOGICAL*2 lhr
        real*4  flow,rev,un_eff,qf,un_power,hrate,dpw,haux
        INTEGER*4 ier,irange
c-----------------------------------------------------------------
        call NDAQ_SU(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *               lim_q(1,itipo),flow,chn(1,itipo),rev,irange)
        call hdaqn(haux,flow,rev,chn(1,itipo))
c---->           efficiency
        call effdaqn(un_eff,flow,rev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))
	un_eff = un_eff*un_eff_corr(1)
cgpe
      if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
	qf = flow/aux3
c---->          power required
cgpe        un_power= AGRVcc * rocn * haux*qf/(un_eff*un_pow_corr(1))
        un_power= AGRVcc * rocn * haux*qf/un_eff
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
        un_power=un_power/(3.6*1.e3)
	lhr = .false.
cgpe	call turb(rev,un_power,hrate,tair,
	call turb(itipo,rev,un_pow_corr(1),un_power,hrate_corr(itipo),
     *     hrate,tair,
     *	 cpwm(1,itipo),cpwt(1,itipo),chr(1,itipo),chrt(1,itipo),
     *     dpw,delta,lhr,ier)
	zero_pow_su = dpw
	return
	end

	REAL*4 Function zero_pow_cp(rev)
c
c  Calcola la differenza fra la potenza richiesta dal compressore e
c  la potenza fornita dalla turbina al numero di giri dato in input.
c  quando la turbina e' accoppiata a due compressori
c
c  Tutti gli altri parametri sono forniti dal common "simpstat"
c----------------------------------------------------------------------
        implicit none
c 
	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
	include '../inc/cc.inc'
c
	LOGICAL*2 lhr
        real*4  rev,un_flow,un_eff,qf,un_power,hrate,dpw,haux
        real*4  app_pow,raux
        INTEGER*4 ier,ITP
c-----------------------------------------------------------------
        un_power = 0.
        itp = 1
        call hdan_ch(clim(3,itp),haux,rev,chn(1,itp))
        if(headm.ge.haux) then
          call qdahn(headm,un_flow,rev,chn(1,itp))
        else
          call qdahn(headm,un_flow,rev,chc(1,itp))
        endif
c---->           efficiency
        call effdaqn(un_eff,un_flow,rev,cen(1,itp),cec(1,itp),
     *               clim(1,itp))
	un_eff = un_eff*un_eff_corr(1)
cgpe
      if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
	qf = un_flow/aux3
c---->          power required
cgpe        un_power= AGRVcc * rocn * headm*qf/(un_eff*un_pow_corr(1))
        un_power= AGRVcc * rocn * headm*qf/un_eff
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
        un_power=un_power/(3.6*1.e3)
c
        itp = 2
        raux = rev * rapp_giri
        call hdan_ch(clim(3,itp),haux,raux,chn(1,itp))
        if(headm.ge.haux) then
          call qdahn(headm,un_flow,raux,chn(1,itp))
        else
          call qdahn(headm,un_flow,raux,chc(1,itp))
        endif
c---->           efficiency
        call effdaqn(un_eff,un_flow,raux,cen(1,itp),cec(1,itp),
     *               clim(1,itp))
	un_eff = un_eff*un_eff_corr(2)
cgpe
      if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
	qf = un_flow/aux3
c---->          power required
cgpe        app_pow = AGRVcc * rocn * headm*qf/(un_eff*un_pow_corr(2))
        app_pow = AGRVcc * rocn * headm*qf/un_eff
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
        app_pow = app_pow/(3.6*1.e3)
        un_power = un_power + app_pow
c
        itp = 1
	lhr = .false.
cgpe	call turb(rev,un_power,hrate,tair,
	call turb(itp,rev,un_pow_corr(itp),un_power,hrate_corr(itp),
     *     hrate,tair,cpwm(1,itP),cpwt(1,itp),
     *	 chr(1,itp),chrt(1,itp),dpw,delta,lhr,ier)
	zero_pow_cp = dpw
	return
	end

	Subroutine out_temper(tout,tin,zin,pin,pout,aux2,z_coef,
     *			eff_coef,unit_num,status,
     *			unit_perc,unit_eff,unit_temp)
c
c	Calcola la temperatura di uscita di ogni compressore e 
c       della stazione
c
C
        implicit none

        include '../inc/RK_PARAM.INC'
CMAR
CMAR      include '../inc/param.inc'
CMAR	include '../inc/RK.inc'
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
c
CMAR
      COMMON/MAR/istaz,ppmol
	integer*4 istaz
	real*4 ppmol
	REAL*4	tout,tin,pin,zin,pout,aux2,eff_coef,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*)
	INTEGER*4   unit_num
	LOGICAL*2   status(*)
c
        INTEGER*4   nu,i,nt
        real*4    temp,temp1,delta
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp
	  real*4 ro_attuale
c---------------------------------------------------------------------
	nu = 0
	do i =1,unit_num
	  if(status(i)) then
c---->                         outlet theoretical temperature extimation
	    temp = tin * aux2
	    do nt=1,10
c---->      gz             iterazions
CMAR
      call zpt1 (istaz,pout,temp,zout)     
	  
CMAR	      zout = prop(pout,temp,z_coef)
	      temp1 = tin * zin/zout*aux2
	      dt = temp-temp1
	      dtp = dt/temp
	      if (abs(dtp).lt.dtmin) go to 30
CMAR              T_ADIM=TEMP/(TRIF+T0)
CMAR              P_ADIM=POUT/(PRIF+P0)

CMAR	      dz = z_coef(4)*P_ADIM+z_coef(3)+
CMAR     *				2.*z_coef(6)*T_ADIM
CMAR   calcolo di dz/dt a p costante

      ro_attuale = (pout*ppmol*1E05)/(erre*zout*temp)
CMAR   dz
      call dzdt(istaz, ro_attuale,temp,dz)
	      
	      df = 1.+tin*zin*aux2*dz/(zout*zout)
	      temp = temp-dt/df
	    end do
c---->              correction for the compression ratio
  30	    delta = eff_coef*(pout-pin)
	    unit_temp(i) = (Tin+delta)+ 
     *			(Temp1-Tin-delta)/unit_eff(i)
c---->               station outlet temperature
            tout = tout + unit_perc(i)*unit_temp(i)
	  end if
	end do
	return
	end

c_NU	Subroutine Recover_station(ntp,type_quant,type_avail,
c_NU     *			type_act,unit_prior,STATO,unita,
c_NU     *                  unit_avail,ier)
c_NUc
c_NUc***********************************************************************
c_NUc	In caso di portata totale di macchina insufficiente,
c_NUc	aumenta, se possibile, il numero dei compressori attivi
c_NUc------------------------------------------------------------------
c_NU      implicit none
c_NU      include '../inc/param.inc'
c_NU      include '../inc/stazione.inc'

c_NU	INTEGER*4   type_quant(*),type_avail(*),type_act(*),
c_NU     * 		            type,priority,UNITA,unit_avail(*)
c_NU	real*4    unit_prior(*)
c_NU        CHARACTER*(*)  STATO(*)
c_NUc 25/9/98
c_NU        integer*4 NU,NTP,JT,IC
c_NU        integer*4 IER
c_NUc
c_NU        unita   = 0
c_NU        type    = 0
c_NU	nu      = 1
c_NU        priority= -1.
c_NU	do jt=1,ntp
c_NUc---->     verifica se ci sono dei compressori disponibili
c_NU	  if (type_act(jt).lt.type_avail(jt)) then
c_NUc---->     ricerca fra i compressori disponibili quello a massima
c_NUc	   priorita': 
c_NUc             vale l'ipotesi che per ogni tipo esso sia
c_NUc             il primo dei non utilizzati
c_NUc---------------------------------------------------------------
c_NU            ic=nu+type_act(jt) 
c_NU            if (unit_avail(ic).gt.0 .and.
c_NU     *          stato(ic).eq.funz_off) then
c_NU              if (unit_prior(ic).gt.priority) then
c_NU                type    = jt
c_NU                UNITA   = IC
c_NU                priority= unit_prior(ic)
c_NU              end if
c_NU            end if
c_NU	  end if
c_NU          nu=nu+type_quant(jt)
c_NU	end do
c_NU	if (type.gt.0) then
c_NUc---->             e' stato individuato un compressore da inserire
c_NUc                 e si incrementa il numero dei compressori attivi
c_NU	  type_act(type)=type_act(type)+1
c_NU          STATO(UNITA) = funz_on
c_NU	  ier = 0
c_NU	else
c_NUc---->               non e' possibile aggiungere alcun compressore,
c_NUc                                         la simulazione ha termine
c_NU	  ier=10
c_NU	end if
c_NU	return
c_NU	end

      subroutine surge_eff(n_tipi,tipi_attivi,tipi_rapp,h,
     *                     max_rev,min_rev,q_limite,IVINC,IRMAX,
     *                     ITIP_MAX)
C-----------------------------------------------------------------------
c calcola il limite effettivo di surge in portata per il tipo itipo 
c nel rispetto del rapporto relativo dei giri fra tutti i compressori 
c attivi
C-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
c
      integer*4 n_tipi             ! I) numero di tipi
      integer*4 tipi_attivi(*)     ! I) unita' attive per tipo
      real*4    tipi_rapp(*)       ! I) rapporto dei giri
      real*4    h                  ! I) altezza adiabatica
      real*4    max_rev(*)         ! I) max revo num
      real*4    min_rev(*)         ! I) min revo num
      real*4    q_limite(*)        ! O) portate limite calcolate
      integer*4 IVINC,IRMAX,ITIP_MAX
c 
      integer*4 i,irange
      logical*2 l_risp
      real*4    raux,rmaxi,haux,qaux,qaux1
      real*4    rdummy
c-----------------------------------------------------------------------
      rdummy = min_rev(1)  ! serve per evitare %FORT-I-VARUNUSED
c
c ---> calcolo del rev_eff piu' limitante (e' il maggiore fra tutti i 
c         tipi con unita' attive)
      l_risp=.true.
      rmaxi=0.
      do i=1,n_tipi
        if (tipi_attivi(i).gt.0) then
          call ndah_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *                 lim_h(1,i),raux,h)
          raux=raux/tipi_rapp(i)
          if (raux.gt.rmaxi) rmaxi=raux
        endif
      enddo
c ora rmaxi e' la portata (filtrata dal ratio) limitante 
      do i=1,n_tipi
        if (tipi_attivi(i).gt.0) then
          raux = rmaxi * tipi_rapp(i)
          call hdan_ch(clim(3,i),haux,raux,chn(1,i))
          if(h.ge.haux) then
            call qdahn(h,q_limite(i),raux,chn(1,i))
          else
            call qdahn(h,q_limite(i),raux,chc(1,i))
          endif
c
          call hdan_ch(clim(3,i),haux,max_rev(i),chn(1,i))
          if(h.ge.haux) then
            call qdahn(h,qaux,max_rev(i),chn(1,i))
          else
            call qdahn(h,qaux,max_rev(i),chc(1,i))
          endif
          call qdah_ch(clim(4,i),h,qaux1)
          if (qaux1.lt.qaux) qaux=qaux1
c
          if (q_limite(i).gt.qaux) then
c il rapporto dei giri non potra' essere rispettato, percio' i qlim dovranno
c essere quelli reali
            l_risp=.false.
          endif
        endif
      enddo
c
      if (.not.l_risp) then
        do i=1,n_tipi
          if (tipi_attivi(i).gt.0) then
            call QDAH_SU(a_coef(1,i),b_coef(1,i),nlim(i),lim_h(1,i),
     *                   h,q_limite(i),irange)
          endif
        enddo
        IVINC=1
        IRMAX=ITIP_MAX
      endif
c
      return
      end

      subroutine choke_eff(n_tipi,tipi_attivi,tipi_rapp,h,clim_eff)
C-----------------------------------------------------------------------
c calcola il coefficiente limite effettivo di choke per il tipo itipo 
c nel rispetto del rapporto relativo dei giri fra tutti i compressori 
c attivi
C-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
c
      integer*4 n_tipi             ! I) numero di tipi
      integer*4 tipi_attivi(*)     ! I) unita' attive per tipo
      real*4    tipi_rapp(*)      ! I) rapporto dei giri
      real*4    h                  ! I) altezza adiabatica
      real*4    clim_eff(*)        ! O) clim effettivi calcolati
c 
      integer*4 i
      real*4    raux,rmini,qaux,had,qad,haux
c-----------------------------------------------------------------------
c ---> calcolo del rev_eff piu' limitante (e' il minore fra tutti i 
c         tipi con unita' attive)
      rmini=10000.
      do i=1,n_tipi
        if (tipi_attivi(i).gt.0) then
c uso clim(4) e chc perche' e' comunque il limite estremo:
c    se e' definita la zona di choke, e' il limite estremo
c    se non e' definita, clim(4)=clim(3) e chc=chn
          call ndah_ch(clim(4,i),h,qaux,raux,chc(1,i))
          raux=raux/tipi_rapp(i)
          if (raux.lt.rmini) rmini=raux
        endif
      enddo
c ora rmini sono i giri limitanti (filtrati dal ratio) 
      do i=1,n_tipi
        if (tipi_attivi(i).gt.0) then
          raux = rmini * tipi_rapp(i)
          call hdan_ch(clim(3,i),haux,raux,chn(1,i))
          if(h.ge.haux) then
            call qdahn(h,qaux,raux,chn(1,i))
          else
            call qdahn(h,qaux,raux,chc(1,i))
          endif
          had = h / headrif
          qad = qaux / qmrif
          clim_eff(i) = had / (qad*qad)
        endif
      enddo
c
      return
      end

      Subroutine init_compstat(istaz,vert_pointer,flow_stat,
csave     *                         stat_varsp,delprint)
     *                         stat_varsp)
c
c***********************************************************************
c       inizializzazione del common compstat
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/COMPSTAT.INC'
c
      integer*4 istaz,vert_pointer
      real*4    flow_stat,stat_varsp(*)
      integer*4 i
      integer*4 idummy
      integer*4 ind_vert_arch
c-----------------------------------------------------------------------
      idummy = istaz ! serve per evitare %FORT-I-VARUNUSED
c---->        inizializzazione
      flow          = flow_stat
      ind_vert_arch = vert_pointer
      do i=1,10
        stat_vars(i)=stat_varsp(i)
      enddo
csave      delprint = 0.

      return
      end

	Subroutine fact_lim_reale(max_rev,min_rev,hmax,hmin)
c
c	Calcola i valori estremi dello head di stazione, senza curarsi del
c       rapporto dei giri assegnato
c
c	max_rev     I	maximum revolution rate for each unit type
c	min_rev     I	minimum revolution rate for each unit type
c----------------------------------------------------------------------
        implicit none
c
	include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/SIMPSTAT.INC'
c
      REAL*4	max_rev(*),min_rev(*),hmax,hmin
c
      real*4    rev,h,qaux
      integer*4 irange,itip
c---------------------------------------------------------------------
      hmax=10000.
      hmin=0.
      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c---->      massima altezza adiabatica ammissibile per il compressore
          rev = max_rev(itip)
          call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *                  nlim(itip),lim_n(1,itip),rev,h,qaux,irange)
          if(h.lt.hmax) then
c---->      ricerca del minimo fra i massimi
            hmax = h
          end if
          rev = min_rev(itip)
          call hdan_ch(clim(4,itip),h,rev,chc(1,itip))
          if(h.gt.hmin) then
c---->      ricerca del massimo fra i minimi 
            hmin = h
          end if
        endif
      end do
c
      return
      end

        Subroutine init_station (istaz,vert_pointer,tipo_criterio,
     *     vert_type,pres1,pres2,pres1_ric,pres2_ric,air,
cgpe     *     tot_cons,delpr1,delpr2,flowm,flow_ric,unit_num,type_num,
     *     tot_cons,flowm,flow_ric,unit_num,type_num,
cgpe     *     type_quant_old,jufir,jtfir,nit,mirev,marev,mieff,lstat)
     *     type_quant_old,jufir,jtfir,mirev,marev,mieff,lstat,
     *     stat_varsp)
c
c***********************************************************************
c       inizializzazione del common simpstat per una centrale semplice
c       o composta con i compressori in parallelo
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
      integer*4 istaz,vert_pointer,vert_type,unit_num
cgpe      integer*4 type_num,type_quant_old(*),jufir,nit,jtfir
      integer*4 type_num,type_quant_old(*),jufir,jtfir
c
      real*4    pres1,pres2,air,tot_cons
cgpe     *         ,delpr1,delpr2,
     *         ,flowm,flow_ric
      real*4    stat_varsp(*)
      real*4    mirev(*),marev(*),mieff(*)
      LOGICAL*2 lstat(*)
      real*4    pres1_RIC,pres2_RIC

      integer*4 i,j,k,nu
      integer*4 tipo_criterio
      real*4    aa,bb,denom,faux
      integer*4 i1,ii1
c-----------------------------------------------------------------------
c---->        inizializzazione
c unit_num e type_num rappresentano le unita' e i tipi effettivi
      nu = jufir
      do i=1,type_num
        do k=1,type_quant_old(i)
         j = nu+k-1
          unit_flow(j) = 0.
          unit_head(j) = 0.
          unit_perc(j) = 0.
          unit_rev(j) = 0.
          unit_eff(j) = 0.
          unit_power(j) = 0.
          unit_hrate(j) = 0.
          unit_cons(j) = 0.
cgpe          unit_temp(j) = 0.
          unit_temp(j) = t0
          unit_max(j) = 0.
          unit_vinc(j) = 0
          unit_vcrit(j) = 0.
          unit_flow(j+unit_num) = 0.
          unit_head(j+unit_num) = 0.
          unit_perc(j+unit_num) = 0.
          unit_rev(j+unit_num) = 0.
          unit_eff(j+unit_num) = 0.
          unit_power(j+unit_num) = 0.
cgpe          unit_temp(j+unit_num) = 0.
          unit_temp(j+unit_num) = t0
          unit_max(j+unit_num) = 0.
          unit_min(j+unit_num) = 0.
c-din_corr
          unit_cons(j+unit_num) = 0.
c-din_corr-end
        end do
        nu = nu + type_quant_old(i)
      end do

c--------------->filtri e air cooler
      do i =1,unit_num
	      j=jufir+i-1
	      f_tg_stat(i)=0.
	      f_tg_k(i)=0.
            ac1_stat(i)=0.
	      ac1_k(i)=0.
	      ac1_q_prog(i)=0.
	      ac1_taria_prog(i)=0.
		  if (unit_bifase(j)) then
                ac2_stat(i)=0.
	          ac2_k(i)=0.
	          ac2_q_prog(i)=0.
	          ac2_taria_prog(i)=0.
	      end if
	end do
c
      tot_cons = 0.
cgpe      delpr1   = 0.
cgpe      delpr2   = 0.
      flowm    = 0.
      flow_ric = 0.
      PRES1_RIC=PRES1
      PRES2_RIC=PRES2
c---->               preparazione common stazione.inc
      ind    = istaz
      ver_poi = vert_pointer
      delta = stat_varsp(1)
c---->               generazione common SIMPSTAT
      num_iter = nit
      tair = air
      tipo_crit = tipo_criterio
      if (vert_type .eq. tipo_semp) then
        ntp = type_num
      else
        ntp = 2*type_num
      endif

      do i = 1, ntp
        j = jtfir+i-1
        type_quant(j) = 0
      end do
c
      if (vert_type .eq. tipo_comp) then
        nu = jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            unit_eff_corr(nu+k-1+unit_num) = unit2_eff_corr(nu+k-1)
            unit_power_corr(nu+k-1+unit_num) =
     *                                        unit2_power_corr(nu+k-1)
          end do
          nu=nu+type_quant_old(i)
        end do
      endif
c
      nu = jufir
      do i=1,type_num
        j=jtfir+i-1
        type_quant(j) = type_quant_old(i)
        type_quant(j+type_num) = type_quant_old(i)
!!-definizione di type_actr per la simulazione
!        if (.not.iopt) then
          type_actr(i) = type_act(j)
!        end if

          if (unit_bifase(nu)) type_actr(i+type_num) = type_act(j)
        nu=nu+type_quant_old(i)
      end do
c
      nu = 0
      do i=1,type_num
        j = nu+jufir
!-trattamento dei coefficienti della prima fase (==> primo tipo)
        eff_corr(i) = unit_eff_corr(j)
        power_corr(i) = unit_power_corr(j)
        hrate_corr(i) = unit_hrate_corr(j)
        type_vinc_maxpow(i)=unit_vinc_maxpow(j)
        type_vinc_minpow(i)=unit_vinc_minpow(j)
        type_flag_maxpow(i)=.false.
c-mf03	  if (unit_vinc_maxrev(j).lt.max_rev(j).and.
c-mf03     *      unit_vinc_maxrev(j).gt.min_rev(j)     ) then
c-mf03	    max_rev(j) = unit_vinc_maxrev(j)
c-mf03          type_flag_maxrev(i) = .true.
c-mf03	  end if
        type_vinc_maxrev(i) = unit_vinc_maxrev(j)
c-mf03-end
        type_ratio(i) = unit_rev_rat(j)
        type_bifase(i) = unit_bifase(j)
        mirev(i)  = min_rev(j) * revrif
        marev(i)  = max_rev(j) * revrif
        mieff(i)  = min_eff(j)
        type_min_rev(i)  = min_rev(j) * revrif
        type_max_rev(i)  = max_rev(j) * revrif
c
        do k=1,max_lim
          a_coef(k,i) = type_a_coef(k,j)
          b_coef(k,i) = type_b_coef(k,j)
          lim_h(k,i) = type_lim_h(k,j)
          lim_n(k,i) = type_lim_n(k,j)
          lim_q(k,i) = type_lim_q(k,j)
        enddo
c
        clim(1,i) = type_c_coef(1,j)
        clim(2,i) = type_c_coef(2,j)
        clim(3,i) = type_c_coef(3,j)
        clim(4,i) = type_c_coef(4,j)
c
        do k=1,5
          chn(k,i) = type_chn(k,j)
          cen(k,i) = type_cen(k,j)
        end do
        chn(6,i) = type_chn(6,j)
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
c
        nlim(i)= type_nlim(j)
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
c
!-trattamento dei coefficienti della seconda fase (==> secondo tipo)
        if (unit_bifase(j)) then
          eff_corr(i+type_num) = unit2_eff_corr(j)
          power_corr(i+type_num) = unit2_power_corr(j)
          type_ratio(i+type_num) = unit2_rev_rat(j)
          type_delpint(i) = unit_delprint(j)
          type_deltint(i) = unit_deltrint(j)
          type_maxtint(i) = unit_maxtint(j)
          mirev(i+type_num)      = min2_rev(j) * revrif
          marev(i+type_num)      = max2_rev(j) * revrif
          mieff(i+type_num)      = min2_eff(j)
          type_min_rev(i+type_num)  = min2_rev(j) * revrif
          type_max_rev(i+type_num)  = max2_rev(j) * revrif
c
          do k=1,max_lim
            a_coef(k,i+type_num) = type2_a_coef(k,j)
            b_coef(k,i+type_num) = type2_b_coef(k,j)
            lim_h(k,i+type_num)  = type2_lim_h(k,j)
            lim_n(k,i+type_num)  = type2_lim_n(k,j)
            lim_q(k,i+type_num)  = type2_lim_q(k,j)
          enddo
c
          clim(1,i+type_num) = type2_c_coef(1,j)
          clim(2,i+type_num) = type2_c_coef(2,j)
          clim(3,i+type_num) = type2_c_coef(3,j)
          clim(4,i+type_num) = type2_c_coef(4,j)
c
          do k=1,5
            chn(k,i+type_num) = type2_chn(k,j)
            cen(k,i+type_num) = type2_cen(k,j)
          end do
          chn(6,i+type_num) = type2_chn(6,j)
c attenzione !!! i chc sono uguali ai chn se non e' definita la zona di choking
          if (clim(3,i+type_num).ne.clim2(4,i+type_num)) then
            do k=1,6
              chc(k,i+type_num) = type2_chc(k,j)
            end do
          else
            do k=1,6
              chc(k,i+type_num) = chn2(k,i)
            end do
          endif
c
          if (clim(1,i+type_num).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
            aa=1/clim(1,i+type_num)
            bb=1/clim(2,i+type_num)
            denom=aa-bb
            faux=cen(1,i+type_num)+cen(2,i+type_num)/aa+
     *           cen(3,i+type_num)/aa/aa
            cec(1,i+type_num)=(aa*faux-mieff(i+type_num)*bb)/denom
            cec(2,i+type_num)=(aa*cen(4,i+type_num))/denom
            cec(3,i+type_num)=(aa*cen(5,i+type_num))/denom
            cec(4,i+type_num)=(aa*bb*(mieff(i+type_num)-faux))/denom
            cec(5,i+type_num)=-(aa*bb*cen(4,i+type_num))/denom
            cec(6,i+type_num)=-(aa*bb*cen(5,i+type_num))/denom
          endif
c
          nlim(i+type_num)= type2_nlim(j)
c
          do k=1,3
            cpwm(k,i+type_num) = type_cpwm(k,j)
            cpwt(k,i+type_num) = type_cpwt(k,j)
            chr(k,i+type_num)  = type_chr(k,j)
            chrt(k,i+type_num) = type_chrt(k,j)
          end do
          cpwt(4,i+type_num) = type_cpwt(4,j)
          chrt(4,i+type_num) = type_chrt(4,j)
          chr(4,i+type_num) = type_chr(4,j)
          chr(5,i+type_num) = type_chr(5,j)
          chr(6,i+type_num) = type_chr(6,j)
c
        endif ! end if (unit_bifase)

        nu = nu+type_quant_old(i)
      end do
c
c---->            set unit status as logical variable
      do i = 1,maxunits
        lstat(i) = .false.
      enddo
c
      if (iopt) then
        i1=0
        nu = jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            if(index(stat(k+i1),funz_on).gt.0) then
              lstat(k+i1) = .true.
              if (unit_bifase(nu+k-1)) then
                lstat(k+i1+unit_num) = .true.
              endif
            end if
          end do
          i1=i1+type_quant_old(i)
          nu=nu+type_quant_old(i)
        end do
      else
        i1=0
        nu=jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            ii1=nu+k-1
            if(index(status(ii1),funz_on).gt.0) then
              lstat(k+i1) = .true.
              if (unit_bifase(ii1)) then
                lstat(k+i1+unit_num) = .true.
              end if
            end if
          end do
          nu=nu+type_quant_old(i)
          i1=i1+type_quant_old(i)
        end do
      end if
c
c_ecol 
c  Inizializzazione aree common 'filtri','air_cooler' e 'air_cooler2'
c  definite nei files d'include 'filtri.inc' e 'air_cooler.inc'
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
c --->trattamento ac di centrale
	ac_ce_stat=c_ac_ce_stat(vert_pointer)
	ac_ce_k=c_ac_ce_k(vert_pointer)
	ac_q_prog=c_ac_q_prog(vert_pointer)
	ac_taria_prog=c_ac_taria_prog(vert_pointer)
	

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
c_ecol
      if (vert_type .eq. tipo_semp) then
        una_turb = .false.
      else
!-i bifase hanno una sola turbina
        una_turb = .true.
      endif
c
      return
      end
