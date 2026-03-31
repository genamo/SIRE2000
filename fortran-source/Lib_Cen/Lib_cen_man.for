c.......................................................................
c  pacchetto contenente le routines per gestire la ripartizione manuale
c  delle portate
c.......................................................................
	Subroutine Single_Station_man(istaz,vert_pointer,tipo_criterio,
     *                  flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *                  tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *                  type_num,unit_num,jufir,jtfir,stat_vars,
     *                  stat_ord,ier)
c
c***********************************************************************
c simulazione di una stazione elementare di tipo misto con bifase in
c parallelo
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


c
c  ind  : indice della stazione nella struttura dati (common stazione.inc)
 
      INTEGER*4  istaz
      INTEGER*4  tipo_criterio
      INTEGER*4  vert_pointer
      INTEGER*4  stat_ord
      real*4     flow2,                ! I flusso di uscita
     *           pres1,pres2,          ! pressioni di ingresso e uscita
     *           temp1,temp2,          ! temperature di ingresso e uscita 
     *           air,comp_ratio,tot_cons,
     *           head,
     *           delpr1,delpr2,
     *           flowm,flow_ric
     *          ,delpr_f,delpr_ac
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4     stat_vars(*)
      INTEGER*4  ier
c
      real*4     mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4     mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2  lstat(maxunits)
c
      integer*4 I, J
      real*4    HTCITJ_OLD, PORTTJ_OLD, PMSETJ_OLD, PVSETJ_OLD,
     *          VALSJC_OLD, QSETTJ_OLD
      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD
      character*3 status_old(maxunits)

      real*4    pres_int,temp_int,delprint,delt_int
      integer*4 type_quant_old(maxunits)
      real*4    unit_eff_corr_old(maxunits),
     *          unit_power_corr_old(maxunits)

	
	real*4 a

       real*4      dp1_M,dp2_M
      


cmar_ac_gestione mess
c	integer*4 i
cmar_ac_gestione mess





c-----------------------------------------------------------------------
c---->              salvataggio condizioni iniziali
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


c---->                                       simulazione stazione
      call Station_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
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

      ac_ce_stat=	ac_ce_stat_M

	ac1_stat = ac1_stat_M

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
      call Station_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
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

c---->                                       simulazione stazione
      call Station_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir),unit_eff_corr(jufir),unit_eff(jufir),
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
cgpe      if (iopt .or. iopt_mess) goto 11

C--->    gestione errore
c nel caso di criterio manuale, non viene corretto il punto di lavoro ko!!!
cgpe      if (ier .gt. 0) then
cgpe        if (ier .le. 5 .or. ier.eq.38) then
cgpe!- ier=1: non esistono punti fattibili (al min_rev viene superata la max power
cgpe!- ier=4: portata negativa
cgpe!- ier=38: pressione di ingresso o di uscita negativa
cgpe!- ier=5: pressione di ingresso supera la pressione di uscita
cgpe!- Non e' possibile portare il punto dentro le mappe, ma e' stato imposto
cgpe!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cgpe          goto 11

cgpe        else if (ier .eq. 6 .or. ier .eq. 7) then
cgpe!- ier=6: altezza adiabatica superiore alla massima fattibile
cgpe!- ier=7: altezza adiabatica inferiore alla minima fattibile
cgpe          if (nit . lt. nitmax) then
cgpe!- viene portato il punto dentro le mappe:
cgpe!- le nuove condizioni di input alla station sono rappresentate da
cgpe!- PRES1/ PRES2/ FLOW2
cgpe            nit = nit + 1
cgpe            goto 10
cgpe          else
cgpe!- se la correzione del punto di lavoro termina con errore, nella
cgpe!- optim_vertex viene dato il messaggio che e' stato raggiunto il
cgpe!- massimo numero di iterazioni (ier>0 e nit=nitmax)
cgpe!            return
cgpe            goto 11
cgpe          end if

cgpe        else if (ier.eq.8 .or. ier.eq.20) then
cgpe!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
cgpe!- (sono stati calcolati i dati di unita')
cgpe          if (nit .lt. nitmax) then
cgpe!- se l'errore non deriva dall'aver portato il punto di lavoro dentro le
cgpe!- mappe, si esce (non sono state modificate le condizioni iniziali)
cgpe!            return
cgpe            goto 11

cgpe          else
cgpe!- nel caso in cui l'errore derivi dalla correzione del punto, sono state
cgpe!- modificate le condizioni iniziali
cgpe            goto 11
cgpe          end if

cgpe        end if
cgpe      end if
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

      Subroutine Station_man(vert_pointer,flow,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,nom_flow,
     *     fl_flow_man,
     *     unit_perc,unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,unit_hrate_corr,
     *     unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c	simulazione di stazione mista con il criterio di controllo manuale
c     delle portate.
c     Nota: 1) iopt = .F. e iopt_mess = .F.
c           2) non dovrebbe venire chiamato in rete
c--------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/prototipo.INC'
      include '../inc/simpstat_aux.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end



cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat

CMAR_EVO_CRIT_MAN
	include '../inc/ASS_COND.INC'
CMAR_EVO_CRIT_MAN

      COMMON/MAR/istaz
      integer*4 istaz
c



cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN



      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
      real*4    nom_flow(*)
c
      real*4 flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),unit_rev(*),
     *	   unit_max(*),unit_min(*),unit_flow(*),unit_eff_corr(*),
     *	   unit_eff(*),unit_head(*),unit_power_corr(*),unit_power(*),
     *	   unit_hrate(*),unit_cons(*),unit_temp(*),stat_vars(*),
     *       delpr1,delpr2,flowm,head,unit_vcrit(*),unit_hrate_corr(*)
      real*4 fl_flow_man(*)
      integer*4 unit_vinc(*)
      real*4 unit2_perc(*),unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*),unit2_max(*),unit2_min(*)

      LOGICAL*2 unit_bifase(maxunits)
      LOGICAL*2 lsim(maxunits),status(*)
c
      real*4   qq,pin,pout,exp_pol,esp,aux1
c
      INTEGER*4  i,ier,itip,ker
c
      real*4   ra,rb,rd
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
      real*4   FLOW_APP
c --- new gz
      real*4    qmin_tot
      real*4    qmax_tot
      real*4    qmin                     ! portata piu' vincolante
      real*4    qmax                     ! portata piu' vincolante
      real*4    head_assoluto(maxunits)
      real*4    qmax_assoluto(maxunits)  ! portata massima di ciascuna unita''
      real*4    qmax_assoluto2(maxunits) ! portata massima seconda fase
      real*4    qmin_assoluto(maxunits)  ! portata minima di ciascuna unita''
      real*4    qmin_assoluto2(maxunits) ! portata minima seconda fase
      integer*4 vinc_min(maxunits)
      integer*4 vinc_max(maxunits)
      real*4    hmin,hmax
      real*4    haux
      real*4    qcrit(maxunits) ! portata prima fase per il rispetto criterio
      real*4    qass(maxunits)
      real*4    qass2(maxunits)
      real*4    fa,fb
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 icj,ij,nutot,numact
      real*4    tot_flow
      real*4    rev,rev2
      real*4    zero_perc2
      EXTERNAL  zero_perc2
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      logical*2 l_ric
      real*4 flow_man(maxunits),tot_flow_man
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
c-UDM-end

cmar_08_05
      real*4 n_sim, n_calc
cmar_08_05


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



cmar_pow_stat_312

      do itip= 1, ntp
      min_p=min_p+type_vinc_minpow(itip)
      max_p=max_p+type_vinc_maxpow(itip)
	enddo
cmar_pow_stat_312




cmar_pow_stat




cmar_DIN
      istaz=am
cmar_DIN



      ier = 0
      ker = 0
cgpe      if (flow.le.0.) then
cgpe      end if
c---->
c              CADUTA DI PRESSIONE DOVUTA AL PIPING
      pin  = pres1 - delpr1
      pout = pres2 + delpr2
c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,flow,delpr_f)
c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p
      ro_in=1.
	delpr_f=delpr_f/ro_in
      pin  = pin - delpr_f

c              CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_p (ntp,type_quant,status,unit_bifase,nom_flow,
     *                 ac_ce_k,ac1_k,ac2_k,flow,delpr_ac)
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
cgpe      endif
cgpe      if(pin.gt.pout) then
cgpe      end if
CMAR
      comp_ratio = pout/pin
      call politrop_esp(istaz,pin,temp1,comp_ratio,exp_pol,ier)

CMAR      exp_pol = prop(pin,temp1,exp_coef(1,ind))
      esp = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c---->             adiabatic head
CMAR      z = prop(pin,temp1,z_coef(1,vert_pointer))
      call zpt1(istaz,pin,temp1,z)
      aux1 = (erre / agrvcc / pmol) * Z * temp1/esp
      aux2 = comp_ratio**esp
      head = aux1 * (aux2-1.)                        ! in km
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp1/pin
      aux3 = ROCN*uno_su_ro_attuale
      flowm = flow*aux3
c calcolo della portata dalle percentuali



      nutot = 0
      do itip=1,ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
           flow_man(icj) = flowm*fl_flow_man(icj)
CMAR_EVO_CRIT_MAN       flow_man(icj) = flow*fl_flow_man(icj)
CMAR_EVO_CRIT_MAN 
	
        enddo
        nutot = nutot+type_quant(itip)
      enddo

c carico headm nel common simpstat.INC per alcune routine zero_***
      headm = head
      do i=1,ntp
        head_assoluto(i) = head      
      enddo
c--->               assign the unit to simulate
      do i=1,unit_num
        lsim(i) = status(i)


	if(lsim(i)) then
cmar_08_05
	n_sim=n_sim+1
cmar_08_05
      endif


      end do






      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo

c---> calcolo gli estremi, in prevalenza (hmax,hmin), della stazione
      call limiti_h(hmin,hmax)
      if (head.gt.hmax) then
c errore: prevalenza richiesta superiore alla massima fattibile
        ier = 6 ! ier = 5
	  if (fl_sim) then
c-gpe-new
          if (iopt_mess) return
c-gpe-new-end
c-UDM          write(mess,557) head,hmax,UDM_INT_H ! write(mess,557) head,hmax
c-UDM557     format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
557     format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
          write(mess,557) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmax,1)
          call app_mess(vert_pointer,ier,mess)
        endif
        RETURN

      else if (head.lt.hmin) then
c errore: prevalenza richiesta inferiore alla mimima fattibile
        ier = 7 ! ier = 6
	  if (fl_sim) then
c-gpe-new
          if (iopt_mess) return
c-gpe-new-end
c-UDM          write(mess,559) head,hmin,UDM_INT_H ! write(mess,559) head,hmin
c-UDM559     format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
559     format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
          write(mess,559) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmin,1)
          call app_mess(vert_pointer,ier,mess)
        endif
        RETURN

      endif
c
c-preparazione common per il calcolo dei limiti estremi
      do i=1,ntp
        cmax_eff(i) = clim(4,i)
      enddo
c---> nella subroutine limiti_q, qmin_assoluto/qmax_assoluto rappresentano
c  la portata minima/massima della prima fase
      call limiti_q_bifase(head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)
      if (ier.ne.0) then
	  if (fl_sim) then
c-gpe-new
          if (iopt_mess) return
c-gpe-new-end
cgpe-mess          call gest_error (1,0,'','Al min_rev si supera la max_power',0)
          mess = ' '
          call app_mess(vert_pointer,ier,mess)
          ier = 1
        endif
        return
      endif
c qmin_assoluto/qmax_assoluto rappresentano la portata minima/massima
c che ciascuna unita' puo' lavorare mentre qmin_tot/qmax_tot rappresentano
c la portata min/max totale fattibile da tutte le unita''
      do itip=1,ntp
        if (type_actr(itip).gt.0) then
          qmin_assoluto(itip)=qmin_assoluto(itip)+qmin_assoluto2(itip)
          qmax_assoluto(itip)=qmax_assoluto(itip)+qmax_assoluto2(itip) 



        end if
      end do


c
c simulazione normale
c flow_man(iunit) rappresenta la portata che ciascuna unita'' attiva deve lavorare.
c Su ciascun tipo di compressore la portata minima e massima sono memorizzate in
c qmin_assoluto(itip) e qmax_assoluto(itip) e il tipo di vincolo e' dato da
c vinc_min dalla subroutine limiti_q
      nutot = 0
      do itip=1,ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
          if (status(icj)) then
            if (flow_man(icj).le.qmin_assoluto(itip)) then
c trattamento da qmin
              if (.not.iopt .and. .not.iopt_mess) then
                if ((vinc_min(itip).eq.sur_1f)    .or.
     *              (vinc_min(itip).eq.sur_2f))    then
cgpe-mess                   write(mess,558) itip, 'Parabola'
                  ker = 21
                else
cgpe-mess                   write(mess,558) itip, 'Giri'
                  ker = 22
                endif
558             format('Tipo vincolato: ',i2,' - Tipo vincolo: ',a)
cgpe-mess                ker = 93
cgpe-corr                mess = ' '
c-UDM565     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A11)
c-UDM                write(mess,565) qmin_assoluto(itip),flow_man(icj),UDM_INT_QM
565     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
                write(mess,565)
     *                udm_out_str(udm_QM,qmin_assoluto(itip),0),
     *                udm_out_str(udm_QM,flow_man(icj),1)
cgpe-corr-end
                ier = 0
                call app_mess(vert_pointer,ker,mess)
cgpe-new                call gest_error (3,0,'STATION_MAN',
cgpe-new     *               'Non si rispetta il criterio',0)
cgpe-new                call gest_error (3,0,'',
cgpe-new     *               'Unita'' vincolata al minimo',0)
cgpe-new                call gest_error (3,0,'',mess,0)
              end if
              tip_vinc(icj)  = vinc_min(itip)
              qass(icj)      = qmin_assoluto(itip)-qmin_assoluto2(itip)
              qass2(icj)     = qmin_assoluto2(itip)

	      elseif (flow_man(icj).ge.qmax_assoluto(itip)) then
c trattamento da qmax
              if (.not.iopt .and. .not.iopt_mess) then
                if ((vinc_max(itip).eq.cho_1f)    .or.
     *              (vinc_max(itip).eq.cho_2f))    then
cgpe-mess                  write(mess,558) itip, 'Parabola'
                  ker = 24
                elseif ((vinc_max(itip).eq.mar_1f) .or.
     *                  (vinc_max(itip).eq.mar_2f)) then
cgpe-mess                  write(mess,558) itip, 'Giri'
                  ker = 23
                else
cgpe-mess                  write(mess,558) itip, 'Potenza'
                  ker = 25
                endif
cgpe-mess                ker = 94
cgpe-corr                mess = ' '
c-UDM                write(mess,565) qmax_assoluto(itip),flow_man(icj),UDM_INT_QM
                write(mess,565)
     *                udm_out_str(udm_QM,qmax_assoluto(itip),0),
     *                udm_out_str(udm_QM,flow_man(icj),1)
cgpe-corr-end
                ier = 0
                call app_mess(vert_pointer,ker,mess)
cgpe-new                call gest_error (3,0,'STATION_MAN',
cgpe-new     *               'Non si rispetta il criterio',0)
cgpe-new                call gest_error (3,0,'',
cgpe-new     *               'Unita'' vincolata al massimo',0)
cgpe-new                call gest_error (3,0,'',mess,0)
              end if
              tip_vinc(icj) = vinc_max(itip)
              qass(icj)     = qmax_assoluto(itip) -qmax_assoluto2(itip)
              qass2(icj)    = qmax_assoluto2(itip)

            else if ((qmin_assoluto(itip).lt.flow_man(icj)).and.
     *               (flow_man(icj).lt.qmax_assoluto(itip))) then
c simulazione normale delle macchine residue
              if (.not.(type_bifase(itip))) then
c assegnazione di qass ai tipi monofase
                qass(icj) = flow_man(icj)
              else
c assegnazione di qass e qass2 ai tipi bifase
                itipo = itip
                flow_unit = flow_man(icj)
                qmin_assoluto(itip) = qmin_assoluto(itip) - 
     *                                qmin_assoluto2(itip)
                qmax_assoluto(itip) = qmax_assoluto(itip) - 
     *                                qmax_assoluto2(itip)
                fa = zero_perc2(qmin_assoluto(itip))
                fb = zero_perc2(qmax_assoluto(itip))
                if ((fa*fb).lt.0 ) then
                  ra = qmin_assoluto(itip)
                  rb = qmax_assoluto(itip)
                  rd = (ra + rb)/2.
cgpe-fun                  call zero_fun1(ra,rb,rd,fa,fb,qcrit(itip),epsma,pmach,
                  call zero_fun(ra,rb,rd,fa,fb,qcrit(itip),epsma,pmach,
     *                   zero_perc2)
	            qass(icj) = qcrit(itip)
c calcolo di qass2 sulla seconda fase corrispondente a qcrit
                  call hdaq_ch(clim(3,itip),haux,qcrit(itip))
  	            if (head.ge.haux) then
                    call ndahq(head,qcrit(itip),rev,chn(1,itip))
                  else
                    call ndahq(head,qcrit(itip),rev,chc(1,itip))
                  end if
                  rev2=rev*type_ratio(itip)/type2_ratio(itip)
  	            call qdahn2(head,qass2(icj),rev2,clim2(3,itip),
     *                        chn2(1,itip), chc2(1,itip))
                else if (abs(fa).le.eps_crit) then
	            qcrit(itip) = qmin_assoluto(itip)
	            qass(icj)  = qcrit(itip)
	            qass2(icj) = qmin_assoluto2(itip)
                else if (abs(fb).le.eps_crit) then
  	            qcrit(itip) = qmax_assoluto(itip) 
                  qass(icj)  = qcrit(itip)
	            qass2(icj) = qmax_assoluto2(itip)
	          end if
	          qmin_assoluto(itip) = qmin_assoluto(itip) + 
     *                                qmin_assoluto2(itip)
                qmax_assoluto(itip) = qmax_assoluto(itip) + 
     *                                qmax_assoluto2(itip)
	        end if

  	      else
ccc   errore?!?!?!?!?

            end if
          end if
        enddo
        nutot = nutot+type_quant(itip)
      enddo
c
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, qass (o qass2), vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old
77    continue
c problema: dati head e qass per ogni tipo, calcolare le altre grandezze
      do itip=1,ntp
        if(type_act_old(itip).gt.0) then
          qmax_assoluto(itip)=qmax_assoluto(itip)-qmax_assoluto2(itip)
          qmin_assoluto(itip)=qmin_assoluto(itip)-qmin_assoluto2(itip)
cmar_pow_stat
cccccccccccccc_312      min_p=min_p+type_vinc_minpow(itip)
cccccccccccccc_312      max_p=max_p+type_vinc_maxpow(itip)
cmar_pow_stat
        end if
      end do

	


c-bpcorr
      l_ric = .true.
c-bpcorr-end
      nutot = 0
      do itip = 1,ntp
        if(type_act_old(itip).gt.0) then
          do ij = 1,type_quant(itip)
            icj = nutot+ij
            if(status(icj)) then

cmar_08_05      
          n_calc=n_calc+1
cmar_08_05   



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


cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)
cmar_pow_stat



c-bpcorr
c-l_ric              if (unit_vinc(icj) .ne. sur_1f .and.
c-l_ric     *            unit_vinc(icj) .ne. sur_2f     ) then
c-l_ric                l_ric = .false.
c-l_ric              end if
c-bpcorr-end
c controllo dell'errore in uscita dalla dati_compressore
              if (ier.gt.0) then
                if (fl_sim) then
cgpe-mess                  call gest_error (1,0,'',' Violata la max_power',0)
                  mess = ' '
cgpe-mess                  ker = 14
                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                else
!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                  ier = 1
                  return
                end if
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
c gestione dell'errore differente dai vari moduli ?????
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
      tot_flow_man = 0.
      do icj=1,unit_num
c calcolo di tot_flow e tot_flow_man (portata totale lavorata e richiesta)
        tot_flow = tot_flow+unit_flow(icj)
        tot_flow_man = tot_flow_man+flow_man(icj)
        tot_cons = tot_cons+unit_cons(icj)
        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do

c definizione della variabile l_ric
      l_ric = .true.
      nutot = 0
      do itip = 1, ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
          if (status(icj).and.(tip_vinc(icj).ne.sur_1f)) then
c test se tutti i vincoli delle unità attive sono sur_1f
            l_ric = .false.
          endif
        enddo
        nutot = nutot+type_quant(itip)
      enddo

cgpe      flow_ric = 0.
      flow_ric = tot_flow/aux3
      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow .lt. flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
c-gpe-new
          if (iopt_mess) return
c-gpe-new-end

          if (fl_sim) then
cgpe-mess            call gest_error (1,0,'','Portata superiore alla massima fattibile',0)
cgpe-mess            write(mess,560) (tot_flow*conv_ns)/aux3, (flowm*conv_ns)/aux3
c_UDM            write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q
cgpe-mess560     format(' Portata lavorata: ',f7.2,' *** Portata richiesta: ',f7.2)
c-UDM560     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
            write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                      udm_out_str(udm_Q,flowm/aux3,1)
            call app_mess(vert_pointer,ier,mess)
          endif
        else if (tot_flow.gt.flowm) then
c tutte le unita' sono in riciclo
!!          if (iopt .or. iopt_mess) return
c-l_ric          if (.not.iopt_mess) then
            if (l_ric) then
              ier = 27
            else
              ier = 20
            endif
            if (l_ric) then
c-bpcorr              ier = 0
cgpe              flow_ric = tot_flow/aux3
c-gpe-new
              if (iopt_mess) return
c-gpe-new-end
              if (fl_sim) then
cgpe-mess                call gest_error (2,0,'','La centrale e'' in riciclo',0)
cgpe-mess                write(mess,560) (tot_flow*conv_ns)/aux3,(tot_flow_man*conv_ns)/aux3
c-UDM                write(mess,560) tot_flow/aux3,tot_flow_man/aux3,UDM_INT_Q
               if (.not.iopt_mess) then
                write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                          udm_out_str(udm_Q,tot_flow_man/aux3,1)
                call app_mess(vert_pointer,ier,mess)
               endif
c-bpcorr
                ier = 0
c-bpcorr-end
              endif
            endif
c-l_ric          endif
        endif
      endif

      do icj = 1,unit_num
        if(status(icj)) then
c unit_perc rappresenta la percentuale rispetto alla portata richiesta al TC
          unit_perc(icj) = unit_flow(icj)/tot_flow
          if (unit_bifase(icj)) then
            unit2_perc(icj) = unit2_flow(icj)/tot_flow
          endif
        endif
      enddo

      temp2 = 0.
      call out_temper_mf(
     *         tair,
     *         temp2,temp1,Z,pin,pout,aux2,
     *         z_coef(1,vert_pointer),
     *         dh_coef(1,ind),cp_par(1,ind),unit_num,status,
     *         unit_perc,unit_eff,
     *         unit_flow,
     *         unit_temp,unit2_perc,unit2_eff,
     *         unit2_flow,
     *         unit2_temp,unit_bifase,stat_vars(7))

c----->intervento su deltat aircooler
      temp2 = temp2 - stat_vars(5)
	 call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7)
     *            ,pres1,temp1,temp2)
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning
cmar_AC_auto
      IF (.not.FLAG_AC)THEN
cmar_AC_auto

      if (temp2 .gt. stat_vars(7)) then
        ker = 79
        if (fl_sim) then
         if (.not.iopt_mess) then
cgpe-mess          call gest_error (2,0,'','Superata la massima temperatura di mandata',0)
c-UDM          write (mess,570) temp2,stat_vars(7),UDM_INT_T ! write (mess,570) temp2,stat_vars(7)
c-UDM570       format('T man: ',f7.2,' - T man max: ',f7.2,'  ',A3)
          write(mess,570) udm_out_str(udm_T,temp2,0),
     *                    udm_out_str(udm_T,stat_vars(7),1)
570   format('T man: ',A10,' - T man max: ',A25)
          call app_mess(vert_pointer,ker,mess)
         endif
        endif
      endif
c
cmar_AC_auto
	ENDIF
cmar_AC_auto

 


      return
      end

      Subroutine comp_Station_man(istaz,vert_pointer,tipo_criterio,
     *                        flow2,pres1,pres2,temp1,temp2,air,
     *                        comp_ratio,tot_cons,delpr1,delpr2,flowm,
     *                        flow_ric,type_num,unit_num,jufir,jtfir,
     *                        stat_varsp,
     *                        stat_ord,ier)
c----------------------------------------------------------------------
c  simulazione di una stazione composta con i compressori bifase in
c  serie
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
c



cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess



c  ind  : indice della stazione nella struttura dati (common stazione.inc)

      INTEGER*4  istaz  ! I indice della stazione nella struttura dati SIRE
      INTEGER*4  vert_pointer
      INTEGER*4  tipo_criterio
      INTEGER*4  stat_ord
      real*4   flow2,                ! I flusso di uscita
     *         pres1,pres2,          ! pressioni di ingresso e uscita
     *         temp1,temp2,          ! temperature di ingresso e uscita
     *         air,comp_ratio,tot_cons,
     *         delpr1,delpr2,flowm,flow_ric,delprint
      INTEGER*4  type_num,unit_num,jufir,jtfir
      real*4       stat_varsp(*)
      INTEGER*4  ier
c
      real*4   mirev(maxunits),marev(maxunits),mieff(maxunits)
      real*4   mirev2(maxunits),marev2(maxunits),mieff2(maxunits)
      LOGICAL*2 lstat(maxunits)
c
      integer*4 I, J
      real*4    HTCITJ_OLD, PORTTJ_OLD, PMSETJ_OLD, PVSETJ_OLD,
     *          VALSJC_OLD, QSETTJ_OLD
      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD
      character*3 status_old(maxunits)

      real*4    pres_int,temp_int,delt_int
      integer*4 type_quant_old(maxunits)
      real*4    unit_eff_corr_old(maxunits),
     *          unit_power_corr_old(maxunits)

      real*4 a

       real*4      dp1_M,dp2_M

	


cmar_ac_gestione mess
c	integer*4 i
cmar_ac_gestione mess


c-----------------------------------------------------------------------
C--->  salvataggio delle condizioni iniziali
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
      call compstat_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),unit_perc(jufir),
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
     *     unit_delpr_ac(jufir),
     *     IER)


	


CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARSP(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  


       ac_ce_stat= ac_ce_stat_M

	ac1_stat=ac1_stat_M


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
      call compstat_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),unit_perc(jufir),
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
     *     unit_delpr_ac(jufir),
     *     IER)



         ENDIF




	


CMAR     IF1
       ELSE
CMAR     IF1


      call compstat_man(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     fl_flow_man(jufir),unit_perc(jufir),
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
     *     unit_delpr_ac(jufir),
     *     IER)






CMAR     IF1
      ENDIF
CMAR     IF1










cgpe-new
C--->    gestione errore
c nel caso di criterio manuale, non viene corretto il punto di lavoro in caso di
c errore       
cgpe!      if (iopt .or. iopt_mess) return
cgpe      if (iopt .or. iopt_mess) goto 11
c
cgpeC---> gestione dell'errore
cgpe      if (ier .gt. 0) then
cgpe!-simulazione KO
cgpe        if (ier .le. 5 .or. ier .eq. 38) then
cgpe!-ier=1: non esistono punti fattibili sulle mappe dei compressori
cgpe!-ier=38: pressione di ingresso o di uscita nulle o negative
cgpe!-ier=4: flow negativo
cgpe!-ier=5: pressione di ingresso superiore alla pressione di uscita
cgpe!- Non e' possibile dare il punto dentro la mappa, ma e' stato imposto
cgpe!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cgpe          goto 11

cgpecgpe        else if (ier.eq.21 .or. ier.eq.23) then
cgpe        else if (ier.eq.71 .or. ier.eq.73) then
cgpe!- ier=71: pressione di uscita superiore alla massima fattibile
cgpe!- ier=73: pressione di uscita inferiore alla minima fattibile
cgpe          if (nit .lt. nitmax) then
cgpe!- viene portato il punto dentro le mappe
cgpe!- le nuove condizioni di input alla station sono rappresentate da
cgpe!- PRES1/ PRES2/ FLOW2
cgpe            nit = nit + 1
cgpe            goto 10
cgpe          else
cgpe!- se la correzione del punto di lavoro termina con errore, nella
cgpe!- optim_vertex viene dato il messaggio che e' stato raggiunto il
cgpe!- massimo numero di iterazioni (ier>0 e nit=nitmax)
cgpe!- Sono da ripristinare i vecchi CIN
cgpe            goto 11
cgpe          end if

cgpe        else if (ier.eq.8 .or. ier.eq.20) then
cgpe!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
cgpe!- (sono stati calcolati i dati di unita')
cgpe          if (nit .lt. nitmax) then
cgpe!- se non e' stato corretto il punto, non sono state modificate le
cgpe!- condizioni iniziali
cgpe!            return
cgpe            goto 11

cgpe          else
cgpe!- nel caso in cui l'errore derivi dalla correzione del punto, sono state
cgpe!- modificate le condizioni iniziali
cgpe            goto 11
cgpe          end if

cgpe        endif
cgpe      end if

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

      Subroutine compStat_man(vert_pointer,flow2,pres1,pres2,temp1,
     *     temp2,
     *     delpr1,delpr2,
     *     unit_pres_int,unit_temp_int,unit_delprint,
     *     unit_deltrint,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     type_quant,unit_num,status,nom_flow,
     *     fl_flow_man,unit_perc,
     *     unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,unit_hrate_corr,
     *     unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_varsp,
     *     unit2_rev,unit2_flow,
     *     unit2_head,unit2_eff_corr,unit2_eff,unit2_power_corr,
     *     unit2_power,unit2_temp,
     *     unit_delpr_ac,
     *     IER) 
c
c      simulazione di stazione composta con il criterio manuale delle
c      portate
c--------------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/th.inc ' ! :in
      include '../inc/tj.inc ' ! :in/out
      include '../inc/simpstat.INC '	
      include '../inc/mf_simpstat.INC '	
      include '../inc/compstat.INC '	
c
      include '../inc/PROTOTIPO.INC '	
      include '../inc/simpstat_aux.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
      include '../inc/err.inc'
c-UDM-end

cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat

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
      COMMON/MAR/istaz
      integer*4 istaz
c
      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4 presMandataCalc
      real*4 nom_flow(*)
      real*4 flow2,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),
     *       unit_rev(*),unit_max(*),unit_min(*),unit_vcrit(*),
     *	   unit_flow(*),unit_eff_corr(*),unit_eff(*),unit_head(*),
     *	   unit_power_corr(*),unit_power(*),unit_hrate(*),
     *	   unit_cons(*),unit_temp(*),stat_varsp(*),
     *       delpr1,delpr2,flowm,unit_hrate_corr(*)
     *       ,unit_delpr_ac(*)
      real*4 fl_flow_man(*)

      integer*4 unit_vinc(*)
      real*4 unit_pres_int(*),unit_temp_int(*),unit_delprint(*)
      real*4 unit_deltrint(*)
      real*4 unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*)

      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   qq,exp_pol,aux1
      real*4   pmin,pmax     ! pressione minima e massima della stazione
c
      INTEGER*4  numact      ! numero di unita' attive fra cui ripartire
c                              la portata totale
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
      real*4 qmin_tot, qmax_tot, qmin, qmax
      real*4 qmax_assoluto(maxunits), qmin_assoluto(maxunits)
     *      ,hmax_assoluto(maxunits), hmin_assoluto(maxunits)
      integer*4 vinc_min(maxunits), vinc_max(maxunits)
      real*4 qass(maxunits), hass(maxunits)
      real*4 qaux, haux, rev_max, rev_min, rev
!      real*4 percent
c
      real*4    ra,rb,rd,rsol
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 icj,ij,nutot
      real*4    tot_flow
      real*4    old_aux3
      real*4    zero_giri
      external  zero_giri
      real*4    eps_q/1.e-3/
      integer*4 nact  ! rappresenta il numero delle unita' attive meno
c      unit_act, che rappresenta il numero delle unita' quelle tali che
c      unit_avail = 2
      real*4    pres_old
      real*4   eps_p/0.05/

      real*4    EESP

      real*4    hmin,hmax
      integer*4 itip_min,itip_max

      real*4    pin_save, pout_save
      integer*4 ier_save
      logical*2 l_ric
      real*4    flow_man(maxunits),tot_flow_man
      real*4 tout_ac,delpr_f,delpr_ac
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
cgpe      if (flow2.le.0.) then
cgpe      end if
c---->                        calcolo variabili in ingresso alla stazione
c tin,pin e pout sono in compstat.INC
      tin = temp1
c              CADUTA DI PRESSIONE DOVUTA AL PIPING
      pin  = pres1 - delpr1
      pout = pres2 + delpr2
c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *                   f_ce_k,f_tg_k,flow,delpr_f)
c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p
      ro_in=1.
	delpr_f=delpr_f/ro_in
      pin  = pin - delpr_f

c              CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_s (ntp,type_quant,status,nom_flow,
     *                 ac_ce_k,ac2_k,flow,delpr_ac)
c-ace18
      call ro_real(ind,pout,temp1,ro_out)
cmar-p
      ro_out=1.
	delpr_ac=delpr_ac/ro_out
      pout = pout + delpr_ac
c-bpcorr
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
      if (.not.(iopt .or. iopt_mess)) then
c NOTA: delpr1 e delpr2 comprendono sia il dp dovuto al piping e il dp dovuto
c       alla presenza di eventuali filtri ed aircooler
        delpr1 = delpr1 + delpr_f
        delpr2 = delpr2 + delpr_ac
      endif
c-bpcorr-end

cgpe      if(pin.le.0.or.pout.le.0) then
cgpe      endif
cgpe      if(pin.gt.pout) then
cgpe      end if
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
c calcolo della portata dalle percentuali
      nutot = 0
      do itip=1,ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
cgpe          flow_man(icj) = flowm/100*fl_flow_man(icj)
          flow_man(icj) = flowm*fl_flow_man(icj)
cgpe-end
        enddo
        nutot = nutot+type_quant(itip)
      enddo
cgpe-end
c--->               
      do i=1,unit_num
        lsim(i) = status(i)
      end do
      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo
c
      do i=1,ntp
        if (type_actr(i).gt.0) then
          cmax_eff(i) = clim(4,i)
        end if
      enddo
c
      call calc_limiti(type_min_rev, type_max_rev, clim_new,cmax_eff)
c
c calcolo i limiti minimi e massimi in portata per ciascun compressore e
c la portata minima e massima totale fattibile
      ier_save = 0
      if (.not.iopt .and. .not.iopt_mess) then
        qmin_tot = 0.
        qmax_tot = 0.
        do i = 1,ntp
          if (type_actr(i).gt.0) then
            call calc_punti_bifase(qmin_assoluto(i),qmax_assoluto(i),
     *           hmin_assoluto(i),hmax_assoluto(i),vinc_min(i),
     *           vinc_max(i),i,ier)
c - gestione errori:
            if (ier.ne.0) then
              ier_save = ier
cgpe              if (ier.eq.12) then
              if (ier.eq.52) then
!-23/08/2000
!                write(mess,561) i
!                call app_mess(vert_pointer,ier,mess)
!                ier = 1
!                if (iopt .or. iopt_mess) return
                ier = 1
                if (iopt .or. iopt_mess) return
                write(mess,561) i
                call app_mess(vert_pointer,ier_save,mess)
                return
!-23/08/2000-end
              end if
561   format('Tipo di compressore: ',i3)
            endif
            qmin_tot = qmin_tot + qmin_assoluto(i)*type_actr(i)
            qmax_tot = qmax_tot + qmax_assoluto(i)*type_actr(i)
          end if
        end do
      endif
c
!-new
      if (ier_save .ne. 0) then
c errore: pressione richiesta superiore alla massima fattibile
cgpe        ier = 21
cgpe-mess        ier = 71
        ier = 52
        if (iopt .or. iopt_mess) return
cgpe-mess        call gest_error (1,0,'','Compressione richiesta non fattibile',0)
        if (fl_sim) then
          mess = ' '
          call app_mess(vert_pointer,ier,mess)
        endif

        RETURN

      endif
c
c simulazione normale
c flow_man(icj) rappresenta la portata che ciascuna unita'' attiva deve lavorare.
c Su ciascun tipo di compressore la portata minima e massima sono memorizzate in
c qmin_assoluto(itip) e qmax_assoluto(itip) e il tipo di vincolo e' dato da
c vinc_min dalla subroutine calc_punti_bifase
      nutot = 0
      do itip=1,ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij

          if (status(icj)) then
	      if (flow_man(icj).le.qmin_assoluto(itip)) then
c trattamento da qmin
              if (.not.iopt .and. .not.iopt_mess) then
                if ((vinc_min(itip).eq.sur_1f)  .or. 
     *              (vinc_min(itip).eq.sur_2f)) then
cgpe-mess                  write(mess,557) itip, 'Parabola'
                  ker = 21
                else
cgpe-mess                  write(mess,557) itip, 'Giri'
                  ker = 22
                endif
557             format('Tipo vincolato: ',i2,' - Tipo vincolo: ',a)
cgpe-mess                ker = 93
cgpe-corr                mess = ' '
c-UDM565     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A11)
c-UDM                write(mess,565) qmin_assoluto(itip),flow_man(icj),UDM_INT_QM
565     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
                write(mess,565)
     *                udm_out_str(udm_QM,qmin_assoluto(itip),0),
     *                udm_out_str(udm_QM,flow_man(icj),1)
cgpe-corr-end
                ier = 0
                call app_mess(vert_pointer,ker,mess)
cgpe-new                call gest_error (3,0,'COMPSTAT_MAN',
cgpe-new     *               'Non si rispetta il criterio',0)
cgpe-new                call gest_error (3,0,'',
cgpe-new     *               'Unita'' vincolata al minimo',0)
cgpe-new                call gest_error (3,0,'',mess,0)
              end if
              tip_vinc(icj)  = vinc_min(itip)
              qass(icj)      = qmin_assoluto(itip)
              hass(icj)      = hmin_assoluto(itip)
              qmin_tot=qmin_tot-qmin_assoluto(itip)*type_actr(itip)
              qmax_tot=qmax_tot-qmax_assoluto(itip)*type_actr(itip)
	      elseif (flow_man(icj).ge.qmax_assoluto(itip)) then
c trattamento da qmax
c le macchine piu' limitanti vanno imposte al massimo (le piu' limitanti
c secondo il criterio) - si riprova con le altre
              if (.not.iopt .and. .not.iopt_mess) then
                if ((vinc_max(itip).eq.cho_1f)  .or.
     *              (vinc_max(itip).eq.cho_2f)) then
cgpe-mess                  write(mess,557) itip, 'Parabola'
                  ker = 24
                elseif ((vinc_max(itip).eq.mar_1f)  .or.
     *                  (vinc_max(itip).eq.mar_2f))  then 
cgpe-mess                  write(mess,557) itip, 'Giri'
                  ker = 23
                else
cgpe-mess                  write(mess,557) itip, 'Potenza'
                  ker = 25
                endif
cgpe-mess                ker = 94
cgpe-corr                mess = ' '
c-UDM                write(mess,565) qmax_assoluto(itip),flow_man(icj),UDM_INT_QM
                write(mess,565)
     *                udm_out_str(udm_QM,qmax_assoluto(itip),0),
     *                udm_out_str(udm_QM,flow_man(icj),1)
cgpe-corr-end
                ier = 0
                call app_mess(vert_pointer,ker,mess)
cgpe-new                call gest_error (3,0,'COMPSTAT_MAN',
cgpe-new     *               'Non si rispetta il criterio',0)
cgpe-new                call gest_error (3,0,'',
cgpe-new     *               'Unita'' vincolata al massimo',0)
cgpe-new                call gest_error (3,0,'',mess,0)
              end if
              tip_vinc(icj)  = vinc_max(itip)
              qass(icj)      = qmax_assoluto(itip)
              hass(icj)      = hmax_assoluto(itip)
              qmin_tot=qmin_tot-qmin_assoluto(itip)*type_actr(itip)
              qmax_tot=qmax_tot-qmax_assoluto(itip)*type_actr(itip)
c
	      elseif ((qmin_assoluto(itip).lt.flow_man(icj)).and.
     *              (flow_man(icj).lt.qmax_assoluto(itip))) then
c simulazione normale delle macchine residue
              uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
              aux3     = ROCN*uno_su_ro_attuale
c - completamento common
              un_flow1 = flow_man(icj)
              flow     = flow_man(icj)/aux3
              call limiti_rev(flow_man(icj),rev_min,rev_max,itip)
              itipo = itip
              write(LGU,*)'ZG CALL 05'
              rb = zero_giri(rev_min)
              write(LGU,*)'ZG CALL 06'
              ra = zero_giri(rev_max)
              if (((ra*rb).lt.0)         .or.
     *             (abs(ra).le.eps_giri) .or.
     *             (abs(rb).le.eps_giri) ) then
                if ((ra*rb).lt.0 ) then
                  rd = rev_max
cgpe-fun                  call zero_fun1(rev_min,rev_max,rd,rb,ra,rsol,
                  call zero_fun(rev_min,rev_max,rd,rb,ra,rsol,
     *                      epsma,pmach,zero_giri)
                  rev = rsol
                else
                  if (abs(ra).le.eps_giri) then
                    rev = rev_max
                  else
                    rev = rev_min
                  endif
                endif
              endif
              call qdan_ch(clim(3,itipo),qaux,rev,chn(1,itipo))
              if (flow_man(icj).le.qaux) then
                call hdaqn(haux,flow_man(icj),rev,chn(1,itipo))
              else
                call hdaqn(haux,flow_man(icj),rev,chc(1,itipo))
              end if
              qass(icj)      = flow_man(icj)
              hass(icj)      = haux

	      else
cgpe   errore!?!?!?!?!

            end if
          end if
        enddo
        nutot = nutot+type_quant(itip)
      enddo

c
c
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

cmar_pow_stat
      nutot = 0
      do itip=1,ntp
        if(type_act_old(itip).gt.0) then
          do ij =1,type_quant(itip)
            icj = nutot+ij
            if(status(icj)) then
cmar_pow_stat
cccccccccc_312      min_p=min_p+type_vinc_minpow(itip)
cccccccccc_312      max_p=max_p+type_vinc_maxpow(itip)
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
            if(status(icj)) then
              write(LGU,*)'VALORE ntp: ',ntp
              write(LGU,*)'VALORE type_quant(itip):',type_quant(itip)
              write(LGU,*)'ITER itip: ',itip
              write(LGU,*)'INPUT hass : ',hass(itip)
              write(LGU,*)'INPUT qass : ',qass(itip) 
              write(LGU,*)'VALORE unit_num:',unit_num
              call dati_compress2(hass(icj),qass(icj),
     *                qmax_assoluto(itip),qmin_assoluto(itip),
     *                tip_vinc(icj),itip,
     *                icj,
     *                unit_head(icj),
     *                unit2_head(icj),unit_rev(icj),unit2_rev(icj),
     *                unit_flow(icj),unit2_flow(icj),unit_eff(icj),
     *                unit2_eff(icj),unit_temp(icj),unit2_temp(icj),
     *                unit_max(icj),unit_min(icj),unit_vcrit(icj),
     *                unit_vinc(icj),unit_eff_corr(icj),
     *                unit2_eff_corr(icj),unit_power_corr(icj),
     *                unit2_power_corr(icj),unit_power(icj),
     *	              unit2_power(icj),unit_hrate_corr(icj),
     *                unit_hrate(icj),unit_cons(icj),
     *                ac1_k(icj),
     *                unit_pres_int(icj),unit_temp_int(icj), 
     *                delta_in_pressione(icj), unit_deltrint(icj),
     *                 unit_delpr_ac(icj), 
     *                stat_varsp, ier)
c-ele 28/4/06
               write(LGU,*)'AC CALL 16'
               if(pres_for_fase2 .eq.0.0) then
                     pres_for_fase2 = press_out_calc(itip)
                     temp_for_fase2 = unit2_temp(icj)
               end if
               call single_out_temp_ac_new(tair,press_out_calc(itip),
     *          icj,2,
     *                  unit2_flow(icj),stat_varsp(7),unit2_temp(icj),
c-ele 1/6/6     *                  ,deltat)
     *    pres_for_fase2 - delta_in_pressione(icj) - delta_ac1(icj),
     *             temp_for_fase2,deltat(icj))



	 
cmar_pow_stat 
	power_tot_staz=power_tot_staz+unit_power(icj)+unit2_power(icj)
cmar_pow_stat



c-bpcorr
c-l_ric              if (unit_vinc(icj) .ne. sur_1f .and.
c-l_ric     *            unit_vinc(icj) .ne. sur_2f     ) then
c-l_ric                l_ric = .false.
c-l_ric              end if
c-bpcorr-end
c controllo dell'errore in uscita dalla dati_compress2
              if (ier.gt.0) then
                if (fl_sim) then
cgpe-mess                  call gest_error (1,0,'',' Violata la max_power',0)
                  mess = ' '
cgpe-mess                  ker = 14
                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
                else
!- violata la massima potenza, non viene dato il punto dentro le mappe
!- dei compressori in quanto questa situazione dovrebbe essere grave
                  ier = 1
                  return
                end if
c-mf03
              else
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
                pow_app = unit_power(icj) + unit2_power(icj)
                ier = ind_vio_vinc(itip,pow_app,unit_rev(icj))
c gestione dell'errore differente dai vari moduli ?????
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
	tot_flow_man = 0.
      do icj=1,unit_num
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)
        tot_flow_man = tot_flow_man+flow_man(icj)
      end do

      temp2 = 0.
      do icj=1,unit_num
!          percent = unit_flow(icj)/tot_flow
!          temp2 = temp2 + percent * unit2_temp(icj)
        unit_perc(icj) = unit_flow(icj)/tot_flow
c-ele 28/4/06
c        temp2 = temp2 + unit_perc(icj) * unit2_temp(icj)
        temp2 = temp2 + unit_perc(icj) * (unit2_temp(icj)-deltat(icj))
      end do
c---->intervento su deltat aircooler
      temp2 = temp2 - stat_vars(5)
      call out_temper_ac_ce_new(tair,press_out_calc(icj),
     *  tot_flow,stat_vars(7)
     *  ,pres_for_fase2 - delta_in_pressione(icj) - delta_ac1(icj)
     ^ ,temp_for_fase2,temp2)

c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning
cmar_AC_auto
      IF (.not.FLAG_AC)THEN
cmar_AC_auto

c_ecol
      if (temp2.gt.stat_vars(7)) then
        ker=79
        if (fl_sim) then
         if (.not.iopt_mess) then
cgpe-mess          call gest_error (2,0,'','Superata la massima temperatura di mandata',0)
c-UDM          write (mess,570) temp2,stat_vars(7),UDM_INT_T ! write (mess,570) temp2,stat_vars(7)
c-UDM570       format('T man: ',f7.2,' - T man max: ',f7.2,'  ',A3)
          write(mess,570) udm_out_str(udm_T,temp2,0),
     *                    udm_out_str(udm_T,stat_vars(7),1)
570   format('T man: ',A10,' - T man max: ',A25)

          call app_mess(vert_pointer,ker,mess)
         endif
        end if
      end if
cmar_AC_auto
	ENDIF
cmar_AC_auto

c
cgpe      flow_ric = 0.
c    ********* 2026/01/27 NUOVO CONTROLLO SU PRESSIONE DI USCITA FASE 2 MAGGIORE DELLA PRESSIONE DI MANDATA***********
      do itip= 1, ntp
        write(LGU,*)'Valore pres_mand input:',pres2
        write(LGU,*)'Valore press_out:', press_out_calc(itip) 
        presMandataCalc = press_out_calc(itip) - delpr2
        write(LGU,*)'Valore pres_mand calc:',presMandataCalc
        if(abs(pres2 - presMandataCalc) .gt. 0.01 
     *   .and. press_out_calc(itip) .gt. 0.0) then
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
      l_ric = .true.
c test se tutti i vincoli delle unità attive sono sur_1f
      nutot = 0
      do itip = 1, ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
          if (status(icj).and.(tip_vinc(icj).ne.sur_1f)) then
            l_ric = .false.
          endif
        enddo
        nutot = nutot+type_quant(itip)
      enddo

      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow.lt.flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
          if (iopt .or. iopt_mess) return
cgpe-mess          call gest_error (1,0,'','Portata superiore alla massima fattibile',0)
cgpe-mess          write(mess,558) (tot_flow*conv_ns)/old_aux3,(tot_flow_man*conv_ns)/old_aux3
c-UDM          write(mess,558) tot_flow/old_aux3,tot_flow_man/old_aux3,UDM_INT_Q
          write(mess,558) udm_out_str(udm_Q,tot_flow/old_aux3,0),
     *                    udm_out_str(udm_Q,tot_flow_man/old_aux3,1)
cgpe-mess558     format(' Portata lavorata: ',f7.2,' *** Portata richiesta: ',f7.2)
c-UDM558      format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
558      format('Q lavorata: ',A10,' - Q richiesta: ',A25)
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
              ier = 20
            endif
cgpe            if (l_ric) ier = 0
            if (l_ric) then
c-bpcorr	        ier = 0
cgpe              flow_ric = tot_flow/old_aux3
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
cgpe-mess              write(mess,558) (tot_flow*conv_ns)/old_aux3,(flowm*conv_ns)/old_aux3
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