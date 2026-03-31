c.......................................................................
c  pacchetto contenente le routines per gestire il calcolo del punto di
c  lavoro ko con TBG:
c  - in parallelo (Single_Station_ko)
c  - in serie     (Comp_Station_ko)
c.......................................................................
c-bp08
	Subroutine Single_Station_ko(istaz,vert_pointer,tipo_criterio,
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
      integer*4 k,nu,ij


		real*4 a

       real*4      dp1_M,dp2_M

c-----------------------------------------------------------------------
c---->              salvataggio condizioni iniziali
csave      call save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
c
      do i = 1, unit_num
        stat(i) = funz_off
        if (lstatus(jufir+i-1) .eq. 1) then
c          stat(i) = .true.
          stat(i) = funz_on
        endif
      enddo

c---->              assign zero all the station variable
      call init_simpstat_mf (istaz,vert_pointer,tipo_criterio,pres1,
     *          pres2,air,tot_cons,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,mirev,
     *          marev,mieff,lstat,mirev2,marev2,mieff2,stat_ord,
     *          stat_vars(1))

cgpe-corr
c      do i = 1, unit_num
c        type_actr(i) = 0
c        if (stat(i) .eq. funz_on) then
cc          stat(i) = .true.
c          type_actr(i) = 1
c        endif
c      enddo
      nu = 0
      do i = 1,type_num
        type_actr(i) = 0
        do k = 1,type_quant(i)
          ij = nu+k
          if (lstat(ij)) then
            type_actr(i)=type_actr(i) + 1
          end if
        end do
        nu = nu+type_quant(i)
      end do
cgpe-corr-end



      

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
      call Station_ko (vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
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

	ac_ce_stat=ac_ce_stat_M
 
      ac1_stat =	ac1_stat_M

	FLAG_AC=.FALSE.


	
cmar_1809
      type_actr=type_actr_M
cmar_1809





cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_13

c---->                                       simulazione stazione
      call Station_ko (vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
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
      call Station_ko (vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
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

      Subroutine Station_ko(vert_pointer,flow,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,nom_flow,
     *     unit_perc,unit_rev,unit_flow,unit_head,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,unit_hrate_corr,
     *     unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c - calcolo di head
c - assegnazione di q = 0. per i TBG che hanno head>hmax oppure head<hmin
c - suddivisione della flow_stat(ivert) fra i TBG OK e proporzionalmente alla q di progetto
c   di ciascun TBG
c - verifica se il punto (q,h) così determinato sta entro i limiti del TBG e nel caso
c   calcolo della potenza
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
c



cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat


      COMMON/MAR/istaz
      integer*4 istaz

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
      real*4   ra,rb,rd,fa,fb
      real*4   prop
      EXTERNAL prop
      real*4   uno_su_ro_attuale
      integer*4  type_act_old(maxunits)
      character*(max_len) mess/' '/
c      real*4   FLOW_APP
c --- new gz
      real*4    qmin_tot,qmax_tot
c      real*4    qmin                     ! portata piu' vincolante
c      real*4    qmax                     ! portata piu' vincolante
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
      real*4    qass(maxunits),qass2(maxunits)
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 icj,ij,nutot,k
      real*4    tot_flow
      real*4    rev,rev2
      real*4    zero_perc2
      EXTERNAL  zero_perc2
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      logical*2 l_ric
      real*4    flow_ko(maxunits)
      real*4    tout_ac,delpr_f,delpr_ac
      real*4    tot_nom_flow,tot_flow_ko
c-mf03
      real*4    pow_app
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18
      real*4 ro_in,ro_out
c_mess_090805
      real*4 max_hmax, min_hmin
c_mess_090805
cgpe
      real*4 min_hmax, max_hmin
cgpe-end
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


      ier = 0
      ker = 0
cgpe      if (flow.le.0.) then
cgpe      endif
c---->             
c              CADUTE DI PRESSIONE DOVUTE AL PIPING
      pin  = pres1 - delpr1
      pout = pres2 + delpr2
c              CADUTA DI PRESSIONE DOVUTA AL FILTRO
      call delpr_filtro (ntp,type_quant,status,nom_flow,
     *f_ce_k,f_tg_k,flow,delpr_f)
c-ace18
      call ro_real(ind,pin,temp1,ro_in)
cmar-p 
      ro_in=1.
	delpr_f=delpr_f/ro_in
      pin  = pin - delpr_f
c              CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_p (ntp,type_quant,status,unit_bifase,nom_flow,
     *ac_ce_k,ac1_k,ac2_k,flow,delpr_ac)
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
c
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

c_mess_090805
      max_hmax = 0.
      min_hmin = R_MAX
c_mess_090805
cgpe
      min_hmax = R_MAX
      max_hmin = 0.
cgpe-end
c-old      do itip=1,unit_num
c-old       if (lsim(itip)) then
c-oldc---> calcolo gli estremi in prevalenza (hmax,hmin)  del TBG
c-old        call limiti_h_un(itip,hmin,hmax)
c-old        if (max_hmax.lt.hmax) max_hmax=hmax
c-old        if (min_hmin.gt.hmin) min_hmin=hmin
c-oldcgpe
c-old        if (min_hmax .gt. hmax) min_hmax = hmax
c-old        if (max_hmin .lt. hmin) max_hmin = hmin
c-oldcgpe-end
c-old        if (head.gt.hmax) then
c-old          lsim(itip)      = .false.
c-old          unit_flow(itip) = 0.
c-old          unit_head(itip) = head
c-old          tip_vinc(itip)  = max_ah
c-oldcgpec Per il calcolo di unit_eff_turb=ckWh/unit_hrate e' necessario mettere un valore
c-oldcgpec di default per unit_hrate
c-oldcgpe          unit_hrate(itip) = 1.
c-old        else if (head.lt.hmin) then
c-old          lsim(itip)      = .false.
c-old          unit_flow(itip) = 0.
c-old          unit_head(itip) = head
c-old          tip_vinc(itip)  = min_ah
c-oldcgpec Per il calcolo di unit_eff_turb=ckWh/unit_hrate e' necessario mettere un valore
c-oldcgpec di default per unit_hrate
c-oldcgpe          unit_hrate(itip) = 1.
c-old        endif
c-old       endif
c-old      enddo

c-new
      nutot = 0
      do itip = 1,ntp
        do k = 1,type_quant(itip)
          icj = nutot + k
          if (lsim(icj)) then
c---> calcolo gli estremi in prevalenza (hmax,hmin)  del TBG
            call limiti_h_un(itip,hmin,hmax)
            if (max_hmax.lt.hmax) max_hmax=hmax
            if (min_hmin.gt.hmin) min_hmin=hmin
cgpe
            if (min_hmax .gt. hmax) min_hmax = hmax
            if (max_hmin .lt. hmin) max_hmin = hmin
cgpe-end
            if (head.gt.hmax) then
              lsim(icj)      = .false.
              unit_flow(icj) = 0.
              unit_head(icj) = head
              tip_vinc(icj)  = max_ah
cgpe-corr-staz
              type_actr(itip) = type_actr(itip) - 1
cgpe-corr-staz-end
cgpec Per il calcolo di unit_eff_turb=ckWh/unit_hrate e' necessario mettere un valore
cgpec di default per unit_hrate
cgpe              unit_hrate(icj) = 1.
            else if (head.lt.hmin) then
              lsim(icj)      = .false.
              unit_flow(icj) = 0.
              unit_head(icj) = head
              tip_vinc(icj)  = min_ah
cgpe-corr-staz
              type_actr(itip) = type_actr(itip) - 1
cgpe-corr-staz-end
cgpec Per il calcolo di unit_eff_turb=ckWh/unit_hrate e' necessario mettere un valore
cgpec di default per unit_hrate
cgpe              unit_hrate(icj) = 1.
            endif
          endif
        enddo
        nutot = nutot + type_quant(itip)
      enddo
c-new-end

c_mess_090805
 	  if (fl_sim) then
          if (head.gt.max_hmax) then
            ker = 6
c-UDM            write(mess,557) head,max_hmax,UDM_INT_H
c-UDM557     format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
557     format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
            write(mess,557) udm_out_str(udm_H,head,0),
     *                      udm_out_str(udm_H,max_hmax,1)
            call app_mess(vert_pointer,ker,mess)
          elseif (head.lt.min_hmin) then
            ker = 7
c-UDM          write(mess,559) head,min_hmin,UDM_INT_H
c-UDM559     format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
559     format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)
            write(mess,559) udm_out_str(udm_H,head,0),
     *                      udm_out_str(udm_H,min_hmin,1)
            call app_mess(vert_pointer,ker,mess)
          endif
        endif
c_mess_090805
c
cgpe
      if (ker .gt. 0.) then
c-corr-staz        if (max_hmax .eq. min_hmax .or.
c-corr-staz     *      max_hmin .eq. min_hmin     ) then
c caso di prevalenza non fattibile per tutti i compressori (caso di compressori tutti
c uguali)
          ier = ker
          return
c-corr-staz        endif
cpuntoko      else
cpuntoko        if (max_hmax .eq. min_hmax .or.
cpuntoko     *      max_hmin .eq. min_hmin     ) then
cpuntokoc caso di prevalenza non fattibile per tutti i compressori (caso di compressori tutti
cpuntokoc uguali)
cpuntoko          ier = ker
cpuntoko          return
cpuntoko        endif
c-corr-staz-end
      endif
cgpe-end
      tot_nom_flow = 0.
      do i = 1, unit_num
        if (status(i) .and. lsim(i)) then
          tot_nom_flow = tot_nom_flow + nom_flow(i)
        endif
	enddo
c calcolo della portata di ciascun TBG in proporzione alle portate nominali fra quelli che
c erano attivi inizialmente e che non sono gia' stati trattati (perche' con head fuori
c mappa)
      nutot = 0
      do itip = 1,ntp
        do ij = 1,type_quant(itip)
          icj = nutot+ij
	    if (status(icj) .and. lsim(icj)) then
            flow_ko(icj) = flowm*(nom_flow(icj)/tot_nom_flow)
          endif
        enddo
        nutot = nutot + type_quant(itip)
      enddo
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
c flow_ko(iunit) rappresenta la portata che ciascuna unita'' attiva deve lavorare.
c Su ciascun tipo di compressore la portata minima e massima sono memorizzate in
c qmin_assoluto(itip) e qmax_assoluto(itip) e il tipo di vincolo e' dato da
c vinc_min dalla subroutine limiti_q
      nutot = 0
      do itip=1,ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
          if (status(icj) .and. lsim(icj)) then
            if (.not. unit_bifase(icj)) then
              if (flow_ko(icj) .le. qmin_assoluto(itip)) then
c trattamento da qmin
                tip_vinc(icj)  = vinc_min(itip)
c                qass(icj)      = qmin_assoluto(itip)-qmin_assoluto2(itip)
c                qass2(icj)     = qmin_assoluto2(itip)
                if (tip_vinc(icj) .eq. sur_1f) then
                  qass(icj)  = qmin_assoluto(itip)
                else
                  lsim(icj)  = .false.
ccorr-lric_minrev
c non è ammesso il riciclo sul minimo numero di giri
c                  status(icj)  = .false.
ccorr-lric_minrev-end
                  unit_flow(icj)  = flow_ko(icj)
                  unit_head(icj)  = head
cgpe                  unit_hrate(icj) = 1.
                  unit2_flow(icj) = 0.
                  unit2_head(icj) = 0.
                endif

 	        elseif (flow_ko(icj).ge.qmax_assoluto(itip)) then
c trattamento da qmax
                lsim(icj)  = .false.
                tip_vinc(icj) = vinc_max(itip)
c                qass(icj)     = qmax_assoluto(itip) -qmax_assoluto2(itip)
c                qass2(icj)    = qmax_assoluto2(itip)
                unit_flow(icj)  = flow_ko(icj)
                unit_head(icj)  = head
cgpe                unit_hrate(icj) = 1.
                unit2_flow(icj) = 0.
                unit2_head(icj) = 0.

cgpe-0012
c miglioramento del file di log
                if (vinc_max(itip) .eq. pow_1f) then
cgpe-mess                  write(mess,558) itip, 'Potenza'
                  ker = 26
                  write(mess,565)
     *                udm_out_str(udm_QM,qmax_assoluto(itip),0),
     *                udm_out_str(udm_QM,flow_ko(icj),1)
                  call app_mess(vert_pointer,ker,mess)

565     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
                endif
cgpe-0012-end
              else if ((qmin_assoluto(itip).lt.flow_ko(icj)).and.
     *                 (flow_ko(icj).lt.qmax_assoluto(itip))) then
c simulazione normale delle macchine residue
                if (.not.(type_bifase(itip))) then
c assegnazione di qass ai tipi monofase (i dati vengono calcolati nella dati_compressore)
                  qass(icj) = flow_ko(icj)
                else
c assegnazione di qass e qass2 ai tipi bifase
c test se flow_ko(icj)/2 sta dentro la mappa (confronto con la
c      qmax_assoluto(itip) -qmax_assoluto2(itip) sulla prima fase e qmax_assoluto2(itip)
c      sulla seconda fase) nel qual caso si calcolano i dati nella dati_compressore,
c      altrimenti si assegna lsim(icj) = .false.
c      Proposta: si può provare a chiedere al modello di riaprtire il gas fra le fasi
c                e nel caso in cui non si trovi lo zero della funzione si assegna
c                unit_flow(icj)  = flow_ko(icj)/2
c                unit2_flow(icj) = unit_flow(icj)
c                lsim(icj)       = .false.
                  itipo = itip
                  flow_unit = flow_ko(icj)
                  qmin_assoluto(itip) = qmin_assoluto(itip) - 
     *                                  qmin_assoluto2(itip)
                  qmax_assoluto(itip) = qmax_assoluto(itip) - 
     *                                  qmax_assoluto2(itip)
                  fa = zero_perc2(qmin_assoluto(itip))
                  fb = zero_perc2(qmax_assoluto(itip))
                  if ((fa*fb).lt.0 ) then
                    ra = qmin_assoluto(itip)
                    rb = qmax_assoluto(itip)
                    rd = (ra + rb)/2.
cgpe-fun                    call zero_fun1(ra,rb,rd,fa,fb,qcrit(itip),epsma,
                    call zero_fun(ra,rb,rd,fa,fb,qcrit(itip),epsma,
     *                   pmach,zero_perc2)
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
     *                          chn2(1,itip), chc2(1,itip))
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
     *                                  qmin_assoluto2(itip)
                  qmax_assoluto(itip) = qmax_assoluto(itip) + 
     *                                  qmax_assoluto2(itip)
	          end if

ccc              else
ccc   errore?!?!?!?!?
              end if

            end if
          end if
        enddo
        nutot = nutot+type_quant(itip)
      enddo
c
c definizione della variabile l_ric
      l_ric = .true.
      nutot = 0
      do itip = 1, ntp
        do ij =1,type_quant(itip)
          icj = nutot+ij
          if (tip_vinc(icj).ne.0) then
            if (status(icj).and.(tip_vinc(icj).ne.sur_1f)) then
c test se tutti i vincoli delle unità attive sono sur_1f
              l_ric = .false.
            endif
          endif
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
ccccccc_312      min_p=min_p+type_vinc_minpow(itip)
ccccccc_312      max_p=max_p+type_vinc_maxpow(itip)
cmar_pow_stat
        end if
      end do
c-bpcorr
c-l_ric      l_ric = .true.
c-bpcorr-end
      nutot = 0
      do itip = 1,ntp
        if(type_act_old(itip).gt.0) then
          do ij = 1,type_quant(itip)
            icj = nutot+ij
cgpe            if(status(icj)) then
            if(lsim(icj)) then
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
c-bpcorr
c-l_ric              if (unit_vinc(icj) .ne. sur_1f .and.
c-l_ric     *            unit_vinc(icj) .ne. sur_2f     ) then
c-l_ric                l_ric = .false.
c-l_ric              end if
c-bpcorr-end


cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)+unit2_power(icj) 
cmar_pow_stat



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
                if (fl_vinc .and. ier.gt.0) then
c gestione dell'errore differente dai vari moduli
                  unit_vinc(icj) = ier
                endif
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
      tot_flow_ko = 0.
      do icj=1,unit_num
c calcolo di tot_flow e tot_flow_ko (portata totale lavorata e richiesta)
        tot_flow = tot_flow+unit_flow(icj)
        tot_flow_ko = tot_flow_ko + flow_ko(icj)
        tot_cons = tot_cons+unit_cons(icj)
        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do

cgpe      flow_ric = 0.
      flow_ric = tot_flow/aux3
      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow .lt. flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
          if (fl_sim) then
cgpe-mess            call gest_error (1,0,'','Portata superiore alla massima fattibile',0)
c-UDM            write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3, (flowm*conv_ns)/aux3
c-UDM560     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12) ! format(' Portata lavorata: ',f7.2,' *** Portata richiesta: ',f7.2)
560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
             write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                       udm_out_str(udm_Q,flowm/aux3,1)
            call app_mess(vert_pointer,ier,mess)
          endif
        else if (tot_flow.gt.flowm) then
c tutte le unita' sono in riciclo
!!          if (iopt .or. iopt_mess) return
c-l_ric          if (.not.iopt_mess) then
cgpe-new            if (l_ric) then
cgpe-new              ier = 27
cgpe-new            else
cgpe-new              ier = 20
cgpe-new            endif
            if (l_ric) then
c-bpcorr              ier = 0
cgpe              flow_ric = tot_flow/aux3
              if (fl_sim) then
cgpe-mess                call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM                write(mess,560) tot_flow/aux3,tot_flow_ko/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3, (tot_flow_ko*conv_ns)/aux3
               if (.not.iopt_mess) then
                write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                          udm_out_str(udm_Q,tot_flow_ko/aux3,1)
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
c-ele 26/10/2006 Proteggo da una divisione per 0
          if (tot_flow.ne.0) then
             unit_perc(icj) = unit_flow(icj)/tot_flow
	    else
             unit_perc(icj) = 0.	    
		end if
          if (unit_bifase(icj)) then
c-ele 26/10/2006 Proteggo da una divisione per 0
             if (tot_flow.ne.0) then
                unit2_perc(icj) = unit2_flow(icj)/tot_flow
	       else
                unit2_perc(icj) = 0.
	       end if
          endif
        endif
      enddo
c



  

      return
      end
c-bp08-end
c-bp08
      subroutine limiti_h_un_ok_orig(itip,hmin,hmax)
c
      implicit none
c
c calcola i limiti in prevalenza del compressore itip
c
c       min_rev     I   numero di giri minimo per ciascun tipo
c       max_rev     I   numero di giri massimo per ciascun tipo
c       tipo_surge  I   tipo di surge per ciascun tipo
c       hmin        O   prevalenza minima di stazione
c       hmax        O   prevalenza massima di stazione
c----------------------------------------------------------------------
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
        include '../inc/prototipo.INC'
c
      INTEGER*4 itip       ! I)
      real*4    hmin,hmax  ! O)
c
      INTEGER*4 irange
      real*4    h,q
c---------------------------------------------------------------------
c      hmax=100000.
c      hmin=0.
c
c distinzione caso in cui la surge sia una spezzata o una parabola
      if (tipo_surge.eq.parabola) then
        call hdan_su_mf(cmin_eff(itip),hmax,type_max_rev(itip),
     *       chn(1,itip))
      else
        call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *       nlim(itip),lim_n(1,itip),type_max_rev(itip),hmax,q,
     *       irange)
      end if
c      if (h.lt.hmax) hmax=h
      
	call hdan_ch(clim(4,itip),hmin,type_min_rev(itip),chc(1,itip))
c      if (h.gt.hmin) hmin=h

      if(type_bifase(itip)) then
        if (tipo_surge.eq.parabola) then
          call hdan_su_mf(clim2_new(itip),hmax,type2_max_rev(itip),
     *         chn2(1,itip))
        else
          call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *         b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *         type2_max_rev(itip),hmax,q,irange)
        end if
c        if (h.lt.hmax) hmax=h

        call hdan_ch(clim2(4,itip),hmin,type2_min_rev(itip),
     *       chc2(1,itip))
c        if (h.gt.hmin) hmin=h

      endif

      return
      end
c-bp08-end


      subroutine limiti_h_un(itip,hmin,hmax)
c
      implicit none
c
c calcola i limiti in prevalenza della stazione
c
c       min_rev     I   numero di giri minimo per ciascun tipo
c       max_rev     I   numero di giri massimo per ciascun tipo
c       tipo_surge  I   tipo di surge per ciascun tipo
c       hmin        O   prevalenza minima di stazione
c       hmax        O   prevalenza massima di stazione
c----------------------------------------------------------------------
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
        include '../inc/prototipo.INC'
cmar
	common/equi_flag/flag_equi_s(max_unit)
      common/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)

	logical*2 flag_equi_s
      integer*4 tipo_criterio
	real*4 perc_equi_min, perc_equi_max
	real*4 kmin, kmax, qmn, qmx
cmar
c
        real*4    hmin,hmax
c
        INTEGER*4 itip
        INTEGER*4 irange(maxunits)
        real*4    h,q
c---------------------------------------------------------------------
      hmax=100000.
      hmin=0.
c

cm
       if (tipo_crit .eq. crit_equi) then
      
c       if (1) then

      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c-prototipo
c distinzione caso in cui la surge sia una spezzata o una parabola
cm          if (tipo_surge.eq.parabola) then

            if(flag_equi_s(itip) ) then
c      if(1 ) then
cm  parabola


            call hdan_su_mf(cmin_eff(itip),hmax,type_max_rev(itip),
     *                    chn(1,itip))
cmar
            call qdan_ch(cmin_eff(itip), q, type_max_rev(itip), 
     *		              chn(1,itip))
cmar
          else
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *             nlim(itip),lim_n(1,itip),type_max_rev(itip),hmax,q,
     *             irange)


cmar   to calculate:

      qmx= q*(1+perc_equi_min(itip))
	qmx=qmx/qmrif
	hmax=hmax/headrif
	kmin=hmax/(qmx**2)

	call qdan_ch(kmin, q, type_max_rev(itip), 
     *		              chn(1,itip))

	call hdaq_ch(kmin, hmax, q)

c	h=h*headrif

c	q= qmx*qmrif



cmar
          end if


          if (h.lt.hmax) hmax=h


          call hdan_ch(clim(4,itip),hmin,type_min_rev(itip),chc(1,itip))

cmar

          call qdan_ch(clim(4,itip), q, type_min_rev(itip), 
     *		              chc(1,itip))

       
	qmn= q*(1-perc_equi_max(itip))
	qmn=qmn/qmrif
	hmin=hmin/headrif
	kmin=hmin/(qmn**2)

	call qdan_ch(kmin, q, type_min_rev(itip), 
     *		              chc(1,itip))

	call hdaq_ch(kmin, hmin, q)
      


        
cmar

          if (h.gt.hmin) hmin=h





          if(type_bifase(itip)) then
c prototipo
cmar            if (tipo_surge.eq.parabola) then
              
                 if(nlim2(itip) .eq. 0) then

              call hdan_su_mf(clim2_new(itip),hmax,type2_max_rev(itip),
     *                      chn2(1,itip))
cmar
                call qdan_ch(clim2_new(itip), q, type2_max_rev(itip), 
     *		              chn2(1,itip))
cmar
 
            else
              call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *             b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *             type2_max_rev(itip),hmax,q,irange)


cmar   to calculate:

      qmx= q*(1+perc_equi_min(itip))
	qmx=qmx/qmrif
	hmax=hmax/headrif
	kmin=hmax/(qmx**2)

	call qdan_ch(kmin, q, type2_max_rev(itip), 
     *		              chn2(1,itip))

	call hdaq_ch(kmin, hmax, q)



cmar


            end if
            if (h.lt.hmax) hmax=h
            call hdan_ch(clim2(4,itip),hmin,type2_min_rev(itip),
     *                   chc2(1,itip))


cmar

          call qdan_ch(clim2(4,itip), q, type2_min_rev(itip), 
     *		              chc2(1,itip))

       
	qmn= q*(1-perc_equi_max(itip))
	qmn=qmn/qmrif
	hmin=hmin/headrif
	kmin=hmin/(qmn**2)

	call qdan_ch(kmin, q, type2_min_rev(itip), 
     *		              chc2(1,itip))

	call hdaq_ch(kmin, hmin, q)
      


        
cmar
            if (h.gt.hmin) hmin=h
          endif
        endif
      enddo

cm
cmar se criterio è diverso dall' Equidistanza

      else

cm
        if (tipo_surge.eq.parabola) then
        call hdan_su_mf(cmin_eff(itip),hmax,type_max_rev(itip),
     *       chn(1,itip))
      else
        call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *       nlim(itip),lim_n(1,itip),type_max_rev(itip),hmax,q,
     *       irange)
      end if
c      if (h.lt.hmax) hmax=h
      
	call hdan_ch(clim(4,itip),hmin,type_min_rev(itip),chc(1,itip))
c      if (h.gt.hmin) hmin=h

      if(type_bifase(itip)) then
        if (tipo_surge.eq.parabola) then
          call hdan_su_mf(clim2_new(itip),hmax,type2_max_rev(itip),
     *         chn2(1,itip))
        else
          call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *         b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *         type2_max_rev(itip),hmax,q,irange)
        end if
c        if (h.lt.hmax) hmax=h

        call hdan_ch(clim2(4,itip),hmin,type2_min_rev(itip),
     *       chc2(1,itip))
c        if (h.gt.hmin) hmin=h

      endif

cm
      endif
cm
      return
      end
cmar+++++++++++       limiti_head_06-08-09    +++++++++++++++++++++++++++