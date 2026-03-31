
	Subroutine comp_Station(istaz,vert_pointer,tipo_criterio,
     *          stat_ord,flow2,pres1,pres2,temp1,temp2,
     *          air,comp_ratio,tot_cons,
     *          delpr1,delpr2,pres_int,temp_int,delprint,delt_int,
     *          flowm,flow_ric,type_num,unit_num,jufir,jtfir,
     *          stat_varsp,ier)
c
c***********************************************************************
c       simulazione stazione composita
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
      include '../inc/MF_UNITS.INC'
cgpe      include '../inc/CMF.INC'



cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess
c
      integer*4 istaz,vert_pointer,tipo_criterio,stat_ord
      integer*4 type_num,unit_num,jufir,jtfir,ier
c
      real*4    flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *          tot_cons,delpr1,delpr2,pres_int,temp_int,delprint,
     *          delt_int,flowm,flow_ric
c_ecol
cgpe     *          ,delpr_f,delpr_ac,delpr_ac_int
c_ecol 
      real*4    stat_varsp(*)

      real*4    pres1_RIC,pres2_RIC       
      real*4    mirev(maxunits),marev(maxunits),mieff(maxunits)
      LOGICAL*2 lstat(maxunits)
cvar      real*4    ff1
cvar      real*4    eps_q/0.001/

      integer*4 i,j,k
cvar      real*4    HTCITJ_OLD, PORTTJ_OLD, PMSETJ_OLD, PVSETJ_OLD,
cvar     *          VALSJC_OLD, QSETTJ_OLD
cvar      real*4    PRES1_OLD, PRES2_OLD, FLOW_OLD
cvar      character*3 status_old(maxunits)
cvar      real*4    unit_eff_corr_old(maxunits),
cvar     *          unit_power_corr_old(maxunits)
      integer*4 type_quant_old(maxunits)


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
csave      call save_cond_ini (type_num,jtfir,pres_int,temp_int,
csave     *                    delprint,delt_int,type_quant_old)
      do i = 1, type_num
        j = jtfir+i-1
        type_quant_old(i) = type_quant(j)
      end do

cgaia      qsettj(istaz) = porttj(istaz)
c
cerr10    continue
c
      call init_multi_stadio(istaz,vert_pointer,tipo_criterio,
     *      pres1,pres2,pres1_ric,pres2_ric,air,tot_cons,
csave
     *      pres_int,temp_int,delprint,delt_int,
csave-end
cgpe     *      delpr1,delpr2,flowm,flow_ric,unit_num,type_num,
     *      flowm,flow_ric,unit_num,type_num,
     *      type_quant_old,jufir,
cgpe     *      jtfir,nit,mirev,marev,mieff,lstat)
     *      jtfir,mirev,marev,mieff,lstat,
     *     stat_varsp(1))

c
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




c---->                                      simulazione stazione
      call multi_stadio(vert_pointer,stat_ord,pres1,pres2,temp1,
     *       temp2,delpr1,delpr2,
c_ecol
cgpe     *        delpr_f,delpr_ac,delpr_ac_int,
c_ecol
     *       delprint,pres_int,temp_int,delt_int,
     *       tot_cons,comp_ratio,flowm,flow_ric,type_quant(jtfir),
     *       PRES1_RIC,PRES2_RIC,unit_num,lstat,nom_flow(jufir),
     *       marev,mirev,
     *       mieff,unit_perc(jufir),unit_head(jufir),
     *       unit_rev(jufir),unit_flow(jufir),
     *       unit_eff_corr(jufir),unit_eff(jufir),
     *       unit_power_corr(jufir),unit_power(jufir),
     *       unit_hrate_corr(jufir),
     *       unit_hrate(jufir),unit_cons(jufir),unit_temp(jufir),
c-mf03
c     *       unit_max(jufir),stat_varsp,ier)
     *       unit_max(jufir),stat_varsp,unit_vinc(jufir),ier)



      

CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARS(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  
c      stat_vars(5)=a


	ac_ce_stat=ac_ce_stat_M

      ac1_stat=	ac1_stat_M 

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
	
c---->                                      simulazione stazione
      call multi_stadio(vert_pointer,stat_ord,pres1,pres2,temp1,
     *       temp2,delpr1,delpr2,
c_ecol
cgpe     *        delpr_f,delpr_ac,delpr_ac_int,
c_ecol
     *       delprint,pres_int,temp_int,delt_int,
     *       tot_cons,comp_ratio,flowm,flow_ric,type_quant(jtfir),
     *       PRES1_RIC,PRES2_RIC,unit_num,lstat,nom_flow(jufir),
     *       marev,mirev,
     *       mieff,unit_perc(jufir),unit_head(jufir),
     *       unit_rev(jufir),unit_flow(jufir),
     *       unit_eff_corr(jufir),unit_eff(jufir),
     *       unit_power_corr(jufir),unit_power(jufir),
     *       unit_hrate_corr(jufir),
     *       unit_hrate(jufir),unit_cons(jufir),unit_temp(jufir),
c-mf03
c     *       unit_max(jufir),stat_varsp,ier)
     *       unit_max(jufir),stat_varsp,unit_vinc(jufir),ier)


      ENDIF
     


CMAR     IF1
       ELSE
CMAR     IF1

c---->                                      simulazione stazione
      call multi_stadio(vert_pointer,stat_ord,pres1,pres2,temp1,
     *       temp2,delpr1,delpr2,
c_ecol
cgpe     *        delpr_f,delpr_ac,delpr_ac_int,
c_ecol
     *       delprint,pres_int,temp_int,delt_int,
     *       tot_cons,comp_ratio,flowm,flow_ric,type_quant(jtfir),
     *       PRES1_RIC,PRES2_RIC,unit_num,lstat,nom_flow(jufir),
     *       marev,mirev,
     *       mieff,unit_perc(jufir),unit_head(jufir),
     *       unit_rev(jufir),unit_flow(jufir),
     *       unit_eff_corr(jufir),unit_eff(jufir),
     *       unit_power_corr(jufir),unit_power(jufir),
     *       unit_hrate_corr(jufir),
     *       unit_hrate(jufir),unit_cons(jufir),unit_temp(jufir),
c-mf03
c     *       unit_max(jufir),stat_varsp,ier)
     *       unit_max(jufir),stat_varsp,unit_vinc(jufir),ier)



CMAR     IF1
      ENDIF
CMAR     IF1


c-mf03-end

!      if (iopt) goto 11
cerr      if (iopt .or. iopt_mess) goto 11

c---->  gestione dell'errore

cerr      if (ier .gt. 0) then
cerr        if (ier .le. 5) then
!-ier=1: errore grave / pressione di ingresso o di uscita nulle
!-ier=4: flow negativo
!-ier=5: pressione di ingresso superiore alla pressione di uscita
!- Non e' possibile portare il punto dentro le mappe, ma e' stato imposto
!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cerr          goto 11

cerr        else if (ier .eq. 17) then
cerr          if (nit .lt. nitmax) then
cerr            ff1 = flow_ric
cerr            ff1 = ff1 - (ff1*eps_q)
cerr            PRES1 = PRES1_RIC
cerr            PRES2 = PRES2_RIC
cgpe            call last_recover_station(vert_pointer,ff1,tipo_corr,ier)
cerr            call last_recover_station(vert_pointer,ff1,ier)
c gz   ORA uso flow_ric perche' rappresenta la somma degli unit_flow
c      effettivamente lavorati
cerr            flow2 = ff1

cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if
cerr        else if (ier .eq. 6 .or. ier .eq. 7) then
!-ier=6: pressione di uscita superiore alla massima fattibile
!-ier=7: pressione di uscita inferiore alla minima fattibile
cerr          if (nit .lt. nitmax) then
!-29/08/2000
!            ff1 = flow2
!! PRES1 o PRES2 vengono forniti modificati in output dalla multi_stadio
!            call last_recover_station(vert_pointer,ff1,tipo_corr,ier)
!-29/08/2000-end

cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!- Sono da sono da ripristinare i vecchi CIN
cerr            goto 11

cerr          end if
cerr        endif
cerr      end if
c

!- ier=0 (simulazione OK oppure correzione OK):
!- aggiornamento dei dati dei compressori
      call agg_multi_stadio(stat_ord,type_num,type_quant_old,unit_num,
     *     jtfir,jufir,pres_int,temp_int,delprint,delt_int,lstat)
c
11    continue
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

      Subroutine multi_stadio(vert_pointer,stat_ord,pres1,pres2,
     *            temp1,temp2,delpr1,delpr2,
c_ecol
cgpe     *             delpr_f,delpr_ac,delpr_ac_int,
c_ecol
     *            delp_int,pres_int,temp_int,
     *            delt_int,tot_cons,comp_ratio,flowm,flow_ric,
     *            type_quant,PRES1_RIC,PRES2_RIC,unit_num,status,
     *            nom_flow,
     *            max_rev,min_rev,min_eff,unit_perc,unit_head,unit_rev,
     *            unit_flow,unit_eff_corr,unit_eff,unit_power_corr,
cgpe     *            unit_power,unit_hrate,unit_cons,unit_temp,
     *            unit_power,unit_hrate_corr,unit_hrate,
     *            unit_cons,unit_temp,
c-mf03
c     *            unit_max,stat_varsp,ier)
     *            unit_max,stat_varsp,unit_vinc,ier)
c-mf03-end
c
c***********************************************************************
c	simulazione stazione a compressori multistadio
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/td.inc'
      include '../inc/cc.inc'
      include '../inc/th.inc ' ! :in
      include '../inc/tj.inc ' ! :in/out
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/compstat.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end

cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat

c
      COMMON/MAR/istaz
      integer*4 istaz
      integer*4 vert_pointer,type_quant(*),unit_num,ier,
     *          stat_ord
      real*4    nom_flow(*)
      real*4    pres1,pres2,temp1,temp2,delpr1,delpr2,delp_int,
     *          pres_int,temp_int,tot_cons,comp_ratio,flowm,flow_ric,
     *          PRES1_RIC,PRES2_RIC,max_rev(*),min_rev(*),min_eff(*), 
     *          unit_perc(*),unit_head(*),unit_rev(*),unit_flow(*),
     *          unit_eff_corr(*),unit_eff(*),unit_power_corr(*),
     *          unit_power(*),unit_hrate(*),unit_cons(*),unit_temp(*),
     *          unit_max(*),stat_varsp(*),delt_int,
     *          unit_hrate_corr(*)
     *         ,delpr_ac_int
     *         ,tout_ac
c-mf03
      integer*4 unit_vinc(*)
c-mf03-end
c
      real*4    max_tint,aux3_int
      real*4    tot_power,un_flow,un_perc,flow_min,un_eff,qf,un_power,
     *          un_hrate,dpw,un_cons,corr
      integer*4 irange,k,j,i2
      LOGICAL*2 lhr,status(*)
c
      real*4    qq,exp_pol,esp2,aux1
      real*4    flow_ass,flow_comp,flow_eff
      real*4    head
      real*4    diff
      real*4    eps_q/1.e-3/
      real*4    pres_old,flow_old
c
      INTEGER*4 i,ker,nutot,itip,imess,ntp1
      INTEGER*4 ivinc
      INTEGER*4 irmax
      INTEGER*4 itip_max
      real*4    rev
      real*4    old_aux3
c
      real*4    zero_pow,prop
      EXTERNAL  zero_pow,prop

      real*4    ZERO_LIM_CH,ZERO_REV2
      EXTERNAL  ZERO_LIM_CH,ZERO_REV2

      real*4    eps_p/0.05/
      real*4    eesp
      real*4    uno_su_ro_attuale
      INTEGER*4 iw
      real*4    qaux,haux
      integer*4 type_act_old(maxunits)
      real*4    hlim(4),flow_lim(4),qlim(maxunits),rev_lim(2),
     *          clim_eff(maxunits)
      real*4    ra,fa,rb,fb,rd
      real*4    FLOW_APP
      real*4    zero_pin
      external  zero_pin
      character*(max_len) mess/' '/
      real*4    qq1,f_aux
      real*4    hmax
      real*4    tot_flow
	real*4    delpr_f,delpr_ac
      real*4    rdummy
      integer*4 idummy
      integer*4 ij,icj,nu
      LOGICAL*2 l_ric
c-mf03
	integer*4 ind_vio_vinc
      external  ind_vio_vinc
c-mf03-end
c-ace18
      real*4 ro_in,ro_out,ro_int
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end
    
      real*4 comp_ratio2
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


      rdummy = min_eff(1)   ! serve per evitare %FORT-I-VARUNUSED
      rdummy = unit_max(1)  ! serve per evitare %FORT-I-VARUNUSED
      idummy = unit_num     ! serve per evitare %FORT-I-VARUNUSED
c
      ier = 0
      ker = 0
c
      itipo = 1
      itipo_coup = 2
c
cgpe      if (flow.le.0.) then
cgpe        ier = 4
cgpe!        if (iopt) return
cgpe        if (iopt .or. iopt_mess) return
cgpec flow negativo ==> imposizione di pout=pin e stato=OFF.
cgpe        pres2 = pres1
cgpe        temp2 = temp1
cgpe        call imp_new_sta(vert_pointer,pres2,'FR')
cgpe        FLOW_APP=FLOW*CONV_NS
cgpe        write(mess,555) flow_APP
cgpe555     format('Q richiesta ',f9.3)
cgpe        call app_mess(vert_pointer,ier,mess)
cgpe        return
cgpe      end if
c---->            Calcolo perdite di pressione all'ingresso e all'uscita
cgpe      qq = flow*flow
      f_aux=flow/type_actr(itipo) !divido la portata lavorata per il numero di
      qq1=f_aux*f_aux             !TC attivi(in qto caso unico tipo)
cgpe      delpr1 = stat_varsp(3)*qq !caduta di pressione per il piping in ingresso
cgpe      delpr2 = stat_varsp(4)*qq !caduta di pressione dovuta al piping in uscita
      delp_int = stat_varsp(8)*qq1 !caduta di pressione per il piping intermedio
      delt_int = stat_varsp(9)
      max_tint = stat_varsp(10)
c---->                        calcolo variabili in ingresso alla stazione
c tin,pin e pout sono in compstat.INC
      tin = temp1
cgpe      if(pin.le.0.or.pout.le.0) then
cgpec pressioni alla stazione negative ==> imposizione di pout=pin e stato=OFF.
cgpe        ier = 19
cgpe!        if (iopt) return
cgpe        if (iopt .or. iopt_mess) return
cgpe        temp2 = temp1
cgpe        pres2 = pres1
cgpe        call imp_new_sta(vert_pointer,pres2,'FR')
cgpe        write(mess,556) pin,pout
cgpe556     format('P asp: ',f7.2,' - P man: ',
cgpe     *          f7.2)
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
      pin = pin - delpr_f

c              CADUTA DI PRESSIONE DOVUTA ALL'AIR COOLER
      call delpr_ac_s (ntp,type_quant,status,nom_flow,
     *                 ac_ce_k,ac2_k,flow,delpr_ac)
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
CMAR
      comp_ratio = pout/pin
      call politrop_esp(istaz,pin,temp1,comp_ratio,exp_pol,ier)

CMAR      exp_pol = prop(pin,temp1,exp_coef(1,ind))
c esp1 e' nel common compstat.INC
      esp1 = (exp_pol-1.)/exp_pol
CMAR      comp_ratio = pout/pin
c zin e' nel common compstat.INC

CMAR      zin = prop(pin,temp1,z_coef(1,vert_pointer))
      call zpt1 (istaz,pin,temp1, zin)
      aux1 = (erre / agrvcc / pmol) * Zin * temp1/esp1
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*temp1/pin
      aux3 = ROCN*uno_su_ro_attuale
      old_aux3 = aux3
      flowm = flow*aux3
      flow_tot = flow*aux3
      flow_ass = 0.
      imess = 0
c---->             simulazione dei compressori  (calcolo di un_rev)
c---->                                         compressori in serie 
c---->                                   calcolo altezza adiabatica
      ivinc = 0
      aux2 = comp_ratio**esp1
      head = aux1 * (aux2-1.)                        ! in km
      do i=1,ntp
        type_act_old(i)=type_actr(i)
      enddo
c---->                                calcolo fattori correttivi
      call limit(ntp,type_actr,type_ratio,max_rev,
     *		min_rev,rev_lim,itip_max,irmax)
      if(irmax.gt.0) ivinc = 1
      ntp1 = 1
      call surge_eff(ntp1,type_actr,type_ratio,head,
     *               max_rev,min_rev,qlim,IVINC,IRMAX,
     *               ITIP_MAX)
c---->             compute the station or compressor extremes
      call fact_lim(itip_max,hlim,flow_lim,clim_eff,rev_lim,max_rev,
     *                min_rev,irmax,ivinc)
      flow_comp = flow_tot
      flow_eff  = flow
      call compr2(vert_pointer,flow_comp,hlim,flow_lim,rev_lim,diff,IER)
c ier=17===> flow_tot>flow_lim(2)
c ier=6 ===> la pressione massima che si ottiene e' minore di quella richiesta
c            e allo stesso tempo flow_tot>flow_lim(1):la macchina non puo' 
c            lavorare a quella pressione, quindi deve essere abbassata la
c            pout (oppure la pin: come fare ???)
c ier=7 ===> la pressione minima che si ottiene e' maggiore di quella richiesta
C            quindi deve essere aumentata la pout (oppure diminuita la pin)
c ier=17 ===> la pressione massima che si ottiene e' minore di quella richiesta
c            e allo stesso tempo,pero', flow_tot<flow_lim(1):
c            la macchina non puo' lavorare a quella pressione, ma posso ancora 
c            provare  ad aggiungere macchine
      flow_ric = flow_comp/old_aux3
      flow = flow_eff
      flow_tot = flow*old_aux3
      ker = ier
!-22/08/2000
!      if (ier.gt.0 .and. (iopt .or. iopt_mess)) return
!-22/08/2000-end
      if(ier.ne.0) call recover_compr2(ind,un_rev,flow_comp,
     *			flow_lim,rev_lim,flow_ric,pres1_ric,pres2_ric,
     *                  stat_varsp,pin,pout,temp1,IER)
cgpe-corr
cgpe-corr      if (ier.eq.6) then
cgpe-corr        if (htcitj(ind).eq.tc_pm) then
cgpe-corrc set del common necessario alla zero_pin
cgpe-corr          giri_lim(1)=rev_lim(2)
cgpe-corr          port_lim(1)=flow_lim(4)
c
cgpe-corr          un_flow1 = flow_lim(1)/type_actr(itipo)
cgpe-corr          pres_old = pres1
cgpe-corr          aux3     = old_aux3
cgpe-corr          ra       = pin
cgpe-corr          fa       = zero_pin(ra)
cgpe-corr          rb       = pout
cgpe-corr          fb       = zero_pin(ra)
cgpe-corr          rd       = ra
cgpe-corr          call zero_fun(ra,rb,rd,fa,fb,pin,epsmi,pmach,zero_pin)
cgpe-corr          pin=pin+ eps_p
cgpe-corr          pres1 = pin+delpr1
cgpe-corrcgpe          call imp_new_sta(vert_pointer,pres1,'PM')
cgpe-corr          call imp_new_sta(ind,vert_pointer,pres1,'PM')
cgpe-corr          write(mess,557) pres_old,pres1
cgpe-corr557       format('P asp richiesta: ',f7.2,' - P asp proposta:',f7.2)
cgpe-corrcgpe          iw  =  51
cgpe-corr          iw  =  41
cgpe-corr          call app_mess(vert_pointer,iw,mess)
cgpe-corr        ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ .or.
cgpe-corr     *                 tipo_corr.eq.1) then
cgpe-corr          pres_old = pres2
cgpe-corr          eesp=1/esp1
cgpe-corr          pout=pout+diff-eps_p
cgpe-corr          pres2 = pout-delpr2
cgpe-corr          if (pres2.gt.stat_vars(6)) then
cgpe-corr            ier = 9
cgpe-corr            write(mess,558) pres2,stat_vars(6)
cgpe-corr558         format('P man: ',f7.2,' - P man max: ',f7.2)
cgpe-corr            call app_mess(vert_pointer,ier,mess)
cgpe-corr            ier = 1
cgpe-corr            return
cgpe-corr          endif
cgpe-corrcgpe          call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr          call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr          write(mess,559) pres_old,pres2
cgpe-corr559       format('P man: richiesta=',f7.2,'  -  proposta=',
cgpe-corr     *          f7.2)
cgpe-corrcgpe          iw  =  52
cgpe-corr          iw  =  42
cgpe-corr          call app_mess(vert_pointer,iw,mess)
cgpe-corr        ELSE
cgpe-corrC IL SET E' IN PORTATA. CHE FARE ???
cgpe-corr          IER = 31
cgpe-corr          mess = ' '          
cgpe-corr          call app_mess(vert_pointer,ier,mess)
cgpe-corr          IER = 1
cgpe-corr        endif
cgpe-corr      else if (ier.eq.7) then
cgpe-corr        if (htcitj(ind).eq.tc_pm) then
cgpe-corrc gz - 1-12-91 ***** in questo caso devo trovare la condizione di funzionamento
cgpe-corrc                    minima per la sola prima fase, e non per entrambe le fasi 
cgpe-corrc                    come ho fatto nella parte commentata
cgpe-corrCCCc set del common necessario alla zero_pin
cgpe-corrCCC          giri_lim(1)=rev_lim(1)
cgpe-corrCCC          port_lim(1)=flow_lim(4)
cgpe-corrCCCc
cgpe-corrCCC          pres_old = pres1
cgpe-corrCCC          aux3     = old_aux3
cgpe-corrCCC          un_flow1 = flow_lim(4)/type_actr(itipo)
cgpe-corrCCC          ra       = pin/10.
cgpe-corrCCC          fa       = zero_pin(ra)
cgpe-corrCCC          rb       = pin
cgpe-corrCCC          fb       = zero_pin(ra)
cgpe-corrCCC          rd       = ra
cgpe-corrCCC          call zero_fun(ra,rb,rd,fa,fb,pin,epsmi,pmach,zero_pin)
cgpe-corrCCC          pin=pin - eps_p
cgpe-corrCCC          pres1 = pin+delpr1
cgpe-corr          pres_old = pres1
cgpe-corr          itipo = 1
cgpe-corr          call HDAN_CH(clim(4,itipo),haux,min_rev(itipo),chc(1,itipo))
cgpe-corr          call trans(pout,temp1,haux,qaux,pin,qaux,1,ind,vert_pointer)
cgpe-corr          pin=pin - eps_p
cgpe-corr          pres1 = pin+delpr1
cgpe-corrcgpe          call imp_new_sta(vert_pointer,pres1,'PM')
cgpe-corr          call imp_new_sta(ind,vert_pointer,pres1,'PM')
cgpe-corr          write(mess,562) pres_old,pres1
cgpe-corr562       format('P asp: richiesta=',f7.2,' - proposta=',
cgpe-corr     *          f7.2)
cgpe-corrcgpe          iw  =  51
cgpe-corr          iw  =  41
cgpe-corr          call app_mess(vert_pointer,iw,mess)
cgpe-corr        ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ .or.
cgpe-corr     *                 tipo_corr.eq.1) then
cgpe-corr          pres_old = pres2
cgpe-corr          eesp=1/esp1
cgpe-corr          pout=pout+diff+eps_p
cgpe-corr          pres2 = pout-delpr2
cgpe-corr          if (pres2.gt.stat_vars(6)) then
cgpe-corr            ier = 9
cgpe-corr            write(mess,563) pres2,stat_vars(6)
cgpe-corr563         format('P man: richiesta=',f7.2,' - massima=',
cgpe-corr     *          f7.2)
cgpe-corr            call app_mess(vert_pointer,ier,mess)
cgpe-corr            ier = 1
cgpe-corr            return
cgpe-corr          endif
cgpe-corrcgpe          call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr          call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr          write(mess,559) pres_old,pres2
cgpe-corrcgpe          iw  =  52
cgpe-corr          iw  =  42
cgpe-corr          call app_mess(vert_pointer,iw,mess)
cgpe-corr        ELSE
cgpe-corrC IL SET E' IN PORTATA. CHE FARE ???
cgpe-corr          IER = 31
cgpe-corr          mess = ' '          
cgpe-corr          call app_mess(vert_pointer,ier,mess)
cgpe-corr          IER = 1
cgpe-corr        endif
cgpe-corr      endif
      if (ier.eq.6 .or. ier.eq.7) then
c        ier = 71
        ier = 52
        if (iopt .or. iopt_mess) return
        if (fl_sim) then
          mess = ' '
          call app_mess(vert_pointer,ier,mess)
        endif
      endif
cgpe-corr-end

      if(ier.ne.0) return

!      if(flow_comp.gt.flow_tot) then
      if ((flow_comp-flow_tot) .gt. eps_q) then
c---->                      !!!!!! warning  compressore in riciclo 
!!        if (.not.iopt) then
!        if (.not.iopt .and. .not.iopt_mess) then
!cgpe          iw = 59
!          iw = 49
!          write(mess,560) flow_tot,flow_comp
!560     format(' Q di macchina: richiesta=',f8.3,'  -  lavorata=',
!     *          f8.3)
!          call app_mess(vert_pointer,iw,mess)
!        end if
        l_ric = .true.
cgpe-riciclo
cgpe-riciclo        if (un_rev .eq. min_rev(1)) l_ric = .false.
        if (.not. fl_ric_rev) then
          if (un_rev .eq. min_rev(1)) l_ric = .false.
cgpe-riciclo
        endif
cgpe-riciclo-end
cgpe-new        if (.not.iopt .and. .not.iopt_mess) then
cgpe-new          if (l_ric) then
cgpe-newc la centrale e' in riciclo
cgpe-new            call gest_error (3,0,'MULTI_STADIO',
cgpe-new     *           'La centrale e'' in riciclo',0)
cgpe-new          else
cgpe-new            call gest_error (2,0,'MULTI_STADIO',
cgpe-new     *           'Portata inferiore alla minima',0)
cgpe-new          end if
cgpe-new        end if

        if (iopt) then
          ier = 0
          if (.not.l_ric) ier = 20
          return
        endif

c-l_ric        if (.not.iopt_mess) then
          if (l_ric) then
c-bpcorr
cgpe-mess            call gest_error (2,0,'','La centrale e'' in riciclo',0)
cgpe-mess            write(mess,560) (flow_comp*conv_ns)/old_aux3,(flow_tot*conv_ns)/old_aux3
c-UDM            write(mess,560) flow_comp/old_aux3,flow_tot/old_aux3,UDM_INT_Q
           if (.not.iopt_mess) then
            iw = 27
            write(mess,560) udm_out_str(udm_Q,flow_comp/old_aux3,0),
     *                      udm_out_str(udm_Q,flow_tot/old_aux3,1)
            call app_mess(vert_pointer,iw,mess)
           endif
		  ier = 0
c-bpcorr-end
          else
           if (.not.iopt_mess) then
            iw = 20
cgpe-mess            write(mess,560) (flow_comp*conv_ns)/old_aux3,(flow_tot*conv_ns)/old_aux3
c-UDM            write(mess,560) flow_comp/old_aux3,flow_tot/old_aux3,UDM_INT_Q
            write(mess,560) udm_out_str(udm_Q,flow_comp/old_aux3,0),
     *                      udm_out_str(udm_Q,flow_tot/old_aux3,1)
            call app_mess(vert_pointer,iw,mess)
           endif
          endif
!          write(mess,560) (flow_tot*conv_ns)/old_aux3,
!     *                    (flow_comp*conv_ns)/old_aux3
cgpe-new          if (l_ric) ier = 0
c-bpcorr          if (l_ric) then
c-bpcorr		  ier = 0
c-bpcorr            call gest_error (2,0,'',
c-bpcorr     *          'La centrale e'' in riciclo',0)
c-bpcorr            write(mess,560) (flow_comp*conv_ns)/old_aux3,
c-bpcorr     *                      (flow_tot*conv_ns)/old_aux3
c-bpcorr            call app_mess(vert_pointer,iw,mess)
cgpe          else
cgpe            flow_ric = 0.
c-bpcorr	    endif
cgpe-new-end
cgpe-mess560       format(' Q lavorata: ',f7.2,' - Q richiesta: ',f7.2)
c-UDM560       format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
560       format('Q lavorata: ',A10,' - Q richiesta: ',A25)
c-l_ric        end if
cgpe-new
cgpe      else
cgpe	  flow_ric = 0.
cgpe-new-end
      end if
      if(ker.gt.1) imess = 1
c---->                       calcolo efficienze e potenze richieste
20    tot_cons = 0.
      tot_power = 0.
cgpe
      tot_flow = 0.
cgpe-end
      nutot = 0
      do itip=1,2
        rev = un_rev*type_ratio(itip)
        if(type_actr(itip).gt.0) then

cmar_pow_stat
ccccccccc_312      min_p=min_p+type_vinc_minpow(itip)	
ccccccccc_312      max_p=max_p+type_vinc_maxpow(itip)  
cmar_pow_stat
          if ((stat_ord.eq.f_serie).and.(itip.eq.itipo)) then
c---->                           primo stadio compressori in serie
            flow_eff = flow_comp/old_aux3
            un_flow = flow_comp/type_actr(itip)
c---->                     !!!!!! warning  compressore oltre l'anti-choke
            call QDAN_CH(clim(3,itip),qaux,rev,chn(1,itip))
            if (un_flow.le.qaux) then
              call hdaqn(head,un_flow,rev,chn(1,itip))
            else
              call hdaqn(head,un_flow,rev,chc(1,itip))
            endif
c
            rb=un_flow*type_actr(itip)
            diff = zero_lim_ch(rb)
            if(diff.gt.0) then
c il punto di lavoro e' oltre la curva di choking
c calcolo la portata della macchina sulla choking e riprovo:
!!!              ier = 17
!!!              if (iopt) return
              ra = flow_lim(4)
              fa = zero_lim_ch(ra)
              rb = flow_lim(2)
              rd = ra
              fb = zero_lim_ch(rb)
              flow_old = un_flow*type_actr(itip)
              call zero_fun(ra,rb,rd,fa,fb,flow_comp,
     *                  epsmi,pmach,zero_lim_ch)
              flow_comp = flow_comp - flow_comp*eps_q
              un_flow = flow_comp/type_actr(itip)
              flow_ric = flow_comp/old_aux3
              call recover_compr2(ind,un_rev,flow_comp,
     *			flow_lim,rev_lim,flow_ric,pres1_ric,pres2_ric,
     *                  stat_varsp,pin,pout,temp1,IER)
              ier = 17
!              write(mess,561) flow_old,flow_comp 
!561           format(' Q di macchina: richiesta=',f8.3,
!     *          '  -  massima=',f8.3)
!              call app_mess(vert_pointer,ier,mess)
              return
            end if
            un_perc = 1./type_actr(itip)
          else if ((stat_ord.eq.f_serie).and.(itip.eq.itipo_coup)) then
c---->                        secondo stadio compressori in serie
c                                        calcolo valori intermedi
            temp2 = 0.
            aux2 = head/aux1+1.
            pres_int = pin*aux2**(1./esp1)
            ntp1 = 1
c-ace       eff_coef = 0
            call out_temper_int(
     *                  tair,temp2,temp1,Zin,pin,pres_int,aux2,
     *                  z_coef(1,vert_pointer),ntp1,type_actr,
     *                  type_quant,
c-ace*                  eff_coef,
     *                  dh_coef(1,ind),cp_par(1,ind),
     *                  unit_perc,unit_eff,
     *                  un_flow,
     *                  unit_temp,
c-ele
     *                  delt_int,max_tint)

            temp_int=temp2
c_ecol----------------------------------------------------------------------------
c     Calcolo della caduta di pressione associata all'air cooler intermedio:
c     tale ac è sempre presente e sempre attivo;dato che pres_int e delp_int sono
c     valori unici validi per tutti i TG attivi nella centrale, per ottenere un 
c     unico valore calcolo la costante k come media e la moltiplico poi per la 
c     portata qq1 lavorata dal singolo TG (qq1 è uguale per tutti visto che nella
c     centrale ho un solo tipo di unità).Tale costante unica è definita nel common
c     SIMPSTAT e valorizzata dalla subroutine INIT_MULTI_STADIO
c_ecol----------------------------------------------------------------------------
c-ace18
            call ro_real(ind,pres_int,temp_int,ro_int)

	      delp_int=delp_int/ro_int
            delpr_ac_int=k_int*qq1/ro_int

c_ecol
c_ecol            pres_int = pres_int-delp_int
            pres_int = pres_int-delp_int-delpr_ac_int
c_ecol
c            temp_int = temp2-delt_int

            if(temp_int.gt.max_tint) then
	        ker=80
	        if (iopt.or.iopt_mess) return
cgpe-mess              if (fl_sim) then
cgpe-mess                call gest_error (2,0,'','Superata la massima temperatura intermedia',0)
cgpe-mess              endif
c-UDM	        write (mess,565) temp_int,max_tint,UDM_INT_T ! write (mess,565) temp_int-t0,max_tint-t0
c-UDM565           format('T int: ',f5.2,' - T int max: ',f5.2,'  ',A3)
              write(mess,565) udm_out_str(udm_T,temp_int,0),
     *                        udm_out_str(udm_T,max_tint,1)
565     format('T int: ',A10,' - T int max: ',A25)
              call app_mess(vert_pointer,ker,mess)
	      end if
CMAR   
      comp_ratio2=pres2/pres_int
	call politrop_esp (istaz,pres_int,temp_int,comp_ratio2,exp_pol,ier)

CMAR            exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
            esp2 = (exp_pol-1.)/exp_pol
CMAR            z = prop(pres_int,temp_int,z_coef(1,vert_pointer))
            call zpt1 (istaz,pres_int,temp_int,z)
            uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
            aux3_int = ROCN*uno_su_ro_attuale
            un_flow=flow_eff*aux3_int/type_actr(itip)
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *                  nlim(itip),lim_n(1,itip),rev,haux,flow_min,
     *                  irange)
            if(un_flow.gt.flow_min) then
              call QDAN_CH(clim(3,itip),qaux,rev,chn(1,itip))
              if (un_flow.le.qaux) then
                call hdaqn(head,un_flow,rev,chn(1,itip))
              else
                call hdaqn(head,un_flow,rev,chc(1,itip))
              endif
              if(head.le.0.) then
                head = 0.
cgpe                iw = 58
                iw = 48
!                if (.not.iopt) then
                if (.not.iopt .and. .not.iopt_mess) then
                  mess = ' '
                  call app_mess(vert_pointer,iw,mess)
                end if
              end if
            else
c!!!!!!!!!!!!!!!!!
              hmax = haux
              head = hmax*(1.-0.05*(un_flow-flow_min)/flow_min)
c!!!!!!!!!!!!!!!!!
            end if
            un_perc = 1./type_actr(itipo_coup)
c ??            aux2 = head*esp2/(head0*z*temp_int)+1.
            if(imess.gt.0) then
c---->                 !!!!!! warning  modificata pressione di mandata
c                                                 ricalcolo della pout
c ??	        pout = pres_int*aux2**(1./esp2)
c ??	        comp_ratio = pout/pin
c ??	        pres2 = pout-delp2
c ??	        imess = 0
            end if
          end if
c---->                 assegnazione risultato alle unita' della serie
          do k=1,type_quant(itip)
            j = nutot+k
            if (status(j)) then


c---->                                  Calcolo efficienza e potenza
              call effdaqn(un_eff,un_flow,rev,cen(1,itip),cec(1,itip),
     *               clim(1,itip))
              un_eff = un_eff*unit_eff_corr(j)
cgpe
              if (un_eff .lt. 0.01) un_eff = 0.01
cgpe-end
              if ((stat_ord.eq.f_serie).and.(itip.eq.itipo)) then
                qf = un_flow/old_aux3
              else
                qf = un_flow/aux3_int
              endif
c---->          power required

cgpe              un_power= AGRVcc * rocn * head*qf/
cgpe     *                        (un_eff*unit_power_corr(j))
              un_power= AGRVcc * rocn * head*qf/un_eff
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
              un_power=un_power/(3.6*1.e3)
              unit_head(j)  = head
              unit_perc(j)  = un_perc
              unit_flow(j)  = un_flow
cgpe
              if ((stat_ord.eq.f_serie).and.(itip.eq.itipo_coup)) then
                tot_flow = tot_flow + unit_flow(j)
              endif
cgpe-end
              unit_rev(j)   = rev
              unit_eff(j)   = un_eff
              unit_power(j) = un_power
            endif
          end do
cgpe ?????????
cgpe forse dovrebbe essere messo insieme al calcolo di tot_flow ????
cgpe forse la spiegazione è:
cgpe   tot_power = somma(unit_power) + somma(unit2_power) che nel caso del vecchio
cgpe   criterio somma(unit_power(per_itipo=1)) + somma(unit2_power(per_itipo_coup=2))
cgpe da passare alla subroutine turb
          tot_power = tot_power+un_power
cgpe ?????????
        end if
        nutot = nutot+type_quant(itip)
      end do
c---->                                 calcolo della o delle turbine
c      if(cpwm(1,2).eq.0.) then
      if (una_turb) then
c---->                              unica turbina con due compressori
        itipo = 1
        nutot = 0
        do k=1,type_quant(itipo)
          j = nutot+k
          if (status(j)) then
            rev = unit_rev(j)
          endif
        enddo
        lhr=.true.
cgpe        call turb(rev,tot_power,un_hrate,tair,
        call turb(itipo,rev,unit_power_corr(1),tot_power,
     *            unit_hrate_corr(1),un_hrate,tair,
     *            cpwm(1,itipo),cpwt(1,itipo),chr(1,itipo),
     *            chrt(1,itipo),dpw,delta,lhr,ier)
        ker = ier
        if(ier.gt.0) then
!          if (iopt) return
          if (iopt .or. iopt_mess) return
          itipo = 1
c---->                           recover per collegamento in serie
          un_flow1=flow_comp/type_actr(itipo)
cgpe-corr
cgpe-corr          if (htcitj(ind).eq.tc_pm) then
cgpe-corrc correzione in pressione
cgpe-corr            ier = 1
cgpe-corr            return
cgpe-corr          ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ ) then
cgpe-corrc     *                 .or.tipo_corr.eq.1) then
cgpe-corrc correzione in pressione
cgpe-corr            call recover_turbs_p(vert_pointer,flow_lim,rev_lim,
cgpe-corr     *                       flow_comp,ier)
cgpe-corrc diff
cgpe-corr            pres_old = pres2
cgpe-corr            diff=zero_rev2(un_rev)
cgpe-corr            eesp=1/esp1
cgpe-corr            pout=pout+diff-eps_p
cgpe-corr            pres2 = pout-delpr2
cgpe-corrcgpe            call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr            call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr            write(mess,559) pres_old,pres2
cgpe-corrcgpe            iw  =  52
cgpe-corr            iw  =  42
cgpe-corr            call app_mess(vert_pointer,iw,mess)
cgpe-corr            return
cgpe-corr          ELSE
cgpe-corrc correzione in portata
cgpe-corr            call recover_turbs_q(vert_pointer,flow_lim,rev_lim,
cgpe-corr     *                       flow_comp,ier)
cgpe-corr            flow_comp = flow_comp - flow_comp*eps_q
cgpe-corr            flow_ric = flow_comp/old_aux3
cgpe-corr            call recover_compr2(ind,un_rev,flow_comp,
cgpe-corr     *		     flow_lim,rev_lim,flow_ric,pres1_ric,pres2_ric,
cgpe-corr     *               stat_varsp,pin,pout,temp1,IER)
cgpe-corr            return
cgpe-corr          endif
          ier = 1
          return
cgpe-corr-end
        end if
c---->                                        consumo dell'unita' 
cgpe        un_cons = un_hrate*tot_power/pclitd(ind)/4186.
c pclitd è stato modificato da kcal/Nm3 a kJ/kNm3
        un_cons = un_hrate*(tot_power/unit_power_corr(1))/
     *            pclitd(ind)


	
cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)
cmar_pow_stat



c-mf03
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
        ier = ind_vio_vinc(itipo,tot_power,rev)
        if (fl_vinc .and. ier.gt.0) then
          ier = 1
          return
        else
c              un_cons = un_cons + soglia_cons
          if (ier .gt. 0 .and. iopt) then
            un_cons = un_cons + soglia_cons
          endif
          if (ier .gt. 0) unit_vinc(itipo) = ier
          ier = 0
        endif
c-mf03-end
c---->           assegnazione risultato a tutte le turbine uguali
        nutot = 0
        do k=1,type_quant(itipo)
          j = nutot+k
          if (status(j)) then
            unit_hrate(j) = un_hrate
            corr = 1
            unit_power(j) = unit_power(j)*corr
            unit_cons(j) = un_cons*corr
cgpe-corr
            unit_vinc(j) = unit_vinc(itipo)
cgpe-corr-end

          endif
        end do
        tot_cons= tot_cons + un_cons*type_actr(itipo)
      else
c---->                             ogni turbina ha un compressore
        nutot = 0
        do itipo=1,2
          if(type_actr(itipo).gt.0) then
            do k=1,type_quant(itipo)
              j = nutot+k
              if (status(j)) then
                rev = unit_rev(j)
                un_power = unit_power(j)
              endif
            enddo
            lhr=.true.
cgpe            call turb(rev,un_power,un_hrate,tair,
            call turb(itipo,rev,power_corr(itipo),un_power,
     *           hrate_corr(itipo),un_hrate,tair,
     *           cpwm(1,itipo),cpwt(1,itipo),chr(1,itipo),
     *           chrt(1,itipo),dpw,delta,lhr,ier)
            ker = ier
            if(ier.gt.0) then
!              if (iopt) return
              if (iopt .or. iopt_mess) return
c---->                                   recover stazioni in serie
              un_flow1=flow_comp/type_actr(itipo)
cgpe-corr
cgpe-corr              if (htcitj(ind).eq.tc_pm) then
cgpe-corrc correzione in pressione
cgpe-corr                ier = 1
cgpe-corr                return
cgpe-corr              ELSE if (htcitj(ind).eq.tc_pV .OR.
cgpe-corr     *                 htcitj(ind).eq.tc_pQ ) then
cgpe-corrc     *                 .or.tipo_corr.eq.1) then
cgpe-corrc correzione in pressione
cgpe-corr                call recover_turbs_p(vert_pointer,flow_lim,rev_lim,
cgpe-corr     *                       flow_comp,ier)
cgpe-corrc diff
cgpe-corr                pres_old = pres2
cgpe-corr                diff=zero_rev2(un_rev)
cgpe-corr                eesp=1/esp1
cgpe-corr                pout=pout+diff-eps_p
cgpe-corr                pres2 = pout-delpr2
cgpe-corrcgpe                call imp_new_sta(vert_pointer,pres2,'PV')
cgpe-corr                call imp_new_sta(ind,vert_pointer,pres2,'PV')
cgpe-corr                write(mess,559) pres_old,pres2
cgpe-corrcgpe                iw  =  52
cgpe-corr                iw  =  42
cgpe-corr                call app_mess(vert_pointer,iw,mess)
cgpe-corr                return
cgpe-corr              ELSE
cgpe-corrc correzione in portata
cgpe-corr                call recover_turbs_q(vert_pointer,flow_lim,rev_lim,
cgpe-corr     *                       flow_comp,ier)
cgpe-corr                flow_comp = flow_comp - flow_comp*eps_q
cgpe-corr                flow_ric = flow_comp/old_aux3
cgpe-corr                call recover_compr2(ind,un_rev,flow_comp,
cgpe-corr     *		     flow_lim,rev_lim,flow_ric,pres1_ric,pres2_ric,
cgpe-corr     *               stat_varsp,pin,pout,temp1,IER)
cgpe-corr                return
cgpe-corr              endif
              ier = 1
              return
cgpe-corr-end
            end if
c---->                                        consumo dell'unita'
cgpe            un_cons = un_hrate*un_power/pclitd(ind)/4186.
c pclitd è stato modificato da kcal/Nm3 a kJ/kNm3
            un_cons = un_hrate*(un_power/power_corr(itipo))/
     *                pclitd(ind)
c-mf03
c check se il punto di lavoro trovato viola un limite operativo in potenza o giri
            ier = ind_vio_vinc(itipo,un_power,rev)
            if (fl_vinc .and. ier.gt.0) then
              ier = 1
              return
            else
c              un_cons = un_cons + soglia_cons
              if (ier .gt. 0 .and. iopt) then
                un_cons = un_cons + soglia_cons
              endif
              if (ier .gt. 0) unit_vinc(itipo) = ier
              ier = 0
            endif
c-mf03-end
c---->           assegnazione risultato a tutte le turbine uguali
            do k=1,type_quant(itipo)
              j = nutot+k
              if (status(j) ) then
                unit_hrate(j) = un_hrate
                corr = 1
                unit_power(j) = unit_power(j)*corr
                unit_cons(j) = un_cons*corr
cgpe-corr
                unit_vinc(j) = unit_vinc(itipo)
cgpe-corr-end
              endif
            end do
            tot_cons= tot_cons + un_cons*type_actr(itipo)
          end if
          nutot = nutot+type_quant(itipo)
        end do
      end if
c---->                         calcolo della temperatura di uscita
      temp2 = 0.
c---->                                       collegamento in serie
      itipo = 1
      i2 = type_quant(itipo)+1
      call out_temper2(
c_ecol
     *                  tair,2,
c_ecol
     *                  temp2,temp_int,Z,pres_int,pout,aux2,
     *                  z_coef(1,vert_pointer),ntp1,
     *                  type_actr,type_quant,
c-ace*                  eff_coef,
     *                  dh_coef(1,ind),cp_par(1,ind),
c-ace
     *                  unit_perc(i2),unit_eff(i2),
c_ecol
     *                  unit_flow(i2),
     *                  stat_varsp(7),
     *                  unit_temp(i2))
c-------->modifica relativa al delta dell'ac
c
c ---> sottraggo la caduta di temperatura dovuta al piping

      temp2 =temp2 -stat_varsp(5)
	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_varsp(7)
     *                            ,pres1,temp1,temp2)  !new

c_ecol 23/9/2004---------------------------------------------------------------
c  In caso di superamento della temperatura massima di mandata (=stat_vars(7))
c  non sarà effettuato un taglio ma sarà semplicemente visualizzato un messaggio
c  di warning
cmar_AC_auto
      IF (.not.FLAG_AC)THEN
cmar_AC_auto
c_ecol
        if (temp2.gt.stat_varsp(7)) then
	     ker=79
	     if (iopt.or.iopt_mess) return
cgpe	     write (mess,570) temp2-t0,stat_varsp(7)-t0
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

c---->                                   air cooler test
c_ecol      temp2 = temp2 - stat_varsp(5)
c
c----->             prima di uscire, si riportano i power di unita' alle
c                   condizioni effettive
c correzione gz --- 20-luglio-1993
c      do i=1,unit_num
c        if(status(i)) then
c          unit_power(i)=unit_power(i)*unit_power_corr(i)
c        end if
c      end do
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

      Subroutine compr2(vert_pointer,flow_comp,hlim,flow_lim,rev_lim,
     *                  diff,IER) 
c
c***********************************************************************
c	calcola i punti di lavoro di due compressori in serie
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
c
      integer*4 vert_pointer,irange
      real*4    zero_rev2,zero_flow,zero_lim2
      EXTERNAL  zero_rev2,zero_flow,zero_lim2
      real*4    flow_comp,rev_lim(*),flow_lim(*),hlim(*)
      real*4    rev0,rev_max,rev_min,rev
      real*4    fb,ra,fa,rb,rd,flow_aux,diff
      real*4    HAUX,old_un_flow1,old_flow
      real*4    raux,qh,diff_aux
      integer*4 ier
      real*4   eps_q/0.001/
      character*(max_len) mess/' '/
c
      real*4rdummy
c-UDM
      character*(max_len_str) udm_out_str
      external                udm_out_str
c-UDM-end
c---------------------------------------------------------------------
      rdummy = hlim(1)  ! serve per evitare %FORT-I-VARUNUSED
c
      rev0 = un_rev
      un_flow1 = flow_tot/type_actr(itipo)
      if(flow_tot.gt.flow_lim(2)) then
        ier = 17
!        if (iopt) return
        if (iopt .or. iopt_mess) return
!        write(mess,555) flow_tot,flow_lim(2)
!555     format(' Q di macchina: richiesta=',f8.3,'  -  massima=',
!     *          f8.3)
!        call app_mess(vert_pointer,ier,mess)
        flow_comp = flow_lim(2)
        flow_comp=flow_comp-flow_comp*eps_q
        return
      end if
c attenzione!!! il test precedente deve sempre essere messo prima dei due
c che seguono
      rev_max = rev_lim(2)
      flow_aux = flow_lim(1)
      old_un_flow1 = un_flow1
      old_flow = flow
      flow = flow_aux/aux3
      un_flow1 = flow_aux/type_actr(itipo)
      diff = zero_rev2(rev_max)
      flow = old_flow
      un_flow1 = old_un_flow1
c diff e' la differenza fra pout richiesta e la massima p ottenibile in
c      stazione alla portata corrispondente al massimo head sulla prima fase
      if (diff.lt.0) then
c I compressori non riescono proprio a mandare a pout
        ier = 6
cgpe-corr
        if (iopt .or. iopt_mess) return
cgpe-corr-end
c diff=pmax-pout dove pout e la p richiesta da utente
        raux=pout+diff
c-UDM        write(mess,556) pout,raux
        write(mess,556) udm_out_str(udm_P,pout,0),
     *                  udm_out_str(udm_P,raux,1)
        call app_mess(vert_pointer,ier,mess)
        return
      endif
      rev_min = rev_lim(1)
      flow_aux = flow_lim(4)
      old_un_flow1 = un_flow1
      un_flow1 = flow_aux/type_actr(itipo)
      old_flow = flow
      flow = flow_aux/aux3
      diff = zero_rev2(rev_min)
      flow = old_flow
      un_flow1 = old_un_flow1
c diff e' ora la differenza fra pout richiesta e la massima p ottenibile in
c      stazione alla portata corrispondente al minimo head sulla prima fase
      if (diff.gt.0) then
c head richiesto e' troppo basso anche per il solo primo compressore
        ier = 7
cgpe-corr
        if (iopt .or. iopt_mess) return
cgpe-corr-end
c diff=pmax-pout dove pout e la p richiesta da utente
        raux=pout+diff
c-UDM        write(mess,557) pout,raux
        write(mess,557) udm_out_str(udm_P,pout,0),
     *                  udm_out_str(udm_P,raux,1)
c-UDM557     format('P man: ',f7.2,' - P man min:',f7.2)
557     format('p man: ',A10,' - p man min: ',A25)
        call app_mess(vert_pointer,ier,mess)
        return
      endif
c ripristino le variabili in common
      un_flow1 = old_un_flow1 
c
c---->                       calcolo numero di giri massimo
      if(flow_tot.gt.flow_lim(1)) then
        rev_max = rev_lim(2)
      else 
c e' sulla surge
        call NDAQ_SU(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *               lim_q(1,itipo),un_flow1,chn(1,itipo),
     *               rev_max,irange)
        rev_max = rev_max/type_ratio(ITIPO)
      end if
      if (rev_max.gt.rev_lim(1)) then
        fb = zero_rev2(rev_max)
      else
        fb = -1.
      end if
      if(fb.lt.0.) then
c---->                    i compressori non riescono a mandare a pout
        if(flow_tot.gt.flow_lim(1)) then
C QUI DEVO VEDERE SE POSSO SPERARE DI RAGGIUNGERE LA POUT RICHIESTA
C ACCENDENDO MACCHINE O NO.
C HO I SEGUENTI CASI: 
C         1)         LA POUT TOTALE RICHIESTA, PONENDO LA PORTATA UGUALE
C                    A FLOW_LIM(1) NON PUO' ESSERE RAGGIUNTA: DEVO CAMBIARE
C                    LA PIN O LA POUT
C         2)         LA POUT TOTALE RICHIESTA, PONENDO LA PORTATA UGUALE
C                    A FLOW_LIM(1) POTREBBE ESSERE RAGGIUNTA: DEVO ALLORA
C                    PRIMA PROVARE A CAMBIARE LE MACCHINE ATTIVE E POI, NEL
C                    LAST RECOVER, EVENTUALMENTE CAMBIARE LA P.
C                    ( NON FACCIO QUEST'ULTIMA COSA PER COERENZA CON LA
C                      STAZIONE SEMPLICE)
          flow_aux = flow_lim(1)
          old_un_flow1 = un_flow1
          un_flow1 = flow_aux/type_actr(itipo)
          old_flow = flow
          flow = flow_aux/aux3
          diff = zero_rev2(rev_max)
          flow = old_flow
          un_flow1 = old_un_flow1
c diff e' la differenza fra pout richiesta e la massima p ottenibile in
c      stazione
          if (diff.lt.0) then
c I compressori non riescono proprio a mandare a pout
            ier = 6
cgpe-corr
            if (iopt .or. iopt_mess) return
cgpe-corr-end
c diff=pmax-pout dove pout e la p richiesta da utente
            raux=pout+diff
c-UDM            write(mess,556) pout,raux
            write(mess,556) udm_out_str(udm_P,pout,0),
     *                      udm_out_str(udm_P,raux,1)
c-UDM556         format('P man: ',f7.2,' - P man max:',f7.2)
556         format('p man: ',A10,' - p man max: ',A25)
            call app_mess(vert_pointer,ier,mess)
            return
          else
c posso provare ad accendere macchine
            ier = 17
cgpe-corr
            if (iopt .or. iopt_mess) return
cgpe-corr-end
            rb = flow_lim(1)
            un_flow1 = old_un_flow1 
            ra = flow_lim(2)
            rd = ra
            un_rev = rev_lim(2)
            old_flow=flow_tot
            fa = zero_flow(ra)
            fb = zero_flow(rb)
            call zero_fun(ra,rb,rd,fa,fb,
     *			flow_comp,epsmi,pmach,zero_flow)
!            write(mess,555) old_flow,flow_comp
!            call app_mess(vert_pointer,ier,mess)
            flow_comp=flow_comp-flow_comp*eps_q
            return
          endif
c!!!gp          un_flow1 = old_un_flow1
        else 
c calcolo il riciclo sulla surge 
          ra = flow_lim(1)
          fa = zero_lim2(ra)
          rb = 0.
          rd = ra
          fb = zero_lim2(rb)
          call zero_fun(ra,rb,rd,fa,fb,flow_comp,
     *			epsmi,pmach,zero_lim2)
c
        end if
        if(un_rev.lt.rev_lim(1)) then
c gz 6-9-93  ---  devo verificare se esiste una soluzione nell'intervallo
c                 tra flow_lim(3) e flow_lim(4) sul rev_lim(1)
          flow_aux = flow_lim(3)
          old_un_flow1 = un_flow1
          un_flow1 = flow_aux/type_actr(itipo)
          old_flow = flow
          flow = flow_aux/aux3
          diff_aux = zero_rev2(rev_lim(1))
          flow=old_flow
          un_flow1 = old_un_flow1
          if (diff_aux.ge.0)then
c esiste la soluzione
c---> il compressore deve essere messo in riciclo al minimo di giri
            ra = flow_comp
C RB E' LA PORTATA ALLA INTERSEZIONE FRA REV_LIM(1) E LA RETTA H=0
            HAUX = 0
            raux = rev_lim(1)*type_ratio(itipo)
            CALL QDAHN(HAUX,RB,raux,chn(1,ITIPO))
            rb=rb*type_actr(itipo)
            rd = ra
            un_rev = rev_lim(1)
            fa = zero_flow(ra)
            fb = zero_flow(rb)
            call zero_fun(ra,rb,rd,fa,fb,
     *			flow_comp,epsmi,pmach,zero_flow)
          else
c non esiste soluzione: non si trova lo zero ne' sulla anti-surge, ne'
c sul minimo numero di giri.
c ???
          end if
         end if
        return
      end if
      HAUX = 0
      qh=haux/(un_flow1*un_flow1)*((qmrif*qmrif)/headrif)
      if (qh.lt.clim(3,itipo) ) then
        CALL NDAHQ(HAUX,UN_FLOW1,rev_MIN,chc(1,ITIPO))
      else
        CALL NDAHQ(HAUX,UN_FLOW1,rev_MIN,chn(1,ITIPO))
      endif
      rev_min = REV_MIN / type_ratio(ITIPO)
c---->                           calcolo minima pressione di uscita
      if(rev_min.lt.rev_lim(1)) then
        fa = zero_rev2(rev_lim(1))
c---->                 compressori in riciclo al numero minimo di giri
        if(fa.gt.0.) then
          if(flow_tot.lt.flow_lim(3)) then
            ra = flow_lim(3)
          else
            ra = flow_tot
          end if
C GIA' COMMENTATO rb = rev_lim(1)*type_ratio(1)*(ceh(14,1)+sqrt(ceh(15,1)))
          rb = flow_lim(4)
          un_rev = rev_lim(1)
          fa = zero_flow(ra)
          fb = zero_flow(rb)
          rd = ra
          call zero_fun(ra,rb,rd,fa,fb,
     *			flow_comp,epsmi,pmach,zero_flow)
          return
        else
          rev_min = rev_lim(1)
        end if
      end if
c---->             calcolo del numero di giri compreso tra min e max_rev
      ra = rev_min
      fa = zero_rev2(ra)
      rb = rev_max
      call zero_fun(ra,rb,rev0,fa,fb,
     *			rev,epsmi,pmach,zero_rev2)
      un_rev = rev
      return
      end

      real*4 Function zero_rev2(rev)
c
c***********************************************************************
c	funzione da azzerare per il
c	calcolo del numero di giri comune ai due compressori
c	per garantire il raggiungimento della pressione di uscita
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
C
      COMMON/MAR/istaz
      integer*4 istaz

      real*4    haux,eesp
      real*4    rev
      real*4    rrev,flow_min,head1,hmax,pres_int,un_eff1,tout,
     *          un_temp1,temp_int,exp_pol,esp2,uno_su_ro_attuale,
     *          un_flow2,head2,pres_out,qaux
      real*4    prop
      external  prop
      real*4    aux1,aux3_int
      integer*4 irange
      real*4    f_aux
c-ace18
      real*4 ro_out
CMAR
      real*4 comp_ratio 
	integer*4 ier
c-----------------------------------------------------------------------
      if(rev.le.0) then
        zero_rev2 = pin-pout
        return
      end if
c---->         calcolo altezza adiabatica e flusso nel primo stadio
      rrev = rev*type_ratio(ITIPO)
      call QDAN_SU(chn(1,itipo),a_coef(1,itipo),b_coef(1,itipo),
     *               nlim(itipo),
     *               lim_n(1,itipo),rrev,flow_min,haux,irange)
      if(un_flow1.gt.flow_min) then
        call QDAN_CH(clim(3,itipo),qaux,rrev,chn(1,itipo))
        if (un_flow1.le.qaux) then
          call hdaqn(head1,un_flow1,rrev,chn(1,itipo))
        else
          call hdaqn(head1,un_flow1,rrev,chc(1,itipo))
        endif
      else
        hmax = haux
        head1 = hmax*(1.-0.05*(un_flow1-flow_min)/flow_min)
      end if
c---->  calcolo efficienza pressione intermedia e temperatura intermedia
c gz verificare
      eesp=1/esp1
c---->                calcolo altezza adiabatica e portata
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      pres_int=pin*(((head1/aux1)+1)**eesp)
      call effdaqn(un_eff1,un_flow1,rrev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))
      un_eff1 = un_eff1*eff_corr(itipo)
cgpe
      if (un_eff1 .lt. 0.01) un_eff1 = 0.01
cgpe-end
      tout = 0.
      aux2 = (pres_int/pin)**esp1
	call out_temper_int(tair,tout,tin,zin,pin,pres_int,aux2,
     *            z_coef(1,ver_poi),
     *			1,1,1,
c-ace*            eff_coef,
     *            dh_coef(1,ind),cp_par(1,ind),
     *			1.,un_eff1,un_flow1,
     *            un_temp1,
c-ele
     *            stat_vars(9),stat_vars(10))
c-ele 28/4/06
c      temp_int=un_temp1
	temp_int=tout

c      pres_int = pres_int-stat_vars(8)
      f_aux=flow/type_actr(itipo)
c_ecol      pres_int = pres_int-stat_vars(8)*f_aux*f_aux
c_ecol
c-ace18
      call ro_real(ind,pres_int,temp_int,ro_out)
      pres_int = pres_int-(stat_vars(8)+k_int)*f_aux*f_aux/ro_out
c_ecol
c-ele      temp_int = un_temp1-stat_vars(9)
      if(temp_int.gt.stat_vars(10)) temp_int=stat_vars(10)
c---->                   secondo stadio
cgpe      exp_pol = cesad(ind)
CMAR

CMAR   ATTENZIONE al COMP_RATIO anche per gli altri casi!!!
      comp_ratio = (pout/pres_int)

	call politrop_esp(istaz,pres_int,temp_int,comp_ratio,exp_pol,ier)

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
      un_flow2 = flow*aux3_int/type_actr(itipo_coup)
      rrev = rev*type_ratio(itipo_coup)
      call QDAN_SU(chn(1,itipo_coup),a_coef(1,itipo_coup),
     *               b_coef(1,itipo_coup),nlim(itipo_coup),
     *               lim_n(1,itipo_coup),rrev,flow_min,haux,irange)
      if(un_flow2.gt.flow_min) then
        call QDAN_CH(clim(3,itipo_coup),qaux,rrev,chn(1,itipo_coup))
        if (un_flow2.le.qaux) then
          call hdaqn(head2,un_flow2,rrev,chn(1,itipo_coup))
        else
          call hdaqn(head2,un_flow2,rrev,chc(1,itipo_coup))
        endif
        if(head2.lt.0.) head2 = 0.
      else
        hmax = haux
        head2 = hmax*(1.-0.05*(un_flow2-flow_min)/flow_min)
      end if
c---->                calcolo pressione di uscita
      eesp=1/esp2
      pres_out=pres_int*(((head2/aux1)+1)**eesp)
      zero_rev2 = pres_out-pout
      return
      end

      real*4 Function zero_flow(flowt)
c***********************************************************************
c	funzione da azzerare per il 
c	calcolo della portata a fissato numero di giri
c	per garantire il raggiungimento della pressione di uscita
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      real*4   flowt
c
      real*4   uno_su_ro_attuale
      real*4   zero_rev2
      external zero_rev2
c---------------------------------------------------------------------
c---->     calcolo portata di macchina
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      flow_tot = flowt
      flow = flow_tot/aux3
      un_flow1 = flow_tot/type_actr(itipo)
c---->            calcolo pressione di uscita a un_rev (dal common)
      zero_flow = zero_rev2(un_rev)
      return
      end

      real*4 Function zero_pin(pres_in)
c***********************************************************************
c	funzione da azzerare per il 
c	calcolo della pressione di ingresso a fissata portata
c	per garantire il raggiungimento della pressione di uscita
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      COMMON/MAR/istaz
      integer*4 istaz

      real*4   pres_in
c
      real*4   rev_max
      real*4   uno_su_ro_attuale,aux1
      real*4   old_flow,old_un_flow1
      real*4   prop,zero_rev2
      external prop,zero_rev2
c---------------------------------------------------------------------
c---->     calcolo portata di macchina
c esp1 e' nel common compstat.INC
CMAR      zin = prop(pres_in,tin,z_coef(1,ver_poi))
      call zpt1(istaz,pres_in,tin,zin)
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pres_in
      aux3 = ROCN*uno_su_ro_attuale
      flow_tot = flow*aux3
      un_flow1 = flow_tot/type_actr(itipo)
      rev_max = giri_lim(1)
      old_flow = flow
      old_un_flow1 = un_flow1
      if (un_flow1.gt.port_lim(1)) then
        un_flow1=port_lim(1)
        flow = (port_lim(1)/aux3)*type_actr(itipo)
      endif
      zero_pin = zero_REV2(rev_max)
      flow = old_flow
      un_flow1 = old_un_flow1
      return
      end

      real*4 Function zero_flow_pw(flowt)
c***********************************************************************
c	funzione da azzerare per il 
c	calcolo della portata a numero di giri variabile
c	per garantire il raggiungimento della pressione di uscita
c       nel rispetto della potenza massima
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      integer*4 irange
      real*4   flowt
      real*4   rev_min,rev_max,fa,fb,fd,uno_su_ro_attuale,rev
      real*4   zero_pow2
      external zero_pow2
c
      real*4   ZERO_REV2
      external ZERO_REV2
c---------------------------------------------------------------------
c---->                      ricerca numero minimo di giri
      un_flow1 = flowt/type_actr(itipo)
      flow_tot = flowt
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3     = ROCN*uno_su_ro_attuale
      flow     = flowt/aux3
      if(flow_tot.lt.port_lim(1)) then
        rev_min = giri_lim(1)
      else
        call ndaq_ch(clim(4,itipo),un_flow1,rev_min,chc(1,itipo))
        rev_min=rev_min/type_ratio(itipo)
      end if
c
      if(flow_tot.gt.port_lim(2)) then
        rev_max = giri_lim(2)
      else
        call NDAQ_SU(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *               lim_q(1,itipo),un_flow1,chn(1,itipo),
     *               rev_max,irange)
        rev_max=rev_max/type_ratio(itipo)
      end if
      if (rev_max.lt.rev_min) rev_max=rev_min
      fb = zero_pow2(rev_min)
      fa = zero_pow2(rev_max)
      if(fa*fb.lt.0.) then
        fd = rev_max
cgpe-fun        call zero_fun1(rev_min,rev_max,fd,fa,fb,rev,
        call zero_fun(rev_min,rev_max,fd,fa,fb,rev,
     *			epsmi,pmach,zero_pow2)
        zero_flow_pw = zero_rev2(rev)
      else
        if (fa.ge.0) then
          zero_flow_pw = zero_rev2(rev_max)
        else
          zero_flow_pw = zero_rev2(rev_min)
        endif          
      endif
      return
      end

      real*4 Function zero_lim2(flowt)
c
c***********************************************************************
c	funzione da azzerare per il calcolo della portata 
c	per garantire il raggiungimento della pressione di uscita
c	e imponendo il punto di lavoro sulla curva anti_surge
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      real*4    flowt
c
      real*4    uno_su_ro_attuale,rev
      integer*4 irange
      real*4    zero_rev2
      external  zero_rev2
c-------------------------------------------------------------------
c---->      calcolo portata e numero di giri sulla curva anti sourge
      flow_tot = flowt
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      flow = flow_tot/aux3
      un_flow1 = flow_tot/type_actr(itipo)
      call NDAQ_SU(a_coef(1,itipo),b_coef(1,itipo),nlim(itipo),
     *               lim_q(1,itipo),un_flow1,chn(1,itipo),
     *               un_rev,irange)
      un_rev = un_rev/type_ratio(itipo)
c---->             calcolo pressione di uscita
      rev = un_rev
      zero_lim2 = zero_rev2(rev)
      return
      end

      real*4 Function zero_lim_ch(flowt)
c
c***********************************************************************
c***********************************************************************
c       funzione da azzerare per il calcolo della portata
c       per garantire il raggiungimento della pressione di uscita
c       e imponendo il punto di lavoro sulla curva anti_chocke della
c       prima fase
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      real*4    flowt
c
      real*4    uno_su_ro_attuale,rev
      real*4    zero_rev2
      external  zero_rev2
c-------------------------------------------------------------------
c---->      calcolo portata e numero di giri sulla curva anti choke
      flow_tot = flowt
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      flow = flow_tot/aux3
      un_flow1 = flow_tot/type_actr(itipo)
      call ndaq_ch(clim(4,itipo),un_flow1,rev,chc(1,itipo))
      rev = rev/type_ratio(itipo)
c---->             calcolo pressione di uscita
      zero_lim_ch = zero_rev2(rev)
      return
      end

      Subroutine recover_compr2(istaz,un_rev,flow_comp,flow_lim,
     *		rev_lim,flow_ric,pres1_ric,pres2_ric,stat_vars,pin,pout,
c-ace18
     *        tin,
     *        IER) 
c
c***********************************************************************
c	recover degli errori per due compressori in serie
c-----------------------------------------------------------------------
      implicit none
      integer*4 istaz   !indice stazione nella struttura dati completa
      integer*4 ier
      real*4    un_rev,flow_comp
      real*4    flow_lim(*),rev_lim(*)
      real*4    flow_ric,pres1_ric,pres2_ric,stat_vars(*)
      real*4    pin,pout
      real*4    qq,delp1,delp2
      integer*4 idummy
      real*4    rdummy
c-ace18
      real*4 tin,ro_in,ro_out
c
      rdummy = un_rev             ! serve per evitare %FORT-I-VARUNUSED
      rdummy = flow_comp       ! serve per evitare %FORT-I-VARUNUSED
      rdummy = flow_lim(1)   ! serve per evitare %FORT-I-VARUNUSED
      rdummy = rev_lim(1)     ! serve per evitare %FORT-I-VARUNUSED

      if(ier.eq.17) then
c devo solo uscire e provare con la nuova portata        
        qq = flow_ric*flow_ric
c-ace18
      call ro_real(istaz,pin,tin,ro_in)
      call ro_real(istaz,pout,tin,ro_out)
        delp1 = stat_vars(3)*qq/ro_in
        delp2 = stat_vars(4)*qq/ro_out
        PRES1_RIC=PIN + DELP1
        PRES2_RIC=POUT - DELP2
        if (pres2_ric.gt.stat_vars(6)) PRES2_ric=STAT_VARS(6)
        return
      endif
c---->                        setto al numero massimo di giri
c      un_rev = rev_lim(2)
c      if(ier.eq.6) flow_comp = flow_lim(1)
c      ier = 0
c      iw = 6
c      if(iw.gt.0) call app_mess(vert_pointer,iw)
      return
      end

      Subroutine Recover_station2(type_quant,type_avail,type_act,STATO,
     *                            unit_avail,ier)
c
c***********************************************************************
c	In caso di portata totale di macchina insufficiente,
c	aumenta, se possibile, il numero dei compressori attivi
c	in una stazione composta
c------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'

      INTEGER*4   ic,ier
      INTEGER*4   type_quant(*),type_avail(*),type_act(*),
     *            itip,itip_coup,unit_avail(*)
      CHARACTER*(*)  STATO(*)
      INTEGER*4   i
c
      itip      = 1
      itip_coup = 2
      ic        = 0
c---->     verifica se ci sono dei compressori disponibili
      if (type_act(itip).lt.type_avail(itip)) then
c---------------------------------------------------------------
        do i=1,itip-1
          ic = ic + type_quant(i)
        enddo
        do i=1,type_quant(itip)
          ic = ic + 1
          if (unit_avail(ic).ne.0 .and.
     *        stato(ic).eq.funz_off) then
            type_act(itip)=type_act(itip)+1
            STATO(ic) = funz_on
c per il tipo accoppiato:
            ic=type_quant(itip) + ic
            type_act(itip_coup)=type_act(itip_coup)+1
            STATO(ic) = funz_on
            goto 100
          endif
        enddo
100     ier = 0
      else
c---->               non e' possibile aggiungere alcun compressore,
c                                         la simulazione ha termine
        ier=10
      end if
      return
      end


	Subroutine dinrec_compr2(flow_comp,flow_lim,rev_lim)
c***********************************************************************
c	recover degli errori per due compressori in serie
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      real*4   flowt,flow_comp
      real*4   ra,rb,rd,fa,fb
      real*4   zero_flow
      EXTERNAL zero_flow
      real*4   flow_lim(*),rev_lim(*)
      
c----------------------------------------------------------------------
      ra = 0.
c---->                determinazione portata massima
      if(flow_tot.gt.flow_lim(2)) then
        rb = flow_lim(2)
      else
        rb = flow_tot
      end if
c---->                   ricerca portata a numero di giri massimo
      un_rev = rev_lim(2)
      rd = ra
      fa = zero_flow(ra)
      fb = zero_flow(rb)
      call zero_fun(ra,rb,rd,fa,fb,flowt,epsmi,pmach,zero_flow)
      flow_tot = flowt
c---->                    verifica curva di riciclo
      if(flow_tot.lt.flow_lim(1)) then
        flow_comp = flow_lim(1)
      else
        flow_comp = flow_tot
      end if
      return
      end

      Subroutine recover_turbs_Q(vert_pointer,flow_lim,rev_lim,
     *                         flow_comp,ier)
c***********************************************************************
c	recover dell'altezza adiabatica per massima potenza 
c	due compressori in serie, con correzione in portata
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      integer*4 vert_pointer
      real*4    zero_pow2,zero_flow_pw,zero_lim2
      EXTERNAL  zero_pow2,zero_flow_pw,zero_lim2
      real*4    flow_lim(*),rev_lim(*),rev_max,rev_min,fa,fb
      real*4    flow_comp
      real*4    ra,rb,rd
      real*4    old_flow
      real*4    flow_aux
      integer*4 ier,iw
      character*(max_len) mess/' '/
c-----------------------------------------------------------------------
      rev_max = un_rev
c verifica funzionamento sulla surge:
      ra = flow_lim(1)
      fa = zero_lim2(ra)
      rb = 0.
      rd = ra
      fb = zero_lim2(rb)
      call zero_fun(ra,rb,rd,fa,fb,flow_aux,
     *			epsmi,pmach,zero_lim2)
c un_rev e' settato in common nella zero_lim2
      rev_min = max(un_rev,rev_lim(1))
c---->                  verifica che con il numero minimo di giri 
c            la potenza richiesta e' minore della massima fornita
      fa = zero_pow2(rev_min)
      if(fa.gt.0.) then
c al minimo numero di giri assoluto la potenza richiesta e' maggiore della
c massima ottenibile dalla turbina. La simulazione termina con errore !!!
        iw = 18
        mess = ' '
        call app_mess(vert_pointer,iw,mess)
        ier = 1
        return
      endif
c---->                           ricerca punto di lavoro
      old_flow=flow_tot
c recover con cambio delle condizioni in portata
      rb = flow_lim(3)
      if(flow_comp.gt.flow_lim(2)) then
        ra = flow_lim(2)
      else
        ra = flow_comp
      end if
      giri_lim(1)=rev_lim(1)
      port_lim(1)=flow_lim(4)
      giri_lim(2)=rev_lim(2)
      port_lim(2)=flow_lim(1)
      rd = ra
      fa = zero_flow_pw(ra)
      fb = zero_flow_pw(rb)
      call zero_fun(ra,rb,rd,fa,fb,
     *			flow_comp,epsmi,pmach,zero_flow_pw)
cgpe      iw = 57
      iw = 47
!      write(mess,555) old_flow,flow_comp
!555   format(' Q di macchina: richiesta=',f8.3,'  -  massima=',
!     *          f8.3)
!      call app_mess(vert_pointer,iw,mess)
c metto ier =17 per chiamare la recover_compr2 e calcolare anche le p_rich
      ier = 17
      return
      end

      Subroutine recover_turbs_p(vert_pointer,flow_lim,rev_lim,
     *                         flow_comp,ier)
c***********************************************************************
c	recover dell'altezza adiabatica per massima potenza 
c	due compressori in serie, con correzione in pressione
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      integer*4 vert_pointer
      real*4    zero_pow2,zero_flow_pw
      EXTERNAL  zero_pow2,zero_flow_pw
      real*4    flow_lim(*),rev_lim(*),rev_max,rev_min,fa,fb,d,rev
      real*4    flow_comp
      integer*4 ier,iw
      character*(max_len) mess/' '/
      real*4    rdummy
c-----------------------------------------------------------------------
      rdummy = flow_comp  ! serve per evitare %FORT-I-VARUNUSED
c
      rev_max = un_rev
c---->                      ricerca numero minimo di giri
      if(flow_tot.lt.flow_lim(4)) then
        rev_min = rev_lim(1)
      else
        call ndaq_ch(clim(4,itipo),un_flow1,rev_min,chc(1,itipo))
        rev_min = rev_min/type_ratio(itipo)
      end if
c---->                  verifica che con il numero minimo di giri 
c            la potenza richiesta e' minore della massima fornita
      fa = zero_pow2(rev_min)
      if(fa.gt.0.) then
c al minimo numero di giri la potenza richiesta e' maggiore della
c massima ottenibile dalla turbina. La simulazione termina con errore !!!
        iw = 18
        mess = ' '
        call app_mess(vert_pointer,iw,mess)
        ier = 1
        return
      endif
c---->                           ricerca punto di lavoro
c devo distinguere fra imposizione in pressione e in portata      

      fb = zero_pow2(rev_max)
      d = rev_min
      call zero_fun(rev_min,rev_max,d,fa,fb,rev,
     *			epsmi,pmach,zero_pow2)
      un_rev = rev-epsmi
c assimilo ad una situazione di max head
      ier = 6
      iw = 15
      call app_mess(vert_pointer,iw,mess)
      return
      end

      real*4 Function zero_pow2(rev)
c***********************************************************************
c	calcola il numero di giri comune ai due compressori
c	per garantire il raggiungimento della pressione di uscita
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c

      COMMON/MAR/istaz
      integer*4 istaz

      real*4  rrev,rev
      real*4  max_tint,delp_int,delt_int,head1,eesp,aux1,tout,pres_int,
     *        uno_su_ro_attuale,un_eff1,qf,un_power1,pwmax,
     *        un_temp1,temp_int,exp_pol,esp2,un_flow2,head2,qr,un_eff2,
     *        un_power2,qaux
      integer*4 iii
      real*4    prop
      external  prop
      real*4    f_aux,aux3_int
      integer*4 irange
      real*4    hmax,haux,flow_min
c-ace18
      real*4 ro_out

CMAR
      real*4 comp_ratio
	integer*4 ier
      write(LGU,*)'START POTENZA......'
      write(LGU,*)'----------------------------------------------------'
c----------------------------------------------------------------------
      rrev = rev*type_ratio(itipo)
c---->                     calcolo potenza assorbita dai compressori
      f_aux=flow/type_actr(itipo)
      write(LGU,*)'f_aux=flow/type_actr(itipo): ',f_aux
c      delp_int = stat_vars(8)*f_aux*f_aux
c_ecol
      delp_int = (stat_vars(8)+k_int)*f_aux*f_aux
      write(LGU,*)'delp_int=(stat_vars(8)+k_int)*f_aux*f_aux:',delp_int
c_ecol
      delt_int = stat_vars(9)
      max_tint = stat_vars(10)
c---->                                compressori primo stadio
      call QDAN_CH(clim(3,itipo),qaux,rrev,chn(1,itipo))
      if (un_flow1.le.qaux) then
        call hdaqn(head1,un_flow1,rrev,chn(1,itipo))
      else
        call hdaqn(head1,un_flow1,rrev,chc(1,itipo))
      endif
      eesp=1/esp1
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      write(LGU,*)'aux1=(erre/agrvcc/pmol) * Zin * tin/esp1:',delp_int
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      write(LGU,*)'uno_su_ro_attuale=(1/1.e5)*
     * (erre/pmol)*Zin*tin/pin:',uno_su_ro_attuale
      aux3_int = ROCN*uno_su_ro_attuale
      write(LGU,*)'aux3_int = ROCN*uno_su_ro_attuale:',aux3_int
      pres_int=pin*(((head1/aux1)+1)**eesp)
      write(LGU,*)'pres_int=pin*(((head1/aux1)+1)**eesp):',pres_int
      call effdaqn(un_eff1,un_flow1,rrev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))
cgpe
	un_eff1 = un_eff1*eff_corr(itipo)
      write(LGU,*)'un_eff1 = un_eff1*eff_corr(itipo):',un_eff1
      if (un_eff1 .lt. 0.01) un_eff1 = 0.01
cgpe-end
      qf = UN_flow1/aux3_int
      write(LGU,*)'qf = UN_flow1/aux3_int:',qf
c---->          power required
cgpe      un_power1= AGRVcc * rocn * head1*qf/(un_eff1*
cgpe     *             eff_corr(itipo)*power_corr(itipo))
cgpe      un_power1= AGRVcc * rocn * head1*qf/(un_eff1*
cgpe     *             eff_corr(itipo))
      un_power1= AGRVcc * rocn * head1*qf/un_eff1
      write(LGU,*)'un_power1=AGRVcc*rocn*head1*qf/un_eff1:',un_power1
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power1=un_power1/(3.6*1.e3)
      write(LGU,*)'un_power1=un_power1/(3.6*1.e3):',un_power1
c-mf03      call real_minpower(itipo,un_power1)
c---->                                         calcolo potenza massima
      call pwdan(pwmax,rrev,tair,cpwm(1,itipo),cpwt(1,itipo))
c-mf03      call real_maxpower(itipo,pwmax)

ccc	fc  = cpw(4,1)+air*(cpw(5,1)+cpw(6,1)*air)
ccc        pwmax = (cpw(1,1)+rrev*(cpw(2,1)+cpw(3,1)*rrev))*fc
      iii = mod(itipo,2)
      if((iii.eq.1).and.(cpwm(1,itipo_coup).ne.0.)) then
c---->                         la turbina limitante e' la prima
        write(LGU,*)'la turbina limitante RISULTA la prima'
        zero_pow2 = un_power1-pwmax*power_corr(itipo)
        write(LGU,*)'un_power1-pwmax*power_corr(itipo):',zero_pow2
        write(LGU,*)'END POTENZA---'
        return
      end if
c---->                 calcolo pressione e temperatura intermedia
      un_eff1 = un_eff1*eff_corr(itipo)
      write(LGU,*)'un_eff1 = un_eff1*eff_corr(itipo):',un_eff1
      tout = 0.
      aux2 = (pres_int/pin)**esp1
      write(LGU,*)'aux2 = (pres_int/pin)**esp1:',aux2
	call out_temper_int(tair,tout,tin,zin,pin,pres_int,aux2,
     *            z_coef(1,ver_poi),
     *			1,1,1,
c-ace*            eff_coef,
     *            dh_coef(1,ind),cp_par(1,ind),
     *			1.,un_eff1,un_flow1,
     *            un_temp1,
     *            delt_int,max_tint)

c-ele 28/4/06
c      temp_int=un_temp1
      temp_int=tout
      write(LGU,*)'temp_int=tout:',temp_int
c-ace18
      call ro_real(ind,pres_int,temp_int,ro_out)
      write(LGU,*)'ro_out:',ro_out
      pres_int = pres_int-delp_int/ro_out
      write(LGU,*)'pres_int=pres_int-delp_int/ro_out:',pres_int
c      temp_int = un_temp1-delt_int
      if(temp_int.gt.max_tint) temp_int=max_tint
c---->                                       secondo stadio
cgpe      exp_pol = cesad(ind)

CMAR
      comp_ratio= (pout/pres_int)
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio,exp_pol,ier)

CMAR     exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))
     
	call zpt1(istaz,pres_int,temp_int,z)  
c---->                                       altezza adiabatica
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
      aux2 = (pout/pin)**esp2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      aux3_int = ROCN*uno_su_ro_attuale
      un_flow2 = flow*aux3_int/type_actr(itipo_coup)
      rrev = rev*type_ratio(itipo_coup)
      call QDAN_SU(chn(1,itipo),a_coef(1,itipo),b_coef(1,itipo),
     *               nlim(itipo),
     *               lim_n(1,itipo),rrev,flow_min,haux,irange)
      if(un_flow2.gt.flow_min) then
        call QDAN_CH(clim(3,itipo_coup),qaux,rrev,chn(1,itipo_coup))
        if (un_flow2.le.qaux) then
          call hdaqn(head2,un_flow2,rrev,chn(1,itipo_coup))
        else
          call hdaqn(head2,un_flow2,rrev,chc(1,itipo_coup))
        endif
        if(head2.lt.0.) head2 = 0.
      else
        hmax = haux
        head2 = hmax*(1.-0.05*(un_flow2-flow_min)/flow_min)
      endif       
      QR = un_flow2/rrev
      write(LGU,*)' QR = un_flow2/rrev:',QR
c---->                            potenza richiesta secondo stadio
      call effdaqn(un_eff2,un_flow2,rrev,cen(1,itipo_coup),
     *             cec(1,itipo_coup),clim(1,itipo_coup))
cgpe
	un_eff2 = un_eff2 * eff_corr(itipo_coup)
      write(LGU,*)' un_eff2 = un_eff2 * eff_corr(itipo_coup):',un_eff2
      if (un_eff2 .lt. 0.01) un_eff2 = 0.01
cgpe-end
      qf = UN_flow2/aux3_int
      write(LGU,*)'qf = UN_flow2/aux3_int:',qf
c---->          power required
cgpe      un_power2= AGRVcc * rocn * head2*qf/
cgpe     *      (un_eff2*eff_corr(itipo_coup)*power_corr(itipo_coup))
cgpe      un_power2= AGRVcc * rocn * head2*qf/
cgpe     *      (un_eff2*eff_corr(itipo_coup))
      un_power2= AGRVcc * rocn * head2*qf/un_eff2
      write(LGU,*)'un_power2= AGRVcc * rocn * 
     * head2*qf/un_eff2:',un_power2
cgpe-end
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power2=un_power2/(3.6*1.e3)
      write(LGU,*)'un_power2=un_power2/(3.6*1.e3):',un_power2
ccc	un_power2= Power0 *head2*flow/(type_actr(2)*
ccc     *		un_eff2*eff_corr(2))
      if(cpwm(1,itipo_coup).eq.0.) then
c---->                           unica turbina per due compressori
cgpe        zero_pow2 = un_power2+un_power1-pwmax
c-mf03        call real_minpower(itipo_coup,un_power2+un_power1)
cgpe_new        zero_pow2 = un_power2+un_power1-pwmax*power_corr(itipo_coup)
        zero_pow2 = un_power2+un_power1-pwmax*power_corr(itipo)
        write(LGU,*)' CASO unica turbina per due compressori'
        write(LGU,*)'zero_pow2 = un_power2+un_power1-
     * pwmax*power_corr(itipo):',zero_pow2
      else
c---->                             turbina limitante e' la seconda
c-mf03        call real_minpower(itipo_coup,un_power2)
c---->                                     calcolo potenza massima
        call pwdan(pwmax,rrev,tair,cpwm(1,itipo_coup),
     *           cpwt(1,itipo_coup))
c-mf03        call real_maxpower(itipo_coup,pwmax)

c        zero_pow2 = un_power2/power_corr(itipo_coup)-pwmax
cgpe        zero_pow2 = un_power2-pwmax
        zero_pow2 = un_power2-pwmax*power_corr(itipo_coup)
        write(LGU,*)' CASO turbina limitante risulta la seconda'
        write(LGU,*)' zero_pow2 = un_power2-pwmax
     *    *power_corr(itipo_coup):',zero_pow2
      end if
      write(LGU,*)'END POTENZA----------------------------'
      return
      end

      Subroutine dinrec_turbs(rev_lim,rev0,flow_lim,ier)
c
c***********************************************************************
c	recover dinamico della potenza massima di compressori in serie
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      real*4   zero_pow2d,zero_rev2
      EXTERNAL zero_pow2d,zero_rev2
c
      real*4  flow_lim(*),rev_lim(*),rrev
      real*4  rev_max,ra,rb,fl,ufl,rd,rev0,fa,fb,rev,rev_min,
     *        uno_su_ro_attuale,flowm,haux
      integer*4 irange,ier
      real*4    rdummy
c-----------------------------------------------------------------
      rdummy = flow_lim(1) ! serve per evitare %FORT-I-VARUNUSED
c--->            determinazione intervallo di ricerca
c	         estremo inferiore con portata zero
      rev_max = un_rev
      ra = rev_lim(1)
      rb = un_rev
      fl = flow
      ufl = un_flow1
      flow = 0.
      un_flow1 = 0.
      rd = rev0
      fa = zero_rev2(ra)
      fb = zero_rev2(rb)
      call zero_fun(ra,rb,rd,fa,fb,rev,epsmi,pmach,zero_rev2)
      flow = fl
      un_flow1 = ufl
      rev_min = rev
c---->                           ricerca soluzione
      fa = zero_pow2d(rev_min)
      fb = zero_pow2d(rev_max)
      call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *		rev,epsmi,pmach,zero_pow2d)
      un_rev = rev-epsmi
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      flow = flow_tot/aux3
c---->                  calcolo portata in riciclo
      rrev = un_rev*type_ratio(itipo)      
      call QDAN_SU(chn(1,itipo),a_coef(1,itipo),b_coef(1,itipo),
     *               nlim(itipo),
     *               lim_n(1,itipo),rrev,flowm,haux,irange)
      if(flowm.lt.flow_tot) ier = 0
      return
      end

      real*4 Function zero_pow2d(rev)
c***********************************************************************
c	funzione da azzerare per il
c	calcolo del numero di giri comune ai due compressori
c	per garantire il raggiungimento della pressione di uscita
c       imponendo la massima potenza della turbina
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/compstat.INC'
c
      COMMON/MAR/istaz
      integer*4 istaz

      real*4    haux,rev,rrev,ra,rb,rd,fa,fb,flowt,uno_su_ro_attuale,
     *          head1,eesp,aux1,pres_int,un_eff1,qf,un_power1,pwmax,
     *          tout,un_temp1,temp_int,exp_pol,esp2,un_flow2,flow_min,
     *          head2,hmax,un_eff2,un_power2,qaux
      real*4    prop
      external  prop
      integer*4 iii,irange
      real*4    zero_flow
      external  zero_flow
      real*4    f_aux,aux3_int
c-ace18
      real*4 ro_out
	real*4 comp_ratio
	integer*4 ier
c--------------------------------------------------------------------
c---->                         assegno un_rev per la zero_flow
      un_rev = rev
c---->                calcolo portata per numero di giri pari a rev
      rrev = rev*type_ratio(itipo)
      ra = 0.
      haux = 0
      CALL QDAHN(HAUX,RB,rrev,chc(1,ITIPO))
      rb=rb*type_actr(itipo)
      rd = flow_tot
      fa = zero_flow(ra)
      fb = zero_flow(rb)
      call zero_fun(ra,rb,rd,fa,fb,flowt,epsmi,pmach,zero_flow)
      flow_tot = flowt
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      flow = flow_tot/aux3
      un_flow1 = flow_tot/type_actr(itipo)
c---->           calcolo potenza assorbita dai compressori
      call QDAN_CH(clim(3,itipo),qaux,rrev,chn(1,itipo))
      if (un_flow1.le.qaux) then
        call hdaqn(head1,un_flow1,rrev,chn(1,itipo))
      else
        call hdaqn(head1,un_flow1,rrev,chc(1,itipo))
      endif
      eesp=1/esp1
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3 = ROCN*uno_su_ro_attuale
      pres_int=pin*(((head1/aux1)+1)**eesp)
      call effdaqn(un_eff1,un_flow1,rrev,cen(1,itipo),cec(1,itipo),
     *               clim(1,itipo))
cgpe
	un_eff1 = un_eff1*eff_corr(itipo)
      if (un_eff1 .lt. 0.01) un_eff1 = 0.01
cgpe-end
      qf = UN_flow1/aux3
c---->          power required
cgpe      un_power1= AGRVcc * rocn * head1*qf/(un_eff1*
cgpe     *              eff_corr(itipo)*power_corr(itipo))
cgpe      un_power1= AGRVcc * rocn * head1*qf/(un_eff1*
cgpe     *              eff_corr(itipo))
      un_power1= AGRVcc * rocn * head1*qf/un_eff1
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power1=un_power1/(3.6*1.e3)
      iii = mod(itipo,2)
      if((iii.eq.1).and.(cpwm(1,itipo_coup).ne.0.)) then
c---->                     calcolo potenza massima turbina
c-mf03        call real_minpower(itipo,un_power1)
        call pwdan(pwmax,rrev,tair,cpwm(1,itipo),cpwt(1,itipo))
c-mf03        call real_maxpower(itipo,pwmax)
c---->                     la turbina limitante e' la prima
cgpe        zero_pow2d = un_power1-pwmax
        zero_pow2d = un_power1-pwmax*power_corr(itipo)
        return
      end if
c---->      calcolo pressione e temperatura intermedia
      un_eff1 = un_eff1*eff_corr(itipo)
      tout = 0.
      aux2 = (pres_int/pin)**esp1
	call out_temper_int(tair,tout,tin,zin,pin,pres_int,aux2,
     *            z_coef(1,ver_poi),
     *			1,1,1,
c-ace*            eff_coef,
     *            dh_coef(1,ind),cp_par(1,ind),
     *			1.,un_eff1,un_flow1,
     *            un_temp1,
     *            stat_vars(9),stat_vars(10))
c-ele 28/4/06
c      temp_int = un_temp1
      temp_int = tout
c-ace18
      call ro_real(ind,pres_int,temp_int,ro_out)
      f_aux=flow/type_actr(itipo)
      pres_int = pres_int-stat_vars(8)*f_aux*f_aux/ro_out
c      temp_int = un_temp1-stat_vars(9)
      if(temp_int.gt.stat_vars(10)) temp_int=stat_vars(10)
c---->                    secondo compressore
cgpe      exp_pol = cesad(ind)

CMAR
      comp_ratio= (pout/pres_int)
      call politrop_esp(istaz,pres_int,temp_int,comp_ratio,exp_pol,ier)


CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
cgpe-end
      esp2 = (exp_pol-1.)/exp_pol
CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))
  
      call zpt1(istaz,pres_int,temp_int,z)
c---->                                       altezza adiabatica
      aux2 = (pout/pin)**esp2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      aux3_int = ROCN*uno_su_ro_attuale
      un_flow2 = flow*aux3_int/type_actr(itipo_coup)
      rrev = rev*type_ratio(itipo_coup)
      call QDAN_SU(chn(1,itipo_coup),a_coef(1,itipo_coup),
     *             b_coef(1,itipo_coup),nlim(itipo_coup),
     *             lim_n(1,itipo_coup),rrev,flow_min,haux,irange)
      if(un_flow2.gt.flow_min) then
        call QDAN_CH(clim(3,itipo_coup),qaux,rrev,chn(1,itipo_coup))
        if (un_flow2.le.qaux) then
          call hdaqn(head2,un_flow2,rrev,chn(1,itipo_coup))
        else
          call hdaqn(head2,un_flow2,rrev,chc(1,itipo_coup))
        endif
        if(head2.lt.0.) head2 = 0.
      else
        hmax = haux
        head2 = hmax*(1.-0.05*(un_flow2-flow_min)/flow_min)
      end if
c---->               efficienza e potenza richiesta
      qf = UN_flow2/aux3_int
      call effdaqn(un_eff2,un_flow2,rrev,cen(1,itipo_coup),
     *             cec(1,itipo_coup),clim(1,itipo_coup))
cgpe
	un_eff2 = un_eff2 * eff_corr(itipo_coup)
      if (un_eff2 .lt. 0.01) un_eff2 = 0.01
cgpe-end
c---->          power required
cgpe      un_power2= AGRVcc * rocn * head2*qf/(un_eff2*
cgpe     *               eff_corr(itipo_coup)*power_corr(itipo_coup))
cgpe      un_power2= AGRVcc * rocn * head2*qf/(un_eff2*
cgpe     *               eff_corr(itipo_coup))
      un_power2= AGRVcc * rocn * head2*qf/un_eff2
c gz un_power e' ora in Kjoule/ora. Conversione in KWatt:
      un_power2=un_power2/(3.6*1.e3)
      if(cpwm(1,itipo_coup).eq.0.) then
c---->                   unica turbina per due compressori
c---->                     calcolo potenza massima turbina
c-mf03        call real_minpower(itipo,un_power2+un_power1)
        call pwdan(pwmax,rrev,tair,cpwm(1,itipo),cpwt(1,itipo))
c-mf03        call real_maxpower(itipo,pwmax)

cgpe        zero_pow2d = un_power2+un_power1-pwmax
        zero_pow2d = un_power2+un_power1-pwmax*power_corr(itipo_coup)
      else
c---->                turbina limitante e' la seconda
c---->                     calcolo potenza massima turbina
c-mf03        call real_minpower(itipo_coup,un_power2)
        call pwdan(pwmax,rrev,tair,cpwm(1,itipo_coup),
     *             cpwt(1,itipo_coup))
c-mf03        call real_maxpower(itipo_coup,pwmax)

cgpe        zero_pow2d = un_power2-pwmax
        zero_pow2d = un_power2-pwmax*power_corr(itipo_coup)
      end if
      return
      end

	Subroutine out_temper2(
     *            tair,fase,
     *            tout,tin,zin,pin,pout,aux2,z_coef,
     *			ntp,type_actr,type_quant,
     *            dh_coef,cp_par,
     *			unit_perc,unit_eff,
     *            unit_flow,
     *            tmax_man,  !new
     *            unit_temp)
c
c	Calcola la temperatura di uscita di ogni compressore della prima
c       fase per una stazione composta che lavora in serie
c
        implicit none
      include '../inc/PARAM.inc'
	include '../inc/RK_PARAM.INC'
      
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
      real*4 tair          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
c
      integer*4 fase       !fase di inserimento dell'aircooler

	REAL	tout,tin,pin,zin,pout,aux2,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*),
     *         unit_flow(*),tmax_man
      REAL*4  dh_coef(*),cp_par(*)

	INTEGER*4   ntp,type_actr(*),type_quant(*)
c
        INTEGER*4 i,nu,ic,icj,nt
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
        real*4 dtp,deltat
        integer*4 idummy
c---------------------------------------------------------------------
        idummy = type_actr(1)  ! serve per evitare %FORT-I-VARUNUSED
	nu = 0
	do ic =1,ntp
          do i=1,type_quant(ic)
            icj=nu+i
            if (unit_eff(icj).gt.0) then
c-ace
c---->                         outlet theoretical temperature extimation
	      call single_out_temp_new(unit_temp(icj),tin,zin,pin,
     *      pout,aux2,
     *      z_coef,dh_coef,cp_par,unit_eff(icj))
c-ace
c-ele-28/4/06
c      	  call single_out_temp_ac(tair,pout,icj,fase,
c     *               unit_flow(icj),tmax_man,unit_temp(icj))
            write(LGU,*)'AC CALL 17'
      	  call single_out_temp_ac_new(tair,pout,icj,fase,
     *               ,unit_flow(icj),tmax_man,unit_temp(icj)
     *               ,pin,tin,deltat)

c---->               station outlet temperature
c-ele-28/4/06
c              tout = tout + unit_perc(icj)*unit_temp(icj)
              tout = tout + unit_perc(icj)*(unit_temp(icj)-deltat)
            endif
          enddo
          nu = nu+type_quant(ic)
	end do
	return
	end
c--------------------------------------------------------------------------------------------
        Subroutine init_multi_stadio(
     *          istaz,vert_pointer,tipo_criterio,
     *          pres1,pres2,pres1_ric,pres2_ric,air,tot_cons,
csave
     *          pres_int,temp_int,delprint,delt_int,
csave-end

cgpe     *          delpr1,delpr2,flowm,flow_ric,
     *          flowm,flow_ric,
     *          unit_num,type_num,type_quant_old,jufir,jtfir,
cgpe     *          nit,mirev,marev,mieff,lstat)
     *          mirev,marev,mieff,lstat,stat_vars)

c
c***********************************************************************
c       inizializzazione del common simpstat per una stazione composta
c       con i compressori in serie (vengono riempite le aree common
c        - del tipo 1 per la I fase
c        - del tipo 2 per la II fase
c       NOTA: prima di chiamare la subroutine, e' necessario di salvare
c             il valore di type_quant e ripristinarlo dopo la
c             agg_multi_stadio
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
c
      include '../inc/prototipo.INC'
c_ecol
      include '../inc/filtri.INC'
      include '../inc/AIR_COOLER.INC'
c_ecol
c
      integer*4 istaz,vert_pointer
      integer*4 unit_num,type_num,type_quant_old(*),jufir,jtfir
c
      real*4    pres1,pres2,air,tot_cons
csave
     *         ,pres_int,temp_int,delprint,delt_int
csave-end

cgpe     *         ,delpr1,delpr2,
     *          ,flowm,flow_ric
      real*4    mirev(*),marev(*),mieff(*)
      real*4    stat_vars(*)
      LOGICAL*2 lstat(*)
      real*4    pres1_RIC,pres2_RIC

      integer*4 i,j,k,nu,ac_nu
      integer*4 tipo_criterio
      real*4    aa,bb,denom,faux
      integer*4 i1,ii1

      integer*4 itipo_act,jtipo_act,iunit_act
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
c--------------------------------->filtri
	    f_tg_stat(k)=0.
	    f_tg_k(k)=0.
c--------------------------------->ac1
	    ac1_stat(k)=0.
  	    ac1_k(k)=0.
	    ac1_q_prog(k)=0.
	    ac1_taria_prog(k)=0.
c--------------------------------->ac2
	    ac2_stat(k)=0.
  	    ac2_k(k)=0.
	    ac2_q_prog(k)=0.
	    ac2_taria_prog(k)=0.
!-seconda fase
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
        end do
        nu = nu + type_quant_old(i)
      end do
c
      itipo = 1
      itipo_coup = 2

!-inizializzazione del vettore lstat
      i1=0
      do i=1,type_num
        j=jtfir+i-1
        do k=1,type_quant_old(i)
          lstat(k+i1) = .false.
          lstat(k+i1+unit_num) = .false.
        end do
        i1=i1+type_quant_old(i)
      end do
c
!-definizione di jtipo_act e iunit_act (sono rispettivamente minori di
!-max_type e max_unit), mentre itipo_act e' minore di maxunits
      if (iopt) then
        i1=0
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            if (index(stat(k+i1),funz_on) .gt. 0) then
              itipo_act = i
              jtipo_act = j
              iunit_act = jufir+k+i1-1
              lstat(k) = .true.
              lstat(k+type_quant_old(i)) = .true.
            end if
          end do
          i1=i1+type_quant_old(i)
        end do
      else
        i1=0
        nu=jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            ii1=nu+k-1
            if(index(status(ii1),funz_on).gt.0) then
              itipo_act = i
              jtipo_act = j
              iunit_act = ii1
              lstat(k) = .true.
              lstat(k+type_quant_old(i)) = .true.
            end if
          end do
          nu=nu+type_quant_old(i)
          i1=i1+type_quant_old(i)
        end do
      end if
c
      tot_cons = 0.
csave
      pres_int = 0.
	temp_int = T0
	delprint = 0.
	delt_int = 0.
csave-end

cgpe      delpr1   = 0.
cgpe      delpr2   = 0.
      flowm    = 0.
      flow_ric = 0.
      PRES1_RIC=PRES1
      PRES2_RIC=PRES2
c---->               preparazione common stazione.inc
      ind    = istaz
      ver_poi = vert_pointer
      delta = stat_vars(1)
c---->                                  generazione common SIMPSTAT
      num_iter = nit
      tair = air
!      ntp = 2*type_num
      ntp = itipo_coup
      tipo_crit = tipo_criterio

      i1=0
      do i=1,type_num
        type_actr(i) = 0
        type_actr(i+type_num) = 0
      end do
c
      type_actr(itipo) = type_act(jtipo_act)
      type_actr(itipo_coup) = type_act(jtipo_act)
c
      type_quant(jtfir+itipo-1) = type_quant_old(itipo_act)
      type_quant(jtfir+itipo_coup-1) = type_quant_old(itipo_act)

      nu = jufir
      do i = 1,type_num
        j = jtfir+i-1
        do k = 1,type_quant_old(i)
          if (i .eq. itipo_act) then
            unit_eff_corr(jufir+k-1) = unit_eff_corr(nu+k-1)
            unit_power_corr(jufir+k-1) = unit_power_corr(nu+k-1)
            unit_eff_corr(jufir+k-1+type_quant_old(i)) = 
     *                                 unit2_eff_corr(nu+k-1)
            unit_power_corr(jufir+k-1+type_quant_old(i)) =
     *                                 unit2_power_corr(nu+k-1)
          endif
        end do
        nu = nu+type_quant_old(i)
      end do

c non dovrebbe essere necessario settare un_eff_corr, mentre eff_corr dovrebbe
c essere settato per tipo, e per la seconda fase
        eff_corr(itipo) = unit_eff_corr(iunit_act)
        power_corr(itipo) = unit_power_corr(iunit_act)
        hrate_corr(itipo) = unit_hrate_corr(iunit_act)
        type_vinc_maxpow(itipo)=unit_vinc_maxpow(iunit_act)
        type_vinc_minpow(itipo)=unit_vinc_minpow(iunit_act)
        type_flag_maxpow(itipo)=.false.
c-mf03	  if (unit_vinc_maxrev(iunit_act).lt.max_rev(iunit_act).and.
c-mf03     *      unit_vinc_maxrev(iunit_act).gt.min_rev(iunit_act)     ) then
c-mf03	    max_rev(iunit_act) = unit_vinc_maxrev(iunit_act)
c-mf03          type_flag_maxrev(itipo) = .true.
c-mf03	  end if
        type_vinc_maxrev(itipo) = unit_vinc_maxrev(iunit_act)
c-mf03-end
        type_ratio(itipo) = unit_rev_rat(iunit_act)
        type_bifase(itipo) = unit_bifase(iunit_act)
        type_delpint(itipo) = unit_delprint(iunit_act)
        type_deltint(itipo) = unit_deltrint(iunit_act)
        type_maxtint(itipo) = unit_maxtint(iunit_act)
        mirev(itipo)  = min_rev(iunit_act) * revrif
        marev(itipo)  = max_rev(iunit_act) * revrif
        mieff(itipo)  = min_eff(iunit_act)
        type_min_rev(itipo)  = min_rev(iunit_act) * revrif
        type_max_rev(itipo)  = max_rev(iunit_act) * revrif
c-seconda fase ==> secondo tipo
        eff_corr(itipo_coup) = unit2_eff_corr(iunit_act)
        power_corr(itipo_coup) = unit2_power_corr(iunit_act)
        type_ratio(itipo_coup) = unit2_rev_rat(iunit_act)
        mirev(itipo_coup)      = min2_rev(iunit_act) * revrif
        marev(itipo_coup)      = max2_rev(iunit_act) * revrif
        mieff(itipo_coup)      = min2_eff(iunit_act)
        type_min_rev(itipo_coup)  = min2_rev(iunit_act) * revrif
        type_max_rev(itipo_coup)  = max2_rev(iunit_act) * revrif
c
        do k=1,max_lim
          a_coef(k,itipo) = type_a_coef(k,iunit_act)
          b_coef(k,itipo) = type_b_coef(k,iunit_act)
          lim_h(k,itipo) = type_lim_h(k,iunit_act)
          lim_n(k,itipo) = type_lim_n(k,iunit_act)
          lim_q(k,itipo) = type_lim_q(k,iunit_act)
c-seconda fase
          a_coef(k,itipo_coup) = type2_a_coef(k,iunit_act)
          b_coef(k,itipo_coup) = type2_b_coef(k,iunit_act)
          lim_h(k,itipo_coup)  = type2_lim_h(k,iunit_act)
          lim_n(k,itipo_coup)  = type2_lim_n(k,iunit_act)
          lim_q(k,itipo_coup)  = type2_lim_q(k,iunit_act)
        enddo
c
        clim(1,itipo) = type_c_coef(1,iunit_act)
        clim(2,itipo) = type_c_coef(2,iunit_act)
        clim(3,itipo) = type_c_coef(3,iunit_act)
        clim(4,itipo) = type_c_coef(4,iunit_act)
c-seconda fase
        clim(1,itipo_coup) = type2_c_coef(1,iunit_act)
        clim(2,itipo_coup) = type2_c_coef(2,iunit_act)
        clim(3,itipo_coup) = type2_c_coef(3,iunit_act)
        clim(4,itipo_coup) = type2_c_coef(4,iunit_act)
c
        do k=1,5
          chn(k,itipo) = type_chn(k,iunit_act)
          cen(k,itipo) = type_cen(k,iunit_act)
c-seconda fase
          chn(k,itipo_coup) = type2_chn(k,iunit_act)
          cen(k,itipo_coup) = type2_cen(k,iunit_act)
        end do
        chn(6,itipo) = type_chn(6,iunit_act)
c-seconda fase
        chn(6,itipo_coup) = type2_chn(6,iunit_act)
c attenzione !!! i chc sono uguali ai chn se non e' definita la zona di choking
        if (clim(3,itipo).ne.clim(4,itipo)) then
          do k=1,6
            chc(k,itipo) = type_chc(k,iunit_act)
          end do
        else
          do k=1,6
            chc(k,itipo) = chn(k,itipo)
          end do
        endif
c-seconda fase
        if (clim(3,itipo_coup).ne.clim2(4,itipo_coup)) then
          do k=1,6
            chc(k,itipo_coup) = type2_chc(k,iunit_act)
          end do
        else
          do k=1,6
            chc(k,itipo_coup) = chn2(k,itipo)
          end do
        endif
c
        if (clim(1,itipo).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim(1,itipo)
          bb=1/clim(2,itipo)
          denom=aa-bb
          faux=cen(1,itipo)+cen(2,itipo)/aa+cen(3,itipo)/aa/aa
          cec(1,itipo)=(aa*faux-mieff(itipo)*bb)/denom
          cec(2,itipo)=(aa*cen(4,itipo))/denom
          cec(3,itipo)=(aa*cen(5,itipo))/denom
          cec(4,itipo)=(aa*bb*(mieff(itipo)-faux))/denom
          cec(5,itipo)=-(aa*bb*cen(4,itipo))/denom
          cec(6,itipo)=-(aa*bb*cen(5,itipo))/denom
        endif
c-seconda fase
        if (clim(1,itipo_coup).gt.0) then
c    calcolo dei coefficienti CEC per l'efficienza in zona choking
          aa=1/clim(1,itipo_coup)
          bb=1/clim(2,itipo_coup)
          denom=aa-bb
          faux=cen(1,itipo_coup)+cen(2,itipo_coup)/aa+
     *         cen(3,itipo_coup)/aa/aa
          cec(1,itipo_coup)=(aa*faux-mieff(itipo_coup)*bb)/denom
          cec(2,itipo_coup)=(aa*cen(4,itipo_coup))/denom
          cec(3,itipo_coup)=(aa*cen(5,itipo_coup))/denom
          cec(4,itipo_coup)=(aa*bb*(mieff(itipo_coup)-faux))/denom
          cec(5,itipo_coup)=-(aa*bb*cen(4,itipo_coup))/denom
          cec(6,itipo_coup)=-(aa*bb*cen(5,itipo_coup))/denom
        endif
c
        nlim(itipo)= type_nlim(iunit_act)
c-seconda fase
        nlim(itipo_coup)= type2_nlim(iunit_act)
c
        do k=1,3
          cpwm(k,itipo) = type_cpwm(k,iunit_act)
          cpwt(k,itipo) = type_cpwt(k,iunit_act)
          chr(k,itipo)  = type_chr(k,iunit_act)
          chrt(k,itipo) = type_chrt(k,iunit_act)
        end do
        cpwt(4,itipo) = type_cpwt(4,iunit_act)
        chrt(4,itipo) = type_chrt(4,iunit_act)
        chr(4,itipo) = type_chr(4,iunit_act)
        chr(5,itipo) = type_chr(5,iunit_act)
        chr(6,itipo) = type_chr(6,iunit_act)
c-seconda fase
        do k=1,3
          cpwm(k,itipo_coup) = type_cpwm(k,iunit_act)
          cpwt(k,itipo_coup) = type_cpwt(k,iunit_act)
          chr(k,itipo_coup)  = type_chr(k,iunit_act)
          chrt(k,itipo_coup) = type_chrt(k,iunit_act)
        end do
        cpwt(4,itipo_coup) = type_cpwt(4,iunit_act)
        chrt(4,itipo_coup) = type_chrt(4,iunit_act)
        chr(4,itipo_coup) = type_chr(4,iunit_act)
        chr(5,itipo_coup) = type_chr(5,iunit_act)
        chr(6,itipo_coup) = type_chr(6,iunit_act)
c_ecol
c  Inizializzazione aree common 'filtri','air_cooler' e 'air_cooler2'
c  definite nei files d'include 'filtri.inc' e 'air_cooler.inc'
c_ecol --------------------------->Filtri
	f_ce_stat=c_f_ce_stat(vert_pointer)
	f_ce_k=c_f_ce_k(vert_pointer)

      nu=jufir
	do i=1,type_num
	   do k=1,type_quant_old(i)
	      if(i.eq.itipo_act) then
	        if(lstat(k))then
	            f_tg_stat(k)=c_f_tg_stat(nu+k-1)
	            f_tg_k(k)=c_f_tg_k(nu+k-1)
	        end if
	      end if
	   end do
	   nu=nu+type_quant_old(i)
	end do

c_ecol --------------------------->AirCooler
c--------------------->gestione aircooler intermedio
c
	nu=jufir
	do i=1,type_num
	  if(i.eq.itipo_act) then
	    do k=1,type_quant_old(i)
	       if(lstat(k)) then
	         ac1_stat(k)=c_ac1_stat(nu+k-1)
  	         ac1_k(k)=c_ac1_k(nu+k-1)
	         ac1_q_prog(k)=c_ac1_q_prog(nu+k-1)
	         ac1_taria_prog(k)=c_ac1_taria_prog(nu+k-1)
			end if
	     end do
	   end if
	   nu=nu+type_quant_old(i)
	end do

      k_int=0.
      ac_nu=0
      do i=1,type_quant_old(itipo_act)
         if (lstat(i))then
            k_int=k_int+ac1_k(i)
            ac_nu=ac_nu+1
         end if
      end do
      if (ac_nu.gt.0) then
         k_int=k_int/ac_nu
      end if
c------->gestione air cooler di centrale
	ac_ce_stat=c_ac_ce_stat(vert_pointer)
	ac_ce_k=c_ac_ce_k(vert_pointer)
	ac_q_prog=c_ac_q_prog(vert_pointer)
	ac_taria_prog=c_ac_taria_prog(vert_pointer)

	nu=jufir
	do i=1,type_num
	   if(i.eq.itipo_act) then
	      do k=1,type_quant_old(i)
	         if(lstat(k+type_quant_old(i))) then
	            ac2_stat(k)=c_ac2_stat(nu+k-1)
  	            ac2_k(k)=c_ac2_k(nu+k-1)
	            ac2_q_prog(k)=c_ac2_q_prog(nu+k-1)
	            ac2_taria_prog(k)=c_ac2_taria_prog(nu+k-1)
			  end if
	       end do
	    end if
	    nu=nu+type_quant_old(i)
	end do
c---->            set unit status as logical variable
c i bifase hanno una sola turbina
c
      una_turb = .true.
      return  ! init_simpstat_g_comp
      end

        Subroutine agg_multi_stadio(stat_ord,type_num,type_quant_old,
     *             unit_num,jtfir,jufir,
     *             pres_int,temp_int,delprint,delt_int,lstat)
c
c       Nel caso del criterio dei giri per una stazione composta con i
c       compressori in serie, aggiornamento dei dati in caso di
c       simulazione oppure correzione OK
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/rk_param.inc'
      include '../inc/stazione.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/MF_TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/simpstat.inc'

      integer*4 stat_ord,type_num,type_quant_old(*),
     *          unit_num,jtfir,jufir
      real*4    temp_int,pres_int,delt_int,delprint
      LOGICAL*2 lstat(*)

      integer*4 i,j,k
      integer*4 nu,ii1,i1

      integer*4 itipo_act,nutot
c-----------------------------------------------------------------------
      itipo = 1
      itipo_coup = 2

!- calcolo preliminare del tipo attivo in input su cui scaricare i dati
!- della simulazione
      if (iopt) then
        i1=0
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            if(index(stat(k+i1),funz_on).gt.0) then
              itipo_act = i
            end if
          end do
          i1=i1+type_quant_old(i)
        end do
      else
        i1=0
        nu=jufir
        do i=1,type_num
          j=jtfir+i-1
          do k=1,type_quant_old(i)
            ii1=nu+k-1
            if(index(status(ii1),funz_on).gt.0) then
              itipo_act = i
            end if
          end do
          nu=nu+type_quant_old(i)
          i1=i1+type_quant_old(i)
        end do
      end if
c
      nutot = jufir
      do i=1,itipo_act-1
        nutot = nutot + type_quant_old(i)
      end do

!      j = type_quant(jtfir+itipo-1)
!      do k = 1,j
!
!!-dati sulla seconda fase
!        unit2_flow(nutot+k-1) = unit_flow(jufir+k-1+j)
!        unit2_head(nutot+k-1) = unit_head(jufir+k-1+j)
!        unit2_rev(nutot+k-1)  = unit_rev(jufir+k-1+j)
!!        unit2_perc(nutot+k-1) = unit_perc(jufir+k-1+j)
!        unit2_eff(nutot+k-1)  = unit_eff(jufir+k-1+j)
!        unit2_power(nutot+k-1)      = unit_power(jufir+k-1+j)
!        unit2_temp(nutot+k-1) = unit_temp(jufir+k-1+j)
!        unit2_max(nutot+k-1)  = unit_max(jufir+k-1+j)
!        unit2_min(nutot+k-1)  = unit_min(jufir+k-1+j)
!
!
!!-dati sulla prima fase
!        unit_flow(nutot+k-1)  = unit_flow(jufir+k-1)
!        unit_head(nutot+k-1)  = unit_head(jufir+k-1)
!        unit_rev(nutot+k-1)   = unit_rev(jufir+k-1)
!        unit_perc(nutot+k-1)  = unit_perc(jufir+k-1)
!        unit_eff(nutot+k-1)   = unit_eff(jufir+k-1)
!        unit_power(nutot+k-1)      = unit_power(jufir+k-1)
!        unit_temp(nutot+k-1)  = unit_temp(jufir+k-1)
!        unit_max(nutot+k-1)   = unit_max(jufir+k-1)
!        unit_min(nutot+k-1)   = unit_min(jufir+k-1)
!        unit_hrate(nutot+k-1) = unit_hrate(jufir+k-1) 
!        unit_cons(nutot+k-1)  = unit_cons(jufir+k-1)
!
!!-dati intermedi in quanto stat_ord=f_serie
!        unit_temp_int(nutot+k-1) = temp_int
!        unit_pres_int(nutot+k-1) = pres_int
!      enddo
c
      j = type_quant(jtfir+itipo-1)
c-ele-3/5/06
c   dopo aver scaricato i dati relativi alla seconda fase nei vettori della common
c   MF_UNITS.INC ripulisco il relativo campo del vettore corrispondente della common
c   UNITS.INC
c
      do k = 1,j
!-dati sulla seconda fase
        unit2_flow(nutot+k-1) = unit_flow(jufir+k-1+j)
c-3/5
        unit_flow(jufir+k-1+j)  = 0.0
        unit2_head(nutot+k-1) = unit_head(jufir+k-1+j)
c-3/5
        unit_head(jufir+k-1+j)  = 0.0
        unit2_rev(nutot+k-1)  = unit_rev(jufir+k-1+j)
c-3/5
        unit_rev(jufir+k-1+j)   = 0.0
!        unit2_perc(nutot+k-1) = unit_perc(jufir+k-1+j)

        unit2_eff(nutot+k-1)  = unit_eff(jufir+k-1+j)
c-3/5
        unit_eff(jufir+k-1+j)   = 0.0
        unit2_power(nutot+k-1)      = unit_power(jufir+k-1+j)
c-3/5
        unit_power(jufir+k-1+j) = 0.0
        unit2_temp(nutot+k-1) = unit_temp(jufir+k-1+j)
c-3/5
        unit_temp(jufir+k-1+j)  = t0
        unit2_max(nutot+k-1)  = unit_max(jufir+k-1+j)
c-3/5
        unit_max(jufir+k-1+j)   = 0.0
	  unit2_min(nutot+k-1)  = unit_min(jufir+k-1+j)
c-3/5
        unit_min(jufir+k-1+j)   = 0.0

!-dati intermedi in quanto stat_ord=f_serie
        unit_temp_int(nutot+k-1) = temp_int
        unit_pres_int(nutot+k-1) = pres_int
      enddo

c-ele-3/5/06
c     commento l'azzeramento dei campi per evitare di perdere i risultati relativi 
c     alla prima fase di compressione
c
      do k = j, 1, -1
!-dati sulla prima fase
        unit_flow(nutot+k-1)  = unit_flow(jufir+k-1)
c-3/5        unit_flow(jufir+k-1)  = 0.0
        unit_head(nutot+k-1)  = unit_head(jufir+k-1)
c-3/5        unit_head(jufir+k-1)  = 0.0
        unit_rev(nutot+k-1)   = unit_rev(jufir+k-1)
c-3/5        unit_rev(jufir+k-1)   = 0.0
        unit_perc(nutot+k-1)  = unit_perc(jufir+k-1)
c-3/5        unit_perc(jufir+k-1)  = 0.0
        unit_eff(nutot+k-1)   = unit_eff(jufir+k-1)
c-3/5        unit_eff(jufir+k-1)   = 0.0
        unit_power(nutot+k-1) = unit_power(jufir+k-1)
c-3/5        unit_power(jufir+k-1) = 0.0
        unit_temp(nutot+k-1)  = unit_temp(jufir+k-1)
cgpe        unit_temp(jufir+k-1)  = 0.0
c-3/5        unit_temp(jufir+k-1)  = t0
        unit_max(nutot+k-1)   = unit_max(jufir+k-1)
c-3/5        unit_max(jufir+k-1)   = 0.0
        unit_min(nutot+k-1)   = unit_min(jufir+k-1)
c-3/5        unit_min(jufir+k-1)   = 0.0
        unit_hrate(nutot+k-1) = unit_hrate(jufir+k-1) 
c-3/5        unit_hrate(jufir+k-1) = 0.0
        unit_cons(nutot+k-1)  = unit_cons(jufir+k-1)
c-3/5        unit_cons(jufir+k-1)  = 0.0

      enddo
c
      return
      end
