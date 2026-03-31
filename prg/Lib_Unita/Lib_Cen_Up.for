c.......................................................................
c  pacchetto contenente le routines per la simulazione di una stazione
c  mista con criterio di uguaglianza delle portate
c.......................................................................
	Subroutine Single_Station_up(istaz,vert_pointer,tipo_criterio,
     *                  flow2,pres1,pres2,temp1,temp2,air,comp_ratio,
     *                  tot_cons,head,delpr1,delpr2,flowm,flow_ric,
     *                  type_num,unit_num,jufir,jtfir,stat_vars,
     *                  stat_ord,ier)
c
c***********************************************************************
c simulazione di una stazione elementare di tipo misto con criterio
c di uguaglianza delle portate
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
      call Station_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,
cgpe-new
     *     nom_flow(jufir),
cgpe-new-end
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir)
     -    ,unit_flow_rat(jufir)
     -    ,unit_eff_corr(jufir),unit_eff(jufir),
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


      ac_ce_stat=ac_ce_stat_M

	ac1_stat=ac1_stat_M
  
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


 
c---->                                       simulazione stazione
      call Station_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,
cgpe-new
     *     nom_flow(jufir),
cgpe-new-end
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir)
     -    ,unit_flow_rat(jufir)
     -    ,unit_eff_corr(jufir),unit_eff(jufir),
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

c---->                                       simulazione stazione
      call Station_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant(jtfir),unit_num,lstat,
cgpe-new
     *     nom_flow(jufir),
cgpe-new-end
     *     unit_perc(jufir),unit_rev(jufir),unit_flow(jufir),
     *     unit_head(jufir)
     -    ,unit_flow_rat(jufir)
     -    ,unit_eff_corr(jufir),unit_eff(jufir),
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

C--->    gestione errore
cerr      if (ier .gt. 0) then
cerr        if (ier .le. 5 .or. ier .eq. 38) then
!- ier=1: non esistono punti fattibili (al min_rev viene superata la max power
!- ier=4: portata negativa
!- ier=38: pressione di ingresso o di uscita negativa
!- ier=5: pressione di ingresso supera la pressione di uscita
!- Non e' possibile portare il punto dentro le mappe, ma e' stato imposto
!- un nuovo stato, pertanto sono da sono da ripristinare i vecchi CIN
cerr          goto 11

cerr        else if (ier .eq. 6 .or. ier .eq. 7) then
!- ier=6: altezza adiabatica superiore alla massima fattibile
!- ier=7: altezza adiabatica inferiore alla minima fattibile
cerr          if (nit . lt. nitmax) then
!- viene portato il punto dentro le mappe:
!- le nuove condizioni di input alla station sono rappresentate da
!- PRES1/ PRES2/ FLOW2
cerr            nit = nit + 1
cerr            goto 10
cerr          else
!- se la correzione del punto di lavoro termina con errore, nella
!- optim_vertex viene dato il messaggio che e' stato raggiunto il
!- massimo numero di iterazioni (ier>0 e nit=nitmax)
!            return
cerr            goto 11
cerr          end if

cerr        else if (ier.eq.8 .or. ier.eq.20) then
!- viene assegnata la portata massima/minima fattibile da ciascuna unita'
!- (sono stati calcolati i dati di unita')
cerr          if (nit .lt. nitmax) then
!- se l'errore non deriva dall'aver portato il punto di lavoro dentro le
!- mappe, si esce (non sono state modificate le condizioni iniziali)
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
c
c------------------------------------------------------------------------------
c
      Subroutine Station_up(vert_pointer,flow,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,comp_ratio,tot_cons,flowm,flow_ric,
     *     head,type_quant,unit_num,status,
cgpe-new
     *     nom_flow,
cgpe-new-end
     *     unit_perc,unit_rev,unit_flow,unit_head,
     *     unit_flow_rat,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_vars,
     *     unit2_perc,unit2_rev,
     *     unit2_flow,unit2_head,unit2_eff_corr,unit2_eff,
     *     unit2_power_corr,unit2_power,unit2_temp,unit2_max,
     *     unit2_min,unit_bifase,IER) 
c
c	Simulazione di stazione mista, con bifase in parallelo, 
c     con il criterio di controllo di ripartizione delle portate
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
      include '../inc/simpstat_aux.INC'
      include '../inc/flag.INC'
c-UDM
      include '../inc/conv.INC'
c-UDM-end
c
      COMMON/MAR/istaz,ppmol
	integer*4 istaz
	real*4 ppmol

cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat

	

cmar_DIN
	 COMMON/A/am
	integer*4 am
cmar_DIN


      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4 nom_flow(*)
      real*4 flow,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),unit_rev(*),
     *       unit_max(*),unit_min(*),unit_flow(*)
     *      ,unit_flow_rat(*) !coeff. di ripartiz. Q per ogni compr attivo
     *      ,unit_eff_corr(*),
     *       unit_eff(*),unit_head(*),unit_power_corr(*),unit_power(*),
     *       unit_hrate(*),unit_cons(*),unit_temp(*),stat_vars(*),
     *       delpr1,delpr2,flowm,head,unit_vcrit(*),
     *       unit_hrate_corr(*)
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
      real*4    qass(maxunits)
      real*4    qass2(maxunits)
      integer*4 tip_vinc(maxunits) ! tipo di vincolo da rispettare: 
      integer*4 icj,ij,nutot,numact
      real*4    tot_flow
      real*4    eps_q/1.e-3/

      real*4   PRES_OLD
      real*4   EESP, QAUX
      real*4   eps_p/0.05/
      real*4   rdummy
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
      ker = 0
      l_ric = .true.
c
cgpe      if (flow.le.0.) then
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
!gz
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
        if (iopt .or. iopt_mess) return
        if (fl_sim) then
c-UDM          write(mess,557) head,hmax,UDM_INT_H ! write(mess,557) head,hmax
c-UDM557       format('Prevalenza ad: ',f6.3,' - Prevalenza ad max:',f6.3,'  ',A4)
          write(mess,557) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmax,1)
557   format('Prevalenza ad: ',A10,' - Prevalenza ad max: ',A25)
          call app_mess(vert_pointer,ier,mess)
        endif
        RETURN

      else if (head.lt.hmin) then
c errore: prevalenza richiesta inferiore alla mimima fattibile
        ier = 7 ! ier = 6
        if (iopt .or. iopt_mess) return
        if (fl_sim) then
c-UDM          write(mess,559) head,hmin,UDM_INT_H ! write(mess,559) head,hmin
c-UDM559       format('Prevalenza ad: ',f6.3,' - Prevalenza ad min:',f6.3,'  ',A4)
          write(mess,559) udm_out_str(udm_H,head,0),
     *                    udm_out_str(udm_H,hmin,1)
559   format('Prevalenza ad: ',A10,' - Prevalenza ad min: ',A25)

          call app_mess(vert_pointer,ier,mess)
        endif
        RETURN

      endif

	call portate_criterioP_parall (vert_pointer 
     -                                   ,head
     -                                   ,flowm
     -                                   ,type_quant
     -                                   ,unit_flow_rat
     -                                   ,qmin_assoluto
     -                                   ,qmin_assoluto2
     -                                   ,qmax_assoluto
     -                                   ,qmax_assoluto2
     -                                   ,tip_vinc
     -                                   ,qass
     -                                   ,qass2
     -								   ,l_ric
     -								   ,ier)
	if (ier.ne.0) return
c --- FINE SIMULAZIONE
c
c note per ogni macchina: head, qass (o qass2), vinc_max, vinc_min 
c calcolo le altre grandezze. Uso type_act_old
77    continue
c problema: dati head e qass per ogni tipo, calcolare le altre grandezze
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

ccccccccccccccccc_312      min_p=min_p+type_vinc_minpow(itip)
ccccccccccccccccc_312      max_p=max_p+type_vinc_maxpow(itip)
    
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

	
cmar_08_05      
          n_calc=n_calc+1
cmar_08_05   


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
                if (.not.iopt .and. .not.iopt_mess) then
cgpe-new                 if (fl_sim) then
cgpe-mess                  call gest_error (2,0,'STATION_UP',' Violata la max_power',0)
cgpe-mess                  mess = ' Violata la max_power'
cgpe-mess                  ker = 14
                  ker = 15
                  call app_mess(vert_pointer,ker,mess)
cgpe-new                 end if
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
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)
        if (unit_bifase(icj)) tot_flow = tot_flow+unit2_flow(icj)
      end do

cgpe      flow_ric = 0.
      flow_ric = tot_flow/aux3
      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow.lt.flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
          if (iopt .or. iopt_mess) return
c-UDM          write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q
c-UDM560     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
560     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
          write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                    udm_out_str(udm_Q,flowm/aux3,1)
          call app_mess(vert_pointer,ier,mess)
        else if (tot_flow.gt.flowm) then
c tutte le unita' sono in riciclo
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
c-bpcorr		    ier = 0
cgpe              flow_ric = tot_flow/aux3
cgpe-mess              call gest_error (2,0,'','La centrale e'' in riciclo',0)
c-UDM              write(mess,560) tot_flow/aux3,flowm/aux3,UDM_INT_Q ! write(mess,560) (tot_flow*conv_ns)/aux3,(flowm*conv_ns)/aux3
             if (.not.iopt_mess) then
              write(mess,560) udm_out_str(udm_Q,tot_flow/aux3,0),
     *                        udm_out_str(udm_Q,flowm/aux3,1)
              call app_mess(vert_pointer,ier,mess)
             endif
		    ier = 0
            endif
!!            flow_ric = tot_flow-flow_tot
!            flow_ric = (tot_flow-flow_tot)/aux3
c-l_ric          endif
        endif
      endif

      if (iopt_mess) then
        do icj=1,unit_num
          if(status(icj)) then
            unit_perc(icj) = unit_flow(icj)/tot_flow
            if (unit_bifase(icj)) then
              unit2_perc(icj) = unit2_flow(icj)/tot_flow
            endif
	    end if
	  end do

        temp2 = 0.
c gz !!!! attenzione !!!!
c per ora non si calcola il fattore eff_coef (cioe' k)
c-ace   eff_coef = 0
        call out_temper_mf(
c_ecol
     *           tair,
c_ecol
     *           temp2,temp1,Z,pin,pout,aux2,
     *           z_coef(1,vert_pointer),
c-ace*           eff_coef,unit_num,status,
     *           dh_coef(1,ind),cp_par(1,ind),unit_num,status,
     *	       unit_perc,unit_eff,
c_ecol              
     *           unit_flow,
c_ecol
     *           unit_temp,unit2_perc,unit2_eff,
c_ecol               
     *           unit2_flow,
c_ecol
     *           unit2_temp,unit_bifase,stat_vars(7))
cc-------->modifica relativa al delta dell'ac
c
c ---> sottraggo la caduta di temperatura dovuta al piping

      temp2 =temp2 -stat_vars(5)
	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7),pres1,temp1
     * ,temp2)  !new

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

c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c !!!!!!!!!!!!! G Z !!!!!!!!!!!!!!!!!!!!!!
c !controllare la out_temper  !!!!!!!!!!!!
c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
        return
      endif

!      if (.not. .and. .not.iopt_mess) then
      if (.not.iopt) then
        do icj=1,unit_num
          if(status(icj)) then
            unit_perc(icj) = unit_flow(icj)/tot_flow
            if (unit_bifase(icj)) then
              unit2_perc(icj) = unit2_flow(icj)/tot_flow
            endif
	    end if
	  end do
        temp2 = 0.
c gz !!!! attenzione !!!!
c per ora non si calcola il fattore eff_coef (cioe' k)
c-ace        eff_coef = 0
         call out_temper_mf(
c_ecol
     *           tair,
c_ecol
     *           temp2,temp1,Z,pin,pout,aux2,
     *           z_coef(1,vert_pointer),
c-ace*           eff_coef,unit_num,status,
     *           dh_coef(1,ind),cp_par(1,ind),unit_num,status,
     *	       unit_perc,unit_eff,
c_ecol
     *           unit_flow,
c_ecol
     *           unit_temp,unit2_perc,unit2_eff,
c_ecol
     *           unit2_flow,
c_ecol
     *           unit2_temp,unit_bifase,stat_vars(7))
c_ecol

c-------->modifica relativa al delta dell'ac
c
c ---> sottraggo la caduta di temperatura dovuta al piping

      temp2 =temp2 -stat_vars(5)
	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7),pres1,temp1
     * ,temp2)  !new


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
c-UDM	     write (mess,570) temp2,stat_vars(7),UDM_INT_T ! write (mess,570) temp2-t0,stat_vars(7)-t0
           write(mess,570) udm_out_str(udm_T,temp2,0),
     *                     udm_out_str(udm_T,stat_vars(7),1)
           call app_mess(vert_pointer,ker,mess)
	  end if
cmar_AC_auto
      ENDIF
cmar_AC_auto
c_ecol------------------------------------------------------------------------
c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c !!!!!!!!!!!!! G Z !!!!!!!!!!!!!!!!!!!!!!
c !controllare la out_temper  !!!!!!!!!!!!
c !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      end if
c


  
      return
      end

      Subroutine limiti_q_bifase_OK_ORIG (head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)
c
c calcola la portata totale perche' tutte le unita' della stazione siano
c esattamente in riciclo (qmin_tot) e la portata totale perche' tutte le
c macchine lavorino al loro massimo (qmax_tot)
c Ricerca quindi il valore piu' limitante fra quelli trovati: e' il
c valore con la percentuale rispetto al riciclo piu' bassa
c Restituisce comunque per ogni tipo la percentuale rispetto al riciclo
c
c       head            I   prevalenza della stazione
c       min_rev         I   numero di giri minimo per ciascun tipo
c       max_rev         I   numero di giri massimo per ciascun tipo
c       tipo_surge      I   tipo di surge per ciascun tipo
c                         1 = spezzata
c                         2 = parabola
c       qmin_tot        O   portata totale con tutti i compressori sulla
c                           curva di riciclo
c       qmin_assoluto   O   portata minima di ciascun tipo della prima fase
c       qmin_assoluto2  O   portata minima di ciascun tipo della seconda fase
c       qmax_assoluto   O   portata massima di ciascun tipo della prima fase
c       qmax_assoluto2  O   portata massima di ciascun tipo della seconda fase
c       qmax_tot        O   portata massima totale di tutte le macchine attive
c       vinc_max        O   tipo di vincolo di massima per ciascun tipo:
c                       mar_1f    5 = max_rev   (prima fase)
c                       cho_1f    6 = choking   (prima fase)
c                       mar_2f    7 = max_rev   (seconda fase)
c                       cho_2f    8 = choking   (seconda fase)
c                       pow_1f    9 = max_power 
c       vinc_min        O   tipo di vincolo di minima per ciascun tipo:
c                       sur_1f    1 = anti-surge (prima fase)
c                       mir_1f    2 = min_rev    (prima fase)
c                       sur_2f    3 = anti-surge (seconda_fase)
c                       mir_2f    4 = min_rev    (seconda fase)

c----------------------------------------------------------------------
        implicit none
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
c
        real*4    head

        real*4    qmin_assoluto(*)
        real*4    qmin_assoluto2(*)
        real*4    qmax_assoluto(*)
        real*4    qmax_assoluto2(*)
        real*4    qmin_tot,qmax_tot
c
        INTEGER*4 itip
        real*4    qaux1_max,qaux2_max,rev_min,rev_max

        real*4    raux
        integer*4 vinc_max(*),vinc_min(*)
        real*4    fa,fb,rev0,rev
        real*4    qmin1,qmax1,qaux
        real*4    qmin2,qmax2,rev2
        real*4    zero_pow_mf
        external  zero_pow_mf
c
        integer*4 vinc_min1,vinc_min2,vinc_max1,vinc_max2
        real*4    min_rev1,min_rev2,max_rev1,max_rev2
        real*4    raux_min,raux_max
        integer*4 ier
c---------------------------------------------------------------------
      qmin_tot=0
      qmax_tot=0
      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c
c   Inizializzazioni
c
          qaux1_max = 0.
          qaux2_max = 0.
          qmax_assoluto(itip) = 0.
          qmax_assoluto2(itip) = 0.
          qmin_assoluto(itip) = 0.
          qmin_assoluto2(itip) = 0.
c
          call limiti_flow_1f(head,qmin1,qmax1,min_rev1,
     *         max_rev1,vinc_min1,vinc_max1,itip)
           if (type_bifase(itip)) then
           call limiti_flow_2f(head,qmin2,qmax2,min_rev2,
     *           max_rev2,vinc_min2,vinc_max2,itip)
c raux_min/raux_max sono i giri sulla prima fase corrispondenti a
c min_rev2/max_rev2
            raux_min = min_rev2 * type2_ratio(itip)/type_ratio(itip)
            raux_max = max_rev2 * type2_ratio(itip)/type_ratio(itip)
c
c  R I C E R C A   D E L   M I N I M O
c
            if (min_rev1.ge.raux_min) then
              qmin_assoluto(itip) = qmin1
              rev_min = min_rev1
              vinc_min(itip) = vinc_min1
c calcolo il punto sulla seconda fase
              rev2 = min_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              qmin_assoluto2(itip) = qaux
            else
              qmin_assoluto2(itip) = qmin2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_min,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
              qmin_assoluto(itip) = qaux
              rev_min = raux_min
              vinc_min(itip) = vinc_min2
            end if
c
c  R I C E R C A   D E L   M A S S I M O
c
            if (max_rev1.le.raux_max) then
              qmax_assoluto(itip) = qmax1
              rev_max = max_rev1
              vinc_max(itip) = vinc_max1
c calcolo il punto sulla seconda fase
              rev2 = max_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              qmax_assoluto2(itip) = qaux
            else
              qmax_assoluto2(itip) = qmax2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_max,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
              qmax_assoluto(itip) = qaux
              rev_max = raux_max
              vinc_max(itip) = vinc_max2
            end if
          else
            qmin_assoluto(itip) = qmin1
            qmax_assoluto(itip) = qmax1
            vinc_min(itip) = vinc_min1
            vinc_max(itip) = vinc_max1
            rev_min = min_rev1
            rev_max = max_rev1
          end if
c
c  R I C E R C A   D E L L A   M A S S I M A   P O T E N Z A
c
          itipo = itip
          fb = zero_pow_mf(rev_max)
          if (abs(fb).le.eps_pow .or. fb.lt.0.) then
c il punto massimo e' vincolato dalla max_power
            Fa = zero_pow_mf(rev_min)
            if ( (abs(fa).le.eps_pow) .or.
     *         (abs(fb).le.eps_pow))   then
              if (abs(fa).le.eps_pow) then
                rev = rev_min
              else if (abs(fb).le.eps_pow) then
                rev = rev_max
              endif
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else if (fa.gt.0) then
c---->              e' possibile trovare una soluzione
              rev0 = (rev_min+rev_max)/2.
              call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *                  rev,epsmi,pmach,zero_pow_mf)
              rev = rev-epsmi
c ricerca del qmax sulla max_power per la prima e la seconda fase
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else
c  Errore: non e' possibile trovare una soluzione
cgpe???              ier = 42
cgpe          ier = 61
              ier = 18
              if (iopt .or. iopt_mess) return
cgpe-new                call gest_error (2,0,'LIMITI_Q_BIFASE',
cgpe-new     *               'Al min_rev si supera la max_power',0)
              return
            end if
c le due fasi vengono limitate dalla massima potenza a rev della max_power
            qmax_assoluto(itip)=qaux1_max
            qmax_assoluto2(itip)=0.
            if (type_bifase(itip)) qmax_assoluto2(itip)=qaux2_max
            vinc_max(itip)=pow_1f
            rev_max = rev
            max_rev2 = 0.
            if (type_bifase(itip))
     *        max_rev2 = rev*type_ratio(itip)/type2_ratio(itip)
          endif
c
          qmin_tot = qmin_tot + qmin_assoluto(itip)*type_actr(itip) +
     *               qmin_assoluto2(itip)*type_actr(itip)
c
          qmax_tot = qmax_tot + qmax_assoluto(itip)*type_actr(itip) +
     *               qmax_assoluto2(itip)*type_actr(itip)
        endif
      enddo
c
      return
      end
CMAR ++++++++++++++++++******************************************************

CMAR_ASS_COND+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      Subroutine limiti_q_bifase_AS(vect,head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)
c
c calcola la portata totale perche' tutte le unita' della stazione siano
c esattamente in riciclo (qmin_tot) e la portata totale perche' tutte le
c macchine lavorino al loro massimo (qmax_tot)
c Ricerca quindi il valore piu' limitante fra quelli trovati: e' il
c valore con la percentuale rispetto al riciclo piu' bassa
c Restituisce comunque per ogni tipo la percentuale rispetto al riciclo
c
c       head            I   prevalenza della stazione
c       min_rev         I   numero di giri minimo per ciascun tipo
c       max_rev         I   numero di giri massimo per ciascun tipo
c       tipo_surge      I   tipo di surge per ciascun tipo
c                         1 = spezzata
c                         2 = parabola
c       qmin_tot        O   portata totale con tutti i compressori sulla
c                           curva di riciclo
c       qmin_assoluto   O   portata minima di ciascun tipo della prima fase
c       qmin_assoluto2  O   portata minima di ciascun tipo della seconda fase
c       qmax_assoluto   O   portata massima di ciascun tipo della prima fase
c       qmax_assoluto2  O   portata massima di ciascun tipo della seconda fase
c       qmax_tot        O   portata massima totale di tutte le macchine attive
c       vinc_max        O   tipo di vincolo di massima per ciascun tipo:
c                       mar_1f    5 = max_rev   (prima fase)
c                       cho_1f    6 = choking   (prima fase)
c                       mar_2f    7 = max_rev   (seconda fase)
c                       cho_2f    8 = choking   (seconda fase)
c                       pow_1f    9 = max_power 
c       vinc_min        O   tipo di vincolo di minima per ciascun tipo:
c                       sur_1f    1 = anti-surge (prima fase)
c                       mir_1f    2 = min_rev    (prima fase)
c                       sur_2f    3 = anti-surge (seconda_fase)
c                       mir_2f    4 = min_rev    (seconda fase)

c----------------------------------------------------------------------
        implicit none
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
c

cmar 
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
      real*4 perc_equi_min, perc_equi_max

        real*4    head

        real*4    qmin_assoluto(*)
        real*4    qmin_assoluto2(*)
        real*4    qmax_assoluto(*)
        real*4    qmax_assoluto2(*)
        real*4    qmin_tot,qmax_tot
c
        INTEGER*4 itip
        real*4    qaux1_max,qaux2_max,rev_min,rev_max

        real*4    raux
        integer*4 vinc_max(*),vinc_min(*)
        real*4    fa,fb,rev0,rev
        real*4    qmin1,qmax1,qaux
        real*4    qmin2,qmax2,rev2
        real*4    zero_pow_mf
        external  zero_pow_mf
c
        integer*4 vinc_min1,vinc_min2,vinc_max1,vinc_max2
        real*4    min_rev1,min_rev2,max_rev1,max_rev2
        real*4    raux_min,raux_max
        integer*4 ier
cmar
       integer*4 tipo_criterio
cmar
cmar_ass_cond
       integer*4 vect(maxunits), type_actr_(maxunits)
cmar_ass_cond
c---------------------------------------------------------------------
      qmin_tot=0
      qmax_tot=0
cmar_ass_cond
	do itip = 1,ntp
      type_actr_(itip)=type_actr(itip)
	qmax_assoluto(itip) = 0.
      qmax_assoluto2(itip) = 0.
      qmin_assoluto(itip) = 0.
      qmin_assoluto2(itip) = 0.
	enddo

cmar_ass_cond
	do itip = 1,ntp

cmar_per sviluppo
ccc	 type_actr(itip)=1

cmar_per_sviluppo


      type_actr(itip)=type_actr(itip)*vect(itip)

	enddo


      do itip = 1,ntp
cmar_ass_cond: modificata condizione nell' if


      


        if( type_actr(itip).gt.0) then
c
c   Inizializzazioni
c
          qaux1_max = 0.
          qaux2_max = 0.
          qmax_assoluto(itip) = 0.
          qmax_assoluto2(itip) = 0.
          qmin_assoluto(itip) = 0.
          qmin_assoluto2(itip) = 0.
c
cmar         if (tipo_criterio.eq.crit_equi) then

c          call limiti_flow_1f_equi(head,qmin1,qmax1,min_rev1,
c     *         max_rev1,vinc_min1,vinc_max1,itip)
cmar	  else 
	 call limiti_flow_1f(head,qmin1,qmax1,min_rev1,
     *         max_rev1,vinc_min1,vinc_max1,itip)
cmar        end if

           if (type_bifase(itip)) then
           
cmar           call limiti_flow_2f_equi(head,qmin2,qmax2,min_rev2,
cmar     *           max_rev2,vinc_min2,vinc_max2,itip)
cmar	     else

	     call limiti_flow_2f(head,qmin2,qmax2,min_rev2,
     *           max_rev2,vinc_min2,vinc_max2,itip)

cmar	     end if
		    
c raux_min/raux_max sono i giri sulla prima fase corrispondenti a
c min_rev2/max_rev2
            raux_min = min_rev2 * type2_ratio(itip)/type_ratio(itip)
            raux_max = max_rev2 * type2_ratio(itip)/type_ratio(itip)
c
c  R I C E R C A   D E L   M I N I M O
c
cmar
!        if (tipo_criterio.eq.crit_equi) then
cmar
            if (min_rev1.ge.raux_min) then
              qmin_assoluto(itip) = qmin1
              rev_min = min_rev1
              vinc_min(itip) = vinc_min1
c calcolo il punto sulla seconda fase
              rev2 = min_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
cmar
	        if(qaux.lt.qmin2)qaux= qaux*(1+perc_equi_min(itip)) 
cmar
              qmin_assoluto2(itip) = qaux
            else
              qmin_assoluto2(itip) = qmin2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_min,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
cmar
             if(qaux .lt.qmin1) qaux= qaux*(1+perc_equi_min(itip)) 
cmar
              qmin_assoluto(itip) = qaux
              rev_min = raux_min
              vinc_min(itip) = vinc_min2
            end if
c
c  R I C E R C A   D E L   M A S S I M O
c
            if (max_rev1.le.raux_max) then
              qmax_assoluto(itip) = qmax1
              rev_max = max_rev1
              vinc_max(itip) = vinc_max1
c calcolo il punto sulla seconda fase
              rev2 = max_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
cmar
       if(qaux.gt.qmax2)qaux= qaux*(1-perc_equi_max(itip)) 
cmar
              qmax_assoluto2(itip) = qaux
            else
              qmax_assoluto2(itip) = qmax2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_max,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
cmar
      if(qaux .gt.qmax1) qaux= qaux*(1-perc_equi_max(itip))
cmar
              qmax_assoluto(itip) = qaux
              rev_max = raux_max
              vinc_max(itip) = vinc_max2
            end if
          else
            qmin_assoluto(itip) = qmin1
            qmax_assoluto(itip) = qmax1
            vinc_min(itip) = vinc_min1
            vinc_max(itip) = vinc_max1
            rev_min = min_rev1
            rev_max = max_rev1
          end if
cmar
!      else
cmar
 
c
c  R I C E R C A   D E L L A   M A S S I M A   P O T E N Z A
c
          itipo = itip
          fb = zero_pow_mf(rev_max)
          if (abs(fb).le.eps_pow .or. fb.lt.0.) then
c il punto massimo e' vincolato dalla max_power
            Fa = zero_pow_mf(rev_min)
            if ( (abs(fa).le.eps_pow) .or.
     *         (abs(fb).le.eps_pow))   then
              if (abs(fa).le.eps_pow) then
                rev = rev_min
              else if (abs(fb).le.eps_pow) then
                rev = rev_max
              endif
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else if (fa.gt.0) then
c---->              e' possibile trovare una soluzione
              rev0 = (rev_min+rev_max)/2.
              call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *                  rev,epsmi,pmach,zero_pow_mf)
              rev = rev-epsmi
c ricerca del qmax sulla max_power per la prima e la seconda fase
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else
c  Errore: non e' possibile trovare una soluzione
cgpe???              ier = 42
cgpe          ier = 61
              ier = 18
              if (iopt .or. iopt_mess) return
cgpe-new                call gest_error (2,0,'LIMITI_Q_BIFASE',
cgpe-new     *               'Al min_rev si supera la max_power',0)
              return
            end if
c le due fasi vengono limitate dalla massima potenza a rev della max_power
            qmax_assoluto(itip)=qaux1_max
            qmax_assoluto2(itip)=0.
            if (type_bifase(itip)) qmax_assoluto2(itip)=qaux2_max
            vinc_max(itip)=pow_1f
            rev_max = rev
            max_rev2 = 0.
            if (type_bifase(itip))
     *        max_rev2 = rev*type_ratio(itip)/type2_ratio(itip)
          endif
          qmin_tot = qmin_tot + qmin_assoluto(itip)*type_actr(itip) +
     *               qmin_assoluto2(itip)*type_actr(itip)
c
          qmax_tot = qmax_tot + qmax_assoluto(itip)*type_actr(itip) +
     *               qmax_assoluto2(itip)*type_actr(itip)

        endif

      enddo

cmar_ass_cond
	do itip = 1,ntp
      type_actr(itip)=type_actr_(itip)
	enddo
cmar_ass_cond
c
      return
      end



CMAR_ASS_COND++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++     
      Subroutine limiti_q_bifase (head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)
c
c calcola la portata totale perche' tutte le unita' della stazione siano
c esattamente in riciclo (qmin_tot) e la portata totale perche' tutte le
c macchine lavorino al loro massimo (qmax_tot)
c Ricerca quindi il valore piu' limitante fra quelli trovati: e' il
c valore con la percentuale rispetto al riciclo piu' bassa
c Restituisce comunque per ogni tipo la percentuale rispetto al riciclo
c
c       head            I   prevalenza della stazione
c       min_rev         I   numero di giri minimo per ciascun tipo
c       max_rev         I   numero di giri massimo per ciascun tipo
c       tipo_surge      I   tipo di surge per ciascun tipo
c                         1 = spezzata
c                         2 = parabola
c       qmin_tot        O   portata totale con tutti i compressori sulla
c                           curva di riciclo
c       qmin_assoluto   O   portata minima di ciascun tipo della prima fase
c       qmin_assoluto2  O   portata minima di ciascun tipo della seconda fase
c       qmax_assoluto   O   portata massima di ciascun tipo della prima fase
c       qmax_assoluto2  O   portata massima di ciascun tipo della seconda fase
c       qmax_tot        O   portata massima totale di tutte le macchine attive
c       vinc_max        O   tipo di vincolo di massima per ciascun tipo:
c                       mar_1f    5 = max_rev   (prima fase)
c                       cho_1f    6 = choking   (prima fase)
c                       mar_2f    7 = max_rev   (seconda fase)
c                       cho_2f    8 = choking   (seconda fase)
c                       pow_1f    9 = max_power 
c       vinc_min        O   tipo di vincolo di minima per ciascun tipo:
c                       sur_1f    1 = anti-surge (prima fase)
c                       mir_1f    2 = min_rev    (prima fase)
c                       sur_2f    3 = anti-surge (seconda_fase)
c                       mir_2f    4 = min_rev    (seconda fase)

c----------------------------------------------------------------------
        implicit none
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
c

cmar 
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
      real*4 perc_equi_min, perc_equi_max

        real*4    head

        real*4    qmin_assoluto(*)
        real*4    qmin_assoluto2(*)
        real*4    qmax_assoluto(*)
        real*4    qmax_assoluto2(*)
        real*4    qmin_tot,qmax_tot
c
        INTEGER*4 itip
        real*4    qaux1_max,qaux2_max,rev_min,rev_max

        real*4    raux
        integer*4 vinc_max(*),vinc_min(*)
        real*4    fa,fb,rev0,rev
        real*4    qmin1,qmax1,qaux
        real*4    qmin2,qmax2,rev2
        real*4    zero_pow_mf
        external  zero_pow_mf
c
        integer*4 vinc_min1,vinc_min2,vinc_max1,vinc_max2
        real*4    min_rev1,min_rev2,max_rev1,max_rev2
        real*4    raux_min,raux_max
        integer*4 ier
cmar
       integer*4 tipo_criterio
cmar
c---------------------------------------------------------------------
      qmin_tot=0
      qmax_tot=0
      do itip = 1,ntp

        if(type_actr(itip).gt.0) then
    

c
c   Inizializzazioni
c
          qaux1_max = 0.
          qaux2_max = 0.
          qmax_assoluto(itip) = 0.
          qmax_assoluto2(itip) = 0.
          qmin_assoluto(itip) = 0.
          qmin_assoluto2(itip) = 0.
c
cmar         if (tipo_criterio.eq.crit_equi) then

c          call limiti_flow_1f_equi(head,qmin1,qmax1,min_rev1,
c     *         max_rev1,vinc_min1,vinc_max1,itip)
cmar	  else 
	 call limiti_flow_1f(head,qmin1,qmax1,min_rev1,
     *         max_rev1,vinc_min1,vinc_max1,itip)
	
cmar        end if

           if (type_bifase(itip)) then
           
cmar           call limiti_flow_2f_equi(head,qmin2,qmax2,min_rev2,
cmar     *           max_rev2,vinc_min2,vinc_max2,itip)
cmar	     else

	     call limiti_flow_2f(head,qmin2,qmax2,min_rev2,
     *           max_rev2,vinc_min2,vinc_max2,itip)

cmar	     end if
		    
c raux_min/raux_max sono i giri sulla prima fase corrispondenti a
c min_rev2/max_rev2
            raux_min = min_rev2 * type2_ratio(itip)/type_ratio(itip)
            raux_max = max_rev2 * type2_ratio(itip)/type_ratio(itip)
c
c  R I C E R C A   D E L   M I N I M O
c
cmar
!        if (tipo_criterio.eq.crit_equi) then
cmar
            if (min_rev1.ge.raux_min) then
              qmin_assoluto(itip) = qmin1
              rev_min = min_rev1
              vinc_min(itip) = vinc_min1
c calcolo il punto sulla seconda fase
              rev2 = min_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
cmar
	        if(qaux.lt.qmin2)qaux= qaux*(1+perc_equi_min(itip)) 
cmar
              qmin_assoluto2(itip) = qaux
            else
              qmin_assoluto2(itip) = qmin2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_min,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
cmar
             if(qaux .lt.qmin1) qaux= qaux*(1+perc_equi_min(itip)) 
cmar
              qmin_assoluto(itip) = qaux
              rev_min = raux_min
              vinc_min(itip) = vinc_min2
            end if
c
c  R I C E R C A   D E L   M A S S I M O
c
            if (max_rev1.le.raux_max) then
              qmax_assoluto(itip) = qmax1
              rev_max = max_rev1
              vinc_max(itip) = vinc_max1
c calcolo il punto sulla seconda fase
              rev2 = max_rev1*type_ratio(itip)/type2_ratio(itip)
              call qdahn2(head,qaux,rev2,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
cmar
       if(qaux.gt.qmax2)qaux= qaux*(1-perc_equi_max(itip)) 
cmar
              qmax_assoluto2(itip) = qaux
            else
              qmax_assoluto2(itip) = qmax2
c calcolo il punto sulla prima fase
              call qdahn2(head,qaux,raux_max,clim(3,itip),
     *             chn(1,itip),chc(1,itip))
cmar
      if(qaux .gt.qmax1) qaux= qaux*(1-perc_equi_max(itip))
cmar
              qmax_assoluto(itip) = qaux
              rev_max = raux_max
              vinc_max(itip) = vinc_max2
            end if
          else
            qmin_assoluto(itip) = qmin1
            qmax_assoluto(itip) = qmax1
            vinc_min(itip) = vinc_min1
            vinc_max(itip) = vinc_max1
            rev_min = min_rev1
            rev_max = max_rev1
          end if
cmar
!      else
cmar
 
c
c  R I C E R C A   D E L L A   M A S S I M A   P O T E N Z A
c
          itipo = itip
          fb = zero_pow_mf(rev_max)
          if (abs(fb).le.eps_pow .or. fb.lt.0.) then
c il punto massimo e' vincolato dalla max_power
            Fa = zero_pow_mf(rev_min)
            if ( (abs(fa).le.eps_pow) .or.
     *         (abs(fb).le.eps_pow))   then
              if (abs(fa).le.eps_pow) then
                rev = rev_min
              else if (abs(fb).le.eps_pow) then
                rev = rev_max
              endif
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else if (fa.gt.0) then
c---->              e' possibile trovare una soluzione
              rev0 = (rev_min+rev_max)/2.
              call zero_fun(rev_min,rev_max,rev0,fa,fb,
     *                  rev,epsmi,pmach,zero_pow_mf)
              rev = rev-epsmi
c ricerca del qmax sulla max_power per la prima e la seconda fase
              call qdahn2(head,qaux1_max,rev,clim(3,itip),chn(1,itip),
     *             chc(1,itip))
              if (type_bifase(itip)) then
                raux = rev*type_ratio(itip)/type2_ratio(itip)
                call qdahn2(head,qaux2_max,raux,clim2(3,itip),
     *             chn2(1,itip),chc2(1,itip))
              endif
            else
c  Errore: non e' possibile trovare una soluzione
cgpe???              ier = 42
cgpe          ier = 61
              ier = 18
              if (iopt .or. iopt_mess) return
cgpe-new                call gest_error (2,0,'LIMITI_Q_BIFASE',
cgpe-new     *               'Al min_rev si supera la max_power',0)
              return
            end if
c le due fasi vengono limitate dalla massima potenza a rev della max_power
            qmax_assoluto(itip)=qaux1_max
            qmax_assoluto2(itip)=0.
            if (type_bifase(itip)) qmax_assoluto2(itip)=qaux2_max
            vinc_max(itip)=pow_1f
            rev_max = rev
            max_rev2 = 0.
            if (type_bifase(itip))
     *        max_rev2 = rev*type_ratio(itip)/type2_ratio(itip)
          endif
c
          qmin_tot = qmin_tot + qmin_assoluto(itip)*type_actr(itip) +
     *               qmin_assoluto2(itip)*type_actr(itip)
c
          qmax_tot = qmax_tot + qmax_assoluto(itip)*type_actr(itip) +
     *               qmax_assoluto2(itip)*type_actr(itip)

        endif

      enddo
c     
      
      return
      end


cmar-eq2-----------------------------------------------------------------------



      Subroutine limiti_q_bifase_eq2 (head,qmin_assoluto,qmin_assoluto2)
c
c calcola la portata totale perche' tutte le unita' della stazione siano
c esattamente in riciclo (qmin_tot) e la portata totale perche' tutte le
c macchine lavorino al loro massimo (qmax_tot)
c Ricerca quindi il valore piu' limitante fra quelli trovati: e' il
c valore con la percentuale rispetto al riciclo piu' bassa
c Restituisce comunque per ogni tipo la percentuale rispetto al riciclo
c
c       head            I   prevalenza della stazione
c       min_rev         I   numero di giri minimo per ciascun tipo
c       max_rev         I   numero di giri massimo per ciascun tipo
c       tipo_surge      I   tipo di surge per ciascun tipo
c                         1 = spezzata
c                         2 = parabola
c       qmin_tot        O   portata totale con tutti i compressori sulla
c                           curva di riciclo
c       qmin_assoluto   O   portata minima di ciascun tipo della prima fase
c       qmin_assoluto2  O   portata minima di ciascun tipo della seconda fase
c       qmax_assoluto   O   portata massima di ciascun tipo della prima fase
c       qmax_assoluto2  O   portata massima di ciascun tipo della seconda fase
c       qmax_tot        O   portata massima totale di tutte le macchine attive
c       vinc_max        O   tipo di vincolo di massima per ciascun tipo:
c                       mar_1f    5 = max_rev   (prima fase)
c                       cho_1f    6 = choking   (prima fase)
c                       mar_2f    7 = max_rev   (seconda fase)
c                       cho_2f    8 = choking   (seconda fase)
c                       pow_1f    9 = max_power 
c       vinc_min        O   tipo di vincolo di minima per ciascun tipo:
c                       sur_1f    1 = anti-surge (prima fase)
c                       mir_1f    2 = min_rev    (prima fase)
c                       sur_2f    3 = anti-surge (seconda_fase)
c                       mir_2f    4 = min_rev    (seconda fase)

c----------------------------------------------------------------------
        implicit none
c
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
        include '../inc/SIMPSTAT.INC'
        include '../inc/MF_SIMPSTAT.INC'
c

cmar 
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
      real*4 perc_equi_min, perc_equi_max

        real*4    head

        real*4    qmin_assoluto(*)
        real*4    qmin_assoluto2(*)
c        real*4    qmax_assoluto(*)
c        real*4    qmax_assoluto2(*)
        real*4    qmin_tot,qmax_tot
c
        INTEGER*4 itip
        real*4    qaux1_max,qaux2_max,rev_min,rev_max

        real*4    raux
cc        integer*4 vinc_max(*),vinc_min(*)
        real*4    fa,fb,rev0,rev
        real*4    qmin1,qmax1,qaux
        real*4    qmin2,qmax2,rev2
        real*4    zero_pow_mf
        external  zero_pow_mf
c
        integer*4 vinc_min1,vinc_min2,vinc_max1,vinc_max2
        real*4    min_rev1,min_rev2,max_rev1,max_rev2
        real*4    raux_min,raux_max
        integer*4 ier
cmar
       integer*4 tipo_criterio
cmar
c---------------------------------------------------------------------
      qmin_tot=0
      qmax_tot=0
      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c
c   Inizializzazioni
c
          qaux1_max = 0.
          qaux2_max = 0.
c          qmax_assoluto(itip) = 0.
c          qmax_assoluto2(itip) = 0.
          qmin_assoluto(itip) = 0.
          qmin_assoluto2(itip) = 0.
c
cmar         if (tipo_criterio.eq.crit_equi) then

c          call limiti_flow_1f_equi(head,qmin1,qmax1,min_rev1,
c     *         max_rev1,vinc_min1,vinc_max1,itip)
cmar	  else 
	 call limiti_flow_1f(head,qmin1,qmax1,min_rev1,
     *         max_rev1,vinc_min1,vinc_max1,itip)


      if(vinc_min1 .eq.sur_1f)then
	qmin1=(qmin1)/(1+perc_equi_min(itip))
	endif


cmar        end if

           if (type_bifase(itip)) then
           
cmar           call limiti_flow_2f_equi(head,qmin2,qmax2,min_rev2,
cmar     *           max_rev2,vinc_min2,vinc_max2,itip)
cmar	     else

	     call limiti_flow_2f(head,qmin2,qmax2,min_rev2,
     *           max_rev2,vinc_min2,vinc_max2,itip)

	if(vinc_min2 .eq.sur_2f) then

	qmin2=(qmin2)/(1+perc_equi_min(itip))
      endif

              qmin_assoluto(itip) = qmin1
           
              qmin_assoluto2(itip) = qmin2


            
c          qmin_tot = qmin_tot + qmin_assoluto(itip)*type_actr(itip) +
c     *               qmin_assoluto2(itip)*type_actr(itip)
c
c          qmax_tot = qmax_tot + qmax_assoluto(itip)*type_actr(itip) +
c     *               qmax_assoluto2(itip)*type_actr(itip)

        
      endif
	endif

      enddo	

c
      return
      end

cmar-eq2--------------------------------------------------------------------




CMAR+++++++++++++++++++******************************************************
      subroutine limiti_flow_1f (head,qmin,qmax,revmin,revmax,
     *           vincmin,vincmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla prima fase, assegnata una
c prevalenza head, restituendo il numero di giri (revmin e revmax) e
c il tipo di vincolo
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c Noto via common:
c        -     cmin_eff
c        -     cmax_eff
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/PROTOTIPO.INC'

c
      include '../inc/units.INC'

      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
	common/equi_k/k_min(max_unit), k_max(max_unit)

	COMMON/equi_flag/flag_equi_s(max_unit)
cmar-equi-05-08-09

		COMMON/equi_cho/kmax_cho(max_unit) 

	COMMON/equi_sur/kmin_sur(max_unit) 

	real*4 kmin_sur


	real*4 kmax_cho
cmar-equi-05-08-09
 
      logical*2 flag_equi_s

cmar
cmar
      real*4 perc_equi_min, perc_equi_max
	real*4 k_min, k_max
cmar
c
        real*4    head,qmin,qmax
        real*4    revmin,revmax
        INTEGER*4 vincmin,vincmax
c
        INTEGER*4 i
        INTEGER*4 irange
        real*4    haux,qaux
c-prec
      real*4    delta_h
      parameter   (delta_h = 0.00001)
c-prec-end
c---------------------------------------------------------------------


cmar
cmar     parte surge     
      real*4 qmin_e, qmin_e_r, head_r

cmar     parte choke

      real*4 qmax_e, qmax_e_r

	integer k

	integer*4 tipo_criterio


c----------------------------------------------------------------------

c calcolo di qmax sulla prima fase
      if (cmax_eff(i).ge.clim(3,i)) then
        call hdan_ch(cmax_eff(i),haux,type_max_rev(i),chn(1,i))
      else
        call hdan_ch(cmax_eff(i),haux,type_max_rev(i),chc(1,i))
      end if
c-prec      if (head.lt.haux) then
      if (head.lt.haux-delta_h) then
        if (cmax_eff(i).ge.clim(3,i)) then
          call ndah_ch(cmax_eff(i),head,qmax,revmax,chn(1,i))
        else
          call ndah_ch(cmax_eff(i),head,qmax,revmax,chc(1,i))

	
        end if

	  
	qmax = qmax*(1-perc_equi_max(i) )	
	 
	qmax_e_r = qmax/qmrif
	head_r = head/headrif
      k_max(i) = head_r/(qmax_e_r**2)

	unit_crit_max(i) = k_max(i)


        vincmax = cho_1f

cmar-26-06
c      cmax_eff(i)=unit_crit_max(i)
cmar-26-06      

cmar-24-06
c-mar-26-06      if (cmax_eff(i).ge.clim(3,i)) then
         if (unit_crit_max(i).ge.clim(3,i)) then
C          call ndah_ch(cmax_eff(i),head,qmax,revmax,chn(1,i))       
          call ndah_ch(unit_crit_max(i),head,qmax,revmax,chn(1,i))

         
        else
C          call ndah_ch(cmax_eff(i),head,qmax,revmax,chc(1,i))
          call ndah_ch(unit_crit_max(i),head,qmax,revmax,chc(1,i))

      end if

cmar-24-06


      else
        call hdan_ch(clim(3,i),haux,type_max_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type_max_rev(i),chn(1,i))
        else
          call qdahn(head,qmax,type_max_rev(i),chc(1,i))
        end if

cmar  
      qmax_e_r = qmax/qmrif
	head_r = head/headrif
      k_max(i) = head_r/(qmax_e_r**2)
CMAR
      
      unit_crit_max(i) = k_max(i)
cmar

cmar-26-06
c      cmax_eff(i)=unit_crit_max(i)
cmar-26-06      


        vincmax = mar_1f
        revmax  = type_max_rev(i)
      end if
cmar  

cmar 
cmar   criterio = equidistanza
      if(tipo_crit.eq.crit_equi) then 

        if (flag_equi_s(i) ) then
    
        if (cmin_eff(i).ge.clim(3,i)) then
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
        else
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chc(1,i))
        end if


      else


        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)


      end if


      if (head.gt.haux) then
cmar        if (tipo_surge.eq.parabola) then
cmar
           if (flag_equi_s(i) ) then
cmar
           
          if (cmin_eff(i).ge.clim(3,i)) then
            call ndah_ch(cmin_eff(i),head,qmin,revmin,chn(1,i))

          else

            call ndah_ch(cmin_eff(i),head,qmin,revmin,chc(1,i))

cmar
cm      qmin_e = qmin*(1+perc_equi_min(i) )
      qmin = qmin*(1+perc_equi_min(i) )	
	 
	qmin_e_r = qmin/qmrif
	head_r = head/headrif
      k_min(i) = head_r/(qmin_e_r**2)

	unit_crit_min(i) = k_min(i)

cmar-26-06
c      cmin_eff(i)=unit_crit_min(i)
cmar-26-06      
cmar
          end if 
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)

cmar
c calcolo la qmin per l'equidistanza
      qmin = qmin*(1+perc_equi_min(i) )	 
	qmin_e_r = qmin/qmrif
	head_r = head/headrif
      k_min(i) = head_r/(qmin_e_r**2)

	unit_crit_min(i) = k_min(i)

cmar-26-06
c      cmin_eff(i)=unit_crit_min(i)
cmar-26-06 



CMAR      cmin_eff(i)=unit_crit_min(i)
cmar-24-06-09
c-mar-26-06      if (cmin_eff(i).ge.clim(3,i)) then
            if (unit_crit_min(i).ge.clim(3,i)) then
C            call ndah_ch(cmin_eff(i),head,qmin,revmin,chn(1,i))
             call ndah_ch(unit_crit_min(i),head,qmin,revmin,chn(1,i))

          else

C            call ndah_ch(cmin_eff(i),head,qmin,revmin,chc(1,i))
             call ndah_ch(unit_crit_min(i),head,qmin,revmin,chc(1,i))
	end if


cmar-24-06-09

cmar
          call ndah_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),revmin,head)
        end if
        vincmin = sur_1f
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
        vincmin = mir_1f
        revmin  = type_min_rev(i)
cmar

      qmin_e_r = qmin/qmrif
	head_r = head/headrif
      k_min(i) = head_r/(qmin_e_r**2)
	unit_crit_min(i) = k_min(i)


cmar-26-06
c      cmin_eff(i)=unit_crit_min(i)
cmar-26-06 

      end if

cmar
	else
cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cmar   criterio diverso da equidistanza             !!!
cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


c calcolo di qmin sulla prima fase

      if (tipo_surge.eq.parabola) then
        if (cmin_eff(i).ge.clim(3,i)) then
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
        else
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chc(1,i))
        end if
      else
        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)
      end if

      if (head.gt.haux) then
        if (tipo_surge.eq.parabola) then
          if (cmin_eff(i).ge.clim(3,i)) then
            call ndah_ch(cmin_eff(i),head,qmin,revmin,chn(1,i))
          else
            call ndah_ch(cmin_eff(i),head,qmin,revmin,chc(1,i))
          end if 
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)
          call ndah_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),revmin,head)
        end if
        vincmin = sur_1f
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
        vincmin = mir_1f
        revmin  = type_min_rev(i)
      end if
cmar
	end if



      return
      end


cmar ------ ---------- ------------- ----------------- -------------------

      subroutine limiti_flow_1f_m_OK (head,qmin,qmax,revmin,revmax,
     *           vincmin,vincmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla prima fase, assegnata una
c prevalenza head, restituendo il numero di giri (revmin e revmax) e
c il tipo di vincolo
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c Noto via common:
c        -     cmin_eff
c        -     cmax_eff
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/PROTOTIPO.INC'
c
c
        real*4    head,qmin,qmax
        real*4    revmin,revmax
        INTEGER*4 vincmin,vincmax
c
        INTEGER*4 i
        INTEGER*4 irange
        real*4    haux,qaux
c-prec
      real*4    delta_h
      parameter   (delta_h = 0.00001)
c-prec-end
c---------------------------------------------------------------------
c calcolo di qmax sulla prima fase
      if (cmax_eff(i).ge.clim(3,i)) then
        call hdan_ch(cmax_eff(i),haux,type_max_rev(i),chn(1,i))
      else
        call hdan_ch(cmax_eff(i),haux,type_max_rev(i),chc(1,i))
      end if
c-prec      if (head.lt.haux) then
      if (head.lt.haux-delta_h) then
        if (cmax_eff(i).ge.clim(3,i)) then
          call ndah_ch(cmax_eff(i),head,qmax,revmax,chn(1,i))
        else
          call ndah_ch(cmax_eff(i),head,qmax,revmax,chc(1,i))
        end if
        vincmax = cho_1f
      else
        call hdan_ch(clim(3,i),haux,type_max_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type_max_rev(i),chn(1,i))
        else
          call qdahn(head,qmax,type_max_rev(i),chc(1,i))
        end if
        vincmax = mar_1f
        revmax  = type_max_rev(i)
      end if

c calcolo di qmin sulla prima fase
      if (tipo_surge.eq.parabola) then
        if (cmin_eff(i).ge.clim(3,i)) then
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
        else
          call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chc(1,i))
        end if
      else
        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)
      end if

      if (head.gt.haux) then
        if (tipo_surge.eq.parabola) then
          if (cmin_eff(i).ge.clim(3,i)) then
            call ndah_ch(cmin_eff(i),head,qmin,revmin,chn(1,i))
          else
            call ndah_ch(cmin_eff(i),head,qmin,revmin,chc(1,i))
          end if 
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)
          call ndah_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),revmin,head)
        end if
        vincmin = sur_1f
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
        vincmin = mir_1f
        revmin  = type_min_rev(i)
      end if
c
      return
      end



cmar-26-06----------- ---------------- ------------------ ---------------
      subroutine limiti_flow_2f(head,qmin,qmax,revmin,revmax,
     *           vincmin,vincmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla seconda fase, assegnata una
c prevalenza head, restituendo il numero di giri (revmin e revmax) e il
c tipo di vincolo
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
c
      include '../inc/PROTOTIPO.INC'
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar   dove non include units.inc, ho l'area common
      COMMON/equi_flag/flag_equi_s(max_unit)
      
      logical*2 flag_equi_s

      real*4 perc_equi_min, perc_equi_max
	integer*4 tipo_criterio
cmar
c
        real*4    head,qmin,qmax
        real*4    revmin,revmax
        INTEGER*4 vincmin,vincmax
c
        INTEGER*4 i
        INTEGER*4 irange
        real*4    haux,qaux

CMAR-24-06-09
C  SURGE      E CHOKING
	REAL*4 HEAD_R, QMIN_E_R, QMAX_E_R, K_MIN, K_MAX




c---------------------------------------------------------------------
c calcolo di qmax sulla seconda fase
      call hdan_ch(clim2(4,i),haux,type2_max_rev(i),chc2(1,i))
      if (head.lt.haux) then
        call ndah_ch(clim2(4,i),head,qmax,revmax,chc2(1,i))
cmar
      
	qmax = qmax*(1-perc_equi_max(i) )
c	 
	qmax_e_r = qmax/qmrif
	head_r = head/headrif
      k_max = head_r/(qmax_e_r**2)

c	unit_crit_max(i) = k_max(i)
cmar
        vincmax = cho_2f
cmar-24-06-09
      call ndah_ch(k_max,head,qmax,revmax,chc2(1,i))   
cmar-24-06-09      



      else
        call hdan_ch(clim2(3,i),haux,type2_max_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type2_max_rev(i),chn2(1,i))
        else
          call qdahn(head,qmax,type2_max_rev(i),chc2(1,i))
        end if
        vincmax = mar_2f
        revmax  = type2_max_rev(i)
      end if
cmar
      
cmar  criterio = equidistanza

      if(tipo_crit.eq.crit_equi) then 
c calcolo di qmin sulla seconda fase
cmar      if (tipo_surge.eq.parabola) then
cmar-06-08-09          if (flag_equi_s(i) ) then
cmar

        if (nlim2(i).eq.0) then
        call hdan_ch(clim2_new(i),haux,type2_min_rev(i),chn2(1,i))
      else
        call hdan_su(chn2(1,i),a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *       lim_n2(1,i),type2_min_rev(i),haux,qaux,irange)
      end if
      if (head.gt.haux) then
cmar        if (tipo_surge.eq.parabola) then
cmar-06-08-09          if (flag_equi_s(i) ) then
cmar
 
cmar-06-08-09         if (flag_equi_s(i) ) then
 
      
      if (nlim2(i).eq.0) then
          call qdah_ch(clim2_new(i),head,qmin)
          call ndaq_su_mf(clim2_new(i),qmin,revmin,chn2(1,i))
        else
         call qdah_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *         lim_h2(1,i),head,qmin,irange)
         call ndaq_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *         lim_q2(1,i),qmin,chn2(1,i),revmin,irange)
        end if
cmar
      qmin = qmin*(1+perc_equi_min(i) )
	QMIN_E_R=QMIN/QMRIF
      HEAD_R=HEAD/HEADRIF

	K_MIN=HEAD_R/(QMIN_E_R**2)

cmar-24-06-09
C      call ndaq_su_mf(clim2_new(i),qmin,revmin,chn2(1,i))
       call ndaq_su_mf(K_MIN,qmin,revmin,chn2(1,i))

cmar-24-06-09
c

        vincmin = sur_2f
      else
        call hdan_ch(clim2(3,i),haux,type2_min_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type2_min_rev(i),chn2(1,i))
        else
          call qdahn(head,qmin,type2_min_rev(i),chc2(1,i))
        end if
        vincmin = mir_2f
        revmin  = type2_min_rev(i)
      end if

cmar
       else
cmar
       if (tipo_surge.eq.parabola) then
cmar
        call hdan_ch(clim2_new(i),haux,type2_min_rev(i),chn2(1,i))
      else
        call hdan_su(chn2(1,i),a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *       lim_n2(1,i),type2_min_rev(i),haux,qaux,irange)
      end if
      if (head.gt.haux) then
        if (tipo_surge.eq.parabola) then

          call qdah_ch(clim2_new(i),head,qmin)
          call ndaq_su_mf(clim2_new(i),qmin,revmin,chn2(1,i))
        else
         call qdah_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *         lim_h2(1,i),head,qmin,irange)
         call ndaq_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *         lim_q2(1,i),qmin,chn2(1,i),revmin,irange)
        end if
cmar
c      qmin = qmin*(1+perc_equi_min(i) )
c

        vincmin = sur_2f
      else
        call hdan_ch(clim2(3,i),haux,type2_min_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type2_min_rev(i),chn2(1,i))
        else
          call qdahn(head,qmin,type2_min_rev(i),chc2(1,i))
        end if
        vincmin = mir_2f
        revmin  = type2_min_rev(i)
      end if

cmar
      end if   

      return
      end

cmar-----------------------------------------------------------------

      subroutine limiti_h_ok_orig(hmin,hmax)
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
      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c-prototipo
c distinzione caso in cui la surge sia una spezzata o una parabola
          if (tipo_surge.eq.parabola) then
            call hdan_su_mf(cmin_eff(itip),h,type_max_rev(itip),
     *                    chn(1,itip))
          else
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *             nlim(itip),lim_n(1,itip),type_max_rev(itip),h,q,
     *             irange(itip))
          end if
          if (h.lt.hmax) hmax=h
          call hdan_ch(clim(4,itip),h,type_min_rev(itip),chc(1,itip))
          if (h.gt.hmin) hmin=h
          if(type_bifase(itip)) then
c prototipo
            if (tipo_surge.eq.parabola) then
              call hdan_su_mf(clim2_new(itip),h,type2_max_rev(itip),
     *                      chn2(1,itip))
            else
              call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *             b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *             type2_max_rev(itip),h,q,irange(itip))
            end if
            if (h.lt.hmax) hmax=h
            call hdan_ch(clim2(4,itip),h,type2_min_rev(itip),
     *                   chc2(1,itip))
            if (h.gt.hmin) hmin=h
          endif
        endif
      enddo
      return
      end
cmar++++++++++       limiti_head_06-08-09  ++++++++++++++++++


      subroutine limiti_h(hmin,hmax)
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


            call hdan_su_mf(cmin_eff(itip),h,type_max_rev(itip),
     *                    chn(1,itip))
cmar
            call qdan_ch(cmin_eff(itip), q, type_max_rev(itip), 
     *		              chn(1,itip))
cmar
          else
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *             nlim(itip),lim_n(1,itip),type_max_rev(itip),h,q,
     *             irange(itip))


cmar   to calculate:

      qmx= q*(1+perc_equi_min(itip))
	qmx=qmx/qmrif
	h=h/headrif
	kmin=h/(qmx**2)

	call qdan_ch(kmin, q, type_max_rev(itip), 
     *		              chn(1,itip))

	call hdaq_ch(kmin, h, q)

c	h=h*headrif

c	q= qmx*qmrif



cmar
          end if


          if (h.lt.hmax) hmax=h


          call hdan_ch(clim(4,itip),h,type_min_rev(itip),chc(1,itip))

cmar

          call qdan_ch(clim(4,itip), q, type_min_rev(itip), 
     *		              chc(1,itip))

       
	qmn= q*(1-perc_equi_max(itip))
	qmn=qmn/qmrif
	h=h/headrif
	kmin=h/(qmn**2)

	call qdan_ch(kmin, q, type_min_rev(itip), 
     *		              chc(1,itip))

	call hdaq_ch(kmin, h, q)
      


        
cmar

          if (h.gt.hmin) hmin=h





          if(type_bifase(itip)) then
c prototipo
cmar            if (tipo_surge.eq.parabola) then
              
                 if(nlim2(itip) .eq. 0) then

              call hdan_su_mf(clim2_new(itip),h,type2_max_rev(itip),
     *                      chn2(1,itip))
cmar
                call qdan_ch(clim2_new(itip), q, type2_max_rev(itip), 
     *		              chn2(1,itip))
cmar
 
            else
              call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *             b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *             type2_max_rev(itip),h,q,irange(itip))


cmar   to calculate:

      qmx= q*(1+perc_equi_min(itip))
	qmx=qmx/qmrif
	h=h/headrif
	kmin=h/(qmx**2)

	call qdan_ch(kmin, q, type2_max_rev(itip), 
     *		              chn2(1,itip))

	call hdaq_ch(kmin, h, q)



cmar


            end if
            if (h.lt.hmax) hmax=h
            call hdan_ch(clim2(4,itip),h,type2_min_rev(itip),
     *                   chc2(1,itip))


cmar

          call qdan_ch(clim2(4,itip), q, type2_min_rev(itip), 
     *		              chc2(1,itip))

       
	qmn= q*(1-perc_equi_max(itip))
	qmn=qmn/qmrif
	h=h/headrif
	kmin=h/(qmn**2)

	call qdan_ch(kmin, q, type2_min_rev(itip), 
     *		              chc2(1,itip))

	call hdaq_ch(kmin, h, q)
      


        
cmar
            if (h.gt.hmin) hmin=h
          endif
        endif

	
      enddo

   

cm
cmar se criterio è diverso dall' Equidistanza

      else

cm


      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c-prototipo
c distinzione caso in cui la surge sia una spezzata o una parabola
          if (tipo_surge.eq.parabola) then
            call hdan_su_mf(cmin_eff(itip),h,type_max_rev(itip),
     *                    chn(1,itip))
          else
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *             nlim(itip),lim_n(1,itip),type_max_rev(itip),h,q,
     *             irange(itip))
          end if
          if (h.lt.hmax) hmax=h
          call hdan_ch(clim(4,itip),h,type_min_rev(itip),chc(1,itip))
          if (h.gt.hmin) hmin=h
          if(type_bifase(itip)) then
c prototipo
            if (tipo_surge.eq.parabola) then
              call hdan_su_mf(clim2_new(itip),h,type2_max_rev(itip),
     *                      chn2(1,itip))
            else
              call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *             b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *             type2_max_rev(itip),h,q,irange(itip))
            end if
            if (h.lt.hmax) hmax=h
            call hdan_ch(clim2(4,itip),h,type2_min_rev(itip),
     *                   chc2(1,itip))
            if (h.gt.hmin) hmin=h
          endif
        endif
      enddo


cm
      endif
cm
      return
      end
cmar+++++++++++       limiti_head_06-08-09    +++++++++++++++++++++++++++

cmar ----------- --------------- ----------------- ----------- --------

      subroutine limiti_h_e(hmin,hmax)
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
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar
      real*4 perc_equi_min, perc_equi_max
cmar
        real*4    hmin,hmax
c
        INTEGER*4 itip
        INTEGER*4 irange(maxunits)
        real*4    h,q
c---------------------------------------------------------------------
      hmax=100000.
      hmin=0.
c
      do itip = 1,ntp
        if(type_actr(itip).gt.0) then
c-prototipo
c distinzione caso in cui la surge sia una spezzata o una parabola
          if (tipo_surge.eq.parabola) then
            call hdan_su_mf(cmin_eff(itip),h,type_max_rev(itip),
     *                    chn(1,itip))
c    da controllare 

      
          else
            call hdan_su(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *             nlim(itip),lim_n(1,itip),type_max_rev(itip),h,q,
     *             irange(itip))
          end if
          if (h.lt.hmax) hmax=h
          call hdan_ch(clim(4,itip),h,type_min_rev(itip),chc(1,itip))
          if (h.gt.hmin) hmin=h
          if(type_bifase(itip)) then
c prototipo
            if (tipo_surge.eq.parabola) then
              call hdan_su_mf(clim2_new(itip),h,type2_max_rev(itip),
     *                      chn2(1,itip))
            else
              call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *             b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *             type2_max_rev(itip),h,q,irange(itip))
            end if
            if (h.lt.hmax) hmax=h
            call hdan_ch(clim2(4,itip),h,type2_min_rev(itip),
     *                   chc2(1,itip))
            if (h.gt.hmin) hmin=h
          endif
        endif
      enddo
      return
      end
cmar  -  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      subroutine QDAHN2(h,q,rev1,clim3,chn,chc)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di Q in funzione di (H,n) assegnati distinguendo tra zona
c   normale e zona di choking.
c
      real*4     h,haux        ! I) head
      real*4     q             ! O) portata
      real*4     rev1          ! I) revolution rate
      real*4     chn(*),chc(*) ! I) coefficienti curva caratteristica
      real*4     clim3         ! I) coefficiente curva clim(3)
c
      call hdan_ch(clim3,haux,rev1,chn)
      if (h.ge.haux) then
c zona normale
	call qdahn(h,q,rev1,chn)
      else
c zona choking
	call qdahn(h,q,rev1,chc)
      end if
c
      return
      end


      real*4 Function zero_perc2(qaux)
c
c  funzione da azzerare per il calcolo della portata (della prima fase)
c  suddivisa tra le due fasi in parallelo di un compressore bifase
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
      include '../inc/MF_SIMPSTAT.INC'
c
      real*4   qaux,qaux2,rev1,rev2
      real*4   qtot
      real*4   haux
c----------------------------------------------------------------------
c      qtot = qaux*type_actr(itipo)
      qtot = qaux
      call hdaq_ch(clim(3,itipo),haux,qaux)
      if (headm.ge.haux) then
        call ndahq(headm,qaux,rev1,chn(1,itipo))
      else
        call ndahq(headm,qaux,rev1,chc(1,itipo))
      end if
      rev2 = rev1*type_ratio(itipo)/type2_ratio(itipo)
      call qdahn2(headm,qaux2,rev2,clim2(3,itipo),
     *                 chn2(1,itipo),chc2(1,itipo))
c      qtot = qtot+qaux2*type_actr(itipo)
      qtot = qtot+qaux2
      zero_perc2 = flow_unit-qtot
c
      return
      end
        Subroutine comp_Station_up(istaz,vert_pointer,tipo_criterio,
     *                        flow2,pres1,pres2,temp1,temp2,air,
     *                        comp_ratio,tot_cons,delpr1,delpr2,flowm,
     *                        flow_ric,type_num,unit_num,jufir,jtfir,
     *                        stat_varsp,
     *                        stat_ord,ier)
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
c
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/mf_UNITS.INC'


	
cmar_ac_gestione mess

      include '../inc/messag.INC'
cmar_ac_gestione mess

cgpe      include '../inc/cmf.INC'

CMAR
      COMMON/pre/ppres2
	
	real*4 ppres2  
c
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
c_ecol
cgpe     *        ,delpr_f,delpr_ac,unit_delpr_ac(maxunits)
c_ecol
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
CMAR      ppres2=pres2
      call init_simpstat_mf(istaz,vert_pointer,tipo_criterio,pres1,
     *      pres2,air,tot_cons,
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
c        a=stat_varsp(5)
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
      call compstat_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     -     unit_flow_rat(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
cgpe     *     unit_power(jufir),unit_hrate(jufir),unit_cons(jufir),
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
     *      IER)


	


CMAR    SPEGNIMENTO AC COME STATO DELL'ELEMENTO       
CMAR 
      IF (TEMP2.GT.STAT_VARSP(7))THEN

      ac_ce_k=ac_ce_k_M
      ac1_k=ac1_k_M

  
c      stat_varsp(5)=a
      ac_ce_stat=	ac_ce_stat_M

      ac1_stat=	ac1_stat_M

	FLAG_AC=.FALSE.

cmar_1809
      type_actr=type_actr_M
cmar_1809


cmar_ac_02_10_13
       delpr1=dp1_M
       
	 delpr2= dp2_M

cmar_ac_02_10_1






cmar_ac_gestione mess
c      mess=' '

      do i=1,mess_num(vert_pointer)
      mess_stri(i,vert_pointer)=''
	mess_type(i,vert_pointer)=0

	enddo


cmar_ac_gestione mess

c---->                                       simulazione stazione
      call compstat_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     -     unit_flow_rat(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
cgpe     *     unit_power(jufir),unit_hrate(jufir),unit_cons(jufir),
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
     *      IER)

      ENDIF
     



      


CMAR     IF1
       ELSE
CMAR     IF1

c---->                                       simulazione stazione
      call compstat_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
c_ecol
cgpe     *     delpr_f,delpr_ac,
c_ecol
     *     unit_pres_int(jufir),unit_temp_int(jufir),
     *     unit_delprint(jufir),unit_deltrint(jufir),comp_ratio,
     *     tot_cons,flowm,flow_ric,
     *     type_quant(jtfir),unit_num,lstat,nom_flow(jufir),
     *     unit_perc(jufir),
     *     unit_rev(jufir),unit_flow(jufir),unit_head(jufir),
     -     unit_flow_rat(jufir),
     *     unit_eff_corr(jufir),unit_eff(jufir),unit_power_corr(jufir),
cgpe     *     unit_power(jufir),unit_hrate(jufir),unit_cons(jufir),
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
     *      IER)





CMAR     IF1
      ENDIF
CMAR     IF1


!      if (iopt .or. iopt_mess) return
cerr      if (iopt .or. iopt_mess) goto 11
c
C---> gestione dell'errore
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
!- ier=71: pressione di uscita superiore alla massima fattibile
!- ier=73: pressione di uscita inferiore alla minima fattibile
cerr          if (nit .lt. nitmax) then
!- viene portato il punto dentro le mappe
!- le nuove condizioni di input alla station sono rappresentate da
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
!- se non e' stato corretto il punto, non sono state modificate le
!- condizioni iniziali
!            return
cerr            goto 11

cerr          else
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
      Subroutine compStat_up(vert_pointer,flow2,pres1,pres2,temp1,temp2,
     *     delpr1,delpr2,
     *     unit_pres_int,unit_temp_int,unit_delprint,
     *     unit_deltrint,
     *     comp_ratio,tot_cons,flowm,flow_ric,
     *     type_quant,unit_num,status,nom_flow,unit_perc,
     *     unit_rev,unit_flow,unit_head,unit_flow_rat,unit_eff_corr,
     *     unit_eff,unit_power_corr,unit_power,
     *     unit_hrate_corr,unit_hrate,unit_cons,
     *     unit_temp,unit_max,unit_min,unit_vcrit,unit_vinc,stat_varsp,
     *     unit2_rev,unit2_flow,
     *     unit2_head,unit2_eff_corr,unit2_eff,unit2_power_corr,
     *     unit2_power,unit2_temp,unit_delpr_ac,
     *     IER) 
c
c      simulazione di stazione composta con il criterio di uguaglianza
c      di portate
c
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
c-UDM-end

cmar_pow_stat
      include '../inc/power.INC'
cmar_pow_stat

CMAR       COMMON/pre/pres2
      COMMON/MAR/istaz
      INTEGER*4 istaz
      INTEGER*4 vert_pointer
      INTEGER*4 type_quant(*),unit_num
c
      real*4 nom_flow(*)
      real*4 flow2,pres1,pres2,temp1,temp2,comp_ratio,tot_cons,
     *       flow_ric,unit_perc(*),
     *       unit_rev(*),unit_max(*),unit_min(*),unit_vcrit(*),
     *	   unit_flow(*)
     -      ,unit_flow_rat(*) !coeff. di ripartiz. Q per ogni compr attivo
     -      ,unit_eff_corr(*),unit_eff(*),unit_head(*),
     *	   unit_power_corr(*),unit_power(*),unit_hrate_corr(*),
     *       unit_hrate(*),
     *	   unit_cons(*),unit_temp(*),stat_varsp(*),
     *       delpr1,delpr2,flowm
     *       ,tout_ac
      integer*4 unit_vinc(*)
      real*4 unit_pres_int(*),unit_temp_int(*),unit_delprint(*)
      real*4 unit_deltrint(*)
      real*4 unit2_rev(*),unit2_flow(*),unit2_head(*),
     *       unit2_eff_corr(*),unit2_eff(*),unit2_power_corr(*),
     *       unit2_power(*),unit2_temp(*)
     *      ,unit_delpr_ac(*)

      LOGICAL*2  lsim(maxunits),status(*)
c
      real*4   exp_pol,aux1
      real*4   flow_ass
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
C      integer*4 ier_save
      logical*2 l_ric
      real*4    delpr_f,delpr_ac,
c-ele 1/6/6
     *          deltat(maxunits)
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
c-----------------------------------------------------------------------
cmar_pow_stat


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
      l_ric = .true.

cgpe      if (flow2.le.0.) then
cgpe      end if
c---->                        calcolo variabili in ingresso alla stazione (compstat.INC)
      tin = temp1
c---->             
c              CADUTE DI PRESSIONE DOVUTE AL PIPING

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

cgpe      if(pin.le.0.or.pout.le.0) then
cgpe      endif
cgpe      if(pin.gt.pout) then
cgpe      end if

CMAR
      comp_ratio = pout/pin
      call politrop_esp(istaz, pin,temp1,comp_ratio,exp_pol,ier) 

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
C      ier_save = 0
C      if (.not.iopt .and. .not.iopt_mess) then
        qmin_tot = 0.
        qmax_tot = 0.
        do i = 1,ntp
          if (type_actr(i).gt.0) then
            call calc_punti_bifase(qmin_assoluto(i),qmax_assoluto(i),
     *           hmin_assoluto(i),hmax_assoluto(i),vinc_min(i),
     *           vinc_max(i),i,ier)



cmar
c - gestione errori:
            if (ier.ne.0) then
C              ier_save = ier
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
C                call app_mess(vert_pointer,ier_save,mess)
                call app_mess(vert_pointer,ier,mess)
                return
!-23/08/2000-end
              end if
561   format('Tipo di compressore: ',i3)
            endif
            qmin_tot = qmin_tot + qmin_assoluto(i)*type_actr(i)
            qmax_tot = qmax_tot + qmax_assoluto(i)*type_actr(i)
          end if
        end do

cmar
c      do itip=1,ntp
c      kcrit_min(itip)=k_min(itip)
c	kcrit_max(itip)=k_max(itip)
c	end do
cmar



C      else
Cc calcolo il numero di compressori attivi
C        nact = 0
C        do i=1,ntp
C          nact = nact + type_actr(i)
C        end do
Cc con un solo compressore acceso se unit_act=0 oppure con piu' compressori
Cc accesi nel caso in cui ci siano unita' necessariamente accese oppure con
Cc un numero minimo di compressori da accendere  maggiore di uno
Cc Calcolo i limiti in portata fino ad un numero di unita' accese pari a
Cc max(unit_act,numin)+1
C        if (unit_act.gt.0) then
C          if (unit_act.lt.numin) unit_act = numin-1
C        else
C          if (numin.gt.1) unit_act = numin-1
C        end if
C        if (nact.le.unit_act+1) then
C          qmin_tot = 0.
C          qmax_tot = 0.
C          do i = 1,ntp
C            if (type_actr(i).gt.0) then
C              call calc_punti_bifase(qmin_assoluto(i),
C     *             qmax_assoluto(i),hmin_assoluto(i),
C     *             hmax_assoluto(i),vinc_min(i),
C     *             vinc_max(i),i,ier)
Cc - gestione errori:
C              if (ier.ne.0) then
C                ier_save = ier
Ccgpe                if (ier.eq.12) then
C                if (ier.eq.52) then
C!-23/08/2000
C!                  write(mess,561) i
C!                  call app_mess(vert_pointer,ier,mess)
C!                  ier = 1
C!                  if (iopt .or. iopt_mess) return
C                  ier = 1
C                  if (iopt .or. iopt_mess) return
C                  write(mess,561) i
C                  call app_mess(vert_pointer,ier_save,mess)
C                  return
C!-23/08/2000-end
C                end if
C              endif
C              qmin_tot = qmin_tot + qmin_assoluto(i)*type_actr(i)
C              qmax_tot = qmax_tot + qmax_assoluto(i)*type_actr(i)
C            end if
C          end do
C        else
Cc se ci sono piu' compressori accesi, non vengono ricalcolati i limiti
Cc massimi e minimi
C          qmin_tot = 0.
C          qmax_tot = 0.
C          do i = 1,ntp
C            if (type_actr(i).gt.0) then
C!- gpe 0051   inizio (26/10/2000)
C             if (qmin_assoluto(i).gt.0.and.qmax_assoluto(i).gt.0) then
C              qmin_tot = qmin_tot + qmin_assoluto(i)*type_actr(i)
C              qmax_tot = qmax_tot + qmax_assoluto(i)*type_actr(i)
C             else
C              ier = 1
C              if (iopt .or. iopt_mess) return
C              write(mess,561) i
C              call app_mess(vert_pointer,ier_save,mess)
C              return
C             end if
C!- gpe 0051   fine (26/10/2000)
C            end if
C          end do
C        end if
C      end if
c
!-new
C      if (ier_save .ne. 0) then
Cc errore: pressione richiesta superiore alla massima fattibile
Ccgpe        ier = 21
Ccgpe-mess        ier = 71
C        ier = 52
C        if (iopt .or. iopt_mess) return
Ccgpe-new        call gest_error (2,0,'COMPSTAT_UP','Compressione richiesta non fattibile',0)
C        if (fl_sim) then
C          mess = ' '
C          call app_mess(vert_pointer,ier,mess)
C        endif
C
C        RETURN
C      endif

	call portate_criterioP_serie (vert_pointer
     -                                   ,flowm
     -                                   ,type_quant
cgpe     -                                   ,type_act_old
     -                                   ,unit_flow_rat
     -                                   ,qmin_assoluto
     -                                   ,hmin_assoluto
     -                                   ,qmax_assoluto
     -                                   ,hmax_assoluto
     -                                   ,qmin_tot
     -                                   ,qmax_tot
     -                                   ,vinc_min
     -                                   ,vinc_max
     -                                   ,tip_vinc
     -                                   ,qass
     -                                   ,hass
     -                                   ,l_ric
     -								   ,ier)



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
cccccccc_312      min_p=min_p+type_vinc_minpow(itip)
cccccccc_312      max_p=max_p+type_vinc_maxpow(itip)
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
             call dati_compress2(hass(itip),qass(itip),
     *                qmax_assoluto(itip),qmin_assoluto(itip),
     *                tip_vinc(itip),itip,
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
     *                unit_delprint(icj), unit_deltrint(icj), 
     *                unit_delpr_ac(icj),
     *                stat_varsp, ier)

cmar_pow_stat
	power_tot_staz=power_tot_staz+unit_power(icj)+unit2_power(icj)
cmar_pow_stat



c-ele 28/4/06
               call single_out_temp_ac_new(tair,pout,icj,2,
     *                  unit2_flow(icj),stat_varsp(7),unit2_temp(icj)
c-ele 1/6/6     *                  ,deltat)  
     *                  pres1,temp1,deltat(icj))
c-bpcorr
c-l_ric              if (unit_vinc(icj) .ne. sur_1f .and.
c-l_ric     *            unit_vinc(icj) .ne. sur_2f     ) then
c-l_ric                l_ric = .false.
c-l_ric              end if
c-bpcorr-end
c controllo dell'errore in uscita dalla dati_compress2
              if (ier.gt.0) then
                if (.not.iopt .and. .not.iopt_mess) then
cgpe-new                  call gest_error (2,0,'COMPSTAT_UP',' Violata la max_power',0)
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
        tot_flow = tot_flow+unit_flow(icj)
        tot_cons = tot_cons+unit_cons(icj)
      end do
!      if (.not.iopt .and. .not.iopt_mess) then
      if (.not.iopt) then
!-26/05/2000
        temp2 = 0.
!-26/05/2000-end
        do icj=1,unit_num
!          percent = unit_flow(icj)/tot_flow
!          temp2 = temp2 + percent * unit2_temp(icj)
          unit_perc(icj) = unit_flow(icj)/tot_flow
c ele 28/4/06          temp2 = temp2 + unit_perc(icj) * unit2_temp(icj)
          temp2 = temp2 + unit_perc(icj) * (unit2_temp(icj)-deltat(icj))
c_ecol
        end do
c-------->modifica relativa al delta dell'ac
c
c ---> sottraggo la caduta di temperatura dovuta al piping

      temp2 =temp2 -stat_vars(5)
	call out_temper_ac_ce_new(tair,pout,tot_flow,stat_vars(7),pres1,temp1,
     * temp2)  !new


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

cgpe      flow_ric = 0.
      flow_ric = tot_flow/old_aux3
      if (abs(flowm-tot_flow).gt.eps_q) then
        if (tot_flow.lt.flowm) then
c la stazione non riesce a lavorare tutto il gas
          ier = 8
          if (iopt .or. iopt_mess) return
cgpe-mess          write(mess,558) (tot_flow*conv_ns)/old_aux3,(flowm*conv_ns)/old_aux3
c-UDM          write(mess,558) tot_flow/old_aux3,flowm/old_aux3,UDM_INT_Q
c-UDM558     format('Q lavorata: ',f7.2,' - Q richiesta: ',f7.2,'  ',A12)
          write(mess,558) udm_out_str(udm_Q,tot_flow/old_aux3,0),
     *                    udm_out_str(udm_Q,flowm/old_aux3,1)          
558     format('Q lavorata: ',A10,' - Q richiesta: ',A25)
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
c-bpcorr		    ier = 0
cgpe              flow_ric = tot_flow/old_aux3
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
!!            flow_ric = tot_flow-flow_tot
!            flow_ric = (tot_flow-flow_tot)/old_aux3
c-l_ric          endif
        endif
      endif
c

      return
      end
cmar ---------------------------------\\\\\\\\\\\----------------------


      subroutine calc_punti_bifase
     *                            (qmin,qmax,hmin,hmax,vincmin,
     *           vincmax,itip,ier)
c
c Calcola la portata minima e massima assoluta (sulle prima fase) per un
c tipo di compressore, tale da:
c  - raggiungere la pout richiesta
c  - non violare la max_power
c  - assicurare la fattibilita' del punto corrispondente sulla 
c    seconda fase
c E' associato ad ogni punto un flag:
c  = 0  il punto sulla prima fase e' ok
c  = 1  nel punto non si raggiunge la pout richiesta
c  = 2  nel punto si supera sempre la pout richiesta
c  = 3  il punto viola la max_power
c  = 4  il punto sulla seconda fase non e' fattibile
c  = 5  il punto non viene calcolato
c--------------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC '	
      include '../inc/mf_limiti.INC '	
c
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit) 
	real*4 perc_equi_min, perc_equi_max
cmar

      real*4    head1,rev1
      integer*4 itip,ier            ! O) indice di errore
      integer*4 vincmin,vincmax     ! O) tipo di vincolo
      real*4    qmin,qmax,hmin,hmax ! O) portata e prevalenza minima e massima
c                                        fattibile dall'unita'
      real*4    qmin1,qmax1,flow2,head2,rev2,qmin2,qmax2,
     *          qmin_aux,qmax_aux,revmin,revmax,flowt
      integer*4 flag_qmin,flag_qmax
      real*4    eps_q/1.e-2/
      real*4    eps_n/1.e-2/
      real*4    zero_giri
      external  zero_giri
c-----------------------------------------------------------------------

      ier = 0
      qmin = 0
      qmax = 0 
c
      itipo = itip
c 
      flowt = qsur_mir(itip) - 0.1
6     flowt = flowt + 0.1
      if (flowt.le.qcho_mar(itip)) then
        call verifica_punti_bifase(flowt,head1,rev1,flow2,
     *       head2,rev2,flag_qmin)

        if (flag_qmin.eq.0) then
          qmin = flowt
          hmin = head1
          goto 7
        else
          goto 6
        end if
      end if
7     continue



      flowt = qcho_mar(itip) + 0.1
8     flowt = flowt - 0.1
      if (flowt.ge.qsur_mir(itip)) then
        call verifica_punti_bifase(flowt,head1,rev1,flow2,
     *       head2,rev2,flag_qmax)


        if (flag_qmax.eq.0) then
          qmax = flowt
          hmax = head1
          goto 9
        else
          goto 8
        end if
      end if
9     continue


      if ((qmin.eq.0).and.(qmax.gt.0)) then
        qmin = qmax
      elseif ((qmin.gt.0).and.(qmax.eq.0)) then
        qmax = qmin
      endif
c
c controllo se il numero di punti fattibili e' maggiore di zero
c31052000       if ((qmin.eq.0).and.(qmax.eq.0))then
       if ((qmin.eq.0).or.(qmax.eq.0))then
c Errore: non esistono punti fattibili sulle due fasi.
        ier = 52 ! ier = 12
        return
      end if
c verifica se le portate qmin e qmax sono la minima e la massima fattibile
      qmin_aux = qmin
      flowt = qmin
10    flowt = flowt - 0.01
      call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *     rev2,flag_qmin)
      if (flag_qmin.eq.0) then
        qmin = flowt
        hmin = head1
        goto 10
      else
        goto 11
      end if
11    continue


      if (qmin.lt.qmin_aux) then
        qmin_aux = qmin
        flowt = qmin
12      flowt = flowt - 0.001
        call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *       rev2,flag_qmin)
        if (flag_qmin.eq.0) then
          qmin = flowt
          hmin = head1
          goto 12
        else
          goto 13
        end if
13      continue
      end if

c
      if (qmin.lt.qmin_aux) then
        qmin_aux = qmin
        flowt = qmin
14      flowt = flowt - 0.0001
        call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *       rev2,flag_qmin)
        if (flag_qmin.eq.0) then
          qmin = flowt
          hmin = head1
          goto 14
        else
          goto 15
        end if
15      continue
      end if



c
      qmax_aux = qmax
      flowt = qmax
16    flowt = flowt + 0.01
      call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *     rev2,flag_qmax)
      if (flag_qmax.eq.0) then
        qmax = flowt
        hmax = head1
        goto 16
      else
        goto 17
      end if
17    continue
c


      if (qmax.gt.qmax_aux) then
        qmax_aux = qmax
        flowt = qmax
18      flowt = flowt + 0.001
        call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *       rev2,flag_qmax)
        if (flag_qmax.eq.0) then
          qmax = flowt
          hmax = head1 
          goto 18
        else
          goto 19
        end if
19      continue
      end if
c


      if (qmax.gt.qmax_aux) then
        qmax_aux = qmax
        flowt = qmax
20      flowt = flowt + 0.0001
        call verifica_punti_bifase(flowt,head1,rev1,flow2,head2,
     *       rev2,flag_qmax)
        if (flag_qmax.eq.0) then
          qmax = flowt
          hmax = head1 
          goto 20
        else
          goto 21
        end if
21      continue
      end if
c



c calcolo dei tipi di  vincoli
      call verifica_punti_bifase(qmin,hmin,revmin,flow2,head2,
     *     rev2,flag_qmin)

     

      if (abs(revmin-type_min_rev(itip)).lt.eps_n) then
        vincmin = mir_1f
      else
c

        call limiti_flow(hmin,qmin1,qmax1,itip)


c

        if (abs(qmin-qmin1).lt.eps_q) then
          vincmin = sur_1f
        else
          if (abs(rev2-type2_min_rev(itip)).lt.eps_n) then
            vincmin = mir_2f
          else
            vincmin = sur_2f
          end if
        end if
      end if
	
c
      call verifica_punti_bifase(qmax,hmax,revmax,flow2,head2,
     *     rev2,flag_qmax)



      if (abs(revmax-type_max_rev(itip)).lt.eps_n) then
        vincmax = mar_1f
      else

c
        call limiti_flow(hmax,qmin1,qmax1,itip)

c



        if (abs(qmax-qmax1).lt.eps_q) then
          vincmax = cho_1f
        else
          if (abs(rev2-type2_max_rev(itip)).lt.eps_n) then
            vincmax = mar_2f
 
         else
            call limiti_flow2(head2,qmin2,qmax2,itip)



            if (abs(flow2-qmax2).lt.eps_q) then
              vincmax = cho_2f
            else
              vincmax = pow_1f
            end if
          end if
        end if
      end if
c

cmar


	
      return
      end 

cmar ---------------------------------\\\\\\\\\\\----------------------

      subroutine verifica_punti_bifase (flow1,head1,rev1,flow2,head2,
     *           rev2,flag)
c
c calcola il punto sulla prima fase tale che:
c  - si raggiunge la pout richiesta
c  - non si viola la max_power
c  - il punto corrispondente sulla seconda fase sia fattibile
c E' associato ad ogni punto un flag:
c  = 0  il punto sulla prima fase e' ok
c  = 1  nel punto non si raggiunge la pout richiesta
c  = 2  nel punto si supera sempre la pout richiesta
c  = 3  il punto viola la max_power
c  = 4  il punto sulla seconda fase non e' fattibile
c  = 5  il punto non viene calcolato
c Noto via common:
c itipo
c--------------------------------------------------------------------------
       implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC '	
      include '../inc/mf_simpstat.INC '	
      include '../inc/compstat.INC '	
c
      real*4    flow1
      real*4    head1,rev1,flow2,head2,rev2            ! O) 
      integer*4 flag                                   ! O) 
c
      real*4    revmin,revmax,ra,rb,rd,rsol
      real*4    qaux,uno_su_ro_attuale,dpw,dpres
      real*4    qmin2,qmax2,hmin2,hmax2
      real*4    qsur2_mir,qcho2_mar,haux
      integer*4 irange
c
      real*4    zero_giri
      external  zero_giri
c-----------------------------------------------------------------------
      flag  = 5
      flow2 = 0
      head2 = 0
      rev2  = 0

c calcolo di revmin e revmax
      call limiti_rev(flow1,revmin,revmax,itipo)
      uno_su_ro_attuale = (1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3     = ROCN*uno_su_ro_attuale
c - completamento common
      un_flow1 = flow1
      flow     = flow1/aux3
      rb = zero_giri(revmin)
      ra = zero_giri(revmax)
      if (((ra*rb).lt.0)         .or.
     *     (abs(ra).le.eps_giri) .or.
     *     (abs(rb).le.eps_giri) ) then
        if ((ra*rb).lt.0 ) then
          rd = revmax
cgpe-fun          call zero_fun1(revmin,revmax,rd,rb,ra,rsol,
          call zero_fun(revmin,revmax,rd,rb,ra,rsol,
     *         epsma,pmach,zero_giri)
          rev1 = rsol
        else
          if (abs(ra).le.eps_giri) then
            rev1 = revmax
          else
            rev1 = revmin
          endif
        endif
      else if (ra.gt.0) then
        flag = 1
        return
      else
        flag = 2
        return
      end if
c
      if ((flag.ne.1).and.(flag.ne.2)) then
        call qdan_ch(clim(3,itipo),qaux,rev1,chn(1,itipo))
        if (flow1.le.qaux) then
          call hdaqn(head1,flow1,rev1,chn(1,itipo))
        else
          call hdaqn(head1,flow1,rev1,chc(1,itipo))
        end if
        call verifica_pwmax(itipo,flow1,head1,dpw)
        if (dpw.lt.0) then
c non si supera la max_power
c verifica se il punto sulla seconda fase e' fattibile
cport          call calc_punto_fase2(flow1,head1,itipo,flow2,
cport     *         head2,rev2,dpres)
          call calc_punto_fase2(flow1,head1,itipo,flow2,
     *         head2,rev2) !  dpres non è citato tra i parametri formali!
                           !  (il compilatore su VMS lo tollerava!!!)
c calcolo i limiti in prevalenza massimo e minimo, se flow2 e' compreso
c tra qsur_mir e qcho_mar sulla seconda fase
          call qdan_su(chn2(1,itipo),a_coef2(1,itipo),b_coef2(1,itipo),
     *         nlim2(itipo),lim_n2(1,itipo),type2_min_rev(itipo),
     *         qsur2_mir,haux,irange)
          call QDAN_CH(clim2(4,itipo),qcho2_mar,type2_max_rev(itipo),
     *         chc2(1,itipo))
          if ((flow2.ge.qsur2_mir).and.(flow2.le.qcho2_mar)) then
            call limiti_head2(flow2,hmin2,hmax2,itipo)
            if ((head2.le.hmax2).and.(head2.ge.hmin2)) then
c calcolo i limiti in portata massimo e minimo
              call limiti_flow2(head2,qmin2,qmax2,itipo)
              if ((flow2.le.qmax2).and.(flow2.ge.qmin2)) then
                flag = 0
                else
                flag = 4
              end if
            else
              flag = 4
            end if
          else
            flag = 4
          end if
        else
          flag = 3
        end if
      end if
10    continue
c
      return
      end 
      subroutine HDAQN2 (h,q,rev1,clim3,chn,chc)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
c
c   Calcolo di H in funzione di (Q,n) assegnati distinguendo tra zona
c   normale e zona di choking.
c
      real*4     h             ! O) head
      real*4     q,qaux        ! O) portata
      real*4     rev1          ! I) revolution rate
      real*4     chn(*),chc(*) ! I) coefficienti curva caratteristica
      real*4     clim3         ! I) coefficiente curva clim(3)
c
      call qdan_ch(clim3,qaux,rev1,chn)
      if (qaux .ge.q) then
c zona normale
        call hdaqn(h,q,rev1,chn)
      else
c zona choking
        call hdaqn(h,q,rev1,chc)
      end if
c
      return
      end
      subroutine calc_limiti_OK (rmin, rmax, cmin, cmax)
c ---
c Calcola la portata alle intersezioni fra le curve limite di ogni tipo di
c compressore della prima fase, distinguendo il caso in cui la surge sia una
c parabola o una spezzata
c ---
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/MF_LIMITI.INC'
c
      real*4    h
      integer*4 irange
      real*4    rmin(maxunits),rmax(maxunits)  !I) rev min,rev max
      real*4    cmin(maxunits),cmax(maxunits)  !I) clim min,clim max
      integer*4 itip
c-----------------------------------------------------------------------
      do itip = 1,ntp
        if (type_actr(itip).gt.0) then
          if (cmax(itip).ge.clim(3,itip)) then
            call QDAN_CH(cmax(itip),qcho_mar(itip),rmax(itip),
     *           chn(1,itip))
            call QDAN_CH(cmax(itip),qcho_mir(itip),rmin(itip),
     *           chn(1,itip))
          else
            call QDAN_CH(cmax(itip),qcho_mar(itip),rmax(itip),
     *           chc(1,itip))
            call QDAN_CH(cmax(itip),qcho_mir(itip),rmin(itip),
     *           chc(1,itip))
          endif
          if (tipo_surge.eq.parabola) then
            if (cmin(itip).ge.clim(3,itip)) then
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chn(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chn(1,itip))
            else
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chc(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chc(1,itip))
            endif
          else
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmax(itip),
     *           qsur_mar(itip),h,irange)
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmin(itip),
     *           qsur_mir(itip),h,irange)
          end if
        end if
      enddo
c
      return
      end

cmar_e -----------------------

      subroutine calc_limiti (rmin, rmax, cmin, cmax)
c ---
c Calcola la portata alle intersezioni fra le curve limite di ogni tipo di
c compressore della prima fase, distinguendo il caso in cui la surge sia una
c parabola o una spezzata
c ---
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/MF_LIMITI.INC'
c
      real*4    h
      integer*4 irange
      real*4    rmin(maxunits),rmax(maxunits)  !I) rev min,rev max
      real*4    cmin(maxunits),cmax(maxunits)  !I) clim min,clim max
      integer*4 itip
c-----------------------------------------------------------------------
      do itip = 1,ntp
        if (type_actr(itip).gt.0) then
          if (cmax(itip).ge.clim(3,itip)) then
            call QDAN_CH(cmax(itip),qcho_mar(itip),rmax(itip),
     *           chn(1,itip))
            call QDAN_CH(cmax(itip),qcho_mir(itip),rmin(itip),
     *           chn(1,itip))
          else
            call QDAN_CH(cmax(itip),qcho_mar(itip),rmax(itip),
     *           chc(1,itip))
            call QDAN_CH(cmax(itip),qcho_mir(itip),rmin(itip),
     *           chc(1,itip))
          endif
          if (tipo_surge.eq.parabola) then
            if (cmin(itip).ge.clim(3,itip)) then
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chn(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chn(1,itip))
            else
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chc(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chc(1,itip))
            endif
          else
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmax(itip),
     *           qsur_mar(itip),h,irange)
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmin(itip),
     *           qsur_mir(itip),h,irange)
          end if
        end if
      enddo
c
      return
      end
CMAR_ASS_COND

          subroutine aaa_aaa (itip,rmin, rmax, cmin, cmax, a)
c ---
c Calcola la portata alle intersezioni fra le curve limite di ogni tipo di
c compressore della prima fase, distinguendo il caso in cui la surge sia una
c parabola o una spezzata
c ---
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/SIMPSTAT.INC'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/MF_LIMITI.INC'

cmar
      COMMON/equi_flag/flag_equi_s(max_unit)

cmar
      logical*2 flag_equi_s

c
      real*4    h
      integer*4 irange
      real*4    rmin(maxunits),rmax(maxunits)  !I) rev min,rev max
      real*4    cmin(maxunits),cmax(maxunits)  !I) clim min,clim max
      integer*4 itip
	real*4 a
c-----------------------------------------------------------------------
cmar      a=0.
commento?	do itip = 1,ntp
cc        if (type_actr(itip).gt.0) then
          
cmar          if (tipo_surge.eq.parabola) then
            if (flag_equi_s(itip) ) then

            if (cmin(itip).ge.clim(3,itip)) then
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chn(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chn(1,itip))
            else
              call QDAN_CH(cmin(itip),qsur_mar(itip),
     *             rmax(itip),chc(1,itip))
              call QDAN_CH(cmin(itip),qsur_mir(itip),
     *             rmin(itip),chc(1,itip))
            endif
          else
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmax(itip),
     *           qsur_mar(itip),h,irange)
            call QDAN_SU(chn(1,itip),a_coef(1,itip),b_coef(1,itip),
     *           nlim(itip),lim_n(1,itip),rmin(itip),
     *           qsur_mir(itip),h,irange)
          end if
cc        end if
	a=qsur_mar(itip)
commento?      enddo
c
      return
      end

cmar_ass_cond


CMAR_ASS_COND

cmar_e ---------------------------
      subroutine calc_punto_fase2 (flow1,head1,itip,un_flow2,
     *                           un_head2,un_rev2)
c***********************************************************************
c dato un punto sulla prima fase, calcola il punto corrispondente sulla
c seconda fase
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
      COMMON/pre/ppres2
	real*4 ppres2
      COMMON/MAR/istaz
	integer*4 istaz
      integer*4 itip
      real*4    flow1,head1,un_flow2,un_head2,un_rev2
      real*4    qaux,haux,rev1,flow_norm
      real*4    uno_su_ro_attuale,eesp,aux1,pres_int,temp_int,
     *          eff1,exp_pol,esp2,aux3_int,deltat
      real*4    prop
      external  prop
	character*(max_len) mess
	integer*4 ier,ker
c-ace18
      real*4 ro_int
CMAR
CMAR	integer*4 ier
	real*4 comp_ratio_2
c---------------------------------------------------------------------
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3      = ROCN*uno_su_ro_attuale
      flow_norm = flow1/aux3
c
      call hdaq_ch(clim(3,itip),haux,flow1)
      if (head1.ge.haux) then
c zona normale
        call ndahq(head1,flow1,rev1,chn(1,itip))
      else
        call ndahq(head1,flow1,rev1,chc(1,itip))
      endif
c---->  calcolo efficienza pressione intermedia e temperatura intermedia
      eesp=1/esp1
      aux1 = (erre / agrvcc / pmol) * Zin * tin/esp1
      pres_int=pin*(((head1/aux1)+1)**eesp)
      call effdaqn(eff1,flow1,rev1,cen(1,itip),cec(1,itip),
     *             clim(1,itip))
c
      eff1 = eff1*eff_corr(itip)
cgpe
      if (eff1 .lt. 0.01) eff1 = 0.01
cgpe-end
      aux2 = (pres_int/pin)**esp1
c-ace
c-ace
	call single_out_temp_new(temp_int,tin,zin,pin,pres_int,aux2,
     *           z_coef(1,ver_poi),dh_coef(1,ind),
     *           cp_par(1,ind),eff1)

c----------->tolgo la caduta di temperatura dovuta al piping
      temp_int = temp_int-type_deltint(itip)
c----------->tolgo la caduta di temperatura dovuta al cooler
c-ele 28/4/06
c      call single_out_temp_ac(tair,pres_int,itip,1,
c     *                        flow1,type_maxtint(itip),temp_int)
      call single_out_temp_ac_new(tair,pres_int,itip,1,
     *                flow1,type_maxtint(itip),temp_int,pin,tin,deltat)
      temp_int = temp_int-deltat

c      pres_int = pres_int-type_delpint(itip)*flow_norm*flow_norm
c_ecol Ricalcolo la pressione intermedia tenendo conto delle cadute di pressione
c-ecol dovute al piping e alla presenza dell'air cooler intermedio

c-ace18
      call ro_real(ind,pres_int,temp_int,ro_int)
c      type_delpint(itip)=type_delpint(itip)/ro_int
c	type_ac_k_int(itip)=type_ac_k_int(itip)/ro_int

      pres_int=pres_int-(type_delpint(itip)+
     *                  type_ac_k_int(itip))*flow_norm*flow_norm/ro_int

c In caso di superamento della massima temperatura intermedia non si effettua il taglio
c     if(temp_int.gt.type_maxtint(itip)) temp_int=type_maxtint(itip)

c---->                   secondo stadio
CMAR calcolo il comp_ratio di 2 stadio = pout/pintermedia 
      comp_ratio_2=ppres2/pres_int
	call politrop_esp (istaz,pres_int,temp_int,comp_ratio_2,exp_pol,ier)

CMAR      exp_pol = prop(pres_int,temp_int,exp_coef(1,ind))
      esp2 = (exp_pol-1.)/exp_pol

CMAR      z = prop(pres_int,temp_int,z_coef(1,ver_poi))

      call zpt1(istaz,pres_int,temp_int,z)
c---->                calcolo altezza adiabatica e portata
      aux1 = (erre / agrvcc / pmol) * Z * temp_int/esp2
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*temp_int/pres_int
      aux3_int = ROCN*uno_su_ro_attuale
      un_flow2 = flow_norm*aux3_int
      un_rev2 = rev1*type_ratio(itip)/type2_ratio(itip)
      call QDAN_CH(clim2(3,itip),qaux,un_rev2,chn2(1,itip))
      if (un_flow2.le.qaux) then
        call hdaqn(un_head2,un_flow2,un_rev2,chn2(1,itip))
      else
        call hdaqn(un_head2,un_flow2,un_rev2,chc2(1,itip))
      endif
c
      return
      end

      subroutine limiti_head2_ok_orig (flowt,hmin,hmax,itip)
c
      implicit none
c
c calcola la prevalenza minima e massima sulla seconda fase, assegnata
c una portata flowt
c
c       tipo_surge   I   tipo di surge per ciascun tipo
c       hmin         O   prevalenza minima di stazione
c       hmax         O   prevalenza massima di stazione
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
c
c
        real*4    flowt,hmin,hmax
c
        INTEGER*4 itip
        INTEGER*4 irange
        real*4    qaux
        real*4    qsur2_mar,qcho2_mir
c---------------------------------------------------------------------
c calcolo di qsur_mar sulla seconda fase
      call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *     b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *     type2_max_rev(itip),hmax,qsur2_mar,irange)
c calcolo di qcho_mir sulla seconda fase
      call qdan_ch(clim2(4,itip),qcho2_mir,type2_min_rev(itip),
     *     chc2(1,itip))
c calcolo di hmax
      if (flowt.ge.qsur2_mar) then
        call qdan_ch(clim2(3,itip),qaux,type2_max_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmax,flowt,type2_max_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmax,flowt,type2_max_rev(itip),chc2(1,itip))
        end if
      else
        call HDAQ_SU(a_coef2(1,itip),b_coef2(1,itip),nlim2(itip),
     *       lim_q2(1,itip),flowt,chn2(1,itip),hmax,irange)
      end if
c calcolo di hmin
      if (flowt.le.qcho2_mir) then
        call QDAN_CH(clim2(3,itip),qaux,type2_min_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmin,flowt,type2_min_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmin,flowt,type2_min_rev(itip),chc2(1,itip))
        end if
      else
        call hdaq_ch(clim2(4,itip),hmin,flowt)
      end if
c
      return
      end
cmar-07-08-09
             subroutine limiti_head2 (flowt,hmin,hmax,itip)
c
      implicit none
c
c calcola la prevalenza minima e massima sulla seconda fase, assegnata
c una portata flowt
c
c       tipo_surge   I   tipo di surge per ciascun tipo
c       hmin         O   prevalenza minima di stazione
c       hmax         O   prevalenza massima di stazione
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'

cmar-07-08-09
      common/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar-07-08-09

c
c
        real*4    flowt,hmin,hmax
c
        INTEGER*4 itip
        INTEGER*4 irange
        real*4    qaux
        real*4    qsur2_mar,qcho2_mir
cmar-07-08-09

      integer*4 tipo_criterio
      real*4 perc_equi_min, perc_equi_max
	real*4 q, h, kmin, kmax
	real*4    qsur2_mir,qcho2_mar
cmar-07-08-09


c---------------------------------------------------------------------

      if (tipo_crit .eq. crit_equi)then


c calcolo di qsur_mar sulla seconda fase
      call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *     b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *     type2_max_rev(itip),hmax,qsur2_mar,irange)

cmar070809
      qsur2_mar=qsur2_mar*(1+perc_equi_min(itip))
	qsur2_mar=qsur2_mar/qmrif
	hmax=hmax/headrif
	kmin=hmax/(qsur2_mar**2)

	call qdan_ch(kmin, qsur2_mar, type2_max_rev(itip),
     *                chn2(1,itip))

	call hdaq_ch(kmin, hmax, qsur2_mar)


cmar070809




c calcolo di qcho_mir sulla seconda fase
      call qdan_ch(clim2(4,itip),qcho2_mir,type2_min_rev(itip),
     *     chc2(1,itip))

cmar070809

      call hdan_ch(clim2(4,itip),h,type2_min_rev(itip),chc2(1,itip))

	qcho2_mir=qcho2_mir*(1-perc_equi_max(itip))
	qcho2_mir=	qcho2_mir/qmrif
	h=h/headrif
	kmax=h/(qcho2_mir**2)

	call qdan_ch(kmax,qcho2_mir, type2_min_rev(itip), chc2(1,itip))

	call hdaq_ch(kmax, h, qcho2_mir)


cmar070809



c calcolo di hmax
      if (flowt.ge.qsur2_mar) then
        call qdan_ch(clim2(3,itip),qaux,type2_max_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmax,flowt,type2_max_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmax,flowt,type2_max_rev(itip),chc2(1,itip))
        end if
      else
        call HDAQ_SU(a_coef2(1,itip),b_coef2(1,itip),nlim2(itip),
     *       lim_q2(1,itip),flowt,chn2(1,itip),hmax,irange)

cmar070809
         call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *     b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *     type2_min_rev(itip),h,qsur2_mir,irange)

      if(flowt .lt.qsur2_mir*(1+perc_equi_min(itip)))then

	hmax=h

	else

      q=flowt*(1+perc_equi_min(itip))

	q=q/qmrif
	hmax=hmax/headrif

	flowt=flowt/qmrif

	kmin=hmax/(q**2)

	hmax=kmin*(flowt**2)
     
	hmax=hmax*headrif

	flowt=flowt*qmrif


      endif

cmar070809
      end if
c calcolo di hmin
      if (flowt.le.qcho2_mir) then
        call QDAN_CH(clim2(3,itip),qaux,type2_min_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmin,flowt,type2_min_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmin,flowt,type2_min_rev(itip),chc2(1,itip))
        end if
      else
        call hdaq_ch(clim2(4,itip),hmin,flowt)
     
	
      
cmar070809

      call qdan_ch(clim2(4,itip),qcho2_mar,type2_max_rev(itip),
     *     chc2(1,itip))


      if(flowt .gt. qcho2_mar*(1-perc_equi_max(itip))) then

	call hdaq_ch (clim2(4,itip),h,qcho2_mar)

	hmin=h

	else



      q=flowt*(1-perc_equi_max(itip))
  
      q=q/qmrif
	hmin=hmin/headrif

	flowt=flowt/qmrif

	kmax=hmin/(q**2)

	hmin=kmax*(flowt**2)
     
	hmin=hmin*headrif

	flowt=flowt*qmrif

      endif
cmar070809


      end if

cmar-07-08-09
cmar se criterio è diverso da Equidistanza


	else

cmar-07-08-09


      call hdan_su(chn2(1,itip),a_coef2(1,itip),
     *     b_coef2(1,itip),nlim2(itip),lim_n2(1,itip),
     *     type2_max_rev(itip),hmax,qsur2_mar,irange)
c calcolo di qcho_mir sulla seconda fase
      call qdan_ch(clim2(4,itip),qcho2_mir,type2_min_rev(itip),
     *     chc2(1,itip))
c calcolo di hmax
      if (flowt.ge.qsur2_mar) then
        call qdan_ch(clim2(3,itip),qaux,type2_max_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmax,flowt,type2_max_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmax,flowt,type2_max_rev(itip),chc2(1,itip))
        end if
      else
        call HDAQ_SU(a_coef2(1,itip),b_coef2(1,itip),nlim2(itip),
     *       lim_q2(1,itip),flowt,chn2(1,itip),hmax,irange)
      end if
c calcolo di hmin
      if (flowt.le.qcho2_mir) then
        call QDAN_CH(clim2(3,itip),qaux,type2_min_rev(itip),
     *       chn2(1,itip))
        if (flowt.le.qaux) then
          call hdaqn(hmin,flowt,type2_min_rev(itip),chn2(1,itip))
        else
          call hdaqn(hmin,flowt,type2_min_rev(itip),chc2(1,itip))
        end if
      else
        call hdaq_ch(clim2(4,itip),hmin,flowt)
      end if
c
   


	endif

c
      return
      end


cmar-07-08-09

      subroutine limiti_flow_ok (head,qmin,qmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla prima fase, assegnata
c una prevalenza head
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
c
      include '../inc/PROTOTIPO.INC'
c
c
        real*4    head,qmin,qmax
c
        INTEGER*4 i
        INTEGER*4 irange
        real*4    haux,qaux
c---------------------------------------------------------------------
c calcolo di qmax sulla prima fase
      call hdan_ch(clim(4,i),haux,type_max_rev(i),chc(1,i))
      if (head.le.haux) then
        call qdah_ch(clim(4,i),head,qmax)
      else
        call hdan_ch(clim(3,i),haux,type_max_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type_max_rev(i),chn(1,i))
        else
          call qdahn(head,qmax,type_max_rev(i),chc(1,i))
        end if
      end if
c calcolo di qmin sulla prima fase
      if (tipo_surge.eq.parabola) then
        call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
      else
        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)
      end if
      if (head.ge.haux) then
        if (tipo_surge.eq.parabola) then
          call qdah_ch(cmin_eff(i),head,qmin)
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)
        end if
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
      end if
c
      return
      end

cmar\\\\\\\\\\\-----------------------\\\\\\\\\\\\\\\\\\\\\\\


      subroutine limiti_flow (head,qmin,qmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla prima fase, assegnata
c una prevalenza head
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
c
      include '../inc/PROTOTIPO.INC'

cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
      common/equi_k/k_min(max_unit), k_max(max_unit)

	COMMON/equi_flag/flag_equi_s(max_unit)
 
      logical*2 flag_equi_s
	integer*4 tipo_criterio
cmar
cmar
	real*4 k_min, k_max
      real*4 perc_equi_min, perc_equi_max
cmar
c
c
        real*4    head,qmin,qmax
c
        INTEGER*4 i
        INTEGER*4 irange
        real*4    haux,qaux
cmar
cmar     parte surge     
      real*4 qmin_e, qmin_e_r, head_r

cmar     parte choke

      real*4 qmax_e, qmax_e_r
c---------------------------------------------------------------------
c calcolo di qmax sulla prima fase
cmar_09_02_10      call hdan_ch(clim(4,i),haux,type_max_rev(i),chc(1,i))

      call hdan_ch(clim(3,i),haux,type_max_rev(i),chc(1,i))

      if (head.le.haux) then
        call qdah_ch(clim(4,i),head,qmax)
		qmax = qmax*(1-perc_equi_max(i) )
cmar-24-06-09

      qmax_e_r = qmax/qmrif
	head_r = head/headrif
      k_max(i) = head_r/(qmax_e_r**2)
 
cmar-26-06-cancellato      call ndah_ch(k_max(i),head,qmax,type_max_rev(i),chc(1,i))
cmar-24-06-09
      else
        call hdan_ch(clim(3,i),haux,type_max_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type_max_rev(i),chn(1,i))
        else
          call qdahn(head,qmax,type_max_rev(i),chc(1,i))
        end if
      end if
cmar
c      qmax = qmax*(1-perc_equi_max(i) )
      qmax_e_r = qmax/qmrif
	head_r = head/headrif
      k_max(i) = head_r/(qmax_e_r**2)
cmar-24-06-09
 
cmar	unit_crit_max(i) = k_max(i)

cmar 

c calcolo di qmin sulla prima fase
cmar      if (tipo_surge.eq.parabola) then
cmar
cmar criterio = equidistanza      
	 if(tipo_crit.eq.crit_equi) then
       if (flag_equi_s(i) ) then
cmar
        call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
      else
        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)
      end if
      if (head.ge.haux) then
cmar        if (tipo_surge.eq.parabola) then
cmar
            if (flag_equi_s(i) ) then
cmar
          call qdah_ch(cmin_eff(i),head,qmin)
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)

cmar-20
	qmin = qmin*(1+perc_equi_min(i) )
      qmin_e_r = qmin/qmrif
	head_r = head/headrif
      k_min(i) = head_r/(qmin_e_r**2)
cmar-20
cmar-26-06-09

cm            if (k_min(i).ge.clim(3,i)) then
C            call ndah_ch(cmin_eff(i),head,qmin,revmin,chn(1,i))
cm             call ndah_ch(k_min(i),head,qmin,revmin,chn(1,i))

cm          else

C            call ndah_ch(cmin_eff(i),head,qmin,revmin,chc(1,i))
cm             call ndah_ch(k_min(i),head,qmin,revmin,chc(1,i))
cm	end if


cmar-24-06-09

        end if
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
cmar-20     
	qmin_e_r = qmin/qmrif
	head_r = head/headrif
      k_min(i) = head_r/(qmin_e_r**2)

cmar-20
      end if
cmar
cmar-20		qmin = qmin*(1+perc_equi_min(i) )

cmar-20          	qmin_e_r = qmin/qmrif
cmar-20	head_r = head/headrif
cmar-20      k_min(i) = head_r/(qmin_e_r**2)

cmar	unit_crit_min(i) = k_min(i)
cmar    criterio diverso da equidistanza
      else
cmar
       if (tipo_surge.eq.parabola) then
        call hdan_ch(cmin_eff(i),haux,type_min_rev(i),chn(1,i))
      else
        call hdan_su(chn(1,i),a_coef(1,i),b_coef(1,i),nlim(i),
     *       lim_n(1,i),type_min_rev(i),haux,qaux,irange)
      end if
      if (head.ge.haux) then
        if (tipo_surge.eq.parabola) then
          call qdah_ch(cmin_eff(i),head,qmin)
        else
          call qdah_su(a_coef(1,i),b_coef(1,i),nlim(i),
     *         lim_h(1,i),head,qmin,irange)
        end if
      else
        call hdan_ch(clim(3,i),haux,type_min_rev(i),chn(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type_min_rev(i),chn(1,i))
        else
          call qdahn(head,qmin,type_min_rev(i),chc(1,i))
        end if
      end if
c
cmar

      end if
c
      return
      end

cmar\\\\\\\\\\\\\\\\\----------------------\\\\\\\\\\\\\\\\\\\\
      subroutine limiti_flow2 (head,qmin,qmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla seconda fase, assegnata
c una prevalenza head
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
cmar
      COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)

c		COMMON/equi_flag/flag_equi_s(max_unit)

      real*4 perc_equi_min, perc_equi_max
	integer*4 tipo_criterio
cmar
c
        real*4    head,qmin,qmax
        real*4    haux,qaux
c
        INTEGER*4 i
        INTEGER*4 irange
c---------------------------------------------------------------------
c calcolo di qmax sulla seconda fase
cmar_09_02_10      call hdan_ch(clim2(4,i),haux,type2_max_rev(i),chc2(1,i))
      call hdan_ch(clim2(3,i),haux,type2_max_rev(i),chc2(1,i))
      if (head.le.haux) then
        call qdah_ch(clim2(4,i),head,qmax)
cmar
	qmax = qmax*(1-perc_equi_max(i) )
cmar

      else
        call hdan_ch(clim2(3,i),haux,type2_max_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type2_max_rev(i),chn2(1,i))
        else
          call qdahn(head,qmax,type2_max_rev(i),chc2(1,i))
        end if
      end if
c calcolo di qmin sulla seconda fase


c      if (flag_equi_s(i) ) then


      call hdan_su(chn2(1,i),a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *     lim_n2(1,i),type2_min_rev(i),haux,qaux,irange)


      if (head.ge.haux) then
        call qdah_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *       lim_h2(1,i),head,qmin,irange)
cmar
       qmin = qmin*(1+perc_equi_min(i) )
cmar
      else
        call hdan_ch(clim2(3,i),haux,type2_min_rev(i),chn2(1,i))

        if (head.ge.haux) then
          call qdahn(head,qmin,type2_min_rev(i),chn2(1,i))
        else
          call qdahn(head,qmin,type2_min_rev(i),chc2(1,i))
        end if

      end if
c


      return
      end


 


cmar-24-06-09
      subroutine limiti_flow2_OK (head,qmin,qmax,i)
c
      implicit none
c
c calcola la portata minima e massima sulla seconda fase, assegnata
c una prevalenza head
c
c       min_rev      I   numero di giri minimo per ciascun tipo
c       max_rev      I   numero di giri massimo per ciascun tipo
c       tipo_surge   I   tipo di surge per ciascun tipo
c       qmin         O   portata minima
c       qmax         O   portata massima
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
c
        real*4    head,qmin,qmax
        real*4    haux,qaux
c
        INTEGER*4 i
        INTEGER*4 irange
c---------------------------------------------------------------------
c calcolo di qmax sulla seconda fase
      call hdan_ch(clim2(4,i),haux,type2_max_rev(i),chc2(1,i))
      if (head.le.haux) then
        call qdah_ch(clim2(4,i),head,qmax)
      else
        call hdan_ch(clim2(3,i),haux,type2_max_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmax,type2_max_rev(i),chn2(1,i))
        else
          call qdahn(head,qmax,type2_max_rev(i),chc2(1,i))
        end if
      end if
c calcolo di qmin sulla seconda fase
      call hdan_su(chn2(1,i),a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *     lim_n2(1,i),type2_min_rev(i),haux,qaux,irange)
      if (head.ge.haux) then
        call qdah_su(a_coef2(1,i),b_coef2(1,i),nlim2(i),
     *       lim_h2(1,i),head,qmin,irange)

      else
        call hdan_ch(clim2(3,i),haux,type2_min_rev(i),chn2(1,i))
        if (head.ge.haux) then
          call qdahn(head,qmin,type2_min_rev(i),chn2(1,i))
        else
          call qdahn(head,qmin,type2_min_rev(i),chc2(1,i))
        end if
      end if
c
      return
      end
cmar-25-06-09



cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cmar------------------------------------------------------------
      subroutine limiti_rev(flowt,revmin,revmax,i)
c
      implicit none
c
c calcola il minimo e massimo numero di giri, assegnata una portata flowt
c
c       tipo_surge   I   tipo di surge per ciascun tipo
c       revmin       O   minimo numero di giri
c       revmax       O   massimo numero di giri
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/mf_limiti.INC'
c
      include '../inc/PROTOTIPO.INC'
c
c
        real*4    flowt,revmin,revmax
c
        INTEGER*4 i
        INTEGER*4 irange
cmar-26-06
      common/equi/perc_equi_min(max_unit),perc_equi_max(max_unit)
      common/equi_flag/flag_equi_s(max_unit)

	logical*2 flag_equi_s


	real*4 perc_equi_min,perc_equi_max
	real*4 flowt_e, h, h_e, kmin_e(max_unit),kmax_e(max_unit)

	integer*4 tipo_criterio
c---------------------------------------------------
c---------------------------------------------------------------------
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if(tipo_crit.eq.crit_equi) then 
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      if (qsur_mar(i).le.qcho_mir(i)) then
        if (flowt.lt.qsur_mar(i)) then
	    revmin = type_min_rev(i)
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)
	    revmax = type_max_rev(i)
	  else
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	elseif (qcho_mir(i).lt.qsur_mar(i)) then
        if (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)


       if ( .not. flag_equi_s(i) ) then 

cmar   alla data q, trovo h, sulla spezzata

      call HDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),h,irange)
	
      flowt_e=flowt*(1+perc_equi_min(i))
	flowt_e=flowt/qmrif
      h_e=h/headrif
	kmin_e(i)=h_e/(flowt_e**2)

cmar   prendo il numero di giri dalla parabola passante per il pto sulla parabola del
cmar    dell'equidistanza, ricalcolata ognivolta
      call NDAQ_SU_MF(kmin_e(i),flowt,revmax,chn(1,i))


cmar-26-06


          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qsur_mar(i)) then
c revmin è l'intersezione di flowt con la choking
cmar-26-06

      call hdaq_ch(cmax_eff(i),h,flowt)

      flowt_e=flowt*(1+perc_equi_min(i))
	flowt_e=flowt/qmrif
      h_e=h/headrif
	kmax_e(i)=h_e/(flowt_e**2)

cmar-26-06

          if (kmax_e(i).ge.clim(3,i))then
            call ndaq_ch(kmax_e(i),flowt,revmin,chn(1,i))
cold          else
           call ndaq_ch(kmax_e(i),flowt,revmin,chc(1,i))
      
          

          endif
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  else
c revmin è l'intersezione di flowt con la choking

          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	endif

c      if (abs(revmax-type_min_rev(i)).lt.eps_giri) then
c        revmax = type_min_rev(i)
c      else if (abs(revmax-type_max_rev(i)).lt.eps_giri) then
c        revmax = type_max_rev(i)
c      end if
c      if (abs(revmin-type_min_rev(i)).lt.eps_giri) then
c        revmin = type_min_rev(i)
c      else if (abs(revmin-type_max_rev(i)).lt.eps_giri) then
c        revmin = type_max_rev(i)
c      end if
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      else
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          if (qsur_mar(i).le.qcho_mir(i)) then
        if (flowt.lt.qsur_mar(i)) then
	    revmin = type_min_rev(i)
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)
	    revmax = type_max_rev(i)
	  else
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	elseif (qcho_mir(i).lt.qsur_mar(i)) then
        if (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qsur_mar(i)) then
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  else
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	endif


c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      endif
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      return
      end
cmar
cmar----------------------




cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


cmar------------------------------------------------------------
      subroutine limiti_rev_ok_orig (flowt,revmin,revmax,i)
c
      implicit none
c
c calcola il minimo e massimo numero di giri, assegnata una portata flowt
c
c       tipo_surge   I   tipo di surge per ciascun tipo
c       revmin       O   minimo numero di giri
c       revmax       O   massimo numero di giri
c----------------------------------------------------------------------
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/MF_SIMPSTAT.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/mf_limiti.INC'
c
      include '../inc/PROTOTIPO.INC'
c
c
        real*4    flowt,revmin,revmax
c
        INTEGER*4 i
        INTEGER*4 irange
c---------------------------------------------------------------------
      if (qsur_mar(i).le.qcho_mir(i)) then
        if (flowt.lt.qsur_mar(i)) then
	    revmin = type_min_rev(i)
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)
	    revmax = type_max_rev(i)
	  else
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	elseif (qcho_mir(i).lt.qsur_mar(i)) then
        if (flowt.lt.qcho_mir(i)) then
	    revmin = type_min_rev(i)
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  elseif (flowt.lt.qsur_mar(i)) then
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
c revmax è l'intersezione di flowt con la surge
          if (tipo_surge.eq.spezzata) then
            call NDAQ_SU(a_coef(1,i),b_coef(1,i),nlim(i),
     *           lim_q(1,i),flowt,chn(1,i),revmax,irange)
          else
            call NDAQ_SU_MF(cmin_eff(i),flowt,revmax,chn(1,i))
          end if
	  else
c revmin è l'intersezione di flowt con la choking
          if (cmax_eff(i).ge.clim(3,i))then
            call ndaq_ch(cmax_eff(i),flowt,revmin,chn(1,i))
          else
            call ndaq_ch(cmax_eff(i),flowt,revmin,chc(1,i))
          endif
	    revmax = type_max_rev(i)
	  endif
	endif

c      if (abs(revmax-type_min_rev(i)).lt.eps_giri) then
c        revmax = type_min_rev(i)
c      else if (abs(revmax-type_max_rev(i)).lt.eps_giri) then
c        revmax = type_max_rev(i)
c      end if
c      if (abs(revmin-type_min_rev(i)).lt.eps_giri) then
c        revmin = type_min_rev(i)
c      else if (abs(revmin-type_max_rev(i)).lt.eps_giri) then
c        revmin = type_max_rev(i)
c      end if

      return
      end

cmar

      Subroutine calc_pmin (itip,ier)
c
c------------------------------------------------------------------------
c   calcola la massima pressione di ingresso in modo da raggiungere pout
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/compstat.INC'
c
      integer*4 itip,ier

      integer*4 vincmin,vincmax
      real*4    qmin,qmax,hmin,hmax,delp,prich
      parameter (delp = 0.1)
c---------------------------------------------------------------------
      prich = pin
      do pin = prich, pout, delp
!-determina se esistono punti fattibili per la compressione richiesta
!-pin e pout
        call calc_punti_bifase (qmin,qmax,hmin,hmax,vincmin,
     *       vincmax,itip,ier)
        if (ier .eq. 0) then
          if (qmin .gt. 0 .and. qmax .gt. 0) return
        endif
      enddo
c
      return
      end

      Subroutine calc_pmax (itip,ier)
c
c------------------------------------------------------------------------
c   calcola la massima pressione di ingresso in modo da raggiungere pout
c------------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/compstat.INC'
c
      integer*4 itip,ier

      integer*4 vincmin,vincmax
      real*4    qmin,qmax,hmin,hmax,delp,prich
      parameter (delp = 0.1)
c---------------------------------------------------------------------
      prich = pout
      do pout = prich, pin, -delp
!-determina se esistono punti fattibili per la compressione richiesta
!-pin e pout
        call calc_punti_bifase (qmin,qmax,hmin,hmax,vincmin,
     *       vincmax,itip,ier)
        if (ier .eq. 0) then
          if (qmin .gt. 0 .and. qmax .gt. 0) return
        endif
      enddo
c
      return
      end

	subroutine portate_criterioP_parall (vert_pointer
     -                                   ,head
     -                                   ,flowm
     -                                   ,type_quant
     -                                   ,flow_rat
     -                                   ,qmin_assoluto
     -                                   ,qmin_assoluto2
     -                                   ,qmax_assoluto
     -                                   ,qmax_assoluto2
     -                                   ,tip_vinc
     -                                   ,qass
     -                                   ,qass2
     -								   ,l_ric
     -								   ,ier)
	! Determina i valori di portata lavorabili da tutti i TC accesi.
	! Questa subroutine è utilizzabile per centrali con TC monofase e/o bifase parallelo.
	! Si può presentare una delle seguenti 3 situazioni:
	! 1) La centrale è in grado di lavorare la portata richiesta.
	!    In questo caso la subroutine verifica per ogni compressore se il punto di lavoro
	!    è dentro mappa: in caso contrario riporta il punto di lavoro
	!    sulle curve limite.
	! 2) La portata richiesta è inferiore alla minima lavorabile dalla centrale
	! 3) La portata richiesta è superiore alle possibilità della centrale

	implicit none
      include '../inc/param.INC'
      include '../inc/stazione.INC'
      include '../inc/simpstat.INC'
      include '../inc/prototipo.INC'
      include '../inc/flag.INC'

      INTEGER*4 vert_pointer    !I)
	real*4	  head		      !I) 
	real*4	  flowm		      !I) portata richiesta
      integer*4 type_quant(*)   !I) numeri di TC per ogni tipo
cgpe      integer*4 type_act_old(*) !I) numeri di TC accesi per ogni tipo
	real*4	  flow_rat(*)	      !I) coeff. di ripartiz. Q per ogni compr attivo
      real*4    qmin_assoluto(maxunits) !I) qmin di 1^ f che può lavorare
      real*4    qmin_assoluto2(maxunits)!I) qmin di 2^ f che può essere lavorata
      real*4    qmax_assoluto(maxunits) !I) qmax di 1^ f che può essere lavorata
      real*4    qmax_assoluto2(maxunits)!I) qmax di 2^ f che può essere lavorata
      integer*4 tip_vinc(*)  !O) tipo di vinc da rispett per ogni tipo di TC
      real*4    qass(*)      !O) q compr di 1^ f per ogni tipo di TC
      real*4    qass2(*)     !O) q compr di 2^ f per ogni tipo di TC
      logical*2 l_ric           !O) flag per indicare se la centrale è in riciclo
	integer*4 ier			  !O)

      real*4    flow_ass ! somma delle portate assegnate ai TC già trattati

	real*4    qmin_tot,qmax_tot
      integer*4 vinc_min(maxunits)
      integer*4 vinc_max(maxunits)
	integer*4 i,itip

      character*(max_len) mess/' '/
c---------------------------------------------------------------------
! Inizializzazioni:
      do itip = 1,ntp
        qass(itip)  = 0.
        qass2(itip) = 0.
      enddo

      flow_ass=0.
cgpe
10    continue
cgpe-end

c assegno flow_tot = parte di portata che deve essere ancora considerata ai fini
c della simulazione dei TC (è definita in simpstat.inc)
	flow_tot = flowm-flow_ass

! determina i limiti inferiore e superiore di portata, per ciascun compressore (q..._assoluto).
      do i=1,ntp
        cmax_eff(i) = clim(4,i)
      enddo

      call limiti_q_bifase(head,qmin_tot,qmin_assoluto,
     *             qmin_assoluto2,qmax_tot,qmax_assoluto,
     *             qmax_assoluto2,vinc_max,vinc_min,ier)
      if(ier.ne.0) then
        if (iopt .or. iopt_mess) return
        mess = ' '
        call app_mess(vert_pointer,ier,mess)
        ier = 1
        return
      endif

c Verifico se la portata richiesta è inferiore alla minima lavorabile
c dalla centrale.
	if (flow_tot .lt. qmin_tot) then
c-l_ric	  l_ric = .true.
	  do itip =1,ntp
	    if (type_actr(itip).gt.0) then
	      if ((vinc_min(itip).ne.sur_1f) .and.
     *          (vinc_min(itip).ne.sur_2f)) then
	         l_ric = .false.
	      endif
	    endif
	  enddo
cgpe-new        if (fl_sim) then
cgpe-new          if (.not.iopt .and. .not.iopt_mess) then
cgpe-new            if (l_ric) then
cgpe-new              call gest_error (3,0,'STATION_UP',
cgpe-new     *           'La centrale e'' in riciclo',0)
cgpe-new            else
cgpe-new	        call gest_error (2,0,'STATION_UP',
cgpe-new     *             'Portata inferiore alla minima',0)
cgpe-new		  end if
cgpe-new	    endif
cgpe-new	  endif

        do itip = 1,ntp
	    if (type_actr(itip) .gt. 0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)  = qmin_assoluto(itip)
            qass2(itip) = qmin_assoluto2(itip)
          endif
        enddo

        goto 100

c-bpcorr      elseif (flow_tot .gt. qmax_tot) then
c-bpcorrc Verifico se la portata richiesta è superiore alla massima lavorabile
c-bpcorrc dalla centrale.
c-bpcorrcgpe-new        if (fl_sim) then
c-bpcorrcgpe-new          if (.not.iopt .and. .not.iopt_mess) then
c-bpcorrcgpe-new            call gest_error (2,0,'STATION_UP',
c-bpcorrcgpe-new     *           'Portata superiore alla massima',0)
c-bpcorrcgpe-new          end if
c-bpcorrcgpe-new        end if

c-bpcorr        do itip = 1,ntp
c-bpcorr	    if (type_actr(itip) .gt. 0) then
c-bpcorr            tip_vinc(itip) = vinc_max(itip)
c-bpcorrc-bpcorr
c-bpcorrc-bpcorr            qass(itip)  = qmin_assoluto(itip)
c-bpcorrc-bpcorr            qass2(itip) = qmin_assoluto2(itip)
c-bpcorr            qass(itip)  = qmax_assoluto(itip)
c-bpcorr            qass2(itip) = qmax_assoluto2(itip)
c-bpcorr-end
c-bpcorr          endif
c-bpcorr        enddo

c-bpcorr        goto 100

	endif
cgpe-new-end

c simulazione dei punti fuori mappa
	call trattaFuoriMappa_parall
     -   (vert_pointer
     -   ,flow_tot
     -   ,type_quant
     -   ,type_actr
     -   ,ntp
     -   ,flow_rat
     -   ,qmin_assoluto
     -   ,qmin_assoluto2
     -   ,qmax_assoluto
     -   ,qmax_assoluto2
     -   ,vinc_min
     -   ,vinc_max
     -   ,iopt
     -   ,iopt_mess
     -   ,flow_ass
     -   ,tip_vinc
     -   ,qass
     -   ,qass2
     -	)

c simulazione normale
	flow_tot = flowm - flow_ass
      call trattaNormale_parall
     -   (head
     -   ,flow_tot
     -   ,type_quant
     -   ,flow_rat
     -   ,qmin_assoluto
     -   ,qmin_assoluto2
     -   ,qmax_assoluto
     -   ,qmax_assoluto2
     -   ,qass
     -   ,qass2
     -	)

cgpe	!Verifico se la portata richiesta è inferiore alla minima lavorabile
cgpe	!dalla centrale.
cgpe	call verificaQmin_parall (flowm
cgpe     -                        ,qmin_assoluto
cgpe     -                        ,qmin_assoluto2
cgpe     -                        ,type_act_old
cgpe     -                        ,vinc_min
cgpe     -                        ,ntp
cgpe     -                        ,sur_1f ,sur_2f
cgpe     -                        ,iopt ,iopt_mess
cgpecgpe
cgpe     -                        ,l_ric
cgpecgpe-end
cgpe     -                        )

cgpe	!Verifico se la portata richiesta è superiore alla massima lavorabile
cgpe	!dalla centrale.
cgpe	call verificaQmax_parall (flowm
cgpe     -                        ,qmax_assoluto
cgpe     -                        ,qmax_assoluto2
cgpe     -                        ,type_act_old
cgpe     -                        ,ntp
cgpe     -                        ,iopt ,iopt_mess
cgpe     -                        )

cgpe
100   continue
cgpe-end
	return
	end

	subroutine trattaFuoriMappa_parall
     -   (vert_pointer      !I)
     -   ,flow_totale       !I) portata da lavorare
     -   ,type_quant        !I) vettore del numero di TC per ogni tipo
     -   ,type_actr         !I) numero di TC accesi per ogni tipo di TC
     -   ,ntp               !I) dimensione di type_quant
     -   ,flow_rat          !I) coeff. di ripartiz. per ogni compr attivo
     -   ,qmin_assoluto     !I) qmin fase1 di ciascun tipo di TC
     -   ,qmin_assoluto2    !I) qmin fase2 di ciascun tipo di TC
     -   ,qmax_assoluto     !I) qmax fase1 di ciascun tipo di TC
     -   ,qmax_assoluto2    !I) qmax fase2 di ciascun tipo di TC
     -   ,vinc_min          !I) tipi di vincolo di minima di ciascun tipo di TC
     -   ,vinc_max          !I) tipi di vincolo di massima di ciascun tipo di TC
     -   ,iopt              !I)
     -   ,iopt_mess         !I)
     -   ,flow_ass          !I,O)
     -   ,tip_vinc   !O) tipo di vincolo da rispettare per ciascun tipo di TC
     -   ,qass    !O) q da attribuire al compr di 1^ f per ciascun tipo di TC
     -   ,qass2   !O) q da attribuire al compr di 2^ f per ciascun tipo di TC
     -	)
	! Tratta tutti i compressori fuori mappa e, di volta in volta, riporta
	! sulla curva limite (a sinistra o a destra) il compressore che maggiormente
	! si discosta dalle curve limite.

	implicit none
      include '../inc/param.INC'
      include '../inc/stazione.INC'

      integer*4 vert_pointer
      real*4    flow_totale          
	integer*4 type_quant(*)   
	integer*4 type_actr(*)   
	integer*4 ntp   
	real*4	  flow_rat(*)	       
      real*4    qmin_assoluto(*) 
      real*4    qmin_assoluto2(*)
      real*4    qmax_assoluto(*) 
      real*4    qmax_assoluto2(*)
	integer*4 vinc_min(*)   
	integer*4 vinc_max(*)   
      logical*2 iopt
      logical*2 iopt_mess
      real*4    flow_ass
      integer*4 tip_vinc(*)  
      real*4    qass(*)
      real*4    qass2(*)



      real*4    flow_tot          
	integer*4 jTc
	real*4 portata
      character*(max_len) mess/' '/
	integer*4 ker,ier,itip
	logical *2 bFuoriMappa
	logical *2 bFuoriMappaSur
	real*4 dQ ! scostamento dalla curva limite
	real*4 dQMax ! scostamento max dalla curva limite
	real*4 qTcMin ! portata min  al TC
	real*4 qTcMax ! portata max  al TC
	integer*4 jFuori ! indice del tipo di TC maggiormente fuori mappa



      flow_tot = flow_totale

	bFuoriMappa=.true.
	do while (bFuoriMappa)
	  ! cerco di individuare il jFuori-esimo tipo di TC
	  ! più limitante, cioè che si discosta maggiormente
	  ! dalle curve o di anti surge o di anti chocking:
	  bFuoriMappa=.false.
	  dQMax=0
        do itip=1,ntp
	    if (type_actr(itip).gt.0) then
	      !Calcolo la portata in base al criterio di ripartizione delle portate:
	      call criterio_P (flow_tot,flow_rat,type_quant,type_actr,ntp
     *                      ,itip,portata)

            qTcMin=qmin_assoluto(itip)+qmin_assoluto2(itip)
            if (portata.le.qTcMin) then
	        dQ=abs(portata-qTcMin)
	        if (dQ.gt.dQMax) then
	          dQMax=dQ
	          jFuori=itip
	          bFuoriMappaSur=.true.
	          bFuoriMappa=.true.
	        endif
	      endif

            qTcMax=qmax_assoluto(itip)+qmax_assoluto2(itip)
            if (portata.ge.qTcMax) then
	        dQ=abs(portata-qTcMax)
	        if (dQ.gt.dQMax) then
	          dQMax=dQ
	          jFuori=itip
	          bFuoriMappaSur=.false.
	          bFuoriMappa=.true.
	        endif
	      endif

	    endif
	  end do

	  ! tratto il compressore più limitante (il jFuori-esimo tipo)
	  if (bFuoriMappa) then

	    if (bFuoriMappaSur) then

	      ! gestione dei messaggi ---------------------------------
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_min(jFuori).eq.sur_1f)    .or.
     *            (vinc_min(jFuori).eq.sur_2f))    then
cgpe-mess                 write(mess,558) jFuori, 'Parabola'
                ker = 21
              else
cgpe-mess                 write(mess,558) jFuori, 'Giri'
                ker = 22
              endif
cgpe-mess558           format('Tipo vincolato:',i2,' - Tipo vincolo:',a)
cgpe-mess              ker = 93
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'STATION_UP',
cgpe-new     *             'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'STATION_UP',
cgpe-new     *             'Unita'' vincolata al minimo',0)
            end if
	      ! -------------------------------------------------------

            tip_vinc(jFuori) = vinc_min(jFuori)
            qass(jFuori)     = qmin_assoluto(jFuori)
            qass2(jFuori)    = qmin_assoluto2(jFuori)
	    else
	      ! oltre l'anti chocking o oltre la curva di max n. di giri o di max potenza

	      ! gestione dei messaggi ---------------------------------
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_max(jFuori).eq.cho_1f)    .or.
     *            (vinc_max(jFuori).eq.cho_2f))    then
cgpe-mess                write(mess,558) jFuori, 'Parabola'
                ker = 24
              elseif ((vinc_max(jFuori).eq.mar_1f) .or.
     *                (vinc_max(jFuori).eq.mar_2f)) then
cgpe-mess                write(mess,558) jFuori, 'Giri'
                ker = 23
              else
cgpe-mess                write(mess,558) jFuori, 'Potenza'
                ker = 25
              endif
cgpe-mess              ker = 94
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'STATION_UP',
cgpe-new     *             'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'STATION_UP',
cgpe-new     *             'Unita'' vincolata al massimo',0)
            end if
	      ! -------------------------------------------------------

            tip_vinc(jFuori)  = vinc_max(jFuori)
            qass(jFuori)      = qmax_assoluto(jFuori)
            qass2(jFuori)     = qmax_assoluto2(jFuori)
	    endif

          flow_ass = flow_ass 
     *               + (qass(jFuori) + qass2(jFuori)) 
     *                  * type_actr(jFuori)
	    type_actr(jFuori) = 0
          flow_tot = flow_totale - flow_ass
	  endif ! bFuoriMappa

	end do ! bFuoriMappa

	return
	end

	subroutine trattaNormale_parall
     -   (head              !I)
     -   ,flow_totale       !I) portata da lavorare
     -   ,type_quant        !I) vettore del numero di TC per ogni tipo
     -   ,flow_rat          !I) coeff. di ripartiz. per ogni compr attivo
     -   ,qmin_assoluto     !I) qmin fase1 di ciascun tipo di TC
     -   ,qmin_assoluto2    !I) qmin fase2 di ciascun tipo di TC
     -   ,qmax_assoluto     !I) qmax fase1 di ciascun tipo di TC
     -   ,qmax_assoluto2    !I) qmax fase2 di ciascun tipo di TC
     -   ,qass    !O) q da attribuire al compr di 1^ f per ciascun tipo di TC
     -   ,qass2   !O) q da attribuire al compr di 2^ f per ciascun tipo di TC
     -	)
	implicit none
      include '../inc/param.INC'
      include '../inc/stazione.INC'
      include '../inc/simpstat.INC'
      include '../inc/MF_SIMPSTAT.INC'

	real*4    head
	real*4    flow_totale
	integer*4 type_quant(*)   
	real*4	  flow_rat(*)	       
      real*4    qmin_assoluto(*) 
      real*4    qmin_assoluto2(*)
      real*4    qmax_assoluto(*) 
      real*4    qmax_assoluto2(*)
      real*4    qass(*)
      real*4    qass2(*)
	 
      real*4    zero_perc2
      EXTERNAL  zero_perc2

	integer*4 itip  
	real*4 portata 
      real*4 fa,fb
      real*4 ra,rb,rd
      real*4 qcrit_1f ! portata prima fase per il rispetto criterio
      real*4 haux
      real*4 rev,rev2

      real*4    flowTot          
	flowTot = flow_totale


	! Calcolo la portata qass per i TC monofase:
      do itip=1,ntp
        if ((.not.(type_bifase(itip))).and.
     *            (type_actr(itip).gt.0)) then

	    call criterio_P (flowTot,flow_rat,type_quant,type_actr,ntp
     *                    ,itip,portata)

          qass(itip) = portata
          qass2(itip) = 0.

	    flowTot = flowTot - portata*type_actr(itip)
          type_actr(itip) = 0

        end if
      end do


	! Calcolo le portate qass qass2 per i TC bifase parallelo:
      do itip=1,ntp
        if ((type_bifase(itip)).and.(type_actr(itip).gt.0)) then

	    
		call criterio_P (flowTot,flow_rat,type_quant,type_actr,ntp
     *                    ,itip,portata)

	    !================ aree common ==================
	    flow_unit = portata
          itipo = itip
	    !================ aree common ==================

          fa = zero_perc2(qmin_assoluto(itip))
          fb = zero_perc2(qmax_assoluto(itip))
          if ((fa*fb).lt.0 ) then
            ra = qmin_assoluto(itip)
            rb = qmax_assoluto(itip)
            rd = (ra + rb)/2.
cgpe-fun            call zero_fun1(ra,rb,rd,fa,fb,qcrit_1f,epsma,pmach,
            call zero_fun(ra,rb,rd,fa,fb,qcrit_1f,epsma,pmach,
     *                     zero_perc2)
	      qass(itip) = qcrit_1f

            ! calcolo di qass2 sulla seconda fase corrispondente a qcrit_1f
            call hdaq_ch(clim(3,itip),haux,qcrit_1f)
	      if (head.ge.haux) then
              call ndahq(head,qcrit_1f,rev,chn(1,itip))
            else
              call ndahq(head,qcrit_1f,rev,chc(1,itip))
            end if
            rev2=rev*type_ratio(itip)/type2_ratio(itip)
	      call qdahn2(head,qass2(itip),rev2,clim2(3,itip),
     *                 chn2(1,itip), chc2(1,itip))
          else if (abs(fa).le.eps_crit) then
	      qass(itip)  = qmin_assoluto(itip)
	      qass2(itip) = qmin_assoluto2(itip)
          else if (abs(fb).le.eps_crit) then
            qass(itip)  = qmax_assoluto(itip)
	      qass2(itip) = qmax_assoluto2(itip)
	    end if
          
	    flowTot = flowTot - portata*type_actr(itip)
		type_actr(itip) = 0

	  end if
      end do
	
	return
	end

	subroutine criterio_P 
     -   (flowTot           !I) portata da lavorare
     -   ,flow_rat          !I) coeff. di ripartiz. per ogni compr attivo
     -   ,typeQuant         !I) numero di TC attivi per ogni tipo di TC
     -   ,typeActr          !I) numero di TC accesi per ogni tipo di TC
     -   ,nTipi             !I) numero di tipi di TC
     -   ,itip              !I) indice del tipo di TC sul quale ripartire la Q
     -   ,portata)          !O) portata calcolata in base al coeff
                            !   dell' itip-esimo tipo di compressore

	implicit none

      real*4    flowTot          
	real*4	  flow_rat(*)	       
      integer*4 typeQuant(*)
      integer*4 typeActr(*)
      integer*4 itip
      integer*4 nTipi
      real*4    portata
  
      integer*4 jTc,jtcp
      integer*4 jtp
	integer*4 getPrimoJTc

      real*4    sumCoeff

	!Calcolo l'indice del primo TC di tipo itip
	jTc = getPrimoJTc(typeQuant,itip)

	!Calcolo la somma dei coefficienti dei TC ancora da considerare:
	sumCoeff=0
      do jtp=1,nTipi
	  jTcp = getPrimoJTc(typeQuant,jtp)
	  sumCoeff=sumCoeff+flow_rat(jTcp)*typeActr(jtp)
	enddo

	portata = flowTot*flow_rat(jTc) / sumCoeff

	return
	end



	integer*4 function getPrimoJTc(vTipi,itip)
	implicit none

	integer*4 vTipi(*) !I)
	integer*4 itip     !I)

      integer*4 primo
      integer*4 sommaV

	if(itip.eq.1)then
	  primo = 1
	else
	  primo = sommaV(vTipi,1,itip-1) + 1
	endif

	getPrimoJTc = primo
	return
	end	

	integer*4 function sommaV(vTipi,inizio,fine)
	implicit none

	integer*4 vTipi(*) !I)
	integer*4 inizio     !I)
	integer*4 fine     !I)

      integer*4 valore,jj

	valore=0
	do jj=inizio,fine
	  valore=valore+vTipi(jj)
	enddo

	sommaV = valore
	return
	end	

	subroutine portate_criterioP_serie(vert_pointer
     -                                   ,flowm
     -                                   ,type_quant
cgpe     -                                   ,type_act_old
     -                                   ,flow_rat
     -                                   ,qmin_assoluto
     -                                   ,hmin_assoluto
     -                                   ,qmax_assoluto
     -                                   ,hmax_assoluto
     -                                   ,qmin_tot
     -                                   ,qmax_tot
     -                                   ,vinc_min
     -                                   ,vinc_max
     -                                   ,tip_vinc
     -                                   ,qass
     -                                   ,hass
     -                                   ,l_ric
     -								   ,ier)
	! Determina i valori di portata lavorabili da tutti i TC accesi.
	! Questa subroutine è utilizzabile per centrali con TC bifase serie.
	! Si può presentare una delle seguenti 3 situazioni:
	! 1) La centrale è in grado di lavorare la portata richiesta.
	!    In questo caso la subroutine verifica per ogni compressore se il punto di lavoro
	!    è dentro mappa: in caso contrario riporta il punto di lavoro
	!    sulle curve limite.
	! 2) La portata richiesta è inferiore alla minima lavorabile dalla centrale
	! 3) La portata richiesta è superiore alle possibilità della centrale

	implicit none

      include '../inc/param.INC'
      include '../inc/stazione.INC'
      include '../inc/simpstat.INC'
      include '../inc/prototipo.INC'
      include '../inc/flag.INC'

      INTEGER*4 vert_pointer    !I)
	real*4	  unit_head		  !I)
	real*4	  flowm		      !I) portata richiesta
      integer*4 type_quant(*)   !I) numeri di TC per ogni tipo
cgpe      integer*4 type_act_old(*) !I) numeri di TC accesi per ogni tipo
	real*4	  flow_rat(*)	      !I) coeff. di ripartiz. Q per ogni compr attivo
      real*4    qmin_assoluto(maxunits) !I) qmin di 1^ f che può lavorare
      real*4    hmin_assoluto(maxunits) !I) 
      real*4    qmax_assoluto(maxunits) !I) qmax di 1^ f che può essere lavorata
      real*4    hmax_assoluto(maxunits) !I) 
     -         ,qmin_tot
     -         ,qmax_tot

      integer*4 vinc_min(maxunits)   !I) tipi di vincolo di minima di ciascun tipo di TC
     -         ,vinc_max(maxunits)   !I) tipi di vincolo di massima di ciascun tipo di TC
     -         ,tip_vinc(*)  !O) tipo di vinc da rispett per ogni tipo di TC
      real*4    qass(*)      !O) q compr (1^f==2^f) per ogni tipo di TC
      real*4    hass(*)      !O) 
      logical*2 l_ric		   !O)
	integer*4 ier		   !O)

      real*4    flow_ass ! somma delle portate assegnate ai TC già trattati
    
      character*(max_len) mess/' '/
	integer*4 i,itip

	!Inizializzazioni:
      do itip = 1,ntp
        qass(itip) = 0.
        hass(itip) = 0.
      enddo

      flow_ass = 0.

c flow_tot = parte di portata che deve essere ancora considerata ai fini della simulazione dei TC
	flow_tot = flowm - flow_ass
c
c-l_ric      l_ric = .true.
c-l_ric      do itip = 1,ntp
c-l_ric        if (type_actr(itip).gt.0) then
c-l_ric          if (vinc_min(itip) .ne. sur_1f .and.
c-l_ric     *        vinc_min(itip) .ne. sur_2f     ) then
c-l_ric            l_ric = .false.
c-l_ric          end if
c-l_ric        end if
c-l_ric      end do
      if (flow_tot .lt. qmin_tot) then
cgpe-new        if (fl_sim) then
cgpe-new          if (.not.iopt .and. .not.iopt_mess) then
cgpe-new            if (l_ric) then
cgpe-newc la centrale e' in riciclo
cgpe-new              call gest_error (3,0,'COMPSTAT_UP',
cgpe-new     *             'La centrale e'' in riciclo',0)
cgpe-new            else
cgpe-new              call gest_error (2,0,'COMPSTAT_UP',
cgpe-new     *             'Portata inferiore alla minima',0)
cgpe-new            end if
cgpe-new          end if
cgpe-new        end if
        do itip = 1,ntp
          if (type_actr(itip).gt.0) then
            if (vinc_min(itip) .ne. sur_1f .and.
     *          vinc_min(itip) .ne. sur_2f     ) then
              l_ric = .false.
            end if
          end if
        end do
        do itip = 1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_min(itip)
            qass(itip)     = qmin_assoluto(itip)
            hass(itip)     = hmin_assoluto(itip)
          endif
        enddo

        goto 100

      else if (flow_tot.gt.qmax_tot) then
c la centrale non puo' lavorare tutto il gas
cgpe-new        if (fl_sim) then
cgpe-new          if (.not.iopt .and. .not.iopt_mess) then
cgpe-new            call gest_error (2,0,'COMPSTAT_UP',
cgpe-new     *           'Portata superiore alla massima',0)
cgpe-new          end if
cgpe-new        end if

        do itip=1,ntp
          if (type_actr(itip).gt.0) then
            tip_vinc(itip) = vinc_max(itip)
            qass(itip)     = qmax_assoluto(itip)
            hass(itip)     = hmax_assoluto(itip)
          endif
        enddo

        goto 100

      endif
c
	call trattaFuoriMappa_serie
     -   (vert_pointer
     -   ,flow_tot
     -   ,type_quant
     -   ,type_actr
     -   ,ntp
     -   ,flow_rat
     -   ,qmin_assoluto
     -   ,hmin_assoluto
     -   ,qmax_assoluto
     -   ,hmax_assoluto
     -   ,vinc_min
     -   ,vinc_max
     -   ,iopt
     -   ,iopt_mess
     -   ,flow_ass
     -   ,tip_vinc
     -   ,qass
     -   ,hass
     -	)

	flow_tot = flowm-flow_ass
      call trattaNormale_serie
     -   (unit_head
     -   ,flow_tot           
     -   ,type_quant        
     -   ,flow_rat             
     -   ,qass    
     -   ,hass   
     -	)

cgpe	!Verifico se la portata richiesta è inferiore alla minima lavorabile dalla centrale.
cgpe	call verificaQmin_serie (flowm
cgpe     -                        ,qmin_assoluto
cgpe     -                        ,type_act_old
cgpe     -                        ,vinc_min
cgpe     -                        ,ntp
cgpe     -                        ,sur_1f ,sur_2f
cgpe     -                        ,iopt ,iopt_mess
cgpe     -                        )

cgpe	!Verifico se la portata richiesta è superiore alla massima lavorabile
cgpe	!dalla centrale.
cgpe	call verificaQmax_serie (flowm
cgpe     -                        ,qmax_assoluto
cgpe     -                        ,type_act_old
cgpe     -                        ,ntp
cgpe     -                        ,iopt ,iopt_mess
cgpe     -                        )

100   continue

	return
	end

	subroutine trattaFuoriMappa_serie
     -   (vert_pointer      !I)
     -   ,flow_totale       !I) portata da lavorare
     -   ,type_quant        !I) vettore del numero di TC per ogni tipo
     -   ,type_actr         !I) numero di TC accesi per ogni tipo di TC
     -   ,ntp               !I) dimensione di type_quant
     -   ,flow_rat          !I) coeff. di ripartiz. per ogni compr attivo
     -   ,qmin_assoluto     !I) qmin fase1 di ciascun tipo di TC
     -   ,hmin_assoluto     !I) mmin fase1 di ciascun tipo di TC
     -   ,qmax_assoluto     !I) qmax fase1 di ciascun tipo di TC
     -   ,hmax_assoluto     !I) hmax fase1 di ciascun tipo di TC
     -   ,vinc_min          !I) tipi di vincolo di minima di ciascun tipo di TC
     -   ,vinc_max          !I) tipi di vincolo di massima di ciascun tipo di TC
     -   ,iopt              !I)
     -   ,iopt_mess         !I)
     -   ,flow_ass          !I,O)
     -   ,tip_vinc   !O) tipo di vincolo da rispettare per ciascun tipo di TC
     -   ,qass    !O) q compr (1^f==2^f) per ogni tipo di TC
     -   ,hass    !O) 
     -	)
	! Tratta tutti i compressori fuori mappa e, di volta in volta, riporta
	! sulla curva limite (a sinistra o a destra) il compressore che maggiormente
	! si discosta dalle curve limite.

	implicit none
      include '../inc/param.INC'
      include '../inc/stazione.INC'

      integer*4 vert_pointer
      real*4    flow_totale          
	integer*4 type_quant(*)   
	integer*4 type_actr(*)   
	integer*4 ntp   
	real*4	  flow_rat(*)	       
      real*4    qmin_assoluto(*) 
      real*4    hmin_assoluto(*)
      real*4    qmax_assoluto(*) 
      real*4    hmax_assoluto(*)
	integer*4 vinc_min(*)   
	integer*4 vinc_max(*)   
      logical*2 iopt
      logical*2 iopt_mess
      real*4    flow_ass
      integer*4 tip_vinc(*)  
      real*4    qass(*)
      real*4    hass(*)

      real*4    flow_tot
	integer*4 jTc
	real*4 portata
      character*(max_len) mess/' '/
	integer*4 ker,ier,itip
	logical *2 bFuoriMappa
	logical *2 bFuoriMappaSur
	real*4 dQ ! scostamento dalla curva limite
	real*4 dQMax ! scostamento max dalla curva limite
	real*4 qTcMin ! portata min  al TC
	real*4 qTcMax ! portata max  al TC
	integer*4 jFuori ! indice del tipo di TC maggiormente fuori mappa


      flow_tot = flow_totale

	bFuoriMappa=.true.
	do while (bFuoriMappa)
	  ! cerco di individuare il jFuori-esimo tipo di TC
	  ! più limitante, cioè che si discosta maggiormente
	  ! dalle curve o di anti surge o di anti chocking:
	  bFuoriMappa=.false.
	  dQMax=0
        do itip=1,ntp
	    if (type_actr(itip).gt.0) then
	      !Calcolo la portata in base al criterio di ripartizione delle portate:
	      call criterio_P (flow_tot,flow_rat,type_quant,type_actr,ntp
     *                      ,itip,portata)

            qTcMin=qmin_assoluto(itip)
            if (portata.le.qTcMin) then
	        dQ=abs(portata-qTcMin)
	        if (dQ.gt.dQMax) then
	          dQMax=dQ
	          jFuori=itip
	          bFuoriMappaSur=.true.
	          bFuoriMappa=.true.
	        endif
	      endif

            qTcMax=qmax_assoluto(itip)
            if (portata.ge.qTcMax) then
	        dQ=abs(portata-qTcMax)
	        if (dQ.gt.dQMax) then
	          dQMax=dQ
	          jFuori=itip
	          bFuoriMappaSur=.false.
	          bFuoriMappa=.true.
	        endif
	      endif

	    endif
	  end do

	  ! tratto il compressore più limitante (il jFuori-esimo tipo)
	  if (bFuoriMappa) then

	    if (bFuoriMappaSur) then

	      ! gestione dei messaggi ---------------------------------
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_min(jFuori).eq.sur_1f)    .or.
     *            (vinc_min(jFuori).eq.sur_2f))    then
cgpe-mess                 write(mess,558) jFuori, 'Parabola'
                ker = 21
              else
cgpe-mess                 write(mess,558) jFuori, 'Giri'
                ker = 22
              endif
cgpe-mess558           format('Tipo vincolato:',i2,' - Tipo vincolo:',a)
cgpe-mess              ker = 93
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'COMPSTAT_UP',
cgpe-new     *             'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'COMPSTAT_UP',
cgpe-new     *             'Unita'' vincolata al minimo',0)
            end if
	      ! -------------------------------------------------------

            tip_vinc(jFuori) = vinc_min(jFuori)
            qass(jFuori)     = qmin_assoluto(jFuori)
            hass(jFuori)     = hmin_assoluto(jFuori)
	    else
	      ! oltre l'anti chocking o oltre la curva di max n. di giri o di max potenza

	      ! gestione dei messaggi ---------------------------------
            if (.not.iopt .and. .not.iopt_mess) then
              if ((vinc_max(jFuori).eq.cho_1f)    .or.
     *            (vinc_max(jFuori).eq.cho_2f))    then
cgpe-mess                write(mess,558) jFuori, 'Parabola'
                ker = 24
              elseif ((vinc_max(jFuori).eq.mar_1f) .or.
     *                (vinc_max(jFuori).eq.mar_2f)) then
cgpe-mess                write(mess,558) jFuori, 'Giri'
                ker = 23
              else
cgpe-mess                write(mess,558) jFuori, 'Potenza'
                ker = 25
              endif
cgpe-mess              ker = 94
              mess = ' '
              ier = 0
              call app_mess(vert_pointer,ker,mess)
cgpe-new              call gest_error (3,0,'COMPSTAT_UP',
cgpe-new     *             'Non si rispetta il criterio',0)
cgpe-new              call gest_error (3,0,'COMPSTAT_UP',
cgpe-new     *             'Unita'' vincolata al massimo',0)
            end if
	      ! -------------------------------------------------------

            tip_vinc(jFuori)  = vinc_max(jFuori)
            qass(jFuori)      = qmax_assoluto(jFuori)
            hass(jFuori)      = hmax_assoluto(jFuori)
	    endif

          flow_ass = flow_ass + qass(jFuori) * type_actr(jFuori)
	    type_actr(jFuori) = 0
          flow_tot = flow_totale - flow_ass
	  endif ! bFuoriMappa

	end do ! bFuoriMappa

	return
	end

	subroutine trattaNormale_serie
     -   (head              !I)
     -   ,flow_totale       !I) portata da lavorare
     -   ,type_quant        !I) vettore del numero di TC per ogni tipo
     -   ,flow_rat          !I) coeff. di ripartiz. per ogni compr attivo
     -   ,qass    !O) q compr (1^f==2^f) per ogni tipo di TC
     -   ,hass    !O) 
     -	)
	implicit none
      include '../inc/param.INC'
      include '../inc/stazione.INC'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
      include '../inc/MF_SIMPSTAT.INC'

      include '../inc/compstat.INC '	

	real*4    head
	real*4    flow_totale
	integer*4 type_quant(*)   
	real*4	  flow_rat(*)	       
      real*4    qass(*)
      real*4    hass(*)
	 
      real*4    zero_giri
      EXTERNAL  zero_giri

	integer*4 itip  
	real*4 portata 
      real*4 ra,rb,rd,rsol
      real*4 qcrit_1f ! portata prima fase per il rispetto criterio
      real*4 qaux
      real*4 rev_min,rev_max
	real*4 uno_su_ro_attuale,rev,haux

      real*4    flowTot          
	flowTot = flow_totale


c zin e' nel common compstat.INC
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Zin*tin/pin
      aux3     = ROCN*uno_su_ro_attuale

      do itip=1,ntp
        if (type_actr(itip).gt.0) then
	    call criterio_P (flowTot,flow_rat,type_quant,type_actr,ntp
     *                    ,itip,portata)

	    !================ aree common ==================
	    flow_unit = portata
          itipo = itip
          un_flow1 = flow_unit
          flow     = flow_unit/aux3
	    !================ aree common ==================

          call limiti_rev(flow_unit,rev_min,rev_max,itip)
          itipo = itip
          rb = zero_giri(rev_min)
          ra = zero_giri(rev_max)
          if (((ra*rb).lt.0)         .or.
     *         (abs(ra).le.eps_giri) .or.
     *         (abs(rb).le.eps_giri) ) then
            if ((ra*rb).lt.0 ) then
              rd = rev_max
cgpe-fun              call zero_fun1(rev_min,rev_max,rd,rb,ra,rsol,
              call zero_fun(rev_min,rev_max,rd,rb,ra,rsol,
     *                  epsma,pmach,zero_giri)
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
          if (flow_unit.le.qaux) then
            call hdaqn(haux,flow_unit,rev,chn(1,itipo))
          else
            call hdaqn(haux,flow_unit,rev,chc(1,itipo))
          end if
          qass(itip)      = flow_unit
          hass(itip)      = haux
	    flowTot = flowTot - portata*type_actr(itip)
          type_actr(itip) = 0
        end if
      end do

	return
	end