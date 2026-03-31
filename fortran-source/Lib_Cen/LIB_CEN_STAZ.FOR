c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Subroutine generiche necessarie per i modelli delle centrali
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c********************************************************************************************
c  Lib_Cen_Staz.for
c*******************************************************************************************
      Subroutine init_state (istaz,ivert)
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.INC'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
*&*
      INTEGER*4 istaz,ivert     ! I)

      INTEGER*4 i,j,k,kk,jk,i1,ii1
c-----------------------------------------------------------------------
      call init_stvar(istaz,ivert)

!-determinazione di type_avail
      i1 = first_unit(ivert)
      do j = 1,type_num(ivert)
        jk = first_type(ivert)+j-1
        type_avail(jk) = 0
        type_act(jk) = 0
        do kk = 1,type_quant(jk)
          ii1 = i1+kk-1
          if (unit_avail(ii1).ge.1) type_avail(jk) = type_avail(jk)+1
          if (status(ii1).eq.funz_on) type_act(jk) = type_act(jk)+1
        enddo
        i1 = i1+type_quant(jk)
      enddo

!- determinazione di vert_typ
      call init_vert_type(ivert,type_quant(first_type(ivert)),
     *     vert_typ(IVERT))
c
      return
      end

!======================================================================
      subroutine init_stvar(istaz,vert_ind)
c
c  Calcolo delle grandezze dipendenti dalla composizione del gas necessarie
c  alla sola simulazione di una stazione (pmol,rocn)
c
c     istaz          I  indice della stazione nella struttura dati dimensionata
c                       a M_OGG (coincide con ind di stazione.inc)
c     vert_ind       I  indice della stazione nella struttura interna di
c                       dati di stazione
c*******************************************************************************************
c_e  M_OGG =dimensionamento massimo della topologia per le strutture dati delle simulazioni
c_e  (vd. param.inc)
c*******************************************************************************************

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk.inc'
c
      INTEGER*4  istaz,vert_ind    ! I)
c
c      real*4   pmin,pmax,tmin,tmax
      real*4   prop
      external prop

c-----------------------------------------------------------------------
cgpec questi limiti vanno forse rivisti (ma considerando che la z forse 
cgpec serve solo per l'input)
cgpe      pmin=20.
cgpe      pmax=120.
cgpe      tmin=260.
cgpe      tmax=360.
cgpe      call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef(1,vert_ind))
 
      pmol = av_weight(istaz)
CMAR
CMAR      ZCN = prop(PCN,TCN,z_coef(1,vert_ind))

      call zpt1 (istaz,PCN,TCN,ZCN)
      ROCN = (pmol*PCN)/(erre*ZCN*TCN)*(1.e5)
c
cgpe      call regre_exp(istaz)
      return
      end

!======================================================================
      SUBROUTINE init_vert_type (ivert,type_act_un,vert_type)
c*****************************************************************************
c  vengono contate le unità monofase e bifase presenti nella stazione e si 
c  stabilisce se è semplice, mista o composta
c*****************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
*&*
      INTEGER*4 ivert,type_act_un(*)
     *         ,vert_type

      INTEGER*4 iold, ii1, I, j, K, num_mono, num_bifa,j_un
c-----------------------------------------------------------------------
!- determinazione di vert_type
      num_mono = 0
      num_bifa = 0
      j_un = 0
      iold = first_unit(ivert)
      do i = 1,type_num(ivert)
        j = first_type(ivert)+i-1
        j_un = j_un + 1
        if (type_act_un(j_un).gt.0) then
          do k=1,type_quant(j)
            ii1 = iold+k-1
            if (unit_bifase(ii1)) then
              num_bifa = num_bifa + 1
            else
              num_mono = num_mono + 1
            end if
          end do
        end if
        iold = iold+type_quant(j)
      end do
c
      if (num_mono.gt.0 .and. num_bifa.gt.0) then
        vert_type = tipo_mist
      else
        if (num_mono.gt.0) then
          vert_type = tipo_semp
        else
          vert_type = tipo_comp
        endif
      endif
c
      return
      end

cmar-Eq2+++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE init_vert_EQ2 (ivert,type_act_un,vert_type)
c*****************************************************************************
c  vengono contate le unità monofase e bifase presenti nella stazione e si 
c  stabilisce se è semplice, mista o composta
c*****************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
*&*


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
      INTEGER*4 ivert,type_act_un(*)
     *         ,vert_type

      INTEGER*4 iold, ii1, I, j, K, num_mono, num_bifa,j_un
c-----------------------------------------------------------------------

     


      mm=0

     

c------------------------------------------------------------------

!- determinazione di vert_type
   
      j_un = 0
      iold = first_unit(ivert)
      do i = 1,type_num(ivert)
        j = first_type(ivert)+i-1
        j_un = j_un + 1
 
c
      do m= 1, type_num(ivert)

      mm= type_quant(m)+mm

	 perc_equi_min(m) = perc_equi_min(j)
	perc_equi_max(m) = perc_equi_max(j)
	flag_equi_s(m)=flag_equi_s(j)
	kmax_cho(m) = kmax_cho(j) 
	kmin_sur(m) = kmin_sur(j)
      
	enddo
c
        if (type_act_un(j_un).gt.0) then
          do k=1,type_quant(j)
            ii1 = iold+k-1
           
          end do
        end if
        iold = iold+type_quant(j)
      end do
c
    
c
      return
      end

cmar-Eq2++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




!======================================================================
      Subroutine gest_errore(istaz,ind_err,tipo_criterio,vert_type,
     *           stat_ord,ier)
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
*&*
      integer*4 istaz      ! I) indice della centrale
      integer*4 ind_err    ! I) tipo di errore su cui fare il check
      integer*4 tipo_criterio,vert_type,stat_ord
      integer*4 ier        ! O) errore restituito: =0 , se ind_err non e' fatal
                           !                       =11, se ind_err e' fatal

      logical*2 ce_serie
c-------------------------------------------------------------------------
c Controllo sugli errori che derivano dalla fattibilita' dei punti sulle
c due fasi.
c Se ind_err e' uguale ad uno di questi errori, si esce dall'ottimizzazione.
c * per tutti i criteri e tipi di funzionamento
c   ind_err = 39 (richiesta compressione con tutte le unita' spente)
c * per tutti i criteri e tipi di funzionamento e se la centrale e' in
c   serie (cap bon o funzionamento booster di Corte):
c     - ind_err = 63: non si rispetta il criterio di UC fra le due fasi,
c                     anche se il punto calcolato e' fattibile su entrambe
c                     le fasi
c     - ind_err = 64: non esistono punti fattibili comunque si fa variare
c                     la pressione intermedia fra le due fasi (e dunque non
c                     si rispetta il criterio UC)
c * se tipo_criterio = crit_port
c     se (vert_type.eq.2) e (stat_ord.eq.2):
c       - ind_err = 1 (dalla calc_punti_bifase: non esistono punti fattibili)
c       - ind_err = 4 (flow2.le.0)
c       - ind_err = 38 ((pin.le.0).or.(pout.le.0))
c       - ind_err = 5 (pin.gt.pout)
c       - ind_err = 71 (pout.gt.pmax)
c       - ind_err = 73 (pout.lt.pmin)
!c       - ind_err = 52 (dalla calc_punti_bifase: non esistono punti fattibili)
c
c     altrimenti se (vert_type = 1) oppure
c                   (vert_type = 3) oppure
c                   (vert_type = 2) e (stat_ord = 1):
c       - ind_err = 1 (dalla limiti_q_bifase: al min_rev si supera la max_power)
c       - ind_err = 4 (flow2.lt.0)
c       - ind_err = 38 ((pin.lt.0).or.(pout.lt.0))
c       - ind_err = 5 (pin.gt.pout)
c       - ind_err = 6 (head.gt.hmax)
c       - ind_err = 7 (head.lt.hmin)
!c       - ind_err = 37 (dalla limiti_q_bifase: al min_rev si supera la max_power)
c
c * se tipo_criterio = crit_giri:
c       - ind_err = 1 (errore grave)
c       - ind_err = 4 (dalla station o multi_stadio, se flow2.le.0)
c       - ind_err = 5 (dalla station o multi_stadio, se pin.gt.pout)
c       - ind_err = 19 (dalla station o multi_stadio, se (pin.le.0) oppure se
c                   (pout.le.0))
c
c * se tipo_criterio = crit_equi:
c     se (vert_type.eq.2) e (stat_ord.eq.2):
c       - ind_err = 1 (dalla calc_punti_bifase: non esistono punti fattibili,
c                      dalla calc_clim,
c                      dalla compstat_mf)
c       - ind_err = 4 (flow2.le.0)
c       - ind_err = 38 ((pin.le.0).or.(pout.le.0))
c       - ind_err = 5 (pin.gt.pout)
c       - ind_err = 71 (pout.gt.pmax)
c       - ind_err = 73 (pout.lt.pmin)
!c       - ind_err = 52 (dalla calc_punti_bifase: non esistono punti fattibili)
!c       - ind_err = 13 (dalla compstat_mf)
!c       - ind_err = 70 (dalla calc_clim)
c
c     altrimenti se (vert_type = 1) oppure
c                   (vert_type = 3) oppure
c                   (vert_type = 2) e (stat_ord = 1):
c       - ind_err = 1 (dalla limiti_q_bifase: al min_rev si supera la max_power,
c                      dalla calc_clim)
c       - ind_err = 4 (flow2.lt.0)
c       - ind_err = 38 ((pin.lt.0).or.(pout.lt.0))
c       - ind_err = 5 (pin.gt.pout)
c       - ind_err = 6 (head.gt.hmax)
c       - ind_err = 7 (head.lt.hmin)
!c       - ind_err = 37 (dalla limiti_q_bifase: al min_rev si supera la max_power)
c-------------------------------------------------------------------------
      ier = 0
      if (ind_err.eq.39) then
        ier = 1 ! ier = 11
        return
      endif
      if (ce_serie(istaz)) then
cgpe        if ((ind_err.eq.11).or.(ind_err.eq.12)) then
        if ((ind_err.eq.63).or.(ind_err.eq.64)) then
          ier = 1 ! ier = 11
          return
        end if
      endif
      if (tipo_criterio.eq.crit_port) then
c se tipo_criterio = crit_port
        if ((vert_type.eq.tipo_comp).and.(stat_ord.eq.f_serie)) then
          if((ind_err.eq.1).or.(ind_err.eq.4).or.(ind_err.eq.38).or.
cgpe     *       (ind_err.eq.5).or.(ind_err.eq.21).or.(ind_err.eq.23)) then
     *       (ind_err.eq.5).or.(ind_err.eq.71).or.(ind_err.eq.73)) then
            ier = 1 ! ier = 11
            return
          end if
        else
          if((ind_err.eq.1).or.(ind_err.eq.4).or.(ind_err.eq.38).or.
     *       (ind_err.eq.5).or.(ind_err.eq.6).or.(ind_err.eq.7)) then
            ier = 1 ! ier = 11
            return
          end if
        end if
      else if (tipo_criterio.eq.crit_giri) then
c se tipo_criterio = crit_giri
        if ((ind_err.eq.1).or.
     *      (ind_err.eq.4).or.(ind_err.eq.5).or.
     *      (ind_err.eq.19)                          ) then
          ier = 1 ! ier = 11
          return
cgpe-new
        elseif ((ind_err.eq.17) .and. (stat_ord.eq.f_serie)) then
          ier = 1 ! ier = 11
          return
cgpe-new-end
        end if
      else
c tipo_criterio = crit_equi
        if ((vert_type.eq.tipo_comp).and.(stat_ord.eq.f_serie)) then
          if((ind_err.eq.1).or.(ind_err.eq.38).or.(ind_err.eq.4).or.
cgpe     *       (ind_err.eq.5).or.(ind_err.eq.21).or.
cgpe     *       (ind_err.eq.23).or.(ind_err.eq.13)) then
     *       (ind_err.eq.5).or.(ind_err.eq.71).or.
cgpe     *       (ind_err.eq.73).or.(ind_err.eq.13)) then
     *       (ind_err.eq.73)                             ) then
            ier = 1 ! ier = 11
            return
          end if
        else
          if((ind_err.eq.1).or.(ind_err.eq.4).or.(ind_err.eq.38).or.
     *       (ind_err.eq.5).or.(ind_err.eq.6).or.(ind_err.eq.7)) then
            ier = 1 ! ier = 11
            return
          end if
        end if
      end if

      return
      end

!======================================================================
csave        Subroutine agg_station(stat_ord,type_num,type_quant_old,
csave     *          unit_num,jtfir,jufir,
csave     *          pres_int,temp_int,delprint,delt_int,lstat)
        Subroutine agg_station(unit_num,jufir,lstat)
c
c       Nel caso del criterio dei giri, aggiornamento dei dati dei
c       compressori in caso di simulazione oppure correzione OK
c-----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/MF_UNITS.INC'

cvar      integer*4 stat_ord,type_num,type_quant_old(*),
cvar     *          unit_num,jtfir,jufir
      integer*4 unit_num,jufir
csave      real*4    temp_int,pres_int,delt_int,delprint
      LOGICAL*2 lstat(*)

      integer*4 i,j,k
c-----------------------------------------------------------------------c
      j = 0
      do i = 1,unit_num
        j = j + 1
        k = jufir+i-1
        if (lstat(j)) then
          unit2_flow(k) = unit_flow(k+unit_num)
          unit2_head(k) = unit_head(k+unit_num)
          unit2_rev(k)  = unit_rev(k+unit_num)
          unit2_perc(k) = unit_perc(k+unit_num)
          unit2_eff(k)  = unit_eff(k+unit_num)
          unit2_max(k)  = unit_max(k+unit_num)
          unit2_min(k)  = unit_min(k+unit_num)
          unit2_power(k)      = unit_power(k+unit_num)
          unit2_temp(k) = unit_temp(k+unit_num)
c-din_corr
          unit_cons(k) = unit_cons(k)+ unit_cons(k+unit_num)
          unit_cons(k+unit_num) = 0.
c-din_corr-end
        endif
      enddo
c
      return
      end

!======================================================================
      subroutine calc_dati_ISO(IVERT,k)
C
C NOTA: tutte le percentuali vengono considerate normalizzate [0:1], dunque non
C       si deve moltiplicare per 100
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/STATIONS.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/argo_reale_ris.inc'
c
      INTEGER*4 IVERT     ! I) indice centrale
      INTEGER*4 k         ! I) indice dell'unita' che si sta considerando

c-bp05
      real*4    fc,tadim,pwmax
c-bp05-end
c
c I campi UNIT_POWER e UNIT2_POWER sono le potenze richieste dal compressore
c di prima fase e di seconda fase, alle condizioni reali (ON SITE)
 

!-potenza richiesta in Kw fase 1 (N.U.)
      power_ric(k) = unit_power(k) * unit_eff_corr(k)
!-potenza richiesta % fase 1 [0:1] (N.U.)
      power_ric_perc(k) = (power_ric(k)/nom_power(k))
c
c unit_power_iso = potenza richiesta dal compressore fase 1 in condizioni ISO
c - correzione dovuta all'altitudine
      unit_power_iso(k) = unit_power(k)/stat_vars(1,ivert)
c - correzione dovuta alla temperatura
      tadim = taria(IVERT)/(trif+t0)
      fc    = type_cpwt(1,k)+tadim*(type_cpwt(2,k)+tadim*
     *        (type_cpwt(3,k)+type_cpwt(4,k)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
      unit_power_iso(k) = unit_power_iso(k)/fc

!-POWER: potenza fornita dalla turbina in Kw (ISO)   -> variabile TBGOUT_POWER
c contributo del compressore di prima fase
      power(k) = unit_power_iso(k)

      unit2_power_iso(k) = 0.
      if (unit_bifase(k)) then
!-potenza richiesta in Kw fase 2  (N.U.)
        power2_ric(k) = unit2_power(k) * unit2_eff_corr(k)
!-potenza richiesta % fase 2  [0:1] (N.U.)
        power2_ric_perc(k) = (power2_ric(k)/nom_power(k))

c unit_power_iso = potenza richiesta dal compressore fase 2 in condizioni ISO
c - correzione dovuta all'altitudine
        unit2_power_iso(k) = unit2_power(k)/stat_vars(1,ivert)
c - correzione dovuta alla temperatura
        unit2_power_iso(k) = unit2_power_iso(k)/fc

!-POWER: potenza fornita dalla turbina in Kw in condizioni ISO (e' quella cumulativa)
c eventuale contributo del compressore di seconda fase
        power(k) = power(k) + unit2_power_iso(k)
      endif

!-!-POWER_PERC: potenza % fornita dalla turbina (ISO) [0:1] -> variabile TBGOUT_POWER_PERC
      power_perc(k) = (power(k)/nom_power(k))

c-bp05
c calcolo della potenza nominale tenendo conto della variazione della potenza massima
c power(k) = unit_power(k)/pow_loss senza il contributo delta dovuto all'altitudine <> 0.
c      pp = nom_power(k) * unit_power_corr(k)
c potenza nominale della turbina tenendo conto del derating della turbina
      nom_pow_der(k) = nom_power(k)*
     *                (unit_power_corr(k)/stat_vars(1,ivert))
      power_perc_var(k) = (power(k)/nom_pow_der(k))

c calcolo della massima potenza al numero di giri del compressore
c      rev = unit_rev(k)*100.
      call pwdan(pwmax,unit_rev(k),taria(ivert),
     *     type_cpwm(1,k),type_cpwt(1,k))

c pwmax in uscita dalla pwdan e' alla tempertura taria e all'altitudine h=0
c fc = fattore correttivo dovuto alla temperatura per renderlo ISO
      pwmax = pwmax/fc

c calcolo massima potenza derat al numero di giri del compressore
      pwmax = pwmax * (unit_power_corr(k)/stat_vars(1,ivert))
      power_perc_rev(k) = (power(k) / pwmax)
c-bp05-end

c unit_hrate_onsite = heat-rate ON-SITE (alle condizioni operative)
      unit_hrate_onsite(k) = unit_hrate(k)

c Calcolo dell'heat-rate in condizioni ISO (unit_hrate)
      tadim = taria(IVERT)/(trif+t0)
      fc    = type_chrt(1,k)+tadim*(type_chrt(2,k)+tadim*
     *        (type_chrt(3,k)+type_chrt(4,k)*tadim))
      unit_hrate(k) = unit_hrate(k)/fc
c
c Potenza fornita dalla turbina ON SITE (non c'è alcun fattore di derating)
      power_onsite(k) = unit_power(k)
      if (unit_bifase(k)) then
        power_onsite(k) = power_onsite(k) + unit2_power(k)
      endif
cc Potenza fornita dalla turbina ON SITE tenendo conto del fattore di derating
cc (se il fattore di derating va ad abbassare la max_power, allora va applicato
cc alla potenza richiesta dal compressore ON SITE aumentandola, cioè nella
cc direzione opposta)


cmar_max_pow

      power_onsite(k) = power_onsite(k) /
     *               (unit_power_corr(k)/stat_vars(1,ivert))

cmar_max_pow


      return
      end

      subroutine calc_dati_ISO_AS(IVERT,k,ii)
C
C NOTA: tutte le percentuali vengono considerate normalizzate [0:1], dunque non
C       si deve moltiplicare per 100
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/STATIONS.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/argo_reale_ris.inc'
c
      INTEGER*4 IVERT     ! I) indice centrale
      INTEGER*4 k         ! I) indice dell'unita' che si sta considerando

	integer*4 ii

c-bp05
      real*4    fc,tadim,pwmax


c-bp05-end
c
c I campi UNIT_POWER e UNIT2_POWER sono le potenze richieste dal compressore
c di prima fase e di seconda fase, alle condizioni reali (ON SITE)

      ii= first_unit(ivert)+ii-1
      

!-potenza richiesta in Kw fase 1 (N.U.)
      power_ric(k) = unit_power(k) * unit_eff_corr(ii)
!-potenza richiesta % fase 1 [0:1] (N.U.)
      power_ric_perc(k) = (power_ric(k)/nom_power(ii))
c
c unit_power_iso = potenza richiesta dal compressore fase 1 in condizioni ISO
c - correzione dovuta all'altitudine
      unit_power_iso(k) = unit_power(k)/stat_vars(1,ivert)
c - correzione dovuta alla temperatura
      tadim = taria(IVERT)/(trif+t0)
      fc    = type_cpwt(1,ii)+tadim*(type_cpwt(2,ii)+tadim*
     *        (type_cpwt(3,ii)+type_cpwt(4,ii)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
      unit_power_iso(k) = unit_power_iso(k)/fc

!-POWER: potenza fornita dalla turbina in Kw (ISO)   -> variabile TBGOUT_POWER
c contributo del compressore di prima fase
      power(k) = unit_power_iso(k)

      unit2_power_iso(k) = 0.
      if (unit_bifase(k)) then
!-potenza richiesta in Kw fase 2  (N.U.)
        power2_ric(k) = unit2_power(k) * unit2_eff_corr(ii)
!-potenza richiesta % fase 2  [0:1] (N.U.)
        power2_ric_perc(k) = (power2_ric(k)/nom_power(ii))

c unit_power_iso = potenza richiesta dal compressore fase 2 in condizioni ISO
c - correzione dovuta all'altitudine
        unit2_power_iso(k) = unit2_power(k)/stat_vars(1,ivert)
c - correzione dovuta alla temperatura
        unit2_power_iso(k) = unit2_power_iso(k)/fc

!-POWER: potenza fornita dalla turbina in Kw in condizioni ISO (e' quella cumulativa)
c eventuale contributo del compressore di seconda fase
        power(k) = power(k) + unit2_power_iso(k)
      endif

!-!-POWER_PERC: potenza % fornita dalla turbina (ISO) [0:1] -> variabile TBGOUT_POWER_PERC
      power_perc(k) = (power(k)/nom_power(ii))

c-bp05
c calcolo della potenza nominale tenendo conto della variazione della potenza massima
c power(k) = unit_power(k)/pow_loss senza il contributo delta dovuto all'altitudine <> 0.
c      pp = nom_power(k) * unit_power_corr(k)
c potenza nominale della turbina tenendo conto del derating della turbina
      nom_pow_der(k) = nom_power(ii)*
     *                (unit_power_corr(ii)/stat_vars(1,ivert))
      power_perc_var(k) = (power(k)/nom_pow_der(k))

c calcolo della massima potenza al numero di giri del compressore
c      rev = unit_rev(k)*100.
       

      call pwdan(pwmax,unit_rev(k),taria(ivert),
     *     type_cpwm(1,ii),type_cpwt(1,ii))

      

c pwmax in uscita dalla pwdan e' alla tempertura taria e all'altitudine h=0
c fc = fattore correttivo dovuto alla temperatura per renderlo ISO
      pwmax = pwmax/fc
    

c calcolo massima potenza derat al numero di giri del compressore
      pwmax = pwmax * (unit_power_corr(ii)/stat_vars(1,ivert))

      power_perc_rev(k) = (power(k) / pwmax)


c-bp05-end

c unit_hrate_onsite = heat-rate ON-SITE (alle condizioni operative)
      unit_hrate_onsite(k) = unit_hrate(k)

c Calcolo dell'heat-rate in condizioni ISO (unit_hrate)
c   ATTENZIONE: I COEFF HANNO ii PERCHè HANNO UN V ETTORE CONTINUO
C   MENTRE I VALORI DI UNITà VIAGGIANO SU K

      tadim = taria(IVERT)/(trif+t0)
      fc    = type_chrt(1,ii)+tadim*(type_chrt(2,ii)+tadim*
     *        (type_chrt(3,ii)+type_chrt(4,ii)*tadim))
      unit_hrate(k) = unit_hrate(k)/fc
c
c Potenza fornita dalla turbina ON SITE (non c'è alcun fattore di derating)
      power_onsite(k) = unit_power(k)
      if (unit_bifase(k)) then
        power_onsite(k) = power_onsite(k) + unit2_power(k)
      endif
cc Potenza fornita dalla turbina ON SITE tenendo conto del fattore di derating
cc (se il fattore di derating va ad abbassare la max_power, allora va applicato
cc alla potenza richiesta dal compressore ON SITE aumentandola, cioè nella
cc direzione opposta)
cmar_max_pow


      power_onsite(k) = power_onsite(k) / 
     *                  (unit_power_corr(k)/stat_vars(1,ivert))

cmar_max_pow
      
 

      ii= ii-first_unit(ivert)+1
      return
      end
   
!======================================================================
cgpe      subroutine imp_new_sta(vert_ind,val_prop,new_sta)
      subroutine imp_new_sta(istaz,vert_ind,val_prop,new_sta)
c
c imposizione del nuovo stato e dei nuovi valori di pressione da imporre
c NOTA: non viene modificato il tipo regolazione (htcitj) e il tipo di set
c       pmsetj e pvsetj ma lo stato (hstatj) e il valore da imporre
c       (memorizzato in valsjc). Lo stato hstatj viene modificato nella
c       function lnsta se non coincide con lo stato precedente
c
      implicit none
      include '../inc/param.inc'
c      include '../inc/stazione.inc'
      include '../inc/th.inc '	! :in
      include '../inc/jc.inc '	! :out
      include '../inc/stations.inc'
c
      INTEGER*4     istaz,vert_ind,i,j
      real*4        val_prop      ! I) valore proposto dalla correzione
      character*(*) new_sta
      logical*2     lvar,lnsta
      external      lnsta
c
cgpe      if (new_sta.eq.funz_off) then
cgpe        lvar = lnsta(ind,sta_off,new_sta)
      if (new_sta.eq.'OFF') then
        lvar = lnsta(istaz,sta_off,new_sta)
cgpe-corr        val_prop = 0.
        valsjc(istaz) = val_prop
c spengo tutte le unita'
        call spegni_staz(unit_num(vert_ind),first_unit(vert_ind))
      elseif (new_sta.eq.'FR') then
        lvar = lnsta(istaz,sta_fr,new_sta)
cgpe-corr        val_prop = 0.
        valsjc(istaz) = val_prop
c spengo tutte le unita'
        call spegni_staz(unit_num(vert_ind),first_unit(vert_ind))
      elseif (new_sta.eq.'NR') then
        lvar = lnsta(istaz,sta_nr,new_sta)
cgpe-corr        val_prop = 0.
        valsjc(istaz) = val_prop
c spengo tutte le unita'
        call spegni_staz(unit_num(vert_ind),first_unit(vert_ind))
      elseif (new_sta.eq.'PV') then
        lvar = lnsta(istaz,sta_pv,new_sta)
        valsjc(istaz) = val_prop
      elseif (new_sta.eq.'PM') then
        lvar = lnsta(istaz,sta_pm,new_sta)
        valsjc(istaz) = val_prop
cgpe-new
      elseif (new_sta.eq.'Q') then
        lvar = lnsta(istaz,sta_q,new_sta)
        valsjc(istaz) = val_prop
cgpe-new-end
      endif 
c
      return
      end

!======================================================================
      subroutine set_stato_stazione(set_flag,istaz)
c
c   Settaggio del flag per indicare se una stazione risulta spenta
c
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc '	! :in
      include '../inc/tj.inc '	! :in/out
c
      INTEGER*4       istaz      ! I)
      INTEGER*4       set_flag   ! O)
c
      if (hstatj(istaz).eq.sta_fr .or.hstatj(istaz).eq.sta_on.or.
     *    hstatj(istaz).eq.sta_off.or.hstatj(istaz).eq.sta_nr  ) then
c centrale spenta
        set_flag = 0
      else
        set_flag = 1
      endif
c
      return
      end

!======================================================================
      subroutine spegni_staz(unit_num,first_unit)
c
c  Vengono spente tutte le unita'
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/UNITS.INC'
c
      integer*4 unit_num,first_unit   ! I)
c
      integer*4 i,j
c-----------------------------------------------------------------------
      do i = 1,unit_num
        j = first_unit+i-1
        status(j) = funz_off
      end do
cgpe      if ((hstatj(ind).eq.sta_on .or.
cgpe     *    hstatj(ind).eq.sta_nr) .and.
cgpe     *    htcitj(ind).ne.tc_nr) then
cgpe        htcitj(ind)=tc_nr
cgpe      endif
      return
      end

!======================================================================
      function lnsta(i,hsta,sta)

!   Cambia lo stato di un elemento
!	i,      ! :in offset elemento
!	hsta,   ! :in codice nuovo stato
!	sta     ! :in stringa nuovo stato

*&*
      implicit none
*&*
      include '../inc/param.inc '	! :costanti
      include '../inc/th.inc '	! :costanti
      include '../inc/tj.inc '	! :costanti
*&*
      integer*4     i
      integer*4     hsta
      logical*2     lnsta
      character*(*) sta

      character*(3) idummy
!----------------------------------------------------------------------
      idummy = sta
      if (hsta .ne. hstatj(i)) then
        hstatj(i) = hsta
        lnsta = .true.
      else
        lnsta = .false.
      endif

      return
      end

!======================================================================
	Subroutine  regre_z(ind,pmin,tmin,pmax,tmax,z_coef)
c
c***********************************************************************
c	calcola i coefficienti per le approssimazioni
c	biquadratiche di z
c    *** ATTENZIONE !! DEVE ESSERE CHIAMATA PRIMA DI SIMULARE LE
C        STAZIONI !!!!!!
c----------------------------------------------------------------------
        implicit none
c
        include '../inc/rk_param.inc'
c
        INTEGER*4   max_p,max_t,max_value
	parameter (max_p = 10 , max_t = 10,  max_value=max_p*max_t)
c
	INTEGER*4 ind
	real*4  pmin,tmin,pmax,tmax
	INTEGER*4 ip(20) , i,j,k
        INTEGER*4 ier
        real*4  dp,dt
	real*4  z_coef(*),x(3,max_value),z(max_value)
        real*4  sum
c-----------------------------------------------------------------------
	do i=1,6
       	  IP(i)=1
	end do
        do i=7,20
          ip(i)=0
        end do
!	pmax = 4.*pmin
!	tmax = 1.2*tmin
c---->                           determinazione dei valori sulla griglia
	dp = (pmax-pmin)/(max_p-1)
	dt = (tmax-tmin)/(max_t-1)
	do i=1,max_p
	    do j=1,max_t
	       k = (i-1)*max_t+j
	       x(1,k) = pmin+(i-1)*dp
	       x(2,k) = tmin+(j-1)*dt
               call zpt1(ind,x(1,k),x(2,k),z(k))
c per problemi numerici adimensionalizzo p,t
	       x(1,k) = x(1,k)/(PRIF+P0)
	       x(2,k) = x(2,k)/(trif+t0)
	    end do
	end do
c---->                            regressioni coefficienti biquadratiche
        ier = 0
	call regres(2,3,max_value,ip,x,z,z_coef,sum,ier)
        if (ier.gt.0) then
! gz ???
        endif
	return
	end

!======================================================================
	real*4 Function prop(P,T,coef)
c
c***********************************************************************
c	calcolo della proprieta' a P e T fissate
c	con formula biquadratica
cgpe  NOTA: P T sono variabili dimensionali e al suo interno chiama la
cgpe        PROP_A che usa variabili adimensionali
c----------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'

	real*4 coef(*),PINT,TINT
      real*4 P,T
	real*4   prop_a
	external prop_a
c----------------------------------------------------------------------
c per problemi numerici adimensionalizzo p,t
      PINT = P/(PRIF+P0)
      TINT = T/(trif+t0)
cgpe
cgpe	prop = coef(1)+(coef(2)+coef(4)*TINT+coef(5)*PINT)*PINT+
cgpe *			(coef(3)+coef(6)*TINT)*TINT
      prop = prop_a(PINT,TINT,coef)
cgpe-end
      return
      end
!======================================================================
	Subroutine app_mess(ist,ier,mess)
c
c	append a message error ier for the station ist
c
c	ist   station index
c	ier   error index
c-----------------------------------------------------------------------
        implicit none

	include '../inc/param.inc'
	include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
	include '../inc/MESSAG.INC'
	include '../inc/simpstat.INC'
c
	INTEGER*4   ier,ist
	INTEGER*4   ERR_COD,INUM
	character*(*) mess
c
c---------------------------------------------------------------------------------------
c_e mess_max(=30) è il numero massimo di messaggi di warning ammissibile nella simulazione
c_e di stazione (vd. stazione.inc)
	if(mess_num(ist).lt.mess_max) then
c_e codice d'errore corrispondente all'indice dato in input
          ERR_COD = ier + 100*num_iter
          INUM    = MESS_NUM(IST)
          IF (INUM.GE.1) THEN
            IF ((MESS_TYPE(INUM,IST).NE.ERR_COD) .or.
     *           mess_stri(inum,ist).ne.mess) THEN
              mess_num(ist) = mess_num(ist)+1
              mess_type(mess_num(ist),ist) = ERR_COD
              mess_stri(mess_num(ist),ist) = mess
            ENDIF
          ELSE
            mess_num(ist) = mess_num(ist)+1
            mess_type(mess_num(ist),ist) = ERR_COD
            mess_stri(mess_num(ist),ist) = mess
          ENDIF
	else
	  mess_num(ist) = mess_max
	  mess_type(mess_num(ist),ist) = 99
          mess_stri(mess_num(ist),ist) = ' '
	end if
  10	return
	end

csave      Subroutine save_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int, 
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
      Subroutine save_cond_ini (type_num,jtfir,
     *        pres_int,temp_int,delprint,delt_int,type_quant_old)
C
C     Salvataggio delle condizioni iniziali
C
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/jc.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/MF_UNITS.INC'
c
c      INTEGER*4 istaz            ! I indice della stazione nella struttura dati
c      INTEGER*4 vert_type
c      INTEGER*4 vert_pointer,type_num,unit_num,jufir,jtfir,
c     *          stat_ord,tipo_criterio
      INTEGER*4 type_num,jtfir

c      real*4    flow2,           ! I flusso di uscita
c     *          pres1,pres2      ! I pressioni di ingresso e uscita
      real*4    pres_int,temp_int,delprint,delt_int ! valori intermedi

c      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
c     *          VALSJC_OLD,PRES1_OLD, PRES2_OLD, FLOW_OLD
      integer*4 type_quant_old(*)
c      character*3 status_old(*)
c      real*4    unit_eff_corr_old(*),unit_power_corr_old(*)

      INTEGER*4 i,j
c-----------------------------------------------------------------------
csave      htcitj_old = htcitj(istaz)
csave      porttj_old = porttj(istaz)
csave      qsettj_old = qsettj(istaz)
csave      pmsetj_old = pmsetj(istaz)
csave      pvsetj_old = pvsetj(istaz)
csave      valsjc_old = valsjc(istaz)

csave      do i = 1, unit_num
csave        j = jufir+i-1
csave        status_old(i) = status(j)
csave      end do

csave      pres1_old = pres1
csave      pres2_old = pres2
csave      flow_old = flow2
c
csave      if (tipo_criterio .eq. crit_giri) then
cgaia        pres_int = 0.
cgaia        temp_int = 0.
cgaia        delprint = 0.
cgaia        delt_int = 0.
c
        do i = 1, type_num
          j = jtfir+i-1
          type_quant_old(i) = type_quant(j)
        end do
c
!        if (vert_type .eq. tipo_comp .and. stat_ord .eq. f_serie) then
cgaia        if (stat_ord .eq. f_serie) then
!          do i = 1, type_num
cgaia          do i = 1,maxunits
cgaia            j = jtfir+i-1
cgaia            type_quant(j) = 0
cgaia          end do
csave          do i = 1, unit_num
csave            j = jufir+i-1
csave            unit_eff_corr_old(i)   = unit_eff_corr(j)
csave            unit_power_corr_old(i) = unit_power_corr(j)
csave          end do
cgaia        endif
csave      endif
c
      return
      end ! save_cond_ini

csave      Subroutine restore_cond_ini (istaz,vert_pointer,
csave     *        tipo_criterio,type_num,unit_num,jufir,jtfir,stat_ord,
csave     *        flow2,pres1,pres2,pres_int,temp_int,delprint,delt_int, 
csave     *        ier,
csave     *        HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
csave     *        VALSJC_OLD,PRES1_OLD,PRES2_OLD,FLOW_OLD,type_quant_old,
csave     *        status_old,unit_eff_corr_old,unit_power_corr_old)
      Subroutine restore_cond_ini (type_num,jtfir,type_quant_old)
C
C     Ripristino delle condizioni iniziali
C
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/tj.inc'
      include '../inc/jc.inc'
      include '../inc/TYPES.INC'
      include '../inc/UNITS.INC'
      include '../inc/MF_UNITS.INC'
c
c      INTEGER*4 istaz            ! I indice della stazione nella struttura dati
c      INTEGER*4 vert_pointer,vert_type,type_num,unit_num,jufir,jtfir,
c     *          stat_ord,tipo_criterio
      INTEGER*4 type_num,jtfir

c      real*4    flow2,           ! I flusso di uscita
c     *          pres1,pres2      ! I pressioni di ingresso e uscita
c      real*4    pres_int,temp_int,delprint,delt_int ! valori intermedi
c      INTEGER*4 ier              ! I indice dell'errore

c      real*4    HTCITJ_OLD,PORTTJ_OLD,QSETTJ_OLD,PMSETJ_OLD,PVSETJ_OLD,
c     *          VALSJC_OLD,PRES1_OLD, PRES2_OLD, FLOW_OLD
      integer*4 type_quant_old(*)
c      character*3 status_old(*)
c      real*4    unit_eff_corr_old(*),unit_power_corr_old(*)

      INTEGER*4 i,j
c-----------------------------------------------------------------------
csave      htcitj(istaz) = htcitj_old
csave      porttj(istaz) = porttj_old
csave      qsettj(istaz) = qsettj_old
csave      pmsetj(istaz) = pmsetj_old
csave      pvsetj(istaz) = pvsetj_old
csave      valsjc(istaz) = valsjc_old
c
csave      do i = 1, unit_num
csave        j = jufir+i-1
csave        status(j) = status_old(i)
csave      end do
c
csave      pres1 = pres1_old
csave      pres2 = pres2_old
csave      flow2 = flow_old
c
csave      if (tipo_criterio .eq. crit_giri) then
        do i = 1, type_num
          j = jtfir+i-1
          type_quant(j) = type_quant_old(i)
        end do
c
csave        if (stat_ord .eq. f_serie) then
csave          do i = 1, unit_num
csave            j = jufir+i-1
csave            unit_eff_corr(j)   = unit_eff_corr_old(i)
csave            unit_power_corr(j) = unit_power_corr_old(i)
csave          end do
csave        endif
csave      endif
c
csave      if (ier .eq. 1) then
csave        do i = 1, unit_num
csave          j = jufir+i-1
csave          unit_rev(j) = 0.
csave        enddo
csave      endif
c
      return
      end ! restore_cond_ini

      Subroutine ini_ottimo ()
C
C     Inizializzazione delle variabili necessaria prima di lanciare un
C     ottimo
C
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/ti.inc'
      include '../inc/STATIONS.INC'
      include '../inc/argo_reale_ris.INC'
      include '../inc/cap_bon_prv.inc'
c
      INTEGER*4 istaz,ivert,i,iu
      logical*2 ce_serie
c-----------------------------------------------------------------------
      do istaz = otipti(e_ce), etipti(e_ce)
        ivert = pgrato(istaz)
        iu = first_unit(ivert)

        tmand_stat(IVERT) = t0
        comp_ratio(IVERT) = 0.
        tot_cons(IVERT)   = 0.
        head(IVERT)       = 0.
        flowm(IVERT)      = 0.
        flow_ric(IVERT)   = 0.
        do i = 1,unit_num(ivert)
          power(iu+i-1) = 0.
          POWER_PERC(iu+i-1) = 0.
          POWER_PERC_VAR(iu+i-1) = 0.
        enddo

        if (ce_serie(istaz)) then
          ivert = pgrato_app(istaz)
          iu = first_unit(ivert)

          tmand_stat(IVERT) = t0
          comp_ratio(IVERT) = 0.
          tot_cons(IVERT)   = 0.
          head(IVERT)       = 0.
          flowm(IVERT)      = 0.
          flow_ric(IVERT)   = 0.
          do i = 1,unit_num(ivert)
            power(iu+i-1) = 0.
            POWER_PERC(iu+i-1) = 0.
            POWER_PERC_VAR(iu+i-1) = 0.
          enddo

        endif
      enddo
c
      return
      end ! ini_ottimo





      integer*4 Function ind_vio_vinc(itip,unit_power,unit_rev)

c***********************************************************************
c   ind_vio_vinc = min_pw   11 = min_pow da vincolo operativo
c                  max_pw   12 = max_pow da    "        "
c                  max_re   13 = max_rev da    "        "
c----------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/simpstat.INC'
cmar_pow_stat
	 include '../inc/power.INC'
cmar_pow_stat


      INTEGER*4 itip                 ! I)
      real*4    unit_power,unit_rev  ! I)
	logical c
c----------------------------------------------------------------------
cmar_pow_stat
      
c      c=.false.
c       c=.true.
c          if(.not.c)then
       ind_vio_vinc = 0
       if(.not.  flag_pow_cle )then
      

c   

     
      if (unit_power .ge. type_vinc_maxpow(itip)) then
        ind_vio_vinc = ind_vio_vinc + max_pw
      endif
      if (unit_power .le. type_vinc_minpow(itip)) then
        ind_vio_vinc = ind_vio_vinc + min_pw
      endif
  

cmar_pow_stat
      else
cmar_pow_stat
   
      if (power_tot_staz .ge. max_p) then
        ind_vio_vinc = ind_vio_vinc + max_pw
      endif
      if (power_tot_staz .le. min_p) then
        ind_vio_vinc = ind_vio_vinc + min_pw
      endif
cmar_08_05   
cmar_pow_stat_29_04_2014

      IF (.not. FLAG_LAST ) THEN


        IF ( ind_vio_vinc .NE.0 ) THEN
	  A_P=ind_vio_vinc
        ind_vio_vinc=0
	  ENDIF

      ENDIF

   
cmar_pow_stat_29_04_2014
cmar_08_05


cmar_pow_stat
	endif
cmar_pow_stat

      if (unit_rev .gt. revrif * type_vinc_maxrev(itip)) then
      ind_vio_vinc = ind_vio_vinc + max_re
      endif




cmar_pow_stat

      return
	end

