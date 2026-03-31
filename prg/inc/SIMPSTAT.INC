c===>>>> per i compressori
c	ntp        number of type on the station
c	itipo      type to consider 
c	itipo_coup tipo da accoppiare type to consider 
c	type_actr  number of unit active for each type (not simulated)
c	type_ratio number for the flow rate partition for each type
c	lvinc       operating point determined
c		(=1 on the maximum curve   =2 on recirculation curve
c		 =0 without contraints)
c	headm       adiabatic head
c	flow_tot   total machine flow to work
c===>>>> per il max power
c	z          compressibility factor
c	tair       air temperature
c	aux2       parameter for adiabatic head computation
c	aux3       parameter to transform nomal flow to machine flow
c	effmi      minimal efficiency for the compressor
c	un_eff_corr efficiency correction
c	un_pow_corr power correction
c===>>>>>   compressor and turbine coefficients for each type
c	clim       coefficiente curva anti-choke
c	a_coef     termini noti curva anti-surge
c	b_coef     coefficiente angolari curva anti-surge
c       lim_h      valori limite per head curva anti-surge
c       nlim       numero di valori limite
c===>>>>>>         normal zone
c	cen       efficiency curve
c	ceC       efficiency curve IN CHOKING ZONE
c	chn       adiabatich head determination
c	chc       adiabatich head determination (choking)
c===>>>>>     turbine
c	cpwm      maximum power
c	cpwt      correction of maximum power
c	chr       heat rate
c	chrt      correction of heat rate
c 28/8/97
c       iopt      optimization index (iopt=.T. optimization)
c       iopt_mess optimization index for messages (iopt_mess=.T.)
c---------------------------------------------------------------------
	INTEGER*4 ntp,itipo,itipo_coup,type_actr,lvinc
        integer*4 numin

        real*4  type_ratio,headm,flow_tot,z,tair,aux2,aux3,
     *		effmi,un_eff_corr,un_pow_corr,clim,
     *          a_coef,b_coef,lim_h,lim_n,lim_q,cen,cec,chn,chc,
     *          cpwm,cpwt,chr,chrt,un_rev,eff_corr,power_corr,
     *          rapp_giri,
cgpe
     *          hrate_corr
     *         ,delta
cgpe-end
        INTEGER*4 nlim
        INTEGER*4 num_iter
        logical*2 una_turb
        logical*2 iopt
        logical*2 iopt_mess
        character*3 stat
c
c Integrazione del common types.cmn per i dati della prima fase
c
        real*4       kcrit_min
        real*4       kcrit_max
c
        real*4       type_max_rev
        real*4       type_min_rev
        real*4       type2_max_rev
        real*4       type2_min_rev
c_ecol
        real*4       type_vinc_maxpow,type_vinc_minpow
c-mf03
        real*4       type_vinc_maxrev
c-mf03-end
        logical*2 type_flag_maxpow  !flag che indica quale valore della massima
c                              !potenza è usato nella simulazione:
c                              !FALSE=massima potenza erogabile calcolata dalla'pwdan'
c                                  !TRUE = valore vincolato
     *         ,type_flag_maxrev   !flag che indica quale valore di max_rev
c                                  !è usato nella simulazione:
c                                  !FALSE = valore effettivo 
c                                  !TRUE = valore vincolato
     *         ,type_flag_minpow  !flag che indica se la potenza richiesta alla turbina
c                                 !è inferiore della minima erogabile assegnata da utente:
c                                 !FALSE= nessuna violazione
c                                 !TRUE = violazione
c_ecol
c---------------->variabili per il trattamento di filtri e air_cooler
c_ecol

      logical*2    f_ce       !flag di presenza filtro di centrale
     *            ,f_tg       !flag di presenza filtro di tg
	                          ! Possibili valori:
	                          ! true  == filtro presente
	                          ! false == filtro non presente

      integer*4    f_ce_stat    !flag stato filtro di centrale
     *            ,f_tg_stat    !flag stato filtro di tg
	                            ! Possibili valori:
	                            ! 0 == filtro assente
 	                            ! 1 == filtro attivo
	                            ! 2 == filtro non attivo


      real*4       f_ce_k,f_tg_k
	                     ! coefficiente caratteristico del filtro
	                     ! (ricavato dalla portata di progetto e
						 !  della caduta di pressione associata) 
c---------->Trattamento air cooler
c
c-ele      logical*2    ac_ce       !flag di presenza air cooler
c	                             ! true  == a livello di centrale
c	                             ! false == a livello di TG

      integer*4    ac_ce_stat    !indica lo stato dell'ac di centrale
     *            ,ac1_stat      !indica lo stato dell'ac di fase1
                                 ! Valori possibili:
                                 ! 0 = assente
                                 ! 1 = attivo
                                 ! 2 = non attivo
								 ! 3 = by pass 
c------------------->Trattamento Air Cooler di centrale

      real*4 ac_ce_k              !coefficiente caratteristico dell'air cooler
     *      ,ac_q_prog            !calore scambiato nell'unità di tempo
     *      ,ac_taria_prog        !temperatura di riferimento dell'aria

c------------------->Trattamento Air Cooler di fase 1

      real*4 ac1_k              !coefficiente caratteristico dell'air cooler
     *      ,ac1_q_prog         !calore scambiato nell'unità di tempo
     *      ,ac1_taria_prog     !temperatura di riferimento dell'aria

c_ecol
      real*4    type_ac_k_int
c_ecol

c_ecol
c_ecol  Costante da usare per il calcolo della caduta di pressione dovuta
c_ecol  all'air-cooler di fase intermedia in una centrale composta, in serie,
c_ecol  con un unico tipo di unità(modulo E)
c_ecol
        real*4       k_int
c_ecol
c
c CMAR_AC 

       real*4    ac_ce_k_M  
     *	        ,ac1_k_M
     *          ,delpr2_M

 


        integer*4    ac_ce_stat_M,ac1_stat_M
		
        INTEGER*4 type_actr_M


	

	   LOGICAL*2 FLAG_AC


        common/SIMPSTAT/ntp,itipo,itipo_coup,type_actr(maxunits),
     *                  type_ratio(maxunits),lvinc(maxunits),
     *                  numin,
     *                  f_ce_stat,f_tg_stat(maxunits),  
     *                  ac_ce_stat,ac1_stat(maxunits),
     *                  headm,flow_tot,z,tair,aux2,aux3,
     *                  delta,
     *                  effmi,un_eff_corr(2),un_pow_corr(2),
     *                  clim(4,maxunits),a_coef(max_lim,maxunits),
     *                  b_coef(max_lim,maxunits),
     *                  lim_h(max_lim,maxunits),
     *                  lim_n(max_lim,maxunits),
     *                  lim_q(max_lim,maxunits),
     *                  nlim(maxunits),chn(6,maxunits),chc(6,maxunits),
     *                  cen(5,maxunits),cec(6,maxunits),
     *                  cpwm(3,maxunits),cpwt(4,maxunits),
     *                  chr(6,maxunits),chrt(4,maxunits),
     *                  num_iter,un_rev,eff_corr(maxunits),
     *                  power_corr(maxunits),rapp_giri,
     *                  hrate_corr(maxunits),
     *                  kcrit_min(maxunits), kcrit_max(maxunits),
     *                  type_max_rev(maxunits),
     *                  type_min_rev(maxunits),
     *                  type2_max_rev(maxunits),
     *                  type2_min_rev(maxunits),
c_ecol
     *                  type_vinc_maxpow(maxunits),
     *                  type_vinc_minpow(maxunits),
c-mf03
     *                  type_vinc_maxrev(maxunits),
c-mf03-end
     *                  f_ce_k,f_tg_k (maxunits),
     *                  ac_ce_k,ac1_k(maxunits),
     *                  ac_q_prog,
     *                  ac_taria_prog,
     *                  ac1_q_prog(maxunits),
     *                  ac1_taria_prog(maxunits),
     *                  type_ac_k_int(maxunits),
c_ecol
     *                  k_int,
     *                  una_turb,iopt,iopt_mess,
c_ecol
     *                  type_flag_maxpow(maxunits),
     *                  type_flag_maxrev(maxunits),
     *                  type_flag_minpow(maxunits),
c     *                  f_ce,f_tg(maxunits),    

c_ecol
     *                  stat(maxunits),
c_ecol
CMAR_AC
     *                  ac_ce_k_M
     *                 ,ac1_k_M(maxunits)
     *                 ,FLAG_AC
     *                 ,delpr2_M
c	 *                 ,stat_vars_M,
     *                 ,ac_ce_stat_M
     *                 ,ac1_stat_M(maxunits)

          
        common/SIMPSTAT_M/type_actr_M(maxunits)
