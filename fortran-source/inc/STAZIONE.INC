c***********************************************************************
**** STAZIONE.INC **** DIMENSIONI E PARAMETRI PER LE CENTRALI **********
c***********************************************************************


c***********************************************************************
C---->  VARIABILI FONDAMENTALI E DERIVATE
c***********************************************************************
        INTEGER*4  max_type, ! numero massimo di tipi in una rete
     *             mess_max  ! numero massimo di messaggi di warning nella
                             ! simulazione di stazione 
C---->  massime dimensioni dei vettori(vertici,segmenti,ecc..)
	PARAMETER (mess_max=30)

C----> dimensioni derivate
	PARAMETER  (max_type=max_unit)

        integer*4 max_lim
	PARAMETER (max_lim=20)


C---->  dimensioni massime coefficienti per i compressori
        INTEGER*4  max_chn,      ! coefficienti zona normale
     *             max_chc,      ! coefficienti zona di choking
     *             max_cen,      ! coefficienti curva efficienza zona normale
     *             max_cec,      ! coefficienti curva efficienza zona choking
     *             max_a_coef,   ! termini noti spezzata di surge
     *             max_b_coef,   ! coefficienti angolari spezzata di surge
     *             max_c_coef,   ! coefficienti costanti curve limite
     *             max_lim_n,    ! valori estremi numero giri
     *             max_lim_q,    ! valori estremi portata
     *             max_lim_h     ! valori estremi altezza adiabatica
	PARAMETER (max_chn=6,  max_chc=6,
     *             max_cen=5,  max_cec=6,
     *    max_a_coef=max_lim,  max_b_coef=max_lim,  max_c_coef=4,
     *    max_lim_n=max_lim,   max_lim_q=max_lim,
     *    max_lim_h=max_lim)


C---->  dimensioni massime coefficienti per le turbine
        INTEGER*4  MAX_CPWM,      ! coefficienti massima potenza
     *             MAX_CPWT,      ! coefficienti correzione massima potenza
     *             MAX_CHR,       ! coefficienti heat rate
     *             MAX_CHRT       ! coefficienti correzione heat rate
	PARAMETER (MAX_CPWM=3,  MAX_CPWT=4,  MAX_CHR=6,
     *             MAX_CHRT=4)


c***********************************************************************
      real*4 epsma,    ! precisione massima nella ricerca di zero
     *       epsmi,    ! precisone minima nella ricerca di zero
     *       pmach     ! precisone di macchina
c
        PARAMETER ( epsma = 1.e-5 ,  epsmi = 1.e-4,
     *              pmach = 1.e-7  )

c***********************************************************************
C---->  
c ind        indice della stazione nella struttura dati completa
c z_coeff    coefficienti per zeta
c pmol       peso molecolare medio della miscela
c rocn       densita' in condizioni normali (Kg/m3)
       INTEGER*4 ind,ver_poi
       real*4    z_coef(6,max_vert),eff_coef
       real*4    pmol
       real*4    rocn
       real*4    zcn
!#ifdef NOINT_0074    !sm
!       common /station/ ind,ver_poi,z_coef,eff_coef,pmol,rocn,zcn
!#else
       common /p_station/ ind,ver_poi,z_coef,eff_coef,pmol,rocn,zcn
!#endif

cgpe
cport      integer*4 nit/0/, nitmax/1/
cport     *         ,tipo_corr/1/
      integer*4 nit, nitmax
     *         ,tipo_corr

      common /cmf/ nit, nitmax, tipo_corr
cgpe-end

c***********************************************************************
C---->  parametri necessari alla multifunzione
! NOTA: e' il vecchio common ssdd$inc:mf.inc.
!       Nei moduli in cui ps.inc era incluso deve essere commentato
c***********************************************************************
c parametri necessari alla multifunzione
c Tipi di criterio:
      integer*4 crit_port   ! uguaglianza delle portate
      integer*4 crit_giri   ! uguaglianza dei giri
      integer*4 crit_equi   ! equidistanza
      integer*4 crit_man    ! gestione manuale delle portate

!      parameter(crit_port=1)
!      parameter(crit_giri=2)
!      parameter(crit_equi=3)
      parameter(crit_equi=0)
      parameter(crit_giri=1)
      parameter(crit_port=2)
      parameter(crit_man=3)

c tipo di surge:
      integer*4 parabola   ! la surge e' una parabola della forma H/Q**2
      integer*4 spezzata   ! la surge e' una spezzata
      parameter(spezzata=1)
      parameter(parabola=2)

c vert_type
      integer*4 tipo_semp, tipo_comp, tipo_mist
c i numeri sono assegnati in modo da permettere una conversione soft fra il
c vecchio e il nuovo archivio.
      parameter (tipo_semp = 1)
      parameter (tipo_comp = 2)
      parameter (tipo_mist = 3)
c_ecol
c Stato di funzionamento di un Filtro
c
      integer*4 fil_off,fil_on,fil_assente
c
      parameter (fil_assente = 0)
      parameter (fil_on = 1)
      parameter (fil_off = 2)

c Stato di funzionamento di un Air Cooler
c
      integer*4 ac_off,ac_on,ac_bypass,ac_assente
c
      parameter (ac_assente = 0)
      parameter (ac_on = 1)
      parameter (ac_off = 2)
      parameter (ac_bypass =3)
c_ecol
c
c tipo di vincolo nella centrale
c       vinc_min    O   tipo di vincolo di minima per ciascun tipo:
c                       sur_1f    1 = anti-surge (prima fase)
c                       mir_1f    2 = min_rev    (prima fase)
c                       sur_2f    3 = anti-surge (seconda_fase)
c                       mir_2f    4 = min_rev    (seconda fase)
c                       min_pw    11 = min_pow da vincolo operativo
c                       min_ah    14 = min_head

c       vinc_max    O   tipo di vincolo di massima per ciascun tipo:
c                       mar_1f    5 = max_rev    (prima fase)
c                       cho_1f    6 = choking    (prima fase) 
c                       pow_1f    7 = max_power  (prima fase)
c                       mar_2f    8 = max_rev    (seconda fase)
c                       cho_2f    9 = choking    (seconda fase)
c                       pow_2f   10 = max_power  (seconda fase)
c                       max_pw   12 = max_pow da vincolo operativo
c                       max_re   13 = max_rev da vincolo operativo
c                       max_ah   15 = max_head
      integer*4 sur_1f, mir_1f, sur_2f, mir_2f,
     *          mar_1f, cho_1f, pow_1f,
     *          mar_2f, cho_2f, pow_2f,
c-mf02
     *          min_pw,max_pw,max_re,
c-mf02-end
c-bp08
     *          min_ah,max_ah
c-bp08-end
      parameter(sur_1f = 1)
      parameter(mir_1f = 2)
      parameter(sur_2f = 3)
      parameter(mir_2f = 4)
      parameter(mar_1f = 5)
      parameter(cho_1f = 6)
      parameter(pow_1f = 7)
      parameter(mar_2f = 8)
      parameter(cho_2f = 9)
      parameter(pow_2f = 10)
c-mf02
      parameter(min_pw = 11)
      parameter(max_pw = 12)
      parameter(max_re = 13)
c-mf02-end
c-bp08
      parameter(min_ah = 14)
      parameter(max_ah = 15)
c-bp08-end
c----------------------------------
c tipo funzionamento
c f_serie   serie
c f_paral   parallelo
      integer*4 f_serie, f_paral
      parameter(f_paral=1)
      parameter(f_serie=2)
c----------------------------------
c precisioni per zero_fun
      real*4 eps_crit
      real*4 eps_flow
      real*4 eps_giri
      real*4 eps_pow
      parameter(eps_crit = 1.e-2)
      parameter(eps_flow = 1.e-2)
      parameter(eps_giri = 1.e-2)
      parameter(eps_pow  = 1.e-1)


cgpe*----------------------------------------------------------------------
cgpec   Parametri necessari alla MF
cgpe*----------------------------------------------------------------------
cgpec Tipi di collettore esterno
cgpe      character*1 col_una   ! collettore unario
cgpe      character*1 col_nsm   ! collettore di nodo
cgpe      character*1 col_ser   ! collettore intermedio di assetto serie
cgpe      parameter(col_una='U')
cgpe      parameter(col_nsm='N')
cgpe      parameter(col_ser='I')

cgpec Tipi di servizi svolti dalla centrale multifunzione
cgpe      character*3 funz_sto   ! stoccaggio
cgpe      character*3 funz_spi   ! spinta
cgpe      character*3 funz_est   ! estrazione
cgpe      parameter(funz_sto='sto')
cgpe      parameter(funz_spi='spi')
cgpe      parameter(funz_est='est')
*----------------------------------------------------------------------
c   Parametri generici
*----------------------------------------------------------------------
c Tipi stato di una unita':
      character*3 funz_on    ! ON
      character*3 funz_off   ! OFF
      parameter(funz_on='ON')
      parameter(funz_off='OFF')

c Stato di una unita':
      integer*4 OFF          ! spenta
     *         ,ON           ! accesa
      parameter( OFF = 0
     *          ,ON  = 1)

c Valori di flag:
      integer*4 OK           ! 
     *         ,KO           ! 
      parameter( OK = 1
     *          ,KO = -1)


      real*4 dp_off   ! delta in pressione per il check di centrale spenta

cport      real*4 dp_off /1.0/  ! delta in pressione per il check di centrale spenta

      common/STATIONS_AUX/ dp_off

      INTEGER*4 fase_1,fase_2
      parameter (fase_1 = 1
     *          ,fase_2 = 2)

*----------------------------------------------------------------------
c   Parametri per le unita' di misura interne da utilizzare per la
c   messaggistica
*----------------------------------------------------------------------
      character*4  UDM_INT_H   ! altezza adiabatica
      parameter   (UDM_INT_H = '[km]')

      character*9  UDM_INT_P   ! pressione
      parameter   (UDM_INT_P = '[bar ass]')

      character*3  UDM_INT_T   ! temperatura
      parameter   (UDM_INT_T = '[K]')

      character*12 UDM_INT_Q   ! portata volumetrica in Condizioni Normali
      parameter   (UDM_INT_Q = '[1000*Nm3/h]')

      character*11 UDM_INT_QM   ! portata volumetrica in Condizioni Macchina
      parameter   (UDM_INT_QM = '[1000*m3/h]')

      character*4  UDM_INT_PW   ! potenza
      parameter   (UDM_INT_PW = '[kW]')

*----------------------------------------------------------------------
c   Parametro per la lunghezza dei messaggi di errore associati alla
c   centrale
*----------------------------------------------------------------------
      INTEGER*4  max_len        ! lunghezza massima dei messaggi
c      parameter( max_len = 80)
      parameter (max_len = 130)

      INTEGER*4  max_len_str    ! lunghezza massima di una stringa da scrivere con la
	                            ! function udm_out_str
      parameter (max_len_str = 25)

*** STAZIONE.INC **** DIMENSIONI E PARAMETRI ******************************

	  
