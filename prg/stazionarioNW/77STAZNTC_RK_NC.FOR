
      subroutine simula_nc (l_cor_ce,l_cons_ce,
cgpe     *                      l_rec_ce,l_conv_ce,fl_fatal)
     *                      l_rec_ce,l_conv_ce)
*&*
C  Simulazione della centrale con il nuovo modello di centrale di compressione
*&*   
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/dimension.inc'
      include '../inc/STATIONS.INC'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/jc.inc'
      include '../inc/ty.inc'
      include '../inc/staznw_par.inc'
      include '../inc/nw.inc'
      include '../inc/MESSAG.INC'
      include '../inc/FLAG.INC'
      include '../inc/cap_bon_prv.inc'
!----------------------------------------------------------------------
      logical*2 l_cor_ce           ! I) = .F. verifica della stazione
                                   !    = .T. viene corretto il punto di lavoro
      logical*2 l_cons_ce          ! I) = .F. verifica della consumo (cons=consty)
                                   !    = .T. viene corretto il consumo (cons=min(tot_cons())
      logical*2 l_rec_ce           ! O) risultato della simulazione globale
                                   !    = .T. richiesta di recupero (almeno una cen ha err recuperabile)
      logical*2 l_conv_ce          ! O) risultato convergenza consumo
                                   !    = .T. convergenza raggiunta
cgpe      integer*4 fl_fatal(max_vert) ! O) risultato della simulazione per ogni centrale
cgpe                                   !    = 0 => centrale ok 
cgpe                                   !    = k => centrale ko, con errore fatale k
c
c      integer*4 j
      real*4    sum_cons,sum_cons_s
      integer*4 kflag
c      real*4    sum_cons,tol_cons,APP_CONS
c      integer*4 istaz,jstaz,flag_fatal,pg,pg_ass,pg_app
c      real*4    pasp_save(max_serv),pmand_save(max_serv),
c     *          flow_save(max_serv)
c     *         ,consumo
c      logical*2 flag_serie ! = .T. se la centrale istaz è in serie
c      logical*2 fl_ass     ! = .T. alla centrale istaz è associata una centrale jstaz
c      logical*2 l_simula   ! = .T. la centrale ha cin ok ed è accesa (=> call s_cnt_new)
crsaconce      real*4    tol_cons
!--------------------------------------------------------------------------
      sum_cons = 0.
crsaconce      tol_cons = 0.5
      l_rec_ce = .false.
      l_conv_ce = .false.
c inizializzazione dei flag per simulare una centrale in rete
      fl_cor  = l_cor_ce
      fl_cons = l_cons_ce

c Simulazione di tutte le centrali complete presenti nella rete
c-din
c-din      call sim_cent (sum_cons,l_rec_ce)
      kflag = 0
      call sim_cent (sum_cons,l_rec_ce,kflag)
c-din-end

c Trattamento di tutte le centrali semplificate presenti nella rete
      call sim_cent_cs (sum_cons_s)

c Calcolo del massimo sum_cons determinato con le centrali complete e le centrali
c semplificate
      sum_cons = max(sum_cons,sum_cons_s)

c Test di convergenza sul consumo delle centrali (il test viene fatto sul massimo
c consumo trovato considerando centrali complete e centrali semplificate)
crsaconce      if (sum_cons .le. tol_cons) l_conv_ce = .true.
      if (sum_cons .le. toll_conce) l_conv_ce = .true.

      return
      end

      Subroutine sim_cent_cs (sum_cons_s)
*&*
!- Per ogni centrale semplificata da trattare viene calcolato il consumo
!- Viene restituita la massima differenza fra in consumo stimato calcolato dal sistema
!  ed il consumo (sempre la stima) con le condizioni trovate dal sistema.
!  Viene inoltre aggiornata la temperatura di uscita della centrale touttz
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/tz.inc'
      include '../inc/jc.inc'
      include '../inc/nw.inc'

      real*4     sum_cons_s  ! O) per la rete, massima differenza fra il consumo
                             !    calcolato e quello imposto

      integer*4  istaz
      real*4     app_cons,consumo,dq,dpa2,dpv2

!--------------------------------------------------------------------------
      sum_cons_s = 0.
      do istaz = otipti(e_cs), etipti(e_cs)

        call qconce(porttj(istaz),
     -              prestj(opumto(istaz)),prestj(opuvto(istaz)),
     -              istaz,consumo,dq,dpa2,dpv2)

cgpe-prv
c per testare le subroutine
cgpe-prv        call calc_out_cs (istaz,head,flowm)
cgpe-prv-end

c  Nella variabile valsjc viene memorizzato il consumo della centrale
c  semplificata da rispettare nell'iterazione successiva, mentre in consty
c  e' stato memorizzato il consumo della centrale calcolato dal sistema
c  nell'iterazione di cui si stanno calcolando i consumi delle centrali
        valsjc(istaz-oics) = consumo
cgpe-tout        temptj(istaz) = temptj(opuvto(istaz))
        touttz(istaz) = temptj(opuvto(istaz))

c memorizzazione della differenza valsjc e consty per la convergenza del consumo
c Per le centrali con flag ok, viene fatta la differenza effettiva
c mentre per quelle ko, i due valori dovrebbe coincidere
        app_cons   = abs(valsjc(istaz-oics)-consty(istaz))
        sum_cons_s = max(sum_cons_s,app_cons)

      enddo

      return
      end ! sim_cent_cs
