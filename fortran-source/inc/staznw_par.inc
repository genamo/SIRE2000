c----------------------------------------------------------------------------------------------
c--------------------------------> STAZNW_PAR.inc<---------------------------------------------
c----------------------------------------------------------------------------------------------
c  Parametri di tolleranza e convergenza usati nella simulazione stazionaria della rete
c (tali parametri sono inizializzati nella subroutine ini_par)
c----------------------------------------------------------------------------------------------

      integer*4 max_count    !max numero di iterazioni per la risoluzione del 
	                       !sistema non lineare in pressioni e portate col metodo di Newton
	                       !(era parametrizzato nella subroutine STAZIO_NC_RK)
     *         ,m_it_nconv   !max numero di iterazioni divergenti
	                       !(era parametrizzato nella subroutine S_STAZIO_NC_RK)

      real*4 rq0             !valore trigger per le portate
	                       !(era parametrizzato nella subroutine INIZPQ_RK)
      real*4 dp_on_vl        !deltaP minimo esercizio valvole
     *      ,dp_on_ce        !deltaP minimo esercizio centrali
	                       !(erano parametrizzati nella subroutine VERSTEL_NW)
    
      common /Istaznw_par/ max_count,m_it_nconv
      common /Rstaznw_par/ rq0,dp_on_vl,dp_on_ce
c----------------------------------------------------------------------------------------------
c   Parametri provenienti dall'include 'WD.INC'
c----------------------------------------------------------------------------------------------
      real*4 TOLFwd          !tolleranza sui residui delle equazioni
     *      ,TOLXwd          !tolleranza sulla differenza tra le iterate
     *      ,TOLCwd          !tolleranza sui cicli in T e C
     *      ,toll_pcls       !tolleranza sui cicli in pcls
     *      ,toll_conce      !tolleranza sui cicli in conce
     *      ,TOLCwd_max	     !massima tolleranza sui cicli in T e C
     
      common /Rwd/ TOLFwd,TOLXwd,TOLCwd,toll_pcls,toll_conce,TOLCwd_max	
	
      integer*4 MITNwd       !massimo numero di iterazioni per Newton
     *         ,MITCwd       !massimo numero di iterazioni in T e C
     *         ,mit_pcls     !massimo numero di iterazioni in pcls
     *         ,mit_conce    !massimo numero di iterazioni in conce

      common /Iwd/ MITNwd,MITCwd,mit_pcls,mit_conce
c----------------------------------------------------------------------------------------------
c   Parametri per controlli di precisione dati calcolati
c----------------------------------------------------------------------------------------------	 
      real*4 repsmch
     *      ,rscale
     *      ,zero
     *      ,uno
     *      ,p05          
     *      ,repsQ
     *      ,repsP             
cgpe
      real*4 perc_p,perc_q,max_deltap,max_deltaq
cgpe-end

      common /Reps/ repsmch,rscale,zero,uno,p05,repsQ,repsP
     *      ,perc_p,perc_q,max_deltap,max_deltaq
c--------------------------------------------------------------------------------------------
c  Parametri usati dalla subroutine ntemp (Lib_gas_tc) nel calcolo e aggiornamento di 
c  composizioni e temperature
c--------------------------------------------------------------------------------------------

      integer*4 maxit   !numero iterazioni nel calcolo temperatura nodi  
      real*4 repsTC     !precisione per controllo temperatura nodi calcolata
     *      ,zeroTC	                	

      common /ITCstaznw_par/ maxit
      common /RTCstaznw_par/ repsTC,zeroTC
