*** 77MSTAZ.FOR *** ricerca soluzione stazionaria ***************************

	program staz_nc ! programma MAIN di ogni stazionario
		            ! (con modello nuova centrale)
*&*
	implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'
      include '../inc/flag.inc'
	include '../inc/err.inc'

	character*(l_D_ERR) msgerr, msg

!-----------------------------------------------------------------------
      fl_ris_gen = err_ok
!-----------------------------------------------------------------------
! inizializzazione dei flag per la rete
      fl_sim  = .false.
      fl_din  = .false.
c-mf03
c NOTA per il momento fl_vinc viene settata in modo statico e non
c      parametrizzabile
c      fl_vinc = .true.      ! versione con vincoli operativi
      fl_vinc = .false.      ! versione con rilascio vincoli operativi
c      soglia_cons = 100.    ! per il consumo (portata in condizioni macchina)
      soglia_cons = 10000.   ! per il consumo (portata in condizioni macchina)
c-mf03-end
c-intervento_temp
      soglia_vio_temp = 1000    ! per la violazione della temperatura massima (o di cen o di tbg)
c-intervento_temp-end

c-cgpe-riciclo
c      fl_ric_rev = .true.   ! per il criterio giri viene accettato il riciclo sul min_rev
      fl_ric_rev = .false.   ! per il criterio giri viene accettato il riciclo sul min_rev
                            ! per compatibilità con il NUOVO SIRE.
						  ! Da rendere .false.nella versione definitiva
c-cgpe-riciclo-end
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
!-Inizializzazione delle aree common
      call ini_common
	call ini_par ()

 	call apre_conn('Stazionario')

!---------------------------------------------------------
!=>Definizione TIPO DI SCENARIO
      tipoScenario = TIPOSCEN_RETE_sta !**STAZIONARIO

!---------------------------------------------------------
!-lettura dei dati caratteristici
!-lettura delle condizioni iniziali
!-Validazione
!---------------------------------------------------------
      call leggi_scenario (msgerr,*7777)

	call ini_par_dep ()

	call validazione_in(*7770)

!---------------------------------------------------------
!-algoritmo del newton
	call stazio_nc_rk (*9999)
!---------------------------------------------------------

!---------------------------------------------------------
c	call validazione_out(*9999)
	call validazione_out(*7771)

      fl_est = 1
      call scrivi_scenario (msgerr,*7777)
cgpe-temp
c      call scrittura_ris_rete(*9999)
      call scrittura_ris_rete(*7772)
cgpe-temp-end
      fl_est = 0

	call risultati_pronti()

!---------------------------------------------------------
1111  continue
      call messaggio_finale()
	call chiude_conn(fl_ris_gen)
	call exit(fl_ris_gen)

!---------------------------------------------------------
!-gestione dell'errore: programma terminato con errori
9999  continue
cgpe-temp
      call scrittura_ris_rete_ko(*9999)
cgpe-temp-end

      goto 1111

7770  continue
      msgerr = " Errore in validazione_in"
	goto 7777
7771  continue
      msgerr = " Errore in validazione_out"
	goto 7777
7772  continue
      msgerr = " Errore in scrittura_ris_rete"
	goto 7777
7777  continue
      call gest_error (3,0,"","",0)
      call tipo_err (1,err_ko,msg)
	call gest_error(3,0,' ',msg,0)
      call gest_error (3,0,"",msgerr,0)
      call gest_error (3,0,"","",0)
	goto 9999


      END
!======================================================================