*** 77MDIN_RK.FOR *** ricerca soluzione dinamica ***************************

	program dinamica ! programma MAIN di ogni dinamico
		             ! (con modello nuova centrale)
*&*
	implicit none
      include '../inc/param.inc'
      include '../inc/flag.inc'
      include '../inc/din_ute.inc'
	include '../inc/err.inc'

	character*(l_D_ERR) msgerr, msg

!-----------------------------------------------------------------------
      fl_ris_gen = err_ok
!-----------------------------------------------------------------------
! inizializzazione dei flag per la rete
      fl_sim  = .false.      ! simulazione di rete
      fl_din  = .true.       ! simulazione dinamica

c-vincoli_operativi
      fl_vinc = .false.      ! con rilascio vincoli operativi
      soglia_cons = 10000.   ! soglia di portata (in condizioni macchina)
                             ! da aggiungere al consumo reale per le
						   ! configurazioni che violano un limite operativo
c-vincoli_operativi-end

c-temperatura
      soglia_vio_temp = 1000 ! per la violazione della temperatura massima
                             ! (o di cen o di tbg)
c-temperatura-end

c-riciclo
      fl_ric_rev = .false.   ! per il criterio giri non viene accettato il
                             ! riciclo sul min_rev (diversamente dal NUOVO SIRE)
c-riciclo-end

c-recupero
      fl_cor = .false.
c-recupero-end
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-Inizializzazione delle aree common
      call ini_common
	call ini_par ()


	call apre_conn('Dinamica')

!---------------------------------------------------------
!=>Definizione TIPO DI SCENARIO
      tipoScenario = TIPOSCEN_RETE_din !**DINAMICA

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
	call s_dinamica (*9999)
!---------------------------------------------------------
!---------------------------------------------------------
c   chiude file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (usa_tlc) call close_file_tlc(*7771)
c   chiude file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
1111  continue
      call messaggio_finale()
	call chiude_conn(fl_ris_gen)
	call exit(fl_ris_gen)

!---------------------------------------------------------
!-gestione dell'errore: programma terminato con errori
9999  continue
c   chiude file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (usa_tlc) call close_file_tlc(*9998)
c   chiude file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
9998  continue
	goto 1111

7770  continue
      msgerr = " Errore in validazione_in"
	goto 7777
7771  continue
      msgerr = " Errore in close_file_tlc"
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