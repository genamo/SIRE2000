*** SimCent_Main.FOR *** ricerca soluzione stazionaria ***************************

	program SimCent ! programma MAIN di ogni stazionario
		            ! (con modello nuova centrale)
*&*
	implicit none
      include '../inc/param.inc'
      include '../inc/flag.inc'
	include '../inc/err.inc'
      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc

 
	character*(l_D_ERR) msgerr, msg
      INTEGER CONT_ZG
      COMMON /CNTBLK/ CONT_ZG
      INTEGER CONT_DC2
      COMMON /CNTBLK/ CONT_DC2
	real*4 t_iniz,t_fine
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione 
      real*4 delta_ac1(100)
      COMMON /DELTA/ delta_ac1
      real*4 delta_ac2(100)
      COMMON /DELTA/ delta_ac2
      
      CONT_ZG = 0
      CONT_DC2 = 0
      LGU = 120
c Ripulisco il file di log all'avvio
      OPEN (UNIT=LGU, FILE='C:\LogSire\Fortran\Simulazione.log',
     * STATUS='UNKNOWN')
      WRITE(LGU,*) ' '
      CLOSE(LGU, STATUS='DELETE')
      OPEN (UNIT=LGU, FILE='C:\LogSire\Fortran\Simulazione.log',
     * STATUS='NEW')
      CLOSE(LGU)
c Fine pulizia
      OPEN (unit = LGU, file='C:\LogSire\Fortran\Simulazione.log',      
     *  status='old',    access='append')
!-----------------------------------------------------------------------
      fl_ris_gen = err_ok
!-----------------------------------------------------------------------

! inizializzazione dei flag per simcent
      fl_sim  = .true.
	fl_cons = .true.
      fl_din  = .false.
      hassZg1=0.0
      qassZg1=0.0
 
c-mf03
c NOTA per il momento fl_vinc viene settata in modo statico e non
c      parametrizzabile
c      fl_vinc = .true.      ! versione con vincoli operativi
      fl_vinc = .false.      ! versione con rilascio vincoli operativi
c      soglia_cons = 100.    ! per il consumo (portata in condizioni macchina)
      soglia_cons = 10000.   ! per il consumo (portata in condizioni macchina)
c-intervento_temp
      soglia_vio_temp = 1000    ! per la violazione della temperatura massima (o di cen o di tbg)
c-intervento_temp-end
c-mf03-end
c-cgpe-riciclo
      fl_ric_rev = .false.   ! per il criterio giri non viene accettato il riciclo sul min_rev
c-cgpe-riciclo-end
!-----------------------------------------------------------------------
!FDN
!PER ATTACH A VISUAL STUDIO
c      call sleep(6)
!FDN END
!-Inizializzazione delle aree common
      call ini_common
	call ini_par ()

	call apre_conn('SimCent')

!---------------------------------------------------------
!=>Definizione TIPO DI C:\appl\Sire2000SA\Scenario\Privato\Centrale\filesSimcent\RIDP8TJ_CAP_BON_BIPOLARE_CURATOLO_20240129_145213_C
      tipoScenario = TIPOSCEN_CE !**SIMCENT

!---------------------------------------------------------
!-lettura dei dati caratteristici
!-lettura delle condizioni iniziali
!-Validazione
!---------------------------------------------------------
!*********************************************************
      t_iniz = secnds(0.0)

      call leggi_scenario (msgerr,*7777)

	call ini_par_dep ()

      t_fine = secnds(t_iniz)
!*********************************************************

      c_path_dirdati=c_path_dirlog

      call validazione_in(*9999)

!---------------------------------------------------------
!-simulazione o ottimizzazione di centrale
!---------------------------------------------------------
      t_iniz = secnds(0.0)
      call s_cnt (*9999)
      t_fine = secnds(t_iniz)

	call validazione_out(*9999)

!*********************************************************
      call scrivi_scenario (msgerr,*7777)
!*********************************************************
      CLOSE(LGU)
!---------------------------------------------------------
1111  continue
c	call chiude_conn(fl_ris)
c	call exit(fl_ris)
      CLOSE(LGU)
      call messaggio_finale()
	call chiude_conn(fl_ris_gen)
	call exit(fl_ris_gen)

!---------------------------------------------------------
!-gestione dell'errore: programma terminato con errori
9999  continue
      goto 1111

7777  continue
      CLOSE(LGU)
      call gest_error (3,0,"","",0)
      call tipo_err (1,err_ko,msg)
	call gest_error(3,0,' ',msg,0)
      call gest_error (3,0,"",msgerr,0)
      call gest_error (3,0,"","",0)
	goto 9999
      
      END
