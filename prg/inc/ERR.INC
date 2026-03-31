*** ERR.INC *** Parametri per la gestione errori ************

      integer*4
     -  err_ok             ! nessun errore      produce file .ok 
     - ,err_ii             ! errore information produce file .ok 
     - ,err_ww             ! errore warning     produce file .na 
     - ,err_ff             ! errore fatal       produce file .ko1 
     - ,err_ko             ! errore sistema     produce file .ko0 
	   integer*4       err_aa             ! ALARM ERROR

      parameter (
     -  err_ok =   0
     - ,err_ii = 202 
     - ,err_ww = 201
     - ,err_ff = 300
     - ,err_ko = 400, err_aa = 50)


     



      integer*4             ! 
     -  len_rec_log         ! Lunghezza record file di log
                            !" gg/mm hh:mm:ss nome_routine I cod_err desc_err"
     - ,n_space             ! numero di spazi  (+tipo errore)
     - ,l_DT_ER             ! Lunghezza data in mess. errore (gg/mm hh:mm:ss)
     - ,l_N_RTN             ! Lunghezza (max) del nome routine
     - ,l_C_ERR             ! Lunghezza (max) del codice errore
     - ,l_D_ERR             ! Lunghezza (max) della descrizione errore
      parameter (
*    -  len_rec_log = 80
css     -  len_rec_log = 132     !L.R. 02/09/99
     -  len_rec_log = 1000     !S.S. 24/05/05
cport    - ,n_space = 5
     - ,n_space = 6
     - ,l_DT_ER = 14
     - ,l_N_RTN = 15
     - ,l_C_ERR = 10
     - ,l_D_ERR = len_rec_log-n_space-l_DT_ER-l_N_RTN-l_C_ERR
     -  )

cport      integer*4     unit_video/0/,    ! Unita' video    (0:NO)
cport     *              unit_filelog/0/   ! Unita' file log (0:NO)
      integer*4     unit_filelog   ! Unita' file log (0:NO)
cport      integer*4     tipo_outdef       ! Tipo di output di default
      character*(l_N_RTN) nome_app    ! Nome applicazione da visualizzare

cport      common /c_err/unit_video,unit_filelog,
ccport     *              tipo_outdef,nome_app
      common /c_err/unit_filelog,nome_app

