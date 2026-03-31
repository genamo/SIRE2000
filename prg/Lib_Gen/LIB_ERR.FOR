c-------------------------------------------------------
c   LIB_ERR.FOR : Libreria gestione messaggi ed errori
c-------------------------------------------------------
      subroutine open_error (         ! Apre gestione errore
     *               U_FLE,           ! Unita' file log
     *               N_FLE,           ! Nome (completo) file log
     *               N_PRG            ! Nome programma
     *                      )
c--------------------------------------------------------
      implicit none
      include '../inc/err.inc'
      include '../inc/param.inc'
cgpe-fase2
      include '../inc/flag.inc'
cgpe-fase2-end
c---------------------------------------------------------------
      integer*4      U_FLE
      character*(*)  N_FLE,
     *               N_PRG

      character*(60)nome_programma
      character*(312)nome_file
      integer*4  stato
c---------------------------------------------------------------
      unit_filelog = U_FLE
      nome_file = N_FLE
      nome_programma = N_PRG

      if (unit_filelog) then

!*********************************************************
!**OLD Versione
!*********************************************************
cgpe-fase2
cgpe-fase2       if (.not.fl_sim) then
cgpe-fase2-end
cgpe-fase2        if (nome_file.eq.' ') nome_file = trim(nome_programma) // '.LOG'
cgpe-fase2        nome_file=trim(c_path_dirlog)//'/'//nome_file
cgpe-fase2
cgpe-fase2       else
cgpe-fase2-end
!*********************************************************
!**NEW Versione (fase 3)
!*********************************************************
!*Nuova_scrittura
        if (nome_file.eq.' ') nome_file = '.LOG'
        nome_file=trim(c_path_dirlog)//nome_file
!*********************************************************
cgpe-fase2
cgpe-fase2       endif
cgpe-fase2-end

        open (unit=unit_filelog,file=nome_file,
     -     recordtype='stream_lf',recl= len_rec_log,
     -     form='formatted',shared,err=100,iostat=stato,
     -     status='replace')

      endif
      goto 1111

100   unit_filelog = 0

1111  continue

      nome_app = nome_programma
      call gest_error (3,0,nome_app,
     *                'Inizio applicazione',0)
      call gest_error (3,0,"","@",0)

      RETURN
      END
c--------------------------------------------------------
      subroutine close_error (       ! Chiude gestione errore
     *                      f_err)
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      integer*4  f_err     ! Flag errore (0:ok, <0:ko, >0 NA[non accettabile])

!*********************************************************
!**OLD Versione
!*********************************************************
c      character*(3)t_file
!*********************************************************
!**NEW Versione
!*********************************************************
      character*(4)t_file
!*********************************************************
      integer*4    stato
      character*(312)nome_file
c---------------------------------------------------------------
      if (f_err.eq.0) then
        call gest_error (3,0,nome_app,
     *                 'Fine applicazione  OK ',0)
        t_file = '.OK'
      elseif (f_err.lt.0) then
        call gest_error (2,0,nome_app,
     *                 'Fine applicazione !!!! KO !!!!',0)

!*********************************************************
!**OLD Versione
!*********************************************************
c        t_file = '.KO'
!*********************************************************
!**NEW Versione
!*********************************************************
        if(f_err.eq.-1)then
           t_file = '.KO0'  !KO senza risultati
        else
           t_file = '.KO1'  !KO con risultati
        endif
!*********************************************************

      else
        call gest_error (3,0,nome_app,
     *                 'Fine applicazione ???? NA ????',0)
        t_file = '.NA'
      endif

!---------------------------------------------------------------
C*** Scrive file "RISULTATO"
!---------------------------------------------------------------
!*********************************************************
!**OLD Versione
!*********************************************************
c      nome_file=trim(c_path_dirlog)//'/'//trim(nome_app)//t_file
!*********************************************************
!**NEW Versione
!*********************************************************
      nome_file=trim(c_path_dirlog)//t_file
!*********************************************************

      open (unit=u_tem,
     -     file=nome_file,
     -     recordtype='stream_lf',recl= len_rec_log,
     -     form='formatted',shared,err=100,iostat=stato,
     -     status='replace')
      close (u_tem)

100   continue

      if (unit_filelog.ne.0) then
        close (unit_filelog)
        unit_filelog = 0
      endif

      RETURN
      END
c--------------------------------------------------------
      subroutine gest_error(            ! Gestione errore
     *                  T_ERR,      ! Tipo errore
     *                  T_OUT,      ! Tipo output
     *                  N_RTN,      ! Nome routine
     *                  D_ERR,      ! Descrizione errore
     *                  C_ERR       ! Codice errore
     *                  )
c--------------------------------------------------------
cport	La subroutine è stata modificata in modo da non 
cport prevedere l'emissione di messaggi a video.
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      integer*4      T_ERR,            ! Tipo errore
                                       !    1 : Fatal
                                       !    2 : Warning
                                       !    3 : Information
     *               T_OUT             ! Tipo output
                                       !    1 : Video
                                       !    2 : FileLog
                                       !    3 : Video & FileLog

      character*(*)  N_RTN             ! Nome routine
      character*(*)  D_ERR             ! Descrizione errore

      integer*4      C_ERR             ! Codice errore

      integer*4     tipo_output,f_tab,f_tip
      integer*4     ps1,s_trim_s
      character*(l_DT_ER)data_err
      character*(1)tipo_err
      character*(data_len)data
      character*(l_N_RTN) nome_rout
css      character*(l_D_ERR) desc_err
!      character*(len_rec_log) desc_err_lunga

	integer*4 stato ! cport
c---------------------------------------------------------------
      tipo_output = T_OUT

      if (tipo_output.eq.0) tipo_output = 2

	if (N_RTN.eq.'') then
        if (tipo_output.eq.2.or.tipo_output.eq.3) then
		if (unit_filelog) then
			write (unit_filelog,200,err=2002,iostat=stato)
     *				D_ERR
		endif
	  endif
200   FORMAT(1x,a)
2002  continue

	return
	endif
      data_err = ' '
      call legge_data(data)
      data_err(1:5) = data(1:5)
      data_err(7:14) = data(12:19)
2222  continue
      if (T_ERR.eq.1) then
        tipo_err = 'F'
      else if (T_ERR.eq.2) then
        tipo_err = 'W'
      else if (T_ERR.eq.3) then
        tipo_err = 'I'
      endif

      ps1 = s_trim_s(N_RTN)
      nome_rout = N_RTN(ps1:)
      ps1 = s_trim_s(D_ERR)
css      desc_err = D_ERR(ps1:)

      if (tipo_output.eq.2.or.tipo_output.eq.3) then
		if (unit_filelog) then
			write (unit_filelog,300,err=3002,iostat=stato) 
css     *				data_err,nome_rout,tipo_err,C_ERR,desc_err
     *				data_err,nome_rout,tipo_err,C_ERR,D_ERR(ps1:)
		endif
4002  continue !cport
3002  continue
      endif
300   FORMAT(1x,a<l_DT_ER>,1x,a<l_N_RTN>,1x,a1,1x,
     *       i<l_C_ERR>,1x,a)
css     *       i<l_C_ERR>,1x,a<l_D_ERR>)

      RETURN
      END
c-------------------------------------------------------
c--------------------------------------------------------
      subroutine messaggio_finale (  ! Sintesi tipo errore
     *                             )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'

c---------------------------------------------------------------
c-------> Controllo il tipo di soluzione raggiunta
   
      if (fl_ris_gen.ge.err_ko) then
	  fl_ris_gen = -1
        call gest_error(3,0,' ',' ',0)
	  call gest_error(3,0,' ',
     *  ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
	  call gest_error(3,0,' ',
     *  ' !!!!!!!!!!     SOLUZIONE NON TROVATA     !!!!!!!!!!',0)
	  call gest_error(3,0,' ',
     *  ' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',' ',0)
	else if (fl_ris_gen.ge.err_ff) then
	  fl_ris_gen = -100
        call gest_error(3,0,' ',' ',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           SOLUZIONE NON TROVATA             !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           CON  RAPPRESENTAZIONE             !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!                RISULTATI KO                 !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',' ',0)
	else if (fl_ris_gen.ge.err_ww) then
	  fl_ris_gen = 1
        call gest_error(3,0,' ',' ',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           SOLUZIONE TROVATA                 !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!                   MA                        !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!            NON ACCETTABILE                  !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',' ',0)
	else if (fl_ris_gen.ge.err_ii) then
	  fl_ris_gen = 0
        call gest_error(3,0,' ',' ',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           SOLUZIONE TROVATA                 !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!                MA CON                       !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           SET NON RISPETTATI                !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',' ',0)
	else
	  fl_ris_gen = 0
        call gest_error(3,0,' ',' ',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!           SOLUZIONE TROVATA                 !!!!!!',0)
        call gest_error(3,0,' ',
     *  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!',0)
        call gest_error(3,0,' ',' ',0)
      end if

	call gest_error(3,0,' ',' ',0)

      RETURN
      END
c-------------------------------------------------------
c--------------------------------------------------------
C subroutine da usare come debug per versione release
C    utilizzano il file di log per scrivere interi o reali
C  -DEB-- "nome variabile":"valore variabile"
c--------------------------------------------------------
      subroutine deb_int(           ! Scrive intero
     *                  var_des,    ! Descrizione variabile
     *                  var_val     ! Valore variabile (intero)
     *                  )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      character*(*)  var_des
      integer*4      var_val

      integer*4      ll,stato
	integer*4      l_dsc
	parameter (l_dsc=45)
c---------------------------------------------------------------
      ll = len(var_des)
	if (ll.gt.l_dsc) ll = l_dsc
	if (unit_filelog) then
			write (unit_filelog,300,err=3002,iostat=stato) 
     *				"-DEBUG-- ",var_des,":",var_val
	endif
3002  continue
300   FORMAT(1x,a,a<ll>,<l_dsc-ll+1>x,a,i25)

      RETURN
      END
c-------------------------------------------------------
      subroutine deb_real(           ! Scrive intero
     *                   var_des,    ! Descrizione variabile
     *                   var_val     ! Valore variabile (reale)
     *                   )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      character*(*)  var_des
      real*4         var_val

      integer*4      ll,stato
	integer*4      l_dsc
	parameter (l_dsc=45)
c---------------------------------------------------------------
      ll = len(var_des)
	if (ll.gt.l_dsc) ll = l_dsc
	if (unit_filelog) then
			write (unit_filelog,300,err=3002,iostat=stato) 
     *				"-DEBUG-- ",var_des,":",var_val
	endif
3002  continue
300   FORMAT(1x,a,a<ll>,<l_dsc-ll+1>x,a,f25.15)

      RETURN
      END
c-------------------------------------------------------
c-------------------------------------------------------
      subroutine mess_param(          ! Scrive messaggio modifica parametri
     *                   i_par 		! Indice parametro modificato
									!   1:
									!   2: Tolleranza comp & temp
     *                   )
c--------------------------------------------------------
      implicit none
      include '../inc/param.inc'
      include '../inc/err.inc'

      integer*4      i_par


	character*(l_D_ERR) msgerr, msg
c---------------------------------------------------------------
      call gest_error (3,0,"   Modifica",
     -                 "Parametri Simulazione",0)

      call tipo_err (1,err_ii,msg)
	call gest_error(3,0,' ',msg,0)
	if (i_par.eq.1) then
	msg=" Aumentato Tempo Minimo per Passo Dinamica" 
	elseif (i_par.eq.2) then
	msg=" Aumentata tolleranza per Temperatura e Composizione" 
	else
	msg=" ??????" 
	endif
      call gest_error (3,0,"",msg,0)

      call gest_error (3,0,"","",0)

      RETURN
      END
c-------------------------------------------------------
