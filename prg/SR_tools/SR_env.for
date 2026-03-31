c  Libreria SR_tools.lib
c
c   Contiene le subroutine da usare per leggere e memorizzare gli argomenti che vengono 
c   passati ad un programma in fase di run. Libreria specifica per il progetto Sire
c
c**************************************************************************************
      subroutine get_DatLog(dat,log,ute)
c*********************************************************************************
c   Viene usata per programmi che richiedono come argomenti,nell'ordine,il pathname
c   dell'area dati scenario (dat), della cartella dei log (log) e dei dati utente(ute).
c   Tale subroutine utilizza la subroutine GET_ARGUMENTS per leggere e 
c   memorizzare gli argomenti suddetti passati al programma 
c   al momento del run.
c   Il default è "" sul primo parametro e il parametro precedente sugli altri
c*********************************************************************************
	implicit none
c
c      Parametri formali
c
      character*(*) dat !O    pathname completo dell'area dati 
      character*(*) log !O    pathname completo della cartella dei log
      character*(*) ute !O    pathname completo dell'area utente 

c        Variabili locali

	integer*4 ndim_arg     !Dimensione vettore arg
      parameter (ndim_arg=3)
	character*(1024) arg(ndim_arg)
	integer*4 no_of_arguments
c*********************************************************************************
	call get_arguments(no_of_arguments,arg,ndim_arg)
c
	if (no_of_arguments.ge.1) then
	   dat=arg(1)
	else
	   dat=""
	end if
c
	if (no_of_arguments.ge.2) then
	   log=arg(2)
	else
	   log=dat
	end if
c
	if (no_of_arguments.ge.3) then
	   ute=arg(3)
	else
	   ute=log
	end if
c
	return
	end
c**************************************************************************************
      subroutine ambiente()
c************************************************************************************
c   - Memorizza i percorsi dell'area dati, dell'area log e dell'area utente
c     nelle variabili c_dat, c_log e c_ute della common_env.
c************************************************************************************
c             
	implicit none
c
	include '../inc/common_env.inc'
c
c
c************************************************************************
c

	call get_DatLog(c_dat,c_log,c_ute)

	return
	end
c--------------------------------------------------------------------------
