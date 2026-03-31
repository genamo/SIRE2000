c   Libreria:LIB_ENV 
c
c   Contiene le subroutine da usare per leggere e memorizzare gli argomenti che vengono 
c   passati ad un programma in fase di run.
c   Viene utilizzato il modulo DigitalFortran DFLIB  

c--------------------------------------------------------------------------
      subroutine get_arguments(no_of_arguments,arg,ndim_arg)
c***************************************************************************
c  Restituisce il numero di argomenti passati ad un programma in fase di run
c  e li memorizza in un vettore
c***************************************************************************
	use dflib             
	implicit none
c
c      Parametri formali
c
	integer*4 no_of_arguments !O
      character*(*) arg(*) !O
	integer*4 ndim_arg !I     Dimensione di arg

c        Variabili locali
c
	integer*2 i_arg,status
      integer*4 n_arg
c************************************************************************
	n_arg = NARGS ( )      
	no_of_arguments = n_arg - 1
crsa	if (no_of_arguments.gt.2) no_of_arguments = 2
	if (no_of_arguments.gt.ndim_arg) no_of_arguments = ndim_arg
	do i_arg=1,no_of_arguments
	  CALL GETARG (i_arg ,arg(i_arg),status)    
	  if (status.lt.0) return   
	enddo
c
	return
	end
c----------------------------------------------------------------------------------
