**** JC.INC ******* Jacobiano della topologia *********************************

!----------------------------------------------------------------------
!
!         contiene tutte le variabili relative al calcolo
!         dello jacobiano e delle funzioni nel sistema stazionario
!
!----------------------------------------------------------------------

        real*4    valsjc

	common /jc/
	
	1    valsjc(m_ogg+max_ce+max_cs)	   ! valori di regolazione

**** JC.INC ******* Jacobiano della topologia *********************************
