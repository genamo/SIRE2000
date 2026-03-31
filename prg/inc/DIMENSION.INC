!----------------------------------------------------------------------
!***** DIMENSION.inc **** dimensioni nei programmi  *******************
!----------------------------------------------------------------------
! Contiene le dimensioni ed i parametri che possono essere modificati
! all`atto di una ISTALLAZIONE di SIRE
!----------------------------------------------------------------------

	integer*4 MAX_MODUL   !max intervalli suddivisione periodo prelievi
	real*4    PRL_PERIODO !valore periodo ciclicita' prelievi (in ore)

	parameter (MAX_MODUL = 24)
	parameter (PRL_PERIODO = 24.)
!------------------------------------------------------------------------
! Tipi di interpolazione prelievi
!------------------------------------------------------------------------
      integer*4  pr_step ! gradino
     -          ,pr_line ! lineare
     -          ,pr_spli ! quadratica 

      parameter (pr_step = 1
     -          ,pr_line = 2
     -          ,pr_spli = 3
     -)
!----------------------------------------------------------------------
! dimensioni rilevanti nei programmi
!----------------------------------------------------------------------
	integer*4 M_vard   	! max n.ro equazioni integrate dal 
				! programma dinamico
	integer*4 M_ord    	! ordine massimo di integrazione +2
	integer*4 M_jac    	! dimensione dello Jacobiano sparso
	integer*4 M_jtr         ! dimensione della decomposizione
				! triangolare dello Jacobiano
				! massimo valore possibile 32000

! dimensioni rilevanti nei programmi
	parameter (M_vard   = 2*M_OGG	)
	parameter (M_ord    = 3		)
crsa	parameter (M_jac    =  5*M_OGG	)
	parameter (M_jac    =  7*M_OGG	)
	parameter (M_jtr    = 15*M_OGG  )
!----------------------------------------------------------------------
