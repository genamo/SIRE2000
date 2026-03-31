
      INCLUDE 'cap_bon_par.inc'     ! dimensioni e parametri

      REAL*4    pint_min,pint_max
      REAL*4    pmand_fase1,pmand_fase2,tmand_fase1,tmand_fase2
      REAL*4    p_int,t_int
c-bpcorr
	  logical*2 iopt_last
c-bpcorr-ennd
      common/CAP_BON_LIMITI/ pint_min,pint_max,
     *       pmand_fase1,pmand_fase2,tmand_fase1,tmand_fase2,
     *       p_int,t_int
     *      ,iopt_last
