**********  FILTRI.INC  **********


      integer*4    c_f_ce_stat,  !flag stato filtro di centrale
     *             c_f_tg_stat   !flag stato filtro di turbogruppo
	                             ! Valori possibili:
 	                             ! 0  ==> filtro assente
	                             ! 1  ==> filtro attivo
								 ! 2  ==> filtro non attivo

cgpe-NU      character*30 c_f_ce_name,    !nome filtro di centrale
cgpe-NU     *             c_f_ce_comm,    !descrittivo filtro di centrale
cgpe-NU     *             c_f_tg_name,    !nome filtro di tg
cgpe-NU     *             c_f_tg_comm     !descrittivo filtro di tg

      common /filtri/
     *                c_f_ce_stat  (max_vert)
     *               ,c_f_tg_stat  (max_unit)
cgpe-NU     *               ,c_f_ce_name  (max_vert)
cgpe-NU     *               ,c_f_ce_comm  (max_vert)
cgpe-NU     *               ,c_f_tg_name  (max_unit)
cgpe-NU     *               ,c_f_tg_comm  (max_unit)

! NOTE: 
c     Presenza filtri:
c     I filtri possono essere introdotti sia a livello di centrale che di turbogruppo e in
c     questo secondo caso possono essere inseriti solo su alcuni dei turbogruppi presenti.

c-------------------------------------------------------------------------------------
c   Area per appoggio dati topologici usati per la definizione di grandezze derivate
      real*4       c_f_ce_flow_prog,c_f_ce_deltap
     *            ,c_f_tg_flow_prog,c_f_tg_deltap

      common /filtri_appo/c_f_ce_flow_prog(max_vert)
     *                   ,c_f_ce_deltap(max_vert)
     *                   ,c_f_tg_flow_prog(max_unit)
     *                   ,c_f_tg_deltap(max_unit)


      real*4       c_f_ce_k,c_f_tg_k
	                       ! coefficiente caratteristico del filtro
	                       ! (ricavato dalla portata di progetto e
						   !  della caduta di pressione associata) 

      common /filtri_deriv/ c_f_ce_k     (max_vert)
     *                     ,c_f_tg_k     (max_unit)
 