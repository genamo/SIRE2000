**********  AIR_COOLER.INC  **********


      integer*4    c_ac_ce_stat      !indica lo stato dell'ac di centrale
     *            ,c_ac1_stat        !indica lo stato dell'ac1 di tbg
     *            ,c_ac2_stat        !indica lo stato dell'ac2 di tbg

	                                 ! valori possibili:
	                                 ! 0=ac_assente (assente)
                                     ! 1=ac_on(attivo)
                                     ! 2=ac_off (non attivo)
                                     ! 3=ac_bypass(by pass)

c------------------->Trattamento Air Cooler di centrale
      real*4        c_ac_q_prog      !calore scambiato nell'unità di tempo
     *             ,c_ac_taria_prog  !temperatura di riferimento dell'aria
cgpe-NU      character*30  c_ac_name        !nome air cooler
cgpe-NU     *             ,c_ac_comm        !descrittivo air cooler

      common /air_cooler/c_ac_ce_stat(max_vert)
     *                   ,c_ac_q_prog(max_vert)
     *                   ,c_ac_taria_prog(max_vert)
cgpe-NU     *                   ,c_ac_name(max_vert)
cgpe-NU     *                   ,c_ac_comm(max_vert)

c------------------->Trattamento Air Cooler di fase 1
      real*4        c_ac1_q_prog      !calore scambiato nell'unità di tempo
     *             ,c_ac1_taria_prog  !temperatura di riferimento dell'aria
cgpe-NU      character*30  c_ac1_name        !nome air cooler
cgpe-NU     *             ,c_ac1_comm        !descrittivo air cooler

      common /air_cooler1/c_ac1_stat(max_unit)
     *                   ,c_ac1_q_prog(max_unit)
     *                   ,c_ac1_taria_prog(max_unit)
cgpe-NU     *                   ,c_ac1_name(max_unit)
cgpe-NU     *                   ,c_ac1_comm(max_unit)
c
c------------------->Trattamento Air Cooler di fase 2
      real*4        c_ac2_q_prog     !calore scambiato nell'unità di tempo
     *             ,c_ac2_taria_prog !temperatura di riferimento dell'aria
cgpe-NU      character*30  c_ac2_name       !nome air cooler
cgpe-NU     *             ,c_ac2_comm       !descrittivo air cooler

      common /air_cooler2/c_ac2_stat(max_unit)
     *                   ,c_ac2_q_prog(max_unit)
     *                   ,c_ac2_taria_prog(max_unit)
cgpe-NU     *                   ,c_ac2_name(max_unit)
cgpe-NU     *                   ,c_ac2_comm(max_unit)


! NOTE:
!       l'AC di fase 1 è sempre presente e attivo.
!       Gli stati che influenzano il calcolo del deltap sono ac inserito e attivo
!       o ac inserito e non attivo;l'unico stato che influenza il calcolo della 
!       temperatura è ac inserito e attivo
c-------------------------------------------------------------------------------------
c   Area per appoggio dati topologici usati per la definizione di grandezze derivate

      real*4        c_ac1_flow_prog
     *             ,c_ac1_deltap
     *             ,c_ac2_flow_prog
     *             ,c_ac2_deltap
     *             ,c_ac_ce_flow_prog
     *             ,c_ac_ce_deltap

      common /air_cooler_appo/c_ac1_flow_prog(max_unit)
     *                       ,c_ac1_deltap(max_unit)
     *                       ,c_ac2_flow_prog(max_unit)
     *                       ,c_ac2_deltap(max_unit)
     *                       ,c_ac_ce_flow_prog(max_vert)
     *                       ,c_ac_ce_deltap(max_vert)

      real*4     c_ac1_k          !coefficiente caratteristico dell'air cooler fase1
     *          ,c_ac2_k          !coefficiente caratteristico dell'air cooler fase 2
     *          ,c_ac_ce_k        !coefficiente caratteristico dell'air cooler di centrale

      common /ac_deriv/c_ac1_k(max_unit),c_ac2_k(max_unit),
     *                 c_ac_ce_k(max_vert)