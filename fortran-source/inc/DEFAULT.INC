!-------------------------------------------------------------------------------
!  *** DEFAULT.INC *** parametri di default di uso generale ***
!-------------------------------------------------------------------------------
!-default dati TOP
!-----------------------------------------------------------------------
!      real*4    pmin_def  !pressione min (bar_ass)
!     -         ,pmax_def  !pressione max (bar_ass)
!     -         ,tmin_def  !temperatura min (kelvin)
!     -         ,tmax_def  !temperatura max (kelvin)
!     -         ,qmax_def  !portata max (migliaia normal metri cubi ora : n3h)
!      parameter(pmin_def = 2.013
!     -         ,pmax_def = 301.013
!     -         ,tmin_def = 173.15
!     -         ,tmax_def = 373.15
!     -         ,qmax_def = 20000.
!      -)
      real*4    pmin_def  !pressione min (bar_ass) [sotto è err_ko]
     -         ,tmin_fnz  !temperatura min buon funzionamento (kelvin) [sotto è err_ww]
     -         ,pcsmin_fnz  !pcs min buon funzionamento (Kj/KNm3) [sotto è err_ww]
     -         ,pcsmax_fnz  !pcs max buon funzionamento (Kj/KNm3) [sotto è err_ww]
!  sostituisce il valore inserito dall'utente che per la temperatura non è previsto
     -         ,tmin_def  !temperatura min (kelvin) [sotto è err_ko]
      parameter(pmin_def = 0.01
     -         ,tmin_fnz = 273.15
     -         ,pcsmin_fnz = 26372977.19     ! 25 in MJ/Sm3
     -         ,pcsmax_fnz = 79118931.58     ! 75 in MJ/Sm3
     -         ,tmin_def = 263.15
     -)
      real*4    pmin_sys  !pressione min sistema equazioni (bar_ass)
     -         ,tmin_sys  !temperatura min sistema equazioni (kelvin)
     -         ,zmci_sys  !coefficiente di comprimibilità (adimensionale)
      parameter(pmin_sys = 0.005
     -         ,tmin_sys = 262.15
     -         ,zmci_sys = 0.3
     -)
      real*4    port_def   !portata (n3h)
     -         ,pres_def   !pressione (bar)
     -         ,temp_def   !temperatura (kelvin)
      parameter(port_def = 100.
     -         ,pres_def = 61.013
     -         ,temp_def = 288.15
     -)
!-----------------------------------------------------------------------
      real*4 rend_def  !rendimento centrale
     -      ,tarc_def  !temperatura att. air cooler centrale
     -      ,ceup_def  !consumo energetico centrale  ([kJ]/[kWh]) lo stesso di H_RATE
      parameter(rend_def = 0.7
     -         ,tarc_def = 300.
     -         ,ceup_def = 10000.
     -)
!-----------------------------------------------------------------------
      real*4 gas_met_def  !componenti gas default (m_comp)
     -      ,gas_et_def
     -      ,gas_prop_def
     -      ,gas_but_def
     -      ,gas_ibut_def
     -      ,gas_pent_def
     -      ,gas_es_def
     -      ,gas_n2_def
     -      ,gas_co2_def
     -      ,gas_h2s_def
      parameter(gas_met_def  = 0.837
     -         ,gas_et_def   = 0.05
     -         ,gas_prop_def = 0.02
     -         ,gas_but_def  = 0.006
     -         ,gas_ibut_def = 0.0
     -         ,gas_pent_def = 0.003
     -         ,gas_es_def   = 0.0
     -         ,gas_n2_def   = 0.054
     -         ,gas_co2_def  = 0.03
     -         ,gas_h2s_def  = 0.00
     -)
!-----------------------------------------------------------------------
	  real*4 PCLS_DEF_MJsuSm3
	  parameter (PCLS_DEF_MJsuSm3 = 38.1)
	  real*4 Conv_da_Sm3_a_Nm3
	  parameter (Conv_da_Sm3_a_Nm3 = 0.94794)
	  real*4 PCLS_DEF
	  parameter (PCLS_DEF = PCLS_DEF_MJsuSm3 /
     -                      Conv_da_Sm3_a_Nm3 *
     -                      1.e+6)
c	  parameter (PCLS_DEF = 4.1398516e+07)
!-----------------------------------------------------------------------
      real*4 trit_def  !ritardo caratteristico (ore)
      parameter (trit_def = 0.1666667)
!-----------------------------------------------------------------------
      real*4 tr_lunmax !lunghezza massima per un tronco (metri)
      parameter (tr_lunmax = 50000.)
!-----------------------------------------------------------------------
c-sire2_0060
      real*4 taria_pri  !temperatura dell'aria in primavera
      parameter (taria_pri = 298.15)
      real*4 taria_est  !temperatura dell'aria in estate
      parameter (taria_est = 303.15)
      real*4 taria_aut  !temperatura dell'aria in autunno
      parameter (taria_aut = 293.15)
      real*4 taria_inv  !temperatura dell'aria in inverno
      parameter (taria_inv = 288.15)
c-sire2_0060-end
!-----------------------------------------------------------------------

