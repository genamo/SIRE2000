****** AI.INC *** decodifica di mnemonici, suddivisi in classi ********
!-----------------------------------------------------------------------
c-elena      character*8
      character*80
     -  STATai(m_sta)   !stati funzionamento

      common/Sai/STATai
cport!-----------------------------------------------------------------------
cport	data STATai (STA_OFF) / 'OFF'/ 	!elemento off
cport	data STATai (STA_NR ) /	'NR' /	!   "      nr
cport	data STATai (STA_QM ) /	'QM' /	!portata max elemento 
cport	data STATai (STA_QC ) / 'QC' /	!portata max curva caratteristica
cport	data STATai (STA_Q  ) /	'Q'  /	!portata set
cport	data STATai (STA_PM ) /	'PM' /	!pressione set monte
cport	data STATai (STA_PV ) /	'P'  /	!pressione set valle
cport	data STATai (STA_PN ) /	'PN' /	!pressione min punto monte
cport	data STATai (STA_PW ) /	'PW' /	!potenza max centrale
cport	data STATai (STA_ON ) /	'ON' /	!flusso libero unidirezionale
cport	data STATai (STA_FR ) /	'FR' /	!flusso libero bidirezionale
cport	data STATai (STA_IL ) / '###'/  !stato illegale
cport	data STATai (STA_ROM ) / 'ROM'/ !ro max centrale
cport	data STATai (STA_NQM ) / 'NQM'/ !portata max negativa
cport	data STATai (STA_QA  ) / 'QA'/  !portata max pozzi consentita da AGIP
cport	data STATai (STA_PA  ) / 'PA'/  !pressione max pozzi consentita da AGIP
cport!-23/11/99
cport	data STATai (STA_P   ) / 'P'/   !pressione di SET monte e di SET valle
cport	data STATai (STA_PMQ ) / 'PMQ'/ !pressione di SET monte e di SET portata
cport	data STATai (STA_PVQ ) / 'PVQ'/ !pressione di SET valle e di SET portata
cport	data STATai (STA_PQ  ) / 'PQ'/  !pressione di SET monte, di SET valle e
cport                                        !di SET portata
cport!-23/11/99-end
cport
cport^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
cport!-----------------------------------------------------------------------
cport	data STATai (STA_OFF) / 'OFF'/ 	
cport	data STATai (STA_NR ) /	'Non Ritorno ILLEGALE' /	
cport	data STATai (STA_QM ) /	'stato illegale con violazione della portata
cport  *                         massima elemento' /	 
cport	data STATai (STA_QC ) / 'stato illegale con violazione della portata
cport  *                         massima curva caratteristica' /	 	
cport	data STATai (STA_Q  ) /	'stato regolato con set in portata'/
cport	data STATai (STA_PM ) /	'stato regolato con set in pressione monte' /	
cport	data STATai (STA_PV ) /	'stato regolato con set in pressione valle'  /	
cport	data STATai (STA_PN ) /	'stato illegale con violazione della pressione 
cport  *                         minima' /
cport	data STATai (STA_PW ) /	'stato illegale con violazione della potenza
cport  *                         massima' /	 
cport	data STATai (STA_ON ) /	'flusso libero UNIDIREZIONALE'/
cport	data STATai (STA_FR ) /	'flusso libero BIDIREZIONALE'/
cport	data STATai (STA_IL ) / 'stato ILLEGALE'/
cport	data STATai (STA_ROM ) / 'stato illegale con violazione del massimo 
cport  *                          rapporto di compressione' /	 	
cport	data STATai (STA_PU ) /	'stato regolato con set in pressione'  /	
cport!-23/11/99
cport	data STATai (STA_P   ) / 'stato regolato con set in pressione monte
cport  *                          e pressione valle'/
cport	data STATai (STA_PMQ ) / 'stato regolato con set in pressione monte
cport  *                          e portata'/
cport	data STATai (STA_PVQ ) / 'stato regolato con set in pressione monte
cport  *                          e portata'/
cport	data STATai (STA_PQ  ) / 'stato regolato con set in pressione monte,
cport  *                          pressione valle e portata'/
cport!-23/11/99-end
cport
cport^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
