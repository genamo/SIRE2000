c gz  -  valore minimo di dt da usare per limitare la velocita' di variazione
c        delle portate
       real*4 dt_rampa/0.016/


C Variazioni nell'unità di tempo (1ora)
       real*4 qperora/60/              ! variazione de 10% in 60 sec.
       real*4 pperora/30/              ! variazione di 0.5 bar a 50 bar in 60 sec.

C Fascia di protezione contro le oscillazioni
       real*4 r_fsc_q/.10/             ! valore in %  per fascia portata
       real*4 a_fsc_q/100/             ! valore assoluto per fascia portata
       real*4 r_fsc_p/.05/             ! valore in %  per fascia pressione
       real*4 a_fsc_p/3/               ! valore assoluto per fascia pressione

C Delta per controllo vincoli
       real*4 delta_q/10/
       real*4 delta_p/.25/

C Memorizzazione dati manovre
       real*4 tempo_man
	   real*4 delta_man

       common/area_man/tempo_man(m_ogg),delta_man(m_ogg)