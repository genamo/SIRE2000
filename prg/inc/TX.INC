!------------------------------------------------------------------------
** TX.INC ** dati TOP caratteristici di elemento (primari e derivati)
!------------------------------------------------------------------------
! dati primari : definiti da utente (maschera).
! dati derivati: calcolati (*).
!------------------------------------------------------------------------
      real*4
     -  diamtx(m_ogg)  !diametro interno (metri)           TR
     - ,rluntx(m_ogg)  !lunghezza        (metri)           TR
     - ,rugotx(m_ogg)  !rugosita`        (metri)           TR
     - ,conntx(m_ogg)  !coef.scambio termico (kcal/mhc)    TR
     - ,disltx(m_ogg)  !* dislivello	 (metri)           TR
     - ,volntx(m_ogg)  !* volume semi_tronco o di punto    TR,PU

      real*4
     -  altetx(m_ogg)  !altitudine       (metri)           PU
     - ,pmaxtx(m_ogg)  !pressione max entrante (bar)       PU,unari entranti
     - ,pmintx(m_ogg)  !pressione min                      PU,unari uscenti
       !ha il significato di pressione massima uscente
     - ,renptx(m_ogg)  !* energia potenziale (altezza*gravita/cjoucc) PU
     - ,tambtx(m_ogg)  !* temperatura ambiente equivalente PU

      real*4
     -  qmaxtx(m_ogg)  !portata max (p~0)   (nqm**3)       VL,CE,VD
c-elena	           negli impianti di riduzione è la portata massima
c-elena 		   della linea principale
     - ,trittx(m_ogg)  !ritardo caratteristico (ore)       speciali
c-elena
     - ,qmintx(m_ogg)  !portata max entrante   (nqm**3)    unari
     - ,qmoutx(m_ogg)  !portata max uscente    (nqm**3)    unari
c-elena
      real*4
     -  rendtx(m_ogg)  !rendimento max stimato             CE
     - ,tarctx(m_ogg)  !temperat. attivazione air-cooler   CE
     - ,ceuptx(m_ogg)  !consumo energetico (Kcal/(Kw*h))   CE
      real*4
     -  tsettx(m_ogg)  !temperatura di attivazione riscaldatore
	                   !(temperatura minima)               VS,VR,VD,VG

c-elena
      integer*4 pnt_app_nsm(m_ogg), !puntatore nodo appartenenza
     -          ind_app_nsm(m_ogg)  !indice el. rif. nel nodo app.
	                                                      !unari,VS,VR,CE

      common/Ntx/
     -  pnt_app_nsm,ind_app_nsm
!-----------------------------------------------------------------------
      real*4
     -  tx0(m_ogg),tx1(m_ogg),tx2(m_ogg),tx3(m_ogg),tx4(m_ogg)
     - ,tx5(m_ogg)

      common/Rtx/
     -  tx0,tx1,tx2,tx3,tx4,tx5
!-----------------------------------------------------------------------
!	                  TR       PU      UNARI       VL          CS
!-----------------------------------------------------------------------
      equivalence
     -  (tx0(1),  diamTX(1),tambtx(1),                qmaxtx(1)     )
     - ,(tx1(1),  rugoTX(1),     pmaxtx(1),                rendtx(1))
     - ,(tx2(1),  rlunTX(1),     pmintx(1)                          )
     - ,(tx3(1),  connTX(1),renptx(1),qmintx(1),           tarcTX(1))
     - ,(tx4(1),  dislTX(1),altetx(1),qmoutx(1),tsettx(1),  ceuptx(1))
     - ,(tx5(1),       volnTX(1)     ,         tritTX(1)            )
