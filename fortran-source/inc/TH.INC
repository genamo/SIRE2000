!-----------------------------------------------------------------------
!***** TH.INC **** header di SIRE *******************
!-----------------------------------------------------------------------
! MNEMONICI TIPI REGOLAZIONE (in comune fra condizioni iniziali e telecomandi)
!-----------------------------------------------------------------------
      integer*4	TC_UND !0  regolazione indefinita
     -         ,TC_PV  !1  regolazione in pressione valle
     -         ,TC_P   !1  regolazione in pressione 
     -         ,TC_PM  !2  regolazione in pressione monte
     -         ,TC_Q   !3  regolazione in portata
     -         ,TC_T   !4  regolazione in temperatura
     -         ,TC_ON  !5  regolazione in stato on
     -         ,TC_OFF !6  regolazione in stato off
     -         ,TC_NR  !7  regolazione in stato non ritorno (vl,ce)
     -         ,TC_PQ  !8  regolazione simultanea in PM, PV, Q (vl,ce)
     -         ,TC_NPV !9  pressione valle con non ritorno
     -         ,TC_NPM !10 pressione monte con non ritorno
     -         ,M_REGO !n.ro complessivo tipi di regolazione 

      parameter (TC_UND = G_UND
     -          ,TC_PV  = 1
     -          ,TC_P   = TC_PV
     -          ,TC_PM  = TC_P   + 1
     -          ,TC_Q   = TC_PM  + 1
     -          ,TC_T   = TC_Q   + 1
     -          ,TC_ON  = TC_T   + 1
     -          ,TC_OFF = TC_ON  + 1
     -          ,TC_NR  = TC_OFF + 1
     -          ,TC_PQ  = TC_NR  + 1
     -          ,TC_NPV = TC_PQ  + 1
     -          ,TC_NPM = TC_NPV + 1
     -          ,M_REGO = TC_NPM)

c-elena
c  non essendo più usata si potrebbe eliminare la regolazione in temperatura TC_T 
c  a cui non corrisponde nessuno stato di regolazione
!-----------------------------------------------------------------------
! MNEMONICI STATI REGOLAZIONE
!-----------------------------------------------------------------------
      integer*4 STA_UND   !0  stato indefinito
     -         ,STA_OFF   !1  off
     -         ,STA_NR    !2  nr non ritorno
     -         ,STA_QM    !3  portata max 
     -         ,STA_QC    !4  portata max curva caratteristica
     -         ,STA_Q     !5  set in portata
     -         ,STA_PM    !6  set in pressione monte
     -         ,STA_PV    !7  set in pressione valle
     -         ,STA_PN    !8  pressione min
     -         ,STA_PW    !9  potenza max (centrale)
     -         ,STA_ON    !10 flusso libero unidirezionale
     -         ,STA_FR    !11 flusso libero bidirezionale
     -         ,STA_IL    !12 stato illegale
     -         ,STA_ROM   !13 rapporto di compressione max
     -         ,STA_P     !14 set in pressione monte e valle
     -         ,STA_PMQ   !15 set in pressione monte e portata
     -         ,STA_PVQ   !16 set in pressione valle e portata
     -         ,STA_PQ    !17 set in pressione monte,valle e portata
     -         ,STA_PMMIN !18 pressione min monte
     -         ,STA_PVMIN !19 pressione min valle
     -         ,STA_PMMAX !20 pressione max monte
     -         ,STA_PVMAX !21 pressione max valle
     -         ,STA_PU    !22 set in pressione per unari   (output)
     -         ,STA_PASP  !23 set in pressione aspirazione (output)
     -         ,STA_PMAN  !24 set in pressione mandata     (output)
     -         ,STA_PAMQ  !25 set in pressione aspirazione,monte e portata
     -         ,M_STA     !n.ro complessivo tipi di stato

      parameter (STA_UND = G_UND
     -          ,STA_OFF   = 1
     -          ,STA_NR    = STA_OFF   + 1
     -          ,STA_QM    = STA_NR    + 1
     -          ,STA_QC    = STA_QM    + 1
     -          ,STA_Q     = STA_QC    + 1
     -          ,STA_PM    = STA_Q     + 1
     -          ,STA_PV    = STA_PM    + 1
     -          ,STA_PN    = STA_PV    + 1
     -          ,STA_PW    = STA_PN    + 1
     -          ,STA_ON    = STA_PW    + 1
     -          ,STA_FR    = STA_ON    + 1
     -          ,STA_IL    = STA_FR    + 1
     -          ,STA_ROM   = STA_IL    + 1
     -          ,STA_P     = STA_ROM   + 1
     -          ,STA_PMQ   = STA_P     + 1
     -          ,STA_PVQ   = STA_PMQ   + 1
     -          ,STA_PQ    = STA_PVQ   + 1
     -          ,STA_PMMIN = STA_PQ    + 1
     -          ,STA_PVMIN = STA_PMMIN + 1
     -          ,STA_PMMAX = STA_PVMIN + 1
     -          ,STA_PVMAX = STA_PMMAX + 1
     -          ,STA_PU    = STA_PVMAX + 1
     -          ,STA_PASP  = STA_PU    + 1
     -          ,STA_PMAN  = STA_PASP  + 1
     -          ,STA_PAMQ  = STA_PMAN  + 1
     -          ,M_STA     = STA_PAMQ)

!ATTENZIONE: gli stati con portata Q calcolata per risoluzione di un sistema
!dipendenti da Q cofluenti agli estremi devono essere compresi fra PM e FR 
!-----------------------------------------------------------------------
! VETTORE STATI FUNZIONAMENTO
!-----------------------------------------------------------------------
      integer*4 STADATC(M_REGO)
      common /sta_tc/ STADATC
!-----------------------------------------------------------------------
