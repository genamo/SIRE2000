!------------------------------------------------------------
! *** WB.INC *** Area dati di un singolo Telecomando
!------------------------------------------------------------
! I Telecomandi sono ordinati per Tempo in un file sequeziale
!------------------------------------------------------------
      real*4    timeWB  !tempo inizio manovra (ore/minuti)
      real*4    tritWB  !durata manovra (ore/minuti)
      integer*4 oftoWB  !offset a elemento topologico
      integer*4 hregWB  !tipo di regolazione
      real*4    pmonWB  !set pressione monte
      real*4    pvalWB  !set pressione valle
      real*4    qsetWB  !set portata
      real*4    tsetWB  !set temperatura

      integer*4 otbgWB  !offset a turbogruppo
      integer*4 atbgWB  !attivazione turbogruppo

      common/RWB/
     - timeWB,tritWB,oftoWB,hregWB,pmonWB,pvalWB,qsetWB,tsetWB,
     - otbgWB,atbgWB
!------------------------------------------------------------
