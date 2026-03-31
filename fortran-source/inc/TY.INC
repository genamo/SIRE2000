*********** TY.INC ***** dati privati del dinamico  ***********

      real*4
     -  aferty(m_ogg)    !(TR) coef. Alfa Fergusson
     - ,bferty(m_ogg)    !(TR) coef. Beta Fergusson
     - ,blamty(m_ogg)    !(TR) lambda
     - ,droty (m_ogg)    !(PU) derivata/tempo di RO	
     - ,dproty(m_ogg)    !(PU) derivata/RO    di P 
     - ,consty(m_ogg)    !(CE) consumo

      real*4
     - 	TY0 (M_OGG),TY1 (M_OGG),TY2 (M_OGG)
      common /RTY/
     - 	TY0,TY1,TY2

      equivalence   ! tronchi  Punti       Centrali
     -   (TY0 (1),  aferTY(1), droTY(1),  consTY(1)),
     -   (TY1 (1),  bferTY(1), dproTY(1)),
     -   (TY2 (1),  blamTY(1))
*********** TY.INC ***** dati privati del dinamico  ***********
