*** TIPO_REG.INC **** TIPI DI REGOLAZIONE ******************************

** DA TABELLA TIPI_REGOLAZIONE
** TIPO_REG_ID = Id.tipo regolazione (chiave)
** Lettura di COD_REG = Tipo regolazione per i programmi di simulazione
** I valori possibili di COD_REG sono quelli definiti in TH.INC
*I valori per TIPO_REG_ID sono:
* PMAN   =  1  !set pressione mandata
* PASP   =  2  !set pressione aspirazione
* QSET   =  3  !set portata
* PSET   =  4  !set pressione
* NPSET  =  5  !set pressione con NR
* PVSET  =  6  !set pressione valle
* NPVSET =  7  !set pressione valle con NR
* PMSET  =  8  !set pressione monte
* NPMSET =  9  !set pressione monte con NR
* PQSET  = 10  !set portata + set pressione monte e valle
* ON     = 11  !ON
* OFF    = 12  !OFF
* NR     = 13  !NR (non ritorno) 
* NPMAN  = 14  !set pressione mandata con NR
* NPASP  = 15  !set pressione aspirazione con NR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      integer*4 tipo_reg_dim   !! dimensione vettore
      parameter( tipo_reg_dim = 15)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

* Valori su Data Base
      integer*4 cod_reg_db  !tipi regolazione definiti per la simulazione

      common/tipo_reg_area/
     -       cod_reg_db (tipo_reg_dim) 

*** TIPO_REG.INC **** TIPI DI REGOLAZIONE ******************************
