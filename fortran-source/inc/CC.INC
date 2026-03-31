********** CC.INC *** costanti geometrico_fisico_chimiche ******************
!-inizializzate dalla routine 'inicc'

*_______________________________________________________________________

      real*4
     -  ZROMcc    !precisione di macchina
     - ,RZLAcc    !precisione nel calcolo di lambda e zeta
     - ,RECOcc    !rendimento adiabatico di compressione
     - ,PGREcc    !pi greca
     - ,PGR4cc    !pi_greca/4
     - ,AGRVcc    !accelerazione di gravita`
     - ,TULAcc    !numero di Reynold confine fra moto turbolento e laminare
     - ,T0CC      !temperatura di riferimento
     - ,P0cc      !pressione   di riferimento
     - ,Rfercc    !dislivello sotto a cui il tronco e` considerato piano
     - ,PROFcc    !profondita' di interramento tronchi
     - ,RSPEcc    !spessore strato terreno attorno ai tronchi interessato
                  !ai fenomeni termici veloci
     - ,RSPOcc    !spessore rivestimento in polietilene per tubi sottomarini [m]
     - ,CPOLcc    !coef. conducibilita` termica polietilene [Kcal/mhC]
     - ,CCEMcc    !coef. conducibilita' termica cemento [Kcal/mhC]
     - ,ALFAcc    !coef. conducibilita` termica tra tubo e acqua [Kcal/m2hC]
     - ,CAIRcc    !coef. conducibilita` termica tra tubo e aria [Kcal/m2hC]
     - ,CEADcc    !coef. espansione adiabatica
     - ,CTFEcc    !calore specifico
     - ,ROFEcc    !densita` ferro
     - ,ROMEGcc   !omega escursione termica annua
     - ,POCEcc    !fattore conversione potenza centrali
     - ,CSGTcc    !coef. scambio termico gas/tubo Kcal/(m**2 h grado)
     - ,CJOUcc    !fattore di conversione kcal/joule 
     - ,VOLMcc    !volume molare (litri/mole)
     - ,CTPVcc    !capacita` termica a pressione e volume costante
     - ,RAVOcc    !costante universali dei gas Perfetti
     - ,CSATcc    !coef.scambio termico attraversamento tubo Kcal/(m**2 h grado)
     - ,CPWCcc    !alfa*p0/t0 (alfa=27.78) fatt. conv. potenza centrali

      common/RCC/
     -  ZROMcc,RZLAcc,RECOcc,PGREcc,PGR4cc,AGRVcc,TULAcc,T0CC,P0cc
     - ,Rfercc,PROFcc,RSPEcc,RSPOcc,CPOLcc,CCEMcc,ALFAcc,CAIRcc,CEADcc
     - ,CTFEcc,ROFEcc,ROMEGcc,POCEcc,CSGTcc,CJOUcc,VOLMcc,CTPVcc
     - ,RAVOcc,CSATcc,CPWCcc

      real*4
     -  CTVCcc(m_comp) 	!capacita` termica a volume costante
     - ,CTPCcc(m_comp) 	!capacita` termica a pressione costante
     - ,PCLIcc(m_comp)  !potere calorifico inferiore
     - ,PCLScc(m_comp)  !potere calorifico superiore

      common/RCC/
     -  CTVCcc,CTPCcc,PCLIcc,PCLScc

********** CC.INC *** costanti geometrico_fisico_chimiche ******************
