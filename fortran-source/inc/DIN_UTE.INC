*** DIN_UTE.INC *** Grandezze utente per la dinamica ************

      REAL*4 
     -       t_d_out       ! delta (passo) per gli output (in ore)
     -      ,t_ini         ! tempo iniziale della dinamica (in ore)
     -      ,t_fin         ! tempo finale della dinamica (in ore)

      logical*2 
     -       usa_prl        ! usa i prelievi
     -      ,usa_tlc        ! usa i telecomandi
     -      ,usa_tlcic      ! usa i telecomandi ciclici


      common /din_ute/t_d_out,t_ini,t_fin
     -               ,usa_prl,usa_tlc,usa_tlcic

