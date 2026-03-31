*** RIC.INC *** Parametri & Area Common per la riconciliazione ************

      integer*4
     -  RC_M_VAR            ! Numero massimo variabili di stato
     - ,RC_M_ORD            ! ordine massimo per il Gear
     - ,RC_M_JAC            ! Numero massimo elementi jacobiano
     - ,RC_M_TXM            ! Numero massimo elementi matrice prodotto
                            !                     (mat.trasposta x mat.) 
      parameter (
     -  RC_M_VAR = M_OGG    ! Una variabile ogni elemento 
                            !  (non sono considerati Temp.,Comp.,etc.)
     - ,RC_M_ORD = 5        ! Dalla libreria Gear
     - ,RC_M_JAC = 4*M_OGG  ! Mediamente vengono considerate
                            !   3 variabili associate ad ogni elemento
                            !   1 pressione imposta per ogni punto
     - ,RC_M_TXM = 10*M_OGG ! Mediamente (3*3)+1
     -  )

      real*4 
     -  rc_weit_p      ! Peso per le pressioni
     - ,rc_weit_q      ! Peso per le portate
     - ,rc_weit_on     ! Peso per pmon=pval
     - ,rc_atol_cvP    ! Tolleranza (ass.) convergenza Pressioni (punti)
     - ,rc_rtol_cvP    ! Tolleranza (rel.) convergenza Pressioni (punti)
     - ,rc_atol_cvQ    ! Tolleranza (ass.) conv. Portate (tronchi,el.spec.)
     - ,rc_rtol_cvQ    ! Tolleranza (rel.) conv. Portate (tronchi,el.spec.)
     - ,rc_rtol_cvS    ! Tolleranza (rel.) conv. increm. somm. (P e Q)**2
     - ,rc_atol_cnB    ! Tolleranza controllo equaz. Bilancio
     - ,rc_atol_cnF    ! Tolleranza controllo equaz. Fergusson
     - ,rc_atol_cnQ    ! Tolleranza controllo Portate Imposte
     - ,rc_atol_cnP    ! Tolleranza controllo Pressioni Imposte
     - ,rc_atol_cnOn   ! Tolleranza controllo pmon=pval

      real*4 
     -  t_d_passo_min  ! Delta minimo di tempo riconoscibile come passo della dinamica 
                       !  (sul quale non effettuano controlli dinamici)
     - ,t_d_passo_max  ! Delta massimo di tempo ammissibile come passo della dinamica
     - ,t_passo_input  ! Delta minimo tempo fra input (meno di un minuto)

      real*4 
     -  tol_gear       ! Tolleranza nell'accuratezza del GEAR 


      common/parasim/
     -  rc_weit_p   ,rc_weit_q   ,rc_weit_on  ,rc_atol_cvP ,rc_rtol_cvP
     - ,rc_atol_cvQ ,rc_rtol_cvQ ,rc_rtol_cvS ,rc_atol_cnB ,rc_atol_cnF
     - ,rc_atol_cnQ ,rc_atol_cnP ,rc_atol_cnOn
     - ,t_d_passo_min ,t_d_passo_max ,t_passo_input
     - ,tol_gear
	  
      real*4 
     -  t_d_cic        ! Delta (periodo) del ciclo (in ore)
     - ,t_d_prl        ! Delta tra due prerlievi (in ore)
     - ,max_delta_time ! Tempo massimo ammesso per la simulazione (in secondi) 

      parameter (
     -  t_d_cic         = 24      
     - ,t_d_prl         = 1 
     - ,max_delta_time  = 2400.0
     -  )



	  real*4 
     -  rc_peso_ctlm(m_ogg)  ! Peso per il controllo val.calc - val.tlm

      integer*4
     -  rc_topvar(m_ogg)     ! Corrispondenza tra indice topologico e 
                             !      indice vettore di stato
      integer*4
     -  F_TCON,              ! Impone stato ON sugli elementi speciali
                             ! (0:NO, 1:SI)
     -  F_PIMPO,             ! Impone pressione sui punti
                             ! (0:NO, 1:SI)
     -  F_CONTROLLO,         ! Controlli da effettuare in successione
                             ! (0:Nessuno, 1:Bilanci, 2:Fergusson, 3:condizioni imposte)
     -  F_DENSITA,           ! Grandezza da considerare nelle equazioni 
                             ! (0:Portata, 1:Densità)
     -  F_RESIDUO            ! Controllo residuo 
                             ! (0:NO, 1:SI)


        common /RC_AREA/
     -                  rc_peso_ctlm
     -                 ,rc_topvar
     -                 ,f_tcon,f_pimpo,f_controllo
     -                 ,f_densita,f_residuo



      integer*4             ! Stati: Attenzione alla gerarchia
     -  O_ST_NVTAB          !   Nuove Tabelle
     - ,O_ST_NVSCEN         !   Nuovo Scenario
     - ,O_ST_NVTOP          !   Nuova Topologia
     - ,O_ST_NVTLM          !   Nuove Telemisure
     - ,O_ST_ALLOLD         !   Tutto vecchio
                            !
      parameter (
     -  O_ST_NVTAB   = 4
     - ,O_ST_NVSCEN  = 3
     - ,O_ST_NVTOP   = 2
     - ,O_ST_NVTLM   = 1
     - ,O_ST_ALLOLD  = 0
     -  )

      integer*4 o_sta_onl    ! stato per online
     -         ,o_sta_ric    ! stato per riconc.
     -         ,o_flg_pfl    ! flag primo filtraggio      :    0=NO   1=SI
     -         ,o_flg_prc    ! flag prima riconciliazione :    0=NO   1=SI

        common /c_online/o_sta_onl,o_sta_ric,o_flg_pfl,o_flg_prc

