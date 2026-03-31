*********** TZ.INC ***** dati privati del dinamico entalpico ***********
!	dati da integrare con TY
!----------------------------------------------------------------------

      real*4    dhroTZ
     *         ,ctvcTZ, ctpcTZ, toutTZ, depxTZ, tambTZ, cpmeTZ
     *         ,dhmeTZ

      dimension
     -  dhroTZ(M_OGG) ! (TUTTI)     d(DH)/dro 
     - ,ctvcTZ(M_OGG) ! (TUTTI)     cap. termica a vol. costante
     - ,ctpcTZ(M_OGG) ! (TUTTI)     Cp + d(dH)/dT
     - ,toutTZ(M_OGG) ! (TR,EL.SP)  temperatura all'uscita
     - ,depxTZ(M_OGG) ! (TR)        d(energia potenziale)/dx
     - ,tambTZ(M_OGG) ! (TR)        temperatura ambiente media
     - ,cpmeTZ(M_OGG) ! (TR)        Cp' medio sul tronco
     - ,dhmeTZ(M_OGG) ! (TR)        d(dh)/dro medio sul tronco


      real*4    TZ0,TZ1,TZ2,TZ3,TZ4,TZ5,TZ6,TZ7
      common /RTZ/
     -        TZ0(M_OGG),TZ1(M_OGG),TZ2(M_OGG),TZ3(M_OGG)
     -       ,TZ4(M_OGG),TZ5(M_OGG),TZ6(M_OGG),TZ7(M_OGG)

      equivalence
!                       PUNTI       TRONCHI      EL. SP.
     *     (TZ0(1),                 toutTZ(1))           ,
     *     (TZ1(1),                 tambTZ(1)),
     *     (TZ2(1),                 cpmeTZ(1)),
     *     (TZ3(1),                 dhmeTZ(1)),
     *     (TZ4(1),                 depxTZ(1)),
     *     (TZ5(1),   ctvcTZ(1)),          ! tutti
     *     (TZ6(1),   ctpcTZ(1)),          ! tutti    
     *     (TZ7(1),   dhroTZ(1))           ! tutti


*********** TZ.INC ***** dati privati del dinamico entalpico ***********
