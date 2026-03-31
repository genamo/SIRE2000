C-----------------------------------------------------------------------
c rsa_ce 26-10-2000
!      SUBROUTINE LEVMARQ (NVAR_TOT,NVAR_PRES,
!     *                   y,WK,IWK,ERROR,EL,
!     *		         IER,IDMAX,IDM,H_H,T_T,
!     *                   f_qimpo,f_pimpo,f_controllo)
      SUBROUTINE din_LEVMARQ (NVAR_TOT,NVAR_PRES,NVAR_TCON,
     *                   y,WK,IWK,ERROR,EL,
     *		         IER,IDMAX,IDM,H_H,T_T)

        implicit none
        include '../inc/PARAM.inc'     !:costanti
        include '../inc/TI.inc'        !:IN
        include '../inc/tj.inc'        ! 
        include '../inc/tjt.inc'       !:telemisure
        include '../inc/td.inc'        !:ravotd
        include '../inc/ty.inc'        !:dproty
        include '../inc/ric.inc'       !:parametri riconciliazione
                                                
C 
C                    SPECIFICATIONS FOR ARGUMENTS
c	NVAR_TOT = numero di incognite totale (vettore di stato)
c	NVAR_PRES = numero di incognite Pressione
c rsa_ce 26-10-2000
c	NVAR_TCON = numero degli elemnti speciali con stato ON
c	da cui si puo' ricavare NVAR_PORT = numero di incognite Portata
C                                   "     = NVAR_TOT - NVAR_PRES
c	Y   = vettore di stato di dimensioni NVAR_TOT
c       WK  = area di lavoro reale  di dimensioni IDMAX
c       IWK = area di lavoro intera di dimensioni IDMAX
c       ERROR = vettore di dimensioni NVAR_TOT contenente la somma degli 
c               incrementi relativi al processo iterativo di convergenza 
c               per il tempo T
c       EL    = vettore dei coefficienti del metodo di GEAR
c       IER   = indice di errore
c       IDMAX = dimensione delle aree di lavoro
c       IDM   = dimensione massima dello Jacobiano
c       H_H   = passo di integrazione
c       T_T   = tempo comprensivo anche di H_H
c

c rsa_ce 26-10-2000
!      integer*4    NVAR_TOT,NVAR_PRES
      integer*4    NVAR_TOT,NVAR_PRES,NVAR_TCON
      real*4       y(NVAR_TOT,*),WK(*)
      integer*4    IWK(*)
      real*4       error(*),EL(*)
      integer*4    IER,IDMAX,IDM
      real*4       H_H,T_T


C 
C                    SPECIFICATIONS FOR Variabiles
c	NEQ_BIL_DIFF = numero di equazioni Bilancio (parte differenziale)
c          IEQ_BIL_DIFF - FEQ_BIL_DIFF    (inizio e fine)
c	NEQ_BIL_ALG = numero di equazioni Bilancio (parte algebrica)
c          IEQ_BIL_ALG - FEQ_BIL_ALG    (inizio e fine)
c	NEQ_FERG = numero di equazioni Fergusson (parte algebrica)
c          IEQ_FERG - FEQ_FERG    (inizio e fine)
c	NEQ_QIMP = numero di equazioni Q imposta (parte algebrica)
c          IEQ_QIMP - FEQ_QIMP    (inizio e fine)
c	NEQ_PIMP = numero di equazioni P imposta (parte algebrica)
c          IEQ_PIMP - FEQ_PIMP    (inizio e fine)
c	NEQ_TOT = numero di equazioni Totali 
c          "    = NEQ_BIL + NEQ_FERG + NEQ_QIMP + NEQ_PIMP

      integer*4    NEQ_TOT
     -            ,NEQ_BIL_DIFF,IEQ_BIL_DIFF,FEQ_BIL_DIFF
     -            ,NEQ_BIL_ALG,IEQ_BIL_ALG,FEQ_BIL_ALG
     -            ,NEQ_FERG,IEQ_FERG,FEQ_FERG
     -            ,NEQ_QIMP,IEQ_QIMP,FEQ_QIMP
     -            ,NEQ_TCON,IEQ_TCON,FEQ_TCON
     -            ,NEQ_PIMP,IEQ_PIMP,FEQ_PIMP



      integer*4    mtm_first(RC_M_VAR),mtm_entry(RC_M_TXM)
      REAL*4       mtm_value(RC_M_TXM)

      integer*4    GRAD,DELX,XN,SCALE,FUN,PWT,PW
      integer*4    GRAD1,DELX1,XN1,SCALE1,FUN1,PWT1,PW1
      integer*4	   IPW,IPWT,JPW,JPWT,IPW1,IPWT1,JPW1,JPWT1

      REAL*4       PREC,BASE

      integer*4    i,iter,icount,j,ind,iflag
      real*4       al,f0,f0sq,erl2,erl2x,
     *             absx,up,onesf0,g
      REAL*8       ssq,ssqold,sqdif
      REAL*4       sum
      real*4       x(RC_M_VAR)
      real*4       H_D,H_F,H_P,H_Q        ! Fase di test per scalatura equazioni
      integer*4    ibad
      real*4       r_tol, a_tol
	real*4       p2_1, p2_2
      INTEGER*4    jj,nci,ncf,jmin,jmax,jc
crsa2sy
      INTEGER*4    neq_max
c
      INTEGER*4    itop_from_ivar         ! Function
C------------------------------------------------------------------------
   
      H_D = H_H

      H_F = H_H !1.0
      H_P = H_H !1.0
      H_Q = H_H !1.0


      IER = 0                                                           
crsa_agg_tjt
      call din_agg_tjt_mod (T_T,H_H)
crsa_agg_tjt

crsa      if (o_sta_ric.ge.O_ST_NVSCEN) then
C----------------------------->  INIZIALIZZA LE VARIABILI
c---->     the machine precision
c        prec = rc_zero_mac
        call zerom(prec,base)

c        AL = prec*100
        AL = sqrt(prec)
        F0 = 3.0
        UP = 1.E+8
        ONESF0 = 1./F0                                                   
        F0SQ = F0*F0                                                       
        ERL2 = 1.10
        ibad = -99
crsa      endif



      if (o_sta_ric.ge.O_ST_NVTOP) then
C-------------->                 DEFINISCE I PUNTATORI NEI VETTORI
C			         DI SERVIZIO WK, IWK
C                                VETTORE WK
c rsa_ce 26-10-2000
!        neq_max = NVAR_TOT + NVAR_PRES
        neq_max = NVAR_TOT + NVAR_PRES + NVAR_TCON
 
        GRAD=1
        DELX=GRAD+NVAR_TOT
        XN=DELX+NVAR_TOT
        SCALE=XN+NVAR_TOT
        FUN=SCALE+NVAR_TOT
        PWT=FUN+neq_max
        PW=PWT+IDM
        IF(PW+IDM.GT.IDMAX) THEN
           IER=-111
           goto 9999
        END IF
C
        GRAD1=GRAD-1
        DELX1=DELX-1
        XN1  =XN-1
        SCALE1=SCALE-1
        FUN1=FUN-1
        PWT1=PWT-1
        PW1 =PW-1
      
C                                 VETTORE IWK 
        IPWT=1
        IPW=IPWT+(NVAR_TOT+1)
        JPWT=IPW+(neq_max+1)
        JPW=JPWT+IDM
        IF(JPW+IDM.GT.IDMAX) THEN
           IER=-111
           goto 9999
        END IF

C     
        IPWT1=IPWT-1
        IPW1=IPW-1
        JPWT1=JPWT-1
        JPW1=JPW-1
      endif

      if (o_sta_ric.ge.O_ST_NVTLM) then
        call DIN_iniJAC(IWK(IPW),IWK(JPW),
     -                NEQ_BIL_DIFF,NEQ_BIL_ALG,NEQ_FERG,NEQ_QIMP,
     -                NEQ_TCON,NEQ_PIMP,*777)
        NEQ_TOT = NEQ_BIL_DIFF+NEQ_BIL_ALG+NEQ_FERG+
     -            NEQ_QIMP+NEQ_TCON+NEQ_PIMP
        IEQ_BIL_DIFF = 1
        FEQ_BIL_DIFF = IEQ_BIL_DIFF + NEQ_BIL_DIFF -1
        IEQ_BIL_ALG = FEQ_BIL_DIFF + 1
        FEQ_BIL_ALG = IEQ_BIL_ALG + NEQ_BIL_ALG -1
        IEQ_FERG = FEQ_BIL_ALG + 1
        FEQ_FERG = IEQ_FERG + NEQ_FERG -1
        IEQ_QIMP = FEQ_FERG + 1
        FEQ_QIMP = IEQ_QIMP + NEQ_QIMP -1
        IEQ_PIMP = FEQ_QIMP + 1
        FEQ_PIMP = IEQ_PIMP + NEQ_PIMP -1
        IEQ_TCON = FEQ_PIMP + 1
        FEQ_TCON = IEQ_TCON + NEQ_TCON -1
      
        call ordinamat_rs(NEQ_TOT,IWK(IPW),IWK(JPW),WK(PW))
        goto 778
777     continue
        ier=-777
        goto 9999      
778     continue
      endif

crsa_prv
      o_sta_ric = O_ST_ALLOLD
    
C- Inizializza 
      DO I=1,NVAR_TOT
         x(I)= y(I,1) 
         WK(XN1+I)= x(I)
         error(I)= 0.0
      END DO

      call DIN_MODEL(wk(xn),wk(fun))
       DO  I=IEQ_BIL_DIFF,FEQ_BIL_DIFF
         wk(fun1+I) = H_D*wk(fun1+I) - y(I,2)     ! equazioni di bilancio differenziale
       enddo
       DO  I=IEQ_BIL_ALG,FEQ_BIL_ALG
         wk(fun1+I) = H_D*wk(fun1+I)              ! equazioni di bilancio algebrico
       enddo
       DO  I=IEQ_FERG,FEQ_FERG
         wk(fun1+I) = H_F*wk(fun1+I)              ! equazione di Fergusson
       enddo
       DO  I=IEQ_QIMP,FEQ_QIMP
c----------       solo un elemento
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
!         wk(fun1+I) = rc_weit_q*H_Q*(PORTTJT(JJ)
        if (f_densita.eq.1) then
         wk(fun1+I) = rc_peso_ctlm(jj)*H_Q*(PORTTJT(JJ)
     *                    * ro0td(JJ)
     *                    - WK(XN1+iwk(JPW1+J)))    ! elementi speciali
        else
         wk(fun1+I) = rc_peso_ctlm(jj)*H_Q*(PORTTJT(JJ)
     *                    - WK(XN1+iwk(JPW1+J)))    ! elementi speciali
        endif
       enddo
crsatt-ini
       DO  I=IEQ_TCON,FEQ_TCON 
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
	   p2_1 = WK(XN1+iwk(JPW1+j))*temptj(jj)
     *          *zmcitj(jj)*ravotd(jj)
	   p2_1 = p2_1*p2_1

         j = iwk(IPW1+I+1)-1
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
	   p2_2 = WK(XN1+iwk(JPW1+j))*temptj(jj)
     *          *zmcitj(jj)*ravotd(jj)
	   p2_2 = p2_2*p2_2
         wk(fun1+I) = rc_weit_on*H_F*(p2_1-p2_2)    ! P2_1-P2_2 
       enddo
crsatt-fin
       DO  I=IEQ_PIMP,FEQ_PIMP
c----------       solo un elemento
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
!         wk(fun1+I) = rc_weit_p*H_P*(ROCITJT(JJ)
         wk(fun1+I) = rc_peso_ctlm(jj)*H_P*(ROCITJT(JJ)
     *                   - WK(XN1+iwk(JPW1+J)))     ! condizioni imposte
       enddo

c                                   CALCOLA LA NORMA DEL RESIDUO
      SSQ =0.
      DO I=1,NEQ_TOT
        SSQ = SSQ+WK(FUN1+I)*WK(FUN1+I)                                
      END DO

      ITER = 0                                                          
C-------------------------------->INIZIO CICLO ITERAZIONE
C
50    SSQOLD = SSQ                                                      
      ICOUNT= 0
      if (f_residuo.eq.1) Then
        IF (SSQ.lt.rc_rtol_cvS) GO TO 6000
      END IF
C				   CALCOLO DELLO JACOBIANO 


      CALL DIN_evajac(WK(XN),IWK(IPW),IWK(JPW),WK(PW))

c
c       Add the identity matrix
c
c-parte relativa bilancio differenziale
        DO  I=IEQ_BIL_DIFF,FEQ_BIL_DIFF
            NCI = iwk(IPW1+I)
            NCF = iwk(IPW1+I+1)-1

c_topvar_12/08/05
!                DO  J=NCI,NCF
!                    IF(iwk(JPW1+J).NE.I) then
!                      wk(PW1+J) = H_D*EL(1)* wk(PW1+J)
!                    ELSE
!                      wk(PW1+J) = wk(PW1+J) - 1.
!                    ENDIF
!                enddo
            jj = itop_from_ivar(I,rc_topvar)
                DO  J=NCI,NCF
                    wk(PW1+J) = H_D*EL(1)* wk(PW1+J)
                    IF(iwk(JPW1+J).EQ.rc_topvar(jj)) then
                      wk(PW1+J) = wk(PW1+J) - 1.
                    ENDIF
                enddo

        enddo
        DO  I=IEQ_BIL_ALG,FEQ_BIL_ALG
c-parte relativa bilancio algebrico
            NCI = iwk(IPW1+I)
            NCF = iwk(IPW1+I+1)-1
                DO  J=NCI,NCF
                      wk(PW1+J) = H_D*EL(1)* wk(PW1+J)
                enddo
        enddo
c-parte relativa Fergusson algebrica
        DO  I=IEQ_FERG,FEQ_FERG
            NCI = iwk(IPW1+I)
            NCF = iwk(IPW1+I+1)-1
                DO  J=NCI,NCF
                      wk(PW1+J) = H_F*EL(1)*wk(PW1+J)
                enddo
        enddo

c-parte relativa agli elementi speciali
        DO  I=IEQ_QIMP,FEQ_QIMP
c----------       solo un elemento
             j = iwk(IPW1+I)
c            NCI = iwk(IPW1+I)
c            NCF = iwk(IPW1+I+1)-1
c                DO  J=NCI,NCF
!                      wk(PW1+J) = -H_Q*EL(1)*rc_weit_q
                      jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
                      WK(PW1+J) = -H_Q*EL(1)*rc_peso_ctlm(jj)
c                enddo
        enddo

crsatt_ini
c-aggiungo parte relativa elementi ON
        DO  I=IEQ_TCON,FEQ_TCON
c----------       due elementi
          j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)  ! derivata rispetto a ro.
          wk(PW1+J) =  H_F*EL(1)*rc_weit_on
          wk(PW1+J) =  wk(PW1+J)*2.*prestj(jj)*dproty(jj)
          j = iwk(IPW1+I+1)-1
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
          wk(PW1+J) = H_F*EL(1)*rc_weit_on
          wk(PW1+J) = -wk(PW1+J)*2.*prestj(jj)*dproty(jj)  ! derivata rispetto a ro.
        enddo
crsatt_fin

c-aggiungo parte relativa telemisura
        DO  I=IEQ_PIMP,FEQ_PIMP
c----------       solo un elemento
             j = iwk(IPW1+I)
c            NCI = iwk(IPW1+I)
c            NCF = iwk(IPW1+I+1)-1
c                DO  J=NCI,NCF
!                   WK(PW1+J) = -H_P*EL(1)*rc_weit_p
                   jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
                   WK(PW1+J) = -H_P*EL(1)*rc_peso_ctlm(jj)
c                enddo
        enddo

c
C                    CALCOLA LA MATRICE J_TRASPOSTO
C			E IL PRODOTTO J_TRASPOSTO*J
C
      call mtramprod_rs(NEQ_TOT,NVAR_TOT,iwk(ipw),IWK(JPW),WK(PW),
     *       IWK(IPWT),IWK(JPWT),WK(PWT),
     *       mtm_first,mtm_entry,mtm_value)
      IF(mtm_first(NVAR_TOT+1)-1.GT.RC_M_TXM) THEN
         IER=-111
         goto 9999
      END IF
      do i=1,NVAR_TOT
        jmin=mtm_first(i)
        jmax=mtm_first(i+1)-1
        do jc=jmin,jmax
          if ( mtm_entry(jc).eq.i) then
             wk(scale1+i)=mtm_value(jc)
          endif
        enddo
      enddo
C                                  CALCOLO DEL GRADIENTE Mt* F
C
      CALL MVMRS(NVAR_TOT,IWK(IPWT),IWK(JPWT),WK(PWT),WK(FUN),WK(GRAD))
C
C                                  CALCOLO LA NORMA DEL GRADIENTE
      ERL2X = ERL2                                                      
      SUM=0.
      DO I=1,NVAR_TOT
         SUM=SUM+WK(GRAD1+I)*WK(GRAD1+I)
      END DO
      ERL2 = SQRT(SUM)

C-------------------------------->CICLO INTERNO
100   CONTINUE					               
C
cc-aggiorno diagonale con al
      do i=1,NVAR_TOT
        jmin=mtm_first(i)
        jmax=mtm_first(i+1)-1
        do jc=jmin,jmax
          if ( mtm_entry(jc).eq.i) then
             mtm_value(jc)=wk(scale1+i)*(1+al)
          endif
        enddo
      enddo

       do i=1,NVAR_TOT
        WK(DELX1+I)= - WK(GRAD1+I)
       ENDDO

          CALL mindegree(NVAR_TOT,mtm_FIRST,mtm_ENTRY,mtm_VALUE,
     *                                 WK(DELX),IFLAG,F_log)


D      if (iflag.NE.0) PRINT *,'MINDEGREE ERROR! '

       if (iflag.ne.0) then
C                                  MATRICE SINGOLARE
         iflag = 0
         if (ibad .le. 0) then
           goto 300
         else
           goto 200
         endif
       endif
       if (ibad .ne. -99) ibad = 0
C
C
C				CALCOLA LA SOLUZIONE DEL SISTEMA
C


C
C				AGGIORNA I VALORI DI X
      DO I=1,NVAR_PRES
        WK(XN1+I) = X(I) + EL(1) * WK(DELX1+I)
      END DO
      DO I=NVAR_PRES+1,NVAR_TOT                                                      
        WK(XN1+I) = X(I) + EL(1) * WK(DELX1+I)
      END DO

      call DIN_MODEL(WK(XN),wk(fun))
       DO  I=IEQ_BIL_DIFF,FEQ_BIL_DIFF
         wk(fun1+i) = H_D*wk(fun1+i)-(y(i,2)+ERROR(i)+WK(DELX1+i))
       enddo
       DO  I=IEQ_BIL_ALG,FEQ_BIL_ALG
         wk(fun1+i) = H_D*wk(fun1+i)
       enddo
       DO  I=IEQ_FERG,FEQ_FERG
         wk(fun1+i) = H_f*wk(fun1+i)
       enddo
       DO  I=IEQ_QIMP,FEQ_QIMP
c----------       solo un elemento
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
!         wk(fun1+I) = rc_weit_q*H_Q*(PORTTJT(JJ)
        if (f_densita.eq.1) then
         wk(fun1+I) = rc_peso_ctlm(jj)*H_Q*(PORTTJT(JJ)
     *                    * ro0td(JJ)
     *                    - WK(XN1+iwk(JPW1+J)))    ! elementi speciali
        else
         wk(fun1+I) = rc_peso_ctlm(jj)*H_Q*(PORTTJT(JJ)
     *                    - WK(XN1+iwk(JPW1+J)))    ! elementi speciali
        endif
       enddo
crsatt-ini
       DO  I=IEQ_TCON,FEQ_TCON
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
	   p2_1 = WK(XN1+iwk(JPW1+j))*temptj(jj)
     *          *zmcitj(jj)*ravotd(jj)
	   p2_1 = p2_1*p2_1

         j = iwk(IPW1+I+1)-1
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
	   p2_2 = WK(XN1+iwk(JPW1+j))*temptj(jj)
     *          *zmcitj(jj)*ravotd(jj)
	   p2_2 = p2_2*p2_2
         wk(fun1+I) = rc_weit_on*H_F*(p2_1-p2_2)    ! Pmonte=Pvalle
       enddo
crsatt-fin
       DO  I=IEQ_PIMP,FEQ_PIMP
c----------       solo un elemento
         j = iwk(IPW1+I)
         jj = itop_from_ivar(iwk(JPW1+J),rc_topvar)
!         wk(fun1+I) = rc_weit_p*H_P*(ROCITJT(JJ)
         wk(fun1+I) = rc_peso_ctlm(jj)*H_P*(ROCITJT(JJ)
     *                   - WK(XN1+iwk(JPW1+J)))     ! condizioni imposte
       enddo

C                                CALCOLA LA NORMA DEL NUOVO RESIDUO
      SSQ =0.
      DO I=1,NEQ_TOT
         SSQ = SSQ+WK(FUN1+I)*WK(FUN1+I)                                
      END DO
C                                  CHECK DESCENT PROPERTY               
 
      IF (SSQ.Le.SSQOLD) GO TO 250
200   CONTINUE
C                                    INCREMENTA AL E RIPROVA

      ICOUNT = ICOUNT+1                                                 
      AL = AL*F0SQ                                                      
      IF(AL.GT.UP) THEN
	    IER=114
            goto 9999
      END IF
      GO TO 100
C                                  IL TEST SULLA DISCESA E' O.K.
C                                  ADJUST MARQUARDT PARAMETER           
250   CONTINUE
      IF (ERL2X.GT.0.) THEN
          G = ERL2/ERL2X                                                    
          IF (ERL2.LT.ERL2X) AL = AL*MAX(ONESF0,G)                        
      END IF
      AL = MAX(AL,PREC)                                               
C                                  ONE ITERATION CYCLE COMPLETED        

      ITER = ITER+1                                                     

      DO  I=1,NVAR_TOT                                                      
         x(I) = WK(XN1+I)
         error(I) = error(I) +  WK(DELX1+I)
      END DO

C
C                                  RELATIVE CONVERGENCE TEST FOR X      
C
      IND=0
      r_tol = rc_rtol_cvP ! Convergenza Pressioni (Punti)
      a_tol = rc_atol_cvP 
      DO I=1,NVAR_PRES
         ABSX=ABS(x(I))
	 IF(ABS(WK(DELX1+I)).LE.r_tol*ABSX+a_tol)  IND=IND+1
      END DO
      IF(IND.lt.NVAR_PRES) THEN
        goto 5000          ! Convergenza non raggiunta: necessita altre iteraz.
      END IF
      r_tol = rc_rtol_cvQ! Convergenza Portate (Tronchi e Elem. Spec.)
      a_tol = rc_atol_cvQ
      DO I=NVAR_PRES+1,NVAR_TOT
         ABSX=ABS(x(I))
	 IF(ABS(WK(DELX1+I)).LE.r_tol*ABSX+a_tol)  IND=IND+1
      END DO
      IF(IND.lt.NVAR_TOT) THEN
        goto 5000        ! Convergenza non raggiunta: necessita altre iteraz.
      END IF
      goto 6000
5000  continue
crsa      go to 50
crsa      SQDIF = DABS(SSQ-SSQOLD)/DMAX1(SSQOLD,DBLE(rc_rtol_cvS))
      SQDIF = DABS(SSQ-SSQOLD)/DMAX1(SSQOLD,DBLE(0.1))
      IF (SQDIF.gt.rc_rtol_cvS) THEN
         GO TO 50
      END IF
      
6000  continue
crsa2sy
      if (f_controllo .eq. 0 ) goto 9999
!!!!!   Tecnica della DERIVA
C Controllo sulle equazioni di bilancio differenziale
      ind = 0
      a_tol = rc_atol_cnB*h_d  !1.e-3*h_D
      DO I=IEQ_BIL_DIFF,FEQ_BIL_DIFF
	 IF(ABS(WK(FUN1+I)).LE.a_tol)  IND=IND+1
      END DO
      IF(IND.LT.NEQ_BIL_DIFF.and.h_h.gt.t_d_passo_min) THEN
         ier=1
      END IF
C Controllo sulle equazioni di bilancio algebrico
      ind = 0
      a_tol = rc_atol_cnB*h_d  !1.e-3*h_D
      DO I=IEQ_BIL_ALG,FEQ_BIL_ALG
	 IF(ABS(WK(FUN1+I)).LE.a_tol)  IND=IND+1
      END DO
      IF(IND.LT.NEQ_BIL_ALG.and.h_h.gt.t_d_passo_min) THEN
         ier=1
      END IF
      if (f_controllo .eq. 1 ) goto 9999
C Controllo sulle equazioni di Fergusson
      ind = 0
      a_tol = rc_atol_cnF*h_F
      DO I=IEQ_FERG,FEQ_FERG
         IF(ABS(WK(FUN1+I)).LE.a_tol)  IND=IND+1
      END DO
      IF(IND.LT.NEQ_FERG.and.h_h.gt.t_d_passo_min) THEN
         ier = ier + 2
      END IF
      if (f_controllo .eq. 2 ) goto 9999
C Controllo sulle pressioni pmon=pval (pressioni)
      ind = 0
      a_tol = rc_atol_cnOn*H_F
      DO I=IEQ_TCON,FEQ_TCON
	   IF(ABS(WK(FUN1+I)).LE.a_tol)  THEN
           IND=IND+1
         ELSE
	     continue
         ENDIF
      END DO
      IF(IND.LT.NEQ_TCON.and.h_h.gt.t_d_passo_min) THEN
         ier = ier + 4
      END IF
      if (f_controllo .eq. 3 ) goto 9999
C Controllo sulle equazioni imposte (elementi speciali)
      ind = 0
      a_tol = rc_atol_cnQ*H_Q*rc_weit_q
      DO I=IEQ_QIMP,FEQ_QIMP
         J = iwk(IPW1+I)
         JJ = iwk(JPW1+J)
	 IF(ABS(WK(FUN1+I))*RC_PESO_CTLM(JJ).LE.a_tol)  THEN
           IND=IND+1
           IF (WK(FUN1+I).eq.0 .or.
     -         RC_PESO_CTLM(JJ)*2.0.LE.a_tol/ABS(WK(FUN1+I))) THEN
             RC_PESO_CTLM(JJ) = MIN(1.0,RC_PESO_CTLM(JJ)*2.0)
           ENDIF
         ELSE
           RC_PESO_CTLM(JJ) = RC_PESO_CTLM(JJ)/2.0
         ENDIF
      END DO
      IF(IND.LT.NEQ_QIMP.and.h_h.gt.t_d_passo_min) THEN
         ier = ier + 8
      END IF
C Controllo sulle equazioni imposte (pressioni)
      ind = 0
      a_tol = rc_atol_cnP*H_P*rc_weit_p
      DO I=IEQ_PIMP,FEQ_PIMP
         J = iwk(IPW1+I)
         JJ = iwk(JPW1+J)
	 IF(ABS(WK(FUN1+I))*RC_PESO_CTLM(JJ).LE.a_tol)  THEN
           IND=IND+1
           IF (WK(FUN1+I).eq.0 .or.
     -         RC_PESO_CTLM(JJ)*2.0.LE.a_tol/ABS(WK(FUN1+I))) THEN
             RC_PESO_CTLM(JJ) = MIN(1.0,RC_PESO_CTLM(JJ)*2.0)
           ENDIF
         ELSE
           RC_PESO_CTLM(JJ) = RC_PESO_CTLM(JJ)/2.0
         ENDIF
      END DO
      IF(IND.LT.NEQ_PIMP.and.h_h.gt.t_d_passo_min) THEN
         ier = ier + 16
      END IF

9999  continue
!      if (ier.eq.0) then
!	  call din_check_elem(ier)
!	endif

c      if (ier.ne.0)  CALL rec_tj_rom (y(1,1))

      RETURN

C                                  RECUPERO DI UNA MATRICE SINGOLARE
C
300   CONTINUE

      IBAD=1
C                                 RIMPIAZZO GLI ZERI SULLA DIAGONALE
C                                 DELLA MATRICE HESSIANA CON 1.
      do i=1,NVAR_TOT
        jmin=mtm_first(i)
        jmax=mtm_first(i+1)-1
        do jc=jmin,jmax
          if ( mtm_entry(jc).eq.i) then
            if (mtm_value(jc).LE.prec) then
		    mtm_value(jc)=1.
              wk(scale1+i)=mtm_value(jc)
            endif
          endif
        enddo
      enddo
      GO TO 100
      END                                                               
