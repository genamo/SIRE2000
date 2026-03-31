	SUBROUTINE GEARSP (N_TOT,n_pres,n_flow,X,H,XEND,HMA,Y,
     *                      INDEX,WK,IDWMAX,IWKA,WKA,IDMAX,IDM,
     *                      UROUND,TOL_GEAR, IER)
       implicit none
c----------------------------------------------------------------------
c
c       Subroutine GEARSP solves a system of algebric/differential equations
c       (DAE) by Levemberg-Marquardt solver.
c       Subroutine GEARSP uses the backward differentiation formulas 
c       of orders one through five. The jacobian is analytic
c       and it is treated as a sparse matrix. 
c
c--------------------------------------------------------------------------
c		specification for arguments
c
	INTEGER*4       n_tot,        ! Dimention of the dipendent variable
     -                  n_pres,       ! Number of pressure of point
     -                  n_flow        ! Number of flow of pipe
        real            x,            ! Independent variable. (TIME)
     -                  h,            ! Next step size
     -                  xend,         ! Independent variable at which
                                      !  the solution is desidered 
     -                  hma           ! Max step size

	REAL		y(*)          ! Dipendent variable
        INTEGER*4       index         ! Parameter used to indicate the type
                                      !  of call to the soubrutine.
	REAL		wk(*)         ! Real work vector
	INTEGER*4       idwmax        ! Dimention real work vector
	INTEGER*4	IWKA(*)       ! Integer work vector (LM) 
	REAL		WKA(*)        ! Real work vector  (LM)
	INTEGER*4       idmax,        ! Dimention real/integer work vector (LM)
     -                  idm           ! Maximum dimention of Jacobian and 
                                      !  its factorization (LM)
        REAL            uround        ! Unit roundoff of the machine
        REAL            tol_gear      ! Accurancy for gear
        INTEGER*4       ier           ! Error parameter

c
c		specification for local variables
c
	INTEGER*4       maxord, i 
        real            d,ayi,dn
        integer*4       nhcut,kgo

	INTEGER*4       nerror, nsave1, ny
        SAVE            nerror, nsave1, ny

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        real            HH_NOCNTR ! Il tempo sotto il quale
                                  ! si evitano i recuperi del GEAR
                                  ! Passa comunque la soluzione
        real            TT,HH,HMIN,HMAX,EPSC,OLDL0
        integer*4       KFLAG,JSTART,n_cntr

	COMMON/GEAR/	TT,HH,HMIN,HMAX,EPSC,OLDL0,HH_NOCNTR,
     *                  KFLAG,JSTART
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c        REAL            tol           ! Accuracy required
c gz 14/7/99        parameter (tol = 1.E-3)
c        parameter (tol = 1.E-2)
c        parameter (tol = 1.E-4)
c______________________________________________________________
c	This block is executed for the initial call only.
c	It contains checking of inputs and initializations
c______________________________________________________________
	IER=0
	TT  = X

c	Check for incorrect input parameters
c
	IF (n_tot.LE.0) GO TO 85
	IF ((X-XEND)*H.GE.0.) GO TO 85
	IF (INDEX.EQ.0) GO TO 10
	IF (INDEX.EQ.2) GO TO 15
	IF (INDEX.EQ.3) GO TO 24
	IF (INDEX.NE.1) GO TO 85
c	Compute work vector indicies. Set pointers into WK
c
        HH_NOCNTR = UROUND*100    !!!!!!!!!!!!
        n_cntr = n_pres+n_flow
	MAXORD = 5
	NERROR = n_tot
	NSAVE1 = NERROR + n_tot
	NY     = NSAVE1 + n_tot
        if ((ny+(maxord+1)*n_tot).gt.idwmax) then
          ier = 18
          goto 88
        endif
c
c	Set error weight vector WK(1):WK(n_tot)	
c
	DO 5 I=1,n_tot
	   WK(I) = ABS(Y(I))
	   IF (WK(I).LT.10.0) WK(I) = 10.0
	   WK(NY+I) = Y(I)
    5      CONTINUE
c
	TT  = X
	HH = H
	IF (ABS(H).LT.UROUND) THEN
	   IER = 17
	   goto 110
	END IF
	HMIN = ABS(H)
c!!!!!       istruzioni cambiate per la simulazione di reti
c	HMAX = ABS(X-XEND)*10.
	hmax = hma
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	EPSC = TOL_GEAR
	JSTART = 0
	NHCUT = 0
	OLDL0 = 1.

	GO TO 30
c
c------------------------------------------------------------
c	This block is for continuation calls only.
c	TOUTP is the previous value of XEND for use in HMAX
c------------------------------------------------------------
c
c!!!!!       istruzioni cambiate per la simulazione di reti
c   10   HMAX = ABS(XEND-TOUTP)*10.
   10	hmax = hma
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	GO TO 45
c
c!!!!!       istruzioni cambiate per la simulazione di reti
c   15   HMAX = ABS(XEND-TOUTP)*10.
   15   hmax = hma
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	IF ( (TT-XEND)*HH.GE.0.) GO TO 95
	GO TO 50
c
c!!!!! istruzione valida per la simulazione di reti di gasdotti
   24	DO 26 I=1,n_tot
	   WK(NY+I) = Y(I)
   26   CONTINUE
c!!!!!       istruzioni cambiate per la simulazione di reti
c	HMAX = ABS(XEND-TOUTP)*10.
	hmax = hma
	HH = H      ! Prende in input lo step passato via lista
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   25   IF (ABS(HH).LT.UROUND) THEN
	   IER = 17
	   goto 110
	END IF
c
c--------------------------------------------------------
c	This block contains the call to the one_step 
c	integrator
c--------------------------------------------------------
c
   30   CALL GRSTSP (WK(NY+1),WK,WK(NERROR+1),WK(NSAVE1+1),
     *               n_tot,n_pres,n_cntr,
     *               IWKA,WKA,IDMAX,IDM,IER)
c
	IF(IER.lt.0) THEN
	   IER = IER*10+1
	   goto 110
	END IF
c
	KGO = 1-KFLAG
	GO TO (35,55,70,80) , KGO
c					KFLAG = 0, -1, -2, -3
   35   CONTINUE
c
c------------------------------------------------------------
c	This block handles the case of a successful return;
c	the weights are updated.
c	Test for stop condition: if INDEX=3, Y is set to the current
c	solution on return; if INDEX=2 the step is controlled to
c	hit XEND, and the current solution is put in Y on return.
c	For any other value of INDEX, control returns to the integrator
c	unless XEND is reached. Then interpolated values of the
c	solution are computed and stored in Y on return.
c	If interpolation is not required, the call to DGRIN 
c	should be remoted.
c       In output X is replaced with the current value of the 
c       independent variable at which integration has been completed.
c------------------------------------------------------------
c
	D=0.
	DO 40 I=1,n_tot
	   AYI = ABS(WK(NY+I))
	   WK(I) = AMAX1(WK(I),AYI)
   40      D = D+(AYI/WK(I))**2
	D= D*(UROUND/TOL_GEAR)**2
	DN = n_tot
	IF (D.GT.DN) GO TO 75
	IF ((INDEX.EQ.3).OR.(INDEX.EQ.1)) GO TO 95
	IF (INDEX.EQ.2) GO TO 50
   45   IF ((TT-XEND)*HH.LT.0.) GO TO 25
	CALL GRIN (XEND,WK(NY+1),n_tot,Y)
	X = XEND
	GO TO 105
   50   IF (((TT+HH)-XEND)*HH.LE.0.)GO TO 25
	IF (ABS(TT-XEND).LE.100.*UROUND*HMAX)GO TO 95
	IF ((TT-XEND)*HH.GE.0.)GO TO 95
	HH = (XEND-TT)+(1.E-4*UROUND)
	JSTART = -1
	GO TO 25
c
c------------------------------------------------------------------
c	This block handles all unsuccessful returns  other then for
c	illegal input
c	If KFLAG=-2 there is an immediate return;
c	if KFLAG=-1 the error test failed;
c	if KFLAG=-3 the convergence failed.
c	To recover HH and HMIN are reduced by a factor of .1 up to
c	10 times before giving up.
c------------------------------------------------------------------
c
   55   IER = 6
   60   IF(NHCUT.EQ.10)GO TO 65
 	NHCUT = NHCUT +1
	HMIN = HMIN*.1
	HH = HH * .1
	JSTART = -1
	GO TO 25
c
   65   IF (IER.EQ.6)  IER=13
	IF (IER.EQ.7)  IER=14
	GO TO 95
c
   70   IER = 15
	GO TO 95
c
   75   IER = 15
	KFLAG = -2
	GO TO 95
c
   80   IER = 7
	GO TO 60
c
   85   IER = 16
   88   ier = -ier
	GO TO 110
c
   95   if (ier .ne. 0) goto 110
        if (HH.lt.HH_NOCNTR) ier = 47
        X = TT
	DO 100 I=1,n_tot
  100   Y(I) = WK(NY+I)
  105   IF(IER.LT.10) INDEX = KFLAG
	h = hh
  110   RETURN
	END
c-------------------------------------------------------------
	SUBROUTINE GRIN(TOUT,Y,N0,Y0)
        implicit none
c
c-------------------------------------------------------------
c	This subroutine computes interpolated values of the 
c	dependent variable Y and stores them in Y0.
c	The interpolation uses the NORDISIECK history array Y.
c-------------------------------------------------------------
c
        real            TOUT
	integer*4       N0
	REAL	 	Y(N0,*),Y0(N0)
        integer*4       i, l, j
        real            s, s1
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        real            HH_NOCNTR
        real            TT,HH,HMIN,HMAX,EPSC,OLDL0
        integer*4       KFLAG,JSTART

	COMMON/GEAR/	TT,HH,HMIN,HMAX,EPSC,OLDL0,HH_NOCNTR,
     *                  KFLAG,JSTART
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	DO 5 I=1,N0
	   Y0(I) = Y(I,1)
    5      CONTINUE
c					
	L = JSTART +1
	S = (TOUT - TT) /HH
	S1= 1.
	DO 15 J=2,L
	   S1 = S1*S
	   DO 10 I = 1,N0
		Y0(I) = Y0(I) + S1*Y(I,J)
   10           CONTINUE
   15      CONTINUE
	RETURN
	END
c---------------------------------------------------------------
	SUBROUTINE GRCS (NQ,EL,TQ)
        implicit none
c
c---------------------------------------------------------------
c	This subroutine computes the coefficients for 
c	the backward differentiation methods of orders 1 to 5
c---------------------------------------------------------------
c
	REAL		TQ(*),EL(*)
        integer*4       nq, k
c					specifications for local variables
	REAL		PERTST(12,2,3)
	DATA		PERTST/1.,1.,2.,1.,.3158,.7407E-1,
     *                  .1391E-1,.2182E-2,.2945E-3,.3492E-4,
     * 			.3692E-5,.3524E-6,1.,1.,.5,.1667,
     *			.4167E-1,7*1.,2.,12.,24.,37.89,
     *			53.33,70.08,87.97,106.9,126.7,
     *			147.4,168.8,191.,2.,4.5,7.333,
     *			10.42,13.7,7*1.,12.,24.,37.89,
     *			53.33,70.08,87.97,106.9,126.7,
     *			147.4,168.8,191.,1.,3.,6.,
     *			9.167,12.5,8*1./
c
        EL(2) = 1.
	GO TO (10,15,20,25,30), NQ
c
   10   EL(1) = 1.
	GO TO 40
   15   EL(1) = 6.6666666E-1
	EL(3) = 3.3333333E-1
	GO TO 40
   20   EL(1) = 5.4545454E-1
	EL(3) = EL(1)
	EL(4) = 9.0909090E-2
	GO TO 40
   25   EL(1) = .48
	EL(3) = .7
	EL(4) = .2
	EL(5) = .02
	GO TO 40
   30   EL(1) = 4.3795620E-1
	EL(3) = 8.2116788E-1
	EL(4) = 3.1021897E-1
	EL(5) = 5.4744525E-2
	EL(6) = 3.6496350E-3
c
  40    DO 45 K=1,3
	   TQ(K) = PERTST(NQ,2,K)
  45       CONTINUE
	TQ(4) = .5*TQ(2)/(NQ+2)
	RETURN
	END
c-----------------------------------------------------------------
	SUBROUTINE GRSTSP(Y,YMAX,ERROR,SAVE1,
     *   		  n_tot,n_pres,n_cntr,
     *                    IWKA,WKA,IDMAX,IDM,IER)
c_________________________________________________________________
        implicit none
c
c	This subroutine performs one step of the integration of a
c       system of Algebric/Differential Equations.
c	Backward differentiation formulas are used. 
c	The code adjusts the stepsize and the order in order to
c	control the local error per step.
c
c-----------------------------------------------------------------
c					Specification for arguments
	INTEGER*4 	n_tot,n_pres,n_cntr
	REAL		Y(n_tot,*),YMAX(*),ERROR(*),SAVE1(*)
        INTEGER*4       IWKA(*)
        REAL            WKA(*)
        INTEGER*4       IDMAX,IDM
        INTEGER*4       ier
c					Specifications for locals
        INTEGER*4       i,j,j1,j2,newq
        REAL            told,rh,r1,pr3,pr2,pr1,enq3,enq2,enq1,
     *                  d,d1



	INTEGER*4 	nq,l,idoub,iret,lmax,iredo
        real            rmax,hold,fn,edn,e,eup
	REAL		TQ(4),EL(6)

        SAVE            NQ,L,IDOUB,IRET,LMAX,IREDO,
     *                  RMAX,HOLD,FN,EDN,E,EUP,
     *			TQ,EL
! rsa_ace 26-10-2000
        integer*4       n_set
c
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        real            HH_NOCNTR
        real            TT,HH,HMIN,HMAX,EPSC,OLDL0
        integer*4       KFLAG,JSTART

	COMMON/GEAR/	TT,HH,HMIN,HMAX,EPSC,OLDL0,HH_NOCNTR,
     *                  KFLAG,JSTART
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! rsa_ace 26-10-2000
        n_set = n_tot - n_cntr

	IER = 0
	KFLAG = 0
	TOLD = TT
	IF (JSTART.NE.0) GO TO 25
c
c-------------------------------------------------------------------
c	Block 1.
c	Initialize. On the first call (JSTART=0) the order is set to
c	1 and the initial dy/dt is calculated.
c	Rmax is the maximum ratio by which H can be increased in a 
c	single step. It is initially set to 1.E4 to compensate for the
c	small initial H, but then it is normally set to 10.
c	If a failure occurs (in corrector convergence or error test)
c	Rmax is set to 2 for the next increase.
c-------------------------------------------------------------------
c


c_ace---------------------------

      CALL DIN_MODEL_Gear (n_tot,n_pres,HH,Y,SAVE1)
c      CALL DIN_MODEL_D (HH,Y,SAVE1)
c      DO  I=1,n_tot
c         Y(I,2) = HH*SAVE1(I)
c      enddo

c      CALL DIN_MODEL (Y,SAVE1)
c      DO  I=1,n_pres 
c         Y(I,2) = HH*SAVE1(I)
c      enddo
c	DO I=n_pres+1,n_tot
c         Y(I,2) = 0.0
c      end do

c_ace---------------------------


	NQ = 1
	L  = 2
	IDOUB = 3
	RMAX  = 1.E+4
	HOLD = HH
	IRET = 3
c====================================
c====================================
c
c-------------------------------------------------------------------
c	Block 2.
c	Computes the coefficients of the formulas for this step. 	
c	Computes E,EDN,EUP.
c	E is a comparison for errors of the current order NQ;
c	EDN is used to test for decreasing the order;
c	EUP is used to test for increasing the order;
c--------------------------------------------------------------------
   15   CALL GRCS (NQ,EL,TQ)
	LMAX = 6
	OLDL0 = EL(1)
   20   FN = n_cntr                !(secondo la teoria ci andrebbe n_tot)
	EDN = FN*(TQ(1)*epsc)**2
	E = FN*(TQ(2)*epsc)**2
	EUP = FN*(TQ(3)*epsc)**2
	GO TO (25,35,50),IRET
c
c----------------------------------------------------------------------
c	Block 3.
c	Checks H : if H has been changed IDOUB is reset to L+1 to 
c	prevent further changes in H for that many steps.
c	Scales the vector Y by the scalar RH; RH is the ratio of new to
c	old value of the step.
c
c----------------------------------------------------------------------
c
   25   IF(HH.EQ.HOLD) GO TO 50
	RH = HH/HOLD
	HH = HOLD
	IREDO = 3
	GO TO 40
   35   RH = AMAX1(RH,HMIN/ABS(HH))
   40   RH = AMIN1(RH,HMAX/ABS(HH),RMAX)
        R1 = 1.
	DO 45 J=2,L
	    R1 = R1*RH
	    DO 45 I=1,n_tot
   45          Y(I,J) = Y(I,J)*R1
	HH = HH*RH
	IDOUB = L+1
	IF (IREDO.EQ.0) GO TO 285
c
c---------------------------------------------------------------------
c	Block 4
c	Computes the predicted values by effectively multiplying the Y
c	array by the PASCAL TRIANGLE MATRIX.
c-----------------------------------------------------------------------
c
   50	TT = TT + HH
        DO 55 J1 = 1,NQ
	   DO 55 J2 = J1,NQ
		J= (NQ+J1)-J2
		DO 55 I=1,n_tot
   55		   Y(I,J) = Y(I,J)+Y(I,J+1)
c
c------------------------------------------------------------------------
c	Block 5.
c	Corrector loop.
c	The sum of the correction is accumulated in the vector ERROR(i).
c	The Y array is not altered in the corrector loop.
c	The updated Y vector is stored temporaly in SAVE1.
c-------------------------------------------------------------------------
c
   75   CONTINUE
c


! rsa_ace 26-10-2000
!       call LEVMARQ (n_tot,n_diff,n_qimp
!     *              y,WKA,IWKA,ERROR,EL,
!     *              IER,IDMAX,IDM,HH,TT,
!     *              1,1,2)
       call din_LEVMARQ (n_tot,n_pres,n_set,
     *              y,WKA,IWKA,ERROR,EL,
     *              IER,IDMAX,IDM,HH,TT)

      IF (IER.EQ.0) GO TO 170
      IF(IER.lt.0) RETURN


cc
c------------------------------------------------------------
c	Block 7.
c	The Y array is retracted to its values before
c	prediction, H is reduced by a factor of 4,if possible,
c	and a new corrector loop is performed.
c	If not, a no-convergence exit is taken.
c------------------------------------------------------------
c
        TT = TOLD
	RMAX = 2.
        DO 160 J1 = 1,NQ
	   DO 160 J2=J1,NQ
		J = (NQ+J1)-J2
		DO 160 I=1,n_tot
  160  		   Y(I,J) = Y(I,J)-Y(I,J+1)
      CALL rec_tj_rom (y(1,1))
	IF (ABS(HH).LE.HMIN*1.0001) GO TO 280
	RH = .25
	IREDO = 1
	GO TO 35
c
c----------------------------------------------------------------
c	Block 8
c	The Marquardt scheme converged. 
c	The error test is made and it is O.K.
c	Update the Y array.
c	Consider changing H if IDOUB = 1; otherwise decrease IDOUB by 1.
c	If IDOUB = 1 and the order is less than 5, then the vector
c	ERROR(i) is saved because it can be used in a possible order
c	increase on the next step.
c	If a change in H is considered, an increase or decrease in
c	order by 1 is considered also.
c	A change in H is made only if it is by a factor of at least
c	1.1. If not IDOUB is set to 10 to prevent testing for that
c	many steps
c-----------------------------------------------------------------
  170   D = 0.
	DO 174 I=1,n_pres
  174      D = D +(ERROR(I)/YMAX(I))**2

	DO 1174 I=n_pres+1,n_cntr
 1174      D = D +(ERROR(I)/(YMAX(I)*100))**2
	IF (D.GT.E.and.hh.ge.HH_NOCNTR) GO TO 190
c
	KFLAG = 0
	IREDO = 0
	DO 180 J=1,L
	   DO 180 I=1,n_tot
  180 		Y(I,J) = Y(I,J)+EL(J)*ERROR(I)
	IF(IDOUB.EQ.1) GO TO 200
	IDOUB = IDOUB-1
	IF (IDOUB.GT.1) GO TO 290
	IF(L.EQ.LMAX) GO TO 290
	DO 185 I=1,n_tot
  185      Y(I,LMAX) = ERROR(I)
	   GO TO 290
c
c----------------------------------------------------------------------
c	Block 9.
c	The error estimate was exceeding the tolerance.
c	KFLAG keeps track of multiple failures.
c	Restore T and Y to their previous values.
c	On first error test failure keep current order or lower order by 1. 
c	Compute the optimum step size and try again unless 3 failures
c	have occurred.
c-----------------------------------------------------------------------
c
  190   KFLAG = KFLAG - 1
	TT = TOLD
        DO 195 J1 = 1,NQ
	   DO 195 J2 =J1,NQ
		J=(NQ+J1)-J2
		DO 195 I=1,n_tot
  195		   Y(I,J) = Y(I,J) - Y(I,J+1)
	RMAX = 2.
C GZ 20-1-99    IF (ABS(HH).LE.HMIN*1.0001) GO TO 270
        IF (ABS(HH).LE.HMIN*1.0001) THEN
C DOVREBBE USCIRE, MA SE NON HA ANCORA AGGIORNATO LE DERIVATE, PROVIAMO PRIMA
C A AGGIORNARLE
          IF (KFLAG.GT.-3) THEN
            KFLAG = -3
            GOTO 260
          ELSE
            GOTO 270
          ENDIF
        ENDIF
C END GZ
	IF (KFLAG.LE.-3) GO TO 260
	IREDO = 2
	PR3 = 1.E+20
	GO TO 210
c
c	Order NQ + 1
c
  200   PR3 = 1.E+20
	IF (L.EQ.LMAX) GO TO 210
	D1 = 0.
	DO 205 I=1,n_pres
  205     D1 = D1+((ERROR(I)-Y(I,LMAX))/YMAX(I))**2
	DO 1205 I=n_pres+1,n_cntr
 1205     D1 = D1+((ERROR(I)-Y(I,LMAX))/(YMAX(I)*100))**2
	ENQ3 = .5/(L+1)
	PR3 = ((D1/EUP)**ENQ3)*1.4+1.4E-6
c
c	Order NQ
c	 
  210   ENQ2 = .5/L
	PR2 = ((D/E)**ENQ2)*1.2+1.2E-6
	PR1 = 1.E+20
	IF (NQ.EQ.1) GO TO 220
c	
c	Order NQ - 1
c
	D = 0.
	DO 215 I=1,n_pres
  215     D = D+(Y(I,L)/YMAX(I))**2
	DO 1215 I=n_pres+1,n_cntr
 1215     D = D+(Y(I,L)/(YMAX(I)*100))**2
	ENQ1=.5/NQ
	PR1 = ((D/EDN)**ENQ1)*1.3+1.3E-6
  220   IF (PR2.LE.PR3) GO TO 225
	IF (PR3.LT.PR1) GO TO 235
	GO TO 230
c
c	Change of order
c
  225   IF (PR2.GT.PR1) GO TO 230
	NEWQ = NQ
	RH = 1./PR2
	GO TO 250
  230   NEWQ = NQ-1
	RH = 1./PR1
	IF (KFLAG.NE.0..AND.RH.GT.1.) RH=1.
	GO TO 250
  235   NEWQ = L
	RH = 1./PR3
	IF (RH.LT.1.1) GO TO 245
	DO 240 I=1,n_tot
  240     Y(I,NEWQ+1) = ERROR(I)*EL(L)/L
	GO TO 255
  245   IDOUB = 10
	GO TO 290		
  250   IF((KFLAG.EQ.0).AND.(RH.LT.1.1)) GO TO 245
	IF (NEWQ.EQ.NQ) GO TO 35
  255   NQ = NEWQ
	L = NQ + 1
	IRET = 2
	GO TO 15
c
c--------------------------------------------------------------------
c	Block 10.
c	On second error test failure, set the order to 1 and reduce
c	the stepsize by a factor of .1	
c	Try again unless 7 failures have occurred.
c	After a total of 7 failures an exit is taken with KFLAG = -2
c--------------------------------------------------------------------
  260   IF (KFLAG.EQ.-7) GO TO 275
	RH = .1
	RH = AMAX1(HMIN/ABS(HH),RH)
	HH = HH*RH

c_ace---------------------------

      CALL DIN_MODEL_Gear (n_tot,n_pres,HH,Y,SAVE1)
c      CALL DIN_MODEL_D (HH,Y,SAVE1)
c      DO  I=1,n_tot
c         Y(I,2) = HH*SAVE1(I)
c      enddo

C      CALL DIN_MODEL (Y,SAVE1)
C      DO  I=1,n_pres 
C         Y(I,2) = HH*SAVE1(I)
C      enddo
C	DO I=n_pres+1,n_tot
C         Y(I,2) = 0.0
C      end do

c_ace---------------------------


	IDOUB = 10
	IF (NQ.EQ.1) GO TO 50
	NQ = 1
	L = 2
	IRET = 3
	GO TO 15
c
c----------------------------------------------------------
c	Block 11
c	All returns are made through this section.
c	H is saved in HOLD.
c----------------------------------------------------------
c	
  270   KFLAG = -1
	GO TO 290
  275   KFLAG = -2
	GO TO 290
  280   KFLAG=-3
	GO TO 290
  285   RMAX = 10.
  290   HOLD = HH
	JSTART = NQ
	RETURN
	END
