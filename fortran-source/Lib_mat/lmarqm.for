C-----------------------------------------------------------------------
C                                                                       
      SUBROUTINE LEVMARQ (M,N,FCN,FCNJ,TOL,EPS,MAXFN,
     *                   PARM,X,F,WK,IWK,INFER,IEVAL,
     *			 IER,IDM1,IDM2,IDMAX)
      implicit none
C
C                                  SPECIFICATIONS FOR ARGUMENTS
c	M = numero di equazioni
c	N = numero di incognite
c
      integer*4            N,M,MAXFN,IDM,IDMAX,INFER,IEVAL,IER
      integer*4            IWK(*)
      integer*4            GRAD,DELX,XN,XBAD,SCALE,FUN,DD,PWT,LL,PW
      integer*4            GRAD1,DELX1,XN1,XBAD1,SCALE1,FUN1,DD1,
     *				PWT1,LL1,PW1
      integer*4	         IPW,IPWT,IL,JPWT,JL,JPW
      REAL               TOL,EPS,PARM(*),X(*),F(*),WK(*)
c
                        ! parametri formali della subroutine zerom(...):
	real  prec        ! precisione della macchina
     *     ,base        ! base della numerazione
c
      integer*4          idm1,idm2,idim,ipwt1,ipw1,il1,jpwt1,jpw1,jl1,
     *                   ibad,i,iter,icount,j,ind
      real               al,f0,uponesf0,f0sq,erl2,ssq,ssqold,erl2x,sum,
     *                   absx,xdif,sqdif,xhold,up,onesf0,g
c
C------------------------------------------------------------------------
      ier = 0                                                           
c---->     evaluation of the machine precision
      call zerom(prec,base)
c-ace      parm(1) = prec*100.
      parm(1)=prec
C-------------->                 DEFINISCE I PUNTATORI NEI VETTORI
C			         DI SERVIZIO WK, IWK
C                                
c                                          VETTORE WK
      GRAD=1
      DELX=GRAD+N
      XN=DELX+N
      XBAD=XN+N
      SCALE=XBAD+N
      FUN=SCALE+N
      DD=FUN+M
      PWT=DD+N
      PW=PWT+IDM1
      LL=PW+IDM2
      IDIM=LL+IDM2
      IF(IDIM.GT.IDMAX) THEN
         IER=11
         RETURN
      END IF
C
      GRAD1=GRAD-1
      DELX1=DELX-1
      XN1  =XN-1
      XBAD1=XBAD-1
      SCALE1=SCALE-1
      FUN1=FUN-1
      DD1=DD-1
      PWT1=PWT-1
      LL1 =LL-1
      PW1 =PW-1
      
C                                 VETTORE IWK
      IPWT=1
      IPW=IPWT+N+1
      IL=IPW+M+1
      JPWT=IL+N+1
      JPW=JPWT+IDM1
      JL=JPW+IDM2
C     
      IPWT1=IPWT-1
      IPW1=IPW-1
      IL1=IL-1
      JPWT1=JPWT-1
      JPW1=JPW-1
      JL1=JL-1
C----------------------------->  INIZIALIZZA LE VARIABILI
      AL = PARM(1)                                                      
      F0 = PARM(2)                                                      
      UP = PARM(3)                                                      
      ONESF0 = 1./F0                                                   
      F0SQ = F0*F0                                                       
      ERL2 = 1.10
      IBAD = -99                                                        
      INFER = 0
      IEVAL = 0                                                         
C---------------------------------->PRIMA VALUTAZIONE DELLA FUNZIONE
      DO I=1,N
         WK(XN1+I)= X(I) 
      END DO
      CALL FCN(M,N,WK(XN),WK(FUN))
      IEVAL = IEVAL+1                                                   
      SSQ =0.
C                                   CALCOLA LA NORMA DEL RESIDUO
      DO I=1,M
         SSQ = SSQ+WK(FUN1+I)*WK(FUN1+I)                                
      END DO
      ITER = 0                                                          
C-------------------------------->INIZIO CICLO ITERAZIONE
C
50    SSQOLD = SSQ                                                      
      ICOUNT= 0
C				   CALCOLO DELLO JACOBIANO 
c
      CALL FCNJ(M,N,WK(XN),IWK(IPW),IWK(JPW),WK(PW),IDM1,IER)
      IF(IER.GT.0) THEN
        IER = -6
	RETURN
      END IF
C
c                                  calcolo della matrice trasposta
      CALL TRSMAT(M,N,IWK(IPW),IWK(JPW),WK(PW),
     *			IWK(IPWT),IWK(JPWT),WK(PWT),IWK(JL))
C
C                                  CALCOLO DEL GRADIENTE IL
C
      CALL MVMRS(N,IWK(IPWT),IWK(JPWT),WK(PWT),WK(FUN),WK(GRAD))
C
C                                  CALCOLO LA NORMA DEL GRADIENTE
      ERL2X = ERL2                                                      
      ERL2 = 0.
      SUM=0.
      DO I=1,N
         SUM=SUM+WK(GRAD1+I)**2
      END DO
      ERL2 = SQRT(SUM)                                                 
C
C                                  CALCOLA IL TRIANGOLO INFERIORE
C				   DELLA MATRICE J_TRASPOSTO*J
C
      CALL ATAPROD(N,IWK(IPWT),IWK(JPWT),WK(PWT),IWK(IPW),
     *             IWK(JPW),WK(PW),WK(SCALE),IDM2,IER)
      IF(IER.NE.0) THEN
        IER = -8
        RETURN
      END IF
C
C-------------------------------->CICLO INTERNO
100   CONTINUE					               
C
      IF (IEVAL.GE.MAXFN) THEN
	    IER=12
	    RETURN
      END IF
C			           AGGIUNGE IL FATTORE DI
C				   LEVEMBERG-MARQUARD AL
C			           TERMINE DIAGONALE E RICOPIA IL
C                                  TERMINE NOTO
      DO I=1,N
        WK(DD1+I)=WK(SCALE1+I)*(1+AL)
        WK(DELX1+I)=WK(GRAD1+I)
      END DO 
C
C				           DECOMPOSIZIONE DI CHOLESKY
C
      CALL LLTDEC(N,IWK(IPW),IWK(JPW),WK(PW),IWK(IL),
     *            IWK(JL),WK(LL),WK(DD),IDM2,IER)
      IF(IER.LT.0) THEN
         IER = -9
	 RETURN
      ELSE IF(IER.GT.0) THEN
	 IER=0
         IF (IBAD.LE.0) GO TO 300
         GO TO 200
      END IF
      IF (IBAD.NE.-99) IBAD = 0    
C
C				CALCOLA LA SOLUZIONE DEL SISTEMA
C
      CALL LLTSOLV(N,IWK(IL),IWK(JL),WK(LL),WK(DD),WK(DELX))
C
C				AGGIORNA I VALORI DI X
      DO J=1,N                                                      
         WK(XN1+J) = X(J)-WK(DELX1+J)
      END DO
      CALL FCN (M,N,WK(XN),WK(FUN))
      IEVAL = IEVAL+1                                                   
      SSQ =0.
C                                CALCOLA LA NORMA DEL NUOVO RESIDUO
      DO I=1,M
         SSQ = SSQ+WK(FUN1+I)*WK(FUN1+I)                                
      END DO
C                                  CHECK DESCENT PROPERTY               
      IF (SSQ.LE.SSQOLD) GO TO 250
200   CONTINUE
C                                    INCREMENTA AL E RIPROVA
C				     SE IBAD=1 NEL TENTATIVO PRECEDENTE LO
C				     JACOBIANO ERA SINGOLARE
      ICOUNT = ICOUNT+1                                                 
      AL = AL*F0SQ                                                      
      IF(AL.GT.UP) THEN
	 IF(IBAD.EQ.1) THEN
	    IER=13
            RETURN
         ELSE
	    IER=14
            RETURN
         END IF
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
      DO  I=1,N                                                      
         X(I) = WK(XN1+I)
      END DO
      DO I=1,M
         F(I) = WK(FUN1+I)
      END DO
C
C                                  RELATIVE CONVERGENCE TEST FOR X      
C
      IND=0
      DO I=1,N
         ABSX=ABS(X(I))
         XDIF = ABS(WK(DELX1+I))/MAX(ABSX,0.1)
	 IF(XDIF.LE.TOL)  IND=IND+1
      END DO
      IF(IND.EQ.N) THEN
         INFER=1
         RETURN
      END IF
C                                  RELATIVE CONVERGENCE TEST FOR SSQ      
      SQDIF = ABS(SSQ-SSQOLD)/MAX(SSQOLD,0.1)                          
      IF (SQDIF.LE.EPS) THEN
         INFER = 2                                 
         RETURN
      END IF
      GO TO 50
C                                  RECUPERO DI UNA MATRICE SINGOLARE
C                                  
300   CONTINUE
C				   
C				IBAD = -99   PRIMO CASO DI SINGOLARITA'
C					     IN XBAD NON C'E' NIENTE
C				IBAD.=   0   E' GIA STATO TROVATO UN
C					     CASO DI SINGOLARITA' ED
C					     E' MEMORIZZATO IN XBAD	
C				USCENDO DAL RECUPERO IBAD = 1		
      
      IF (IBAD.EQ.0) THEN
C	                        VIENE VERIFICATO CHE IL NUOVO PUNTO 
C				SINGOLARE NON COINCIDA CON IL 
C			        PRECEDENTE.
	  DO I=1,N
             XHOLD = WK(XBAD1+I-1)                                         
             IF(ABS(X(I)-XHOLD).GT.TOL*MAX(0.1,ABS(XHOLD))) GO TO 350
	  END DO
          IER=15
          RETURN
      END IF
350   DO I=1,N
         WK(XBAD1+I)=X(I)
      END DO
      IBAD=1
C                                 RIMPIAZZO GLI ZERI SULLA DIAGONALE
C                                 DELLA MATRICE HESSIANA CON 1.
      DO J=1,N
         IF (WK(SCALE1+J).EQ.0.) THEN
             WK(SCALE1+J)=1.
         END IF
      END DO
      GO TO 100
      END                                                               
c-----------------------------------------------------------------
	SUBROUTINE ATAPROD(N,first_AT,AT_entry,AT_value,
     *			first_A,A_entry,A_value,D_value,IDM,IER)
C----------------------------------------------------
C	CALCOLA IL TRIANGOLO INFERIORE DELLA MATRICE 
C	SIMMETRICA, DEFINITA POSITIVA A_trasposto*A
C	IL RISULTATO E' MEMORIZZATO NELLA FORMA SEGUENTE:
C	I TERMINI SOTTO DIAGONALI NELLA MATRICE A IN FORMA SPARSA
C	I TERMINI DIAGONALI NEL VETTORE D
C____________________________________________________
      implicit none
	REAL     AT_value(1),A_value(1),D_value(1)
	integer*4  first_AT(1),AT_entry(1),first_A(1),A_entry(1)
	integer*4  ind,i,n,il,ir,icp,icu,j,jrl,jru,jcu,k,ic,iniz,jcp,
     *             jrc,jcc,idm,ier
        real       sum
C----------------------------------------------------
	IND=1
	DO I=1,N
	  first_A(I)=IND
	  IL=first_AT(I)
	  IR=first_AT(I+1)-1
	  ICP=AT_entry(IL)
	  ICU=AT_entry(IR)
	  DO j=1,I-1
	    JRL=first_AT(j)
	    JRU=first_AT(j+1)-1
	    sum = 0.
	    JCU = AT_entry(JRU)
	    if(ICP.GT.JCU) goto 20
	    DO 10 K=IL,IR
	      IC=AT_entry(K)
	      if(IC.gt.JCU) goto 20
	      iniz = JRL
	      JCP = AT_entry(iniz)
	      if(JCP.gt.ICU) go to 20
	      DO JRC=iniz,JRU
	        JCC=AT_entry(JRC)
	        IF(JCC.EQ.IC) THEN
	          sum = sum+AT_value(K)*AT_value(JRC)
	          JRL = JRC+1
	          go to 10
	        else if(jcc.gt.ic) then
	          JRL = JRC
	          go to 10
	        END IF
	      END DO
  10        continue
  20	    IF(sum.NE.0.) then
	      A_value(ind) = sum
	      A_entry(ind) = j
	      ind = ind+1
	    end if
	  END DO
c---->               termine diagonale
	  sum = 0.
	  DO K=IL,IR
	    sum = sum+AT_value(k)*AT_value(k)
	  end do
	  D_value(i) = sum
	END DO	 
	first_A(N+1)=IND
	IF(IDM.LE.IND) THEN
	   IDM = IND-1
	   IER = -1
	END IF
	RETURN
	END  
C____________________________________________________________________
	SUBROUTINE LLTDEC(N,first_A,A_entry,A_value,
     *			first_L,L_entry,L_value,d_value,IDM,IER)
C_____________________________________________________________________
C
C	QUESTA SUBROUTINE DECOMPONE LA MATRICE A SIMMETRICA, DEFINITA
C	POSITIVA, NELLA FORMA L*L_TRASPOSTO; 
C	NELLA A E' MEMORIZZATO SOLO IL TRIANGOLO INFERIORE
c	NEL VETTORE D E' MEMORIZZATO IL TERMINE DIAGONALE
C
C___________________________________________________________________
      implicit none
	integer*4  first_A(1),A_entry(1),first_L(1),L_entry(1)
	REAL     A_value(1),L_value(1),d_value(1)
	integer*4  ier,ind,i,n,il,iamin,iamax,jmin,ir,icp,icu,j,jrl,
     *             jru,jcu,k,ic,iniz,jcp,jrc,jcc,iaold,idm
        real       sum
C------------------------------------------------------------------
	IER=0
	if(D_value(1).ne.0.) then
	  D_value(1) = SQRT(D_value(1))
	else
	  ier = 10
	  return
	end if
	IND = 1
	first_L(1) = 1
	DO I=2,N
c--->                   estremi della riga i-esima di L
	  first_L(I)=IND
	  IL=first_L(I)
c--->                   estremi della riga i-esima di A
	  iamin = first_A(i)
	  iamax = first_A(i+1)-1
	  if(iamax.ge.iamin) then
C---->                         calcolo elemento L(i,1)
	    jmin = A_entry(iamin)
	    L_entry(ind) = jmin
	    L_value(ind) = A_value(iamin)/d_value(jmin)
	    iamin = iamin+1
	    IR = IND
	    ICP=L_entry(IL)
	    ICU=L_entry(IR)
	    ind = ind+1
c---->            calcolo sommatoria l(i,k)*l(j,k) per k=1,j
	    DO j=jmin+1,I-1
	      JRL=first_L(j)
	      JRU=first_L(j+1)-1
	      sum = 0.
	      JCU = L_entry(JRU)
	      if(ICP.GT.JCU) goto 20
	      DO 10 K=IL,IR
	        IC=L_entry(K)
	        if(IC.gt.JCU) goto 20
	        iniz = JRL
	        JCP = L_entry(iniz)
	        if(JCP.gt.ICU) go to 20
	        DO JRC=iniz,JRU
	          JCC=L_entry(JRC)
	          IF(JCC.EQ.IC) THEN
	            sum = sum-L_value(K)*L_value(JRC)
	            JRL = JRC+1
	            go to 10
	          else if(jcc.gt.ic) then
	            JRL = JRC
	            go to 10
	          END IF
	        END DO
  10          continue
c---->       ricerca del termin A(i,j)
  20	      iaold = iamin
	      if(A_entry(iamax).lt.j) go to 30
	      do k = iaold,iamax
	        IF(A_entry(K).EQ.J) then
	          sum  = A_value(K)+sum
	          iamin = k+1
	          go to 30
	        else if(A_entry(k).gt.j) then
	          iamin = k
	          go to 30
	        end if
	      END DO
  30	      IF(sum.NE.0.) then
	        L_value(ind) = sum/d_value(j)
	        L_entry(ind) = j
	        IR = ind
	        ICU = J
	        ind = ind+1
	      end if
	    END DO
	  else
	   ir = il-1
	  end if
c---->                   ricerca del termine A(i,i) => d_value(i)
c---->               termine diagonale
	  sum = d_value(i)
	  DO K=IL,IR
	    sum = sum-L_value(k)*L_value(k)
	  end do
	  if(sum.gt.0.) then
	    D_value(i) = SQRT(sum)
	  else
	    ier = 10
	    return
	  end if
	END DO	 
	First_L(n+1) = ind
	IF(IDM.LE.IND) THEN
	   IDM = IND-1
	   IER = -1
	END IF
	RETURN
	END	 	      	         
C__________________________________________________________________
C------------------------------------------------------------------------
	SUBROUTINE LLTSOLV(N,first_L,L_entry,L_value,
     *			D_value,B)
C________________________________________________________________________
C
C	QUESTA SUBROUTINE RISOLVE IL SISTEMA A*X=B, DOVE LA 
C	MATRICE A E' STATA DECOMPOSTA NELLA FORMA L*L_TRASPOSTO
C	D CONTIENE I TERMINI DIAGONALI
C	L CONTIENE I TERMINI SOTTO-DIAGONALI
C	LA SOLUZIONE E' MEMORIZZATA IN B; 
C	INIZIALMENTE B E' IL TERMINE NOTO
C
C_________________________________________________________________________
      implicit none
	integer*4 first_L(*),L_entry(*)
	REAL    B(*),L_value(*),D_value(*)
	integer*4  i,n,ilmin,ilmax,j,jc,k,irl,iru
C_________________________________________________________________________
C--------------------------------------> risoluzione del sistema LY=B
	B(1) = B(1)/D_value(1)
	DO I=2,N
	   ILMIN=first_L(I)
	   ILMAX=first_L(I+1)-1
	   DO J=ILMIN,ILMAX
	      JC=L_entry(J)
	      B(I)=B(I)-L_value(J)*B(JC)
	   END DO
	   B(I)=B(I)/D_value(I)
	END DO
C--------------------------------------> risoluzione di L_trasposto*X=Y
	DO K=N,1,-1
	  B(K)=B(K)/D_value(K)
	  IRL=first_L(K)
	  IRU=first_L(K+1)-1
	  DO J=IRL,IRU
  	    JC=L_entry(J)
	    B(JC)=B(JC)-L_value(J)*B(K)
	  END DO
	END DO
	RETURN
	END
c-------------------------------------------------------------
