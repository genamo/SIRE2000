C       ****************************************************************
C
C       MAIN.F :  DRIVER THAT SOLVES A SPARSE, SYMMETRIC, POSITIVE 
C       DEFINITE LINEAR SYSTEMS VIA SPARSE CHOLESKY FACTORIZATION.  
C       THE MOST EFFICIENT KNOWN ALGORITHMS AND ROUTINES ARE INCLUDED:  
C       THE MULTIPLE MINIMUM DEGREE ROUTINES COME FROM THE MOST RECENT 
C       RELEASE OF SPARSPAK; MOST OF THE SYMBOLIC FACTORIZATION ROUTINES 
C       AND ALL OF THE NUMERICAL FACTORIZATION ROUTINES WERE DEVELOPED 
C       AT OAK RIDGE NATIONAL LABORATORY.
C
C       ****************************************************************
C       ****************************************************************
C
C       THE AMOUNT OF STORAGE REQUIRED TO SOLVE A LINEAR SYSTEM
C       DEPENDS ON THE FOLLOWING PARAMETERS, EACH OF WHICH IS EITHER
C       READ IN FROM A FILE OR COMPUTED DURING THE SOLUTION PROCESS.
C
C       (1) N      -- THE NUMBER OF ROWS (COLUMNS) IN A.
C
C       (2) NSUPER -- THE NUMBER OF SUPERODES (NSUPER <= N).
C
C       (3) NNZA   -- THE NUMBER OF NONZERO ENTRIES IN A, EXCLUDING
C                     ENTRIES ON THE MAIN DIAGONAL.
C
C       (4) NSUB   -- THE NUMBER OF ROW SUBSCRIPTS NEEDED TO 
C                     REPRESENT THE ZERO-NONZERO STRUCTURE OF L. 
C
C       (5) NNZL   -- THE NUMBER OF NONZERO ENTRIES IN L, INCLUDING
C                     ENTRIES ON THE MAIN DIAGONAL.
C
C       (6) TMPSIZ -- THE SIZE OF THE FLOATING POINT WORKING STORAGE 
C                     REQUIRED BY THE FACTORIZATION ROUTINE (BLKFCT).
C
C       WITH THE EXCEPTION OF THE IWORK(*) ARRAY, THE LENGTH OF EACH 
C       ARRAY SHOULD BE N+1, NSUPER+1, OR ONE OF THE PRECEDING SIX
C       PARAMETERS. IN THIS DRIVER THE LENGTH OF EACH ARRAYS IS
C       DECLARED AT RUN TIME, WHICH IS, OF COURSE, BEFORE THE
C       APPROPRIATE LENGTH IS KNOWN.  THE FOLLOWING PARAMETERS,
C       INITIALIZED IN THE CODE BELOW, ARE UPPER BOUNDS ON THE SIX
C       VARIABLES LISTED ABOVE AND ARE USED TO DECLARE THE LENGTH OF
C       ALL ARRAYS USED BY THE PROGRAM.
C
C       (1) NMAX    -   UPPER BOUND ON N AND NSUPER.
C       (2) ANZMAX  -   UPPER BOUND ON NNZA.
C       (3) SUBMAX  -   SIMULTANEOUSLY AN UPPER BOUND ON NSUB AND NNZA.
C       (4) LNZMAX  -   UPPER BOUND ON NNZL.
C       (5) TMPMAX  -   UPPER BOUND ON TMPSIZ.
C       (6) IWMAX   -   UPPER BOUND ON IWSIZ
C
C       THE AMOUNT OF INTEGER WORK SPACE (IWORK) REQUIRED BY EACH
C       ROUTINE VARIES FROM ONE ROUTINE TO ANOTHER.  THE MAXIMUM 
C       REQUIRED IS 7*N+3.
C
C       ****************************************************************
       
      subroutine mindegree(n,XADJD,ADJD,ANZ,RHS,IFLAG,OUTUNT) 

      implicit none
C
C       -----------
C       PARAMETERS.
C       -----------

        INTEGER*4             IWMAX , LNZMAX, NMAX  , ANZMAX, SUBMAX, 
     &                      TMPMAX
crsa_dim        PARAMETER       (   NMAX        = 1824,
crsa_dim     &                      ANZMAX      = 61648,
crsa_dim     &                      SUBMAX      = MAX(ANZMAX,61648),
crsa_dim     &                      LNZMAX      = 117177,
crsa_dim     &                      TMPMAX      = 9180,
crsa_dim     &                      IWMAX       = 7*NMAX + 3    )
        PARAMETER       (   NMAX        = 2000,
     &                      ANZMAX      = 65000,
     &                      SUBMAX      = MAX(ANZMAX,61648),
     &                      LNZMAX      = 150000,
     &                      TMPMAX      = 10000,
     &                      IWMAX       = 7*NMAX + 3    )
C
C       ****************************************************************
C
C       THE FOLLOWING DECLARATIONS GIVE A ROUGH OVERVIEW OF THE 
C       LOGICAL FLOW THROUGH THE DRIVER AND THE KEY OUTPUT PRODUCED AT 
C       EACH STEP.
C
C       ****************************************************************
C
C       ---------------------------------------------
C       ADJACENCY STRUCTURE READ IN FROM MATRIX FILE.
C       ---------------------------------------------
        INTEGER*4             N     , NNZA
        INTEGER*4             XADJ(NMAX+1),
     &                      ADJ(ANZMAX)
C
C       ---------------------------------------------
C       ADJACENCY STRUCTURE WITH DIAGONAL ENTRIES AND
C       NUMERICAL VALUES ADDED (CREATE).
C       ---------------------------------------------
        INTEGER*4             XADJD(NMAX+1),
     &                      ADJD(ANZMAX+NMAX)
        real*4    ANZ(ANZMAX+NMAX)
        double precision   DB_ANZ(ANZMAX+NMAX)
C
C       -------------------------------------------------
C       SOLUTION AND ASSOCIATED RIGHT-HAND SIDE (GETRHS).
C       -------------------------------------------------
        real*4    RHS(NMAX)
        double precision    DB_nwRHS(NMAX)
C
C       ---------------------------------------
C       PERMUTATION VECTORS (ORDNAT OR ORDMMD).
C       ---------------------------------------
        INTEGER*4             PERM(NMAX),
     &                      INVP(NMAX)
C
C       ----------------------------------------------
C       SUPERNODE PARTITION AND ROW AND COLUMN NONZERO 
C       COUNTS FOR L (SFINIT).
C       ----------------------------------------------
        INTEGER*4             NNZL  , NSUB  , NSUPER
        INTEGER*4             COLCNT(NMAX), 
     &                      SNODE(NMAX),
     &                      XSUPER(NMAX+1)
C
C       ---------------------------------------------
C       COMPRESSED (SUPERNODAL) REPRESENTATION OF THE 
C       ZERO-NONZERO STRUCTURE OF L (SYMFCT).
C       ---------------------------------------------
        INTEGER*4             XLINDX(NMAX+1),
     &                      LINDX(SUBMAX),
     &                      XLNZ(NMAX+1)
C
C       ---------------------------------------------------------
C       SIZE OF FLOATING POINT WORKING STORAGE NEEDED BY BLKFCT;
C       SPLITTING OF SUPERNODES TO BETTER EXPLOIT CACHE (BFINIT).
C       ---------------------------------------------------------
        INTEGER*4             TMPSIZ
        INTEGER*4             SPLIT(NMAX)
C
C       -----------------------------------------------------------------
C       LNZ FIRST HAS SCATTERED INTO IT THE NONZERO ENTRIES OF A (INPNV);
C       THEN IT CONTAINS THE ENTRIES OF THE CHOLESKY FACTOR (BLKFCT).
C       -----------------------------------------------------------------
        double precision    LNZ(LNZMAX)
        double precision    TMPVEC(TMPMAX)
C
C       ----------------------------------------
C       GENERAL PURPOSE INTEGER WORKING STORAGE.
C       ----------------------------------------
        INTEGER*4             IWORK(IWMAX)
C
C       ------------------------------
C       MISCELLANEOUS LOCAL VARIABLES.
C       ------------------------------
        INTEGER*4             CACHSZ, I, IFLAG, IWSIZ,
     &                      LEVEL, OUTUNT
C
C       -----------------------------------
C       EXTERNAL ROUTINES PASSED TO BLKFCT.
C       -----------------------------------
        EXTERNAL            SMXPY1, SMXPY2, SMXPY4, SMXPY8
        EXTERNAL            MMPY1 , MMPY2 , MMPY4 , MMPY8
C
C       ****************************************************************
C       ****************************************************************
C
C       *****************************************************
C       COMPUTE NUMERICAL ENTRIES OF THE MATRIX;
C       COMPUTE TRUE SOLUTION AND ASSOCIATED RIGHT-HAND SIDE.
C       *****************************************************
C
C       --------------------------------------------------------
C       THE PURPOSE OF CREATE IS TO PREPARE A DATA STRUCTURE
C       FOR COMPUTING THE RIGHT-HAND SIDE FROM A GIVEN SOLUTION,
C       AND FROM WHICH IT IS VERY SIMPLE TO LOAD THE ENTRIES OF
C       A INTO L.
C
C       CREATE : ADD DIAGONAL ENTRIES TO THE STUCTURE
C                AND ADD ALL NONZERO ENTRIES, TOO.
C
C       INPUT  : N, XADJ, ADJ
C       OUTPUT : XADJD, ADJD, ANZ
C       --------------------------------------------------------


      call  con_adj( n, XADJD , ADJD , XADJ  , ADJ ,NNZA)


C           -------------------------------------------------------
C           COPY MATRIX STRUCTURE FROM (XADJ,ADJ) TO (XLINDX,LINDX)
C           (BECAUSE MATRIX STRUCTURE IS DESTROYED BY THE MINIMUM
C           DEGREE ORDERING ROUTINE).
C           -------------------------------------------------------
            DO  200  I = 1, N+1
                XLINDX(I) = XADJ(I)
  200       CONTINUE
            DO  300  I = 1, NNZA
                LINDX(I) = ADJ(I)
  300       CONTINUE
C
C           ------------------------------------------------------
C           ORDMMD : MULTIPLE MINIMUM DEGREE ORDERING.
C
C           INPUT  : N, XLINDX, LINDX, IWSIZ
C           OUTPUT : PERM, INVP, NSUB, IFLAG
C           WORK   : IWORK(4*N)
C           ------------------------------------------------------
            IWSIZ  = 4*N
            CALL  ORDMMD (  N     , XLINDX, LINDX , INVP  , PERM  ,
     &                      IWSIZ , IWORK , NSUB  , IFLAG           )
            IF  ( IFLAG .EQ. -1 )  THEN
                WRITE (OUTUNT,*) ' '
                WRITE (OUTUNT,*) '*** SIZE OF IWORK = ',
     &                           IWSIZ
                WRITE (OUTUNT,*) '*** IS LARGER THAN IWMAX = ',
     &                           IWMAX
                GO TO 1700
            ENDIF
C
C
C       --------------------------------------------------------------
C       SFINIT: SYMBOLIC FACTORIZATION INITIALIZATION.
C               COMPUTE SUPERNODE PARTITION AND STORAGE REQUIREMENTS 
C               FOR SYMBOLIC FACTORIZATION.  NEW ORDERING IS A 
C               POSTORDERING OF THE NODAL ELIMINATION TREE.
C
C       INPUT  : N, NNZA, XADJ, ADJ, PERM, INVP, IWSIZ
C       OUTPUT : PERM, INVP, COLCNT, NNZL, NSUB, NSUPER, SNODE, XSUPER,
C                IFLAG
C       WORK   : IWORK(7*N+3) [MAXIMUM REQUIRED BY ANY ROUTINE.]
C       --------------------------------------------------------------
        IWSIZ = 7 * N + 3
        CALL  SFINIT  ( N     , NNZA  , XADJ  , ADJ   , PERM  , 
     &                  INVP  , COLCNT, NNZL  , NSUB  , NSUPER, 
     &                  SNODE , XSUPER, IWSIZ , IWORK , IFLAG   )
        IF  ( IFLAG .EQ. -1 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) '*** SIZE OF IWORK = ',
     &                       IWSIZ
            WRITE (OUTUNT,*) '*** IS LARGER THAN IWMAX = ',
     &                       IWMAX
            GO TO 1700
        ENDIF
        IF  ( NNZL .GT. LNZMAX )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) '*** NUMBER OF NONZEROS IN L = ', 
     &                       NNZL
            WRITE (OUTUNT,*) '*** IS LARGER THAN LNZMAX = ', 
     &                       LNZMAX

            GO TO 1700
        ENDIF
        IF  ( NSUB .GT. SUBMAX )  THEN
            WRITE (OUTUNT,*) '*** NUMBER OF FACTOR SUBSCRIPTS = ',
     &                       NSUB
            WRITE (OUTUNT,*) '*** IS LARGER THAN SUBMAX = ',
     &                       SUBMAX
            GO TO 1700
        ENDIF
C
C       ---------------------------------------------------------
C       SYMFCT : PERFORM SUPERNODAL SYMBOLIC FACTORIZATION.
C
C       INPUT  : N, NNZA, XADJ, ADJ, PERM, INVP, COLCNT, NSUPER,
C                XSUPER, SNODE , NSUB, IWSIZ
C       OUTPUT : XLINDX, LINDX, XLNZ, IFLAG
C       WORK   : IWORK(NSUPER+2*N)
C       NO LONGER NEEDED: ADJ, COLCNT, XADJ
C       ---------------------------------------------------------
        IWSIZ = NSUPER + 2 * N + 1
        CALL  SYMFCT ( N     , NNZA  , XADJ  , ADJ   , PERM  , 
     &                 INVP  , COLCNT, NSUPER, XSUPER, SNODE ,
     &                 NSUB  , XLINDX, LINDX , XLNZ  , IWSIZ , 
     &                 IWORK , IFLAG                            )
        IF  ( IFLAG .EQ. -1 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) '*** SIZE OF IWORK = ',
     &                       IWSIZ
            WRITE (OUTUNT,*) '*** IS LARGER THAN IWMAX = ',
     &                       IWMAX
            GO TO 1700
        ENDIF
        IF  ( IFLAG .EQ. -2 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) 
     &          '*** INCONSISTENCY IN THE INPUT TO SYMFCT ***'
            GO TO 1700
        ENDIF
C
C       ----------------------------------------------------
C       INPNV : INPUT NUMERICAL VALUES INTO DATA STRUCTURES.
C
C       INPUT  : XADJD, ADJD, ANZ, PERM, INVP, NSUPER,
C                XSUPER, XLINDX, LINDX, XLNZ
C       OUTPUT : LNZ
C       WORK   : IWORK
C       ----------------------------------------------------
        call conv_a_double( n+NNZA, ANZ, DB_ANZ)

        IWSIZ = N
        CALL  INPNV  (  N     , XADJD , ADJD  , DB_ANZ   , PERM  , 
     &                  INVP  , NSUPER, XSUPER, XLINDX, LINDX , 
     &                  XLNZ  , LNZ   , IWORK                   )

c        call conv_da_double( n, DB_ANZ, ANZ)
C
C       --------------------------------------------------------
C       BFINIT : INITIALIZATION FOR BLOCK FACTORIZATION
C
C       INPUT  : N, NSUPER, XSUPER, SNODE, XLINDX, LINDX, CACHSZ
C       OUTPUT : TMPSIZ, SPLIT
C       --------------------------------------------------------
        CALL  BFINIT (  N     , NSUPER, XSUPER, SNODE , XLINDX, 
     &                  LINDX , CACHSZ, TMPSIZ, SPLIT           )
        IF  ( TMPSIZ .GT. TMPMAX )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) '*** WORKING SPACE REQUIREMENT = ', 
     &                       TMPSIZ
            WRITE (OUTUNT,*) '*** IS LARGER THAN TMPMAX = ', 
     &                       TMPMAX

            GO TO 1700
        ENDIF
C
C       -----------------------------------------------------------
C       BLKFCT : NUMERICAL FACTORIZATION.
C
C       INPUT  : NSUPER, XSUPER, SNODE, SPLIT, XLINDX, LINDX, XLNZ,
C                LNZ, IWSIZ, TMPSIZ, MMPY[LVL], SMXPY[LVL]
C       OUTPUT : LNZ, IFLAG
C       WORK   : IWORK(2*N+2*NSUPER), TMPVEC(TMPSIZ)
C       -----------------------------------------------------------
        IWSIZ = 2 * N + 2 * NSUPER
        LEVEL=8
        IF  ( LEVEL .EQ. 1 )  THEN
            CALL  BLKFCT (  N     , NSUPER, XSUPER, SNODE , SPLIT , 
     &                      XLINDX, LINDX , XLNZ  , LNZ   , IWSIZ , 
     &                      IWORK , TMPSIZ, TMPVEC, IFLAG , MMPY1 , 
     &                      SMXPY1                                  )
        ELSEIF  ( LEVEL .EQ. 2 )  THEN
            CALL  BLKFCT (  N     , NSUPER, XSUPER, SNODE , SPLIT , 
     &                      XLINDX, LINDX , XLNZ  , LNZ   , IWSIZ , 
     &                      IWORK , TMPSIZ, TMPVEC, IFLAG , MMPY2 , 
     &                      SMXPY2                                  )
        ELSEIF  ( LEVEL .EQ. 4 )  THEN
            CALL  BLKFCT (  N     , NSUPER, XSUPER, SNODE , SPLIT , 
     &                      XLINDX, LINDX , XLNZ  , LNZ   , IWSIZ , 
     &                      IWORK , TMPSIZ, TMPVEC, IFLAG , MMPY4 , 
     &                      SMXPY4                                  )
        ELSEIF  ( LEVEL .EQ. 8 )  THEN
            CALL  BLKFCT (  N     , NSUPER, XSUPER, SNODE , SPLIT , 
     &                      XLINDX, LINDX , XLNZ  , LNZ   , IWSIZ , 
     &                      IWORK , TMPSIZ, TMPVEC, IFLAG , MMPY8 , 
     &                      SMXPY8                                  )
        ENDIF
        IF  ( IFLAG .EQ. -1 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) 
     &          '*** MATRIX IS NOT POSITIVE DEFINITE ***'
            GO TO 1700
        ELSEIF ( IFLAG .EQ. -2 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) 
     &          '*** INSUFFICIENT WORK STORAGE [TMPVEC(*)] ***'
            GO TO 1700
        ELSEIF ( IFLAG .EQ. -3 )  THEN
            WRITE (OUTUNT,*) ' '
            WRITE (OUTUNT,*) '*** SIZE OF IWORK = ',
     &                       IWSIZ
            WRITE (OUTUNT,*) '*** IS LARGER THAN IWMAX = ',
     &                       IWMAX
            GO TO 1700
        ENDIF
C
C       ------------------------------------------------------
C       BLKSLV : NUMERICAL SOLUTION.
C
C       INPUT  : NSUPER, XSUPER, XLINDX, LINDX, XLNZ, LNZ, RHS
C       OUTPUT : RHS
C       ------------------------------------------------------

c        call conv_a_double( n, RHS, db_RHS)
	  do i = 1,n
	     db_nwrhs(i) = rhs(perm(i))
	  enddo

        CALL  BLKSLV (  NSUPER, XSUPER, XLINDX, LINDX , XLNZ  ,
     &                  LNZ   , db_nwRHS                          )

	  do i = 1,n
	     rhs(i) = db_nwrhs(invp(i))
	  enddo
c        call conv_da_double( n, db_RHS, RHS)

        RETURN
 1700   CONTINUE
        IFLAG=-999
        RETURN
        END

c----------------------------------------------------------------------
        subroutine con_adj( n, XADJD , ADJD , XADJ  , ADJ ,NCOUNT)
c
c  converte rappresentazione di Gustaffson di una matrice simmetrica 
c  in rapp. di adiacenza
c
c----------------------------------------------------------------------

      implicit none
      integer*4 n,XADJD(*) , ADJD(*) , XADJ(*), ADJ(*)
      integer*4 jmin,jmax,i,j,NCOUNT

      ncount=0 
      do i=1,n
       xadj(i)=XADJD(i)-i+1
       jmin= XADJD(i)
       jmax= XADJD(i+1)-1
       do j=jmin,jmax
        if (ADJD(j).ne.i) then
           ncount=ncount+1
           adj(ncount)=ADJD(j)
        endif
       enddo
      enddo
      xadj(n+1)=XADJD(n+1)-n
        

      return
      end
c----------------------------------------------------------------------
        subroutine conv_da_double( nvar, db_xvar, xvar)
c
c  converte da doppia a semplice
c
c----------------------------------------------------------------------

      implicit none
      integer*4 nvar
      double precision db_xvar(*)
      real*4 xvar(*)


      integer*4 ivar

      do ivar=1,nvar
c        xvar = sngl(db_xvar)
        xvar(ivar) = db_xvar(ivar)
      enddo
        

      return
      end
c----------------------------------------------------------------------
        subroutine conv_a_double( nvar, xvar, db_xvar)
c
c  converte da semplice a doppia
c
c----------------------------------------------------------------------

      implicit none
      integer*4 nvar
      real*4 xvar(*)
      double precision db_xvar(*)


      integer*4 ivar

      do ivar=1,nvar
c        db_xvar = dble(xvar) 
        db_xvar(ivar) = xvar(ivar) 
      enddo
        

      return
      end
