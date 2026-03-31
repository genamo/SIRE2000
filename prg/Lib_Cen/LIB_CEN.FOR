c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Subroutine generiche necessarie alle centrali (ZERO_FUN1,
C     TRANS)
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!======================================================================
        SUBROUTINE ZERO_FUN1(A,B,D,FA,FB,X,ERR,EPS,FUN)
C============================================================================
C
C.....CALCOLA UNO ZERO  X  DELLA FUNZIONE  FUN  NELL'INTERVALLO  (A,B)
C     CON UNA PRECISIONE   6*EPS*ABS(X) + ERR ,  EPS  PRECISIONE DI MACC
C.....I PARAMETRI DI CHIAMATA SONO:
C         A,B      ESTREMI DELL'INTERVALLO CONTENENTE LO ZERO
C         ERR      PRECISIONE RICHIESTA (SU X)
C         FUN      NOME DELLA FUNZIONE DI CUI SI VUOLE LO ZERO; FUN DEVE
C                  ESSERE DICHIARATO EXTERNAL NEL PROGRAMMA CHIAMANTE
C          X       ZERO TROVATO
C-------------------------------------------------------------------------
*&*
        implicit none
*&*
        real*4    SA,SB,A,B
        real*4    FA,FB,FC,FD,NF,FUN
        real*4    C,D,E,P,Q,R,S,X
        real*4    PROD,TOL,EPS,ERR,RM
*&*
	SA=A
	SB=B
	IF(ABS(FA).GT.1.E-5) GO TO 5
	X=A
	RETURN
    5	IF(ABS(FB).gt.1.E-5) goto 15
	x = b
	RETURN
   15	NF = 0
	IF((D.LE.A).OR.(D.GE.B)) D = A
	IF(D.EQ.A) GO TO 10
	FD=FUN(D)
	NF=NF+1
	PROD=FA*FD
	IF (PROD.GT.0) THEN
	      SA=SB
	      FA=FB
	      SB=D
	      FB=FD
	      GOTO 10
	ELSE
	      C=SA
	      FC=FA
	      SA=SB
	      FA=FB
	      SB=D
	      FB=FD
	      GOTO 20
	ENDIF
10	C=SA
	FC=FA
	E=SB-SA
	D=E
20	IF( ABS(FC).GE.ABS(FB) ) GO TO 30
	    SA=SB
	    SB=C
	    C=SA
	    FA=FB
	    FB=FC
	    FC=FA
30	TOL=2.*EPS*ABS(SB) + ERR
	RM=0.5*(C-SB)
C.....CONTROLLO SULLA PRECISIONE
	IF( (ABS(RM).LE.TOL) .OR. (FB.EQ.0.) )GO TO 140
C.....SCELTA DEL METODO
	IF( (ABS(E).GE.TOL) .AND. (ABS(FA).GT.ABS(FB)) ) GO TO 40
C.....METODO DI BISEZIONE
	    E=RM
	    D=E
	    GOTO 100
40	S=FB/FA
	IF(SA.NE.C) GOTO 50
C.....INTERPOLAZIONE LINEARE
	    P=2.0*RM*S
	    Q=1.0-S
	    GOTO 60
C.....INTERPOLAZIONE QUADRATICA INVERSA
50	Q=FA/FC
	R=FB/FC
	P=S*(2.0*RM*Q*(Q-R)-(SB-SA)*(R-1.))
	Q=(Q-1.0)*(R-1.0)*(S-1.0)
60	IF (P.LE.0)  GOTO 70
	    Q=-Q
	    GO TO 80
70	P=-P
80	S=E
	E=D
	IF((2.*P.GE.3.*RM*Q-ABS(TOL*Q)).OR.
     *		(P.GE.ABS(.5*S*Q))) GO TO 90
	    D=P/Q
	    GOTO 100
90	E=RM
	D=E
100	SA=SB
	FA=FB
	IF( ABS(D).LE.TOL ) GO TO 110
	SB=SB+D
	GOTO 130
  110	IF(RM.LE.0.) GOTO 120
	SB=SB+TOL
	GOTO 130
  120	SB=SB-TOL
  130	X = SB
	FB=FUN(X)
	IF(ABS(FB).LT.1.E-5) RETURN

	NF=NF+1
	IF(NF.GT.50)  RETURN

	IF ((FB.GT.0.).AND.(FC.GT.0.)) GOTO 10
	IF ((FB.LE.0.).AND.(FC.LE.0.)) GO TO 10
	        GOTO 20
C.....ZERO TROVATO
  140	X=SB

	RETURN
	END

!======================================================================
	SUBROUTINE TRANS(POUT,TIN,H,QM,P,Q,NP,istaz,vert_pointer)
c***********************************************************************
c	trasforma NP punti del grafico (QM,H) nei corrispondenti
c	punti del grafico (Q,PIN), a pressione di uscita e temperatura
c       di ingresso assegnate.	
c   *** USA GLI Z_COEF PER IL CALCOLO DI Z
c------------------------------------------------------------------
        implicit none
*&*
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk_param.inc'
*&*
c
	REAL*4    H(*),QM(*),P(*),Q(*)
	REAL*4    eps,base,ppout,pout,ttin,tin,pmin,plim,err,hh,f1,f2,
     *            p3,pin,z,qqm,qq
        integer*4 istaz  ! puntatore alla centrale nella struttura SIRE
        integer*4 vert_pointer ! puntatore alla centrale nella struttura centrali
        integer*4 i,np,iistaz,vvert_pointer
        real*4    uno_su_ro_attuale,aux3
c
	COMMON /AUXTR/  hh,ppout,ttin,iistaz,vvert_pointer
	real*4   FUNTR, prop
	EXTERNAL FUNTR, prop
c---------------------------------------------------------------------
cgpe        call zeroma(eps,base)
        call zerom(eps,base)
	ppout = pout
	ttin  = tin
        iistaz = istaz
        vvert_pointer=vert_pointer
	do i = 1,np
		pmin = .333*pout
		plim = .999*pout
		err  = pout*1.E-4
		hh  = h(i)
                f1 = funtr(pmin)
                f2 = funtr(plim)
                p3 = pmin
                call zero_fun(pmin,plim,p3,f1,f2,pin,err,eps,funtr)
		P(i) =   pin
cmar                z = prop(pin,tin,z_coef(1,vert_pointer))
	        call ZPT1(istaz,pin,tin,z)
		qqm    =  qm(i)/QMRIF
                uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tin/pin
                aux3 = ROCN*uno_su_ro_attuale
                qq = qqm / aux3                
		Q(i)   =  qq*QRIF
	end do
	return
	end

!======================================================================
	REAL*4 FUNCTION  FUNTR(pin)
c***********************************************************************
c	funzione da azzerare per il calcolo della pressione
c	di ingresso a fissata pressione di uscita e altezza adiabatica
c   *** USA GLI Z_COEF PER IL CALCOLO DI Z
c---------------------------------------------------------------------
*&*
        implicit none
*&*
        include '../inc/param.inc'
        include '../inc/stazione.inc'
        include '../inc/rk.inc'
        include '../inc/th.inc'
        include '../inc/cc.inc'
c
        integer*4 istaz  ! puntatore alla centrale nella struttura SIRE
        integer*4 vert_pointer ! puntatore alla centrale nella struttura centrali
        real*4    pin,z

        real*4    ex,aux1,aux2,h1
        real*4    prop
        external  prop
        real*4    h,pout,tin
CMAR
       real*4 comp_ratio
	integer*4 ier

	COMMON /AUXTR/ h,pout,tin,istaz,vert_pointer
c-------------------------------------------------------------------
c---->                     calcolo esponente della politropica
cgpe        ex = cesad(istaz)

CMAR
      comp_ratio = pout/pin
	call politrop_esp(istaz,pin,tin,comp_ratio,ex,ier)

CMAR        ex = prop(pin,tin,exp_coef(1,istaz))
cgpe-end
        eX = (ex-1.)/ex


CMAR
CMAR        z = prop(pin,tin,z_coef(1,vert_pointer))
c
        call ZPT1(istaz,pin,tin,z)
c---->                  calcolo altezza adiabatica
        aux1 = (erre / agrvcc / pmol) * Z * tin/ex
        aux2 = (pout/pin)**ex
        h1   = aux1 * (aux2-1.)                        ! in km
	FUNTR = h - h1
	return
	end