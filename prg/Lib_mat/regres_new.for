c......................................................................
      subroutine  regres(nv,nvmax,npt,ip,x,f,c,sum,ier)
      implicit none
c
c     this routine extimates the coefficients of a third degree polynom
c     in at most three indipendent variables
c
c              for three variables
c     c(1)+c(2)*x(1)+c(3)*x(2)+c(4)*x(3)+c(5)*x(1)*x(2)+c(6)*x(1)*x(3)+
c       c(7)*x(2)*x(3)+c(8)*x(1)**2+c(9)*x(2)**2+c(10)*x(3)**2+
c       c(11)*x(1)*x(2)**2+c(12)*x(1)*x(3)**2+c(13)*x(2)*x(3)**2+
c       c(14)*x(1)**2*x(2)+c(15)*x(1)**2*x(3)+c(16)*x(2)**2*x(3)+
c       c(17)*x(1)*x(2)*x(3)+c(18)*x(1)**3+c(19)*x(2)**3+c(20)*x(3)**3
c
c
c              for one variable
c     c(1)+c(2)*x(1)+c(3)*x(1)**2+c(4)*x(1)**3
c
c     the logical vector ip defines which coefficients must be           
c     considered zero and which not.                                   
c----------------------------------------------------------------------
c      nv=numero di variabili indipendenti ( f=f( x(i) ) i=1,..nv) 
c      nvmax=numero max variabili indip. ( dimensione nella routine chiamante)
c      npt=numero di punti (x(i),y)                                        
c      ip=indice di presenza dei termini del polinomio
c        (in caso di assenza di un termine la componente corrispondente 
c         di ip assume valore 0)                
c      x(i,j)=matrice variabili indipendenti (i=1,..,nv;j=1,..,npt)                            
c      f(j)=vettore variabile dipendente                              
c      c(k)=coefficienti del polinomio calcolati                     
c      sum=somma del quadrato degli scarti      
c      ier=index error                          
c--------------------------------------------------------------------------
c      ier can assume the value:
c      10    singular matrix
c      11    not enough points
c--------------------------------------------------------------------------
c                PARAMETRI FORMALI DELLA SUBROUTINE

      integer*4 nv       !(I)
     *	     ,nvmax    !(I)
     *	     ,npt      !(I) 
     *	     ,ip(*)    !(I)
c
      real x(nvmax,npt)  !(I)
     *	,f(npt)        !(I)
     *	,c(20)         !(O)
     *	,sum           !(O)
c
c-ace per permettere la doppia precisione
c
      real*8 x_8(3,200)  !     (si suppone npt<=200)
     *	,f_8(200)      !     (si suppone npt<=200)
     *	,c_8(20)         
     *    ,sum_8           
c
      integer*4 ier      !(O)
c
c                PARAMETRI LOCALI
c
      integer*4 i      
     *	     ,j      !
     *	     ,l      ! 
     *	     ,id     !
     *	     ,nv1    ! indici usati nei cicli 
     *	     ,i1     !
     *	     ,k      !
c
      integer*4 n      ! conta il numero di elementi non nulli del vettore ip,
	                 !       ossia il numero di coefficienti da determinare 
	                 !       mediante regressione       
c 
      real*8 a(2000)     !registra i valori osservati delle variabili
	                 !   indipendenti e dei loro prodotti se il corrispondente 
	                 !   coefficiente è diverso da zero
c
	real*8 y(200)      !valori osservati variabile dipendente
	                 !     (si suppone npt<=200)
 
c                Parametri formali della subroutine leastq(...) 

      integer*4 ipv(20),kr
      real*8 b(20),s(20),sm(20)
c--------------------------------------------------------------------------
      ier=0
      do j=1,npt
	  f_8(j)=DBLE(f(j))
        y(j)=f_8(j)
      end do
	do i= 1,nv
	  do j=1,npt
	    x_8(i,j)=DBLE(x(i,j))
	  end do
	end do
c                                                                      
      n = 0
      l = 0
      id = 1
c---->   conta il numero dei coefficienti da calcolare esaminando ip
c---->                        coefficienti di grado 0
      if(ip(id).ne.0) then
        n = n + 1
        do j = 1,npt
          a(j) = 1.
        end do
        l = l + npt
      end if
c---->                        coefficienti di grado 1
      do i = 1,nv
        id = id + 1
        if(ip(id).ne.0) then
          n = n + 1
          do  j = 1,npt
            a(l+j) = x_8(i,j)
	  end do
          l = l + npt
        end if
      end do
      if (nv.gt.1) then
c---->                        coefficienti misti
        nv1 = nv- 1
        do i = 1,nv1
          i1 = i + 1
          do  k = i1,nv
            id = id + 1
            if ( ip(id).ne.0) then
              n = n + 1
              do j =1,npt
                a(l+j) = x_8(i,j) * x_8(k,j)
	      end do
              l = l + npt
            end if
          end do
        end do
      end if
c---->                       coefficienti di secondo grado
      do i = 1,nv
        id = id + 1
        if (ip(id).ne.0) then
          n = n + 1
          do j = 1,npt
            a(l+j) = x_8(i,j) **2
          end do
          l = l + npt
        end if
      end do
      if (nv.gt.1) then
c---->                       coefficienti misti di terzo grado
        nv1 = nv- 1
        do i = 1,nv1
          i1 = i + 1
          do  k = i1,nv
            id = id + 1
            if ( ip(id).ne.0) then
              n = n + 1
              do j =1,npt
                a(l+j) = x_8(i,j) * x_8(k,j)**2
	      end do
              l = l + npt
            end if
          end do
        end do
        do i = 1,nv1
          i1 = i + 1
          do  k = i1,nv
            id = id + 1
            if ( ip(id).ne.0) then
              n = n + 1
              do j =1,npt
                a(l+j) = x_8(i,j)**2 * x_8(k,j)
	      end do
              l = l + npt
            end if
          end do
        end do
        if (nv.eq.3) then
          id = id + 1
          if (ip(id).ne.0) then
            n = n + 1
            do j =1,npt
              a(l+j) = x_8(1,j) * x_8(2,j)* x_8(3,j)
            end do
            l = l + npt
          endif
        endif
      end if
c---->                       coefficienti di terzo grado
      do i = 1,nv
        id = id + 1
        if (ip(id).ne.0) then
          n = n + 1
          do j = 1,npt
            a(l+j) = x_8(i,j) **3
          end do
          l = l + npt
        end if
      end do
c
      if (npt.lt.n) then
        ier = 11
        return
      endif                          
      call  leastq(npt,n,a,y,b,s,c_8,sm,sum_8,ipv,kr)
	do i=1,n
	  c(i)= SNGL(c_8(i))
	  sum = SNGL(sum_8)
	end do
      if (kr.ne.n) ier=10
      return
      end
c-----------------------------------------------------------------
      subroutine leastq(m,n,a,y,b,s,c,sm,sum,ipv,kr)
c  **************************************************************               
c     this routine solves the least squares problem              
c                   minimize //y-a*c//**2                        
c     using the q r factorization by means of hauseholder        
c     ortonormal transformation                                  
c  **************************************************************                                                              
      implicit none                                                         
      integer*4 n,m,kr,n1,k,i,iv,i1,j,kv
      integer*4 ipv(n)
      real*8 a(m,n),y(m),b(n),s(n),c(n),sm(n)
c-ace occorre permettere alla routine householder di lavorare in real*8
c                Parametri formali della subroutine zerom(...)      
	real*8 eps         !precisione della macchina
     * 	    ,base        !base di numerazione
	real*8 ald,aux,sum
c  **************************************************************                                                               
      call zerom_r8(eps,base)
      call househ(m,n,a,b,s,sm,eps,ipv,kr)
      if (kr.ne.n) return
      n = kr
      n1 = n + 1
      do  20 k = 1,n
        ald = 0.
        do 10 i = k,m
          ald = ald + a(i,k) * y(i)
   10     continue
        ald = b(k) *ald
        do 20 i =k,m
          y(i) = y(i) - ald*a(i,k)
   20     continue
      do 30 i = 1,n
        c(i) = y(i)
        y (i) = 0.
   30   continue
      c(n) = c(n) / s(n)
      do 50 iv =2,n
        i = n - iv + 1
        i1 = i + 1
        aux = 0
        do 40 j = i1,n
          aux = aux + a(i,j) *c(j)
   40     continue
        c(i) =(c(i)-aux) / s(i)
   50   continue
      do 55 i = 1,n
        do 53 k =i,n
          if (i.eq.ipv(k)) go to 54
   53     continue
   54   if (k.eq.i) go to 55
        ipv(k) = ipv(i)
        aux = c(i)
        c(i) = c(k)
        c(k)      = aux
   55   continue
      sum = 0.
      if(m.le.n) return
      do 60 i =n1,m
        sum = sum + y(i) *y(i)
   60   continue
      do 80 kv = 1,n
        k = n - kv + 1
        ald = 0.
        do 70 i =k,m
          ald = ald  + a(i,k)*y(i)
   70     continue
        ald = b(k) * ald
        do 80 i = k,m
          y(i) = y(i) - ald *a(i,k)
   80     continue
      return
      end
c-----------------------------------------------------------------------
      subroutine  househ(m,n,a,b,s,summa,eps,ip,kr)
      implicit none
c                                                                    
c.....this routine reduces a matrix a (m*n) with m.ge.n to a         
c     superior triangular form r  using hauseholder  transformation  
c                                t                                   
c                 p = i - b * u u                                    
c                                                                    
c     the non zero elements of u(k) vectors are memorized in         
c     place of the corresponding terms of a as the non zero terms of 
c     triangular matrix r.                                           
c     only the diagonal elements r(k,k) are memorized in  avector s  
c     as the same of elements b(k)                                   
c                                                                    
c 
c-ace la routine lavora in doppia precisione                                                                   
      integer*4 n,m,kr,k,i,kpiv,l,jp,k1,j
      real*8      eps,tol,smax,sa,ald,as
      integer*4 ip(n)
      real*8 a(m,n),b(n),s(n),summa(n)
      real*8 ss,sk
c                                                                    
      kr=0
      ss =0.
      do 2 k =1,n
        ip(k)=k
        summa(k)=0.
        do 1 i=1,m
      if (eps.gt.abs(a(i,k))) go to 1
          summa(k)=summa(k) + a(i,k)*a(i,k)
    1     continue
        ss =  ss  + summa(k)
    2   continue
      sk = ss
      tol= eps *dsqrt(ss)
      do 100 k =1,n
        smax = 0.
        kpiv = k
        do 3 l =k,n
          if (smax.ge.summa(l)) go to 3
          smax = summa(l)
          kpiv = l
    3     continue
        if (kpiv.eq.k) go to 5
        jp = ip(k)
        ip(k) = ip(kpiv)
        ip(kpiv) = jp
        sa = summa(k)
        summa(k) = summa(kpiv)
        summa(kpiv) = sa
        do 4 i=1,m
          sa = a(i,k)
          a(i,k) = a(i,kpiv)
          a(i,kpiv) = sa
    4     continue
    5   sk = sk - summa(k)
        sa = 0.
        ald = 0.
        do 10 i = k,m
          as = abs(a(i,k))
          sa = sa + as
          if (as.lt.ald) go to 10
          ald = as
   10     continue
        if (ald.lt.tol) return
        kr = kr + 1
c_ace        if ((sa - abs(a(k,k))).gt.eps) go to 30
        if ((sa - abs(a(k,k))).gt.tol) go to 30
        b(k) = 0.
        s(k) = a(k,k)
        go to 100
   30   s(k) = 0.
        do 40 i = k,m
          a(i,k) = a(i,k) /ald
          s(k) = s(k) + a(i,k) * a(i,k)
   40     continue
        s(k) = sqrt(s(k))
        if (a(k,k).lt.0) s(k) = -s(k)
        a(k,k) = a(k,k) + s(k)
        b(k) = 1. / (s(k)*a(k,k))
        s(k) = -ald*s(k)
        if (k.eq.n) go to 110
        k1 = k + 1
        do 90 j = k1,n
          ald = 0.
          do 50 i =k,m
            ald = ald + a(i,k) * a(i,j)
   50       continue
          ald = b(k) * ald
          do  80 i = k,m
            a(i,j) = a(i,j) - ald * a(i,k)
   80       continue
          sa = a(k,j)*a(k,j)
          summa(j) =summa(j) - sa
          sk = sk - sa
   90 continue
  100     continue
  110 return
      end
!======================================================================
      SUBROUTINE ZERO_FUN(A,B,D,FA,FB,X,ERR,EPS,FUN)
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
      subroutine zerom(eps,base)
!======================================================================
        implicit none
c.....computes the machine precision eps as the greatest number that 
c..... float(1.+eps)= 1.                                             
c.....base is the base of the numeration                             
c
cgpe-prv per i moduli che utilizzano la zerom come function eps = zerom()
cgpe-prv è da sostituire tale istruzione con la chiamata a questa routine
      real*4 uno,eps,acc,base,ops
      real*8 eps8, base8
c                                                                    
      uno = 1.
      eps = uno
    1 eps = eps/2.
      acc= uno + eps
      if (acc-uno)2,2,1
    2 eps = eps*2.
      base=uno
    3 base=base+uno
      acc=base+eps
      if(acc-base) 4,4,3
    4 if(base-2.) 10,10,11
   11 eps=uno
    5 eps=eps/base
      acc=uno+eps
      if(acc-uno)6,6,5
    6 ops=eps
      eps=eps*base
      acc=uno+eps
      acc=acc-ops
      if (acc-uno)10,10,8
    8 eps=eps/2.
   10 continue
C per una maggiore precisione viene fatto il calcolo come se si dovesse lavorare su real*8
      eps8 = eps
	call zerom_r8 (eps8,base8)
	eps = eps8
      return
      end

      subroutine zerom_r8(eps,base)
!======================================================================
        implicit none
c.....computes the machine precision eps as the greatest number that 
c..... float(1.+eps)= 1.                                             
c.....base is the base of the numeration                             
c
cgpe-prv per i moduli che utilizzano la zerom come function eps = zerom()
cgpe-prv è da sostituire tale istruzione con la chiamata a questa routine
      real*8 uno,eps,acc,base,ops
c                                                                    
      uno = 1.
      eps = uno
    1 eps = eps/2.
      acc= uno + eps
      if (acc-uno)2,2,1
    2 eps = eps*2.
      base=uno
    3 base=base+uno
      acc=base+eps
      if(acc-base) 4,4,3
    4 if(base-2.) 10,10,11
   11 eps=uno
    5 eps=eps/base
      acc=uno+eps
      if(acc-uno)6,6,5
    6 ops=eps
      eps=eps*base
      acc=uno+eps
      acc=acc-ops
      if (acc-uno)10,10,8
    8 eps=eps/2.
   10 continue
      return
      end

