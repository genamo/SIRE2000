      subroutine c_coef_turb(nom_tair,nom_power,nom_hrate,
     *	                   ncorr,temp,fpw,fhr,
     *                       nmaxp,rev_pwmax,pwmax,airpwm,
     *                       nhrate,rev_hrate,power,airhrt,hrate,
     *                       cpw,cpwt,chr,chrt,jer,error_msg)
c**************************************************************************
c     Calcola per regressione i coefficienti delle curve caratteristiche di
c     una turbina dopo aver adimensionalizzato i dati di input.
c     Produce inoltre un messaggio che descrive l'esito del calcolo.
c     (Elena Colonna)
c**************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------------->Parametri della subroutine
c------------------->Input
c
      real*4 nom_tair,         !valore nominale della temperatura dell'aria
     *     nom_power,        !valore nominale della potenza
     *	 nom_hrate         !valore nominale dell'heat-rate
c
c           Dati per la regressione dei coefficienti correttivi
c
      integer*4 ncorr       !punti assegnati  
                            !grandezze relative a tali punti:
      real*4 temp(*),     !temperatura;
     *     fpw(*),      !fattore di correzione per la potenza fpw=f(temp)
     *     fhr(*)       !fattore di correzione per l'heat rate fhr=f(temp)
	                      
c
c           Dati per la regressione dei coefficienti CPW   
c
      integer*4 nmaxp        !punti assegnati
                             !grandezze relative a tali punti:
	real*4 rev_pwmax(*), !numero di giri; 
     *     pwmax(*),     !potenza massima
     *     airpwm(*)     !temperatura dell'aria
c
c           Dati per la regressione dei coefficienti CHR
c
      integer*4 nhrate        !punti assegnati
                              !grandezze relative a tali punti:
	real*4 rev_hrate(*), !numero di giri; 
     *     power(*),     !potenza massima;
     *     airhrt(*),    !temperatura dell'aria;
     *     hrate(*)      !heat rate

c------------------->Output
      real*4 cpw(3),      !coefficienti per il calcolo della potenza massima:
	                  !pw(n)=cpw(1)+cpw(2)*n+cpw(3)*n^2
     *     cpwt(4),     !coefficienti per il calcolo del fattore correttivo
	                  !per la potenza:
	                  !fpw(t)=cpwt(1)+cpwt(2)*t+cpwt(3)*t^2+cpwt(4)*t^3
     *     chr(6),      !coefficienti per il calcolo dell'heat rate:
	                  !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
	                  !         chr(5)*n^2+chr(6)*pw^2
     *     chrt(4)      !coefficienti per il calcolo del fattore correttivo 
	                  !per l'heat rate:
	                  !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
c
      character*(*) error_msg  !messaggio che descrive l'esito della regressione
                             !e gli eventuali errori commessi
c   
      integer*4 jer          !indice d'errore
c------------------------------->Variabili locali
      integer*4 ii,jj,kk

c---------------------------------------------------------------------------------
c     calcolo dei coefficienti:
c     CASO1:ncorr>0
c         si usano i punti assegnati per la regressione dei coefficienti correttivi 
c         e poi si usano questi ultimi per determinare i cpw e i chr;per i calcoli
c         si utilizza la subroutine regres.
c     CASO2:ncorr=0
c         non sono assegnati punti per la regressione dei coefficienti correttivi;
c         in questo caso si procede come segue:
c         - si usa la regres per calcolare un primo valore di tentativo dei cpw
c           e chr;
c         - si assegna un valore iniziale ai cpwt e ai chrt
c         - si usa la subroutine levmarq per la regressione
c*********************************************************************************
c     Inizializzo il messaggio di errore
c
      error_msg=''
c
c     Adimensionalizzazione degli input
c 
c_temp	nom_tair=(nom_tair+t0)/taux
	nom_tair=nom_tair/taux
	nom_power=nom_power/powerif
	nom_hrate=nom_hrate/hraterif
c
      if (ncorr.gt.0) then
	   do ii=1,ncorr
c_temp	      temp(ii)=(temp(ii)+t0)/taux
	      temp(ii)=temp(ii)/taux
c-old
c            fpw(ii)=fpw(ii)/percrif
c	       fhr(ii)=fhr(ii)/percrif
c-old
            fpw(ii)=fpw(ii)
	      fhr(ii)=fhr(ii)
	   end do
	end if
c
      do jj=1,nmaxp
c-old	   rev_pwmax(jj)=rev_pwmax(jj)/percrif
	   rev_pwmax(jj)=rev_pwmax(jj)
c-old	   pwmax(jj)=pwmax(jj)*nom_power/percrif
	   pwmax(jj)=pwmax(jj)*nom_power
c_temp         airpwm(jj)=(airpwm(jj)+t0)/taux
         airpwm(jj)=airpwm(jj)/taux
      end do
c
      do kk=1,nhrate
c-old	   rev_hrate(kk)=rev_hrate(kk)/percrif
	   rev_hrate(kk)=rev_hrate(kk)
c-old	   power(kk)=power(kk)*nom_power/percrif
	   power(kk)=power(kk)*nom_power
c_temp	   airhrt(kk)=(airhrt(kk)+t0)/taux
	   airhrt(kk)=airhrt(kk)/taux
c-old	   hrate(kk)=hrate(kk)*nom_hrate/percrif
	   hrate(kk)=hrate(kk)*nom_hrate
	end do
c
c     Calcolo per regressione i coefficienti delle curve caratteristiche della
c     turbina analizzata
c 
	call coef_turb(nom_tair,ncorr,temp,fpw,fhr,
     *	           nmaxp,rev_pwmax,pwmax,airpwm,
     *               nhrate,rev_hrate,power,airhrt,hrate,
     *               cpw,cpwt,chr,chrt,jer)
c
c     Eseguo la dimensionalizzazione degli output
	nom_tair=nom_tair*taux
	nom_power=nom_power*powerif
	nom_hrate=nom_hrate*hraterif
      if (ncorr.gt.0) then
	   do ii=1,ncorr
	      temp(ii)=temp(ii)*taux
	   end do
	end if
      do jj=1,nmaxp
	   pwmax(jj)=pwmax(jj)/nom_power
         airpwm(jj)=airpwm(jj)*taux
      end do
      do kk=1,nhrate
	   power(kk)=power(kk)/nom_power
	   airhrt(kk)=airhrt(kk)*taux
	   hrate(kk)=hrate(kk)/nom_hrate
	end do
c

c     Controllo l'esito del calcolo
c 
      if (jer.eq.0) then
c	   nom_tair=nom_tair*taux-t0
	   error_msg='Calcolo dei coefficienti eseguito correttamente'
	else
         error:select case(jer)
	      case(1110)
	         error_msg='Matrice singolare:i punti introdotti per la'//
     *             ' regressione dei coefficienti della curva di'//
     *             ' correzione della massima potenza danno origine'//
     *             ' ad un sistema di equazioni linearmente dipendenti.'  
	      case(1210)
	         error_msg='Matrice singolare:i punti introdotti per la'//
     *             ' regressione dei coefficienti della curva di'//
     *             ' correzione dello heat rate danno origine'//
     *             ' ad un sistema di equazioni linearmente dipendenti.'  
	      case(1310)
	         error_msg='Matrice singolare:i punti introdotti per la'//
     *             ' regressione dei coefficienti della curva di'//
     *             ' massima potenza danno origine'//
     *             ' ad un sistema di equazioni linearmente dipendenti.'  
	      case(1410)
	         error_msg='Matrice singolare:i punti introdotti per la'//
     *             ' regressione dei coefficienti delle curve di'//
     *             ' heat rate danno origine'//
     *             ' ad un sistema di equazioni linearmente dipendenti.'  
	      case(1111)
	         error_msg='I punti introdotti non sono sufficienti per'//
     *             ' la regressione dei coefficienti della curva di'//
     *             ' correzione della massima potenza.' 
	      case(1211)
	         error_msg='I punti introdotti non sono sufficienti per'//
     *             ' la regressione dei coefficienti della curva di'//
     *             ' correzione dello heat rate.' 
	      case(1311)
	         error_msg='I punti introdotti non sono sufficienti per'//
     *             ' la regressione dei coefficienti della curva di'//
     *             ' massima potenza.' 
	      case(1411)
	         error_msg='I punti introdotti non sono sufficienti per'//
     *             ' la regressione dei coefficienti della curva di'//
     *             ' heat rate.' 
	      case(121)
	         error_msg='Non sono stati assegnati punti per il'// 
     *	         ' calcolo dei coefficienti della curva di massima'//
     *             ' potenza.'
	      case(122)
	         error_msg='Non sono stati assegnati punti per il'// 
     *	         ' calcolo dei coefficienti delle curve di hrate.'
         end select error
	end if
      return
      end subroutine
c============================================================================
	 subroutine Coef_turb(nom_tair,ncorr,temp,fpw,fhr,nmaxp,
     *                     rev_pwmax,pwmax,
     *                     airpwm,nhrate,rev_hrate,power,airhrt,hrate,
     *                     CPW,CPWT,CHR,CHRT,jer)
c***********************************************************************
c       Computes the coefficients for the turbine as functions
c       of the assigned points.
c------------------------------------------------------------------------------
        implicit none
        include '../inc/param.inc'
        include '../inc/lvmar.inc'
C------------------------------------------------------------------------------
c------------------>ARGOMENTI DELLA SUBROUTINE
C
	  REAL*4 nom_tair          ! valore nominale della temperatura dell'aria
c
	  integer*4 ncorr        ! numero di punti usati per definire i fattori di 
	                         ! di correzione
c
	  REAL*4 temp(*)           ! temperatura dell'aria
     *      ,fpw(*)            ! fattore di correzione per la potenza massima
	                         !     fpw=fpw(temp)
     *	  ,fhr(*)            ! fattore di correzione per l'heat rate
c                              !     fhr=fhr(temp)

        integer*4 nmaxp        ! punti a disposizione per il calcolo dei 
	                         !    coefficienti CPW          
c
                               ! grandezze relative ai punti osservati a disposizione:
        REAL*4 rev_pwmax(*)  ! numero di giri alla potenza massima; 
     *      ,pwmax(*)      ! potenza massima;
     *	  ,airpwm(*)     ! potenza massima alla temperatura dell'aria.
c
        integer*4 nhrate       ! punti a disposizione per il calcolo dei 
	                         !    coefficienti CHR
                               
	                         ! grandezze relative ai punti osservati a disposizione:
	  REAL*4 rev_hrate(*) ! numero di giri; 
     *      ,power(*)     ! potenza;
     *      ,airhrt(*)    ! heat rate alla temperatura dell'aria;
     *      ,hrate(*)     ! heat rate;
c
        REAL*4 CPW(3)            ! coeff. per il calcolo della potenza massima
	                         !     pw=pw(n)
     *	  ,CPWT(4)           ! coeff. correttivi per la potenza massima
                               !     calcolati
     *	  ,CHR(6)            ! coeff. per il calcolo dell'heat rate:
	                         !     hr=hr(n,PW)
     *	  ,CHRT(4)           ! coeff. correttivi per l'heat rate calcolati
c
     	  integer*4 jer          ! error index (O)(vedi subroutine chiamante)
c
c-elena        CHARACTER row*(*)      ! descrive l'errore che provoca un eventuale 
c-elena                               ! uscita dalla subroutine (O)
c-------------------------------------------------------------------------------
c------------------->VARIABILI LOCALI
C
        integer*4 ilen         ! variabile locale che registra la lunghezza 
	                         !    della stringa turb
     *           ,i            ! variabile locale usata come indice nei cicli
      
					         ! argomenti della sobroutine regres(...)

        integer*4 nv           ! numero di variabili indipendenti
	                         !    (f=f(x(i)) i=1,..nv)   (I)      
     *           ,nvmax        ! numero massimo di variabili indipendenti(I)
     *           ,ip(20)       ! indice di presenza dei termini del polinomio(I)

        REAL*4 X(3,MAX_POINT)    ! matrice variabili indipendenti (I)
     *      ,F(MAX_POINT)      ! vettore variabile dipendente (I)
     *	  ,C(20)             ! coefficienti del polinomio calcolati (O)
     *      ,sum               ! somma del quadrato degli scarti (O)
c      -------------------------------------------------------------------------
        real*4 corrf             ! variabile locale che registra i valori dei 
	                         !    fattori correttivi per la temperatura          
c
                               ! argomenti della subroutine levmarq(...):
        integer*4 neq          ! numero di equazioni 
     *           ,ninco        ! numero di incognite
c
	  EXTERNAL  resmp
     *	       ,jacmp
     *		   ,reshr
     * 		   ,jachr
C
     	  real*4      tol
     *           ,eps          ! machine precision

        DATA eps/1.e-6/,tol/1.e-3/
	  
	  integer*4 maxfn

        DATA maxfn/50/
  
        REAL*4	parm(3)	

        DATA parm/1.E-6,2,1000./
 
        integer*4    idmax
        PARAMETER   (idmax=30*MAX_POINT+70)
        
	  REAL*4    WK(idmax)
        
	  integer*4 IWK(idmax)
     *           ,idm1
     *		   ,idm2
     *           ,infer
     *           ,ieval
	  integer*4 ier
c------------------------------------------------------------------------------
      jer=0
c-elena      row=' '
c
	if(nmaxp.le.0) then
	   jer=121
	   return
	end if
c
	if(nhrate.le.0) then
	   jer=122
	   return
	end if
c
      if (ncorr.ne.0) then
c---->               The corrective factors' points are given
c---->               Computation of CPWT 
	   nv=1   
	   nvmax=3
	   ip(1)=1
	   ip(2)=1
	   ip(3)=1
	   ip(4)=1
         do i=1,ncorr
	     X(1,i)=temp(i)
	     F(i)=fpw(i)
	   end do
         call regres(nv,nvmax,ncorr,ip,x,f,c,sum,ier)
         if (ier.gt.0) then
c-eelna             row='calcolo della correzione di max_power.'
	       if (ier.eq.10) then
                 jer=1110
	       else if (jer.eq.11) then
	           jer=1111
		   end if
            return
         endif
	   CPWT(1)=C(1)
	   CPWT(2)=C(2)
	   CPWT(3)=C(3)
	   CPWT(4)=C(4)
c---->               Computation of the CHRT   
	   nv=1   
	   nvmax=3
	   ip(1)=1     
	   ip(2)=1     
	   ip(3)=1     
	   ip(4)=1
	   do i=1,ncorr
	     X(1,i)=temp(i)
	     F(i)=fhr(i)
	   end do
         call regres (nv,nvmax,ncorr,ip,X,F,C,sum,ier) 
         if (ier.gt.0) then
c-elena             row='calcolo della correzione di heat_rate.'
	       if (ier.eq.10) then
                 jer=1210
	       else if (ier.eq.11) then
	           jer=1211
		   end if
            return
         endif
	   CHRT(1)=C(1)
	   CHRT(2)=C(2)
	   CHRT(3)=C(3)
	   CHRT(4)=C(4)
c---->               Determination of the coefficients CPW    
	   nv=1 !rev_pwmax  
	   nvmax=3
	   ip(1)=1
	   ip(2)=1
	   ip(3)=1
         ip(4)=0

	   do i=1,nmaxp
            corrf=CPWT(1)+CPWT(2)*airpwm(i)+CPWT(3)*airpwm(i)**2+
     *          CPWT(4)*airpwm(i)**3    ! fattore correttivo per la potenza max
	     X(1,i)=rev_pwmax(i)
	     F(i)=pwmax(i)/corrf          ! valore osservato della potenza massima 
	                                  ! alla temperatura dell'aria
	   end do
         call regres (nv,nvmax,nmaxp,ip,X,F,C,sum,ier) 
         if (ier.gt.0) then
c-elena             row='calcolo dei coefficienti di max_power.'
	       if (ier.eq.10) then
                 jer=1310
	       else if (ier.eq.11) then
	           jer=1311
		   end if
            return
         endif
	   CPW(1)=C(1)
	   CPW(2)=C(2)
	   CPW(3)=C(3)
c---->             Determination of the coefficients CHR   
	   nv=2      ! rev_hrate,power
	   nvmax=3
	   do i=1,6
	     IP(i)=1
         end do
         do i=7,20
            IP(i)=0
         end do
	   do i=1,Nhrate
            corrf=CHRT(1)+CHRT(2)*airhrt(i)+CHRT(3)*airhrt(i)**2+
     *             CHRT(4)*airhrt(i)**3  !coefficiente correttivo per l'hrate
	      X(1,i)=rev_hrate(i)
	      X(2,i)=power(i)
	      F(i)=hrate(i)/corrf          ! valore osservato dell'hrate alla 
		                               ! temperatura dell'aria 
	   end do
         call regres (nv,nvmax,nhrate,IP,X,F,C,SUM,ier) 
         if (ier.gt.0) then
c-elena             row='calcolo dei coefficienti di heat_rate.'
	       if (ier.eq.10) then
                 jer=1410
	       else if (ier.eq.11) then
	           jer=1411
		   end if
             return
         endif
	   do i=1,6
	     CHR(i)=C(i)
         end do
      else   !---------------------------------------------------------------
c---->            computation of the first attempt values for the LEVMARQ
	   CPWT(1)=1.
	   CPWT(2)=0.
	   CPWT(3)=0.
	   CPWT(4)=0.
	   nv=1   
	   nvmax=3
	   ip(1)=1
	   ip(2)=1
	   ip(3)=1
         ip(4)=0
c        do i=5,20
c           ip(i)=0
c        end do
	   do i=1,nmaxp
	     X(1,i)=rev_pwmax(i)
	     F(i)=pwmax(i)
	   end do
         call regres(nv,nvmax,nmaxp,ip,X,F,C,sum,ier) 
         if (ier.gt.0) then
c-elena            row='calcolo dei primi valori di tentativo dei cpw.'
	      if (ier.eq.10) then
                 jer=1310
	      else if (ier.eq.11) then
	           jer=1311
		  end if
            return
         endif
	   CPW(1)=C(1)
	   CPW(2)=C(2)
	   CPW(3)=C(3)
c
	   CHRT(1)=1.
	   CHRT(2)=0.
	   CHRT(3)=0.
	   CHRT(4)=0.
	   nv=2   
	   nvmax=3
	   do i=1,6
	     IP(i)=1
         end do
         do i=7,20
           IP(i)=0
         end do
	   do i=1,Nhrate
	     X(1,i)=rev_hrate(i)
	     X(2,i)=power(i)
	     F(i)=hrate(i)
	   end do
         call regres (nv,nvmax,nhrate,ip,X,F,C,sum,ier) 
         if (ier.gt.0) then
c-elena            row='calcolo dei primi valori di tentativo dei chr.'
	      if (ier.eq.10) then
                jer=1410
	      else if (ier.eq.11) then
	          jer=1411
		  end if
            return
         endif
	   do i=1,6
	     CHR(i)=C(i)
	   end do
c
         do 77 i=1,nmaxp
           pow_lm(i)=pwmax(i)
           tem_lm(i)=airpwm(i)
           rev_lm(i)=rev_pwmax(i)
77       continue
         nomt_lm=nom_tair
         neq=nmaxp+1
         ninco=7         
         idm1=neq*ninco
         idm2=idm1
         C(1)=CPWT(1)
         C(2)=CPWT(2)
         C(3)=CPWT(3)
         C(4)=CPWT(4)
         C(5)=CPW(1)
         C(6)=CPW(2)
         
	   C(7)=CPW(3)
         call LEVMARQ (neq,ninco,resmp,jacmp,tol,eps,maxfn,
     *                   parm,C,F,WK,IWK,INFER,IEVAL,
     *			 ier,IDM1,IDM2,IDMAX)
        if (ier.gt.0) then
c          jer=jer*10+1
c          row='Calcolo dei coefficienti di max_power.'
           jer=1310
          return
        endif
        CPWT(1)=C(1)
        CPWT(2)=C(2)
        CPWT(3)=C(3)
        CPWT(4)=C(4)
        CPW(1)=C(5)
        CPW(2)=C(6)
        CPW(3)=C(7)
c
        do 88 i=1,nhrate
          pow_lm(i)=power(i)
          tem_lm(i)=airhrt(i)              
          rev_lm(i)=rev_hrate(i)
          hrat_lm(i)=hrate(i)
88      continue
        neq=nhrate+1
        ninco=10      
        idm1=neq*ninco
        idm2=idm1
        C(1)=CHRT(1)
        C(2)=CHRT(2)
        C(3)=CHRT(3)
        C(4)=CHRT(4) 
        C(5)=CHR(1)
        C(6)=CHR(2)
        C(7)=CHR(3)
        C(8)=CHR(4)
        C(9)=CHR(5)
        C(10)=CHR(6)
        call LEVMARQ (neq,ninco,reshr,jachr,tol,eps,maxfn,
     *                   parm,C,F,WK,IWK,INFER,IEVAL,
     *			 ier,IDM1,IDM2,IDMAX)
        if (ier.gt.0) then
c          jer=jer*10+1
c          row='Calcolo dei coefficienti di heat_rate.'
           jer=1410
          return
        endif
        CHRT(1)=C(1)
        CHRT(2)=C(2)
        CHRT(3)=C(3)
        CHRT(4)=C(4)
        CHR(1)=C(5)
        CHR(2)=C(6)
        CHR(3)=C(7)
        CHR(4)=C(8)
        CHR(5)=C(9)
        CHR(6)=C(10)
      endif
c-----------------------------------------------------------------------------
      return
      end

c------------------------------------------------------------------------
	Subroutine jacmp(neq,ninco,temp,first_jac,jac_entry,
     *		           jac_value,idm,ier)
c
c***********************************************************************
c	computes the Jacobian matrix for maximum power
c----------------------------------------------------------------------
	include '../inc/param.inc'
c
	include '../inc/lvmar.inc'
c
        integer*4 first_jac(*),jac_entry(*)
	REAL*4    temp(*),jac_value(*)
        integer*4 neq,ninco
        integer*4 ier,j,i,idm
c---------------------------------------------------------------------
        ier=0
        j = 0
        do 10 i=1,neq-1
          j = j + 1
          first_jac(i)=j
          jac_entry(j)=1
          jac_value(j)=temp(5)+temp(6)*rev_lm(i)+
     *                   temp(7)*rev_lm(i)**2
          j = j + 1
          jac_entry(j)=2
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=3
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=4
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=5
          jac_value(j)=temp(1)+temp(2)*tem_lm(i)+
     *                   temp(3)*tem_lm(i)**2+
     *                   temp(4)*tem_lm(i)**3
          j = j + 1
          jac_entry(j)=6
          jac_value(j)=jac_value(j-1)*rev_lm(i)
          j = j + 1
          jac_entry(j)=7
          jac_value(j)=jac_value(j-1)*rev_lm(i)
10      continue
        j = j + 1
        first_jac(neq)=j
        jac_entry(j)=1
        jac_value(j)=1.
        j = j + 1
        jac_entry(j)=2
        jac_value(j)=nomt_lm
        j = j + 1
        jac_entry(j)=3
        jac_value(j)=jac_value(j-1)*nomt_lm
        j = j + 1
        jac_entry(j)=4
        jac_value(j)=jac_value(j-1)*nomt_lm
        first_jac(neq+1)=j+1
c
        if (j.gt.idm) ier=20
c
	return
	end

c------------------------------------------------------------------------
	Subroutine resmp(neq,ninco,temp,res)
c
c***********************************************************************
c	Computesa the residuals for maximum power
c----------------------------------------------------------------------
	include '../inc/param.inc'
c
	include '../inc/lvmar.inc'
c
        integer*4   neq,ninco
	REAL*4  temp(*),res(*)
        integer*4   i
c---------------------------------------------------------------------
        do 10 i=1,neq-1
          res(i)=(temp(1)+temp(2)*tem_lm(i)+
     *           temp(3)*tem_lm(i)**2+temp(4)*tem_lm(i)**3)*
     *           (temp(5)+temp(6)*rev_lm(i)+temp(7)*rev_lm(i)**2)
     *           -pow_lm(i)
10      continue
        res(neq)=(temp(1)+temp(2)*nomt_lm+
     *           temp(3)*nomt_lm**2+temp(4)*nomt_lm**3)-1
	return
	end
c------------------------------------------------------------------------
	Subroutine jachr(neq,ninco,temp,first_jac,jac_entry,
     *		           jac_value,idm,ier)
c
c***********************************************************************
c	computes the Jacobian matrix for heat rate
c----------------------------------------------------------------------
	include '../inc/param.inc'
c
	include '../inc/lvmar.inc'
c
        integer*4 first_jac(*),jac_entry(*)
	REAL*4    temp(*),jac_value(*)
        integer*4 neq,ninco
        integer*4 ier,j,i,idm
c---------------------------------------------------------------------
        ier=0
        j = 0
        do 10 i=1,neq-1
          j = j + 1
          first_jac(i)=j
          jac_entry(j)=1
          jac_value(j)=temp(5)+temp(6)*rev_lm(i)+temp(7)*pow_lm(i)+
     *            temp(8)*rev_lm(i)*pow_lm(i)+temp(9)*rev_lm(i)**2+
     *            temp(10)*pow_lm(i)**2
          j = j + 1
          jac_entry(j)=2
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=3
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=4
          jac_value(j)=jac_value(j-1)*tem_lm(i)
          j = j + 1
          jac_entry(j)=5
          jac_value(j)=temp(1)+temp(2)*tem_lm(i)+
     *                   temp(3)*tem_lm(i)**2+
     *                   temp(4)*tem_lm(i)**3
          j = j + 1
          jac_entry(j)=6
          jac_value(j)=jac_value(j-1)*rev_lm(i)
          j = j + 1
          jac_entry(j)=7
          jac_value(j)=jac_value(j-2)*pow_lm(i)
          j = j + 1
          jac_entry(j)=8
          jac_value(j)=jac_value(j-1)*rev_lm(i)
          j = j + 1
          jac_entry(j)=9
          jac_value(j)=jac_value(j-3)*rev_lm(i)
          j = j + 1
          jac_entry(j)=10
          jac_value(j)=jac_value(j-3)*pow_lm(i)
10      continue
        j = j + 1
        first_jac(neq)=j
        jac_entry(j)=1
        jac_value(j)=1.
        j = j + 1
        jac_entry(j)=2
        jac_value(j)=nomt_lm
        j = j + 1
        jac_entry(j)=3
        jac_value(j)=jac_value(j-1)*nomt_lm
        j = j + 1
        jac_entry(j)=4
        jac_value(j)=jac_value(j-1)*nomt_lm
        first_jac(neq+1)=j+1
c
        if (j.gt.idm) ier=20
c
	return
	end

c------------------------------------------------------------------------
	Subroutine reshr(neq,ninco,temp,res)
c
c***********************************************************************
c	Computes the residuals for heat rate
c----------------------------------------------------------------------
	include '../inc/param.inc'
c
	include '../inc/lvmar.inc'
c
        integer*4   neq,ninco
	REAL*4  temp(*),res(*)
        integer*4   i
c---------------------------------------------------------------------
        do 10 i=1,neq-1
          res(i)=(temp(1)+temp(2)*tem_lm(i)+
     *           temp(3)*tem_lm(i)**2+temp(4)*tem_lm(i)**3)*
     *           (temp(5)+temp(6)*rev_lm(i)+temp(7)*pow_lm(i)+
     *            temp(8)*rev_lm(i)*pow_lm(i)+temp(9)*rev_lm(i)**2+
     *            temp(10)*pow_lm(i)**2) - hrat_lm(i)
10      continue
        res(neq)=(temp(1)+temp(2)*nomt_lm+
     *           temp(3)*nomt_lm**2+temp(4)*nomt_lm**3)-1
	return
	end
