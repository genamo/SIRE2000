      Subroutine c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                    	chlim_h,n_char,rev,flow,head,eff,nlim,
     *                        lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                        alim,blim,clim,ier,err_msg,iwr)
***************************************************************************
c     Calcola per regressione i coefficienti delle curve caratteristiche di 
c     un compressore dopo aver adimensionalizzato i dati di input.
c     Produce inoltre un messaggio che descrive l'esito del calcolo
c     (Elena Colonna)
**************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c----------------------------->Parametri della subroutine
c----------------------->Input
c
	real*4 nom_rev       !valore nominale del numero di giri
c
      real*4 max_rev,      !numero di giri massimo
     *     min_rev       !numero di giri minimo
c
      real*4 chlim_q,      !portata del punto limite di choking
     *     chlim_h       !prevalenza del punto limite di choking
c
      integer*4 n_char     !punti assegnati per effettuare la regressione
c                   
                         !grandezze relative ai punti assegnati:
      real*4 rev(*),       !numero di giri (dim=n_char)
     *     flow(*),      !portata (dim=n_char)
     *     head(*),      !altezza adiabatica (dim=n_char)
     *     eff(*)        !efficienza (dim=n_char)
c
      integer*4 nlim       !punti limite assegnati per definire le spezzate 
	                   !di surge
c----------------------->Input/Output  
c
c     caso 1- se nlim>0 le spezzate di surge vengono definite in input ----->
c             lim_n,lim_q sono input
c     caso 2- se nlim=0 le spezzate di surge non vengono definite in input e quindi 
c             le definisco da programma------>lim_n,lim_q sono output
c
                         !grandezze relative ai punti limite assegnati:
      real*4 lim_n(*),     !numero di giri(dim=nlim)
     *	 lim_q(*)      !portata (dim=nlim)
c----------------------->Output
      real*4 chlim_n       !numero di giri corrispondente al pto limite di choking 
c 
      real*4 lim_h(*)      !altezza adiabatica dei punti limite
c
c                        !coefficienti per calcolare:
      real*4 chn(*),       !altezza adiabatica nella zona normale (dim=6)
     *     chc(*),       !altezza adiabatica nella zona di choking (dim=6)
     *     cen(*)        !efficienza nella zona normale (dim=5)
c
                         !coefficienti delle rette che descrivono la spezzata di 
					   !intervento anti pompaggio:
      real*4 alim(*),      !termine noto (dim=nlim+1)
     *     blim(*)       !coefficiente angolare(dim=nlim+1)
c
      real*4 clim(*)       !limiti anti-choke (dim=4)
	                   !zona di choking per l'efficienza:
					   !clim(1):limite relativo dal punto assegnato in input
	                   !clim(2):limite assoluto dal punto estremo 
	                   !zona di choking per il numero di giri:
					   !clim(3):limite relativo dal punto assegnato in input
	                   !clim(4):limite assoluto dal punto estremo 
c
      character err_msg*(*) !messaggio che descrive l'esito della regressione e
	                     !gli eventuali errori commessi
	integer*4 ier          !codice d'errore
c-ele 30/8/2006
	integer*4 iwr          !codice di warning
c
c----------------------->Variabili locali
      integer*4 ii,jj      !indici nei cicli
c     Argomenti da passare alla subroutine coef_compr
c	integer flag
c      character mess*(80)
c--------------------------------------------------------------------------------
c*************************************************************************
c
c     Inizializzo il messaggio di errore
c
      err_msg=''
c
c     Adimensionalizzazione dei dati di input
c
c-old
c      max_rev=max_rev/percrif
c	min_rev=min_rev/percrif
c-old
c --->mi aspetto in input una percentuale normalizzata (numero da 0 a 1) 
c
	chlim_q=chlim_q/qmrif/1000. !mi aspetto m^3/h
	chlim_h=chlim_h/headrif/1000. !mi aspetto m
c
	do ii=1,n_char
         rev(ii)=rev(ii)/nom_rev   !ottengo una percentuale normalizzata
	   flow(ii)=flow(ii)/qmrif/1000.
	   if (head(ii).eq.0) then
	      head(ii)=undef
	   else 
	      head(ii)=head(ii)/headrif/1000.
	   end if
	   if (eff(ii).eq.0) eff(ii)=undef
	end do
c	
      if(nlim.gt.0)then
	   do jj=1,nlim
	      lim_n(jj)=lim_n(jj)/nom_rev !ottengo una % normalizzata
	      lim_q(jj)=lim_q(jj)/qmrif/1000.
	   end do
	end if
c	
c     Calcolo per regressione i coefficienti delle curve caratteristiche del
c     compressore analizzato
c
c Elena C. - 12 gennaio 2006- Nuovo trattamento dei punti limite di un compressore
	call COEF_COMPR(nom_rev,max_rev,min_rev,nlim,lim_n,lim_q,lim_h,
     *                        n_char,rev,flow,head,eff,chn,chc,cen,
     *                        alim,blim,clim,chlim_n,chlim_q,chlim_h,
     *                        ier,iwr)
c12-------------------------------------------------------
c 
c     eseguo la dimensionalizzazione degli output
      do ii=1,n_char
        rev(ii)=rev(ii)*nom_rev   !ottengo una percentuale normalizzata
	  flow(ii)=flow(ii)*qmrif*1000.
	  if (head(ii).eq.undef) then
	     head(ii)=0.
	  else 
	     head(ii)=head(ii)*headrif*1000.
	  end if
	  if (eff(ii).eq.undef) eff(ii)=0.	   
	end do
c-prov0/2
	chlim_q=chlim_q*qmrif*1000.   !restituisco m^3/h
	chlim_h=chlim_h*headrif*1000. !restituisco m
	chlim_n=chlim_n*nom_rev       !restituisco rpm
	do jj=1,nlim
	   lim_n(jj)=lim_n(jj)*nom_rev   !restituisco rpm
	   lim_q(jj)=lim_q(jj)*qmrif*1000. !restituisco km^3/h
	   lim_h(jj)=lim_h(jj)*headrif*1000. !restituisco m
      end do

      if (ier.eq.0) then
c         max_rev=max_rev*percrif
c	   min_rev=min_rev*percrif
c-prova 20/2
	   err_msg='Regressione dei coefficienti eseguita correttamente.'

	else
         error:select case(ier)
	      case(110)
	         err_msg='Matrice singolare:i punti introdotti per la'//
     *                 ' regressione dei coefficienti delle curve'//
     *                 ' a numero di giri costante danno origine'//
     *                 ' ad un sistema di equazioni linarmente'//
     *                 ' dipendenti.'
	      case(210)
	         err_msg='Matrice singolare:i punti introdotti per la'//
     *                 ' regressione dei coefficienti delle curve'//
     *                 ' a numero di giri costante - zona di choking-'//
     *                 ' danno origine ad un sistema di equazioni'//
     *                 ' linarmente dipendenti.'
	      case(310)
	         err_msg='Matrice singolare:i punti introdotti per la'//
     *                 ' regressione dei coefficienti delle curve'//
     *                 ' ad efficienza costante danno origine'//
     *                 ' ad un sistema di equazioni linarmente'//
     *                 ' dipendenti.'
	      case(111)
	       err_msg='I punti introdotti non sono sufficienti per la'//
     *               ' regressione dei coefficienti delle curve a'//
     *               ' numero di giri costante.'
c-elena
	      case(112)
	       err_msg='I punti introdotti cadono tutti a destra della '//
     *               'curva di choking relativo.'//
     *               'Impossibile descrivere il compressore nella '//
     *               ' zona di funzionamento normale.'
c-elena
	      case(211)
	       err_msg='I punti introdotti non sono sufficienti per la'//
     *               ' regressione dei coefficienti delle curve a'//
     *               ' numero di giri costante -zona di choking- .'
c-elena
	      case(212)
	       err_msg='I punti introdotti cadono tutti a sinistra '//
     *               'della curva di choking relativo.'//
     *               'Impossibile descrivere il compressore nella '//
     *               'zona di choking.'
c-elena
	      case(311)
	       err_msg='I punti introdotti non sono sufficienti per la'//
     *               ' regressione dei coefficienti delle curve ad'//
     *               ' efficienza costante.'
	      case(21)
	       err_msg='Non sono stati inseriti punti per la'//
     *			   ' regressione dei coefficienti delle curve'//
     *               ' caratteristiche.'
	      case(31)
	       err_msg='Un punto descrittivo ha portata negativa o nulla.'
	      case(32)
	       err_msg='Un punto descrittivo ha il numero di giri'//
     *               ' negativo o nullo.'
	      case(33)
	       err_msg='Un punto descrittivo ha prevalenza e efficienza'//
     *               '  entrambi nulli.'
	      case(41)
	       err_msg='Curva limite di surge definita in modo non'//
     *               ' corretto:un segmento della spezzata ha'//
     *               ' pendenza negativa.'
c-elena-410
	      case(410)
	       err_msg='Definizione non corretta della zona di choking.'//
     *               'I punti introdotti non permettono di descrivere'//
     *               ' l''efficienza secondo il modello inserito.' 
c-elena-410
c-elena-510
	      case(510)
	       err_msg='Definizione non corretta della zona di choking.'//
     *               'Il punto limite di choking inserito cade '//
     *               ' a destra della curva di choking assoluto.'
c-elena-510
         end select error
      end if
      
      return
	end subroutine


c****************************************************************************************

	SUBROUTINE COEF_COMPR(nom_rev,max_rev,min_rev,nlim,lim_n,lim_q,lim_h,
     *                        n_char,rev,flow,head,eff,chn,chc,cen,
     *                        alim,blim,clim,chlim_n,chlim_q,chlim_h,
     *                        ier,iwr)
c******************************************************************************
c  Versione con nuova gestione dei punti limite
c******************************************************************************
c       Computes the coefficients for the compressor characteristic curves 
c       as functions of the assigned points
c**************************************************************************
        implicit none
        include '../inc/param.inc'  !Definisce alcune costanti e le massime 
	                         !grandezze dei vettori
        include '../inc/rk_param.inc'!Contiene le grandezze di riferimento e
	                         !i coefficienti per la trasformazione in
	                         !unità M.K.S
C-----------------------------------------------------------------------

c----------->ARGOMENTI DELLA SUBROUTINE

        real*4 max_rev        ! numero di giri massimo (I)
     *      ,min_rev        ! numero di giri minimo (I)
     *      ,nom_rev

        integer*4 nlim      ! numero di punti usati per definire la 
	                      ! curva di pompaggio teorica
	                      ! (spezzata di intervento antipompaggio)
c
                            ! Grandezze relative ai punti di lavoro sulla 
	                      ! spezzata antipompaggio: 
        real*4 lim_n(*)       ! numero di giri(I/O)
     *	  ,lim_q(*)       ! portata(I/O)
     *      ,lim_h(*)       ! altezza adiabatica(I/O)
c
        integer*4 n_char    ! numero di punti a disposizione per la 
	                      !   regressione(I)
                           
                            ! Grandezze relative ai punti di lavoro osservati:
        real*4 rev(n_char)    ! numero di giri(I),
     *	  ,flow(n_char)   ! portata(I),
     *	  ,head(n_char)   ! altezza adiabatica(I),
     *	  ,eff(n_char)    ! efficienza(I)
c
	  real*4 chn(6)         ! coefficienti per il calcolo di H=f(n,Q) per la 
     *	                  !      zona normale (O);
     *	  ,chc(6)         ! coefficienti per il calcolo di H=f(n,Q) per la
                            !      zona di choking (O);  
     *	  ,cen(5)         ! coefficienti per il calcolo di eff=f(n,Q) per
     *                      !      la zona normale (O);  
     *	  ,alim(*)        ! coefficienti delle rette che descrivono la 
     *	  ,blim(*)        !      spezzata di intervento antipompaggio
     *
     *
     *	                  ! Grandezze relative alla zone di choking per 
     *                      ! l'efficienza:    
     *      ,clim(*)        ! clim(1)=valore del rapporto (H/n) sulla curva di 
     *                      !      choking;
     *                      ! climi(2)=limite anti-choke assoluto (massimo 
     *                      !      rapporto (H/n) fra quelli assegnati).
     *                      ! Grandezze relative alla zona di choking per i giri:   
     *		   			  ! clim(3)=costante della curva di choking;
     *	                  ! clim(4)=limite anti-choke assoluto (massimo
     *                      !      rapporto H/(Q**2) fra quelli assegnati.  
     *
     *	  ,chlim_n        ! numero di giri al limite della zona di choking(O)
     *	  ,chlim_q        ! portata al limite della zona di choking(I)
     *	  ,chlim_h        ! altezza adiabatica al limite della zona di 
     *                      !      choking(I)
     *                      ! 
      integer*4            ier       ! error index(O)
c-ele30/8/2006
      integer*4            iwr       !O codice di  warning:
	              !                0)nessun warning informativo da segnalare
	              !                100)presenza di punti limite oltre la curva di
	              !                    surge teorica e quindi ricalcolati dal programma
	              !                101)regressione vincolata
	              !                201)punti limite oltre la curva+regressione vincolata! 
     
c----------------------------------------------------------------------------------

c------------>VARIABILI LOCALI
                                          
                            ! argomenti dell subroutine regres(...):

	  integer*4 nv        ! numero di variabili indipendenti
     *                      !        (f=f(x(i)) i=1,..nv)	                             
     *	       ,nvmax     ! numero max di variabili indipendenti 
     *		   ,npoint    ! numero di punti (x(i),f(x(i))) 
     *		   ,ip(20)    ! indice di presenza dei termini del polinomio   
	  
	  REAL*4 x(3,MAX_POINT) ! matrice variabili indipendenti 
     *	  ,f(MAX_POINT)   ! vettore variabile dipendente 
     *	  ,c(20)          ! coefficienti del polinomio calcolati 
     *      ,sum            ! somma del quadrato degli scarti 

c	  
	  integer*4 marker(MAX_POINT) ! indica per ogni punto assegnato se
     *                              ! altezza adiabatica ed efficienza sono
     *                              ! ben definiti
c
	  logical*2   check(6)  ! controlla i coefficienti calcolati mediante 
	                      ! regressione
c

        integer*4 inp       ! variabile locale
     * 
     * 		   ,i         !
     *           ,ii        !
     *		   ,i1        ! variabili locali usate come indici nei cicli
     *           ,i2        !
     *           ,ik        !
c
        integer*4 ilim      ! indice del punto di lavoro col più piccolo valore 
     *	                  ! H/(Q**2)
c
        real*4      aux       ! 
     *           ,caux      ! variabile locali usate per la memorizzazione
     *	       ,qql       !        temporanea del risultato di alcuni calcoli 
     *           ,dg        !
c     
        REAL*4      EPS_Q/0.001/,MIN_Q    ! costanti usate per definire i valori 
                                        ! limite sulla spezzata di surge  	   
                                   
                                        ! Grandezze relative ai punti di lavoro nella 
	                                  ! zona di choking:
        real*4      flow_cho(max_point)   ! portata
     *           ,rev_cho(max_point)    ! numero di giri 
     *           ,head_cho(max_point)   ! altezza adiabatica

                                        ! Grandezze relative a tre punti di lavoro sulla
									  ! curva di choking: 
        real*4      q_condi(3)            ! portata
     *	       ,h_condi(3)            ! altezza adiabatica
     *	       ,n_condi(3)            ! numero giri
c  
	  
	  real*4      qc1,qc2,qc3           !
     *	       ,nc1,nc2,nc3           !
     *		   ,hc1,hc2,hc3           !
     *		   ,der1,der2             ! Parametri usati per la determinazione 
     *		   ,dn                    !     dei coefficienti CHC
     *	   	   ,a1,a2,a3,a4,a6        ! 
     *		   ,b1,b2,b3,b4,b6        ! 
     *		   ,c1,c3                 !
      
	 integer*4 jer
c-elena
       integer*4 rev_da,rev_min,ncurve,irange,nlim_tot,max
	 real*4    alimite(3),blimite(3),
     *           nlimite(2),qlimite(2),hlimite(2)

c-elena23
	real*4 qn(12),qc(12),hn(12),hc(12),der_1(12),der_2(12)
     
c*************************************************************************************


      ier=0
	iwr=0                  !inizializzo il codice di warning
c     
      do i=1,20
        ip(i)=0
      end do
      do i=1,6
        chn(i)=0             !inizializzazione dei coefficienti da calcolare
        chc(i)=0
      end do
      do i=1,5
        cen(i)=0
      end do
c
      if (n_char.le.0) then
         ier=21
	   return
	end if
                            
      do i=1,n_char
        marker(i) = 0
c
c la portata e il numero di giri devono essere positivi per ogni punto di lavoro osservato
       if (flow(i).le.0) then
         ier=31
	    return
       end if
c
       if (rev(i).le.0) then
         ier=32
	    return
       end if
c      
c altezza adiabatica ed efficienza devono essere entrambi ben definiti
c                                             (confronto con undef)
c
        if (head(i).eq.undef.and.eff(i).eq.undef)then
         ier=33                                 
          return
        else if (head(i).eq.undef) then
          marker(i)=marker(i)-2
        else if (eff(i).eq.undef) then
          marker(i)=marker(i)+2
        endif
      end do

c
c calcolo del limite anti-choke assoluto dal punto piu' estremo
c
      clim(4)=head(1)/(flow(1)**2)
      do i=2,n_char
        aux=head(i)/(flow(i)**2)
        if (aux.lt.clim(4).and.head(i).gt.0) then
          ilim = i
          clim(4)=aux
        endif
      end do
c
c calcolo del coefficiente della parabola limite fra le due zone 
c (se definite)
c
      if (chlim_q.gt.0) then
        clim(3)=chlim_h/(chlim_q**2)
      else
        clim(3)=clim(4)
      endif
      if (clim(3).lt.clim(4)) then
C i limiti non sono ben definiti ==> non definisco una zona di choking
        clim(3)=clim(4)
c-elena-510
        ier=510   !   !!!Definizione non corretta della zona di choking.Il punto
        return    !   limite di choking inserito cade a destra della curva  
c-elena-510       !   di choking assoluto !!!!
      endif
c---->            determinazione dei coefficienti CHN per regressione
c                 (zona normale)
      nv=2   
      nvmax=3
      do i=1,6
        ip(i)=1
      end do
c      ip(7)=0   
      do i=7,10
        ip(i)=0
      end do
c              
c                !conteggio dei punti validi per la regressione e
c                !memorizzazione dei valori osservati in appositi
c                !array da passare alla subroutine regres(...) 
c            
      npoint=0
      do i=1,n_char
        if (marker(i) .eq. 0 .or. marker(i).eq.2) then
          caux=head(i)/(flow(i)*flow(i))
          if (caux.ge.clim(3)) then
            npoint=npoint+1
            X(1,npoint)=flow(i)
            X(2,npoint)=rev(i)
            F(npoint)=head(i)
          endif
        endif
      end do
      if (npoint.eq.0) then    
c-elena: definisco in modo più esplicativo la situazione di errore
c         ier = 111
         ier = 112 !!!!I punti caratteristici inseriti cadono tutti a destra 
         return    !   della curva di choking relativo. Impossibile descrivere  
      endif        !   il compressore nella zona di funzionamento normale 
      inp=npoint
      call  regres(nv,nvmax,npoint,ip,x,f,c,sum,jer) 
      if (jer.gt.0) then
	  if(jer.eq.10)then
	    ier = 110
	  else if (jer.eq.11) then
	    ier = 111
	  end if
        return
      endif
      do i=1,6
        check(i)=.true.
      enddo
777   ii=0
               !eventuale correzione del valore della variabile dipendente 
	         !   e ulteriore calcolo dei coefficienti 
      if(check(5).and.C(5).gt.0. ) then
        chn(5) = -0.01
        ip(5)= 0
        do i=1,inp
          F(i)=F(i)-chn(5)*X(1,i)**2
        end do
        check(5) = .false.
        ii = ii+1
      end if
      if(check(6).and.C(6).lt.0.) then
        chn(6) = 0.01
        ip(6)= 0
        do i=1,inp
          F(i)=F(i)-chn(6)*X(2,i)**2
        end do
        check(6) = .false.
        ii = ii+1
      end if
      if(check(4).and.C(4).lt.0.) then
        chn(4) = 0.01
        IP(4)= 0
        do i=1,inp
          F(i)=F(i)-chn(4)*X(2,i)*X(1,i)
        end do
        check(4) = .false.
        ii = ii+1
      end if
      if(ii.gt.0) then
        call regres(nv,nvmax,inp,ip,x,f,c,sum,jer) 
c-ele 14/9 inserito indice di warning per segnalare che la regressione è stata vincolata
        iwr=101
        goto 777
      end if
      do i=1,6
        if(check(i)) then
          chn(i)=C(i)
        endif
      end do
c
c determinazione, se richiesto, dei coefficienti chc
c (zona di choking)
c
      if (clim(3).ne.clim(4) ) then
        npoint = 0
        do i=1,n_char
          if (marker(i) .eq. 0 .or. marker(i).eq.2) then
            caux=head(i)/(flow(i)*flow(i))
            if (caux.lt.clim(3)) then
              npoint=npoint+1
              flow_cho(npoint)=flow(i)
              rev_cho(npoint)=rev(i)
              head_cho(npoint)=head(i)
            endif
          endif
        end do
        if (npoint.eq.0) then
c-elena          ier = 211
          ier = 212 !se clim4 è diverso da clim3 questa situazione non dovrebbe
          return    !mai verificarsi perchè a destra della curva di choking rel
        endif       !c'è almeno il punto da cui si è calcolato clim4
cc
c------>calcolo i CHC chiamando la leastq per il calcolo del solo coefficiente chc(5):
c       i rimanenti coefficienti si calcolano a partire da esso sfruttando le relazioni 
c       lineari ottenute imponendo la continuità delle curve a giri costanti in 3 punti
c       (max_rev,min_rev,(max_rev-min_rev)/2) e la continuità delle deivate di H rispetto a Q
c       sulla curva di choking ai giri costanti graficati

      call calcola_chc_derivate(max_rev,min_rev,clim,npoint,
     *                       rev_cho,flow_cho,head_cho,chn,
     *                       chc,jer)


        if (jer.gt.0) then
	      ier = 210
	      return
	  end if
     

c------>il coefficiente chc(5) definisce la concavità della curva e deve essere negativo
        if (chc(5).gt.0) then
          do i=1,6
            chc(i)=chn(i)
          enddo
	  end if

      else
        do i=1,6
           chc(i)=chn(i)
        enddo
      endif
c
      if (clim(3).NE.clim(4)) then
c e' definita una zona di choking per i giri, quindi posso definirla anche
c per l'efficienza:
c     calcolo anzitutto il chlim_n
        call NDAHQ_a(chlim_h,chlim_q,chlim_n,chn)
c calcolo del limite anti-choke relativo dal punto assegnato in input
        clim(1)=chlim_q/chlim_n
c calcolo del limite anti-choke assoluto dal punto estremo come massimo 
c rapporto Q/n fra quelli assegnati
        clim(2)=0.
        do i=1,n_char
          if (rev(i).gt.0) then
            aux=flow(i)/rev(i)
            if (aux.gt.clim(2)) then
              ilim = i
              clim(2)=aux
            endif
          endif
        end do
        IF (CLIM(2).LE.CLIM(1)) THEN
C I LIMITI PER L'EFFICIENZA NON SONO BEN DEFINITI ==> NON DEFINISCO UNA zona
C DI CHOKING
          clim(1)=0
          clim(2)=0
          clim(3)=clim(4)
          CHLIM_N=0
          CHLIM_Q=0
          CHLIM_H=0
c-elena-410
          ier=410
          return
c-elena-410
        ENDIF
      else
        clim(1)=0
        clim(2)=0
      endif
c---->                Determination of the CEN coefficients by regression
c                     (normal zone)
c---->        Check if the working_points agree with the limit curve values
c             ( for the @@@ points)
      NV=2   
      NVMAX=3
      IP(1)=1
      IP(2)=1
      IP(3)=1
      IP(4)=0
      IP(5)=1
      IP(6)=1
      IP(7)=0
      IP(8)=0
      IP(9)=0
      IP(10)=0
      npoint=0
      do i=1,n_char
        if (marker(i) .eq. 0 .or. marker(i).eq.-2) then
          npoint=npoint+1
          X(1,npoint)=flow(i)/rev(i)
          X(2,npoint)=rev(i)
          F(npoint)=eff(i)
        endif
      end do
      if (npoint.eq.0) then
        ier = 311
        return
      endif
      CALL  REGRES(NV,NVMAX,npoint,IP,X,F,C,SUM,jer) 
      if (jer.gt.0) then
	    if(jer.eq.10)then
	      ier = 310
	    else if (jer.eq.11) then
	      ier = 311
	    end if

        return
      endif
      CEN(1)=C(1)   !c(1)
      CEN(2)=C(2)   !c(2)
      CEN(3)=C(4)   !c(5)
      CEN(4)=C(3)   !c(3)
      CEN(5)=C(5)   !c(6)



c**********************************************************************
c non sono state definite in input le spezzate di surge. Le definisco
c da programma come luogo dei massimi delle parabole a numero di giri 
c costante
c***********************************************************************
c-conto il numero di punti limite minimo da calcolare  
      rev_min=nint(min_rev*100.)+10 !Serve per evitare 
	                   ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                   ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre

	rev_da=rev_min
	max=nint(max_rev*100.)
	ncurve=2
	do while (rev_da.lt.max)
	   ncurve=ncurve+1
	   rev_da=rev_da+10
	end do
c-calcola i punti limite---->CASO CURVA LIMITE NO
      if (nlim.eq.0) then  
        nlim=1
c-------punto sulla curva a numero di giri minimo
	  lim_n(nlim)=min_rev
        lim_Q(nlim) = (-((chn(4)*lim_n(nlim)+chn(2))/(2*chn(5))))*1.01
        MIN_Q = EPS_Q*nlim 
        IF (LIM_Q(NLIM).LT.MIN_Q) LIM_Q(NLIM) = MIN_Q
c------->
        rev_da=rev_min
	  do while (rev_da.lt.max)
	    nlim=nlim+1
          lim_n(nlim) = rev_da/100.
          lim_Q(nlim) = (-((chn(4)*lim_n(nlim)+chn(2))/(2*chn(5))))*1.01
          MIN_Q = EPS_Q * nlim
          IF (LIM_Q(NLIM).LT.MIN_Q) LIM_Q(NLIM) = MIN_Q
          rev_da=rev_da+10
	  end do
c------->punto sulla curva a numero di giri massimo 
        nlim=nlim+1
	  lim_n(nlim)=max_rev
        lim_Q(nlim) = (-((chn(4)*lim_n(nlim)+chn(2))/(2*chn(5))))*1.01
        MIN_Q = EPS_Q*nlim 
        IF (LIM_Q(NLIM).LT.MIN_Q) LIM_Q(NLIM) = MIN_Q

c---->        calcolo dei coefficienti delle rette che
c             descrivono la spezzata di intervento anti pompaggio
        do i=1,nlim
          lim_h(i)=chn(1)+chn(2)*lim_q(i)+chn(3)*lim_n(i)+
     *             chn(4)*lim_q(i)*lim_n(i)+chn(5)*lim_q(i)*lim_q(i)+
     *             chn(6)*lim_n(i)*lim_n(i)
        enddo
        do i=1,nlim-1
          blim(i+1)=(lim_h(i+1)-lim_h(i))/(lim_q(i+1)-lim_q(i))
          alim(i+1)=lim_h(i)-lim_q(i)*blim(i+1)
          if (blim(i+1).le.0) then
            ier = 41
            return
          endif
        enddo  
C LA PRIMA SPEZZATA DEVE PARTIRE DALL'ORIGINE
        blim(1)=lim_h(1)/lim_q(1)
        alim(1)=0.
        if (blim(1).le.0) then
          ier = 41
          return
        endif
C
c-17gennaio
c        alim(nlim+1)=alim(nlim)
c        blim(nlim+1)=blim(nlim)
        alim(nlim+1)=0
        blim(nlim+1)=0
c-17gennaio

	else
c ----->nuovo trattamento dei punti limite quando CURVA LIMITE SI
c         call calcola_punti_limite (min_rev,max_rev,ncurve,nlim,
c     *                                 lim_n,lim_q,chn,
c     *                                 lim_h,alim,blim,
c     *                                 ier)
C-22 febbraio 2006 ----> ripristinata la gestione dei punti limite tipo NUOVO SIRE con
c                        intervento sui punti limite assegnati in tabella ma non usati 
c                        nel calcolo.
         call calcola_punti_limite_SIRE (nom_rev,min_rev,max_rev,nlim,
     *                                 lim_n,lim_q,chn,
     *                                 lim_h,alim,blim,
     *                                 ier,iwr)
         
      end if

      return
      end
c****************************************************************************
      subroutine calcola_chc_old(max_rev,min_rev,clim,npoint,
     *                       rev_cho,flow_cho,head_cho,chn,
     *                       chc,ier)

	implicit none
      include '../inc/param.inc'  !Definisce alcune costanti e le massime 
	                         !grandezze dei vettori
      include '../inc/rk_param.inc'!Contiene le grandezze di riferimento e
	                         !i coefficienti per la trasformazione in
	                         !unità M.K.S

c--------->INPUT 
      real*4 max_rev,  !giri max
     *       min_rev,  !giri min
     *       clim(4),  !coefficienti curve limite
     *       chn(6)    !coeff curve giri zona normale

      integer*4 npoint !numero di punti assegnati da utente nella zona choking


C--------->INPUT/OUTPUT
      real*4 rev_cho(*),  !coordinate dei punti assegnati da utente nella zona 
     *       flow_cho(*), !   di choking
     *       head_cho(*)  

C--------->OUTPUT
      REAL*4 chc(6)   !coeff curve giri zona choking
      integer*4 ier

c---------------->variabili locali
	integer*4   nv        ! numero di variabili indipendenti
     *                      !        (f=f(x(i)) i=1,..nv)	                             
     *	       ,nvmax     ! numero max di variabili indipendenti 
     *		   ,ip(20)    ! indice di presenza dei termini del polinomio  
     *           ,i,jer
	real*4 x(3,MAX_POINT) ! matrice variabili indipendenti 
     *	  ,f(MAX_POINT)   ! vettore variabile dipendente 
     *	  ,c(20)          ! coefficienti del polinomio calcolati 
     *      ,sum            ! somma del quadrato degli scarti 

	real*4      qc1,qc2,qc3           !
     *	       ,nc1,nc2,nc3           !
     *		   ,hc1,hc2,hc3           !
     *		   ,der1,der2             ! Parametri usati per la determinazione 
     *		   ,dn,dg                 !     dei coefficienti CHC
     *	   	   ,a1,a2,a3,a4,a6        ! 
     *		   ,b1,b2,b3,b4,b6        ! 
     *		   ,c1,c3                 !
      real*4      q_condi(3)            ! portata
     *	       ,h_condi(3)            ! altezza adiabatica
     *	       ,n_condi(3)            ! numero giri
      
c*****************************************************************************************

c
cc calcolo delle coordinate di tre punti sulla curva di choking
c
      dg = (max_rev-min_rev)/2
      do i=1,3
        n_condi(i) = max_rev-dg*(i-1)
        call QDAN_CH_a(clim(3),q_condi(i),n_condi(i),chn)
        call HDAN_CH_a(clim(3),h_condi(i),n_condi(i),chn)
      enddo
c
cc      calcolo dei parametri:
c
      qc1=q_condi(1)
      nc1=n_condi(1)
      hc1=h_condi(1)
      qc2=q_condi(3)
      nc2=n_condi(3)
      hc2=h_condi(3)
      qc3=q_condi(2)
      nc3=n_condi(2)
      hc3=h_condi(2)
      der1=chn(2)+chn(4)*nc1+2*chn(5)*qc1
      der2=chn(2)+chn(4)*nc2+2*chn(5)*qc2
      dn=nc2-nc1
      a4=(der2-der1)/dn
      b4=2*(qc1-qc2)/dn
      a2=(der1*nc2-der2*nc1)/dn
      b2=2*(nc1*qc2-qc1*nc2)/dn
      a3=(hc2-hc1)/dn + a2*(qc1-qc2)/dn + a4*(nc1*qc1-nc2*qc2)/dn
      b3=b2*(qc1-qc2)/dn + b4*(nc1*qc1-nc2*qc2)/dn + 
     *  (qc1**2-qc2**2)/dn
      c3=(nc1**2-nc2**2)/dn
      a1=hc1-a2*qc1-a3*nc1-a4*nc1*qc1
      b1=-(b2*qc1+b3*nc1+b4*nc1*qc1+qc1**2)
      c1=-c3*nc1-nc1**2
      a6=(hc3-a1-a2*qc3-a3*nc3-a4*nc3*qc3)/(c1+c3*nc3+nc3**2)
      b6=-(b1+b2*qc3+b3*nc3+b4*nc3*qc3+qc3**2)/(c1+c3*nc3+nc3**2)
      do I=1,npoint
         X(1,I)=b1 + b2*flow_CHO(i) + b3*rev_cho(i) +
     *       b4*rev_cho(i)*flow_cho(i) + b6*rev_cho(i)**2 + b6*c1 +
     *       b6*c3*rev_cho(i) + flow_cho(i)**2
         F(I)=head_cho(I)-a1-a2*flow_cho(i)-a3*rev_cho(i)-
     *             a4*rev_cho(i)*flow_cho(i)
c-prova elli
c     *             -a6*rev_cho(i)**2-a6*c1-a6*c3*rev_cho(i)
      enddo
      nv=1   
      nvmax=3
      do i=1,20
        ip(i)=0
      enddo
      ip(2)=1
      call  regres(nv,nvmax,npoint,ip,x,f,c,sum,jer) 
      if (jer.gt.0) then
	  if(jer.eq.10)then
	    ier = 210
	  else if (jer.eq.11) then
	    ier = 211
	  end if
        return
      endif
      chc(5)=c(1)
      chc(6)=a6+b6*chc(5)
      chc(1)=a1+b1*chc(5)+c1*chc(6)
      chc(2)=a2+b2*chc(5)
      chc(3)=a3+b3*chc(5)+c3*chc(6)
      chc(4)=a4+b4*chc(5)
c        
      return
	end
c**********************************************************************************
      subroutine calcola_chc_new(max_rev,min_rev,clim,npoint,
     *                       rev_cho,flow_cho,head_cho,chn,
     *                       chc_new,ier)

	implicit none
      include '../inc/param.inc'  !Definisce alcune costanti e le massime 
	                         !grandezze dei vettori
      include '../inc/rk_param.inc'!Contiene le grandezze di riferimento e
	                         !i coefficienti per la trasformazione in
	                         !unità M.K.S

c--------->INPUT 
      real*4 max_rev,  !giri max
     *       min_rev,  !giri min
     *       clim(4),  !coefficienti curve limite
     *       chn(6)    !coeff curve giri zona normale

      integer*4 npoint !numero di punti assegnati da utente nella zona choking


C--------->INPUT/OUTPUT
      real*4 rev_cho(*),  !coordinate dei punti assegnati da utente nella zona 
     *       flow_cho(*), !   di choking
     *       head_cho(*)  
c     *       ,head_cho_ac(20)  


C--------->OUTPUT
      REAL*4 chc_new(6)   !coeff curve giri zona choking
      integer*4 ier
c-variabili locali
      integer*4 ngiri,count,i,ii,j,k,kk,m_righe,n_colonne,ipv(20),kr,
     *          k_colonne,k_righe
	real*4 giri(max_ngiri),
     *       c(20),sum

      real*8 aa(max_point+3*max_ngiri,5)  !     (si suppone npt<=200)
     *    ,a(2000)
     *    ,y(200)
     *	,c_8(20),b(20),sm(20),s(20)        
     *    ,sum_8    
     
     
      integer*4 status,jj       
c*****************************************************************************************
      ier =0
c-conto le curve a giri costanti graficate sulla mappa del compressore
c    ngiri-->numero di curve
c    valori dei giri graficati

      call calcola_giri(max_rev,min_rev,ngiri,giri)

c  preparo i dati da passare alla LEASTQ
c  A: rappresentazione matriciale del sistema da risolvere  (I)
c  Y: termine noto   (I)
c  C: vettore delle incognite  (O)


c------->TERMINE NOTO Y
c------>calcolo le coordinate dei punti sulla curva di choking relativo (clim(3))
c       ai numeri di giri graficati
	do i=1,ngiri
	      rev_cho(npoint+i)=giri(i)
            call qdan_ch_a(clim(3),flow_cho(npoint+i),giri(i),chn)
c            head_cho_ac(npoint+i)=clim(3)*flow_cho(npoint+i)
c     *                                    *flow_cho(npoint+i)
            call hdaqn_a(head_cho(npoint+i),flow_cho(npoint+i),
     *                   giri(i),chn)
     	end do
      j=npoint+ngiri
      do i=1,j
	   y(i)=DBLE(head_cho(i)-head_cho(npoint+1))
	end do
      do i=1,ngiri
         y(j+i)=DBLE(chn(2)+giri(i)*chn(4)+2*chn(5)*flow_cho(npoint+i))	
	end do
c	jj=j+ngiri
c      do i=1,ngiri
c         y(jj+i)=DBLE(chn(3)+flow_cho(npoint+i)*chn(4)+2*chn(6)*giri(i))	
c	end do

c------->MATRICE A
      m_righe= j+ngiri
c      m_righe= j+2*ngiri

	n_colonne= 5  !numero di incognite
     
      do i=1,m_righe
	  do k=1,n_colonne
	     aa(i,k)=0.
	  end do
	end do

      do k=1,j
         aa(k,1)=DBLE(flow_cho(k)-flow_cho(npoint+1))
         aa(k,2)=DBLE(rev_cho(k)-rev_cho(npoint+1))
         aa(k,3)=DBLE(rev_cho(k)*flow_cho(k)-
     *               rev_cho(npoint+1)*flow_cho(npoint+1))
         aa(k,4)=DBLE(flow_cho(k)**2-flow_cho(npoint+1)**2)
         aa(k,5)=DBLE(rev_cho(k)**2-rev_cho(npoint+1)**2)
	end do
     
	do k=1,ngiri
         aa(j+k,1)=DBLE(1)
         aa(j+k,3)=DBLE(giri(k))
         aa(j+k,4)=DBLE(2*flow_cho(npoint+k))
	end do

c	do k=1,ngiri
c         aa(jj+k,2)=DBLE(1)
c         aa(jj+k,3)=DBLE(flow_cho(npoint+k))
c         aa(jj+k,5)=DBLE(2*giri(k))
c	end do

c   Scarico la matrice AA sul vettore A per poterlo passare alla LEASTQ
c   (sfrutto il fatto che il FORTRAN passa da vettore a matrice spostandosi di 
c    colonna in colonna)

      count=0
	do k_colonne=1,n_colonne
	   do k_righe=1,m_righe
	      a(count+k_righe)=aa(k_righe,k_colonne)
	   end do
	   count=count+m_righe
	end do


c    VETTORE C
      call leastq(m_righe,n_colonne,a,y,b,s,c_8,sm,sum_8,ipv,kr)

	do i=1,n_colonne
	  c(i)= SNGL(c_8(i))
	  sum = SNGL(sum_8)
	end do
c-gestione errore
      if (kr.ne.n_colonne) then
	   ier=10
	   return
	end if

C  definisco i coefficienti CHC tenendo conto che:
c  c1=chc2, c2=chc3, c3=chc4, c4=chc5, c5=chc6
c  chc1 viene definito imponendo la continuita sul punto che sulla curva di choking
c       relativo si trova al massimo numero di giri
      chc_new(1)=head_cho(npoint+1)-c(1)*flow_cho(npoint+1)
     *          -c(2)*rev_cho(npoint+1)
     *          -c(3)*rev_cho(npoint+1)*flow_cho(npoint+1)
     *          -c(4)*flow_cho(npoint+1)**2
     *          -c(5)*rev_cho(npoint+1)**2
      
      do i=2,6
         chc_new(i)=c(i-1)
      enddo
c-elena
c9999  continue
	return
	end
c*************************************************************************************
      subroutine calcola_chc_derivate(max_rev,min_rev,clim,npoint,
     *                       rev_cho,flow_cho,head_cho,chn,
     *                       chc,ier)

	implicit none
      include '../inc/param.inc'  !Definisce alcune costanti e le massime 
	                         !grandezze dei vettori
      include '../inc/rk_param.inc'!Contiene le grandezze di riferimento e
	                         !i coefficienti per la trasformazione in
	                         !unità M.K.S

c--------->INPUT 
      real*4 max_rev,  !giri max
     *       min_rev,  !giri min
     *       clim(4),  !coefficienti curve limite
     *       chn(6)    !coeff curve giri zona normale

      integer*4 npoint !numero di punti assegnati da utente nella zona choking


C--------->INPUT/OUTPUT
      real*4 rev_cho(*),  !coordinate dei punti assegnati da utente nella zona 
     *       flow_cho(*), !   di choking
     *       head_cho(*)  

C--------->OUTPUT
      REAL*4 chc(6)   !coeff curve giri zona choking
      integer*4 ier
c-variabili locali
      integer*4 count,i,ii,j,k,kk,m_righe,n_colonne,ipv(20),kr,
     *          k_colonne,k_righe,ngiri
	real*4 c(20),sum

      real*8 aa(max_point+2*max_ngiri,3)  !     (si suppone npt<=200)
     *    ,a(2000)
     *    ,y(200)
     *	,c_8(20),b(20),sm(20),s(20)        
     *    ,sum_8  

	real*4      qc1,qc2,qc3           !
     *	       ,nc1,nc2,nc3           !
     *		   ,hc1,hc2,hc3           !
     *		   ,der1,der2             ! Parametri usati per la determinazione 
     *		   ,dn,dg                 !     dei coefficienti CHC
     *	   	   ,a1,a2,a3,a4,a6        ! 
     *		   ,b1,b2,b3,b4,b6        ! 
     *		   ,c1,c3                 !
     
      real*4 n_condi(3),q_condi(3),h_condi(3),der_q,der_n,
     *       giri(12),head_giri(12),flow_giri(12)       
c*****************************************************************************************
      ier =0

c--->calcolo i punti su cui imporre le condizioni di continuità

      dg = (max_rev-min_rev)/2
      do i=1,3
        n_condi(i) = max_rev-dg*(i-1)
        call QDAN_CH_a(clim(3),q_condi(i),n_condi(i),chn)
        call HDAN_CH_a(clim(3),h_condi(i),n_condi(i),chn)
      enddo

      call calcola_giri(max_rev,min_rev,ngiri,giri)

c------>calcolo le coordinate dei punti sulla curva di choking relativo (clim(3))
c       ai numeri di giri graficati
	do i=1,ngiri
            call qdan_ch_a(clim(3),flow_giri(i),giri(i),chn)
            call hdaqn_a(head_giri(i),flow_giri(i),giri(i),chn)
     	end do
c  preparo i dati da passare alla LEASTQ
c  A: rappresentazione matriciale del sistema da risolvere  (I)
c  Y: termine noto   (I)
c  C: vettore delle incognite  (O)

c
cc      calcolo dei parametri:
c
      qc1=q_condi(1)
      nc1=n_condi(1)
      hc1=h_condi(1)
      qc2=q_condi(3)
      nc2=n_condi(3)
      hc2=h_condi(3)
      qc3=q_condi(2)
      nc3=n_condi(2)
      hc3=h_condi(2)
      der1=chn(2)+chn(4)*nc1+2*chn(5)*qc1
      der2=chn(2)+chn(4)*nc2+2*chn(5)*qc2
      dn=nc2-nc1
      a4=(der2-der1)/dn
      b4=2*(qc1-qc2)/dn
      a2=(der1*nc2-der2*nc1)/dn
      b2=2*(nc1*qc2-qc1*nc2)/dn
      a3=(hc2-hc1)/dn + a2*(qc1-qc2)/dn + a4*(nc1*qc1-nc2*qc2)/dn
      b3=b2*(qc1-qc2)/dn + b4*(nc1*qc1-nc2*qc2)/dn + 
     *  (qc1**2-qc2**2)/dn
      c3=(nc1**2-nc2**2)/dn
      a1=hc1-a2*qc1-a3*nc1-a4*nc1*qc1
      b1=-(b2*qc1+b3*nc1+b4*nc1*qc1+qc1**2)
      c1=-c3*nc1-nc1**2
      a6=(hc3-a1-a2*qc3-a3*nc3-a4*nc3*qc3)/(c1+c3*nc3+nc3**2)
      b6=-(b1+b2*qc3+b3*nc3+b4*nc3*qc3+qc3**2)/(c1+c3*nc3+nc3**2)

c------->TERMINE NOTO Y
c     

      do i=1,npoint
	   y(i)=DBLE(head_cho(I)-a1-a2*flow_cho(i)-a3*rev_cho(i)-
     *             a4*rev_cho(i)*flow_cho(i))
     	end do
	j=npoint
	do i=1,ngiri
	   der_q=chn(2)+giri(i)*chn(4)+2*chn(5)*flow_giri(i)
	   y(j+i)=DBLE(der_q-a2-giri(i)*a4)
	end do
c-elena 25 gennaio 2006- continuità della derivata di H rispetto a n nei punti sulla 
c                        curva di choking ai giri costanti graficati;
c                        considerando anche questa condizione le parabole a giri costanti
c                        nella zona di choking cadono verso il basso più velocemente;
c     j=j+ngiri
c	do i=1,ngiri
c	   der_n=chn(3)+flow_giri(i)*chn(4)+2*chn(6)*giri(i)
c	   y(j+i)=DBLE(der_n-a3-giri(i)*a4-a6*c3)
c	end do
c--------------------------------------------------------------------------------------

c------->MATRICE A
c      m_righe= npoint+2*ngiri
      m_righe= npoint+ngiri
	n_colonne= 1  !numero di incognite
     
      do i=1,m_righe
	     a(i)=0.
	end do

      do i=1,npoint
         a(i)=DBLE(b1 + b2*flow_CHO(i) + b3*rev_cho(i) +
     *       b4*rev_cho(i)*flow_cho(i) + b6*rev_cho(i)**2 + b6*c1 +
     *       b6*c3*rev_cho(i) + flow_cho(i)**2)
	end do
	j=npoint
	do i=1,ngiri
	   a(j+i)=DBLE(b2+giri(i)*b4+2*flow_giri(i))
	end do
      
c-elena 25 gennaio 2006- continuità della derivata di H rispetto a n nei punti sulla 
c                        curva di choking ai giri costanti graficati;
c	j=npoint+ngiri
c	do i=1,ngiri
c	   a(j+i)=DBLE(b3+b6*c3+giri(i)*b4+2*flow_giri(i))
c	end do
c-----------------------------------------------------------------------------------

c    VETTORE C
      call leastq(m_righe,n_colonne,a,y,b,s,c_8,sm,sum_8,ipv,kr)

	do i=1,n_colonne
	  c(i)= SNGL(c_8(i))
	end do
c-gestione errore
      if (kr.ne.n_colonne) then
	   ier=10
	   return
	end if

C  definisco i coefficienti CHC tenendo conto che c1=chc(5)

      chc(5)=c(1)
      chc(6)=a6+b6*chc(5)
      chc(1)=a1+b1*chc(5)+c1*chc(6)
      chc(2)=a2+b2*chc(5)
      chc(3)=a3+b3*chc(5)+c3*chc(6)
      chc(4)=a4+b4*chc(5)


	return
	end
c******************************************************************************************
      subroutine calcola_punti_limite (min_rev,max_rev,ncurve,nlim,
     *                                 lim_n,lim_q,chn,
     *                                 lim_h,alim,blim,
     *                                 ier)
c----------------------------------------------------------------------------------------
c   subroutine per la gestione dei punti limite nel caso CURVA LIMITE SI, ossia quando 
c   l'utente richiede la graficazione della curva di surge sperimentale e a tale scopo 
c   inserisce punti limite nell'apposito pannello.
c   CAS0 1: I punti inseriti cadono tutti all'interno dell'intervallo [giri_min,giri_max]
c           e sono in numero minore al numero di curve a giri costante da graficare(= numero 
c           di segmenti nella spezzata di surge)
c           Nella tabella dei punti limite oltre alla valorizzazione della colonna relativa
c           all'altezza adiabatica vengono anche visualizzati gli estremi di ogni segmento
c           calcolati dal sistema.
c   CAS0 2: I punti inseriti cadono tutti all'interno dell'intervallo [giri_min,giri_max]
c           e sono in numero maggiore o uguale al numero di curve a giri costante da graficare
c          (= numero di segmenti nella spezzata di surge)
c           Nella tabella dei punti limite viene valorizzata la colonna relativa all'altezza 
c           adiabatica
c   CAS03:  Alcuni punti inseriti cadono al di fuori dell'intervallo [giri_min,giri_max].
c           Questi punti non vengono presi in considerazione nel calcolo ma vengono comunque 
c           visualizzati nel pannello dei punti limite; si riconoscono dal fatto che la loro 
c           altezza adiabatica è zero. Nel caso i punti interessati dal calcolo siano in 
c           numero inferiore ai segmenti della spezzata la gestione è la stessa descritta
c           in precedenza.
c           CASO LIMITE: se il numero dei punti limite calcolati sommato al numero dei punti 
c                        inseriti supera il numero massimo dei punti limite inseribili (=20)
c                        nel pannello dei punti limite rimarranno visualizzati solo i punti 
c                        interessati dal calcolo della spezzata di surge
c----------------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'

c------------>INPUT
      real*4 min_rev,max_rev
	real*4 lim_n(*),lim_q(*),chn(*)
      integer*4 ncurve,nlim
c------------>OUTPUT
      real*4 lim_h(*),alim(*),blim(*)

c------------>variabili locali
      real*4 lim_n_app(20),lim_q_app(20)
      real*4 lim_n_work(20),lim_q_work(20)
      real*4 lim_h_work(20),alim_work(21),blim_work(21)

      integer*4 nlim_tot

c      integer*4 inizio,fine,i,j,k,
      integer*4 inizio,fine,i,k,
     *          nlim_work,scarto,scarto_fine,
     *          ier,rev_da,rev_min,irange,max
	real*4    alimite(3),blimite(3),
     *           nlimite(2),qlimite(2),hlimite(2),qql


c------------>Individuo la porzione di vettore da usare nel calcolo dei punti limite
c             (i punti sono passati alla dll in ordine crescente)
      do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_n_work(i)=0
	   lim_q_work(i)=0
	end do

c	nlim_work=nlim
	inizio=1
	do while (lim_n(inizio).lt.min_rev) 
	   inizio=inizio+1
c	   nlim_work=nlim_work-1
	end do
    
      fine=nlim
	do while (lim_n(fine).gt.max_rev) 
	   fine=fine-1
c	   nlim_work=nlim_work-1
	end do

      nlim_work=0
      do i=inizio,fine
	   nlim_work=nlim_work+1
	   lim_n_work(nlim_work)=lim_n(i)
	   lim_q_work(nlim_work)=lim_q(i)
c	   j=j+1
	end do

	scarto=nlim-nlim_work
      scarto_fine=nlim-fine
c----->calcolo i punti limite
      if (nlim_work.lt.ncurve) then
C-creo i vettori di appoggio
        nlimite(1)=min_rev
	  nlimite(2)=max_rev
        qlimite(1)=lim_q_work(1)    !n=min_rev
	  qlimite(2)=lim_q_work(nlim_work) !n_max_rev
        call hdaqn_a(hlimite(1),qlimite(1),min_rev,chn)
        call hdaqn_a(hlimite(2),qlimite(2),max_rev,chn)
        blimite(1)=hlimite(1)/qlimite(1)
        alimite(1)=0.
        blimite(2)=(hlimite(2)-hlimite(1))/(qlimite(2)-qlimite(1))
        alimite(2)=hlimite(1)-qlimite(1)*blimite(2)
        do i=1,2
          if (blimite(i).le.0) then
             ier = 41
             return
          endif
	  end do
        alimite(3)=alimite(2)
        blimite(3)=blimite(2)

c-------->riempio il vettore LIM_N e definisco NLIM
        rev_min=nint(min_rev*100.)+10 !Serve per evitare 
	                   ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                   ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre
	  max=nint(max_rev*100)

	  rev_da=rev_min
        nlim_work=1
	  lim_n_work(nlim_work)=min_rev
	  do while (rev_da.lt.max)
	    nlim_work=nlim_work+1
          lim_n_work(nlim_work) = rev_da/100.
          rev_da=rev_da+10
	  end do
        nlim_work=nlim_work+1
	  lim_n_work(nlim_work)=max_rev
c-------->
        do i=1,nlim_work
           call qdan_su_a(chn,alimite,blimite,2,nlimite,
     *                   lim_n_work(i),lim_q_work(i),irange)
           QQL = (-((chn(4)*lim_n_work(i)+chn(2))/(2*chn(5))))*1.01
           IF (QQL.GT.LIM_Q_work(i)) LIM_Q_work(i)=QQL
        end do
c-elena*************************************
      ELSE
C VERIFICO CHE LE LIM_Q DEFINITE DA UTENTE SIANO A DESTRA DEL LUOGO DEI
C MASSIMI, ALTRIMENTI IMPONGO LA QMIN DEFINITA COME MASSIMO DELLA PARABOLA
C RELATIVA AL VALORE LIM_N ASSEGNATO
        DO I=1,NLIM_work
          QQL = (-((chn(4)*lim_n_work(I)+chn(2))/(2*chn(5))))*1.01
          IF (QQL.GT.LIM_Q_work(I)) LIM_Q_work(I)=QQL
        ENDDO
      endif
c-------------> calcolo h
      do i=1,nlim_work
        lim_h_work(i)=chn(1)+chn(2)*lim_q_work(i)+chn(3)*lim_n_work(i)+
     *             chn(4)*lim_q_work(i)*lim_n_work(i)+
     *             chn(5)*lim_q_work(i)*lim_q_work(i)+
     *             chn(6)*lim_n_work(i)*lim_n_work(i)
      enddo
c------------->calcolo alim/blim
      do i=1,nlim_work-1
        blim_work(i+1)=(lim_h_work(i+1)-lim_h_work(i))/
     *                 (lim_q_work(i+1)-lim_q_work(i))
        alim_work(i+1)=lim_h_work(i)-lim_q_work(i)*blim_work(i+1)
        if (blim_work(i+1).le.0) then
          ier = 41
          return
        endif
      enddo  
C LA PRIMA SPEZZATA DEVE PARTIRE DALL'ORIGINE
      blim_work(1)=lim_h_work(1)/lim_q_work(1)
      alim_work(1)=0.
      if (blim_work(1).le.0) then
        ier = 41
        return
      endif
C
c-17gennaio
c      alim_work(nlim_work+1)=alim_work(nlim_work)
c      blim_work(nlim_work+1)=blim_work(nlim_work)
c-17gennaio

      nlim_tot=nlim_work+scarto
	if (nlim_tot.gt.max_nlim) then
	    do i=1,max_nlim
	       if (i.gt.nlim_work) then
	          lim_n(i)=0
	          lim_q(i)=0
	          lim_h(i)=0
	          alim(i)=0
	          blim(i)=0
	       else
	          lim_n(i)=lim_n_work(i)
	          lim_q(i)=lim_q_work(i)
	          lim_h(i)=lim_h_work(i)
	          alim(i)=alim_work(i)
	          blim(i)=blim_work(i)
             end if
		end do
	    nlim=nlim_work
	else
	    k=0
	    i=1
          do while (i.lt.inizio)
	       k=k+1
	       lim_n(k)=lim_n_app(i)
	       lim_q(k)=lim_q_app(i)
	       lim_h(k)=0
	       alim(k)=0
	       blim(k)=0
             i=i+1
	    end do
		do i=1,nlim_work       		
	          k=k+1
	          lim_n(k)=lim_n_work(i)
	          lim_q(k)=lim_q_work(i)
	          lim_h(k)=lim_h_work(i)
	          alim(k)=alim_work(i)
	          blim(k)=blim_work(i)
          end do
c-17 gennaio
c	    alim(k+1)=alim(k)
c	    blim(k+1)=blim(k)
c-17 gennaio
	    do i=1,scarto_fine
	       k=k+1	      
	       lim_n(k)=lim_n_app(fine+i)
	       lim_q(k)=lim_q_app(fine+i)
	       lim_h(k)=0
c-17
c	       alim(k+1)=0
c	       blim(k+1)=0
c-17
	       alim(k)=0
	       blim(k)=0
          end do
	    nlim=nlim_tot
	end if
      return 
	end

c******************************************************************************************
      subroutine calcola_punti_limite_SIRE(nom_rev,min_rev,max_rev,nlim,
     *                                 lim_n,lim_q,chn,
     *                                 lim_h,alim,blim,
     *                                 ier,iwr)
c----------------------------------------------------------------------------------------
c   subroutine per la gestione dei punti limite nel caso CURVA LIMITE SI, ossia quando 
c   l'utente richiede la graficazione della curva di surge sperimentale e a tale scopo 
c   inserisce punti limite nell'apposito pannello.
c   CAS0 1: I punti inseriti cadono tutti all'interno dell'intervallo [giri_min,giri_max]
c           Nella tabella dei punti limite viene valorizzata la colonna relativa all'altezza 
c           adiabatica
c   CAS0 2:  Alcuni punti inseriti cadono al di fuori dell'intervallo [giri_min,giri_max].
c           Questi punti non vengono presi in considerazione nel calcolo ma vengono comunque 
c           visualizzati nel pannello dei punti limite; si riconoscono dal fatto che la loro 
c           altezza adiabatica è zero. 
c----------------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'

c------------>INPUT
      real*4 min_rev,max_rev,nom_rev
	real*4 lim_n(*),lim_q(*),chn(*)
      integer*4 nlim
c------------>OUTPUT
      real*4 lim_h(*),alim(*),blim(*)

c------------>variabili locali
      real*4 lim_n_app(20),lim_q_app(20),lim_n_rpm(20)
      real*4 lim_n_work(20),lim_q_work(20)
      real*4 lim_h_work(20),alim_work(21),blim_work(21)

      integer*4 nlim_tot

c      integer*4 inizio,fine,i,j,k,
      integer*4 inizio,fine,i,k,
     *          nlim_work,scarto,scarto_fine,
     *          ier,iwr_app,
c-ele 30/8/2006
     *          iwr !I/O cod warning:0)nessun warning informativo da segnalare
	              !                100)presenza di punti limite oltre la curva di
	              !                    surge teorica e quindi ricalcolati dal programma
	              !                101)regressione vincolata
	              !                201)punti limite oltre la curva+regressione vincolata
	real*4    qql,min_q
c 2 novembre****************
      logical*2 warning
      real*4 limite_n(20),limite_q(20)
      integer*4 n_lim
*********************************************************************************************
      iwr_app=0
c------------>Individuo la porzione di vettore da usare nel calcolo dei punti limite
c             (i punti sono passati alla dll in ordine crescente)
      do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_n_work(i)=0
	   lim_q_work(i)=0
         lim_n_rpm(i)=lim_n(i)*nom_rev
	end do

c	nlim_work=nlim
	inizio=1
c-ele 14/9/06 Inserita tolleranza di un RPM nel filtro dei valori da considerare 
c             nel calcolo della spezzata di surge
c	do while (lim_n(inizio).lt.min_rev) 
c	   inizio=inizio+1
c	end do
c-prova
	do while (min_rev*nom_rev-lim_n_rpm(inizio).gt.1) 
	   inizio=inizio+1
	end do
c
    
      fine=nlim
c-ele 14/9/06 Inserita tolleranza di un RPM nel filtro dei valori da considerare 
c             nel calcolo della spezzata di surge
c	do while (lim_n(fine).gt.max_rev) 
c	   fine=fine-1
c	end do
c-prova
	do while (lim_n_rpm(fine)-max_rev*nom_rev.gt.1) 
	   fine=fine-1
	end do
c

      nlim_work=0
      do i=inizio,fine
	   nlim_work=nlim_work+1
	   lim_n_work(nlim_work)=lim_n(i)
	   lim_q_work(nlim_work)=lim_q(i)
c	   j=j+1
	end do

	scarto=nlim-nlim_work
      scarto_fine=nlim-fine

c-ele 2 novembre*********************************************************************
      call controlla_punti_limite(min_rev,max_rev,chn,
     *                            nlim_work,lim_n_work,lim_q_work,
     *                            n_lim,limite_n,limite_q,warning,ier)
      if (ier.gt.0) return
	if (warning) then
	   nlim_work=n_lim
	   iwr_app=100
	   do i=1,n_lim
	      lim_n_work(i)=limite_n(i)
	      lim_q_work(i)=limite_q(i)
	   end do	  
	else
     
c********************************************************************************
c----->calcolo i punti limite
C VERIFICO CHE LE LIM_Q DEFINITE DA UTENTE SIANO A DESTRA DEL LUOGO DEI
C MASSIMI, ALTRIMENTI IMPONGO LA QMIN DEFINITA COME MASSIMO DELLA PARABOLA
C RELATIVA AL VALORE LIM_N ASSEGNATO
         DO I=1,NLIM_work
            QQL = (-((chn(4)*lim_n_work(I)+chn(2))/(2*chn(5))))*1.01
c          IF (QQL.GT.LIM_Q_work(I)) LIM_Q_work(I)=QQL
c-ele 30/8/2006**********************************************************************
c   Nel caso uno o più punti limite assegnati da utente cadano oltre la curva teorica
c   di surge il punto viene ricalcolato e riportato sul limite teorico. L'indice IWR
c   tiene traccia di tale operazione a cui deve essere associata la visualizzazione a 
c   a video di un messaggio di warning informativo
c-prova
            min_q=0.01*i
            if (qql.lT.min_q) qql=min_q
c-prova
            if ((qql-lim_q_work(i)).gt.0.001) then
		     lim_q_work(I)=qql
	         iwr_app=100
	      end if
         ENDDO
	end if
	iwr=iwr+iwr_app
c-ele 30/8/2006
c-------------> calcolo h
      do i=1,nlim_work
        lim_h_work(i)=chn(1)+chn(2)*lim_q_work(i)+chn(3)*lim_n_work(i)+
     *             chn(4)*lim_q_work(i)*lim_n_work(i)+
     *             chn(5)*lim_q_work(i)*lim_q_work(i)+
     *             chn(6)*lim_n_work(i)*lim_n_work(i)
      enddo
c------------->calcolo alim/blim
      do i=1,nlim_work-1
        blim_work(i+1)=(lim_h_work(i+1)-lim_h_work(i))/
     *                 (lim_q_work(i+1)-lim_q_work(i))
        alim_work(i+1)=lim_h_work(i)-lim_q_work(i)*blim_work(i+1)
        if (blim_work(i+1).le.0) then
          ier = 41
          return
        endif
      enddo  
C LA PRIMA SPEZZATA DEVE PARTIRE DALL'ORIGINE
      blim_work(1)=lim_h_work(1)/lim_q_work(1)
      alim_work(1)=0.
      if (blim_work(1).le.0) then
        ier = 41
        return
      endif
C
c-17gennaio
c      alim_work(nlim_work+1)=alim_work(nlim_work)
c      blim_work(nlim_work+1)=blim_work(nlim_work)
c-17gennaio

      nlim_tot=nlim_work+scarto
	if (nlim_tot.gt.max_nlim) then
	    do i=1,max_nlim
	       if (i.gt.nlim_work) then
	          lim_n(i)=0
	          lim_q(i)=0
	          lim_h(i)=0
	          alim(i)=0
	          blim(i)=0
	       else
	          lim_n(i)=lim_n_work(i)
	          lim_q(i)=lim_q_work(i)
	          lim_h(i)=lim_h_work(i)
	          alim(i)=alim_work(i)
	          blim(i)=blim_work(i)
             end if
		end do
	    nlim=nlim_work
	else
	    k=0
	    i=1
          do while (i.lt.inizio)
	       k=k+1
	       lim_n(k)=lim_n_app(i)
	       lim_q(k)=lim_q_app(i)
	       lim_h(k)=0
	       alim(k)=0
	       blim(k)=0
             i=i+1
	    end do
		do i=1,nlim_work       		
	          k=k+1
	          lim_n(k)=lim_n_work(i)
	          lim_q(k)=lim_q_work(i)
	          lim_h(k)=lim_h_work(i)
	          alim(k)=alim_work(i)
	          blim(k)=blim_work(i)
          end do
c-17 gennaio
c	    alim(k+1)=alim(k)
c	    blim(k+1)=blim(k)
c-17 gennaio
	    do i=1,scarto_fine
	       k=k+1	      
	       lim_n(k)=lim_n_app(fine+i)
	       lim_q(k)=lim_q_app(fine+i)
	       lim_h(k)=0
c-17
c	       alim(k+1)=0
c	       blim(k+1)=0
c-17
	       alim(k)=0
	       blim(k)=0
          end do
	    nlim=nlim_tot
	end if
      return 
	end

c******************************************************************************************
      subroutine controlla_punti_limite(min_rev,max_rev,chn,
     *                             nlim_work,lim_n_work,lim_q_work,
     *                             n_lim,limite_n,limite_q,warning,ier)

      implicit none
      include '../inc/rk_param.inc'

c------------>INPUT
      real*4 min_rev,max_rev
	real*4 chn(*)
	integer*4 nlim_work
      real*4 lim_n_work(*),lim_q_work(*)

c------------>OUTPUT
	integer*4 n_lim,ier
      real*4 limite_n(*),limite_q(*)
	logical*2 warning

c------------>variabili locali

      integer*4 i,rev_da,rev_min,irange,max,ncurve
	real*4    alimite(3),blimite(3),
     *          nlimite(2),qlimite(2),hlimite(2),qql,min_q

      warning=.false.
      rev_min=nint(min_rev*100.)+10 !Serve per evitare 
	                   ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                   ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre

	rev_da=rev_min
	max=nint(max_rev*100.)
	ncurve=2
	do while (rev_da.lt.max)
	   ncurve=ncurve+1
	   rev_da=rev_da+10
	end do
 
      if (nlim_work.lt.ncurve) then

C-creo i vettori di appoggio
         nlimite(1)=min_rev
	   nlimite(2)=max_rev
         qlimite(1)=lim_q_work(1)    !n=min_rev
	   qlimite(2)=lim_q_work(nlim_work) !n_max_rev
         call hdaqn_a(hlimite(1),qlimite(1),lim_n_work(1),chn)
         call hdaqn_a(hlimite(2),qlimite(2),lim_n_work(nlim_work),chn)
         blimite(1)=hlimite(1)/qlimite(1)
         alimite(1)=0.
         blimite(2)=(hlimite(2)-hlimite(1))/(qlimite(2)-qlimite(1))
         alimite(2)=hlimite(1)-qlimite(1)*blimite(2)
         do i=1,2
           if (blimite(i).le.0) then
             ier = 41
             return
           endif
	   end do
         alimite(3)=alimite(2)
         blimite(3)=blimite(2)

c------>riempio il vettore LIM_N e definisco NLIM
         rev_min=nint(min_rev*100.)+10 !Serve per evitare 
	                 ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                 ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre
	   max=nint(max_rev*100)

	   rev_da=rev_min
         n_lim=1
	   limite_n(n_lim)=min_rev
	   do while (rev_da.lt.max)
	      n_lim=n_lim+1
            limite_n(n_lim) = rev_da/100.
            rev_da=rev_da+10
	   end do
         n_lim=n_lim+1
	   limite_n(n_lim)=max_rev
c------>calcolo q
         do i=1,n_lim
            call qdan_su_a(chn,alimite,blimite,2,nlimite,
     *                 limite_n(i),limite_q(i),irange)
            QQL = (-((chn(4)*limite_n(i)+chn(2))/(2*chn(5))))*1.01
c-prov
            min_q=0.01*i
            if (qql.lT.min_q) qql=min_q
c-prov
            if ((qql-limite_q(i)).gt.0.001) then
		      limite_q(I)=qql
	          warning =.true.
	     end if
         end do

      end if

      return
	end
