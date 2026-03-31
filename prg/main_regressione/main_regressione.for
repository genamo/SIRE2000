c=======================================================================c
      Program main_regressione
c=======================================================================c
c     -------------------> main di regressione <------------------------
c   Parte 1-
c   Calcola i coefficienti delle curve caratteristiche di un compressore.
c   Parte 2-
c   Calcola i coefficienti delle curve caratteristiche di una turbina.
c=======================================================================c
	implicit none
c
c        Variabili per compressori
c
	integer*4 max_nlim,max_n_char
	parameter (max_nlim=20,max_n_char=50)
c
      character compr*(80)!nome del compressore analizzato
     *           ,compr_id*(80)
     *           ,compr_desc*(80)
c
	real nom_rev        !valore nominale del numero di giri
     *     ,nom_flow,nom_head
c
      real max_rev,       !numero di giri massimo rispetto al valore nominale
     *     min_rev,        !numero di giri minimo rispetto al valore nominale
     *     min_eff
c
      real chlim_q,       !portata del punto limite di choking
     *     chlim_h        !prevalenza del punto limite di choking
c
      integer n_char      !punti assegnati per effettuare la regressione
c                   
                          !grandezze relative ai punti assegnati:
      real rev(max_n_char),       !numero di giri (dim=n_char,max_n_char=50)
     *     flow(max_n_char),      !portata (dim=n_char)
     *     head(max_n_char),      !altezza adiabatica (dim=n_char)
     *     eff(max_n_char)        !efficienza (dim=n_char)
c
      integer nlim       !punti limite assegnati per definire le spezzate 
	                   !di anti-surge
c----------------------->Input/Output  
c
c     caso 1- se nlim>0 le spezzate di surge vengono definite in input ----->
c             lim_n,lim_q sono input
c     caso 2- se nlim=0 le spezzate di surge non vengono definite in input e quindi 
c             le definisco da programma------>lim_n,lim_q sono output
c
                          !grandezze relative ai punti limite assegnati:
      real lim_n(max_nlim),     !numero di giri(dim=nlim,max_n_lim=20)
     *	 lim_q(max_nlim)      !portata (dim=nlim)
c----------------------->Output
      real chlim_n        !numero di giri corrispondente al pto limite di choking 
c  
      real lim_h(max_nlim)      !altezza adiabatica dei punti limite
c
c                        !coefficienti per calcolare:
      real chn(6),       !altezza adiabatica nella zona normale (dim=6)
     *     chc(6),       !altezza adiabatica nella zona di choking (dim=6)
     *     cen(5),        !efficienza nella zona normale (dim=5)
     *     cec(6)        !efficienza nella zona normale (dim=5)
c
                         !coefficienti delle rette che descrivono la spezzata di 
					   !intervento anti pompaggio:
      real alim(max_nlim+1),      !termine noto (dim=nlim+1)
     *     blim(max_nlim+1)       !coefficiente angolare(dim=nlim+1)
c
      real clim(4)       !limiti anti-choke (dim=4)
	                   !zona di choking per l'efficienza:
					   !clim(1):limite relativo dal punto assegnato in input
	                   !clim(2):limite assoluto dal punto estremo 
	                   !zona di choking per il numero di giri:
					   !clim(3):limite relativo dal punto assegnato in input
	                   !clim(4):limite assoluto dal punto estremo 
c
      character err_msg*(200)!messaggio che descrive l'esito della regressione e
	                     !gli eventuali errori commessi
	integer ier          !codice d'errore
	integer iwr          !codice d'errore
c

c        Variabili per turbine
	integer*4 max_ncorr,max_nmaxp,max_nhrate
	parameter (max_ncorr=50,max_nmaxp=50,max_nhrate=50)

      character*(80) turb    !nome della turbina analizzata
     *           ,turb_id*(80)
     *           ,turb_desc*(80)
c
      real nom_tair,         !valore nominale della temperatura dell'aria
     *     nom_power,        !valore nominale della potenza
     *	 nom_hrate         !valore nominale dell'heat-rate
c
c           Dati per la regressione dei coefficienti correttivi
c
      integer*4 ncorr       !punti assegnati,max_ncorr=50  
                            !grandezze relative a tali punti:
      real temp(max_ncorr),        !temperatura;
     *     fpw(max_ncorr),         !fattore di correzione per la potenza fpw=f(temp)
     *     fhr(max_ncorr)          !fattore di correzione per l'heat rate fhr=f(temp)
	                      
c
c           Dati per la regressione dei coefficienti CPW   
c
      integer*4 nmaxp        !punti assegnati
                             !grandezze relative a tali punti:
	real rev_pwmax(max_nmaxp), !numero di giri; 
     *     pwmax(max_nmaxp),     !potenza massima
     *     airpwm(max_nmaxp)     !temperatura dell'aria
c
c           Dati per la regressione dei coefficienti CHR
c
      integer*4 nhrate        !punti assegnati
                              !grandezze relative a tali punti:
	real rev_hrate(max_nhrate), !numero di giri; 
     *     power(max_nhrate),     !potenza massima;
     *     airhrt(max_nhrate),    !temperatura dell'aria;
     *     hrate(max_nhrate)      !heat rate

c------------------->Output
      real cpw(3),      !coefficienti per il calcolo della potenza massima:
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
      character*(200) error_msg  !messaggio che descrive l'esito della regressione
                             !e gli eventuali errori commessi
c   
      integer jer            !indice d'errore

      character*(120) ute,dat,log
      character*(150) i_name_file,o_name_file
      integer*4 ii,jj,status
	logical*2 flag_lim


      integer*4 nlim_app,i      
      real*4 lim_n_app(max_nlim),     !numero di giri(dim=nlim,max_n_lim=20)
     *	 lim_q_app(max_nlim)      !portata (dim=nlim)
      real*4 lim_h_app(max_nlim)      !altezza adiabatica dei punti limite
      real*4 alim_app(max_nlim+1),      !termine noto (dim=nlim+1)
     *     blim_app(max_nlim+1)       !coefficiente angolare(dim=nlim+1)
     *     ,app
cc************************************************************************
c
      flag_lim = .true.
	call get_DatLog(ute,dat,log)

	i_name_file = trim(dat) //'/'// 'compr_testa.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) compr,compr_id,compr_desc
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'compr_dcar.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) app,nom_rev,nom_flow,nom_head,max_rev,min_rev,min_eff,
     *            chlim_q,chlim_h
      close (unit=100)


	i_name_file = trim(dat) //'/'// 'compr_pcar.sies' 
   
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) n_char
	do ii=1,n_char
	   read(100,*) eff(ii),flow(ii),head(ii),rev(ii)
	end do
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'compr_plim.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

      read(100,*) nlim
	if (nlim.eq.0) then
	   flag_lim = .false.
	else

	   do ii=1,nlim
	     read(100,*) lim_n(ii),lim_q(ii)
	   end do
	end if
      close (unit=100)


c	i_name_file = trim(dat) //'/'// 'runits_c.txt' 
	o_name_file = trim(dat) //'/'// 'units_c.txt' 
c    
c     Apre il file di input per il compressore   
cc      open(UNIT=10,FILE=i_name_file,
cc     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
c      read(10,*) compr
c	read(10,*) nom_rev
c      read(10,*) max_rev,min_rev,min_eff
c      read(10,*) chlim_q,chlim_h
c      read(10,*) nlim
c	if (nlim.eq.0) then
c	   flag_lim = .false.
c	else
c	   do ii=1,nlim
c	      read(10,*) lim_n(ii),lim_q(ii)
c	   end do
c	end if
c     read(10,*) n_char
c	do ii=1,n_char
c	   read(10,*) rev(ii),flow(ii),head(ii),eff(ii)
c	end do
c      close (unit=10)


c	call DLL_C_COEF_COMPR(nom_rev,max_rev,min_rev,chlim_q,
c     *                    	chlim_h,n_char,rev,flow,head,eff,nlim,
c     *                        lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
c     *                        alim,blim,clim,ier,iwr)

      call c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                  chlim_h,n_char,rev,flow,head,eff,nlim,
     *                  lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                  alim,blim,clim,ier,err_msg,iwr)
      if (ier.eq.0) then
         call calcola_cec(min_eff,cen,clim,cec) 
      end if

	do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_h_app(i)=lim_h(i)
	   alim_app(i)=alim(i)
	   blim_app(i)=blim(i)
	end do
	nlim_app=nlim

      call filtra_plim(nlim_app,lim_n_app,lim_q_app,lim_h_app,
     *                 alim_app,blim_app)
c   
c     Apre il file di output per il compressore
c
      open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
c
c     Scrive i valori di output ottenuti per regressione
c     
	write(20,*) 'Compressore analizzato:'
	write(20,*) compr
	write(20,*) 'Codice di errore :'
	write(20,*) ier
	write(20,*) 'Esito del calcolo:'
	write(20,*) err_msg
	write(20,*) 'Codice di warning :'
	write(20,*) iwr
	if (ier.eq.0) then
	 write(20,*) 'Coefficienti chn:'
	 write(20,*) chn
	 write(20,*) 'Coefficienti chc:'
	 write(20,*) chc
	 write(20,*) 'Coefficienti cen:'
	 write(20,*) cen
	 write(20,*) 'Coefficienti cec:'
	 write(20,*) cec
       write(20,*) 'Coefficienti alim:'
       write(20,*) alim(1:nlim+1)
	 write(20,*) 'Coefficienti blim:'
       write(20,*) blim(1:nlim+1)
	 write(20,*) 'Coefficienti clim:'
       write(20,*) clim
       if (flag_lim) then
          write(20,*) 'CURVA LIMITE SI'
	 else
          write(20,*) 'CURVA LIMITE NO'
	 end if
       write(20,*) 'Numero punti limite:'
	 write(20,*) nlim
       write(20,*) 'Valori limite REV:'
	 write(20,*) lim_n(1:nlim)
       write(20,*) 'Valori limite Q:'
	 write(20,*) lim_q(1:nlim)
       write(20,*) 'Valori limite H:'
	 write(20,*) lim_h(1:nlim)
       write(20,*) 'chlim_n'
	 write(20,*) chlim_n
       write(20,*) 'PROVA FILTRO PLIM:'
       write(20,*) 'Numero punti limite_app:'
	 write(20,*) nlim_app
       write(20,*) 'Valori limite REV_app:'
	 write(20,*) lim_n_app(1:nlim_app)
       write(20,*) 'Valori limite Q_app:'
	 write(20,*) lim_q_app(1:nlim_app)
       write(20,*) 'Valori limite H_app:'
	 write(20,*) lim_h_app(1:nlim_app)
       write(20,*) 'Coefficienti alim_app:'
       write(20,*) alim_app(1:nlim_app)
	 write(20,*) 'Coefficienti blim_app:'
       write(20,*) blim_app(1:nlim_app)
      end if
	close (unit=20)

c	i_name_file = trim(dat) //'/'// 'runits_t_new.txt' 
	o_name_file = trim(dat) //'/'// 'units_t_new.txt' 

c     Apre il file di input per la turbina

c	open(UNIT=30,FILE=i_name_file,
c     *     STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di una turbina
c
c	read(30,*) turb
c	read(30,*) nom_tair,nom_power,nom_hrate
c     read(30,*) ncorr
c	do jj=1,ncorr
c	   read(30,*) temp(jj),fpw(jj),fhr(jj)
c	end do
c     read(30,*) nmaxp
c	do jj=1,nmaxp
c	   read(30,*) rev_pwmax(jj),pwmax(jj),airpwm(jj)
c	end do
c      read(30,*) nhrate
c	do jj=1,nhrate
c	   read(30,*) rev_hrate(jj),power(jj),airhrt(jj),hrate(jj)
c	end do

c      close (unit=30)

	i_name_file = trim(dat) //'/'// 'turb_testa.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) turb,turb_id,turb_desc
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'turb_dcar.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) app,nom_rev,nom_power,nom_hrate,nom_tair
      close (unit=100)


	i_name_file = trim(dat) //'/'// 'turb_maxp.sies' 
   
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

c    Legge i valori di input per la regressione di un compressore

      read(100,*) nmaxp
	do jj=1,nmaxp
	   read(100,*) rev_pwmax(jj),pwmax(jj),airpwm(jj)
	end do
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'turb_hrate.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)

      read(100,*) nhrate
	do jj=1,nhrate
	   read(100,*) rev_hrate(jj),power(jj),hrate(jj),airhrt(jj)
	end do
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'turb_fcorr.sies' 
c    Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
      read(100,*) ncorr
	do jj=1,ncorr
	   read(100,*) fpw(jj),temp(jj),fhr(jj)
	end do
      close (unit=100)
	call c_coef_turb(nom_tair,nom_power,nom_hrate,
     *  	             ncorr,temp,fpw,fhr,nmaxp,rev_pwmax,pwmax,
     *                 airpwm,nhrate,rev_hrate,power,airhrt,hrate,
     *                 cpw,cpwt,chr,chrt,jer,error_msg)
c	call DLL_c_coef_turb(nom_tair,nom_power,nom_hrate,
c     *  	             ncorr,temp,fpw,fhr,nmaxp,rev_pwmax,pwmax,
c     *                 airpwm,nhrate,rev_hrate,power,airhrt,hrate,
c     *                 cpw,cpwt,chr,chrt,jer)

c     Apre il file di output per la turbina
c
      open(UNIT=40,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
c
c     Scrive i valori di output ottenuti per regressione
c     
	write(40,*) 'Turbina analizzata :'
	write(40,*) turb
	write(40,*) 'Codice di errore :'
	write(40,*) jer
	write(40,*) 'Esito del calcolo:'
	write(40,*) error_msg
	write(40,*) 'Coefficienti cpw:'
	write(40,*) cpw
	write(40,*) 'Coefficienti cpwt:'
	write(40,*) cpwt
	write(40,*) 'Coefficienti chr:'
	write(40,*) chr
      write(40,*) 'Coefficienti chrt:'
      write(40,*) chrt

      close (unit=40)
	
	
	
	call exit (0)  !return

9999      continue
      call exit (1)
c     aggiungere la gestione dell'errore legato al valore della variabile status

      END ! main_regressione

