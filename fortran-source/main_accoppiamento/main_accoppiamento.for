      program main_accoppiamento
c*************************************************************************************
c*************************************************************************************
c     Variables declarations
      implicit none
C
      integer*4 max_np,max_nt
	parameter (max_np=100,max_nt=100)
c     
c-----------------------------> INPUT <----------------------------------------
C
c------------------>CONDIZIONI OPERATIVE
	real*4 compos(10),!composizione del gas:(%)
	                  !1)meth(matano) 2)eth(etano) 3)prop(propano)
	                  !4)n_but(n-butano) 5)i_but(i-butano)
	                  !6)pent(pentano) 7)hexa(esano) 8)n2(azoto)
	                  !9)co2(anidride carbonica) 10)h2s(acido solfidrico)
     *      altitude,   !altezza slm della centrale (m)
     *      power_loss, !perdita di potenza dovuto all'invecchiamento (%)
     *      eff_corr    !correzione dell'efficienza (%)
c
      real*4 tin,    !temperatura d'ingresso del gas (K)
     *       tair    !temperatura dell'aria (K)
c     
      real*4 pin,    !pressione d'ingresso del gas (Pa)
     *       pout    !pressione d'uscita del gas (Pa)
	               !(viene assegnata o l'una o l'altra di queste grandezze,
	               !che avrà quindi valore positivo mentre l'altra sarà nulla)
C
c---------------------------> VARIABILI PER COMPRESSORE    
C
      real*4 max_rev  !numero di giri massimo (%)
     *      ,min_rev  !numero di giri minimo (%)
     *      ,min_eff
     *	  ,chn(6)   !coefficienti di h=f(n,q) nella zona normale 
     *	  ,chc(6)   !coefficienti di h=f(n,q) nella zona di choking 
     *	  ,cen(5)   !coefficienti di eff=f(n,q) nella zona normale 
     *	  ,cec(6)   !coefficienti di eff=f(n,q) nella zona di choking 
c
      integer*4 nlim !numero di punti limite sulla spezzata anti-sourge

c
      real*4 alim(21) !vettore dei termini noti della spezzata di surge
     *      ,blim(21) !vettore dei coefficienti angolari della spezzata di surge
     *      ,lim_n(20)!vettore dei valori estremi del numero di giri   
     *      ,lim_q(20)!vettore dei valori estremi della portata        
     *      ,lim_h(20)!vettore dei valori estremi della prevalenza           
c          (vettori di dimensione effettiva pari a nlim;grandezze dimensionali
c           lette in parte da maschera e in parte ottenute da regressione)
     *	  ,clim(4) !costanti delle curve limite del compressore
c
c---------------------------> VARIABILI PER TURBINA    
C
      real*4 cpw(3)    !coefficienti per il calcolo della potenza
     *      ,cpwt(4)   !coefficienti correttivi per la potenza 
     *      ,nom_rev   !numero di giri nominale (rpm)
     *      ,nom_power !potenza nominale (kW)
C
C-------------------------------->OUTPUT<-----------------------------------------
C
      logical*2 flag,flag_ch,flag_out
c
c------>punti curva max_power nella zona normale
c
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(max_np),ha(max_np), !coordinate HQ per disegnare il ramo A
	                              !(vettori di dim=na)
     *       qb(max_np),hb(max_np)  !coordinate HQ per disegnare il ramo B 
	                              !(vettori di dim=nb)
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(max_nt),      !numero di punti su ogni tronco del ramo A
     *       ptb(max_nt)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(max_np),un_q(max_np) !coordinate dei pti di raccordo 
                                       !(dim=ptr)
c
      integer*4 npoint(max_nt)
	real*4 qta(max_nt,max_np),
     *       hta(max_nt,max_np)
      integer*4 npoint_b(max_nt)
	real*4 qtb(max_nt,max_np),
     *       htb(max_nt,max_np)
c------>punti curva max_power nell'eventuale zona di choking
      integer*4 na_ch,      !numero di punti sul ramo A
     *          nb_ch       !numero di punti sul ramo B

      real*4 qa_ch(max_np),ha_ch(max_np), !coordinate HQ per disegnare il ramo A
	                    !(vettori di dim=na) dimensionali
     *       qb_ch(max_np),hb_ch(max_np)  !coordinate HQ per disegnare il ramo B 
	                    !(vettori di dim=nb) dimensionali
c
      integer*4 nta_ch,      !numero di tronchi sul ramo A
     *          ntb_ch       !numero di tronchi sul ramo B
c
      integer*4 pta_ch(max_np),      !numero di punti su ogni tronco del ramo A
     *       ptb_ch(max_np)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr_ch      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h_ch(max_np),un_q_ch(max_np)  !coordinate dei pti di raccordo 
                              !(dim=ptr) dimensionali
c
      integer*4 npoint_ch(max_nt)
	real*4 qta_ch(max_nt,max_np),
     *       hta_ch(max_nt,max_np)
      integer*4 npoint_b_ch(max_nt)
	real*4 qtb_ch(max_nt,max_np),
     *       htb_ch(max_nt,max_np)

      character*(120) ute,dat,log
      character*(150) i_name_file,o_name_file
	integer*4 status,k,it,ii,jj


	real*4 rev_hq(max_np),pow_hq(max_np)

      logical*2 flag_lim
      character*(80) compr  !compressore analizzato
	integer*4 max_n_char
	parameter (max_n_char=50)

      integer*4 n_char      !punti assegnati per effettuare la regressione
c                   
                          !grandezze relative ai punti assegnati:
      real*4 rev(max_n_char),       !numero di giri (dim=n_char,max_n_char=50)
     *     flow(max_n_char),      !portata (dim=n_char)
     *     head(max_n_char),      !altezza adiabatica (dim=n_char)
     *     eff(max_n_char)        !efficienza (dim=n_char)
c
      real*4 chlim_q,       !portata del punto limite di choking
     *     chlim_h,        !prevalenza del punto limite di choking
     *     chlim_n

      character err_msg*(200)!messaggio che descrive l'esito della regressione e
	                     !gli eventuali errori commessi
	integer*4 ier          !codice d'errore
	integer*4 iwr          !codice d'errore
	integer*4 max_ncorr,max_nmaxp,max_nhrate
	parameter (max_ncorr=50,max_nmaxp=50,max_nhrate=50)

      character*(80) turb    !nome della turbina analizzata
c
      real*4 nom_tair,         !valore nominale della temperatura dell'aria
     *	 nom_hrate         !valore nominale dell'heat-rate
c
c           Dati per la regressione dei coefficienti correttivi
c
      integer*4 ncorr       !punti assegnati,max_ncorr=50  
                            !grandezze relative a tali punti:
      real*4 temp(max_ncorr),        !temperatura;
     *     fpw(max_ncorr),         !fattore di correzione per la potenza fpw=f(temp)
     *     fhr(max_ncorr)          !fattore di correzione per l'heat rate fhr=f(temp)
	                      
c
c           Dati per la regressione dei coefficienti CPW   
c
      integer*4 nmaxp        !punti assegnati
                             !grandezze relative a tali punti:
	real*4 rev_pwmax(max_nmaxp), !numero di giri; 
     *     pwmax(max_nmaxp),     !potenza massima
     *     airpwm(max_nmaxp)     !temperatura dell'aria
c
c           Dati per la regressione dei coefficienti CHR
c
      integer*4 nhrate        !punti assegnati
                              !grandezze relative a tali punti:
	real*4 rev_hrate(max_nhrate), !numero di giri; 
     *     power(max_nhrate),     !potenza massima;
     *     airhrt(max_nhrate),    !temperatura dell'aria;
     *     hrate(max_nhrate)      !heat rate

      real*4 chr(6),      !coefficienti per il calcolo dell'heat rate:
	                  !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
	                  !         chr(5)*n^2+chr(6)*pw^2
     *     chrt(4)      !coefficienti per il calcolo del fattore correttivo 
	                  !per l'heat rate:
	                  !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
      character*(200) error_msg  !messaggio che descrive l'esito della regressione
                             !e gli eventuali errori commessi
c   
      integer jer            !indice d'errore
      character compr_id*(80),turb_id*(80)
     *           ,compr_desc*(80),turb_desc*(80)
c
	real nom_flow,nom_head,app

c************************************************************************************
c     Body of program

	call get_DatLog(dat,log,ute)

	i_name_file = trim(dat) //'/'// 'compr_testa.sies' 
c     Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
      read(100,*) compr,compr_id,compr_desc
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'compr_dcar.sies' 
c     Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
      read(100,*) app,nom_rev,nom_flow,nom_head,max_rev,min_rev,min_eff,
     *            chlim_q,chlim_h
      close (unit=100)


	i_name_file = trim(dat) //'/'// 'compr_pcar.sies' 
c    
c     Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
      read(100,*) n_char
	do ii=1,n_char
	   read(100,*) eff(ii),flow(ii),head(ii),rev(ii)
	end do
      close (unit=100)

	i_name_file = trim(dat) //'/'// 'compr_plim.sies' 
c     Apre il file di input per il compressore   
      open(UNIT=100,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
      read(100,*) nlim
	if (nlim.eq.0) then
	   flag_lim = .false.
	else

	   do ii=1,nlim
	     read(100,*) lim_n(ii),lim_q(ii)
	   end do
	end if
      close (unit=100)

c      Apre il file di input per il compressore
c      flag_lim = .true.
c	i_name_file = trim(dat) //'/'// 'runits_c.txt' 
c    
c     Apre il file di input per il compressore   
c      open(UNIT=40,FILE=i_name_file,
c     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
c      read(40,*) compr
c	read(40,*) nom_rev
c      read(40,*) max_rev,min_rev,min_eff
c      read(40,*) chlim_q,chlim_h
c      read(40,*) nlim
c	if (nlim.eq.0) then
c	   flag_lim = .false.
c	else
c	   do ii=1,nlim
c	      read(40,*) lim_n(ii),lim_q(ii)
c	   end do
c	end if
c      read(40,*) n_char
c	do ii=1,n_char
c	   read(40,*) rev(ii),flow(ii),head(ii),eff(ii)
c	end do
c     close (unit=40)

      call c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                  chlim_h,n_char,rev,flow,head,eff,nlim,
     *                  lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                  alim,blim,clim,ier,err_msg,iwr)
      call calcola_cec(min_eff,cen,clim,cec) 


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
c	i_name_file = trim(dat) //'/'// 'runits_t_new.txt' 

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
c      read(30,*) nmaxp
c	do jj=1,nmaxp
c	   read(30,*) rev_pwmax(jj),pwmax(jj),airpwm(jj)
c	end do
c      read(30,*) nhrate
c	do jj=1,nhrate
c	   read(30,*) rev_hrate(jj),power(jj),airhrt(jj),hrate(jj)
c	end do

c      close (unit=30)


	call c_coef_turb(nom_tair,nom_power,nom_hrate,
     *  	             ncorr,temp,fpw,fhr,nmaxp,rev_pwmax,pwmax,
     *                 airpwm,nhrate,rev_hrate,power,airhrt,hrate,
     *                 cpw,cpwt,chr,chrt,jer,error_msg)


	i_name_file = trim(dat) //'/'// 'input_link.txt' 
	o_name_file = trim(dat) //'/'// 'output_link.txt' 
c
c     Apre il file per la lettura degli input   
c
      open(UNIT=10,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di un compressore
c
c------------>condizioni operative
      read(10,*) compos
	read(10,*) altitude
      read(10,*) power_loss
      read(10,*) eff_corr
      read(10,*) tin
      read(10,*) tair
      read(10,*) pin
      read(10,*) pout
c------------>dati compressore
c      read(10,*) max_rev
c      read(10,*) min_rev
c      read(10,*) chn
c      read(10,*) chc
c      read(10,*) cen
c      read(10,*) cec
c      read(10,*) nlim
c      read(10,*) alim(1:nlim+1)
c      read(10,*) blim(1:nlim+1)
c      read(10,*) lim_n(1:nlim)
c      read(10,*) lim_q(1:nlim)
c      read(10,*) lim_h(1:nlim)
c      read(10,*) clim
c------------->dati turbina
c      read(10,*) cpw
c      read(10,*) cpwt
c      read(10,*) nom_rev
c      read(10,*) nom_power
c
      close (unit=10)
c
c      call dll_max_power(compos,altitude,power_loss,eff_corr,
c     *                    max_rev,min_rev,CHN,CHC,CEN,CEC,
c     *                    nlim,alim,blim,lim_n,lim_q,lim_h,
c     *                    clim,CPW,CPWT,nom_rev,nom_power,
c     *                    tin,tair,pin,pout,
c     *                    flag,flag_out,na,nb,qa,ha,qb,hb,
c     *                    nta,ntb,pta,ptb,ptr,un_q,un_h)
      call max_power_new(compos,altitude,power_loss,eff_corr,
     *                    max_rev,min_rev,CHN,CHC,CEN,CEC,
     *                    nlim,alim,blim,lim_n,lim_q,lim_h,
     *                    clim,CPW,CPWT,nom_rev,nom_power,
     *                    tin,tair,pin,pout,
     *                    flag,flag_out,na,nb,qa,ha,qb,hb,
     *                    nta,ntb,pta,ptb,ptr,un_q,un_h)
 
      if (flag) then
c           Apre il file per la scrittura degli output   
         open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
c      else
c         open(UNIT=20,FILE=o_name_file,
c     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
c	   write(20,*) 'Curva OUT RANGE'
c         close (unit=20)
c	   go to 9999
      else if ((.not.flag).and.(.not.flag_out))then
         open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
	   write(20,*) 'Curva OUT RANGE:buon accoppiamento'
         close (unit=20)
	   go to 9999
      else if ((.not.flag).and.(flag_out))then
         open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status,ERR=9999)
	   write(20,*) 'Curva OUT RANGE:accoppiamento impossibile'
         close (unit=20)
	   go to 9999
	end if
c
	write(20,*) 'Numero punti sul ramo A:'
	write(20,*) na
	write(20,*) 'Numero punti sul ramo B:'
	write(20,*) nb
	write(20,*) 'Ascisse ramo A:'
	write(20,*) qa(1:na)
	write(20,*) 'Ordinate ramo A:'
	write(20,*) ha(1:na)
	write(20,*) 'Ascisse ramo B:'
	write(20,*) qb(1:nb)
	write(20,*) 'Ordinate ramo B:'
	write(20,*) hb(1:nb)
      if (nta.gt.0) then
	    call tronco(na,nta,pta,qa,ha,npoint,qta,hta)
	    write(20,*) 'Numero di tronchi sul ramo A:'
	    write(20,*) nta
          do it=1,nta
	       k=npoint(it)
	       if (k.gt.0) then
                write(20,*) 'Punti tronco :'
                write(20,*) k
	          write(20,*) 'Ascisse :'
	          write(20,*) qta(it,1:k)
	          write(20,*) 'Ordinate :'
	          write(20,*) hta(it,1:k)
c-------->per studio su accoppiamento
                call revpow_da_hq(k,qta(it,1:k),hta(it,1:k),chn,
     *                            cpw,altitude,rev_hq,pow_hq)
	          write(20,*) 'CURVA LINK SUL PIANO DELLA TURBINA'
	          write(20,*) 'Numero giri:'
	          write(20,*) rev_hq(1:k)
	          write(20,*) 'Potenza:'
	          write(20,*) pow_hq(1:k)
c-elena
	       end if
	    end do
	end if
      if (ntb.gt.0) then
	    call tronco(nb,ntb,ptb,qb,hb,npoint_b,qtb,htb)
	    write(20,*) 'Numero di tronchi sul ramo B:'
	    write(20,*) ntb
          do it=1,ntb
	       k=npoint_b(it)
	       if (k.gt.0) then
                write(20,*) 'Punti tronco :' 
                write(20,*) k
	          write(20,*) 'Ascisse :'
	          write(20,*) qtb(it,1:k)
	          write(20,*) 'Ordinate :'
	          write(20,*) htb(it,1:k)
c-------->per studio su accoppiamento
                call revpow_da_hq(k,qtb(it,1:k),htb(it,1:k),chn,
     *                            cpw,altitude,rev_hq,pow_hq)
	          write(20,*) 'CURVA LINK SUL PIANO DELLA TURBINA'
	          write(20,*) 'Numero giri:'
	          write(20,*) rev_hq(1:k)
	          write(20,*) 'Potenza:'
	          write(20,*) pow_hq(1:k)
c-elena

	       end if
	    end do
	end if

	write(20,*) 'Punti nel vettore dei raccordi:'
      write(20,*) ptr
      write(20,*) 'Ascisse punti di raccordo:'
	write(20,*) un_Q(1:ptr)
      write(20,*) 'Ordinate punti di raccordo:'
	write(20,*) un_H(1:ptr)
c
c      if ((clim(3).gt.clim(4)).and.(flag_ch)) then
c	   write(20,*) 'ZONA DI CHOKING'
c	   write(20,*) 'Numero punti sul ramo A:'
c	   write(20,*) na_ch
c	   write(20,*) 'Numero punti sul ramo B:'
c	   write(20,*) nb_ch
c	   write(20,*) 'Ascisse ramo A:'
c	   write(20,*) qa_ch(1:na_ch)
c	   write(20,*) 'Ordinate ramo A:'
c	   write(20,*) ha_ch(1:na_ch)
c	   write(20,*) 'Ascisse ramo B:'
c	   write(20,*) qb_ch(1:nb_ch)
c	   write(20,*) 'Ordinate ramo B:'
c	   write(20,*) hb_ch(1:nb_ch)
c     if (nta_ch.gt.0) then
c	    call tronco(na_ch,nta_ch,pta_ch,qa_ch,ha_ch,npoint_ch,
c     *             qta_ch,hta_ch)
c	    write(20,*) 'Numero di tronchi sul ramo A:'
c	    write(20,*) nta_ch
c          do it=1,nta_ch
c	       k=npoint_ch(it)
c	       if (k.gt.0) then
c                write(20,*) 'Punti tronco:'
c                write(20,*) k
c	          write(20,*) 'Ascisse :'
c	          write(20,*) qta_ch(it,1:k)
c	          write(20,*) 'Ordinate :'
c	          write(20,*) hta_ch(it,1:k)
c-------->per studio su accoppiamento
c                call revpow_da_hq(k,qta_ch(it,1:k),hta_ch(it,1:k),chc,
c     *                            cpw,altitude,rev_hq,pow_hq)
c	          write(20,*) 'CURVA LINK SUL PIANO DELLA TURBINA'
c	          write(20,*) 'Numero giri:'
c	          write(20,*) rev_hq(1:k)
c	          write(20,*) 'Potenza:'
c	          write(20,*) pow_hq(1:k)
c-elena
c
c	       end if
c	    end do
c	end if
c     if (ntb_ch.gt.0) then
c	    call tronco(nb_ch,ntb_ch,ptb_ch,qb_ch,hb_ch,npoint_b_ch,
c     *                qtb_ch,htb_ch)
c	    write(20,*) 'Numero di tronchi sul ramo B:'
c	    write(20,*) ntb_ch
c          do it=1,ntb_ch
c	       k=npoint_b_ch(it)
c	       if (k.gt.0) then
c                write(20,*) 'Punti tronco :' 
c                write(20,*) k
c	          write(20,*) 'Ascisse :'
c	          write(20,*) qtb_ch(it,1:k)
c	          write(20,*) 'Ordinate :'
c	          write(20,*) htb_ch(it,1:k)
c-------->per studio su accoppiamento
c                call revpow_da_hq(k,qtb_ch(it,1:k),htb_ch(it,1:k),chc,
c     *                            cpw,altitude,rev_hq,pow_hq)
c	          write(20,*) 'CURVA LINK SUL PIANO DELLA TURBINA'
c	          write(20,*) 'Numero giri:'
c	          write(20,*) rev_hq(1:k)
c	          write(20,*) 'Potenza:'
c	          write(20,*) pow_hq(1:k)
c
c	       end if
c	    end do
c	end if
c        write(20,*) 'Ascisse punti di raccordo:'
c	   write(20,*) un_q_ch(1:ptr_ch)
c         write(20,*) 'Ordinate punti di raccordo:'
c	   write(20,*) un_h_ch(1:ptr_ch)
c      end if
      close (unit=20)
c   
	call exit (0)  !return
9999      continue
      call exit (1)
c
      end   !main_accoppiamento
