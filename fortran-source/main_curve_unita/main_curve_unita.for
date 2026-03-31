      program main_curve_unita
c===============================================================================================
c ------------>  - Parte I -
c     Calcola le coordinate (dimensionali) dei punti utilizzati per disegnare 
c     le curve caratteristiche di un compressore nel piano QH(sull'asse delle ascisse
c     varia la portata (Q) mentre sull'asse delle ordinate varia l'altezza adiabatica (H).
c ------------>  - Parte II -
c     calcola le coordinate (dimensionali) dei punti utilizzati per disegnare 
c     le curve caratteristiche di una turbina nel piano RP(sull'asse delle ascisse 
c     varia il numero di giri(rev) mentre sull'asse delle ordinate varia la potenza(power))
c     e per disegnare l'andamento dei fattori ccrrettivi in funzione della temperatura.
c
c     I file di I/O sono contenuti nella cartella D:\sire_dati\dat
c===============================================================================================
      implicit none
      include '../inc/rk_param.inc'
c
      integer*4 max_ip
	parameter (max_ip=10)
	

      real*4 max_rev  !numero di giri massimo
     *    ,min_rev  !numero di giri minimo 
c
c---------------------------> VARIABILI PER COMPRESSORI    
      character*(80) compr,compr_id,compr_desc  !compressore analizzato
c
      real*4 nom_rev  !valore nominale del numero di giri
     *    ,min_eff  !efficienza minima
     *	,chn(6)   !coefficienti di h=f(n,q) nella zona normale 
     *	,chc(6)   !coefficienti di h=f(n,q) nella zona di choking 
     *	,cen(5)   !coefficienti di eff=f(n,q) nella zona normale 
c
      logical*2 flag_lim !curva limite teorica SI/NO
      integer*4 nlim !numero di punti limite assegnati sulla spezzata anti-sourge
c
      real*4 alim(max_nlim+1) !vettore dei termini noti della spezzata di surge
     *    ,blim(max_nlim+1) !vettore dei coefficienti angolari della spezzata di 
	             !                                                     surge 
     *	,clim(4) !costanti delle curve limite del compressore
     *    ,lim_n(max_nlim)!vettore dei valori estremi del numero di giri   
     *    ,lim_q(max_nlim)!vettore dei valori estremi della portata        
     *    ,lim_h(max_nlim)!vettore dei valori estremi della prevalenza           
c
      integer*4 np   !numero di punti da calcolare sulle singole curve 
     *       ,ip   !numero di punti sulla curva teorica di anti-surge
c
      real*4 qs(max_nlim)  !ascisse dei pti sulla curva anti-surge
     *	,hs(max_nlim)  !ordinate dei pti sulla curva anti-surge
     *    ,qst(max_ip)   !ascisse dei pti sulla curva anti-surge teorica 
     *    ,hst(max_ip)   !ordinate dei pti sulla curva anti-surge teorica
     *    ,qc(max_np)  !ascisse dei pti sulla curva anti-choke
     *    ,hc(max_np)  !ordinate dei pti sulla curva anti-choke
     *    ,qol(max_np) !ascisse dei pti sulla curva operating limit
     *	,hol(max_np) !ordinate dei pti sulla curva operating limit
c
      integer*4 ngiri   !curve a numero di giri costante da disegnare
c
      real*4 giri(max_ngiri) !valori costanti del numero di giri da graficare 
c
      real*4 qrn(max_ngiri,max_np)!ascisse pti su curve a rev cost (zona normale) 
     *    ,hrn(max_ngiri,max_np)!ordinate pti su curve a rev cost (zona normale)
     *    ,qrc(max_ngiri,max_np)!ascisse pti su curve a rev cost (zona choking)
     *    ,hrc(max_ngiri,max_np)!ordinate pti su curve a rev cost (zona choking)
c
      integer*4 neff    !curve a efficienza costante da disegnare
c
      real*4 effic(max_neff) !valori dell'efficienza calcolati 
c
      integer*4 ngiri_qe
      real*4 giri_qe(max_ngiri)
c
      real*4 qef1(max_neff,max_np)!ascisse dei pti su curve a eff cost(1°set pt)
     *    ,hef1(max_neff,max_np)!ordinate dei pti su curve a eff cost
     *    ,qef2(max_neff,max_np)!ascisse dei pti su curve a eff cost(2°set pt)
     *    ,hef2(max_neff,max_np)!ordinate dei pti su curve a eff cost
     *    ,ef_q(max_ngiri,max_np)!ascisse per l'eff nel piano Q-eta
     *    ,eta(max_ngiri,max_np) !ordinate per l'eff nel piano Q-eta
     *    ,qmini                !limite minimo di q sull'asse delle ascisse
     *	,qmaxi                !limite massimo di q sull'asse delle ascisse
     *    ,hmini                !limite minimo di h sull'asse delle ordinate
     *    ,hmaxi                !limite massimo di h sull'asse delle ordinate

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
	real*4 app,nom_flow,nom_head
c
c
c
c---------------------------> VARIABILI PER TURBINE
c
	integer*4 max_ncorr,max_nmaxp,max_nhrate
	parameter (max_ncorr=50,max_nmaxp=50,max_nhrate=50)
	integer*4 nhr
c	
      integer*4 npt     !numero di punti da calcolare
c
      character*(15) turb   !turbina analizzata
c
      real*4 nom_power !valore nominale della potenza
     *    ,nom_hrate !valore nominale dell'hrate
     *    ,nom_tair

      real*4 cpw(3)    !coefficienti per il calcolo della potenza
     *    ,chr(6)    !coefficienti per il calcolo dell'heat-rate
     *    ,cpwt(4)   !coefficienti correttivi per la potenza 
     *    ,chrt(4)   !coefficienti correttivi per l'hrate 
c
      real*4 rev_pw(max_np),          !ascisse
     *     ppwmax(max_np),          !ordinate
c                               curva ad heat rate costante
     *     rev_hr_up(max_nhr,max_np),  !ascisse(1°set)
     *     rev_hr_dw(max_nhr,max_np),  !ascisse(2°set) 
     *     pwup(max_nhr,max_np),    !ordinate(1°set)
     *     pwdw(max_nhr,max_np)     !ordinate(2°set)
c                            
c
      real*4 temp(max_np) !ascisse dei punti calcolati per disegnare l'andamento  
	                  !dei fattori correttivi (temperature calcolate)
     *    ,fac_hr(max_np)!ordinate dei punti calcolati per disegnare l'andamento  
                        !del fattore correttivo per l'heat rate
     *    ,fac_pw(max_np) !ordinate dei punti calcolati per disegnare l'andamento 
                         !del fattore correttivo per la potenza
     *    ,hr(7) !valori costanti dell'hr da graficare

      integer*4 nmaxp          ! punti a disposizione per il calcolo dei CPW          
c                              ! grandezze relative ai punti osservati a disposizione:
      real*4 rev_pwmax(max_nmaxp)  ! numero di giri alla potenza massima; 
     *      ,pwmax(max_nmaxp)      ! potenza massima;
     *	  ,airpwm(max_nmaxp)     ! potenza massima alla temperatura dell'aria.
c
      integer*4 nhrate         ! punti a disposizione per il calcolo dei CHR                               
	                         ! grandezze relative ai punti osservati a disposizione:
	real*4 rev_hrate(max_Nhrate) ! numero di giri; 
     *      ,power(max_Nhrate)     ! potenza;
     *      ,airhrt(max_nhrate)    ! heat rate alla temperatura dell'aria;
     *      ,hrate(max_nhrate)
c                  
      real*4 pwmax_t(max_nmaxp),        !potenza alla temperatura di graficazione
     *       power_t(max_nhrate)
      integer*4 ncorr       !punti assegnati,max_ncorr=50  
                            !grandezze relative a tali punti:
      real temp_fact(max_ncorr),        !temperatura;
     *     fpw(max_ncorr),         !fattore di correzione per la potenza fpw=f(temp)
     *     fhr(max_ncorr),          !fattore di correzione per l'heat rate fhr=f(temp)
     *     new_fpw(max_ncorr),         !fattore di correzione per la potenza fpw=f(temp)
     *     new_fhr(max_ncorr)          !fattore di correzione per l'heat rate fhr=f(temp)
c-
c
      integer*4 i,j,ii,status,jj
c
      character*(120) ute,dat,log
      character*(150) i_name_file,o_name_file

	real*4 tmin,tmax
      character*(200) error_msg  !messaggio che descrive l'esito della regressione
                             !e gli eventuali errori commessi
c   
      integer jer            !indice d'errore
      real*4 t,fpw_t,fhr_t
c************************************************************************************************
c
c------------------------------->COMPRESSORI<--------------------------------------------
	call get_DatLog(dat,log,ute)
      flag_lim = .true.
c
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
	o_name_file = trim(dat) //'/'// 'out_compr_pt.txt' 
c    
c     Apre il file di input per il compressore   
c      open(UNIT=10,FILE=i_name_file,
c     *     	STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
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
c      read(10,*) n_char
c	do ii=1,n_char
c	   read(10,*) rev(ii),flow(ii),head(ii),eff(ii)
c	end do
c      close (unit=10)

      call c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                  chlim_h,n_char,rev,flow,head,eff,nlim,
     *                  lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                  alim,blim,clim,ier,err_msg,iwr)


      np=20
      call compr_pt(nom_rev,max_rev,min_rev,min_eff,chn,chc,
     *              cen,flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
     *              np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
     *              qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
     *              ef_q,eta,qmini,qmaxi,hmini,hmaxi)
c      call dll_compr_pt(nom_rev,max_rev,min_rev,min_eff,chn,chc,
c     *              cen,flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
c     *              np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
c     *              qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
c     *              ngiri_qe,giri_qe,ef_q,eta,qmini,qmaxi,hmini,hmaxi)
c     *              ef_q,eta,qmini,qmaxi,hmini,hmaxi)

c      Apre il file di output per il compressore
c

      open(UNIT=20,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status)

c      Scrive i valori di output ottenuti per regressione
     
	write(20,*) 'Compressore analizzato:'
	write(20,*) compr
	write(20,*) 'Punti sulla curva anti-surge'
	write(20,*) 'Ascisse:'
      write(20,*) qs(1:nlim)     
	write(20,*) 'Ordinate:'
	write(20,*) hs(1:nlim)
c	if (flag_lim) then
		write(20,*) 'Punti sulla curva teorica di anti-surge'
		write(20,*) 'Ascisse:'
		write(20,*) qst(1:ip)     
		write(20,*) 'Ordinate:'
		write(20,*) hst(1:ip)
c	end if
      write(20,*) 'Punti sulla curva anti-choke'
	write(20,*) 'Ascisse:'
	write(20,*) qc(1:np)
	write(20,*) 'Ordinate:'
	write(20,*) hc(1:np)
	if (clim(3).ne.clim(4)) then
	   write(20,*) 'Punti sulla curva operating limit'
	   write(20,*) 'Ascisse:'
	   write(20,*) qol(1:np)
	   write(20,*) 'Ordinate:'
	   write(20,*) hol(1:np)
	else
	   write(20,*)'Le curve anti-choke e operating limit coincidono.'
	   write(20,*)'Non è definita la zona di choking.'
	end if
	write(20,*) 'Curve a numero di giri costante da graficare:'
	write(20,*) ngiri
      do i=1,ngiri
	   write(20,*) 'rev='
         write(20,*) giri(i) 
	   write(20,*) 'zona normale'
         write(20,*) 'Ascisse:'
         write(20,*) qrn(i,1:np)
         write(20,*) 'Ordinate:'
         write(20,*) hrn(i,1:np)
	   if(clim(3).ne.clim(4)) then
	      write(20,*) 'zona di choking'
            write(20,*) 'Ascisse:'
            write(20,*) qrc(i,1:np)
            write(20,*) 'Ordinate:'
            write(20,*) hrc(i,1:np)
	   end if
	end do
      write(20,*) 'Curve a efficienza costante da graficare:'
      write(20,*) neff
	do ii=1,neff
	   write(20,*) 'Efficienza(%)='
	   write(20,*) effic(ii)
	   write(20,*) 'Ascisse (1°set):'
	   write(20,*) qef1(ii,1:max_np)
	   write(20,*) 'Ordinate (1°set):' 
	   write(20,*) hef1(ii,1:max_np)
	   write(20,*) 'Ascisse (2°set):'
	   write(20,*) qef2(ii,1:max_np)
	   write(20,*) 'Ordinate (2°set):' 
	   write(20,*) hef2(ii,1:max_np)
	end do
	write(20,*) 'Efficienza nel piano Q-eta:'
      do i=1,ngiri
	   write(20,*) 'eff(%)='
         write(20,*) giri(i) 
         write(20,*) 'Ascisse:'
         write(20,*) ef_q(i,1:np)
         write(20,*) 'Ordinate:'
         write(20,*) eta(i,1:np)
	end do
	write(20,*) 'Valori limite per scalare gli assi'
	write(20,*) 'Portata minima:'
	write(20,*) qmini
	write(20,*) 'Portata massima:'
	write(20,*) qmaxi 
	write(20,*) 'Prevalenza minima:'
	write(20,*) hmini
	write(20,*) 'Prevalenza massima:'
	write(20,*) hmaxi
c
      close (unit=20)
c
c---------------------------------->TURBINA<--------------------------------------------
c
c     Apre il file di input per la turbina
c


	call get_DatLog(dat,log,ute)
	i_name_file = trim(dat) //'/'// 'runits_t_new.txt' 
c	i_name_file = trim(dat) //'/'// 'in_turb_pt_new.txt' 
	o_name_file = trim(dat) //'/'//'out_turb_pt_new.txt' 

c     Apre il file di input per la turbina

	open(UNIT=30,FILE=i_name_file,
     *     STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di una turbina
c
	read(30,*) turb
	read(30,*) nom_tair,nom_power,nom_hrate
      read(30,*) ncorr
	do jj=1,ncorr
	   read(30,*) temp_fact(jj),fpw(jj),fhr(jj)
	end do
      read(30,*) nmaxp
	do jj=1,nmaxp
	   read(30,*) rev_pwmax(jj),pwmax(jj),airpwm(jj)
	end do
      read(30,*) nhrate
	do jj=1,nhrate
	   read(30,*) rev_hrate(jj),power(jj),airhrt(jj),hrate(jj)
	end do

      close (unit=30)


	call c_coef_turb(nom_tair,nom_power,nom_hrate,
     *  	             ncorr,temp_fact,fpw,fhr,nmaxp,rev_pwmax,pwmax,
     *                 airpwm,nhrate,rev_hrate,power,airhrt,hrate,
     *                 cpw,cpwt,chr,chrt,jer,error_msg)
C	call DLL_C_COEF_TURB(nom_tair,nom_power,nom_hrate,
C     *  	             ncorr,temp_fact,fpw,fhr,nmaxp,rev_pwmax,pwmax,
C     *                 airpwm,nhrate,rev_hrate,power,airhrt,hrate,
C     *                 cpw,cpwt,chr,chrt,jer)
cc
	open(UNIT=30,FILE=i_name_file,
     *     STATUS='old',ACTION='read',IOSTAT=status,ERR=9999)
c
c     Legge i valori di input per la regressione di una turbina
c

	read(30,*) turb
	read(30,*) nom_tair,nom_power,nom_hrate
      read(30,*) ncorr
	do jj=1,ncorr
	   read(30,*) temp_fact(jj),fpw(jj),fhr(jj)
	end do
      read(30,*) nmaxp
	do jj=1,nmaxp
	   read(30,*) rev_pwmax(jj),pwmax(jj),airpwm(jj)
	end do
      read(30,*) nhrate
	do jj=1,nhrate
	   read(30,*) rev_hrate(jj),power(jj),airhrt(jj),hrate(jj)
	end do

      close (unit=30)

      tmin=268.15   !minima e massima temperatura di correzione
	tmax=318.15
	npt=50
      call turb_point(npt,nom_power,nom_hrate,cpw,cpwt,chr,chrt,
     *            tmin,tmax,
     *            rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
     *            pwup,pwdw,temp,fac_hr,fac_pw,hr)
c	call DLL_TURB_POINT(npt,nom_power,nom_hrate,cpw,cpwt,
c     *                          chr,chrt,
c     *                          tmin,tmax, 
c     *                          rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
c    *                          pwup,pwdw,temp,fac_hr,fac_pw,hr)

      call trans_turb(nom_power,cpwt,nmaxp,pwmax,airpwm,
     *                nhrate,power,airhrt,pwmax_t,power_t)
      call trans_fcorr(ncorr,temp_fact,fpw,fhr)

      do i=1,nhrate
        rev_hrate(i)=rev_hrate(i)*100.
	end do
      do i=1,nmaxp
        rev_pwmax(i)=rev_pwmax(i)*100.
	end do
c	t=1
c      fhr_t  = chrt(1)+t*(chrt(2)+t*(chrt(3)+chrt(4)*t))
c      fpw_t  = cpwt(1)+t*(cpwt(2)+t*(cpwt(3)+cpwt(4)*t))

      do i=1,ncorr
	   t=(temp_fact(i)+273.15)/288.15
	   new_fpw(i)=cpwt(1)+t*(cpwt(2)+t*(cpwt(3)+cpwt(4)*t))
	   new_fhr(i)=chrt(1)+t*(chrt(2)+t*(chrt(3)+chrt(4)*t))
	   new_fpw(i)=new_fpw(i)*100.
	   new_fhr(i)=new_fhr(i)*100.
	end do

c     Apre il file di output per la turbina
c
      open(UNIT=21,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status)
c
c     Scrive i valori di output ottenuti per regressione
c     
	write(21,*) 'Coefficienti cpwt:'
	write(21,*) cpwt
      write(21,*) 'Coefficienti chrt:'
      write(21,*) chrt
	write(21,*) 'Turbina analizzata :'
	write(21,*) turb
	write(21,*) 'Punti per la curva di massima potenza'
	write(21,*) 'Ascisse:'
	write(21,*) rev_pw(1:npt)
	write(21,*) 'Ordinate:'
	write(21,*) ppwmax(1:npt)
	write(21,*) 'Curve ad heat-rate costante'
	write(21,*) 'Ordinate:'
      do i=1,nhr
	   write(21,*) 'Ascisse 1:'
	   write(21,*) rev_hr_dw(i,1:npt)
         write(21,*) 'Ordinate 1 (pwdw)'
 	   write(21,*) pwdw(i,1:npt)
	   write(21,*) 'Ascisse 1:'
	   write(21,*) rev_hr_up(i,1:npt)
         write(21,*) 'Ordinate 2 (pwup)'
         write(21,*) pwup(i,1:npt)
      end do
      write(21,*) 'Fattori correttivi'
      write(21,*) 'Ascisse(temperature)'
	write(21,*) temp(1:npt)
      write(21,*) 'Correzione per heat rate'
      write(21,*) fac_hr(1:npt)
      write(21,*) 'Correzione per la potenza'
	write(21,*) fac_pw(1:npt)
      write(21,*) 'Punti da mappare (rev,pow)'
	do i=1,nmaxp
	   write(21,*) rev_pwmax(i),pwmax_t(i)
	end do
	do i=1,nhrate
	   write(21,*) rev_hrate(i),power_t(i)
	end do
      write(21,*) 'Fattori corr:assegnati-calcolati'
      write(21,*) 'Potenza'
	do i=1,ncorr
	   write(21,*) temp_fact(i),fpw(i),new_fpw(i)
	end do
      write(21,*) 'heat rate'
	do i=1,ncorr
	   write(21,*) temp_fact(i),fhr(i),new_fhr(i)
	end do

c
      close (unit=21)

      call exit(0) !return

9999      continue
      call exit (1)
      
c
	end
