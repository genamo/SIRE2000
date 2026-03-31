c=======================================================================c
      Program main_analisi_statistica
c=======================================================================c
c                    main di esempio
c=======================================================================c
	implicit none

c     Variabili locali
      character*(150) i_name_file,o_name_file
c
      character*(120) ute,dat,log
c
      integer*4 unit,status
c
      integer*4 max_npoint
	parameter (max_npoint=50)
c
      integer max_nlim
	parameter (max_nlim=20)
c
c        Variabili per compressori
c
      character compr*(16),   !compressore analizzato
     *          comment*(60)  !commento sul compressore analizzato
c
      real*4 nom_rev,          !numero di giri
     *       nom_flow,         !portata
     *       nom_head          !altezza adiabatica
c
      real*4 max_rev,          !numero di giri massimo
     *       min_rev,          !numero di giri minimo
     *       min_eff           !efficienza minima
c
      integer*4 npoint         !punti di lavoro assegnati
c
                                  !grandezze relative ai punti assegnati:
	real*4 rev(max_npoint),     !numero di giri
     *       flow(max_npoint),    !portata
     *       head(max_npoint),    !altezza adiabatica
     *       eff(max_npoint)      !efficienza
	                            !(vettori di dimensione npoint)
c
      integer*4 nlim           !punti limite assegnati
c
                               !grandezze relative ai punti limite assegnati
      real*4 lim_n(max_nlim),  !numero di giri
     *       lim_q(max_nlim),  !portata
     *       lim_h(max_nlim)   !altezza adiabatica
	                         !(vettori di dimensione nlim)
c
                             !grandezze relative alla spezzata anti-surge:
      real*4 alim(max_nlim+1), !termini noti
     *       blim(max_nlim+1)  !coefficienti angolari
                             !(vettori di dimensione nlim+1)
c
      real*4 clim(4)           !costanti delle curve limite 
	                         !zona di choking per l'efficienza:
	                         !(1)curva di choking,(2)curva operating limit
	                         !zona di choking per i giri:
	                         !(1)curva di choking,(2)curva operating limit
c
                             !grandezze relative al punto limite di choking:
	real*4 chlim_n,          !numero di giri
     *       chlim_q,          !portata
     *       chlim_h           !altezza adiabatica
c
      real*4 chn(6),           !coeff di h=f(n,q) nella zona normale (dim=6)
     *       chc(6),           !coeff di h=f(n,q) nella zona di choking (dim=6)
     *       cen(5)            !coeff di eff=f(n,q) nella zona normale (dim=5)
c-------------------->OUTPUT
      real*4 cec(6)            !coefficienti di eff=f(n,q) nella zona di choking
c                            !dim=6
	                       !grandezze relative ai punti calcolati:
      real*4 new_rev(max_npoint),       !numero di giri
     *     new_eff(max_npoint)        !efficienza      (dim=npoint)
c
                             !valori statistici:
	real*4 sq1_r,            !deviazione standard per i giri
     *     sq2_r,            !scostamento medio per i giri
     *     sq1_e,            !deviazione standard per l'efficienza
     *     sq2_e             !scostamento medio per l'efficienza
c
      real*4 delta(max_npoint),            !differenza giri assegnati e calcolati
     *     delta_eff(max_npoint)         !differenza efficienza assegnata e calcolata

      real*4 delta_max,        !max scostamento percentuale per i giri
     *     delta_max_eff     !max scostamento percentuale per l'efficienza
c
      integer*4 idelta_max,    !indice del punto col max scostamento per i giri
     *        idelta_max_eff !indice del punto col max scostamento per l'eff

      character err_msg*(200)!messaggio che descrive l'esito della regressione e
	                     !gli eventuali errori commessi
	integer*4 ier          !codice d'errore
	integer*4 iwr          !codice d'errore
	integer*4 i

c
c      Variabili per turbine
c
      character turb*(16),      !turbina analizzata
     *          comment_t*(60)    !commento sulla turbina analizzata
c
c                              !valori nominali:
      real*4 nom_rev_t,            !numero di giri
     *       nom_power,          !potenza massima
     *       nom_hrate,
     *       nom_tair            !temperatura dell'aria
c
                           !Tabella delle correzioni
c
      integer*4 ncorr        !numero di punti assegnati      
c
                                    !grandezze relative ai punti assegnati:
      real*4 temp_cor(max_npoint),    !temperatura
     *       pw_cor(max_npoint),      !fattore di correzione per la potenza
     *       hrate_cor(max_npoint)    !fattore di correzione per l'hrate
      	                          !(vettori di dimensione ncorr)
c
                           !Tabella della potenza
c 
      integer*4 npwmax       !numero di punti assegnati
c
                                    !grandezze relative ai punti assegnati:
      real*4 rev_pwmax(max_npoint),   !numero di giri
     *       pwmax(max_npoint),       !massima potenza
     *       temp_pwmax(max_npoint)   !temperatura
                                    !(vettori di dimensione npwmax)

                           !Tabella dello heat rate
c
      integer*4 nhrate       !numero di punti assegnati
c
                                    !grandezze relative ai punti assegnati:
      real*4 rev_hrate(max_npoint),   !numero di giri
     *       pw_hrate(max_npoint),    !massima potenza
     *       temp_hrate(max_npoint),  !temperatura
     *       hrate(max_npoint)        !heat rate
                                    !(vettori di dimensione nhrate)

c
      real*4 cpw(3),      !coefficienti per il calcolo della potenza massima:
                        !pw(n)=cpw(1)+cpw(2)*n+cpw(3)*n^2 
     *       cpwt(4),     !coefficienti per il calcolo del fattore correttivo
                        !per la potenza:
                        !fpw(t)=cpwt(1)+cpwt(2)*t+cpwt(3)*t^2+cpwt(4)*t^3
     *       chr(6),      !coefficienti per il calcolo dell'heat rate:
                        !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
                        !         chr(5)*n^2+chr(6)*pw^2   
     *       chrt(4)      !coefficienti per il calcolo del fattore correttivo 
                        !per l'heat rate:
                        !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
c

      integer*4 ii,codice

      real*4 new_pwm(50),     !massima potenza 
	                          !(fisso temperatura e numero di giri)
     *     new_hrate(50),    !heat rate
	                          !(fisso temperatura,numero di giri e potenza)
     *     new_fpw(50),     !correzione potenza  
     *     new_fhr(50),      !correzione heat rate

     *     delta_p(50),
     *     delta_hr(50),
     *     delta_fpw(50),   !differenza valore di fpw calcolato e misurato
     *     delta_fhr(50)   !differenza valore di fhr calcolato e misurato

c
      real*4 st_dev_pwmax,        !deviazione standard per la massima potenza
     *     st_dev_hrate         !deviazione standard per lo heat rate

c
c*******************************************************************************
c
c------------------------------->COMPRESSORI<-----------------------------------
c     Apre il file di input per il compressore
c
      call get_DatLog(dat,log,ute)
c	i_name_file = trim(dat) //'/'//'dati_stat_c.txt'
	i_name_file = trim(dat) //'/'//'runits_c.txt'
	o_name_file = trim(dat) //'/'//'alfanum_c.txt'

c
	unit = 10                                  
	open(UNIT=unit,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status)

c
c     Legge i valori di input per il compressore
c
      read(unit,*) compr,comment
	read(unit,*) nom_rev,nom_flow,nom_head
      read(unit,*) max_rev,min_rev,min_eff
      read(unit,*) chlim_q,chlim_h
      read(unit,*) nlim
	do ii=1,nlim
	   read(unit,*) lim_n(ii),lim_q(ii)
	end do
      read(unit,*) npoint
	do ii=1,npoint
	   read(unit,*) rev(ii),flow(ii),head(ii),eff(ii)
	end do
c      read(unit,*) chn(1:6)
c	read(unit,*) chc(1:6)
c      read(unit,*) cen(1:5)
c      read(unit,*) alim(1:nlim)
c      read(unit,*) blim(1:nlim)
c	read(unit,*) clim(1:4)
c      read(unit,*) lim_h(1:nlim)
c	read(unit,*) chlim_n
      close (unit)

      call c_coef_compr(nom_rev,max_rev,min_rev,chlim_q,
     *                  chlim_h,npoint,rev,flow,head,eff,nlim,
     *                  lim_n,lim_q,chlim_n,lim_h,chn,chc,cen,
     *                  alim,blim,clim,ier,err_msg,iwr)
      call filtra_plim(nlim,lim_n,lim_q,lim_h,alim,blim)

c      call dll_dati_compr(nom_rev,nom_flow,nom_head,max_rev,
c     *                   	min_rev,min_eff,npoint,rev,flow,head,eff,
c     *                      nlim,lim_n,lim_q,lim_h,alim,blim,clim,
c     *                      chlim_n,chlim_q,chlim_h,chn,chc,cen,
c     *                      cec,new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e,
c     *                      delta,delta_eff,delta_max,delta_max_eff,
c     *                      idelta_max,idelta_max_eff,codice) 
      call dati_compr(nom_rev,nom_flow,nom_head,max_rev,
     *                   	min_rev,min_eff,npoint,rev,flow,head,eff,
     *                      nlim,lim_n,lim_q,lim_h,alim,blim,clim,
     *                      chlim_n,chlim_q,chlim_h,chn,chc,cen,
     *                      cec,new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e,
     *                      delta,delta_eff,delta_max,delta_max_eff,
     *                      idelta_max,idelta_max_eff,codice) 
c     Scrive il file di output per il compressore
c
c	unit =11
c
c      call print_c(unit,o_name_file,compr,comment,nom_rev,nom_flow,
c     *            nom_head,max_rev,min_rev,min_eff,npoint,rev,flow,head,
c     *            eff,nlim,lim_n,lim_q,lim_h,alim,blim,clim,
c     *            chlim_n,chlim_q,chlim_h,chn,chc,cen)

      open(UNIT=11,FILE=o_name_file,
     *     	STATUS='replace',ACTION='write',IOSTAT=status)
c
c     Scrive i valori di output ottenuti per regressione
c     
	write(11,*) 'Giri assegnati' 
	   write(11,*) rev(1:npoint)
	write(11,*) 'Giri calcolati' 	    
	   write(11,*) new_rev(1:npoint) 
	write(11,*) 'Delta giri' 	    
	   write(11,*) delta(1:npoint) 
	write(11,*) 'Delta giri max' 	    
	   write(11,*) delta_max
	write(11,*) 'Indice delta giri max' 	    
	   write(11,*) idelta_max

	write(11,*) 'Efficienza assegnata' 
	   write(11,*) eff(1:npoint)
	write(11,*) 'Efficienza calcolata' 	    
	   write(11,*) new_eff(1:npoint) 
	write(11,*) 'Delta efficienza' 	    
	   write(11,*) delta_eff(1:npoint) 
	write(11,*) 'Delta eff max' 	    
	   write(11,*) delta_max_eff
	write(11,*) 'Indice Delta eff max' 	    
	   write(11,*) idelta_max_eff

	write(11,*) 'Codice:'
	write(11,*) codice
	if (Codice.eq.2) then
	   write(11,*) 'I punti introdotti non permettono'
	   write(11,*) '    di descrivere il compressore'
	   write(11,*) 'secondo il modello adottato.'
	else if (codice.eq.1) then
	   write(11,*) 'Grafici a supporto dell''utente'
	   write(11,*) 'I punti introdotti non permettono'
	   write(11,*) '    di descrivere il compressore'
	   write(11,*) 'secondo il modello adottato.'
	else
	   write(11,*) 'Tutto OK'
      end if


      close(unit)
c
c     Legge i valori di input per la turbina
c
	i_name_file = trim(dat) //'/'//'dati_stat_t.txt'
	o_name_file = trim(dat) //'/'//'alfanum_t.txt'
     
	unit=20
c
	open(UNIT=unit,FILE=i_name_file,
     *     	STATUS='old',ACTION='read',IOSTAT=status)
c
      read(unit,*) turb,comment_t
      read(unit,*) nom_tair,nom_power,nom_hrate,nom_rev_t
      read(unit,*) ncorr
      do ii=1,ncorr
         read(unit,*) temp_cor(ii),pw_cor(ii),hrate_cor(ii)
	end do
      read(unit,*) npwmax
	do ii=1,npwmax
         read(unit,*) rev_pwmax(ii),pwmax(ii),temp_pwmax(ii)
	end do
      read(unit,*) nhrate
	do ii=1,nhrate
	   read(unit,*) rev_hrate(ii),pw_hrate(ii),temp_hrate(ii),hrate(ii)
	end do
      read(unit,*) cpw(1:3)
      read(unit,*) cpwt(1:4)
      read(unit,*) chr(1:6)
      read(unit,*) chrt(1:4)
c
      close (unit)
c

c-prova
      call dll_dati_turb(nom_power,nom_hrate,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *            ncorr,temp_cor,pw_cor,hrate_cor,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            new_fpw,new_fhr,
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
     *            delta_fpw,delta_fhr)
c-prova


c     Scrive il file di output per la turbina
c
      unit=21
c
      call print_t(unit,o_name_file,turb,comment_t,nom_rev_t,nom_power,
     *            nom_hrate,nom_tair,ncorr,temp_cor,pw_cor,hrate_cor,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *            cpw,cpwt,chr,chrt)
c
      call exit(0)    !return
      END ! main_analisi_statistica

