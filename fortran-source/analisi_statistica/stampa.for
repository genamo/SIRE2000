      subroutine print_c(unit,file,compr,comment,nom_rev,nom_flow,
     *                  nom_head,max_rev,min_rev,min_eff,npoint,rev,
     *                  flow,head,eff,nlim,lim_n,lim_q,lim_h,alim,blim,
     *                  clim,chlim_n,chlim_q,chlim_h,chn,chc,cen)
************************************************************************************************
c     Stampa sul file indicato le grandezze caratteristiche di un compressore calcolate
c     a partire da un insieme di punti di lavoro assegnati,tabulati nel file suddetto.
************************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c--------------------->INPUT
c
      integer unit           !numero di unità da associare al file di output
c
      character file*(*),    !file di output
     *	      compr*(*),   !compressore analizzato
     *          comment*(*)  !commento sul compressore analizzato
c
      real nom_rev,          !numero di giri
     *     nom_flow,         !portata
     *     nom_head          !altezza adiabatica
c
      real max_rev,          !numero di giri massimo
     *     min_rev,          !numero di giri minimo
     *     min_eff           !efficienza minima
c
      integer npoint         !punti di lavoro assegnati
c
                             !grandezze relative ai punti assegnati:
	real rev(*),           !numero di giri
     *     flow(*),          !portata
     *     head(*),          !altezza adiabatica
     *     eff(*)            !efficienza
c
      integer nlim           !punti limite assegnati
c
                             !grandezze relative ai punti limite assegnati
      real lim_n(*),         !numero di giri
     *     lim_q(*),         !portata
     *     lim_h(*)          !altezza adiabatica
	                       !(vettori di dimensione nlim)
c
                             !grandezze relative alla spezzata anti-surge:
      real alim(*),          !termini noti
     *     blim(*)           !coefficienti angolari
                             !(vettori di dimensione nlim+1)
c
      real clim(*)           !costanti delle curve limite (dim=4)
	                       !zona di choking per l'efficienza:
	                       !(1)curva di choking,(2)curva operating limit
	                       !zona di choking per i giri:
	                       !(1)curva di choking,(2)curva operating limit
c
                             !grandezze relative al punto limite di choking:
	real chlim_n,          !numero di giri
     *     chlim_q,          !portata
     *     chlim_h           !altezza adiabatica
c
      real chn(*),           !coeff di h=f(n,q) nella zona normale (dim=6)
     *     chc(*),           !coeff di h=f(n,q) nella zona di choking (dim=6)
     *     cen(*)           !coeff di eff=f(n,q) nella zona normale (dim=5)
c
c----------------------->VARIABILI LOCALI
c
      real cec(6)            !coefficienti di eff=f(n,q) nella zona di choking
c
	                       !grandezze relative ai punti calcolati:
      real new_rev(npoint),  !numero di giri
     *     new_eff(npoint)   !efficienza
c
                             !valori statistici:
	real sq1_r,            !deviazione standard per i giri
     *     sq2_r,            !scostamento medio per i giri
     *     sq1_e,            !deviazione standard per l'efficienza
     *     sq2_e             !scostamento medio per l'efficienza
c
      integer i
c    
************************************************************************************************
c-old      min_eff=min_eff/percrif
c
c     Calcola CEC
c
      call calcola_cec(min_eff,cen,clim,cec)
c
c     Adimensionalizzazioni
c
      do i=1,nlim
	   lim_h(i)=lim_h(i)/headrif/1000.  !trasformo in km
	end do
c-17 gennaio
	alim(nlim+1)=alim(nlim)
	blim(nlim+1)=blim(nlim)
c-17 gennaio
c
	do i=1,npoint
         rev(i)=rev(i)/nom_rev
	   flow(i)=flow(i)/qmrif/1000.      !trasformo in km^3/h
	   if (head(i).eq.0) then
	      head(i)=-999.
  	   else 
	      head(i)=head(i)/headrif/1000. !trasformo in km
	   end if
	   if (eff(i).eq.0) then
	      eff(i)=-999.
	   else
c-old	      eff(i)=eff(i)/percrif
	      eff(i)=eff(i)
	   end if
	end do
c      
      call stat_c(npoint,head,flow,rev,eff,
     *            nlim,alim,blim,clim,lim_h,chn,chc,cen,cec,
     *            new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e)
c
c     Dimensionalizzazioni
c
      max_rev=max_rev*percrif
      min_rev=min_rev*percrif
      min_eff=min_eff*percrif
	nom_head=nom_head/1000.
	nom_flow=nom_flow/1000.
	chlim_q=chlim_q/1000.
	chlim_h=chlim_h/1000.
c
      do i=1,nlim
	   lim_h(i)=lim_h(i)*headrif
	   lim_q(i)=lim_q(i)/1000.
	end do
c
      do i=1,npoint
	   rev(i)=rev(i)*nom_rev
	   if (new_rev(i).ne.-999.) new_rev(i)=new_rev(i)*nom_rev
	   flow(i)=flow(i)*qmrif    ! stampa in km^3/h
	   if(head(i).ne.-999.) head(i)=head(i)*headrif ! stampa in km
	   if(eff(i).ne.-999.) eff(i)=eff(i)*percrif
	   if(new_eff(i).ne.-999.) new_eff(i)=new_eff(i)*percrif
	end do
c
      sq1_r=sq1_r*nom_rev
	sq2_r=sq2_r*nom_rev
	sq1_e=sq1_e*100.
	sq2_e=sq2_e*100.
c
      call stampa_c(unit,file,compr,comment,nom_rev,nom_flow,nom_head,
     *              max_rev,min_rev,min_eff,npoint,rev,flow,
     *              head,eff,new_rev,new_eff,nlim,lim_n,lim_q,
     *              lim_h,alim,blim,clim,chlim_n,chlim_q,chlim_h,
     *              chn,chc,cen,cec,sq1_r,sq2_r,sq1_e,sq2_e)     
c
	return
	end
c----------------------------------------------------------------------------------
      subroutine dati_compr(nom_rev,nom_flow,nom_head,max_rev,min_rev,
     *                      min_eff,npoint,rev,flow,head,eff,
     *                      nlim,lim_n,lim_q,lim_h,alim,blim,clim,
     *                      chlim_n,chlim_q,chlim_h,chn,chc,cen,
     *                      cec,new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e,
     *                      delta,delta_eff,delta_max,delta_max_eff,
     *                      idelta_max,idelta_max_eff,codice) 

c-----------------------------------------------------------------------------------
c     calcola i valori statistici relativi ad un compressore
c------------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'
c-------->INPUT
      real*4 nom_rev,          !numero di giri
     *     nom_flow,         !portata
     *     nom_head          !altezza adiabatica
c
      real*4 max_rev,          !numero di giri massimo
     *     min_rev,          !numero di giri minimo
     *     min_eff           !efficienza minima
c
      integer*4 npoint         !punti di lavoro assegnati
c
                             !grandezze relative ai punti assegnati:
	real*4 rev(*),           !numero di giri
     *     flow(*),          !portata
     *     head(*),          !altezza adiabatica
     *     eff(*)            !efficienza
c
      integer*4 nlim           !punti limite assegnati
c
                             !grandezze relative ai punti limite assegnati
      real*4 lim_n(*),         !numero di giri
     *     lim_q(*),         !portata
     *     lim_h(*)          !altezza adiabatica
	                       !(vettori di dimensione nlim)
c
                             !grandezze relative alla spezzata anti-surge:
      real*4 alim(*),          !termini noti
     *     blim(*)           !coefficienti angolari
                             !(vettori di dimensione nlim+1)
c
      real*4 clim(*)           !costanti delle curve limite (dim=4)
	                       !zona di choking per l'efficienza:
	                       !(1)curva di choking,(2)curva operating limit
	                       !zona di choking per i giri:
	                       !(1)curva di choking,(2)curva operating limit
c
                             !grandezze relative al punto limite di choking:
	real*4 chlim_n,          !numero di giri
     *     chlim_q,          !portata
     *     chlim_h           !altezza adiabatica
c
      real*4 chn(*),           !coeff di h=f(n,q) nella zona normale (dim=6)
     *     chc(*),           !coeff di h=f(n,q) nella zona di choking (dim=6)
     *     cen(*)           !coeff di eff=f(n,q) nella zona normale (dim=5)
c-------------------->OUTPUT
      real*4 cec(*)            !coefficienti di eff=f(n,q) nella zona di choking
c                            !dim=6
	                       !grandezze relative ai punti calcolati:
      real*4 new_rev(*),       !numero di giri
     *     new_eff(*)        !efficienza      (dim=npoint)
c
                             !valori statistici:
	real*4 sq1_r,            !deviazione standard per i giri
     *     sq2_r,            !scostamento medio per i giri
     *     sq1_e,            !deviazione standard per l'efficienza
     *     sq2_e             !scostamento medio per l'efficienza
c
      real*4 delta(*),            !differenza giri assegnati e calcolati
     *     delta_eff(*)         !differenza efficienza assegnata e calcolata

      real*4 delta_max,        !max scostamento percentuale per i giri
     *     delta_max_eff     !max scostamento percentuale per l'efficienza
c
      integer*4 idelta_max,    !indice del punto col max scostamento per i giri
     *        idelta_max_eff !indice del punto col max scostamento per l'eff


      integer*4 i,codice
c-------------------------------------------------------------------------------

c-old      min_eff=min_eff/percrif
c
c     Calcola CEC
c
      call calcola_cec(min_eff,cen,clim,cec)
c
c     Adimensionalizzazioni
c
      do i=1,nlim
	   lim_h(i)=lim_h(i)/headrif/1000.  !trasformo in km
	end do
c
c-17 gennaio
	alim(nlim+1)=alim(nlim)
	blim(nlim+1)=blim(nlim)
c-17 gennaio

	do i=1,npoint
         rev(i)=rev(i)/nom_rev
	   flow(i)=flow(i)/qmrif/1000.      !trasformo in km^3/h
	   if (head(i).eq.0) then
	      head(i)=-999.
  	   else 
	      head(i)=head(i)/headrif/1000.   !trasformo in km
	   end if
	   if (eff(i).eq.0) then
	      eff(i)=-999.
	   else
c-old	      eff(i)=eff(i)/percrif
	      eff(i)=eff(i)                  !ricevo una %N
	   end if
	end do
c      
      call stat_c(npoint,head,flow,rev,eff,
     *            nlim,alim,blim,clim,lim_h,chn,chc,cen,cec,
     *            new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e)
c
c     Dimensionalizzazioni
c
c-old      min_eff=min_eff*percrif
c
      do i=1,nlim
	   lim_h(i)=lim_h(i)*headrif*1000.   !trasformo in m
	end do
c
      do i=1,npoint
	   rev(i)=rev(i)*nom_rev  !rpm
	   if (new_rev(i).ne.-999.) new_rev(i)=new_rev(i)*nom_rev   !rpm
	   flow(i)=flow(i)*qmrif*1000.      !trasformo in m^3/h
	   if(head(i).ne.-999.) head(i)=head(i)*headrif
	   if(eff(i).ne.-999.) eff(i)=eff(i)*percrif
	   if(new_eff(i).ne.-999.) new_eff(i)=new_eff(i)*percrif
	end do
c
      sq1_r=sq1_r*nom_rev
	sq2_r=sq2_r*nom_rev
	sq1_e=sq1_e*100.
	sq2_e=sq2_e*100.
c
      call stat_cc(npoint,head,rev,new_rev,eff,new_eff,
     *             delta,delta_eff,delta_max,delta_max_eff,
     *             idelta_max,idelta_max_eff,codice) 

      do i=1,npoint
	   if(head(i).ne.-999.) head(i)=head(i)*1000.  !trasformo in m
         if(eff(i).ne.-999.) eff(i)=eff(i)/percrif             !%N
	   if(new_eff(i).ne.-999.) new_eff(i)=new_eff(i)/percrif !%N
c-elena
	   if(delta_eff(i).ne.-999.) delta_eff(i)=delta_eff(i)/percrif  ! %N
      end do
      return
	end
c----------------------------------------------------------------------------------------------
      subroutine print_t(unit,file,turb,comment,nom_rev,nom_power,
     *                   nom_hrate,nom_tair,ncorr,temp_cor,pw_cor,
     *                   hrate_cor,npwmax,rev_pwmax,pwmax,temp_pwmax,
     *                   nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *                   cpw,cpwt,chr,chrt)
***********************************************************************************************
c     Stampa sul file indicato le grandezze caratteristiche di una turbina calcolate a partire
c     da un insieme di punti assegnati,tabulati anch'essi nel file suddetto.
***********************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c------------------------>INPUT
c
      integer unit             !numero di unità da associare al file di output
c
      character file*(*),      !nome del file di output
     *          turb*(*),      !turbina analizzata
     *          comment*(*)    !commento sulla turbina analizzata
c
                               !valori nominali:
      real nom_rev,            !numero di giri
     *     nom_power,          !potenza massima
     *     nom_hrate,          !heat rate
     *     nom_tair            !temperatura dell'aria
c
c                          !Tabella delle correzioni
c
      integer ncorr        !numero di punti assegnati      
c
c                          !grandezze relative ai punti assegnati:
      real temp_cor(*),    !temperatura
     *     pw_cor(*),      !fattore di correzione per la potenza
     *     hrate_cor(*)    !fattore di correzione per l'hrate
	                     !(vettori di dimensione ncorr)
c
c                          !Tabella della potenza
c 
      integer npwmax       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real rev_pwmax(*),   !numero di giri
     *     pwmax(*),       !massima potenza
     *     temp_pwmax(*)   !temperatura
	                     !(vettori di dimensione npwmax)
c
c                          !Tabella dello heat rate
c
      integer nhrate       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real rev_hrate(*),   !numero di giri
     *     pw_hrate(*),    !massima potenza
     *     temp_hrate(*),  !temperatura
     *     hrate(*)        !heat rate
	                     !(vettori di dimensione nhrate)
c
      real cpw(*),      !coefficienti per il calcolo della potenza massima:
	                  !pw(n)=cpw(1)+cpw(2)*n+cpw(3)*n^2 (dim=3)
     *     cpwt(*),     !coefficienti per il calcolo del fattore correttivo
	                  !per la potenza (dim=4):
	                  !fpw(t)=cpwt(1)+cpwt(2)*t+cpwt(3)*t^2+cpwt(4)*t^3
     *     chr(*),      !coefficienti per il calcolo dell'heat rate:
	                  !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
	                  !         chr(5)*n^2+chr(6)*pw^2   (dim=6)
     *     chrt(*)      !coefficienti per il calcolo del fattore correttivo 
	                  !per l'heat rate (dim=4):
	                  !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
c
c-------------------------->VARIABILI LOCALI
c
c                               !grandezze relative ai punti calcolati:
      real new_pwm(npwmax),     !massima potenza 
	                          !(fisso temperatura e numero di giri)
     *     new_hrate(nhrate),    !heat rate
	                          !(fisso temperatura,numero di giri e potenza)
     *     new_fpw(ncorr),     !correzione potenza  
     *     new_fhr(ncorr),      !correzione heat rate
     *     delta_p(npwmax),
     *     delta_hr(nhrate),
     *     delta_fpw(ncorr),   !differenza valore di fpw calcolato e misurato
     *     delta_fhr(ncorr)   !differenza valore di fhr calcolato e misurato

c
      real st_dev_pwmax,        !deviazione standard per la massima potenza
     *     st_dev_hrate         !deviazione standard per lo heat rate
c
      integer i
***********************************************************************************************
c
c     Adimensionalizzazioni
c
      nom_tair=nom_tair-t0
      do i=1,npwmax
	   pwmax(i)=pwmax(i)*nom_power/powerif
         temp_pwmax(i)=temp_pwmax(i)/taux
      end do
c
      do i=1,nhrate
	   pw_hrate(i)=pw_hrate(i)*nom_power/powerif
	   temp_hrate(i)=temp_hrate(i)/taux
	   hrate(i)=hrate(i)*nom_hrate/hraterif
	end do
c-elena 21/12/05
      do i=1,ncorr
         temp_cor(i)=(temp_cor(i))/taux     !ricevo in K
      end do
c
      call stat_t(npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *            ncorr,temp_cor,pw_cor,hrate_cor,new_fpw,new_fhr,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
c-elena 21/12/05
     *            delta_fpw,delta_fhr)

c
c     Dimensionalizzazioni
c
      do i=1,ncorr
         temp_cor(i)=(temp_cor(i)*taux)-t0
c	   pw_cor(i)=pw_cor(i)*100.
c	   hrate_cor(i)=hrate_cor(i)*100.
c	   new_pwm(i)=new_pwm(i)*100.
c	   new_hrate(i)=new_hrate(i)*100.
	   delta_fpw(i)=delta_fpw(i)*100.
	   delta_fhr(i)=delta_fhr(i)*100.
	end do

      do i=1,npwmax
         rev_pwmax(i)=rev_pwmax(i)*percrif
	   pwmax(i)=(pwmax(i)*percrif*powerif)/nom_power
	   new_pwm(i)=(new_pwm(i)*percrif*powerif)/nom_power
	   temp_pwmax(i)=(temp_pwmax(i)*taux)-t0  !stampo in °C
	end do
c
      do i=1,nhrate
	   rev_hrate(i)=rev_hrate(i)*percrif
         pw_hrate(i)=(pw_hrate(i)*percrif*powerif)/nom_power
	   temp_hrate(i)=(temp_hrate(i)*taux)-t0  !stampo in °C
         hrate(i)=(hrate(i)*percrif*hraterif)/nom_hrate
	   new_hrate(i)=(new_hrate(i)*percrif*hraterif)/nom_hrate
	end do
c
      st_dev_pwmax=st_dev_pwmax*percrif
	st_dev_hrate=st_dev_hrate*percrif
c
      call stampa_t(unit,file,turb,comment,nom_rev,nom_power,nom_hrate,
     *              nom_tair,ncorr,temp_cor,pw_cor,hrate_cor,
     *              npwmax,rev_pwmax,pwmax,temp_pwmax,nhrate,
     *              rev_hrate,pw_hrate,temp_hrate,hrate,new_pwm,
     *              new_hrate,new_fpw,new_fhr,
     *              cpw,cpwt,chr,chrt,st_dev_pwmax,
     *              st_dev_hrate)
c
      return
	end
c------------------------------------------------------------------------------
      subroutine dati_turb(nom_power,nom_hrate,
     *            npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *            ncorr,temp_corr,fpw,fhr,new_fpw,new_fhr,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
c-elena 21/12/05
     *            delta_fpw,delta_fhr)
c-------------------------------------------------------------------------------
c Calcola i valori statistici relativi ad una turbina
c---------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'
cc----------------->INpUT
      real*4 nom_power,          !potenza massima
     *     nom_hrate           !heat rate
c
c                          !Tabella della potenza
c 
      integer*4 npwmax       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 rev_pwmax(*),   !numero di giri
     *     pwmax(*),       !massima potenza
     *     temp_pwmax(*)   !temperatura
	                     !(vettori di dimensione npwmax)
c
c                          !Tabella dello heat rate
c
      integer*4 nhrate       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 rev_hrate(*),   !numero di giri
     *     pw_hrate(*),    !massima potenza
     *     temp_hrate(*),  !temperatura
     *     hrate(*)        !heat rate
	                     !(vettori di dimensione nhrate)
      integer*4 ncorr       !numero di punti assegnati
c
c                          !grandezze relative ai punti assegnati:
      real*4 temp_corr(*),    !temperatura
     *     fpw(*),            !correzione potenza
     *     fhr(*)             !correzione heat rate

c
      real*4 cpw(*),      !coefficienti per il calcolo della potenza massima:
	                  !pw(n)=cpw(1)+cpw(2)*n+cpw(3)*n^2 (dim=3)
     *     cpwt(*),     !coefficienti per il calcolo del fattore correttivo
	                  !per la potenza (dim=4):
	                  !fpw(t)=cpwt(1)+cpwt(2)*t+cpwt(3)*t^2+cpwt(4)*t^3
     *     chr(*),      !coefficienti per il calcolo dell'heat rate:
	                  !hr(n,pw)=chr(1)+chr(2)*n+chr(3)*pw+chr(4)*n*pw+
	                  !         chr(5)*n^2+chr(6)*pw^2   (dim=6)
     *     chrt(*)      !coefficienti per il calcolo del fattore correttivo 
	                  !per l'heat rate (dim=4):
	                  !fhr=chrt(1)+chrt(2)*t+chrt(3)*t^2+chrt(4)*t^3
c------------>OUTPUT
      real*4 new_pwm(npwmax),     !massima potenza 
	                          !(fisso temperatura e numero di giri)
     *     new_hrate(nhrate),    !heat rate
	                          !(fisso temperatura,numero di giri e potenza)
     *     new_fpw(ncorr),     !correzione potenza  
     *     new_fhr(ncorr),      !correzione heat rate
     *     delta_p(npwmax),
     *     delta_hr(nhrate), 
     *     delta_fpw(ncorr),   !differenza valore di fpw calcolato e misurato
     *     delta_fhr(ncorr)   !differenza valore di fhr calcolato e misurato
c
      real*4 st_dev_pwmax,        !deviazione standard per la massima potenza
     *     st_dev_hrate         !deviazione standard per lo heat rate
c---------->locali
      integer i
c-----------------------------------------------------------------------------
c
c     Adimensionalizzazioni
c
      do i=1,npwmax
	   pwmax(i)=pwmax(i)*nom_power/powerif    !%N
         temp_pwmax(i)=(temp_pwmax(i))/taux     !ricevo in K
      end do
c
      do i=1,nhrate
	   pw_hrate(i)=pw_hrate(i)*nom_power/powerif
	   temp_hrate(i)=(temp_hrate(i))/taux    !ricevo in K
	   hrate(i)=hrate(i)*nom_hrate/hraterif
	end do

c-elena 21/12/05
      do i=1,ncorr
         temp_corr(i)=(temp_corr(i))/taux     !ricevo in K
      end do

c
      call stat_t(npwmax,rev_pwmax,pwmax,temp_pwmax,
     *            nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *            ncorr,temp_corr,fpw,fhr,new_fpw,new_fhr,
     *            cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *            st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
c-elena 21/12/05
     *            delta_fpw,delta_fhr)


c
c     Dimensionalizzazioni
c
      do i=1,npwmax
	   pwmax(i)=(pwmax(i)*powerif)/nom_power
	   new_pwm(i)=(new_pwm(i)*powerif)/nom_power
	   temp_pwmax(i)=(temp_pwmax(i)*taux)  
c-elena
         delta_p(i)=(delta_p(i)*powerif)/nom_power
	end do
c
      do i=1,nhrate
         pw_hrate(i)=(pw_hrate(i)*powerif)/nom_power
	   temp_hrate(i)=(temp_hrate(i)*taux)
         hrate(i)=(hrate(i)*hraterif)/nom_hrate
	   new_hrate(i)=(new_hrate(i)*hraterif)/nom_hrate
c-elena
	   delta_hr(i)=(delta_hr(i)*hraterif)/nom_hrate

	end do
c-elena 21/12/05
      do i=1,ncorr
	   temp_corr(i)=(temp_corr(i)*taux)  
	end do

c
      st_dev_pwmax=st_dev_pwmax*percrif   !%
	st_dev_hrate=st_dev_hrate*percrif   !%
c
      return
	end