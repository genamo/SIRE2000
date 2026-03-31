c----------------------------------------------------------------------------------------      
      subroutine maxpw_curve(np,max_rev,min_rev,cpw,
     *                       pwmin,pwmax,rev_pw,ppwmax)
c*********************************************************************************************
c    calcola le coordinate dei punti usati per disegnare la curva di massima potenza
c    di una turbina (rev(i),ppwmax(i)) i=1,...,np , alla temperatura in corrispondenza
c    della quale il corrispondente fattore di correzione vale 1.
C    Gli input MAX_REV,MIN_REV,PWMIN,PWMAX e gli output REV_PW,PPWMAX sono grandezze
c    adimensionali
c*********************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c----------------------------------->Input
      integer*4 np     !numero di punti da calcolare
c
      real*4 max_rev,  !numero di giri minimo
     *     min_rev,  !numero di giri massimo 
     *     cpw(*),   !coef per determinare la max power
     *     pwmin,    !minimo valore della potenza
     *     pwmax    !massimo valore della potenza
c------------------------------------>Output
c                      curva di massima potenza
      real*4 rev_pw(*), !ascisse
     *     ppwmax(*)  !ordinate
c
c------------------------------------->variabili locali
      real*4 dr,pow
c
	integer*4 i
c*********************************************************************************************
      dr=(max_rev-min_rev)/(np-1)
	do i=1,np
	   rev_pw(i)=min_rev+(i-1)*dr
	   call pwdan_ec_a(pow,rev_pw(i),cpw)
	   if (pow.lt.pwmin) then
	      ppwmax(i)=pwmin
	   else if (pow.gt.pwmax) then
	      ppwmax(i)=pwmax
	   else
	      ppwmax(i)=pow
	   end if
	end do
c
      return
	end
c----------------------------------------------------------------------------------------------
      subroutine fcor_curve_ec(np,tmin,tmax,fmin,fmax,cpwt,chrt,
     * 	                  temp,fac_hr,fac_pw)
c************************************************************************************************
c     Calcola i fattori correttivi per l'heat-rate e per la potenza massima relativi a
c     np valori della temperatura calcolati:(temp(i),fac_hr(i)),(temp(i),fac_pw(i)) 
c     per i=1,...,np;tali punti saranno usati per disegnare l'andamento dei due fattori
c     correttivi.
c     TMIN e TMAX sono assegnate in gradi centigradi;FMIN e FMAX sono valori percentuali
c************************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------------------------->Argomenti della subroutine
c--------------------------------->INPUT
      integer*4 np    !numero di punti da calcolare
c
      real*4 tmin   !temperatura minima in °C
     *    ,tmax     !temperatura massima in °C
     *    ,fmin     !valore minimo del fattore correttivo
     *    ,fmax     !valore massimo del fattore correttivo
     *    ,cpwt(*)  !coefficienti correttivi per la potenza  (dim=4)
     *    ,chrt(*)  !coefficienti correttivi per l'heat-rate (dim=4)
c-------------------------------->OUTPUT
      real*4 temp(*)  !valori della temperatura calcolati(ascissa) (dim=np)
     *    ,fac_hr(*) !valori corrispondenti del fattore correttivo 
c                                                 per l'heat rate(dim=np)
     *    ,fac_pw(*) !valori corrispondenti del fattore correttivo
c                                          per la potenza massima(dim=np)
c------------------------------------------->Variabili locali
      real*4 dt       !passo di discretizzazione dell'intervallo [tmin,tmax]
     *    ,t_aux
c
      integer*4 i
c************************************************************************************************
	dt=(tmax-tmin)/(np-1)
	do i=1,np 
	   temp(i)=tmin+(i-1)*dt
	   t_aux=((temp(i)+t0)/(trif+t0))
c---------------------->FATTORE CORRETTIVO PER L'HEAT RATE
	   fac_hr(i)=(chrt(1)+t_aux*(chrt(2)
     *	               +t_aux*(chrt(3)+chrt(4)*t_aux)))*100.
         if (fac_hr(i).lt.fmin) then
	      fac_hr(i)=fmin
	   end if
c---------------------->FATTORE CORRETTIVO PER LA POTENZA MASSIMA
	   fac_pw(i)=(cpwt(1)+t_aux*(cpwt(2)+
     *                   t_aux*(cpwt(3)+cpwt(4)*t_aux)))*100.
	   if (fac_pw(i).lt.fmin) then
	      fac_pw(i)=fmin
	   end if
	end do
	return
	end
c------------------------------------------------------------------------------------------------
      subroutine turb_point(np,nom_power,nom_hrate,cpw,cpwt,chr,chrt,
     *                      tmin,tmax,
     *                      rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
     *                      pwup,pwdw,temp,fac_hr,fac_pw,hr)
c**********************************************************************************************
c     calcola i punti per disegnare le curve caratteristiche di una turbina nel piano RP
c     (sull'asse delle ascisse varia il numero di giri(rev) mentre sull'asse delle ordinate
c     varia la potenza(power)) e per disegnare l'andamento dei fattori ccrrettivi in funzione
c     della temperatura
c**********************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------------>input
      integer*4 np         !numero di punti da calcolare
c
      real*4 nom_power,  !potenza nominale
     *     nom_hrate,    !heat rate nominale
     *     cpw(*),       !coeff per determinare la max potenza della turbina
     *     cpwt(*),      !coeff per la correzione della potenza in funzione 
                         !della temperatura dell'aria
     *     chr(*),       !coefficienti per il calcolo dell'heat rate
     *     chrt(*),      !coeff per la correzione dell'heat rate in funzione 
	                   !della temperatura dell'aria
     *     tmin,         !temperatura minima per valutazione fattori correttivi 
     *     tmax          !temperatura massima per valutazione fattori correttivi
	                   !(valori passati in gradi KELVIN)
c-------------------------------->output
c                               curva di massima potenza
      real*4 rev_pw(*),          !ascisse
     *     ppwmax(*),          !ordinate
c                               curva ad heat rate costante
     *     rev_hr_up(max_nhr,*),  !ascisse(1°set)
     *     rev_hr_dw(max_nhr,*),  !ascisse(2°set) 
     *     pwup(max_nhr,*),    !ordinate(1°set)
     *     pwdw(max_nhr,*)     !ordinate(2°set)
c                            
c                               andamento fattori correttivi
      real*4 temp(*)             !ascisse (temperatura)
     *    ,fac_hr(*)           !ordinate correzione hr
     *    ,fac_pw(*)           !ordinate correzione pw
c
      real*4 hr(7)       !valori costanti dell'heat rate

c--------------------------------->variabili locali
      integer*4 i,j,k
c
      real*4 max_rev,      !numero di giri massimo rispetto al nominale
     *     min_rev,      !numero di giri minimo rispetto al nominale
     *     pwmin,        !minima potenza
     *     pwmax,        !massima potenza
     *     fmin,         !minimo valore di correzione
     *     fmax          !massimo valore di correzione

      integer*4 nhr        !numero di curve a heat rate costante da disegnare
c
c
c     Inizializzazioni
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c                            ATTENZIONE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   L'istruzione DATA viene eseguita una volta soltanto,la prima volta che lancio il
c   programma------------>Mai utilizzarla!<-------------------------------------------
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


c-elena      data min_rev/50./,pwmin/30./,max_rev/110./,pwmax/110./
c-elena      data tmin/-20./,fmin/60./
c-elena      data tmax/40./,fmax/140./
c**********************************************************************************************
      if (np.eq.max_np) np=np-2
c-------------------------------->inizializzioni
      min_rev=50.
	max_rev=110.
	pwmin=30.
	pwmax=110.
	fmin=60.
	fmax=140.

      nhr=7
	hr(1)=130.
	hr(2)=120.
	hr(3)=115.
	hr(4)=110.
	hr(5)=105.
	hr(6)=100.
	hr(7)=90.
c-prove per gaia
c
c	hr(1)=104.
c	hr(2)=103.5
c	hr(3)=103.
c	hr(4)=102.5
c	hr(5)=101.79
c	hr(6)=100.5
c	hr(7)=100.
c-----------------------------
c---------------------------------->adimensionalizzazioni
      max_rev=max_rev/100.
	min_rev=min_rev/100.

c
      nom_power=nom_power/powerif
	nom_hrate=nom_hrate/hraterif
c
      pwmin=(pwmin*nom_power)/100.
      pwmax=(pwmax*nom_power)/100.

      call maxpw_curve(np,max_rev,min_rev,cpw,
     *                 pwmin,pwmax,rev_pw,ppwmax)
c
	do i=1,nhr
	   hr(i)=(hr(i)*nom_hrate)/100.
	end do

      call hr_curve_ec_new(nhr,hr,np,chr,pwmin,pwmax,cpw,
     *                    rev_pw,ppwmax,rev_hr_up,rev_hr_dw,pwup,pwdw)

c      call hr_curve_ec(nhr,hr,np,chr,pwmin,pwmax,
c     *                    rev_pw,ppwmax,rev_hr_up,rev_hr_dw,pwup,pwdw)
c-------> I valori TMIN e TMAX devono essere convertiti da KELVIN a °C
      tmin=tmin-t0
	tmax=tmax-t0 
c  I limiti per la temperatura nel vecchio Sire erano scolpiti a -20 e 40  
c  Per garantire che almeno questa situazione sia rispettata mi comporto come segue:  
c  Se tmin>-20 pongo tmin=-20, se tmax<40 pongo tmax=40.Se ciò non accade per evitare 
c  di dover valutare i fattori correttivi agli estremi delle curve lascio un margine di
c  5°C sia a destra che a sinistra
	if (tmin.gt.-20) then
	   tmin=-20
	else
	   tmin=tmin-5
	end if
	if (tmax.lt.40) then
	   tmax=40
	else
	   tmax=tmax+5
	end if


      call fcor_curve_ec(np,tmin,tmax,fmin,fmax,cpwt,chrt,
     * 	                  temp,fac_hr,fac_pw)
c
      max_rev=max_rev*100.
	min_rev=min_rev*100.
c
      do i=1,nhr
	   hr(i)=(hr(i)/nom_hrate)*100.
	end do
c
      do i=1,np
	   rev_pw(i)=rev_pw(i)*100.
	   ppwmax(i)=(ppwmax(i)/nom_power)*100.
      end do
c 
      k=np+2
      do i=1,nhr
	   do j=1,k
	      rev_hr_dw(i,j)=rev_hr_dw(i,j)*100.
	      rev_hr_up(i,j)=rev_hr_up(i,j)*100.
	      pwup(i,j)=(pwup(i,j)/nom_power)*100.
	      pwdw(i,j)=(pwdw(i,j)/nom_power)*100.
	   end do
	end do

      nom_power=nom_power*powerif
	nom_hrate=nom_hrate*hraterif

c
      return
	end
c---------------------------------------------------------------------------------
       subroutine trans_turb(nom_power,cpwt,nmaxp,pwmax,airpwm,nhrate,
     *                       power,airhrt,pwmax_t,power_t)
c*********************************************************************************
c   Converte le grabìndezze relative ai punti assegnati per la regressione dei 
c   coefficienti caratteristici di una turbina alla temperatura di graficazione
c   (quella per cui i fattori di correzione valgono 1)
c   In input le temperature sono assegnate in KELVIN e la potenza in percentuale
c*********************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------->Input
      real*4 nom_power    !potenza nominale
c
      integer*4 nmaxp          ! punti a disposizione per il calcolo dei CPW          
c                              ! grandezze relative ai punti osservati a disposizione:
      real*4 pwmax(*)          ! potenza massima;
     *	  ,airpwm(*)         ! temperatura.
c
      integer*4 nhrate         ! punti a disposizione per il calcolo dei CHR                               
	                         ! grandezze relative ai punti osservati a disposizione:
	real*4 power(*)          ! potenza;
     *      ,airhrt(*)         ! temperatura;
     *      ,cpwt(*)           !coeff per la correzione della potenza funzione 
                               !della temperatura dell'aria
c------->Output                  
      real*4 pwmax_t(*),        !potenza alla temperatura di graficazione
     *       power_t(*)
c------->Variabili locali
      integer*4 i
c*********************************************************************************
      nom_power=nom_power/powerif
c
      do i =1,nmaxp
c-old	   pwmax(i)=(pwmax(i)*nom_power)/100.
	   pwmax(i)=(pwmax(i)*nom_power)
	   airpwm(i)=airpwm(i)/taux
	   call trans_t_a(pwmax(i),airpwm(i),cpwt,pwmax_t(i))
	   pwmax_t(i)=(pwmax_t(i)*100.)/nom_power
	end do
c
	do i=1,nhrate
c-old	   power(i)=(power(i)*nom_power)/100.
	   power(i)=(power(i)*nom_power)
	   airhrt(i)=airhrt(i)/taux
	   call trans_t_a(power(i),airhrt(i),cpwt,power_t(i))
	   power_t(i)=(power_t(i)*100.)/nom_power
	end do
c
      return
	end

c-----------------------------------------------------------------------------------
      subroutine trans_fcorr(ncorr,temp,fpw,fhr)
c***********************************************************************************
c    Converte l'UDM delle grandezze caratteristiche dei punti descrittivi assegnati 
c    per il calcolo dei fattori correttivi da interna a UDM di graficazione  
c    (conformi alle condizioni ISO):
c    - le temperature devono essere espresse in °C
c    - i fattori correttivi devono essere delle % a 100
c***********************************************************************************
      implicit none
      include '../inc/rk_param.inc'
      
	integer*4 ncorr     !numero di punti assegnati per la valutazione dei
	                    !fattori correttivi
                          !grandezze caratteristiche di tali punti:
	real*4 temp(*)      !I/O) temperatura    (dim=ncorr)
     *      ,fpw(*)       !I/O) correzione potenza (dim=ncorr)
     *      ,fhr(*)       !I/O) correzione heat rate (dim=ncorr)

      integer*4 i
c------------------------------------------------------------------------------------
      do i=1,ncorr
	   temp(i)=temp(i)-t0
	   fpw(i)=fpw(i)*100.
	   fhr(i)=fhr(i)*100.
	end do

      return
	end  !trans turb

c--------------------------------------------------------------------------------------

      subroutine hr_curve_ec_new(nhr,hr,np,chr,pwmin,pwmax,cpw,
     *                    rev_pw,ppwmax,rev_hr_up,rev_hr_dw,pwup,pwdw)
c***************************************************************************************
c     calcola le coordinate dei punti usati per disegnare le curve ad heat-rate costante
c     per diversi valori dell'heat-rate ;
c     gli input HR,PWMIN,PWMAX,REW_PW,PPWMAX e gli output REV_HR_UP,REV_HR_DW,PWUP,PWDW 
c     sono grandezze adimensionali;
c***************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------->input
      integer*4 nhr        !numero di curve ad hr costante da graficare
c
      real*4 hr(*)         !valori costanti dell'heat-rate (dim=nhr)
c
      integer*4 np         !numero di punti calcolati su ogni curva
c
      real*4 chr(*),cpw(*),       !coefficienti 
     *     pwmin,        !potenza minima(%)
     *     pwmax,        !potenza massima(%)
c                         coordinate punti sulla max power
     *     rev_pw(*),    !ascisse 
     *     ppwmax(*)     !ordinate
c-------------------------->output
c                              punti sulle curva ad hr costante
      real*4 rev_hr_up(max_nhr,*), !ascisse(1°set) (numero di giri)
     *     rev_hr_dw(max_nhr,*), !ascisse(2°set) 
     *     pwup(max_nhr,*),   !ordinate (1°set)
     *     pwdw(max_nhr,*)    !ordinate(2°set)
c--------------------------->variabili locali
      integer*4 i,k,j,j1,j2,jp,mp
c
      real*4 heat,
     *     raux(max_np),
     *     a,b,c,
     *     delta,
     *     pw1,pw2,pw3,
     *     aux_up(max_np),
     *     aux_dw(max_np),
     *     rev1,rev2,rev3,rev,dr,rev_min,rev_max,pow(max_np),pw,
     *     alfa,beta,gamma,delta_hr,n_und,j_und 

	real*4  min_rev,max_rev
	parameter (min_rev=0.5,max_rev=1.1)
***************************************************************************************
      do i=1,nhr
	   heat=hr(i)
c-elena+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C----->trovo n in corrispondenza del quale ho un unica intersezione con la curva di hr
	   a=chr(4)*chr(4)-4*chr(5)*chr(6)
	   b=2*(chr(3)*chr(4)-2*chr(2)*chr(6))
	   c=chr(3)*chr(3)-4*chr(1)*chr(6)+4*chr(6)*heat
	   delta=b*b-4*a*c
	   if (delta.gt.0) then 
	         rev1=(-b+sqrt(delta))/2/a
	         rev2=(-b-sqrt(delta))/2/a
	         if (rev1.gt.rev2) then
	            rev3=rev1
	            rev1=rev2
	            rev2=rev3
			 end if
	         if(a.lt.0)then
	           rev_min=rev1
	           rev_max=rev2
	         else
	           rev_min=rev2
	           rev_max=rev1
               end if
	   else if (delta.eq.0) then
	         rev_min=(-b)/2/a
	         rev_max=max_rev
	   else if (delta.lt.0) then
	        rev_min=min_rev 
			rev_max=max_rev 
         end if

         if (rev_min.gt.rev_max) then
	      dr= rev_min
	      rev_min=rev_max
	      rev_max=dr
	   end if
         if (rev_min.lt.min_rev .or.
     -       rev_min.gt.max_rev     ) then
	      rev_min=min_rev 
	   end if
         if (rev_max.lt.min_rev .or.
     -       rev_max.gt.max_rev     ) then
	      rev_max=max_rev 
	   end if
	   dr=(rev_max-rev_min)/(np-1)
	   rev_min=rev_min-dr
	   rev_max=rev_max+dr
	   rev_min=max(min_rev,rev_min)
         rev_max=min(max_rev,rev_max)
c-elena+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	   k=0
         j1=1    
	   j2=1 
	   n_und=0
	   j_und=0
	   dr=(rev_max-rev_min)/(np-1)   
	   do j=1,np
	      raux(j)=rev_min+(j-1)*dr
	      call pwdan_ec_a(pow(j),raux(j),cpw)
	      alfa=chr(6)
	      beta=chr(3)+chr(4)*raux(j)
	      gamma=chr(1)+chr(2)*raux(j)+chr(5)*raux(j)*raux(j)-heat
	      delta_hr=beta*beta-4*alfa*gamma
	      if (delta_hr.ge.0) then
               j2=j2+1   
	         pw1=(-beta+sqrt(delta_hr))/2/alfa
	         pw2=(-beta-sqrt(delta_hr))/2/alfa
	         if (pw2.gt.pw1) then
	            pw3=pw2
	            pw2=pw1
	            pw1=pw3
			 end if
	         if ((pw1.le.pow(j)).and.(pw1.ge.pwmin)) then
                  aux_up(j)=pw1
	         else if (pw1.lt.pwmin) then
	            aux_up(j)=pwmin
               else
                  aux_up(j)=pow(j)
	            if(n_und.eq.0) then
	              j_und=j
	              n_und=n_und+1
				else
	              n_und=n_und+1
	            end if
 	         end if
	         if ((pw2.le.pow(j)).and.(pw2.ge.pwmin)) then
                  aux_dw(j)=pw2
	         else if (pw2.lt.pwmin) then
                  aux_dw(j)=pwmin
               else
                  aux_dw(j)=pow(j)
	         end if
            else if (j1.eq.j) then
	         j1=j1+1
	         j2=j1
	      end if
         end do
	   mp=j2-j1   
         if (mp.gt.1) then
	      do j=1,mp
	         rev_hr_up(i,j)=raux(j1+j-1)
	         rev_hr_dw(i,j+1)=rev_hr_up(i,j)
	         pwup(i,j)=aux_up(j1+j-1)
	         pwdw(i,j+1)=aux_dw(j1+j-1)
		  end do
c------->RACCORDO A SINISTRA
	      if ((j1.gt.1).and.(aux_up(j1).ne.aux_dw(j1))) then
	         if(aux_up(j1).ne.pow(j1)) then
			   pwdw(i,1)=pwup(i,1)
	           rev_hr_dw(i,1)= rev_hr_up(i,1)
	           k=mp+1
	         else
	           raux(j1-1)=raux(j1)-(dr/5.)
	           call pwdan_ec_a(pw,raux(j1-1),cpw)
			   pwdw(i,1)=pw
	           rev_hr_dw(i,1)=raux(j1-1)
	           k=mp+1
			 end if
	      else
	         do j=1,mp
	            pwdw(i,j)=pwdw(i,j+1)
	            rev_hr_dw(i,j)=rev_hr_dw(i,j+1)
	            k=mp
			 end do
		  end if
C-------->RACCORDO A DESTRA
            jp=j2-1
            if ((jp.lt.np).and.(pwup(i,mp).ne.pwdw(i,mp))) then
	         if(pwup(i,mp).ne.pow(j2)) then
	           pwdw(i,k+1)=pwup(i,mp)
	           rev_hr_dw(i,k+1)=rev_hr_up(i,mp)
	         else
	           raux(k+1)=raux(k)+(dr/5.)
	           call pwdan_ec_a(pw,raux(k+1),cpw)
	           pwdw(i,k+1)=pw
	           rev_hr_dw(i,k+1)=raux(k+1)
		     end if
		  end if
	   end if 

	end do
	return
	end

c---------------------------------------------------------------------------------------------
c  Routine di prova per rendere flessibile il calcolo del range di temperature utilizzando
c  per il calcolo della temperatura minima e massima la subroutine RANGE_TEMP
c---------------------------------------------------------------------------------------------
      subroutine turb_point_new(np,nom_power,nom_hrate,cpw,cpwt,
     *               chr,chrt,
     *               ncorr,tcorr,
     *               rev_pw,ppwmax,nhr,rev_hr_up,rev_hr_dw,
     *               pwup,pwdw,temp,fac_hr,fac_pw,hr)
c**********************************************************************************************
c     calcola i punti per disegnare le curve caratteristiche di una turbina nel piano RP
c     (sull'asse delle ascisse varia il numero di giri(rev) mentre sull'asse delle ordinate
c     varia la potenza(power)) e per disegnare l'andamento dei fattori ccrrettivi in funzione
c     della temperatura
c**********************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------------>input
      integer*4 np         !numero di punti da calcolare
c
      real*4 nom_power,    !potenza nominale
     *     nom_hrate,    !heat rate nominale
     *     cpw(*),       !coeff per determinare la max potenza della turbina
     *     cpwt(*),      !coeff per la correzione della potenza in funzione 
                         !della temperatura dell'aria
     *     chr(*),       !coefficienti per il calcolo dell'heat rate
     *     chrt(*)       !coeff per la correzione dell'heat rate in funzione 
	                   !della temperatura dell'aria
c
      integer*4 ncorr   !numero punti nella tabella dei fattori correttivi
	real*4 tcorr(*)    !valori di temperatura dei punti assegnati

c-------------------------------->output
c                               curva di massima potenza
      real*4 rev_pw(*),          !ascisse
     *     ppwmax(*),          !ordinate
c                               curva ad heat rate costante
     *     rev_hr_up(max_nhr,*),  !ascisse(1°set)
     *     rev_hr_dw(max_nhr,*),  !ascisse(2°set) 
     *     pwup(max_nhr,*),    !ordinate(1°set)
     *     pwdw(max_nhr,*)     !ordinate(2°set)
c                            
c                               andamento fattori correttivi
      real*4 temp(*)             !ascisse (temperatura)
     *    ,fac_hr(*)           !ordinate correzione hr
     *    ,fac_pw(*)           !ordinate correzione pw
c
      real*4 hr(7)       !valori costanti dell'heat rate

c--------------------------------->variabili locali
      integer*4 i,j,k
c
      real*4 max_rev,      !numero di giri massimo rispetto al nominale
     *     min_rev,      !numero di giri minimo rispetto al nominale
     *     pwmin,        !minima potenza
     *     pwmax,        !massima potenza
     *     tmin,         !temperatura minima
     *     tmax,         !temperatura massima
     *     fmin,         !minimo valore di correzione
     *     fmax          !massimo valore di correzione

      integer*4 nhr        !numero di curve a heat rate costante da disegnare
c
c
c     Inizializzazioni
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c                            ATTENZIONE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   L'istruzione DATA viene eseguita una volta soltanto,la prima volta che lancio il
c   programma------------>Mai utilizzarla!<-------------------------------------------
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


c-elena      data min_rev/50./,pwmin/30./,max_rev/110./,pwmax/110./
c-elena      data tmin/-20./,fmin/60./
c-elena      data tmax/40./,fmax/140./
c**********************************************************************************************
      if (np.eq.max_np) np=np-2
c-------------------------------->inizializzioni
      min_rev=50.
	max_rev=110.
	pwmin=30.
	pwmax=110.
	fmin=60.
	fmax=140.

      nhr=7
	hr(1)=130.
	hr(2)=120.
	hr(3)=115.
	hr(4)=110.
	hr(5)=105.
	hr(6)=100.
	hr(7)=90.
c---->calcolo dinamico del range di temperatura per scalare gli assi nella mappa dei fattori
c     correttivi
      call range_temp(ncorr,tcorr,Tmin,Tmax)

c---------------------------------->adimensionalizzazioni
      max_rev=max_rev/100.
	min_rev=min_rev/100.

c
      nom_power=nom_power/powerif
	nom_hrate=nom_hrate/hraterif
c
      pwmin=(pwmin*nom_power)/100.
      pwmax=(pwmax*nom_power)/100.

      call maxpw_curve(np,max_rev,min_rev,cpw,
     *                 pwmin,pwmax,rev_pw,ppwmax)
c
	do i=1,nhr
	   hr(i)=(hr(i)*nom_hrate)/100.
	end do
c----->prova nuova routine
      call hr_curve_ec_new(nhr,hr,np,chr,pwmin,pwmax,cpw,
     *                    rev_pw,ppwmax,rev_hr_up,rev_hr_dw,pwup,pwdw)

      call fcor_curve_ec(np,tmin,tmax,fmin,fmax,cpwt,chrt,
     * 	                  temp,fac_hr,fac_pw)
c
      max_rev=max_rev*100.
	min_rev=min_rev*100.
c
      do i=1,nhr
	   hr(i)=(hr(i)/nom_hrate)*100.
	end do
c
      do i=1,np
	   rev_pw(i)=rev_pw(i)*100.
	   ppwmax(i)=(ppwmax(i)/nom_power)*100.
      end do
c 
      k=np+2
      do i=1,nhr
	   do j=1,k
	      rev_hr_dw(i,j)=rev_hr_dw(i,j)*100.
	      rev_hr_up(i,j)=rev_hr_up(i,j)*100.
	      pwup(i,j)=(pwup(i,j)/nom_power)*100.
	      pwdw(i,j)=(pwdw(i,j)/nom_power)*100.
	   end do
	end do


      nom_power=nom_power*powerif
	nom_hrate=nom_hrate*hraterif

c
      return
	end
c--------------------
      subroutine hr_curve_ec(nhr,hr,np,chr,pwmin,pwmax,
     *                    rev_pw,ppwmax,rev_hr_up,rev_hr_dw,pwup,pwdw)
c***************************************************************************************
c     calcola le coordinate dei punti usati per disegnare le curve ad heat-rate costante
c     per diversi valori dell'heat-rate ;
c     gli input HR,PWMIN,PWMAX,REW_PW,PPWMAX e gli output REV_HR_UP,REV_HR_DW,PWUP,PWDW 
c     sono grandezze adimensionali;
c***************************************************************************************
      implicit none
	include '../inc/reference.inc'
c------------------------->input
      integer nhr        !numero di curve ad hr costante da graficare
c
      real hr(*)         !valori costanti dell'heat-rate (dim=nhr)
c
      integer np         !numero di punti calcolati su ogni curva
c
      real chr(*),       !coefficienti per calcolare l'hr
     *     pwmin,        !potenza minima(%)
     *     pwmax,        !potenza massima(%)
c                         coordinate punti sulla max power
     *     rev_pw(*),    !ascisse 
     *     ppwmax(*)     !ordinate
c-------------------------->output
c                              punti sulle curva ad hr costante
      real rev_hr_up(max_nhr,*), !ascisse(1°set) (numero di giri)
     *     rev_hr_dw(max_nhr,*), !ascisse(2°set) 
     *     pwup(max_nhr,*),   !ordinate (1°set)
     *     pwdw(max_nhr,*)    !ordinate(2°set)
c--------------------------->variabili locali
      integer i,k,j,j1,j2,jp,mp
c
      real heat,
     *     raux,
     *     a,b,c,
     *     delta,
     *     pw1,pw2,pw3,
     *     aux_up(max_np),
     *     aux_dw(max_np)
c***************************************************************************************
      do i=1,nhr
	   heat=hr(i)
	   k=0
         j1=1    
	   j2=1    
	   do j=1,np
	      raux=rev_pw(j)
	      a=chr(6)
	      b=chr(3)+chr(4)*raux
	      c=chr(1)+chr(2)*raux+chr(5)*raux*raux-heat
	      delta=b*b-4*a*c
	      if (delta.ge.0) then
               j2=j2+1   
	         pw1=(-b+sqrt(delta))/2/a
	         pw2=(-b-sqrt(delta))/2/a
	         if (pw2.gt.pw1) then
	            pw3=pw2
	            pw2=pw1
	            pw1=pw3
			 end if
	         if ((pw1.le.ppwmax(j)).and.(pw1.ge.pwmin)) then
                  aux_up(j)=pw1
	         else if (pw1.lt.pwmin) then
	            aux_up(j)=pwmin
               else
                  aux_up(j)=ppwmax(j)
 	         end if
	         if ((pw2.le.ppwmax(j)).and.(pw2.ge.pwmin)) then
                  aux_dw(j)=pw2
	         else if (pw2.lt.pwmin) then
                  aux_dw(j)=pwmin
               else
                  aux_dw(j)=ppwmax(j)
	         end if
            else if (j1.eq.j) then
	         j1=j1+1
	         j2=j1
	      end if
         end do
	   mp=j2-j1   
         if (mp.gt.1) then
	      do j=1,mp
	         rev_hr_up(i,j)=rev_pw(j1+j-1)
	         rev_hr_dw(i,j+1)=rev_hr_up(i,j)
	         pwup(i,j)=aux_up(j1+j-1)
	         pwdw(i,j+1)=aux_dw(j1+j-1)
		  end do
c------->RACCORDO A SINISTRA
	      if ((j1.gt.1).and.(aux_up(j1).ne.aux_dw(j1))) then
	         pwdw(i,1)=pwup(i,1)
	         rev_hr_dw(i,1)=rev_hr_up(i,1)
	         k=mp+1
	      else
	         do j=1,mp
	            pwdw(i,j)=pwdw(i,j+1)
	            rev_hr_dw(i,j)=rev_hr_dw(i,j+1)
	            k=mp
			 end do
		  end if
C-------->RACCORDO A DESTRA
            jp=j2-1
            if ((jp.lt.np).and.(aux_up(jp).ne.aux_dw(jp))) then
	         pwdw(i,k+1)=pwup(i,jp)
	         rev_hr_dw(i,k+1)=rev_hr_up(i,jp)
		  end if
	   end if      
	end do
	return
	end
