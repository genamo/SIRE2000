c----------------------------------------------------------------------------------------
c
      subroutine rev_curve_ec(np,ngiri,giri,chn,chc,nlim,
     * 	                 alim,blim,clim,qs,hs,qc,hc,qol,hol,
     *                     lim_n,lim_q,lim_h,qrn,hrn,qrc,hrc)
c****************************************************************************************
c     calcola le coordinate (dimensionali) dei punti per disegnare le curve ai vari numeri 
c     di giri
c****************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c--------------------------------------------->Argomenti della subroutine
c
c---------------------------------------->INPUT
c 
      integer*4 np    !numero di punti da calcolare per ogni curva
     *	   ,ngiri !numero di curve da disegnare
c
      real*4 giri(*) !valori del numero di giri assegnati (dim=ngiri)
     *	,chn(*)  !coeff di h=f(n,q) nella zona normale (dim=6)
     *	,chc(*)  !coeff di h=f(n,q) nella zona di choking (dim=6)
c
      integer*4 nlim !numero di punti limite assegnati
c
      real*4 alim(*) !vettore dei termini noti della spezzata di surge(dim=nlim+1)
     *    ,blim(*) !vettore dei coefficienti angolari della spezzata di surge
     *             !                                                (dim=nlim+1)
     *    ,clim(*)  !costanti delle curve limite del compressore :     (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
	              !(3) curva di choking,(4) operating limit
     *    ,qs(*)    !ascisse dei punti calcolati sull'anti-sourge (dim=nlim)
     *	,hs(*)    !ordinate dei punti calcolati sull'anti-sourge(dim=nlim)
     *	,qc(*)    !ascisse dei punti calcolati sull'anti-choke  (dim=np)
     *	,hc(*)    !ordinate dei punti calcolati sull'anti-choke (dim=np)
     *	,qol(*)   !ascisse dei punti calcolati sulla operating limit (dim=np)
     *	,hol(*)   !ordinate dei punti calcolati sulla operating limit (dim=np)
c                   (le coordinate calcolate sono DIMENSIONALI)
	    
     *    ,lim_n(*)!vettore dei valori estremi del numero di giri (dim=nlim)
     *    ,lim_q(*)!vettore dei valori estremi della portata (dim=nlim)
     *    ,lim_h(*)!vettore dei valori limite della prevalenza (dim=nlim)
c
c---------------------------------------->OUTPUT
c 
      real*4 qrn(max_ngiri,*)!ascisse dei punti sulle curve ai vari numeri di giri 
	                  !                                  nella zona normale
     *	,hrn(max_ngiri,*)!ordinate dei pti sulle curve ai vari numeri di giri
	                  !                                   nella zona normale
     *	,qrc(max_ngiri,*)!ascisse dei punti sulle curve ai vari numeri di giri
	                  !                               nella zona di choking  
     *	,hrc(max_ngiri,*) !ordinate dei pti sulle curve ai vari numeri di giri 
		              !                                nella zona di choking 
c                       (sono tutte matrici di np colonne)
c
c---------------------------------------->Variabili ausiliarie
c 
      real*4 qaux1
     *	,qaux2
     *	,qaux3
     *	,qmini !valore minimo di q sulle curve
     *	,qmaxi !valore massimo di q sulle curve
     *	,dq    !passo di discretizzazione
     *	,qq
     *    ,haux
c 
	integer*4 k  !indici usati nei cicli
     *	   ,i
     *       ,irange
c------------------------------------------------------------------------------------------
c******************************************************************************************
c    Per ciascuna curva a numero di giri costante si calcolano le coordinate di np punti 
c    la cui ascissa è ottenuta discretizzando l'intervallo [qmini,qmaxi] usando un passo dq 
c******************************************************************************************
c-------------------------->CURVE AI VARI NUMERI DI GIRI(normal zone)
c
      do k=1,ngiri
	   if(k.eq.1) then
	     qmini=qs(nlim)
	     qmaxi=qc(np)
         else if (k.eq.ngiri) then
	     qmini=qs(1)
	     qmaxi=qc(1)
	   else
           call qdan_su_a(chn,alim,blim,nlim,lim_n,giri(k),qaux1,irange)

	     qmini=qaux1*qmrif           !dimensionalizzazione
           call qdan_ch_a(clim(3),qaux2,giri(k),chn)

	     qmaxi=qaux2*qmrif           !dimensionalizzazione
	   end if
	   dq=(qmaxi-qmini)/(np-1)
	   do i=1,np
	      qrn(k,i)=qmini+(i-1)*dq
	      qq=qrn(k,i)/qmrif
            call hdaqn_a(haux,qq,giri(k),chn)
      	  hrn(k,i)=haux*headrif    !dimensionalizzazione
         end do
c------------------------->CURVE AI VARI NUMERI DI GIRI(choking zone)
c
         if (clim(3).ne.clim(4))then     !la zona di choking è definita
	      ! continuità delle curve:il qmaxi della zona normale diventa
		  ! il qmini della zona di choking 
	      qmini=qmaxi
	      qrc(k,1)=qmini
	      hrc(k,1)=hrn(k,np)
	      if(k.eq.1) then
	        qmaxi=qol(np)
            else if (k.eq.ngiri) then
	        qmaxi=qol(1)
	      else	
              call qdan_ch_a(clim(4),qaux3,giri(k),chc)
              qmaxi=qaux3*qmrif
	      end if
	      dq=(qmaxi-qmini)/(np-1)
	      do i=2,np
	         qrc(k,i)=qmini+(i-1)*dq
	         qq=qrc(k,i)/qmrif
               call hdaqn_a(haux,qq,giri(k),chc)
     	         hrc(k,i)=haux*headrif
            end do
	   end if
	end do
	return
	end
c*************************************************************************************
	Subroutine curv_eff_ec(min_rev,max_rev,qlim,cen,
     *                    	cec,clim,neff,effic)
c***********************************************************************
c	determina il numero di curve a efficienza costante da disegnare e i  
c     valori costanti da graficare(adimensionali)
c***********************************************************************
      implicit none
C           ----------------------->Argomenti della subroutine
c                       INPUT  
      real*4    min_rev   !numero di giri minimo
     *	   ,max_rev   !numero di giri massimo
     *       ,qlim(*)   !valori limite di q  
     *	   ,cen(*)    !coefficienti di eff=f(q,n) nella zona normale 
     *       ,cec(*)    !coefficienti di eff=f(q,n) nella zona di choking 
     *	   ,clim(*)   !costanti delle curve limite del compressore :   (dim=4)
	                  !zona di choking per l'efficienza:
	                  !(1) curva di choking,(2) curva operating limit
	                  !zona di choking per il numero di giri:
	                  !(3) curva di choking,(4) operating limit
C                       OUTPUT
      integer*4 neff      ! numero di valori dell'efficienza calcolati

	real*4 effic(*)     !valori costanti efficienza calcolati(adimensionali;
                        !	gli ultimi due,se il loro calcolo è possibile,
					  ! sono relativi alla zona di choking;
	                  ! dim=neff)
c           ----------------------->Variabili locali
      
	integer*4 nin      ! numero di punti in cui viene suddiviso un'intervallo
                       !                                     nelle partizioni
c     
	real*4    dint     ! passo di discretizzazione
     *	   ,eff_max  ! massima efficienza del compressore
     *       ,eff_min  ! minima efficienza del compressore
     *       ,eff_min1  ! minima efficienza del compressore
     *	   ,eff      ! massima efficienza per ogni fissato n
     *   	   ,ri       ! i-esimo valore del numero di giri fissato
     *	   ,qi       ! valore di q dell'i-esimo punto di max efficienza
     *	   ,qmr      ! rapporto (q/n)
     *	   ,effaux1  ! variabili introdotte per la ricerca della minima 
     *       ,effaux2  !                                         efficienza
     	 
	integer*4 i        ! variabile usata nei cicli
c     *       ,ne       ! variabile ausiliaria
	
	
c------------------------------------------------------------------------------
c------------------------------>            CALCOLO DEI VALORI DELL'EFFICIENZA
c	
c----------------------------------->       ricerca massima efficienza(eff_max)
c****************************************************************************
c     per ogni n (=numero di giri) fissato eff=f(q) è una parabola del tipo
c     eff=c(1)+c(2)q+c(3)q^2 il cui massimo è assunto per q=-[c(2)/2*c(3)];
c     il luogo dei massimi dell'efficienza è una curva del tipo (q/n)=cost;
c*****************************************************************************  
      eff_max=0.
                          ! partizione dell'intervallo [min_rev,max_rev]:
	nin = 20                          ! punti della partizione;
	dint=(max_rev-min_rev)/(nin-1)    ! passo di discretizzazione
c	
	do i=1,nin
	   ri=min_rev+(i-1)*dint          ! numero di giri
	   qi=-cen(2)*ri/(2*cen(3))       ! portata
         call effdaqn_a(eff,qi,ri,cen,cec,clim)
	   if(i.eq.1) eff_max=eff
	   if(eff.gt.eff_max)  eff_max=eff
	end do
c---------------------------------->         ricerca minima efficienza (eff_min) 
c                                            sulla curva a numero minimo di giri
	effaux1 = eff_max
                         ! partizione dell'intervallo [qlim(3),qlim(6)]:
      nin=20                ! punti della partizione;
	dint=(qlim(6)-qlim(3))/(nin-1)       ! passo di discretizzazione
c
	do i=1,nin		
	   qi = qlim(3)+(i-1)*dint
         call effdaqn_a(eff,qi,min_rev,cen,cec,clim) 
	   if (eff.lt.effaux1) effaux1 = eff
	end do
c---------------------------------->         ricerca minima efficienza  
c                                            sulla curva a numero massimo di giri
	effaux2 = eff_max
                         ! partizione dell'intervallo [qlim(1),qlim(5)]:
      nin=20                ! punti della partizione;
	dint=(qlim(5)-qlim(1))/(nin-1)       ! passo di discretizzazione
c
	do i=1,nin		
	   qi = qlim(1)+(i-1)*dint
         call effdaqn_a(eff,qi,max_rev,cen,cec,clim) 
	   if (eff.lt.effaux2) effaux2 = eff
	end do
c
      eff_min=max(effaux1,effaux2)
c---------------------------------->DETERMINAZIONE VALORI COSTANTI DELL'EFFICIENZA
c
C----->            determinazione del numero di curve da graficare(3<=ne<=10)                    
c
	eff_max = int(eff_max*100.)/100. !considero solo le prime 2 cifre decimali     
	eff_min = int(eff_min*100.+1.)/100. !prendo le prime 2 decimali dopo aver 
	                                    !arrotondato
	                              ! partizione dell'intervallo [eff_min,eff_max]:

      dint = 0.01
      do while ((eff_max-eff_min)/dint .gt. 10)
        dint = dint + 0.01
      enddo	 
c	                                      
c----->	                 determinazione valori efficienza 
c
	neff=1 
      effic(neff)=eff_max 
	eff_min1=eff_min+dint                         
c-prova	do while (neff.le.10.and.effic(neff).ge.eff_min)                             
	do while (neff.lt.10.and.effic(neff).ge.eff_min1)                             
	   effic(neff+1) = effic(neff)-dint     
	   neff=neff+1  
	end do                                             
	return                                       
	end                                          
c-----------------------------------------------------------------------------------------------
      subroutine limit_curve_ec(np,flag_lim,nlim,clim,lim_q,lim_h,qlim,
     *              chn,min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,hol)
c****************************************************************************************
c     calcola le coordinate (dimensionali) dei punti usati per disegnare le curve 
c     anti-sourge,anti-choke e operating-limit.
c******************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c--------------------------------------->Argomenti della subroutine
c---------------------------------->Input
c
      real*4 min_rev !numero di giri minimo
     *    ,max_rev !numero di giri massimo
c
      integer*4 np  !numero di punti da calcolare

	logical*2 flag_lim !flag curva limite:
	                   !true=curva limite si,cioè l'utente ha assegnato pti
	                   !     limite in maschera
	                   !false=curva limite no,cioè l'utente NON ha assegnato pti
	                   !     limite in maschera
c     
      integer*4 nlim !numero di punti limite assegnati
c
      real*4 clim(*)  !costanti delle curve limite del compressore : (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
	              !(3) curva di choking,(4) operating limit
     *    ,lim_q(*)!vettore dei valori estremi della portata (dim=nlim)
     *    ,lim_h(*)!vettore dei valori estremi della prevalenza (dim=nlim)
     *	,qlim(*) !valori limite della portata (adimensionali) (dim=6)
     *    ,chn(*)  !coefficienti della curva caratteristica di un compressore
                   !(H=f(Q,n)) nella zona normale    (dim=6) 
c
c---------------------------------->Output
c
      real*4 qs(*) !ascisse dei punti calcolati sull'anti-sourge (dim=nlim)
     *	,hs(*) !ordinate dei punti calcolati sull'anti-sourge(dim=nlim)
     *    ,qst(*)!ascisse dei punti calcolati sull'anti-surge teorica
     *    ,hst(*)!ordinate dei punti calcolati sull'anti-surge teorica
     *	,qc(*) !ascisse dei punti calcolati sull'anti-choke  (dim=np)
     *	,hc(*) !ordinate dei punti calcolati sull'anti-choke (dim=np)
     *	,qol(*)!ascisse dei punti calcolati sulla operating limit (dim=np)
     *	,hol(*)!ordinate dei punti calcolati sulla operating limit (dim=np)
c                (le coordinate calcolate sono DIMENSIONALI)
      integer*4 ip !punti calcolati per disegnare la curva di surge teorica
c                
c--------------------------------------->Variabili locali
c
	real*4 qmini !minimo valore di q sulle curve 
     *	,qmaxi !massimo valore di q sulle curve 
     *	,dq    !passo di discretizzazione
     *    ,qq
     *    ,hh
     *    ,rev

      integer*4 rev_da
	real*4 max
      real*4 EPS_Q/0.001/,min_q   ! costanti usate per definire i valori 
                                  ! teorici sulla spezzata di surge  	   
c
	integer*4 i,irange,i1,i2,j
c----------------------------------------------------------------------------------------------
c*******************************************************************************************
c    La spezzata anti-sourge si disegna a partire dai punti limite assegnati.
c    La curva di antisurge teorica è definita come la curva che congiunge i massimi sulle 
c    parabole a numero di giri costante più un 10% in termini di portata.
c    Per le altre curve limite vengono calcolate le coordinate di np punti,la cui ascissa 
c    è ottenuta discretizzando l'intervallo [qmini,qmaxi] usando un passo dq 
c*******************************************************************************************
c----------------------------------------->SPEZZATA ANTI-SOURGE
c I valori dei vettori lim_q e lim_h sono calcolati dalla coef_compr
c
	do i=1,nlim              !dimensionalizzazione
	   qs(i)=lim_q(i)*qmrif
         hs(i)=lim_h(i)*headrif
	end do
c
c----------------------------------------->CURVA DI ANTISURGE TEORICA
c Se l'utente ha definito la spezzata di surge sperimentale la curva di surge teorica
c viene definita come luogo dei massimi delle parabole a giri costanti (con una tolleranza
c dell 1%)
      if (flag_lim) then
          ip=1
	    qq=(-((chn(4)*min_rev+chn(2))/(2*chn(5))))*1.01
          MIN_Q = EPS_Q*ip
          IF(qq.LT.MIN_Q) then
		  qst(ip)=MIN_Q*qmrif
	    else
	      qst(ip)=qq*qmrif
	    end if
	    call hdaqn_a(hst(ip),qq,min_rev,chn)
		hst(ip)=hst(ip)*headrif
!	    rev_da=int(min_rev*10)+1
          rev_da=int(nint(min_rev*100.)/10.)+1 !Serve per evitare 
	                   ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                   ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre
	    max=max_rev*10
          do while(rev_da.lt.max)
             ip=ip+1
	       rev=rev_da/10.
	       qq=(-((chn(4)*rev+chn(2))/(2*chn(5))))*1.01
             MIN_Q = EPS_Q*ip
c-ele se il valore di portata calcolato qq è inferiore al valore di tolleranza
c     porto il valore di qq al suo limite minimo, altrimenti dimensionalizzo il valore 
c     calcolato
             IF(qq.LT.MIN_Q) then
		      qst(ip)=MIN_Q*qmrif
	       else
		      qst(ip)=qq*qmrif
	       end if
c-ele
		   call hdaqn_a(hst(ip),qq,rev,chn)
		   hst(ip)=hst(ip)*headrif
             rev_da=rev_da+1
          end do
          ip=ip+1
	    qq=(-((chn(4)*max_rev+chn(2))/(2*chn(5))))*1.01
          MIN_Q = EPS_Q*ip
c-ele se il valore di portata calcolato qq è inferiore al valore di tolleranza
c     porto il valore di qq al suo limite minimo, altrimenti dimensionalizzo il valore 
c     calcolato
          IF(qq.LT.MIN_Q) then 
		   qst(ip)=MIN_Q*qmrif
          else
	       qst(ip)=qq*qmrif
	    end if
c-ele
	    call hdaqn_a(hst(ip),qq,max_rev,chn)
		hst(ip)=hst(ip)*headrif
c-ele 30/8/2006 se l'utente non definisce la spezzata si ha un'unica curva di surge, quella 
c               teorica, i cui valori vengono assegnati sia ai vettori (qs,hs) che (qst,hst)
      else 
	  ip=nlim
	  do i=1,nlim              
	     qst(i)=qs(i)
           hst(i)=hs(i)
	  end do
	end if
c
c----------------------------------------->CURVA ANTI-CHOKE (anti-choke relativo)
c
      qmini=qlim(4)*qmrif       !dimensionalizzazione
	qmaxi=qlim(2)*qmrif
	dq=(qmaxi-qmini)/(np-1)
	do i=1,np
	   qc(i)=qmini+(i-1)*dq
	   qq=qc(i)/qmrif
         call hdaq_ch_a(clim(3),hh,qq)
	   hc(i)=hh*headrif
	end do
c
c----------------------------------------->CURVA OPERATING-LIMIT (anti-choke assoluto)
C
      if (clim(3).ne.clim(4)) then    !la zona di choking è definita
	   qmini=qlim(6)*qmrif          !dimensionalizzazione
	   qmaxi=qlim(5)*qmrif
	   dq=(qmaxi-qmini)/(np-1)
	   do i=1,np
	      qol(i)=qmini+(i-1)*dq
	      qq=qol(i)/qmrif
            call hdaq_ch_a(clim(4),hh,qq)
	      hol(i)=hh*headrif
	   end do
	else          !la curva anti-choke e la operating limit coincidono
	  do i=1,np
	     qol(i)=qc(i)
	     hol(i)=hc(i)
	  end do
	end if
	return
	end
c
c----------------------------------------------------------------------------------------

cmar******************      EQUIDISTANZA GRAFICA    *****************************

      subroutine limit_curve_ec_EQUI(k_equi_min, k_equi_max,
     *              alim,blim,np,flag_lim,nlim,clim,
     *            lim_q,lim_h,qlim,hlim,
     *              chn,chc,min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,
     *               hol,nse,qse, hse, nce,qce, hce)
c****************************************************************************************
c     calcola le coordinate (dimensionali) dei punti usati per disegnare le curve 
c     anti-sourge,anti-choke e operating-limit.
c******************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c--------------------------------------->Argomenti della subroutine
c---------------------------------->Input

      real*4 	alim(*),blim(*) !surge sperimentale
c
      real*4 min_rev !numero di giri minimo
     *    ,max_rev !numero di giri massimo
c
      integer*4 np  !numero di punti da calcolare

	logical*2 flag_lim !flag curva limite:
	                   !true=curva limite si,cioè l'utente ha assegnato pti
	                   !     limite in maschera
	                   !false=curva limite no,cioè l'utente NON ha assegnato pti
	                   !     limite in maschera
 
c     
      integer*4 nlim !numero di punti limite assegnati
c
      real*4 clim(*)  !costanti delle curve limite del compressore : (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
	              !(3) curva di choking,(4) operating limit
     *    ,lim_q(*)!vettore dei valori estremi della portata (dim=nlim)
     *    ,lim_h(*)!vettore dei valori estremi della prevalenza (dim=nlim)
     *	,qlim(*) !valori limite della portata (adimensionali) (dim=6)
     *    ,hlim(*) ! valori limite della prevalenza (adimensionali) (dim=6)
     *    ,chn(*)  !coefficienti della curva caratteristica di un compressore
                   !(H=f(Q,n)) nella zona normale    (dim=6) 
     *    ,chc(*)

c
c---------------------------------->Output
c
      real*4 qs(*) !ascisse dei punti calcolati sull'anti-sourge (dim=nlim)
     *	,hs(*) !ordinate dei punti calcolati sull'anti-sourge(dim=nlim)
     *    ,qst(*)!ascisse dei punti calcolati sull'anti-surge teorica
     *    ,hst(*)!ordinate dei punti calcolati sull'anti-surge teorica
     *	,qc(*) !ascisse dei punti calcolati sull'anti-choke  (dim=np)
     *	,hc(*) !ordinate dei punti calcolati sull'anti-choke (dim=np)
     *	,qol(*)!ascisse dei punti calcolati sulla operating limit (dim=np)
     *	,hol(*)!ordinate dei punti calcolati sulla operating limit (dim=np)
c                (le coordinate calcolate sono DIMENSIONALI)
      integer*4 ip !punti calcolati per disegnare la curva di surge teorica

c            output equidistanza

	real*4 qse(max_np),hse(max_np),qce(max_np),hce(max_np)
     

c                
c--------------------------------------->Variabili locali
c
	real*4 qmini !minimo valore di q sulle curve 
     *	,qmaxi !massimo valore di q sulle curve 
     *	,dq    !passo di discretizzazione
     *    ,qq
     *    ,hh
     *    ,rev

      integer*4 rev_da
	real*4 max
      real*4 EPS_Q/0.001/,min_q   ! costanti usate per definire i valori 
                                  ! teorici sulla spezzata di surge  	   
c
	integer*4 i,irange,i1,i2,j

cmar   dichiarazione
   
      real*4 lim_q_min,lim_q_max
!	lim_n_min
      real*4 delta_h,lim_h_max,lim_h_min

!	real*4 lim_n_max,

      real*4 k_s_min_e,h_equi_s_m, h_equi_s_0

	real*4 q_equi_s_0, q_equi_s_m

      real*4	 h_h, h_equi_s(3*max_np), q_equi_s (3*max_np)

	real*4 k_equi_min, k_equi_max, k_s_max_e

      real*4 h_equi_c (max_np), q_equi_c(max_np)
	
	real*4 k_c_min_e, k_c_max_e

	real*4 h_equi_c_0, q_equi_c_0, h_equi_c_m, q_equi_c_m

	integer*4 nh, nlim_m

	integer*4 jj

	real*4 q_equi_st(max_np),h_equi_st(max_np), q_equi_t
	real*4 qse_s(max_np),hse_s(max_np)
	integer*4 nse, nce
cmar

      logical*2 flag_lim_m

c ------------------------------------------------------------------------------------------
cmar

c----------------------------------------------------------------------------------------------
c*******************************************************************************************
c    La spezzata anti-sourge si disegna a partire dai punti limite assegnati.
c    La curva di antisurge teorica è definita come la curva che congiunge i massimi sulle 
c    parabole a numero di giri costante più un 10% in termini di portata.
c    Per le altre curve limite vengono calcolate le coordinate di np punti,la cui ascissa 
c    è ottenuta discretizzando l'intervallo [qmini,qmaxi] usando un passo dq 
c*******************************************************************************************

c--------------EQUI GRAFICA---------

c I valori dei vettori lim_q e lim_h sono calcolati dalla coef_compr
c
c
cmar ------------------------------
cmar 
       

cmar ------------------------------
      flag_lim_m=flag_lim
      flag_lim = .true.
cmar
  

      lim_q_min = lim_q(1)
      lim_q_max = lim_q(nlim)
	lim_h_min = lim_h(1)
	lim_h_max = lim_h(nlim)


	nh = 10

      delta_h = lim_h_max - lim_h_min

	h_h = delta_h/nh


      h_equi_s (1) = lim_h_min


      do i = 2, nh+1

      h_equi_s (i) = h_equi_s (i-1) + h_h

	enddo

      q_equi_s(1)=lim_q_min

     

cmar_13-05	  do i = 1,nlim

cmar	  if( h_equi_s(i) .lt. lim_h(i+1) ) then


cmar              do j=1,nh
cmar	         q_equi_s (j+1) = ( h_h/ blim(i+1) ) + q_equi_s (j)
	          
cmar	         enddo

cmar	  endif
cmar_13-05      enddo

cmar_13-05
cmar  correzione algoritmo di calcolo sulla surge, rispetto i pti limite
	 
c	 	 do i = 2,nlim

c	i=2

	jj=1
	j=1

	   do i=2, nlim   

      do  while(h_equi_s(j).gt.0.000001.and.h_equi_s(j).lt.lim_h(i)) 
c      do  while(h_equi_s(j).lt.lim_h(i))

c	    if (h_equi_s(j) .eq. lim_h(i)) goto 15

c	    do j=jj,nh+1

           q_equi_s (j+1) = ( h_h/ blim(i) ) + q_equi_s (j)

	j=j+1

c         if(j.eq.(nh+1))then
          enddo
c15    return        
           jj=j   
c	     i=i+1
		   			   
      enddo
cmar_13-05
	do j = 1, nh+1
       q_equi_s (j) =  q_equi_s (j) *(1 + k_equi_min)  
      enddo

c     calcolo l'estremo inferiore  
      k_s_min_e = lim_h_min/(q_equi_s(1)**2)

	call qdan_ch_a(k_s_min_e,q_equi_s_0, min_rev, chn)
	call hdaq_ch_a(k_s_min_e,h_equi_s_0, q_equi_s_0)

c     calcolo l'estremo superiore 

      k_s_max_e = lim_h_max/(q_equi_s(nh+1)**2)

	call qdan_ch_a(k_s_max_e,q_equi_s_m, max_rev, chn)
	call hdaq_ch_a(k_s_max_e,h_equi_s_m, q_equi_s_m)

      

 
c----------------------------------------->SPEZZATA ANTI-SOURGE


	do i=1,nlim              !dimensionalizzazione
	   qs(i)=lim_q(i)*qmrif
         hs(i)=lim_h(i)*headrif
	end do

c      if(h_equi_s(1).lt.h_equi_s_0) then

c        dimensionalizzo qse e hse
      
!	qse_s (1) = lim_q_min() * qmrif
!	hse_s (1) = lim_q_min * headrif
c      endif
	
      do i=1, nh+1
       qse_s(i+1)=q_equi_s(i)*qmrif
	 hse_s(i+1)=h_equi_s(i)*headrif
      enddo

      qse_s(1)= lim_q_min * qmrif
      hse_s(1)= h_equi_s (1) * headrif



c       if(h_equi_s(nh+1) .gt. h_equi_s_m ) then
	qse_s (nh+2) = q_equi_s_m * qmrif
	hse_s (nh+2) = h_equi_s_m * headrif
c      endif 

c così ho definito qse, hse - surge equi 





c----------------------------------------->CURVA DI ANTISURGE TEORICA
c Se l'utente ha definito la spezzata di surge sperimentale la curva di surge teorica
c viene definita come luogo dei massimi delle parabole a giri costanti (con una tolleranza
c dell 1%)
      if (flag_lim) then
          ip=1
	    qq=(-((chn(4)*min_rev+chn(2))/(2*chn(5))))*1.01

	    
		 
          MIN_Q = EPS_Q*ip
          IF(qq.LT.MIN_Q) then
		  qst(ip)=MIN_Q*qmrif
	    else
	      qst(ip)=qq*qmrif
	    end if
cmar
	    q_equi_t=qq
          
          q_equi_t=q_equi_t*(1+k_equi_min)

	    call hdaqn_a(hst(ip),qq,min_rev,chn)
		hst(ip)=hst(ip)*headrif

           q_equi_st(1) = qq*qmrif

           q_equi_st(2) = q_equi_t*qmrif
!---------------------------------------------------------------
	    call hdaqn_a(h_equi_st(ip),q_equi_t,min_rev,chn)

	     h_equi_st(1)=h_equi_st(ip)*headrif
           h_equi_st(2)= h_equi_st(1)

         
cmar17          q_equi_st(ip) = q_equi_t*qmrif
c-17          h_equi_st(ip) = h_equi_st(ip)*headrif
!----------------------------------------------------------------



cmar17         q_equi_st(ip) = qst(ip)
cmar17          h_equi_st(ip) = hst(ip)
cmar

!	    rev_da=int(min_rev*10)+1
          rev_da=int(nint(min_rev*100.)/10.)+1 !Serve per evitare 
	                   ! che possibili arrotondamenti modificano 
					   ! il calcolo della funzione int
	                   ! Viene moltiplicato *100 perchè il valore min_rev 
					   ! è normalizzato a cento e il dato di input è al massimo 2 cifre
	    max=max_rev*10
          do while(rev_da.lt.max)
             ip=ip+1
	       rev=rev_da/10.
	       qq=(-((chn(4)*rev+chn(2))/(2*chn(5))))*1.01
             MIN_Q = EPS_Q*ip
c-ele se il valore di portata calcolato qq è inferiore al valore di tolleranza
c     porto il valore di qq al suo limite minimo, altrimenti dimensionalizzo il valore 
c     calcolato
             IF(qq.LT.MIN_Q) then
		      qst(ip)=MIN_Q*qmrif
	       else
		      qst(ip)=qq*qmrif
	       end if
c-ele         
cmar
            q_equi_t=qq
	      q_equi_t=q_equi_t*(1+k_equi_min)
             
		   call hdaqn_a(hst(ip),qq,rev,chn)
		   hst(ip)=hst(ip)*headrif

c	call hdaqn_a(h_equi_st(ip),q_equi_t,min_rev,chn)
         
          q_equi_st(ip+1) = q_equi_t*qmrif
          h_equi_st(ip+1) = hst(ip)

cmar

             rev_da=rev_da+1
          end do
          ip=ip+1
	    qq=(-((chn(4)*max_rev+chn(2))/(2*chn(5))))*1.01
          MIN_Q = EPS_Q*ip
c-ele se il valore di portata calcolato qq è inferiore al valore di tolleranza
c     porto il valore di qq al suo limite minimo, altrimenti dimensionalizzo il valore 
c     calcolato
          IF(qq.LT.MIN_Q) then 
		   qst(ip)=MIN_Q*qmrif
          else
	       qst(ip)=qq*qmrif
	    end if

c-ele
cmar
              q_equi_t=qq
	      q_equi_t=q_equi_t*(1+k_equi_min)
	    call hdaqn_a(hst(ip),qq,max_rev,chn)
		hst(ip)=hst(ip)*headrif

	call hdaqn_a(h_equi_st(ip),q_equi_t,max_rev,chn)
cmar
      

cmar          
          q_equi_st(ip) = q_equi_t*qmrif
          h_equi_st(ip) = h_equi_st(ip)*headrif

cmar
      q_equi_st(1) = qst(1)
      h_equi_st(1) = hst(1)

c-ele 30/8/2006 se l'utente non definisce la spezzata si ha un'unica curva di surge, quella 
c               teorica, i cui valori vengono assegnati sia ai vettori (qs,hs) che (qst,hst)
      else 
	  ip=nlim
	  do i=1,nlim              
	     qst(i)=qs(i)
           hst(i)=hs(i)
	  end do
!!!!!!!!      
	if(k_equi_min .lt.0.0001) then

	do i=1,ip+10            
	    q_equi_st(i) = qst(i)
          h_equi_st(i)= hst(i)
	  end do
      
	do i=1,nlim+10

	qse_s (i) = qs(i)
	hse_s (i) = hs(i)
      
	end do

      end if

      


!!!!!!!!!
	 end if
cmar   
      
CMAR-17       if (qse_s(nh+1) .gt. q_equi_st(ip) ) then 
         if (qse_s(1) .gt. q_equi_st(1) ) then 
c          if (qs(nlim) .ge. qst(ip) ) then 

cmar   sostituito da:
  
cmar      if (nlim .ne.0 ) then 

      do i= 1, nh+2

          qse(i)=qse_s(i)
	    hse(i)=hse_s(i)
          nse=nh+2	
	end do
       
c	else if (qs(nlim) .eq. qst(ip)) then

      else

      do i= 1, ip

          qse(i)=q_equi_st(i)
	    hse(i)=h_equi_st(i)
          nse=ip
	end do
	

      end if

	flag_lim = flag_lim_m
      
      


c      end do
c
c----------------------------------------->CURVA ANTI-CHOKE (anti-choke relativo)
c
      qmini=qlim(4)*qmrif       !dimensionalizzazione
	qmaxi=qlim(2)*qmrif
	dq=(qmaxi-qmini)/(np-1)
	do i=1,np
	   qc(i)=qmini+(i-1)*dq
	   qq=qc(i)/qmrif
         call hdaq_ch_a(clim(3),hh,qq)
	   hc(i)=hh*headrif
	end do
c
c----------------------------------------->CURVA OPERATING-LIMIT (anti-choke assoluto)
cmar  ++++++++++++++++++++++++++++++++++++++++++++++++++++++
      delta_h = hlim(5) - hlim(6)

	h_h = delta_h/nh


      h_equi_c (1) = hlim(6)


      do i = 2, nh+1

      h_equi_c (i) = h_equi_c (i-1) + h_h

	enddo

      q_equi_c(1)=qlim(6)

    
c	  do i = 1,nlim

c	  if( h_equi_s(i) .lt. lim_h(i+1) ) then

              do j=1,nh

	        call qdah_ch_a (clim(4),h_equi_c(j+1), q_equi_c (j+1) )
	         
	          
	         enddo

	do j = 1, nh+1
       q_equi_c(j) =  q_equi_c(j) *(1 - k_equi_max)  
      enddo

c     calcolo l'estremo inferiore  
      k_c_min_e = hlim(6)/(q_equi_c(1)**2)

	call qdan_ch_a(k_c_min_e,q_equi_c_0, min_rev, chc)
	call hdaq_ch_a(k_c_min_e,h_equi_c_0, q_equi_c_0)

c     calcolo l'estremo superiore 

      k_c_max_e = hlim(5)/(q_equi_c(nh)**2)

	call qdan_ch_a(k_c_max_e,q_equi_c_m, max_rev, chc)
	call hdaq_ch_a(k_c_max_e,h_equi_c_m, q_equi_c_m)



	qce (1) = q_equi_c_0 * qmrif
	hce (1) = h_equi_c_0 * headrif

      do i=2, nh+1
       qce(i)=q_equi_c(i)*qmrif
	 hce(i)=h_equi_c(i)*headrif
      enddo
      
	qce (nh+2) = qlim(5) * qmrif
	hce (nh+2) = h_equi_c(nh+1) * headrif

      nce = nh + 2


	
cmar	 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


C
      if (clim(3).ne.clim(4)) then

	    !la zona di choking è definita
	   qmini=qlim(6)*qmrif          !dimensionalizzazione
	   qmaxi=qlim(5)*qmrif
	   dq=(qmaxi-qmini)/(np-1)
	   do i=1,np
	      qol(i)=qmini+(i-1)*dq
	      qq=qol(i)/qmrif
            call hdaq_ch_a(clim(4),hh,qq)
	      hol(i)=hh*headrif
	   end do
	else          !la curva anti-choke e la operating limit coincidono
	  do i=1,np
	     qol(i)=qc(i)
	     hol(i)=hc(i)
	  end do
	end if


!*************************************************
	return
	end
c

cmar *************** ******************** ********************



c ------------------------------------------------------------------

      subroutine limit_scale(qlim,hlim,qmini,qmaxi,hmini,hmaxi)
c********************************************************************************
c     determina i valori limite (dimensionali) per scalare gli assi nei grafici 
c     delle curve caratteristiche di un compressore  ;
c********************************************************************************
c     le coordinate (q,h) passate come input sono adimensionali;la dimensionaliz_
c     zazione si effettua moltiplicando q e h per i valori di riferimento QMRIF e
c     HEADRIF, rispettivamente, definiti nel file 'reference,inc'	
c******************************************************************************
      implicit none        
      include '../inc/rk_param.inc'
c     
C         -----------> Argomenti della subroutine
c                  INPUT
      real*4 qlim(*) !valori limite della portata (dim=6)
     *	,hlim(*) !valori limite dell'altezza adiabatica (dim=6)
c	             OUTPUT
      real*4 qmini   !limite minimo di q sull'asse delle ascisse
     *	,qmaxi   !limite massimo di q sull'asse delle ascisse
     *    ,hmini   !limite minimo di h sull'asse delle ordinate
     *    ,hmaxi   !limite massimo di h sull'asse delle ordinate
                   
      
c*******************************************************************
C     Punto di intersezione tra la curva anti-surge e la curva a numero
c           di giri minimo  ------------> minimo valore di q
C      qmini = INT(qlim(3)*qmrif)-1
      qmini = INT(qlim(3)*qmrif)
      IF(qmini.LT.0.) qmini = 0.
C
C     Punto di intersezione tra la curva anti-surge e la curva a numero 
c           di giri massimo ------------> massimo valore di h
      hmaxi = INT(hlim(1)*headrif)+1
C
C     Punto di intersezione tra la curva operting limit (anti-choke assoluto)
c     e la curva a numero di giri massimo ------------> minimo valore di h 
C      hmini = INT(hlim(6)*headrif)-1
      hmini = INT(hlim(6)*headrif)
      IF(hmini.LT.0.) hmini = 0.
C
C     Punto di intersezione tra la curva operating limit (anti-choke assoluto)
c     e la curva a numero di giri minimo -------------> massimo valore di q
      qmaxi = INT(qlim(5)*qmrif)+1
      return
      end
c

c---------------------------------------------------------------------------------------
      subroutine eff_curve_qe(np,ngiri,giri,cen,chn,chc,
     *                        nlim,alim,blim,clim,lim_n,
     *                        ef_q,eta)
c***************************************************************************************
c calcola le coordinate dei punti usati per disegnare nel piano q-eta le curve eta(n,q) 
c (descriventi l'andamento dell'efficienza) per diversi valori fissati del numero di giri;
c il valore calcolato di q è dimensionale,quello di eta assoluto invece che percentuale
c****************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c------------------------------------->Argomenti della subroutine
c---------------------->Input
      integer*4 np,           !numero di punti da calcolare
     *        ngiri         !numero di valori costanti fissati per i giri
c
      real*4 giri(*),         !valori costanti dei giri (dim=ngiri)
     *     cen(*),          !coeff di eff(n,q) nella zona normale (dim=5)
     *     chn(*),           !coeff di h(n,q) nella zona normale (dim=6)
     *     chc(*)           !coeff di h(n,q) in zona choking (dim=6)
c
      integer*4 nlim          !numero di punti limite assegnati      
c
      real*4 alim(*),         !termini noti della spezzata anti-surge(dim=nlim+1)
     *     blim(*),         !coeff angolari spezzata anti-surge(dim=nlim+1)
     *     clim(*),         !costanti delle curve limite del compressore(dim=4)
     *     lim_n(*)         !valori limite dei giri(dim=nlim)

c---------------------->Output
      real*4 ef_q(max_ngiri,max_np),
     *     eta(max_ngiri,max_np)
c---------------------->Variabili locali
      integer*4 k,
     *        i,
     *        irange
c
      real*4 rev,
     *     a,
     *     b,
     *     c,
     *     qmini,
     *     qmaxi,
     *     dq,
     *     qi
c-------------------------------------------------------------------------------------
      do k=1,ngiri
         rev=giri(k)
         a=cen(1)+cen(4)*rev+cen(5)*rev*rev
         b=cen(2)/rev
         c=cen(3)/rev/rev
         call qdan_su_a(chn,alim,blim,nlim,lim_n,rev,qmini,irange)
	      if (clim(3).eq.clim(4)) then
               call qdan_ch_a(clim(3),qmaxi,rev,chn)
	      else
               call qdan_ch_a(clim(4),qmaxi,rev,chc)
	      end if
         dq=(qmaxi-qmini)/(np-1)
         do i=1,np
            qi=qmini+(i-1)*dq
            ef_q(k,i)=qi*qmrif
            eta(k,i)=a+b*qi+c*qi*qi
         end do
      end do
	return
	end
c--------------------------------------------------------------------------------------
      subroutine compr_pt_old(nom_rev,max_rev,min_rev,min_eff,
     *                    chn,chc,cen,
     *                   flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
     *                   np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
     *                   qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
     *                   ef_q,eta,
     *                   qmini,qmaxi,hmini,hmaxi)
c**************************************************************************************
c     Calcola le coordinate (dimensionali) dei punti utilizzati per disegnare 
c     i grafici delle curve caratteristiche di un compressore nel piano HQ
c**************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c------------------------------------------>Argomenti della subroutine
c-----------------------------------> Input
c
cmar      real*4 k_equi_min, k_equi_max
      real*4 nom_rev !valore nominale del numero di giri
     *	,max_rev !numero di giri massimo    
     *	,min_rev !numero di giri minimo
     *    ,min_eff !efficienza minima
     *	,chn(*)  !coefficienti di h=f(n,q) nella zona normale (dim=6)
     *	,chc(*)  !coefficienti di h=f(n,q) nella zona di choking (dim=6)
     *	,cen(*)  !coefficienti di eff=f(n,q) nella zona normale (dim=5)
c  
      logical*2 flag_lim  !curva limite SI/NO
	                    !se SI disegno anche l'AntiSurge Teorica   
      integer*4 nlim !numero di punti limite assegnati sulla spezzata anti-sourge
c     
      real*4 alim(*) !vettore dei termini noti della spezzata di surge(dim=nlim+1)
     *    ,blim(*) !vettore dei coefficienti angolari della spezzata di surge
	             !                                                (dim=nlim+1)
     *	,clim(*)  !costanti delle curve limite del compressore :     (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
	              !(3) curva di choking,(4) operating limit
     *    ,lim_n(*)!vettore dei valori estremi del numero di giri     (dim=nlim)
     *    ,lim_q(*)!vettore dei valori estremi della portata          (dim=nlim)
     *    ,lim_h(*)!vettore dei valori estremi della prevalenza       (dim=nlim)     
c
      integer*4 np    !numero di punti da calcolare per ogni curva
c
c      real*4 rev_pt,
c     *     h_pt
c----------------------------------> Output
      real*4 qs(*)  !ascisse dei pti sulla curva anti-sourge (dim=nlim)
     *	,hs(*)  !ordinate dei pti sulla curva anti-sourge(dim=nlim)
     *    ,qst(*) !ascisse dei pti sulla curva anti-sourge teorica (dim=ip)
     *    ,hst(*) !ordinate dei pti sulla curva anti-sourge teorica (dim=ip)
     *    ,qc(*)  !ascisse dei pti sulla curva anti-choke
     *    ,hc(*)  !ordinate dei pti sulla curva anti-choke
     *    ,qol(*) !ascisse dei pti sulla curva operating limit
     *	,hol(*) !ordinate dei pti sulla curva operating limit
c                 (sono tutti vettori di dimensione np)   
cmar
      real*4 qse(max_np),hse(max_np),qce(max_np),hce(max_np)
cmar

c-elena  
     *	,h_c(max_np) !eff limite zona normale
     *	,h_cc(max_np) !eff limite zona choking
      integer*4 ip  !punti calcolati per didegnare la curva teorica di anti-surge
c
      real*4 qrn(max_ngiri,*)!ascisse dei pti su curve a rev cost in zona normale 
     *    ,hrn(max_ngiri,*)!ordinate dei pti su curve a rev cost in zona normale
     *    ,qrc(max_ngiri,*)!ascisse dei pti su curve a rev cost in zona choking
     *    ,hrc(max_ngiri,*)!ordinate dei pti su curve a rev cost in zona choking
     *    ,qef1(max_neff,*)!ascisse dei pti su curve a eff cost (1°set di punti)
     *    ,hef1(max_neff,*)!ordinate dei pti su curve a eff cost
     *    ,qef2(max_neff,*)!ascisse dei pti su curve a eff cost (2°set di punti)
     *    ,hef2(max_neff,*)!ordinate dei pti su curve a eff cost
     *    ,ef_q(max_ngiri,*)!ascisse per l'eff nel piano Q-eta
     *    ,eta(max_ngiri,*) !ordinate per l'eff nel piano Q-eta
c                 (sono tutte matrici di np colonne)
c
      integer*4 ngiri   !curve a numero di giri costante da disegnare
     *       ,neff    !curve a efficienza costante da disegnare
      real*4 giri(*)  !valori costanti del numero di giri da graficare (dim=ngiri)
     *    ,effic(*) !valori dell'efficienza calcolati;l'ultimo o gli(dim=neff) 
	                     !ultimi 2 sono relativi alla zona di choking      

      real*4 qmini   !limite minimo di q sull'asse delle ascisse
     *	,qmaxi   !limite massimo di q sull'asse delle ascisse
     *    ,hmini   !limite minimo di h sull'asse delle ordinate
     *    ,hmaxi   !limite massimo di h sull'asse delle ordinate
                   ! (per disegnare le scale sugli assi)     
c----------------------------------> Variabili ausiliarie
	real*4 qlim(6) !valori limite della portata 
     *    ,hlim(6) !valori limite dell'altezza adiabatica 
     *	,cec(6)  !coefficienti di eff=f(n,q) nella zona di choking 
c
      integer*4 j,i,status
c-elena
      character*(150) o_name_file

      integer*4 nlim_app      
      real*4 lim_n_app(max_nlim),     
     *	 lim_q_app(max_nlim)     
      real*4 lim_h_app(max_nlim)      
      real*4 alim_app(max_nlim+1),      
     *     blim_app(max_nlim+1)       


cmar
cmar      real*4 k_equi_min, k_equi_max, 
c***************************************************************************************
c     Controllo sul numero di punti
cmar      nlim_m=0 
      if (np.eq.max_np) np=np-2

c
c     Adimensionalizzazioni
c
	do j=1,nlim
	   lim_q(j)=lim_q(j)/qmrif/1000.   !trasformo in km^3/h
	   lim_h(j)=lim_h(j)/headrif/1000. !trasformo in km
	   lim_n(j)=lim_n(j)/nom_rev
	end do

	do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_h_app(i)=lim_h(i)
	   alim_app(i)=alim(i)
	   blim_app(i)=blim(i)
	end do
	nlim_app=nlim

      call filtra_plim(nlim,lim_n,lim_q,lim_h,alim,blim)
c-17
	alim(nlim+1)=alim(nlim)
	blim(nlim+1)=blim(nlim)



CMAR ----------- equi ---------------  grafica  ---------  -------------

c       k_equi = k_equi_min + k_equi_max

C     if(k_equi.ne.0.0) then

c      call calcola_punti_equi_s ( 
c                    input : nlim,lim_n,lim_q,lim_h,alim,blim
c                    output: q_lim_e, h_lim_e) 




c-17
c     Calcolo delle coordinate adimensionali dei punti limite delle curve caratteristiche
c                                                                         del compressore
c
      call limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,lim_q,lim_h,
     *                  qlim,hlim)
c
c     Calcolo delle coordinate dimensionali dei punti utilizzati per disegnare le curve 
c                                              anti-sourge,anti-choke e operating-limit

cmar     -----------------------------------------                   ------------------
c      if (k_equi.ne. 0.0 ) then
c
c
c       call limit_curve_ec_equi(alim, blim,np,flag_lim,nlim,clim,lim_q,lim_h,qlim,chn,
c     *	        min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,hol,q_equi,h_equi)

c      call limit_curve_ec_EQUI(k_equi_min, k_equi_max,nlim_m,
c     *                  alim,blim,np,flag_lim,nlim,clim,lim_q,
c     *                         lim_h,qlim,hlim,
c     *              chn,chc,min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,
c     *               hol,qse, hse, qce, hce)

c      else
 


      call limit_curve_ec(np,flag_lim,nlim,clim,lim_q,lim_h,qlim,chn,
     *	        min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,hol)
c
c     Calcolo delle coordinate dimensionali dei punti utilizzati per disegnare le curve  
c                                                                ai vari numeri di giri
      call calcola_giri(max_rev,min_rev,ngiri,giri)
c
 	call rev_curve_ec(np,ngiri,giri,chn,chc,nlim,alim,blim,clim,
     *  	              qs,hs,qc,hc,qol,hol,lim_n,lim_q,lim_h,
     *                  qrn,hrn,qrc,hrc)

c
c     Calcolo delle coordinate dei punti utilizzati per disegnare le curve ad
c                                                          efficienza costante
c 
      call calcola_cec(min_eff,cen,clim,cec)
c
      call curv_eff_ec(min_rev,max_rev,qlim,cen,cec,clim,neff,effic)
c    
      call new_efficienza(np,neff,max_rev,min_rev,effic,
     *	                    nlim,alim,blim,clim,lim_h,cen,
     *                        min_eff,chn,chc,qef1,hef1,qef2,hef2)
c-prova
c     Calcolo delle coordinate dei punti usati per disegnare l'andamento dell'efficienza
c              nel piano Q-eta
      call eff_curve_qe(np,ngiri,giri,cen,chn,chc,
     *                  nlim,alim,blim,clim,lim_n,
     *                  ef_q,eta)
c
      do j=1,ngiri
	   giri(j)=giri(j)*100.
	end do

      do j=1,neff
	   effic(j)=effic(j)*100.
	end do

      do i=1,ngiri
	   do j=1,np
	      eta(i,j)=eta(i,j)*100.
	   end do
      end do
c	Calcolo dei valori limite dimensionali per scalare gli assi
c
	call limit_scale(qlim,hlim,qmini,qmaxi,hmini,hmaxi)
	return
	end
c-----------------------------------------------------------------------------------
cmar++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      subroutine compr_pt(k_equi_min, k_equi_max,flag_equi,
     *                    nom_rev,max_rev,min_rev,min_eff,
     *                    chn,chc,cen,
     *                   flag_lim,nlim,alim,blim,clim,lim_n,lim_q,lim_h,
     *                   np,qs,hs,qst,hst,ip,qc,hc,qol,hol,ngiri,giri,
     *                   qrn,hrn,qrc,hrc,neff,effic,qef1,hef1,qef2,hef2,
     *                   ef_q,eta,
     *                   qmini,qmaxi,hmini,hmaxi,
     *                     nse,qse, hse, nce,qce, hce)
c**************************************************************************************
c     Calcola le coordinate (dimensionali) dei punti utilizzati per disegnare 
c     i grafici delle curve caratteristiche di un compressore nel piano HQ
c**************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c------------------------------------------>Argomenti della subroutine
c-----------------------------------> Input
c
      real*4 k_equi_min, k_equi_max, k_equi
	logical*2 flag_equi
      real*4 nom_rev !valore nominale del numero di giri
     *	,max_rev !numero di giri massimo    
     *	,min_rev !numero di giri minimo
     *    ,min_eff !efficienza minima
     *	,chn(*)  !coefficienti di h=f(n,q) nella zona normale (dim=6)
     *	,chc(*)  !coefficienti di h=f(n,q) nella zona di choking (dim=6)
     *	,cen(*)  !coefficienti di eff=f(n,q) nella zona normale (dim=5)
c  
      logical*2 flag_lim  !curva limite SI/NO
	                    !se SI disegno anche l'AntiSurge Teorica   
      integer*4 nlim !numero di punti limite assegnati sulla spezzata anti-sourge
c     
      real*4 alim(*) !vettore dei termini noti della spezzata di surge(dim=nlim+1)
     *    ,blim(*) !vettore dei coefficienti angolari della spezzata di surge
	             !                                                (dim=nlim+1)
     *	,clim(*)  !costanti delle curve limite del compressore :     (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
	              !(3) curva di choking,(4) operating limit
     *    ,lim_n(*)!vettore dei valori estremi del numero di giri     (dim=nlim)
     *    ,lim_q(*)!vettore dei valori estremi della portata          (dim=nlim)
     *    ,lim_h(*)!vettore dei valori estremi della prevalenza       (dim=nlim)     
c
      integer*4 np    !numero di punti da calcolare per ogni curva
c
c      real*4 rev_pt,
c     *     h_pt
c----------------------------------> Output
      real*4 qs(*)  !ascisse dei pti sulla curva anti-sourge (dim=nlim)
     *	,hs(*)  !ordinate dei pti sulla curva anti-sourge(dim=nlim)
     *    ,qst(*) !ascisse dei pti sulla curva anti-sourge teorica (dim=ip)
     *    ,hst(*) !ordinate dei pti sulla curva anti-sourge teorica (dim=ip)
     *    ,qc(*)  !ascisse dei pti sulla curva anti-choke
     *    ,hc(*)  !ordinate dei pti sulla curva anti-choke
     *    ,qol(*) !ascisse dei pti sulla curva operating limit
     *	,hol(*) !ordinate dei pti sulla curva operating limit
c                 (sono tutti vettori di dimensione np)   
cmar
      real*4 qse(max_np),hse(max_np),qce(max_np),hce(max_np)
	integer*4 nse, nce
cmar

c-elena  
     *	,h_c(max_np) !eff limite zona normale
     *	,h_cc(max_np) !eff limite zona choking
      integer*4 ip  !punti calcolati per didegnare la curva teorica di anti-surge
c
      real*4 qrn(max_ngiri,*)!ascisse dei pti su curve a rev cost in zona normale 
     *    ,hrn(max_ngiri,*)!ordinate dei pti su curve a rev cost in zona normale
     *    ,qrc(max_ngiri,*)!ascisse dei pti su curve a rev cost in zona choking
     *    ,hrc(max_ngiri,*)!ordinate dei pti su curve a rev cost in zona choking
     *    ,qef1(max_neff,*)!ascisse dei pti su curve a eff cost (1°set di punti)
     *    ,hef1(max_neff,*)!ordinate dei pti su curve a eff cost
     *    ,qef2(max_neff,*)!ascisse dei pti su curve a eff cost (2°set di punti)
     *    ,hef2(max_neff,*)!ordinate dei pti su curve a eff cost
     *    ,ef_q(max_ngiri,*)!ascisse per l'eff nel piano Q-eta
     *    ,eta(max_ngiri,*) !ordinate per l'eff nel piano Q-eta
c                 (sono tutte matrici di np colonne)
c
      integer*4 ngiri   !curve a numero di giri costante da disegnare
     *       ,neff    !curve a efficienza costante da disegnare
      real*4 giri(*)  !valori costanti del numero di giri da graficare (dim=ngiri)
     *    ,effic(*) !valori dell'efficienza calcolati;l'ultimo o gli(dim=neff) 
	                     !ultimi 2 sono relativi alla zona di choking      

      real*4 qmini   !limite minimo di q sull'asse delle ascisse
     *	,qmaxi   !limite massimo di q sull'asse delle ascisse
     *    ,hmini   !limite minimo di h sull'asse delle ordinate
     *    ,hmaxi   !limite massimo di h sull'asse delle ordinate
                   ! (per disegnare le scale sugli assi)     
c----------------------------------> Variabili ausiliarie
	real*4 qlim(6) !valori limite della portata 
     *    ,hlim(6) !valori limite dell'altezza adiabatica 
     *	,cec(6)  !coefficienti di eff=f(n,q) nella zona di choking 
c
      integer*4 j,i,status
c-elena
      character*(150) o_name_file

      integer*4 nlim_app , nlim_m     
      real*4 lim_n_app(max_nlim),     
     *	 lim_q_app(max_nlim)     
      real*4 lim_h_app(max_nlim)      
      real*4 alim_app(max_nlim+1),      
     *     blim_app(max_nlim+1)       


cmar   per scrittura file

      integer*4 ii  

c***************************************************************************************
cmar


c      flag_lim = .true.




c     Controllo sul numero di punti

      if (np.eq.max_np) np=np-2

c
c     Adimensionalizzazioni
c
	do j=1,nlim
	   lim_q(j)=lim_q(j)/qmrif/1000.   !trasformo in km^3/h
	   lim_h(j)=lim_h(j)/headrif/1000. !trasformo in km
	   lim_n(j)=lim_n(j)/nom_rev
	end do

	do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_h_app(i)=lim_h(i)
	   alim_app(i)=alim(i)
	   blim_app(i)=blim(i)
	end do
	nlim_app=nlim

      call filtra_plim(nlim,lim_n,lim_q,lim_h,alim,blim)
c-17
	alim(nlim+1)=alim(nlim)
	blim(nlim+1)=blim(nlim)

CMAR ----------- equi ---------------  grafica  ---------  -------------

c       k_equi = k_equi_min + k_equi_max


c-17
c     Calcolo delle coordinate adimensionali dei punti limite delle curve caratteristiche
c                                                                         del compressore
c
      call limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,lim_q,lim_h,
     *                  qlim,hlim)
c
c     Calcolo delle coordinate dimensionali dei punti utilizzati per disegnare le curve 
c                                              anti-sourge,anti-choke e operating-limit

cmar     -----------------------------------------                   ------------------
c      if (k_equi.ne. 0.0 ) then
c
c
c       call limit_curve_ec_equi(alim, blim,np,flag_lim,nlim,clim,lim_q,lim_h,qlim,chn,
c     *	        min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,hol,q_equi,h_equi)


cmar
      if (flag_equi) then
cmar
      call limit_curve_ec_EQUI(k_equi_min, k_equi_max,
     *                      alim,blim,np,flag_lim,nlim,clim,lim_q,
     *                         lim_h,qlim,hlim,
     *             chn,chc,min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,
     *               hol,nse,qse, hse, nce,qce, hce)

      else
 


      call limit_curve_ec(np,flag_lim,nlim,clim,lim_q,lim_h,qlim,chn,
     *	        min_rev,max_rev,qs,hs,qst,hst,ip,qc,hc,qol,hol)

c
       endif
	 
	 
	  
c
c     Calcolo delle coordinate dimensionali dei punti utilizzati per disegnare le curve  
c                                                                ai vari numeri di giri
      call calcola_giri(max_rev,min_rev,ngiri,giri)
c
 	call rev_curve_ec(np,ngiri,giri,chn,chc,nlim,alim,blim,clim,
     *  	              qs,hs,qc,hc,qol,hol,lim_n,lim_q,lim_h,
     *                  qrn,hrn,qrc,hrc)

c
c     Calcolo delle coordinate dei punti utilizzati per disegnare le curve ad
c                                                          efficienza costante
c 
      call calcola_cec(min_eff,cen,clim,cec)
c
      call curv_eff_ec(min_rev,max_rev,qlim,cen,cec,clim,neff,effic)
c    
      call new_efficienza(np,neff,max_rev,min_rev,effic,
     *	                    nlim,alim,blim,clim,lim_h,cen,
     *                        min_eff,chn,chc,qef1,hef1,qef2,hef2)
c-prova
c     Calcolo delle coordinate dei punti usati per disegnare l'andamento dell'efficienza
c              nel piano Q-eta
      call eff_curve_qe(np,ngiri,giri,cen,chn,chc,
     *                  nlim,alim,blim,clim,lim_n,
     *                  ef_q,eta)
c
      do j=1,ngiri
	   giri(j)=giri(j)*100.
	end do

      do j=1,neff
	   effic(j)=effic(j)*100.
	end do

      do i=1,ngiri
	   do j=1,np
	      eta(i,j)=eta(i,j)*100.
	   end do
      end do
c	Calcolo dei valori limite dimensionali per scalare gli assi



cmar

c
	call limit_scale(qlim,hlim,qmini,qmaxi,hmini,hmaxi)
	return
	end
c-----------------------------------------------------------------------------------
cmar+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine trans_compr(npoint,flow,head,eff)
c***********************************************************************************
c    Converte le grandezze dei punti da disegnare sulle mappe dei compressori da UDM
c    interna a UDM di graficazione (conformi alle condizioni ISO)
c***********************************************************************************
      implicit none
      
	integer*4 npoint     !numero di punti assegnati
                           !coordinate relative a tali punti:
	real*4 flow(*)       !I/O) portata    (dim=npoint)
     *      ,head(*)       !I/O) prevalenza (dim=npoint)
     *      ,eff(*)        !I/O) efficienza (dim=npoint)

      integer*4 i
c------------------------------------------------------------------------------------
      do i=1,npoint
	   flow(i)=flow(i)/1000.
	   head(i)=head(i)/1000.
	   eff(i)=eff(i)*100.
	end do

      return
	end  !trans turb

c---------------------------------------------------------------------------------------------
c************************************************************************************************
      subroutine fine_spezzata(i1,i2,v1,v2)
      ! Introduce un solo "fine spezzata" e solo se la spezzata è composta da più di 2 punti 
	implicit none
      include '../inc/rk_param.inc'


      integer*4 i1,i2
      real*4 v1(max_neff,*),v2(max_neff,*)
	if (i2.eq.0) then
	  continue
	elseif (i2.eq.1) then
	  v1(i1,i2)=0.0
	  v2(i1,i2)=0.0
	  i2 = i2 -1
	else if (v1(i1,i2).ne.0.0) then
	  if (v1(i1,i2-1).ne.0.0) then
	    i2 = i2+1
	    v1(i1,i2)=0.0
	    v2(i1,i2)=0.0
	  else
	    v1(i1,i2)=0.0
	    v2(i1,i2)=0.0
	    i2 = i2 -1
	  endif
	endif
	return
	end
c************************************************************************************************
      subroutine choking_eff(clim,chn,chc,np,qc,hc,hcc)
!-------------------------------------------
! q e h sono coordinate di graficazione
!---------------------------------------- 
	implicit none
      include '../inc/rk_param.inc'

      real*4 clim(*),chn(*),chc(*)
    
      integer*4 np

	real*4 qc(*),hc(*),hcc(*)

	integer*4 i
	real*4 rev,qq,hh,hhc


	do i=1,np
	   qq=qc(i)/qmrif
	   rev=qq/clim(1)
         call hdaqn_a(hh,qq,rev,chn)
	   hc(i)=hh*headrif
         call hdaqn_a(hhc,qq,rev,chc)
	   hcc(i)=hhc*headrif
	end do
c
	return
	end
c*********************************************************************************************+
      subroutine new_efficienza(np,neff,max_rev,min_rev,effic,
     *	                    nlim,alim,blim,clim,lim_h,cen,
     *                        eff_min,chn,chc,qef1,hef1,qef2,hef2)
c***********************************************************************************************
c     calcola le coordinate dei punti usati per disegnare le curve a efficienza costante
C***********************************************************************************************
c     MODIFICHE ALLA VECCHIA VERSIONE:
c 1)  nel verificare se il punto di lavoro del compressore si trova entro la mappa  
c     si è tenuto conto del fatto che le due zone di funzionamento normale e di choking
c     coincidono per i giri e l'efficienza;a seconda dei dati a disposizione per stabilire dove 
c     si trova un punto si possono usare sia le due parabole di choking individuate dai coeff
c     clim(3) e clim(4) nel piano QH sia le due curve individuate dai coeff clim(1) e clim(2) sul
c     piano NQ.Quindi un punto che si trova nella zona normale per l'efficienza non può stare
c     nella zona di choking per i giri e analogamente un punto che si trova nella zona di choking
c     per l'efficienza non può stare nella zona normale per i giri.
c 2)  quando una parte del ramo di curva cade al di fuori della mappa si utilizza uno 0 per 
c     spezzare il ramo in più segmenti che andranno graficati singolarmente.
c***********************************************************************************************
      implicit none 
      include '../inc/rk_param.inc'
c--------------------------------------------->Argomenti della subroutine
c---------------------------------------->Input
      integer*4 np   !numero di punti da calcolare
     *	   ,neff !numero di curve a eff costante da tracciare
c
      real*4 max_rev !numero di giri massimo
     *	,min_rev !numero di giri minimo
     *	,effic(*)!valori costanti dell'efficienza da graficare (dim=neff)
c
      integer*4 nlim !numero di punti limite assegnati
c
      real*4 alim(*)!vettore dei termini noti spezzata di surge(dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari della spezzata di surge
     *              !                                             (dim=nlim+1)

     *    ,clim(*)  !costanti delle curve limite del compressore :     (dim=4)
	              !zona di choking per l'efficienza:
	              !(1) curva di choking,(2) curva operating limit
	              !zona di choking per il numero di giri:
                    !(3) curva di choking,(4) operating limit
     *    ,lim_h(*)!vettore dei valori estremi della prevalenza (dim=nlim)
     *    ,cen(*)  !coeff di eff=f(n,q) nella zona normale (dim=5)
c-elena_prec     *	,cec(*)  !coeff di eff=f(n,q) nella zona di choking (dim=6)
     *	,chn(*)  !coeff di h=f(n,q) nella zona normale (dim=6)
     *    ,chc(*)  !coeff di h=f(n,q) nella zona di choking (dim=6)
     *    ,eff_min
c---------------------------------------->Output
	real*4 qef1(max_neff,max_np)!ascisse dei punti calcolati (1°set di valori)
     *	,hef1(max_neff,max_np)!ordinate dei punti calcolati 
     *	,qef2(max_neff,max_np)!ascisse dei punti calcolati   (2°set di valori)
     *	,hef2(max_neff,max_np)!ordinate dei punti calcolati 
c                     (sono tutte matrici di np colonne)
c                     (le coordinate sono dimensionali)
c---------------------------------------------->Variabili locali
      integer*4 k
     *       ,i 
     *	   ,i1 !contano i punti di intersezione tra una curva a eff costante
     *	   ,i2 !e le varie curve a numero di giri costante
     *       ,irange
c
      real*4 eff
     *	,drev
     *	,delta1
     *	,rev
     *	,a
     *	,b
     *	,c
     *	,delta
     *	,q1
     *	,q2
     *	,h1
     *	,h2
     *	,qq
     *    ,qmin1
     *    ,qmin2
     *    ,rev_ini
     *    ,rev_da
     *    ,rev_a
     *    ,cec(6)
     *    ,qch
     *    ,c_clim(4)
c--------------------------------------------------------------------------------------------
c********************************************************************************************
c    Per ciascuna curva a efficienza costante vengono calcolate le coordinate di np punti 
c    il cui valore corrispondente di n si ottiene discretizzando l'intervallo [min_rev,max_rev] 
c    usando un passo drev
c********************************************************************************************
c
      do i=1,4
	   c_clim(i)=clim(i)
	end do

	do k=1,neff
	   eff=effic(k)
	   drev=(max_rev-min_rev)/(np-1)
         rev_ini = max_rev
	   i1=0
	   i2=0
	   delta =0.
	   delta1=0.
c----------------------------------------------------------------------------------------------
c     eff=c1+c2*(q/n)+c3*(q/n)^2+c4*n+c5*n^2;
c     si vogliono determinare i valori assunti da q per un valore fissato di n:allo scopo  
c     si scrive l'equazione nella forma a*q^2+b*q+c=0 e se ne calcolano le radici
c-----------------------------------------------------------------------------------------------
	   rev_da=0.
	   rev_a =0.
         do i=1,np
	      rev=rev_ini-(i-1)*drev
                                        !ricerca punto di intersezione con rev
	      c=cen(4)*rev-eff+cen(1)+cen(5)*rev*rev
	      b=cen(2)/rev
	      a=cen(3)/rev/rev
	      delta=b*b-4.*c*a
            if (delta.ge.0) then     !l'equazione ha due radici
              if (rev_da.eq.0) rev_da = rev
              rev_a = rev
            endif
         enddo
                           ! trova_estremi
         rev_da = rev_da + drev
         rev_a  = rev_a  - drev
         rev_da = min(rev_da,max_rev)
         rev_a = max(rev_a,min_rev)
	   drev=(rev_da-rev_a)/(np-1)
         rev_ini = rev_da
c----------------------------------->ricerca intersezioni
         do i=1,np
	      rev=rev_ini-(i-1)*drev
                                        !ricerca punto di intersezione con rev
	      c=cen(4)*rev-eff+cen(1)+cen(5)*rev*rev
	      b=cen(2)/rev
	      a=cen(3)/rev/rev
	      delta=b*b-4.*c*a
            if (delta.ge.0) then     !l'equazione ha due radici
	         q1=(-b+sqrt(delta))/2./a
	         q2=(-b-sqrt(delta))/2./a
               call hdaqn_a(h1,q1,rev,chn)
               call hdaqn_a(h2,q2,rev,chn)
c
c---> verifica che i punti di intersezione siano entro la mappa del compressore
c
c-------------------------> controllo relativo al PRIMO punto <-----------------------------          
c
C-----> trovo il punto che sulla spezzata di surge si trova all'altezza H1
c
               call qdah_su_a(alim,blim,nlim,lim_h,h1,qmin1,irange)
c-elena
	         if(q1.ge.qmin1.and.q1.gt.0.) then !(1)
			                                  !siamo a dx dell'antisurge 
				qq=q1*q1

                  if(clim(3).ne.clim(4)) then  !(2)
	                                   !ho due zone operative
	               if(qq.le.h1/clim(3)) then !(3)
					                   !punto in zona normale           
                        i1=i1+1 !conta i punti calcolati
				      qef1(k,i1) = q1*qmrif 
                        hef1(k,i1) = h1*headrif 
				   else !(3)            
c-elena ------------------>PUNTO IN ZONA DI CHOKING
C           devo ricalcolare la Q e verificare che il punto cada all'interno della mappa
c
c-elena-prec
                        call qdan_ch_a(clim(3),qch,rev,chn)
	                  c_clim(1)=qch/rev
	                  call calcola_cec(eff_min,cen,c_clim,cec)
c-elena-prec
				      q1=(eff-cec(1)-cec(2)*rev-cec(3)*rev**2)/      
     *                     (cec(4)/rev+cec(5)+cec(6)*rev)
                        call hdaqn_a(h1,q1,rev,chc)
c-elena-------------------------->controllo di accettabilità punto
                        call qdah_su_a(alim,blim,nlim,lim_h,h1,
     *                                   qmin1,irange)
                        qq=q1*q1
				      if(qq.le.h1/clim(4))then !(4)          
                           i1=i1+1 !conta i punti calcolati
				         qef1(k,i1) = q1*qmrif 
                           hef1(k,i1) = h1*headrif 
				      else  !(4)        
       	                 call fine_spezzata(k,i1,qef1,hef1)
				      end if  !(4) 
 				   end if !(3)
			    else      !(2)   
				          !non è definita la zona di choking 	   
				  if(qq.le.h1/clim(3)) then !(3)
					                   !zona normale per i giri          
                        i1=i1+1 !conta i punti calcolati
				      qef1(k,i1) = q1*qmrif 
                        hef1(k,i1) = h1*headrif 
				  else   !(3)      
       	              call fine_spezzata(k,i1,qef1,hef1)
				  end if !(3) 
                  end if  !(2)
c-elena
	         else    !(1)
c-elena-prec
                 i1=i1+1 !conta i punti calcolati
			   qef1(k,i1) = qmin1*qmrif 
                 hef1(k,i1) = h1*headrif 

       	       call fine_spezzata(k,i1,qef1,hef1)
               end if  !(1)
c
c-------------------------> controllo relativo al SECONDO punto <-----------------------------          
c
C-----> trovo il punto che sulla spezzata di surge si trova all'altezza H1
c
               call qdah_su_a(alim,blim,nlim,lim_h,h2,qmin2,irange)
c-elena
	         if(q2.ge.qmin2.and.q2.gt.0.) then !(1)
			                                   !siamo a dx dell'antisurge 
                  qq=q2*q2

                  if(clim(3).ne.clim(4)) then  !(2)
	                                   !ho due zone per l'efficienza
				     if(qq.le.h2/clim(3)) then !(3)
					                   !zona normale per i giri          
                          i2=i2+1 !conta i punti calcolati
				        qef2(k,i2) = q2*qmrif 
                          hef2(k,i2) = h2*headrif 
				     else !(3)            
c-elena ------------------> L'EFFICIENZA E' IN ZONA DI CHOKING
C           devo ricalcolare la Q e verificare che il punto cada all'interno della mappa
c-elena-prec
                        call qdan_ch_a(clim(3),qch,rev,chn)
	                  c_clim(1)=qch/rev
	                  call calcola_cec(eff_min,cen,c_clim,cec)
c-elena-prec

				       q2=(eff-cec(1)-cec(2)*rev-cec(3)*rev**2)/      
     *                     (cec(4)/rev+cec(5)+cec(6)*rev)
                         call hdaqn_a(h2,q2,rev,chc)
c-elena-------------------------->controllo di accettabilità punto
                         call qdah_su_a(alim,blim,nlim,lim_h,h2,
     *                                qmin2,irange)
                         qq=q2*q2
				       if(qq.le.h2/clim(4))then !(4)          
                            i2=i2+1 !conta i punti calcolati
				          qef2(k,i2) = q2*qmrif 
                            hef2(k,i2) = h2*headrif 
				        else  !(4)        
		                   call fine_spezzata(k,i2,qef2,hef2)
				        end if  !(4) 
				    end if !(3)
			    else      !(2)   
				          !non è definita la zona di choking per l'efficienza	   
				  if(qq.le.h2/clim(3)) then !(3)
					                   !zona normale per i giri          
                        i2=i2+1 !conta i punti calcolati
				      qef2(k,i2) = q2*qmrif 
                        hef2(k,i2) = h2*headrif 
				  else   !(3)      
		              call fine_spezzata(k,i2,qef2,hef2)
				  end if !(3) 
                  end if  !(2)
                  if (i2.eq.1 .and. delta1.lt.0) then
                    if (i1.eq.0) then
                       qef2(k,2) = qef2(k,1)
                       hef2(k,2) = hef2(k,1)
                       qef2(k,1) = qmin1*qmrif
                       hef2(k,1) = h1*headrif 
                    else
                       qef2(k,2) = qef2(k,1)
                       hef2(k,2) = hef2(k,1)
                       qef2(k,1) = qef1(k,1)
                       hef2(k,1) = hef1(k,1)
                    endif
                    i2=i2+1
                  endif
c-elena
	         else    !(1)
c-elena-prec
                 i2=i2+1 !conta i punti calcolati
			   qef2(k,i2) = qmin2*qmrif 
                 hef2(k,i2) = h2*headrif 

		       call fine_spezzata(k,i2,qef2,hef2)
               end if  !(1)
            else
               if(i.eq.1) delta1=delta       !delta<0
	      end if
         end do

c---->                CHIUSURA DELLE CURVE SE NECESSARIO
         if(delta.lt.0.) then
c---->                          chiusura in basso
c            i1=i1+1
c            qef1(k,i1) = qef2(k,i2)
c            hef1(k,i1) = hef2(k,i2)
             if (q1.lt.qmin1) then
                i2=i2+1	       
			  qef2(k,i2) = qmin1*qmrif 
                hef2(k,i2) = h1*headrif 
		   else
                i2=i2+1
                qef2(k,i2) = qef1(k,i1)
                hef2(k,i2) = hef1(k,i1)
	       end if

          end if 

!          if(delta1.lt.0.) then
c---->                          chiusura in alto
!            do i=i2,1,-1
!               qef2(k,i+1) = qef2(k,i)
!               hef2(k,i+1) = hef2(k,i)
!            end do
!            i2 = i2+1
!            qef2(k,1) = qef1(k,1)
!            hef2(k,1) = hef1(k,1)
!          end if 
	   call fine_spezzata(k,i1,qef1,hef1)
	   call fine_spezzata(k,i2,qef2,hef2)
	end do
	return
	end




