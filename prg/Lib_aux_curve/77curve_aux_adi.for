c****************************************************************************************
c                            77curve_aux_adi.for
c----------------------------------------------------------------------------------------
c  Subroutine generiche per il calcolo del punto di lavoro sulle mappe di compressori e 
c  turbine.Ciascuna di esse opera su valori adimensionali e restituisce valori dello stesso
c  tipo.Le subroutine di questo pacchetto sono utilizzate dai programmi di regressione,
c  graficazione e accoppiamento.In Simcent invece si utilizzano le stesse routine nella 
c  loro versione dimensionale duplicate nel pacchetto lib_cen_curv.for.
c  NB:i coefficienti delle curve caratteristiche sono calcolati per regressione in modo
c  da lavorare con valori adimensionali.  
c****************************************************************************************
c
c-------------------------------->COMPRESSORI<-------------------------------------------
      subroutine hdaqn_a(h,q,rev,chn)
c**********************************************************************************
c   Calcolo di H in funzione di (Q,n) assegnati.La subroutine può essere usata per 
c   il calcolo di H sia nella zona normale che nella zona di choking:nel primo caso
c   saranno passati come input i coefficienti CHN,nel secondo i coefficienti CHC
c**********************************************************************************
      implicit none
c
c------------------->INPUT
c
	real*4 q      !portata assegnata  
     *      ,rev    !numero di giri assegnato 
     *      ,chn(6) !coefficienti della curva caratteristica di un compressore
                    !(H=f(Q,n))  
c
c------------------->OUTPUT
c
      real*4  h      !head calcolato
c
c*******************************************************************************
      h = (chn(1) + chn(2)*q + chn(3)*rev + chn(4)*q*rev +
     *    chn(5)*q*q + chn(6)*rev*rev)
c
      return
      end
c
c--------------------------------------------------------------------------------
      subroutine calcola_cqn(chn,cqn)
c********************************************************************************
C    Calcola i coefficienti CQN in funzione dei CHN.Tali coefficienti permettono
c    di calcolare Q in funzione di (H,n) assegnati, nella zona normale.
c    Se il calcolo non può essere effettuato viene restituito un vettore nullo e 
c    di conseguenza il valore calcolato di Q sarà 0.
c    (elena colonna)
c********************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 chn(6) !coefficienti della curva caratteristica di un compresssore
	            !(H=f(Q,n)) nella zona normale
c
c------------------->OUTPUT
c
      real*4 cqn(6) !coefficienti per il calcolo di Q=f(H,n) nella zona normale
c
c------------------->Variabili locali
c
      real*4 denom
      integer*4 i
c
c********************************************************************************
c
      denom=chn(5)
c
      if(denom.eq.0) then
        do i=1,6
	     cqn(i)=0.
	  end do
	else
	  cqn(1)=-chn(2)/(2*denom)
        cqn(2)=-chn(4)/(2*denom)
        cqn(3)=chn(4)*chn(4)/(4*denom**2) - chn(6)/denom
        cqn(4)=chn(2)*chn(4)/(2*denom**2) - chn(3)/denom
        cqn(5)=chn(2)*chn(2)/(4*denom**2) - chn(1)/denom
        cqn(6)=1./denom
      end if
c
      return
	end
c------------------------------------------------------------------------------------
      subroutine qdahn_a(h,q,rev,chn)
c************************************************************************************
c     Calcolo di Q in funzione di (H,n) assegnati
c*************************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 h      !head assegnata 
     *      ,rev    !numero di giri assegnato
     *      ,chn(6) !coefficienti di H=f(Q,n)
c
c------------------->OUTPUT
c
      real*4 q    !portata calcolata
c
c------------------->Variabili locali
c
      real*4 cqn(6)
     *	  ,radice
c
c************************************************************************************
c
      call calcola_cqn(chn,cqn)
c
	radice = (cqn(3)*rev*rev+cqn(4)*rev+cqn(5)+cqn(6)*h) 
      if (radice.lt.0) radice=0
      q = (cqn(1)+cqn(2)*rev+sqrt(radice))
c
      return
      end
c------------------------------------------------------------------------------------
      subroutine calcola_crn(chn,crn)
c***********************************************************************************
C    Calcola i coefficienti CRN in funzione dei CHN.Tali coefficienti permettono
c    di calcolare REV in funzione di (H,Q) assegnati, nella zona normale.
c***********************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 chn(6) !coefficienti della curva caratteristica di un compresssore
	            !(H=f(Q,n)) nella zona normale
c
c------------------->OUTPUT
c
      real*4 crn(6) !coefficienti per il calcolo di rev=f(H,Q) nella zona normale
c
c------------------->Variabili locali
c
      real*4 denom
c
c********************************************************************************
c
	denom=chn(6)
      crn(1)=-chn(3)/(2*denom)
      crn(2)=-chn(4)/(2*denom)
      crn(3)=chn(4)*chn(4)/(4*denom**2) - chn(5)/denom
      crn(4)=chn(3)*chn(4)/(2*denom**2) - chn(2)/denom
      crn(5)=chn(3)*chn(3)/(4*denom**2) - chn(1)/denom
      crn(6)=1./denom
c
      return
	end
c----------------------------------------------------------------------------------
      subroutine ndahq_a(h,q,rev,chn)
c**********************************************************************************
c     Calcolo di REV in funzione di (H,Q) assegnati
c*************************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 h      !head assegnata 
     *    ,q      !portata assegnata
     *    ,chn(6) !coefficienti di H=f(Q,n)
c
c------------------->OUTPUT
c
      real*4 rev    !numero di giri calcolato
c
c------------------->Variabili locali
c
      real*4 crn(6)
     *	,radice
c
c************************************************************************************
c
      call calcola_crn(chn,crn)
c
      radice = (crn(3)*q*q+crn(4)*q+crn(5)+crn(6)*h)
      if (radice.lt.0) radice=0
      if (crn(6).gt.0) then
        rev = crn(1)+crn(2)*q+sqrt(radice)
      else
        rev = crn(1)+crn(2)*q-sqrt(radice)
      endif
c
      return
      end
c-------------------------------------------------------------------------------------
c-----------------------------> zona di choking <-------------------------------------
c---------------------------------------------------------------------------------------
      subroutine qdah_ch_a(k,h,q)
c**************************************************************************************
c    Calcolo di Q in funzione di H, sulla curva di choking, la cui costante
c    e' k (H=k*Q^2)
c**************************************************************************************
      implicit none
c
c-------------------->INPUT
c
      real*4 k      ! costante della curva di choking (clim(3))
     *    ,h      ! head assegnata
c
c-------------------->OUTPUT
c
      real*4 q      ! portata calcolata
c 
c***************************************************************************************
c
      q = sqrt(h/k)
c
      return
      end
c---------------------------------------------------------------------------------------
      subroutine hdaq_ch_a(k,h,q)
c**************************************************************************************
c    Calcolo di H in funzione di Q, sulla curva di choking, la cui costante
c    e' k (H=k*Q^2)
c**************************************************************************************
      implicit none
c
c-------------------->INPUT
c
      real*4 k      ! costante della curva di choking (clim(3))
     *    ,q      ! portata assegnata
c
c-------------------->OUTPUT
c
      real*4 h      ! head calcolata
c 
c***************************************************************************************
c
      h = (k*q*q)
c
      return
      end
c-------------------------------------------------------------------------------------
      subroutine calcola_crc(k,chn,crc)
c***********************************************************************************
C    Calcola i coefficienti CRC in funzione dei CHN.Tali coefficienti permettono
c    di calcolare REV in funzione di (H,Q) assegnati, nella zona di choking.
c    Se il calcolo non può essere effettuato viene restituito un vettore nullo.
c***********************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 k      !costante della curva di choking (h=k*q^2) 
     *	,chn(6) !coefficienti della curva caratteristica di un compresssore
	            !(H=f(Q,n)) nella zona normale
c
c------------------->OUTPUT
c
      real*4 crc(5) !coefficienti per il calcolo di rev=f(H,Q) nella zona di 
	            !choking
c
c------------------->Variabili locali
c
      real*4 denom
     *	,cc5
c
      integer*4 i
c
c********************************************************************************
c
      denom=chn(6)
      cc5=chn(5)-k
c
      if (denom.eq.0) then
	   do i=1,5
	      crc(i)=0.
	   end do
      else
         crc(1)=-chn(3)/(2*denom)
         crc(2)=-chn(4)/(2*denom)
         crc(3)=chn(4)*chn(4)/(4*denom**2) - cc5/denom
         crc(4)=chn(3)*chn(4)/(2*denom**2) - chn(2)/denom
         crc(5)=chn(3)*chn(3)/(4*denom**2) - chn(1)/denom
	end if
c
      return
	end
c----------------------------------------------------------------------------------
      subroutine nqdah_ch_a(k,h,q,rev,chn)
c**********************************************************************************
c   Calcolo di Q,REV in funzione di H sulla curva di choking, la cui costante
c   e' k (H=k*Q^2)
c**********************************************************************************
      implicit none
c
c--------------------->INPUT
c
	real*4 k      !costante  curva di choking(clim3) o operating limit(clim4)
     *    ,h      !head assegnata
     *    ,chn(6) ! coefficienti curva caratteristica H=f(Q,n)
c
c--------------------->OUTPUT
c
      real*4 q      !portata calcolata 
     *    ,rev    !numero di giri calcolato
c
c--------------------->Variabili locali
c
      real*4 crc(5)
     *    ,radice 
c
c*************************************************************************************
c 
      q = sqrt(h/k)
c
      call calcola_crc(k,chn,crc)
c      
      radice = (crc(3)*q*q+crc(4)*q+crc(5)) 
      if (radice.lt.0) radice=0
      if (chn(6).gt.0) then
        rev = (crc(1)+crc(2)*q+sqrt(radice))
      else
        rev = (crc(1)+crc(2)*q-sqrt(radice))
      endif
c
      return
      end
c--------------------------------------------------------------------------------------
      subroutine ndaq_ch_a(k,q,rev,chn)
c**********************************************************************************
c   Calcolo di REV in funzione di Q sulla curva di choking, la cui costante
c   e' k (H=k*Q^2)
c**********************************************************************************
      implicit none
c
c--------------------->INPUT
c
	real*4 k      !costante della curva di choking (clim(3))
     *    ,q      !portata assegnata
     *    ,chn(6) ! coefficienti curva caratteristica H=f(Q,n)
c
c--------------------->OUTPUT
c
      real*4 rev    !numero di giri calcolato
c
c--------------------->Variabili locali
c
      real*4 crc(5)
     *    ,radice 
c
c*************************************************************************************
c
      call calcola_crc(k,chn,crc)
c      
      radice = (crc(3)*q*q+crc(4)*q+crc(5)) 
      if (radice.lt.0) radice=0
      if (chn(6).gt.0) then
        rev = (crc(1)+crc(2)*q+sqrt(radice))
      else
        rev = (crc(1)+crc(2)*q-sqrt(radice))
      endif
c
      return
      end
c---------------------------------------------------------------------------------------
      subroutine calcola_cqc(k,chn,cqc)
c********************************************************************************
C    Calcola i coefficienti CQC in funzione dei CHN.Tali coefficienti permettono
c    di calcolare Q in funzione di (H,n) assegnati, nella zona di choking.
c    Se il calcolo non può essere effettuato viene restituito un vettore nullo.
c********************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 k      !costante della curva di choking (clim(3))
     *	,chn(6) !coefficienti della curva caratteristica di un compresssore
	            !(H=f(Q,n)) nella zona normale
c
c------------------->OUTPUT
c
      real*4 cqc(5) !coefficienti per il calcolo di Q=f(H,n) nella zona di choking
c
c------------------->Variabili locali
c
      real*4 denom
	integer*4 i
c**********************************************************************************
      denom=chn(5)-k
c
      if (denom.eq.0) then
         do i=1,5
	      cqc(i)=0.
	   end do        
      else  
         cqc(1)=-chn(2)/(2*denom)
         cqc(2)=-chn(4)/(2*denom)
         cqc(3)=chn(4)*chn(4)/(4*denom**2) - chn(6)/denom
         cqc(4)=chn(2)*chn(4)/(2*denom**2) - chn(3)/denom
         cqc(5)=chn(2)*chn(2)/(4*denom**2) - chn(1)/denom
      end if
c 
      return
	end
c------------------------------------------------------------------------------------
      subroutine qdan_ch_a(k,q,rev,chn)
c*********************************************************************************
c   Calcolo di Q in funzione di REV sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c*********************************************************************************
      implicit none
c
c--------------------->INPUT
c 
      real*4  k      !costante della curva di choking (clim(3))
     *     ,rev    !numero di giri assegnato
     *     ,chn(6) !coefficienti curva caratteristica di un compressore
c
c--------------------->OUTPUT
c
      real*4  q      !portata calcolata
c 
c--------------------->Variabili locali
c 
      real*4 radice
     *    ,cqc(5)
c
c***********************************************************************************
c
      call calcola_cqc(k,chn,cqc)
c 
      radice = (cqc(3)*rev*rev+cqc(4)*rev+cqc(5)) 
      if (radice.lt.0) radice=0
      q = (cqc(1)+cqc(2)*rev+sqrt(radice))
c
      return
      end
c------------------------------------------------------------------------------------
      subroutine hdan_ch_a(k,h,rev,chn)
c************************************************************************************
c   Calcolo di H in funzione di REV sulla curva di choking, la cui costante
c   e' k (H=K*Q**2)
c************************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 k      !costante della curva di choking (clim(3))
     *    ,rev    !numero di giri assegnato
     *    ,chn(6) !coefficienti curva caratteristica
c
c------------------->OUTPUT
c
      real*4 h      !head calcolata
c
c------------------->Variabili locali
c
      real*4 q
c
c***********************************************************************************
c
      call qdan_ch_a(k,q,rev,chn)
c
      h = (k * q * q)
c
      return
      end
c--------------------------------------------------------------------------------------
c--------------------------->curva di anti-surge<--------------------------------------
c--------------------------------------------------------------------------------------
      subroutine rsurge_a(nlim,rval,lim,irange)
C*****************************************************************************************
c Determina l'intervallo della spezzata di anti-surge a cui appartiene il punto di lavoro
c avente una delle tre grandezze caratteristiche uguale a RVAL
c*****************************************************************************************
      implicit none
c
c-------------------->INPUT
c
	integer*4 nlim     !numero di punti limite assegnati
c
	real*4 rval      !grandezza nota del punto di cui si vuole individuare il 
	                 !range di appartenenza;può essere la prevalenza(H),la portata
	                 !(Q) o il numero di giri(REV)
c
      real*4 lim(nlim)   !vettore degli estremi dei range
c
c-------------------->OUTPUT
c
      integer*4 irange   !indice dell'intervallo a cui appartiene RVAL
      
c-------------------->variabili locali
	integer*4 i
C
c*****************************************************************************************
c
      irange=0
      do i=1,nlim
         if (rval.lt.lim(i) ) then
           irange=i
           goto 10
         endif
      enddo
10    continue
      if (irange.eq.0) then
c    RVAL e' sopra il massimo definito dall'utente per la surge limit
         irange=nlim+1
      endif               
      return
      end
c-----------------------------------------------------------------------------------------
      subroutine hdaq_su_a(nlim,alim,blim,q,lim_q,h,irange)
c*****************************************************************************************
c     calcolo di H in funzione di Q sulla curva di anti-surge con ricerca del range ed
c     applicazione della relativa equazione della retta H=alim+q*blim
c     elena colonna
c*****************************************************************************************
      implicit none
c
c----------------------->INPUT
      integer*4 nlim  !numero di punti limite assegnati
c
      real*4 alim(*)  !vettore dei termini noti a (dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari b (dim=nlim+1)
     *    ,q        !portata assegnata
     *    ,lim_q(*) !vettore dei valori estremi di Q (dim=nlim)
c
c----------------------->OUTPUT
c      
      real*4 h          !prevalenza calcolata
	integer*4 irange  !indice dell'intervallo trovato
c
c*****************************************************************************************
c     
      call rsurge_a(nlim,q,lim_q,irange)
      h=(q*blim(irange)+alim(irange))
c
      return
	end
c-----------------------------------------------------------------------------------------
      subroutine qdan_su_a(chn,alim,blim,nlim,lim_rev,rev,q,irange)
c*****************************************************************************************
c   Calcolo di Q in funzione di REV sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = alim + Q * blim
c****************************************************************************************
      implicit none
c
c------------------------->INPUT
c
      integer*4 nlim       !numero di punti limite assegnati
c
	real*4   alim(*)    !vettore dei termini noti a (dim=nlim+1)
     *      ,blim(*)    !vettore dei coefficienti angolari b (dim=nlim+1)
     *      ,rev        !numero di giri assegnato
     *      ,lim_rev(*) !vettore dei valori estremi di rev (dim=nlim)
     *      ,chn(*)     !coefficienti della curva caratteristica (h=f(Q,n))
c
c------------------------->OUTPUT
c	
      real*4       q   !portata calcolata 
c
	integer*4    irange     !indice dell'intervallo trovato 
c
c------------------------->Variabili locali
      real*4       denom,cc1,cc2
      real*4       cqs(5)
      real*4       radice
c*****************************************************************************************
      call rsurge_a(nlim,rev,lim_rev,irange)
c
      denom=chn(5)
      cc1 = chn(1) - alim(irange)
      cc2 = chn(2) - blim(irange)
c
      if (denom.eq.0) then
	   q = 0.      
         return
      endif
c
      cqs(1)=-cc2/(2*denom)
      cqs(2)=-chn(4)/(2*denom)
      cqs(3)=chn(4)*chn(4)/(4*denom**2) - chn(6)/denom
      cqs(4)=cc2*chn(4)/(2*denom**2) - chn(3)/denom
      cqs(5)=cc2*cc2/(4*denom**2) - cc1/denom
c
      radice = (cqs(3)*rev*rev+cqs(4)*rev+cqs(5)) 
      if (radice.lt.0) radice=0
c
      q = (cqs(1)+cqs(2)*rev+sqrt(radice))
c
      return
      end

c--------------------------------------------------------------------------------------------
      subroutine hqdan_su_a(chn,alim,blim,nlim,lim_rev,rev,h,q,irange)
c*****************************************************************************************
c     calcolo di H in funzione di Q sulla curva di anti-surge con ricerca del range ed
c     applicazione della relativa equazione della retta H=alim+q*blim
c*****************************************************************************************
      implicit none
c
c----------------------->INPUT
      integer*4 nlim  !numero di segmenti nella spezzata di anti-surge
c
      real*4 alim(*)  !vettore dei termini noti a (dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari b (dim=nlim+1)
     *    ,rev      !numero di giri assegnato
     *    ,lim_rev(*) !vettore dei valori estremi di REV (dim=nlim+1)
     *    ,chn(6)     !coefficienti della curva caratteristica H=f(Q,n)
c
c----------------------->OUTPUT
c      
      real*4 h          !prevalenza calcolata
	integer*4 irange  !indice dell'intervallo trovato
      real*4 q          !portata calcolata
c
c*****************************************************************************************
c     
      call qdan_su_a(chn,alim,blim,nlim,lim_rev,rev,q,irange)

      h=(q*blim(irange)+alim(irange))
c
      return
	end
c-----------------------------------------------------------------------------------------
      subroutine qdah_su_a(alim,blim,nlim,lim_h,h,q,irange)
c*****************************************************************************************
c   Calcolo di Q in funzione di (H) sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H=alim+q*blim
c*****************************************************************************************
      implicit none
c
c-------------------->INPUT
c
      integer*4 nlim  !numero di punti limite assegnati
c
      real*4 alim(*)  !vettore dei termini noti a (dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari b (dim=nlim+1)
     *    ,h        !prevalenza assegnata
     *    ,lim_h(*) !vettore dei valori estremi di H (dim=nlim+1)
c
c----------------------->OUTPUT
c      
      real*4 q          !portata calcolata
	integer*4 irange  !indice dell'intervallo trovato
c****************************************************************************************
c
      call rsurge_a(nlim,h,lim_h,irange)
      q = (h-alim(irange))/blim(irange)               
c
      return
      end
c----------------------------------------------------------------------------------------
cgpe      subroutine ndah_su_a(chn,alim,blim,nlim,lim_h,rev,h)
cgpe!!!!! aggiunto in output il parametro q
      subroutine ndah_su_a(chn,alim,blim,nlim,lim_h,rev,h,q)
c************************************************************************************
c   Calcolo di REV in funzione di H sulla curva di anti-surge, 
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = alim + Q * blim
c************************************************************************************
      implicit none
c
c---------------------->INPUT
c
      integer*4 nlim  !numero di segmenti nella spezzata di anti-surge
c
      real*4 alim(*)  !vettore dei termini noti a (dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari b (dim=nlim+1)
     *    ,h        !prevalenza assegnata
     *    ,lim_h(*) !vettore dei valori estremi di H (dim=nlim+1)
     *    ,chn(6)   !coefficienti della curva caratteristica H=f(Q,n)
c
c----------------------->OUTPUT
c      
      real*4 rev        !numero di giri calcolato
      real*4 q
c
c----------------------->Variabili locali
c
	integer*4 irange  !indice dell'intervallo trovato
c
c****************************************************************************************
c
      call rsurge_a(nlim,h,lim_h,irange)
      q =  (h - alim(irange)) / blim(irange)
c
      call ndahq_a(h,q,rev,chn)
c
      return
      end
c---------------------------------------------------------------------------------------
      subroutine ndaq_su_a(alim,blim,nlim,lim_q,q,chn,rev,irange)
c***************************************************************************************
c   Calcolo di N in funzione di Q sulla curva di anti-surge,
c   con ricerca del range, ed applicazione della relativa equazione
c   della retta H = qblim + alim
c***************************************************************************************
      implicit none
c
c---------------------->INPUT
c
      integer*4 nlim  !numero di segmenti nella spezzata di anti-surge
c
      real*4 alim(*)  !vettore dei termini noti a (dim=nlim+1)
     *    ,blim(*)  !vettore dei coefficienti angolari b (dim=nlim+1)
     *    ,q        !portata assegnata
     *    ,lim_q(*) !vettore dei valori estremi di Q (dim=nlim+1)
     *    ,chn(6)   !coefficienti della curva caratteristica H=f(Q,n)
c
c----------------------->OUTPUT
c      
      real*4 rev        !numero di giri calcolato
	integer*4 irange  !indice dell'intervallo trovato
c---------------------->Variabili locali
      real*4 h
c
c****************************************************************************************
c     
      call rsurge_a(nlim,q,lim_q,irange)
      h = (q * blim(irange) + alim(irange))
      call ndahq_a(h,q,rev,chn)
c
      return
      end
c-----------------------------------------------------------------------------------------
c--------------------------------------giri----------------------------------------
      subroutine calcola_giri(max_rev,min_rev,ngiri,giri)
c******************************************************************************************
c      calcola il numero di curve a rev costante e i valori da graficare
c******************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c--------------------------------->Argomenti della subroutine
c------------------------>INPUT
      real*4 max_rev   !numero di giri massimo
     *    ,min_rev   !numero di giri minimo
c
c------------------------>OUTPUT
      integer*4 ngiri  !numero di curve a rev costante da graficare
c
      real*4 giri(*)   !valori costanti del numero di giri da graficare
	               !                                     (dim=ngiri)
c------------------------>Variabili locali
      integer*4 j,ng
c*******************************************************************************************
      giri(1)=max_rev
	if (max_rev.gt.1.) then
	   giri(2)=1.
	   ngiri=2
	else
	   ngiri=1
	end if
	ng=ngiri+1
      do j=ng,max_ngiri
	   giri(j)=giri(j-1)-0.1
	   if (giri(j).gt.min_rev) then
	      ngiri=ngiri+1
	   else
	      giri(j)=min_rev
	      ngiri=ngiri+1
	      exit
	   end if 
	end do
	return
	end
c----------------------------------------------------------------------------------------------
c--------------------------------------efficienza----------------------------------------
      subroutine calcola_cec(eff_min,cen,clim,cec)
c*****************************************************************************************
c     calcola i sei coefficienti CEC della curva a efficienza costante nella zona
c     di choking del compressore
c     elena colonna
c*****************************************************************************************
      implicit none
c
c------------------->INPUT
c
      real*4 eff_min    !minimo valore dell'efficienza
c
      real*4 cen(5)     !coefficienti della curva a efficienza costante nella
     *                !zona normale
     *    ,clim(*)    !costanti delle curve limite del compressore:
	                !(1) anti-choke efficienza
	                !(2) operating limit efficienza
	                !(3) anti-choke giri
	                !(4) operating limit giri
c
c------------------->OUTPUT
c
      real*4 cec(6)     !coefficienti della curva a efficienza costante nella
	                !zona di choking
c
c------------------->Variabili locali
c
      real*4 aa,bb,cc,denom,faux,i
c
c*****************************************************************************************
c
      if (clim(1).gt.0) then
	   if (clim(2).eq.0) return
	   aa=1./clim(1)
	   bb=1./clim(2)
	   cc=aa*bb
	   denom=aa-bb
	   if (denom.eq.0) return
	   faux=cen(1)+(cen(2)/aa)+((cen(3)/aa)/aa)
	   cec(1)=(aa*faux-eff_min*bb)/denom
         cec(2)=(cen(4)*aa)/denom
	   cec(3)=(cen(5)*aa)/denom
	   cec(4)=((eff_min-faux)*cc)/denom
	   cec(5)=-(cen(4)*cc)/denom
	   cec(6)=-(cen(5)*cc)/denom
      else
	   do i=1,6
	      cec(i)=0.
	   end do
      end if

	return
	end
c----------------------------------------------------------------------------------------------
      subroutine effdaqn_a(eff,q,rev,cen,cec,clim)
c**********************************************************************************************
c   Calcolo di eff in funzione di (rev,q) assegnati.
c**********************************************************************************************
      implicit none
c---------------->INPUT
      real*4 q       ! portata assegnata
     *    ,rev     ! numero di giri assegnato
     *    ,cen(5)  ! coefficienti curva 
     *    ,cec(6)  ! coefficienti curva choking zone
     *    ,clim(*) ! coefficienti costanti curve limite
c
c--------------->OUTPUT
C
      real*4 eff     ! efficienza calcolata (valore da 0 a 1)
c
C--------------->Variabili locali
c
      real*4       qr
c
c***********************************************************************************************
c
      qr = q/rev
      if (clim(1).gt.0.and.qr.gt.clim(1)) then
c choking zone
        eff = cec(1)+cec(2)*rev+cec(3)*rev*rev+cec(4)*qr+
     *       cec(5)*q+cec(6)*rev*q
        if (eff.lt.0.01) eff=0.01
      else
        eff = cen(1)+ qr*(cen(2)+cen(3)*qr)+(cen(4)+cen(5)*rev)*rev
        if (eff.lt.0.01) eff=0.01
      endif
c
      return
      end
c------------------------------------------------------------------------------------------
c-------------------------------------------------------------------------------------------
c----------------------------------->TURBINE<-----------------------------------------------
      subroutine pwdan_a(pow,rev,temp,cpwm,cpwt)
c***********************************************************************************
c   Calcolo di PWMAX in funzione di (rev,temp) assegnati.
c***********************************************************************************
      implicit none
c--------->INPUT
      real*4 rev     ! numero di giri assegnato
     *    ,temp    ! temperatura dell'aria assegnata
     *    ,cpwm(3) ! coefficienti curva caratteristica pwm=f(rev)
     *    ,cpwt(4) ! coefficienti curva correzione temperatura fc=f(temp)
c--------->OUTPUT      
	real*4 pow     ! pwmax calcolato (in Kwatt)

c--------->Variabili locali
      real*4 fc
c*************************************************************************************
      fc  = cpwt(1)+temp*(cpwt(2)+temp*(cpwt(3)+cpwt(4)*temp))
      pow = ((cpwm(1)+ rev*(cpwm(2)+cpwm(3)*rev)) * fc)
c
      return
      end
c-----------------------------------------------------------------------------------
      subroutine hrdapn_a(hr,pow,rev,temp,chr,chrt)
c**********************************************************************************
c   Calcolo di HR in funzione di (rev,temp) assegnati.
c**********************************************************************************
      implicit none
c-------->INPUT
      real*4 Pow     ! pwmax assegnato (in Kwatt)
     *    ,rev     ! numero di giri assegnato
     *    ,temp    ! temperatura dell'aria assegnata
cgpe     *    ,chr(6)  ! coefficienti curva caratteristica hr=f(rev,pow)
cgpe     *    ,chrt(4) ! coefficienti curva correzione temp
     *    ,chr(*)  ! coefficienti curva caratteristica hr=f(rev,pow)
     *    ,chrt(*) ! coefficienti curva correzione temp

c-------->OUTPUT      
	real*4 hr      ! hrate calcolato (in KJ/Kwatth)
c-------->Variabili locali
      real*4 fc
c**********************************************************************************
c
      fc  = chrt(1)+temp*(chrt(2)+temp*(chrt(3)+chrt(4)*temp))
      hr  = (chr(1)+rev*(chr(2)+pow*chr(4)+rev*chr(5))+
     *                 pow*(chr(3)+pow*chr(6)))*fc
c
      return
      end
c---------------------------------------------------------------------------------------
      subroutine pwdan_ec_a(pow,rev,cpwm)
c***********************************************************************************
c   Calcolo di PWMAX in funzione di (rev,temp) assegnati alla temperatura in cui il 
c   fattore di correzione fc vale 1.
c***********************************************************************************
      implicit none
c--------->INPUT
      real*4 rev     ! numero di giri assegnato
     *    ,cpwm(3) ! coefficienti curva caratteristica pwm=f(rev)
c--------->OUTPUT      
	real*4 pow     ! pwmax calcolato (in Kwatt)

c*************************************************************************************
      pow = cpwm(1)+ rev*(cpwm(2)+cpwm(3)*rev) 
c
      return
      end
c-----------------------------------------------------------------------------------
      subroutine hrdapn_ec_a(hr,pow,rev,chr)
c**********************************************************************************
c   Calcolo di HR in funzione di (rev,temp) assegnati alla temperatura in cui il 
c   fattore di correzione fc vale 1. .
c**********************************************************************************
      implicit none
c-------->INPUT
      real*4 Pow     ! pwmax assegnato (in Kwatt)
     *    ,rev     ! numero di giri assegnato
     *    ,chr(6)  ! coefficienti curva caratteristica hr=f(rev,pow)
c-------->OUTPUT      
	real*4 hr      ! hrate calcolato (in KJ/Kwatth)
**********************************************************************************
c
      hr  = chr(1)+rev*(chr(2)+pow*chr(4)+rev*chr(5))+
     *                 pow*(chr(3)+pow*chr(6))
c
      return
      end
c-----------------------------------------------------------------------------------
      subroutine trans_t_a(powt,t,cpwt,pow)
c***********************************************************************************
c  Converte un punto di lavoro assegnato sulla mappa di una turbina alla temperatura
c  a cui viene effettuata la graficazione delle curve caratteristiche(temperatura in 
c  corrispondenza della quale i fattori correttivi per potenza ed heat-rate valgono 1)
c  (i valori di input devono essere adimensionali)
c***********************************************************************************
      implicit none
c--------->INPUT
      real*4 powt,   !potenza riferita alla temperatura t
     *       t,      !temperatura a cui è assegnato il pto di lavoro
     *       cpwt(4),! coefficienti curva correzione temperatura fc=f(temp)

c--------->OUTPUT
     *       pow,    !potenza riferita alla temperatura di graficazione
c--------->Variabili locali
     *       fc

c*************************************************************************************
c
      fc  = cpwt(1)+t*(cpwt(2)+t*(cpwt(3)+cpwt(4)*t))
      pow = powt/fc
c
      return
	end

c-----------------------------------------------------------------
      subroutine limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,
     *                  	lim_q,lim_h,qlim,hlim)
c*****************************************************************************************
c determina i punti limite in coordinate H Q (adimensionali) delle curve 
c caratteristiche di un compressore,ossia:
c  (1)punto di intersezione tra la spezzata anti-surge e la curva a numero max di giri;
c  (2)punto di intersezione tra la curva anti-choke e la curva a numero max di giri;
c  (3)punto di intersezione tra la spezzata anti-surge e la curva a numero min di giri;
c  (4)punto di intersezione tra la curva anti-choke e la curva a numero min di giri;
c  (5)punto di intersezione tra la curva operating-limit e la curva a numero max di giri;
c  (6)punto di intersezione tra la curva operating-limit e la curva a numero min di giri;
c*****************************************************************************************
c  anti-choke:limite di choking relativo (non è un vero e proprio limite operativo,ma 
c      viene definito per assicurarsi che il punto di funzionamento abbia un rendimento
c      accettabile)
c  operating limit:limite operativo o di choking assoluto
c*****************************************************************************************
c  Gli estremi della spezzata anti-surge sono fissati e devono coincidere con i punti al
c  minimo e al massimo numero di giri.Tale regola va ricordata quando si assegnano i punti
c  limite:1°punto--->min_rev,ultimo punto(indice nlim)--->max_rev
c*****************************************************************************************

      implicit none
c
c--------------------------->Argomenti della subroutine	
c-------------->   INPUT
      real*4  min_rev  !numero di giri ("rev = revolution rate") minimo
     * 	 ,max_rev  !numero di giri massimo
     *     ,chn(*)   !coefficienti di h=f(n,q) nella zona normale       (dim=6)
     *	 ,chc(*)   !coefficienti di h=f(n,q) nella zona di choking    (dim=6)
     *	 ,clim(*)  !costanti delle curve limite del compressore :     (dim=4)
	               !zona di choking per l'efficienza:
	               !(1) curva di choking,(2) curva operating limit
	               !zona di choking per il numero di giri:
	               !(3) curva di choking,(4) operating limit
c
      integer*4 nlim   !numero di punti limite assegnati sulla spezzata anti-surge
c
      real*4 lim_q(*)  !valori limite di q sulla curva anti-sourge:
	               !valore di q relativo a min_rev(1) e a max_rev(nlim)(dim=nlim)
     *    ,lim_h(*)  !vettore dei punti limite sulla curva anti-sourge:
	               !valore di h relativo a min_rev(1) e a max_rev(nlim)(dim=nlim)
c------------------------------------------------------------------------------------------ 
c-------------->   OUTPUT
      real*4  qlim(*) !vettore dei valori limite della portata(ascissa) (dim=6)
     *	 ,hlim(*) !vettore dei valori limite dell'altezza adiabatica(ordinata) 
     *              !                                                 (dim=6)
c
c****************************************************************************************
c
C---> Punto di intersezione tra la curva anti-sourge e la curva a numero
c                                                      massimo di giri(1)
c
      qlim(1) = lim_q(nlim)
      hlim(1) = lim_h(nlim)
c
C---> Punto di intersezione tra la curva anti-sourge e la curva a numero
c                                                       minimo di giri(3)
c   
      qlim(3) = lim_q(1)
      hlim(3) = lim_h(1)


c
C---> Punto di intersezione tra la curva anti-choke e la curva a numero
c                                                     massimo di giri(2)
C 
      call qdan_ch_a(clim(3),qlim(2),max_rev,chn)
      call hdaq_ch_a(clim(3),hlim(2),qlim(2))    
c
C---> Punto di intersezione tra la curva anti-choke e la curva a numero 
c                                                      minimo di giri(4)
      call qdan_ch_a(clim(3),qlim(4),min_rev,chn)
      call hdaq_ch_a(clim(3),hlim(4),qlim(4))    
c
c                                  
      if (clim(3).ne.clim(4)) then !la curva operating-limit e anti-choke
	                             !non coincidono:è definita la zona di choking                   
      
c---> Punto di intersezione tra la curva operting limit e la curva a
c                                            numero massimo di giri(5)      
         call qdan_ch_a(clim(4),qlim(5),max_rev,chc)
         call hdaq_ch_a(clim(4),hlim(5),qlim(5))
c    
c---> Punto di intersezione tra la curva operating limit e la curva a 
c                                             numero minimo di giri(6)
         call qdan_ch_a(clim(4),qlim(6),min_rev,chc)
         call hdaq_ch_a(clim(4),hlim(6),qlim(6))
      else     !le curve operating-limit e anti-choke coincidono
        qlim(5) = qlim(2)
        hlim(5) = hlim(2)
        qlim(6) = qlim(4)
        hlim(6) = hlim(4)
      endif        
      return
      end
c**********************************************************************************************
c                    STUDIO SU ACCOPPIAMENTO COMPRESSORE-TURBINA
c
c     subroutine per riportare i punti della curva maxpower calata sul piano del compressore
c     in punti sul piano della turbina;si tratta di trasformare punti assegnati in termini di
c     (portata,prevalenz) in punti dati in termini di (numero giri,potenza).
c     La temperatura dell'aria si ricava dalle condizioni operative in corrispondenza delle 
c     quali viene studiato l'accoppiamento
c**********************************************************************************************
      subroutine  revpow_da_hq(np,q,h,ch,cpw,altitude,rev,pow)
c----------------------------------------------------------------------------------------------
c   Riceve punti in termini di (Q,H) e restituisce punti in termini di (rev,pow)
c   I valori di Q sono espressi in 1000*m3/h
c   I valori di H sono espressi in km
c   La temperatura è data in KELVIN 
c   I valori di rev e pow devono essere % riferite alle condizioni ISO (di graficazione)
c----------------------------------------------------------------------------------------------

	implicit none
      include '../inc/rk_param.inc'

c------->parametri della subroutine      
	integer*4 np     !numero punti curva di accoppiamento

	real*4 q(*)      !vettore delle portate
     *      ,h(*)      !vettore delle prevalenze

	real*4 ch(*)     !coeff curve caratteristiche compressore
     *      ,cpw(*)    !coeff maxpower
     *      ,altitude  !altezza della centrale in cui si studia l'accoppiamento
         
      real*4 rev(*)    !vettore del numero di giri
     *      ,pow(*)    !vettore delle potenze
c------->parametri locali
      integer*4 i
	real*4  delta,pow_zero,pow_t,fc
c----------------------------------------------------------------------------------------------
c  ricevo le Q in km3/h e le H in km
c------->adimensionalizzazione
      do i=1,np
         q(i)=q(i)/qmrif
	   h(i)=h(i)/headrif    
	end do
c-------->calcolo la correzione per l'altezza

      delta= 1.-1.165E-4*ALTITUDE + 4.67E-9*ALTITUDE**2

      do i=1,np
         call ndahq_a(h(i),q(i),rev(i),ch)
c------->rev è una %n
         call pwdan_ec_a(pow_zero,rev(i),cpw)
c------->la potenza è riferita alle condizioni di graficazione:
c        temperatura     15°C
c        altitudine      livello del mare
c------->   correzione dovuta all'altezza   
         pow(i)=pow_zero/delta
c------->dimensionalizzazione
	   rev(i)=rev(i)*100.
	   pow(i)=pow(i)*100.
	end do

	return
	end  !sub revpow_da_hq

*************************************************************************************************
c                                   UTILITA'
*************************************************************************************************
      Subroutine range_temp(ncorr,tcorr,Tmin,Tmax)
c------------------------------------------------------------------------------------------------
c  Trova il minimo e il massimo valore della temperatura per definire la scala sull'asse delle
c  ascisse del piano in cui disegno l'andamento dei fattori correttivi
c------------------------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'

c-------------->INPUT
      integer*4 ncorr   !cardinalità dell'insieme di valori
     	real*4 Tcorr(*)  !insieme delle temperature assegnate (kelvin)
c-------------->OUTPUT
      real*4 Tmin,    !temperatura minima (°C) 
     *       Tmax     !temperatura massima (°C)
c-------------->
      integer*4 i
      real*4 min_app,max_app
c-----------------------------------------------------------------------------------------------

      Tmin=tcorr(1)
	Tmax=tcorr(1)
      do i=1,ncorr
         min_app=tcorr(i)
	   max_app=tcorr(i)
	   if(min_app.lt.Tmin) Tmin=min_app
         if(max_app.gt.Tmax) Tmax=max_app
 	end do

c  Trasformo TMIN e TMAX in gradi
	tmin=tmin-t0
	tmax=tmax-t0
c  I limiti per la temperatura nel vecchio Sire erano scolpiti a -20 e 40  
c  Per garantire che almeno questa situazione sia rispettata mi comporto come segue:  
c  Se tmin>-20 pongo tmin=20, se tmax<40 pongo tmax=40.In ciascuno dei due casi impongo 
c  un range di 5°C sia a destra che a sinistra
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

	return
	end   !subroutine

c*******************************************************************************************
c  Elena C.  : 12 gennaio 2006
c  Subroutine per il trattamento dei punti limite di un compressore
c*******************************************************************************************

      subroutine filtra_plim(nlim,lim_n,lim_q,lim_h,alim,blim)
c-------------------------------------------------------------------------------------------
c   Filtra i vettori contenenti le coordinate dei punti limite di un compressore eliminando
c   i valori dei punti non utilizzati nel calcolo della spezzata di surge in quanto al di 
c   fuori dell'intervallo [giri_min,giri_max]. Tali punti si riconoscono dal fatto che 
c   l'altezza adiabatica, calcolata dal programma di regressione, è nulla.
c   Viene inoltre aggiornato nlim (=numero di punti limite)  
c-------------------------------------------------------------------------------------------
      implicit none
      include '../inc/rk_param.inc'

      integer*4 nlim
	real*4 lim_n(*),lim_q(*),lim_h(*),alim(*),blim(*)

      integer*4 i,k,nlim_app
	real*4 lim_n_app(max_nlim),lim_q_app(max_nlim),lim_h_app(max_nlim),
     *       alim_app(max_nlim+1),blim_app(max_nlim+1)
c------------------------------------------------------------------------------------------
! memorizzo in vettori di appoggio i dati relativi alla spezzata di surge passati in input

	do i=1,nlim
	   lim_n_app(i)=lim_n(i)
	   lim_q_app(i)=lim_q(i)
	   lim_h_app(i)=lim_h(i)
	   alim_app(i)=alim(i)
	   blim_app(i)=blim(i)
	end do

! pulisco i vettori e inizializzo il loro valore a zero

	do i=1,nlim
	   lim_n(i)=0
	   lim_q(i)=0
	   lim_h(i)=0
	   alim(i)=0
	   blim(i)=0
	end do
c-17gennaio
c	alim(nlim+1)=0
c	blim(nlim+1)=0
c-17gennaio
	nlim_app=nlim

! riempio i vettori con i valori limite contenuti nel range [min_rev,max_rev]

      k=0
	do i=1,nlim_app
	  if (lim_h_app(i).ne.0) then
	   k=k+1
	   lim_n(k)=lim_n_app(i)
	   lim_q(k)=lim_q_app(i)
	   lim_h(k)=lim_h_app(i)
	   alim(k)=alim_app(i)
	   blim(k)=blim_app(i)
	  end if
	end do
c-17gennaio
c	alim(k+1)=alim(k)
c	blim(k+1)=blim(k)
	alim(k+1)=0
	blim(k+1)=0
c-17gennaio

! aggiorno il valore di nlim
      nlim=k

	return
	end
c****************************************************************************************





