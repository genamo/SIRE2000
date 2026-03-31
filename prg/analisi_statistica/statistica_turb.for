      subroutine stat_t(npwmax,rev_pwmax,pwmax,temp_pwmax,
     *                  nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
c-elena 21/12/05
     *                  ncorr,temp_corr,fpw,fhr,new_fpw,new_fhr,
     *                  cpw,cpwt,chr,chrt,new_pwm,new_hrate,
     *                  st_dev_pwmax,st_dev_hrate,delta_p,delta_hr,
c-elena 21/12/05
     *                  delta_fpw,delta_fhr)
**************************************************************************************
c     confronta i punti di lavoro assegnati con quelli calcolati di una data turbina
c     e determina la deviazione standard per la massima potenza e l'heat rate
**************************************************************************************
c     tutti i calcoli vengono eseguiti su valori adimensionali
**************************************************************************************
      implicit none
c
c---------------------------->INPUT
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

c-elena 21/12/05 introduco il calcolo dei fattori correttivi alla temperature inserite da utente
c                          !Tabella dei fattori correttivi
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

c---------------------------->OUTPUT
c                          !grandezze relative ai punti calcolati:
      real*4 new_pwm(*),     !massima potenza 
	                     !(fisso temperatura e numero di giri)
     *     new_hrate(*),    !heat rate
     *     new_fpw(*),     !correzione potenza  
     *     new_fhr(*)      !correzione heat rate
	                     !(fisso temperatura,numero di giri e potenza)
	real*4 delta_p(*),   !differenza valore di potenza calcolato e misurato
     *       delta_hr(*),   !differenza valore di heatrate calcolato e misurato
     *       delta_fpw(*),   !differenza valore di fpw calcolato e misurato
     *       delta_fhr(*)   !differenza valore di fhr calcolato e misurato
c
      real*4 st_dev_pwmax,   !deviazione standard per la massima potenza
     *     st_dev_hrate    !deviazione standard per lo heat rate
c
c---------------------------->Variabili locali
c
      integer*4 i
c
	real*4 fp,      !
     *     pwmax1,  ! variabili ausiliarie usate per definire le due
     *     fh,      ! deviazioni standard
     *     hrate1   !
c
***********************************************************************************************
c
c     Inizializzazioni
c
      st_dev_pwmax=0.0
	st_dev_hrate=0.0
c
c     Confronta i valori assegnati con quelli calcolati ed esegue i calcoli statistici
c
      do i=1,npwmax
	   fp=cpwt(1)+cpwt(2)*temp_pwmax(i)+cpwt(3)*(temp_pwmax(i)**2)+
     *      cpwt(4)*(temp_pwmax(i)**3)
	   pwmax1=cpw(1)+cpw(2)*rev_pwmax(i)+cpw(3)*(rev_pwmax(i)**2)
         new_pwm(i)=fp*pwmax1
	   delta_p(i)=new_pwm(i)-pwmax(i)
	   st_dev_pwmax=st_dev_pwmax+(delta_p(i))**2
	end do
c
      do i=1,nhrate
	   fh=chrt(1)+chrt(2)*temp_hrate(i)+chrt(3)*(temp_hrate(i)**2)+
     *      chrt(4)*(temp_hrate(i)**3)
	   hrate1=chr(1)+chr(2)*rev_hrate(i)+chr(3)*pw_hrate(i)+
     *          chr(4)*rev_hrate(i)*pw_hrate(i)+chr(5)*(rev_hrate(i)**2)
     *          +chr(6)*(pw_hrate(i)**2)
	   new_hrate(i)=fh*hrate1
	   delta_hr(i)=new_hrate(i)-hrate(i)
	   st_dev_hrate=st_dev_hrate+(delta_hr(i))**2
	end do
c
      st_dev_pwmax=sqrt(st_dev_pwmax)/npwmax    !valori adimensionali
	st_dev_hrate=sqrt(st_dev_hrate)/nhrate
c
c-elena
      do i=1,ncorr
	   new_fpw(i)=cpwt(1)+cpwt(2)*temp_corr(i)+
     *              cpwt(3)*(temp_corr(i)**2)+cpwt(4)*(temp_corr(i)**3)
	   delta_fpw(i)=new_fpw(i)-fpw(i)
	   new_fhr(i)=chrt(1)+chrt(2)*temp_corr(i)+
     *              chrt(3)*(temp_corr(i)**2)+chrt(4)*(temp_corr(i)**3)
	   delta_fhr(i)=new_fhr(i)-fhr(i)
	end do
c-elena
      return
	end
c--------------------------------------------------------------------------------------------
      subroutine stampa_t(unit,file,turb,comment,nom_rev,nom_power,
     *                    nom_hrate,nom_tair,ncorr,temp_cor,pw_cor,
     *                    hrate_cor,npwmax,rev_pwmax,pwmax,temp_pwmax,
     *                    nhrate,rev_hrate,pw_hrate,temp_hrate,hrate,
     *                    new_pwm,new_hrate,new_fpw,new_fhr,
     *                    cpw,cpwt,chr,chrt,
     *                    st_dev_pwmax,st_dev_hrate)
*********************************************************************************************
c     stampa sul file indicato della cartella c:\sire\src\analisi_statistica\dati 
c     le grandezze caratteristiche di una turbina calcolate a partire da un
c     insieme di punti di lavoro assegnati,tabulati nel file suddetto 
*********************************************************************************************
      implicit none
c
c----------------------------->INPUT
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
c                          !grandezze relative ai punti calcolati:
      real new_pwm(*),     !massima potenza 
	                     !(fisso temperatura e numero di giri)
     *     new_hrate(*),    !heat rate
	                     !(fisso temperatura,numero di giri e potenza)
     *     new_fpw(*),    
     *     new_fhr(*)    
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
      real st_dev_pwmax,   !deviazione standard per la massima potenza
     *     st_dev_hrate    !deviazione standard per lo heat rate
c
c--------------------------->Intestazioni
c
	character turbin*12/'**TURBINA   '/ 
	character rev*14/'REV_NUM_NOM'/
	character pow*11/'POTENZA_NOM'/
	character hea*13/'HEAT_RATE_NOM'/		  
	character air*10/'T_ARIA_NOM'/
	character corr*17/'CORREZIONE_T_ARIA'/
      character temp_corr*11/'TEMPERATURA'/
	character power_corr*18/'CORREZIONE_POTENZA'/
	character heat_corr*20/'CORREZIONE_HEAT_RATE'/
	character pcurv*11/'POTENZA_MAX'/
	character hcurv*9/'HEAT_RATE'/
	character revol*9/'REV_NUM_%'/
	character heat*11/'HEAT_RATE_%'/
	character ppow*9/'POTENZA_%'/
	character atem*6/'T_ARIA'/
	character maxp*29/'COEFFICIENTI MASSIMA POTENZA'/
      character maxpc*40/'COEFFICIENTI CORREZIONE MASSIMA POTENZA'/
	character hrat*22/'COEFFICIENTI HEAT_RATE'/
      character hratc*33  /'COEFFICIENTI CORREZIONE HEAT_RATE'/
      character st_pwm*36/'MASSIMA POTENZA - STANDARD DEVIATION'/
      character st_hr*31/'HEAT_RATE - STANDARD DEVIATION'/
c     
c-------------------------->Variabili locali
c
      logical*2 warn   !variabile di controllo
c
      integer error,i
*********************************************************************************************
c     
c     Inizializzazioni
c
      warn=.false.
c
c     Apre il file di output per la turbina
c  
      open(UNIT=unit,FILE=file,
     *     	STATUS='replace',ACTION='write',IOSTAT=error)
c
c     Scrive il nome della turbina e il relativo commento
c
	write(unit,100,err=9000) turbin,turb,comment
9000  continue
c
c     Valori nominali
c
	write(unit,110,err=9001) rev,pow,hea,air
9001  continue
	write(unit,115,err=9002) nom_rev,nom_power,nom_hrate,nom_tair
9002  continue
c
c     Correzioni per la temperatura
c
      if(ncorr.gt.0) then
	  write(unit,110,err=9003) corr
9003    continue
	  write(unit,120,err=9004) temp_corr,power_corr,heat_corr
9004    continue
c        do 10 i=1,ncorr
c	  write(unit,140,err=10) temp_cor(i),pw_cor(i),
c     *            hrate_cor(i)
c10      continue
        do 10 i=1,ncorr
	  write(unit,240,err=10) temp_cor(i),pw_cor(i),new_fpw(i),
     *            hrate_cor(i),new_fhr(i)
10      continue
      else
	  write(unit,111,err=9005) corr
9005    continue
	  write(unit,121,err=9006) temp_corr,power_corr,heat_corr
9006    continue
      endif
c
c     Punti sulla curva di massima potenza
c
      if(npwmax.gt.0) then
	  write(unit,110,err=9007) pcurv
9007    continue
	  write(unit,130,err=9008) revol,ppow,atem
9008    continue
	  do 30 i=1,npwmax
 	    write(unit,125,err=30) rev_pwmax(i),pwmax(i),new_pwm(i),
     *            temp_pwmax(i)
30        continue
      else
	  write(unit,111,err=9009) pcurv
9009      continue
	  write(unit,131,err=9010) revol,ppow,atem
9010      continue
      endif  
c
c     Punti sulla curva ad heat rate costante
c
      if(nhrate.gt.0) then
	  write(unit,110,err=9011) hcurv
9011    continue
	  write(unit,136,err=9012) revol,ppow,atem,heat
9012    continue
	  do 40 i=1,nhrate
	    write(unit,135,err=40) rev_hrate(i),pw_hrate(i),
     *               temp_hrate(i),hrate(i),new_hrate(i)
40        continue
      else
	  write(unit,111,err=9013) hcurv
9013      continue
	  write(unit,106,err=9014) revol,ppow,atem,heat
9014      continue
      endif
c
c     Coefficienti massima potenza	
c
	write(unit,170,err=9015) maxp
9015  continue
	write(unit,175,err=9016) (cpw(i),i=1,3)
9016  continue
c
c     Coefficienti correzione massima potenza
c
	write(unit,170,err=9017) maxpc
9017  continue
	write(unit,180,err=9018) (cpwt(i),i=1,4)
9018  continue
c
c     Coefficienti heat rate
c
	write(unit,170,err=9019) hrat
9019  continue
	write(unit,185,err=9020) (chr(i),i=1,6)
9020  continue
c
c     Coefficienti correzione heat rate
c
	write(unit,170,err=9021) hratc
9021  continue
	write(unit,180,err=9022) (chrt(i),i=1,4)
9022  continue
c
c     Deviazione standard massima potenza
c
	write(unit,170,err=9023) st_pwm
9023  continue
	write(unit,190,err=9024) st_dev_pwmax
9024  continue
c
c     Deviazione standard heat rate
c
	write(unit,170,err=9025) st_hr
9025  continue
	write(unit,190,err=9026) st_dev_hrate
9026  continue
c
      close(unit)
c---------------->Specifiche di formato
  100	format(a,a,1x,a)
  105 format(1x,a,3x,a,3x,a,3x,a,7x,a,6x,a,5x,a)
  106 format(1x,'%%',a,3x,a,3x,a,3x,a,7x,a,6x,a,5x,a)
  110 format(a,3x,a,3x,a,3x,a,3x,a,3x,a,3x,a)
  111 format(1x,'%%',a,3x,a,3x,a,3x,a,3x,a,3x,a,3x,a)
C     valori nominali
  115 format(f10.0,5x,f10.0,5x,f10.0,5x,f10.0)
  120 format(1x,a,9x,a,9x,a)
  121 format(1x,'%%',a,9x,a,9x,a)
c     massima potenza
  125 format(f6.1,8x,f5.1,' => ',f5.1,f11.1)
  130 format(1x,a,7x,a,8x,a)
  131 format(1x,'%%',a,5x,a,5x,a,17x,a,6x,a,5x,a)
c     heat rate 
  135 format(f6.1,3X,f10.1,f11.1,9x,f5.1,' => ',f5.1)
  136 format(1x,a,3x,a,3x,a,10x,a)
c     valori correttivi
  140 format(f8.1,12x,f12.2,16x,f12.2)
  170 format(a)
  175 format(3(1X,F9.4))
  180 format(4(1X,F9.4))
  185 format(6(1X,F9.4))
  190 format(1X,F10.4)
c-elena
  240 format(f8.1,12x,f8.4,' => ',f8.4,7x,f8.4,' => ',f8.4)

      return
      end
c--------------------------------------------------------------------------------------------