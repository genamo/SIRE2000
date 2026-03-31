      subroutine stat_c(npoint,head,flow,rev,eff,
     *                  nlim,alim,blim,clim,lim_h,chn,chc,cen,cec,
     *                  new_rev,new_eff,sq1_r,sq2_r,sq1_e,sq2_e)
***************************************************************************************
c     confronta i punti di lavoro assegnati con quelli calcolati di un dato compressore 
c     e determina la deviazione standard,lo scostamento medio per il numero di giri e per l'efficienza
***************************************************************************************
c     tutti i calcoli vengono eseguiti su valori adimensionali
***************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c
c--------------------->INPUT
c
      integer*4 npoint   !punti di lavoro assegnati

c                      !grandezze relative ai punti di lavoro assegnati:
      real*4 head(*),    !altezza adiabatica
     *     flow(*),    !portata
     *     rev(*),     !numero di giri
     *     eff(*)      !efficienza
	                 !(vettori di dimensione npoint)
c
      integer*4 nlim     !punti limite assegnati
c
      real*4 alim(*),    !termini noti della spezzata anti-surge (dim=nlim+1)     
     *     blim(*),    !coef. angolari della spezzata anti-surge (dim=nlim+1)
     *     clim(*),    !costanti delle curve limite
     *     lim_h(*),   !valori limite della prevalenza
     *     chn(*),     !coef di h=f(q,n) nella zona normale (dim=6)
     *     chc(*),     !coef di h=f(q,n) nella zona di choking (dim=6)
     *     cen(*),     !coef di eff=f(q,n) nella zona normale (dim=5)
     *     cec(*)      !coef di eff=f(q,n) nella zona di choking (dim=6)
c
C---------------------->OUTPUT
C
c                      !grandezze calcolate mantenendo fissa la prevalenza e la portata:
      real*4 new_rev(*), !numero di giri
     *     new_eff(*)  !efficienza
	                 !(vettori di dimensione npoint)

      real*4 sq1_r,      !deviazione standard per i giri
     *     sq2_r,      !scostamento medio per i giri
     *     sq1_e,      !deviazione standard per l'efficienza
     *     sq2_e       !scostamento medio per l'efficienza
c
c---------------------> variabili locali
c
      integer*4 np_r,    !pti usati per calcolare i valori statistici per i giri
     *        np_e     !pti usati per calcolare i valori statistici per l'eff
C    
      integer*4 i,       
     *        irange
c
      real*4 qmin,cnew
***************************************************************************************
c     
c     Inizializzazioni
c
      sq1_r=0.0
      sq2_r=0.0
      sq1_e=0.0
      sq2_e=0.0
	np_r=0
	np_e=0
c
c     Confronta i valori assegnati con quelli calcolati ed esegue i calcoli statistici
c
      do i=1,npoint
	   cnew=head(i)/(flow(i)**2)
c
	  !calcolo il valore di Q che sulla spezzata anti-surge corrisponde al valore di H
	  !assegnato
c
        call qdah_su_a(alim,blim,nlim,lim_h,head(i),qmin,irange)
c
	  !stabilisco se il punto assegnato si trova nella zona operativa e in questo caso
	  !se cade nella zona normale o in quella di choking
c
	  if (flow(i).lt.qmin.or.cnew.lt.clim(4)) then
c       Caso 1:il punto cade fuori dalla zona operativa
	     new_rev(i)=undef
	     new_eff(i)=undef
	  else if (cnew.ge.clim(3)) then
c	  Caso 2:il punto cade nella zona normale
           !calcolo relativo ai giri             
           if (head(i).gt.0) then
              call ndahq_a(head(i),flow(i),new_rev(i),chn)
	        np_r=np_r+1
	        sq1_r=sq1_r+(new_rev(i)-rev(i))**2
	        sq2_r=sq2_r+abs(new_rev(i)-rev(i))
	        call effdaqn_a(new_eff(i),flow(i),new_rev(i),cen,cec,clim)
           else             !head(i)=undef
	        new_rev(i)=undef
	        new_eff(i)=undef
	     end if
           !calcolo relativo all'efficienza
		 if (eff(i).gt.0.and.head(i).gt.0) then
	        np_e=np_e+1
	        sq1_e=sq1_e+(new_eff(i)-eff(i))**2
	        sq2_e=sq2_e+abs(new_eff(i)-eff(i))
	     end if
	  else
c       Caso 3:il punto cade nella zona di choking
           !calcolo relativo ai giri
           if (head(i).gt.0) then
              call ndahq_a(head(i),flow(i),new_rev(i),chc)
	        np_r=np_r+1
	        sq1_r=sq1_r+(new_rev(i)-rev(i))**2
	        sq2_r=sq2_r+abs(new_rev(i)-rev(i))
 	        call effdaqn_a(new_eff(i),flow(i),new_rev(i),cen,cec,clim)
          else             !head(i)=undef
	        new_rev(i)=undef
	        new_eff(i)=undef
	     end if
           !calcolo relativo all'efficienza
		 if (eff(i).gt.0.and.head(i).gt.0) then
	        np_e=np_e+1
	        sq1_e=sq1_e+(new_eff(i)-eff(i))**2
	        sq2_e=sq2_e+abs(new_eff(i)-eff(i))
	     end if
        end if
	end do
c
c     Calcolo dei valori statistici
c
      if (np_r.gt.0) then
	   !deviazione standard per i giri
	   sq1_r=(sqrt(sq1_r))/np_r
	   !scostamento medio per i giri
	   sq2_r=(sq2_r/np_r)
      else
	   sq1_r=0.
	   sq2_r=0.
	end if
c
      if (np_e.gt.0) then
	   !deviazione standard per l'efficienza
	   sq1_e=(sqrt(sq1_e))/np_e
	   !scostamento medio per l'efficienza
	   sq2_e=(sq2_e/np_e)
      else
	   sq1_e=0.
	   sq2_e=0.
	end if
c
      return
	end
c-----------------------------------------------------------------------------------------------
      subroutine stat_cc(npoint,head,rev,new_rev,eff,new_eff,
     *                   delta,delta_eff,delta_max,delta_max_eff,
     *                   idelta_max,idelta_max_eff,codice) 
c*********************************************************************************
c  Calcola il delta tra punto assegnato e misurato,il massimo scostamento percentuale
c  e individua il punto in corrispondenza del quale si ha tale valore per un dato 
c  compressore
c-----------------------------------------------------------------------------------
c  I calcoli vengono eseguiti su valori dimensionali
c********************************************************************************
      implicit none
      include '../inc/rk_param.inc'

c---------------->INPUT
c
      integer*4 npoint   !punti di lavoro assegnati

c                      !grandezze relative ai punti di lavoro assegnati:
      real*4 head(*),    !altezza adiabatica
     *     rev(*),     !numero di giri
     *     eff(*)      !efficienza
	                 !(vettori di dimensione npoint)
c                      !grandezze calcolate mantenendo fissa la prevalenza e la portata:
      real*4 new_rev(*), !numero di giri
     *     new_eff(*)  !efficienza
	                 !(vettori di dimensione npoint)
c
c-------------------->OUTPUT
C
      real*4 delta(*),            !differenza giri assegnati e calcolati
     *     delta_eff(*)         !differenza efficienza assegnata e calcolata

      real*4 delta_max,        !max scostamento percentuale per i giri
     *     delta_max_eff     !max scostamento percentuale per l'efficienza
c
      integer*4 idelta_max,    !indice del punto col max scostamento per i giri
     *        idelta_max_eff !indice del punto col max scostamento per l'eff

	integer*4 codice   !codice di controllo sui valori ricalcolati dal sistema
					   !0-->ok
					   !1-->ko con grafici di supporto
					   !2-->ko	

c---------------------->variabili locali
      integer*4 i
c
      real*4 delta1,           !variabili usate per la ricerca del massimo
     *     delta1_eff        !scostamento percentuale
c**********************************************************************************
c
c     Inizializzazioni
c
      delta_max=0.
	delta_max_eff=0.
      idelta_max=0
	idelta_max_eff=0
	codice=0
c     
	do i=1,npoint
        if (head(i).eq.undef) then
	     delta(i)=undef
	     delta_eff(i)=undef
        else if (eff(i).eq.undef) then
           if(new_rev(i).ne.undef) then
             delta(i)=new_rev(i)-rev(i)
             delta1=100*abs(delta(i))/rev(i)
             if (delta1.gt.delta_max)then
                delta_max=delta1
                idelta_max=i
             endif
	       delta_eff(i)=undef
           else if(new_rev(i).eq.undef) then
	          delta(i)=undef
	          delta_eff(i)=undef
           endif
        else if(eff(i).ne.undef) then
           if((new_eff(i).ne.undef).and.(new_rev(i).ne.undef)) then
              delta(i)=new_rev(i)-rev(i)
              delta1=100*abs(delta(i))/rev(i)
              if (delta1.gt.delta_max)then
                 delta_max=delta1
                 idelta_max=i
              endif
              delta_eff(i)=new_eff(i)-eff(i)
              delta1_eff=100*abs(delta_eff(i))/eff(i)
              if (delta1_eff.gt.delta_max_eff)then
                 delta_max_eff=delta1_eff
                 idelta_max_eff=i
              endif
           else if((new_eff(i).eq.undef).and.(new_rev(i).ne.undef))then
              delta(i)=new_rev(i)-rev(i)
              delta1=100*abs(delta(i))/rev(i)
              if (delta1.gt.delta_max)then
                 delta_max=delta1
                 idelta_max=i
              endif
	        delta_eff(i)=undef
           else if((new_eff(i).ne.undef).and.(new_rev(i).eq.undef)) then
              delta_eff(i)=new_eff(i)-eff(i)
              delta1_eff=100*abs(delta_eff(i))/eff(i)
              if (delta1_eff.gt.delta_max_eff)then
                 delta_max_eff=delta1_eff
                 idelta_max_eff=i
              endif
	        delta(i)=undef
           else
	        delta(i)=undef
	        delta_eff(i)=undef
           endif
	  end if
	end do
c-ele 6/9/2006 controllo sui valori ricalcolati
      if(idelta_max.ne.0.and.idelta_max_eff.ne.0) then
	   codice=0
	else if (idelta_max.ne.0.and.idelta_max_eff.eq.0) then
	   codice=1
	else if (idelta_max.eq.0.and.idelta_max_eff.ne.0) then
	   codice=1
	else
	   codice=2
	end if
c-ele

      return 
      end

c---------------------------------------------------------------------------------
      subroutine stampa_c(unit,file,compr,comment,nom_rev,nom_flow,
     *                 nom_head,max_rev,min_rev,min_eff,npoint,rev,flow,
     *                 head,eff,new_rev,new_eff,nlim,lim_n,lim_q,
     *                 lim_h,alim,blim,clim,chlim_n,chlim_q,chlim_h,
     *                 chn,chc,cen,cec,sq1_r,sq2_r,sq1_e,sq2_e)     
************************************************************************************************
c     stampa sul file indicato della cartella c:\sire\src\analisi_statistica\dati 
c     le grandezze caratteristiche di un compressore calcolate a partire da un
c     insieme di punti di lavoro assegnati,tabulati nel file suddetto 
************************************************************************************************
      implicit none
      include '../inc/rk_param.inc'
c-------------------------->INPUT
c
      integer unit           !numero di unità da associare al file di output
c
      character file*(*),    !file di output
     *	      compr*(*),   !compressore analizzato
     *          comment*(*)  !commento sul compressore analizzato
c
                             !valori nominali:   
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
     *     eff(*),           !efficienza
	                       !grandezze relative ai punti calcolati:
     *     new_rev(*),       !numero di giri
     *     new_eff(*)        !efficienza
	                       !(vettori di dimensione npoint)
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
     *     cen(*),           !coeff di eff=f(n,q) nella zona normale (dim=5)
     *     cec(*)            !coeff di eff=f(n,q) nella zona di choking(dim=6) 
c
                             !valori statistici:
	real sq1_r,            !deviazione standard per i giri
     *     sq2_r,            !scostamento medio per i giri
     *     sq1_e,            !deviazione standard per l'efficienza
     *     sq2_e             !scostamento medio per l'efficienza
c
c--------------------------->variabili locali
c
      logical*2 warn           !variabile di controllo
c
      real delta,            !
     *     delta1,           !variabili usate per la ricerca del massimo
     *     delta_eff,        !scostamento percentuale
     *     delta1_eff        !
c
      real delta_max,        !max scostamento percentuale per i giri
     *     delta_max_eff     !max scostamento percentuale per l'efficienza
c
      integer idelta_max,    !indice del punto col max scostamento per i giri
     *        idelta_max_eff !indice del punto col max scostamento per l'eff
c
      integer error,i
c
c--------------------------->intestazioni 
c
	character comp*15/'**COMPRESSORE  '/
      character revol*11/'REV_NUM_NOM'/
	character flo*13/'PORT_NOM'/		  
	character hea*8/'HEAD_NOM'/
	character max*9/'MAX_REV_%'/
	character min*9/'MIN_REV_%'/
	character mief*9/'MIN_EFF_%'/
	character curve*17/'PUNTI DI LAVORO'/
	character revnu*7/'REV_NUM'/
	character flow_rate*7/'PORT'/
	character errre*8/'CALC-MIS'/
	character erref*8/'CALC-MIS'/
	character adiab*6/'HEAD'/
	character efficen*10/'EFFICIENZA'/
      character lchio*3/'$$$'/
      character lims*32/'COEFFICIENTI DEL LIMITE DI SURGE'/
      character limc1*43/'LIMITE RELATIVO DI CHOKING'/
      character ccoef*12/'COEFFICIENTE'/
      character limc2*43/'COEFFICIENTE DEL LIMITE ASSOLUTO DI CHOKING'/
      character revc*39/'COEFFICIENTI DELLE CURVE A REV COSTANTE'/
      character revcc*57
     * /'COEFFICIENTI DELLE CURVE A REV COSTANTE - ZONA DI CHOKING'/
      character effc*39/'COEFFICIENTI DELLE CURVE A EFF COSTANTE'/
      character effcc*57
     * /'COEFFICIENTI DELLE CURVE A EFF COSTANTE - ZONA DI CHOKING'/
      character ln*8/'REV_NUM'/
      character lq*7/'PORT'/
      character lh*4/'HEAD'/
      character la*9/'TERM_NOTO'/
      character lb*8/'PENDENZA'/
      character hsq1_r*39/'REV_NUM - STANDARD DEVIATION'/
      character hsq2_r*39/'REV_NUM - SCOSTAMENTO MEDIO'/
      character hsq1_e*39/'EFF     - STANDARD DEVIATION'/
      character hsq2_e*39/'EFF     - SCOSTAMENTO MEDIO'/
      character dmax*42/'REV NUM - MASSIMO SCOSTAMENTO PERCENTUALE'/
      character idmax*42/' SUL PUNTO'/
      character dmax1*42/'EFF     - MASSIMO SCOSTAMENTO PERCENTUALE'/
      character*80 mess_warn
     */'** WARNING --> UNO O PIU'' PUNTI CALCOLATI SONO OLTRE I LIMITI'/
c
************************************************************************************************
c
c     Inizializzazioni
c
      delta_max=0.
	delta_max_eff=0.
      idelta_max=0
	idelta_max_eff=0
	warn=.false.
c
c     Apre il file di output per il compressore
c
      open(UNIT=unit,FILE=file,
     *     	STATUS='replace',ACTION='write',IOSTAT=error)
c
c     Scrive il nome del compressore e il relativo commento 
c
	write(unit,100,err=9000) comp,compr,comment
9000  continue
c
c     Valori nominali
c
	write(unit,110,err=9001) revol,flo,hea,max,min,mief
9001  continue
	write(unit,115,err=9002) nom_rev,nom_flow,nom_head,
     *      max_rev,min_rev,min_eff
9002  continue
c
c     Punti di lavoro
c
	write(unit,100,err=9007) curve
9007  continue
	write(unit,105,err=9008) revnu,errre,flow_rate,adiab,efficen,
     *                             erref
9008  continue
	do i=1,npoint
        if (head(i).eq.undef) then
	     write(unit,145,err=9009) rev(i),lchio,flow(i),
     *             lchio,eff(i),lchio
9009       continue
        else if(eff(i).eq.undef) then
           if((new_eff(i).ne.undef).and.(new_rev(i).ne.undef)) then
             delta=new_rev(i)-rev(i)
             delta1=100*abs(delta)/rev(i)
             if (delta1.gt.delta_max)then
                delta_max=delta1
                idelta_max=i
             endif
	       write(unit,135,err=9010) rev(i),new_rev(i),
     *            delta,
     *            flow(i),head(i),lchio,new_eff(i)
9010         continue
           else if((new_eff(i).eq.undef).and.(new_rev(i).ne.undef)) then
             warn=.true.
             delta=new_rev(i)-rev(i)
             delta1=100*abs(delta)/rev(i)
             if (delta1.gt.delta_max)then
                delta_max=delta1
                idelta_max=i
             endif
	       write(unit,136,err=9011) rev(i),new_rev(i),
     *          delta,
     *          flow(i),head(i),lchio,lchio
9011         continue
           else if((new_eff(i).ne.undef).and.(new_rev(i).eq.undef)) then
             warn=.true.
	       write(unit,137,err=9012) rev(i),lchio,flow(i),
     *             head(i),lchio,new_eff(i)
9012         continue
           else
             warn=.true.
	       write(unit,138,err=9013) rev(i),lchio,flow(i),
     *             head(i),lchio,lchio
9013         continue
           endif
        else if((new_eff(i).ne.undef).and.(new_rev(i).ne.undef)) then
          delta=new_rev(i)-rev(i)
          delta1=100*abs(delta)/rev(i)
          if (delta1.gt.delta_max)then
            delta_max=delta1
            idelta_max=i
          endif
          delta_eff=new_eff(i)-eff(i)
          delta1_eff=100*abs(delta_eff)/eff(i)
          if (delta1_eff.gt.delta_max_eff)then
            delta_max_eff=delta1_eff
            idelta_max_eff=i
          endif
	    write(unit,140,err=9014) rev(i),new_rev(i),
     *         delta,
     *         flow(i),head(i),eff(i),new_eff(i),delta_eff
9014      continue
        else if((new_eff(i).eq.undef).and.(new_rev(i).ne.undef)) then
          warn=.true.
          delta=new_rev(i)-rev(i)
          delta1=100*abs(delta)/rev(i)
          if (delta1.gt.delta_max)then
            delta_max=delta1
            idelta_max=i
          endif
	    write(unit,141,err=9015) rev(i),new_rev(i),
     *         delta,
     *         flow(i),head(i),eff(i),lchio
9015      continue
        else if((new_eff(i).ne.undef).and.(new_rev(i).eq.undef)) then
          warn=.true.
          delta_eff=new_eff(i)-eff(i)
          delta1_eff=100*abs(delta_eff)/eff(i)
          if (delta1_eff.gt.delta_max_eff)then
            delta_max_eff=delta1_eff
            idelta_max_eff=i
          endif
	    write(unit,142,err=9016) rev(i),lchio,flow(i),
     *          head(i),eff(i),new_eff(i),delta_eff
9016      continue
        else
          warn=.true.
	    write(unit,143,err=9017) rev(i),lchio,flow(i),
     *          head(i),eff(i),lchio
9017      continue
          endif
	end do
c
c     Coefficienti del limite di surge
c
	write(unit,100,err=9003) lims
9003  continue
	write(unit,111,err=9027) ln,lq,lh,la,lb
9027  continue
      do i=1,nlim
  	  write(unit,186,err=9004) lim_n(i),lim_q(i),lim_h(i),
     *                               alim(i),blim(i)
9004    continue
      enddo
c
c     Coefficienti della curva limite di choking e di limite operativo
c
	write(unit,100,err=9128) limc1
9128  continue
	write(unit,112,err=9127) ccoef,revnu,lq,lh
9127  continue
      write(unit,188,err=9129) clim(3),chlim_n,chlim_q,chlim_h
9129  continue
	write(unit,100,err=9028) limc2
9028  continue
      write(unit,185,err=9029) clim(4)
9029  continue
c
c     Coefficienti delle curve a rev costante
c
	write(unit,170,err=9021) revc
9021  continue
	write(unit,185,err=9022) (chn(i),i=1,6) 
9022  continue
	write(unit,170,err=9121) revcc
9121  continue
	write(unit,185,err=9122) (chc(i),i=1,6)
9122  continue
c
c     Coefficienti delle curve a eff costante
c
	write(unit,170,err=9023) effc
9023  continue
	write(unit,185,err=9024) (cen(i),i=1,5)
9024  continue
	write(unit,170,err=9025) effcc
9025  continue
	write(unit,185,err=9026) (cec(i),i=1,6)
9026  continue
c
c     Valori statistici relativi ai giri
c
	write(unit,170,err=9030) hsq1_r
9030  continue
	write(unit,190,err=9031) sq1_r
9031  continue
	write(unit,170,err=9035) hsq2_r
9035  continue
	write(unit,190,err=9036) sq2_r
9036  continue
	write(unit,170,err=9097) dmax
9097  continue
	write(unit,190,err=9098) delta_max
9098  continue
	write(unit,170,err=9197) idmax
9197  continue
	write(unit,191,err=9198) rev(idelta_max),
     *      flow(idelta_max),head(idelta_max)
9198  continue
c
c     valori statistici relativi all'efficienza
c
	write(unit,170,err=9032) hsq1_e
9032  continue
	write(unit,190,err=9033) sq1_e
9033  continue
	write(unit,170,err=9037) hsq2_e
9037  continue
	write(unit,190,err=9038) sq2_e
9038  continue
	write(unit,170,err=9100) dmax1
9100  continue
	write(unit,190,err=9101) delta_max_eff
9101  continue
	write(unit,170,err=9200) idmax
9200  continue
	write(unit,192,err=9201) rev(idelta_max_eff),
     *      flow(idelta_max_eff),eff(idelta_max_eff)
9201  continue
c
c     warning
c
      if (warn) then
  	write(unit,170,err=9034) mess_warn
9034  continue
      endif 
c
      close (unit)
c
      return 
c-------------------->Specifiche di formato
  100	format(a,a,1x,a)
  101	format(1x,'%%',a,a,a)
  105   format(5x,a,5x,a,4x,a,4x,a,5x,a,5x,a)
  110   format(a,2x,a,2x,a,2x,a,2x,a,2x,a,2x,a)
  111   format(3X,a,3x,a,3x,a,5x,a,2x,a,3x,a,3x,a)
  112   format(3x,a,3x,a,7x,a,2x,a)
c
c     valori nominali
c
  115   format(f8.0,2x,f10.3,5x,f10.3,3x,f6.0,4x,f6.0,5x,f6.0,9x,f5.3)
  125   format(2x,a,5x,f10.3,7x,f10.3)
  130   format(1x,a,8x,a,8x,a)
  131   format(1x,'%%',a,8x,a,8x,a)
c
c     punti di lavoro
c
  140   format(f6.0,' => ',f6.0,2x,f6.0,1X,f9.3,1x,f10.3,5x,
     *   f5.1,' => ',f5.1,1x,f6.1)
  141   format(f6.0,' => ',f6.0,2x,f6.0,1X,f9.3,1x,f10.3,5x,
     *   f5.1,' => ',a)
  142   format(f6.0,' => ',a,3x,5x,4x,f9.3,1x,f10.3,5x,f5.1,' => ',
     *   f5.1,1x,f6.1)
  143   format(f6.0,' => ',a,3x,5x,4x,f9.3,1x,f10.3,5x,f5.1,' => ',a)
  145   format(f6.0,' => ',a,3x,5x,4x,f9.3,8x,a,5x,f5.1,' => ',a)
  135   format(f6.0,' => ',f6.0,2x,f6.0,1X,f9.3,1x,f10.3,7x,a,
     *     ' => ',f5.1)
  136   format(f6.0,' => ',f6.0,2x,f6.0,1X,f9.3,1x,f10.3,7x,a,
     *     ' => ',a)
  137   format(f6.0,' => ',a,3x,5x,4x,f9.3,1x,f10.3,7x,a,' => ',f5.1)
  138   format(f6.0,' => ',a,3x,5x,4x,f9.3,1x,f10.3,7x,a,' => ',a)
  170   format(a)
  175   format(3(1X,F9.4))
  180   format(5(1X,F9.4))
  185   format(6(1X,F9.4))
  186   format(1x,f9.3,5(1X,F9.4))
  187   format(1x,f9.4,1x,f9.3,4(1X,F9.4))
  188   format(4x,f9.4,4x,f9.3,2x,f8.3,1x,f8.3)
  190   format(1X,F10.4)
  191   format(1X,'REV_NUM=',F10.0,5X,'PORT=',F10.3,5X,
     *         'HEAD=',F10.3)
  192   format(1X,'REV_NUM =',F10.0,5X,'PORT=',F10.3,5X,
     *         'Effic.=',F10.3)
c
      end
