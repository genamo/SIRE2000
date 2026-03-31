c--------------------------------------------------------------------------------
      subroutine design(inpw,max_rev,min_rev,chn,chc,cen,cec,
     *                  nlim,alim,blim,lim_n,lim_q,lim_h,
     *                  clim,tin,pres,FUN,
     *                  flag,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *                  ptr,un_q,un_h)
c***************************************************************************
c      Restituisce il vettore coi punti da usare per la graficazione della 
c      curva di massima potenza della turbina sulla mappa del compressore 
c      nella zona normale
c***************************************************************************
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/cc.inc'	    !:IN accelerazione di gravità
      integer*4  np,maxsud,nsud
	parameter (np=100,maxsud=5,nsud=2)

      real*4 prop_a		!calcola l'esponente della politropica
	external prop_a
      real*4   fun    !function da azzerare
	external fun
c---------------->INPUT
      logical*2 inpw !indica la variabile fissata in input:
	             !.TRUE. = pressione d'ingresso (azzero FUN2(H))
	             !         (se var_as=1)
	             !.FALSE.= pressione d'uscita (azzero FUN(PIN))
	             !         (se var_as=0)
c
      real*4 max_rev,   !max numero di giri compressore
     *	   min_rev,   !min numero di giri compressore
     *	   chn(6),    
     *       chc(6),    !coeff curve caratteristiche
     *       cen(5),    !                compressore
     *       cec(6),
     *	   alim(*),
     *	   blim(*), 
     *	   clim(4),
     *	   lim_n(*),
     *	   lim_q(*),
     *	   lim_h(*)   
c
      integer*4  nlim
c
      real*4 tin,     !temperatura d'ingresso adimensionale
     *       pres     !variabile assegnata in input adimensionale(PIN o POUT)
c
c-------------->OUTPUT
c
      logical*2 flag    !.TRUE. ci sono abb pti per disegnare la curva
	                  !.FALSE. non ci sono abb p.ti per disegnare la curva 
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(*),ha(*), !coordinate HQ per disegnare il ramo A
	                    !(vettori di dim=na) dimensionali
     *       qb(*),hb(*)  !coordinate HQ per disegnare il ramo B 
	                    !(vettori di dim=nb) dimensionali
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(*),      !numero di punti su ogni tronco del ramo A
     *       ptb(*)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(*),un_q(*)  !coordinate dei pti di raccordo 
                              !(dim=ptr) dimensionali
c---------------->varialbili locali
      real*4 qlim(6),hlim(6),
     *	   qqmin,qqmed1,qqmed4,qqmax,dq,
     *       err,eps,base,
     *       pin,pout,
     *       qi,qq,hup,hdown,yup,ydown,fa,fb,
     *       fl,dely,y,y1,y2,y3,f1,f2,yroot(2),
     *       z,ex,
     *       hbuf(2),
     *       unve(2),unor(2),
     *       auxq(np),auxh(np),yappo
     *       ,hhmin,hhmax,dh,h1,h2
cMAR
       real*4 comp_ratio, ier
c- ace
      real*4 p_bar,t_k,aux
c
      integer*4 ind
c
	integer*4 smooth, !indicail tronco in costruzione:
	                  !(0=nessun tronco,1=solo tronco in ha,
	                  ! 2=solo tronco in hb,3=tronco in ha e hb)
     *	      i,irange,itemp,nroot,
     *          nint,kk,j,it,ifirst,npoint,jp
	logical*2 unides,!indica se serve il raccordo a dx delle due semicurve
     *          unisin !indica se serve il raccordo a sx delle due semicurve
CMAR
      integer*4 istaz     
	common/pwaux/qq   !riempie l'area common per la FUN
	common/MAR/istaz


c****************************************************************************
c
      ind=1
      call zerom(eps,base)    !calcola la precisione di macchina
	                        !(aux1.for)
c
c   calcolo degli estremi di variabilità di portata e altezza adiabatica
c
      call limit_abs_ec_a (min_rev,max_rev,chn,chc,clim,nlim,
     *                  	lim_q,lim_h,qlim,hlim)
c
      qqmin  = qlim(3)*qmrif
      qqmed1 = qlim(1)*qmrif
      qqmed4 = qlim(4)*qmrif
      qqmax  = qlim(2)*qmrif
c
      hhmin =hlim(6)*headrif
      hhmax =hlim(1)*headrif

      dq=(qqmax-qqmin)/(np-1)
	dh=(hhmax-hhmin)/(np-1)
c 
      err=pres*1.E-4
	aux = r_mks/AGRVcc/av_weight(ind)
c
      if (inpw) then        !valorizza la variabile assegnata in input
	   pin=pres
	else
	   pout=pres
	end if

c---------------RICERCA DEI PUNTI DELLA CURVA DA TRACCIARE----------------
                               !inizializzazione variabili 
      na=0                     !p.ti effettivi ramo in ha 
	nb=0                     !p.ti effettivi ramo in hb
	nta=0                    !numero tronchi in ha
	ntb=0                    !numero tronchi in hb
	smooth=0                 !rami in costruzione: smooth=0 nessun tronco
	                         !                     smooth=1 1 tronco ha
	                         !                     smooth=2 1 tronco hb
	                         !                     smooth=3 2 tronchi ha e hb
	unides=.false.           !unione a dx dei due rami
	unisin=.false.           !unione a sx dei due rami
      ptr=0
      do i=1,np
c        calcolo degli estremi per la ricerca degli zeri
         qi=qqmin+(i-1)*dq        ! dimensionale     
	   qq=qi/QMRIF
	   if (qi.le.qqmed1) then
            call hdaq_su_a(nlim,alim,blim,qq,lim_q,hup,irange)
         else
            call hdaqn_a(hup,qq,max_rev,chn)
	   end if
	   if (qi.le.qqmed4) then
            call hdaqn_a(hdown,qq,min_rev,chn)
         else
            call hdaq_ch_a(clim(3),hdown,qq)
	   end if
c
         if (inpw) then        !se assegno PIN azzero la funzione in H
	      yup=hup            !FUN2
	      ydown=hdown
	   else                  !se assegno POUT azzero la funzione in PIN
	      hup=hup*HEADRIF    !FUN
	      hdown=hdown*HEADRIF
	      call trans(pout,tin,hup,qi,yup,fl,1)   !(subunir.for)
	      call trans(pout,tin,hdown,qi,ydown,fl,1)
	      yup = yup/(PRIF+P0)
	      ydown = ydown/(PRIF+P0)
c-elena
            if (yup.lt.ydown) then
	          yappo=ydown
	          ydown=yup
	          yup=yappo
		  end if 
c-elena
	   end if
         fa=FUN(ydown)
	   fb=FUN(yup)
	   if ((fa*fb).gt.0) then
	      if (fa.lt.0) then    !ricerca di due zeri
	         do itemp=1,maxsud
	            nroot=0
	            nint=nsud**itemp
	            dely=(yup-ydown)/nint
	            do kk=1,nint
	               y1=ydown+(kk-1)*dely
	               y2=ydown+kk*dely
	               f1=FUN(y1)
	               f2=FUN(y2)
	               if ((f1*f2).lt.0) then
	                  y3=y1
	                  call zero_fun(y1,y2,y3,f1,f2,y,err,eps,FUN)
	                  nroot=nroot+1
	                  yroot(nroot)=y
	               end if
	            end do
	            if (nroot.eq.2) go to 110
			 end do
	         go to 300
110            unides=.true.	   
	                      !valori di h corrispondenti agli zeri trovati
	   
	         if (.not.inpw) then
c- ace !yroot è una pressione adimensionale
	            do j=1,2           
				   p_bar = yroot(j)*(PRIF+P0)  !bar
	               t_k   = tin*(TRIF+T0)       !Kelvin
                     call zpt1(ind,p_bar,t_k,z)

CMAR
        comp_ratio = pout/p_bar
        call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)

cmar				   ex=prop_a(yroot(j),tin,exp_coef(1,ind))
				   ex=(ex-1.)/ex
c- ace               hbuf(j) in Km
				   hbuf(j)=z*t_k*aux*((pout/yroot(j))**ex-1.)/ex  
  				end do
	         else
	            hbuf(1)=yroot(1)*HEADRIF
	            hbuf(2)=yroot(2)*HEADRIF
			 end if
	         if ((na+nb).eq.0) unisin=.true.   !1°loop
	         if (smooth.eq.0) then            !1°loop
	                          !non ci sono tronchi di curva in costruzione
	            na=na+1       !punti effettivi sul primo tronco
	            nb=nb+1       !punti effettivi sul 2° tronco
	            ha(na)=hbuf(1)
	            hb(nb)=hbuf(2)
	            qa(na)=qi
	            qb(nb)=qi
	            smooth=3
	         else if (smooth.eq.3) then
	                         !ci sono due tronchi di curva in costruzione
                  if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
				end if
	         else if (smooth.eq.1) then
	                         !c'è un solo tronco di curva in costruzione in HA
	            smooth=3
                  if(abs(ha(na)-hbuf(1)).gt.abs(ha(na)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
	         else if (smooth.eq.2) then
	                         !c'è un solo tronco di curva in costruzione in HB
	            smooth=3
                  if(abs(hb(nb)-hbuf(1)).gt.abs(hb(nb)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
               end if
            else
	         go to 300

	      end if
	   else               !ricerca di un solo zero(fa e fb di segno discorde)
	      y3=ydown
	      call zero_fun(ydown,yup,y3,fa,fb,y,err,eps,FUN)
	      unides=.false.
	      if (.not.inpw) then
	         p_bar = y*(PRIF+P0)         !bar
	         t_k   = tin*(TRIF+T0)  !Kelvin
               call zpt1(ind,p_bar,t_k,z)

cmar

	comp_ratio = pout/p_bar
        call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)
cmar			 ex=prop_a(y,tin,exp_coef(1,ind))

 		     ex=(ex-1.)/ex
c- ace         hbuf(1) in Km
		     hbuf(1)=z*t_k*aux*((pout/y)**ex-1.)/ex
	      else
	          hbuf(1)=y*HEADRIF
		  end if
            if ((smooth.eq.0).or.(smooth.eq.1)) then
	     !non ci sono tronchi di curva in costruzione o un unico in HA
	          smooth=1
	          na=na+1
                ha(na)=hbuf(1)
	          qa(na)=qi
	      else if (smooth.eq.2) then
	                         !unico tronco in costruzione in HB
	          nb=nb+1
                hb(nb)=hbuf(1)
	          qb(nb)=qi
	      else if (smooth.eq.3) then
	               !due tronchi in costruzione dei quali uno appena estinto
                if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	             smooth=2
	             nb=nb+1
                   hb(nb)=hbuf(1)
	             qb(nb)=qi
	             nta=nta+1          !il tronco in HA si è estinto
	             pta(nta)=na
	          else
	             smooth=1
	             na=na+1
                   ha(na)=hbuf(1)
	             qa(na)=qi
	             ntb=ntb+1          !il tronco in HB si è estinto
	             ptb(ntb)=nb
			  end if
		  end if
	   end if
	   go to 400
300      if ((smooth.eq.1).or.(smooth.eq.3)) then
            nta=nta+1
	      pta(nta)=na
         end if
         if ((smooth.eq.2).or.(smooth.eq.3)) then
            ntb=ntb+1
            ptb(ntb)=nb
         end if
400      continue
	end do  
c         verifica se sono stati trovati abbastanza punti per disegnare la curva
	if ((na+nb).le.2) then
	   flag=.false.                !non ci sono punti sufficienti
         return                      !esco dalla routine
	end if    
      flag=.true.
c
c        verifica se le due semicurve vanno raccordate a destra o a sinistra
c                                    raccordo a sinistra
      if (unisin) then
	   if ((na.ge.2).and.(nb.ge.2)) then
	      if (abs(ha(1)-hb(1)).gt.abs(ha(2)-hb(2))) unisin=.false.
	   end if
	end if
c                                      raccordo a destra           
      if (unides) then
	  if ((na.ge.2).and.(nb.ge.2)) then
	   if (abs(ha(na)-hb(nb)).gt.abs(ha(na-1)-hb(nb-1))) unides=.false.
	  end if
	end if
c
      if (unisin) then
c                           raccordo a sinistra delle due semicurve
          UNVE(1)=HA(1)
          UNVE(2)=HB(1)
          UNOR(1)=QA(1)
          UNOR(2)=QB(1)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
c
         if (pta(1).eq.1) then
c          !prolungamento del primo tronco di ha fin su una curva limite
           unor(1)=qb(1)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(1)/headrif
	     unve(2)=ha(1)/headrif
           call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	                     !descompr.for
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(1).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(1)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(1)/headrif
	     unve(2)=hb(1)/headrif
           call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
      end if
c
c
      if (unides) then
c                           raccordo a destra delle due semicurve
         unve(1)=ha(na)
         unve(2)=hb(nb)
	   unor(1)=qa(na)
	   unor(2)=qb(nb)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
         if (pta(nta).eq.1) then
c          !prolungamento dell'ultimo tronco di ha fin su una curva limite
           unor(1)=qb(nb)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(nb)/headrif
	     unve(2)=ha(1)/headrif
           call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
          un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(ntb).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(na)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(na)/headrif
	     unve(2)=hb(1)/headrif
           call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
c
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
	   end if
      end if
c
      DO IT=1,NTA
C
C--->     GRAFICAZIONE DEI TRONCHI DI HA
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTA(IT-1)+1
          END IF
          NPOINT = PTA(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                AUXq(JP)=QA(IFIRST+JP-1)
                AUXh(JP)=HA(IFIRST+JP-1)
              end do
              IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif
                  call section (unor,unve,chn,chc,nlim,alim,blim,
     *		           lim_q,lim_h,clim,max_rev,min_rev)
                 DO J=1,2
                    UNOR(J) = UNOR(J)*qmrif
                    UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
              END IF
            IF((.NOT.UNIDES).OR.(IT.NE.NTA)) THEN
C
C--->            PROLUNGAMENTO A DESTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMITE
C
                   UNOR(1) = AUXq(NPOINT-1)/qmrif
                   UNOR(2) = AUXq(NPOINT)/qmrif
                   UNVE(1) = AUXh(NPOINT-1)/headrif
                   UNVE(2) = AUXh(NPOINT)/headrif
                   call section (unor,unve,chn,chc,nlim,alim,blim,
     *		        lim_q,lim_h,clim,max_rev,min_rev)
                   DO J=1,2
                      UNOR(J) = UNOR(J)*qmrif
                      UNVE(J) = UNVE(J)*headrif
                   end do
c
                   un_h(ptr+1)=unve(1)
                   un_h(ptr+2)=unve(2)
	             un_q(ptr+1)=unor(1)
	             un_q(ptr+2)=unor(2)
                   ptr=ptr+2
           END IF
	   end if
	end do
C--->
      DO IT=1,NTB
C
C--->     GRAFICAZIONE DEI TRONCHI DI HB
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTB(IT-1)+1
          END IF
          NPOINT = PTB(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                 AUXq(JP)=QB(IFIRST+JP-1)
                 AUXh(JP)=HB(IFIRST+JP-1)
              end do
            IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif
                  call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
                 DO J=1,2
                        UNOR(J) = UNOR(J)*qmrif
                        UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
            END IF
          IF((.NOT.UNIDES).OR.(IT.NE.NTB)) THEN
C---->        PROLUNGAMENTO A DESTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMITE
            UNOR(1) = AUXq(NPOINT-1)/qmrif
            UNOR(2) = AUXq(NPOINT)/qmrif
            UNVE(1) = AUXh(NPOINT-1)/headrif
            UNVE(2) = AUXh(NPOINT)/headrif
            call section (unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
            DO J=1,2
              UNOR(J) = UNOR(J)*qmrif
              UNVE(J) = UNVE(J)*headrif
            end do
                 un_h(ptr+1)=unve(1)
                 un_h(ptr+2)=unve(2)
	           un_q(ptr+1)=unor(1)
	           un_q(ptr+2)=unor(2)
                 ptr=ptr+2
          END IF
	  end if
      end do
	return
	end  
	
C=======================================================
      subroutine design_ch(inpw,max_rev,min_rev,chn,chc,cen,cec,
     *                  nlim,alim,blim,lim_n,lim_q,lim_h,
     *                  clim,tin,pres,FUN,
     *                  flag,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *                  ptr,un_q,un_h)
c***************************************************************************
c      Restituisce il vettore coi punti da usare per la graficazione della 
c      curva di massima potenza della turbina sulla mappa del compressore 
c      nella zona di choking
c*******************************************************************
C      TRACCIA LA CURVA CORRISPONDENTE ALLA FUNCTION FUN PASSATA COME
C      PARAMETRO. LA CURVA VIENE TRACCIATA SOLO NELLA ZONA DI FUNZIONAMENTO
C      DI CHOKING DEL COMPRESSORE E VIENE RACCORDATA ALLE CURVE LIMITE QUANDO
C      NECESSARIO.
C      N.B. PER IL BUON FUNZIONAMENTO E` NECESSARIO CHE PER UN DATO VALORE
C           DI PORTATA SI ABBIANO AL PIU` DUE SOLUZIONI PER L'ALTEZZA
C           ADIABATICA.
c***************************************************************************
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/cc.inc'	!:IN

c
      integer*4 np,maxsud,nsud
      PARAMETER (NP=100,   MAXSUD=5  ,  NSUD=2)
      real*4 prop_a					!calcola l'esponente della politropica
	external prop_a

c---------------->INPUT
      logical*2 inpw !indica la variabile fissata in input:
	             !.TRUE. = pressione d'ingresso (azzero FUN2(H))
	             !         (se var_as=1)
	             !.FALSE.= pressione d'uscita (azzero FUN(PIN))
	             !         (se var_as=0)
c
      real*4 max_rev,   !max numero di giri compressore
     *	   min_rev,   !min numero di giri compressore
     *	   chn(6),    
     *       chc(6),    !coeff curve caratteristiche
     *       cen(5),    !                compressore
     *       cec(6),
     *	   alim(*),
     *	   blim(*), 
     *	   clim(4),
     *	   lim_n(*),
     *	   lim_q(*),
     *	   lim_h(*)   
c
      integer*4  nlim
c
      real*4 tin,     !temperatura d'ingresso adimensionale
     *       pres     !variabile assegnata in input adimensionale(PIN o POUT)
c
      real*4   fun    !function da azzerare
	external fun
c
c--------------->OUTPUT
c
      logical*2 flag    !.TRUE. ci sono abb pti per disegnare la curva
	                  !.FALSE. non ci sono abb p.ti per disegnare la curva 
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(*),ha(*), !coordinate HQ per disegnare il ramo A
	                    !(vettori di dim=na) dimensionali
     *       qb(*),hb(*)  !coordinate HQ per disegnare il ramo B 
	                    !(vettori di dim=nb) dimensionali
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(*),      !numero di punti su ogni tronco del ramo A
     *       ptb(*)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(*),un_q(*)  !coordinate dei pti di raccordo 
                              !(dim=ptr) dimensionali
c---------------->varialbili locali
c
      integer*4 ind     !offset elem. top. fittizio
c
	integer*4 smooth, !indicail tronco in costruzione:
	                  !(0=nessun tronco,1=solo tronco in ha,
	                  ! 2=solo tronco in hb,3=tronco in ha e hb)
     *	      irange
	logical*2 unides,!indica se serve il raccordo a dx delle due semicurve
     *          unisin !indica se serve il raccordo a sx delle due semicurve
     
	common/pwaux/qq   !riempie l'area common per la FUN
c*
      real*4    eps,base,qqmin,qqmed2,qqmed6,qqmax,dq,err,
     *          pout,qi,qq,hup,hdown,yup,ydown,fl,fa,fb,dely,y1,
     *          y2,f1,f2,y3,y,z,ex,
     *          qlim(6),hlim(6),aux,pin,yroot(2),p_bar,t_k,
     *          auxq(np),auxh(np),unor(2),unve(2),hbuf(2)
      integer*4 i,itemp,nroot,nint,kk,j,it,ifirst,
     *          npoint,jp
cmar
      real*4 comp_ratio, ier
C
C----------------------------------------------------------------------------
      ind = 1
      call zerom(eps,base)

c----> CALCOLO DEGLI ESTREMI DI VARIABILITA' DELLA PORTATA E DELL'ALTEZZA
C      ADIABATICA
c
c   calcolo degli estremi di variabilità di portata e altezza adiabatica
c
      call limit_abs_ec_a (min_rev,max_rev,chn,chc,clim,nlim,
     *                  	lim_q,lim_h,qlim,hlim)
c
      qqmin  = qlim(4)*qmrif
      qqmed2 = qlim(2)*qmrif
      qqmed6 = qlim(6)*qmrif
      qqmax  = qlim(5)*qmrif
c
      DQ    = (QQMAX - QQMIN)/(NP-1)
      ERR   =  pres*1.E-4
      aux = r_mks/AGRVcc/av_weight(ind)
      IF (inpw) THEN
        PIN  = pres
      ELSE
        POUT = pres
      END IF
C
C--->      RICERCA DEI PUNTI DELLA CURVA DA TRACCIARE
c---------------RICERCA DEI PUNTI DELLA CURVA DA TRACCIARE----------------
                               !inizializzazione variabili 
      na=0                     !p.ti effettivi ramo in ha 
	nb=0                     !p.ti effettivi ramo in hb
	nta=0                    !numero tronchi in ha
	ntb=0                    !numero tronchi in hb
	smooth=0                 !rami in costruzione: smooth=0 nessun tronco
	                         !                     smooth=1 1 tronco ha
	                         !                     smooth=2 1 tronco hb
	                         !                     smooth=3 2 tronchi ha e hb
	unides=.false.           !unione a dx dei due rami
	unisin=.false.           !unione a sx dei due rami
      ptr=0
c
      DO I=1,NP
C---->       CALCOLO DEGLI ESTREMI PER LA RICERCA DEGLI ZERI
        QI = QQMIN+(I-1)*DQ
        QQ = QI/QMRIF
        IF(QI.LE.QQMED2)THEN
          call hdaq_ch_a(clim(3),hup,qq)
        ELSE 
          call hdaqn_a(hup,qq,max_rev,CHC)
        END IF
        IF (QI.LE.QQMED6) THEN
          call hdaqn_a(hdown,qq,min_rev,CHC)
        ELSE
          call hdaq_ch_a(clim(4),hdown,qq)
        END IF
        IF(inpw) THEN
          YUP  = HUP
          YDOWN= HDOWN
        ELSE 
          HUP  = HUP*HEADRIF
          HDOWN= HDOWN*HEADRIF
          CALL TRANS(POUT,TIN,HUP,QI,YDOWN,FL,1)
          CALL TRANS(POUT,TIN,HDOWN,QI,YUP,FL,1)
          YDOWN =YDOWN / (PRIF+P0)
          YUP  = YUP/ (PRIF+P0)
        END IF
c
         fa=FUN(ydown)
	   fb=FUN(yup)
	   if ((fa*fb).gt.0) then
	      if (fa.lt.0) then    !ricerca di due zeri
	         do itemp=1,maxsud
	            nroot=0
	            nint=nsud**itemp
	            dely=(yup-ydown)/nint
	            do kk=1,nint
	               y1=ydown+(kk-1)*dely
	               y2=ydown+kk*dely
	               f1=FUN(y1)
	               f2=FUN(y2)
	               if ((f1*f2).lt.0) then
	                  y3=y1
	                  call zero_fun(y1,y2,y3,f1,f2,y,err,eps,FUN)
	                  nroot=nroot+1
	                  yroot(nroot)=y
	               end if
	            end do
	            if (nroot.eq.2) go to 110
			 end do
	         go to 300
110            unides=.true.	   
	                      !valori di h corrispondenti agli zeri trovati
	   
	         if (.not.inpw) then
	            do j=1,2           !yroot è una pressione adimensionale
	               p_bar = yroot(j)*(PRIF+P0)  !bar
	               t_k   = tin*(TRIF+T0)  !Kelvin
                     call zpt1(ind,p_bar,t_k,z)
cmar             
           comp_ratio=pout/p_bar
		 
        call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)     

cmar				   ex=prop_a(yroot(j),tin,exp_coef(1,ind))
				   ex=(ex-1.)/ex
				   hbuf(j)=z*t_k*aux*((pout/yroot(j))**ex-1.)/ex
  				end do
	         else
	            hbuf(1)=yroot(1)*HEADRIF
	            hbuf(2)=yroot(2)*HEADRIF
			 end if
	         if ((na+nb).eq.0) unisin=.true.   !1°loop
	         if (smooth.eq.0) then            !1°loop
	                          !non ci sono tronchi di curva in costruzione
	            na=na+1       !punti effettivi sul primo tronco
	            nb=nb+1       !punti effettivi sul 2° tronco
	            ha(na)=hbuf(1)
	            hb(nb)=hbuf(2)
	            qa(na)=qi
	            qb(nb)=qi
	            smooth=3
	         else if (smooth.eq.3) then
	                         !ci sono due tronchi di curva in costruzione
                  if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
				end if
	         else if (smooth.eq.1) then
	                         !c'è un solo tronco di curva in costruzione in HA
	            smooth=3
                  if(abs(ha(na)-hbuf(1)).gt.abs(ha(na)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
	         else if (smooth.eq.2) then
	                         !c'è un solo tronco di curva in costruzione in HB
	            smooth=3
                  if(abs(hb(nb)-hbuf(1)).gt.abs(hb(nb)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
               end if
            else
	         go to 300
	      end if
	   else               !ricerca di un solo zero(fa e fb di segno discorde)
	      y3=ydown
	      call zero_fun(ydown,yup,y3,fa,fb,y,err,eps,FUN)
	      unides=.false.
	      if (.not.inpw) then
	         p_bar = y*(PRIF+P0)    !bar
	         t_k   = tin*(TRIF+T0)  !Kelvin
               call zpt1(ind,p_bar,t_k,z)
cmar
      comp_ratio = pout/p_bar
        call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)

cmar			 ex=prop_a(y,tin,exp_coef(1,ind))
 		     ex=(ex-1.)/ex
		     hbuf(1)=z*t_k*aux*((pout/y)**ex-1.)/ex
	      else
	          hbuf(1)=y*HEADRIF
		  end if
            if ((smooth.eq.0).or.(smooth.eq.1)) then
	     !non ci sono tronchi di curva in costruzione o un unico in HA
	          smooth=1
	          na=na+1
                ha(na)=hbuf(1)
	          qa(na)=qi
	      else if (smooth.eq.2) then
	                         !unico tronco in costruzione in HB
	          nb=nb+1
                hb(nb)=hbuf(1)
	          qb(nb)=qi
	      else if (smooth.eq.3) then
	               !due tronchi in costruzione dei quali uno appena estinto
                if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	             smooth=2
	             nb=nb+1
                   hb(nb)=hbuf(1)
	             qb(nb)=qi
	             nta=nta+1          !il tronco in HA si è estinto
	             pta(nta)=na
	          else
	             smooth=1
	             na=na+1
                   ha(na)=hbuf(1)
	             qa(na)=qi
	             ntb=ntb+1          !il tronco in HB si è estinto
	             ptb(ntb)=nb
			  end if
		  end if
	   end if
	   go to 400
300      if ((smooth.eq.1).or.(smooth.eq.3)) then
            nta=nta+1
	      pta(nta)=na
         end if
         if ((smooth.eq.2).or.(smooth.eq.3)) then
            ntb=ntb+1
            ptb(ntb)=nb
         end if
400      continue
	end do  
c         verifica se sono stati trovati abbastanza punti per disegnare la curva
	if ((na+nb).le.2) then
	   flag=.false.                !non ci sono punti sufficienti
         return                      !esco dalla routine
	end if    
      flag=.true.
c
c        verifica se le due semicurve vanno raccordate a destra o a sinistra
c                                    raccordo a sinistra
      if (unisin) then
	   if ((na.ge.2).and.(nb.ge.2)) then
	      if (abs(ha(1)-hb(1)).gt.abs(ha(2)-hb(2))) unisin=.false.
	   end if
	end if
c                                      raccordo a destra           
      if (unides) then
	  if ((na.ge.2).and.(nb.ge.2)) then
	   if (abs(ha(na)-hb(nb)).gt.abs(ha(na-1)-hb(nb-1))) unides=.false.
	  end if
	end if
c
      if (unisin) then
c                           raccordo a sinistra delle due semicurve
          UNVE(1)=HA(1)
          UNVE(2)=HB(1)
          UNOR(1)=QA(1)
          UNOR(2)=QB(1)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
c
         if (pta(1).eq.1) then
c          !prolungamento del primo tronco di ha fin su una curva limite
           unor(1)=qb(1)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(1)/headrif
	     unve(2)=ha(1)/headrif
           call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
c
	                     !descompr.for
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(1).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(1)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(1)/headrif
	     unve(2)=hb(1)/headrif
           call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
      end if
c
c
      if (unides) then
c                           raccordo a destra delle due semicurve
         unve(1)=ha(na)
         unve(2)=hb(nb)
	   unor(1)=qa(na)
	   unor(2)=qb(nb)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
         if (pta(nta).eq.1) then
c          !prolungamento dell'ultimo tronco di ha fin su una curva limite
           unor(1)=qb(nb)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(nb)/headrif
	     unve(2)=ha(1)/headrif
           call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(ntb).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(na)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(na)/headrif
	     unve(2)=hb(1)/headrif
           call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
c
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
	   end if
      end if
c
      DO IT=1,NTA
C
C--->     GRAFICAZIONE DEI TRONCHI DI HA
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTA(IT-1)+1
          END IF
          NPOINT = PTA(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                AUXq(JP)=QA(IFIRST+JP-1)
                AUXh(JP)=HA(IFIRST+JP-1)
              end do
              IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif
                  call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
                  DO J=1,2
                    UNOR(J) = UNOR(J)*qmrif
                    UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
              END IF
            IF((.NOT.UNIDES).OR.(IT.NE.NTA)) THEN
C
C--->            PROLUNGAMENTO A DESTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMITE
C
                   UNOR(1) = AUXq(NPOINT-1)/qmrif
                   UNOR(2) = AUXq(NPOINT)/qmrif
                   UNVE(1) = AUXh(NPOINT-1)/headrif
                   UNVE(2) = AUXh(NPOINT)/headrif
                   call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		       lim_q,lim_h,clim,max_rev,min_rev)
                    DO J=1,2
                      UNOR(J) = UNOR(J)*qmrif
                      UNVE(J) = UNVE(J)*headrif
                   end do
c
                   un_h(ptr+1)=unve(1)
                   un_h(ptr+2)=unve(2)
	             un_q(ptr+1)=unor(1)
	             un_q(ptr+2)=unor(2)
                   ptr=ptr+2
           END IF
	   end if
	end do
C--->
      DO IT=1,NTB
C
C--->     GRAFICAZIONE DEI TRONCHI DI HB
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTB(IT-1)+1
          END IF
          NPOINT = PTB(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                 AUXq(JP)=QB(IFIRST+JP-1)
                 AUXh(JP)=HB(IFIRST+JP-1)
              end do
            IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif
                  call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		      lim_q,lim_h,clim,max_rev,min_rev)
                   DO J=1,2
                        UNOR(J) = UNOR(J)*qmrif
                        UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
            END IF
          IF((.NOT.UNIDES).OR.(IT.NE.NTB)) THEN
C---->        PROLUNGAMENTO A DESTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMITE
            UNOR(1) = AUXq(NPOINT-1)/qmrif
            UNOR(2) = AUXq(NPOINT)/qmrif
            UNVE(1) = AUXh(NPOINT-1)/headrif
            UNVE(2) = AUXh(NPOINT)/headrif
            call section_ch(unor,unve,chn,chc,nlim,alim,blim,
     *		    lim_q,lim_h,clim,max_rev,min_rev)
            DO J=1,2
              UNOR(J) = UNOR(J)*qmrif
              UNVE(J) = UNVE(J)*headrif
            end do
                 un_h(ptr+1)=unve(1)
                 un_h(ptr+2)=unve(2)
	           un_q(ptr+1)=unor(1)
	           un_q(ptr+2)=unor(2)
                 ptr=ptr+2
          END IF
	  end if
      end do
	return
	end  
	
c-----------------------------------------------------------
      real function fun (pin)
c*******************************************************************
C
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/cc.inc' 	!:IN
cmar
      common/pres/pout_p

	real*4 pout_p, comp_ratio, ier
c 
      real*4    ex,p_bar,t_k,z,aux,pin
	real*4    h,qq2,rev_c,eff,t_aux,ftpw,pwc,rev_t
	real*4    h_km,qm_dim,ro_dim 
c
	integer*4 ind
      real*4   prop_a
      EXTERNAL prop_a
c
      real*4 q,pow_corr,eff_corr,CHN(6),CHC(6),CEN(5),CEC(6),
     *       nlim,alim(max_nlim),blim(max_nlim),
     *       lim_n(max_nlim),lim_q(max_nlim),lim_h(max_nlim),
     *       clim(4),CPW(3),CPWT(4),pwt,tin,tair,ppin,pout,
     *       nom_power
c
      COMMON/PWAUX/Q,CEN,CEC,CHN,CHC,NLIM,ALIM,BLIM,CLIM,
     *             LIM_N,LIM_Q,LIM_H,PWT,TIN,POUT,
     *             EFF_CORR,PPIN,CPW,CPWT,
     *             TAIR,POW_CORR,nom_power


      

c
CMAR_design 
c
C--------------------------------------------------------------------------------
c
      ind=1
	aux = r_mks/AGRVcc/av_weight(ind)
c     pin e tin adimensionali      
CMAR	EX = PROP_a(pin,tin,exp_coef(1,ind))



	p_bar = pin*(PRIF+P0)  !bar
	t_k   = tin*(TRIF+T0)  !Kelvin
cmar
	comp_ratio=pout_p/p_bar
		 
      call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)     
      call zpt1(ind,p_bar,t_k,z)
	ex=(ex-1.)/ex
	h=z*t_k*aux*((pout/pin)**ex-1.)/ex  !Km 
	h= h/HEADRIF 
      QQ2 = Q*Q
      IF ((H/QQ2).GE.CLIM(3)) THEN        !zona normale
        call ndahq_a(h,q,rev_c,CHN)
        call effdaqn_a(eff,q,rev_c,cen,cec,clim)
      ELSE
        IF (CLIM(3).NE.CLIM(4)) THEN      !zona di choking
          call ndahq_a(h,q,rev_c,CHC)
          call effdaqn_a(eff,q,rev_c,cen,cec,clim)
        ELSE
C---->  This case is necessary in order to prevent errors due the machine
c       precision on the evaluation of the boundaries of the research.
          call ndahq_a(h,q,rev_c,CHC)
          call effdaqn_a(eff,q,rev_c,cen,cec,clim)
        ENDIF
      ENDIF
c
c potenza richiesta dal compressore
c
c      eff  = eff*eff_corr
c-elena-5/12/2005
c NOTA: in input viene letta la variazione di efficienza (eff_corr), nelle simulazioni e'
c       utilizzata moltiplicando eff = eff * eff_corr

      eff  = eff*(1.+eff_corr)
c-elena-5/12/2005
cgpe
      if (eff .lt. 0.01) eff = 0.01
cgpe-end
	h_km = h*HEADRIF
	qm_dim = q*QMRIF
      ro_dim = p_bar*av_weight(ind)*PMKS/(r_mks*z*t_k*3600)
	pwc = agrvcc*h_km*qm_dim*ro_dim/eff
	pwc = pwc/POWERIF
c
      rev_t = rev_c
	t_aux=tair
	FTPW=CPWT(1)+T_AUX*(CPWT(2)+CPWT(3)*T_AUX+CPWT(4)*T_AUX**2)
	PWT = (CPW(1)+CPW(2)*REV_T+CPW(3)*REV_T*REV_T)*FTPW*POW_CORR
      FUN = PWT - PWC
      RETURN
      END
C-------------------------------------------------------------------
      REAL FUNCTION FUN2 (H)
c*******************************************************************
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
	include '../inc/cc.inc'		!:IN

c
c
	integer*4 ind
      real*4   prop_a
      EXTERNAL prop_a
c
      real*4 q,pow_corr,eff_corr,CHN(6),CHC(6),CEN(5),CEC(6),
     *       nlim,alim(max_nlim),blim(max_nlim),
     *       lim_n(max_nlim),lim_q(max_nlim),lim_h(max_nlim),
     *       clim(4),CPW(3),CPWT(4),pwt,tin,tair,pin,pout,
     *       nom_power
c
      COMMON/PWAUX/Q,CEN,CEC,CHN,CHC,NLIM,ALIM,BLIM,CLIM,
     *             LIM_N,LIM_Q,LIM_H,PWT,TIN,POUT,
     *             EFF_CORR,PIN,CPW,CPWT,
     *             TAIR,POW_CORR,nom_power
c
      real     qq2,h,rev_c,eff,rev_t,t_aux,ftpw
	real*4   p_bar,t_k,z,pwc
	real*4   h_km,qm_dim,ro_dim
c
C------------------------------------------------------------------
      ind = 1
      QQ2 = Q*Q
      IF ((H/QQ2).GE.CLIM(3)) THEN        !zona normale
        call ndahq_a(h,q,rev_c,CHN)
        call effdaqn_a(eff,q,rev_c,cen,cec,clim)
      ELSE
        IF (CLIM(3).NE.CLIM(4)) THEN      !zona di choking
          call ndahq_a(h,q,rev_c,CHC)
          call effdaqn_a(eff,q,rev_c,cen,cec,clim)
        ELSE
C---->  This case is necessary in order to prevent errors due the machine
c       precision on the evaluation of the boundaries of the research.
          call ndahq_a(h,q,rev_c,CHC)
          call effdaqn_a(eff,q,rev_c,cen,cec,clim)
        ENDIF
      ENDIF
	p_bar = pin*(PRIF+P0)  !bar
	t_k   = tin*(TRIF+T0)  !Kelvin
      call zpt1(ind,p_bar,t_k,z)
c potenza richiesta dal compressore
c
c      eff  = eff*eff_corr
c-elena-5/12/2005

c NOTA: in input viene letta la variazione di efficienza (eff_corr), nelle simulazioni e'
c       utilizzata moltiplicando eff = eff * eff_corr

      eff  = eff*(1.+eff_corr)
c-elena-5/12/2005

cgpe
      if (eff .lt. 0.01) eff = 0.01
cgpe-end
	h_km = h*HEADRIF
	qm_dim = q*QMRIF
      ro_dim = p_bar*av_weight(ind)*PMKS/(r_mks*z*t_k*3600)
	pwc = agrvcc*h_km*qm_dim*ro_dim/eff
	pwc = pwc/POWERIF
c
      rev_t = rev_c
      T_AUX=TAIR
      FTPW=CPWT(1)+T_AUX*(CPWT(2)+CPWT(3)*T_AUX+CPWT(4)*T_AUX**2)
      PWT = (CPW(1)+CPW(2)*REV_T+CPW(3)*
     *            REV_T*REV_T)*FTPW*POW_CORR
      FUN2 = PWT - PWC
      RETURN
      END
c
	SUBROUTINE TRANS(POUT,TIN,H,QM,P,Q,NP)
c***********************************************************************
c	trasforma NP punti del grafico (QM,H) nei corrispondenti
c	punti del grafico (Q,PIN), a pressione di uscita e temperatura
c     di ingresso assegnate.	
c------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
c
c
      real*4    pout,tin              !adimensionali
      real*4    H(*),QM(*),P(*),Q(*)  !dimensionali su grafico

	REAL*4    eps,base,pmin,plim,err,f1,f2,ro_dim,
     *          p3,pin,z,qqm,qq,t_k
      integer*4 i,np,ind
c
      real*4 hh,ppout,ttin
	COMMON /AUXTR/  hh,ppout,ttin
	real*4   FUNTR
	EXTERNAL FUNTR
c---------------------------------------------------------------------
      ind = 1
	ppout=pout
	ttin=tin
      call zerom(eps,base)
	do i = 1,np
		pmin = .333*pout
		plim = .999*pout
		err  = pout*1.E-4
		hh  = h(i)/HEADRIF
          f1 = funtr(pmin)
          f2 = funtr(plim)
          p3 = pmin
          call zero_fun(pmin,plim,p3,f1,f2,pin,err,eps,funtr)
		p(i)  =  pin*(PRIF+P0)-P0  !bar
	    t_k   =  tin*(TRIF+T0)     !Kelvin
  	    call zpt1(ind,p(i),t_k,z)
          ro_dim = p(i)*av_weight(ind)*PMKS/(r_mks*z*t_k)
		q(i)   = qm(i)*ro_dim  !in kg/h
	end do
	return
	end
c---------------------------------------------------------------
	REAL*4 FUNCTION  FUNTR(pin)
c***********************************************************************
c	funzione da azzerare per il calcolo della pressione
c	di ingresso a fissata pressione di uscita e altezza adiabatica
c---------------------------------------------------------------------
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/cc.inc'		!:IN
c
      real*4 hh,ppout,ttin
	COMMON /AUXTR/  hh,ppout,ttin
      integer*4 ind
	real*4 ex,pin,p_bar,t_k,z, aux,h
c
	real*4 prop_a
	EXTERNAL prop_a

CMAR
      real*4 comp_ratio
	integer*4 ier
c-------------------------------------------------------------------
c---->
c     calcolo esponente della politropica
      ind = 1
	aux = r_mks/AGRVcc/av_weight(ind)
c     pin e ttin adimensionali      

CMAR	EX = PROP_a(pin,ttin,exp_coef(1,ind))
	p_bar = pin*(PRIF+P0)  !bar
	t_k   = ttin*(TRIF+T0)  !Kelvin

CMAR  calcolare l'esp_politrop con i valori
CMAR  di pressione e temperatura dimensionali

      comp_ratio=(ppout/pin)
      call politrop_esp(ind,p_bar,t_k,comp_ratio,ex,ier)


      call zpt1(ind,p_bar,t_k,z)
	ex=(ex-1.)/ex
	h=z*t_k*aux*((ppout/pin)**ex-1.)/ex   !Km 
	h= h/HEADRIF 
c                           calcolo esponente della politropica
	FUNTR = hh - h
	return
	end
C-------
      subroutine tronco(npt,nt,pt,q,h,npoint,qt,ht)
c*******************************************************************************
c la curva di massima potenza di una turbina sulla mappa del compressore ad essa
c accoppiato è suddivisa in due rami e ogni ramo a sua volta in più tronchi che 
c vanno graficati singolarmente.Questa subroutine crea partendo dai vettori q e h 
c contenenti i punti relativi ad un ramo intero i vettori qt e ht contenenti i punti
c relativi ai singoli rami
c*******************************************************************************
      implicit none
c
      integer*4 max_nt      !massimo numero di tronchi calcolabili
	parameter (max_nt=100)
c------>INPUT
	integer*4 npt   !numero punti sul ramo analizzato
	integer*4 nt   !numero tronchi sul ramo analizzato
	integer*4 pt(*)  !vettore punti calcolati sui vari tronchi
	real*4 q(*),   !ascisse punti sul ramo analizzato
     *       h(*)	   !ordinate punti sul ramo analizzato
c------>OUTPUT
      integer*4 npoint(*) !numero di punti sul singolo tronco (dim=nt)
      real*4 qt(max_nt,*),!ascisse punti sul singolo tronco
     *       ht(max_nt,*)	!ordinate punti sul singolo tronco 
c    
c------>variabili locali
      integer*4 it,ifirst,j,k

c*******************************************************************************
      k=0
      do it=1,nt
	   if (it.eq.1) then
	      ifirst=1
	   else
            ifirst=pt(it-1)+1
	   end if
         npoint(it)=pt(it)-ifirst+1
	   if (npoint(it).ge.2) then
	      do j=1,npoint(it)
	        qt(it,j)=q(ifirst+j-1)
	        ht(it,j)=h(ifirst+j-1)
		  end do
	   end if
         k=k+npoint(it)
	   if(k.eq.npt) then
	      nt=it
	      return
	   end if
	end do
c
      return
	end

C-----------------------------------------------------------------------------
c--------------------------------------------------------------------------------
      subroutine design_UNICA(inpw,max_rev,min_rev,chn,chc,cen,cec,
     *                  nlim,alim,blim,lim_n,lim_q,lim_h,
     *                  clim,tin,pres,FUN,
     *                  flag,flag_out,na,nb,qa,ha,qb,hb,nta,ntb,pta,ptb,
     *                  ptr,un_q,un_h)
c***************************************************************************
c      Restituisce il vettore coi punti da usare per la graficazione della 
c      curva di massima potenza della turbina sulla mappa del compressore 
c      nella zona normale
c***************************************************************************
      implicit none
      include '../inc/param.inc'	!:IN
      include '../inc/rk.inc'		!:IN
      include '../inc/cc.inc'	    !:IN accelerazione di gravità
      integer*4  np,maxsud,nsud
	parameter (np=100,maxsud=5,nsud=2)
c
      real*4 prop_a		!calcola l'esponente della politropica
	external prop_a
      real*4   fun    !function da azzerare
	external fun
c---------------->INPUT
      logical*2 inpw !indica la variabile fissata in input:
	             !.TRUE. = pressione d'ingresso (azzero FUN2(H))
	             !         (se var_as=1)
	             !.FALSE.= pressione d'uscita (azzero FUN(PIN))
	             !         (se var_as=0)
c
      real*4 max_rev,   !max numero di giri compressore
     *	   min_rev,   !min numero di giri compressore
     *	   chn(6),    
     *       chc(6),    !coeff curve caratteristiche
     *       cen(5),    !                compressore
     *       cec(6),
     *	   alim(*),
     *	   blim(*), 
     *	   clim(4),
     *	   lim_n(*),
     *	   lim_q(*),
     *	   lim_h(*)   
c
      integer*4  nlim
c
      real*4 tin,     !temperatura d'ingresso adimensionale
     *       pres     !variabile assegnata in input adimensionale(PIN o POUT)
c
c-------------->OUTPUT
c
      logical*2 flag    !.TRUE. ci sono abb pti per disegnare la curva
	                  !.FALSE. non ci sono abb p.ti per disegnare la curva 
      integer*4 na,      !numero di punti sul ramo A
     *          nb       !numero di punti sul ramo B

      real*4 qa(*),ha(*), !coordinate HQ per disegnare il ramo A
	                    !(vettori di dim=na) dimensionali
     *       qb(*),hb(*)  !coordinate HQ per disegnare il ramo B 
	                    !(vettori di dim=nb) dimensionali
c
      integer*4 nta,      !numero di tronchi sul ramo A
     *          ntb       !numero di tronchi sul ramo B
c
      integer*4 pta(*),      !numero di punti su ogni tronco del ramo A
     *       ptb(*)       !numero di punti su ogni tronco del ramo B
c
      integer*4 ptr      !numero di punti nel vettore dei raccordi 
c
      real*4 un_h(*),un_q(*)  !coordinate dei pti di raccordo 
                              !(dim=ptr) dimensionali
c---------------->varialbili locali
      real*4 qlim(6),hlim(6),
     *	   qqmin,qqmed1,qqmed4,qqmed2,qqmed6,qqmax,dq,
     *       err,eps,base,
     *       pin,pout,
     *       qi,qq,hup,hdown,yup,ydown,fa,fb,
     *       fl,dely,y,y1,y2,y3,f1,f2,yroot(2),
     *       z,ex,
     *       hbuf(2),
     *       unve(2),unor(2),
     *       auxq(np),auxh(np),yappo
     *       ,hhmin,hhmax,dh,h1,h2,ka,kb,ymin
c- ace
      real*4 p_bar,t_k,aux
c
      integer*4 ind
c
	integer*4 smooth, !indica il tronco in costruzione:
	                  !(0=nessun tronco,1=solo tronco in ha,
	                  ! 2=solo tronco in hb,3=tronco in ha e hb)
     *	      i,irange,itemp,nroot,
     *          nint,kk,j,it,ifirst,npoint,jp
	logical*2 unides,!indica se serve il raccordo a dx delle due semicurve
     *          unisin !indica se serve il raccordo a sx delle due semicurve
     
	common/pwaux/qq   !riempie l'area common per la FUN

	logical*2 flag_out

CMAR
      real*4 pout_d, comp_ratio_d
	integer*4 ier 
c****************************************************************************
c
      flag_out=.false.
      ind=1
      call zerom(eps,base)    !calcola la precisione di macchina
	                        !(aux1.for)
c
c   calcolo degli estremi di variabilità di portata e altezza adiabatica
c
      call limit_abs_ec_a (min_rev,max_rev,chn,chc,clim,nlim,
     *                  	lim_q,lim_h,qlim,hlim)  !qlim/hlim adimensionali
c  
c     dimensionalizzazione punti di intersezine tra curve limite

      qqmin  = qlim(3)*qmrif  !antisurge-giri min
      qqmed1 = qlim(1)*qmrif  !antisurge-giri max
      qqmed4 = qlim(4)*qmrif  !choking rel-giri min
      qqmed2 = qlim(2)*qmrif  !choking rel-giri max
      qqmed6 = qlim(6)*qmrif  !choking ass-giri min
      qqmax  = qlim(5)*qmrif  !choking ass-giri max

c
      hhmin =hlim(6)*headrif
      hhmax =hlim(1)*headrif

      dq=(qqmax-qqmin)/(np-1)
	dh=(hhmax-hhmin)/(np-1)
c 
      err=pres*1.E-4
	aux = r_mks/AGRVcc/av_weight(ind)
c
      if (inpw) then        !valorizza la variabile assegnata in input
	   pin=pres
	else
	   pout=pres
	end if

c---------------RICERCA DEI PUNTI DELLA CURVA DA TRACCIARE----------------
                               !inizializzazione variabili 
      na=0                     !p.ti effettivi ramo in ha 
	nb=0                     !p.ti effettivi ramo in hb
	nta=0                    !numero tronchi in ha
	ntb=0                    !numero tronchi in hb
	smooth=0                 !rami in costruzione: smooth=0 nessun tronco
	                         !                     smooth=1 1 tronco ha
	                         !                     smooth=2 1 tronco hb
	                         !                     smooth=3 2 tronchi ha e hb
	unides=.false.           !unione a dx dei due rami
	unisin=.false.           !unione a sx dei due rami
      ptr=0                    !p.ti nel vettore dei raccordi

      do i=1,np
c        calcolo degli estremi per la ricerca degli zeri
         qi=qqmin+(i-1)*dq        ! dimensionale     
	   qq=qi/QMRIF              ! adimensionale
	   
C------> Calcolo HUP
	   if (qi.le.qqmed1) then   !spezzata di surge
            call hdaq_su_a(nlim,alim,blim,qq,lim_q,HUP,irange)
         else                     !curva caratteristica max_rev
	      if (qi.le.qqmed2) then                
               call hdaqn_a(HUP,qq,max_rev,chn)
	      else
               call hdaqn_a(HUP,qq,max_rev,chc)	      
		  end if
	   end if

C------> Calcolo HDOWN
	   if (qi.le.qqmed6) then
	      if (qi.le.qqmed4) then
              call hdaqn_a(HDOWN,qq,min_rev,chn)
            else
              call hdaqn_a(HDOWN,qq,min_rev,chc)
		  end if
	   else
            call hdaq_ch_a(clim(4),HDOWN,qq)
	   end if
c
         if (inpw) then        !se assegno PIN azzero la funzione in H
	      yup=hup            !FUN2
	      ydown=hdown
	      ymin=hlim(6)
            if (yup.lt.ydown) then
	          yappo=ydown
	          ydown=yup
	          yup=yappo
		  end if 

	   else                  !se assegno POUT azzero la funzione in PIN
	      hup=hup*HEADRIF    !FUN
	      hdown=hdown*HEADRIF
	      call trans(pout,tin,hup,qi,yup,fl,1)   !(subunir.for)
	      call trans(pout,tin,hdown,qi,ydown,fl,1)
	      yup = yup/(PRIF+P0)
	      ydown = ydown/(PRIF+P0)
c-prova
	      call trans(pout,tin,hhmin,qi,ymin,fl,1)
	      ymin = ymin/(PRIF+P0)
            
c-elena
            if (yup.lt.ydown) then
	          yappo=ydown
	          ydown=yup
	          yup=yappo
		  end if 
c-elena
	   end if
         fa=FUN(ydown)
	   fb=FUN(yup)
	   if ((fa*fb).gt.0) then
	      if (fa.lt.0) then    !ricerca di due zeri
	         do itemp=1,maxsud
	            nroot=0
	            nint=nsud**itemp
	            dely=(yup-ydown)/nint
	            do kk=1,nint
	               y1=ydown+(kk-1)*dely
	               y2=ydown+kk*dely
	               f1=FUN(y1)
	               f2=FUN(y2)
	               if ((f1*f2).lt.0) then
	                  y3=y1
	                  call zero_fun(y1,y2,y3,f1,f2,y,err,eps,FUN)
	                  nroot=nroot+1
	                  yroot(nroot)=y
	               end if
	            end do
	            if (nroot.eq.2) go to 110
			 end do
	         go to 300
110            unides=.true.
	   
	                      !valori di h corrispondenti agli zeri trovati
	   
	         if (.not.inpw) then
c- ace !yroot è una pressione adimensionale
	            do j=1,2           
				   p_bar = yroot(j)*(PRIF+P0)  !bar
	               t_k   = tin*(TRIF+T0)       !Kelvin
                     call zpt1(ind,p_bar,t_k,z)

CMAR   provo il calcolo con i valori di p e t
CMAR   nel calcolo dell' esp_politrop dimensionali

               pout_d=pout*(PRIF+P0)         !bar
	         comp_ratio_d=(pout_d/p_bar)

      call politrop_esp(ind,p_bar,t_k,comp_ratio_d,ex,ier)

CMAR				   ex=prop_a(yroot(j),tin,exp_coef(1,ind))
				   ex=(ex-1.)/ex
c- ace               hbuf(j) in Km
				   hbuf(j)=z*t_k*aux*((pout/yroot(j))**ex-1.)/ex  
  				end do
	         else
	            hbuf(1)=yroot(1)*HEADRIF
	            hbuf(2)=yroot(2)*HEADRIF
			 end if
	         if ((na+nb).eq.0) unisin=.true.   !1°loop
	         if (smooth.eq.0) then            !1°loop
	                          !non ci sono tronchi di curva in costruzione
	            na=na+1       !punti effettivi sul primo tronco
	            nb=nb+1       !punti effettivi sul 2° tronco
	            ha(na)=hbuf(1)
	            hb(nb)=hbuf(2)
	            qa(na)=qi
	            qb(nb)=qi
	            smooth=3
	         else if (smooth.eq.3) then
	                         !ci sono due tronchi di curva in costruzione
                  if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
				end if
	         else if (smooth.eq.1) then
	                         !c'è un solo tronco di curva in costruzione in HA
	            smooth=3
                  if(abs(ha(na)-hbuf(1)).gt.abs(ha(na)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
	         else if (smooth.eq.2) then
	                         !c'è un solo tronco di curva in costruzione in HB
	            smooth=3
                  if(abs(hb(nb)-hbuf(1)).gt.abs(hb(nb)-hbuf(2)))then
  	               na=na+1
                     nb=nb+1
                     ha(na)=hbuf(2)
	               hb(nb)=hbuf(1)
	               qa(na)=qi
	               qb(nb)=qi
	            else
                     na=na+1
                     nb=nb+1
                     ha(na)=hbuf(1)
	               hb(nb)=hbuf(2)
	               qa(na)=qi
	               qb(nb)=qi
                  end if
               end if
            else
	         go to 300

	      end if
	   else               !ricerca di un solo zero(fa e fb di segno discorde)
	      y3=ydown
	      call zero_fun(ydown,yup,y3,fa,fb,y,err,eps,FUN)
	      unides=.false.
	      if (.not.inpw) then
	         p_bar = y*(PRIF+P0)         !bar
	         t_k   = tin*(TRIF+T0)  !Kelvin
               call zpt1(ind,p_bar,t_k,z)
CMAR   provo il calcolo con i valori di p e t
CMAR   nel calcolo dell' esp_politrop dimensionali

               pout_d=pout*(PRIF+P0)         !bar
	         comp_ratio_d=(pout_d/p_bar)

      call politrop_esp(ind,p_bar,t_k,comp_ratio_d,ex,ier)
CMAR			 ex=prop_a(y,tin,exp_coef(1,ind))
 		     ex=(ex-1.)/ex
c- ace         hbuf(1) in Km
		     hbuf(1)=z*t_k*aux*((pout/y)**ex-1.)/ex
	      else
	          hbuf(1)=y*HEADRIF
		  end if
            if ((smooth.eq.0).or.(smooth.eq.1)) then
	     !non ci sono tronchi di curva in costruzione o un unico in HA
	          smooth=1
	          na=na+1
                ha(na)=hbuf(1)
	          qa(na)=qi
	      else if (smooth.eq.2) then
	                         !unico tronco in costruzione in HB
	          nb=nb+1
                hb(nb)=hbuf(1)
	          qb(nb)=qi
	      else if (smooth.eq.3) then
	               !due tronchi in costruzione dei quali uno appena estinto
                if (abs(ha(na)-hbuf(1)).gt.abs(hb(nb)-hbuf(1))) then
	             smooth=2
	             nb=nb+1
                   hb(nb)=hbuf(1)
	             qb(nb)=qi
	             nta=nta+1          !il tronco in HA si è estinto
	             pta(nta)=na
	          else
	             smooth=1
	             na=na+1
                   ha(na)=hbuf(1)
	             qa(na)=qi
	             ntb=ntb+1          !il tronco in HB si è estinto
	             ptb(ntb)=nb
			  end if
		  end if
	   end if
	   go to 400
300      call zero_outrange(ymin,ydown,fun,flag_out)
         if ((smooth.eq.1).or.(smooth.eq.3)) then
            nta=nta+1
	      pta(nta)=na
         end if
         if ((smooth.eq.2).or.(smooth.eq.3)) then
            ntb=ntb+1
            ptb(ntb)=nb
         end if
400      continue
	end do  
c         verifica se sono stati trovati abbastanza punti per disegnare la curva
	if ((na+nb).le.2) then
	   flag=.false.                !non ci sono punti sufficienti
         return                      !esco dalla routine
	end if    
      flag=.true.
c
c        verifica se le due semicurve vanno raccordate a destra o a sinistra
c                                    raccordo a sinistra
      if (unisin) then
	   if ((na.ge.2).and.(nb.ge.2)) then
	      if (abs(ha(1)-hb(1)).gt.abs(ha(2)-hb(2))) unisin=.false.
	   end if
	end if
c                                      raccordo a destra           
      if (unides) then
	  if ((na.ge.2).and.(nb.ge.2)) then
	   if (abs(ha(na)-hb(nb)).gt.abs(ha(na-1)-hb(nb-1))) unides=.false.
	  end if
	end if
c
      if (unisin) then
c                           raccordo a sinistra delle due semicurve
          UNVE(1)=HA(1)
          UNVE(2)=HB(1)
          UNOR(1)=QA(1)
          UNOR(2)=QB(1)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
c
         if (pta(1).eq.1) then
c          !prolungamento del primo tronco di ha fin su una curva limite
           unor(1)=qb(1)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(1)/headrif
	     unve(2)=ha(1)/headrif
c-prova 
		call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)

	                     !descompr.for
	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(1).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(1)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(1)/headrif
	     unve(2)=hb(1)/headrif

	     call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)


	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
      end if
c
c
      if (unides) then
c                           raccordo a destra delle due semicurve
         unve(1)=ha(na)
         unve(2)=hb(nb)
	   unor(1)=qa(na)
	   unor(2)=qb(nb)
c
         un_h(ptr+1)=unve(1)
         un_h(ptr+2)=unve(2)
	   un_q(ptr+1)=unor(1)
	   un_q(ptr+2)=unor(2)
         ptr=ptr+2
         if (pta(nta).eq.1) then
c          !prolungamento dell'ultimo tronco di ha fin su una curva limite
           unor(1)=qb(nb)/qmrif
           unor(2)=qa(1)/qmrif
	     unve(1)=hb(nb)/headrif
	     unve(2)=ha(1)/headrif

	     call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *          lim_q,lim_h,clim,max_rev,min_rev)

		 
		 do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
          un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
         end if	
         if (ptb(ntb).eq.1) then
c          !prolungamento del primo tronco di hb fin su una curva limite
           unor(1)=qa(na)/qmrif
           unor(2)=qb(1)/qmrif
	     unve(1)=ha(na)/headrif
	     unve(2)=hb(1)/headrif

	      call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)


	     do j=1,2
	        unor(j)=unor(j)*qmrif
	        unve(j)=unve(j)*headrif
	     end do
c
           un_h(ptr+1)=unve(1)
           un_h(ptr+2)=unve(2)
	     un_q(ptr+1)=unor(1)
	     un_q(ptr+2)=unor(2)
           ptr=ptr+2
	   end if
      end if
c
      DO IT=1,NTA
C
C--->     GRAFICAZIONE DEI TRONCHI DI HA
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTA(IT-1)+1
          END IF
          NPOINT = PTA(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                AUXq(JP)=QA(IFIRST+JP-1)
                AUXh(JP)=HA(IFIRST+JP-1)
              end do
              IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif


			    call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)


                 DO J=1,2
                    UNOR(J) = UNOR(J)*qmrif
                    UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
              END IF
            IF((.NOT.UNIDES).OR.(IT.NE.NTA)) THEN
C
C--->            PROLUNGAMENTO A DESTRA DEL TRONCO DI HA FIN SU UNA CURVA LIMITE
C
                   UNOR(1) = AUXq(NPOINT-1)/qmrif
                   UNOR(2) = AUXq(NPOINT)/qmrif
                   UNVE(1) = AUXh(NPOINT-1)/headrif
                   UNVE(2) = AUXh(NPOINT)/headrif

			     call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)

                   DO J=1,2
                      UNOR(J) = UNOR(J)*qmrif
                      UNVE(J) = UNVE(J)*headrif
                   end do
c
                   un_h(ptr+1)=unve(1)
                   un_h(ptr+2)=unve(2)
	             un_q(ptr+1)=unor(1)
	             un_q(ptr+2)=unor(2)
                   ptr=ptr+2
           END IF

	   end if
	end do
C--->
      DO IT=1,NTB
C
C--->     GRAFICAZIONE DEI TRONCHI DI HB
C
          IF(IT.EQ.1) THEN
            IFIRST=1
          ELSE
            IFIRST=PTB(IT-1)+1
          END IF
          NPOINT = PTB(IT)-IFIRST+1
          IF(NPOINT.GE.2) THEN
              DO JP=1,NPOINT
                 AUXq(JP)=QB(IFIRST+JP-1)
                 AUXh(JP)=HB(IFIRST+JP-1)
              end do
            IF((.NOT.UNISIN).OR.(IT.NE.1)) THEN
C
C--->            PROLUNGAMENTO A SINISTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMI

C
                  UNOR(1) = AUXq(2)/qmrif
                  UNOR(2) = AUXq(1)/qmrif
                  UNVE(1) = AUXh(2)/headrif
                  UNVE(2) = AUXh(1)/headrif

			    call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *        		  lim_q,lim_h,clim,max_rev,min_rev)


                 DO J=1,2
                        UNOR(J) = UNOR(J)*qmrif
                        UNVE(J) = UNVE(J)*headrif
                  end do
                  un_h(ptr+1)=unve(1)
                  un_h(ptr+2)=unve(2)
	            un_q(ptr+1)=unor(1)
	            un_q(ptr+2)=unor(2)
                  ptr=ptr+2
            END IF
          IF((.NOT.UNIDES).OR.(IT.NE.NTB)) THEN
C---->        PROLUNGAMENTO A DESTRA DEL TRONCO DI HB FIN SU UNA CURVA LIMITE
            UNOR(1) = AUXq(NPOINT-1)/qmrif
            UNOR(2) = AUXq(NPOINT)/qmrif
            UNVE(1) = AUXh(NPOINT-1)/headrif
            UNVE(2) = AUXh(NPOINT)/headrif

	      call section_unica (unor,unve,chn,chc,nlim,alim,blim,
     *           lim_q,lim_h,clim,max_rev,min_rev)

            DO J=1,2
              UNOR(J) = UNOR(J)*qmrif
              UNVE(J) = UNVE(J)*headrif
            end do
                 un_h(ptr+1)=unve(1)
                 un_h(ptr+2)=unve(2)
	           un_q(ptr+1)=unor(1)
	           un_q(ptr+2)=unor(2)
                 ptr=ptr+2
          END IF
	  end if
      end do
	return
	end  
	
C=======================================================

      subroutine zero_outrange(ydown,yup,fun,flag)

      implicit none
    
      real*4 yup,ydown,yappo

      integer*4  maxsud,nsud
	parameter (maxsud=5,nsud=2)
      real*4   fun    !function da azzerare
	external fun
	
	real*4 fa,fb,dely,y1,y2,y3,f1,f2,y,err,eps,base
	integer*4 itemp,nint,kk
	logical*2 flag

c----------------------------------------------------------------

c      flag=.false.
      call zerom(eps,base)  !calcola la precisione di macchina

      if (yup.lt.ydown) then
	    yappo=ydown
	    ydown=yup
	    yup=yappo
      end if 

      fa=FUN(ydown)
	fb=FUN(yup)
	if ((fa*fb).gt.0) then
	   if (fa.lt.0) then    !ricerca di due zeri
	      do itemp=1,maxsud
	         nint=nsud**itemp
	         dely=(yup-ydown)/nint
	         do kk=1,nint
	            y1=ydown+(kk-1)*dely
	            y2=ydown+kk*dely
	            f1=FUN(y1)
	            f2=FUN(y2)
	            if ((f1*f2).lt.0) then
	               y3=y1
	               call zero_fun(y1,y2,y3,f1,f2,y,err,eps,FUN)
	               flag=.true.
	            end if
	         end do
		   end do
	   end if
	else               !esiste uno zero(fa e fb di segno discorde)
	   flag=.true.
	end if

      return
	end
