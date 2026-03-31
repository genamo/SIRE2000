C---------------------------------------------------------------------
      SUBROUTINE SECTION (Q,H,CHN,CHC,NLIM,ALIM,BLIM,LIM_Q,
     *                    LIM_H,CLIM,MAX_REV,MIN_REV)
c*******************************************************************
c      calcola le intersezioni di una retta con le curve limite
c      di un compressore (limitato da una spezzata e da una 
c      parabola h/q2 = k (rappresentata dalla clim(3)):zona normale
c------------------------------------------------------------------
      implicit none
      REAL*4 Q(2),H(2),CHC(6),CHN(6),ALIM(*),BLIM(*),CLIM(4),
     *       LIM_Q(*),LIM_H(*)
      REAL*4 MAX_REV,MIN_REV,M,N,QVAL(2),HVAL(2)
      LOGICAL*2 OK1,OK2
      DATA EPS/1.E-5/
      real*4 qd,hd,qa,ha,qb,hb,qc,hc,h1,h2,h3,h4,a,
     *       b,c,delta,qroot1,qroot2,hroot1,hroot2,
     *      aux,dist1,dist2,eps
      integer*4  nlim,nval,irange,i,kk
      real*4     qlim(6),hlim(6),qroot(20)
                                   
c-----------------------------------------------------------------------
C
      call limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,lim_q,lim_h,
     *                  qlim,hlim)
c
C---->  CALCOLO DEI PUNTI INTERSEZIONE DELLE CURVE LIMITE
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA ANTI_SOURGE E LA CURVA A
C       NUMERO MINIMO DI GIRI
      QD    = qlim(3)
      HD    = hlim(3)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA ANTI_SOURGE E LA CURVA A
C      NUMERO MASSIMO DI GIRI
      QA    = qlim(1)
      HA    = hlim(1)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA ANTI_CHOKE E LA CURVA A
C      NUMERO MINIMO DI GIRI
      QC = qlim(4)
	HC = hlim(4)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA ANTI_CHOKE E LA CURVA A
C      NUMERO MASSIMO DI GIRI
      QB = qlim(2)
	HB = hlim(2)
C
      NVAL = 0
C---->  CALCOLO DEL COEFFICIENTE ANGOLARE DELLA RETTA E TERMINE NOTO
      IF(Q(1).EQ.Q(2)) THEN
C---->     RETTA VERTICALE
        CALL hdaq_su_a(NLIM,ALIM,BLIM,Q(1),LIM_Q,H1,IRANGE)
        IF((H1.GE.HD).AND.(H1.LE.HA)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H1
          QVAL(NVAL)=Q(1)
        END IF
        call hdaq_ch_a(clim(3),h2,q(1))
        IF((H2.GE.HC).AND.(H2.LE.HB)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H2
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
        call HDAQN_a(H3,q(1),MAX_REV,CHN)
        IF((H3.GE.HB).AND.(H3.LE.HA)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H3
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
        call HDAQN_a(H4,q(1),MIN_REV,CHN)
        IF((H4.GE.HC).AND.(H4.LE.HD)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H4
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
      Else
c---->           retta a fissata pendenza
        M = (H(2)-H(1))/(Q(2)-Q(1))
        N =  H(1)-M*Q(1)
        do i=1,4
          if(i.eq.1) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MASSIMO DI GIRI (NORMALE)
            A = CHN(5)
            B = CHN(2)+CHN(4)*MAX_REV-M
            C = CHN(1)+CHN(3)*MAX_REV+CHN(6)*MAX_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.2) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MINIMO DI GIRI (NORMALE)
            A = CHN(5)
            B = CHN(2)+CHN(4)*MIN_REV-M
            C = CHN(1)+CHN(3)*MIN_REV+CHN(6)*MIN_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.3) then
C---->  INTERSEZIONE CON LA CURVA LIMITE 1
            DELTA = 1
          else if(i.eq.4) then
C---->  INTERSEZIONE CON LA CURVA LIMITE 2
            A = clim(3)
            B = -M
            C = -N
            DELTA =B**2-4.*A*C
          end if
          IF(DELTA.GE.0.) THEN
            if(i.eq.1) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              call hdaqn_a(hroot1,qroot1,max_rev,CHN)
	        call hdaqn_a(hroot2,qroot2,max_rev,CHN)
              OK1=((QROOT1.GE.QA).AND.(QROOT1.LE.QB).AND.(HROOT1.LE.HA)
     *                          .AND.(HROOT1.GE.HB))
              OK2=((QROOT2.GE.QA).AND.(QROOT2.LE.QB).AND.(HROOT2.LE.HA)
     *                          .AND.(HROOT2.GE.HB))
            else if(i.eq.2) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              call hdaqn_a(hroot1,qroot1,min_rev,CHN)
	        call hdaqn_a(hroot2,qroot2,min_rev,CHN)
              OK1=((QROOT1.GE.QD).AND.(QROOT1.LE.QC).AND.(HROOT1.LE.HD)
     *                          .AND.(HROOT1.GE.HC))
              OK2=((QROOT2.GE.QD).AND.(QROOT2.LE.QC).AND.(HROOT2.LE.HD)
     *                          .AND.(HROOT2.GE.HC))
            else if(i.eq.3) then
C INTERSEZIONE CON LE SPEZZATE DI ANTI-SURGE
              DO kk=1,NLIM+1
                QROOT(kk)=(N-ALIM(kk))/(BLIM(kk)-M)
              ENDDO
c devo trovare lo head che sia compreso nel range relativo alla portata
c per la quale viene calcolato
              DO kk=1,NLIM
                CALL RSURGE_a(NLIM,qroot(kk),LIM_Q,IRANGE)
                if (kk.eq.irange) then
                  hroot1 = ALIM(IRANGE)+BLIM(IRANGE)*QROOT(kk)
                  qroot1=qroot(kk)
                  goto 777
                endif               
              ENDDO
777           continue
              OK1=((QROOT1.GE.QD).AND.(QROOT1.LE.QA).AND.(HROOT1.GE.HD)
     *                          .AND.(HROOT1.LE.HA))
              OK2=.false.
            else if(i.eq.4) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              HROOT1 = clim(3)*QROOT1**2
              HROOT2 = clim(3)*QROOT2**2
              OK1=((QROOT1.GE.QC).AND.(QROOT1.LE.QB).AND.(HROOT1.GE.HC)
     *                          .AND.(HROOT1.LE.HB))
              OK2=((QROOT2.GE.QC).AND.(QROOT2.LE.QB).AND.(HROOT2.GE.HC)
     *                          .AND.(HROOT2.LE.HB))
            end if
            IF(OK1.AND.(.NOT.OK2)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT1
              HVAL(NVAL) = HROOT1
            ELSE IF (OK2.AND.(.NOT.OK1)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT2
              HVAL(NVAL) = HROOT2
            ELSE IF (OK1.AND.OK2) THEN
              AUX = (QROOT1-Q(2))**2+(HROOT1-H(2))**2
              DIST1 = SQRT(AUX)
              AUX = (QROOT2-Q(2))**2+(HROOT2-H(2))**2
              DIST2 = SQRT(AUX)
              IF(DIST1.LT.DIST2) THEN
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT1
                HVAL(NVAL) = HROOT1
              ELSE
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT2
                HVAL(NVAL) = HROOT2
              END IF
            END IF
          END IF
          if(nval.eq.2) goto 100
        end do
      end if
c---->           punti trovati
100   IF(NVAL.EQ.1) THEN
        qval(2) = qval(1)
        hval(2) = hval(1)
      ENDIF
      IF(Q(1).NE.Q(2)) THEN
        if(abs(q(1)-qval(1)).le.eps) then
          q(2) = qval(1)
          h(2) = hval(1)
        else if(abs(q(1)-qval(2)).le.eps) then
          q(2) = qval(2)
          h(2) = hval(2)
        else if(abs(q(2)-qval(1)).le.eps) then
          q(1) = qval(1)
          h(1) = hval(1)
        else if(abs(q(2)-qval(2)).le.eps) then
          q(1) = qval(2)
          h(1) = hval(2)
        else IF(((QVAL(1)-Q(2))*(Q(2)-Q(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      ELSE
        IF(((HVAL(1)-H(2))*(H(2)-H(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      END IF
      RETURN
      END

C---------------------------------------------------------------------
      SUBROUTINE SECTION_ch (Q,H,CHN,CHC,NLIM,ALIM,BLIM,LIM_Q,
     *                    LIM_H,CLIM,MAX_REV,MIN_REV)
c*******************************************************************
c      calcola le intersezioni di una retta con le curve limite
c      di un compressore (limitato dalla clim(3) e dalla clim(4))
c      zona di choking
c------------------------------------------------------------------
      implicit none
      REAL*4 Q(2),H(2),CHC(6),CHN(6),ALIM(*),BLIM(*),CLIM(4),
     *       LIM_Q(*),LIM_H(*)
      REAL*4 MAX_REV,MIN_REV,M,N,QVAL(2),HVAL(2)
      LOGICAL*2 OK1,OK2
      REAL*4    QROOT(20)
      DATA EPS/1.E-5/
      real*4  qd,hd,qa,ha,qb,hb,qc,hc,h1,h2,h3,h4,a,
     *        b,c,delta,qroot1,qroot2,hroot1,hroot2,
     *        aux,dist1,dist2,eps
      integer*4  nlim,nval,irange,i,kk
      real*4     qlim(6),hlim(6)    
c-----------------------------------------------------------------------
C
      call limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,lim_q,lim_h,
     *                  qlim,hlim)


C---->  CALCOLO DEI PUNTI INTERSEZIONE DELLE CURVE LIMITE
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA clim(3) E LA CURVA A
C      NUMERO MINIMO DI GIRI
      QD    = qlim(4)
      HD    = hlim(4)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA clim(3) E LA CURVA A
C      NUMERO MASSIMO DI GIRI
      QA    = qlim(2)
      HA    = hlim(2)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA clim(4) E LA CURVA A
C      NUMERO MINIMO DI GIRI
      QC = qlim(6)
	HC = hlim(6)
C
C---->  PUNTO DI INTERSEZIONE FRA LA CURVA clim(4) E LA CURVA A
C      NUMERO MASSIMO DI GIRI
      QB = qlim(5)
	HB = hlim(5)
C
      NVAL = 0
C---->  CALCOLO DEL COEFFICIENTE ANGOLARE DELLA RETTA E TERMINE NOTO
      IF(Q(1).EQ.Q(2)) THEN
C---->     RETTA VERTICALE
        call hdaq_ch_a(clim(3),h1,q(1))
        IF((H1.GE.HD).AND.(H1.LE.HA)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H1
          QVAL(NVAL)=Q(1)
        END IF
        call hdaq_ch_a(clim(4),h2,q(1))
        IF((H2.GE.HC).AND.(H2.LE.HB)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H2
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
        call HDAQN_a(H3,q(1),MAX_REV,CHC)
        IF((H3.GE.HB).AND.(H3.LE.HA)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H3
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
        call HDAQN_a(H4,q(1),MIN_REV,CHC)
        IF((H4.GE.HC).AND.(H4.LE.HD)) THEN
          NVAL = NVAL+1
          HVAL(NVAL)=H4
          QVAL(NVAL)=Q(1)
        END IF
        IF (NVAL.EQ.2) GO TO 100
      Else
c---->           retta a fissata pendenza
        M = (H(2)-H(1))/(Q(2)-Q(1))
        N =  H(1)-M*Q(1)
        do i=1,4
          if(i.eq.1) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MASSIMO DI GIRI (CHOKING)
            A = CHC(5)
            B = CHC(2)+CHC(4)*MAX_REV-M
            C = CHC(1)+CHC(3)*MAX_REV+CHC(6)*MAX_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.2) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MINIMO DI GIRI (NORMALE)
            A = CHC(5)
            B = CHC(2)+CHC(4)*MIN_REV-M
            C = CHC(1)+CHC(3)*MIN_REV+CHC(6)*MIN_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.3) then
C---->  INTERSEZIONE CON LA CURVA LIMITE 1
            A = clim(3)
            B = -M
            C = -N
            DELTA =B**2-4.*A*C
          else if(i.eq.4) then
C---->  INTERSEZIONE CON LA CURVA LIMITE 2
            A = clim(4)
            B = -M
            C = -N
            DELTA =B**2-4.*A*C
          end if
          IF(DELTA.GE.0.) THEN
            if(i.eq.1) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
	        call hdaqn_a(hroot1,qroot1,max_rev,CHC)
	        call hdaqn_a(hroot2,qroot2,max_rev,CHC)
              OK1=((QROOT1.GE.QA).AND.(QROOT1.LE.QB).AND.(HROOT1.LE.HA)
     *                         .AND.(HROOT1.GE.HB))
              OK2=((QROOT2.GE.QA).AND.(QROOT2.LE.QB).AND.(HROOT2.LE.HA)
     *                          .AND.(HROOT2.GE.HB))
            else if(i.eq.2) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
	        call hdaqn_a(hroot1,qroot1,min_rev,CHC)
	        call hdaqn_a(hroot2,qroot2,min_rev,CHC)
              OK1=((QROOT1.GE.QD).AND.(QROOT1.LE.QC).AND.(HROOT1.LE.HD)
     *                          .AND.(HROOT1.GE.HC))
              OK2=((QROOT2.GE.QD).AND.(QROOT2.LE.QC).AND.(HROOT2.LE.HD)
     *                          .AND.(HROOT2.GE.HC))
            else if(i.eq.3) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              HROOT1 = clim(3)*QROOT1**2
              HROOT2 = clim(3)*QROOT2**2
              OK1=((QROOT1.GE.QD).AND.(QROOT1.LE.QA).AND.(HROOT1.GE.HD)
     *                          .AND.(HROOT1.LE.HA))
              OK2=.false.
            else if(i.eq.4) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              HROOT1 = clim(4)*QROOT1**2
              HROOT2 = clim(4)*QROOT2**2
              OK1=((QROOT1.GE.QC).AND.(QROOT1.LE.QB).AND.(HROOT1.GE.HC)
     *                          .AND.(HROOT1.LE.HB))
              OK2=((QROOT2.GE.QC).AND.(QROOT2.LE.QB).AND.(HROOT2.GE.HC)
     *                          .AND.(HROOT2.LE.HB))
            end if
            IF(OK1.AND.(.NOT.OK2)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT1
              HVAL(NVAL) = HROOT1
            ELSE IF (OK2.AND.(.NOT.OK1)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT2
              HVAL(NVAL) = HROOT2
            ELSE IF (OK1.AND.OK2) THEN
              AUX = (QROOT1-Q(2))**2+(HROOT1-H(2))**2
              DIST1 = SQRT(AUX)
              AUX = (QROOT2-Q(2))**2+(HROOT2-H(2))**2
              DIST2 = SQRT(AUX)
              IF(DIST1.LT.DIST2) THEN
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT1
                HVAL(NVAL) = HROOT1
              ELSE
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT2
                HVAL(NVAL) = HROOT2
              END IF
            END IF
          END IF
          if(nval.eq.2) goto 100
        end do
      end if
c---->           punti trovati
100   IF(NVAL.EQ.1) THEN
        qval(2) = qval(1)
        hval(2) = hval(1)
      ENDIF
      IF(Q(1).NE.Q(2)) THEN
        if(abs(q(1)-qval(1)).le.eps) then
          q(2) = qval(1)
          h(2) = hval(1)
        else if(abs(q(1)-qval(2)).le.eps) then
          q(2) = qval(2)
          h(2) = hval(2)
        else if(abs(q(2)-qval(1)).le.eps) then
          q(1) = qval(1)
          h(1) = hval(1)
        else if(abs(q(2)-qval(2)).le.eps) then
          q(1) = qval(2)
          h(1) = hval(2)
        else IF(((QVAL(1)-Q(2))*(Q(2)-Q(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      ELSE
        IF(((HVAL(1)-H(2))*(H(2)-H(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      END IF
      RETURN
      END
c
C-------------------------------------------------------------------------
      SUBROUTINE SECTION_unica (Q,H,CHN,CHC,NLIM,ALIM,BLIM,LIM_Q,
     *                    LIM_H,CLIM,MAX_REV,MIN_REV)
c*************************************************************************
c    Calcola le intersezioni della retta individuata dalla coppia di punti
c    di coordinate (Q1,H1),(Q2,H2) con le curve limite di un compressore :
c    - spezzata di surge
c    - parabola di choking relativo (clim3)
c    - parabola di choking assoluto (clim4)
c    - curva caratteristica al minimo numero di giri
c    - curva caratteristica al massimo numero di giri
c------------------------------------------------------------------
      implicit none

c------->INPUT
      real*4 q(2),h(2),    !coordinate dei punti che individuano la retta 
     *       chn(6),chc(6),clim(4),    !coefficienti curve caratteristiche
     *       alim(*),blim(*),         !coefficienti spezzata di surge
     *       lim_q(*),lim_h(*),       !coordinate punti limite
     *       max_rev,min_rev          !giri max e min  
c------->OUTPUT
      real*4 qval(2),hval(2) !coordinate punti di intersezione  
c------->variabili locali    
      real*4 m,n
      logical*2   OK1,OK2,flag_min,flag_max
      data EPS/1.E-5/
      real*4 qa,ha,qb,hb,qc,hc,qd,hd,qe,he,qf,hf,
     *       h1,h2,h3,h4,a,b,c,delta,
     *       qroot1,qroot2,hroot1,hroot2,
     *      aux,dist1,dist2,eps
      integer*4  nlim,nval,irange,i,kk
      real*4     qlim(6),hlim(6),qroot(21)
                                   
c-----------------------------------------------------------------------
C
c-ele inizializzazione dei flag che individuano l'intersezione con la curva al
c     massimo numero di giri
      flag_max=.false.
	flag_min=.false.
c-ele
	ok1=.false.
	ok2=.false.
C     CALCOLO DEI PUNTI INTERSEZIONE DELLE CURVE LIMITE

      call limit_abs_ec_a(min_rev,max_rev,chn,chc,clim,nlim,lim_q,lim_h,
     *                  qlim,hlim)
c
c     Intersezione tra spezzata di surge e curva al minimo numero di giri
      qf    = qlim(3)
      hf    = hlim(3)
C
c     Intersezione tra spezzata di surge e curva al massimo numero di giri
      qa    = qlim(1)
      ha    = hlim(1)
C
c     Intersezione tra curva di choking relativo (clim3) e curva al minimo numero di giri
      qe = qlim(4)
	he = hlim(4)
C
c     Intersezione tra curva di choking relativo (clim3) e curva al massimo numero di giri
      qb = qlim(2)
	hb = hlim(2)
C
c     Intersezione tra curva di choking assoluto (clim4) e curva al minimo numero di giri
      qd = qlim(6)
	hd = hlim(6)
C
c     Intersezione tra curva di choking assoluto (clim4) e curva al massimo numero di giri
      qc = qlim(5)
	hc = hlim(5)
C
      nval = 0   !numero di radici trovate

C     CALCOLO DEL COEFFICIENTE ANGOLARE DELLA RETTA E TERMINE NOTO

      if (q(1).eq.q(2)) then  ! punti sulla stessa retta verticale
C                             ! Q=Q(1)
c    I punti (q(i),h(i))i=1,2 sono assegnati in modo che h(1)<h(2)
c     
c 	1-->intersezione con la spezzata di surge (q(1),h1)
c
		call hdaq_su_a(nlim,alim,blim,q(1),lim_q,h1,irange)
		IF((H1.GE.hf).AND.(H1.LE.HA)) THEN
			NVAL = NVAL+1
			HVAL(NVAL)=H1
			QVAL(NVAL)=Q(1)
		END IF
c
c    2-->intersezione con la curva di choking assoluto(clim4) (q(1),h2)

		call hdaq_ch_a(clim(4),h2,q(1))
	    IF((H2.GE.hf).AND.(H4.LE.he)) THEN
			NVAL = NVAL+1
			HVAL(NVAL)=H2
			QVAL(NVAL)=Q(1)
		END IF
		IF (NVAL.EQ.2) GO TO 100
c
c    3-->intersezione con la curva al massimo numero di giri (q(1),h3)
		if(q(1).le.qlim(2)) then
			call HDAQN_a(H3,q(1),MAX_REV,CHN)
	    else
			call HDAQN_a(H3,q(1),MAX_REV,chc)
		end if
		IF((H3.GE.hc).AND.(H3.LE.HA)) THEN
			NVAL = NVAL+1
			HVAL(NVAL)=H3
			QVAL(NVAL)=Q(1)
		END IF
		IF (NVAL.EQ.2) GO TO 100
c
c    4-->intersezione con la curva al minimo numero di giri (q(1),h4)

		if(q(1).le.qlim(4)) then
			call HDAQN_a(H4,q(1),MIN_REV,CHN)
	    else
			call HDAQN_a(H4,q(1),MIN_REV,chc)
		end if

		IF((H4.GE.hd).AND.(H4.LE.hf)) THEN
			NVAL = NVAL+1
			HVAL(NVAL)=H4
			QVAL(NVAL)=Q(1)
		END IF
		IF (NVAL.EQ.2) GO TO 100

      Else       !retta a fissata pendenza
           
        M = (H(2)-H(1))/(Q(2)-Q(1))   !coefficiente angolare
        N =  H(1)-M*Q(1)              !termine noto

        do i=1,6
          if(i.eq.1) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MASSIMO DI GIRI (NORMALE)
            A = CHN(5)
            B = CHN(2)+CHN(4)*MAX_REV-M
            C = CHN(1)+CHN(3)*MAX_REV+CHN(6)*MAX_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.2) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MINIMO DI GIRI (NORMALE)
            A = CHN(5)
            B = CHN(2)+CHN(4)*MIN_REV-M
            C = CHN(1)+CHN(3)*MIN_REV+CHN(6)*MIN_REV**2-N
            DELTA =B**2-4.*A*C
          else if(i.eq.3) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MASSIMO DI GIRI (CHOKING)
            if ((clim(3).ne.clim(4)).and.(.not.flag_max))then
			  A = CHC(5)
			  B = CHC(2)+CHC(4)*MAX_REV-M
			  C = CHC(1)+CHC(3)*MAX_REV+CHC(6)*MAX_REV**2-N
			  DELTA =B**2-4.*A*C
	      end if
          else if(i.eq.4) then
C---->  INTERSEZIONE CON LA CURVA A NUMERO MINIMO DI GIRI (choking)
            if ((clim(3).ne.clim(4)).and.(.not.flag_min))then
			  A = CHC(5)
			  B = CHC(2)+CHC(4)*MIN_REV-M
			  C = CHC(1)+CHC(3)*MIN_REV+CHC(6)*MIN_REV**2-N
			  DELTA =B**2-4.*A*C
	      end if
          else if(i.eq.5) then
C---->  INTERSEZIONE CON LA CURVA antisurge
            DELTA = 1
          else if(i.eq.6) then
C---->  INTERSEZIONE CON LA CURVA operating limit
            A = clim(4)
            B = -M
            C = -N
            DELTA =B**2-4.*A*C
          end if
          IF(DELTA.GE.0.) THEN
            if(i.eq.1) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              call hdaqn_a(hroot1,qroot1,max_rev,CHN)
	        call hdaqn_a(hroot2,qroot2,max_rev,CHN)
              OK1=((QROOT1.GE.QA).AND.(QROOT1.LE.QB).AND.(HROOT1.LE.HA)
     *                          .AND.(HROOT1.GE.HB))
              OK2=((QROOT2.GE.QA).AND.(QROOT2.LE.QB).AND.(HROOT2.LE.HA)
     *                          .AND.(HROOT2.GE.HB))
	        if (ok1.or.ok2) flag_max=.true. !individuata intersezione
            else if(i.eq.2) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              call hdaqn_a(hroot1,qroot1,min_rev,CHN)
	        call hdaqn_a(hroot2,qroot2,min_rev,CHN)
              OK1=((QROOT1.GE.qf).AND.(QROOT1.LE.qe).AND.(HROOT1.LE.hf)
     *                          .AND.(HROOT1.GE.he))
              OK2=((QROOT2.GE.qf).AND.(QROOT2.LE.qe).AND.(HROOT2.LE.hf)
     *                          .AND.(HROOT2.GE.he))
	        if (ok1.or.ok2) flag_min=.true.  !individuata intersezione
            else if(i.eq.3) then
	        if (clim(3).ne.clim(4))then
				QROOT1 = (-B + SQRT(DELTA))/(2.*A)
				QROOT2 = (-B - SQRT(DELTA))/(2.*A)
				call hdaqn_a(hroot1,qroot1,max_rev,CHC)
				call hdaqn_a(hroot2,qroot2,max_rev,CHC)
				OK1=((QROOT1.GE.Qb).AND.(QROOT1.LE.Qc).AND.
     *                 (HROOT1.LE.Hb).AND.(HROOT1.GE.Hc))
				OK2=((QROOT2.GE.Qb).AND.(QROOT2.LE.Qc).AND.
     *                 (HROOT2.LE.Hb).AND.(HROOT2.GE.Hc))
	        else 
	            OK1=.false.
	            OK2=.false.
	        end if
		  else if(i.eq.4) then
	        if (clim(3).ne.clim(4))then
				QROOT1 = (-B + SQRT(DELTA))/(2.*A)
				QROOT2 = (-B - SQRT(DELTA))/(2.*A)
				call hdaqn_a(hroot1,qroot1,min_rev,CHC)
				call hdaqn_a(hroot2,qroot2,min_rev,CHC)
				OK1=((QROOT1.GE.Qe).AND.(QROOT1.LE.Qd).AND.
     *                 (HROOT1.LE.He).AND.(HROOT1.GE.Hd))
				OK2=((QROOT2.GE.Qe).AND.(QROOT2.LE.Qd).AND.
     *                 (HROOT2.LE.He).AND.(HROOT2.GE.Hd))
	        else
	            OK1=.false.
	            OK2=.false.	        
			end if
 
            else if(i.eq.5) then
C INTERSEZIONE CON LE SPEZZATE DI ANTI-SURGE
              DO kk=1,NLIM+1
                QROOT(kk)=(N-ALIM(kk))/(BLIM(kk)-M)
              ENDDO
c devo trovare lo head che sia compreso nel range relativo alla portata
c per la quale viene calcolato
              DO kk=1,NLIM
                CALL RSURGE_a(NLIM,qroot(kk),LIM_Q,IRANGE)
                if (kk.eq.irange) then
                  hroot1 = ALIM(IRANGE)+BLIM(IRANGE)*QROOT(kk)
                  qroot1=qroot(kk)
                  goto 777
                endif               
              ENDDO
777           continue
              OK1=((QROOT1.GE.Qf).AND.(QROOT1.LE.QA).AND.
     *             (HROOT1.GE.Hf).AND.(HROOT1.LE.HA))
              OK2=.false.
            else if(i.eq.6) then
              QROOT1 = (-B + SQRT(DELTA))/(2.*A)
              QROOT2 = (-B - SQRT(DELTA))/(2.*A)
              HROOT1 = clim(4)*QROOT1**2
              HROOT2 = clim(4)*QROOT2**2
              OK1=((QROOT1.GE.Qd).AND.(QROOT1.LE.Qc).AND.(HROOT1.GE.Hd)
     *                          .AND.(HROOT1.LE.HB))
              OK2=((QROOT2.GE.Qd).AND.(QROOT2.LE.Qc).AND.(HROOT2.GE.Hd)
     *                          .AND.(HROOT2.LE.HB))
            end if
            IF(OK1.AND.(.NOT.OK2)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT1
              HVAL(NVAL) = HROOT1
            ELSE IF (OK2.AND.(.NOT.OK1)) THEN
              NVAL = NVAL+1
              QVAL(NVAL) = QROOT2
              HVAL(NVAL) = HROOT2
            ELSE IF (OK1.AND.OK2) THEN
              AUX = (QROOT1-Q(2))**2+(HROOT1-H(2))**2
              DIST1 = SQRT(AUX)
              AUX = (QROOT2-Q(2))**2+(HROOT2-H(2))**2
              DIST2 = SQRT(AUX)
              IF(DIST1.LT.DIST2) THEN
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT1
                HVAL(NVAL) = HROOT1
              ELSE
                NVAL = NVAL+1
                QVAL(NVAL) = QROOT2
                HVAL(NVAL) = HROOT2
              END IF
            END IF
          END IF
          if(nval.eq.2) goto 100
        end do
      end if
c---->           punti trovati
100   IF(NVAL.EQ.1) THEN
        qval(2) = qval(1)
        hval(2) = hval(1)
      ENDIF
      IF(Q(1).NE.Q(2)) THEN
        if(abs(q(1)-qval(1)).le.eps) then
          q(2) = qval(1)
          h(2) = hval(1)
        else if(abs(q(1)-qval(2)).le.eps) then
          q(2) = qval(2)
          h(2) = hval(2)
        else if(abs(q(2)-qval(1)).le.eps) then
          q(1) = qval(1)
          h(1) = hval(1)
        else if(abs(q(2)-qval(2)).le.eps) then
          q(1) = qval(2)
          h(1) = hval(2)
        else IF(((QVAL(1)-Q(2))*(Q(2)-Q(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      ELSE
        IF(((HVAL(1)-H(2))*(H(2)-H(1))).GT.0) THEN
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(1)
          H(2)=HVAL(1)
        ELSE
          Q(1)=Q(2)
          H(1)=H(2)
          Q(2)=QVAL(2)
          H(2)=HVAL(2)
        END IF
      END IF
      RETURN
      END

C
