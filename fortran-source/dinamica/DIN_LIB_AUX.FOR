!======================================================================
	SUBROUTINE din_agg_tjt_mod(	! Aggiorna le variabili TJT
	1	tempo,		! :IN istante attuale + passo integrazione
	2	dt)		! :IN passo integrazione
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/th.inc'		!:costante
	INCLUDE '../inc/tx.inc'		!:IN
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/top_app_ele.inc'	!:INOUT
	INCLUDE '../inc/ty.inc'		!:INOUT
	INCLUDE '../inc/tjt.inc'		!:telemisure
	INCLUDE '../inc/ric.inc'		!:telemisure

        real*4 tempo,dt

       real*4 port_app,cons_app,pres_imp,pmin,pmax
       integer*4 JE,it,oo,n_ele,kflagc
       logical*2 elem_spe,elem_vlv,elem_cen,elem_uni
       external  elem_spe,elem_vlv,elem_cen,elem_uni
       logical*2 simula_ce
       external simula_ce

        real*4 QPUNT_new,Qelem_sp               ! Function
        real*4 PSET_rampa                       ! Function
*&*
!----------------------------------------------------------------------
!  	Elementi Speciali
!----------------------------------------------------------------------
	DO it = 1,ne_spe
  	  DO je = otipti(te_spe(it)),etipti(te_spe(it))

          if (elem_vlv(je)) then        ! Valvola

            call controllo_set_valvola (je)

            porttjt(je) = Qelem_sp(tempo,je)

          else if (elem_cen(je)) then   ! Centrale

            call controllo_set_centrale (je)

            porttjt(je) = Qelem_sp(tempo,je)

          else                                  ! Unari

           porttjt(je)   = Qelem_sp(tempo,je)

          endif
	  ENDDO
	ENDDO
!----------------------------------------------------------------------
!  	Off_take + Set in P per i punti non capacitivi
!----------------------------------------------------------------------
	DO je = otipti(e_PU),etipti(e_pu)

           porttjt(je)   = QPUNT_new(tempo,dt,je)

         pu_pimp(pgrato(je)) = 0
         pmin = pmintx(je)
         pmax = pmaxtx(je)
!         if (fl_nocap(pgrato(je))) then
           n_ele = 0
!          elemento entrante
           oo = oseeto(je)
           DO WHILE (oo .NE. je)
             if (elem_spe(oo)) then
!chsta               IF (htcitj(oo) .EQ. tc_pv .or. 
!chsta     -             htcitj(oo) .EQ. tc_npv .or.
!chsta     -             htcitj(oo) .EQ. tc_pq )   THEN	
               if (elem_uni(oo)) then
                 pmin = max(pmin,pmintx(oo))
                 pmax = min(pmax,pmaxtx(oo))
	         else if (htcitj(oo).eq.tc_pq) then
	           pmax = min(pmax,pvsetj(oo))
	         endif
               IF (hstatj(oo) .EQ. sta_pv)   THEN	
                 pu_pimp(pgrato(je)) = -oo
                 n_ele = n_ele +1
	         endif
	       endif
             oo = oseeto(oo)
           ENDDO
!          elemento uscente
           oo = oseuto(je)
           DO WHILE (oo .NE. je)
             if (elem_spe(oo)) then
!chsta               IF (htcitj(oo) .EQ. tc_pm .or. 
!chsta     -             htcitj(oo) .EQ. tc_npm .or.
!chsta     -             htcitj(oo) .EQ. tc_pq )   THEN	
               if (elem_uni(oo)) then
                 pmin = max(pmin,pmintx(oo))
                 pmax = min(pmax,pmaxtx(oo))
	         else if (htcitj(oo).eq.tc_pq) then
	           pmin = max(pmin,pmsetj(oo))
	         endif
               IF (hstatj(oo) .EQ. sta_pm)   THEN	
                 pu_pimp(pgrato(je)) = oo
                 n_ele = n_ele +1
	         endif
	       endif
             oo = oseuto(oo)
           ENDDO
!!!!!!!!!  gestire eventuale errore  if (n_ele.ne.1)
           if (pu_pimp(pgrato(je)).eq.0) then
		   pres_imp = prestj(je)
             rc_peso_ctlm(je) = rc_weit_p/10
	     else
		   pres_imp = PSET_rampa(tempo,pu_pimp(pgrato(je)))
             rc_peso_ctlm(je) = rc_weit_p
	     endif

           if (pres_imp.lt.pmin) then
              pu_pimp(pgrato(je)) = -je
              pres_imp = pmin
              rc_peso_ctlm(je) = rc_weit_p
C              rc_peso_ctlm(je) = rc_weit_p*10
           elseif (pres_imp.gt.pmax) then
              pu_pimp(pgrato(je)) = je
              pres_imp = pmax
              rc_peso_ctlm(je) = rc_weit_p
c              rc_peso_ctlm(je) = rc_weit_p*10
		 endif
           CALL zrotdp(je,rocitj(je),temptj(je),zmcitj(je),dproty(je))
		 rocitjt(je) =  pres_imp / 
     *               (temptj(je) * zmcitj(je) * ravotd(je))

	ENDDO

	DO it = 1,ne_cen
  	  DO je = otipti(te_cen(it)),etipti(te_cen(it))
          if(simula_ce(je)) call controllo_centrale (je,1,kflagc)
	  ENDDO
	ENDDO


	RETURN
	END

	SUBROUTINE din_check_ko_elem(kflag)	!Controlla solo limiti KO dei punti
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/default.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT

        integer*4 kflag      !-1 superati limiti sulle pressioni (punti)
	                       ! 1 superati limiti sulle portate (elementi speciali)

       integer*4 JE,it,kflagc

       logical*2 simula_ce
       external simula_ce

*&*
      kflag = 0
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	DO je = otipti(e_PU),etipti(e_pu)
	      IF   (prestj(je) .lt. pmin_def)  THEN 
			 kflag = -1
		  endif
	enddo

c-sim_ce
c check della fattibilità delle centrali completi
      if (kflag .eq. 0) then
	 DO it = 1,ne_cen
  	   DO je = otipti(te_cen(it)),etipti(te_cen(it))
           if(simula_ce(je)) then
		   call controllo_centrale (je,0,kflagc)
	       if (kflagc.lt.0) kflag=-1
	     endif
	   ENDDO
	 ENDDO
c-din-end
cgpe-end
      endif
c-sim_ce-end

	RETURN
	END

	SUBROUTINE din_check_elem(kflag)	!Controlla limiti degli elementi
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/default.inc'		!:costante
	INCLUDE '../inc/th.inc'		!:costante
	INCLUDE '../inc/tx.inc'		!:IN
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/top_app_ele.inc'	!:INOUT
	INCLUDE '../inc/ty.inc'		!:INOUT
	INCLUDE '../inc/tjt.inc'		!:telemisure
	INCLUDE '../inc/ric.inc'		!:telemisure
	INCLUDE '../inc/rampa.inc'		!:telemisure
c-sim_ce
      include '../inc/FLAG.INC'
c-sim_ce-end

        integer*4 kflag      !-1 superati limiti sulle pressioni (punti)
	                       ! 1 superati limiti sulle portate (elemnti speciali)

       integer*4 kflagc,JE,it
	real*4 qmaxapp


       logical*2 elem_spe
       external  elem_spe
       logical*2 elem_vlv
       external elem_vlv
       logical*2 elem_cen
       external elem_cen
       logical*2 reg_nr
       external reg_nr

      logical*2 l_cor_ce           ! I) = .F. verifica della stazione
                                   !    = .T. viene corretto il punto di lavoro
      logical*2 l_cons_ce          ! I) = .F. verifica della consumo (cons=consty)
                                   !    = .T. viene corretto il consumo (cons=min(tot_cons())
      logical*2 l_rec_ce           ! O) risultato della simulazione globale
                                   !    = .T. richiesta di recupero (almeno una cen ha err recuperabile)
      logical*2 l_conv_ce          ! O) risultato convergenza consumo

	logical*2 flag_vlim
      !Function
	logical*2 set_non_ritorno
	external set_non_ritorno
	real*4 qset_real
	external qset_real
       logical*2 simula_ce
       external simula_ce

*&*
      kflag = 0
!----------------------------------------------------------------------
!  	punti (per prima perchè controllo meno stringente)
!----------------------------------------------------------------------
	DO je = otipti(e_PU),etipti(e_pu)
	      IF   (prestj(je) .lt. pmin_def)  THEN 
			 kflag = -1

		     
	      ELSEIF   (prestj(je) .lt. pmintx(je)-delta_p)  THEN    
!					----------------------------------
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
!----------------------------------------------------------------------
	      ELSEIF   (prestj(je) .gt. pmaxtx(je)+delta_p)  THEN 
!					----------------------------------
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
            ELSE
	         n_agg(je) = 0
            ENDIF

	ENDDO
!----------------------------------------------------------------------
!  	Elementi Speciali
!----------------------------------------------------------------------
	DO it = 1,ne_spe
  	  DO je = otipti(te_spe(it)),etipti(te_spe(it))

!----------------------------------------------------------------------
!----------------------------------------------------------------------
          if (htipto(je).eq.e_vg) call qmaxvg(je,qmaxtx(je),flag_vlim)
cqpcls          qmaxapp = qmaxtx(je)*pcls_def/pclstd(je)
          qmaxapp = qmaxtx(je)
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	      IF   (htcitj(je) .EQ. tc_off .and.
     -             abs(porttj(je)).gt.delta_q)  THEN    
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
!----------------------------------------------------------------------
	      ELSEIF   (abs(porttj(je)).gt.qmaxapp+delta_q)  THEN 
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	      ELSEIF   (htcitj(je) .EQ. tc_pq) then
		   if (abs(porttj(je)).gt.qset_real(je)+delta_q .or.
     -           prestj(opumto(je)).lt.pmsetj(je)-delta_p .or.
     -           prestj(opuvto(je)).gt.pvsetj(je)+delta_p)  THEN 
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
	       endif
!----------------------------------------------------------------------
	      ELSEIF (reg_nr(je) .and.
     -              sign(1.0,diretj(je)).ne.sign(1.0,porttj(je))) then !inversione di direzione
		   if (abs(porttj(je)).gt.delta_q)  then
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
	       endif
            ELSE
	         n_agg(je) = 0
            ENDIF
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	  ENDDO
	ENDDO

c-sim_ce
c check della fattibilità delle centrali completi
      if (kflag .eq. 0) then
	 DO it = 1,ne_cen
  	   DO je = otipti(te_cen(it)),etipti(te_cen(it))
           if(simula_ce(je)) then
		   call controllo_centrale (je,0,kflagc)
	       if (kflagc.lt.0) then
		          kflag = -1
	       elseif (kflagc.gt.0) then
	         if (n_agg(je).le.n_agg_max) then
	              n_agg(je) = n_agg(je) + 1
		          kflag = 1
	         endif
             else
	            n_agg(je) = 0
	       endif
	     endif
	   ENDDO
	 ENDDO
c-din-end
cgpe-end
      endif
c-sim_ce-end

	RETURN
	END

!======================================================================
	SUBROUTINE DIN_MODEL   (! calcola il residuo del sistema
	1		 rom,		! :IN vettore densita' attuali
	2		 roder)		! :OUT vettore della funzione del sistema:
c_ace                        punti, tronchi
*&*------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tx.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/ric.inc'    !:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/tjt.inc'	!:IN
	INCLUDE '../inc/ty.inc'		!:INOUT

	real*4   	rom   (*)
	real*4   	roder (*)

        integer*4 ie,im,iv,it
        real*4 x,cons_app
      logical*2 elem_cen
      external elem_cen
*&*------------------------------------------------------------
!----------------------------------------------------------------------
!	punti :  ro, zm, pres, dproty, droty
!----------------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
c_ace   if (f_densita.eq.1) then
c		rocitj(ie) = rom(rc_topvar(ie))
c	  else
c		rocitj(ie) = rom(rc_topvar(ie))*ro0td(ie)
c	  endif
c_ace
		droty(ie)  = 0.0	    	!inizializza la derivata di ro
          rocitj(ie) = rom(rc_topvar(ie))
		CALL zrotdp(ie,rocitj(ie),temptj(ie),zmcitj(ie),dproty(ie))
		prestj(ie) = rocitj(ie) * temptj(ie) * zmcitj(ie) * ravotd(ie)
	ENDDO
!----------------------------------------------------------------------
!	tronchi :   portate
!----------------------------------------------------------------------
	DO ie = otipti(e_tr),etipti(e_tr)
		im = opumto(ie)			! offset punto monte in TOPO
		iv = opuvto(ie)			! offset punto valle in TOPO
		x  = rom(rc_topvar(ie))
        if (f_densita.eq.1) then
 		porttj(ie) = x/ro0td(ie)
	  else
 		porttj(ie) = x
	  endif
	  roder(rc_topvar(ie))   =   
     *              aferty(ie) * prestj(im) * prestj(im)
     *              -   prestj(iv) * prestj(iv)
     *              -   bferty(ie) * porttj(ie) * ABS(porttj(ie))
	  droty(im)  = droty(im) - x
	  droty(iv)  = droty(iv) + x
	ENDDO
!----------------------------------------------------------------------
!     elementi speciali : portate
!----------------------------------------------------------------------
        DO it=1,ne_spe
 	  DO ie = otipti(te_spe(it)),etipti(te_spe(it))
		im = opumto(ie)			! offset punto monte in TOPO
		iv = opuvto(ie)			! offset punto valle in TOPO
		x = rom(rc_topvar(ie))
		cons_app =  0.0         ! consty = consumo in volume
        if (f_densita.eq.1) then
 		porttj(ie) = x/ro0td(ie)
	    if (elem_cen(ie)) 
     -	   cons_app =  consty(ie)*ro0td(ie)
	  else
 		porttj(ie) = x
	    if (elem_cen(ie)) 
     -	   cons_app =  consty(ie)
	  endif
c	  if (elem_cen(ie)) then
c		cons_app =  consty(ie)*ro0td(ie)
c	  else
c		cons_app =  0.0
c	  endif
          if (htipto(im).eq.e_pu) droty(im)  = droty(im) - x - cons_app
		if (htipto(iv).eq.e_pu) droty(iv)  = droty(iv) + x
 	  ENDDO
	ENDDO
!----------------------------------------------------------------------
!     prelievi : portata
!----------------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
          porttj(ie) = porttjt(ie)
c_ace
c         porttjt è volumetrica pertanto nel calcolo di 
c         droty (con f_densita.eq.1) occorre trasformarla in portata massica
        if (f_densita.eq.1) then     ! c_den_attilia
		droty(ie) = droty(ie) - porttjt(ie)*ro0td(ie)
  	    roder(rc_topvar(ie))  = droty(ie) / volntx(ie)
	  else                   !??????????????????????????????????????????
		droty(ie) = droty(ie) - porttjt(ie)
	    roder(rc_topvar(ie))  = droty(ie) / volntx(ie) * ro0td(ie)
	  endif
	ENDDO
	RETURN
	END	!DIN_MODEL

!======================================================================
	SUBROUTINE DIN_MODEL_D   (! calcola il residuo del sistema
	2		 dt,		! :IN intervallo di tempo
	3		 rom,		! :IN vettore densita' attuali
	4		 roder)		! :OUT vettore delle derivate prima per:
c_ace                            punti,tronchi, el. speciali
*&*------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/top_app_ele.inc'		!:IN
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tx.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/ric.inc'	!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/tjt.inc'	!:IN
	INCLUDE '../inc/ty.inc'		!:INOUT

	real*4   	dt
	real*4   	rom   (*)
	real*4   	roder (*)

        integer*4 ie,im,iv,it
        real*4 x
        real*4 ro_aux,z_aux,presnew(m_ogg),deltaq,radice
        real*4 dqs,tqs
	  integer*4 oo
        logical*2 elem_spe

        real*4 cons_app
      logical*2 elem_cen
      external elem_cen
*&*------------------------------------------------------------
!----------------------------------------------------------------------
!	punti :  calcola: ro, zm, pres, dproty, droty
!----------------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
		rocitj(ie) = rom(rc_topvar(ie))
		CALL zrotdp(ie,rocitj(ie),temptj(ie),zmcitj(ie),dproty(ie))
		prestj(ie) = rocitj(ie) * temptj(ie) * zmcitj(ie) * ravotd(ie)
		droty(ie)  = 0.0		!inizializza la derivata di ro
	ENDDO
!----------------------------------------------------------------------
!	tronchi : calcola portate volumetriche
!----------------------------------------------------------------------
	DO ie = otipti(e_tr),etipti(e_tr)
		im = opumto(ie)			! offset punto monte in TOPO
		iv = opuvto(ie)			! offset punto valle in TOPO
		x	   = rom(rc_topvar(ie))
        if (f_densita.eq.1) then
 		porttj(ie) = x/ro0td(ie)
	  else
 		porttj(ie) = x
	  endif
		droty(im)  = droty(im) - x
		droty(iv)  = droty(iv) + x
	ENDDO
!----------------------------------------------------------------------
!     elementi speciali : calcola portate volumetriche
!----------------------------------------------------------------------
      DO it=1,ne_spe
 	  DO ie = otipti(te_spe(it)),etipti(te_spe(it))
		im = opumto(ie)			! offset punto monte in TOPO
		iv = opuvto(ie)			! offset punto valle in TOPO
		x  = rom(rc_topvar(ie))
		cons_app =  0.0         ! consty = consumo in volume
        if (f_densita.eq.1) then
 		porttj(ie) = x/ro0td(ie)
	    if (elem_cen(ie)) 
     -	   cons_app =  consty(ie)*ro0td(ie)
	  else
 		porttj(ie) = x
	    if (elem_cen(ie)) 
     -	   cons_app =  consty(ie)
	  endif
c	  if (elem_cen(ie)) then
c		cons_app =  consty(ie)*ro0td(ie)
c	  else
c		cons_app =  0.0
c	  endif

          if (htipto(im).eq.e_pu) droty(im)  = droty(im) - x - cons_app
		if (htipto(iv).eq.e_pu) droty(iv)  = droty(iv) + x
 	  ENDDO
	ENDDO

!----------------------------------------------------------------------
!     punti : aggiorna i prelievi per il calcolo di dro/dt 
!----------------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
          porttj(ie) = porttjt(ie)
c         porttjt è volumetrica pertanto nel calcolo di 
c         droty (con f_densita.eq.1) occorre trasformarla in portata massica
        if (f_densita.eq.1) then     
		droty(ie) = droty(ie) - porttjt(ie)*ro0td(ie)
	    roder(rc_topvar(ie))  = droty(ie) / volntx(ie)
	  else                   
		droty(ie) = droty(ie) - porttjt(ie)
	    roder(rc_topvar(ie))  = droty(ie) / volntx(ie) * ro0td(ie)
	  endif
	ENDDO
c
c calcolo dell'effettivo deltaq/dt per i tronchi
c
	DO ie = otipti(e_pu),etipti(e_pu)
                ro_aux = rom(rc_topvar(ie)) + roder(rc_topvar(ie))*dt
                call zrot (ie,ro_aux,temptj(ie),z_aux)
                presnew(ie) = ro_aux * temptj(ie) * z_aux *
     *                       ravotd(ie)
      ENDDO
	DO ie = otipti(e_tr),etipti(e_tr)
                im = opumto(ie)                 ! offset punto monte in TOPO
                iv = opuvto(ie)                 ! offset punto valle in TOPO

                radice = aferty(ie) * presnew(im) * presnew(im)
     *                  - presnew(iv) * presnew(iv)
                radice = sign(sqrt(abs(radice) / bferty(ie)),radice)
                deltaq = -porttj(ie) + radice
              if (f_densita.eq.1) then
                roder(rc_topvar(ie)) = (deltaq * ro0td(ie))/dt
			else
                roder(rc_topvar(ie)) =  deltaq/dt
			endif     
      ENDDO
!----------------------------------------------------------------------
! calcolo della derivata degli el. speciali 
!----------------------------------------------------------------------
      DO it=1,ne_spe
 	  DO ie = otipti(te_spe(it)),etipti(te_spe(it))
                dqs = 0.
                tqs = 0.
		      iv = opuvto(ie)			! offset punto valle in TOPO

!                                       ------------------------------
!                                       per ogni elemento entrante
!                                       ------------------------------
                oo = oseeto(iv)
                DO WHILE (oo .NE. iv)
                  if (.not.elem_spe(oo)) then
                    dqs = dqs + roder(rc_topvar(oo))
                  else
                    tqs = tqs + abs(rom(rc_topvar(oo)))
                  endif
                  oo = oseeto(oo)

                ENDDO
!                                       ------------------------------
!                                       per ogni elemento uscente
!                                       ------------------------------
                oo = oseuto(iv)
                DO WHILE (oo .NE. iv)
                  if (.not.elem_spe(oo)) then
                    dqs = dqs - roder(rc_topvar(oo))
                  else
                    tqs = tqs + abs(rom(rc_topvar(oo)))
                  endif
                  oo = oseuto(oo)
                ENDDO

                roder(rc_topvar(ie)) = 0.0
                if (tqs.eq.0.0) then
                  roder(rc_topvar(ie)) = 0.0
                else
                  roder(rc_topvar(ie)) = -dqs *
     -                       (abs(rom(rc_topvar(ie)))/tqs)
                endif                

 	  ENDDO
	ENDDO
c end deltaq

	RETURN
	END	!DIN_MODEL_D

!======================================================================
	SUBROUTINE DIN_MODEL_Gear (! calcola il residuo del sistema
	2		 n_tot,		! :IN intervallo di tempo
	2		 n_pres,	! :IN intervallo di tempo
	2		 hh,		! :IN intervallo di tempo
	3		 Y,			! :OUT vettore densita' attuali
	4		 SAVE1)		! :OUT vettore delle derivate prima per:
c_ace                            punti,tronchi, el. speciali
*&*------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante

      integer*4   n_tot,n_pres
	real*4   	hh
	real*4   	Y(n_tot,*)
	real*4   	SAVE1 (*)

        integer*4 i
*&*------------------------------------------------------------
      CALL DIN_MODEL_D (HH,Y,SAVE1)
      DO  I=1,n_tot
         Y(I,2) = HH*SAVE1(I)
      enddo

c      CALL DIN_MODEL (Y,SAVE1)
c      DO  I=1,n_pres 
c         Y(I,2) = HH*SAVE1(I)
c      enddo
c	DO I=n_pres+1,n_tot
c         Y(I,2) = 0.0
c      end do
	RETURN
	END	!DIN_MODEL_Gear

!======================================================================
!======================================================================
        real*4 function q_ferg_tj(i_tr)
c----------------------------------------------------------------
c       Questa fuction calcola la portata di un tronco (i_tr)
c       attraverso la formula di Fergusson
c----------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/ty.inc'		!:INOUT

        INTEGER*4 i_tr

        real*4   x_x
        INTEGER*4 im,iv
!------------
	im = opumto(i_tr)			! offset punto monte in TOPO
	iv = opuvto(i_tr)			! offset punto valle in TOPO
        call agg_tronco(i_tr)
        x_x = (aferty(i_tr)*prestj(im)**2 -
     -         prestj(iv)**2) / bferty(i_tr)
        
        q_ferg_tj = sign(sqrt(abs(x_x)),x_x)
        return
        end
!======================================================================
!======================================================================
!-----------------------------------------------------------------------
cs
      subroutine verifica_limiti_cs(frec,i_ele,
     *           pm_rich,pv_rich,q_rich,c_rich,ivar)
*&*
c----------------------------------------------------------------------------
c  Verifica la fattibilità di una condizione di esercizio di una centrale
c  semplificata descritta dalla terna  (Pm,Pv,Q,T)
c----------------------------------------------------------------------------
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/th.inc'    !stati di regolazione
	include '../inc/ti.inc'    !identificatori elementi
	include '../inc/tx.inc'    !dati topologici elementi (qmaxtx)
	include '../inc/tj.inc'    !cin (pressioni,portate...)
	include '../inc/top_app_ele.inc' 
   	include '../inc/staznw_par.inc'
c
      integer*4  frec      ! I) fare recupero 1:si
     *          ,i_ele     ! I) indice dell'elemento nella struttura dati completa
      real*4     pm_rich   ! O)
     *          ,pv_rich   ! O)
     *          ,q_rich    ! O)
     *          ,c_rich    ! O)
      integer*4  ivar      ! O)

      integer*4 im,iv, ipg, ind1,ind2
      real*4    pvmax
      real*4    qpwmce,cons_cen
	external  qpwmce,cons_cen

	logical*2 l_ok
	logical*2 set_non_ritorno
	external  set_non_ritorno
c-------------------------------------------------------------------------------------
      im = opumto(i_ele)
      iv = opuvto(i_ele)
      ipg = pgrato(i_ele)

c inizializzazione
      pm_rich = R_MAX
      pv_rich = R_MAX
      q_rich= R_MAX

	ivar   = 0        
          pvmax = prestj(im)*romax(ipg)
          if (prestj(iv) .gt. pvmax +repsP) then
            pv_rich = pvmax
          endif
          if (porttj(i_ele) .gt. qpwmce(i_ele)) then
            q_rich = qpwmce(i_ele)
          endif
c calcolo di ivar
      if (pm_rich .ne. R_MAX .and. pm_rich .ne. prestj(im)) then
          ivar = 1
	else
	    pm_rich = prestj(im)
	endif
      if (pv_rich .ne. R_MAX .and. pv_rich .ne. prestj(iv)) then
          ivar = ivar+1*10
	else
	    pv_rich = prestj(iv)
	endif
      if (q_rich .ne. R_MAX .and. q_rich .ne. porttj(i_ele)) then
          ivar = ivar+1*100
	else
	    q_rich = porttj(i_ele)
	endif

      if (frec.eq.0.and.ivar.ne.0) then
	  c_rich = R_MAX
	else
	  c_rich = cons_cen (i_ele,pm_rich,pv_rich,q_rich)
	endif

      return
	end
cs-end

      subroutine controllo_centrale(i_ele,frec,kflagc)
*&*
c----------------------------------------------------------------------------
c  Verifica la fattibilità di una condizione di esercizio di una centrale
c----------------------------------------------------------------------------
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/default.inc' !parametri costanti
	include '../inc/tx.inc'    
	include '../inc/th.inc'    !stati di regolazione
	include '../inc/ti.inc'    !identificatori elementi
	include '../inc/ty.inc'    !identificatori elementi
	include '../inc/tj.inc'    !cin (pressioni,portate...)
	include '../inc/tjt.inc'    !cin (pressioni,portate...)
	include '../inc/td.inc'    !cin (pressioni,portate...)
	include '../inc/ric.inc'    !cin (pressioni,portate...)
c
      integer*4  i_ele     ! I) indice dell'elemento nella struttura dati completa
     -          ,frec      ! I) frec 1:cerca valori da proporre
     -          ,kflagc    ! O) flag 0:ok


      real*4     pm_rich
     *          ,pv_rich
     *          ,q_rich 
     *          ,c_rich

      real*4     pm_sav
     *          ,pv_sav
     *          ,q_sav 
     *          ,qmax_sav 
     *          ,pmmin_sav
     *          ,pvmax_sav

      integer*4  ivar   


      integer*4 im,iv

      real*4    cons_cen
      external  cons_cen
	real*4 qset_real
	external qset_real
c-------------------------------------------------------------------------------------
      im = opumto(i_ele)
      iv = opuvto(i_ele)

c     Salvo i valori tj
	q_sav = porttj(i_ele)
	pv_sav = prestj(iv)
	pm_sav = prestj(im)
	qmax_sav = qmaxtx(i_ele)
      pmmin_sav = pmintx(im) 
      pvmax_sav = pmaxtx(iv) 

      if (htcitj(i_ele).eq.tc_pq) then
        qmaxtx(i_ele) = min(qmaxtx(i_ele),qset_real(i_ele))
        pmintx(im) = max(pmintx(im),pmsetj(i_ele))
        pmaxtx(iv) = min(pmaxtx(iv),pvsetj(i_ele))
	else
        qmaxtx(i_ele) = R_MAX
	endif
c     Riporto in tj i valori attesi tjt
c     per usare gli stessi controlli su tj 
cqpcls	porttj(i_ele) = porttjt(i_ele)*pclstd(i_ele)/pcls_def
	porttj(i_ele) = porttjt(i_ele)
      prestj(iv) = rocitjt(iv) *
     *               (temptj(iv) * zmcitj(iv) * ravotd(iv))
      prestj(im) = rocitjt(im) *
     *               (temptj(im) * zmcitj(im) * ravotd(im))

c  Verifica che siano rispettati i vincoli fisici
	if(hstatj(i_ele).eq.sta_fr .or.
     -   hstatj(i_ele).eq.sta_on .or. 
     -   hstatj(i_ele).eq.sta_off) then
	  c_rich = 0
        goto 100 
      else if (prestj(im).ge.prestj(iv)) then
	  c_rich = 0
	  goto 100
      endif

      if (htipto(i_ele).eq.e_ce) then
	  call verifica_limiti_ce(frec,i_ele,pm_rich,pv_rich,q_rich,
     *       c_rich,ivar)
	else
	  call verifica_limiti_cs(frec,i_ele,pm_rich,pv_rich,q_rich,
     *       c_rich,ivar)
	endif

c controllo ivar
	  if (ivar.ge.100) then      ! q  fuori controllo
	    ivar = ivar - 100
          rc_peso_ctlm(i_ele) = max(rc_weit_q,rc_peso_ctlm(i_ele))
cqpcls    	    porttjt(i_ele) =  q_rich*pcls_def/pclstd(i_ele)
    	    porttjt(i_ele) =  q_rich
	    porttj(i_ele) = q_rich
	  endif

	  if (ivar.ge.10) then       ! pm fuori controllo
	    ivar = ivar - 10
          rc_peso_ctlm(im) = max(rc_weit_p,rc_peso_ctlm(im))
          rocitjt(im) =  pm_rich / 
     *              (temptj(im) * zmcitj(im) * ravotd(im))
	    prestj(im) = pm_rich
	  endif

	  if (ivar.ge.1) then      ! pv fuori controllo
	    ivar = ivar - 1
          rc_peso_ctlm(iv) = max(rc_weit_p,rc_peso_ctlm(iv))
          rocitjt(iv) =  pv_rich / 
     *              (temptj(iv) * zmcitj(iv) * ravotd(iv))
	    prestj(iv) = pv_rich
	  endif

c-din-assetti-coniugati
	if (ivar.lt.0) then      ! la verifica di centrale ha dato esito ko
          kflagc = -1
	    c_rich = 0
c-din-assetti-coniugati-end
      elseif (c_rich.eq.R_MAX) then
	    kflagc = 1
	    c_rich = cons_cen (i_ele,prestj(im),prestj(iv),porttj(i_ele))
	    if (c_rich.eq.R_MAX) c_rich = 0
	else
	    kflagc = 0
	endif
	



100   continue

cqpcls      consty(i_ele) = c_rich*pcls_def/pclstd(i_ele)
      consty(i_ele) = c_rich

c     Ripristino i valori tj
      porttj(i_ele) = q_sav
      prestj(iv)    = pv_sav
      prestj(im)    = pm_sav

	qmaxtx(i_ele) = qmax_sav
      pmintx(im) = pmmin_sav
      pmaxtx(iv) = pvmax_sav

      return
	end
c----------------------------------------------------------------------------
c----------------------------------------------------------------------------
      subroutine controllo_set_valvola(i_ele)
*&*
c----------------------------------------------------------------------------
c  Controlla set di una valvola
c----------------------------------------------------------------------------
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/default.inc' !parametri costanti
	include '../inc/th.inc'    !stati di regolazione
	include '../inc/ti.inc'    !identificatori elementi
	include '../inc/tj.inc'    !cin (pressioni,portate...)
	include '../inc/tjt.inc'    !cin (pressioni,portate...)
	include '../inc/td.inc'    !cin (pressioni,portate...)
	include '../inc/ric.inc'    !cin (pressioni,portate...)
c
      integer*4  i_ele     ! I) indice dell'elemento nella struttura dati completa


      real*4     pm_sav
     *          ,pv_sav


      integer*4 im,iv ,h_sav 

      logical*2 cnt_eq_on  !function

c-------------------------------------------------------------------------------------
      im = opumto(i_ele)
      iv = opuvto(i_ele)

c      pm_sav = rocitjt(im) *
c     *               (temptj(im) * zmcitj(im) * ravotd(im))
c      pv_sav = rocitjt(iv) *
c     *               (temptj(iv) * zmcitj(iv) * ravotd(iv))

      pm_sav = prestj(im)
      pv_sav = prestj(iv)

      h_sav = hstatj(i_ele)

      if (htcitj(i_ele).eq.tc_pv.or.htcitj(i_ele).eq.tc_npv) then
	 if (pm_sav.le.pvsetj(i_ele)) then
        if (htcitj(i_ele).eq.tc_pv)  hstatj(i_ele) = sta_fr
        if (htcitj(i_ele).eq.tc_npv) hstatj(i_ele) = sta_on
	 else
	  hstatj(i_ele) = sta_pv
	 endif
       if(cnt_eq_on(h_sav,hstatj(i_ele))) then
         if (o_sta_ric.lt.O_ST_NVTLM) o_sta_ric = O_ST_NVTLM
	 endif
      elseif (htcitj(i_ele).eq.tc_pq) then
	 if (pm_sav.le.pmsetj(i_ele).and.
     -     h_sav.ne.sta_pm) then
          hstatj(i_ele) = sta_pm
	 elseif (pv_sav.ge.pvsetj(i_ele).and.
     -     h_sav.ne.sta_pv) then
          hstatj(i_ele) = sta_pv
	 endif
       if(cnt_eq_on(h_sav,hstatj(i_ele))) then
         if (o_sta_ric.lt.O_ST_NVTLM) o_sta_ric = O_ST_NVTLM
	 endif
	endif
      return
	end
c----------------------------------------------------------------------------
c----------------------------------------------------------------------------
      subroutine controllo_set_centrale(i_ele)
*&*
c----------------------------------------------------------------------------
c Controlla set di una centrale
c----------------------------------------------------------------------------
	implicit none

	include '../inc/param.inc' !parametri costanti
	include '../inc/default.inc' !parametri costanti
	include '../inc/th.inc'    !stati di regolazione
	include '../inc/ti.inc'    !identificatori elementi
	include '../inc/tj.inc'    !cin (pressioni,portate...)
	include '../inc/tjt.inc'    !cin (pressioni,portate...)
	include '../inc/td.inc'    !cin (pressioni,portate...)
	include '../inc/ric.inc'    !cin (pressioni,portate...)
c
      integer*4  i_ele     ! I) indice dell'elemento nella struttura dati completa


      real*4     pm_sav
     *          ,pv_sav


      integer*4 im,iv ,h_sav 

      logical*2 cnt_eq_on  !function

c-------------------------------------------------------------------------------------
      im = opumto(i_ele)
      iv = opuvto(i_ele)

c      pm_sav = rocitjt(im) *
c     *               (temptj(im) * zmcitj(im) * ravotd(im))
c      pv_sav = rocitjt(iv) *
c     *               (temptj(iv) * zmcitj(iv) * ravotd(iv))

      pm_sav = prestj(im)
      pv_sav = prestj(iv)

      h_sav = hstatj(i_ele)

      if (htcitj(i_ele).eq.tc_pq) then
	 if (pm_sav.le.pmsetj(i_ele).and.
     -     h_sav.ne.sta_pm) then
          hstatj(i_ele) = sta_pm
	 elseif (pv_sav.ge.pvsetj(i_ele).and.
     -     h_sav.ne.sta_pv) then
          hstatj(i_ele) = sta_pv
	 endif
       if(cnt_eq_on(h_sav,hstatj(i_ele))) then
         if (o_sta_ric.lt.O_ST_NVTLM) o_sta_ric = O_ST_NVTLM
	 endif
	endif

      return
	end
****** DIN_LIB_AUX.FOR *
