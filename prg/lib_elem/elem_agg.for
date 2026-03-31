c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Aggiornamenti
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!======================================================================
	SUBROUTINE AGG_PUNTO (je)        ! Aggiorna Punto da
                                       ! Pressione
                                       ! Temperatura
*&*
        implicit none
	INCLUDE '../inc/param.inc'	! :costante
	INCLUDE '../inc/TJ.inc'		! :IN

        integer*4 je


*&*
        call zpt1(je,prestj(je),temptj(je),zmcitj(je))
c_ace calcolo della densità
        call gaspt(je,prestj(je),rocitj(je),temptj(je),zmcitj(je))

	RETURN 	! AGG_PUNTO
	END


	SUBROUTINE AGG_TRONCO (je)       ! Aggiorna Tronco da
                                       ! Portata
                                       ! Punto Monte Punto Valle
*&*
        implicit none
	INCLUDE '../inc/param.inc'	! :costante
	INCLUDE '../inc/TI.inc'
	INCLUDE '../inc/TD.inc'
	INCLUDE '../inc/TJ.inc'
	INCLUDE '../inc/TY.inc'
	INCLUDE '../inc/Tz.inc'
	INCLUDE '../inc/Tx.inc'
	INCLUDE '../inc/staznw_par.inc'

        integer*4 je


        integer*4 im,iv

c        real*4 x
cT
        real*4 temp
*&*
        im = opumto(je)
        iv = opuvto(je)
!----------------------------------------------------------------------
!			calcola densita,temperatura,z,pressione del tronco
!----------------------------------------------------------------------
          depxtz(je) = ro0td(je)*(renptx(iv) - renptx(im))
cT		temptj(je) = (temptj(im)+temptj(iv)) /2.0
		temp = (temptj(im)+temptj(iv)) /2.0
		zmcitj(je) = (zmcitj(im)+zmcitj(iv)) /2.0
c		x       = prestj(im) + prestj(iv)
c		prestj(je) = 2.0/3.0 * (x - (prestj(im) * prestj(iv))/x)
c		rocitj(je) = prestj(je)/( zmcitj(je) * ravotd(je)* temptj(je) )
c		rocitj(je) = (rocitj(im) + rocitj(iv))/2.0

		CALL lambd2(je,porttj(je))
c	    if (abs(porttj(je)).gt.repsQ) then
		   CALL ferg1 (je,aferty(je),bferty(je),temp,zmcitj(je))
c	    else
c	       aferty(je)=0.
c	       bferty(je)=1.
c	    endif

	RETURN 	! AGG_TRONCO
	END

        SUBROUTINE AGG_TRONCO_Q(je)    ! Aggiorna Tronco +
                                       !    Portata
c----------------------------------------------------------------
c       Questa fuction calcola iterativamente la portata di un tronco (je)
c       (attraverso la formula di Fergusson)
c       aggiornando anche le grandezze del tronco
C       Si presuppone un valore iniziale di porttj
c----------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'			!:IN
	INCLUDE '../inc/tj.inc'			!:INOUT
	INCLUDE '../inc/ty.inc'			!:INOUT

        INTEGER*4 je

        real*4   x_x,q_del,q_app
        INTEGER*4 im,iv
!------------
	im = opumto(je)			! offset punto monte in TOPO
	iv = opuvto(je)			! offset punto valle in TOPO
        
        q_del = R_MAX
c        do while (q_del.gt.rc_rtol_cvQ)
        do while (q_del .gt. 0.001)
          q_app = porttj(je)
          call agg_tronco(je)
          x_x = (aferty(je)*prestj(im)**2 -
     -              prestj(iv)**2) / bferty(je)
          porttj(je) = sign(sqrt(abs(x_x)),x_x)
          q_del = abs(q_app-porttj(je))/
     -                  max(10.0,abs(porttj(je)))
	ENDDO
        return
        end
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Modelli entalpici elementi speciali
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!----------------------------------------------------------------------

	subroutine pipent(		! calcola la temperatura e Cv' 
					! all'uscita del tronco e i valori 
					! medi di Cp',d(dH)/dro 
	1		   ie,		! :in offset elemento
	1		   im,		! :in offset nodo monte
	1		   iv)		! :in offset nodo valle

*&*
        implicit none

	include '../inc/param.inc '
	include '../inc/staznw_par.inc '
	include '../inc/th.inc    '
	include '../inc/tj.inc '	!: in
	include '../inc/tz.inc '	!: in/out

*&*
        integer*4 ie,im,iv
        integer*4 oi,of
	  real*4  cp_new
*&*
	if(porttj(ie) .lt. -zeroTC) then
		oi = iv
		of = im
	else
		oi = im
		of = iv
	endif

	call deroh(ie,rocitj(of),touttz(ie),ctvctz(ie),
     -           ctpctz(ie),dhrotz(ie))

	dhmetz(ie) = (dhrotz(oi) + dhrotz(ie))/2.0

c-ace-bilancio termico
	call dHdT_ro(ie,rocitj(of),touttz(ie),cp_new)
c
c      cpmetz(ie) = (ctpctz(oi) + ctpctz(ie))/2.0

      cpmetz(ie) = cp_new
c-ace-bilancio termico
	return
	end	! PIPENT


!======================================================================


	real*4 FUNCTION  dttron(		! temperatura del gas all'uscita
					!	di un tronco
	1		  ie)		! :IN offset tronco
!	RETURN  temperatura
*&*
        implicit none

	INCLUDE '../inc/param.inc'		!:costante
	include '../inc/staznw_par.inc '
	INCLUDE '../inc/ti.inc'			!:IN
	INCLUDE '../inc/tj.inc'			!:INOUT
	INCLUDE '../inc/tz.inc'			!:INOUT
*&*
        integer*4 ie,oi,of
        real*4    tlim,rexp
*&*
!----------------------------------------------------------------------
!	flusso di energia
!----------------------------------------------------------------------

	if(porttj(ie) .gt. zeroTC) then
	  oi = opumto(ie)
	  of = opuvto(ie)
	elseif (porttj(ie) .lt. -zeroTC) then
	  oi = opuvto(ie)
	  of = opumto(ie)
	else
		dttron = tambtz(ie)
		return
	endif

      call coef_tutronc(ie,oi,of,tlim,rexp)
	dttron = tlim + (temptj(oi) - tlim)*exp(-rexp)

	RETURN


!======================================================================

cT	entry rjactron(		! Calcolo el. Jacobiano per la 
cT					! temperatura di uscita del tronco
cT	1			ie,	! :in offset tronco
cT	1			im,	! :in offset monte
cT	1			iv,	! :in offset valle
cT	1			dtmo,	! :out derivata T di monte
cT	1			dtva,	! :out		T di valle
cT	1			drom,	! :out		ro di monte
cT	1			drov,	! :out		ro di valle
cT	1			dpor)	! :out		portata
cT!----------------------------------------------------------------------
cT! 	Prima versione (vedi sopra)
cT!----------------------------------------------------------------------

cT	rjactron = - 1.0
cT	dhrox = dhmetz(ie)*(rocitj(iv) - rocitj(im))
cTC ccfact contiene la conversione di conntx da w/mK a Kcal/hmC
cTc-elena        ccfact = (conntx(ie)*rluntx(ie))/1162.0
cTc  in SIRE conntx ha il significato di coeff di conducibiltà termica e non
cTc  di coeff di scambio termico come in SSDD
cT      ccfact = conntx(ie)
cTc-elena
cT	dtldq = - (dhrox + depxtz(ie))/ccfact

cT	if(porttj(ie) .eq. 0.0) then
cT		dtmo = 0.0
cT		dtva = 0.0
cT		drom = 0.0
cT		drov = 0.0
cT		dpor = dtldq/ro0td(ie)
cT		return
cT	endif

cT	tlim = tambtz(ie) + porttj(ie)*dtldq
cT	rexp = ccfact/(abs(porttj(ie))*cpmetz(ie))
cT	dert = exp(-rexp)

cT	drom = porttj(ie)*dhmetz(ie)*(1.0 - dert)/ccfact
cT	drov = -drom

cT	if(porttj(ie) .ge. 0.0) then
cT		dtmo = dert
cT		dtva = 0.0
cT		dpor = ( rexp*(temptj(im) - tlim)*dert/porttj(ie)
cT	1		 + dtldq*(1.0 - dert) )/ro0td(ie)
cT	else
cT		dtmo = 0.0
cT		dtva = dert
cT		dpor = ( rexp*(temptj(iv) - tlim)*dert/porttj(ie)
cT	1		 + dtldq*(1.0 - dert) )/ro0td(ie)
cT	endif
cT
cT	return

	end	! dttron cT, jactron



!======================================================================


	subroutine dtvalv(		! calcola la temperatura e Cv'
					! a monte e/o a valle di una valvola
	1		   ie)		! :in offset elemento

*&*
        implicit none
	include '../inc/param.inc '
	include '../inc/th.inc '
	include '../inc/td.inc '
	include '../inc/tx.inc '
	include '../inc/tj.inc '
	include '../inc/tz.inc '
	include '../inc/ti.inc '
      INCLUDE '../inc/staznw_par.inc'
*&*
        integer*4 ie


        integer*4 iv
*&*
		call tout_valv(ie)
		call deroh(ie,rocitj(opuvto(ie)),touttz(ie),ctvctz(ie),
	1			ctpctz(ie),dhrotz(ie))

	return
!======================================================================

	entry	  dtuno(		! calcola la temperatura e Cv'
					! a valle di un el. unario
	1		   ie)		! :in offset elemento

      iv = opuvto(ie)
crsatemp
         if (porttj(ie) .lt. -zeroTC) then
              touttz(ie) = temptj(iv)
              if (diretj(ie) .eq. -1) ! per gli unari uscenti e portata negativa
	                               ! aggiorno set temperatura
     -            tsettj(ie) = touttz(ie)
         else                      !Per le portate non negative impongo set compos
              touttz(ie) = tsettj(ie)
	   endif
cT	touttz(ie) = tsettj(ie)
crsatemp
	call deroh(ie,rocitj(iv),touttz(ie),
     -              ctvctz(ie),ctpctz(ie),dhrotz(ie))

	return

	end    ! DTVALV, DTCENT, DTUNO

	subroutine tout_valv(		! calcola la temperatura e Cv'
					! a monte e/o a valle di una valvola
	1		   ie)		! :in offset elemento

*&*
        implicit none
	include '../inc/param.inc '
	include '../inc/th.inc '
	include '../inc/td.inc '
	include '../inc/tx.inc '
	include '../inc/tj.inc '
	include '../inc/tz.inc '
	include '../inc/ti.inc '
      INCLUDE '../inc/staznw_par.inc'
*&*
        integer*4 ie


        integer*4 im,iv
        real*4    dhm,dhv
        real*4    tuscita
cmar-tmin-29-06-09
    
       integer*4 err_ko, msgerr

cmar-tmin-29-06-09
*&*

cmar-tmin



    
	if(porttj(ie) .lt. -zeroTC) then
        im = opuvto(ie)
        iv = opumto(ie)
	else
        im = opumto(ie)
        iv = opuvto(ie)
	endif

		call deltah1(ie,rocitj(im),temptj(im),dhm)
		call deltah1(ie,rocitj(iv),touttz(ie),dhv)



		tuscita = temptj(im) + (dhm - dhv)/ctpctd(im)




      
		if(hstatj(ie) .eq. sta_on .or. 
	1		hstatj(ie) .eq. sta_fr) tuscita = temptj(im)




cT		if(tsettj(ie) .eq. R_UND .or. tuscita .gt. tsettj(ie)) then


		if(tsettx(ie) .eq. R_UND .or. tuscita .gt. tsettx(ie)) then
			touttz(ie) = tuscita
		else
cT			touttz(ie) = tsettj(ie)
			touttz(ie) = tsettx(ie)
		endif



	return
	end

cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	subroutine tout_valv_files(		! calcola la temperatura e Cv'
					! a monte e/o a valle di una valvola
	1		   ie)		! :in offset elemento

*&*
        implicit none
	include '../inc/param.inc '
	include '../inc/th.inc '
	include '../inc/td.inc '
	include '../inc/tx.inc '
	include '../inc/tj.inc '
	include '../inc/tz.inc '
	include '../inc/ti.inc '
      INCLUDE '../inc/staznw_par.inc'
*&*
        integer*4 ie


        integer*4 im,iv
        real*4    dhm,dhv
        real*4    tuscita
*&*

cmar-tmin

    
	if(porttj(ie) .lt. -zeroTC) then
        im = opuvto(ie)
        iv = opumto(ie)
	else
        im = opumto(ie)
        iv = opuvto(ie)
	endif

		call deltah1(ie,rocitj(im),temptj(im),dhm)
		call deltah1(ie,rocitj(iv),touttz(ie),dhv)




		tuscita = temptj(im) + (dhm - dhv)/ctpctd(im)


   

      
		if(hstatj(ie) .eq. sta_on .or. 
	1		hstatj(ie) .eq. sta_fr) tuscita = temptj(im)




cT		if(tsettj(ie) .eq. R_UND .or. tuscita .gt. tsettj(ie)) then

    

		if(tsettx(ie) .eq. R_UND .or. tuscita .gt. tsettx(ie)) then
			touttz(ie) = tuscita
		else
cT			touttz(ie) = tsettj(ie)
			touttz(ie) = tsettx(ie)
		endif




cmar-tmin
	return
	end


cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       subroutine tout_valv_ok_orig(		! calcola la temperatura e Cv'
					! a monte e/o a valle di una valvola
	1		   ie)		! :in offset elemento

*&*
        implicit none
	include '../inc/param.inc '
	include '../inc/th.inc '
	include '../inc/td.inc '
	include '../inc/tx.inc '
	include '../inc/tj.inc '
	include '../inc/tz.inc '
	include '../inc/ti.inc '
      INCLUDE '../inc/staznw_par.inc'
*&*
        integer*4 ie


        integer*4 im,iv
        real*4    dhm,dhv
        real*4    tuscita
*&*
	if(porttj(ie) .lt. -zeroTC) then
        im = opuvto(ie)
        iv = opumto(ie)
	else
        im = opumto(ie)
        iv = opuvto(ie)
	endif

		call deltah1(ie,rocitj(im),temptj(im),dhm)
		call deltah1(ie,rocitj(iv),touttz(ie),dhv)
		tuscita = temptj(im) + (dhm - dhv)/ctpctd(im)
		if(hstatj(ie) .eq. sta_on .or. 
	1		hstatj(ie) .eq. sta_fr) tuscita = temptj(im)
cT		if(tsettj(ie) .eq. R_UND .or. tuscita .gt. tsettj(ie)) then
		if(tsettx(ie) .eq. R_UND .or. tuscita .gt. tsettx(ie)) then
			touttz(ie) = tuscita
		else
cT			touttz(ie) = tsettj(ie)
			touttz(ie) = tsettx(ie)
		endif
	return
	end


cmar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!======================================================================
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      Subroutine dtcent_gen (ie)
*&*
!   Calcola la temperatura e Cv' a monte e a valle di una centrale
*&*
        implicit none
      include '../inc/param.inc '
      include '../inc/rk.inc '
      include '../inc/DEFAULT.inc '
      include '../inc/cc.inc '
      include '../inc/th.inc '
      include '../inc/td.inc '
      include '../inc/tx.inc '
      include '../inc/top_app_ele.inc '
      include '../inc/tj.inc '
      include '../inc/tz.inc '
      include '../inc/ti.inc '
      include '../inc/nw.inc '
      INCLUDE '../inc/stations.inc'
      INCLUDE '../inc/cap_bon_prv.inc'
      INCLUDE '../inc/staznw_par.inc'

*&*
      integer*4 ie                ! I) offset elemento

      integer*4 im
     -         ,iv
      real*4    dhm,dhv
      real*4    tuscita,pa,pm,xx,xeea
      real*4    prop
      external  prop
      logical*2 elem_bin
      external  elem_bin
      real*4    ro_in,ro_out
      real*4    delta_p,dp_in,dp_out
      external  dp_in,dp_out

CMAR
CMAR      real*4 comp_ratio
	integer*4 ier
!----------------------------------------------------------------------
c	if(porttj(ie) .lt. -zeroTC) then
c        im = opuvto(ie)
c        iv = opumto(ie)
c	else
        im = opumto(ie)
        iv = opuvto(ie)
c	endif

      if (porttj(ie) .gt. zeroTC) then
c calcolo della temperatura della centrale binaria con portata positiva (temptj(ie))
        if (hstatj(ie) .eq. sta_on .or. 
     -      hstatj(ie) .eq. sta_fr) then
cgpe-tout          temptj(ie) = temptj(im)
          touttz(ie) = temptj(im)
        else
          if (lcent_first) then

            delta_p = dp_in(ie,prestj(im),temptj(im),porttj(ie))
            pa = MAX(prestj(im)-delta_p,pmintx(im))

            delta_p = dp_out(ie,prestj(iv),temptj(iv),porttj(ie))
            pm = MAX(prestj(iv)+delta_p,pmintx(iv))
CMAR da controllare in debug!
         
	      comp_ratio= (pm/pa)
	call politrop_esp(ie,pa,temptj(im),comp_ratio,xx,ier)

CMAR            xx = prop(pa,temptj(im),exp_coef(1,ie))
            xeea = (xx-1.0)/xx
cgpe-tout            temptj(ie) = temptj(im)*
            touttz(ie) = temptj(im)*
     *                    (1.0 + ((pm/pa)**xeea -1.0)/recocc)
cgpe-tout            if (temptj(ie) .gt. tarctx(ie)) temptj(ie) =  tarctx(ie)
            if (touttz(ie) .gt. tarctx(ie)) touttz(ie) =  tarctx(ie)
c          else
c            if (htipto(ie).eq.e_ce) then
c              if (fl_ris_ce(pgrato(ie)) .ge. 0) then
cgpe-tout                temptj(ie) = tmand_stat(pgrato_app(ie))
c                touttz(ie) = tmand_stat(pgrato_app(ie))
c              endif
c            endif
          endif
        endif

cgpe-tout        touttz(ie) = temptj(ie)
cprv
c        if (temptj(ie) .gt. tarctx(ie)) touttz(ie) =  tarctx(ie)
cprv-end

        call deroh(ie,rocitj(iv),touttz(ie),ctvctz(ie),
     -       ctpctz(ie),dhrotz(ie))

      else
!-centrale binaria con portata negativa
cgpe-tout        temptj(ie) = temptj(iv)
cgpe-tout-corr        touttz(ie) = temptj(ie)
        touttz(ie) = temptj(iv)
cgpe-tout-corr-end
        ctvctz(ie) = ctvctz(iv)
      endif
c
      return ! dtcent_gen
      end
c
c*******************************************************************************
c
c	real*4 FUNCTION Qpwmce( ! Portata a potenza massima
c     *		ie)	! offset elemento
cc
c      implicit none
c      include '../inc/param.inc '
c      include '../inc/rk.inc '
c      include '../inc/cc.inc '
c      include '../inc/ti.inc '
c      include '../inc/tx.inc '
c      include '../inc/tj.inc '
c      include '../inc/top_app_ele.inc '

c	common/comp/comp_ratio_p
c	real*4 comp_ratio_p
c
cgpe      real*4 cesad
cgpe      external cesad
c
c      integer*4 ie,im,iv,k
c      real*4    pa,pm,x,xeea
c      real*4    prop
c      external  prop
CMAR
c     real*4 comp_ratio
c	integer*4 ier
c
c      im = opumto(ie)
c      iv = opuvto(ie)
c      k  = pgrato(ie)

c	pa= MAX( prestj(im)-dpin(k) ,pmintx(im))
c	pm= MAX( prestj(iv)+dpout(k) ,pmintx(iv))

cgpe	x = cesad (ie)
CMAR
c      
c      comp_ratio= (pm/pa)
c     comp_ratio_p= comp_ratio
c
c	call politrop_esp(ie,pa,temptj(im),comp_ratio,x,ier)
c
CMAR      x  = prop(pa,temptj(im),exp_coef(1,ie))
cgpe-end
c	xeea = (x-1.0)/x
c	qpwmce = powmax(k) /(cpwccc/xeea*zmcitj(im)*temptj(im)/rendtx(ie)
c     *	    *	((pm/pa)** xeea - 1.0) )
c	qpwmce = MAX(qpwmce,0.0)
c	RETURN 
c      end    ! qpwmce
!========================================================================
c	real*4 FUNCTION Ppwmce(! Pressione di mandata a potenza massima
c    *		ie)	! offset elemento
c

      

c      implicit none
c      include '../inc/param.inc '
c      include '../inc/rk.inc '
c      include '../inc/cc.inc '
c      include '../inc/ti.inc '
c      include '../inc/tx.inc '
c      include '../inc/tj.inc '
c      include '../inc/top_app_ele.inc '

cgpe      real*4   cesad
cgpe      external cesad

cmar 
c      common/comp/comp_ratio_p
c	real*4 comp_ratio_p, ier

c      integer*4 ie,im,iv,k
c      real*4    pa,pw1,x,xeea
c      real*4    prop
c      external  prop

c      im = opumto(ie)
c      iv = opuvto(ie)
c      k  = pgrato(ie)

c	pa	= MAX( prestj(im)-dpin(k) ,pmintx(im))
cgpe	x = cesad (ie)
CMAR devo andare in debug

c      call politrop_esp(ie,pa,temptj(im), comp_ratio_p, x,ier)


cmar      x  = prop(pa,temptj(im),exp_coef(1,ie))
cgpe-end
c	xeea  = (x-1.0)/x
c	pw1 = powmax(k)/(cpwccc/xeea * porttj(ie) * zmcitj(im) * 
c	1		temptj(im) / rendtx(ie) ) + 1.0

c	ppwmce = pa * pw1 ** (1.0/xeea) - dpout(k)

c	RETURN 
c	END   ! ppwmce
!

c
	real*4 FUNCTION Qpwmce( ! Portata a potenza massima
     *		ie)	! offset elemento
c
      implicit none
      include '../inc/param.inc '
      include '../inc/rk.inc '
      include '../inc/cc.inc '
      include '../inc/ti.inc '
      include '../inc/tx.inc '
      include '../inc/tj.inc '
      include '../inc/top_app_ele.inc '

	common/comp/comp_ratio_p
	real*4 comp_ratio_p

cgpe      real*4 cesad
cgpe      external cesad

      integer*4 ie,im,iv,k
      real*4    pa,pm,x,xeea
      real*4    prop
      external  prop, dp_in, dp_out
	real*4 dp_in, dp_out
CMAR
      real*4 comp_ratio
	integer*4 ier
c
      im = opumto(ie)
      iv = opuvto(ie)
      k  = pgrato(ie)
     
cmar-dp-cs
      if (htipto(ie).eq.e_cs) then
      dpin(k) = dp_in(ie,prestj(im),temptj(im),porttj(ie))
	dpout(k)= dp_out(ie,prestj(iv),temptj(iv),porttj(ie))
	endif
cmar-dp-cs


	pa= MAX( prestj(im)-dpin(k) ,pmintx(im))
	pm= MAX( prestj(iv)+dpout(k) ,pmintx(iv))

cgpe	x = cesad (ie)
CMAR
      
      comp_ratio= (pm/pa)
      comp_ratio_p= comp_ratio

	call politrop_esp(ie,pa,temptj(im),comp_ratio,x,ier)

CMAR      x  = prop(pa,temptj(im),exp_coef(1,ie))
cgpe-end
	xeea = (x-1.0)/x
	qpwmce = powmax(k) /(cpwccc/xeea*zmcitj(im)*temptj(im)/rendtx(ie)
     *	    *	((pm/pa)** xeea - 1.0) )
	qpwmce = MAX(qpwmce,0.0)
	RETURN 
      end    ! qpwmce
!========================================================================
	real*4 FUNCTION Ppwmce(! Pressione di mandata a potenza massima
     *		ie)	! offset elemento
c

      

      implicit none
      include '../inc/param.inc '
      include '../inc/rk.inc '
      include '../inc/cc.inc '
      include '../inc/ti.inc '
      include '../inc/tx.inc '
      include '../inc/tj.inc '
      include '../inc/top_app_ele.inc '

cgpe      real*4   cesad
cgpe      external cesad

cmar 
      common/comp/comp_ratio_p
	real*4 comp_ratio_p, ier

      integer*4 ie,im,iv,k
      real*4    pa,pw1,x,xeea
      real*4    prop
      external  prop


      external dp_in, dp_out
	real*4 dp_in, dp_out
      real*4 pm

      im = opumto(ie)
      iv = opuvto(ie)
      k  = pgrato(ie)

cmar-dp-cs
      if (htipto(ie).eq.e_cs) then
      dpin(k) = dp_in(ie,prestj(im),temptj(im),porttj(ie))
	dpout(k)= dp_out(ie,prestj(iv),temptj(iv),porttj(ie))
	endif
cmar-dp-cs

	pa	= MAX( prestj(im)-dpin(k) ,pmintx(im))
	pm  = MAX( prestj(iv)+dpout(k) ,pmintx(iv))


     
      comp_ratio_p= (pm/pa)


cgpe	x = cesad (ie)
CMAR devo andare in debug

      call politrop_esp(ie,pa,temptj(im), comp_ratio_p, x,ier)


cmar      x  = prop(pa,temptj(im),exp_coef(1,ie))
cgpe-end
	xeea  = (x-1.0)/x
	pw1 = powmax(k)/(cpwccc/xeea * porttj(ie) * zmcitj(im) * 
	1		temptj(im) / rendtx(ie) ) + 1.0

	ppwmce = pa * pw1 ** (1.0/xeea) - dpout(k)

	RETURN 
	END   ! ppwmce

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine agg_centrale (istaz)
c
!   Aggiorna i coefficienti z_coef ed ezp_coef per la centrale
!   in quanto dipendenti dalla composizione
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/rk.inc'
      include '../inc/cap_bon_prv.inc'
c
      INTEGER*4  istaz   ! I) indice della stazione nella struttura dati della rete
c
      INTEGER*4  vert_ind,vert_ind_app,iz
      real*4     pmin,pmax,tmin,tmax
      logical*2  ce_serie
      external   ce_serie
c-----------------------------------------------------------------------
      vert_ind = pgrato(istaz)

cint_z questi limiti vanno forse rivisti (ma considerando che la z forse
cint_z serve solo per l'input)
cint_z      pmin=20.
cint_z      pmax=120.
      pmin=5.
      pmax=300.

cint_z-end
      tmin=260.
      tmax=360.
cgpe-corr
      if (htipto(istaz).eq.E_CE) then
cgpe-corr-end
        call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef(1,vert_ind))
        if (ce_serie(istaz)) then
          vert_ind_app = pgrato_app(istaz)
          do iz = 1,6
            z_coef(iz,vert_ind_app) = z_coef(iz,vert_ind)
          enddo
        endif
cgpe-corr
      else if (htipto(istaz).eq.E_CS) then
c cosa fare???

      endif !(htipto(istaz).eq.E_CE)
cgpe-corr-end
cace
      call regre_dh(istaz)
cace-end
      call regre_exp(istaz)

      return
      end
!----------------------------------------------------------------------
c
