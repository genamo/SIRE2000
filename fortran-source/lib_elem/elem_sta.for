c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Modelli statici
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!======================================================================
      subroutine Qconce(   ! consumo di una centrale
     *		port,        ! :IN portata	
     *		pmon,        ! :IN pressione monte	
     *		pval,        ! :IN pressione valle	
     *		ie,		     ! :IN offset centrale
     *		conce,		 ! :OUT consumo centrale  kNm3/h
     *		dqconce,	 ! :OUT derivata rispetto a Q del consumo centrale
     *		dpasp2conce, ! :OUT derivata rispetto a Pasp**2 del consumo centrale
     *		dpman2conce) ! :OUT derivata rispetto a Pman**2 consumo centrale
*&*
      implicit none
	INCLUDE '../inc/param.inc'	! :costante
      INCLUDE '../inc/default.inc'     ! :costante
	INCLUDE '../inc/CC.inc'		! :IN
	INCLUDE '../inc/TD.inc'		! :IN
	INCLUDE '../inc/TX.inc'		! :IN
	INCLUDE '../inc/TH.inc'		! :IN
	INCLUDE '../inc/top_app_ele.inc'		! :IN
	INCLUDE '../inc/tj.inc'		! :IN
	INCLUDE '../inc/ti.inc'
	INCLUDE '../inc/rk.inc'
	INCLUDE '../inc/stations.inc'
	INCLUDE '../inc/stazione.inc'
	INCLUDE '../inc/cap_bon_prv.inc'

        real*4    port,pmon,pval
        integer*4 ie
        real*4    conce,dqconce,dpasp2conce,dpman2conce

        integer*4 im,iv
        real*4    pasp,pman,pasp_min,pman_min
        real*4    xe,xeea,coefq,rap
        real*4    prop             ! Function
        logical*2 elem_bin          ! Function
      real*4      delta_p,dp_in,dp_out
      external    dp_in,dp_out
CMAR
CMAR      real*4 comp_ratio
CMAR_2
      real*4 comp_ratio_CS
CMAR_2
	integer*4 ier
*&*
!----------------------------------------------------------------------
      conce       = 0.
      dqconce     = 0.
      dpasp2conce = 0.
      dpman2conce = 0.
      if (pmon.gt.pval) return
      if (hstatj(ie) .eq. sta_on .or. hstatj(ie) .eq. sta_fr) return
	if (pmon .lt. pmin_def) return
	if (pval .lt. pmin_def) return
!	if (abs(pmon - pval) .lt. dp_off) return

	im = opumto(ie)
	iv = opuvto(ie)
! centrale binaria o centrale semplificata
      pasp = pmon
      pman = pval
      pasp_min = pmintx(im)
      pman_min = pmintx(iv)

      delta_p = dp_in(ie,pasp,temptj(im),port)
      pasp = MAX(pasp-delta_p,pasp_min)

      delta_p = dp_out(ie,pman,temptj(iv),port)
      pman = MAX(pman+delta_p,pman_min)

CMAR 

CMAR_2
CMAR_2      IF(htipto(ie) .eq. e_cs)then


      comp_ratio_CS = (pman/pasp)
	call politrop_esp(ie,pasp,temptj(im),comp_ratio_CS,xe,ier)


CMAR_2	ELSE

CMAR_2      comp_ratio = (pman/pasp)
CMAR_2	call politrop_esp(ie,pasp,temptj(im),comp_ratio,xe,ier)
	

CMAR_2	ENDIF
CMAR_2

CMAR	xe = prop(pasp,temptj(im),exp_coef(1,ie))

      xeea  = (xe-1.0)/xe

c il vecchio procedimento (viene lasciato per verificare la dimensionalità)
C è stato modificato per facilitare il calcolo delle derivate 
C ma il risultato è lo stesso 
c      power = cpwccc/xeea*port*zmcitj(im)*temptj(im)/rendtx(ie) 
c     -            *((pman/pasp)**xeea - 1.0)                            [MW]
c      power = MAX(power,0.0)

c      qconce = ceuptx(ie) * power /pclitd(ie)*3600.0	! kNm3/h
c      qconce = ceuptx(ie) * power /pclitd(ie)*3600.0	! kNm3/h
c     [kNm3/h] = [kcal/kJ] * [MW] / [kcal/Nm3]
c      coefq = cpwccc/xeea*zmcitj(im)*temptj(im)
c     *        /rendtx(ie)*ceuptx(ie)/(pclitd(ie)/4186)*3600.0
c  CON LE NUOVE UDM (interne) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      qconce = ceuptx(ie) * power /pclitd(ie)	! kNm3/h
c     [kNm3/h] = [kJ/kWh] * [kW] / [kJ/kNm3]
      coefq = cpwccc/xeea*zmcitj(im)*temptj(im)
     *        /rendtx(ie)*ceuptx(ie)/pclitd(ie)

      rap = (pman/pasp)**xeea
	
	conce       = coefq*port*(rap - 1.0)
cgpe
      conce = MAX(conce,0.0)
cgpe-end

      dqconce     = coefq*(rap - 1.0)
	dpasp2conce = xeea*coefq*port*rap/(2.0*pasp*pasp)
	dpman2conce = xeea*coefq*port*rap/(2.0*pman*pman)


c  le derivate rispetto al quadrato delle pressioni 
c  venivano calcolate tenendo conto anche delle cadute del piping.
c  Non riuscendo a travare una spiegazione matematica e 
c  dato la modesta entità delle stesse, si è ritenuto di trascurare questa dipendenza.
c  Per i posteri si lascia traccia del vecchio calcolo:
c	dpasp2conce = xeea*coefq*port*rap/(2.0*pasp*pasp0)
c	dpman2conce = xeea*coefq*port*rap/(2.0*pman*pman0)
c  dove pasp0 e pman0 sono le pressioni prima di introdurre le "stat_vars" corrispondenti.


      RETURN
      END
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
