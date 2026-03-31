!======================================================================
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Modelli dinamici
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!-----------------------------------------------------------------------
!========================================================================
      REAL*4 FUNCTION QPUNT_new (	!portata di un prelievo
     -  tempo ! :in istante attuale 
     - ,dt    ! :in passo integrazione
     - ,ie)   ! :in offset punto
*&*
      implicit none      
      INCLUDE '../inc/param.inc'
      INCLUDE '../inc/default.inc'
      INCLUDE '../inc/dimension.inc'
      INCLUDE '../inc/td.inc'
      INCLUDE '../inc/tq.inc'
      INCLUDE '../inc/tx.inc'
      INCLUDE '../inc/ti.inc'
      INCLUDE '../inc/tj.inc'	! :OUT

      real*4 tempo,dt
      integer*4 ie
*&*
      real*4 qq,qm,ht
      integer*4 pos ,pos_mod
!----------------------------------------------------------------------
!		portata richiesta dall`utente attraverso le modulazioni
!----------------------------------------------------------------------
      pos=pgrato(ie)

	pos_mod= int(tempo - 
     -        (int(tempo/PRL_PERIODO)*PRL_PERIODO)) + 1

      ht = mod(tempo,PRL_PERIODO)

      qq= qsp3tq(pos_mod,pos)*ht*ht
     -  + qsp2tq(pos_mod,pos)*ht
     -  + qsp1tq(pos_mod,pos)


!----------------------------------------------------------------------
!		qm = QRPRE(dt,ie)	     !stima della portata max.
					             !  prelevabile
!----------------------------------------------------------------------
!		portata effettiva del prelievo
!----------------------------------------------------------------------
crsa_direz
!	IF    (qq.LE.0.0 .OR. prestj(ie).LE.pmintx(ie))  THEN ! prelievo chiuso
	IF    (prestj(ie).LT.pmintx(ie))  THEN ! prelievo chiuso
			QPUNT_new	   = 0.0
!	ELSEIF (qq.GT.qm)	  THEN		! prelievo ridotto
!			QPUNT_new      = qm
	ELSE   !(0.0 < qq < qm)			!prelievo normale
crsa_pcls_din
			QPUNT_new	   = qq*pcls_def/pclstd(ie)
	ENDIF
	RETURN	!qpunt
	END

!======================================================================
      REAL*4 FUNCTION	Qelem_sp( 	! portata di un elemento speciale
     2        tempo,  ! :IN istante attuale + passo integrazione
     3        ie)     ! :in offset elemento
*&*
        implicit none
	INCLUDE '../inc/param.inc'
	INCLUDE '../inc/default.inc'	! :costante
	INCLUDE '../inc/rampa.inc'	! :costante
	INCLUDE '../inc/th.inc'
	INCLUDE '../inc/tx.inc'
	INCLUDE '../inc/ti.inc'
	INCLUDE '../inc/td.inc'
	INCLUDE '../inc/tj.inc'    ! :OUT
!	INCLUDE '../inc/tjt.inc'    ! :OUT
	INCLUDE '../inc/ric.inc'

        real*4 tempo
        integer*4 ie

      real*4 qric,qmaxapp
      real*4 pmin,pmax

      real*4 qset_rampa    ! Function
	logical*2 flag_vlim
	logical*2 elem_uni,elem_bin,reg_nr  ! Function
	real*4 qset_real
	external qset_real
*&*
!----------------------------------------------------------------------
!----------------------------------------------------------------------
      if (htipto(ie).eq.e_vg) call qmaxvg(ie,qmaxtx(ie),flag_vlim)
!----------------------------------------------------------------------
!      call rientro(tempo,ie)
!----------------------------------------------------------------------
!	telecomandi di stato
!----------------------------------------------------------------------
!chsta       IF  (htcitj(ie).EQ.tc_q .or. htcitj(ie).EQ.tc_pq)   THEN	! portata
      IF  (hstatj(ie).EQ.sta_q)   THEN	! portata
          qric = qset_rampa(tempo,ie)
		qric = sign(qric,diretj(ie))
          rc_peso_ctlm(ie) = rc_weit_q
!----------------------------------------------------------------------
!chsta 	ELSEIF   (htcitj(ie) .EQ. tc_off )  THEN    
 	ELSEIF   (hstatj(ie) .EQ. sta_off )  THEN    
          qric = qset_rampa(tempo,ie)
		qric = sign(qric,diretj(ie))
c	 	qric = 0.0
          rc_peso_ctlm(ie) = rc_weit_q
c          rc_peso_ctlm(ie) = rc_weit_q*10
!----------------------------------------------------------------------
!----------------------------------------------------------------------
	ELSE                                   ! tutti i set
	    qric = porttj(ie)
          rc_peso_ctlm(ie) = rc_weit_q/100
	ENDIF ! tipo regolazione
      
!----------------------------------------------------------------------
! Imposizione Vincoli
!----------------------------------------------------------------------
crsa_pcls_din
      if (htcitj(ie).eq.tc_pq) then
cqpcls        qmaxapp = qsettj(ie)*pcls_def/pclstd(ie)
        qmaxapp = min(qset_real(ie),qmaxtx(ie))
	else
cqpcls        qmaxapp = qmaxtx(ie)*pcls_def/pclstd(ie)
        qmaxapp = qmaxtx(ie)
	endif
      if (abs(porttj(ie)).gt.qmaxapp) then
	    qric = sign(qmaxapp,porttj(ie))
          rc_peso_ctlm(ie) = rc_weit_q
c          rc_peso_ctlm(ie) = rc_weit_q*10
!          hstatjt(ie)=sta_qm
      elseif (reg_nr(ie) .and.
     _        sign(1.0,diretj(ie)).ne.sign(1.0,qric)) then !inversione di direzione
	    qric = 0.0
          rc_peso_ctlm(ie) = rc_weit_q*10
!          hstatjt(ie)=sta_nr
	endif
	if (elem_uni(ie)) then
	  pmin = max (pmintx(opuvto(ie)),pmintx(ie))
	  pmax = min (pmaxtx(opuvto(ie)),pmaxtx(ie))
	else
	  pmin = pmintx(opuvto(ie))
	  pmax = pmaxtx(opuvto(ie))
	endif
      if (prestj(opuvto(ie)).lt.pmin) then
	    qric = porttj(ie)
          rc_peso_ctlm(ie) = rc_weit_q/100
!          hstatjt(ie)=sta_pvmin
      elseif (prestj(opuvto(ie)).gt.pmax) then
	    qric = porttj(ie)
          rc_peso_ctlm(ie) = rc_weit_q/100
!          hstatjt(ie)=sta_pvmax
      elseif (elem_bin(ie)) then
        if (prestj(opumto(ie)).lt.pmintx(opumto(ie))) then
	      qric = porttj(ie)
            rc_peso_ctlm(ie) = rc_weit_q/100
!            hstatjt(ie)=sta_pmmin
        elseif (prestj(opumto(ie)).gt.pmaxtx(opumto(ie))) then
	      qric = porttj(ie)
            rc_peso_ctlm(ie) = rc_weit_q/100
!            hstatjt(ie)=sta_pmmax
        endif
	endif

      Qelem_sp = qric

      RETURN
      END
!======================================================================
!======================================================================
      REAL*4 FUNCTION	QSET_rampa ( ! portata da settare in funzione della rampa
     -  tempo       ! :IN istante attuale + passo integrazione
     - ,ie)         ! :in offset elemento
*&*
        implicit none
	INCLUDE '../inc/param.inc'
	INCLUDE '../inc/default.inc'
	INCLUDE '../inc/tj.inc'
	INCLUDE '../inc/tx.inc'
	INCLUDE '../inc/td.inc'
	INCLUDE '../inc/rampa.inc'
	INCLUDE '../inc/Ti.inc'	!:IN

        real*4 tempo
        integer*4 ie

      real*4 dqric,dt,dt_ute
c	real*4 qmaxapp
	logical*2 flag_vlim
	real*4 qset_real
	external qset_real

*&*
      dt = max(tempo - tempo_man(ie),0.)
      dt_ute = tfintj(ie) - tempo_man(ie)
      if (dt_ute.gt.dt) then
        dqric = delta_man(ie)*dt/dt_ute
      else
        dqric = delta_man(ie)
	endif
c	if (htipto(ie).eq.e_vg) call qmaxvg(ie,qmaxtx(ie),flag_vlim)
crsa_pcls_din
c      qmaxapp = qmaxtx(ie)*pcls_def/pclstd(ie)
c      dqric = dqric/qmaxpcsreal*100

c      if (abs(dqric).gt. qperora*dt) then
c         dqric = sign(qperora*dt,dqric)
c	endif
c	dqric = dqric * qmaxpcsreal / 100
crsa_pcls_din
      QSET_rampa = qset_real(ie) - delta_man(ie)+dqric

      RETURN
      END
!======================================================================
      REAL*4 FUNCTION	PSET_rampa ( ! pressione da settare in funzione della rampa
     -  tempo       ! :IN istante attuale + passo integrazione
     - ,iedir)      ! :in offset elemento
*&*
        implicit none
	INCLUDE '../inc/param.inc'
	INCLUDE '../inc/tj.inc'
	INCLUDE '../inc/tx.inc'
	INCLUDE '../inc/rampa.inc'

        real*4 tempo
        integer*4 iedir

      real*4 dpric, dt, dt_ute
      integer*4 ie


*&*
      ie = abs(iedir)
      dt = max(tempo - tempo_man(ie),0.)
      dt_ute = tfintj(ie) - tempo_man(ie)
      if (dt_ute.gt.dt) then
        dpric = delta_man(ie)*dt/dt_ute
      else
        dpric = delta_man(ie)
	endif
c      if (abs(dpric).gt. pperora*dt) then
c         dpric = sign(pperora*dt,dpric)
c	endif
      if (iedir.gt.0) then
        PSET_rampa = pmsetj(ie) - delta_man(ie) + dpric
      else
        PSET_rampa = pvsetj(ie) - delta_man(ie) + dpric
      endif

      RETURN
      END
!======================================================================
!======================================================================
      subroutine	rientro(	! Determina il rientro nel set imposto
     -		tempo,		! :IN tempo dinamica
     -		ie)  		! :IN offset elemento speciale
*&*
        implicit none
	INCLUDE '../inc/param.inc'	! :costante
	INCLUDE '../inc/rampa.inc'	! :costante
	INCLUDE '../inc/default.inc'	! :costante
	INCLUDE '../inc/th.inc'
	INCLUDE '../inc/tx.inc'
	INCLUDE '../inc/ti.inc'
	INCLUDE '../inc/tj.inc'
	INCLUDE '../inc/td.inc'
	INCLUDE '../inc/staznw_par.inc'



	  real*4 tempo
        integer*4 ie


      logical*2 fl_rientro
	real*4 v_app
      integer*4 i_pv,i_pm, hsta_imp

      logical*2 elem_vlv
      external  elem_vlv
	real*4 qset_real
	external qset_real


*&*
      hsta_imp = stadatc(htcitj(ie))
	if (elem_vlv(ie)) then
             if ((prestj(opuvto(ie))-prestj(opumto(ie))).gt.repsP) then
               if (htcitj(ie).eq.tc_npv) then
  	           hsta_imp=sta_on
	         elseif (htcitj(ie).eq.tc_pv) then
  	           hsta_imp=sta_fr
	         endif
		   endif
      endif


      if (hstatj(ie).eq.hsta_imp) return
	  
      fl_rientro = .false.

      i_pv = opuvto(ie)
      i_pm = opumto(ie)
      if (hstatj(ie) .EQ. sta_pvmin) then
	   v_app = pmintx(i_pv)
	   v_app = v_app + max(v_app*r_fsc_p,a_fsc_p)
	   if (prestj(i_pv).gt.v_app)  THEN
           fl_rientro = .true.
         endif  
      elseif (hstatj(ie) .EQ. sta_pvmax) then
	   v_app = pmaxtx(i_pv)
	   v_app = v_app - max(v_app*r_fsc_p,a_fsc_p)
	   if (prestj(i_pv).lt.v_app)  THEN
           fl_rientro = .true.
         endif  
      elseif (hstatj(ie) .EQ. sta_pmmin) then
	   v_app = pmintx(i_pm)
	   v_app = v_app + max(v_app*r_fsc_p,a_fsc_p)
	   if (prestj(i_pm).gt.v_app)  THEN
           fl_rientro = .true.
         endif  
      elseif (hstatj(ie) .EQ. sta_pmmax) then
	   v_app = pmaxtx(i_pm)
	   v_app = v_app - max(v_app*r_fsc_p,a_fsc_p)
	   if (prestj(i_pm).lt.v_app)  THEN
           fl_rientro = .true.
         endif  
      elseif (hstatj(ie) .EQ. sta_qm) then
crsa_pcls_din
cqpcls	   v_app = qmaxtx(ie)*pcls_def/pclstd(ie)
	   v_app = qmaxtx(ie)
	   v_app = v_app - max(v_app*r_fsc_q,a_fsc_q)
	   if (porttj(ie).lt.v_app)  THEN
           fl_rientro = .true.
         endif  
      elseif (hstatj(ie) .EQ. sta_nr) then
	   if (sign(1.0,diretj(ie)).EQ.sign(1.0,porttj(ie))) then !inversione rientrata
           v_app = 0
	     v_app = v_app + max(v_app*r_fsc_q,a_fsc_q)
	     if (abs(porttj(ie)).gt.v_app)  THEN
             fl_rientro = .true.
           endif  
         endif  
	else
c??             fl_rientro = .true.
	endif

      if (fl_rientro) then
	  hstatj(ie) = hsta_imp
	  if (hstatj(ie).EQ.sta_q) then
          tempo_man(ie) = tempo
          delta_man(ie) = qset_real(ie)-porttj(ie)
	  elseif (hstatj(ie).EQ.sta_pv) then
          tempo_man(i_pv) = tempo
          delta_man(i_pv) = pvsetj(ie)-prestj(i_pv)
	  elseif (hstatj(ie).EQ.sta_pm) then
          tempo_man(i_pm) = tempo
          delta_man(i_pm) = pmsetj(ie)-prestj(i_pm)
	  endif
	endif

	RETURN
	END
