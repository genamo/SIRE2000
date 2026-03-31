!----------------------------------------------------------------------
!
!	Simulazione Dinamica della RETE con
!           Algoritmo Stiff di GEAR
!       e soluzione ai minimi quadrati mediante
!           Algoritmo di Levember&Marquard
!
!	SUB S_DINAMICA(....)	! Subroutine della dinamica
!
!----------------------------------------------------------------------

	SUBROUTINE S_DINAMICA(	!controlla il periodo di simulazione
					        !  e integra in tutto l`intervallo indicato
     -		*)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!					condizioni iniziali in TJ
*&*--------------------------------------------------------------------
        implicit none
	  include '../inc/param.inc'
        include '../inc/din_ute.inc'
        include '../inc/ric.inc'
        include '../inc/SCENARIO.inc'


! Dimensionamenti necessari alle routine di Gear e Marquard
        INTEGER*4 idmwk
        parameter(idmwk=3*RC_M_VAR+(RC_M_ORD+1)*RC_M_VAR) !Dimensione WK (GEAR)

!        integer*4 idmax_pseudo
!        parameter (idmax_pseudo=5*PS_M_VAR+PS_M_EQU+2*PS_M_JAC)
!        integer*4 idmax_att_check
!inizio batch
!        parameter (idmax_att_check=2*PS_M_VAR+2*PS_M_EQU+6*PS_M_EQU)
!        parameter (idmax_att_check=2)
!fine batch
!        integer*4 idmax
!        parameter (idmax=max(idmax_pseudo,idmax_att_check))
        integer*4 idmax,ncount
        parameter (idmax=1)
        INTEGER*4 idm_ric_mrq
        parameter (idm_ric_mrq=5*RC_M_VAR+2*RC_M_JAC) 
                                         !Dimensione WKA IWKA (MARQUARD)

        integer*4 idmwka
        parameter (idmwka=max(idm_ric_mrq,idmax))

        REAL*4    wk(idmwk), wka(idmwka)
        INTEGER*4 iwka(idmwka)

!        REAL*4    T_INI,T_FINI
!        REAL*4    tot_time /0.0/

        INTEGER*4 nn,nd,nf,ni,index,kflag
        integer*4 JSTART

!	AREE TEMPORANEE: (per ciascuna variabile indipendente)
	REAL*4	yy(RC_M_VAR), yy_old(RC_M_VAR)

      REAL*4 
     -       t_d_cor
     -      ,t_cor
     -      ,t_out
     -      ,t_tlc
     -      ,t_prl
     -      ,t_cor_old
     -      ,t_d_max
     -      ,t_d_usato

      REAL*4 DELTA_TIME, ZERO_TIME
      REAL*4 PREC,BASE,sqrprec
      logical*2 fl_interrupt
      logical*2 fl_timemod

      logical*2 time_mod
	external time_mod
*&*

!======================================================================
!	INIZIALIZZAZIONE DATI AREA COMMON
!--------------------------------------------------
!======================================================================
      o_sta_ric = O_ST_NVSCEN

      f_tcon = 1             ! Impone stato ON sugli elementi speciali
                             ! (0:NO, 1:SI)
      f_pimpo = 1            ! Impone pressione sui punti
                             ! (0:NO, 1:SI)
      f_controllo = 2        ! Controlli da effettuare in successione
                             ! (0:Nessuno, 1:Bilanci, 2:Fergusson, 
						   !  3:pmon=pval, 4:condizioni imposte)
      f_densita = 0          ! Grandezza da considerare nelle equazioni 
                             ! (0:Portata, 1:Densità)
      f_residuo = 0          ! Controllo residuo 
                             ! (0:NO, 1:SI)
      fl_interrupt = .FALSE.


     	t_d_out = 0.1  
      t_ini   = 0.0
      t_fin   = 3.0 
      usa_prl = .FALSE.
      usa_tlc = .FALSE.
      usa_tlcic = .FALSE.
!======================================================================
!	INIZIALIZZAZIONE DATI UTENTI
!--------------------------------------------------
!======================================================================
c      integer*4 fl_tlc      !0=No telecomandi,1=Si telecomandi       (Dinamica)
c      integer*4 tipo_tlc    !0=No ciclici,1=Si ciclici (se fl_tlc=1) (Dinamica)
c      integer*4 fl_modul    !0=No modulazioni,1=Si modulazioni       (Dinamica)
c      integer*4 ultimo_camp !Ultimo campionamento                    (Dinamica)

c      real*4 ist_iniz       !Istante iniziale       (Dinamica)
c      real*4 ist_fine       !Istante finale         (Dinamica) 
c      real*4 passo_camp     !Passo di campionamento (Dinamica)
      t_d_out = passo_camp  
      t_ini   = ist_iniz
      t_fin   = ist_fine
      usa_prl   = .FALSE.
      usa_tlc   = .FALSE.
      usa_tlcic = .FALSE.
      if (fl_modul.eq.1) usa_prl   = .TRUE.
      if (fl_tlc.eq.1)   usa_tlc   = .TRUE.
      if (tipo_tlc.eq.1) usa_tlcic = .TRUE.

!======================================================================
      call time_ini (t_ini,t_fin)

      call copia_TJ_su_TJT()

        ZERO_TIME = secnds(0.0)
        call zerom(prec,base)
	  sqrprec = sqrt(prec)

!        call agg_tc(*9999)          ! Aggiornamento temperatura e comp.

!======================================================================
!	INIZIALIZZAZIONE WORKING AREA
!--------------------------------------------------
!======================================================================
      t_d_cor = t_d_passo_min*10
	t_cor = t_ini


!----------------------
! Se istante zero (stazionaria) interessa nel calcolo 
!                  delle grandezze tipiche della dinamica (medie etc.)
      if  (t_ini.eq.0) then 
         t_out = t_ini
      else
         t_out = t_ini + t_d_out
      endif
!  altrimenti
!      t_out = t_ini + t_d_out
!----------------------
	if (abs(t_out-t_fin) .LT. t_passo_input) t_out = t_fin


c	call  ini_prl_elem()

      if (usa_prl) then
c        call skip_prl(t_ini,t_prl,*9999)
c Tutti i coef. di modulazione sono in memoria
        t_prl = R_MAX
      else
        t_prl = R_MAX
      endif

      if (usa_tlc) then
        call skip_tlc(t_ini,t_tlc,*9999)
      else
        t_tlc = R_MAX
      endif

!--------------------------------------------------------------------------
!	valori iniziali delle portate e della derivata della massa
!--------------------------------------------------------------------------
c      CALL iniziatj()
!      CALL sd_sta_init()   !
! Attenzione: il residuo Fergusson deve essere NULLO
cace_velocità_rete
       call out_TIMCOST() 
cace_velocità_rete

	CALL DIN_inizsta(yy,nd,nf,ni)
      nn = nd+nf+ni

!======================================================================
      t_cor_old = -1
  	jstart = 0			! richiesta operazioni preliminari

1111  continue
      fl_timemod = .false.
	DO WHILE (t_fin-t_cor .GT. t_d_passo_min .and. 
     -          .not.fl_interrupt .and. .not.fl_timemod)

       
	if (t_cor.gt.t_cor_old) then
     	  t_cor_old   = t_cor
        call agg_out(t_cor,t_out)
c_prl_mem        if (usa_prl) call agg_prl(t_cor,t_prl,*9999)
        if (usa_tlc) call agg_tlc(t_cor,t_tlc,*9999)
        t_d_max = min(t_fin,t_out,t_tlc,t_prl) - t_cor
        t_d_cor = min(t_d_cor,t_d_max,t_d_passo_max)
      endif
        if (jstart.eq.0) then
c ---      first call
          index = 1
        elseif (jstart.eq.1) then
c---       continuation
          index = 3
        elseif (jstart.eq.-1) then
c--- ripeti ultimo step
          index = 1
        endif

crsa_agg_tjt
!        CALL din_agg_tjt_mod  (t_cor+t_d_cor,t_d_cor)
        ncount=ncount+1
        CALL copia_yy (yy,yy_old)

	  CALL GEARSP(nn,nd,nf,
     *              t_cor,t_d_cor,t_fin,t_d_max, 
     *              yy,index,
     *              wk,idmwk,iwka,wka,idmwka,RC_M_JAC,
     *              prec,tol_gear,kflag)


c ATTENZIONE: La gestione di kflag (chiamato IER dentro il GEAR) deve essere
c             ancora fatta. I codici di errori definiti dentro la GEARSP
c             dovranno essere in accordo con la gestione effettuata alla

	  IF(kflag.lt.0)	GOTO 7777	! errori di integrazione

!rsa        call agg_tc(*9999)          ! Aggiornamento temperatura e comp.

!  Controllo interrupt utente
        call interrupt_utente (fl_interrupt)

        if (fl_interrupt) then
          call gest_error(2,0,'DINAMICA',
     *            'Interrotto da utente',0)
        endif

        t_d_usato = t_cor - t_cor_old

	  IF(.not. fl_interrupt .and.
     -     kflag .gt. 0 .and. kflag .ne. 47
     -    .and. t_d_usato .gt.t_d_passo_min) then
          jstart = -1
          t_d_cor = t_d_passo_min
          t_cor = t_cor_old
          call gest_error(2,0,'DINAMICA',
     -                  'Riparte ...',kflag)
        ELSE

!          call agg_port_pcls_def()
	    if (t_d_usato.gt.t_d_passo_min+sqrprec) then
            call din_check_elem(kflag)
	    else
            call din_check_ko_elem(kflag)
	    endif
!          call agg_port_pcls_cor()
          if (kflag.eq.0) then
		  CALL agg_derivati (yy)
		  jstart = 1			 ! per le call successive
		elseif (kflag.lt.0) then
		  if (t_d_usato.gt.t_d_passo_min+sqrprec) then
               jstart = -1
               t_d_cor = t_d_passo_min
               t_cor = t_cor_old
               CALL copia_yy (yy_old,yy)
               CALL rec_tj_rom (yy)
            else
              fl_interrupt = .true.
            endif
          else  
            jstart = -1
            t_d_cor = t_d_passo_min
            t_cor = t_cor_old
            CALL copia_yy (yy_old,yy)
            CALL rec_tj_rom (yy)
          endif
        ENDIF


!        DELTA_TIME = secnds(ZERO_TIME)
!        DELTA_TIME = 0           !temporaneo per il debug
!        if (DELTA_TIME.gt.max_delta_time) then
!          fl_interrupt = .TRUE.
!          call gest_error(2,0,'DINAMICA',
!     *            'Superato tempo massimo',0)
!        endif

      if (.not.fl_interrupt) then
  	  if (time_mod(t_cor)) then
	    if (t_d_passo_min*10.lt.t_passo_input) then
	      fl_timemod = .True.
	      t_d_passo_min = t_d_passo_min*10
            call mess_param(1)
          endif
        endif
      endif

	ENDDO !(T_COR < T_FIN)
	if (fl_timemod) goto 1111
!======================================================================
2222  continue
      if (t_fin-t_cor .LE. t_d_passo_min) t_cor = t_fin
	t_out = t_cor          ! scrive gli ultimi dati calcolati
      call agg_out(t_cor,t_out)

      RETURN
!--------------------------------------------------
!	ERROR
!--------------------------------------------------
7777	CONTINUE			! errori di integrazione
        call gest_error(1,0,'DINAMICA',
     *              'Dal Gear',kflag)
        goto 9999
8888    continue
        goto 9999

9999    continue
        RETURN 1
!--------------------------------------------------
333	FORMAT(1 x,i6,'  T=',f8.3,'  dt=',f7.4,'  ord=',i1
	1		,' n.f.=',i6,'  n.j.=',i5,' Err=',i3)
	END    ! S_DINAMICA

!======================================================================
	SUBROUTINE DIN_inizsta(		! inizializza l'area di lavoro
					! dell' integrazione (vettore di stato)
	4			yy,	! :OUT valori iniziali in yy(*)
	5		    np,	! :OUT n. punti
	5		    nt,	! :OUT n. tronchi
	5		    ns)	! :OUT n. el.spec.
*&*--------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/TH.inc'		!:IN
	INCLUDE '../inc/TI.inc'		!:IN
	INCLUDE '../inc/TD.inc'		!:IN
	INCLUDE '../inc/TJ.inc'		!:IN
	INCLUDE '../inc/RIC.inc'		!:IN
	INCLUDE '../inc/top_app_ele.inc'	!:INOUT

        integer*4 np,nt,ns
	real*4	yy(*)

        integer*4 j,ie,it
        integer*4 im,iv
        integer*4 ne_spe_prv
*&*
!--------------------------------------------------
!	inizializzazione di Y partendo dai TJ:
!     densita' (punti_cap+punti_nocap);
!     portate ponderali (tronchi + el.spec)
!--------------------------------------------------
	j=0
	DO ie= otipti(E_PU),etipti(E_PU)	! copia le condizioni iniz.
        if (.not.fl_nocap(pgrato(ie))) then
		j      = j+1
          rc_topvar(ie) = j
c_ace
c	   if (f_densita.eq.1) then
c		yy(j) = rocitj(ie)
c	   else
c		yy(j) = rocitj(ie)/ro0td(ie)
c         endif
c_ace
          yy(j) = rocitj(ie)
          rc_peso_ctlm(ie) = rc_weit_p/10
        endif
	ENDDO

	DO ie= otipti(E_PU),etipti(E_PU)	! copia le condizioni iniz.
        if (fl_nocap(pgrato(ie))) then
		j      = j+1
          rc_topvar(ie) = j
c_ace	   if (f_densita.eq.1) then
c		yy(j) = rocitj(ie)
c	   else
c		yy(j) = rocitj(ie)/ro0td(ie)
c        endif
c_ace
          yy(j) = rocitj(ie)
          rc_peso_ctlm(ie) = rc_weit_p/10
        endif
	ENDDO
      np  = j

	DO ie = otipti(e_tr),etipti(e_tr)
		j      = j+1
          rc_topvar(ie) = j
	   if (f_densita.eq.1) then
		yy(j) = porttj(ie)*ro0td(ie)
	   else
		yy(j) = porttj(ie)
         endif
          rc_peso_ctlm(ie) = rc_weit_q/100
	ENDDO
      nt  = j - np

      ne_spe_prv = ne_spe       ! tutti gli elementi spec.
!      ne_spe_prv = 0
	DO it = 1,ne_spe_prv
  	  DO ie = otipti(te_spe(it)),etipti(te_spe(it))
		j      = j+1
          rc_topvar(ie) = j
	   if (f_densita.eq.1) then
		yy(j) = porttj(ie)*ro0td(ie)
	   else
		yy(j) = porttj(ie)
         endif
!====================================================
          if (hstatj(ie).eq.sta_q) then
              rc_peso_ctlm(ie) = rc_weit_q
!          else if (htcitj(ie).eq.tc_on) then
!              rc_peso_ctlm(ie) = rc_weit_q
          else if (hstatj(ie).eq.sta_off) then
              rc_peso_ctlm(ie) = rc_weit_q*10
          else if (hstatj(ie).eq.sta_on.and.
     -               porttj(ie).eq.0.0) then
              rc_peso_ctlm(ie) = rc_weit_q*10
          else
              rc_peso_ctlm(ie)=  rc_weit_q/100.
          endif
!====================================================
  	  ENDDO
	ENDDO
      ns  = j - nt - np

	RETURN
	END
!======================================================================

	SUBROUTINE sd_sta_init (	! Calcola il primo tentativo
	1		 )		! per il GEAR
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/tjt.inc'		!:INOUT
	INCLUDE '../inc/ric.inc'		!:INOUT
*&*
        integer*4 ie,it
c        real*4 q_app,q_del,q_del_max

*&*------------------------------------------------------------
!			portate dei tronchi
*&*------------------------------------------------------------
        
  	  DO ie = otipti(e_tr),etipti(e_tr)
                call agg_tronco_q(ie)
	  ENDDO
!----------------------------------------------------------------------
!			portate elementi speciali
!----------------------------------------------------------------------
	DO it = 1,ne_spe
  	  DO ie = otipti(te_spe(it)),etipti(te_spe(it))
		porttj(ie) = porttjt(ie) 
	  ENDDO
	ENDDO
!----------------------------------------------------------------------
	RETURN
	END

c
!======================================================================

	SUBROUTINE agg_derivati (	! Aggiorna derivati
	1		         rom)		! :IN vettore densita' attuali
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/ric.inc'		!:IN
	INCLUDE '../inc/tjt.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/ty.inc'		!:INOUT
*&*
	REAL*4   	rom   (*)

        real*4 x
        integer*4 ie
*&*------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
          porttj(ie) = porttjt(ie)

	ENDDO
!----------------------------------------------------------------------
!			tronchi
!----------------------------------------------------------------------
	DO ie = otipti(e_tr),etipti(e_tr)
		call agg_tronco(ie)        ! coeff fergusson
	ENDDO
!----------------------------------------------------------------------
	RETURN
	END

c
!======================================================================

	SUBROUTINE rec_tj_rom (	! Recupera tj da soluzione
	1		         rom)		! :IN vettore densita' attuali
*&*
        implicit none
	INCLUDE '../inc/param.inc'		!:costante
	INCLUDE '../inc/ti.inc'		!:IN
	INCLUDE '../inc/td.inc'		!:IN
	INCLUDE '../inc/ric.inc'		!:IN
	INCLUDE '../inc/tj.inc'		!:INOUT
	INCLUDE '../inc/ty.inc'		!:INOUT
*&*
	REAL*4   	rom   (*)

        integer*4 ie,it
*&*------------------------------------------------------------
	DO ie = otipti(e_pu),etipti(e_pu)
c	   if (f_densita.eq.1) then
c 		  rocitj(ie) = rom(rc_topvar(ie))c
c	   else
c 		  rocitj(ie) = rom(rc_topvar(ie))*ro0td(ie)
c         endif
 		rocitj(ie) = rom(rc_topvar(ie))
		CALL zrot(ie,rocitj(ie),temptj(ie),zmcitj(ie))
		prestj(ie) = rocitj(ie) * temptj(ie) * zmcitj(ie) * ravotd(ie)
	ENDDO
!----------------------------------------------------------------------
!			tronchi
!----------------------------------------------------------------------
	DO ie = otipti(e_tr),etipti(e_tr)
	     if (f_densita.eq.1) then
 		  porttj(ie) = rom(rc_topvar(ie))/ro0td(ie)
	     else
 		  porttj(ie) = rom(rc_topvar(ie))
           endif
	ENDDO
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!			portate degli elementi speciali
!----------------------------------------------------------------------
        DO it=1,ne_spe
 	    DO ie = otipti(te_spe(it)),etipti(te_spe(it))
	     if (f_densita.eq.1) then
 		  porttj(ie) = rom(rc_topvar(ie))/ro0td(ie)
	     else
 		  porttj(ie) = rom(rc_topvar(ie))
           endif
 	  ENDDO
	ENDDO
	RETURN
	END

c
!======================================================================
!======================================================================
	SUBROUTINE iniziaTJ()	! completa la working area del dinamico
				            ! ( TJ,TY,TZ)
                              ! stima iniziale della dro/dt per il bilancio ai punti
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/ric.inc'	!:IN
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/TD.inc'	!:IN
	INCLUDE '../inc/TJ.inc'	!:OUT
	INCLUDE '../inc/TY.inc'	!:OUT

        integer*4 i,im,iv,it,j
        real*4 x,x_app
        real*4 rap1,rap2,rap3

*&*
!----------------------------------------------------------------------
!	 Punti  :dati dei punti e del prelievo associato
!      calcola dproty + droty partendo da TJ
!----------------------------------------------------------------------
	do i=otipti(e_pu),etipti(e_pu)
c_rip		CALL agg_punto(i)
c_rip		CALL zrotdp(i,rocitj(i),temptj(i),zmcitj(i),dproty(i))
	   CALL zrotdp(i,rocitj(i),temptj(i),x_app,dproty(i))
         if (f_densita.eq.1) then
		x = porttj(i) * ro0td(i)
         else
		x = porttj(i)
         endif
		droty(i) = - x
	ENDDO
!----------------------------------------------------------------------
!		 Tronchi
!----------------------------------------------------------------------
	do i=otipti(e_tr),etipti(e_tr)

		im = opumto(i)
		iv = opuvto(i)
c_rip		CALL agg_tronco(i)
         if (f_densita.eq.1) then
		x = porttj(i) * ro0td(i)
         else
		x = porttj(i)
         endif
		droty(im) = droty(im) - x
		droty(iv) = droty(iv) + x
	ENDDO
!----------------------------------------------------------------------
!		 valvole 
!----------------------------------------------------------------------
      do j=1,ne_vlv
	  do i=otipti(te_vlv(j)),etipti(te_vlv(j))
		im = opumto(i)
		iv = opuvto(i)
         if (f_densita.eq.1) then
		x = porttj(i) * ro0td(i)
         else
		x = porttj(i)
         endif
		droty(im)  = droty(im) - x
		droty(iv)  = droty(iv) + x
        ENDDO
	ENDDO
!----------------------------------------------------------------------
!		 Centrali
!----------------------------------------------------------------------
	do i=otipti(e_ce),etipti(e_ce)
		im = opumto(i)
		iv = opuvto(i)
c_rip          call qconce(porttj(i),prestj(im),prestj(iv),i,
c_rip     -                 consty(i),rap1,rap2,rap3)
         if (f_densita.eq.1) then
		x = porttj(i) * ro0td(i)
	    x_app = consty(i)*ro0td(i)
         else
		x = porttj(i)
	    x_app = consty(i)
         endif
         droty(im)  = droty(im) - (x + x_app)
         droty(iv)  = droty(iv) + x
	ENDDO
!----------------------------------------------------------------------
!		 Unari
!----------------------------------------------------------------------
	do it=1,ne_uni
         do i=otipti(te_uni(it)),etipti(te_uni(it))
		iv = opuvto(i)
         if (f_densita.eq.1) then
		x = porttj(i) * ro0td(i)
         else
		x = porttj(i)
         endif
		droty(iv)  = droty(iv) + x
          ENDDO
	ENDDO
!----------------------------------------------------------------------
	RETURN
	END ! iniziatj
!----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------------------------------------------------------------------
!-----------------------------------------------------------------------
	SUBROUTINE agg_tlc(t_cor,t_tlc,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/DIN_UTE.inc'	!:IN
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/RIC.inc'	!:IN

      real*4 t_cor,t_tlc

      real*4 t_cor_lcl,t_tlc_lcl
      integer*4 ier
*&*
!----------------------------------------------------------------------
	if (abs(t_tlc-t_cor) .GT. t_d_passo_min) return

      if (usa_tlcic) then
        t_tlc_lcl = mod(t_tlc,t_d_cic)
        t_cor_lcl = mod(t_cor,t_d_cic)
	else
        t_tlc_lcl = t_tlc
        t_cor_lcl = t_cor
	endif
	DO WHILE (abs(t_tlc_lcl-t_cor_lcl) .LT. t_passo_input)
        call agg_tlc_elem(t_tlc)
        call legge_tlc(t_tlc_lcl,*9999)
      ENDDO
      if (usa_tlcic) then
	  if (t_tlc_lcl .GE. t_d_cic) then
          t_tlc = t_tlc + t_d_cic
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          call rewind_file_tlc(*9999)
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          t_cor_lcl = 0
          t_tlc_lcl = 0
          call legge_tlc(t_tlc_lcl,*9999)
        endif
        t_tlc = int(t_tlc/t_d_cic)*t_d_cic + t_tlc_lcl
	else
        t_tlc = t_tlc_lcl
	endif

      RETURN

9999  continue
      Return 1

	END 
!-----------------------------------------------------------------------
	SUBROUTINE agg_prl(t_cor,t_prl,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/RIC.inc'	!:IN
c	INCLUDE '../inc/TQ.inc'	!:out

      real*4 t_cor,t_prl


      real*4 delta_t
      integer*4 ie,pos
*&*
!----------------------------------------------------------------------
c	if (abs(t_prl-t_cor) .LT. t_d_passo_min) then
c	  if (mod(t_prl,t_d_cic) .LT. t_d_passo_min) then
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c          call rewind_file_prl(*9999)
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c        endif
c        call legge_prl(t_prl,*9999)
c      endif
c      delta_t=MOD(t_cor ,t_d_prl)
c      call agg_prl_elem(delta_t)
      call agg_prl_elem(t_cor)
      
	RETURN
9999  continue
      Return 1 
	END 
!-----------------------------------------------------------------------
	SUBROUTINE agg_out(t_cor,t_out,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/DIN_UTE.inc'	!:IN
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/RIC.inc'	!:IN
	include '../inc/err.inc'
	include '../inc/scenario.inc'
	INCLUDE '../inc/nw.inc'	!:prov
c	INCLUDE '../inc/Tj.inc'	!:prov


      real*4 t_cor,t_out

!      integer n_out /0/
! 	save n_out


      logical*2 lapp
	real*4    rapp
	integer*4 iapp

	character*(l_D_ERR) msgerr
	character*(7) str_tempo
	external str_tempo

!----------------------------------------------------------------------
	if (abs(t_out-t_cor) .LT. t_d_passo_min) then
        ultimo_camp = ultimo_camp + 1
        call gest_error (3,0,""," ",0)
        write (msgerr,'(" ======= Campionamento",I4,
     -         " al tempo",a7)') ultimo_camp, str_tempo(t_out)
        call gest_error (3,0,"",msgerr,0)
        call gest_error (3,0,"Dinamica","Visualizza data",0)

c-din-corr
        lcent_last = .true.
c-din-corr-end

        call sim_cent(rapp,lapp,iapp)
crsa_pcls_din
!        call agg_port_pcls_def()
crsa_pcls_din

c-din-corr
        lcent_last = .false.
c-din-corr-end

        call verstel_nw()

crsa_pcls_din
!        call agg_port_pcls_cor()
crsa_pcls_din

        call validazione_out(*9999)

        corr_ist = t_out
        fl_est = 1

        call scrivi_scenario (msgerr,*7777)
cgpe-temp
        call scrittura_ris_rete(*7777)
cgpe-temp-end

        fl_est = 0

!        call risultati_pronti()   !!!!!??????

C ripristina i valori dipendenti dalla direzione per continuare il calcolo
        call agg_dire()

        call gest_error (3,0,""," ",0)
        write (msgerr,'(" ======= Scritto campionamento",I4,
     -         " al tempo",a7)') ultimo_camp, str_tempo(t_out)
        call gest_error (3,0,"",msgerr,0)
        call gest_error (3,0,""," ",0)
        call gest_error (3,0,""," ",0)
!        n_out = n_out + 1
!        t_out = n_out * t_d_out
        t_out = t_out + t_d_out
	  if (abs(t_out-t_fin) .LT. t_passo_input) t_out = t_fin
      endif

	RETURN
9999  continue
      Return 1 
7777  continue
      call gest_error (3,0,"","",0)
      call gest_error (3,0,"",msgerr,0)
      call gest_error (3,0,"","",0)
	goto 9999

	END 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
	SUBROUTINE agg_tlc_elem(tempo)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/default.inc'	!:costante
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/TJ.inc'	!:IN
	INCLUDE '../inc/TH.inc'	!:IN
	INCLUDE '../inc/TD.inc'	!:IN
	INCLUDE '../inc/Tv.inc'	!:IN
	INCLUDE '../inc/WB.inc'	!:OUT
	INCLUDE '../inc/rampa.inc'	!:OUT
	INCLUDE '../inc/STAZIONE.INC'	!
	INCLUDE '../inc/STATIONS.INC'	!
	INCLUDE '../inc/UNITS.INC'	!
	INCLUDE '../inc/ric.inc'	!
	INCLUDE '../inc/err.inc'	!

      real*4 tempo

      logical*2 cnt_eq_on  !function

      integer*4 hstasav
      integer*4 ier
      integer*4 kk,ii,ii1

      integer*4 oftowb_ass,iu,ju,k

	real*4 qset_real
	external qset_real

	character*(l_D_ERR) msgerr
	character*(7) str_tempo
	external str_tempo
	character*(6) str_reg
	external str_reg
*&*
!----------------------------------------------------------------------

      tempo_man(oftowb) = tempo
      tfintj(oftowb) = tempo + tritwb

 	IF (hregwb .NE. tc_und)   THEN

		 htcitj(oftowb) = hregwb 

          write (msgerr,'(" ==== Nuovo Telecomando al tempo",a7,
     -      " con durata",a7)') str_tempo(tempo),str_tempo(tritwb)
           call gest_error (3,0,"",msgerr,0)
	      msgerr=trim(te_stri(htipto(oftowb)))//' : '//
     -             trim(snomtv(oftowb)) //' con regolazione: '// 
     -             str_reg(oftowb)
            call gest_error(3,0,' ',msgerr,0)
c	      msgerr=trim(te_stri(htipto(oftowb)))//' : '//
c     -             trim(snomtv(oftowb))  
c            call gest_error(3,0,' ',msgerr,0)
            call gest_error (3,0,""," ",0)
c            call tipo_err(1,err_ko,str_err)
c            call gest_error(3,0,' ',str_err,0)
c            str_err = 'Lunghezza minore del dislivello'
c            call gest_error(3,0,' ',str_err,0)
c            write(str_err,540) udm_out_str(udm_lun,rluntx(pos),0),
c     *                         udm_out_str(udm_lun,disl,1)
c            call gest_error(3,0,' ',str_err,0)

		 hstasav = hstatj(oftowb) 
	     hstatj(oftowb)=stadatc(hregwb)
           if(cnt_eq_on(hstasav,hstatj(oftowb)) .and.
     -        o_sta_ric.lt.O_ST_NVTLM) o_sta_ric = O_ST_NVTLM

		 IF     (hregwb .EQ. tc_pm .or.
	1		    hregwb .EQ. tc_npm)  THEN	! P_monte
					pmsetj(oftowb) = pmonwb
                      delta_man(oftowb) = pmonwb-prestj(opumto(oftowb))
		 ELSEIF (hregwb .EQ. tc_pv .or.
	1		    hregwb .EQ. tc_npv)  THEN	! P_valle
					pvsetj(oftowb) = pvalwb
                      delta_man(oftowb) = pvalwb-prestj(opuvto(oftowb))
		 ELSEIF (hregwb .EQ. tc_off )  THEN	! Portata
					qsettj(oftowb)  = 0.0
                      delta_man(oftowb) = qset_real(oftowb)
     1                                   -porttj(oftowb)
		 ELSEIF (hregwb .EQ. tc_q )  THEN	! Portata
					qsettj(oftowb)  = diretj(oftowb)*qsetwb
crsa_pcls_din
                      delta_man(oftowb) = qset_real(oftowb)
     1                                   -porttj(oftowb)
c                      if (tritwb) .eq.0) then
c					  dqsetj(oftowb)  = 0
c                  	else
c			 		  dqsetj(oftowb)  = 
c	1					  (porttj(oftowb)-qsetwb)/tritwb
c	                endif

		 ELSEIF (hregwb .EQ. tc_pq)  THEN	! Pm/pv/q
					pmsetj(oftowb) = pmonwb
					pvsetj(oftowb) = pvalwb
					qsettj(oftowb) = diretj(oftowb)*qsetwb
                      delta_man(oftowb) = pvalwb-prestj(opuvto(oftowb))
c                      if (tritwb) .eq.0) then
c					  dqsetj(oftowb)  = 0
c                  	else
c			 		  dqsetj(oftowb)  = 
c	1					  (porttj(oftowb)-qsetwb)/tritwb
c	                endif

		 ENDIF  !hreg..
	ENDIF

		IF (tsetwb .NE. R_UND )  tsettj(oftowb)  = tsetwb	! temperatura

c          if (htipto(oftowb).eq.e_ce) then
          if (otbgwb .NE. I_UND) then
            unit_attiv_tlc(otbgwb) = atbgwb
c-din-assetti coniugati
            if (atbgwb .eq. 1) then
              oftowb_ass = I_UND
              call find_ce_ass(oftowb,oftowb_ass)
              if (oftowb_ass .ne. I_UND) then
                iu = first_unit(pgrato(oftowb))
                ju = first_unit(pgrato(oftowb_ass))
                k = otbgwb-iu+1
                unit_attiv_tlc(ju+k-1) = 0
              endif
           endif
c-din-end
          endif

	RETURN
	END
!-----------------------------------------------------------------------
	SUBROUTINE agg_prl_elem(tempo)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/dimension.inc'	!:costante
	INCLUDE '../inc/TI.inc'
	INCLUDE '../inc/TJ.inc'
	INCLUDE '../inc/TQ.inc'

      real*4 tempo

      integer*4 ie,pos
      real*4 QPUNT_new
*&*
!----------------------------------------------------------------------
      do ie = otipti(e_pu),etipti(e_pu)
         porttj(ie)   = QPUNT_new(tempo,0.0,ie)
      enddo

	RETURN
	END 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
	SUBROUTINE legge_tlc(t_tlc,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/WB.inc'

      real*4 t_tlc

      integer*4 ier
*&*
c   Aggiorna WB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call leggi_file_tlc(ier,*9999)
c   Aggiorna WB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (ier .eq. 0) then
        t_tlc = timeWB
      else
        t_tlc = R_MAX
      endif

	RETURN
9999  continue
      Return 1 
	END 
!-----------------------------------------------------------------------
	SUBROUTINE legge_prl(t_prl,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/ric.inc'
c	INCLUDE '../inc/TQ.inc'

      real*4 t_prl

      integer*4 ier
*&*
c   Aggiorna TQ.INC !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      call leggi_file_prl(ier,*9999)
      IER = -1
c   Aggiorna TQ.INC !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (ier .eq. 0) then
        t_prl = t_prl + t_d_prl
      else
        t_prl = R_MAX
      endif

	RETURN
9999  continue
      Return 1 
	END 

!-----------------------------------------------------------------------
	SUBROUTINE ini_prl_elem()
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/TI.inc'
	INCLUDE '../inc/TJ.inc'
c	INCLUDE '../inc/TQ.inc'


      integer*4 ie,pos
*&*
!----------------------------------------------------------------------
c      do ie = otipti(e_pu),etipti(e_pu)
c         pos=pgrato(ie)
c         qsp3tq(pos) = porttj(ie)
c         qsp2tq(pos) = 0.0
c         qsp1tq(pos) = 0.0
c      enddo

	RETURN
	END 
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
	SUBROUTINE skip_tlc(t_cor,t_tlc,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/DIN_UTE.inc'	!:IN
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/RIC.inc'	!:IN

      real*4 t_cor,t_tlc

      real*4 t_cor_lcl,t_tlc_lcl
*&*
!----------------------------------------------------------------------
c   apre file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call open_file_tlc(*9999)
c   apre file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      t_tlc = t_cor
      t_tlc_lcl = -1.0  !0.0
      if (usa_tlcic) then
        t_cor_lcl = mod(t_cor,t_d_cic)
	else
        t_cor_lcl = t_cor
	endif
	DO WHILE (t_tlc_lcl .LT. t_cor_lcl - t_d_passo_min)
        call legge_tlc(t_tlc_lcl,*9999)
      ENDDO
      if (usa_tlcic) then
	  if (t_tlc_lcl .GE. t_d_cic) then
          t_tlc = t_tlc + t_d_cic
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          call rewind_file_tlc(*9999)
c   rilegge file !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          t_cor_lcl = 0
          t_tlc_lcl = 0
          call legge_tlc(t_tlc_lcl,*9999)
        endif
c        if (t_tlc_lcl .eq. R_MAX) then
        if (t_tlc_lcl .ge. t_d_cic) then
          t_tlc = R_MAX
        else
          t_tlc = int(t_tlc/t_d_cic)*t_d_cic + t_tlc_lcl
        endif
	else
        t_tlc = t_tlc_lcl
	endif

      RETURN

9999  continue
      Return 1

	END 
!-----------------------------------------------------------------------
	SUBROUTINE skip_prl(t_cor,t_prl,*)
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/DIN_UTE.inc'	!:IN
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/RIC.inc'	!:IN

      real*4 t_cor,t_prl

      integer*4 i_rec,ier
*&*
!----------------------------------------------------------------------
c     	i_rec = mod(mod(t_cor,t_d_cic),t_d_prl)
c   salta i i_rec record !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c     call skip_file_prl(i_rec,*9999)
c   salta i i_rec record !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c      t_prl = int(t_cor/t_d_prl)*t_d_prl
c      call legge_prl(t_prl,*9999)

      RETURN

9999  continue
      Return 1

	END 
!-----------------------------------------------------------------------
	SUBROUTINE copia_TJ_su_TJT()	! copia aree common
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/TI.inc'	!:IN
	INCLUDE '../inc/TJ.inc'	!:IN
	INCLUDE '../inc/TJT.inc'	!:OUT


        integer*4 je,it
*&*
!----------------------------------------------------------------------
	do it=1,ne_gen
          do je=otipti(te_gen(it)),etipti(te_gen(it))
           TJT0(je) = TJ0(je)
!           TJT1(je) = TJ1(je)
           TJT2(je) = TJ2(je)
!           TJT3(je) = TJ3(je)
           TJT4(je) = TJ4(je)
          ENDDO
	ENDDO
!----------------------------------------------------------------------
	RETURN
	END 
!-----------------------------------------------------------------------
	SUBROUTINE copia_yy(yy_in,yy_out)	! copia aree common
*&*----------------------------------------------------------------------
        implicit none
	INCLUDE '../inc/param.inc'	!:costante
	INCLUDE '../inc/TI.inc'	!:IN

        real*4 yy_in(*),yy_out(*)

        integer*4 je,it
*&*
!----------------------------------------------------------------------
	do it=1,ne_gen
          do je=otipti(te_gen(it)),etipti(te_gen(it))
           yy_out(je) = yy_in(je)
          ENDDO
	ENDDO
!----------------------------------------------------------------------
	RETURN
	END 
c-------------------------------------------------------
c-------------------------------------------------------
c-------------------------------------------------------
      logical*2 function cnt_eq_on(hsta_prima,hsta_dopo)  ! Controllo equazioni ON
! T: se la variazione di stato ha prodotto aggiunta o eliminazione equazione ON
! F: se la variazione di stato non ha prodotto aggiunta o eliminazione equazione ON

      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/th.inc'
	include '../inc/ti.inc' !identificatori elementi
      include '../inc/tj.inc'
      include '../inc/staznw_par.inc'

	integer*4 hsta_prima,hsta_dopo
	integer*4 itp,iel,im,iv

	logical*2 set_non_ritorno
	external set_non_ritorno
	
      cnt_eq_on = (hsta_prima.eq.sta_fr.or.hsta_prima.eq.sta_on) .ne.
     -            (hsta_dopo.eq.sta_fr.or.hsta_dopo.eq.sta_on)


	RETURN
	END 
!-----------------------------------------------------------------------
****** DIN_LIB.FOR *
