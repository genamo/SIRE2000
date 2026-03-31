
      real*4 function dp_in (ie,pin,tin,q)

      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/ti.inc'	!: in
      include '../inc/cc.inc'
      include '../inc/tx.inc'	!: in
      include '../inc/top_app_ele.inc'	!: in
      include '../inc/tj.inc'	!: in/out
      INCLUDE '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'	!: in/out
      include '../inc/rk.inc'	!: in/out
      include '../inc/nw.inc'	!:lcent_first

      integer*4 IE
      real*4    pin,tin,q

      real*4    ro_in
!---------------------------------------------------------------------------

     


      if (htipto(ie).eq.e_cs) then ! centrale semplificata
cmar-dp_new_cs-31-07-09        dp_in = dpin(pgrato(ie))
      dp_in = dpin_s(pgrato(ie))*q**2
      else
        call ro_real(ie,pin,tin,ro_in)
cmar-p
	ro_in = 1.

       

        dp_in = stat_vars(3,pgrato(ie))*(q**2/ro_in)
	   
c        if (dp_in .gt. 3.) then
c          dp_in = delpin(pgrato(ie))
c        end if
      end if

    
      return
	end

      real*4 function dp_out (ie,pout,tout,q)

      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/ti.inc'	!: in
      include '../inc/cc.inc'
      include '../inc/tx.inc'	!: in
      include '../inc/top_app_ele.inc'	!: in
      include '../inc/tj.inc'	!: in/out
      INCLUDE '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'	!: in/out
      include '../inc/rk.inc'	!: in/out
      include '../inc/nw.inc'	!:lcent_first

      integer*4 IE
      real*4    pout,tout,q

      real*4    ro_out
!---------------------------------------------------------------------------

     

      if (htipto(ie).eq.e_cs) then ! centrale semplificata
cmar-dp_new_cs-31-07-09        dp_out = dpout(pgrato(ie))
      dp_out = dpout_s(pgrato(ie))*q**2
      else
        call ro_real(ie,pout,tout,ro_out)
cmar-p
	ro_out =1.

     

        dp_out = stat_vars(4,pgrato_app(ie))*(q**2/ro_out)
	 
c        if (dp_out .gt. 3.) then
c          dp_out = delpout(pgrato_app(ie))
c        end if

      end if

      


      return
	end

      Subroutine check_ce (istaz,jstaz,l_simula)
c---------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
cgpe-flag
      include '../inc/staznw_par.inc'
cgpe-flag-end
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/stations.inc'
      include '../inc/flag.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/units.inc'



	include '../inc/ass_cond.inc' 



      integer*4 istaz,jstaz    ! I)
      logical*2 l_simula       ! O) risultato del check per la centrale istaz,jstaz
                               !   = T se ha condizioni ok ed e' richiesta una compressione effettiva

      integer*4 ivert,jvert,ier,iu,j,i
      real*4    pasp_stat_dp,pmand_stat_dp
      real*4    flow_app,dp,p_app
      logical*2 l_ko,l_rec,flag_serie
      logical*2 ce_accesa,ce_off,ce_serie,ce_avail
      external  ce_accesa,ce_off,ce_serie,ce_avail
cgpe-flag
      logical*2 fl_fatal_ok,fl_set
	real*4 qset_real
	external qset_real
cgpe-flag-end
c-------------------------------------------------------------------------
c     Aggiunta le gestione del flag fl_fatal

      l_ko = .false.
c      l_rec = .false.
      l_simula = .false.

      flag_serie = ce_serie(istaz)
      if (.not.flag_serie) then
        ivert = pgrato(istaz)
        fl_fatal(ivert) = 0
        call set_stato_stazione(set_flag(ivert),istaz)
        if (set_flag(ivert) .eq. 0) then
c---->  gestione stazione off
          call spegni_ce (istaz,ivert)
c          temptj(istaz) = tmand_stat(ivert)

        else    !set_flag=1
c-ele 18/7
!-test sulla disponibilità delle unità di compressione: almeno una delle unità
! presenti in centrale deve essere disponibile o accesa
          if (.not.ce_avail(ivert)) l_ko=.true.
!-test sulla portata
cmar_ass_cond-08-06-2011
cmar   
cmar    l
cmar_ass_cond-08-06-2011

          if (flow_stat(ivert) .le. 0) l_ko = .true.

!-test sulla pressioni
          if (pmand_stat(ivert) .gt. stat_vars(6,ivert)+repsP) then
c pressione di mandata superiore alla pressione massima di mandata ==> imposizione di pout=pin e stato=OFF.
            l_ko = .true.
          endif

          call calc_deltap_eff (istaz,ivert,pasp_stat_dp,pmand_stat_dp)
          if (pasp_stat_dp  .le. 0 .or.  pmand_stat_dp .le. 0) then
c richieste pressioni alla stazione negative
c            tmand_stat(ivert) = tasp_stat(ivert)
c            pmand_stat(ivert) = pasp_stat(ivert)
            l_ko = .true.
          endif

          if (pmand_stat_dp .lt. pasp_stat_dp) then
c richiesta una pressione in uscita minore di quella di ingresso
            l_ko = .true.
          endif

c-din-corrc richiesta compressione nulla
c-din-corr          if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
c-din-corr            l_ko = .true.
c-din-corr          endif

cgpe-flag
          fl_fatal_ok = .false.
          if (.not.fl_sim .and. fl_recupero) then
            fl_set = .true.
            if (hstatj(istaz) .eq. sta_q .and.
     *          abs(flow_stat(ivert)-qset_real(istaz)) .gt. repsQ) then
              fl_set = .false.
            elseif (hstatj(istaz) .eq. sta_pv .and.
     *          abs(pmand_stat(ivert)-pvsetj(istaz)) .gt. repsP) then
              fl_set = .false.
            elseif (hstatj(istaz) .eq. sta_pm .and.
     *          abs(pasp_stat(ivert)-pmsetj(istaz)) .gt. repsP) then
              fl_set = .false.
            endif
          else
            fl_set = .false.
          endif
          if (fl_set) fl_fatal_ok = .true.
cgpe-flag-end

c-ele 26/7/06
c  Controllo sulla corretta definizione delle curve limite per l'applicazione
c  del criterio di equidistanza: errore bloccante se la parabola massima cade
c  a sinistra della parabola minima.
          if (tipo_criterio(ivert).eq.crit_equi) then
            iu=first_unit(ivert)
            do j = 1,unit_num(ivert)
               i = iu + j - 1
	         if (unit_crit_max(i).gt.unit_crit_min(i)) l_ko =.true.
            end do
          endif
c-ele
c test dell'indice di errore
          if (l_ko) then
            set_flag(ivert) = 0
            call spegni_ce (istaz,ivert)
            fl_fatal(ivert)= -1
cgpe-flag
            if (fl_fatal_ok) fl_fatal(ivert)= 0
cgpe-flag-end
          endif
          if (set_flag(ivert) .ne. 0) then
            l_simula = .true.

c-din-corr
c richiesta compressione nulla
            if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
              l_simula = .false.
            endif
c-din-corr-end
          endif
        endif

c eventuale assetto jstaz
        if (jstaz .ne. I_UND) then


cmar_ass_cond-13-06-2011
cmar  
cmar_ass_cond-13-06-2011


c-ele-10/8 pulisco l_ko dall'eventuale valore assegnato relativamente a istaz
	    l_ko=.false.
          jvert = pgrato(jstaz)
          fl_fatal(jvert) = 0
          call set_stato_stazione(set_flag(jvert),jstaz)
cmar-13062011

ccccccccccccccccc      set_flag(jvert)=1

      IF(flag_ass_cond(JSTAZ))THEN
      set_flag(jvert)=1

	ENDIF
cmar-13062011
          if (set_flag(jvert) .eq. 0) then
c---->  gestione stazione off
            call spegni_ce_ass (jstaz,jvert)
c            temptj(jstaz) = tmand_stat(jvert)
          else   !set_flag=1
c-ele 18/7
!-test sulla disponibilità delle unità di compressione: almeno una delle unità
! presenti in centrale deve essere disponibile o accesa
            if (.not.ce_avail(jvert)) l_ko=.true.

!-test sulla portata
            if (flow_stat(jvert) .le. 0) l_ko = .true.

!-test sulla pressioni
            if (pmand_stat(jvert) .gt. stat_vars(6,jvert)+repsP) then
              l_ko = .true.
            endif

c calcolo dei delta_p dovuti al piping
            call calc_deltap_eff (jstaz,jvert,pasp_stat_dp,
     *           pmand_stat_dp)
            if (pasp_stat_dp  .le. 0 .or. pmand_stat_dp .le. 0) then
c pressioni alla stazione negative
              l_ko = .true.
            endif

            if (pmand_stat_dp .lt. pasp_stat_dp) then
c richiesta una pressione in uscita minore di quella di ingresso
              l_ko = .true.
            endif

c-din-corr            if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
c-din-corr              l_ko = .true.
c-din-corr            endif

cgpe-flag
            fl_fatal_ok = .false.
            if (.not.fl_sim .and. fl_recupero) then
              fl_set = .true.
              if (hstatj(jstaz) .eq. sta_q .and.
     *            abs(flow_stat(jvert)-qset_real(jstaz)) .gt.repsQ) then
                fl_set = .false.
              elseif (hstatj(jstaz) .eq. sta_pv .and.
     *            abs(pmand_stat(jvert)-pvsetj(jstaz)) .gt. repsP)then
                fl_set = .false.
              elseif (hstatj(jstaz) .eq. sta_pm .and.
     *            abs(pasp_stat(jvert)-pmsetj(jstaz)) .gt. repsP) then
                fl_set = .false.
              endif
            else
              fl_set = .false.
            endif
            if (fl_set) fl_fatal_ok = .true.
cgpe-flag-end
c-ele 26/7/06
c  Controllo sulla corretta definizione delle curve limite per l'applicazione
c  del criterio di equidistanza: errore bloccante se la parabola massima cade
c  a sinistra della parabola minima.
            if (tipo_criterio(jvert) .eq. crit_equi) then
              iu=first_unit(jvert)
              do j = 1,unit_num(jvert)
                 i = iu + j - 1
	           if (unit_crit_max(i).gt.unit_crit_min(i)) l_ko =.true.
              end do
            endif
c-ele
cgpe-corr-staz
cgpe-flag
            if (fl_fatal_ok) fl_fatal(jvert)= 0

cgpe-flag-end
cgpe-corr-staz-end
            if (l_ko) then
              set_flag(jvert) = 0
              call spegni_ce(jstaz,jvert)
              fl_fatal(jvert) = -1
            endif
cgpe-corr-staz
cgpe-corr-stazcgpe-flag
cgpe-corr-staz            if (fl_fatal_ok) fl_fatal(jvert)= 0
cgpe-corr-stazcgpe-flag-end
cgpe-corr-staz-end
            if (set_flag(ivert).ne.0 .or. set_flag(jvert).ne.0) then
c almeno una centrale è da simulare (cioè almeno una centrale ha set_flag diverso da zero)
              l_simula = .true.

c-din-corr
              if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
                l_simula = .false.
              endif
c-din-corr-end
            endif

          endif
        endif
      else
c centrale in serie
        ivert = pgrato(istaz)
        jvert = pgrato_app(istaz)
        fl_fatal(ivert) = 0
        fl_fatal(jvert) = 0
        call set_stato_stazione(set_flag(ivert),istaz)
        call set_stato_stazione(set_flag(jvert),istaz)
        if (set_flag(ivert) .eq. 0 .or. set_flag(jvert) .eq. 0) then
          if (set_flag(ivert) .eq. 0 .and. set_flag(jvert) .eq. 1 .or.
     *        set_flag(ivert) .eq. 1 .and. set_flag(jvert) .eq. 0 ) then
c solo una delle due fasi e' richiesta spenta => errore
            l_ko = .true.
          endif
c---->  gestione stazione off
          call spegni_ce(istaz,ivert)
c          temptj(istaz) = tmand_stat(ivert)

          call spegni_ce(istaz,jvert)
c          temptj(istaz) = tmand_stat(jvert)
    	  else
c-ele 18/7
!-test sulla disponibilità delle unità di compressione: almeno una delle unità
! presenti in centrale deve essere disponibile o accesa
          if (.not.ce_avail(ivert).or..not.ce_avail(jvert)) then
		   l_ko=.true.
	    end if
!-test sulla portata
          if (flow_stat(ivert) .le. 0 .or. flow_stat(jvert) .le. 0) then
            l_ko = .true.
          endif

!-test sulla pressioni
          if (pmand_stat(jvert) .gt. stat_vars(6,jvert)+repsP) then
            l_ko = .true.
          endif

c calcolo pressione di aspirazione fase 1
          call calc_deltap_eff(istaz,ivert,pasp_stat_dp,p_app)
c calcolo pressione di mandata fase 2
          call calc_deltap_eff(istaz,jvert,p_app,pmand_stat_dp)

          if (pasp_stat_dp  .le. 0 .or. pmand_stat_dp .le. 0) then
c pressioni alla stazione negative ==> imposizione di pout=pin e stato=OFF.
            l_ko = .true.
          endif

          if (pmand_stat_dp .lt. pasp_stat_dp) then
c richiesta una pressione in uscita minore di quella di ingresso
            l_ko = .true.
          endif

c-din-corr          if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
c-din-corr            l_ko = .true.
c-din-corr          endif

cgpe-flag
          fl_fatal_ok = .false.
          if (.not.fl_sim .and. fl_recupero) then
            fl_set = .true.
            if (hstatj(istaz) .eq. sta_q .and.
     *        abs(flow_stat(ivert)-qset_real(istaz)) .gt. repsQ) then
              fl_set = .false.
            elseif (hstatj(istaz) .eq. sta_pv .and.
     *            abs(pmand_stat(jvert)-pvsetj(istaz)) .gt. repsP)then
              fl_set = .false.
            elseif (hstatj(istaz) .eq. sta_pm .and.
     *            abs(pasp_stat(ivert)-pmsetj(istaz)) .gt. repsP) then
              fl_set = .false.
            endif
          else
            fl_set = .false.
          endif
          if (fl_set) fl_fatal_ok = .true.
cgpe-flag-end

c-ele 26/7/06
c  Controllo sulla corretta definizione delle curve limite per l'applicazione
c  del criterio di equidistanza: errore bloccante se la parabola massima cade
c  a sinistra della parabola minima.
          if (tipo_criterio(ivert) .eq. crit_equi) then
            iu=first_unit(ivert)
            do j = 1,unit_num(ivert)
               i = iu + j - 1
	         if (unit_crit_max(i).gt.unit_crit_min(i)) l_ko =.true.
            end do

	      iu=first_unit(jvert)
            do j = 1,unit_num(jvert)
               i = iu + j - 1
	         if (unit_crit_max(i).gt.unit_crit_min(i)) l_ko =.true.
            end do
          endif
c-ele
          if (l_ko) then
            set_flag(ivert) = 0
            call spegni_ce(istaz,ivert)
            fl_fatal(ivert) = -1
cgpe-flag
            if (fl_fatal_ok) fl_fatal(ivert)= 0
cgpe-flag-end
            set_flag(jvert) = 0
            call spegni_ce(istaz,jvert)
            fl_fatal(jvert) = -1
cgpe-flag
            if (fl_fatal_ok) fl_fatal(jvert)= 0
cgpe-flag-end
          endif
          if (set_flag(ivert).ne.0 .and. set_flag(jvert).ne.0) then
            l_simula = .true.

c-din-corr
            if (ce_off(pasp_stat_dp,pmand_stat_dp)) then
              l_simula = .true.
            endif
c-din-corr-end

          endif
        endif
      endif
c
      return
      end ! check_ce

       Subroutine calc_deltap_eff (istaz,ivert,pasp_stat_dp,
     *           pmand_stat_dp)
c---------------------------------------------------------------------------
c Calcolo dei delta_p dovuti al piping erelative pressioni ai turbogruppi
c---------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
c	common deltap/dp1, dp2

      integer*4 istaz,ivert                   ! I)
	real*4    pasp_stat_dp,pmand_stat_dp    ! O)

      real*4    dp_in,dp_out
      external  dp_in,dp_out
c	real*4    dp1, dp2 
c---------------------------------------------------------------------------

      


      delpr1(ivert) = dp_in(istaz,pasp_stat(ivert),
     *                tasp_stat(ivert),flow_stat(ivert))
      delpr2(ivert) = dp_out(istaz,pmand_stat(ivert),
     *                stat_vars(7,ivert),flow_stat(ivert))




       
cmar-p-rpt
      
      pasp_stat_dp  = pasp_stat(ivert) - delpr1(ivert)
      pmand_stat_dp = pmand_stat(ivert) + delpr2(ivert)
cmar-p-rpt      
   
   
      return
      end
cmar-p-29-07-09
cmar*****************************************************************


       Subroutine calc_deltapTOT_eff (istaz,ivert,pasp_stat_dp,
     *           pmand_stat_dp)
c---------------------------------------------------------------------------
c Calcolo dei delta_p dovuti al piping erelative pressioni ai turbogruppi
c---------------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'

      integer*4 istaz,ivert                   ! I)
	real*4    pasp_stat_dp,pmand_stat_dp    ! O)

      real*4    dp_in,dp_out
      external  dp_in,dp_out

c---------------------------------------------------------------------------
 
cmar-dp-rpt      
      


      pasp_stat_dp  = pasp_stat(ivert) - delpr1(ivert)
      pmand_stat_dp = pmand_stat(ivert) + delpr2(ivert)

      
    
cmar-p-rpt      
  
      return
      end

cmar*****************************************************************



      subroutine spegni_ce(istaz,ivert)
c
c  Vengono azzerate le variaili di output di centrale e spente tutte le unita'
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/UNITS.INC'
c
      integer*4 istaz,ivert      ! I)

      integer*4 i,j
c-----------------------------------------------------------------------
cgpe-prv
c      call imp_new_sta(istaz,ivert,pmand_stat(ivert),'FR')
cgpe-prv-end
      tot_cons(ivert)   = 0.
      comp_ratio(ivert) = 1.
c      pmand_stat(ivert) = pasp_stat(ivert)
      tmand_stat(ivert) = tasp_stat(ivert)

      do i = 1,unit_num(ivert)
        j = first_unit(ivert)+i-1
        status(j) = funz_off
      end do

      return
      end



      subroutine spegni_ce_ass(istaz,ivert)
c
c  Vengono azzerate le variaili di output di centrale e spente tutte le unita'
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/UNITS.INC'
c
      integer*4 istaz,ivert      ! I)

      integer*4 i,j
c-----------------------------------------------------------------------
cgpe-prv
c      call imp_new_sta(istaz,ivert,pmand_stat(ivert),'FR')
cgpe-prv-end
      tot_cons(ivert)   = 0.
      comp_ratio(ivert) = 1.
c      pmand_stat(ivert) = pasp_stat(ivert)
      tmand_stat(ivert) = tasp_stat(ivert)

cc      do i = 1,unit_num(ivert)
cc        j = first_unit(ivert)+i-1
cc        status(j) = funz_off
cc      end do

      return
      end
