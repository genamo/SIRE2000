c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
c
C---  Generalità
c
c$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!======================================================================
!-------------------------------------------------------------------------------
      logical*2 function elem_bin (i_elem)
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 i_elem

      elem_bin = (htipto(opumto(i_elem)).eq.e_pu .and.
     -            htipto(opuvto(i_elem)).eq.e_pu)
      return
      end
!-------------------------------------------------------------------------------
      logical*2 function elem_uni (i_elem)
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 i_elem
      logical*2 elem_bin

      elem_uni = (.not.(htipto(i_elem).eq.e_pu) .and.
     -            .not.(elem_bin(i_elem)) )
      return
      end
!-------------------------------------------------------------------------------
      logical*2 function elem_spe (i_elem)
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 i_elem

      elem_spe = (.not.(htipto(i_elem).eq.e_pu) .and.
     -            .not.(htipto(i_elem).eq.e_tr) )
      return
      end
!-------------------------------------------------------------------------------
      logical*2 function elem_vlv (i_elem)
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 i_elem
	integer*4 i


      elem_vlv=.false.
	do i=1,ne_vlv
	   if (te_vlv(i).eq.htipto(i_elem)) elem_vlv=.true.
	end do

      return
      end
c-------------------------------------------------------------------------------------------
      logical*2 function elem_cen (i_elem)
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 i_elem
	integer*4 i


      elem_cen=.false.
	do i=1,ne_cen
	   if (te_cen(i).eq.htipto(i_elem)) elem_cen=.true.
	end do

      return
      end
!-------------------------------------------------------------------------------
      logical*2 function ce_serie (i_el)

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni
      include '../inc/cap_bon_prv.inc'    !parametri centrali

      integer*4 i_el

      ce_serie = .false.
      if (pgrato_app(i_el) .gt. 0 .and.
     *    pgrato_app(i_el) .le. max_vert .and.
     *    pgrato_app(i_el) .ne. pgrato(i_el) ) then
        ce_serie = .true.
      endif

      return
      end ! ce_serie
!-------------------------------------------------------------------------------
        integer*4 function itop_from_ivar(ivs,v_topvar)
c----------------------------------------------------------------
c       Questa fuction restituisce l'indice dell'elemento topologico
c       dato l'indice della variabile di stato
c----------------------------------------------------------------
        implicit none
        INCLUDE '../inc/param.inc'             !:costante
        include '../inc/ti.inc '
        include '../inc/nw.inc '

        INTEGER*4 ivs
        INTEGER*4 v_topvar(*)

        INTEGER*4 ie,it
	  integer*4 oicen
!------------
	do it=1,ne_gen
          do ie=otipti(te_gen(it)),etipti(te_gen(it))
            if (v_topvar(ie).eq.ivs) then
              itop_from_ivar = ie
              return                !!!!!!!!!!!!!!!!!
            endif
          enddo
      enddo
!	Ciclo sulle centrali per i consumi
      do it=1,ne_cen
       if (te_cen(it).eq.e_ce) then
          oicen=oice
       else
          oicen=oics
       end if
	 do ie=otipti(te_cen(it)),etipti(te_cen(it))
            if (v_topvar(ie-oicen).eq.ivs) then
              itop_from_ivar = ie-oicen
C_new              itop_from_ivar = ie
              return                !!!!!!!!!!!!!!!!!
            endif
	  end do
      enddo
      itop_from_ivar = ie
      return
      end

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      logical*2 function reg_nr (i_elem) !definisce se l'elemento è regolato in
	                                   ! NON RITORNO
     
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/th.inc'
      include '../inc/tj.inc'

      integer*4 i_elem

      logical*2 elem_uni   ! function

      reg_nr = (
c_din_nr
!     -          elem_uni(i_elem) .or.
c_din_nr
     -          htcitj(i_elem).eq.tc_nr .or.
     -          htcitj(i_elem).eq.tc_npv .or.
     -          htcitj(i_elem).eq.tc_npm
     -         )
      return
      end
