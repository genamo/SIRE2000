!===============================================================================
!                     LIB_VALIDAZ.FOR
!
! Libreria moduli per il calcolo dei dati derivati da quelli primari di input
! per: topologia TOP e condizioni iniziali CIN
! Verifica congruenza dati CIN
!---------------------------------------------------
!-TOP subroutine definisci_connessioni(*)
!-TOP subroutine calcola_derivati_top
!-CIN subroutine calcola_derivati_cin (*)
!===============================================================================
      Subroutine validazione_in(*)

c[|   Definisce la validazione di rete

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/flag.inc'    !flag

!-------------------------------------------
!-------------------------------------------
!-Definisce le connessioni di rete
        call definisci_connessioni()
!-------------------------------------------
!-Calcola dati derivati TOP elementi
!-------------------------------------------
        call calcola_derivati_top
!-------------------------------------------
!-Calcola dati derivati CIN elementi
!-------------------------------------------
      call calcola_derivati_cin (*9999)
!-------------------------------------------
      return

9999  continue
      return 1
      end
!-------------------------------------------------------------------------------
      Subroutine validazione_out(*)

c[|   Prepara dati per output

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'   
      include '../inc/th.inc'   
      include '../inc/tj.inc'   
      include '../inc/cap_bon_prv.inc'

      integer*4 iel,ivet
      
	logical*2  ce_serie
	external ce_serie

! Riporto le portate secondo la direzione per l'output
      call agg_dire()

!-------------------------------------------
c--------->CENTRALI
	do iel=otipti(e_ce),etipti(e_ce)
	     call calcola_derivati_out_ce(iel,pgrato(iel))
           call converti_out_ce(pgrato(iel))
	     if (ce_serie(iel)) then
	       call calcola_derivati_out_ce(iel,pgrato_app(iel))
cgpe
	       call calcola_out_serie(iel,pgrato(iel),pgrato_app(iel))
cgpe-end
             call converti_out_ce(pgrato_app(iel))
	     end if
	end do

c--------->CENTRALI SEMPLIFICATE
	do iel=otipti(e_cs),etipti(e_cs)
	     call calcola_derivati_out_cs(iel,pgrato(iel))
           call converti_out_cs(pgrato(iel))
	end do

      return
c 9999 return 1
      end
!----------------------------------------------------------------------------------
      Subroutine agg_dire()

c[|   Aggiorna i valori in funzione della direzione

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'   
      include '../inc/tj.inc'   

      integer*4 iel,ivet
      

      do ivet=1,ne_uni
       do iel=otipti(te_uni(ivet)),etipti(te_uni(ivet))
         qsettj(iel) = diretj(iel)*qsettj(iel)
         porttj(iel) = diretj(iel)*porttj(iel)
       enddo
      enddo
      return
      end
!----------------------------------------------------------------------------------
!----------------------------------------------------------------------------------
      Subroutine definisci_connessioni()

!-Definisce le connessioni di rete

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni

      integer*4 iel,ipu,last_e,last_u,ivet
      integer*4 el_primo_en,el_primo_us
      logical*2 primo_en,primo_us
!-------------------------------------------
      primo_en=.false.
      primo_us=.false.
!-------------------------------------------



!-PUNTI
    
      do ipu=otipti(e_pu),etipti(e_pu)
      

         oseeto(ipu)=ipu
         oseuto(ipu)=ipu
         last_e=ipu
         last_u=ipu
	  




         do ivet=1,ne_bin
          do iel=otipti(te_bin(ivet)),etipti(te_bin(ivet))

             if(ipu.eq.opuvto(iel))then
                oseeto(last_e)=iel
                oseeto(iel)=ipu

                last_e=iel
             endif

             if(ipu.eq.opumto(iel))then
                oseuto(last_u)=iel
                oseuto(iel)=ipu

                last_u=iel
             endif
          enddo
         enddo

!-UNARI
         do ivet=1,ne_uni
          do iel=otipti(te_uni(ivet)),etipti(te_uni(ivet))
             if(ipu.eq.opuvto(iel))then

                oseeto(last_e)=iel
                oseeto(iel)=ipu



                last_e=iel
             endif
          enddo
         enddo
      enddo
!-------------------------------------------
      do ivet=ne_uni,1,-1
         do iel=etipti(te_uni(ivet)),otipti(te_uni(ivet))-1
            oseuto(iel)=off_oi
            if(primo_en)then
               oseuto(last_e)=iel
            else
               primo_en=.true.
               el_primo_en=iel
            endif
            last_e=iel
          enddo
      enddo
!-------------------------------------------
!-poiche' tutti unari entranti
      el_primo_us=off_dp
!-------------------------------------------
      oseeto(off_oi)=i_und
      if(primo_us)then
        oseeto(off_dp)=el_primo_us
      else
        oseeto(off_dp)=off_dp
      endif
      oseeto(off_re)=off_re

      if(primo_en)then
        oseuto(off_oi)=el_primo_en
      else
        oseuto(off_oi)=off_oi
      endif
      oseuto(off_dp)=i_und
      oseuto(off_re)=off_re
!-------------------------------------------


      return
      end
!-------------------------------------------------------------------------------
      subroutine calcola_derivati_top

!-Calcola i dati derivati TOP topologia degli elementi

      implicit none
      include '../inc/param.inc' !parametri
      include '../inc/default.inc'      !top default
      include '../inc/ti.inc'           !top connessioni
      include '../inc/tx.inc'           !top dati caratteristici
	INCLUDE '../inc/top_app_ele.inc'  !top dati caratteristici ulteriori
      include '../inc/cc.inc'           !costanti gas
      include '../inc/cap_bon_prv.inc'  !puntatore alla fase_2 della centrale 
                                        !in serie 
      include '../inc/STATIONS.inc'           

      integer*4 iel,pm,pv,ie,j
      logical*2 ce_serie
      external  ce_serie
      real*4 perc_qprog  /0.20/       ! Percentuale (20%) di aumento della portata  
	 								! di progetto per valorizzare la portata massima
!-------------------------------------------
!-definizione costanti chimiche gas (CC.inc)
!-------------------------------------------
      call definisci_cost_cc
!-------------------------------------------
!- e_pu: PUNTI
!-------------------------------------------
!**dati calcolati:
!-volume         !volntx()
!-energia pot.   !renptx()

      do iel=otipti(e_pu),etipti(e_pu)

         renptx(iel) = altetx(iel) * agrvcc/cjoucc
         fl_nocap(pgrato(iel)) = .true.
	   ie = oseuto(iel)
	   do while (ie .ne. iel .and. fl_nocap(pgrato(iel)))
		  if(htipto(ie) .eq. e_tr) fl_nocap(pgrato(iel)) = .false.
      	  ie = oseuto(ie)
         enddo
	   ie = oseeto(iel)
	   do while (ie .ne. iel .and. fl_nocap(pgrato(iel)))
		  if(htipto(ie) .eq. e_tr) fl_nocap(pgrato(iel)) = .false.
      	  ie = oseeto(ie)
         enddo
	   if (fl_nocap(pgrato(iel))) then
           volntx(iel) = 1
	   else
           volntx(iel) = 0
	   endif
      enddo
!-------------------------------------------
!- e_tr: TRONCHI
!-------------------------------------------
!**dati calcolati di tronco:
!-dislivello !disltx() 
!-volume     !volntx()
!**dati calcolati di punto:
!-volume         !volntx()

      do iel=otipti(e_tr),etipti(e_tr)
         pm=opumto(iel)
         pv=opuvto(iel)
       
         disltx(iel)=altetx(pv)-altetx(pm)
         volntx(iel)=rluntx(iel)*diamtx(iel)*diamtx(iel)*
     -               (pgr4cc/2)/1000.

!-definizione volume punti monte e valle
         volntx(pm) = volntx(pm) + volntx(iel)
         volntx(pv) = volntx(pv) + volntx(iel)

!->lambda (Serghides)
!         call lambd1 (iel)
      enddo
!-------------------------------------------
!- e_ce: CENTRALI
!-------------------------------------------
!**dati calcolati:
!default-rendimento          !rendtx()
!  "    -tem.att.air cooler  !tarctx()
!  "    -consumo energetico  !ceuptx() 
!  "    -portata massima     !qmaxtx()	da portata di progetto

      do iel=otipti(e_ce),etipti(e_ce)
c questi campi vengono settati come default per una centrale non semplificata e
c vengono utilizzati nella function qconce
         rendtx(iel) = rend_def
c         tarctx(iel) = tarc_def
         tarctx(iel) = max_tout(pgrato(iel))

         ceuptx(iel) = ceup_def
	   qmaxtx(iel) = flow_prog(pgrato(iel)) + 
     -                   flow_prog(pgrato(iel))*perc_qprog

         call calcola_derivati_top_ce(pgrato(iel))
         if (ce_serie(iel)) then
           call calcola_derivati_top_ce(pgrato_app(iel))
         endif
      end do
!---------------------------------------------------------
!- ELEMENTI SPECIALI
!---------------------------------------------------------
c   assegno al ritardo caratteristico il valore di default 
c   fissato a 0.1666667 ore (10 minuti)

      do j=1,ne_spe
         do iel=otipti(te_spe(j)),etipti(te_spe(j))
	      trittx(iel) = trit_def
	   end do
	end do

      return
      end
!-------------------------------------------------------------------------------
      subroutine calcola_derivati_cin (*)

!-Calcola i dati derivati CIN condizioni iniziali degli elementi

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/default.inc'   !parametri
      include '../inc/flag.inc'   !parametri
      include '../inc/top_app_ele.inc'    !top appoggio dati
      include '../inc/ti.inc'             !top connessioni
      include '../inc/tx.inc'             !top dati caratteristici
      include '../inc/th.inc'             !cin tipi di stato
      include '../inc/tj.inc'             !cin condizioni iniziali
      include '../inc/tc.inc'             !cin caratteristiche gas
      include '../inc/ty.inc'             !cin dati dinamici
      include '../inc/td.inc'             !cin caratteristiche gas
      include '../inc/tz.inc'             !
      include '../inc/cc.inc'             !costanti gas
      include '../inc/stazione.inc'
      include '../inc/units.inc'          !unita'
      include '../inc/stations.inc'       !stazione
      include '../inc/cap_bon_prv.inc'             !costanti gas
      include '../inc/err.inc'             !costanti gas
      include '../inc/staznw_par.inc'   !parametri

      integer*4 iel,jel,pm,pv,ivet,j

      logical*2 ce_serie
      logical*2 attiva

	character*(l_D_ERR) msgerr
	real*4 pcls_real
      external pcls_real
!---------------------------------------
!-definizione:
  ! stato regolazione,set,portata,pressione,temperatura
  !stato regolazione definito     !hstatj()
  !composizione gas               !comptc(,jel) (jel=1,m_comp)
  !set temperatura risc.          !tsettj()
  !comprimibilita' in cond. nom.  !zm0td()
  !densita'in cond. nom.          !ro0td()
  !rapporto cost. gas e peso mol. !ravotd()
  !potere calorifico inferiore    !pclitd()
  !capacita' termica a vol.cost.  !ctvctd()
  !capacita' termica a pres.cost. !ctpctd()
!-------------------------------------------

      if(tipoScenario.eq.TIPOSCEN_RETE_din)then
!-    Lettura Ripartenza Dinamico
         call ripartenza (msgerr,*9999)
         call ripartenza_compos ()
c         call ripartenza_centrali ()
crsa_pcls_din
crsa_pcls_din	   call rete_comp_def ()   ! la dinamica potrebbe lavora alla
	                           ! composizione di default
                                 ! ma bisogna considerare che per fare questo 
	                           ! bisogna portare le portate alle comp def
	                           ! e lavorare con portate diverse si ottengono
	                           ! risultati diversi
      else
!-------------------------------------------
!-calcola composizione gas media da spalmare
!-------------------------------------------
         if (.not.fl_sim) then
           call spalma_tc()
         endif
         call default_cin()
      endif

!-------------------------------------------
!- e_pu: PUNTI
!-------------------------------------------
      do iel=otipti(e_pu),etipti(e_pu)

         pclstd(iel) = PCLS_REAL(iel)

!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)

!-definizione di coef.comprimibilita' e densita'
         call agg_punto(iel)
c-elena
c------->Definizione dello stato 
	   hstatj(iel) = sta_q
c-elena

      enddo
!-------------------------------------------
!- UNARI:
!-------------------------------------------
      do ivet=1,ne_uni
       do iel=otipti(te_uni(ivet)),etipti(te_uni(ivet))
c         pv=opuvto(iel)

         pclstd(iel) = PCLS_REAL(iel)

         touttz(iel) = tsettj(iel)
!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)

! Setto le variabili interessate dalla direzione
c         qsettj(iel) = diretj(iel)*qsettj(iel)
         if (diretj(iel) .gt. 0) then
           qmaxtx(iel) = qmintx(iel)
         else
           qmaxtx(iel) = qmoutx(iel)
         endif

       enddo
      enddo
!-------------------------------------------
!- VALVOLE:
!-------------------------------------------
      do ivet=1,ne_vlv
       do iel=otipti(te_vlv(ivet)),etipti(te_vlv(ivet))

         pclstd(iel) = PCLS_REAL(iel)

         touttz(iel) = (temptj(opumto(iel))+temptj(opuvto(iel)))/2.0
         
!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)

         call tout_valv(iel)

cT!-definizione tsettj = set temperatura riscaldatore
cT         tsettj(iel) = tsettx(iel)

!-per gli elemnti speciali binari setto la direzione positiva sempre da pmonte a pvalle
         diretj(iel) = 1

       enddo
      enddo
!-------------------------------------------
!- e_ce: CENTRALI
!-------------------------------------------
      do iel=otipti(e_ce),etipti(e_ce)

         pclstd(iel) = PCLS_REAL(iel)

         touttz(iel) = (temptj(opumto(iel))+temptj(opuvto(iel)))/2.0

         call calcola_derivati_cin_ce(pgrato(iel))
         if (ce_serie(iel)) then
           call calcola_derivati_cin_ce(pgrato_app(iel))
         endif
!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)

         call agg_centrale(iel)

c-14/6/6 riconoscimento dei tipi e riordino dei vettori di tbg
         call riordina_unita(iel)

!-per gli elementi speciali binari setto la direzione positiva sempre da pmonte a pvalle
         diretj(iel) = 1

      end do
!-------------------------------------------
!- e_cs: CENTRALI SEMPLIFICATE
!-------------------------------------------
      do iel=otipti(e_cs),etipti(e_cs)

         pclstd(iel) = PCLS_REAL(iel)

         touttz(iel) = (temptj(opumto(iel))+temptj(opuvto(iel)))/2.0

c-sire2_0060
         call calcola_derivati_cin_cs(pgrato(iel))
c         taria_cs(pgrato(iel)) = taria_def(stagione)
c-sire2_0060-end

!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)
cgpe-new
         call agg_centrale(iel)
cgpe-new-end

!-per gli elemnti speciali binari setto la direzione positiva sempre da pmonte a pvalle
         diretj(iel) = 1

      end do
!-------------------------------------------
!- e_tr: TRONCHI
!-------------------------------------------
!*loop tronchi deve essere dopo quello dei punti
!-------------------------------------------
      do iel=otipti(e_tr),etipti(e_tr)

         pclstd(iel) = PCLS_REAL(iel)

cT
         tambtz(iel) = (tambtx(opumto(iel))+tambtx(opuvto(iel)))/2.0
         touttz(iel) = (temptj(opumto(iel))+temptj(opuvto(iel)))/2.0
!-calcola derivati cin comuni a tutti gli elementi
         call derivati_cin_comuni(iel)

!-definizione temperatura,coef.comprimibilita',pressione,densita',portata
c         porttj(iel) = 0.

c         call agg_tronco_q(iel)
         call agg_tronco(iel)
         call pipent(iel,opumto(iel),opuvto(iel))
!-eventuale calcolo di portata trigger (con velocita'=5 m/s)
c         if(prestj(pm) .eq. prestj(pv)) then
c            porttj(iel) = (diamtx(iel)**3
c     -      *rocitj(iel)*3.6*pgr4cc*5.0) / ro0td(iel)
c         endif

      enddo
!-----------------------------------------------------------
	call agg_dire()

      if(tipoScenario.eq.TIPOSCEN_RETE_din)then
         call ripartenza_centrali ()
         call ripartenza_set ()
      endif

      return
9999  continue
      return 1
      end
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      subroutine spalma_tc()

c[|   Calcola il valore medio delle composizioni e temperature e le spalma

      implicit none
      include '../inc/param.inc'
      include '../inc/Ti.inc'
      include '../inc/Tj.inc'
      include '../inc/Tc.inc'
      include '../inc/Th.inc'
      include '../inc/Tv.inc'
      include '../inc/flag.inc'
      include '../inc/default.inc'
      include '../inc/staznw_par.inc'
      include '../inc/conv.inc'
      include '../inc/err.inc'

      integer*4 iel,itt,ic,j
      real*4    sm_portat
      real*4    compap(m_comp),tempap,portap
      integer*4 fl_int,fl_end
	real*4 pcls_real, pcs_dato
      external pcls_real
c-UDM
      character*(100) msg_int,msg 
	integer*4 max_len_st
	parameter (max_len_st=25)
      character*(max_len_st) udm_out_str
      external                udm_out_str
c-UDM-end
!---------------------------------------------------
c inizializzazioni

      sm_portat = 0.0
      tempap = 0.0
      do ic=1,m_comp
        compap(ic) = 0.0
      enddo

      do itt=1,ne_uni
        do iel=otipti(te_uni(itt)),etipti(te_uni(itt))
          if (diretj(iel) .eq. 1) then
            if (htcitj(iel) .eq. tc_q  .and. 
     *          qsettj(iel) .gt. repsTC     ) then
              portap = qsettj(iel)
            else
              portap = repsTC
            endif
            sm_portat = sm_portat + portap
            tempap = tempap + tsettj(iel)*portap
            do ic =1,m_comp
              compap(ic) = compap(ic)+comptc(iel,ic)*portap
            enddo
	      pcs_dato = pcls_real(iel)
	      msg_int=trim(te_stri(te_uni(itt)))//' : '//trim(snomtv(iel)) 
	      fl_int=.false.
            if (pcs_dato.lt.pcsmin_fnz) then
              call tipo_err (iel,err_ww,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='PCS inferiore al minimo consentito.'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,630) udm_out_str(udm_pcs,pcs_dato,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
            elseif (pcs_dato.gt.pcsmax_fnz) then
              call tipo_err (iel,err_ww,msg)
	        call messtel(fl_int,fl_end,msg_int,msg)
	        msg='PCS superiore al massimo consentito.'
	        call messtel(fl_int,fl_end,msg_int,msg)
              write(msg,630) udm_out_str(udm_pcs,pcs_dato,1)
	        call messtel(fl_int,fl_end,msg_int,msg)
	      endif
crsa_pcs
          endif
        enddo
      enddo

      if (sm_portat.eq.0.0) then
        tempap = temp_def
        call norm_compos(compap)
        sm_portat = 1.0
      endif

      do ic=1,m_comp
        compap(ic) = compap(ic)/sm_portat
        do iel=otipti(e_pu),etipti(e_pu)
          comptc(iel,ic) = compap(ic)
        enddo
        do iel=otipti(e_tr),etipti(e_tr)
          comptc(iel,ic) = compap(ic)
        enddo
        do j=1,ne_vlv
	    do iel=otipti(te_vlv(j)),etipti(te_vlv(j))
            comptc(iel,ic) = compap(ic)
          enddo
	  end do

        do j=1,ne_cen
	    do iel=otipti(te_cen(j)),etipti(te_cen(j))
            comptc(iel,ic) = compap(ic)
          enddo
        end do

        do itt=1,ne_uni
          do iel=otipti(te_uni(itt)),etipti(te_uni(itt))
            if(diretj(iel) .eq. -1) comptc(iel,ic) = compap(ic)
crsacomp
cace
            comptc_set(iel,ic) = comptc(iel,ic)   ! valorizzo set di composizione
	                                            ! da utente o da spalma
cace
crsacomp
          enddo
        enddo
      enddo

      tempap = tempap/sm_portat
      do itt=1,ne_uni
        do iel=otipti(te_uni(itt)),etipti(te_uni(itt))
          if(diretj(iel) .eq. -1) then
             tsettj(iel) = tempap
          endif
        enddo
      enddo

      return
630   format('PCS: ',A25)
      end
!-------------------------------------------------------------------------------
      subroutine norm_compos(comp_def_n)

!- Normalizza la composizione gas di default

      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'

      real*4 comp_def_n(*)

      integer*4 gas
!-----------------------------------------
      comp_def_n(gas_met)      = gas_met_def
      comp_def_n(gas_et)       = gas_et_def
      comp_def_n(gas_prop)     = gas_prop_def
      comp_def_n(gas_but)      = gas_but_def
      comp_def_n(gas_ibut)     = gas_ibut_def
      comp_def_n(gas_pent)     = gas_pent_def
      comp_def_n(gas_es)       = gas_es_def
      comp_def_n(gas_n2)       = gas_n2_def
      comp_def_n(gas_co2)      = gas_co2_def
      comp_def_n(gas_h2s)      = gas_h2s_def


!#ifdef NOINT_2219
!      ! normalizzazione:
!      do gas=1,m_comp
!         comp_def_n(gas) = comp_def_n(gas)
!      enddo
!#else
!#endif

      return
      end
!-------------------------------------------------------------------------------
      subroutine derivati_cin_comuni(iel)

!- Calcola i dati derivati cin comuni a tutti gli elementi

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni
      include '../inc/tc.inc'             !cin caratteristiche gas

      integer*4 iel   !I
!-----------------------------------------
!-inizializzazione parametri per REDLICH-KWONG
!-------------------------------------------
      call par_comp_rk(iel,m_ogg,comptc)
      CALL GASTD (iel)
!-------------------------------------------
      return
      end
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
      subroutine default_cin()

!- Setta i dati derivati cin con valori di default

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/default.inc'   !parametri
      include '../inc/ti.inc'             !top connessioni
      include '../inc/tj.inc'             !cin
      include '../inc/th.inc'         
      include '../inc/td.inc'         
      include '../inc/tx.inc'         

      integer*4 iel,ivet

	real*4 pcls_real
      external pcls_real
	real*4 qset_real
	external qset_real

!-----------------------------------------
      do iel=otipti(e_pu),etipti(e_pu)
        prestj(iel) = pres_def
!       temptj(iel) = temp_def
        temptj(iel) = tambtx(iel)
      enddo

      do iel=otipti(e_tr),etipti(e_tr)
        porttj(iel) = port_def
      enddo

      do ivet=1,ne_spe
        do iel=otipti(te_spe(ivet)),etipti(te_spe(ivet))
           if (htcitj(iel).eq.tc_off) then
             porttj(iel) = 0
           elseif (htcitj(iel).eq.tc_q.or.
     -             htcitj(iel).eq.tc_pq) then
             porttj(iel) = qset_real(iel)
           else
             porttj(iel) = port_def
           endif
           if (htcitj(iel).eq.tc_pv.or.
     -             htcitj(iel).eq.tc_npv) then
             prestj(opuvto(iel)) = pvsetj(iel)
           elseif (htcitj(iel).eq.tc_pm.or.
     -             htcitj(iel).eq.tc_npm) then
             prestj(opuvto(iel)) = pmsetj(iel)
           endif
           hstatj(iel) = stadatc(htcitj(iel))
        enddo
      enddo

      return
      end
!-------------------------------------------------------------------------------
!----------------------------------------------------------------------
      SUBROUTINE agg_pcls ()      ! aggiorna pcs

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TD.inc'
*&*

********
      integer*4 i_elem
      integer*4 gg
	
	real*4 pcls_real
      external pcls_real
!-------------------------------------------------------
      do gg = 1, ne_gen
        do i_elem = otipti(te_gen(gg)), etipti(te_gen(gg))
           pclstd(i_elem) = pcls_real (i_elem)
        end do
      enddo

      return
      end

      SUBROUTINE rete_comp_def ()     ! porta la rete
	                                ! alla composizione di default

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/default.inc '    !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
      include '../inc/TC.inc'
*&*

********
      integer*4 i_elem
      integer*4 gg, ic

      real*4    compap(m_comp)

      real*4 pcls_real, pcls_app
      external pcls_real
	
!-------------------------------------------------------
      pcls_app = pcls_def
      do gg = 1, ne_gen
        do i_elem = otipti(te_gen(gg)), etipti(te_gen(gg))
           pclstd(i_elem) = pcls_real(i_elem)
           porttj(i_elem) = porttj(i_elem)*pclstd(i_elem)/pcls_def
           call norm_compos(compap)
           do ic=1,m_comp
             comptc(i_elem,ic) = compap(ic)
           enddo
           pclstd(i_elem) = pcls_real(i_elem)
           porttj(i_elem) = porttj(i_elem)*pcls_def/pclstd(i_elem)
        end do
      enddo

      return
      end

      SUBROUTINE agg_port_pcls_def ()  ! riporta tutte le portate degli elementi 
	                                 ! speciali al pcls di default

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/default.inc '    !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
*&*

********
      integer*4 i_elem
      integer*4 i_spe
	
!-------------------------------------------------------
      do i_spe = 1, ne_spe
        do i_elem = otipti(te_spe(i_spe)), etipti(te_spe(i_spe))
           porttj(i_elem) = porttj(i_elem)*pclstd(i_elem)/pcls_def
        end do
      enddo

      return
      end

      SUBROUTINE agg_port_pcls_cor ()  ! riporta tutte le portate degli elementi 
	                                 ! speciali al pcls corrente

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/default.inc '    !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
*&*

********
      integer*4 i_elem
      integer*4 i_spe
	
!-------------------------------------------------------
      do i_spe = 1, ne_spe
        do i_elem = otipti(te_spe(i_spe)), etipti(te_spe(i_spe))
           porttj(i_elem) = porttj(i_elem)*pcls_def/pclstd(i_elem)
        end do
      enddo

      return
      end

      SUBROUTINE agg_prel ()      ! aggiorna prelievi

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/jc.inc'       !:out
*&*

********
      integer*4 i_elem
	
!-------------------------------------------------------
        do i_elem = otipti(e_pu), etipti(e_pu)
           porttj(i_elem) = valsjc (i_elem)
        end do

      return
      end

      SUBROUTINE ripartenza_compos()   ! recupera composizione

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
      include '../inc/Tc.inc'
      include '../inc/staznw_par.inc'
*&*

********
      integer*4 i_elem,te,gas,oi
	
!-------------------------------------------------------
      do te = 1, ne_bin
        do i_elem = otipti(te_bin(te)), etipti(te_bin(te))
          if (porttj(i_elem).lt.-zeroTC) then
	      oi = opuvto(i_elem)
	    else
	      oi = opumto(i_elem)
	    endif
          do gas = 1, m_comp
            comptc(i_elem,gas) = comptc(oi,gas)
          enddo
        end do
      enddo

      do te = 1, ne_uni
        do i_elem = otipti(te_uni(te)), etipti(te_uni(te))
	    oi = opuvto(i_elem)
          do gas = 1, m_comp
	      if(diretj(i_elem) .eq. -1) 
     -                             comptc(i_elem,gas) = comptc(oi,gas)
Crsa      queste ultime assegnazioni per la dinamica 
Crsa      potrebbero risultare superflue
            comptc_set(i_elem,gas) = comptc(i_elem,gas)
            if (porttj(i_elem).lt.-zeroTC)
     -                             comptc(i_elem,gas) = comptc(oi,gas)
          enddo
        end do
      enddo

      return
      end

      Subroutine ripartenza_centrali ()  ! recupera dati delle centrali

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/nw.inc'
*&*
      integer*4  kflag
      real*4     sum_cons
      logical*2  l_rec_ce ! e' valorizzato per la rete
c----------------------------------------------------------------
      lcent_last = .false.
      sum_cons = 0.
      l_rec_ce = .false.

c-din
c-din      call sim_cent (sum_cons,l_rec_ce)
       kflag = 0
       call sim_cent (sum_cons,l_rec_ce,kflag)
c-din-end

      return
      end

      SUBROUTINE ripartenza_set()   ! recupera set

      implicit none
*&*
      include '../inc/param.inc '      !: costante
      include '../inc/default.inc '      !: costante
      include '../inc/scenario.inc '  
      include '../inc/TI.inc'
      include '../inc/TJ.inc'
      include '../inc/TD.inc'
      include '../inc/Th.inc'
      include '../inc/rampa.inc'
*&*
********
      integer*4 i_elem,te
	
	real*4 qset_real
	external qset_real
!-------------------------------------------------------
      do te = 1, ne_spe
       do i_elem = otipti(te_spe(te)), etipti(te_spe(te))
        if (hstatj(i_elem).eq.STA_OFF) then
	    qsettj(i_elem) = 0.
          delta_man(i_elem) = qset_real(i_elem)
     1                          -porttj(i_elem)
        elseif (hstatj(i_elem).eq.STA_ON) then
	    continue
        elseif (hstatj(i_elem).eq.STA_FR) then
	    continue
        elseif (hstatj(i_elem).eq.STA_Q) then
          delta_man(i_elem) = qset_real(i_elem)
     1                          -porttj(i_elem)
        elseif (hstatj(i_elem).eq.STA_PV) then
	    delta_man(i_elem) = pvsetj(i_elem)-prestj(opuvto(i_elem))
        elseif (hstatj(i_elem).eq.STA_PM) then
	    delta_man(i_elem) = pmsetj(i_elem)-prestj(opumto(i_elem))
        else
           continue
        endif
	  tempo_man(i_elem) = ist_iniz
       enddo
      enddo

      return
      end
