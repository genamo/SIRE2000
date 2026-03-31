*** 77MSTAZ.FOR *** ricerca soluzione stazionaria ***************************
!======================================================================
cgpe tolgo la cpretc_nc e verstel_nc_rk
cgpe      subroutine cpretc_nc( !Calcola portate dei prelievi e telecomandi
**********************************************************************************

	subroutine aggstel(fl_blk,iel_mod)
c***********************************************************************************
c   Questa subroutine viene chiamata all'interno del loop per la risoluzione 
c   stazionaria della rete col metodo di Newton per verificare la fattibilità 
c   della soluzione trovata negli elementi speciali.Nasce da una rielaborazione
c   della aggstel di SIRE.Sono stati eliminati il controllo sui punti,la 
c   routine LNSTA(quindi i messaggi di errore lanciati a video tra una iterazione 
c   e la successiva) e i messaggi a video sono stati sostituiti con chiamate alla
c   subroutine gest_error
c-----------------------------------------------------------------------------------
c  Le verifiche fatte per il rispetto dei vincoli fisici e dei valori di set sugli
c  elementi speciali sono eseguiti a blocchi secondo il tipo di elementi
c************************************************************************************
      implicit none

	include '../inc/param.inc '	! :costanti
	include '../inc/default.inc ' ! :default
	include '../inc/th.inc '	! :in
	include '../inc/ti.inc'	    ! :in
	include '../inc/tx.inc '	! :in
	include '../inc/tj.inc '	! :in/
	include '../inc/td.inc '	! :in/
	include '../inc/tz.inc '	! :in/
	include '../inc/jc.inc '	! :outout
	include '../inc/top_app_ele.inc '	! :out
c-elena
	include '../inc/cap_bon_prv.inc '	! :out
	include '../inc/stazione.inc '	! :out
	include '../inc/stations.inc '	! :out
	include '../inc/staznw_par.inc '	! :out
c-elena
c------>INPUT
c
c
      integer*4 fl_blk    !indica quanti blocchi di controllo vogliamo
	                    !eseguire (in totale abbiamo 5 blocchi);
	                    !0   non esegue controlli
						!1   unari
						!2   unari+valvole
						!>2  unari+valvole+centrali
c
c------>OUTPUT
c 
      integer*4 iel_mod   !indica quale belemento è stato modificato
	                    !0 nessuna modifica
c
c------>Parametri locali
c
      logical*2 l_ripr    !indica se ripristinare set e valori
	                    !.T. si
	                    !.F. no

	integer*4 i,j,k,hstasav, sflag
	real*4 ptest,pvrmax

      logical*2 lvar   !controlla i cambiamenti di stato

      real*4 qmax, pasp_stat_dp, pmand_stat_dp
	real*4 qmaxpcsreal, qsetpcsreal
	logical*2 flag_vlim, fl_ok
c Function
	logical*2 set_non_ritorno ,set_condir
	external set_non_ritorno ,set_condir
	logical*2 set_stapres, set_stapm, set_stapv
	external set_stapres, set_stapm, set_stapv
      real*4 qset_real
	external qset_real
      real*4 ppwmce,qpwmce,dp_in,dp_out
	external ppwmce,qpwmce,dp_in,dp_out
	logical*2 ce_off, ce_avail
	external ce_off, ce_avail

      integer*4 ii_vl,im,iv
      logical*2 ltest_qmax
!----------------------------------------------------------------------
c-elena
c	lpzero = .false.   !controlla la positività della pressione nei punti
c-elena
c
      iel_mod=0


      if (fl_blk .lt. 0) then
	  return
      endif

	lvar = .false.
c     
c---->BLOCCO 0: Per simulazione senza recupero 
!               vengono comunque trattate le valvole che comprimono
c      if (fl_blk .eq. 0) then
c----------------------------------------------------------------------------
c   Valvole + Impianti
c---------------------------------------------------------------------------
       do j=1,ne_vlv
	   do i=otipti(te_vlv(j)),etipti(te_vlv(j))
	     if (n_agg(i).le.n_agg_max) then
             l_ripr = .true.
             if ((prestj(opuvto(i))-prestj(opumto(i))).gt.repsP) then
               if (htcitj(i).eq.tc_npv) then
   	           l_ripr = .false.
  	           hstatj(i)=sta_on
  	           goto 1111
	         elseif (htcitj(i).eq.tc_pv) then
   	           l_ripr = .false.
  	           hstatj(i)=sta_fr
  	           goto 1111
	         endif
		   endif
           end if
         end do
	 end do

c	 return
c      endif

      if (fl_blk .eq. 0) then
	  return
      endif
c     
c---->BLOCCO 1:
c----------------------------------------------------------------------------
!   Elementi Unari
!---------------------------------------------------------------------------
      do j=1,ne_uni
	   do i=otipti(te_uni(j)),etipti(te_uni(j))
	     if (n_agg(i).le.n_agg_max) then
             l_ripr = .true.
		   if(prestj(opuvto(i)) .lt. pmintx(i)) then
	         l_ripr = .false.
               if (te_uni(j).eq.e_pr) then
C              controllo particolare per i prelievi interrompibili
         	       if(hstatj(i).ne.sta_off) then
                   if(hstatj(i).eq.sta_pv) then 
			   !Modifica che potrebbe compromettere la regolazione in P della rete
                     hstasav = hstatj(i)
	               hstatj(i)=sta_off
				   call cntr_set(fl_ok)
	               if (fl_ok) then
	                 goto 1111
	               else
	                 hstatj(i) = hstasav
	               endif
				 else
	               hstatj(i)=sta_off
  	               goto 1111
				 endif
                 end if
               elseif (hstatj(i) .ne. sta_pv .or.
     -           valsjc(i) .ne. pmintx(i))then
                 hstatj(i)=sta_pv	
                 valsjc(i) = pmintx(i)
                 goto 1111
               end if
 		   endif
		   if(prestj(opuvto(i)) .gt. pmaxtx(i)) then
	         l_ripr = .false.
               if (hstatj(i) .ne. sta_pv .or.
     -           valsjc(i) .ne. pmaxtx(i))then
                 hstatj(i)=sta_pv	
                 valsjc(i) = pmaxtx(i)
                 goto 1111
               end if
 		   endif
		   if(diretj(i)*porttj(i).lt.-repsQ) then
		     if(set_non_ritorno(i)) then
	           l_ripr = .false.
         	       if(hstatj(i).ne.sta_off) then
                   if(hstatj(i).eq.sta_pv) then 
			   !Modifica che potrebbe compromettere la regolazione in P della rete
                     hstasav = hstatj(i)
	               hstatj(i)=sta_off
		           call cntr_set(fl_ok)
	               if (fl_ok) then
	                 goto 1111
	               else
	                 hstatj(i) = hstasav
	               endif
		         else
	               hstatj(i)=sta_off
  	               goto 1111
				 endif
                 endif
 		     endif
	       endif
		   if(abs(porttj(i)).gt.qmaxtx(i)) then
cqpcls	         qmaxpcsreal = qmaxtx(i)*pcls_def/pclstd(i)
	         qmaxpcsreal = qmaxtx(i)
		     l_ripr = .false.
         	     if(hstatj(i).ne.sta_q) then
                 if(hstatj(i).eq.sta_pv) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)=sta_q
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = diretj(i)*qmaxpcsreal
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)=sta_q
                   valsjc(i) = diretj(i)*qmaxpcsreal
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.diretj(i)*qmaxpcsreal) then
	           valsjc(i) = diretj(i)*qmaxpcsreal
	           goto 1111
	         endif
             end if


C Il ripristino così come è fatto può portare a un loop
C    Imponendo il flag uguale a FALSE viene evitato il ripristino
C     lasciando il sotware per studi futuri 
             l_ripr = .false.
      	   if (l_ripr) then
!  Controlla se è necessario un ripristino
	         qsetpcsreal = qset_real(i)  
			 if (hstatj(i).ne. stadatc(htcitj(i))) then
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                 hstasav = hstatj(i)
	           hstatj(i)=stadatc(htcitj(i))
			   call cntr_set(fl_ok)
	           if (fl_ok) then
	             lvar = .true.
	           else
	             hstatj(i) = hstasav
	           endif
               endif
               if (hstatj(i) .eq. sta_pv .and.
     -             valsjc(i) .ne. pvsetj(i)) then
                 valsjc(i) = pvsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_pm .and.
     -                 valsjc(i) .ne. pmsetj(i)) then
                 valsjc(i) = pmsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_q .and.
     -                 valsjc(i) .ne. qsetpcsreal) then
                 valsjc(i) = qsetpcsreal
                 lvar = .true.
               endif
               if (lvar) goto 1111
             endif
           endif
         end do
	end do

c	if(lvar) then
c	  return
c	end if

      if (fl_blk .eq. 1) then
	  return
      endif

c     
c---->BLOCCO 2:
c----------------------------------------------------------------------------
c   Valvole + Impianti
c---------------------------------------------------------------------------
      do j=1,ne_vlv
	   do i=otipti(te_vlv(j)),etipti(te_vlv(j))
c-e_vt: si esclude il trattamento delle valvole di stacco, perché non deve esserne
c       modificato lo stato
          if (htipto(i) .ne. e_vt) then
c-e_vt-end
	     if (n_agg(i).le.n_agg_max) then
             l_ripr = .true.
	       if(porttj(i).lt.-repsQ) then
		     if(set_non_ritorno(i) .or.
     -          abs(prestj(opumto(i))-prestj(opuvto(i))).gt.repsP) then
	           l_ripr = .false.
         	       if(hstatj(i).ne.sta_off) then
                   if(set_stapres(i)) then 
			   !Modifica che potrebbe compromettere la regolazione in P della rete
                     hstasav = hstatj(i)
	               hstatj(i)=sta_off
			       call cntr_set(fl_ok)
	               if (fl_ok) then
	                 goto 1111
	               else
	                 hstatj(i) = hstasav
	               endif
				 else
	               hstatj(i)=sta_off
  	               goto 1111
				 endif
                 endif
!	         else
!crsa_fr	           if (htcitj(i).ne.tc_on) then
!	             l_ripr = .false.
!         	         if(hstatj(i).ne.sta_fr) then
!	               hstatj(i)= sta_fr
!  	               goto 1111
!	             endif
!crsa_fr 		       endif
			 endif
             endif
             if ((set_condir(i) .and. 
     -          abs(prestj(opumto(i))-prestj(opuvto(i))).gt.repsP) .or.
     -          ((prestj(opumto(i)).lt.prestj(opuvto(i))-repsP).and.
     -            (htcitj(i).ne.tc_on))) then
	         l_ripr = .false.
crsa_fr-i
crsa_fr         	     if(hstatj(i).ne.sta_off) then
         	     if(hstatj(i).ne.sta_off.and.hstatj(i).ne.sta_fr) then
	           hstatj(i)=sta_fr
  	           goto 1111
         	     elseif(hstatj(i).ne.sta_off) then
crsa_fr-f
                 if(set_stapres(i)) then 
			 !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)=sta_off
		    	 call cntr_set(fl_ok)
	             if (fl_ok) then
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)=sta_off
  	             goto 1111
			   endif
               endif
 	       endif
c      la regolazione con set tc_pq richiede che Pm>pmset
		   if(htcitj(i) .eq. tc_pq .and. 
     *             prestj(opumto(i)) .lt.pmsetj(i)) then
               l_ripr = .false.
         	     if(hstatj(i).ne.sta_pm) then
                 if(set_stapv(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)=sta_pm
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = pmsetj(i)
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)=sta_pm
                   valsjc(i) = pmsetj(i)
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.pmsetj(i)) then
	           valsjc(i) = pmsetj(i)
	           goto 1111
	         endif
             endif

	       if (te_vlv(j).eq.e_vg) call qmaxvg(i,qmaxtx(i),flag_vlim)
cgpe-corr-e_vd
c Aggiunto il test per gli impianti di riduzione:
c qmaxtx ha senso per valori di deltaP superiori al dpmin    ============>  modifica (vedi seguito)
c Controllare che per il ripristino non sia necessario anche testare il valore di
c ltest_qmax9o
             ltest_qmax = .true.
c             if (te_vlv(j) .eq. e_vd) then  ! si ipotizza che il gas passi sempre nell'impianto per cui il controllo
c               ii_vl = pgrato(i)            ! sulla qmax viene fatto sempre
c               im = opumto(i)
c               iv = opuvto(i)
c               if ((prestj(im)-prestj(iv)) .lt. dpmin(ii_vl)) then
c                 ltest_qmax = .false.
c               endif
c             endif
            if (ltest_qmax) then
cgpe-corr-e_vd-end
		   if(abs(porttj(i)) .gt. qmaxtx(i)) then
cqpcls               qmaxpcsreal = qmaxtx(i)*pcls_def/pclstd(i)
               qmaxpcsreal = qmaxtx(i)
               qmaxpcsreal = sign(qmaxpcsreal,porttj(i)) 
			 l_ripr = .false.
         	     if(hstatj(i).ne.sta_q) then
                 if(set_stapres(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)=sta_q
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = qmaxpcsreal
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)=sta_q
                   valsjc(i) = qmaxpcsreal
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.qmaxpcsreal) then
	           valsjc(i) = qmaxpcsreal
	           goto 1111
	         endif
	       endif
cgpe-corr-e_vd
            endif !ltest_qmax
cgpe-corr-e_vd-end

C Il ripristino così come è fatto può portare a un loop
C    Imponendo il flag uguale a FALSE viene evitato il ripristino
C     lasciando il sotware per studi futuri 
             l_ripr = .false.
      	   if (l_ripr) then
!  Controlla se è necessario un ripristino
	         qsetpcsreal = qset_real(i)  
			 if (hstatj(i).ne. stadatc(htcitj(i))) then
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                 hstasav = hstatj(i)
	           hstatj(i)=stadatc(htcitj(i))
			   call cntr_set(fl_ok)
	           if (fl_ok) then
	             lvar = .true.
	           else
	             hstatj(i) = hstasav
	           endif
               endif
               if (hstatj(i) .eq. sta_pv .and.
     -             valsjc(i) .ne. pvsetj(i)) then
                 valsjc(i) = pvsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_pm .and.
     -                 valsjc(i) .ne. pmsetj(i)) then
                 valsjc(i) = pmsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_q .and.
     -                 valsjc(i) .ne. qsetpcsreal) then
                 valsjc(i) = qsetpcsreal
                 lvar = .true.
               endif
               if (lvar) goto 1111
             endif
           endif
c-e_vt
          endif
c-e_vt-end
         enddo
      enddo


c	if(lvar) then
c	  return
c	end if

      if (fl_blk .eq. 2) then
	  return
      endif
c     
c---->BLOCCO 3:
c----------------------------------------------------------------------------
c   Centrali (binarie,assetti,semplificate)
c---------------------------------------------------------------------------
      do j=1,ne_cen
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
	     if (n_agg(i).le.n_agg_max) then
             l_ripr = .true.
	       if(porttj(i).lt.-repsQ) then
		     if(set_non_ritorno(i) .or.
     -          abs(prestj(opumto(i))-prestj(opuvto(i))).gt.repsP) then
	           l_ripr = .false.
         	       if(hstatj(i).ne.sta_off) then
                   if(set_stapres(i)) then 
			   !Modifica che potrebbe compromettere la regolazione in P della rete
                     hstasav = hstatj(i)
	               hstatj(i)= sta_off
			       call cntr_set(fl_ok)
	               if (fl_ok) then
	                 goto 1111
	               else
	                 hstatj(i)= hstasav
	               endif
				 else
	               hstatj(i)= sta_off
  	               goto 1111
				 endif
                 endif
!	         else
!crsa_fr	           if (htcitj(i).ne.tc_on) then
!	             l_ripr = .false.
!         	         if(hstatj(i).ne.sta_fr) then
!	               hstatj(i)= sta_fr
!  	               goto 1111
!	             endif
!crsa_fr 		       endif
			 endif
             endif
             if ((set_condir(i) .and. 
     -          abs(prestj(opumto(i))-prestj(opuvto(i))).gt.repsP) .or.
     -          ((prestj(opumto(i)).gt.prestj(opuvto(i))+repsP).and.
     -            (htcitj(i).ne.tc_on))) then
	         l_ripr = .false.
crsa_fr-i
crsa_fr         	     if(hstatj(i).ne.sta_off) then
         	     if(hstatj(i).ne.sta_off.and.hstatj(i).ne.sta_fr) then
	           hstatj(i)=sta_fr
  	           goto 1111
         	     elseif(hstatj(i).ne.sta_off) then
crsa_fr-f
                 if(set_stapres(i)) then 
			 !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_off
		    	 call cntr_set(fl_ok)
	             if (fl_ok) then
	               goto 1111
	             else
	               hstatj(i)= hstasav
	             endif
			   else
	             hstatj(i)= sta_off
  	             goto 1111
			   endif
               endif
 	       endif
 	     if (htipto(i).eq.e_cs) then   ! solo per le centrali semplificate
c      la regolazione con set tc_pq richiede che Pm>pmset
		   if(htcitj(i) .eq. tc_pq .and. 
     *             prestj(opumto(i)) .lt.pmsetj(i)) then
               l_ripr = .false.
         	     if(hstatj(i).ne.sta_pm) then
                 if(set_stapv(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_pm
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = pmsetj(i)
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)= sta_pm
                   valsjc(i) = pmsetj(i)
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.pmsetj(i)) then
	           valsjc(i) = pmsetj(i)
	           goto 1111
	         endif
             endif

             if(porttj(i).gt.qpwmce(i))then
               l_ripr = .false.
		     ptest = ppwmce(i)
         	     if(hstatj(i).ne.sta_pv) then
                 if(set_stapm(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_pv
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = ptest
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)= sta_pv
                   valsjc(i) = ptest
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.ptest) then
	           valsjc(i) = ptest
	           goto 1111
	         endif
             endif
	    
             pvrmax = (prestj(opumto(i)) - 
     -            dpin(pgrato(i)))*romax(pgrato(i)) - dpout(pgrato(i))
		   if(prestj(opuvto(i)) .gt. pvrmax + repsP) then
         	     if(hstatj(i).ne.sta_pv) then
                 if(set_stapm(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_pv
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = pvrmax
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)= sta_pv
                   valsjc(i) = pvrmax
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.pvrmax) then
	           valsjc(i) = pvrmax
	           goto 1111
	         endif
	       endif
c calcolo delle pressioni in funzione ai delta_p dovuti al piping
             pasp_stat_dp  = prestj(opumto(i)) - dpin(pgrato(i))
             pmand_stat_dp = prestj(opuvto(i)) + dpout(pgrato(i))

!-test sulla pressioni
             if (pasp_stat_dp  .le. 0 .or.
     *           pmand_stat_dp .le. 0 .or.
     *           pmand_stat_dp .lt. pasp_stat_dp) then
crsa_fr-i
crsa_fr         	     if(hstatj(i).ne.sta_fr) then
         	     if(hstatj(i).ne.sta_off.and.hstatj(i).ne.sta_fr) then
crsa_fr-f
	           hstatj(i)= sta_fr
  	           goto 1111
crsa_fr-i
         	     elseif(hstatj(i).ne.sta_off) then
                 if(set_stapres(i)) then 
			 !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_off
		    	 call cntr_set(fl_ok)
	             if (fl_ok) then
	               goto 1111
	             else
	               hstatj(i)= hstasav
	             endif
			   else
	             hstatj(i)= sta_off
  	             goto 1111
			   endif
crsa_fr-f
	         endif
             endif

 	     else   ! Centrali non semplificate
c calcolo delle pressioni in funzione ai delta_p dovuti al piping
             pasp_stat_dp  = prestj(opumto(i)) -
     *                       dp_in(i,prestj(opumto(i)),
cgpe-corr     *                       tasp_stat(pgrato(i)),porttj(i))
     *                       temptj(opumto(i)),porttj(i))
cgpe-corr-end
             pmand_stat_dp = prestj(opuvto(i)) + 
     *                       dp_out(i,prestj(opuvto(i)),
     *                       stat_vars(7,pgrato_app(i)),porttj(i))

!-test sulla pressioni
             if (pasp_stat_dp  .le. 0 .or.
     *           pmand_stat_dp .le. 0 .or.
     *           pmand_stat_dp .lt. pasp_stat_dp) then
crsa_fr-i
crsa_fr         	     if(hstatj(i).ne.sta_fr) then
         	     if(hstatj(i).ne.sta_off.and.hstatj(i).ne.sta_fr) then
crsa_fr-f
	           hstatj(i)= sta_fr
  	           goto 1111
crsa_fr-i
         	     elseif(hstatj(i).ne.sta_off) then
                 if(set_stapres(i)) then 
			 !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_off
		    	 call cntr_set(fl_ok)
	             if (fl_ok) then
	               goto 1111
	             else
	               hstatj(i)= hstasav
	             endif
			   else
	             hstatj(i)= sta_off
  	             goto 1111
			   endif
crsa_fr-f
	         endif
             endif

c test sulla pressione di mandata della centrale se maggiore della pressione di mandata massima
c definita in topologia
             call set_stato_stazione(sflag,i)
             if (sflag .ne. 0 .and. 
     *           prestj(opuvto(i)) .gt. stat_vars(6,pgrato_app(i))) then
         	     if(hstatj(i).ne.sta_pv) then
                 if(set_stapm(i)) then 
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_pv
			     call cntr_set(fl_ok)
	             if (fl_ok) then
                     valsjc(i) = stat_vars(6,pgrato_app(i))
	               goto 1111
	             else
	               hstatj(i) = hstasav
	             endif
			   else
	             hstatj(i)= sta_pv
                   valsjc(i) = stat_vars(6,pgrato_app(i))
  	             goto 1111
			   endif
	         elseif (valsjc(i).ne.stat_vars(6,pgrato_app(i))) then
	           valsjc(i) = stat_vars(6,pgrato_app(i))
	           goto 1111
	         endif
             endif

             if ((sflag .ne. 0 .and. 
     *            ce_off(prestj(opumto(i)),prestj(opuvto(i)))) .and.
     *     	   .not.ce_avail(pgrato(i)) .or. 
     *           .not.ce_avail(pgrato_app(i))) then
crsa_fr-i
crsa_fr         	     if(hstatj(i).ne.sta_fr) then
         	     if(hstatj(i).ne.sta_off.and.hstatj(i).ne.sta_fr) then
crsa_fr-f
	           hstatj(i)= sta_fr
  	           goto 1111
crsa_fr-i
         	     elseif(hstatj(i).ne.sta_off) then
                 if(set_stapres(i)) then 
			 !Modifica che potrebbe compromettere la regolazione in P della rete
                   hstasav = hstatj(i)
	             hstatj(i)= sta_off
		    	 call cntr_set(fl_ok)
	             if (fl_ok) then
	               goto 1111
	             else
	               hstatj(i)= hstasav
	             endif
			   else
	             hstatj(i)= sta_off
  	             goto 1111
			   endif
crsa_fr-f
	         endif
             endif
 	     endif

C Il ripristino così come è fatto può portare a un loop
C    Imponendo il flag uguale a FALSE viene evitato il ripristino
C     lasciando il sotware per studi futuri 
             l_ripr = .false.
      	   if (l_ripr) then
!  Controlla se è necessario un ripristino
               qsetpcsreal = qset_real(i)     
			 if (hstatj(i).ne. stadatc(htcitj(i))) then
		     !Modifica che potrebbe compromettere la regolazione in P della rete
                 hstasav = hstatj(i)
	           hstatj(i)=stadatc(htcitj(i))
			   call cntr_set(fl_ok)
	           if (fl_ok) then
	             lvar = .true.
	           else
	             hstatj(i) = hstasav
	           endif
               endif
               if (hstatj(i) .eq. sta_pv .and.
     -             valsjc(i) .ne. pvsetj(i)) then
                 valsjc(i) = pvsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_pm .and.
     -                 valsjc(i) .ne. pmsetj(i)) then
                 valsjc(i) = pmsetj(i)
                 lvar = .true.
               elseif (hstatj(i) .eq. sta_q .and.
     -                 valsjc(i) .ne. qsetpcsreal) then
                 valsjc(i) = qsetpcsreal
                 lvar = .true.
               endif
               if (lvar) goto 1111
             endif
           endif
         enddo
      enddo


c	if(lvar) then
c	  return
c	end if

      if (fl_blk .eq. 3) then
	  return
      endif

	return 

1111  continue

      n_agg(i) = n_agg(i) + 1

      iel_mod=i

	return 

	end  !AGGSTEL


	subroutine cntr_set(fl_ok)
c***********************************************************************************
c   Questa subroutine controlla che i set in pressione 
c     siano tali da regolare tutta la rete
C---- Attenzione: non è stato ritenuto necessario il controllo che 
C                  un punto sia settato in P da un e uno solo elemento
C                 Qualora si ritenesse necessario introdurre il controllo 
C                  bisognerà chiamare la subroutine anche quando 
C                  viene imposto un nuovo SET in pressione 
c************************************************************************************
      implicit none

	include '../inc/param.inc '	! :costanti
	include '../inc/top_app_ele.inc '	! :in
	include '../inc/th.inc '	! :in
	include '../inc/ti.inc'	    ! :in
	include '../inc/tj.inc'	    ! :in
	include '../inc/tv.inc '	! :in
c
      logical*2 fl_ok

	logical*2 fl_nomod
	integer*4 j,i

	do i=otipti(e_pu),etipti(e_pu)
	  pu_regp(pgrato(i)) = 0
	end do

      do j=1,ne_spe
	   do i=otipti(te_spe(j)),etipti(te_spe(j))
            if (hstatj(i).eq.sta_pv) then
	          pu_regp(pgrato(opuvto(i)))=-1
            else if(hstatj(i).eq.sta_pm) then
	          pu_regp(pgrato(opumto(i)))=-1
            end if
	   end do
	end do


      fl_nomod = .true. 
      do while (fl_nomod)
	 fl_nomod = .false. 
       do j=1,ne_bin
	   do i=otipti(te_bin(j)),etipti(te_bin(j))
               if(hstatj(i).eq.sta_on.or.
     *	        hstatj(i).eq.sta_fr.or.
     *			te_bin(j).eq.e_tr) then
	           if    (pu_regp(pgrato(opumto(i))).eq.-1.and.
     *                  pu_regp(pgrato(opuvto(i))).ne.-1) then
  	             pu_regp(pgrato(opuvto(i)))=-1
                   fl_nomod = .true. 
	           elseif(pu_regp(pgrato(opuvto(i))).eq.-1.and.
     *                  pu_regp(pgrato(opumto(i))).ne.-1) then
  	             pu_regp(pgrato(opumto(i)))=-1
                   fl_nomod = .true. 
               end if
            end if
	   end do
       end do
	end do

      fl_ok = .true. 
	do i=otipti(e_pu),etipti(e_pu)
	  if (pu_regp(pgrato(i)).ne.-1) then
	    fl_ok = .false. 
        end if
	end do
	return 

	end  !cntr_set
