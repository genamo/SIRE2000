*** 77PJAC.FOR *** calcolo dello jacobiano per il metodo di newton *******
cgpe Subroutine prese da ssdd$src:pse_levmarq.for

      subroutine creajac_ssdd (ia,ja,*)
cgpe coincide con la subroutine pse_inijac (ia,ja,*)
cgpe a cui è stata aggiunta la gestione di elementi speciali con più equazioni
cgpe presa da ssdd$src:ott_newton.for

!     Crea i vettori indice riga e colonna della matrice jacobiana e i
!     vettori contenenti il tipo (lineare, quadratico) ma non i
!     coefficienti delle equazioni del sistema.
!     L'insieme di questi vettori definisce completamente la struttura del
!     sistema di equazioni.
*&*
      implicit none
      include '../inc/param.inc'  !:costante
      include '../inc/stazione.inc'    !:costante
cgpe      include '../inc/pse.inc'    !:costante
      include '../inc/nw.inc'    !:costante
      include '../inc/dimension.inc'    !:costante
      include '../inc/th.inc'	    !:costante
      include '../inc/ti.inc'	    !:in
      include '../inc/tj.inc'	    !:in
cgpe      INCLUDE '../inc/cap_bon_prv.inc/nolist'
*&*
      integer*4  ia(*)     ! O  vettore indici riga della matrice sistema
     *          ,ja(*)     ! O  vettore indici colonna ""   ""
*&*
      integer*4 IEQ,JACNT,I,J,IM,IV
      integer*4 IELE
      real*4    XE,XEEA
      logical*2 elem_bin
	external  elem_bin
	integer*4 oicen
c-elena
      logical*2 elem_cen
	external  elem_cen
      
!-----------------------------------------------------------------------
!  Inizializzazione puntatori e contatori
      ieq = 1
!      oice = otipti(e_ce) - (M_OGG + 1)
      ia(1) = 1
      jacnt = 1

!----------------------------------------------------------------------
!  Ciclo sui tronchi di condotta (equazioni di Fergusson)
!----------------------------------------------------------------------

      do i = otipti(e_tr), etipti(e_tr)
        im = opumto(i)
        iv = opuvto(i)

!  Controlla che non sia superata la dimensione massima dello jacobiano
        if(jacnt+3 .gt. m_jac) goto 100

!  Variabili dell'equazione (portata, p di monte, p di valle)
        ja(jacnt) = nw_topvar(i)
        ja(jacnt + 1) = nw_topvar(im)
        ja(jacnt + 2) = nw_topvar(iv)
        jacnt = jacnt + 3

        ieq = ieq + 1
        ia(ieq) = jacnt
      enddo


!----------------------------------------------------------------------
!	Ciclo sugli elementi speciali
!----------------------------------------------------------------------

      do j = 1, ne_spe
        do i = otipti(te_spe(j)), etipti(te_spe(j))
          im = opumto(i)
          iv = opuvto(i)

!-mi-21/09/99
!!  Se la regolazione e' on o non ritorno:
!  Se la regolazione e' on o flusso unidirezionale:
!  - sta_fr deriva da tc_on, quindi sono ammissibili entrambe le direzi
!    del flusso
!  - sta_on deriva da tc_nr, e' pertanto non ammessa portata negativa.
!    Se lo stato ricostruito calcola una portata negativa, viene
!    considerato uno stato illegale e viene segnalato nel file di log
!-mi-21/09/99-end
          if (hstatj(i) .eq. sta_on .or. hstatj(i) .eq. sta_fr) then

            if (jacnt+2 .gt. m_jac) goto 100

!  P di monte = P di valle
            ja(jacnt) = nw_topvar(im)
            ja(jacnt + 1) = nw_topvar(iv)
            jacnt = jacnt + 2

            ieq = ieq + 1
            ia(ieq) = jacnt

!-mi-21/09/99
!!  Se la regolazione e' in off
!  Se la regolazione e' in off o non ritorno:
!-mi-21/09/99-end
          elseif(hstatj(i) .eq. sta_off .or. hstatj(i) .eq. sta_nr) then

            if (jacnt+1 .gt. m_jac) goto 100

!  Portata = 0
            ja(jacnt) = nw_topvar(i)
            jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!  Se la regolazione e' in portata
          elseif (hstatj(i) .eq. sta_q .or.
     *            hstatj(i) .eq. sta_qm .or.
     *            hstatj(i) .eq. sta_qc     ) then

            if (jacnt+1 .gt. m_jac) goto 100

!  Portata = Q di set
            ja(jacnt) = nw_topvar(i)
            jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!  Se la regolazione e' in pressione a monte
          elseif (hstatj(i) .eq. sta_pm) then

            if (jacnt+1 .gt. m_jac) goto 100

!  P di monte = P di set
            ja(jacnt) = nw_topvar(im)
            jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!  se la regolazione e' in pressione a valle o in Pw max o ro max
          elseif (hstatj(i) .eq. sta_pv .or.
     *            hstatj(i) .eq. sta_pw .or.
     *            hstatj(i) .eq. sta_rom    ) then

            if (jacnt+1 .gt. m_jac) goto 100

!  P di valle = P di set
            ja(jacnt) = nw_topvar(iv)
            jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

cgpe-new
!  da ssdd$src:ott_newton.for viene introdotta la gestione di elementi speciali
!  per i quali si scrivono più equazioni

	    elseif(hstatj(i) .eq. sta_p ) then
!	se la regolazione e' in pressione (valle e monte)

!	Controlla che non sia superata la dimensione massima dello jacobiano
  	      if(jacnt+2 .gt. m_jac) goto 100

!	P di valle = P di set
	      ja(jacnt) = nw_topvar(iv)
		  jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!	P di monte = P di set
            ja(jacnt) = nw_topvar(im)
		  jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

	    elseif(hstatj(i) .eq. sta_pvq ) then
!	se la regolazione e' in pressione a valle e in portata

!	Controlla che non sia superata la dimensione massima dello jacobiano
  	      if(jacnt+2 .gt. m_jac) goto 100

!	P di valle = P di set
            ja(jacnt) = nw_topvar(iv)
            jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!	Portata = Q di set
            ja(jacnt) = nw_topvar(i)
		  jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

	    elseif(hstatj(i) .eq. sta_pmq ) then
!	se la regolazione e' in pressione a monte e in portata

!	Controlla che non sia superata la dimensione massima dello jacobiano
  	      if(jacnt+2 .gt. m_jac) goto 100

!	P di monte = P di set
            ja(jacnt) = nw_topvar(im)
		  jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

!	Portata = Q di set
		  ja(jacnt) = nw_topvar(i)
		  jacnt = jacnt + 1

            ieq = ieq + 1
            ia(ieq) = jacnt

	    elseif(hstatj(i) .eq. sta_pq ) then
!	se la regolazione e' in pressione (monte e valle) e in portata

!	Controlla che non sia superata la dimensione massima dello jacobiano
            if(jacnt+3 .gt. m_jac) goto 100

!	P di valle = P di set
   	      ja(jacnt) = nw_topvar(iv)
		  jacnt = jacnt + 1

	      ieq = ieq + 1
	      ia(ieq) = jacnt

!	P di monte = P di set
		  ja(jacnt) = nw_topvar(im)
		  jacnt = jacnt + 1

            ieq = ieq + 1
	      ia(ieq) = jacnt

!	Portata = Q di set
		  ja(jacnt) = nw_topvar(i)
		  jacnt = jacnt + 1

	      ieq = ieq + 1
	      ia(ieq) = jacnt
cgpe-new-end

          elseif (hstatj(i) .eq. sta_und) then
!  Regolazione indefinita, tipo di regolazione prevista 

!!!     Per questo tipo di regolazione non viene scritta alcuna equazione     !!!

          else
cgpe   errs stz_09
cgpe            call gest_error(2,0,'pse_inijac',
cgpe     *                    'Regolazione indefinita',0)
            call gest_error(2,0,'Creajac',
     *                     'Regolazione non prevista',0)
	      goto 100
          endif

        end do
      enddo


!----------------------------------------------------------------------
!  Ciclo sui nodi per le equazioni lineari relative (somma delle
!  portate entranti e uscenti su un nodo = 0)
!----------------------------------------------------------------------

      do i = otipti(e_pu), etipti(e_pu)

        iele = oseeto(i)

!  Ciclo sugli elementi entranti
        do while (iele.ne.i)
cgpe
!!         if (htipto(iele).gt.0) then
cgpe-end
          if (jacnt+1 .gt. m_jac) goto 100

          ja(jacnt) = nw_topvar(iele)
          jacnt = jacnt + 1
cgpe
!!         endif
cgpe-end
          iele = oseeto(iele)
        enddo

        iele = oseuto(i)

!  Ciclo sugli elementi uscenti
        do while (iele.ne.i)

          if (jacnt+1 .gt. m_jac) goto 100

          ja(jacnt) = nw_topvar(iele)
          jacnt = jacnt + 1

c          if (htipto(iele).eq.e_ce) then    ! centrali bin. o una. in stoccaggio
c              ja(jacnt) = nw_topvar(iele-oice)
c              jacnt = jacnt + 1
c          end if
c-elena ---->tengo conto delle centrali semplificate
          if (elem_cen(iele)) then    ! centrali bin/sempl o una. in stoc
             if (htipto(iele).eq.e_ce) then
                oicen=oice
             else
                oicen=oics
             end if              
             ja(jacnt) = nw_topvar(iele-oicen)
             jacnt = jacnt + 1
          end if
c-elena
          iele = oseuto(iele)
        enddo


        ieq = ieq + 1
        ia(ieq) = jacnt
	
      enddo

!----------------------------------------------------------------------
!	Ciclo sulle centrali per i consumi
!----------------------------------------------------------------------
c-elena --->considero anche le centrali semplificate
      do j=1,ne_cen
         if (te_cen(j).eq.e_ce) then
            oicen=oice
         else
            oicen=oics
         end if
	   do i=otipti(te_cen(j)),etipti(te_cen(j))
c      oicen = oice
c      do i = otipti(e_ce), etipti(e_ce)
cgpe-new
c            if (lcent_first) then
            if (i.lt.0) then                       !lcent_first
cgpe-new-end
              im = opumto(i)
              iv = opuvto(i)
              if (elem_bin(i)) then
               if (jacnt+4 .gt. m_jac) goto 100

               ja(jacnt) = nw_topvar(iv)
               ja(jacnt + 1) = nw_topvar(im)
               ja(jacnt + 2) = nw_topvar(i)
               ja(jacnt + 3) = nw_topvar(i-oicen)
               jacnt = jacnt + 4
              elseif (htipto(im) .eq. e_pu) then

               if (jacnt+3 .gt. m_jac) goto 100

               ja(jacnt) = nw_topvar(im)
               ja(jacnt + 1) = nw_topvar(i)
               ja(jacnt + 2) = nw_topvar(i-oicen)
               jacnt = jacnt + 3

              else

               if (jacnt+3 .gt. m_jac) goto 100

               ja(jacnt) = nw_topvar(iv)
               ja(jacnt + 1) = nw_topvar(i)
               ja(jacnt + 2) = nw_topvar(i-oicen)
               jacnt = jacnt + 3
              endif

cgpe-new
            else

! consumo = consumo del giro precedente
              ja(jacnt) = nw_topvar(i-oicen)
              jacnt = jacnt + 1

	      endif
cgpe-new-end

            ieq = ieq + 1
            ia(ieq) = jacnt

         enddo
      end do

!  Controlla che non sia superata la dimensione massima dello jacobiano
      if (jacnt .gt. m_jac) goto 100

      return ! creajac

100     CONTINUE

cgpe stz_03 
      call gest_error(1,0,'CREAJAC',
     *                  'Dimensione di jac insuff.',0)

      RETURN  1 !  creajac_ssdd

      end  

!======================================================================
*** 77PJAC.FOR *** calcolo dello jacobiano per il metodo di newton *******
