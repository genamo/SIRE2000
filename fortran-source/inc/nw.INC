*** NW.INC *** Parametri & Area Common per lo stazionario  metodo Newton ************
*** DA SSDD$INC:PSE.INC *** Parametri & Area Common per lo pseudo-stazionario *******
*** DA sire$inc:JAC_NEW.INC *** Parametri & Area Common per lo pseudo-stazionario ***
!----------------------------------------------------------------------
c-elena
c      integer*4 nw_topvar(m_ogg+max_ce)  ! Corrispondenza tra indice topologico 
c                                         ! e indice vettore di stato
      integer*4 nw_topvar(m_ogg+max_ce+max_cs)  ! Corrispondenza tra indice topologico 
                                                ! e indice vettore di stato

      integer*4 oice     ! Passo fisso per avere l'indice dell'elemento
                         ! topologico "fittizio" corrispondente al consumo
                         ! delle CE (oice = otipti(e_ce)-(M_OGG+1))
      integer*4 oics     ! Passo fisso per avere l'indice dell'elemento
                         ! topologico "fittizio" corrispondente al consumo
                         ! delle centrali semplificate CS
						 ! (oics = otipti(e_cs)-(M_OGG+max_ce+1))

      integer*4 i_elem_ko     ! L'indice dell'elemento che ha mandato in crisi il Newton

      common /newton/ nw_topvar,oice,oics,i_elem_ko

cgpe dal common sire$inc:JAC_NEW.INC
      logical*2 lcent_first
      logical*2 lcent_last
      common /jaccent/ lcent_first,lcent_last
cgpe-end
!----------------------------------------------------------------------
*** NW.INC *** Parametri & Area Common per lo stazionario  metodo Newton ************
