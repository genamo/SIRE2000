!   pacchetto contente le subroutine e le function per le verifiche di trattabilità di un
!   nodo di smistamento
!*****************************************************************************************
!
!   vg_tab   --> MATRICE DEGLI STATI COLLETTORI-PUNTI RETE
!                descrive lo stato delle valvole  elettriche poste all'incrocio tra i 
!                collettori di nodo e le linee di grigliato;gli stati possibili sono:
!                ON   --->bypass       --->parametro associato 1
!                OFF  --->disgiunzione --->parametro associato 0
!                FREE --->stato libero --->parametro associato 100
!                RID  --->riduzione    --->parametro associato 2 se la riduzione è dal 
!                                          collettore al punto oppure -2 se la riduzione
!                                          è dal punto al collettore
!   vg_tab_T --> MATRICE DEGLI STATI PUNTI RETE-COLLETTORI
!                trasposta della matrice precedente
!   cl_tab   --> MATRICE DELLE RIDUZIONI COLLETTORE-COLLETTORE:
!                all'incrocio tra due collettori:
!                * il valore 1 ci dice che è stata inserita una centrale con aspirazione 
!                  nel primo collettore e mandata nel secondo
!                * il valore 2 ci dice che è stata inserita una riduzione 
!                  con punto monte nel primo collettore e punto valle nel secondo
!                * il valore -1 che tra i due collettori c'è un elemento binario ma il 
!                  link esaminato non è possibile perchè viola la direzionalità dell'elemento
!                * il valore 0 ci dice che nessun elemento collega direttamente i due 
!                  collettori
!                * i termini diagonali sono per convenzioni posti a 1.
!****************************************************************************************
      subroutine stato_pt_cl (n_cl,n_pt,vg_tab,vg_tab_t)
!----------------------------------------------------------------------------------------
!  Questa subroutine traspone la matrice degli stati. La matrice degli stati VG_TAB è una 
!  matrice COLLETTORE-PUNTO. La trasposta è una matrice PUNTO-COLLETTORE.
!  L'unico valore impattato dalla trasposizione è 2 relativo alla riduzione:
!  2--->riduzione da collettore a punto      -2--->riduzione da punto a collettore 
!----------------------------------------------------------------------------------------

      implicit none
	include '../inc/nodo_ridotto.inc '	

	integer*4 n_cl !numero collettori
     *         ,n_pt !numero punti griglia
	
	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)      !(I)
     *         ,vg_tab_t(max_lg_nsm,max_cl_nsm)    !(O)

	integer*4 k,kk
c----------------------------------------------------------------------------------------
c  INIZIALIZZO LE VARIABILI DI OUTPUT
   
	do k=1,n_pt
	   do kk=1,n_cl
            vg_tab_t(k,kk)=STATO_IND
         end do
	end do
      


	vg_tab_t = transpose(vg_tab)

	do k=1,n_pt
	   do kk=1,n_cl
	      if (vg_tab_t(k,kk).eq.STATO_RID_CL_PT) then
                vg_tab_t(k,kk)=STATO_RID_PT_CL
	      else if(vg_tab_t(k,kk).eq.STATO_RID_PT_CL) then
                vg_tab_t(k,kk)=STATO_RID_CL_PT
	      end if
         end do
	end do
     
	return
	end

!-------------------------------------------------------------------------------------
      logical*2 function punto_isolato (vg_tab,n_cl,ind_cl,i_pt)
!***************************************************************************************
!   Questa function serve per stabilire se il punto di rete con indice i_pt è isolato,
!   ossia non raggiungibile all'interno del nodo analizzato.Tale informazione si ricava
!   dalla tabella degli stati,dato che se un punto è isolato tutte le valvole elettriche
!   all'incrocio tra la linea di grigliato associata al punto e i collettori del nodo 
!   sono in stato off
!***************************************************************************************
      implicit none
	include '../inc/nodo_ridotto.inc '	

	
	
	integer*1 ind_cl(*)
      integer*4 n_cl, !numero effettivo di collettori
     *          i_pt  !indice del punto di rete analizzato
c	integer*4 vg_tab(max_cl_nsm,max_rt_nsm)   !cl*pt
	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)   !cl*pt
     
      integer*4 k,ind_k
     
      punto_isolato = .TRUE.
	do k=1,n_cl
	   if (vg_tab(k,i_pt).ne.STATO_OFF) then
	      punto_isolato= .FALSE.
	      exit
	   end if
	end do
	
	return
	end 

!----------------------------------------------------------------------------------------

      integer*4 function indice_gruppo (vg_tab,n_cl,ind_cl,i_pt1,i_pt2)
!****************************************************************************************
!  Questa function serve per stabilire se due punti di rete sono associati allo stesso 
!  collettore;se indice_gruppo=0 significa che i due punti sono associati a collettori
!  diversi,altrimenti indice_gruppo>0 individua il collettore su cui finiscono entrambi 
!  i punti
!****************************************************************************************
      implicit none
	include '../inc/nodo_ridotto.inc '	


	integer*4 n_cl !numero effettivo di collettori
     *         ,i_pt1 !indice del primo punto rete analizzato
     *         ,i_pt2 !indice del secondo punto rete analizzato
      integer*1 ind_cl(*)
      
c	integer*4 vg_tab(max_cl_nsm,max_rt_nsm)   !cl*pt
	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)   !cl*pt

      integer*4 k,ind_k
	 
	indice_gruppo = 0

	do k=1,n_cl
	   if (vg_tab(k,i_pt1).ne.STATO_OFF.and.
     *	   vg_tab(k,i_pt2).ne.STATO_OFF) then
	       indice_gruppo=k
             exit
	   end if
	end do

      return
	end

!---------------------------------------------------------------------------------------

      subroutine indice_collettore(vg_tab,n_cl,ind_cl,i_pt,count,indici)
!**************************************************************************************
!  Questa subroutine restituisce il numero di collettori a cui è associato il punto
!  i_pt e gli indici che permettono di identificare gli stessi
!**************************************************************************************
      implicit none
	include '../inc/nodo_ridotto.inc '	

	
	integer*4 n_cl !numero effettivo di collettori   (I)
     *         ,i_pt !indice del punto di rete analizzato  (I)
      integer*1 ind_cl(*)

c	integer*4 vg_tab(max_cl_nsm,max_rt_nsm)   !(I)  cl*pt
	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)   !(I)  cl*pt

      integer*4 count  ! numero di collettori a cui è associato il punto i_pt (O)
	integer*4 indici(*) ! indici dei collettori associati (O)  dim=max_cl
    
      integer*4 k,ind_k

	count = 0
	do k=1,n_cl
	   if (vg_tab(k,i_pt).ne.STATO_OFF) then
	      count= count + 1
	      indici(count) = k
	   end if
	end do

	return
	end
c-------------------------------------------------------------------------------------------
      subroutine punti_equivalenti(num_pu_gri,num_cl_ns,ind_cl,
     *                             vlg_stato_griglia,
     *                             count,pu_equivalenti)
c*******************************************************************************************
c    Dato un collettore,restitisce il numero(count) e i puntatori a i punti che stanno
c    su quel collettore e quindi sono a ugual pressione
c*******************************************************************************************
    
      implicit none

	include '../inc/nodo_ridotto.inc '	

	
	integer*4 num_pu_gri !numero di punti gri   (I)
     *         ,num_cl_ns
     *         ,ind_cl    !indice collettore da analizzare
	integer*4 vlg_stato_griglia(max_cl_nsm,max_lg_nsm)   !(I)

      integer*4 count  
	integer*4 pu_equivalenti(max_cl_nsm,max_lg_nsm)    !cl*pt
    
      integer*4 k,ind_k


	count = 0
	do k=1,num_pu_gri
c	   if (vlg_stato_griglia(ind_cl,k).ne.VLG_STATO_OFF) then
	   if (vlg_stato_griglia(ind_cl,k).eq.VLG_STATO_ON) then
	       count= count + 1
	       pu_equivalenti(ind_cl,count) = k
	   end if
	end do


	return
	end
c----------------------------------------------------------------------------------------------
      logical*2 function confronta_GUID(ielem,jelem,v1_guid,v2_guid)

      implicit none

	include '../inc/nodo_ridotto.inc '	
   
      integer*4 ielem,
     *          jelem
	integer*1 v1_guid(*), !dimensione effettiva=n_elem*lun
     *          v2_guid(*)
      integer*4 ii,jj,k

	confronta_guid=.true.

	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   if(v1_guid(ii+k).ne.v2_guid(jj+k))then
	      confronta_guid=.false.
	      exit 
	   end if
	end do

	return
	end
c----------------------------------------------------------------------------------------------
      logical*2 function confronta_GUID_mat(m1righe,m2righe,m2col,
     *                                ielem,jelem,n_link,
     *                                m1_guid,m2_guid)

      implicit none

	include '../inc/nodo_ridotto.inc '	
   
      integer*4 m1righe,m2righe,m2col,
     *          ielem,
     *          jelem,
     *          n_link
	integer*1 m1_guid(m1righe,*), !dimensione effettiva=n_elem*lun
     *          m2_guid(m2righe,m2col,*)
      integer*4 ii,jj,k

	confronta_guid_mat=.true.

	ii=lunGuid*(n_link-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   if(m1_guid(ielem,jj+k).ne.m2_guid(ielem,jelem,ii+k))then
	      confronta_guid_mat=.false.
	      exit 
	   end if
	end do

	return
	end
c--------------------------------------
      logical*2 function confronta_GUID_mat_forc(m1righe,m2righe,m2col,
     *                                elem_i,elem_j,ielem,jelem,n_link,
     *                                m1_guid,m2_guid)

      implicit none

	include '../inc/nodo_ridotto.inc '	
   
      integer*4 m1righe,m2righe,m2col,elem_i,elem_j,
     *          ielem,
     *          jelem,
     *          n_link
	integer*1 m1_guid(m1righe,*), !dimensione effettiva=n_elem*lun
     *          m2_guid(m2righe,m2col,*)
      integer*4 ii,jj,k,j

	confronta_guid_mat_forc=.true.

	j=lunGuid*(elem_j-1)    
	ii=lunGuid*(n_link-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
c	   if(m1_guid(ielem,jj+k).ne.m2_guid(ielem,jelem,ii+k))then
	   if(m1_guid(elem_i,j+k).ne.m2_guid(ielem,jelem,ii+k))then
	      confronta_guid_mat_forc=.false.
	      exit 
	   end if
	end do

	return
	end


c----------------------------------------------------------------------------------------------
      subroutine copia_Guid(ielem,jelem,v1_guid,v2)

c Da vettore a vettore

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 ielem,
     *          jelem
	integer*1 v1_guid(*), !dimensione effettiva=n_elem*lun
     *          v2(*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   v2(jj+k)=v1_guid(ii+k)
	end do

	return
	end
c----------------------------------------------------------------------------------------------
      subroutine copia_Guid_m(ielem,nrighe,j_riga,jelem,v1_guid,v2)

c Da vettore a matrice

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 nrighe,j_riga,
     *          ielem,
     *          jelem
	integer*1 v1_guid(*), !dimensione effettiva=n_elem*lun
     *          v2(nrighe,*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   v2(j_riga,jj+k)=v1_guid(ii+k)
	end do

	return
	end
c----------------------------------------------------------------------------------------------
      subroutine copia_Guid_dam2_am3(m2righe,i_riga,ielem,
     *                               m3righe,m3colonne,j_riga,j_colonna,
     *                              jelem,m2_guid,m3)

c Da matrice2 a matrice3 

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 m2righe,i_riga,
     *          m3righe,m3colonne,j_riga,j_colonna,
     *          ielem,
     *          jelem
	integer*1 m3(m3righe,m3colonne,*), !dimensione effettiva=n_elem*lun
     *          m2_guid(m2righe,*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   m3(j_riga,j_colonna,jj+k)=m2_guid(i_riga,ii+k)
	end do

	return
	end
c-----------------------------------------------------------------------------------------------
      subroutine copia_Guid_dam2_am3t(m2righe,i_riga,ielem,
     *                               m3righe,m3colonne,j_riga,j_colonna,
     *                              jelem,m2_guid,m3)

c Da matrice2 a matrice3 

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 m2righe,i_riga,
     *          m3righe,m3colonne,j_riga,j_colonna,
     *          ielem,
     *          jelem
	integer*1 m3(m3righe,m3colonne,*), !dimensione effettiva=n_elem*lun
     *          m2_guid(m2righe,*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   m3(j_riga,j_colonna,jj+k)=m2_guid(ii+k,i_riga)
	end do

	return
	end

c----------------------------------------------------------------------------------------------
      subroutine copia_Guid_dam3(ielem,nrighe,ncolonne,j_riga,j_colonna,
     *                           jelem,m3_guid,v)

c Da matrice3 a vettoree 

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 nrighe,ncolonne,j_riga,j_colonna,
     *          ielem,
     *          jelem
	integer*1 m3_guid(nrighe,ncolonne,*), !dimensione effettiva=n_elem*lun
     *          v(*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   v(jj+k)=m3_guid(j_riga,j_colonna,ii+k)
	end do

	return
	end
c--------------------------------------------------------------------------
      subroutine copia_Guid_dam2(ielem,nrighe,j_riga,jelem,m2_guid,v)

c Da matrice2 a vettoree 

	implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 nrighe,j_riga,
     *          ielem,
     *          jelem
	integer*1 m2_guid(nrighe,*), !dimensione effettiva=n_elem*lun
     *          v(*)

      integer*4 ii,jj,k
 
	ii=lunGuid*(ielem-1)
	jj=lunGuid*(jelem-1)

	do k=1,lunGuid
	   v(jj+k)=m2_guid(j_riga,ii+k)
	end do

	return
	end

C*********************************************************************************************
      subroutine connessione(n_cl,n_pt,ind_pu,ind_cl,
     *                       vg_tab,vg_tab_t,tipo_rid_gri,elem_id_gri,
     *                       cl_tab,cl_link_vg,elem_id_cl,
     *                       tipo_rid_gri_t,elem_id_gri_t,
     *                       conn,numero_link,tipo_link,warn,elem_id)
!**************************************************************************************
!  Crea la matrice delle connessioni punto punto
!*************************************************************************************
      implicit none
	include '../inc/nodo_ridotto.inc '	

	external punto_isolato,indice_gruppo
	logical*2 punto_isolato
	integer*4 indice_gruppo
	
	
	integer*4 n_cl !numero effettivo di collettori   (I)
     *         ,n_pt !numero di punti griglia          (I)
      integer*1 ind_pu(*)
     *         ,ind_cl(*)

	integer*4 vg_tab(max_cl_nsm,max_lg_nsm)   !(I)  cl*pt
     *         ,vg_tab_t(max_lg_nsm,max_cl_nsm) !(I)  pt_cl
     *         ,tipo_rid_gri(max_cl_nsm,max_lg_nsm) !(I)  cl*pt
     *         ,tipo_rid_gri_t(max_lg_nsm,max_cl_nsm)  !locale/output 
      integer*1 elem_id_gri(max_cl_nsm,max_lunguid*max_lg_nsm) !(I) cl*pt
     *         ,elem_id_gri_t(max_lunguid*max_lg_nsm,max_cl_nsm)  !locale/output 
      integer*4 cl_tab(max_cl_nsm,max_cl_nsm)     !(I) cl*cl
      integer*1 elem_id_cl(max_cl_nsm,max_cl_nsm*max_lunguid)     !(I) cl*cl
      logical*2 cl_link_vg(max_cl_nsm,max_cl_nsm)    !(I) cl*cl


      logical*2 conn(max_lg_nsm,max_lg_nsm) 
	                 !(O)matrice delle connessioni punto griglia - punto griglia	
	integer*4 tipo_link(max_lg_nsm,max_lg_nsm,max_link)   
	integer*4 numero_link(max_lg_nsm,max_lg_nsm)   
	integer*4 warn(max_lg_nsm,max_lg_nsm)  !1=doppia riduzione
	integer*1 elem_id(max_lg_nsm,max_lg_nsm,max_link*max_lunguid)   

c----------------------------------------->variabili locali
	integer*4 somma,i,j,ii,jj,k,icount,jcount,ind_i,ind_j,count,
     *         i1,i2,lun,
     *         i_indici(max_cl_nsm),j_indici(max_cl_nsm)

c**************************************************************************************************
c    INIZIALIZZO LE VARIABILI DI OUTPUT
	
	lun=max_link*lunguid

      do i=1,n_pt
        do j=1,n_pt
           warn(i,j)=0
           numero_link(i,j)=0
           conn(i,j)=.false.
           do k=1,max_link
              tipo_link(i,j,k)=NESSUN_LINK
	     end do
           do k=1,lun
              elem_id(i,j,k)=0
	     end do
	  end do
	end do

c    INIZIALIZZO LE VARIABILI LOCALI

      lun=n_pt*lunguid

      do i=1,n_cl
	   i_indici(i)=0
	   j_indici(i)=0
	end do
	
	do i=1,n_pt
	   do j=1,n_cl
	      tipo_rid_gri_t(i,j) = 0
         end do
	end do

	do i=1,lun
	   do j=1,n_cl
	      elem_id_gri_t(i,j) = 0
         end do
	end do



	tipo_rid_gri_t=transpose(tipo_rid_gri)
      elem_id_gri_t=transpose(elem_id_gri)
      
	do i=1,n_pt
	  if (punto_isolato (vg_tab,n_cl,ind_cl,i)) then   !if0
	      do j=1,n_pt
	         conn(i,j) =.false.     !punto isolato
	      end do 
	  else !if0
	      do j=1,n_pt
	         count=0
               if (j.eq.i) then   !if1  punti uguali
	            conn(i,j)=.true.
			 else  !if1     punti diversi

c                                  punti su collettori diversi
                  call indice_collettore(vg_tab,n_cl,ind_cl,i,
     *                                   icount,i_indici)

                  call indice_collettore(vg_tab,n_cl,ind_cl,j,
     *                                  jcount,j_indici)
	            loop1 :do ii = 1,icount
  	               loop2 :do jj=1,jcount
	                  IF (i_indici(ii).EQ.j_indici(jj)) THEN
		                  somma = vg_tab_t(i,i_indici(ii))+
     *     						  vg_tab(j_indici(jj),j)
	                      if (somma.gt.0) then !if4
c				              conn(i,j)=.true.
	                          if (somma.gt.2.and.somma.lt.4) then !if4
				                conn(i,j)=.true.
	                            if (tipo_rid_gri_t(i,i_indici(ii))
     *                                                 .eq.VR_GRI) then
	                                count=count+1
                                      numero_link(i,j)=count  
                                      tipo_link(i,j,count)=VR_GRI !vl rid gri
                                      call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                    i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                   else if( tipo_rid_gri(j_indici(jj),j)
     *												.eq.VR_GRI) then !if5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VR_GRI   !vl rid gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									j,max_lg_nsm,
     *                                    max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
	                             else if(tipo_rid_gri_t(i,i_indici(ii))
     *                                                 .eq.VD_GRI) then
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VD_GRI   !imp rid gri
                                      call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                     i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                   else if(tipo_rid_gri(j_indici(jj),j)
     *												.eq.VD_GRI) then !5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VD_GRI   !imp rid gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									 j,max_lg_nsm,
     *                                     max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
	                             else if(tipo_rid_gri_t(i,i_indici(ii))
     *                                                   .eq.VG_GRI)then
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VG_GRI   !imp reg gri
                                      call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                    i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                   else if(tipo_rid_gri(j_indici(jj),j)
     *												.eq.VG_GRI) then !5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VG_GRI   !imp reg gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									j,max_lg_nsm,
     *                                    max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
                                   end if !5  
							  else if (somma.eq.4) then 
c15/11				                   conn(i,j)=.false.
							       warn(i,j)=1
	                           
	                          else !if5
				                   conn(i,j)=.true.
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VL_SEZ  ! vl sez
   	                          end if !if5
c                                exit loop 
	                         end if  !i
	                ELSE if(cl_link_vg(i_indici(ii),j_indici(jj)))then !if3
	                         somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                         if (somma.gt.0) then !if4
c				              conn(i,j)=.true.
	                           if (somma.gt.2.and.somma.lt.4) then !if4
				                 conn(i,j)=.true.
	                             if (tipo_rid_gri_t(i,i_indici(ii))
     *												.eq.VR_GRI) then
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VR_GRI !vl rid gri
                                     call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                     i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                    else if(tipo_rid_gri(j_indici(jj),j)
     *												.eq.VR_GRI) then !if5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VR_GRI   !vl rid gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									j,max_lg_nsm,
     *                                    max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
	                              else if(tipo_rid_gri_t(i,i_indici(ii))
     *                                                  .eq.VD_GRI) then
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VD_GRI   !imp rid gri
                                      call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                     i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                    else if(tipo_rid_gri(j_indici(jj),j)
     *												.eq.VD_GRI) then !5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VD_GRI   !imp rid gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									 j,max_lg_nsm,
     *                                     max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
	                              else if(tipo_rid_gri_t(i,i_indici(ii))
     *													.eq.VG_GRI)then
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VG_GRI   !imp reg gri
                                      call copia_Guid_dam2_am3t
     *                                   (max_lunguid*max_lg_nsm,
     *                                    i_indici(ii),i,
     *                                    max_lg_nsm,max_lg_nsm,
     *                                    i,j,count,
     *                                    elem_id_gri_t,elem_id)
                                    else if(tipo_rid_gri(j_indici(jj),j)
     *												.eq.VG_GRI) then !5
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VG_GRI   !imp reg gri
                                      call copia_Guid_dam2_am3
     *                                   (max_cl_nsm,j_indici(jj),
     *									j,max_lg_nsm,
     *                                    max_lg_nsm,i,j,count,
     *                                    elem_id_gri,elem_id)
                                    end if !5  
							   else if (somma.eq.4) then 
c15/11				                   conn(i,j)=.false.
							       warn(i,j)=1	                           
	                           else !if5
				                   conn(i,j)=.true.
	                               count=count+1
                                     numero_link(i,j)=count  
                                     tipo_link(i,j,count)=VL_SEZ  !vl sez
   	                          end if !if5
c                                exit loop 
	                         end if  !if4
	                      end if

	                      if(cl_tab(i_indici(ii),j_indici(jj)) !if3
     *                             .eq.CE_BIN) then
	                          somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=2
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)=CE_BIN   !ce bin tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                 (max_cl_nsm,i1,i2,max_lg_nsm,
     *                                  max_lg_nsm,i,j,count,
     *                                  elem_id_cl,elem_id)
                                  end if 
c	                            if (somma.gt.2) warn(i,j)=2
c                                  exit loop2 	                       

  	                          end if !if4
	                      else if(cl_tab(i_indici(ii),j_indici(jj))
     *                             .eq.ASS_MF) then !if3
	                             somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=2
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)= ASS_MF   !ass mf tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                (max_cl_nsm,i1,i2,max_lg_nsm,
     *                                 max_lg_nsm,i,j,count,
     *                                 elem_id_cl,elem_id)
	                             end if
c	                            if (somma.gt.2) warn(i,j)=2
c                                  exit loop2 	                       
  	                          end if  !if4

	                      else if(cl_tab(i_indici(ii),j_indici(jj))
     *                             .eq.CE_SEMP) then !if3
	                             somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=2
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)= CE_SEMP !ce sempl tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                 (max_cl_nsm,i1,i2,max_lg_nsm,
     *                                  max_lg_nsm,i,j,count,
     *                                  elem_id_cl,elem_id)
	                             end if
c	                            if (somma.gt.2) warn(i,j)=2
c                                  exit loop2 	                       

  	                          end if  !if4
	                      else if(cl_tab(i_indici(ii),j_indici(jj))
     *                             .eq.VR_COL) then !if3
	                             somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=3
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)= VR_COL   !vl rid tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                 (max_cl_nsm,i1,i2,max_lg_nsm,
     *                                   max_lg_nsm,i,j,count,
     *                                  elem_id_cl,elem_id)
	                            end if
c	                            if (somma.gt.2) warn(i,j)=3
c                                  exit loop2 	                       

  	                          end if  !if4
	                      else if(cl_tab(i_indici(ii),j_indici(jj))
     *                             .eq.VD_COL) then !if3
	                             somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=3
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)= VD_COL   !imp rid tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                 (max_cl_nsm,i1,i2,
     *                                  max_lg_nsm,max_lg_nsm,i,j,count,
     *                                  elem_id_cl,elem_id)
	                             end if
c	                              if (somma.gt.2) warn(i,j)=3
c                                  exit loop2 	                       

  	                          end if  !if4
	                      else if(cl_tab(i_indici(ii),j_indici(jj))
     *                             .eq.VG_COL) then !if3
	                             somma = vg_tab_t(i,i_indici(ii))+
     *     						     vg_tab(j_indici(jj),j)
	                          if (somma.gt.0) then !if4
	                            if (somma.gt.2)then
c15/11						          conn(i,j)=.false.
								  warn(i,j)=2
	                            else
						          conn(i,j)=.true.
	                              count=count+1
                                    numero_link(i,j)=count  
                                    tipo_link(i,j,count)= VG_COL   !imp reg tra cl
                                    i1=i_indici(ii)
                                    i2=j_indici(jj)
                                    call copia_Guid_dam2_am3
     *                                 (max_cl_nsm,i1,i2,
     *                                  max_lg_nsm,max_lg_nsm,i,j,count,
     *                                  elem_id_cl,elem_id)
	                            end if
c	                            if (somma.gt.2) warn(i,j)=3
c                                  exit loop2	                       

  	                          end if  !if4
	                      end if !if3

				      end do loop2				
				   end do loop1

               end if  !if1
            end do
	   end if  !if0

	end do
   
	return
	end