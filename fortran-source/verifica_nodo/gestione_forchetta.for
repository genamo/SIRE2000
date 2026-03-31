	subroutine equivalenza_punti(num_pu_ns,num_lg_ns,id_pu,lg_id_pu,
     *                            ptrt_eq_ptgri)
c****************************************************************************************
c Matrice che definisce l'equivalenza tra punti rete e punti di grigliato
c****************************************************************************************
	implicit none

	include '../inc/nodo_ridotto.inc '

	integer*4 num_pu_ns,num_lg_ns
	integer*1 id_pu(*),lg_id_pu(*)
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm) !OUTPUT

	integer*4 iel,jel
      logical*2 confronta_guid
      external confronta_guid
c----------------------------------------------------------------------------------------

c     INIZIALIZZO LA MATRICE DI OUTPUT

      do iel=1,num_pu_ns
	   do jel=1,num_lg_ns
	      ptrt_eq_ptgri(iel,jel)=.false.
	   end do
	end do

      do iel=1,num_pu_ns
	   do jel=1,num_lg_ns
            if(.not.confronta_guid(jel,iel,lg_id_pu,id_pu)) go to 20
	      ptrt_eq_ptgri(iel,jel)=.true.
20          continue
	   end do
	end do

	return
	end
c****************************************************************************************
      integer*4 function punto_rete_equi(num_pu,pt_gri,ptrt_eq_ptgri)
c****************************************************************************************
c Dato un punto di grigliato restituisce il punto rete associato
c****************************************************************************************

	implicit none

	include '../inc/nodo_ridotto.inc '
     
	integer*4 num_pu,pt_gri
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)

	integer*4 iel

      punto_rete_equi=0
      do iel=1,num_pu
            if(ptrt_eq_ptgri(iel,pt_gri)) exit
	end do
	punto_rete_equi=iel

	return
	end
c***********************************************************************************************
      subroutine punti_gri_equi(ind_pu,num_pu_gri,ptrt_eq_ptgri,
     *                                        count,pu_gri_equi)
c****************************************************************************************
c Dato un punto di rete restituisce i punti griglia associati
c****************************************************************************************

	implicit none

	include '../inc/nodo_ridotto.inc '	! :costanti
     
	integer*4 ind_pu,num_pu_gri
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	integer*4 pu_gri_equi(*)

	integer*4 k,count

	count = 0
	do k=1,num_pu_gri
	   if (ptrt_eq_ptgri(ind_pu,k)) then
	       count= count + 1
	       pu_gri_equi(count) = k
	   end if
	end do

	return
	end

c***********************************************************************************************
      subroutine tabella_stati_forc(num_lg_ns,num_cl_ns,
     *             num_vr_lg_ns,num_vd_lg_ns,num_vg_lg_ns,
     *             id_lg,id_cl,id_vr_lg,id_vd_lg,id_vg_lg,
     *             lg_id_pu,vr_id_lg,vd_id_lg,vg_id_lg,
     *             dir_vr_lg,dir_vd_lg,dir_vg_lg,
     *             stato_free,vlg_stato_griglia,
     *             tipo_rid_gri,elem_id_gri,vlg_tab)
c**************************************************************************************
c Tabella stati che elimina il concetto di forchetta
c**************************************************************************************
	implicit none

	include '../inc/nodo_ridotto.inc '	

c---------------------------------------------------------->descrizione grigliato
	integer*4 num_pu_ns,           !num.punti rete
     *          num_lg_ns,           !num.linee di grigliato
     *          num_cl_ns,           !num.collettori
c---------------------------------------------------------->numero elementi su linee
     *          num_vr_lg_ns,        !num.valvole su linee di grigliato (vr)
     *          num_vd_lg_ns,        !num.impianti rid su linee grigliato(vd)
     *          num_vg_lg_ns         !num.impianti reg su linee grigliato (vg)
c---------------------------------------------------------->id. elementi
      integer*1 
     *          id_lg(*),           !id.linee grigliato
     *          id_cl(*),           !id.collettori
     *          id_vr_lg(*), 
     *          id_vd_lg(*), 
     *          id_vg_lg(*), 
c---------------------------------------------------------->associazioni
     *          lg_id_pu(*),        !id.pu cui è associata la lg
     *          vr_id_lg(*),     !id.lg cui è associata la vr
     *          vd_id_lg (*),    !id.lg cui è associata la vd
     *          vg_id_lg (*)    !id.lg cui è associata la vg
c---------------------------------------------------------->direzione riduzioni
      integer*4 dir_vr_lg(*),    !direzione connessione vr
	                                       !0->da lg a pu
	                                       !1->da pu a lg
     *          dir_vd_lg(*),    !direzione connessione vd
     *          dir_vg_lg(*),    !direzione connessione vg
c---------------------------------------------------------->valvole di grigliato (vlg)
     *          vlg_stato_griglia(max_cl_nsm,max_lg_nsm),      !stato vlg
c---------------------------------------------------------->stato free
     *          stato_free,                   !0-->OFF   1-->ON
c---------------------------------------------------------->OUTPUT:tabella stati
     *          vlg_tab(max_cl_nsm,max_lg_nsm),    
     *          tipo_rid_gri(max_cl_nsm,max_lg_nsm)
         
      integer*1 elem_id_gri(max_cl_nsm,max_lg_nsm*max_lunguid)    

c---------------------------------->locali
      integer*4 appo_stato(max_cl_nsm)
      integer*4 appo_dir(max_cl_nsm,max_lg_nsm)

	integer*4 i_forc,dir,i_riga,i_colo,
     *          iel,jel,kel,mel,ier,
     *          id_punto,id_linea,mel1,mel2,mel3,
     *          ind_vr,ind_vd,ind_vg ,id_coll,lun
	logical*2 si_rid,
     *          vr_tipo,vd_tipo,vg_tipo

	logical*2 confronta_Guid
	external  confronta_Guid
c***************************************************************************************************

c     Inizializzo la matrice assegnando stato indefinito a tutte le valvole 
c     di grigliato

      lun= num_lg_ns*lunguid 
	    
      do i_riga=1,num_cl_ns
	  appo_stato(i_riga) = STATO_IND
        do i_colo=1,num_lg_ns
            vlg_tab(i_riga,i_colo) = STATO_IND
            tipo_rid_gri(i_riga,i_colo) = 0
	  end do
        do i_colo=1,lun
            elem_id_gri(i_riga,i_colo)=0 
	  end do
	end do

c17---->individua il tipo di riduzione

      vr_tipo=.false.  !valvola di riduzione
      vd_tipo=.false.  !impianto di riduzione
      vg_tipo=.false.  !impianto di regolazione

c-------------------------------------------------------------------------------------------
c     Operazione AND tra valvole di riduzione linea/punto e valvole di grigliato:
c     Rid-On-->Rid Rid-Off-->Off On-On-->On On-Off-->Off  Off-On-->Off  
c
c-------------------------------------------------------------------------------------------
c     LINEE DI GRIGLIATO CONNESSE AI PUNTI RETE
C-------------------------------------------------------------------------------------------
c     jel-->linee di grigliato
c     mel1-->valvole di riduzione su linea/punto
c     mel2-->impianti di riduzione su linea/punto
c     mel3-->impianti di regolazione su linea/punto
c     kel-->collettori

         
  
      do jel=1,num_lg_ns    !2  jel->linee di grigliato

	      si_rid=.false.         !flag presenza riduzioni linea/punto

            do mel1=1,num_vr_lg_ns   !3   mel-> vr linea/punto
	         ind_vr=mel1
	         dir= dir_vr_lg(mel1)
               if(confronta_Guid(mel1,jel,vr_id_lg,id_lg))then
	            si_rid=.true.
	            vr_tipo=.true.  !valvola di riduzione
	            goto 10
	         end if             
            end do  !mel1

            do mel2=1,num_vd_lg_ns   !3   mel->vd linea/punto
	         ind_vd=mel2
	         dir= dir_vd_lg(mel2)
               if(confronta_Guid(mel2,jel,vd_id_lg,id_lg))then
	            si_rid=.true.
	            vd_tipo=.true.  !valvola di riduzione
	            goto 10
	          end if             
            end do  !mel2

            do mel3=1,num_vg_lg_ns   !3   mel->vg linea/punto
	         ind_vg=mel3
	         dir= dir_vg_lg(mel3)
               if(confronta_Guid(mel3,jel,vg_id_lg,id_lg))then
	            si_rid=.true.
	            vg_tipo=.true.  !valvola di riduzione
	            goto 10
	          end if             
            end do  !mel3


10          continue

            !gestione forchetta
               do kel=1,num_cl_ns  !3
	            appo_stato(kel)=vlg_stato_griglia(kel,jel)
	            if (appo_stato(kel).eq.VLG_STATO_LIBERO) then !la vlg ha stato free
	               if(stato_free.gt.0)then
	                 appo_stato(kel)=VLG_STATO_ON
				   else   
	                 appo_stato(kel)=VLG_STATO_OFF
				   end if
	            end if
	         end do !3

	      !definizione tabella degli stati

            if (si_rid) then    !presenza riduzioni

!------------------->matrice collettore punto che individua il tipo di riduzione sul grigliato
!                    (valvola/impianto)
               if (vr_tipo) then
                  do kel=1,num_cl_ns  
	               tipo_rid_gri(kel,jel)= VR_GRI
                     call copia_Guid_m(ind_vr,max_cl_nsm,kel,jel,
     *                                id_vr_lg,elem_id_gri)
	               vr_tipo=.false.
			    end do
	         else if (vd_tipo)then
                  do kel=1,num_cl_ns  
	               tipo_rid_gri(kel,jel)= VD_GRI
                     call copia_Guid_m(ind_vd,max_cl_nsm,kel,jel,
     *                                id_vd_lg,elem_id_gri)
	               vd_tipo=.false.
			    end do
	         else if (vg_tipo) then
                  do kel=1,num_cl_ns  
	               tipo_rid_gri(kel,jel)= VG_GRI
                     call copia_Guid_m(ind_vg,max_cl_nsm,kel,jel,
     *                                id_vg_lg,elem_id_gri)
	               vg_tipo=.false.
			    end do
	         end if


                  do kel=1,num_cl_ns  
	               appo_dir(kel,jel)=dir
	               if (appo_stato(kel).eq.VLG_STATO_ON) then
	                   vlg_tab(kel,jel)= VLG_STATO_RIDUCE    
				   else if (appo_stato(jel).eq.VLG_STATO_OFF) then
                         vlg_tab(kel,jel)= VLG_STATO_OFF   
	               end if
			    end do

	      else   !assenza riduttori
               do kel=1,num_cl_ns 
	            vlg_tab(kel,jel)= appo_stato(kel)    
			 end do
	      end if


	end do     !jel         
c------------------------------------------------->traduco la tabella degli stati
c  0-->OFF    1-->ON   2-->riduzione da collettore a punto  -2-->riduzione da punto a collettore
      do kel=1,num_cl_ns 
           do jel=1,num_lg_ns 
	       
	      if (vlg_tab(kel,jel).eq.VLG_STATO_OFF) then	                
		      vlg_tab(kel,jel)=STATO_OFF
	      elseif (vlg_tab(kel,jel).eq.VLG_STATO_ON) then	                
		      vlg_tab(kel,jel)=STATO_ON
	      elseif (vlg_tab(kel,jel).eq.VLG_STATO_RIDUCE) then
c		      if (dir.eq.1.or.dir.eq.3) then	                
		      if (appo_dir(kel,jel).eq.1.or.
     *                 appo_dir(kel,jel).eq.3) then	                
		          vlg_tab(kel,jel)=STATO_RID_pt_cl
	          else
		          vlg_tab(kel,jel)=STATO_RID_cl_pt
	          end if
	      end if
	     end do
	end do
	
      
      

      return
	end
c************************************************************************************
      subroutine controlla_connessioni_forc(num_pu_gri,num_pu_ns,id_pu,
     *                                 nsrid,conn,ptrt_eq_ptgri,link_ok)
c************************************************************************************
      implicit none

	include '../inc/nodo_ridotto.inc '	

      integer*4 num_pu_ns,num_pu_gri,
     *          nsrid(max_rt_nsm,max_rt_nsm)
      integer*1 id_pu(*)

	logical*2 link_ok(max_rt_nsm,max_rt_nsm),  !(O)
     *          conn(max_lg_nsm,max_lg_nsm)


	integer*4 i,ii,k,j,jj,ind_k,index,n_iter,count_i,count_ii,
     *          pu_eq_i(max_lg_nsm),pu_eq_ii(max_lg_nsm)	
	real*4 log2,log_arg
	logical*2 flag,ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
     *          ,test1,test2,test3,test4
	integer*4  punto_rete_equi
	external  punto_rete_equi

c-------------------------------------------------------------------------------------
      do i=1,num_pu_ns
         do ii=1,num_pu_ns
	      link_ok(i,ii)=.false.
         end do
	end do


c------------>verifico i collegamenti diretti assegnati da utente nella riduzione
c             (linl_ok viene definito a partire sa NSRID; viene settato a TRUE se la
c              connessione assegnata da utente è tra quelle possibili)

      do i=1,num_pu_ns
	   do ii=1,num_pu_ns
	      if (i.ne.ii) then
              call punti_gri_equi(i,num_pu_gri,ptrt_eq_ptgri,
     *                                	     count_i,pu_eq_i)
              call punti_gri_equi(ii,num_pu_gri,ptrt_eq_ptgri,
     *                                         count_ii,pu_eq_ii)
              loop1: do j=1,count_i
                loop2: do jj=1,count_ii
	          if (nsrid(i,ii).gt.0) then !PU COLLEGATI direttemente        
			     if (conn(pu_eq_i(j),pu_eq_ii(jj))) then
	              link_ok(i,ii)=.true.
			     else	      
	              link_ok(i,ii)=.false.
		         end if
			  end if
	         end do loop2
	        end do loop1
	      end if
	  end do
	end do



c---------->collegamenti indiretti definiti a partire da quelli assegnati a utente
c           numero di iterazioni che garantisce il controllo si tutti i link
      log_arg=num_pu_ns-1
	log2 = log(log_arg)/log(2.)
	n_iter =int(log2)+1

      do index=1,n_iter 
         do i=1,num_pu_ns
	      do ii=1,num_pu_ns
	         if (i.ne.ii) then
	            if(nsrid(i,ii).le.0)then !PU non COLLEGATI direttemente        
	               do k=1,num_pu_ns
c	                  flag=(link_ok(i,k).and.
c     *                             link_ok(k,ii))
c---- modificato il controllo delle connessioni indirette
                        test1=link_ok(i,k).and.link_ok(k,ii)
                        test2=link_ok(i,k).and.link_ok(ii,k).and.
     *                        (nsrid(ii,k).eq.VL_SEZ)
                        test3=link_ok(k,i).and.link_ok(k,ii).and.
     *                        (nsrid(k,i).eq.VL_SEZ)
                        test4=link_ok(k,i).and.link_ok(ii,k).and.
     *                        (nsrid(k,i).eq.VL_SEZ).and.
     *                        (nsrid(ii,k).eq.VL_SEZ)
	                  flag=test1.or.test2.or.test3.or.test4



	                  if (flag) link_ok(i,ii)=.true.
                     end do
			     end if
	          end if
	      end do
	   end do
	end do
c


      return
	end ! controlla_connessioni


c*********************************************************************************
      subroutine trova_indici_conn (pu1,pu2,num_pu_gri,ptrt_eq_ptgri,
     *                            nsrid,numero_link,tipo_link,
     *                            n_link,indice_1,indice_2,indice_3,
     *                            tipo,flag)

      implicit none
	include '../inc/nodo_ridotto.inc '


	integer*4 pu1,pu2,num_pu_gri,nsrid,
c29/11          nsrid(max_rt_nsm,max_rt_nsm),
     *          numero_link(max_lg_nsm,max_lg_nsm),
     *          tipo_link(max_lg_nsm,max_lg_nsm,max_link)
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	logical*2 flag
	integer*4 indice_1(max_link),indice_2(max_link),
     *          indice_3(max_link),tipo(max_link)
  
      integer*4 k,kk,count_i,count_ii,n_link,ind,j,
     *          pu_gri_equi_i(max_lg_nsm),pu_gri_equi_ii(max_lg_nsm)
	logical*2 i_forc,ii_forc

c	      cerca la forchetta
	call punti_gri_equi(pu1,num_pu_gri,ptrt_eq_ptgri,
     *                      count_i,pu_gri_equi_i)

	call punti_gri_equi(pu2,num_pu_gri,ptrt_eq_ptgri,
     *                      count_ii,pu_gri_equi_ii)
	

	n_link=0
	do ind=1,max_link
          indice_1(ind)=0
          indice_2(ind)=0
          indice_3(ind)=0
          tipo(ind)=0
	end do

	do k=1,count_i
	 do kk=1,count_ii
	    if(pu_gri_equi_i(k).ne.pu_gri_equi_ii(kk)) then
	     do j=1,numero_link(pu_gri_equi_i(k),pu_gri_equi_ii(kk))
			     if (nsrid.eq.
     *               tipo_link(pu_gri_equi_i(k),
     *                             pu_gri_equi_ii(kk),j)) then
	               flag=.true.
	               n_link=n_link+1
	               indice_1(n_link)=pu_gri_equi_i(k)
	               indice_2(n_link)=pu_gri_equi_ii(kk)
	               indice_3(n_link)=j
	               tipo(n_link)=tipo_link(pu_gri_equi_i(k),
     *                             pu_gri_equi_ii(kk),j)
	            end if 
	      end do
	    end if
	  end do
      end do


	return
	end
c*************************************************************************************
      subroutine trova_indici_imp (pu1,pu2,num_pu_gri,ptrt_eq_ptgri,
     *                            numero_link,tipo_link,
     *                            n_link,indice_1,indice_2,indice_3,
     *                            tipo,flag)

      implicit none
	include '../inc/nodo_ridotto.inc '


	integer*4 pu1,pu2,num_pu_gri,nsrid(max_rt_nsm,max_rt_nsm),
     *          numero_link(max_lg_nsm,max_lg_nsm),
     *          tipo_link(max_lg_nsm,max_lg_nsm,max_link)
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	logical*2 flag
	integer*4 indice_1(max_link),indice_2(max_link),
     *          indice_3(max_link),tipo(max_link)
  
      integer*4 k,kk,count_i,count_ii,n_link,ind,j,
     *          pu_gri_equi_i(max_lg_nsm),pu_gri_equi_ii(max_lg_nsm)
	logical*2 i_forc,ii_forc

c	      cerca la forchetta
	call punti_gri_equi(pu1,num_pu_gri,ptrt_eq_ptgri,
     *                      count_i,pu_gri_equi_i)

	call punti_gri_equi(pu2,num_pu_gri,ptrt_eq_ptgri,
     *                      count_ii,pu_gri_equi_ii)
	

	n_link=0
	do ind=1,max_link
          indice_1(ind)=0
          indice_2(ind)=0
          indice_3(ind)=0
          tipo(ind)=0
	end do

	do k=1,count_i
	 do kk=1,count_ii
	    if(pu_gri_equi_i(k).ne.pu_gri_equi_ii(kk)) then
	     do j=1,numero_link(pu_gri_equi_i(k),pu_gri_equi_ii(kk))
	               flag=.true.
	               n_link=n_link+1
	               indice_1(n_link)=pu_gri_equi_i(k)
	               indice_2(n_link)=pu_gri_equi_ii(kk)
	               indice_3(n_link)=j
	               tipo(n_link)=tipo_link(pu_gri_equi_i(k),
     *                             pu_gri_equi_ii(kk),j)
	      end do
	    end if
	  end do
      end do


	return
	end
c****************************************************************************
      subroutine trova_warning (pu1,pu2,num_pu_gri,ptrt_eq_ptgri,
     *                            warn,
     *                            n_warn,indice_1,indice_2,flag)

      implicit none
	include '../inc/nodo_ridotto.inc '


	integer*4 pu1,pu2,warn(max_lg_nsm,max_lg_nsm),num_pu_gri
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	logical*2 flag
	integer*4 indice_1(max_link),indice_2(max_link)
  
      integer*4 k,kk,count_i,count_ii,n_warn,ind,j,
     *          pu_gri_equi_i(max_lg_nsm),pu_gri_equi_ii(max_lg_nsm)
	logical*2 i_forc,ii_forc

c	      cerca la forchetta
	call punti_gri_equi(pu1,num_pu_gri,ptrt_eq_ptgri,
     *                      count_i,pu_gri_equi_i)

	call punti_gri_equi(pu2,num_pu_gri,ptrt_eq_ptgri,
     *                      count_ii,pu_gri_equi_ii)
	

	n_warn=0
	do ind=1,max_link
          indice_1(ind)=0
          indice_2(ind)=0
	end do

	do k=1,count_i
	 do kk=1,count_ii
	    if(pu_gri_equi_i(k).ne.pu_gri_equi_ii(kk)) then
	      if (warn(pu_gri_equi_i(k),pu_gri_equi_ii(kk)).gt.0) then
	          flag=.true.
	          n_warn=n_warn+1
	          indice_1(n_warn)=pu_gri_equi_i(k)
	          indice_2(n_warn)=pu_gri_equi_ii(kk)
	      end if
	    end if
	  end do
      end do


	return
	end
c************************************************************************************    
      logical*2 function forchetta (pu1,pu2,num_pu_gri,ptrt_eq_ptgri)

      implicit none
	include '../inc/nodo_ridotto.inc '


	integer*4 pu1,pu2,num_pu_gri,nsrid(max_rt_nsm,max_rt_nsm)
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
  
      integer*4 k,kk,count_i,count_ii,
     *          pu_gri_equi_i(max_lg_nsm),pu_gri_equi_ii(max_lg_nsm)
	logical*2 i_forc,ii_forc

c	      cerca la forchetta
      forchetta=.false.
	call punti_gri_equi(pu1,num_pu_gri,ptrt_eq_ptgri,
     *                      count_i,pu_gri_equi_i)

	call punti_gri_equi(pu2,num_pu_gri,ptrt_eq_ptgri,
     *                      count_ii,pu_gri_equi_ii)
	
	if (count_i.gt.1) i_forc=.true. 
	if (count_ii .gt.1) ii_forc=.true. 

	if (i_forc.or.ii_forc) forchetta=.true.

	return
	end
c************************************************************************************ 
      logical*2 function forchetta_conn (pu1,pu2,num_pu_gri,
     *                      ptrt_eq_ptgri,conn)

      implicit none
	include '../inc/nodo_ridotto.inc '


	integer*4 pu1,pu2,num_pu_gri,nsrid(max_rt_nsm,max_rt_nsm)

	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm),
     *          conn(max_lg_nsm,max_lg_nsm)  
      integer*4 k,kk,count_i,count_ii,
     *          pu_gri_equi_i(max_lg_nsm),pu_gri_equi_ii(max_lg_nsm)
	logical*2 i_forc,ii_forc

      forchetta_conn=.false.	 
c      cerca la forchetta
	call punti_gri_equi(pu1,num_pu_gri,ptrt_eq_ptgri,
     *                      count_i,pu_gri_equi_i)

	call punti_gri_equi(pu2,num_pu_gri,ptrt_eq_ptgri,
     *                      count_ii,pu_gri_equi_ii)
	
	do k=1,count_i
	 do kk=1,count_ii
    	   if(pu_gri_equi_i(k).ne.pu_gri_equi_ii(kk))then
	     if(conn(pu_gri_equi_i(k),pu_gri_equi_ii(kk)))then
	        forchetta_conn=.true.
			  exit           
		    end if
	   end if
	 end do
      end do

	return
	end
c**************************************************************************************
      subroutine trova_errori_forc(num_pu_gri,num_pu_ns,id_pu,nsrid,
     *                  flag_vc,
     *                  conn_pos,conn_imp,
     *                  numero_link_pos,numero_link_imp,
     *                  tipo_link_pos,tipo_link_imp,
     *                  link_ok,warn_pos,warn_imp,
     *                  elem_id,elem_id_pos,elem_id_imp,
     *                  ptrt_eq_ptgri,nsrid_warn,      
     *                  n_err,cod_err,cod_elem,type_elem,
     *                  pt_err_m,pt_err_v,
     *                  n_warn,cod_warn,pt_warn_m,pt_warn_v)

	implicit none

	include '../inc/nodo_ridotto.inc '	

      logical*2 confronta_GUID_mat_forc
      external  confronta_GUID_mat_forc


      integer*4 num_pu_ns,num_pu_gri
      integer*1 id_pu(*)
      integer*4 numero_link_pos(max_lg_nsm,max_lg_nsm),
     *        numero_link_imp(max_lg_nsm,max_lg_nsm),
     *        tipo_link_pos(max_lg_nsm,max_lg_nsm,max_link),    
     *        tipo_link_imp(max_lg_nsm,max_lg_nsm,max_link),
     *        nsrid(max_rt_nsm,max_rt_nsm),
     *        warn_pos(max_lg_nsm,max_lg_nsm),
     *        warn_imp(max_lg_nsm,max_lg_nsm)
      integer*1 elem_id(max_rt_nsm,max_rt_nsm*max_lunguid),   !elementi assegnati
     *        elem_id_pos(max_lg_nsm,max_lg_nsm,max_link*max_lunguid),      !
     *        elem_id_imp(max_lg_nsm,max_lg_nsm,max_link*max_lunguid)

	logical*2 link_ok(max_rt_nsm,max_rt_nsm),
     *          conn_pos(max_lg_nsm,max_lg_nsm),
     *          conn_imp(max_lg_nsm,max_lg_nsm)
      integer*4 flag_vc (max_rt_nsm,max_rt_nsm)
	integer*2 punto_rete_equi
	external  punto_rete_equi
	logical*2 ptrt_eq_ptgri(max_rt_nsm,max_lg_nsm)
	logical*2 nsrid_warn(max_rt_nsm,max_rt_nsm)


c------------->output
      integer*4 n_err                 !numero errori individuati nella riduzione
     *         ,cod_err(*)     !codice d'errore 
     *         ,type_elem(*)   !codice d'errore 
      integer*1 cod_elem(*)    !codice d'errore 
     *         ,pt_err_m(*)!punto monte nel collegamento errato                                     
     *         ,pt_err_v(*)!punto valle nel collegamento errato                                     
      integer*4 n_warn               !numero warning individuati nella riduzione
     *         ,cod_warn(*)   !codice di warning 
      integer*1 pt_warn_m(*)!punti nel collegamento errato                                     
     *         ,pt_warn_v(*)!punti nel collegamento errato                                     
	integer*4 i,ii,ind_i,ind_ii,indice(max_link),j,jj,n_link,
     *          n_link_imp,ind,tipo(max_link),tipo_imp(max_link),
     *          count_i,count_j,ind_j,
     *          pu_i,pu_j,w,
     *          indice_1(max_link),indice_2(max_link),
     *          indice_3(max_link),
     *          n_warning,iwarn_1(max_link),iwarn_2(max_link),
     *          ind_1(max_link),ind_2(max_link),ind_3(max_link)

	logical*2 flag,flag_imp,fl_plus,forchetta_conn,flag_warn
	external forchetta_conn

c-----------------------------------------------------------------------------------------------
      n_err=0
      n_warn=0
c------>Le variabili di output sono inizializzate nella subroutine verifica_nodo_ridotto
c------>Inizializzo le variabili locali
      do i=1,max_link
	   indice(i)=0
	   tipo(i)=0
	   tipo_imp(i)=0
	   indice_1(i)=0
	   indice_2(i)=0
	   indice_3(i)=0
	   ind_1(i)=0
	   ind_2(i)=0
	   ind_3(i)=0
	   iwarn_1(i)=0
	   iwarn_2(i)=0
	end do



      do pu_i=1,num_pu_ns
         do pu_j=1,num_pu_ns
	      flag=.false.
	      fl_plus=.false.

	      if (pu_i.ne.pu_j) then   !IF1
c-------------->GESTIONE WARNING
                  call trova_warning(pu_i,pu_j,num_pu_gri,ptrt_eq_ptgri,
     *                            warn_imp,
     *                            n_warning,iwarn_1,iwarn_2,flag_warn)
                   
			    if (n_warning.gt.0) then  
                   do w=1,n_warning 
          	       if (warn_imp(iwarn_1(w),iwarn_2(w)).eq.1) then   
	                  n_err=n_err+1
	                  cod_err(n_err)=30
	                  type_elem(n_err)=undefined
                        call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
	                  link_ok(pu_i,pu_j)=.true.
			     else if(warn_imp(iwarn_1(w),iwarn_2(w)).eq.2) then   
	                  n_err=n_err+1
	                  cod_err(n_err)=30
	                  type_elem(n_err)=undefined
                        call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
	                  link_ok(pu_i,pu_j)=.true.
			     else if (warn_imp(iwarn_1(w),iwarn_2(w)).eq.3) then   
	                  n_err=n_err+1
	                  cod_err(n_err)=30
	                  type_elem(n_err)=undefined
                        call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
	                  link_ok(pu_i,pu_j)=.true.
			        end if	
	             end do
                 end if
c----------->GESTIONE WARNING
10	                  if (nsrid(pu_i,pu_j).gt.0) then   !IF3 link ASSEGNATO
c-prova                    
                           if(nsrid_warn(pu_i,pu_j)) then
	                       n_err=n_err+1      !link non possibile
	                       cod_err(n_err)=20
	                       type_elem(n_err)=undefined
                             call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                             call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
				         end if
c-prova
			             if (.not.forchetta_conn(pu_i,pu_j,num_pu_gri,
     *                                    ptrt_eq_ptgri,conn_pos)) then !IF4
	                       n_err=n_err+1      !link non possibile
	                       cod_err(n_err)=1
	                       type_elem(n_err)=undefined
                             call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                             call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
                           else   !IF4 



	                       call trova_indici_conn(pu_i,pu_j,num_pu_gri
     *                            ,ptrt_eq_ptgri,nsrid(pu_i,pu_j)
c 29/11                           ,nsrid
     *                            ,numero_link_pos
     *                            ,tipo_link_pos,n_link,indice_1
     *                            ,indice_2,indice_3,tipo,flag)
							             !il link assegnato non è tra quelli possibili
	                        if (.not. flag) then  !if5
	                              if(nsrid(pu_i,pu_j).eq.VL_SEZ) then
	                                 n_err=n_err+1
	                                 cod_err(n_err)=2
c-bidir                           
                                       link_ok(pu_j,pu_i)=.true.
     	                                 call copia_Guid_dam2(pu_j,
     *                                          max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=connection_valve

                                       call copia_Guid
     *                                       (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							           (pu_j,n_err,id_pu,pt_err_v)
	                              else if(nsrid(pu_i,pu_j).eq.VR_GRI
     *								         .or.
     *                                 nsrid(pu_i,pu_j).eq.VR_COL) then
	                                 n_err=n_err+1 
	                                 cod_err(n_err)=3
     	                                 call copia_Guid_dam2
     *                                          (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=valve
                                       call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                               else if(nsrid(pu_i,pu_j)
     *                                                  .eq.CE_BIN) then
	                                 n_err=n_err+1
	                                 cod_err(n_err)=4
     	                                 call copia_Guid_dam2
     *                                     (pu_j,max_rt_nsm,pu_i,
     * 								      n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=bipolar_plant
                                       call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                                else if(nsrid(pu_i,pu_j)
     *                                                  .eq.ASS_MF) then
	                                  n_err=n_err+1
	                                  cod_err(n_err)=5
     	                                  call copia_Guid_dam2
     *                                         (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                  type_elem(n_err)=mf_plant_array
                                        call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                        call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                                else if(nsrid(pu_i,pu_j)
     *                                                 .eq.CE_SEMP) then
	                                 n_err=n_err+1
	                                 cod_err(n_err)=6
     	                                  call copia_Guid_dam2
     *                                         (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=simplified_plant
                                       call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                                else if(nsrid(pu_i,pu_j)
     *                                         .eq.VD_GRI.or.
     *                                 nsrid(pu_i,pu_j).eq.VD_COL) then
	                                 n_err=n_err+1
	                                 cod_err(n_err)=7
     	                                  call copia_Guid_dam2
     *                                         (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=reduction_plant
                                       call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                               else if(nsrid(pu_i,pu_j)
     *                                 .eq.VG_GRI.or.
     *                                 nsrid(pu_i,pu_j).eq.VG_COL)then
	                                 n_err=n_err+1
	                                 cod_err(n_err)=8
     	                                  call copia_Guid_dam2
     *                                         (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                 type_elem(n_err)=regulation_plant
                                       call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                       call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
                                     end if

	                 
	                 
                       else    !if4
                            do ind=1,n_link
	                        if (tipo(ind).gt.1) then
                                  if(confronta_GUID_mat_forc
     *                                (max_rt_nsm,max_lg_nsm,max_lg_nsm,
     *                                pu_i,pu_j,
     *                                indice_1(ind),indice_2(ind),
     *                                indice_3(ind),
     *                                elem_id,elem_id_pos))then

                                  go to 100
	                            end if
	                        else !non posso fare un confronto
                                  go to 100
	                        end if
	                      end do
	                      if(nsrid(pu_i,pu_j).eq.VL_SEZ) then
	                          n_err=n_err+1
	                          cod_err(n_err)=2  !9
c-bidir                           
                                link_ok(pu_j,pu_i)=.true.
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=connection_valve
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.VR_GRI.or.
     *                                nsrid(pu_i,pu_j).eq.VR_COL) then
	                          n_err=n_err+1 
	                          cod_err(n_err)=3 !10
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=valve
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.CE_BIN) then
	                          n_err=n_err+1
	                          cod_err(n_err)=4 !11
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=bipolar_plant
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.ASS_MF) then
	                          n_err=n_err+1
	                          cod_err(n_err)=5 !12
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=mf_plant_array
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.CE_SEMP) then
	                          n_err=n_err+1
	                          cod_err(n_err)=6 !13
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=simplified_plant
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.VD_GRI.or.
     *                               nsrid(pu_i,pu_j).eq.VD_COL) then
	                          n_err=n_err+1
	                          cod_err(n_err)=7 !14
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=reduction_plant
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
	                       else if(nsrid(pu_i,pu_j).eq.VG_GRI.or.
     *                               nsrid(pu_i,pu_j).eq.VG_COL) then
	                          n_err=n_err+1
	                          cod_err(n_err)=8 !15
     	                      call copia_Guid_dam2(pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                          type_elem(n_err)=regulation_plant
                                   call copia_Guid
     *                                   (pu_i,n_err,id_pu,pt_err_m)
                                   call copia_Guid
     *							       (pu_j,n_err,id_pu,pt_err_v)
                             end if

			       end if 
	          end if   !IF3
c 29/11 ************controllo per messaggio su inversione elemento

			  if (nsrid(pu_i,pu_j).eq.VR_GRI.or.
     *                             nsrid(pu_i,pu_j).eq.VR_COL) then

                   if (forchetta_conn(pu_j,pu_i,num_pu_gri,
     *                                ptrt_eq_ptgri,conn_pos)) then 
	                 call trova_indici_conn(pu_j,pu_i,num_pu_gri
     *                      ,ptrt_eq_ptgri,nsrid(pu_i,pu_j)
     *                      ,numero_link_pos
     *                      ,tipo_link_pos,n_link,indice_1
     *                      ,indice_2,indice_3,tipo,flag)
	                 if (flag) then
                            do ind=1,n_link
	                        if (tipo(ind).gt.1) then
                                  if(confronta_GUID_mat_forc
     *                                (max_rt_nsm,max_lg_nsm,max_lg_nsm,
     *                                pu_i,pu_j,
     *                                indice_1(ind),indice_2(ind),
     *                                indice_3(ind),
     *                                elem_id,elem_id_pos))then
		                            n_err=n_err+1
	                                cod_err(n_err)= 13 !possibile inversione
     	                                call copia_Guid_dam2
     *                                          (pu_j,max_rt_nsm,pu_i,
     * 								          n_err,elem_id,cod_elem)
	                                type_elem(n_err)=valve
                                      call copia_Guid
     *                                   (pu_j,n_err,id_pu,pt_err_m)
                                      call copia_Guid
     *							       (pu_i,n_err,id_pu,pt_err_v)
	                                go to 100
                                  end if
	                        end if
	                      end do
                        end if
                   end if

                end if

c29/11 *****************************************************************
100	          call trova_indici_imp (pu_i,pu_j,num_pu_gri, !nel caso degli assetti non vedeva il 
     *                            ptrt_eq_ptgri,             !doppio link imposto
     *                            numero_link_imp,tipo_link_imp,
     *                            n_link_imp,ind_1,ind_2,ind_3,
     *                            tipo_imp,flag_imp)
                j=n_link_imp	
	          if (j.gt.1) then
	              do i=2,n_link_imp
	                if (tipo_imp(i-1).ne.tipo_imp(i)) then
					   fl_plus=.true.
				    end if	                      
	              end do
                    if (fl_plus) then
	                 n_err=n_err+1
	                 cod_err(n_err)=20
	                 type_elem(n_err)=undefined
                       call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                       call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
	                 link_ok(pu_j,pu_i)=.true.
	              end if
	           end if


                 if (nsrid(pu_i,pu_j).eq.VL_sez) then
                   if (flag_vc(pu_i,pu_j).eq.cambia_stato) then
	                  nsrid(pu_i,pu_j)=0
	                  link_ok(pu_i,pu_j)=.false.
	                  go to 10
	             end if
	           end if


		    else    !IF2        !COLLEGAMENTO NON ASSEGNATO
C
c     LA GESTIONE DEI WARNING E'STATA PORTATA SOPRA,PERCHE' IN QUESTA POSIZIONE I WARNING VENIVANO
C     INDIVIDUATI SOLO NEL CASO DI COLLEGAMENTO NON ASSEGNATO TRA I DUE PUNTI
c
C                  call trova_warning(pu_i,pu_j,num_pu_gri,ptrt_eq_ptgri,
C     *                            warn_imp,
C     *                            n_warning,iwarn_1,iwarn_2,flag_warn)
C
C                   
C			    if (n_warning.gt.0) then  !warning imp
C                   do w=1,n_warning 
C          	       if (warn_imp(iwarn_1(w),iwarn_2(w)).eq.1) then   !doppia rid.warningC
c	                  n_err=n_err+1
c	                  cod_err(n_err)=30
c	                  type_elem(n_err)=undefined
c                       call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
c                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c	                  link_ok(pu_i,pu_j)=.true.
c			     else if(warn_imp(iwarn_1(w),iwarn_2(w)).eq.2) then   
c	                  n_err=n_err+1
c	                  cod_err(n_err)=30
c	                  type_elem(n_err)=undefined
c                        call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
c                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c	                  link_ok(pu_i,pu_j)=.true.
c			     else if (warn_imp(iwarn_1(w),iwarn_2(w)).eq.3) then   
c	                  n_err=n_err+1
c	                  cod_err(n_err)=30
c	                  type_elem(n_err)=undefined
c                        call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
c                        call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c	                  link_ok(pu_i,pu_j)=.true.
c			        end if	
c	             end do
c                 end if
c

			   if (forchetta_conn(pu_i,pu_j,num_pu_gri,
     *                               ptrt_eq_ptgri,conn_imp)) then    !collegamento indiretto
                  if (.not.link_ok(pu_i,pu_j) )then
                      call trova_indici_imp (pu_i,pu_j,num_pu_gri,
     *                            ptrt_eq_ptgri,
     *                            numero_link_imp,tipo_link_imp,
     *                            n_link_imp,ind_1,ind_2,ind_3,
     *                            tipo_imp,flag_imp)
c	                j=numero_link_imp(ind_i,ind_j)   !il link imposto è unico
c	                if (j.gt.1) then
                      j=n_link_imp	
200	                if (j.gt.1) then
	                   do i=2,n_link_imp
	                      if (tipo_imp(i-1).ne.tipo_imp(i)) then
						      fl_plus=.true.
						  end if	                      
	                   end do
                         if (fl_plus) then
	                     n_err=n_err+1
	                     cod_err(n_err)=20
	                     type_elem(n_err)=undefined
                           call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                           call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
	                     link_ok(pu_j,pu_i)=.true.
                         else
	                     j=1
	                     go to 200
                         end if
					else 
c	                  if (tipo_link_imp(ind_i,ind_j,1).eq.VL_SEZ
	                  if (tipo_link_imp(ind_1(j),ind_2(j),ind_3(j))
     *                        .eq.VL_SEZ
c     *					    .and.nsrid(pu_j,pu_i).ne.VL_SEZ) then	                  
     *                        .and..not.link_ok(pu_j,pu_i)) then
c	                    conn_imp(ind_2(j),ind_1(j))=.false. !x evitare di duplicare il msg
c-bidir                           
                          link_ok(pu_j,pu_i)=.true.
	                    n_err=n_err+1
	                    cod_err(n_err)=21
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=connection_valve
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1).eq.VR_GRI
                        else if(tipo_link_imp(ind_1(j),ind_2(j),
     *                                        ind_3(j)).eq.VR_GRI.or.
     *  					  tipo_link_imp(ind_1(j),ind_2(j),ind_3(j))
     *                                    .eq.VR_COL) then
	                    n_err=n_err+1
	                    cod_err(n_err)=22
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=valve
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1)
                        else if (tipo_link_imp(ind_1(j),ind_2(j),
     *                                    ind_3(j)).eq.CE_BIN) then
	                    n_err=n_err+1
	                    cod_err(n_err)=23
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=bipolar_plant
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1)
                        else if (tipo_link_imp(ind_1(j),ind_2(j),
     *                           ind_3(j)).eq.ASS_MF) then
	                    n_err=n_err+1
	                    cod_err(n_err)=24
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=mf_plant_array
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1)
                        else if (tipo_link_imp(ind_1(j),ind_2(j),
     *                                ind_3(j)).eq.CE_SEMP) then
	                    n_err=n_err+1
	                    cod_err(n_err)=25
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=simplified_plant
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1)
                        else if (tipo_link_imp(ind_1(j),ind_2(j),
     *                               ind_3(j)).eq.VD_GRI.or.
c     *  					 tipo_link_imp(ind_i,ind_j,1).eq.VD_COL) then
     *  					 tipo_link_imp(ind_1(j),ind_2(j),ind_3(j))
     *                                  .eq.VD_COL) then
	                    n_err=n_err+1
	                    cod_err(n_err)=26
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=reduction_plant
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
c                        else if (tipo_link_imp(ind_i,ind_j,1)
                        else if (tipo_link_imp(ind_1(j),ind_2(j),
     *                                  ind_3(j)).eq.VG_GRI.or.
c     *  					  tipo_link_imp(ind_i,ind_j,1).eq.VG_COL) then
     *  					  tipo_link_imp(ind_1(j),ind_2(j),ind_3(j))
     *                       .eq.VG_COL) then
	                    n_err=n_err+1
	                    cod_err(n_err)=27
     	                    call copia_Guid_dam3(
     *                          ind_3(j),max_lg_nsm,max_lg_nsm,
     * 						  ind_1(j),ind_2(j),n_err,
     *                          elem_id_imp,cod_elem)
	                    type_elem(n_err)=regulation_plant
                          call copia_Guid(pu_i,n_err,id_pu,pt_err_m)
                          call copia_Guid(pu_j,n_err,id_pu,pt_err_v)
					  end if
	                end if
				end if
c-warn
c-warn
                 end if   

			 end if   !IF2
			 end if	
c	      end if   !IF1
	   end do
	end do


      return
	end   !trova_errori_forc


