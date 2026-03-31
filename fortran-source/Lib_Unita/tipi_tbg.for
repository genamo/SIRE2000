	real*4 function eps (id_grd)
c-------------------------------------------------------------------------------
c   Per ogni grandezza, restituisce in base al grado di precisione usato per la 
c   memorizzazione nel db, il delta da utilizzare nei confronti di uguaglianza
c-------------------------------------------------------------------------------
	implicit none
	include '../inc/grd_soglie.inc' 

      integer*4 id_grd
c-------------------------------------------------------------------------------
      
	if (id_grd.eq.id_qm) then		
c     1-->PORTATA DI COMPRESSIONE [m3/h]  : grd_prec_db=0
c         nei calcoli è usata in 1000*m3/h (conversione eseguita nella validazione)
c	    quindi considero grd_prec=3
		eps = 1.E-4
	else if (id_grd.eq.id_qprog) then
c     2-->PORTATA DI PROGETTO : grd_prec=1
		eps = 1.E-2
	else if (id_grd.eq.id_deltap.or.
     *         id_grd.eq.id_ploss) then
c     8-->DELTA PRESSIONE
c     9-->CADUTA PRESSIONE   :grd_prec=1
		eps = 1.E-2
	else if (id_grd.eq.id_numgiri) then
c     10-->NUMERO DI GIRI RPM  : grd_prec=0
		eps = 1.E-1
	else if (id_grd.eq.id_taria.or.
     *         id_grd.eq.id_tmax.or.
     *         id_grd.eq.id_deltat) then
c     15-->TEMPERATURA ARIA
c     16-->TEMPERATURA MASSIMA
c     19-->DELTA TEMPERATURA   :grd_prec=2
		eps = 1.E-3
	else if (id_grd.eq.id_pw) then
c     20-->POTENZA : grd_prec=1
		eps = 1.E-2
	else if (id_grd.eq.id_hrate) then
c     25-->HEAT RATE : grd_prec=1
		eps = 1.E-2
	else if (id_grd.eq.id_deltaperc.or.
     *         id_grd.eq.id_perc.or.
     *         id_grd.eq.id_100perc) then
c     28-->DELTA %
c     29-->PERCENTUALE
c     30-->PERCENTUALE A 100   :grd_prec=4
		eps = 1.E-5
	else if (id_grd.eq.id_coeff) then
c     31-->COEFFICIENTE : grd_prec=4
		eps = 1.E-5
	else if (id_grd.eq.id_head) then		
c     35-->PREVALENZA AD. [m]  : grd_prec_db=0
c         nei calcoli è usata in [km] (conversione eseguita nella validazione)
c	    quindi considero grd_prec=3
		eps = 1.E-4
	else if (id_grd.eq.id_calore) then
c     45-->CALORE SCAMBIATO NELL'UNITA' DI TEMPO : grd_prec=4
		eps = 1.E-5
	else if (id_grd.eq.id_ripcarico) then
c     82-->RIPARTIZIONE CARICO : grd_prec=1
		eps = 1.E-2
      end if

	return
	end

C********************************************************************************************

      subroutine trova_tipi(ivert,unit_num,type_num,type_quant,ordine)
  
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'

	integer*4 ivert         !INPUT
     *         ,unit_num      !INPUT
     *         ,type_num      !OUTPUT 
     *         ,type_quant(*) !OUTPUT
     *         ,ordine(*)     !OUTPUT: ordine(posizione)=indice tbg

	logical*2 stesso_tipo
	external stesso_tipo 
     
     
      integer*4 tipo(max_type),ind_tipo(max_type),type_num_old
	logical*2 fl_tipo(max_type)
	integer*4 i,j,k,ii
	

c-------------------------------------------------------------------------------------------
c----->inizializzazioni
c      (Si suppone che i tbg siano tutti di diverso tipo,quindi il numero di tipi coincide
c       col numero di tbg,l'indice del tipo è l'indice stesso del tbg e fl_tipo è TRUE per tutti
c       perchè ogni tbg è rappresentativo di un tipo)

      type_num_old=unit_num
	do i=1,unit_num
	   tipo(i)=i
	   fl_tipo(i)=.true.
	end do

	do	i=1,unit_num
		if	(fl_tipo(i)) then
			ii=i+1
			do	j=ii,unit_num
				if	(fl_tipo(j).and.stesso_tipo(ivert,i,j)) then
					tipo(j)=i
					fl_tipo(j)=.false.
				end if
			end do
		end if
	end do
c     
c     In seguito alla ricerca avranno fl_tipo TRUE solo i tbg rappresentativi di un tipo 
c     e i tbg di uguale tipo avranno tutti l'indice di tale rappresentante

      k=0
	do	i=1,unit_num
		if	(fl_tipo(i)) then
			k=k+1
			ind_tipo(k)=i !indice del tipo k
		end if
	end do

      type_num=k

c	conto i tbg di ogni tipo
      j=0
	do	k=1,type_num_old
		type_quant(k)=0
	end do
c	riordino i tbg in base al tipo
      do	k=1,type_num
		do	i=1,unit_num
			if	(tipo(i).eq.ind_tipo(k)) then
				type_quant(k)=type_quant(k)+1
				j=j+1
				ordine(j)=i
			end if
		end do
	end do

	return
	end

c*******************************************************************************************
      logical*2 function stesso_tipo(ivert,ind1,ind2)

      implicit none

      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
	include '../inc/grd_soglie.inc' 

	integer*4 ivert,ind1,ind2,fase,i1,i2
	logical*2 stesso_compressore,stessa_turbina,stesso_aircooler
	external stesso_compressore,stessa_turbina,stesso_aircooler
	real*4 eps
	external eps


      stesso_tipo=.false.
c-elena
      i1= first_unit(ivert)+ind1-1
      i2= first_unit(ivert)+ind2-1

c	tipo compressore (monofase/bifase)
	if (unit_bifase(i1).ne.unit_bifase(i2)) return
	fase=1
c	confronto compressori prima fase
	if (.not.stesso_compressore(i1,i2,fase)) return
c	delta% efficienza ad.
	if (abs(unit_eff_corr(i1)-unit_eff_corr(i2))
     *                                .gt.eps(id_deltaperc)) return
c	confronto turbine
	if (.not.stessa_turbina(i1,i2)) return
c	delta% potenza
	if (abs(unit_power_corr(i1)-unit_power_corr(i2))
     *                                .gt.eps(id_deltaperc)) return
c	delta% consumo specifico
	if (abs(unit_hrate_corr(i1)-unit_hrate_corr(i2))
     *                                .gt.eps(id_deltaperc)) return
	if	(unit_bifase(i1)) then
		fase=2
c	confronto compressori seconda fase
		if (.not.stesso_compressore(i1,i2,fase)) return
c	delta% efficienza ad.
		if (abs(unit2_eff_corr(i1)-unit2_eff_corr(i2))
     *                                .gt.eps(id_deltaperc)) return
c	deltap intermedio
		if (abs(unit_delprint(i1)-unit_delprint(i2))
     *                                .gt.eps(id_deltap)) return
c	massima temperatura intermedia
		if (abs(unit_maxtint(i1)-unit_maxtint(i2))
     *                                .gt.eps(id_tmax)) return
c	deltat intermedio
		if (abs(unit_deltrint(i1)-unit_deltrint(i2))
     *                                .gt.eps(id_deltat)) return
		if(stat_ord(ivert).eq.f_serie) then
c	confronto aircooler intermedio nei tbg in serie
			if (.not.stesso_aircooler(i1,i2)) return
		end if
      end if
!  CRITERIO DI RIPARTIZIONE
      if (tipo_criterio(ivert).eq.crit_equi) then
c-commento per prova nuova lettura 28/9
c		if (abs(unit_min_head(ind1)-unit_min_head(ind2))
c     *                                .gt.eps(id_qm)) return
c		if (abs(unit_max_head(ind1)-unit_max_head(ind2))
c     *                                .gt.eps(id_qm)) return
c		if (abs(unit_min_flow(ind1)-unit_min_flow(ind2))
c     *                                .gt.eps(id_head)) return
c		if (abs(unit_max_flow(ind1)-unit_max_flow(ind2))
c     *                                .gt.eps(id_head)) return
c-ele 26/7/06 Nuova implementazione del criterio di equidistanza:
c             i parametri passati da utente sono le percentuali di
c             scostamento delle curve limite dalla spezzata di surge 
c             e dalla curva operating limit.
		if (abs(unit_perc_min_equi(i1)-unit_perc_min_equi(i2))
     *                                .gt.eps(id_100perc)) return
		if (abs(unit_perc_max_equi(i1)-unit_perc_max_equi(i2))
     *                                .gt.eps(id_100perc)) return
c-ele
	else if (tipo_criterio(ivert).eq.crit_port) then
		if (abs(unit_flow_rat(i1)-unit_flow_rat(i2))
     *                                .gt.eps(id_ripcarico)) return
	else if (tipo_criterio(ivert).eq.crit_giri) then
		if (abs(unit_rev_rat(i1)-unit_rev_rat(i2))
     *                                .gt.eps(id_ripcarico)) return
      end if

      stesso_tipo=.true.

	return
	end
c******************************************************************************************
      logical*2 function stesso_compressore(ind1,ind2,fase)

      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/types.inc'
      include '../inc/mf_types.inc'

	include '../inc/grd_soglie.inc' 

	integer*4 ind1,ind2,fase,i1,i2
	integer*4 i,nlim
	real*4 eps
	external eps

	stesso_compressore=.false.

	if	(fase.eq.1) then
c	numero di giri nominale
		if(abs(nom_rev(ind1)-nom_rev(ind2)).gt.eps(id_numgiri)) return
c	portata nominale
		if(abs(nom_flow(ind1)-nom_flow(ind2)).gt.eps(id_qm)) return
c	prevalenza ad. nominale
		if(abs(nom_head(ind1)-nom_head(ind2)).gt.eps(id_head)) return
c	efficienza minima
		if(abs(min_eff(ind1)-min_eff(ind2)).gt.eps(id_100perc)) return
c	numero di giri minimo
		if(abs(min_rev(ind1)-min_rev(ind2)).gt.eps(id_100perc)) return
c	numero di giri massimo
		if(abs(max_rev(ind1)-max_rev(ind2)).gt.eps(id_perc)) return
c	coefficienti curve limite (clim)
		do	i=1,4
			if(abs(type_c_coef(i,ind1)-type_c_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a giri costanti-zona normale
		do	i=1,6
			if(abs(type_chn(i,ind1)-type_chn(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a giri costanti-zona di choking
		do	i=1,6
			if(abs(type_chc(i,ind1)-type_chc(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a efficienza costante-zona normale
		do	i=1,5
			if(abs(type_cen(i,ind1)-type_cen(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a efficienza costante-zona di choking
		do	i=1,6
			if(abs(type_cec(i,ind1)-type_cec(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	numero di punti limite
		if(type_nlim(ind1).ne.type_nlim(ind2)) return
	    nlim=type_nlim(ind1)
c	coefficienti alim
		do	i=1,nlim
			if(abs(type_a_coef(i,ind1)-type_a_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	coefficienti blim
		do	i=1,nlim
			if(abs(type_b_coef(i,ind1)-type_b_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	numero di giri punti limite
		do	i=1,nlim
			if(abs(type_lim_n(i,ind1)-type_lim_n(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	portata punti limite
		do	i=1,nlim
			if(abs(type_lim_q(i,ind1)-type_lim_q(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do

	else if	(fase.eq.2) then
c	numero di giri nominale
		if(abs(nom2_rev(ind1)-nom2_rev(ind2))
     *						.gt.eps(id_numgiri)) return
c	portata nominale
		if(abs(nom2_flow(ind1)-nom2_flow(ind2)).gt.eps(id_qm)) return
c	prevalenza ad. nominale
		if(abs(nom2_head(ind1)-nom2_head(ind2))
     *							.gt.eps(id_head)) return
c	efficienza minima
		if(abs(min2_eff(ind1)-min2_eff(ind2))
     *							.gt.eps(id_100perc)) return
c	numero di giri minimo
		if(abs(min2_rev(ind1)-min2_rev(ind2))
     *							.gt.eps(id_100perc)) return
c	numero di giri massimo
		if(abs(max2_rev(ind1)-max2_rev(ind2)).gt.eps(id_perc)) return
c	coefficienti curve limite (clim)
		do	i=1,4
			if(abs(type2_c_coef(i,ind1)-type2_c_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a giri costanti-zona normale
		do	i=1,6
			if(abs(type2_chn(i,ind1)-type2_chn(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a giri costanti-zona di choking
		do	i=1,6
			if(abs(type2_chc(i,ind1)-type2_chc(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a efficienza costante-zona normale
		do	i=1,5
			if(abs(type2_cen(i,ind1)-type2_cen(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	coefficienti curve a efficienza costante-zona di choking
		do	i=1,6
			if(abs(type2_cec(i,ind1)-type2_cec(i,ind2))
     *								 .gt.eps(id_coeff)) return
		end do
c	numero di punti limite
		if(type2_nlim(ind1).ne.type2_nlim(ind2)) return
	    nlim=type2_nlim(ind1)
c	coefficienti alim
		do	i=1,nlim
			if(abs(type2_a_coef(i,ind1)-type2_a_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	coefficienti blim
		do	i=1,nlim
			if(abs(type2_b_coef(i,ind1)-type2_b_coef(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	numero di giri punti limite
		do	i=1,nlim
			if(abs(type2_lim_n(i,ind1)-type2_lim_n(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do
c	portata punti limite
		do	i=1,nlim
			if(abs(type2_lim_q(i,ind1)-type2_lim_q(i,ind2))
     *									   .gt.eps(id_coeff)) return
		end do

	end if

	stesso_compressore=.true.

	return
	end
c******************************************************************************************
      logical*2 function stessa_turbina(ind1,ind2)

      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/types.inc'
	include '../inc/grd_soglie.inc' 

	integer*4 ind1,ind2
	integer*4 i
	real*4 eps
	external eps

	stessa_turbina=.false.
c	numero di giri nominale
	if(abs(nom_rev(ind1)-nom_rev(ind2)).gt.eps(id_numgiri)) return
c	potenza nominale
	if(abs(nom_power(ind1)-nom_power(ind2)).gt.eps(id_pw)) return
c	heat rate nominale
	if(abs(nom_hrate(ind1)-nom_hrate(ind2)).gt.eps(id_hrate)) return
c	temperatura dell'aria
	if(abs(nom_tair(ind1)-nom_tair(ind2)).gt.eps(id_taria)) return
c	coefficienti curva massima potenza
	do	i=1,3
		if(abs(type_cpwm(i,ind1)-type_cpwm(i,ind2))
     *                               .gt.eps(id_coeff)) return
	end do
c	coefficienti correzione curva di massima potenza
	do	i=1,4
		if(abs(type_cpwt(i,ind1)-type_cpwt(i,ind2))
     *                               .gt.eps(id_coeff)) return
	end do
c	coefficienti curve ad heat rate costante
	do	i=1,6
		if(abs(type_chr(i,ind1)-type_chr(i,ind2))
     *                             .gt.eps(id_coeff)) return
	end do
c	coefficienti di correzione curve ad heat rate costante
	do	i=1,4
		if(abs(type_chrt(i,ind1)-type_chrt(i,ind2))
     *                               .gt.eps(id_coeff)) return
	end do

	stessa_turbina=.true.

	return
	end
c******************************************************************************************
      logical*2 function stesso_aircooler(ind1,ind2)

      implicit none

      include '../inc/param.inc'
	include '../inc/air_cooler.inc' 
	include '../inc/grd_soglie.inc' 

	integer*4 ind1,ind2
	real*4 eps
	external eps

	stesso_aircooler=.false.

c	portata di progetto
	if(abs(c_ac1_flow_prog(ind1)-c_ac1_flow_prog(ind2))
     *                                 .gt.eps(id_qprog)) return
c	caduta di pressione
	if(abs(c_ac1_deltap(ind1)-c_ac1_deltap(ind2))
     *                           .gt.eps(id_ploss)) return
c	calore scambiato nell'unità di tempo
	if(abs(c_ac1_q_prog(ind1)-c_ac1_q_prog(ind2))
     *                          .gt.eps(id_calore)) return
c	temperatura di riferimento
	if(abs(c_ac1_taria_prog(ind1)-c_ac1_taria_prog(ind2))
     *                                   .gt.eps(id_taria)) return

	stesso_aircooler=.true.

	return
	end
c******************************************************************************************
      Subroutine copia_unita(iunit_da,iunit_a)
c------------------------------------------------------------------------------------------
c     Per i vettori di tbg copia il valore di indice IUNIT_DA nella posizione IUNIT_A
c------------------------------------------------------------------------------------------
	implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
	include '../inc/types.inc' 
	include '../inc/mf_types.inc' 
	include '../inc/units.inc' 
	include '../inc/mf_units.inc' 
	include '../inc/air_cooler.inc' 
	include '../inc/filtri.inc' 

      integer*4 iunit_a,     !I)
     *          iunit_da      !I)

	integer*4 i
c************************************************************************************++
c-------->common TYPES
c      type_quant(iunit_a)=type_quant(iunit_da) riassegnato dalla subroutine trova_tipi
c      type_avail(iunit_a)=type_avail(iunit_da) definito a partire da status e unit_avail
c      type_act(iunit_a)=type_act(iunit_da) definito a partire da status e unit_avail
!  la dimensione dei tre vettori precedenti è dim=max_type;essendo max_type=max_unit e
!  first_type=first_unit vanno bene gli indici passati in input
      type_nlim(iunit_a)=type_nlim(iunit_da) 
      max_rev(iunit_a)=max_rev(iunit_da) 
      min_rev(iunit_a)=min_rev(iunit_da) 
      min_eff(iunit_a)=min_eff(iunit_da) 
      nom_rev(iunit_a)=nom_rev(iunit_da) 
      t_nom_rev(iunit_a)=t_nom_rev(iunit_da) 
	do i=1,max_lim
	   type_lim_n(i,iunit_a) = type_lim_n(i,iunit_da)
	   type_lim_q(i,iunit_a) = type_lim_q(i,iunit_da)
	   type_lim_h(i,iunit_a) = type_lim_h(i,iunit_da)
	   type_a_coef(i,iunit_a) = type_a_coef(i,iunit_da)
	   type_b_coef(i,iunit_a) = type_b_coef(i,iunit_da)
	end do
	type_a_coef(max_lim+1,iunit_a) = type_a_coef(max_lim+1,iunit_da)
	type_b_coef(max_lim+1,iunit_a) = type_b_coef(max_lim+1,iunit_da)
	do i=1,4
	   type_c_coef(i,iunit_a) = type_c_coef(i,iunit_da)
      end do	
	do i=1,5
	   type_cen(i,iunit_a) = type_cen(i,iunit_da)
      end do	
	do i=1,6
	   type_chn(i,iunit_a) = type_chn(i,iunit_da)
      end do	
	do i=1,6
	   type_chc(i,iunit_a) = type_chc(i,iunit_da)
      end do	
	do i=1,6
	   type_cec(i,iunit_a) = type_cec(i,iunit_da)
      end do	
	do i=1,3
	   type_cpwm(i,iunit_a) = type_cpwm(i,iunit_da)
      end do	
	do i=1,4
	   type_cpwt(i,iunit_a) = type_cpwt(i,iunit_da)
      end do	
	do i=1,6
	   type_chr(i,iunit_a) = type_chr(i,iunit_da)
      end do	
	do i=1,4
	   type_chrt(i,iunit_a) = type_chrt(i,iunit_da)
      end do	
      nom_flow(iunit_a)=nom_flow(iunit_da) 
      nom_head(iunit_a)=nom_head(iunit_da) 
      nom_tair(iunit_a)=nom_tair(iunit_da)
cgpe-corr
      nom_power(iunit_a)=nom_power(iunit_da)
      nom_hrate(iunit_a)=nom_hrate(iunit_da)
cgpe-corr-end
!  la dimensione dei tre vettori precedenti è dim=max_type;essendo max_type=max_unit e
!  first_type=first_unit vanno bene gli indici passati in input
      c_name(iunit_a)=c_name(iunit_da) 
      c_comm(iunit_a)=c_comm(iunit_da) 
      t_name(iunit_a)=t_name(iunit_da) 
      t_comm(iunit_a)=t_comm(iunit_da)

c-------->common MF_TYPES

      type2_nlim(iunit_a)=type2_nlim(iunit_da) 
      max2_rev(iunit_a)=max2_rev(iunit_da) 
      min2_rev(iunit_a)=min2_rev(iunit_da) 
      min2_eff(iunit_a)=min2_eff(iunit_da) 
      nom2_rev(iunit_a)=nom2_rev(iunit_da) 
	do i=1,max_lim
	   type2_lim_n(i,iunit_a) = type2_lim_n(i,iunit_da)
	   type2_lim_q(i,iunit_a) = type2_lim_q(i,iunit_da)
	   type2_lim_h(i,iunit_a) = type2_lim_h(i,iunit_da)
	   type2_a_coef(i,iunit_a) = type2_a_coef(i,iunit_da)
	   type2_b_coef(i,iunit_a) = type2_b_coef(i,iunit_da)
	end do
	type2_a_coef(max_lim+1,iunit_a) = type2_a_coef(max_lim+1,iunit_da)
	type2_b_coef(max_lim+1,iunit_a) = type2_b_coef(max_lim+1,iunit_da)
	do i=1,4
	   type2_c_coef(i,iunit_a) = type2_c_coef(i,iunit_da)
      end do	
	do i=1,5
cgpe-corr	   type2_cen(i,iunit_a) = type_cen(i,iunit_da)
	   type2_cen(i,iunit_a) = type2_cen(i,iunit_da)
      end do	
	do i=1,6
	   type2_chn(i,iunit_a) = type2_chn(i,iunit_da)
      end do	
	do i=1,6
	   type2_chc(i,iunit_a) = type2_chc(i,iunit_da)
      end do	
	do i=1,6
	   type2_cec(i,iunit_a) = type2_cec(i,iunit_da)
      end do	
      nom2_flow(iunit_a)=nom2_flow(iunit_da) 
      nom2_head(iunit_a)=nom2_head(iunit_da) 
!  la dimensione dei due vettori precedenti è dim=max_type;essendo max_type=max_unit e
!  first_type=first_unit vanno bene gli indici passati in input
      c2_name(iunit_a)=c2_name(iunit_da) 
      c2_comm(iunit_a)=c2_comm(iunit_da) 

c-------->common UNITS

      unit_ind(iunit_a)=unit_ind(iunit_da)
      unit_avail(iunit_a)=unit_avail(iunit_da)
      unit_prior(iunit_a)=unit_prior(iunit_da)
      unit_eff_corr(iunit_a)=unit_eff_corr(iunit_da)
      unit_power_corr(iunit_a)=unit_power_corr(iunit_da)
      unit_rev_rat(iunit_a)=unit_rev_rat(iunit_da)
      unit_flow_rat(iunit_a)=unit_flow_rat(iunit_da)
      unit_min_head(iunit_a)=unit_min_head(iunit_da)
      unit_max_head(iunit_a)=unit_max_head(iunit_da)
      unit_min_flow(iunit_a)=unit_min_flow(iunit_da)
      unit_max_flow(iunit_a)=unit_max_flow(iunit_da)
      unit_crit_min(iunit_a)=unit_crit_min(iunit_da)
      unit_crit_max(iunit_a)=unit_crit_max(iunit_da)
      unit_flow_prog(iunit_a)=unit_flow_prog(iunit_da)
      unit_start_cost(iunit_a)=unit_start_cost(iunit_da)
      unit_shut_cost(iunit_a)=unit_shut_cost(iunit_da)
      unit_hrate_corr(iunit_a)=unit_hrate_corr(iunit_da)
      fl_flow_man(iunit_a)=fl_flow_man(iunit_da)
      unit_delta_pasp(iunit_a)=unit_delta_pasp(iunit_da)
      unit_delta_pman(iunit_a)=unit_delta_pman(iunit_da)
      tg_comp_id(iunit_a)=tg_comp_id(iunit_da)

c-------->output  (i dati di output non vanno riordinati;a livello di validazione questi
c                  vettori non sono ancora valorizzati)
c      unit_perc(iunit_a)=unit_perc(iunit_da)
c      unit_rev(iunit_a)=unit_rev(iunit_da)
c      unit_flow(iunit_a)=unit_flow(iunit_da)
c      unit_head(iunit_a)=unit_head(iunit_da)
c      unit_eff(iunit_a)=unit_eff(iunit_da)
c      unit_power(iunit_a)=unit_power(iunit_da)
c      unit_hrate(iunit_a)=unit_hrate(iunit_da)
c      unit_cons(iunit_a)=unit_cons(iunit_da)
c      unit_temp(iunit_a)=unit_temp(iunit_da)
c      unit_max(iunit_a)=unit_max(iunit_da)
	
c-------->common UNITC

      status(iunit_a)=status(iunit_da)
c      lstatus(iunit_a)=lstatus(iunit_da)
cgpe-tmp
      unit_attiv_tlc(iunit_a) = unit_attiv_tlc(iunit_da)
cgpe-tmp-end

c-------->common VINCOLI

      unit_vinc_maxpow(iunit_a)=unit_vinc_maxpow(iunit_da)
      unit_vinc_minpow(iunit_a)=unit_vinc_minpow(iunit_da)
      unit_vinc_maxrev(iunit_a)=unit_vinc_maxrev(iunit_da)

c-------->common APP_UNITS

      unit_delprint(iunit_a)=unit_delprint(iunit_da)
      unit_deltrint(iunit_a)=unit_deltrint(iunit_da)
      unit_maxtint(iunit_a)=unit_maxtint(iunit_da)
      unit_bifase(iunit_a)=unit_bifase(iunit_da)
      unit_comm(iunit_a)=unit_comm(iunit_da)
c      unit_vinc(iunit_a)=unit_vinc(iunit_da)
c      unit_min(iunit_a)=unit_min(iunit_da)
c      unit_vcrit(iunit_a)=unit_vcrit(iunit_da)
c      unit_delpr_ac(iunit_a)=unit_delpr_ac(iunit_da)
c      unit_pres_int(iunit_a)=unit_pres_int(iunit_da)
c      unit_temp_int(iunit_a)=unit_temp_int(iunit_da)

c-------->common MF_UNITS

      unit2_eff_corr(iunit_a)=unit2_eff_corr(iunit_da)
      unit2_power_corr(iunit_a)=unit2_power_corr(iunit_da)
      unit2_rev_rat(iunit_a)=unit2_rev_rat(iunit_da)
c      unit2_perc(iunit_a)=unit2_perc(iunit_da)
c      unit2_rev(iunit_a)=unit2_rev(iunit_da)
c      unit2_flow(iunit_a)=unit2_flow(iunit_da)
c      unit2_head(iunit_a)=unit2_head(iunit_da)
c      unit2_eff(iunit_a)=unit2_eff(iunit_da)
c      unit2_temp(iunit_a)=unit2_temp(iunit_da)
c      unit2_power(iunit_a)=unit2_power(iunit_da)
c      unit2_min(iunit_a)=unit2_min(iunit_da)
c      unit2_max(iunit_a)=unit2_max(iunit_da)

c-------->common FILTRI

      c_f_tg_stat(iunit_a)=c_f_tg_stat(iunit_da)

c-------->common FILTRI_APPO

      c_f_tg_flow_prog(iunit_a)=c_f_tg_flow_prog(iunit_da)
      c_f_tg_deltap(iunit_a)=c_f_tg_deltap(iunit_da)

c-------->common FILTRI_DERIV

      c_f_tg_k(iunit_a)=c_f_tg_k(iunit_da)

c-------->common AIR_COOLER1

      c_ac1_stat(iunit_a)=c_ac1_stat(iunit_da)
      c_ac1_q_prog(iunit_a)=c_ac1_q_prog(iunit_da)
      c_ac1_taria_prog(iunit_a)=c_ac1_taria_prog(iunit_da)

c-------->common AIR_COOLER2

      c_ac2_stat(iunit_a)=c_ac2_stat(iunit_da)
      c_ac2_q_prog(iunit_a)=c_ac2_q_prog(iunit_da)
      c_ac2_taria_prog(iunit_a)=c_ac2_taria_prog(iunit_da)

c-------->common AIR_COOLER_APPO

      c_ac1_flow_prog(iunit_a)=c_ac1_flow_prog(iunit_da)
      c_ac1_deltap(iunit_a)=c_ac1_deltap(iunit_da)
      c_ac2_flow_prog(iunit_a)=c_ac2_flow_prog(iunit_da)
      c_ac2_deltap(iunit_a)=c_ac2_deltap(iunit_da)

c-------->common AC_DERIV

      c_ac1_k(iunit_a)=c_ac1_k(iunit_da)
      c_ac2_k(iunit_a)=c_ac2_k(iunit_da)

	return
	end !copia_unita

c----------------------------------------------------------------------------------------
      subroutine riordina_unita(iel)
c*****************************************************************************************
c    Per la centrale ISTAZ, calcola le variabili TYPE_NUM e TYPE_QUANT e riordina le aree
c    di memoria relative ai tbg in base al tipo di appartenenza (dati relativi a tbg dello
c    stesso tipo occupano posizioni contigue nei vettori) 
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
	include '../inc/ti.inc' 
	include '../inc/types.inc' 
	include '../inc/cap_bon_prv.inc' 

	integer*4 iel, !indice della centrale nella struttura dati completa
c                    !iel assume i valori tra otipti(e_ce) ed etipti(e_ce)
     *          ii,j,
     *          ivert,jvert,
     *          ordine(maxunits),
     *          iu_max,iu,it,ju,jt,
     *          unum

	logical*2 ce_serie
	external ce_serie

c------>puntatore all'area relativa alla centrale di indice MAX_VERT
c       tale area viene usata come appoggio nella fase di riordino dei dati
	iu_max = (max_vert-1)*MAXUNITS+1   !=first_unit(max_vert)

	ivert=pgrato(iel)
	iu=first_unit(ivert)
	it=first_type(ivert)

c------>la chiamata a tale subroutine aggiorna i valori type_num,type_quant e restituisce
c       il vettore ORDINE che permette di riordinare i dati di tbg in base al tipo
c       di appartenenza:nelle aree di memoria,informazioni relative a tbg dello stesso tipo
c       devono occupare posizioni contigue.
c       NOTA BENE: ORDINE(posizione)=indice di tbg
	call trova_tipi(ivert,unit_num(ivert),
     *                type_num(ivert),type_quant(it),ordine)

c------>copio i dati di turbogruppo nell'area di appoggio prevista tenendo conto del nuovo 
c       ordinamento dettato dal vettore ORDINE

	do ii=1,unit_num(ivert)
         call copia_unita(iu+ordine(ii)-1,iu_max+ii-1)
	end do

c------>copio i dati riordinati nell'area di memoria originale
	do ii=1,unit_num(ivert)
         call copia_unita(iu_max+ii-1,iu+ii-1)
	end do

c------>ripeto le operazioni di riordino e copiatura per il secondo assetto del macroassetto 
c       serie
	if (ce_serie(iel)) then
	   jvert=pgrato_app(iel)
	   ju=first_unit(jvert)
	   jt=first_type(jvert)
	   type_num(jvert)=type_num(ivert)
c	   do j=1,type_num(ivert)
	   do j=1,unit_num(ivert)
	      type_quant(jt+j-1)=type_quant(it+j-1)	       
	   end do
	   do ii=1,unit_num(jvert)
            call copia_unita(ju+ordine(ii)-1,iu_max+ii-1)
	   end do	    
	   do ii=1,unit_num(jvert)
            call copia_unita(iu_max+ii-1,ju+ii-1)
	   end do	    
	end if

      return
	end !riordina_unita



