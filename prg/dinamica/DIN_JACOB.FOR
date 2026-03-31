**** DIN_JACOB.FOR ******** Jacobiano analitico sparso *******************
!=======================================================================
	SUBROUTINE DIN_iniJAC(		! individua i punti non nulli
					! della matrice Jacobiana 
					! del DINAMICO 
crsa2sy
	1			jac_first,	    ! Indice di riga
	1			jac_entry,	    ! Indice di colonna
	1			neq_bil_diff,	! Numero equazioni Bilancio differenziali
	1			neq_bil_alg,	! Numero equazioni Bilancio algebriche (punto non capacitivo)
	1			neq_ferg,	    ! Numero equazioni Fergusson
	1			neq_qimp,	    ! Numero equazioni Q imposte
	1			neq_tcon,	    ! Numero equazioni stato ON imposto
	1			neq_pimp,	    ! Numero equazioni P imposte
	1			*)	! Error Label
*&*

        implicit none
	include '../inc/PARAM.inc' 	!:costanti
	include '../inc/TI.inc' 	!:IN 
	include '../inc/TJ.inc' 	!:IN 
	include '../inc/TH.inc' 	!:IN 
	INCLUDE '../inc/top_app_ele.inc'		!:IN
	include '../inc/TJT.inc' 	!:IN 
	include '../inc/RIC.inc' 	!:IN 
*&*
crsa2sy        
        INTEGER*4 jac_first(1),jac_entry(1)
        INTEGER*4 neq_bil_diff,neq_bil_alg,neq_ferg
     -           ,neq_qimp,neq_tcon,neq_pimp

        integer*4 ii,jj,je,oo,im,iv,it

!----------------------------------------------------------------------
!----------------------------------------------------------------------
        
        if (o_sta_ric.ge.O_ST_NVTOP) then

 	    jac_first(1)    =  1 
	    ii=0			! indice di jac_first:contatore delle righe
	    jj=0			! indice di jac_entry
!----------------------------------------------------------------------
!	per ogni punto capacitivo
!----------------------------------------------------------------------
          neq_bil_diff = ii
	    DO je=otipti(e_pu),etipti(e_pu)
           if (.not. fl_nocap(pgrato(je))) then
		  ii = ii + 1
!					------ elemento sulla diagonale
		  IF(jj .GE. rc_m_jac) GOTO 8888
		  jj = jj + 1
c_topvar_12/08/05
!            jac_entry(jj) = ii
            jac_entry(jj) = rc_topvar(je)
!					------------------------------
!					per ogni elemento entrante
!					------------------------------
		  oo = oseeto(je)
		  DO WHILE (oo .NE. je)
			IF(jj.GE.rc_m_jac) GOTO 8888
			jj = jj + 1
              jac_entry(jj) = rc_topvar(oo)
			oo = oseeto(oo)
		  ENDDO
!					------------------------------
!					per ogni elemento uscente
!					------------------------------
		  oo = oseuto(je)
		  DO WHILE (oo .NE. je)
			IF(jj.GE.rc_m_jac) GOTO 8888
			jj = jj + 1
                        jac_entry(jj) = rc_topvar(oo)
			oo = oseuto(oo)
		  ENDDO
!--------------------------------- 
		  jac_first(ii+1) = jj + 1
	     endif
	    ENDDO
          neq_bil_diff = ii - neq_bil_diff

!----------------------------------------------------------------------
!	per ogni punto non capacitivo
!----------------------------------------------------------------------
          neq_bil_alg = ii
	    DO je=otipti(e_pu),etipti(e_pu)
           if (fl_nocap(pgrato(je))) then
		  ii = ii + 1
c_diag_nocap_12/08/05
!					------ elemento sulla diagonale
c_d		  IF(jj .GE. rc_m_jac) GOTO 8888
c_d		  jj = jj + 1
c_topvar_12/08/05
!            jac_entry(jj) = ii
c_d            jac_entry(jj) = rc_topvar(je)
!					------------------------------
!					per ogni elemento entrante
!					------------------------------
		  oo = oseeto(je)
		  DO WHILE (oo .NE. je)
			IF(jj.GE.rc_m_jac) GOTO 8888
			jj = jj + 1
              jac_entry(jj) = rc_topvar(oo)
			oo = oseeto(oo)
		  ENDDO
!					------------------------------
!					per ogni elemento uscente
!					------------------------------
		  oo = oseuto(je)
		  DO WHILE (oo .NE. je)
			IF(jj.GE.rc_m_jac) GOTO 8888
			jj = jj + 1
              jac_entry(jj) = rc_topvar(oo)
			oo = oseuto(oo)
		  ENDDO
!--------------------------------- 
		  jac_first(ii+1) = jj + 1
	     endif
	    ENDDO
          neq_bil_alg = ii - neq_bil_alg

!----------------------------------------------------------------------
!	per ogni tronco
!----------------------------------------------------------------------
          neq_ferg = ii
	    DO je=otipti(e_tr),etipti(e_tr)	
		  im = opumto(je)
		  iv = opuvto(je)
		  ii = ii + 1
!					------ coeff. portata (sulla diagonale)
		  IF(jj+3 .GT. rc_m_jac) GOTO 8888
		  jj = jj + 1
            jac_entry(jj) = rc_topvar(je)
!					------ coeff. pressione di monte
		  jj = jj + 1
            jac_entry(jj) = rc_topvar(im)

!					------ coeff. pressione di valle
	      jj = jj + 1
            jac_entry(jj) = rc_topvar(iv)

		  jac_first(ii+1) = jj + 1
	    ENDDO
          neq_ferg = ii - neq_ferg
crsa2sy
          neq_qimp = ii
!----------------------------------------------------------------------
!	per ogni elemento speciale (vengono imposti tutti)
!----------------------------------------------------------------------
     	      DO it=1,ne_spe
  	        DO je=otipti(te_spe(it)),etipti(te_spe(it))	
                ii = ii + 1
!					------ coeff. portata (sulla diagonale)
		      IF(jj .GE. rc_m_jac) GOTO 8888
                jj = jj + 1
                jac_entry(jj) = rc_topvar(je)

		      jac_first(ii+1) = jj + 1
              ENDDO
            ENDDO
          neq_qimp = ii - neq_qimp
        else
          ii = neq_bil_diff + neq_bil_alg + neq_ferg + neq_qimp
          jj = jac_first(ii+1)-1
        endif
crsatt-ini
        neq_pimp = ii
        if (f_pimpo .eq. 1 ) then
!----------------------------------------------------------------------
!	per ogni punto vengono imposti solo quelli settati
!----------------------------------------------------------------------
          DO je=otipti(e_pu),etipti(e_pu)	
crsa_prv           if (pu_pimp(pgrato(je)).ne.0) then
             ii = ii + 1
             IF(jj .GE. rc_m_jac) GOTO 8888
             jj = jj + 1
             jac_entry(jj) = rc_topvar(je)
             jac_first(ii+1) = jj + 1
crsa_prv           endif
          ENDDO
        endif
        neq_pimp = ii - neq_pimp

!----------------------------------------------------------------------
        neq_tcon = ii
        if (f_tcon .eq. 1 ) then
         do it=1,ne_vlv
          do je = otipti(te_vlv(it)),etipti(te_vlv(it))
!chsta           if (htcitj(je).eq.tc_on.or.htcitj(je).eq.tc_nr) then
           if (hstatj(je).eq.sta_on.or.hstatj(je).eq.sta_fr) then
             ii = ii + 1
             IF(jj .GE. rc_m_jac) GOTO 8888
cace     viene imposto l'ordine crescente(invece del monte-valle) per evitare che 
c        la routine di ordinamento possa modificare tale ordinamento inducendo
c        un cambiamento nel calcolo della funzione
c
             if (rc_topvar(opumto(je)).lt.rc_topvar(opuvto(je))) then
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opumto(je))
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opuvto(je))
               jac_first(ii+1) = jj + 1
             else
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opuvto(je))
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opumto(je))
               jac_first(ii+1) = jj + 1
             endif
           endif
          ENDDO
         ENDDO
         do it=1,ne_cen
          do je = otipti(te_cen(it)),etipti(te_cen(it))
!chsta           if (htcitj(je).eq.tc_on.or.htcitj(je).eq.tc_nr) then
           if (hstatj(je).eq.sta_on.or.hstatj(je).eq.sta_fr) then
             ii = ii + 1
             IF(jj .GE. rc_m_jac) GOTO 8888
             if (rc_topvar(opumto(je)).lt.rc_topvar(opuvto(je))) then
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opumto(je))
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opuvto(je))
               jac_first(ii+1) = jj + 1
             else
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opuvto(je))
               jj = jj + 1
               jac_entry(jj) = rc_topvar(opumto(je))
               jac_first(ii+1) = jj + 1
             endif
           endif
          ENDDO
         ENDDO
        endif
        neq_tcon = ii - neq_tcon
crsatt-fin

	RETURN			! O.K.
!----------------------------------------------------------------------
8888	CONTINUE
        call gest_error(1,0,'DIN_INIJAC',
     *      'dimensione di jac_entry insuff.',0)
	RETURN	1	
	END	!DIN_INIJAC

!----------------------------------------------------------------------
!----------------------------------------------------------------------

      SUBROUTINE  DIN_EVAJAC(		! stima lo Jacobiano per via Analitica
     *                     rom,         ! Vettore di stato
     *                     jac_first,   ! Indice di riga
     *                     jac_entry,   ! Indice di colonna
     *                     jac_value)   ! Valore
*&*------------------------------------------------------------

        implicit none
	include '../inc/PARAM.inc' 	!:costanti
	include '../inc/TI.inc' 	!:IN identificaz. e connessioni
	INCLUDE '../inc/top_app_ele.inc'		!:IN
	include '../inc/TD.inc' 	!:IN 
	include '../inc/TX.inc' 	!:IN 
	include '../inc/TY.inc' 	!:IN 
	include '../inc/TJ.inc' 	!:IN 
	include '../inc/RIC.inc' 	!:IN 

	  real*4 rom   (*)
        integer*4 jac_first(*), jac_entry(*)
        real*4    jac_value(*)
        integer*4 ii,jj,ie,je,im,iv
        real*4 xx
        integer*4 itop_from_ivar  ! function
*&*
	ii = 0				! indice di riga ( equazione)
!----------------------------------------------------------------------
!	per ogni punto capacitivo: calcolo elementi non nulli 
!----------------------------------------------------------------------
	DO je=otipti(e_pu),etipti(e_pu)	
        if (.not.fl_nocap(pgrato(je))) then
		ii = ii + 1
cace
        if (f_densita.eq.1) then
		xx  = 1.0/volntx(je)
        else
		xx  = ro0td(je)/volntx(je)
        endif

		DO jj =jac_first(ii),jac_first(ii+1)-1
c_topvar_12/08/05
!            if (jac_entry(jj).eq.ii) then
            if (jac_entry(jj).eq.rc_topvar(je)) then
!					------ elemento sulla diagonale
              jac_value(jj) = 0.0
            else
!					------------------------------
!					per ogni elemento entrante/uscente
!					------------------------------
              ie = itop_from_ivar(jac_entry(jj),rc_topvar)
              if (opumto(ie).eq.je) then
 		      jac_value(jj) = -xx          ! uscente
              else
 		      jac_value(jj) =  xx          ! entrante
              endif
            endif
		ENDDO !jj
	  endif
	ENDDO

!----------------------------------------------------------------------
!	per ogni punto non capacitivo: calcolo elementi non nulli 
!----------------------------------------------------------------------
	DO je=otipti(e_pu),etipti(e_pu)	
        if (fl_nocap(pgrato(je))) then
		ii = ii + 1
cace
        if (f_densita.eq.1) then
		xx  = 1.0/volntx(je)
        else
		xx  = ro0td(je)/volntx(je)
        endif
		DO jj =jac_first(ii),jac_first(ii+1)-1
c_diag_nocap_12/08/05
c_topvar_12/08/05
!            if (jac_entry(jj).eq.ii) then
c_d            if (jac_entry(jj).eq.rc_topvar(je)) then
!					------ elemento sulla diagonale
cd              jac_value(jj) = 0.0
c_d            else
!					------------------------------
!					per ogni elemento entrante/uscente
!					------------------------------
              ie = itop_from_ivar(jac_entry(jj),rc_topvar)
              if (opumto(ie).eq.je) then
 		      jac_value(jj) = -xx          ! uscente
              else
 		      jac_value(jj) =  xx          ! entrante
              endif
c_d            endif
		ENDDO !jj
	  endif
	ENDDO

!----------------------------------------------------------------------
!	per ogni tronco: calcolo elementi non nulli (Fergusson)
!       Si ricorda che il vettore jac_first non e' in ordine crescente
!----------------------------------------------------------------------
	DO ie=otipti(e_tr),etipti(e_tr)	
		im = opumto(ie)
		iv = opuvto(ie)
		ii = ii + 1
		DO jj =jac_first(ii),jac_first(ii+1)-1
            if (jac_entry(jj).eq.ii) then
!					------ coeff. portata
             if (f_densita.eq.1) then               ! c_den_attilia
  		    jac_value(jj) = -2*bferty(ie)*ABS(rom(ii))
	1		                / (ro0td(ie)*ro0td(ie))
             else
  		    jac_value(jj) = -2*bferty(ie)*ABS(rom(ii))
             endif

            else if (jac_entry(jj).eq.rc_topvar(im)) then
!					------ coeff. pressione di monte
 		    jac_value(jj) =  2*aferty(ie)*prestj(im)*dproty(im)

            else if (jac_entry(jj).eq.rc_topvar(iv)) then
!					------ coeff. pressione di valle
  		    jac_value(jj) = -2*prestj(iv)*dproty(iv)	

            endif
          ENDDO


	ENDDO

      RETURN
	END	!DIN_EVAJAC


**** DIN_JACOB.FOR ******** Jacobiano analitico sparso *******************
