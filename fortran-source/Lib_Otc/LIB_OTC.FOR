! Lib_Otc.for
***********************************************************************************************
      subroutine simula_cent_all(istaz,jstaz)
***********************************************************************************************
c     Per ogni centrale la centrale con indice istaz e se jstaz <>I_UND:
c     - vengono settate le variabili locali necessarie alla simulazione, in particolare viene
c       settato a FALSE un flag per indicare che la centrale è gia stata simulata (importante
c       per centrali provenienti dalla stessa MF)
c     -viene chiamata la simula_cent_serie() per il trattamento delle centrali in serie e la
c      simula_cent() per il trattamento di tutte le altre centrali
***********************************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/err.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/cap_bon.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/simpstat.inc'

      integer*4 istaz   ! I) indice della stazione nella struttura dati completa
	                  !    otipti(e_ce)<istaz<etipti(e_ce) (e_ce=centrale binaria;
	                  !    otipti(),etipti() sono puntatori alle zone inizio e fine tipo
	                  !    di elemento)
     *          ,jstaz  ! I) indice della seconda stazione

      integer*4  ivert   !puntatore alla prima fase di una centrale in serie
     *          ,jvert   !puntatore alla seconda fase di una centrale in serie
     *          ,pg      !puntatore alle grandezze di una centrale (istaz)
     *          ,jpg     !puntatore alle grandezze di una centrale (jstaz)
     *          ,ier     !index error
      integer*4 jstaz_app
c
      logical*2  ce_serie          !flag cm_serie  
     *          ,ce_off
      real*4 pasp_save(max_serv),pmand_save(max_serv),
     *       flow_save(max_serv)
      real*4 cons_tot              !consumo totale
     *      ,pint
c**********************************************************************************************
c
c     INIZIALIZZAZIONI:vengono settate le variabili locali necessarie alla simulazione
c  
c      msg_err = ' '
c
c     SIMULAZIONE:vengono chiamate la simula_cent o la simula_cent_serie
c
      pg = pgrato(istaz)           
      if (ce_serie(istaz)) then
!trattamento CENTRALE IN SERIE
        if (.not. fl_simu(pgrato(istaz)) .and.
     *      .not. fl_simu(pgrato_app(istaz))) then
          call find_ce_serie (istaz,jstaz,ivert,jvert)
          call save_cin_s (ivert)
          call calc_int (istaz,ivert,jstaz,jvert,ier)
          if (ier .eq. 0) then
c-bpcorr
            iopt_last = .true.
c-bpcorr-end
!simulazione della centrale in serie
            call simula_cent_serie (istaz,jstaz,ivert,jvert,
     *           cons_tot,pint,ier)
          endif
cgpe-new
          if (ier .ne. 0) then
            fl_ris_ce(ivert) = -1
            fl_ris_ce(jvert) = -1
          else
            fl_ris_ce(ivert) = 0
            fl_ris_ce(jvert) = 0
          endif
cgpe-new-end
        endif
      else
!trattamento CENTRALE BINARIA
cgpe        if (.not.ce_off(istaz)) then                        !telemisure
        if (set_flag(pg).eq.1) then                        !telemisure
cgpe-end
!ISTAZ ha almeno una unità accesa
          if (.not. fl_simu(pg)) then
!ISTAZ ha almeno una unità accesa e non è stata ancora trattata
            if (jstaz .ne. I_UND) then
!ISTAZ ha almeno una unità accesa,non è stata ancora trattata ed è associata a JSTAZ
ce (jstaz è da considerarsi come modello di riferimento per la simulazione)
              if (.not.fl_simu(pgrato(jstaz)) ) then
!ISTAZ ha almeno una unità accesa,non è stata ancora trattata ed è associata a JSTAZ
!che non è ancora stata trattata
cgpe                if (.not.ce_off(jstaz)) then
                if (set_flag(pgrato(jstaz)).eq.1) then            !telemisure
cgpe-end
!ISTAZ E JSTAZ hanno almeno una unità accesa e non sono state ancora trattate
ce Inizializzazioni delle variabili locali e salvataggio CIN
                  call init_loc_var (istaz,jstaz)
                  call save_cin (istaz,jstaz,pasp_save,
     *                 pmand_save,flow_save)
                  jpg = I_UND
                  if (jstaz .ne. I_UND) jpg = pgrato(jstaz)
ce Verifica se sono presenti le condizioni topologiche e le condizioni iniziali necessarie 
ce per simulare
cgpe                  call check_vert (pg,jpg)
                  call simula_cent ()
                  call restore_cin (istaz,jstaz,pasp_save,
     *                 pmand_save,flow_save)
                else
!ISTAZ ha almeno una unità accesa,non è stata trattata ed è associata a JSTAZ,
!ma JSTAZ non ha unità accese
                  call init_ce_off(jstaz)
                  jstaz_app = I_UND
                  call init_loc_var (istaz,jstaz_app)
                  call save_cin (istaz,jstaz,pasp_save,
     *                 pmand_save,flow_save)
                  jpg = I_UND
                  if (jstaz .ne. I_UND) jpg = pgrato(jstaz)
cgpe                  call check_vert (pg,jpg)
                  call simula_cent ()
!restore delle condizioni iniziali date in input
                  call restore_cin (istaz,jstaz,pasp_save,
     *                 pmand_save,flow_save)
                endif
              else
!ISTAZ ha almeno una unità accesa,non è stata trattata ed è associata a JSTAZ
!ma JSTAZ (con almeno una unità accesa) è stata gia trattata
                jstaz_app = I_UND
                call init_loc_var (istaz,jstaz_app)
                call save_cin (istaz,jstaz,pasp_save,
     *                 pmand_save,flow_save)
                jpg = I_UND
                if (jstaz .ne. I_UND) jpg = pgrato(jstaz)
cgpe                call check_vert (pg,jpg)
                call simula_cent ()
                call restore_cin (istaz,jstaz,pasp_save,
     *               pmand_save,flow_save)
              endif
            else
!ISTAZ ha almeno una unità accesa ed è binaria (JSTAZ = I_UND)
              call init_loc_var (istaz,jstaz)
              call save_cin (istaz,jstaz,pasp_save,pmand_save,
     *             flow_save)
              jpg = I_UND
              if (jstaz .ne. I_UND) jpg = pgrato(jstaz)
cgpe              call check_vert (pg,jpg)
              call simula_cent ()
              call restore_cin(istaz,jstaz,pasp_save,pmand_save,
     *             flow_save)
            endif
          endif
        else
!ISTAZ HA TUTTE LE UNITA' SPENTE
          if (.not.fl_simu(pgrato(istaz))) then
            jstaz_app = I_UND
            call init_loc_var (istaz,jstaz_app)
            call init_ce_off (istaz)
          endif
        endif
      endif  !-fine centrale non in serie
c
      return 
      end  ! simula_cent_all
c-----------------------------------------------------------------------------------------------
      Subroutine init_loc_var (istaz,jstaz)
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'
      include '../inc/ty.inc'
      include '../inc/ai.inc'
      include '../inc/rk_param.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/simpstat.inc'
      include '../inc/FLAG.INC'
	include '../inc/ASS_COND.INC'

      INTEGER*4 istaz       ! I)
      INTEGER*4 jstaz       ! I) off_set assetto coniugato a istaz

      INTEGER*4 pg,iserv
      integer*4 j,i1
!------------------------------------------------------------
!-inizializzazioni
      lmf = .false.
      iserv = 1
      pg = pgrato(istaz)
      fl_simu(pg) = .true.

cmar_ass_cond
	flag_ass_cond_s=flag_ass_cond_p(pg)

c	flag_dati=flag_dati_s(pg) 
cmar_ass_cond

!-viene settato iopt
      iopt = .false.
      if (fl_opt) then
        iopt = .true.
      endif

      i_staz(iserv) = istaz
      i_vert(iserv) = pg
      i_serv(pg) = iserv

cgpe-staz      l_simu(iserv) = .true.
      l_simu(iserv) = .false.
      if (set_flag(pg).ne.0) then
        l_simu(iserv) = .true.
      endif
cgpe-staz-end

!-numero di assetti dati in input (cmf_assetti.inc)
      num_servizi = iserv

!-indice dell'assetto dato in input (cmf_assetti.inc)
      numrig_ass_mf(iserv) = assetto_cm(pg)

!-check se la centrale e' binaria o MF
      if (assetto_cm(pg) .gt. 0) then
!-flag tipo di ottimizzazione (.T. ==> MF)
        lmf = .true.
      end if


!-imposizione del tipo e stato di funzionamento iniziali (queste condizioni
!-iniziali possono venire modificate nel corso della simulazione)
      htelec(pg) = htcitj(istaz)
      hstato(pg) = STATai(hstatj(istaz))

cgpe      call init_stvar(istaz,pg)
cgpe
cgpe!-determinazione di type_avail e di type_act
cgpe      i1 = first_unit(pg)
cgpe      do j = 1,type_num(pg)
cgpe        jk = first_type(pg)+j-1
cgpe        type_avail(jk) = 0
cgpe        type_act(jk) = 0
cgpe        do kk = 1,type_quant(jk)
cgpe          ii1 = i1+kk-1
cgpe          if (unit_avail(ii1).ge.1) type_avail(jk) = type_avail(jk)+1
cgpe          if (status(ii1).eq.funz_on) type_act(jk) = type_act(jk)+1
cgpe        enddo
cgpe        i1 = i1+type_quant(jk)
cgpe      enddo

!-inizializzazione di l_act
      i1 = first_unit(pg)
      do j = i1 ,i1+unit_num(pg)-1
        l_act(j) = .false.
      enddo

cgpe!-determinazione di vert_typ
cgpe      call init_vert_type(pg,type_quant(first_type(pg)),vert_typ(pg))
cgpe
      call init_state(istaz,pg)
cgpe-end

!-inizializzazione del consumo imposto
      if (fl_cons) then
        if (lmf) then
          cons_serv_imp(iserv) = 0.
        else
          cons_imp = 0.
        endif
      else
        if (lmf) then
          cons_serv_imp(iserv) = consty(istaz)
        else
          cons_imp = consty(istaz)
        endif
      endif

!-eventuale inizializzazione delle variabili relative alla centrale jstaz
      if (jstaz .ne. I_UND) then
         
        pg = pgrato(jstaz)
        iserv = iserv + 1
        i_staz(iserv) = jstaz
        i_vert(iserv) = pg
        i_serv(pg) = iserv
        numrig_ass_mf(iserv) = assetto_cm(pg)
        fl_simu(pg) = .true.
        num_servizi = iserv

cgpe-staz        l_simu(iserv) = .true.
        l_simu(iserv) = .false.
        if (set_flag(pg).ne.0) then
          l_simu(iserv) = .true.
        endif
cgpe-staz-end

!-imposizione del tipo e stato di funzionamento iniziali (queste condizioni
!-iniziali possono venire modificate nel corso della simulazione)
        htelec(pg) = htcitj(jstaz)
        hstato(pg) = STATai(hstatj(jstaz))

!-inizializzazione di l_act
        i1 = first_unit(pg)
        do j = i1 ,i1+unit_num(pg)-1
          l_act(j) = .false.
        enddo

        call init_state(jstaz,pg)

!-inizializzazione del consumo imposto
        cons_imp = 0.
        cons_serv_imp(iserv) = 0.
        if (fl_cons) then
          if (lmf) then
            cons_serv_imp(iserv) = 0.
          else
            cons_imp = 0.
          endif
        else
          if (lmf) then
            cons_serv_imp(iserv) = consty(jstaz)
          else
            cons_imp = consty(jstaz)
          endif
        endif
      endif
c
      return
      END ! init_loc_var

      Subroutine ottimo_cent_all (istaz,jstaz)

!- Modulo per l'ottimizzazione di una centrale utilizzato sia nell'ottimo
!- di rete, sia in argo manuale:
!  - vengono settate le variabili locali necessarie alla simulazione, in
!    particolare viene settato a false un flag per indicare che la
!    centrale e' gia' stata simulata (ha importanza per le centrali
!    provenienti dalla stessa MF)
!  - viene chiamata la ottimo_cent oppure la ottimo_cent_serie
*&*
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
      include '../inc/tv.inc'
      include '../inc/flag.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/cap_bon.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'

CMAR_ASS_COND
      	include '../inc/ASS_COND.INC'
CMAR_ASS_COND 

      INTEGER*4 istaz,jstaz     ! I)

      INTEGER*4 pg,jstaz_aux,ivert,jvert,ia,ier,jstaz_app
      logical*2 ce_avail,ce_serie
c-------------------------------------------------------------------------
      pg = pgrato(istaz)
      if (hstatj(istaz) .ne. STA_UND) then
        if (ce_serie(istaz)) then
!-trattamento centrale in serie
          if (.not. fl_simu(pgrato(istaz)) .and.
     *        .not. fl_simu(pgrato_app(istaz))) then
c-elena 17/10- aggiungo valorizzazione valori intermedi usati in caso KO
          call find_ce_serie (istaz,jstaz,ivert,jvert)
          call save_cin_s (ivert)

            call ottimo_cent_serie (istaz)
cgpe            fl_simu(pgrato(istaz)) = .true.
cgpe            fl_simu(pgrato_app(istaz)) = .true.
          endif
        else
cgpe          if (ce_avail(istaz)) then
cstaz-corr          if (ce_avail(pg)) then
          if (ce_avail(pg) .and. set_flag(pg)) then
cstaz-corr-end
!-ISTAZ E' DISPONIBILE (=> istaz ha almeno una unità disponibile)
            if (.not. fl_simu(pg)) then
!-ISTAZ E' DISPONIBILE E NON E' STATA CONSIDERATA
              if (jstaz .ne. I_UND) then
!-ISTAZ E' DISPONIBILE, NON E' STATA CONSIDERATA ED E' ASSOCIATA A JSTAZ
                if (hstatj(jstaz) .eq. STA_UND) then
!-se JSTAZ non e' da trattare, viene settata fl_simu(pgrato(jstaz))=.true.
                  fl_simu(pgrato(jstaz)) = .true.
                endif
                if (.not.fl_simu(pgrato(jstaz)) ) then
!-ISTAZ E' DISPONIBILE, NON E' STATA CONSIDERATA ED E' ASSOCIATA A JSTAZ E
!-JSTAZ NON E' STATA CONSIDERATA
cgpe                  if (ce_avail(jstaz)) then
cstaz-corr                  if (ce_avail(pgrato(jstaz))) then
                  if (ce_avail(pgrato(jstaz)) .and.
     *                set_flag(pgrato(jstaz))       ) then
cstaz-corr-end
!-ISTAZ E JSTAZ SONO DISPONIBILI E NON ANCORA TRATTATE
                    call init_loc_var (istaz,jstaz)
                    call ottimo_cent ()
                  else
!-ISTAZ E' DISPONIBILE, NON E' STATA CONSIDERATA ED E' ASSOCIATA A JSTAZ,
!-MA JSTAZ NON HA UNITA' DISPONIBILI
                    jstaz_aux = I_UND

      IF(FLAG_ASS_COND(ISTAZ))THEN
      jstaz_aux=jstaz
	ENDIF
                    call init_loc_var (istaz,jstaz_aux)
                    call init_ce_nd (jstaz)
                    call ottimo_cent ()
      IF(FLAG_ASS_COND(ISTAZ))THEN
      jstaz_aux = I_UND
	ENDIF
                  endif

                else
!-ISTAZ E' DISPONIBILE, NON E' STATA CONSIDERATA ED E' ASSOCIATA A JSTAZ,
!-MA JSTAZ E' DISPONIBILE ED E' STATA GIA' TRATTATA
                  jstaz_aux = I_UND
                  call init_loc_var (istaz,jstaz_aux)
                  call ottimo_cent ()
                endif
              else
!-ISTAZ E' DISPONIBILE E BINARIA (e JSTAZ = I_UND)
                call init_loc_var (istaz,jstaz)
                call ottimo_cent ()
              endif
            endif
          else
!-ISTAZ NON E' DISPONIBILE (BINARIA / MULTIFUNZIONE)
            if (.not.fl_simu(pgrato(istaz))) then
              jstaz_aux = I_UND
              call init_loc_var (istaz,jstaz_aux)
              call init_ce_nd (istaz)
cstaz-corr
c simulazione dell'assetto jstaz
              jstaz_aux = I_UND
              call init_loc_var (jstaz,jstaz_aux)
              call ottimo_cent ()
cstaz-corr-end
            endif
          endif
        endif  !-fine trattamento centrale in serie
cgpe      else
cgpe        msg_ce = 'Centrale non trattata'
      endif ! if (hstatj(istaz) .ne. STA_UND)
c
      return
      end ! ottimo_cent_all
c-----------------------------------------------------------------------------------------------
c
      Subroutine simula_cent()
c***********************************************************************************************
c Chiamata dalla simula_cent_all() per il trattamento della centrale binaria.
c***********************************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/tv.inc'
      include '../inc/rk_param.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/MF_TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
      include '../inc/messag.inc'
      include '../inc/cmf_assetti.inc'

	include '../inc/ASS_COND.INC'
      INTEGER*4 ier,iserv
      INTEGER*4 I,J,K,iu,ia
      LOGICAL*2 iopt_aux
      character*(tit_len) msg_err
      INTEGER*4 I_ERR
      INTEGER*4 ker
      character*(l_N_RTN) nome_routine
	logical*2 l_act_(m_ogg)
	integer*4 iserv_,h(m_ogg)
c-------------------------------------------------------------------------
      msg_err = ' '

!-per la simulazione di una centrale MF
cgpe      if (lmf .and. (.not.iopt)) then
cgpe
cgpeC---> controllo che la pressione di mandata della stazione non superi la
cgpeC     massima pressione del collettore
cgpece   (controllo effettuato su ogni assetto attivo)
cgpeC---> controllo della compatibilita' degli assetti dati in input
cgpece    la compatibilità tra due assetti viene definita dopo la lettura dei dati topologici 
cgpece    relativi agli assetti (vd.subroutine lettura_top_assetti(...))
cgpe-end

cgpeC---> se abbiamo piu' di due servizi, controllo che le unita' da simulare
cgpeC     non siano accese per entrambi i servizi
cgpe        if (num_servizi.gt.1) then
cgpe          do i=1,unit_num(i_vert(1))     !unit_num=numero effettivo unità
cgpe            j = first_unit(i_vert(1))+i-1 
cgpe            k = first_unit(i_vert(2))+i-1 
cgpece first_unit(ivert(i))=posizione 1°unità del servizio i-esimo
cgpe            if((status(j).eq.funz_on).and.(status(k).eq.funz_on))then
cgpec ier = 12: unita' accesa in entrambi i servizi
cgpe              ier = 12
cgpe              do iserv = 1, num_servizi
cgpe                err(i_vert(iserv)) = ier
cgpe              enddo
cgpe              msg_err = 'Unita'' ON in due assetti'
cgpe              call gest_error (2,0,'SIMULA_CENT',msg_err,0)
cgpe              return
cgpe            end if
cgpe          end do
cgpe        end if
cgpe      end if !if (lmf .and. (.not.iopt))

Ce  viene settato il flag iopt_aux per il tipo di simulazione:
                         !    = .F. ==> simulazione vera e propria
                         !    = .T. ==> simulazione dello stato ottimo
      iopt_aux = iopt
CMAR:    SE VERIFICATA LA CONDIZIONE DEL FLAG DEGLI ASSETTI CONDIVISI: 


      
ccccccccccccccccccccccccccc	NUM_SERVIZI=1
CMAR:    
CMAR        
CMAR_ASS_COND-15-01-11
C---> simulazione dei servizi
CMAR
       IF(FLAG_ASS_COND_S)THEN
       num_servizi=1
CMAR
      do iserv = 1 ,num_servizi
       if (l_simu(iserv)) then
!-inizializzazione di stations.inc
        tot_cons(i_vert(iserv)) = 0.   !consumo totale
        tmand_stat(i_vert(iserv)) = t0 !temperatura di mandata
        ind_serv = iserv               !indice servizio
c
ce inizializzazione delle grandezze dipendenti dalla composizione del gas
cgpe forse questo non serve
        call init_stvar(i_staz(iserv),i_vert(iserv))
c-----------------------------------------------------------------------------------
ce i_staz(iserv)=indice della stazione nella struttura dati dimensionata
ce                             a M_OGG (coincide con ind di stazione.inc)
ce i_vert(iserv)=indice della stazione nella struttura interna di dati di stazione
c-----------------------------------------------------------------------------------
c
c definizione del tipo di centrale:semplice,composta o mista  
        call init_vert_type(i_vert(iserv),
     *       type_act(first_type(i_vert(iserv))),
     *       vert_typ(i_vert(iserv)))
c     
cmar_ass_cond
      IF(FLAG_ASS_COND_S)THEN
	FLAG_S = .TRUE.
	ENDIF


cmar_ass_cond
c calcola i dati di stazione e chiama la optim_vertex(..) per la simulazione
        call calc_dati_cent(iopt_aux,i_staz(iserv),i_vert(iserv),ier)
c
cgpe-new
CMAR_ASS_COND
      FLAG_S = .FALSE.
CMAR_ASS_COND



cmar    if flag
      

ccccccccccc	   call tbg_ass(ier)
c     do i=1,2   ! i=iserv
c	
	
c	if(i.eq.1)then
c      call assetto1 (i_staz(i))
c	elseif(i.eq.2)then
c      call assetto2 (i_staz(i))
c	endif
  
c      enddo
cc 


cmar_Ass_cond 
cmar_ass_cond_rpt	do j=1,24
      do j=first_type(i_vert(iserv)),first_type(i_vert(iserv))+23
	l_act_(j)=.false.
	enddo


cmar_ass_cond_rpt	i=1
      i=first_type(i_vert(iserv))
cmar_ass_Cond_rpt     
      do j=first_type(i_vert(iserv)),first_type(i_vert(iserv))+23


      
	if(unit_avail_as(j).ne.0)then
	l_act_(j)=l_act(i)  
	i=i+1
	endif
      
       
	enddo



cmar_ass_cond_rpt
      do j=first_type(i_vert(iserv)),first_type(i_vert(iserv))+23


	l_act(j)=.false.


	l_act(j)=l_act_(j)


	enddo

cmar_ass_cond_rpt     
      do j=first_type(i_vert(iserv)),first_type(i_vert(iserv))+23
              lstatus(j) = OFF
              if (l_act(j)) then
c	h(j)=1
                lstatus(j) = ON
              endif
           enddo

cc
CCCCCCCCCCCCCCCCCCCCCCCCCCC        call tbg_ass(ier)

cc
      
        if (tipo_criterio(i_vert(iserv)) .eq. crit_man) then
          err(i_vert(iserv)) = ier
          if (ier .ne. 0) then
cgpe-corr            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-corr            fl_ris_ce(i_vert(iserv)) = 1
            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-end
          endif
        endif
cgpe-new-end
c Controllo sugli errori che derivano dalla fattibilita' dei punti. 
        call gest_errore(i_staz(iserv),ier,tipo_criterio(i_vert(iserv)),
     *       vert_typ(i_vert(iserv)),
     *       stat_ord(i_vert(iserv)),i_err)
c-----------------------------------------------------------------------------
ce   i_err=0  errore non fatal durante la simulazione
ce   i_err=11 errore fatal durante la simulazione
c-----------------------------------------------------------------------------
cmar_ass_cond
   
cmr_ass_Cond

        if (i_err.gt.0) then
c la simulazione del servizio iserv non ha trovato un punto di lavoro fattibile
          if (fl_sim) then
            nome_routine = 'SIMULA_CENT'
            if (.not.iopt_mess) then
              if (lmf) then
cgpe                call write_id_ce (nome_routine,i_staz(iserv),
cgpe     *               i_vert(iserv))
cgpe              endif
cgpe-new                msg_err = 'Non esiste punto simulato'
cgpe-new                call gest_error (2,0,'SIMULA_CENT',msg_err,0)
              endif
            endif
          endif
        else
cmar_Ass_cond
cmar_Ass_condccccccc      do iserv_=1,2
cmar_Ass_cond
cmar_Ass_cond          iu = first_unit(i_vert(iserv_))
cmar_Ass_cond          do i = 1, type_num(i_vert(iserv_))
cmar_Ass_cond            k = first_type(i_vert(iserv_))+i-1
cmar_Ass_cond            do j = iu,iu+type_quant(k)-1
     
cmar_Ass_cond            iu = iu + type_quant(k)
cmar_Ass_condccccccc          enddo  
cmar_Ass_cond
cc      do j=1, 24
cc        lstatus(j) = OFF
cc              if (l_act(j)) then
cc              lstatus(j) = on
cc              endif



cc      enddo
cmar_Ass_cond
        endif
cgpe-staz
       else
c gestione di una centrale spenta (il cui set_flag=0)
         tot_cons(i_vert(iserv)) = 0.
         tmand_stat(i_vert(iserv)) = tasp_stat(i_vert(iserv))
         comp_ratio(i_vert(iserv)) = 1.
c         call spegni_staz(unit_num(i_vert(iserv)),
c     *                    first_unit(i_vert(iserv)))
cgpe-staz-end
       endif
      enddo


cmar
      
	ELSE

cmar     
C---> simulazione dei servizi
      do iserv = 1 ,num_servizi
       if (l_simu(iserv)) then
!-inizializzazione di stations.inc
        tot_cons(i_vert(iserv)) = 0.   !consumo totale
        tmand_stat(i_vert(iserv)) = t0 !temperatura di mandata
        ind_serv = iserv               !indice servizio
c
ce inizializzazione delle grandezze dipendenti dalla composizione del gas
cgpe forse questo non serve
        call init_stvar(i_staz(iserv),i_vert(iserv))
c-----------------------------------------------------------------------------------
ce i_staz(iserv)=indice della stazione nella struttura dati dimensionata
ce                             a M_OGG (coincide con ind di stazione.inc)
ce i_vert(iserv)=indice della stazione nella struttura interna di dati di stazione
c-----------------------------------------------------------------------------------
c
c definizione del tipo di centrale:semplice,composta o mista  
        call init_vert_type(i_vert(iserv),
     *       type_act(first_type(i_vert(iserv))),
     *       vert_typ(i_vert(iserv)))
c
c calcola i dati di stazione e chiama la optim_vertex(..) per la simulazione
        call calc_dati_cent(iopt_aux,i_staz(iserv),i_vert(iserv),ier)
c
cgpe-new
        if (tipo_criterio(i_vert(iserv)) .eq. crit_man) then
          err(i_vert(iserv)) = ier
          if (ier .ne. 0) then
cgpe-corr            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-corr            fl_ris_ce(i_vert(iserv)) = 1
            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-end
          endif
        endif
cgpe-new-end
c Controllo sugli errori che derivano dalla fattibilita' dei punti. 
        call gest_errore(i_staz(iserv),ier,tipo_criterio(i_vert(iserv)),
     *       vert_typ(i_vert(iserv)),
     *       stat_ord(i_vert(iserv)),i_err)
c-----------------------------------------------------------------------------
ce   i_err=0  errore non fatal durante la simulazione
ce   i_err=11 errore fatal durante la simulazione
c-----------------------------------------------------------------------------
        if (i_err.gt.0) then
c la simulazione del servizio iserv non ha trovato un punto di lavoro fattibile
          if (fl_sim) then
            nome_routine = 'SIMULA_CENT'
            if (.not.iopt_mess) then
              if (lmf) then
cgpe                call write_id_ce (nome_routine,i_staz(iserv),
cgpe     *               i_vert(iserv))
cgpe              endif
cgpe-new                msg_err = 'Non esiste punto simulato'
cgpe-new                call gest_error (2,0,'SIMULA_CENT',msg_err,0)
              endif
            endif
          endif
        else
          iu = first_unit(i_vert(iserv))
          do i = 1, type_num(i_vert(iserv))
            k = first_type(i_vert(iserv))+i-1
            do j = iu,iu+type_quant(k)-1
              lstatus(j) = OFF
              if (l_act(j)) then
                lstatus(j) = ON
              endif
            enddo
            iu = iu + type_quant(k)
          enddo  
        endif
cgpe-staz
       else
c gestione di una centrale spenta (il cui set_flag=0)
         tot_cons(i_vert(iserv)) = 0.
         tmand_stat(i_vert(iserv)) = tasp_stat(i_vert(iserv))
         comp_ratio(i_vert(iserv)) = 1.
c         call spegni_staz(unit_num(i_vert(iserv)),
c     *                    first_unit(i_vert(iserv)))
cgpe-staz-end
       endif
      enddo
c
CMAR
      ENDIF
CMAR
cc    
c	ier(i_vert(iserv))=ier
c      ier(i_vert(iserv+1))=ier
cc	

      return
      end   !simula_cent
c-------------------------------------------------------------------------

c-------------------------------------------------------------------------

      Subroutine simula_cent___()
c***********************************************************************************************
c Chiamata dalla simula_cent_all() per il trattamento della centrale binaria.
c***********************************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/tv.inc'
      include '../inc/rk_param.inc'
      include '../inc/flag.inc'
      include '../inc/err.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/MF_TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/simpstat.inc'
      include '../inc/messag.inc'
      include '../inc/cmf_assetti.inc'

	include '../inc/ASS_COND.INC'
      INTEGER*4 ier,iserv
      INTEGER*4 I,J,K,iu,ia
      LOGICAL*2 iopt_aux
      character*(tit_len) msg_err
      INTEGER*4 I_ERR
      INTEGER*4 ker
      character*(l_N_RTN) nome_routine
	logical*2 l_act_(24)
	integer*4 iserv_,h(24)
c-------------------------------------------------------------------------
      msg_err = ' '

!-per la simulazione di una centrale MF
cgpe      if (lmf .and. (.not.iopt)) then
cgpe
cgpeC---> controllo che la pressione di mandata della stazione non superi la
cgpeC     massima pressione del collettore
cgpece   (controllo effettuato su ogni assetto attivo)
cgpeC---> controllo della compatibilita' degli assetti dati in input
cgpece    la compatibilità tra due assetti viene definita dopo la lettura dei dati topologici 
cgpece    relativi agli assetti (vd.subroutine lettura_top_assetti(...))
cgpe-end

cgpeC---> se abbiamo piu' di due servizi, controllo che le unita' da simulare
cgpeC     non siano accese per entrambi i servizi
cgpe        if (num_servizi.gt.1) then
cgpe          do i=1,unit_num(i_vert(1))     !unit_num=numero effettivo unità
cgpe            j = first_unit(i_vert(1))+i-1 
cgpe            k = first_unit(i_vert(2))+i-1 
cgpece first_unit(ivert(i))=posizione 1°unità del servizio i-esimo
cgpe            if((status(j).eq.funz_on).and.(status(k).eq.funz_on))then
cgpec ier = 12: unita' accesa in entrambi i servizi
cgpe              ier = 12
cgpe              do iserv = 1, num_servizi
cgpe                err(i_vert(iserv)) = ier
cgpe              enddo
cgpe              msg_err = 'Unita'' ON in due assetti'
cgpe              call gest_error (2,0,'SIMULA_CENT',msg_err,0)
cgpe              return
cgpe            end if
cgpe          end do
cgpe        end if
cgpe      end if !if (lmf .and. (.not.iopt))

Ce  viene settato il flag iopt_aux per il tipo di simulazione:
                         !    = .F. ==> simulazione vera e propria
                         !    = .T. ==> simulazione dello stato ottimo
      iopt_aux = iopt
CMAR:    SE VERIFICATA LA CONDIZIONE DEL FLAG DEGLI ASSETTI CONDIVISI: 


      
ccccccccccccccccccccccc
CMAR:    
CMAR
C---> simulazione dei servizi

cmar
      IF(FLAG_ASS_COND_S)THEN
cmar
      do iserv = 1 ,num_servizi
       if (l_simu(iserv)) then
!-inizializzazione di stations.inc
        tot_cons(i_vert(iserv)) = 0.   !consumo totale
        tmand_stat(i_vert(iserv)) = t0 !temperatura di mandata
        ind_serv = iserv               !indice servizio
c
ce inizializzazione delle grandezze dipendenti dalla composizione del gas
cgpe forse questo non serve
        call init_stvar(i_staz(iserv),i_vert(iserv))
c-----------------------------------------------------------------------------------
ce i_staz(iserv)=indice della stazione nella struttura dati dimensionata
ce                             a M_OGG (coincide con ind di stazione.inc)
ce i_vert(iserv)=indice della stazione nella struttura interna di dati di stazione
c-----------------------------------------------------------------------------------
c
c definizione del tipo di centrale:semplice,composta o mista  
        call init_vert_type(i_vert(iserv),
     *       type_act(first_type(i_vert(iserv))),
     *       vert_typ(i_vert(iserv)))
c     
cmar_ass_cond
      IF(FLAG_ASS_COND_S)THEN
	FLAG_S = .TRUE.
	ENDIF


cmar_ass_cond
c calcola i dati di stazione e chiama la optim_vertex(..) per la simulazione
        call calc_dati_cent(iopt_aux,i_staz(iserv),i_vert(iserv),ier)
c
cgpe-new
CMAR_ASS_COND
      FLAG_S = .FALSE.
CMAR_ASS_COND



cmar_ass_cond   se c'e il flag per gli assetti condivisi:
cmar    if flag
      

ccccccccccc	   call tbg_ass(ier)
c     do i=1,2   ! i=iserv
c	
	
c	if(i.eq.1)then
c      call assetto1 (i_staz(i))
c	elseif(i.eq.2)then
c      call assetto2 (i_staz(i))
c	endif
  
c      enddo
cc setto qui gli stati!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	i=1
	do j=1,24
	l_act_(j)=.false.
	enddo

      do j=1,24

	if(unit_avail_as(j).ne.0)then
	l_act_(j)=l_act(i)  
	i=i+1
	endif
       
       
	enddo



	do j=1,24

	l_act(j)=.false.


	l_act(j)=l_act_(j)


	enddo

      do j=1,24
              lstatus(j) = OFF
              if (l_act(j)) then
c	h(j)=1
                lstatus(j) = ON
              endif
           enddo

cc
CCCCCCCCCCCCCCCCCCCCCCCCCCC        call tbg_ass(ier)

cc
      
        if (tipo_criterio(i_vert(iserv)) .eq. crit_man) then
          err(i_vert(iserv)) = ier
          if (ier .ne. 0) then
cgpe-corr            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-corr            fl_ris_ce(i_vert(iserv)) = 1
            fl_ris_ce(i_vert(iserv)) = -1
cgpe-corr-end
          endif
        endif
cgpe-new-end
c Controllo sugli errori che derivano dalla fattibilita' dei punti. 
        call gest_errore(i_staz(iserv),ier,tipo_criterio(i_vert(iserv)),
     *       vert_typ(i_vert(iserv)),
     *       stat_ord(i_vert(iserv)),i_err)
c-----------------------------------------------------------------------------
ce   i_err=0  errore non fatal durante la simulazione
ce   i_err=11 errore fatal durante la simulazione
c-----------------------------------------------------------------------------
cmar_ass_cond
   
cmr_ass_Cond

        if (i_err.gt.0) then
c la simulazione del servizio iserv non ha trovato un punto di lavoro fattibile
          if (fl_sim) then
            nome_routine = 'SIMULA_CENT'
            if (.not.iopt_mess) then
              if (lmf) then
cgpe                call write_id_ce (nome_routine,i_staz(iserv),
cgpe     *               i_vert(iserv))
cgpe              endif
cgpe-new                msg_err = 'Non esiste punto simulato'
cgpe-new                call gest_error (2,0,'SIMULA_CENT',msg_err,0)
              endif
            endif
          endif
        else
cmar_Ass_cond
cmar_Ass_condccccccc      do iserv_=1,2
cmar_Ass_cond
cmar_Ass_cond          iu = first_unit(i_vert(iserv_))
cmar_Ass_cond          do i = 1, type_num(i_vert(iserv_))
cmar_Ass_cond            k = first_type(i_vert(iserv_))+i-1
cmar_Ass_cond            do j = iu,iu+type_quant(k)-1
     
cmar_Ass_cond            iu = iu + type_quant(k)
cmar_Ass_condccccccc          enddo  
cmar_Ass_cond
cc      do j=1, 24
cc        lstatus(j) = OFF
cc              if (l_act(j)) then
cc              lstatus(j) = on
cc              endif



cc      enddo
cmar_Ass_cond
        endif
cgpe-staz
       else
c gestione di una centrale spenta (il cui set_flag=0)
         tot_cons(i_vert(iserv)) = 0.
         tmand_stat(i_vert(iserv)) = tasp_stat(i_vert(iserv))
         comp_ratio(i_vert(iserv)) = 1.
c         call spegni_staz(unit_num(i_vert(iserv)),
c     *                    first_unit(i_vert(iserv)))
cgpe-staz-end
       endif
      enddo


c     
cc    
c	ier(i_vert(iserv))=ier
c      ier(i_vert(iserv+1))=ier
cc	
cmar
      else
cmar
      

	endif

cmar


      return
      end   !simula_cent
c-------------------------------------------------------------------------

c-------------------------------------------------------------------------

      subroutine assetto1(istaz)

	  implicit none
C
C      Calcola i dati della stazione e viene chiamata:
C      1) per il calcolo del punto simulato di una centrale binaria o di
C         un assetto di una centrale multifunzione
C      2) per il calcolo dei dati di stazione e di unita' per i
C         turbocompressori accesi dall'ottimo (di una centrale binaria o
C         di una centrale multifunzione)
C      
cgpe Nota: le unità da simulare sono memorizzate rispettivamente:
cgpe       - if (.not.lmf) in ind_s(ind_opt,1:type_num) dalla subroutine optcent
cgpe       - elseif (lmf)  in ind_u(1:unit_num)=0/1/2 per assegnare una unità al servizio 1 o 2
cgpe       Se si è fatta la correzione del punto di lavoro, nella suboruitne calc_corr ci si
cgpe       è riporatati a uno di questi casi
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/stations.inc'
      include '../inc/messag.inc'

      include '../inc/OTC.INC'
      include '../inc/OTC_TIME.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/flag.inc'


	 
      

	INTEGER*4 ISTAZ,i
      

	do i= 1,12
	unit_flow(i)=unit_flow(i)
	enddo


	return 
	end


	
      subroutine assetto2(istaz)

	  implicit none
C
C      Calcola i dati della stazione e viene chiamata:
C      1) per il calcolo del punto simulato di una centrale binaria o di
C         un assetto di una centrale multifunzione
C      2) per il calcolo dei dati di stazione e di unita' per i
C         turbocompressori accesi dall'ottimo (di una centrale binaria o
C         di una centrale multifunzione)
C      
cgpe Nota: le unità da simulare sono memorizzate rispettivamente:
cgpe       - if (.not.lmf) in ind_s(ind_opt,1:type_num) dalla subroutine optcent
cgpe       - elseif (lmf)  in ind_u(1:unit_num)=0/1/2 per assegnare una unità al servizio 1 o 2
cgpe       Se si è fatta la correzione del punto di lavoro, nella suboruitne calc_corr ci si
cgpe       è riporatati a uno di questi casi
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/stations.inc'
      include '../inc/messag.inc'

      include '../inc/OTC.INC'
      include '../inc/OTC_TIME.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/flag.inc'

      

      INTEGER*4 ISTAZ,i

	do i= 1,12
	unit_flow(i)=unit_flow(i+12)
	enddo

	return 
	end

      Subroutine calc_dati_cent(iopt_aux,istaz,ivert,ier)
c
      implicit none
C
C      Calcola i dati della stazione e viene chiamata:
C      1) per il calcolo del punto simulato di una centrale binaria o di
C         un assetto di una centrale multifunzione
C      2) per il calcolo dei dati di stazione e di unita' per i
C         turbocompressori accesi dall'ottimo (di una centrale binaria o
C         di una centrale multifunzione)
C      
cgpe Nota: le unità da simulare sono memorizzate rispettivamente:
cgpe       - if (.not.lmf) in ind_s(ind_opt,1:type_num) dalla subroutine optcent
cgpe       - elseif (lmf)  in ind_u(1:unit_num)=0/1/2 per assegnare una unità al servizio 1 o 2
cgpe       Se si è fatta la correzione del punto di lavoro, nella suboruitne calc_corr ci si
cgpe       è riporatati a uno di questi casi

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/stations.inc'
      include '../inc/messag.inc'

      include '../inc/OTC.INC'
      include '../inc/OTC_TIME.INC'
      include '../inc/cmf_assetti.inc'
      include '../inc/flag.inc'

cmar-p-29-07-09
      include '../inc/filtri.inc'
      include '../inc/air_cooler.inc'

         
cmar-p-29-07-09

cc
      include '../inc/ass_cond.inc'
cc
c
      logical*2 iopt_aux ! I) flag per il tipo di simulazione:
                         !    = .F. ==> simulazione vera e propria
                         !    = .T. ==> simulazione dello stato ottimo
      INTEGER*4 ISTAZ,   ! I) indice della stazione nella struttura completa
     *          IVERT    ! I)   "      "     "        "    " di dati di stazione
      INTEGER*4 ier      ! O) indice di errore della optim_vertex
c
      INTEGER*4 i,j,i1,IU,K,II1,kj,i_ass
      REAL*4    flow_in
      INTEGER*4 vert_typ_old
      CHARACTER*3 status_old(maxunits)
      REAL*4    stat_vars_old(10)

      logical*2 fl_act,ce_off
      character*(tit_len) msg_err
c-------------------------------------------------------------------------



c viene settata la variabile stat in base rispettivamente al vettore ind_s
c (le unita' da simulare sono quelle con valore pari a 1) e ind_u (le unita'
c da simulare per il servizio s1 sono quelle con valore pari a 1), prima per
c le unita' che devono essere necessariamente accese, successivamente per le
c altre unita'
c nota: stat (nel common simpstat) e status_old sono dimensionati a maxunits,
c       mentre status e' dimensionato a max_unit


!-salvataggio del valore dello stato delle unita' dato in input
      do i = 1,unit_num(ivert)
        j = first_unit(ivert)+i-1
        status_old(i) = status(j)
      end do

!-inizializzazione delle variabili
      if (iopt_aux) then
!-CASO: simulazione dello stato ottimo o di una centrale binaria o di un
!-      assetto di una multifunzione
        ier = 0
        do i = 1,type_num(ivert)
          j = first_type(ivert)+i-1
          type_act(j) = 0
        end do

        if (lmf) then
          ind_opt = 1
        end if

!-viene settata la variabile stat uguale a funz_on per le unita' che devono
!-essere necessariamente accese
        i1 = 0
        iu = first_unit(ivert)
        do i = 1,type_num(ivert)
          j = first_type(ivert)+i-1
          do k = 1,type_quant(j)
            ii1 = iu+k-1
            if (unit_avail(ii1).gt.1 .and.
     *          ((.not.lmf .and. ind_s(ind_opt,i).gt.0) .or.
     *           (lmf .and. ind_u(k+i1).eq.i_serv(ivert)))) then
              stat(k+i1)  = funz_on
              status(ii1) = funz_on
              type_act(j) = type_act(j) + 1
              if (.not.lmf) then
                ind_s(ind_opt,i) = ind_s(ind_opt,i)-1
              else
                ind_u(k+i1) = 0
              end if
            else
              stat(k+i1)  = funz_off
              status(ii1) = funz_off
            end if
          end do
          i1 = i1+type_quant(j)
          iu = iu+type_quant(j)
        end do

        i1 = 0
        iu = first_unit(ivert)
        do i = 1,type_num(ivert)
          j = first_type(ivert)+i-1
          do k = 1,type_quant(j)
            ii1 = iu+k-1
            if ((.not.lmf                  .and.
     *           (stat(k+i1).eq.funz_off)     .and.
     *           (ind_s(ind_opt,i).gt.0)        ) .or.
     *          (lmf                       .and.
     *           (stat(k+i1).eq.funz_off)     .and.
     *           (ind_u(k+i1).eq.ind_serv)      ) ) then
              if (unit_avail(ii1).ge.1) then
                stat(k+i1)  = funz_on
                status(ii1) = funz_on
              else
                do kj = k+1,type_quant(j)
                  if (.not.lmf .and.
     *                unit_avail(iu+kj-1).ge.1  .and.
     *                status(iu+kj-1) .eq. funz_off   ) then
                    stat(kj+i1) = funz_on
                    status(iu+kj-1) = funz_on
                    goto 11

                  elseif (lmf .and. ind_u(kj+i1).eq.0 .and.
     *                    unit_avail(iu+kj-1).ge.1  .and.
     *                    status(iu+kj-1) .eq. funz_off   ) then
                    stat(kj+i1) = funz_on
                    status(iu+kj-1) = funz_on
                    ind_u(kj+i1) = 0
                    goto 11
                  endif
                enddo
              endif

11            continue

              type_act(j) = type_act(j) + 1
              if (.not.lmf) then
                ind_s(ind_opt,i) = ind_s(ind_opt,i)-1
              else
                ind_u(k+i1) = 0
              end if
            end if
          end do
          i1 = i1+type_quant(j)
          iu = iu+type_quant(j)
        end do

!-determinazione delle unita' attive
cgpe        set_flag(ivert) = 0
        do i = 1,unit_num(ivert)
          j = first_unit(ivert)+i-1
          if (stat(i).eq.funz_on) then
            l_act(j) = .true.
cgpe            set_flag(ivert) = 1
          else
            l_act(j) = .false.
          end if
        end do


!-assegnazione del vert_typ, se il funzionamento e' f_serie
        if (stat_ord(ivert).eq.2) then
          vert_typ_old = vert_typ(ivert)
          call init_vert_type(ivert,type_act(first_type(ivert)),
     *         vert_typ(ivert))
        end if

      else
!-CASO: simulazione delle unita' dichiarate ON di una centrale binaria o
!-      di un assetto di una multifunzione

!-determinazione delle unita' attive
        do i = 1,unit_num(ivert)
          j = first_unit(ivert)+i-1
          if (status(j).eq.funz_on) then
            l_act(j) = .true.
          else
            l_act(j) = .false.
          end if
        end do

      end if ! if (iopt_aux)

      iopt = .false.

cgpe non dovrebbe essere più necessario, in quanto non c'è implementata
cgpe una logica di cambio assetto
!-calcolo delle caratteristiche topologiche della centrale
      if (lmf .and. iopt_aux .and. .not. iopt_mess) then

!-salvataggio delle caratteristiche topologiche della centrale
        stat_vars_old(3) = stat_vars(3,ivert)
        stat_vars_old(4) = stat_vars(4,ivert)
        stat_vars_old(5) = stat_vars(5,ivert)
        stat_vars_old(6) = stat_vars(6,ivert)
        stat_vars_old(7) = stat_vars(7,ivert)
      endif

      if (iopt_aux) then
        call init_vert_type (ivert,type_act(first_type(ivert)),
     *       vert_typ(ivert))
      endif
cmar_ass_cond

cmar_ass_cond
!-calcolo del punto di lavoro
      call optim_vertex(istaz,iopt,vert_typ(ivert),tipo_criterio(ivert),
     *ivert,flow_in,flow_stat(ivert),
     *pasp_stat(ivert),pmand_stat(ivert),tasp_stat(ivert),
     *tmand_stat(ivert),TARIA(ivert),delpr1(ivert),delpr2(ivert),
     *comp_ratio(ivert),tot_cons(ivert),flowm(ivert),
     *flow_ric(ivert),head(ivert),unit_num(ivert),
     *stat_ord(ivert),first_type(ivert),type_num(ivert),
     *first_unit(ivert),stat_vars(1,ivert),pres_int(ivert),
     *temp_int(ivert),delprint(ivert),deltrint(ivert),ier)
cmar_ass_cond_rpt
      err(ivert) = ier
cc	if(flag_ass_cond(ivert)) then


cc    
      if(flag_ass_cond_s) then
      tot_cons(ivert+1)=TOT_CONS(IVERT)
      comp_ratio(ivert+1)=COMP_RATIO_2

cmar_ass_cond_temp_rpt
	tmand_stat(ivert+1)=temp2_2
cmar_ass_cond_temp_rpt

	flow_stat(ivert)=f1
	flow_stat(ivert+1)=f2

      flowm(ivert)= flowm_1
	flowm(ivert+1)= flowm_2

CMAR_ASS_COND
      HEAD(IVERT)=H1
	HEAD(IVERT+1)=H2
CMAR_ASS_COND



      flow_ric(ivert)=flow_ric_1
	flow_ric(ivert+1)=flow_ric_2

	delpr1(ivert)=dp_in_1
	delpr2(ivert)=dp_out_1
	delpr1(ivert+1)=dp_in_2
	delpr2(ivert+1)=dp_out_2

cmar_ass_cond_rpt
	tot_cons(ivert)=tot_cons1
	tot_cons(ivert+1)=tot_cons2


cmar_ass_cond_rpt
cmar_ass_cond_rpt
      err(ivert+1) = ier

cmar_ass_cond_warning
	pmand_stat(ivert+1)=pmand_stat(ivert)
	pasp_stat(ivert+1)=pasp_stat(ivert)
cmar_ass_cond_warning

cmar_ass_cond_rpt

      endif



cmar_ass_cond_rpt
cc
!-se e' stato corretto il punto di lavoro, la simulazione e' da considerarsi
!-terminata con errore
      if (nit .eq. nitmax) err(ivert) = 1

      if (iopt_aux .and. ier.eq.0) then
c e' necessario aggiungere a tot_cons(ivert) il costo di accensione o
c spegnimento quando si calcolano i dati della configurazione ottima
c NOTA: la variabile stat e' definita solo se iopt_aux = .T.
        if (opt_time.ne.0) then
          iu = first_unit(ivert)
          do i=1,unit_num(ivert)
            call calc_cons_tot (status_old(i),stat(i),
     *           unit_start_cost(iu+i-1),unit_shut_cost(iu+i-1),
     *           opt_time,tot_cons(ivert))
          end do
        end if
      end if

!-ripristino del tipo di vertice, se il funzionamento e' f_serie
      if (stat_ord(ivert).eq.tipo_comp .and. iopt_aux) then
        vert_typ(ivert) = vert_typ_old
      end if

!-ripristino delle unita' dato in input
      do i = 1,unit_num(ivert)
        j = first_unit(ivert)+i-1
        status(j) = status_old(i)
      end do

!-ripristino delle caratteristiche topologiche della centrale
      if (lmf .and. iopt_aux .and. .not. iopt_mess) then
        stat_vars(3,ivert) = stat_vars_old(3)
        stat_vars(4,ivert) = stat_vars_old(4)
        stat_vars(5,ivert) = stat_vars_old(5)
        stat_vars(6,ivert) = stat_vars_old(6)
        stat_vars(7,ivert) = stat_vars_old(7)
      endif
c
  

      return
      end
c---------------------------------------------------------------------------------
      subroutine ottimo_cent()
c
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/ti.inc'
      include '../inc/flag.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/TYPES.INC'
      include '../inc/messag.inc'
      include '../inc/nw.inc'
c
      INTEGER*4 ier,pg,iserv,iu,i,ii,j,ker
      integer*4 iserv_app,istaz_app(max_serv),pg_app(max_serv)
      integer*4 ncorr
c      REAL*4    tot_cons_min
      character*(tit_len) msg_err
!------------------------------------------------------------
!-inizializzazioni
      ier = 0
      msg_err = ' '




      call calc_ottimo (ier)
cmar_asss_cond
ccccccccccccccccccccccccccccccccccccccccccc      num_servizi=1
cmar_ass_cond
      if (ier .eq. 0) then
! caso di centrale ok
        do iserv_app = 1,num_servizi
          err(i_vert(iserv_app)) = 0
          fl_ris_ce(i_vert(iserv_app)) = 0
        enddo
      else  ! caso di errore
! inizializzazione che serve in caso di non recupero
        do iserv_app = 1,num_servizi
cgpe-corr-staz
c         if (.not.fl_sim .and. set_flag(i_vert(iserv_app)) .eq. 1) then
         if (set_flag(i_vert(iserv_app)) .eq. 1) then
cgpe-corr-staz-end
          err(i_vert(iserv_app)) = ier
c caso di centrale ko (eventualmente modificabile in caso di recupero ok)
          fl_ris_ce(i_vert(iserv_app)) = -1
cgpe-corr-staz
         endif
cgpe-corr-staz-end
        enddo

c-staz-corr
c-staz-corr        if (fl_cor) then
        if (fl_cor .and. try_rec) then
c-staz-corr-end
! viene richiesta la correzione del punto di lavoro della centrale
          ier = 0
          iserv_app = 1
	    if (num_servizi.eq.1) then
            istaz_app(1) = i_staz(iserv_app)
            pg_app(1) = i_vert(iserv_app)
            istaz_app(2) = I_UND
            pg_app(2) = 0
	    else
            do iserv_app = 1,max_serv

              istaz_app(iserv_app) = i_staz(iserv_app)
              pg_app(iserv_app) = i_vert(iserv_app)
            enddo
          endif

          ncorr = 60
          call calc_corr (istaz_app,pg_app,ncorr,ier)

          do iserv_app = 1,num_servizi
            err(pg_app(iserv_app)) = ier
            if (ier .eq. 0) then
c caso di centrale ok con il recupero
              fl_ris_ce(pg_app(iserv_app)) = 1
            endif
          enddo
        endif ! if (fl_cor)

ctrap        if (lcent_last) then
        if (ier .ne. 0) then
        if (lcent_last) then
ctrap-end
! può derivare dal caso in cui la calc_ottimo sia andata in errore e non si recupera
! o nel caso di recupero andato in errore
            ier = 0
            iserv_app = 1
	      if (num_servizi.eq.1) then
              istaz_app(1) = i_staz(iserv_app)
              pg_app(1) = i_vert(iserv_app)
              istaz_app(2) = I_UND
              pg_app(2) = 0
	      else
              do iserv_app = 1,max_serv
                istaz_app(iserv_app) = i_staz(iserv_app)
                pg_app(iserv_app) = i_vert(iserv_app)
              enddo
            endif
            call calc_ottimo_ko (istaz_app,pg_app,ier)
          goto 9998
ctrap
        else
          goto 9998
ctrap-end
        endif
        endif ! lcent_last

      endif

!-determinazione della configurazione delle unita' relativa allo stato ottimo
!-calcolo dei dati delle unita' attive per la loro visualizzazione
      call simula_cent ()

      return
c
9998  continue

      return ! OTTIMO_CENT
      END
c--------------------------------------------------------------------------------
c
      Subroutine calc_ottimo (ier)
*********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/ti.inc'
      include '../inc/UNITS.INC'
      include '../inc/TYPES.INC'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
c
c      REAL*4    tot_cons_min  ! O) consumo minimo totale
c     *         ,cons_ass(*)   ! O) consumo di ciascun assetto che rende minimo
c                              !    il consumo globale
      INTEGER*4 ier           ! O) indice di errore

      INTEGER*4 pg,iserv,unit_sim,iu,i,ii,j
c-------------------------------------------------------------------------
!-inizializzazioni
      ier = 0
c
c Lettura della matrice tab_grid e dei valori di dhs e dqs e lettura dei
c valori di start_cost, shut_cost, opt_time
c Per costruire la griglia tab_grid, nel caso di una centrale multifunzione,
c sara' necessario costruire una griglia per ogni tipo di funzionamento
c
      if (.not.lmf) then
        iserv = 1
        pg = pgrato(i_staz(iserv))
        unit_sim = unit_num(pg)

!-centrale non multifunzione
        call optcent(i_staz(iserv),vert_typ(pg),tipo_criterio(pg),
     *pg,flow_stat(pg),pasp_stat(pg),pmand_stat(pg),
cgpe     *pg,flow_stat(pg),pasp_stat_dp(pg),pmand_stat_dp(pg),
     *delpr1(pg),delpr2(pg),
cgpe-end
     *tasp_stat(pg),TARIA(pg),unit_sim,stat_ord(pg),first_type(pg),
     *type_num(pg),first_unit(pg),type_quant(first_type(pg)),
     *type_avail(first_type(pg)),unit_avail(first_unit(pg)),
     *status(first_unit(pg)),
     *stat_vars(1,pg),unit_start_cost(first_unit(pg)),
     *unit_shut_cost(first_unit(pg)),tot_cons(pg),ier)
      else

!-centrale multifunzione
       call optcent_mf(ier)
      end if
c
c      if (.not.lmf) then
c        tot_cons_min = tot_cons(pg)
c      endif
c
      return
      end ! calc_ottimo
c---------------------------------------------------------------------------------
      Subroutine save_cin (istaz,jstaz,pasp_save,pmand_save,
     *           flow_save)
c
c   Salvataggio delle condizioni iniziali
c   
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'

      INTEGER*4 istaz,jstaz         ! I)
      REAL*4    pasp_save(*)        ! O)
     *         ,pmand_save(*)       ! O)
     *         ,flow_save(*)        ! O)

      INTEGER*4 pg
!------------------------------------------------------------
!-centrale istaz
      pg = pgrato(istaz)
      tot_cons(pg) = 0.
      pasp_save(1)  = pasp_stat(pg)
      pmand_save(1) = pmand_stat(pg)
      flow_save(1)  = flow_stat(pg)
!-eventuale centrale jstaz
      if (jstaz .ne. i_und) then
        pg = pgrato(jstaz)
        tot_cons(pg) = 0.
        pasp_save(2)  = pasp_stat(pg)
        pmand_save(2) = pmand_stat(pg)
        flow_save(2)  = flow_stat(pg)
      endif
c
      return
      END ! save_cin
c-------------------------------------------------------------------------------
c
      Subroutine restore_cin (istaz,jstaz,pasp_save,pmand_save,
     *           flow_save)
c********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'

      INTEGER*4 istaz               ! I)
     *         ,jstaz               ! I)
      REAL*4    pasp_save(*)        ! I)
     *         ,pmand_save(*)       ! I)
     *         ,flow_save(*)        ! I)

      INTEGER*4 pg
!------------------------------------------------------------
!-restore delle condizioni iniziali date in input
!-centrale istaz
      pg = pgrato(istaz)
      pasp_stat(pg) = pasp_save(i_serv(pg))
      pmand_stat(pg) = pmand_save(i_serv(pg))
      flow_stat(pg) = flow_save(i_serv(pg))

      if (jstaz .ne. i_und) then
        pg = pgrato(jstaz)
        pasp_stat(pg) = pasp_save(i_serv(pg))
        pmand_stat(pg) = pmand_save(i_serv(pg))
        flow_stat(pg) = flow_save(i_serv(pg))
      endif
c
      return
      END ! restore_cin
c
c------------------------------------------------------------------------------------
c
      Subroutine find_ce_ass (istaz,jstaz)
c************************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ti.inc'
      include '../inc/top_app_ele.inc'

      INTEGER*4 istaz       ! I)
      INTEGER*4 jstaz       ! O) off_set centrale associata a istaz

      INTEGER*4 iel,i_elem,ii_elem,j
      logical*2 ce_ass
!------------------------------------------------------------
      i_elem = pgrato(istaz)
      if (assetto_cm(i_elem) .gt. 0) then
!-ricerca dell'eventuale  centrale proveniente dalla stessa MF
        do iel = otipti(e_ce), etipti(e_ce)
          ii_elem = pgrato(iel)
          if (iel.ne.istaz .and.               ! iel non coincide con istaz
     *        assetto_cm(ii_elem) .gt. 0 .and. ! iel deriva da una MF
     *        ce_ass (istaz,iel) ) then        ! entrambe provengono dalla stessa CMF
            jstaz = iel
            goto 11
          end if
        enddo
      end if

11    continue

      return
      END ! find_ce_ass
c-------------------------------------------------------------------------------
      logical*2 function ce_ass (istaz,jstaz)
********************************************************************************
!     function ce_ass = .true. se le centrali i_elem ed ii_elem provengono
!     dalla stessa centrale multifunzione

      implicit none
      include '../inc/param.inc'        !parametri
      include '../inc/TI.INC'  !top AGGIUNTIVE per gli elementi

      integer*4 istaz,jstaz

      ce_ass = (elem_id(istaz)  .eq.   ! puntatore centrale multifunzione
     -           elem_id(jstaz))
      return
      end ! function ce_ass
c-------------------------------------------------------------------------------------
      logical*2 function ce_off (pres1,pres2)

      implicit none
      include '../inc/param.inc'   !parametri
cgpe      include '../inc/ti.inc'
      include '../inc/stazione.inc'       !parametri centrali
c      include '../inc/stations.inc'

      real*4 pres1,pres2     ! I)

      ce_off = .false.

      if (abs(pres1-pres2) .le. dp_off) then
        ce_off = .true.
      endif
c
      return
      end
c-------------------------------------------------------------------------------------
      Subroutine init_ce_off (istaz)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/UNITS.INC'
      include '../inc/CMF_ASSETTI.INC'

      INTEGER*4 istaz       ! I)
      integer*4 i,iu,ivert
!------------------------------------------------------------
      ivert = pgrato(istaz)
      fl_simu(ivert) = .true.
      iu = first_unit(ivert)
      do i = 1,unit_num(ivert)
        l_act(i+iu-1) = .false.
      enddo
c
      return
      END ! init_ce_off
c--------------------------------------------------------------------------------
      logical*2 function ce_avail (ivert)
c
      implicit none
      include '../inc/param.inc'     !parametri
      include '../inc/ti.inc'        !top connessioni
      include '../inc/stations.inc'  !centrali
      include '../inc/units.inc'     !unita' centrali

      integer*4 ivert      ! I)
      integer*4 i,iu
!------------------------------------------------------------
      ce_avail = .false.

!-una centrale viene definita disponibile se ha almeno una unità disponibile
      iu = first_unit(ivert)-1
      do i = 1, unit_num(ivert)
        if (unit_avail(iu+i) .ge. 1) then
          ce_avail = .true.
          return
        endif
      enddo

      return
      end ! ce_avail
c--------------------------------------------------------------------------------
      Subroutine init_ce_nd (istaz)
c
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/stations.inc'
      include '../inc/UNITS.INC'
      include '../inc/CMF_ASSETTI.INC'

      INTEGER*4 istaz       ! I)
      integer*4 i,iu,ia,ivert
!------------------------------------------------------------
      ivert = pgrato(istaz)
      fl_simu(ivert) = .true.
      iu = first_unit(ivert)
      do i = 1,unit_num(ivert)
        l_act(i+iu-1) = .false.
      enddo
c
      return
      END ! init_ce_nd
c---------------------------------------------------------------------------------
      subroutine calc_cons_tot (stat_in,stat_out,
     *           start_cost,shut_cost,opt_time,consumo)
!
!  Aggiunge al consumo totale della simulazione di centrale l'eventuale costo
!  di accensione/spegnimento di una unita' che viene accesa o spenta
!  
      implicit none
      include '../inc/param.inc'      !parametri
      include '../inc/stazione.inc'   !parametri di centrale
      include '../inc/RK_PARAM.INC'

      CHARACTER*3 stat_in        ! I) stato dell'unita' da cin
      CHARACTER*3 stat_out       ! I) stato dell'unita' determinato dall'ottimo
      real*4      start_cost     ! I) costo di accensione
      real*4      shut_cost      ! I) costo di spegnimento
      real*4      opt_time       ! I) durata dell'ottimizzazione
      real*4      consumo        ! I/O
!------------------------------------------------------------
      if (opt_time.ne.0) then
        if((stat_in.eq.funz_on).and.(stat_out.eq.funz_off))then
!          consumo = consumo + (shut_cost/QRIF)/opt_time
          consumo = consumo + (shut_cost/opt_time)
        else if ((stat_in.eq.funz_off).and.(stat_out.eq.funz_on)) then
!          consumo = consumo + (start_cost/QRIF)/opt_time
          consumo = consumo + (start_cost/opt_time)
        end if
      end if
c
      return
      end !calc_cons_tot
c--------------------------------------------------------------------------
      Subroutine find_ce_serie (i_el,ii_el,ivert_fase1,ivert_fase2)
c
      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/stazione.inc'             !top connessioni
      include '../inc/ti.inc'             !top connessioni
      include '../inc/cap_bon_par.inc'       !parametri centrali
      include '../inc/cap_bon_prv.inc'       !parametri centrali

      integer*4 i_el,
     *          ii_el,         ! settato a I_UND
     *          ivert_fase1,   ! puntatore prima fase
     *          ivert_fase2    ! puntatore seconda fase

      ii_el = I_UND
      ivert_fase1 = I_UND
      ivert_fase2 = I_UND
      if (tipo_fase(pgrato(i_el)) .eq. fase_1 .and.
     *    tipo_fase(pgrato_app(i_el)) .eq. fase_2  ) then
        ivert_fase1 = pgrato(i_el)
        ivert_fase2 = pgrato_app(i_el)
      elseif (tipo_fase(pgrato_app(i_el)) .eq. fase_1 .and.
     *        tipo_fase(pgrato(i_el)) .eq. fase_2          ) then
        ivert_fase1 = pgrato_app(i_el)
        ivert_fase2 = pgrato(i_el)
      endif
c
      return
      end ! find_ce_serie
c-------------------------------------------------------------------------------
c
        integer*4 function itop_from_pgrato (ivert,v_pgrato)
c----------------------------------------------------------------
c       Questa fuction restituisce l'indice dell'elemento
c       topologico dato l'indice del pgrato
c----------------------------------------------------------------
        implicit none
        INCLUDE '../inc/param.inc'             !:costante

        INTEGER*4 ivert
        INTEGER*4 v_pgrato(*)

        INTEGER*4 istaz
!------------
        istaz = 1
        do while (v_pgrato(istaz).ne.ivert)
          istaz = istaz+1
        enddo
        itop_from_pgrato = istaz
        end
c
c-----------------------------------------------------------------------------------
      Subroutine ce_s_ko (ivert,tipo_fase)
c
c Inizializza i risultati ko di una centrale in serie
c
      implicit none
      include '../inc/param.inc'      !parametri
      include '../inc/stazione.inc'   !parametri di centrale
      include '../inc/stations.INC'
      include '../inc/UNITS.INC'
      include '../inc/MF_UNITS.INC'
      include '../inc/cmf_assetti.INC'
      include '../inc/argo_reale_ris.INC'
      include '../inc/cap_bon.INC'
c
      integer*4 ivert       ! I)
      INTEGER*4 tipo_fase   ! I)
      integer*4 i,iu
!------------------------------------------------------------
      if (tipo_fase .eq. fase_1) then
        tmand_stat(IVERT) = t_int
        pmand_stat(IVERT) = p_int
      else
        pasp_stat(IVERT) = p_int
        tasp_stat(IVERT) = t_int
        pmand_stat(IVERT) = pmand_fase2
        tmand_stat(IVERT) = tmand_fase2
      endif
      comp_ratio(IVERT) = 0.
      tot_cons(IVERT)   = 0.
      head(IVERT)       = 0.
      flowm(IVERT)      = 0.
      flow_ric(IVERT)   = 0.
      iu = first_unit(ivert)
      do i = 1, unit_num(ivert)
        status(iu+i-1) = funz_off
        l_act(iu+i-1) = .false.
        unit_rev(iu+i-1) = 0.
        unit_flow(iu+i-1) = 0.
        power(iu+i-1) = 0.
        POWER_PERC(iu+i-1) = 0.
c-bp05
        POWER_PERC_VAR(iu+i-1) = 0.
c-bp05-end
        if (unit_bifase(iu+i-1)) then
          unit2_rev(iu+i-1) = 0.
          unit2_flow(iu+i-1) = 0.
        endif
      enddo
c
      return
      end   !ce_s_ko
c-------------------------------------------------------------------------------
      Subroutine save_cin_s (ivert)
c
c   Salvataggio dati intermedi per la centrale in serie
c
      implicit none
      include '../inc/param.inc'
      include '../inc/stations.inc'
      include '../inc/cap_bon.INC'

      INTEGER*4 ivert               ! I)
!------------------------------------------------------------
      p_int = pmand_stat(ivert)
      t_int = tmand_stat(ivert)
c
      return
      END ! save_cin_s
c----------------------------------------------------------------------------------
      subroutine ottimo_cent_serie (istaz)
C**********************************************************************************
C   Modulo per l'ottimizzazione della centrale in serie:
C   - ricerca dell'intervallo entro il quale operare la ricerca dello zero 
C     della funzione consumo
C   - chiamata della subroutine calc_ottimo_serie
C   - in caso di errore, nell'argo manuale viene operata la correzione del punto
C     di lavoro della centrale. Se questa correzione non è fattibile, si esce
C     con errore
C*********************************************************************************
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.inc'
      include '../inc/ti.inc'
      include '../inc/flag.inc'
      include '../inc/cap_bon.inc'
      include '../inc/cap_bon_prv.inc'
      include '../inc/stations.inc'
      include '../inc/cmf_assetti.inc'
      include '../inc/TYPES.INC'
      include '../inc/messag.inc'
      include '../inc/nw.inc'
c
      INTEGER*4 istaz     ! I)

      INTEGER*4 jstaz,ivert,jvert,ier,ker
      integer*4 iserv_app,istaz_app(max_serv),pg_app(max_serv)
      INTEGER*4 ncorr
      character*(tit_len) msg_err
!------------------------------------------------------------
!-inizializzazioni
      ier = 0
      msg_err = ' '

      call find_ce_serie (istaz,jstaz,ivert,jvert)

      iserv_app = 1
      istaz_app(iserv_app) = istaz
      pg_app(iserv_app) = ivert
      iserv_app = 2
      istaz_app(iserv_app) = jstaz
      pg_app(iserv_app) = jvert

      call calc_ottimo_serie (istaz,jstaz,ivert,jvert,ier)

      if (ier .eq. 0) then
! caso di centrale ok
        do iserv_app = 1, max_serv
          err(pg_app(iserv_app)) = 0
          fl_ris_ce(pg_app(iserv_app)) = 0
        enddo
      else
! in caso di errore (ier.ne.0)
! inizializzazione che serve in caso di non recupero
        do iserv_app = 1, max_serv
          err(pg_app(iserv_app)) = ier
c caso di centrale ko (eventualmente modificabile in caso di recupero ok)
          fl_ris_ce(pg_app(iserv_app)) = -1
        enddo

c-staz-corr
c-staz-corr        if (fl_cor) then
        if (fl_cor .and. try_rec) then
c-staz-corr-end
! viene richiesta la correzione del punto di lavoro della centrale
          ier = 0

!-viene chiamata la routine per correggere il punto di lavoro ottimo
          ncorr = 50
          call calc_corr (istaz_app,pg_app,ncorr,ier)

          do iserv_app = 1, max_serv
            err(pg_app(iserv_app)) = ier
	      if (ier .eq. 0) then
c caso di centrale ok con il recupero
              fl_ris_ce(pg_app(iserv_app)) = 1
            endif
          enddo
        endif  ! if (fl_cor)

ctrap        if (lcent_last) then
        if (ier .ne. 0) then
        if (lcent_last) then
ctrap-end
!-in caso di errore (può derivare dal caso in cui la calc_ottimo_serie sia uscita
! con errore e non si recupera o nel caso di recupero andato in errore),
! inizializza i dati della centrale in serie
          do iserv_app = 1, max_serv
            call ce_s_ko(pg_app(iserv_app),tipo_fase(pg_app(iserv_app)))
	    enddo
          ier = 0
          call calc_ottimo_ko (istaz_app,pg_app,ier)
          goto 9998
ctrap
        else
          goto 9998
ctrap-end
        endif
        endif ! lcent_last
      endif
c
      return
c
9998  continue
c
      return ! ottimo_cent_serie
      END
c-------------------------------------------------------------------------------
      logical*2 function ce_accesa (i_elem)
!
!    Una centrale viene definita necessariamente accesa se ha almeno
!    una unita' necessariamente accesa.
!
      implicit none
      include '../inc/param.inc'         !parametri
      include '../inc/ti.inc'            !top connessioni
      include '../inc/stations.inc'      !centrali
      include '../inc/units.inc'         !unita' centrali

      integer*4 i_elem   ! I) off_set centrale tra pgrato(otipti(e_ce)),pgrato(etipti(e_ce))

      integer*4 i,iu
!------------------------------------------------------------
      ce_accesa = .false.
      iu = first_unit(i_elem)-1
      do i = 1, unit_num(i_elem)
        if (unit_avail(iu+i) .gt. 1) then
          ce_accesa = .true.
          return
        endif
      enddo
c
      return
      end ! ce_accesa

      real*4 function soglia_vio_vinc(ivert)

      implicit none
      include '../inc/param.inc'         !parametri
      include '../inc/ti.inc'            !top connessioni
      include '../inc/flag.inc'
      include '../inc/stations.inc'      !centrali
      include '../inc/stazione.inc'      !centrali
      include '../inc/units.inc'         !unita' centrali
      include '../inc/mf_units.inc'      !unita' centrali

      integer*4 ivert   ! I) off_set centrale tra pgrato(otipti(e_ce)),pgrato(etipti(e_ce))

      integer*4 i,iu
!------------------------------------------------------------
      soglia_vio_vinc = 0
      iu = first_unit(ivert)-1
      do i = 1, unit_num(ivert)
cgpe-da verificare se lstatus e' sempre settato
        if (lstatus(iu+i) .eq. ON) then
cgpe-da verificare se lstatus e' sempre settato-end
          if (unit_vinc(iu+i) .eq. min_pw .or.
     *        unit_vinc(iu+i) .eq. max_pw .or.
     *        unit_vinc(iu+i) .eq. max_re .or.
     *        unit_vinc(iu+i) .eq. (max_pw + max_re) .or.
     *        unit_vinc(iu+i) .eq. (min_pw + max_re)     ) then
            soglia_vio_vinc = soglia_cons
            return
          endif
        endif
      enddo
c
      return
      end ! soglia_vio_vinc
