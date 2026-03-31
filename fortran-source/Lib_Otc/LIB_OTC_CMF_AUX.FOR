
        Subroutine opt_cent_mf(istaz,vert_type,tipo_criterio,
     *      vert_pointer,nserv,l_inv,unit_num,type_num,jufir,jtfir,
     *      type_quant_opt,type_avail_opt,unit_avail_opt,
cgpe     *      flow2,pres1,pres2,temp1,air,stat_ord,status,
cgpe-new
     *      set_flag_mf,
cgpe-new-end
     *      flow2,pres1,pres2,delpr1,delpr2,temp1,air,stat_ord,status,
     *      stat_vars,start_cost,shut_cost,ier)
c
c  ottimizzazione di una stazione  multifunzione a cui vengono assegnati
c  gli assetti s1 ed s2, con ricerca della configurazione a minor costo:
c  restituisce l'indice dello stato ottimo, con associato lo stato delle
c  unita'per ciascun servizio 
c----------------------------------------------------------------------
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/RK_PARAM.INC'
	include '../inc/SIMPSTAT.INC'
	include '../inc/TYPES.INC'
c
      include '../inc/OTC.INC'
      include '../inc/OTC_TIME.INC'
      include '../inc/FLAG.INC'
	include '../inc/CMF_ASSETTI.INC'
c
c       istaz        I  indice della stazione nella struttura dati SIRE
c                       completa (ind=oiwato(j)): va messo nel common
c                       stazione.inc nella variabile ind.
c       iopt         I  indice di ottimizzazione (iopt=.T. ottimizzazione)
c       vert_type    I  tipo di stazione (1=semplice, 2=composta, 3=mista)
c       vert_pointer I  indice della stazione nella struttura interna di
c                       dati di stazione
c       unit_num     I  numero totale di macchine (effettive) della stazione
c       jtfir        I  puntatore al primo tipo della stazione
c       type_num     I  numero di tipi (effettivi) della stazione
c       jufir        I  puntatore alla prima unita' della stazione
c       stat_vars    I  variabili topologiche di stazione
c       start_cost   I  consumo dovuto all'accensione di una unita'
c       shut_cost    I  consumo dovuto allo spegnimento di una unita'
c       opt_time     I  tempo di ottimizzazione
c       nserv        I  numero degli assetti da ottimizzare
c
c Per ciascun servizio:
c       flow        I/O portata entrante in stazione
c       flow2        I  portata uscente (lavorata dalla stazione)
c       pres1        I  pressione di aspirazione
c       pres2        I  pressione di mandata
c       temp1        I  temperatura di aspirazione
c       temp2        O  temperatura di mandata
c       air          I  temperatura dell'aria
c       delpr1       I  perdita di pressione effettiva in ingresso
c       delpr2       I  perdita di pressione effettiva in uscita
c       comp_ratio   O  rapporto di compressione effettivo
c       consumo    I/O consumo della stazione
c       flowm        O  portata di macchina totale della stazione
c       flow_ric     O  portata normal condition totale della stazione
c                       (con eventuale riciclo)
c       head         O  adiabatic head della stazione
c       stat_ord     I  tipo di funzionamento della stazione, se di tipo
c                       composto

c nn    nit          I  numero di iterazione (per i messaggi)
c nn    nitmax       I  numero di iterazione massimo richiesto dall'esterno
c nn    tipo_corr    I  tipo di correzione da apportare nei casi estremi
c                       (se il set e' in pressione e la correzione e'
c                        in portata, oppure se il set e' in portata e la
c                        correzione e' in pressione)
c                        1=correzione
c                        0=nessuna correzione
c       ind_min         indice dell'assetto con il numero minore di unita'
c       ind_max         indice dell'assetto con il numero maggiore di unita'
c       jtest           numero di test trovati per ciascun assetto           
c       ntest           numero di simulazioni con ier = 0
c       ndisp           nel caso di un solo assetto, e' il numero minimo
c                       di unita' attive 
c       cost         O  costo della simulazione
c       ind_s        O  ind_s(max_ch,maxunits): per il test j, ind_s(j,k)
c                       assume i valori:
c                         0 se l'unita' k-esima non e' attiva
c                         ind_min se l'unita' k-esima e' attiva per il servizio ind_min
c                         ind_max se l'unita' k-esima e' attiva per il servizio ind_max
c       ind_opt      O  indice della simulazione a minor costo
c       ier          O  error index
c
c       status       I  stato (ON/OFF) delle unita' della centrale (units.inc)
c       lstat           variabile logica (T/F) per le unita' da simulare
c       stat            stato (ON/OFF) per le unita' da simulare (simpstat.inc)
c
c       unit_avail_opt(i,j)  I disponibilita' dell'unita' i per il servizio j
c       type_avail_opt(i,j)  I disponibilita' delle unita' per tipo per il
c                                servizio j
c       

      INTEGER*4 istaz(*),vert_type(*),tipo_criterio,vert_pointer(*),
     *          nserv
      logical*2 l_inv
      INTEGER*4 unit_num,type_num,jtfir(*),jufir(*)

      INTEGER*4 type_quant_opt(maxunits,max_serv)
      INTEGER*4 unit_avail_opt(maxunits,max_serv),
     *          type_avail_opt(maxunits,max_serv)

      REAL*4    flow2(*),pres1(*),pres2(*),temp1(*),
     *          air(*),stat_vars(12,max_serv)
      CHARACTER*3 status(maxunits,max_serv)
      INTEGER*4 stat_ord(*),set_flag_mf(*)


      REAL*4    start_cost(maxunits,max_serv),
     *          shut_cost(maxunits,max_serv)

      REAL*4    consumo(max_ch)
      INTEGER*4 ier
c
      REAL*4  flow(max_serv),temp2(max_serv),delpr1(max_serv),
     *        delpr2(max_serv),comp_ratio(max_serv),flowm(max_serv),
     *        flow_ric(max_serv),head(max_serv),pres_int(max_serv),
     *        temp_int(max_serv),delprint(max_serv),deltrint(max_serv)

      INTEGER*4 jtest(max_serv)
      INTEGER*4 ind_err(max_serv)
      INTEGER*4 ntest,ntest_old,ind_ntest
      INTEGER*4 num
      LOGICAL*2 lstat(maxunits)
      INTEGER*4 nact_min,nact_max,ind_min,ind_max
      INTEGER*4 j_ind  ! indice del servizio che stiamo simulando
      INTEGER*4 nact
      INTEGER*4 type_act_opt(maxunits)

      INTEGER*4 I,I1,J,K,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12
      REAL*4    QQ,PIN,POUT
      INTEGER*4 INUM

      REAL*4    cons_ass(max_serv,max_ch),diff_cons,diff_cons_aux
      REAL*4    cost(max_serv,max_ch)
      LOGICAL*2 lact_min(max_ch,maxunits)
      LOGICAL*2 lact_max(max_ch,maxunits)

      real*4    idummy
cgpe      LOGICAL*2 ce_off
c-----------------------------------------------------------------------
cgpe      idummy = dhs            ! serve per evitare %FORT-I-VARUNUSED
cgpe      idummy = dqs            ! serve per evitare %FORT-I-VARUNUSED
cgpe      idummy = tab_grid(1,1)  ! serve per evitare %FORT-I-VARUNUSED





c
      ier = 0
      do j=1,max_serv
        ind_err(j) = 0
        jtest(j) = 0
      end do
      ntest = 0
      ind_opt = 0
cgpe      ntest = 1
cgpe      ind_opt = 1
cgpe      consumo(ntest) = R_MAX
      diff_cons = R_MAX
c
      if (.not.l_inv) then
        ind_min = 1
        ind_max = 2
      else
        if (nserv.eq.1) then
          ind_min = 1
        else
          ind_min = 2
          ind_max = 1
        end if
      end if
c
c---->  simulazione del servizio con il numero minore di unita' disponibili
      nact = 0

      j_ind = ind_min
      ind_serv = j_ind



		

c calcolo del numero di unita' che devono essere necessariamente accese per il
c servizio j_ind = ind_min
      i1=0
      num = 0
	
      do j=1,type_num
        type_act_opt(j) = 0
        do k=1,type_quant_opt(j,j_ind)

    

          if (unit_avail_opt(k+i1,j_ind).gt.1) then
            num = num+1
            type_act_opt(j) = type_act_opt(j)+1

       
          end if
        end do
        i1 = i1+type_quant_opt(j,j_ind)
      


      end do

c
C---->              riconoscimento di una centrale spenta
cgpe-staz      if (set_flag_mf(j_ind) .eq. 0) then
      if (l_simu(j_ind) .eq. 0) then
cgpe-staz-end
cgpe        if (num .eq. 0) then
!-non esistono unita' necessariamente accese
          jtest(j_ind) = 1
          do i = 1,unit_num
            lact_min(jtest(j_ind),i) = 0
          enddo
          cost(j_ind,jtest(j_ind)) = 0.
!-alla label 10:
! - se c'e' un solo assetto attivo (nserv=1), si calcola lo stato ottimo
!   'globale' della MF e poi si va a 11
! - se ci sono due assetti attivi (nserv=2), si procede con il calcolo
!   delle possibili configurazioni per l'assetto ind_max
          goto 10
cgpe        else
cgpe!-ERRORE: la centrale e' spenta ed esistono unita' necessariamente accese
cgpe!         per il servizio ind_min
cgpe! - se c'e' un solo assetto attivo (nserv=1), si va alla label 11
cgpe! - se ci sono due assetti attivi (nserv=2), non si procede con il calcolo
cgpe!   delle possibili configurazioni per l'assetto ind_max, perche' un errore
cgpe!   nell'ottimizzazione di un assetto comporta un errore nell'ottimizzazione
cgpe!   della centrale multifunzione
cgpe!          ntest = 0
cgpe          ntest = 1
cgpe          goto 11
cgpe        endif
      endif
c
c Nell'ottimizzazione dei servizi contemporanei, e' necessario che una unita'
c comune ai due assetti, necessariamente accesa per un assetto, non deve
c risultare disponibile per l'altro assetto
c---->              devo generare tutte le possibili configurazioni
c
c calcolo del numero minimo di unita' da accendere
c---->      grid verification
      

      ind = istaz(j_ind)
cgpe      call init_stvar(istaz(j_ind),vert_pointer(j_ind))

!      qq = flow2(j_ind)*flow2(j_ind)
!      delpr1(j_ind) = stat_vars(3,j_ind)*qq
!      delpr2(j_ind) = stat_vars(4,j_ind)*qq
!      pin  = pres1(j_ind)-delpr1(j_ind)
!      pout = pres2(j_ind)+delpr2(j_ind)
!      exp_pol=cesad(ind)
!      esp = (exp_pol-1.)/exp_pol
!      z = prop(pin,temp1(j_ind),z_coef)
!      comp_ratio(j_ind) = pout/pin
!      aux1 = (erre / agrvcc / pmol) * Z * temp1(j_ind)/esp
!      aux2 = comp_ratio(j_ind)**esp
!      head(j_ind) = aux1 * (aux2-1.)
!      aux3 = ROCN*(1/1.e5)*(erre/pmol)*z*temp1(j_ind)/pin
!      flow_tot = flow2(j_ind)*aux3

c per ora pongo numin=1, da sostituire con il numero minimo
c effettivo di unita' da accendere quando sara' creata la griglia
c tab_grid
c      call ver_grid(flow_tot,head(j_ind),dqs,dhs,type_num,numin,
c     *tab_grid,nhmax,nqmax,ind_err(j_ind))

      numin = 1

      if (ind_err(j_ind).ne.0) then
        ier = 1 ! ier = 11
        return
      end if
c---->                        simulation with minimal configuration
      if (num.ge.numin) then
!-check sul numero di test OK
       if (jtest(j_ind).lt.max_ch) then
        call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
!        call genera_stat(tipo_criterio,vert_type(j_ind),unit_num,
!     *type_num,type_act(jtfir(j_ind)),type_act_opt,
!     *type_quant_opt(1,j_ind),unit_ind(jufir(j_ind)),
!     *lstat,stat)
        call genera_stat(tipo_criterio,vert_type(j_ind),unit_num,
     *type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
c
        call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
c se ind_err(j_ind)=0, i risultati della simulazione devono essere memorizzati
c nelle variabili cost(j_ind) e lact(j_ind,maxunits)
        if (ind_err(j_ind).eq.0) then
c la variabile logica lact_min indica se le unita' sono attive o meno
          call new_opt_mf(cost,start_cost(1,j_ind),shut_cost(1,j_ind),
     *opt_time,unit_num,lact_min,lstat,status(1,j_ind),jtest,j_ind)
c calcolo il numero delle unita'attive, se esiste un solo servizio da
c ottimizzare
          if (nserv.eq.1) then
            nact = num
          end if
        else
c Controllo sugli errori che derivano dalla fattibilita' dei punti sulle fasi
          call gest_errore(istaz(j_ind),ind_err(j_ind),tipo_criterio,
     *vert_type(j_ind),stat_ord(j_ind),ier)

c Se viene restituito ier non nullo, si esce dall'ottimizzazione.
          if (ier.gt.0) then
C            ier = 1 ! ier = 11
            return
          end if
        end if
       end if
      end if

c-dinamica
      if (fl_din) then
c viene provata solo la configurazione con i turbogruppi necessariamente accesi
        goto 10
      endif
c-dinamica-end
c---->              parte iterativa
c
c Nella parte iterativa, la variabile inum rappresenta il numero di unita'
c da aggiungere:
c - se num=0, la parte iterativa prova tutte le possibili configurazioni
c - se num>0, inum rappresenta il numero di unita' da aggiungere, oltre
c   quelle per cui unit_avail_opt e' uguale a due
c
      do inum=1,12
       do k1=1,type_num
        type_act_opt(k1) = type_act_opt(k1)+1
        if(type_avail_opt(k1,j_ind).ge.type_act_opt(k1)) then
         if(inum.gt.1) then
c---->    +++  111 -----> piu' di un compressore inserito
          do k2=k1,type_num
           type_act_opt(k2) = type_act_opt(k2)+1
           if(type_avail_opt(k2,j_ind).ge.type_act_opt(k2)) then
            if(inum.gt.2) then
c---->    +++  222 ----> piu' di due compressori da inserire
             do k3=k2,type_num
              type_act_opt(k3) = type_act_opt(k3)+1
              if(type_avail_opt(k3,j_ind).ge.type_act_opt(k3)) then
               if(inum.gt.3) then
c---->    +++  333 ---->  piu' di tre compressori da inserire
                do k4=k3,type_num
                 type_act_opt(k4) = type_act_opt(k4)+1
                 if(type_avail_opt(k4,j_ind).ge.type_act_opt(k4))then
                  if(inum.gt.4) then
c---->    +++  444 ---->  piu' di quattro compressori da inserire
                   do k5=k4,type_num
                    type_act_opt(k5) = type_act_opt(k5)+1
                    if(type_avail_opt(k5,j_ind).ge.
     *                 type_act_opt(k5)) then
                     if(inum.gt.5) then
c---->    +++  555 ---->  piu' di cinque compressori da inserire
                      do k6=k5,type_num
                       type_act_opt(k6) = type_act_opt(k6)+1
                       if(type_avail_opt(k6,j_ind).ge.
     *                    type_act_opt(k6))then
                        if(inum.gt.6) then
c---->    +++  666 ---->  piu' di sei compressori da inserire
                         do k7=k6,type_num
                          type_act_opt(k7)=type_act_opt(k7)+1
                          if(type_avail_opt(k7,j_ind).ge.
     *                       type_act_opt(k7)) then
                           if(inum.gt.7) then
c---->    +++  777 ---->  piu' di sette compressori da inserire
                            do k8=k7,type_num
                             type_act_opt(k8)=type_act_opt(k8)+1
                             if(type_avail_opt(k8,j_ind).ge.
     *                          type_act_opt(k8))then
                              if(inum.gt.8) then
c---->    +++  888 ---->  piu' di otto compressori da inserire
                               do k9=k8,type_num
                                type_act_opt(k9) =
     *                            type_act_opt(k9)+1
                                if(type_avail_opt(k9,j_ind).ge.
     *                             type_act_opt(k9)) then
                                 if(inum.gt.9) then
c---->    +++  999 ---->  piu' di nove compressori da inserire
                                  do k10=k9,type_num
                                   type_act_opt(k10) =
     *                               type_act_opt(k10)+1
                                   if(type_avail_opt(k10,j_ind).ge.
     *                                type_act_opt(k10)) then
                                    if(inum.gt.10) then
c---->    +++  10 ---->  piu' di dieci compressori da inserire
                                     do k11=k10,type_num
                                      type_act_opt(k11) =
     *                                  type_act_opt(k11)+1
                                      if(type_avail_opt(k11,j_ind)
     *                                    .ge.type_act_opt(k11)) then
                                       if(inum.gt.11) then
c---->    +++  11 ---->  piu' di undici compressori da inserire
                                        do k12=k11,type_num
                                         type_act_opt(k12)=
     *                                       type_act_opt(k12)+1 
                                         if(type_avail_opt(k12,j_ind)
     *                                      .ge.type_act_opt(k12)) then
c---->    +++  12 ---->  simulazione con dodici  compressori inseriti
                                          if((num+12).ge.numin) then
                                           if (jtest(j_ind).lt.max_ch)
     *                                                             then
                                            call init_vert_type
     *(vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                            call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
c quando viene chiamata la tinit_simpstat_mf, e' necessario che venga settata
c opportunamente la variabile unit_act
                                            call optim_vertex(
     *istaz(j_ind),iopt,vert_type(j_ind),tipo_criterio,
     *vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                            if(ind_err(j_ind).eq.0) then
                                             call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_min,lstat,status(1,j_ind),jtest,j_ind)
c calcolo il numero delle unita'attive nact se nserv=1 e nact=0
                                             if (nserv.eq.1) then
                                              if (nact.eq.0) then
                                               do i=1,type_num
                                                nact=nact+
     *                                               type_act_opt(i)
                                               end do
                                              end if
                                             end if

c Se (jtest(j_ind).eq.max_ch), non e' necessario uscire, in quanto tutte le
c variabili dimensionate a max_ch utilizzate sono relative al servizio con
c indice j_ind=ind_min, pertanto se jtest(j_ind)=max_ch (e dunque si sono
c trovate configurazioni per il servizio j_ind), si prosegue ottimizzando
c il servizio j_ind=ind_max
                                             if(jtest(j_ind).eq.max_ch)
     *                                        goto 10
                                            end if
                                           end if

                                          end if
                                         end if
                                         type_act_opt(k12) =
     *                                     type_act_opt(k12)-1 
                                        end do
                                       else
c---->                     simulazione con 11 compressori inseriti
                                        if((num+11).ge.numin) then
                                         if (jtest(j_ind).lt.max_ch)
     *                                                            then
                                          call init_vert_type(
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                          call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
                                          call optim_vertex(
     *istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                          if(ind_err(j_ind).eq.0) then
                                           call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_min,lstat,status(1,j_ind),jtest,j_ind)
                                           if (nserv.eq.1) then
                                            if (nact.eq.0) then
                                             do i=1,type_num
                                              nact=nact+type_act_opt(i)
                                             end do
                                            end if
                                           end if
                                           if (jtest(j_ind).eq.max_ch)
     *                                      goto 10
                                          end if
                                         end if

                                        end if
                                       end if
                                      end if
                                      type_act_opt(k11)=
     *                                  type_act_opt(k11)-1
                                     end do
                                    else
c---->                     simulazione con dieci compressori inseriti
                                     if((num+10).ge.numin) then
                                      if (jtest(j_ind).lt.max_ch) then
                                       call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                       call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
                                       call optim_vertex(istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                       if(ind_err(j_ind).eq.0) then
                                        call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_min,lstat,status(1,j_ind),jtest,j_ind)
                                        if (nserv.eq.1) then
                                         if (nact.eq.0) then
                                          do i=1,type_num
                                           nact=nact+type_act_opt(i)
                                          end do
                                         end if
                                        end if
                                        if (jtest(j_ind).eq.max_ch)
     *                                   goto 10
                                       end if
                                      end if

                                     end if
                                    end if
                                   end if
                                   type_act_opt(k10)=
     *                               type_act_opt(k10)-1
                                  end do
                                 else
c---->                     simulazione con nove compressori inseriti
                                  if((num+9).ge.numin) then
                                   if (jtest(j_ind).lt.max_ch) then
                                    call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                    call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                                    call optim_vertex(istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                    if(ind_err(j_ind).eq.0) then
                                     call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_min,lstat,status(1,j_ind),jtest,j_ind)
                                     if (nserv.eq.1) then
                                      if (nact.eq.0) then
                                       do i=1,type_num
                                        nact=nact+type_act_opt(i)
                                       end do
                                      end if
                                     end if
                                     if (jtest(j_ind).eq.max_ch) goto 10
                                    end if
                                   end if

                                  end if
                                 end if
                                end if
                                type_act_opt(k9) =
     *                            type_act_opt(k9)-1
                               end do
                              else
c---->                     simulazione con otto compressori inseriti
                               if((num+8).ge.numin) then
                                if (jtest(j_ind).lt.max_ch) then
                                 call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                 call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                                 call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                 if(ind_err(j_ind).eq.0) then
                                  call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_min,lstat,status(1,j_ind),jtest,j_ind)
                                  if (nserv.eq.1) then
                                   if (nact.eq.0) then
                                    do i=1,type_num
                                     nact=nact+type_act_opt(i)
                                    end do
                                   end if
                                  end if
                                  if (jtest(j_ind).eq.max_ch) goto 10
                                 end if
                                end if

                               end if
                              end if
                             end if
                             type_act_opt(k8) =
     *                         type_act_opt(k8)-1
                            end do
                           else
c---->                     simulazione con sette compressori inseriti
                            if((num+7).ge.numin) then
                             if (jtest(j_ind).lt.max_ch) then
                              call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                              call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                              call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                              if(ind_err(j_ind).eq.0) then
                               call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_min,lstat,
     *status(1,j_ind),jtest,j_ind)
                               if (nserv.eq.1) then
                                if (nact.eq.0) then
                                 do i=1,type_num
                                  nact=nact+type_act_opt(i)
                                 end do
                                end if
                               end if
                               if (jtest(j_ind).eq.max_ch) goto 10
                              end if
                             end if

                            end if
                           end if
                          end if
                          type_act_opt(k7)=type_act_opt(k7)-1
                         end do
                        else
c---->                     simulazione con sei compressori inseriti
                         if((num+6).ge.numin) then
                          if (jtest(j_ind).lt.max_ch) then
                           call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                           call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                           call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                           if(ind_err(j_ind).eq.0) then
                            call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_min,lstat,
     *status(1,j_ind),jtest,j_ind)
                            if (nserv.eq.1) then
                             if (nact.eq.0) then
                              do i=1,type_num
                               nact=nact+type_act_opt(i)
                              end do
                             end if
                            end if
                            if (jtest(j_ind).eq.max_ch) goto 10
                           end if
                          end if

                         end if
                        end if
                       end if
                       type_act_opt(k6) = type_act_opt(k6)-1
                      end do
                     else
c---->                     simulazione con cinque compressori inseriti
                      if((num+5).ge.numin) then
                       if (jtest(j_ind).lt.max_ch) then
                        call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                        call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                        call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                        if(ind_err(j_ind).eq.0) then
                         call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_min,lstat,
     *status(1,j_ind),jtest,j_ind)
                         if (nserv.eq.1) then
                          if (nact.eq.0) then
                           do i=1,type_num
                            nact=nact+type_act_opt(i)
                           end do
                          end if
                         end if
                         if (jtest(j_ind).eq.max_ch) goto 10
                        end if
                       end if

                      end if
                     end if
                    end if
                    type_act_opt(k5) = type_act_opt(k5)-1
                   end do
                  else
    

c---->                  simulazione con quattro compressori inseriti
                   if((num+4).ge.numin) then
                    if (jtest(j_ind).lt.max_ch) then
                     call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                     call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)




                     call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                     if(ind_err(j_ind).eq.0) then
                      call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_min,lstat,
     *status(1,j_ind),jtest,j_ind)
                      if (nserv.eq.1) then
                       if (nact.eq.0) then
                        do i=1,type_num
                         nact=nact+type_act_opt(i)
                        end do
                       end if
                      end if
                      if (jtest(j_ind).eq.max_ch) goto 10
                     end if
                    end if

                   end if
                  end if
                 end if
                 type_act_opt(k4) = type_act_opt(k4)-1
                end do
               else
c---->                  simulazione con tre compressori inseriti
                if((num+3).ge.numin) then
                 if (jtest(j_ind).lt.max_ch) then
                  call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
                  call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
                  call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),delprint(j_ind),
     *deltrint(j_ind),ind_err(j_ind))
                  if(ind_err(j_ind).eq.0) then
                   call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),
     *opt_time,unit_num,lact_min,lstat,status(1,j_ind),jtest,j_ind)
                   if (nserv.eq.1) then
                    if (nact.eq.0) then
                     do i=1,type_num
                      nact=nact+type_act_opt(i)
                     end do
                    end if
                   end if
                   if (jtest(j_ind).eq.max_ch) goto 10
                  end if
                 end if

                end if
               end if
              end if
              type_act_opt(k3) = type_act_opt(k3)-1
             end do
            else
c---->                  simulazione con due compressori inseriti
             if((num+2).ge.numin) then
              if (jtest(j_ind).lt.max_ch) then
               call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
               call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
               call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),delprint(j_ind),
     *deltrint(j_ind),ind_err(j_ind))
               if(ind_err(j_ind).eq.0) then
                call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,
     *unit_num,lact_min,lstat,status(1,j_ind),jtest,j_ind)
                if (nserv.eq.1) then
                 if (nact.eq.0) then
                  do i=1,type_num
                   nact=nact+type_act_opt(i)
                  end do
                 end if
                end if
                if (jtest(j_ind).eq.max_ch) goto 10
               end if
              end if

             end if
            end if
           end if
           type_act_opt(k2) = type_act_opt(k2)-1
          end do
         else
c---->                  simulazione con un solo compressore inserito
          if((num+1).ge.numin) then
           if (jtest(j_ind).lt.max_ch) then
            call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
            call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
            call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),delprint(j_ind),
     *deltrint(j_ind),ind_err(j_ind))
            if(ind_err(j_ind).eq.0) then
             call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,
     *unit_num,lact_min,lstat,status(1,j_ind),jtest,j_ind)
             if (nserv.eq.1) then
              if (nact.eq.0) then
               do i=1,type_num
                nact=nact+type_act_opt(i)
               end do
              end if
             end if
             if (jtest(j_ind).eq.max_ch) goto 10
            else
c Se una unita' risulta non avere punti fattibili sulle due fasi, non viene
c piu' simulata, in particolare se (type_num=1) e (nserv=1) si esce
             call gest_errore(istaz(j_ind),ind_err(j_ind),tipo_criterio,
     *vert_type(j_ind),stat_ord(j_ind),ier)
             if (ier.gt.0) then
cgpe-corr-MB              type_avail_opt(k1,j_ind) = 0
c!!!-vedere questa condizione!!!
              if ((type_num.eq.1).and.(nserv.eq.1)) then
               ier = 1 ! ier = 11
               return
              end if
             end if
c
            end if
           end if

          end if
         end if
        end if
        type_act_opt(k1) = type_act_opt(k1)-1
       end do
       if (nserv.eq.1) then
c Nel caso in cui abbiamo un solo servizio da ottimizzare (nserv=1), il
c controllo viene fatto sul numero di test: la parte iterativa ha
c termine quando ho raggiunto inum=nact+2, o e' terminato il loop sulle
c unita'
        if ((jtest(j_ind).gt.0)  .and.
     *      ((inum+num).eq.(nact+max_test))) then
         ier = 0
         goto 10
        end if
       else if (nserv.eq.2) then
c Nel caso in cui nserv=2, bisogna fare tutte le possibili combinazioni
c delle unita', cioe' la parte iterativa ha termine se il loop sulle
c unita' e' terminato
        if (inum.ge.unit_num) then
         if (jtest(j_ind).gt.0) then
           goto 10
         else
!           ntest = 0
cgpe           ntest = 1
           ntest = 0
           goto 11
         end if
        end if
       end if
      end do
c
10    continue
c
c Nel caso in cui deve essere ottimizzato un solo servizio, e' necessario
c che i risultati dell'ottimizzazione vengano memorizzati nelle variabili
c ntest, ind_s e consumo, se jtest(j_ind)>0: viene calcolato l'indice
c dello stato ottimo. Altrimenti si pone ntest = 1. 
      if (nserv.eq.1) then
        if (jtest(j_ind).gt.0) then
          call new_opt_one(unit_num,type_num,type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,jtest,cost,lact_min,ntest,consumo,ind_s,
     *cons_ass)
        else
!          ntest = 0
cgpe          ntest = 1
          ntest = 0
        end if
        goto 11
      end if
c
c Nel caso in cui i servizi da ottimizzare siano due, calcolo delle
c possibili configurazioni per il servizio con il numero maggiore
c di unita'.
c L'algoritmo e' fatto in modo tale da provare tutte le unita' disponibili
c Nel caso in cui una simulazione termini con ind_err(j_ind)=0, allora
c vengono fatte le possibili combinazioni di questa simulazione con le
c simulazioni ottenute per l'altro servizio.
c Solamente nel caso ci sia compatibilita' fra i due funzionamenti (per
c la contemporaneita' delle unita'), viene incrementata la variabile ntest
c e memorizzato il consumo e le unita' attive dell combinazione trovata.
      j_ind = ind_max
      ind_serv = j_ind

C---->              calcolo del numero di unita' necessariamente accese
      i1=0
      num = 0
      do j=1,type_num
        type_act_opt(j) = 0
        do k=1,type_quant_opt(j,j_ind)
          if (unit_avail_opt(k+i1,j_ind).gt.1) then
            num = num+1
            type_act_opt(j) = type_act_opt(j)+1
          end if
        end do
        i1 = i1+type_quant_opt(j,j_ind)
      end do

C---->              riconoscimento di una centrale spenta
cgpe-staz      if (set_flag_mf(j_ind) .eq. 0) then
      if (l_simu(j_ind) .eq. 0) then
cgpe-staz-end
cgpe        if (num .eq. 0) then
cgpe!-non esistono unita' necessariamente accese
          jtest(j_ind) = 1
          do i = 1,unit_num
            lact_max(jtest(j_ind),i) = 0
          enddo
          cost(j_ind,jtest(j_ind)) = 0.
          call new_opt_global(unit_num,type_num,type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,ind_max,jtest,cost,lact_min,
     *lact_max,ntest,consumo,ind_s,cons_ass)
          goto 11
cgpe        else
cgpe!-ERRORE: la centrale e' spenta ed esistono unita' necessariamente accese
cgpe!         per il servizio ind_min
cgpe! - se c'e' un solo assetto attivo (nserv=1), si va alla label 11
cgpe! - se ci sono due assetti attivi (nserv=2), non si procede con il calcolo
cgpe!   delle possibili configurazioni per l'assetto ind_max, perche' un errore
cgpe!   nell'ottimizzazione di un assetto comporta un errore nell'ottimizzazione
cgpe!   della centrale multifunzione
cgpe!          ntest = 0
cgpe          ntest = 1
cgpe          goto 11
cgpe        endif
      endif
c
c---->              devo generare tutte le possibili configurazioni
c
c calcolo del numero minimo di unita' da accendere
c---->      grid verification

      ind = istaz(j_ind)
cgpe      call init_stvar(istaz(j_ind),vert_pointer(j_ind))

!      qq = flow2(j_ind)*flow2(j_ind)
!      delpr1(j_ind) = stat_vars(3,j_ind)*qq
!      delpr2(j_ind) = stat_vars(4,j_ind)*qq
!      pin  = pres1(j_ind)-delpr1(j_ind)
!      pout = pres2(j_ind)+delpr2(j_ind)
!      exp_pol=cesad(ind)
!      esp = (exp_pol-1.)/exp_pol
!      z = prop(pin,temp1(j_ind),z_coef)
!      comp_ratio(j_ind) = pout/pin
!      aux1 = (erre / agrvcc / pmol) * Z * temp1(j_ind)/esp
!      aux2 = comp_ratio(j_ind)**esp
!      head(j_ind) = aux1 * (aux2-1.)
!      aux3 = ROCN*(1/1.e5)*(erre/pmol)*z*temp1(j_ind)/pin
!      flow_tot = flow2(j_ind)*aux3
c per ora pongo numin=1, da sostituire con il numero minimo
c effettivo di unita' da accendere quando sara' creata la griglia
c tab_grid
c      call ver_grid(flow_tot,head(j_ind),dqs,dhs,type_num,numin,
c     *tab_grid,nhmax,nqmax,ind_err(j_ind))
      numin = 1
      if (ind_err(j_ind).ne.0) then
        ier = 1 ! ier = 11
        return
      end if
c---->                        simulation with minimal configuration
c passare le variabili poste con l'indice j_ind:
      if(num.ge.numin) then
       if (jtest(j_ind).lt.max_ch) then
        call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
        call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
        call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
c
c se ind_err(j_ind)=0, i risultati della simulazione devono essere memorizzati
c nelle variabili cost(j_ind) e lact(j_ind,maxunits)
        if (ind_err(j_ind).eq.0) then
c la variabile logica lact_min indica se le unita' sono attive o meno
          call new_opt_mf(cost,start_cost(1,j_ind),shut_cost(1,j_ind),
     *opt_time,unit_num,lact_max,lstat,status(1,j_ind),jtest,j_ind)
          call new_opt_global(unit_num,type_num,type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,ind_max,jtest,cost,lact_min,
     *lact_max,ntest,consumo,ind_s,cons_ass)
c calcolo il numero delle unita' attive nact_max del servizio ind_max
          nact_max = num
        else
c Controllo sugli errori che derivano dalla fattibilita' dei punti sulle
c due fasi
          call gest_errore(istaz(j_ind),ind_err(j_ind),tipo_criterio,
     *vert_type(j_ind),stat_ord(j_ind),ier)

c Se viene restituito ier non nullo, si esce dall'ottimizzazione.
          if (ier.gt.0) then
C            ier = 1 ! ier = 11
            return
          end if
c
        end if
       end if
      end if

c-dinamica
      if (fl_din) then
c viene provata solo la configurazione con i turbogruppi necessariamente accesi
        goto 11
      endif
c-dinamica-end
c
      do inum=1,12
       do k1=1,type_num
        type_act_opt(k1) = type_act_opt(k1)+1
        if(type_avail_opt(k1,j_ind).ge.type_act_opt(k1)) then
         if(inum.gt.1) then
c---->    +++  111 -----> piu' di un compressore inserito
          do k2=k1,type_num
           type_act_opt(k2) = type_act_opt(k2)+1
           if(type_avail_opt(k2,j_ind).ge.type_act_opt(k2)) then
            if(inum.gt.2) then
c---->    +++  222 ----> piu' di due compressori da inserire
             do k3=k2,type_num
              type_act_opt(k3) = type_act_opt(k3)+1
              if(type_avail_opt(k3,j_ind).ge.type_act_opt(k3)) then
               if(inum.gt.3) then
c---->    +++  333 ---->  piu' di tre compressori da inserire
                do k4=k3,type_num
                 type_act_opt(k4) = type_act_opt(k4)+1
                 if(type_avail_opt(k4,j_ind).ge.
     *              type_act_opt(k4)) then
                  if(inum.gt.4) then
c---->    +++  444 ---->  piu' di quattro compressori da inserire
                   do k5=k4,type_num
                    type_act_opt(k5) = type_act_opt(k5)+1
                    if(type_avail_opt(k5,j_ind).ge.
     *                 type_act_opt(k5)) then
                     if(inum.gt.5) then
c---->    +++  555 ---->  piu' di cinque compressori da inserire
                      do k6=k5,type_num
                       type_act_opt(k6) = type_act_opt(k6)+1
                       if(type_avail_opt(k6,j_ind).ge.
     *                    type_act_opt(k6)) then
                        if(inum.gt.6) then
c---->    +++  666 ---->  piu' di sei compressori da inserire
                         do k7=k6,type_num
                          type_act_opt(k7)=type_act_opt(k7)+1
                          if(type_avail_opt(k7,j_ind).ge.
     *                       type_act_opt(k7))then
                           if(inum.gt.7) then
c---->    +++  777 ---->  piu' di sette compressori da inserire
                            do k8=k7,type_num
                             type_act_opt(k8) =
     *                         type_act_opt(k8)+1
                             if(type_avail_opt(k8,j_ind).ge.
     *                          type_act_opt(k8))then
                              if(inum.gt.8) then
c---->    +++  888 ---->  piu' di otto compressori da inserire
                               do k9=k8,type_num
                                type_act_opt(k9) =
     *                            type_act_opt(k9)+1
                                if(type_avail_opt(k9,j_ind).ge.
     *                             type_act_opt(k9))then
                                 if(inum.gt.9) then
c---->    +++  999 ---->  piu' di nove compressori da inserire
                                  do k10=k9,type_num
                                   type_act_opt(k10) = 
     *                               type_act_opt(k10)+1
                                   if(type_avail_opt(k10,j_ind).ge.
     *                                type_act_opt(k10)) then
                                    if(inum.gt.10) then
c---->    +++  10 ---->  piu' di dieci compressori da inserire
                                     do k11=k10,type_num
                                      type_act_opt(k11) =
     *                                   type_act_opt(k11)+1
                                      if(type_avail_opt(k11,j_ind)
     *                                     .ge.type_act_opt(k11)) then
                                       if(inum.gt.11) then
c---->    +++  11 ---->  piu' di undici compressori da inserire
                                        do k12=k11,type_num
                                         type_act_opt(k12)=
     *                                     type_act_opt(k12)+1 
                                         if(type_avail_opt(k12,j_ind)
     *                                      .ge.type_act_opt(k12)) then
c---->    +++  12 ---->  simulazione con dodici  compressori inseriti
                                          if((num+12).ge.numin) then
!-check sul numero di test OK
                                           if (jtest(j_ind).lt.max_ch)
     *                                                             then
                                            call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                            call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
                                            call optim_vertex(
     *istaz(j_ind),iopt,vert_type(j_ind),tipo_criterio,
     *vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),jtfir(j_ind),type_num,
     *jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                            if(ind_err(j_ind).eq.0) then
                                             call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_max,lstat,status(1,j_ind),jtest,j_ind)
c  Nel caso in cui ind_err(j_ind)=0, bisogna fare le possibili combinazioni
c  della simulazione con indice jtest(ind_max) con le simulazioni ottenute
c  per l'altra centrale.
                                             ntest_old = ntest
                                             call new_opt_global(
     *unit_num,type_num,type_quant_opt(1,j_ind),unit_avail_opt,
     *ind_min,
     *ind_max,jtest,cost,lact_min,lact_max,ntest,consumo,
     *ind_s,cons_ass)
c calcolo il numero delle unita' attive nact_max del servizio ind_max
                                             if(ntest.gt.ntest_old)then
                                              do ind_ntest=ntest_old+1,
     *                                                     ntest
                                               call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                              end do
                                             end if
                                            end if
                                           end if

                                          end if
                                         end if
                                         type_act_opt(k12)=
     *                                     type_act_opt(k12)-1 
                                        end do
                                       else
c---->                     simulazione con 11 compressori inseriti
                                        if((num+11).ge.numin) then
                                         if (jtest(j_ind).lt.max_ch)
     *                                                            then
                                          call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                          call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
                                          call optim_vertex(
     *istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                          if(ind_err(j_ind).eq.0) then
                                           call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_max,lstat,status(1,j_ind),jtest,j_ind)
                                           ntest_old = ntest
                                           call new_opt_global(unit_num,
     *type_num,type_quant_opt(1,j_ind),unit_avail_opt,ind_min,
     *ind_max,
     *jtest,cost,lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                                           if (ntest.gt.ntest_old) then
                                            do ind_ntest=ntest_old+1,
     *                                                   ntest
                                             call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                            end do
                                           end if
c Se jtest(ind_max)=max_ch oppure se ntest=max_ch:
c   - se ntest>0, bisogna calcolare l'indice di ottimizzazione e lo stato
c        ottimo delle simulazioni di entrambi i servizi trovate fino ad ora
c oppure
c   - se ntest = 0, uscire con ier=11
                                           if((jtest(j_ind).eq.max_ch)
     *                                                 .or.
     *                                        (ntest.eq.max_ch)) goto 11
                                          end if
                                         end if

                                        end if
                                       end if
                                      end if
                                      type_act_opt(k11) =
     *                                  type_act_opt(k11)-1
                                     end do
                                    else
c---->                     simulazione con dieci compressori inseriti
                                     if((num+10).ge.numin) then
                                      if (jtest(j_ind).lt.max_ch) then
                                       call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                       call genera_stat(
     *tipo_criterio,vert_type(j_ind),unit_num,type_num,
     *type_act(jtfir(j_ind)),type_act_opt,type_quant_opt(1,j_ind),
     *lstat,stat)
                                       call optim_vertex(istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                       if(ind_err(j_ind).eq.0) then
                                        call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_max,lstat,status(1,j_ind),jtest,j_ind)
                                        ntest_old = ntest
                                        call new_opt_global(unit_num,
     *type_num,type_quant_opt(1,j_ind),unit_avail_opt,ind_min,
     *ind_max,jtest,
     *cost,lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                                        if (ntest.gt.ntest_old) then
                                         do ind_ntest=ntest_old+1,ntest
                                          call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                         end do
                                        end if
                                        if((jtest(j_ind).eq.max_ch).or.
     *                                     (ntest.eq.max_ch)) goto 11
                                       end if
                                      end if

                                     end if
                                    end if
                                   end if
                                   type_act_opt(k10)=
     *                               type_act_opt(k10)-1
                                  end do
                                 else
c---->                     simulazione con nove compressori inseriti
                                  if((num+9).ge.numin) then
                                   if (jtest(j_ind).lt.max_ch) then
                                    call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                    call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                                    call optim_vertex(istaz(j_ind),
     *iopt,vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                    if(ind_err(j_ind).eq.0) then
                                     call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_max,lstat,status(1,j_ind),jtest,j_ind)
                                     ntest_old = ntest
                                     call new_opt_global(unit_num,
     *type_num,type_quant_opt(1,j_ind),unit_avail_opt,ind_min,
     *ind_max,
     *jtest,cost,lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                                     if (ntest.gt.ntest_old) then
                                      do ind_ntest=ntest_old+1,ntest
                                       call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                      end do
                                     end if
                                     if((jtest(j_ind).eq.max_ch) .or.
     *                                  (ntest.eq.max_ch)) goto 11
                                    end if
                                   end if

                                  end if
                                 end if
                                end if
                                type_act_opt(k9) =
     *                            type_act_opt(k9)-1
                               end do
                              else
c---->                     simulazione con otto compressori inseriti
                               if((num+8).ge.numin) then
                                if (jtest(j_ind).lt.max_ch) then
                                 call init_vert_type (
     *vert_pointer(j_ind),type_act_opt,vert_type(j_ind))
                                 call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                                 call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                                 if(ind_err(j_ind).eq.0) then
                                  call new_opt_mf(cost,
     *start_cost(1,j_ind),shut_cost(1,j_ind),opt_time,unit_num,
     *lact_max,lstat,status(1,j_ind),jtest,j_ind)
                                  ntest_old = ntest
                                  call new_opt_global(unit_num,
     *type_num,type_quant_opt(1,j_ind),unit_avail_opt,ind_min,
     *ind_max,
     *jtest,cost,lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                                  if (ntest.gt.ntest_old) then
                                   do ind_ntest=ntest_old+1,ntest
                                    call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                   end do
                                  end if
                                  if((jtest(j_ind).eq.max_ch) .or.
     *                               (ntest.eq.max_ch)) goto 11
                                 end if
                                end if

                               end if
                              end if
                             end if
                             type_act_opt(k8) = 
     *                         type_act_opt(k8)-1
                            end do
                           else
c---->                     simulazione con sette compressori inseriti
                            if((num+7).ge.numin) then
                             if (jtest(j_ind).lt.max_ch) then
                              call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                              call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                              call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),tipo_criterio,vert_pointer(j_ind),
     *flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                              if(ind_err(j_ind).eq.0) then
                               call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_max,lstat,
     *status(1,j_ind),jtest,j_ind)
                               ntest_old = ntest
                               call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),unit_avail_opt,ind_min,ind_max,jtest,
     *cost,
     *lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                               if (ntest.gt.ntest_old) then
                                do ind_ntest=ntest_old+1,ntest
                                 call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                                end do
                               end if
                               if((jtest(j_ind).eq.max_ch) .or.
     *                            (ntest.eq.max_ch)) goto 11
                              end if
                             end if

                            end if
                           end if
                          end if
                          type_act_opt(k7)=type_act_opt(k7)-1
                         end do
                        else
c---->                     simulazione con sei compressori inseriti
                         if((num+6).ge.numin) then
                          if (jtest(j_ind).lt.max_ch) then
                           call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                           call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                           call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                           if(ind_err(j_ind).eq.0) then
                            call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_max,lstat,
     *status(1,j_ind),jtest,j_ind)
                            ntest_old = ntest
                            call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),unit_avail_opt,ind_min,ind_max,jtest,
     *cost,
     *lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                            if (ntest.gt.ntest_old) then
                             do ind_ntest=ntest_old+1,ntest
                              call calc_nact(unit_num,
     *ind_ntest,ind_min,ind_max,ind_s,nact_min,nact_max)
                             end do
                            end if
                            if((jtest(j_ind).eq.max_ch) .or.
     *                         (ntest.eq.max_ch)) goto 11
                           end if
                          end if

                         end if
                        end if
                       end if
                       type_act_opt(k6) = type_act_opt(k6)-1
                      end do
                     else
c---->                     simulazione con cinque compressori inseriti
                      if((num+5).ge.numin) then
                       if (jtest(j_ind).lt.max_ch) then
                        call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                        call genera_stat(tipo_criterio,
     *vert_type(j_ind),unit_num,type_num,type_act(jtfir(j_ind)),
     *type_act_opt,type_quant_opt(1,j_ind),lstat,stat)
                        call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                        if(ind_err(j_ind).eq.0) then
                         call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_max,lstat,
     *status(1,j_ind),jtest,j_ind)
                         ntest_old = ntest
                         call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),unit_avail_opt,ind_min,ind_max,jtest,
     *cost,
     *lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                         if (ntest.gt.ntest_old) then
                          do ind_ntest=ntest_old+1,ntest
                           call calc_nact(unit_num,ind_ntest,ind_min,
     *ind_max,ind_s,nact_min,nact_max)
                          end do
                         end if
                         if((jtest(j_ind).eq.max_ch) .or.
     *                      (ntest.eq.max_ch)) goto 11
                        end if
                       end if

                      end if
                     end if
                    end if
                    type_act_opt(k5) = type_act_opt(k5)-1
                   end do
                  else
c---->                  simulazione con quattro compressori inseriti
                   if((num+4).ge.numin) then
                    if (jtest(j_ind).lt.max_ch) then
                     call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                     call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
                     call optim_vertex(istaz(j_ind),iopt,
     *vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),
     *jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                     if(ind_err(j_ind).eq.0) then
                      call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),opt_time,unit_num,lact_max,lstat,
     *status(1,j_ind),jtest,j_ind)
                      ntest_old = ntest
                      call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),unit_avail_opt,ind_min,ind_max,jtest,
     *cost,
     *lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
                      if (ntest.gt.ntest_old) then
                       do ind_ntest=ntest_old+1,ntest
                        call calc_nact(unit_num,ind_ntest,ind_min,
     *ind_max,ind_s,nact_min,nact_max)
                       end do
                      end if
                      if((jtest(j_ind).eq.max_ch) .or.
     *                   (ntest.eq.max_ch)) goto 11
                     end if
                    end if

                   end if
                  end if
                 end if
                 type_act_opt(k4) = type_act_opt(k4)-1
                end do
               else
c---->                  simulazione con tre compressori inseriti
                if((num+3).ge.numin) then
                 if (jtest(j_ind).lt.max_ch) then
                  call init_vert_type (vert_pointer(j_ind),
     *type_act_opt,vert_type(j_ind))
                  call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
                  call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,vert_pointer(j_ind),flow(j_ind),
     *flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),temp1(j_ind),temp2(j_ind),air(j_ind),
     *delpr1(j_ind),delpr2(j_ind),comp_ratio(j_ind),
     *cost(j_ind,jtest(j_ind)+1),flowm(j_ind),flow_ric(j_ind),
     *head(j_ind),unit_num,stat_ord(j_ind),jtfir(j_ind),
     *type_num,jufir(j_ind),
     *stat_vars(1,j_ind),pres_int(j_ind),
     *temp_int(j_ind),
     *delprint(j_ind),deltrint(j_ind),ind_err(j_ind))
                  if(ind_err(j_ind).eq.0) then
                   call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),
     *opt_time,unit_num,lact_max,lstat,status(1,j_ind),jtest,j_ind)
                   ntest_old = ntest
                   call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,ind_max,jtest,cost,lact_min,lact_max,
     *ntest,consumo,ind_s,cons_ass)
                   if (ntest.gt.ntest_old) then
                    do ind_ntest=ntest_old+1,ntest
                     call calc_nact(unit_num,ind_ntest,ind_min,
     *ind_max,ind_s,nact_min,nact_max)
                    end do
                   end if
                   if((jtest(j_ind).eq.max_ch) .or.
     *                (ntest.eq.max_ch)) goto 11
                  end if
                 end if

                end if
               end if
              end if
              type_act_opt(k3) = type_act_opt(k3)-1
             end do
            else
c---->                  simulazione con due compressori inseriti
             if((num+2).ge.numin) then
              if (jtest(j_ind).lt.max_ch) then
               call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
               call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
               call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),delprint(j_ind),
     *deltrint(j_ind),ind_err(j_ind))
               if(ind_err(j_ind).eq.0) then
                call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),
     *opt_time,unit_num,lact_max,lstat,status(1,j_ind),jtest,j_ind)
                ntest_old = ntest
                call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,ind_max,jtest,cost,lact_min,lact_max,
     *ntest,consumo,ind_s,cons_ass)
                if (ntest.gt.ntest_old) then
                 do ind_ntest=ntest_old+1,ntest
                  call calc_nact(unit_num,ind_ntest,ind_min,ind_max,
     *ind_s,nact_min,nact_max)
                 end do
                end if
                if((jtest(j_ind).eq.max_ch) .or.
     *             (ntest.eq.max_ch)) goto 11
               end if
              end if

             end if
            end if
           end if
           type_act_opt(k2) = type_act_opt(k2)-1
          end do
         else
c---->                  simulazione con un solo compressore inserito
          if((num+1).ge.numin) then
           if (jtest(j_ind).lt.max_ch) then
            call init_vert_type (vert_pointer(j_ind),type_act_opt,
     *vert_type(j_ind))
            call genera_stat(tipo_criterio,vert_type(j_ind),
     *unit_num,type_num,type_act(jtfir(j_ind)),type_act_opt,
     *type_quant_opt(1,j_ind),lstat,stat)
            call optim_vertex(istaz(j_ind),iopt,vert_type(j_ind),
     *tipo_criterio,
     *vert_pointer(j_ind),flow(j_ind),flow2(j_ind),
     *pres1(j_ind),pres2(j_ind),
     *temp1(j_ind),temp2(j_ind),air(j_ind),delpr1(j_ind),
     *delpr2(j_ind),comp_ratio(j_ind),cost(j_ind,jtest(j_ind)+1),
     *flowm(j_ind),flow_ric(j_ind),head(j_ind),unit_num,
     *stat_ord(j_ind),jtfir(j_ind),type_num,jufir(j_ind),
     *stat_vars(1,j_ind),
     *pres_int(j_ind),temp_int(j_ind),delprint(j_ind),
     *deltrint(j_ind),ind_err(j_ind))
            if(ind_err(j_ind).eq.0) then
             call new_opt_mf(cost,start_cost(1,j_ind),
     *shut_cost(1,j_ind),
     *opt_time,unit_num,lact_max,lstat,status(1,j_ind),jtest,j_ind)
             ntest_old = ntest
             call new_opt_global(unit_num,type_num,
     *type_quant_opt(1,j_ind),
     *unit_avail_opt,ind_min,ind_max,jtest,cost,lact_min,
     *lact_max,ntest,consumo,ind_s,cons_ass)
             if (ntest.gt.ntest_old) then
              do ind_ntest=ntest_old+1,ntest
               call calc_nact(unit_num,ind_ntest,ind_min,ind_max,
     *ind_s,nact_min,nact_max)
              end do
             end if
             if((jtest(j_ind).eq.max_ch) .or.
     *          (ntest.eq.max_ch)) goto 11
            else
c Controllo sugli errori che derivano dalla fattibilita' dei punti sulle fasi
             call gest_errore(istaz(j_ind),ind_err(j_ind),tipo_criterio,
     *vert_type(j_ind),stat_ord(j_ind),ier)

c Se viene restituito ier non nullo, si esce dall'ottimizzazione.
             if (ier.gt.0) then
              type_avail_opt(k1,j_ind) = 0
cc                if (type_num.eq.1) then
c                if ((type_num.le.2).and.(nserv.eq.2)) then
c                 ier = 1 ! ier = 11
c                 return
c                end if
             end if
c
            end if
           end if
          end if
         end if
        end if
        type_act_opt(k1) = type_act_opt(k1)-1
       end do
c La parte iterativa ha termine quando ho raggiunto il numero delle unita'
c simulate nel test ntest pari a 2 sommato al numero minimo di unita'
c attive per i servizi ind_min e ind_max oppure e' terminato il loop sulle
c unita'
       if ((inum.ge.unit_num)                           .or.
     *     ((jtest(j_ind).gt.0)                         .and.
     *     ((inum+num).eq.(nact_min+nact_max+max_test))) ) then
        ind_err(j_ind) = 0
        goto 11
       end if
      end do
c
11    continue
c
cgpe      if (ntest.eq.1 .and. consumo(ntest).eq.R_MAX) then
      if (ntest .eq. 0) then
C l'algoritmo di ottimizzazione non ha trovato soluzione
        IER = 1 ! ier = 11
      else
c e' stata trovata almeno una configurazione fattibile
cgpe        if (fl_cons) then
cgpec fl_cons = .F. => cons_serv_imp = 0.
cgpe          ier = 0
cgpe          ind_opt = 1
cgpe          do i=2,ntest
cgpe            if (consumo(i).le.consumo(ind_opt) .and.
cgpe     *          consumo(i).ge.0                     ) then
cgpe              ind_opt = i
cgpe            end if
cgpe          end do
cgpe        else
c fl_cons = .T. => cons_serv_imp = consty
c correzione del consumo, viene scelta la configurazione che minimizza
c la differenza di consumo trovato per ciascun servizio con il consumo dato
c in input
c fl_cons = .F. => cons_serv_imp = 0.
c               => viene scelta la configurazione che minimizza il consumo
cgpe         ind_opt = 0
cgpe          if (ntest.gt.0) then
            ier = 0
            diff_cons = R_MAX
            do i = 1,ntest
cgpe              diff_cons_aux = abs(consumo(i) - (cons_serv_imp(ind_min) +
cgpe     *                        cons_serv_imp(ind_max)))
              if (nserv .eq. 1) then
c e' stato definito solo il servizio ind_min
                diff_cons_aux = abs(consumo(i) - cons_serv_imp(ind_min))
              else
                diff_cons_aux = abs(consumo(i) -(cons_serv_imp(ind_min)+
     *                          cons_serv_imp(ind_max)))
              endif
              if (diff_cons_aux .lt. diff_cons) then
                diff_cons = diff_cons_aux
                ind_opt = i
              end if
            end do
cgpe          endif
cgpe        endif
      endif
c
      if (ind_opt .eq. 0) then
C la simulazione non ha trovato soluzione
        IER = 1 ! ier = 11
      endif
c
      return
      end

      Subroutine new_opt_mf(tot_cost,start_cost,shut_cost,
     *             opt_time,unit_num,lact,lstatus,
     *             status,jtest,jind)
c
c  Viene calcolato il costo (tot_cost -O-) della simulazione come il totale
c  dei consumi (tot_cost -I-) piu' il costo di accensione o spegnimento
c  (start_cost/shut_cost), se opt_time e' diverso da zero.
c  I valori di lstatus e di status indicano se e' necessario aggiungere
c  un costo di accensione o spegnimento
c
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'
      include '../inc/RK_PARAM.INC'
      include '../inc/OTC_PAR.INC'
C
      integer*4 i,jind
      integer*4 unit_num
      integer*4 jtest(max_serv)
      real*4    start_cost(*),shut_cost(*),opt_time
      real*4    tot_cost(max_serv,max_ch)
      logical*2 lstatus(*),lact(max_ch,maxunits)
      character*3 status_app ! stato dell'unita' dell'attuale configurazione
      character*3 status(*)  ! stato dell'unita' da CIN

      INTEGER*4 IND_UNI
c..........................................................................

      jtest(jind) = jtest(jind)+1

c calcolo il costo di spegnimento e di accensione solo se opt_time e'
c diverso da zero
      if (opt_time.ne.0) then
        do i=1,unit_num
          if (lstatus(i)) then
            status_app = funz_on
          else
            status_app = funz_off
          endif
          call calc_cons_tot (status(i),status_app,start_cost(i),
     *         shut_cost(i),opt_time,tot_cost(jind,jtest(jind)))

        end do
      end if
c
      do ind_uni=1,maxunits
        if (lstatus(ind_uni)) then
          lact(jtest(jind),ind_uni) = .true.
        else
          lact(jtest(jind),ind_uni) = .false.
        end if
      enddo
c
      return
      end

      Subroutine calc_nact(unit_num,ntest,imin,imax,ind_s,
     *           nactmin,nactmax)
c
c Calcolo del numero minimo di unita' attive per la centrale con indice imin
c (nactmin) e per la centrale con indice imax (nactmax)
c
c     ntest         I   indice della simulazione globale
c     ind_s(j,k)    I    per il test j, k assume i valori:
c                          0  se l'unita' non e' attiva
c                          imin  se l'unita' e' attiva per il servizio imin
c                          imax  se l'unita' e' attiva per il servizio imax
c     nactmin       O    numero delle unita' attive per la centrale imin
c     nactmax       O    numero delle unita' attive per la centrale imax
c
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'
      include '../inc/RK_PARAM.INC'
      include '../inc/OTC_PAR.INC'
C
      integer*4 j
      integer*4 unit_num
      integer*4 ntest
      integer*4 imin,imax
      INTEGER*4 ind_s(max_ch,maxunits)
      integer*4 nactmin,nactmax,nact

      INTEGER*4 I
c-----------------------------------------------------------------------
c calcolo del numero delle unita' attive per il servizio imax
      nact = 0
      if (nactmax.eq.0) then
        do j=1,unit_num
          if (ind_s(ntest,j).eq.imax) then
            nactmax = nactmax + 1
          end if
        end do
      else
        do j=1,unit_num
          if (ind_s(ntest,j).eq.imax) then
            nact = nact + 1
          end if
        end do
      end if
c
c nactmax e' il numero minimo di unita' attive per le simulazioni relative
c alla centrale con il numero maggiore di unita'
      if ((nact.gt.0).and.(nact.lt.nactmax)) then
        nactmax = nact
      end if
c
c calcolo del numero di unita' attive per il servizio imin 
      nact = 0
      if (nactmin.eq.0) then
        do i=1,unit_num
          if (ind_s(ntest,i).eq.imin) then
            nactmin = nactmin + 1
          end if
        end do
      else
        do i=1,unit_num
          if (ind_s(ntest,i).eq.imin) then
            nact = nact + 1
          end if
        end do
      end if
c
c nactmin e' il numero minimo di unita' attive per le simulazioni relative
c alla centrale con il numero minimo di unita'
      if ((nact.gt.0).and.(nact.lt.nactmin)) then
        nactmin = nact
      end if
c
      return
      end

      Subroutine new_opt_global(unit_num,type_num,type_quant,
     *             unit_avail_mf_opt,imin,imax,jtest,cost,
     *             lact_min,lact_max,ntest,consumo,ind_s,cons_ass)
c
c  Vengono restituiti, nel caso in cui le configurazioni dei due servizi
c  siano compatibili:
c    -  ntest incrementato per ogni configurazione compatibile
c    -  associato ad ntest, il costo della simulazione globale (consumo)
c    -  associato ad ntest, le unita' attive per ciascun servizio
c
c  Vengono fatte le possibili combinazioni della simulazione con indice
c  jtest(imax) del servizio imax con tutte le simulazioni del servizio
c  imin, di cui abbiamo per ciascuna configurazione le unita' attive e
c  i relativi consumi (cost(imin,max_ch) e cost(imax,max_ch)).
c
c     ntest        I/O   indice della simulazione globale
c     jtest(imin)   I    numero dei test per la centrale imin
c     jtest(imax)   I    numero dei test per la centrale imax
c     ind_s(j,k)    O    per il test j, k assume i valori:
c                          0    se l'unita' non e' attiva
c                          imin se l'unita' e' attiva per il servizio imin
c                          imax se l'unita' e' attiva per il servizio imax
c
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'
      include '../inc/RK_PARAM.INC'
      include '../inc/OTC_PAR.INC'
C
      integer*4 i,i1,i2,j,k,jind,imin,imax
      integer*4 unit_num,type_num,type_quant(*)
      integer*4 ntest,jtest(max_serv)
      real*4    consumo(max_ch),cost(max_serv,max_ch)
      logical*2 lact_min(max_ch,maxunits),lact_max(max_ch,maxunits)
      logical*2 l_comp
      INTEGER*4 ind_s(max_ch,maxunits)
      integer*4 n,nact_min,nact_max
      INTEGER*4 unit_avail_mf_opt(maxunits,max_serv)
      integer*4 num_act_min,num_act_max

      real*4    cons_ass(max_serv,*)
      real*4    cons_aux,cons_prec
c-----------------------------------------------------------------------
c Ciclo sulle simulazioni della centrale con il numero minore di unita'
      do jind=1,jtest(imin)
        cons_aux = cost(imin,jind) + cost(imax,jtest(imax))
cgpe
cgpe        if (cons_aux .lt. consumo(ntest)) then
        if (ntest .eq. 0) then
          cons_prec = R_MAX
        else
          cons_prec = consumo(ntest)
        endif
        if (cons_aux .lt. cons_prec) then
cgpe-end
!-check se le configurazioni trovate per imin e imax sono compatibili
          i1 = 0
          do i=1,type_num
            if (type_quant(i).eq.1) then
              do k=1,type_quant(i)
               if ((lact_min(jind,k+i1))   .and.
     *             (lact_max(jtest(imax),k+i1))) then
                  l_comp = .false.
                  goto 11
                else
                  l_comp = .true.
                end if
              end do
            else if (type_quant(i).gt.1) then
c calcolo del numero di unita' accese del tipo i-esimo 
              nact_min = 0
              nact_max = 0
              do j=1,type_quant(i)
                if (lact_min(jind,j+i1)) then
                  nact_min = nact_min + 1
                end if
                if (lact_max(jtest(imax),j+i1)) then
                  nact_max = nact_max + 1
                end if
              end do
              if ((nact_max+nact_min).le.type_quant(i)) then
                l_comp = .true.
                goto 10
              else
                l_comp = .false.
                goto 11
              end if
            end if
10        continue
            i1 = i1+type_quant(i)
          end do
c
11        continue
          if (l_comp) then
            if (ntest+1.eq.max_ch) then
c assegnare un codice di errore
              return
            end if
            ntest = ntest + 1
            do i = 1,unit_num
              ind_s(ntest,i) = 0
            enddo
c calcolo di consumo, consumo relativo alla simulazione globale, come
c somma di cost(imin,jind), consumo relativo alla simulazione jind
c della centrale imin e di cost(imax,jtest(imax)), consumo relativo
c alla simulazione jtest(imax) 
            consumo(ntest) = cost(imin,jind) + cost(imax,jtest(imax))

            cons_ass(imin,ntest) = cost(imin,jind)
            cons_ass(imax,ntest) = cost(imax,jtest(imax))

c assegnazione del vettore ind_s
            i2 = 0
            do i=1,type_num
              if (type_quant(i).eq.1) then
                do k=1,type_quant(i)
                  if (lact_min(jind,k+i2)) then
                    ind_s(ntest,k+i2) = imin
                  else if (lact_max(jtest(imax),k+i2)) then
                    ind_s(ntest,k+i2) = imax
                  else
                    ind_s(ntest,k+i2) = 0
                  end if
                end do
              else if (type_quant(i).gt.1) then
                nact_min = 0
                nact_max = 0
                num_act_min = 0
                num_act_max = 0
                do j=1,type_quant(i)
!-calcolo delle unita' accese nei servizi imin e imax nella configurazione
!-ottima globale
                  if (lact_min(jind,j+i2)) then
                    nact_min = nact_min + 1
                  end if
                  if (lact_max(jtest(imax),j+i2)) then
                    nact_max = nact_max + 1
                  end if

!-calcolo delle unita' necessariamente accese nei servizi imin e imax
!-NOTA: una unita' non deve risultare necessariamente accesa in entrambi i
!-      servizi
                  if (unit_avail_mf_opt(j+i2,imin).eq.2) then
                    num_act_min = num_act_min + 1
                  end if
                  if (unit_avail_mf_opt(j+i2,imax).eq.2) then
                    num_act_max = num_act_max + 1
                  end if
                end do
c
!-assegnazione delle unita' necessariamente accese per i servizi imin e imax
                do j=1,type_quant(i)
                  if (unit_avail_mf_opt(j+i2,imin).eq.2) then
                    ind_s(ntest,j+i2) = imin
                    nact_min = nact_min - 1
                    num_act_min = num_act_min - 1
                  else if (unit_avail_mf_opt(j+i2,imax).eq.2) then
                    ind_s(ntest,j+i2) = imax
                    nact_max = nact_max - 1
                    num_act_max = num_act_max - 1
                  endif
                enddo
c
!-assegnazione delle unita' accese per i servizi imin e imax che non sono
!-ancora state assegnate
                do j=1,type_quant(i)
                  if (nact_min.gt.0 .and. ind_s(ntest,j+i2).eq.0) then
                    ind_s(ntest,j+i2) = imin
                    nact_min = nact_min - 1
                  else if (nact_max.gt.0 .and.
     *                     ind_s(ntest,j+i2).eq.0) then
                    ind_s(ntest,j+i2) = imax
                    nact_max = nact_max - 1
                  end if
                end do
              end if
              i2 = i2+type_quant(i)
            end do
          end if ! end (if (l_comp))

        end if ! end (if cons_aux.lt.consumo(ntest))

      end do
c
      return ! new_opt_global
      end

      Subroutine new_opt_one(unit_num,type_num,type_quant,
     *           unit_avail_mf_opt,imin,jtest,cost,
     *           lact_min,ntest,consumo,ind_s,cons_ass)
c
c  Caso in cui viene ottimizzato un solo servizio. Vengono restituiti:
c
c    -  ntest incrementato per ogni configurazione trovata per il servizio
c    -  associato ad ntest, il costo della simulazione (consumo)
c    -  associato ad ntest, le unita' attive per il servizio
c
c  dei risultati della simulazione del servizio imin, di cui abbiamo per
c  ciascuna configurazione le unita' attive e i consumi
c
c     ntest        I/O   indice della simulazione globale
c     jtest(imin)   I    numero dei test per la centrale imin
c     ind_s(j,k)    O    per il test j, k assume i valori:
c                          0 se l'unita' non e' attiva
c                          imin se l'unita' e' attiva per il servizio imin
c
      implicit none
*&*
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'
      include '../inc/RK_PARAM.INC'
      include '../inc/OTC_PAR.INC'
C
      integer*4 i,i1,k,jind,imin
      integer*4 unit_num,type_num,type_quant(*),
     *          unit_avail_mf_opt(max_ch,maxunits)
      integer*4 ntest,jtest(max_serv)
      real*4    consumo(max_ch),cost(max_serv,max_ch)
      logical*2 lact_min(max_ch,maxunits)
      INTEGER*4 ind_s(max_ch,maxunits)
      integer*4 nact_min,num_act_min,j,i2
      real*4    cons_ass(max_serv,*)
c-----------------------------------------------------------------------
c Ciclo sulle simulazioni della centrale con il numero minore di unita'
      do jind=1,jtest(imin)
        if (ntest+1.eq.max_ch) then
c assegnare un indice di errore
          return
        end if

cgpe
        if (cost(imin,jind) .lt. R_MAX) then
cgpe        if (cost(imin,jind) .lt. consumo(ntest)) then
cgpe-end

c          cons_ass(imin) = cost(imin,jind)

          ntest = ntest + 1
c calcolo di consumo, consumo relativo alla simulazione della centrale
c imin  pari a cost(imin,jind), 
          consumo(ntest) = cost(imin,jind)
          cons_ass(imin,ntest) = cost(imin,jind)

!-inizializzazione del vettore ind_s
          do i = 1,unit_num
            ind_s(ntest,i) = 0
          enddo

!-assegnazione del vettore ind_s
          i2 = 0
          do i=1,type_num
            if (type_quant(i).eq.1) then
              do k=1,type_quant(i)
                if (lact_min(jind,k+i2)) then
                  ind_s(ntest,k+i2) = imin
                else
                  ind_s(ntest,k+i2) = 0
                end if
              end do
            else if (type_quant(i).gt.1) then
              nact_min = 0
              num_act_min = 0
              do j=1,type_quant(i)
!-calcolo delle unita' accese nel servizio imin
                if (lact_min(jind,j+i2)) then
                  nact_min = nact_min + 1
                end if
!-calcolo delle unita' necessariamente accese nel servizio imin
                if (unit_avail_mf_opt(j+i2,imin).eq.2) then
                  num_act_min = num_act_min + 1
                end if
              end do

!-assegnazione delle unita' necessariamente accese nel servizio imin
              do j=1,type_quant(i)
                if (unit_avail_mf_opt(j+i2,imin).eq.2) then
                  ind_s(ntest,j+i2) = imin
                  nact_min = nact_min - 1
                  num_act_min = num_act_min - 1
                endif
              enddo

!-assegnazione delle unita' accese nel servizio imin che non sono
!-ancora state assegnate
              do j=1,type_quant(i)
                if (nact_min.gt.0 .and.
     *              ind_s(ntest,j+i2).eq.0 ) then
                  ind_s(ntest,j+i2) = imin
                  nact_min = nact_min - 1
                end if
              end do
            end if
            i2 = i2+type_quant(i)
          end do

        endif ! end if (cost(imin,jind).lt.consumo(ntest)) 
      end do
c
      return ! new_opt_one
      end
