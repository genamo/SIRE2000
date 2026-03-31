
      Subroutine optcent(istaz,vert_type,tipo_criterio,
cgpe     *      vert_pointer,flow2,pres1,pres2,temp1,
     *      vert_pointer,flow2,pres1,pres2,delpr1,delpr2,temp1,
cgpe-end
     *      air,unit_num,stat_ord,jtfir,type_num,jufir,
     *      type_quant_opt,type_avail_opt,unit_avail_opt,
     *      status,stat_vars,start_cost,shut_cost,
c     *      consumo,msg_err,ier)
     *      consumo,ier)
c
c	ottimizzazione di una stazione non multifunzione
c	con ricerca della configurazione a minor costo
c----------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/rk_param.INC'
      include '../inc/SIMPSTAT.INC'
      include '../inc/TYPES.INC'
      include '../inc/OTC.INC'
      include '../inc/OTC_TIME.INC'
      include '../inc/flag.INC'
c
c       istaz        I  indice della stazione nella struttura dati SIRE
c                       completa (ind=oiwato(j)): va messo nel common
c                       stazione.inc nella variabile ind.
c       iopt         I  indice di ottimizzazione (iopt=.T. ottimizzazione)
c       vert_type    I  tipo di stazione (1=semplice, 2=composta, 3=mista)
c       vert_pointer I  indice della stazione nella struttura interna di
c                       dati di stazione
c       flow        I/O portata entrante in stazione
c       flow2        O  portata uscente (lavorata dalla stazione)
c       pres1        I  pressione di aspirazione
c       pres2        I  pressione di mandata
c       temp1        I  temperatura di aspirazione
c       temp2        O  temperaura di mandata
c       air          I  temperatura dell'aria
c       delpr1       I  perdita di pressione effettiva in ingresso
c       delpr2       I  perdita di pressione effettiva in uscita
c       comp_ratio   O  rapporto di compressione effettivo
c       tot_cons     I/O consumo della stazione
c       flowm        O  portata di macchina totale della stazione
c       flow_ric     O  portata normal condition totale della stazione
c                       (con eventuale riciclo)
c       head         O  adiabatic head della stazione
c       unit_num     I  numero totale di macchine (effettive) della stazione
c       stat_ord     I  tipo di funzionamento della stazione, se di tipo
c                       composto
c       jtfir        I  puntatore al primo tipo della stazione
c       type_num     I  numero di tipi (effettivi) della stazione
c       jufir        I  puntatore alla prima unita' della stazione
c       stat_vars    I  variabili topologiche di stazione
c nn    nit          I  numero di iterazione (per i messaggi)
c nn    nitmax       I  numero di iterazione massimo richiesto dall'esterno
c nn    tipo_corr    I  tipo di correzione da apportare nei casi estremi
c                       (se il set e' in pressione e la correzione e'
c                        in portata, oppure se il set e' in portata e la
c                        correzione e' in pressione)
c                        1=correzione
c                        0=nessuna correzione
c       start_cost   I  consumo dovuto all'accensione di una unita'
c       shut_cost    I  consumo dovuto allo spegnimento di una unita'
c       opt_time     I  tempo di ottimizzazione
c       ntest           indice della simulazione con ier=0
c       nact            numero delle unita' attive di ntest=1
c       cost         O  costo della simulazione
c       ind_s        O  ind_s(max_ch,maxunits)
c       ind_opt      O  indice della simulazione a minor costo
c-mf03
c       consumo      O  consumo della configurazione a minor costo (al netto dell'eventuale
c                       soglia_cons
c-mf03-end
c       ier          O  error index
c
c       status       I  stato (ON/OFF) delle unita' della centrale
c       lstat           variabile logica (T/F) per le unita' da simulare
c       stat            stato (ON/OFF) per le unita' da simulare (simpstat.INC)
c
      INTEGER*4 istaz,vert_type,tipo_criterio,vert_pointer,
     *          unit_num,jtfir,type_num,jufir,stat_ord
      INTEGER*4 type_quant_opt(maxunits),type_avail_opt(maxunits),
     *          unit_avail_opt(maxunits)
      REAL*4    flow2,pres1,pres2,temp1,air,stat_vars(10)
      CHARACTER*3 status(*)     ! I) stato delle unita' da CIN

      REAL*4    start_cost(*)   ! I) costo di accensione delle unita''
     *         ,shut_cost(*)    ! I) costo di spegnimento  "     "

      REAL*4    consumo         ! O) consumo ottimo della centrale vert_pointer
c      CHARACTER*(*) msg_err     ! O) messaggio di errore
      INTEGER*4 ier             ! O) indice di errore
c
      REAL*4    cost(max_ch),tot_cons(max_ch)
      LOGICAL*2 lstat(maxunits)
      INTEGER*4 type_act_opt(maxunits)
c
      INTEGER*4 nact,num,ntest,i
!      REAL*4    esp,exp_pol,aux1,QQ,PIN,POUT
      REAL*4    head
      REAL*4    pres_int,temp_int,delprint,deltrint,
     *          flow,temp2,delpr1,delpr2,comp_ratio,flowm,flow_ric
      real*4    diff_cons
c
      INTEGER*4 I1,J,K,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12
      INTEGER*4 IND_ERR,INUM
cgpe      LOGICAL*2 ce_off
	integer*4 ier_app
c-----------------------------------------------------------------------
      ier = 0
c      msg_err = ' '
cgpe-corr	diff_cons = 1000.
      diff_cons = R_MAX
c
      ntest = 0
      nact = 0

c---->          calcolo le unita' necessariamente da accendere
      num = 0
      i1 = 0
      do j=1,type_num
        type_act_opt(j) = 0
        do k=1,type_quant_opt(j)
          if(unit_avail_opt(k+i1).gt.1) then
            num = num+1
            type_act_opt(j) = type_act_opt(j)+1
          end if
        end do
        i1 = i1+type_quant_opt(j)
      end do
c---->              devo generare tutte le possibili configurazioni
cgpe      ind = istaz
cgpe      call init_stvar(istaz,vert_pointer)

cgpec---->              riconoscimento di una centrale spenta
cgpe      if (ce_off(pres1,pres2)) then
cgpe        if (num .eq. 0) then
cgpe!-non esistono unita' necessariamente accese
cgpe          ntest = 1
cgpe          do i = 1,type_num
cgpe            ind_s(ntest,i) = 0
cgpe          enddo
cgpe          cost(ntest) = 0.
cgpe          goto 10
cgpe        else
cgpe          ntest = 0
cgpe          goto 10
cgpe        endif
cgpe      endif

c calcolo del numero minimo di unita' da accendere
c---->      grid verification
!      qq = flow2*flow2
!      delpr1 = stat_vars(3)*qq
!      delpr2 = stat_vars(4)*qq
!      pin  = pres1-delpr1
!      pout = pres2+delpr2
!      exp_pol=cesad(ind)
!      esp = (exp_pol-1.)/exp_pol
!      z = prop(pin,temp1,z_coef)
!      comp_ratio = pout/pin
!      aux1 = (erre / agrvcc / pmol) * Z * temp1/esp
!      aux2 = comp_ratio**esp
!      head = aux1 * (aux2-1.)
!      aux3 = ROCN*(1/1.e5)*(erre/pmol)*z*temp1/pin
!      flow_tot = flow2*aux3
c per ora pongo numin=1, da sostituire con il numero minimo
c effettivo di unita' da accendere quando sara' creata la griglia
c tab_grid
c      call ver_grid(flow_tot,head,dqs,dhs,type_num,numin,
c     *			tab_grid,nhmax,nqmax,ier)
      numin = 1
!      if(ier.ne.0) return
c---->                        simulation with minimal configuration
      if(num.ge.numin) then
!-check sul numero di test OK
       if (ntest.lt.max_ch) then
!        call genera_stat(tipo_criterio,vert_type,
!     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
!     *unit_ind(jufir),lstat,stat)
        call genera_stat(tipo_criterio,vert_type,
     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
        call optim_vertex(istaz,iopt,vert_type,tipo_criterio,
     *       vert_pointer,flow,flow2,pres1,pres2,temp1,temp2,
     *       air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),flowm,
     *       flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *       stat_vars,pres_int,temp_int,
     *       delprint,deltrint,ier)
        if(ier.eq.0) then
          call new_opt(tot_cons,cost,start_cost,shut_cost,opt_time,
     *unit_num,type_act_opt,ind_s,lstat,status,ntest,type_num,nact)
c calcolo il numero delle unita'attive
          nact = num
        else
c Controllo sugli errori che derivano dalla fattibilita' dei punti sulle
c fasi dei compressori
          call gest_errore(istaz,ier,tipo_criterio,vert_type,
     *stat_ord,ind_err)

c Se viene restituito ind_err non nullo, si esce dall'ottimizzazione
          if (ind_err.gt.0) then
!            ier = 1 ! ier = 11
!            msg_err = 'Non esiste ottimo centrale binaria'
!            return
            goto 10
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

!      if(ntest.eq.max_ch) goto 10
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
        if(type_avail_opt(k1).ge.type_act_opt(k1)) then
         if(inum.gt.1) then
c---->    +++  111 -----> piu' di un compressore inserito
          do k2=k1,type_num
           type_act_opt(k2) = type_act_opt(k2)+1
           if(type_avail_opt(k2).ge.type_act_opt(k2)) then
            if(inum.gt.2) then
c---->    +++  222 ----> piu' di due compressori da inserire
             do k3=k2,type_num
              type_act_opt(k3) = type_act_opt(k3)+1
              if(type_avail_opt(k3).ge.type_act_opt(k3)) then
               if(inum.gt.3) then
c---->    +++  333 ---->  piu' di tre compressori da inserire
                do k4=k3,type_num
                 type_act_opt(k4) = type_act_opt(k4)+1
                 if(type_avail_opt(k4).ge.type_act_opt(k4)) then
                  if(inum.gt.4) then
c---->    +++  444 ---->  piu' di quattro compressori da inserire
                   do k5=k4,type_num
                    type_act_opt(k5) = type_act_opt(k5)+1
                    if(type_avail_opt(k5).ge.type_act_opt(k5)) then
                     if(inum.gt.5) then
c---->    +++  555 ---->  piu' di cinque compressori da inserire
                      do k6=k5,type_num
                       type_act_opt(k6) = type_act_opt(k6)+1
                       if(type_avail_opt(k6).ge.type_act_opt(k6)) then
                        if(inum.gt.6) then
c---->    +++  666 ---->  piu' di sei compressori da inserire
                         do k7=k6,type_num
                          type_act_opt(k7) = type_act_opt(k7)+1
                          if(type_avail_opt(k7).ge.type_act_opt(k7))then
                           if(inum.gt.7) then
c---->    +++  777 ---->  piu' di sette compressori da inserire
                            do k8=k7,type_num
                             type_act_opt(k8) = type_act_opt(k8)+1
                             if(type_avail_opt(k8).ge.type_act_opt(k8))
     *                                                              then
                              if(inum.gt.8) then
c---->    +++  888 ---->  piu' di otto compressori da inserire
                               do k9=k8,type_num
                                type_act_opt(k9) = type_act_opt(k9)+1
                                if(type_avail_opt(k9).ge.
     *                              type_act_opt(k9))then
                                 if(inum.gt.9) then
c---->    +++  999 ---->  piu' di nove compressori da inserire
                                  do k10=k9,type_num
                                   type_act_opt(k10)=type_act_opt(k10)+1
                                   if(type_avail_opt(k10).ge.
     *                                  type_act_opt(k10)) then
                                    if(inum.gt.10) then
c---->    +++  10 ---->  piu' di dieci compressori da inserire
                                     do k11=k10,type_num
                                      type_act_opt(k11)=
     *                                    type_act_opt(k11)+1
                                      if(type_avail_opt(k11).ge.
     *                                     type_act_opt(k11)) then
                                       if(inum.gt.11) then
c---->    +++  11 ---->  piu' di undici compressori da inserire
                                        do k12=k11,type_num
                                         type_act_opt(k12)=
     *                                           type_act_opt(k12)+1 
                                         if(type_avail_opt(k12).
     *                                       ge.type_act_opt(k12)) then
c---->    +++  12 ---->  simulazione con dodici  compressori inseriti
                                          if((num+12).ge.numin) then
!-check sul numero di test OK
                                           if (ntest.lt.max_ch) then
                                            call init_vert_type
     *(vert_pointer,type_act_opt,vert_type)
                                            call genera_stat(
     *tipo_criterio,vert_type,unit_num,type_num,type_act(jtfir),
     *type_act_opt,type_quant_opt,lstat,stat)
                                            call optim_vertex(istaz,
     *iopt,vert_type,tipo_criterio,vert_pointer,flow,flow2,
     *pres1,pres2,temp1,temp2,air,delpr1,delpr2,comp_ratio,
     *tot_cons(ntest+1),flowm,flow_ric,head,unit_num,stat_ord,jtfir,
     *type_num,jufir,stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                                            if(ier.eq.0) then
                                             call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
!                                             if (nact.eq.0) then
!                                              do i=1,type_num
!                                               nact=nact+type_act_opt(i)
!                                              end do
!                                             end if
                                             if(ntest.eq.max_ch) goto 10
                                            end if
                                           end if

                                          end if
                                         end if
                                         type_act_opt(k12)=
     *                                      type_act_opt(k12)-1 
                                        end do
                                       else
c---->                     simulazione con 11 compressori inseriti
                                        if((num+11).ge.numin) then
                                         if (ntest.lt.max_ch) then
                                          call init_vert_type
     *(vert_pointer,type_act_opt,vert_type)
                                          call genera_stat(
     *tipo_criterio,vert_type,unit_num,type_num,type_act(jtfir),
     *type_act_opt,type_quant_opt,lstat,stat)
                                          call optim_vertex(istaz,iopt,
     *vert_type,tipo_criterio,vert_pointer,flow,flow2,pres1,
     *pres2,temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                                          if(ier.eq.0) then
                                           call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,lstat,
     *status,ntest,type_num,nact)
                                           if(ntest.eq.max_ch) goto 10
                                          end if
                                         end if

                                        end if
                                       end if
                                      end if
                                      type_act_opt(k11) =
     *                                     type_act_opt(k11)-1
                                     end do
                                    else
c---->                     simulazione con dieci compressori inseriti
                                     if((num+10).ge.numin) then
                                      if (ntest.lt.max_ch) then
                                       call init_vert_type
     *(vert_pointer,type_act_opt,vert_type)
                                       call genera_stat(tipo_criterio,
     *vert_type,unit_num,type_num,type_act(jtfir),type_act_opt,
     *type_quant_opt,lstat,stat)
                                       call optim_vertex(istaz,iopt,
     *vert_type,tipo_criterio,vert_pointer,flow,flow2,pres1,
     *pres2,temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                                       if(ier.eq.0) then
                                        call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
                                        if(ntest.eq.max_ch) goto 10
                                       end if
                                      end if

                                     end if
                                    end if
                                   end if
                                   type_act_opt(k10)=
     *                                   type_act_opt(k10)-1
                                  end do
                                 else
c---->                     simulazione con nove compressori inseriti
                                  if((num+9).ge.numin) then
                                   if (ntest.lt.max_ch) then
                                    call init_vert_type
     *(vert_pointer,type_act_opt,vert_type)
                                    call genera_stat(tipo_criterio,
     *vert_type,unit_num,type_num,type_act(jtfir),type_act_opt,
     *type_quant_opt,lstat,stat)
                                    call optim_vertex(istaz,iopt,
     *vert_type,tipo_criterio,vert_pointer,flow,flow2,pres1,
     *pres2,temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                                    if(ier.eq.0) then
                                     call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
                                     if(ntest.eq.max_ch) goto 10
                                    end if
                                   end if
                                  end if
                                 end if
                                end if
                                type_act_opt(k9) = type_act_opt(k9)-1
                               end do
                              else
c---->                     simulazione con otto compressori inseriti
                               if((num+8).ge.numin) then
                                if (ntest.lt.max_ch) then
                                 call init_vert_type(vert_pointer,
     *type_act_opt,vert_type)
                                 call genera_stat(tipo_criterio,
     *vert_type,unit_num,type_num,type_act(jtfir),type_act_opt,
     *type_quant_opt,lstat,stat)
                                 call optim_vertex(istaz,iopt,vert_type,
     *tipo_criterio,vert_pointer,flow,flow2,pres1,pres2,temp1,
     *temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                                 if(ier.eq.0) then
                                  call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
                                  if(ntest.eq.max_ch) goto 10
                                 end if
                                end if
                               end if
                              end if
                             end if
                             type_act_opt(k8) = type_act_opt(k8)-1
                            end do
                           else
c---->                     simulazione con sette compressori inseriti
                            if((num+7).ge.numin) then
                             if (ntest.lt.max_ch) then
                              call init_vert_type(vert_pointer,
     *type_act_opt,vert_type)
                              call genera_stat(tipo_criterio,
     *vert_type,unit_num,type_num,type_act(jtfir),type_act_opt,
     *type_quant_opt,lstat,stat)
                              call optim_vertex(istaz,iopt,vert_type,
     *tipo_criterio,vert_pointer,flow,flow2,pres1,pres2,
     *temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                              if(ier.eq.0) then
                               call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
                               if(ntest.eq.max_ch) goto 10
                              end if
                             end if

                            end if
                           end if
                          end if
                          type_act_opt(k7) = type_act_opt(k7)-1
                         end do
                        else
c---->                     simulazione con sei compressori inseriti
                         if((num+6).ge.numin) then
                          if (ntest.lt.max_ch) then
                           call init_vert_type(vert_pointer,
     *type_act_opt,vert_type)
                           call genera_stat(tipo_criterio,
     *vert_type,unit_num,type_num,type_act(jtfir),type_act_opt,
     *type_quant_opt,lstat,stat)
                           call optim_vertex(istaz,iopt,vert_type,
     *tipo_criterio,vert_pointer,flow,flow2,pres1,pres2,
     *temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                           if(ier.eq.0) then
                            call new_opt(tot_cons,cost,
     *start_cost,shut_cost,opt_time,unit_num,type_act_opt,ind_s,
     *lstat,status,ntest,type_num,nact)
                            if(ntest.eq.max_ch) goto 10
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
                       if (ntest.lt.max_ch) then
                        call init_vert_type(vert_pointer,type_act_opt,
     *vert_type)
                        call genera_stat(tipo_criterio,vert_type,
     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
                        call optim_vertex(istaz,iopt,vert_type,
     *tipo_criterio,vert_pointer,flow,flow2,pres1,pres2,
     *temp1,temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                        if(ier.eq.0) then
                         call new_opt(tot_cons,cost,start_cost,
     *shut_cost,opt_time,unit_num,type_act_opt,ind_s,lstat,
     *status,ntest,type_num,nact)
                         if(ntest.eq.max_ch) goto 10
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
                    if (ntest.lt.max_ch) then
                     call init_vert_type(vert_pointer,type_act_opt,
     *vert_type)
                     call genera_stat(tipo_criterio,vert_type,
     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
                     call optim_vertex(istaz,iopt,vert_type,
     *tipo_criterio,vert_pointer,flow,flow2,pres1,pres2,temp1,
     *temp2,air,delpr1,delpr2,comp_ratio,tot_cons(ntest+1),
     *flowm,flow_ric,head,unit_num,stat_ord,jtfir,type_num,jufir,
     *stat_vars,pres_int,temp_int,delprint,deltrint,ier)
                     if(ier.eq.0) then
                      call new_opt(tot_cons,cost,start_cost,shut_cost,
     *opt_time,unit_num,type_act_opt,ind_s,lstat,status,ntest,type_num,
     *nact)
                      if(ntest.eq.max_ch) goto 10
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
                 if (ntest.lt.max_ch) then
                  call init_vert_type(vert_pointer,type_act_opt,
     *vert_type)
                  call genera_stat(tipo_criterio,vert_type,
     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
                  call optim_vertex(istaz,iopt,vert_type,tipo_criterio,
     *vert_pointer,flow,flow2,pres1,pres2,temp1,temp2,air,
     *delpr1,delpr2,comp_ratio,tot_cons(ntest+1),flowm,flow_ric,head,
     *unit_num,stat_ord,jtfir,type_num,jufir,stat_vars,pres_int,
     *temp_int,delprint,deltrint,ier)
                  if(ier.eq.0) then
                   call new_opt(tot_cons,cost,start_cost,shut_cost,
     *opt_time,unit_num,type_act_opt,ind_s,lstat,status,ntest,type_num,
     *nact)
                   if(ntest.eq.max_ch) goto 10
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
              if (ntest.lt.max_ch) then
               call init_vert_type(vert_pointer,type_act_opt,vert_type)
               call genera_stat(tipo_criterio,vert_type,
     *unit_num,type_num,type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
               call optim_vertex(istaz,iopt,vert_type,tipo_criterio,
     *vert_pointer,flow,flow2,pres1,pres2,temp1,temp2,air,
     *delpr1,delpr2,comp_ratio,tot_cons(ntest+1),flowm,flow_ric,head,
     *unit_num,stat_ord,jtfir,type_num,jufir,stat_vars,pres_int,
     *temp_int,delprint,deltrint,ier)
               if(ier.eq.0) then
                call new_opt(tot_cons,cost,start_cost,shut_cost,
     *opt_time,unit_num,type_act_opt,ind_s,lstat,status,ntest,type_num,
     *nact)
                if(ntest.eq.max_ch) goto 10
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
           if (ntest.lt.max_ch) then
            call init_vert_type(vert_pointer,type_act_opt,vert_type)
            call genera_stat(tipo_criterio,vert_type,unit_num,type_num,
     *type_act(jtfir),type_act_opt,type_quant_opt,
     *lstat,stat)
            call optim_vertex(istaz,iopt,vert_type,tipo_criterio,
     *vert_pointer,flow,flow2,pres1,pres2,temp1,temp2,air,
     *delpr1,delpr2,comp_ratio,tot_cons(ntest+1),flowm,flow_ric,head,
     *unit_num,stat_ord,jtfir,type_num,jufir,stat_vars,
     *pres_int,temp_int,delprint,deltrint,ier)
            if(ier.eq.0) then
             call new_opt(tot_cons,cost,start_cost,shut_cost,opt_time,
     *unit_num,type_act_opt,ind_s,lstat,status,ntest,type_num,nact)
             if(ntest.eq.max_ch) goto 10
            else
c Se una unita' risulta non avere punti fattibili sulle due fasi, non viene
c piu' simulata, in particolare se type_num=1 si esce
cgpe
             ier_app = 0
cgpe-prv             call gest_errore(istaz,ier,tipo_criterio,vert_type,
cgpe-prv     *            stat_ord,ier_app)
cgpe-prv	       if (ier_app.gt.0) then
cgpe-prv	        type_avail_opt(k1) = 0
cgpe-prv	        if (type_num.eq.1) then
cgpe-prv	         goto 10
cgpe-prv	        endif
cgpe-prv             end if
cgpe-end
            end if
           end if

          end if
         end if
        end if
        type_act_opt(k1) = type_act_opt(k1)-1
       end do
c La parte iterativa ha termine quando ho raggiunto inum=nact+2, o
c e' terminato il loop sulle unita'
       if ((ntest.gt.0).and.((inum+num).eq.(nact+max_test))) then
         ier=0
         go to 10
       end if
      end do
c
10    continue
c
cgpe      if (fl_cons) then
cgpec correzione del consumo, viene scelta la configurazione che minimizza
cgpec la differenza di consumo trovato con il consumo dato in input
c fl_cons = .T. => cons_imp = consty
c correzione del consumo, viene scelta la configurazione che minimizza
c la differenza di consumo trovato con il consumo dato in input
c fl_cons = .F. => cons_imp = 0.
c viene scelta la configurazione che minimizza il consumo
      ind_opt = 0
      if (ntest.gt.0) then
        ier = 0
        do i = 1,ntest
          if (cost(i) .gt. 0.  .and.
     *        abs(cost(i)-cons_imp).lt.diff_cons) then
	      diff_cons = cost(i)-cons_imp
            ind_opt = i
          end if
        end do
        if (ind_opt .gt. 0) then
          consumo = cost(ind_opt)
c-mf03
          do while (consumo .ge. soglia_cons)
            consumo = consumo - soglia_cons
          enddo
c-mf03-end
        else
C la simulazione non ha trovato soluzione
          IER = 1 ! IER = 11
c          msg_err = 'Non esiste ottimo centrale BINARIA'
        endif
      endif
cgpe      else
cgpe!-viene scelta la configurazione che minimizza il consumo
cgpe        if (ntest.eq.1) then
cgpe          ier = 0
cgpe          ind_opt = 1
cgpe          consumo = cost(ind_opt)
cgpe        else if (ntest.gt.1) then
cgpe          ier = 0
cgpe          ind_opt = 1
cgpe          do i = 2,ntest
cgpe            if (cost(i).le.cost(ind_opt) .and. cost(i).ge.0) then
cgpe              ind_opt = i
cgpe            end if
cgpe          end do
cgpe          consumo = cost(ind_opt)
cgpe        endif
cgpe      endif
c
      if (fl_corr_mf .and. ier.eq.0) then
c salvataggio nel caso di correzione del punto per due assetti ottimizzati separatamente
        do i = 1,unit_num
          iunit_corr(i) = ind_s(ind_opt,i)
        enddo
      endif
c
      return
      end
c
cgpe      Subroutine ver_grid(flow_tot,head,dqs,dhs,ntp,
cgpe     *		numin,tab_grid,nhmax,nqmax,ier)
cgpec
cgpec	verifica l'ammissibilita' del funzionamento con la configurazione 
cgpec	proposta
cgpec
cgpe      implicit none
cgpe
cgpe      include '../inc/param.inc'
cgpe      include '../inc/stazione.inc'
cgpe      include '../inc/rk_param.INC'
cgpec
cgpe      REAL*4     FLOW_TOT,HEAD,DQS,DHS
cgpe      INTEGER*4  ntp
cgpe      INTEGER*4  NHMAX,NQMAX
cgpe      INTEGER*4  numin
cgpe      INTEGER*4  tab_grid(nhmax,nqmax)
cgpec
cgpe      INTEGER*4  IER,JTH,JTQ,NT,NT1,NTAB,NTAB1
cgpe      INTEGER*4  I,N,J,J1,ITAB,K
c----------------------------------------------------------------------
c
cgpe	ier = 0
cgpe	jth = int(head/dhs)+1
cgpe	if(jth.gt.nhmax) then
cgpe	  ier = 4
cgpe	  return
cgpe	end if
cgpe	jtq = int(flow_tot/dqs)+1
cgpe	if(jtq.gt.nqmax) then
cgpe	  ier = 4
cgpe	  return
cgpe	end if
cgpe	if(tab_grid(jth,jtq).gt.0) then
cgpe	  nt = tab_grid(jth,jtq)
cgpe	  nt1 = tab_grid(jth+1,jtq)
cgpe	  if(nt1.gt.0) then
cgpe            ntab=0
cgpe            ntab1=0
cgpe	    do i=1,ntp
cgpe	      n = 10**i
cgpe!	      j = imod(nt,n)
cgpe!	      j1 = imod(nt1,n)
cgpe	      j = jmod(nt,n)
cgpe	      j1 = jmod(nt1,n)
cgpe	      ntab = ntab + j*10/n
cgpe	      ntab1 = ntab1 + j1*10/n
cgpe	      nt = nt-j
cgpe	      nt1 = nt1-j1
cgpe	    end do
cgpe	    nt = tab_grid(jth,jtq)
cgpe	    nt1 = tab_grid(jth+1,jtq)
cgpe            if (ntab.gt.ntab1) then
cgpe   	      nt = nt1
cgpe            else if (nt.gt.nt1) then
cgpe              nt = nt1
cgpe            endif
cgpe          endif
cgpe	  numin = 0
cgpe	  do i=1,ntp
cgpe	    n = 10**i
cgpe!	    j = imod(nt,n)
cgpe	    j = jmod(nt,n)
cgpe	    itab = j*10/n
cgpe	    numin = numin+itab
cgpe	    nt = nt-j
cgpe	  end do
cgpe	else
cgpe	  do k=jth+1,nhmax
cgpe	    if(tab_grid(k,jtq).gt.0) then
cgpe	      if(k.eq.(jth+1)) then
cgpe	        nt = tab_grid(k,jtq)
cgpe	        numin = 0
cgpe	        do i=1,ntp
cgpe	          n = 10**i
cgpe!	          j = imod(nt,n)
cgpe	          j = jmod(nt,n)
cgpe	          itab = j*10/n
cgpe	          numin = numin+itab
cgpe	          nt = nt-j
cgpe	        end do
cgpe	      else
cgpe	        ier = 2
cgpe	      end if
cgpe	      return
cgpe	    end if
cgpe	  end do
cgpe	  ier = 4
cgpe	end if
cgpe	return
cgpe	end

      Subroutine new_opt(tot_cons,cost,start_cost,shut_cost,opt_time,
     *unit_num,type_act,ind_s,lstatus,status,ntest,type_num,nact)
c
c  Viene calcolato il costo della simulazione come il totale dei consumi
c  piu' il costo di accensione o spegnimento, se opt_time e' diverso da
c  zero
c  I valori di lstatus e di status indicano se e' necessario aggiungere
c  un costo di accensione o spegnimento
c  Viene calcolato il numero delle unita'attive nact se nact=0
c
      implicit none

      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'
      include '../inc/rk_param.INC'
      include '../inc/OTC_PAR.INC'
C
      integer*4 unit_num,type_act(*)
      INTEGER*4 ind_s(max_ch,maxunits)

      REAL*4    start_cost(*),shut_cost(*),
     *          tot_cons(*),cost(*),opt_time
      logical*2 lstatus(*)
      character*3 status_app ! stato dell'unita' dell'attuale configurazione
      character*3 status(*)  ! stato dell'unita' da CIN

      INTEGER*4 NTEST,I,IND_UNI
      INTEGER*4 type_num,
     *          nact         ! I/O) numero di unita' attive
c----------------------------------------------------------------------
c
      ntest = ntest+1
      cost(ntest) = 0.
c calcolo il costo di spegnimeto e di accensione solo se opt_time e'
c diverso da zero 
      if (opt_time.ne.0) then
        do i=1,unit_num
          if (lstatus(i)) then
            status_app = funz_on
          else
            status_app = funz_off
          endif
!          if(lstatus(i).and.(status(I).eq.funz_off)) then
!!            cost(ntest) = cost(ntest)+(start_cost(i)/QRIF)/opt_time
!	      cost(ntest) = cost(ntest)+(start_cost(i)/opt_time)
!          else if(.not.lstatus(i).and.(status(I).eq.funz_on)) then
!!            cost(ntest) = cost(ntest)+(shut_cost(i)/QRIF)/opt_time
!	      cost(ntest) = cost(ntest)+(shut_cost(i)/opt_time)
!          end if
          call calc_cons_tot (status(i),status_app,
     *         start_cost(i),shut_cost(i),opt_time,cost(ntest))

        end do
      end if
      cost(ntest) = cost(ntest)+tot_cons(ntest)
c
      do ind_uni = 1,maxunits
        ind_s(ntest,ind_uni) = type_act(ind_uni)
      enddo
c
c calcolo il numero delle unita'attive nact se nact=0
      if (nact .eq. 0) then
        do i = 1,type_num
          nact = nact+type_act(i)
        end do
      end if
c
      return
      end

!      Subroutine genera_stat (tipo_criterio,vert_type,unit_num,
!     *           type_num,type_act,type_act_opt,type_quant_opt,
!     *           unit_ind,lstat,stat)
      Subroutine genera_stat (tipo_criterio,vert_type,unit_num,
     *           type_num,type_act,type_act_opt,type_quant_opt,
     *           lstat,stat)
C
C       Chiama le opportune subroutine per la generazione del vettore
C       delle unita' accese in base al criterio e al tipo di centrale
C
      implicit none
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'

      INTEGER*4 tipo_criterio,vert_type,unit_num,type_num,
     *          type_quant_opt(*),type_act_opt(*),
     *          type_act(*)
!     *         ,unit_ind(*)
      LOGICAL*2   lstat(*)
      character*3 stat(*)

      INTEGER*4 I
c----------------------------------------------------------------------


      if ((tipo_criterio.eq.crit_giri) .and.
     *    (vert_type.eq.tipo_comp)          ) then
!        call comp_gen_stat (unit_num,type_num,type_act_opt,
!     *type_quant_opt,unit_ind,lstat,stat)
        call comp_gen_stat (unit_num,type_num,type_act_opt,
     *type_quant_opt,lstat,stat)
      else
!        call gen_stat (unit_num,type_num,type_act_opt,type_quant_opt,
!     *unit_ind,lstat,stat)
        call gen_stat (unit_num,type_num,type_act_opt,type_quant_opt,
     *lstat,stat)
      end if



c	
      
!-viene settato il vettore delle unita' attive
      do i = 1,type_num
        type_act(i) = type_act_opt(i)
      end do



      return
      end

!      Subroutine gen_stat (unit_num,type_num,type_act,type_quant,
!     *           unit_ind,lstatus,status)
      Subroutine gen_stat (unit_num,type_num,type_act,type_quant,
     *           lstatus,status)
C
C	Genera il vettore delle unita' accese per ogni centrale
C       con criterio di equidistanza, uguaglianza di portata e
C       per stazioni semplici con il criterio dei giri
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'

      INTEGER*4 unit_num,type_num,type_act(*),type_quant(*)
!     *         ,unit_ind(*)
      LOGICAL*2   lstatus(*)    ! (1:maxunits)
      CHARACTER*3 status(*)     ! (1:maxunits)

      INTEGER*4 I,I1,K
c----------------------------------------------------------------------

      

	

cgpe-staz      do i=1,unit_num
       
      do i=1,maxunits
       lstatus(i) = .false.
       status(i)  = funz_off
              
      end do
      i1 = 0
	
                

      do i=1,type_num

        do k=1,type_act(i)                   


!          j = unit_ind(k+i1)
!          lstatus(j) = .true.
!          status(j)  = funz_on
          
          lstatus(k+i1) = .true.
          status(k+i1)  = funz_on
         
        end do

        i1 = i1+type_quant(i)

C
              

      end do
     
     
  

      return
      end

!      Subroutine comp_gen_stat (unit_num,type_num,type_act,
!     *           type_quant,unit_ind,lstatus,status)
      Subroutine comp_gen_stat (unit_num,type_num,type_act,
     *           type_quant,lstatus,status)
C
C       Genera il vettore delle unita' accese per centrali con
C       criterio dei giri per stazioni composte
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'
*&*
      INTEGER*4 unit_num,type_num,type_act(*),type_quant(*)
!     *         ,unit_ind(*)
      LOGICAL*2   lstatus(*)
      CHARACTER*3 status(*)

      INTEGER*4 I,I1,K
c-----------------------------------------------------------------------
cgpe-staz      do i=1,unit_num
      do i=1,maxunits
        lstatus(i) = .false.
        status(i) = funz_off
      end do
c
      i1 = 0
      do i = 1,type_num
!        if (type_act(i) .gt. 0 ) then
          do k=1,type_act(i)
!            j = i1+k 
!            lstatus(j) = .true.
!            status(j) = funz_on
            lstatus(i1+k) = .true.
            status(i1+k)  = funz_on
          end do
!        endif
        i1 = i1+type_quant(i)
      end do
c
      return
      end

      Subroutine genera_stat_new (unit_num,type_num,type_act,
     *           type_act_opt,type_quant,lstat,stat)
C
C	Genera il vettore delle unita' accese per ogni centrale
C
C NOTA: Rispetto alla subroutine genera_stat e' necessario togliere i
C       campi tipo_criterio e vert_type
C
      implicit none
      include '../inc/param.inc'
      include '../inc/STAZIONE.INC'

      INTEGER*4 unit_num            ! numero di unita'
     *         ,type_num            ! numero di tipi diversi di unita'
     *         ,type_quant(*)       ! numero di unita' per ciascun tipo
     *         ,type_act_opt(*)     ! numero di unita' attive per ciascun tipo
                                    ! per la configurazione da provare
     *         ,type_act(*)         ! numero di unita' attive per ciascun tipo
                                    ! da passare in area common
      LOGICAL*2   lstat(*)
      character*3 stat(*)

      INTEGER*4 I,i1,k
c----------------------------------------------------------------------
cgpe-staz      do i = 1,unit_num
      do i=1,maxunits
        lstat(i) = .false.
        stat(i)  = funz_off
      end do
      i1 = 0
      do i = 1,type_num
        do k=1,type_act_opt(i)
          lstat(k+i1) = .true.
          stat(k+i1)  = funz_on
        end do
        i1 = i1+type_quant(i)
      end do

!-viene settato il vettore delle unita' attive
      do i = 1,type_num
        type_act(i) = type_act_opt(i)
      end do
c
      return
      end




c--------------------------



      Subroutine gen_stat_wr(unit_num,type_num,type_act,type_quant,
     *           lstatus,status)
C
C	Genera il vettore delle unita' accese per ogni centrale
C       con criterio di equidistanza, uguaglianza di portata e
C       per stazioni semplici con il criterio dei giri
C
      implicit none
      include '../inc/param.inc'
      include '../inc/stazione.inc'

      INTEGER*4 unit_num,type_num,type_act(*),type_quant(*)
!     *         ,unit_ind(*)
      LOGICAL*2   lstatus(*)    ! (1:maxunits)
      CHARACTER*3 status(*)     ! (1:maxunits)

      INTEGER*4 I,I1,K
c----------------------------------------------------------------------

      

cgpe-staz      do i=1,unit_num
        write (120,*)'                      prima'
      do i=1,maxunits
       lstatus(i) = .false.
       status(i)  = funz_off
              

      end do
      i1 = 0
	
              


              

      do i=1,type_num

	           

        do k=1,type_act(i)

   
                  


!          j = unit_ind(k+i1)
!          lstatus(j) = .true.
!          status(j)  = funz_on
          
          lstatus(k+i1) = .true.
          status(k+i1)  = funz_on
                     	

        end do
        i1 = i1+type_quant(i)

	        

      end do
     


	  
	


      return
      end