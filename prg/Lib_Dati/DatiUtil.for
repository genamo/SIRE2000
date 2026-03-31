!==============================================================
!-Routine di uso generale  
!==============================================================
      subroutine cerca_connessione (pu_id,pos_pu,*)

!-Cerca la posizione per un identificativo di connessione
!-nel tabellone elementi(m_ogg) fra i punti di rete
	implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'

	integer*4 pu_id     !I) elem_id del punto di connessione
	integer*4 pos_pu    !O) posizione elemento nel tabellone punti
!--------------------------------------------------------------	
      do pos_pu=otipti(e_pu),etipti(e_pu)
         if(pu_id.eq.elem_id(pos_pu))then
            return
         endif
      enddo

      return 1
      end
!==============================================================
      subroutine stringa_intero (ival,pos_in,pos_fi,stringa)

!-Converte un valore intero in una stringa.
!-Restituisce da sinistra la posizione del primo e ultimo carattere in stringa
      implicit none
      integer*4 ival        !I) valore intero da convertire
      integer*4 pos_in      !O) posizione primo carattere valore
      integer*4 pos_fi      !O) posizione ultimo carattere valore
      character*(*) stringa !O) stringa contenente il numero
      integer*4 iel
!------------------------------
      write(stringa,'(i20)',err=999) ival

      do iel=1,len_trim(stringa)
         if(stringa(iel:iel).ne.' ')then
            pos_in=iel
            pos_fi=len_trim(stringa)
            goto 1000
         endif
      enddo
999   continue
	stringa=' '
      pos_in=1
      pos_fi=1

1000  continue
      return
      end
!==============================================================
      subroutine msgerr_key (ival,str_des,str_err)

!-Restituisce un messaggio str_err con ival in fondo
      implicit none
      integer*4 ival         !I) intero
      character*(*) str_des  !I) messaggio
      character*(*) str_err  !O) messaggio + intero in fondo
      integer*4 pos_in,pos_fi !pos iniziale e finale di stringa intero
      integer*4 pf,s_trim_d
      character*(50) stringa
!------------------------------
      call stringa_intero (ival,pos_in,pos_fi,stringa)
      pf = s_trim_d(str_des)
      str_err = str_des(1:pf)//' '//stringa(pos_in:pos_fi)

      return
      end
!==============================================================
      subroutine pos_elem_tipo (tipo,elemId,pos,*)

!-Restituisce la posizione di elemId nel tabellone del tipo scelto
	implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
	integer*4 tipo    !I) tipo elemento da cercare
	integer*4 elemId  !I) elemento da cercare
	integer*4 pos     !IO) pos di elemId nel tabellone
	integer*4 iel
!--------------------------------------------------------------	
      if(elem_id(pos).eq.elemId)then
         return
      endif
      do pos=otipti(tipo),etipti(tipo)
         if(elem_id(pos).eq.elemId)then
            return
         endif
      enddo

      return 1
      end
!==============================================================
      subroutine connPuntoFittizio (bInserisci,bMonte,jel,pgr)

      implicit none
      include '../inc/param.inc'   !parametri
      include '../inc/tx.inc'
      include '../inc/ti.inc'
      include '../inc/tj.inc'
      include '../inc/stations.inc'
      include '../inc/default.inc'

      logical*2 bInserisci !I)
      logical*2 bMonte     !I)
      integer*4 jel        !I) indice nel tabellone 1:M_OGG
      integer*4 pgr        !I) pgrato o pgratoApp, secondo il chiamante
!---------------------------------------------------------
      if (bInserisci) then
         !Inserisco 1 punto fittizo:
          etipti(e_pu) = etipti(e_pu) + 1
          pgrato(etipti(e_pu)) = etipti(e_pu) - otipti(e_pu) + 1    
          htipto(etipti(e_pu)) = e_pu
      endif

      if (bMonte) then
        !Connessioni dell'elemento bipolare al punto fittizio:
         if (bInserisci) then
            opumto(jel) = etipti(e_pu)
           !Inizializzazione TOP sul punto
            pmaxtx(opumto(jel)) = max_pout(pgr)
            tambtx(opumto(jel)) = tasp_stat (pgr)

ccc		 !Inizializzazione CIN sul punto
ccc            prestj(opumto(jel)) = pasp_stat (pgr)
ccc            temptj(opumto(jel)) = tasp_stat (pgr)
         else
	      opumto(jel) = opumto(jel-1)
         endif

      else
        !Connessioni dell'elemento bipolare al punto fittizio:
         if (bInserisci) then
		  opuvto(jel) = etipti(e_pu)

           !Inizializzazione TOP sul punto
            pmaxtx(opuvto(jel)) = max_pout(pgr)
            tambtx(opuvto(jel)) = temp_def

ccc           !Inizializzazione CIN sul punto
ccc            prestj(opuvto(jel)) = pmand_stat(pgr)
ccccgpe		temptj(opuvto(jel)) = tmand_stat(pgr)
ccccgpe la temperatura di mandata di centrale è un dato calcolato e non un CIN
ccc            temptj(opuvto(jel)) = temp_def

         else
            opuvto(jel) = opuvto(jel-1)
         endif
      endif

      return
      end
!==============================================================
      subroutine open_vfile (unita,cartella,nomefile,modo,stato,*)

!-Open per file dati input e output di estensione txt
      implicit none
      include '../inc/param.inc'
      include '../inc/scenario.inc'


      integer*4     unita    !I
      character*(*) cartella !I
      character*(*) nomefile !I
      character*(*) modo     !I status
      integer*4 stato        !O
!-----------------
      integer*4 n_charcamp
      parameter (n_charcamp = 5)
      character*(n_charcamp) str_camp
      character*(1) str_punto
!--------------------------------- 
!-flag per scrittura dei risultati della rete
      if (fl_est.eq.0) then
	  str_camp=""
	  str_punto=""
	else
        write (str_camp,'(i<n_charcamp>.<n_charcamp>)') ultimo_camp
	  str_punto="."
      endif


      stato=0
      open (unita,file=trim(cartella)//trim(nomefile)
     -                 //str_punto//str_camp,
     - blocksize=512*8,access='SEQUENTIAL',status=modo,
     - iostat=stato,err=9999)
      return

9999  continue
      return 1
      end
!==============================================================
      subroutine read_i (stringa,pos_i,dato,nullo,*)

!-Legge un dato integer su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva lettura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I
      integer*4     pos_i   !I/O
      integer*4     dato    !O
      logical*2     nullo   !O
      integer*4 pos,pos_f,stato
!------------------------------ 
1000  format(I)
!------------------------------ 
!-Definisce posizione finale dato, escluso separatore
      nullo = .false.
      do pos = pos_i,len(stringa)
         if(stringa(pos:pos).eq.term_campi) goto 100
      enddo
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_i','Manca separatore dato: '
     - //stringa(pos_i:pos_i+20),0)
      goto 9999 !*errore

100   continue
      pos_f = pos-1
      if(pos_f.lt.pos_i)then
         nullo = .true.
         dato = 0
      else
         read(stringa(pos_i:pos_f),1000,err=9998,iostat=stato) dato
      endif
      pos_i = pos_f + 2
      return

9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_i',
     - 'Dato errato: '//stringa(pos_i:pos_f),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine read_r (stringa,pos_i,dato,nullo,*)

!-Legge un dato real su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva lettura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I
      integer*4     pos_i   !I/O
      real*4        dato    !O
      logical*2     nullo   !O
      integer*4 pos,pos_f,stato
!------------------------------ 
!-Definisce posizione finale dato, escluso separatore
      nullo = .false.
      do pos = pos_i,len(stringa)
         if(stringa(pos:pos).eq.term_campi) goto 100
      enddo
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_r','Manca separatore dato: '
     - //stringa(pos_i:pos_i+20),0)
      goto 9999 !*errore
      
100   continue
      pos_f = pos-1
      if(pos_f.lt.pos_i)then
         nullo = .true.
         dato = 0
      else
         read(stringa(pos_i:pos_f),*,err=9998,iostat=stato) dato
      endif
      pos_i = pos_f + 2
      return

9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_r',
     - 'Dato errato: '//stringa(pos_i:pos_f),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine read_c (stringa,pos_i,dato,*)

!-Legge un dato character su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva lettura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I
      integer*4     pos_i   !I/O
      character*(*) dato    !O
      integer*4 pos,pos_f,stato
!------------------------------ 
1000  format(<pos_f-pos_i+1>A)
!------------------------------ 
!-Definisce posizione finale dato, escluso separatore
      do pos = pos_i,len(stringa)
         if(stringa(pos:pos).eq.term_campi) goto 100
      enddo
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_c','Manca separatore dato: '
     - //stringa(pos_i:pos_i+20),0)
      goto 9999 !*errore
      
100   continue
      pos_f = pos-1
      if(pos_f.lt.pos_i)then
         dato = ' '
      else
         read(stringa(pos_i:pos_f),1000,err=9998,iostat=stato) dato
      endif
      pos_i = pos_f + 2
      return

9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Read_c',
     - 'Dato errato: '//stringa(pos_i:pos_f),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine write_i (stringa,pos_i,dato,*)

!-Scrive un dato integer su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva scrittura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I/O
      integer*4     pos_i   !I/O
      integer*4     dato    !I
      integer*4 p1,p2,ps1,ps2,stato,l_strapp,s_trim_s,s_trim_d
      parameter (l_strapp=15)
      character*(l_strapp) strapp
!------------------------------ 
1000  format(I)
1100  format(A)
!------------------------------ 
      p1 = pos_i
      if(dato.ne.I_UND)then
         write(strapp,1000,err=9997,iostat=stato) dato
         ps1 = s_trim_s(strapp)
         ps2 = s_trim_d(strapp)
         p2 = p1 + ps2 - ps1
         write(stringa(p1:p2),1100,err=9998,iostat=stato)
     -         strapp(ps1:ps2)
         p1 = p2 + 1
      endif
      write(stringa(p1:p1),1100,err=9998,iostat=stato) term_campi
      pos_i = p1 + 1
      return

9997  continue
      ps1=1
      ps2=1
      strapp(ps1:ps2)=' '
9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Write_i',
     - 'Dato errato: '//strapp(ps1:ps2),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine write_r (stringa,pos_i,dato,*)

!-Scrive un dato real su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva scrittura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I/O
      integer*4     pos_i   !I/O
      real*4        dato    !I
      integer*4 p1,p2,ps1,ps2,stato,l_strapp,s_trim_s,s_trim_d
      parameter (l_strapp=30)
      character*(l_strapp) strapp
!------------------------------ 
1000  format(G)
1100  format(A)
!------------------------------ 
      p1 = pos_i
      if(dato.ne.R_UND)then
         write(strapp,1000,err=9997,iostat=stato) dato
         ps1 = s_trim_s(strapp)
         ps2 = s_trim_d(strapp)
         p2 = p1 + ps2 - ps1
         write(stringa(p1:p2),1100,err=9998,iostat=stato)
     -         strapp(ps1:ps2)
         p1 = p2 + 1
      endif
      write(stringa(p1:p1),1100,err=9998,iostat=stato) term_campi
      pos_i = p1 + 1
      return

9997  continue
      ps1=1
      ps2=1
      strapp(ps1:ps2)=' '
9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Write_r',
     - 'Dato errato: '//strapp(ps1:ps2),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine write_c (stringa,pos_i,dato,*)

!-Scrive un dato character su stringa, tra pos_i e separatore.
!-Restituisce pos_i per successiva scrittura.
      implicit none
      include '../inc/param.inc'

      character*(*) stringa !I/O
      integer*4     pos_i   !I/O
      character*(*) dato    !I
      integer*4 p1,p2,ps1,ps2,stato,s_trim_d
!------------------------------ 
1100  format(A)
!------------------------------ 
      p1 = pos_i
      ps2 = s_trim_d(dato)
      if(ps2.ne.0)then
        ps1 = 1
        p2 = p1 + ps2 - ps1
        write(stringa(p1:p2+2),1100,err=9998,iostat=stato)
     -        dato(ps1:ps2)
        p1 = p2 + 1
      endif
      write(stringa(p1:p1),1100,err=9998,iostat=stato) term_campi
      pos_i = p1 + 1
      return

9998  continue
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Write_c',
     - 'Dato errato: '//dato(ps1:ps2),stato)
9999  continue
      return 1
      end
!==============================================================
      subroutine scomponi_lun_tr (str_err,*)

!-Scompone i tronchi con lunghezza > max (50 km.) in sottotronchi,
!-creando tronchi fittizi e punti fittizi 
      implicit none
      include '../inc/param.inc'
      include '../inc/default.inc'
      include '../inc/dimension.inc'
      include '../inc/ti.inc'
      include '../inc/tv.inc'
      include '../inc/top_app_ele.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/scomp_tronchi.inc'

      character*(*) str_err  !O) messaggio errore

      integer*4 pos,nro_tro,nro_fit,iel,jel,pf2,s_trim_d
!===========================================
!-Definizione iniziale
      tr_flag_scomp = .false.      !Flag scomposizione (false/true = no/si)
!===========================================
!-Test lunghezza max tronchi > 0
      if(tr_lunmax.le.0.)then
         return
      endif
!------------------------------------
!-Salvataggio originale etipti di tronchi e punti
      tr_ori_etipti = etipti(e_tr)
      pu_ori_etipti = etipti(e_pu)
!------------------------------------
!-Ricerca tronchi con lunghezza > max
!------------------------------------
      do pos=otipti(e_tr),tr_ori_etipti

         tr_ori_lun(pgrato(pos)) = 0. !=0 per indicare tronco non scomposto

         if(rluntx(pos).gt.tr_lunmax)then

!------------------------------------
!-Calcolo numero tronchi (nro_tro) componenti il tronco scomposto
!-nro_tro: comprende tronco padre scomposto
!------------------------------------
            if(amod(rluntx(pos),tr_lunmax).ne.0.)then
               nro_tro = int(rluntx(pos)/tr_lunmax) + 1
            else
               nro_tro = int(rluntx(pos)/tr_lunmax)
            endif
            nro_fit=nro_tro-1
!------------------------------------
!-Test se numero fittizi (nro_fit) tronchi e punti creati > relativi max
!-nro_fit tronchi: non comprende tronco padre
!-nro_fit punti:   non comprende punto monte padre
!------------------------------------
            if(((etipti(e_tr)-otipti(e_tr)+1)+nro_fit).gt.max_tr)then
               goto 9997 !*errore: tronchi > max
            endif
            if(((etipti(e_pu)-otipti(e_pu)+1)+nro_fit).gt.max_pu)then
               goto 9998 !*errore: punti > max
            endif
!------------------------------------
!-Salvataggio valori originali tronco padre
!------------------------------------
            tr_ori_lun(pgrato(pos)) = rluntx(pos) !*lunghezza originale
            tr_ori_pmo(pgrato(pos)) = opumto(pos) !*punto monte originale
!------------------------------------
            do iel=1,nro_fit

               etipti(e_tr) = etipti(e_tr)+1 !*incremento tronchi
               etipti(e_pu) = etipti(e_pu)+1 !*incremento punti
!------------------------------
               tr_new_etipti = etipti(e_tr)  !& definizione nuova etipti tronchi
               pu_new_etipti = etipti(e_pu)  !& definizione nuova etipti punti
!------------------------------------
!-Dati_Primari nuovo Tronco fittizio 
!------------------------------------
               elem_id(etipti(e_tr)) = -etipti(e_tr) !*
               snomtv (etipti(e_tr)) = snomtv(pos)   !*nome tr_padre
               htipto (etipti(e_tr)) = e_tr          !*
               pgrato (etipti(e_tr)) = (etipti(e_tr)-otipti(e_tr))+1 !*
!-Salvataggio puntatore a tronco padre in tr_ori_pmo (inutilizzato per tronchi originali)
               tr_ori_pmo(pgrato(etipti(e_tr))) = pos
!------------------------------------
!-Creazione di tr_fit con pu_fit valle da sinistra verso destra.
!-Primo tr_fit con pu_monte originale e pu_valle fit. 
!-Eventuale successivo tr_fit con pu_monte = pu_valle di tr_fit precedente
! e pu_valle fit. 
!-Tronco padre posto ultimo componente con
! pu_monte = pu_valle di tr_fit precedente e pu_valle di originale padre. 
!------------------------------------
               if(iel.eq.1)then
!-tr_fit con punto_monte di tronco_padre all'inizio
                  opumto(etipti(e_tr)) = opumto(pos)
               else
!-tr_fit con punto_monte = punto_valle di tr_fit precedente
                  opumto(etipti(e_tr)) = opuvto(etipti(e_tr)-1)
               endif
!-tr_fit con punto_valle = pu_fit
               opuvto(etipti(e_tr)) = etipti(e_pu)
!------------------------------------
!-Dati_Top_nuovo_Tronco fittizio
!------------------------------------
               diamtx(etipti(e_tr)) = diamtx(pos) !* 
               rugotx(etipti(e_tr)) = rugotx(pos) !*
               conntx(etipti(e_tr)) = conntx(pos) !*
!-lunghezza nuovo tronco
               rluntx(etipti(e_tr)) = rluntx(pos)/real(nro_tro) !*
!------------------------------------
!-Dati_Primari nuovo Punto fittizio
!------------------------------------
               elem_id(etipti(e_pu)) = -etipti(e_pu)       !*
               snomtv (etipti(e_pu)) = snomtv(opumto(pos)) !*nome monte_padre
               htipto (etipti(e_pu)) = e_pu                !*          
               pgrato (etipti(e_pu)) = (etipti(e_pu)-otipti(e_pu))+1 !*
!------------------------------------
!-Dati_Top nuovo Punto fittizio
!------------------------------------
               altetx(etipti(e_pu)) = altetx(opumto(etipti(e_tr))) +
     -         ((altetx(opuvto(pos))-altetx(opumto(pos)))/nro_tro)   !*

               pmintx(etipti(e_pu)) =
     -         AMIN1 (pmintx(opumto(pos)),pmintx(opuvto(pos)))       !*

               pmaxtx(etipti(e_pu)) =
     -         AMAX1 (pmaxtx(opumto(pos)), pmaxtx(opuvto(pos)))      !*

               tambtx(etipti(e_pu)) = tambtx(opumto(etipti(e_tr))) +
     -         ((tambtx(opuvto(pos))-tambtx(opumto(pos)))/nro_tro)   !*
!------------------------------------
!-Dati_Cin nuovo Punto fittizio
!------------------------------------
               prestj(etipti(e_pu)) = prestj(opumto(pos))            !*
               temptj(etipti(e_pu)) = temptj(opumto(pos))            !*
               porttj(etipti(e_pu)) = 0.                             !*
!------------------------------------

            enddo
!------------------------------------
!-Dati ultimo tronco componente (=tronco padre, con pos originale) 
!------------------------------------
            opumto(pos) = etipti(e_pu)               !*monte=ultimo pu_fit
            rluntx(pos) = rluntx (pos)/real(nro_tro) !*
!------------------------------
!-Salvataggio valori nuovi tronco padre
            tr_new_pmo(pgrato(pos)) = opumto(pos) !& punto monte nuovo
            tr_new_lun(pgrato(pos)) = rluntx(pos) !& lunghezza nuova
!------------------------------
!-Definizione Flag avvenuta scomposizione
            tr_flag_scomp = .true.  !true -> presenza scomposizioni

         endif

      enddo
!------------------------------------

      return
!------------------------------------
!-Errori
9997  continue
      str_err='Tronchi > Max'
      goto 9999
9998  continue
      str_err='Punti > Max'
9999  continue
      pf2 = s_trim_d(str_err)
      call gest_error(1,0,' ',' ',0)
      call gest_error(1,0,'Scomponi_lun_tr',str_err(1:pf2),0)
      str_err=' '
      return 1
      end
!==============================================================
      subroutine attiva_tr_originali ()

!-Mette in linea i tronchi originali che eventualmente sono stati scomposti.
!-Ricompone alla lunghezza originale i tronchi scomposti per lunghezza > max.
!-Elimina tronchi e punti fittizi creati in scomposizione.
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/cc.inc'
      include '../inc/scomp_tronchi.inc'

      integer*4 pos
!===========================================
!-Test presenza scomposizioni: False=No scomposizioni
      if(.not.tr_flag_scomp)then
         return
      endif
!===========================================
!-Recupero originali etipti per tronchi e punti
      etipti(e_tr) = tr_ori_etipti 
      etipti(e_pu) = pu_ori_etipti 

      do pos=otipti(e_tr),etipti(e_tr)

         if(tr_ori_lun(pgrato(pos)).gt.0.)then
!-Recupero originali rluntx e opumto
            rluntx(pos) = tr_ori_lun(pgrato(pos))
            opumto(pos) = tr_ori_pmo(pgrato(pos))
!-Ricalcolo dislivello e volume tronco
            disltx(pos)=altetx(opuvto(pos))-altetx(opumto(pos))
            volntx(pos) = rluntx(pos)*diamtx(pos)*diamtx(pos)*
     -                  (pgr4cc/2)/1000.
         endif

      enddo

      return
      end
!==============================================================
      subroutine attiva_tr_scomposti ()

!-Mette in linea i tronchi eventualmente scomposti per lunghezza > max.
!-Attiva i tronchi e punti fittizi creati in scomposizione.
      implicit none
      include '../inc/param.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/cc.inc'
      include '../inc/scomp_tronchi.inc'
      integer*4 pos
!===========================================
!-Test presenza scomposizioni: False=No scomposizioni
      if(.not.tr_flag_scomp)then
         return
      endif
!===========================================
      do pos=otipti(e_tr),tr_ori_etipti

         if(tr_ori_lun(pgrato(pos)).gt.0.)then
!-Recupero opumto e rluntx calcolati per i tronchi padre scomposti
            rluntx(pos) = tr_new_lun(pgrato(pos))
            opumto(pos) = tr_new_pmo(pgrato(pos))
!-Ricalcolo dislivello e volume tronco
!-I valori per tronchi e punti fittizi sono puntati dopo gli etipti originali
!-e pertanto non sono stati modificati
            disltx(pos)=altetx(opuvto(pos))-altetx(opumto(pos))
            volntx(pos) = rluntx(pos)*diamtx(pos)*diamtx(pos)*
     -                  (pgr4cc/2)/1000.
         endif

      enddo

!-Recupero nuovo etipti di tronchi e punti con i fittizi di scomposizione
      etipti(e_tr) = tr_new_etipti  !& 
      etipti(e_pu) = pu_new_etipti  !&

      return
      end
!==============================================================
      subroutine tabella_tipi_regolazione ()

!-Definisce i valori dei tipi di regolazione per Sire.
!-(sono memorizzati in cod_reg_db il cui indice corrisponde al valore su DB)
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tipo_reg.inc'
!===========================================
!Tabella valori                DB     Sire
      cod_reg_db (1) = tc_pv  !PMAN    1
      cod_reg_db (2) = tc_pm  !PASP    2
      cod_reg_db (3) = tc_q   !QSET    3
      cod_reg_db (4) = tc_p   !PSET    1
      cod_reg_db (5) = tc_npv !NPSET   9
      cod_reg_db (6) = tc_pv  !PVSET   1   
      cod_reg_db (7) = tc_npv !NPVSET  9  
      cod_reg_db (8) = tc_pm  !PMSET   2
      cod_reg_db (9) = tc_npm !NPMSET 10
      cod_reg_db(10) = tc_pq  !PQSET   8
      cod_reg_db(11) = tc_on  !ON      5
      cod_reg_db(12) = tc_off !OFF     6
      cod_reg_db(13) = tc_nr  !NR      7
      cod_reg_db(14) = tc_npv !NPMAN   9
      cod_reg_db(15) = tc_npm !NPASP  10

      return
      end
!==============================================================
      subroutine conv_tiporeg_db_sire (treg_db,treg_sire)

!-Converte il tipo regolazione DB nel tipo regolazione Sire
!-Utilizzata in lettura dati da DB per la simulazione Sire
      implicit none
      include '../inc/tipo_reg.inc'

	integer*4 treg_db   !I tipo regolazione per DB
	integer*4 treg_sire !O tipo regolazione per Sire
!--------------------------------------------------------------	
      if((treg_db.gt.0).and.(treg_db.le.tipo_reg_dim))then
          treg_sire = cod_reg_db(treg_db)
      else
          treg_sire = 0
      endif

      return
      end
!==============================================================
      subroutine conv_tiporeg_sire_db (tipoel,iel,treg_db)

!-Converte il tipo regolazione Sire nel tipo regolazione DB
!-Pone=r_und i set non utilizzati
!-Utilizzata in scrittura dati simulazione Sire per il DB
      implicit none
      include '../inc/param.inc'
      include '../inc/th.inc'
      include '../inc/tj.inc'

	integer*4 tipoel  !I tipo elemento
	integer*4 iel     !I puntatore elemento
	integer*4 treg_db !O tipo regolazione per DB
!--------------------------------------------------------------	
      if(htcitj(iel).eq.tc_pv)then
         treg_db = 6      !*DB_PVSET
         if((tipoel.eq.e_im) .or.
     -      (tipoel.eq.e_pz) .or.
     -      (tipoel.eq.e_st) .or.
     -      (tipoel.eq.e_pr) .or.
     -      (tipoel.eq.e_ut))then
             treg_db = 4  !*DB_PSET
         endif
         if((tipoel.eq.e_ce).or.(tipoel.eq.e_cs))then
             treg_db = 1  !*DB_PMAN
         endif
c         pmsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_npv)then
         treg_db = 7      !*DB_NPVSET
         if((tipoel.eq.e_im) .or.
     -      (tipoel.eq.e_pz) .or.
     -      (tipoel.eq.e_st) .or.
     -      (tipoel.eq.e_pr) .or.
     -      (tipoel.eq.e_ut))then
             treg_db = 5  !*DB_NPSET
         endif
         if((tipoel.eq.e_ce).or.(tipoel.eq.e_cs))then
             treg_db = 14 !*DB_NPMAN
         endif
c         pmsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_pm)then
         treg_db = 8      !*DB_PMSET
         if((tipoel.eq.e_im) .or.
     -      (tipoel.eq.e_pz) .or.
     -      (tipoel.eq.e_st) .or.
     -      (tipoel.eq.e_pr) .or.
     -      (tipoel.eq.e_ut))then
             treg_db = 4  !*DB_PSET
         endif
         if((tipoel.eq.e_ce).or.(tipoel.eq.e_cs))then
             treg_db = 2  !*DB_PASP
         endif
c         pvsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_npm)then
         treg_db = 9      !*DB_NPMSET
         if((tipoel.eq.e_im) .or.
     -      (tipoel.eq.e_pz) .or.
     -      (tipoel.eq.e_st) .or.
     -      (tipoel.eq.e_pr) .or.
     -      (tipoel.eq.e_ut))then
             treg_db = 5  !*DB_NPSET
         endif
         if((tipoel.eq.e_ce).or.(tipoel.eq.e_cs))then
             treg_db = 15 !*DB_NPASP
         endif
c         pvsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_q)then
         treg_db = 3      !*DB_QSET
c         pmsetj(iel)=r_und
c         pvsetj(iel)=r_und

      elseif(htcitj(iel).eq.tc_pq)then
         treg_db = 10     !*DB_PQSET

      elseif(htcitj(iel).eq.tc_on)then
         treg_db = 11     !*DB_ON
c         pmsetj(iel)=r_und
c         pvsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_off)then
         treg_db = 12     !*DB_OFF
c         pmsetj(iel)=r_und
c         pvsetj(iel)=r_und
c         qsettj(iel)=r_und

      elseif(htcitj(iel).eq.tc_nr)then
         treg_db = 13     !*DB_NR
c         pmsetj(iel)=r_und
c         pvsetj(iel)=r_und
c         qsettj(iel)=r_und

      else
         treg_db = 0      !*DB_non definito

      endif

      return
      end
!==============================================================
