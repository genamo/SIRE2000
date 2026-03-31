
      SUBROUTINE scrittura_ris_rete (*)
*&*
      implicit none
      include '../inc/param.inc'	! :costanti
      include '../inc/default.inc'	! :costanti
      include '../inc/stazione.inc'	! :costanti
      include '../inc/scenario.inc'	! :costanti
      include '../inc/rk_param.inc'  ! 
      include '../inc/ti.inc'	! :in
      include '../inc/tx.inc'	! :in
      include '../inc/tj.inc'	! :out
      include '../inc/tv.inc'	! :in
      include '../inc/td.inc'	! :in
      include '../inc/STATIONS.INC'
      include '../inc/cap_bon_prv.INC'
      include '../inc/top_app_ele.INC'
      include '../inc/th.inc'	! :in
cT
      include '../inc/tz.inc'	! :in
cgpe-prv
      include '../inc/ty.inc'	! :in
cgpe-prv-end
C      include '../inc/ai.inc'	! :in
*&*
      integer*4 i,j,im,iv
      integer*4 i_file
      integer*4 IOS
      logical*2 elem_bin,elem_uni,elem_spe,elem_cen
      external  elem_bin,elem_uni,elem_spe,elem_cen
      character*(data_len) data
      integer*4 i_app1,i_app2
      character*25 tipo_elem
      character*1 simb
cgpe
      character*25 nome_ce
cgpe-end
      real*4 tot_c,tot_q,tot_m
      real*4 delta_p
      logical*2 ce_serie
!-----------------------------------------------------------------------
      character*3 sta_sym(m_sta)
	data STA_SYM (STA_OFF) /'OFF'/ 	
	data STA_SYM (STA_NR ) /' NR' /	
	data STA_SYM (STA_QM)  /' QM' /	
	data STA_SYM(STA_QC )  /' QC' /	
	data STA_SYM(STA_Q  )  /'  Q'/
	data STA_SYM(STA_PM )  /' PM'/
	data STA_SYM(STA_PV )  /' PV'/
	data STA_SYM(STA_PN )  /' PN'/
	data STA_SYM(STA_PW )  /' PW'/
	data STA_SYM(STA_ON )  /' ON'/
	data STA_SYM(STA_FR )  /' FR'/
	data STA_SYM(STA_IL )  /' IL'/
	data STA_SYM(STA_ROM ) /'ROM'/
	data STA_SYM(STA_P )   /'  P'/
	data STA_SYM(STA_PMQ ) /'PMQ'/
	data STA_SYM(STA_PVQ)  /'PVQ'/
	data STA_SYM(STA_PQ )  /' PQ'/
	data STA_SYM(STA_PU )  /' PU'/	

      character*3 tci_sym(m_rego)
	data TCI_SYM (TC_PV) /' PV'/ 	
	data TCI_SYM (TC_PM) /' PM'/ 	
	data TCI_SYM (TC_Q) /'  Q'/ 	
	data TCI_SYM (TC_T) /'  T'/ 	
	data TCI_SYM (TC_ON) /' ON'/ 	
	data TCI_SYM (TC_OFF) /'OFF'/ 	
	data TCI_SYM (TC_NR) /' NR'/ 	
	data TCI_SYM (TC_PQ) /' PQ'/ 	
	data TCI_SYM (TC_NPV) /'NPV'/ 	
	data TCI_SYM (TC_NPM) /'NPM'/ 	
c-elena
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

      tot_c = 0
      tot_q = 0
      tot_m = 0

      i_file = u_tem

      call open_vfile (i_file,c_path_dirlog,'_'//'ris_rete.txt',
     -    'replace',ios,*1010)


c------ Inizializzazione testata del file:
         write (UNIT=i_file,FMT=05,ERR=1011)
05    FORMAT (1x,'Risultati')

c- data di elaborazione
cport      call legge_data(data,i_app1,i_app2,0,*9999)
      call legge_data(data)

      write (UNIT=i_file,FMT=11,ERR=1011) data
11    FORMAT (1x,'Ora di elaborazione: ',1x,a20)

cgpe-corr      write (UNIT=i_file,FMT=12,ERR=1011) ultimo_camp,corr_ist
cgpe-corr12    FORMAT (1x,'Istante di campionamento: ',1x,f6.3,
cgpe-corr     -        3x,'Numero campionamento: ',1x,i3)
      write (UNIT=i_file,FMT=13,ERR=1011) corr_ist, ultimo_camp
13    FORMAT (1x,'Istante di campionamento: ',1x,f6.3,
     -        3x,'Numero campionamento: ',1x,i3)

      write (UNIT=i_file,FMT=113,ERR=1011)

c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c elementi speciali

      write (UNIT=i_file,FMT=110,ERR=1011)
110   FORMAT (1x,'Codice',1x,'Descrizione',15x,'TCI',1X,'STA',3X,
     *        'PortDef',4x,'Portata',4x,
     *        'Pmonte',4x,'Pvalle',4x,'Consumo')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=111,ERR=1011)
111   FORMAT (44x,'(KSm3h)'4x,'(KSm3h)'3x,'(Bar.ass)',
     *        1x,'(Bar.ass)',2x,'(KSm3h)')
112   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2)
122   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,10x,f10.2)
123   FORMAT 
     *     (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,10x,f10.2,15x,f10.2)
!-format per i risultati delle centrali
132   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2,f10.2)
cgpe-new
133   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2,6x,a4)
cgpe-new-end
!-format per i risultati delle centrali-end
113   FORMAT (1x)

      do j = 1, ne_spe


       write (UNIT=i_file,FMT=113,ERR=1011)

c- tipo di elemento trattato
c /E_VL,E_CE,E_IM,E_PZ,E_ST,E_RG,E_PR,E_VS,E_VR,E_VD,E_VG,E_CS/
       if (j.eq.1) then
         tipo_elem = 'VALVOLA'
       elseif (j.eq.2) then
         tipo_elem = 'CENTRALE'
       elseif (j.eq.3) then
         tipo_elem = 'IMPORTAZIONE'
       elseif (j.eq.4) then
         tipo_elem = 'POZZO'
       elseif (j.eq.5) then
         tipo_elem = 'STOCCAGGIO'
       elseif (j.eq.6) then
         tipo_elem = 'REGOLATORE ESTERNO'
c-elena
       elseif (j.eq.7) then
         tipo_elem = 'PRELIEVO SPECIALE'
       elseif (j.eq.8) then
         tipo_elem = 'VALVOLA DI SEZIONAMENTO'
       elseif (j.eq.9) then
         tipo_elem = 'VALVOLA DI RIDUZIONE'
       elseif (j.eq.10) then
         tipo_elem = 'IMPIANTO DI RIDUZIONE'
       elseif (j.eq.11) then
         tipo_elem = 'IMPIANTO DI REGOLAZIONE'
       elseif (j.eq.12) then
         tipo_elem = 'CENTRALE SEMPLIFICATA'
	 end if
c-elena
cgpe       write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
       if (etipti(te_spe(j)) .ge. otipti(te_spe(j))) then
cgpe-corr       write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
       write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem
       endif

       do i = otipti(te_spe(j)), etipti(te_spe(j))

        if ((i.gt.otipti(e_vl)).and.(htipto(i).ne.htipto(i-1))) then
          write (UNIT=i_file,FMT=113,ERR=1011)
        end if

        if (elem_bin(i) .and. elem_spe(i)) then
          im = opumto(i)
          iv = opuvto(i)
          if (elem_cen(i)) then
c          if (htipto(i) .eq. e_ce) then
           if (ce_serie(i)) then
             write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
c     *                                     prestj(im)-1.013,
c     *                                     prestj(iv)-1.013,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     (tot_cons(pgrato(i))+
     *                                      tot_cons(pgrato_app(i)))*
     *                                      CONV_NS

             tot_c =tot_c+tot_cons(pgrato(i))+tot_cons(pgrato_app(i))
           else
            if (htipto(i) .eq. e_ce) then
             if (assetto_cm(pgrato(i)) .eq. 0) then
               nome_ce = snomtv(i)
             else
               nome_ce = trim(sigla_ass(pgrato(i)))
             endif
cgpe-new
             if (fl_fatal(pgrato(i)) .ge. 0) then
cgpe-new-end
cgpe              write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
              write (UNIT=i_file,FMT=132,ERR=1011) i,
     *                               nome_ce,
cgpe-end
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     tot_cons(pgrato(i))*CONV_NS
cgpe-new
             tot_c =tot_c+tot_cons(pgrato(i))
             else
cgpe              write (UNIT=i_file,FMT=133,ERR=1011) i,snomtv(i),
              write (UNIT=i_file,FMT=133,ERR=1011) i,
     *                               nome_ce,
cgpe-end
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     '$$$$'
             endif
cgpe-new-end
cgpe-prv
            elseif (htipto(i) .eq. e_cs) then
             write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     consty(i)*CONV_NS
cgpe-prv-end
             tot_c =tot_c+consty(i)
            endif
           endif
          else

           write (UNIT=i_file,FMT=112,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv)

          endif
        else if (elem_uni(i)) then
            iv = opuvto(i)
            write (UNIT=i_file,FMT=122,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                           porttj(i)*CONV_NS,
     *                                           prestj(iv)
          tot_q = tot_q + diretj(i)*porttj(i)
        end if
       end do
      end do


c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c punti

      write (UNIT=i_file,FMT=140,ERR=1011)
140   FORMAT (1x,'Codice',1x,'Descrizione',25x,'PortDef'8x,
     *        'Portata'4x,'Pressione',4x,'Temperatura')
c_delta     *        'Pressione',7x,'Temperatura',7x,'Delta Lim.Pres.')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=141,ERR=1011)
141   FORMAT (44x,'(KSm3h)',8x,'(KSm3h)',4x,'(Bar.ass)',
     *        4x,'    (C)    ')
!142   FORMAT (1x,I6,1x,a30,4x,f10.2,2x,f10.2,8x,f10.2)
142   FORMAT (1x,I6,1x,a30,4x,f10.2,4x,f10.2,2x,f10.2,1x,a1,
     *        2x,f10.2)
c_delta     *        6x,f10.2,6x,f10.2)

      write (UNIT=i_file,FMT=113,ERR=1011)

c- tipo di elemento trattato
      tipo_elem = 'PUNTO'
cgpe-corr      write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
      write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem

CRSA --  Delta Pressione
      do i = otipti(e_pu), etipti(e_pu)
        simb = ' '
        delta_p = 0.0
        if (prestj(i).lt.pmintx(i)-.1) then
          simb = '<'
          delta_p = pmintx(i)-prestj(i)
        elseif (prestj(i).gt.pmaxtx(i)+.1) then
          simb = '>'
          delta_p = prestj(i)-pmaxtx(i)
        endif
        write (UNIT=i_file,FMT=142,ERR=1011) i,snomtv(i),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                      porttj(i)*CONV_NS,
     *                                      prestj(i),
     *                                      simb,
     *                                      temptj(i)-T0
c_delta     *                                      delta_p
          tot_m = tot_m + porttj(i)
      end do

c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c tronchi

      write (UNIT=i_file,FMT=100,ERR=1011)
100   FORMAT (1x,'Codice',1x,'Descrizione',25x,'Portata',4x,
     *        'Temperatura')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=101,ERR=1011)
101   FORMAT (44x,'(KSm3h)',4x,'    (C)    ')
102   FORMAT (1x,I6,1x,a30,4x,f10.2,2x,f10.2)

c- tipo di elemento trattato
      tipo_elem = 'TRONCO'
      write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem
103    FORMAT (1x,'Tipo di elemento: ',1x,a20)

      do i = otipti(e_tr), etipti(e_tr)
        write (UNIT=i_file,FMT=102,ERR=1011) i,snomtv(i),
     *                                       porttj(i)*CONV_NS,
cT     *                                       temptj(i)-T0
cT     *    ((temptj(opumto(i))+temptj(opuvto(i)))/2.0)-T0
     *                                       touttz(i)-T0
      end do


      write (UNIT=i_file,FMT=150,ERR=1011) tot_c,tot_q,tot_m
      write (UNIT=i_file,FMT=150,ERR=1011) 
     -       tot_c*CONV_NS*24,tot_q*CONV_NS*24,tot_m*CONV_NS*24
150   FORMAT (2x,"Tot. consumo: ",f10.2,
     -        2x,"Tot. unari: ",f10.2,
     -        2x,"Tot. prelievo: ",f10.2)
      close(i_file)
1111  continue
      return ! scrittura_ris_rete

9999  return1

1010  continue
        call gest_error(2,0,'SCRITTURA_RIS_RETE',
     *   'Errore in apertura file',0)
      goto 1111

1011  continue
        call gest_error(2,0,'SCRITTURA_RIS_RETE',
     *   'File output corrotto',0)
      close(i_file)
      goto 1111
      
      end
c---------------------------------------------------------------------------
c**************************************************************************************
c-elena-Calcolo grandezze derivate IN/OUT per centrali e conversioni da unità di misura
c       interna a unità di misura di calcolo
c-elena
      subroutine calcola_derivati_out_ce(istaz,ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per ricavare alcuni valori non calcolati direttamente.Vengono inoltre 
c   convertite all'UDM interna portata di macchina,altezza adiabatica e giri% usati nei 
c   calcoli con una UDM diversa
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
      include '../inc/rk.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/mf_types.inc'
      include '../inc/argo_reale_ris.inc'


	include '../inc/ass_cond.inc'

	real*4 pcls_real          
      external pcls_real      !function per il calcolo del PCS reale
	                        !(calcolato dalla composizione)

      integer*4 istaz,      !I)indice centrale nella struttura dati completa
     *          ivert,      !indice centrale nella struttura dati ridotta
     *          i,iu,k
      real*4 ptt_stat,      !PTT(=potenza teorica termodinamica) di centrale
     *       pft_stat,      !PFT(=potenza fornita dalla turbina) di centrale
     *       ro_mol_cn      !densità molare calcolata in condizioni normali
c-bp20
      real*4   eps_q/1.e-3/
c-bp20-end
c-bp05
      real*4    tot_nom_pow_der,unit2_perc_app
c	real*4	PERC_POW_STAZIONE(m_ogg)


	integer*4 ii,jj

	logical*2 aa
c-bp05-end
c------------------------------------------------------------------------------------------

      unit_1=unit_1_(ISTAZ)
      unit_2=unit_2_(ISTAZ)

c-tbg-off
      call ini_derivati_out_ce(istaz,ivert)
c-tbg-off-end

c-bp20
c----->dati di centrale/assetto
      q_riciclo(ivert) = flow_ric(ivert) - flow_stat(ivert)
cgpe      if (abs(q_riciclo(ivert)).lt.eps_q .or.
      if (q_riciclo(ivert) .lt. eps_q .or.
     *    flow_stat(ivert) .le. 0.         ) then
        q_riciclo(ivert) = 0.
      endif
      if (q_riciclo(ivert) .gt. 0) then
          call messtel(.true.,.true.,' ',
     * 'Centrale in riciclo su almeno un turbogruppo')
      endif
c-bp20-end
cgpe
      flow_stat_in(ivert) = flow_stat(ivert) + tot_cons(ivert)
cgpe-end
c-bp05
      power_tot(ivert) = 0.
      tot_nom_pow_der = 0.
c-bp05-end
	iu=first_unit(ivert)
c-tbg_attivi
      num_tbg_attivi(ivert) = 0
c-tbg_attivi-end
c----->dati di turbogruppo
C---------------------------------
      IF (FLAG_ASS_COND(ISTAZ))THEN
C---------------------------------

   
cmar_ass_cond_san      do i=1,unit_1
   
c      aa=.true.


           if( a_err(istaz) )then
c      do i=1,unit_1
c      if(unit_avail_as(iu+i-1).ne.0)then
    
c	status(iu+i-1)=	status(iu+i-1)

c    
c	endif
c      enddo
      call tbgV()
c tbgV va bene per i 24 tbg

      endif
    

c  attenzone a questo giro !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!             
      i=0
cmarrrrrrrrrrrrrrrrrrr
      ii=0
cmarrrrrrrrrrrrrrrrrrr
       

c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!  
      do while (ii.lt.unit_1)
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!  
      i=i+1
      if(unit_avail_as(iu+i-1).ne.0)then
       
      ii=ii+1
      endif
	
	  if (lstatus(iu+i-1).eq.ON) then

c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
cc27       if(unit_avail_as(iu+i-1).ne.0)then
cc27      ii=ii+1
cc27      endif
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
cmar_ass_cond_san  k = iu+i-1
c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
cccccccccccccccccccc          k=i
	
ccc27      k = iu+i-1

      k = iu+i-1
	
          call calc_dati_ISO_as (ivert,k,ii)
ccccc27            call calc_dati_ISO(ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
cmar_ass_cond_san          unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!  
      unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(ii+iu-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)





c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
cgpe            unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
c              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
c     *	                        ro_mol_cn*av_weight(istaz)
c-ele-3/5            endif
cgpe-end
c     potenza teorica termodinamica -fase2-
c	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
cgpe-end
c     rapporto consumo/qout -fase2-
c              unit2_cons_su_qout(iu+i-1)=
c     *                unit_cons(iu+i-1)/unit2_qout(iu+i-1)
c-ele-3/5	      end if
          end if
        end if
	end do

C----------------------
	ELSEIF(FLAG_ASS_COND(ISTAZ-1))THEN
C----------------------
      unit_1=unit_1_(ISTAZ-1)
      unit_2=unit_2_(ISTAZ-1)

     

cmar_ass_cond_san  do i=unit_1+1,unit_1+unit_2
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!i=0
c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ii=0
c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
c      do while ((ii.lt.first_unit(istaz)) .and. 
c     *             (ii.le.first_unit(istaz)+unit_2))
      i=0
c      ii=unit_1
      ii=0
ccccc      do while ((ii.gt.unit_1).OR.(ii.le.unit_1+unit_2))

ccccc27      do while (ii.lt.unit_1+unit_2)
    







      do while (ii.lt.unit_2)
	


c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      i=i+1

      
	if(unit_avail_as(iu+i-1).ne.0)then
	


      ii=ii+1
      endif



	  if (lstatus(iu+i-1).eq.ON) then

c  attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
cccc27      ii=ii+1
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
          k = iu+i-1
	    jj=ii+unit_1
          call calc_dati_ISO_AS (ivert,k,jj)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
       unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/
     *                              nom_hrate(ii+unit_1+iu-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)


		




c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
cgpe            unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
c              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
c     *	                        ro_mol_cn*av_weight(istaz)
c-ele-3/5            endif
cgpe-end
c     potenza teorica termodinamica -fase2-
c	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
cgpe-end
c     rapporto consumo/qout -fase2-
c              unit2_cons_su_qout(iu+i-1)=
c     *                unit_cons(iu+i-1)/unit2_qout(iu+i-1)
c-ele-3/5	      end if
          end if
        end if
	end do
C----------------------
	ELSE
C----------------------
            do i=1,unit_num(ivert)
	  if (lstatus(iu+i-1).eq.ON) then
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
          k = iu+i-1
          call calc_dati_ISO (ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
          unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)
c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if

          end if
        end if
	end do
C----------------------
	ENDIF
C----------------------

c----->dati di centrale
c     PTT totale  [kW]
      ptt_stat=0
c	PFT totale  [kW]
	pft_stat=0
	do i=1,unit_num(ivert)
	   if (lstatus(iu+i-1).eq.ON) then
            ptt_stat=ptt_stat+unit_ptt(iu+i-1)
            pft_stat=pft_stat+power(iu+i-1)
	      if(unit_bifase(iu+i-1)) then
               ptt_stat=ptt_stat+unit2_ptt(iu+i-1)
            end if
	   end if
	end do
        
c     rapporto consumo/portata uscente [kNm3/h]/[kNm3/h]=[%N]
!--SM--17/11/06--
      if(flow_stat(ivert).ne.0.)then
         cons_su_qout(ivert)=tot_cons(ivert)/flow_stat(ivert)
      endif
!--SM--17/11/06--

c     rapporto fuel/PTT [numero puro]
c--------------------------------------->Analisi dimensionale
c Il FUEL è il consumo visto in termini di energia,e si ricava moltiplicando quest'ultimo
c per il PCS.Dimensionalmente abbiamo:
c         tot_cons*pcls_real/ptt_stat=[kNm3/h][kJ/kNm3]/[kW]=[kJ/kWh]
c Per ottenere un numero puro devo quindi dividere per il fattore di conversione
c ckWh=3600,essendo kJ=3600kWh
c----------------------------------------------------------------------------------------
cgpe-prv
      if (ptt_stat .ne. 0) then
       fuel_su_ptt(ivert)=tot_cons(ivert)*pcls_real(istaz)/ptt_stat/ckWh
      endif
cgpe-prv-end

c     rapporto PTT/PFT [kNm3/h]/[kNm3/h]=[%N]
cgpe-prv
      if (pft_stat .ne. 0) then
       ptt_su_pft(ivert)=ptt_stat/pft_stat
      endif
cgpe-prv-end

c-bp05
      power_perc_der(ivert) = 0.
      if (tot_nom_pow_der .ne. 0) then
cgpe        power_perc_der(ivert) = power_perc_der(ivert) / tot_nom_pow_der
        power_perc_der(ivert) = power_tot(ivert) / tot_nom_pow_der
      endif
cmar_ass_cond INTERVENTO INTEGRAZIONE PERCENTUALE POWER DI STAZIONE
      tot_nom(istaz)=tot_nom_pow_der


	IF(FLAG_ASS_COND(ISTAZ-1))THEN

	PERC_POW_STAZIONE(IVERT)= (power_tot(ivert)+power_tot(ivert-1))/
     *                (tot_nom(istaz-1)+tot_nom(istaz))
      PERC_POW_STAZIONE(IVERT-1)=PERC_POW_STAZIONE(IVERT)
	ENDIF
cmar_ass_cond

c-bp05-end
c-report
      iu = first_unit(ivert)
      nom_max_pow(ivert) = 0.
      nom_max_pow_der(ivert) = 0.
      nom_max_pow_vinc(ivert) = 0.
      nom_min_pow(ivert) = R_MAX
      do i = 1 , unit_num(ivert)
	  if (unit_avail(iu+i-1) .ge. 1) then
          k = iu+i-1
          nom_max_pow(ivert) = nom_max_pow(ivert) + nom_power(k)
          nom_pow_der(k) = nom_power(k)*
     *                    (unit_power_corr(k)/stat_vars(1,ivert))
          nom_max_pow_der(ivert) = nom_max_pow_der(ivert) +
     *                             nom_pow_der(k)
c unit_vinc_maxpow e unit_vinc_minpow contengono il valore di potenza nominale
c "tagliate" con i vincoli operativi alle condizioni reali mentre unit_vinc_maxpow_iso
c e unit_vinc_minpow_iso sono rispettivamente i valori condizioni iso
          nom_max_pow_vinc(ivert) = nom_max_pow_vinc(ivert) +
     *                              unit_vinc_maxpow_iso(k)
          nom_min_pow(ivert) = min(nom_min_pow(ivert),
     *                             unit_vinc_minpow_iso(k))
        endif
      enddo
c-report-end
 

	return
	end

c---------------------------------------------------------------------------
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine calcola_derivati_out_ce_wor(istaz,ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per ricavare alcuni valori non calcolati direttamente.Vengono inoltre 
c   convertite all'UDM interna portata di macchina,altezza adiabatica e giri% usati nei 
c   calcoli con una UDM diversa
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
      include '../inc/rk.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/mf_types.inc'
      include '../inc/argo_reale_ris.inc'


	include '../inc/ass_cond.inc'

	real*4 pcls_real          
      external pcls_real      !function per il calcolo del PCS reale
	                        !(calcolato dalla composizione)

      integer*4 istaz,      !I)indice centrale nella struttura dati completa
     *          ivert,      !indice centrale nella struttura dati ridotta
     *          i,iu,k
      real*4 ptt_stat,      !PTT(=potenza teorica termodinamica) di centrale
     *       pft_stat,      !PFT(=potenza fornita dalla turbina) di centrale
     *       ro_mol_cn      !densità molare calcolata in condizioni normali
c-bp20
      real*4   eps_q/1.e-3/
c-bp20-end
c-bp05
      real*4    tot_nom_pow_der,unit2_perc_app


	integer*4 ii
c-bp05-end
c------------------------------------------------------------------------------------------
c-tbg-off
      call ini_derivati_out_ce(istaz,ivert)
c-tbg-off-end

c-bp20
c----->dati di centrale/assetto
      q_riciclo(ivert) = flow_ric(ivert) - flow_stat(ivert)
cgpe      if (abs(q_riciclo(ivert)).lt.eps_q .or.
      if (q_riciclo(ivert) .lt. eps_q .or.
     *    flow_stat(ivert) .le. 0.         ) then
        q_riciclo(ivert) = 0.
      endif
c-bp20-end
cgpe
      flow_stat_in(ivert) = flow_stat(ivert) + tot_cons(ivert)
cgpe-end
c-bp05
      power_tot(ivert) = 0.
      tot_nom_pow_der = 0.
c-bp05-end
	iu=first_unit(ivert)
c-tbg_attivi
      num_tbg_attivi(ivert) = 0
c-tbg_attivi-end
c----->dati di turbogruppo
C---------------------------------
      IF (FLAG_ASS_COND(ISTAZ))THEN
C---------------------------------
cmar_ass_cond_san      
      do i=1,unit_1

c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!              i=0
       
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!   do while (ii.lt.unit_1)
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!  i=i+1
	
	  if (lstatus(iu+i-1).eq.ON) then
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ii=ii+1

c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
cmar_ass_cond_san 
          k = iu+i-1
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! k=i
          call calc_dati_ISO (ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
cmar_ass_cond_san          
      unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!   unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(ii))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)
c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
cgpe            unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
c              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
c     *	                        ro_mol_cn*av_weight(istaz)
c-ele-3/5            endif
cgpe-end
c     potenza teorica termodinamica -fase2-
c	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
cgpe-end
c     rapporto consumo/qout -fase2-
c              unit2_cons_su_qout(iu+i-1)=
c     *                unit_cons(iu+i-1)/unit2_qout(iu+i-1)
c-ele-3/5	      end if
          end if
        end if
	end do

C----------------------
	ELSEIF(FLAG_ASS_COND(ISTAZ-1))THEN
C----------------------
cmar_ass_cond_san  
          do i=unit_1+1,unit_1+unit_2
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!i=0
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!ii=0
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! do while ((ii.lt.first_unit(istaz)) .and. 
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*             (ii.le.first_unit(istaz)+unit_2))
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!i=i+1
	  if (lstatus(iu+i-1).eq.ON) then
c   attenzone a questo giro!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! ii=ii+1
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
          k = iu+i-1
          call calc_dati_ISO (ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
          unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)
c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
cgpe            unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
c              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
c     *	                        ro_mol_cn*av_weight(istaz)
c-ele-3/5            endif
cgpe-end
c     potenza teorica termodinamica -fase2-
c	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
cgpe-end
c     rapporto consumo/qout -fase2-
c              unit2_cons_su_qout(iu+i-1)=
c     *                unit_cons(iu+i-1)/unit2_qout(iu+i-1)
c-ele-3/5	      end if
          end if
        end if
	end do
C----------------------
	ELSE
C----------------------
            do i=1,unit_num(ivert)
	  if (lstatus(iu+i-1).eq.ON) then
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
          k = iu+i-1
          call calc_dati_ISO (ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
          unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)
c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if

          end if
        end if
	end do
C----------------------
	ENDIF
C----------------------

c----->dati di centrale
c     PTT totale  [kW]
      ptt_stat=0
c	PFT totale  [kW]
	pft_stat=0
	do i=1,unit_num(ivert)
	   if (lstatus(iu+i-1).eq.ON) then
            ptt_stat=ptt_stat+unit_ptt(iu+i-1)
            pft_stat=pft_stat+power(iu+i-1)
	      if(unit_bifase(iu+i-1)) then
               ptt_stat=ptt_stat+unit2_ptt(iu+i-1)
            end if
	   end if
	end do
        
c     rapporto consumo/portata uscente [kNm3/h]/[kNm3/h]=[%N]
!--SM--17/11/06--
      if(flow_stat(ivert).ne.0.)then
         cons_su_qout(ivert)=tot_cons(ivert)/flow_stat(ivert)
      endif
!--SM--17/11/06--

c     rapporto fuel/PTT [numero puro]
c--------------------------------------->Analisi dimensionale
c Il FUEL è il consumo visto in termini di energia,e si ricava moltiplicando quest'ultimo
c per il PCS.Dimensionalmente abbiamo:
c         tot_cons*pcls_real/ptt_stat=[kNm3/h][kJ/kNm3]/[kW]=[kJ/kWh]
c Per ottenere un numero puro devo quindi dividere per il fattore di conversione
c ckWh=3600,essendo kJ=3600kWh
c----------------------------------------------------------------------------------------
cgpe-prv
      if (ptt_stat .ne. 0) then
       fuel_su_ptt(ivert)=tot_cons(ivert)*pcls_real(istaz)/ptt_stat/ckWh
      endif
cgpe-prv-end

c     rapporto PTT/PFT [kNm3/h]/[kNm3/h]=[%N]
cgpe-prv
      if (pft_stat .ne. 0) then
       ptt_su_pft(ivert)=ptt_stat/pft_stat
      endif
cgpe-prv-end

c-bp05
      power_perc_der(ivert) = 0.
      if (tot_nom_pow_der .ne. 0) then
cgpe        power_perc_der(ivert) = power_perc_der(ivert) / tot_nom_pow_der
        power_perc_der(ivert) = power_tot(ivert) / tot_nom_pow_der
      endif
c-bp05-end

c-report
      iu = first_unit(ivert)
      nom_max_pow(ivert) = 0.
      nom_max_pow_der(ivert) = 0.
      nom_max_pow_vinc(ivert) = 0.
      nom_min_pow(ivert) = R_MAX
      do i = 1 , unit_num(ivert)
	  if (unit_avail(iu+i-1) .ge. 1) then
          k = iu+i-1
          nom_max_pow(ivert) = nom_max_pow(ivert) + nom_power(k)
          nom_pow_der(k) = nom_power(k)*
     *                    (unit_power_corr(k)/stat_vars(1,ivert))
          nom_max_pow_der(ivert) = nom_max_pow_der(ivert) +
     *                             nom_pow_der(k)
c unit_vinc_maxpow e unit_vinc_minpow contengono il valore di potenza nominale
c "tagliate" con i vincoli operativi alle condizioni reali mentre unit_vinc_maxpow_iso
c e unit_vinc_minpow_iso sono rispettivamente i valori condizioni iso
          nom_max_pow_vinc(ivert) = nom_max_pow_vinc(ivert) +
     *                              unit_vinc_maxpow_iso(k)
          nom_min_pow(ivert) = min(nom_min_pow(ivert),
     *                             unit_vinc_minpow_iso(k))
        endif
      enddo
c-report-end

	return
	end

c---------------------------------------------------------------------------

c**************************************************************************************
c-elena-Calcolo grandezze derivate IN/OUT per centrali e conversioni da unità di misura
c       interna a unità di misura di calcolo
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c**************************************************************************************
c-elena-Calcolo grandezze derivate IN/OUT per centrali e conversioni da unità di misura
c       interna a unità di misura di calcolo
c-elena
      subroutine calcola_derivati_out_ce_old(istaz,ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per ricavare alcuni valori non calcolati direttamente.Vengono inoltre 
c   convertite all'UDM interna portata di macchina,altezza adiabatica e giri% usati nei 
c   calcoli con una UDM diversa
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
      include '../inc/rk.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/mf_types.inc'
      include '../inc/argo_reale_ris.inc'

	real*4 pcls_real          
      external pcls_real      !function per il calcolo del PCS reale
	                        !(calcolato dalla composizione)

      integer*4 istaz,      !I)indice centrale nella struttura dati completa
     *          ivert,      !indice centrale nella struttura dati ridotta
     *          i,iu,k
      real*4 ptt_stat,      !PTT(=potenza teorica termodinamica) di centrale
     *       pft_stat,      !PFT(=potenza fornita dalla turbina) di centrale
     *       ro_mol_cn      !densità molare calcolata in condizioni normali
c-bp20
      real*4   eps_q/1.e-3/
c-bp20-end
c-bp05
      real*4    tot_nom_pow_der,unit2_perc_app
c-bp05-end
c------------------------------------------------------------------------------------------
c-tbg-off
      call ini_derivati_out_ce(istaz,ivert)
c-tbg-off-end

c-bp20
c----->dati di centrale/assetto
      q_riciclo(ivert) = flow_ric(ivert) - flow_stat(ivert)
cgpe      if (abs(q_riciclo(ivert)).lt.eps_q .or.
      if (q_riciclo(ivert) .lt. eps_q .or.
     *    flow_stat(ivert) .le. 0.         ) then
        q_riciclo(ivert) = 0.
      endif
c-bp20-end
cgpe
      flow_stat_in(ivert) = flow_stat(ivert) + tot_cons(ivert)
cgpe-end
c-bp05
      power_tot(ivert) = 0.
      tot_nom_pow_der = 0.
c-bp05-end
	iu=first_unit(ivert)
c-tbg_attivi
      num_tbg_attivi(ivert) = 0
c-tbg_attivi-end
c----->dati di turbogruppo
      do i=1,unit_num(ivert)
	  if (lstatus(iu+i-1).eq.ON) then
c-tbg_attivi
          num_tbg_attivi(ivert) = num_tbg_attivi(ivert) + 1
c-tbg_attivi-end
          k = iu+i-1
          call calc_dati_ISO (ivert,k)
c-bp05
cgpe          power_perc_der(ivert) = power_perc_der(ivert) + power(k)
          power_tot(ivert) = power_tot(ivert) + power(k)
          tot_nom_pow_der = tot_nom_pow_der + nom_pow_der(k)
c-bp05-end

c     heat rate %[%N]
          unit_hrate_perc(iu+i-1)=(unit_hrate(iu+i-1)/nom_hrate(iu+i-1))
c     efficienza turbina[%N]
c------------------------>Analisi dimensionale
c     L'UDM dell'HR è [kJ/kWh].Per ottenere un efficienza espressa come %N devo
c     moltiplicare per il fattore di conversione ckWh=3600,essendo kJ=3600kWh
c---------------------------------------------------------------------------------
cgpe          unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          if (unit_hrate(iu+i-1) .ne. 0.) then
            unit_eff_turb(iu+i-1)=ckWh/unit_hrate(iu+i-1)
          endif
c     numero di giri RPM [RPM]
	    unit_rev_rpm(iu+i-1)=nom_rev(iu+i-1)*unit_rev(iu+i-1)/100.
c     portata uscente dal compressore [kNm3/h] 
          unit_qout(iu+i-1)=flow_stat(ivert)*unit_perc(iu+i-1)
c     portata in peso [kg/h]
c------------------------>Analisi dimensionale
c     ro_mol_cn=densità molare in condizioni normali(t=273.15K)   [mole/Nm3]
c     av_weight=peso molecolare medio                             [g/mole]
c     unit_qpeso=unit_qout*ro_mol_cn*av_weight
c                [kNm3/h][mole/Nm3][g/mole]=[kg/h]  (grandezza fisica=portata in massa)
c---------------------------------------------------------------------------------

          ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
          unit_qpeso(iu+i-1)=unit_qout(iu+i-1)*
     *	                   ro_mol_cn*av_weight(istaz)
c     potenza teorica termodinamica [kW]
	    unit_ptt(iu+i-1)=power_ric(iu+i-1)*unit_eff(iu+i-1)
c     rapporto consumo/qout [kNm3/h]/[kNm3/h]=[%N]
c-ele 26/10/2006 proteggo da una divisione per zero
          if (unit_qout(iu+i-1).ne.0) then
              unit_cons_su_qout(iu+i-1)=unit_cons(iu+i-1)/
     *                                  unit_qout(iu+i-1)
	    else
              unit_cons_su_qout(iu+i-1)=0.
	    end if
c
	    if (unit_bifase(iu+i-1)) then
c     numero di giri RPM  -fase2-
	      unit2_rev_rpm(iu+i-1)=nom2_rev(iu+i-1)*unit2_rev(iu+i-1)/100.
c-ele 5/5/2006
c     potenza teorica termodinamica -fase2-
	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      else
	        unit2_perc_app=unit_perc(iu+i-1)
c     portata uscente dal compressore -fase2-
              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc_app
c     portata in peso  -fase2-
              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
     *	                        ro_mol_cn*av_weight(istaz)
c     rapporto consumo/qout -fase2-
!--SM--17/11/06--
              if(unit2_qout(iu+i-1).ne.0.)then
                 unit2_cons_su_qout(iu+i-1)=
     *           unit_cons(iu+i-1)/unit2_qout(iu+i-1)
              endif
!--SM--17/11/06--

	      end if
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
c     portata uscente dal compressore -fase2-
cgpe            unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c              unit2_qout(iu+i-1)=flow_stat(ivert)*unit2_perc(iu+i-1)
c     portata in peso  -fase2-
c              unit2_qpeso(iu+i-1)=unit2_qout(iu+i-1)*
c     *	                        ro_mol_cn*av_weight(istaz)
c-ele-3/5            endif
cgpe-end
c     potenza teorica termodinamica -fase2-
c	      unit2_ptt(iu+i-1)=power2_ric(iu+i-1)*unit2_eff(iu+i-1)
cgpe
c-ele-3/5            if(stat_ord(ivert).eq.f_paral) then
cgpe-end
c     rapporto consumo/qout -fase2-
c              unit2_cons_su_qout(iu+i-1)=
c     *                unit_cons(iu+i-1)/unit2_qout(iu+i-1)
c-ele-3/5	      end if
          end if
        end if
	end do
c----->dati di centrale
c     PTT totale  [kW]
      ptt_stat=0
c	PFT totale  [kW]
	pft_stat=0
	do i=1,unit_num(ivert)
	   if (lstatus(iu+i-1).eq.ON) then
            ptt_stat=ptt_stat+unit_ptt(iu+i-1)
            pft_stat=pft_stat+power(iu+i-1)
	      if(unit_bifase(iu+i-1)) then
               ptt_stat=ptt_stat+unit2_ptt(iu+i-1)
            end if
	   end if
	end do
        
c     rapporto consumo/portata uscente [kNm3/h]/[kNm3/h]=[%N]
!--SM--17/11/06--
      if(flow_stat(ivert).ne.0.)then
         cons_su_qout(ivert)=tot_cons(ivert)/flow_stat(ivert)
      endif
!--SM--17/11/06--

c     rapporto fuel/PTT [numero puro]
c--------------------------------------->Analisi dimensionale
c Il FUEL è il consumo visto in termini di energia,e si ricava moltiplicando quest'ultimo
c per il PCS.Dimensionalmente abbiamo:
c         tot_cons*pcls_real/ptt_stat=[kNm3/h][kJ/kNm3]/[kW]=[kJ/kWh]
c Per ottenere un numero puro devo quindi dividere per il fattore di conversione
c ckWh=3600,essendo kJ=3600kWh
c----------------------------------------------------------------------------------------
cgpe-prv
      if (ptt_stat .ne. 0) then
       fuel_su_ptt(ivert)=tot_cons(ivert)*pcls_real(istaz)/ptt_stat/ckWh
      endif
cgpe-prv-end

c     rapporto PTT/PFT [kNm3/h]/[kNm3/h]=[%N]
cgpe-prv
      if (pft_stat .ne. 0) then
       ptt_su_pft(ivert)=ptt_stat/pft_stat
      endif
cgpe-prv-end

c-bp05
      power_perc_der(ivert) = 0.
      if (tot_nom_pow_der .ne. 0) then
cgpe        power_perc_der(ivert) = power_perc_der(ivert) / tot_nom_pow_der
        power_perc_der(ivert) = power_tot(ivert) / tot_nom_pow_der
      endif
c-bp05-end

c-report
      iu = first_unit(ivert)
      nom_max_pow(ivert) = 0.
      nom_max_pow_der(ivert) = 0.
      nom_max_pow_vinc(ivert) = 0.
      nom_min_pow(ivert) = R_MAX
      do i = 1 , unit_num(ivert)
	  if (unit_avail(iu+i-1) .ge. 1) then
          k = iu+i-1
          nom_max_pow(ivert) = nom_max_pow(ivert) + nom_power(k)
          nom_pow_der(k) = nom_power(k)*
     *                    (unit_power_corr(k)/stat_vars(1,ivert))
          nom_max_pow_der(ivert) = nom_max_pow_der(ivert) +
     *                             nom_pow_der(k)
c unit_vinc_maxpow e unit_vinc_minpow contengono il valore di potenza nominale
c "tagliate" con i vincoli operativi alle condizioni reali mentre unit_vinc_maxpow_iso
c e unit_vinc_minpow_iso sono rispettivamente i valori condizioni iso
          nom_max_pow_vinc(ivert) = nom_max_pow_vinc(ivert) +
     *                              unit_vinc_maxpow_iso(k)
          nom_min_pow(ivert) = min(nom_min_pow(ivert),
     *                             unit_vinc_minpow_iso(k))
        endif
      enddo
c-report-end

	return
	end
c--------------------------------------------------------------------------------------
c-elena
      subroutine converti_out_ce(ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per convertire l'UDM di alcune grandezze che nel calcolo sono usate con
c   una UDM diversa da quella interna(del DB).
c   Nello specifico i problemi riguardano:
c      portata di macchina:l'UDM interna è [m3/h] ma i calcoli sono eseguiti in [km3/h]
c      prevalenza:l'UDM interna è [m] ma i calcoli sono eseguiti in [km]
c      numero di giri percentuale:i calcoli restituiscono percentuali a 100,ma l'UDM
c                                 interna le prevede normalizzate
c*****************************************************************************************

      implicit none

	include '../inc/param.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
      include '../inc/stations.inc'
  	include '../inc/conv.inc'

	integer*4 ivert,      !indice centrale nella struttura dati ridotta
     *          i,iu

      real*4 udm_int ! function

c-----------------------------------------------------------------------------------------
	iu=first_unit(ivert)
c---------------------------------------------------->DATI DI CENTRALE
c------>altezza adiabatica:da [km] a [m]
c      head(ivert)=head(ivert)*conv_h
      head(ivert)=udm_int(udm_H,head(ivert))
c------>portata di macchina totale:da [km3/h] a [m3/h]
c	flowm(ivert)=flowm(ivert)*conv_qm
	flowm(ivert)=udm_int(udm_QM,flowm(ivert))
c---------------------------------------------------->DATI DI UNITA'
	do i=1,unit_num(ivert)
c------>altezza adiabatica:da [km] a [m]
c         unit_head(iu+i-1)=unit_head(iu+i-1)*conv_h
         unit_head(iu+i-1)=udm_int(udm_H,unit_head(iu+i-1))
c------>portata di macchina fase1:da [km3/h] a [m3/h]
c         unit_flow(iu+i-1)=unit_flow(iu+i-1)*conv_qm
         unit_flow(iu+i-1)=udm_int(udm_QM,unit_flow(iu+i-1))
c------>numero di giri% fase1: da [%] a [%N]
c         unit_rev(iu+i-1)=unit_rev(iu+i-1)/100.
         unit_rev(iu+i-1)=udm_int(udm_NG_perc,unit_rev(iu+i-1))
         if(unit_bifase(iu+i-1)) then
c------>altezza adiabatica:da [km] a [m]
c            unit2_head(iu+i-1)=unit2_head(iu+i-1)*conv_h
            unit2_head(iu+i-1)=udm_int(udm_H,unit2_head(iu+i-1))
c------>portata di macchina fase2:da [km3/h] a [m3/h]
c            unit2_flow(iu+i-1)=unit2_flow(iu+i-1)*conv_qm
            unit2_flow(iu+i-1)=udm_int(udm_QM,unit2_flow(iu+i-1))
c------>numero di giri% fase2: da [%] a [%N]
c            unit2_rev(iu+i-1)=unit2_rev(iu+i-1)/100.
            unit2_rev(iu+i-1)=udm_int(udm_NG_perc,unit2_rev(iu+i-1))
         end if
	end do

	return
	end

c--------------------------------------------------------------------------------------
c-elena
      subroutine calcola_derivati_top_ce(ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata dopo la lettura dei dati necessari alla simulazione
c   di centrale per convertire l'UDM di alcune grandezze che nel calcolo sono usate con
c   una UDM diversa da quella interna(del DB).
c   Nello specifico i problemi riguardano:
c      portata di macchina:l'UDM interna è [m3/h] ma i calcoli sono eseguiti in [km3/h]
c      prevalenza:l'UDM interna è [m] ma i calcoli sono eseguiti in [km]
c   Vengono inoltre calcolate alcune grandezze derivate usate nei calcoli
c   (queste operazioni prima venivano eseguite in fase di lettura dalla subroutine
c    'LETTURA_TOP_CE')
c*****************************************************************************************
      implicit none

	include '../inc/param.inc'
	include '../inc/rk_param.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
      include '../inc/types.inc'
      include '../inc/mf_types.inc'
      include '../inc/stations.inc'
      include '../inc/top_app_ele.inc'
  	include '../inc/conv.inc'
	
c	include '../inc/ass_cond.inc'    



	integer*4 ivert,      !indice centrale nella struttura dati ridotta
     *          i,iu,j,jk,k,ii1,iunit_bifa,i_lim

	real*4  delta
c-ace-mod
	real*4 ro_rif_in,ro_rif_out
c-ace-mod
cgpe-prv
      integer*4 ier

	real*4 perc_equi_min, perc_equi_max
cgpe-prv-end
c-----------------------------------------------------------------------------------------

        COMMON/EQUI/perc_equi_min(max_unit), perc_equi_max(max_unit)
cmar--------------------------------------------------------------------------------------
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione  
  

	
cmar

	iu=first_unit(ivert)
      do j = 1,unit_num(ivert)
        i = iu + j - 1


cmar
       
      perc_equi_min(i)=unit_perc_min_equi(i)
	perc_equi_max(i)=unit_perc_max_equi(i)



      unit_perc_min_equi(i)=0
	unit_perc_max_equi(i)=0



cmar

        do i_lim = 1, type_nlim(i)
          type_lim_n(i_lim,i) = type_lim_n(i_lim,i)/nom_rev(i)
!-conversione per la portata di macchina da [m3/h] a [km3/h]
          type_lim_q(i_lim,i) = type_lim_q(i_lim,i)/(QMRIF*CONV_QM)
!-conversione per l'altezza adiabatica da [m] a [km]
          type_lim_h(i_lim,i) = type_lim_h(i_lim,i)/(HEADRIF*CONV_H)
        enddo
        if (unit_bifase(i)) then
          do i_lim = 1, type2_nlim(i)
            type2_lim_n(i_lim,i) = type_lim_n(i_lim,i)/nom_rev(i)
!-conversione per la portata di macchina da [m3/h] a [km3/h]
            type2_lim_q(i_lim,i) = type_lim_q(i_lim,i)/(QMRIF*CONV_QM)
!-conversione per l'altezza adiabatica da [m] a [km]
            type2_lim_h(i_lim,i) = type2_lim_h(i_lim,i)/(HEADRIF*CONV_H)
          enddo
        endif
c
        unit_flow_prog(i) = flow_prog(ivert)/unit_num(ivert)

!-------------------------->dati per criterio equidistanza
        if (tipo_criterio(ivert).eq.crit_equi) then
c-commento per prova nuove letture 28/9
c!-punto minimo di altezza adiabatica 
c          unit_min_head(i) = unit_min_head(i)/(HEADRIF*CONV_H)
c!-punto massimo di altezza adiabatica 
c          unit_max_head(i) = unit_max_head(i)/(HEADRIF*CONV_H)
c!-punto minimo di portata 
c          unit_min_flow(i) = unit_min_flow(i)/(QMRIF*CONV_QM)
c!-punto massimo di portata 
c          unit_max_flow(i) = unit_max_flow(i)/(QMRIF*CONV_QM)

c          if (unit_min_flow(i).gt.0 .and.
c     *        unit_max_flow(i).gt.0      ) then
c!-curva limite minimo del criterio di equidistanza (calcolata come K=H/Q**2)
c            unit_crit_min(i) = unit_min_head(i)/
c     *                             (unit_min_flow(i)**2)
c!-curva limite massimo del criterio di equidistanza
c            unit_crit_max(i) = unit_max_head(i)/
c     *                             (unit_max_flow(i)**2)
c          end if
c-ele 26/7/06 Calcolo di unit_crit_min e unit_crit_max secondo la nuova 
c             implementazione del criterio di equidistanza



	    call k_equi_surge(i)
	    call k_equi_choking(i)





cmar
        unit_perc_min_equi(i)=perc_equi_min(i)
        unit_perc_max_equi(i)=perc_equi_max(i)
	

cmar





c-ele
	  end if
        unit_start_cost(i) = 0.
        unit_shut_cost(i) = 0.
	end do
cmar

!-calcolo di stat_vars
      delta=1.-1.165e-4*altit(ivert)+4.76e-9*altit(ivert)**2

	iu=first_unit(ivert)
      do j=1,unit_num(ivert)
          i=iu+j-1
c-bp01
c NOTA: in input viene letta la variazione di efficienza, nelle simulazioni e'
c       utilizzata moltiplicando unit_eff = unit_eff * unit_eff_corr
          unit_eff_corr(i) = 1 + unit_eff_corr(i)
c-bp01-end
cgpe          unit_delprint(i)=unit_delprint(i)/(flow_PROG(ivert)**2)
c***************** BUG FIX 17/12/2025**************************************************
c     RICHIESTA DI MARIN: RIGA 1950 DA VALUTARESE COMMENTARE. DELTA PRESSIONE INTERMEDIA
c***************************************************************************************
         delta_in_pressione(i) = unit_delprint(i)
         unit_delprint(i)=unit_delprint(i)/(unit_flow_PROG(i)**2)
c-bp01
c NOTA: in input viene letta la variazione di potenza, nelle simulazioni e'
c       utilizzata per variare la massima potenza e contiene la correzione
c       dovuta all'altitudine (delta) e la variazione (pow_loss) data a
c       livello topologico
c-bp01          unit_power_corr(i)=delta*(1-unit_power_corr(i))
          unit_power_corr(i) = delta*(1 + unit_power_corr(i))
c NOTA: unit2_power_corr si potrebbe eliminare

c NOTA: in input viene letta la variazione di heat rate, nelle simulazioni e'
c       utilizzata moltiplicando unit_hrate = unit_hrate * unit_hrate_corr
          unit_hrate_corr(i) = 1 + unit_hrate_corr(i)
c-bp01-end
          if (unit_bifase(i)) then
            unit2_power_corr(i) = unit_power_corr(i)
c-bp01
c NOTA: in input viene letta la variazione di efficienza, nelle simulazioni e'
c       utilizzata moltiplicando unit_eff = unit_eff * unit_eff_corr
            unit2_eff_corr(i) = 1 + unit2_eff_corr(i)
c-bp01-end
          endif
      enddo
     

      stat_vars(1,ivert)  = delta
      stat_vars(2,ivert)  = flow_PROG(ivert)
      stat_vars(3,ivert)  = delpIN(ivert)/(flow_PROG(ivert)**2)
      stat_vars(4,ivert)  = delpOUT(ivert)/(flow_PROG(ivert)**2)


c-ace-mod
      ro_rif_in =1.
	ro_rif_out=1.
cmar	ro_rif_in=30.		!densità in kg/m3 (p=45bar,T=15 gradi centigradi)
cmar	ro_rif_out=45.      !densità in kg/m3 (p=70bar,T=45 gradi centigradi)
      stat_vars(3,ivert)  = stat_vars(3,ivert)*ro_rif_in
      stat_vars(4,ivert)  = stat_vars(4,ivert)*ro_rif_out
c-ace-mod
      stat_vars(5,ivert)  = dtout(ivert)
      stat_vars(6,ivert)  = max_POUT(ivert)
      stat_vars(7,ivert)  = max_TOUT(ivert)

      
      

	iu=first_unit(ivert)
      if (tipo_criterio(ivert) .eq. crit_giri) then   ! criterio dei giri
!-ricerca dell'unita' bifase
          do j=1,type_num(ivert)
            jk = first_type(ivert)+j-1
            do k=1,type_quant(jk)
              ii1=iu+k-1
              if (unit_bifase(ii1)) then
                iunit_bifa = ii1
                goto 10
              endif
            enddo
            iu = iu+type_quant(jk)
          enddo
10    continue
          if (iunit_bifa .gt. 0) then
            stat_vars(8,ivert)  = unit_delprint(iunit_bifa)*ro_rif_out
            stat_vars(9,ivert)  = unit_deltrint(iunit_bifa)   
            stat_vars(10,ivert) = unit_maxtint(iunit_bifa)
          end if
      end if

c------------------------------->Spezzata di surge
	iu=first_unit(ivert)
      do i=1,unit_num(ivert)
	   j=iu+i-1
c--------->termini noti
         type_a_coef(type_nlim(j)+1,j)=type_a_coef(type_nlim(j),j)
c--------->coefficienti angolari
         type_b_coef(type_nlim(j)+1,j)=type_b_coef(type_nlim(j),j)
         if(unit_bifase(j)) then
c--------->termini noti
           type2_a_coef(type2_nlim(j)+1,j)=type2_a_coef(type2_nlim(j),j)
c--------->coefficienti angolari
           type2_b_coef(type2_nlim(j)+1,j)=type2_b_coef(type2_nlim(j),j)
         end if
      end do

	iu = first_unit(ivert)
      do j=1,unit_num(ivert)
          i=iu+j-1
cgpe-corr        if (tipo_criterio(ivert) .ne. crit_giri) then 
cgpe-corr          unit_rev_rat(i) = 1.
cgpe-corr          unit2_rev_rat(i) = unit_rev_rat(i)
cgpe-corr        endif
        if (tipo_criterio(ivert) .ne. crit_giri) then 
          unit_rev_rat(i) = 1.
        endif
        unit2_rev_rat(i) = unit_rev_rat(i)

      enddo
cmar
    
cmar
	return
	end
c-------------------------------------------------------------------------------------------
c-sire2_0060
      subroutine calcola_derivati_cin_cs(ivert)
	implicit none

	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/scenario.inc'
      include '../inc/top_app_ele.inc'
      
      integer*4 ivert      ! I) indice centrale nella struttura dati ridotta
c-----------------------------------------------------------------------------------------
cmar_Taria      taria_cs(ivert) = taria_def(stagione)

      IF(Taria_s(ivert).EQ.R_UND)THEN
      taria_cs(ivert) = taria_def(stagione)
	ELSE
      taria_cs(ivert)=Taria_s(ivert)
      ENDIF


	return
	end
c-sire2_0060-end

      subroutine calcola_derivati_cin_ce(ivert)
c*************************************************************************************
c   Calcolo di alcune grandezze derivate dai CIN di centrale
c   Nota Le portate di progetto sono portate volumetriche, dunque non hanno bisogno di
c        conversioni
c*************************************************************************************
	implicit none

	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'
	include '../inc/ti.inc'
  	include '../inc/conv.inc'
      include '../inc/stations.inc'
      include '../inc/types.inc'
      include '../inc/units.inc'
      include '../inc/mf_units.inc'
      include '../inc/filtri.inc'
      include '../inc/air_cooler.inc'
c
      integer*4 ivert      ! I) indice centrale nella struttura dati ridotta
      real*4 delta_ac1(100)
      COMMON /DELTA/ delta_ac1
      real*4 delta_ac2(100)
      COMMON /DELTA/ delta_ac2
      integer*4 i,iu
	real*4 ro_rif_out,ro_rif_in
      real*4    fc,tadim


   



c
      ro_rif_in = 1.
	ro_rif_out= 1.

cmar	ro_rif_out=45.      !densità in kg/m3 (p=70bar,T=45 gradi centigradi)
cmar      ro_rif_in=30.
      iu = first_unit(ivert)
      if(c_f_ce_stat(ivert) .eq. fil_on) then
         c_f_ce_k(ivert)=c_f_ce_deltap(ivert)/
     *                  (c_f_ce_flow_prog(ivert)**2)*ro_rif_in

	 
      else
         c_f_ce_k(ivert)=0.
	end if
          
	do i=1,unit_num(ivert)
         if (c_f_tg_stat(iu+i-1) .eq. fil_on) then
             c_f_tg_k(iu+i-1)=c_f_tg_deltap(iu+i-1)/
     *                       (c_f_tg_flow_prog(iu+i-1)**2)*ro_rif_in

   
	   else
             c_f_tg_k(iu+i-1)=0.
	   end if
	end do


c-ele--->trattamento air cooler di centrale
      if (c_ac_ce_stat(ivert) .eq. ac_on .or. !influenza sul deltap 
     *    c_ac_ce_stat(ivert) .eq. ac_off) then
          c_ac_ce_k(ivert) = c_ac_ce_deltap(ivert)/
     *                      (c_ac_ce_flow_prog(ivert)**2)*ro_rif_out

	


	else
          c_ac_ce_k(ivert) = 0.
      end if
c-ele--->trattamento air cooler di turbogruppo
      if (stat_ord(ivert).eq.f_serie) then
          do i = 1,unit_num(ivert)
c-ele 9 novembre 2006*************
             if (unit_bifase(iu+i-1)) then
           
	          c_ac1_stat(iu+i-1) = 1.   
		      !ac intermedio sempre presente e sempre attivo 
                c_ac1_k(iu+i-1) = c_ac1_deltap(iu+i-1)/
     *                        (c_ac1_flow_prog(iu+i-1)**2)*ro_rif_out
                delta_ac1(iu+i-1) = c_ac1_deltap(iu+i-1)
                if (c_ac2_stat(iu+i-1) .eq. ac_on .or.   
     *              c_ac2_stat(iu+i-1) .eq. ac_off      ) then
                    c_ac2_k(iu+i-1) = c_ac2_deltap(iu+i-1)/
     *                           (c_ac2_flow_prog(iu+i-1)**2)*ro_rif_out
                    delta_ac2(iu+i-1) = c_ac2_deltap(iu+i-1)
	          else
                    c_ac2_k(iu+i-1) = 0.
                end if
	        else
                if (c_ac1_stat(iu+i-1) .eq. ac_on .or.   
     *              c_ac1_stat(iu+i-1) .eq. ac_off  ) then

                    c_ac1_k(iu+i-1) = c_ac1_deltap(iu+i-1)
     *                     /(c_ac1_flow_prog(iu+i-1)**2)*ro_rif_out
	          else
                    c_ac1_k(iu+i-1) = 0.
	          end if
	        end if
c-ele 9 novembre 2006 -fine-*******
          end do
	else
          do i = 1,unit_num(ivert)
             if (c_ac1_stat(iu+i-1) .eq. ac_on .or.   
     *           c_ac1_stat(iu+i-1) .eq. ac_off  ) then

                 c_ac1_k(iu+i-1) = c_ac1_deltap(iu+i-1)
     *                     /(c_ac1_flow_prog(iu+i-1)**2)*ro_rif_out
	       else
                 c_ac1_k(iu+i-1) = 0.
	       end if
             if (unit_bifase(iu+i-1)) then
               if (c_ac2_stat(iu+i-1) .eq. ac_on .or.   
     *             c_ac2_stat(iu+i-1) .eq. ac_off ) then

                   c_ac2_k(iu+i-1) = c_ac2_deltap(iu+i-1)/
     *                         (c_ac2_flow_prog(iu+i-1)**2)*ro_rif_out
	         else
                   c_ac2_k(iu+i-1) = 0.
	         end if
             end if
          end do
     	end if
  
c------------------------------->Vincoli operativi potenza
      do i = 1,unit_num(ivert)
c-bp04
         unit_vinc_maxpow(iu+i-1) = nom_power(iu+i-1)*
     *                              unit_vinc_maxpow(iu+i-1)
         unit_vinc_minpow(iu+i-1) = nom_power(iu+i-1)*
     *                              unit_vinc_minpow(iu+i-1)
c-report
         unit_vinc_maxpow_iso(iu+i-1) = unit_vinc_maxpow(iu+i-1)
         unit_vinc_minpow_iso(iu+i-1) = unit_vinc_minpow(iu+i-1)
c-report-end

CGPE-BEGIN
c I valori di unit_vinc_maxpow e unit_vinc_minpow sono valori ISO.
C Siccome vengono confrontati con valori di potenza richiesta dal
c compressore ON SITE (h<>0 e TAIR <> 15°) e' necessario correggerli con il fattore
c dovuto alla temperatura e all'altitudine.
c Correzione dovuta all'altitudine:
cmar_maxpow         unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1)*
cmar_maxpow     *                              stat_vars(1,ivert)
cmar_maxpow         unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1)*
cmar_maxpow     *                              stat_vars(1,ivert)
c Correzione dovuta alla temperatura:
         tadim = taria(IVERT)/(trif+t0)
         fc    = type_cpwt(1,iu+i-1)+tadim*(type_cpwt(2,iu+i-1)+tadim*
     *          (type_cpwt(3,iu+i-1)+type_cpwt(4,iu+i-1)*tadim))
         unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1)*fc
         unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1)*fc
c Eventuale correzione dovuta al derating della turbina
c         unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1)*(unit_power_corr(iu+i-1)/stat_vars(1,ivert))
c         unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1)*(unit_power_corr(iu+i-1)/stat_vars(1,ivert))
CGPE-END
c NOTA: come veniva calcolato precedentemente (non si teneva conto della variazione
c       di potenza data a livello topologico pow_loss) era una potenza ISO
c       Ora si vuole calcolare una percentuale della potenza nominale (dunque un valore ISO)
c       con la variazione della potenza e viene corretta anche con il fattore dell'altitudine
c-bp04   unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1) *
c-bp04     *            (nom_power(iu+i-1) * unit_power_corr(iu+i-1))
c-bp04         unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1) *
c-bp04     *            (nom_power(iu+i-1) * unit_power_corr(iu+i-1))
c-bp04-???         unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1) *
c-bp04-???     *(nom_power(iu+i-1) * unit_power_corr(iu+i-1)/stat_vars(1,ivert))
c-bp04-???         unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1) *
c-bp04-???     *(nom_power(iu+i-1) * unit_power_corr(iu+i-1)/stat_vars(1,ivert))
      
c NOTA: Il vincolo operativo sul massimo numero di giri memorizzato unit_vinc_maxrev
c       rappresenta una percentuale rispetto al nominale (nom_rev) ed e' gia' del tipo
c       utilizzato nei calcoli (e' del tipo max_rev)
c-bp04-end
cmar_max_pow
      unit_vinc_maxpow(iu+i-1) = unit_vinc_maxpow(iu+i-1)*
     *                                        unit_power_corr(iu+i-1)
cmar_max_pow
c unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1)*(unit_power_corr(iu+i-1)/stat_vars(1,ivert))

cmar_max_pow
      unit_vinc_minpow(iu+i-1) = unit_vinc_minpow(iu+i-1)*
     *                                        unit_power_corr(iu+i-1)
cmar_max_pow


         unit_avail_cin(iu+i-1) = unit_avail(iu+i-1)

      end do


       
	return
	end
cgpe
cgpe
      subroutine calcola_out_serie(istaz,pgr,pgr_app)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per ricavare alcuni valori non calcolati direttamente.Vengono inoltre 
c   convertite all'UDM interna portata di macchina,altezza adiabatica e giri% usati nei 
c   calcoli con una UDM diversa
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
c      include '../inc/rk.inc'
c	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
c      include '../inc/units.inc'
c      include '../inc/types.inc'
c      include '../inc/mf_units.inc'
c      include '../inc/mf_types.inc'
c      include '../inc/argo_reale_ris.inc'

	                        !(calcolato dalla composizione)

      integer*4 istaz,      !I)indice centrale nella struttura dati completa
     *          pgr,         !indice centrale nella struttura dati ridotta
     *          pgr_app      !indice centrale nella struttura dati ridotta, secondo stadio
      real*4    r_val
c------------------------------------------------------------------------------------------
      flow_stat_in(pgr) = flow_stat_in(pgr) + tot_cons(pgr_app)
	flow_stat_in(pgr_app) = flow_stat(pgr_app)

c-report
      nom_max_pow(pgr) = nom_max_pow(pgr) + nom_max_pow(pgr_app)
      nom_max_pow_der(pgr) = nom_max_pow_der(pgr) + 
     *                       nom_max_pow_der(pgr_app)
      nom_max_pow_vinc(pgr) = nom_max_pow_vinc(pgr) +
     *                        nom_max_pow_vinc(pgr_app)
      r_val = min(nom_min_pow(pgr),nom_min_pow(pgr_app))
      nom_min_pow(pgr) = r_val
c-report-end

      return
      end
cgpe-end
c******************************************************************************************
	subroutine k_equi_surge_2151(i_tg)
c-ele 26/7/06-------------------------------------------------------------------------------
c Restituisce la costante che individua la parabola limite minimo del criterio di 
c equidistanza, ossia la parabola passante per il punto che sulla curva al massimo 
c numero di giri si discosta della percentuale assegnata da utente dal punto sulla
c spezzata di surge allo stesso numero di giri.Si verifica comunque che la parabola calcolata
c cada a destra di tutti i punti limite per evitare  che il limite minimo cada fuori mappa.
c---------------------------------------------------------------------------------------- 
	implicit none
	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'

      include '../inc/TYPES.INC'
      include '../inc/units.INC'

	COMMON/equi_flag/flag_equi_s(max_unit)
 
      logical*2 flag_equi_s


	integer*4 i_tipo,i_tg,stat_ord
      real*4 k_surge_min
	external k_surge_min
      
	real*4 kmin,qq,hh,qauxv

	kmin=k_surge_min(type_nlim(i_tg),type_lim_q(1,i_tg),
     *                             type_lim_h(1,i_tg))
      unit_crit_min(i_tg) = kmin

	qq=type_lim_q(type_nlim(i_tg),i_tg)*(1+unit_perc_min_equi(i_tg))
!--SM--13/11/06--
      if(qq.le.0.)then
         qq=0.001
      endif
!--SM--13/11/06--

	call hdaqn_a(hh,qq,max_rev(i_tg),type_chn(1,i_tg))
	kmin=hh/qq**2
cmar
	if(kmin .eq.unit_crit_min(i_tg)) flag_equi_s(i_tg) = .true.
cmar
	if (kmin.lt.unit_crit_min(i_tg))unit_crit_min(i_tg)=kmin


	return
	end
cmar
cmar-equi-02-07-09
	subroutine k_equi_surge_mm(i_tg)
c-ele 26/7/06-------------------------------------------------------------------------------
c Restituisce la costante che individua la parabola limite minimo del criterio di 
c equidistanza, ossia la parabola passante per il punto che sulla curva al massimo 
c numero di giri si discosta della percentuale assegnata da utente dal punto sulla
c spezzata di surge allo stesso numero di giri.Si verifica comunque che la parabola calcolata
c cada a destra di tutti i punti limite per evitare  che il limite minimo cada fuori mappa.
c---------------------------------------------------------------------------------------- 
	implicit none
	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'

      include '../inc/TYPES.INC'
      include '../inc/units.INC'

	COMMON/equi_flag/flag_equi_s(max_unit)
 
      logical*2 flag_equi_s


	integer*4 i_tipo,i_tg,stat_ord
      real*4 k_surge_min, k_min_rev, k_surge_min_rev
	external k_surge_min, k_surge_min_rev
	real*4 kmin,qq,hh,qauxv



	kmin=k_surge_min(type_nlim(i_tg),type_lim_q(1,i_tg),
     *                             type_lim_h(1,i_tg))
      unit_crit_min(i_tg) = kmin

	qq=type_lim_q(type_nlim(i_tg),i_tg)*(1+unit_perc_min_equi(i_tg))
!--SM--13/11/06--
      if(qq.le.0.)then
         qq=0.001
      endif
!--SM--13/11/06--

	call hdaqn_a(hh,qq,max_rev(i_tg),type_chn(1,i_tg))
	kmin=hh/qq**2

	k_min_rev=k_surge_min_rev(type_nlim(i_tg),type_lim_q(1,i_tg),
     *                             type_lim_h(1,i_tg))
cmar
c	if(k_min_rev .eq.unit_crit_min(i_tg)) flag_equi_s(i_tg) = .true
      if(k_min_rev .eq.kmin) flag_equi_s(i_tg) = .true.
cmar
	if (kmin.lt.unit_crit_min(i_tg))unit_crit_min(i_tg)=kmin


	return
	end
cmar
c****************************************************************************************
	subroutine k_equi_choking_mm(i_tg)
c-ele 26/7/06----------------------------------------------------------------------------
c Restituisce la costante che individua la parabola limite massimo del criterio di 
c equidistanza, ossia la parabola passante per il punto che sulla curva al massimo 
c numero di giri si discosta della percentuale assegnata da utente dal punto sulla
c operating limit allo stesso numero di giri.Si verifica comunque che l'operating limit 
c cada a destra della parabola calcolata.
c---------------------------------------------------------------------------------------- 
	implicit none
	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'
      include '../inc/TYPES.INC'
      include '../inc/units.INC'

      integer*4 i_tg
	real*4 qq,hh,kmax

      call qdan_ch_a(type_c_coef(4,i_tg),qq,
     *               max_rev(i_tg),type_chc(1,i_tg))
	qq=qq*(1-unit_perc_max_equi(i_tg))
!--SM--13/11/06--
      if(qq.le.0.)then
         qq=0.001
      endif
!--SM--13/11/06--

	call hdaqn_a(hh,qq,max_rev(i_tg),type_chc(1,i_tg))
	kmax=hh/qq**2
      if (type_c_coef(4,i_tg).gt.kmax) then
         unit_crit_max(i_tg) = type_c_coef(4,i_tg)
      else
         unit_crit_max(i_tg) = kmax
      endif

      return 
	end

cmar
	subroutine k_equi_surge(i_tg)
c-ele 26/7/06-------------------------------------------------------------------------------
c Restituisce la costante che individua la parabola limite minimo del criterio di 
c equidistanza, ossia la parabola passante per il punto che sulla curva al massimo 
c numero di giri si discosta della percentuale assegnata da utente dal punto sulla
c spezzata di surge allo stesso numero di giri.Si verifica comunque che la parabola calcolata
c cada a destra di tutti i punti limite per evitare  che il limite minimo cada fuori mappa.
c---------------------------------------------------------------------------------------- 
	implicit none
	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'

      include '../inc/TYPES.INC'
      include '../inc/units.INC'

	COMMON/equi_flag/flag_equi_s(max_unit)
 
      logical*2 flag_equi_s
cmar-equi-05-08-09
     
	COMMON/equi_sur/kmin_sur(max_unit) 

	real*4 kmin_sur

cmar-equi-05-08-09

	integer*4 i_tipo,i_tg,stat_ord
      real*4 k_surge_min, k_min_rev, k_surge_min_rev
	external k_surge_min, k_surge_min_rev
	real*4 kmin,qq,hh,qauxv



	kmin=k_surge_min(type_nlim(i_tg),type_lim_q(1,i_tg),
     *                             type_lim_h(1,i_tg))
      unit_crit_min(i_tg) = kmin

	qq=type_lim_q(type_nlim(i_tg),i_tg)*(1+unit_perc_min_equi(i_tg))
!--SM--13/11/06--
      if(qq.le.0.)then
         qq=0.001
      endif
!--SM--13/11/06--

	call hdaqn_a(hh,qq,max_rev(i_tg),type_chn(1,i_tg))
	kmin=hh/qq**2

	k_min_rev=k_surge_min_rev(type_nlim(i_tg),type_lim_q(1,i_tg),
     *                             type_lim_h(1,i_tg))
cmar
c	if(k_min_rev .eq.unit_crit_min(i_tg)) flag_equi_s(i_tg) = .true
      if(k_min_rev .eq.kmin) flag_equi_s(i_tg) = .true.
cmar
	if (kmin.lt.unit_crit_min(i_tg))unit_crit_min(i_tg)=kmin
cmar-equi-05-08-09
      kmin_sur(i_tg) = unit_crit_min(i_tg)
cmar-equi-05-08-09

	return
	end
cmar
c****************************************************************************************
	subroutine k_equi_choking(i_tg)
c-ele 26/7/06----------------------------------------------------------------------------
c Restituisce la costante che individua la parabola limite massimo del criterio di 
c equidistanza, ossia la parabola passante per il punto che sulla curva al massimo 
c numero di giri si discosta della percentuale assegnata da utente dal punto sulla
c operating limit allo stesso numero di giri.Si verifica comunque che l'operating limit 
c cada a destra della parabola calcolata.
c---------------------------------------------------------------------------------------- 
	implicit none
	include '../inc/param.inc'
	include '../inc/rk_param.inc'
      include '../inc/stazione.inc'
      include '../inc/TYPES.INC'
      include '../inc/units.INC'
cmar-equi-05-08-09

	COMMON/equi_cho/kmax_cho(max_unit) 

	real*4 kmax_cho
cmar-equi-05-08-09

      integer*4 i_tg
	real*4 qq,hh,kmax

      call qdan_ch_a(type_c_coef(4,i_tg),qq,
     *               max_rev(i_tg),type_chc(1,i_tg))
	qq=qq*(1-unit_perc_max_equi(i_tg))
!--SM--13/11/06--
      if(qq.le.0.)then
         qq=0.001
      endif
!--SM--13/11/06--

	call hdaqn_a(hh,qq,max_rev(i_tg),type_chc(1,i_tg))
	kmax=hh/qq**2
      if (type_c_coef(4,i_tg).gt.kmax) then
         unit_crit_max(i_tg) = type_c_coef(4,i_tg)
      else
         unit_crit_max(i_tg) = kmax
      endif
cmar-equi-05-08-09

      kmax_cho(i_tg) = unit_crit_max(i_tg)
cmar-equi-05-08-09

      return 
	end
cmar
c**************************************************************************
      real*4 function k_surge_min(nlim,qlim,hlim)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
	implicit none
!--SM--17/11/06--
	include '../inc/param.inc'
!--SM--17/11/06--

	integer*4 nlim
	real*4 qlim(*),hlim(*)

	integer*4 i
	real*4 k

!--SM--17/11/06--
      if(qlim(1).ne.0.)then
         k_surge_min= hlim(1)/qlim(1)**2
      else
         k_surge_min= r_und
      endif
!--SM--17/11/06--
cmar-equi
	do i=2,nlim
	   k=hlim(i)/qlim(i)**2
	   if (k.lt.k_surge_min) k_surge_min=k
      end do
cmar-equi      
	return
	end

cmar-equi---------------

      real*4 function k_surge_min_rev(nlim,qlim,hlim)
c-ele 26/7/06------------------------------------------------------------------------------
! Calcola la parabola a destra dell'antipompaggio sperimentale:
! considera le parabole del tipo h=k*q^2 passanti per i punti limite del compressore
! che definiscono la spezzata di surge e restituisce la costante k che individua la 
! parabola del fascio che sta a destra delle altre, ossia il minimo valore di k calcolato.
! In genere tale k è quello che individua la parabola passante per il punto al massimo
! numero di giri .In particolare ciò si verifica se le pendenze dei segmenti della spezzata
! hanno pendenze crescenti.
!----------------------------------------------------------------------------------------
cmar-equi
!
!function per il calcolo dellla parabola k_surge_min_rev passante per il minimo num giri e surge.
!
cmar-equi

	implicit none
!--SM--17/11/06--
	include '../inc/param.inc'
!--SM--17/11/06--

	integer*4 nlim
	real*4 qlim(*),hlim(*)

	integer*4 i
	real*4 k

!--SM--17/11/06--
      if(qlim(1).ne.0.)then
         k_surge_min_rev= hlim(1)/qlim(1)**2
      else
         k_surge_min_rev= r_und
      endif
!--SM--17/11/06--
cmar-equi
c	do i=2,nlim
c	   k=hlim(i)/qlim(i)**2
c	   if (k.lt.k_surge_min) k_surge_min=k
c     end do
cmar-equi      
	return
	end

cmar-equi---------------
cgpe-new
      subroutine calcola_derivati_out_cs(istaz,ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per ricavare alcuni valori non calcolati direttamente.Vengono inoltre 
c   convertite all'UDM interna portata di macchina,altezza adiabatica e giri% usati nei 
c   calcoli con una UDM diversa
c*****************************************************************************************
	implicit none

      include '../inc/param.inc'
      include '../inc/top_app_ele.inc'

      integer*4  istaz,ivert    ! I)
c------------------------------------------------------------------------------------------
c-sire2_0060
c-sire2_0060      call calc_out_cs (istaz,head_cs(ivert),flowm_cs(ivert),
c-sire2_0060     *     power_cs(ivert))
      call calc_out_cs (istaz,cpwt_def,taria_cs(ivert),
     *     head_cs(ivert),flowm_cs(ivert),power_cs(ivert))
c-sire2_0060-end

	return
	end

c-sire2_0060      Subroutine calc_out_cs (istaz,head,flowm,power)
      Subroutine calc_out_cs_ok_orig (istaz,cpwt,taria,head,flowm,power)
c-sire2_0060-end

C->   CALCOLO L'ALTEZZA ADIABATICA E LA PORTATA IN CONDIZIONI MACCHINA 
C->   DI UNA CENTRALE SEMPLIFICATA

      implicit none
      include '../inc/param.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'

      integer*4 istaz       ! I) indice della centrale nel vettore completo
      real*4    cpwt(*)     ! I) coefficienti per la correzione dovuta alla taria
     *         ,taria       ! I) 

      real*4    head        ! O) altezza adiabatica
     *         ,flowm       ! O) portata in condizioni macchina
     *         ,power       ! O) potenza della centrale

      integer*4 im,iv
      real*4    pmin,tmin,pmax,tmax
      real*4    ZCN,RCN,ROCN,pmol
      real*4    pasp,pman,tasp,tman,delpr1,delpr2,flow,pasp_min,pman_min
      real*4    z,exp_pol,esp,aux1,aux2,aux3,comp_ratio,
     *          uno_su_ro_attuale,pin,pout,z_coef(6)
      real*4    prop,dp_in,dp_out
      EXTERNAL  prop,dp_in,dp_out

c-sire2_0060
      integer*4 ivert
      real*4    h,delta,tadim,fc
	integer*4 ier
c-sire2_0060-end
c-----------------------------------------------------------------------
c  INIZIALIZZAZIONI
      head  = 0.
	flowm = 0.
      power = 0.

c  Calcolo degli z_coef
cint_z
      pmin=5.
      pmax=300.

cint_z-end
      tmin=260.
      tmax=360.
      call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef)

c  Calcolo delle grandezze (pmol,rocn) dipendenti dalla composizione del
c  gas necessarie al calcolo dell'altezza adiabatica e della portata di
c  macchina una centrale semplificata
      pmol = av_weight(istaz)
CMAR      ZCN  = prop(PCN,TCN,z_coef)
        call zpt1(istaz,PCN,TCN,ZCN)

      ROCN = (pmol*PCN)/(erre*ZCN*TCN)*(1.e5)

      im = opumto(istaz)
      iv = opuvto(istaz)
	pasp = prestj(im)
	pman = prestj(iv)
	tasp = temptj(im)
	tman = temptj(iv)
      flow = porttj(istaz)

ccs-corr
      if (hstatj(istaz) .eq. sta_on .or.
     *    hstatj(istaz) .eq. sta_fr     ) then
        return
      endif
ccs-corr-end

      if (flow .gt. 0. .and. pasp .lt. pman) then
        pasp_min = pmintx(im)
        pman_min = pmintx(iv)

c---->  calcolo delle cadute di pressione dovute al piping
        delpr1 = dp_in(istaz,pasp,tasp,flow)
        pasp = MAX(pasp-delpr1,pasp_min)

        delpr2 = dp_out(istaz,pman,tman,flow)
        pman = MAX(pman+delpr2,pman_min)
        if (pasp .lt. pasp_min) return
        if (pman .lt. pman_min) return

        pin  = pasp - delpr1
        pout = pman + delpr2


CMAR        z = prop(pin,tasp,z_coef)
       call zpt1(istaz,pin,tasp,z)
CMAR
      comp_ratio = pout/pin
	call politrop_esp(istaz,pin,tasp,comp_ratio,exp_pol,ier)


CMAR        exp_pol = prop(pin,tasp,exp_coef(1,istaz))

        esp = (exp_pol-1.)/exp_pol

CMAR        comp_ratio = pout/pin
c---->  calcolo dell'altezza adiabatica
        aux1 = (erre / agrvcc / pmol) * Z * tasp/esp
        aux2 = comp_ratio**esp
        head = aux1 * (aux2-1.)                        ! in km
c---->  calcolo della portata in condizioni macchina
        uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pin
c coeff per passare da normal a machine conditions (flow_mach=flow_norm*aux3)
        aux3 = ROCN*uno_su_ro_attuale
        flowm = flow*aux3

ccs-corr
c calcolo della potenza ON SITE [kW]
        power = cpwccc/esp*flow*z*tasp/rendtx(istaz) 
     -          *((comp_ratio)**esp - 1.0)
        power = MAX(power,0.0)
ccs-corr-end

c-sire2_0060 calcolo della potenza ISO [kW]
c correzione dovuta all'altitudine
        h = (altetx(im) + altetx(iv)) / 2.
        delta = 1.- 1.165e-4 * h + 4.76e-9 * h**2
        power = power/delta

c correzione dovuta alla temperatura dell'aria
        ivert = pgrato(istaz)
        tadim = taria/(trif+t0)
        fc    = cpwt(1)+tadim*(cpwt(2)+tadim*
     *          (cpwt(3)+cpwt(4)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
        power = power/fc
c-sire2_0060-end

      endif

      return ! calc_out_cs
      end

cmar-p-30-07-09----------------------------------------------------------
         Subroutine calc_out_cs (istaz,cpwt,taria,head,flowm,power)
c-sire2_0060-end

C->   CALCOLO L'ALTEZZA ADIABATICA E LA PORTATA IN CONDIZIONI MACCHINA 
C->   DI UNA CENTRALE SEMPLIFICATA

      implicit none
      include '../inc/param.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'
	include '../inc/top_app_ele.inc '

      integer*4 istaz       ! I) indice della centrale nel vettore completo
      real*4    cpwt(*)     ! I) coefficienti per la correzione dovuta alla taria
     *         ,taria       ! I) 

      real*4    head        ! O) altezza adiabatica
     *         ,flowm       ! O) portata in condizioni macchina
     *         ,power       ! O) potenza della centrale

      integer*4 im,iv
      real*4    pmin,tmin,pmax,tmax
      real*4    ZCN,RCN,ROCN,pmol
      real*4    pasp,pman,tasp,tman,delpr1,delpr2,flow,pasp_min,pman_min
      real*4    z,exp_pol,esp,aux1,aux2,aux3,comp_ratio,
     *          uno_su_ro_attuale,pin,pout,z_coef(6)
      real*4    prop,dp_in,dp_out
      EXTERNAL  prop,dp_in,dp_out

c-sire2_0060
      integer*4 ivert
      real*4    h,delta,tadim,fc
	integer*4 ier
c-sire2_0060-end
 

cmar-dp_new_cs-31-07-09

      real*4 tout

cmar-dp_new_cs-31-07-09
     
cmar_add_27_09_11
cc      real*4 Pn_corr(max_cs)
cmar_add_27_09_11


c-----------------------------------------------------------------------
c  INIZIALIZZAZIONI
      head  = 0.
	flowm = 0.
      power = 0.
	Pn_corr=0.
	dp_asp_cs(pgrato(istaz))= 0.
      dp_man_cs(pgrato(istaz))= 0.

c  Calcolo degli z_coef
cint_z
      pmin=5.
      pmax=300.

cint_z-end
      tmin=260.
      tmax=360.
      call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef)


c  Calcolo delle grandezze (pmol,rocn) dipendenti dalla composizione del
c  gas necessarie al calcolo dell'altezza adiabatica e della portata di
c  macchina una centrale semplificata
      pmol = av_weight(istaz)
CMAR      ZCN  = prop(PCN,TCN,z_coef)
        call zpt1(istaz,PCN,TCN,ZCN)

      ROCN = (pmol*PCN)/(erre*ZCN*TCN)*(1.e5)

      im = opumto(istaz)
      iv = opuvto(istaz)
	pasp = prestj(im)
	pman = prestj(iv)
	tasp = temptj(im)
	tman = temptj(iv)
      flow = porttj(istaz)



ccs-corr
      if (hstatj(istaz) .eq. sta_on .or.
     *    hstatj(istaz) .eq. sta_fr     ) then
        return
      endif
ccs-corr-end

      if (flow .gt. 0. .and. pasp .lt. pman) then
        pasp_min = pmintx(im)
        pman_min = pmintx(iv)

c---->  calcolo delle cadute di pressione dovute al piping
cmar-dp_new_cs-31-07-09
      

cmar-dp_new_cs-31-07-09
c        flow_dp = flow/qmaxtx(istaz)

        delpr1 = dp_in(istaz,pasp,tasp,flow)
        pasp = MAX(pasp-delpr1,pasp_min)
cmar_add
        dp_asp_cs(pgrato(istaz))=delpr1
cmar_add
        delpr2 = dp_out(istaz,pman,tman,flow)
        pman = MAX(pman+delpr2,pman_min)

cmar_add
       dp_man_cs(pgrato(istaz))=delpr2
cmar_add
        if (pasp .lt. pasp_min) return
        if (pman .lt. pman_min) return

c        pin  = pasp - delpr1
c        pout = pman + delpr2

cmar
        pin  = pasp 

        pout = pman 
   
cmar
CMAR        z = prop(pin,tasp,z_coef)
       call zpt1(istaz,pin,tasp,z)
CMAR
      comp_ratio = pout/pin
	call politrop_esp(istaz,pin,tasp,comp_ratio,exp_pol,ier)


CMAR        exp_pol = prop(pin,tasp,exp_coef(1,istaz))

        esp = (exp_pol-1.)/exp_pol

CMAR        comp_ratio = pout/pin
c---->  calcolo dell'altezza adiabatica
        aux1 = (erre / agrvcc / pmol) * Z * tasp/esp
        aux2 = comp_ratio**esp
        head = aux1 * (aux2-1.)                        ! in km
c---->  calcolo della portata in condizioni macchina
        uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pin
c coeff per passare da normal a machine conditions (flow_mach=flow_norm*aux3)
        aux3 = ROCN*uno_su_ro_attuale
        flowm = flow*aux3


      call single_out_temp_new_cs(istaz, tman,tasp,z,pasp,pman,aux2,

     *           dh_coef(1,istaz),cp_par(1,istaz) ,rendtx(istaz))
cmar-cs-30-05-11

	if(tman.gt.tarctx(istaz))then
cmar abbattimento da air-cooler ideale

      tman=tarctx(istaz)
   
      endif

cmar-cs-30-05-11

cmar-cs-06-08-09

      temptj(iv)= tman

cmar-cs-06-08-09



ccs-corr
c calcolo della potenza ON SITE [kW]
        power = cpwccc/esp*flow*z*tasp/rendtx(istaz) 
     -          *((comp_ratio)**esp - 1.0)
        power = MAX(power,0.0)
ccs-corr-end

c-sire2_0060 calcolo della potenza ISO [kW]
c correzione dovuta all'altitudine
        h = (altetx(im) + altetx(iv)) / 2.
        delta = 1.- 1.165e-4 * h + 4.76e-9 * h**2
        power = power/delta

c correzione dovuta alla temperatura dell'aria
        ivert = pgrato(istaz)
        tadim = taria/(trif+t0)
        fc    = cpwt(1)+tadim*(cpwt(2)+tadim*
     *          (cpwt(3)+cpwt(4)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
        power = power/fc
c-sire2_0060-end

cmar_add_27_09_11
      Pn_corr=power/powmax
cmar_add_27_09_11
	
	endif

      return ! calc_out_cs
      end


cmar-p-31-07-09----------------------------------------------------------


      Subroutine calc_out_cs_MMMM (istaz,cpwt,taria,head,flowm,power)
c-sire2_0060-end

C->   CALCOLO L'ALTEZZA ADIABATICA E LA PORTATA IN CONDIZIONI MACCHINA 
C->   DI UNA CENTRALE SEMPLIFICATA

      implicit none
      include '../inc/param.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'

      integer*4 istaz       ! I) indice della centrale nel vettore completo
      real*4    cpwt(*)     ! I) coefficienti per la correzione dovuta alla taria
     *         ,taria       ! I) 

      real*4    head        ! O) altezza adiabatica
     *         ,flowm       ! O) portata in condizioni macchina
     *         ,power       ! O) potenza della centrale

      integer*4 im,iv
      real*4    pmin,tmin,pmax,tmax
      real*4    ZCN,RCN,ROCN,pmol
      real*4    pasp,pman,tasp,tman,delpr1,delpr2,flow,pasp_min,pman_min
      real*4    z,exp_pol,esp,aux1,aux2,aux3,comp_ratio,
     *          uno_su_ro_attuale,pin,pout,z_coef(6)
      real*4    prop,dp_in,dp_out
      EXTERNAL  prop,dp_in,dp_out

c-sire2_0060
      integer*4 ivert
      real*4    h,delta,tadim,fc
	integer*4 ier
c-sire2_0060-end
c-----------------------------------------------------------------------
c  INIZIALIZZAZIONI
      head  = 0.
	flowm = 0.
      power = 0.

c  Calcolo degli z_coef
cint_z
      pmin=5.
      pmax=300.

cint_z-end
      tmin=260.
      tmax=360.
      call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef)

c  Calcolo delle grandezze (pmol,rocn) dipendenti dalla composizione del
c  gas necessarie al calcolo dell'altezza adiabatica e della portata di
c  macchina una centrale semplificata
      pmol = av_weight(istaz)
CMAR      ZCN  = prop(PCN,TCN,z_coef)
        call zpt1(istaz,PCN,TCN,ZCN)

      ROCN = (pmol*PCN)/(erre*ZCN*TCN)*(1.e5)

      im = opumto(istaz)
      iv = opuvto(istaz)
	pasp = prestj(im)
	pman = prestj(iv)
	tasp = temptj(im)
	tman = temptj(iv)
      flow = porttj(istaz)

ccs-corr
      if (hstatj(istaz) .eq. sta_on .or.
     *    hstatj(istaz) .eq. sta_fr     ) then
        return
      endif
ccs-corr-end

      if (flow .gt. 0. .and. pasp .lt. pman) then
        pasp_min = pmintx(im)
        pman_min = pmintx(iv)

c---->  calcolo delle cadute di pressione dovute al piping
        delpr1 = dp_in(istaz,pasp,tasp,flow)
        pasp = MAX(pasp-delpr1,pasp_min)

        delpr2 = dp_out(istaz,pman,tman,flow)
        pman = MAX(pman+delpr2,pman_min)
        if (pasp .lt. pasp_min) return
        if (pman .lt. pman_min) return

cmar-p-30-07-09
cmar-p-30-07-09        pin  = pasp - delpr1
cmar-p-30-07-09        pout = pman + delpr2
cmar-p-30-07-09





cmar-p-30-07-09

      pin=pasp
	pout=pman

cmar-p-30-07-09

CMAR        z = prop(pin,tasp,z_coef)
       call zpt1(istaz,pin,tasp,z)
CMAR
      comp_ratio = pout/pin
	call politrop_esp(istaz,pin,tasp,comp_ratio,exp_pol,ier)


CMAR        exp_pol = prop(pin,tasp,exp_coef(1,istaz))

        esp = (exp_pol-1.)/exp_pol

CMAR        comp_ratio = pout/pin
c---->  calcolo dell'altezza adiabatica
        aux1 = (erre / agrvcc / pmol) * Z * tasp/esp
        aux2 = comp_ratio**esp
        head = aux1 * (aux2-1.)                        ! in km
c---->  calcolo della portata in condizioni macchina
        uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pin
c coeff per passare da normal a machine conditions (flow_mach=flow_norm*aux3)
        aux3 = ROCN*uno_su_ro_attuale
        flowm = flow*aux3
cmar-p-30-07-09
cmar
cmar++++++++calcolo della temperatura di uscita, come se fosse una C.le bipolare

     

c      call single_out_temp_new_cs(tout,tin,z,pin,

c     *          pout,aux2, z_coef,dh_coef,cp_par(istaz),effic)

 
      

cmar-p-30-07-09++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ccs-corr
c calcolo della potenza ON SITE [kW]
        power = cpwccc/esp*flow*z*tasp/rendtx(istaz) 
     -          *((comp_ratio)**esp - 1.0)
        power = MAX(power,0.0)
ccs-corr-end

c-sire2_0060 calcolo della potenza ISO [kW]
c correzione dovuta all'altitudine
        h = (altetx(im) + altetx(iv)) / 2.
        delta = 1.- 1.165e-4 * h + 4.76e-9 * h**2
        power = power/delta

c correzione dovuta alla temperatura dell'aria
        ivert = pgrato(istaz)
        tadim = taria/(trif+t0)
        fc    = cpwt(1)+tadim*(cpwt(2)+tadim*
     *          (cpwt(3)+cpwt(4)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
        power = power/fc
c-sire2_0060-end

      endif

      return ! calc_out_cs
      end

cmar-p-30-07-09 ----------------------------------------------------------------          
      Subroutine calc_out_cs_30_07(istaz,cpwt,taria,head,flowm,power)
c-sire2_0060-end

C->   CALCOLO L'ALTEZZA ADIABATICA E LA PORTATA IN CONDIZIONI MACCHINA 
C->   DI UNA CENTRALE SEMPLIFICATA

      implicit none
      include '../inc/param.inc'
      include '../inc/rk.inc'
      include '../inc/cc.inc'
      include '../inc/ti.inc'
      include '../inc/tx.inc'
      include '../inc/tj.inc'
      include '../inc/th.inc'

     

     
      integer*4 istaz       ! I) indice della centrale nel vettore completo
      real*4    cpwt(*)     ! I) coefficienti per la correzione dovuta alla taria
     *         ,taria       ! I) 

      real*4    head        ! O) altezza adiabatica
     *         ,flowm       ! O) portata in condizioni macchina
     *         ,power       ! O) potenza della centrale

      integer*4 im,iv
      real*4    pmin,tmin,pmax,tmax
      real*4    ZCN,RCN,ROCN,pmol
      real*4    pasp,pman,tasp,tman,delpr1,delpr2,flow,pasp_min,pman_min
      real*4    z,exp_pol,esp,aux1,aux2,aux3,comp_ratio,
     *          uno_su_ro_attuale,pin,pout,z_coef(6)
      real*4    prop,dp_in,dp_out
      EXTERNAL  prop,dp_in,dp_out

c-sire2_0060
      integer*4 ivert
      real*4    h,delta,tadim,fc
	integer*4 ier
c-sire2_0060-end

cmar-p-30-07-09        
      real*4 tout
	integer*4 i
cmar-p-30-07-09        

c-----------------------------------------------------------------------
c  INIZIALIZZAZIONI
      head  = 0.
	flowm = 0.
      power = 0.

c  Calcolo degli z_coef
cint_z
      pmin=5.
      pmax=300.

cint_z-end
      tmin=260.
      tmax=360.
      call regre_z(istaz,pmin,tmin,pmax,tmax,z_coef)

c  Calcolo delle grandezze (pmol,rocn) dipendenti dalla composizione del
c  gas necessarie al calcolo dell'altezza adiabatica e della portata di
c  macchina una centrale semplificata
      pmol = av_weight(istaz)
CMAR      ZCN  = prop(PCN,TCN,z_coef)
        call zpt1(istaz,PCN,TCN,ZCN)

      ROCN = (pmol*PCN)/(erre*ZCN*TCN)*(1.e5)

      im = opumto(istaz)
      iv = opuvto(istaz)
	pasp = prestj(im)
	pman = prestj(iv)
	tasp = temptj(im)
	tman = temptj(iv)
      flow = porttj(istaz)

ccs-corr
      if (hstatj(istaz) .eq. sta_on .or.
     *    hstatj(istaz) .eq. sta_fr     ) then
        return
      endif
ccs-corr-end

      if (flow .gt. 0. .and. pasp .lt. pman) then
        pasp_min = pmintx(im)
        pman_min = pmintx(iv)

c---->  calcolo delle cadute di pressione dovute al piping
        delpr1 = dp_in(istaz,pasp,tasp,flow)
        pasp = MAX(pasp-delpr1,pasp_min)

        delpr2 = dp_out(istaz,pman,tman,flow)
        pman = MAX(pman+delpr2,pman_min)
        if (pasp .lt. pasp_min) return
        if (pman .lt. pman_min) return

cmar-p-30-07-09
       pin  = pasp - delpr1
       pout = pman + delpr2
cmar-p-30-07-09

CMAR        z = prop(pin,tasp,z_coef)
cmar-p-30-07-09       call zpt1(istaz,pin,tasp,z)
CMAR
cmar-p-30-07-09  
       call zpt1(istaz,pasp,tasp,z)
    

cmar-p-30-07-09      comp_ratio = pout/pin
cmar-p-30-07-09      
      comp_ratio = pman/pasp

cmar-p-30-07-09  	call politrop_esp(istaz,pin,tasp,comp_ratio,exp_pol,ier)
cmar-p-30-07-09  	
      call politrop_esp(istaz,pasp,tasp,comp_ratio,exp_pol,ier)

CMAR        exp_pol = prop(pin,tasp,exp_coef(1,istaz))

        esp = (exp_pol-1.)/exp_pol

CMAR        comp_ratio = pout/pin
c---->  calcolo dell'altezza adiabatica
        aux1 = (erre / agrvcc / pmol) * Z * tasp/esp
        aux2 = comp_ratio**esp
        head = aux1 * (aux2-1.)                        ! in km
c---->  calcolo della portata in condizioni macchina

cmar-p-30-07-09        uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pin
cmar-p-30-07-09        
      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*Z*tasp/pasp
c coeff per passare da normal a machine conditions (flow_mach=flow_norm*aux3)
        aux3 = ROCN*uno_su_ro_attuale
        flowm = flow*aux3

cmar-p-30-07-09        
cmar +++++++++ cerco di calcolare la Tout, come se fosse una Cle Bipolare ++++++ 

      
        pin  = pasp 
        pout = pman 

c      rendtx(istaz)=75.27
      call single_out_temp_new_cs(istaz,tout,tasp,z,pasp,pman,aux2,

     *           dh_coef(1,istaz),cp_par(1,istaz) ,rendtx(istaz))


cmar-p-30-07-09 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++        

ccs-corr
c calcolo della potenza ON SITE [kW]
        power = cpwccc/esp*flow*z*tasp/rendtx(istaz) 
     -          *((comp_ratio)**esp - 1.0)
        power = MAX(power,0.0)
ccs-corr-end

c-sire2_0060 calcolo della potenza ISO [kW]
c correzione dovuta all'altitudine
        h = (altetx(im) + altetx(iv)) / 2.
        delta = 1.- 1.165e-4 * h + 4.76e-9 * h**2
        power = power/delta

c correzione dovuta alla temperatura dell'aria
        ivert = pgrato(istaz)
        tadim = taria/(trif+t0)
        fc    = cpwt(1)+tadim*(cpwt(2)+tadim*
     *          (cpwt(3)+cpwt(4)*tadim))
c      fc = (700./taria(IVERT)-1)/(700./(15.+t0)-1)
        power = power/fc
c-sire2_0060-end

      endif

      return ! calc_out_cs
      end




cmar-p-30-07-09------------------------------------------------------------------------


      subroutine converti_out_cs(ivert)
c*****************************************************************************************
c   Questa subroutine viene chiamata prima della scrittura dei risultati della simulazione
c   di centrale per convertire l'UDM di alcune grandezze che nel calcolo sono usate con
c   una UDM diversa da quella interna(del DB).
c   Nello specifico i problemi riguardano:
c      portata di macchina:l'UDM interna è [m3/h] ma i calcoli sono eseguiti in [km3/h]
c      prevalenza:l'UDM interna è [m] ma i calcoli sono eseguiti in [km]
c*****************************************************************************************

      implicit none

	include '../inc/param.inc'
      include '../inc/conv.inc'
  	include '../inc/TOP_APP_ELE.INC'

	integer*4 ivert   ! indice centrale nella struttura dati ridotta

      real*4    udm_int ! function
c-----------------------------------------------------------------------------------------
c------>altezza adiabatica:da [km] a [m]
      head_cs(ivert) = udm_int(udm_H,head_cs(ivert))

c------>portata di macchina totale:da [km3/h] a [m3/h]
	flowm_cs(ivert) = udm_int(udm_QM,flowm_cs(ivert))

	return
	end
cgpe-corr-staz
      SUBROUTINE scrittura_ris_rete_ko (*)
*&*
      implicit none
      include '../inc/param.inc'	! :costanti
      include '../inc/default.inc'	! :costanti
      include '../inc/stazione.inc'	! :costanti
      include '../inc/scenario.inc'	! :costanti
      include '../inc/rk_param.inc'  ! 
      include '../inc/ti.inc'	! :in
      include '../inc/tx.inc'	! :in
      include '../inc/tj.inc'	! :out
      include '../inc/tv.inc'	! :in
      include '../inc/td.inc'	! :in
      include '../inc/tc.inc'	! :in
      include '../inc/STATIONS.INC'
      include '../inc/cap_bon_prv.INC'
      include '../inc/top_app_ele.INC'
      include '../inc/th.inc'	! :in
cT
      include '../inc/tz.inc'	! :in
cgpe-prv
      include '../inc/ty.inc'	! :in
cgpe-prv-end
C      include '../inc/ai.inc'	! :in
*&*
      integer*4 i,j,im,iv
      integer*4 i_file
      integer*4 IOS
      logical*2 elem_bin,elem_uni,elem_spe,elem_cen
      external  elem_bin,elem_uni,elem_spe,elem_cen
      character*(data_len) data
      integer*4 i_app1,i_app2
      character*25 tipo_elem
      character*1 simb
cgpe
      character*25 nome_ce
cgpe-end
      real*4 delta_p
      real*4 compos
      integer*4 gas
      logical*2 ce_serie
!-----------------------------------------------------------------------
      character*3 sta_sym(m_sta)
	data STA_SYM (STA_OFF) /'OFF'/ 	
	data STA_SYM (STA_NR ) /' NR' /	
	data STA_SYM (STA_QM)  /' QM' /	
	data STA_SYM(STA_QC )  /' QC' /	
	data STA_SYM(STA_Q  )  /'  Q'/
	data STA_SYM(STA_PM )  /' PM'/
	data STA_SYM(STA_PV )  /' PV'/
	data STA_SYM(STA_PN )  /' PN'/
	data STA_SYM(STA_PW )  /' PW'/
	data STA_SYM(STA_ON )  /' ON'/
	data STA_SYM(STA_FR )  /' FR'/
	data STA_SYM(STA_IL )  /' IL'/
	data STA_SYM(STA_ROM ) /'ROM'/
	data STA_SYM(STA_P )   /'  P'/
	data STA_SYM(STA_PMQ ) /'PMQ'/
	data STA_SYM(STA_PVQ)  /'PVQ'/
	data STA_SYM(STA_PQ )  /' PQ'/
	data STA_SYM(STA_PU )  /' PU'/	

      character*3 tci_sym(m_rego)
	data TCI_SYM (TC_PV) /' PV'/ 	
	data TCI_SYM (TC_PM) /' PM'/ 	
	data TCI_SYM (TC_Q) /'  Q'/ 	
	data TCI_SYM (TC_T) /'  T'/ 	
	data TCI_SYM (TC_ON) /' ON'/ 	
	data TCI_SYM (TC_OFF) /'OFF'/ 	
	data TCI_SYM (TC_NR) /' NR'/ 	
	data TCI_SYM (TC_PQ) /' PQ'/ 	
	data TCI_SYM (TC_NPV) /'NPV'/ 	
	data TCI_SYM (TC_NPM) /'NPM'/ 	
c-elena
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


      i_file = u_tem

      call open_vfile (i_file,c_path_dirlog,'_'//'ris_rete.txt',
     -    'replace',ios,*1010)


c------ Inizializzazione testata del file:
         write (UNIT=i_file,FMT=05,ERR=1011)
05    FORMAT (1x,'Risultati Stazionario KO')

c- data di elaborazione
cport      call legge_data(data,i_app1,i_app2,0,*9999)
      call legge_data(data)

      write (UNIT=i_file,FMT=11,ERR=1011) data
11    FORMAT (1x,'Ora di elaborazione: ',1x,a20)

cgpe-corr      write (UNIT=i_file,FMT=12,ERR=1011) ultimo_camp,corr_ist
cgpe-corr12    FORMAT (1x,'Istante di campionamento: ',1x,f6.3,
cgpe-corr     -        3x,'Numero campionamento: ',1x,i3)
      write (UNIT=i_file,FMT=13,ERR=1011) ultimo_camp,corr_ist
13    FORMAT (1x,'Istante di campionamento: ',1x,i3,
     -        3x,'Numero campionamento: ',1x,f6.3)

      write (UNIT=i_file,FMT=113,ERR=1011)

c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c elementi speciali

      write (UNIT=i_file,FMT=110,ERR=1011)
110   FORMAT (1x,'Codice',1x,'Descrizione',15x,'TCI',1X,'STA',3X,
     *        'PortDef',4x,'Portata',4x,
     *        'Pmonte',4x,'Pvalle',4x,'Consumo',4x,'Composizione')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=111,ERR=1011)
111   FORMAT (44x,'(KSm3h)'4x,'(KSm3h)'3x,'(Bar.ass)',
     *        1x,'(Bar.ass)',2x,'(KSm3h)',9x,'(%)')
112   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2,
     *        10x,f10.2)
122   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,10x,f10.2,
     *        13x,f10.2)
123   FORMAT 
     *     (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,10x,f10.2,15x,f10.2)
!-format per i risultati delle centrali
132   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2,f10.2,
     *        5x,f10.2)
cgpe-new
133   FORMAT (1x,I6,1x,a25,1x,a3,1x,a3,1x,f10.2,f10.2,f10.2,f10.2,6x,a4,
     *        5x,f10.2)
cgpe-new-end
!-format per i risultati delle centrali-end
113   FORMAT (1x)

      do j = 1, ne_spe


       write (UNIT=i_file,FMT=113,ERR=1011)

c- tipo di elemento trattato
c /E_VL,E_CE,E_IM,E_PZ,E_ST,E_RG,E_PR,E_VS,E_VR,E_VD,E_VG,E_CS/
       if (j.eq.1) then
         tipo_elem = 'VALVOLA'
       elseif (j.eq.2) then
         tipo_elem = 'CENTRALE'
       elseif (j.eq.3) then
         tipo_elem = 'IMPORTAZIONE'
       elseif (j.eq.4) then
         tipo_elem = 'POZZO'
       elseif (j.eq.5) then
         tipo_elem = 'STOCCAGGIO'
       elseif (j.eq.6) then
         tipo_elem = 'REGOLATORE ESTERNO'
c-elena
       elseif (j.eq.7) then
         tipo_elem = 'PRELIEVO SPECIALE'
       elseif (j.eq.8) then
         tipo_elem = 'VALVOLA DI SEZIONAMENTO'
       elseif (j.eq.9) then
         tipo_elem = 'VALVOLA DI RIDUZIONE'
       elseif (j.eq.10) then
         tipo_elem = 'IMPIANTO DI RIDUZIONE'
       elseif (j.eq.11) then
         tipo_elem = 'IMPIANTO DI REGOLAZIONE'
       elseif (j.eq.12) then
         tipo_elem = 'CENTRALE SEMPLIFICATA'
	 end if
c-elena
cgpe       write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
       if (etipti(te_spe(j)) .ge. otipti(te_spe(j))) then
cgpe-corr       write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
       write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem
       endif

       do i = otipti(te_spe(j)), etipti(te_spe(j))

        if ((i.gt.otipti(e_vl)).and.(htipto(i).ne.htipto(i-1))) then
          write (UNIT=i_file,FMT=113,ERR=1011)
        end if

        if (elem_bin(i) .and. elem_spe(i)) then
          im = opumto(i)
          iv = opuvto(i)
          compos = 0.
          do gas = 1, m_comp
            compos = compos + comptc(i,gas)
          enddo

          if (elem_cen(i)) then
c          if (htipto(i) .eq. e_ce) then
           if (ce_serie(i)) then
             write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
c     *                                     prestj(im)-1.013,
c     *                                     prestj(iv)-1.013,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     (tot_cons(pgrato(i))+
     *                                      tot_cons(pgrato_app(i)))*
     *                                      CONV_NS,
     *                                      compos*100.

           else
            if (htipto(i) .eq. e_ce) then
             if (assetto_cm(pgrato(i)) .eq. 0) then
               nome_ce = snomtv(i)
             else
               nome_ce = trim(sigla_ass(pgrato(i)))
             endif
cgpe-new
             if (fl_fatal(pgrato(i)) .ge. 0) then
cgpe-new-end
cgpe              write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
              write (UNIT=i_file,FMT=132,ERR=1011) i,
     *                               nome_ce,
cgpe-end
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     tot_cons(pgrato(i))*CONV_NS,
     *                                     compos*100.

cgpe-new
             else
cgpe              write (UNIT=i_file,FMT=133,ERR=1011) i,snomtv(i),
              write (UNIT=i_file,FMT=133,ERR=1011) i,
     *                               nome_ce,
cgpe-end
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     '$$$$',
     *                                     compos*100.
             endif
cgpe-new-end
cgpe-prv
            elseif (htipto(i) .eq. e_cs) then
             write (UNIT=i_file,FMT=132,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     consty(i)*CONV_NS,
     *                                     compos*100.
cgpe-prv-end
            endif
           endif
          else

           write (UNIT=i_file,FMT=112,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                     porttj(i)*CONV_NS,
     *                                     prestj(im),
     *                                     prestj(iv),
     *                                     compos*100.

          endif
        else if (elem_uni(i)) then
          compos = 0.
          do gas = 1, m_comp
            compos = compos + comptc(i,gas)
          enddo

            iv = opuvto(i)
            write (UNIT=i_file,FMT=122,ERR=1011) i,snomtv(i),
     *                               trim(TCI_SYM(htcitj(i))),
     *                               trim(STA_SYM(hstatj(i))),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                           porttj(i)*CONV_NS,
     *                                           prestj(iv),
     *                                     compos*100.
        end if
       end do
      end do


c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c punti

      write (UNIT=i_file,FMT=140,ERR=1011)
140   FORMAT (1x,'Codice',1x,'Descrizione',25x,'PortDef'8x,
     *        'Portata'4x,'Pressione',4x,'Temperatura',
     *         4x,'Composizione')
c_delta     *        'Pressione',7x,'Temperatura',7x,'Delta Lim.Pres.')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=141,ERR=1011)
141   FORMAT (44x,'(KSm3h)',8x,'(KSm3h)',4x,'(Bar.ass)',
     *        4x,'    (C)    ',9x,'(%)')
!142   FORMAT (1x,I6,1x,a30,4x,f10.2,2x,f10.2,8x,f10.2)
142   FORMAT (1x,I6,1x,a30,4x,f10.2,4x,f10.2,2x,f10.2,1x,a1,
     *        2x,f10.2,5x,f10.2)
c_delta     *        6x,f10.2,6x,f10.2)

      write (UNIT=i_file,FMT=113,ERR=1011)

c- tipo di elemento trattato
      tipo_elem = 'PUNTO'
cgpe-corr      write (UNIT=i_file,FMT=12,ERR=1011) tipo_elem
      write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem

CRSA --  Delta Pressione
      do i = otipti(e_pu), etipti(e_pu)
        simb = ' '
        delta_p = 0.0
        if (prestj(i).lt.pmintx(i)-.1) then
          simb = '<'
          delta_p = pmintx(i)-prestj(i)
        elseif (prestj(i).gt.pmaxtx(i)+.1) then
          simb = '>'
          delta_p = prestj(i)-pmaxtx(i)
        endif

        compos = 0.
        do gas = 1, m_comp
          compos = compos + comptc(i,gas)
        enddo

        write (UNIT=i_file,FMT=142,ERR=1011) i,snomtv(i),
     *                                     porttj(i)*pclstd(i)/pcls_def
     *                                     *CONV_NS,
     *                                      porttj(i)*CONV_NS,
     *                                      prestj(i),
     *                                      simb,
     *                                      temptj(i)-T0,
     *                                      compos*100.

c_delta     *                                      delta_p
      end do

c------
      write (UNIT=i_file,FMT=113,ERR=1011)
c tronchi

      write (UNIT=i_file,FMT=100,ERR=1011)
100   FORMAT (1x,'Codice',1x,'Descrizione',25x,'Portata',4x,
     *        'Temperatura')
!-format per l'unita' di misura
      write (UNIT=i_file,FMT=101,ERR=1011)
101   FORMAT (44x,'(KSm3h)',4x,'    (C)    ')
102   FORMAT (1x,I6,1x,a30,4x,f10.2,2x,f10.2)

c- tipo di elemento trattato
      tipo_elem = 'TRONCO'
      write (UNIT=i_file,FMT=103,ERR=1011) tipo_elem
103    FORMAT (1x,'Tipo di elemento: ',1x,a20)

      do i = otipti(e_tr), etipti(e_tr)
        write (UNIT=i_file,FMT=102,ERR=1011) i,snomtv(i),
     *                                       porttj(i)*CONV_NS,
cT     *                                       temptj(i)-T0
cT     *    ((temptj(opumto(i))+temptj(opuvto(i)))/2.0)-T0
     *                                       touttz(i)-T0
      end do


      close(i_file)
1111  continue
      return ! scrittura_ris_rete_ko

9999  return1

1010  continue
        call gest_error(2,0,'SCRITTURA_RIS_RETE_KO',
     *   'Errore in apertura file',0)
      goto 1111

1011  continue
        call gest_error(2,0,'SCRITTURA_RIS_RETE_KO',
     *   'File output corrotto',0)
      close(i_file)
      goto 1111
      
      end

c-tbg-off
      subroutine ini_derivati_out_ce(istaz,ivert)
c-----------------------------------------------------------------------------
	implicit none

      include '../inc/param.inc'
      include '../inc/rk.inc'
	include '../inc/ti.inc'
      include '../inc/stazione.inc'
      include '../inc/stations.inc'
      include '../inc/units.inc'
      include '../inc/types.inc'
      include '../inc/mf_units.inc'
      include '../inc/mf_types.inc'
      include '../inc/argo_reale_ris.inc'

      integer*4 istaz,      !I) indice centrale nella struttura dati completa
     *          ivert       !I) indice centrale nella struttura dati ridotta

      integer*4 i,iu,k
c-----------------------------------------------------------------------------
c----->dati di centrale/assetto
      q_riciclo(ivert) = 0.
      flow_stat_in(ivert) = 0.
      power_tot(ivert) = 0.
      cons_su_qout(ivert) = 0.
      fuel_su_ptt(ivert) = 0.
      ptt_su_pft(ivert) = 0.
      power_perc_der(ivert) = 0.
      power_tot(ivert) = 0.

      nom_max_pow(ivert) = 0.
      nom_max_pow_der(ivert) = 0.
      nom_max_pow_vinc(ivert) = 0.
      nom_min_pow(ivert) = 0.

      num_tbg_attivi(ivert) = 0

c----->dati di turbogruppo
      iu = first_unit(ivert)
      do i = 1,unit_num(ivert)
        k = iu+i-1
        unit_hrate_perc(k) = 0.
        unit_eff_turb(k) = 0.
        unit_rev_rpm(k) = 0.
        unit_qout(k) = 0.
        unit_qpeso(k) = 0.
        unit_ptt(k) = 0.
        unit_cons_su_qout(k) = 0.
        unit_power_iso(k) = 0.

        power_ric(k) = 0.
        power_ric_perc(k) = 0.
        unit_power_iso(k) = 0.
        power(k) = 0.
        power_perc(k) = 0.
        nom_pow_der(k) = 0.
        power_perc_var(k) = 0.
        power_perc_rev(k) = 0.
        unit_hrate_onsite(k) = 0.
        power_onsite(k) = 0.

        if (unit_bifase(k)) then
          unit2_rev_rpm(k) = 0.
          unit2_ptt(k) = 0.
          unit2_qout(k) = 0.
          unit2_qpeso(k) = 0.
          unit2_cons_su_qout(k) = 0.
          power2_ric(k) = 0.
          power2_ric_perc(k) = 0.
          unit2_power_iso(k) = 0.

        end if
	end do

	return
	end
c-tbg-off-end