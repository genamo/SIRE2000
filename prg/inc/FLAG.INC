
      real*4    cons_imp,cons_serv_imp    ! cons_imp = consty, se fl_cor = .T.
	                                      ! cons_imp = 0, se fl_cor = .F.
c-mf03
      real*4    soglia_cons                ! valore di soglia per il consumo da aggiungere
                                          ! per soluzioni entro la mappa topologica e al
										  ! di fuori dei vincoli operativi
c-mf03-end
c-intervento_temp
      integer*4 soglia_vio_temp
c-intervento_temp-end
      integer*4 fl_ris_gen  ! = 0  => risultati trovati
	                        !  controllo su 
                            !   err_ok   nessun errore      produce file .ok 
                            !   err_ii   errore information produce file .ok 
                            !   err_ww   errore warning     produce file .na 
                            !   err_ff   errore fatal       produce file .ok1
                            !   err_ko   errore sistema     produce file .ok0 
							!   err_aa   errore sistema  

      logical*2 fl_opt
     *         ,fl_sim  ! = .T. => simcent / = .F. => rete
cgpe-prv
     *         ,fl_din  ! = .T. => dinamica / = .F. => stazionario
cgpe-prv-end
     *         ,fl_cons ! = .T. => l'ottimo sceglie la soluzione per cui tot_cons(iopt)= consty
	                    ! = .F. => l'ottimo sceglie la configurazione che minimizza il consumo
c-mf03
     *         ,fl_vinc ! = .T. => l'ottimo sceglie la soluzione rispettando i vincoli operativi
	                    ! = .F. => l'ottimo sceglie la soluzione rilasciando i vincoli operativi
c-mf03-end
c-bp08
     *         ,fl_punto_ko   ! = .T. => e' stato calcolato il punto ko
c-bp08-end

cgpe-riciclo
     *         ,fl_ric_rev   ! = .T. => e' accettato il riciclo sul min_rev
cgpe-riciclo-end
     *         ,fl_recupero  ! = .F. => non e' richiesto il recupero (stazionario Hardy-Cross)
	                         ! = .T. => e' richiesto il recupero (stazionario Newton)
cstaz-corr
     *         ,try_rec
     *         ,try_ass_par
cstaz-corr-end
     *         ,fl_pcls_def  ! = .T. => usa pcls di default e non quello reale
      common/FLAG/ cons_imp 
     *            ,cons_serv_imp(max_serv)
c-mf03
     *            ,soglia_cons
c-mf03-end
c-intervento_temp
     *            ,soglia_vio_temp
c-intervento_temp-end
     *            ,fl_ris_gen
     *            ,fl_opt
     *            ,fl_sim
cgpe-prv
     *            ,fl_din
cgpe-prv-end
     *            ,fl_cons
c-mf03
     *            ,fl_vinc
c-mf03-end
c-bp08
     *            ,fl_punto_ko
c-bp08-end
cgpe-riciclo
     *            ,fl_ric_rev
cgpe-riciclo-end
     *            ,fl_recupero
cstaz-corr
     *            ,try_rec
     *            ,try_ass_par
cstaz-corr-end
     *            ,fl_pcls_def 

      integer*4 num_test,num_test_tot
      integer*4 iunit_corr
      logical*2 l_corr
      logical*2 fl_corr_mf
cgpe      logical*2 fl_corr_uni,flag_corr
      logical*2 fl_cor ! = .T. se il punto di lavoro della centrale deve essere corretto

      common/FLAG_CORR/ num_test,num_test_tot
     *                 ,iunit_corr(maxunits)
     *                 ,l_corr
     *                 ,fl_corr_mf
cgpe     *                 ,fl_corr_uni
cgpe     *                 ,flag_corr
     *                 ,fl_cor
