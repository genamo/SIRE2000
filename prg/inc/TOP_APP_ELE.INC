*-------------------------------------------------------------------------------
*                         TOP_APP_ELE.INC
*
*        Definizione grandezze topologia AGGIUNTIVE per gli elementi
*-------------------------------------------------------------------------------
!===============================================================================
!                    PUNTO:  e_pu
!===============================================================================
      logical*2 fl_nocap       !punto non capacitivo
      integer*4 pu_pimp        !punto con pressione imposta
	                           ! puntatore all'elemnto che impone pressione
							   !         > 0 come pressione punto monte
							   !         < 0 come pressione punto valle
      integer*4 pu_regp        !punto regolato in pressione

      common/top_app_pu/
     -     fl_nocap(max_pu)
     -    ,pu_pimp(max_pu)
     -    ,pu_regp(max_pu)
!===============================================================================
!                    CENTRALE:  e_ce
!===============================================================================
      integer*4 assetto_cm    !assetto cm
	  integer*4 ass_comp_id   ! campo ass_comp_id della tabella ASSSETTI
	  integer*4 ass_coll_asp  
	  integer*4 ass_coll_man
      real*4    altit         !altezza della centrale
cmar_ass_cond
      integer*4 ASSCIN_ASS_COMP_COND_ID ! campo ASSCIN_ASS_COMP_COND_ID di ASSETTI
 
cmar_ass_cond      

      common/top_app_ce/
     -     assetto_cm  (max_vert)
     -    ,ass_comp_id (max_vert)
     -    ,ass_coll_asp(max_vert)
     -    ,ass_coll_man(max_vert)
     -    ,altit       (max_vert)
     -    ,ASSCIN_ASS_COMP_COND_ID(1000)
!===============================================================================
!                   CENTRALE SEMPLIFICATA:e_cs
!===============================================================================
      real*4 powmax     !potenza massima stimata (kw)
     -      ,romax      !rapporto di compressione massimo
     -      ,dpin       !perdita carico monte (bar)
     -      ,dpout      !perdita carico valle (bar)
     -      ,dpman      !delta pressione mandata (bar)
     -      ,dpasp      !delta pressione aspirazione (bar)
     -      ,dpin_s
     -      ,dpout_s
cmar_add   
     -      ,Pn_corr
     -      ,dp_asp_cs
     -      ,dp_man_cs
cmar_add     
cmar_Taria 
     -      ,Taria_s
cmar_Taria    
c
c
     
      common/top_app_cs/
     -      powmax(max_cs)
     -     ,romax(max_cs)
     -     ,dpin(max_cs)
     -     ,dpout(max_cs)
     -     ,dpman(max_cs)
     -     ,dpasp(max_cs)
     -     ,dpin_s(max_cs)
     -     ,dpout_s(max_cs)
cmar_add 
     -     ,Pn_corr(max_cs)  
     -     ,dp_asp_cs(max_cs)  
     -     ,dp_man_cs(max_cs)  
cmar_add       
cmar_Taria 
     -     ,Taria_s(max_cs)
cmar_Taria    
     
     
     
c-sire2_0060
      real*4 taria_cs   !temperatura dell'aria

      common/cin_app_cs/
     -      taria_cs(max_cs)
c-sire2_0060-end

      real*4 flowm_cs   !portata macchina calcolata per output
      real*4 head_cs    !prevalenza adiabatica calcolata per output
      real*4 power_cs   !potenza calcolata ISO per output
      common/dati_cs_out/
     -       flowm_cs(max_cs)
     -      ,head_cs(max_cs)
     -      ,power_cs(max_cs)
!===============================================================================
!                 IMPIANTI DI RIDUZIONE:e_vd
!===============================================================================
      real*4 qprog       !portata di progetto dell'impianto
     -      ,dpmin       !delta pressione minimo (bar assoluti)
      common/top_app_vd/
     -      qprog(max_vd)
     -     ,dpmin(max_vd)
!===============================================================================
!                 IMPIANTI DI REGOLAZIONE:e_vg
!===============================================================================
      integer*4 line_vg   !numero linee impianto di regolazione
      real*4 cvmnt        !coeff. efflusso valvola monitor
     -      ,cvreg        !coeff. efflusso valvola regolante
     -      ,xtreg        !pressione differenziale relativa
     -      ,vlim_out     !velocità limite uscita gas
     -      ,diam_in      !diametro di monte di linea
!  campo non obbligatorio;se non assegnato si pone diam_in=diam_out
     -      ,diam_out     !diametro di valle di linea
     -      ,dnom_vg      !diametro nominale valvola
      common/top_app_vg/
     -      line_vg(max_vg)
     -     ,cvmnt(max_vg)
     -     ,cvreg(max_vg)
     -     ,xtreg(max_vg)
     -     ,vlim_out(max_vg)
     -     ,diam_in(max_vg)
     -     ,diam_out(max_vg)
     -     ,dnom_vg(max_vg)
*-------------------------------------------------------------------------------
