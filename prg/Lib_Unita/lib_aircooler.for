*****************************************************************************
c    Libreria contenente le subroutine per il calcolo della caduta di pressione e
c    della temperatura associata all'aircooler quando viene inserito a livello di 
c    centrale o di turbogruppo
*****************************************************************************

      subroutine delpr_ac_p(type_num,type_quant,status,unit_bifase,
     *           nom_flow,
     *           ac_ce_k,ac1_k,ac2_k,q,delpr_ac)

c----------------------------------------------------------------------------
c    Questa subroutine calcola la caduta di pressione associata all'air cooler 
c    in mandata per centrali semplici,composte in parallelo e miste.
c    I due stati che influenzano il calcolo sono AC inserito e attivo oppure inserito 
c    e non attivo.
c    Se l'air cooler è stato inserito a livello di centrale applico direttamente la 
c    formula quadratica delpr_ac=f_ac*q*q;se gli AC sono stati introdotti a livello 
c    di turbogruppo ricavo un unico ac_k come media e poi applico la formula,
c    riportandomi così a livello di centrale.
c    Se l'AC non è stato inserito in nessuno dei due casi viene restituito un delta
c    nullo.
c    Il delta calcolato va sommato algebricamente a quello prodotto dal piping.
c    Tale subroutine sarà utilizzata nei moduli di simulazione A,B,C,H.
c-----------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
cc
	include '../inc/ass_cond.inc'    
cc
c
c----------->INPUT
      integer*4 type_num,      ! numero di tipi di TBG
     *          type_quant(*)  ! numero di TBG per ogni tipo
      logical*2 status(*)      ! stato del TBG
      logical*2 unit_bifase(*) ! flag TBG bifase
      real*4    nom_flow(*)    ! portata nominale dei TBG
      real*4    q              ! portata lavorata dalla stazione o dal TG
	real*4    ac_ce_k,ac1_k(*),ac2_k(*)
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
c
c----------->OUTPUT
      real*4 delpr_ac          !caduta di pressione associata agli AC
c 
c----------->variabili locali
      real*4    kac,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,ac_nu,nu
	real*4 delpr_ce,delpr_tg
c---------------------------------------------------------------------------------
      delpr_ac = 0.
	delpr_ce = 0.
	delpr_tg = 0.
c------->contributo dell'air cooler di centrale
cc
cmar_ass_cond     if(.not.flag_ass_cond_s)then
      qq = q * q
      delpr_ce = ac_ce_k * qq
car_ass_cond      endif
cc
c------->contributo degli air cooler di turbogruppo
      nu = 0
      sum_flow = 0.
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu+ij
          if (status(icj))then
            sum_flow = sum_flow + nom_flow(icj)
          end if
        end do
        nu = nu+type_quant(i)
      end do

      ac_nu = 0
      nu = 0
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu + ij
	    delp(icj) = 0.
          if (status(icj)) then
	        q_un= q*(nom_flow(icj)/sum_flow)
              if (unit_bifase(icj)) then
                  ac_nu = ac_nu + 1
                  kac = (ac1_k(icj)+ ac2_k(icj))/2
	        else
                  ac_nu = ac_nu + 1
                  kac = ac1_k(icj)
              end if
	        delp(icj)=kac*(q_un*q_un)
	        delpr_tg=delpr_tg+delp(icj)+delta_in_pressione(i)
          end if
        end do
        nu = nu + type_quant(i)
      end do
      if (ac_nu .gt. 0) then
        delpr_tg = delpr_tg/ac_nu
      end if

c------------------>>>>caduta totale di pressione
      delpr_ac=delpr_ce+delpr_tg
  
      return
	end      !subroutine delpr_ac_p



	
      subroutine delpr_ac_p1(type_num,type_quant,status,unit_bifase,
     *           nom_flow,
     *           ac_ce_k,ac1_k,ac2_k,q,delpr_ac)

c----------------------------------------------------------------------------
c    Questa subroutine calcola la caduta di pressione associata all'air cooler 
c    in mandata per centrali semplici,composte in parallelo e miste.
c    I due stati che influenzano il calcolo sono AC inserito e attivo oppure inserito 
c    e non attivo.
c    Se l'air cooler è stato inserito a livello di centrale applico direttamente la 
c    formula quadratica delpr_ac=f_ac*q*q;se gli AC sono stati introdotti a livello 
c    di turbogruppo ricavo un unico ac_k come media e poi applico la formula,
c    riportandomi così a livello di centrale.
c    Se l'AC non è stato inserito in nessuno dei due casi viene restituito un delta
c    nullo.
c    Il delta calcolato va sommato algebricamente a quello prodotto dal piping.
c    Tale subroutine sarà utilizzata nei moduli di simulazione A,B,C,H.
c-----------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
cc
	include '../inc/ass_cond.inc'    
cc
c
c----------->INPUT
      integer*4 type_num,      ! numero di tipi di TBG
     *          type_quant(*)  ! numero di TBG per ogni tipo
      logical*2 status(*)      ! stato del TBG
      logical*2 unit_bifase(*) ! flag TBG bifase
      real*4    nom_flow(*)    ! portata nominale dei TBG
      real*4    q              ! portata lavorata dalla stazione o dal TG
	real*4    ac_ce_k,ac1_k(*),ac2_k(*)
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
c
c----------->OUTPUT
      real*4 delpr_ac          !caduta di pressione associata agli AC
c 
c----------->variabili locali
      real*4    kac,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,ac_nu,nu
	real*4 delpr_ce,delpr_tg
c---------------------------------------------------------------------------------
      delpr_ac = 0.
	delpr_ce = 0.
	delpr_tg = 0.
c------->contributo dell'air cooler di centrale
cc
cmar_ass_cond     if(.not.flag_ass_cond_s)then
      qq = q * q
      delpr_ce = dp_ac_1 * qq
car_ass_cond      endif
cc
c------->contributo degli air cooler di turbogruppo
      nu = 0
      sum_flow = 0.
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu+ij
          if (status(icj))then
            sum_flow = sum_flow + nom_flow(icj)
          end if
        end do
        nu = nu+type_quant(i)
      end do

      ac_nu = 0
      nu = 0
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu + ij
	    delp(icj) = 0.
          if (status(icj)) then
	        q_un= q*(nom_flow(icj)/sum_flow)
              if (unit_bifase(icj)) then
                  ac_nu = ac_nu + 1
                  kac = (ac1_k(icj)+ ac2_k(icj))/2
	        else
                  ac_nu = ac_nu + 1
                  kac = ac1_k(icj)
              end if
	        delp(icj)=kac*(q_un*q_un)
	        delpr_tg=delpr_tg+delp(icj)+delta_in_pressione(i)
          end if
        end do
        nu = nu + type_quant(i)
      end do
      if (ac_nu .gt. 0) then
        delpr_tg = delpr_tg/ac_nu
      end if

c------------------>>>>caduta totale di pressione
      delpr_ac=delpr_ce+delpr_tg
  
      return
	end      !subroutine delpr_ac_p
c




      subroutine delpr_ac_p2(type_num1,type_num,type_quant,status,
     *           unit_bifase,nom_flow,
     *           ac_ce_k,ac1_k,ac2_k,q,delpr_ac)

c----------------------------------------------------------------------------
c    Questa subroutine calcola la caduta di pressione associata all'air cooler 
c    in mandata per centrali semplici,composte in parallelo e miste.
c    I due stati che influenzano il calcolo sono AC inserito e attivo oppure inserito 
c    e non attivo.
c    Se l'air cooler è stato inserito a livello di centrale applico direttamente la 
c    formula quadratica delpr_ac=f_ac*q*q;se gli AC sono stati introdotti a livello 
c    di turbogruppo ricavo un unico ac_k come media e poi applico la formula,
c    riportandomi così a livello di centrale.
c    Se l'AC non è stato inserito in nessuno dei due casi viene restituito un delta
c    nullo.
c    Il delta calcolato va sommato algebricamente a quello prodotto dal piping.
c    Tale subroutine sarà utilizzata nei moduli di simulazione A,B,C,H.
c-----------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
cc
	include '../inc/ass_cond.inc'    
cc
c
c----------->INPUT
      integer*4 type_num,      ! numero di tipi di TBG
     *          type_quant(*)  ! numero di TBG per ogni tipo
      logical*2 status(*)      ! stato del TBG
      logical*2 unit_bifase(*) ! flag TBG bifase
      real*4    nom_flow(*)    ! portata nominale dei TBG
      real*4    q              ! portata lavorata dalla stazione o dal TG
	real*4    ac_ce_k,ac1_k(*),ac2_k(*)
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
c
    

      integer*4  type_num1


c----------->OUTPUT
      real*4 delpr_ac          !caduta di pressione associata agli AC
c 
c----------->variabili locali
      real*4    kac,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,ac_nu,nu
	real*4 delpr_ce,delpr_tg
c---------------------------------------------------------------------------------
      delpr_ac = 0.
	delpr_ce = 0.
	delpr_tg = 0.
c------->contributo dell'air cooler di centrale
cc
cmar_ass_cond     if(.not.flag_ass_cond_s)then
      qq = q * q
      delpr_ce = dp_ac_2 * qq
car_ass_cond      endif
cc
c------->contributo degli air cooler di turbogruppo
      nu = 0
      sum_flow = 0.
      do i = type_num1+1,type_num1+type_num
        do ij = 1,type_quant(i)
c          icj = nu+ij
            icj = nu+ij+unit_1
          if (status(icj))then
            sum_flow = sum_flow + nom_flow(icj)
          end if
        end do
        nu = nu+type_quant(i)
      end do

      ac_nu = 0
      nu = 0
      do i = type_num1+1,type_num1+type_num
        do ij = 1,type_quant(i)
c          icj = nu + ij
            icj = nu+ij+unit_1
	    delp(icj) = 0.
          if (status(icj)) then
	        q_un= q*(nom_flow(icj)/sum_flow)
              if (unit_bifase(icj)) then
                  ac_nu = ac_nu + 1
                  kac = (ac1_k(icj)+ ac2_k(icj))/2
	        else
                  ac_nu = ac_nu + 1
                  kac = ac1_k(icj)
              end if
	        delp(icj)=kac*(q_un*q_un)
	        delpr_tg=delpr_tg+delp(icj)+delta_in_pressione(i)
          end if
        end do
        nu = nu + type_quant(i)
      end do
      if (ac_nu .gt. 0) then
        delpr_tg = delpr_tg/ac_nu
      end if

c------------------>>>>caduta totale di pressione
      delpr_ac=delpr_ce+delpr_tg
  
      return
	end      !subroutine delpr_ac_p
c
c
***********************************************************************************

      subroutine delpr_ac_p_tc(type_num,type_quant,status,unit_bifase,
     *           nom_flow,
     *           ac_ce_k,ac1_k,ac2_k,q,delpr_ac)

c----------------------------------------------------------------------------
c    Questa subroutine calcola la caduta di pressione associata all'air cooler 
c    in mandata per centrali semplici,composte in parallelo e miste.
c    I due stati che influenzano il calcolo sono AC inserito e attivo oppure inserito 
c    e non attivo.
c    Se l'air cooler è stato inserito a livello di centrale applico direttamente la 
c    formula quadratica delpr_ac=f_ac*q*q;se gli AC sono stati introdotti a livello 
c    di turbogruppo ricavo un unico ac_k come media e poi applico la formula,
c    riportandomi così a livello di centrale.
c    Se l'AC non è stato inserito in nessuno dei due casi viene restituito un delta
c    nullo.
c    Il delta calcolato va sommato algebricamente a quello prodotto dal piping.
c    Tale subroutine sarà utilizzata nei moduli di simulazione A,B,C,H.
c-----------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
c
c----------->INPUT
      integer*4 type_num,      ! numero di tipi di TBG
     *          type_quant(*)  ! numero di TBG per ogni tipo
      logical*2 status(*)      ! stato del TBG
      logical*2 unit_bifase(*) ! flag TBG bifase
      real*4    nom_flow(*)    ! portata nominale dei TBG
      real*4    q              ! portata lavorata dalla stazione o dal TG
	real*4    ac_ce_k,ac1_k(*),ac2_k(*)
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
c
c----------->OUTPUT
      real*4 delpr_ac          !caduta di pressione associata agli AC
c 
c----------->variabili locali
      real*4    kac,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,ac_nu,nu
	real*4 delpr_ce,delpr_tg
c---------------------------------------------------------------------------------
      delpr_ac = 0.
	delpr_ce = 0.
	delpr_tg = 0.
c------->contributo dell'air cooler di centrale
      qq = q * q
      delpr_ce = ac_ce_k * qq
c------->contributo degli air cooler di turbogruppo
      nu = 0
      sum_flow = 0.
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu+ij
          if (status(icj))then
            sum_flow = sum_flow + nom_flow(icj)
          end if
        end do
        nu = nu+type_quant(i)
      end do

      ac_nu = 0
      nu = 0
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu + ij
	    delp(icj) = 0.
          if (status(icj)) then
	        q_un= q*(nom_flow(icj)/sum_flow)
              if (unit_bifase(icj)) then
                  ac_nu = ac_nu + 1
                  kac = (ac1_k(icj)+ ac2_k(icj))/2
	        else
                  ac_nu = ac_nu + 1
                  kac = ac1_k(icj)
              end if
	        delp(icj)=kac*(q_un*q_un)
	        delpr_tg=delpr_tg+delp(icj)+delta_in_pressione(i)
          end if
        end do
        nu = nu + type_quant(i)
      end do
      if (ac_nu .gt. 0) then
        delpr_tg = delpr_tg/ac_nu
      end if

c------------------>>>>caduta totale di pressione
      delpr_ac=delpr_ce+delpr_tg
  
      return
	end      !subroutine delpr_ac_p
c
***********************************************************************************
      subroutine delpr_ac_s(type_num,type_quant,status,nom_flow,
     *           ac_ce_k,ac2_k,q,delpr_ac)
c----------------------------------------------------------------------------
c     Questa subroutine calcola la caduta di pressione associata all'air cooler 
c     in mandata per centrali composte in serie.Il calcolo coinvolge soltanto l'AC
c     relativo alla seconda fase.I due stati che influenzano il calcolo sono AC 
c     inserito e attivo oppure inserito e non attivo.
c     Se l'air cooler è stato inserito a livello di centrale applico direttamente la 
c     formula quadratica delpr_ac=f_ac*q*q;se gli AC sono stati introdotti a livello 
c     di turbogruppo ricavo un unico ac_k come media e poi applico la formula,
c     riportandomi così a livello di centrale.
c     Se l'AC non è stato inserito in nessuno dei due casi viene restituito un delta
c     nullo.
c     Il delta calcolato va sommato algebricamente a quello prodotto dal piping.
c     Tale subroutine sarà utilizzata nei moduli di simulazione D,E,F,G, per i quali
c     il calcolo della caduta di pressione associata all'AC di fase 1 va risolta a
c     livello di TG
c-----------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
c
c----------->INPUT
      integer*4 type_num,     ! numero di tipi di TBG
     *          type_quant(*) ! numero di TBG per ogni tipo
      logical*2 status(*)
      real*4    nom_flow(*)   ! portata nominale dei TBG

      real*4    q             ! portata lavorata dalla stazione o dal TG
	real*4    ac_ce_k,ac2_k(*)
      real*4 delta_in_pressione(100)
      COMMON /DELTAP/ delta_in_pressione
c
c----------->OUTPUT
      real*4    delpr_ac      !caduta di pressione associata all'air cooler
c 
c----------->variabili locali
      real*4    qq,delp(maxunits),q_un,sum_flow
      integer*4 ac_nu,nu,i,ij,icj
	real*4 delpr_ce,delpr_tg
c----------------------------------------------------------------------------
      delpr_ac = 0.
      delpr_ce = 0.
      delpr_tg = 0.
c-------->>contributo air cooler di centrale
      qq = q * q
      delpr_ce = ac_ce_k * qq
c-------->>contributo air cooler di tbg
      nu = 0
      sum_flow = 0.
      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu+ij
          if (status(icj))then
            sum_flow = sum_flow + nom_flow(icj)
          end if
        end do
        nu = nu+type_quant(i)
      end do


      ac_nu = 0
      nu = 0

      do i = 1,type_num
        do ij = 1,type_quant(i)
          icj = nu + ij
          if (status(icj)) then
             ac_nu = ac_nu + 1
	       q_un = q* (nom_flow(icj)/sum_flow)
	       delp(icj) = ac2_k(icj)* (q_un*q_un)
	       delpr_tg = delpr_tg + delp(icj)+delta_in_pressione(i)
		end if
        end do
        nu = nu + type_quant(i)
      end do

      if (ac_nu .gt.0 ) then
        delpr_tg = delpr_tg /ac_nu
      end if

c--------->caduta di pressione totale
      delpr_ac=delpr_ce+delpr_tg


      return
	end      !subroutine delpr_ac_s
c
c**********************************************************************************
c
      subroutine out_temper_ac_ce(taria,pin,flow,tmax_man,temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	
c
c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      integer*4 i
	real*4 deltat_ric,delta_ac
cgpe
	real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------


  
      deltat_ric = max(0.,temp - tmax_man)

      
	if (deltat_ric.gt.0) then
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
	      call zpt1(ind,pin,temp,zeta)
	      ro=pin/(rr*temp*zeta)
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
	      call calccp(ind,ro,temp_aux,ctpc,ctvc)
cgpe-end
        
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog*3600./4.186
	      t_aux=ac_taria_prog/taria
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)




c                              temperatura di uscita dalla centrale

            temp=temp-min(deltat_ric,delta_ac)


     
         end if
	end if
c



	return
	end
c
c**********************************************************************************


      subroutine out_temper_ac_ce_as1(taria,pin,flow,tmax_man,temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	

	include '../inc/ass_cond.inc'


c
c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      integer*4 i
	real*4 deltat_ric,delta_ac
cgpe
	real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
      deltat_ric = max(0.,temp - tmax_man)
      
	if (deltat_ric.gt.0) then
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
	      call zpt1(ind,pin,temp,zeta)
	      ro=pin/(rr*temp*zeta)
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
	      call calccp(ind,ro,temp_aux,ctpc,ctvc)
cgpe-end
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog_1*3600./4.186
	      t_aux=ac_taria_prog_1/taria
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)
c                              temperatura di uscita dalla centrale
            temp=temp-min(deltat_ric,delta_ac)
         end if
	end if
c
	return
	end
c
c**********************************************************************************
c

      subroutine out_temper_ac_ce_as2(taria,pin,flow,tmax_man,temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'
		
	include '../inc/ass_cond.inc'
c   



c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      integer*4 i
	real*4 deltat_ric,delta_ac
cgpe
	real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------

      deltat_ric = max(0.,temp - tmax_man)
      



	if (deltat_ric.gt.0) then
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
	      call zpt1(ind,pin,temp,zeta)
	      ro=pin/(rr*temp*zeta)
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
	      call calccp(ind,ro,temp_aux,ctpc,ctvc)



cgpe-end
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog_2*3600./4.186
	      t_aux=ac_taria_prog_2/taria
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)







c                              temperatura di uscita dalla centrale








            temp=temp-min(deltat_ric,delta_ac)




         end if
	end if
c
      

	return
	end
c
c**********************************************************************************


c
      subroutine single_out_temp_ac(taria,pin,itg,fase,
c-ele-aggiungo parametro di output
c 28/4/06     *                              flow,tmax_man,unit_temp)
     *                              flow,tmax_man,unit_temp,deltat)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è stato introdotto a livello di unità;se l'air cooler non è 
c    stato inserito la temperatura d'uscita è uguale a quella d'ingresso.
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	


c--------->Input
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso all'air cooler[bar]
                            !(=pressione di USCITA dal singolo TG)
c
      integer*4 itg    !indice del turbogruppo
c
     *      ,fase      !fase di inserimento dell'ac
c
c
      real*4 flow      !portata lavorata dal compressore [km3]/[h]
c
     *      ,tmax_man !temperatura massima di mandata centrale
     

c---------->Input/Output
c-28/4/06
      real*4 unit_temp !I)temperatura in ingresso all'aircooler 
	                 !(=temperatura di uscita al singolo TG )
	                 !O)temperatura in uscita all'air cooler
      real*4 deltat    !O)DELTAT prodotto dall'air cooler

c---------->variabili locali
      real*4 rr 
     *      ,zeta
     *      ,ro 
     *      ,ctpc
     *      ,ctvc
     *      ,t_aux
     *      ,aux
     *      ,tout
     *      ,q_prog_aux    !per trasformazione da [kW] a [kcal]/[h] 
     *      ,deltat_ric
     *      ,delta_ac
cgpe
      real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
      deltat=0.
	deltat_ric = max(0.,unit_temp - tmax_man)

	if (deltat_ric.gt.0) then
	  if (fase.eq.1) then
          if (ac1_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
	        call zpt1(ind,pin,unit_temp,zeta)
	        ro=pin/(rr*unit_temp*zeta)
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
	        call calccp(ind,ro,temp_aux,ctpc,ctvc)
cgpe-end
              aux=ro*flow*conv_qm*ctpc

	        q_prog_aux = ac1_q_prog(itg)*3600./4.186  
 	        t_aux=ac1_taria_prog(itg)/taria
	        delta_ac=q_prog_aux*t_aux/aux
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
              deltat=min(deltat_ric,delta_ac)
	     end if
         else   !fase 2
           if (ac2_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
	        call zpt1(ind,pin,unit_temp,zeta)
	        ro=pin/(rr*unit_temp*zeta)
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
	        call calccp(ind,ro,temp_aux,ctpc,ctvc)
cgpe-end
              aux=ro*flow*conv_qm*ctpc

	        q_prog_aux = ac2_q_prog(itg)*3600./4.186  
		    t_aux=ac2_taria_prog(itg)/taria
	        delta_ac=q_prog_aux*t_aux/aux
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
              deltat=min(deltat_ric,delta_ac)
           end if
	   end if
	end if

c
	return
	end
c
c**********************************************************************************

      subroutine out_temper_ac_ce_new(taria,pin,flow,tmax_man,p1,t1,
     *temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	
c
c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      integer*4 i
	real*4 deltat_ric,delta_ac
      
      real*4 p1     !nuovo parametro evolutiva 2024  
      
      real*4 t1,ro0   !nuovi parametri evolutiva 2024
cgpe
	real*4 temp_aux
cevo_2024
      real*4 deltat
cgpe-end
c---------------------------------------------------------------------------------


cTestScrittura intermedia
c      OPEN (unit = 120, file='C:\LOG_FORTRAN\AC_NEW.txt',
c     * status='old',access='append')
      write(LGU,*)'START'
      write(LGU,*)'---------------------------'
      
cEndTest  
      deltat_ric = max(0.,temp - tmax_man)

      
	if (deltat_ric.gt.0) then
          write(LGU,*)'CENTRALE'
          write(LGU,*)' '
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
           write(LGU,*)'rr:',rr
           write(LGU,*)'P T out compressore:',pin,temp            
	      call zpt1(ind,pin,temp,zeta)
            write(LGU,*)'zeta out compressore:',zeta
	      ro0=pin/(rr*temp*zeta)
              write(LGU,*)'ro0:',ro0
cEv2024
            write(LGU,*)'P T IN compressore:',p1,t1
            call zpt1(ind,p1,t1,zeta) 
             write(LGU,*)'zeta in compressore:',zeta
            ro=p1/(rr*t1*zeta)
            write(LGU,*)'ro:',ro
cEv2024 end            
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
	      write(LGU,*)'temp_aux in calccp:',temp_aux
            write(LGU,*)'ro0 in calccp:',ro0
            call calccp(ind,ro0,temp_aux,ctpc,ctvc)
            write(LGU,*)'Cp Cv:',ctpc,ctvc
            write(LGU,*)'Flow:',flow
cgpe-end
        
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog*3600./4.186
            write(LGU,*)'q_prog_aux:',q_prog_aux
	      t_aux=ac_taria_prog/taria
              write(LGU,*)'t_aux ac_taria_prog taria:',t_aux
     *                    ,ac_taria_prog,taria
              write(LGU,*)'conv_qm:',conv_qm
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)
              write(LGU,*)'aux:',ro*flow*conv_qm*ctpc
              write(LGU,*)'delta_ac:',delta_ac
              write(LGU,*)'deltat_ric:',deltat_ric            




c                              temperatura di uscita dalla centrale

cEv2024     temp=temp-min(deltat_ric,delta_ac)
              
              if (delta_ac.ge.deltat_ric) then
                  deltat = deltat_ric
              elseif (delta_ac.lt.deltat_ric) then
                  deltat = delta_ac
              end if
              write(LGU,*)'deltat:',deltat
              temp=temp-deltat
              write(LGU,*)'FINE'
              write(LGU,*)'///\\\'
              write(LGU,*)''     
         end if
	end if
c


c      close(120)
	return
      end
c
c

      
      
      subroutine out_temper_ac_ce_as1_new(taria,pin,flow,tmax_man,p1,t1,
     *                 temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	

	include '../inc/ass_cond.inc'


c
c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      real*4 p1     !nuovo parametro evolutiva 2024  
      
      real*4 t1,ro0   !nuovi parametri evolutiva 2024  
      
      integer*4 i
	real*4 deltat_ric,delta_ac
cevo_2024
      real*4 deltat
cgpe
	real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
cTestScrittura intermedia
c      OPEN (unit = 120, file='C:\LOG_FORTRAN\AC_NEW.txt',
c     *    status='old',access='append')
      write(LGU,*)'START'
      write(LGU,*)'---------------------------'
      write(LGU,*)'ASSETTO 1'
cEndTest      
      deltat_ric = max(0.,temp - tmax_man)
      
	if (deltat_ric.gt.0) then
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
              write(LGU,*)'rr:',rr
              write(LGU,*)'P T out compressore:',pin,temp
	      call zpt1(ind,pin,temp,zeta)
            write(LGU,*)'zeta out compressore:',zeta
	      ro0=pin/(rr*temp*zeta)
             write(LGU,*)'ro0:',ro0
cEv2024
             write(LGU,*)'P T IN compressore:',p1,t1
             call zpt1(ind,p1,t1,zeta) 
             write(LGU,*)'zeta in compressore:',zeta
             ro=p1/(rr*t1*zeta) 
             write(LGU,*)'ro:',ro
cEv2024 end            
            
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
	      write(LGU,*)'temp_aux in calccp:',temp_aux
            write(LGU,*)'ro0 in calccp:',ro0
            call calccp(ind,ro0,temp_aux,ctpc,ctvc)
            write(LGU,*)'Cp Cv:',ctpc,ctvc
            write(LGU,*)'Flow:',flow
cgpe-end
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog_1*3600./4.186
            write(LGU,*)'q_prog_aux:',q_prog_aux
	      t_aux=ac_taria_prog_1/taria
              write(LGU,*)'t_aux ac_taria_prog_1 taria:',t_aux
     *                    ,ac_taria_prog_1,taria
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)
             write(LGU,*)'aux:',ro*flow*conv_qm*ctpc
              write(LGU,*)'delta_ac:',delta_ac
              write(LGU,*)'deltat_ric:',deltat_ric            

c                              temperatura di uscita dalla centrale
cEvo2024            temp=temp-min(deltat_ric,delta_ac)
            if (delta_ac.ge.deltat_ric) then
                deltat = deltat_ric
            elseif (delta_ac.lt.deltat_ric) then
                deltat = delta_ac
            end if
             write(LGU,*)'deltat:',deltat
            temp=temp-deltat
              write(LGU,*)'FINE'
              write(LGU,*)'///\\\'
              write(LGU,*)''
         end if
	end if
c
c      close(120)
	return
      end
c
c
c**********************************************************************************

      subroutine out_temper_ac_ce_as2_new(taria,pin,flow,tmax_man,p1,t1,
     *     temp)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è attivo a livello di centrale
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'
		
	include '../inc/ass_cond.inc'
c   



c--------------->Input
c
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso al cooler [bar]
	                      !(pressione di USCITA DALLA CENTRALE)
     *      ,flow           !portata lavorata in [km3/h]
     *      ,tmax_man       !temperatura massima di mandata centrale
c
c--------------->Input/Output
c
      real*4 temp       !I)temperatura in ingresso al cooler
                        !(temperatura di USCITA DALLA CENTRALE)
					  !O)temperatura in uscita all'aircooler 
c
c--------------->Variabili locali
c	                
      real*4 rr 
     *      ,zeta
     *      ,ro     ! [kg]/[m3]
     *      ,ctpc   ! [kcal]/[kg]/[K]
     *      ,ctvc
     *      ,t_aux
     *      ,q_prog_aux   ! [kcal]/[h]
c
      real*4 p1     !nuovo parametro evolutiva 2024  
      
      real*4 t1,ro0   !nuovi parametri evolutiva 2024  
      integer*4 i
	real*4 deltat_ric,delta_ac
cevo_2024
      real*4 deltat
cgpe
	real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
cTestScrittura intermedia
c      OPEN (unit = 120, file='C:\LOG_FORTRAN\AC_NEW.txt',
c     *    status='old',access='append')
      write(LGU,*)'START'
      write(LGU,*)'---------------------------'
      
cEndTest
      deltat_ric = max(0.,temp - tmax_man)
      



	if (deltat_ric.gt.0) then
         write(LGU,*)'ASSETTO 2'
          write(LGU,*)' '
         if (ac_ce_stat.eq.ac_on) then
c 
c                            proprietà del gas lavorato dalla centrale   
            rr=r_mks*romks/pmks/av_weight(ind)
           write(LGU,*)'rr:',rr
           write(LGU,*)'P T out compressore:',pin,temp            
	      call zpt1(ind,pin,temp,zeta)
            write(LGU,*)'zeta out compressore:',zeta
	      ro0=pin/(rr*temp*zeta)
cEv2024
            write(LGU,*)'ro0:',ro0
            write(LGU,*)'P T IN compressore:',p1,t1
             call zpt1(ind,p1,t1,zeta) 
             write(LGU,*)'zeta in compressore:',zeta
             ro=p1/(rr*t1*zeta) 
             write(LGU,*)'ro:',ro
cEv2024 end   
cgpe	      call calccp(ind,ro,temp,ctpc,ctvc)
            temp_aux = (temp + tmax_man) / 2.
            write(LGU,*)'temp_aux in calccp:',temp_aux
            write(LGU,*)'ro0 in calccp:',ro0
            call calccp(ind,ro0,temp_aux,ctpc,ctvc)
            write(LGU,*)'Cp Cv:',ctpc,ctvc
            write(LGU,*)'Flow:',flow

cgpe-end
c                             deltat prodotto dall'air cooler
		  q_prog_aux = ac_q_prog_2*3600./4.186
            write(LGU,*)'q_prog_aux:',q_prog_aux
	      t_aux=ac_taria_prog_2/taria
             write(LGU,*)'t_aux ac_taria_prog_2 taria:',t_aux
     *                    ,ac_taria_prog_2,taria
            delta_ac=q_prog_aux*t_aux/(ro*flow*conv_qm*ctpc)
              write(LGU,*)'aux:',ro*flow*conv_qm*ctpc
              write(LGU,*)'delta_ac:',delta_ac
              write(LGU,*)'deltat_ric:',deltat_ric            

cEvo2024            temp=temp-min(deltat_ric,delta_ac)
         if (delta_ac.ge.deltat_ric) then
            deltat = deltat_ric
         elseif (delta_ac.lt.deltat_ric) then
            deltat = delta_ac
         end if
         write(LGU,*)'deltat:',deltat
         temp=temp-deltat
             write(LGU,*)'FINE'
              write(LGU,*)'///\\\'
              write(LGU,*)''     
       end if
	end if
c
      
c      close(120)
	return
      end
c
c**********************************************************************************
c*********************************************************************************
c
     
      subroutine single_out_temp_ac_new(taria,pin,itg,fase,
c-ele-aggiungo parametro di output
c 28/4/06     *                              flow,tmax_man,unit_temp)
     *                        flow,tmax_man,unit_temp,p1,t1,deltat)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è stato introdotto a livello di unità;se l'air cooler non è 
c    stato inserito la temperatura d'uscita è uguale a quella d'ingresso.
c**********************************************************************************
      
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	

      REAL*4 pres_for_fase2
      REAL*4 temp_for_fase2
      REAL*4 head_for_fase2
      REAL*4 hassZg1
      REAL*4 qassZg1
      REAL*4 press_out_calc(100)
      REAL*4 press_mand_calc(100)

      COMMON /staz/ pres_for_fase2, temp_for_fase2, head_for_fase2,
     *               hassZg1, qassZg1,
     *               press_out_calc, press_mand_calc
      

c--------->Input
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso all'air cooler[bar]
                            !(=pressione di USCITA dal singolo TG)
c
      integer*4 itg    !indice del turbogruppo
c
     *      ,fase      !fase di inserimento dell'ac
c
c
      real*4 flow      !portata lavorata dal compressore [km3]/[h]
      
      real*4 p1     !nuovo parametro evolutiva 2024  
      
      real*4 t1,ro0   !nuovi parametri evolutiva 2024  
c
     *      ,tmax_man !temperatura massima di mandata centrale
     

c---------->Input/Output
c-28/4/06
      real*4 unit_temp !I)temperatura in ingresso all'aircooler 
	                 !(=temperatura di uscita al singolo TG )
	                 !O)temperatura in uscita all'air cooler
      real*4 deltat    !O)DELTAT prodotto dall'air cooler

c---------->variabili locali
      real*4 rr 
     *      ,zeta
     *      ,ro 
     *      ,ctpc
     *      ,ctvc
     *      ,t_aux
     *      ,aux
     *      ,tout
     *      ,q_prog_aux    !per trasformazione da [kW] a [kcal]/[h] 
     *      ,deltat_ric
     *      ,delta_ac
cgpe
      real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
cTestScrittura intermedia
c      OPEN (unit = 120, file='C:\LOG_FORTRAN\AC_NEW.txt',
c     *    status='old',access='append')
      write(LGU,*)'START'
      write(LGU,*)'---------------------------'
      
cEndTest
      deltat=0.
	deltat_ric = max(0.,unit_temp - tmax_man)

	if (deltat_ric.gt.0) then
	  if (fase.eq.1) then
          write(LGU,*)'FASE 1'
          write(LGU,*)'INDICE TURBOGRUPPO:',itg
          if (ac1_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
              write(LGU,*)'rr:',rr
              write(LGU,*)'P T out compressore:',pin,unit_temp
              call zpt1(ind,pin,unit_temp,zeta)
              write(LGU,*)'zeta out compressore:',zeta
              ro0=pin/(rr*unit_temp*zeta)
              write(LGU,*)'ro0:',ro0
cEv2024
              write(LGU,*)'P T IN compressore:',p1,t1
             call zpt1(ind,p1,t1,zeta)
             write(LGU,*)'zeta in compressore:',zeta
             ro=p1/(rr*t1*zeta) 
              write(LGU,*)'ro:',ro
cEv2024 end              
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
	        write(LGU,*)'temp_aux in calccp:',temp_aux
              write(LGU,*)'ro0 in calccp:',ro0
              call calccp(ind,ro0,temp_aux,ctpc,ctvc)
              
              write(LGU,*)'Cp Cv:',ctpc,ctvc 
cgpe-end
              write(LGU,*)'Flow:',flow
              aux=ro*flow*conv_qm*ctpc
              write(LGU,*)'aux:',aux
	        q_prog_aux = ac1_q_prog(itg)*3600./4.186 
              write(LGU,*)'q_prog_aux:',q_prog_aux
 	        t_aux=ac1_taria_prog(itg)/taria
              write(LGU,*)'t_aux ac1_taria_prog taria:',t_aux
     *                    ,ac1_taria_prog(itg),taria
	        delta_ac=q_prog_aux*t_aux/aux
              write(LGU,*)'delta_ac:',delta_ac
              write(LGU,*)'deltat_ric:',deltat_ric
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
cEv2024       deltat=min(deltat_ric,delta_ac)
              if (delta_ac.ge.deltat_ric) then
                  deltat = deltat_ric
              elseif (delta_ac.lt.deltat_ric) then
                  deltat = delta_ac
              end if   
              write(LGU,*)'deltat:',deltat
              pres_for_fase2 = pin
              temp_for_fase2 = unit_temp - deltat
	     end if
           write(LGU,*)'FINE'
           write(LGU,*)'///\\\'
           write(LGU,*)''
          else   !fase 2
          write(LGU,*)'FASE 2'
           write(LGU,*)'INDICE TURBOGRUPPO:',itg
           if (ac2_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
              write(LGU,*)'rr:',rr
              write(LGU,*)'P T out compressore:',pin,unit_temp
              call zpt1(ind,pin,unit_temp,zeta)
              write(LGU,*)'zeta out compressore:',zeta
	        ro0=pin/(rr*unit_temp*zeta)
              write(LGU,*)'ro0:',ro0
cEv2024
              write(LGU,*)'P T IN compressore:',p1,t1
              call zpt1(ind,p1,t1,zeta)
              write(LGU,*)'zeta in compressore:',zeta
              ro=p1/(rr*t1*zeta) 
              write(LGU,*)'ro:',ro
cEv2024 end              
              
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
              write(LGU,*)'temp_aux in calccp:',temp_aux
              write(LGU,*)'ro0 in calccp:',ro0
	        call calccp(ind,ro0,temp_aux,ctpc,ctvc)
              write(LGU,*)'Cp Cv:',ctpc,ctvc
cgpe-end
              write(LGU,*)'Flow:',flow
              aux=ro*flow*conv_qm*ctpc
              write(LGU,*)'aux:',aux
	        q_prog_aux = ac2_q_prog(itg)*3600./4.186
              write(LGU,*)'q_prog_aux:',q_prog_aux
		    t_aux=ac2_taria_prog(itg)/taria
             write(LGU,*)'t_aux ac2_taria_prog taria:',t_aux
     *                    ,ac2_taria_prog(itg),taria
	        delta_ac=q_prog_aux*t_aux/aux
              write(LGU,*)'delta_ac:',delta_ac
              write(LGU,*)'deltat_ric:',deltat_ric
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
cEv2024              deltat=min(deltat_ric,delta_ac)
              if (delta_ac.ge.deltat_ric) then
                  deltat = deltat_ric
              elseif (delta_ac.lt.deltat_ric) then
                  deltat = delta_ac
              end if
              write(LGU,*)'deltat:',deltat
            end if
            write(LGU,*)'FINE'
            write(LGU,*)'///\\\'
            write(LGU,*)''
	   end if
	end if

c
c      close(120)
	return
      end
c
c*********************************************************************************
c*********************************************************************************
c 
cevo_2024
	Subroutine out_temper_new(tair,tout,tin,zin,pin,pout,aux2,z_coef,
c-ace*           eff_coef,unit_num,status,unit_perc,unit_eff,
     *           dh_coef,cp_par,unit_num,status,unit_perc,unit_eff,
     *           unit_flow,unit_temp,unit_bifase,tmax_man)
c
c**********************************************************************************
c    Calcola la temperatura di uscita dal singolo turbogruppo e dalla centrale 
c    tenendo conto sia del piping che dell'eventuale presenza di air cooler a 
c    livello di unità.Tale subroutine è chiamata esclusivamente dalla subroutine 
c    station (lib_cen_ug_p.for) che simula il criterio dei giri in una centrale
c    semplice o composta in parallelo;la subroutine si usa al posto della
c    out_temper_mf chiamata dai moduli analoghi poichè in questo caso i dati relativi 
c    ai compressori di seconda fase non sono memorizzati in appositi vettori (es.
c    unit2_flow,unit2_temp...) ma nei vettori relativi alla prima fase dopo un primo 
c    blocco di informazioni dimensionato unit_num(perchè il modulo è stato
c    recuperato dal vecchio sire). 
c**********************************************************************************
c
        implicit none
        include '../inc/RK_PARAM.inc'
        include '../inc/PARAM.inc'
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_num  I	total number of unit in the station
c	status    I	status of each unit
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
c---------------->Input
      integer*4 istaz       !indice della centrale nella struttura dati completa
     *         ,vert_pointer !indice della centrale nella struttura dati ridotta
c
      real*4    tair  !temperatura dell'aria (stations.inc)
	REAL*4	  tout,tin,pin,zin,pout,aux2,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*)
     *         ,unit_flow(*),
     *          tmax_man  !max temperatura di mandata per la centrale (TOP)
	INTEGER*4   unit_num
	LOGICAL*2   status(*)
c
        INTEGER*4   nu,i,nt
c-ace 
c-ace REAL*4 eff_coef  
      REAL*4 dh_coef(*),cp_par(*)
c-ace
        real*4    temp,temp1,delta
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
c
        logical*2 unit_bifase(*)  !flag bifase
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp
        real*4 deltat
c---------------------------------------------------------------------
	nu = 0
	do i =1,unit_num
	  if(status(i)) then

	    call single_out_temp_new(unit_temp(i),tin,zin,pin,pout,
     *         aux2,z_coef,dh_coef,cp_par,unit_eff(i))
c---------->

c-28/4/06
c          call single_out_temp_ac(tair,pout,i,1,
c     *                       unit_flow(i),tmax_man,unit_temp(i))
          write(LGU,*)'AC CALL 01'
          call single_out_temp_ac_new(tair,pout,i,1,
     *            unit_flow(i),tmax_man,unit_temp(i),pin,tin,deltat)

c-28/4/06
c          tout = tout + unit_perc(i)*unit_temp(i)
          tout = tout + unit_perc(i)*(unit_temp(i)-deltat)
           
		if (unit_bifase(i)) then
	        call single_out_temp_new(unit_temp(i+unit_num),
     *        tin,zin,pin,pout,
     *        aux2,z_coef,dh_coef,cp_par,unit_eff(i+unit_num))
c-ace
c--------->modifica relativa al deltat di aircooler
c-28/4/06
c	        call single_out_temp_ac(tair,pout,i,2,
c     *            unit_flow(i+unit_num),tmax_man,unit_temp(i+unit_num))  !new
              write(LGU,*)'AC CALL 02'
	        call single_out_temp_ac_new(tair,pout,i,2,
     *            unit_flow(i+unit_num),tmax_man,unit_temp(i+unit_num),
     *            pin,tin,deltat)  !new
c
c---->               station outlet temperature
c-28/4/06
c              tout = tout + unit_perc(i+unit_num)*unit_temp(i+unit_num)
              tout = tout + 
     *              unit_perc(i+unit_num)*(unit_temp(i+unit_num)-deltat)
            endif
	  end if
	end do
	return
	end
c*********************************************************************************

c****************************************************************************
	Subroutine out_temper_int(tair,tout,tin,zin,pin,pout,aux2,z_coef,
     *			ntp,type_actr,type_quant,
c-ace*            eff_coef,
     *            dh_coef,cp_par,
     *			unit_perc,unit_eff,unit_flow,
     *            unit_temp,
     *            delt_int,max_tint)  !
c*****************************************************************************
c	Calcola la temperatura di uscita da ogni compressore della prima
c     fase per una stazione composta che lavora in serie (modulo E)
c*****************************************************************************
        implicit none
C
	include '../inc/RK_PARAM.INC'
      include '../inc/PARAM.INC'
C
c	tout      O	station outlet temperature
c	tin       I	inlet station temperature
c	zin       I	compressibility factor at the inlet condition
c	pin       I	inlet pressure
c	pout      I	outlet pressure
c	aux2      I	cofficient function of the compr_ratio
c	eff_coef  I	coefficient fo outlet temperature computation
c	unit_perc I	flow rate partition
c	unit_eff  I	efficiency of each compressor
c	unit_temp O	gas outlet temperature from each compressor
c-----------------------------------------------------------------------
c
      integer*4 istaz       !indice della centrale nella struttura dati completa
     *         ,vert_pointer !indice della centrale nella struttura dati ridotta
c
      real*4    tair  !temperatura dell'aria (stations.inc)
cgpe	REAL	tout,tin,pin,zin,pout,aux2,eff_coef,z_coef(*),
	REAL*4	tout,tin,pin,zin,pout,aux2,eff_coef,z_coef(*),
     *          unit_perc(*),unit_eff(*),unit_temp(*),unit_flow(*)

      real*4    delt_int !(I) deltap intermedio per piping -stat_vars9
     *         ,max_tint !(I) massima temperatura intermedia -stat_vars10
      
	INTEGER*4   ntp,type_actr(*),type_quant(*)
c
        INTEGER*4 i,nu,ic,icj,nt
c-ace
      REAL*4 dh_coef(*),cp_par(*)
c-ace REAL*4 eff_coef
c_ace
        real*4    temp,temp1,delta
        real*4    zout
        real*4    dt,dtmin,dz,df
        real*4    T_ADIM,P_ADIM
c
        real*4    prop
        external  prop
c
c        DATA dtmin /0.005/
        DATA dtmin /0.01/
        real*4 dtp,deltat,temp_int
        integer*4 idummy
c---------------------------------------------------------------------
	nu = 0
	do ic =1,ntp
          do i=1,type_quant(ic)
            icj=nu+i
            if (unit_eff(icj).gt.0) then
c---->                         outlet theoretical temperature extimation

	    call single_out_temp_new(unit_temp(icj),tin,zin,pin,pout,
     *         aux2,z_coef,dh_coef,cp_par,unit_eff(icj))
c-ace
c------->tengo conto del piping
c28/4          unit_temp(icj)=unit_temp(icj)-delt_int
          temp_int=unit_temp(icj)-delt_int

c-28/4/06
c	    call single_out_temp_ac(tair,pout,icj,1,
c     *               unit_flow(icj),max_tint,unit_temp(icj))
          write(LGU,*)'AC CALL 03'

	    call single_out_temp_ac_new(tair,pout,icj,1,
     *            unit_flow(icj),max_tint,temp_int,pin,tin,deltat)
          temp_int=temp_int-deltat

c
c---->               station outlet temperature
c              tout = tout + unit_perc(icj)*unit_temp(icj)
              tout = tout + unit_perc(icj)*temp_int
            endif
          enddo
          nu = nu+type_quant(ic)
	end do
	return
	end

c**********************************************************************************
c
c*********************************************************************************
c
     
      subroutine single_out_temp_ac_ns(taria,pin,itg,fase,
c-ele-aggiungo parametro di output
c 28/4/06     *                              flow,tmax_man,unit_temp)
     *                        flow,tmax_man,unit_temp,p1,t1,deltat)
c**********************************************************************************
c    Subroutine per il calcolo della temperatura di uscita dall'air cooler
c    quando questo è stato introdotto a livello di unità;se l'air cooler non è 
c    stato inserito la temperatura d'uscita è uguale a quella d'ingresso.
c**********************************************************************************
      implicit none

      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
      include '../inc/mf_simpstat.inc'
      include '../inc/rk.inc'	
      include '../inc/conv.inc'	


c--------->Input
      real*4 taria          !temperatura dell'aria 
	                      !(memorizzata in 'stations.inc')
     *      ,pin            !pressione in ingresso all'air cooler[bar]
                            !(=pressione di USCITA dal singolo TG)
c
      integer*4 itg    !indice del turbogruppo
c
     *      ,fase      !fase di inserimento dell'ac
c
c
      real*4 flow      !portata lavorata dal compressore [km3]/[h]
      
      real*4 p1     !nuovo parametro evolutiva 2024  
      
      real*4 t1,ro0   !nuovi parametri evolutiva 2024  
c
     *      ,tmax_man !temperatura massima di mandata centrale
     

c---------->Input/Output
c-28/4/06
      real*4 unit_temp !I)temperatura in ingresso all'aircooler 
	                 !(=temperatura di uscita al singolo TG )
	                 !O)temperatura in uscita all'air cooler
      real*4 deltat    !O)DELTAT prodotto dall'air cooler

c---------->variabili locali
      real*4 rr 
     *      ,zeta
     *      ,ro 
     *      ,ctpc
     *      ,ctvc
     *      ,t_aux
     *      ,aux
     *      ,tout
     *      ,q_prog_aux    !per trasformazione da [kW] a [kcal]/[h] 
     *      ,deltat_ric
     *      ,delta_ac
cgpe
      real*4 temp_aux
cgpe-end
c---------------------------------------------------------------------------------
cTestScrittura intermedia
c      OPEN (unit = 120, file='C:\LOG_FORTRAN\AC_NEW.txt',
c     *    status='old',access='append')
c      write(LGU,*)'START'
c      write(LGU,*)'---------------------------'
      
cEndTest
      deltat=0.
	deltat_ric = max(0.,unit_temp - tmax_man)

	if (deltat_ric.gt.0) then
	  if (fase.eq.1) then
c          write(LGU,*)'FASE 1'
c          write(LGU,*)'INDICE TURBOGRUPPO:',itg
          if (ac1_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
c              write(LGU,*)'rr:',rr
c              write(LGU,*)'P T out compressore:',pin,unit_temp
              call zpt1(ind,pin,unit_temp,zeta)
c              write(LGU,*)'zeta out compressore:',zeta
              ro0=pin/(rr*unit_temp*zeta)
c              write(LGU,*)'ro0:',ro0
cEv2024
c              write(LGU,*)'P T IN compressore:',p1,t1
             call zpt1(ind,p1,t1,zeta)
c             write(LGU,*)'zeta in compressore:',zeta
             ro=p1/(rr*t1*zeta) 
c              write(LGU,*)'ro:',ro
cEv2024 end              
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
	        call calccp(ind,ro0,temp_aux,ctpc,ctvc)
c              write(LGU,*)'Cp Cv:',ctpc,ctvc 
cgpe-end
c              write(LGU,*)'Flow:',flow
              aux=ro*flow*conv_qm*ctpc
c              write(LGU,*)'aux:',aux
	        q_prog_aux = ac1_q_prog(itg)*3600./4.186 
c              write(LGU,*)'q_prog_aux:',q_prog_aux
 	        t_aux=ac1_taria_prog(itg)/taria
c              write(LGU,*)'t_aux ac1_taria_prog taria:',t_aux
c     *                    ,ac1_taria_prog(itg),taria
	        delta_ac=q_prog_aux*t_aux/aux
c             write(LGU,*)'delta_ac:',delta_ac
c             write(LGU,*)'deltat_ric:',deltat_ric
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
cEv2024       deltat=min(deltat_ric,delta_ac)
              if (delta_ac.ge.deltat_ric) then
                  deltat = deltat_ric
              elseif (delta_ac.lt.deltat_ric) then
                  deltat = delta_ac
              end if   
c              write(LGU,*)'deltat:',deltat
	     end if
c           write(LGU,*)'FINE'
c           write(LGU,*)'///\\\'
c           write(LGU,*)''
          else   !fase 2
c          write(LGU,*)'FASE 2'
c           write(LGU,*)'INDICE TURBOGRUPPO:',itg
           if (ac2_stat(itg).eq.ac_on) then
c                              proprietà del gas lavorato dalla centrale
              rr=r_mks*romks/pmks/av_weight(ind)
c              write(LGU,*)'rr:',rr
c              write(LGU,*)'P T out compressore:',pin,unit_temp
              call zpt1(ind,pin,unit_temp,zeta)
c              write(LGU,*)'zeta out compressore:',zeta
	        ro0=pin/(rr*unit_temp*zeta)
c              write(LGU,*)'ro0:',ro0
cEv2024
c              write(LGU,*)'P T IN compressore:',p1,t1
              call zpt1(ind,p1,t1,zeta)
c              write(LGU,*)'zeta in compressore:',zeta
               ro=p1/(rr*t1*zeta) 
c              write(LGU,*)'ro:',ro
cEv2024 end              
              
cgpe	        call calccp(ind,ro,unit_temp,ctpc,ctvc)
              temp_aux = (unit_temp + tmax_man) / 2.
	        call calccp(ind,ro0,temp_aux,ctpc,ctvc)
c              write(LGU,*)'Cp Cv:',ctpc,ctvc
cgpe-end
c              write(LGU,*)'Flow:',flow
              aux=ro*flow*conv_qm*ctpc
c              write(LGU,*)'aux:',aux
	        q_prog_aux = ac2_q_prog(itg)*3600./4.186
c              write(LGU,*)'q_prog_aux:',q_prog_aux
		    t_aux=ac2_taria_prog(itg)/taria
c              write(LGU,*)'t_aux ac2_taria_prog taria:',t_aux
c     *                    ,ac2_taria_prog(itg),taria
	        delta_ac=q_prog_aux*t_aux/aux
c              write(LGU,*)'delta_ac:',delta_ac
c              write(LGU,*)'deltat_ric:',deltat_ric
c-28/4              unit_temp=unit_temp-min(deltat_ric,delta_ac)
cEv2024              deltat=min(deltat_ric,delta_ac)
              if (delta_ac.ge.deltat_ric) then
                  deltat = deltat_ric
              elseif (delta_ac.lt.deltat_ric) then
                  deltat = delta_ac
              end if
c              write(LGU,*)'deltat:',deltat
            end if
c            write(LGU,*)'FINE'
c            write(LGU,*)'///\\\'
c            write(LGU,*)''
	   end if
	end if

c
c      close(120)
	return
      end
c
c*********************************************************************************

