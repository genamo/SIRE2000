*****************************************************************************
c    Libreria contenente le subroutine per il calcolo della caduta di pressione
c    associata ai filtri quando sono inseriti a livello di centrale o di turbogruppo
*****************************************************************************
      subroutine delpr_filtro(type_num,type_quant,status,nom_flow,
     *           f_ce_k,f_tg_k,q,delpr_f)
c------------------------------------------------------------------------------
c  Questa subroutine calcola la caduta di pressione associata al filtro in ingresso:
c  se il filtro è stato introdotto a livello di centrale applico direttamente la
c  formula quadratica delpr_f=f_k*q*q ; se invece i filtri sono stati introdotti
c  a livello di turbogruppo ricavo un unico f_k come media e poi applico la 
c  formula, riportandomi così a livello di centrale.
c  Se il filtro non è stato inserito in nessuno dei due casi viene restituito un
c  delta nullo.
c  La caduta di pressione prodotta dal filtro va sommata algebricamente a quella
c  dovuta al piping.
c------------------------------------------------------------------------------
c NOTA BENE: l'attivazione a livello di TG riguarda tutte le unità attive nella 
c centrale analizzata.
c------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ass_cond.inc'
c
cc

c
c----------->INPUT
      integer*4 type_num,     !numero di tipi di TBG
     *          type_quant(*) !numero di TBG per ogni tipo
      logical*2 status(*)     !stato del TBG
      real*4    nom_flow(*)   !portata nominale dei TBG
      real*4    q             !portata lavorata dalla stazione
	real*4    f_ce_k,f_tg_k(*)
c
c----------->OUTPUT
      real*4 delpr_f          !caduta di pressione associata al filtro
c 
c----------->variabili locali
      real*4    kf,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,f_nu,nu
	real*4 delpr_ce,delpr_tg
c----------------------------------------------------------------------------
      delpr_f=0.
      delpr_ce = 0.
	delpr_tg=0.

c------->>>>contributo del filtro di centrale
cc
cmar_ass_cond      if(.not.flag_ass_cond_s)then
      qq=q*q
	delpr_ce=f_ce_k*qq
cmar_ass_cond	endif

	
cc
cgpe      if (f_stat .ne. fil_assente) then
c      if (f_ce_stat.ne. fil_assente.and.f_ce_stat.ne.fil_off) then
c filtro presente e definito non spento
c-ele        if (f_ce) then
c filtro localizzato a livello di centrale/assetto
c           qq = q*q
c           delpr_ce = f_ce_k*qq
c      end if

c------->>>>contributo del filtro di tbg
    
 200    nu = 0
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

c filtro localizzato a livello di TBG
c          kf = 0.
      nu = 0
      f_nu = 0
      do i = 1,type_num
         do ij = 1,type_quant(i)
            icj = nu+ij
            delp(icj) = 0.
            if (status(icj))then
cgpe                kf = kf + f_k(icj)
                f_nu = f_nu + 1
                q_un = q * (nom_flow(icj)/sum_flow)
                delp(icj) = f_tg_k(icj) * (q_un * q_un)
                delpr_tg = delpr_tg + delp(icj)
	      end if
          end do
          nu = nu+type_quant(i)
      end do
      if (f_nu .gt. 0) then
c            kf = kf/f_nu
c            qq = (q/f_nu)*(q/f_nu)
c            delpr_f = kf*qq
          delpr_tg = delpr_tg / f_nu
      end if

	delpr_f=delpr_ce+delpr_tg
c
      return
	end     !subroutine delpr_filtro


      subroutine delpr_filtro1(type_num,type_quant,status,nom_flow,
     *           f_ce_k,f_tg_k,q,delpr_f)
c------------------------------------------------------------------------------
c  Questa subroutine calcola la caduta di pressione associata al filtro in ingresso:
c  se il filtro è stato introdotto a livello di centrale applico direttamente la
c  formula quadratica delpr_f=f_k*q*q ; se invece i filtri sono stati introdotti
c  a livello di turbogruppo ricavo un unico f_k come media e poi applico la 
c  formula, riportandomi così a livello di centrale.
c  Se il filtro non è stato inserito in nessuno dei due casi viene restituito un
c  delta nullo.
c  La caduta di pressione prodotta dal filtro va sommata algebricamente a quella
c  dovuta al piping.
c------------------------------------------------------------------------------
c NOTA BENE: l'attivazione a livello di TG riguarda tutte le unità attive nella 
c centrale analizzata.
c------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ass_cond.inc'
c
cc

c
c----------->INPUT
      integer*4 type_num,     !numero di tipi di TBG
     *          type_quant(*) !numero di TBG per ogni tipo
      logical*2 status(*)     !stato del TBG
      real*4    nom_flow(*)   !portata nominale dei TBG
      real*4    q             !portata lavorata dalla stazione
	real*4    f_ce_k,f_tg_k(*)
c
c----------->OUTPUT
      real*4 delpr_f          !caduta di pressione associata al filtro
c 
c----------->variabili locali
      real*4    kf,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,f_nu,nu
	real*4 delpr_ce,delpr_tg
c----------------------------------------------------------------------------
      delpr_f=0.
      delpr_ce = 0.
	delpr_tg=0.

c------->>>>contributo del filtro di centrale
cc
cmar_ass_cond      if(.not.flag_ass_cond_s)then
      qq=q*q
	delpr_ce=dp_f_1*qq
cmar_ass_cond	endif

	
cc
cgpe      if (f_stat .ne. fil_assente) then
c      if (f_ce_stat.ne. fil_assente.and.f_ce_stat.ne.fil_off) then
c filtro presente e definito non spento
c-ele        if (f_ce) then
c filtro localizzato a livello di centrale/assetto
c           qq = q*q
c           delpr_ce = f_ce_k*qq
c      end if

c------->>>>contributo del filtro di tbg
    
 200    nu = 0
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

c filtro localizzato a livello di TBG
c          kf = 0.
      nu = 0
      f_nu = 0
      do i = 1,type_num
         do ij = 1,type_quant(i)
            icj = nu+ij
            delp(icj) = 0.
            if (status(icj))then
cgpe                kf = kf + f_k(icj)
                f_nu = f_nu + 1
                q_un = q * (nom_flow(icj)/sum_flow)
                delp(icj) = f_tg_k(icj) * (q_un * q_un)
                delpr_tg = delpr_tg + delp(icj)
	      end if
          end do
          nu = nu+type_quant(i)
      end do
      if (f_nu .gt. 0) then
c            kf = kf/f_nu
c            qq = (q/f_nu)*(q/f_nu)
c            delpr_f = kf*qq
          delpr_tg = delpr_tg / f_nu
      end if

	delpr_f=delpr_ce+delpr_tg
c
      return
	end     !subroutine delpr_filtro




	subroutine delpr_filtro2(type_num1,type_num,type_quant,status,
     *           nom_flow,f_ce_k,f_tg_k,q,delpr_f)
c------------------------------------------------------------------------------
c  Questa subroutine calcola la caduta di pressione associata al filtro in ingresso:
c  se il filtro è stato introdotto a livello di centrale applico direttamente la
c  formula quadratica delpr_f=f_k*q*q ; se invece i filtri sono stati introdotti
c  a livello di turbogruppo ricavo un unico f_k come media e poi applico la 
c  formula, riportandomi così a livello di centrale.
c  Se il filtro non è stato inserito in nessuno dei due casi viene restituito un
c  delta nullo.
c  La caduta di pressione prodotta dal filtro va sommata algebricamente a quella
c  dovuta al piping.
c------------------------------------------------------------------------------
c NOTA BENE: l'attivazione a livello di TG riguarda tutte le unità attive nella 
c centrale analizzata.
c------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/ass_cond.inc'
c
cc

c
c----------->INPUT
      integer*4 type_num,     !numero di tipi di TBG
     *          type_quant(*) !numero di TBG per ogni tipo
      logical*2 status(*)     !stato del TBG
      real*4    nom_flow(*)   !portata nominale dei TBG
      real*4    q             !portata lavorata dalla stazione
	real*4    f_ce_k,f_tg_k(*)
c
c----------->OUTPUT
      real*4 delpr_f          !caduta di pressione associata al filtro
c 
c----------->variabili locali
      real*4    kf,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,f_nu,nu
	real*4 delpr_ce,delpr_tg


	integer*4 type_num1
c----------------------------------------------------------------------------
      delpr_f=0.
      delpr_ce = 0.
	delpr_tg=0.

c------->>>>contributo del filtro di centrale
cc
cmar_ass_cond      if(.not.flag_ass_cond_s)then
      qq=q*q
	delpr_ce=dp_f_2*qq
cmar_ass_cond	endif

	
cc
cgpe      if (f_stat .ne. fil_assente) then
c      if (f_ce_stat.ne. fil_assente.and.f_ce_stat.ne.fil_off) then
c filtro presente e definito non spento
c-ele        if (f_ce) then
c filtro localizzato a livello di centrale/assetto
c           qq = q*q
c           delpr_ce = f_ce_k*qq
c      end if

c------->>>>contributo del filtro di tbg
    
 200    nu = 0
      sum_flow = 0.
      do i = type_num1+1,type_num1+type_num
         do ij = 1,type_quant(i)
c            icj = nu+ij
            icj = nu+ij+unit_1
            if (status(icj))then
                sum_flow = sum_flow + nom_flow(icj)
            end if
          end do
          nu = nu+type_quant(i)
      end do

c filtro localizzato a livello di TBG
c          kf = 0.
      nu = 0
      f_nu = 0
      do i = type_num1+1,type_num1+type_num
         do ij = 1,type_quant(i)
c            icj = nu+ij
            icj = nu+ij+unit_1
            delp(icj) = 0.
            if (status(icj))then
cgpe                kf = kf + f_k(icj)
                f_nu = f_nu + 1
                q_un = q * (nom_flow(icj)/sum_flow)
                delp(icj) = f_tg_k(icj) * (q_un * q_un)
                delpr_tg = delpr_tg + delp(icj)
	      end if
          end do
          nu = nu+type_quant(i)
      end do
      if (f_nu .gt. 0) then
c            kf = kf/f_nu
c            qq = (q/f_nu)*(q/f_nu)
c            delpr_f = kf*qq
          delpr_tg = delpr_tg / f_nu
      end if

	delpr_f=delpr_ce+delpr_tg
c
      return
	end     !subroutine delpr_filtro

c----------------------------------------------------------------------------------
      subroutine delpr_filtro_tc(type_num,type_quant,status,nom_flow,
     *           f_ce_k,f_tg_k,q,delpr_f)
c------------------------------------------------------------------------------
c  Questa subroutine calcola la caduta di pressione associata al filtro in ingresso:
c  se il filtro è stato introdotto a livello di centrale applico direttamente la
c  formula quadratica delpr_f=f_k*q*q ; se invece i filtri sono stati introdotti
c  a livello di turbogruppo ricavo un unico f_k come media e poi applico la 
c  formula, riportandomi così a livello di centrale.
c  Se il filtro non è stato inserito in nessuno dei due casi viene restituito un
c  delta nullo.
c  La caduta di pressione prodotta dal filtro va sommata algebricamente a quella
c  dovuta al piping.
c------------------------------------------------------------------------------
c NOTA BENE: l'attivazione a livello di TG riguarda tutte le unità attive nella 
c centrale analizzata.
c------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
c
c----------->INPUT
      integer*4 type_num,     !numero di tipi di TBG
     *          type_quant(*) !numero di TBG per ogni tipo
      logical*2 status(*)     !stato del TBG
      real*4    nom_flow(*)   !portata nominale dei TBG
      real*4    q             !portata lavorata dalla stazione
	real*4    f_ce_k,f_tg_k(*)
c
c----------->OUTPUT
      real*4 delpr_f          !caduta di pressione associata al filtro
c 
c----------->variabili locali
      real*4    kf,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,f_nu,nu
	real*4 delpr_ce,delpr_tg
c----------------------------------------------------------------------------
      delpr_f=0.
      delpr_ce = 0.
	delpr_tg=0.

c------->>>>contributo del filtro di centrale

      qq=q*q
	delpr_ce=f_ce_k*qq

cgpe      if (f_stat .ne. fil_assente) then
c      if (f_ce_stat.ne. fil_assente.and.f_ce_stat.ne.fil_off) then
c filtro presente e definito non spento
c-ele        if (f_ce) then
c filtro localizzato a livello di centrale/assetto
c           qq = q*q
c           delpr_ce = f_ce_k*qq
c      end if

c------->>>>contributo del filtro di tbg
    
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

c filtro localizzato a livello di TBG
c          kf = 0.
      nu = 0
      f_nu = 0
      do i = 1,type_num
         do ij = 1,type_quant(i)
            icj = nu+ij
            delp(icj) = 0.
            if (status(icj))then
cgpe                kf = kf + f_k(icj)
                f_nu = f_nu + 1
                q_un = q * (nom_flow(icj)/sum_flow)
                delp(icj) = f_tg_k(icj) * (q_un * q_un)
                delpr_tg = delpr_tg + delp(icj)
	      end if
          end do
          nu = nu+type_quant(i)
      end do
      if (f_nu .gt. 0) then
c            kf = kf/f_nu
c            qq = (q/f_nu)*(q/f_nu)
c            delpr_f = kf*qq
          delpr_tg = delpr_tg / f_nu
      end if

	delpr_f=delpr_ce+delpr_tg
c
      return
	end     !subroutine delpr_filtro
c----------------------------------------------------------------------------------

      subroutine delpr_max(type_num,type_quant,status,nom_flow,
     *                a,ss,rr,f_ce_k,f_tg_k,q,delpr_f, ss_primo, 
     *                rr_primo,df_1, df_2, dac_1, dac_2,type_actr)
c------------------------------------------------------------------------------
c  Questa subroutine calcola la caduta di pressione associata al filtro in ingresso:
c  se il filtro è stato introdotto a livello di centrale applico direttamente la
c  formula quadratica delpr_f=f_k*q*q ; se invece i filtri sono stati introdotti
c  a livello di turbogruppo ricavo un unico f_k come media e poi applico la 
c  formula, riportandomi così a livello di centrale.
c  Se il filtro non è stato inserito in nessuno dei due casi viene restituito un
c  delta nullo.
c  La caduta di pressione prodotta dal filtro va sommata algebricamente a quella
c  dovuta al piping.
c------------------------------------------------------------------------------
c NOTA BENE: l'attivazione a livello di TG riguarda tutte le unità attive nella 
c centrale analizzata.
c------------------------------------------------------------------------------
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
c	include '../inc/SIMPSTAT.INC'
c
c----------->INPUT
      integer*4 type_num,     !numero di tipi di TBG
     *          type_quant(*), !numero di TBG per ogni tipo
     *          type_actr(*)

      logical*2 status(*)     !stato del TBG
      real*4    nom_flow(*)   !portata nominale dei TBG
      real*4    q             !portata lavorata dalla stazione
	real*4    f_ce_k,f_tg_k(*)
c
c----------->OUTPUT
      real*4 delpr_f          !caduta di pressione associata al filtro
c 
c----------->variabili locali
      real*4    kf,qq,sum_flow,delp(maxunits),q_un
      integer*4 i,ij,icj,f_nu,nu
	real*4 delpr_ce,delpr_tg
	real*4 a(maxunits), ss_, rr_, portata_min_tot, coef_,
     *       ss_primo, rr_primo, df_1, df_2, dac_1, dac_2
       real*4 delp_1, delp_2,delp_ac_1, delp_ac_2   
	integer*4 ss(maxunits),rr(maxunits)
c----------------------------------------------------------------------------
      delpr_f=0.

	delp_1=0.
      

      ss_=0
cmar: va sui type:
      do i=1, 1 ! 4-0=4: dim. di ss
	ss_=ss_+type_actr(i)*a(i)
	enddo

	rr_=0
	do i=2,2 ! 6-4= 2: dim. di rr
	rr_=rr_+type_actr(i)*a(i)
	enddo

	portata_min_tot= ss_+ rr_

ccccprima	coef_= q/portata_min_tot
      
cmar : gli _primo sono le portate:

ccccprima	ss_primo=ss_*coef_

ccccprima	rr_primo=rr_*coef_
      coef_ = ss_/portata_min_tot
      
      ss_primo=q*coef_
      rr_primo=q*(1-coef_)



      delp_1=delpr_ce
	delp_2=delpr_ce

	df_1= delp_1*(ss_primo)*(ss_primo)

	df_2= delp_2*(rr_primo)*(rr_primo)

	dac_1 = delp_ac_1*(ss_primo)*(ss_primo)

	dac_2 = delp_ac_2*(rr_primo)*(rr_primo)


c
      return
	end     !
c----------------------------------------------------------------------------------
