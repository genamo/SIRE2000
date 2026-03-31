c------------------------------------------------------------------------------------------------
c   Pacchetto contenente le subroutine e le function per il trattamento di impianti di 
c   riduzione e di regolazione
c------------------------------------------------------------------------------------------------
c
c                             TIPOLOGIE DI VALVOLE
c   Valvole semplificate: e_vs --->valvole di sezionamento
c                         e_vr --->valvole di riduzione
c   Impianti:             e_vd --->impianti di riduzione
c                         e_vg --->impianti di regolazione
c
c   Ai dati relativi all'impianto di regolazione memorizzati nella common TOP_APP_ELE,
c   dimensionate MAX_VG, si accede con l'indice dell'elemento nella struttura dati
c   ridotta, ossia i_vg che varia nell'intervallo [1,max_vg].Alle aree dati dimensionate
c   M_OGG si accede con l'indice dell'elemento nella struttura dati completa, ossia 
c   i_elem che varia nell'intervallo [otipti(e_vg),etipti(e_vg)] a sua volta
c   contenuto nell'intervallo [1,m_ogg]

c---------------------------------------------------------------------------
c  Calcolo della portata massima di un impianto di regolazione
c---------------------------------------------------------------------------
      subroutine qmaxvg(i_elem,qmax,flag_vlim)
c***************************************************************************
c   calcola la portata massima di una valvola regolante 
c***************************************************************************
      implicit none

	include '../inc/param.inc' 
	include '../inc/rk.inc' 
	include '../inc/CC.inc' 
	include '../inc/ti.inc' 
	include '../inc/tj.inc' 
	include '../inc/top_app_ele.inc' 
	include '../inc/cin_app_ele.inc'
	include '../inc/staznw_par.inc'  !per includere maxit

	

      integer*4 i_elem      !indice dell'elemento nella struttura dati completa
     *                      !1<i_elem<m_ogg

	real*4 qmax   !portata massima impianto di regolazione

	logical*2 flag_vlim !T--->fattore limitante è la vlim
	                    !F--->fattore limitante è l'impianto
  
      real*4 enne              !La definizione di questa costante dipende dal
	parameter (enne=27.3)    !dimensionamento dei parametri; per il valore 
	                         !assegnato, vedi tabella a pg 73 manuale valvole
	                         !Le UDM devono essere:
	                         !Qm in Kg/h;Qv in Nm3/h;p in bar ass;ro in kg/m3;
	                         !T in K;alcune di queste non corrispondono alle
	                         !UDM interne e quindi per potere usare tale valore
	                         !dobbiamo effettuare delle opportune conversioni

	real*4 fp_vg             !fattore di piping valvola regolante
	external fp_vg
	real*4 pmonitor          !pressione di uscita valvola monitor 
	external pmonitor

	integer*4 im,iv
     *         ,i_vg		  !indice dell'elemento nella struttura dati ridotta
     *                      !1<i_vg<max_vg						


	real*4 pin,x,xlim
	real*4 diam_vg_out,pres_out,temp_out, temp_inp

      real*4 rr
     *      ,pmol
     *      ,zcn
     *      ,rocn 
     *      ,uno_su_ro_attuale
     *      ,zeta
     *      ,ro 
     *      ,area,qlim,porttj_linea
cgpe
      real*4   aux,ro_mol_cn,ro_cn,y
	real*4   fgamma
	external fgamma
cgpe-end
crsa
	real*4 qmax_input
	integer*4 ntrov
	logical*2 f_trov
crsa
c---------------------------------------------------------------------------

      im=opumto(i_elem)
      iv=opuvto(i_elem)
	i_vg=pgrato(i_elem)

c*******************
c  Nel caso in cui la direzione della valvola venga violata, ai fini del calcolo
c  inverto le pressioni
c*******************
	if (prestj(im).lt.prestj(iv))then
	   pres_out=prestj(im)
	   temp_out=temptj(im)
	   diam_vg_out=diam_in(i_vg)

	   temp_inp=temptj(iv)

	else
	   pres_out=prestj(iv)
	   temp_out=temptj(iv)
	   diam_vg_out=diam_out(i_vg)

	   temp_inp=temptj(im)

	end if



c      pmol = av_weight(i_elem)
c	 call zpt1(i_elem,PCN,TCN,ZCN)
c      ROCN = (pmol*PCN)*(1.e5)/(erre*ZCN*TCN)
c      call zpt1(i_elem,prestj(im),temptj(im),zeta)
c      uno_su_ro_attuale=(1/1.e5)*(erre/pmol)*zeta*temptj(im)/prestj(im)
c      aux = ROCN*uno_su_ro_attuale

      ! la subroutine ZPT1 lavora con pressioni in Bar e temperature in Kelvin
	call zpt1(i_elem,PCN,TCN,ZCN)  !zeta alle condizioni normali
      call zpt1(i_elem,pres_out,temp_out,zeta) !zeta alle cond. di uscita dell'impianto
c  AUX è il rapporto RO_CN/RO_C_OP;essendo
c       RO=(1.e5*pmol*pres)/(erre*zeta*temp)
c  semplificando si ottiene:
      aux=(temp_out/TCN)*(zeta/ZCN)*(PCN/pres_out)  

	
      area=PGR4cc*diam_vg_out**2
	qlim=vlim_out(i_vg)*area    !qlim-->m3/s
	 ! dobbiamo esprimere QLIM come portata volumetrica la cui unità di 
	 ! misura interna è 1000*Nm3/h :
       ! m3/s --> m3/h --> 1000m3/h --> 1000 Nm3/h
       ! moltiplicando per 3600 passo da [m3/s] a [m3/h]
	 ! dividendo per 1000 passo da [m3/h] a [km3/h]
	 ! dividendo per AUX passo da [km3/h] a [kNm3/h]
	 ! quindi:
	 ! [k*Nm3/h]=(3600/1000)*(1/aux)[m3/s]
	qlim=3.6*qlim/aux

c	porttj_linea=abs(porttj(i_elem))/line_vg_on(i_vg)

c*********** La portata massima imposta dalla velocità limite dipende
c            unicamente dalla geometria della valvola;
c            tale valore è quindi assegnato alla qmax quando non viene 
c*********** rispettato il limite di confidenza della x.

c	if (abs(porttj_linea).gt.qlim) then
c	   qmax=qlim
c         flag_vlim=.true.
c	else

c     processo iterativo per Qmax dell'impianto, indipendente dalla
c     Q di ingresso 
      f_trov =.false.

	qmax_input = qlim
	ntrov = 0

      do while (.not.f_trov)
	   pin=pmonitor(i_elem,qmax_input)
	   rr=r_mks*romks/pmks/av_weight(i_elem)
	   call zpt1(i_elem,pin,temp_inp,zeta)
	   ro=pin/(rr*temp_inp*zeta)
         ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
         ro_cn = av_weight(i_elem) * ro_mol_cn / romks  !---> [kg]/[Nm3]

         x=1-(pres_out/pin)   !0<x<1

	   if (x.le.0.) then

	      qmax=qlim

	   else

	      xlim=fgamma(i_elem)*xtreg(i_vg)
 
	      if (x.le.xlim) then

	         y=1-x/(3.*xlim)

	         qmax=enne*fp_vg(i_elem)*cvreg(i_vg)*y*sqrt(x*pin*ro) !kg/h
	         qmax=qmax/ro_cn/1000     !kNm3/h

	      else
   	         y=2./3.

	         qmax=enne*fp_vg(i_elem)*cvreg(i_vg)*y*sqrt(xlim*pin*ro)
	         qmax=qmax/ro_cn/1000     !kNm3/h
         
	      end if
         end if

c     l' interazione termina quando la qmax_input, data in pasto alla pmonitor, è molto vicina alla qmax
         if (abs(qmax-qmax_input).le.zeroTC.or.
     -       ntrov.gt.maxit) then
	       f_trov = .true.
	   else
	     qmax_input = qmax
	     ntrov = ntrov +1
	   endif
      enddo
c vengono trovate sia la qlim (legata alla velocità limite), e qmax(legata alle condizioni operative deltap)
c infine viene assegnata all'impianto come portata massima, la minore tra le due, 
c e viene marcato il flag se la portata è quella legata alla velocità.


	   if (qlim.le.qmax) then
	      qmax=qlim
            flag_vlim=.true.
	   else
            flag_vlim=.false.
         end if
c      end if

c le portate finora considerate sono quelle sulla singola linea:
	qmax=qmax*line_vg_on(i_vg)

      return
	end 

c--------------------------------------------------------------------------------
      real*4 function Fp_vg(i_elem)
c***************************************************************
c     calcola il FATTORE DI PIPING Fp di una valvola regolante;
c     grandezza usata nel calcolo della portata massima di un
c     impianto di regolazione
c***************************************************************

      implicit none

	include '../inc/param.inc' 
	include '../inc/ti.inc' 
	include '../inc/top_app_ele.inc' !diametri e Cv

      integer*4 i_elem !indice dell'impianto nella struttura dati completa
     *               !1<i_elem<m_ogg
    
	real*4 k_in,k_out,
     *       kb_in,kb_out,
     *       k,
     *       denom,dnom_vg_mm

	integer*4 i_vg !indice dell'impianto nella struttura dati ridotta
     *               !1<i_vg<max_vg 

      real*4 enne
	parameter (enne=2.14E-03)  

	i_vg=pgrato(i_elem)

	k_in=0.5*(1-(dnom_vg(i_vg)/diam_in(i_vg))**2)**2
	k_out=(1-(dnom_vg(i_vg)/diam_out(i_vg))**2)**2

	kb_in=(1-(dnom_vg(i_vg)/diam_in(i_vg))**4)
	kb_out=(1-(dnom_vg(i_vg)/diam_out(i_vg))**4)

	k=k_in+k_out+kb_in-kb_out

	dnom_vg_mm=dnom_vg(i_vg)*1000  !conversione in mm del diamentro

      denom=sqrt(1+(k/enne)*(cvreg(i_vg)/dnom_vg_mm**2)**2)

	Fp_vg=1/denom

	return

	end   !function fp_vg
c--------------------------------------------------------------------------
      real*4 function Fgamma(i_elem)
c*************************************************************************
c     calcola il coefficiente correttivo Fgamma,rapporto tra l'esponente 
c     dell'adiabatica del gas  che transita attraverso la valvola e quello
c     dell'aria;usato nel calcolo della portata massima di un impianto di
c     regolazione
c*************************************************************************+
      implicit none
      
	include '../inc/param.inc' 
	include '../inc/ti.inc' 
	include '../inc/tj.inc' 
	include '../inc/rk.inc' 

      integer*4 i_elem !indice dell'impianto nella struttura dati completa
     *                 !1<i_elem<m_ogg

      real*4 rr 
     *      ,zeta
     *      ,ro 
     *      ,ctpc
     *      ,ctvc
     *      ,gamma
     *      ,pres,temp

	integer*4 im,iv
       
      im=opumto(i_elem)
      iv=opuvto(i_elem)

	if (prestj(im).lt.prestj(iv))then
	   pres=prestj(iv)
	   temp=temptj(iv)
	else
	   pres=prestj(im)
	   temp=temptj(im)
	end if

      rr=r_mks*romks/pmks/av_weight(i_elem)
	call zpt1(i_elem,pres,temp,zeta)
	ro=pres/(rr*temp*zeta)
	call calccp(i_elem,ro,temp,ctpc,ctvc)

	gamma=ctpc/ctvc
	Fgamma=gamma/1.4

	return
	end     !function Fgamma

c---------------------------------------------------------------------------
      real*4 function pmonitor (i_elem,q_input)
c***************************************************************************
c   calcola la pressione di valle della valvola monitor di un impianto di
c   regolazione;il valore del coefficiente di espansione y è fisso e pari a 
c   0.95 a scopo cautelativo
c*************************************************************************** 

      implicit none

	include '../inc/param.inc' 
	include '../inc/ti.inc' 
	include '../inc/tj.inc' 
	include '../inc/top_app_ele.inc' 
	include '../inc/cin_app_ele.inc' 
	include '../inc/rk.inc' 
      
      integer*4 i_elem   !indice impianto nella struttura dati completa
     *                   !1<i_elem<m_ogg

      real*4 q_input     !portata di ingresso alla valvola  monitor della singola linea


      real*4 fp_vg                           !è corretto usare questo fattore di 
	external fp_vg                         !piping(quello dell'impianto)? 

      real*4 ypsilon
	parameter (ypsilon=0.95)
      real*4 enne
	parameter (enne=27.3)  !per l'utilizzo di tale parametro vedere nota sopra

      real*4 deltap,denom,porttj_linea
      integer*4 im,iv
      real*4 rr 
     *      ,zeta
     *      ,ro 
     *      ,ro_mol_cn
     *      ,ro_cn
     *      ,pres,temp
     
      integer*4 i_vg     !indice impianto nella struttura dati ridotta
     *                   !1<i_vg<max_vg

	im=opumto(i_elem)
	iv=opuvto(i_elem)
	i_vg=pgrato(i_elem)

c*******************
c  Nel caso in cui la direzione della valvola venga violata, ai fini del calcolo
c  inverto le pressioni
c*******************

	if (prestj(im).lt.prestj(iv))then
	   pres=prestj(iv)
	   temp=temptj(iv)
	else
	   pres=prestj(im)
	   temp=temptj(im)
	end if

      rr=r_mks*romks/pmks/av_weight(i_elem)
	call zpt1(i_elem,pres,temp,zeta)
	ro=pres/(rr*temp*zeta)

      ro_mol_cn=(PCN*PMKS)/R_MKS/(TCN*TMKS)
      ro_cn = av_weight(i_elem) * ro_mol_cn / romks  !---> [kg]/[Nm3]

      denom=enne*cvmnt(i_vg)*fp_vg(i_elem)*ypsilon
c      porttj_linea=abs(porttj(i_elem))/line_vg_on(i_vg) !kNm3/h
      porttj_linea=q_input !kNm3/h
	!per usare correttamente la formula con N=27.3 converto la portata in [kg/h]
	porttj_linea=porttj_linea*1000*ro_cn

	deltap=((porttj_linea/denom)**2)/ro
	     
	pmonitor=pres-deltap

	if (pmonitor.lt.0) then
	   pmonitor=pres
	end if

	return

	end !pmonitor
