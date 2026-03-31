      integer  ferror,ftopol,fperfo,ftele,fconst,fout,fwarn,
     *     fresult,fmessag,fevent,foutd,dim_file,
     *     fsave,flist,ffield,frunit,flink,fgks,
     *     fmess,irunit,itopol, iname, iperfo,fcontem,
     $     iresult, iconst, isecond, iminc,
     $     fnouf,  !-SM-tag3 indica: Non possibile ufficializzare topologia
     $     fflag,  !gz indica file che fa da flag
     $     frete,                !ff File memorizzazione rete attuale
     $     igrete,               !ff File con informazioni grafiche sulla rete
     $     ftem,                 !ff Indice per aprire file temporanei
     $     fqdet                 !ff File info quadranti di dettaglio grafica
      PARAMETER (fout=6,       ftopol=11,      fperfo=12,
     *     ftele=13,     fconst=14,      fresult=15,
     *     fwarn=16,     ferror=17,      fmessag=18,
     *     fevent=19,    fsave=27,       flist = 26,
     *     frunit=28,    ffield=29,      flink=5,
     $     frete=34,     igrete=35,      ftem=20,
     $     fqdet=36,     fnouf=69,       fflag=77,
     *     itopol=21,    iname=22,       iperfo=23, 
     *     iresult=24,   iconst=25,      foutd=31, 
     *     fgks=78 ,     fmess=68,       iminc=30,
     *     irunit=32,    isecond=33,     fcontem=37)
c     
C---->  massime dimensioni dei vettori(vertici,segmenti,ecc..)
      INTEGER max_vert, max_compr, max_turb,
     $     maxelem, maxunits, max_lim,
     $     max_compo, maxcurve, max_point,
     $     max_stat, max_stc, max_stt,
     $     max_arcc, max_arct,numgrand,
     $     maxvalve, max_tri, max_env,
     $     max_accp,max_pacco,max_pcurv,max_valve_or_and,
     $     max_prel_indir
c
      PARAMETER (max_vert=15,    max_compr=25,  max_turb =25,   
     *     maxelem=80,    maxunits=12,   max_lim=20,
     *     max_compo=10,  maxcurve=30,  max_point=50,
     *     max_stat=15,   max_stc=12,    max_stt=12,
     *     max_arcc=200,   max_arct=100, numgrand=3,    
     *     maxvalve=5,    max_tri=10,    max_env=25,
     *     max_valve_or_and=5, !max valve in .mis
     *    MAX_ACCP=20,   ! numero max di accoppiamenti
     *    max_Pacco=5,   ! numero max di curve di potenza per singolo accoppiam
     *     max_prel_indir=10) !max prelievi sommabili come indirizzi in .mis
C---->  dimensioni derivate
      INTEGER max_unit, maxlink,maxpcurv,
     $     max_seg, max_type,
     $     max_el, max_valve, max_acc_tot
      PARAMETER  (max_unit=maxunits*max_vert, maxlink=max_unit, 
     *     max_seg=max_vert-1, max_type=max_unit,
     *     max_el=max_seg*maxelem,
     *     max_pcurv=MAX_ACCP*MAX_PACCO,
     *     max_valve=maxvalve*max_seg,
     *     max_acc_tot=max_accp+max_arcc+max_arct)
c
      INTEGER max_recu, nmaxrec,lrmaxrec,lumaxrec,lmaxrec,
     *  lvmaxrec,lsmaxrec
      PARAMETER (max_recu=max_arcc+max_arct+7,
     $     nmaxrec=2*max_seg+2*max_vert+11)
c
c   cambiare la dimensione dei record e' operazione delicatissima!!!!
c ---> expected points data base
	PARAMETER (lrmaxrec=1000) 
c ---> units coefficient data base
	PARAMETER (lumaxrec=1000) 
c ---> topology
c->max
	PARAMETER (lmaxrec=2048) 
c ---> constraint
c max	PARAMETER (lvmaxrec=492) 
	PARAMETER (lvmaxrec=1100) 
c ---> simulation
c max	PARAMETER (lsmaxrec=220) 
	PARAMETER (lsmaxrec=420) 
