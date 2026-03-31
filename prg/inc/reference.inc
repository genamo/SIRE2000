c......................................................................
        real QRIF,QMKS,P0,PRIF,PMKS,APRIF,DPRIF,
     *       TRIF,T0,TMKS,LRIF,LMKS,DRIF,DMKS,ROURIF,ROUMKS,
     *       ALTRIF,ALTMKS,HCOEFFRIF,HCOEFFMKS,HEADRIF,HEADMKS,
     *       QMRIF,QMMKS,POWERIF,POWERMKS,HRATERIF,HRATEMKS,
     *       AMOUNTRIF,AMOUNTMKS,CPRIF,CPMKS,VISRIF,VISMKS,
     *       HEAT_VALUEMKS,PCN,TCN,G,R,PI,percrif,undef,taux
c
c---->  "Grandezze di riferimento e coefficienti per la trasformazione 
c        in unita' M.K.S."
c
c
	PARAMETER (QRIF= 1000.,      QMKS  = 1000./3600.,
     *	           PRIF= 48.987,P0  = 1.013,PMKS  = 1.E05,
     *	           APRIF=50.,DPRIF= 500.,
     *	           TRIF= 15.,T0  = 273.15,TMKS = 1., 
     *				LRIF= 100.,       LMKS  = 1000., 
     *		   DRIF= 1.,         DMKS  = 1.,
     *		 ROURIF= 1000.,     ROUMKS  = 0.001,
     *	     ALTRIF= 100.,       ALTMKS= 1.,  
     *	      HCOEFFRIF=1.,       HCOEFFMKS= 1.,
     *        HEADRIF=10.,       HEADMKS= 1000.,      
     *        QMRIF=10.,         QMMKS= 1000./3600.,
     *		POWERIF=1.E4,      POWERMKS= 1000.,
     *	       HRATERIF=1.E4,      HRATEMKS= 1./3600.,
     *	      AMOUNTRIF=1.,        AMOUNTMKS= 1.E6,
     *          percrif=100.,undef=-999.,taux=trif+t0)
c
c---->  "Proprieta' del gas e coefficienti per la trasformazione
c	 in unita' M.K.S." 
	PARAMETER(CPRIF = 2.16E3,          CPMKS = 1.,
     *	 	  VISRIF      = 1.1E-5,     VISMKS       =1.,
     *			                HEAT_VALUEMKS=1000.)
c
c---->  "Condizioni normali"
	PARAMETER ( PCN = 1.01325,  
     *		    TCN = 0.)
c---->  "Costanti"   (in unita' M.K.S.)
	PARAMETER (G = 9.80665,
     *		   R = 8314.,
     *		   PI= 3.14159) 

      integer max_np
       parameter (max_np=50)  !massimo numero di punti che si possono assegnare
c
      integer max_neff
       parameter (max_neff=10)  !massimo numero di curve a efficienza costante
c
      integer max_ngiri
       parameter (max_ngiri=12)  !massimo numero di curve a numero di giri costanti
c
      integer max_nlim
       parameter (max_nlim=20)  !massimo numero di punti limite
c
      integer max_nhr
       parameter (max_nhr=10)  !massimo numero di punti limite
c


