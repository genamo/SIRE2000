*----------------------------------------------------------------------
c parametri e common per l'utilizzo della Redlich-Kwong
*----------------------------------------------------------------------
c
      real      R_RK     ! costante universale dei gas
      parameter (R_RK= 8314.)  ! da usare con: P in Pascal
                               !               T in Kelvin
                               !               M in g/mole
                               !               V in litri
      real      R_MKS          ! costante universale dei gas in MKS
      parameter (R_MKS= 8.314)  ! in joule / K / mole
*----------------------------------------------------------------------
c adimensionalizzazioni eq. gas
*----------------------------------------------------------------------
      real      prif,pmks,aprif,trif,tmks,t0,p0,cprif,cpmks,
     *          visrif,vismks,romks
c
!      PARAMETER (PRIF= 48.987,     P0  = 1.013,      PMKS  = 1.E05,
      PARAMETER (PRIF= 50.,         P0  = 0.,         PMKS  = 1.E05,
     *           APRIF=50.,
     *           romks=1000.,
     *           TRIF= 15.,        T0  = 273.15,  TMKS = 1.,
     *           CPRIF = 2.16E3,   CPMKS = 1.,
     *           VISRIF= 1.1E-5,   VISMKS =1.)
c
      real volume_molare
      parameter (volume_molare = 22.414)    ! volume molare (litri/mole)
*----------------------------------------------------------------------
c condizioni normali
*----------------------------------------------------------------------
      real pcn,tcn
!      parameter (PCN = 1., tcn=273.15)
      parameter (PCN = 1.01325, tcn=273.15)


*----------------------------------------------------------------------
c costanti del gas
*----------------------------------------------------------------------
        real*4   erre                ! costante universale dei gas
                                     ! in J / mole K
        parameter (erre = R_MKS)



*----------------------------------------------------------------------
c---->  "Grandezze di riferimento e coefficienti per la trasformazione
c        in unita' M.K.S."
*----------------------------------------------------------------------
c
        real*4 qrif
!     *      ,trif,t0
     *      ,headrif,qmrif,powerif,hraterif,revrif
     *      ,T0S,CONV_NS,CONV_SN
c
        PARAMETER (QRIF= 1000.,
c     *             TRIF= 15.,      T0  = 273.15,
     *             HEADRIF=10.,    QMRIF=10.,      POWERIF=1.E4,
     *             HRATERIF=1.E4,  REVRIF=100.,    T0S = 288.15,
     *             CONV_NS = T0S/T0, CONV_SN = T0/T0S)

c-elena
      real*4 ckWh    !fattore per conversione da kWh a kJ (kJ=3660kWh)

      parameter (ckWh=3600.)
c-elena
*-----------------------------------------------------------------------
c   Grandezze ausiliarie usate nelle adimensionalizzazioni
*-----------------------------------------------------------------------
      real*4 percrif,undef,taux

      parameter(percrif=100.,undef=-999.,taux=trif+t0)
*------------------------------------------------------------------------
c   Massimi per graficazione
*--------------------------------------------------------------------------
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
       parameter (max_nhr=10)  !massimo numero di curve ad heat rate costante
c
*L.R.
      REAL*4 F100
        PARAMETER (F100  = 100.0)
