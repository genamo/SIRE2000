
      INCLUDE 'otc_par.inc'     ! dimensioni e parametri

cgpe      INTEGER*4 tab_grid(nhmax,nqmax)
cgpe     *         ,dhs,dqs

      INTEGER*4 ind_opt       ! O) indice stato per la configurazione ottima
      INTEGER*4 ind_s(max_ch,maxunits)
cgpe      INTEGER*4 ind_u(max_serv,maxunits)
      INTEGER*4 ind_u(maxunits)

cgpe      COMMON /ARGO/ tab_grid,dhs,dqs
      COMMON /ARGO/ ind_opt,ind_s,ind_u
