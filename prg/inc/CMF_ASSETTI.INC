*----------------------------------------------------------------------
c   Informazioni relative agli assetti (servizi) definiti in input
c   per una centrale MF
*----------------------------------------------------------------------
      integer*4    num_servizi      ! numero di servizi dati in input
      integer*4    numrig_ass_mf    ! contatore assetti

      common/cmf_serv/num_servizi,numrig_ass_mf(MAX_SERV)

*----------------------------------------------------------------------
c   Informazioni relative agli assetti (servizi) definiti per una
c   centrale MF
*----------------------------------------------------------------------
      INTEGER*4   i_serv
     *           ,i_staz
     *           ,i_vert
     *           ,ind_serv
cgpe     *           ,num_ass_mf   ! indice degli assetti scelti dall'ottimo
cgpe      logical*2   l_mf
      logical*2   l_act
     *           ,lmf
     *           ,l_simu

      common/cemf/i_serv(max_vert)
     *           ,i_staz(max_serv)
     *           ,i_vert(max_vert)
     *           ,ind_serv
cgpe     *           ,num_ass_mf(max_serv)
cgpe     *           ,l_mf(max_vert)
     *           ,l_act(max_unit)
     *           ,lmf
     *           ,l_simu(max_serv)
