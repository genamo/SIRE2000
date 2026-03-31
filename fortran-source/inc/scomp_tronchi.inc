!-----------------------------------------------------------------------
!-Area memoria per Scomposizione tronchi con lunghezza > max (50 km.)
!-----------------------------------------------------------------------
	  logical*2 tr_flag_scomp      !flag presenza scomposizione (false/true -> no/si)
!
      integer*4 tr_ori_etipti      !originale etipti tronchi
      integer*4 pu_ori_etipti      !originale etipti punti
      integer*4 tr_ori_pmo(max_tr) !originale punto monte tronchi
      real*4    tr_ori_lun(max_tr) !originale lunghezza tronchi
!
      integer*4 tr_new_etipti      !nuova etipti tronchi in scomposizione (con fittizi)
      integer*4 pu_new_etipti      !nuova etipti punti in scomposizione   (con fittizi)
      integer*4 tr_new_pmo(max_tr) !nuovo punto monte tronchi originali scomposti
      real*4    tr_new_lun(max_tr) !nuova lunghezza tronchi originali scomposti
!
      common/tr_scomp0/tr_flag_scomp

      common/tr_scomp1/tr_ori_etipti,pu_ori_etipti,
     -                 tr_ori_pmo,tr_ori_lun

      common/tr_scomp2/tr_new_etipti,pu_new_etipti,
     -                 tr_new_pmo,tr_new_lun
!-----------------------------------------------------------------------
