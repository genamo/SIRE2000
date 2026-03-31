
cgpe      subroutine real_maxpower(pwmax)
      subroutine real_maxpower(itip,pwmax)
c************************************************************************************
c  Questa subroutine confronta la massima potenza erogabile dalla turbina
c  calcolata dalla subroutine 'pwdan' con la massima potenza che può essere 
c  effettivamente erogata tenendo conto del vincolo operativo imposto dall'utente.
c  Se tale valore viene superato si esegue un taglio e si impone che la pwmax sia
c  quella vincolata.Per tenere traccia di tale operazione si assegna il valore
c  .true. al flag type_maxpower
c************************************************************************************
      implicit none
c
      include '../inc/param.inc'
      include '../inc/stazione.inc'
      include '../inc/simpstat.inc'
c------------------------------------>INPUT/OUTPUT
      integer*4 itip   !indice del tipo, sostituisce il vecchio itipo in common
      real*4    pwmax  !massima potenza della turbina [KWatt]
c------------------------------------>Variabili locali
      real*4 vinc_pow
c************************************************************************************
c 
      vinc_pow = type_vinc_maxpow(itip)
	if (vinc_pow.gt.0) then
        if (pwmax .gt. vinc_pow) then
          pwmax = vinc_pow
	    type_flag_maxpow(itip) = .true.
	  endif
	end if
c
      return
	end
c-----------------------------------------------------------------------------------
cgpe-mess      subroutine real_minpower(itip,pwc)
cgpe-messc************************************************************************************
cgpe-messc  Questa subroutine confronta la potenza richiesta dal compressore con la minima
cgpe-messc  potenza erogabile dalla turbina assegnata dall'utente.
cgpe-messc  Se la potenza richiesta è inferiore al minimo erogabile si lancia un warning che 
cgpe-messc  attualmente non blocca la simulazione
cgpe-messc************************************************************************************
cgpe-mess      implicit none
cgpe-messc
cgpe-mess      include '../inc/param.inc'
cgpe-mess      include '../inc/stazione.inc'
cgpe-mess      include '../inc/simpstat.inc'
cgpe-mess      include '../inc/flag.inc'
cgpe-messc------------------------------------>INPUT
cgpe-mess      integer*4 itip   !indice del tipo, sostituisce lil vecchio itipo in common
cgpe-mess      real*4    pwc    !potenza richiesta dal compressore [KWatt]
cgpe-messc------------------------------------>Variabili locali
cgpe-mess      real*4       min_pow
cgpe-mess	integer*4    ier
cgpe-mess      character*(max_len) mess
cgpe-messc************************************************************************************
cgpe-mess      min_pow = type_vinc_minpow(itip)
cgpe-mess      if (pwc.lt.min_pow) then
cgpe-mess	   type_flag_minpow(itip)=.true.
cgpe-mess	   ier=81
cgpe
cgpe-mess         if (iopt .or. iopt_mess) return
cgpe-mess         if (fl_sim) then
cgpe-mess           call gest_error (2,0,'',
cgpe-mess     *         'Non viene rispettata la minima potenza della turbina',0)
cgpe-mess            write(mess,568) itip
cgpe-mess568   format(' Turbogruppo vincolato:',i2)
cgpe-mess           call gest_error (2,0,'',mess,0)

cgpe-mess           write (mess,570) pwc,min_pow,UDM_INT_PW ! write (mess,570) pwc,min_pow
cgpe-mess570   format('P richiesta: ',f7.0,' - P min: ',f7.0,'  ',A4)
cgpe-mess           call app_mess(ver_poi,ier,mess)
cgpe-mess         endif

cgpe-mess	end if
c
cgpe-mess      return
cgpe-mess	end
c-----------------------------------------------------------------------------------
