
c Il parametro max_len e' stato spostato in stazione.inc
c      INTEGER*4  max_len        ! lunghezza massima dei messaggi
c      parameter( max_len =  80)
c*     parameter( max_len = 132) ! L.R. 02/09/1999

      INTEGER*4  max_rig_mess
      parameter (max_rig_mess = 5)

      INTEGER*4           mess_num, mess_type
      character*(max_len) mess_stri

      INTEGER*4           num_rig_mess
      character*(max_len) mess_descr
c
      common/STMESS/mess_num(max_vert), mess_type(mess_max,max_vert)
     *             ,mess_stri(mess_max,max_vert)
     *             ,num_rig_mess(mess_max)
     *             ,mess_descr(max_rig_mess,mess_max)


      INTEGER*4  err
      common/ERR/err(MAX_VERT)
