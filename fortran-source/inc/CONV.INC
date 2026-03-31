*** CONV.INC **** FATTORI DI CONVERSIONE ******************************

      integer*4 CONV_QM     ! fattore di conversione per la portata di
                            ! macchina (da m3/h a Km3/h)
     *         ,CONV_H      ! fattore di conversione per la prevalenza
                            ! (da m a Km)

      parameter( CONV_QM = 1000
     *          ,CONV_H = 1000
     *                      )


* Gestione unità di misura
      integer*4 udm_T
     *         ,udm_P
     *         ,udm_PW
     *         ,udm_H
     *         ,udm_Q
     *         ,udm_QM
     *         ,udm_NG_perc
     *         ,udm_HR
     *         ,udm_V
     *         ,udm_coeff
     *         ,udm_pcs
     *         ,udm_lun

      integer*4 udm_dim   !!!!! dimensioni dei vettori

      parameter( udm_T = 1					!temperatura
     *          ,udm_P = udm_T + 1			!pressione
     *          ,udm_PW = udm_P + 1			!potenza
     *          ,udm_H = udm_PW + 1			!prevalenza
     *          ,udm_Q = udm_H + 1			!portata volumetrica in Condizioni Normali
     *          ,udm_QM = udm_Q + 1			!portata volumetrica in Condizioni Macchina
     *          ,udm_NG_perc = udm_QM + 1	!numero giri in percentuale
     *          ,udm_HR = udm_NG_perc + 1	!heat rate
     *          ,udm_V = udm_HR + 1	        !velocità
     *          ,udm_coeff =udm_V + 1	    !coefficienti
     *          ,udm_pcs =udm_coeff + 1     !pcs
     *          ,udm_lun =udm_pcs + 1)      !lunghezza

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      parameter( udm_dim = udm_lun)
!!!!!!!!  deve essere sempre uguale all'ultima udm sopra definita  !!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C  per le grandezze che nella parte modellistica usano un udm differente dall'udm DB
	  real*4    udm_fact_md  !fattore moltiplicativo per riportare la grandezza all'udm DB
     *         ,udm_plus_md  !fattore additivo per riportare la grandezza all'udm DB
* Valori su Data Base
      integer*4 udm_grd_id_db   !identificativo della grandezza nel DB
     *         ,udm_grd_prec_db !num. decimali per l'udm di output
      real*4    udm_fact_db !fact. moltipl. per riportare le grandezze all'udm di output
     *         ,udm_plus_db !fact. additivo per riportare le grandezze all'udm di output
      character*(30) udm_sgl_db !sigla per l'udm di output 

      common/udm_area/
     -       udm_fact_md (udm_dim) 
     -      ,udm_plus_md (udm_dim) 
     -      ,udm_grd_id_db (udm_dim) 
     -      ,udm_grd_prec_db (udm_dim) 
     -      ,udm_fact_db (udm_dim) 
     -      ,udm_plus_db (udm_dim) 
     -      ,udm_sgl_db (udm_dim) 


*** CONV.INC **** FATTORI DI CONVERSIONE ******************************
