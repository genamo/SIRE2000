!-----------------------------------------------------------------------
! Subroutine per il calcolo dei coefficienti della curva di interpolazione.
! Tale interpolazione può essere fatta mediante una curva che può essere: 
! step/lineare/quadratica 
!-----------------------------------------------------------------------
      Subroutine INTERP (ciclo,nint,type_int,qmed_input,modul,eps,a,b,c,
     *                   ier_interp)
!
!     ciclo     = lunghezza del ciclo (input)
!     nint      = numero di intervalli in cui è suddiviso il ciclo (input)
!     type_int  = tipo di interpolazione (input):
!			  = 1 step
!			  = 2 lineare
!			  = 3 quadratica
!    qmed_input = valore medio sul ciclo
!    modul(nint)= modulazione per ogni intervallo (input)
!    eps        = eps per test zero macchina
!    a(nint)	  = coefficiente del termine quadratico (output)
!    b(nint)	  = coefficiente del termine lineare (output)
!    c(nint)	  = termine noto (output)
!    ier_interp = indice di errore (output):
!				ier_interp =0       ->Calcolo OK      
!
!				ier_interp <0        errore sub gauss
!				ier_interp =1        errore sub matrice

      implicit none
!
      real*4 qmed_input,modul(*),ciclo,qsum,qmed,eps
      real*4 a(*),b(*),c(*)  !coefficienti di interpolazione
      integer*4 ier_interp,nint,type_int,ii
!------------------------------------------
!Per le interpolazioni a gradino e lineari non c'è indice di errore
!------------------------------------------
      ier_interp = 0 
!------------------------------------------
!Test portata input
      if(abs(qmed_input).le.eps)then
          do ii=1,nint
             a(ii) = 0.
             b(ii) = 0.
	       c(ii) = 0.
          enddo
          return
      endif
!------------------------------------------
!Calcolo somma dei modul
      qsum = 0.0
      do ii=1,nint
         qsum = qsum + modul(ii)
      enddo
!------------------------------------------
!Test somma coef.modul = 0 o non assegnati
      if(abs(qsum).le.eps)then
          do ii=1,nint
             a(ii) = 0.
             b(ii) = 0.
	       c(ii) = qmed_input
          enddo
          return
      endif
!------------------------------------------
!Trasformazione portata input
      qmed = qmed_input*ciclo
!------------------------------------------
      if    (type_int.eq.1)then   !interpolazione a gradino

           call interp_step(nint,qmed,qsum,modul,a,b,c)

      elseif(type_int.eq.2)then   !interpolazione lineare

!Trasformazione coef.modulazione in portata
           do ii=1,nint
              modul(ii) = (modul(ii)/qsum)*qmed
           enddo

           call interp_line(ciclo,nint,modul,a,b,c)

      elseif(type_int.eq.3)then   !interpolazione quadratica

!Trasformazione coef.modulazione in portata
           do ii=1,nint
              modul(ii) = (modul(ii)/qsum)*qmed
           enddo

           call interp_spli(ciclo,nint,qmed,modul,a,b,c,ier_interp)
      else
           ier_interp = 3
      endif
!------------------------------------------
      return
      end
!-----------------------------------------------------------------------
      subroutine interp_step(nint,qmed,qsum,modul,aa,bb,cc)
!
!     nint       = numero di intervalli in cui è suddiviso il ciclo (input)
!     qmed	   = valore medio sul ciclo (input)
!     qsum       = somma modulazioni (input)
!     modul(nint)= modulazione per ogni intervallo (input)
!     aa(nint)   = coefficiente del termine quadratico (output)
!     bb(nint)   = coefficiente del termine lineare (output)
!     cc(nint)   = termine noto (output)
!
!     Calcola i coefficienti di interpolazione a gradino.

      implicit none
      integer*4 nint               !n.ro intervalli      
      real*4 qmed,qsum,modul(*)
      real*4 aa(*),bb(*),cc(*)     !coefficienti di interpolazione
      integer*4 ii
!------------------------------------------
      do ii=1,nint
         aa(ii) = 0.
         bb(ii) = 0.
         cc(ii) =(modul(ii)/qsum)*qmed
      enddo

      return
      end
!-----------------------------------------------------------------------
      subroutine interp_line(ciclo,nint,modul,aa,bb,cc)
!
!     ciclo      = lunghezza del ciclo (input)
!     nint       = numero di intervalli in cui è suddiviso il ciclo (input)
!     modul(nint)= modulazione per ogni intervallo (input)
!     aa(nint)   = coefficiente del termine quadratico (output)
!     bb(nint)   = coefficiente del termine lineare (output)
!     cc(nint)   = termine noto (output)
!
!     Calcola i coefficienti di interpolazione lineare.

      implicit none
      integer*4 max_int
      parameter (max_int=50)
!
      real*4 modul(*),ciclo
      real*4 delta_t
      real*4 aa(*),bb(*),cc(*)    !coefficienti di interpolazione
      integer*4 nint              !n.ro intervalli
!
      integer*4 ii
      real*4 tvet(max_int+1)      !istanti
!------------------------------------------
      delta_t = ciclo/nint
      do ii=1,nint
        tvet(ii)=(ii-1)*delta_t
      enddo
      tvet(nint+1)=ciclo
!------------------------------------------
      do ii=1,nint-1
	   aa(ii) = 0.
	   bb(ii) = (modul(ii+1)-modul(ii))/delta_t
         cc(ii) = (modul(ii)*tvet(ii+1)-modul(ii+1)*tvet(ii))/
     *             delta_t
      enddo
	aa(nint) = 0.
	bb(nint) = (modul(1)-modul(nint))/(tvet(nint+1)-tvet(nint))
      cc(nint) = (modul(nint)*tvet(nint+1)-modul(1)*tvet(nint))/
     *           (tvet(nint+1)-tvet(nint))

      return
      end
!-----------------------------------------------------------------------
      subroutine interp_spli(ciclo,nint,qmed,modul,aa,bb,cc,ier)
!
!     ciclo      = lunghezza del ciclo (input)
!     nint       = numero di intervalli in cui è suddiviso il ciclo (input)
!     qmed	   = valore medio sul ciclo (input)
!     modul(nint)= modulazione per ogni intervallo (input)
!     aa(nint)   = coefficiente del termine quadratico (output)
!     bb(nint)   = coefficiente del termine lineare (output)
!     cc(nint)   = termine noto (output)
!     ier        = indice di errore
!
c       Calcola i coefficienti di interpolazione quadratica del tipo y=a*t**2+b*t+c

      implicit none
!     max_int  = numero massimo di intervalli
!     max_equ  = numero massimo di equazioni
!     max_ele  = numero massimo di elementi non nulli nella matrice del sistema
      integer*4 max_int,max_equ,max_ele
      integer*4 idmax
      parameter (max_int=25,max_equ=3*max_int,max_ele=max_int*max_equ,
     *           idmax=7*max_equ+2*max_ele)
!
      real*4 qmed,modul(*),ciclo
      real*4 delta_t
      real*4 aa(*),bb(*),cc(*)         !coefficienti di interpolazione
      integer*4 nint                   !n.ro intervalli
      integer*4 ier                    !indice di errore
!
      real*4 yy(max_equ)
      integer*4 ii,im(max_equ+1),jm(max_ele),iwk(idmax)
      integer*4 k_rango,el_rec,neq
      real*4 mm(max_ele),wk(idmax) 
      real*4 tvet(max_int+1)           !istanti
!------------------------------------------
      delta_t = ciclo/nint
      do ii=1,nint
        tvet(ii)=(ii-1)*delta_t
      enddo
      tvet(nint+1)= ciclo
!------------------------------------------
c-ace            definisco la matrice
      call matrice (nint,tvet,modul,im,jm,mm,max_ele,ier)
	if (ier.ne.0) return

      neq = nint*3
c
c                  calcolo del termine noto
      do ii=1,neq
         yy(ii) = 0.
      enddo
      yy(neq) = qmed

      call gauss (neq,yy,im,jm,mm,yy,wk,iwk,idmax,
     *            max_ele,k_rango,el_rec,ier)
      if(ier.ne.0) return

      if(k_rango.ne.neq) then
         call interp_line(ciclo,nint,modul,aa,bb,cc)
         return
      endif
!
      do ii=1,nint
         cc(ii) = yy(ii)
         bb(ii) = yy(ii+nint)
         aa(ii) = yy(ii+2*nint)
	enddo

      return
	end
!-----------------------------------------------------------------------
      Subroutine matrice(nint,tvet,modul,ia,ja,aa,idm,ier)
!
!    nint       = numero di intervalli in cui è suddiviso il ciclo (input)
!    tvet       = vettore dei tempi
!    modul(nint)= modulazione per ogni intervallo (input)
!    ia()	      = indice di riga (output)
!    ja()	      = indice di colonna (output)
!    aa()	      = elementi non nulli della matrice (output)
!    idm        = numero massimo di elementi non nulli della matrice(input)
!    ier        = indice di errore
!
      integer*4 ia(*),ja(*),nint
      REAL*4    modul(*),aa(*),tvet(*)
      integer*4 idm,ier
      integer*4 ieq,jacnt,i
      real*4 tn,tn2,tn3,tk,tk2,tk3
c---------------------------------------------------------------------
      ier=0
      ieq = 1
      ia(ieq)=1
      jacnt = 1
      do i=1,nint-1
        if(jacnt+6 .gt. idm)then
          ier =1
          return
        endif
!  Variabili dell'equazione (a(i)*t**2+b(i)*t+c(i)=a(i+1)*t**2+b(i+1)*t+c(i+1))
        ja(jacnt)     = i
        ja(jacnt + 1) = i+1
        ja(jacnt + 2) = i+nint
        ja(jacnt + 3) = (i+1)+nint
        ja(jacnt + 4) = i+2*nint
        ja(jacnt + 5) = (i+1)+2*nint
        aa(jacnt)     = 1.
        aa(jacnt + 1) = -1.
        aa(jacnt + 2) = tvet(i+1)
        aa(jacnt + 3) = -tvet(i+1)
        aa(jacnt + 4) = tvet(i+1)**2
        aa(jacnt + 5) = -tvet(i+1)**2

        jacnt = jacnt + 6
        ieq = ieq + 1
        ia(ieq) = jacnt
      enddo
!
      if(jacnt+6 .gt. idm)then
        ier =1
        return
      endif
!
!  Variabili dell'equazione per l'ultimo intervallo
      ja(jacnt)     = 1
      ja(jacnt + 1) = nint
      ja(jacnt + 2) = 1+nint
      ja(jacnt + 3) = 2*nint
      ja(jacnt + 4) = 1+2*nint
      ja(jacnt + 5) = 3*nint
      aa(jacnt)     = 1.
      aa(jacnt + 1) = -1.
      aa(jacnt + 2) = tvet(1)
      aa(jacnt + 3) = -tvet(nint+1)
      aa(jacnt + 4) = tvet(1)**2
      aa(jacnt + 5) = -tvet(nint+1)**2
!
      jacnt = jacnt + 6
      ieq = ieq + 1
      ia(ieq) = jacnt
!
      do i=1,nint-1
        if(jacnt+4 .gt. idm)then
          ier =1
          return
        endif
!  Variabili dell'equazione 2*a(i)*t+b(i)=2*a(i+1)*t+b(i+1)
        ja(jacnt)     = i+nint
        ja(jacnt + 1) = (i+1)+nint
        ja(jacnt + 2) = i+2*nint
        ja(jacnt + 3) = (i+1)+2*nint
        aa(jacnt)     = 1.
        aa(jacnt + 1) = -1.
        aa(jacnt + 2) = 2*tvet(i+1)
        aa(jacnt + 3) = -2*tvet(i+1)
! 
        jacnt = jacnt + 4
        ieq = ieq + 1
        ia(ieq) = jacnt
      enddo
c
      if(jacnt+4 .gt. idm)then
        ier =1
        return
      endif      
!  Variabili dell'equazione per l'ultimo intervallo
      ja(jacnt)     = 1+nint
      ja(jacnt + 1) = 2*nint
      ja(jacnt + 2) = 1+2*nint
      ja(jacnt + 3) = 3*nint
      aa(jacnt)     = 1.
      aa(jacnt + 1) = -1.
      aa(jacnt + 2) = 2*tvet(1)
      aa(jacnt + 3) = -2*tvet(nint+1)
!
      jacnt = jacnt + 4
      ieq = ieq + 1
      ia(ieq) = jacnt
!
      tn  = tvet(nint+1)-tvet(nint)
      tn2 = (tvet(nint+1)**2-tvet(nint)**2)/2.
      tn3 = (tvet(nint+1)**3-tvet(nint)**3)/3.
      do i=1,nint-1
        if(jacnt+6 .gt. idm) then
          ier =1
          return
        endif 
        tk  = tvet(i+1)-tvet(i)
        tk2 = (tvet(i+1)**2-tvet(i)**2)/2.
        tk3 = (tvet(i+1)**3-tvet(i)**3)/3.           
!  Variabili dell'equazione 
        ja(jacnt)     = i
        ja(jacnt + 1) = nint
        ja(jacnt + 2) = i+nint
        ja(jacnt + 3) = 2*nint
        ja(jacnt + 4) = i+2*nint
        ja(jacnt + 5) = 3*nint
        aa(jacnt)     = modul(nint)*tk
        aa(jacnt + 1) = -modul(i)*tn
        aa(jacnt + 2) = modul(nint)*tk2
        aa(jacnt + 3) = -modul(i)*tn2
        aa(jacnt + 4) = modul(nint)*tk3
        aa(jacnt + 5) = -modul(i)*tn3
        jacnt = jacnt + 6
        ieq = ieq + 1
        ia(ieq) = jacnt
      end do
      do i=1,nint
        if(jacnt+nint*3 .gt. idm) then
          ier =1
          return
        endif
        tk  = tvet(i+1)-tvet(i)
        tk2 = (tvet(i+1)**2-tvet(i)**2)/2.
        tk3 = (tvet(i+1)**3-tvet(i)**3)/3.     
        j = i-1
!  Variabili dell'dell'ultima equazione
        ja(jacnt +j )        = i
        ja(jacnt +j+nint)    = i+nint
        ja(jacnt +j+2*nint)  = i+2*nint
        aa(jacnt +j)         = tk
        aa(jacnt + j+nint)   = tk2
        aa(jacnt + j+2*nint) = tk3
      enddo
      jacnt = jacnt + 3*nint
      ieq = ieq + 1
      ia(ieq) = jacnt
!
      return
      end
!-----------------------------------------------------------------------