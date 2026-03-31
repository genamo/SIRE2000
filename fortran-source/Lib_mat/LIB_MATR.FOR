!======================================================================
C__________________________________________________________________
	SUBROUTINE MVMRS(N,first_A,A_entry,A_value,F,G)
C__________________________________________________________________
C
C	QUESTA SUBROUTINE MOLTIPLICA UNA MATRICE A, MEMORIZZATA
C	IN FORMA SPARSA, PER UN VETTORE F; IL RISULTATO E'
C	MEMORIZZATO NEL VETTORE G.
C__________________________________________________________________
        implicit none
	REAL*4   A_value(1),F(1),G(1)
	integer*4  first_A(1),A_entry(1)
	integer*4  j,n,il,iu,i,ii
C
        DO J=1,N      
	   G(j) = 0.
	   IL=first_A(J)
	   IU=first_A(J+1)-1
           DO I=IL,IU
	      II=A_entry(I)
	      G(j) = G(j)+A_value(I)*F(II)
	   END DO
        END DO
	RETURN
	END
c-------------------------------------------------------------
	SUBROUTINE TRSMAT(M,N,first_A,A_entry,A_value,
     *				first_AT,AT_entry,AT_value,
     *				first_AUX)
C---------------------------------------------------------------
C
C	TRASPOSTA DELLA MATRICE A(M*N) MEMORIZZATA IN FORMA
C	SPARSA. 
c	LA MATRICE TRASPOSTA E' MEMORIZZATA IN FORMA SPARSA
C---------------------------------------------------------------
        implicit none
	REAL*4   A_value(1),AT_value(1)
	integer*4  first_A(1),A_entry(1),first_AT(1),AT_entry(1),
     *			first_AUX(1)
	integer*4  i,m,ntot,n,j,jc,jc1
C---------------------------------------------------------------

	do i=1,M+1
	  first_AUX(i) = first_A(i)
	end do
	ntot = 1
	do i=1,N
	  first_AT(i) = ntot
	  do 10 j=1,M
	    jc = first_AUX(j)
	    if(A_entry(jc).eq.i.and.jc.lt.first_A(j+1)) then
	      AT_entry(ntot) = j
	      AT_value(ntot) = A_value(jc)
	      ntot = ntot+1
	      jc1 = jc+1
	      if(jc1.lt.first_AUX(j+1)) first_AUX(j) = jc1
	    end if
  10	  continue
	end do
	first_AT(N+1) = ntot

C La parte seguente deve prendere il posto della parte superiore se
C      la matrice non e' ordinata

c	ntot = 1
c	do i=1,N
c         first_AT(i) = ntot
c	  do  j=1,M
c            do k=first_a(j),first_a(j+1)-1
c              if (a_entry(k).eq.i) then
c                  at_entry(ntot)=j
c                  at_value(ntot)=a_value(k)
c                  ntot=ntot+1
c              endif
c            enddo
c          enddo
c	end do
c	first_AT(N+1) = ntot

	return
	end

c-----------------------------------------------------------------------------
       subroutine ordinamat_rs(nra,a_first,a_entry,a_value)

c Ordina le righe della matrice in forma sparsa A
       implicit none

       integer*4  nra                       ! I Numero righe
       integer*4  a_first(*),a_entry(*)     ! I/O Matrice in forma sparsa
       real*4    a_value(*)                 ! I: Disordinata  O: Ordinata

       integer*4 ir,ic,iic,ici,icf
       integer*4 i_app
       real*4 r_app

        do ir = 1,nra
          ici = a_first(ir)
          icf = a_first(ir+1)-1
           do ic = ici,icf
             do iic = ic+1,icf
               if (a_entry(iic).lt.a_entry(ic)) then
                  i_app = a_entry(ic)
                  a_entry(ic) = a_entry(iic)
                  a_entry(iic) = i_app
                  r_app = a_value(ic)
                  a_value(ic) = a_value(iic)
                  a_value(iic) = r_app
               endif
             enddo
           enddo
        enddo
      
       return
       end
c-------------------------------------------------------------
c-----------------------------------------------------------------------------
       subroutine mtramprod_rs(m_m,n_n,m_first,m_entry,m_value,
     *          mt_first,mt_entry,mt_value,mr_first,mr_entry,mr_value)

C Data la matrice M [m_m x n_n]
C  calcola la matrice trasposta MT [n_n x m_m]
c esegue il prodotto fra matrici 
c              MT*M
c e mette il risultato nella matrice MR [n_n x n_n].
c
c-sfrutta simmetria assegnando il valore calcolato per il 
c- triangolo sup al triangolo inf.
C N.B.: Tutte le matrice sono espresse in forma sparsa.
c NOTA: il termine diagonale viene memorizzato anche se nullo.

       implicit none
       integer*4  m_m,n_n,m_first(1),m_entry(1)
       real*4  m_value(1)
       integer*4  mt_first(1),mt_entry(1)
       real*4  mt_value(1)
       integer*4  mr_first(1),mr_entry(1)
       real*4  mr_value(1)

       integer*4 iec, irc, icc, mr_i, mr_f
       integer*4 mt_i, mt_f, m_i, m_f
       integer*4 ind, jnd

c--------------------------
C Calcola la matrice trasposta
       CALL TRSMAT(m_m,n_n,m_first,m_entry,m_value,
     *			mt_first,mt_entry,mt_value,mr_first)
       iec=1
       mr_first(1)=iec
       mr_value(iec)=0

       do irc=1,n_n

c---- riporto la parte superiore gia' calcolata nella parte inferiore
c----       ( Sfrutto simmetria)
         do icc=1,irc-1
           mr_i = mr_first(icc)
           mr_f = mr_first(icc+1)-1
           do ind=mr_f,mr_i,-1
              if(mr_entry(ind).eq.irc) then
                 mr_value(iec) = mr_value(ind)
                 mr_entry(iec) = icc
                 iec=iec+1                  
                 mr_value(iec) = 0
                 goto 200
              endif
c---- La riga seguente deve essere eliminata se la matrice non e' ordinata
              if(mr_entry(ind).lt.irc) goto 200
c------------------------------------------------------------------
           enddo
200      enddo


         mt_i = mt_first(irc)
         mt_f = mt_first(irc+1)-1
         do icc=irc,n_n
C---- Calcolo il valore della colonna icc nella riga irc
C---- Si ricorda che la matrice MR e' quadrata [n_n x n_n]
          do ind=mt_i,mt_f
             m_i=m_first(mt_entry(ind))
             m_f=m_first(mt_entry(ind)+1)-1
             do jnd=m_f,m_i,-1
                if(m_entry(jnd).eq.icc) then
                  mr_value(iec)=mr_value(iec) + 
     -                    mt_value(ind) * m_value(jnd) 
                  goto 100
                endif
c---- La riga seguente deve essere eliminata se la matrice non e' ordinata
                if(m_entry(jnd).lt.icc) goto 100
c------------------------------------------------------------------
             enddo
100       enddo
          if(mr_value(iec).ne.0 .or. irc.eq.icc) then
c-- Setto l'entry della colonna
             mr_entry(iec)=icc
             iec=iec+1
             mr_value(iec)=0
          endif  
         enddo !icc
C-- Setto il first della riga
         mr_first(irc+1)=iec
       enddo ! irc
       return
       end
c-------------------------------------------------------------
