c       replacement for ibm routine
c       does a transpose of the matrix
     
      subroutine SGETMO (a, lda, m, n, b, ldb)
      real(4) b(ldb,m),a(lda,n)
      integer lda,m,n,ldb
      do j=1,n
       do i=1,m
          b(j,i)=a(i,j)
       enddo
      enddo
      return
      end
