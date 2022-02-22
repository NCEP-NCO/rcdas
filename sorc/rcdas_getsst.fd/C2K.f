C-CP
      SUBROUTINE C2K(M,N,A)
C-CP
C-CP CONVERTS AN ARRAY OF TEMPERATURES FROM CENTIGRADE TO KELVIN
C-CP
c     INCLUDE "mpif.h"
c     INCLUDE "MPI_PARM.comm"
C
      DIMENSION A(M,N)
C
      IMAX=M
      JMAX=N
C
      DO 50 J =1,JMAX
      DO 50 I =1,IMAX
      A(I,J) = A(I,J) + 273.16
   50 CONTINUE
C
      RETURN
      END
