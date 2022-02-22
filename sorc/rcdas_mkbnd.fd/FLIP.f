C-------------------------------------------------------------------
      SUBROUTINE FLIP(AE,KD)
C
C SUBPROGRAM: FLIP          REVERSE THE VERTICAL INDEXING OF ARRAY AE
C   PRGMMR: T.BLACK         ORG: W/NMC22    DATE: ??-??-??
C
C ABSTRACT: REVERSES THE VERTICAL INDEXING OF ARRAY AE, WHICH IS REVERSE
C           FROM VERICAL INDEXING IN THE ETA MODEL
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T.BLACK
C
C USAGE     CALL FLIP(AE,KD)
C   INPUT ARGUMENT LIST:
C     AE      -INPUT ARRAY
C     KD      -NUMBER OF VERTICAL LEVELS
C
C   OUTPUT ARGUMENT LIST:
C     AE      -FLIPPED OUTPUT ARRAY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
        INCLUDE "parmlbc"
C
      DIMENSION AE(KB,KD),TEMP(KB)
C************************************************************************
      KDH=KD/2
      DO 100 KN=1,KDH
      K=KD+1-KN
      DO 50 LB=1,KB
      TEMP(LB)=AE(LB,KN)
      AE(LB,KN)=AE(LB,K)
      AE(LB,K)=TEMP(LB)
   50 CONTINUE
  100 CONTINUE
      RETURN
      END