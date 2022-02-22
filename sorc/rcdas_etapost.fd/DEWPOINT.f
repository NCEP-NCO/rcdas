      SUBROUTINE DEWPOINT( VP, TD, IM,JM )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    DEWPOINT    COMPUTES DEWPOINTS FROM VAPOR PRESSURE
C   PRGMMR: J TUCCILLO       ORG: W/NP2       DATE: 90-05-19
C
C ABSTRACT: COMPUTES THE DEWPOINTS FOR THE N VALUES
C           OF VAPOR PRESSURE IN ARRAY VP.
C   .
C   .       THE FORMULA
C   .
C   .            VP = 0.611 * (X**A) * EXP( (A+B)*(1-X) )
C   .
C   IS USED TO GET DEWPOINT TEMPERATURE T, WHERE
C   .
C   X = T3/T,                   T3=TRIPLE PT TEMPERATURE,
C   VP=VAPOR PRESSURE IN CBS,   0.611=VP AT T3,
C   A=(SPEC. HT. OF WATER-CSUBP OF VAPOR)/GAS CONST OF VAPOR
C   .                        AND
C   B=LATENT HEAT AT T3/(GAS CONST OF VAPOR TIMES T3).
C   .
C   ON THE FIRST CALL, A TABLE  TDP  IS CONSTRUCTED GIVING
C   DEWPOINT AS A FUNCTION OF VAPOR PRESSURE.
C   .
C   VALUES OF VP LESS THAN THE FIRST TABLE ENTRY
C   (RVP1 IN THE CODE) WILL BE GIVEN DEWPOINTS FOR
C   THAT BEGINNING VALUE.  SIMILARLY , VP VALUES THAT
C   EXCEED THE MAXIMUM TABLE VALUE (RVP2 IN THE CODE)
C   WILL BE ASSIGNED DEWPOINTS FOR THAT MAXIMUM VALUE.
C   .
C   THE VALUES 0.02 AND 8.0 FOR RVP1 AND RVP2 YIELD
C   DEWPOINTS OF 233.6K AND 314.7K,RESPECTIVELY.
C   .
C
C PROGRAM HISTORY LOG:
C   90-05-19  J TUCCILLO
C   93-05-12  R TREADON - EXPANDED TABLE SIZE AND RESET
C                         RANGE OF PRESSURES COVERED BY
C                         TABLE.
C   98-06-12  T BLACK   - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION
C
C USAGE:  CALL DEWPOINT( VP, TD, N )
C   INPUT ARGUMENT LIST:
C     VP       - ARRAY OF N VAPOR PRESSURES(CENTIBARS)
C     IM,JM    - DIMENSIONS OF THE INPUT ARRAY VP.
C
C   OUTPUT ARGUMENT LIST:
C     TD       - DEWPOINT IN DEGREES ABSOLUTE
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY C-90
C
C$$$
       INCLUDE "CTLBLK.comm"
C
C          NT IS THE TABLE SIZE
      PARAMETER (NT=2000)
C...TRANSLATED BY FPP 3.00Z36 11/09/90  14:48:53  
C...SWITCHES: OPTON=I47,OPTOFF=VAE0
      DIMENSION TDP(NT),VP(IM,JM),TD(IM,JM)
CX      LOGICAL LFRST
CX      DATA LFRST / .TRUE. /
C
C
CX      IF ( LFRST ) THEN
C
CX      LFRST = .FALSE.
C          PREPARE THE TABLE (TDP=DEWPT AS FCN OF VAPOR PRESS).
C          RANGE IN CENTIBARS IS FROM RVP1 THRU RVP2
coff      RVP1=0.02E0
coff      RVP2=8.E0
      rvp1=0.0001E0
      rvp2=10.E0
C          THE TRIPLE POINT
      RT3=273.16E0
C          VAPOR PRESS AT THE TRIPLE POINT
      RVP3=0.611E0
      RLOG3=LOG(RVP3)
C          (SPEC HT OF WATER -CSUBP OF VAPOR)/GAS CONST OF VAPOR.
      RA=5.0065E0
C          LATENT HEAT AT T3/(GAS CONST OF VAPOR * TRIPLE PT TEMP).
      RB=19.83923E0
      RAPB=RA+RB
C          CRITERION FOR CONVERGENCE OF NEWTON ITERATION
      RTEST=1.E-6
CMEB  RTEST=1.E-8  !  PROBABLY WON'T CONVERGE WITH 32-BIT AT THIS CRITERION
C
      RNT=FLOAT(NT)
C          TABLE INCREMENT IN VAPOR PRESS
      RDVP=(RVP2-RVP1)/(RNT-1.E0)
C          RGS WILL BE THE GUESSED VALUE OF (T3  /  DEWPOINT)
      RGS=1.E0
      RVP=RVP1-RDVP
C
      DO 20 NN=1,NT
      RVP=RVP+RDVP
      RLVP=LOG(RVP)-RLOG3-RAPB
C     ***** ENTER NEWTON ITERATION LOOP
   10 RN=RA*LOG(RGS)-RAPB*RGS-RLVP
C          THAT WAS VALUE OF FUNCTION
C          NOW GET ITS DERIVATIVE
      RD=(RA/RGS)-RAPB
C          THE DESIRED CHANGE IN THE GUESS
      RCH=RN/RD
      IF( ABS(RCH) .LT. RTEST ) GO TO 15
C          NEED MORE ITERATIONS
      RGS=RGS-RCH
      GO TO 10
C          *****
C          HAVE ACCURATE ENUF VALUE OF RGS=T3/DEWPOINT.
   15 RT=RT3/RGS
      TDP(NN)=RT
C
   20 CONTINUE
C      PRINT 25,RVP1,RVP2,TDP(1),TDP(NT)
C  25  FORMAT(/'0', 'IN SUBROUTINE DEWPOINT, THE DEWPT TABLE ',
C    1             'HAS RVP1=', 1PE13.6, ', RVP2=', 1PE13.6,
C    2             ', TDP(1)=', 1PE13.6, ', AND TDP(NT)=',
C    3             1PE13.6, '.'/)
C           CONSTANTS FOR USING THE TABLE
      A=1./RDVP
      B=1.-A*RVP1
      DNTM1=FLOAT(NT) -.01
C
CX      END IF
C
C          *********** ENTER TO USE THE TABLE.  ************
C
      DO J=JSTA,JEND
      DO I=1,IM
        W1=AMIN1(AMAX1((A*VP(I,J)+B),1.0),DNTM1)
        W2=AINT(W1)
        JNT=INT(W2)
        TD(I,J) = TDP(JNT)+(W1-W2)*(TDP(JNT+1)-TDP(JNT))
      ENDDO
      ENDDO
C
C
      RETURN
      END
