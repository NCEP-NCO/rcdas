      SUBROUTINE MIXLEN
     I (ZINT,T,PDSL,AETA,PT,Q2,HGT,HTM,EL0
     I, LM,LM1,LP1,IM,JM
     O, EL)
C
C     CALCULATES LAYER-AVERAGED BLACKADAR'S MIXING LENGTH, AND PBL TOP
C     AS CPBLT*(ASYMPTOTIC EL); AND THEN EL, ACCOUNT TAKEN OF STABILITY,
C     PBL TOP AND VERTICAL GRID DISTANCE RESTRICTIONS (SEE BELOW)
C
C     SET FROM EXISTING CODES BY L. LOBOCKI, JUNE 5, 1992
C       MODIFIED BY FEDOR MESINGER, OCTOBER 13, NOVEMBER 19
C       MODIFIED BY JIM TUCCILLO FOR MPI IMPLEMENTATION
C
C     INPUT:
C     ------
C
C     ZINT (IM,JM,LP1) - ETA INTERFACES HEIGHT FIELD
C     T    (IM,JM,LM)  - TEMPERATURE
C     APE  (IM,JM,02)  - (P/P0)**-KAPPA IN LAYERS
C     Q2   (IM,JM,LM)  - TURBULENCE KINETIC ENERGY * 2
C     HGT  (IM,JM)     - SURFACE ELEVATION ARRAY
C     HTM  (IM,JM,LM)  - HEIGHT TOPOGRAPHY MASK ARRAY
C     EL0  (IM,JM)     - ARRAY OF ASYMPTOTIC VALUES FOR MIXING LENGTH
C     LM               - ARRAY DIMENSION FOR VERTICAL GRIDS
C     LM1              - ARRAY DIMENSION FOR VERTICAL GRIDS, MINUS ONE
C     LP1              - ARRAY DIMENSION FOR VERTICAL GRIDS, PLUS ONE
C
C     IM,JM            - ARRAY DIMENSIONS FOR HORIZONTAL GRIDS
C
C
C     OUTPUT:
C     -------
C
C     EL   (IM,JM,LM) - FIELD OF RESULTING MASTER LENGTH SCALES
C
C
C     SCRATCH AREAS:
C     --------------
C
C     VKRMZ(IM,JM)
C
C     RELEVANT CONSTANTS:
C     -------------------
C
C     VON KARMAN CONSTANT:
      PARAMETER (VKRM=0.4)
C     CONSTANTS NEEDED FOR THE EL(BL,ST,ZI) SCHEME:
      PARAMETER (FRG=4.*9.8,DRDRFF=0.54,CPBLT=10.,CSH=0.23*0.5
     &, EPSN2=1.E-7,EPSQ2=1.E-4,CAPA=0.28589641)
C
C     ------------------------------------------------------------------
C
      DIMENSION ZINT(IM,JM,LP1),EL(IM,JM,LM),EL0(IM,JM),PDSL(IM,JM)
     &,         HTM(IM,JM,LM),HGT(IM,JM),Q2(IM,JM,LM)
     &,           T(IM,JM,LM),APE(IM,JM,2),AETA(LM)

       INCLUDE "CTLBLK.comm"
C***********************************************************************
C
!$omp  parallel do
      DO L=1,LM
        DO J=JSTA,JEND
        DO I=1,IM
          EL(I,J,L)=0.
        ENDDO
        ENDDO
      ENDDO
C
C---THE AVERAGE EL SCHEME---------------------------(FM, AUGUST 19 MEMO)
C   FIRST GET EL IN THE LAYERS
C
!$omp  parallel do
!$omp& private(vkrmz,zl)
      DO 260 L=1,LM
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        ZL=0.5*(ZINT(I,J,L)+ZINT(I,J,L+1))
        VKRMZ=(ZL-HGT(I,J))*VKRM
        EL(I,J,L)=EL0(I,J)*VKRMZ/(EL0(I,J)+VKRMZ)
      ENDDO
      ENDDO
  260 CONTINUE
C***
C***  GET NOW THE INTERFACE EL BY TWO-POINT AVERAGING OF LAYER VALUES
C***
      DO 280 L=1,LM1
!$omp  parallel do
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        EL(I,J,L)=0.5*(EL(I,J,L)+EL(I,J,L+1))*HTM(I,J,L+1)
      ENDDO
      ENDDO
 280  CONTINUE
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        EL(I,J,LM)=0.0
      ENDDO
      ENDDO
C---STABILITY, PBL TOP, AND VERTICAL GRID DISTANCE RESTRICTIONS:--------
C   COMPUTE EL STABLE AND
C   * USE THE SMALLER OF EL BLACKADAR, EL STABLE IF WITHIN PBL;
C   * USE THE SMALLEST OF EL STABLE, ELVGD, AND VKRMZ IF ABOVE PBL
C       (ASSUME PBL TOP IS AT CPBLT*EL0(K));
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        APE(I,J,1)=(1.E5/(PDSL(I,J)*AETA(1)+PT))**CAPA
      ENDDO
      ENDDO
C
      DO 380 L=1,LM1
!$omp  parallel do
!$omp& private(elst,elvgd,ensq,q2kl,ziag)
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        APE(I,J,2)=(1.E5/(PDSL(I,J)*AETA(L+1)+PT))**CAPA
        ENSQ=HTM(I,J,L+1)*
     1       FRG*(T(I,J,L)*APE(I,J,1)-T(I,J,L+1)*APE(I,J,2))/
     2       ((T(I,J,L)*APE(I,J,1)+T(I,J,L+1)*APE(I,J,2))*
     3        (ZINT(I,J,L)-ZINT(I,J,L+2))+EPSN2)
        ENSQ=AMAX1(ENSQ,EPSN2)
        Q2KL=AMAX1(EPSQ2,Q2(I,J,L))
        ELST=DRDRFF*SQRT(Q2KL/ENSQ)
CWAS    ELST=DRDRFF*SQRT(Q2(I,J,L)/ENSQ)
        ZIAG=ZINT(I,J,L+1)-HGT(I,J)
C
        IF(ZIAG.LT.CPBLT*EL0(I,J))THEN
          EL(I,J,L)=AMIN1(EL(I,J,L),ELST)
        ELSE
          ELVGD=CSH*(ZINT(I,J,L)-ZINT(I,J,L+2))
          EL(I,J,L)=AMIN1(ELST,ELVGD,VKRM*ZIAG)
        ENDIF
        APE(I,J,1)=APE(I,J,2)
      ENDDO
      ENDDO
  380 CONTINUE
C
      RETURN
      END

