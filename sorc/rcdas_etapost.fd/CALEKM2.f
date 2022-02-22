      SUBROUTINE CALEKM2(U1D,V1D)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    CALEKM2     COMPUTES EKMAN ROT. GEOS. WINDS
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-03-25       
C     
C ABSTRACT:
C     THIS ROUTINE COMPUTES EKMAN SPRIAL ROTATED GEOSTROPHIC
C     WINDS FROM THE SEA LEVEL PRESSURE.  THE EKMAN SPIRAL 
C     ROTATION IS BASED ON THE MATERIAL PRESENTED IN SECTION
C     8.5.2 (PP274-277) OF "NUMERICAL WEATHER PREDICTION AND
C     DYNAMIC METEOROLOGY" BY HALTINER AND WILLIAMS 
C     (WILEY,1980).
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-03-23  RUSS TREADON
C   98-06-16  T BLACK - CONVERSION FROM 1-D TO 2-D
C   00-01-04  JIM TUCCILLO - MPI VERSION           
C     
C USAGE:    CALL CALEKM2(U1D,V1D)
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     U1D      - EKMAN SPIRAL GEOSTROPHIC U WIND
C     V1D      - EKMAN SPIRAL GEOSTROPHIC V WIND
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - EXTRA
C                  VRBLS
C                  DYNAMD
C                  MAPOT
C                  CTLBLK
C                  MASKS
C                  LOOPS
C                  INDX
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : CRAY C-90
C$$$  
C
C
C     INCLUDE PARAMETERS
      INCLUDE "parmeta"
      INCLUDE "params"
C
      PARAMETER (EDDYK=5.,TWOK=2.*EDDYK)
      PARAMETER (Z=250.,H=50.,ZMH=Z-H)
      PARAMETER (ARGLIM=500.,ISMTHP=2)
      PARAMETER (FACTOR=1.0,RHO=1.)
C
C     DECLARE VARIABLES
      REAL U1D(IM,JM),V1D(IM,JM),SLPH(IM,JM),SLPV(IM,JM)
C
C     INCLUDE COMMON BLOCKS.
      INCLUDE "EXTRA.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "MAPOT.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "INDX.comm"
C
C******************************************************************
C     START CALEKM2 HERE.
C
C     SET CONSTANTS.
      D75PI =3.*ACOS(-1.)/4.
      DEG2RD=ACOS(-1.)/180.
      SQRT2 =SQRT(2.)
C
C     INITIALIZE WIND COMPONENTS TO ZERO.
C
!$omp  parallel do
      DO J=JSTA,JEND
      DO I=1,IM
        U1D(I,J) =D00
        V1D(I,J) =D00
        SLPH(I,J)=SLP(I,J)
        SLPV(I,J)=D00
      ENDDO
      ENDDO
C     
C     COMPUTE 1000MB HEIGHTS AT V POINTS.
C
      CALL P2FILT(ISMTHP,HBM2,SLPH)
C
      DO J=JSTA_M,JEND_M
      DO I=2,IM-1
        SLPV(I,J)=D25*(SLPH(I+IVE(J),J)+SLPH(I+IVW(J),J)
     1                +SLPH(I,J+1)+SLPH(I,J-1))
      ENDDO
      ENDDO
C
      CALL P2FLTV(ISMTHP,VBM2,SLPV)
C
C     LOOP OVER HORIZONTAL GRID.
C
      DO 30 J=JSTA_M2,JEND_M2
      DO 30 I=2,IM-1
C     
C        OBTAIN FAL WIND COMPONENTS
C
      LLMH=LMH(I,J)
      UFAL=U(I,J,LLMH)
      VFAL=V(I,J,LLMH)

CX         WRITE(81,*)' '
CX 1234    FORMAT(I5,1X,I2,1X,5(G12.6,1X))

C
C        COMPUTE GEOSTROPHIC WIND BASED ON SEA LEVEL PRESSURE.
C
      FTRUE=(2.*F(I,J))/DT
      RRHOF=1./(RHO*FTRUE)
      DPDX =(SLPV(I+IHE(J),J)-SLPV(I+IHW(J),J))/(2.*DX(I,J))
      DPDY =(SLPV(I,J+1)-SLPV(I,J-1))/(2.*DY)
      UG   =-1.*RRHOF*DPDY*HBM2(I,J)
      VG   =RRHOF*DPDX*HBM2(I,J)
      SPDG =SQRT(UG**2+VG**2)

CX         WRITE(81,1235) G,FTRUE,DX(K),DY
CX         WRITE(81,1235) SLPV(K),SLPV(K-1),SLPV(K+IM-1),SLPV(K-IM)
CX         WRITE(81,1235) GRF,DZDX,DZDY,UG,VG,SPDG
CX 1235    FORMAT(5(G12.6,1X))

C
C        COMPUTE EKMAN SPIRAL COEFFICIENTS.
C     
      WDIRT=WDIR(UFAL,VFAL)
      WDIRG=WDIR(UG,VG)
      B    =SQRT(FTRUE/TWOK)
      BZMH =B*ZMH
      IF (BZMH.GT. ARGLIM) BZMH= ARGLIM
      IF (BZMH.LT.-ARGLIM) BZMH=-ARGLIM
      EXBZMH = EXP(-1.*BZMH)
C     
C        COMPUTE EKMAN SPIRAL U WIND COMPONENT.
C
      ALPHAS=+45.
      ALPHAS=ALPHAS*DEG2RD*FACTOR
      SINALF=SIN(ALPHAS)
      ARG   =D75PI + ALPHAS - BZMH
      COSARG=COS(ARG)
      IF(((WDIRG.GE.000.).AND.(WDIRG.LE.090.)).OR.
     X     ((WDIRG.GE.270.).AND.(WDIRG.LE.360.)) )
     X    COSARG = -1.*COSARG
      U1D(I,J)=UG+SQRT2*SPDG*SINALF*EXBZMH*COSARG

CX         WRITE(81,1235) WDIRG,WDIRT,ALPHAS,SINALF
CX         WRITE(81,1235) B,ZMH,BZMH,EXBZMH
CX         WRITE(81,1235) ARG,COSARG,SINARG
C     
C        COMPUTE EKMAN SPIRAL V WIND COMPONENT.
C
      ALPHAS=+45.
      ALPHAS=ALPHAS*DEG2RD*FACTOR
      SINALF=SIN(ALPHAS)
      ARG   =D75PI + ALPHAS - BZMH
      SINARG=SIN(ARG)
      IF((WDIRG.GE.000.).AND.(WDIRG.LE.180.))
     X    SINARG=-1.*SINARG
      V1D(I,J)=VG+SQRT2*SPDG*SINALF*EXBZMH*SINARG

CX         WRITE(81,1235) WDIRG,WDIRT,ALPHAS,SINALF
CX         WRITE(81,1235) B,ZMH,BZMH,EXBZMH
CX         WRITE(81,1235) ARG,COSARG,SINARG

C
C     SCALE EKMAN SPIRAL WIND COMPONENTS TO AN ACCEPTABLE
C     LEVEL THIS IS ENTIRELY AD HOC.  IT WAS DONE TO PRODUCE
C     A PLEASING WIND FIELD.
C
      U1D(I,J)=D50*U1D(I,J)
      V1D(I,J)=D50*V1D(I,J)

CX         WRITE(81,1234) K,LLMH,WDIRG,WDIRT,UG,VG
CX         WRITE(81,1234) K,LLMH,SQRT2,SPDG,ALPHAS/DEG2RD,SINALF
CX         WRITE(81,1234) K,LLMH,EXBZMH,COSARG,SINARG
CX         WRITE(81,1234) K,LLMH,UFAL,VFAL,U1D(K),V1D(K)

 30   CONTINUE
C
C     END OF ROUTINE.
      RETURN
      END




