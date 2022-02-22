      SUBROUTINE PETABCS(ZLATB,ZWLONB,ULATB,UWLONB,SI,SL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: PETABC         INTERPOLATES ETA BND VALUES FROM SIGMA TO E
C   PRGMMR: ROGERS           ORG: NP22        DATE: 97-12-31  
C
C ABSTRACT: PERFORMS VERTICAL INTERPOLATION (QUADRATIC IN LOG P
C   FOR HEIGHT, LINEAR IN LOG P FOR WIND/SPECIFIC HUMIDITY) OF THE
C   ETA MODEL BOUNDARY VALUES FROM THE AVN SIGMA LEVELS TO ETA LEVELS.
C
C PROGRAM HISTORY LOG:
C   ??-??-??  T BLACK
C   95-02-07  E ROGERS ADDED DOCBLOCK
C   96-11-18  E ROGERS ADDED INCLUDES FOR E-GRID DIMENSIONS AND GLOBAL
C             MODEL VERTICAL STRUCTURE
C   98-03-08  E ROGERS ADDED READ OF GDAS SIGMA VALUES FROM BINARY FILE
C   99-02-10  E ROGERS COMBINED MKBND AND PETABCS INTO ONE CODE
C
C USAGE:
C
C   INPUT FILES:
C     UNIT42     - DEPTH OF THE ETA MODEL LAYERS
C     UNIT90     - ETA BOUNDARY VALUES FROM AVN FORECAST IN SIGMA
C
C   OUTPUT FILES:
C     UNIT52     - ETA BOUNDARY VALUES FROM AVN FORECAST IN ETA
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:    - FLIP, ROTLLE
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN
C   MACHINE: CRAY YMP/C90
C
C
C     SETTING UP OF THE VERTICAL GRID VARIABLES, AND
C     VERTICAL INTERPOLATION, PRESSURE TO ETA SURFACES
C     THIS VERSION USES INPUT SIGMA DATA INSTEAD OF
C     STANDARD LEVEL PRESSURE DATA
C$$$
C----------------------------------------------------------------
C----------------------------------------------------------------
C
        INCLUDE "parmlbc"
        PARAMETER(LDM=KMAX)
C
                        P A R A M E T E R
     1 (LMM1=LM-1,LMP1=LM+1)
                        P A R A M E T E R
     1 (KBVBR=IM-1,KBVTL=IM,KBVTR=2*(IM-1)
     2, KBVLL=KBVTR+1,KBVUL=KBVLL+JM/2-1
     3, KBVLR=KBVUL+1,KBVUR=KB)
                        P A R A M E T E R
     1 (IMT=2*IM-1,JMT=JM/2+1)
                        P A R A M E T E R
     1 (CM1=2937.4,CM2=4.9283,CM3=23.5518,EPS=0.622)
C----------------------------------------------------------------
                        D I M E N S I O N
     1  T(KB,LDM),H(KB,LDM+1),U(KB,LDM),V(KB,LDM),HETA(KB,LM)
     2, PSFC(KB),ZSFC(KB)
     3, Q(KB,LDM),QETA(KB,LM),QETAIJ(LM)
     4, PD(KB),UETA(KB,LM),VETA(KB,LM),TETA(KB,LM)
     5, HETAIJ(LM),UETAIJ(LM),VETAIJ(LM),SPL(LDM)
c    5, HETAIJ(LM),UETAIJ(LM),VETAIJ(LM),SPL(LDM),ALPR(LDM+1)
     6, LDT1(LM)
c    6, ALP(LDM+1), ALPSQ(LDM+1), LDT1(LM), DLT(LM)
     7, PDB(KB,2), TB(KB,LM,2), UB(KB,LM,2), VB(KB,LM,2)
     8, QB(KB,LM,2),HGT(KB),PHIS(KB),EXNL(KB)
C
     9, SI(KMAX+1),SL(KMAX),SII(LDM+1),SLL(LDM),ZWLONB(KB)  
     1, UWLONB(KB)
                        D I M E N S I O N
     1  SIGMID(LDM),SIGINT(LDM+1)
     2, ETA(LMP1),ETAL(LM),DETA(LM),ZETA(LMP1),REF(KB)
     3, PMID(KB,LDM),PINT(KB,LDM+1),TSGBT(KB)
     4, ZLATB(KB),ZLONB(KB),ULATB(KB),ULONB(KB)
     5, K1(KB),K2(KB)
C
                             P A R A M E T E R
     & (K15=SELECTED_REAL_KIND(15))
C
                             R E A L
     & (KIND=K15) ALPSQ(LDM+1),DLT(LM),ALPR(LDM+1),ALP(LDM+1)
     &  ,ARGL2G,ALPETA,ALPM,DLTPD,B,C,DISCRM
C----------------------------------------------------------------
C
C  SIGMA SURFACES FOR GLOBAL SYSTEM
C  NOW READ IN FROM SUBROUTINE CALL
C
                            D A T A
     1  LINI/90/, LOUT/52/
C
C----------------------------------------------------------------
c                    E Q U I V A L E N C E
c    1  (UETA(1,1),U(1,1))
c    2, (VETA(1,1),V(1,1)),(TETA(1,1),H(1,1))
c    3, (HGT(1),PHIS(1)),(QETA(1,1),Q(1,1))
C----------------------------------------------------------------
                     N A M E L I S T /MODTOP/
     1 ETOP
C***********************************************************************
      LDMP=LDM+1
      LDMM=LDM-1
c     print *,'petabcs ',kmax,ldm,ldmp,ldmm
C
      REWIND LINI
C
      READ(41,MODTOP)
C     READ(41,MODTOP,END=10)
   10 CONTINUE
C
      DO K = 1, LDM
       SII(K) = SI(K)
       SLL(K) = SL(K)
      ENDDO
      SII(LDMP) = SI(LDMP)
C
C  FLIP SII, SLL ARRAYS
C
      DO K = 1, LDMP
        IREV = LDMP-K+1
        SIGINT(IREV) = SII(K)
      ENDDO
      DO K = 1, LDM
        IREV = LDM-K+1
        SIGMID(IREV) = SLL(K)
      ENDDO
      WRITE(6,15)SIGMID,SIGINT
15    FORMAT(10G12.5)
C
C   SWITCH INPUT LONGITUDE BACK TO DEGREES EAST
C
      DO K = 1, KB
        ZLONB(K) = 360.0 - ZWLONB(K)
        ULONB(K) = 360.0 - UWLONB(K)
      ENDDO
C
      DO 1000 KTIME=1,100
C***
C***  READ PRESSURE DATA ON BOUNDARY
C***
      READ(LINI,END=1500)T,U,V,Q,PSFC,ZSFC
      WRITE(6,12121)KTIME
12121 FORMAT(' READ PTETBC  KTIME=',I2)
C***
C***  FLIP 3-DIMENSIONAL ARRAYS BEFORE INTERPOLATION
C***
      CALL FLIP(T,LDM)
      CALL FLIP(U,LDM)
      CALL FLIP(V,LDM)
      CALL FLIP(Q,LDM)
C***
C***  ROTATE WINDS FROM LAT/LONG GRID TO ETA GRID
C***
      CALL ROTLLE(U,V,ULATB,ULONB,LDM)
C***
C***  CONVERT SURFACE PRESSURE TO PASCALS
C***
      DO 50 K=1,KB
       PSFC(K)=PSFC(K)*100.
       TSGBT(K)=T(K,LDM)
   50 CONTINUE
C
c     DO 75 KIN=1,KB
c       IF(KIN.EQ.5 .OR. MOD(KIN,30).EQ.0)THEN
c        WRITE(6,60)KIN,PSFC(KIN),ZSFC(KIN)
c  60    FORMAT(2X,'KB(INPUT)=',I3,2(1X,E12.5))
c        DO 70 LIN=1,LDM
c           WRITE(6,65)KTIME,LIN,T(KIN,LIN),Q(KIN,LIN),
c    1         U(KIN,LIN),V(KIN,LIN)
c  65       FORMAT(2X,2(I3,1X),4(E12.5,1X))
c  70    CONTINUE
c       ENDIF
c  75 CONTINUE
C***
C***  DEFINITION OF CONSTANTS NEEDED FOR VERTICAL INTERPOLATION
C***
      G=9.80
      R=287.04
      CP=1004.5
      P00=1.0E5
      PRF0=101325.
      GAMMA=0.0065
      T0=288.
      ROG=R/G
      CPOG=CP/G
      CAPA=R/CP
      GORG=G/(R*GAMMA)
      RGOG=R*GAMMA/G
C***
C***  DEFINITION OF PRESSURE DATA AND ETA LAYER CONSTANTS
C***
      LDMM1=LDM-1
C***
C*** NOTE: THE PRESSURE AT THE TOP OF THE MODEL IS ETOP MB
C***
      PT= ETOP*100.
      ALPT=ALOG(PT)
C
C------- DEPTH OF ETA LAYERS ----------------------------------------
C
      REWIND 42
      READ(42)DETA
C***
C***  ETA AT THE INTERFACES OF THE MODEL LAYERS
C***
      ETA(1)=0.
      DO 125 L=1,LMM1
      ETA(L+1)=ETA(L)+DETA(L)
  125 CONTINUE
      ETA(LMP1)=1.
      DETA(LM)=1.-ETA(LM)
      DO 150 L=1,LM
      ETAL(L)=0.5*(ETA(L)+ETA(L+1))
  150 CONTINUE
C
      DO 160 L=1,LMP1
      ZETA(L)=T0*(1.-(PT+ETA(L)*(PRF0-PT)/PRF0)**RGOG)/GAMMA
  160 CONTINUE
C
      DO 175 K=1,KB
      HGT(K)=0.
  175 CONTINUE
C
      DO 180 K=1,KB
      PD(K)=1.
      REF(K)=1.
  180 CONTINUE
C***
C***  FIND PRESSURES ON SIGMA LAYER INTERFACES & MIDPOINTS
C***  FIND HEIGHTS ON SIGMA LAYER INTERFACES
C***
      DO 225 K=1,KB
      DO 200 L=1,LDM
      PMID(K,L)=SIGMID(L) * PSFC(K)
  200 CONTINUE
C
        DO 210 L=1,LDMP
      PINT(K,L)=SIGINT(L)*PSFC(K)
  210 CONTINUE
      H(K,LDMP)=ZSFC(K)
c     IF(ZSFC(K) .LT. 0.0) THEN
c       H(K,LDMP) = 0.0
c     ENDIF
  225 CONTINUE
C
      DO 250 LL=1,LDMM
        L=LDMP-LL
        DO 250 K=1,KB
        IF(L.EQ.LDM)EXNL(K)=(PSFC(K)/P00)**CAPA
        EXNT=(PINT(K,L)/P00)**CAPA
        H(K,L)=H(K,L+1)-CPOG*T(K,L)*(EXNT-EXNL(K))
     1       *(P00/PMID(K,L))**CAPA
C
c       IF(K.EQ.5) THEN
c         WRITE(6,251)K,L,T(K,L),H(K,L),H(K,L+1),
c    1     PINT(K,L),PMID(K,L),EXNL(K),EXNT,PSFC(K),ZSFC(K)
c 251     FORMAT(2X,2I3,9(E12.5,1X))
c       ENDIF
C
        EXNL(K)=EXNT
  250 CONTINUE
C
C***
C***  VERTICAL INTERPOLATION OF HEIGHTS AND SPECFIC HUMIDITY
C***  QUADRATICALLY IN LN P AND OF WINDS LINEARLY IN LN P.
C***
C***
C***  COMPUTATION OF THE 'SEA LEVEL' PRESSURE DIFFERENCE, PD.
C***
      DO 500 K=1,KB
      SPLM=PINT(K,LDMP)
      RSPL=SPLM/PINT(K,LDM)
      DO 275 LD=2,LDM
      ALPR(LD)=ALOG(PINT(K,LD+1)/PINT(K,LD))
  275 CONTINUE
      DO 290 LD=2,LDMP
      ALP(LD)=ALOG(PINT(K,LD))
      ALPSQ(LD)=ALP(LD)**2
  290 CONTINUE
C***
C***  QUADRATIC INTERPOLATION FOR ETA LEVEL N WILL BE CALCULATED USING
C***  THE THREE PRESSURE LEVELS LDT1(N-1), LDT1(N), AND LDT1(N+1).
C***
C
      LDMP1=LDMP-1
      DO 300 LS=2,LDMP1
      IF(H(K,LS).LT.HGT(K))THEN
        L1=LS-1
        GO TO 305
      ENDIF
  300 CONTINUE
      L1=LDMP-2
  305 IF(L1.LT.2)L1=2
      DLTPD=ALPR(L1+1)*ALOG(PINT(K,L1+2)/PINT(K,L1))*(-ALPR(L1))
C
      L2=L1+1
      L3=L1+2
      H1=H(K,L1)
      H2=H(K,L2)
      H3=H(K,L3)
      H3M2=H3-H2
      H3M1=H3-H1
      B=(H3M2*(ALPSQ(L3)-ALPSQ(L1))-H3M1*(ALPSQ(L3)-ALPSQ(L2)))/DLTPD
      C=(H3M1*(ALP(L3)-ALP(L2))-H3M2*(ALP(L3)-ALP(L1)))/DLTPD
      IF(ABS(C).LT.1.E-5)GO TO 350
      DISCRM=B**2-4.*C*(-C*ALPSQ(L3)-B*ALP(L3)+H3-HGT(K))
      IF(DISCRM.LT.0.)THEN
        WRITE(6,*)'DISCRM<0 ',K,KTIME,DISCRM,L3,H3,HGT(K),H2,H1
C       WRITE(3,315)B,C,ALPSQ(L3),ALP(L3)
C       WRITE(3,320)H3M2,ALPSQ(L1),H3M1,ALPSQ(L2),L,DLT(L)
C       WRITE(3,325)ALP(L2),ALP(L1)
C 310   FORMAT(' K=',I4,' KTIME =',I2,' DISCRM=',E12.5,' L3=',I3,
C    1   ' H3=',E12.5,' HGT=',E12.5,' H2=',E12.5,' H1=',E12.5)
C 315   FORMAT(' B=',E12.5,' C=',E12.5,' ALPSQ(L3)=',E12.5,
C    1   ' ALP(L3)=',E12.5)
C 320   FORMAT(' H3M2=',E12.5,' ALPSQ(L1)=',E12.5,' H3M1=',E12.5,
C    1   ' ALPSQ(L2)=',E12.5,' L=',I2,' DLT=',E12.5)
C 325   FORMAT(' ALP(L2)=',E12.5,' ALP(L1)=',E12.5,/)
C       STOP666
      ENDIF
      IF(DISCRM.LT.0.)DISCRM=0.
      ALPGT=(-B-SQRT(DISCRM))/(2.*C)
C     ALPGT=(-B-SQRT(B**2-4.*C*(-C*ALPSQ(L3)-B*ALP(L3)+H3-HGT(K))))/
C    1 (2.*C)
      PDIJ=(EXP(ALPGT)-PT)/REF(K)
      GO TO 375
  350 HLDM=H(K,LDM)
C***
C***  PDIJ IS EQUAL TO THE DIFFERENCE PSFC EXTRAPOLATED TO
C***  MEAN SEA LEVEL (ETA=1) AND PT.
C***
      PDIJ=(SPLM*RSPL**((HLDM-HGT(K))/(H(K,LDMM1)-HLDM))-PT)/REF(K)
C***
C***  COMPUTATION OF HEIGHTS AT UPPER BOUNDARIES OF ETA LAYERS.
C***
  375 CONTINUE
      DETA1=0.
      KNTSIG=2
      PETA=PT
      DO 390 L=1,LM
      IF(L.GT.1)DETA1=DETA(L-1)
      PETA=PETA+DETA1*PDIJ
  380 CONTINUE
      IF(PETA.LE.PINT(K,2).OR.
     1      PETA.GT.PINT(K,KNTSIG).AND.PETA.LE.PINT(K,KNTSIG+1)
     2      .OR.KNTSIG.GE.LDMM)THEN
        LDT1(L)=KNTSIG
        GO TO 390
      ENDIF
      KNTSIG=KNTSIG+1
      GO TO 380
  390 CONTINUE
      DO 400 L=1,LM
      L1=LDT1(L)
      IF(L1.LT.2)L1=2
      DLT(L)=ALPR(L1+1)*ALOG(PINT(K,L1+2)/PINT(K,L1))
     1            *(-ALPR(L1))
  400 CONTINUE
C
         L=LM
  415 L1=LDT1(L)
      IF(L1.LT.2)L1=2
      L2=L1+1
      L3=L1+2
      H1=H(K,L1)
      H2=H(K,L2)
      H3=H(K,L3)
      H3M2=H3-H2
      H3M1=H3-H1
      B=(H3M2*(ALPSQ(L3)-ALPSQ(L1))-H3M1*(ALPSQ(L3)-ALPSQ(L2)))/
     1        DLT(L)
      C=(H3M1*(ALP(L3)-ALP(L2))-H3M2*(ALP(L3)-ALP(L1)))/DLT(L)
  425 ARGLOG=PT+PDIJ*ETA(L)
      ALPETA=ALOG(ARGLOG)
C
      ARGLG2=PT+PDIJ*(ETA(L)+0.5*DETA(L))
C
      ALPM=ALOG(ARGLG2)
C
      HETAIJ(L)=H2+B*(ALPETA-ALP(L2))+C*
     1    (ALPETA*ALPETA-ALPSQ(L2))
C
      IF(L.EQ.1)GO TO 450
      L=L-1
      IF (LDT1(L).EQ.L1)GO TO 425
      GO TO 415
C
  450    PD(K)=PDIJ
      DO 475  L=1,LM
      HETA(K,L)=HETAIJ(L)
  475 CONTINUE
  500 CONTINUE
C***
C***  ARRANGEMENT OF LOCATIONS IN THE V BOUNDARY FILE
C***
C                       2ND
C           IBBP--------->----------ITB
C           ILB                     KB
C            |                       |
C       3RD /|\                     /|\ 4TH
C            |                       |
C           IMT                     ILBP
C            1----------->----------IBB
C                       1ST
      IBB=IMT/2
      IBBP=IBB+1
      ITB=IMT-1
      ILB=IMT+JMT-2
      ILBP=ILB+1
      ILBP2=ILB+2
      ILBM=ILB-1
      IMTP=IMT+1
      KBM=KB-1
C***
C***  FIND AVERAGE PD OVER VELOCITY BOUNDARY POINTS
C***
      DO 505 K=1,IBB
      K1(K)=K
      K2(K)=K+1
  505 CONTINUE
      DO 506 K=IBBP,ITB
      K1(K)=K+1
      K2(K)=K+2
  506 CONTINUE
      DO 507 K=IMTP,ILBM
      K1(K)=K+1
      K2(K)=K+2
  507 CONTINUE
      DO 508 K=ILBP2,KBM
      K1(K)=K
      K2(K)=K+1
  508 CONTINUE
      K1(IMT)=1
      K2(IMT)=IMT+2
      K1(ILB)=ILB+1
      K2(ILB)=IBB+2
      K1(ILBP)=IBB+1
      K2(ILBP)=ILBP+1
      K1(KB)=KB
      K2(KB)=ITB+2
C
      DO 600 K=1,KB
      K1K=K1(K)
      K2K=K2(K)
      PDVP=0.5*(PD(K1K)+PD(K2K))
C
      DO 515 LD=1,LDMM
      PMVPU=0.5*(PMID(K1K,LD)+PMID(K2K,LD))
      PMVPL=0.5*(PMID(K1K,LD+1)+PMID(K2K,LD+1))
      ALPR(LD)=ALOG(PMVPL/PMVPU)
  515 CONTINUE
      DO 520 LD=1,LDM
      PMVP=0.5*(PMID(K1K,LD)+PMID(K2K,LD))
      ALP(LD)=ALOG(PMVP)
  520 CONTINUE
C
C***
C***  COMPUTATION OF WINDS WITHIN ETA LAYERS
C***
      DO 550 L=1,LM
      IF(L.EQ.1)ALPLB=ALPT
      ALPUB=ALPLB
      ALPLB=ALOG(PT+PDVP*ETA(L+1))
      ALPETA=SQRT(0.5*(ALPUB**2+ALPLB**2))
      DO 525 LD=2,LDM
      LDM1=LD-1
      IF(ALPETA.GT.ALP(LD).AND.LD.LT.LDM)GO TO 525
      ULD=U(K,LD)
      VLD=V(K,LD)
      CF=(ALP(LD)-ALPETA)/ALPR(LDM1)
      UETAIJ(L)=ULD+(U(K,LDM1)-ULD)*CF
      VETAIJ(L)=VLD+(V(K,LDM1)-VLD)*CF
      GO TO 550
  525 CONTINUE
  550 CONTINUE
C
      DO 600 L=1,LM
      UETA(K,L)=UETAIJ(L)
      VETA(K,L)=VETAIJ(L)
  600 CONTINUE
C***
C***  CONVERT HEIGHTS TO GEOPOTENTIALS AND FIND THE HYDROSTATIC
C***  VIRTUAL TEMPERATURES.
C***
      DO 700 K=1,KB
      PHIS(K)=G*HGT(K)
      PHUB=0.
      DO 625 IVI=1,LM
      L=LMP1-IVI
      PHLB=PHUB
      PHUB=G*HETA(K,L)
      TETA(K,L)=-(PHLB-PHUB)*(PT+ETAL(L)*PD(K))/(R*DETA(L)*PD(K))
c     IF(K.EQ.5)THEN
c       WRITE(6,610)K,L,HETA(K,L),TETA(K,L),TSGBT(K),PD(K),
c    1  PHLB,PHUB,ETAL(L)
c 610   FORMAT(2X,' K=',I3,' L=',I2,' HETA=',E12.5,
c    1            ' TETA=',E12.5,
c    2            ' TSGBT=',E12.5,' PD=',E12.5,
c    3            ' PHLB=',E12.5,' PHUB=',E12.5,' ETAL=',E12.5)
c       PRINT *,DETA(L),R,PT
c     ENDIF
  625 CONTINUE
      DO 650 LD=1,LDMM
      ALPR(LD)=ALOG(SIGMID(LD+1)/SIGMID(LD))
  650 CONTINUE
      DO 660 LD=1,LDM
      ALP(LD)=ALOG(SIGMID(LD)*PSFC(K))
  660 CONTINUE
C
C     INTERPOLATE Q LINEARLY IN LN(P) FROM SIGMA TO ETA.
C
      ALPLB=ALPT
      DO 680 L=1, LM
      ALPUB=ALPLB
      ALPLB=ALOG(PT+PD(K)*ETA(L+1))
      ALPETA=SQRT(0.5*(ALPUB**2+ALPLB**2))
      DO 670 LD=2, LDM
      IF(ALPETA.GT.ALP(LD).AND.LD.LT.LDM)GO TO 670
      IF(ALPETA.LE.ALP(LD))THEN
        QLD=Q(K,LD)
        LDM1=LD-1
        CF=(ALP(LD)-ALPETA)/ALPR(LDM1)
        QETAIJ(L)=QLD+(Q(K,LDM1)-QLD)*CF
        CLOGES=-CM1/TETA(K,L)-CM2*ALOG10(TETA(K,L))+CM3
        ESE=10.**(CLOGES+2.)
        QS=EPS*ESE/(ETAL(L)*PD(K)+PT-ESE*(1.-EPS))
        QSMX=0.95*QS
        QETAIJ(L)=AMIN1(QETAIJ(L),QSMX)
        GO TO 680
C
C     IF THE ETA SURFACE IS BELOW THE LOWEST SIGMA SURFACE,
C     ASSUME THE RH (GIVEN TETA) IS EQUAL TO THAT ON THE
C     LOWEST SIGMA SURFACE.
C
      ELSEIF(LD.EQ.LDM)THEN
        CLOGES=-CM1/TSGBT(K)-CM2*ALOG10(TSGBT(K))+CM3
        ESS=10.0 **(CLOGES+2.)
        QSLDM=EPS*ESS/(SIGMID(LDM)*PSFC(K)-ESS*(1.-EPS))
        RHLDM=Q(K,LDM)/QSLDM
        RHLDM=AMIN1(RHLDM,0.95)
        CLOGES=-CM1/TETA(K,L)-CM2*ALOG10(TETA(K,L))+CM3
        ESL=10.**(CLOGES+2.)
        QSL=EPS*ESL/(ETAL(L)*PD(K)+PT-ESL*(1.-EPS))
        QETAIJ(L)=RHLDM*QSL
      ENDIF
  670 CONTINUE
  680 CONTINUE
C
      DO 690 L=1,LM
      QETA(K,L)=QETAIJ(L)
      IF(QETA(K,L).LT.1.E-10)QETA(K,L)=1.E-10
      IF(QETA(K,L).GT.25.E-3)QETA(K,L)=25.E-3
  690 CONTINUE
  700 CONTINUE
C***
C***  CONVERT FROM VIRTUAL TO TRUE TEMPERATURE.
C***
      DO 750 K=1,KB
      DO 750 L=4,LM
      IF(TETA(K,L).GT.315.)TETA(K,L)=315.
      IF(TETA(K,L).LT.180.)TETA(K,L)=180.
      TV=TETA(K,L)
      DO 725 ITER=1,3
      TETA(K,L)=TV/(1.+0.608*QETA(K,L))
      CLOGES=-CM1/TETA(K,L)-CM2*ALOG10(TETA(K,L))+CM3
      ESE=10.**(CLOGES+2.)
      QS=EPS*ESE/(ETAL(L)*PD(K)+PT-ESE*(1.-EPS))
      QSMX=0.95*QS
      IF(QETA(K,L).LT.QSMX)GO TO 750
      QETA(K,L)=QSMX
  725 CONTINUE
  750 CONTINUE
C***
C***  REDEFINE PD TO EQUAL PSFC MINUS PT.
C***
      DO 775  K=1,KB
      PD(K)=REF(K)*PD(K)
  775 CONTINUE
C***
C***  THE BOUNDARY VALUES AND TENDENCIES.
C***
      DO 800 N=1,KB
      PDB(N,1)=PD(N)
      PDB(N,2)=0.
      DO 800 L=1,LM
      TB(N,L,1)=TETA(N,L)
      TB(N,L,2)=0.
      QB(N,L,1)=QETA(N,L)
      QB(N,L,2)=0.
      UB(N,L,1)=UETA(N,L)
      UB(N,L,2)=0.
      VB(N,L,1)=VETA(N,L)
      VB(N,L,2)=0.
  800 CONTINUE
      PDBMAX=-9999999.9
C***
C***  WRITE THE BOUNDARY VALUES.
C***
      DO LI=1,LM
        TBMAX=-9999999.9
        QBMAX=-9999999.9
        UBMAX=-9999999.9
        VBMAX=-9999999.9
        PDBMIN=9999999.9
        TBMIN=9999999.9
        QBMIN=9999999.9
        UBMIN=9999999.9
        VBMIN=9999999.9
        DO KI=1,KB
        PDBMAX=AMAX1(PDB(KI,1),PDBMAX)
        TBMAX=AMAX1(TB(KI,LI,1),TBMAX)
        QBMAX=AMAX1(QB(KI,LI,1),QBMAX)
        UBMAX=AMAX1(UB(KI,LI,1),UBMAX)
        VBMAX=AMAX1(VB(KI,LI,1),VBMAX)
        PDBMIN=AMIN1(PDB(KI,1),PDBMIN)
        TBMIN=AMIN1(TB(KI,LI,1),TBMIN)
        QBMIN=AMIN1(QB(KI,LI,1),QBMIN)
        UBMIN=AMIN1(UB(KI,LI,1),UBMIN)
        VBMIN=AMIN1(VB(KI,LI,1),VBMIN)
        ENDDO
        WRITE(6,830)KTIME,LI,PDBMAX,PDBMIN
        WRITE(6,830)KTIME,LI,TBMAX,TBMIN
        WRITE(6,830)KTIME,LI,QBMAX,QBMIN
        WRITE(6,830)KTIME,LI,UBMAX,UBMIN
        WRITE(6,830)KTIME,LI,VBMAX,VBMIN
  830   FORMAT(2X,' KTIME=',2I3,2(E12.5,1X))
      ENDDO
C
      WRITE(LOUT)PDB,TB,QB,UB,VB
C
c     DO 900 KI=1,KB
c     IF(MOD(KI,50).EQ.0)THEN
c     WRITE(6,860)KI,PDB(KI,1),ZSFC(KI)
c 860 FORMAT(2X,' petabcs= ',I6,2(E12.5,1X))
c     DO 880 LI=1,LM
c     WRITE(6,870)KTIME,LI,TB(KI,LI,1),QB(KI,LI,1),
c    1      UB(KI,LI,1),VB(KI,LI,1)
c 870 FORMAT(2X,2(I5,1X),4(E12.5,1X))
c 880 CONTINUE
c     ENDIF
c 900 CONTINUE
 
 1000 CONTINUE
 1500 CONTINUE
      RETURN
      END
