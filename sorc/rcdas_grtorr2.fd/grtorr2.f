C-----------------------------------------------------------------------
C  PREPROCESSES A GLOBAL REANALYSIS FILE FOR THE REGIONAL SYSTEM
C-----------------------------------------------------------------------
      PROGRAM GRTORR
 
      CHARACTER*80 STR
      CHARACTER*8  SUBSET,SRC,PRG
      EQUIVALENCE  (SRC,SRCPRG(1))
      EQUIVALENCE  (PRG,SRCPRG(2))
      LOGICAL      WRIT
      REAL*8       HDR(5),ARR(10,255),SRCPRG(2)
 
      DATA X1 /180/
      DATA X2 /  0/
      DATA Y1 / -5/
      DATA Y2 / 90/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      LUBFR = 20
      LUOUT = 51
      BMISS = 10E10
 
      CALL OPENBF(LUBFR,'IN' ,LUBFR)
      CALL OPENBF(LUOUT,'OUT',LUBFR)
 
C  READ THROUGH THE MESSAGES
C  -------------------------
 
1     DO WHILE (IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
 
C  REMOVE ALL SATEMP OBS
C  ---------------------
 
      IF(SUBSET.EQ.'SATEMP') GOTO 1
 
C  READ THROUGH THE OTHER DATA
C  ---------------------------
 
2     DO WHILE (IREADSB(LUBFR).EQ.0)
 
C  CUT OUT ANY DATA OUTSIDE RR DOMAIN
C  ----------------------------------
 
      CALL UFBINT(LUBFR,HDR,5,1,IRET,'XOB YOB')
      XOB = HDR(1)
      XOB = MOD(XOB+360.,360.)
      YOB = HDR(2)
      IF(X1.LE.X2) THEN
         IF(XOB.LT.X1.OR.XOB.GT.X2) GOTO 2
      ELSE
         IF(XOB.LT.X1.AND.XOB.GT.X2) GOTO 2
      ENDIF
      IF(YOB.LT.Y1.OR.YOB.GT.Y2) GOTO 2
 
C  PREPARE TO WRITE OTHER OBS
C  --------------------------
 
      CALL OPENMB(LUOUT,SUBSET,IDATE)
      CALL UFBCPY(LUBFR,LUOUT)
 
C  REMOVE ANY SATWND MASS OBS
C  --------------------------
 
      IF(SUBSET.EQ.'SATWND') THEN
         CALL UFBINT(LUBFR,HDR,5,1,IRET,'TYP')
         IF(HDR(1).LT.200) GOTO 2
      ENDIF
 
C  ADPSFC OBS NEED QM=9 CHANGED TO QM=2 FOR P,Q,T,Z,U,V
C  ----------------------------------------------------
 
      DO I=0,5

      IF(I.EQ.0) STR = 'POB PQM PPC PRC NUL PFC PAN'
      IF(I.EQ.1) STR = 'QOB QQM QPC QRC NUL QFC QAN'
      IF(I.EQ.2) STR = 'TOB TQM TPC TRC TVO TFC TAN'
      IF(I.EQ.3) STR = 'ZOB ZQM ZPC ZRC NUL ZFC ZAN'
      IF(I.EQ.4) STR = 'UOB WQM WPC WRC VOB UFC UAN'
      IF(I.EQ.5) STR = 'NUL NUL NUL NUL NUL VFC VAN'
      CALL UFBINT(LUBFR,ARR,10,255,NLEV,STR)
 
      WRIT = .FALSE.

C  ----------------------------------------------------
C  ADPSFC OBS NEED QM=9 CHANGED TO QM=2 FOR P,Q,T,Z,U,V
C  ----------------------------------------------------
C  ADPSFC TEMP OBS SHOULD BE VIRTUAL - MAKE IT SO
C  ----------------------------------------------
 
      IF(SUBSET.EQ.'ADPSFC') THEN
         IF(ARR(2,1).EQ.9) THEN
            ARR(2,1) = 2
            ARR(3,1) = 1
            ARR(4,1) = 0
            WRIT = .TRUE.
         ENDIF
         IF(I.EQ.2 .AND. ARR(5,1).LT.BMISS) THEN
            ARR(1,1) = ARR(5,1)
            ARR(3,1) = 8
            ARR(4,1) = 0
            WRIT = .TRUE.
         ENDIF
      ENDIF
 
C  ----------------------------------------------------
C  SFCSHP OBS NEED QM=* CHANGED TO QM=2 FOR P,Q,T,Z,U,V
C  ----------------------------------------------------
C  SFCSHP TEMP OBS SHOULD BE VIRTUAL - MAKE IT SO
C  ----------------------------------------------
 
      IF(SUBSET.EQ.'000000') THEN
         IF(ARR(1,1).LT.BMISS) THEN
            ARR(2,1) = 2
            ARR(3,1) = 1
            ARR(4,1) = 0
         ELSEIF(ARR(2,1).LT.BMISS) THEN
            ARR(2,1) = 10E9
         ENDIF
         IF(I.EQ.2) THEN
            CALL UFBINT(LUBFR,HDR,5,1,IRET,'POB QOB TOB')
            POB = HDR(1)
            QOB = HDR(2)
            TOB = HDR(3)
            IF(POB.LT.BMISS.AND.QOB.LT.BMISS.AND.TOB.LT.BMISS) THEN
               ARR(1,1) = VIRT(POB,ESPH(POB,QOB*1.E-6),TOB)
               ARR(3,1) = 8
               ARR(4,1) = 0
               ARR(5,1) = ARR(1,1)
            ENDIF
         ENDIF
         SRC = 'COADSRR '
         PRG = 'COADTFS '
         CALL UFBINT(LUOUT,SRCPRG,2,1,IRET,'SRC PRG')
         WRIT = .TRUE.
      ENDIF
 
C  SET ALL THE BACKGROUND VALUES TO MISSING
C  ----------------------------------------
 
      DO L=1,NLEV
      IF(ARR(6,L).LT.BMISS.or.ARR(7,L).LT.BMISS) THEN
         ARR(6,L) = 10E9
         ARR(7,L) = 10E9
         WRIT = .TRUE.
      ENDIF
      ENDDO
 
C  REFLECT ADJUSTMENTS MADE TO THIS OBSERVATION
C  --------------------------------------------
 
      IF(WRIT) CALL UFBINT(LUOUT,ARR,10,NLEV,IRET,STR)
 
      ENDDO ! END OF VARTYPE(I) LOOP
 
C  WRITE THIS REPORT INTO THE RR OBSERVATION FILE
C  ----------------------------------------------
 
      CALL WRITSB(LUOUT)
 
      ENDDO ! END OF READSB LOOP
      ENDDO ! END OF READMG LOOP
 
C  WHEN DONE FINISH UP THE OUTPUT FILE AND EXIT
C  --------------------------------------------
 
      CALL CLOSBF(LUOUT)
      STOP
      END
C-----------------------------------------------------------------------
C  MOIFUN IS A SET OF MOISTURE VARIABLE CONVERSION FUNCTIONS. THE
C  FUNCTION PACKAGE RELATES ATMOSPHERIC PRESSURE, TEMPERATURE, AND
C  WATER VAPOR PRESSURE, WITH THE MOISTURE VARIABLES OF RELATIVE
C  HUMIDITY, MIXING RATIO, SPECIFIC HUMIDITY, DEWPOINT, AND VIRTUAL
C  TEMPERATURE. VARIOUS COMPONENTS OF THE FUNCTIONS ARE GIVEN BELOW.
C
C
C  SYMBOLS (AND THEIR UNITS) USED IN THE FUNCTION ARGUMENTS ARE:
C
C     P  = ATMOSPHEREIC PRESSURE     (MB)
C     E  = WATER VAPOR PRESSURE      (MB)
C     T  = TEMPERATURE               (DEG C)
C     R  = RELATIVE HUMIDITY         (PERCENT)
C     W  = MIXING RATIO              (G/G)
C     Q  = SPECIFIC HUMIDITY         (G/G)
C     DP = TEMPERATURE               (DEG C)
C     ES = SATURATION VAPOR PRESSURE (MB)
C     WS = SATURATION MIXING RATION  (G/G)
C     TV = VIRTUAL TEMPERATURE       (DEG C)
C     TS = SENSIBLE TEMPERATURE      (DEG C)
C
C  PHYSICAL CONSTANTS USED ARE:
C
C     EZERO = WATER VAPOR ES @ 0 DEG C    (   6.11  MILIBARS        )
C     EVLAT = LATENT HEAT OF EVAPORATION  (  597.3  CAL/G           )
C     VMOLW = MOLECULAR WEIGHT OF WATER   ( 18.016  G/MOL           )
C     DMOLW = MOLECULAR WEIGHT OF DRY AIR ( 28.966  G/MOL           )
C     RSTAR = UNIVERSAL GAS CONSTANT      (   1.98  CAL/(MOL*DEG K) )
C     TZERO = 0 DEG C IN DEG K            ( 273.16  DEG K           )
C     EPSLN = VMOLW/DMOLW                 (   .622  NO UNITS        )
C
C  THE SATURATION WATER VAPOR PRESSURE(ES) IS COMPUTED AS A SOLUTION
C  OF THE CLAUSIUS-CLAPEYRON EQUATION (SEE HESS,"INTRODUCTION TO
C  THEORETICAL METEOROLOGY", PGS 48-49). THE SPECIFIC FORMULATION,
C
C  ES(T)=EZERO*EXP(17.269*T/(T+237.3)),
C
C  WHERE T IS DEGREES CELSIUS, IS A FORM OF TETENS' FORMULA FOR SATURATION
C  VAPOR PRESSURE. FOR MORE DETAILS SEE NMC OFFICE NOTE 36, OR
C  TETENS, O., 1930: OBER EINIGE METEOROLOGISCHE VEGRIFFE. Z. GEOPHYS.
C
C  THE DEFINITIONS OF MOISTURE VARIABLES ARE (FROM HESS,PGS 58-60):
C
C     RELATIVE HUMIDITY    R  = W/WS
C     MIXING RATIO         W  = EPSLN*E/(P-E)
C     SPECIFIC HUMIDITY    Q  = EPSLN*E/(P-E*(1-EPSLN))    (EXACT FORM)
C     DEW POINT            DP = TEMPERATURE WHERE WS(DP,P,E) = W(T,P,E)
C     VIRTUAL TEMPERATURE  VT = T*(1+W/EPSLN)/(1+W)
C     SENSIBLE TEMPERATURE TS = T*(1+W)/(1+W/EPSLN)
C
C  MOIFUN PROVIDES ENTRY POINTS WHICH COMPUTE MOISTURE VARIABLES FROM
C  THEIR COMPONENTS. ENTRIES ARE ALSO GIVEN WHICH CALCULATE E FROM
C  THE MOISTURE VARIABLES. THIS ENABLES CONVERSIONS BETWEEN MOISTURE
C  VARIABLES. THE FUNCTION ENTRY POINTS ARE:
C
C     1) ESVP(T     ) - SATURATION WATER VAPOR PRESSURE FROM T
C     2) ERLH(P,R,T ) - WATER VAPOR PRESSURE FROM P,R,T
C     3) EMIX(P,W   ) - WATER VAPOR PRESSURE FROM P,W
C     4) ESPH(P,Q   ) - WATER VAPOR PRESSURE FROM P,Q
C     5) EDEW(DP    ) - WATER VAPOR PRESSURE FROM DP
C     6) RELH(P,E,T ) - RELATIVE HUMIDITY FROM P,E,T
C     7) WMIX(P,E   ) - MIXING RATIO FROM P,E
C     8) QSPH(P,E   ) - SPECIFIC HUMIDITY FROM P,E
C     9) DPAL(E     ) - DEW POINT FROM E   (ALGEBRAIC)
C    10) VIRT(P,E,TS) - VIRTUAL  TEMPERATURE FROM P,E,TS
C    11) SENT(P,E,TV) - SENSIBLE TEMPERATURE FROM P,E,TV
C
C  THE MAIN ENTRY POINT (MOIFUN) IS A DUMMY FUNCTION.
C
C     CALLING MOIFUN RESULTS IN AN ABORT.
C
C-----------------------------------------------------------------------
      FUNCTION MOIFUN(DUMMY)
 
      PARAMETER (RSTAR = 1.98        )
      PARAMETER (TZERO = 273.16      )
      PARAMETER (EVLAT = 597.3       )
      PARAMETER (VMOLW = 18.016      )
      PARAMETER (DMOLW = 28.966      )
      PARAMETER (EZERO = 6.11        )
      PARAMETER (EPSLN = VMOLW/DMOLW )
 
C-----------------------------------------------------------------------
      ES(T) = EZERO*EXP(17.269*T/(T+237.3))
C-----------------------------------------------------------------------
 
C  MAIN ENTRY POINT DOES NOTHING
C  -----------------------------

      moifun = 0 
      RETURN
 
C  SATURATION VAPOR PRESSURE FROM T
C  --------------------------------
 
      ENTRY ESVP(T)
      ESVP = ES(T)
      RETURN
 
C  VAPOR PRESSURE FROM P,R,T
C  -------------------------
 
      ENTRY ERLH(P,R,T)
      RH   = .01*R
      ERLH = P/(1.+P/(RH*ES(T))-1./RH)
      RETURN
 
C  VAPOR PRESSURE FROM P,W
C  -----------------------
 
      ENTRY EMIX(P,W)
      EMIX = W*P/(EPSLN+W)
      RETURN
 
C  VAPOR PRESSURE FROM P,Q
C  -----------------------
 
      ENTRY ESPH(P,Q)
      ESPH = Q*P/(epsln+Q*(1.-EPSLN))
      RETURN
 
C  VAPOR PRESSURE FROM DP
C  ----------------------
 
      ENTRY EDEW(DP)
      EDEW = ES(DP)
      RETURN
 
C  RELATIVE HUMIDITY FROM P,E,T
C  ----------------------------
 
      ENTRY RELH(P,E,T)
      EST  = ES(T)
      RELH = (E/(P-E))/(EST/(P-EST))
      RETURN
 
C  MIXING RATIO FROM P,E
C  ---------------------
 
      ENTRY WMIX(P,E)
      WMIX = EPSLN*E/(P-E)
      RETURN
 
C  SPECIFIC HUMIDITY FROM P,E
C  --------------------------
 
      ENTRY QSPH(P,E)
      QSPH = EPSLN*E/(P-E*(1.-EPSLN))
      RETURN
 
C  DEW POINT FROM ALGEBRAIC MANIPULATION OF ES FUNCTION GIVEN E
C  ------------------------------------------------------------
 
      ENTRY DPAL(E)
      XLNE = LOG(E/EZERO)
      DPAL = 237.3*XLNE/(17.269-XLNE)
      RETURN
 
C  VIRTUAL TEMPERATURE FROM P,E,TS
C  -------------------------------
 
      ENTRY VIRT(P,E,TS)
      RMIX = EPSLN*E/(P-E)
      VIRT = (TS+TZERO)*(1.+RMIX/EPSLN)/(1.+RMIX) - TZERO
      RETURN
 
C  SENSIBLE TEMPERATURE FROM P,E,TV
C  --------------------------------
 
      ENTRY SENT(P,E,TV)
      RMIX = EPSLN*E/(P-E)
      SENT = (TV+TZERO)*(1.+RMIX)/(1.+RMIX/EPSLN) - TZERO
      RETURN
 
      END
