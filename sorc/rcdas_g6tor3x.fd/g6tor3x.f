C-----------------------------------------------------------------------
C  SPLIT 6 HOUR REANALYSIS FILE INTO THREE HOUR GROUPS - RETAIN EVENTS
C-----------------------------------------------------------------------
      PROGRAM G6TOR3X
 
      CHARACTER*80 STR
      CHARACTER*8  SUBSET
      REAL*8       HDR(20),ARR(10,255,10)
      REAL*8       ADATE,BDATE,CDATE,DDATE
 
      DATA X1 /180/
      DATA X2 /  0/
      DATA Y1 / -5/
      DATA Y2 / 90/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
      LUBFR0 = 20
      LUOUT = 51
      BMISS = 10E10
 
      READ(5,*) ADATE
      CALL RADDATE(ADATE,-1.5,CDATE)
      CALL RADDATE(ADATE,+1.5,DDATE)
      JDATE = I4DY(NINT(ADATE))
 
C  OFTEN NEED TWO SIX HOUR FILES FOR ONE THREE HOUR ONE!
C  -----------------------------------------------------
 
      DO LUBFR = LUBFR0,LUBFR0+1
         CALL OPENBF(LUBFR,'IN' ,LUBFR)
         IF (LUBFR.EQ.LUBFR0) THEN
             CALL OPENBF(LUOUT,'OUT',LUBFR)
         ENDIF
 
C  READ THROUGH THE MESSAGES
C  -------------------------
 
1     DO WHILE (IREADMG(LUBFR,SUBSET,IDATE).EQ.0)
 
      IDATE = I4DY(IDATE)

C  REMOVE ALL SATEMP OBS
C  ---------------------
 
      IF(SUBSET.EQ.'SATEMP') GOTO 1
 
C  READ THROUGH THE OTHER DATA
C  ---------------------------
 
2     DO WHILE (IREADSB(LUBFR).EQ.0)
 
C  CUT OUT ANY DATA OUTSIDE RR DOMAIN OR TIME WINDOW
C  -------------------------------------------------
 
      CALL UFBINT(LUBFR,HDR,5,1,IRET,'XOB YOB DHR')
      XOB = HDR(1)
      XOB = MOD(XOB+360.,360.)
      YOB = HDR(2)
      IF(X1.LE.L2) THEN
         IF(XOB.LT.X1.OR. XOB.GT.X2) GOTO 2
      ELSE
         IF(XOB.LT.X1.AND.XOB.GT.X2) GOTO 2
      ENDIF
      IF(YOB.LT.Y1.OR.YOB.GT.Y2) GOTO 2
 
      ADATE = IDATE
      DHR = HDR(3)
      CALL RADDATE(ADATE,DHR,BDATE)
      IF(BDATE.LT.CDATE.OR.BDATE.GT.DDATE) GOTO 2
 
C  REMOVE ANY SATWND MASS OBS
C  --------------------------
 
      IF(SUBSET.EQ.'SATWND') THEN
         CALL UFBINT(LUBFR,HDR,5,1,IRET,'TYP')
         IF(HDR(1).LT.200) GOTO 2
      ENDIF
 
C  WRITE THIS OB IN THE OUTPUT FILE
C  --------------------------------
 
      CALL OPENMB(LUOUT,SUBSET,JDATE)
 
C  CANT UFBCPY BECAUSE INPUT TABLES MAY DIFFER
C  -------------------------------------------
 
      ! STORE THE HEADER - ADJUST DHR TO REFLECT THREE HOUR SYNOPTIC TIME
 
      STR = 'SID XOB YOB DHR ELV TYP T29 ITP SQN PRG SRC RUD'
      CALL UFBINT(LUBFR,HDR,20,1,IRET,STR)
      DHR = HDR(4)
      IF(IDATE.GT.JDATE) DHR = DHR+3
      IF(IDATE.LT.JDATE) DHR = DHR-3
      HDR(4) = DHR
      CALL UFBINT(LUOUT,HDR,20,1,IRET,STR)
 
      ! SPECIAL FOR AIRCFT AND ADPSFC AND SFCSHP

!     changed 12-15-2008 RTC is replicated in new bufr style
!      STR = 'RCT TSB PMO PMQ'
!      CALL UFBINT(LUBFR,HDR,20,1,IRET,STR)
!      CALL UFBINT(LUOUT,HDR,20,1,IRET,STR)
      STR = 'TSB PMO PMQ'
      CALL UFBINT(LUBFR,HDR,20,1,IRET,STR)
      CALL UFBINT(LUOUT,HDR,20,1,IRET,STR)
      STR = 'RCT'
      CALL UFBINT(LUBFR,HDR,20,1,IRET,STR)
      CALL UFBINT(LUOUT,HDR,20,1,IRET,STR)
 
      !  STORE THE REGULAR LEVEL DATA
 
      DO I=1,5
      IF(I.EQ.1) STR = 'POB PQM PPC PRC CAT  '
      IF(I.EQ.2) STR = 'QOB QQM QPC QRC TDO  '
      IF(I.EQ.3) STR = 'TOB TQM TPC TRC TVO  '
      IF(I.EQ.4) STR = 'ZOB ZQM ZPC ZRC      '
      IF(I.EQ.5) STR = 'UOB WQM WPC WRC VOB  '
 
      CALL UFBEVN(LUBFR,ARR,10, 255,10,NLEV,STR)
      DO N=10,1,-1
      CALL UFBINT(LUOUT,ARR(1,1,N),10,NLEV,IRET,STR)
      ENDDO
 
      ENDDO
 
      !  WRITE THE REPORT OUT
 
      CALL WRITSB(LUOUT)
 
      ENDDO ! END OF READSB LOOP
      ENDDO ! END OF READMG LOOP
      ENDDO ! END OF LUBFR  LOOP
 
C  WHEN DONE FINISH UP THE OUTPUT FILE AND EXIT
C  --------------------------------------------
 
      CALL CLOSBF(LUOUT)
      STOP
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE RADDATE(ADATE,DHOUR,BDATE)
 
      DIMENSION   MON(12)
      REAL(8)     ADATE,BDATE
 
      DATA MON/31,28,31,30,31,30,31,31,30,31,30,31/
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
 
C  ONE WAY OR ANOTHER PARSE A TEN DIGIT DATE INTEGER
C  -------------------------------------------------
 
      KDATE = NINT(ADATE)
      IDATE = I4DY(KDATE)
      IY = MOD(IDATE/1000000,10000)
      IM = MOD(IDATE/10000  ,100  )
      ID = MOD(IDATE/100    ,100  )
      HR = MOD(ADATE        ,100._8 ) + DHOUR
      IF(MOD(IY    ,4).EQ.0) MON(2) = 29
      IF(MOD(IY/100,4).NE.0) MON(2) = 28
 
 
1     IF(HR.LT.0) THEN
         HR = HR+24
         ID = ID-1
         IF(ID.EQ.0) THEN
            IM = IM-1
            IF(IM.EQ.0) THEN
               IM = 12
               IY = IY-1
            ENDIF
            ID = MON(IM)
         ENDIF
         GOTO 1
      ELSEIF(HR.GE.24) THEN
         HR = HR-24
         ID = ID+1
         IF(ID.GT.MON(IM)) THEN
            ID = 1
            IM = IM+1
            IF(IM.GT.12) THEN
               IM = 1
               IY = IY+1
            ENDIF
         ENDIF
         GOTO 1
      ENDIF
 
      BDATE = IY*1000000 + IM*10000 + ID*100
      BDATE = BDATE + HR
 
      RETURN
      END
