C***********************************************************************
C  PROGRAM:  PRECFORCE              MAKE PRECIP FORCING FIELDS FOR LDAS 
C  PRGMMR:  MARSHALL                ORG:  W/NP20
C
C  ABSTRACT:  MERGE STAGE IV HOURLY PRECIP DATA (MULTISENSOR ANALYSES) 
C  AND CPC DAILY TOTAL PRECIP (GAUGE ONLY) TO GENERATE HOURLY PRECIP
C  FORCING FIELDS TO BE USED FOR FORCING THE LDAS.  HOURLY STAGE IV
C  ESTIMATES ARE SUMMED OVER THE 24 HOUR PERIOD OF THE CPC ANALYSIS.  
C  WEIGHTS ARE DERIVED FOR EACH HOUR BY FINDING THE FRACTION OF 24 H
C  SUM OVER ALL HOURS. THESE HOURLY WEIGHTS, DERIVED ONLY FROM THE STAGE
C  IV, ARE THEN MULTIPLIED BY THE CPC DAILY TOTAL TO "SPLIT OUR" HOURLY
C  PRECIPITATION AMOUNTS.  NOTE THAT WHEN A STAGE IV DATA POINT IS    
C  MISSING, IT IS REPLACED WITH ITS NEAREST NEIGHBOR, OUT TO 240 KM.  
C  BEYOND THAT RANGE, EDAS HOURLY PRECIPS ARE USED.  PRACTICALLY, THIS
C  MEANS AREAS OUTSIDE THE EDGE OF THE STAGE IV DOMAIN, BUT INSIDE THE 
C  LDAS DOMAIN, ARE DOMINATED BY EDAS IN THE DERIVATION OF THE WEIGHTS.  
C
C  PROGRAM HISTORY LOG:
C  99-01-21  MARSHALL  ORIGINAL CODING
C  99-11-16  MARSHALL  ADDED STAGE IV ON LDAS GRID TO OUTPUT FILES
C  02-02-01  LOHMANN   CHANGED TO PRIZM BASED PRECIP
C  02-03-18  LOHMANN   CHANGED GRID on STAGE IV
C  04-07-06  EBISUZAKI CONVERSION FOR NARR, USE 3 HOURLY NDAS FILES, 
C                      BUG FIXES: INITIALISE UNDEF ARRAYS, 1/24 -> 1.0/24.0
C
C  INPUT FILES
C
C    UNIT5  DATE CODES
C    UNIT09 OCEANMASK.GRB  
C    UNIT11 NDAS 12Z 
C    UNIT14 NDAS 15Z
C    UNIT17 NDAS 18Z
C    UNIT20 NDAS 21Z
C    UNIT23 NDAS 00Z (NEXT DAY)
C    UNIT26 NDAS 03Z (NEXT DAY)
C    UNIT29 NDAS 06Z (NEXT DAY)
C    UNIT31 NDAS 09Z (NEXT DAY)
C    UNIT35-58 STAGE IV PRECIP 
C    UNIT08 DAILY GAUGE PRECIP FOR CONUS (BINARY)
C
C  OUTPUT FILES
C
C    UNIT60-83 GRIB CONUS PRECIP
C
C  SUBPROGRAMS CALLED:
C     MAKGDS
C     BAOPEN
C     BACLOSE
C     GETGB
C     PUTGB
C     IPOLATES
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN 90
C$$$
C***********************************************************************
C
C  DECLARATIONS
C
	IMPLICIT NONE
C
	INTEGER NLDAS, NX, NY, BOUND, JPDS(200), IP, IPOPT(20), I, N, 
     +          LUGB, IRET, LUGI, J, JGDS(200), KF, K, KPDS(200), KM,
     +          KGDSEDAS(200), KGDSS4(200), IBI, IBO, NO, 
     +          X, Y, COUNT, DIST, XX, YY, NSTAGE4,
     +          KGDSLDAS(200), GDS(200), LENGDS, LDASGRD, JRET
        INTEGER KPDS1(200), DATES(24), ii
        INTEGER IP_NDAS, IPOPT_NDAS(20), NNDAS
C
 	PARAMETER (NLDAS = 103936, NX = 464, NY = 224, BOUND=16,
     +             NSTAGE4 = 987601, NNDAS=185*129)
C
	REAL EDAS(NLDAS), S4IN(NSTAGE4), S4OUT(NLDAS), RLAT(NLDAS),
     +       RLON(NLDAS), S42DTEMP(NX,NY), EDAS2D(NX,NY), S42D(NX,NY),
     +       S4(24,NLDAS), LSUM(NLDAS), WEIGHT(24,NLDAS), CPC(NLDAS),
     +       HOURPREC(NLDAS), WEIGHTSUM(NLDAS),
     +       CPCTMP(NX,NY), WTSUMTMP(NX,NY), WT2DTMP(24,NX,NY),
     +       WT2D(24,NX,NY), NDAS(NNDAS)
C
	LOGICAL*1 OCEANMASK(NLDAS), LB(NSTAGE4), LO(NLDAS),
     +            MASKTEMP(NX,NY), FOUND, 
     +            NDASMASK(NNDAS), EDASMASK(NLDAS)
	LOGICAL S4AVAIL(24)
C
	CHARACTER  FILENAM*255, CLUGB*2
C
C  SOME CONSTANTS AND SUCH
C
	J = 0
	JPDS = -1
	JPDS(5) = 61
        IP = 3
	IPOPT(1) = 4
	IPOPT(2) = -1
        LDASGRD = 110
        IBI = 1
        KM = 1

        S4AVAIL = .TRUE.
        LSUM = 0.0
        WEIGHTSUM = 0.0

        IP_NDAS = 0
        IPOPT_NDAS = 0

C       READ OCEAN MASK
        LUGB = 9 
        WRITE (CLUGB, '(i1.1)') LUGB
        FILENAM = 'fort.'//CLUGB//char(0)
        CALL BAOPEN (LUGB, FILENAM, IRET)
        CALL GETGB(LUGB,LUGI,NLDAS,J,JPDS,JGDS,KF,K,KPDS,
     +        KGDSEDAS,OCEANMASK,EDAS,IRET)
        if (iret.ne.0) then
           write(*,*) 'error in reading mask file'
           oceanmask = .true.
           stop 8
        endif

C       SAVE PDS INFO FOR USE IN OUTPUT FILES.  
        KPDS1 = KPDS
        CALL BACLOSE(LUGB,IRET)
C
C  MAKE A GDS FOR THE OUTPUT GRID
C
	CALL MAKGDS (LDASGRD,KGDSLDAS,GDS,LENGDS,IRET)
C
C  READ IN 24 DATE CODES (INCREASING BY 1 HOUR)
C
        READ(5,'(i10)') DATES
C
C  READ IN PRECIP GRID FROM EDAS-BASED FORCING FILE and STAGE IV
C
	DO I = 1, 24
          if (mod(i-1,3) .eq. 0) then
C             read 3-hr accum NDAS precip, interpolate, divide by 3 to get hourly
              LUGB = 10+I
              WRITE (CLUGB, '(i2.2)') LUGB
              FILENAM = 'fort.'//CLUGB//char(0)
              CALL BAOPEN (LUGB, FILENAM, IRET)
              CALL GETGB(LUGB,LUGI,NNDAS,J,JPDS,JGDS,KF,K,KPDS,KGDSEDAS,
     +                    NDASMASK,NDAS,IRET)
              if (iret.ne.0) then
                 edas = 0.0
                 print*, 'missing ndas i=',i, ' using zero precip'
              else
                 print*, 'got ndas i=',i,dates(i),kpds(8:11)
C                INTERPOLATE
                 CALL IPOLATES (IP_NDAS,IPOPT_NDAS,KGDSEDAS,KGDSLDAS,
     +             NNDAS,NLDAS,KM,IBI,NDASMASK,NDAS,NO,RLAT,RLON,IBO,
     +             EDASMASK,EDAS,IRET)
                 if (iret.ne.0) then
                   write(*,*) 'interpolation ndas -> nldas grid failed'
                   stop 8
                 endif
                 where (edas < 0.0) edas = 0.0
                 edas = edas / 3.0
              endif
	      CALL BACLOSE (LUGB, JRET)
          endif

C
C   READ IN THE STAGE IV GRID AND INTERPOLATE TO LDAS GRID.  IF ENTIRE
C   STAGE IV GRID IS MISSING (I.E., THE HOURLY FILE IS MISSING) THEN
C   REPLACE GRID WITH EDAS GRID.  ELSE, PROCEED AND FILL HOLES IN 
C   STAGE IV GRID WITH EDAS VALUES BELOW.
C
	  LUGB = 34+I
          WRITE (CLUGB, '(i2.2)') LUGB
          FILENAM = 'fort.'//CLUGB//char(0)
          CALL BAOPEN (LUGB, FILENAM, IRET)
          CALL GETGB (LUGB,LUGI,NSTAGE4,J,JPDS,JGDS,KF,K,KPDS,KGDSS4,
     +                LB,S4IN,IRET)
          IF (IRET.NE.0) THEN
             S4AVAIL(I) = .FALSE.
             print *, 'no stage4, i=',i,' IRET=', IRET, dates(i)
             S4(i,:) = EDAS
          ELSE 
	     print*, 'got stage4 i=',i, dates(i),kpds(8:11)
             CALL IPOLATES (IP,IPOPT,KGDSS4,KGDSLDAS,NSTAGE4,NLDAS,KM,
     +                   IBI,LB,S4IN,NO,RLAT,RLON,IBO,LO,S4OUT,IRET)
          END IF
          CALL BACLOSE (LUGB,JRET)
C
C  SAVE S4OUT AND MASK FOR LATER OUTPUT IF STAGE IV AVAILABLE.
C
C-------------compute s4 .. holed filled data ---
          IF (S4AVAIL(I)) THEN
C
C  PUT 1D ARRAYS OF DATA ONTO 2D LDAS GRID (FOR NEIGHBOR SEARCHING 
C  ALGORITHM BELOW).
C
          COUNT = 0
          DO Y = 1, NY
            DO X = 1, NX
              S42DTEMP(X,Y) = S4OUT(X+COUNT)
              EDAS2D(X,Y) = EDAS(X+COUNT)
              MASKTEMP(X,Y) = LO(X+COUNT)
            END DO
            COUNT = COUNT + NX
          END DO
C
C  REPLACE MISSING DATA POINTS WITH NEAREST NEIGHBOR IF NEAREST NEIGHBOR
C  WITHIN ABOUT 240 KM ("BOUND").  ELSE, REPLACE WITH EDAS VALUE.
C 
	  DO Y = 1, NY
            DO X = 1, NX
              FOUND = .TRUE.
	      IF (.NOT. MASKTEMP(X,Y)) THEN
                FOUND = .FALSE.
                IF ((X.LE.(NX-BOUND)).AND.(X.GT.BOUND).AND.   
     +              (Y.LE.(NY-BOUND)).AND.(Y.GT.BOUND)) THEN
                  DIST = 1
                  DO WHILE ((DIST .LE. BOUND).AND.(.NOT. FOUND))    
                    YY = -1*DIST
                    DO WHILE ((YY .LE. DIST).AND.(.NOT. FOUND))
                      XX = -1*DIST
                      DO WHILE ((XX .LE. DIST).AND.(.NOT. FOUND))
	                IF (MASKTEMP(X+XX,Y+YY)) THEN 
	                  S42D(X,Y) = S42DTEMP(X+XX,Y+YY)
	                  FOUND = .TRUE.
	                END IF
	                XX = XX+1
	              END DO
                      YY = YY+1
	            END DO
                  DIST = DIST+1
	          END DO
                ELSE
                  S42D(X,Y)=EDAS2D(X,Y)        
                  FOUND = .TRUE.
                END IF
              ELSE
	        S42D(X,Y)=S42DTEMP(X,Y)	
	      END IF
              IF (.NOT. FOUND) THEN
                S42D(X,Y)=EDAS2D(X,Y)
              END IF
            END DO
          END DO
C
C  WRITE THE HOLE-FILLED GRID BACK OUT INTO A 1D ARRAY
C
          count = 0
          do y = 1, ny
             s4(i,count+1:count+nx) = s42d(:,y)
	     count = count+nx
          enddo

          END IF
	END DO
C-------------------------------------------------
C
C  SUM HOURLY STAGE IV PRECIPS (WITH NO MISSING DATA) OVER 24H PERIOD
C
	DO I = 1, 24
	  LSUM = LSUM + S4(I,:)
	END DO
        print*, 'sums complete'

C  Use this call for the the PRISM corrected daily precip data
C  Dag Lohmann 02/01/2002

        CPC = -9999.0
	OPEN(8, FORM = 'UNFORMATTED', STATUS = 'OLD', ERR=1000)  
	READ(8,ERR=1000) (CPC(I), I = 1, NLDAS) 
        close(8)

C	set wgrib undefined to local undefined and scale
C	from mm/sec to mm/day
	do i = 1, nldas
	    if (CPC(I) > 9.998e20 .and. CPC(i) < 9.9999e20) then
		CPC = -9999.0
	    else if (cpc(i) > 0.0) then
		cpc(i) = cpc(i) * 86400
	    else
		cpc(i) = -9999.0
	    endif
	enddo


C If CPC missing using STAGE IV or EDAS data 
C Helin Wei 03/26/2003
1000    continue
	where (cpc < 0.0) cpc = lsum
C
C  COMPUTE WEIGHT FOR EACH HOUR.  WATCH OUT FOR SINGULARITY!  FIND SUM
C  OF HOURLY WEIGHTS.  SHOULD ALWAYS BE ZERO OR ONE!  
C
	DO I = 1, 24
          DO N = 1, NLDAS
            WEIGHT(I,N) = 0
            IF (LSUM(N).GT.0) WEIGHT(I,N) = S4(I,N)/LSUM(N)
            WEIGHTSUM(N)=WEIGHTSUM(N)+WEIGHT(I,N)
          END DO
 	END DO
        print*, 'got weights'
C
C  PUT WEIGHTS AND WEIGHTSUM ON 2D ARRAYS FOR SEARCHING ALGORITHM BELOW.
C
        COUNT = 0
        DO Y = 1, NY
          DO X = 1, NX
            CPCTMP(X,Y) = CPC(X+COUNT)
            WTSUMTMP(X,Y) = WEIGHTSUM(X+COUNT)
            DO I = 1, 24
              WT2DTMP(I,X,Y)=WEIGHT(I,X+COUNT)
            END DO
          END DO
          COUNT = COUNT + NX
        END DO
C
C  SEARCH FOR OCCURRENCES WHERE WEIGHTSUM IS ZERO, BUT CPC PRECIP IS
C  NON-ZERO.  IN THESE INSTANCES, TAKE HOURLY WEIGHTS FROM NEAREST NEIGHBOR.
C  IN THIS MANNER, THE SUM OF THE 24 HOURLY PRECIPS TO BE DERIVED BELOW
C  WILL ALWAYS EQUAL THE CPC 24H ANALYSIS AT ALL GRID POINTS.  
C
	DO Y = 1, NY
          DO X = 1, NX
            FOUND = .TRUE.
            IF ((WTSUMTMP(X,Y).EQ.0).AND.(CPCTMP(X,Y).NE.0)) THEN
              FOUND = .FALSE.
              IF ((X.LE.(NX-BOUND)).AND.(X.GT.BOUND).AND.   
     +            (Y.LE.(NY-BOUND)).AND.(Y.GT.BOUND)) THEN
                DIST = 1
                DO WHILE ((DIST .LE. BOUND).AND.(.NOT. FOUND))    
                  YY = -1*DIST
                  DO WHILE ((YY .LE. DIST).AND.(.NOT. FOUND))
                    XX = -1*DIST
                    DO WHILE ((XX .LE. DIST).AND.(.NOT. FOUND))
	              IF (WTSUMTMP(X+XX,Y+YY).NE.0) THEN 
                        DO I = 1, 24
	                  WT2D(I,X,Y) = WT2DTMP(I,X+XX,Y+YY)
	                END DO
	                FOUND = .TRUE.
	              END IF
	              XX = XX+1
	            END DO
                    YY = YY+1
	          END DO
                  DIST = DIST+1
	        END DO
              ELSE
                DO I = 1, 24
                  WT2D(I,X,Y)=1.0/24.0
                END DO
                FOUND = .TRUE.
              END IF
            ELSE
              DO I = 1, 24
                WT2D(I,X,Y)=WT2DTMP(I,X,Y)
              END DO
	    END IF
            IF (.NOT. FOUND) THEN
              DO I = 1, 24
                WT2D(I,X,Y) = 1.0/24.0
              END DO 
            END IF
          END DO
        END DO
C
C  NOW WRITE WEIGHTS BACK OUT INTO 1D ARRAY
C
        COUNT = 0
        DO Y = 1, NY
          DO X = 1, NX
            DO I = 1, 24
              WEIGHT(I,X+COUNT)=WT2D(I,X,Y)
            END DO
          END DO
          COUNT = COUNT + NX
        END DO
        print*, 'done checking weights'
C
C  COMPUTE HOURLY PRECIPS FROM WEIGHTS AND CPC TOTAL
C
	DO I = 1, 24
          DO N = 1, NLDAS
	    HOURPREC(N)=WEIGHT(I,N)*CPC(N)
          END DO 
C 
C  GET PDS INFO, OPEN FILE AND WRITE DATA TO HOURLY GRIB FILES.  
C 
          LUGB = 59+I 
          KPDS = KPDS1

C         SET DATE CODE
          II = DATES(I)
          KPDS(11) = MOD(II,100)
          II = II / 100
          KPDS(10) = MOD(II,100)
          II = II / 100
          KPDS(9) = MOD(II,100)
          II = II / 100
          KPDS(8) = MOD(II,100)
          II = II / 100
          if (KPDS(8).eq.0) then
             KPDS(8) = 100
             II = II - 1
          ENDIF
C         CENTURY
          KPDS(21) = II + 1

C         APCP
          KPDS(5) = 61
          KPDS(6) = 1
          KPDS(7) = 0
C         0-1 hour accumulation
          KPDS(13) = 1
          KPDS(14) = 0
          KPDS(15) = 1
          KPDS(16) = 4
C         grid 110
          KPDS(3) = 110
          WRITE (CLUGB, '(i2.2)') LUGB
          FILENAM = 'fort.'//CLUGB//char(0)
          write(*,*) 'filename=',FILENAM(1:7)

          KPDS(2) = 155
          CALL BAOPEN (LUGB, FILENAM, IRET)
          CALL PUTGB (LUGB,NLDAS,KPDS,KGDSLDAS,OCEANMASK,HOURPREC,IRET)
          IF (IRET .NE. 0) THEN
            PRINT*, 'PUTGB NLDAS PREC FAILED, IRET=', IRET
          END IF
C
          CALL BACLOSE(LUGB, JRET)
          if (JRET.ne.0) write(*,*) 'BACLOSE LUGB,JRET=',LUGB,JRET
	END DO
C
	STOP
	END
