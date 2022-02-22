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
C  14-07-10  EBISUZAKI V2 PROBLEMS IN WCOSS VERSION, TIME FOR CHANGES
C                      SO THAT I CAN FIGURE OUT CODE
C                      replace f95 code (where), ifort problem
C                       caused NDAS precip to be used
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
C     BAOPEN, BAOPENR
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
     +          X, Y, COUNT, DIST, NSTAGE4, 
     +          KGDSLDAS(200), GDS(200), LENGDS, LDASGRD, JRET
        INTEGER KPDS1(200), DATES(24), ii, icount
        INTEGER IP_NDAS, IPOPT_NDAS(20), NNDAS
C
        PARAMETER (NLDAS = 103936, NX = 464, NY = 224, BOUND=16,
     +             NSTAGE4 = 987601, NNDAS=185*129)
C       NLDAS grid lat-lon 1/8  x 1/8 degree conus
C
        REAL EDAS(NLDAS), S4IN(NSTAGE4), S4OUT(NLDAS), RLAT(NLDAS),
     +       RLON(NLDAS), S42DTEMP(NX,NY), S42D(NX,NY),
     +       S4(24,NLDAS), LSUM(NLDAS), WEIGHT(24,NLDAS), CPC(NLDAS),
     +       HOURPREC(NLDAS), NDAS(NNDAS)
        real box_precip(24), rweight, rtmp, rdist
        integer box_count, ix, ix0, ix1, ixx, iy, iy0, iy1, iyy, n_fixed
C
        LOGICAL*1 OCEANMASK(NLDAS), LB(NSTAGE4), LO(NLDAS),
     +            MASKTEMP(NX,NY), NDASMASK(NNDAS), EDASMASK(NLDAS)
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

        IP_NDAS = 0
        IPOPT_NDAS = 0

C       READ OCEAN MASK
        LUGB = 9 
        WRITE (CLUGB, '(i1.1)') LUGB
        FILENAM = 'fort.'//CLUGB
        CALL BAOPENR(LUGB, FILENAM, IRET)
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
C makgds: in iplib
C  LDASGRD=110, make KGDSLDAS and GDS using ncep grid 110
C  ncep grid 110 = NLDAS grid  (NLDAS = 103936, NX = 464, NY = 224
C
        CALL MAKGDS (LDASGRD,KGDSLDAS,GDS,LENGDS,IRET)
        if (iret.ne.0) then
           write(*,*) 'error in makgds()'
           stop 8
        endif
C
C  READ IN 24 DATE CODES (INCREASING BY 1 HOUR)
C
        READ(5,'(i10)') DATES
C
C  READ IN PRECIP GRID FROM EDAS-BASED FORCING FILE and STAGE IV
C
C  First read EDAS (NDAS) file
C  Then the stage iv
C
        DO I = 1, 24
          if (mod(i-1,3) .eq. 0) then
C             read 3-hr accum NDAS precip, interpolate, divide by 3 to get hourly
              LUGB = 10+I
              WRITE (CLUGB, '(i2.2)') LUGB
              FILENAM = 'fort.'//CLUGB
              CALL BAOPENR(LUGB, FILENAM, IRET)
              CALL GETGB(LUGB,LUGI,NNDAS,J,JPDS,JGDS,KF,K,KPDS,KGDSEDAS,
     +                    NDASMASK,NDAS,IRET)
              if (iret.ne.0) then
                 edas = 0.0
                 print *, 'missing ndas i=',i, ' using zero precip'
                 write(*,*) 'ERROR- NDAS is missing, INFORM CPC'
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
                 do ii = 1, NLDAS
C                    EDAS is 0-3 hour acc APCP, convert to 1 hourly amount
                     if (EDAS(ii) < 0.0) then
                         EDAS(ii) = 0.0
                     else
                         EDAS(ii) = EDAS(ii) / 3.0
                     endif
                 enddo
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
          FILENAM = 'fort.'//CLUGB
          CALL BAOPENR(LUGB, FILENAM, IRET)
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
C  PUT 1D ARRAYS OF DATA ONTO 2D LDAS GRID (FOR NEIGHBOR SEARCHING 
C  ALGORITHM BELOW).
C
             COUNT = 0
             DO Y = 1, NY
                 DO X = 1, NX
                     S42DTEMP(X,Y) = S4OUT(X+COUNT)
                     S42D(X,Y) = EDAS(X+COUNT)
                     MASKTEMP(X,Y) = LO(X+COUNT)
                 END DO
                 COUNT = COUNT + NX
             END DO
C
C  REPLACE MISSING DATA POINTS WITH NEAREST NEIGHBOR IF NEAREST NEIGHBOR
C  WITHIN ABOUT 240 KM ("BOUND").  ELSE, REPLACE WITH EDAS VALUE.
C
             do y = 1, ny
                do x = 1, nx
                   IF (MASKTEMP(X,Y)) THEN
                      S42D(X,Y) = S42DTEMP(X,Y)
                   else
                      do dist = 1, BOUND
                         ix0 = max0(1,x-dist)
                         iy0 = max0(1,y-dist)
                         ix1 = min0(nx,x+dist)
                         iy1 = min0(ny,y+dist)
                         rdist = 1.1e20
                         do ix = ix0, ix1
                            if (MASKTEMP(ix,iy0)) then
                               rtmp = (ix-x)**2 + (iy0-y)**2
                               if (rtmp < rdist) then
                                   rdist = rtmp
                                   S42D(x,y) = S42DTEMP(ix,iy0)
                               endif
                            endif
                            if (MASKTEMP(ix,iy1)) then
                               rtmp = (ix-x)**2 + (iy1-y)**2
                               if (rtmp < rdist) then
                                   rdist = rtmp
                                   S42D(x,y) = S42DTEMP(ix,iy1)
                               endif
                            endif
                         enddo
                         do iy = iy0, iy1
                            if (MASKTEMP(ix0,iy)) then
                               rtmp = (ix0-x)**2 + (iy-y)**2
                               if (rtmp < rdist) then
                                   rdist = rtmp
                                   S42D(x,y) = S42DTEMP(ix0,iy)
                               endif
                            endif
                            if (MASKTEMP(ix1,iy)) then
                               rtmp = (ix1-x)**2 + (iy-y)**2
                               if (rtmp < rdist) then
                                   rdist = rtmp
                                   S42D(x,y) = S42DTEMP(ix1,iy)
                               endif
                            endif
                         enddo
                         if (rdist < 1e20) goto 1000
                      enddo
1000                  continue
                   endif
                enddo
             enddo

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
        do i = 1, nldas
            lsum(i) = sum(s4(:,i))
        enddo
        print*, 'sums complete'

C  Use this call read the daily precip data

        OPEN(8, FORM = 'UNFORMATTED', STATUS = 'OLD', ERR=2010)  
        READ(8,ERR=2000) (CPC(I), I = 1, NLDAS) 
        close(8)

C       set wgrib undefined to local undefined and scale
C       from mm/sec to mm/day
        icount = 0
        do i = 1, nldas
            if (CPC(I) > 9.998e20 .and. CPC(i) < 9.9999e20) then
               icount = icount + 1
               cpc(i) = lsum(i)
            else if (cpc(i) > 0.0) then
               cpc(i) = cpc(i) * 86400.0
            else
               icount = icount + 1
               cpc(i) = lsum(i)
            endif
        enddo
        write(*,*) 'daily analysis read, undefined=', icount, nldas
        goto 2020

C If CPC missing using STAGE IV or EDAS data 
C Helin Wei 03/26/2003

2000    continue
        write(*,*) 'ERROR: open unit 8, daily precip'
        cpc = lsum
        goto 2020
        close(8)
2010    continue
        write(*,*) 'ERROR: read unit 8, daily precip'
        cpc = lsum
2020    continue


C
C  COMPUTE WEIGHT FOR EACH HOUR.  WATCH OUT FOR SINGULARITY!  FIND SUM
C  OF HOURLY WEIGHTS.  SHOULD ALWAYS BE ZERO OR ONE!  
C
        do i = 1, nldas
            if (lsum(i).eq.0.0) then
                weight(:,i) = 0.0
            else
                weight(:,i) = s4(:,i) / lsum(i)
            endif
        enddo
        print*, 'got weights for points with stage iv precip'

C at this point, s4(ihr, nldas) == hourly precip,
C                    defined on all points (EDAS used to fill)
C                lsum(nldas) == sum of hourly precip
C                weight(ihr,nldas) == s4 / lsum  or 0 if lsum == 0
C                cpc == daily precip
C                    defined on all points (EDAS used to fill)
C
C only problem is if have daily precip and no data for hourly weights
C original program used nearest neighbor which had non-zero hourly weights
C         i.e. lsum != 0
C   pain to program
C
C comment 1: the odds of having lsum == 0 and cpc != 0 is small
C            So it is not worthwhile spending too much time reproducing
C            original version exactly
C comment 2: going to use a weighted box average of non-zero s4
C comment 3: this version will not be sensitive to search path of
C            nearest neighbor
C comment 4: if only one s4 != 0 in the box, same results as nearest neighbor
C comment 5: If use high order weighting (1/r**N), then weighted ave
C
C v2 7/2014 use box average precip
C   
        n_fixed = 0
        do i = 1, nldas
            if (cpc(i).gt.0.0 .and. lsum(i).eq.0.0) then
C               ix, iy are relative to 0
                ix = mod(i-1, nx)
                iy = (i-1) / nx
                ix0 = max0(0, ix-bound)
                ix1 = min0(nx-1, ix+bound)
                iy0 = max0(0, iy-bound)
                iy1 = min0(ny-1, iy+bound)
                box_precip(:) = 0.0
                box_count = 0
                do iyy = iy0, iy1
                    do ixx = ix0, ix1
                        ii = 1 + ixx + iyy*nx
                        if (lsum(ii) /= 0.0) then
                            box_count = box_count + 1
                            rweight = 1.0 / ((ixx-ix)**2 + (iyy-iy)**2)
                            box_precip(:) = box_precip(:) + 
     1                          rweight*s4(:,ii)
                        endif
                    enddo
                enddo
                n_fixed = n_fixed + 1
                if (box_count .eq. 0) then
                    weight(:, i) = 1.0 / 24.0
                else
                    weight(:,i) = box_precip(:)/sum(box_precip(:))
                endif
                write(*,*) 'v2: cpc > 0 but lsum = 0, box_count=', 
     `                  box_count
            endif
        enddo
        write(*,*) 'num cpc>0, lsum=0 ',n_fixed

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
          FILENAM = 'fort.'//CLUGB
          write(*,*) 'filename=',FILENAM(1:7)

          KPDS(2) = 155
          CALL BAOPEN(LUGB, FILENAM, IRET)
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