C********************************************************************
C  GRIDTOBS   -  CREATE VERIFICATION STATS BETWEEN OBSERVATIONS AND
C                BACKGROUNDS.  THE BACKGROUND & OBS VALUES ARE READ
C                FROM A PREPBTIM (GLOBAL) OR PREPFITS (MESO) BUFR FILE
C
      PROGRAM gridtobs

      INCLUDE 'parm.inc'

      real*8 sumdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            sumgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            sumprod(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            ssqdata(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            ssqgrid(mxfcst,mxvrbl,maxlvl,mxarea,maxobs), 
     +            count(mxfcst,mxvrbl,maxlvl,mxarea,maxobs)
      real*8 sumd, sumg, sump, ssqd, ssqg

      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)

      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      LOGICAL	  vtflg, nmbflg
      COMMON /cnvrsns/ vtflg, nmbflg (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
      CHARACTER*3 regions (29)
      COMMON /grdef/ mode(mxarea), imax(mxarea), imin(mxarea), 
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea), 
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea), 
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea), 
     +            lambert(mxarea), polarstereo(mxarea), numreg(mxarea),
     +            ig104(147,110), regions

      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)

      CHARACTER*1 blank, equal
C
      CHARACTER*50 headr, obstr, qmstr
      CHARACTER*8 subset, stnid
C
      INTEGER	lstart (maxobs), lstop (maxobs)
      REAL*8 hdr(10), obs(12,255,mxb), qms(8,255)
      real*8 dhr,probs
      EQUIVALENCE (hdr(1),stnid)
C
C...   STRING FOR HEADER PARAMETERS
C
      DATA headr /'SID XOB YOB DHR ELV TYP T29 ITP'/
C
C...   STRING FOR THE OB, GUESS, ANALYSIS ....
C
      DATA obstr /'SRC FHR POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI'/
C
C...   STRING FOR THE QUALITY MARKS ....
C
      DATA qmstr /'CAT PRC PQM QQM TQM ZQM WQM    '/
      DATA stnid /'        '/
C----------------------------------------------------
      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
C
      DATA bmiss /10E10/
      DATA rmiss /99999./
   10 CONTINUE
C     
C     READ VERIFICATION DATABASE VERSION AND INPUT UNIT NUMBER
C
C     Also read in logical flag (T or F) to convert virtual temperature
C     observed into actual temperature.
C     
      READ (5,'(A)',END=160) input
      CALL ST_CLST ( input, ' ', ' ', 3, substr, num, ier )
      namversion = substr (1)
      CALL ST_NUMB ( substr (2), lunin, ier )
      vtflg = ( substr (3) .eq. 't' .or. substr (3) .eq. 'T' )
      PRINT '(A3,I5)', namversion, lunin
      print*,'iy,im,id,ih=',iy,im,id,ih
      CALL datebf(lunin,iy,im,id,ih,idate)
      print*,'idate=',idate
      CALL openbf(lunin,'IN ',lunin)
      PRINT *, ' DATE OF INPUT BUFR FILE', idate, ' UNIT=', lunin
C     
C     READ REST OF THIS CONTROL-FILE GROUP
C     TO GET NUMBER OF THINGS TO BE VERIFIED
C     
      CALL readcntl(numodel,numfcst,numvfdate,numvfyobs,numarea,numstat,
     +            numvarbl,numlevel,numvector)
      print*,'numvarbl,namvarbl=',numvarbl,namvarbl

      print*,'numvfdate,nchrvfdate(1)=',numvfdate,nchrvfdate(1)
      IF (numvfdate.eq.1.and.nchrvfdate(1).eq.2) THEN
        WRITE (namvfdate(1)(3:10),'(I8)') idate
        nchrvfdate(1) = 10
        PRINT '(" NEW NAMVFDATE =",A24)', namvfdate(1)
        PRINT '(" CHARACTER COUNT =",I5)', nchrvfdate(1)
      END IF
C     
C     LET'S DO THE LOOPS
C     
C     OUTERMOST LOOP OVER VERIFYING DATE
C     
c     print*,'numvfdate=',numvfdate
      DO 150 ivfdate = 1, numvfdate
C       
C       OUTER LOOP OVER FORECAST MODEL TO BE VERIFIED
C       
c       print*,'numodel=',numodel
        DO 140 imodel = 1, numodel
C         
C         ZERO THE SUMS
C         
          count = 0.0
          sumdata = 0.0
          ssqdata = 0.0
          sumgrid = 0.0
          ssqgrid = 0.0
          sumprod = 0.0
C         
C         HERE WE GET A BUFR FILE AND START LOOP OVER EACH OB
C         
          DO WHILE (ireadmg(lunin,subset,jdate).eq.0)
C           nsub = nmsub(lunin)
C           PRINT *, 'SUBSET,NSUB,JDATE', subset, nsub, jdate
            ichk = 0
c           print*,'numvfyobs=',numvfyobs
            DO 20 iob = 1, numvfyobs
	      lstart (iob) = 1
	      lstop (iob) = 0
              IF (namvfyobs(iob)(:6).eq.'ANYSFC') THEN
                IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP'.or.
     +                      subset(:6).eq.'ADPUPA'.or.subset(:6).eq.
     +                      'PROFLR') THEN
			    ichk = 1
			    IF (subset(:6).eq.'ADPSFC'.or.
     +			        subset(:6).eq.'SFCSHP') lstop (iob) = 1
		END IF
              ELSE IF (namvfyobs(iob)(:6).eq.'ANYAIR') THEN
                IF (subset(:6).eq.'AIRCAR'.or.subset(:6).eq.'AIRCFT') 
     +                      ichk = 1
	      ELSE IF (namvfyobs(iob)(:6).eq.'ONLYSF') THEN
                IF (subset(:6).eq.'ADPSFC'.or.subset(:6).eq.'SFCSHP')
     +		THEN
	            ichk = 1
		    lstart (iob) = 2
		END IF
              ELSE IF (subset(:6).eq.namvfyobs(iob)(:6)) THEN
                ichk = 1
		IF (subset(:6).eq.'ADPSFC'.or.
     +		    subset(:6).eq.'SFCSHP') lstop (iob) = 1
              END IF
   20       CONTINUE
            DO WHILE (ireadsb(lunin).eq.0.and.ichk.eq.1)
              CALL ufbint(lunin,hdr,10,1,nlev,headr)
c             PRINT *, 'NLEV,HDR', nlev, hdr
              dhr = hdr(4)
              adate = jdate
              CALL raddate(adate,dhr,vdata)
C             
C             INNER LOOP FOR ACTUAL OB-TYPE TO BE VERIFIED
C             
              DO 80 iob = 1, numvfyobs
                IF (isitob(subset,hdr(6),iob).eq.0) THEN
C                 
C                 INNER LOOP OVER VERIFYING AREA
C                 
c                 print*,'numarea=',numarea
                  DO 70 iar = 1, numarea
c                   print*,'iar,namarea(iar)=',iar,namarea(iar)
c                   if(iar.eq.16) then
c       if(inarea(imodel,stnid,hdr(2),hdr(3),iar,rm1,rm2).eq.0) then
c       print*,'hdr(2),hdr(3)=',hdr(2),hdr(3)
c       print*,'inarea=',inarea(imodel,stnid,hdr(2),hdr(3),iar,rm1,rm2)
c                   endif
                    IF (inarea(imodel,stnid,hdr(2),hdr(3),iar,rm1,rm2)
     +			.eq.0) THEN
                      CALL ufbin3(lunin,obs,12,255,mxb,nlev,nevn,obstr)
c                     if (iar.eq.16) then
c                       k=1
c                       do j=1,50
c                       do i=1,12
c                        print*,'i,obs(i,j,1)=',i,obs(i,j,1)
c                       enddo
c                       enddo
c                       if (nint(obs(3,j,k)).eq.500) then
c                        print*,'obs(6,j,k)=',obs(6,j,k)
c                       endif
c                       enddo
c                       if (obs(4,j,k).ne.bmiss) then
c                       print*,'j,k,obs(4,j,k)=',j,k,obs(4,j,k)
c                       endif
c                       enddo
c                     endif
c                     print*,'nevn=',nevn
                      CALL ufbint(lunin,qms,8,255,nlev,qmstr)
C                     
C                     INNER LOOP OVER OBSERVATION LEVELS
C                     
		      IF ( lstop (iob) .eq. 0 ) THEN
			nstop = nlev
		      ELSE
			nstop = lstop (iob)
		      END IF
c      IF (namvfyobs(iob)(:6).eq.'ONLYSF') then
c         print*,'nlev,nstop=',nlev,nstop
c      endif
                      DO 60 nlv = lstart (iob), nstop
c                       print*,'nlv,nevn=',nlv,nevn
                        probs = obs(3,nlv,nevn)
c                       prbak = obs(3,nlv,1) 
c                       print*,'probs,prbak=',probs,prbak
c        if(subset(:6).eq.'ADPUPA') then
c        if(abs(obs(5,nlv,1)-obs(5,nlv,2)).gt.20.0) then
c        if(nint(probs).eq.850) then
c        if(stnid.eq.'UWEC ') then
c        print*,'stnid,hdr(2),hdr(3)=',stnid,hdr(2),hdr(3)
c        print*,'stnid,probs,fcs,obs=',stnid,probs,obs(5,nlv,1),
c    *      obs(5,nlv,2)
c        endif
c        endif
c        endif
                        kat = nint(qms(1,nlv))
C                       INNER LOOP OVER REQUESTED LEVELS
                        DO 50 ilv = 1, numlevel
c                         print*,'subset,probs,kat,ilv=',
c    *                       subset,probs,kat,ilv
                          IF (inlayer(subset,probs,kat,ilv).eq.0) THEN
C                           
C                           INNER LOOP OVER VARIABLE 
C                           
                            DO 40 ivr = 1, numvarbl
C                             
C                             INNER LOOP OVER NUMBER OF FORECAST HOUR (EVENT)
C                             
                              DO 30 ifh = 1, numfcst
c     if(namlevel(ilv)(1:4).eq.'P500') then
c      print*,'500 millibars',namlevel(ilv)(1:4)
c      endif
c     print*,'igotdata=',
c    * igotdata(obsval,forcst,obs,qms,nevn,
c    +                                      nlv,imodel,ifh,ivr,ilv,
c    +                                      iob,rm1,rm2)
                                IF (igotdata(obsval,forcst,obs,qms,nevn,
     +                                      nlv,imodel,ifh,ivr,ilv,
     +					    iob,rm1,rm2,subset,stnid)
     +                                      .eq.0) THEN
                                  sumd = sumdata(ifh,ivr,ilv,iar,iob) *
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + obsval
                                  ssqd = ssqdata(ifh,ivr,ilv,iar,iob) *
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + obsval * obsval
c     print*,'namlevel(ilv)(1:4)=',namlevel(ilv)(1:4)
c     print*,'namvfyobs(ilv)(:6)=',namvfyobs(ilv)(:6)
c     print*,'namarea(ilv)(:4)=',namarea(ilv)(:4)
c     if(namlevel(ilv)(1:4).eq.'P500'.and.
c    *   namvfyobs(ilv)(:6).eq.'ADPUPA'.and.
c    *   namvarbl(ilv)(:1).eq.'T'.and.
c    *   namarea(ilv)(:4).eq.'G212') then
c     if(iar.eq.16.and.nint(probs).eq.250.and.subset(:6).eq.'ADPUPA'.
c    * and.ivr.eq.1) then 
c       print*,'forcst,obsval=',forcst,obsval
c     endif 
                                  sumg = sumgrid(ifh,ivr,ilv,iar,iob) *
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + forcst
                                  ssqg = ssqgrid(ifh,ivr,ilv,iar,iob) *
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + forcst * forcst
                                  prod = forcst * obsval
                                  sump = sumprod(ifh,ivr,ilv,iar,iob) *
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + prod
                                  count(ifh,ivr,ilv,iar,iob) = 
     +                                        count(ifh,ivr,ilv,iar,iob)
     +                                        + 1.0
c     print*,'ifh,ivr,ilv,iar,iob,count(ifh,ivr,ilv,iar,iob)=',
c    *  ifh,ivr,ilv,iar,iob,count(ifh,ivr,ilv,iar,iob)
                                  sumdata(ifh,ivr,ilv,iar,iob) = sumd /
     +                                        count(ifh,ivr,ilv,iar,iob)
                                  ssqdata(ifh,ivr,ilv,iar,iob) = ssqd /
     +                                        count(ifh,ivr,ilv,iar,iob)
                                  sumgrid(ifh,ivr,ilv,iar,iob) = sumg /
     +                                        count(ifh,ivr,ilv,iar,iob)
                                  ssqgrid(ifh,ivr,ilv,iar,iob) = ssqg /
     +                                        count(ifh,ivr,ilv,iar,iob)
                                  sumprod(ifh,ivr,ilv,iar,iob) = sump /
     +                                        count(ifh,ivr,ilv,iar,iob)
                                END IF
C                             
C                             END INNER LOOP OVER NUMBER OF FORECAST HOUR (EVENT)
   30                         CONTINUE
C                           END INNER LOOP OVER VARIABLE
   40                       CONTINUE
                          END IF
C                       END INNER LOOP OVER REQUESTED LEVELS
   50                   CONTINUE
C                     END INNER LOOP OVER OBSERVATION LEVELS
   60                 CONTINUE
                    END IF
C                 END INNER LOOP OVER VERIFYING AREA
   70             CONTINUE
                END IF
C             END LOOP FOR ACTUAL OB-TYPE
   80         CONTINUE
C           END DO WHILE LOOP OVER BUFR REPORT
            END DO
C         END DO WHILE LOOP OVER BUFR MESSAGE
          END DO
C         
C         NOW IS THE TIME TO WRITE OUT THE STAT RECORDS
C         
          ist = 1
          DO 130 iob = 1, numvfyobs
            DO 120 iar = 1, numarea
              DO 110 ifh = 1, numfcst
                numv = numvarbl
                IF (numvector.gt.0) numv = numvarbl - 1
                DO 100 ivr = 1, numv
                  DO 90 ilv = 1, numlevel
c                   rtest = count(ifh,ivr,ilv,iar,iob)
c                   print *, 'COUNT(IFH,IVR,ILV,IAR,IOB)=',RTEST
                    IF (count(ifh,ivr,ilv,iar,iob).gt.0.0) THEN
                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrmodel(imodel)
                      vdbhdr132(istrt:iend) = 
     +                            namodel(imodel)(:nchrmodel(imodel))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfcst(ifh)
                      vdbhdr132(istrt:iend) = 
     +                            namfcst(ifh)(:nchrfcst(ifh))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfdate(ivfdate)
                      vdbhdr132(istrt:iend) = 
     +                            namvfdate(ivfdate)(:
     +                            nchrvfdate(ivfdate))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfyobs(iob)
                      vdbhdr132(istrt:iend) = 
     +                            namvfyobs(iob)(:nchrvfyobs(iob))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrarea(iar)
                      vdbhdr132(istrt:iend) = 
     +                            namarea(iar)(:nchrarea(iar))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      ivstrt = istrt
                      iend = iend + nchrstat(ist)
                      vdbhdr132(istrt:iend) = 
     +                            namstat(ist)(:nchrstat(ist))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvarbl(ivr)
                      vdbhdr132(istrt:iend) = 
     +                            namvarbl(ivr)(:nchrvarbl(ivr))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrlevel(ilv)
                      vdbhdr132(istrt:iend) = 
     +                            namlevel(ilv)(:nchrlevel(ilv))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      iend = iend + 1
                      vdbhdr132(iend:iend) = equal
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
c                     print *, 'IVR=',IVR,' and NUMVECTOR=',NUMVECTOR
                      IF (ivr.ne.numvector) THEN
C                       PRINT'(A,F7.0,5E18.9)',VDBHDR132(:IEND),
                        WRITE (51,1000) vdbhdr132(:iend), 
     +                              count(ifh,ivr,ilv,iar,iob), 
     +                              sumgrid(ifh,ivr,ilv,iar,iob), 
     +                              sumdata(ifh,ivr,ilv,iar,iob), 
     +                              sumprod(ifh,ivr,ilv,iar,iob), 
     +                              ssqgrid(ifh,ivr,ilv,iar,iob), 
     +                              ssqdata(ifh,ivr,ilv,iar,iob)
 1000                   FORMAT (A,F7.0,5E18.9)
                      END IF
C                     
                      IF (ivr.eq.numvector) THEN
                        sump = sumprod(ifh,ivr,ilv,iar,iob) + 
     +                              sumprod(ifh,ivr+1,ilv,iar,iob)
                        sumg = ssqgrid(ifh,ivr,ilv,iar,iob) + 
     +                              ssqgrid(ifh,ivr+1,ilv,iar,iob)
                        sumd = ssqdata(ifh,ivr,ilv,iar,iob) + 
     +                              ssqdata(ifh,ivr+1,ilv,iar,iob)
                        vdbhdr132(ivstrt:ivstrt) = namversion(1:)
C                       PRINT'(A,F7.0,7E18.9)',VDBHDR132(:IEND),
                        WRITE (51,1100) vdbhdr132(:iend), 
     +                              count(ifh,ivr,ilv,iar,iob), 
     +                              sumgrid(ifh,ivr,ilv,iar,iob), 
     +                              sumgrid(ifh,ivr+1,ilv,iar,iob), 
     +                              sumdata(ifh,ivr,ilv,iar,iob), 
     +                              sumdata(ifh,ivr+1,ilv,iar,iob), 
     +                              sump, sumg, sumd
 1100                   FORMAT (A,F7.0,7E18.9)
                      END IF
                    END IF
   90             CONTINUE
  100           CONTINUE
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
C         END OUTER LOOP OVER FORECAST MODEL
  140     CONTINUE
C         END OUTERMOST LOOP OVER VERIFYING DATE
  150     CONTINUE
          CALL closbf(lunin)
C         GO BACK AND DO IT ALL AGAIN   WHOOPEE!
          GO TO 10
C23456789012345678901234567890123456789012345678901234567890123456789012
  160     CONTINUE
          STOP
          END
