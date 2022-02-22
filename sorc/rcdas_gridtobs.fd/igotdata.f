      FUNCTION igotdata(obsval,forcst,obs,qms,nevn,nlv,imodel,ifh,ivr,
     +            ilv,iob,rm1,rm2,subset,stnid)
C
      INCLUDE 'parm.inc'
C
      COMMON /obmrk/ iqmod(maxobs)
      COMMON /fcsthr/ fhour(mxfcst)
      COMMON /model/ fprp, fmodel(maxmod)
      LOGICAL	  vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
C
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), 
     +            namvfdate(mxdate), namvfyobs(maxobs), 
     +            namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)
C
      CHARACTER*8 eric,eric1(maxmod),stnid,subset
      REAL 	  ericr1(maxmod)
      parameter(ideny=12)
      character*5 denystaid(ideny)
      EQUIVALENCE (eric,ericr)
      EQUIVALENCE (eric1,ericr1)
C
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
C
      real*8 obs(12,255,mxb), qms(8,255)
C...   STRING FOR THE OB, GUESS, ANALYSIS ....
C     DATA OBSTR /'SRC FHR POB QOB TOB ZOB UOB VOB PMO CAPE CINH LI'/
C...   STRING FOR THE QUALITY MARKS ....
C     DATA QMSTR /'CAT PRC PQM QQM TQM ZQM WQM'/
      DATA bmiss /10E10/
      DATA denystaid/'EREA ','78384','78988','80001','76458','76644',
     *   '76394','76256','08522','FNPH ','76723','UWEC '/
C
c     NPRP = 0
c     DO N=1,NEVN
C  FIRST CHECK MODEL NAME FOR PRP
c     IF(OBS(1,NLV,N).EQ.FPRP) THEN
c       NPRP=N
c     ENDIF
c     ENDDO
      nprp = nevn
      nevm = nevn - 1
      nfcs = 0
      DO n = 1, nevm
        ericr = obs (1,nlv,n)
        ericr1 (imodel) = fmodel (imodel)
        ericr=ericr1 (imodel)
C       NEXT CHECK FOR  B O T H  FMODEL  A N D  FHOUR
c       print*,'ericr,ericr1(imodel)=',ericr,ericr1(imodel)
c       print*,'obs(2,nlv,n),fhour(ifh)=',obs(2,nlv,n),fhour(ifh)
        IF ( ericr .eq. ericr1 (imodel) .and.
     +      nint(obs(2,nlv,n)).eq.nint(fhour(ifh))) THEN
          nfcs = n
        END IF
      END DO
c     print*,'nfcs,nprp=',nfcs,nprp
      igotdata = -1
c     do i=1,ideny
c      print*,'stnid,denystaid(i)=',stnid,denystaid(ideny)
c      if(stnid.eq.denystaid(i)) then
       if(stnid.eq.'UWEC ') then
        print*,'stnid ',stnid,' is bad'
        print*,'fcs,ob,qm=',obs(5,nlv,nfcs),obs(5,nlv,nprp),qms(5,nlv)
        igotdata=-1
        return
      endif
c     enddo
      IF (nfcs.eq.0) THEN
c       PRINT*,'*** NFCS=0 ***',NLV,NPRP,NFCS
c       PRINT 1000,NEVN,IMODEL,IFH,FPRP,FMODEL(IMODEL),FHOUR(IFH)
c       PRINT 1100,((OBS(I,NLV,N),I=1,8),N=1,NEVN)
        RETURN
      END IF
C     CHECK FOR NON-MISSING SEA-LEVEL PRESSURE WITH PROPER QUALITY MARK
      IF (nchrvarbl(ivr).eq.3.and.namvarbl(ivr).eq.'SLP' ) THEN
        IF ((iqmod(iob).eq.1.and.qms(3,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(3,nlv).ge.3.)) THEN
          IF (obs(9,nlv,nprp).lt.bmiss.and.obs(9,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(9,nlv,nprp)
            forcst = obs(9,nlv,nfcs)
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR CAPE
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'CAPE' ) THEN
        IF (obs(10,nlv,nprp).lt.bmiss.and.obs(10,nlv,nfcs).lt.bmiss)
     +              THEN
           obsval = obs(10,nlv,nprp)
           forcst = obs(10,nlv,nfcs)
           igotdata = 0
           RETURN
         END IF
C     CHECK FOR CINH
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'CINH' ) THEN
         IF (obs(11,nlv,nprp).lt.bmiss.and.obs(11,nlv,nfcs).lt.bmiss)
     +               THEN
            obsval = obs(11,nlv,nprp)
            forcst = obs(11,nlv,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C      CHECK FOR LI
       ELSE IF (nchrvarbl(ivr).eq.2.and.namvarbl(ivr).eq.'LI') THEN
         IF (obs(12,nlv,nprp).lt.bmiss.and.obs(12,nlv,nfcs).lt.bmiss)
     +               THEN
            obsval = obs(12,nlv,nprp)
            forcst = obs(12,nlv,nfcs)
            igotdata = 0
            RETURN
          ENDIF
C     CHECK FOR NON-MISSING HEIGHT WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'Z'.and.nchrvarbl(ivr).eq.1) THEN
        IF ((iqmod(iob).eq.1.and.qms(6,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(6,nlv).ge.3.)) THEN
c       if(abs(obs(6,nlv,nprp)-obs(6,nlv,nfcs)).gt.100) then
c         print*,'obsval,forcst=',obs(6,nlv,nprp),obs(6,nlv,nfcs)
c       endif
          IF (obs(6,nlv,nprp).lt.bmiss.and.obs(6,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(6,nlv,nprp)
            forcst = obs(6,nlv,nfcs)
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING TEMPERATURE WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'T'.and.nchrvarbl(ivr).eq.1) THEN
        IF ((iqmod(iob).eq.1.and.qms(5,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(5,nlv).ge.3.)) THEN
          IF (obs(5,nlv,nprp).lt.bmiss.and.obs(5,nlv,nfcs).lt.bmiss) 
     +                THEN
	    prs = obs(3,nlv,nprp)
c     do i=1,ideny
c       if(stnid.eq.denystaid(i)) then
c       if(stnid.eq.'UWEC '.and.nint(prs).eq.850) then
        if(stnid.eq.'UWEC ') then
c       print*,'stnid,obsval,forcst=',obs(5,nlv,nprp),obs(5,nlv,nfcs)
        print*,'stnid,prs=',stnid,prs
        print*,'obs,fcst,prs=',obs(5,nlv,nprp),obs(5,nlv,nfcs),prs
        endif
c     enddo
c           if(obs(5,nlv,nfcs).lt.-270.0) then
c             print*,'stnid=',stnid
c             igotdata=-1
c             print*,'obs(5,nlv,nfcs)=',obs(5,nlv,nfcs)
c             print*,'nlv,nfcs=',nlv,nfcs
c             print*,'changing igotdata to -1'
c             return
c           endif
	    IF ( vtflg .and. prs .gt. 400 ) THEN
		IF ((iqmod(iob).eq.1.and.qms(4,nlv).lt.3.).or.
     +		    (iqmod(iob).eq.2.and.qms(4,nlv).ge.3.)) THEN
		    IF ( obs(4,nlv,nprp).lt.bmiss) THEN
                	forcst = obs(5,nlv,nfcs)
			fac = 1. + .608 * obs(4,nlv,nprp) * .000001
			obsval = ( 273.15 + obs (5,nlv,nprp) ) / fac
			obsval = obsval - 273.15
			igotdata = 0
		    END IF
		END IF
	    ELSE
                obsval = obs(5,nlv,nprp)
                forcst = obs(5,nlv,nfcs)
                igotdata = 0
	    END IF
c           print*,'obsval,forcst=',obsval,forcst
c           print*,'stnid=',stnid
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING SPEC.HUM. WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'Q'.and.nchrvarbl(ivr).eq.1) THEN
c       print*,'iqmod(iob),qms(4,nlv)=',iqmod(iob),qms(4,nlv)
c       print*,'subset=',subset
        IF ((iqmod(iob).eq.1.and.qms(4,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(4,nlv).ge.3.)) THEN
c         print*,'bmiss,obs(4,nlv,nprp),obs(4,nlv,nfcs)=',
c    *     bmiss,obs(4,nlv,nprp),obs(4,nlv,nfcs)
          IF (obs(4,nlv,nprp).lt.bmiss.and.obs(4,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(4,nlv,nprp) * 0.001
            forcst = obs(4,nlv,nfcs) * 0.001
c           obsval = obs(4,nlv,nprp)
c           forcst = obs(4,nlv,nfcs)
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING TEMP&SPCHUM WITH PROPER QUALITY MARK
      ELSE IF (nchrvarbl(ivr).eq.2.and.(namvarbl(ivr).eq.'TV'
     +            .or.namvarbl(ivr).eq.'TD'
     +            .or.namvarbl(ivr).eq.'RH') ) THEN
        IF ((iqmod(iob).eq.1.and.(qms(4,nlv).lt.3..and.qms(5,nlv).lt.3.)
     +              ).or.(iqmod(iob).eq.2.and.(qms(4,nlv).ge.3..or.
     +              qms(5,nlv).ge.3.))) THEN
c         print*,'nlv,nprp,nfcs=',nlv,nprp,nfcs
c         print*,'obs(3,nlv,nprp),obs(4,nlv,nprp),obs(5,nlv,nprp)=',
c    *     obs(3,nlv,nprp),obs(4,nlv,nprp),obs(5,nlv,nprp)
c         print*,'obs(4,nlv,nfcs),obs(5,nlv,nfcs)=',
c    *     obs(4,nlv,nfcs),obs(5,nlv,nfcs)
          IF (obs(3,nlv,nprp).lt.bmiss.and.obs(4,nlv,nprp).lt.bmiss.and.
     +                obs(4,nlv,nfcs).lt.bmiss.and.obs(5,nlv,nprp).lt.
     +                bmiss.and.obs(5,nlv,nfcs).lt.bmiss) THEN
            pobsval = obs(3,nlv,nprp)
            qobsval = obs(4,nlv,nprp) * 0.000001
            qforcst = obs(4,nlv,nfcs) * 0.000001
            tobsval = obs(5,nlv,nprp) + 273.16
            tforcst = obs(5,nlv,nfcs) + 273.16
            IF (namvarbl(ivr).eq.'TV') THEN
              obsval = (tobsval*(1.+qobsval/.62197)/(1.+qobsval)) - 
     +                    273.16
              forcst = (tforcst*(1.+qforcst/.62197)/(1.+qforcst)) - 
     +                    273.16
            ELSE IF (namvarbl(ivr).eq.'TD')
     +                  THEN
              vpobs = (pobsval*qobsval) / (.622+.378*qobsval)
              obsval = alog(vpobs/6.112) * 243.5 / (17.67-
     +                    alog(vpobs/6.112))
              vpfcs = (pobsval*qforcst) / (.622+.378*qforcst)
              forcst = alog(vpfcs/6.112) * 243.5 / (17.67-
     +                    alog(vpfcs/6.112))
            ELSE IF (namvarbl(ivr).eq.'RH')
     +                  THEN
              vpobs = w3fa09(tobsval) * 10.
              qsobs = .622 * vpobs / (pobsval-.378*vpobs)
              vpfcs = w3fa09(tforcst) * 10.
              qsfcs = .622 * vpfcs / (pobsval-.378*vpfcs)
              obsval = (qobsval/qsobs) * 100.
              forcst = (qforcst/qsfcs) * 100.
            END IF
            igotdata = 0
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING U-COMP WITH PROPER QUALITY MARK
      ELSE IF ((namvarbl(ivr).eq.'U'.and.nchrvarbl(ivr).eq.1).or.
     +            namvarbl(ivr).eq.'VWND') THEN
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(7,nlv,nprp).lt.bmiss.and.obs(7,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(7,nlv,nprp)
c           print*,'nmbgrd(imodel)=',nmbgrd(imodel)
c           print*,'rm1,rm2=',rm1,rm2
	    IF ( nmbgrd (imodel) .gt. 0 ) THEN
                vf = obs(8,nlv,nfcs)
		IF ( vf .lt. bmiss ) THEN
		    uf = obs(7,nlv,nfcs)
		    forcst = rm1 * uf + rm2 * vf
		    igotdata = 0
		END IF
	    ELSE
                forcst = obs(7,nlv,nfcs)
                igotdata = 0
	    END IF
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING V-COMP WITH PROPER QUALITY MARK
      ELSE IF (namvarbl(ivr).eq.'V'.and.nchrvarbl(ivr).eq.1) THEN
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(8,nlv,nprp).lt.bmiss.and.obs(8,nlv,nfcs).lt.bmiss) 
     +                THEN
            obsval = obs(8,nlv,nprp)
	    IF ( nmbgrd (imodel) .gt. 0) THEN
                uf = obs(7,nlv,nfcs)
		IF ( uf .lt. bmiss ) THEN
		    vf = obs(8,nlv,nfcs)
		    forcst = -rm2 * uf + rm1 * vf
		    igotdata = 0
		END IF
	    ELSE
                forcst = obs(8,nlv,nfcs)
                igotdata = 0
	    END IF
            RETURN
          END IF
        END IF
C     CHECK FOR NON-MISSING U-&V-COMP WITH PROPER QUALITY MARK
      ELSE IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'WSPD') THEN
        IF ((iqmod(iob).eq.1.and.qms(7,nlv).lt.3.).or.(iqmod(iob).eq.2
     +              .and.qms(7,nlv).ge.3.)) THEN
          IF (obs(7,nlv,nprp).lt.bmiss.and.obs(7,nlv,nfcs).lt.bmiss.and.
     +                obs(8,nlv,nprp).lt.bmiss.and.obs(8,nlv,nfcs).lt.
     +                bmiss) THEN
            uobsval = obs(7,nlv,nprp)
            uf = obs(7,nlv,nfcs)
            vobsval = obs(8,nlv,nprp)
            vf = obs(8,nlv,nfcs)
	    IF ( nmbgrd (imodel) .gt. 0) THEN
		uforcst = rm1 * uf + rm2 *vf
		vforcst = -rm2 * uf + rm1 * vf
	    ELSE
		uforcst = uf
		vforcst = vf
	    END IF
            obsval = sqrt(uobsval*uobsval+vobsval*vobsval)
            forcst = sqrt(uforcst*uforcst+vforcst*vforcst)
            igotdata = 0
            RETURN
          END IF
        END IF
      END IF
 1000 FORMAT (3I5,2A8,F7.1)
 1100 FORMAT (A8,7(F8.2,2X))
      END
