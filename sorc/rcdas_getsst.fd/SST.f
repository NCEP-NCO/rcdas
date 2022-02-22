C&&&&&&&&
C PROGRAM DOCUMENTATION BLOCK
C
CPROGRAM: narr_getsst
C  PRGMMR:  PERRY SHAFRAN?               ORG: W/NP20    DATE: ????-??
C
CABSTRACT: 
C   READS IN CANADA-LAKES ICE DATA, GREAT LAKES SST, GLOBAL SST, GLOBAL ICE and MASKS
C   MAKES SST FILE FOR NARR (EDAS)
C   USE CLIMATOLOGY FOR GULF OF MEXICO
C
C PROGRAM HISTORY LOG:
C    200?-??   P. SHAFRAN?
C
C
C USAGE:
C    echo "YY MM DD" | narr_getsst.x
C         YY, MM, DD are the date code
C
C INPUT FILES:
C    UNIT5   text file: line 1: "iyear  imonth  iday"
C    UNIT11  binary file: canada lakes ice table
C    UNIT14  binary file: land-sea mask
C    UNIT39  grib file: global sst
C    UNIT40  grib file: global ice
C    UNIT41  binary file: great lakes sst
C
C OUTPUT FILES:
C    UNIT51  grib file: merged SST
C
C ROUTINES CALLED
C   BAOPEN, BAOPENR
C   GETGB
C   PUTGB    
C   GRIBST (INTERNAL)
C
CATTRIBUTES:
C   LANGUAGE: F90
C
C$$$$$$$$$$$$
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
      program SSTHIRES 
C
      IMPLICIT REAL (A-H, O-Z)
C
      PARAMETER (IM=237,JM=387,LM=45,LSM=39)
C
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
      parameter (hm1=-1.0)
C
      INTEGER IDATE(4),IDAT(3),MONTH(12)
      DIMENSION SSTLL(361,180),SALTLK(12),SALTLA(2),SALTLO(2)
      real ice(im,jm)
      dimension ipoint(161),jpoint(161)
      dimension sstgl(im,jm)
C
      DIMENSION  SST(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
      dimension temp2(im,jm)
c     dimension idate(4)
 
      logical*1 bit(im,jm)
      INTEGER KPDS(25), KGDS(22)
      character gds(42)

      REAL XDAY(12),MSST(13)
C
      DATA   INSST/39/
      DATA   INDXST/38/

      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
      DATA SALTLK/273.38,274.27,278.50,283.01,287.33,293.41
     1,           297.13,297.73,294.97,289.58,282.31,275.67/
c
C**   XDAY DEFINES THE # DAYS IN EACH MONTH, MSST DEFINES THE MEAN GULF
C**   SST VALUES VALID AT THE BEGINNING OF EACH MONTH.  THE JANUARY
C**   SST VALUE IS REPEATED AFTER DECEMBER.
C
      DATA XDAY/31.,28.,31.,30.,31.,30.,31.,31.,30.,31.,30.,31./
      DATA MSST/17.0,16.3,17.2,19.3,22.0,25.5,29.0,32.0,33.0,
     1          30.5,25.8,20.5,17.0/
      DATA R2D/57.2957795/
C
C     CORNERS OF SALT LAKE LATITUDE/LONGITUDE BOX
C     in degrees---> 40.0     42.0            111.0    114.0
      DATA SALTLA/0.698132,0.733038/,SALTLO/1.937315,1.989675/

      Data  PI/3.141592654/

C hard-wired 32km resolution with the big domain
        
       bit=.TRUE.
c      do j=1,jm
c      do i=1,im
c        print*,'i,j,bit(i,j)=',i,j,bit(i,j)
c      enddo
c      enddo
      
       call baopen(51,'fort.51',ireto1)
       call baopenr(39,'fort.39',ireto2)
       call baopenr(40,'fort.40',ireto3)
       print*,'ireto1,ireto2,ireto3=',ireto1,ireto2,ireto3
      do i=1,161
       read(11,234) ipoint(i),jpoint(i)
234    format(1x,i3,1x,i3)
c      print*,'i,ipoint(i),jpoint(i)=',i,ipoint(i),jpoint(i)
      enddo
       
       TLM0D=-111.0
       TPH0D= 50.0
       WBD=  -53.0
       SBD=  -40.0
       DLMD=  0.224576271           !32km resolution
       DPHD=  0.207253886
       DT=90.0                      !delta t
       W=0.250

        REWIND 14
        READ(14) TEMP2,SM     !the mask used here is different from the model
        READ(14) TEMP2

C--------------DERIVED GEOMETRICAL CONSTANTS----------------------------

      DTR=PI/180.
      DPR=180./PI
      TPH0=TPH0D*DTR
      WB=WBD*DTR
      SB=SBD*DTR
      DLM=DLMD*DTR
      DPH=DPHD*DTR
      TDLM=DLM+DLM
      TDPH=DPH+DPH
      RDLM=1./DLM
      RDPH=1./DPH
C
      WBI=WB+TDLM
      SBI=SB+TDPH
      EBI=WB+IM2*TDLM
      ANBI=SB+JM3*DPH
      tph=sb-dph
C
      STPH0=SIN(TPH0)
      CTPH0=COS(TPH0)
C

C--------------GEOGRAPHIC LAT AND LONG OF TLL GRID POINTS-----------

          DO 183 J=1,JM
C
           TLM=WB-TDLM+MOD(J+1,2)*DLM
           TPH=TPH+DPH
           STPH=SIN(TPH)
           CTPH=COS(TPH)
C
          DO 183 I=1,IM

      TLM=TLM+TDLM
      SINPHI=CTPH0*STPH+STPH0*CTPH*COS(TLM)
      SINPHI=AMIN1(SINPHI,1.)
      SINPHI=AMAX1(SINPHI,-1.)
      GLAT(I,J)=ASIN(SINPHI)
      COSLAM=CTPH*COS(TLM)/(COS(GLAT(I,J))*CTPH0)-TAN(GLAT(I,J))*
     1 TAN(TPH0)
      IF(COSLAM.GT.H1)  COSLAM=H1
      IF(COSLAM.LT.-1.) COSLAM=-1.
      FACT=H1
      IF(TLM.GT.D00)FACT=HM1
      GLON(I,J)=-TLM0D*DTR+FACT*ACOS(COSLAM)
c     print*,'i,j,glat,glon=',i,j,glat(i,j),glon(i,j)
  183 CONTINUE
c
c Start time loop here
c

c     iyr=82
c     do imo=1,12
c     if(mod(iyr,4).eq.0) month(2)=29  !  accounts for leap years
c     idybeg=1
c     if(imo.eq.11) idybeg=8
c     if(imo.eq.12) idybeg=1
c     idybeg=1
c     do idy=idybeg,month(imo)
c     iyr=02
c     imo=12
c     idy=01
      read(5,111) iyr,imo,idy
111   format(4(i2,1x))
      print*,'imo,idy=',imo,idy
C
      CALL BAOPENr(INSST,'fort.39',IRETO)
c     call baopenr(38,'fort.38',ireto)
c     IOUTUPRT = LIST
      insstgl=41
      inice=40
      CALL GRIBST(INSST,insstgl,inice,SSTLL,sstgl,ice,IERR,iyr,imo,idy)
      IF (IERR.NE.0) then
        print*,'Error reading in SST or ice'
        print*,'IERR=',IERR
        stop 4500
      endif
c     print*,'Printout of SST slice at I=190'
c     print*
c     do j=180,1,-1
c     do i=1,361
c      print*,'i,j,sst(i,180)=',i,j,sstll(i,j)
c     enddo
c     enddo
C
C----  INTERPOLATE 1-DEG GLOBAL SST TO ETA GRID  -------
C
C-CP NOTE:  THIS SUBROUTINE AND INTERPOLATION ALGORITHM ASSUME
C-CP A 1-DEG GLOBAL SST FIELD IN THE FOLLOWING FORMAT:  
C-CP
C-CP  I=1 AT 0.5 E,  I=2 AT 1.5 E, ... , I=360 at 0.5W
C-CP  J=1 AT 89.5S, J=2 AT 88.5 S, ..., J=180 at 89.5N
C-CP  
C-CP In the interpolation algorithm below, glon is positive westward,
C-CP from 0 to 360, with 0 at the greenwich meridian.  Elon is positive 
C-CP eastward, thus the need to subtract glon from 360 to get the index
C-CP of the correct oisst point.  If your input 1 deg SST field is in
C-CP a different indexing scheme, you will need to change the algorithm
C-CP below - see "grdeta.oldoi"
C-CP
      DO J=1,JM
      DO I=1,IM
      ELAT=H90+GLAT(I,J)/DTR
      ELON=H360-GLON(I,J)/DTR
      IF(ELON.GT.H360)ELON=ELON-H360
c     print*,'i,j,elat,elon=',i,j,elat-90.0,abs(elon-360.0)
      ILON1=INT(ELON)
      DIF=ELON-ILON1
      IF(DIF.GT.D5)ILON1=ILON1+1
      IF(ILON1.EQ.D00)ILON1=360
      ILON2=ILON1+1
      ILAT1=INT(ELAT)
      DIF=ELAT-ILAT1
      IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
c     if(i.eq.119.and.j.eq.387) then
c       print*,'h90,dtr=',h90,dtr
c       print*,'glat(i,j)=',glat(i,j)
c       print*,'glat(i,j)/dtr=',glat(i,j)/dtr
c       print*,'elat,ilat1,dif=',elat,ilat1,dif
c     endif
c     IF(ILAT1.EQ.180.OR.ILAT1.EQ.0)THEN
c       WRITE(6,6788)I,J,GLAT(I,J),GLON(I,J),ELAT,ELON
c6788   FORMAT(' I,J=',2I4,' GLAT=',E12.5,' GLON=',E12.5,
c    1   ' ELAT=',E12.5,' ELON=',E12.5)
c       STOP 333
c     ENDIF
      ILAT2=ILAT1+1
      W1=ELON-ILON1+D5
      IF(W1.LT.D00)W1=W1+H360
      W2=ELAT-ILAT1+D5
      AR1=W1*W2
      AR2=W1*(H1-W2)
      AR3=(H1-W1)*(H1-W2)
      AR4=(H1-W1)*W2
        
      SST(I,J) = AR1*SSTLL(ILON2,ILAT2)+AR2*SSTLL(ILON2,ILAT1)+
     1            AR3*SSTLL(ILON1,ILAT1)+AR4*SSTLL(ILON1,ILAT2)
      if(ilat2.eq.181) then
       sst(i,j)=(sstll(ilon2,ilat1)+sstll(ilon1,ilat1))*0.5
c      sst(i,j)=271.3
      endif
c     print*,'i,j,sst(i,j)=',i,j,sst(i,j)
c     print*,'ilat1,ilon1,ilat2,ilon2=',ilat1,ilon1,ilat2,ilon2
c     if(i.eq.119.and.j.eq.387) then
c     if(ilat2.gt.180) then
c       print*,'i,j=',i,j
c       print*,'sst(i,j)=',sst(i,j)
c       print*,'ilon1,ilat1=',ilon1,ilat1
c       print*,'ilon2,ilat2=',ilon2,ilat2
c       print*,'ar1,ar2,ar3,ar4=',ar1,ar2,ar3,ar4
c       print*,'sstll(ilon2,ilat2)=',sstll(ilon2,ilat2)
c       print*,'sstll(ilon2,ilat1)=',sstll(ilon2,ilat1)
c       print*,'sstll(ilon1,ilat1)=',sstll(ilon1,ilat1)
c       print*,'sstll(ilon1,ilat2)=',sstll(ilon1,ilat2)
c     endif
      ENDDO
      ENDDO
C***
C***  INSERT TEMPERATURES FOR THE GREAT SALT LAKE
C***
c     ID1=IDAT(1)
c     ID2=IDAT(2)
      id1=imo
      id2=idy
      MARG0=ID1-1
      IF(MARG0.LT.1)MARG0=12
      MNTH0=MONTH(MARG0)
      MNTH1=MONTH(ID1)
      IF(ID2.LT.15)THEN
        NUMER=ID2+MNTH0-15
        DENOM=MNTH0
        IARG1=MARG0
        IARG2=ID1
      ELSE
        NUMER=ID2-15
        DENOM=MNTH1
        IARG1=ID1
        IARG2=ID1+1
        IF(IARG2.GT.12)IARG2=1
      ENDIF
      FRAC=NUMER/DENOM
      DO J=1,JM
      DO I=1,IM
        IF(GLAT(I,J).GT.SALTLA(1).AND.GLAT(I,J).LT.SALTLA(2))THEN
          IF(GLON(I,J).GT.SALTLO(1).AND.GLON(I,J).LT.SALTLO(2))THEN
            IF(SM(I,J).GT.0.5)
     1        SST(I,J)=SALTLK(IARG1)+
     2                (SALTLK(IARG2)-SALTLK(IARG1))*FRAC
          ENDIF
        ENDIF
      ENDDO
      ENDDO
c
c  Insert routine to put in temperatures for the Gulf of California
c
C**  CHECK FOR LEAP YEARS
C
      ILEAP=MOD(IYR,4)
      IF(ILEAP.EQ.0) XDAY(2)=29.
C
C**  SET VALUE OF SSTVAL TO THE MEAN SURFACE VALUE NEAR GUAYMAS FROM
C**  RIPA AND MARIONONE (1989, QUART. J. ROY. METEOR. SOC., 115,
C**  887-913) - THEIR FIGURE 4 AND TABLE 2 SHOWING TEMPERATURE CURVE
C**  EXPLAINS 93% OF THE VARIANCE AT THE 90+% CONFIDENCE LEVEL.
C
C**  NOTE:  THEIR DATA SET IS NOT EXTENSIVE, AND THUS BETTER ONES MAY
C**         BE AVAILABLE (ALTHOUGH I DON'T KNOW OF ANY!).
C
      SSTVAL=273.15+MSST(IMO)+
     1      (REAL(IDY-1)/XDAY(IMO))*(MSST(IMO+1)-MSST(IMO))
C
C**  BEGIN LOOP TO DETERMINE IF A GRID POINT IS WITHIN THE GULF OF
C**  CALIFORNIA
C
C**  XLONMIN DEFINES A LINE THAT LIES ALONG THE CENTER OF THE BAJA
C**  CALIFORNIA PENNINSULA.  ANY WATER THAT LIES WITHIN 5 DEG LONGITUDE
C**  TO THE EAST OF THIS LINE, BETWEEN 24 AND 32 DEG LATITUDE, IS ASSUMED
C**  TO BE WITHIN THE GULF OF CALIFORNIA.
C
      do 20 j=1,jm
      do 20 i=1,im
      glatd=glat(i,j)*r2d
      glond=-glon(i,j)*r2d
c     print*,'i,j,glon(i,j),glond=',i,j,glon(i,j),glond
      IF(GLATD.GE.23..AND.GLATD.LE.32.)THEN
        XLONMIN=-109.75-(GLATD-23.)*0.75
        XLONMAX=XLONMIN+5.
        IF(GLOND.GE.XLONMIN.AND.GLOND.LE.XLONMAX)THEN
          IF(SM(i,j).GT.0.9.AND.SSTVAL.GT.SST(i,j))SST(i,j)=SSTVAL
c         print*,'glatd,glond=',glatd,glond
c         print*,'xlonmin,xlonmax=',xlonmin,xlonmax
c         print*,'i,j,sstval=',i,j,sstval
        ENDIF
      ENDIF
  20  CONTINUE
c
c Now for the Great Lakes SST
c
      call SST14K(sst,sm,glat,glon)
c     do j=1,387
c     do i=1,237
c      print*,'i,j,sstgl(i,j)=',i,j,sstgl(i,j)
c      if(sstgl(i,j).gt.100.) then
c        print*,'i,j,sstgl(i,j)=',i,j,sstgl(i,j)
c        sst(i,j)=sstgl(i,j)
c      endif
c     enddo
c     enddo
c
c Make sure the Canadian lakes are consistent with sea-ice
c
      do i=1,161
       if(ice(ipoint(i),jpoint(i)).gt.0.9) then
c        print*,'ipoint(i),jpoint(i)=',ipoint(i),jpoint(i)
c        print*,'ice(ipoint(i),jpoint(i))=',ice(ipoint(i),jpoint(i))
         sst(ipoint(i),jpoint(i))=273.15
       endif
      enddo

c
c Now we can write out the SST field
c
      call makgds(192,kgds,gds,lengds,iret)
      WRITE(6,*) 'FINISHED MAKGDS, IRET=', IRET

      print*,'putgb kgds=',kgds

c     kpds(1)=7
c     kpds(2)=44
      kpds(3)=192
      kpds(4)=192
      kpds(5)=11
      kpds(6)=1
      kpds(7)=0
c     iyear=1982
c     icent=20
c     kpds(8)=iyear-(icent-1)*100
c     kpds(9)=imo
c     kpds(10)=idy
      kpds(8)=iyr
      kpds(9)=imo
      kpds(10)=idy
      kpds(11)=0
      kpds(12)=0
      kpds(13)=0
      kpds(14)=0
      kpds(15)=0
      kpds(16)=0
      kpds(17)=0
      kpds(18)=1
      kpds(19)=3
      kpds(20)=0
      kpds(21)=20+1
      kpds(22)=2
c     kpds(23)=4
c     kpds(24)=0
c     kpds(25)=32

      print*,'putgb kpds=',kpds

c     do j=1,jm
c     do i=1,im
c      print*,'putgb,i,j,sst(i,j),bit(i,j)=',i,j,sst(i,j),bit(i,j)
c     enddo
c     enddo
      call putgb(51,im*jm,kpds,kgds,bit,sst,iret)
      print*,'iret=',iret

c     enddo  ! idy=1,month(imo)
c     enddo  ! imo=1,12

      STOP 
      END

C85106$  sub  PROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C sub  PROGRAM:  gribst      UNPACK GLOBAL Grib SST
C   PRGMMR: GERALD           ORG: W/NMC21     DATE: 95-08-01
C
C ABSTRACT: DECODES GLOBAL AVN, FNL, OR MRF GRIB GRIDDED FIELDS.
C
C PROGRAM HISTORY LOG:
C   95-08-01  GERALD
C   96-07-23  PETERS - UPDATED FOR USE WITH ETA MODEL, GETS 
C                      REYNOLDS 1X1 OISST 
C
C
C USAGE:
C   INPUT FILES:
C     DDNAME1  - GENERIC NAMES & USAGE
C     FTNNF001 - START IN COL 7
C              - BUT TAB CONTINUATIONS TO LINE UP LIKE THIS
C     FXN      - LINE UP DASHES WHEN DESCRIBING FILES
C     TAPENN   - CRAY TYPE DESIGNATION
C     UNITNN   - CRAY TYPE DESIGNATION
C     PARM     - IF PARM FIELD IS USED, DESCRIBE HERE
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     FTMMF001 - NAMES & USAGE AS ABOVE IN THE INPUT SECTION
C     FT06F001 - INCLUDE IF ANY PRINTOUT
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - ROUTINES THAT ACCOMPANY SOURCE FOR COMPILE
C     LIBRARY:
C       COMMON   - LIST COMMON LIBRARY ROUTINES, E.G., CONSOL
C       W3LIB    -
C       W4LIB    - DELETE THE CORRESPONDING LINE OR LINES
C       GRAPHICS - IF LIBRARY IS UNNEEDED
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - TROUBLE OR SPECIAL FLAG - SPECIFY NATURE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  CRAY
C
C85106$
C
      subroutine gribst(insst,insstgl,inice,gsst,sstgl,ice,
     *   ierr,iyr,imo,idy)
c
      Parameter(jf1=360*180)
      parameter(jf2=237*387)
      INTEGER PDS,GDS,GRID
      INTEGER JPDS(25),JGDS(22),IGRD(5,3)
      INTEGER KPDS(25),KGDS(22)
      REAl FLD(jf1),out(360,180)
      dimension gsst(361,180),ice(237,387)
      dimension idate(4)
      logical*1 lb(jf1),lb2(jf2)
      character*6 date
      character*38 fname
      character*39 fname2
      equivalence(out(1,1),fld(1))
c
C
C                sst
      DATA IGRD/ 11, 91,  2, 52, 11,
     *            1,  1,102,105,105,
     *            0,  1,  0,  2,  2/
c
C            INPUT UNITS FOR DECODING GRIB FILE
C
          LUGB=INSST
c         LUGI=INDXST
          lugi=0
          ierr = 0
          j = 0
          sstgl=0.0
C
C........    DECODE THE FIELDS
C
          DO 10 PDS=1,25
          JPDS(PDS) = -1
   10      CONTINUE
C
             DO 20 GDS = 1,22
           JGDS(GDS) = -1
   20     CONTINUE
C
C........   GET SELECT FIELDS
C
c          jPDS(5) = IGRD(GRID,1)
c          jPDS(6) = IGRD(GRID,2)
c          jPDS(7) = IGRD(GRID,3)
           jpds(5)=11
           jpds(6)=1
           jpds(7)=0
c          icent = (idate(4) - 1) / 100 + 1
           icent=21
c          jpds(8) = idate(4) - (icent-1)*100
c          jpds(9) = idate(2)
c          jpds(10)= idate(3)
           jpds(21)=icent
           jpds(8)=iyr
           if(iyr.eq.0) jpds(8)=iyr+100
c          jpds(8)=01
c          iyear=00
c          jpds(8)=iyear-(icent-1)*100
c          jpds(8)=100
           jpds(9)=imo
           jpds(10)=idy
           print*,'sst jpds=',jpds
C
             print *,'call getgb, lugb=',lugb
           LUGI = 0
           CALL GETGB(LUGB,LUGI,JF1,j,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB,fld,IRET)
	   print*,'call getgb<< iret=',iret
           print*,'sst kpds=',kpds
           print*,'sst kgds=',kgds
C
          IF(IRET.NE.0) THEN
            ierr = 1
	    write(*,*) 'error in reading sst unit ',lugb
            WRITE(6,60)IRET
            GO TO 999
          ENDIF
   60  FORMAT(1X,' IRET =',I5)
       do jj=1,180
       do kk=1,360
       gsst(kk,jj)=out(kk,jj)
c      print*,'kk,jj,gst=',kk,jj,gsst(kk,jj)
       enddo
       enddo
c
c The following lines are commented out for the Regional Reanalysis
c because the climate SST is already oriented S->N.  In operations,
c the SST comes in oriented N->S, so that is why this code exists.
c Perry Shafran - 21 December 2001
c
c..   flip grid to PT(1,1) =(0e,-90.0)
c
              do  jj= 1,180
                do  kk= 1,360
                 gsst(kk,180-jj+1) = out(kk,jj)
                end do
              end do
c
c...   add greenich to right side of grid
c
             do jj = 1,180
               gsst(361,jj) = gsst(1,jj)
             end do
c
            WRITE(6,61)KPDS,KF,KGDS
   61  FORMAT(2(/,2X,'PDS=',13I7),2(/,2X,' GDS=',11I7 ))
C
c Check to see that point (1,1) is located at (0, 90S)
c
       if(kgds(4).ne. 89500.or.kgds(5).ne.500) then
        print*,'*** WARNING ***'
        print*,'SST ORIGIN POINT NOT AT (0 LON, 90 S LAT)'
        print*,'GRDETA TERMINATING IN GRIBST'
        stop312
       endif
C
c Get names of ice and Great Lakes SST files
c
c         write(date,555) iyr,imo,idy
c         print*,'date=',date
c555       format(3i2.2)
c         fname='/groupB/users/pshaf/ice32/ice.19'// date
c         print*,'Opening ice file ',fname, unit 41'
c         fname2='/groupB/users/pshaf/sst_gl/sst.20'//date
c         print*,'Opening GL SST file ',fname2,' unit 41'
c         lugb=insstgl
c         open(lugb,file=fname2,status='old',form='unformatted')
c         call baopenr(lugb,'fort.41',ireto)
c         read(lugb) sstgl
c         print*,'sstgl=',sstgl
          
          lugb=inice
c         open(lugb2,file=fname,status='old',form='unformatted')
c         call baopenr(lugb2,'fort.40',ireto)
c
c Get the ice fields
c
           jpds=-1
           jgds=-1
c          jPDS(5) = IGRD(2,1)
c          jPDS(6) = IGRD(2,2)
c          jPDS(7) = IGRD(2,3)
           jpds(5)=91
           jpds(6)=1
c          jpds(7)=1	removed 1/2007 wne

c           icent=21 mod 1/2007 wne

           icent = 21
           if (iyr .gt. 50) icent = 20

c          jpds(8) = idate(4) - (icent-1)*100
c          jpds(9) = idate(2)
c          jpds(10)= idate(3)
           jpds(21)=icent
           jpds(8)=iyr
c          jpds(8)=01
c          iyear=00
c          jpds(8)=iyear-(icent-1)*100
c          jpds(8)=100
           jpds(9)=imo
           jpds(10)=idy
           LUGI = 0
           print*,'ice jpds=',jpds
             print *,'call getgb, lugb=',lugb
           CALL GETGB(LUGB,0,JF2,j,JPDS,JGDS,
     *                          KF,K,KPDS,KGDS,LB2,ice,IRET)
           print*,'ice kpds=',kpds
           print*,'ice kgds=',kgds
          IF(IRET.NE.0) THEN
            ierr = 2
            WRITE(6,62)IRET
            GO TO 999
          ENDIF
   62  FORMAT(1X,' IRET =',I5)
C
  999           CONTINUE
c            close(lugb2)
             close(41)
             return
             END
      SUBROUTINE SST14K (SST,SM,GLAT,GLON)
C
      IMPLICIT REAL (A-H, O-Z)
C
c     INCLUDE "parmeta.res"
      PARAMETER (IM=237,JM=387,LM=45,LSM=39)
C
      PARAMETER  (IMY=1041,JMY=441,HD5=0.125)
C
      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
      PARAMETER  (RAD2D=57.29578E0,PI=3.141592654)
C
C
C
      DIMENSION    SST14   (IMY,JMY)
C
      DIMENSION  SST(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
      DIMENSION HLON(IM,JM),SSTOLD(IM,JM),HLAT(IM,JM)
      DIMENSION ALAT(IMY,JMY),ALON(IMY,JMY)
C
C
      DATA   INSST/41/
C
C**************************  BEGIN EXECUTION ***********************
C
      DO J=1,JMY
        DO I=1,IMY
C
C   10N,195E is the lat/lon of the SW corner point of the
C   14 km SST grid
C
          ALAT(I,J)=10.0 +(J-1)*HD5
          ALON(I,J)=195.0+(I-1)*HD5
        ENDDO
      ENDDO
C
C  READ HI RESOLUTION 14 KM OPC SST ANALYSIS, CONVERT To KELVIN
C
      DO 25 I = 1, IMY
       READ (INSST,ERR=200,END=210) (SST14(I,J),J=1,JMY)
   25 CONTINUE

      DO J = 1,JMY
        DO I = 1, IMY
          SST14(I,J) = SST14(I,J) + 273.16
        ENDDO
      ENDDO
C
C SWITCH TO EAST LONGITUDE 
C
      DO J = 1,JM
      DO I = 1,IM
        HLON(I,J) = 360.0 - (GLON(I,J)*RAD2D)
        HLAT(I,J) = GLAT(I,J) * RAD2D
      ENDDO
      ENDDO
C
C
C----  INTERPOLATE 1/8 DEG GLOBAL SATELLITE SST TO ETA GRID  -------
C
      DO J = 1,JM
      DO I = 1,IM
        SSTOLD(I,J)=SST(I,J)
      ENDDO
      ENDDO
C
C  TRY NEAREST NEIGHBOR
C
      DO J = 1,JM
      DO I = 1,IM
        JNIEB = INT(((HLAT(I,J)-10.0)*8.0)) + 1
        INIEB = INT(((HLON(I,J)-195.0)*8.0)) + 1
        IF(INIEB.GE.1.AND.JNIEB.GE.1) THEN
          IF(INIEB.LE.IMY.AND.JNIEB.LE.JMY) THEN
c
c Set only for Great Lakes
c
       if(i.ge.147.and.i.le.184.and.j.ge.165.and.j.le.200) then
            SST(I,J) = SST14(INIEB,JNIEB)
       endif
          ENDIF
        ENDIF
      ENDDO
      ENDDO
C
C   REACHING HERE MEANS 14KM SST READ OK
C
      RETURN
C
C   REACHING HERE MEANS SOMETHING IS WRONG
C
  200 CONTINUE      !  SOME KIND OF ERROR READING FILE
      WRITE(6,555) INSST
  210 CONTINUE      !  HIT UNEXPECTED END O' FILE
      WRITE(6,556) INSST
  555 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN 14 KM SST        ',
     1             'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
  556 FORMAT ('0', 'HIT UNEXPECTED END OF FILE READING 14K SST',
     1             'ON UNIT', I3, ': SKIPPING 14 KM SST FIELD.')
      RETURN
      END
