        program pcpto32
C&&&&&&&&
C PROGRAM DOCUMENTATION BLOCK
C
CPROGRAM: narr_pcpto32
C  PRGMMR:  PERRY SHAFRAN               ORG: W/NP20    DATE: 2000-03
C
CABSTRACT: 
C   Reads precip data on 1/8th degree LDAS GRID and format for E-GRID
C
C PROGRAM HISTORY LOG:
C    2000-03   P. SHAFRAN original program
C    2001-03   P. SHAFRAN added Pentad data
C    2001-03   P. SHAFRAN added Mexico data
C    2001-11   P. SHAFRAN 32-km version
C    2001-12   P. SHAFRAN 32-km version
C                               - 32-km version:
C                               - CONUS data now in GRIB
C                               - blending of US and Canada using new LDAS mask
C                               - blending of US and Mexico using new LDAS mask
C                               - blending of Mexico and CMAP using 50/50 mask
C                               - No CMAP north of 50 degrees north
C    2004-03   P. SHAFRAN New version for 2003 and real-time R-CDAS:
C                         Uses CMORPH for oceans, NLDAS for Conus
C                         and US/Mexico for Mexico
C    2012-01   W. EBISUZAKI allow missing cmorph and us-mex data
C                          quick cleanup, minor bug fix,
C                          increase size of KPDS, KGDS
C                          enable reading us-mex precip
C                           typo incroduced with new us-mex dataset?
C    2013-01   W. EBISUZAKI fixed call to getgb
C    2014-07   W. EBISUZAKI enable reading of cmorph dataset
C
C USAGE:
C    echo "YY MM DD" | rcdas_pcpto32
C         YY, MM, DD are the date code
C
C INPUT FILES:
C    UNIT2  binary file: land sea mask
C    UNIT3  grib file: land/sea mask e-grid
C    UNIT4  grib file: blending mask
C    UNIT5   text file: line 1: "iyear  imonth  iday"
C
C    UNIT11-UNIT34: conus hourly precip on grid 192 (grib)
C    UNIT35-UNIT58: cmorph hourly precip on grid 192 (grib)
C    UNIT59-UNIT82: mexico hourly precip on grid 192 (grib)
C
C OUTPUT FILES:
C OUTPUT FILES:
C    UNIT101-108 observed precip files, hourly in 3 hour chunks
C
C
C ROUTINES CALLED
C   BAOPEN
C   GETGB
C   PUTGB    
C
CATTRIBUTES:
C   LANGUAGE: F90
C
C$$$$$$$$$$$$
c original comments
c
c Programmer: Perry Shafran
c Program date: 8 March 2000
c               7 March 2001 - added Pentad data
c              28 March 2001 - added Mexico data
c              26 November 2001 - 32-km version
c              10 December 2001 - 32-km version:
c                               - CONUS data now in GRIB
c                               - blending of US and Canada using new LDAS mask
c                               - blending of US and Mexico using new LDAS mask
c                               - blending of Mexico and CMAP using 50/50 mask
c                               - No CMAP north of 50 degrees north
c               2 March 2004    - New version for 2003 and real-time R-CDAS:
c                                 Uses CMORPH for oceans, NLDAS for Conus
c                                  and US/Mexico for Mexico
c
c Purpose: This program reads the precipitation data on the 1/8th LDAS
c grid, and then formats the data to the 80-km (or 32-km) E-grid.
c
c
c bitmap for data
c
c  bitus = T/F for conus NLDAS
c  bitm = T/F for us-mex
c  bitc = T/F for cmorph


c     parameter(nxin=464,nyin=224, jfin=nxin*nyin,jfout=12972)
c     parameter(nxin=464,nyin=224, jfin=nxin*nyin,jfout=91719)
      parameter(jfout=91719)
c
      dimension pcpout(jfout,1)
      dimension rlat(jfout),rlon(jfout)
      dimension pcpc(jfout,1),pcpm(jfout,1),pcpus(jfout,1)
      real dum1(jfout),mask(jfout)
      integer imask(jfout)
      real land(jfout)
      integer maskout(jfout)
      real amask(jfout)
      dimension alat(jfout),alon(jfout)
      integer jpds(200)
      integer kpds(200),kgds(200)
      integer ipopt(200),kpdsin(25),kpdsout(200),kgdsin(200)
      integer kgdsout(200)
      character gds(42)
      character*50 fname, fname2
      character*2 cmorph,mexico,conus
      character*3 out
      logical*1 bitout(jfout,1),bit(jfout),
     *    bitm(jfout,1),bitus(jfout,1),bitc(jfout,1)
c
c     Choose ig=190 for 80-km native grid, ig=192 for 32-km native grid
c
c     DATA IG/190/
      data ig/192/
      data kpdsin/7,152,192,192,61,1,0,00,00,00,0,0,1,0,1,4,
     *             0,1,2,0,20,1,0,0,32/
c
c Set the output bitmap
c
        read(5,111) iyr,imo,idy
111     format(3(i2,1x))
      bitout = .false.
c
c Read in the blending mask
c
      j=0
      jpds=-1
      jgds=-1
      jpds(5)=61
c     call baopen(4,'2degmaskout.grb_new',ireto)
      call baopen(4,'fort.4',ireto)
      print*,'ireto=',ireto
      call getgb(4,0,jfout,j,jpds,jgds,kf,k,kpds,kgds,
     *           bit,amask,iret)
      if(iret.ne.0) then
        print*,'kpds=',kpds
        print*,'iret=',iret
        stop 94
      endif
      do i=1,jfout
c      if(amask(i).ne.0.0) print*,'i,amask(i)=',i,amask(i)
       maskout(i)=nint(amask(i))
c      if(maskout(i).ne.0) print*,'i,maskout(i)=',i,maskout(i)
      enddo
c
c Read in Land/sea from an EGRD file
c
c     j=0
c     jpds=-1
c     jpds(5)=81
      fname='fort.3'
      call baopen(3,trim(fname),iret)
c     call getgb(2,0,jfout,j,jpds,jgds,jfout,k,kpds,kgds,
c    *            bit,land,iret)
c     if(iret.ne.0) then
c       print*,'iret=',iret
c       stop 81
c     endif
c
c Read the land-sea mask
c
      print*,'Reading land-sea mask'
      read(2) dum1,mask
      do i=1,jfout
       imask(i)=nint(mask(i))
      enddo
      j=0
      jpds=-1
      jpds(5)=176
      call getgb(3,0,jfout,j,jpds,jgds,kf,k,kpds,kgds,
     *            bit,alat,iret) 
      if(iret.ne.0) then
        print*,'iret=',iret
        stop 176
      endif
      j=0
      jpds=-1
      jpds(5)=177
      call getgb(3,0,jfout,j,jpds,jgds,kf,k,kpds,kgds,
     *            bit,alon,iret)
      if(iret.ne.0) then
        print*,'iret=',iret
        stop 177
      endif

C
C    loop over 24 hours
C
      iout=100
      do ihr=0,23
c
c Read in hourly CONUS data
c pcpus is the CONUS precip data on Grid 192
c bitm is set 

         j=0
         jpds=-1
         jpds(5)=61
         iconus=11+ihr
         write (conus,'(i2)') iconus
         fname='fort.' // conus
         print*,'fname=',trim(fname)
         call baopen(iconus,trim(fname),iret)
         call getgb(iconus,0,jfout,j,jpds,jgds,kf,k,kpdsout,kgdsout,
     *              bitus,pcpus,iret)
         print*,'jpds=',jpds
         print*,'kpds=',kpdsout
         if (iret.ne.0) then
            print*,'iret=',iret
c           stop 10
            print *, 'NO NLDAS precip for this date'
            bitus = .FALSE.
         endif
         do i = 1, jfout
             if (bitus(i,1)) then
                 if (pcpus(i,1) .lt. 0.0) bitus(i,1) = .false.
             endif
         enddo

c
c The CONUS data is in mm/hr.
c
c       print*,'ihr pentad=',ihr
C        do i=1,jfout
c        if(alat(i).ge.41.and.alat(i).le.43.and
c    *    .alon(i).ge.268.and.alon(i).le.272) then
c        print*,'i,pcpus(i,1),bitus=',i,pcpus(i,1),bitus(i,1)
c        endif
c        pcpus(i,1)=pcpus(i,1)*3600.0
c        if(pcpus(i,1).ne.0.0) print*,'i,pcpus(i,1)=',i,pcpus(i,1)
c        if(pcpus(i,1).lt.0.0) bitus(i,1)=.f.
c        if(pcpus(i,1).lt.0.0) pcpus(i,1)=0.
C        enddo
c

c
c        Read in Mexico data
c        pcpm is the Mexico precip data on Grid 192
c
c
         j=0
         jpds=-1
C WNE 1/2012         jpds(5)=35
         jpds(5)=59
         imexico=59+ihr
         write (mexico,'(i2)') imexico
         fname='fort.' // mexico
         print *,'fname=',trim(fname)
         call baopen(imexico,trim(fname),iret)
         call getgb(imexico,0,jfout,j,jpds,jgds,kf,k,kpdsout,kgdsout,
     *              bitm,pcpm,iret)
         print*,'jpds=',jpds
         print*,'kpds=',kpdsout
         if (iret.ne.0) then
            print*,'us-mex read: iret=',iret
c           stop 70
            print *, 'NO US-MEX precip for this date'
            bitm=.false.
         endif
c
c        The Mexico data is in mm/s, so we need to multiply by 3600 to get mm/hr
c
c        print*,'ihr pentad=',ihr
         do i=1,jfout
            if (bitm(i,1)) then
               if (pcpm(i,1).lt.0.0) bitm(i,1)=.false.
               pcpm(i,1)=pcpm(i,1)*3600.0
            endif
         enddo

c        Read in CMORPH data
c        pcpc is the CMORPH precip data on Grid 192
c        bitmask: bitc

         j=0
         jpds=-1
C WNE 8/2014         jpds(5)=35
         jpds(5)=59
         icmorph=40+ihr
         write (cmorph,'(i2)') icmorph
         fname='fort.' // cmorph
         call baopen(icmorph,trim(fname),iret)
         call getgb(icmorph,0,jfout,j,jpds,jgds,kf,k,kpdsout,kgdsout,
     *              bitc,pcpc,iret)
         if(iret.ne.0) then
            print*,'iret=',iret
c           stop 40
            bitc = .false.
         endif
c
c The CMORPH data is in mm/s, so we need to multiply by 3600 to get mm/hr
c
c         print*,'ihr pentad=',ihr
          do i=1,jfout
            if (bitc(i,1)) then
               if (pcpc(i,1) < 0.0) then
                  pcpc(i,1) = 0.0
                  bitc(i,1) = .false.
               else
                  pcpc(i,1)=pcpc(i,1)*3600.0
               endif
            endif
          enddo

c       ifile=ifile+1

c
c Now that we have interpolated, put in the CMORPH data using the mask
c Apply Pentad data everywhere, except North of 50 degrees N latitude,
c because the CMORPH data's reliability decreases as one goes North.
c
         
          do i=1,jfout
            if (alat(i).lt.50.0.and.bitc(i,1)) then
               pcpout(i,1)=pcpc(i,1)
               bitout(i,1)=.TRUE.
            else
               pcpout(i,1) = 0.0
               bitout(i,1)=.FALSE.
            endif
          enddo 

          do i=1,jfout
c           modify land precip
	    if (imask(i).eq.0) then
c
c Next apply the US/Mexico precipitation on land.
c
                if (bitm(i,1)) then
                   bitout(i,1) = .TRUE.
                   pcpout(i,1) = pcpm(i,1)
                endif
c
c Now we apply the NLDAS precipitation over the US.
c
                if (maskout(i).gt.15.5 .and. bitus(i,1)) then
                   pcpout(i,1) = pcpus(i,1)
                   bitout(i,1) = .TRUE.
                endif

c turn off northern data

                if (alat(i).gt.45.0 .and. maskout(i).lt.16.0) then
                   pcpout(i,1) = 0.0
                   bitout(i,1)=.FALSE.
                endif

c         
c Put in the Mexico data using the land/sea value and the mask.  Use the 
c blending between 25 and 35 degrees North latitude, using the weights
c from the blending mask.
c

              if (alat(i).ge.25.0.and.alat(i).le.35.0) then
c
c The if blocks take care of what to do if there is missing CONUS
c If CONUS is missing, block out all data in CONUS
c If CONUS is not missing, then calculate the precipitation blending as
c before.
c
                 if (bitm(i,1)) then
                    pcpout(i,1) = pcpm(i,1)
                    bitout(i,1) = .TRUE.
                  endif
                  if (bitus(i,1) .and. bitm(i,1)) then
                    pcpout(i,1)=(float(maskout(i))/16.0)*pcpus(i,1)+
     *                ((16.0-float(maskout(i)))/16.0)*pcpm(i,1)
                    bitout(i,1)=.TRUE.
                 endif
              endif
c
c Between 15 and 17 degrees North latitude, blend the CMAP and the Mexico
c data using a 50/50 blending mask.
c
c comments 1/2012 WNE: change
c old        if(alat(i).ge.15.0.and.alat(i).le.17.0.and. 
c old    *      pcpm(i,1).ge.0.0.and.pcpc(i,1).ge.0.0) then
c
           if (alat(i).ge.15.0.and.alat(i).le.17.0.and. 
     1         bitm(i,1) .and. bitc(i,1)) then

              pcpout(i,1) = 0.5*(pcpm(i,1)+pcpc(i,1))
c         if(iyr.eq.78) pcpout(i,1)=pcpm(i,1)  ! No CMAP data in 1978
              if(pcpout(i,1).lt.0) then
                 print*,'Mexico/CMAP blend'
                 print*,'i,pcpout(i,1)=',i,pcpout(i,1)
              endif
              bitout(i,1)=.TRUE.
           endif 
c
c
c Make sure land north of 50 N is masked out
c
           if (alat(i).ge.50.0) bitout(i,1)=.FALSE.

        endif  ! iland(i).eq.1 if block
      enddo   ! 1,jfout loop
c
c 

c
c Do a QC to filter out obs that are too high (about 40 mm/hr)
c
        do i=1,jfout
	   if (bitout(i,1) .and. pcpout(i,1) .gt. 24.0) then
              bitout(i,1) = .false.
              pcpout(i,1) = 0.0
           endif
        enddo

C       write out hourly grib file

c       KPDS1 = KPDS0
c       KPDS1(3) = IG
c       KPDS1(22) = 1
        kpdsout(1:25) = kpdsin
        kpdsout(26:200) = -1

        kpdsout(3)=ig
        kpdsout(22)=1
        kpdsout(21)=21
        kpdsout(22)=3
        kpdsout(8)=iyr
        kpdsout(9)=imo
        kpdsout(10)=idy
        kpdsout(11)=ihr
        if(mod(ihr,3).eq.0) then
           iout=iout+1
           print*,'iout=',iout
           write (out,'(i3)') iout
           fname2='fort.' // out
           call baopen(iout,trim(fname2),iret)
        endif
c       iout=iout+1
        print*,'jfout=',jfout
        print*,'kpdsout=',kpdsout(1:25)
        print*,'kgdsout=',kgdsout(1:25)
c       print*,'pcpout=',pcpout
c       print*,'bitout=',bitout
        call putgb(iout,jfout,kpdsout,kgdsout,bitout,pcpout,iret)
c       close(iout)
        WRITE(6,*) 'FINISHED PUTGB, ihr,UNIT #, IRET=',iout, ihr,IRET

      enddo
      stop
      end
C-----------------------------------------------------------------------
