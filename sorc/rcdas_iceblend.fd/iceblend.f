	program blend
C$$$ MAIN PROGRAM DOCUMENATION BLOCK
C
C MAIN PROGRAM: ICEBLEND make ice field for NARR
C  PGRMMR: P. Shefran	ORG: NP20 DATE: February 2004
C
C  ABSTRACT: Reads ice data from various sources and make ice field initial conditions
C              with the output as a grib file in model native grid
C
C  PROGRAM HISTORY LOG:
C  2006-08-30   Ebisuzaki     Wrote doc block
C
C USAGE:
C   INPUT FILES:
C     UNIT  5  - Formatted input to select program data
C     UNIT 11  - land-sea mask
C     UNIT 12  - Canadian lake data points
C     UNIT 20  - ice climatology
C     UNIT 30  - IMS ice
C
C   OUTPUT FILES:
C     UNIT 51  - grib ice field
C
C   SUBROUTINES CALLED:
C     LIBRARIES:
C          W3LIB:  PUTGB, GETGB
C          BACIO:  baopen, baopenr
C
C ATTRIBUTES:
C    LANGUAGE: FORTRAN 95, single threaded
C
C$$$
      real dum(237,387),mask(237,387)
      integer imask(237,387)
      integer icanada(161),jcanada(161)
      real iceclim(237,387)
      real iceims(237,387)
      real iceout(237,387)
      integer kgds(25),kpds(25),jpds(25),jgds(25)
      logical*1 bit(237,387)

      read(5,111) iyr,imo,idy
111   format(3(i2,1x))
c111   format (i4,3(i2))
      print*,'iyr,imo,idy,ihr=',iyr,imo,idy,ihr
      print*,'iyr2,imo2,idy2,ihr2=',iyr2,imo2,idy2,ihr2

      iceout=0.0
c
c Read in land/sea mask
c
      read(11) dum,mask
      do j=1,387
      do i=1,237
       imask(i,j)=nint(mask(i,j))
      enddo
      enddo
c
c Read in list of Canadian ice points
c
      do i=1,161
       read(12,112) icanada(i),jcanada(i)
112    format(2(1x,i3))
       if(imask(icanada(i),jcanada(i)).eq.1) then
        imask(icanada(i),jcanada(i))=2
       endif
      enddo
      
c
c At this point we now have made the mask so that open sea has a value of 1
c and the Canadian lakes have a value of 2.  We will apply the IMS ice to
c the points with a value of 1 and the climatological ice to the points
c with a value of 2.  For the Great Lakes, a box from points (147,165) to
c (184,200) contains the Great Lakes; those will also have climatological
c values.  Point (116,152) is the Great Salt Lake.
c
c First get the climatological ice.
c
      call baopenr(20,'fort.20',ireto)
      print*,'ireto=',ireto
      jgds=-1
      jpds=-1
      jpds(5)=91
      jpds(6)=1
      jpds(7)=1
      jpds(9)=imo
      jpds(10)=idy
      print*,'jpds=',jpds
      call getgb(20,0,237*387,0,jpds,jgds,kf,k,kpds,kgds,bit
     * ,iceclim,iret)
      print*,'kpds=',kpds
      print*,'kgds=',kgds
      if(iret.ne.0) then
        print*,'climo ice iret=',iret
         stop 1
       endif
c
c Now get the IMS ice.
c
      call baopenr(30,'fort.30',ireto)
      print*,'ireto=',ireto
c	icec surface
      jpds=-1
      jgds=-1
      jpds(5)=91
      jpds(6)=1
      jpds(7)=-1 ! do not care mod 1/2007 wne
      call getgb(30,0,237*387,0,jpds,jgds,kf,k,kpds,kgds,bit
     *  ,iceims,iret)
      print*,'kpds=',kpds
      print*,'kgds=',kgds
      if(iret.ne.0) then
c	try icec msl
	jpds=-1
	jgds=-1
	jpds(5)=91
	jpds(6)=102
        call getgb(30,0,237*387,0,jpds,jgds,kf,k,kpds,kgds,bit
     *  ,iceims,iret)
        print*,'kpds=',kpds
        print*,'kgds=',kgds
	if (iret.ne.0) then
           print*,'ims ice: iret=',iret
           stop 2
        endif 
      endif

c     do j=1,387
c     do i=1,237
c      print*,'i,j,iceims=',i,j,iceims(i,j)
c     enddo
c     enddo
c
c Now start applying ice values to the iceout array.
c
      do j=1,387
      do i=1,237
       bit(i,j)=.true.
c      if(imask(i,j).gt.0) print*,'i,j,imask=',i,j,imask(i,j)
       if(imask(i,j).eq.1) then
        iceout(i,j)=iceims(i,j) ! imask.eq.1 open sea, IMS ice
c       print*,'i,j,imask,iceims,iceout=',i,j,imask(i,j),iceims(i,j),
c    *   iceout(i,j)
       endif
       if(imask(i,j).eq.2) then
        iceout(i,j)=iceclim(i,j) ! imask.eq.2 Canadian lake, clim ice
       endif
c
c Now for the Great Lakes
c
C These lines are commented because we want to use the IMS Ice values
c for the Great Lakes.  - 9 March 2004 PS
c
c      if(i.ge.147.and.i.le.184.and.j.ge.165.and.j.le.200) then
c       if(imask(i,j).eq.1) then
c        iceout(i,j)=iceclim(i,j)
c       endif
c      endif
c
c Now for the Great Salt Lake
c
      if(i.ge.113.and.i.le.119.and.j.ge.149.and.j.le.155) then
       if(imask(i,j).eq.1) then
        print*,'i,j=',i,j
        iceout(i,j)=iceclim(i,j)
       endif
      endif
      enddo
      enddo
c
c Here we should have a complete blended ice file for the day.  Write out
c the file.
c 
      kpds(3)=192
      kpds(5)=91
      kpds(6)=1
      kpds(7)=1
      kpds(8)=iyr
      kpds(9)=imo
      kpds(10)=idy
      kpds(11)=00
      kpds(21)=21
      call baopen(51,'fort.51',ireto)
      call putgb(51,237*387,kpds,kgds,bit,iceout,iret)
      print*,'putgb iret=',iret

      stop
      end
