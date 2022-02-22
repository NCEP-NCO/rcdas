      program window1b
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: window1b      creates satellite ieee file with dat from time1 to time2
C   PRGMMR: Russ Treadon           ORG: NP22        DATE: 1998-10-2
C
C ABSTRACT: READ IEEE FILE AND EXTRACTS TOVS1B DATA FROM TIME1 TO TIME2
C   customized for specific satellite
C
C PROGRAM HISTORY LOG:
C   1998-10   RUSS TREADON
C   ????-??   MODIFIED FOR NARR?
C
C USAGE:
C
C   INPUT FILES:
C     UNIT5  - TEXT: FILENAME AND TIME
C     UNIT11 - IEEE: SATELLITE DATA
C     UNIT12 - IEEE: SATELLITE DATA
C
C   OUTPUT FILES:
C     UNIT51     - IEEE OUTPUT
C
C   SUBPROGRAMS CALLED:
C     W3MOVDAT
C     W3FS21
C     UFBSEQ
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C   REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: STANDARD FORTRAN 90
C
C$$$

c
c     File    :  window1b.f
c     Written :  2 October 1998, Russ Treadon
c     Abstract:  create an ssi-ready tovs1b data file 
c                containing data from time1 to time2
c
c     Set parameters
      parameter (mdata=18)
c
c     Declare variables
      integer stdin,stdout
      integer ibdate(5),iedate(5),ida(8),jda(8)
      integer idate5(5)
      real data1b(mdata),fha(5)
      character*60 filnam(2)
c
c     Set i/o units
      data stdin,stdout /  5,  6 /
      data lunin,lunout / 10, 51 /
c
c
c*************************************************************************     
c     Start window here
c     print*,'WINDOW OUTPUT'
c
c     Get user input from stdin

c      read(stdin,111) filnam(1),filnam(2),idate,iwin
c111   format(a60,1x,a60,1x,i10,1x,i1)

      read(stdin,*) filnam(1),filnam(2),idate,iwin

      print*,'filnam(1),filnam(2),idate,iwin=',
     * filnam(1),filnam(2),idate,iwin
c     print*,'filnam(1)=',filnam(1)
c     print*,'filnam(2)=',filnam(2)
c     print*,'idate=',idate
c     print*,'iwin=',iwin
c     stop
c
c
c     Convert central time and window to beginning and end times.
      jdate = idate
      iyr   = int(jdate/1.e6 + 0.5)
c     jdate = jdate - iyr*1e6
      jdate = jdate - iyr*1000000
      imo   = int(jdate/1.e4 + 0.5)
      jdate = jdate - imo*1e4
      idy   = int(jdate/1.e2 + 0.5)
      jdate = jdate - idy*1e2
      ihr   = jdate

      jda    = 0
      ida    = 0
      ida(1) = iyr
      ida(2) = imo
      ida(3) = idy
      ida(4) = 0
      ida(5) = ihr
      write(stdout,*)' '
      write(stdout,1000)'middle date:',iyr,imo,idy,ihr,0,iwin

      fha    = 0.0
      fha(2) = -1.*iwin/2.
c     print*,'fha=',fha

      call w3movdat(fha,ida,jda)

      ibdate(1) = jda(1) ! year
      ibdate(2) = jda(2) ! month
      ibdate(3) = jda(3) ! day
      ibdate(4) = jda(5) ! hour
      ibdate(5) = jda(6) ! minute
c     print*,'ibdate=',ibdate
      call w3fs21(ibdate,nbeg)
      write(stdout,1000)'begin date:',ibdate,nbeg
 1000 format(a15,1x,5(i5,1x),i12)

c
c
c     Compute end time for window.
      jda = 0
      fha = 0.0
      fha(2) = +1.*iwin/2.
c     print*,'fha=',fha
      call w3movdat(fha,ida,jda)
      iedate(1) = jda(1) ! year
      iedate(2) = jda(2) ! month
      iedate(3) = jda(3) ! day
      iedate(4) = jda(5) ! hour
      iedate(5) = jda(6) ! minute
c     print*,'iedate=',iedate
      call w3fs21(iedate,nend)
      write(stdout,1000)'end date:',iedate,nend
      write(stdout,*)' '
      write(stdout,*)'Searching from ',nbeg,' to ',nend,' minutes'
      write(stdout,*)' '

c
c
c     Big loop over two input files.  Find all observations
c     between user specified start and end times.
c
      ifirst = 1
      nall = 0
      nout = 0
      do ifile = 1,2
         lunin = 10 + ifile
         print*,'filnam(ifile)=',filnam(ifile)
         open(lunin,file=filnam(ifile),form='unformatted')
         nrec  = 0
c
c        Read header record
         if(ifile.eq.1) read(lunin,end=1110) nreal,nchanl
         if(ifile.eq.2) read(lunin,end=110) nreal,nchanl
c        nreal=0
c        nchanl=0
c        ndata=34
         print*,'nreal,nchanl=',nreal,nchanl
         ndata = nreal + nchanl
         print*,'ndata=',ndata
c        if (ndata.gt.mdata) then
c           write(6,*)'***ERROR*** ndata>mdata'
c           write(6,*)' ndata,mdata=',ndata,mdata
c           stop '***ERROR*** ndata>mdata'
c        endif
c        write(99,*)'read header record from ',ifile,lunin
c        write(99,*)'    nreal,nchanl,ndata = ',nreal,nchanl,ndata
c
c
c        If first time in loop, write header record to ouptut
c        file.
         if (ifirst.eq.1) then
            ifirst = 0
            write(lunout) nreal,nchanl
c           write(99,*)' ' 
c           write(99,*)'write header record to lunout=',lunout
c           write(99,*)'nreal,nchanl=',nreal,nchanl
c           write(99,*)'set ifirst  =',ifirst
c           write(99,*)' '
           endif
c
c
c
c        Read data record from input file.
 10      continue
         read(lunin,end=100) (data1b(i),i=1,ndata)
c        print*,'input data1b=',data1b
         nall = nall + 1
         nrec = nrec + 1
c
c        Extract observation time.  Convert to absolute time
         iyr  = data1b(3)
c        iyr  = data1b(1)
         if (iyr.ge.0.and.iyr.le.99) then
            if (iyr.gt.51) then
               iyr = iyr + 1900
            else
               iyr = iyr + 2000
            endif
         endif
         imo  = data1b(4)
         idy  = data1b(5)
         ihr  = data1b(6)/3600.
         imi  = (data1b(6) - ihr*3600)/60.
         isc  = (data1b(6) - ihr*3600) - imi*60
c        imo  = data1b(2)
c        idy  = data1b(3)
c        ihr  = data1b(4)/3600.
c        ihr  = data1b(4)
c        imi  = data1b(5)
c        isc  = data1b(6)
c        imi  = (data1b(4) - ihr*3600)/60.
c        isc  = (data1b(4) - ihr*3600) - imi*60
c        print *,nrec,ifile,data1b(2),data1b(3),data1b(4),data1b(5),
c    *     data1b(6)
c        print*,nrec,ifile,iyr,imo,idy,ihr,imi,isc
         idate5(1) = iyr
         idate5(2) = imo
         idate5(3) = idy
         idate5(4) = ihr
         idate5(5) = imi
c        print*,'ihr,data1b(4)=',ihr,data1b(4),data1b(4)/3600.
c        print*,'idate5=',idate5
c        print*,'scanpos=',data1b(15)
c        do i=1,34
c         print*,'i,data1b(i)=',i,data1b(i)
c        enddo
         call w3fs21(idate5,nobs)
         nobs = nobs + isc/60.
c
c
c        Does obs fall within time window?  If so, write
c        to output file.
c        print*,'nbeg,nobs,nend=',nbeg,nobs,nend
         if ( (nbeg.le.nobs) .and. (nobs.le.nend) ) then
c           time=nobs/60.
c           print*,'nobs/60=',time
c        do i=1,34
c         print*,'i,data1b(i)=',i,data1b(i)
c        enddo
         rlat=data1b(9)
         rlon=data1b(10)
         if(rlat.ge.-5.0.and.rlat.le.90.0.
     *       and.rlon.ge.-180.0.and.rlon.lt.0.0) then
c        do i=1,18
c         print*,'i,data1b(i)=',i,data1b(i)
c        enddo
            write(lunout) (data1b(i),i=1,ndata)
         endif
c           print*,'output data1b=',data1b
            nout = nout + 1
         endif
c
c        End of loop
         goto 10
c
c
c        Read eof from data record
 100     continue
         write(stdout,*)'read eof for lunin=',lunin,' with nrec=',nrec
         close(lunin)
         go to 1111
c
c        Error reading header record for first file
1110     continue
         write(stdout,*)'***ERROR*** Can not read header record '
         write(stdout,*)'of first file; read second file'
c
c     End of loop over input files
1111  continue
      end do
      write(stdout,*)' '
      write(stdout,*)'read  nall=',nall,' data records'
      write(stdout,*)'wrote nout=',nout,' data records'
      write(stdout,*)' '
      goto 200
c
c
c     Error reading header record
 110  continue
      write(stdout,*)'***ERROR*** can not read header record'
c
c
c     End of program
 200  continue
      close(lunout)
      stop
      end
