      program extrtv1b
c$$$  main program documentation block
c                .      .    .                                       .
c main program: EXTRTV1B     time-windows and dupl-checks tovs1b data
C   PRGMMR: KATZ             ORG: NP2         DATE: 1998-04-30
c
c abstract: reads in time-windowing specifications and list of tovs1b 
c   orbital files from standard input.  the tovs1b reports are then
c   read in from their separate files and checked against the time
c   window.  reports which satisfy the time window are checked for 
c   duplicates and written out to the dump file.
c
c program history log:
c   98-01-20  bert b. katz , np2  
c   98-04-10  bert b. katz , np2 -- code is now y2k compliant
c   98-04-27  bert b. katz , np2 -- year, month, day separated to 
c                                   achieve true y2k compliance.
c                                   converted to fortran 90 to use
c                                   kind parameter for long integers
c                                   in date comparisons.
c   01-01-24  treadon, russ, np3 -- bug fix for maximum time test
c   01-02-15  bert b. katz , np2 -- bug fix for rejection counts 
c
c usage:
c
c   input files:
c     unit 05  - standard input : namelist contains number of input 
c                files, satellite id, time-windowing specifications, 
c                line and spot sampling factors and the output file.
c                a list of tovs1b input orbital files follows the 
c                end of the namelist.
c     unit 11  - binary tovs1b orbital files
c
c   output files:
c     unit 06  - printout
c     unit 51  - binary tovs1b dump file
c
c   subprograms called: none
c
c   exit states:
c     cond =   0 - successful run
c          =  11 - error reading input hirs file        
c
c attributes:
c   language: cray cft77 fortran
c   machine:  cray
c
c$$$
c     Set parameters
      parameter (mxreal=20,mxchan=30)
      parameter (mxdata=mxreal+mxchan)
c      
c     Declare variables
      character*8 statid
      character*100 cfile,ofile
      integer stdin, stdout
      integer ihead(18000),ihash(180000),link(-18000:180000)
      integer ichan(mxchan)
      integer (kind=selected_int_kind(12)) iwin,imin,imax
      integer(8) iyrmax,imomax,idymax,ihrmax,imimax
      integer(8) iyrmin,imomin,idymin,ihrmin,imimin
      integer(8) iyr,imo,idy,ihr,imi
      real rdata(mxdata)
c
c     Declare namelist input
      namelist /input/ nfile,isat,iyrmax,imomax,idymax,ihrmax,
     x     imimax,iyrmin,imomin,idymin,ihrmin,imimin,iskipl,
     x     iskips,ofile
c
c     Initialize variables.
      data stdin, stdout /  5,  6 /
      data lunin, lunout / 11, 51 /
      data lungrd        / 61 /
      data link / 198001*0 /
      data nhirs, nmsu / 20, 4 /
c
c***************************************************************************
c	Start extrtovs here
c
      CALL W3TAGB('EXTRTV1B',1998,0120,0078,'NP2    ')
c
c
c     Initialize variables.
      npass = 0
      nkeep = 0
      ncnt  = 0
      nwrite= 0
      ndupe = 0
      nexcl = 0
      ntmin = 0
      ntmax = 0
      nsat  = 0
      ncldy = 0
      nchnl = 0
      nrec  = 0      
      iret  = 0 
c
c     Flag to activate (=1) creation of GrADS station data file.
c     Currently, the data file only contains the obs (lat,lon)
      igrads = 0
c
c
c     Get user input      
      read(stdin,input)  
      write(stdout,*)' '
      write(stdout,*)'Run extrtv1b with variable settings below'
      write(stdout,input)
      write(stdout,*)
c
c
c     Set min,max limits for time windows.
      imax = iyrmax*100000000_8 + imomax*1000000_8 + idymax*10000_8 + 
     x     ihrmax*100_8 + imimax
      imin = iyrmin*100000000_8 + imomin*1000000_8 + idymin*10000_8 + 
     x     ihrmin*100_8 + imimin
      write(stdout,*)'max window=',imax,' min window=',imin
c     
c
c     Open unit to output file.
      open(unit=lunout,file=ofile,form='unformatted')
c
c
c     Big loop over number of input files.
      do ifile = 1,nfile
         read(stdin,'(A)') cfile
c	   
c        Open unit to input file
         open(unit=lunin,file=cfile,form='unformatted')
         write(stdout,*)'file ',cfile,' opened ok'
c
c        Read header record
         read(lunin,err=98,end=99) nreal,nchan,(ichan(i),i=1,nchan)
         ndata = nreal + nchan
c
c        If this is the first input file, read, write header
c        record to the output file.  NOTE:  This code assumes
c        all input files have the same structure (ie, the same
c        header record).
         if (ifile.eq.1) then
            write(stdout,*)'write header record to output file'
            write(lunout) nreal,nchan,(ichan(i),i=1,nchan)
            write(stdout,*)'nreal,nchan=',nreal,nchan
            write(stdout,*)'ichan below'
            write(stdout,*) (ichan(i),i=1,nchan)
         endif	 
c
c        Initialize number of scan lines read to 1.     
c        Initialize 1st obs skip flag to off
         iline  = 1
         ifirst = 1
         iline2 = 0
c
c        Set number of scan steps, channel for gross check,
c        and obs skipping for every other good line.
         if (nchan.eq.nhirs) then
            nspot = 56
            ichek = 14 + 8
            iskip = 2*iskipl
            iskip = max(2,iskip)
         elseif (nchan.eq.nmsu) then
            nspot = 11
            ichek = 14 + 2
            iskip = 1
         else
            write(stdout,*)'EXTRTV1B ERROR:  invalid number of channels'
            write(stdout,*)'   nchan = ',nchan
            call errexit(12)
            CALL W3TAGE('EXTRTV1B')
         endif
c         
c        Top of loop to read over all lines in currently opened
c        input data file.
   10    continue
         ngood = 0
c    
c        Loop over number of spots
         do ispot = 1,nspot
            read(lunin,err=98,end=99) (rdata(i),i=1,ndata)
c
c           Increment record counter.
            nrec = nrec + 1
c
c           Thin data as a function of line number.
c           Take every iskipl-th line.
            if (mod(iline,iskipl).ne.0) goto 90
            if (ispot.eq.1) iline2 = iline2 + 1
            if ( (mod(iline2,iskipl).eq.0) .and.
     x           (ispot.eq.1) ) then
               ngood = 1
            endif
c
c           Set satellite id.
            isatid = rdata(1)
            instru = rdata(2)
c  
c           Construct packed date/time stamp for data just read.
            iyr  = rdata(3)
            imo  = rdata(4)
            idy  = rdata(5)
            ihr  = rdata(6) / 3600
            imi  = (rdata(6)-ihr*3600) / 60
            if(iyr.lt.21) then
              iyr = iyr + 2000
            else if(iyr.lt.100) then
              iyr = iyr + 1900
            endif
            iwin = iyr*100000000_8+imo*1000000_8+idy*10000_8+
     .             ihr*100_8+imi
c
c           Apply time window, satellite id, line skip, and gross
c           error check.  These tests are done here merely for 
c           bookkeeping purposes.  Data failing all tests are tossed
c           below.
            if (iwin.lt.imin)              ntmin = ntmin + 1
            if (iwin.ge.imax)              ntmax = ntmax + 1
            if (isatid.ne.isat)            nsat  = nsat  + 1
            if (abs(rdata(ichek)).ge.500.) nchnl = nchnl + 1
c
c           Toss data if fail any test.
            if ( (iwin.ge.imin) .and. 
     x           (iwin.lt.imax) .and.
     x           (isatid.eq.isat) .and. 
     x           (abs(rdata(ichek)).lt.500.) ) then
               npass = npass + 1
               ngood = ngood + 1
               if (mod(ngood,iskips).eq.0) then
                  nwrite = nwrite + 1
                  write(lunout) (rdata(i),i=1,ndata)
               endif
            else
               nexcl = nexcl + 1 
            endif
c
c           Jump here if skipping this entire scan line
 90         continue
c
c        End of loop over scan line
         end do
c
c        Increment line number
         iline = iline + 1
c
         go to 10
c
c        Error detected reading current input file.
 98      continue
         write(stdout,*)' unexpected end of record on file:',ifile
         iret = 11
         go to 100
c
c        EOF read from input file.
99       continue
         write(stdout,*)' end of file found for file:',ifile
c
c        Write running totals to standard out.
100      continue
         write(stdout,*)'total records read so far =',nrec
         write(stdout,*)'total passed so far       =',npass  
         write(stdout,*)'total written so far      =',nwrite
         write(stdout,*)'total duplicates so far   =',ndupe 
         write(stdout,*)'total excluded so far     =',nexcl 
         write(stdout,*)'iline,iline2              =',iline,iline2

         write(stdout,*)' '
         write(stdout,*)'total fail min time test, ntmin=',ntmin
         write(stdout,*)'total fail max time test, ntmax=',ntmax
         write(stdout,*)'total fail sat id test,   nsat =',nsat
         write(stdout,*)'total fail cloud test,    ncldy=',ncldy
         write(stdout,*)'total fail channel test,  nchnl=',nchnl
c
c        Close unit to current input file.
         close(lunin)
c
c     End of loop over input files.
      end do	
c
c     Write terminator record for grads station file.
      if (igrads.eq.1) then
         statid = 'test'
         xlat = 0.0
         xlon = 0.0
         rtim = 0.0
         nlev = 0
         nflg = 0
         write(lungrd) statid,xlat,xlon,rtim,nlev,nflg
      endif
c
c     Close unit to output file.	
      close(lunout)
c
c
      if (iret.eq.0) then
         CALL W3TAGE('EXTRTV1B')
         stop
      else
         CALL W3TAGE('EXTRTV1B')
         call errexit(11)
      endif
c
c     End of program
c
      end
