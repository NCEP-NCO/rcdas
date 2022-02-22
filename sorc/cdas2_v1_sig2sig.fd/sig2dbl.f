C-----------------------------------------------------------------------
      PROGRAM SIG2DBL
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: SIG2DBL      CHANGE SINGLE PRECISION SIGMA/SFC FILE 
C                            TO DOUBLE PRECISION, GOOD FOR R1 AND R2
C         
C   PRGMMR: EBISUZAKI        ORG: NP51        DATE: 2012-06-06
C
C ABSTRACT: NCEP/NCAR and NCEP/DOE reanalysis use single precision
C  big-endian sigma and sfc files as their archival format.  However,
C  the fortran codes expect a double precision big-endian files.
C
C  sig2dbl and sig2sngl are codes that convert between single and
C  double precision sigma/sfc files.
C
C The codes in AIX-CCS operations were written in C.  They were
C replaced by these fortran codes with the transistion to the linux 
C systems
C
C PROGRAM HISTORY LOG:
C 2004-08-11  EBISUZAKI
C 2012-06-06  EBISUZAKI   READ FILE NAMES FROM COMMAND LINE
C
C
C COMMAND LINE OPTIONS
C
C sig2dbl  in out
C          in =  input single precision sigma/sfc file
C          out = output double precision sigma/sfc file
C
C sig2sngl  in out
C           in =  input double precision sigma/sfc file
C           out = output single precision sigma/sfc file
C
C INPUT FILES:
C        ARG1 of command line
C OUTPUT FILES:
C        ARG2 of command line
C
C ATTRIBUTES
C  LANGUAGE: FORTRAN 95
C
C$$$
        integer, parameter :: nwords = 64000
        integer :: i

        integer*4, dimension(nwords) :: idata
        real*4, dimension(nwords) :: rdata
        real*8, dimension(nwords) :: ddata
        character (len=120) ::  filename

        equivalence (idata(1),rdata(1))

        call getarg(1,filename)
        i = len_trim(filename)
        write(*,*) 'input file=(',filename(1:i),')'

        open (unit=1,file=filename(1:i), access='direct',
     1          form='unformatted',recl=4)

c        open(unit=1,file=filename,access='direct',form='unformatted',recl=4)

        call getarg(2,filename)
        i = len_trim(filename)
        write(*,*) 'output file=(',filename(1:i),')'
        open(unit=50,form='unformatted',file=filename(1:i))


*       read 1st record (string)
	irec = 1
        call readrec(idata,n,irec,1)
	write(*,*) n
	write(50) idata(1:n)

*       read 2nd  record
        call readrec(idata,n,irec,1)
	write(*,*) n
	ddata(1) = rdata(1)
        ddata(6:n) = rdata(6:n)
	write(50) ddata(1),idata(2:5),ddata(6:n)
	write(*,*) 'date=',idata(2:5), ' fhour=',ddata(1)

*       read rest of records

        do while (n.gt.0)
            call readrec(idata,n,irec,1)
            if (n.gt.0) then
                ddata(1:n) = rdata(1:n)
                write(50) ddata(1:n)
            endif
	    write(*,*) n, rdata(1:3)
        enddo
        stop
        end

        subroutine readrec(idata,n,irec,iunit)

        integer*4, dimension(*) :: idata
        integer :: n, irec, i, j, iunit

        read(iunit,rec=irec,err=200) i
        irec = irec + 1
        n = i / 4
        do i = 1, n
            read(iunit,rec=irec) idata(i)
            irec = irec + 1
        enddo
        read(iunit,rec=irec) i
        irec = irec + 1
        if (i.ne.n*4) then
            write(*,*) 'format error'
            stop 99
        endif
        return

200     continue
        n = 0
        return
        end
