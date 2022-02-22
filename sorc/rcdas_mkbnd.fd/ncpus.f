C-----------------------------------------------------------------------
      FUNCTION NCPUS()
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: NCPUS          SET NUMBER OF CPUS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-08-19
C
C ABSTRACT: SET NUMBER OF CPUS 
C   DESIGNATING THE NUMBER OF PROCESSORS OVER WHICH TO PARALLELIZE.
C
C PROGRAM HISTORY LOG:
C   94-08-19  IREDELL
C   98-11-09  VUONG     ADD DOC BLOCK AND REMOVE CRAY REFERENCES
C 1998-12-18  IREDELL   IBM SMP VERSION
C 2012-10-02  EBISUZAKI splib uses openmp .. here ncpus=1
C
C USAGE:    NC=NCPUS()
C   OUTPUT ARGUMENTS:
C     NCPUS        INTEGER NUMBER OF CPUS
C
C SUBPROGRAMS CALLED:
C     NUM_PARTHDS  XLF INTRINSIC TO RETURN NUMBER OF THREADS
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN

	character*16 thrdval
	integer nval
C
C$$$
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Cmp      NCPUS=NUM_PARTHDS()

	call get_environment_variable("OMP_NUM_THREADS",thrdval)
	read(thrdval(1:3),73) nval
  73    format(I3)
	NCPUS=nval
C        NCPUS=1
      RETURN
      END
