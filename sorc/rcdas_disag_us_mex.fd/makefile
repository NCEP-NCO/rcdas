#FC=ifort
# FC=ftn
FFLAGS=-O2 -convert big_endian -assume byterecl -g -traceback

prog=rcdas_disag_us_mex

${prog}:	${prog}.f90
	${FC} ${FFLAGS} -o ${prog} ${prog}.f90

clean:
	touch ${prog} ; rm ${prog}
