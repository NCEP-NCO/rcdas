#FC=ifort
# FC=ftn
FFLAGS=-O2 -convert big_endian -assume byterecl
sorc=window1b_m1b.f
# LIB=-L/nwprod/lib -lw3nco_4
LIB=${W3NCO_LIB4}
pgrm=rcdas_window1b_m1b

${pgrm}:	${sorc}
	${FC} ${FFLAGS} -o ${pgrm} ${sorc} ${LIB}

clean:
	touch ${pgrm} ; rm ${pgrm}
