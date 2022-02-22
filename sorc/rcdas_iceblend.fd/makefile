#FC=ifort
# FC=ftn
FFLAGS=-O2 -convert big_endian -assume byterecl

#LIBS=-L/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4
LIBS=${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

pgrm=rcdas_iceblend

${pgrm}:	iceblend.f
	${FC} ${FFLAGS} -o ${pgrm} iceblend.f ${LIBS}

clean:
	touch ${pgrm} ; rm ${pgrm}
