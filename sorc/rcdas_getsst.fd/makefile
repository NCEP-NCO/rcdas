#FC=ifort
# FC=ftn
FFLAGS=-O2  -convert big_endian -assume byterecl
sorc=SST.f
pgrm=rcdas_getsst
# LIB=-L/nwprod/lib -lw3nco_4 -lbacio_4 -lip_4
LIB=${W3NCO_LIB4} ${BACIO_LIB4} ${IP_LIB4}

${pgrm}:	${sorc}
	${FC} ${FFLAGS} -o ${pgrm} ${sorc} ${LIB}

clean:
	touch ${pgrm} ; rm ${pgrm}
