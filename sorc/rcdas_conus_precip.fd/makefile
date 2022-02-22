#FC=ifort
# FC=ftn

# -L/usr/lib -lessl -lxlsmp
# lib=-L/nwprod/lib -lw3nco_4 -lip_4 -lbacio_4  -lsp_4
lib=${W3NCO_LIB4} ${IP_LIB4} ${IP_LIB4} ${BACIO_LIB4} ${SP_LIB4}

sorc=precforcev2.f
pgrm=rcdas_conus_precip


FFLAGS=-O2 -qopenmp -convert big_endian -assume byterecl -g -traceback

${pgrm}:	${sorc}
	${FC} ${FFLAGS} -o ${pgrm} ${sorc} ${lib}

clean:
	touch ${pgrm} ; rm ${pgrm}
