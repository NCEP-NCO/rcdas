#FC=ifort
# FC=ftn
FFLAGS=-O2 -convert big_endian -assume byterecl

prg=rcdas_pcpto32
src=pcpto32.f
#LIB=-L/nwprod/lib -lbacio_4 -lw3nco_4 
LIB=${BACIO_LIB4} ${W3NCO_LIB4}

${prg}:	${src}
	${FC} ${FFLAGS} -o ${prg} ${src} ${LIB}

clean:
	touch ${prg} ; rm ${prg}
