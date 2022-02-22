#FC=ifort
# FC=ftn
FFLAGS=-O2 -convert big_endian -assume byterecl

src=SNOHIRES.f SNO8GET.f PRINTETA.f PRINTAF.f
#LIBS=-L/nwprod/lib -lbacio_4 -lw3emc_4 -lw3nco_4 -lip_4
LIBS=${BACIO_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${IP_LIB4}
pgm=rcdas_snowget

${pgm}:	${src}
	${FC} ${FFLAGS} -o ${pgm} ${src} ${LIBS}

clean:
	touch ${pgm} ; rm ${pgm}
