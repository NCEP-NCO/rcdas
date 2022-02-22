#FC=ifort
# FC=ftn
FFLAGS=-O2
src=grtorr2.f
pgm=rcdas_grtorr2
#LIB=-L/nwprod/lib -lbufr_4_64
LIB=${BUFR_LIB4}

${pgm}:	${src}
	${FC} ${FFLAGS}  -o ${pgm} ${src} ${LIB}

clean:
	touch ${pgm} ; rm ${pgm}
