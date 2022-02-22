#FC=ifort
# FC=ftn
FFLAGS=-O2

src=g6tor3x.f
pgm=rcdas_g6tor3x
#LIB=-L/nwprod/lib -lbufr_4_64
LIB=${BUFR_LIB4}

${pgm}:	${src}
	${FC} ${FFLAGS} -o ${pgm}  ${src} ${LIB}

clean:
	touch ${pgm} ; rm ${pgm}
