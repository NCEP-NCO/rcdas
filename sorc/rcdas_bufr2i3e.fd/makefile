#FC=ifort
# FC=ftn
# LIBS=-L/nwprod/lib -lbufr_4_64
LIBS=${BUFR_LIB4}


FFLAGS=-convert big_endian -assume byterecl

SRC=bufr2i3e.f
CMD=rcdas_bufr2i3e

${CMD}:	${SRC}
	${FC} ${FFLAGS} -o ${CMD} ${SRC} ${LIBS}

clean:
	touch ${CMD} ; rm ${CMD}
