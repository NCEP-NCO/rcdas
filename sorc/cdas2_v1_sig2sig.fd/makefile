#FC=ifort
FFLAGS=-O2 -convert big_endian -assume byterecl
TARGET1=cdas2_v1_sig2dbl
TARGET2=cdas2_v1_sig2sngl


all:	${TARGET1} ${TARGET2}

${TARGET1}: sig2dbl.f
	${FC} -o ${TARGET1} ${FFLAGS} sig2dbl.f

${TARGET2}:	sig2sngl.f
	${FC} -o ${TARGET2} -O2 ${FFLAGS} sig2sngl.f

clean:
	touch ${TARGET1} ${TARGET2} ; rm ${TARGET1} ${TARGET2}
