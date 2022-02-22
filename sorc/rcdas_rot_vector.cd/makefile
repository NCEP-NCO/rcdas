#CC=icc
# CC=cc
#CCFLAGS=-O2

# INCLUDE=-I. -I/nwprod/lib/sorc/gribw/include
INCLUDE=-I. -I../gribw.cd/include
# LIB=-L/nwprod/lib -lgribw -lm
LIB=-L../gribw.cd -lgribw
CMD=rcdas_rot_vector

${CMD}:	rcdas_rot_vector.c
	${CC} ${CCFLAGS} -o ${CMD} rcdas_rot_vector.c ${INCLUDE} ${LIB}

clean:
	touch ${CMD} ; rm ${CMD}
