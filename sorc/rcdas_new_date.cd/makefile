#CC=icc
#CC=cc
#CCFLAGS=-O2

# INCLUDE=-I. -I/nwprod/lib/sorc/gribw/include
INCLUDE=-I. -I../gribw.cd/include

# LIB=-L/nwprod/lib -lgribw
LIB=-L../gribw.cd -lgribw
CMD=rcdas_new_date

${CMD}: new_date.c
	${CC} ${CCFLAGS} -o ${CMD} new_date.c ${INCLUDE} ${LIB}

clean:
	touch ${CMD} ; rm ${CMD}
