#CC=icc
# CC=cc
#CCFLAGS=-O2

GRIBW_SRC=../gribw.cd
# INCLUDE=-I. -I/nwprod/lib/sorc/gribw/include
INCLUDE=-I. -I${GRIBW_SRC}/include

# LIB=-L/nwprod/lib -lgribw -lm
#LIB=${GRIBW_LIB}
LIB=-L${GRIBW_SRC} -lgribw

CMD=rcdas_gribify_cmorph

${CMD}: rcdas_gribify_cmorph.c
	${CC} ${CCFLAGS}  -o ${CMD} rcdas_gribify_cmorph.c ${INCLUDE} ${LIB}

clean:
	touch ${CMD} ; rm ${CMD}
