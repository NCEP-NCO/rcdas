#CC=icc
# CC=cc
#CCFLAGS:=-O2

GRIBW_SRC:=../gribw.cd
# INCLUDE=-I. -I/nwprod/lib/sorc/gribw/include
INCLUDE=-I. -I${GRIBW_SRC}/include

# LIB=-L/nwprod/lib -lgribw -lm
#LIB=${GRIBW_LIB}
LIB:=-L${GRIBW_SRC} -lgribw

CMD=rcdas_fix_rr_conv

${CMD}: rcdas_fix_rr_conv.c
	${CC} ${CCFLAGS} -o ${CMD} rcdas_fix_rr_conv.c ${INCLUDE} ${LIB} 

clean:
	touch ${CMD} ; rm ${CMD}
