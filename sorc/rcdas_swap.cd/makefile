#CC=icc
#CCFLAGS=-O2

CMD=rcdas_swap

${CMD}:	swap.c
	${CC} ${CCFLAGS} -o ${CMD} swap.c ${INCLUDE} ${LIB}

clean:
	touch ${CMD} ; rm ${CMD}
