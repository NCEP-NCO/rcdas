CMD=rcdas_fix_afwa_snow
#CC=icc
#CCFLAGS=-O2
code=fix_afwa_snow.c  readgrib.c  seekgrib.c  set_int3.c

${CMD}:	${code}
	${CC} -o ${CMD} ${CCFLAGS} ${code}

clean:
	[ -f ${CMD} ] && rm ${CMD}

