# LIBS=-L$NWROOT/lib  -lw3nco_d -lbufr_d_64
LIBS=${W3NCO_LIBd} ${BUFR_LIBd}

f=r2_postvents
target=cdas2_v1_postvents

#FC=ifort
FFLAGS=-O2 -r8 -convert big_endian -assume byterecl

${target}:	$f.f
	${FC} -o ${target} ${FFLAGS} $f.f ${LIBS} 

clean:
	touch ${target} ; rm ${target}
