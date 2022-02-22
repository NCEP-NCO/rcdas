SHELL=/bin/sh
#
src=rrvents.f
#FC = ifort
# LIBS = -L/nwprod/lib -lbufr_4_64
LIBS = ${BUFR_LIB4}

pgm = rcdas_rrvents

# FFLAGS = -O2 -qstrict -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -q nosave -qspillsize=548 -bloadmap:lm
# FFLAGS = -C -O2 -qstrict -qrealsize=4 
FFLAGS = -O2 -convert big_endian -assume byterecl

${pgm}:	${src}
	$(FC) $(FFLAGS) -o ${pgm} $(src) $(LIBS)

clean:
	touch ${pgm} ; rm ${pgm}

