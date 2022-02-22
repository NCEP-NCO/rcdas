SHELL=/bin/sh
#FC = ifort
# FC = ftn

# LIBS =    -L/nwprod/lib -lw3nco_4 -lw3emc_4
LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4}

OBJS=	BOCO32_6h.o BC6432.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#

# LDFLAGS = -bD:2000000000 -bmap:map  -bnoquiet

CMD = rcdas_bctend

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
# FFLAGS = -O2 -qstrict -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -q nosave -qspillsize=548 -bloadmap:lm
FFLAGS=-O2 -convert big_endian -assume byterecl -auto-scalar -fp-model precise

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) ${FFLAGS} -o $(@) $(OBJS) $(LIBS)

clean:
	touch junk.lst ${CMD} junk.o
	rm ${CMD} *.o *.lst
