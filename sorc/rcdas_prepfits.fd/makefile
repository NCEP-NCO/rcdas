SHELL=/bin/sh
#
SRCS=	CALCAPE.f   getfct.f    ireadpb.f   raddate.f   ufbxy3.f \
        adddate.f   getbak.f    getprof.f   prepfits.f  setterp.f

OBJS=	CALCAPE.o   getfct.o    ireadpb.o   raddate.o   ufbxy3.o \
        adddate.o   getbak.o    getprof.o   prepfits.o  setterp.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
#FC = ifort
# FC = ftn
LDFLAGS =
CMD =		rcdas_prepfits

#LIBS =          -L/usr/local/lib -lessl -lmass \
#                -L/nwprod/lib -lbacio_4 -lw3_4 -lbufr_4_64
# LIBS = -L/nwprod/lib -lbacio_4 -lw3nco_4 -lw3emc_4 -lbufr_4_64
LIBS = ${BACIO_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}

#FFLAGS = -O -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -qspillsize=548  -g
#FFLAGS = -O -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -qspillsize=548  -g -c
FFLAGS = -O2 -convert big_endian -assume byterecl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	touch junk.o junk.lst ${CMD} ; rm *.o *.lst ${CMD}

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile
