SHELL=/bin/sh
#FC=ifort
# FC=ftn
# LIBS = -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64
LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}
#
SRCS=	editbufr.f  gtgdef.f  raddate.f 

OBJS=	editbufr.o  gtgdef.o  raddate.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#

LDFLAGS =
FFLAGS = -O2 -convert big_endian -assume byterecl
CMD = rcdas_editbufr


# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


clean:
	touch junk.o ${CMD} ; rm *.o $(CMD)
