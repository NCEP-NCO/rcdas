SHELL=/bin/sh
#
#
SRCS=	ADD_WMO.f PRDGEN.f CTL_RDR.f \
	FNDBIT.f GET_PDS.f MAK_WMO.f PUT_GB.f \
	SET_O11.f SET_O14.f SET_O23.f SET_O56.f SET_TBL.f SET_WMO.f \
        EXTEND.f FILTER_SC.f FILTER_UV.f GRIB_IN.f GRIB_OUT.f INTERP_PPT.f \
        INTERP_SC.f INTERP_UV.f READ_SORT_CTL.f READ_SORT_GRIB_64bits.f TILE_OUT.f 

OBJS=	ADD_WMO.o PRDGEN.o CTL_RDR.o \
	FNDBIT.o GET_PDS.o MAK_WMO.o PUT_GB.o \
	SET_O11.o SET_O14.o SET_O23.o SET_O56.o SET_TBL.o SET_WMO.o \
        EXTEND.o FILTER_SC.o FILTER_UV.o GRIB_IN.o GRIB_OUT.o INTERP_PPT.o \
        INTERP_SC.o INTERP_UV.o READ_SORT_CTL.o READ_SORT_GRIB_64bits.o TILE_OUT.o 

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
# FC =		ncepxlf
#FC =		ifort
# FC =		ftn
#LDFLAGS =	-p
#LDFLAGS =	-qsmp
#LDFLAGS =	
# LDFLAGS =	-q64 
# LIBS = -L/nwprod/lib -ltransutil_4 -lw3nco_4 -lbacio_4 -lip_4
TRANSUTIL_LIB4:=-L../transutil.fd -ltransutil_4
LIBS = ${TRANSUTIL_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${IP_LIB4}

CMD =		rcdas_prdgen
# PROFLIB =	-lprof

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
# FFLAGS =         -O3 -q64  -qnosave  -C 
# FFLAGS =         -O3 -g -traceback -check all -fp-stack-check  -convert big_endian -assume byterecl
FFLAGS =         -O2 -g -traceback -convert big_endian -assume byterecl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build prdgen.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it prdgen.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	touch junk.o ${CMD} ; rm *.o ${CMD}

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
