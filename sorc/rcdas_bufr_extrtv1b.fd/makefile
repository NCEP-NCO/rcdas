SHELL=/bin/sh
#FC = ifort
# FC = ftn
# LIBS = -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbacio_4 -lbufr_4_64 
LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4} ${BUFR_LIB4}

#
# This makefile was produced by /usr/bin/fmgen at 04:27:45 PM on 10/08/96
# If it is invoked by the command line
#	make -f makefile.extrat1b
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named extrtv1b.x.
#
# If it is invoked by the command line
#	make -f makefile.extrat1b extrat1b.x.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named extrat1b.x.prof.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile.extrat1b clean
#
# To remove everything but the source files use the command line
#	make -f makefile.extrtat1b clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile.extrat1b void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=	extrtv1b.f
OBJS=	extrtv1b.o


# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#

LDFLAGS =

CMD =		rcdas_bufr_extrtv1b

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 

FFLAGS = -O2 -auto-scalar -convert big_endian -assume byterecl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build extrat1b.x.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) ${FFLAGS} -o $(@) $(OBJS) $(LIBS)

clean:
	touch junk.o ${CMD} ; -rm *.o ${CMD}

clobber:	clean
	-rm -f $(CMD)

void:	clobber
	-rm -f $(SRCS) makefile
