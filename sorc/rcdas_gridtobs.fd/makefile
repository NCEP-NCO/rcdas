SHELL=/bin/sh
#
SRCS=	gridtobs.f  inarea.f    nevn.f      setarea.f   setmodel.f  stclst.f \
        stnumb.f    strxbl.f    w3fb11.f    gtgdef.f    inlayer.f   raddate.f \
        setfcst.f   setobtyp.f  stldsp.f    stread.f     \
        igotdata.f  isitob.f    readcntl.f  setlevel.f  status.f    stlstr.f \
        strmbl.f    w3fb06.f

OBJS=	gridtobs.o  inarea.o    nevn.o      setarea.o   setmodel.o  stclst.o \
        stnumb.o    strxbl.o    w3fb11.o    gtgdef.o    inlayer.o   raddate.o \
        setfcst.o   setobtyp.o  stldsp.o    stread.o    \
        igotdata.o  isitob.o    readcntl.o  setlevel.o  status.o    stlstr.o \
        strmbl.o    w3fb06.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
#FC = ifort
#FC = ftn
LDFLAGS = 
CMD =		rcdas_gridtobs


# LIBS = -L/usr/local/lib -lessl -lmass /nwprod/w3lib90/w3lib_4  /nwprod/bufrlib90/bufrlib_4
# LIBS = -L/usr/local/lib -lessl -lmass -L/nwprod/lib -lw3_4 -lbufr_4_64
# LIBS = -L/nwprod/lib -lw3nco_4 -lw3emc_4 -lbufr_4_64
LIBS = ${W3NCO_LIB4} ${W3EMC_LIB4} ${BUFR_LIB4}

# FFLAGS = -O -qarch=pwr3 -qrealsize=4 -qmaxmem=-1 -qsource -qxref=full -qattr -qspillsize=548  -g
# FFLAGS = -O2 -qrealsize=4 -g
FFLAGS = -O2 -g -convert big_endian -assume byterecl

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


clean:
	touch junk.o junk.list ${CMD} ; rm *.lst $(CMD) *.o

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
