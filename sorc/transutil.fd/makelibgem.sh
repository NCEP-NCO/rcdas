#!/bin/sh
###############################################################
#
#   AUTHOR:    Vuong - W/NP11
#
#   DATE:      01/11/1999
#
#   PURPOSE:   This script uses the make utility to update the libgem 
#              archive libraries.
#              It first reads a list of source files in the library and
#              then generates a makefile used to update the archive
#              libraries.  The make command is then executed for each
#              archive library, where the archive library name and 
#              compilation flags are passed to the makefile through 
#              environment variables.
#
#   REMARKS:   Only source files that have been modified since the last
#              library update are recompiled and replaced in the object
#              archive libraries.  The make utility determines this
#              from the file modification times.
#
#              New source files are also compiled and added to the object 
#              archive libraries.
#
###############################################################

#
#     Generate a list of object files that corresponds to the
#     list of Fortran ( .f ) files in the current directory
#
for i in `ls *.f`
do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libgem ] 
then
  rm -f make.libgem
fi
#
#     Generate a new make file ( make.libgem), with the updated object list,
#     from this HERE file.
#
cat > make.libgem << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	${FC} -c \$(FFLAGS) \$<
	ar -ruv \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libgem_4.a
#
export LIB="libgem_4.a"
export FFLAGS=" -O3 -g"
export CFLAGS=" -O3 -DLINUX"
make -f make.libgem
#
#     Update 8-byte version of libgem_8.a
#
export LIB="libgem_8.a"
export FFLAGS=" -O3 -g -r8 -i8"
export CFLAGS=" -O3 -DLINUX"
make -f make.libgem
#
#     Update Double Precision (Size of Real 8-byte and default Integer) version
#     of libgem_d.a
#
export LIB="libgem_d.a"
export FFLAGS=" -O3 -g -r8"
export CFLAGS=" -O3 -DLINUX"
make -f make.libgem

rm -f make.libgem
