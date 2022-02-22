#!/bin/sh
#                                            4/04 Wesley Ebisuzaki
#
# runs rot_vector (convert grid winds to earth-relative winds
#   puts data in order
# v1.1 - eliminate UFLX and VFLX from output as not enough precision was used
# v2.0 - scale WVCONV and WCCONV by "dx"
#

# locations of executables and data files

ROT=$EXECrcdas/rcdas_rot_vector
CONV=$EXECrcdas/rcdas_fix_rr_conv
U1000=$FIXrcdas/rcdas_U1000_rot.grb
DX=$FIXrcdas/rcdas_AWIP32_dx.grib
fix_rr_mask=$EXECrcdas/rcdas_fix_rr_mask

set -x

if [ $# -lt 2 ] ; then
   echo "usage $0 in out"
   exit 8
fi

#
# need to put UFLX/VFLX, WVUFLX/WVVFLX, WCUFLX/WCVFLX
#  next to each other for the rot_vector program to
#  work
#
# rename V_type fluxes
#

$WGRIB $1 >$2.tmp.inv
[ -f $2 ] && rm $2

# copy all unaffected variables to new file

egrep -v ':([UV]FLX|W[VC][UV]FLX):' <$2.tmp.inv | \
  egrep -v ':W[VC]CONV:' | \
  $WGRIB $1 -i -s -grib -o $2 >/dev/null

# fix W[VC]CONV, make file with only convergence

[ -s $2.tmp ] && rm $2.tmp
egrep ':W[VC]CONV:' <$2.tmp.inv | $WGRIB $1 -i -s -grib -o $2.tmp

# run fixit program
if [ -s $2.tmp ] ; then
   $CONV $2.tmp $2.tmp.fixed $DX
   cat $2.tmp.fixed >> $2
   rm $2.tmp.fixed
fi
[ -f $2.tmp ] && rm $2.tmp

egrep ':([UV]FLX|W[VC][UV]FLX):' <$2.tmp.inv | \
   egrep -v ':[UV]FLX:' | \
   sed 's/kpds7=//' | sed -e 's/:VFLX:/:UFLX:/' \
   -e 's/:WVVFLX:/:WVUFLX:/' -e 's/:WCVFLX:/:WCUFLX:/' |
     sort -t: -k3,3 -k4,4 -k6,6 -k7n,7 -k5,5 | \
	$WGRIB -i -s -grib $1 -o $2.tmp 

if [ -s $2.tmp ] ; then
   $ROT $2.tmp $2.tmp.fixed $U1000
   cat $2.tmp.fixed >> $2
   n1=`$WGRIB $2.tmp | wc -l`
   n2=`$WGRIB $2.tmp.fixed | wc -l`
   echo "$ROT diag in=$n1 out=$n2 grib records"
   rm $2.tmp.fixed
fi
[ -f $2.tmp ] && rm $2.tmp

# added 5-14-2004
$fix_rr_mask $2 $2.mask
mv $2.mask $2

rm $2.tmp.inv

exit 0
