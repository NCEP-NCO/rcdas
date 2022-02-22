#!/bin/sh
#
# run prdgen - convert egrid to awipgrid
#   run fixup programs
# 
# 1 cpu job
#
# $0 egrid awipgrid
#
set -e
set -x

# clean_unit .. removes old fortran unit number connections
clean_unit() {
   touch fort.1
   rm fort.*

   export FORT0=abc
   unset `set | grep '^FORT' | sed 's/=.*//'`
}

tmmark=
fhr=
COMSP=

clean_unit
[ -f eta.AWIP32 ] && rm eta.AWIP32

# ln -sf $FIXrcdas/rcdas_master_nn.ctl   fort.9
# ln -sf $FIXrcdas/rcdas_wgt_3245_221   fort.21

export FORT9=$FIXrcdas/rcdas_master_nn.ctl
export FORT21=$FIXrcdas/rcdas_wgt_3245_221
echo $1 | $EXECrcdas/rcdas_prdgen >junk.out
# output is in eta.AWIP32

$EXECrcdas/rcdas_fix_rr1 eta.AWIP32 eta.AWIP32.tmp $FIXrcdas/rcdas_awipland.grb
$USHrcdas/rcdas_rot_vector.sh eta.AWIP32.tmp $2
rm eta.AWIP32
exit
