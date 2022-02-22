#!/bin/sh
#                                            9/2019 Wesley Ebisuzaki
#
# snow depth is obtained from AFWA (old: AF Weather Agency - new: 577th Weather Squadron)
#
#  with conversion to phase-3, obsproc is not converting the AFWA snow depth
#
#
# INPUT:
# variable: DCOM_SNOW_ICE        ($DCOMROOT/prod)  source of AFWA data
#           COMOUT               output of snow depth
#           CDATE                YYYYMMDD
#           DATA                 location for computations
#           EXECrcdas            location of executables
#
# file:     $DCOM_SNOW_ICE/YYYYMMDD/wgrbbul/NPR.SNWN.SP.S1200.MESH16
# file:     $DCOM_SNOW_ICE/YYYYMMDD/wgrbbul/NPR.SNWS.SP.S1200.MESH16
#
# OUTPUT:
#           $COMOUT/snowdepth.${CDATE}00.gbl.grb
#             snow depth should not be future snow depth
#             so search from (CDATE - 1 DAY)
#             This is the file for assimilation from CDATE.
#             In theory, data is dated at 12Z, so could use analyses
#             after 12Z.  Not good practice because same file name
#             for two different quantities.
#
# routines used
#          $COPYGB, $WGRIB, $NDATE, rcdas_fix_afwa_snow
#
# modules used
#          grib_util, prod_util, prod_env, wgrib2
#
# returns 0 if ok
# returns 1 if failure
#
# v1.0 09/18/2019               Initial version Wesley Ebisuzaki
# v1.1 03/25/2021               always make snow cover

# for testing
# export DCOM_SNOW_ICE=/gpfs/dell1/nco/ops/dcom/prod
# export CDATE=20190911
# export COMOUT=$ptmp
# export DATA=$stmp
# export EXECrcdas=../exec

set -x

# if file already exists, return
#if [ -f $COMOUT/snowdepth.$CDATE.gbl.grb ] ; then
#   n=`$WGRIB $COMOUT/snowdepth.$DATE.gbl.grb | grep -c ':SNOD:'`
#   [ "$n" -eq 1 ] && exit 0
#fi

for i in 1 2 3 4 5 6 7
do
   hour=`expr $i \* 24`
   date=`$NDATE -$hour ${CDATE}00 | cut -c1-8`
   nh=$DCOM_SNOW_ICE/$date/wgrbbul/NPR.SNWN.SP.S1200.MESH16
   sh=$DCOM_SNOW_ICE/$date/wgrbbul/NPR.SNWS.SP.S1200.MESH16
   [ ! -f $nh ] && continue
   [ ! -f $sh ] && continue

   # copy snow to $DATA and fix it
   cp $nh $DATA/nh_snow.grb.tmp
   chmod 644 $DATA/nh_snow.grb.tmp
   $EXECrcdas/rcdas_fix_afwa_snow $DATA/nh_snow.grb.tmp
   cp $sh $DATA/sh_snow.grb.tmp
   chmod 644 $DATA/sh_snow.grb.tmp
   $EXECrcdas/rcdas_fix_afwa_snow $DATA/sh_snow.grb.tmp

   # remove all but snow depth
   $WGRIB $DATA/nh_snow.grb.tmp | grep ':SNOD:' | $WGRIB -i $DATA/nh_snow.grb.tmp -grib -o $DATA/nh_snow.grb
   $WGRIB $DATA/sh_snow.grb.tmp | grep ':SNOD:' | $WGRIB -i $DATA/sh_snow.grb.tmp -grib -o $DATA/sh_snow.grb
   rm $DATA/nh_snow.grb.tmp $DATA/sh_snow.grb.tmp

   # merge the files together
   $COPYGB -g 129 -M $DATA/sh_snow.grb -x $DATA/nh_snow.grb $DATA/snowdepth.${CDATE}00.gbl.grb

   # check file for results
   [ ! -f $DATA/snowdepth.${CDATE}00.gbl.grb ] && continue
   n=`$WGRIB $DATA/snowdepth.${CDATE}00.gbl.grb | grep -c ':SNOD:'`
   [ "$n" -ne 1 ] && continue

#  save data in $COMOUT
   cp $DATA/snowdepth.${CDATE}00.gbl.grb $COMOUT/snowdepth.${CDATE}00.gbl.grb
   exit 0

done

# failed
exit 1
