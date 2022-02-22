#!/bin/sh
set -x
#
# processing of precip for input to the RCDAS assimilation
#
# input:
#       CMORPH binary 30-minute precip data
#       cpc global precip  binary daily precip data 12Z-12Z
#       cpc conus precip
#       stage iv radar precip data
#
# output:
#       Hourly CMORPH grib files
#       Hourly US_MEX grib precip
#
# failure mode:
#      no stage iv .. conus: hourly precip is 1/24 of daily precip
#      no cmorph -- non-conus: hourly precip is 1/24 of daily precip
#      no cpc conus precip .. conus: use ndas precip
#      no us_mex -- use cmorph data
#
# programs:
#      rcdas_gribify_cmorph converts two 30 minute CMORPH fields into 1 hour ave grib file
#      rcdas_gribify_conus_gauge convert cpc conus precip into grib
#      rcdas_gribify_us_mex convert hourly binary data into grib
#      rcdas_gribify_us_mex2 convert binary global data into grib
#
#
# 2/2007: updated script from Marco Carrera by Wesley Ebisuzaki
#         updated documentation
#         very minor bug fix
# 4/2007: fix copy to IBM section
# 9/2009: compute farm mods
# 1/2012: update for missing data
# 7/2014: update for change in stage iv names, add more missing file warnings
# 1/2020: update for stage 4 in grib2 (old files were stage ii)
#
# variables: date, day and day1

fatal() {
  echo "fatal error $*"
  exit 8
}
# clean_unit .. removes old fortran unit number connections
clean_unit() {
   touch fort.1
   rm fort.*

   export FORT0=abc
   unset `set | grep '^FORT' | sed 's/=.*//'`
}

export FORT_BUFFERED=true

# get files and save into $DATA

# GET cpc daily analyses from compute farm

eval in="$DCOM_IN/$DATE/wgrbbul/cpc_rcdas/$cpc_conus_in"

cp $in $DATA/gauge_precip.${CDATP1}.tmp

# convert to grib
export pgm=rcdas_gribify_conus_gauge
. prep_step
startmsg
$EXECrcdas/rcdas_gribify_conus_gauge $DATA/gauge_precip.${CDATP1}.tmp \
   $DATA/gauge_precip.${CDATP1}.grb ${CDATP1}12

# 1/2012 WNE: missing data results in no grib file, make grib file with negative numbers
if [ ! -f $DATA/gauge_precip.${CDATP1}.grb  -o ! -s $DATA/gauge_precip.${CDATP1}.grb ] ; then
   echo "CPC US gauge data missing, not used, notify CPC"
   echo "${PDY}${cyc}: CPC US gauge data missing, not used, notify CPC" >>$LOG
   $EXECrcdas/rcdas_gribify_conus_gauge $FIXrcdas/rcdas_missing_us_gauge $DATA/gauge_precip.${CDATP1}.grb ${CDATP1}12
fi

# convert to LDAS grid
export grid='255 0 464 224 25063 -124938 128 52938 -67063 125 125 64'
$COPYGB -g"$grid" -x $DATA/gauge_precip.${CDATP1}.grb $DATA/gauge_precip.${CDATP1}.grb.ldas

# convert to binary with f77 header
$WGRIB -ieee -d 1 $DATA/gauge_precip.${CDATP1}.grb.ldas -o $DATA/gauge_precip.${CDATP1}

# get stage ii/iv data

err=0
for hh in 13 14 15 16 17 18 19 20 21 22 23
do
   if [ -f $COMINpcpanl/st4_conus.${CDATE}${hh}.01h.grb2 ] ; then
      $CNVGRIB -g21 $COMINpcpanl/st4_conus.${CDATE}${hh}.01h.grb2 $DATA/st4_conus.${CDATE}${hh}.grb
   else
      err=`expr $err + 1`
   fi
done
echo "err=$err"

for hh in 00 01 02 03 04 05 06 07 08 09 10 11 12
do
   if [ -f $COMINpcpanl_P1/st4_conus.${CDATP1}${hh}.01h.grb2 ] ; then
      $CNVGRIB -g21 $COMINpcpanl_P1/st4_conus.${CDATP1}${hh}.01h.grb2 $DATA/st4_conus.${CDATP1}${hh}.grb
   else
      err=`expr $err + 1`
   fi
done

if [ "$err" -ne 0 ] ; then
   echo "${PDY}${cyc}: Stage IV is missing, not used, notify CPC"
   echo "${PDY}${cyc}: Stage IV is missing, not used, notify CPC" >>$LOG
fi

# old: use NDAS 3 hourly precip
# new: use nam 1 hourly precip

#  OLD get NDAS precip

#err=0
#cp $COMIN_NDAS_P1/ndas.t00z.awip3d03.tm12 $DATA/NDAS.${CDATE}12.old
#err=`expr $err + $?`
#ls -l $COMIN_NDAS_P1/ndas.t00z.awip3d03.tm12 $DATA/NDAS.${CDATE}12.old
#cp $COMIN_NDAS_P1/ndas.t00z.awip3d03.tm09 $DATA/NDAS.${CDATE}15.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t00z.awip3d03.tm06 $DATA/NDAS.${CDATE}18.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t00z.awip3d03.tm03 $DATA/NDAS.${CDATE}21.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t12z.awip3d03.tm12 $DATA/NDAS.${CDATP1}00.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t12z.awip3d03.tm09 $DATA/NDAS.${CDATP1}03.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t12z.awip3d03.tm06 $DATA/NDAS.${CDATP1}06.old
#err=`expr $err + $?`
#cp $COMIN_NDAS_P1/ndas.t12z.awip3d03.tm03 $DATA/NDAS.${CDATP1}09.old
#err=`expr $err + $?`
#echo $err

#  NEW get NAM precip, make into 3 hourly fcsts

err=0
cat $COMINnam_P1/nam.t00z.awip3d01.tm04.grib2 $COMINnam_P1/nam.t00z.awip3d01.tm05.grib2 \
        $COMINnam_P1/nam.t00z.awip3d01.tm06.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATE}18
[ ! -s $DATA/NDAS.${CDATE}18 ] && err=1

cat $COMINnam_P1/nam.t00z.awip3d01.tm01.grib2 $COMINnam_P1/nam.t00z.awip3d01.tm02.grib2 \
        $COMINnam_P1/nam.t00z.awip3d01.tm03.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATE}21
[ ! -s $DATA/NDAS.${CDATE}21 ] && err=1

cat $COMINnam_P1/nam.t06z.awip3d01.tm04.grib2 $COMINnam_P1/nam.t06z.awip3d01.tm05.grib2 \
        $COMINnam_P1/nam.t06z.awip3d01.tm06.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATP1}00
[ ! -s $DATA/NDAS.${CDATP1}00 ] && err=1

cat $COMINnam_P1/nam.t06z.awip3d01.tm01.grib2 $COMINnam_P1/nam.t06z.awip3d01.tm02.grib2 \
        $COMINnam_P1/nam.t06z.awip3d01.tm03.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATP1}03
[ ! -s $DATA/NDAS.${CDATP1}03 ] && err=1

cat $COMINnam_P1/nam.t12z.awip3d01.tm04.grib2 $COMINnam_P1/nam.t12z.awip3d01.tm05.grib2 \
        $COMINnam_P1/nam.t12z.awip3d01.tm06.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATP1}06
[ ! -s $DATA/NDAS.${CDATP1}06 ] && err=1

cat $COMINnam_P1/nam.t12z.awip3d01.tm01.grib2 $COMINnam_P1/nam.t12z.awip3d01.tm02.grib2 \
        $COMINnam_P1/nam.t12z.awip3d01.tm03.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATP1}09
[ ! -s $DATA/NDAS.${CDATP1}09 ] && err=1

cat $COMINnam/nam.t18z.awip3d01.tm04.grib2 $COMINnam/nam.t18z.awip3d01.tm05.grib2 \
        $COMINnam/nam.t18z.awip3d01.tm06.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATE}12
[ ! -s $DATA/NDAS.${CDATE}12 ] && err=1

cat $COMINnam/nam.t18z.awip3d01.tm01.grib2 $COMINnam/nam.t18z.awip3d01.tm02.grib2 \
        $COMINnam/nam.t18z.awip3d01.tm03.grib2 | \
   $WGRIB2 - -match APCP \
      -if ":n=1:" -rpn sto_0 -fi \
      -if ":n=2:" -rpn sto_1 -fi \
      -if ":n=3:" -rpn "rcl_0:rcl_1:+:+" -set_ave "0-3 hour acc fcst" -grib_out junk.grb2
$CNVGRIB -g21 junk.grb2 $DATA/NDAS.${CDATE}15
[ ! -s $DATA/NDAS.${CDATE}15 ] && err=1

for f in $DATA/NDAS.*
do
  echo "APCP in $f === ` $WGRIB -s $f | grep APCP`"
  cp $f $stmp
done
echo "err=$err"

if [ "$err" -ne 0 ] ; then
   echo "${PDY}${cyc}: NAM is missing, not used, notify CPC"
   echo "${PDY}${cyc}: NAM is missing, not used, notify CPC" >>$LOG
fi

# make NDAS files smaller .. already done with with NAM input
#for f in $DATA/NDAS.${CDATE}12 $DATA/NDAS.${CDATE}15 $DATA/NDAS.${CDATE}18 $DATA/NDAS.${CDATE}21 \
#       $DATA/NDAS.${CDATP1}00 $DATA/NDAS.${CDATP1}03 $DATA/NDAS.${CDATP1}06 $DATA/NDAS.${CDATP1}09
#do
#   $WGRIB $f | grep ":APCP:" | $WGRIB $f  -i -grib -s -o $f.tmp
#   mv $f.tmp $f
#done

# get CMORPH data

date0=${CDATE}12
enddate=${CDATP1}11

date=$date0
while [ $date -le $enddate ] ; do

   DATE=`echo ${date} | cut -c1-8`
   eval in="$DCOM_IN/$DATE/wgrbbul/cpc_rcdas/$cmorph_in"

   out=$DATA/CMORPH_025deg_${date}

   if [ ! -s $out ] ; then
      cp $in $out
   fi
   if [ ! -s $out ] ; then
      if [ -f $in.Z ] ; then
         cp $in.Z $out.Z && gunzip $out.Z
      fi
      if [ -f $in.gz ] ; then
         cp $in.gz $out.gz && gunzip $out.gz
      fi
   fi
   if [ ! -s $out ] ; then
      echo "missing file $date $in, $out"
      echo "missing file $date $in, $out" >>$pgmout
      echo "${PDY}${cyc}: $out is missing, not used, notify CPC"
      echo "${PDY}${cyc}: $out is missing, not used, notify CPC" >>$LOG
   fi
   date=`$NDATE +1 $date`
done

# get us-mex precip from CPC global
# convert from global to us-mex like file

eval in="$DCOM_IN/$DATE/wgrbbul/cpc_rcdas/$cpc_gbl_in"

us_mex=$DATA/us_mex$CDATP1
cp $in $us_mex.tmp
[ -f "$us_mex" ] && rm $us_mex

if [ ! -s $us_mex.tmp ] ; then
  echo "MISSING FILE US_MEX PRECIP: $us_mex, notify CPC"
  echo "${PDY}${cyc}: MISSING FILE US_MEX PRECIP: $us_mex, notify CPC" >>$LOG
else

  # convert to grib
  export pgm=rcdas_gribify_us_mex2
  . prep_step
  $EXECrcdas/rcdas_gribify_us_mex2 $us_mex.tmp $us_mex.tmp.grb ${CDATP1}12 12
  if [ $? -ne 0 ] ; then
     echo "MISSING OBS IN US_MEX PRECIP: $us_mex, notify CPC"
     echo "${PDY}${cyc}: MISSING OBS IN US_MEX PRECIP: $us_mex, notify CPC" >>$LOG
  fi

# 1/2012 WNE: check for missing grib file - will occur if binary file is all missing
  if [ ! -f $us_mex.tmp.grb -o ! -s $us_mex.tmp.grb ] ; then
     echo "MISSING OBS IN US_MEX PRECIP: $us_mex, notify CPC"
     echo "${PDY}${cyc}: MISSING OBS IN US_MEX PRECIP: $us_mex, notify CPC" >>$LOG
  else

     # convert to new grid  ..  new_LatLon_GDS(pds,321,201,-140.0,10.0,-60.0,60.0,0.25,0.25);
     export grid='255 0 321 201 10000 -140000 128 60000 -60000 250 250 64'
     $COPYGB -g"$grid" -x $us_mex.tmp.grb $us_mex.grb

     # convert to binary f77 headers
     $WGRIB -ieee $us_mex.grb -d 1 -o $us_mex

     # convert to binary
     echo "${PDY}${cyc}: FOUND US_MEX PRECIP: $us_mex"
  fi
fi

#          PROCESS the data

#  make hourly conus precip

clean_unit

i=0
>STDIN
while [ $i -lt 24 ]
do
   date=`$NDATE +$i $date0`
   if [ $(($i % 3)) -eq 0 ] ; then
#     grib file
      ln -s NDAS.$date fort.$((11 + $i))
   fi
   echo "$date" >>STDIN

   date=`$NDATE +1 $date`
#  grib file
#  stage ii 
#  ln -s ST2gg$date.Grb fort.$((35 + $i))
#  stage iv
   ln -s $DATA/st4_conus.${date}.grb fort.$((35 + $i))
#  check for stage iv data
#   $WGRIB $DATA/st4_conus.${date}.grb
#  grib file
   ln -s $date.lsmforce_noaa fort.$((60+$i))
   i=$(($i + 1))
done
export pgm=rcdas_conus_precip
. prep_step

export FORT8=gauge_precip.${CDATP1}
# ln -s gauge_precip.${CDATP1} fort.8

# grib file
ln -s $FIXrcdas/rcdas_oceanmask.grb fort.9

startmsg
$EXECrcdas/rcdas_conus_precip <STDIN

export err=$?; sh err_chk

# finished making conus precip

export missing_cmorph=0
date=$date0
while [ $date -le $enddate ] ; do

#  convert the 30 fields (out) into hourly grib (grib) and hourly binary (hourly)
   export pgm=rcdas_gribify_cmorph
   . prep_step

   out=$DATA/CMORPH_025deg_${date}
   grib=$DATA/CMORPH_025deg_${date}.hourly.grb
   hourly=$DATA/cmorph$date.bin

   startmsg
   $EXECrcdas/rcdas_gribify_cmorph $grib $hourly $date $out 
   if [ $? -ne 0 ] ; then
       echo "CPC cmorph data missing, not used, notify CPC"
       export missing_cmorph=1
   fi

#  make sure no empty files
   [ ! -s $grib ] && rm $grib

   date=`$NDATE +1 $date`
done

if [ "${missing_cmorph}" -eq '1' ]; then
   echo "${PDY}${cyc}: CPC cmorph data missing, not used, notify CPC"
   echo "${PDY}${cyc}: CPC cmorph data missing, not used, notify CPC" >>$LOG
fi

#
# make "files_to_process" input data file
#

pgm=rcdas_disag_us_mex
. prep_step

date=$date0
f=files_to_process
[ -f $f ] && rm $f

while [ $date -le $enddate ] ; do
   hourly=$DATA/cmorph$date.bin
   echo "$hourly" >> $f
   echo "$date" >> $f
   date=`$NDATE +1 $date`
done
echo "$us_mex" >>$f

# 
# run program to do disaggregation
#

clean_unit

export FORT1=files_to_process

# ln -sf files_to_process fort.1

startmsg
$EXECrcdas/rcdas_disag_us_mex
export err=$?; sh err_chk

# convert binary files into grib

date=$date0
while [ $date -le $enddate ] ; do

   pgm=rcdas_gribify_us_mex
   . prep_step
   data=${date}.us_mex
   grib=$DATA/${date}.us_mex.grb

   startmsg 
   $EXECrcdas/rcdas_gribify_us_mex $data $grib $date >/dev/null
   if [ $? -ne 0 ] ; then
       echo "CPC us-mex data missing, not used, notify CPC"
       echo "${PDY}${cyc}: CPC us-mex data missing, not used, notify CPC" >>$LOG
   fi
   date=`$NDATE +1 $date`
done

echo "cleanup"
[ -f $us_mex ] && rm $us_mex
date=$date0
while [ $date -le $enddate ] ; do
   rm $DATA/cmorph$date.bin 
   rm ${date}.us_mex
   date=`$NDATE +1 $date`
done

# copy to $COMOUT
if [ "$SENDCOM" = 'YES' ] ; then
   date=$date0
   while [ $date -le $enddate ] ; do
      hr=`echo $date | cut -c9-10`
      datep1=`$NDATE +1 $date`
      if [ $hr -ge 12 ] ; then
         if [ -s $DATA/${date}.us_mex.grb ]; then
            cp $DATA/${date}.us_mex.grb $COMOUT/
         fi
         cp $DATA/CMORPH_025deg_${date}.hourly.grb $COMOUT/
         if [ $hr -eq 23 ] ; then
            cp $DATA/${datep1}.lsmforce_noaa $COMOUT_P1/
         else 
            cp $DATA/${datep1}.lsmforce_noaa $COMOUT/
         fi
      else
         if [ -s $DATA/${date}.us_mex.grb ]; then
            cp $DATA/${date}.us_mex.grb $COMOUT_P1/
         fi
         cp $DATA/CMORPH_025deg_${date}.hourly.grb $COMOUT_P1/
         cp $DATA/${datep1}.lsmforce_noaa $COMOUT_P1/
      fi        
      date=`$NDATE +1 $date`
  done
  touch $COMOUT_P1/${model}.t09z.status_precip
fi

echo "finished"
