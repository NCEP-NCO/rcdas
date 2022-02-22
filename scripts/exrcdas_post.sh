#!/bin/sh
#
# RCDAS Postprocessing
#
# 1. make e-grid grib files
# 2. make AWIPS grib files
# 3. make merged files
# 4. make grib2 merged files
# 5. (not working) make molts (bufr files from analyses)
#
# input: restart files
# output: egrid-grib, AWIPS, merged AWIPS files, merged grib2 AWIPS files
#
# runs in 12 hour chunks
#
# runs on 1 CPU (no mpi)
#
# Usage:
#
#      rcdas_etapost.sh [YYYYMMDDHH]
#
# If no date code is given, the code tries to to the next 12 hour chunk.
# If a date code is given, the HH (hour) must either be 00 or 12.
#

set -aeux
# for ifort buffered io
export FORT_BUFFERED=true

fatal() {
  echo "fatal error $*"
  exit 8
}
# clean_unit .. removes old fortran unit number connections
clean_unit() {
  touch fort.1
  rm fort.*
  export FORT1="abc"
  unset `set | grep '^FORT' | sed 's/=.*//'`
}

cp $FIXrcdas/rcdas_cntrl.parm                 cntrl.parm
chmod 644 cntrl.parm
cp $FIXrcdas/rcdas_nhb3245                    nhb3245
chmod 644 nhb3245

time=$DATE
for tmmark in tm12 tm09 tm06 tm03 ; do
  export tmmark
  hr=`echo $time | cut -c9-10`

  restart00=$COMIN/${model}.t${hr}z.restrt00
  restart03=$COMIN/${model}.t${hr}z.restrt03
  egrd00=$COMOUT/${model}.t${hr}z.egrdsf00
  egrd03=$COMOUT/${model}.t${hr}z.egrdsf03
  awip00=$COMOUT/${model}.t${hr}z.awip3200
  awip03=$COMOUT/${model}.t${hr}z.awip3203
  merged=${model}.t${hr}z.awip32.merged
  merged_grb2=${model}.t${hr}z.awip32.merged.grb2

  # bufr soundings are turned off
  # snding=$COMOUT/${model}.t${hr}z.class1.bufr

  # make EGRID, analysis and 3hr forecast

  if [ -f $restart00.gz ] ; then
    cp $restart00.gz restrt00.$tmmark.gz
    gunzip restrt00.$tmmark.gz
  else
    cp $restart00 restrt00.$tmmark || fatal "no restart file: $restart00"
  fi
  if [ -f $restart03.gz ] ; then
    cp $restart03.gz restrt03.$tmmark.gz
    gunzip restrt03.$tmmark.gz
  else
    cp $restart03 restrt03.$tmmark || fatal "no restart file: $restart03"
  fi

  cp $FIXrcdas/rcdas_fcstdata.parm.${tmmark}_etag fcstdata.parm
  chmod 644 fcstdata.parm
  cp $FIXrcdas/rcdas_bcexdata.parm.${tmmark} bcexdata.parm
  chmod 644 bcexdata.parm
  cp $FIXrcdas/rcdas_inval.${tmmark}.post    post0.parm
  chmod 644  post0.parm

  for fhr in 00 03 ; do
    export pgm='rcdas_etapost'
    . prep_step
    clean_unit

    export FORT11=fcstdata.parm
    export FORT12=nhb3245
    export FORT13=restrt${fhr}.${tmmark}
    export FORT14=cntrl.parm

    startmsg
    ${EXECrcdas}/rcdas_etapost < $FIXrcdas/rcdas_edspost.${fhr}.parm > outmeso.out${fhr}.${tmmark}
    export err=$?;err_chk
    [ $? -ne 0 ] && fatal "rcdas_etapost died"
  done

  if [ "$SENDCOM" = 'YES' ] ; then
    cp EGDAWP00.$tmmark $egrd00
    cp EGDAWP03.$tmmark $egrd03
    [ -s $egrd00 ] || fatal "no egrid file made: $egrd00"
    [ -s $egrd03 ] || fatal "no egrid file made: $egrd03"
  fi

  # make AWIPS grid

  $USHrcdas/rcdas_prdgen.sh EGDAWP00.$tmmark awip00 || fatal "prdgen died $time"
  $USHrcdas/rcdas_prdgen.sh EGDAWP03.$tmmark awip03 || fatal "prdgen died $time"

  if [ "$SENDCOM" = 'YES' ] ; then
    cp awip00 $awip00
    cp awip03 $awip03
    [ -s $awip00 ] || fatal "no awip file made: $awip00"
    [ -s $awip03 ] || fatal "no awip file made: $awip03"
  fi

  # make merged file
  $USHrcdas/rcdas_merge.sh awip00 awip03 $merged

  [ -s $merged ] || fatal "no merged file made: $merged"
  [ -s ${merged}.b ] || fatal "no merged file made: ${merged}.b"

  $CNVGRIB -g12 -p31 -m $merged $merged_grb2
  $CNVGRIB -g12 -p31 -m ${merged}.b ${merged_grb2}.b

  if [ "$SENDCOM" = 'YES' ] ; then
      cp $merged $COMOUT/
      cp ${merged}.b $COMOUT/
      cp $merged_grb2 $COMOUT/
      cp ${merged_grb2}.b $COMOUT/
  fi

  [ -s $merged_grb2 ] || fatal "no merged file made: $merged_grb2"
  [ -s ${merged_grb2}.b ] || fatal "no merged file made: ${merged_grb2}.b"
  time=`$NDATE +3 $time`
done


# add event flags to bufr files

#
# removed 11/18/2009
#
# programs not needed rcdas_prepfits, rcdas_gridtobs, rcdas_editbufr
#

#export pll3="EDS     "
#for dhr in 0 3 6 9
#do
#   time=`$NDATE +$dhr $DATE`
#   hr=`echo $time | cut -c9-10`
#
#   anl=${model}.t${hr}z.awip3200
#   prepout=$COMOUT/${model}.t${hr}z.prepout
#   if [ -f $prepout.gz ] ; then
#      cp $prepout.gz prepout.gz
#      gunzip prepout
#   else
#      cp $prepout prepout
#   fi
#
#   hrges=`$NDATE -3 $time | cut -c9-10`
#   ges=$COMOUT/${model}.t${hrges}z.awip3203
#   [ $hrges -eq 21 ] && ges=$COMOUT_M12/${model}.t21z.awip3203
#   prevent=$COMOUT/${model}.t${hr}z.fits_ges.bufr_d
#   postvent=$COMOUT/${model}.t${hr}z.fits_anl.bufr_d
#   rm -f data00 prepfits.ETA.$hr
#
#   if [ -s $anl -a -s $ges -a -s $prepout ] ; then
#
#      # make index files
#      $UTIL/grbindex $anl anl.idx
#      $UTIL/grbindex $ges ges.idx
#
#      echo  >prepfits.in00 "${pll3} $anl"
#      echo >>prepfits.in00 "${pll3} anl.idx"
#      echo  >prepfits.in03 "${pll3} $ges"
#      echo >>prepfits.in03 "${pll3} ges.idx"
#      pgm='rcdas_editbufr'
#      . prep_step
#      clean_unit
#      export FORT20=prepout
#      export FORT51=data00
#
#      startmsg
#      $EXECrcdas/rcdas_editbufr < $FIXrcdas/rcdas_keeplist.eta
#      export err=$?;err_chk
#
#      pgm='rcdas_prepfits'
#      . prep_step
#      clean_unit
#      export FORT11=$FIXrcdas/rcdas_levcat.eta
#      export FORT20=data00
#      export FORT22=$FIXrcdas/rcdas_prepfits.tab
#      export FORT51=prepfits.ETA00.$hr
#
#      startmsg
#      $EXECrcdas/rcdas_prepfits < prepfits.in00 > prepfits.${hr}_00.out
#      export err=$?;err_chk
#
#      pgm='rcdas_gridtobs'
#      . prep_step
#      clean_unit
#      export FORT11=prepfits.ETA00.$hr
#      export FORT20=$FIXrcdas/rcdas_grid#104
#      export FORT21=$FIXrcdas/rcdas_regions
#      export FORT51=$COMOUT/${model}.t${hr}z.vdb.f00
#
#      startmsg
#      $EXECrcdas/rcdas_gridtobs < $FIXrcdas/rcdas_gridtobs.eds0012a.opn > gridtobs.${hr}_00.out
#      export err=$?;err_chk
#      if [ "$SENDCOM" = 'YES' ] ; then
#         cp prepfits.ETA00.$hr        $postvent
#      fi
#
#      pgm='rcdas_prepfits'
#      . prep_step
#      clean_unit
#      export FORT11=$FIXrcdas/rcdas_levcat.eta
#      export FORT20=data00
#      export FORT22=$FIXrcdas/rcdas_prepfits.tab
#      export FORT51=prepfits.ETA03.$hr
#
#      startmsg
#      $EXECrcdas/rcdas_prepfits < prepfits.in03 > prepfits.${hr}_03.out
#      export err=$?;err_chk
#
#      pgm='rcdas_gridtobs'
#      . prep_step
#      clean_unit
#      export FORT11=prepfits.ETA03.$hr
#      export FORT20=$FIXrcdas/rcdas_grid#104
#      export FORT21=$FIXrcdas/rcdas_regions
#      export FORT51=$COMOUT/${model}.t${hr}z.vdb.f03
#
#      startmsg
#      $EXECrcdas/rcdas_gridtobs < $FIXrcdas/rcdas_gridtobs.eds0012a.opn > gridtobs.${hr}_03.out
#      export err=$?;err_chk
#
#      if [ "$SENDCOM" = 'YES' ] ; then
#         cp prepfits.ETA03.$hr        $prevent
#      fi
#      rm data00
#  fi
#done


HR=`expr $HR + 9`
[ $HR -eq 9 ] && HR='09'
if [ "$SENDCOM" = 'YES' ] ; then
   touch $COMOUT/${model}.t${HR}z.status_post
fi

exit
