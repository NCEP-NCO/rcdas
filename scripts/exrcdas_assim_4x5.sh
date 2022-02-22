#!/bin/sh
#
# this job runs 3dvar for RCDAS and forecast step
#
#

fatal() {
  echo "fatal error $*"
  exit 8
}
# clean_units .. removes old fortran unit number connections
clean_unit() {
   touch fort.1
   rm fort.*

   export FORT0=abc
   unset `set | grep '^FORT' | sed 's/=.*//'`
}

# fixed files are rrr
# don't want to copy rrr because copy
# will fail if previous file is there
# also don't know if cleanup will do rm * or rm -f *
# personally I do not like rm -f *

CP() {
  cp $1 $2
  err=$?
  chmod 644 $2
}

set -x
# for ifort buffered io
export FORT_BUFFERED=true

# get prepbufr files
cp $COMIN_M12/prepbufr2.$DATE_M12 prepcq.$DATE_M12 || fatal "copy prepbufr2 file"
cp $COMIN_M12/prepbufr2.$DATE_M09 prepcq.$DATE_M09 || fatal "copy prepbufr2 file"
cp $COMIN_M12/prepbufr2.$DATE_M06 prepcq.$DATE_M06 || fatal "copy prepbufr2 file"
cp $COMIN_M12/prepbufr2.$DATE_M03 prepcq.$DATE_M03 || fatal "copy prepbufr2 file"

# get precip
if [ $DOPCP = 'YES' ]; then
  cp $COMIN_M12/obsprcp.$DATE_M12 egrd80.tm12.3hr
  cp $COMIN_M12/obsprcp.$DATE_M09 egrd80.tm09.3hr
  cp $COMIN_M12/obsprcp.$DATE_M06 egrd80.tm06.3hr
  cp $COMIN_M12/obsprcp.$DATE_M03 egrd80.tm03.3hr
fi

if [ $DORAD = YES ]; then
  CP $FIXrcdas/rcdas_most_of_satinfo satinfo

  obname="h1bn05 h1bn06 h1bn07 h1bn08 h1bn09 h1bn10 h1bn11 h1bn12 h1bn14 h1bn15 h1bn16 \
            m1bn05 m1bn06 m1bn07 m1bn08 m1bn09 m1bn10 m1bn11 m1bn12 m1bn14 m1bn15 m1bn16"
  for name in $obname
  do
      [ -f $COMIN_M12/${name}.$DATE_M12 ] && cp $COMIN_M12/${name}.$DATE_M12 ${name}.tm12
      [ -f $COMIN_M12/${name}.$DATE_M09 ] && cp $COMIN_M12/${name}.$DATE_M09 ${name}.tm09
      [ -f $COMIN_M12/${name}.$DATE_M06 ] && cp $COMIN_M12/${name}.$DATE_M06 ${name}.tm06
      [ -f $COMIN_M12/${name}.$DATE_M03 ] && cp $COMIN_M12/${name}.$DATE_M03 ${name}.tm03
# not needed      [ -f $COMOUT/${name}.$DATE ] && cp $COMOUT/${name}.$DATE ${name}.tm00   # for free forecast
  done

fi

# from previous cycle
#      tm03 3-hr forecast for 1st guess
#      nmcdate
#      satbias_in
#

HR=`echo $DATE_M15| cut -c9-10`
if [ -f $COMIN_M15/${model}.t${HR}z.restrt03 ] ; then
  cp $COMIN_M15/${model}.t${HR}z.restrt03 restrt03
elif [ -f $COMIN_M15/${model}.t${HR}z.restrt03.gz ] ; then
  cp $COMIN_M15/${model}.t${HR}z.restrt03.gz restrt03.gz
  gunzip restrt03.gz
else
  fatal "missing restrt03 file: $COMIN_M15/${model}.t${HR}z.restrt03"
fi
cp $COMIN_M15/${model}.t${HR}z.satbias satbias_in || fatal "missing $COMIN_M15/${model}.t${HR}z.satbias"
echo DATEXX$DATE_M12 > nmcdate.tm12

if [ $CYC = 12 ]; then
   CP $FIXrcdas/rcdas_rstupt.parm_newsnow snow.parm
else
   CP $FIXrcdas/rcdas_rstupt.parm_cycsnow snow.parm
fi
CP $FIXrcdas/rcdas_nhb3245  nhb3245 

cp $COMIN_SNOW/snowdepth.${DATE_SNOW}.grb snowdepth || fatal "missing $COMIN_SNOW/snowdepth.${DATE_SNOW}.grb"
cp $COMIN_SNOW/ice.${DATE_SNOW}.grb ice || fatal "missing $COMIN_SNOW/ice.${DATE_SNOW}.grb"
cp $COMIN_SNOW/sst.${DATE_SNOW}.grb sst || fatal "missing $COMIN_SNOW/sst.${DATE_SNOW}.grb"


if [ $DOPCP = 'YES' ] ; then
   CP $FIXrcdas/rcdas_adjppt.parm_yes adjppt.parm
else
   CP $FIXrcdas/rcdas_adjppt.parm_no adjppt.parm
fi


CP $FIXrcdas/rcdas_radco2               radco2
CP $FIXrcdas/rcdas_solar                solar
CP $FIXrcdas/rcdas_namelist_filename.txt namelist_filename.txt
CP $FIXrcdas/rcdas_soil_veg_namelist_ver_2.1 soil_veg_namelist_ver_2.1
CP $FIXrcdas/rcdas_z0eff                z0eff
CP $FIXrcdas/rcdas_staids_prof          staids_prof
CP $FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks    hgtsmref.r3245.2d_tmasks
CP $FIXrcdas/rcdas_albedo.grb        albedo.grb
CP $FIXrcdas/rcdas_veg.grb           veg.grb
HR=`echo $DATE_M12 | cut -c9-10`
cp $COMIN_M12/${model}.t${HR}z.etabcs etabcs || fatal "missing $COMIN_M12/${model}.t${HR}z.etabcs"

export itag=03
export restrtahr=03

export spec_coefs=$FIXrcdas/rcdas_spectral_coefficients.most_of_satinfo
export trans_coefs=$FIXrcdas/rcdas_transmittance_coefficients.most_of_satinfo


#-------------------------------------------------------------------------------
for mark in 12 09 06 03 ; do
#-------------------------------------------------------------------------------

   export tmmark="tm$mark"
   time=`$NDATE -$mark $DATE`

   [ $DOOBS = 'YES' ] && cp prepcq.$time prepcq

   # rename sat obs
   sat_name="h1bn05:hirs1b5 m1bn05:umsu1b5 h1bn06:hirs1b6 m1bn06:umsu1b6 h1bn07:hirs1b7 \
     m1bn07:umsu1b7 h1bn08:hirs1b8 m1bn08:umsu1b8 h1bn09:hirs1b9 m1bn09:umsu1b9 h1bn10:hirs1b10 \
     m1bn10:umsu1b10 h1bn11:hirs1b11 m1bn11:umsu1b11 h1bn12:hirs1b12 m1bn12:umsu1b12 h1bn14:hirs1b14 \
     m1bn14:umsu1b14 h1bn15:hirs1b15 m1bn15:umsu1b15 h1bn16:hirs1b16 m1bn16:umsu1b16"
   for n in $sat_name ; do
      f1="`echo $n | cut -f1 -d:`"
      f2="`echo $n | cut -f2 -d:`"
      [ -f $f1.${tmmark} ] && cp $f1.${tmmark} $f2
   done

   [ $DOPCP = 'YES' ] && cp egrd80.${tmmark}.3hr pptdata.${tmmark}
   CP $FIXrcdas/rcdas_fcstdata.parm.${tmmark}_3dvar fcstdata.parm

   #  3D-VAR ANALYSIS
   export pgm='rcdas_r3dvar'
   . prep_step

   clean_unit
   rm -f events9* fit*
   mv restrt03 restrt03.ges

export FORT8=snow.parm
export FORT9=$FIXrcdas/rcdas_eta3245_globstats
export FORT11=fcstdata.parm
export FORT12=nhb3245
export FORT13=restrt03.ges
export FORT15=$FIXrcdas/rcdas_r3dvar.parm32_4x5
export FORT16=sges.3dvar
export FORT19=$FIXrcdas/rcdas_errtable.r3dv
export FORT22=z0eff
export FORT30=prepcq
export FORT41=hgtsmref.r3245.2d_tmasks
export FORT47=$FIXrcdas/rcdas_etaerr1780_ieee
export FORT51=restrt03
export FORT52=nfc3245
export FORT53=nbc3245
export FORT79=radiance.out.${tmmark}

# ln -sf snow.parm                       fort.8
# ln -sf $FIXrcdas/rcdas_eta3245_globstats     fort.9
# ln -sf fcstdata.parm                    fort.11
# ln -sf nhb3245       fort.12
# ln -sf restrt03.ges                   fort.13
# ln -sf $FIXrcdas/rcdas_r3dvar.parm32_7x1      fort.15
# ln -sf sges.3dvar                     fort.16
# ln -sf $FIXrcdas/rcdas_errtable.r3dv             fort.19
# ln -sf z0eff                   fort.22
# ln -sf prepcq                         fort.30
# ln -sf hgtsmref.r3245.2d_tmasks fort.41
ln -sf snowdepth                      fort.42
ln -sf ice                      fort.43
ln -sf sst                      fort.44
ln -sf albedo.grb            fort.45
ln -sf veg.grb               fort.46
# ln -sf $FIXrcdas/rcdas_etaerr1780_ieee           fort.47
# ln -sf restrt03                       fort.51
# ln -sf nfc3245                        fort.52
# ln -sf nbc3245                        fort.53
# ln -sf radiance.out.${tmmark}         fort.79
ln -sf $spec_coefs                    spectral_coefficients
ln -sf $trans_coefs                   transmittance_coefficients

set +e

startmsg

export OMP_NUM_THREADS=1

# mpirun.lsf $EXECrcdas/rcdas_r3dvar > r3dvar.out.${tmmark}
# aprun -d 1 -n 21 -N 21 -S 11 $EXECrcdas/rcdas_r3dvar > r3dvar.out.${tmmark}
# mpirun $EXECrcdas/rcdas_r3dvar > r3dvar.out.${tmmark}
mpiexec -n 25 -ppn 128  --cpu-bind core $EXECrcdas/rcdas_r3dvar > r3dvar.out.${tmmark}

export err=$?; err_chk

set -e

if [ "$SENDCOM" = 'YES' ] ; then
   cat fit* > $COMIN_M12/fits.all.$time
   cat diag_rad1* > $COMIN_M12/radstats.$time
#SH   cat fit* > $COMOUT/fits.all.$time
#SH   cat diag_rad1* > $COMOUT/radstats.$time
fi
# rm -f diag_rad1*

rm restrt03.ges
cp satbias_out satbias_in

cat events9*>allevents
sed '1,109d' allevents > allevents.tmp

clean_unit
export FORT20=allevents.tmp
export FORT21=prepcq
export FORT51=prepout.$time
# ln -sf allevents.tmp           fort.20
# ln -sf prepcq                  fort.21
# ln -sf prepout.$time           fort.51
startmsg
set +e
$EXECrcdas/rcdas_rrvents > rrvents.out.$time
# Turn off the follow error checking lines to let the program run through as it is not critical
#  acccording to Wesley - by SPA, 02/27/2017
#export err=$?; sh $DATA/err_chk

set +e
grep ">>>>>>>>>>" rrvents.out.$time
# [ $? -eq 0 ] && fatal "checksum error or uv event error for rrvents.out.$time"
[ $? -eq 0 ] && echo "WARNING: checksum error or uv event error for rrvents.out.$time"
echo "rrvents OK"


### Running of Jack's cmpevn code to produce output for QC monitoring
if [ "$SENDCOM" = 'YES' ] ; then
  cp prepout.$time $COMIN_M12/
#SH  cp prepout.$time $COMOUT/
fi

# don't know where code is located
#mkdir -p ${DATA}/cmpevn
#echo prepout.$time | $EXECrcdas/cmpevn > ${DATA}/cmpevn/cmpevn.$time

set -e
# linux machine doesn't include compress
cp allevents $COMIN_M12/allevents.$time
gzip -f $COMIN_M12/allevents.$time
# compress -c allevents > $COMIN_M12/allevents.$time.Z &
#SH compress -c allevents > $COMOUT/allevents.$time.Z &

# bctend

clean_unit
export FORT17=etabcs
export FORT18=nbc3245
export FORT51=nbcout.${tmmark}

#ln -sf etabcs                  fort.17
#ln -sf nbc3245                 fort.18
#ln -sf nbcout.${tmmark}        fort.51
startmsg
$EXECrcdas/rcdas_bctend > bctend.out.$tmmark || fatal "bctend failed $time"
export err=$?; err_chk

# do a three hour forecast from tmmark to tmmark+3

CP $FIXrcdas/rcdas_bcexdata.parm.${tmmark} bcexdata.parm
cp $USHrcdas/${model}_template.para.${tmmark}_f90 template
CP $FIXrcdas/rcdas_fcstdata.parm.${tmmark}_etag fcstdata.parm

##########################

clean_unit

cat << EOF > ijtest
56 31
EOF

export FORT8=snow.parm
export FORT9=adjppt.parm
export FORT11=fcstdata.parm
export FORT12=nhb3245
export FORT13=restrt03
export FORT14=radco2
export FORT15=staids_prof
export FORT16=nbcout.$tmmark
export FORT17=bcexdata.parm
export FORT19=solar
export FORT21=template
export FORT22=z0eff
export FORT30=hgtsmref.r3245.2d_tmasks
export FORT31=fort.31
touch fort.31
export FORT75=profil
export FORT76=pro_c1
export FORT98=ppt.out.$tmmark

#ln -sf adjppt.parm                   fort.9
#ln -sf snow.parm                     fort.8
#ln -sf fcstdata.parm                 fort.11
#ln -sf nhb3245                       fort.12
#ln -sf restrt03                      fort.13
#ln -sf radco2                         fort.14
#ln -sf staids_prof                   fort.15
#ln -sf nbcout.$tmmark               fort.16
#ln -sf bcexdata.parm               fort.17
#ln -sf solar                       fort.19
#ln -sf template                    fort.21
#ln -sf z0eff                         fort.22
#ln -sf hgtsmref.r3245.2d_tmasks        fort.30

# grib files 41-46
ln -sf pptdata.$tmmark           fort.41
ln -sf snowdepth   fort.42
ln -sf ice                           fort.43
ln -sf sst                            fort.44
ln -sf albedo.grb        fort.45
ln -sf veg.grb           fort.46

# files are not used by following programs
# ln -s -f profil                      fort.75
# ln -s -f pro_c1                      fort.76
# ln -s -f ppt.out.$tmmark             fort.98

set +e
startmsg
#poe $EXECrcdas/rcdas_edasfcst7x1 < ijtest > etafcst.out.${tmmark}

export OMP_NUM_THREADS=1
# mpirun.lsf $EXECrcdas/rcdas_edasfcst4x5 < ijtest
# aprun -d 1 -n 21 -N 21 -S 11 $EXECrcdas/rcdas_edasfcst4x5 < ijtest
# mpirun $EXECrcdas/rcdas_edasfcst4x5 < ijtest
mpiexec -n 25 -ppn 128  --cpu-bind core $EXECrcdas/rcdas_edasfcst4x5 < ijtest

export err=$?; err_chk

if [ $? -ne 0 ] ; then
   echo `date`" !!!!!!!!!!! rcdas_edasfcst crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! rcdas_edasfcst crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! rcdas_edasfcst crashed !!!!!!!!!!! ${DATE}" >>$pgmout
   echo `date`" !!!!!!!!!!! rcdas_edasfcst crashed !!!!!!!!!!! ${DATE}" >>${DATA}/CRASHED.fcst mv etafcst.out.${tmmark} $COMIN_M12/etafcst.out.${tmmark}.crash
   fatal " !!!!!!!!!!! etafcst crashed !!!!!!!!!!! ${DATE}"
fi
set -e

CP $FIXrcdas/rcdas_rstupt.parm_cycsnow snow.parm
cp restrt03.${tmmark} restrt03

#-------------------------------------------------------------------------------
done   ######### for mark in 12 09 06 03; do
#-------------------------------------------------------------------------------


# finished 12 hours worth of analyses and 1st gueses
# copy them to COM

if [ "$SENDCOM" = 'YES' ] ; then
   export time=$DATE_M12
   for tmmark in tm12 tm09 tm06 tm03 ; do
      HR=`echo $time | cut -c9-10`
      cp restrt00.$tmmark $COMIN_M12/${model}.t${HR}z.restrt00 || fatal "cp restrt00.$tmmark"
      cp restrt03.$tmmark $COMIN_M12/${model}.t${HR}z.restrt03 || fatal "cp restrt03.$trmark"
      cp prepout.$time $COMIN_M12/${model}.t${HR}z.prepout || fatal "cp prepout.$time"
      time=`$NDATE +3 $time`
   done
   cp satbias_in $COMIN_M12/${model}.t${HR}z.satbias
   touch $COMIN_M12/${model}.t${HR}z.status_edas
fi

# rm fort.* fcstdone0?.tm?? 3dvrdone00.tm??

echo "done"

exit