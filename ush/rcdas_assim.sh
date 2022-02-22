#! /bin/sh
#
# this job runs 3dvar for RCDAS and forecast step
#
#

fatal() {
  echo "fatal error $*"
  export err=8
  err_exit ; exit $err
}
# clean_unit .. removes old fortran unit number connections
clean_unit() {
   touch fort.1
   rm fort.*

   export FORT0=abc
   unset `set | grep '^FORT' | sed 's/=.*//'`
}

set -x

# get prepbufr files
cp $COM_M12/prepbufr2.$DATE_M12 prepcq.$DATE_M12 || fatal "copy prepbufr2 file"
cp $COM_M12/prepbufr2.$DATE_M09 prepcq.$DATE_M09 || fatal "copy prepbufr2 file"
cp $COM_M12/prepbufr2.$DATE_M06 prepcq.$DATE_M06 || fatal "copy prepbufr2 file"
cp $COM_M12/prepbufr2.$DATE_M03 prepcq.$DATE_M03 || fatal "copy prepbufr2 file"


# get precip
if [ $DOPCP = 'YES' ]; then
  cp $COM_M12/obsprcp.$DATE_M12 egrd80.tm12.3hr
  cp $COM_M12/obsprcp.$DATE_M09 egrd80.tm09.3hr
  cp $COM_M12/obsprcp.$DATE_M06 egrd80.tm06.3hr
  cp $COM_M12/obsprcp.$DATE_M03 egrd80.tm03.3hr
fi

if [ $DORAD = YES ]; then
  cp $FIXrcdas/most_of_satinfo satinfo  # NEW

  obname="h1bn05 h1bn06 h1bn07 h1bn08 h1bn09 h1bn10 h1bn11 h1bn12 h1bn14 h1bn15 h1bn16 \
            m1bn05 m1bn06 m1bn07 m1bn08 m1bn09 m1bn10 m1bn11 m1bn12 m1bn14 m1bn15 m1bn16"
  for name in $obname
  do
      [ -f $COM_M12/${name}.$DATE_M12 ] && cp $COM_M12/${name}.$DATE_M12 ${name}.tm12
      [ -f $COM_M12/${name}.$DATE_M09 ] && cp $COM_M12/${name}.$DATE_M09 ${name}.tm09
      [ -f $COM_M12/${name}.$DATE_M06 ] && cp $COM_M12/${name}.$DATE_M06 ${name}.tm06
      [ -f $COM_M12/${name}.$DATE_M03 ] && cp $COM_M12/${name}.$DATE_M03 ${name}.tm03
# not needed      [ -f $COM/${name}.$DATE ] && cp $COM/${name}.$DATE ${name}.tm00   # for free forecast
  done

fi

# from previous cycle
#      tm03 3-hr forecast for 1st guess
#      nmcdate
#      satbias_in
#

HR=`echo $DATE_M15| cut -c9-10`
if [ -f $COM_M15/${model}.t${HR}z.restrt03 ] ; then
  cp $COM_M15/${model}.t${HR}z.restrt03 restrt03
elif [ -f $COM_M15/${model}.t${HR}z.restrt03.gz ] ; then
  cp $COM_M15/${model}.t${HR}z.restrt03.gz restrt03.gz
  gunzip restrt03.gz
else
  fatal "missing restrt03 file: $COM_M15/${model}.t${HR}z.restrt03"
fi
cp $COM_M15/${model}.t${HR}z.satbias satbias_in || fatal "missing $COM_M15/${model}.t${HR}z.satbias"
echo DATEXX$DATE_M12 > nmcdate.tm12

if [ $CYC = 12 ]; then
   cp $FIXrcdas/rstupt.parm_newsnow snow.parm
else
   cp $FIXrcdas/rstupt.parm_cycsnow snow.parm
fi
cp $FIXrcdas/nhb3245     .

cp $COM_SNOW/snowdepth.${DATE_SNOW}.grb snowdepth || fatal "missing $COM_SNOW/snowdepth.${DATE_SNOW}.grb"
cp $COM_SNOW/ice.${DATE_SNOW}.grb ice || fatal "missing $COM_SNOW/ice.${DATE_SNOW}.grb"
cp $COM_SNOW/sst.${DATE_SNOW}.grb sst || fatal "missing $COM_SNOW/sst.${DATE_SNOW}.grb"


if [ $DOPCP = 'YES' ] ; then
   cp $FIXrcdas/adjppt.parm_yes adjppt.parm
else
   cp $FIXrcdas/adjppt.parm_no adjppt.parm
fi


cp $FIXrcdas/radco2               radco2
cp $FIXrcdas/solar                solar
cp $FIXrcdas/namelist_filename.txt .
cp $FIXrcdas/soil_veg_namelist_ver_2.1 .
cp $FIXrcdas/z0eff                z0eff
cp $FIXrcdas/staids_prof          staids_prof
cp $FIXrcdas/hgtsmref.r3245.2d_tmasks    .
cp $FIXrcdas/albedo.grb        .
cp $FIXrcdas/veg.grb           .
HR=`echo $DATE_M12 | cut -c9-10`
cp $COM_M12/${model}.t${HR}z.etabcs etabcs || fatal "missing $COM_M12/${model}.t${HR}z.etabcs"

export itag=03
export restrtahr=03

export spec_coefs=$FIXrcdas/spectral_coefficients.most_of_satinfo
export trans_coefs=$FIXrcdas/transmittance_coefficients.most_of_satinfo


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
   cp $FIXrcdas/fcstdata.parm.${tmmark}_3dvar fcstdata.parm

   #  3D-VAR ANALYSIS
   pgm='rcdas_r3dvar'
   . prep_step

   clean_unit
   rm -f events9* fit*
   mv restrt03 restrt03.ges

export FORT8=snow.parm
export FORT9=$FIXrcdas/eta3245_globstats
export FORT11=fcstdata.parm
export FORT12=nhb3245
export FORT13=restrt03.ges
export FORT15=$FIXrcdas/r3dvar.parm32_7x1
export FORT16=sges.3dvar
export FORT19=$FIXrcdas/errtable.r3dv
export FORT22=z0eff
export FORT30=prepcq
export FORT41=hgtsmref.r3245.2d_tmasks
export FORT47=$FIXrcdas/etaerr1780_ieee
export FORT51=restrt03
export FORT52=nfc3245
export FORT53=nbc3245
export FORT79=radiance.out.${tmmark}

# ln -sf snow.parm                       fort.8
# ln -sf $FIXrcdas/eta3245_globstats     fort.9
# ln -sf fcstdata.parm                    fort.11
# ln -sf nhb3245       fort.12
# ln -sf restrt03.ges                   fort.13
# ln -sf $FIXrcdas/r3dvar.parm32_7x1      fort.15
# ln -sf sges.3dvar                     fort.16
# ln -sf $FIXrcdas/errtable.r3dv             fort.19
# ln -sf z0eff                   fort.22
# ln -sf prepcq                         fort.30
# ln -sf hgtsmref.r3245.2d_tmasks fort.41
ln -sf snowdepth                      fort.42
ln -sf ice                      fort.43
ln -sf sst                      fort.44
ln -sf albedo.grb            fort.45
ln -sf veg.grb               fort.46
# ln -sf $FIXrcdas/etaerr1780_ieee           fort.47
# ln -sf restrt03                       fort.51
# ln -sf nfc3245                        fort.52
# ln -sf nbc3245                        fort.53
# ln -sf radiance.out.${tmmark}         fort.79
ln -sf $spec_coefs                    spectral_coefficients
ln -sf $trans_coefs                   transmittance_coefficients

set +e

startmsg
# $EXECrcdas/rcdas_r3dvar > r3dvar.out.${tmmark}
poe $EXECrcdas/rcdas_r3dvar > $pgmout
err_chk

set -e

if [ "$SENDCOM" = 'YES' ] ; then
   cat fit* > $COM/fits.all.$time
   cat diag_rad1* > $COM/radstats.$time
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
$EXECrcdas/rcdas_rrvents > rrvents.out.$time
err_chk

set +e
grep ">>>>>>>>>>" rrvents.out.$time
[ $? -eq 0 ] && fatal "checksum error or uv event error for rrvents.out.$time"
echo "rrvents OK"


### Running of Jack's cmpevn code to produce output for QC monitoring
if [ "$SENDCOM" = 'YES' ] ; then
  cp prepout.$time $COM/
fi

# don't know where code is located
#mkdir -p ${DATA}/cmpevn
#echo prepout.$time | $EXECrcdas/cmpevn > ${DATA}/cmpevn/cmpevn.$time

set -e
compress -c allevents > $COM/allevents.$time.Z &

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
err_chk

# do a three hour forecast from tmmark to tmmark+3

cp $FIXrcdas/bcexdata.parm.${tmmark} bcexdata.parm
cp $USHrcdas/${model}_template.para.${tmmark}_f90 template
cp $FIXrcdas/fcstdata.parm.${tmmark}_etag fcstdata.parm


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
poe $EXECrcdas/rcdas_edasfcst7x1 < ijtest > etafcst.out.${tmmark}
err_chk

if [ $? -ne 0 ] ; then
   echo `date`" !!!!!!!!!!! rcdas_edasfcst7x1 crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! rcdas_edasfcst7x1 crashed !!!!!!!!!!! ${DATE}"
   echo `date`" !!!!!!!!!!! rcdas_edasfcst7x1 crashed !!!!!!!!!!! ${DATE}" >>$pgmout
   echo `date`" !!!!!!!!!!! rcdas_edasfcst7x1 crashed !!!!!!!!!!! ${DATE}" >>${DATA}/CRASHED.fcst mv etafcst.out.${tmmark} $COM_M12/etafcst.out.${tmmark}.crash
   fatal " !!!!!!!!!!! etafcst crashed !!!!!!!!!!! ${DATE}"
   [ $? -ne 0 ] && fatal " !!!!!!!!!!! etafcst6x6.notraps crashed !!!!!!!!!!! ${DATE}"
fi
set -e

cp $FIXrcdas/rstupt.parm_cycsnow snow.parm
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
      cp restrt00.$tmmark $COM_M12/${model}.t${HR}z.restrt00 || fatal "cp restrt00.$tmmark"
      cp restrt03.$tmmark $COM_M12/${model}.t${HR}z.restrt03 || fatal "cp restrt03.$trmark"
      cp prepout.$time $COM_M12/${model}.t${HR}z.prepout || fatal "cp prepout.$time"
      time=`$NDATE +3 $time`
   done
   cp satbias_in $COM_M12/${model}.t${HR}z.satbias
   touch $COM_M12/${model}.t${HR}z.status_edas
fi

# rm fort.* fcstdone0?.tm?? 3dvrdone00.tm??

echo "done"

exit
