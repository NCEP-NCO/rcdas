#!/bin/sh
set -x
#
# this script gets all the data for running RCDAS
#
# input data ranges from precip, snow, sst,
# to model boundary conditions
# to bufr data for the assimilation
#
# 
#----------------------------------------------------------------------------------------------------

fatal() {
  msg="fatal error $*"
  postmsg "$msg"
  exit 8
}
warn() {
  msg="warning problem $*"
  postmsg "$msg"
}
warn1() {
  msg="warning problem $*"
  postmsg "$msg"
  err=1
}

# clean_units .. removes old fortran unit number connections
clean_unit() {
   touch fort.1
   rm fort.*

   export FORT0=abc
   unset `set | grep '^FORT' | sed 's/=.*//'`
}


#
# copy is good for getting data
# if data is already obtained .. doesn't get it again
# fatal error if no data
#
# copy: 1 file
# mcopy: many files, gz uncompression
# hcopy: copy hourly files $h = 00..23
#
copy() {
  if [ -f "$2" ] ; then
    echo "using existing $2"
  else
    f=$2
    [ -d $2 ] && f="$2/`basename $1`"
    if [ ! -s $f ] ; then
      cp $1 $2 || warn1 "$DATE: missing $3 in cp $1 $2"
    fi
    :
  fi
  echo "finished copy"
}

#
# must_copy() are for files that must copy
# if they dont .. die a horrible death
#

must_copy() {
  if [ -f "$2" -a ! -f "$1" ] ; then
    echo "using existing $2"
  else
    if [ ! -s "$1" ] ; then
      echo "missing $1"
      msg="missing $1 copy to $2 pwd=`pwd`"
      postmsg "$msg"
      export err=7; err_exit ; exit $err
    fi
    cp $1 $2
    if [ $? -ne 0 ] ; then
      echo "failed cp $1 $2"
      msg="failed $1 copy to $2 $2 exists? pwd=`pwd`"
      postmsg "$msg"
      export err=7; err_exit ; exit $err
    fi
  fi
}

mcopy() {
  eval tdir=\$$#
  [ ! -d $tdir ] && fatal "mcopy error: mcopy (list) directory"
  while [ $# -gt 1 ] ; do
     f=`basename $1`
     if [ ! -s $tdir/$f ] ; then
        if [ -s $1 ] ; then
          cp $1 $tdir || warn1 "mcopy error 1"
        elif [ -s $1.gz ] ; then
          gunzip -c $1.gz >$tdir/$f || warn1 "mcopy error 2"
        else
          warn1 "missing file $1"
        fi
     fi
     shift 1
  done
}
hcopy() {
  [ ! -d $2 ] && fatal "hcopy 2nd arg must be directory"
  for h in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23
  do
     eval f=$1
     if [ ! -s $2/`basename $f` ] ; then
        cp $f $2 || warn1 "hcopy $f $2 hour=$h"
     fi
  done
}


# export mode="warn";
export mode="fatal";
# for ifort buffered io
export FORT_BUFFERED=true

#----------------------------------------------------------------------------------------------------

#  Get Ingest data: check transfer, no wild cards should be used

export err=0
export DATE=${CDATE}00


# get snowdepth

# with phase-3, need to make snow depth from AFWA grib files
# search from CDATE to CDATE-7 for snow

$USHrcdas/rcdas_get_afwa_snow.sh
if [ $? -ne 0 ] ; then
# no snow found for last 7 days
    echo "No snow for last week found by $USHrcdas/rcdas_get_afwa_snow.sh"
    export err=7; err_exit ; exit $err
fi
# snow is saved in $COMOUT/snowdepth.$DATE.gbl.grb "snow"

copy $DCOM_SNOW_ICE/$CDATM1/wgrbbul/imssnow.grb $COMOUT/ice.$DATE.gbl.grb "ice"

[ $err -eq 1 ] && $mode "copy of obs failed"
export err=0

# copy $DIRsst/sst2dvar.t12z.nam_grid $COMOUT/glsst.$DATE  "gl sst"

# need to make a look clone of sst2dvar.t12z.nam_grid
copy $COMINsst/rtgssthr_grb_0.083.grib2 $DATA/rtgsst "gl sst"

# convert to 1041 x 441 grid
$WGRIB2 $DATA/rtgsst -new_grid_winds earth \
  -new_grid latlon 195:1041:0.125 10:441:0.125 $DATA/rtgsst2
if [ $? -ne 0 ] ; then
    export err=7; err_exit ; exit $err
fi

# put fortran header/trailers around each row
[ -f rtgsst3 ] && rm rtgsst3
i=1
while [ $i -le 1041 ]
do
   $WGRIB2 rtgsst2 -rpn "273.16:-" -append -header -ijbox $i:$i 1:441 rtgsst3 bin
   if [ $? -ne 0 ] ; then
      export err=7; err_exit ; exit $err
   fi
   i=`expr $i + 1`
done
# swap bytes (integer*4/float*4)

$EXECrcdas/rcdas_swap rtgsst3 $COMOUT/glsst.$DATE


#A copy $COMINcdas/sstgrb${CDATE}12 $COMOUT/sst.$DATE.gbl.grb "sst"
must_copy $COMINcdas/cdas.t12z.sstgrb $COMOUT/sst.$DATE.gbl.grb "sst"

#A [ $HAS_HRS2 -eq 1 ] && mcopy $COMINcdas/1bhrs2.${CDATE}00.bufr_d $COMINcdas/1bhrs2.${CDATE}06.bufr_d 
#A     $COMINcdas/1bhrs2.${CDATE}12.bufr_d $COMINcdas/1bhrs2.${CDATE}18.bufr_d $COMINcdas/1bhrs2.${CDATP1}00.bufr_d $COMOUT

if [ $HAS_HRS2 -eq 1 ] ; then
  copy $COMINcdas/cdas.t00z.1bhrs2.tm00.bufr_d $COMOUT/1bhrs2.${CDATE}00.bufr_d
  copy $COMINcdas/cdas.t06z.1bhrs2.tm00.bufr_d $COMOUT/1bhrs2.${CDATE}06.bufr_d
  copy $COMINcdas/cdas.t12z.1bhrs2.tm00.bufr_d $COMOUT/1bhrs2.${CDATE}12.bufr_d
  copy $COMINcdas/cdas.t18z.1bhrs2.tm00.bufr_d $COMOUT/1bhrs2.${CDATE}18.bufr_d
  copy $COMINcdasp1/cdas.t00z.1bhrs2.tm00.bufr_d $COMOUT_P1/1bhrs2.${CDATP1}00.bufr_d
#SH  copy $COMINcdasp1/cdas.t00z.1bhrs2.tm00.bufr_d $COMOUT/1bhrs2.${CDATP1}00.bufr_d
fi

#A [ $HAS_HRS3 -eq 1 ] && mcopy $COMINcdas/1bhrs3.${CDATE}00.bufr_d $COMINcdas/1bhrs3.${CDATE}06.bufr_d  \
#A   $COMINcdas/1bhrs3.${CDATE}12.bufr_d $COMINcdas/1bhrs3.${CDATE}18.bufr_d $COMINcdas/1bhrs3.${CDATP1}00.bufr_d $COMOUT

if [ $HAS_HRS3 -eq 1 ] ; then
  copy $COMINcdas/cdas.t00z.1bhrs3.tm00.bufr_d $COMOUT/1bhrs3.${CDATE}00.bufr_d
  copy $COMINcdas/cdas.t06z.1bhrs3.tm00.bufr_d $COMOUT/1bhrs3.${CDATE}06.bufr_d
  copy $COMINcdas/cdas.t12z.1bhrs3.tm00.bufr_d $COMOUT/1bhrs3.${CDATE}12.bufr_d
  copy $COMINcdas/cdas.t18z.1bhrs3.tm00.bufr_d $COMOUT/1bhrs3.${CDATE}18.bufr_d
  copy $COMINcdasp1/cdas.t00z.1bhrs3.tm00.bufr_d $COMOUT_P1/1bhrs3.${CDATP1}00.bufr_d
#SH  copy $COMINcdasp1/cdas.t00z.1bhrs3.tm00.bufr_d $COMOUT/1bhrs3.${CDATP1}00.bufr_d
fi

#A [ $HAS_MSU -eq 1 ] && mcopy $COMINcdas/1bmsu.${CDATE}00.bufr_d $COMINcdas/1bmsu.${CDATE}06.bufr_d \
#A $COMINcdas/1bmsu.${CDATE}12.bufr_d $COMINcdas/1bmsu.${CDATE}18.bufr_d $COMINcdas/1bmsu.${CDATP1}00.bufr_d $COMOUT

if [ $HAS_MSU -eq 1 ] ; then
  copy $COMINcdas/cdas.t00z.1bmsu.tm00.bufr_d $COMOUT/1bmsu.${CDATE}00.bufr_d
  copy $COMINcdas/cdas.t06z.1bmsu.tm00.bufr_d $COMOUT/1bmsu.${CDATE}06.bufr_d
  copy $COMINcdas/cdas.t12z.1bmsu.tm00.bufr_d $COMOUT/1bmsu.${CDATE}12.bufr_d
  copy $COMINcdas/cdas.t18z.1bmsu.tm00.bufr_d $COMOUT/1bmsu.${CDATE}18.bufr_d
  copy $COMINcdasp1/cdas.t00z.1bmsu.tm00.bufr_d $COMOU_P1/1bmsu.${CDATP1}00.bufr_d
#SH  copy $COMINcdasp1/cdas.t00z.1bmsu.tm00.bufr_d $COMOUt/1bmsu.${CDATP1}00.bufr_d
fi

#A mcopy $COMINcdas/sfcanl${CDATE}00 $COMINcdas/sfcanl${CDATE}06 $COMINcdas/sfcanl${CDATE}12 \
#A      $COMINcdas/sfcanl${CDATE}18 $COMOUT

[ $err -eq 1 ] && $mode "copy of obs failed"

mode="fatal"
must_copy $COMINcdas/cdas.t00z.sfcanl $COMOUT/sfcanl${CDATE}00
must_copy $COMINcdas/cdas.t06z.sfcanl $COMOUT/sfcanl${CDATE}06
must_copy $COMINcdas/cdas.t12z.sfcanl $COMOUT/sfcanl${CDATE}12
must_copy $COMINcdas/cdas.t18z.sfcanl $COMOUT/sfcanl${CDATE}18

#A mcopy $COMINcdas/grb2d${CDATE}00 $COMINcdas/grb2d${CDATE}06 $COMINcdas/grb2d${CDATE}12 \
#A       $COMINcdas/grb2d${CDATE}18 $COM

mode="fatal"
must_copy $COMINcdas/cdas.t00z.sfluxgrbf06 $COMOUT/grb2d${CDATE}00
must_copy $COMINcdas/cdas.t06z.sfluxgrbf06 $COMOUT/grb2d${CDATE}06
must_copy $COMINcdas/cdas.t12z.sfluxgrbf06 $COMOUT/grb2d${CDATE}12
must_copy $COMINcdas/cdas.t18z.sfluxgrbf06 $COMOUT/grb2d${CDATE}18

echo ">>copy of sanl"
mode="fatal"
#A mcopy $COMINcdas/sanl${CDATE}00 $COMINcdas/sanl${CDATE}06 $COMINcdas/sanl${CDATE}12 \
#A      $COMINcdas/sanl${CDATE}18 $COMINcdas/sanl${CDATP1}00 $COMOUT

must_copy $COMINcdas/cdas.t00z.sanl $COMOUT/sanl${CDATE}00
must_copy $COMINcdas/cdas.t06z.sanl $COMOUT/sanl${CDATE}06
must_copy $COMINcdas/cdas.t12z.sanl $COMOUT/sanl${CDATE}12
must_copy $COMINcdas/cdas.t18z.sanl $COMOUT/sanl${CDATE}18
must_copy $COMINcdasp1/cdas.t00z.sanl $COMOUT_P1/sanl${CDATP1}00

echo ">>copy prepbufr"
mode="fatal"


must_copy $COMINobsproc/cdas.t00z.prepbufr prepbufr${CDATE}00
must_copy $COMINobsproc/cdas.t06z.prepbufr prepbufr${CDATE}06
must_copy $COMINobsproc/cdas.t12z.prepbufr prepbufr${CDATE}12
must_copy $COMINobsproc/cdas.t18z.prepbufr prepbufr${CDATE}18

# ------- make prepqm

for hr in 00 06 12 18
do
   clean_unit
#  convert sigma to double precision
#   /nwprod/exec/cdas2_v1_sig2dbl $COMOUT/sanl${CDATE}$hr sanl${hr}.dbl
   $EXECrcdas/cdas2_v1_sig2dbl $COMOUT/sanl${CDATE}$hr sanl${hr}.dbl

   clean_unit
   [ -f prepqm${CDATE}${hr}.post ] && rm prepqm${CDATE}${hr}.post
   export FORT20=prepbufr${CDATE}$hr
   export FORT21=sanl${hr}.dbl
   export FORT50=prepqm${CDATE}${hr}.post
   export FORT51=GFIT.anl.jwpk
   export FORT52=AFIT.anl.jwpk
   export FORT53=dummy
   export FORT54=dummy2
#   /nwprod/exec/cdas2_v1_postvents >postevents.out.${hr}
   $EXECrcdas/cdas2_v1_postvents >postevents.out.${hr}
   cp prepqm${CDATE}${hr}.post $COMOUT/${model}.t${hr}z.prepqm
   cp prepqm${CDATE}${hr}.post $COMOUT/prepqm${CDATE}${hr}.post
   rm sanl${hr}.dbl
done

echo ">>finished bufr copy"


[ $err -eq 1 ] && $mode "copy of obs failed"

touch $COMOUT/done.obs.$CDATE


# check date codes
echo ">>check data codes"

export snowdate=`$WGRIB -s $COMOUT/snowdepth.$DATE.gbl.grb -4yr -d 1 -o /dev/null | cut -f3 -d: | cut -c3-`
[ "$snowdate" -lt "`$NDATE -48 $DATE`" ] && warn "snowdepth file is more than 48 hours old"
[ "$snowdate" -lt "`$NDATE -96 $DATE`" ] && $mode "snowdepth file is more than 96 hours old"
[ "$snowdate" -gt "$DATE" ] && $mode "snowdepth file newer than analysis time"

export icedate=`$WGRIB -s $COMOUT/ice.$DATE.gbl.grb -4yr -d 1 -o /dev/null | cut -f3 -d: | cut -c3-`
[ "$icedate" -lt "`$NDATE -48 $DATE`" ] && $mode "ice file is more than 48 hours old"
if [ "$icedate" -gt "$DATE" ] ; then
   echo "ice file newer than analysis time"
   # if ice is within 24 hours of analysis time .. ok
   [ `$NDATE -24 $icedate` -gt "$DATE" ] && $mode "ice file is too new"
fi

export sstdate=`$WGRIB -s $COMOUT/sst.$DATE.gbl.grb -4yr -d 1 -o /dev/null | cut -f3 -d: | cut -c3-`
[ "$sstdate" -lt "`$NDATE -48 $DATE`" ] && $mode "sst file is more than 48 hours old"
[ "$sstdate" -gt "$DATE" ] && $mode "sst file newer than analysis time"


### convert snow to model grid

if [ ! -s $COMOUT/snowdepth.$DATE.grb ] ; then

# convert 1/16 degree mesh to 1/8 degree mesh
#  a)  convert undefined to zero (as in original 1/8 degree mesh)
$COPYGB -x -M#0 "$COMOUT/snowdepth.$DATE.gbl.grb" snowdepth.${DATE}.tmp1.grb
#  b)  convert snowdepth to 1/8 degree nps grid
export gds="255 5 512 512 -20720 -125112 8 280000 47625 47625 0 64"
$COPYGB -x -g "$gds"  snowdepth.${DATE}.tmp1.grb snowdepth.${DATE}.gbl2.grb
rm  snowdepth.${DATE}.tmp1.grb

export pgm='rcdas_snowget'

. prep_step
clean_unit
arg="`echo $snowdate | sed 's/..\(..\)\(..\)\(..\).*/\1 \2 \3/'`"
export FORT44="$FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks"
export FORT46="$FIXrcdas/rcdas_nhb3245"
# ln -sf "$COMOUT/snowdepth.$DATE.gbl.grb" fort.41
ln -sf snowdepth.$DATE.gbl2.grb fort.41
export FORT42="$FIXrcdas/rcdas_rfusaflw"
ln -sf "$FIXrcdas/rcdas_EGRD3D00.tm12" fort.43
export FORT44="$FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks"
export FORT46="$FIXrcdas/rcdas_nhb3245"
ln -sf "snowdepth.${DATE}.grb" fort.51

startmsg
echo "$arg" | $EXECrcdas/rcdas_snowget > snowprint
export err=$?;err_chk
[ $? -ne 0 ] && fatal "no snow"

#SH [ ! -s $COMOUT/snowdepth.$DATE.grb ] && fatal "no snow"
[ ! -s snowdepth.$DATE.grb ] && fatal "no snow"
if [ $snowdate -ne $DATE ] ; then
   export pgm='rcdas_new_date'
   . prep_step
   startmsg
   $EXECrcdas/rcdas_new_date snowdepth.$DATE.grb $DATE
   export err=$?;err_chk
fi

if [ "$SENDCOM" = 'YES' ] ; then
  cp snowdepth.${DATE}.grb $COMOUT/snowdepth.${DATE}.grb
fi

fi

### Ice processing

if [ ! -s $COMOUT/ice.$DATE.grb ] ; then

[ ! -s $COMOUT/ice.$DATE.gbl.grb ] && fatal "no ice file: $COMOUT/ice.$DATE.gbl.grb "
$COPYGB -g192 -i0 -x $COMOUT/ice.$DATE.gbl.grb ice2.$DATE

arg="`echo $icedate | sed 's/..\(..\)\(..\)\(..\).*/\1 \2 \3/'`"

export pgm='rcdas_iceblend'
. prep_step
clean_unit
export FORT11="$FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks"
export FORT12="$FIXrcdas/rcdas_ice_canada.tbl"
ln -sf $FIXrcdas/rcdas_ice.clim.grb fort.20
ln -sf ice2.$DATE fort.30
ln -sf ice.${DATE}.grb fort.51

startmsg
echo "$arg" | $EXECrcdas/rcdas_iceblend > iceprint
export err=$?;err_chk
[ $? -ne 0 ] && fatal "no ice"
[ ! -s ice.$DATE.grb ] && fatal "no ice"
if [ $icedate -ne $DATE ] ; then
   startmsg
   $EXECrcdas/rcdas_new_date ice.$DATE.grb $DATE
   export err=$?;err_chk
fi

if [ "$SENDCOM" = 'YES' ] ; then
  cp ice.$DATE.grb $COMOUT/ice.$DATE.grb
fi


fi


### SST processing

if [ ! -s $COMOUT/sst.$DATE.grb ] ; then

cp $COMOUT/sst.$DATE.gbl.grb $DATA/
if [ $sstdate -ne $DATE ] ; then
export pgm='rcdas_new_date'
. prep_step
startmsg
   $EXECrcdas/rcdas_new_date $DATA/sst.$DATE.gbl.grb $DATE
   export err=$?;err_chk
fi

export pgm='rcdas_getsst'
. prep_step
clean_unit
export FORT11="$FIXrcdas/rcdas_ice_canada.tbl"
export FORT14="$FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks"
ln -s -f $DATA/sst.$DATE.gbl.grb                          fort.39
ln -s -f $COMOUT/ice.${DATE}.grb                                fort.40
export FORT41="$COMOUT/glsst.$DATE"
ln -s -f sst.${DATE}.grb                                fort.51

echo "$YEAR2 $MONTH $DAY" | $EXECrcdas/rcdas_getsst > sstprint
export err=$?;err_chk
[ $? -ne 0 ] && fatal "no sst"
[ ! -s sst.$DATE.grb ] && fatal "no sst"
if [ "$SENDCOM" = 'YES' ] ; then
  cp sst.$DATE.grb $COMOUT
fi

fi

# TOVS H1B processing .. convert 4x daily to ieee

if [ ! -f $COMOUT/done.tovs_h1b.$DATE ] ; then
for hr in 0 6 12 18 24
do
  export tdate=`$NDATE +$hr $DATE`
  TDATE=`echo $tdate | cut -c1-8`
  if [ $TDATE -eq $CDATP1 ] ; then 
     CURR_COMOUT=$COMOUT_P1 
  fi
  if [ $TDATE -eq $CDATE  ] ; then
     CURR_COMOUT=$COMOUT 
  fi

  [ $HAS_HRS2 -eq 1 ] && $USHrcdas/rcdas_ieeetovs.sh 1bhrs2 14 $tdate >>ieeeprt.hrs2.$tdate
  [ $HAS_HRS3 -eq 1 ] && $USHrcdas/rcdas_ieeetovs.sh 1bhrs3 15 $tdate >>ieeeprt.hrs3.15.$tdate
  [ $HAS_HRS3 -eq 1 ] && $USHrcdas/rcdas_ieeetovs.sh 1bhrs3 16 $tdate >>ieeeprt.hrs3.16.$tdate
  [ $HAS_MSU -eq 1 ]  && $USHrcdas/rcdas_ieeetovs.sh 1bmsu  14 $tdate >>ieeeprt.msu.$tdate

  if [ "$SENDCOM" = 'YES' ] ; then
    [ $HAS_HRS2 -eq 1 ] &&  cp ieeeprt.hrs2.$tdate    $CURR_COMOUT
    [ $HAS_HRS3 -eq 1 ] &&  cp ieeeprt.hrs3.15.$tdate $CURR_COMOUT
    [ $HAS_HRS3 -eq 1 ] &&  cp ieeeprt.hrs3.16.$tdate $CURR_COMOUT
    [ $HAS_MSU -eq 1 ]  &&  cp ieeeprt.msu.$tdate     $CURR_COMOUT
  fi
done

if [ "$SENDCOM" = 'YES' ] ; then
    touch $COMOUT/done.tovs_h1b.$DATE
fi

fi

# now convert TOVS data to 3 hour windows
if [ ! -f $COMOUT/done.tovs_window.$DATE ] ; then

clean_unit

sat="n14 n15 n16"
for hr in 0 6 12 18
do
   export tdate=`$NDATE +$hr $DATE`
   export tdate3=`$NDATE +3 $tdate`
   export tdate6=`$NDATE +6 $tdate`
   for s in $sat
   do
      export pgm='rcdas_window1b'
      . prep_step
      clean_unit
      export FORT51=$COMOUT/h1b$s.$tdate
      # ln -sf $COMOUT/h1b$s.$tdate fort.51
      startmsg
      echo "h1b$s.$tdate.ieee_d h1b$s.$tdate6.ieee_d $tdate 3" | \
      $EXECrcdas/rcdas_window1b_h1b >> $COMOUT/h1bprint
      export err=$?;err_chk

      export pgm='rcdas_window1b'
      . prep_step
      export FORT51=$COMOUT/h1b$s.$tdate3
      # ln -sf $COMOUT/h1b$s.$tdate3  fort.51
      startmsg
      echo "h1b$s.$tdate.ieee_d h1b$s.$tdate6.ieee_d $tdate3 3" | \
      $EXECrcdas/rcdas_window1b_h1b >> $COMOUT/h1bprint
      export err=$?;err_chk
   done
done

### TOVS M1B processing .. into 3 hour chunks
sat=n14
for hr in 0 6 12 18
do
   export tdate=`$NDATE +$hr $DATE`
   export tdate3=`$NDATE +3 $tdate`
   export tdate6=`$NDATE +6 $tdate`
   for s in $sat
   do
      export pgm='rcdas_window1b'
      . prep_step
      clean_unit
      export FORT51=$COMOUT/m1b$s.$tdate
#      ln -s -f $COMOUT/m1b$s.$tdate fort.51
      startmsg
      echo "m1b$s.$tdate.ieee_d m1b$s.$tdate6.ieee_d $tdate 3" | \
      $EXECrcdas/rcdas_window1b_m1b >> m1bprint
      export err=$?;err_chk

      export pgm='rcdas_window1b'
      . prep_step
      export FORT51=$COMOUT/m1b$s.$tdate3
#      ln -s -f $COMOUT/m1b$s.$tdate3 fort.51
      startmsg
      echo "m1b$s.$tdate.ieee_d m1b$s.$tdate6.ieee_d $tdate3 3" | \
      $EXECrcdas/rcdas_window1b_m1b >> m1bprint
      export err=$?;err_chk
   done
done
   if [ "$SENDCOM" = 'YES' ] ; then
      touch $COMOUT/done.tovs_window.$DATE
   fi
fi

### Boundary condition processing

if [ ! -f $COMOUT/done.bc.$DATE ] ; then

export pgm='rcdas_mkbnd'
. prep_step
clean_unit
export FORT11=$COMOUT/sanl${CDATE}00
export FORT12=$COMOUT/sanl${CDATE}06
export FORT13=$COMOUT/sanl${CDATE}12
export FORT41=$FIXrcdas/rcdas_modtop.parm
export FORT42=$FIXrcdas/rcdas_deta_ldt1.45.25mb
export FORT90=rvetalbc
export FORT52=${model}.t00z.etabcs

   startmsg
   $EXECrcdas/rcdas_mkbnd < $FIXrcdas/rcdas_mkbnd.parm_3 > mkbnd1print
   export err=$?;err_chk
   [ $? -ne 0 ] && fatal "missing ${CDATE}00  boundary conditions"

   if [ "$SENDCOM" = 'YES' ] ; then
      cp ${model}.t00z.etabcs $COMOUT
   fi


export pgm='rcdas_mkbnd'
. prep_step

clean_unit
export FORT11=$COMOUT/sanl${CDATE}12
export FORT12=$COMOUT/sanl${CDATE}18
export FORT13=$COMOUT_P1/sanl${CDATP1}00
export FORT41=$FIXrcdas/rcdas_modtop.parm
export FORT42=$FIXrcdas/rcdas_deta_ldt1.45.25mb
export FORT90=rvetalbc
export FORT52=${model}.t12z.etabcs

startmsg
$EXECrcdas/rcdas_mkbnd < $FIXrcdas/rcdas_mkbnd.parm_3 > mkbnd2print
export err=$?;err_chk
[ $? -ne 0 ] && fatal "missing ${CDATP1}00 boundary conditions"

   if [ "$SENDCOM" = 'YES' ] ; then
      cp ${model}.t12z.etabcs $COMOUT/
      touch $COMOUT/done.bc.$DATE
   fi
fi

### Prepbufr processing .. 4x daily prepbufr to 8x daily

# copy prepqm files to $DATA

if [ ! -f $COMOUT/done.prebufr2.$DATE ] ; then
  cp $COMOUT/prepqm*.post .
# convert files into 3 hour chunks

for hh in 00 03 06 09 12 15 18 21
do
   if [ `expr $hh % 6` -eq 0 ] ; then
      f1=prepqm$CDATE$hh.post
      echo >null
      f2=null
   else
      f1=prepqm`$NDATE -3 $CDATE$hh`.post
      f2=prepqm`$NDATE +3 $CDATE$hh`.post
   fi

#  make a three hour file from one or two six hour files

export pgm='rcdas_gr2rr'
. prep_step
   clean_unit
   export FORT20=$f1
   export FORT21=$f2
   export FORT51=rrfile
  
   startmsg
   echo $CDATE$hh | $EXECrcdas/rcdas_g6tor3x
   export err=$?;err_chk

#  modify the global format to a regional equivalent

   clean_unit
   export  FORT20=rrfile
   export  FORT51=prepbufr2.$CDATE$hh

   startmsg
   $EXECrcdas/rcdas_grtorr2
   export err=$?;err_chk
   [ $? -ne 0 ] && fatal "prebufr error prepbufr2.$CDATE$hh"
   [ ! -s prepbufr2.$CDATE$hh ] && fatal "prebufr error prepbufr2.$CDATE$hh"

   if [ "$SENDCOM" = 'YES' ] ; then
      cp prepbufr2.$CDATE$hh $COMOUT/
      touch $COMOUT/done.prebufr2.$DATE
   fi
done
fi

#  precip processing -  convert data to model grid

for f in $COMOUT/*us_mex.grb $COMOUT/CMORPH_025deg_*.hourly.grb
do
   ff=`basename $f`
   $COPYGB -g192 -i0 -x $f $ff.g192
done

for f in `ls $COMOUT/*lsmforce_noaa`
do
   tdate=`$WGRIB -V $f | egrep '(^rec|center.*subcenter.*process)' | \
      sed '$!N;s/\n//' | sed -e 's/^rec //' | grep ' APCP ' | \
      grep 'process 155' | $WGRIB -i -s -4yr $f -grib | cut -d: -f3 | cut -c3-`
   mv dump nldas.$tdate
   $COPYGB -g192 -i6 -x nldas.$tdate nldas.$tdate.g192
done

# run merging program
# uses many unit numbers

export pgm='rcdas_pcpto32'
. prep_step
clean_unit

for hh in 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23
do
   ln -sf nldas.$CDATE$hh.g192                             fort.`expr 11 + $hh`
   ln -sf CMORPH_025deg_$CDATE$hh.hourly.grb.g192          fort.`expr 35 + $hh`
   ln -sf $CDATE${hh}.us_mex.grb.g192                      fort.`expr 59 + $hh`
done

export FORT2=$FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks
# ln -s -f $FIXrcdas/rcdas_hgtsmref.r3245.2d_tmasks fort.2
ln -s -f $FIXrcdas/rcdas_EGRD3D00.tm12 fort.3
ln -s -f $FIXrcdas/rcdas_2degmaskout.grb_new fort.4


ln -s -f obsprcp.${CDATE}00    fort.101
ln -s -f obsprcp.${CDATE}03    fort.102
ln -s -f obsprcp.${CDATE}06    fort.103
ln -s -f obsprcp.${CDATE}09    fort.104
ln -s -f obsprcp.${CDATE}12    fort.105
ln -s -f obsprcp.${CDATE}15    fort.106
ln -s -f obsprcp.${CDATE}18    fort.107
ln -s -f obsprcp.${CDATE}21    fort.108

   startmsg
echo "$YEAR2 $MONTH $DAY" | $EXECrcdas/rcdas_pcpto32 > pcp.print
   export err=$?;err_chk
[ $? -ne 0 ] && fatal "precip merging"

if [ "$SENDCOM" = 'YES' ] ; then
  cp obsprcp.${CDATE}?? $COMOUT/
fi

touch $COMOUT/${model}.t21z.status_ingest
echo "finished injest $CDATE"
