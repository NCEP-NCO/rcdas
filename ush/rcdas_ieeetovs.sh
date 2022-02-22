#!/bin/sh
#
# processes satellite data in bufr and convert to a form for RCDAS (ieee)
#  runs two programs, convert to ieee, and thin to RCDAS grid
#
set -euax

type=$1; isat=$2; date=$3; here=`pwd`

extrtv1b=$EXECrcdas/rcdas_bufr_extrtv1b
extrat1b=$EXECrcdas/rcdas_bufr_extrat1b
bufr2i3e=$EXECrcdas/rcdas_bufr2i3e

[ $type = 1bmsu  ] && { skip=1; thin=$extrtv1b; otyp=m1bn$isat; }
[ $type = 1bhrs2 ] && { skip=2; thin=$extrtv1b; otyp=h1bn$isat; }
[ $type = 1bhrs3 ] && { skip=2; thin=$extrat1b; otyp=h1bn$isat; }

file=$type.$date.bufr_d
if [ -s $COMOUT/$file* ]; then
   cp $COMOUT/$file* . 
else
   if [ -s $COMOUT_P1/$file* ]; then
      cp $COMOUT_P1/$file* .
   else
      echo " cannot find the $file ....  "
  fi
fi

if [ -s  $file.gz ] ; then
  [ -f $file ] && rm $file
  gunzip $file.gz
fi

# convert the bufr file to ieee
export FORT20=$file
export FORT51=$file.ieee
$bufr2i3e

$thin  <<EOF
 &input
 nfile=1,
 isat=$isat,
 iyrmax=9999,
 imomax=0,
 idymax=0,
 ihrmax=0,
 imimax=0,
 iyrmin=0000,
 imomin=0,
 idymin=0,
 ihrmin=0,
 imimin=0,
 iskipl=$skip,
 iskips=$skip,
 ofile='$here/$otyp.$date.ieee_d',
 /
$file.ieee
EOF
