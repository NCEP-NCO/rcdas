#!/bin/sh
#
# takes the 1 hour ave/acc forecasts from NAM and makes 3 hour ave/acc
# note: the files have an unusual metadata .. the reference time is
#  not the start of the forecast but the start of the ave/acc time period.
#
# Steps
#   1:  convert files to the same reference time, restrictions ave/acc
#   2:  sort the records to get the correct order for wgrib2 -merge_fcst
#   3:  wgrib2 -merge_fcst 3 new_file
#   4:  convert from grib2 to grib1
#        note: works for all ave/acc but for speed it is restricted to APCP
#
# requirements: wgrib2 v2.0.5+     -set_date -1hr requires v2.0.5+
#               cnvgrib
#
# Input:
#              $NAM/nam.$cycle.awip3d01.tm$N.grib2  N = 01,02,...,06
# Output:
#              ./nam.$cycle.0-3.grib2        grib2 files
#              ./nam.$cycle.3-6.grib2
#              ./nam.$cycle.0-3.grib         grib1 files
#              ./nam.$cycle.3-6.grib
#                 
# NAM = directory with the NAM files
# cycle = t00z, t06z, t12z or t18z
# WGRIB2 = name of wgrib2 2.0.5+
# CNVGRIB = name of cnvgrib utility
#
#
# v1.0.0     9/2016 W Ebisuzaki    initial version
#

#NAM=/ptmpp1/Wesley.Ebisuzaki/nam
#WGRIB2=wgrib2new
#cycle=t00z
#cycle=t06z

# change files so they have the same reference time

# 1 hour fcst
# need to convert metadata to be compatible with junk2 and junk3
# octet 54 is set to 255 in NAM vs 01 from wgrib2

$WGRIB2 $NAM/nam.$cycle.awip3d01.tm06.grib2  -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "0-1 hour ave fcst" -grib junk1  \
   -if ":0-1 hour acc fcst:" -set_ave "0-1 hour acc fcst" -grib junk1

# 2 hour fcst
$WGRIB2 $NAM/nam.$cycle.awip3d01.tm05.grib2 -set_date -1hr -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "1-2 hour ave fcst" -grib junk2  \
   -if ":0-1 hour acc fcst:" -set_ave "1-2 hour acc fcst" -grib junk2

# 3 hour fcst
$WGRIB2 $NAM/nam.$cycle.awip3d01.tm04.grib2  -set_date -2hr -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "2-3 hour ave fcst" -grib junk3  \
   -if ":0-1 hour acc fcst:" -set_ave "2-3 hour acc fcst" -grib junk3

cat junk1 junk2 junk3 > junkall
$WGRIB2 junkall | sort -t: -k4,4 -k5,5 -k6,6n | wgrib2 -i junkall -grib junk_sorted
$WGRIB2 junk_sorted -merge_fcst 3 nam.$cycle.0-3.grib2
$CNVGRIB -g21 nam.$cycle.0-3.grib2 nam.$cycle.0-3.grib
rm junk1 junk2 junk3 junkall junk_sorted

# 1 hour fcst, fix metadata
$WGRIB2 $NAM/nam.$cycle.awip3d01.tm03.grib2 -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "0-1 hour ave fcst" -grib junk1  \
   -if ":0-1 hour acc fcst:" -set_ave "0-1 hour acc fcst" -grib junk1

# 2 hour fcst
$WGRIB2 $NAM/nam.$cycle.awip3d01.tm02.grib2 -set_date -1hr -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "1-2 hour ave fcst" -grib junk2  \
   -if ":0-1 hour acc fcst:" -set_ave "1-2 hour acc fcst" -grib junk2

# 3 hour fcst
$WGRIB2 $NAM/nam.$cycle.awip3d01.tm01.grib2  -set_date -2hr -match APCP \
   -if ":0-1 hour ave fcst:" -set_ave "2-3 hour ave fcst" -grib junk3  \
   -if ":0-1 hour acc fcst:" -set_ave "2-3 hour acc fcst" -grib junk3

cat junk1 junk2 junk3 > junkall
$WGRIB2 junkall | sort -t: -k4,4 -k5,5 -k6,6n | wgrib2 -i junkall -grib junk_sorted
$WGRIB2 junk_sorted -merge_fcst 3 nam.$cycle.3-6.grib2
$CNVGRIB -g21 nam.$cycle.3-6.grib2 nam.$cycle.3-6.grib
rm junk1 junk2 junk3 junkall junk_sorted

