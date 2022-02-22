#/bin/sh
set -x

source ../versions/build.ver
module reset
module use `pwd`
module load build-rcdas.module.lua
module list

export FC=ftn

make clean
make

