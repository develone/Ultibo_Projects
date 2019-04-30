#!/bin/bash 

rm -f dwtlift.o
rm -f libdwtlift.a
rm -f libopenjp2.a
cp  libopenjp2_816f53.a libopenjp2.a 

arm-none-eabi-gcc -L. -llibopenjp2 -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c dwtlift.c

arm-none-eabi-ar rcs libopenjp2.a dwtlift.o 

cp libopenjp2.a libdwtlift.a
  
