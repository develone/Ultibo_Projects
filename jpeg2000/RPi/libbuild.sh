#!/bin/bash 

rm -f dwtlift.o
rm -f libdwtlift.a
rm -f libopenjp2.a
cp  libopenjp2_rpi_e357de.a libopenjp2.a 

arm-none-eabi-gcc -L. -llibopenjp2 -O2 -mabi=aapcs -marm -march=armv6 -mfpu=vfp -mfloat-abi=hard -c dwtlift.c

arm-none-eabi-ar rcs libopenjp2.a dwtlift.o 

cp libopenjp2.a libdwtlift.a
  
