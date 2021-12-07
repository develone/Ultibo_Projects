#!/bin/bash 

rm -f dwtlift.o
rm -f libdwtlift.a
rm -f libopenjp2.a
#cp  libopenjp2_816f53.a libopenjp2.a 
cp  src/libopenjp2.a libopenjp2.a 

arm-none-eabi-gcc -L. -llibopenjp2 -O3 -mcpu=arm1176jzf-s -mabi=aapcs -marm -mfpu=vfp -mfloat-abi=hard -c dwtlift.c

arm-none-eabi-ar rcs libopenjp2.a dwtlift.o 

cp libopenjp2.a libdwtlift.a
arm-none-eabi-ar t libdwtlift.a > libdwtlift_obj.txt
arm-none-eabi-objdump -d libdwtlift.a > libdwtlift.txt 
  
