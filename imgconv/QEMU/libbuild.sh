#!/bin/bash
rm -f libcvtutils.a cvtutils.o
arm-none-eabi-gcc -L. -llibopenjp2 -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c cvtutils.c 
arm-none-eabi-ar rcs libcvtutils.a cvtutils.o
arm-none-eabi-ar t libcvtutils.a > libcvtutils_obj.txt
wc libcvtutils_obj.txt