#!/bin/bash
rm -f *.o 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__  -c filter.c 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__  -c example.c 
arm-none-eabi-ar rcs libfilter.a *.o
arm-none-eabi-objdump -d libfilter.a > filter-obj.txt
cp libfilter.a RPi3

