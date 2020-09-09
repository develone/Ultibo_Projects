#!/bin/bash
rm -f bwlp.o libbwlp.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__  -c bwlp.c 
 
arm-none-eabi-ar rcs libbwlp.a *.o
arm-none-eabi-ar t libbwlp.a
arm-none-eabi-objdump -d libbwlp.a > bwlp-obj.txt
cp libbwlp.a RPi3
