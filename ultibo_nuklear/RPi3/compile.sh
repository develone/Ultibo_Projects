#!/bin/bash
rm -f *.o libultiboCgeneric.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c ultiboCgeneric.c 
arm-none-eabi-ar rcs libultiboCgeneric.a ultiboCgeneric.o
