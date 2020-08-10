#!/bin/bash
rm -f *.a *.o 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -I/opt/vc/include -c Testnanogl.c
arm-none-eabi-ar rcs libTestnanogl.a Testnanogl.o