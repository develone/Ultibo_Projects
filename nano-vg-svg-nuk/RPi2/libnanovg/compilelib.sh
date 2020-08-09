#!/bin/bash
rm -f *.o libnanovg.a libnanovg-dis.txt
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -I/opt/vc/include -c nanovg.c 
arm-none-eabi-ar rcs libnanovg.a nanovg.o
arm-none-eabi-objdump -d nanovg.o > libnanovg-dis.txt
