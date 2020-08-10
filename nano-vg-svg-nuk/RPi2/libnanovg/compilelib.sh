#!/bin/bash
rm -f *.o libTestnanogl-dis.txt ../libTestnanogl.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -L. -I/opt/vc/include -lm -c Testnanogl.c 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -I/opt/vc/include -c nanovg.c 
arm-none-eabi-ar rcs libTestnanogl.a nanovg.o Testnanogl.o
arm-none-eabi-objdump -d libTestnanogl.a > libTestnanogl-dis.txt
cp libTestnanogl.a ../
