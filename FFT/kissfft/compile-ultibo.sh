#!/bin/bash
rm -f kiss_fft.o libkissfft.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c kiss_fft.c \
data_processor.c data_processor_test.c
arm-none-eabi-ar rcs libkissfft.a *.o
arm-none-eabi-ar t libkissfft.a
arm-none-eabi-objdump -d libkissfft.a > kissfft-obj.txt
cp libkissfft.a RPi3
