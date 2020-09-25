#!/bin/bash
#rm -f kiss_fft.o libkissfft.a
rm -f *.o libkissfft.a

arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__  -D_POSIX_MONOTONIC_CLOCK -I libtess2/Include/ -c kiss_fft.c \
fftutil.c kiss_fftr.c kiss_fftnd.c kiss_fftr.c kiss_fftndr.c pstats.c
arm-none-eabi-ar rcs libkissfft.a *.o
arm-none-eabi-ar t libkissfft.a
arm-none-eabi-objdump -d libkissfft.a > kissfft-obj.txt
cp libkissfft.a RPi3
