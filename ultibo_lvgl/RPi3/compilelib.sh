#!/bin/bash
rm -f *.o libultiboClvgl.a
arm-none-eabi-gcc -O2 -DUltibo -D_POSIX_THREADS -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -I../ -c ultiboClvgl.c
cp liblvgl.a libultiboClvgl.a
arm-none-eabi-ar rcs libultiboClvgl.a ultiboClvgl.o
arm-none-eabi-ar t libultiboClvgl.a > libultiboClvgl_obj.txt
arm-none-eabi-objdump -d libultiboClvgl.a > libultiboClvgl_disasm.txt
