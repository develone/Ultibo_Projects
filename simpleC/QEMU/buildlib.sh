#!/bin/bash
#export PATH=/home/devel/ultibo/core/fpc/bin:$PATH
rm -f *.o
rm -f libsimpleC.a

arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c simpleC.c -o simpleC.o

arm-none-eabi-ar rcs libsimpleC.a *.o
arm-none-eabi-ar -t libsimpleC.a > libsimpleC_obj.txt
