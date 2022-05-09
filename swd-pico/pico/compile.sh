#!/bin/sh
rm -f swdloader.o

#arm-none-eabi-gcc -DAARCH=32 -mcpu=cortex-a7 -marm -mfpu=neon-vfpv4 -mfloat-abi=$(FLOAT_ABI)  -o swdloader.o swdloader.cpp
arm-none-eabi-gcc -c -I../include -I../ -DAARCH=32 -mcpu=cortex-a7 -marm -mfpu=neon-vfpv4 -mfloat-abi=hard  -o swdloader.o swdloader.cpp
arm-none-eabi-objdump -d swdloader.o > swdloader.dis
