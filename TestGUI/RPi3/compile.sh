#!/bin/bash
rm -f *.o libsvgTessellator.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c svgTessellator.c \
libtess2/Source/bucketalloc.c libtess2/Source/dict.c libtess2/Source/geom.c \
libtess2/Source/mesh.c libtess2/Source/priorityq.c libtess2/Source/sweep.c \
libtess2/Source/tess.c
arm-none-eabi-ar rcs libsvgTessellator.a svgTessellator.o bucketalloc.o \
dict.o geom.o mesh.o priorityq.o sweep.o tess.o
