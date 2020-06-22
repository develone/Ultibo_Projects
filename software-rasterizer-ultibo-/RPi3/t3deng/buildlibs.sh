#!/bin/bash
rm -f *.o *.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c ../Testnanogl.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib1.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib4.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib5.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib6.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib7.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib8.c 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib9.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib10.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib11.c
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib12.c 
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -c t3dlib13.c
arm-none-eabi-ar rcs libt3dlib1.a t3dlib1.o
arm-none-eabi-ar rcs libt3dlib4.a t3dlib4.o
arm-none-eabi-ar rcs libt3dlib5.a t3dlib5.o
arm-none-eabi-ar rcs libt3dlib6.a t3dlib6.o
arm-none-eabi-ar rcs libt3dlib7.a t3dlib7.o
arm-none-eabi-ar rcs libt3dlib8.a t3dlib8.o
arm-none-eabi-ar rcs libt3dlib9.a t3dlib9.o
arm-none-eabi-ar rcs libt3dlib10.a t3dlib10.o
arm-none-eabi-ar rcs libt3dlib11.a t3dlib11.o
arm-none-eabi-ar rcs libt3dlib12.a t3dlib12.o
arm-none-eabi-ar rcs libt3dlib13.a t3dlib13.o
arm-none-eabi-ar rcs libTestnanogl.a Testnanogl.o
cp *.a ../
