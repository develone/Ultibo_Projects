#!/bin/bash
rm -f   checksum_ultibo.o libgps.a 
echo 'Compiling checksum_ultibo.c'

if [ $1 = "WR" ]; then 
	echo "Compiling with -D WR "
	arm-none-eabi-gcc -D WR -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c checksum_ultibo.c
else
	echo "Not compiling with -D WR "
	arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c checksum_ultibo.c
fi
echo 'Creating libgps.a'
arm-none-eabi-ar rcs libgps.a checksum_ultibo.o
 
date
ls -la checksum_ultibo.o libgps.a
