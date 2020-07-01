#!/bin/bash
#export PATH=/home/devel/ultibo/core/fpc/bin:$PATH
rm -f *.o
rm -f libsvd.a

arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c svd.c -o svd.o
arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c disp_mat.c -o disp_mat.o
arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c trans_mat.c -o trans_mat.o
arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c mul_mat.c -o mul_mat.o
arm-none-eabi-gcc -D_POSIX_THREADS -lpthread -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c mythread.c -o mythread.o
arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c pnmio.c -o pnmio.o
arm-none-eabi-gcc -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c error.c -o error.o
 
 
echo "Compiling example ultibo_th_svd "
arm-none-eabi-gcc -D_POSIX_THREADS -lpthread -I../include -O3 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -c ultibo_th_svd.c -o ultibo_th_svd.o

	
#gcc test_svd.c svd.o disp_mat.o -lm -o test_svd
arm-none-eabi-ar rcs libsvd.a *.o
arm-none-eabi-ar -t libsvd.a > libsvd_obj.txt
#fpc -vi -B -Tultibo -Parm -CpARMV7A -WpRPI3B @/home/devel/ultibo/core/fpc/bin/RPI3.CFG -O4 svd_FS_RPi3.lpr
