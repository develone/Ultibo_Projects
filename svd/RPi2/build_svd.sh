#!/bin/bash
rm -f *.o test_svd img_svd

gcc -g -O3 -c svd.c
gcc -g -O3 -c disp_mat.c
gcc -g -O3 -c trans_mat.c
gcc -g -O3 -c mul_mat.c

if [ $1 = '1' ]; then
	echo "Compiling test_svd"
	gcc -g -O3 test_svd.c svd.o disp_mat.o trans_mat.o mul_mat.o -lm -o test_svd
else
	echo "Compiling img_svd"
	gcc -g -O3 img_svd.c svd.o disp_mat.o trans_mat.o mul_mat.o -lm -o img_svd
fi
