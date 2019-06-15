#!/bin/bash
gcc -g -O3 -c svd.c
gcc -g -O3 -c disp_mat.c
gcc -g -O3 -c trans_mat.c
gcc -g -O3 -c mul_mat.c

gcc -g -O3 img_svd.c svd.o disp_mat.o trans_mat.o mul_mat.o -lm -o img_svd
