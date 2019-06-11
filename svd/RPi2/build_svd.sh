#!/bin/bash
gcc -c svd.c
gcc -c disp_mat.c
gcc -c trans_mat.c
gcc -c mul_mat.c

gcc test_svd.c svd.o disp_mat.o trans_mat.o mul_mat.o -lm -o test_svd
