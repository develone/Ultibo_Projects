#!/bin/bash
gcc -c svd.c
gcc -c disp_mat.c
gcc test_svd.c svd.o disp_mat.o -lm -o test_svd
