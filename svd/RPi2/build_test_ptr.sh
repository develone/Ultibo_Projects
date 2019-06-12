#!/bin/bash
gcc -g -O3 -c disp_mat.c
gcc test_ptr.c disp_mat.o -o test_ptr
