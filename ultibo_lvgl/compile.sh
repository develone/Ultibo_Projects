#!/bin/bash
rm -f *.o libultiboCgeneric.a
arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c ultiboClvgl.c lvgl/src/lv_core/*.c \
lvgl/src/lv_draw/*.c lvgl/src/lv_font/*.c lvgl/src/lv_gpu/*.c lvgl/src/lv_hal/*.c \
lvgl/src/lv_misc/*.c lvgl/src/lv_themes/*.c lvgl/src/lv_widgets/*.c \
lv_examples/src/lv_demo_printer/*.c lv_examples/src/lv_demo_printer/images/*.c \
lv_examples/src/lv_demo_benchmark/*.c lv_examples/assets/*.c 
arm-none-eabi-ar rcs libultiboClvgl.a *.o
arm-none-eabi-objdump -d libultiboClvgl.a > libultiboClvgl.txt
cp libultiboClvgl.a RPi3/

