#!/bin/bash
rm -f *.o liblvgl.a RPi3/liblvgl.a
arm-none-eabi-gcc -DUltibo -D_POSIX_THREADS -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 \
-mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c lvgl/src/lv_core/*.c \
lvgl/src/lv_draw/*.c lvgl/src/lv_font/*.c lvgl/src/lv_gpu/*.c lvgl/src/lv_hal/*.c \
lvgl/src/lv_misc/*.c lvgl/src/lv_themes/*.c lvgl/src/lv_widgets/*.c \
lv_examples/src/lv_demo_printer/*.c lv_examples/src/lv_demo_printer/images/*.c \
lv_examples/src/lv_demo_benchmark/*.c lv_examples/assets/*.c \
lv_examples/src/lv_demo_stress/*.c lv_examples/src/lv_demo_widgets/*.c \
lv_examples/src/lv_demo_keypad_encoder/*.c 

arm-none-eabi-ar rcs liblvgl.a *.o
arm-none-eabi-ar t liblvgl.a > liblvgl_obj.txt
arm-none-eabi-objdump -d liblvgl.a > liblvgl_disasm.txt
cp liblvgl.a RPi3/
rm -f *.o

