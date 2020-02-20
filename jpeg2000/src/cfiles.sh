#!/bin/bash
#commit 563ecfb55ca77c0fc5ea19e4885e00f55ec82ca9 (HEAD -> master, upstream/master, origin/master, origin/HEAD)
#Author: Even Rouault <even.rouault@spatialys.com>
#Date:   Thu Feb 13 09:59:17 2020 +0100

#    opj_compress: improve help message regarding new IMF switch

#devel@mypi3-15:~/openjpeg/build/bin $ ./opj_compress -r 125 -i lena_rgb_512.bmp -o test2.j2k

#Error, invalid green mask value 0
#Unable to load bmp file


#devel@mypi3-15:~/Ultibo_Projects/jpeg2000/src $ ./compile_ultibo.sh 
#The word count here should be 21
#the word count in /home/pi/jpeg-2000-test/bare-metal/openjp
#when ./libbuild.sh is executed should be 22
# 21  21 161 libopenjp2_obj.txt

#devel@mypi3-15:~/Ultibo_Projects/jpeg2000/RPi2 $ ./libbuild.sh 
#dwtlift.c: In function 'decompress':
#dwtlift.c:658:3: warning: implicit declaration of function 'octave_write_byte'; did you mean 'opj_write_tile'? [-Wimplicit-function-declaration]
#   octave_write_byte(r_decompress_fn,r_decompress,da_x1*da_y1);
#   ^~~~~~~~~~~~~~~~~
#   opj_write_tile

#Used Lazarus IDE (Ultibo Edition) to compile kernel7.img

#devel@mypi3-15:~/Ultibo_Projects/jpeg2000/RPi2 $ scrot -d 10 -s laz.png

#devel@mypi3-15:~/openjpeg/build/bin $ ./opj_decompress -i test.j2k -o ultibo.bmp
 
#[INFO] Start to read j2k main header (0).
#[INFO] Main header has been correctly decoded.
#[INFO] No decoded area parameters, set the decoded area to the whole image
#[INFO] Header of tile 1 / 1 has been read.
#[INFO] Generated Outfile ultibo.bmp
#decode time: 144 ms

diff ~/openjpeg/src/lib/openjp2/bio.c bio.c
diff ~/openjpeg/src/lib/openjp2/cio.c cio.c
#dwt.c & dwt.h
diff ~/openjpeg/src/lib/openjp2/dwt.c dwt.c
diff ~/openjpeg/src/lib/openjp2/dwt.h dwt.h
diff ~/openjpeg/src/lib/openjp2/event.c event.c
diff ~/openjpeg/src/lib/openjp2/function_list.c function_list.c
diff ~/openjpeg/src/lib/openjp2/image.c image.c
diff ~/openjpeg/src/lib/openjp2/invert.c invert.c
diff ~/openjpeg/src/lib/openjp2/j2k.c j2k.c
#jp2.c & jp2.h
diff ~/openjpeg/src/lib/openjp2/jp2.c jp2.c
diff ~/openjpeg/src/lib/openjp2/jp2.h jp2.h
#mct.c & mct.h
diff ~/openjpeg/src/lib/openjp2/mct.c mct.c
diff ~/openjpeg/src/lib/openjp2/mct.h mct.h
#mqc.c & mqc.h
diff ~/openjpeg/src/lib/openjp2/mqc.c mqc.c
diff ~/openjpeg/src/lib/openjp2/mqc.h mqc.h
#openjpeg.c & openjpeg,h
diff ~/openjpeg/src/lib/openjp2/openjpeg.c openjpeg.c
diff ~/openjpeg/src/lib/openjp2/openjpeg.h openjpeg.h
diff ~/openjpeg/src/lib/openjp2/opj_clock.c opj_clock.c
diff ~/openjpeg/src/lib/openjp2/opj_malloc.c opj_malloc.c
#pi.c & pi.h
diff ~/openjpeg/src/lib/openjp2/pi.c  pi.c
diff ~/openjpeg/src/lib/openjp2/pi.h  pi.h
diff ~/openjpeg/src/lib/openjp2/raw.c raw.c
#t1.c & t1.h
diff ~/openjpeg/src/lib/openjp2/t1.c t1.c
diff ~/openjpeg/src/lib/openjp2/t1.h t1.h
#t2.c & t2.h
diff ~/openjpeg/src/lib/openjp2/t2.c t2.c
diff ~/openjpeg/src/lib/openjp2/t2.c t2.h
#tcd.c & tcd.h
diff ~/openjpeg/src/lib/openjp2/tcd.c tcd.c
diff ~/openjpeg/src/lib/openjp2/tcd.h tcd.h
diff ~/openjpeg/src/lib/openjp2/tgt.c tgt.c
#thread.c & thread.h
diff ~/openjpeg/src/lib/openjp2/thread.c thread.c
diff ~/openjpeg/src/lib/openjp2/thread.h thread.h
