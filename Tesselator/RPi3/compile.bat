arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -I libtess2/Include/ -c svgTessellator.c libtess2/Source/bucketalloc.c libtess2/Source/dict.c libtess2/Source/geom.c libtess2/Source/mesh.c libtess2/Source/priorityq.c libtess2/Source/sweep.c libtess2/Source/tess.c
arm-none-eabi-ar rcs libsvgTessellator.a svgTessellator.o bucketalloc.o dict.o geom.o mesh.o priorityq.o sweep.o tess.o
@echo off
del kernel7.img
del *.o
del *.ppu
c:\Ultibo\Core\fpc\3.1.1\bin\i386-win32\fpc -B -Tultibo -Parm -CpARMV7A -WpRPI3B @c:\Ultibo\Core\fpc\3.1.1\bin\i386-win32\RPI3.CFG -O2 -dPLATFORM_PI3 svgTessellator.lpr
echo Done.
echo copying kernel to image Dir
copy kernel7.img svgTessellator_image
echo copying kernel to SD
copy kernel7.img G:\
del kernel7.img