%Requires 3 files 
%-rw-r--r-- 1 pi pi 4194304 Feb 27 16:03 blue
%-rw-r--r-- 1 pi pi 4194304 Feb 27 16:03 green
%-rw-r--r-- 1 pi pi 4194304 Feb 27 16:02 red
%These were obtained from the decompression of test.j2k
%The file test.j2k was obtained from the compression of lena_rgb_2048.bmp

fid = fopen('red','r'); im1 = fread(fid, [2048,inf], 'char'); fclose(fid);
figure; imagesc(im1);
title"RED SUB BAND INPUT 2048 x 2048 RPi2 BARE-METAL ULTIBO"

fid = fopen('green','r'); im2 = fread(fid, [2048,inf], 'char'); fclose(fid);
figure; imagesc(im2);
title"GREEN SUB BAND INPUT 2048 x 2048 RPi2 BARE-METAL ULTIBO"

fid = fopen('blue','r'); im3 = fread(fid, [2048,inf], 'char'); fclose(fid);
figure; imagesc(im3);
title"BLUE SUB BAND INPUT 2048 x 2048 RPi2 BARE-METAL ULTIBO"
