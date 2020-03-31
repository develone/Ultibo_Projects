clear
close all
A = imread('red.pgm');
figure
imagesc(A);
colorbar;
colormap 'gray';
title("red.pgm");
[U,S,V] = svd(A);
%S(128:256,128:256) = 0;
AR = U*S*V';
V;
V'; 
figure
imagesc(AR);
title("rred.pgm");
colorbar;
colormap 'gray';

B = imread('grn.pgm');
figure
imagesc(B);
colorbar;
colormap 'gray';
title("grn.pgm");
[U,S,V] = svd(B);
%S(128:256,128:256) = 0;
BR = U*S*V';
V;
V'; 
figure
imagesc(BR);
colorbar;
title("rgrn.pgm");
colormap 'gray';

C = imread('blu.pgm');
figure
imagesc(C);
colorbar;
title("blu.pgm");
colormap 'gray';
[U,S,V] = svd(C);
%S(128:256,128:256) = 0;
CR = U*S*V';
V;
V'; 
figure
imagesc(CR);
colorbar;
title("rblu.pgm");
colormap 'gray';
n = 25;
p = 0;

fid = fopen('Sred.bin','r'); red = fread(fid, 256, 'float');
fid = fopen('Sgrn.bin','r'); grn = fread(fid, 256, 'float');
fid = fopen('Sblu.bin','r'); blu = fread(fid, 256, 'float');

fid = fopen('rcred.bin','r'); im1 = fread(fid, [256,inf], 'int32');
fid = fopen('rcgrn.bin','r'); im2 = fread(fid, [256,inf], 'int32');
fid = fopen('rcblu.bin','r'); im3 = fread(fid, [256,inf], 'int32');
 
aa(1:n)= red(1:n);
figure
stem(aa)
title("n S values red image from C img_svd")

figure;
imagesc(im1);
colorbar;
colormap 'gray';
title 'rcred svd from C'


bb(1:n)= grn(1:n);
figure
stem(bb)
title("n S value green image from C img_svd")

figure;
imagesc(im2);
colorbar;
colormap 'gray';
title 'rcgrn svd from C'


cc(1:n)= blu(1:n);
figure
stem(cc)
title("n S values blue image from C img_svd")

figure;
imagesc(im3);
colorbar;
colormap 'gray';
title 'rcblu svd from C'


 
