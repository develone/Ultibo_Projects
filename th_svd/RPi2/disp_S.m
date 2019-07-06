clear
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
title 'rcred svd from C'


bb(1:n)= grn(1:n);
figure
stem(bb)
title("n S value green image from C img_svd")

figure;
imagesc(im2);
colorbar;
title 'rcgrn svd from C'


cc(1:n)= blu(1:n);
figure
stem(cc)
title("n S values blue image from C img_svd")

figure;
imagesc(im3);
colorbar;
title 'rcblu svd from C'


 
