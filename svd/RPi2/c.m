clear
n = 25;
p = 0;

fid = fopen('S.bin','r'); aa = fread(fid, 256, 'float');

fid = fopen('reconst.bin','r'); im4 = fread(fid, [256,inf], 'int32');


fid = fopen('Srasp.bin','r'); aaa = fread(fid, 256, 'float');

fid = fopen('reconstrasp.bin','r'); im5 = fread(fid, [256,inf], 'int32');
figure;
imagesc(im4);
title 'reconst svd from C'
b(1:n)= aa(1:n);
figure
stem(b)
title("n S values from C img_svd")

figure;
imagesc(im5);
title 'reconst svd from C'
b(1:n)= aaa(1:n);
figure
stem(b)
title("n S values from C img_svd")
