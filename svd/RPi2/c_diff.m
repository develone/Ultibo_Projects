clear
n = 25;
p = 0;
fid = fopen('S.bin','r'); aa = fread(fid, 256, 'float');
fid = fopen('Srasp.bin','r'); ar = fread(fid, 256, 'float');
fid = fopen('reconstrasp.bin','r'); im3 = fread(fid, [256,inf], 'int32');

fid = fopen('reconst.bin','r'); im4 = fread(fid, [256,inf], 'int32');
%ver from ultibo - raspbian
adiff = aa - ar;
b(1:n)= aa(1:n);
arasp = ar(1:n);
im2 = im4 - im3;
for i = 1:256
  for j = 1:256
    if im2(i,j) != 0
      im2(i,j), i, j
      im2(i,j) = 0;
      im4(i,j),im3(i,j)
    end
   end
end
figure;
imagesc(im4);
colorbar
title "Ultibo"
figure;
imagesc(im3);
colorbar
title "RaspBian"
figure;
imagesc(im2);
colorbar
title "Diff Ultibo RaspBian"
%title 'reconst svd from C'
%figure
%stem(b)
%title("n S values from C img_svd")
