clear
n = 25;
p = 1;


fid = fopen('red.bin','r'); im1 = fread(fid, [256,inf], 'int32');
if p == 1
  figure;
  imagesc(im1);
  colorbar;
  title "red image 256 X 256 "
end
 
[U,S,V] = svd(im1);
VT = V';
im2 = U*S*VT;

if p == 1
  figure;
  imagesc(im2);
  colorbar;
  title "red image 256 X 256 reconstructed from U*S*VT "
end
for i = 1:n
  for j = 1:n
    if i == j
      a(i) = S(i,j);
    end
   end
end
figure
stem(a)
title "n S values from [U,S,V'] = svd(im1)"