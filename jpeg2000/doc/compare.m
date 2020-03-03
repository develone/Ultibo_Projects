clear
close all
A = imread('lena_rgb_1024.bmp');
%figure
%imagesc(A);
%colorbar;

B = imread('ultibo.bmp');
%figure
%imagesc(B);
%colorbar;

sum = 0.;

ctn = 0.;
k = 3
	sum = 0;
	ctn = 0;
	for j = 1:1024
		for i = 1:1024
			ctn = ctn + 1;
			C = A(i,j,k);
			D = B(i,j,k);
			diff(ctn) = (C - D)^2;
			sum = sum + sqrt(diff(ctn));
			
		endfor
	endfor
	sum
	ctn
diffbar = sqrt(diff);
hist(diffbar,30);
if (k==1)
	title '1024x1024 red 125:1'
else
	if(k==2)
		title '1024x1024 green 125:1'
	else
		if(k==3)
			title '1024x1024 blue 125:1'
		endif
	endif
endif
		
mse= sum/ctn
