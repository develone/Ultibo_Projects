%Comaparing red.pgm with rcred.bin
for J = 1:256
	for I = 1:256
		a=im1(J,I);
		b=A(J,I);
		c = a - b;
		if ( c > 0)	
			c,I,J,A(I,J), im1(I,J)
		endif
		if ( c < 0)
			c,I,J,A(I,J), im1(I,J)
		endif
 
	endfor
endfor
%Comaparing grn.pgm with rcgrn.bin
for J = 1:256
	for I = 1:256
		a=im2(J,I);
		b=B(J,I);
		c = a - b;
		if ( c > 0)	
			c,I,J,B(I,J), im2(I,J)
		endif
		if ( c < 0)
			c,I,J,B(I,J), im2(I,J)
		endif
 
	endfor
endfor
%Comaparing blu.pgm with rcblu.bin
for J = 1:256
	for I = 1:256
		a=im3(J,I);
		b=C(J,I);
		c = a - b;
		if ( c > 0)	
			c,I,J,C(I,J), im3(I,J)
		endif
		if ( c < 0)
			c,I,J,C(I,J), im3(I,J)
		endif
 
	endfor
endfor
