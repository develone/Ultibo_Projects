#include <stdio.h>
#include <string.h>

int asciiValueToBinary(int asciiInput)
{
	int res = 0, i = 1, rem;
        
	while (asciiInput > 0)
	{
		rem = asciiInput % 2;
		res = res + (i * rem);
		asciiInput = asciiInput / 2;
		i = i * 10;
	}
	//printf("%x\n",res);
	return(res);
}

void processstr(char  *x) {
int i,l;
l=strlen(x); 
int outstr[l]; 
//printf("C %d %s\n",l,x);
for(i=0;i<l;i++) {
	printf("%d %08d ",i,asciiValueToBinary(*x));
  //printf("%08d",asciiValueToBinary(*x));
  outstr[i]=asciiValueToBinary(*x);
	x++;
}
printf("\n");
for(i=0;i<l;i++) printf("%08d",outstr[i]);
printf("\n");
}

