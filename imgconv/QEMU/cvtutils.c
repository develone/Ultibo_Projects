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
int i;
//printf("%d %d\n",sizeof(x),strlen(x));
//printf("%s\n",x);
for(i=0;i<strlen(x);i++) {
	printf("%08d",asciiValueToBinary(*x));
	x++;
}
printf("\n");
}

