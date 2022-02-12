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
printf("In processstr\n"); 
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
char * returnfromprocessstr(char  *x)
{
    int i, l;
    l=strlen(x); 
    
    int outstr[l]; 
    
    char bitstr[l * 8];
    char *strbuf;
    printf("In returnfromprocessstr\n");
    //printf("C %d %s\n",l,x);
    
    strbuf = bitstr;
    
    for(i=0;i<l;i++) {
        printf("%d %08d ",i,asciiValueToBinary(*x));
        //printf("%08d",asciiValueToBinary(*x));
        outstr[i]=asciiValueToBinary(*x);
        sprintf(strbuf, "%08d", outstr[i]);
        x++;
        strbuf+=8;
    }

    printf("\n");
    //for(i=0;i<l;i++) printf("%08d",outstr[i]);
    printf(bitstr);
    printf("\n");
		return(strbuf);
}

int main() {
	char *p;
	char a[]="Now we are engaged in a great ci";
	p = a;
	processstr(p);
	returnfromprocessstr(p);
return (0);
}
