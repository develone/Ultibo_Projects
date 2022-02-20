#include <stdio.h>
#include <string.h>
#include <stdlib.h>
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

void returnfromprocessstr(char *x);

void returnasciifromprocessstr(char *x);

void processstr(char  *x)
{
    int i, l;
    l=strlen(x); 
    
    int outstr[l]; 
    
    char bitstr[l * 8];
    char *strbuf;
    
    //printf("C %d %s\n",l,x);
    
    strbuf = bitstr;
    
    for(i=0;i<l;i++) {
        //printf("%d %08d ",i,asciiValueToBinary(*x));
        //printf("%08d",asciiValueToBinary(*x));
        outstr[i]=asciiValueToBinary(*x);
        sprintf(strbuf, "%08d", outstr[i]);
        x++;
        strbuf+=8;
    }

    //printf("\n");
    //for(i=0;i<l;i++) printf("%08d",outstr[i]);
    printf(bitstr);
    printf("\n");
    returnfromprocessstr(bitstr);
}

unsigned long binaryToDecimal(char *binary, int length)
{
	int i;
	unsigned long decimal = 0;
	unsigned long weight = 1;
	binary += length - 1;
	weight = 1;
	for(i = 0; i < length; ++i, --binary)
	{
		if(*binary == '1')
			decimal += weight;
		weight *= 2;
	}
	
	return decimal;
}



void binaryToText(char *binary, int binaryLength, char *text, int symbolCount)
{
     
    int i;
		 
    for(i = 0; i < binaryLength; i+=8, binary += 8)
    {
        char *byte = binary;
        byte[8] = '\0';
        *text++ = binaryToDecimal(byte, 8);
    }
		 
    text -= symbolCount;
		 
 
}
void processbinascstr(char  *binary) {
/*
processbinascstr is going to be called from Pascal.
This is doing the main function from the program bin2ascii.c
Calls binaryToText(char *binary, int binaryLength, char *text, int symbolCount)
*/

int binaryLength,l;
l=strlen(binary); 
    
    int outstr[l]; 
    
    char ascstr[l / 8];
    char *strbuf;
		strbuf = ascstr;
binaryLength = strlen(binary);
//int symbolCount = binaryLength / 8;
//char text[symbolCount+1];
//char *textptr;
//*textptr=text[0];
printf("In C %d %d\n",binaryLength,l);
printf("%s\n",binary);
//binaryToText(binary,binaryLength,strbuf,l);
//printf("%s in binary is the following text:\n%s\n", binary, text);
//returnasciifromprocessstr(ascstr);
}

 