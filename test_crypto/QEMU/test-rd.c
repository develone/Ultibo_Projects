#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>


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

	return(res);
}

const char* filename = "some.secret.enc";
int main(void) {

		uint8_t data[32];
		uint8_t r[32];
		uint8_t l[32];
		uint8_t right;
		uint8_t left;
		uint8_t lt;
		uint8_t rt;
		uint8_t *ptr;
		int i,j;   
		ptr = &data[0];

		FILE* input_file = fopen(filename, "rb");
    if (!input_file) {
        perror("fopen");
        exit(EXIT_FAILURE);
    }
		
		for(i=0;i<32;i++) {
			fread(ptr,sizeof(uint8_t),1,input_file);
			//printf("%x\n",data[i]);
			left = data[i] & 0xF0;
			left = left>>4;
			l[i] = left;
			
			if (left <=9) {
				left = left | 0x30;
			}
			else left = left + 0x37;
			right = data[i] & 0x0F;
			r[i] =  right;
			if (right <=9) {
				right = right | 0x30;
			}
			else right = right + 0x37;
			
			//printf("%d %x %x %c %x %c\n",i,data[i],left,left,right,right);
			
			ptr++;
		}	
		for(i=0;i<32;i++) {
			rt=l[i];
			lt=r[i];
			//printf("%d\n",rt);
			//printf("%x %08d %08d\n",data[i],asciiValueToBinary(rt),asciiValueToBinary(lt));
			printf("%08d%08d",asciiValueToBinary(rt),asciiValueToBinary(lt));
		} 

		printf("\n",rt);
}
