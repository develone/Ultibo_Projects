#include <stdio.h>
#include <stdlib.h>

int main() {
int a, *pa;
FILE *ptr;

printf("%x\n",ptr);
ptr = fopen("640320com","wb");
pa=&(a);
printf("pa = 0x%x \n", pa);
a = 25;
printf("Compression ratio a = 0x%x a = %d \n", a,a);
fwrite(pa,1,sizeof(int),ptr);
a = 1;
printf("encode/decode a = 0x%x a = %d \n", a,a);
fwrite(pa,1,sizeof(int),ptr);
a = 0;
printf("a = 0x%x \n", a);
fwrite(pa,1,sizeof(int),ptr);
a = 0;
printf("a = 0x%x \n", a);
fwrite(pa,1,sizeof(int),ptr);
a = 640;
printf("Width a = 0x%x a = %d \n", a,a);
fwrite(pa,1,sizeof(int),ptr);
a = 360;
printf("Height a = 0x%x a = %d \n", a,a);
fwrite(pa,1,sizeof(int),ptr);
return 0;
}
