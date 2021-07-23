#include <stdio.h>
#include <stdlib.h>
#ifdef Ultibo
void simpleC() {
#else
void main() {
#endif
	int i;
	printf("This is a C program called by Ultibo\n");
	printf("Hello World \n");
	for(i=0;i<10;i++) printf("i= %d \n",i);
}
