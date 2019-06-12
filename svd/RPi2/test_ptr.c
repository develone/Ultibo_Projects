#include<stdio.h> 
#include<stdlib.h> 

int main() 
{ 
	int r=9, c=8, len=0,result; 
	float *pa, **ppa; 
	int count = 0,i,j; 

	len = sizeof(float *) * r + sizeof(float) * c * r; 
	printf("len = %d \n",len);
	ppa = (float **)malloc(len); 

	// ptr is now pointing to the first element in of 2D array 
	pa = (float *)(ppa + r); 

	// for loop to point rows pointer to appropriate location in 2D array 
	for(i = 0; i < r; i++) 
		ppa[i] = (pa + c * i); 
/*
	for (i = 0; i < r; i++) 
		for (j = 0; j < c; j++) 
			arr[i][j] = (float)++count; // OR *(*(arr+i)+j) = ++count 
*/
ppa[0][0]=1.0;
ppa[0][1]=2.0;
ppa[0][2]=1.0;
ppa[0][3]=22.0;
ppa[0][4]=11.0;
ppa[0][5]=22.0;
ppa[0][6]=11.0;
ppa[0][7]=11.0;

ppa[1][0]=3.0;
ppa[1][1]=4.0;
ppa[1][2]=2.0;
ppa[1][3]=41.0;
ppa[1][4]=21.0;
ppa[1][5]=41.0;
ppa[1][6]=21.0; 
ppa[1][7]=11.0;

ppa[2][0]=5.0;
ppa[2][1]=6.0;
ppa[2][2]=3.0;
ppa[2][3]=63.0;
ppa[2][4]=33.0;
ppa[2][5]=63.0;
ppa[2][6]=33.0;
ppa[2][7]=11.0;

ppa[3][0]=7.0;
ppa[3][1]=8.0;
ppa[3][2]=4.0;
ppa[3][3]=82.0;
ppa[3][4]=42.0;
ppa[3][5]=82.0;
ppa[3][6]=42.0;
ppa[3][7]=11.0;
 
ppa[4][0]=7.0;
ppa[4][1]=8.0;
ppa[4][2]=4.0;
ppa[4][3]=282.0;
ppa[4][4]=242.0; 
ppa[4][5]=282.0;
ppa[4][6]=242.0; 
ppa[4][7]=11.0;

ppa[5][0]=7.0;
ppa[5][1]=8.0;
ppa[5][2]=4.0;
ppa[5][3]=182.0;
ppa[5][4]=142.0;
ppa[5][5]=82.0;
ppa[5][6]=42.0;
ppa[5][7]=11.0;

ppa[6][0]=1.0;
ppa[6][1]=2.0;
ppa[6][2]=1.0;
ppa[6][3]=22.0;
ppa[6][4]=11.0;
ppa[6][5]=82.0;
ppa[6][6]=42.0;
ppa[6][7]=11.0;

ppa[7][0]=3.0;
ppa[7][1]=4.0;
ppa[7][2]=2.0;
ppa[7][3]=41.0;
ppa[7][4]=21.0;
ppa[7][5]=82.0;
ppa[7][6]=42.0; 
ppa[7][7]=11.0;

ppa[8][0]=5.0;
ppa[8][1]=6.0;
ppa[8][2]=3.0;
ppa[8][3]=63.0;
ppa[8][4]=33.0;
ppa[8][5]=82.0;
ppa[8][6]=42.0;
ppa[8][7]=11.0;
printf("a row = %d col = %d \n",r,c);
result = disp(ppa,r,c);
/*
	for (i = 0; i < r; i++) { 
		for (j = 0; j < c; j++) {
			printf("%5.3f ", ppa[i][j]); 
		}
		printf("\n");
	}
*/
	return 0; 
} 
