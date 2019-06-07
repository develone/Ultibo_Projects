#include <stdio.h>
#include <stdlib.h>

int disp(float **a,int m,int n) {
	int i,j,result;
	
	
	
for(i=0;i<m;i++)
	{
	for(j=0;j<n;j++)
	{
		printf("%5.2f ",a[i][j]);

	}
	printf("\n");
}
	
	
	return(1);
}
