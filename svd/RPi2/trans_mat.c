#include <stdio.h>
#include <stdlib.h>

int trans(float **a,float **b,int m,int n) {
 
	int i,j,result;
 	

for(i=0;i<m;i++)
	{
	for(j=0;j<n;j++)
	{
		b[j][i] = a[i][j];
	}
	
}

	
	return(1);
}
