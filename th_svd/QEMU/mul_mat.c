#include <stdio.h>
#include <stdlib.h>

int mul(float **a,float **b,float **c,int m,int n,int p,int q) {
	int i,j,k,result;
	float sum=0.0;
	for (i = 0; i < m; i++) {
      for (j = 0; j < q; j++) {
        for (k = 0; k < p; k++) {
		  //printf("i %d j %d k %d a %5.3f b %5.3f \n",i,j,k,a[i][k],b[i][j]);
          sum = sum + a[i][k]*b[k][j];
          //printf("sum %5.3f \n",sum);
        }
 
        c[i][j] = sum;
        //printf(c %5.3f \n",c[i][j]);
        sum = 0.0;
      }
    }
	return(1);
}
