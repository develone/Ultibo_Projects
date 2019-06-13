#include <stdio.h>
#include <stdlib.h>


void test_svd() {
/*
 * Input to dsvd is as follows:
 *   a = mxn matrix to be decomposed, gets overwritten with u
 *   m = row dimension of a
 *   n = column dimension of a
 *   w = returns the vector of singular values of a
 *   v = returns the right orthogonal transformation matrix
 * dsvd(float **a, int m, int n, float *w, float **v)
*/	

extern int dsvd(float **a, int m, int n, float *w, float **v);

extern int trans(float **a,float **b,int m,int n);
extern int disp(float **a,int m,int n);
extern int mul(float **a,float **b,float **c,int m,int n,int p,int q);	

int m=9,n=8,i,j,p=9,q=9,result,len1,len2,len3;
 
float w[m],*pw;

/*	
 *  a 9 x 8
 *  u 9 x 8
 *  v 9 x 8 
 *  ds 9 x 8 
 *  vt 8 x 9
 *  uds 9 x 8
 *  udsvt 9 X 9
*/

//Several of the arrays use 2 pointers to allocate memory. 
//9 x 8 arrays
float *pv, **ppv;
float *puds, **ppuds;
float *pa,**ppa;
float *pds, **ppds;
//8 x 9 arrays
float *pvt, **ppvt;
//9 x 9 arrays
float *pudsvt, **ppudsvt;

len1 = sizeof(float *) * m + sizeof(float) * n * m;
len2 = sizeof(float *) * n + sizeof(float) * m * n;
len3 = sizeof(float *) * p + sizeof(float) * p * q;
printf("len = %d len2 = %d len3 = %d\n",len1, len2,len3);
ppv = (float **)malloc(len1);
ppuds = (float **)malloc(len1);
ppa = (float **)malloc(len1);
ppds = (float **)malloc(len1);
ppvt = (float **)malloc(len2);
ppudsvt = (float **)malloc(len3);

// pv, puds, pa, pds, pvt, and pudsvt are now pointing to the first elements of 2D arrays 
pv = (float *)(ppv + m);
puds = (float *)(ppuds + m);
pa = (float *)(ppa + m);
pds = (float *)(ppds + m);
pvt = (float *)(ppvt + n);
pudsvt = (float *)(ppudsvt + p);
// for loop to point rows pointer to appropriate location in 2D array 
for(i = 0; i < m; i++) {
	ppa[i] = (pa + n * i);
	ppuds[i] = (puds + n * i);
	ppv[i] = (pv + n * i);
	ppds[i] = (pds + n * i);

}	

for(i = 0; i < m; i++) ppvt[i] = (pvt + n * i);
for(i = 0; i < m; i++) ppudsvt[i] = (pudsvt + q * i); 
 
 
	
	
 
pw=(float *)&w;
 



/*
printf("pa 0x%x ppa 0x%x  \n",pa,ppa);
printf("pv = 0x%x ppv = 0x%x \n",pv,ppv);
printf("pvt = 0x%x ppvt = 0x%x \n",pvt,ppvt);
printf("pds = 0x%x ppds = 0x%x \n",pds,ppds);
printf("puds = 0x%x ppuds = 0x%x \n",puds,ppuds);
printf("pudsvt = 0x%x ppudsvt = 0x%x \n",pudsvt,ppudsvt);
*/

/*
 * a = { {1.0,2.0,1.0.22.0,11.0},{3.0,4.0,2.0,41.0,21.0},{5.0,6.0,3.0,63.0,33.0},{7.0,8.0,4.0,282.0,242.0},{7.0,8.0,4.0,182.0,142.0}};
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
 
printf("a row = %d col = %d \n",m,n);
result = disp(ppa,m,n);
/*
for(i=0;i<m;i++)
	{
	for(j=0;j<n;j++)
	{
		printf("0x%x 0x%x 0x%x %5.2f %5.2f\n",ppa,pa,&a[i][j],a[i][j],*pa);
		pa++;
	}
	
}
ppa=&pa;
pa=&a;
*/
 

//printf("pa 0x%x ppa 0x%x  \n",pa,ppa);
result = dsvd(ppa,m,n,pw,ppv);
printf("U row = %d col = %d \n",m,n);
result = disp(ppa,m,n);
printf("Singular Values\n");
for(i=0;i<m;i++) { 
	for(j=0;j<n;j++) {
		ppds[i][j] = 0;
	}	
}	

j=0;
for(i=0;i<m;i++) { 
	ppds[i][j] = w[i];
	j++;
}
printf("S row = %d col = %d \n",m,n);	
result = disp(ppds,m,n);	
//for(i=0;i<m;i++) printf("%5.2f \n",w[i]);

printf("V row = %d col = %d \n",m,n);
result = disp(ppv,m,n);

printf("V' row = %d col = %d \n",n,m);
 
result = trans(ppv,ppvt,m,n);
result = disp(ppvt,n,m);
printf("Call mul u * s  \n");
result = mul(ppa,ppds,ppuds,m,n,p,q);
printf("UDS row = %d col = %d \n",m,n);
result = disp(ppuds,m,n);
printf("Call mul u * ds * vt \n");

result = mul(ppuds,ppvt,ppudsvt,m,n,n,m);
printf("USDVT row = %d col = %d \n",p,q); 
 

result = disp(ppudsvt,p,q);
/*
free(ppv);
free(ppuds);
free(ppa);
free(ppds);
free(ppvt);
*/
}
