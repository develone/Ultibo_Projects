#include <stdio.h>
#include <stdlib.h>


void main() {
/*
 * Input to dsvd is as follows:
 *   a = mxn matrix to be decomposed, gets overwritten with u
 *   m = row dimension of a
 *   n = column dimension of a
 *   w = returns the vector of singular values of a
 *   v = returns the right orthogonal transformation matrix
 * dsvd(float **a, int m, int n, float *w, float **v)
*/	
FILE *inptr,*outptr;
int *inbuf, *inbuffr;

extern int dsvd(float **a, int m, int n, float *w, float **v);

extern int trans(float **a,float **b,int m,int n);
extern int disp(float **a,int m,int n);
extern int mul(float **a,float **b,float **c,int m,int n,int p,int q);	

int m=256,n=256,i,j,p=256,q=256,result,len1,len2,len3;
inbuf = (int *)malloc(sizeof(int)*m*n);
inbuffr = inbuf;
inptr = fopen("red.bin","r");
if (inptr == 0) printf("file not found\n");
len1 = fread(inbuf,sizeof(int),m*n,inptr);
printf("len1 = %d \n",len1);

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
float *pv, **ppv, **ppvfr;
float *puds, **ppuds, **ppudsfr;
float *pa,**ppa, **ppafr;
float *pds, **ppds, **ppdsfr;
//8 x 9 arrays
float *pvt, **ppvt, **ppvtfr;
//9 x 9 arrays
float *pudsvt, **ppudsvt, **ppudsvtfr;

len1 = sizeof(float *) * m + sizeof(float) * n * m;
len2 = sizeof(float *) * n + sizeof(float) * m * n;
len3 = sizeof(float *) * p + sizeof(float) * p * q;
printf("len = %d len2 = %d len3 = %d\n",len1, len2,len3);
ppv = (float **)malloc(len1);
ppvfr = ppv;
ppuds = (float **)malloc(len1);
ppudsfr = ppuds;
ppa = (float **)malloc(len1);
ppafr = ppa;
ppds = (float **)malloc(len1);
ppdsfr = ppds;
ppvt = (float **)malloc(len2);
ppvtfr = ppvt;
ppudsvt = (float **)malloc(len3);
ppudsvtfr = ppudsvt;
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


for(i=0;i<m;i++) {
	for(j=0;j<n;j++) {
		ppa[i][j]=(float)*inbuf;
		//printf("%d %d %5.1f \n",i,j,ppa[i][j]);
		inbuf++;
	}
}
	
  
printf("a row = %d col = %d \n",m,n);
//result = disp(ppa,m,n);
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
//result = disp(ppa,m,n);
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
//result = disp(ppds,m,n);	
for(i=0;i<m;i++) printf("%5.8f \n",w[i]);

printf("V row = %d col = %d \n",m,n);
//result = disp(ppv,m,n);

printf("V' row = %d col = %d \n",n,m);
 
result = trans(ppv,ppvt,m,n);
//result = disp(ppvt,n,m);
printf("Call mul u * s  \n");
result = mul(ppa,ppds,ppuds,m,n,p,q);
printf("UDS row = %d col = %d \n",m,n);
//result = disp(ppuds,m,n);
printf("Call mul u * ds * vt \n");

result = mul(ppuds,ppvt,ppudsvt,m,n,n,m);
printf("USDVT row = %d col = %d \n",p,q); 
 

//result = disp(ppudsvt,p,q);
free(inbuffr);
free(ppvfr);
free(ppudsfr);
free(ppafr);
free(ppdsfr);
free(ppvtfr);
free(ppudsvtfr);
}
