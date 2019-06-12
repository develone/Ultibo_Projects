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

extern int dsvd(float **a, int m, int n, float *w, float **v);

extern int trans(float **a,float **b,int m,int n);
extern int disp(float **a,int m,int n);
extern int mul(float **a,float **b,float **c,int m,int n,int p,int q);	

int m=9,n=8,i,j,p=9,q=9,result;
 
float w[m],*pw;
float v[m][n],*pv[m], **ppv;
float vt[n][m],*pvt[n], **ppvt;
float a[m][n],*pa[m],**ppa;
float ds[m][n], *pds[m], **ppds;
/*	
 *  a 9 x 8
 *  u 9 x 8
 *  v 9 x 8 
 *  ds 9 x 8 
 *  vt 8 x 9
 *  uds 9 x 8
 *  udsvt 9 X 9
*/
float uds[m][n], *puds[m], **ppuds;
float udsvt[p][q], *pudsvt[p], **ppudsvt;

	for(i=0;i<m;i++) {	
		pa[i]=&a[i][0];
		pv[i]=&v[i][0];
		pds[i]=&ds[i][0];
		puds[i]=&uds[i][0];
		//pusdvt[i]=&pusdvt[i][0];
		//printf("0x%x 0x%x\n",pa[i],pv[i]);
	}
	for(i=0;i<n;i++) {
		pvt[i] = &vt[i][0];
		
	}
	for(i=0;i<p;i++) {
		pudsvt[i] = &pudsvt[i][0];
		
	}
	
	
 
pw=(float *)&w;
ppv=(float **)&pv;
ppa=(float **)&pa;
ppvt=(float **)&pvt;
ppds=(float **)&pds;
ppuds=(float **)&puds;
ppudsvt=(float **)&pudsvt;

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

a[0][0]=1.0;
a[0][1]=2.0;
a[0][2]=1.0;
a[0][3]=22.0;
a[0][4]=11.0;
a[0][5]=22.0;
a[0][6]=11.0;
a[0][7]=11.0;

a[1][0]=3.0;
a[1][1]=4.0;
a[1][2]=2.0;
a[1][3]=41.0;
a[1][4]=21.0;
a[1][5]=41.0;
a[1][6]=21.0; 
a[1][7]=11.0;

a[2][0]=5.0;
a[2][1]=6.0;
a[2][2]=3.0;
a[2][3]=63.0;
a[2][4]=33.0;
a[2][5]=63.0;
a[2][6]=33.0;
a[2][7]=11.0;

a[3][0]=7.0;
a[3][1]=8.0;
a[3][2]=4.0;
a[3][3]=82.0;
a[3][4]=42.0;
a[3][5]=82.0;
a[3][6]=42.0;
a[3][7]=11.0;
 
a[4][0]=7.0;
a[4][1]=8.0;
a[4][2]=4.0;
a[4][3]=282.0;
a[4][4]=242.0; 
a[4][5]=282.0;
a[4][6]=242.0; 
a[4][7]=11.0;

a[5][0]=7.0;
a[5][1]=8.0;
a[5][2]=4.0;
a[5][3]=182.0;
a[5][4]=142.0;
a[5][5]=82.0;
a[5][6]=42.0;
a[5][7]=11.0;

a[6][0]=1.0;
a[6][1]=2.0;
a[6][2]=1.0;
a[6][3]=22.0;
a[6][4]=11.0;
a[6][5]=82.0;
a[6][6]=42.0;
a[6][7]=11.0;

a[7][0]=3.0;
a[7][1]=4.0;
a[7][2]=2.0;
a[7][3]=41.0;
a[7][4]=21.0;
a[7][5]=82.0;
a[7][6]=42.0; 
a[7][7]=11.0;

a[8][0]=5.0;
a[8][1]=6.0;
a[8][2]=3.0;
a[8][3]=63.0;
a[8][4]=33.0;
a[8][5]=82.0;
a[8][6]=42.0;
a[8][7]=11.0;
 
for(i=0;i<m;i++)
	{
	for(j=0;j<n;j++)
	{
		printf("%5.2f ",a[i][j]);
	}
	printf("\n");
}
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
printf("U\n");
result = disp(ppa,m,n);
printf("Singular Values\n");
for(i=0;i<m;i++) { 
	for(j=0;j<n;j++) {
		ds[i][j] = 0;
	}	
}	

j=0;
for(i=0;i<m;i++) { 
	ds[i][j] = w[i];
	j++;
}	
result = disp(ppds,m,n);	
//for(i=0;i<m;i++) printf("%5.2f \n",w[i]);

printf("V\n");
result = disp(ppv,m,n);

printf("V'\n");
 
result = trans(ppv,ppvt,m,n);
result = disp(ppvt,n,m);
printf("Call mul_mat \n");
result = mul(ppa,ppds,ppuds,m,n,p,q);
printf("UDS\n");
result = disp(ppuds,m,n);
printf("Call mul_mat \n");

//result = mul(ppuds,ppvt,ppudsvt,m,n,q,p);
//printf("USDVT\n");
for(i=0;i<m;i++) {
	for(j=0;j<p;j++) {
		udsvt[i][j]=0;
	}
}
/*
for(i=0;i<m;i++) {
	for(j=0;j<p;j++) {
		printf("%5.3f ",udsvt[i][j]);
	}
	printf("\n");
}
*/ 
//result = disp(ppudsvt,m,m);
}
