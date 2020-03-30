#define FLOAT_TO_INT(x) ((x)>=0?(int)((x)+0.5):(int)((x)-0.5))
pthread_t th_id[2];

 


extern int dsvd(float **a, int m, int n, float *w, float **v);

extern int trans(float **a,float **b,int m,int n);
extern int disp(float **a,int m,int n);
extern int mul(float **a,float **b,float **c,int m,int n,int p,int q);	

extern void *hello(void *input);
extern void *mysvd(void *strptr);


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
 

struct args {
    char* name;
    int age;
};


struct FILEs {
	char *input_file;
	char *first_output;
	char *second_output;
	char *pgm1,*pgm2,*pgm3;
	int status;
	//status 0 nothing done
	//status 1 input_file read
	//status 2 svd done
	//status 3 Sx.bin written
	//status 4 reconst file written
	int num_bytes_rd;
	int mem_allocated;
	
	
	
};

struct th_var {
	float w[256], *pw;
	int m,n,i,j,p,q,result,len1,len2,len3,len4;
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

	int *ps, **pps, **ppsfr;
	char *inbuf, *inbuffr;
	char *inbufch, *inbufchfr;
	FILE *inptr,*outptr;
	unsigned char *img1,*img2,*img3;
	int ncols, nrows;

};

struct th_var th0;
struct th_var th1;
struct th_var th2;
