/*
 *  Copyright (c) 2003-2004, Mark Borgerding. All rights reserved.
 *  This file is part of KISS FFT - https://github.com/mborgerding/kissfft
 *
 *  SPDX-License-Identifier: BSD-3-Clause
 *  See COPYING file for more information.
 */

#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>

#include "kiss_fft.h"
#include "kiss_fftndr.h"
//#include "pstats.h"


//#define CLOCK_MONOTONIC (clockid_t)4
unsigned Microseconds(void);

unsigned Microseconds(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec*1000000 + ts.tv_nsec/1000;
}

static void complex_abs(kiss_fft_cpx *cout, int n) {
	while (n-- > 0) {
		cout->r = sqrt(cout->r * cout->r + cout->i * cout->i);
		cout->i = 0;
		cout++;
	}
}

static
void fft_file(FILE * fin,FILE * fout,int nfft,int isinverse)
{
    kiss_fft_cfg st;
    kiss_fft_cpx * buf;
    kiss_fft_cpx * bufout;

    buf = (kiss_fft_cpx*)malloc(sizeof(kiss_fft_cpx) * nfft );
    bufout = (kiss_fft_cpx*)malloc(sizeof(kiss_fft_cpx) * nfft );
    st = kiss_fft_alloc( nfft ,isinverse ,0,0);

    while ( fread( buf , sizeof(kiss_fft_cpx) * nfft ,1, fin ) > 0 ) {
        kiss_fft( st , buf ,bufout);
        fwrite( bufout , sizeof(kiss_fft_cpx) , nfft , fout );
    }
    /*
    complex_abs(bufout,nfft);
    int j;
    printf("%d %d %d\n",nfft,isinverse,nfft/2+1);
         
    for(j=0;j<nfft;j++) {
         
        printf("%d %f %f \n",j,bufout[j].r,bufout[j].i);
    }
    */
    free(st);
    free(buf);
    free(bufout);
}

static
void fft_filend(FILE * fin,FILE * fout,int *dims,int ndims,int isinverse)
{
    kiss_fftnd_cfg st;
    kiss_fft_cpx *buf;
    int dimprod=1,i;
    for (i=0;i<ndims;++i) 
        dimprod *= dims[i];

    buf = (kiss_fft_cpx *) malloc (sizeof (kiss_fft_cpx) * dimprod);
    st = kiss_fftnd_alloc (dims, ndims, isinverse, 0, 0);

    while (fread (buf, sizeof (kiss_fft_cpx) * dimprod, 1, fin) > 0) {
        kiss_fftnd (st, buf, buf);
        fwrite (buf, sizeof (kiss_fft_cpx), dimprod, fout);
    }
    free (st);
    free (buf);
}



static
void fft_filend_real(FILE * fin,FILE * fout,int *dims,int ndims,int isinverse)
{
    int dimprod=1,i;
    kiss_fftndr_cfg st;
    void *ibuf;
    void *obuf;
    int insize,outsize; // size in bytes

    for (i=0;i<ndims;++i) 
        dimprod *= dims[i];
    insize = outsize = dimprod;
    int rdim = dims[ndims-1];

    if (isinverse)
        insize = insize*2*(rdim/2+1)/rdim;
    else
        outsize = outsize*2*(rdim/2+1)/rdim;

    ibuf = malloc(insize*sizeof(kiss_fft_scalar));
    obuf = malloc(outsize*sizeof(kiss_fft_scalar));

    st = kiss_fftndr_alloc(dims, ndims, isinverse, 0, 0);

    while ( fread (ibuf, sizeof(kiss_fft_scalar), insize,  fin) > 0) {
        if (isinverse) {
            kiss_fftndri(st,
                    (kiss_fft_cpx*)ibuf,
                    (kiss_fft_scalar*)obuf);
        }else{
            kiss_fftndr(st,
                    (kiss_fft_scalar*)ibuf,
                    (kiss_fft_cpx*)obuf);
        }
        fwrite (obuf, sizeof(kiss_fft_scalar), outsize,fout);
    }
    free(st);
    free(ibuf);
    free(obuf);
}

static
void fft_file_real(FILE * fin,FILE * fout,int nfft,int isinverse,int wrflag)
{
    kiss_fftr_cfg st;
    kiss_fft_scalar * rbuf;
    kiss_fft_cpx * cbuf;
    int result;
    //printf("wrflag = %d \n",wrflag);
    rbuf = (kiss_fft_scalar*)malloc(sizeof(kiss_fft_scalar) * nfft );
    cbuf = (kiss_fft_cpx*)malloc(sizeof(kiss_fft_cpx) * (nfft/2+1) );
    st = kiss_fftr_alloc( nfft ,isinverse ,0,0);

    if (isinverse==0) {
        while ( fread( rbuf , sizeof(kiss_fft_scalar) * nfft ,1, fin ) > 0 ) {
            kiss_fftr( st , rbuf ,cbuf);
            if(wrflag==1) { 
                result = fwrite( cbuf , sizeof(kiss_fft_cpx) , (nfft/2 + 1) , fout );
                printf("result = %d \n",result);
            }
            
        }
    }else{
        while ( fread( cbuf , sizeof(kiss_fft_cpx) * (nfft/2+1) ,1, fin ) > 0 ) {
            kiss_fftri( st , cbuf ,rbuf);
            if(wrflag==1) {
                result = fwrite( rbuf , sizeof(kiss_fft_scalar) , nfft , fout );
                printf("result = %d \n",result);
            }
        }
    }
    /*
    int j;
    printf("%d %d \n",nfft,isinverse);
    for(j=0;j<nfft;j++) {
        printf("%f\n",rbuf[j]);
    }
    
    
    
    complex_abs(cbuf,nfft/2+1);
    //int j;
    printf("%d %d %d\n",nfft,isinverse,nfft/2+1);
         
    for(j=0;j<nfft/2+1;j++) {
         
        printf("%d %f %f \n",j,cbuf[j].r,cbuf[j].i);
    }
    */
    free(st);
    free(rbuf);
    free(cbuf);
}

static
int get_dims(char * arg,int * dims)
{
    char *p0;
    int ndims=0;

    do{
        p0 = strchr(arg,',');
        if (p0)
            *p0++ = '\0';
        dims[ndims++] = atoi(arg);
//         fprintf(stderr,"dims[%d] = %d\n",ndims-1,dims[ndims-1]); 
        arg = p0;
    }while (p0);
    return ndims;
}

test()
{
    int isinverse=0,i,wrflag=0;
    int isreal=0;
    FILE *fin=stdin;
    FILE *fout=stdout;
    int ndims=1;
    int dims[32];
    dims[0] = 2048; /*default fft size*/
    unsigned t[2];
    
    
    //pstats_init();
    wrflag=0;
    t[0] = Microseconds();
    for (i = 0; i < 4999; i++) {
        fin = fopen("mysig.bin","rb");
        fout = fopen("myfft.bin","wb");
        fft_file_real(fin,fout,dims[0],isinverse,wrflag);
        if (fout!=stdout) fclose(fout);
        if (fin!=stdin) fclose(fin);

    }
    t[1] = Microseconds();
    printf("fft usec = %d \n",t[1]-t[0]);
    wrflag=1; 
    fin = fopen("mysig.bin","rb");
    fout = fopen("myfft.bin","wb");
    fft_file_real(fin,fout,dims[0],isinverse,wrflag);
    if (fout!=stdout) fclose(fout);
    if (fin!=stdin) fclose(fin);
    
     
    //printf("fft cpu time\n");
    //pstats_report();
 
    
    
    isinverse=1;
    isreal=1;
 
     
    //pstats_init();
    wrflag=0; 
    t[0] = Microseconds();
    
    for (i = 0; i < 4999; i++) {
        fin = fopen("myfft.bin","rb");
        fout = fopen("myfftinv.bin","wb");
        fft_file_real(fin,fout,dims[0],isinverse,wrflag);
        if (fout!=stdout) fclose(fout);
        if (fin!=stdin) fclose(fin);         
    }
    t[1] = Microseconds();
    printf("inv fft usec = %d \n",t[1]-t[0]);
    wrflag=1;
    fin = fopen("myfft.bin","rb");
    fout = fopen("myfftinv.bin","wb");
    fft_file_real(fin,fout,dims[0],isinverse,wrflag);
    if (fout!=stdout) fclose(fout);
    if (fin!=stdin) fclose(fin);         

    //printf("inverse fft cpu time\n");
    //pstats_report();
 
    
}
