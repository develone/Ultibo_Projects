#include "filter.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define pi 3.14159265

static int gensindata(double *ptrsignal)
{
 int i,L=2048;
 float t[L],freq=1000,T; 
 //double sample[L];
 printf("0x%x \n",ptrsignal);
 T=1/freq;
 //T=.000125;
 //t = (0:L-1)*T;
 for(i=0;i<L;i++) t[i] = i*T;
 
    for (i=0;i<(L-1);i++)
    {
		 
       //sample = 1000*sin(2*pi*300*t[i])+500;
       //sample = 10*sin(2*pi*460*t[i]);
       //sample[i]= 10*(sin(2*pi*50*t[i]) + sin(2*pi*120*t[i]) + sin(2*pi*300*t[i])); //no DC
       //sample = 10*sin(2*pi*120*t[i]);  //no DC
       *ptrsignal = (double)10*(sin(2*pi*50*t[i]) + sin(2*pi*120*t[i]) + sin(2*pi*300*t[i])); //no DC
       ptrsignal++;
       //cin[i].i = 0;
       //printf("%f,",cin[i].r);  
	//printf("%f, ",sample[i]);
    }
     
    return 0;
 }

void band_low_example()
{
    int i,result;
    FILE *wrptr;
  
    double *ptrsignal, signal[2048],myfilt[2048];
    ptrsignal = &signal[0];
    printf("0x%x \n",ptrsignal);
    gensindata(ptrsignal);
    //for (i=0;i<2047;i++) printf("%f,",signal[i]);
    wrptr = fopen("mysig.bin","w");
    if (wrptr == 0) printf("can not open file S.bin for writing\n");
    result = fwrite(&signal[0],sizeof(double),2048,wrptr); 
    fclose(wrptr);
    //BWBandPass* filter = create_bw_band_pass_filter(4, 250, 2, 45);
    BWLowPass* filter = create_bw_low_pass_filter(4,250,45);
    printf("\n");
    for( i = 0; i < 2048; i++){
        //printf("%f,", bw_low_pass(filter, signal[i]));
        myfilt[i] = bw_low_pass(filter, signal[i]);
    }
    //printf("\n");
    wrptr = fopen("myfilt.bin","w");
    if (wrptr == 0) printf("can not open file S.bin for writing\n");
    result = fwrite(&myfilt[0],sizeof(double),2048,wrptr); 
    fclose(wrptr);
    
    free_bw_low_pass(filter);

}

int test() {   
    printf("========= Band low filter example =========\n\n");
    band_low_example();
    printf("========= Done. =========\n\n");
    return 0;
}
