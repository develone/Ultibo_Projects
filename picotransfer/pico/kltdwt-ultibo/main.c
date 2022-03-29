/**
 * Copyright (c) 2020 Raspberry Pi (Trading) Ltd.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <stdio.h>
#include "pico/stdlib.h"
#include "lifting.h"
#include "crc.h"
#include "head-tail.h"
#include "klt.h"
#define DBUG 0
#define DBUG1 0
#define DBUG2 0
#define DBUG3 0
#define DBUG4 1

#define imgsize 4096
//#define imgsize 512
struct PTRs {
	/*This is the buffer for inp & output
	2048 x 2048 = 4194304
	256 x 256 = 65536
	64 x 64 = 4096
	*/
	short int inpbuf[imgsize*2];
	short int *inp_buf;
	short int *out_buf;
	short int flag;
	short int w;
	short int h;
	short int *fwd_inv;
	short int fwd;
	short int *red;
	char *head;
	char *tail;
	char *topofbuf;
	char *endofbuf;
} ptrs;



unsigned char tt[128];
const char src[] = "Hello, world! ";
const short int a[]; 

//const unsigned char CRC7_POLY = 0x91;
unsigned char CRCTable[256];
 

int read_tt(char * head, char * endofbuf,char * topofbuf) {

	int i,numtoread = 64;
	unsigned char CRC;
	 
	//printf("0x%x 0x%x 0x%x \n",ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
	for(i=0;i<numtoread;i++) {
		
		*ptrs.head = getchar();
	 	ptrs.head = (char *)bump_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
	}
	
	CRC = getCRC(tt,numtoread);
	printf("0x%x\n",CRC);
	//for(i=0;i<numtoread;i++) bump_tail(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
	//for(i=0;i<numtoread;i++) printf("%c",tt[i]);
	
	
	//printf("\n");

	 
	//printf("0x%x 0x%x 0x%x \n",ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
	//printf("CRC = 0x%x\n",CRC);
	
	return(1);
}
unsigned char userInput;

int main() {
	unsigned char recCRC;
	unsigned char message[3] = {0xd3, 0x01, 0x00};
	int flag = 0,numofchars,error=0,syncflag=1,rdyflag=1,testsx=10;
    float a,b,c;
    stdio_init_all();
    int i,j,l,index;
    ptrs.w = 64;
    ptrs.h = 64;
    KLT_FeatureList fl;
    int nFeatures = 100;
    
    int ncols, nrows;
    unsigned char inpbuf[imgsize*2]; 
    unsigned char *img1, *img2;
    
    KLT_TrackingContext tc;
    tc = KLTCreateTrackingContext();
    //printf("tc 0x%x\n",tc);
    fl = KLTCreateFeatureList(nFeatures);
    //KLTPrintTrackingContext(tc);
    ncols = 64;
    nrows = 64;
     
    ptrs.inp_buf = ptrs.inpbuf;   
    ptrs.head = &tt[0];
	ptrs.tail = &tt[0];
	ptrs.topofbuf = &tt[0];
	
	ptrs.out_buf = ptrs.inpbuf + imgsize;
	ptrs.endofbuf = &tt[128];
	sleep_ms(2000);
	//printf("setting pointers\n");
	//printf("ptrs.inp_buf = 0x%x ptrs.out_buf = 0x%x\n",ptrs.inpbuf, ptrs.out_buf);
	img1 = &inpbuf[0];
        img2 = &inpbuf[4096];
	//printf("ncols & nrows and img1 were set by pgmReadHeaderFile\n"); 
       // printf("img1 = 0x%x img2 = 0x%x\n",img1, img2);
	//printf("head 0x%x tail 0x%x end 0x%x top 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
	
	ptrs.fwd_inv =  &ptrs.fwd;
    *ptrs.fwd_inv = 1;
    
    buildCRCTable();
	message[2] = getCRC(message, 2);
	//const uint SERIAL_BAUD = 1000000;
    a = 100.0;
    b = 0.33333;
    c = a /b;
    //printf("this is testing floating point needed for the KLT %5.5f %5.5f %5.5f \n",a,b,c);
    c = a * b;
    //printf("this is testing floating point needed for the KLT %5.5f %5.5f %5.5f \n",a,b,c);
    //printf("ncols %d nrows %d\n",ncols,nrows);

    //printf("tc 0x%x fl 0x%x\n",tc,fl);
		
    while (true) {
        if (DBUG == 1 ) {
            printf("Hello, world!\n");
            printf("Now copmpiles with lifting code as part of hello_usb.c\n"); 
            printf("structure PTRS added to hello_usb.c\n");
            printf("ptrs.w = %d ptrs.h = %d \n", ptrs.w, ptrs.h);
            printf("These are the variables needed for lifting\n");
            printf("ptrs.inp_buf = 0x%x ptrs.out_buf = 0x%x\n",ptrs.inp_buf, ptrs.out_buf);
            
            printf("w = %d ptrs.fwd_inv = 0x%x ptrs.fwd_inv = %d\n",ptrs.w,ptrs.fwd_inv, *ptrs.fwd_inv);
            printf("head = 0x%x tail = 0x%x end = 0x%x top = 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 
            //for(int i=0;i<25;i++) printf("%d ",a[i]);
            //printf("\n");
            printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 
            ptrs.head = (char *)bump_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)bump_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.head = (char *)bump_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)bump_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.head = (char *)bump_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)bump_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
			printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 
            ptrs.head = (char *)dec_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)dec_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.head = (char *)dec_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)dec_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.head = (char *)dec_head(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);
            ptrs.tail = (char *)dec_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
            
            printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 

        } 
        if (DBUG1 == 1) {
			printf("Command (1 = Send or 0 = Wait):\n");
			userInput = getchar();
			
			//for(i = 0; i < imgsize;i++) ptrs.inp_buf[i] = a[i];
			 
			//lifting(ptrs.w,ptrs.inp_buf,ptrs.out_buf,ptrs.fwd_inv);
			
			if(userInput == '1'){
				for(i=0;i<imgsize;i++) printf("%d ",ptrs.inp_buf[i]);
				index = 0;
				for(j=0;j<64;j++) {
					//for(l=0;l<4;l++) {
					//printf("%d\n",l);
					for(i=0;i<64;i++) {
						printf("%d,",ptrs.inp_buf[index]);
						//printf("%d %d %d\n",i,index,index++);
						index++;
					}
					//index = index + 64;
					printf("\n");
					//}
				}
			}
			
		}
		if (DBUG2 == 1) {
			for (i = 0; i < sizeof(message); i++)
			{
				for (j = 0; j < 8; j++)
				printf("%d", (message[i] >> j) % 2);
				printf(" ");
			}
			printf("\n");
			printf("0x%x 0x%x 0x%x 0x%x\n",*ptrs.head,*ptrs.tail,*ptrs.endofbuf,*ptrs.topofbuf);	
		}
	//printf("read 16 values\n");
	

			//printf("\n");

	
		if (DBUG3 == 1) {
			//printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 
			read_tt(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);	
			//printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
			for(i=0;i<32;i++) printf("%d ",tt[i]);
			printf("\n");
			for(i=32;i<64;i++) printf("%d ",tt[i]);
			printf("\n");
			
			//numofchars = ptrs.head -ptrs.tail;
			//printf("%d ", numofchars);
			//printf("0x%x 0x%x 0x%x 0x%x 0x%x \n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf,ptrs.inp_buf);
			for(i=0;i<64;i++) {
				*ptrs.inp_buf = (unsigned short int)*ptrs.tail;
				ptrs.inp_buf++;
				ptrs.tail = (char *)bump_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
			}
			recCRC = getchar();
			printf("recCRC 0x%x ",recCRC);
			
			printf("0x%x 0x%x 0x%x 0x%x 0x%x \n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf,ptrs.inp_buf);
			
		}
		
		if (DBUG4 == 1) {
			/* Reads 4096 values*/
			while (testsx) {
					printf("Sync\n");
          sleep_ms(2000);
					testsx--;
        }
				testsx=10;
				while (testsx) {
					printf("Ready\n");
          sleep_ms(2000);
					testsx--;

        }
				//userInput = getchar();
			while (ptrs.inp_buf < ptrs.out_buf) {
        
			 
			
				
				//printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf); 
				read_tt(ptrs.head,ptrs.endofbuf,ptrs.topofbuf);	
				//printf("head = 0x%x tail = 0x%x 0x%x 0x%x\n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
				for(i=0;i<32;i++) printf("%d ",tt[i]);
				printf("\n");
				for(i=32;i<64;i++) printf("%d ",tt[i]);
				printf("\n");
			
				//numofchars = ptrs.head -ptrs.tail;
				//printf("%d ", numofchars);
				//printf("0x%x 0x%x 0x%x 0x%x 0x%x \n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf,ptrs.inp_buf);
				for(i=0;i<64;i++) {
					*ptrs.inp_buf = (unsigned short int)*ptrs.tail;
					ptrs.inp_buf++;
					ptrs.tail = (char *)bump_tail(ptrs.tail,ptrs.endofbuf,ptrs.topofbuf);
				}
				recCRC = getchar();
				printf("recCRC 0x%x ",recCRC);
			
				printf("0x%x 0x%x 0x%x 0x%x 0x%x \n",ptrs.head,ptrs.tail,ptrs.endofbuf,ptrs.topofbuf,ptrs.inp_buf);
			}
			ptrs.inp_buf = ptrs.inpbuf;
			/*
			for(i = 0; i < imgsize;i++) {
				if (ptrs.inp_buf[i] == a[i]) {
					//printf("matched %d \n",i);
					error = 0;
				} else {
					//printf("error %d\n",i);
					error = 1;
				}
				
			}
			printf("errors %d \n",error);
 
			*/
			printf("Command (1 = Send or 0 = Wait):\n");
			userInput = getchar();
			 
			
			if(userInput == '1'){
			printf("need to copy the data received from host to img1\n");
			printf("img1 = 0x%x img2 = 0x%x\n",img1, img2);
			for(i = 0; i < ncols*nrows;i++) {
			      img1[i] = ptrs.inp_buf[i];
			      //img2[i+4095] = img1[i];	
			      if (i < 5) printf("%d img1 %d ptrs.buf %d \n",i, img1[i],ptrs.inp_buf[i]); 
			      if (i > 4090) printf("%d img1 %d ptrs.buf %d \n",i,img1[i],ptrs.inp_buf[i]);	
			}
			printf("need to copy the data from img1 to img2\n");
			for(i = 0; i < ncols*nrows;i++) {
				*img2 = *img1;
				if (i < 5) printf("%d img2 %d img1 %d \n",i,*img2,*img1);
                                if (i > 4090) printf("%d img2 %d img1 %d \n",i,*img2,*img1);
                                img2++;
                                img1++; 
			} 
			img1 = &inpbuf[0];
        		img2 = &inpbuf[4096];
			printf("img1 = 0x%x img2 = 0x%x\n",img1, img2);

	KLTSelectGoodFeatures(tc, img1, ncols, nrows, fl);

  	printf("\nIn first image:\n");
  	for (i = 0 ; i < fl->nFeatures ; i++)  {
    		printf("Feature #%d:  (%f,%f) with value of %d\n",
           	i, fl->feature[i]->x, fl->feature[i]->y,
           	fl->feature[i]->val);
        }
				lifting(ptrs.w,ptrs.inp_buf,ptrs.out_buf,ptrs.fwd_inv);
				printf("liftting done \n");
				
				//for(i=0;i<imgsize;i++) printf("%d ",ptrs.inp_buf[i]);
				index = 0;
				for(j=0;j<64;j++) {
					//for(l=0;l<4;l++) {
					//printf("%d\n",l);
					for(i=0;i<64;i++) {
						printf("%d,",ptrs.inp_buf[index]);
						//printf("%d %d %d\n",i,index,index++);
						index++;
					}
					//index = index + 64;
					printf("\n");
					//}
				}
			} 
		}
        syncflag=1;
				//sleep_ms(8000);
        sleep_ms(50);
    }
    return 0;
}
