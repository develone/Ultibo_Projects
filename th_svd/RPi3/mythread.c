#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include<unistd.h>
#include "../include/master_slave.h"
#include "../include/pnmio.h"
#include "../include/klt.h"

void *hello(void *input) {
    printf("name: %s\n", ((struct args*)input)->name);
    printf("age: %d\n", ((struct args*)input)->age);
}

void *mysvd(void *strptr) {
	pthread_t id = pthread_self();
	int sw;
	if(pthread_equal(id,th_id[0])!=0) {
		sw = 0;
		
		th0.m = 512;
		th0.n = 512;
		th0.p = 512;
		th0.q = 512;
		
		th0.inbuf = (char *)malloc(sizeof(char)*th0.m*th0.n);
		th0.inbuffr = th0.inbuf;
		th0.inbufch = (char *)malloc(sizeof(char)*th0.m*th0.n);
		th0.inbufchfr = th0.inbufch;
	}
	if(pthread_equal(id,th_id[1])!=0) {
		sw = 1;
		
		th1.m = 512;
		th1.n = 512;
		th1.p = 512;
		th1.q = 512;
		
		th1.inbuf = (char *)malloc(sizeof(char)*th1.m*th1.n);
		th1.inbuffr = th1.inbuf;
		th1.inbufch = (char *)malloc(sizeof(char)*th1.m*th1.n);
		th1.inbufchfr = th1.inbufch;
	}
	if(pthread_equal(id,th_id[2])!=0) {
		sw = 2;
		
		th2.m = 512;
		th2.n = 512;
		th2.p = 512;
		th2.q = 512;
		
		th2.inbuf = (char *)malloc(sizeof(char)*th2.m*th2.n);
		th2.inbuffr = th2.inbuf;
		th2.inbufch = (char *)malloc(sizeof(char)*th2.m*th2.n);
		th2.inbufchfr = th2.inbufch;
	}
	switch(sw) {
		case 0:
			printf("\n 1st thread processing th_id[0] 0x%x\n",th_id[0]);
			printf("In mysvd input_file: %s\n", ((struct FILEs*)strptr)->input_file);
			printf("In mysvd first_output: %s\n", ((struct FILEs*)strptr)->first_output);
			printf("In mysvd second_output: %s\n", ((struct FILEs*)strptr)->second_output);
			printf("In mysvd status: %d\n", ((struct FILEs*)strptr)->status);
			printf("In mysvd num_bytes_rd: %d\n", ((struct FILEs*)strptr)->num_bytes_rd);
			printf("\n");
			//Now reading for the input image using pgmReadFile
			th0.img1 = pgmReadFile(((struct FILEs*)strptr)->input_file, NULL, &th0.ncols, &th0.nrows);
			printf("ncols=%d nrows=%d \n",th0.ncols,th0.nrows);
			/*
			th0.inptr = fopen(((struct FILEs*)strptr)->input_file,"r");
			if (th0.inptr == 0) printf("file not found\n");
			//th0.len1 = fread(th0.inbuf,sizeof(int),th0.m*th0.n,th0.inptr);
			th0.len1 = fread(th0.inbuf,sizeof(char),th0.m*th0.n,th0.inptr);
			fclose(th0.inptr);
			((struct FILEs*)strptr)->num_bytes_rd = th0.len1;
			*/
			((struct FILEs*)strptr)->num_bytes_rd = th0.ncols*th0.nrows; 
			((struct FILEs*)strptr)->status = 1;
			printf("In mysvd status input file read: %d num_bytes_rd %d\n", ((struct FILEs*)strptr)->status,((struct FILEs*)strptr)->num_bytes_rd);
			printf("red.pgm th0.len1 = %d \n",th0.len1);
			th0.len1 = sizeof(float *) * th0.m + sizeof(float) * th0.n * th0.m;
			th0.len2 = sizeof(float *) * th0.n + sizeof(float) * th0.m * th0.n;
			th0.len3 = sizeof(float *) * th0.p + sizeof(float) * th0.p * th0.q;
			th0.len4 = sizeof(int *) * th0.n + sizeof(int) * th0.m * th0.n;
			if(((struct FILEs*)strptr)->mem_allocated == 0) {
				printf("len = %d th0.len2 = %d th0.len3 = %d th0.len4 = %d\n",th0.len1, th0.len2,th0.len3,th0.len4);		
				printf("setting up ptrs with malloc\n");
				th0.ppv = (float **)malloc(th0.len1);
				th0.ppvfr = th0.ppv;
				th0.ppuds = (float **)malloc(th0.len1);
				th0.ppudsfr = th0.ppuds;
				th0.ppa = (float **)malloc(th0.len1);
				th0.ppafr = th0.ppa;
				th0.ppds = (float **)malloc(th0.len1);
				th0.ppdsfr = th0.ppds;
				th0.ppvt = (float **)malloc(th0.len2);
				th0.ppvtfr = th0.ppvt;				
				th0.pps = (int **)malloc(th0.len4);
				th0.ppsfr = th0.pps;
				th0.ppudsvt = (float **)malloc(th0.len3);
				th0.ppudsvtfr = th0.ppudsvt;
				((struct FILEs*)strptr)->mem_allocated = 1;
			} 
			else {
				th0.ppv = th0.ppvfr;
				th0.ppuds = th0.ppudsfr;
				th0.ppa = th0.ppafr;
				th0.ppds = th0.ppdsfr;
				th0.ppds = th0.ppdsfr;
				th0.ppudsvt = th0.ppudsvtfr;
			}
				
			// pv, puds, pa, pds, pvt, and pudsvt are now pointing to the first elements of 2D arrays 
			th0.pv = (float *)(th0.ppv + th0.m);
			th0.puds = (float *)(th0.ppuds + th0.m);
			th0.pa = (float *)(th0.ppa + th0.m);
			th0.pds = (float *)(th0.ppds + th0.m);
			th0.pvt = (float *)(th0.ppvt + th0.n);
			th0.pudsvt = (float *)(th0.ppudsvt + th0.p);
			th0.ps = (int *)(th0.pps + th0.m);
			printf("pa 0x%x ppa 0x%x  \n",th0.pa,th0.ppa);
			printf("pv = 0x%x ppv = 0x%x \n",th0.pv,th0.ppv);
			printf("pvt = 0x%x ppvt = 0x%x \n",th0.pvt,th0.ppvt);
			printf("pds = 0x%x ppds = 0x%x \n",th0.pds,th0.ppds);
			printf("puds = 0x%x ppuds = 0x%x \n",th0.puds,th0.ppuds);
			printf("pudsvt = 0x%x ppudsvt = 0x%x \n",th0.pudsvt,th0.ppudsvt);
	
			for(th0.i = 0; th0.i < th0.m; th0.i++) {
				th0.ppa[th0.i] = (th0.pa + th0.n * th0.i);
				th0.ppuds[th0.i] = (th0.puds + th0.n * th0.i);
				th0.ppv[th0.i] = (th0.pv + th0.n * th0.i);
				th0.ppds[th0.i] = (th0.pds + th0.n * th0.i);
	

			}	

			for(th0.i = 0; th0.i < th0.m; th0.i++) {
				th0.ppvt[th0.i] = (th0.pvt + th0.n * th0.i);
				th0.pps[th0.i] = (th0.ps + th0.n * th0.i);
			}
			for(th0.i = 0; th0.i < th0.m; th0.i++) th0.ppudsvt[th0.i] = (th0.pudsvt + th0.q * th0.i); 
	
			for(th0.j=0;th0.j<th0.m;th0.j++) {
				for(th0.i=0;th0.i<th0.n;th0.i++) {
					th0.ppa[th0.i][th0.j]=(float)*th0.img1;
					//*th0.inbufch=(char)*th0.inbuf;
					//printf("%d %d %5.1f \n",th0.i,th0.j,th0.ppa[th0.i][th0.j]);
					//printf("%d %d %d \n",th0.i,th0.j,*th0.inbufch);
					th0.img1++;
					//th0.inbufch++;
				}
			}
			//pgmWriteFile(((struct FILEs*)strptr)->pgm1,th0.inbufchfr,th0.m,th0.n);
			th0.pw=(float *)&th0.w;
			th0.result = dsvd(th0.ppa,th0.m,th0.n,th0.pw,th0.ppv);
			//for(th0.i=0;th0.i<th0.m;th0.i++) printf("%5.8f \n",th0.w[th0.i]);
			((struct FILEs*)strptr)->status = 2;
			th0.outptr = fopen(((struct FILEs*)strptr)->first_output,"w");
			if (th0.outptr == 0) printf("can not open file S.bin for writing\n");
			th0.result = fwrite(th0.pw,sizeof(float),th0.m,th0.outptr);
			fclose(th0.outptr);
			printf("U row = %d col = %d \n",th0.m,th0.n);
			//result = disp(th0.ppa,th0.m,th0.n);
			printf("Singular Values\n");
			//clear the S diagonal matrix
			for(th0.j=0;th0.j<th0.m;th0.j++) { 
				for(th0.i=0;th0.i<th0.n;th0.i++) {
					th0.ppds[th0.i][th0.j] = 0;
				}	
			}	
			//create the S diagonal matrix from w
			th0.j=0;
			for(th0.i=0;th0.i<th0.m;th0.i++) { 
				if (th0.i <= 63) th0.ppds[th0.i][th0.j] = th0.w[th0.i];
				th0.j++;
			}
			
			printf("V row = %d col = %d \n",th0.m,th0.n);
			//th0.result = disp(th0.ppv,th0.m,th0.n);

			printf("V' row = %d col = %d \n",th0.m,th0.n);
 
			th0.result = trans(th0.ppv,th0.ppvt,th0.m,th0.n);
			//result = disp(ppvt,n,m);
			printf("Call mul u * s  \n");
			th0.result = mul(th0.ppa,th0.ppds,th0.ppuds,th0.m,th0.n,th0.p,th0.q);
			printf("UDS row = %d col = %d \n",th0.m,th0.n);
			//th0.result = disp(th0.ppuds,th0.m,th0.n);
			printf("Call mul u * ds * vt \n");

			th0.result = mul(th0.ppuds,th0.ppvt,th0.ppudsvt,th0.m,th0.n,th0.m,th0.n);
			printf("USDVT row = %d col = %d \n",th0.p,th0.q);
			for(th0.j=0;th0.j<th0.m;th0.j++) {
				for(th0.i=0;th0.i<th0.n;th0.i++) {
					//th0.pps[th0.i][th0.j]=(int)th0.ppudsvt[th0.i][th0.j];
					th0.pps[th0.i][th0.j]=FLOAT_TO_INT(th0.ppudsvt[th0.i][th0.j]);
					//printf("%d ",pps[i][j]);
		
				}
			}
			th0.outptr = fopen(((struct FILEs*)strptr)->second_output,"w"); 
			printf("ps converted from float to int 0x%x \n",th0.outptr);
			
			if (th0.outptr == 0) printf("can not open file reconst.bin for writing\n");
			th0.result = fwrite(th0.ps,sizeof(int),th0.m*th0.n,th0.outptr);
			fclose(th0.outptr);
			((struct FILEs*)strptr)->status = 4;
			printf("# of data written 0x%x \n",th0.result);
			//Cleaning up 
			free(th0.inbuffr);
			free(th0.ppvfr);
			free(th0.ppudsfr);
			free(th0.ppafr);
			free(th0.ppvtfr);
			free(th0.ppdsfr);
			free(th0.ppudsvtfr);
			free(th0.ppsfr);
			
			break;
		case 1:
			printf("\n 2nd thread processing th_id[1] 0x%x\n",th_id[1]);
			printf("In mysvd input_file: %s\n", ((struct FILEs*)strptr)->input_file);
			printf("In mysvd first_output: %s\n", ((struct FILEs*)strptr)->first_output);
			printf("In mysvd second_output: %s\n", ((struct FILEs*)strptr)->second_output);
			printf("In mysvd status: %d\n", ((struct FILEs*)strptr)->status);
			printf("In mysvd num_bytes_rd: %d\n", ((struct FILEs*)strptr)->num_bytes_rd);
			printf("\n");
			//Now reading for the input image using pgmReadFile
			th1.img2 = pgmReadFile(((struct FILEs*)strptr)->input_file, NULL, &th1.ncols, &th1.nrows);
			printf("ncols=%d nrows=%d \n",th1.ncols,th1.nrows);
			/*
			th1.inptr = fopen(((struct FILEs*)strptr)->input_file,"r");
			if (th1.inptr == 0) printf("file not found\n");
			//th1.len1 = fread(th0.inbuf,sizeof(int),th1.m*th1.n,th1.inptr);
			th0.len1 = fread(th0.inbuf,sizeof(char),th0.m*th1.n,th1.inptr);
			fclose(th0.inptr);
			((struct FILEs*)strptr)->num_bytes_rd = th1.len1;
			*/
			((struct FILEs*)strptr)->num_bytes_rd = th1.ncols*th1.nrows; 
			((struct FILEs*)strptr)->status = 1;
			printf("In mysvd status input file read: %d num_bytes_rd %d\n", ((struct FILEs*)strptr)->status,((struct FILEs*)strptr)->num_bytes_rd);
			printf("grn.pgm th1.len1 = %d \n",th1.len1);
			th1.len1 = sizeof(float *) * th1.m + sizeof(float) * th1.n * th1.m;
			th1.len2 = sizeof(float *) * th1.n + sizeof(float) * th1.m * th1.n;
			th1.len3 = sizeof(float *) * th1.p + sizeof(float) * th1.p * th1.q;
			th1.len4 = sizeof(int *) * th1.n + sizeof(int) * th1.m * th1.n;
			printf("len = %d th1.len2 = %d th1.len3 = %d th1.len4 = %d\n",th1.len1, th1.len2,th1.len3,th1.len4);		
			if(((struct FILEs*)strptr)->mem_allocated == 0) {
				printf("len = %d th1.len2 = %d th1.len3 = %d th1.len4 = %d\n",th1.len1, th1.len2,th1.len3,th1.len4);		
				printf("setting up ptrs with malloc\n");
				th1.ppv = (float **)malloc(th1.len1);
				th1.ppvfr = th1.ppv;
				th1.ppuds = (float **)malloc(th1.len1);
				th1.ppudsfr = th1.ppuds;
				th1.ppa = (float **)malloc(th1.len1);
				th1.ppafr = th1.ppa;
				th1.ppds = (float **)malloc(th1.len1);
				th1.ppdsfr = th1.ppds;
				th1.ppvt = (float **)malloc(th1.len2);
				th1.ppdsfr = th1.ppvt;
				th1.pps = (int **)malloc(th1.len4);
				th1.ppsfr = th1.pps;
				th1.ppudsvt = (float **)malloc(th1.len3);
				th1.ppudsvtfr = th1.ppudsvt;
				((struct FILEs*)strptr)->mem_allocated = 1;
			} 
			else {
				th1.ppv = th1.ppvfr;
				th1.ppuds = th1.ppudsfr;
				th1.ppa = th1.ppafr;
				th1.ppds = th1.ppdsfr;
				th1.ppds = th1.ppdsfr;
				th1.ppudsvt = th1.ppudsvtfr;
			}

			// pv, puds, pa, pds, pvt, and pudsvt are now pointing to the first elements of 2D arrays 
			th1.pv = (float *)(th1.ppv + th1.m);
			th1.puds = (float *)(th1.ppuds + th1.m);
			th1.pa = (float *)(th1.ppa + th1.m);
			th1.pds = (float *)(th1.ppds + th1.m);
			th1.pvt = (float *)(th1.ppvt + th1.n);
			th1.pudsvt = (float *)(th1.ppudsvt + th1.p);
			th1.ps = (int *)(th1.pps + th1.m);
			printf("pa 0x%x ppa 0x%x  \n",th1.pa,th1.ppa);
			printf("pv = 0x%x ppv = 0x%x \n",th1.pv,th1.ppv);
			printf("pvt = 0x%x ppvt = 0x%x \n",th1.pvt,th1.ppvt);
			printf("pds = 0x%x ppds = 0x%x \n",th1.pds,th1.ppds);
			printf("puds = 0x%x ppuds = 0x%x \n",th1.puds,th1.ppuds);
			printf("pudsvt = 0x%x ppudsvt = 0x%x \n",th1.pudsvt,th1.ppudsvt);
	
			for(th1.i = 0; th1.i < th1.m; th1.i++) {
				th1.ppa[th1.i] = (th1.pa + th1.n * th1.i);
				th1.ppuds[th1.i] = (th1.puds + th1.n * th1.i);
				th1.ppv[th1.i] = (th1.pv + th1.n * th1.i);
				th1.ppds[th1.i] = (th1.pds + th1.n * th1.i);
	

			}	

			for(th1.i = 0; th1.i < th1.m; th1.i++) {
				th1.ppvt[th1.i] = (th1.pvt + th1.n * th1.i);
				th1.pps[th1.i] = (th1.ps + th1.n * th1.i);
			}
			for(th1.i = 0; th1.i < th1.m; th1.i++) th1.ppudsvt[th1.i] = (th1.pudsvt + th1.q * th1.i); 
	
			for(th1.j=0;th1.j<th1.m;th1.j++) {
				for(th1.i=0;th1.i<th1.n;th1.i++) {
					th1.ppa[th1.i][th1.j]=(float)*th1.img2;
					//*th0.inbufch=(char)*th0.inbuf;
					//printf("%d %d %5.1f \n",th1.i,th1.j,th1.ppa[th1.i][th1.j]);
					//printf("%d %d %d \n",th1.i,th1.j,*th1.inbufch);
					th1.img2++;
					//th0.inbufch++;
				}
			}
			//pgmWriteFile(((struct FILEs*)strptr)->pgm2,th1.inbufchfr,th1.m,th1.n);
			th1.pw=(float *)&th1.w;
			th1.result = dsvd(th1.ppa,th1.m,th1.n,th1.pw,th1.ppv);
			//for(th1.i=0;th1.i<th1.m;th1.i++) printf("%5.8f \n",th1.w[th1.i]);
			((struct FILEs*)strptr)->status = 2;
			th1.outptr = fopen(((struct FILEs*)strptr)->first_output,"w");
			if (th1.outptr == 0) printf("can not open file S.bin for writing\n");
			th1.result = fwrite(th1.pw,sizeof(float),th1.m,th1.outptr);
			fclose(th1.outptr);
			printf("U row = %d col = %d \n",th1.m,th1.n);
			//result = disp(th1.ppa,th1.m,th1.n);
			printf("Singular Values\n");
			//clear the S diagonal matrix
			for(th1.j=0;th1.j<th1.m;th1.j++) { 
				for(th1.i=0;th1.i<th1.n;th1.i++) {
					th1.ppds[th1.i][th1.j] = 0;
				}	
			}	
			//create the S diagonal matrix from w
			th1.j=0;
			for(th1.i=0;th1.i<th1.m;th1.i++) { 
				if (th1.i <= 128) th1.ppds[th1.i][th1.j] = th1.w[th1.i];
				th1.j++;
			}
			
			printf("V row = %d col = %d \n",th1.m,th1.n);
			//th1.result = disp(th1.ppv,th1.m,th1.n);

			printf("V' row = %d col = %d \n",th1.m,th1.n);
 
			th1.result = trans(th1.ppv,th1.ppvt,th1.m,th1.n);
			//result = disp(ppvt,n,m);
			printf("Call mul u * s  \n");
			th1.result = mul(th1.ppa,th1.ppds,th1.ppuds,th1.m,th1.n,th1.p,th1.q);
			printf("UDS row = %d col = %d \n",th1.m,th1.n);
			//th1.result = disp(th1.ppuds,th1.m,th1.n);
			printf("Call mul u * ds * vt \n");

			th1.result = mul(th1.ppuds,th1.ppvt,th1.ppudsvt,th1.m,th1.n,th1.m,th1.n);
			printf("USDVT row = %d col = %d \n",th1.p,th1.q);
			for(th1.j=0;th1.j<th1.m;th1.j++) {
				for(th1.i=0;th1.i<th1.n;th1.i++) {
					//th1.pps[th1.i][th1.j]=(int)th1.ppudsvt[th1.i][th1.j];
					th1.pps[th1.i][th1.j]=FLOAT_TO_INT(th1.ppudsvt[th1.i][th1.j]);
					//printf("%d ",th1.pps[th1.i][th1.j]);
		
				}
			}
			th1.outptr = fopen(((struct FILEs*)strptr)->second_output,"w"); 
			printf("ps converted from float to int 0x%x \n",th1.outptr);
			
			if (th1.outptr == 0) printf("can not open file reconst.bin for writing\n");
			th1.result = fwrite(th1.ps,sizeof(int),th1.m*th1.n,th1.outptr);
			fclose(th1.outptr);
			printf("# of data written 0x%x \n",th1.result);
			((struct FILEs*)strptr)->status = 4;
			//Cleaning up 
			/*free(th1.inbuffr);*/
			free(th1.ppvfr);
			free(th1.ppudsfr);
			free(th1.ppafr);
			free(th1.ppvtfr);
			free(th1.ppdsfr);
			free(th1.ppudsvtfr);
			free(th1.ppsfr);
			
			break;		
			case 2:
			printf("\n 3rd thread processing th_id[2] 0x%x\n",th_id[2]);
			printf("In mysvd input_file: %s\n", ((struct FILEs*)strptr)->input_file);
			printf("In mysvd first_output: %s\n", ((struct FILEs*)strptr)->first_output);
			printf("In mysvd second_output: %s\n", ((struct FILEs*)strptr)->second_output);
			printf("In mysvd status: %d\n", ((struct FILEs*)strptr)->status);
			printf("In mysvd num_bytes_rd: %d\n", ((struct FILEs*)strptr)->num_bytes_rd);
			printf("\n");
			//Now reading for the input image using pgmReadFile
			th2.img3 = pgmReadFile(((struct FILEs*)strptr)->input_file, NULL, &th2.ncols, &th2.nrows);
			printf("ncols=%d nrows=%d \n",th2.ncols,th2.nrows);
			/*
			th2.inptr = fopen(((struct FILEs*)strptr)->input_file,"r");
			if (th2.inptr == 0) printf("file not found\n");
			//th2.len1 = fread(th2.inbuf,sizeof(int),th0.m*th2.n,th2.inptr);
			th2.len1 = fread(th0.inbuf,sizeof(char),th2.m*th0.n,th2.inptr);
			fclose(th2.inptr);
			((struct FILEs*)strptr)->num_bytes_rd = th2.len1;
			*/
			((struct FILEs*)strptr)->num_bytes_rd = th2.ncols*th2.nrows; 
			((struct FILEs*)strptr)->status = 1;
			printf("In mysvd status input file read: %d num_bytes_rd %d\n", ((struct FILEs*)strptr)->status,((struct FILEs*)strptr)->num_bytes_rd);
			printf("blu.pgm th2.len1 = %d \n",th2.len1);
			th2.len1 = sizeof(float *) * th2.m + sizeof(float) * th2.n * th2.m;
			th2.len2 = sizeof(float *) * th2.n + sizeof(float) * th2.m * th2.n;
			th2.len3 = sizeof(float *) * th2.p + sizeof(float) * th2.p * th2.q;
			th2.len4 = sizeof(int *) * th2.n + sizeof(int) * th2.m * th2.n;
			printf("len = %d th2.len2 = %d th2.len3 = %d th2.len4 = %d\n",th2.len1, th2.len2,th2.len3,th2.len4);		
						if(((struct FILEs*)strptr)->mem_allocated == 0) {
				printf("len = %d th2.len2 = %d th2.len3 = %d th2.len4 = %d\n",th2.len1, th2.len2,th2.len3,th2.len4);		
				printf("setting up ptrs with malloc\n");
				th2.ppv = (float **)malloc(th2.len1);
				th2.ppvfr = th2.ppv;
				th2.ppuds = (float **)malloc(th2.len1);
				th2.ppudsfr = th2.ppuds;
				th2.ppa = (float **)malloc(th2.len1);
				th2.ppafr = th2.ppa;
				th2.ppds = (float **)malloc(th2.len1);
				th2.ppdsfr = th2.ppds;
				th2.ppvt = (float **)malloc(th2.len2);
				th2.ppdsfr = th2.ppvt;
				th2.pps = (int **)malloc(th2.len4);
				th2.ppsfr = th2.pps;
				th2.ppudsvt = (float **)malloc(th2.len3);
				th2.ppudsvtfr = th2.ppudsvt;
				((struct FILEs*)strptr)->mem_allocated = 1;
			} 
			else {
				th2.ppv = th2.ppvfr;
				th2.ppuds = th2.ppudsfr;
				th2.ppa = th2.ppafr;
				th2.ppds = th2.ppdsfr;
				th2.ppds = th2.ppdsfr;
				th2.ppudsvt = th2.ppudsvtfr;
			}

			// pv, puds, pa, pds, pvt, and pudsvt are now pointing to the first elements of 2D arrays 
			th2.pv = (float *)(th2.ppv + th2.m);
			th2.puds = (float *)(th2.ppuds + th2.m);
			th2.pa = (float *)(th2.ppa + th2.m);
			th2.pds = (float *)(th2.ppds + th2.m);
			th2.pvt = (float *)(th2.ppvt + th2.n);
			th2.pudsvt = (float *)(th2.ppudsvt + th2.p);
			th2.ps = (int *)(th2.pps + th2.m);
			printf("pa 0x%x ppa 0x%x  \n",th2.pa,th2.ppa);
			printf("pv = 0x%x ppv = 0x%x \n",th2.pv,th2.ppv);
			printf("pvt = 0x%x ppvt = 0x%x \n",th2.pvt,th2.ppvt);
			printf("pds = 0x%x ppds = 0x%x \n",th2.pds,th2.ppds);
			printf("puds = 0x%x ppuds = 0x%x \n",th2.puds,th2.ppuds);
			printf("pudsvt = 0x%x ppudsvt = 0x%x \n",th2.pudsvt,th2.ppudsvt);
	
			for(th2.i = 0; th2.i < th2.m; th2.i++) {
				th2.ppa[th2.i] = (th2.pa + th2.n * th2.i);
				th2.ppuds[th2.i] = (th2.puds + th2.n * th2.i);
				th2.ppv[th2.i] = (th2.pv + th2.n * th2.i);
				th2.ppds[th2.i] = (th2.pds + th2.n * th2.i);
	

			}	

			for(th2.i = 0; th2.i < th2.m; th2.i++) {
				th2.ppvt[th2.i] = (th2.pvt + th2.n * th2.i);
				th2.pps[th2.i] = (th2.ps + th2.n * th2.i);
			}
			for(th2.i = 0; th2.i < th2.m; th2.i++) th2.ppudsvt[th2.i] = (th2.pudsvt + th2.q * th2.i); 
	
			for(th2.j=0;th2.j<th2.m;th2.j++) {
				for(th2.i=0;th2.i<th2.n;th2.i++) {
					th2.ppa[th2.i][th2.j]=(float)*th2.img3;
					//*th3.inbufch=(char)*th2.inbuf;
					//printf("%d %d %5.1f \n",th2.i,th2.j,th2.ppa[th2.i][th2.j]);
					//printf("%d %d %d \n",th2.i,th2.j,*th2.inbufch);
					th2.img3++;
					//th2.inbufch++;
				}
			}
			//pgmWriteFile(((struct FILEs*)strptr)->pgm3,th2.inbufchfr,th2.m,th2.n);
			th2.pw=(float *)&th2.w;
			th2.result = dsvd(th2.ppa,th2.m,th2.n,th2.pw,th2.ppv);
			//for(th2.i=0;th2.i<th2.m;th2.i++) printf("%5.8f \n",th2.w[th2.i]);
			((struct FILEs*)strptr)->status = 2;
			th2.outptr = fopen(((struct FILEs*)strptr)->first_output,"w");
			if (th2.outptr == 0) printf("can not open file S.bin for writing\n");
			th2.result = fwrite(th2.pw,sizeof(float),th2.m,th2.outptr);
			fclose(th2.outptr);
			printf("U row = %d col = %d \n",th2.m,th2.n);
			//result = disp(th2.ppa,th2.m,th2.n);
			printf("Singular Values\n");
			//clear the S diagonal matrix
			for(th2.j=0;th2.j<th2.m;th2.j++) {
				for(th2.i=0;th2.i<th2.n;th2.i++) {
					th2.ppds[th2.i][th2.j] = 0;
				}	
			}	
			//create the S diagonal matrix from w
			th2.j=0;
			for(th2.i=0;th2.i<th2.m;th2.i++) { 
				if (th2.i <= 255) th2.ppds[th2.i][th2.j] = th2.w[th2.i];
				th2.j++;
			}
			
			printf("V row = %d col = %d \n",th2.m,th2.n);
			//th2.result = disp(th2.ppv,th2.m,th2.n);

			printf("V' row = %d col = %d \n",th2.m,th2.n);
 
			th2.result = trans(th2.ppv,th2.ppvt,th2.m,th2.n);
			//result = disp(ppvt,n,m);
			printf("Call mul u * s  \n");
			th2.result = mul(th2.ppa,th2.ppds,th2.ppuds,th2.m,th2.n,th2.p,th2.q);
			printf("UDS row = %d col = %d \n",th2.m,th2.n);
			//th2.result = disp(th2.ppuds,th2.m,th2.n);
			printf("Call mul u * ds * vt \n");

			th2.result = mul(th2.ppuds,th2.ppvt,th2.ppudsvt,th2.m,th2.n,th2.m,th2.n);
			printf("USDVT row = %d col = %d \n",th2.p,th2.q);
			for(th2.j=0;th2.j<th2.m;th2.j++) {
				for(th2.i=0;th2.i<th2.n;th2.i++) {
					//th2.pps[th2.i][th2.j]=(int)th2.ppudsvt[th2.i][th2.j];
					th2.pps[th2.i][th2.j]=FLOAT_TO_INT(th2.ppudsvt[th2.i][th2.j]);
					//printf("%d ",pps[i][j]);
		
				}
			}
			th2.outptr = fopen(((struct FILEs*)strptr)->second_output,"w"); 
			printf("ps converted from float to int 0x%x \n",th2.outptr);
			
			if (th2.outptr == 0) printf("can not open file reconst.bin for writing\n");
			th2.result = fwrite(th2.ps,sizeof(int),th2.m*th2.n,th2.outptr);
			fclose(th2.outptr);
			printf("# of data written 0x%x \n",th2.result);
			((struct FILEs*)strptr)->status = 4;
			//Cleaning up 
			free(th2.inbuffr);
			free(th2.ppvfr);
			free(th2.ppudsfr);
			free(th2.ppafr);
			free(th2.ppvtfr);
			free(th2.ppdsfr);
			free(th2.ppudsvtfr);
			free(th2.ppsfr);
			
			break;		
		
		}

			 
	return NULL;
}
 

 
