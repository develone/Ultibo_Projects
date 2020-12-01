/*
 * test.c
 * 
 * A simple C library to include in your Ultibo project
 * 
 */
 
#include <stdio.h>

////////////////////////////////////////////////////////////////////////////////
//
// Filename: 	lifting.c
//
// Project:	XuLA2-LX25 SoC based upon the ZipCPU
//
// Purpose:	This goal of this file is to perform, on either the ZipCPU or
//		a more traditional architecture, the lifting/WVT step of the
//	JPEG-2000 compression (and decompression) scheme.
//
//	Currently, the lifting scheme performs both forward and inverse 
//	transforms, and so (if done properly) it constitutes an identity
//	transformation.
//
// Creator:	Dan Gisselquist, Ph.D.
//		Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2015-2016, Gisselquist Technology, LLC
//
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of  the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory, run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
//
// License:	GPL, v3, as defined and found on www.gnu.org,
//		http://www.gnu.org/licenses/gpl.html
//
//
////////////////////////////////////////////////////////////////////////////////
//
//
#include "lifting.h"
#include "test.h"
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>
void	singlelift(int rb, int w, int * const ibuf, int * const obuf) {
	int	col, row;

	for(row=0; row<w; row++) {
		register int	*ip, *op, *opb;
		register int	ap,b,cp,d;

		//
		// Ibuf walks down rows (here), but reads across columns (below)
		// We might manage to get rid of the multiply by doing something
		// like: 
		//	ip = ip + (rb-w);
		// but we'll keep it this way for now.
		//
		//setting to beginning of each row
		ip = ibuf+row*rb;

		//
		// Obuf walks across columns (here), writing down rows (below)
		//
		// Here again, we might be able to get rid of the multiply,
		// but let's get some confidence as written first.
		//
		op = obuf+row;
		opb = op + w*rb/2;

		//
		// Pre-charge our pipeline
		//

		// a,b,c,d,e ...
		// Evens get updated first, via the highpass filter
		ap = ip[0];
		b  = ip[1];
		cp = ip[2];
		d  = ip[3]; ip += 4;
		//
		ap = ap-b; // img[0]-(img[1]+img[-1])>>1)
		cp = cp- ((b+d)>>1);
		 
		op[0] = ap;
		opb[0]  = b+((ap+cp+2)>>2);

		for(col=1; col<w/2-1; col++) {
			op +=rb; // = obuf+row+rb*col = obuf[col][row]
			opb+=rb;// = obuf+row+rb*(col+w/2) = obuf[col+w/2][row]
			ap = cp;
			b  = d;
			cp = ip[0];	// = ip[row][2*col+1]
			d  = ip[1];	// = ip[row][2*col+2]

			//HP filter in fwd dwt			
			cp  = (cp-((b+d)>>1)); //op[0] is obuf[col][row]
			*op = ap; //op[0] is obuf[col][row]
			 
			//LP filter in fwd dwt
			*opb = b+((ap+cp+2)>>2);
			ip+=2;	// = ibuf + (row*rb)+2*col
		}

		op += rb; opb += rb;
		*op  = cp;
		*opb = d+((cp+1)>>3);
	}
}

void	ilift(int rb, int w, int * const ibuf, int * const obuf) {
	int	col, row;

	for(row=0; row<w; row++) {
		register int	*ip, *ipb, *op;
		register int	b,c,d,e;

		//
		// Ibuf walks down rows (here), but reads across columns (below)
		// We might manage to get rid of the multiply by doing something
		// like: 
		//	ip = ip + (rb-w);
		// but we'll keep it this way for now.
		//
		//setting to beginning of each row
		op = obuf+row*rb;

		//
		// Obuf walks across columns (here), writing down rows (below)
		//
		// Here again, we might be able to get rid of the multiply,
		// but let's get some confidence as written first.
		//
		ip  = ibuf+row;
		ipb = ip + w*rb/2;

		//
		// Pre-charge our pipeline
		//

		// a,b,c,d,e ...
		// Evens get updated first, via the highpass filter
		c = ip[0]; // would've been called 'a'
		ip += rb;
		e = ip[0];	// Would've been called 'c'
		d  = ipb[0] -((c+e+2)>>2);

		op[0] = c+d;	// Here's the mirror, left-side
		op[1] = d;

		for(col=1; col<w/2-1; col++) {
			op += 2;
			ip += rb; ipb += rb;

			c = e; b = d;
			e = ip[0];
			d = ipb[0] - ((c+e+2)>>2);
			c = c + ((b+d)>>1);

			op[0] = c;
			op[1] = d;
		}

		ipb += rb;
		d = ipb[0] - ((e+1)>>3);

		c = e + ((b+d)>>1);
		op[2] = c;
		op[3] = d;	// Mirror
	}
}

void	lifting(int w, int *ibuf, int *tmpbuf) {
	const	int	rb = w;
	int	lvl;

	int	*ip = ibuf, *tp = tmpbuf;
	int	ov[3];

	const int	LVLS = 1;

/*
	for(lvl=0; lvl<w*w; lvl++)
		ibuf[lvl] = 0;
	for(lvl=0; lvl<w*w; lvl++)
		tmpbuf[lvl] = 5000;

	for(lvl=0; lvl<w; lvl++)
		ibuf[lvl*(rb+1)] = 20;

	singlelift(rb,w,ip,tp);
	for(lvl=0; lvl<w*w; lvl++)
		ibuf[lvl] = tmpbuf[lvl];

	return;
*/

	for(lvl=0; lvl<LVLS; lvl++) {
		// Process columns -- leave result in tmpbuf
		singlelift(rb, w, ip, tp);
		// Process columns, what used to be the rows from the last
		// round, pulling the data from tmpbuf and moving it back
		// to ibuf.
		singlelift(rb, w, tp, ip);

		// lower_upper
		//
		// For this, we just adjust our pointer(s) so that the "image"
		// we are processing, as referenced by our two pointers, now
		// references the bottom right part of the image.
		//
		// Of course, we haven't really changed the dimensions of the
		// image.  It started out rb * rb in size, or the initial w*w,
		// we're just changing where our pointer into the image is.
		// Rows remain rb long.  We pretend (above) that this new image
		// is w*w, or should I say (w/2)*(w/2), but really we're just
		// picking a new starting coordinate and it remains rb*rb.
		//
		// Still, this makes a subimage, within our image, containing
		// the low order results of our processing.
		int	offset = w*rb/2+w/2;
		ip = &ip[offset];
		tp = &tp[offset];
		ov[lvl] = offset + ((lvl)?(ov[lvl-1]):0);

		// Move to the corner, and repeat
		w>>=1;
	}
    	/*
	for(lvl=(LVLS-1); lvl>=0; lvl--) {
		int	offset;

		w <<= 1;

		if (lvl)
			offset = ov[lvl-1];
		else
			offset = 0;
		ip = &ibuf[offset];
		tp = &tmpbuf[offset];

		ilift(rb, w, ip, tp);
		ilift(rb, w, tp, ip);
	}
	*/
}


void test ()
{

   printf ("Hello Ultibo from C!!\n");
   
	sam = 164;
	lf = 156;
	rh = 160;
	fwd = 7;
	dwt = lift(sam, lf, rh, fwd);
	printf ("fwd dwt even \n");
	printf("%d %d %d %d\n",sam, lf, rh, dwt);
	fwd = 5;
	sam = dwt;
	dwt = lift(sam, lf, rh, fwd);
	printf ("inv dwt even \n");
	printf("%d %d %d %d\n",sam, lf, rh, dwt);
	
	fwd = 6;
	sam = 164;
	dwt = lift(sam, lf, rh, fwd);
	printf ("fwd dwt odd \n");
	printf("%d %d %d %d\n",sam, lf, rh, dwt);
	fwd = 4;
	sam = dwt;
	dwt = lift(sam, lf, rh, fwd);
	printf ("inv dwt odd \n");
	printf("%d %d %d %d\n",sam, lf, rh, dwt);
	gettimeofday(&start, NULL);
	
	
	printf("start time in sec %ld\n", start);
	for(i = 0;i < 1e9; i++) {
		sam = 164;
		lf = 156; 
		rh = 160;
		fwd = 7;
		dwt = lift(sam, lf, rh, fwd); 
	}
	gettimeofday(&end, NULL);
	seconds  = end.tv_sec  - start.tv_sec;
	useconds = end.tv_usec - start.tv_usec;
 
    mtime = seconds + useconds;
 
    printf("Elapsed time: %ld microseconds\n", mtime);
	//end_sec = time(NULL);
	//printf("start time in sec %ld end time in sec %ld 1e9 dwt processing time %ld\n", start, end,(end - start) );
	
	
		
}
 
int lift(int sam, int lf, int rh, int fwd) 
{
	if (fwd==7) 
		dwt = sam - ((lf + rh) >> 1);
	else if (fwd==5)
		 dwt = sam + ((lf + rh) >> 1);
	else if (fwd==6)
		dwt = sam + ((lf + rh + 2) >> 2);
	else 
		dwt = sam - ((lf + rh + 2 ) >> 2);

	return dwt;
}	

//pass ip to a routine 
//which malloc 3 area
//read 65536  values of red 262144 32 bit int  0xc0000424 to 0xc0040423
//read 65536  values of green 262144 32 bit int 0xc0040424 to 0xc00c0423
//read 65536  values of blue 262144 32 bit int

void xyz(int bp, long ss,int *xx)
{
	gettimeofday(&start, NULL);
	//start_sec = currentTime.tv_usec;	

printf("In xyz\n");
printf("bpp %ld\n",bp);

printf("size %ld\n",ss);
printf("pointer passed %x\n",*xx);

char *lclip = (char *)*xx;
int loop;
/* Need to determine the ww width & hh height 
 * given the ss bpp BPP bits per pixel & Size
 */   
double tt;
if(bp==8) tt = (double)ss;
else tt = (double)(ss/3.0);
int ww, hh;
ww = (int)sqrt(tt);
hh = ww;
printf("tt %lf sqrt tt %lf %d %d \n",tt,sqrt(tt),ww,hh);
printf("local char ptr %x\n",&lclip[0]);
/*
printf("local char ptr %x\n",&ip[0]);
printf("red %x\n",ip[0]);
ip++;

printf("local char ptr %x\n",&ip[0]);
printf("green %x\n",ip[0]);
ip++;

printf("local char ptr %x\n",&ip[0]);
printf("blue %x\n",ip[0]);
ip++;
*/
	IMAGEP		img;
	//int ww = 256;
	//int hh = 256;
	printf("allocating memory with malloc \n");
	img = (IMAGEP)malloc(sizeof(IMAGE)+4*ww*hh*sizeof(int));
	img->m_w = ww;
	img->m_h = hh;
	img->m_red   = img->data;
	img->m_green = &img->data[ww*hh];
	img->m_blue  = &img->data[2*ww*hh];
	img->m_tmp  = &img->data[3*ww*hh];
	//printf("the size of malloc %x \n",sizeof(img));
	printf("img->m_red 0x%x \n",img->m_red);
	printf("img->m_green 0x%x \n",img->m_green);
	printf("img->m_blue 0x%x \n",img->m_blue);
	printf("img->m_tmp 0x%x \n",img->m_tmp);
	
	printf("Copying RGB 8 bit char to 32 int \n");
	
	for (loop=0; loop < ss/3; loop++) {
		 *img->m_red = lclip[0];
		 lclip++;
		 img->m_red++;
		 *img->m_green = lclip[0];
		 lclip++;
		 img->m_green++;
		 *img->m_blue = lclip[0];
		 lclip++;
		 img->m_blue++;
	}
		
	printf("img->m_red 0x%x  \n",img->m_red);
	printf("img->m_green 0x%x \n",img->m_green);
	printf("img->m_blue 0x%x \n",img->m_blue);

    printf("reseting pointers \n");
     	
	img->m_red   = img->data;
	img->m_green = &img->data[ww*hh];
	img->m_blue  = &img->data[2*ww*hh];	

	lclip = (char *)*xx;

	printf("img->m_red 0x%x passed ptr 0x%x\n",img->m_red, &lclip[0]);
	printf("img->m_green 0x%x \n",img->m_green);
	printf("img->m_blue 0x%x \n",img->m_blue);	
	
	printf("Calling lifting red\n");
	
	//img->m_red   = img->data;
	lifting(ww, img->m_red, img->m_tmp);
	img->m_tmp  = &img->data[3*ww*hh];
	printf("Calling lifting green\n");
	
	//img->m_green = &img->data[ww*hh];
	lifting(ww, img->m_green, img->m_tmp);
	img->m_tmp  = &img->data[3*ww*hh];
	printf("Calling lifting blue\n");
	
	//img->m_blue  = &img->data[2*ww*hh];
	lifting(ww, img->m_blue, img->m_tmp);
	printf("lifting to Buffer\n");
	for (loop=0; loop < ss/3; loop++) {
		 lclip[0] = *img->m_red ;
		 lclip++;
		 img->m_red++;
		 lclip[0] = *img->m_green ;
		 lclip++;
		 img->m_green++;
		 lclip[0] = *img->m_blue ;
		 lclip++;
		 img->m_blue++;
	}
	
	gettimeofday(&end, NULL);
 
	seconds  = end.tv_sec  - start.tv_sec;
	useconds = end.tv_usec - start.tv_usec;
 
	mtime = seconds + useconds;
 
    printf("Elapsed time: %ld microseconds\n", mtime); 	
	free(img);	 
}
void PtoCptrs (int w, int *ibuf, int *tmpbuf)
{

	printf ("Hello Ultibo from C!!\n");
	printf ("This will demonstrate passing integers from Pascal to C\n");
	printf ("In additon demonstrate passing integers as pointer from Pascal to C\n");
	printf(" a %d b %d c %d \n",w,ibuf,tmpbuf);
	printf(" a %d b %d c %d \n",w,*ibuf,*tmpbuf);
	printf("Modifing ibuf to send back to Pascal\n");
	
	*ibuf = *ibuf + 50;
	printf(" ibuf %d \n",*ibuf);
	IMAGEP		img;
	int ww = 512;
	int hh = 512;
	printf("allocating memory with malloc \n");
	img = (IMAGEP)malloc(sizeof(IMAGE)+3*ww*hh*sizeof(int));
	img->m_w = ww;
	img->m_h = hh;
	img->m_red   = img->data;
	img->m_green = &img->data[ww*hh];
	img->m_blue  = &img->data[2*ww*hh];
	printf("the size of malloc %x \n",sizeof(img));
	printf("img->m_red 0x%x \n",img->m_red);
	printf("img->m_green 0x%x \n",img->m_green);
	printf("img->m_blue 0x%x \n",img->m_blue);
	free(img);
	
}
