/*
 * Copyright (c) 2008, Jerome Fimes, Communications & Systemes <jerome.fimes@c-s.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#define USE_OPJ_DEPRECATED
/* set this macro to enable profiling for the given test */
/* warning : in order to be effective, openjpeg must have been built with profiling enabled !! */
/*#define _PROFILE*/
 
#include <stdio.h>
 
#include "dwtlift.h"
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>
#include "openjpeg.h"
#include "opj_config.h"
#include "openjpeg.h"
#include <string.h>

#include "format_defs.h"
/* -------------------------------------------------------------------------- */
/* Declarations                                                               */ 
int get_file_format(const char *filename);
static int infile_format(const char *fname);
static void info_callback(const char *msg, void *client_data); 
#define JP2_RFC3745_MAGIC "\x00\x00\x00\x0c\x6a\x50\x20\x20\x0d\x0a\x87\x0a"
#define JP2_MAGIC "\x0d\x0a\x87\x0a"
/* position 45: "\xff\x52" */
#define J2K_CODESTREAM_MAGIC "\xff\x4f\xff\x51"
int height, width;
/* -------------------------------------------------------------------------- */
typedef struct
    {
        unsigned char RGB[3];
    }RGB;
    
    
// ********** Create Matrix **********
RGB** createMatrix(){
    RGB** Matrix;
    int i;
    Matrix = (RGB **) malloc (sizeof (RGB*) * height);
    if (Matrix == NULL){
        perror("***** No memory available *****");
        exit(0);
    }
    for (i=0;i<height;i++){
        Matrix[i] = (RGB *) malloc (sizeof(RGB) * width);
        if (Matrix[i] == NULL){
        perror("***** No memory available *****");
            exit(0);
        }
    }
    return(Matrix);
}

RGB** loadImage(FILE *arq, RGB** Matrix){
    int i,j;
    RGB tmp;
    long pos = 119;
 
    fseek(arq,0,0);
 
    for (i=0; i<height; i++){
        for (j=0; j<width; j++){
            pos+= 3;
            fseek(arq,pos,0);
            fread(&tmp,(sizeof(RGB)),1,arq);
            Matrix[i][j] = tmp;
        }
    }
    return(Matrix);
}
/*upside down data in bitmap*Wr_p_matrix(Matrix_aux_wr,b_decompress, g_decompress, r_decompress );*/
void Wr_p_matrix(RGB** Matrix,char *r,char *g,char *b) {
	int i,j;
	RGB tmp;
	for (i=height-1; i>-1; i--){
		for (j=0; j<width; j++){
			
			//printf("%d %d 0x%x 0x%x 0x%x \n ",i,j,tmp.RGB[1],tmp.RGB[0],tmp.RGB[2]);
			tmp.RGB[1] = g[0];
			g++;
			tmp.RGB[0] = r[0];
			r++;			
			tmp.RGB[2] = b[0];
			b++;
			//printf("0x%x 0x%x 0x%x \n",tmp.RGB[0],tmp.RGB[1],tmp.RGB[2]);	
			Matrix[i][j] = tmp;		
		}
	}
}
void Wr_pp_matrix(RGB** Matrix,char *r,char *g,char *b) {
	int i,j;
	RGB tmp;
	for (i=0; i<height; i++){
		for (j=0; j<width; j++){
			
			//printf("%d %d 0x%x 0x%x 0x%x \n ",i,j,tmp.RGB[1],tmp.RGB[0],tmp.RGB[2]);
			tmp.RGB[1] = r[0];
			r++;
			tmp.RGB[0] = b[0];
			b++;			
			tmp.RGB[2] = g[0];
			g++;
			//printf("0x%x 0x%x 0x%x \n",tmp.RGB[0],tmp.RGB[1],tmp.RGB[2]);	
			Matrix[i][j] = tmp;		
		}
	}
}
/*upside down data in bitmap*/
void p_matrix(RGB** Matrix,char *r,char *g,char *b) {
	int i,j;
	RGB tmp;
	for (i=height-1; i>-1; i--){
		for (j=0; j<width; j++){
			tmp = Matrix[i][j];
			//printf("%d %d 0x%x 0x%x 0x%x \n ",i,j,tmp.RGB[1],tmp.RGB[0],tmp.RGB[2]);
			r[0] = tmp.RGB[1];
			r++;
			g[0] = tmp.RGB[0];
			g++;			
			b[0] = tmp.RGB[2];
			b++;			
		}
	}
}

/*top down data in bitmap*/
void pp_matrix(RGB** Matrix,char *r,char *g,char *b) {
	int i,j;
	RGB tmp;
	for (i=0; i<height; i++){
		for (j=0; j<width; j++){
			tmp = Matrix[i][j];
			//printf("%d %d 0x%x 0x%x 0x%x \n ",i,j,tmp.RGB[1],tmp.RGB[0],tmp.RGB[2]);
			r[0] = tmp.RGB[1];
			r++;
			g[0] = tmp.RGB[0];
			g++;			
			b[0] = tmp.RGB[2];
			b++;			
		}
	}
}
 
typedef struct
    {
        unsigned int size;
        int width,height;
        unsigned short int planes;
        unsigned short int bpp;
        unsigned int compression;
        unsigned int imagesize;
        int xresolution,yresolution;
        unsigned int colours;
        unsigned int impcolours;
    }INFOHEADER;
 
// ********** Read BMP info from file **********
INFOHEADER readInfo(FILE *arq){
    INFOHEADER info;
 
    // Image Width in pixels
    fseek(arq,18,0);
    fread(&info.width,1,4,arq);
 
    // Image Height in pixels
    fseek(arq,22,0);
    fread(&info.height,1,4,arq);

    fseek(arq,26,0);
    fread(&info.planes,1,2,arq); 
    // Color depth, BPP (bits per pixel)
    fseek(arq,28,0);
    fread(&info.bpp,1,2,arq);
 
    // Compression type
    // 0 = Normmally
    // 1 = 8 bits per pixel
    // 2 = 4 bits per pixel
    fseek(arq,30,0);
    fread(&info.compression,1,4,arq);
 
    // Image size in bytes
    fseek(arq,34,0);
    fread(&info.imagesize,1,4,arq);
    
    fseek(arq,38,0);
    fread(&info.xresolution,1,4,arq); 
    fseek(arq,42,0);
    fread(&info.yresolution,1,4,arq);    
    // Number of color used (NCL)
    // vccalue = 0 for full color set
    fseek(arq,46,0);
    fread(&info.colours,1,4,arq);
 
    // Number of important color (NIC)
    // value = 0 means all collors important
    fseek(arq,54,0);
    fread(&info.impcolours,1,4,arq);
 
    return(info);
}
void writeImage(FILE *arqw, RGB** Matrix){
    int i,j;
    RGB tmp;
    //long pos = 119;
    long pos = 122;
	//printf("height %d width %d \n", height,width);
	//tmp = Matrix[0][0];
	//printf("0x%x 0x%x 0x%x \n",tmp.RGB[0],tmp.RGB[1],tmp.RGB[2]);
    //fseek(arqw,0,0);
    char *wrbuf;
    wrbuf = (char *)malloc(height*width*3);
    //printf("0x%x \n",wrbuf); 
    for (i=0; i<height; i++){
        for (j=0; j<width; j++){
            //pos+= 3;
            //fseek(arqw,pos,0);
            //printf("%ld ",pos);
            //printf("%d %d \n",i,j);
            tmp = Matrix[i][j];
            //printf("0x%x 0x%x 0x%x \n",tmp.RGB[0],tmp.RGB[1],tmp.RGB[2]);
            //fwrite(&tmp,(sizeof(RGB)),1,arqw);
            Matrix[i][j] = tmp;
            wrbuf[0] = (char)tmp.RGB[0];
            wrbuf++;
            wrbuf[0] = (char)tmp.RGB[1];
            wrbuf++;
            wrbuf[0] = (char)tmp.RGB[2];
            wrbuf++;            
        }
    }
    fseek(arqw,pos,0);
    //printf("0x%x \n",wrbuf);
    wrbuf= wrbuf - height*width*3;
    //printf("0x%x \n",wrbuf);
    fwrite(wrbuf,(height*width*3),1,arqw);
    free(wrbuf);    
    //return(Matrix);
    fclose(arqw);
}
// ********** Write BMP info from file **********61 73 63
void writeInfo(FILE *arqw,INFOHEADER infowrite){
    //INFOHEADER info;
    char type[3];
    int hdr;
	char offset=122,unknow=108;
    
    hdr = infowrite.height*infowrite.width*3+offset;
    type[0] = 0x42;
    type[1] = 0x4D;
    
	fseek(arqw,0,0);
    fwrite(type,1,2,arqw);
    
    fseek(arqw,2,0);
    fwrite(&hdr,1,4,arqw);

    type[0] = 0x00;
    type[1] = 0x00;
    type[2] = 0x00;

    fseek(arqw,6,0);
	fwrite(type,1,3,arqw);
	
    fseek(arqw,9,0);
	fwrite(type,1,1,arqw);	  
	  
    fseek(arqw,10,0);
    fwrite(&offset,1,1,arqw);

    fseek(arqw,11,0);
	fwrite(type,1,3,arqw);
	
    fseek(arqw,14,0);
    fwrite(&unknow,1,1,arqw);

    fseek(arqw,15,0);
	fwrite(type,1,3,arqw);
	    	    
    // Image Width in pixels
    fseek(arqw,18,0);
    fwrite(&infowrite.width,1,4,arqw);
 
    // Image Height in pixels
    fseek(arqw,22,0);
    fwrite(&infowrite.height,1,4,arqw);
    
    fseek(arqw,26,0);
    fwrite(&infowrite.planes,1,2,arqw); 
    
    // Color depth, BPP (bits per pixel)
    fseek(arqw,28,0);
    fwrite(&infowrite.bpp,1,2,arqw);
 
    // Compression type
    // 0 = Normmally
    // 1 = 8 bits per pixel
    // 2 = 4 bits per pixel
    fseek(arqw,30,0);
    fwrite(&infowrite.compression,1,4,arqw);
 
    // Image size in bytes
    fseek(arqw,34,0);
    fwrite(&infowrite.imagesize,1,4,arqw);
		
    fseek(arqw,38,0);
    fwrite(&infowrite.xresolution,1,4,arqw); 
    fseek(arqw,42,0);
    fwrite(&infowrite.yresolution,1,4,arqw);
 
    // Number of color used (NCL)
    // vccalue = 0 for full color set
    fseek(arqw,46,0);
    fwrite(&infowrite.colours,1,4,arqw);
 
    // Number of important color (NIC)
    // value = 0 means all collors important
    fseek(arqw,54,0);
    fwrite(&infowrite.impcolours,1,4,arqw);
 
    //return(info);
    //fclose(arqw);

	//fseek(arqw,15,0);
    //fwrite(type,1,3,arqw);    
}
// ********** Verify if the file is BMP *********
void isBMP(FILE* arq, INFOHEADER info){
    char type[3];
    unsigned short int bpp;
    fseek(arq,0,0);
    fread(type,1,2,arq);
    type[2] = '\0';
 
    fseek(arq,28,0);
    fread(&bpp,1,2,arq);
	printf("testing if bitmap %c%c bpp = %d \n",type[0],type[1],bpp);
    if (strcmp(type,"BM") || (bpp != 24)){
        printf("\nThe file is not BMP format or is not 24 bits\n");
            exit(0);
    }
}
static int infile_format(const char *fname)
{
		
        FILE *reader;
        const char *s, *magic_s;
        int ext_format, magic_format,ii;
        unsigned char buf[12];
        unsigned int l_nb_read;

        reader = fopen(fname, "rb");
		printf("In infile_format %s reader 0x%x\n", fname,reader);
        if (reader == NULL)
                return -1;

        memset(buf, 0, 12);
        l_nb_read = (unsigned int)fread(buf, 1, 12, reader);
        printf("%s l_nb_read %d 0x%x ",fname,l_nb_read,reader);
        for(ii = 0;ii < 12;ii++) printf("0x%x ",buf[ii]);
        printf("\n");
        fclose(reader);
        if (l_nb_read != 12)
                return -1;

        ext_format = get_file_format(fname);

        if (ext_format == JPT_CFMT)
                return JPT_CFMT;

        if (memcmp(buf, JP2_RFC3745_MAGIC, 12) == 0 || memcmp(buf, JP2_MAGIC, 4) == 0) {
                magic_format = JP2_CFMT;
                magic_s = ".jp2";
        }
        else if (memcmp(buf, J2K_CODESTREAM_MAGIC, 4) == 0) {
                magic_format = J2K_CFMT;
                magic_s = ".j2k or .jpc or .j2c";
        }
        else
                return -1;

        if (magic_format == ext_format)
                return ext_format;

        s = fname + strlen(fname) - 4;

        fputs("\n===========================================\n", stderr);
        fprintf(stderr, "The extension of this file is incorrect.\n"
                        "FOUND %s. SHOULD BE %s\n", s, magic_s);
        fputs("===========================================\n", stderr);

        return magic_format;
}

int get_file_format(const char *filename) {
        unsigned int i;
        static const char *extension[] = {"pgx", "pnm", "pgm", "ppm", "bmp","tif", "raw", "rawl", "tga", "png", "j2k", "jp2", "jpt", "j2c", "jpc" };
        static const int format[] = { PGX_DFMT, PXM_DFMT, PXM_DFMT, PXM_DFMT, BMP_DFMT, TIF_DFMT, RAW_DFMT, RAWL_DFMT, TGA_DFMT, PNG_DFMT, J2K_CFMT, JP2_CFMT, JPT_CFMT, J2K_CFMT, J2K_CFMT };
        char * ext = strrchr(filename, '.');
        if (ext == NULL)
                return -1;
        ext++;
        if(ext) {
                for(i = 0; i < sizeof(format)/sizeof(*format); i++) {
                        if(strcasecmp(ext, extension[i]) == 0) {
                                return format[i];
                        }
                }
        }

        return -1;
}


int decompress(int da_x0, int da_y0, int da_x1, int da_y1,const char *input_file)
{
		/*
		char lclip = (char *)*bufferptr;
		char *r_decompress,*g_decompress,*b_decompress;
		const char *r_decompress_fn="red";
		const char *g_decompress_fn="green";
		const char *b_decompress_fn="blue";
		*/
		const char *r_decompress_fn="red";
		const char *g_decompress_fn="green";
		const char *b_decompress_fn="blue";		
		char *r_decompress,*g_decompress,*b_decompress;	
        opj_dparameters_t l_param;
        opj_codec_t * l_codec;
        opj_image_t * l_image;
        opj_stream_t * l_stream;
        OPJ_UINT32 l_data_size;
        //OPJ_UINT32 l_max_data_size = 1000;
        OPJ_UINT32 l_max_data_size = da_x1*da_y1*3;
        OPJ_UINT32 l_tile_index;
        //OPJ_BYTE * l_data = (OPJ_BYTE *) malloc(1000);
        OPJ_BYTE * l_data = (OPJ_BYTE *) malloc(da_x1*da_y1*3);
        OPJ_BOOL l_go_on = OPJ_TRUE;
        OPJ_UINT32 l_nb_comps=0 ;
        OPJ_INT32 l_current_tile_x0,l_current_tile_y0,l_current_tile_x1,l_current_tile_y1;
		
		//const char *input_file;
		//input_file = "dtest.j2k";
		printf("%d %d %d %d %s l_data_size %d 0x%x \n",da_x0,da_y0,da_x1,da_y1,input_file,l_max_data_size,l_data);
		
		/*
        int da_x0=0;
        int da_y0=0;
        int da_x1=1000;
        int da_y1=1000;
        
        const char *input_file;
        */

        /* should be test_tile_decoder 0 0 1000 1000 tte1.j2k */
        /*
        if( argc == 6 )
        {
                da_x0=atoi(argv[1]);
                da_y0=atoi(argv[2]);
                da_x1=atoi(argv[3]);
                da_y1=atoi(argv[4]);
                input_file = argv[5];

        }
        else
        {
                da_x0=0;
                da_y0=0;
                da_x1=1000;
                da_y1=1000;
                input_file = "test.j2k";
        }
		*/
	gettimeofday(&start, NULL);

	seconds  = end.tv_sec  - start.tv_sec;
	useconds = end.tv_usec - start.tv_usec;
 
	mtime = seconds + useconds;
 
	printf("Start decompression: %ld seconds %ld useconds %ld starting openjpeg\n", mtime,seconds, useconds);
        if (! l_data) {
                return EXIT_FAILURE;
        }

        l_stream = opj_stream_create_default_file_stream(input_file,OPJ_TRUE);
        printf("l_stream 0x%x\n", l_stream);
        if (!l_stream){
                free(l_data);
                fprintf(stderr, "ERROR -> failed to create the stream from the file\n");
                return EXIT_FAILURE;
        }

        /* Set the default decoding parameters */
        opj_set_default_decoder_parameters(&l_param);

        /* */
        l_param.decod_format = infile_format(input_file);

        /** you may here add custom decoding parameters */
        /* do not use layer decoding limitations */
        l_param.cp_layer = 0;

        /* do not use resolutions reductions */
        l_param.cp_reduce = 0;

        /* to decode only a part of the image data */
        /*opj_restrict_decoding(&l_param,0,0,1000,1000);*/
        switch(l_param.decod_format) {
                case J2K_CFMT:	/* JPEG-2000 codestream */
                        {
                                /* Get a decoder handle */
                                l_codec = opj_create_decompress(OPJ_CODEC_J2K);
                                break;
                        }
                case JP2_CFMT:	/* JPEG 2000 compressed image data */
                        {
                                /* Get a decoder handle */
                                l_codec = opj_create_decompress(OPJ_CODEC_JP2);
                                break;
                        }
                default:
                        {    
                                fprintf(stderr, "ERROR -> Not a valid JPEG2000 file!\n");
                                free(l_data);
                                opj_stream_destroy(l_stream);
                                return EXIT_FAILURE;
                        }
        }        

        

        /* catch events using our callbacks and give a local context */		
        opj_set_info_handler(l_codec, info_callback,00);
        //opj_set_warning_handler(l_codec, warning_callback,00);
        //opj_set_error_handler(l_codec, error_callback,00);

        /* Setup the decoder decoding parameters using user parameters */
        if (! opj_setup_decoder(l_codec, &l_param))
        {
                fprintf(stderr, "ERROR -> j2k_dump: failed to setup the decoder\n");
                free(l_data);
                opj_stream_destroy(l_stream);
                opj_destroy_codec(l_codec);
                return EXIT_FAILURE;
        }

        /* Read the main header of the codestream and if necessary the JP2 boxes*/
        if (! opj_read_header(l_stream, l_codec, &l_image))
        {
                fprintf(stderr, "ERROR -> j2k_to_image: failed to read the header\n");
                free(l_data);
                opj_stream_destroy(l_stream);
                opj_destroy_codec(l_codec);
                return EXIT_FAILURE;
        }

        if (!opj_set_decode_area(l_codec, l_image, da_x0, da_y0,da_x1, da_y1)){
                fprintf(stderr,	"ERROR -> j2k_to_image: failed to set the decoded area\n");
                free(l_data);
                opj_stream_destroy(l_stream);
                opj_destroy_codec(l_codec);
                opj_image_destroy(l_image);
                return EXIT_FAILURE;
        }


        while (l_go_on)
        {
                if (! opj_read_tile_header( l_codec,
                                        l_stream,
                                        &l_tile_index,
                                        &l_data_size,
                                        &l_current_tile_x0,
                                        &l_current_tile_y0,
                                        &l_current_tile_x1,
                                        &l_current_tile_y1,
                                        &l_nb_comps,
                                        &l_go_on))
                {
                        free(l_data);
                        opj_stream_destroy(l_stream);
                        opj_destroy_codec(l_codec);
                        opj_image_destroy(l_image);
                        return EXIT_FAILURE;
                }

                if (l_go_on)
                {
                        if (l_data_size > l_max_data_size)
                        {
                                OPJ_BYTE *l_new_data = (OPJ_BYTE *) realloc(l_data, l_data_size);
                                if (! l_new_data)
                                {
                                        free(l_new_data);
                                        opj_stream_destroy(l_stream);
                                        opj_destroy_codec(l_codec);
                                        opj_image_destroy(l_image);
                                        return EXIT_FAILURE;
                                }
                                l_data = l_new_data;
                                l_max_data_size = l_data_size;
                        }

                        if (! opj_decode_tile_data(l_codec,l_tile_index,l_data,l_data_size,l_stream))
                        {
                                free(l_data);
                                opj_stream_destroy(l_stream);
                                opj_destroy_codec(l_codec);
                                opj_image_destroy(l_image);
                                return EXIT_FAILURE;
                        }
                        /** now should inspect image to know the reduction factor and then how to behave with data */
                }
        }
        gettimeofday(&end, NULL);

		seconds  = end.tv_sec  - start.tv_sec;
		useconds = end.tv_usec - start.tv_usec;
 
		mtime = seconds + useconds;
		printf("Decompression time: %ld seconds %ld useconds %ld \n",mtime,seconds,useconds);
		gettimeofday(&start, NULL);

		seconds  = end.tv_sec  - start.tv_sec;
		useconds = end.tv_usec - start.tv_usec;
 
		mtime = seconds + useconds;
 
		printf("start writing: %ld seconds %ld useconds %ld starting openjpeg\n", mtime,seconds, useconds);		
		r_decompress = 	l_data;
		octave_write_byte(r_decompress_fn,r_decompress,da_x1*da_y1);
		g_decompress = 	l_data+da_x1*da_y1+da_x1*da_y1;
		octave_write_byte(g_decompress_fn,g_decompress,da_x1*da_y1);
		b_decompress = 	l_data+da_x1*da_y1;
		octave_write_byte(b_decompress_fn,b_decompress,da_x1*da_y1);
		/*
		 *Writing the decompressed values 
		 * 
		 *  
		*/
		/*
		for (loop=0; loop < da_x1*da_y1/3; loop++) {
				lclip = *r_decompress ;
				lclip++;
				r_decompress++;
				lclip = *g_decompress ;
				lclip++;
				g_decompress++;
				lclip = *b_decompress ;
				lclip++;
				b_decompress++;
		}
		*/
			RGB** Matrix_aux_wr;
	

	//Matrix_aux_wr = Matrix;
	int bpp;
	FILE *oo;
	height = da_y1;
	width = da_x1;
	Matrix_aux_wr = createMatrix();
 
	
	Wr_p_matrix(Matrix_aux_wr,g_decompress, b_decompress, r_decompress ); 
	oo = fopen("test_wr.bmp","wb");
	if (!oo) {
			printf("Unable to open file for writing!");
			return 1;
	}
	INFOHEADER infowr;	
	infowr.height = height;
	infowr.width = width;
	
	infowr.imagesize = height*width*3;
	bpp = 24;
	infowr.bpp = bpp;
	infowr.planes = 1;
	infowr.compression = 0;
	infowr.impcolours = 0x73524742;
	infowr.xresolution = 2835;
	infowr.yresolution = 2835;
	
	printf("WR imagesize = 0x%x \n",infowr.imagesize);
	printf("Wr bpp = %d \n",infowr.bpp);
	printf("Wr xresolution = %d yresolution %d \n",infowr.xresolution,infowr.yresolution);
 
	writeInfo(oo,infowr);
	writeImage(oo,Matrix_aux_wr);
	free(Matrix_aux_wr);
		gettimeofday(&end, NULL);

		seconds  = end.tv_sec  - start.tv_sec;
		useconds = end.tv_usec - start.tv_usec;
 
		mtime = seconds + useconds;
 
		printf("File writes: %ld seconds %ld useconds %ld starting openjpeg\n", mtime,seconds, useconds);

        if (! opj_end_decompress(l_codec,l_stream))
        {
                free(l_data);
                opj_stream_destroy(l_stream);
                opj_destroy_codec(l_codec);
                opj_image_destroy(l_image);
                return EXIT_FAILURE;
        }

        /* Free memory */
        free(l_data);
        opj_stream_destroy(l_stream);
        opj_destroy_codec(l_codec);
        opj_image_destroy(l_image);

        /* Print profiling*/
        /*PROFPRINT();*/
 
        return EXIT_SUCCESS;
}

//pass ip to a routine 
//which malloc 3 area
//read 65536  values of red 262144 32 bit int  0xc0000424 to 0xc0040423
//read 65536  values of green 262144 32 bit int 0xc0040424 to 0xc00c0423
//read 65536  values of blue 262144 32 bit int
int octave_write_byte(const char * fn,char * d_ptr, int sz) {
	 
	FILE *subfileptr;
	subfileptr = fopen(fn,"w");
	printf("file name %s data ptr 0x%x size %d \n",fn, d_ptr,sz);
	if (NULL == subfileptr) {
		/*
		fprintf(stderr, "Could not open red for writing\n");
		perror("RED-WR:");
		exit(EXIT_FAILURE);
		*/
		return(0);
	}
 
	if (sz != (int)fwrite(d_ptr,  sizeof(char), sz, subfileptr)) {
		fprintf(stderr, "Write of red failed\n"); perror("RED:");
		exit(EXIT_FAILURE);
	}
	
 	
	fclose(subfileptr);
	
	return(1);
}

/**
sample error debug callback expecting no client object
*/
static void error_callback(const char *msg, void *client_data) {
	(void)client_data;
	fprintf(stdout, "[ERROR] %s", msg);
}
/**
sample warning debug callback expecting no client object
*/
static void warning_callback(const char *msg, void *client_data) {
	(void)client_data;
	fprintf(stdout, "[WARNING] %s", msg);
}
/**
sample debug callback expecting no client object
*/
static void info_callback(const char *msg, void *client_data) {
	(void)client_data;
	fprintf(stdout, "[INFO] %s", msg);
}
struct GPU_FFT_HOST {
    unsigned mem_flg, mem_map, peri_addr, peri_size;
};
void lift_config(int dec, int enc, int TCP_DISTORATIO, int FILTER, int CR, int flg, int bp, long imgsz,long him,long wim, int *bufferptr)
{
	struct GPU_FFT_HOST host;
	
	printf ("Hello Ultibo from C!! Called by Pascal ");
	

	printf("starting compression: %ld seconds %ld useconds %ld \n", mtime,seconds, useconds);
	int height, width;
	int TopDown,plot;
	TopDown = 0;
	plot = 0;
	
	if (flg==0) printf("in lift_config dec %d enc %d compression CR %d bpp %d flg %d him %d wim %d\n", dec,enc,CR,bp,flg,him,wim);
	else printf("in lift_config dec %d enc %d distoratio %d bpp %d CR %d flg %d him wim %d%d\n", dec,enc,TCP_DISTORATIO,bp,CR,flg,him,wim);
	
	decomp = dec;
	encode = enc;
	
 	
	char *lclip = (char *)*bufferptr;
	//printf("In lift_config first byte 0x%x\n",lclip[0]);
	//printf("bpp %ld\n",bp);

	printf("size %ld ",imgsz);
	printf("pointer passed %x %x ",*bufferptr,bufferptr);


	/* Need to determine the ww width & hh height 
 	* given the ss bpp BPP bits per pixel & Size
 	*   

	if(bp==8) memory = (double)imgsz;
	else memory = (double)(imgsz/3.0);*/

	//width = (int)sqrt(memory);
	width = wim;
	
	//height = width;
	height = him;
	printf("width %d height %d \n",width,height);
	//sleep(15);
	//printf("local char ptr %x ",&lclip[0]);
 
	

	/*from test_tile_encoder*/
 
 
	TopDown = 0;
	
 	char *r,*g,*b;
	const char *octave_output_file_1;
	const char *octave_output_file_2;
	const char *octave_output_file_3;
	#define NUM_COMPS_MAX 4
	opj_cparameters_t l_param;
	opj_codec_t * l_codec;
	opj_image_t * l_image;
	opj_image_cmptparm_t l_params [NUM_COMPS_MAX];
	opj_stream_t * l_stream;
	OPJ_UINT32 l_nb_tiles;
	OPJ_UINT32 l_data_size;
	size_t len;

#ifdef USING_MCT
	const OPJ_FLOAT32 l_mct [] =
	{
		1 , 0 , 0 ,
		0 , 1 , 0 ,
		0 , 0 , 1
	};

	const OPJ_INT32 l_offsets [] =
	{
		128 , 128 , 128
	};
#endif

	opj_image_cmptparm_t * l_current_param_ptr;
	OPJ_UINT32 i;
	OPJ_BYTE *l_data;

  OPJ_UINT32 num_comps;
  int image_width;
  int image_height;
  int tile_width;
  int tile_height;
  int comp_prec;
  int irreversible;
  const char *output_file;

  
 

    num_comps = 3;
    image_width = width;
    image_height = height;
    tile_width = width;
    tile_height = height;
    comp_prec = 8;
    irreversible = FILTER;
    output_file = "test.j2k";
    
  if( num_comps > NUM_COMPS_MAX )
    {
	printf("num_comps > NUM_COMPS_MAX\n");	
    //return 1;
    }
	l_nb_tiles = (OPJ_UINT32)(image_width/tile_width) * (OPJ_UINT32)(image_height/tile_height);
	l_data_size = (OPJ_UINT32)tile_width * (OPJ_UINT32)tile_height * (OPJ_UINT32)num_comps * (OPJ_UINT32)(comp_prec/8);
	printf("l_nb_tiles %d l_data_size %d \n", l_nb_tiles, l_data_size);
	l_data = (OPJ_BYTE*) malloc(l_data_size * sizeof(OPJ_BYTE));
	if(l_data == NULL){
		printf("could not allocate the memory for l_data\n");
		//return 1;
	}	
	
	

	
	
	/*from test_tile_encoder*/
	
 
    /*
	r = malloc(sizeof(char)*height*width);
	g = malloc(sizeof(char)*height*width);
	b = malloc(sizeof(char)*height*width);
	printf("allocating rgb 0x%x 0x%x 0x%x \n",r,g,b);	 
	
	for (loop=0; loop < imgsz/3; loop++) {
			
		*b = lclip[0];
		lclip++;
		b++;
		*r = lclip[0];
		lclip++;
		r++;
		*g = lclip[0];
		lclip++;
		g++;
	}
		 
	printf("reseting pointers \n");
	r = r - (imgsz/3);
	g = g - (imgsz/3);
	b = b - (imgsz/3);
	*/
	for (i=0;i<((imgsz/3));i++)	{
		/*	
		l_data[i] = (OPJ_BYTE)g[i];
		l_data[i+(imgsz/3)] = (OPJ_BYTE)r[i];
		l_data[i+(imgsz/3)*2] = (OPJ_BYTE)b[i];	
		*/
		if(i==0) printf("0x%x 0x%x 0x%x\n", lclip[0],lclip[1],lclip[2]);
					
		/*green from Pascal*/
		l_data[i+(imgsz/3)*2]  = (OPJ_BYTE)lclip[0];
		lclip++;
						
		/*blue from Pascal*/
		l_data[i+(imgsz/3)] = (OPJ_BYTE)lclip[0];
		lclip++;

		/*red from Pascal*/
		l_data[i] = (OPJ_BYTE)lclip[0];
		lclip++;
	}
	b = l_data+(imgsz/3);
	g = l_data+(imgsz/3*2);
	r = l_data;
	if(plot == 1) {
			
		printf("write the files \n");
		printf("red-out.32t, grn-out.32t, and blu-out.32t\n");
		octave_output_file_1 = "red-out.32t";
			 
			
		i = octave_write_byte(octave_output_file_1,r , width*height);
		if(i == 0) printf("could not write file\n");
	
		octave_output_file_2 = "grn-out.32t";
		//i = octave_write(octave_output_file_2, imgbm->m_green, sz);
		i = octave_write_byte(octave_output_file_2, g, width*height);	
		if(i == 0) printf("could not write file\n");
	
		octave_output_file_3 = "blu-out.32t";
			 
		i = octave_write_byte(octave_output_file_3, b, width*height);
		if(i == 0) printf("could not write file\n");
		}

	/*
	printf("before reset 0x%x 0x%x 0x%x \n",r,g,b); 
	printf(" rgb 0x%x 0x%x 0x%x %d \n",r,g,b,plot);
	if(plot == 1) {
			
		printf("write the files \n");
		printf("red-out.32t, grn-out.32t, and blu-out.32t\n");
		octave_output_file_1 = "red-out.32t";
			 
			
		i = octave_write_byte(octave_output_file_1,r , width*height);
		if(i == 0) printf("could not write file\n");
	
		octave_output_file_2 = "grn-out.32t";
		//i = octave_write(octave_output_file_2, imgbm->m_green, sz);
		i = octave_write_byte(octave_output_file_2, g, width*height);	
		if(i == 0) printf("could not write file\n");
	
		octave_output_file_3 = "blu-out.32t";
			 
		i = octave_write_byte(octave_output_file_3, b, width*height);
		if(i == 0) printf("could not write file\n");
		}
		printf("FREE rgb 0x%x 0x%x 0x%x \n",r,g,b);
		free(r);
		free(g);
		free(b); 	 
 	*/
	lclip = (char *)*bufferptr;
 
 	
	gettimeofday(&start, NULL);
	/*from test_tile_encoder*/
	opj_set_default_encoder_parameters(&l_param);
	/** you may here add custom encoding parameters */
	/* rate specifications */
	/** number of quality layers in the stream */
	l_param.tcp_numlayers = 1;
	if ( flg == 0 ) {
		l_param.cp_disto_alloc = 1;
		l_param.tcp_rates[0] = CR;
	}
	else {
		l_param.cp_fixed_quality = 1;
		l_param.tcp_distoratio[0] = TCP_DISTORATIO;
	}
	/* is using others way of calculation */
	/* l_param.cp_disto_alloc = 1 or l_param.cp_fixed_alloc = 1 */
	/* l_param.tcp_rates[0] = ... */
	

	/* tile definitions parameters */
	/* position of the tile grid aligned with the image */
	l_param.cp_tx0 = 0;
	l_param.cp_ty0 = 0;
	/* tile size, we are using tile based encoding */
	l_param.tile_size_on = OPJ_TRUE;
	l_param.cp_tdx = tile_width;
	l_param.cp_tdy = tile_height;

	/* use irreversible encoding ?*/
	l_param.irreversible = irreversible;

	/* do not bother with mct, the rsiz is set when calling opj_set_MCT*/
	/*l_param.cp_rsiz = OPJ_STD_RSIZ;*/

	/* no cinema */
	/*l_param.cp_cinema = 0;*/

	/* no not bother using SOP or EPH markers, do not use custom size precinct */
	/* number of precincts to specify */
	/* l_param.csty = 0;*/
	/* l_param.res_spec = ... */
	/* l_param.prch_init[i] = .. */
	/* l_param.prcw_init[i] = .. */


	/* do not use progression order changes */
	/*l_param.numpocs = 0;*/
	/* l_param.POC[i].... */

	/* do not restrain the size for a component.*/
	/* l_param.max_comp_size = 0; */

	/** block encoding style for each component, do not use at the moment */
	/** J2K_CCP_CBLKSTY_TERMALL, J2K_CCP_CBLKSTY_LAZY, J2K_CCP_CBLKSTY_VSC, J2K_CCP_CBLKSTY_SEGSYM, J2K_CCP_CBLKSTY_RESET */
	/* l_param.mode = 0;*/

	/** number of resolutions */
	l_param.numresolution = decomp;

	/** progression order to use*/
	/** OPJ_LRCP, OPJ_RLCP, OPJ_RPCL, PCRL, CPRL */
	l_param.prog_order = OPJ_LRCP;

	/** no "region" of interest, more precisally component */
	/* l_param.roi_compno = -1; */
	/* l_param.roi_shift = 0; */

	/* we are not using multiple tile parts for a tile. */
	/* l_param.tp_on = 0; */
	/* l_param.tp_flag = 0; */

	/* if we are using mct */
#ifdef USING_MCT
	opj_set_MCT(&l_param,l_mct,l_offsets,NUM_COMPS);
#endif


	/* image definition */
	l_current_param_ptr = l_params;
	for (i=0;i<num_comps;++i) {
		/* do not bother bpp useless */
		/*l_current_param_ptr->bpp = COMP_PREC;*/
		l_current_param_ptr->dx = 1;
		l_current_param_ptr->dy = 1;

		l_current_param_ptr->h = (OPJ_UINT32)image_height;
		l_current_param_ptr->w = (OPJ_UINT32)image_width;

		l_current_param_ptr->sgnd = 0;
		l_current_param_ptr->prec = (OPJ_UINT32)comp_prec;

		l_current_param_ptr->x0 = 0;
		l_current_param_ptr->y0 = 0;

		++l_current_param_ptr;
	}

  /* should we do j2k or jp2 ?*/
  len = strlen( output_file );
  if( strcmp( output_file + len - 4, ".jp2" ) == 0 )
    {
    l_codec = opj_create_compress(OPJ_CODEC_JP2);
    }
  else
    {
		printf("In test_tile_encoder ");
		printf("creating J2k\n");
    l_codec = opj_create_compress(OPJ_CODEC_J2K);
    }
	if (!l_codec) {
		free(l_data);
		printf("no l_codec\n");
		//return 1;
	}

	/* catch events using our callbacks and give a local context */
	opj_set_info_handler(l_codec, info_callback,00);
	opj_set_warning_handler(l_codec, warning_callback,00);
	opj_set_error_handler(l_codec, error_callback,00);

	l_image = opj_image_tile_create(num_comps,l_params,OPJ_CLRSPC_SRGB);
	if (! l_image) {
		free(l_data);
		opj_destroy_codec(l_codec);
		printf("no l_image\n");
		//return 1;
	}

	l_image->x0 = 0;
	l_image->y0 = 0;
	l_image->x1 = (OPJ_UINT32)image_width;
	l_image->y1 = (OPJ_UINT32)image_height;
	l_image->color_space = OPJ_CLRSPC_SRGB;

	if (! opj_setup_encoder(l_codec,&l_param,l_image)) {
		fprintf(stderr, "ERROR -> test_tile_encoder: failed to setup the codec!\n");
		opj_destroy_codec(l_codec);
		opj_image_destroy(l_image);
		free(l_data);
		printf("no opj_setup_encoder\n");
		//return 1;
	}

    l_stream = opj_stream_create_default_file_stream(output_file, OPJ_FALSE);
    if (! l_stream) {
		fprintf(stderr, "ERROR -> test_tile_encoder: failed to create the stream from the output file %s !\n",output_file );
		opj_destroy_codec(l_codec);
		opj_image_destroy(l_image);
		free(l_data);
		printf("no l_stream\n");
		//return 1;
	}

	if (! opj_start_compress(l_codec,l_image,l_stream)) {
		fprintf(stderr, "ERROR -> test_tile_encoder: failed to start compress!\n");
        opj_stream_destroy(l_stream);
		opj_destroy_codec(l_codec);
		opj_image_destroy(l_image);
		free(l_data);
		printf("no opj_start_compress\n");
		//return 1;
	}
	gettimeofday(&end, NULL);

	seconds  = end.tv_sec  - start.tv_sec;
	useconds = end.tv_usec - start.tv_usec;
 
	mtime = seconds + useconds;
 
	printf("Compression time: %ld seconds %ld useconds %ld starting openjpeg\n", mtime,seconds, useconds);
	gettimeofday(&start, NULL);
	for (i=0;i<l_nb_tiles;++i) {
		if (! opj_write_tile(l_codec,i,l_data,l_data_size,l_stream)) {
			fprintf(stderr, "ERROR -> test_tile_encoder: failed to write the tile %d!\n",i);
            opj_stream_destroy(l_stream);
			opj_destroy_codec(l_codec);
			opj_image_destroy(l_image);
			free(l_data);
			printf("no opj_write_tile\nn");
			//return 1;
		}
	}

	if (! opj_end_compress(l_codec,l_stream)) {
		fprintf(stderr, "ERROR -> test_tile_encoder: failed to end compress!\n");
        opj_stream_destroy(l_stream);
		opj_destroy_codec(l_codec);
		opj_image_destroy(l_image);
		free(l_data);
		printf("no opj_end_compress\n");
		//return 1;
	}

    opj_stream_destroy(l_stream);
	opj_destroy_codec(l_codec);
	opj_image_destroy(l_image);
		
		/*from test_tile_encoder*/
 
	
		/* 
		for (loop=0; loop < imgsz/3; loop++) {
 
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
		*/
	 
	gettimeofday(&end, NULL);

	seconds  = end.tv_sec  - start.tv_sec;
	useconds = end.tv_usec - start.tv_usec;
 
	mtime = seconds + useconds;
	printf("Compression time: %ld seconds %ld useconds %ld \n",mtime,seconds,useconds);
 

 	 
}

void decom_test(int x0, int y0, int x1, int y1,char *ff_in) {
    const char *input_file;
    input_file = "dtest.j2k";
    
	printf("In decom_test called by Pascal %s %d %d %d %d %s\n",input_file,x0,y0,x1,y1,ff_in);
	decompress(x0, y0, x1, y1,ff_in);	
}
 
 
