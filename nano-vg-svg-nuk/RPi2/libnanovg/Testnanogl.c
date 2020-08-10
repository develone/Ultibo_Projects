/*
 * test.c
 *  
 * A simple C library to include in your Ultibo project
 *  
 */
/* Modification to nanovg files
 
  commented line  //#include <memory.h>
  from demo.c removed everything related to glfw and glew
              removed ifdef _MSC_VER to include <iconv.h> 

  functions moved to this file from demo.c
    saveScreenShot
	unpremultiplyAlpha
	setAlpha
	flipHorizontal
	saveScreenShot	
    All functions used to take/save ScreenShot, to avoid
    including more files, they were moved because of 
    glReadPixels 	

	rendergraph not used for simplicity included
	some functions to read gpu timer
    	

*/ 

//arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard -D__DYNAMIC_REENT__ -I opt/vc/include -c Testnanogl.c nanovg/src/nanovg.c demo.c
//arm-none-eabi-ar rcs libTestnanogl.a Testnanogl.o

//arm-none-eabi-gcc -O2 -mabi=aapcs -marm -march=armv6 -mfpu=vfp -mfloat-abi=hard -D__DYNAMIC_REENT__ -c Testnanogl.c nanovg/src/nanovg.c demo.c
 
#include <stdio.h>
#include <stdlib.h>

#include "/opt/vc/include/GLES/gl.h"
#include "/opt/vc/include/GLES/glext.h"
//#include "/opt/vc/include/GLES2/gl2.h"

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"
#include "nanovg.h"
#include "nanovg_gl.h"
#include <math.h> 
//Implemented in Ultibo
#ifdef __cplusplus
extern "C" {
#endif
 void glSwapBuffer();
 void getMouseXY(int *cx, int *cy, int *btc);
 void getScreenSize(unsigned long int *scrWidth, unsigned long int *scrHeight);

#ifdef __cplusplus
}
#endif


//used to be in demo.c
static int mini(int a, int b);
static void setAlpha(unsigned char* image, int w, int h, int stride, unsigned char a);
static void flipHorizontal(unsigned char* image, int w, int h, int stride);
void saveScreenShot(int w, int h, int premult, const char* name);


int PanelMouseX = 0;
int PanelMouseY = 0;
int ButtonsMouse = 0;

unsigned long int ScreenWidth = 800;
unsigned long int ScreenHeight = 480;

//----------------------------------------------------------------------------
typedef struct {
    float x, y;
} Vertex;

#define MAX_VERTICES 65536

Vertex vertices[MAX_VERTICES];
int n_vertices = 0;

void addVertex(float x, float y){
    Vertex v;
    
    v.x = x;
    v.y = y;
    
    vertices[n_vertices++] = v;
}

void drawVertices(GLenum mode, const void *vertices, int n_vertices){
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(2, GL_FLOAT, 0, vertices);
    glDrawArrays(mode, 0, n_vertices);
}

// Draw concave polygon using stencil buffer, technique from:
// "Drawing Filled, Concave Polygons Using the Stencil Buffer"
// OpenGl Programming Guide - The Official Guide to Learning OpenGL,
// Release 1, Eighth priting, May 1996
// Alternative techniques may use the color or depth buffer instead of
// the stencil buffer to accumulate polygon winding information.
// Make sure to request stencil buffer support on window creation!
void drawConcavePolygon(const void *vertices, int n_vertices){
    if (n_vertices == 0) return;
    
    // prepare stencil buffer
    glEnable(GL_STENCIL_TEST);
    glClear(GL_STENCIL_BUFFER_BIT);
    
    // set stencil buffer to invert value on draw, 0 to 1 and 1 to 0
    glStencilFunc(GL_ALWAYS, 0, 1);
    glStencilOp(GL_INVERT, GL_INVERT, GL_INVERT);
    
    // disable writing to color buffer
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
    
    // draw polygon into stencil buffer
    drawVertices(GL_TRIANGLE_FAN, vertices, n_vertices);
    
    // set stencil buffer to only keep pixels when value in buffer is 1
    glStencilFunc(GL_EQUAL, 1, 1);
    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
    
    // enable color again
    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
    
    // redraw polygon again, this time into color buffer.
    // alternatively, only draw bounding box of polygon
    drawVertices(GL_TRIANGLE_FAN, vertices, n_vertices);
    
    glDisable(GL_STENCIL_TEST);
}

void drawEyes(NVGcontext* vg, float x, float y, float w, float h, float mx, float my, float t)
{
	NVGpaint gloss, bg;
	float ex = w *0.23f;
	float ey = h * 0.5f;
	float lx = x + ex;
	float ly = y + ey;
	float rx = x + w - ex;
	float ry = y + ey;
	float dx,dy,d;
	float br = (ex < ey ? ex : ey) * 0.5f;
	float blink = 1 - pow(sinf(t*0.5f),200)*0.8f;

	bg = nvgLinearGradient(vg, x,y+h*0.5f,x+w*0.1f,y+h, nvgRGBA(0,0,0,32), nvgRGBA(0,0,0,16));
	nvgBeginPath(vg);
	nvgEllipse(vg, lx+3.0f,ly+16.0f, ex,ey);
	nvgEllipse(vg, rx+3.0f,ry+16.0f, ex,ey);
	nvgFillPaint(vg, bg);
	nvgFill(vg);

	bg = nvgLinearGradient(vg, x,y+h*0.25f,x+w*0.1f,y+h, nvgRGBA(220,220,220,255), nvgRGBA(128,128,128,255));
	nvgBeginPath(vg);
	nvgEllipse(vg, lx,ly, ex,ey);
	nvgEllipse(vg, rx,ry, ex,ey);
	nvgFillPaint(vg, bg);
	nvgFill(vg);

	dx = (mx - rx) / (ex * 10);
	dy = (my - ry) / (ey * 10);
	d = sqrtf(dx*dx+dy*dy);
	if (d > 1.0f) {
		dx /= d; dy /= d;
	}
	dx *= ex*0.4f;
	dy *= ey*0.5f;
	nvgBeginPath(vg);
	nvgEllipse(vg, lx+dx,ly+dy+ey*0.25f*(1-blink), br,br*blink);
	nvgFillColor(vg, nvgRGBA(32,32,32,255));
	nvgFill(vg);

	dx = (mx - rx) / (ex * 10);
	dy = (my - ry) / (ey * 10);
	d = sqrtf(dx*dx+dy*dy);
	if (d > 1.0f) {
		dx /= d; dy /= d;
	}
	dx *= ex*0.4f;
	dy *= ey*0.5f;
	nvgBeginPath(vg);
	nvgEllipse(vg, rx+dx,ry+dy+ey*0.25f*(1-blink), br,br*blink);
	nvgFillColor(vg, nvgRGBA(32,32,32,255));
	nvgFill(vg);

	gloss = nvgRadialGradient(vg, lx-ex*0.25f,ly-ey*0.5f, ex*0.1f,ex*0.75f, nvgRGBA(255,255,255,128), nvgRGBA(255,255,255,0));
	nvgBeginPath(vg);
	nvgEllipse(vg, lx,ly, ex,ey);
	nvgFillPaint(vg, gloss);
	nvgFill(vg);

	gloss = nvgRadialGradient(vg, rx-ex*0.25f,ry-ey*0.5f, ex*0.1f,ex*0.75f, nvgRGBA(255,255,255,128), nvgRGBA(255,255,255,0));
	nvgBeginPath(vg);
	nvgEllipse(vg, rx,ry, ex,ey);
	nvgFillPaint(vg, gloss);
	nvgFill(vg);
}

void test ()
{
    NVGcontext* vg = NULL;
  int PrevX = 0, PrevY = 0;
  getScreenSize(&ScreenWidth, &ScreenHeight);
  
    glClearColor(0.5f, 0.5f, 0.5f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
	glOrthof(0, ScreenWidth, ScreenHeight, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glSwapBuffer();
  
  while(1)
  {
	
    glClearColor(0.5f, 0.5f, 0.5f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrthof(0, ScreenWidth, ScreenHeight, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    
    drawConcavePolygon(vertices, n_vertices);	
        

	if (ButtonsMouse == 2) {
        ButtonsMouse=0; 			
		saveScreenShot(ScreenWidth, ScreenHeight, 1, "dump.png");
	}
	
    if (ButtonsMouse == 1){
        n_vertices = 0;

        addVertex(PanelMouseX, PanelMouseY);
    }	
	
	if(ButtonsMouse == 4)
	{	 
	 if (n_vertices >= MAX_VERTICES)
	     n_vertices = 0;
	
	 if((PrevX != PanelMouseX) || (PrevY != PanelMouseY))		 
      addVertex(PrevX, PrevY);	
	 
	 PrevX = PanelMouseX;
     PrevY = PanelMouseY;	  
	} 
	 
	getMouseXY(&PanelMouseX, &PanelMouseY, &ButtonsMouse);	
    glSwapBuffer();
  }
  
 
  
 
}


static int mini(int a, int b) { return a < b ? a : b; }

static void unpremultiplyAlpha(unsigned char* image, int w, int h, int stride)
{
	int x,y;

	// Unpremultiply
	for (y = 0; y < h; y++) {
		unsigned char *row = &image[y*stride];
		for (x = 0; x < w; x++) {
			int r = row[0], g = row[1], b = row[2], a = row[3];
			if (a != 0) {
				row[0] = (int)mini(r*255/a, 255);
				row[1] = (int)mini(g*255/a, 255);
				row[2] = (int)mini(b*255/a, 255);
			}
			row += 4;
		}
	}

	// Defringe
	for (y = 0; y < h; y++) {
		unsigned char *row = &image[y*stride];
		for (x = 0; x < w; x++) {
			int r = 0, g = 0, b = 0, a = row[3], n = 0;
			if (a == 0) {
				if (x-1 > 0 && row[-1] != 0) {
					r += row[-4];
					g += row[-3];
					b += row[-2];
					n++;
				}
				if (x+1 < w && row[7] != 0) {
					r += row[4];
					g += row[5];
					b += row[6];
					n++;
				}
				if (y-1 > 0 && row[-stride+3] != 0) {
					r += row[-stride];
					g += row[-stride+1];
					b += row[-stride+2];
					n++;
				}
				if (y+1 < h && row[stride+3] != 0) {
					r += row[stride];
					g += row[stride+1];
					b += row[stride+2];
					n++;
				}
				if (n > 0) {
					row[0] = r/n;
					row[1] = g/n;
					row[2] = b/n;
				}
			}
			row += 4;
		}
	}
}

static void setAlpha(unsigned char* image, int w, int h, int stride, unsigned char a)
{
	int x, y;
	for (y = 0; y < h; y++) {
		unsigned char* row = &image[y*stride];
		for (x = 0; x < w; x++)
			row[x*4+3] = a;
	}
}

static void flipHorizontal(unsigned char* image, int w, int h, int stride)
{
	int i = 0, j = h-1, k;
	while (i < j) {
		unsigned char* ri = &image[i * stride];
		unsigned char* rj = &image[j * stride];
		for (k = 0; k < w*4; k++) {
			unsigned char t = ri[k];
			ri[k] = rj[k];
			rj[k] = t;
		}
		i++;
		j--;
	}
}

void saveScreenShot(int w, int h, int premult, const char* name)
{
	unsigned char* image = (unsigned char*)malloc(w*h*4);
	if (image == NULL)
		return;
	glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, image);
	if (premult)
		unpremultiplyAlpha(image, w, h, w*4);
	else
		setAlpha(image, w, h, w*4, 255);
	flipHorizontal(image, w, h, w*4);
 	stbi_write_png(name, w, h, 4, image, w*4);
 	free(image);
}
