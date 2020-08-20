/*
 * svgTessellator.c
 *  
 * 
 *  
 */
 
#include <stdio.h>

#include "/opt/vc/include/GLES/gl.h"
#include "/opt/vc/include/GLES2/gl2.h"


#define NK_INCLUDE_FIXED_TYPES
#define NK_INCLUDE_STANDARD_IO
#define NK_INCLUDE_STANDARD_VARARGS
#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_INCLUDE_VERTEX_BUFFER_OUTPUT
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#define NK_IMPLEMENTATION

#define WINDOW_WIDTH ScreenWidth
#define WINDOW_HEIGHT ScreenHeight

//#define MAX_VERTEX_MEMORY 512 * 1024
//#define MAX_ELEMENT_MEMORY 128 * 1024

#define MAX_VERTEX_MEMORY 256 * 1024
#define MAX_ELEMENT_MEMORY 64 * 1024

#define UNUSED(a) (void)a
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#define LEN(a) (sizeof(a)/sizeof(a)[0])


#define NK_ULTIBO_GLES2_IMPLEMENTATION
#include "nuklear/nuklear.h"
#include "nuklear_ultibo_gles2.h"

#define NANOSVG_ALL_COLOR_KEYWORDS	// Include full list of color keywords.
#define NANOSVG_IMPLEMENTATION		// Expands implementation
#include "nanosvg/src/nanosvg.h"

#include "libtess2/Include/tesselator.h"

typedef struct svgContainer {
   GLint start;
   GLint end;
   GLfloat transforms[4][4];
 } svgContainer;

 svgContainer svgFileLoadTest;

int resetsvgCont(svgContainer *svgFileContainer);
int scalesvgCont(svgContainer *svgFileContainer, GLfloat x, GLfloat y);
int translatesvgCont(svgContainer *svgFileContainer, GLfloat x, GLfloat y);
int rotatesvgCont(svgContainer *svgFileContainer, GLfloat angle);
int LoadsvgFile( svgContainer *svgFileContainer, GLint oglVertexVBuff,
                GLint oglVertexCBuff, GLint oglElementBuff, char* filename);

// From ultibo
#ifdef __cplusplus
extern "C" {
#endif
  void glSwapBuffer();
  void getMouseXY(int *cx, int *cy, int *btc);
  void getScreenSize(unsigned long int *scrWidth, unsigned long int *scrHeight);
  void getKey(int *value);

#ifdef __cplusplus
}
#endif

int PanelMouseX = 0;
int PanelMouseY = 0;
int ButtonsMouse = 0;

unsigned long int ScreenWidth = 800;
unsigned long int ScreenHeight = 480;

int BUTTON_LEFT  = 0; 
int BUTTON_RIGHT  = 0; 
int BUTTON_MIDDLE = 0;

//------------------------------------------

// gui context, manages, input , drawing 
// everything
struct nk_context *ctx;
char CoordBuufer[10] = {0};
char buf[256] = {0};

// gui code
//static void
//MainLoop(void* loopArg);
static void nuklear_MainLoop(void* loopArg);
//-------------------------------------------------------------------------------------------------


const GLchar *vertexShaderSource =
 "attribute vec2 vPosition;                             \n"
 "attribute vec4 vColor;                                \n" 
 "varying vec4 vertColor;                               \n"
 "uniform mat4 orthoproj;                               \n"
 "uniform mat4 rotation;                                \n"
 "uniform mat4 transforms;                              \n"
 "void main()                                           \n"
 "{                                                     \n"
 " vertColor = vColor;                                  \n"//vec4(1.0, 0.0, 1.0, 1.0);                                  \n" 
 " gl_Position = orthoproj * transforms * rotation * vec4(vPosition.x, vPosition.y, 0, 1) ; \n"
 "} \n";

 const GLchar *fragmentShaderSource =
 "precision mediump float; \n"
 "varying vec4 vertColor;                      \n" 
 "void main() \n"
 "{ \n"
 " gl_FragColor = vertColor; \n"
 "} \n";				

GLuint VBO1,VBO2, EBO1;
int shaderProgram ;

void ultibo_C_main()
{
    getScreenSize(&ScreenWidth, &ScreenHeight);
	
 GLfloat ortho[4][4] = {
        {2.0f, 0.0f, 0.0f, 0.0f},
        {0.0f,-2.0f, 0.0f, 0.0f},
        {0.0f, 0.0f,-1.0f, 0.0f},
        {-1.0f,1.0f, 0.0f, 1.0f},
 };

 ortho[0][0] /= (GLfloat)ScreenWidth;
 ortho[1][1] /= (GLfloat)ScreenHeight;	

    // build and compile our shader program
    // ------------------------------------
    // vertex shader
    int vertexShader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertexShader, 1, &vertexShaderSource, NULL);
    glCompileShader(vertexShader);
    // check for shader compile errors
    int success;
    char infoLog[512];
    glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
        //std::cout << "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n" << infoLog << std::endl;
    }
    // fragment shader
    int fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader, 1, &fragmentShaderSource, NULL);
    glCompileShader(fragmentShader);
    // check for shader compile errors
    glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
    if (!success)
    {
        glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
        //std::cout << "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n" << infoLog << std::endl;
    }
    // link shaders
    shaderProgram = glCreateProgram();
    glAttachShader(shaderProgram, vertexShader);
    glAttachShader(shaderProgram, fragmentShader);
    glLinkProgram(shaderProgram);
    // check for linking errors
    glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
    if (!success) {
        glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
        //std::cout << "ERROR::SHADER::PROGRAM::LINKING_FAILED\n" << infoLog << std::endl;
    }
    glDeleteShader(vertexShader);
    glDeleteShader(fragmentShader);
	
	//glBindAttribLocation ( shaderProgram, 0, "vPosition" );
	GLuint positionLoc = glGetAttribLocation(shaderProgram, "vPosition");
	GLuint colorLoc    = glGetAttribLocation(shaderProgram, "vColor");

    glGenBuffers(1, &VBO1);
	glGenBuffers(1, &VBO2);
    glGenBuffers(1, &EBO1);
	
    glBindBuffer(GL_ARRAY_BUFFER, VBO1);
    glBufferData(GL_ARRAY_BUFFER, (186384*2) * sizeof(float), NULL, GL_STREAM_DRAW);

    glBindBuffer(GL_ARRAY_BUFFER, VBO2);  
    glBufferData(GL_ARRAY_BUFFER, (186384*2) * sizeof(float), NULL, GL_STREAM_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO1);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, (186384) * sizeof(GLshort), NULL, GL_STREAM_DRAW);	

    LoadsvgFile( &svgFileLoadTest, VBO1, VBO2, EBO1, "example\\23ftCenter.svg" );
     ///--------------------------------------------nuklear init
	 
    ctx = nk_ultibo_init();
    /* Load Fonts: if none of these are loaded a default font will be used  */
    /* Load Cursor: if you uncomment cursor loading please hide the cursor */
    {struct nk_font_atlas *atlas;
    nk_ultibo_font_stash_begin(&atlas);   //left here you can test more fonts
    /*struct nk_font *droid = nk_font_atlas_add_from_file(atlas, "../../../extra_font/DroidSans.ttf", 14, 0);*/
    struct nk_font *roboto = nk_font_atlas_add_from_file(atlas, "nuklear\\extra_font\\Roboto-Regular.ttf", 16, 0);
    //struct nk_font *future = nk_font_atlas_add_from_file(atlas, "Nuklear/extra_font/kenvector_future_thin.ttf", 13, 0);
    /*struct nk_font *clean = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyClean.ttf", 12, 0);*/
    /*struct nk_font *tiny = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyTiny.ttf", 10, 0);*/
    /*struct nk_font *cousine = nk_font_atlas_add_from_file(atlas, "../../../extra_font/Cousine-Regular.ttf", 13, 0);*/
    nk_ultibo_font_stash_end();
    /*nk_style_load_all_cursors(ctx, atlas->cursors);*/
    nk_style_set_font(ctx, &roboto->handle);}	 
	 
	 ///--------------------------------------------------------
	 GLint rotation = 0;
 
	while(1)
	{
GLfloat rotate[4][4] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 1.0f},
    };

    GLfloat rad_angle = rotation * M_PI/180;

    rotate[0][0] = cos(rad_angle);
    rotate[1][0] = -sin(rad_angle);
    rotate[0][1] = sin(rad_angle);
    rotate[1][1] = cos(rad_angle);		
		
        glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
        //glClear(GL_COLOR_BUFFER_BIT);
		glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
		
	  getMouseXY(&PanelMouseX, &PanelMouseY, &ButtonsMouse);

    nuklear_MainLoop((void*)ctx); // here is the gui code			

		
		if(ButtonsMouse == 2)
		{	
	     rotation+=15;         
		}

		if(ButtonsMouse == 3)
		{	
	     rotation=0;          
		}
		
		rotatesvgCont(&svgFileLoadTest, rotation);

        translatesvgCont(&svgFileLoadTest, (GLfloat)PanelMouseX, (GLfloat)PanelMouseY);

        //if(ButtonsMouse == 1)
		//scalesvgCont(&svgFileLoadTest,(GLfloat)2.5,(GLfloat)2.5);			
		//else
		//scalesvgCont(&svgFileLoadTest,(GLfloat)1.0f,(GLfloat)1.0f);		


        glUseProgram(shaderProgram);
		GLint uniTrans = glGetUniformLocation(shaderProgram, "orthoproj");
		GLint mulTrans = glGetUniformLocation(shaderProgram, "transforms");
		GLint rotTrans = glGetUniformLocation(shaderProgram, "rotation");
        glUniformMatrix4fv(uniTrans, 1, GL_FALSE, &ortho[0][0]);
		glUniformMatrix4fv(rotTrans, 1, GL_FALSE, &rotate[0][0]);//&svgFileLoadTest.transforms[0][0]);
		glUniformMatrix4fv(mulTrans, 1, GL_FALSE, &svgFileLoadTest.transforms[0][0]);
		
		glBindBuffer(GL_ARRAY_BUFFER, VBO1);
		glVertexAttribPointer(positionLoc, 2, GL_FLOAT, GL_FALSE, 0, (void*)0);
        glEnableVertexAttribArray(positionLoc);
		
		glBindBuffer(GL_ARRAY_BUFFER, VBO2);		
		glVertexAttribPointer(colorLoc, 4, GL_FLOAT, GL_FALSE, 0, (void*)0);
        glEnableVertexAttribArray(colorLoc);
		
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, EBO1);
        //glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, 0);		
		glDrawElements(GL_TRIANGLES,svgFileLoadTest.start , GL_UNSIGNED_SHORT,(void*)svgFileLoadTest.end);
        glUseProgram(0);
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
        glDisable(GL_BLEND);
        glDisable(GL_SCISSOR_TEST);		
		
        
	  glSwapBuffer();
 
	}
	
    nk_ultibo_shutdown(); // not needed but to show where it goes
 
}


int LoadsvgFile( svgContainer *svgFileContainer, GLint oglVertexVBuff,
                GLint oglVertexCBuff, GLint oglElementBuff, char* filename)
{
  static unsigned int vertexoffset = 0;
  static unsigned int indexoffset = 0;
  static unsigned int indexTotalCount = 0;

  GLfloat identity [4][4] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 1.0f},
    };
  //static int count = 0;

  NSVGshape * shape;
  NSVGpath  * path;
  NSVGimage * SVGFile;

  SVGFile = nsvgParseFromFile(filename , "px", 96);
  //----------------------------------------------------------------------------
      //here for the starting vertexoffset 0
  svgFileContainer->end   = vertexoffset;

  memcpy(svgFileContainer->transforms,identity, 4*4*sizeof(GLfloat));

for (shape = SVGFile->shapes; shape != NULL; shape = shape->next) {

 TESStesselator *tessellator = tessNewTess(NULL);
 tessSetOption(tessellator, TESS_CONSTRAINED_DELAUNAY_TRIANGULATION, 1);

 for(path = shape->paths; path != NULL; path = path->next) {

   tessAddContour(tessellator, 2, path->pts , sizeof(float)*2, path->npts);
 }

 tessTesselate(tessellator, TESS_WINDING_ODD, TESS_POLYGONS, 3, 2, NULL);

 int vertexCount = tessGetVertexCount(tessellator);
 const TESSreal *vertices = tessGetVertices(tessellator);
 int indexCount = tessGetElementCount(tessellator);
 TESSindex *indices = (int*)tessGetElements(tessellator);
 //GLshort *indices = (GLshort*)tessGetElements(tessellator);
 GLushort *indiceshort = (GLushort*)calloc(indexCount*3,sizeof(GLushort));

 float *vertexColors = (float*)calloc(vertexCount*4,sizeof(float));

 for(int i = 0; i < vertexCount*4; i+=4)
 {
  vertexColors[i] = (float)(shape->fill.color & 0xff)/255;
  vertexColors[i+1] = (float)((shape->fill.color >> 8) & 0xff)/255;
  vertexColors[i+2] = (float)((shape->fill.color >> 16) & 0xff)/255;
  vertexColors[i+3] = (float)((shape->fill.color >> 24) & 0xff)/255;
 }

 for(int i =0 ; i < indexCount*3; i++)
 {
  indiceshort[i] = indices[i] + (vertexoffset>>1);
 }
 
 glUseProgram(shaderProgram);
 GLuint positionLoc = glGetAttribLocation(shaderProgram, "vPosition");
 GLuint colorLoc    = glGetAttribLocation(shaderProgram, "vColor"); 


 glBindBuffer(GL_ARRAY_BUFFER, oglVertexVBuff);
 glBufferSubData(GL_ARRAY_BUFFER, vertexoffset * sizeof(float), (vertexCount*2) * sizeof(float), vertices);
 glVertexAttribPointer(positionLoc, 2, GL_FLOAT, GL_FALSE, 0, (void*)0);
 glEnableVertexAttribArray(positionLoc);

 glBindBuffer(GL_ARRAY_BUFFER, oglVertexCBuff);
 glBufferSubData(GL_ARRAY_BUFFER, vertexoffset * 8, (vertexCount*4) * sizeof(float), vertexColors);
 glVertexAttribPointer(colorLoc, 4, GL_FLOAT, GL_FALSE, 0, (void*)0);
 glEnableVertexAttribArray(colorLoc);

 //glBindBuffer(GL_ARRAY_BUFFER, 0);
 //glBindVertexArray(0);

 glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, oglElementBuff);
 glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, indexoffset * sizeof(GLshort), indexCount*6, indiceshort);


 vertexoffset += (vertexCount*2);
 indexoffset  += (indexCount*3);

 indexTotalCount += (indexCount*3);

 //count++;

 tessDeleteTess(tessellator);
 free(vertexColors);
 free(indiceshort);
}
  svgFileContainer->start = indexTotalCount;



  //-----------------------------------------------------------------------------
  free(shape);
  free(path);
  nsvgDelete(SVGFile);

}

int rotatesvgCont(svgContainer *svgFileContainer, GLfloat angle)
{
  GLfloat identity [4][4] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 1.0f},
    };
  
   memcpy(svgFileContainer->transforms,identity, 4*4*sizeof(GLfloat));	 
	
  GLfloat rad_angle = angle * (M_PI/180);

  svgFileContainer->transforms[0][0] =  cosf(rad_angle);
  svgFileContainer->transforms[1][0] = -sinf(rad_angle);
  svgFileContainer->transforms[0][1] =  sinf(rad_angle);
  svgFileContainer->transforms[1][1] =  cosf(rad_angle);
}


int translatesvgCont(svgContainer *svgFileContainer, GLfloat x, GLfloat y)
{
  GLfloat identity [4][4] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 1.0f},
    };
  
   memcpy(svgFileContainer->transforms,identity, 4*4*sizeof(GLfloat));	
	
  svgFileContainer->transforms[3][0] = x;
  svgFileContainer->transforms[3][1] = y;
}


int scalesvgCont(svgContainer *svgFileContainer, GLfloat x, GLfloat y)
{
  svgFileContainer->transforms[0][0] = x;
  svgFileContainer->transforms[1][1] = y;
}

int resetsvgCont(svgContainer *svgFileContainer)
{
  GLfloat identity [4][4] = {
        {1.0f, 0.0f, 0.0f, 0.0f},
        {0.0f, 1.0f, 0.0f, 0.0f},
        {0.0f, 0.0f, 1.0f, 0.0f},
        {0.0f, 0.0f, 0.0f, 1.0f},
    };

  memcpy(svgFileContainer->transforms,identity, 4*4*sizeof(GLfloat));

}


//------------------------------------------------------------------------------------------------



static void nuklear_MainLoop(void* loopArg){
    
	struct nk_context *ctx = (struct nk_context *)loopArg;

    static float value = 100.0f;
	
    
	/*Mouse*/
	// needs a much better code
	// works for now
	switch(ButtonsMouse)
	{
		case 1 : BUTTON_LEFT   = 1; break;
		case 2 : BUTTON_RIGHT  = 1; break;
		case 4 : BUTTON_MIDDLE = 1; break;
		default :  
		          BUTTON_LEFT  = 0; 
		          BUTTON_RIGHT  = 0; 
		          BUTTON_MIDDLE = 0;
		          break;
	}
	
	nk_input_begin(ctx);
    nk_input_motion(ctx, (int)PanelMouseX,(int)PanelMouseY);	
    nk_input_button(ctx, NK_BUTTON_LEFT, (int)PanelMouseX, (int)PanelMouseY, BUTTON_LEFT);
    nk_input_button(ctx, NK_BUTTON_RIGHT, (int)PanelMouseX,(int)PanelMouseY, BUTTON_RIGHT);
    nk_input_button(ctx, NK_BUTTON_MIDDLE, (int)PanelMouseX,(int)PanelMouseY, BUTTON_MIDDLE);
    nk_input_end(ctx);	

    /* GUI */
    if (nk_begin(ctx, "Demo", nk_rect(50, 50, 200, 260),
        NK_WINDOW_BORDER|NK_WINDOW_MOVABLE|NK_WINDOW_SCALABLE|
        NK_WINDOW_CLOSABLE|NK_WINDOW_MINIMIZABLE|NK_WINDOW_TITLE))
    {
        nk_menubar_begin(ctx);
        nk_layout_row_begin(ctx, NK_STATIC, 25, 2);
        nk_layout_row_push(ctx, 45);
        if (nk_menu_begin_label(ctx, "FILE", NK_TEXT_LEFT, nk_vec2(120, 200))) {
            nk_layout_row_dynamic(ctx, 30, 1);
            nk_menu_item_label(ctx, "OPEN", NK_TEXT_LEFT);
            nk_menu_item_label(ctx, "CLOSE", NK_TEXT_LEFT);
            nk_menu_end(ctx);
        }
        nk_layout_row_push(ctx, 45);
        if (nk_menu_begin_label(ctx, "EDIT", NK_TEXT_LEFT, nk_vec2(120, 200))) {
            nk_layout_row_dynamic(ctx, 30, 1);
            nk_menu_item_label(ctx, "COPY", NK_TEXT_LEFT);
            nk_menu_item_label(ctx, "CUT", NK_TEXT_LEFT);
            nk_menu_item_label(ctx, "PASTE", NK_TEXT_LEFT);
            nk_menu_end(ctx);
        }
        nk_layout_row_end(ctx);
        nk_menubar_end(ctx);

        enum {EASY, HARD};
        static int op = EASY;
        static int property = 20;
        nk_layout_row_static(ctx, 30, 80, 1);
        if (nk_button_label(ctx, "button"))
            fprintf(stdout, "button pressed\n");
        nk_layout_row_dynamic(ctx, 30, 2);
        if (nk_option_label(ctx, "easy", op == EASY)) op = EASY;
        if (nk_option_label(ctx, "hard", op == HARD)) op = HARD;
        nk_layout_row_dynamic(ctx, 25, 1);
		
        itoa(PanelMouseX,CoordBuufer,10);
        nk_label(ctx, "X:", NK_TEXT_LEFT);
        nk_label(ctx, CoordBuufer, NK_TEXT_LEFT);
		
        itoa(PanelMouseY,CoordBuufer,10);
        nk_label(ctx, "Y:", NK_TEXT_LEFT);
        nk_label(ctx, CoordBuufer, NK_TEXT_LEFT);
		
        nk_property_int(ctx, "Compression:", 0, &property, 100, 10, 1);

        nk_edit_string_zero_terminated (ctx, NK_EDIT_FIELD, buf, sizeof(buf) - 1, nk_filter_default);
        if (nk_button_label (ctx, "Done"))
	        printf ("%s\n", buf);

        /* custom widget pixel width */
        nk_layout_row_begin(ctx, NK_STATIC, 30, 2);
        {
          nk_layout_row_push(ctx, 50);
          nk_label(ctx, "Volume:", NK_TEXT_LEFT);
          nk_layout_row_push(ctx, 110);
          nk_slider_float(ctx, 0, &value, 100.0f, 1.0f);          
        }
        nk_layout_row_end(ctx);

    }
    nk_end(ctx);


    if (nk_begin(ctx, "Demo02", nk_rect(260, 50, 200, 200),
        /*NK_WINDOW_BORDER|*/NK_WINDOW_MOVABLE|NK_WINDOW_SCALABLE|
        /*NK_WINDOW_CLOSABLE|*/NK_WINDOW_MINIMIZABLE|NK_WINDOW_TITLE))
    {

    }
    nk_end(ctx);

    /* Draw */
    float bg[4];
    int win_width, win_height;
    nk_color_fv(bg, nk_rgb(28,48,62));

    glViewport(0, 0, ScreenWidth, ScreenHeight);
    glClear(GL_COLOR_BUFFER_BIT);
    //glClearColor(bg[0], bg[1], bg[2], bg[3]);
	glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    /* IMPORTANT: `nk_sdl_render` modifies some global OpenGL state
     * with blending, scissor, face culling, depth test and viewport and
     * defaults everything back into a default state.
     * Make sure to either a.) save and restore or b.) reset your own state after
     * rendering the UI. */
    nk_ultibo_render(NK_ANTI_ALIASING_ON, MAX_VERTEX_MEMORY, MAX_ELEMENT_MEMORY, ScreenWidth, ScreenHeight);

    
	
}


