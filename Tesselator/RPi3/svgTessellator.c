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

#include "nuklear/nuklear.h"

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
struct nk_gen_device {
    struct nk_buffer cmds;
    struct nk_draw_null_texture null;
    GLuint vbo, ebo;
    GLuint prog;
    GLuint vert_shdr;
    GLuint frag_shdr;
    GLint attrib_pos;
    GLint attrib_uv;
    GLint attrib_col;
    GLint uniform_tex;
    GLint uniform_proj;
    GLuint font_tex;
    GLsizei vs;
    size_t vp, vt, vc;
};

struct nk_gen_vertex {
    GLfloat position[2];
    GLfloat uv[2];
    nk_byte col[4];
};

static struct nk_gen {
    //SDL_Window *win;
    struct nk_gen_device ogl;
    struct nk_context ctx;
    struct nk_font_atlas atlas;
} gen;

NK_API void
nk_gen_device_create(void);

NK_API void
nk_gen_device_destroy(void);

NK_INTERN void
nk_gen_device_upload_atlas(const void *image, int width, int height);

NK_API void
nk_gen_render(enum nk_anti_aliasing AA, int max_vertex_buffer, int max_element_buffer, int rwidth, int rheight);

NK_API struct nk_context*
nk_gen_init();

NK_API void
nk_gen_font_stash_begin(struct nk_font_atlas **atlas);

NK_API void
nk_gen_font_stash_end(void);

NK_API
void nk_gen_shutdown(void);

// gui context, manages, input , drawing 
// everything
struct nk_context *ctx;
char CoordBuufer[10] = {0};
char buf[256] = {0};

// gui code
static void
MainLoop(void* loopArg);
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

void svgTessellator_main()
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
	 
    ctx = nk_gen_init();
    /* Load Fonts: if none of these are loaded a default font will be used  */
    /* Load Cursor: if you uncomment cursor loading please hide the cursor */
    {struct nk_font_atlas *atlas;
    nk_gen_font_stash_begin(&atlas);   //left here you can test more fonts
    /*struct nk_font *droid = nk_font_atlas_add_from_file(atlas, "../../../extra_font/DroidSans.ttf", 14, 0);*/
    struct nk_font *roboto = nk_font_atlas_add_from_file(atlas, "nuklear\\extra_font\\Roboto-Regular.ttf", 16, 0);
    //struct nk_font *future = nk_font_atlas_add_from_file(atlas, "Nuklear/extra_font/kenvector_future_thin.ttf", 13, 0);
    /*struct nk_font *clean = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyClean.ttf", 12, 0);*/
    /*struct nk_font *tiny = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyTiny.ttf", 10, 0);*/
    /*struct nk_font *cousine = nk_font_atlas_add_from_file(atlas, "../../../extra_font/Cousine-Regular.ttf", 13, 0);*/
    nk_gen_font_stash_end();
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
        MainLoop((void*)ctx); // here is the gui code		
		
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
	
    nk_gen_shutdown(); // not needed but to show where it goes
 
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


static void
MainLoop(void* loopArg){
    
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
    nk_gen_render(NK_ANTI_ALIASING_ON, MAX_VERTEX_MEMORY, MAX_ELEMENT_MEMORY, ScreenWidth, ScreenHeight);

    
	
}

#define NK_SHADER_VERSION "#version 100\n"


NK_API void
nk_gen_device_create(void)
{
    GLint status;
    static const GLchar *vertex_shader =
        NK_SHADER_VERSION
        "uniform mat4 ProjMtx;\n"
        "attribute vec2 Position;\n"
        "attribute vec2 TexCoord;\n"
        "attribute vec4 Color;\n"
        "varying vec2 Frag_UV;\n"
        "varying vec4 Frag_Color;\n"
        "void main() {\n"
        "   Frag_UV = TexCoord;\n"
        "   Frag_Color = Color;\n"
        "   gl_Position = ProjMtx * vec4(Position.xy, 0, 1);\n"
        "}\n";
    static const GLchar *fragment_shader =
        NK_SHADER_VERSION
        "precision mediump float;\n"
        "uniform sampler2D Texture;\n"
        "varying vec2 Frag_UV;\n"
        "varying vec4 Frag_Color;\n"
        "void main(){\n"
        "   gl_FragColor = Frag_Color * texture2D(Texture, Frag_UV);\n"
        "}\n";

    struct nk_gen_device *dev = &gen.ogl;
    
    nk_buffer_init_default(&dev->cmds);
    dev->prog = glCreateProgram();
    dev->vert_shdr = glCreateShader(GL_VERTEX_SHADER);
    dev->frag_shdr = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(dev->vert_shdr, 1, &vertex_shader, 0);
    glShaderSource(dev->frag_shdr, 1, &fragment_shader, 0);
    glCompileShader(dev->vert_shdr);
    glCompileShader(dev->frag_shdr);
    glGetShaderiv(dev->vert_shdr, GL_COMPILE_STATUS, &status);
    assert(status == GL_TRUE);
    glGetShaderiv(dev->frag_shdr, GL_COMPILE_STATUS, &status);
    assert(status == GL_TRUE);
    glAttachShader(dev->prog, dev->vert_shdr);
    glAttachShader(dev->prog, dev->frag_shdr);
    glLinkProgram(dev->prog);
    glGetProgramiv(dev->prog, GL_LINK_STATUS, &status);
    assert(status == GL_TRUE);


    dev->uniform_tex = glGetUniformLocation(dev->prog, "Texture");
    dev->uniform_proj = glGetUniformLocation(dev->prog, "ProjMtx");
    dev->attrib_pos = glGetAttribLocation(dev->prog, "Position");
    dev->attrib_uv = glGetAttribLocation(dev->prog, "TexCoord");
    dev->attrib_col = glGetAttribLocation(dev->prog, "Color");
    {
        dev->vs = sizeof(struct nk_gen_vertex);
        dev->vp = offsetof(struct nk_gen_vertex, position);
        dev->vt = offsetof(struct nk_gen_vertex, uv);
        dev->vc = offsetof(struct nk_gen_vertex, col);
        
        /* Allocate buffers */
        glGenBuffers(1, &dev->vbo);
        glGenBuffers(1, &dev->ebo);
    }
    glBindTexture(GL_TEXTURE_2D, 0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
}

NK_INTERN void
nk_gen_device_upload_atlas(const void *image, int width, int height)
{
    struct nk_gen_device *dev = &gen.ogl;
    glGenTextures(1, &dev->font_tex);
    glBindTexture(GL_TEXTURE_2D, dev->font_tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, (GLsizei)width, (GLsizei)height, 0,
                GL_RGBA, GL_UNSIGNED_BYTE, image);
}

NK_API void
nk_gen_device_destroy(void)
{
    struct nk_gen_device *dev = &gen.ogl;
    glDetachShader(dev->prog, dev->vert_shdr);
    glDetachShader(dev->prog, dev->frag_shdr);
    glDeleteShader(dev->vert_shdr);
    glDeleteShader(dev->frag_shdr);
    glDeleteProgram(dev->prog);
    glDeleteTextures(1, &dev->font_tex);
    glDeleteBuffers(1, &dev->vbo);
    glDeleteBuffers(1, &dev->ebo);
    nk_buffer_free(&dev->cmds);
}

NK_API void
nk_gen_render(enum nk_anti_aliasing AA, int max_vertex_buffer, int max_element_buffer, int rwidth, int rheight)
{
    struct nk_gen_device *dev = &gen.ogl;    
    int display_width= rwidth, display_height = rheight;
    struct nk_vec2 scale;
    GLfloat ortho[4][4] = {
        {2.0f, 0.0f, 0.0f, 0.0f},
        {0.0f,-2.0f, 0.0f, 0.0f},
        {0.0f, 0.0f,-1.0f, 0.0f},
        {-1.0f,1.0f, 0.0f, 1.0f},
    };

    ortho[0][0] /= (GLfloat)rwidth;
    ortho[1][1] /= (GLfloat)rheight;

    scale.x = (float)display_width/(float)rwidth;    //seems redundant but I thing its for high dpi devices
    scale.y = (float)display_height/(float)rheight;

    /* setup global state */
    glViewport(0,0,display_width,display_height);
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_SCISSOR_TEST);
    glActiveTexture(GL_TEXTURE0);

    /* setup program */
    glUseProgram(dev->prog);
    glUniform1i(dev->uniform_tex, 0);
    glUniformMatrix4fv(dev->uniform_proj, 1, GL_FALSE, &ortho[0][0]);
    {
        /* convert from command queue into draw list and draw to screen */
        const struct nk_draw_command *cmd;
        void *vertices, *elements;
        const nk_draw_index *offset = NULL;

        /* Bind buffers */
        glBindBuffer(GL_ARRAY_BUFFER, dev->vbo);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, dev->ebo);
        
        {
            /* buffer setup */
            glEnableVertexAttribArray((GLuint)dev->attrib_pos);
            glEnableVertexAttribArray((GLuint)dev->attrib_uv);
            glEnableVertexAttribArray((GLuint)dev->attrib_col);

            glVertexAttribPointer((GLuint)dev->attrib_pos, 2, GL_FLOAT, GL_FALSE, dev->vs, (void*)dev->vp);
            glVertexAttribPointer((GLuint)dev->attrib_uv, 2, GL_FLOAT, GL_FALSE, dev->vs, (void*)dev->vt);
            glVertexAttribPointer((GLuint)dev->attrib_col, 4, GL_UNSIGNED_BYTE, GL_TRUE, dev->vs, (void*)dev->vc);
        }

        glBufferData(GL_ARRAY_BUFFER, max_vertex_buffer, NULL, GL_STREAM_DRAW);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, max_element_buffer, NULL, GL_STREAM_DRAW);

        /* load vertices/elements directly into vertex/element buffer */
        vertices = malloc((size_t)max_vertex_buffer);
        elements = malloc((size_t)max_element_buffer);
        {
            /* fill convert configuration */
            struct nk_convert_config config;
            static const struct nk_draw_vertex_layout_element vertex_layout[] = {
                {NK_VERTEX_POSITION, NK_FORMAT_FLOAT, NK_OFFSETOF(struct nk_gen_vertex, position)},
                {NK_VERTEX_TEXCOORD, NK_FORMAT_FLOAT, NK_OFFSETOF(struct nk_gen_vertex, uv)},
                {NK_VERTEX_COLOR, NK_FORMAT_R8G8B8A8, NK_OFFSETOF(struct nk_gen_vertex, col)},
                {NK_VERTEX_LAYOUT_END}
            };
            NK_MEMSET(&config, 0, sizeof(config));
            config.vertex_layout = vertex_layout;
            config.vertex_size = sizeof(struct nk_gen_vertex);
            config.vertex_alignment = NK_ALIGNOF(struct nk_gen_vertex);
            config.null = dev->null;
            config.circle_segment_count = 22;
            config.curve_segment_count = 22;
            config.arc_segment_count = 22;
            config.global_alpha = 1.0f;
            config.shape_AA = AA;
            config.line_AA = AA;

            /* setup buffers to load vertices and elements */
            {struct nk_buffer vbuf, ebuf;
            nk_buffer_init_fixed(&vbuf, vertices, (nk_size)max_vertex_buffer);
            nk_buffer_init_fixed(&ebuf, elements, (nk_size)max_element_buffer);
            nk_convert(&gen.ctx, &dev->cmds, &vbuf, &ebuf, &config);}
        }
        glBufferSubData(GL_ARRAY_BUFFER, 0, (size_t)max_vertex_buffer, vertices);
        glBufferSubData(GL_ELEMENT_ARRAY_BUFFER, 0, (size_t)max_element_buffer, elements);
        free(vertices);
        free(elements);

        /* iterate over and execute each draw command */
        nk_draw_foreach(cmd, &gen.ctx, &dev->cmds) {
            if (!cmd->elem_count) continue;
            glBindTexture(GL_TEXTURE_2D, (GLuint)cmd->texture.id);
            glScissor((GLint)(cmd->clip_rect.x * scale.x),
                (GLint)((rheight - (GLint)(cmd->clip_rect.y + cmd->clip_rect.h)) * scale.y),
                (GLint)(cmd->clip_rect.w * scale.x),
                (GLint)(cmd->clip_rect.h * scale.y));
            glDrawElements(GL_TRIANGLES, (GLsizei)cmd->elem_count, GL_UNSIGNED_SHORT, offset);
            offset += cmd->elem_count;
        }
        nk_clear(&gen.ctx);
        nk_buffer_clear(&dev->cmds);
    }

    glUseProgram(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

    glDisable(GL_BLEND);
    glDisable(GL_SCISSOR_TEST);
}


NK_API struct nk_context*
nk_gen_init()
{    
    nk_init_default(&gen.ctx, 0);
    nk_gen_device_create();
    return &gen.ctx;
}

NK_API void
nk_gen_font_stash_begin(struct nk_font_atlas **atlas)
{
    nk_font_atlas_init_default(&gen.atlas);
    nk_font_atlas_begin(&gen.atlas);
    *atlas = &gen.atlas;
}

NK_API void
nk_gen_font_stash_end(void)
{
    const void *image; int w, h;
    image = nk_font_atlas_bake(&gen.atlas, &w, &h, NK_FONT_ATLAS_RGBA32);
    nk_gen_device_upload_atlas(image, w, h);
    nk_font_atlas_end(&gen.atlas, nk_handle_id((int)gen.ogl.font_tex), &gen.ogl.null);
    if (gen.atlas.default_font)
        nk_style_set_font(&gen.ctx, &gen.atlas.default_font->handle);

}


NK_API
void nk_gen_shutdown(void)
{
    nk_font_atlas_clear(&gen.atlas);
    nk_free(&gen.ctx);
    nk_gen_device_destroy();
    memset(&gen, 0, sizeof(gen));
}
