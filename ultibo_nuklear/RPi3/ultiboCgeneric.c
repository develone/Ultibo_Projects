/*
 * ultiboCgeneric.c
 *  
 * 
 *  
 */
 
#include <stdio.h>

#include "opt/vc/include/GLES/gl.h"
#include "opt/vc/include/GLES2/gl2.h"

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

/* This are some code examples to provide a small overview of what can be
 * done with this library. To try out an example uncomment the include
 * and the corresponding function. */
#include "style.c"
#include "calculator.c"
//#include "overview.c" //this needs a few extra functions to be implemented or redirected from ultibo
#include "node_editor.c"

//***** From ultibo
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


// gui context, manages, input , drawing 
// everything
struct nk_context *ctx;
char CoordBuufer[10] = {0};
char buf[256] = {0};

// gui code


static void nuklear_MainLoop(void* loopArg);



void ultibo_C_main()
{
    getScreenSize(&ScreenWidth, &ScreenHeight);	

	 
    ctx = nk_ultibo_init();
    /* Load Fonts: if none of these are loaded a default font will be used  */
    /* Load Cursor: if you uncomment cursor loading please hide the cursor */
    {struct nk_font_atlas *atlas;
    nk_ultibo_font_stash_begin(&atlas);   //left here you can test more fonts
    /*struct nk_font *droid = nk_font_atlas_add_from_file(atlas, "../../../extra_font/DroidSans.ttf", 14, 0);*/
    struct nk_font *roboto = nk_font_atlas_add_from_file(atlas, "extra_font\\Roboto-Regular.ttf", 16, 0);
    //struct nk_font *future = nk_font_atlas_add_from_file(atlas, "extra_font/kenvector_future_thin.ttf", 13, 0);
    /*struct nk_font *clean = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyClean.ttf", 12, 0);*/
    /*struct nk_font *tiny = nk_font_atlas_add_from_file(atlas, "../../../extra_font/ProggyTiny.ttf", 10, 0);*/
    /*struct nk_font *cousine = nk_font_atlas_add_from_file(atlas, "../../../extra_font/Cousine-Regular.ttf", 13, 0);*/
    nk_ultibo_font_stash_end();
    nk_style_load_all_cursors(ctx, atlas->cursors);
    nk_style_set_font(ctx, &roboto->handle);}

    /* style.c */
    /*set_style(ctx, THEME_WHITE);*/
    set_style(ctx, THEME_RED);
    /*set_style(ctx, THEME_BLUE);*/
    /*set_style(ctx, THEME_DARK);*/	
	 
 
	while(1)
	{

		
	  getMouseXY(&PanelMouseX, &PanelMouseY, &ButtonsMouse);
      nuklear_MainLoop((void*)ctx); // here is the gui code			
        
	  glSwapBuffer();
 
	}
	
    nk_ultibo_shutdown(); // not needed but to show where it goes
 
}
//------------------------------------------------------------------------------------------------


static void
nuklear_MainLoop(void* loopArg){
    
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


    /* -------------- EXAMPLES ---------------- */
    calculator(ctx);
    //overview(ctx); //this needs a few extra functions to be implemented  or redirected from ultibo
    node_editor(ctx);
    /* ----------------------------------------- */	

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


