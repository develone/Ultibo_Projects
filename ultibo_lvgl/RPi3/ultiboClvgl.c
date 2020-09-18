/*
 * ultiboClvgl.c
 *  
 * lvgl library test Framework
 * 
 *  
 */
#include <pthread.h>
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <sys/timeb.h>
#include <time.h>
#include<unistd.h>




#include "lvgl/lvgl.h"
#include "lv_examples/lv_examples.h"


// this builds a 32 bit color value in A.8.8.8 format (8-bit alpha mode)
#define _RGB32BIT(a,r,g,b) ((b) + ((g) << 8) + ((r) << 16) + ((a) << 24))

typedef unsigned long  ULONG;
typedef unsigned char  UCHAR;
typedef unsigned short USHORT;


#ifdef __cplusplus
extern "C" {
#endif

void getMouseXY(int *cx, int *cy, int *btc);
void getScreenSize(unsigned long int *scrWidth, unsigned long int *scrHeight);

int getScreenPitch(void);
USHORT* getScreenBuffer(void);

void getKey(int *value);  

#ifdef __cplusplus
}
#endif

int PanelMouseX = 0;
int PanelMouseY = 0;
int ButtonsMouse = 0;
int Key = -1;


unsigned long int ScreenWidth = 800;
unsigned long int ScreenHeight = 480;

unsigned char *lvideo_buffer;
int mempitch;
UCHAR* dest_buffer;


bool ultibo_input_read(lv_indev_drv_t * drv, lv_indev_data_t*data);

void ultibo_fbddev_flush(lv_disp_drv_t * drv, const lv_area_t * area, lv_color_t * color_p);

void *tick_thread (void *args);

bool ultibo_keyboard_read(lv_indev_drv_t * drv, lv_indev_data_t*data);





int lvglmain()
{
  getScreenSize(&ScreenWidth, &ScreenHeight);   
	
  mempitch = getScreenPitch();
  
  lvideo_buffer = (unsigned char*) calloc ( (ScreenWidth * 2) * (ScreenHeight * 2 ) ,sizeof(unsigned char) );	 
  //lvideo_buffer = (unsigned char*) calloc ( (480*4) * (320*4),sizeof(unsigned char) );	 
  
  dest_buffer = (UCHAR*)getScreenBuffer();
  

    /*LittlevGL init*/
    lv_init(); 

    /*A small buffer for LittlevGL to draw the screen's content*/
    static lv_color_t *buf[(800*4)*(480*4)]; 

    /*Initialize a descriptor for the buffer*/
    static lv_disp_buf_t disp_buf;
    lv_disp_buf_init(&disp_buf, buf, NULL, (800*4)*(480*4));	
  
    /*Initialize and register a display driver*/
    lv_disp_drv_t disp_drv;
    lv_disp_drv_init(&disp_drv);
    disp_drv.buffer = &disp_buf;
    disp_drv.flush_cb = ultibo_fbddev_flush;
    lv_disp_drv_register(&disp_drv);
	
	/*Initialize input driver for mouse */
    lv_indev_drv_t indev_drv;
    lv_indev_drv_init(&indev_drv);      /*Basic initialization*/
    indev_drv.type = LV_INDEV_TYPE_POINTER;                 /*See below.*/
    indev_drv.read_cb =ultibo_input_read;              /*See below.*/
    /*Register the driver in LVGL and save the created input device object*/
    lv_indev_t * my_indev = lv_indev_drv_register(&indev_drv);
	
	/*Initialize input driver for keyboard */
    lv_indev_drv_t keyb_indev_drv;
    lv_indev_drv_init(&keyb_indev_drv);      /*Basic initialization*/
    keyb_indev_drv.type = LV_INDEV_TYPE_KEYPAD;                 /*See below.*/
    keyb_indev_drv.read_cb =ultibo_keyboard_read;              /*See below.*/
    /*Register the driver in LVGL and save the created input device object*/
    lv_indev_t * keyb_indev = lv_indev_drv_register(&keyb_indev_drv);

    //Keyboard is not as global as the mouse, objects that respond
	//to keyboard inputs must be added
	//lv_group_t * g = lv_group_create();
	//lv_group_add_obj(g, obj);

    /*Quick cursor*/
	lv_obj_t * cursor_obj = lv_label_create(lv_scr_act(), NULL);          
    lv_label_set_text(cursor_obj, "*");
    lv_obj_align(cursor_obj, NULL, LV_ALIGN_CENTER, 0, 0);  	
	lv_indev_set_cursor(my_indev, cursor_obj); 

  
	//lv_demo_printer();
	//lv_demo_benchmark();
	//lv_demo_stress();
	lv_demo_widgets();
	//lv_demo_keypad_encoder();
	
    pthread_t thread1;
    int thr = 1;
    pthread_create(&thread1, NULL, *tick_thread, (void *) thr); 
   
  while(1)
  {
	 
     getMouseXY(&PanelMouseX, &PanelMouseY, &ButtonsMouse);	
     getKey(&Key);	 
     lv_task_handler();
		
     memcpy(dest_buffer,lvideo_buffer, (ScreenWidth * 2) * (ScreenHeight * 2 ));
     
  }
  
 

}

// create the function to be executed as a thread
void * tick_thread (void *args)
{
      while(1) {
        usleep(5*1000);   /*Sleep for 5 millisecond*/
        lv_tick_inc(5);      /*Tell LVGL that 5 milliseconds were elapsed*/
    }
}


/**
 * from: https://github.com/lvgl/lv_drivers/blob/master/display/fbdev.c
 * Flush a buffer to the marked area
 * @param drv pointer to driver where this function belongs
 * @param area an area where to copy `color_p`
 * @param color_p an array of pixel to copy to the `area` part of the screen
 */
void ultibo_fbddev_flush(lv_disp_drv_t * drv, const lv_area_t * area, lv_color_t * color_p)
{
    //if(fbp == NULL ||
    //        area->x2 < 0 ||
    //        area->y2 < 0 ||
    //        area->x1 > (int32_t)ScreenWidth - 1 ||
    //        area->y1 > (int32_t)ScreenHeight - 1) {
    //    lv_disp_flush_ready(drv);
    //    return;
    //}

    /*Truncate the area to the screen*/
    int32_t act_x1 = area->x1 < 0 ? 0 : area->x1;
    int32_t act_y1 = area->y1 < 0 ? 0 : area->y1;
    int32_t act_x2 = area->x2 > (int32_t)ScreenWidth - 1 ? (int32_t)ScreenWidth - 1 : area->x2;
    int32_t act_y2 = area->y2 > (int32_t)ScreenHeight - 1 ? (int32_t)ScreenHeight - 1 : area->y2;


    lv_coord_t w = (act_x2 - act_x1 + 1);
    long int location = 0;
    long int byte_location = 0;
    unsigned char bit_location = 0;

    /*32 or 24 bit per pixel*/
    //if(vinfo.bits_per_pixel == 32 || vinfo.bits_per_pixel == 24) {
        uint32_t * fbp32 = (uint32_t *)lvideo_buffer;
        int32_t y;
        for(y = act_y1; y <= act_y2; y++) {
            //location = (act_x1 + vinfo.xoffset) + (y + vinfo.yoffset) * finfo.line_length / 4;
			location = act_x1 + y * (mempitch>>2);// /4;
            memcpy(&fbp32[location], (uint32_t *)color_p, (act_x2 - act_x1 + 1) * 4);
            color_p += w;
        }
    //}
    /*16 bit per pixel*/
	/*
    else if(vinfo.bits_per_pixel == 16) {
        uint16_t * fbp16 = (uint16_t *)fbp;
        int32_t y;
        for(y = act_y1; y <= act_y2; y++) {
            location = (act_x1 + vinfo.xoffset) + (y + vinfo.yoffset) * finfo.line_length / 2;
            memcpy(&fbp16[location], (uint32_t *)color_p, (act_x2 - act_x1 + 1) * 2);
            color_p += w;
        }
    }
	*/
    /*8 bit per pixel*/
	/*
    else if(vinfo.bits_per_pixel == 8) {
        uint8_t * fbp8 = (uint8_t *)fbp;
        int32_t y;
        for(y = act_y1; y <= act_y2; y++) {
            location = (act_x1 + vinfo.xoffset) + (y + vinfo.yoffset) * finfo.line_length;
            memcpy(&fbp8[location], (uint32_t *)color_p, (act_x2 - act_x1 + 1));
            color_p += w;
        }
    }
	*/
    /*1 bit per pixel*/
	/*
    else if(vinfo.bits_per_pixel == 1) {
        uint8_t * fbp8 = (uint8_t *)fbp;
        int32_t x;
        int32_t y;
        for(y = act_y1; y <= act_y2; y++) {
            for(x = act_x1; x <= act_x2; x++) {
                location = (x + vinfo.xoffset) + (y + vinfo.yoffset) * vinfo.xres;
                byte_location = location / 8; //find the byte we need to change
                bit_location = location % 8; // inside the byte found, find the bit we need to change
                fbp8[byte_location] &= ~(((uint8_t)(1)) << bit_location);
                fbp8[byte_location] |= ((uint8_t)(color_p->full)) << bit_location;
                color_p++;
            }

            color_p += area->x2 - act_x2;
        }
    } else {
		*/
        /*Not supported bit per pixel*/
		/*
    }

    //May be some direct update command is required
    //ret = ioctl(state->fd, FBIO_UPDATE, (unsigned long)((uintptr_t)rect));
    */
    lv_disp_flush_ready(drv);
}

bool ultibo_input_read(lv_indev_drv_t * drv, lv_indev_data_t*data)
{
    data->state = ButtonsMouse ? LV_INDEV_STATE_PR : LV_INDEV_STATE_REL;
    
	//if(data->state == LV_INDEV_STATE_PR) 
	//{	
		data->point.x = PanelMouseX;
		data->point.y = PanelMouseY;
	//}	

    return false; /*Return `false` because we are not buffering and no more data to read*/
}

bool ultibo_keyboard_read(lv_indev_drv_t * drv, lv_indev_data_t*data){
  
    if(Key != -1) 
    { 
      if (Key != 0)
      {
         // Standard keys
		 /*
         if(Key == 0x08)
           data->key = ;//nk_input_key(ctx, NK_KEY_BACKSPACE, 1);         
         else if(Key == 0x09)
           data->key = ;//nk_input_key(ctx, NK_KEY_TAB, 1);         
         else if(Key == 0x0D)
           data->key = ;//nk_input_key(ctx, NK_KEY_ENTER, 1);
         else if(Key == 0x18)
           data->key = ;//nk_input_key(ctx, NK_KEY_CUT, 1);	   
         else if(Key == 0x03)
           data->key = ;//nk_input_key(ctx, NK_KEY_COPY, 1);	   	   
         else if(Key == 0x16)
           data->key = ;//nk_input_key(ctx, NK_KEY_PASTE, 1);	   	   	   
         else
         */			 
         data->key =  (char)Key; 
		 data->state = LV_INDEV_STATE_PR;
      }
      else
      {
       // Extended keys	
       getKey(&Key);
       if(Key == 0x53)
		 data->key = LV_KEY_DEL; //nk_input_key(ctx, NK_KEY_DEL, 1);
	   else if(Key == 0x48)
         data->key = LV_KEY_UP;//nk_input_key(ctx, NK_KEY_UP, 1); 
       else if(Key == 0x50)
         data->key = LV_KEY_DOWN;//nk_input_key(ctx, NK_KEY_DOWN, 1);	     
       else if(Key == 0x4B)
         data->key = LV_KEY_LEFT;//nk_input_key(ctx, NK_KEY_LEFT, 1);
       else if(Key == 0x4D)
         data->key = LV_KEY_RIGHT;//nk_input_key(ctx, NK_KEY_RIGHT, 1);	   
       else if(Key == 0x47) // HOME       
         data->key = LV_KEY_HOME; //nk_input_key(ctx, NK_KEY_TEXT_LINE_START, 1);             
       else if(Key == 0x4F) // END
         data->key = LV_KEY_END;//nk_input_key(ctx, NK_KEY_TEXT_LINE_END, 1);       
	   
	   data->state = LV_INDEV_STATE_PR;  
	  }
	  
	}
	else{
		data->state = LV_INDEV_STATE_REL;
	}

  return false; /*No buffering now so no more data read*/
}


