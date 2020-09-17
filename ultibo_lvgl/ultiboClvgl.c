/*
 * ultiboClvgl.c
 *  
 * lvgl library test Framework
 * 
 *  
 */

#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <sys/timeb.h>
#include <time.h>

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



bool ultibo_input_read(lv_indev_drv_t * drv, lv_indev_data_t*data);

void fbdev_flush(lv_disp_drv_t * drv, const lv_area_t * area, lv_color_t * color_p);



int PanelMouseX = 0;
int PanelMouseY = 0;
int ButtonsMouse = 0;
int Key;


unsigned long int ScreenWidth = 800;
unsigned long int ScreenHeight = 480;

unsigned char *lvideo_buffer;
int mempitch;
UCHAR* dest_buffer;



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
    disp_drv.flush_cb = fbdev_flush;
    lv_disp_drv_register(&disp_drv);
	
	/*Initialize input driver*/
    lv_indev_drv_t indev_drv;
    lv_indev_drv_init(&indev_drv);      /*Basic initialization*/
    indev_drv.type = LV_INDEV_TYPE_POINTER;                 /*See below.*/
    indev_drv.read_cb =ultibo_input_read;              /*See below.*/
    /*Register the driver in LVGL and save the created input device object*/
    lv_indev_t * my_indev = lv_indev_drv_register(&indev_drv);

    /*Quick cursor*/
	lv_obj_t * cursor_obj = lv_label_create(lv_scr_act(), NULL);          
    lv_label_set_text(cursor_obj, "*");
    lv_obj_align(cursor_obj, NULL, LV_ALIGN_CENTER, 0, 0);  	
	lv_indev_set_cursor(my_indev, cursor_obj); 

  
	lv_demo_printer();
	//lv_demo_benchmark();
    
	
  while(1)
  {
	
     getMouseXY(&PanelMouseX, &PanelMouseY, &ButtonsMouse);	
  
     //memset(lvideo_buffer, 0 ,  (480*4) * (320*4));

     lv_tick_inc(5);
     lv_task_handler();
		
     memcpy(dest_buffer,lvideo_buffer, (ScreenWidth * 2) * (ScreenHeight * 2 ));
     
  }
  
 

}


/**
 * from: https://github.com/lvgl/lv_drivers/blob/master/display/fbdev.c
 * Flush a buffer to the marked area
 * @param drv pointer to driver where this function belongs
 * @param area an area where to copy `color_p`
 * @param color_p an array of pixel to copy to the `area` part of the screen
 */
void fbddev_flush(lv_disp_drv_t * drv, const lv_area_t * area, lv_color_t * color_p)
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

