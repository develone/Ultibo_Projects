/*  dirent.h
 *
 */
#ifndef DIRENT_H
#define DIRENT_H


#ifdef __cplusplus
extern "C" {
#endif

struct dirent
{
    char        d_name[260];
};

typedef struct
{
    unsigned long d_hdir;              
    char         *d_dirname[255+1];           
    unsigned      d_magic;             
    unsigned      d_nfiles;            
    char          d_buf[320];  
} DIR;


DIR            * opendir  (const char *dirname);
struct dirent  * readdir  (DIR *dir);
int            closedir (DIR *dir);
void           rewinddir(DIR *dir);

#ifdef __cplusplus
}
#endif

#endif