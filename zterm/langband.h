#ifndef LANGBAND_H
#define LANGBAND_H



#ifdef WIN32
/* angband.h included earlier with these defs */
#ifndef INCLUDED_H_TYPE_H
typedef unsigned char byte;
typedef const char *cptr;
typedef int errr;
typedef char bool;
#endif
#endif /* win32 */


#ifdef USE_SOUND

typedef struct {
        long mtype;
       	short extra;
	char str[1024];
} snd_msg;

typedef enum {
	SNDMSG_PLAY = 1,
	SNDMSG_LOADSOUND,
	SNDMSG_CLOSE
} snd_msg_types;


errr
send_sound_msg (int type, int extra, const cptr str);
errr
sound_init (void);

#endif /* use_sound */

#define NO_FLAGS 0
#define LANGBAND_GRAPHICS 1

#define LANGBAND_TEXT_END 0x80
#define LANGBAND_GFX_START 0x100

#define IMAGE_ARRAY_SIZE 64

extern errr init_graphics();
extern int paint_image(const char *fname, int x, int y);
extern int load_gfx_image(const char *fname, const char *type);
INTERFACE int paint_gfx_image(const char *fname, const char *name, int x, int y);
extern int init_tile_files();
extern int fill_area(int image_index, int tile_num, int x1, int y1, int x2, int y2);

/* remove later */
extern void print_image_list();
extern int load_scaled_image(const char *filename, int image_index, int width, int height);

#endif /* langband_h */
