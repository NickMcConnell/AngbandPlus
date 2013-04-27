// File: sys-dos/main.cpp
// Main system-specific file for DOS machines


/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Original code by Billy Tanksley (wtanksle@@ucsd.edu)
 *
 * Support for DJGPP v2 by Scott Egashira (egashira@@u.washington.edu)
 *
 * Extensive modifications by Ben Harrison (benh@@voicenet.com).
 *
 * True color palette support by Mike Marcelais (mrmarcel@@eos.ncsu.edu),
 * with interface to the "color_table" array by Ben Harrison.  Matt
 * Craighead also helped with developing and testing the palette code.
 *
 * Massive revision for graphics and other stuff by Matt Craighead.
 */


//#include <sys/farptr.h>
//#include <osfcn.h>
#include "../utumno.h"
#include "sys-dos.h"


// Include the character bitmaps, palette data, mouse cursor, and fonts
#include "chars.h"
#include "palette.h"
#include "cursor.h"
#include "bold.h"
#include "regular.h"

#include <X11/Xlib.h>
#define XK_MISCELLANY
#define XK_LATIN1
#define XK_PUBLISHING
#include <X11/keysymdef.h>
#define NIL (0)
/*  Xlib global variables */

Display *dpy ;
int blackColor;
int whiteColor;
Window w ; 
GC gc ;
XEvent report;
int x_palette[500];
Pixmap buffer_pixmap, pixmap;
/*                        */
struct CFrame {
    byte height;
    u16b ndata;
    byte *data;
};

struct CScene {
    int nframes;
    char *name;
    CFrame *frames; // allocate nviews*nframes of these
};

struct CTile {
    char *name;
    int nviews;
    int nscenes;
    CScene *scenes;
};

// Maximum number of tiles
const int MAX_TILES = 1024;

// The graphics mode we are using
const int VESA_MODE = 0x101; // 640x480, 256 colors

// The height/width of the graphics mode
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

// The mode properties
const int FULL_BANKS = 4;          // 4 blocks of 65536 bytes
const int SIZE_LAST_BANK = 45056;  // 1 block of 45056 bytes

// Darkening info
static byte darken_tab[256];

// A virtual screen
static byte *virtual_screen = NULL;

// The tile database
static int ntiles;
static CTile *tiles[MAX_TILES];

// The current clipping rectangle
static int cx1 = 0, cy1 = 0, cx2 = 639, cy2 = 479;

// Verify against the clipping rectangle
#define test_clip(x, y) (((x) < cx1) || ((x) > cx2) || ((y) < cy1) || ((y) > cy2))

// Put a pixel on the virtual screen
// Ignores clipping
/*SAW  
#define put_pixel(x, y, color) virtual_screen[SCREEN_WIDTH*(y)+(x)] = color;
*/
#define put_pixel(x,y,color) { XSetForeground(dpy, gc, x_palette[color]); XDrawPoint(dpy, buffer_pixmap, gc, x, y);}

/* SAW
#define set_bank(bank){    regs.x.ax = 0x4F05; regs.x.bx = 0; regs.x.dx = (bank);     __dpmi_int(0x10, &regs);     regs.x.ax = 0x4F05; regs.x.bx = 1; regs.x.dx = (bank);     __dpmi_int(0x10, &regs); }
*/

/*
 * Flush changes to the screen
 */
/*SAW*/
void screen_refresh(void)
{
    /*
    int bank;
    __dpmi_regs regs;

    for (bank = 0; bank < 4; bank++) {
        set_bank(bank << modeinfo.WinGranularity);
        dosmemput(&virtual_screen[0] + 65536L * ((long) bank),
            65536L, 0xA0000);
    }
    set_bank(4 << modeinfo.WinGranularity);
    dosmemput(&virtual_screen[0] + 65536L * ((long) 4),
        45056L, 0xA0000);
	*/
	XCopyArea(dpy, buffer_pixmap, w,gc, 0, 0, 640,  480,  0, 0);
	XFlush(dpy);  //SAW GUESS
}
void partial_screen_refresh(int x,int y, int width, int height)
{
  XCopyArea(dpy, buffer_pixmap, w,gc, x, y, width,  height,  x, y);	
  XFlush(dpy);
}
/*
 * Set a clipping rectangle
 */
void set_clip_rect(int x1, int y1, int x2, int y2)
{
    cx1 = x1; cx2 = x2; cy1 = y1; cy2 = y2;
}


/*
 * Remove any clipping rectangle
 */
void clear_clip_rect(void) { set_clip_rect(0, 0, 639, 479); }


// Keypress input modifier flags (hard-coded by DOS)
const int K_RSHIFT       = 0;   // Right shift key down
const int K_LSHIFT       = 1;   // Left shift key down
const int K_CTRL         = 2;   // Ctrl key down
const int K_ALT          = 3;   // Alt key down
const int K_SCROLL       = 4;   // Scroll lock on
const int K_NUM          = 5;   // Num lock on
const int K_CAPS         = 6;   // Caps lock on
const int K_INSERT       = 7;   // Insert on

/*
 * Is shift depressed?
 */

/*SAW REWRITE */
bool get_shift(void)
{
	/*
    // Acquire mods via BIOS
    int mods = bioskey(2);

    // If it checks out, shift is depressed
    if (mods & (1 << K_LSHIFT)) return TRUE;
    if (mods & (1 << K_RSHIFT)) return TRUE;
*/
    // No shift
    return FALSE;
}


/*
 * Is caps lock on?
 */

/*SAW REWRITE */
bool get_capslock(void)
{
    // Acquire mods via BIOS
/*    int mods = bioskey(2);

    // If it checks out, it's on
    if (mods & (1 << K_CAPS)) return TRUE;
*/
    // Not on
    return FALSE;
}


/*
 * Scan for a keypress; return its scan code or 0
 */
int scan_inkey_scan(void)
{
/*    if (!bioskey(0x11)) return 0;
    return (bioskey(0x10) >> 8) & 0xFF;*/
/*SAW MEGLIO NONBLOCKING ? */
    int key;
    XNextEvent(dpy, &report);
    if (report.type==KeyPress)
    {
     key = XLookupKeysym(&report.xkey, 0);
     /* Wrapper from X keysyms  */
     switch(key)
     {	    
      case XK_Escape: return KEY_ESCAPE ;
      case XK_1: return KEY_1 ;
      case XK_2: return KEY_2 ;
      case XK_3: return KEY_3 ;
      case XK_4: return KEY_4 ;
      case XK_5: return KEY_5 ;
      case XK_6: return KEY_6 ;
      case XK_7: return KEY_7 ;
      case XK_8: return KEY_8 ;
      case XK_9: return KEY_9 ;
      case XK_0: return KEY_0 ;
      case XK_minus: return KEY_MINUS ;
      case XK_equal: return KEY_EQUAL ;
      case XK_BackSpace: return KEY_BACKSPACE ;
      case XK_Tab: return KEY_TAB ;
      case XK_q: return KEY_Q ;
      case XK_w: return KEY_W ;
      case XK_e: return KEY_E ;
      case XK_r: return KEY_R ;
      case XK_t: return KEY_T ;
      case XK_y: return KEY_Y ;
      case XK_u: return KEY_U ;
      case XK_i: return KEY_I ;
      case XK_o: return KEY_O ;
      case XK_p: return KEY_P ;
      case XK_bracketleft: return KEY_LBRACKET ;
      case XK_bracketright: return KEY_RBRACKET ;
      case XK_Return: return KEY_ENTER ;
      case XK_a: return KEY_A ;
      case XK_s: return KEY_S ;
      case XK_d: return KEY_D ;
      case XK_f: return KEY_F ;
      case XK_g: return KEY_G ;
      case XK_h: return KEY_H ;
      case XK_j: return KEY_J ;
      case XK_k: return KEY_K ;
      case XK_l: return KEY_L ;
      case XK_semicolon: return KEY_SEMICOLON ;
      case XK_rightsinglequotemark: return KEY_QUOTE ;
      case XK_leftsinglequotemark: return KEY_BACKQUOTE ;
      case XK_backslash: return KEY_BACKSLASH ;
      case XK_z: return KEY_Z ;
      case XK_x: return KEY_X ;
      case XK_c: return KEY_C ;
      case XK_v: return KEY_V ;
      case XK_b: return KEY_B ;
      case XK_n: return KEY_N ;
      case XK_m: return KEY_M ;
      case XK_comma: return KEY_COMMA ;
      case XK_period: return KEY_PERIOD ;
      case XK_slash: return KEY_SLASH ;
      case XK_asterisk: return KEYPAD_STAR ;
      case XK_space: return KEY_SPACE ;
      case XK_F1: return KEY_F1 ;
      case XK_F2: return KEY_F2 ;
      case XK_F3: return KEY_F3 ;
      case XK_F4: return KEY_F4 ;
      case XK_F5: return KEY_F5 ;
      case XK_F6: return KEY_F6 ;
      case XK_F7: return KEY_F7 ;
      case XK_F8: return KEY_F8 ;
      case XK_F9: return KEY_F9 ;
      case XK_F10: return KEY_F10 ;
      case XK_KP_7: return KEYPAD_7 ;
      case XK_KP_8: return KEYPAD_8 ;
      case XK_KP_9: return KEYPAD_9 ;
      case XK_KP_Subtract: return KEYPAD_MINUS ;
      case XK_KP_4: return KEYPAD_4 ;
      case XK_KP_5: return KEYPAD_5 ;
      case XK_KP_6: return KEYPAD_6 ;
      case XK_KP_Add: return KEYPAD_PLUS ;
      case XK_KP_1: return KEYPAD_1 ;
      case XK_KP_2: return KEYPAD_2 ;
      case XK_KP_3: return KEYPAD_3 ;
      case XK_KP_0: return KEYPAD_0 ;
      case XK_KP_Decimal: return KEYPAD_PERIOD ;
      case XK_Up: return KEY_UP ;
      case XK_Left: return KEY_LEFT ;
      case XK_Right: return KEY_RIGHT ;
      case XK_Down: return KEY_DOWN ;
      case XK_Home: return KEY_HOME ;
      case XK_End: return KEY_END ;
      case XK_Page_Up: return KEY_PGUP ;
      case XK_Page_Down: return KEY_PGDN ;
     } 
    }
	return 0;    
}


// Save/restore the virtual screen

/*SAW DA RIFARE ...*/
void save_screen(void)
{ 
     XCopyArea(dpy, w, pixmap, gc, 0, 0, 640, 480,  0, 0);

    /*byte *rval = new byte[SCREEN_HEIGHT*SCREEN_WIDTH];
    memcpy(rval, virtual_screen, SCREEN_HEIGHT*SCREEN_WIDTH*sizeof(byte));
    return rval;*/
}

void restore_screen(void)
{
//    memcpy(virtual_screen, from, SCREEN_HEIGHT*SCREEN_WIDTH*sizeof(byte));
      XCopyArea(dpy, pixmap, w,gc, 0, 0, 640, 480,  0, 0);
}

/*
 * Detect the presence of a VESA SVGA card
 */
int vesa_detect(void/*vgainfo_type *info*/)
{
    /* SAW
    __dpmi_regs regs;

    regs.x.ax = 0x4F00;
    regs.x.di = __tb & 0x0F;
    regs.x.es = (__tb >> 4) & 0xFFFF;
    dosmemput(info, sizeof(*info), __tb);
    __dpmi_int(0x10, &regs);
    dosmemget(__tb, sizeof(*info), info);
    if (strncmp(info->VESASignature,"VESA",4) != 0) {
        return 0;
    }
    return info->VESAVersion;
    */
return 0;    
}

/*
 * Get mode info
 */
static void get_mode_info(/*int mode, modeinfo_type *info*/void)
{
	/*SAW
    __dpmi_regs regs;

    regs.x.ax = 0x4F01;
    regs.x.cx = mode;
    regs.x.di = __tb & 0x0F;
    regs.x.es = (__tb >> 4) & 0xFFFF;
    __dpmi_int(0x10, &regs);
    dosmemget(__tb, sizeof(*info), info);
    */
}

/*
 * Set the VESA graphics mode
 */
static void set_vesa_mode(int mode)
{
	/*SAW
    __dpmi_regs regs;

    regs.x.ax = 0x4F02;
    regs.x.bx = mode;
    __dpmi_int(0x10, &regs);
    */
}

/*
 * Set the graphics mode with the BIOS
 */
static void set_bios_mode(int mode)
{
	/*
    __dpmi_regs regs;

    regs.h.ah = 0x00;
    regs.h.al = mode;
    __dpmi_int(0x10, &regs);
    */
}


/*
 * Get mouse location with button status
 *
 * x = x coordinate
 * y = y coordinate
 * left = left button
 */
/*SAW REWRITE */
void get_mouse_status(int *x, int *y, bool *left)
{/*
    __dpmi_regs regs;

    regs.x.ax = 3;
    __dpmi_int(0x33, &regs);
    *x = regs.x.cx;
    *y = regs.x.dx;
    *left = (regs.x.bx & 1) ? TRUE : FALSE;
    */
}


/*
 * Get last release of left mouse button
 */
/*SAW REWRITE */
bool get_last_left_button_release(int *rx, int *ry)
{
	/*
    __dpmi_regs regs;

    regs.x.ax = 6;
    regs.x.bx = 0;
    __dpmi_int(0x33, &regs);
    *rx = regs.x.cx;
    *ry = regs.x.dx;
    return (regs.x.bx ? TRUE : FALSE);*/
}


/*
 * Get last release of right mouse button
 */
/*SAW REWRITE */

bool get_last_right_button_release(int *rx, int *ry)
{
/*    __dpmi_regs regs;

    regs.x.ax = 6;
    regs.x.bx = 1;
    __dpmi_int(0x33, &regs);
    *rx = regs.x.cx;
    *ry = regs.x.dx;
    return (regs.x.bx ? TRUE : FALSE);
*/
	return FALSE;
}


/*
 * Mouse stuff
 */
static byte backup[160];

void virt_draw_mouse(int x, int y)
{
    byte *vs;
    int pixel;
    int xx, yy;
    byte data;

    for (pixel = 0; pixel < 160; pixel++) {
        xx = (pixel % 10) + x - hotspot_x;
        if ((xx < 0) || (xx > SCREEN_WIDTH-1)) continue;
        yy = (pixel / 10) + y - hotspot_y;
        if ((yy < 0) || (yy > SCREEN_HEIGHT-1)) continue;
        vs = &virtual_screen[yy*640 + xx];
        backup[pixel] = *vs;
        data = mouse_pointer_data[pixel];
        if (data != 255) *vs = data;
    }
}

void virt_kill_mouse(int x, int y)
{
    int pixel;
    int xx, yy;

    for (pixel = 0; pixel < 160; pixel++) {
        xx = (pixel % 10) + x - hotspot_x;
        if ((xx < 0) || (xx > SCREEN_WIDTH-1)) continue;
        yy = (pixel / 10) + y - hotspot_y;
        if ((yy < 0) || (yy > SCREEN_HEIGHT-1)) continue;
        virtual_screen[yy*640 + xx] = backup[pixel];
    }
}


/*
 * Set a palette entry
 *
 * c, r, g, b all from 0..255
 *
 * WARNING: do not use except at times when it is safe!
 */
/*SAW REWRITE */

void set_palette_entry(int c, int r, int g, int b)
{
/*    r /= 4; g /= 4; b /= 4;    // Set the color reg using ports    outportb(0x3C6, 0xFF);    outportb(0x3C8, c);    outportb(0x3C9, r);    outportb(0x3C9, g);   outportb(0x3C9, b); */
	XColor xcolor;
	Colormap colormap;
	colormap = DefaultColormap(dpy, DefaultScreen(dpy));
	xcolor.red=(unsigned short)((r*65535)/256);
        xcolor.green=(unsigned short)((g*65535)/256);
        xcolor.blue=(unsigned short)((b*65535)/256);
	XAllocColor(dpy, colormap, &xcolor);
	x_palette[c]=xcolor.pixel;
}

/*
 * Reset the palette to the default
 */
void set_default_palette(void)
{
    for (int c = 0; c < 256; c++) {
        set_palette_entry(c, palette[c*3], palette[c*3+1], palette[c*3+2]);
    }
}


/*
 * Return to text mode
 */
/*SAW REWRITE */

static void kill_system_specific(void)
{
	/*
    if (virtual_screen) delete[] virtual_screen;
    set_bios_mode(3);
    remove_timer();
    */
// *** MV
	XFreeGC(dpy,gc);
    XFreePixmap(dpy,pixmap);
    XFreePixmap(dpy,buffer_pixmap);
    XCloseDisplay(dpy);

}


/*
 * Starting here are the graphics primitives.
 */


// Begin or end a batch of pixel draws
void start_pixel_draw(void) {}
void end_pixel_draw(void) {}


// Draw a pixel to the virtual screen
void draw_pixel(int x, int y, byte c)
{
    if (!test_clip(x, y)) put_pixel(x, y, c);
}

// Draw a box
void box(int x1, int y1, int x2, int y2, byte color)
{
//SAW    int y;

    if ((x1 > cx2) || (x2 < cx1)) return;
    if (x2 < x1) return;
    if (x1 < cx1) x1 = cx1;
    if (x2 > cx2) x2 = cx2;
/*SAW
    for (y = y1; y <= y2; y++) {
        if ((y < cy1) || (y > cy2)) continue;
        memset(virtual_screen+y*SCREEN_WIDTH+x1, color, x2-x1+1);
    }
*/
	XSetForeground(dpy, gc, x_palette[color]);
	XFillRectangle(dpy, buffer_pixmap, gc, x1, y1, x2-x1, y2-y1);
}

// Set the whole screen to one color
// Bypasses any clipping rectangle
/*SAW*/
void blank_screen(byte color)
{
   // memset(virtual_screen, color, SCREEN_HEIGHT*SCREEN_WIDTH);
//SAW METTI A POSTO I COLORI   
   XSetForeground(dpy, gc, x_palette[color]);	
   XFillRectangle(dpy, buffer_pixmap, gc, 0, 0, 640, 480);
}

// Draw a horizontal or vertical line
/* SAW REWRITE */
void horiz_line(int x1, int x2, int y, byte color)
{
	// use XDrawLine
    if ((y < cy1) || (y > cy2)) return;

    if ((x1 > cx2) || (x2 < cx1)) return;
    if (x2 < x1) return;
    if (x1 < cx1) x1 = cx1;
    if (x2 > cx2) x2 = cx2;

    for (int x = x1; x <= x2; x++) put_pixel(x, y, color);
}

void vert_line(int x, int y1, int y2, byte color)
{
    if ((x < cx1) || (x > cx2)) return;

    if ((y1 > cy2) || (y2 < cy1)) return;
    if (y2 < y1) return;
    if (y1 < cy1) y1 = cy1;
    if (y2 > cy2) y2 = cy2;

    for (int y = y1; y <= y2; y++) put_pixel(x, y, color);
}


// Draw a rectangle
void rectangle(int x1, int y1, int x2, int y2, byte color)
{
    horiz_line(x1, x2, y1, color);
    horiz_line(x1, x2, y2, color);
    vert_line(x1, y1, y2, color);
    vert_line(x2, y1, y2, color);
}

// Draw a character of text
static void put_character(int x, int y, int c, byte color)
{
    int cx, cy, px, py;

    for (cy = 0; cy < 16; cy++) {
        py = y+cy;
        if ((py < cy1) || (py > cy2)) continue;
        for (cx = 0; cx < 8; cx++) {
            px = x+cx;
            if ((px < cx1) || (px > cx2)) continue;
            if ((char_map[c*16 + cy] >> (7-cx)) & 1) put_pixel(px, py, color);
        }
    }
}

// Draw a string
void put_string(int x, int y, char *c, byte color)
{
    int i, len = strlen(c);

    for (i = 0; i < len; i++) {
        put_character(x + 8*i, y, c[i], color);
    }
}

int get_char_width(int c, int font)
{
    if ((c < 32) || (c > 127)) return 0;

    switch (font) {
        case FONT_REGULAR: return font_regular_widths[c-32];
        case FONT_BOLD:    return font_bold_widths[c-32];
        default:           return 0;
    }
}

int string_width(char *c, int font)
{
    int i, len = strlen(c);
    int pixlen = 0;

    for (i = 0; i < len; i++) {
        pixlen += get_char_width(c[i], font);
    }
    return pixlen;
}

// Draw a character of text in a font
static void put_character_font(int x, int y, int c, byte color, int font)
{
    int cx, cy, px, py;
    int height, width, ohang;
    int data[16], i;

    if ((c < 32) || (c > 127)) return;

    switch (font) {
        case FONT_REGULAR:
            height = font_regular_height;
            ohang = font_regular_overhang;
            width = font_regular_widths[c-32];
            for (i = 0; i < height; i++) data[i] = font_regular_data[(c-32)*height+i];
            break;

        case FONT_BOLD:
            height = font_bold_height;
            ohang = font_bold_overhang;
            width = font_bold_widths[c-32];
            for (i = 0; i < height; i++) data[i] = font_bold_data[(c-32)*height+i];
            break;

        default: return;
    }

    for (cy = 0; cy < height; cy++) {
        py = y+cy;
        if ((py < cy1) || (py > cy2)) continue;
        for (cx = 0; cx < width+ohang; cx++) {
            px = x+cx;
            if ((px < cx1) || (px > cx2)) continue;
            if ((data[cy] >> cx) & 1) put_pixel(x+cx, y+cy, color);
        }
    }
}

// Draw a string in a font
void put_text(int x, int y, char *c, byte color, int font)
{
    XTextItem items[1];
    int i, len = strlen(c);

    items[0].chars = c;
    items[0].nchars = len;
    items[0].delta = 0;
    items[0].font = XLoadFont(dpy, "*");

    for (i = 0; i < len; i++) {
        put_character_font(x, y, c[i], color, font);
        x += get_char_width(c[i], font);
    }
/*
    XSetForeground(dpy,gc,x_palette[color]);
    XDrawText(dpy, w, gc, x, y, items, 1);
    screen_refresh();*/
}


/*
 * Load a tile
 */
void load_tile(char *filename)
{
    FILE *f;
    char c, buf[80];
    int i, j, k;
    CTile *map;

    strcpy(buf, "dat/tile/");
    strcat(buf, filename);
    strcat(buf, ".til");

    f = fopen(buf, "rb");
    if (f == NULL) {
        quit(format("Missing tilemap: %s\n", filename));
    }

    // Signature
    c = fgetc(f);
    if (c != 'T') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }
    c = fgetc(f);
    if (c != 'I') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }
    c = fgetc(f);
    if (c != 'L') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }

    // Revision
const int PROPER_REVISION = 2;
    c = fgetc(f);
    if (c != PROPER_REVISION) {
        quit(format("Improper revision in tilemap %s: %d.  This version uses revision %d.",
            filename, c, PROPER_REVISION));
    }

    // Create a tilemap
    map = new CTile;

    // Set tile name
    map->name = new char[strlen(filename)+1];
    strcpy(map->name, filename);

    // Get nviews
    map->nviews = fgetc(f);

    // Get nscenes, allocate scenes
    map->nscenes = fgetc(f);
    map->scenes = new CScene[map->nscenes];

    // For each scene...
    for (i = 0; i < map->nscenes; i++) {
        CScene *scene = &map->scenes[i];

        // Get number of frames, allocate frames
        scene->nframes = fgetc(f);
        scene->frames = new CFrame[map->nviews * scene->nframes];

        // Get name
        j = 0;
        for (;;) {
            char c = fgetc(f);
            buf[j++] = c;
            if (!c) break;
        }
        scene->name = new char[j];
        strcpy(scene->name, buf);

        // For each frame and view...
        for (j = 0; j < scene->nframes*map->nviews; j++) {
            CFrame *frame = &scene->frames[j];

            // Height
            frame->height = fgetc(f);

            // The data
            frame->ndata = fgetc(f);
            frame->ndata += fgetc(f) << 8;
            frame->data = new byte[frame->ndata];
            for (k = 0; k < frame->ndata; k++) frame->data[k] = fgetc(f);
        }
    }

    // Save the tile
    tiles[ntiles] = map;
    ntiles++;

    // Done
    fclose(f);
}


/*
 * A compare function for sort_tiles.
 */
int tile_compare(const void *a, const void *b)
{
    CTile *x = *((CTile **) a);
    CTile *y = *((CTile **) b);
    return strcmp(x->name, y->name);
}


/*
 * Sort the tiles once they are all loaded, for purposes of binary-search access.
 */
void sort_tiles(void)
{
    qsort(tiles, ntiles, sizeof(CTile *), tile_compare);
}


/*
 * Draw an isometric tile at isometric location (ix, iy).
 *
 * The screen is laid out in this fashion:
 *
 * ----------------------
 * |(-1,  /\            /
 * | 0)  /  \  (0,-1)  /
 * |    /    \        /
 * |   /      \      /
 * |  /        \    /
 * | /          \  /
 * |/   (0,0)    \/
 * |\            /\
 * | \          /  \
 * |  \        /    \
 * |   \      /      \
 * |    \    /        \
 * |     \  /          \
 * |(0,1) \/    (1,0)   \
 *
 * and so on.
 */
static void draw_iso_tile(int off_x, int off_y, CFrame *frame, bool darken)
{
    int n, x, y;
    byte *d = frame->data;
    bool can_plot;

    off_y += ISO_HEIGHT - frame->height;

    x = off_x; y = off_y;

    if ((x > cx2) || (y > cy2)) return;
    if (x+ISO_WIDTH-1 < cx1) return;

    for (;;) {
        can_plot = ((y >= cy1) && (x <= cx2));
        switch (*d++) {
            case 0:
                n = *d++;
                if (can_plot) {
                    while ((x < cx1) && (n > 0)) {
                        x++; n--; d++;
                    }
                    if (x+n-1 <= cx2) {
                        if (!darken) {
                            while (n >= 4) {
                                int py = SCREEN_WIDTH*y;
                                *((u32b *) (virtual_screen+py+x)) = *((u32b *) d);
//								put_pixel quattro volte MV
                                n -= 4; d += 4; x += 4;
                            }
                            for (; n > 0; n--) {
                                int py = SCREEN_WIDTH*y;
                                virtual_screen[py+x] = *d;
                                d++; x++;
                            }
                        }
                        else {
                            for (; n > 0; n--) {
                                int py = SCREEN_WIDTH*y;
                                virtual_screen[py+x] = darken_tab[*d];
                                d++; x++;
                            }
                        }
                    }
                    else {
                        for (; n > 0; n--) {
                            int py = SCREEN_WIDTH*y;
                            if (x <= cx2) {
                                if (!darken) {
                                    virtual_screen[py+x] = *d;
                                }
                                else {
                                    virtual_screen[py+x] = darken_tab[*d];
                                }
                            }
                            d++; x++;
                        }
                    }
                }
                else {
                    d += n;
                }
                break;
            case 1:
                x += *d++;
                break;
            case 2:
                x = off_x;
                y++;
                if (y > cy2) return;
                break;
            case 3: return;
        }
    }
}

/*
 * Find an isometric tile by name.
 */
static int find_tile(char *name, int first, int last)
{
    if (first == last) {
        if (strcmp(name, tiles[first]->name) == 0) return first;
        return -1;
    }
    else if (last == first+1) {
        if (strcmp(name, tiles[first]->name) == 0) return first;
        if (strcmp(name, tiles[last]->name) == 0) return last;
        return -1;
    }
    else {
        int middle, result;
        middle = (first+last)/2;
        result = strcmp(tiles[middle]->name, name);
        if (result == 0) return middle;
        else if (result < 0) return find_tile(name, middle, last);
        else return find_tile(name, first, middle);
    }
}

/*
 * A global version of find_tile for general use
 */
int locate_tile(char *name)
{
    return find_tile(name, 0, ntiles-1);
}

/*
 * Draw an isometric tile given its index
 */
void draw_tile_idx(int off_x, int off_y, int index, bool darken, char *scene_name, int view,
    int frame)
{
    CTile *tile = tiles[index];
    CScene *scene = NULL;
    int i;
    
    // Find scene
    for (i = 0; i < tile->nscenes; i++) {
        if (!strcmp(tile->scenes[i].name, scene_name)) {
            scene = &tile->scenes[i];
            break;
        }
    }
    if (!scene) {
        quit(format("Could not find scene %s\n"));
    }

    draw_iso_tile(off_x, off_y, &scene->frames[tile->nviews*frame + view], darken);
}

/*
 * Draw an isometric tile given its name
 */
void draw_tile(int off_x, int off_y, char *tile_name, bool darken, char *scene_name,
    int view, int frame)
{
    int result = locate_tile(tile_name);
    if (result == -1) {
        quit(format("Could not find tile: %s", tile_name));
    }
    else draw_tile_idx(off_x, off_y, result, darken, scene_name, view, frame);
}

/*
 * Miscellaneous system routines
 */
void bell(void) { //write(1, "\007", 1); 
;
}


/*
 * Dump the screen into the .pnm format.  You must then use a converter
 * if you want a decent format, but this format is a pretty easy one
 * to code for and I found a converter on the 'net that works well.
 * Someday I might rewrite this to dump in a better format.
 */
void dump_screen(void)
{
    FILE *f;
    int x, y;
    int color;

    f = fopen("dump.pnm", "wt");
    fprintf(f, "P3\n");
    fprintf(f, "640 480\n");
    fprintf(f, "255\n");
    for (y = 0; y < 480; y++) {
        for (x = 0; x < 640; x++) {
            color = virtual_screen[y*640+x];
            fprintf(f, "%d %d %d ", palette[color*3], palette[color*3+1],
                palette[color*3+2]);
        }
    }
    fclose(f);
}



/*
 * The main() function for the game
 */
int main(void)
{
    dpy = XOpenDisplay(NIL);
    blackColor = BlackPixel(dpy, DefaultScreen(dpy));
    whiteColor = WhitePixel(dpy, DefaultScreen(dpy));
    int depth = DefaultDepth(dpy, DefaultScreen(dpy));
     
    w = XCreateSimpleWindow(dpy, DefaultRootWindow(dpy), 0, 0,
		         640, 480, 0, blackColor, blackColor);
    buffer_pixmap = XCreatePixmap(dpy, w, 640, 480, depth);
    pixmap	  = XCreatePixmap(dpy, w, 640, 480, depth);
    XSelectInput (dpy, w, StructureNotifyMask | ExposureMask | 
		           KeyPressMask | ButtonPressMask); 
    XMapWindow(dpy, w);
    gc = XCreateGC(dpy, w, 0, NIL);
    // Set the color regs
    set_default_palette();
    for (int c = 0; c < 256; c++) {
         int r, g;
         r = c/12;
         g = c%12;
         if ((r != 0) || (g != 0)) {
              g += 2;
              if (g >= 12) g = 11;
         }
        darken_tab[c] = r*12+g;
    }
    //                                                                                     
    // Catch nasty signals
    signals_init();
    // Play the game
    play_game();

    // Quit
    quit(NULL);

    // Exit
    return 0;
}



/*
 * Exit (ala "exit()").  If 'str' is NULL, do "exit(0)".
 * If 'str' begins with "+" or "-", do "exit(atoi(str))".
 * Otherwise, output str and exit with an error code of -1.
 * But always use 'quit_aux', if set, before anything else.
 */
void quit(char *str)
{
    // Kill system-specific stuff
    kill_system_specific();

    /* Success */
    if (!str) exit(0);

    /* Extract a "special error code" */
    if ((str[0] == '-') || (str[0] == '+')) (void)(exit(atoi(str)));

    // Print the message on stderr
    if (str) fprintf(stderr, "xutumno: %s\n", str);

    /* Failure */
    exit(-1);
}


/*
 * Convert a scan code along with the status of the modifiers to an ASCII code
 */
int convert(int scan, bool shift, bool caps)
{
    // Shift effectively switches caps lock
    if (shift) caps = !caps;

    switch (scan) {
        case KEY_ESCAPE:
            return ESCAPE;
        case KEY_1:
            if (shift) return '!';
            return '1';
        case KEY_2:
            if (shift) return '@@';
            return '2';
        case KEY_3:
            if (shift) return '#';
            return '3';
        case KEY_4:
            if (shift) return '$';
            return '4';
        case KEY_5:
            if (shift) return '%';
            return '5';
        case KEY_6:
            if (shift) return '^';
            return '6';
        case KEY_7:
            if (shift) return '&';
            return '7';
        case KEY_8:
            if (shift) return '*';
            return '8';
        case KEY_9:
            if (shift) return '(';
            return '9';
        case KEY_0:
            if (shift) return ')';
            return '0';
        case KEY_MINUS:
            if (shift) return '_';
            return '-';
        case KEY_EQUAL:
            if (shift) return '+';
            return '=';
        case KEY_BACKSPACE:
            return '\b';
        case KEY_TAB:
            return '\t';
        case KEY_Q:
            if (caps) return 'Q';
            return 'q';
        case KEY_W:
            if (caps) return 'W';
            return 'w';
        case KEY_E:
            if (caps) return 'E';
            return 'e';
        case KEY_R:
            if (caps) return 'R';
            return 'r';
        case KEY_T:
            if (caps) return 'T';
            return 't';
        case KEY_Y:
            if (caps) return 'Y';
            return 'y';
        case KEY_U:
            if (caps) return 'U';
            return 'u';
        case KEY_I:
            if (caps) return 'I';
            return 'i';
        case KEY_O:
            if (caps) return 'O';
            return 'o';
        case KEY_P:
            if (caps) return 'P';
            return 'p';
        case KEY_LBRACKET:
            if (shift) return '{';
            return '[';
        case KEY_RBRACKET:
            if (shift) return '}';
            return ']';
        case KEY_ENTER:
            return '\n';
        case KEY_A:
            if (caps) return 'A';
            return 'a';
        case KEY_S:
            if (caps) return 'S';
            return 's';
        case KEY_D:
            if (caps) return 'D';
            return 'd';
        case KEY_F:
            if (caps) return 'F';
            return 'f';
        case KEY_G:
            if (caps) return 'G';
            return 'g';
        case KEY_H:
            if (caps) return 'H';
            return 'h';
        case KEY_J:
            if (caps) return 'J';
            return 'j';
        case KEY_K:
            if (caps) return 'K';
            return 'k';
        case KEY_L:
            if (caps) return 'L';
            return 'l';
        case KEY_SEMICOLON:
            if (shift) return ':';
            return ';';
        case KEY_QUOTE:
            if (shift) return '"';
            return '\'';
        case KEY_BACKQUOTE:
            if (shift) return '~';
            return '`';
        case KEY_BACKSLASH:
            if (shift) return '|';
            return '\\';
        case KEY_Z:
            if (caps) return 'Z';
            return 'z';
        case KEY_X:
            if (caps) return 'X';
            return 'x';
        case KEY_C:
            if (caps) return 'C';
            return 'c';
        case KEY_V:
            if (caps) return 'V';
            return 'v';
        case KEY_B:
            if (caps) return 'B';
            return 'b';
        case KEY_N:
            if (caps) return 'N';
            return 'n';
        case KEY_M:
            if (caps) return 'M';
            return 'm';
        case KEY_COMMA:
            if (shift) return '<';
            return ',';
        case KEY_PERIOD:
            if (shift) return '>';
            return '.';
        case KEY_SLASH:
            if (shift) return '?';
            return '/';
        case KEYPAD_STAR:
            return '*';
        case KEY_SPACE:
            return ' ';
        case KEYPAD_MINUS:
            return '-';
        case KEYPAD_PLUS:
            return '+';
    }
    return 0;
}

