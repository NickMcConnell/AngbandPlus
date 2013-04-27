// File: sys-lin/main.cpp
// Main system-specific file for Linux machines


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

/*
 * Port to the XWindow system by Marcello Sega and Marco Vecchiocattivi.
 * sega@physics.it (2002)
 *
 */  


#include "../utumno.h"
#include "sys-lin.h"


// Include the character bitmaps, palette data, mouse cursor, and fonts
#include "chars.h"
#include "palette.h"
#include "cursor.h"
#include "bold.h"
#include "regular.h"
#include <unistd.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xutil.h>

#ifdef USE_MIT_SHM
// SAW Uses MIT shared memory extention to avoid heavy data streaming 
//     between X server and client due to XImages
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <X11/extensions/XShm.h>
#endif

#define NIL (0)
// ** MV
#ifdef USE_PTHREAD
# include <pthread.h>
#endif


/*  Xlib global variables */

Display *dpy;
int blackColor;
int whiteColor;
int pxlsize;
Window w;
GC gc;
#ifdef USE_MIT_SHM
XShmSegmentInfo shminfo;
#endif
XEvent report, report_mouse;
unsigned int code_shift_l;
unsigned int code_shift_r;

unsigned int x_palette[500];

Pixmap buffer_pixmap;
XImage *ximage_buffer;		// used for double buffering

bool shift_flag = FALSE;
bool mouse_flag = FALSE;

/*                        */
struct CFrame
{
  byte height;
  u16b ndata;
  byte *data;
};

struct CScene
{
  int nframes;
  char *name;
  CFrame *frames;		// allocate nviews*nframes of these
};

struct CTile
{
  char *name;
  int nviews;
  int nscenes;
  CScene *scenes;
};

// Maximum number of tiles
const int MAX_TILES = 1024;


// The height/width of the graphics mode
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

// Darkening info
static byte darken_tab[256];

// A virtual screen
//static byte *virtual_screen = NULL;

// The tile database
static int ntiles;
static CTile *tiles[MAX_TILES];

// The current clipping rectangle
static int cx1 = 0, cy1 = 0, cx2 = SCREEN_WIDTH - 1, cy2 = SCREEN_HEIGHT -1;

// ** MV -> threaded routine to control the X events.
// ...rigth now, just for expose, mouse and shift status...

#ifdef USE_PTHREAD

// variable that holds the thread id
pthread_t thr_id;

// the mouse structures where to store and read the mouse-status
// both for right click and left click
struct thr_mouse_status_t
{
  bool click;
  int x;
  int y;
  bool shift;
};

static struct thr_mouse_status_t thr_last_lmouse = { false, 0, 0, false };
static struct thr_mouse_status_t thr_last_rmouse = { false, 0, 0, false };
static struct thr_mouse_status_t thr_mouse = { false, 0, 0, false };

bool thr_shift_status = 0;
bool thr_caps_status = 0;

// and the mutex lock for mouse structures and modifiers values
pthread_mutex_t thr_mouse_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t thr_last_mouse_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t thr_modifiers_mutex = PTHREAD_MUTEX_INITIALIZER;

// the test function used in the XCheckIfEvent function call to test for shift/caps status
// this is called for each event in the queue and search for a press or a release of shift/caps key

int
test_for_modifier_keys (Display * test_dpy, XEvent * test_event,
			XPointer test_arg)
{
  if (test_event->type == KeyPress || test_event->type == KeyRelease)
    {
      if (test_event->xkey.keycode == code_shift_l ||
	  test_event->xkey.keycode == code_shift_r)
	{
	  pthread_mutex_lock (&thr_modifiers_mutex);
	  thr_shift_status = (test_event->type == KeyPress);	// set the shift status ...
	  pthread_mutex_unlock (&thr_modifiers_mutex);
	  return true;		// ...  and tell XCheckIfEvent to discard the event from the queue
	}
/*	
 *	SAW find the right way to get caps lock status 
 *	
 	else if
	(test_event->xkey.keycode == code_lock)
	{
		pthread_mutex_lock (&thr_modifiers_mutex);
		thr_caps_status = (test_event->type == KeyPress);
                pthread_mutex_unlock (&thr_modifiers_mutex);
                return true;
	}*/
      else
	return false;
    }
  return false;
}


/* 
 * the threaded routine that handles all the X events.
 * ... for now just expose/mouse/shift events ...
 * we still need to think of a smart way of handling the keyboard
 */
void *
handle_x_events (void *unused_arg)
{
  bool found = false;
  XEvent thr_event;
  for (;;)
    {
      /* 
       * Get exclusive access to the display 
       * We need it because there are still some calls in the main thread that search&modify the
       * event queue. We must think of a nice way to handle all these call in this thread...
       * ... maybe another queue?
       */
      XLockDisplay (dpy);

      // search all the queue for the last expose event
      while (XCheckTypedEvent (dpy, Expose, &thr_event))
	found = true;
      // if found, go ahead with it
      if (found)
	{
	  found = false;
	  partial_screen_refresh (thr_event.xexpose.x, thr_event.xexpose.y,
				  thr_event.xexpose.width,
				  thr_event.xexpose.height);
	}

      // Watch for the mouse movement. Let's use only the last unseen position
      while (XCheckTypedEvent (dpy, MotionNotify, &thr_event))
	found = true;
      if (found)
	{
	  found = false;

	  if (pthread_mutex_trylock (&thr_mouse_mutex) != EBUSY)
	    {
	      thr_mouse.x = thr_event.xmotion.x;
	      thr_mouse.y = thr_event.xmotion.y;
	      pthread_mutex_unlock (&thr_mouse_mutex);
	    }
	  // Here we check and set even the modifiers status
	  if (pthread_mutex_trylock (&thr_modifiers_mutex) != EBUSY)
	    {
	      thr_shift_status = thr_event.xmotion.state & ShiftMask;
	      thr_caps_status = thr_event.xmotion.state & LockMask;
	      pthread_mutex_unlock (&thr_modifiers_mutex);
	    }
	}

      // Last left&right mouse click
      while (XCheckTypedEvent (dpy, ButtonPress, &thr_event))
	found = true;
      if (found)
	{
	  found = false;

	  if (pthread_mutex_trylock (&thr_last_mouse_mutex) != EBUSY)
	    {
	      if (thr_event.xbutton.button == Button1)
		{
		  thr_last_lmouse.click = true;
		  thr_last_lmouse.x = thr_event.xbutton.x;
		  thr_last_lmouse.y = thr_event.xbutton.y;
		  thr_last_lmouse.shift = thr_event.xbutton.state & ShiftMask;
		}
	      if ((thr_event.xbutton.button == Button2) || (thr_event.xbutton.button == Button3))
		{
		  thr_last_rmouse.click = true;
		  thr_last_rmouse.x = thr_event.xbutton.x;
		  thr_last_rmouse.y = thr_event.xbutton.y;
		  thr_last_rmouse.shift = thr_event.xbutton.state & ShiftMask;
		}
	      pthread_mutex_unlock (&thr_last_mouse_mutex);
	    }
	}

      /* 
       *  Let's search the event queue for shift/caps key press/release
       *  we set the modifier status right inside the test function
       *  again we need the last status...
       */

      while (XCheckIfEvent (dpy, &thr_event, &test_for_modifier_keys, NULL));

      // Release the display
      XUnlockDisplay (dpy);

      // And go to sleep...
      usleep (1000);
    }

}

#endif /* USE_PTHREAD */

// Verify against the clipping rectangle
#define test_clip(x, y) (((x) < cx1) || ((x) > cx2) || ((y) < cy1) || ((y) > cy2))
// Put a pixel on the virtual screen
// Ignores clipping
#define put_pixel(x,y,color) {XPutPixel(ximage_buffer, (x), (y), x_palette[(color)]);}
/*
 * Flush changes to the screen
 */
void
screen_refresh (void)
{
  //double time; 

#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
# ifndef USE_MIT_SHM
  XPutImage (dpy, w, gc, ximage_buffer, 0, 0, 0, 0, SCREEN_WIDTH,
	     SCREEN_HEIGHT);
# else
   
  XShmPutImage (dpy, w, gc, ximage_buffer, 0, 0, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, true);
#endif
#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif
}

void
partial_screen_refresh (int x, int y, int width, int height)
{
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
# ifndef USE_MIT_SHM
  
  XPutImage (dpy, w, gc, ximage_buffer, x, y, x, y, width, height);
# else
  XShmPutImage (dpy, w, gc, ximage_buffer,x, y, x, y, width, height, true);
# endif
#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif
}

/*
 * Set a clipping rectangle
 */
void
set_clip_rect (int x1, int y1, int x2, int y2)
{
  cx1 = x1;
  cx2 = x2;
  cy1 = y1;
  cy2 = y2;
}

/*
 * Remove any clipping rectangle
 */
void
clear_clip_rect (void)
{
  set_clip_rect (0, 0, SCREEN_WIDTH -1 , SCREEN_HEIGHT -1 );
}


// Keypress input modifier flags (hard-coded by DOS)
const int K_RSHIFT = 0;		// Right shift key down
const int K_LSHIFT = 1;		// Left shift key down
const int K_CTRL = 2;		// Ctrl key down
const int K_ALT = 3;		// Alt key down
const int K_SCROLL = 4;		// Scroll lock on
const int K_NUM = 5;		// Num lock on
const int K_CAPS = 6;		// Caps lock on
const int K_INSERT = 7;		// Insert on

/*
 * Is shift depressed?
 */

// --MV: new way of handling this with the threaded function

bool
get_shift (void)
{

#ifndef USE_PTHREAD
  int key;

  if (shift_flag == TRUE)	// checked from scan_inkey_scan ...
    {
      if (XCheckTypedEvent (dpy, KeyRelease, &report))
	{
	  key = XLookupKeysym (&report.xkey, 0);
	  if (key == XK_Shift_L || key == XK_Shift_R)
	    shift_flag = FALSE;	// change flag only if Shift is released.

	}
      return TRUE;
    }
  if (XCheckTypedEvent (dpy, KeyPress, &report))	// checking now
    {
      key = XLookupKeysym (&report.xkey, 0);
      if (key == XK_Shift_L || key == XK_Shift_R)
	{
	  shift_flag = TRUE;
	  return TRUE;
	}
      XPutBackEvent (dpy, &report);

    }
  return FALSE;
#else // with threaded version

  bool ret = false;
  // we still need this usleep with the threaded version...
//  usleep(1000);
  pthread_mutex_lock (&thr_modifiers_mutex);
  ret = thr_shift_status;
  pthread_mutex_unlock (&thr_modifiers_mutex);
  return ret;
#endif
}


/*
 * Is caps lock on?
 */

// -- MV: now this is easy...
bool
get_capslock (void)
{
#ifndef USE_PTHREAD
  return FALSE;
#else
  bool ret = false;

  pthread_mutex_lock (&thr_modifiers_mutex);
  ret = thr_caps_status;
  pthread_mutex_unlock (&thr_modifiers_mutex);
  return ret;
#endif
}


/*
 * Scan for a keypress; return its scan code or 0
 */

int
scan_inkey_scan (void)
{
  KeySym key;
  XKeyEvent temp;
  usleep (1000);
  while (TRUE)
    {
#ifdef USE_PTHREAD
      bool check_event;

      XLockDisplay (dpy);
      check_event = XCheckTypedEvent (dpy, KeyPress, &report);
      XUnlockDisplay (dpy);
      if (check_event)
#else
      if (XCheckTypedEvent (dpy, ButtonPress, &report_mouse))
	mouse_flag = TRUE;
      if (XCheckTypedEvent (dpy, Expose, &report))
	partial_screen_refresh (report.xexpose.x, report.xexpose.y,
				report.xexpose.width, report.xexpose.height);
      if (XCheckTypedEvent (dpy, KeyPress, &report))
#endif
	{
	  temp=report.xkey;
	  XLockDisplay (dpy);
          XLookupString(&report.xkey,NULL,0,&key,NULL);
	  XUnlockDisplay (dpy);
	  // Wrapper from X keysyms  
	  switch (key)
	    {
	    case XK_Escape:
	      return KEY_ESCAPE;
	    case XK_1:
	      return KEY_1;
	    case XK_2:
	      return KEY_2;
	    case XK_3:
	      return KEY_3;
	    case XK_4:
	      return KEY_4;
	    case XK_5:
	      return KEY_5;
	    case XK_6:
	      return KEY_6;
	    case XK_7:
	      return KEY_7;
	    case XK_8:
	      return KEY_8;
	    case XK_9:
	      return KEY_9;
	    case XK_0:
	      return KEY_0;
	    case XK_minus:
	      return KEY_MINUS;
	    case XK_equal:
	      return KEY_EQUAL;
	    case XK_BackSpace:
	      return KEY_BACKSPACE;
	    case XK_Tab:
	      return KEY_TAB;
	    case XK_q:
	    case XK_Q:
	      return KEY_Q;
	    case XK_w:
	    case XK_W:
	      return KEY_W;
	    case XK_e:
	    case XK_E:
	      return KEY_E;
	    case XK_r:
	    case XK_R:
	      return KEY_R;
	    case XK_t:
	    case XK_T:
	      return KEY_T;
	    case XK_y:
	    case XK_Y:
	      return KEY_Y;
	    case XK_u:
	    case XK_U:
	      return KEY_U;
	    case XK_i:
	    case XK_I:
	      return KEY_I;
	    case XK_o:
	    case XK_O:
	      return KEY_O;
	    case XK_p:
	    case XK_P:
	      return KEY_P;
	    case XK_bracketleft:
	      return KEY_LBRACKET;
	    case XK_bracketright:
	      return KEY_RBRACKET;
	    case XK_Return:
	      return KEY_ENTER;
	    case XK_a:
	    case XK_A:
	      return KEY_A;
	    case XK_s:
	    case XK_S:
	      return KEY_S;
	    case XK_d:
	    case XK_D:
	      return KEY_D;
	    case XK_f:
	    case XK_F:
	      return KEY_F;
	    case XK_g:
	    case XK_G:
	      return KEY_G;
	    case XK_h:
	    case XK_H:
	      return KEY_H;
	    case XK_j:
	    case XK_J:
	      return KEY_J;
	    case XK_k:
	    case XK_K:
	      return KEY_K;
	    case XK_l:
	    case XK_L:
	      return KEY_L;
	    case XK_semicolon:
	      return KEY_SEMICOLON;
	    case XK_quoteright:
	      return KEY_BACKQUOTE;
	    case XK_backslash:
	      return KEY_BACKSLASH;
	    case XK_z:
	    case XK_Z:
	      return KEY_Z;
	    case XK_x:
	    case XK_X:
	      return KEY_X;
	    case XK_c:
	    case XK_C:
	      return KEY_C;
	    case XK_v:
	    case XK_V:
	      return KEY_V;
	    case XK_b:
	    case XK_B:
	      return KEY_B;
	    case XK_n:
	    case XK_N:
	      return KEY_N;
	    case XK_m:
	    case XK_M:
	      return KEY_M;
	    case XK_comma:
	      return KEY_COMMA;
	    case XK_period:
	      return KEY_PERIOD;
	    case XK_slash:
	      return KEY_SLASH;
	    case XK_asterisk:
	      return KEYPAD_STAR;
	    case XK_space:
	      return KEY_SPACE;
	    case XK_F1:
	      return KEY_F1;
	    case XK_F2:
	      return KEY_F2;
	    case XK_F3:
	      return KEY_F3;
	    case XK_F4:
	      return KEY_F4;
	    case XK_F5:
	      return KEY_F5;
	    case XK_F6:
	      return KEY_F6;
	    case XK_F7:
	      return KEY_F7;
	    case XK_F8:
	      return KEY_F8;
	    case XK_F9:
	      return KEY_F9;
	    case XK_F10:
	      return KEY_F10;
	    case XK_KP_7:
	      return KEYPAD_7;
	    case XK_KP_8:
	      return KEYPAD_8;
	    case XK_KP_9:
	      return KEYPAD_9;
	    case XK_KP_Subtract:
	      return KEYPAD_MINUS;
	    case XK_KP_4:
	      return KEYPAD_4;
	    case XK_KP_5:
	      return KEYPAD_5;
	    case XK_KP_6:
	      return KEYPAD_6;
	    case XK_KP_Add:
	      return KEYPAD_PLUS;
	    case XK_KP_1:
	      return KEYPAD_1;
	    case XK_KP_2:
	      return KEYPAD_2;
	    case XK_KP_3:
	      return KEYPAD_3;
	    case XK_KP_0:
	      return KEYPAD_0;
	    case XK_KP_Decimal:
	      return KEYPAD_PERIOD;
	    case XK_Up:
	      return KEY_UP;
	    case XK_Left:
	      return KEY_LEFT;
	    case XK_Right:
	      return KEY_RIGHT;
	    case XK_Down:
	      return KEY_DOWN;
	    case XK_Home:
	      return KEY_HOME;
	    case XK_End:
	      return KEY_END;
	    case XK_Page_Up:
	      return KEY_PGUP;
	    case XK_Page_Down:
	      return KEY_PGDN;
/*
 * SAW : return -1 for other KeySyms; otherwise the code 
 * isn't passed to convert().
 */	      
	    default:
	    return (-1); 
	    }
	}
      return 0;
    }
}


// Save/restore the virtual screen

byte *
save_screen (void)
{
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
#ifndef USE_MIT_SHM
  ximage_buffer = XGetImage (dpy, w, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT, AllPlanes, ZPixmap);	//SAW mem leak ?
#else
  XShmGetImage(dpy, w, ximage_buffer, 0, 0, AllPlanes);
#endif
#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif
  return 0;
}

void
restore_screen (byte * from)
{
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
# ifndef USE_MIT_SHM  
  XPutImage (dpy, w, gc, ximage_buffer, 0, 0, 0, 0, SCREEN_WIDTH,
	     SCREEN_HEIGHT);
# else
  XShmPutImage (dpy, w, gc, ximage_buffer, 0, 0, 0, 0, SCREEN_WIDTH,
		               SCREEN_HEIGHT, true);
# endif
#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif
}

// -- MV: new threaded version...
void
get_mouse_status (int *x, int *y, bool * left)
{
#ifndef USE_PTHREAD
  Window root, child;
  int rx, ry, xx, yy;
  unsigned int mask;

  if (XQueryPointer (dpy, w, &root, &child, &rx, &ry, &xx, &yy, &mask))
    {
      *x = xx;
      *y = yy;
      *left = FALSE;
      if (mask == Button1Mask)
	{
	  *left = TRUE;
	  mouse_flag = 1;
	}
    }
  else
    {
      *x = 0;
      *y = 0;
    }
#else

  pthread_mutex_lock (&thr_mouse_mutex);
  *x = thr_mouse.x;
  *y = thr_mouse.y;
  *left = thr_mouse.click;
  pthread_mutex_unlock (&thr_mouse_mutex);
#endif
}


/*
 * Get last release of left mouse button
 */
bool
get_last_left_button_release (int *rx, int *ry)
{
#ifndef USE_PTHREAD
  if (mouse_flag == FALSE)
    if (!XCheckTypedEvent (dpy, ButtonRelease, &report_mouse))
      return FALSE;
  if (report_mouse.xbutton.button != Button1)
    return FALSE;
  *rx = report_mouse.xbutton.x;
  *ry = report_mouse.xbutton.y;
  mouse_flag = FALSE;
  return TRUE;
#else
  bool ret;
  pthread_mutex_lock (&thr_last_mouse_mutex);
  *rx = thr_last_lmouse.x;
  *ry = thr_last_lmouse.y;
  ret = thr_last_lmouse.click;
  thr_last_lmouse.click = false;
  pthread_mutex_unlock (&thr_last_mouse_mutex);

  return ret;
#endif
}


/*
 * Get last release of right mouse button
 */

bool
get_last_right_button_release (int *rx, int *ry)
{
#ifndef USE_PTHREAD
  if (mouse_flag == FALSE)
    if (!XCheckTypedEvent (dpy, ButtonRelease, &report_mouse))
      return FALSE;
  if (report_mouse.xbutton.button == Button1)
    return FALSE;
  *rx = report_mouse.xbutton.x;
  *ry = report_mouse.xbutton.y;
  mouse_flag = FALSE;
  return TRUE;
#else
  bool ret;
  pthread_mutex_lock (&thr_last_mouse_mutex);
  *rx = thr_last_rmouse.x;
  *ry = thr_last_rmouse.y;
  ret = thr_last_rmouse.click;
  thr_last_rmouse.click = false;
  pthread_mutex_unlock (&thr_last_mouse_mutex);

  return ret;
#endif
}


/*
 * Mouse stuff
 */
// ­- MV: We're using X so we don't need these calls...

void
virt_draw_mouse (int x, int y)
{
}

void
virt_kill_mouse (int x, int y)
{
}

/*
 * Set a palette entry
 *
 * c, r, g, b all from 0..255
 *
 * WARNING: do not use except at times when it is safe!
 */

void
set_palette_entry (int c, int r, int g, int b)
{
// -- MV: Thread safe calls
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
  XColor xcolor;
  Colormap colormap;

  colormap = DefaultColormap (dpy, DefaultScreen (dpy));
  xcolor.red = (unsigned short) ((r * 65535) / 256);
  xcolor.green = (unsigned short) ((g * 65535) / 256);
  xcolor.blue = (unsigned short) ((b * 65535) / 256);
  XAllocColor (dpy, colormap, &xcolor);
  x_palette[c] = xcolor.pixel;

#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif

}

/*
 * Reset the palette to the default
 */
void
set_default_palette (void)
{
  for (int c = 0; c < 256; c++)
    set_palette_entry (c, palette[c * 3], palette[c * 3 + 1],
		       palette[c * 3 + 2]);

}


/*
 * Free Resources
 */

// ** MV

static void
kill_system_specific (void)
{
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
  XFreeGC (dpy, gc);
  XFreePixmap (dpy, buffer_pixmap);
  XDestroyImage (ximage_buffer);
  XDestroyWindow (dpy, w);
  XCloseDisplay (dpy);
}


/*
 * Starting here are the graphics primitives.
 */


// Begin or end a batch of pixel draws
void
start_pixel_draw (void)
{
	XLockDisplay(dpy);
}
void
end_pixel_draw (void)
{
	XUnlockDisplay(dpy);
}

// Draw a pixel to the virtual screen
void
draw_pixel (int x, int y, byte c)
{
  if (!test_clip (x, y))
    put_pixel (x, y, c);
}

// Draw a box
void
box (int x1, int y1, int x2, int y2, byte color)
{
  int y;
  if ((x1 > cx2) || (x2 < cx1))
    return;
  if (x2 < x1)
    return;
  if (x1 < cx1)
    x1 = cx1;
  if (x2 > cx2)
    x2 = cx2;
  for (y = y1; y < y2; y++)
    horiz_line (x1, x2, y, color);
}

// Set the whole screen to one color
// Bypasses any clipping rectangle

void
blank_screen (byte color)
{
  int i, j;
  if (color != COLOR_BLACK)
    {
#ifdef USE_THREAD
      XLockDisplay (dpy);
#endif
      for (i = 0; i < SCREEN_HEIGHT; i++)
	for (j = 0; j < SCREEN_WIDTH; j++)
	  put_pixel (j, i, 0);

#ifdef USE_THREAD
      XUnlockDisplay (dpy);
#endif
    }
  else
    memset (ximage_buffer->data, 0, SCREEN_HEIGHT * SCREEN_WIDTH * pxlsize);
}

// Draw a horizontal or vertical line
void
horiz_line (int x1, int x2, int y, byte color)
{
  int x;
  if ((y < cy1) || (y > cy2))
    return;

  if ((x1 > cx2) || (x2 < cx1))
    return;
  if (x2 < x1)
    return;
  if (x1 < cx1)
    x1 = cx1;
  if (x2 > cx2)
    x2 = cx2;
#ifdef USE_THREAD
  XLockDisplay (dpy);
#endif
  x = x1;
  put_pixel (x, y, color);  
  for (x = x1 +1 ; x <= x2; x++)
	  put_pixel (x, y, color);
#ifdef USE_THREAD
  XUnlockDisplay (dpy);
#endif

}

void
vert_line (int x, int y1, int y2, byte color)
{
  if ((x < cx1) || (x > cx2))
    return;

  if ((y1 > cy2) || (y2 < cy1))
    return;
  if (y2 < y1)
    return;
  if (y1 < cy1)
    y1 = cy1;
  if (y2 > cy2)
    y2 = cy2;
#ifdef USE_THREAD
  XLockDisplay (dpy);
#endif

  for (int y = y1; y <= y2; y++)
    put_pixel (x, y, color);
#ifdef USE_THREAD
  XUnlockDisplay (dpy);
#endif

}


// Draw a rectangle
void
rectangle (int x1, int y1, int x2, int y2, byte color)
{
  horiz_line (x1, x2, y1, color);
  horiz_line (x1, x2, y2, color);
  vert_line (x1, y1, y2, color);
  vert_line (x2, y1, y2, color);
}

// Draw a character of text
static void
put_character (int x, int y, int c, byte color)
{
  int cx, cy, px, py;
#ifdef USE_THREAD
  XLockDisplay (dpy);
#endif

  for (cy = 0; cy < 16; cy++)
    {
      py = y + cy;
      if ((py < cy1) || (py > cy2))
	continue;
      for (cx = 0; cx < 8; cx++)
	{
	  px = x + cx;
	  if ((px < cx1) || (px > cx2))
	    continue;
	  if ((char_map[c * 16 + cy] >> (7 - cx)) & 1)
	    put_pixel (px, py, color);
	}
    }
#ifdef USE_THREAD
  XUnlockDisplay (dpy);
#endif

}

// Draw a string
void
put_string (int x, int y, char *c, byte color)
{
  int i, len = strlen (c);

  for (i = 0; i < len; i++)
    {
      put_character (x + 8 * i, y, c[i], color);
    }
}

int
get_char_width (int c, int font)
{
  if ((c < 32) || (c > 127))
    return 0;

  switch (font)
    {
    case FONT_REGULAR:
      return font_regular_widths[c - 32];
    case FONT_BOLD:
      return font_bold_widths[c - 32];
    default:
      return 0;
    }
}

int
string_width (char *c, int font)
{
  int i, len = strlen (c);
  int pixlen = 0;

  for (i = 0; i < len; i++)
    {
      pixlen += get_char_width (c[i], font);
    }
  return pixlen;
}

// Draw a character of text in a font
static void
put_character_font (int x, int y, int c, byte color, int font)
{
  int cx, cy, px, py;
  int height, width, ohang;
  int data[16], i;

  if ((c < 32) || (c > 127))
    return;

  switch (font)
    {
    case FONT_REGULAR:
      height = font_regular_height;
      ohang = font_regular_overhang;
      width = font_regular_widths[c - 32];
      for (i = 0; i < height; i++)
	data[i] = font_regular_data[(c - 32) * height + i];
      break;

    case FONT_BOLD:
      height = font_bold_height;
      ohang = font_bold_overhang;
      width = font_bold_widths[c - 32];
      for (i = 0; i < height; i++)
	data[i] = font_bold_data[(c - 32) * height + i];
      break;

    default:
      return;
    }
#ifdef USE_THREAD
  XLockDisplay (dpy);
#endif

  for (cy = 0; cy < height; cy++)
    {
      py = y + cy;
      if ((py < cy1) || (py > cy2))
	continue;
      for (cx = 0; cx < width + ohang; cx++)
	{
	  px = x + cx;
	  if ((px < cx1) || (px > cx2))
	    continue;
	  if ((data[cy] >> cx) & 1)
	    put_pixel (x + cx, y + cy, color);
	}
    }
#ifdef USE_THREAD
  XUnlockDisplay (dpy);
#endif

}

// Draw a string in a font
void
put_text (int x, int y, char *c, byte color, int font)
{
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif

  XTextItem items[1];
  int i, len = strlen (c);

  items[0].chars = c;
  items[0].nchars = len;
  items[0].delta = 0;
  items[0].font = XLoadFont (dpy, "*");

  for (i = 0; i < len; i++)
    {
      put_character_font (x, y, c[i], color, font);
      x += get_char_width (c[i], font);
    }

#ifdef USE_PTHREAD
  XUnlockDisplay (dpy);
#endif

}


/*
 * Load a tile
 */
void
load_tile (char *filename)
{
  FILE *f;
  char c, buf[80];
  int i, j, k;
  CTile *map;

  strcpy (buf, "dat/tile/");
  strcat (buf, filename);
  strcat (buf, ".til");

  f = fopen (buf, "rb");
  if (f == NULL)
    {
      quit (format ("Missing tilemap: %s\n", filename));
    }

  // Signature
  c = fgetc (f);
  if (c != 'T')
    {
      quit (format ("Bad signature in tilemap: %s.\n", filename));
    }
  c = fgetc (f);
  if (c != 'I')
    {
      quit (format ("Bad signature in tilemap: %s.\n", filename));
    }
  c = fgetc (f);
  if (c != 'L')
    {
      quit (format ("Bad signature in tilemap: %s.\n", filename));
    }

  // Revision
  const int PROPER_REVISION = 2;
  c = fgetc (f);
  if (c != PROPER_REVISION)
    {
      quit (format
	    ("Improper revision in tilemap %s: %d.  This version uses revision %d.",
	     filename, c, PROPER_REVISION));
    }

  // Create a tilemap
  map = new CTile;

  // Set tile name
  map->name = new char[strlen (filename) + 1];
  strcpy (map->name, filename);

  // Get nviews
  map->nviews = fgetc (f);

  // Get nscenes, allocate scenes
  map->nscenes = fgetc (f);
  map->scenes = new CScene[map->nscenes];

  // For each scene...
  for (i = 0; i < map->nscenes; i++)
    {
      CScene *scene = &map->scenes[i];

      // Get number of frames, allocate frames
      scene->nframes = fgetc (f);
      scene->frames = new CFrame[map->nviews * scene->nframes];

      // Get name
      j = 0;
      for (;;)
	{
	  char c = fgetc (f);
	  buf[j++] = c;
	  if (!c)
	    break;
	}
      scene->name = new char[j];
      strcpy (scene->name, buf);

      // For each frame and view...
      for (j = 0; j < scene->nframes * map->nviews; j++)
	{
	  CFrame *frame = &scene->frames[j];

	  // Height
	  frame->height = fgetc (f);

	  // The data
	  frame->ndata = fgetc (f);
	  frame->ndata += fgetc (f) << 8;
	  frame->data = new byte[frame->ndata];
	  for (k = 0; k < frame->ndata; k++)
	    frame->data[k] = fgetc (f);
	}
    }

  // Save the tile
  tiles[ntiles] = map;

  ntiles++;

  // Done
  fclose (f);
}


/*
 * A compare function for sort_tiles.
 */
int
tile_compare (const void *a, const void *b)
{
  CTile *x = *((CTile **) a);
  CTile *y = *((CTile **) b);
  return strcmp (x->name, y->name);
}


/*
 * Sort the tiles once they are all loaded, for purposes of binary-search access.
 */
void
sort_tiles (void)
{
  qsort (tiles, ntiles, sizeof (CTile *), tile_compare);
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
static void
draw_iso_tile (int off_x, int off_y, CFrame * frame, bool darken)
{

  int n, x, y;
  byte *d = frame->data;
  bool can_plot;

  off_y += ISO_HEIGHT - frame->height;

  x = off_x;
  y = off_y;
  if ((x > cx2) || (y > cy2))
    return;
  if (x + ISO_WIDTH - 1 < cx1)
    return;
#ifdef USE_THREAD
          XLockDisplay(dpy);
#endif

  for (;;)
    {
      can_plot = ((y >= cy1) && (x <= cx2));
      switch (*d++)
	{
	case 0:
	  n = *d++;
	  if (can_plot)
	    {
	      while ((x < cx1) && (n > 0))
		{
		  x++;
		  n--;
		  d++;
		}
	      for (; n > 0; n--)
		{
		  if (x <= cx2)
		    {
		      if (!darken)
			{
			  put_pixel (x, y, *d);
			}
		      else
			{
			  put_pixel (x + 1, y, darken_tab[*d]);
			}
		    }
		  d++;
		  x++;
		}
	    }
	  else
	    {
	      d += n;
	    }
	  break;
	case 1:
	  x += *d++;
	  break;
	case 2:
	  x = off_x;
	  y++;
	  if (y > cy2)
	  {
#ifdef USE_THREAD
              XUnlockDisplay(dpy);
#endif
	    return;
	  }
	  break;
	case 3:
	  {
#ifdef USE_THREAD
              XUnlockDisplay(dpy);
#endif
	  return;
	  }
	}
    }
}

/*
 * Find an isometric tile by name.
 */
static int
find_tile (char *name, int first, int last)
{
  if (first == last)
    {
      if (strcmp (name, tiles[first]->name) == 0)
	return first;
      return -1;
    }
  else if (last == first + 1)
    {
      if (strcmp (name, tiles[first]->name) == 0)
	return first;
      if (strcmp (name, tiles[last]->name) == 0)
	return last;
      return -1;
    }
  else
    {
      int middle, result;
      middle = (first + last) / 2;
      result = strcmp (tiles[middle]->name, name);
      if (result == 0)
	return middle;
      else if (result < 0)
	return find_tile (name, middle, last);
      else
	return find_tile (name, first, middle);
    }
}

/*
 * A global version of find_tile for general use
 */
int
locate_tile (char *name)
{
  return find_tile (name, 0, ntiles - 1);
}

/*
 * Draw an isometric tile given its index
 */
void
draw_tile_idx (int off_x, int off_y, int index, bool darken, char *scene_name,
	       int view, int frame)
{
  CTile *tile = tiles[index];
  CScene *scene = NULL;
  int i;

  // Find scene
  for (i = 0; i < tile->nscenes; i++)
    {
      if (!strcmp (tile->scenes[i].name, scene_name))
	{
	  scene = &tile->scenes[i];
	  break;
	}
    }
  if (!scene)
    {
      quit (format ("Could not find scene %s\n"));
    }

  draw_iso_tile (off_x, off_y, &scene->frames[tile->nviews * frame + view],
		 darken);
}

/*
 * Draw an isometric tile given its name
 */
void
draw_tile (int off_x, int off_y, char *tile_name, bool darken,
	   char *scene_name, int view, int frame)
{
  int result = locate_tile (tile_name);
  if (result == -1)
    {
      quit (format ("Could not find tile: %s", tile_name));
    }
  else
    draw_tile_idx (off_x, off_y, result, darken, scene_name, view, frame);
}

/*
 * Miscellaneous system routines
 */
void
bell (void)
{
  printf ("bell\n");
}


/*
 * Dump the screen in bmp format
 */

// *** MV
// SAW rewrite completely...
void
dump_screen (void)
{
// SAW to do...
}

void
create_window (void)
{
  int depth;
  XSizeHints hint;
  const char *name = "Xutumno";
  Visual *visual;

  // -- MV: change to use threads with X lib

#ifdef USE_PTHREAD
  if (XInitThreads () == 0)
    exit (printf ("Error: could not inizitialize XLib for using threads\n"));
#endif
  dpy = XOpenDisplay (NIL);
#ifdef USE_MIT_SHM

  if(!XShmQueryExtension (dpy)) 
	  exit (printf ("Error: could not use shared memory extension\n"));
#endif  
#ifdef USE_PTHREAD
  XLockDisplay (dpy);
#endif
  blackColor = BlackPixel (dpy, DefaultScreen (dpy));
  whiteColor = WhitePixel (dpy, DefaultScreen (dpy));
  depth = DefaultDepth (dpy, DefaultScreen (dpy));
  visual = DefaultVisual (dpy, DefaultScreen (dpy));
  hint.x = 0;
  hint.y = 0;
  hint.width = SCREEN_WIDTH;
  hint.height = SCREEN_HEIGHT;
  hint.min_width = SCREEN_WIDTH;
  hint.min_height = SCREEN_HEIGHT;
  hint.max_width = SCREEN_WIDTH;
  hint.max_height = SCREEN_HEIGHT;
  hint.win_gravity = StaticGravity;
  hint.flags = PPosition | PSize | PWinGravity | PMinSize | PMaxSize;
  w = XCreateSimpleWindow (dpy, DefaultRootWindow (dpy), 0, 0,
			   SCREEN_WIDTH, SCREEN_HEIGHT, 0, blackColor,
			   blackColor);

  XSetWMNormalHints (dpy, w, &hint);
  XStoreName (dpy, w, name);

  buffer_pixmap = XCreatePixmap (dpy, w, SCREEN_WIDTH, SCREEN_HEIGHT, depth);

// -- MV the mouse is treated completely in a threaded function...
#ifdef USE_PTHREAD
  XSelectInput (dpy, w,
		StructureNotifyMask | ExposureMask | PointerMotionMask |
		KeyPressMask | KeyReleaseMask | ButtonPressMask |
		ButtonReleaseMask);
#else
  XSelectInput (dpy, w, StructureNotifyMask | ExposureMask |
		KeyPressMask | KeyReleaseMask | ButtonPressMask |
		ButtonReleaseMask);
#endif

  XMapWindow (dpy, w);

  gc = XCreateGC (dpy, w, 0, NIL);
  XSetGraphicsExposures (dpy, gc, 0);
  switch (depth)
    {
    case 8:
      pxlsize = 1;
      break;
    case 16:
      pxlsize = 2;
      break;
    case 24:
      pxlsize = 4;
      break;
    default:
      pxlsize = 1;
    }
#ifndef USE_MIT_SHM  
  ximage_buffer =
    XCreateImage (dpy, visual, depth, ZPixmap, 0,NULL, SCREEN_WIDTH,
		    SCREEN_HEIGHT, 8, SCREEN_WIDTH * pxlsize);
  ximage_buffer->data=
    (char *) malloc (SCREEN_WIDTH * SCREEN_HEIGHT * pxlsize * sizeof (char));
#else
   ximage_buffer = XShmCreateImage (dpy, visual, depth, ZPixmap, NULL,
                                    &shminfo, SCREEN_WIDTH, SCREEN_HEIGHT);
   shminfo.shmid = shmget (IPC_PRIVATE,
		          ximage_buffer->bytes_per_line * ximage_buffer->height,
		          IPC_CREAT|0777);
   shminfo.shmaddr =(char *) shmat (shminfo.shmid, NULL, 0);
   ximage_buffer->data = shminfo.shmaddr;
   shminfo.readOnly = False;
   
   if(!XShmAttach (dpy, &shminfo))
	   exit(printf("Error: Server not ready to attach shared memory segment\n"));
   
#endif 
 
#ifdef USE_PTHREAD
  XFlush (dpy);
  XUnlockDisplay (dpy);
  if (pthread_create (&thr_id, NULL, handle_x_events, NULL) != 0)
    {
      perror ("Thread Creation:");
      exit (printf ("Error: could not start a thread\n"));
    }
#endif

}


/*
 * The main() function for the game
 */
int
main (void)
{
  create_window ();
  code_shift_l = (unsigned int) XKeysymToKeycode (dpy, XK_Shift_L);
  code_shift_r = (unsigned int) XKeysymToKeycode (dpy, XK_Shift_L);

  set_default_palette ();
  for (int c = 0; c < 256; c++)
    {
      int r, g;
      r = c / 12;
      g = c % 12;
      if ((r != 0) || (g != 0))
	{
	  g += 2;
	  if (g >= 12)
	    g = 11;
	}
      darken_tab[c] = r * 12 + g;
    }
  // Catch nasty signals
  signals_init ();
  // Play the game
  play_game ();
  // Quit
  quit (NULL);

  // Exit
  return 0;
}



/*
 * Exit (ala "exit()").  If 'str' is NULL, do "exit(0)".
 * If 'str' begins with "+" or "-", do "exit(atoi(str))".
 * Otherwise, output str and exit with an error code of -1.
 * But always use 'quit_aux', if set, before anything else.
 */
void
quit (char *str)
{
  // Kill system-specific stuff
  kill_system_specific ();

  /* Success */
  if (!str)
    exit (0);

  /* Extract a "special error code" */
  if ((str[0] == '-') || (str[0] == '+'))
    (void) (exit (atoi (str)));

  // Print the message on stderr
  if (str)
    fprintf (stderr, "utumno: %s\n", str);

  /* Failure */
  exit (-1);
}

int
convert (int scan, bool shift, bool caps)
{
  // Shift effectively switches caps lock
  /*
   * 
   * SAW  Ok, it is stupid to call get_shift() and get_caps()
   * but this is the price for backward compatibility.
   *
   */
  char c;
  XLookupString(&report.xkey,&c,1,NULL,NULL);
  return (int)c;
}


