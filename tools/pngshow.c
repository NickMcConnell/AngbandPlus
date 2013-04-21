#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/libpng/png.h"
#include <stdlib.h>

extern int read_png_info(FILE *fp, png_structp *pngPtr, png_infop *infoPtr);
extern int clean_png_info(FILE *fp, png_structp *pngPtr, png_infop *infoPtr);
extern XImage * readPng(Display *dpy, char *fname);

typedef unsigned char byte;

XImage *
readPng(Display *dpy, char *fname) {

   png_structp png_ptr;
   png_infop info_ptr;
   XImage *res = NULL;
   Visual *visual;
   png_uint_32 width, height;
   int thescreen;

   int bit_depth, color_type;
   unsigned int depth;
   char *Data; // the image data
   int retval = 0;
   
   FILE *fp;

   if ((fp = fopen(fname, "rb")) == NULL)
      return NULL;

   retval = read_png_info(fp, &png_ptr, &info_ptr);

//   fprintf(stderr, "Return value is %d\n", retval);
   
   /* At this point you have read the entire image */
   width      = info_ptr->width;
   height     = info_ptr->height;
   color_type = info_ptr->color_type;
   bit_depth  = info_ptr->bit_depth;

   thescreen = DefaultScreen(dpy);
   Data = malloc(width*height*4+1);
   visual = DefaultVisual(dpy, thescreen);
   depth = DefaultDepth(dpy, thescreen);
 
   res = XCreateImage(dpy, visual, depth, ZPixmap, 0 /*offset*/,
		      Data, width, height, 32 /*bitmap_pad*/, 0 /*bytes_per_line*/);

   if (res == NULL)
       return res;

   fprintf(stderr, "Read file! %ld,%ld %d %d %d\n", width, height,
	   color_type, bit_depth, info_ptr->pixel_depth);

   {
       
       unsigned int x, y;
       unsigned long whitepixel, blackpixel;
       unsigned int offset = info_ptr->pixel_depth / 8;

       png_bytepp row_pointers = malloc(height*sizeof(png_bytep));
       row_pointers = png_get_rows(png_ptr, info_ptr);
       whitepixel = WhitePixel(dpy, thescreen);
       blackpixel = BlackPixel(dpy, thescreen);

       
       for (y=0; y < height; y++) {
	   png_bytep arr = row_pointers[y];
	   for (x=0; x < width; x++) {
	       byte red = arr[x*offset];
	       byte green = arr[x*offset+1];
	       byte blue = arr[x*offset+2];
	       unsigned long pixel;

	       if (red == 0 && green == 0 && blue == 0)
		   pixel = blackpixel;
	       else if (red == 255 && green == 255 && blue == 255)
		   pixel = whitepixel;
	       else {
//		   pixel = get_pixel(arr[x*3], arr[x*3+1], arr[x*3+2]);
		   pixel = (red<<16) | (green<<8) | blue;
	       }
	       
	       XPutPixel(res, x, y, pixel);
//		XPutPixel(pic, x, y, xcolour.pixel);
	       //fprintf(stderr, "Val %d %d %d\n", arr[0], arr[1], arr[2]);
	   }
	   
       }
       
   }
   
   clean_png_info(fp, &png_ptr, &info_ptr);
   
   return res;
}

int
main(int argc, char *argv[]) {

    Window thewindow;
    GC thegc;
    int thescreen;
//    char *Data;
    Display *dpy;
    XImage *pic = NULL;
    unsigned long whitepixel, blackpixel;

    dpy = XOpenDisplay("");
    thescreen = DefaultScreen(dpy);
//    cmap = DefaultColormapOfScreen(DefaultScreenOfDisplay(thedisplay));

    if (argc> 1)
	pic = readPng(dpy, argv[1]);

    if (!pic)
	return -1;

    whitepixel = WhitePixel(dpy, thescreen);
    blackpixel = BlackPixel(dpy, thescreen);

    thewindow = XCreateSimpleWindow(dpy, RootWindow(dpy, thescreen),
				    5, 5, pic->width, pic->height, 2, whitepixel, blackpixel);	

    thegc = XCreateGC(dpy, thewindow, 0, 0);
    XMapRaised(dpy, thewindow);

    
    XPutImage(dpy, thewindow, thegc, pic, 0, 0, 0, 0, pic->width, pic->height);

    
    XSelectInput(dpy, thewindow, ButtonPressMask | ExposureMask);

    {
	int done = 0;
	XEvent theevent;
	while (!done) {
	    XNextEvent(dpy, &theevent);
	    switch (theevent.type) {
	    case ButtonPress:
		done = 1;
		break;
	    
	    case Expose:
		XPutImage(dpy, thewindow, thegc, pic, 0, 0, 0, 0, pic->width, pic->height);
		break;
	    }

	}
    }
    
    XFreeGC(dpy, thegc);
    XDestroyWindow(dpy, thewindow);
    XCloseDisplay(dpy);
    
    return 0;
}
