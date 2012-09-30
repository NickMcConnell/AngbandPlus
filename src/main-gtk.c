/* File: main-gtk.c */

/*
 * Copyright (c) 2000 Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#ifdef USE_GTK

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>


#define NO_PADDING 0

#define MAX_TERM_DATA 8

/*
 * Extra data to associate with each "window"
 *
 * Each "window" is represented by a "term_data" structure, which
 * contains a "term" structure, which contains a pointer (t->data)
 * back to the term_data structure.
 */

typedef struct term_data term_data;

struct term_data
{
	term t;

	GtkWidget *window;
	GtkWidget *drawing_area;
	GdkFont *font;
	GdkPixmap *pixmap;
	GdkGC *gc;
	
	GdkImage *tiles;
	GdkImage *temp;

	int font_wid;
	int font_hgt;
	

	int rows;
	int cols;

	cptr name;
	cptr fontname;
};

typedef struct infoclr infoclr;

struct infoclr
{
	/* The allocated pixel */
	GdkColor pixel;
	
	/* The colour values we tried to allocate */
	byte red;
	byte green;
	byte blue;
};

/*
 * An array of "term_data" structures, one for each "sub-window"
 */
static term_data data[MAX_TERM_DATA];


/*
 * game in progress
 */
static bool game_in_progress = FALSE;


/*
 * Start a new game
 */
static bool gtk_newgame;

/*
 * Number of active terms
 */
static int num_term = 3;


#ifdef SUPPORT_GAMMA
static bool gamma_table_ready = FALSE;
static int gamma_val = 0;
#endif /* SUPPORT_GAMMA */

/* The colours */
static infoclr colours[256];

#ifdef USE_GRAPHICS
static GdkImage *tiles_norm;
static int	tile_size;
#endif /* USE_GRAPHICS */

/*
 * Hack -- Convert an RGB value to an X11 Pixel, or die.
 */
static GdkColor create_pixel(byte red, byte green, byte blue)
{
	GdkColor gcolour;

#ifdef SUPPORT_GAMMA

	if (!gamma_table_ready)
	{
		cptr str = getenv("ANGBAND_X11_GAMMA");
		if (str != NULL) gamma_val = atoi(str);

		gamma_table_ready = TRUE;

		/* Only need to build the table if gamma exists */
		if (gamma_val) build_gamma_table(gamma_val);
	}

	/* Hack -- Gamma Correction */
	if (gamma_val > 0)
	{
		red = gamma_table[red];
		green = gamma_table[green];
		blue = gamma_table[blue];
	}

#endif /* SUPPORT_GAMMA */

	/* Build the color */

	gcolour.red = red * 255;
	gcolour.green = green * 255;
	gcolour.blue = blue * 255;
	
	/* Attempt to Allocate the Parsed color */
	if (!gdk_colormap_alloc_color(gdk_colormap_get_system(),
		 &gcolour, FALSE, TRUE))
	{
		g_print("Couldn't allocate color.");
	}

	return (gcolour);
}


static void store_pixel_colors(void)
{
	int i;
	infoclr *clr;
	
	for (i = 0; i < 256; i++)
	{
		clr = &colours[i];
		
		/* Get the rgb of the colour */
		clr->red = angband_color_table[i][1];
		clr->green = angband_color_table[i][2];
		clr->blue = angband_color_table[i][3];

		/* Create the colour structure */
		clr->pixel = create_pixel(clr->red, clr->green, clr->blue);
	}
}


static errr Term_xtra_gtk_react(void)
{
	int i;
	infoclr *clr;
	for (i = 0; i < 256; i++)
	{
		clr = &colours[i];
		
		/* Has the colour changed? */
		if ((clr->red != angband_color_table[i][1])
			|| (clr->green != angband_color_table[i][2])
			|| (clr->blue != angband_color_table[i][3]))
		{
			/* Save new colour values */
			clr->red = angband_color_table[i][1];
			clr->green = angband_color_table[i][2];
			clr->blue = angband_color_table[i][3];

			/* Create the colour structure */
			clr->pixel = create_pixel(clr->red, clr->green, clr->blue);
		}
	}
	
	/* Success */
	return (0);
}

/*
 * Erase the whole term.
 */
static errr Term_clear_gtk(void)
{
	int width, height;

	term_data *td = (term_data*)(Term->data);

	g_assert(td->pixmap != NULL);
	g_assert(td->drawing_area->window != 0);

    /* Find proper dimensions for rectangle */
    gdk_window_get_size(td->drawing_area->window, &width, &height);

    /* Clear the area */
	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc,
	                   1, 0, 0, width, height);

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0, width, height);

	/* Success */
	return (0);
}

#ifdef USE_GRAPHICS

/*
 * The Win32 "BITMAPFILEHEADER" type.
 */
typedef struct BITMAPFILEHEADER
{
	u16b bfType;
	u32b bfSize;
	u16b bfReserved1;
	u16b bfReserved2;
	u32b bfOffBits;
} BITMAPFILEHEADER;


/*
 * The Win32 "BITMAPINFOHEADER" type.
 */
typedef struct BITMAPINFOHEADER
{
	u32b biSize;
	u32b biWidth;
	u32b biHeight;
	u16b biPlanes;
	u16b biBitCount;
	u32b biCompresion;
	u32b biSizeImage;
	u32b biXPelsPerMeter;
	u32b biYPelsPerMeter;
	u32b biClrUsed;
	u32b biClrImportand;
} BITMAPINFOHEADER;

/*
 * The Win32 "RGBQUAD" type.
 */
typedef struct RGBQUAD
{
	unsigned char b, g, r;
	unsigned char filler;
} RGBQUAD;


/*** Helper functions for system independent file loading. ***/

static byte get_byte(FILE *fff)
{
	/* Get a character, and return it */
	return (getc(fff) & 0xFF);
}

static void rd_byte(FILE *fff, byte *ip)
{
	*ip = get_byte(fff);
}

static void rd_u16b(FILE *fff, u16b *ip)
{
	(*ip) = get_byte(fff);
	(*ip) |= ((u16b)(get_byte(fff)) << 8);
}

static void rd_u32b(FILE *fff, u32b *ip)
{
	(*ip) = get_byte(fff);
	(*ip) |= ((u32b)(get_byte(fff)) << 8);
	(*ip) |= ((u32b)(get_byte(fff)) << 16);
	(*ip) |= ((u32b)(get_byte(fff)) << 24);
}


/*
 * Read a Win32 BMP file.
 *
 * This function replaces the old ReadRaw and RemapColors functions.
 *
 * Assumes that the bitmap has a size such that no padding is needed in
 * various places.  Currently only handles bitmaps with 3 to 256 colors.
 */
static void ReadBMP(char *Name)
{
	FILE *f;

	BITMAPFILEHEADER fileheader;
	BITMAPINFOHEADER infoheader;

	int ncol;

	int i;

	u32b x, y;

	gint32 clr_pixels[256];
	
	/* No tiles yet */
	tiles_norm = NULL;
	
	/* Open the BMP file */
	f = fopen(Name, "r");

	/* No such file */
	if (f == NULL) return;

	/* Read the "BITMAPFILEHEADER" */
	rd_u16b(f, &(fileheader.bfType));
	rd_u32b(f, &(fileheader.bfSize));
	rd_u16b(f, &(fileheader.bfReserved1));
	rd_u16b(f, &(fileheader.bfReserved2));
	rd_u32b(f, &(fileheader.bfOffBits));

	/* Read the "BITMAPINFOHEADER" */
	rd_u32b(f, &(infoheader.biSize));
	rd_u32b(f, &(infoheader.biWidth));
	rd_u32b(f, &(infoheader.biHeight));
	rd_u16b(f, &(infoheader.biPlanes));
	rd_u16b(f, &(infoheader.biBitCount));
	rd_u32b(f, &(infoheader.biCompresion));
	rd_u32b(f, &(infoheader.biSizeImage));
	rd_u32b(f, &(infoheader.biXPelsPerMeter));
	rd_u32b(f, &(infoheader.biYPelsPerMeter));
	rd_u32b(f, &(infoheader.biClrUsed));
	rd_u32b(f, &(infoheader.biClrImportand));

	/* Verify the header */
	if (feof(f) ||
	    (fileheader.bfType != 19778) ||
	    (infoheader.biSize != 40))
	{
		quit_fmt("Incorrect BMP file format %s", Name);
	}

	/* The two headers above occupy 54 bytes total */
	/* The "bfOffBits" field says where the data starts */
	/* The "biClrUsed" field does not seem to be reliable */
	/* Compute number of colors recorded */
	ncol = (fileheader.bfOffBits - 54) / 4;

	for (i = 0; i < ncol; i++)
	{
		RGBQUAD clrg;

		/* Read an "RGBQUAD" */
		rd_byte(f, &(clrg.b));
		rd_byte(f, &(clrg.g));
		rd_byte(f, &(clrg.r));
		rd_byte(f, &(clrg.filler));

		/* Analyze the color */
		clr_pixels[i] = create_pixel(clrg.r, clrg.g, clrg.b).pixel;
	}

	/* Make the normal bitmap */
	tiles_norm = gdk_image_new(GDK_IMAGE_FASTEST, gdk_visual_get_system(),
			infoheader.biWidth, infoheader.biHeight);

	/* Failure */
	if (tiles_norm == NULL)
	{
		fclose(f);
		return;
	}

	for (y = 0; y < infoheader.biHeight; y++)
	{
		u32b y2 = infoheader.biHeight - y - 1;

		for (x = 0; x < infoheader.biWidth; x++)
		{
			int ch = getc(f);

			/* Verify not at end of file XXX XXX */
			if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);

			if (infoheader.biBitCount == 24)
			{
				int c3, c2 = getc(f);
				
				/* Verify not at end of file XXX XXX */
				if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);
				
				c3 = getc(f);

				/* Verify not at end of file XXX XXX */
				if (feof(f)) quit_fmt("Unexpected end of file in %s", Name);
				
				gdk_image_put_pixel(tiles_norm, x, y2,
					create_pixel(ch, c2, c3).pixel);
			}
			else if (infoheader.biBitCount == 8)
			{
				gdk_image_put_pixel(tiles_norm, x, y2, clr_pixels[ch]);
			}
			else if (infoheader.biBitCount == 4)
			{
				gdk_image_put_pixel(tiles_norm, x, y2, clr_pixels[ch / 16]);
				x++;
				gdk_image_put_pixel(tiles_norm, x, y2, clr_pixels[ch % 16]);
			}
			else
			{
				/* Technically 1 bit is legal too */
				quit_fmt("Illegal biBitCount %d in %s",
				         infoheader.biBitCount, Name);
			}
		}
	}

	/* Close the file */
	fclose(f);
}

static void copy_pixels8(int wid, int y, int offset, int *xoffsets,
							 GdkImage *new_image)
{
	int i;
	
	/* Get source and destination */
	byte *src = &((byte *)tiles_norm->mem)[offset * tiles_norm->bpl];
    byte *dst = &((byte *)new_image->mem)[y * new_image->bpl];
	
	/* Copy to the image */
	for (i = 0; i < wid; i++)
	{
		*dst++ = src[xoffsets[i]];
	}
}

static void copy_pixels16(int wid, int y, int offset, int *xoffsets,
							 GdkImage *new_image)
{
	int i;
	
	/* Get source and destination */
	byte *src = &((byte *)tiles_norm->mem)[offset * tiles_norm->bpl];
    byte *dst = &((byte *)new_image->mem)[y * new_image->bpl];
	
	/* Copy to the image */
	for (i = 0; i < wid; i++)
	{
		*dst++ = src[2 * xoffsets[i]];
		*dst++ = src[2 * xoffsets[i] + 1];
	}
}

static void copy_pixels24(int wid, int y, int offset, int *xoffsets,
							 GdkImage *new_image)
{
	int i;
	
	/* Get source and destination */
	byte *src = &((byte *)tiles_norm->mem)[offset * tiles_norm->bpl];
    byte *dst = &((byte *)new_image->mem)[y * new_image->bpl];
	
	/* Copy to the image */
	for (i = 0; i < wid; i++)
	{
		*dst++ = src[3 * xoffsets[i]];
		*dst++ = src[3 * xoffsets[i] + 1];
		*dst++ = src[3 * xoffsets[i] + 2];
	}
}

static void copy_pixels32(int wid, int y, int offset, int *xoffsets,
							 GdkImage *new_image)
{
	int i;
	
	/* Get source and destination */
	byte *src = &((byte *)tiles_norm->mem)[offset * tiles_norm->bpl];
    byte *dst = &((byte *)new_image->mem)[y * new_image->bpl];
	
	/* Copy to the image */
	for (i = 0; i < wid; i++)
	{
		*dst++ = src[4 * xoffsets[i]];
		*dst++ = src[4 * xoffsets[i] + 1];
		*dst++ = src[4 * xoffsets[i] + 2];
		*dst++ = src[4 * xoffsets[i] + 3];
	}
}

/*
 * Resize the tiles for a given font size.
 */
static GdkImage *resize_tiles(int tile_wid, int tile_hgt)
{
	int i;

	int add, remainder, rem_tot, offset;

	int *xoffsets;

	/* Function pointer used to copy line to image */
	void (*copy_pixels)(int, int, int, int*, GdkImage*) = NULL;
	
	/* Get the size of the old image */
	int old_wid = tiles_norm->width;
	int old_hgt = tiles_norm->height;

	/* Get the size of the new image */
	int new_wid = (old_wid / tile_size) * tile_wid;
	int new_hgt = (old_hgt / tile_size) * tile_hgt;
	
	GdkImage *new_image = gdk_image_new(GDK_IMAGE_FASTEST,
						 gdk_visual_get_system(), new_wid, new_hgt);	
	
	/* Paranoia */
	if (!new_image) return (NULL);
	
	/*
	 * Calculate an offsets table, so the transformation
	 * is faster.  This is much like the Bresenham algorithm
	 */
	
	/* Set up x offset table */
	C_MAKE(xoffsets, new_wid, int);
	
	/* Initialize line parameters */
	add = old_wid / new_wid;
	remainder = old_wid % new_wid;

	/* Start at left */
	offset = 0;
	
	/* Half-tile offset so 'line' is centered correctly */
	rem_tot = new_wid / 2;

	for(i = 0; i < new_wid; i++)
	{
		/* Store into the table */
		xoffsets[i] = offset;
		
		/* Move to next entry */
		offset += add;
		
		/* Take care of fractional part */
		rem_tot += remainder;
		if (rem_tot >= new_wid)
		{
			rem_tot -= new_wid;
			offset++;
		}
	}
	
	/* Initialize copy routine */
	switch(tiles_norm->depth)
	{
		case 8: copy_pixels = copy_pixels8; break;
		case 16: copy_pixels = copy_pixels16; break;
		case 24: copy_pixels = copy_pixels24; break;
		case 32: copy_pixels = copy_pixels32; break;
 		default:
		{
			quit_fmt("Invalid bits per pixel of image: %d", new_image->bpp);
			break;
		}
	}
	
	/* Scan each row */
	
	/* Initialize line parameters */
	add = old_hgt / new_hgt;
	remainder = old_hgt % new_hgt;

	/* Start at left */
	offset = 0;
	
	/* Half-tile offset so 'line' is centered correctly */
	rem_tot = new_hgt / 2;

	for(i = 0; i < new_hgt; i++)
	{
		/* Copy pixels to new image */
		copy_pixels(new_wid, i, offset, xoffsets, new_image);
				
		/* Move to next entry */
		offset += add;
		
		/* Take care of fractional part */
		rem_tot += remainder;
		if (rem_tot >= new_hgt)
		{
			rem_tot -= new_hgt;
			offset++;
		}
	}
	
	/* Free offset table */
	C_FREE(xoffsets, new_wid, int);
	
	return (new_image);
}

#endif /* USE_GRAPHICS */

/*
 * Erase some characters.
 */
static errr Term_wipe_gtk(int x, int y, int n)
{
	term_data *td = (term_data*)(Term->data);

	g_assert(td->pixmap != NULL);
	g_assert(td->drawing_area->window != 0);

	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc,
					TRUE, x * td->font_wid, y * td->font_hgt,
					n * td->font_wid, td->font_hgt);

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                x * td->font_wid, y * td->font_hgt,
	                x * td->font_wid, y * td->font_hgt,
	                n * td->font_wid, td->font_hgt);

	/* Success */
	return (0);
}


/*
 * Draw some textual characters.
 */
static errr Term_text_gtk(int x, int y, int n, byte a, cptr s)
{
	int i;
	term_data *td = (term_data*)(Term->data);
	GdkColor color;

	/* Create the colour structure */
	color = colours[a].pixel;

	g_assert(td->pixmap != NULL);
	g_assert(td->drawing_area->window != 0);

	/* Set the forground colour */
	gdk_gc_set_foreground(td->gc, &color);

	/* Clear the line */
	Term_wipe_gtk(x, y, n);

	/* Draw the text to the pixmap */
	for (i = 0; i < n; i++)
	{
		gdk_draw_text(td->pixmap, td->font, td->gc,
					 (x + i) * td->font_wid,
					 td->font->ascent + y * td->font_hgt, s + i, 1);
	}

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                x * td->font_wid, y * td->font_hgt,
	                x * td->font_wid, y * td->font_hgt,
	                n * td->font_wid, td->font_hgt);

	/* Success */
	return (0);
}

#ifdef USE_GRAPHICS

/*
 * Draw some graphical characters.
 */
# ifdef USE_TRANSPARENCY
static errr Term_pict_gtk(int x, int y, int n, const byte *ap, const char *cp,
	const byte *tap, const char *tcp)
# else /* USE_TRANSPARENCY */
static errr Term_pict_gtk(int x, int y, int n, const byte *ap, const char *cp)
# endif /* USE_TRANSPARENCY */
{
	int i, x1, y1, x0 = x, y0 = y;

	byte a;
	char c;


#ifdef USE_TRANSPARENCY
	byte ta;
	char tc;

	int x2, y2;
	int k,l;

	guint32 pixel, blank;
#endif /* USE_TRANSPARENCY */

	term_data *td = (term_data*)(Term->data);

	y *= td->font_hgt;
	x *= td->font_wid;

	for (i = 0; i < n; ++i)
	{
		a = *ap++;
		c = *cp++;

		/* For extra speed - cache these values */
		x1 = (c&0x7F) * td->font_wid;
		y1 = (a&0x7F) * td->font_hgt;

#ifdef USE_TRANSPARENCY

		ta = *tap++;
		tc = *tcp++;

		/* For extra speed - cache these values */
		x2 = (tc&0x7F) * td->font_wid;
		y2 = (ta&0x7F) * td->font_hgt;

		/* Optimise the common case */
		if ((x1 == x2) && (y1 == y2))
		{
			/* Draw object / terrain */
			gdk_draw_image(td->pixmap, td->gc, td->tiles,
				 x1, y1, x, y,
				 td->font_wid, td->font_hgt);
		}
		else
		{

			/* Mega Hack^2 - assume the top left corner is "black" */
			blank = gdk_image_get_pixel(td->tiles, 0, td->font_hgt * 6);

			for (k = 0; k < td->font_wid; k++)
			{
				for (l = 0; l < td->font_hgt; l++)
				{
					/* If mask set... */
					if ((pixel = gdk_image_get_pixel(td->tiles, x1 + k, y1 + l)) == blank)
					{
						/* Output from the terrain */
						pixel = gdk_image_get_pixel(td->tiles, x2 + k, y2 + l);
					}

					/* Store into the temp storage. */
					gdk_image_put_pixel(td->temp, k, l, pixel);
				}
			}


			/* Draw to screen */

			gdk_draw_image(td->pixmap, td->gc, td->temp,
				 0, 0, x, y,
				 td->font_wid, td->font_hgt);
		}

#else /* USE_TRANSPARENCY */

		
		
		/* Draw object / terrain */
		gdk_draw_image(td->pixmap, td->gc, td->tiles,
				 x1, y1, x, y,
				 td->font_wid, td->font_hgt);

#endif /* USE_TRANSPARENCY */
		x += td->font_wid;
	}
	
	
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                x0 * td->font_wid, y0 * td->font_hgt,
	                x0 * td->font_wid, y0 * td->font_hgt,
	                n * td->font_wid, td->font_hgt);

	/* Success */
	return (0);
}
#endif /* USE_GRAPHICS */


static errr CheckEvent(bool wait)
{
	/* Do not wait unless requested */
	if (!wait && !gtk_events_pending()) return (1);
	
	/* Process some events */
	gtk_main_iteration();
	
	return (0);
}


static errr Term_flush_gtk(void)
{
	/* XXX */
	return (0);
}


/*
 * Handle a "special request"
 */
static errr Term_xtra_gtk(int n, int v)
{
	/* Handle a subset of the legal requests */
	switch (n)
	{
		/* Make a noise */
		case TERM_XTRA_NOISE: return (0);

		/* Flush the output */
		case TERM_XTRA_FRESH: return (0);

		/* Process random events */
		case TERM_XTRA_BORED: return (CheckEvent(0));

		/* Process Events */
		case TERM_XTRA_EVENT: return (CheckEvent(v));

		/* Flush the events */
		case TERM_XTRA_FLUSH: return (Term_flush_gtk());

		/* Handle change in the "level" */
		case TERM_XTRA_LEVEL: return (0);

		/* Clear the screen */
		case TERM_XTRA_CLEAR: return (Term_clear_gtk());

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY: usleep(1000 * v); return (0);

		/* React to changes */
		case TERM_XTRA_REACT: return (Term_xtra_gtk_react());
	}

	/* Unknown */
	return (1);
}


static errr Term_curs_gtk(int x, int y)
{
	term_data *td = (term_data*)(Term->data);

	g_assert(td->pixmap != NULL);
	g_assert(td->drawing_area->window != 0);

	gdk_gc_set_foreground(td->gc, &colours[TERM_YELLOW].pixel);

	gdk_draw_rectangle(td->pixmap, td->gc, FALSE,
	                   x * td->font_wid, y * td->font_hgt,
					   td->font_wid - 1, td->font_hgt - 1);

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                x * td->font_wid, y * td->font_hgt,
	                x * td->font_wid, y * td->font_hgt,
	                td->font_wid, td->font_hgt);

	/* Success */
	return (0);
}


static void save_game_gtk(void)
{
	if (game_in_progress && character_generated)
	{
		if (!inkey_flag)
		{
			plog("You may not do that right now.");
			return;
		}

		/* Hack -- Forget messages */
		msg_flag = FALSE;

		/* Save the game */
#ifdef ZANGBAND
		do_cmd_save_game(FALSE);
#else /* ZANGBAND */
		do_cmd_save_game();
#endif /* ZANGBAND */
	}
}

static void cleanup_angband (void)
{
	/* Do nothing, because zangband doesn't have this yet. */
}


static void hook_quit(cptr str)
{
	/* Hack - Ignore parameter */
	(void) str;
	
	gtk_exit(0);
}


static void quit_event_handler(GtkButton *was_clicked, gpointer user_data)
{
	/* Hack - Ignore parameters */
	(void) was_clicked;
	(void) user_data;

	save_game_gtk();

	quit(NULL);
}


static void destroy_event_handler(GtkButton *was_clicked, gpointer user_data)
{
	/* Hack - Ignore parameters */
	(void) was_clicked;
	(void) user_data;
	
	cleanup_angband();
	
	quit(NULL);
}


static void hide_event_handler(GtkWidget *window, gpointer user_data)
{
	/* Hack - Ignore parameter */
	(void) user_data;
	
	gtk_widget_hide(window);
}



static void new_event_handler(GtkButton *was_clicked, gpointer user_data)
{
	/* Hack - Ignore parameters */
	(void) was_clicked;
	(void) user_data;
	
	if (!game_in_progress)
	{
		/* Continue into angband code */
		game_in_progress = TRUE;

		/* Start a new game */
		gtk_newgame = TRUE;
	}
}


static void load_font(term_data *td, cptr fontname)
{
	td->font = gdk_font_load(fontname);
	td->fontname = fontname;

	/* Calculate the size of the font XXX */
	td->font_wid = gdk_char_width(td->font, '@');
	td->font_hgt = td->font->ascent + td->font->descent;
}


static void font_ok_callback(GtkWidget *widget, GtkWidget *font_selector)
{
	gchar *fontname;
	GdkGeometry win_geom;
	
	term_data *td = gtk_object_get_data(GTK_OBJECT(font_selector), "term_data");

	/* Hack - ignore widget */
	(void) widget;
	
	g_assert(td != NULL);

	fontname = gtk_font_selection_dialog_get_font_name(
						GTK_FONT_SELECTION_DIALOG(font_selector));

	/* The user hasn't selected a font? */
	if (fontname == NULL) return;

	load_font(td, fontname);
	
	/* Delete the old pixmap */
	gdk_pixmap_unref(td->pixmap);
	
	/* Note - the font-changing only affects the main window */
#ifdef USE_GRAPHICS

	/* Need to get a new tile image */	
	gdk_image_destroy(td->tiles);
	
	/* Resize tiles */
	td->tiles = resize_tiles(td->font_wid, td->font_hgt);

#ifdef USE_TRANSPARENCY

	/* Get a new temp */ 
	gdk_image_destroy(td->temp);
	
	/* Initialize the transparency temp storage*/			
	td->temp = gdk_image_new(GDK_IMAGE_FASTEST, gdk_visual_get_system(),
				td->font_wid, td->font_hgt);

#endif /* USE_TRANSPARENCY */
#endif /* USE_GRAPHICS */	
		
	/* Create a pixmap as buffer for screenupdates */
	td->pixmap = gdk_pixmap_new(td->drawing_area->window,
					 td->cols * td->font_wid, td->rows * td->font_hgt, -1);
	gtk_object_set_data(GTK_OBJECT(td->drawing_area), "pixmap", td->pixmap);
	/* td->gc = gdk_gc_new(td->drawing_area->window); */
	
	/* Clear the pixmap */
	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc, TRUE,
		                0, 0,
		                td->cols * td->font_wid, td->rows * td->font_hgt);
						
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Show the widgets */
	gtk_widget_show_all(td->window);

	/* Resize the drawing area */
	gtk_drawing_area_size(GTK_DRAWING_AREA(td->drawing_area),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
	gtk_window_set_default_size(GTK_WINDOW(td->window),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Initialize the geometry information */
	win_geom.width_inc = td->font_wid;
	win_geom.height_inc = td->font_hgt;
	win_geom.min_width = 80 * td->font_wid;
	win_geom.min_height = 24 * td->font_hgt;
	win_geom.max_width = 255 * td->font_wid;
	win_geom.max_height = 255 * td->font_hgt;
	win_geom.base_width = 1;
	win_geom.base_height = 1;
	gtk_window_set_geometry_hints(GTK_WINDOW(td->window),
				 td->drawing_area, &win_geom,
				 GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE
				| GDK_HINT_BASE_SIZE | GDK_HINT_RESIZE_INC);
	
	/* Redraw the term */
	Term_redraw();
	
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
}


static void change_font_event_handler(GtkWidget *widget, gpointer user_data)
{
	GtkWidget *font_selector = gtk_font_selection_dialog_new("Select font");

	term_data *td = user_data;
	gchar *foundery[] = { "misc", NULL};
	gchar *spacings[] = { "c", "m", NULL };
	gchar *charsets[] = { "iso8859-1", NULL};

	/* Hack - ignore widget */
	(void) widget;

	gtk_object_set_data(GTK_OBJECT(font_selector), "term_data", user_data);

	/* Filter to show only fixed-width fonts */
	gtk_font_selection_dialog_set_filter(
					GTK_FONT_SELECTION_DIALOG(font_selector),
					GTK_FONT_FILTER_BASE, GTK_FONT_ALL,
					foundery, NULL, NULL, NULL, spacings, charsets);
					
	/* Show the current font in the dialog */
	gtk_font_selection_dialog_set_font_name(
					GTK_FONT_SELECTION_DIALOG(font_selector), 
					td->fontname);

	gtk_signal_connect(
			GTK_OBJECT(GTK_FONT_SELECTION_DIALOG(font_selector)->ok_button),
	        "clicked", font_ok_callback, (gpointer)font_selector);

	/* Ensure that the dialog box is destroyed when the user clicks a button. */
	gtk_signal_connect_object(
			GTK_OBJECT(GTK_FONT_SELECTION_DIALOG(font_selector)->ok_button),
			"clicked", GTK_SIGNAL_FUNC(gtk_widget_destroy),
			(gpointer)font_selector);

	gtk_signal_connect_object(
			GTK_OBJECT(GTK_FONT_SELECTION_DIALOG(font_selector)->cancel_button),
			"clicked", GTK_SIGNAL_FUNC(gtk_widget_destroy),
			(gpointer)font_selector);
	
	gtk_widget_show(GTK_WIDGET(font_selector));
}


static void file_ok_callback(GtkWidget *widget, GtkWidget *file_selector)
{
	/* Hack - ignore widget */
	(void) widget;
	
	/* Get the savefile name */
	strcpy(savefile,
		gtk_file_selection_get_filename(GTK_FILE_SELECTION(file_selector)));

	gtk_widget_destroy(file_selector);

	/* Continue into angband */
	game_in_progress = TRUE;
}


static void open_event_handler(GtkButton *was_clicked, gpointer user_data)
{
	GtkWidget *file_selector;
	char buf[1024];

	/* Hack - ignore parameters */
	(void) was_clicked;
	(void) user_data;
	
	if (!game_in_progress)
	{
		/* Prepare the savefile path */
		path_build(buf, 1024, ANGBAND_DIR_SAVE, "*");

		file_selector = gtk_file_selection_new("Select a savefile");
		gtk_file_selection_set_filename(GTK_FILE_SELECTION(file_selector), buf);
		gtk_signal_connect(
				GTK_OBJECT(GTK_FILE_SELECTION(file_selector)->ok_button),
		    	"clicked", file_ok_callback, (gpointer)file_selector);

		/*
		 * Ensure that the dialog box is destroyed
		 * when the user clicks a button.
		 */
		gtk_signal_connect_object(
				GTK_OBJECT(GTK_FILE_SELECTION(file_selector)->ok_button),
				"clicked", GTK_SIGNAL_FUNC(gtk_widget_destroy),
				(gpointer)file_selector);

		gtk_signal_connect_object(
				GTK_OBJECT(GTK_FILE_SELECTION(file_selector)->cancel_button),
				"clicked", GTK_SIGNAL_FUNC(gtk_widget_destroy),
				(gpointer)file_selector);

		gtk_window_set_modal(GTK_WINDOW(file_selector), TRUE);
		gtk_widget_show(GTK_WIDGET(file_selector));
	}
}


static gboolean delete_event_handler(GtkWidget *widget, GdkEvent *event,
										 gpointer user_data)
{
	/* Hack - ignore parameters */
	(void) widget;
	(void) event;
	(void) user_data;
	
	save_game_gtk();

	/* Don't prevent closure */
	return (FALSE);
}


static gboolean keypress_event_handler(GtkWidget *widget, GdkEventKey *event,
										 gpointer user_data)
{
	int i, mc, ms, mo, mx;

	char msg[128];
	
	/* Hack - do not do anything until the player picks from the menu */
	if (!game_in_progress) return (TRUE);

	/* Hack - Ignore parameters */
	(void) widget;
	(void) user_data;
	
	/* Extract four "modifier flags" */
	mc = (event->state & GDK_CONTROL_MASK) ? TRUE : FALSE;
	ms = (event->state & GDK_SHIFT_MASK) ? TRUE : FALSE;
	mo = (event->state & GDK_MOD1_MASK) ? TRUE : FALSE;
	mx = (event->state & GDK_MOD3_MASK) ? TRUE : FALSE;
	
	/*
	 * Hack XXX
	 * Parse shifted numeric (keypad) keys specially.
	 */
	if ((event->state == GDK_SHIFT_MASK)
		&& (event->keyval >= GDK_KP_0) && (event->keyval <= GDK_KP_9))
	{
		/* Build the macro trigger string */
		sprintf(msg, "%cS_%X%c", 31, event->keyval, 13);
	
		/* Enqueue the "macro trigger" string */
		for (i = 0; msg[i]; i++) Term_keypress(msg[i]);
	
		/* Hack -- auto-define macros as needed */
		if (event->length && (macro_find_exact(msg) < 0))
		{
			/* Create a macro */
			macro_add(msg, event->string);
		}

		return (TRUE);
	}
	
	/* Normal keys with no modifiers */
	if (event->length && !mo && !mx)
	{
		/* Enqueue the normal key(s) */
		for (i = 0; i < event->length; i++) Term_keypress(event->string[i]);

		/* All done */
		return (TRUE);
	}


	/* Handle a few standard keys (bypass modifiers) XXX XXX XXX */
	switch ((uint) event->keyval)
	{
		case GDK_Escape:
		{
			Term_keypress(ESCAPE);
			return (TRUE);
		}

		case GDK_Return:
		{
			Term_keypress('\r');
			return (TRUE);
		}

		case GDK_Tab:
		{
			Term_keypress('\t');
			return (TRUE);
		}

		case GDK_Delete:
		case GDK_BackSpace:
		{
			Term_keypress('\010');
			return (TRUE);
		}
		
		/* Hack - the cursor keys */
		case GDK_Up:
		{
			Term_keypress('8');
			return (TRUE);
		}
		
		case GDK_Down:
		{
			Term_keypress('2');
			return (TRUE);
		}
		
		case GDK_Left:
		{
			Term_keypress('4');
			return (TRUE);
		}
		
		case GDK_Right:
		{
			Term_keypress('6');
			return (TRUE);
		}
		
		case GDK_Shift_L:
		case GDK_Shift_R:
		case GDK_Control_L:
		case GDK_Control_R:
		case GDK_Caps_Lock:
		case GDK_Shift_Lock:
		case GDK_Meta_L:
		case GDK_Meta_R:
		case GDK_Alt_L:
		case GDK_Alt_R:
		case GDK_Super_L:
		case GDK_Super_R:
		case GDK_Hyper_L:
		case GDK_Hyper_R:
		{
			/* Hack - do nothing to control characters */
			return (TRUE);
		}
	}

	/* Build the macro trigger string */
	sprintf(msg, "%c%s%s%s%s_%X%c", 31,
	        mc ? "N" : "", ms ? "S" : "",
	        mo ? "O" : "", mx ? "M" : "",
	        event->keyval, 13);
	
	/* Enqueue the "macro trigger" string */
	for (i = 0; msg[i]; i++) Term_keypress(msg[i]);
	
	/* Hack -- auto-define macros as needed */
	if (event->length && (macro_find_exact(msg) < 0))
	{
		/* Create a macro */
		macro_add(msg, event->string);
	}

	return (TRUE);
}


static gboolean expose_event_handler(GtkWidget *widget, GdkEventExpose *event,
				 gpointer user_data)
{
	term_data *td = user_data;
	term_data *old_data = (term_data*)(Term->data); 
	gint height, width;

	GdkPixmap *pixmap = gtk_object_get_data(GTK_OBJECT(widget), "pixmap");

	gdk_window_get_size(widget->window, &width, &height);
	
	/* Determine "proper" number of rows/cols */
	width = (width + 1) / td->font_wid;
	height = (height + 1) / td->font_hgt;

	/* Check height and width - and then resize */
	if ((width != td->cols) || (height != td->rows))
	{
		/* Delete the old pixmap */
		gdk_pixmap_unref(td->pixmap);
	
		/* Save correct size */
		td->cols = width;
		td->rows = height;
		
		/* Create a pixmap as buffer for screenupdates */
		td->pixmap = gdk_pixmap_new(td->drawing_area->window,
						 td->cols * td->font_wid, td->rows * td->font_hgt, -1);
		gtk_object_set_data(GTK_OBJECT(td->drawing_area), "pixmap", td->pixmap);
	
		/* Clear the pixmap */
		gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc, TRUE,
			                0, 0,
		    	            td->cols * td->font_wid, td->rows * td->font_hgt);
		
		/* Copy the data to the window */
		gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
		
		/* Hack -- activate the Term */
		Term_activate(&td->t);
		
		/* Resize the Term (if needed) */
		(void)Term_resize(td->cols, td->rows);
		
		/* Hack -- Activate the old term */
		Term_activate(&old_data->t);
	}

	/* Just redraw the exposed part */
	else if (pixmap)
	{
		g_assert(widget->window != 0);

		gdk_draw_pixmap(widget->window, td->gc, pixmap,
		                event->area.x, event->area.y,
		                event->area.x, event->area.y,
		                event->area.width, event->area.height);
	}

	return (TRUE);
}


static errr term_data_init(term_data *td, int i)
{
	cptr font;
	
	term *t = &td->t;

	td->cols = 80;
	td->rows = 24;

	/* Initialize the term */
	term_init(t, td->cols, td->rows, 1024);

	/* Store the name of the term */
	td->name = angband_term_name[i];

	/* Use a "soft" cursor */
	t->soft_cursor = TRUE;

	/* Erase with "white space" */
	t->attr_blank = TERM_WHITE;
	t->char_blank = ' ';

	t->xtra_hook = Term_xtra_gtk;
	t->text_hook = Term_text_gtk;
    t->wipe_hook = Term_wipe_gtk;
	t->curs_hook = Term_curs_gtk;

	/* Save the data */
	t->data = td;

	/* Activate (important) */
	Term_activate(t);

	/* Get default font for this term */
	font = get_default_font(i);

	load_font(td, font);

	/* Success */
	return (0);
}


static void init_gtk_window(term_data *td, int i)
{
	bool main_win = (i == 0) ? TRUE : FALSE;
	
	GdkGeometry win_geom;
	
	GtkWidget *menu_bar, *file_item, *file_menu, *box;
	GtkWidget *seperator_item, *file_exit_item, *file_new_item, *file_open_item;
	GtkWidget *options_item, *options_menu, *options_font_item;

	/* Create widgets */
	td->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	box = gtk_vbox_new(FALSE, 0);
	td->drawing_area = gtk_drawing_area_new();

	/* The main window is special */
	if (main_win)
	{
		/* Create menu */
		menu_bar = gtk_menu_bar_new();
		file_item = gtk_menu_item_new_with_label("File");
		file_menu = gtk_menu_new();
		file_new_item = gtk_menu_item_new_with_label("New");
		file_open_item = gtk_menu_item_new_with_label("Open");
		seperator_item = gtk_menu_item_new();
		file_exit_item = gtk_menu_item_new_with_label("Exit");
		options_item = gtk_menu_item_new_with_label("Options");
		options_menu = gtk_menu_new();
		options_font_item = gtk_menu_item_new_with_label("Font");
	
		/* Set attributes */
		gtk_window_set_title(GTK_WINDOW(td->window), td->name);
		gtk_drawing_area_size(GTK_DRAWING_AREA(td->drawing_area),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
		gtk_window_set_policy(GTK_WINDOW(td->window), TRUE, TRUE, TRUE);
		
		/* Initialize the geometry information */
		win_geom.width_inc = td->font_wid;
		win_geom.height_inc = td->font_hgt;
		win_geom.min_width = 80 * td->font_wid;
		win_geom.min_height = 24 * td->font_hgt;
		win_geom.max_width = 255 * td->font_wid;
		win_geom.max_height = 255 * td->font_hgt;
		win_geom.base_width = 1;
		win_geom.base_height = 1;
		gtk_window_set_geometry_hints(GTK_WINDOW(td->window),
					 td->drawing_area, &win_geom,
					 GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE
					| GDK_HINT_BASE_SIZE | GDK_HINT_RESIZE_INC);
		
		/* Register callbacks */
		gtk_signal_connect(GTK_OBJECT(file_exit_item), "activate",
					 quit_event_handler, NULL);
		gtk_signal_connect(GTK_OBJECT(file_new_item), "activate",
					 new_event_handler, NULL);
		gtk_signal_connect(GTK_OBJECT(file_open_item), "activate",
					 open_event_handler, NULL);
		gtk_signal_connect(GTK_OBJECT(options_font_item), "activate",
					 change_font_event_handler, td);
		
		/* Register more callbacks */
		gtk_signal_connect(GTK_OBJECT(td->window), "delete_event",
					 GTK_SIGNAL_FUNC(delete_event_handler), NULL);
		gtk_signal_connect(GTK_OBJECT(td->window), "key_press_event", 
					 GTK_SIGNAL_FUNC(keypress_event_handler), NULL);
		gtk_signal_connect(GTK_OBJECT(td->drawing_area), "expose_event",
					 GTK_SIGNAL_FUNC(expose_event_handler), td);
		gtk_signal_connect(GTK_OBJECT(td->window), "destroy_event", 
					 GTK_SIGNAL_FUNC(destroy_event_handler), NULL);
		
		/* Pack widgets */
		gtk_container_add(GTK_CONTAINER(td->window), box);
		gtk_box_pack_start(GTK_BOX(box), menu_bar, FALSE, FALSE, NO_PADDING);
		gtk_box_pack_start_defaults(GTK_BOX(box), td->drawing_area);
	
		/* Pack the menu bar */
		gtk_menu_bar_append(GTK_MENU_BAR(menu_bar), file_item);
		gtk_menu_bar_append(GTK_MENU_BAR(menu_bar), options_item);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(file_item), file_menu);
		gtk_menu_item_set_submenu(GTK_MENU_ITEM(options_item), options_menu);
		gtk_menu_append(GTK_MENU(file_menu), file_new_item);
		gtk_menu_append(GTK_MENU(file_menu), file_open_item);
		gtk_menu_append(GTK_MENU(file_menu), seperator_item);
		gtk_menu_append(GTK_MENU(file_menu), file_exit_item);
		gtk_menu_append(GTK_MENU(options_menu), options_font_item);
	}
	else
	{
		/* Set attributes */
		gtk_window_set_title(GTK_WINDOW(td->window), td->name);
		gtk_drawing_area_size(GTK_DRAWING_AREA(td->drawing_area),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
		gtk_window_set_policy(GTK_WINDOW(td->window), TRUE, TRUE, TRUE);
		
		/* Initialize the geometry information */
		win_geom.width_inc = td->font_wid;
		win_geom.height_inc = td->font_hgt;
		win_geom.min_width = 1 * td->font_wid;
		win_geom.min_height = 1 * td->font_hgt;
		win_geom.max_width = 255 * td->font_wid;
		win_geom.max_height = 255 * td->font_hgt;
		gtk_window_set_geometry_hints(GTK_WINDOW(td->window), NULL, &win_geom,
					GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE | GDK_HINT_RESIZE_INC);
		
		/* Register callbacks */
		gtk_signal_connect(GTK_OBJECT(td->window), "delete_event",
					 GTK_SIGNAL_FUNC(delete_event_handler), NULL);
		gtk_signal_connect(GTK_OBJECT(td->window), "key_press_event", 
					 GTK_SIGNAL_FUNC(keypress_event_handler), NULL);
		gtk_signal_connect(GTK_OBJECT(td->drawing_area), "expose_event",
					 GTK_SIGNAL_FUNC(expose_event_handler), td);
		gtk_signal_connect(GTK_OBJECT(td->window), "destroy_event",
					 GTK_SIGNAL_FUNC(hide_event_handler), td);
		
		/* Pack widgets */
		gtk_container_add(GTK_CONTAINER(td->window), box);
		gtk_box_pack_start_defaults(GTK_BOX(box), td->drawing_area);
	}
	
	/* Show the widgets */
	gtk_widget_show_all(td->window);

	/* Create a pixmap as buffer for screenupdates */
	td->pixmap = gdk_pixmap_new(td->drawing_area->window,
					 td->cols * td->font_wid, td->rows * td->font_hgt, -1);
	gtk_object_set_data(GTK_OBJECT(td->drawing_area), "pixmap", td->pixmap);
	td->gc = gdk_gc_new(td->drawing_area->window);
	
	/* Clear the pixmap */
	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc, TRUE,
		                0, 0,
		                td->cols * td->font_wid, td->rows * td->font_hgt);
						
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Show the widgets */
	gtk_widget_show_all(td->window);
}


/*
 * Initialization function
 */
errr init_gtk(unsigned char *new_game, int argc, char **argv)
{
	int i;

#ifdef USE_GRAPHICS

	char filename[1024];
	int bitdepth = 0;
#endif /* USE_GRAPHICS */
	
	/* See if gtk exists and works */
	if (!gtk_init_check(&argc, &argv)) return (1);
	
	/* Hack - save variable so that everyone can use it */
	gtk_newgame = *new_game;

	/* Initialize the environment */
	gtk_init(&argc, &argv);
	
	/* Prepare normal colors */
	store_pixel_colors();
	
	/* Parse args */
	for (i = 1; i < argc; i++)
	{
		if (prefix(argv[i], "-n"))
		{
			num_term = atoi(&argv[i][2]);
			if (num_term > MAX_TERM_DATA) num_term = MAX_TERM_DATA;
			else if (num_term < 1) num_term = 1;
			continue;
		}

#ifdef USE_GRAPHICS
		
		if (prefix(argv[i], "-b"))
		{
			bitdepth = atoi(&argv[i][2]);
			
			/* paranoia */
			if ((bitdepth != 16) && (bitdepth != 8)) bitdepth = 0;
			continue;
		}

#endif /* USE_GRAPHICS */

		plog_fmt("Ignoring option: %s", argv[i]);
	}

#ifdef USE_GRAPHICS

	/* Try graphics */
	if (arg_graphics)
	{
		use_graphics = FALSE;
		
		if ((bitdepth == 0) || (bitdepth == 16))
		{
			/* Try the "16x16.bmp" file */
			path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/16x16.bmp");

			/* Use the "16x16.bmp" file if it exists */
			if (0 == fd_close(fd_open(filename, O_RDONLY)))
			{
				/* Use graphics */
				use_graphics = TRUE;

				use_transparency = TRUE;

				tile_size = 16;

				ANGBAND_GRAF = "new";
			}
		}
		
		/* We failed, or we want 8x8 graphics */
		if (!use_graphics && ((bitdepth == 0) || (bitdepth == 8)))
		{
			/* Try the "8x8.bmp" file */
			path_build(filename, 1024, ANGBAND_DIR_XTRA, "graf/8x8.bmp");

			/* Use the "8x8.bmp" file if it exists */
			if (0 == fd_close(fd_open(filename, O_RDONLY)))
			{
				/* Use graphics */
				use_graphics = TRUE;

				tile_size = 8;

				ANGBAND_GRAF = "old";
			}
		}
	}

	/* Load graphics */
	if (use_graphics)
	{
		/* Read the bitmap */
		ReadBMP(filename);

		/* Paranoia */
		if (!tiles_norm) quit("Could not load tiles properly.");
	}

#endif /* USE_GRAPHICS */
		
	/* Initialize the windows */
	for (i = 0; i < num_term; i++)
	{
		term_data *td = &data[i];

#ifdef USE_GRAPHICS		
		term *t = &td->t;
#endif /* USE_GRAPHICS */
		
		/* Initialize the term_data */
		term_data_init(td, i);

		/* Save global entry */
		angband_term[i] = Term;

#ifdef USE_GRAPHICS

		if (use_graphics)
		{
			t->pict_hook = Term_pict_gtk;

			t->higher_pict = TRUE;

			/* Resize tiles */
			td->tiles = resize_tiles(td->font_wid, td->font_hgt);

#ifdef USE_TRANSPARENCY

			/* Initialize the transparency temp storage*/			
			td->temp = gdk_image_new(GDK_IMAGE_FASTEST, gdk_visual_get_system(),
				td->font_wid, td->font_hgt);
		}
#endif /* USE_TRANSPARENCY */
#endif /* USE_GRAPHICS */

		/* Init the window */
		init_gtk_window(td, i);
	}

	/* Activate the "Angband" window screen */
	Term_activate(&data[0].t);
	
	/*
	 * Mega-Hack XXX XXX XXX
	 * Initialize resize_hook of main term.
	 *
	 * For some reason, we cannot have this as the default in
	 * z-term.c  (Other ports do not work properly.)
	 */
	data[0].t.resize_hook = Term_fresh;

	/* Activate hooks */
	quit_aux = hook_quit;
	core_aux = hook_quit;

	/* Catch nasty signals */
	signals_init();
	
	/* Need to initialize system type */
	ANGBAND_SYS = "gtk";

	/* Initialize */
	init_angband();
	
	/* Prompt the user */
	prt("[Choose 'New' or 'Open' from the 'File' menu]", 23, 17);
	Term_fresh();
	
	while (!game_in_progress)
	{
		while (gtk_events_pending())
		{
			gtk_main_iteration();
		}

		/* Wait so we don't waste all the processor */
		usleep(200);
	}
	
	/* Load 'newgame' flag */
	*new_game = gtk_newgame;
	
	/* Press a key for the player */
	Term_keypress(' ');
	
	/* Success */
	return (0);
}

#endif /* USE_GTK */
