/* File: main-gtk.c */

/*
 * Copyright (c) 2000 Robert Ruehlmann
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * Robert Ruehlmann wrote the original Gtk port. Since an initial work is
 * much harder than enhancements, his effort worth more credits than
 * others.
 *
 * Steven Fuerst implemented colour-depth independent X server support,
 * graphics, resizing and big screen support for ZAngband as well as
 * fast image rescaling that is included here.
 *
 * "pelpel" added GtkItemFactory based menu system and added comments.
 */

#include "angband.h"

#include "maid-grf.h"

#ifdef USE_GTK

cptr help_gtk[] =
{
	"To use GTK toolkit",
#ifdef USE_GRAPHICS
	"-b#   Set tileset bitmap",
#endif /* USE_GRAPHICS */
	"-n#   Number of terms to use",
	NULL
};


/* Mega-hack, these include files require double and float to work */
#undef float
#undef double

/* Ansi C please */
#define __STRICT_ANSI__ 

#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

/* Mega-hack redefine them again */
#undef float
#define float floating_point_is_not_allowed
#undef double
#define double floating_point_is_not_allowed

/*
 * Number of pixels inserted between the menu bar and the main screen
 */
#define NO_PADDING 0

/*
 * Largest possible number of terminal windows supported by the game
 */
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

#ifdef USE_GRAPHICS

	GdkImage *tiles;
	GdkImage *b_tiles;
	GdkImage *temp;
	
#endif /* USE_GRAPHICS */

	int font_wid;
	int font_twid;
	int font_hgt;
	

	int rows;
	int cols;

	cptr name;
	cptr fontname;
	
	bool shown;
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
 * Hack - exit the game
 */
static bool gtk_exitgame = FALSE;

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
static int xsize;
static int ysize;

static guint32 black_pixel;
#endif /* USE_GRAPHICS */


/* Temp return value for create_pixel */
static GdkColor temp_color;

/*
 * Hack -- Convert an RGB value to an X11 Pixel, or die.
 */
static GdkColor *create_pixel(byte red, byte green, byte blue)
{
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

	temp_color.red = red * 255;
	temp_color.green = green * 255;
	temp_color.blue = blue * 255;
	
	/* Attempt to Allocate the Parsed color */
	if (!gdk_colormap_alloc_color(gdk_colormap_get_system(),
		 &temp_color, FALSE, TRUE))
	{
		g_print("Couldn't allocate color.");
	}

	return (&temp_color);
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
		clr->pixel = *create_pixel(clr->red, clr->green, clr->blue);
	}
}


static errr Term_xtra_gtk_react(void)
{
	int i;
	infoclr *clr;
	
	bool redraw = FALSE;
	
	/* Hack - check colours */
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
			clr->pixel = *create_pixel(clr->red, clr->green, clr->blue);
			
			/* Set flag */
			redraw = TRUE;
		}
	}
	
	/* Hack - Redraw it if the colours have changed */
	if (redraw) Term_redraw();
	
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
		plog_fmt("Incorrect BMP file format %s", Name);
		
		return;
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
		clr_pixels[i] = create_pixel(clrg.r, clrg.g, clrg.b)->pixel;
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
			if (feof(f))
			{
				plog_fmt("Unexpected end of file in %s", Name);
				
				/* Delete reference to image */
				gdk_image_destroy(tiles_norm);
				
				/* Hack - clear tiles */
				tiles_norm = NULL;
				return;
			}	
				

			if (infoheader.biBitCount == 24)
			{
				int c3, c2 = getc(f);
				
				/* Verify not at end of file XXX XXX */
				if (feof(f))
				{
					plog_fmt("Unexpected end of file in %s", Name);
				
					/* Delete reference to image */
					gdk_image_destroy(tiles_norm);
				
					/* Hack - clear tiles */
					tiles_norm = NULL;
					return;
				}	
							
				c3 = getc(f);

				/* Verify not at end of file XXX XXX */
				if (feof(f))
				{
					plog_fmt("Unexpected end of file in %s", Name);
				
					/* Delete reference to image */
					gdk_image_destroy(tiles_norm);
				
					/* Hack - clear tiles */
					tiles_norm = NULL;
					return;
				}
				
				gdk_image_put_pixel(tiles_norm, x, y2,
					create_pixel(ch, c2, c3)->pixel);
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
				plog_fmt("Illegal biBitCount %d in %s",
				         infoheader.biBitCount, Name);
				{
					/* Delete reference to image */
					gdk_image_destroy(tiles_norm);
				
					/* Hack - clear tiles */
					tiles_norm = NULL;
					return;
				}
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
	int new_wid = (old_wid / xsize) * tile_wid;
	int new_hgt = (old_hgt / ysize) * tile_hgt;
	
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
	switch (new_image->bpp)
	{
		case 1: copy_pixels = copy_pixels8; break;
		case 2: copy_pixels = copy_pixels16; break;
		case 3: copy_pixels = copy_pixels24; break;
		case 4: copy_pixels = copy_pixels32; break;
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
	FREE(xoffsets);
	
	return (new_image);
}

#endif /* USE_GRAPHICS */


/*
 * Find the pixel at the top-left corner of a square.
 */
static void square_to_pixel(int *x, int *y, int ox, int oy)
{
	term_data *td = (term_data*)(Term->data);
	
	(*y) = oy * td->font_hgt;
	
	if ((use_bigtile) && (oy >= Term->scr->big_y1)
			&& (oy <= Term->scr->big_y2)
			&& (ox > Term->scr->big_x1))
	{
		(*x) = ox * td->font_twid - Term->scr->big_x1 * td->font_wid;
	}
	else
	{
		(*x) = ox * td->font_wid;
	}
}


/*
 * Erase some characters.
 */
static errr Term_wipe_gtk(int x, int y, int n)
{
	int x1, y1, x2, y2;
	
	term_data *td = (term_data*)(Term->data);
	
	/* Don't draw to hidden windows */
	if (!td->shown) return (0);

	g_assert(td->pixmap);
	g_assert(td->drawing_area->window);
	
	/*** Find the dimensions ***/
	square_to_pixel(&x1, &y1, x, y);
	square_to_pixel(&x2, &y2, x + n, y);

	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc,
					TRUE, x1, y1,
					x2 - x1, td->font_hgt);

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                x1, y1,
	                x1, y1,
	                x2 - x1, td->font_hgt);

	/* Success */
	return (0);
}


/*
 * Draw some textual characters.
 */
static errr Term_text_gtk(int cx, int cy, int n, byte a, cptr s)
{
	int i;
	term_data *td = (term_data*)(Term->data);
	GdkColor color;
	
	int x, y;
	int sx, sy;
	
	/* Don't draw to hidden windows */
	if (!td->shown) return (0);

	/* Create the colour structure */
	color = colours[a].pixel;

	g_assert(td->pixmap);
	g_assert(td->drawing_area->window);

	/* Set the forground colour */
	gdk_gc_set_foreground(td->gc, &color);

	/* Clear the line */
	Term_wipe_gtk(cx, cy, n);
	
	/*** Decide where to place the string, vertically ***/
	square_to_pixel(&x, &y, cx, cy);
	
	/* Start location */
	sx = x;
	sy = y;
	
	/* Ignore Vertical Justifications */
	y += td->font->ascent;

	/* Draw the text to the pixmap */
	for (i = 0; i < n; i++)
	{
		if (is_bigtiled(cx + i, cy))
		{
			/* Note that the Infoclr is set up to contain the Infofnt */
			gdk_draw_text(td->pixmap, td->font, td->gc,
							x, y, s + i, 1);
			x += td->font_twid;
		}
		else
		{
			/* Note that the Infoclr is set up to contain the Infofnt */
			gdk_draw_text(td->pixmap, td->font, td->gc,
							x, y, s + i, 1);
			x += td->font_wid;
		}
	}

	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                sx, sy,
	                sx, sy,
	                x, td->font_hgt);

	/* Success */
	return (0);
}


static errr Term_curs_gtk(int x, int y)
{
	term_data *td = (term_data*)(Term->data);
	
	int x1, y1;
	
	/* Don't draw to hidden windows */
	if (!td->shown) return (0);

	g_assert(td->pixmap);
	g_assert(td->drawing_area->window);

	gdk_gc_set_foreground(td->gc, &colours[TERM_YELLOW].pixel);
	
	square_to_pixel(&x1, &y1, x, y);

	if (is_bigtiled(x, y))
	{
		gdk_draw_rectangle(td->pixmap, td->gc, FALSE,
							x1, y1,
							td->font_twid - 1, td->font_hgt - 1);

		/* Copy it to the window */
		gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
							x1, y1,
							x1, y1,
							td->font_twid, td->font_hgt);
	}
	else
	{
		gdk_draw_rectangle(td->pixmap, td->gc, FALSE,
							x1, y1,
							td->font_wid - 1, td->font_hgt - 1);

		/* Copy it to the window */
		gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
							x1, y1,
							x1, y1,
							td->font_wid, td->font_hgt);
	}

	/* Success */
	return (0);
}


#ifdef USE_GRAPHICS

/*
 * Draw some graphical characters.
 */
static errr Term_pict_gtk(int ox, int oy, int n, const byte *ap, const char *cp,
	const byte *tap, const char *tcp)
{
	int i;
	int x1, y1;

	byte a;
	char c;

	byte ta;
	char tc;

	int x, y;
	int sx;
	int x2, y2;
	int k, l;

	guint32 pixel, blank;

	term_data *td = (term_data*)(Term->data);
	
	int wid, hgt = td->font_hgt;
	GdkImage *tiles;

	/* Mega Hack^2 - assume the top left corner is "black" */
	blank = gdk_image_get_pixel(td->tiles, 0, td->font_hgt * 6);
	
	/* Don't draw to hidden windows */
	if (!td->shown) return (0);
	
	/* Paranoia */
	g_assert(td->tiles);
	g_assert(use_graphics);

	/* Starting point */
	square_to_pixel(&x, &y, ox, oy);
	
	/* Save start x coord */
	sx = x;

	for (i = 0; i < n; i++)
	{
		/* What are we drawing? */
		if (is_bigtiled(ox + i, oy))
		{
			tiles = td->b_tiles;
			wid = td->font_twid;
		}
		else
		{
			tiles = td->tiles;
			wid = td->font_wid;
		}

		a = *ap++;
		c = *cp++;

		/* For extra speed - cache these values */
		x1 = (c&0x7F) * wid;
		y1 = (a&0x7F) * hgt;

		ta = *tap++;
		tc = *tcp++;

		/* For extra speed - cache these values */
		x2 = (tc&0x7F) * wid;
		y2 = (ta&0x7F) * hgt;

		/* Optimise the common case */
		if (!use_transparency || ((x1 == x2) && (y1 == y2)))
		{
			/* Draw object / terrain */
			gdk_draw_image(td->pixmap, td->gc, tiles,
				 x1, y1, x, y,
				 wid, hgt);
		}
		else
		{
			for (k = 0; k < wid; k++)
			{
				for (l = 0; l < hgt; l++)
				{
					/* If mask set... */
					if ((pixel = gdk_image_get_pixel(tiles, x1 + k, y1 + l)) == blank)
					{
						/* Output from the terrain */
						pixel = gdk_image_get_pixel(tiles, x2 + k, y2 + l);
					}

					/* Store into the temp storage. */
					gdk_image_put_pixel(td->temp, k, l, pixel);
				}
			}


			/* Draw to screen */
			gdk_draw_image(td->pixmap, td->gc, td->temp,
				 0, 0, x, y,
				 wid, hgt);
		
			/* Hack - flush the changes */
			gdk_flush();
		}
		
		x += wid;
	}
	
	
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                sx, y,
	                sx, y,
	                x - sx, td->font_hgt);

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
	/* Flush the pending events */
	while (gtk_events_pending()) gtk_main_iteration();
	
	/* Done */
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
		case TERM_XTRA_NOISE:
		{
			/* Beep */
			gdk_beep();
			
			/* Done */
			return (0);
		}
		/* Flush the output */
		case TERM_XTRA_FRESH:
		{
			/* Flush pending X requests - almost always no-op */
			gdk_flush();
			
			/* Done */
			return (0);
		}
		
		/* Process random events */
		case TERM_XTRA_BORED: return (CheckEvent(FALSE));

		/* Process Events */
		case TERM_XTRA_EVENT: return (CheckEvent(v));

		/* Flush the events */
		case TERM_XTRA_FLUSH: return (Term_flush_gtk());

		/* Handle change in the "level" */
		case TERM_XTRA_LEVEL: return (0);

		/* Delay for some milliseconds */
		case TERM_XTRA_DELAY:
		{
			/* Delay */
			if (v > 0) usleep(1000 * v);
			
			return (0);
		}

		/* React to changes */
		case TERM_XTRA_REACT: return (Term_xtra_gtk_react());
	}

	/* Unknown */
	return (1);
}

/*
 * Make sure the pixmap is correctly allocated
 * for the size of the window
 */
static void init_pixmap(term_data *td)
{
	/* Paranoia */
	g_assert(td->drawing_area->window);
	
	if (td->pixmap)
	{
		/* Delete the old pixmap */
		gdk_pixmap_unref(td->pixmap);	
	}
	
	/* Create a pixmap as buffer for screenupdates */
	td->pixmap = gdk_pixmap_new(td->drawing_area->window,
					 td->cols * td->font_wid, td->rows * td->font_hgt, -1);
	gtk_object_set_data(GTK_OBJECT(td->drawing_area), "pixmap",
			 td->pixmap);
	
	/* Clear the pixmap */
	gdk_draw_rectangle(td->pixmap, td->drawing_area->style->black_gc, TRUE,
		                0, 0,
		                td->cols * td->font_wid, td->rows * td->font_hgt);
}

/*
 * Display message in a modal dialog
 */
static void gtk_message(cptr msg)
{
	GtkWidget *dialog, *label, *ok_button;

	/* Create the widgets */
	dialog = gtk_dialog_new();
	g_assert(dialog);

	label = gtk_label_new(msg);
	g_assert(label);

	ok_button = gtk_button_new_with_label("OK");
	g_assert(ok_button);

	/* Ensure that the dialogue box is destroyed when OK is clicked */
	gtk_signal_connect_object(GTK_OBJECT(ok_button), "clicked",
		GTK_SIGNAL_FUNC(gtk_widget_destroy), (gpointer)dialog);
	
	/* Add the button */
	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->action_area), ok_button);

	/* Add the label, and show the dialog */
	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

	/* And make it modal */
	gtk_window_set_modal(GTK_WINDOW(dialog), TRUE);

	/* Show the dialog */
	gtk_widget_show_all(dialog);
}


static void set_size_hints(term_data *td)
{
	GdkGeometry win_geom;
	
	/* Main window? */
	if (td == &data[0])
	{
		/* Initialize the geometry information */
		win_geom.width_inc = td->font_wid;
		win_geom.height_inc = td->font_hgt;
		win_geom.min_width = 80 * td->font_wid;
		win_geom.min_height = 24 * td->font_hgt;
		win_geom.max_width = 255 * td->font_wid;
		win_geom.max_height = 255 * td->font_hgt;
		gtk_window_set_geometry_hints(GTK_WINDOW(td->window),
					 td->drawing_area, &win_geom,
					 GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE
					 | GDK_HINT_RESIZE_INC);
	}
	else
	{
		/* Initialize the geometry information */
		win_geom.width_inc = td->font_wid;
		win_geom.height_inc = td->font_hgt;
		win_geom.min_width = 1 * td->font_wid;
		win_geom.min_height = 1 * td->font_hgt;
		win_geom.max_width = 255 * td->font_wid;
		win_geom.max_height = 255 * td->font_hgt;
		gtk_window_set_geometry_hints(GTK_WINDOW(td->window),
					 td->drawing_area, &win_geom,
					 GDK_HINT_MIN_SIZE | GDK_HINT_MAX_SIZE
					 | GDK_HINT_RESIZE_INC);
	}
}


static void load_font(term_data *td, cptr fontname)
{
	GdkFont *old = td->font;

	/* Load font */
	td->font = gdk_font_load(fontname);
	td->fontname = fontname;

	if (td->font)
	{
		/* Free the old font */
		if (old) gdk_font_unref(old);
	}
	else
	{
		/* Oops, but we can still use the old one */
		td->font = old;
	}

	/* Calculate the size of the font XXX */
	td->font_wid = gdk_char_width(td->font, '@');
	td->font_hgt = td->font->ascent + td->font->descent;
	
	/* Bigtile size */
	td->font_twid = 2 * td->font_wid;

}


static void font_ok_callback(GtkWidget *widget, GtkWidget *font_selector)
{
	gchar *fontname;
	
	term_data *old_td = (term_data*)(Term->data);
	term_data *td = gtk_object_get_data(GTK_OBJECT(font_selector), "term_data");

	/* Hack - ignore widget */
	(void) widget;
	
	g_assert(td);
	
	/* Hack -- activate current term */
	Term_activate(&td->t);
	
	/* Retrieve font name from player's selection */
	fontname = gtk_font_selection_dialog_get_font_name(
						GTK_FONT_SELECTION_DIALOG(font_selector));

	/* The user hasn't selected a font? */
	if (fontname == NULL) return;
	
	/* Load font and update font size info */
	load_font(td, fontname);
	
	/* Hack - Hide the window - finally found the trick... */
	gtk_widget_hide_all(td->window);
	
#ifdef USE_GRAPHICS

	if (use_graphics)
	{
		/* Need to get a new tile image */	
		if (td->tiles) gdk_image_destroy(td->tiles);
		if (td->b_tiles) gdk_image_destroy(td->b_tiles);
	
		/* Resize tiles */
		td->tiles = resize_tiles(td->font_wid, td->font_hgt);
		td->b_tiles = resize_tiles(td->font_twid, td->font_hgt);

		/* Get a new temp */ 
		if (td->temp) gdk_image_destroy(td->temp);
	
		/* Initialize the transparency temp storage*/			
		td->temp = gdk_image_new(GDK_IMAGE_FASTEST,
						gdk_visual_get_system(),
						td->font_twid, td->font_hgt);
	}

#endif /* USE_GRAPHICS */
	
	init_pixmap(td);

	/* Recalculate size hints */	
	set_size_hints(td);
	
	/* Show the widgets */
	gtk_widget_show_all(td->window);

	/* Resize the drawing area */
	gtk_drawing_area_size(GTK_DRAWING_AREA(td->drawing_area),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
	gtk_window_set_default_size(GTK_WINDOW(td->window),
				 td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Redraw the term */
	Term_redraw();
	
	/* Copy it to the window */
	gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Hack -- Activate the old term */
	Term_activate(&old_td->t);
}


static void change_font_event_handler(GtkWidget *widget, gpointer user_data)
{
	GtkWidget *font_selector = gtk_font_selection_dialog_new("Select font");

	term_data *td = user_data;
	gchar *foundery[] = { (char * ) "misc", NULL};
	gchar *spacings[] = { (char * ) "c", (char *) "m", NULL };
	gchar *charsets[] = { (char * ) "iso8859-1", NULL};

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


/*
 * Process Terms-* menu command - hide/show terminal window
 */
static void term_event_handler(GtkWidget *widget, gpointer user_data)
{
	term_data *td = (term_data *)user_data;
	
	/* Ignore unused parameter */
	(void) widget;

	/* We don't mess with the Angband window */
	if (td == &data[0]) return;

	/* It's shown */
	if (td->shown)
	{
		/* Hide the window */
		gtk_widget_hide_all(td->window);
	}

	/* It's hidden */
	else
	{
		/* Show the window */
		gtk_widget_show_all(td->window);
	}
}


/*
 * Widget customisation (for drawing area) - "realize" signal
 *
 * In this program, called when window containing the drawing
 * area is shown first time.
 */
static void realize_event_handler(GtkWidget *widget, gpointer user_data)
{
	term_data *td = (term_data *)user_data;
	
	/* Paranoia */
	g_assert(td->drawing_area->window);

	/* Create graphic context */
	td->gc = gdk_gc_new(td->drawing_area->window);

	/* Set foreground and background colours - isn't bg used at all? */
	gdk_gc_set_background(td->gc, &colours[TERM_DARK].pixel);
	gdk_gc_set_foreground(td->gc, &colours[TERM_WHITE].pixel);
	
	/* resize the pixmap */
	init_pixmap(td);
	

	/* Clear the window */
	gdk_draw_rectangle(td->drawing_area->window, widget->style->black_gc,
		TRUE, 0, 0,
		td->cols * td->font_wid,
		td->rows * td->font_hgt);
}


/*
 * Widget customisation (for drawing area) - "show" signal
 */
static void show_event_handler(GtkWidget *widget, gpointer user_data)
{
	term_data *td = (term_data *)user_data;
	
	/* Hack - ignore widget */
	(void) widget;

	/* Set the shown flag */
	td->shown = TRUE;
}


/*
 * Widget customisation (for drawing area) - "hide" signal
 */
static void hide_event_handler(GtkWidget *widget, gpointer user_data)
{
	term_data *td = (term_data *)user_data;

	/* Hack - ignore widget */
	(void) widget;
	
	/* Set the shown flag */
	td->shown = FALSE;
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
		path_make(buf, ANGBAND_DIR_SAVE, "*");

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

#ifdef USE_GRAPHICS

/*
 * Free all tiles and graphics buffers associated with windows
 */
static void graf_nuke(void)
{
	int i;

	term_data *td;

	/* Nuke all terms */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Access term_data structure */
		td = &data[i];

		/* Free previously allocated tiles */
		if (td->tiles) gdk_image_destroy(td->tiles);
		if (td->b_tiles) gdk_image_destroy(td->b_tiles);

		/* Forget pointer */
		td->tiles = NULL;
		td->b_tiles = NULL;

		/* Free previously allocated transparency buffer */
		if (td->temp) gdk_image_destroy(td->temp);

		/* Forget stale pointer */
		td->temp = NULL;
	}
}

/*
 * Initialise the graphics
 */
static void graf_init(void)
{
	int i;
	
	term_data *td;
	term *t;
	
	if (!tiles_norm) quit("Error - no tiles yet!");
	
	/* Mega Hack^2 - assume the top left corner is "black" */
	black_pixel = gdk_image_get_pixel(tiles_norm, 0, ysize * 6);
	
	/* Initialize the windows */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		td = &data[i];
		
		t = &td->t;

		if (use_graphics)
		{
			t->pict_hook = Term_pict_gtk;

			t->higher_pict = TRUE;

			/* Resize tiles */
			td->tiles = resize_tiles(td->font_wid, td->font_hgt);
			td->b_tiles = resize_tiles(td->font_twid, td->font_hgt);
			
			/* Initialize the transparency temp storage*/			
			td->temp = gdk_image_new(GDK_IMAGE_FASTEST, gdk_visual_get_system(),
				td->font_twid, td->font_hgt);
		}
		else
		{
			t->pict_hook = NULL;
			t->higher_pict = FALSE;
		}
	}
}


static bool set_graph_mode(int graphmode)
{
	char filename[1024];
	
	GdkImage *tiles_back = tiles_norm;
	
	int old_mode = use_graphics;
	
	/* See if can change tiles */
	if (!pick_graphics(graphmode, &xsize, &ysize, filename))
	{
		/* Revert to the old settings */
		pick_graphics(old_mode, &xsize, &ysize, filename);
	
		/* Failed */
		return (FALSE);
	}
	
	/* Erase the old graphics */
	graf_nuke();
	
	/* Load graphics */
	if (use_graphics)
	{
		/* Read the bitmap */
		ReadBMP(filename);

		/* Paranoia */
		if (!tiles_norm)
		{
			plog("Could not load tiles properly.");
			
			/* No tiles */
			use_graphics = GRAPHICS_NONE;
		}
	}
	
	/* Init the new graphics */
	graf_init();
	
	/* Destroy old tiles */
	if (tiles_back)
	{
		gdk_image_destroy(tiles_back);
	}

	/* Success! */
	return (TRUE);
}


/*
 * Set graf_mode_request according to user selection,
 * and let Term_xtra react to the change.
 */
static void change_graf_mode_event_handler(GtkButton *was_clicked,
										gpointer user_data)
{
	/* Hack - ignore parameter */
	(void) was_clicked;
	
	/* Set request according to user selection */
	if ((int)user_data != use_graphics)
	{
		/* Try to set mode */
		if (set_graph_mode((int)user_data))
		{
			/* Reset visuals */
#ifdef ANGBAND_2_8_1
			reset_visuals();
#else /* ANGBAND_2_8_1 */
			reset_visuals(TRUE);
#endif /* ANGBAND_2_8_1 */

			/* Hack - force redraw */
			Term_key_push(KTRL('R'));
		}

	}
}


/*
 * Toggles the boolean value of use_transparency
 */
static void change_trans_mode_event_handler(GtkButton *was_clicked,
										gpointer user_data)
{
	/* Hack - Ignore unused parameters */
	(void) was_clicked;
	(void) user_data;

	/* Toggle the transparency mode */
	use_transparency = !use_transparency;

	/* Hack - force redraw */
	Term_key_push(KTRL('R'));
}

/*
 * Toggles the boolean value of use_bigtile
 */
static void change_bigtile_mode_event_handler(GtkButton *was_clicked,
										gpointer user_data)
{
	/* Hack - Ignore unused parameters */
	(void) was_clicked;
	(void) user_data;

	/* Toggle the bigtile mode */
	toggle_bigtile();
}


#endif /* USE_GRAPHICS */



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
		strnfmt(msg, 128, "%cS_%X%c", 31, event->keyval, 13);
	
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
	strnfmt(msg, 128, "%c%s%s%s%s_%X%c", 31,
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


/*
 * Widget customisation (for drawing area)- handle size allocation requests
 */
static void size_allocate_event_handler(GtkWidget *widget, 
	GtkAllocation *allocation, gpointer user_data)
{
	term_data *td = user_data;
	int old_rows, old_cols;
	term_data *old_data = (term_data*)(Term->data);

	/* Paranoia */
	g_return_if_fail(widget != NULL);
	g_return_if_fail(allocation != NULL);
	g_return_if_fail(td != NULL);

	/* Remember old values */
	old_cols = td->cols;
	old_rows = td->rows;

	/* Update numbers of rows and columns */
	td->cols = (allocation->width + td->font_wid - 1) / td->font_wid;
	td->rows = (allocation->height + td->font_hgt - 1) / td->font_hgt;

	/* Adjust size request and set it */
	allocation->width = td->cols * td->font_wid;
	allocation->height = td->rows * td->font_hgt;
	widget->allocation = *allocation;
	
	/* Check height and width - and then resize */
	if ((old_cols != td->cols) || (old_rows != td->rows))
	{
		/* Resize pixmap */
		init_pixmap(td);
		
		/* Hack -- activate the Term */
		Term_activate(&td->t);
		
		/* Resize the Term (if needed) */
		(void)Term_resize(td->cols, td->rows);
		
		/* Hack -- Activate the old term */
		Term_activate(&old_data->t);
	}

	/* Widget is realized, so we do some drawing works */
	if (GTK_WIDGET_REALIZED(widget))
	{
		/* Copy the data to the window */
		gdk_draw_pixmap(td->drawing_area->window, td->gc, td->pixmap,
	                0, 0, 0, 0,
					td->cols * td->font_wid, td->rows * td->font_hgt);
		
		/* Actually handles resizing in Gtk */
		gdk_window_move_resize(widget->window,
			allocation->x, allocation->y,
			allocation->width, allocation->height);

		/* And in the term package */
		Term_activate(&td->t);

		/* Resize if necessary */
		if ((td->cols != old_cols) || (td->rows != old_rows))
			(void)Term_resize(td->cols, td->rows);
			
		/* Hack -- Activate the old term */
		Term_activate(&old_data->t);
	}
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
		/* Save correct size */
		td->cols = width;
		td->rows = height;
		
		/* Resize pixmap */
		init_pixmap(td);
		
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
		g_assert(widget->window);

		gdk_draw_pixmap(widget->window, td->gc, pixmap,
		                event->area.x, event->area.y,
		                event->area.x, event->area.y,
		                event->area.width, event->area.height);
	}

	return (TRUE);
}


/*
 * Find widget corresponding to path name
 * return NULL on error
 */
static GtkWidget *get_widget_from_path(cptr path)
{
	GtkItemFactory *item_factory;
	GtkWidget *widget;
	
	/* Paranoia */
	g_assert(path);

	/* Look up item factory */
	item_factory = gtk_item_factory_from_path(path);

	/* Paranoia */
	g_assert(item_factory);

	/* Look up widget */
	widget = gtk_item_factory_get_widget(item_factory, path);
	
	if (!widget) plog(path);

	/* Return result */
	return (widget);
}


/*
 * Enable/disable a menu item
 */
static void enable_menu_item(cptr path, bool enabled)
{
	GtkWidget *widget;

	/* Access menu item widget */
	widget = get_widget_from_path(path);

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_MENU_ITEM(widget));

	/*
	 * In Gtk's terminology, enabled is sensitive
	 * and disabled insensitive
	 */
	gtk_widget_set_sensitive(widget, enabled);
}


/*
 * Check/uncheck a menu item. The item should be of the GtkCheckMenuItem type
 */
static void check_menu_item(cptr path, bool checked)
{
	GtkWidget *widget;

	/* Access menu item widget */
	widget = get_widget_from_path(path);

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_CHECK_MENU_ITEM(widget));

	/* Put/remove check mark
	 *
	 * Mega-Hack -- The function supposed to be used here,
	 * gtk_check_menu_item_set_active(), emits an "activate" signal
	 * to the GtkMenuItem class of the widget, as if the menu item
	 * were selected by user, thereby causing bizarre behaviour.
	 * XXX XXX XXX
	 */
	GTK_CHECK_MENU_ITEM(widget)->active = checked;
}


/*
 * Update the "File" menu
 */
static void file_menu_update_handler(GtkWidget *widget, gpointer user_data)
{
	bool save_ok = FALSE;
	bool quit_ok = FALSE;
	bool start_ok = !gtk_newgame;
	
	/* Ignore parameter */
	(void) widget;
	(void) user_data;

	/* Cave we save/quit now? */
	if (!character_generated || !game_in_progress)
	{
		quit_ok = TRUE;
	}
	else
	{
		if (p_ptr->cmd.inkey_flag && game_in_progress && character_generated)
		{
			save_ok = TRUE;
			quit_ok = TRUE;
		}
	}

	/* Enable / disable menu items according to those conditions */
	enable_menu_item("<Angband>/File/New", start_ok);
	enable_menu_item("<Angband>/File/Open", start_ok);
	enable_menu_item("<Angband>/File/Save", save_ok);
	enable_menu_item("<Angband>/File/Quit", quit_ok);
}


/*
 * Update the "Terms" menu
 */
static void term_menu_update_handler(GtkWidget *widget, gpointer user_data)
{
	int i;
	char buf[64];

	/* Ignore parameters */
	(void) widget;
	(void) user_data;
	
	/* For each term */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Build the path name */
		strnfmt(buf, 64, "<Angband>/Terms/%s", angband_term_name[i]);

		/* Update the check mark on the item */
		check_menu_item(buf, data[i].shown);
	}
}


/*
 * Update the "Font" submenu
 */
static void font_menu_update_handler(GtkWidget *widget, gpointer user_data)
{
	int i;
	char buf[64];
	
	/* Ignore parameters */
	(void) widget;
	(void) user_data;

	/* For each term */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* Build the path name */
		strnfmt(buf, 64, "<Angband>/Options/Font/%s", angband_term_name[i]);

		/* Enable selection if the term is shown */
		enable_menu_item(buf, data[i].shown);
	}
}


#ifdef USE_GRAPHICS

/*
 * Update the "Graphics" submenu
 */
static void graf_menu_update_handler(GtkWidget *widget, gpointer user_data)
{
	/* Ignore unused parameters */
	(void) widget;
	(void) user_data;

	/* Update menu items */
	check_menu_item(
		"<Angband>/Options/Graphics/None",
		(use_graphics == GRAPHICS_NONE));
	check_menu_item(
		"<Angband>/Options/Graphics/8x8",
		(use_graphics == GRAPHICS_ORIGINAL));
	check_menu_item(
		"<Angband>/Options/Graphics/16x16",
		(use_graphics == GRAPHICS_ADAM_BOLT));
	check_menu_item(
		"<Angband>/Options/Graphics/32x32",
		(use_graphics == GRAPHICS_DAVID_GERVAIS));
	check_menu_item(
		"<Angband>/Options/Graphics/Transparency",
		use_transparency);
	check_menu_item(
		"<Angband>/Options/Graphics/BigTile",
		use_bigtile);
}

#endif /* USE_GRAPHICS */


/*
 * Install callbacks to update menus
 */
static void add_menu_update_callbacks(void)
{
	GtkWidget *widget;

	/* Access the "File" menu */
	widget = get_widget_from_path("<Angband>/File");

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_MENU(widget));

	/* Assign callback */
	gtk_signal_connect(GTK_OBJECT(widget), "show",
		GTK_SIGNAL_FUNC(file_menu_update_handler), NULL);

	/* Access the "Terms" menu */
	widget = get_widget_from_path("<Angband>/Terms");

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_MENU(widget));

	/* Assign callback */
	gtk_signal_connect(GTK_OBJECT(widget), "show",
		GTK_SIGNAL_FUNC(term_menu_update_handler), NULL);

	/* Access the "Font" menu */
	widget = get_widget_from_path("<Angband>/Options/Font");

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_MENU(widget));

	/* Assign callback */
	gtk_signal_connect(GTK_OBJECT(widget), "show",
		GTK_SIGNAL_FUNC(font_menu_update_handler), NULL);

#ifdef USE_GRAPHICS

	/* Access Graphics menu */
	widget = get_widget_from_path("<Angband>/Options/Graphics");

	/* Paranoia */
	g_assert(widget);
	g_assert(GTK_IS_MENU(widget));

	/* Assign callback */
	gtk_signal_connect(GTK_OBJECT(widget), "show",
		GTK_SIGNAL_FUNC(graf_menu_update_handler), NULL);

#endif /* USE_GRAPHICS */
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
	else
	{
		plog("You can't start a new game while you're still playing!");
	}
}


static void save_game_gtk(void)
{
	if (game_in_progress && character_generated)
	{
		if (!p_ptr->cmd.inkey_flag)
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


/*
 * Process File-Save menu command
 */
static void save_event_handler(GtkButton *was_clicked, gpointer user_data)
{
	/* Ignore unused parameters */
	(void) was_clicked;
	(void) user_data;

	/* Save current game */
	save_game_gtk();
}


static gboolean delete_event_handler(GtkWidget *widget, GdkEvent *event,
										gpointer user_data)
{
	/* Hack - ignore parameters */
	(void) widget;
	(void) event;
	(void) user_data;
	
	save_game_gtk();

	/* Hack - set exit flag */
	gtk_exitgame = TRUE;

	/* Don't prevent closure */
	return (FALSE);
}


/*
 * Process File-Quit menu command
 */
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



/*
 * Neater menu code with GtkItemFactory.
 *
 * Menu bar of the Angband window
 *
 * Entry format: Path, Accelerator, Callback, Callback arg, type
 * where type is one of:
 * <Item> - simple item, alias NULL
 * <Branch> - has submenu
 * <Separator> - as you read it
 * <CheckItem> - has a check mark
 * <ToggleItem> - is a toggle
 */
static GtkItemFactoryEntry main_menu_items[] =
{
	/* "File" menu */
	{ (char *) "/File", NULL,
	  NULL, 0, (char *) "<Branch>" },
	{ (char *) "/File/New", (char *) "<mod1>N",
	  new_event_handler, 0, NULL },
	{ (char *) "/File/Open", (char *) "<mod1>O",
	  open_event_handler, 0, NULL },
	{ (char *) "/File/sep1", NULL,
	  NULL, 0, (char *) "<Separator>" },
	{ (char *) "/File/Save", (char *) "<mod1>S",
	  save_event_handler, 0, NULL },
	{ (char *) "/File/Quit", (char *) "<mod1>Q",
	  quit_event_handler, 0, NULL },

	/* "Terms" menu */
	{ (char * ) "/Terms", NULL,
	  NULL, 0, (char * ) "<Branch>" },
	/* XXX XXX XXX NULL's are replaced by the program */
	{ NULL, (char * ) "<mod1>0",
	  term_event_handler, (guint)&data[0], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>1",
	  term_event_handler, (guint)&data[1], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>2",
	  term_event_handler, (guint)&data[2], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>3",
	  term_event_handler, (guint)&data[3], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>4",
	  term_event_handler, (guint)&data[4], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>5",
	  term_event_handler, (guint)&data[5], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>6",
	  term_event_handler, (guint)&data[6], (char * ) "<CheckItem>" },
	{ NULL, (char * ) "<mod1>7",
	  term_event_handler, (guint)&data[7], (char * ) "<CheckItem>" },

	/* "Options" menu */
	{ (char * ) "/Options", NULL,
	  NULL, 0, (char * ) "<Branch>" },

	/* "Font" submenu */
	{ (char * ) "/Options/Font", NULL,
	  NULL, 0, (char * ) "<Branch>" },
	/* XXX XXX XXX Again, NULL's are filled by the program */
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[0], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[1], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[2], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[3], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[4], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[5], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[6], NULL },
	{ NULL, NULL,
	  change_font_event_handler, (guint)&data[7], NULL },

#ifdef USE_GRAPHICS

	/* "Graphics" submenu */
	{ (char * ) "/Options/Graphics", NULL,
	  NULL, 0, (char * ) "<Branch>" },
	{ (char * ) "/Options/Graphics/None", NULL,
	  change_graf_mode_event_handler, GRAPHICS_NONE, (char * ) "<CheckItem>" },
	{ (char * ) "/Options/Graphics/8x8", NULL,
	  change_graf_mode_event_handler, GRAPHICS_ORIGINAL, (char * ) "<CheckItem>" },
	{ (char * ) "/Options/Graphics/16x16", NULL,
	  change_graf_mode_event_handler, GRAPHICS_ADAM_BOLT, (char * ) "<CheckItem>" },
	{ (char * ) "/Options/Graphics/32x32", NULL,
	  change_graf_mode_event_handler, GRAPHICS_DAVID_GERVAIS, (char * ) "<CheckItem>" },
	{ (char * ) "/Options/Graphics/sep1", NULL,
	  NULL, 0, (char * ) "<Separator>" },
	{ (char * ) "/Options/Graphics/Transparency", NULL,
	  change_trans_mode_event_handler, 0, (char * ) "<CheckItem>" },
	{ (char * ) "/Options/Graphics/BigTile", NULL,
	  change_bigtile_mode_event_handler, 0, (char * ) "<CheckItem>" },
#endif /* USE_GRAPHICS */
};


/*
 * XXX XXX Fill those NULL's in the menu definition with
 * angband_term_name[] strings
 */
static void setup_menu_paths(void)
{
	int i;
	int nmenu_items = sizeof(main_menu_items) / sizeof(main_menu_items[0]);
	GtkItemFactoryEntry *term_entry, *font_entry;
	char buf[64];

	/* Find the "Terms" menu */
	for (i = 0; i < nmenu_items; i++)
	{
		/* Skip NULLs */
		if (main_menu_items[i].path == NULL) continue;

		/* Find a match */
		if (streq(main_menu_items[i].path, "/Terms")) break;
	}
	g_assert(i < (nmenu_items - MAX_TERM_DATA));

	/* Remember the location */
	term_entry = &main_menu_items[i + 1];

	/* Find "Font" menu */
	for (i = 0; i < nmenu_items; i++)
	{
		/* Skip NULLs */
		if (main_menu_items[i].path == NULL) continue;

		/* Find a match */
		if (streq(main_menu_items[i].path, "/Options/Font")) break;
	}
	g_assert(i < (nmenu_items - MAX_TERM_DATA));

	/* Remember the location */
	font_entry = &main_menu_items[i + 1];

	/* For each terminal */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* XXX XXX Build the real path name to the entry */
		strnfmt(buf, 64, "/Terms/%s", angband_term_name[i]);

		/* XXX XXX Store it in the menu definition */
		term_entry[i].path = (char *) string_make(buf);

		/* XXX XXX Build the real path name to the entry */
		strnfmt(buf, 64, "/Options/Font/%s", angband_term_name[i]);

		/* XXX XXX Store it in the menu definition */
		font_entry[i].path = (char *) string_make(buf);
	}
}


/*
 * Construct a menu hierarchy using GtkItemFactory, setting up
 * callbacks and accelerators along the way, and return
 * a GtkMenuBar widget.
 */
static GtkWidget *get_main_menu(term_data *td)
{
	GtkItemFactory *item_factory;
	GtkAccelGroup *accel_group;
	gint nmenu_items = sizeof(main_menu_items) / sizeof(main_menu_items[0]);


	/* XXX XXX Setup path names in the "Terms" and "Font" menus */
	setup_menu_paths();

	/* Allocate an accelerator group */
	accel_group = gtk_accel_group_new();
	g_assert(accel_group);

	/* Initialise the item factory */
	item_factory = gtk_item_factory_new(GTK_TYPE_MENU_BAR, "<Angband>",
		accel_group);
	g_assert(item_factory);

	/* Generate the menu items */
	gtk_item_factory_create_items(item_factory, nmenu_items,
		main_menu_items, NULL);

	/* Attach the new accelerator group to the window */
	gtk_window_add_accel_group(GTK_WINDOW(td->window), accel_group);

	/* Return the actual menu bar created */
	return (gtk_item_factory_get_widget(item_factory, "<Angband>"));
}


/*
 * XXX XXX Free strings allocated by setup_menu_paths()
 */
static void free_menu_paths(void)
{
	int i;
	int nmenu_items = sizeof(main_menu_items) / sizeof(main_menu_items[0]);
	GtkItemFactoryEntry *term_entry, *font_entry;

	/* Find the "Terms" menu */
	for (i = 0; i < nmenu_items; i++)
	{
		/* Skip NULLs */
		if (main_menu_items[i].path == NULL) continue;

		/* Find a match */
		if (streq(main_menu_items[i].path, "/Terms")) break;
	}
	
	/* Paranoia */
	g_assert(i < (nmenu_items - MAX_TERM_DATA));

	/* Remember the location */
	term_entry = &main_menu_items[i + 1];

	/* Find "Font" menu */
	for (i = 0; i < nmenu_items; i++)
	{
		/* Skip NULLs */
		if (main_menu_items[i].path == NULL) continue;

		/* Find a match */
		if (streq(main_menu_items[i].path, "/Options/Font")) break;
	}
	
	/* Paranoia */
	g_assert(i < (nmenu_items - MAX_TERM_DATA));

	/* Remember the location */
	font_entry = &main_menu_items[i + 1];

	/* For each terminal */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		/* XXX XXX Free Term menu path */
		string_free((cptr)term_entry[i].path);

		/* XXX XXX Free Font menu path */
		string_free((cptr)font_entry[i].path);
	}
}


/*
 * Quit hook when exiting the game
 */
static void hook_quit(cptr str)
{
	/* Hack - Ignore parameter */
	(void) str;
	
	/* Free menu paths dynamically allocated */
	free_menu_paths();
	
	gtk_exit(0);
}


/*
 * Hook to tell the user something important
 */
static void hook_plog(cptr str)
{
	/* Warning message */
	gtk_message(str);
}


/*
 * Handle destruction of Subwindows
 */
static void destroy_sub_event_handler(GtkWidget *window, gpointer user_data)
{
	term_data *td = (term_data *)user_data;
	
	/* Do nothing if window is not visible */
	if (!td->shown) return;
	
	/* Hide the window */
	gtk_widget_hide_all(window);

}


/*
 * Handle deletion of Subwindows
 */
static void delete_sub_event_handler(GtkWidget *window, gpointer user_data)
{
	term_data *td = (term_data *)user_data;
	
	/* Do nothing if window is not visible */
	if (!td->shown) return;
	
	td->shown = FALSE;
	
	/* Hide the window */
	gtk_widget_hide_all(window);
}


static errr term_data_init(term_data *td, int i)
{
	cptr font;
	
	term *t = &td->t;

	td->cols = 80;
	td->rows = 24;

	/* Initialize the term */
	term_init(t, td->cols, td->rows, 1024);
	
	/* Save the name */
	td->name = angband_term_name[i];
	
	/* Use a "soft" cursor */
	t->soft_cursor = TRUE;

	/* Erase with "black space" */
	t->attr_blank = TERM_DARK;
	t->char_blank = ' ';

	t->xtra_hook = Term_xtra_gtk;
	t->text_hook = Term_text_gtk;
    t->wipe_hook = Term_wipe_gtk;
	t->curs_hook = Term_curs_gtk;
#ifdef USE_GRAPHICS
	t->pict_hook = Term_pict_gtk;
	t->higher_pict = TRUE;
#endif /* USE_GRAPHICS */

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
	GtkWidget *menu_bar, *box;
	
	bool main_win = (i == 0) ? TRUE : FALSE;		

	/* Create window */
	td->window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	/* Set title */
	gtk_window_set_title(GTK_WINDOW(td->window), td->name);
	
	/* Create drawing area */
	td->drawing_area = gtk_drawing_area_new();
	
	/* Set the size of the drawing area */
	gtk_drawing_area_size(GTK_DRAWING_AREA(td->drawing_area),
			td->cols * td->font_wid, td->rows * td->font_hgt);
	
	/* Set geometry hints */
	set_size_hints(td);
	
	/* Set resize policy */
	gtk_window_set_policy(GTK_WINDOW(td->window), TRUE, TRUE, TRUE);
	
	
	/* Install window event handlers */
	gtk_signal_connect(GTK_OBJECT(td->window), "key_press_event",
		GTK_SIGNAL_FUNC(keypress_event_handler), NULL);
	
	/* Destroying the Angband window terminates the game */
	if (main_win)
	{
		gtk_signal_connect(GTK_OBJECT(td->window), "delete_event",
			GTK_SIGNAL_FUNC(delete_event_handler), NULL);
		
		gtk_signal_connect(GTK_OBJECT(td->window), "destroy_event",
			GTK_SIGNAL_FUNC(destroy_event_handler), NULL);
	}

	/* The other windows are just hidden */
	else
	{
		gtk_signal_connect(GTK_OBJECT(td->window), "delete_event",
			GTK_SIGNAL_FUNC(delete_sub_event_handler), td);
		
		gtk_signal_connect(GTK_OBJECT(td->window), "destroy_event",
			GTK_SIGNAL_FUNC(destroy_sub_event_handler), td);
	}
	
	/* Install drawing area event handlers */
	gtk_signal_connect(GTK_OBJECT(td->drawing_area), "realize",
		GTK_SIGNAL_FUNC(realize_event_handler), (gpointer)td);
	gtk_signal_connect(GTK_OBJECT(td->drawing_area), "show",
		GTK_SIGNAL_FUNC(show_event_handler), (gpointer)td);
	gtk_signal_connect(GTK_OBJECT(td->drawing_area), "hide",
		GTK_SIGNAL_FUNC(hide_event_handler), (gpointer)td);
	gtk_signal_connect(GTK_OBJECT(td->drawing_area), "size_allocate",
		GTK_SIGNAL_FUNC(size_allocate_event_handler), (gpointer)td);
	gtk_signal_connect(GTK_OBJECT(td->drawing_area), "expose_event",
		GTK_SIGNAL_FUNC(expose_event_handler), (gpointer)td);

	/* Create menu */
	if (main_win)
	{
		/* Build the main menu bar */
		menu_bar = get_main_menu(td);
		g_assert(menu_bar);

		/* Since it's tedious to scatter the menu update code around */
		add_menu_update_callbacks();
		
		/* Pack the menu bar together with the main window */
		/* For vertical placement of the menu bar and the drawing area */
		box = gtk_vbox_new(FALSE, 0);

		/* Let the window widget own it */
		gtk_container_add(GTK_CONTAINER(td->window), box);
		
		/* The main window has a menu bar */
		gtk_box_pack_start(GTK_BOX(box), menu_bar, FALSE, FALSE, NO_PADDING);
	}
	else
	{
		/* Pack the menu bar together with the main window */
		/* For vertical placement of the menu bar and the drawing area */
		box = gtk_vbox_new(FALSE, 0);

		/* Let the window widget own it */
		gtk_container_add(GTK_CONTAINER(td->window), box);
	}
	
	/* And place the drawing area just beneath it */
	gtk_box_pack_start_defaults(GTK_BOX(box), td->drawing_area);
		
	/* Show the widgets - use of td->shown is a dirty hack XXX XXX */
	if (!td->shown) return;

	/* Show the widgets */
	gtk_widget_show_all(td->window);
}


/*
 * Initialization function
 */
errr init_gtk(int argc, char **argv, unsigned char *new_game)
{
	int i;

#ifdef USE_GRAPHICS
	int graphmode = GRAPHICS_ANY;
#endif /* USE_GRAPHICS */
	
	/* See if gtk exists and works */
	if (!gtk_init_check(&argc, &argv)) return (1);
	
	/* Hack - save variable so that everyone can use it */
	gtk_newgame = *new_game;
	
	/* Hack - commandline option sets new game? */
	game_in_progress = gtk_newgame;

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
			int bitdepth = 0;
			
			bitdepth = atoi(&argv[i][2]);
			
			/* Paranoia */
			if (bitdepth == 32) graphmode = GRAPHICS_DAVID_GERVAIS;
			if (bitdepth == 16) graphmode = GRAPHICS_ADAM_BOLT;
			if (bitdepth == 8) graphmode = GRAPHICS_ORIGINAL;
			
			continue;
		}

#endif /* USE_GRAPHICS */

		plog_fmt("Ignoring option: %s", argv[i]);
	}
	
#ifdef USE_GRAPHICS
	/* We support bigtile mode */
	if (arg_bigtile && arg_graphics) use_bigtile = TRUE;
#endif /* USE_GRAPHICS */

	/*
	 * Initialize the terms
	 */
	for (i = 0; i < MAX_TERM_DATA; i++)
	{
		term_data *td = &data[i];
		
		/* Initialize the term_data */
		term_data_init(td, i);

		/* Save global entry */
		angband_term[i] = Term;
	}

#ifdef USE_GRAPHICS

	/* Set graphics mode */
	if (arg_graphics) set_graph_mode(graphmode);

#endif /* USE_GRAPHICS */
		
	/*
	 * Initialize the windows
	 * (Backwards so main window is on top)
	 */
	for (i = MAX_TERM_DATA - 1; i >= 0; i--)
	{
		term_data *td = &data[i];

		/* Hack - Set the shown flag */
		if (i < num_term)
		{
			td->shown = TRUE;
		}
		else
		{	
			td->shown = FALSE;
		}
		
		/* Init the window */
		init_gtk_window(td, i);
	}

#ifdef USE_GRAPHICS
	if (use_graphics)
	{	
		/* Initialise the graphics */
		graf_init();
	}

#endif /* USE_GRAPHICS */


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
	plog_aux = hook_plog;
	quit_aux = hook_quit;
	core_aux = hook_quit;

	/* Catch nasty signals */
	signals_init();
	
	/* Need to initialize system type */
	ANGBAND_SYS = "gtk";

	/* Initialize */
	init_angband();
	
	/* Prompt the user */
	prtf(17, 23, "[Choose 'New' or 'Open' from the 'File' menu]");
	Term_fresh();
	
	while (!game_in_progress)
	{
		while (gtk_events_pending())
		{
			gtk_main_iteration();
		}

		/* Wait so we don't waste all the processor */
		usleep(200);
		
		/* Die if window is closed */
		if (gtk_exitgame) quit(NULL);
	}
	
	/* Load 'newgame' flag */
	*new_game = gtk_newgame;
	
	/* Press a key for the player */
	Term_keypress(' ');
	
	/* Success */
	return (0);
}

#endif /* USE_GTK */
