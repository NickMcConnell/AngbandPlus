/* File: util-dll.c */

/* Purpose: misc stuff */

/*
 * Copyright (c) 1997-2001 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "tnb.h"

#include <limits.h>
#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

#define MAX_DELTA (255 * 6)
#define MAX_COLOR_ENTRY 1021

#ifdef HAVE_TKFONT_H
#include <tkFont.h>
#else /* HAVE_TKFONT_H */

typedef struct TkFontAttributes {
    Tk_Uid family;		/* Font family, or NULL to represent
				 * plaform-specific default system font. */
    int size;			/* Pointsize of font, 0 for default size, or
				 * negative number meaning pixel size. */
    int weight;			/* Weight flag; see below for def'n. */
    int slant;			/* Slant flag; see below for def'n. */
    int underline;		/* Non-zero for underline font. */
    int overstrike;		/* Non-zero for overstrike font. */
} TkFontAttributes;

#define TK_FW_NORMAL	0
#define TK_FW_BOLD	1

#define TK_FS_ROMAN	0	
#define TK_FS_ITALIC	1

typedef struct TkFontMetrics {
    int	ascent;			/* From baseline to top of font. */
    int	descent;		/* From baseline to bottom of font. */
    int maxWidth;		/* Width of widest character in font. */
    int fixed;			/* Non-zero if this is a fixed-width font,
				 * 0 otherwise. */
} TkFontMetrics;


typedef struct TkFont {
    /*
     * Fields used and maintained exclusively by generic code.
     */

    int resourceRefCount;	/* Number of active uses of this font (each
				 * active use corresponds to a call to
				 * Tk_AllocFontFromTable or Tk_GetFont).
				 * If this count is 0, then this TkFont
				 * structure is no longer valid and it isn't
				 * present in a hash table: it is being
				 * kept around only because there are objects
				 * referring to it.  The structure is freed
				 * when resourceRefCount and objRefCount
				 * are both 0. */
    int objRefCount;		/* The number of Tcl objects that reference
				 * this structure. */
    Tcl_HashEntry *cacheHashPtr;/* Entry in font cache for this structure,
				 * used when deleting it. */
    Tcl_HashEntry *namedHashPtr;/* Pointer to hash table entry that
				 * corresponds to the named font that the
				 * tkfont was based on, or NULL if the tkfont
				 * was not based on a named font. */
    Screen *screen;		/* The screen where this font is valid. */
    int tabWidth;		/* Width of tabs in this font (pixels). */
    int	underlinePos;		/* Offset from baseline to origin of
				 * underline bar (used for drawing underlines
				 * on a non-underlined font). */
    int underlineHeight;	/* Height of underline bar (used for drawing
				 * underlines on a non-underlined font). */

    /*
     * Fields used in the generic code that are filled in by
     * platform-specific code.
     */

    Font fid;			/* For backwards compatibility with XGCValues
				 * structures.  Remove when TkGCValues is
				 * implemented.  */
    TkFontAttributes fa;	/* Actual font attributes obtained when the
				 * the font was created, as opposed to the
				 * desired attributes passed in to
				 * TkpGetFontFromAttributes().  The desired
				 * metrics can be determined from the string
				 * that was used to create this font. */
    TkFontMetrics fm;		/* Font metrics determined when font was
				 * created. */
    struct TkFont *nextPtr;	/* Points to the next TkFont structure with
				 * the same name.  All fonts with the
				 * same name (but different displays) are
				 * chained together off a single entry in
				 * a hash table. */
} TkFont;

#endif /* !HAVE_TKFONT_H */

/* Structure for a hash table used by rgb2index() */
typedef struct
{
	int valid; /* The hash-table entry is valid */
	u32b pixel; /* an RGB pixel value */
	int index; /* closest matching palette index for 'pixel' */
} t_color_entry;

typedef struct IndexedColor IndexedColor;
struct IndexedColor
{
	byte rgb[256 * 3];
	t_color_entry hash[MAX_COLOR_ENTRY];
	void *platData;
};

static IndexedColor g_palette;
static bool Palette_Initialized = 0;

int g_palette_black = 255;
int g_palette_white = 0;
int g_colormap_black;
int g_colormap_white;

byte g_palette2colormap[256];

/*
 * Append an element to an array
 */
void *Array_Append(void *array_ptr, int *count, int elem_size,
	void *elem_ptr)
{
	char *ap = array_ptr;
	int n = (*count) + 1;

	ap = Tcl_Realloc(ap, n * elem_size);
	(void) memcpy(ap + (n - 1) * elem_size, elem_ptr, elem_size);
	(*count) = n;
	return ap;
}

/*
 * Insert an element in an array
 */
void *Array_Insert(void *array_ptr, int *count, int elem_size,
	int index)
{
	char *ap = array_ptr;
	int n = (*count) + 1;

	if (index < 0) index = 0;
	if (index >= n) index = n - 1;

	ap = Tcl_Realloc(ap, n * elem_size);
	if (index != n - 1)
	{
		(void) memcpy(ap + (index + 1) * elem_size,
			ap + index * elem_size,
			(n - index - 1) * elem_size);
	}
	else
	{
		memset(ap + index * elem_size, 0, elem_size);
	}
	(*count) = n;
	return ap;
}

/*
 * Delete an element from an array
 */
void *Array_Delete(void *array_ptr, int *count, int elem_size,
	int index)
{
	char *ap = array_ptr;
	int i, n = (*count) - 1;

	if (index < 0) index = 0;
	if (index > n) index = n;

	if (index != n)
	{
		(void) memcpy(ap + index * elem_size,
			ap + (index + 1) * elem_size,
			(n - index) * elem_size);
	}

	/* Zero the trailing slot to catch errors */
	for (i = 0; i < elem_size; i++)
	{
		ap[n * elem_size + i] = 0;
	}
	
	(*count) = n;
	return (void *) Tcl_Realloc(ap, n * elem_size);
}

static void IndexedColor_ResetHash(IndexedColor *idc)
{
	int i;

	for (i = 0; i < 256; i++)
	{
		idc->hash[i].valid = 0;
	}
}

/*
 * Returns the nearest matching palette index for the given
 * RGB values.
 */
static int IndexedColor_RGB2Index(IndexedColor *idc, unsigned char r, unsigned char g, unsigned char b)
{
	int i, diff, index, max;
	unsigned char *col;
	unsigned long pixel;
	t_color_entry *entry;

	/* Calculate the pixel value */
	pixel = (r << 16) | (g << 8) | b;

	/* Hash the pixel value */
	entry = &idc->hash[pixel % MAX_COLOR_ENTRY];

	/* We already calculated the palette index */
	if (entry->valid && (entry->pixel == pixel))
	{
		/* Return the palette index */
		return entry->index;
    }

	index = 0;
	max = MAX_DELTA;

	col = idc->rgb;

	/* Check each palette entry */
	for (i = 0; i < 256; i++)
	{
		/* Work out the 'difference' between the colours */

		diff = abs(r - col[0]);
		diff += abs(g - col[1]);
		diff += abs(b - col[2]);

		/* Multiply by the 'colour factor' */
		diff *= 3;

		/* Add in the effects of brightness */
		diff += abs(b + r + g - col[0] - col[1] - col[2]);
		
		col += 3;

		/* This palette entry is a better match than any other so far */
		if (diff < max)
		{
			/* Remember the palette index */
			index = i;

			/* Remember the minimum difference */
			max = diff;
		}
	}

	/* Remember the hash table entry info */
	entry->pixel = pixel;
	entry->index = index;
	entry->valid = 1;

	/* Return the palette index */
	return index;
}

/* set $index ?$color? */
static int objcmd_palette_set(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	char buf[20];
	int i, r, g, b;

	if (Tcl_GetIntFromObj(interp, objV[1], &i) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((i < 0) || (i >= 256))
	{
		return TCL_ERROR;
	}

	if (objC == 3)
	{
		XColor *xColorPtr = Tk_AllocColorFromObj(interp, Tk_MainWindow(interp), objV[2]);
		if (xColorPtr == NULL)
		{
			return TCL_ERROR;
		}
		g_palette.rgb[i * 3] = xColorPtr->red / 255;
		g_palette.rgb[i * 3 + 1] = xColorPtr->green / 255;
		g_palette.rgb[i * 3 + 2] = xColorPtr->blue / 255;
		Tk_FreeColor(xColorPtr);
		return TCL_OK;
	}

	r = g_palette.rgb[i * 3];
	g = g_palette.rgb[i * 3 + 1];
	b = g_palette.rgb[i * 3 + 2];
	strnfmt(buf, 20, "#%02X%02X%02X", r, g, b);
	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	return TCL_OK;
}

static CommandInit commandInit[] = {
	{0, "palette", 0, 0, NULL, NULL, (ClientData) 0},
		{1, "set", 2, 3, "?color?", objcmd_palette_set, (ClientData) 0},
	{0, NULL, 0, 0, NULL, NULL, 0}
};

/* Uniform colour table */
static byte def_pal1[6] = {255, 204, 153, 102, 51, 0};
static byte def_pal2[10] = {238, 221, 187, 170, 136, 119, 85, 68, 34, 17};

int Palette_Init(Tcl_Interp *interp)
{
	int i, j, k;
	unsigned char *rgb;

	if (Palette_Initialized) return TCL_OK;

	rgb = g_palette.rgb;
	g_palette.platData = NULL;
	
	/* Create colour cube */
	for (i = 0; i < 6; i++)
	{
		for (j = 0; j < 6; j++)
		{
			for (k = 0; k < 6; k++)
			{
				/* Write this color to the array */
				*rgb++ = def_pal1[i];
				*rgb++ = def_pal1[j];
				*rgb++ = def_pal1[k];
			}
		}
	}
	
	/* Create primary colours */
	for (i = 0; i < 10; i++)
	{
		*rgb++ = def_pal2[i];
		*rgb++ = 0;
		*rgb++ = 0;
	}
	for (i = 0; i < 10; i++)
	{
		*rgb++ = 0;
		*rgb++ = def_pal2[i];
		*rgb++ = 0;
	}
	for (i = 0; i < 10; i++)
	{
		*rgb++ = 0;
		*rgb++ = 0;
		*rgb++ = def_pal2[i];
	}

	/* Create greys */
	for (i = 0; i < 10; i++)
	{
		*rgb++ = def_pal2[i];
		*rgb++ = def_pal2[i];
		*rgb++ = def_pal2[i];
	}

	g_palette.platData = Plat_PaletteInit(g_palette.rgb);

	(void) CommandInfo_Init(interp, commandInit, NULL);

	Palette_ResetHash();

	Palette_Initialized = TRUE;

	Colormap_Init(interp);

	return TCL_OK;
}


void Palette_ResetHash(void)
{
	IndexedColor_ResetHash(&g_palette);
}

/*
 * Returns the nearest matching palette index for the given
 * RGB values.
 */
int Palette_RGB2Index(unsigned char r, unsigned char g, unsigned char b)
{
	return IndexedColor_RGB2Index(&g_palette, r, g, b);
}


#ifdef PLATFORM_WIN

void *Palette_GetHPal(void)
{
	return g_palette.platData; /* HPALETTE */
}

#endif /* PLATFORM_WIN */

unsigned char *Palette_GetRGB(void)
{
	return g_palette.rgb;
}


Tcl_Obj *ExtToUtf_NewStringObj(CONST char *bytes, int length)
{
	char *utfString;
	Tcl_DString utfDString;
	Tcl_Obj *objResult;

	utfString = Tcl_ExternalToUtfDString(NULL, bytes, length, &utfDString);
	objResult = Tcl_NewStringObj(utfString, Tcl_DStringLength(&utfDString));
	Tcl_DStringFree(&utfDString);
	return objResult;
}

void ExtToUtf_SetResult(Tcl_Interp *interp, cptr string)
{
	char *utfString;
	Tcl_DString utfDString;

	utfString = Tcl_ExternalToUtfDString(NULL, string, -1, &utfDString);
	Tcl_SetResult(interp, utfString, TCL_VOLATILE);
	Tcl_DStringFree(&utfDString);
}

char *UtfToExt_TranslateFileName(Tcl_Interp *interp, char *utfPath, Tcl_DString *extDStringPtr)
{
	char *extString, *utfString;
	Tcl_DString utfDString;
	int utfLen;

	utfString = Tcl_TranslateFileName(interp, utfPath, &utfDString);
	if (utfString == NULL) return NULL;
	utfLen = Tcl_DStringLength(&utfDString);
	extString = Tcl_UtfToExternalDString(NULL, utfString, utfLen, extDStringPtr);
	Tcl_DStringFree(&utfDString);
	return extString;
}

static IndexedColor g_colormap;
unsigned char *g_colormap_rgb;

int Colormap_Init(Tcl_Interp *interp)
{
#ifdef PLATFORM_X11
	Tk_Window tkwin = Tk_MainWindow(interp);
	Display *display = Tk_Display(tkwin);
	Colormap colormap = Tk_Colormap(tkwin); /* DefaultColormap() */
	XColor xColor;
#endif
	int i, k, r, g, b;

	IndexedColor_ResetHash(&g_colormap);

#ifdef PLATFORM_X11
	if (Tk_Depth(tkwin) == 8)
	{
		/*
		 * Allocate each color in the colormap, to prevent the colormap
		 * entries from changing.
		 */
		for (i = 0; i < 256; i++)
		{
			xColor.pixel = i;
			XQueryColor(display, colormap, &xColor);
			(void) Tk_GetColorByValue(tkwin, &xColor);
		}
	}
#endif /* PLATFORM_X11 */

	for (i = 0; i < 256; i++)
	{
		r = g_palette.rgb[i * 3 + 0];
		g = g_palette.rgb[i * 3 + 1];
		b = g_palette.rgb[i * 3 + 2];

#ifdef PLATFORM_X11
		if (Tk_Depth(tkwin) == 8)
		{
			/* Get the XColor at this location in the colormap */
			xColor.pixel = i;
			XQueryColor(display, colormap, &xColor);
	
			/* Convert RGB values to 0-255 */
			r = xColor.red / 255;
			g = xColor.green / 255;
			b = xColor.blue / 255;
		}
#endif /* PLATFORM_X11 */

		/* Remember RGB values at this colormap index */
		g_colormap.rgb[i * 3 + 0] = r;
		g_colormap.rgb[i * 3 + 1] = g;
		g_colormap.rgb[i * 3 + 2] = b;
	}

	for (i = 0; i < 256; i++)
	{
		/* Get the RGB values at this palette index */
		r = g_palette.rgb[i * 3 + 0];
		g = g_palette.rgb[i * 3 + 1];
		b = g_palette.rgb[i * 3 + 2];		

		/* Find the closest color in the colormap */
		k = Colormap_RGB2Index(r, g, b);

		/* Map palette index -> colormap index */
		g_palette2colormap[i] = k;
	}

	/* Get the black and white pixels */
	g_colormap_black = PALETTE_BLACK;
	g_colormap_white = PALETTE_WHITE;

#ifdef PLATFORM_X11
	if (Tk_Depth(tkwin) == 8)
	{
		g_colormap_black = BlackPixelOfScreen(Tk_Screen(tkwin));
		g_colormap_white = WhitePixelOfScreen(Tk_Screen(tkwin));
	}
#endif /* PLATFORM_X11 */

	g_colormap_rgb = g_colormap.rgb;

	return TCL_OK;
}

unsigned char *Colormap_GetRGB(void)
{
	return g_colormap.rgb;
}

int Colormap_RGB2Index(unsigned char r, unsigned char g, unsigned char b)
{
	return IndexedColor_RGB2Index(&g_colormap, r, g, b);
}

/*
 * Return a "standardized" string describing a font.
 */
int objcmd_fontdesc(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tk_Font tkfont;
	TkFont *fontPtr;
	char buf[1024];

	/* Hack - ignore unused parameter */
	(void) dummy;
	
    if (objc != 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "font");
		return TCL_ERROR;
    }

	tkfont = Tk_AllocFontFromObj(interp, Tk_MainWindow(interp), objv[1]);
	if (tkfont == NULL)
	{
		return TCL_ERROR;
	}

	fontPtr = (TkFont *) tkfont;
	
	strnfmt(buf, 1024, "-family {%s} -size %d -weight %s -slant %s "
		"-underline %d -overstrike %d",
		fontPtr->fa.family, fontPtr->fa.size,
		(fontPtr->fa.weight == TK_FW_BOLD) ? "bold" : "normal",
		(fontPtr->fa.slant == TK_FS_ITALIC) ? "italic" : "roman",
		fontPtr->fa.underline,
		fontPtr->fa.overstrike);
		
	Tcl_SetStringObj(Tcl_GetObjResult(interp), buf, -1);

	Tk_FreeFontFromObj(Tk_MainWindow(interp), objv[1]);

	return TCL_OK;
}


cptr keyword_term_color[16] = {
	"TERM_DARK",
	"TERM_WHITE",
	"TERM_SLATE",
	"TERM_ORANGE",
	"TERM_RED",
	"TERM_GREEN",
	"TERM_BLUE",
	"TERM_UMBER",
	"TERM_L_DARK",
	"TERM_L_WHITE",
	"TERM_VIOLET",
	"TERM_YELLOW",
	"TERM_L_RED",
	"TERM_L_GREEN",
	"TERM_L_BLUE",
	"TERM_L_UMBER"
};


byte g_prompt_attr = TERM_WHITE;

/*
 * Display a prompt in the "message line", but don't save it.
 */
void prompt_print(cptr str)
{
	cptr attr = keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "set", str, attr, NULL);
}

/*
 * Erase the "message line".
 */
void prompt_erase(void)
{
	angtk_eval("angband_prompt", "wipe", NULL);
}

/*
 * Display a formatted prompt, using "vstrnfmt()" and "prompt_print()".
 */
void prompt_format(cptr fmt, ...)
{
	va_list vp;
	
	char buf[1024];
	
	/* Begin the Varargs Stuff */
	va_start(vp, fmt);
	
	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, fmt, &vp);
	
	/* End the Varargs Stuff */
	va_end(vp);
	
	/* Display */
	prompt_print(buf);
}

void prompt_append(cptr str)
{
	cptr attr = keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "append", str, attr, NULL);
}

void prompt_open(cptr str)
{
	cptr attr = keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "open", str, attr, NULL);
}

void prompt_update(cptr str)
{
	cptr attr = keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "update", str, attr, NULL);
}

/*
 * Display a prompt, wait for a keypress.
 */
void any_more(cptr prompt)
{
	bool old_quick = quick_messages;

	/* Set the prompt */
	if (!prompt)
		prompt = "Hit any key to continue";

	/* Set quick_messages so any key is accepted */
	quick_messages = TRUE;

	/* Display the message, wait for a response */
	msgf(prompt);
	message_flush();

	/* Restore quick_messages */
	quick_messages = old_quick;
}

int ExtToUtf_SetArrayValueString(cptr varName, cptr field, cptr value)
{
	Tcl_DString utfDString;
	cptr utfString;

	Tcl_ExternalToUtfDString(NULL, value, -1, &utfDString);
	utfString = Tcl_DStringValue(&utfDString);
	if (Tcl_SetVar2(g_interp, varName, field, utfString, TCL_LEAVE_ERR_MSG)
		== NULL)
	{
		Tcl_DStringFree(&utfDString);
		return TCL_ERROR;
	}
	Tcl_DStringFree(&utfDString);	
	return TCL_OK;
}


