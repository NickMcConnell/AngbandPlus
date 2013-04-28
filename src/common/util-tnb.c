/* File util-tnb.c */

/* Purpose: generic utilities */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tk.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"

#ifdef PLATFORM_WIN
#include <tkWinInt.h>
#endif
#ifdef PLATFORM_X11
#include <tkUnixInt.h>
#endif
#include <tkFont.h>

/*
 * Return a "standardized" string describing a font.
 */
int objcmd_fontdesc(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tk_Font tkfont;
	TkFont *fontPtr;
	char buf[1024];

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

	(void) sprintf(buf, "-family {%s} -size %d -weight %s -slant %s "
		"-underline %d -overstrike %d",
		fontPtr->fa.family, fontPtr->fa.size,
		(fontPtr->fa.weight == TK_FW_BOLD) ? "bold" : "normal",
		(fontPtr->fa.slant == TK_FS_ITALIC) ? "italic" : "roman",
		fontPtr->fa.underline,
		fontPtr->fa.overstrike);

	Tcl_SetResult(interp, buf, TCL_VOLATILE);

	Tk_FreeFontFromObj(Tk_MainWindow(interp), objv[1]);

	return TCL_OK;
}

#if TK_MINOR_VERSION >= 2 && TK_MINOR_VERSION < 5

#include "tkMenu.h"

/*
 * This is a colossal hack to counterattack the abysmal performance of
 * "$menu entryconfigure $index -state $state" under Tk 8.2. It allows
 * us to directly change the state of individual menu entries.
 *
 * Usage:
 *		menuentrystate $menu $index ?$state?
 */
int objcmd_menuentrystate(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *tkMenuStateStrings[] = {"active", "normal", "disabled",
		(char *) NULL};
	Tcl_HashEntry *hashEntryPtr;
	Tcl_HashTable *menuTablePtr;
	TkMenu *menuPtr;
	TkMenuEntry *mePtr;
	TkMenuReferences *menuRefPtr;
	char *pathName;
	int entryIndex, state;

	/* Required number of arguments */
	if ((objc != 3) && (objc != 4))
	{
		Tcl_WrongNumArgs(interp, 1, objv, "menu index ?state?");
		return TCL_ERROR;
	}

	/* Lookup the menu by name */
	pathName = Tcl_GetStringFromObj(objv[1], NULL);
	menuTablePtr = TkGetMenuHashTable(interp);
	hashEntryPtr = Tcl_FindHashEntry(menuTablePtr, pathName);
	if (hashEntryPtr == NULL)
	{
		return TCL_ERROR;
	}

	/* See how we get access to the TkMenu structure */
	menuRefPtr = (TkMenuReferences *) Tcl_GetHashValue(hashEntryPtr);
	menuPtr = menuRefPtr->menuPtr->masterMenuPtr;
	if (menuPtr == NULL)
	{
		return TCL_ERROR;
	}

	/* Get the entry index */
	if (TkGetMenuIndex(interp, menuPtr, objv[2], 0, &entryIndex) != TCL_OK)
	{
		return TCL_ERROR;	
	}

	/* Return the current value */
	if (objc == 3)
	{
		mePtr = menuPtr->entries[entryIndex];
		Tcl_SetResult(interp, (char *) tkMenuStateStrings[mePtr->state], TCL_STATIC);
		return TCL_OK;
	}

	/* Get the desired state */
	if (Tcl_GetIndexFromObj(interp, objv[3], tkMenuStateStrings, "state", 0, 
		&state) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Configure the menu (in all the clone menus as well) */
	for (; menuPtr != NULL; menuPtr = menuPtr->nextInstancePtr)
	{
		mePtr = menuPtr->entries[entryIndex];
		mePtr->state = state;
		TkpConfigureMenuEntry(mePtr);
	}

	/* Success */
	return TCL_OK;
}

#endif /* Tk 8.2 */

#ifdef BORG_TK

/*
 * XXX Mega-Hack -- The Borg needs to know the prompt/message.
 */
ANG_STORAGE_CLASS char angtk_prompt_text[256] = "";
char angtk_prompt_prefix[256] = "";

#endif /* BORG_TK */

byte g_prompt_attr = TERM_WHITE;

/*
 * Display a prompt in the "message line", but don't save it.
 */
void prompt_print(cptr str)
{
	char *attr = (char *) keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "set", str, attr, NULL);
#ifdef BORG_TK
	(void) strcpy(angtk_prompt_text, str);
#endif /* BORG_TK */
}

/*
 * Erase the "message line".
 */
void prompt_erase(void)
{
	angtk_eval("angband_prompt", "wipe", NULL);
#ifdef BORG_TK
	angtk_prompt_text[0] = '\0';
#endif /* BORG_TK */
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
	(void)vstrnfmt(buf, 1024, fmt, vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	prompt_print(buf);
}

void prompt_append(cptr str)
{
	char *attr = (char *) keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "append", str, attr, NULL);
#ifdef BORG_TK
	(void) strcat(angtk_prompt_text, str);
#endif
}

void prompt_open(cptr str)
{
	char *attr = (char *) keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "open", str, attr, NULL);
#ifdef BORG_TK
	(void) strcpy(angtk_prompt_prefix, str);
	(void) strcpy(angtk_prompt_text, angtk_prompt_prefix);
#endif
}

void prompt_update(cptr str)
{
	char *attr = (char *) keyword_term_color[g_prompt_attr];

	angtk_eval("angband_prompt", "update", str, attr, NULL);
#ifdef BORG_TK
	(void) strcpy(angtk_prompt_text, angtk_prompt_prefix);
	(void) strcat(angtk_prompt_text, str);
#endif
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
	msg_print(prompt);
	message_flush();

	/* Restore quick_messages */
	quick_messages = old_quick;
}

#ifdef ALLOW_STATUS_EXTRA

typedef struct redraw_type redraw_type;

struct redraw_type
{
	int type; /* PR_XXX */
	bool redraw; /* TRUE if redraw pending */
	struct redraw_type *next; /* Linked list */
};

redraw_type g_redraw[PR_MAX];
redraw_type *g_redraw_head = NULL;
redraw_type *g_redraw_tail = NULL;

void redraw_add(int pr)
{
	redraw_type *rd_ptr = &g_redraw[pr];

	/* Already added */
	if (rd_ptr->redraw) return;

	/* Make it the first */
	if (g_redraw_head == NULL)
		g_redraw_head = rd_ptr;

	/* Make it the last */
	if (g_redraw_tail != NULL)
		g_redraw_tail->next = rd_ptr;

	/* Remember the last */
	g_redraw_tail = rd_ptr;

	/* Clear link to next */
	rd_ptr->next = NULL;

	/* It is added */
	rd_ptr->redraw = TRUE;
}

void redraw_flush(void)
{
	redraw_type *rd_ptr;

	/* Nothing to draw */
	if (g_redraw_head == NULL) return;

	for (rd_ptr = g_redraw_head; rd_ptr != NULL; rd_ptr = rd_ptr->next)
	{
		Bind_Status(KEYWORD_STATUS_EXTRA + rd_ptr->type + 1);
		rd_ptr->redraw = FALSE;
	}

	g_redraw_head = g_redraw_tail = NULL;
}

void redraw_init(void)
{
	int i;

	g_redraw_head = NULL;
	g_redraw_tail = NULL;

	for (i = 0; i < PR_MAX; i++)
	{
		g_redraw[i].type = i;
		g_redraw[i].redraw = FALSE;
		g_redraw[i].next = NULL;
	}
}

#endif /* ALLOW_STATUS_EXTRA */

#if TK_MINOR_VERSION == 2

typedef struct PhotoMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter associated with the
				 * application using this image. */
    Tcl_Command imageCmd;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    int	flags;			/* Sundry flags, defined below. */
    int	width, height;		/* Dimensions of image. */
    int userWidth, userHeight;	/* User-declared image dimensions. */
    Tk_Uid palette;		/* User-specified default palette for
				 * instances of this image. */
    double gamma;		/* Display gamma value to correct for. */
    char *fileString;		/* Name of file to read into image. */
    char *dataString;		/* String value to use as contents of image. */
    char *format;		/* User-specified format of data in image
				 * file or string value. */
    unsigned char *pix24;	/* Local storage for 24-bit image. */
    int ditherX, ditherY;	/* Location of first incorrectly
				 * dithered pixel in image. */
    TkRegion validRegion;	/* Tk region indicating which parts of
				 * the image have valid image data. */
    struct PhotoInstance *instancePtr;
				/* First in the list of instances
				 * associated with this master. */
} PhotoMaster;

/*
 * photocopy $imageSrc $imageDst $srcX $srcY $srcW $srcH
 * Like "$imageName copy args..." but considers transparency.
 */
int
objcmd_photo_copy(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tk_PhotoHandle photoH;
	Tk_PhotoImageBlock photoBlock;
	char *imageName;
	int y, x, width, height;
	PhotoMaster *masterPtr, *masterDstPtr;
	TkRegion validRegion;
	XRectangle rect;

	/* Get the name of the Tk photo image. It must already exist */
	imageName = Tcl_GetStringFromObj(objv[1], NULL);

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Failure */
		return TCL_ERROR;
	}

	(void) Tk_PhotoGetImage(photoH, &photoBlock);

	masterPtr = (PhotoMaster *) photoH;

	/* Get the name of the Tk photo image. It must already exist */
	imageName = Tcl_GetStringFromObj(objv[2], NULL);

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Failure */
		return TCL_ERROR;
	}

	masterDstPtr = (PhotoMaster *) photoH;

	if (Tcl_GetIntFromObj(interp, objv[3], &x) != TCL_OK ||
		Tcl_GetIntFromObj(interp, objv[4], &y) != TCL_OK ||
		Tcl_GetIntFromObj(interp, objv[5], &width) != TCL_OK ||
		Tcl_GetIntFromObj(interp, objv[6], &height) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if ((x < 0) || (x + width > photoBlock.width)
		|| (y < 0) || (y + height > photoBlock.height))
	{
		Tcl_AppendResult(interp, "coordinates out of range", (char *) NULL);
		return TCL_ERROR;
	}

	photoBlock.pixelPtr += x * photoBlock.pixelSize +
		y * photoBlock.pitch;
	photoBlock.width = width;
	photoBlock.height = height;
	Tk_PhotoPutBlock(photoH, &photoBlock, 0, 0, width, height);

	/* The big hack: Intersect the valid regions */
	rect.x = x;
	rect.y = y;
	rect.width = width;
	rect.height = height;
	validRegion = TkCreateRegion();
	TkUnionRectWithRegion(&rect, validRegion, validRegion);
	TkIntersectRegion(masterPtr->validRegion, validRegion, validRegion);
	OffsetRgn((HRGN) validRegion, -x, -y);
	TkIntersectRegion(validRegion, masterDstPtr->validRegion,
		masterDstPtr->validRegion);
	TkDestroyRegion(validRegion);

	return TCL_OK;
}

#endif /* Tk 8.2 */

#if TK_MINOR_VERSION >= 3

typedef struct PhotoMaster {
    Tk_ImageMaster tkMaster;	/* Tk's token for image master.  NULL means
				 * the image is being deleted. */
    Tcl_Interp *interp;		/* Interpreter associated with the
				 * application using this image. */
    Tcl_Command imageCmd;	/* Token for image command (used to delete
				 * it when the image goes away).  NULL means
				 * the image command has already been
				 * deleted. */
    int	flags;			/* Sundry flags, defined below. */
    int	width, height;		/* Dimensions of image. */
    int userWidth, userHeight;	/* User-declared image dimensions. */
    Tk_Uid palette;		/* User-specified default palette for
				 * instances of this image. */
    double gamma;		/* Display gamma value to correct for. */
    char *fileString;		/* Name of file to read into image. */
    Tcl_Obj *dataString;	/* Object to use as contents of image. */
    Tcl_Obj *format;		/* User-specified format of data in image
				 * file or string value. */
    unsigned char *pix24;	/* Local storage for 24-bit image. */
    int ditherX, ditherY;	/* Location of first incorrectly
				 * dithered pixel in image. */
    TkRegion validRegion;	/* Tk region indicating which parts of
				 * the image have valid image data. */
    struct PhotoInstance *instancePtr;
				/* First in the list of instances
				 * associated with this master. */
} PhotoMaster;

#endif /* Tk 8.3 */

#if TK_MINOR_VERSION >= 2

/*
 * photoget $imageName $x $y
 * Like "$imageName get $x $y" but returns empty list on transparency.
 */
int
objcmd_photo_get(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	Tk_PhotoHandle photoH;
	char *imageName;
	int y, x;
	PhotoMaster *masterPtr;
	unsigned char *pixelPtr;

	/* Required number of arguments */
	if (objc != 4)
	{
		Tcl_WrongNumArgs(interp, 1, objv, "imageName x y");
		return TCL_ERROR;
	}

	/* Get the name of the Tk photo image. It must already exist */
	imageName = Tcl_GetStringFromObj(objv[1], NULL);

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Failure */
		return TCL_ERROR;
	}

	masterPtr = (PhotoMaster *) photoH;

	if (Tcl_GetIntFromObj(interp, objv[2], &x) != TCL_OK ||
		Tcl_GetIntFromObj(interp, objv[3], &y) != TCL_OK)
		return TCL_ERROR;

	if ((x < 0) || (x >= masterPtr->width)
		|| (y < 0) || (y >= masterPtr->height))
	{
		Tcl_AppendResult(interp, "coordinates out of range", (char *) NULL);
		return TCL_ERROR;
	}

	if (TkRectInRegion(masterPtr->validRegion, x, y, 1, 1))
	{
		pixelPtr = masterPtr->pix24 + (y * masterPtr->width + x) * 3;
		Tcl_SetResult(interp, format("%d %d %d",
			pixelPtr[0], pixelPtr[1], pixelPtr[2]), TCL_VOLATILE);
	}

	return TCL_OK;
}

/*
 * photomask $image ?$imageMask?
 * This is the ugliest hack you (n)ever wanted to see. Tk takes a good
 * long time to load in a masked image, due to the amount of time it
 * takes to create the validRegion (ie, mask), through calls to the
 * TkUnionRectWithRegion() function. This command will quickly create
 * the validRegion by building the region by hand, using ExtCreateRegion().
 * All the white pixels (RGB=255,255,255) are left out of the mask.
 */
int
objcmd_photo_mask(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
#ifdef PLATFORM_WIN

	Tk_PhotoHandle photoH;
	Tk_PhotoImageBlock photoBlock, photoBlockDst, *photoBlockPtr;
	char *imageName;
	int y, x, end, width, height;
	unsigned char *rowPtr, *pixelPtr, *rowDstPtr, *pixelDstPtr;
	PhotoMaster *masterPtr;
	DWORD dataSize = 0;
	HRGN validRegion;
	int loop, rectCount = 0;
	RGNDATA *rgnData = NULL;
	RECT rcBound = {5000, 5000, 0, 0}, *rectPtr = NULL;

	/* Get the name of the Tk photo image. It must already exist */
	imageName = Tcl_GetStringFromObj(objv[1], NULL);

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Failure */
		return TCL_ERROR;
	}

	(void) Tk_PhotoGetImage(photoH, &photoBlock);
	masterPtr = (PhotoMaster *) photoH;
	photoBlockPtr = &photoBlock;

	if (objc == 3)
	{
		/* Get the name of the Tk photo image. It must already exist */
		imageName = Tcl_GetStringFromObj(objv[2], NULL);

		/* Lookup the photo by name */
		photoH = Tk_FindPhoto(interp, imageName);

		/* The photo was not found */
		if (photoH == NULL)
		{
			/* Failure */
			return TCL_ERROR;
		}

		(void) Tk_PhotoGetImage(photoH, &photoBlockDst);
		photoBlockPtr = &photoBlockDst;
	}

	height = photoBlockPtr->height;
	width = photoBlockPtr->width;

	/* Loop 0: count rectangles */
	/* Loop 1: create region */
	for (loop = 0; loop <= 1; loop++)
	{
		/* Adapted from tkImgGIF.c */
		rowPtr = photoBlockPtr->pixelPtr;
		rowDstPtr = photoBlock.pixelPtr;
		for (y = 0; y < height; y++)
		{
			x = 0;
			pixelPtr = rowPtr;
			pixelDstPtr = rowDstPtr;
			while (x < width)
			{
				/* Scan past transparent pixels */
				while ((x < width) && ((pixelPtr[0] + pixelPtr[1] + pixelPtr[2]) == 255 * 3))
				{
					x++;
#if TK_MINOR_VERSION >= 3
					/* Write to the alpha channel */
					if (loop)
					{
						pixelDstPtr[0] = pixelDstPtr[1] = pixelDstPtr[2] = 0xd9;
						pixelDstPtr[3] = 0;
					}
#endif
					pixelPtr += photoBlockPtr->pixelSize;
					pixelDstPtr += 4;
				}
				end = x;

				/* Scan past non-transparent pixels */
				while ((end < width) && ((pixelPtr[0] + pixelPtr[1] + pixelPtr[2]) != 255 * 3))
				{
					end++;
					pixelPtr += photoBlockPtr->pixelSize;
					pixelDstPtr += 4;
				}
				if (end > x)
				{
					if (loop)
					{
						rectPtr->left = x;
						rectPtr->top = y;
						rectPtr->right = end;
						rectPtr->bottom = y + 1;
						rectPtr++;

						if (x < rcBound.left)
							rcBound.left = x;
						if (end > rcBound.right)
							rcBound.right = end;
						if (y < rcBound.top)
							rcBound.top = y;
						if (y + 1 > rcBound.bottom)
							rcBound.bottom = y + 1;
					}
					else
						rectCount++;
				}
				x = end;
			}
			rowPtr += photoBlockPtr->pitch;
			rowDstPtr += photoBlock.pitch;
		}

		if (loop == 1) break;

		/* 2413 rects for townactions.gif */
		/* 2413 * sizeof(RECT) = 38608 bytes */
		dbwin("rectCount is %d\n", rectCount);

		dataSize = sizeof(RGNDATA) + rectCount * sizeof(RECT);
		rgnData = (void *) HeapAlloc(GetProcessHeap(), 0, dataSize);
		rgnData->rdh.dwSize = sizeof(RGNDATAHEADER);
		rgnData->rdh.iType = RDH_RECTANGLES;
		rgnData->rdh.nCount = rectCount;
		rgnData->rdh.nRgnSize = 0;
		rgnData->rdh.rcBound = rcBound;
		rectPtr = (RECT *) rgnData->Buffer;
	}

	if ((validRegion = ExtCreateRegion(NULL, dataSize, rgnData)) != NULL)
	{
		TkDestroyRegion(masterPtr->validRegion);
		masterPtr->validRegion = (TkRegion) validRegion;
	}

	HeapFree(GetProcessHeap(), 0, rgnData);

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

	Tk_PhotoHandle photoH;
	Tk_PhotoImageBlock photoBlock, photoBlockDst, *photoBlockPtr;
	char *imageName;
	int y, x, end, width, height;
	unsigned char *rowPtr, *pixelPtr, *rowDstPtr, *pixelDstPtr;
	PhotoMaster *masterPtr;
	XRectangle rect;
	TkRegion validRegion;

	/* Get the name of the Tk photo image. It must already exist */
	imageName = Tcl_GetStringFromObj(objv[1], NULL);

	/* Lookup the photo by name */
	photoH = Tk_FindPhoto(interp, imageName);

	/* The photo was not found */
	if (photoH == NULL)
	{
		/* Failure */
		return TCL_ERROR;
	}

	(void) Tk_PhotoGetImage(photoH, &photoBlock);
	masterPtr = (PhotoMaster *) photoH;
	photoBlockPtr = &photoBlock;

	if (objc == 3)
	{
		/* Get the name of the Tk photo image. It must already exist */
		imageName = Tcl_GetStringFromObj(objv[2], NULL);

		/* Lookup the photo by name */
		photoH = Tk_FindPhoto(interp, imageName);

		/* The photo was not found */
		if (photoH == NULL)
		{
			/* Failure */
			return TCL_ERROR;
		}

		(void) Tk_PhotoGetImage(photoH, &photoBlockDst);
		photoBlockPtr = &photoBlockDst;
	}

	validRegion = TkCreateRegion();

	height = photoBlockPtr->height;
	width = photoBlockPtr->width;

	/* Adapted from tkImgGIF.c */
	rowPtr = photoBlockPtr->pixelPtr;
	rowDstPtr = photoBlock.pixelPtr;
	for (y = 0; y < height; y++)
	{
		x = 0;
		pixelPtr = rowPtr;
		pixelDstPtr = rowDstPtr;
		while (x < width)
		{
			/* Scan past transparent pixels */
			while ((x < width) && ((pixelPtr[0] + pixelPtr[1] + pixelPtr[2]) == 255 * 3))
			{
				x++;
#if TK_MINOR_VERSION >= 3
				/* Write to the alpha channel */
				pixelDstPtr[0] = pixelDstPtr[1] = pixelDstPtr[2] = 0xd9;
				pixelDstPtr[3] = 0;
#endif /* Tk >= 8.3 */
				pixelPtr += photoBlockPtr->pixelSize;
				pixelDstPtr += 4;
			}
			end = x;

			/* Scan past non-transparent pixels */
			while ((end < width) && ((pixelPtr[0] + pixelPtr[1] + pixelPtr[2]) != 255 * 3))
			{
				end++;
				pixelPtr += photoBlockPtr->pixelSize;
				pixelDstPtr += 4;
			}
			if (end > x)
			{
				rect.x = x;
				rect.y = y;
				rect.width = end - x;
				rect.height = 1;
				TkUnionRectWithRegion(&rect, validRegion, validRegion);
			}
			x = end;
		}
		rowPtr += photoBlockPtr->pitch;
		rowDstPtr += photoBlock.pitch;
	}

	TkDestroyRegion(masterPtr->validRegion);
	masterPtr->validRegion = validRegion;

#endif /* PLATFORM_X11 */

	return TCL_OK;
}

#endif /* Tk 8.2 or 8.3 */

int ExtToUtf_SetArrayValueString(char *varName, char *field, char *value)
{
	Tcl_DString utfDString;
	char *utfString;

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

/*
 * If a spell/wand/rod/etc calls fire_bolt() followed by fire_cloud()
 * and the targetted monster is killed by the first fire_bolt(), then
 * the target is cleared and the second fire_cloud() will start at
 * the character's location. For example:
 *
 *     fire_bolt(GF_POIS, dir, damroll(plev / 2, 11));
 *     fire_cloud(GF_POIS, dir, 30, 6);
 *
 * The solution is to remember the target, and if the monster is
 * killed by the fire_bolt() then the target is set to the location
 * the monster was at. The macros to do this are:
 *
 * aim_dir_preserve -- Remember the current target monster's location
 * aim_dir_maintain -- Set the target to the saved location if needed
 * aim_dir_finish   -- Clear the target location if needed
 *
 * The above statements would now be written:
 *
 *     get_aim_dir(&dir);
 *     aim_dir_preserve(dir); <-----
 *     fire_bolt(GF_POIS, dir, damroll(plev / 2, 11));
 *     aim_dir_maintain();    <-----
 *     fire_cloud(GF_POIS, dir, 30, 6);
 *     aim_dir_finish();      <-----
 */

static int save_target_y, save_target_x;
static int save_target_set;

void aim_dir_preserve(int dir)
{
	if ((dir == 5) && target_okay() && (p_ptr_target_who > 0))
	{
		save_target_y = p_ptr_target_row;
		save_target_x = p_ptr_target_col;
		save_target_set = 1;
	}
	else
		save_target_set = 0;
}

void aim_dir_maintain(void)
{
	if ((save_target_set == 1) && (p_ptr_target_who <= 0))
	{
		target_set_location(save_target_y, save_target_x);
		save_target_set = 2;
	}
}

void aim_dir_finish(void)
{
	if (save_target_set == 2)
		target_set_monster(0);
}

