#define HAVE_DBWIN_H
#include "tkTreeCtrl.h"
#include "tkTreeElem.h"
#undef WIPE
#undef FormatResult

#ifdef PLATFORM_SDL
#define WIN32
#endif

#ifdef PLATFORM_WIN
#include <windows.h>
#include <tkWinInt.h>
#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11
#include <sys/param.h> /* Warning about NBBY being redefined */
#include <tkInt.h>
#endif /* PLATFORM_X11 */

#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "util-dll.h"
#include "icon.h"

TreeCtrlStubs *stubs;
#define TreeCtrl_RegisterElementType(i,t) \
	stubs->TreeCtrl_RegisterElementType(i,t)
#define Tree_RedrawElement(t,i,e) \
	stubs->Tree_RedrawElement(t,i,e)
#define Tree_ElementIterateBegin(t,et) \
	stubs->Tree_ElementIterateBegin(t,et)
#define Tree_ElementIterateNext(i) \
	stubs->Tree_ElementIterateNext(i)
#define Tree_ElementIterateGet(i) \
	stubs->Tree_ElementIterateGet(i)
#define Tree_ElementIterateChanged(i,m) \
	stubs->Tree_ElementIterateChanged(i,m)

static BitmapType s_bitmap = { NULL };
static int s_256;

/*
 * Linked list of all "assign"-type elements in all TreeCtrl widgets which
 * display animated icons.
 */
static DoubleLinker s_anim_linker;

/*
 * Keep a list of all TreeCtrl displaying ElementAssign so I can clear the
 * t_assign when "angband init_icons" is called.
 */
typedef struct ElementTree {
	TreeCtrl *tree;
	int count; /* Number of ElementAssign */
} ElementTree;
static ElementTree *ElementTreeList = NULL;
static int ElementTreeCount = 0;

typedef struct ElementAssign ElementAssign;
struct ElementAssign
{
	TreeElement_ header;
	t_assign fg;
	Tcl_Obj *fgObj;
	t_assign bg;
	Tcl_Obj *bgObj;
	TreeCtrl *tree; /* So we can redraw sprite etc */
	TreeItem item; /* So we can redraw sprite etc */
	TreeItemColumn column; /* So we can redraw sprite etc */
	DoubleLink link; /* So we can redraw sprite etc */
};

#define ASSIGN_CONF_FG		0x0001
#define ASSIGN_CONF_BG		0x0002

static Tk_OptionSpec assignOptionSpecs[] = {
	{TK_OPTION_STRING, "-fg", (char *) NULL, (char *) NULL,
		"icon none 0", Tk_Offset(ElementAssign, fgObj), -1,
		0, (ClientData) NULL, ASSIGN_CONF_FG},
	{TK_OPTION_STRING, "-bg", (char *) NULL, (char *) NULL,
		"icon none 0", Tk_Offset(ElementAssign, bgObj), -1,
		0, (ClientData) NULL, ASSIGN_CONF_BG},
	{TK_OPTION_END, (char *) NULL, (char *) NULL, (char *) NULL,
		(char *) NULL, 0, -1, 0, (ClientData) NULL, 0}
};

static void DeleteProcAssign(TreeElementArgs *args)
{
	ElementAssign *elemX = (ElementAssign *) args->elem;
	int i;

	DoubleLink_Unlink(&elemX->link);

	for (i = 0; i < ElementTreeCount; i++)
	{
		if (ElementTreeList[i].tree == args->tree)
		{
			ElementTreeList[i].count--;
			if (ElementTreeList[i].count == 0)
				ElementTreeList = Array_Delete(ElementTreeList,
					&ElementTreeCount, sizeof(ElementTree), i);
			return;
		}
	}
	panic("can't find tree %s %d", __FILE__, __LINE__);
}

static int WorldChangedAssign(TreeElementArgs *args)
{
	int flagM = args->change.flagMaster;
	int flagS = args->change.flagSelf;
	int mask = 0;

	if ((flagS | flagM) & (ASSIGN_CONF_FG | ASSIGN_CONF_BG))
		mask |= CS_DISPLAY | CS_LAYOUT;

	return mask;
}

static int ConfigProcAssign(TreeElementArgs *args)
{
	TreeCtrl *tree = args->tree;
	TreeElement elem = args->elem;
	ElementAssign *elemX = (ElementAssign *) elem;
	ElementAssign savedX;
	Tk_SavedOptions savedOptions;
	int error;
	Tcl_Obj *errorResult = NULL;

	for (error = 0; error <= 1; error++)
	{
		if (error == 0)
		{
			if (Tk_SetOptions(tree->interp, (char *) elemX,
				elem->typePtr->optionTable,
				args->config.objc, args->config.objv, tree->tkwin,
				&savedOptions, &args->config.flagSelf) != TCL_OK)
			{
				args->config.flagSelf = 0;
				continue;
			}

			if (args->config.flagSelf & ASSIGN_CONF_FG)
				savedX.fg = elemX->fg;
			if (args->config.flagSelf & ASSIGN_CONF_BG)
				savedX.bg = elemX->bg;

			if (args->config.flagSelf & ASSIGN_CONF_FG)
			{
				char *t = Tcl_GetString(elemX->fgObj);
				if (assign_parse(tree->interp, &elemX->fg, t) != TCL_OK)
					continue;
			}
			if (args->config.flagSelf & ASSIGN_CONF_BG)
			{
				char *t = Tcl_GetString(elemX->bgObj);
				if (assign_parse(tree->interp, &elemX->bg, t) != TCL_OK)
					continue;
			}

			Tk_FreeSavedOptions(&savedOptions);
			break;
		}
		else
		{
			errorResult = Tcl_GetObjResult(tree->interp);
			Tcl_IncrRefCount(errorResult);
			Tk_RestoreSavedOptions(&savedOptions);

			if (args->config.flagSelf & ASSIGN_CONF_FG)
				elemX->fg = savedX.fg;
			if (args->config.flagSelf & ASSIGN_CONF_BG)
				elemX->bg = savedX.bg;

			Tcl_SetObjResult(tree->interp, errorResult);
			Tcl_DecrRefCount(errorResult);
			return TCL_ERROR;
		}
	}

	/* Keep a linked list of ElementAssign's displaying sprites */
	if (elemX->link.isLinked)
	{
		if (!is_sprite(&elemX->fg) && !is_sprite(&elemX->bg))
			DoubleLink_Unlink(&elemX->link);
	}
	else
	{
		if (is_sprite(&elemX->fg) || is_sprite(&elemX->bg))
			DoubleLink_Link(&elemX->link);
	}

	return TCL_OK;
}

static int CreateProcAssign(TreeElementArgs *args)
{
	int i;

#if 1 /* handled by initial "configure"? */
	ElementAssign *elemX = (ElementAssign *) args->elem;

	elemX->fg.assignType = ASSIGN_TYPE_ICON;
	elemX->fg.icon.type = ICON_TYPE_NONE;
	elemX->fg.icon.index = 0;
	elemX->fg.icon.ascii = -1;

	elemX->bg = elemX->fg;
#endif

	elemX->tree = args->tree;
	elemX->item = args->create.item;
	elemX->column = args->create.column;
	DoubleLink_Init(&s_anim_linker, &elemX->link, elemX);

	/* Keep a list of TreeCtrl displaying ElementAssign */
	for (i = 0; i < ElementTreeCount; i++)
	{
		if (ElementTreeList[i].tree == args->tree)
		{
			ElementTreeList[i].count++;
			break;
		}
	}
	if (i == ElementTreeCount)
	{
		ElementTree eTree;

		eTree.tree = args->tree;
		eTree.count = 1;
		ElementTreeList = Array_Append(ElementTreeList, &ElementTreeCount,
			sizeof(ElementTree), &eTree);
	}

	return TCL_OK;
}

/*
 * Draw one icon into our bitmap.
 */
static void DrawIconSpec(IconSpec *iconSpecPtr)
{
	IconPtr srcPtr, dstPtr;
	int y;
	t_icon_type *iconTypePtr;

	iconTypePtr = &g_icon_type[iconSpecPtr->type];

	dstPtr = s_bitmap.pixelPtr;

	/*
	 * If this is an ascii-type icon, then we must call a routine to
	 * give us the colorized icon data.
	 */
	if (iconSpecPtr->ascii != -1)
	{
		IconData iconData;
		srcPtr = Icon_GetAsciiData(iconSpecPtr, iconData);
		for (y = 0; y < iconTypePtr->height; y++)
		{
			memcpy(dstPtr, srcPtr, iconTypePtr->pitch);
			srcPtr += iconTypePtr->pitch;
			dstPtr += s_bitmap.pitch;
		}
	}

	/* Not an ascii-type icon */
	else
	{
		srcPtr = iconTypePtr->icon_data + iconSpecPtr->index * iconTypePtr->length;

		/* Transparent */
		if (iconTypePtr->rle_data)
		{
			int col = 0;
			IconPtr rlebuf;
			unsigned char *bounds = iconTypePtr->rle_bounds +
				iconSpecPtr->index * 4;
			int bypp = iconTypePtr->bypp;
			IconPtr dst = dstPtr + bounds[0] * bypp +
				bounds[1] * s_bitmap.pitch;
			int w = bounds[2], h = bounds[3];

			if (iconTypePtr->dynamic)
			{
				IconValue empty[2] = { 0, 0 };
				rlebuf = ((IconPtr *) iconTypePtr->rle_data)[iconSpecPtr->index];
				if (!rlebuf)
					rlebuf = empty;
			}
			else
			{
				rlebuf = iconTypePtr->rle_data +
					iconTypePtr->rle_offset[iconSpecPtr->index];
			}

			while (1)
			{
				unsigned int trans, opaq;

				trans = rlebuf[0];
				opaq = rlebuf[1];
				rlebuf += 2;

				col += trans;

				if (opaq)
				{
					memcpy(dst + col * bypp, rlebuf, opaq * bypp);
					rlebuf += opaq * bypp;
					col += opaq;
				}
				else if (!col)
					break;

				if (col == w)
				{
					if (!--h)
						break;
					col = 0;
					dst += s_bitmap.pitch;
				}
			}
		}

		/* Not transparent */
		else
		{
			for (y = 0; y < iconTypePtr->height; y++)
			{
				memcpy(dstPtr, srcPtr, iconTypePtr->pitch);
				srcPtr += iconTypePtr->pitch;
				dstPtr += s_bitmap.pitch;
			}
		}
	}
}

static void DisplayProcAssign(TreeElementArgs *args)
{
#ifdef PLATFORM_WIN

	TreeCtrl *tree = args->tree;
	TreeElement elem = args->elem;
	ElementAssign *elemX = (ElementAssign *) elem;
	IconSpec iconSpecFG, iconSpecBG;
	HDC dc, dc2;
	TkWinDCState state, state2;

	FinalIcon(&iconSpecFG, &elemX->fg, 0, NULL, NULL);
	FinalIcon(&iconSpecBG, &elemX->bg, 0, NULL, NULL);

	if ((iconSpecFG.type != ICON_TYPE_NONE) || (iconSpecBG.type != ICON_TYPE_NONE))
	{
		/* This code is like TkPutImage() */
		dc = TkWinGetDrawableDC(tree->display, args->display.drawable, &state);
		dc2 = TkWinGetDrawableDC(tree->display, s_bitmap.pixmap, &state2);

		/* Transparent */
		if (g_icon_type[iconSpecFG.type].rle_data ||
			g_icon_type[iconSpecBG.type].rle_data)
		{
			/*
			 * Ignorance alert! I want to copy the background from where
			 * the masked icon should be drawn to the s_bitmap
			 * and draw the masked icon on top of that. So I figure I
			 * will BitBlt() from the treectrl HDC to s_bitmap HDC.
			 * But on 256-color monitors it is way too slow. So I find
			 * what color is under the canvas item and use that.
			 */
			if (!s_256)
			{
				BitBlt(dc2, 0, 0,
					g_icon_type[iconSpecFG.type].width,
					g_icon_type[iconSpecFG.type].height,
					dc,
					args->display.x,
					args->display.y,
					SRCCOPY);
			}
			else
			{
				TkWinFillRect(dc2, 0, 0,
					g_icon_type[iconSpecFG.type].width,
					g_icon_type[iconSpecFG.type].height,
					GetPixel(dc, args->display.x, args->display.y));
			}
		}

		/* Draw background icon */
		if (iconSpecBG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecBG);
		}

		/* Draw foreground icon */
		if (iconSpecFG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecFG);
		}

		BitBlt(dc, args->display.x, args->display.y,
			g_icon_type[iconSpecFG.type].width,
			g_icon_type[iconSpecFG.type].height, dc2, 0, 0, SRCCOPY);

		TkWinReleaseDrawableDC(s_bitmap.pixmap, dc2, &state2);
		TkWinReleaseDrawableDC(args->display.drawable, dc, &state);
	}

#endif /* PLATFORM_WIN */

#ifdef PLATFORM_X11

	TreeCtrl *tree = args->tree;
	TreeElement elem = args->elem;
	ElementAssign *elemX = (ElementAssign *) elem;
	IconSpec iconSpecFG, iconSpecBG;

	FinalIcon(&iconSpecFG, &elemX->fg, 0, NULL, NULL);
	FinalIcon(&iconSpecBG, &elemX->bg, 0, NULL, NULL);

	if ((iconSpecFG.type != ICON_TYPE_NONE) || (iconSpecBG.type != ICON_TYPE_NONE))
	{
		/* Transparent */
		if (g_icon_type[iconSpecFG.type].rle_data ||
			g_icon_type[iconSpecBG.type].rle_data)
		{
			XCopyArea(tree->display,
				args->display.drawable, /* source drawable */
				s_bitmap.pixmap, /* dest drawable */
				tree->copyGC, /* graphics context */
				args->display.x, args->display.y, /* source top-left */
				g_icon_type[iconSpecFG.type].width, /* width */
				g_icon_type[iconSpecFG.type].height, /* height */
				0, 0 /* dest top-left */
			);

			Plat_SyncDisplay(tree->display);
		}

		/* Draw background icon */
		if (iconSpecBG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecBG);
		}

		/* Draw foreground icon */
		if (iconSpecFG.type != ICON_TYPE_NONE)
		{
			DrawIconSpec(&iconSpecFG);
		}

		XCopyArea(tree->display,
			s_bitmap.pixmap, /* source drawable */
			args->display.drawable, /* dest drawable */
			tree->copyGC, /* graphics context */
			0, 0, /* source top-left */
			g_icon_type[iconSpecFG.type].width, /* width */
			g_icon_type[iconSpecFG.type].height, /* height */
			args->display.x, /* dest top-left */
			args->display.y
		);
	}

#endif /* PLATFORM_X11 */
}

static void NeededProcAssign(TreeElementArgs *args)
{
/*	TreeCtrl *tree = args->tree; */
	TreeElement elem = args->elem;
	ElementAssign *elemX = (ElementAssign *) elem;
	IconSpec iconSpec;

	FinalIcon(&iconSpec, &elemX->fg, 0, NULL, NULL);
	args->needed.width = g_icon_type[iconSpec.type].width;
	args->needed.height = g_icon_type[iconSpec.type].height;
}

static int StateProcAssign(TreeElementArgs *args)
{
	int mask = 0;

	return mask;
}

static int UndefProcAssign(TreeElementArgs *args)
{
	return 0;
}

static int ActualProcAssign(TreeElementArgs *args)
{
	return TCL_OK;
}

static void OnScreenProcAssign(TreeElementArgs *args)
{
/*	TreeCtrl *tree = args->tree;
	TreeElement elem = args->elem;
	ElementAssign *elemX = (ElementAssign *) elem;

	if (args->screen.visible)
	{
		DoubleLink_Link(&elemX->link);
	}
	else
	{
		DoubleLink_Unlink(&elemX->link);
	} */
}

TreeElementType elemTypeAssign = {
	"assign",
	sizeof(ElementAssign),
	assignOptionSpecs,
	NULL,
	CreateProcAssign,
	DeleteProcAssign,
	ConfigProcAssign,
	DisplayProcAssign,
	NeededProcAssign,
	NULL, /* heightProc */
	WorldChangedAssign,
	StateProcAssign,
	UndefProcAssign,
	ActualProcAssign,
	OnScreenProcAssign
};

void TreeCtrl_Idle(void)
{
	DoubleLink *link;
	ElementAssign *elemX;

	for (link = s_anim_linker.head; link; link = link->next)
	{
		elemX = DoubleLink_Data(link, ElementAssign);
		Tree_RedrawElement(elemX->tree, elemX->item, (TreeElement) elemX);
	}
}

void TreeCtrl_Setup(void)
{
	int i;
	t_assign none;

	s_bitmap.height = 100;
	s_bitmap.width = 100;
	s_bitmap.depth = g_icon_depth;
	Bitmap_New(g_interp, &s_bitmap);

	/*
	 * If the game's icon configuration changed, we must go through all the
	 * TreeCtrl widgets displaying ElementAssign elements and set the
	 * assignments to something safe since all the icon types have now been
	 * deleted.
	 */
	none.assignType = ASSIGN_TYPE_ICON;
	none.icon.type = ICON_TYPE_NONE;
	none.icon.index = 0;
	none.icon.ascii = -1;

	for (i = 0; i < ElementTreeCount; i++)
	{
		TreeCtrl *tree = ElementTreeList[i].tree;
		TreeIterate iter;

		iter = Tree_ElementIterateBegin(tree, &elemTypeAssign);
		while (iter != NULL)
		{
			ElementAssign *elemX = (ElementAssign *) Tree_ElementIterateGet(iter);

			elemX->fg = none;
			elemX->bg = none;
			/* FIXME: update fgObj and bgObj */
			Tree_ElementIterateChanged(iter, CS_DISPLAY | CS_LAYOUT);
			iter = Tree_ElementIterateNext(iter);
		}
	}
}

void TreeCtrl_Unload(void)
{
	if (s_bitmap.pixelPtr)
	{
		Bitmap_Delete(&s_bitmap);
		s_bitmap.pixelPtr = NULL;
	}
}

int TreeCtrl_Init(void)
{
	if (Tcl_PkgRequire(g_interp, "treectrl", "2.0", 0) == NULL)
		return TCL_ERROR;

	(void) Tcl_EvalEx(g_interp, "catch {treectrl}", -1, TCL_EVAL_GLOBAL); /* load it */

	stubs = Tcl_GetAssocData(g_interp, "TreeCtrlStubs", NULL);
	if (stubs == NULL)
		return TCL_ERROR;

	if (TreeCtrl_RegisterElementType(g_interp, &elemTypeAssign) != TCL_OK)
		return TCL_ERROR;

#ifdef PLATFORM_WIN
	{
		HDC monDC = GetDC(NULL);
		s_256 = (GetDeviceCaps(monDC, BITSPIXEL) == 8);
		ReleaseDC(NULL, monDC);
	}
#endif /* PLATFORM_WIN */

	DoubleLink_Init(&s_anim_linker, NULL, NULL);

	return TCL_OK;
}

void TreeCtrl_Exit(void)
{
	TreeCtrl_Unload();

#if 0 /* Should be freed when all TreeCtrl widgets deleted */
	if (ElementTreeList)
		Tcl_FreeDebug(ElementTreeList);
#endif
}

