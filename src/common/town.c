/* File: town.c */

/* Purpose: special town layout */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "util-dll.h"
#include "icon.h"
#include "widget.h"

/* Save only thos icon types used by a vault */
#define SAVE_MIN_TYPES

/*
 * 1.0.0 - Original
 * 1.1.0 - Add background icon array
 * 1.2.0 - Now 4 icon arrays
 */
#define VLT_VERSION_MAJOR 1
#define VLT_VERSION_MINOR 2
#define VLT_VERSION_PATCH 0
#define VLT_VERSION_EXTRA 0

enum {
VLT_PLANE_FEAT,
VLT_PLANE_ICON_1,
VLT_PLANE_ICON_2,
VLT_PLANE_ICON_3,
VLT_PLANE_ICON_4,
VLT_PLANE_SYMBOL,
VLT_PLANE_LETTER,
VLT_PLANE_MAX
};

static CONST char *keyword_plane[] = {"feature", "icon1", "icon2", "icon3", "icon4",
	"symbol", "letter", NULL};

static int s_plane_size[VLT_PLANE_MAX] = {
	sizeof(byte), sizeof(IconSpec), sizeof(IconSpec), sizeof(IconSpec),
	sizeof(IconSpec), sizeof(byte), sizeof(char)
};

typedef struct t_vault t_vault;
struct t_vault
{
	int id;
	int y, x;
	int height;
	int width;
	int dataHeight;
	int dataWidth;
	byte *data[VLT_PLANE_MAX];
	DoubleLinker *linker[VLT_PLANE_MAX];
	DoubleLink link[VLT_PLANE_MAX];
};

/* List of vaults */
t_vault **g_vault = NULL;

/* Number of vaults */
int g_vault_count = 0;

/* Next unique vault id */
int g_vault_id = 0;

/* Vault for the current level */
t_vault *g_vault_current = NULL;

static int ValidateVault(Tcl_Interp *interp, int vaultId, int *vaultIndex)
{
	int i;

	for (i = 0; i < g_vault_count; i++)
	{
		if (g_vault[i]->id == vaultId)
		{
			(*vaultIndex) = i;
			return TCL_OK;
		}
	}

	FormatResult(interp, "no such vault \"%d\"", vaultId);
	return TCL_ERROR;
}

static int ValidateCoord(Tcl_Interp *interp, int vaultIndex, int y, int x)
{
	t_vault *vaultPtr = g_vault[vaultIndex];

	if ((y < vaultPtr->y) || (y >= vaultPtr->y + vaultPtr->height) ||
		(x < vaultPtr->x) || (x >= vaultPtr->x + vaultPtr->width))
	{
		FormatResult(interp,
			"location \"%d %d\" not in bounds: must be in %d %d %d %d",
			y, x, vaultPtr->y, vaultPtr->x, vaultPtr->y + vaultPtr->height - 1,
			vaultPtr->x + vaultPtr->width - 1);
		return TCL_ERROR;
	}
	return TCL_OK;
}

static int GetVaultFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr,
	int *vaultId, int *vaultIndex, int allowZero)
{
	if (Tcl_GetIntFromObj(interp, objPtr, vaultId) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (!(*vaultId) && allowZero)
	{
		*vaultIndex = -1;
		return TCL_OK;
	}
	if (ValidateVault(interp, *vaultId, vaultIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}
	return TCL_OK;
}

static void VaultChanged(Tcl_Interp *interp, int vaultIndex)
{
	t_vault *vaultPtr = g_vault[vaultIndex];
	DoubleLink *link;
	Widget *widgetPtr;

	for (link = WidgetList.head; link; link = link->next)
	{
		widgetPtr = DoubleLink_Data(link, Widget);

		/* The widget is displaying this vault */
		if (((ExWidget *) widgetPtr)->vaultNum == vaultPtr->id)
		{
			/* Set the vaultPtr, because of the "swap" command */
			Widget_SetVault(widgetPtr);

			Widget_Wipe(widgetPtr);
		}

		/* The vault is the "current" vault, and the widget is displaying
		 * the cave (not a vault) */
		if ((vaultPtr == g_vault_current) &&
			(((ExWidget *) widgetPtr)->vaultNum == 0))
		{
			Widget_Wipe(widgetPtr);
		}
	}

	if (vaultPtr == g_vault_current)
	{
/*		g_icon_map_changed = TRUE; */
		angtk_cave_changed(); /* Too slow? Oct 24 2004 */
	}
}

static void VaultDestroyed(Tcl_Interp *interp, int vaultIndex)
{
	t_vault *vaultPtr = g_vault[vaultIndex];
	DoubleLink *link;
	Widget *widgetPtr;

	for (link = WidgetList.head; link; link = link->next)
	{
		widgetPtr = DoubleLink_Data(link, Widget);
		if (((ExWidget *) widgetPtr)->vaultNum == vaultPtr->id)
		{
			((ExWidget *) widgetPtr)->vaultNum = 0;
			Widget_Wipe(widgetPtr);
		}
	}

	if (vaultPtr == g_vault_current)
	{
		g_vault_current = NULL;
	}
}

static void VaultFreePlane(int vaultIndex, int plane)
{
	t_vault *vaultPtr = g_vault[vaultIndex];
	DoubleLinker *linker = vaultPtr->linker[plane];

	/* This plane is shared */
	if (linker)
	{
		DoubleLink_Unlink(&vaultPtr->link[plane]);
		vaultPtr->linker[plane] = NULL;
		if (linker->count == 1)
		{
			vaultPtr = DoubleLink_Data(linker->head, t_vault);
			DoubleLink_Unlink(&vaultPtr->link[plane]);
			Tcl_FreeDebug((char *) linker);
			vaultPtr->linker[plane] = NULL;
		}
	}

	/*
	 * The plane is not shared. Free the memory. We test for existence
	 * to handle incomplete "create".
	 */
	else
	{
		if (vaultPtr->data[plane])
		{
			Tcl_FreeDebug((char *) vaultPtr->data[plane]);
			vaultPtr->data[plane] = NULL;
		}
	}
}

static void VaultFree(int vaultIndex)
{
	t_vault *vaultPtr = g_vault[vaultIndex];
	int plane;

	VaultDestroyed(g_interp, vaultIndex);
	for (plane = 0; plane < VLT_PLANE_MAX; plane++)
		VaultFreePlane(vaultIndex, plane);
	Tcl_FreeDebug((char *) vaultPtr);
	g_vault = Array_Delete(g_vault, &g_vault_count,
		sizeof(t_vault *), vaultIndex);
}

char *IconToString(char *buf, IconSpec *iconSpecPtr)
{
	if (iconSpecPtr->ascii == -1)
	{
		(void) sprintf(buf, "%s %d",
			g_icon_type[iconSpecPtr->type].desc,
			iconSpecPtr->index);
	}
	else
	{
		(void) sprintf(buf,"%s %d %d",
			g_icon_type[iconSpecPtr->type].desc,
			iconSpecPtr->index, iconSpecPtr->ascii);
	}

	return buf;
}

static int PlaneDataFromObj(Tcl_Interp *interp, void *dataPtr,
	Tcl_Obj *objPtr, int plane)
{
	if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
	{
		char *t = Tcl_GetStringFromObj(objPtr, NULL);
		char buf[128];
		t_assign assign;
		IconSpec *specPtr = (IconSpec *) dataPtr;
		(void) sprintf(buf, "icon %s", t);
		if (assign_parse(interp, &assign, buf) != TCL_OK)
		{
			return TCL_ERROR;
		}
		specPtr->type = assign.icon.type;
		specPtr->index = assign.icon.index;
		specPtr->ascii = assign.icon.ascii;
	}
	else if (plane == VLT_PLANE_LETTER)
	{
		char *t = Tcl_GetStringFromObj(objPtr, NULL);
		if (strlen(t) != 1)
		{
			Tcl_SetResult(interp,
				format("bad letter \"%s\"", t), TCL_VOLATILE);
			return TCL_ERROR;
		}
		*((char *) dataPtr) = t[0];
	}
	else if (plane == VLT_PLANE_SYMBOL)
	{
		int symbol;
		if (map_symbol_find(interp, objPtr, &symbol) != TCL_OK)
		{
			return TCL_ERROR;
		}
		*((byte *) dataPtr) = symbol;
	}
	else if (plane == VLT_PLANE_FEAT)
	{
		int feat;
		if (Tcl_GetIntFromObj(interp, objPtr, &feat)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}
		if ((feat < 0) || (feat >= MAX_F_IDX))
		{
			Tcl_SetResult(interp,
				format("bad feature \"%d\"", feat), TCL_VOLATILE);
			return TCL_ERROR;
		}
		*((byte *) dataPtr) = feat;
	}

	return TCL_OK;
}

static Tcl_Obj *PlaneDataToObj(Tcl_Interp *interp, void *dataPtr, int plane)
{
	Tcl_Obj *objPtr = NULL;
	if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
	{
		char buf[128];
		objPtr = Tcl_NewStringObj(IconToString(buf, (IconSpec *) dataPtr), -1);
	}
	else if (plane == VLT_PLANE_LETTER)
	{
		objPtr = Tcl_NewStringObj((char *) dataPtr, 1);
	}
	else if (plane == VLT_PLANE_SYMBOL)
	{
		objPtr = Tcl_NewStringObj(map_symbol_name(*((byte *) dataPtr)), -1);
	}
	else if (plane == VLT_PLANE_FEAT)
	{
		objPtr = Tcl_NewIntObj(*((byte *) dataPtr));
	}
	return objPtr;
}

#define PlaneDataPtr(v,yy,xx,p) \
	(v->data[p] + ((yy - v->y) * v->dataWidth + (xx - v->x)) * s_plane_size[p])

#define PlaneDataGet(v,y,x,p,d) \
	memcpy(d, PlaneDataPtr(v, y, x, p), s_plane_size[p])

#define PlaneDataPut(v,y,x,p,d) \
	memcpy(PlaneDataPtr(v, y, x, p), d, s_plane_size[p])

static FILE *vault_fp;

static void wr_byte(byte v)
{
	(void) putc((int) v, vault_fp);
}

static void wr_s16b(s16b v)
{
	wr_byte((byte) (v & 0xFF));
	wr_byte((byte) ((v >> 8) & 0xFF));
}

static void rd_byte(byte *ip)
{
	*ip = getc(vault_fp) & 0xFF;
}

static void rd_s16b(s16b *ip)
{
	(*ip) = getc(vault_fp) & 0xFF;
	(*ip) |= (s16b) (getc(vault_fp) & 0xFF) << 8;
}

int vault_read(Tcl_Interp *interp, int vaultIndex, char *fileName)
{
	t_vault *vaultPtr = g_vault[vaultIndex];
	char fake[4];
	FILE *fp;
	s16b hgt16b, wid16b;
	IconSpec iconSpec, iconSpec2;
	int i, y, x, layer, plane;
	byte count, feat;
	int iconType[50];
	byte iconTypeCount;

	if ((fp = fopen(fileName, "rb")) == NULL)
	{
		FormatResult(interp, "error opening \"%s\" for reading", fileName);
		return TCL_ERROR;
	}
	vault_fp = fp;

	/* Read the file version */
	(void) fread(fake, 1, 4, fp);
dbwin("vault version %d.%d.%d\n", (int) fake[0], (int) fake[1], (int) fake[2]);

	/* Read a list of icon types */
	rd_byte(&iconTypeCount);
	if (iconTypeCount >= 50)
	{
		FormatResult(interp, "invalid iconTypeCount \"%d\":"
			" must be from 0 to 49", (int) iconTypeCount);
		goto error;
	}
dbwin("icon type count %d\n", (int) iconTypeCount);
	for (i = 0; i < iconTypeCount; i++)
	{
		char typeName[80];
		int c, n = 0;
		Tcl_HashEntry *hPtr;

		while ((n < 79) && (c = fgetc(fp)) != 0) typeName[n++] = c;
		typeName[n] = '\0';
dbwin("icon type \"%s\"\n", typeName);
		hPtr = Tcl_FindHashEntry(&g_icon_table, typeName);
		if (hPtr == NULL)
		{
dbwin("unknown icon type \"%s\"\n", typeName);
			iconType[i] = ICON_TYPE_NONE;
			continue;
		}
		iconType[i] = (int) Tcl_GetHashValue(hPtr);
	}

	/* Read the vault size */
	rd_s16b(&hgt16b);
	rd_s16b(&wid16b);
dbwin("vault hgt %d, wid %d\n", (int) hgt16b, (int) wid16b);

	/*
	 * XXX Hack -- If creating the vault, and the dimensions
	 * weren't given, then set them here.
	 */
	if (!vaultPtr->width || !vaultPtr->height)
	{
		int size;

		if (!vaultPtr->height) vaultPtr->height = hgt16b;
		if (!vaultPtr->width) vaultPtr->width = wid16b;

		vaultPtr->dataHeight = vaultPtr->height;
		vaultPtr->dataWidth = vaultPtr->width;
		size = vaultPtr->height * vaultPtr->width;
		for (plane = 0; plane < VLT_PLANE_MAX; plane++)
		{
			vaultPtr->data[plane] =
				(byte *) Tcl_AllocDebug(size * s_plane_size[plane]);
		}
		memset(vaultPtr->data[VLT_PLANE_LETTER], '.',
			size * s_plane_size[VLT_PLANE_LETTER]);
		memset(vaultPtr->data[VLT_PLANE_SYMBOL], 0,
			size * s_plane_size[VLT_PLANE_SYMBOL]);
	}

	if ((hgt16b < 0) || (hgt16b > vaultPtr->dataHeight))
	{
		Tcl_SetResult(interp, format("invalid height \"%d\":"
			" must be from 0 to %d", (int) hgt16b, vaultPtr->height),
			TCL_VOLATILE);
		goto error;
	}
	if ((wid16b < 0) || (wid16b > vaultPtr->dataWidth))
	{
		Tcl_SetResult(interp, format("invalid width \"%d\":"
			" must be from 0 to %d", (int) wid16b, vaultPtr->width),
			TCL_VOLATILE);
		goto error;
	}

	/* Read the icons with RLE */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		for (x = y = 0; y < hgt16b;)
		{
			byte type;
			s16b index;

			rd_byte(&count);
			rd_byte(&type);
			rd_s16b(&index);
			iconSpec.type = iconType[type];
			if (iconSpec.type == ICON_TYPE_NONE)
				index = 0;
			iconSpec.index = index;
			iconSpec.ascii = -1;
			for (i = count; i > 0; --i)
			{
				int layer2 = layer;

				/* 1.1.0 -- Swap layer 1 && 2 */
				if (fake[0] == 1 && fake[1] == 1)
					layer2 = !layer2;

				PlaneDataPut(vaultPtr, y, x,
					VLT_PLANE_ICON_1 + layer2, &iconSpec);

				if (++x >= wid16b)
				{
					x = 0;
					if (++y >= hgt16b) break;
				}
			}
		}

		/* 1.0.0 -- Only layer 1 */
		if (fake[0] == 1 && fake[1] == 0)
			break;

		/* 1.1.0 -- Only layer 1 && 2 */
		if (fake[0] == 1 && fake[1] == 1 && layer)
			break;
	}

	/* Read the features with RLE */
	for (x = y = 0; y < hgt16b;)
	{
		rd_byte(&count);
		rd_byte(&feat);
		if (feat >= MAX_F_IDX)
			feat = 0;
		for (i = count; i > 0; --i)
		{
			PlaneDataPut(vaultPtr, y, x, VLT_PLANE_FEAT, &feat);
			if (++x >= wid16b)
			{
				x = 0;
				if (++y >= hgt16b) break;
			}
		}
	}

	iconSpec.type = ICON_TYPE_NONE;
	iconSpec.index = 0;
	iconSpec.ascii = -1;
	for (layer = ICON_LAYER_2; layer < ICON_LAYER_MAX; layer++)
	{
		for (y = 0; y < hgt16b; y++)
		{
			for (x = 0; x < wid16b; x++)
			{
				/* Version 1.0.0 -- Init layer 2, 3, 4 */
				if ((fake[0] == 1) && (fake[1] == 0))
				{
					PlaneDataPut(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec);
				}

				/* Version 1.1.0 -- Copy opaque layer 2 -> 1 */
				if ((fake[0] == 1) && (fake[1] == 1) && (layer == ICON_LAYER_2))
				{
					PlaneDataGet(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec2);
					if (!g_icon_type[iconSpec2.type].rle_data)
					{
						PlaneDataPut(vaultPtr, y, x, VLT_PLANE_ICON_1, &iconSpec2);
						PlaneDataPut(vaultPtr, y, x, VLT_PLANE_ICON_2, &iconSpec);
					}
				}

				/* Version 1.1.0 -- Init layer 3, 4 */
				if ((fake[0] == 1) && (fake[1] == 1) && (layer > ICON_LAYER_2))
				{
					PlaneDataPut(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec);
				}
			}
		}
	}

	fclose(fp);

	VaultChanged(interp, vaultIndex);

	return TCL_OK;

error:
	fclose(fp);

	return TCL_ERROR;
}

/*
 * (vault) copy $plane $v y1 x1 y2 x2 $v2 y3 x3
 */
int objcmd_vault_copy(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultSrcPtr, *vaultDstPtr;
	int vaultIndex, vaultId;
	int y, plane;
	int src_y1, src_x1, src_y2, src_x2, dst_y1, dst_x1;
	char *dataSrc, *dataDst;
	int dataWidth, elemSize;

	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_plane, "plane",
		0, &plane) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (GetVaultFromObj(interp, objV[2], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultSrcPtr = g_vault[vaultIndex];
	if (Tcl_GetIntFromObj(interp, objV[3], &src_y1) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[4], &src_x1) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[5], &src_y2) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[6], &src_x2) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (ValidateCoord(interp, vaultIndex, src_y1, src_x1) != TCL_OK)
		return TCL_ERROR;
	if (ValidateCoord(interp, vaultIndex, src_y2, src_x2) != TCL_OK)
		return TCL_ERROR;

	if (GetVaultFromObj(interp, objV[7], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultDstPtr = g_vault[vaultIndex];

	if (Tcl_GetIntFromObj(interp, objV[8], &dst_y1) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (Tcl_GetIntFromObj(interp, objV[9], &dst_x1) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (ValidateCoord(interp, vaultIndex, dst_y1, dst_x1) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (dst_y1 + (src_y2 - src_y1) >= vaultDstPtr->height)
		return TCL_ERROR;
	if (dst_x1 + (src_x2 - src_x1) >= vaultDstPtr->width)
		return TCL_ERROR;

	dataSrc = vaultSrcPtr->data[plane];
	dataDst = vaultDstPtr->data[plane];
	elemSize = s_plane_size[plane];

	dataWidth = elemSize * ((src_x2 - src_x1) + 1);
	dataSrc += (src_x1 + src_y1 * vaultSrcPtr->dataWidth) * elemSize;
	dataDst += (dst_x1 + dst_y1 * vaultDstPtr->dataWidth) * elemSize;

	for (y = src_y1; y <= src_y2; y++)
	{
		memcpy(dataDst, dataSrc, dataWidth);
		dataSrc += vaultSrcPtr->dataWidth * elemSize;
		dataDst += vaultDstPtr->dataWidth * elemSize;
	}

	/* FIXME: if plane is shared, update other vaults too! */
	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

/*
 * (vault) create ?$option $value ...?
 */
int objcmd_vault_create(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex;
	int y, x, n;
	static CONST char *createSwitch[] = {"-file", "-height", "-width", NULL};
	Tcl_Obj *CONST *objPtr;
	char *fileName = NULL;
	int height, width, layer, plane;
	IconSpec iconSpec;

	objPtr = objV + 1;
	objC -= 1;
	height = width = 0;

	while (objC > 1)
	{
		int index;
	    if (Tcl_GetIndexFromObj(interp, objPtr[0], createSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }
		switch (index)
		{
			case 0: /* -file */
				fileName = Tcl_GetStringFromObj(objPtr[1], NULL);
				break;
			case 1: /* -height */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &height)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((height <= 0) || (height > 1000))
					return TCL_ERROR;
				break;
			case 2: /* -width */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &width)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((width <= 0) || (width > 1000))
					return TCL_ERROR;
				break;
		}
		objPtr += 2;
		objC -= 2;
	}

	/* Required number of arguments */
	if (objC != 0)
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, 1, objV,
			"-fileName path -height height -width width");

		/* Failure */
		return TCL_ERROR;
	}
	if (fileName == NULL)
	{
		if (!height) height = DUNGEON_HGT;
		if (!width) width = DUNGEON_WID;
	}
	vaultPtr = (t_vault *) Tcl_AllocDebug(sizeof(t_vault));
	vaultPtr->id = ++g_vault_id;
	vaultPtr->y = vaultPtr->x = 0;
	vaultPtr->height = height;
	vaultPtr->width = width;
	if (height && width)
	{
		n = height * width;
		vaultPtr->dataWidth = width;
		vaultPtr->dataHeight = height;
		for (plane = 0; plane < VLT_PLANE_MAX; plane++)
		{
			vaultPtr->data[plane] =
				(byte *) Tcl_AllocDebug(n * s_plane_size[plane]);
		}
	}
	else
	{
		/* These will be allocated by vault_read() */
		for (plane = 0; plane < VLT_PLANE_MAX; plane++)
			vaultPtr->data[plane] = NULL;
	}
	for (n = 0; n < VLT_PLANE_MAX; n++)
		vaultPtr->linker[n] = NULL;
	iconSpec.type = ICON_TYPE_NONE;
	iconSpec.index = 0;
	iconSpec.ascii = -1;
	for (y = 0; y < height; y++)
	{
		for (x = 0; x < width; x++)
		{
			byte feat = 1;
			char letter = '.';
			byte symbol = 0;
			for (layer = 0; layer < ICON_LAYER_MAX; layer++)
				PlaneDataPut(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec);
			PlaneDataPut(vaultPtr, y, x, VLT_PLANE_FEAT, &feat);
			PlaneDataPut(vaultPtr, y, x, VLT_PLANE_LETTER, &letter);
			PlaneDataPut(vaultPtr, y, x, VLT_PLANE_SYMBOL, &symbol);
		}
	}
	vaultIndex = g_vault_count;
	g_vault = Array_Insert(g_vault, &g_vault_count,
		sizeof(t_vault *), g_vault_count);
	g_vault[vaultIndex] = vaultPtr;
	if (fileName)
	{
		if (vault_read(interp, vaultIndex, fileName) != TCL_OK)
		{
			for (n = 0; n < VLT_PLANE_MAX; n++)
				VaultFreePlane(vaultIndex, n);
			Tcl_FreeDebug((char *) vaultPtr);
			g_vault = Array_Delete(g_vault, &g_vault_count,
				sizeof(t_vault *), vaultIndex);
			return TCL_ERROR;
		}
	}
	IntResult(interp, vaultPtr->id);

	return TCL_OK;
}

/*
 * (vault) current ?$v?
 */
int objcmd_vault_current(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int vaultIndex, vaultId;

	if (objC == 2)
	{
		DoubleLink *link;

		if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex,
			TRUE) != TCL_OK)
		{
			return TCL_ERROR;
		}
#if 0
		if (vaultId && g_vault_current && (g_vault_current->id == vaultId))
			return TCL_OK;
		if (!vaultId && !g_vault_current)
			return TCL_OK;
#endif
		if (vaultId)
			g_vault_current = g_vault[vaultIndex];
		else
			g_vault_current = NULL;

		/* Oct 24 2004 */
		for (link = WidgetListMapped.head; link; link = link->next)
		{
			Widget *widgetPtr = DoubleLink_Data(link, Widget);
			if (((ExWidget *) widgetPtr)->vaultNum == 0)
				Widget_Wipe(widgetPtr);
		}
		angtk_cave_changed(); /* Oct 24 2004 */

		return TCL_OK;
	}
	if (g_vault_current)
		IntResult(interp, g_vault_current->id);
	else
		IntResult(interp, 0);

	return TCL_OK;
}

/*
 * (vault) delete $v
 */
int objcmd_vault_delete(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int vaultIndex, vaultId;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	VaultFree(vaultIndex);

	return TCL_OK;
}

/*
 * (vault) get $v $plane ?$y1 $x1? ?y2 $x2?
 */
int objcmd_vault_get(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;
	int plane;
	int x, y, y1, x1, y2, x2;
	Tcl_Obj *listObjPtr;
	void *dataPtr;

	if ((objC != 3) && (objC != 5) && (objC != 7))
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv,
			"vaultId plane ?y1 x1? ?y2 x2?");
		return TCL_ERROR;
	}
	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];
	if (Tcl_GetIndexFromObj(interp, objV[2], keyword_plane, "plane",
		0, &plane) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (objC == 3)
	{
		y1 = vaultPtr->y;
		x1 = vaultPtr->x;
		y2 = vaultPtr->y + vaultPtr->height - 1;
		x2 = vaultPtr->x + vaultPtr->width - 1;
	}
	if (objC >= 5)
	{
		if (Tcl_GetIntFromObj(interp, objV[3], &y1) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[4], &x1) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, y1, x1) != TCL_OK)
		{
			return TCL_ERROR;
		}
		y2 = y1;
		x2 = x1;
	}
	/* Return a single element */
	if (objC == 5)
	{
		dataPtr = PlaneDataPtr(vaultPtr, y1, x1, plane);
		Tcl_SetObjResult(interp, PlaneDataToObj(interp, dataPtr, plane));
		return TCL_OK;
	}
	if (objC == 7)
	{
		if (Tcl_GetIntFromObj(interp, objV[5], &y2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[6], &x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, y2, x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}
	listObjPtr = Tcl_NewListObj(0, NULL);
	for (y = y1; y <= y2; y++)
	{
		Tcl_Obj *rowPtr = Tcl_NewListObj(0, NULL);
		for (x = x1; x <= x2; x++)
		{
			dataPtr = PlaneDataPtr(vaultPtr, y, x, plane);
			Tcl_ListObjAppendElement(interp, rowPtr,
				PlaneDataToObj(interp, dataPtr, plane));
		}
		Tcl_ListObjAppendElement(interp, listObjPtr, rowPtr);
	}
	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;
}

/*
 * (vault) height $v ?$height?
 */
int objcmd_vault_height(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;
	int height;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];
	if (objC == 2)
	{
		IntResult(interp, vaultPtr->height);
		return TCL_OK;
	}
	if (Tcl_GetIntFromObj(interp, objV[2], &height) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((height < 1) || (height > vaultPtr->dataHeight))
	{
		Tcl_SetResult(interp,
			format("invalid height \"%d\": must from %d to %d",
			height, 1, vaultPtr->dataHeight),
			TCL_VOLATILE);
		return TCL_ERROR;
	}
	vaultPtr->height = height;
	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

/*
 * (vault) put $v $plane $data ?$y1 $x1? ?$y2 $x2?
 */
int objcmd_vault_put(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;
	int width, plane;
	int row, col, dataHeight, dataWidth = 0, elemSize = 0;
	int y, put_y, put_x, put_y2, put_x2, data_y;
	char *dataPtr = NULL, *destPtr = NULL;
	Tcl_Obj *elemPtr, *listObjPtr, *rowPtr;

	if ((objC != 4) && (objC != 6) && (objC != 8))
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv,
			"vaultId plane data ?y1 x1? ?y2 x2?");
		return TCL_ERROR;
	}
	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];
	if (Tcl_GetIndexFromObj(interp, objV[2], keyword_plane, "plane",
		0, &plane) != TCL_OK)
	{
		return TCL_ERROR;
	}
	listObjPtr = objV[3];
	if (Tcl_ListObjLength(interp, listObjPtr, &dataHeight) != TCL_OK)
	{
		return TCL_ERROR;
	}
	/* Nothing to do */
	if (!dataHeight)
		return TCL_OK;
	put_y = 0;
	put_x = 0;
	if (objC > 4)
	{
		if (Tcl_GetIntFromObj(interp, objV[4], &put_y) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[5], &put_x) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, put_y, put_x) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}
	put_y2 = put_y + dataHeight - 1;
	put_x2 = -1;
	if (objC == 8)
	{
		if (Tcl_GetIntFromObj(interp, objV[6], &put_y2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[7], &put_x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, put_y2, put_x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}
	if (put_y + dataHeight > vaultPtr->height)
	{
		StaticResult(interp, "data too tall");
		return TCL_ERROR;
	}
	for (row = 0; row < dataHeight; row++)
	{
		if (Tcl_ListObjIndex(interp, listObjPtr, row,
			&rowPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_ListObjLength(interp, rowPtr, &width) != TCL_OK)
		{
			return TCL_ERROR;
		}
		/* Nothing to do */
		if (!width)
			return TCL_OK;
		if (row == 0)
		{
			elemSize = s_plane_size[plane];
			destPtr = vaultPtr->data[plane];
			dataWidth = width;
			dataPtr = Tcl_AllocDebug(elemSize * dataHeight * dataWidth);
		}
		else if (width != dataWidth)
		{
			return TCL_ERROR;
		}
		if (put_x + width > vaultPtr->width)
		{
			StaticResult(interp, "data too wide");
			return TCL_ERROR;
		}
		for (col = 0; col < dataWidth; col++)
		{
			if (Tcl_ListObjIndex(interp, rowPtr, col, &elemPtr)
				!= TCL_OK)
			{
				return TCL_ERROR;
			}			
			if (PlaneDataFromObj(interp,
				dataPtr + (row * dataWidth + col) * elemSize,
				elemPtr, plane) != TCL_OK)
			{
				return TCL_ERROR;
			}
		}
/*
		if (put_x2 == -1)
			put_x2 = put_x + dataWidth - 1;
*/
	}
	for (y = put_y; y <= put_y2; )
	{
		for (data_y = 0; data_y < dataHeight; data_y++, y++)
		{
			memcpy(destPtr + put_x * elemSize +
				y * elemSize * vaultPtr->dataWidth,
				dataPtr + data_y * elemSize * dataWidth,
				elemSize * dataWidth);
			if (y >= vaultPtr->dataHeight) break;
		}
	}
	Tcl_FreeDebug(dataPtr);
	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

/*
 * (vault) share $plane $v $v2
 */
int objcmd_vault_share(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultSrcPtr, *vaultDstPtr;
	int vaultIndex, vaultId;
	int plane;
	DoubleLinker *linker;

	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_plane, "plane",
		0, &plane) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (GetVaultFromObj(interp, objV[2], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultSrcPtr = g_vault[vaultIndex];
	if (GetVaultFromObj(interp, objv[4], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultDstPtr = g_vault[vaultIndex];
	if (vaultSrcPtr == vaultDstPtr)
		return TCL_ERROR;
	if ((vaultSrcPtr->dataHeight != vaultDstPtr->dataHeight) ||
		(vaultSrcPtr->dataWidth != vaultDstPtr->dataWidth))
		return TCL_ERROR;

	VaultFreePlane(vaultIndex, plane);
	linker = vaultSrcPtr->linker[plane];

	/* The source plane is shared already */
	if (linker)
	{
		DoubleLink_Init(linker, &vaultDstPtr->link[plane], vaultDstPtr);
		DoubleLink_Link(&vaultDstPtr->link[plane]);
		vaultDstPtr->linker[plane] = linker;
	}

	/* The source plane is not shared */
	else
	{
		linker = (DoubleLinker *) Tcl_AllocDebug(sizeof(DoubleLinker));
		DoubleLink_Init(linker, NULL, NULL);
		linker->what = "vault";
		vaultSrcPtr->linker[plane] = linker;
		DoubleLink_Init(linker, &vaultSrcPtr->link[plane], vaultSrcPtr);
		DoubleLink_Link(&vaultSrcPtr->link[plane]);
		vaultDstPtr->linker[plane] = linker;
		DoubleLink_Init(linker, &vaultDstPtr->link[plane], vaultDstPtr);
		DoubleLink_Link(&vaultDstPtr->link[plane]);
	}
	vaultDstPtr->data[plane] = vaultSrcPtr->data[plane];
	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

#if 0

/*
 * (vault) symbol $v $f_idx $?symbol?
 */
int objcmd_vault_symbol(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;
	int feat, symbol;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];
	if (Tcl_GetIntFromObj(interp, objV[2], &feat) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((feat < 0) || (feat >= MAX_F_IDX))
	{
		Tcl_SetResult(interp,
			format("bad feature \"%d\"", feat), TCL_VOLATILE);
		return TCL_ERROR;
	}
	if (objC == 3)
	{
		StringResult(interp, map_symbol_name(vaultPtr->symbol[feat]));
		return TCL_OK;
	}

	if (map_symbol_find(interp, objV[3], &symbol) != TCL_OK)
	{
		return TCL_ERROR;
	}
    vaultPtr->symbol[feat] = symbol;

	return TCL_OK;
}

#endif /* 0 */

/*
 * (vault) swap $v $v2
 */
int objcmd_vault_swap(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultSrcPtr, *vaultDstPtr;
	int vaultIndexSrc, vaultId, vaultIndexDst;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndexSrc,
		FALSE) != TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultSrcPtr = g_vault[vaultIndexSrc];
	if (GetVaultFromObj(interp, objV[2], &vaultId, &vaultIndexDst,
		FALSE) != TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultDstPtr = g_vault[vaultIndexDst];
	if (vaultSrcPtr == vaultDstPtr)
		return TCL_ERROR;

	/* Just swap the ids */
	vaultId = vaultSrcPtr->id;
	vaultSrcPtr->id = vaultDstPtr->id;
	vaultDstPtr->id = vaultId;

	VaultChanged(interp, vaultIndexSrc);
	VaultChanged(interp, vaultIndexDst);

	return TCL_OK;
}

/*
 * (vault) tocave $v
 */
int objcmd_vault_tocave(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;

	int i, y, x, qy, qx;

	int stick_to_stair = FALSE, stick_ok = FALSE;

	int daytime = (turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2);

#if defined(OANGBANDTK)
	int special_y[20], special_x[20], special_f[20], special_cnt = 0;
#endif

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex,
		FALSE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	vaultPtr = g_vault[vaultIndex];

	/* In the town */
	if (!p_ptr_depth)
	{
		/*
		 * If this is a new game, or the character just went up to the town,
		 * he will be on the down staircase.
		 */
		stick_to_stair = (cave_feat(p_ptr_py, p_ptr_px) == FEAT_MORE);
	}

	qy = 0;
	qx = 0;

	/* Remember the size of the cave */
	g_cave_hgt = vaultPtr->height;
	g_cave_wid = vaultPtr->width;

#if defined(ZANGBANDTK)
	cur_hgt = g_cave_hgt;
	cur_wid = g_cave_wid;
#endif /* ZANGBANDTK */

	/* Start with solid walls */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
#if defined(OANGBANDTK)
			if ((cave_feat(y, x) == FEAT_MONSTER_TRAP) ||
				(cave_feat(y, x) == FEAT_GLYPH))
			{
				special_y[special_cnt] = y;
				special_x[special_cnt] = x;
				special_f[special_cnt] = cave_feat(y, x);
				++special_cnt;
			}
#endif /* OANGBANDTK */

			/* No flags */
			cave_info(y, x) = 0;

			/* No objects */
			cave_o_idx(y, x) = 0;

			/* No monsters */
			cave_m_idx(y, x) = 0;

			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);

#if defined(ZANGBANDTK)
			/* Hack -- Cancel the wilderness mimicry */
			cave[y][x].mimic = 0;
#endif /* ZANGBANDTK */
		}
	}

	/* Apply the vault features */
	vaultPtr->y = qy;
	vaultPtr->x = qx;
	for (y = 0; y < vaultPtr->height; y++)
	{
		for (x = 0; x < vaultPtr->width; x++)
		{
			byte feat;

			PlaneDataGet(vaultPtr, y + qy, x + qx, VLT_PLANE_FEAT, &feat);
			cave_info(y + qy, x + qx) = 0;
			cave_set_feat(y + qy, x + qx, feat);

			if (stick_to_stair && (feat == FEAT_MORE))
			{
				player_place(y + qy, x + qx);
				stick_ok = TRUE;
			}
		}
	}

	/* Place the player */
	if (stick_to_stair && stick_ok)
	{
		/* Nothing */
	}
	else if (cave_floor_bold(p_ptr_py, p_ptr_px))
	{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
		cave_m_idx(p_ptr_py, p_ptr_px) = -1;
#endif /* ANGBANDTK, KANGBANDTK */
	}
	else
	{
		/* Find an empty location */
		while (TRUE)
		{
			/* Pick a location */
			y = qy + rand_range(1, vaultPtr->height - 2);
			x = qx + rand_range(1, vaultPtr->width - 2);

			/* Require an "empty" floor grid */
			if (cave_floor_bold(y, x)) break;
		}

		player_place(y, x);
	}

#if defined(OANGBANDTK)

	/* Process special features */
	for (i = 0; i < special_cnt; i++)
	{
		y = special_y[i];
		x = special_x[i];

		/* Require an "empty" floor grid */
		if (cave_empty_bold(y, x))
		{
			cave_set_feat(y, x, special_f[i]);
			continue;
		}

		/* Find an empty location */
		while (TRUE)
		{
			/* Pick a location */
			y = qy + rand_range(1, vaultPtr->height - 2);
			x = qx + rand_range(1, vaultPtr->width - 2);

			/* Require an "empty" floor grid */
			if (cave_empty_bold(y, x)) break;
		}

		/* Place the feature */
		cave_set_feat(y, x, special_f[i]);
	}

#endif /* OANGBANDTK */

	/* Process objects */
	for (i = 1; i < o_max; i++)
	{
		/* Access object */
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip carried objects */
		if (o_ptr->held_m_idx) continue;

		/* Require an "empty" floor grid */
		if (cave_empty_bold(o_ptr->iy, o_ptr->ix))
		{
			cave_o_idx(o_ptr->iy, o_ptr->ix) = i;
			continue;
		}

		/* Find an empty location */
		while (TRUE)
		{
			/* Pick a location */
			y = qy + rand_range(1, vaultPtr->height - 2);
			x = qx + rand_range(1, vaultPtr->width - 2);

			/* Require an "empty" floor grid */
			if (cave_empty_bold(y, x)) break;
		}

		/* Place the object */
		o_ptr->iy = y;
		o_ptr->ix = x;
		cave_o_idx(y, x) = i;
	}

	/* Process the monsters */
	for (i = 0; i < m_max; i++)
	{
		/* Access the monster */
		monster_type *m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

		/* Require an "empty" floor grid */
		if (cave_empty_bold(m_ptr->fy, m_ptr->fx))
		{
			cave_m_idx(m_ptr->fy, m_ptr->fx) = i;
			continue;
		}

		/* Find an empty location */
		while (TRUE)
		{
			/* Pick a location */
			y = qy + rand_range(1, vaultPtr->height - 2);
			x = qx + rand_range(1, vaultPtr->width - 2);

			/* Require an "empty" floor grid */
			if (cave_empty_bold(y, x)) break;
		}

		/* Place the monster */
		m_ptr->fy = y;
		m_ptr->fx = x;
		cave_m_idx(y, x) = i;
	}

	if (!p_ptr_depth)
	{
		/* Apply illumination */
		town_illuminate(daytime);
	}

	return TCL_OK;
}

/*
 * (vault) width $v ?$width?
 */
int objcmd_vault_width(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultIndex, vaultId;
	int width;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];
	if (objC == 2)
	{
		IntResult(interp, vaultPtr->width);
		return TCL_OK;
	}
	if (Tcl_GetIntFromObj(interp, objV[2], &width) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((width < 1) || (width > vaultPtr->dataWidth))
	{
		FormatResult(interp, "invalid width \"%d\": must from %d to %d",
			width, 1, vaultPtr->dataWidth);
		return TCL_ERROR;
	}
	vaultPtr->width = width;
	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

/*
 * (vault) write $v $path
 */
int objcmd_vault_write(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_vault *vaultPtr;
	int vaultId, vaultIndex;
	char fake[4], *fileName, *t;
	FILE *fp;
	IconSpec iconSpec, iconSpec2;
	int i, y, x, layer;
	int type2index[50];
	byte count, feat, feat2;
	Tcl_DString extDString;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE) != TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];

	/* Get the file name */
	t = Tcl_GetStringFromObj(objV[2], NULL);

	/* Translate the file name */
	fileName = UtfToExt_TranslateFileName(interp, t, &extDString);
	if (fileName == NULL)
	{
		/* Note: Tcl_DStringFree() is called for us */
		return TCL_ERROR;
	}

	if ((fp = fopen(fileName, "wb")) == NULL)
	{
		FormatResult(interp, "error opening \"%s\" for writing", t);
		Tcl_DStringFree(&extDString);
		return TCL_ERROR;
	}

	Tcl_DStringFree(&extDString);

	vault_fp = fp;

	/* Write the file version */
	fake[0] = VLT_VERSION_MAJOR;
	fake[1] = VLT_VERSION_MINOR;
	fake[2] = VLT_VERSION_PATCH;
	fake[3] = VLT_VERSION_EXTRA;
	(void) fwrite(fake, 1, 4, fp);

	/* Write a list of all the icon types */
#ifdef SAVE_MIN_TYPES
	{
		int iconType[50], count = 0;
		for (i = 0; i < g_icon_type_count; i++)
		{
			iconType[i] = 0;
		}
		for (y = 0; y < vaultPtr->height; y++)
		{
			for (x = 0; x < vaultPtr->width; x++)
			{
				for (layer = 0; layer < ICON_LAYER_MAX; layer++)
				{
					PlaneDataGet(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer,
						&iconSpec);
					if (!iconType[iconSpec.type])
					{
						iconType[iconSpec.type] = 1;
						++count;
					}
				}
			}
		}
		wr_byte((byte) count);
		count = 0;
		for (i = 0; i < g_icon_type_count; i++)
		{
			if (iconType[i])
			{
				char *s = g_icon_type[i].desc;
				(void) fwrite(s, 1, strlen(s) + 1, fp);

				type2index[i] = count++;
			}
		}
	}
#else
	wr_byte((byte) g_icon_type_count);
	for (i = 0; i < g_icon_type_count; i++)
	{
		char *s = g_icon_type[i].desc;
		(void) fwrite(s, 1, strlen(s) + 1, fp);

		type2index[i] = i;
	}
#endif

	/* Write the vault size */
	wr_s16b(vaultPtr->height);
	wr_s16b(vaultPtr->width);

	/* Write the icons with RLE */
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		count = 0;
		PlaneDataGet(vaultPtr, 0, 0, VLT_PLANE_ICON_1 + layer, &iconSpec);
		for (y = 0; y < vaultPtr->height; y++)
		{
			for (x = 0; x < vaultPtr->width; x++)
			{
				PlaneDataGet(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec2);
				if (((iconSpec.type != iconSpec2.type) ||
					(iconSpec.index != iconSpec2.index)) ||
					(count == MAX_UCHAR))
				{
					byte type = type2index[iconSpec.type];
					s16b index = iconSpec.index;

					wr_byte(count);
					wr_byte(type);
					wr_s16b(index);

					iconSpec = iconSpec2;
					count = 1;
				}
				else
				{
					++count;
				}
			}
		}
		if (count)
		{
			byte type = type2index[iconSpec.type];
			s16b index = iconSpec.index;

			wr_byte(count);
			wr_byte(type);
			wr_s16b(index);
		}
	}

	/* Write the features with RLE */
	count = 0;
	PlaneDataGet(vaultPtr, 0, 0, VLT_PLANE_FEAT, &feat);
	for (y = 0; y < vaultPtr->height; y++)
	{
		for (x = 0; x < vaultPtr->width; x++)
		{
			PlaneDataGet(vaultPtr, y, x, VLT_PLANE_FEAT, &feat2);
			if ((feat != feat2) || (count == MAX_UCHAR))
			{
				wr_byte(count);
				wr_byte(feat);

				feat = feat2;
				count = 1;
			}
			else
			{
				++count;
			}
		}
	}
	if (count)
	{
		wr_byte(count);
		wr_byte(feat);
	}

	fclose(fp);

	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

/*
 * (vault) read $v $path
 */
int objcmd_vault_read(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int vaultId, vaultIndex;
	char *fileName, *t;
	int result;
	Tcl_DString extDString;

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the file name */
	t = Tcl_GetStringFromObj(objV[2], NULL);

	/* Translate the file name */
	fileName = UtfToExt_TranslateFileName(interp, t, &extDString);
	if (fileName == NULL)
	{
		/* Note: Tcl_DStringFree() is called for us */
		return TCL_ERROR;
	}

	result = vault_read(interp, vaultIndex, fileName);

	Tcl_DStringFree(&extDString);

	return result;
}

/*
 * (vault) replace $v $matchList $replaceList ?$y1 $x1? ?$y2 $x2?
 */
int objcmd_vault_replace(ClientData clientData, Tcl_Interp * interp, int objc,
	Tcl_Obj * CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int vaultId, vaultIndex;
	t_vault *vaultPtr;
	int i, layer, length, plane;
	int y, x, y1, x1, y2, x2;
	bool matchPlane[VLT_PLANE_MAX], repPlane[VLT_PLANE_MAX];
	IconSpec matchSpec[ICON_LAYER_MAX], repSpec[ICON_LAYER_MAX], iconSpec;
	byte matchFeat, repFeat, feat;
	byte matchSymbol, repSymbol, symbol;
	char matchLetter, repLetter, letter;
	Tcl_Obj *listObjPtr, *objPtr;

	if ((objC != 4) && (objC != 6) && (objC != 8))
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv,
			"vaultId matchList replaceList ?y1 x1? ?y2 x2?");
		return TCL_ERROR;
	}

	if (GetVaultFromObj(interp, objV[1], &vaultId, &vaultIndex, FALSE)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}
	vaultPtr = g_vault[vaultIndex];

	for (plane = 0; plane < VLT_PLANE_MAX; plane++)
	{
		matchPlane[plane] = FALSE;
		repPlane[plane] = FALSE;
	}

	listObjPtr = objV[2];
	if (Tcl_ListObjLength(interp, listObjPtr, &length) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (length & 1)
		return TCL_ERROR;
	for (i = 0; i < length; i += 2)
	{
		if (Tcl_ListObjIndex(interp, listObjPtr, i, &objPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIndexFromObj(interp, objPtr, keyword_plane, "plane",
			0, &plane) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_ListObjIndex(interp, listObjPtr, i + 1, &objPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
		{
			layer = plane - VLT_PLANE_ICON_1;
			if (PlaneDataFromObj(interp, &matchSpec[layer], objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_LETTER)
		{
			if (PlaneDataFromObj(interp, &matchLetter, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_SYMBOL)
		{
			if (PlaneDataFromObj(interp, &matchSymbol, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_FEAT)
		{
			if (PlaneDataFromObj(interp, &matchFeat, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		matchPlane[plane] = TRUE;
	}

	listObjPtr = objV[3];
	if (Tcl_ListObjLength(interp, listObjPtr, &length) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if (length & 1)
		return TCL_ERROR;
	for (i = 0; i < length; i += 2)
	{
		if (Tcl_ListObjIndex(interp, listObjPtr, i, &objPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIndexFromObj(interp, objPtr, keyword_plane, "plane",
			0, &plane) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_ListObjIndex(interp, listObjPtr, i + 1, &objPtr) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
		{
			layer = plane - VLT_PLANE_ICON_1;
			if (PlaneDataFromObj(interp, &repSpec[layer], objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_LETTER)
		{
			if (PlaneDataFromObj(interp, &repLetter, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_SYMBOL)
		{
			if (PlaneDataFromObj(interp, &repSymbol, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		else if (plane == VLT_PLANE_FEAT)
		{
			if (PlaneDataFromObj(interp, &repFeat, objPtr, plane) != TCL_OK)
				return TCL_ERROR;
		}
		repPlane[plane] = TRUE;
	}

	y1 = 0, y2 = vaultPtr->dataHeight - 1;
	x1 = 0, x2 = vaultPtr->dataWidth - 1;

	if (objC > 4)
	{
		if (Tcl_GetIntFromObj(interp, objV[4], &y1) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[5], &x1) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, y1, x1) != TCL_OK)
		{
			return TCL_ERROR;
		}

		y2 = y1;
		x2 = x1;
	}

	if (objC == 8)
	{
		if (Tcl_GetIntFromObj(interp, objV[6], &y2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (Tcl_GetIntFromObj(interp, objV[7], &x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (ValidateCoord(interp, vaultIndex, y2, x2) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			bool match = TRUE;
			for (plane = 0; plane < VLT_PLANE_MAX; plane++)
			{
				if (!matchPlane[plane]) continue;
				if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
				{
					layer = plane - VLT_PLANE_ICON_1;
					PlaneDataGet(vaultPtr, y, x, plane, &iconSpec);
					if ((iconSpec.type != matchSpec[layer].type) ||
						(iconSpec.index != matchSpec[layer].index) ||
						(iconSpec.ascii != matchSpec[layer].ascii))
					{
						match = FALSE;
						break;
					}
				}
				else if (plane == VLT_PLANE_LETTER)
				{
					PlaneDataGet(vaultPtr, y, x, plane, &letter);
					if (letter != matchLetter)
					{
						match = FALSE;
						break;
					}
				}
				else if (plane == VLT_PLANE_SYMBOL)
				{
					PlaneDataGet(vaultPtr, y, x, plane, &symbol);
					if (symbol != matchSymbol)
					{
						match = FALSE;
						break;
					}
				}
				else if (plane == VLT_PLANE_FEAT)
				{
					PlaneDataGet(vaultPtr, y, x, plane, &feat);
					if (feat != matchFeat)
					{
						match = FALSE;
						break;
					}
				}
			}
			if (!match) continue;
			for (plane = 0; plane < VLT_PLANE_MAX; plane++)
			{
				if (!repPlane[plane]) continue;
				if ((plane >= VLT_PLANE_ICON_1) && (plane <= VLT_PLANE_ICON_4))
				{
					layer = plane - VLT_PLANE_ICON_1;
					PlaneDataPut(vaultPtr, y, x, plane, &repSpec[layer]);
				}
				else if (plane == VLT_PLANE_LETTER)
				{
					PlaneDataPut(vaultPtr, y, x, plane, &repLetter);
				}
				else if (plane == VLT_PLANE_SYMBOL)
				{
					PlaneDataPut(vaultPtr, y, x, plane, &repSymbol);
				}
				else if (plane == VLT_PLANE_FEAT)
				{
					PlaneDataPut(vaultPtr, y, x, plane, &repFeat);
				}
			}
		}
	}

	VaultChanged(interp, vaultIndex);

	return TCL_OK;
}

int vault_apply_feat(int y, int x)
{
	t_vault *vaultPtr;
	int y2, x2;
	byte feat;

	if (g_vault_current == NULL) return FALSE;
	vaultPtr = g_vault_current;

	vaultPtr->y = y;
	vaultPtr->x = x;
	for (y2 = 0; y2 < vaultPtr->height; y2++)
	{
		for (x2 = 0; x2 < vaultPtr->width; x2++)
		{
			PlaneDataGet(vaultPtr, y2 + y, x2 + x, VLT_PLANE_FEAT, &feat);
			cave_info(y2 + y, x2 + x) = 0;
			cave_set_feat(y2 + y, x2 + x, feat);
		}
	}

	return TRUE;
}


/*
 * Hack -- In order to display a masked icon, we need another icon
 * to act as the background. In the cave, we can use a hard-coded
 * feature index. In the town, we can have any (unmasked) icon for
 * a floor grid. This routine tells us what icon to use.
 */
int vault_icon(int y, int x, bool test_feat, t_assign assign[ICON_LAYER_MAX])
{
	t_vault *vaultPtr;
	IconSpec iconSpec;
	int layer;
	byte feat;

	if (g_vault_current == NULL)
		return FALSE;
	vaultPtr = g_vault_current;

	if (y < vaultPtr->y || y >= vaultPtr->y + vaultPtr->height)
		return FALSE;
	if (x < vaultPtr->x || x >= vaultPtr->x + vaultPtr->width)
		return FALSE;

	if (test_feat)
	{
		int feat2 = cave_feat(y, x);

#if defined(ZANGBANDTK)
		/* Apply "mimic" field */
		if (cave[y][x].mimic)
			feat2 = cave[y][x].mimic;
#endif /* ZANGBANDTK */

		/* Apply mimic field */
		feat2 = f_info[feat2].mimic;

		/* XXX Hack -- Invisible trap -> floor */
		if (feat2 == FEAT_INVIS) feat2 = FEAT_FLOOR;

		PlaneDataGet(vaultPtr, y, x, VLT_PLANE_FEAT, &feat);
		if (feat != feat2)
			return FALSE;
	}

	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		PlaneDataGet(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &iconSpec);

		/* If layer 1 is NONE, assume no icon assigned here */
		if (!layer && (iconSpec.type == ICON_TYPE_NONE))
			return FALSE;

		assign[layer].assignType = ASSIGN_TYPE_ICON;
		assign[layer].icon.type = iconSpec.type;
		assign[layer].icon.index = iconSpec.index;
		assign[layer].icon.ascii = iconSpec.ascii;
	}

	return TRUE;
}

void Widget_SetVault(Widget *widgetPtr)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	int i;

	for (i = 0; i < g_vault_count; i++)
	{
		if (g_vault[i]->id == exPtr->vaultNum)
		{
			exPtr->vaultPtr = g_vault[i];
			break;
		}
	}
}

int Icon_Transparent(t_icon_type *iconTypePtr, int index, int iso)
{
	if (index < 0 || index >= iconTypePtr->icon_count)
		return 1;
	if (iconTypePtr->flags[index] & ICON_FLAG_ISO)
		return 0;
	if (iso)
		return 1;
	return (iconTypePtr->rle_data != NULL);
}

/* Widget.whatToDrawProc() */
void vault_wtd(Widget *widgetPtr, int y, int x, t_display *wtd)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	t_vault *vaultPtr;
	IconSpec iconFG, iconBG;
	t_assign assign;
	byte feat;
	int layer, iso = (widgetPtr->style == WIDGET_STYLE_ISO);

	/* Get the vault for this widget */
	vaultPtr = (t_vault *) exPtr->vaultPtr;

	/* Should never happen */
	if (!vaultPtr)
	{
		wtd->blank = TRUE;
		return;
	}

	/*  */
	if ((y < vaultPtr->y) || (y >= vaultPtr->y + vaultPtr->height) ||
		(x < vaultPtr->x) || (x >= vaultPtr->x + vaultPtr->width))
	{
		wtd->blank = TRUE;
		return;
	}

	wtd->anim = FALSE;
	wtd->blank = FALSE;
	wtd->fg.type = ICON_TYPE_NONE;
	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		PlaneDataGet(vaultPtr, y, x, VLT_PLANE_ICON_1 + layer, &wtd->bg[layer]);
	}

	iconFG = wtd->bg[ICON_LAYER_2];
	iconBG = wtd->bg[ICON_LAYER_1];

	/* No icon was specified. Use the feature assignment */
	if ((iconFG.type == ICON_TYPE_NONE) && (iconBG.type == ICON_TYPE_NONE))
	{
		PlaneDataGet(vaultPtr, y, x, VLT_PLANE_FEAT, &feat);

		/* Apply mimic field */
		feat = f_info[feat].mimic;

		assign = g_assign[ASSIGN_FEATURE].assign[feat];

		FinalIcon(&iconFG, &assign, 1, NULL, NULL);
		if (is_sprite(&assign))
			wtd->anim = TRUE;

		/* The foreground icon is transparent */
		if (feat != g_background[feat] /* g_icon_type[iconFG.type].rle_data */)
		{
			/* Get the background feature assignment */
			t_assign assign = g_assign[ASSIGN_FEATURE].assign[g_background[feat]];

			FinalIcon(&iconBG, &assign, 1, NULL, NULL);
			if (is_sprite(&assign))
				wtd->anim = TRUE;

			/* Error: background icon is also transparent */
			if (Icon_Transparent(&g_icon_type[iconBG.type], iconBG.index, iso))
			{
				iconBG.type = ICON_TYPE_DEFAULT;
				iconBG.index = 0;
				iconBG.ascii = -1;
			}
		}

		/* Foreground is not transparent */
		else
		{
			/* Swap fg & bg */
			iconBG = iconFG;

			/* Ignore fg */
			iconFG.type = ICON_TYPE_NONE;
		}
	}

	/* Two icons specified. Sanity check */
	else if ((iconFG.type != ICON_TYPE_NONE) && (iconBG.type != ICON_TYPE_NONE))
	{
		/* The foreground icon is transparent */
		if (Icon_Transparent(&g_icon_type[iconFG.type], iconFG.index, iso))
		{
			/* Error: background icon is also transparent */
			if (Icon_Transparent(&g_icon_type[iconBG.type], iconBG.index, iso))
			{
				iconBG.type = ICON_TYPE_DEFAULT;
				iconBG.index = 0;
				iconBG.ascii = -1;
			}
		}

		/* Foreground is not transparent */
		else
		{
			/* Swap fg & bg */
			iconBG = iconFG;

			/* Ignore fg */
			iconFG.type = ICON_TYPE_NONE;
		}
	}

	/* Only foreground specified */
	else if (iconBG.type == ICON_TYPE_NONE)
	{
		/* Error: must have background */
		iconFG.type = ICON_TYPE_NONE;
		iconBG.type = ICON_TYPE_DEFAULT;
		iconBG.index = 0;
		iconBG.ascii = -1;
	}

	/* Only background specified */
	else
	{
		/* Error: background is masked */
		if (Icon_Transparent(&g_icon_type[iconBG.type], iconBG.index, iso))
		{
			iconBG.type = ICON_TYPE_DEFAULT;
			iconBG.index = 0;
			iconBG.ascii = -1;
		}
	}

	wtd->bg[ICON_LAYER_2] = iconFG;
	wtd->bg[ICON_LAYER_1] = iconBG;

	for (layer = 0; layer < ICON_LAYER_MAX; layer++)
	{
		wtd->bg[layer].dark = 0;
	}
}

/* Widget.symbolProc() */
int vault_symbol_proc(Widget *widgetPtr, int y, int x)
{
	ExWidget *exPtr = (ExWidget *) widgetPtr;
	t_vault *vaultPtr;
	byte symbol;

	/* Get the vault for this widget */
	vaultPtr = (t_vault *) exPtr->vaultPtr;

	/* Should never happen */
	if (!vaultPtr)
		return -1;

	if ((y >= vaultPtr->y) && (y < vaultPtr->y + vaultPtr->height) &&
		(x >= vaultPtr->x) && (x < vaultPtr->x + vaultPtr->width))
	{
DLL_EXTVAR int g_symbol_count;
		PlaneDataGet(vaultPtr, y, x, VLT_PLANE_SYMBOL, &symbol);
		if (symbol >= g_symbol_count) return -1;
		return symbol;
	}

	return -1;
}

static CommandInit commandInit[] = {
 {0, "vault", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
  {1, "copy", 10, 10, "plane vaultId y1 x1 y2 x2 vaultId y3 x3", objcmd_vault_copy, (ClientData) 0},
  {1, "create", 0, 0, (char *) NULL, objcmd_vault_create, (ClientData) 0},
  {1, "current", 0, 2, "?vaultId?", objcmd_vault_current, (ClientData) 0},
  {1, "delete", 2, 2, "vaultId", objcmd_vault_delete, (ClientData) 0},
  {1, "get", 0, 0, "vaultId plane ?y1 x1? ?y2 x2?", objcmd_vault_get, (ClientData) 0},
  {1, "height", 2, 3, "vaultId ?height", objcmd_vault_height, (ClientData) 0},
  {1, "put", 0, 0, "vaultId plane data ?y1 x1? ?y2 x2?", objcmd_vault_put, (ClientData) 0},
  {1, "read", 3, 3, "vaultId file", objcmd_vault_read, (ClientData) 0},
  {1, "replace", 4, 8, "vaultId matchList replaceList ?y1 x1? ?y2 x2?", objcmd_vault_replace, (ClientData) 0},
  {1, "share", 4, 4, "plane vaultId vaultId", objcmd_vault_share, (ClientData) 0},
  {1, "swap", 3, 3, "vaultId vaultId", objcmd_vault_swap, (ClientData) 0},
  {1, "tocave", 2, 2, "vaultId", objcmd_vault_tocave, (ClientData) 0},
  {1, "width", 2, 3, "vaultId ?width?", objcmd_vault_width, (ClientData) 0},
  {1, "write", 3, 3, "vaultId file", objcmd_vault_write, (ClientData) 0},
 {0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

void vault_setup(void)
{
}

void vault_unload(void)
{
	while (g_vault_count)
		VaultFree(0);
	g_vault_id = 0;
}

void vault_init(void)
{
	g_vault = Array_New(0, sizeof(t_vault *));
	g_vault_count = 0;

	(void) CommandInfo_Init(g_interp, commandInit, NULL);
}

void vault_exit(void)
{
	vault_unload();
}
