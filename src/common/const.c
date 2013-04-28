/* File: const.c */

/* Purpose: interpreter access to constants */

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

enum {
CONST_TYPE_INT
};

#define CONST_INT(a) {sizeof(a) ? #a : "", CONST_TYPE_INT, (ClientData) a}

#define STRINGIFY2(x) (sizeof(x) ? #x : "")

typedef struct ConstType ConstType;
struct ConstType {
	char *name;
	int type;
	ClientData data;
	int size; /* length of array */
};

typedef Tcl_Obj *(*ConstGetProc)(Tcl_Interp *interp, ConstType *constPtr);

Tcl_Obj *const_get_int(Tcl_Interp *interp, ConstType *c)
{
	return Tcl_NewLongObj((long) c->data);
}

static ConstGetProc s_proc[] = {
	const_get_int
};

Tcl_HashTable g_const_hash;

int Const_FindByName(Tcl_Interp *interp, char *constName,
	Tcl_HashTable *tablePtr, ConstType **constPtrPtr)
{
	Tcl_HashEntry *hPtr;

	if ((hPtr = Tcl_FindHashEntry(tablePtr, constName)) == NULL)
	{
		/* Set the error */
		FormatResult(interp, "unknown const \"%s\"", constName);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return the field index */
	(*constPtrPtr) = (ConstType *) Tcl_GetHashValue(hPtr);

	/* Success */
	return TCL_OK;
}

int Const_FromObj(Tcl_Interp *interp, ConstType **constPtrPtr,
	Tcl_Obj *objPtr)
{
	char *t;

	t = Tcl_GetString(objPtr);
	if (Const_FindByName(interp, t, &g_const_hash, constPtrPtr) != TCL_OK)
	{
		return TCL_ERROR;
	}

	return TCL_OK;
}

int Const_Add(Tcl_Interp *interp, ConstType *typePtr)
{
	int new;
	Tcl_HashEntry *hPtr;

	hPtr = Tcl_CreateHashEntry(&g_const_hash, typePtr->name, &new);
	if (!new)
	{
		/* REPORT "duplicate const" */
		return TCL_ERROR;
	}
	Tcl_SetHashValue(hPtr, (ClientData) typePtr);

	return TCL_OK;
}

/*
 * const $name ?$index?
 */
int objcmd_const(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	Tcl_Obj *objPtr;
	ConstType *constPtr;

	if (Const_FromObj(interp, &constPtr, objV[1]) != TCL_OK)
	{
		return TCL_ERROR;
	}
	objPtr = (*s_proc[constPtr->type])(interp, constPtr);
	Tcl_SetObjResult(interp, objPtr);

	return TCL_OK;
}

static CommandInit commandInit[] = {
	{0, "const", 2, 3, "name ?index?", objcmd_const, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

int Const_Init(Tcl_Interp *interp)
{
	Tcl_InitHashTable(&g_const_hash, TCL_STRING_KEYS);
	(void) CommandInfo_Init(g_interp, commandInit, NULL);
	return TCL_OK;
}


static ConstType s_const_init[] = {
	CONST_INT(FEAT_FLOOR),
	CONST_INT(FEAT_OPEN),
	CONST_INT(FEAT_BROKEN),
	CONST_INT(FEAT_LESS),
	CONST_INT(FEAT_MORE),
	CONST_INT(FEAT_DOOR_HEAD),
	CONST_INT(FEAT_MAGMA),
	CONST_INT(FEAT_QUARTZ),
	CONST_INT(FEAT_MAGMA_K),
	CONST_INT(FEAT_QUARTZ_K),
	CONST_INT(FEAT_WALL_EXTRA),
	CONST_INT(FEAT_PERM_EXTRA),
	CONST_INT(FEAT_SHOP_HEAD),
	CONST_INT(FEAT_SHOP_TAIL),
	{"FEAT_HOME", CONST_TYPE_INT, (ClientData) (FEAT_SHOP_HEAD + STORE_HOME)},
	CONST_INT(INVEN_WIELD),
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
	CONST_INT(FEAT_TREES),
	CONST_INT(FEAT_GRASS),
	CONST_INT(FEAT_QUEST_ENTER),
	CONST_INT(FEAT_QUEST_EXIT),
	CONST_INT(FEAT_QUEST_DOWN),
	CONST_INT(FEAT_QUEST_UP),
	CONST_INT(FEAT_BLDG_HEAD),
	CONST_INT(FEAT_BLDG_TAIL),
	CONST_INT(PET_CLOSE_DIST),
	CONST_INT(PET_FOLLOW_DIST),
	CONST_INT(PET_SEEK_DIST),
	CONST_INT(PET_DESTROY_DIST),
	CONST_INT(PET_SPACE_DIST),
	CONST_INT(PET_AWAY_DIST),
#endif /* KANGBANDTK, ZANGBANDTK */
	{NULL, 0, NULL}
};

int init_const(Tcl_Interp *interp)
{
	int i;

	(void) Const_Init(interp);

	for (i = 0; s_const_init[i].name != NULL; i++)
	{
		if (Const_Add(interp, &s_const_init[i]) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	return TCL_OK;
}
