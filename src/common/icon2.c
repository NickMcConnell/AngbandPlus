/* File: icon2.c */

/* Purpose: more icon stuff */

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

char *AssignToString_Alternate(char *buf, t_assign *assign)
{
	(void) sprintf(buf, "alternate %d", assign->alternate.index);
	return buf;
}

int StringToAssign_Alternate(Tcl_Interp *interp, t_assign *assignPtr, char *desc)
{
	char option[64];
	int index;

	if (sscanf(desc, "%s %d", option, &index) != 2)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	if ((index < 0) || (index >= g_alternate_count))
	{
		/* Set the error */
		Tcl_SetResult(interp, format("bad alternate \"%d\": "
			"must be from 0 to %d", index, g_alternate_count - 1),
			TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	assignPtr->assignType = ASSIGN_TYPE_ALTERNATE;
	assignPtr->alternate.index = index;

	return TCL_OK;
}

char *AssignToString_Flavor(char *buf, t_assign *assignPtr)
{
	(void) sprintf(buf, "flavor %s %d",
		g_flavor[assignPtr->flavor.group].desc,
		assignPtr->flavor.index);
	return buf;
}

int StringToAssign_Flavor(Tcl_Interp *interp, t_assign *assignPtr, char *desc)
{
	char option[64], flavorName[64];
	int group, index;
	Tcl_HashEntry *hPtr;

	if (sscanf(desc, "%s %s %d", option, flavorName, &index) != 3)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	/* Lookup the flavor by name */
	hPtr = Tcl_FindHashEntry(&g_flavor_table, flavorName);

	/* The flavor was not found */
	if (hPtr == NULL)
	{
		/* Set the error */
		Tcl_SetResult(interp, format("unknown flavor \"%s\"", flavorName),
			TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the g_flavor[] index */
	group = (int) Tcl_GetHashValue(hPtr);

	/* Verify the flavor index */
	if ((index < 0) || (index >= g_flavor[group].count))
	{
		/* Set the error */
		Tcl_SetResult(interp,
			format("bad flavor index \"%d\": must be from 0 to %d",
			index, g_flavor[group].count - 1), TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	assignPtr->assignType = ASSIGN_TYPE_FLAVOR;
	assignPtr->flavor.group = group;
	assignPtr->flavor.index = index;

	return TCL_OK;
}

char *AssignToString_Icon(char *buf, t_assign *assign)
{
	if (assign->icon.ascii == -1)
	{
		(void) sprintf(buf, "icon %s %d",
			g_icon_type[assign->icon.type].desc,
			assign->icon.index);
	}
	else
	{
		(void) sprintf(buf,"icon %s %d %d",
			g_icon_type[assign->icon.type].desc,
			assign->icon.index, assign->icon.ascii);
	}

	return buf;
}

int StringToAssign_Icon(Tcl_Interp *interp, t_assign *assignPtr, char *desc)
{
	char option[64], typeName[64];
	IconSpec iconSpec;

	iconSpec.ascii = -1;
	if (sscanf(desc, "%s %s %d %d", option, typeName, &iconSpec.index,
		&iconSpec.ascii) < 3)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	if (Icon_Validate(interp, typeName, iconSpec.index, iconSpec.ascii,
		&iconSpec) != TCL_OK)
	{
		return TCL_ERROR;
	}

	assignPtr->assignType = ASSIGN_TYPE_ICON;
	assignPtr->icon.type = iconSpec.type;
	assignPtr->icon.index = iconSpec.index;
	assignPtr->icon.ascii = iconSpec.ascii;

	return TCL_OK;
}

char *AssignToString_Sprite(char *buf, t_assign *assign)
{
	(void) sprintf(buf, "sprite %d", assign->sprite.index);
	return buf;
}

int StringToAssign_Sprite(Tcl_Interp *interp, t_assign *assignPtr, char *desc)
{
	char option[64];
	int index;

	if (sscanf(desc, "%s %d", option, &index) != 2)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	if ((index < 0) || (index >= g_sprite_count))
	{
		/* Set the error */
		Tcl_SetResult(interp, format("bad sprite \"%d\": "
			"must be from 0 to %d", index, g_sprite_count - 1),
			TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	assignPtr->assignType = ASSIGN_TYPE_SPRITE;
	assignPtr->sprite.index = index;

	return TCL_OK;
}

CONST char *keyword_assign_type[] = {"alternate", "flavor", "icon", "sprite", NULL};

/* char* -> t_assign */
typedef char *(*AssignToStringProc)(char *buf, t_assign *assign);
AssignToStringProc gAssignToStringProc[ASSIGN_TYPE_MAX] = {
	AssignToString_Alternate,
	AssignToString_Flavor,
	AssignToString_Icon,
	AssignToString_Sprite
};

/* t_assign -> char* */
typedef int (*StringToAssignProc)(Tcl_Interp *interp, t_assign *assignPtr, char *desc);
StringToAssignProc gStringToAssignProc[ASSIGN_TYPE_MAX] = {
	StringToAssign_Alternate,
	StringToAssign_Flavor,
	StringToAssign_Icon,
	StringToAssign_Sprite
};

int assign_parse(Tcl_Interp *interp, t_assign *assignPtr, char *desc)
{
	char option[64];
	Tcl_Obj *objPtr;
	int assignType;

	/* Ex. "icon dungeon 10" or "icon ascii 12 10" or "sprite 2" */
	if (sscanf(desc, "%s", option) != 1)
	{
		Tcl_SetResult(interp, format("malformed assignment \"%s\"",
			desc), TCL_VOLATILE);
		return TCL_ERROR;
	}

	objPtr = Tcl_NewStringObj(option, -1);
	if (Tcl_GetIndexFromObj(interp, objPtr, keyword_assign_type,
		"option", 0, &assignType) != TCL_OK)
	{
		Tcl_DecrRefCount(objPtr);
		return TCL_ERROR;
	}
	Tcl_DecrRefCount(objPtr);

	return (*gStringToAssignProc[assignType])(interp, assignPtr, desc);
}

char *assign_print(char *buf, t_assign *assignPtr)
{
	return (*gAssignToStringProc[assignPtr->assignType])(buf, assignPtr);
}

char *assign_print2(char *buf, int assignType, int assignIndex)
{
	t_assign *assignPtr = &g_assign[assignType].assign[assignIndex];
	return assign_print(buf, assignPtr);
}

char *assign_print_object(char *buf, object_type *o_ptr)
{
	t_assign assign;
	get_object_assign(&assign, o_ptr);
	return assign_print(buf, &assign);
}

char *assign_print_monster(char *buf, monster_type *m_ptr)
{
	t_assign assign;
	get_monster_assign(&assign, m_ptr);
	return assign_print(buf, &assign);
}

/*
 * Get the assignment for the given object. Handle "empty" objects and
 * resolve alternate assignments.
 */
void get_object_assign(t_assign *assignPtr, object_type *o_ptr)
{
	t_assign assign;

	if (o_ptr->k_idx)
	{
		assign = g_assign[ASSIGN_OBJECT].assign[o_ptr->k_idx];
		if (assign.assignType == ASSIGN_TYPE_ALTERNATE)
		{
			t_alternate *alternatePtr = &g_alternate[assign.alternate.index];
			int index = 0;
			switch (alternatePtr->reason)
			{
				case REASON_NUMBER:
					if (o_ptr->number == 1) ++index;
					break;
				
				case REASON_IDENT:
					if (object_known_p(o_ptr)) ++index;
					break;
			}					
			assign.assignType = ASSIGN_TYPE_ICON;
			assign.icon.type = alternatePtr->icon[index].type;
			assign.icon.index = alternatePtr->icon[index].index;
			assign.icon.ascii = alternatePtr->icon[index].ascii;
		}
	}
	else
	{
		/*
		 * Use ICON_TYPE_NONE icon. This is needed because the "pile" icon is
		 * assigned to object zero.
		 */
		assign.assignType = ASSIGN_TYPE_ICON;
		assign.icon.type = ICON_TYPE_NONE;
		assign.icon.index = 0;
		assign.icon.ascii = -1;
	}

	(*assignPtr) = assign;
}

/*
 * Get the assignment for the given object. Resolve alternate assignments.
 */
void get_monster_assign(t_assign *assignPtr, monster_type *m_ptr)
{
	t_assign assign;

	if (m_ptr->r_idx > 0)
	{
		assign = g_assign[ASSIGN_MONSTER].assign[m_ptr->r_idx];
		if (assign.assignType == ASSIGN_TYPE_ALTERNATE)
		{
			t_alternate *alternatePtr = &g_alternate[assign.alternate.index];
			int index = 0;
			switch (alternatePtr->reason)
			{
				case REASON_FRIEND:
					if (monster_is_friend(m_ptr)) ++index;
					break;
			}					
			assign.assignType = ASSIGN_TYPE_ICON;
			assign.icon.type = alternatePtr->icon[index].type;
			assign.icon.index = alternatePtr->icon[index].index;
			assign.icon.ascii = alternatePtr->icon[index].ascii;
		}
		if (assign.assignType == ASSIGN_TYPE_ICON)
		{
			if (assign.icon.ascii != -1 &&
				g_ascii[assign.icon.ascii].altFriend != -1 &&
				monster_is_friend(m_ptr))
			{
				assign.icon.ascii = g_ascii[assign.icon.ascii].altFriend;
			}
		}
	}
	else
	{
		/*
		 * Use ICON_TYPE_DEFAULT icon for errors.
		 */
		assign.assignType = ASSIGN_TYPE_ICON;
		assign.icon.type = ICON_TYPE_DEFAULT;
		assign.icon.index = 0;
		assign.icon.ascii = -1;
	}

	(*assignPtr) = assign;
}

/*
 * Set the interpreter result with an error if the given index is
 * outside the g_alternate[] array bounds.
 */
static int ValidateAlternate(Tcl_Interp *interp, int alternateIndex)
{
	/* Verify the alternate index */
	if ((alternateIndex < 0) || (alternateIndex >= g_alternate_count))
	{
		/* Set the error */
		FormatResult(interp, "bad alternate \"%d\": "
			"must be from 0 to %d", alternateIndex,
			g_alternate_count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	/* Sucess */
	return TCL_OK;
}

/*
 * Set the interpreter result with an error if the given index specifies
 * an invalid frame in the given alternate. The alternate must be valid.
 */
static int ValidateFrameAlternate(Tcl_Interp *interp, int alternateIndex,
	int frameIndex)
{
	/* Access the alternate */
	t_alternate *alternatePtr = &g_alternate[alternateIndex];

	/* Verify the frame index */
	if ((frameIndex < 0) || (frameIndex >= alternatePtr->count))
	{
		/* Set the error */
		FormatResult(interp, "bad frame \"%d\" on alternate \"%d\": "
			"must be from 0 to %d", frameIndex, alternateIndex,
			alternatePtr->count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

cptr keyword_alternate_reason[] = {
	"none", "number", "ident", "feature", "friend", NULL
};

int AlternateFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr,
	t_alternate **alternatePtrPtr, int *alternateIndexPtr)
{
	int alternateIndex;

	/* Get the alternate index */
	if (Tcl_GetIntFromObj(interp, objPtr, &alternateIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the alternate index */
	if (ValidateAlternate(interp, alternateIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Access the alternate */
	(*alternatePtrPtr) = &g_alternate[alternateIndex];

	if (alternateIndexPtr)
		(*alternateIndexPtr) = alternateIndex;

	/* Success */
	return TCL_OK;
}

int AlternateFrameFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr,
	int alternateIndex, int *frameIndexPtr)
{
	int frameIndex;

	/* Get the frame index */
	if (Tcl_GetIntFromObj(interp, objPtr, &frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the frame index */
	if (ValidateFrameAlternate(interp, alternateIndex, frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	(*frameIndexPtr) = frameIndex;

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) assign alternateIndex frameIndex -type iconType -index iconIndex
 */
int objcmd_alternate_assign(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_alternate *alternate_ptr;
	int alternateIndex, frameIndex;
	IconSpec iconSpec;

	/* Get the alternate */
	if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the frame index */
	if (AlternateFrameFromObj(interp, objV[2], alternateIndex, &frameIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Scan the arguments for icon type and index */
	if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
		!= TCL_OK)
	{
		/* Failure */
		return TCL_ERROR;
	}

	/* Set the frame */
	alternate_ptr->icon[frameIndex] = iconSpec;

	if (alternate_ptr->reason == REASON_FEATURE)
	{
		g_icon_map_changed = TRUE;
	}

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) create reason
 */
int objcmd_alternate_create(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int index;
	t_alternate *alternate_ptr;

	if (Tcl_GetIndexFromObj(interp, objV[1],
		keyword_alternate_reason,
		"reason", 0, &index) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Insert a new alternate into the global array */
	g_alternate = Array_Insert(g_alternate, &g_alternate_count,
		sizeof(t_alternate), g_alternate_count);

	/* Access the alternate */
	alternate_ptr = &g_alternate[g_alternate_count - 1];

	/* Set the fields, allocate empty frames array */
	alternate_ptr->reason = index;
	alternate_ptr->count = 0;
	alternate_ptr->icon = Array_New(0, sizeof(IconSpec));

	/* Return the index of the new alternate */
	IntResult(interp, g_alternate_count - 1);

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) delete alternateIndex ?frameIndex?
 */
int objcmd_alternate_delete(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_alternate *alternate_ptr;
	int alternateIndex, frameIndex;

	/* Get the alternate */
	if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* A frame index argument was given */
	if (objC == 3)
	{
		/* Get the frame index */
		if (AlternateFrameFromObj(interp, objV[2], alternateIndex, &frameIndex)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Delete the frame */
		alternate_ptr->icon = Array_Delete(alternate_ptr->icon,
			&alternate_ptr->count, sizeof(IconSpec), frameIndex);

		/* Done */
		return TCL_OK;
	}

	/* Free the frame array */
	Tcl_Free((char *) alternate_ptr->icon);

	/* Delete the alternate from the global array */
	g_alternate = Array_Delete(g_alternate,
		&g_alternate_count, sizeof(t_alternate), alternateIndex);

	/***
	 *** Now we must go through all the possible assignments and
	 *** update any references to the deleted alternate or any of the
	 *** moved alternates. NOT IMPLEMENTED
	 ***/

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) get alternateIndex ?frameIndex?
 */
int objcmd_alternate_get(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_alternate *alternate_ptr;
	int alternateIndex, frameIndex;
	Tcl_Obj *listObjPtr;
	IconSpec iconSpec;

	/* Get the alternate */
	if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* A frame index argument was given */
	if (objC == 3)
	{
		/* Get the frame index */
		if (AlternateFrameFromObj(interp, objV[2], alternateIndex, &frameIndex)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Create a new Tcl list object */
		listObjPtr = Tcl_NewListObj(0, NULL);

		/* Get the icon for the frame */
		iconSpec = alternate_ptr->icon[frameIndex];
		
		/* Append icon description to the list */
		if (iconSpec.ascii == -1)
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(format("%s %d",
					g_icon_type[iconSpec.type].desc, iconSpec.index), -1));
		}
		else
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(format("%s %d %d",
					g_icon_type[iconSpec.type].desc, iconSpec.index,
					iconSpec.ascii), -1));
		}

		/* Return the list object */
		Tcl_SetObjResult(interp, listObjPtr);

		/* Done */
		return TCL_OK;
	}

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check each frame */
	for (frameIndex = 0; frameIndex < alternate_ptr->count; frameIndex++)
	{
		/* Get the icon for the frame */
		iconSpec = alternate_ptr->icon[frameIndex];
		
		/* Append icon description to the list */
		if (iconSpec.ascii == -1)
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(format("%s %d",
					g_icon_type[iconSpec.type].desc, iconSpec.index), -1));
		}
		else
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(format("%s %d %d",
					g_icon_type[iconSpec.type].desc, iconSpec.index,
					iconSpec.ascii), -1));
		}
	}

	/* Return the list object */
	Tcl_SetObjResult(interp, listObjPtr);

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) count ?alternateIndex?
 */
int objcmd_alternate_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_alternate *alternate_ptr;
	int alternateIndex;

	/* An alternate index was given */
	if (objC == 2)
	{
		/* Get the alternate */
		if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Return the number of frames in the alternate */
		IntResult(interp, alternate_ptr->count);

		/* Done */
		return TCL_OK;
	}

	/* Return total number of alternates */
	IntResult(interp, g_alternate_count);

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) configure alternateIndex ?option? ?value? ?option value ...?
 */
int objcmd_alternate_configure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *configSwitch[] = {"-reason", NULL};
	t_alternate *alternate_ptr;
	int alternateIndex, index;
	Tcl_Obj *CONST *objPtr;

	/* Get the alternate */
	if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* "alternate configure $alternateIndex" */
	/* Return all configuration options */
	if (objC == 2)
	{
		/* NOT IMPLEMENTED */
		return TCL_OK;
	}

	/* "alternate configure $alternateIndex $option" */
	/* Return the value of a single option */
	if (objC == 3)
	{
		if (Tcl_GetIndexFromObj(interp, objV[2], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
		}

		switch (index)
		{
			case 0: /* -reason */
				StaticResult(interp, 
					(char *) keyword_alternate_reason[alternate_ptr->reason]);
				break;
		}

		/* Done */
		return TCL_OK;
	}

	/* Access the first option/value argument */
	objPtr = objV + 2;
	objC -= 2;

	/* Require even number of arguments */
	if (objC & 1)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv,
			"alternateIndex option value ?option value ...?");
		return TCL_ERROR;
	}

	/* Scan all option/value pairs */
	while (objC > 1)
	{
		if (Tcl_GetIndexFromObj(interp, objPtr[0], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
		}

		switch (index)
		{
			case 0: /* -reason */
				if (Tcl_GetIndexFromObj(interp, objPtr[1],
					keyword_alternate_reason,
					"reason", 0, &alternate_ptr->reason) != TCL_OK)
				{
					return TCL_ERROR;
				}
				break;
		}

		/* Next option/value pair */
		objPtr += 2;
		objC -= 2;
	}

	/* Success */
	return TCL_OK;
}

/*
 * (alternate) insert alternateIndex frameIndex ?-type iconType -index iconIndex?
 */
int objcmd_alternate_insert(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_alternate *alternate_ptr;
	int alternateIndex, frameIndex;
	IconSpec iconSpec;

	/* Get the alternate */
	if (AlternateFromObj(interp, objV[1], &alternate_ptr, &alternateIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the desired frame index */
	if (Tcl_GetIntFromObj(interp, objV[2], &frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Scan the arguments for icon type and index */
	if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
		!= TCL_OK)
	{
		/* Failure */
		return TCL_ERROR;
	}

	/* Append */
	if (frameIndex > alternate_ptr->count)
	{
		frameIndex = alternate_ptr->count;
	}

	/* Prepend */
	else if (frameIndex < 0)
	{
		frameIndex = 0;
	}

	/* Insert a frame */
	alternate_ptr->icon = Array_Insert(alternate_ptr->icon,
		&alternate_ptr->count, sizeof(IconSpec), frameIndex);

	/* Set the frame */
	alternate_ptr->icon[frameIndex] = iconSpec;

	/* Success */
	return TCL_OK;
}

/* (assign) groups */
int objcmd_assign_groups(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	int i;
	Tcl_Obj *listObjPtr;

	listObjPtr = Tcl_NewListObj(0, NULL);
	
	for (i = 0; i < ASSIGN_MAX; i++)
	{
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj(keyword_assign[i], -1));
	}

	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;
}

/* (assign) types */
int objcmd_assign_types(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	int i;
	Tcl_Obj *listObjPtr;

	listObjPtr = Tcl_NewListObj(0, NULL);
	
	for (i = 0; i < ASSIGN_TYPE_MAX; i++)
	{
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj(keyword_assign_type[i], -1));
	}

	Tcl_SetObjResult(interp, listObjPtr);

	return TCL_OK;
}

/*
 * (assign) set $group $member ?$assign?
 */
int objcmd_assign_set(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	int group, member;
	t_assign *assignPtr;
	char buf[128];

	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_assign,
		"group", 0, &group) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIntFromObj(interp, objV[2], &member) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if ((member < 0) || (member >= g_assign[group].count))
	{
		Tcl_SetObjResult(interp,
			Tcl_NewStringObj(format("bad member \"%d\" : "
			"must be from 0 to %d", member, g_assign[group].count - 1), -1));
		return TCL_ERROR;
	}
	assignPtr = &g_assign[group].assign[member];

	/* Return the current assignment */
	if (objC == 3)
	{
		(void) assign_print(buf, assignPtr);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
		return TCL_OK;
	}

	if (assign_parse(interp, assignPtr, Tcl_GetString(objV[3])) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/*
	 * XXX Hack -- If we assigned a sprite to a feature, we are
	 * going to set the lighting mode for that feature to "none".
	 * Otherwise the get_display_info() will give back an invalid
	 * icon. We could set the lighting mode to FT_LIGHT_TINT.
	 */
	if ((group == ASSIGN_FEATURE) &&
		(assignPtr->assignType == ASSIGN_TYPE_SPRITE) &&
		g_feat_lite[member])
	{
		g_feat_lite[member] = FT_LIGHT_NONE;
	}

	g_icon_map_changed = TRUE;

	/* Fire off an event to inform the world of the assignment */
	Bind_Assign(group + 1, member, assignPtr);

	return TCL_OK;
}

/* (assign) toicon $assign */
int objcmd_assign_toicon(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	char buf[128], *t;
	IconSpec iconSpec;
	t_assign assign;

	t = Tcl_GetString(objV[1]);
	if (assign_parse(interp, &assign, t) != TCL_OK)
	{
		return TCL_ERROR;
	}

	FinalIcon(&iconSpec, &assign, 0, NULL, NULL);
	(void) AssignToString_Icon(buf, &assign);
	Tcl_SetResult(interp, buf + 5, TCL_VOLATILE);

	return TCL_OK;
}

/* (assign) validate $assign */
int objcmd_assign_validate(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	char *t;
	t_assign assign;

	t = Tcl_GetString(objV[1]);
    return assign_parse(interp, &assign, t);
}

/* (assign) count $group */
int objcmd_assign_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	int group;

    if (Tcl_GetIndexFromObj(interp, objV[1], keyword_assign,
    	"option", 0, &group) != TCL_OK)
	{
		return TCL_ERROR;
    }
    Tcl_SetObjResult(interp, Tcl_NewIntObj(g_assign[group].count));

	/* Success */
	return TCL_OK;
}

/* Indexes are EFFECT_SPELL_XXX constants */
cptr keyword_effect_spell[] = {
"arrow",
"missile",
"mana",
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
"holy_orb",
#endif /* ANGBANDTK, KANGBANDTK */
"light_weak",
"dark_weak",
"water",
"plasma",
"meteor",
"ice",
"gravity",
"inertia",
"force",
"time",
"acid",
"electricity",
"fire",
"cold",
"poison",
"light",
"dark",
"confusion",
"sound",
"shard",
"nexus",
"nether",
"chaos",
"disenchant",
#if defined(ZANGBANDTK)
"rocket",
"nuke",
"death_ray",
"holy_fire",
"hell_fire",
"disintegrate",
"psi",
"psi_drain",
"telekenesis",
"domination",
#endif /* ZANGBANDTK */
NULL
};

/* Indexes are EFFECT_AMMO_XXX constants */
cptr keyword_effect_ammo[] = {
"arrow",
"bolt",
NULL
};

cptr keyword_effect_group[] = {"ball", "bolt", "ammo", NULL};

/*
 * (effect) assign $group $effect ?-type iconType -index iconIndex?
 */
int objcmd_effect_assign(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_effect *effect_ptr;
	int effectIndex, effectType;
	IconSpec iconSpec;
	t_assign assign;
	char buf[128];

	/* Get the effect type */
	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_effect_group,
		"type", 0, &effectType) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Access the effect */
	effect_ptr = &g_effect[effectType];

	/* Get the effect keyword */
	if (Tcl_GetIndexFromObj(interp, objV[2], (CONST char **) effect_ptr->name,
		"effect", 0, &effectIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* */
	if (objC > 3)
	{
		/* Scan the arguments for icon type and index */
		if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Set the icon */
		effect_ptr->icon[effectIndex] = iconSpec;

		/* Success */
		return TCL_OK;;
	}

	/* Get the icon */
	iconSpec = effect_ptr->icon[effectIndex];

	/* Return an icon description */
	assign.assignType = ASSIGN_TYPE_ICON;
	assign.icon.type = iconSpec.type;
	assign.icon.index = iconSpec.index;
	assign.icon.ascii = iconSpec.ascii;
	Tcl_SetResult(interp, assign_print(buf, &assign), TCL_VOLATILE);

	/* Success */
	return TCL_OK;;
}

int effect_to_gf[EFFECT_SPELL_MAX] = {
GF_ARROW, /* EFFECT_SPELL_ARROW, */
GF_MISSILE, /* EFFECT_SPELL_MISSILE, */
GF_MANA, /* EFFECT_SPELL_MANA, */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
GF_HOLY_ORB, /* EFFECT_SPELL_HOLY_ORB, */
#endif /* ANGBANDTK, KANGBANDTK */
GF_LITE_WEAK, /* EFFECT_SPELL_LITE_WEAK, */
GF_DARK_WEAK, /* EFFECT_SPELL_DARK_WEAK, */
GF_WATER, /* EFFECT_SPELL_WATER, */
GF_PLASMA, /* EFFECT_SPELL_PLASMA, */
GF_METEOR, /* EFFECT_SPELL_METEOR, */
GF_ICE, /* EFFECT_SPELL_ICE, */
GF_GRAVITY, /* EFFECT_SPELL_GRAVITY, */
GF_INERTIA, /* EFFECT_SPELL_INERTIA, */
GF_FORCE, /* EFFECT_SPELL_FORCE, */
GF_TIME, /* EFFECT_SPELL_TIME, */
GF_ACID, /* EFFECT_SPELL_ACID, */
GF_ELEC, /* EFFECT_SPELL_ELEC, */
GF_FIRE, /* EFFECT_SPELL_FIRE, */
GF_COLD, /* EFFECT_SPELL_COLD, */
GF_POIS, /* EFFECT_SPELL_POIS, */
GF_LITE, /* EFFECT_SPELL_LITE, */
GF_DARK, /* EFFECT_SPELL_DARK, */
GF_CONFUSION, /* EFFECT_SPELL_CONFUSION, */
GF_SOUND, /* EFFECT_SPELL_SOUND, */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
GF_SHARD, /* EFFECT_SPELL_SHARD, */
#endif
#if defined(ZANGBANDTK)
GF_SHARDS, /* EFFECT_SPELL_SHARD, */
#endif
GF_NEXUS, /* EFFECT_SPELL_NEXUS, */
GF_NETHER, /* EFFECT_SPELL_NETHER, */
GF_CHAOS, /* EFFECT_SPELL_CHAOS, */
GF_DISENCHANT, /* EFFECT_SPELL_DISENCHANT, */
#if defined(ZANGBANDTK)
GF_ROCKET, /* EFFECT_SPELL_ROCKET, */
GF_NUKE, /* EFFECT_SPELL_NUKE, */
GF_DEATH_RAY, /* EFFECT_SPELL_DEATH_RAY, */
GF_HOLY_FIRE, /* EFFECT_SPELL_HOLY_FIRE, */
GF_HELL_FIRE, /* EFFECT_SPELL_HELL_FIRE, */
GF_DISINTEGRATE, /* EFFECT_SPELL_DISINTEGRATE, */
GF_PSI, /* EFFECT_SPELL_PSI, */
GF_PSI_DRAIN, /* EFFECT_SPELL_PSI_DRAIN, */
GF_TELEKINESIS, /* EFFECT_SPELL_TELEKINESIS, */
GF_DOMINATION /* EFFECT_SPELL_DOMINATION, */
#endif /* ZANGBANDTK */
};

extern byte spell_color(int type);

/*
 * (effect) color $effect
 */
int objcmd_effect_color(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth;*/
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int effectIndex;
	int gf;
	byte a;
#if defined(ZANGBANDTK)
	cptr s;
#endif

	/* Get the effect keyword */
	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_effect_spell,
		"effect", 0, &effectIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	gf = effect_to_gf[effectIndex];
#if defined(ZANGBANDTK)
	s = quark_str(gf_color[gf]);
	if (s)
	{
		a = strchr(color_char, s[0]) - color_char;
		if (a > 15)
			a = TERM_WHITE;
	}
	else
		a = TERM_WHITE;
#else
	a = spell_color(gf);
#endif
	IntResult(interp, a);

	return TCL_OK;
}

/*
 * (effect) groups
 */
int objcmd_effect_groups(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
/*	CommandInfo *infoCmd = (CommandInfo *) clientData; */
/*	int objC = objc - infoCmd->depth; */
/*	Tcl_Obj *CONST *objV = objv + infoCmd->depth; */

	int index;
	Tcl_Obj *listObjPtr;

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check each effect keyword */
	for (index = 0; index < EFFECT_MAX; index++)
	{
		/* Append effect keyword as a string object to the list*/
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj((char *) keyword_effect_group[index], -1));
	}

	/* Return the list */
	Tcl_SetObjResult(interp, listObjPtr);

	/* Success */
	return TCL_OK;;
}

/*
 * (effect) names $group
 */
int objcmd_effect_names(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_effect *effect_ptr;
	int effectType, index;
	Tcl_Obj *listObjPtr;

	/* Get the effect type */
	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_effect_group,
		"type", 0, &effectType) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Access the effect */
	effect_ptr = &g_effect[effectType];

	/* Create a new Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check each effect keyword */
	for (index = 0; effect_ptr->name[index]; index++)
	{
		/* Append effect keyword as a string object to the list*/
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj(effect_ptr->name[index], -1));
	}

	/* Return the list */
	Tcl_SetObjResult(interp, listObjPtr);

	/* Success */
	return TCL_OK;;
}

cptr keyword_feat_lite[] = {"none", "icon", "tint", NULL};

int FeatFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int *f_idx)
{
	/* Get the f_info[] index */
	if (Tcl_GetIntFromObj(interp, objPtr, f_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the feature index */
	if ((*f_idx < 0) || (*f_idx >= MAX_F_IDX))
	{
		/* Set the error */
		FormatResult(interp,
			"bad f_info index \"%d\": must be between 0 and %d",
			*f_idx, (int) MAX_F_IDX - 1);
	
		/* Failure */
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

/*
 * (feature) assignshape f_idx shape ?assign?
 */
int objcmd_feature_assignshape(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int shape;
	char *t;
	int f_idx;
	t_assign assign;

	/* Get the f_info[] index */
	if (FeatFromObj(interp, objV[1], &f_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

    if (Tcl_GetIndexFromObj(interp, objV[2], keyword_wall,
		"shape", 0, &shape) != TCL_OK)
	{
		return TCL_ERROR;
    }

    if (objC == 3)
    {
		char buf[128];
		(void) assign_print(buf, &g_assignshape[shape][f_idx]);
		Tcl_SetResult(interp, buf, TCL_VOLATILE);
		return TCL_OK;
    }

	t = Tcl_GetString(objV[3]);
	if (assign_parse(interp, &assign, t) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (objC == 5)
		g_assignshape[shape][MAX_F_IDX + f_idx] = assign;
	else
		g_assignshape[shape][f_idx] = assign;
	g_icon_map_changed = TRUE;

	g_assignshape_inuse = TRUE;

	return TCL_OK;
}

/*
 * (feature) configure f_idx ?option value?
 */
int objcmd_feature_configure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *configSwitch[] = {"-background", "-light", "-boring",
		"-town", NULL};
	Tcl_Obj *CONST *objPtr;
	int f_idx, i, index;

	/* Get the f_info[] index */
	if (FeatFromObj(interp, objV[1], &f_idx) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* "feature configure $index" */
	/* Return all configuration options */
	if (objC == 2)
	{
		/* NOT IMPLEMENTED */
		return TCL_OK;
	}

	/* "feature configure $index $option */
	/* Return the value of a single option */
	if (objC == 3)
	{
	    if (Tcl_GetIndexFromObj(interp, objV[2], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }

		switch (index)
		{
			case 0: /* -background */
				IntResult(interp, g_background[f_idx]);
				break;
				
			case 1: /* -light */
				StaticResult(interp, 
					(char *) keyword_feat_lite[g_feat_lite[f_idx]]);
				break;
				
			case 2: /* -boring */
				BooleanResult(interp, g_feat_flag[f_idx] & FEAT_FLAG_BORING);
				break;
				
			case 3: /* -town */
				BooleanResult(interp, g_feat_flag[f_idx] & FEAT_FLAG_TOWN);
				break;
		}

		/* Done */
		return TCL_OK;
	}

	/* Point to the first option/value pair */
	objPtr = objV + 2;
	objC -= 2;

	/* Required number of arguments */
	if (objC & 1)
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv,
			"f_idx ?option? ?value? ?option value ...?");
	
		/* Failure */
		return TCL_ERROR;
	}

	/* Scan all option/value pairs */
	while (objC > 1)
	{
	    if (Tcl_GetIndexFromObj(interp, objPtr[0], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }

		switch (index)
		{
			case 0: /* -background */
				if (FeatFromObj(interp, objPtr[1], &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				g_background[f_idx] = index;
				g_icon_map_changed = TRUE;
				break;
				
			case 1: /* -light */
				if (Tcl_GetIndexFromObj(interp, objPtr[1],
					keyword_feat_lite, "option", 0,
					&g_feat_lite[f_idx]) != TCL_OK)
				{
					return TCL_ERROR;
				}
				break;
				
			case 2: /* -boring */
				if (Tcl_GetBooleanFromObj(interp, objPtr[1], &i) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (i)
				{
					g_feat_flag[f_idx] |= FEAT_FLAG_BORING;
				}
				else
				{
					g_feat_flag[f_idx] &= ~(FEAT_FLAG_BORING);
				}
				break;
				
			case 3: /* -town */
				if (Tcl_GetBooleanFromObj(interp, objPtr[1], &i) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (i)
				{
					g_feat_flag[f_idx] |= FEAT_FLAG_TOWN;
				}
				else
				{
					g_feat_flag[f_idx] &= ~(FEAT_FLAG_TOWN);
				}
				break;
		}

		/* Next option/value pair */
		objPtr += 2;
		objC -= 2;
	}

	/* Success */
	return TCL_OK;
}

/*
 * (feature) torchlite ?boolean?
 */
int objcmd_feature_torchlite(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	/* New value given */
	if (objC == 2)
	{
		if (Tcl_GetBooleanFromObj(interp, objV[1], &g_torchlite) != TCL_OK)
		{
			return TCL_ERROR;
		}
	}

	/* Return the current value */
	BooleanResult(interp, g_torchlite);

	/* Success */
	return TCL_OK;
}

/*
 * (feature) torch paletteIndex opacity
 */
int objcmd_feature_torch(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int tint, opacity;

	/* Get the palette index */
	if (Tcl_GetIntFromObj(interp, objV[1], &tint) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* VERIFY tint */
	
	/* Get the opacity */
	if (Tcl_GetIntFromObj(interp, objV[2], &opacity) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* VERIFY opacity */

	/* Set the tint table */
	Palette_TintTable(tint, opacity, g_yellow);

	/* Success */
	return TCL_OK;
}

/* (flavor) assign $group $index ?-type $type -index $index -ascii $ascii? */
int
objcmd_flavor_assign(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *flavorName;
	int flavor, flavorIndex;
	IconSpec iconSpec;
	Tcl_HashEntry *hPtr;

	/* Get the specified flavor name */
	flavorName = Tcl_GetStringFromObj(objV[1], NULL);

	/* Lookup the flavor by name */
	hPtr = Tcl_FindHashEntry(&g_flavor_table, flavorName);

	/* The flavor was not found */
	if (hPtr == NULL)
	{
		/* Set the error */
		Tcl_SetResult(interp, format("unknown flavor \"%s\"", flavorName),
			TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the g_flavor[] index */
	flavor = (int) Tcl_GetHashValue(hPtr);

	/* Get the desired flavor index */
	if (Tcl_GetIntFromObj(interp, objV[2], &flavorIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the flavor index */
	if ((flavorIndex < 0) || (flavorIndex >= g_flavor[flavor].count))
	{
		/* Set the error */
		Tcl_SetResult(interp, format("bad flavor index \"%d\": must be from 0 to %d",
			flavorIndex, g_flavor[flavor].count - 1), TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	/* Return icon assigned to that flavor index */
	if (objC == 3)
	{
		iconSpec = g_flavor[flavor].icon[flavorIndex];
		if (iconSpec.ascii == -1)
		{
			Tcl_SetResult(interp, format("%s %d",
				g_icon_type[iconSpec.type].desc, iconSpec.index),
				TCL_VOLATILE);
		}
		else
		{
			Tcl_SetResult(interp, format("%s %d %d",
				g_icon_type[iconSpec.type].desc, iconSpec.index,
				iconSpec.ascii), TCL_VOLATILE);
		}

		/* Success */
		return TCL_OK;
	}

	if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Finally, assign the icon */
	g_flavor[flavor].icon[flavorIndex] = iconSpec;

	/* Success */
	return TCL_OK;
}

/* (flavor) count $group */
int
objcmd_flavor_count(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *flavorName;
	int flavor;
	Tcl_HashEntry *hPtr;

	/* Get the specified flavor name */
	flavorName = Tcl_GetStringFromObj(objV[1], NULL);

	/* Lookup the flavor by name */
	hPtr = Tcl_FindHashEntry(&g_flavor_table, flavorName);

	/* The flavor was not found */
	if (hPtr == NULL)
	{
		/* Set the error */
		Tcl_SetResult(interp, format("unknown flavor \"%s\"", flavorName),
			TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	/* Get the g_flavor[] index */
	flavor = (int) Tcl_GetHashValue(hPtr);

	Tcl_SetObjResult(interp, Tcl_NewIntObj(g_flavor[flavor].count));

	return TCL_OK;
}

/*
 * This command allows us to adjust the tint tables used to darken
 * feature icons according to light radius (see get_display_info()).
 * To specify that a feature should use a tint table, call the "feature
 * configure" command with the "-light tint" option. Using tint tables
 * slows that game down somewhat, so it is usually desireable to
 * use icons for lighting, via "feature configure -light icon".
 *
 * The tint tables adjusted by this routine are in fact initialized
 * using the tk/config/dark file. The values in that file were created
 * on a Macintosh using a somewhat complicated procedure.
 */
int
objcmd_lighting(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int index;
	Tcl_Obj *CONST *objPtr;

	int brightness, contrast, radius;
	int do_brightness = 0, do_contrast = 0, do_gamma = 0;
	double gamma;
	t_darken *dark_ptr;

	/* The radius argument must be given */
	radius = -1;
	
	/* Point to the first option/value pair */
	objPtr = objV + 1;
	objC -= 1;

	/* Scan all option/value pairs */
	while (objC > 1)
	{
		static CONST char *lightOption[] = {"-brightness", "-contrast",
			"-gamma", "-radius", NULL};
			
	    if (Tcl_GetIndexFromObj(interp, objPtr[0], lightOption,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }
	    
		switch (index)
		{
			case 0: /* -brightness */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &brightness)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((brightness < -127) || (brightness > 127))
				{
					/* Set the error */
					FormatResult(interp, "bad brightness \"%d\":"
						" must be from -127 to +127", brightness);

					/* Failure */
					return TCL_ERROR;
				}
				do_brightness = 1;
				break;
				
			case 1: /* -contrast */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &contrast)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((contrast < -127) || (contrast > 127))
				{
					/* Set the error */
					FormatResult(interp, "bad contrast \"%d\":"
						" must be from -127 to +127", contrast);

					/* Failure */
					return TCL_ERROR;
				}
				do_contrast = 1;
				break;
				
			case 2: /* -gamma */
				if (Tcl_GetDoubleFromObj(interp, objPtr[1], &gamma)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((gamma < 0.0) || (gamma > 2.0))
				{
					/* Set the error */
					FormatResult(interp, "bad gamma \"%f\":"
						" must be from 0.0 to 2.0", gamma);

					/* Failure */
					return TCL_ERROR;
				}
				do_gamma = 1;
				break;
				
			case 3: /* -radius */
				if (Tcl_GetIntFromObj(interp, objPtr[1], &radius)
					!= TCL_OK)
				{
					return TCL_ERROR;
				}
				if ((radius < 1) || (radius > 3))
				{
					/* Set the error */
					FormatResult(interp, "bad radius \"%d\":"
						" must be from 1 to 3", radius);

					/* Failure */
					return TCL_ERROR;
				}
				break;
		}

		/* Next option/value pair */
		objPtr += 2;
		objC -= 2;
	}

	/* Required number of arguments */
	if ((objC != 0) || (radius == -1))
	{
		goto wrongNumArgs;
	}

	/* Access the lighting info */
	dark_ptr = &g_darken[radius - 1];

	/* Return current values */
	if (!do_brightness && !do_contrast && !do_gamma)
	{
		/* Set result */
		FormatResult(interp, "%d %d %f", dark_ptr->brightness,
			dark_ptr->contrast, dark_ptr->gamma);

		/* Success */
		return TCL_OK;
	}
	
	/* Reset the table */
	Colormap_One2OneTable(dark_ptr->table);

	/* Do gamma */
	if (!do_gamma) gamma = dark_ptr->gamma;
	if (gamma != 1.0) Colormap_GammaTable(gamma, dark_ptr->table);
	dark_ptr->gamma = gamma;

	/* Do brightness */
	if (!do_brightness) brightness = dark_ptr->brightness;
	if (brightness) Colormap_BrightnessTable(brightness, dark_ptr->table);
	dark_ptr->brightness = brightness;

	/* Do contrast */
	if (!do_contrast) contrast = dark_ptr->contrast;
	if (contrast) Colormap_ContrastTable(contrast, dark_ptr->table);
	dark_ptr->contrast = contrast;

	/* Success */	
	return TCL_OK;

wrongNumArgs:

	/* Set the error */
	Tcl_WrongNumArgs(interp, 1, objv,
		"-radius radius ?-brightness b -contrast c -gamma g?");

	/* Failure */
	return TCL_ERROR;
}

/*
 * Set the interpreter result with an error if the given index is
 * outside the g_sprite[] array bounds.
 */
static int ValidateSprite(Tcl_Interp *interp, int spriteIndex)
{
	/* Verify the sprite index */
	if ((spriteIndex < 0) ||(spriteIndex >= g_sprite_count))
	{
		/* Set the error */
		FormatResult(interp, "bad sprite \"%d\": "
			"must be from 0 to %d", spriteIndex,
			g_sprite_count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

/*
 * Set the interpreter result with an error if the given index specifies
 * an invalid frame in the given sprite. The sprite must be valid.
 */
static int ValidateFrame(Tcl_Interp *interp, int spriteIndex, int frameIndex)
{
	/* Access the sprite */
	t_sprite *spritePtr = &g_sprite[spriteIndex];

	/* Verify the frame index */
	if ((frameIndex < 0) || (frameIndex >= spritePtr->count))
	{
		/* Set the error */
		FormatResult(interp, "bad frame \"%d\" on sprite \"%d\": "
			"must be from 0 to %d", frameIndex, spriteIndex,
			spritePtr->count - 1);

		/* Failure */
		return TCL_ERROR;
	}

	/* Success */
	return TCL_OK;
}

int SpriteFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, t_sprite **spritePtrPtr,
	int *spriteIndexPtr)
{
	int spriteIndex;

	/* Get the sprite index */
	if (Tcl_GetIntFromObj(interp, objPtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the sprite index */
	if (ValidateSprite(interp, spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Access the sprite */
	(*spritePtrPtr) = &g_sprite[spriteIndex];

	if (spriteIndexPtr)
		(*spriteIndexPtr) = spriteIndex;

	/* Success */
	return TCL_OK;
}

int SpriteFrameFromObj(Tcl_Interp *interp, Tcl_Obj *objPtr, int spriteIndex,
	int *frameIndexPtr)
{
	int frameIndex;

	/* Get the frame index */
	if (Tcl_GetIntFromObj(interp, objPtr, &frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Verify the frame index */
	if (ValidateFrame(interp, spriteIndex, frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	(*frameIndexPtr) = frameIndex;

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) assign spriteIndex frameIndex args ...
 */
int objcmd_sprite_assign(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_sprite *spritePtr;
	int spriteIndex, frameIndex;
	IconSpec iconSpec;

	/* Get the sprite */
	if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the frame index */
	if (SpriteFrameFromObj(interp, objV[2], spriteIndex, &frameIndex)
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Scan the arguments for icon type and index */
	if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
		!= TCL_OK)
	{
		/* Failure */
		return TCL_ERROR;
	}

	/* Set the frame */
	spritePtr->icon[frameIndex] = iconSpec;

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) create
 */
int objcmd_sprite_create(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
/*	CommandInfo *infoCmd = (CommandInfo *) clientData; */
/*	int objC = objc - infoCmd->depth; */
/*	Tcl_Obj *CONST *objV = objv + infoCmd->depth; */

	t_sprite *spritePtr;

	/* Append a sprite to the global array */
	g_sprite = Array_Insert(g_sprite, &g_sprite_count,
		sizeof(t_sprite), g_sprite_count);

	/* Access the sprite */
	spritePtr = &g_sprite[g_sprite_count - 1];

	/* Set the fields */
	spritePtr->count = 0;
	spritePtr->frame = 0;
	spritePtr->speed = 0;
	spritePtr->ticks = 0;
	spritePtr->reverse = 0;
	spritePtr->changed = FALSE;
	spritePtr->icon = (IconSpec *) Array_New(0, sizeof(IconSpec));

	/* Return the index of the new sprite */
	IntResult(interp, g_sprite_count - 1);

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) delete spriteIndex ?frameIndex?
 */
int objcmd_sprite_delete(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_sprite *spritePtr;
	int spriteIndex, frameIndex;

	/* Get the sprite */
	if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* A frame index was given */
	if (objC == 3)
	{
		/* Get the frame index */
		if (SpriteFrameFromObj(interp, objV[2], spriteIndex, &frameIndex)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Delete the frame from the frames array */
		spritePtr->icon = Array_Delete(spritePtr->icon,
			&spritePtr->count, sizeof(IconSpec), frameIndex);

		if (spritePtr->frame >= spritePtr->count)
			spritePtr->frame = spritePtr->count ? spritePtr->count - 1 : 0;

		/* Done */
		return TCL_OK;
	}

	/* Free the frames array */
	Tcl_Free((char *) spritePtr->icon);

	/* Delete the sprite from the global array */
	g_sprite = Array_Delete(g_sprite, &g_sprite_count,
		sizeof(t_sprite), spriteIndex);

	/***
	 *** Now we must go through all the possible assignments and
	 *** update any references to the deleted sprite or any of the
	 *** moved sprites. NOT IMPLEMENTED
	 ***/

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) get spriteIndex ?frameIndex?
 */
int objcmd_sprite_get(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	Tcl_Obj *listObjPtr;
	t_sprite *spritePtr;
	int spriteIndex, frameIndex;
	IconSpec iconSpec;

	/* Get the sprite */
	if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* A frame index was given */
	if (objC == 3)
	{
		/* Get the frame index */
		if (SpriteFrameFromObj(interp, objV[2], spriteIndex, &frameIndex)
			!= TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Create a Tcl list object */
		listObjPtr = Tcl_NewListObj(0, NULL);

		/* Get the frame icon */
		iconSpec = spritePtr->icon[frameIndex];

		/* Append a description of the icon to the list object */
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj(format("%s %d",
				g_icon_type[iconSpec.type].desc, iconSpec.index), -1));

		/* Return the list object */
		Tcl_SetObjResult(interp, listObjPtr);

		/* Done */
		return TCL_OK;
	}

	/* Create a Tcl list object */
	listObjPtr = Tcl_NewListObj(0, NULL);

	/* Check each frame */
	for (frameIndex = 0; frameIndex < spritePtr->count; frameIndex++)
	{
		/* Get the frame icon */
		iconSpec = spritePtr->icon[frameIndex];

		/* Append a description of the icon to the list object */
		Tcl_ListObjAppendElement(interp, listObjPtr,
			Tcl_NewStringObj(format("%s %d",
				g_icon_type[iconSpec.type].desc, iconSpec.index), -1));
	}

	/* Return the list object */
	Tcl_SetObjResult(interp, listObjPtr);

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) count ?spriteIndex?
 */
int objcmd_sprite_count(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_sprite *spritePtr;
	int spriteIndex;

	/* A sprite index was given */
	if (objC == 2)
	{
		/* Get the sprite */
		if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Return the number of frames */
		IntResult(interp, spritePtr->count);

		/* Done */
		return TCL_OK;
	}

	/* Return the number of sprites */
	IntResult(interp, g_sprite_count);

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) configure spriteIndex ?option? ?value? ?option value ...?
 */
int objcmd_sprite_configure(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *configSwitch[] = {"-delay", "-reverse", NULL};
	Tcl_Obj *CONST *objPtr;
	t_sprite *spritePtr;
	int index, spriteIndex;

	/* Get the sprite */
	if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* "sprite configure $spriteIndex" */
	/* Return all configuration options */
	if (objC == 2)
	{
		/* NOT IMPLEMENTED */
		return TCL_OK;
	}

	/* "sprite configure $index $option" */
	/* Return the value of a single option */
	if (objC == 3)
	{
	    if (Tcl_GetIndexFromObj(interp, objV[2], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }
		switch (index)
		{
			case 0: /* -delay */
				IntResult(interp, spritePtr->speed);
				break;

			case 1: /* -reverse */
				BooleanResult(interp, spritePtr->reverse);
				break;
		}

		/* Done */
		return TCL_OK;
	}

	/* Point to the first option/value pair */
	objPtr = objV + 2;
	objC -= 2;

	/* Required number of arguments */
	if (objC & 1)
	{
		/* Set the error */
		Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv,
			"spriteIndex option value ?option value ...?");

		/* Failure */
		return TCL_ERROR;
	}

	/* Scan all option/value pairs */
	while (objC > 1)
	{
	    if (Tcl_GetIndexFromObj(interp, objPtr[0], configSwitch,
			"switch", 0, &index) != TCL_OK)
		{
			return TCL_ERROR;
	    }

		switch (index)
		{
			case 0: /* -delay */
				if (Tcl_GetIntFromObj(interp, objPtr[1],
					&spritePtr->speed) != TCL_OK)
				{
					return TCL_ERROR;
				}
				break;

			case 1: /* -reverse */
				if (Tcl_GetBooleanFromObj(interp, objPtr[1],
					&spritePtr->reverse) != TCL_OK)
				{
					return TCL_ERROR;
				}
				break;
		}

		/* Next option/value pair */
		objPtr += 2;
		objC -= 2;
	}

	/* Success */
	return TCL_OK;
}

/*
 * (sprite) insert spriteIndex frameIndex args
 */
int objcmd_sprite_insert(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	t_sprite *spritePtr;
	int spriteIndex, frameIndex;
	IconSpec iconSpec;

	/* Get the sprite */
	if (SpriteFromObj(interp, objV[1], &spritePtr, &spriteIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the frame index */
	if (Tcl_GetIntFromObj(interp, objV[2], &frameIndex) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Scan the arguments for icon type and index */
	if (Icon_ParseArgs(interp, objc, objv, infoCmd->depth + 3, &iconSpec)
		!= TCL_OK)
	{
		/* Failure */
		return TCL_ERROR;
	}

	/* Append */
	if (frameIndex > spritePtr->count)
	{
		frameIndex = spritePtr->count;
	}

	/* Prepend */
	else if (frameIndex < 0)
	{
		frameIndex = 0;
	}

	/* Insert a new frame */
	spritePtr->icon = Array_Insert(spritePtr->icon,
		&spritePtr->count, sizeof(IconSpec), frameIndex);

	/* Set the frame */
	spritePtr->icon[frameIndex] = iconSpec;

	/* Success */
	return TCL_OK;
}

CommandInit assignCmdInit[] = {
	{0, "alternate", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assign", 3, 0, "alternateIndex frameIndex ?args ...?", objcmd_alternate_assign, (ClientData) 0},
		{1, "configure", 2, 0, "alternateIndex ?option? ?value? ?option value ...?", objcmd_alternate_configure, (ClientData) 0},
		{1, "count", 1, 2, "?alternateIndex?", objcmd_alternate_count, (ClientData) 0},
		{1, "create", 2, 2, "reason", objcmd_alternate_create, (ClientData) 0},
		{1, "delete", 2, 3, "alternateIndex ?frameIndex?", objcmd_alternate_delete, (ClientData) 0},
		{1, "get", 2, 3, "alternateIndex ?frameIndex?", objcmd_alternate_get, (ClientData) 0},
		{1, "insert", 3, 0, "alternateIndex frameIndex ?args ...?", objcmd_alternate_insert, (ClientData) 0},
	{0, "assign", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "count", 2, 2, "group", objcmd_assign_count, (ClientData) 0},
		{1, "groups", 1, 1, (char *) NULL, objcmd_assign_groups, (ClientData) 0},
		{1, "types", 1, 1, (char *) NULL, objcmd_assign_types, (ClientData) 0},
		{1, "set", 3, 4, "group member ?assign?", objcmd_assign_set, (ClientData) 0},
		{1, "toicon", 2, 2, "assign", objcmd_assign_toicon, (ClientData) 0},
		{1, "validate", 2, 2, "assign", objcmd_assign_validate, (ClientData) 0},
	{0, "effect", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assign", 3, 0, "group effect ?args ...?", objcmd_effect_assign, (ClientData) 0},
		{1, "color", 2, 2, "type", objcmd_effect_color, (ClientData) 0},
		{1, "groups", 1, 1, (char *) NULL, objcmd_effect_groups, (ClientData) 0},
		{1, "names", 2, 2, "group", objcmd_effect_names, (ClientData) 0},
	{0, "feature", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assignshape", 3, 5, "f_idx shape ?assign? ?hack?", objcmd_feature_assignshape, (ClientData) 0},
		{1, "configure", 2, 0, "f_idx ?option? ?value? ?option value ...?", objcmd_feature_configure, (ClientData) 0},
		{1, "lighting", 3, 0, "-radius r ?option? ?value? ?option value ...?", objcmd_lighting, (ClientData) 0},
		{1, "torch", 1, 2, "paletteIndex opacity", objcmd_feature_torch, (ClientData) 0},
		{1, "torchlite", 1, 2, "?boolean?", objcmd_feature_torchlite, (ClientData) 0},
	{0, "flavor", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assign", 3, 0, "flavorName flavorIndex ?args ...?", objcmd_flavor_assign, (ClientData) 0},
		{1, "count", 2, 2, "flavorName", objcmd_flavor_count, (ClientData) 0},
	{0, "sprite", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "assign", 3, 0, "spriteIndex frameIndex ?args ...?", objcmd_sprite_assign, (ClientData) 0},
		{1, "configure", 2, 0, "spriteIndex ?option? ?value? ?option value ...?", objcmd_sprite_configure, (ClientData) 0},
		{1, "count", 1, 2, "?spriteIndex?", objcmd_sprite_count, (ClientData) 0},
		{1, "create", 1, 1, (char *) NULL, objcmd_sprite_create, (ClientData) 0},
		{1, "delete", 2, 3, "spriteIndex ?frameIndex?", objcmd_sprite_delete, (ClientData) 0},
		{1, "get", 2, 3, "spriteIndex ?frameIndex?", objcmd_sprite_get, (ClientData) 0},
		{1, "insert", 3, 0, "spriteIndex frameIndex ?args ...?", objcmd_sprite_insert, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

