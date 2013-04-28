/* File: interp2.c */

/* Purpose: more tcl commands */

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
#include "struct-dll.h"
#include "util-dll.h"
#include "icon.h"

#define SND_MSG_COLORS

int g_track_grid_x = -1, g_track_grid_y = -1;

/*
 * Write the user's inscription for the given object into buf.
 */
static char *object_inscription(object_type *o_ptr, char *buf)
{
	/* No inscription yet */
	buf[0] = '\0';

	/* Use the user's inscription if available */
	if (get_user_inscription(o_ptr))
	{
		(void) strcpy(buf, quark_str(get_user_inscription(o_ptr)));
	}

	return buf;
}

#if defined(KANGBANDTKxxx)

/* This is an information block set in building.c */
_buildingdata g_buildingdata = {0};

/*
 *--------------------------------------------------------------
 *
 * objcmd_building --
 *
 *	Implements the "building" script command.
 * 	Syntax:
 *		building ownername -- name of owner
 *		building ownerrace -- race of owner
 *		building buildingename -- building name
 *		building actions -- List of action chars/text
 *
 *--------------------------------------------------------------
 */

int
objcmd_building(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"ownername", "ownerrace", "buildingname",
		"actions", NULL};
	enum {IDX_OWNERNAME, IDX_OWNERRACE, IDX_BUILDINGNAME,
		IDX_ACTIONS} option;

	building *bldg;
	char *t, buff[20];
	int i;
	Tcl_Obj *listObjPtr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (!g_buildingdata.inside)
	{
		StaticResult(interp, "character isn't inside a building");
		return TCL_ERROR;
	}

	bldg = g_buildingdata.bldg;

	switch (option) {

		case IDX_OWNERNAME: /* ownername */
			ExtToUtf_SetResult(interp, (char *) bldg->name);
			break;

		case IDX_OWNERRACE: /* ownerrace */
			ExtToUtf_SetResult(interp, (char *) bldg->race);
			break;

		case IDX_BUILDINGNAME: /* buildingname */
			t = (char *) (f_name + f_info[FEAT_BLDG_HEAD + g_buildingdata.building_loc].name);
			ExtToUtf_SetResult(interp, t);
			break;

		case IDX_ACTIONS: /* actions */
		{
			Tcl_Obj *elemObjv[8];			
			int elemObjc;

			listObjPtr = Tcl_NewListObj(0, NULL);
			for (i = 0; i < bldg->num_actions; i++)
			{
				if (bldg->action_restr[i] == 0)
				{
					if (((bldg->class == p_ptr->pclass) && (bldg->class_costs[i] == 0)) ||
					    ((bldg->class != p_ptr->pclass) && (bldg->other_costs[i] == 0)))
					{
						buff[0] = '\0';
					}
					else if (bldg->class == p_ptr->pclass)
					{
						sprintf(buff, " (%dgp)", bldg->class_costs[i]);
					}
					else
					{
						sprintf(buff, " (%dgp)", bldg->other_costs[i]);
					}
				}
				else if (bldg->action_restr[i] == 1)
				{
					if (!(bldg->g_class[p_ptr->pclass]))
					{
						strcpy(buff, " (closed)");
					}
					else if (((bldg->class == p_ptr->pclass) && (bldg->class_costs[i] == 0)) ||
					    ((bldg->class != p_ptr->pclass) && (bldg->other_costs[i] == 0)))
					{
						buff[0] = '\0';
					}
					else if (bldg->class == p_ptr->pclass)
					{
						sprintf(buff, " (%dgp)", bldg->class_costs[i]);
					}
					else
					{
						sprintf(buff, " (%dgp)", bldg->other_costs[i]);
					}
				}
				else
				{
					if (p_ptr->pclass != bldg->class)
					{
						strcpy(buff, " (closed)");
					}
					else if (bldg->class_costs[i] != 0)
					{
						sprintf(buff, " (%dgp)", bldg->class_costs[i]);
					}
					else
					{
						buff[0] = '\0';
					}
				}

				/* Reset */
				elemObjc = 0;

				/* char */
				elemObjv[elemObjc++] = Tcl_NewStringObj("char", -1);
				elemObjv[elemObjc++] = Tcl_NewStringObj(&bldg->letters[i], 1);

				/* label */
				elemObjv[elemObjc++] = Tcl_NewStringObj("label", -1);
				elemObjv[elemObjc++] = ExtToUtf_NewStringObj(bldg->act_names[i], -1);

				/* info */
				elemObjv[elemObjc++] = Tcl_NewStringObj("info", -1);
				elemObjv[elemObjc++] = ExtToUtf_NewStringObj(buff, -1);

				/* action */
				elemObjv[elemObjc++] = Tcl_NewStringObj("action", -1);
				elemObjv[elemObjc++] = Tcl_NewIntObj(bldg->actions[i]);

				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewListObj(elemObjc, elemObjv));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}
	}

	return TCL_OK;
}

#endif /* KANGBANDTK */

#if defined(KANGBANDTK) || defined(ZANGBANDTK)

/* This is an information block set in building.c */
_buildingdata g_buildingdata = {0};

/* From bldg.c */
static bool is_owner(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass] == BUILDING_OWNER)
	{
		return (TRUE);	
	}

	if (bldg->member_race[p_ptr->prace] == BUILDING_OWNER)
	{
		return (TRUE);	
	}

#if defined(ZANGBANDTK)

	if ((bldg->member_realm[p_ptr->realm1] == BUILDING_OWNER) ||
		(bldg->member_realm[p_ptr->realm2] == BUILDING_OWNER))
	{
		return (TRUE);
	}

#endif

	return (FALSE);
}

/* From bldg.c */
static bool is_member(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass])
	{
		return (TRUE);	
	}

	if (bldg->member_race[p_ptr->prace])
	{
		return (TRUE);	
	}

#if defined(ZANGBANDTK)

	if ((bldg->member_realm[p_ptr->realm1]) || (bldg->member_realm[p_ptr->realm2]))
	{
		return (TRUE);
	}

#endif

	return (FALSE);
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_building --
 *
 *	Implements the "building" script command.
 * 	Syntax:
 *		building ownername -- name of owner
 *		building ownerrace -- race of owner
 *		building buildingename -- building name
 *		building action -- List of action chars/text
 *
 *--------------------------------------------------------------
 */

int
objcmd_building(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"ownername", "ownerrace", "buildingname",
		"actions", "index", NULL};
	enum {IDX_OWNERNAME, IDX_OWNERRACE, IDX_BUILDINGNAME,
		IDX_ACTIONS, IDX_INDEX} option;

	building_type *bldg;
	char buff[20];
	int i;
	Tcl_Obj *listObjPtr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (!g_buildingdata.inside)
	{
		StaticResult(interp, "character isn't inside a building");
		return TCL_ERROR;
	}

	bldg = g_buildingdata.bldg;

	switch (option)
	{
		case IDX_OWNERNAME: /* ownername */
			ExtToUtf_SetResult(interp, (char *) bldg->owner_name);
			break;

		case IDX_OWNERRACE: /* ownerrace */
			ExtToUtf_SetResult(interp, (char *) bldg->owner_race);
			break;

		case IDX_BUILDINGNAME: /* buildingname */
			ExtToUtf_SetResult(interp, (char *) bldg->name);
			break;

		case IDX_ACTIONS: /* actions */
		{
			Tcl_Obj *elemObjv[8];			
			int elemObjc;

			listObjPtr = Tcl_NewListObj(0, NULL);
			for (i = 0; i < 6; i++)
			{
				if (!bldg->letters[i]) continue;
				if (bldg->action_restr[i] == 0)
				{
					if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
						(!is_owner(bldg) && (bldg->other_costs[i] == 0)))
					{
						buff[0] = '\0';
					}
					else if (is_owner(bldg))
					{
						sprintf(buff, " (%dgp)", bldg->member_costs[i]);
					}
					else
					{
						sprintf(buff, " (%dgp)", bldg->other_costs[i]);
					}
				}
				else if (bldg->action_restr[i] == 1)
				{
					if (!is_member(bldg))
					{
						strcpy(buff, " (closed)");
					}
					else if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
						(is_member(bldg) && (bldg->other_costs[i] == 0)))
					{
						buff[0] = '\0';
					}
					else if (is_owner(bldg))
					{
						sprintf(buff, " (%dgp)", bldg->member_costs[i]);
					}
					else
					{
						sprintf(buff, " (%dgp)", bldg->other_costs[i]);
					}
				}
				else
				{
					if (!is_owner(bldg))
					{
						strcpy(buff, " (closed)");
					}
					else if (bldg->member_costs[i] != 0)
					{
						sprintf(buff, " (%dgp)", bldg->member_costs[i]);
					}
					else
					{
						buff[0] = '\0';
					}
				}

				/* Reset */
				elemObjc = 0;

				/* char */
				elemObjv[elemObjc++] = Tcl_NewStringObj("char", -1);
				elemObjv[elemObjc++] = Tcl_NewStringObj(&bldg->letters[i], 1);

				/* label */
				elemObjv[elemObjc++] = Tcl_NewStringObj("label", -1);
				elemObjv[elemObjc++] = ExtToUtf_NewStringObj(bldg->act_names[i], -1);

				/* info */
				elemObjv[elemObjc++] = Tcl_NewStringObj("info", -1);
				elemObjv[elemObjc++] = ExtToUtf_NewStringObj(buff, -1);

				/* action */
				elemObjv[elemObjc++] = Tcl_NewStringObj("action", -1);
				elemObjv[elemObjc++] = Tcl_NewIntObj(bldg->actions[i]);

				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewListObj(elemObjc, elemObjv));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

		case IDX_INDEX: /* index */
			IntResult(interp, bldg - building);
			break;
	}

	return TCL_OK;
}

#endif /* ZANGBANDTK */

/*
 *--------------------------------------------------------------
 *
 * objcmd_cave --
 *
 *	Implements the "cave" script command.
 * 	Syntax:
 *		cave assign y x plane ?assign? --
 *		cave blocked y x -- can player move there
 *		cave day --
 *		cave examine y x -- describe what's there
 *		cave exists --
 *		cave height -- height of cave
 *		cave info -- get info about a grid
 *		cave in_bounds y x --
 *		cave in_bounds_fully y x --
 *		cave shape y x --
 *		cave track y x --
 *		cave town_illuminate --
 *		cave width -- width of cave
 *		cave wild_name -- get name of wilderness area
 *
 *--------------------------------------------------------------
 */

int
objcmd_cave(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"assign", "blocked", "examine", "height", "width",
		"info", "in_bounds", "in_bounds_fully", "exists", "shape", "day",
		"town_illuminate", "track",
#if defined(ZANGBANDTK)
		"wild_name",
#endif /* ZANGBANDTK */
		NULL};
	enum {IDX_ASSIGN, IDX_BLOCKED, IDX_EXAMINE, IDX_HEIGHT, IDX_WIDTH,
		IDX_INFO, IDX_IN_BOUNDS, IDX_IN_BOUNDS_FULLY, IDX_EXISTS, IDX_SHAPE,
		IDX_DAY, IDX_TOWN_ILLUMINATE, IDX_TRACK
#if defined(ZANGBANDTK)
		, IDX_WILD_NAME
#endif /* ZANGBANDTK */
		} option;

	int feat, y, x;
	int blocked;
	char desc[160], *varName;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_ASSIGN: /* assign */
		{
			static CONST char *opt[] = {"icon1", "icon2", "icon3", "icon4", NULL};
			int plane;
			t_assign assign;
			char *t;

			if (objC < 5 || objC > 6)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv,
					"y x plane ?assign?");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (!in_bounds_test(y, x))
			{
				goto bad_location;
			}
			if (Tcl_GetIndexFromObj(interp, objV[4], opt, "plane", 0, 
				&plane) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (objC == 5)
			{
				char buf[128];
				assign = g_icon_map[plane][y][x];
				(void) assign_print(buf, &assign);
				Tcl_SetResult(interp, buf, TCL_VOLATILE);
				break;
			}
			t = Tcl_GetString(objV[5]);
			if (assign_parse(interp, &assign, t) != TCL_OK)
			{
				return TCL_ERROR;
			}
			g_icon_map[plane][y][x] = assign;
			break;
		}

		case IDX_BLOCKED: /* blocked */
			if (!character_dungeon) goto not_exists;
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}

			if (!in_bounds_test(y, x))
			{
				goto bad_location;
			}

			/* Visible monsters can be attacked even if in a wall. */
			if (cave_m_idx(y, x) > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx(y, x)];
				if (m_ptr->ml)
				{
					BooleanResult(interp, FALSE);
					break;
				}
			}

#if defined(KANGBANDTK) || defined(ZANGBANDTK)
			blocked = !player_test_feature(y, x, 0);
#endif /* KANGBANDTK, ZANGBANDTK */
#if defined(ANGBANDTK)
			if (!cave_floor_bold(y, x))
#endif /* */
#if defined(OANGBANDTK)
			if (!cave_passable_bold(y, x))
#endif /* */
#if defined (ANGBANDTK) || defined(OANGBANDTK)
			{
				/* Unknown grids are not "blocked" */
				if (!(cave_info(y, x) & (CAVE_MARK)))
				{
					blocked = 0;
				}

				/* Jammed/locked/closed doors are not "blocked" */
				else if (cave_feat(y, x) < FEAT_SECRET)
				{
					blocked = 0;
				}

				/* Rubble, walls and secret doors are "blocked" */
				else
				{
					blocked = 1;
				}
			}
			else
			{
				blocked = 0;
			}
#endif /* ANGBANDTK, OANGBANDTK */

			BooleanResult(interp, blocked);
			break;

		case IDX_EXAMINE: /* examine */
			if (!character_dungeon) goto not_exists;

			/* Required number of arguments */
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x");
				return TCL_ERROR;
			}

			/* Get the y coordinate */
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				y = -1;
			}

			/* Get the x coordinate */
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				x = -1;
			}

			/* Get a string describing what is seen. */
			angtk_examine(y, x, desc);

			/* Set the result */
			ExtToUtf_SetResult(interp, desc);
			break;

		case IDX_HEIGHT: /* height */
			if (!character_dungeon) goto not_exists;
			IntResult(interp, g_cave_hgt);
			break;

		case IDX_WIDTH: /* width */
			if (!character_dungeon) goto not_exists;
			IntResult(interp, g_cave_wid);
			break;

		case IDX_INFO: /* info */
			if (!character_dungeon) goto not_exists;
			if (objC != 5)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x arrayName");
				return TCL_ERROR;
			}

			/* Get the y coordinate */
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Get the x coordinate */
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Validate coordinates */
			if (!in_bounds_test(y, x))
			{
				goto bad_location;
			}

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[4], NULL);

			/* Get the feature index */
			feat = cave_feat(y, x);
			feat = f_info[feat].mimic;
#if defined(KANGBANDTK)
			if (SetArrayValueLong(varName, "special", cave_special[y][x]) != TCL_OK)
			{
				return TCL_ERROR;
			}
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
			if (SetArrayValueLong(varName, "special", cave[y][x].special) != TCL_OK)
			{
				return TCL_ERROR;
			}
			/* XXX Hack -- Get the "mimic" index */
			if (cave[y][x].mimic)
			{
				feat = cave[y][x].mimic;
			}
#endif /* ZANGBANDTK */

			if (SetArrayValueLong(varName, "f_idx", feat) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(varName, "m_idx", cave_m_idx(y, x)) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (SetArrayValueLong(varName, "o_idx", cave_o_idx(y, x)) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_IN_BOUNDS: /* in_bounds */
		case IDX_IN_BOUNDS_FULLY: /* in_bounds_fully */
			if (!character_dungeon) goto not_exists;
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}

			if (option == IDX_IN_BOUNDS)
				BooleanResult(interp, in_bounds_test(y, x));
			else
				BooleanResult(interp, in_bounds_fully_test(y, x));
			break;

		case IDX_EXISTS: /* exists */
			BooleanResult(interp, character_dungeon);
			break;

		case IDX_SHAPE: /* shape */
		{
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}
			StaticResult(interp, (char *) keyword_wall[g_grid[y][x].shape]);
			break;
		}

		case IDX_DAY: /* day */
		{
			StaticResult(interp, is_daytime() ? "day" : "night");
			break;
		}

		case IDX_TOWN_ILLUMINATE: /* town_illuminate */
		{
			/* Hack -- Update the global now */
			g_daytime = !p_ptr_depth &&
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
				!p_ptr->inside_quest &&
#endif
				((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

			if (!p_ptr_depth)
			{
				town_illuminate(g_daytime);
				angtk_cave_changed();
			}
			break;
		}

		case IDX_TRACK: /* track */
		{
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "y x");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &y) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[3], &x) != TCL_OK)
			{
				return TCL_ERROR;
			}
			g_track_grid_x = x;
			g_track_grid_y = y;
			break;
		}

#if defined(ZANGBANDTK)
		case IDX_WILD_NAME: /* wild_name */
			if (!character_dungeon) goto not_exists;
			if (!dun_level)
			{
				if (p_ptr->town_num)
				{
					ExtToUtf_SetResult(interp, town[p_ptr->town_num].name);
				}
				else
				{
					StaticResult(interp, "Wilderness");
				}
			}
			break;
#endif /* ZANGBANDTK */
	}

	/* Success */
	return TCL_OK;

bad_location:

	/* Set the error */
	FormatResult(interp, "y=%d,x=%d is not in bounds", y, x);
	goto error;

not_exists:

	/* Set the error */
	StaticResult(interp, "dungeon has not been generated yet");

error:

	/* Failure */
	return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_equipment --
 *
 *	Implements the "equipment" script command.
 * 	Syntax:
 *		equipment find SEARCHCOMMAND ...
 *		equipment info SLOT
 *		equipment memory SLOT
 *
 *--------------------------------------------------------------
 */

int
objcmd_equipment(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"find", "info", "memory", "flags",
		"inscription", NULL};
	enum {IDX_FIND, IDX_INFO, IDX_MEMORY, IDX_FLAGS,
		IDX_INSCRIPTION} option;
	int index;

	int i, i_idx, tval;
	object_type *o_ptr;
	char buf[80], *buffer, *varName, *t;
	long length;
	Tcl_Obj *listObjPtr;

	/* Default to finding all matches */
	int request_limit = 0, match_limit = 0, cnt = 0;

	/* Default to ignoring activate */
	int request_activate = 0, match_activate = 0;

	/* Default to ignoring item_tester_okay() hook */
	int request_tester = 0, match_tester = 0;

	/* Default to no restriction on tval */
	int request_tval = 0, match_tval[10], tval_cnt = 0;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (!character_generated)
	{
		StaticResult(interp, "character has not been generated yet");
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_FIND: /* find */

			/* Scan arguments for options */
			for (i = 2; i < objC; )
			{
				static CONST char *cmdOptions[] = {"-limit", "-tester", "-tval",
					"-activate", NULL};

				/* Get the sub-option */
				if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* Limit */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_limit)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						request_limit = 1;
						i += 2;
						break;

					case 1: /* Tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;

					case 2: /* Tval */
						t = Tcl_GetStringFromObj(objV[i+1], NULL);
						if (angtk_tval_const(&tval, t) != TCL_OK)
						{
							return TCL_ERROR;
						}
						match_tval[tval_cnt++] = tval;
						request_tval = 1;
						i += 2;
						break;

					case 3: /* Activate */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_activate) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_activate = 1;
						i += 2;
						break;
				}
			}

			/* Return a list of inventory[] indexes */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Scan inventory list */
			for (i_idx = INVEN_WIELD; i_idx < INVEN_TOTAL; i_idx++)
			{
				u32b f1, f2, f3;

				/* Get the n'th item */
				o_ptr = &inventory[i_idx];

#if defined(OANGBANDTK)
				/* Never show empty quiver slots */
				if ((i_idx >= INVEN_BLANK) && !o_ptr->k_idx) continue;
#endif /* OANGBANDTK */

				/* Extract the flags */
				object_flags(o_ptr, &f1, &f2, &f3);

				if (request_tester && match_tester)
				{
					if (!item_tester_okay(o_ptr)) continue;
				}
				if (request_tval)
				{
					for (i = 0; i < tval_cnt; i++)
					{
						if (match_tval[0] == o_ptr->tval) break;
					}
					if (i == tval_cnt) continue;
				}
				if (request_activate)
				{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
					bool activate = object_known_p(o_ptr) &&
						((f3 & TR3_ACTIVATE) != 0);
#endif /* */
#if defined(OANGBANDTK)
					bool activate = object_known_p(o_ptr) &&
						(o_ptr->xtra1 == OBJECT_XTRA_TYPE_ACTIVATION);
#endif /* */
					if (activate != match_activate) continue;
				}

				/* Found a match */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(i_idx - INVEN_WIELD));

				/* Return x matches */
				if (request_limit && (++cnt >= match_limit)) break;
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_INFO: /* info */

			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "slot arrayName");
				return TCL_ERROR;
			}

			/* Get a numerical index or slot name */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				Tcl_ResetResult(interp);
				if (Tcl_GetIndexFromObj(interp, objV[2],
					keyword_slot, "slot", 0, &i_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}
			if ((i_idx < 0) || (i_idx >= (INVEN_TOTAL - INVEN_WIELD)))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &inventory[INVEN_WIELD + i_idx];

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[3], NULL);

			if (dump_object_info(varName, o_ptr, INVEN_WIELD + i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_MEMORY: /* memory */

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "slot");
				return TCL_ERROR;
			}

			/* Get a numerical index or slot name */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				Tcl_ResetResult(interp);
				if (Tcl_GetIndexFromObj(interp, objV[2],
					keyword_slot, "slot", 0, &i_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}
			if ((i_idx < 0) || (i_idx >= (INVEN_TOTAL - INVEN_WIELD)))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &inventory[INVEN_WIELD + i_idx];

			buffer = Tcl_Alloc(5 * 1024L);
			length = angtk_describe_object(o_ptr, buffer, FALSE);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			Tcl_Free(buffer);
			break;

		case IDX_FLAGS: /* flags */

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "slot");
				return TCL_ERROR;
			}

			/* Get a numerical index or slot name */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				Tcl_ResetResult(interp);
				if (Tcl_GetIndexFromObj(interp, objV[2],
					keyword_slot, "slot", 0, &i_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}
			if ((i_idx < 0) || (i_idx >= (INVEN_TOTAL - INVEN_WIELD)))
			{
				goto bad_index;
			}

			/* Get object info */
			o_ptr = &inventory[INVEN_WIELD + i_idx];

			listObjPtr = dump_object_flags(interp, o_ptr);
			if (listObjPtr == NULL)
			{
				return TCL_ERROR;
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_INSCRIPTION: /* inscription */

			if (objC < 3)
			{
				/* Set the error */
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index ?string?");

				/* Failure */
				return TCL_ERROR;
			}

			/* Get a numerical index or slot name */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				Tcl_ResetResult(interp);
				if (Tcl_GetIndexFromObj(interp, objV[2],
					keyword_slot, "slot", 0, &i_idx) != TCL_OK)
				{
					return TCL_ERROR;
				}
			}

			/* Verify i_idx */
			if ((i_idx < 0) || (i_idx >= (INVEN_TOTAL - INVEN_WIELD)))
			{
				goto bad_index;
			}

			/* Get item info */
			o_ptr = &inventory[INVEN_WIELD + i_idx];

			/* Require a real item */
			if (!o_ptr->k_idx)
			{
				/* Set the error */
				FormatResult(interp, "equipment item %d is empty", i_idx);

				/* Failure */
				return TCL_ERROR;
			}

			/* Set the inscription. */
			if (objC == 4)
			{
				/* Get the new inscription */
				t = Tcl_GetStringFromObj(objV[3], NULL);

				if (strlen(t))
				{
					/* Save the inscription */
					set_user_inscription(o_ptr, quark_add(t));
				}
				else
				{
					/* Clear the inscription */
					set_user_inscription(o_ptr, 0);
				}

				/* Combine the pack */
				p_ptr->notice |= (PN_COMBINE);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN | PW_EQUIP);

				/* Done */
				break;
			}

			/* Get the current inscription, if any */
			(void) object_inscription(o_ptr, buf);

			/* Return the current inscription */
			ExtToUtf_SetResult(interp, buf);

			/* Done */
			break;
	}

	/* Success */
	return TCL_OK;

bad_index:

	/* Set the error */
	FormatResult(interp,
		"bad equipment index \"%s\": must be between 0 and %d",
		Tcl_GetString(objV[2]), INVEN_TOTAL - INVEN_WIELD - 1);

	/* Failure */
	return TCL_ERROR;
}

#ifdef ALLOW_EASY_FLOOR

int floor_y = -1, floor_x = -1;

/*
 *--------------------------------------------------------------
 *
 * objcmd_floor --
 *
 *	Implements the "floor" script command.
 * 	Syntax:
 *
 *		floor find SEARCHCOMMAND ?arg arg ...?
 * 			Return list of indexes of matching objects
 *		floor info INDEX arrayName
 *		floor memory INDEX
 *
 *--------------------------------------------------------------
 */

int
objcmd_floor(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"find", "info", "memory", "inscription",
		NULL};
	enum {IDX_FIND, IDX_INFO, IDX_MEMORY, IDX_INSCRIPTION} option;
	int index;

	Tcl_Obj *listObjPtr;
	char buf[80], *buffer, *t, *varName;
	int this_o_idx, next_o_idx;
	int i, tval;
	long length;
	object_type *o_ptr;
	int fy, fx;

	/* Default to finding all matches */
	int request_limit = 0, match_limit = 0, cnt = 0;

	/* Default to ignoring item_tester_okay() hook */
	int request_tester = 0, match_tester = 0;

	/* Default to no restriction on tval */
	int request_tval = 0, match_tval[10], tval_cnt = 0;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* XXX Hack -- Determine the location to display */
	fy = floor_y;
	fx = floor_x;
	if (fy == -1)
	{
		fy = p_ptr_py;
		fx = p_ptr_px;
	}

	switch (option)
	{
		case IDX_FIND: /* find */
		{
			extern bool item_tester_hook_cast(const object_type *o_ptr);
			extern bool item_tester_hook_study(const object_type *o_ptr);

			bool (*old_tester_hook)(const object_type *) = item_tester_hook;
			bool (*temp_tester_hook)(const object_type *) = NULL;

			/* Scan arguments for options */
			for (i = 2; i < objC; )
			{
				static CONST char *cmdOptions[] = {"-hook", "-limit", "-tester",
					"-tval", NULL};

				/* Get the sub-option */
				if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* hook */
						t = Tcl_GetStringFromObj(objV[i+1], NULL);
						if (streq(t, "cast"))
							temp_tester_hook = item_tester_hook_cast;
						else if (streq(t, "study"))
							temp_tester_hook = item_tester_hook_study;
						else
						{
							Tcl_SetResult(interp, format("unknown hook \"%s\"",
								t), TCL_VOLATILE);
							return TCL_ERROR;
						}
						request_tester = 1;
						match_tester = 1;
						i += 2;
						break;

					case 1: /* Limit */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_limit)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						request_limit = 1;
						i += 2;
						break;

					case 2: /* Tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;

					case 3: /* Tval */
						t = Tcl_GetStringFromObj(objV[i+1], NULL);
						if (angtk_tval_const(&tval, t) != TCL_OK)
						{
							return TCL_ERROR;
						}
						match_tval[tval_cnt++] = tval;
						request_tval = 1;
						i += 2;
						break;
				}
			}

			if (temp_tester_hook) item_tester_hook = temp_tester_hook;

			/* Return a list of o_list[] indexes */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx(fy, fx); this_o_idx; this_o_idx = next_o_idx)
			{
				/* Acquire object */
				o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				if (request_tester && match_tester)
				{
					/* Accept TV_GOLD if no tester */
					if ((o_ptr->tval == TV_GOLD) && !item_tester_hook &&
						!item_tester_tval)
					{
						/* Nothing */
					}
					else if (!item_tester_okay(o_ptr)) 
					{
						continue;
					}
				}
				if (request_tval)
				{
					for (i = 0; i < tval_cnt; i++)
					{
						if (match_tval[0] == o_ptr->tval) break;
					}
					if (i == tval_cnt) continue;
				}

				/* Found a match */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(this_o_idx));

				/* Return x matches */
				if (request_limit && (++cnt >= match_limit)) break;
			}

			/* XXX Hack -- Restore the hook */
			item_tester_hook = old_tester_hook;

			/* Return a list of o_list[] indexes */
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

		case IDX_INFO: /* info */

			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index arrayName");
				return TCL_ERROR;
			}
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (i <= 0 || i > o_max) goto bad_index;

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[3], NULL);

			/* Get item info */
			o_ptr = &o_list[i];

			/* Illegal */
			if (!o_ptr->k_idx || (o_ptr->iy != fy) || (o_ptr->ix != fx))
			{
				goto bad_index;
			}

			/* Floor items always get 'a' for char */
			if (dump_object_info(varName, o_ptr, 0) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_MEMORY: /* memory */

			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (i <= 0 || i > o_max) goto bad_index;

			/* Get item info */
			o_ptr = &o_list[i];

			/* Illegal */
			if (!o_ptr->k_idx || (o_ptr->iy != fy) || (o_ptr->ix != fx))
			{
				goto bad_index;
			}

			buffer = Tcl_Alloc(5 * 1024L);
			length = angtk_describe_object(o_ptr, buffer, FALSE);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			Tcl_Free(buffer);
			break;

		case IDX_INSCRIPTION: /* inscription */

			if (objC < 3)
			{
				/* Set the error */
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index ?string?");

				/* Failure */
				return TCL_ERROR;
			}

			/* Get the item index */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify i_idx */
			if (i <= 0 || i > o_max) goto bad_index;

			/* Get item info */
			o_ptr = &o_list[i];

			/* Illegal */
			if (!o_ptr->k_idx || (o_ptr->iy != fy) || (o_ptr->ix != fx))
			{
				goto bad_index;
			}

			/* Set the inscription. */
			if (objC == 4)
			{
				/* Get the new inscription */
				t = Tcl_GetStringFromObj(objV[3], NULL);

				if (strlen(t))
				{
					/* Save the inscription */
					set_user_inscription(o_ptr, quark_add(t));
				}
				else
				{
					/* Clear the inscription */
					set_user_inscription(o_ptr, 0);
				}

				/* Combine the pack */
				p_ptr->notice |= (PN_COMBINE);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN | PW_EQUIP);

				/* Done */
				break;
			}

			/* Get the current inscription, if any */
			(void) object_inscription(o_ptr, buf);

			/* Return the current inscription */
			ExtToUtf_SetResult(interp, buf);

			/* Done */
			break;
	}

	return TCL_OK;

bad_index:
	FormatResult(interp, "bad floor index \"%d\"", i);
	return TCL_ERROR;
}

#endif /* ALLOW_EASY_FLOOR */

/*
 *--------------------------------------------------------------
 *
 * objcmd_game --
 *
 *	Implements the "game" script command.
 * 	Syntax:
 *		game abort ?confirm? -- Quit without saving
 *		game directory -- Get a directory pathname
 *		game file_character -- Dump a character file
 *		game keymap_dump -- Dump a keymap file
 *		game macro_dump -- Dump a macro file
 *		game new -- Start a new game
 *		game open -- Open a save file
 *		game process_pref_file -- Process a preference file
 *		game quit -- Quit with save
 *		game variant -- Which variant is this?
 *
 *--------------------------------------------------------------
 */

extern errr macro_dump(cptr fname); /* see cmd4.c */
extern errr keymap_dump(cptr fname); /* see cmd4.c */

#if defined(ZANGBANDTK) && !defined(KEYSTUFF_283) /* TNB */
errr keymap_dump(cptr fname) {return -1;}
#endif /* KEYSTUFF_283 -- TNB */

/* List of directory keywords */
CONST char *keyword_path[] = {
	"ANGBAND_DIR_ROOT",
	"ANGBAND_DIR_USER",
	"ANGBAND_DIR_TK",
	"ANGBAND_DIR_COMMON",
	"ANGBAND_DIR_COMMON_TK",
	NULL
};

/* Can the above directories be changed? */
static bool s_edit_path[] = {
	FALSE,
	FALSE,
	FALSE,
	TRUE,
	TRUE
};

extern bool check_dir(cptr s);

int
objcmd_game(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"abort", "directory", "file_character",
		"macro_dump", "new", "open", "process_pref_file", "quit",
		"keymap_dump", "savefile_info", "version", "variant",
		"savefile", NULL};
	enum {IDX_ABORT, IDX_DIRECTORY, IDX_FILE_CHARACTER,
		IDX_MACRO_DUMP, IDX_NEW, IDX_OPEN, IDX_PROCESS_PREF_FILE, IDX_QUIT,
		IDX_KEYMAP_DUMP, IDX_SAVEFILE_INFO, IDX_VERSION, IDX_VARIANT,
		IDX_SAVEFILE} option;
	int index;

	char *t, *utfString, *extString, *varName;
	cptr *angband_path[10];
	Tcl_DString utfDString, extDString;
	Tcl_Channel c;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_ABORT: /* abort */
		{
			int confirm = 1;
			if (objC == 3)
			{
				static CONST char *abortSwitch[] = {"-noask", NULL};
				if (Tcl_GetIndexFromObj(interp, objV[2], abortSwitch,
					"switch", 0, &index) != TCL_OK)
				{
					return TCL_ERROR;
				}
				confirm = 0;
			}
			if (confirm && game_in_progress && character_generated)
			{
#if 1 /* def PLATFORM_X11 */
				int result;

				result = Tcl_EvalEx(g_interp,
					"tk_messageBox -icon warning -type okcancel -message \"Your character will not be saved!\" -title \"Quit Without Saving\"",
					-1, TCL_EVAL_GLOBAL);
				if (result == TCL_OK)
				{
					CONST char *s = Tcl_GetStringResult(g_interp);
					if (!strcmp(s, "cancel")) break;
				}
#endif /* PLATFORM_X11 */

#ifdef PLATFORM_WINxx
				int oldMode;
				int winResult;

				oldMode = Tcl_SetServiceMode(TCL_SERVICE_ALL);
				winResult = MessageBox(NULL,
					"Your character will not be saved!", "Quit Without Saving",
					MB_ICONEXCLAMATION | MB_OKCANCEL);
				(void) Tcl_SetServiceMode(oldMode);

				if (winResult == IDCANCEL) break;
#endif /* PLATFORM_WIN */
			}
			quit(NULL);
			break;
		}

		case IDX_DIRECTORY: /* directory */
			if (objC < 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "dirname ?path?");
				return TCL_ERROR;
			}
			if (Tcl_GetIndexFromObj(interp, objV[2], keyword_path, "dirname",
				0, &index) != TCL_OK)
			{
				return TCL_ERROR;
			}

			angband_path[0] = &ANGBAND_DIR_ROOT;
			angband_path[1] = &ANGBAND_DIR_USER;
			angband_path[2] = &ANGBAND_DIR_TK;
			angband_path[3] = &ANGBAND_DIR_COMMON;
			angband_path[4] = &ANGBAND_DIR_COMMON_TK;

			if (objC == 4)
			{
				/* Only the sound directory can be changed */
				if (!s_edit_path[index])
				{
					/* Set the error */
					FormatResult(interp, "can't change directory \"%s\"",
						keyword_path[index]);

					/* Failure */
					return TCL_ERROR; 
				}

				/* Get the new directory path */
				t = Tcl_GetString(objV[3]);

				/* Translate the directory path */
				extString = UtfToExt_TranslateFileName(interp, t, &extDString);
				if (extString == NULL) return TCL_ERROR;

				/* Check that the directory exists */
				if (!check_dir(extString))
				{
					/* Set the error */
					FormatResult(interp, "can't access directory \"%s\"", t);

					/* Clean up */
					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}

				/* Free the old directory path */
				string_free(*angband_path[index]);

				/* Save the new directory path */
				*angband_path[index] = string_make(extString);

				/* Clean up */
				Tcl_DStringFree(&extDString);

				/* Done */
				break;
			}

			/* Return the current directory path */
			ExtToUtf_SetResult(interp, (char *) *angband_path[index]);
			break;

		case IDX_FILE_CHARACTER: /* file_character */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "filename");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetStringFromObj(objV[2], NULL);

			/* */
			if (t[0] && t[0] != ' ')
			{
				/* Translate the file path */
				extString = UtfToExt_TranslateFileName(interp, t, &extDString);
				if (extString == NULL) return TCL_ERROR;

				/* Create a character dump */
				if (file_character(extString, FALSE) == -1)
				{
					/* Set the error */
					FormatResult(interp, "character dump failed to \"%s\"", t);

					/* Clean up */
					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}

				/* Clean up */
				Tcl_DStringFree(&extDString);
			}
			break;

		case IDX_MACRO_DUMP: /* macro_dump */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "filename");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetStringFromObj(objV[2], NULL);

			if (t[0] && t[0] != ' ')
			{
				/* Translate the file path */
				extString = UtfToExt_TranslateFileName(interp, t, &extDString);
				if (extString == NULL) return TCL_ERROR;

				/* Dump the macros */
				if (macro_dump(extString) == -1)
				{
					/* Set the error */
					FormatResult(interp, "error writing macro file \"%s\"", t);

					/* Clean up */
					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}

				/* Clean up */
				Tcl_DStringFree(&extDString);
			}
			break;

		case IDX_NEW: /* new */
			if (!g_initialized)
			{
				StaticResult(interp, "game is not initialized");
				return TCL_ERROR;
			}
			if (game_in_progress)
			{
				StaticResult(interp, "game is in progress");
				return TCL_ERROR;
			}
			game_in_progress = 1;
			play_game(TRUE);
			quit(NULL);
			break;

		case IDX_OPEN: /* open */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "filename");
				return TCL_ERROR;
			}
			if (!g_initialized)
			{
				StaticResult(interp, "game is not initialized");
				return TCL_ERROR;
			}
			if (game_in_progress)
			{
				StaticResult(interp, "game is in progress");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetString(objV[2]);

			/* Translate the file name */
			utfString = Tcl_TranslateFileName(interp, t, &utfDString);
			if (utfString == NULL)
			{
				/* Note: Tcl_DStringFree() is called for us */
				return TCL_ERROR;
			}

			/* Test that the file is readable */
			c = Tcl_OpenFileChannel(interp, utfString, "r", 0);
			if (c == (Tcl_Channel) NULL)
			{
				/* Clean up */
				Tcl_DStringFree(&utfDString);

				/* Failure */
				return TCL_ERROR;
			}

			/* Convert UTF8 to native encoding */
			Tcl_UtfToExternalDString(NULL, utfString, -1, &extDString);
			extString = Tcl_DStringValue(&extDString);

			/* Set savefile */
			(void) strcpy(savefile, extString);

			/* Clean up */
			Tcl_DStringFree(&extDString);
			Tcl_DStringFree(&utfDString);
			Tcl_Close(NULL, c);

			/* Play the game (never returns) */
			game_in_progress = 1;
			play_game(FALSE);
			quit(NULL);
			break;

		case IDX_PROCESS_PREF_FILE: /* process_pref_file */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "filename");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetString(objV[2]);

			/* */
			if (t[0] && t[0] != ' ')
			{
				/* Translate the file path */
				extString = UtfToExt_TranslateFileName(interp, t, &extDString);
				if (extString == NULL) return TCL_ERROR;

				/* Read the preferences */
				if (process_pref_file(extString) == -1)
				{
					/* Set the error */
					FormatResult(interp, "error processing pref file \"%s\"", t);

					/* Clean up */
					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}

				/* Clean up */
				Tcl_DStringFree(&extDString);
			}
			break;

		case IDX_QUIT: /* quit */
			if (game_in_progress && character_generated)
			{
				/* Hack -- Forget messages */
				msg_flag = FALSE;

				/* Save the game */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
				do_cmd_save_game();
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(ZANGBANDTK) || defined(OANGBANDTK)
				do_cmd_save_game(FALSE);
#endif /* ZANGBANDTK */
			}
			quit(NULL);
			break;

		case IDX_KEYMAP_DUMP: /* keymap_dump */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "filename");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetString(objV[2]);

			/* */
			if (t[0] && t[0] != ' ')
			{
				/* Translate the file path */
				extString = UtfToExt_TranslateFileName(interp, t, &extDString);
				if (extString == NULL) return TCL_ERROR;

				/* Dump the keymap */
				if (keymap_dump(extString) == -1)
				{
					/* Set the error */
					FormatResult(interp, "error writing keymap file \"%s\"", t);

					/* Clean up */
					Tcl_DStringFree(&extDString);

					/* Failure */
					return TCL_ERROR;
				}

				/* Clean up */
				Tcl_DStringFree(&extDString);
			}
			break;

		case IDX_SAVEFILE_INFO: /* savefile_info */
			if (objC != 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "fileName varName");
				return TCL_ERROR;
			}
			if (!g_initialized)
			{
				StaticResult(interp, "game is not initialized");
				return TCL_ERROR;
			}

			/* Get the file path */
			t = Tcl_GetString(objV[2]);

			/* Get the file path */
			varName = Tcl_GetString(objV[3]);

			/* Translate the file path */
			extString = UtfToExt_TranslateFileName(interp, t, &extDString);
			if (extString == NULL) return TCL_ERROR;

			/* Scan the savefile */
			if (angtk_savefile_info(extString, varName))
			{
				/* Set the error */
				StaticResult(interp, "error parsing savefile");

				/* Clean up */
				Tcl_DStringFree(&extDString);

				/* Failure */
				return TCL_ERROR;
			}

			/* Clean up */
			Tcl_DStringFree(&extDString);
			break;

		case IDX_VERSION: /* version */
#if defined(ANGBANDTK) || defined(KANGBANDTK)
			FormatResult(interp, "%d.%d.%d", version_major,
				version_minor, version_patch);
#endif /* ANGBANDTK, KANGBANDTK */
#if defined(OANGBANDTK)
			FormatResult(interp, "%d.%d.%d", O_VERSION_MAJOR,
				O_VERSION_MINOR, O_VERSION_PATCH);
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
			FormatResult(interp, "%d.%d.%d", FAKE_VER_MAJOR,
				FAKE_VER_MINOR, FAKE_VER_PATCH);
#endif /* ZANGBANDTK */
			break;

		case IDX_VARIANT: /* variant */
			/* Try char *g_variant = "ANGBANDTK" */
#if defined(ANGBANDTK)
			StaticResult(interp, "ANGBANDTK");
#endif /* ANGBANDTK */
#if defined(KANGBANDTK)
			StaticResult(interp, "KANGBANDTK");
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
			StaticResult(interp, "OANGBANDTK");
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
			StaticResult(interp, "ZANGBANDTK");
#endif /* ZANGBANDTK */
			break;

		case IDX_SAVEFILE: /* savefile */
			if (!g_initialized)
			{
				StaticResult(interp, "game is not initialized");
				return TCL_ERROR;
			}

			/* Return current savefile */
			if (objC == 2)
			{
				ExtToUtf_SetResult(interp, savefile);
				break;
			}

			/* Get the file path */
			t = Tcl_GetString(objV[2]);

			/* Translate the file path */
			extString = UtfToExt_TranslateFileName(interp, t, &extDString);
			if (extString == NULL) return TCL_ERROR;

			/* Remember the savefile */
			(void) strcpy(savefile, extString);

			/* Clean up */
			Tcl_DStringFree(&extDString);

			break;
	}

	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_highscore --
 *
 *	Implements the "highscore" script command.
 * 	Syntax:
 *		highscore find -- search for high scores
 *		highscore info -- get info about a highscore
 *		highscore predict -- get predicted character score (if any)
 *
 *--------------------------------------------------------------
 */

bool (*highscore_tester_hook)(high_score *the_score) = NULL;
int highscore_tester_param = 0;
bool highscore_predict = FALSE;

#if defined(KANGBANDTK)

static bool highscore_tester_class(high_score *the_score)
{
	int arena_number, cur_lev, p_c;
	int building = highscore_tester_param;

	p_c = atoi(the_score->p_c);
	cur_lev = atoi(the_score->cur_lev);
	arena_number = atoi(the_score->arena_number);

	if (((p_c == (building - 10)) && (building != 1) && (building != 2)) ||
		((building == 1) && (cur_lev >= PY_MAX_LEVEL)) ||
		((building == 2) && (arena_number > MAX_ARENA_MONS)))
	{
		return (TRUE);
	}
	return (FALSE);
}

static bool highscore_tester_race(high_score *the_score)
{
	int p_r;
	int race_num = highscore_tester_param;

	p_r = atoi(the_score->p_r);
	if (p_r == race_num) return (TRUE);
	return (FALSE);
}

/*
 * show_highclass - selectively list highscores based on class
 * -KMW-
 */
void show_highclass(int building)
{
	char *title;

	highscore_tester_hook = highscore_tester_class;
	highscore_tester_param = building;
	highscore_predict = FALSE;

	switch (building)
	{
		case 1:
			title = "Busts of Greatest Kings";
			break;
		case 2:
			title = "Plaque - Greatest Arena Champions";
			break;
		case 10:
			title = "Plaque - Greatest Fighters";
			break;
		case 11:
			title = "Spires of the Greatest Magic-Users";
			break;
		case 12:
			title = "Busts of Greatest Priests";
			break;
		case 13:
			title = "Wall Inscriptions - Greatest Thieves";
			break;
		case 14:
			title = "Plaque - Greatest Rangers";
			break;
		case 15:
			title = "Plaque - Greatest Paladins";
			break;
		case 16:
			title = "Spires of the Greatest Illusionists";
			break;
#if 1 /* BUG */
		case 17:
			title = "Tree Carvings of the Greatest Druids";
			break;
#endif /* BUG */
		default: 
			title = "Unknown building number!";
			break;
	}

	/* Now, list the active player if they qualify */
	if ((building == 1) && (p_ptr->lev >= PY_MAX_LEVEL))
	{
		highscore_predict = TRUE;
	}
	else if ((building == 2) && (p_ptr->arena_number > MAX_ARENA_MONS))
	{
		highscore_predict = TRUE;
	}
	else if ((building != 1) && (building != 2))
	{
		if (p_ptr->pclass == (building - 10))
		{
			highscore_predict = TRUE;
		}
	}

	angtk_eval("angband_display", "highscore", "show", title, NULL);

	any_more(NULL);

	angtk_eval("angband_display", "highscore", "hide", NULL);

	highscore_tester_hook = NULL;
	highscore_tester_param = 0;
	highscore_predict = FALSE;
}

/*
 * Race Legends
 * -KMW- 
 */
void race_score(int race_num)
{
	char out_val[80];

	highscore_tester_hook = highscore_tester_race;
	highscore_tester_param = race_num;
	highscore_predict = FALSE;

	/* add player if qualified */
	if (p_ptr->prace == race_num) {
		highscore_predict = TRUE;
	}

	(void) sprintf(out_val, "The Greatest heroes of all time (%s)",
		p_name + p_info[race_num].name);
	angtk_eval("angband_display", "highscore", "show", out_val, NULL);

	any_more(NULL);

	highscore_tester_hook = NULL;
	highscore_tester_param = 0;
}

/*
 * Race Legends
 * -KMW-
 */
void race_legends(void)
{
	int i;

	for (i = 0; i < MAX_P_IDX; i++) {
		race_score(i);
	}

	angtk_eval("angband_prompt", "wipe", NULL);
	angtk_eval("angband_display", "highscore", "hide", NULL);
}

#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)

static bool highscore_tester_class(high_score *the_score)
{
	return (TRUE);
}

static bool highscore_tester_race(high_score * the_score)
{
	int p_r;
	int race_num = highscore_tester_param;

	p_r = atoi(the_score->p_r);
	if (p_r == race_num)
		return (TRUE);
	return (FALSE);
}

/*
 * show_highclass - selectively list highscores based on class
 * -KMW-
 */
void show_highclass(int building)
{
	char *title = "Unknown building number!";

	highscore_tester_hook = highscore_tester_class;
	highscore_tester_param = building;
	highscore_predict = FALSE;

	switch (building)
	{
		case 0:
			break; /* Library */
		case 1:
			title = "Busts of Greatest Kings";
			break;
		case 2:
			title = "Plaque - Greatest Arena Champions";
			break;
		case 3:
			break; /* Gambling House */
		case 4:
			break; /* Inn */
		case 5:
			break; /* Beastmaster */
		case 6:
			break; /* Weaponsmaster */
		case 7:
			title = "Plaque - Greatest Fighters";
			break;
		case 8:
			title = "Spires of the Greatest Magic-Users";
			break;
		case 9:
			title = "Busts of Greatest Priests";
			break;
		case 10:
			title = "Wall Inscriptions - Greatest Thieves";
			break;
		case 11:
			title = "Plaque - Greatest Rangers";
			break;
		case 12:
			title = "Shrine to the Greatest Paladins";
			break;
		case 13:
			title = "Spires of the Greatest Illusionists";
			break;
	}

	/* Now, list the active player if they qualify */
	if ((building == 1) && (p_ptr->lev >= PY_MAX_LEVEL))
	{
		highscore_predict = TRUE;
	}
	else if ((building == 2) && (p_ptr->arena_number > MAX_ARENA_MONS))
	{
		highscore_predict = TRUE;
	}
	else if ((building != 1) && (building != 2))
	{
		if (p_ptr->pclass == (building - 10))
		{
			highscore_predict = TRUE;
		}
	}

	angtk_eval("angband_display", "highscore", "show", title, NULL);

	any_more(NULL);

	angtk_eval("angband_display", "highscore", "hide", NULL);

	highscore_tester_hook = NULL;
	highscore_tester_param = 0;
	highscore_predict = FALSE;
}

/*
 * Race Legends
 * -KMW- 
 */
void race_score(int race_num)
{
	char out_val[80];

	highscore_tester_hook = highscore_tester_race;
	highscore_tester_param = race_num;
	highscore_predict = FALSE;

	/* add player if qualified */
	if (p_ptr->prace == race_num)
	{
		highscore_predict = TRUE;
	}

	(void) sprintf(out_val, "The Greatest of all the %s",
		race_info[race_num].title);
	angtk_eval("angband_display", "highscore", "show", out_val, NULL);

	any_more(NULL);

	highscore_tester_hook = NULL;
	highscore_tester_param = 0;
}

/*
 * Race Legends
 * -KMW-
 */
void race_legends(void)
{
	int i;

	for (i = 0; i < MAX_P_IDX; i++)
	{
		race_score(i);
	}

	angtk_eval("angband_display", "highscore", "hide", NULL);
}

#endif /* ZANGBANDTK */

bool highscore_tester_okay(high_score *the_score)
{
	/* Check the hook */
	if (highscore_tester_hook)
	{
		if (!(*highscore_tester_hook)(the_score)) return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}

int highscore_delete(int slot)
{
	char pathBak[1024], pathRaw[1024], pathTmp[1024];
	high_score score;
	int fd, i, result = TCL_OK;
	int mode = 0644;

	path_build(pathBak, 1024, ANGBAND_DIR_APEX, "scores.bak");
	path_build(pathRaw, 1024, ANGBAND_DIR_APEX, "scores.raw");
	path_build(pathTmp, 1024, ANGBAND_DIR_APEX, "scores.tmp");

	/* Create scores.tmp */
	fd_kill(pathTmp);
	FILE_TYPE(FILE_TYPE_DATA);
	fd = fd_make(pathTmp, mode);
	if (fd < 0) return TCL_ERROR;

	for (i = 0; i < MAX_HISCORES; i++)
	{
		/* Read the current score. Stop if we reach the end.*/
		if (highscore_read(&score)) break;

		/* Skip the score to be deleted */
		if (i == slot) continue;

		/* Write the score to the temp file */
		if (fd_write(fd, (char *) &score, sizeof(score)))
		{
			result = TCL_ERROR;
			break;
		}
	}

	/* Close scores.tmp */
	fd_close(fd);

	if (result == TCL_OK)
	{
		/* Close scores.raw */
		fd_close(highscore_fd);
		highscore_fd = -1;

		/* scores.raw --> scores.bak */
		fd_kill(pathBak);
		rename(pathRaw, pathBak);

		/* scores.tmp --> scores.raw */
		rename(pathTmp, pathRaw);
	}

	/* Delete scores.tmp */
	fd_kill(pathTmp);

	return result;
}

int HighScoreToArray(char *arrayName, high_score *the_score)
{
	char *when;

	(void) ExtToUtf_SetArrayValueString(arrayName, "what", the_score->what);
	(void) SetArrayValueLong(arrayName, "pts", atol(the_score->pts));
	(void) SetArrayValueLong(arrayName, "gold", atol(the_score->gold));
	(void) SetArrayValueLong(arrayName, "turns", atol(the_score->turns));
	when = the_score->day;
#if defined(ANGBANDTK) || defined(KANGBANDTK)
	/* Clean up standard encoded form of "when" */
	if ((*when == '@') && strlen(when) == 9)
	{
		char tmp_val[24];
		sprintf(tmp_val, "%.4s-%.2s-%.2s", when + 1, when + 5,
			when + 7);
		when = tmp_val;
	}
#endif /* A */
	(void) ExtToUtf_SetArrayValueString(arrayName, "day", when);
	(void) ExtToUtf_SetArrayValueString(arrayName, "who", the_score->who);
	(void) SetArrayValueLong(arrayName, "uid", atoi(the_score->uid));
	(void) ExtToUtf_SetArrayValueString(arrayName, "sex", the_score->sex);
	(void) SetArrayValueLong(arrayName, "p_r", atoi(the_score->p_r));
	(void) SetArrayValueLong(arrayName, "p_c", atoi(the_score->p_c));
	(void) SetArrayValueLong(arrayName, "cur_lev", atoi(the_score->cur_lev));
	(void) SetArrayValueLong(arrayName, "cur_dun", atoi(the_score->cur_dun));
	(void) SetArrayValueLong(arrayName, "max_lev", atoi(the_score->max_lev));
	(void) SetArrayValueLong(arrayName, "max_dun", atoi(the_score->max_dun));
	(void) ExtToUtf_SetArrayValueString(arrayName, "how", the_score->how);

	/* Extra human-readable names */
	(void) SetArrayValueString(arrayName, "race",
		(char *) keyword_race[atoi(the_score->p_r)]);
	(void) SetArrayValueString(arrayName, "class",
		(char *) keyword_class[atoi(the_score->p_c)]);

#if defined(KANGBANDTK)
	(void) SetArrayValueLong(arrayName, "arena_number", atoi(the_score->arena_number));
	(void) SetArrayValueLong(arrayName, "inside_arena", atoi(the_score->inside_arena));
	(void) SetArrayValueLong(arrayName, "exit_bldg", atoi(the_score->exit_bldg));
#endif /* KANGBANDTK */

	return TCL_OK;
}

int
objcmd_highscore(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"delete", "find", "info", "predict", NULL};
	enum {OPT_DELETE, OPT_FIND, OPT_INFO, OPT_PREDICT} option;
	int index;

	char path[1024], *t;
	high_score the_score;
	int i, old_fd = highscore_fd;
	int request_class = 0, match_class;
	int request_race = 0, match_race;
	int request_tester = 0, match_tester;
	Tcl_Obj *listObjPtr;

	if (objC < 2)
	{
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Open the highscore file, unless it is already open */
	path_build(path, 1024, ANGBAND_DIR_APEX, "scores.raw");
	if (highscore_fd == -1)
	{
		highscore_fd = fd_open(path, O_RDONLY);
		if (highscore_fd < 0)
		{
			StaticResult(interp, "score file unavailable");
			return TCL_ERROR;
		}
	}

	if (highscore_seek(0)) goto noread;

	switch (option)
	{
		case OPT_DELETE: /* delete */
			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objV[2], &index) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (highscore_delete(index) != TCL_OK) goto close_err;
			if (old_fd != -1)
				highscore_fd = fd_open(path, O_RDONLY);
			break;

		case OPT_FIND: /* find */

			/* Scan arguments for options */
			for (i = 2; i < objC; )
			{
				static CONST char *cmdOptions[] = {"-class", "-race", "-tester", NULL};

				/* Get the sub-option */
			    if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					goto close_err;
				}

				switch (index)
				{
					case 0: /* class */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_class)
							!= TCL_OK)
						{
							goto close_err;
						}
						if (match_class < 0 || match_class >= MAX_CLASS) {
							FormatResult(interp,
								"bad class \"%d\": must be between 0 and %d",
								match_class, MAX_CLASS - 1);
							goto close_err;
						}
						request_class = 1;
						i += 2;
						break;

					case 1: /* race */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_race)
							!= TCL_OK)
						{
							goto close_err;
						}
						if (match_race < 0 || match_race >= MAX_P_IDX)
						{
							FormatResult(interp,
								"bad race \"%d\": must be between 0 and %d",
								match_race, MAX_P_IDX - 1);
							goto close_err;
						}
						request_race = 1;
						i += 2;
						break;

					case 2: /* tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						i += 2;
						break;
				}
			}

			/* Return a list of highscore indexes */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Scan highscore list */
			for (i = 0; i < MAX_HISCORES; i++)
			{
				if (highscore_read(&the_score)) break;

				if (request_tester && match_tester)
				{
					if (!highscore_tester_okay(&the_score)) continue;
				}
				if (request_class)
				{
					if (atoi(the_score.p_c) != match_class) continue;
				}
				if (request_race)
				{
					if (atoi(the_score.p_r) != match_race) continue;
				}

				/* Found a match */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(i));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case OPT_INFO: /* info */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index arrayName");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objV[2], &index) != TCL_OK)
			{
				return TCL_ERROR;
			}
			t = Tcl_GetStringFromObj(objV[3], NULL);

			if (highscore_seek(index)) goto noread;
			if (highscore_read(&the_score)) goto noread;

			(void) HighScoreToArray(t, &the_score);
			break;

		case OPT_PREDICT: /* predict */
		    if (objC < 3 || objC > 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "?-force? arrayName");
				return TCL_ERROR;
		    }
			t = Tcl_GetStringFromObj(objV[2], NULL);
			if (objC == 4) /* -force */
			{
				t = Tcl_GetStringFromObj(objV[3], NULL);
			}
			if (highscore_predict || (objC == 4))
			{
				predict_score(&the_score);
				(void) HighScoreToArray(t, &the_score);
			}
			BooleanResult(interp, highscore_predict);
			break;
	}

	/* Close the file if it wasn't already open */
	if (old_fd == -1)
	{
		(void) fd_close(highscore_fd);
		highscore_fd = -1;
	}
	return TCL_OK;

noread:
	StaticResult(interp, "error reading score file");

close_err:
	if (highscore_fd != -1)
	{
		if (old_fd == -1)
		{
			(void) fd_close(highscore_fd);
			highscore_fd = -1;
		}
	}

	return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_info --
 *
 *	Implements the "info" script command.
 * 	Syntax:
 *		info class_name -- get list of class names
 *		info gender_name -- get list of gender names
 *		info race_name -- get list of race names
 *		info tval -- get list of tval names
 *
 *--------------------------------------------------------------
 */

int
objcmd_info(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"class_name", "gender_name", "race_name",
		"tval", "flavor_color", "stat_name", "term_attr",
#if defined(OANGBANDTK)
		"shape_name",
#endif /* */
		NULL};
	enum {IDX_CLASS_NAME, IDX_GENDER_NAME, IDX_RACE_NAME,
		IDX_TVAL, IDX_FLAVOR_COLOR, IDX_STAT_NAME, IDX_TERM_ATTR
#if defined(OANGBANDTK)
		, IDX_SHAPE_NAME
#endif /* */
		} option;
	int index;

	char **names = NULL;
	int i, subIndex = -1, nameCount = 0;
	Tcl_Obj *listObjPtr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?index?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (option == IDX_FLAVOR_COLOR) /* flavor_color */
	{
		extern cptr keyword_term_color[];
		static CONST char *flavorName[] = {"amulet", "mushroom", "potion",
			"ring", "rod", "staff", "wand", NULL};
		if (objC != 3)
		{
			Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "flavor");
			return TCL_ERROR;
		}
	    if (Tcl_GetIndexFromObj(interp, objV[2], flavorName, "flavor", 0, 
			&index) != TCL_OK)
		{
			return TCL_ERROR;
	    }
		listObjPtr = Tcl_NewListObj(0, NULL);
		for (i = 0; i < g_flavor[index].count; i++)
		{
			int c = g_flavor[index].color[i];
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj((char *) keyword_term_color[c], -1));
		}
		Tcl_SetObjResult(interp, listObjPtr);
		return TCL_OK;
	}

	if (objC == 3)
	{
		if (Tcl_GetIntFromObj(interp, objV[2], &subIndex) != TCL_OK)
		{
			return TCL_ERROR;
		}
		if (subIndex < 0) subIndex = -1;
	}

	switch (option)
	{
		case IDX_CLASS_NAME: /* class_name */
			names = (char **) keyword_class;
			nameCount = MAX_CLASS;
			break;

		case IDX_GENDER_NAME: /* gender_name */
			names = (char **) keyword_gender;
			nameCount = MAX_SEXES;
			break;

		case IDX_RACE_NAME: /* race_name */
			names = (char **) keyword_race;
			nameCount = MAX_P_IDX;
			break;

		case IDX_TVAL: /* tval */
			listObjPtr = Tcl_NewListObj(0, NULL);
			for (i = 0; g_tval[i].key; i++)
			{
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(g_tval[i].key, -1));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			return TCL_OK;

		case IDX_STAT_NAME: /* stat_name */
			names = (char **) keyword_stat;
			nameCount = 6;
			break;

		case IDX_TERM_ATTR: /* term_attr */
			names = (char **) keyword_term_color;
			nameCount = 16;
			break;

#if defined(OANGBANDTK)
		case IDX_SHAPE_NAME: /* shape_name */
			names = (char **) keyword_shape;
			nameCount = MAX_SHAPE;
			break;
#endif /* */

		/* Prevent compiler warnings */
		case IDX_FLAVOR_COLOR:
			break;
	}

	if (subIndex != -1)
	{
		if ((subIndex < 0) || (subIndex >= nameCount))
		{
			return TCL_ERROR;
		}
		StaticResult(interp, names[subIndex]);
	}
	else
	{
		listObjPtr = Tcl_NewListObj(0, NULL);
		for (i = 0; i < nameCount; i++)
		{
			Tcl_ListObjAppendElement(interp, listObjPtr,
				Tcl_NewStringObj(names[i], -1));
		}
		Tcl_SetObjResult(interp, listObjPtr);
	}

	return TCL_OK;
}

/* init_icons $width $height $depth $style */
int
objcmd_init_icons(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int width, height, depth, style;

	/* FIXME: from icon-dll.c */
	static CONST char *keyword_icon_style[] = {"icon", "iso", NULL};

	/* Get the width */
	if (Tcl_GetIntFromObj(interp, objV[1], &width) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the height */
	if (Tcl_GetIntFromObj(interp, objV[2], &height) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get the depth */
	if (Tcl_GetIntFromObj(interp, objV[3], &depth) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[4], keyword_icon_style,
		"style", 0, &style) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Reset */
	icons_unload();

	/* Initialize (quit on failure) */
	icons_setup(width, height, depth, style);

	return TCL_OK;
}

/* Strings returned by "inkey_flags" command, indexed by INKEY_XXX defines. */
cptr inkey_to_str[] = {"", "INKEY_CMD", "INKEY_DIR", "INKEY_DISTURB",
	"INKEY_ITEM", "INKEY_ITEM_STORE", "INKEY_MORE", "INKEY_SPELL",
	"INKEY_TARGET",
#if defined(KANGBANDTK)
	"INKEY_CMD_PET",
#endif /* KANGBANDTK */
#if defined(OANGBANDTK)
	"INKEY_ELE_ATTACK",
#endif /* OANGBANDTK */
#if defined(ZANGBANDTK)
	"INKEY_MINDCRAFT", "INKEY_POWER", "INKEY_CMD_PET",
#endif /* ZANGBANDTK */
	NULL};

/* (inkey) flags */
int
objcmd_inkey_flags(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	Tcl_SetResult(interp, (char *) inkey_to_str[inkey_flags], TCL_VOLATILE);
	return TCL_OK;
}

/* (inkey) options */
int
objcmd_inkey_options(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	Tcl_SetResult(interp, inkey_options ? (char *) inkey_options : "",
		TCL_VOLATILE);
	return TCL_OK;
}

/* (inkey) other */
int
objcmd_inkey_other(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	if (inkey_flags == INKEY_ITEM)
	{
#if defined(ANGBANDTK) || defined(KANGBANDTK)

		Tcl_SetResult(interp,
			(p_ptr_command_wrk == (USE_EQUIP)) ? "equipment" : 
			(p_ptr_command_wrk == (USE_INVEN)) ? "inventory" :
			"floor", TCL_VOLATILE);

#endif /* ANGBANDTK, KANGBANDTK */
#if defined(OANGBANDTK) || defined(ZANGBANDTK)

#ifdef ALLOW_EASY_FLOOR 

		if (easy_floor)
		{
			Tcl_SetResult(interp,
				(p_ptr_command_wrk == (USE_EQUIP)) ? "equipment" : 
				(p_ptr_command_wrk == (USE_INVEN)) ? "inventory" :
				"floor", TCL_VOLATILE);
		}
		else

#endif /* ALLOW_EASY_FLOOR */

		Tcl_SetResult(interp,
			p_ptr_command_wrk ? "equipment" : "inventory", TCL_VOLATILE);

#endif /* O, Z */
	}
	else if (inkey_flags == INKEY_SPELL)
	{
		Tcl_SetObjResult(interp, Tcl_NewIntObj(inkey_book));
	}

	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_inventory --
 *
 *	Implements the "inventory" script command.
 * 	Syntax:
 *
 *		inventory count
 *			Return number of inventory items carried
 *
 *		inventory find SEARCHCOMMAND ?arg arg ...?
 * 			Return list of indexes of matching objects
 *
 *		inventory info INDEX VARNAME
 *			Return info about specific object
 *
 *		inventory memory INDEX
 *			Return memory about about specific object
 *
 *		inventory total_weight
 *			Return total weight carried
 *
 *		inventory weight_limit
 *			Return carrying capacity in 10ths of pounds
 *
 *--------------------------------------------------------------
 */

int
objcmd_inventory(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"count", "find", "info", "memory",
		"total_weight", "weight_limit", "inscription", "worthless",
		NULL};
	enum {IDX_COUNT, IDX_FIND, IDX_INFO, IDX_MEMORY,
		IDX_TOTAL_WEIGHT, IDX_WEIGHT_LIMIT, IDX_INSCRIPTION, IDX_WORTHLESS
	} option;
	int index;

	Tcl_Obj *listObjPtr;
	int i_idx, k_idx, i, z = 0, tval;
	char buf[80], *buffer, *varName, *t;
	object_type *o_ptr;
	long length;

	/* Default to no restriction on k_idx */
	int request_k_idx = 0, match_k_idx[10], k_idx_cnt = 0;

	/* Default to finding all matches */
	int request_limit = 0, match_limit = 0, cnt = 0;

	/* Default to ignoring store_will_buy() */
	int request_store = 0, match_store = 0;

	/* Default to ignoring item_tester_okay() hook */
	int request_tester = 0, match_tester = 0;

	/* Default to no restriction on tval */
	int request_tval = 0, match_tval[10], tval_cnt = 0;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	/* Find the "final" slot */
	for (i_idx = 0; i_idx < INVEN_PACK; i_idx++)
	{
		/* Get the n'th item */
		o_ptr = &inventory[i_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i_idx + 1;
	}

	switch (option)
	{
		case IDX_COUNT: /* count */
			IntResult(interp, z);
			break;

		case IDX_FIND: /* find */
		{
			extern bool item_tester_hook_cast(const object_type *o_ptr);
			extern bool item_tester_hook_study(const object_type *o_ptr);

			bool (*old_tester_hook)(const object_type *) = item_tester_hook;
			bool (*temp_tester_hook)(const object_type *) = NULL;

			/* Scan arguments for options */
			for (i = 2; i < objC; i += 2)
			{
				static CONST char *cmdOptions[] = {"-hook", "-k_idx", "-limit",
					"-tester", "-tval", "-store_will_buy", NULL};

				/* Get the sub-option */
			    if (Tcl_GetIndexFromObj(interp, objV[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
			    }

				if (i + 1 == objC)
				{
					FormatResult(interp, "missing value for \"%s\" option",
						cmdOptions[index]);
					return TCL_ERROR;
				}

				switch (index)
				{
					case 0: /* hook */
						t = Tcl_GetStringFromObj(objV[i+1], NULL);
						if (streq(t, "cast"))
							temp_tester_hook = item_tester_hook_cast;
						else if (streq(t, "study"))
							temp_tester_hook = item_tester_hook_study;
						else
						{
							Tcl_SetResult(interp, format("unknown hook \"%s\"",
								t), TCL_VOLATILE);
							return TCL_ERROR;
						}
						request_tester = 1;
						match_tester = 1;
						break;

					case 1: /* -k_idx */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &k_idx)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						match_k_idx[k_idx_cnt++] = k_idx;
						request_k_idx = 1;
						break;

					case 2: /* -limit */
						if (Tcl_GetIntFromObj(interp, objV[i+1], &match_limit)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						request_limit = 1;
						break;

					case 3: /* -tester */
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_tester) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_tester = 1;
						break;

					case 4: /* -tval */
						t = Tcl_GetStringFromObj(objV[i+1], NULL);
						if (angtk_tval_const(&tval, t) != TCL_OK)
						{
							return TCL_ERROR;
						}
						match_tval[tval_cnt++] = tval;
						request_tval = 1;
						break;

					case 5: /* -store_will_buy */
						if (!storedata.shopping)
						{
							StaticResult(interp, "character isn't shopping");
							return TCL_ERROR;
						}
						if (Tcl_GetBooleanFromObj(interp, objV[i+1],
							&match_store) != TCL_OK)
						{
							return TCL_ERROR;
						}
						request_store = 1;
						break;
				}
			}

			if (temp_tester_hook) item_tester_hook = temp_tester_hook;

			/* Return a list of inventory[] indexes */
			listObjPtr = Tcl_NewListObj(0, NULL);

			/* Scan inventory list */
			for (i_idx = 0; i_idx < z; i_idx++)
			{
				/* Get the n'th item */
				o_ptr = &inventory[i_idx];

				/* Return items by k_info[] index */
				if (request_k_idx)
				{
					for (i = 0; i < k_idx_cnt; i++)
					{
						if (match_k_idx[0] == o_ptr->k_idx) break;
					}
					if (i == k_idx_cnt) continue;
				}

				/* Return "okay" items */
				if (request_tester && match_tester)
				{
					if (!item_tester_okay(o_ptr)) continue;
				}

				/* Return items the store will buy */
				if (request_store && match_store)
				{
					if (!store_will_buy(o_ptr)) continue;
				}

				/* Return items by tval */
				if (request_tval)
				{
					for (i = 0; i < tval_cnt; i++)
					{
						if (match_tval[0] == o_ptr->tval) break;
					}
					if (i == tval_cnt) continue;
				}

				/* Found a match */
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(i_idx));

				/* Return x matches */
				if (request_limit && (++cnt >= match_limit)) break;
			}

			/* XXX Hack -- Restore the hook */
			item_tester_hook = old_tester_hook;

			/* Return the list of indexes */
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

		case IDX_INFO: /* info */

		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index arrayName");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((i_idx < 0) || (i_idx >= z))
			{
				goto bad_index;
			}

			/* Get the array variable name to dump results in */
			varName = Tcl_GetStringFromObj(objV[3], NULL);

			/* Get item info */
			o_ptr = &inventory[i_idx];

			if (dump_object_info(varName, o_ptr, i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_MEMORY: /* memory */

			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((i_idx < 0) || (i_idx >= z))
			{
				goto bad_index;
			}

			/* Get item info */
			o_ptr = &inventory[i_idx];

			buffer = Tcl_Alloc(5 * 1024L);
			length = angtk_describe_object(o_ptr, buffer, FALSE);
			Tcl_SetObjResult(interp, ExtToUtf_NewStringObj(buffer, length));
			Tcl_Free(buffer);
			break;

		case IDX_TOTAL_WEIGHT: /* total_weight */
			IntResult(interp, p_ptr->total_weight);
			break;

		case IDX_WEIGHT_LIMIT: /* weight_limit */

			/* Max carrying capacity in 10ths of pounds */
			i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;
			IntResult(interp, i);
			break;

		case IDX_INSCRIPTION: /* inscription */

		    if (objC < 3)
		    {
		    	/* Set the error */
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index ?string?");

				/* Failure */
				return TCL_ERROR;
		    }

			/* Get the item index */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify i_idx */
			if ((i_idx < 0) || (i_idx >= z))
			{
				goto bad_index;
			}

			/* Get item info */
			o_ptr = &inventory[i_idx];

			/* Set the inscription. */
			if (objC == 4)
			{
				/* Get the new inscription */
				t = Tcl_GetStringFromObj(objV[3], NULL);

				if (strlen(t))
				{
					/* Save the inscription */
					set_user_inscription(o_ptr, quark_add(t));
				}
				else
				{
					/* Clear the inscription */
					set_user_inscription(o_ptr, 0);
				}

				/* Combine the pack */
				p_ptr->notice |= (PN_COMBINE);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN | PW_EQUIP);

				/* Done */
				break;
			}

			/* Get the current inscription, if any */
			(void) object_inscription(o_ptr, buf);

			/* Return the current inscription */
			ExtToUtf_SetResult(interp, buf);

			/* Done */
			break;

		case IDX_WORTHLESS: /* worthless */
		{
			int worthless;

		    if (objC != 3)
		    {
		    	/* Set the error */
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index");

				/* Failure */
				return TCL_ERROR;
		    }

			/* Get the item index */
			if (Tcl_GetIntFromObj(interp, objV[2], &i_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}

			/* Verify i_idx */
			if ((i_idx < 0) || (i_idx >= z))
			{
				goto bad_index;
			}

			/* Get item info */
			o_ptr = &inventory[i_idx];

			/* Is it worthless? */
			worthless = (object_value(o_ptr) < 1) ||
				((o_ptr->tval == TV_CHEST) && (o_ptr->pval == 0))
#ifdef TNB_SQUELCH
				|| object_squelch_p(o_ptr)
#endif
			;
			BooleanResult(interp, worthless);
			break;
		}
	}

	return TCL_OK;

bad_index:
	FormatResult(interp,
		"bad inventory index \"%s\": must be between 0 and %d",
		Tcl_GetString(objV[2]), z - 1);
	return TCL_ERROR;
}

/* keycount */
int
objcmd_keycount(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	BooleanResult(interp, Term->key_head != Term->key_tail);
	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_keymap --
 *
 *	Implements the "keymap" script command.
 * 	Syntax:
 *		keymap action KEY ?ACTION? -- get/set a keymap's action
 *		keymap find STRING -- find keymap with underlying action
 *
 *--------------------------------------------------------------
 */

int
objcmd_keymap(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
#if !defined(ZANGBANDTK) || defined(KEYSTUFF_283)

	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"action", "find", NULL};
	enum {IDX_ACTION, IDX_FIND} option;

	bool cntrl = FALSE;
	char *t, *t2, buf[80];
	int ch, i, mode, n;
/*	int j, match, max_len; */

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	/* Roguelike */
	if (rogue_like_commands)
	{
		mode = KEYMAP_MODE_ROGUE;
	}

	/* Original */
	else
	{
		mode = KEYMAP_MODE_ORIG;
	}

	switch (option)
	{
		case IDX_ACTION: /* action */

			if (objC < 3 || objC > 4)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "key action");
				return TCL_ERROR;
			}

			t = Tcl_GetStringFromObj(objV[2], NULL);
			n = strlen(t);

			if (!n)
			{
				StaticResult(interp, "expected keypress but got \"\"");
				return TCL_ERROR;
			}

			ch = t[0];

			/* XXX Hack -- Convert control sequence */
			if ((n >= 2) && (ch == '^'))
			{
				ch = KTRL(t[1]);
			}

			/* Define new action, or delete the keymap */
			if (objC == 4)
			{
				/* Get the action */
				t2 = Tcl_GetStringFromObj(objV[3], NULL);

				/* Free old keymap */
				string_free(keymap_act[mode][(byte)ch]);

				/* Delete the keymap if empty action given */
				/* Delete the keymap if is identity keymap */
				if ((t2[0] == '\0') || !strcmp(t, t2))
				{
					keymap_act[mode][(byte)ch] = NULL;
				}
				else
				{
					/* Printable --> Ascii */
					text_to_ascii(buf, t2);

					/* Make new keymap */
					keymap_act[mode][(byte)ch] = string_make(buf);
				}

				/* Let the world know the keymap changed */
				Bind_Keymap(ch);
			}

			if (keymap_act[mode][(byte)ch] == NULL)
			{
				/* No action is defined */
				strcpy(buf, "");

			}
			else
			{
				/* Ascii --> Printable */
				ascii_to_text(buf, keymap_act[mode][(byte)ch]);
			}

			/* Return action */
			StringResult(interp, buf);

			break;

#if 1 /* July 11 2004 */

		case IDX_FIND: /* find */
		{
			Tcl_Obj *listObjPtr;

			if (objC < 2 || objC > 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "?action?");
				return TCL_ERROR;
			}

			if (objC == 2)
			{
				listObjPtr = Tcl_NewListObj(0, NULL);

				for (i = 0; i < 256; i++)
				{
					cptr action = keymap_act[mode][i];

					if (!action) continue;

					if (iscntrl(i))
						t = format("^%c", (char) (i + 64));
					else
						t = format("%c", (char) i);

					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewStringObj(t, -1));

					ascii_to_text(buf, action);
					Tcl_ListObjAppendElement(interp, listObjPtr,
						Tcl_NewStringObj(buf, -1));
				}
				Tcl_SetObjResult(interp, listObjPtr);
				break;
			}

			t = Tcl_GetStringFromObj(objV[2], NULL);
			n = strlen(t);
			if (!n)
			{
				StaticResult(interp, "expected keypress but got \"\"");
				return TCL_ERROR;
			}

			/* Return list of every matching keypress */
			listObjPtr = Tcl_NewListObj(0, NULL);

			ch = t[0];

			/* XXX Hack -- Convert control sequence */
			if ((n == 2) && (ch == '^'))
			{
				ch = KTRL(t[1]);
				cntrl = TRUE;
			}

			/*
			 * If the given underlying sequence is a single character,
			 * and no keymap exists for that character, then return the
			 * given character. For example, "w" maps to "w" under the
			 * original keyset.
			 */
			if (((n == 1) || cntrl) && (keymap_act[mode][(byte)ch] == NULL))
			{
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(t, -1));
			}

			/* Printable --> Ascii */
			text_to_ascii(buf, t);

			for (i = 0; i < 256; i++)
			{
				cptr action = keymap_act[mode][i];

				/* Skip unassigned keymap */
				if (!action) continue;

				if (strcmp(action, buf)) continue;

				if (iscntrl(i))
				{
					t = format("^%c", (char) (i + 64));
				}
				else
				{
					t = format("%c", (char) i);
				}

				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(t, -1));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

#else /* July 11 2004 */

		case IDX_FIND: /* find */

			if (objC != 3)
			{
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "string");
				return TCL_ERROR;
			}

			t = Tcl_GetStringFromObj(objV[2], NULL);
			n = strlen(t);
			if (!n)
			{
				StaticResult(interp, "expected keypress but got \"\"");
				return TCL_ERROR;
			}

			ch = t[0];

			/* XXX Hack -- Convert control sequence */
			if ((n >= 2) && (ch == '^'))
			{
				ch = KTRL(t[1]);
				cntrl = TRUE;
			}

			match = 0;
			max_len = 128;

			/*
			 * If the given underlying sequence is a single character,
			 * and no keymap exists for that character, then return the
			 * given character. For example, "w" maps to "w" under the
			 * original keyset.
			 */
			if (((n == 1) || cntrl) && (keymap_act[mode][(byte)ch] == NULL))
			{
				match = ch;
				max_len = 1;
			}

			/* Printable --> Ascii */
			text_to_ascii(buf, t);

			n = strlen(buf);

			/* Look for the shortest matching action */
			for (i = 0; i < 256; i++)
			{
				cptr action = keymap_act[mode][i];

				/* Skip unassigned keymap */
				if (!action) continue;

				/* strncmp(action, buf, n) */
				for (j = 0; j < n; j++)
				{
					if (action[j] != buf[j]) break;
				}
				if (j < n) continue;

				/* strlen(action) */
				while (action[j]) j++;

				/* Remember the trigger with the shortest action */
				if (j < max_len)
				{
					max_len = j;
					match = i;
				}

				/* Prefer non-control trigger */
				else if (match && (j == max_len))
				{
					if (iscntrl(match) && !iscntrl(i))
					{
						match = i;
					}
				}
			}
			if (match)
			{
				if (iscntrl(match))
				{
					FormatResult(interp, "^%c", (char) (match + 64));
				}
				else
				{
					FormatResult(interp, "%c", (char) match);
				}
			}
			break;

#endif /* July 11 2004 */

	}

#endif /* KEYSTUFF_283 */

	/* Success */
	return TCL_OK;
}

/* keypress $string */
int
objcmd_keypress(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	char *t;
	int i;

	t = Tcl_GetStringFromObj(objV[1], NULL);
	for (i = 0; t[i]; i++)
	{
		Term_keypress(t[i]);
	}
	return TCL_OK;
}

/* (array) find ?arg ...? */
int
objcmd_ARRAY_find(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	char *arrayName = (char *) infoCmd->clientData;

	return Struct_Find(interp, Struct_Lookup(interp, arrayName),
		objc, objv, infoCmd->depth + 1);
}

/* (array) info $index ?arg ...? */
int
objcmd_ARRAY_info(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	char *arrayName = (char *) infoCmd->clientData;
	StructType *typePtr = Struct_Lookup(interp, arrayName);
	int elemIndex;

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[1])
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Info(interp, typePtr, elemIndex, objc, objv,
		infoCmd->depth + 2);
}

/* (array) max */
int
objcmd_ARRAY_max(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	char *arrayName = (char *) infoCmd->clientData;

	StructType *typePtr = Struct_Lookup(interp, arrayName);
	Tcl_SetObjResult(interp, Tcl_NewIntObj(typePtr->max));
	return TCL_OK;
}

/* (array) set $index ?$field? ?$value? */
int
objcmd_ARRAY_set(ClientData clientData, Tcl_Interp *interp, int objc,
	Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;
	char *arrayName = (char *) infoCmd->clientData;

	StructType *typePtr = Struct_Lookup(interp, arrayName);
	int elemIndex;

	/* Get the array index */
	if (Struct_GetArrayIndexFromObj(interp, typePtr, &elemIndex, objV[1])
		!= TCL_OK)
	{
		return TCL_ERROR;
	}

	return Struct_Set(interp, typePtr, elemIndex, objc, objv,
		infoCmd->depth + 2);
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_macro --
 *
 *	Implements the "macro" script command.
 * 	Syntax:
 *		macro action INDEX ?ACTION? -- get/set a macro's action
 *		macro command INDEX ?BOOLEAN? -- get/set a macro's "command" flag
 *		macro create KEYPRESS -- create a new macro with given keypress
 *		macro delete INDEX -- delete a macro
 *		macro keypress INDEX -- get/set a macro's trigger keypress
 *		macro max -- get number of macros
 *
 *--------------------------------------------------------------
 */

int
objcmd_macro(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"action", "command", "create", "delete",
		"keypress", "max", NULL};
	enum {IDX_ACTION, IDX_COMMAND, IDX_CREATE, IDX_DELETE,
		IDX_KEYPRESS, IDX_MAX} option;

	int i, n;
	char *t, buf[80];

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_ACTION: /* action */
			if (Tcl_GetIntFromObj(interp, objV[2], &n) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (n < 0 || n >= macro__num) goto bad_index;
			if (objC == 4)
			{
				/* Get the action */
				t = Tcl_GetStringFromObj(objV[3], NULL);

				/* Printable --> Ascii */
				text_to_ascii(buf, t);

				/* Free the old macro action */
				string_free(macro__act[n]);

				/* Save the macro action */
				macro__act[n] = string_make(buf);
			}

			/* Ascii --> Printable */
			ascii_to_text(buf, macro__act[n]);

			/* Return action */
			StringResult(interp, buf);
			break;

		case IDX_COMMAND: /* command */
#if defined(ZANGBANDTK) && !defined(KEYSTUFF_283)
			if (Tcl_GetIntFromObj(interp, objV[2], &n) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (n < 0 || n >= macro__num) goto bad_index;
			if (objC == 4)
			{
				int cmd_flag;

				if (Tcl_GetBooleanFromObj(interp, objV[3], &cmd_flag) != TCL_OK)
				{
					return TCL_ERROR;
				}

				/* Save the "cmd_flag" */
				macro__cmd[n] = cmd_flag;
			}

			/* Return "cmd_flag" */
			BooleanResult(interp, macro__cmd[n]);
			break;
#else /* KEYSTUFF_283 */
			return TCL_OK; /* not implemented in 2.8.3 */
#endif /* KEYSTUFF_283 */

		case IDX_CREATE: /* create */

			/* Get the keypress */
			t = Tcl_GetStringFromObj(objV[2], NULL);

			/* Printable --> Ascii */
			text_to_ascii(buf, t);

#if !defined(ZANGBANDTK) || defined(KEYSTUFF_283) /* TNB */

			/* Look for an existing macro */
			if ((n = macro_find_exact(buf)) != -1)
			{
				IntResult(interp, n);
				break;
			}

			/* Create a new macro */
			/* VC++ doesn't do "\e" */
			macro_add(buf, "\033\033\033");

#else /* KEYSTUFF_283 -- TNB */

			/* Look for an existing macro */
			for (n = 0; n < macro__num; n++)
			{
				/* Notice macro redefinition */
				if (streq(macro__pat[n], buf))
				{
					IntResult(interp, n);
					return TCL_OK;
				}
			}

			/* Create an empty (normal) macro */
			macro_add(buf, "", FALSE);

#endif /* KEYSTUFF_283 -- TNB */

			/* Return the macro index */
			IntResult(interp, macro__num - 1);
			break;

		case IDX_DELETE: /* delete */
/*			StaticResult(interp, "macros cannot be deleted (yet)");
			return TCL_ERROR;*/
			if (Tcl_GetIntFromObj(interp, objV[2], &n) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (n < 0 || n >= macro__num) goto bad_index;
extern void macro_delete(int n);
			macro_delete(n);
			return TCL_OK;

		case IDX_KEYPRESS: /* keypress */
			if (Tcl_GetIntFromObj(interp, objV[2], &n) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (n < 0 || n >= macro__num) goto bad_index;

			if (objC == 4)
			{
				/* Get the trigger */
				t = Tcl_GetStringFromObj(objV[3], NULL);

				/* Printable --> Ascii */
				text_to_ascii(buf, t);

#if !defined(ZANGBANDTK) || defined(KEYSTUFF_283) /* TNB */

				/* Look for an existing macro */
				if ((i = macro_find_exact(buf)) != -1)
				{
					IntResult(interp, i);
					break;
				}

#else /* KEYSTUFF_283 -- TNB */

				/* Look for an existing macro */
				for (n = 0; n < macro__num; n++)
				{
					/* Notice macro redefinition */
					if (streq(macro__pat[n], buf))
					{
						IntResult(interp, n);
						break;
					}
				}

#endif /* KEYSTUFF_283 -- TNB */

				/* Free the old macro pattern */
				string_free(macro__pat[n]);

				/* Save the macro pattern */
				macro__pat[n] = string_make(buf);

				/* Return macro index */
				IntResult(interp, n);
				break;
			}

			/* Ascii --> Printable */
			ascii_to_text(buf, macro__pat[n]);

			/* Return keypress */
			StringResult(interp, buf);
			break;

		case IDX_MAX: /* max */
			IntResult(interp, macro__num);
			break;
	}

	return TCL_OK;

bad_index:
	FormatResult(interp, "bad macro index \"%s\": must be between %d and %d",
		Tcl_GetString(objV[2]), 0, macro__num - 1);
	return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_message --
 *
 *	Implements the "message" script command.
 * 	Syntax:
 *		message color -- Return color for message $index
 *		message count -- Return number of saved messages
 *		message get $index -- Return most-recent number of messages
 *		message sound $index -- Return sound for message $index
 *
 *--------------------------------------------------------------
 */

int
objcmd_message(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOption[] = {"color", "count", "get", "sound", NULL};
	enum {IDX_COLOR, IDX_COUNT, IDX_GET, IDX_SOUND} option;

	int i, k;
	byte attr;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOption, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	if (!character_generated)
	{
		StaticResult(interp, "character has not been generated yet");
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_COLOR: /* color */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			k = message_num();
			if (i < 0 || i >= k)
			{
				FormatResult(interp, "invalid message index \"%d\": "
					"must be from 0 to %d", i, k - 1);
				return TCL_ERROR;
			}
#ifdef SND_MSG_COLORS
			extern int message_snd_color(s16b age);
			attr = message_snd_color(i);
			if (attr >= 16) attr = TERM_WHITE;
#else
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			attr = message_color(i);
			if (attr >= 16) attr = TERM_WHITE;
#endif /* A, K, O */
#if defined(ZANGBANDTK)
			attr = TERM_WHITE;
#endif
#endif
			StaticResult(interp, (char *) keyword_term_color[attr]);
			break;

		case IDX_COUNT: /* count */
			IntResult(interp, message_num());
			break;

		case IDX_GET: /* get */
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			k = message_num();
			if (i < 0 || i >= k)
			{
				FormatResult(interp, "invalid message index \"%d\": "
					"must be from 0 to %d", i, k - 1);
				return TCL_ERROR;
			}
			ExtToUtf_SetResult(interp, (char *) message_str(i));
			break;

		case IDX_SOUND: /* sound */
		{
			extern void message_snd(Tcl_Interp *interp, s16b age);

			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			k = message_num();
			if (i < 0 || i >= k)
			{
				FormatResult(interp, "invalid message index \"%d\": "
					"must be from 0 to %d", i, k - 1);
				return TCL_ERROR;
			}
			message_snd(interp, i);
			break;
		}
	}

	return TCL_OK;
}

#if defined(ZANGBANDTK)

/*
 *--------------------------------------------------------------
 *
 * objcmd_mindcraft --
 *
 *	Implements the "mindcraft" script command. Usage:
 *	> Get a list of allowable mindcraft power indexes:
 *		mindcraft get
 *	> Get info about a mindcraft power by index:
 *		mindcraft info $index $arrayName
 *
 *--------------------------------------------------------------
 */

int
objcmd_mindcraft(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {"get", "info", NULL};
	enum {IDX_GET, IDX_INFO} option;

	mindcraft_power *pow_ptr;
	Tcl_Obj *listObjPtr;
	int i, chance, min_fail;
	char comment[80], *s = comment, *t;

    if (objC < 2)
    {
		Tcl_WrongNumArgs(interp, infoCmd->depth + 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_GET: /* get */
			listObjPtr = Tcl_NewListObj(0, NULL);
			for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
			{
				pow_ptr = &mindcraft_powers[i];
/*				if (pow_ptr->min_lev > p_ptr->lev) break; */

				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewIntObj(i));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;

		case IDX_INFO: /* info */
		    if (objC != 4)
		    {
				Tcl_WrongNumArgs(interp, infoCmd->depth + 2, objv, "index arrayName");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objV[2], &i) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (i < 0 || i >= MAX_MINDCRAFT_POWERS)
			{
				return TCL_ERROR;
			}
			t = Tcl_GetStringFromObj(objV[3], NULL);

			pow_ptr = &mindcraft_powers[i];

			chance = pow_ptr->fail;

			/* Reduce failure rate by "effective" level adjustment */
			chance -= 3 * (p_ptr->lev - pow_ptr->min_lev);

			/* Reduce failure rate by INT/WIS adjustment */
			chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

			/* Not enough mana to cast */
			if (pow_ptr->mana_cost > p_ptr->csp)
			{
				chance += 5 * (pow_ptr->mana_cost - p_ptr->csp);
			}

			/* Extract the minimum failure rate */
			min_fail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

			/* Minimum failure rate */
			if (chance < min_fail) chance = min_fail;

			/* Stunning makes spells harder */
			if (p_ptr->stun > 50) chance += 25;
			else if (p_ptr->stun) chance += 15;

			/* Always a 5 percent chance of working */
			if (chance > 95) chance = 95;

			/* Get info */
			mindcraft_info(s, i);
			if (s[0]) s++;

			SetArrayValueString(t, "char", format("%c", I2A(i)));
			ExtToUtf_SetArrayValueString(t, "name", (char *) pow_ptr->name);
			SetArrayValueLong(t, "level", pow_ptr->min_lev);
			SetArrayValueLong(t, "fail", chance);
			SetArrayValueLong(t, "mana", pow_ptr->mana_cost);
			SetArrayValueString(t, "comment", s);
			SetArrayValueLong(t, "okay", pow_ptr->min_lev <= p_ptr->lev);
			break;
	}

	return TCL_OK;
}

#endif /* ZANGBANDTK */

