/* File: debug.c */

/* Purpose: debugging stuff */

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
#include "qebind-dll.h" /* for debug_bindings */
#include "util-dll.h"

int debug_commands = 0;
DLLIMPORT int debug_widgets;
int wiz_see_monsters = 0;
int wiz_auto_lite = 0;
int wiz_free_moves = 0;

static int do_debug_cmd(int index)
{
	char ybuf[10], xbuf[10];
	int y, x;

	switch (index)
	{
		case 2: /* auto_lite */
			wiz_auto_lite = !wiz_auto_lite;
			break;

		case 4: /* cure_all */
			break;

		case 5: /* detection */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			detect_all();
#endif
#if defined(OANGBANDTK)
			detect_all(DETECT_RAD_MAP, FALSE);
#endif
			break;

		case 6: /* free_moves */
			wiz_free_moves = !wiz_free_moves;
			break;

		case 7: /* gain_experience */
			if (p_ptr_command_arg)
			{
				gain_exp(p_ptr_command_arg);
			}
			else
			{
				gain_exp(p_ptr->exp + 1);
			}
			break;

		case 8: /* go_to_level */
			break;

		case 13: /* magic_mapping */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			map_area();
#endif
#if defined(OANGBANDTK)
			map_area(0, 0, FALSE);
#endif
			break;

		case 14: /* mass_genocide */
			break;

		case 15: /* phase_door */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			teleport_player(10);
#endif
#if defined(OANGBANDTK)
			teleport_player(10, TRUE);
#endif
			break;

		case 16: /* rerate_hitpoints */
			break;

		case 17: /* see_monsters */
			wiz_see_monsters = !wiz_see_monsters;
			break;

		case 20: /* teleport */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			teleport_player(100);
#endif
#if defined(OANGBANDTK)
			teleport_player(100, TRUE);
#endif
			break;

		case 21: /* teleport_to_target */
			break;

		case 22: /* wiz_dark */
			wiz_dark();
			break;

		case 23: /* wiz_lite */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
			wiz_lite();
#endif /* */
#if defined(OANGBANDTK)
			wiz_lite(TRUE);
#endif /* */
			break;

		case 24: /* teleport_to_location */
			if (get_string("y: ", ybuf, 3) && get_string("x: ", xbuf, 3))
			{
				y = atoi(ybuf);
				x = atoi(xbuf);
				teleport_player_to(y, x);
			}
			break;
	}

	return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_debug_command --
 *
 *	Implements the "debug command" script command.
 *
 *--------------------------------------------------------------
 */

int
objcmd_debug_command(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOptions[] = {"acquirement", "alloc_artifact", "auto_lite",
		"alloc_object", "cure_all", "detection", "free_moves",
		"gain_experience", "go_to_level", "identify", "identify_fully",
		"identify_many", "identify_pack", "magic_mapping", "mass_genocide",
		"phase_door", "rerate_hitpoints", "see_monsters", "self_knowledge",
		"summon_monster", "teleport", "teleport_to_target", "wizard_dark",
		"wizard_lite", "teleport_to_location", NULL};
	int index;

    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objv[1], cmdOptions, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	return do_debug_cmd(index);
}

/*
 *--------------------------------------------------------------
 *
 * objcmd_debug --
 *
 *	Implements the "debug" script command.
 *
 *--------------------------------------------------------------
 */

int
objcmd_debug(ClientData dummy, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOptions[] = {"command", "create_item", "summon_specific",
		"create_artifact", NULL};
	int index;
	
	int i, a_idx, k_idx, r_idx;
	int y, x, y1, x1;
	object_type *o_ptr;
	object_type object_type_body;
	
    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objv[1], cmdOptions, "option", 0, 
		&index) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (index)
	{
		case 0: /* command */
			return objcmd_debug_command(dummy, interp, objc - 1,
				objv + 1);

		case 1: /* create_item */
		    if (objc < 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "k_idx ?y x?");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objv[2], &k_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			o_ptr = &object_type_body;
			object_prep(o_ptr, k_idx);
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			apply_magic(o_ptr, p_ptr_depth, FALSE, FALSE, FALSE);
#endif /* */
#if defined(ZANGBANDTK)
			apply_magic(o_ptr, p_ptr_depth, FALSE, FALSE, FALSE, FALSE);
#endif /* */
			drop_near(o_ptr, -1, p_ptr_py, p_ptr_px);
			break;

		case 2: /* summon_specific */
		{
			int friend = FALSE, group = FALSE, pet = FALSE, sleep = FALSE;
			
		    if (objc < 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "r_idx ?-group boolean? ?-sleep boolean?");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objv[2], &r_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if (!monster_race_valid(r_idx))
			{
				FormatResult(interp,
					"bad r_info index \"%d\": must be between 0 and %d",
					r_idx, (int) MAX_VALID_R_IDX - 1);
				return TCL_ERROR;
			}

			/* Scan arguments for options */
			for (i = 3; i < objc; )
			{
				static CONST char *cmdOptions[] = {"-friend", "-group", "-pet",
					"-sleep", NULL};

				/* Get the sub-option */
			    if (Tcl_GetIndexFromObj(interp, objv[i], cmdOptions, "option",
					0, &index) != TCL_OK)
				{
					return TCL_ERROR;
			    }

				switch (index)
				{
					case 0: /* friend */
						if (Tcl_GetBooleanFromObj(interp, objv[i+1], &friend)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						i += 2;
						break;

					case 1: /* group */
						if (Tcl_GetBooleanFromObj(interp, objv[i+1], &group)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						i += 2;
						break;

					case 2: /* pet */
						if (Tcl_GetBooleanFromObj(interp, objv[i+1], &pet)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						i += 2;
						break;

					case 3: /* sleep */
						if (Tcl_GetBooleanFromObj(interp, objv[i+1], &sleep)
							!= TCL_OK)
						{
							return TCL_ERROR;
						}
						i += 2;
						break;
				}
			}

			y1 = p_ptr_py;
			x1 = p_ptr_px;

			/* Look for a location */
			for (i = 0; i < 20; ++i)
			{
				/* Pick a distance */
				int d = (i / 15) + 1;
		
				/* Pick a location */
				scatter(&y, &x, y1, x1, d, 0);
		
				/* Require "empty" floor grid */
				if (!cave_empty_bold(y, x)) continue;
		
				/* Hack -- no summon on glyph of warding */
				if (cave_feat(y, x) == FEAT_GLYPH) continue;
#if defined(ZANGBANDTK)
				if (cave_feat(y, x) == FEAT_MINOR_GLYPH) continue;
		
				/* ... nor on the Pattern */
				if ((cave_feat(y, x) >= FEAT_PATTERN_START)
					&& (cave_feat(y, x) <= FEAT_PATTERN_XTRA2))
						continue;
#endif /* ZANGBANDTK */
		
				/* Okay */
				break;
			}
			if (i == 20)
			{
				Tcl_SetResult(interp, "couldn't pick location to alloc",
					TCL_STATIC);
				return TCL_ERROR;
			}
#if defined(ANGBANDTK) || defined(OANGBANDTK)
			if (!place_monster_aux(y, x, r_idx, sleep, group)) return TCL_ERROR;
#endif /* ANGBANDTK, OANGBANDTK */
#if defined(KANGBANDTK)
			if (!place_monster_aux(y, x, r_idx, sleep, group, pet)) return TCL_ERROR;
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
			if (!place_monster_aux(y, x, r_idx, sleep, group, friend, pet)) return TCL_ERROR;
#endif /* ZANGBANDTK */
			break;
		}

		case 3: /* create_artifact */
		    if (objc < 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "a_idx ?y x?");
				return TCL_ERROR;
		    }
			if (Tcl_GetIntFromObj(interp, objv[2], &a_idx) != TCL_OK)
			{
				return TCL_ERROR;
			}
			o_ptr = &object_type_body;
			object_wipe(o_ptr);
#if defined(ZANGBANDTK)
			/* ZAngband also adds a random resist */
			create_named_art(a_idx, p_ptr_py, p_ptr_px);
#else
			make_fake_artifact(o_ptr, a_idx);
			drop_near(o_ptr, -1, p_ptr_py, p_ptr_px);
#endif
			break;
	}

	return TCL_OK;
}

cptr keyword_debug_widget[] = {"all", "config", "draw", "item", "iso", NULL};

/*
 *--------------------------------------------------------------
 *
 * objcmd_debughook --
 *
 *	Access to debugging stuff.
 *
 *--------------------------------------------------------------
 */
static int
objcmd_debughook(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	static CONST char *cmdOptions[] = {"bindings", "commands", "widgets", "vault", NULL};
	enum {IDX_BINDINGS, IDX_COMMANDS, IDX_WIDGETS, IDX_VAULT} option;

    if (objc < 2)
    {
		Tcl_WrongNumArgs(interp, 1, objv, "option ?arg ...?");
		return TCL_ERROR;
    }

	/* Get requested option */
    if (Tcl_GetIndexFromObj(interp, objv[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
    }

	switch (option)
	{
		case IDX_BINDINGS: /* bindings */
		    if (objc != 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "boolean");
				return TCL_ERROR;
		    }
			if (Tcl_GetBooleanFromObj(interp, objv[2],
				&debug_bindings) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		case IDX_COMMANDS: /* commands */
			/* Required number of arguments */
		    if (objc != 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "boolean");
				return TCL_ERROR;
		    }
			if (Tcl_GetBooleanFromObj(interp, objv[2],
				&debug_commands) != TCL_OK)
			{
				return TCL_ERROR;
			}
			break;

		/* debughook widgets ?flag? ?boolean? */
		case IDX_WIDGETS: /* widgets */
		{
			int flag;
			if (objc == 2)
			{
				Tcl_DString dString;
				Tcl_DStringInit(&dString);
				for (flag = 1; keyword_debug_widget[flag]; flag++)
				{
					if (debug_widgets & (1L << (flag - 1)))
					{
						Tcl_DStringAppendElement(&dString,
							keyword_debug_widget[flag]);
					}
				}
				Tcl_DStringResult(interp, &dString);
				break;
			}
			if (Tcl_GetIndexFromObj(interp, objv[2],
				keyword_debug_widget,
				"flag", 0, &flag) != TCL_OK)
			{
				return TCL_ERROR;
			}
			if ((objc == 3) && flag)
			{
				BooleanResult(interp,
					(debug_widgets & (1L << (flag - 1))) != 0);
				break;
			}
			if (objc == 4)
			{
				int on;
				if (Tcl_GetBooleanFromObj(interp, objv[3], &on) != TCL_OK)
				{
					return TCL_ERROR;
				}
				if (on)
				{
					if (!flag)
						debug_widgets = 0xFFFF;
					else
						debug_widgets |= 1L << (flag - 1);
				}
				else
				{
					if (!flag)
						debug_widgets = 0;
					else
						debug_widgets &= ~(1L << (flag - 1));
				}
			}
			break;
		}

		case IDX_VAULT: /* vault */
		{
			static CONST char *vaultOption[] = {"max", "info", NULL};
			int index, v_idx;
			vault_type *v_ptr;
			char *arrayName;
			Tcl_Obj *namePtr;
			
			/* Required number of arguments */
		    if (objc != 3)
		    {
				Tcl_WrongNumArgs(interp, 2, objv, "option");
				return TCL_ERROR;
		    }
		    if (Tcl_GetIndexFromObj(interp, objv[2], vaultOption, "option", 0, 
				&index) != TCL_OK)
			{
				return TCL_ERROR;
			}
			switch (index)
			{
				case 0: /* max */
					IntResult(interp, MAX_V_IDX);
					break;
					
				case 1: /* info */
					if (objc != 5)
					{
						Tcl_WrongNumArgs(interp, 3, objv, "vaultIndex arrayName");
						return TCL_ERROR;
					}
					if (Tcl_GetIntFromObj(interp, objv[3], &v_idx) != TCL_OK)
						return TCL_ERROR;
					if ((v_idx < 0) || (v_idx >= MAX_V_IDX))
					{
						FormatResult(interp,
							"bad v_info[] index \"%d\"", v_idx);
						return TCL_ERROR;
					}
					v_ptr = &v_info[v_idx];
					arrayName = Tcl_GetStringFromObj(objv[4], NULL);
					if (SetArrayValueString(arrayName, "name", v_name + v_ptr->name) != TCL_OK)
					{
						return TCL_ERROR;
					}
					namePtr = Tcl_NewStringObj("text", -1);
					if (Tcl_ObjSetVar2(interp, objv[4], namePtr,
						Tcl_NewStringObj(v_text + v_ptr->text,
						v_ptr->hgt * v_ptr->wid), TCL_LEAVE_ERR_MSG) == NULL)
					{
						Tcl_DecrRefCount(namePtr);
						return TCL_ERROR;
					}
					Tcl_DecrRefCount(namePtr);
					if (SetArrayValueLong(arrayName, "type", v_ptr->typ) != TCL_OK)
					{
						return TCL_ERROR;
					}
					if (SetArrayValueLong(arrayName, "rating", v_ptr->rat) != TCL_OK)
					{
						return TCL_ERROR;
					}
					if (SetArrayValueLong(arrayName, "height", v_ptr->hgt) != TCL_OK)
					{
						return TCL_ERROR;
					}
					if (SetArrayValueLong(arrayName, "width", v_ptr->wid) != TCL_OK)
					{
						return TCL_ERROR;
					}
					break;
			}
			break;
		}
	}

	return TCL_OK;
}

void init_debug(void)
{
	/* Create the "debug" command */
    Tcl_CreateObjCommand(g_interp, "debug", objcmd_debug, NULL, NULL);

	/* Hack -- Create the "debughook" command */
    Tcl_CreateObjCommand(g_interp, "debughook", objcmd_debughook, NULL, NULL);
}
