/* File: interp.h */

/* Purpose: Tcl-related header */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#ifndef _INCLUDE_INTERP_H_
#define _INCLUDE_INTERP_H_

extern Tcl_Interp *g_interp;

/* const.c */
extern int init_const(Tcl_Interp *interp);

/* describe.c */
extern Tcl_Obj *dump_object_flags(Tcl_Interp *interp, object_type *o_ptr);
extern Tcl_Obj *dump_player_flags(Tcl_Interp *interp);
extern int objcmd_equipinfo _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_inveninfo _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

/* interp1.c */
typedef struct {
	char *key; /* Textual name of TVAL_XXX macro */
	int value; /* TVAL_XXX constant */
} t_tval;
extern t_tval g_tval[];
extern Tcl_HashTable *g_tval_str;
extern Tcl_HashTable *g_tval_const;

extern void angtk_display_info_aux(char *title, Tcl_Obj *listObjPtr);

extern int objcmd_angband _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_player _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
#if defined(ZANGBANDTK)
extern int objcmd_power _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
#endif /* ZANGBANDTK */
extern int objcmd_setting _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_spell _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_store _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_system _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

/* interp2.c */
#if defined(KANGBANDTK) || defined(ZANGBANDTK)
extern int objcmd_building _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
#endif /* KANGBANDTK, ZANGBANDTK */
extern int objcmd_cave _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_equipment _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_floor _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_game _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_highscore _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_info _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_init_icons _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_inkey_flags _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_inkey_options _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_inkey_other _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_inventory _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_keycount _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_keymap _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_keypress _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_macro _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_message _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
#if defined(ZANGBANDTK)
extern int objcmd_mindcraft _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
#endif /* ZANGBANDTK */

extern int objcmd_ARRAY_find _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_ARRAY_info _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_ARRAY_max _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));
extern int objcmd_ARRAY_set _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

/* map.c */
extern int map_symbol_find(Tcl_Interp *interp, Tcl_Obj *objName,
	int *symbolIndex);

/* sound.c */
extern int objcmd_sound _ANSI_ARGS_((ClientData clientData,
		    Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[]));

#endif /* _INCLUDE_INTERP_H_ */
