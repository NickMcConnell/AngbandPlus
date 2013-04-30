/*
 * File: c-cmds.h
 * Purpose: "extern" declarations for command routines
 */

#ifndef INCLUDED_CMDS_H
#define INCLUDED_CMDS_H

#include "game-cmd.h"

/* c-cmd0.c */
extern void cmd_init(void);
extern cmd_code cmd_lookup(unsigned char key, int mode);

/* c-cmd.c */
extern void textui_cmd_poly(void);
extern void textui_cmd_rest(void);
extern void do_cmd_look(void);
extern void do_cmd_target(void);
extern void do_cmd_target_friendly(void);
extern void do_cmd_target_closest(void);
extern void textui_cmd_fire_at_nearest(void);
extern void textui_cmd_throw(void);
extern void do_cmd_equip(void);
extern void do_cmd_inven(void);
extern void textui_cmd_drop_gold(void);
extern void textui_cmd_destroy_menu(int item);
extern void textui_cmd_destroy(void);
extern void textui_cmd_toggle_ignore(void);
extern void do_cmd_view_map(void);
extern void do_cmd_wild_map(void);
extern void do_cmd_itemlist(void);
extern void do_cmd_monlist(void);
extern void do_cmd_locate(void);
extern void do_cmd_help(void);
extern void do_cmd_query_symbol(void);
extern void do_cmd_change_name(void);
extern void do_cmd_messages(void);
extern void do_cmd_message(void);
extern void do_cmd_chat(void);
extern void do_cmd_party(void);
extern void do_cmd_describe(void);
extern void textui_cmd_suicide(void);
extern void do_cmd_redraw(void);
extern void do_cmd_purchase_house(void);
extern void do_cmd_quest(void);
extern void do_cmd_pref(void);
extern void do_cmd_master(void);
extern void do_cmd_social(void);
extern void do_cmd_feeling(void);
extern void do_cmd_fountain(void);
extern void do_cmd_center_map(void);

/* c-files.c */
extern void do_cmd_save_screen(void);

/* c-obj-ui.c */
extern bool get_item(int *cp, const char *pmt, const char *str, cmd_code cmd, int mode);

/* ui-knowledge.c */
extern void textui_browse_knowledge(void);
extern void do_cmd_players(void);

/* ui-spell.c */
extern void textui_spell_browse(int book);
extern int textui_obj_study(int book);
extern void textui_obj_cast(int book);
extern void textui_obj_project(int book);

#endif
