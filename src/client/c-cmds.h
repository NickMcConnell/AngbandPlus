/*
 * File: c-cmds.h
 * Purpose: Header for game command files
 */

#ifndef INCLUDED_C_CMDS_H
#define INCLUDED_C_CMDS_H

/* c-cmd.c */
extern void textui_cmd_poly(void);
extern void do_cmd_target(void);
extern void do_cmd_target_friendly(void);
extern void do_cmd_target_closest(void);
extern void do_cmd_fire_at_nearest(void);
extern void textui_cmd_drop_gold(void);
extern void do_cmd_view_map(void);
extern void do_cmd_wild_map(void);
extern void do_cmd_help(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message(void);
extern void send_msg_chunks(char *pmsgbuf, int msglen);
extern void do_cmd_chat(void);
extern void do_cmd_party(void);
extern void do_cmd_describe(void);
extern void do_cmd_purchase_house(void);
extern void do_cmd_quest(void);
extern void do_cmd_master(void);
extern void do_cmd_social(void);
extern void do_cmd_feeling(void);
extern void do_cmd_fountain(void);
extern void do_cmd_time(void);

/* c-cmd-obj.c */
extern bool obj_can_takeoff(struct player *p, const struct object *obj);
extern bool obj_has_inscrip(struct player *p, const struct object *obj);
extern bool obj_can_browse(struct player *p, const struct object *obj);
extern bool obj_browse_pre(void);
extern bool obj_can_study(struct player *p, const struct object *obj);
extern bool obj_study_pre(void);
extern bool obj_can_cast_from(struct player *p, const struct object *obj);
extern bool obj_cast_pre(void);
extern bool obj_is_staff(struct player *p, const struct object *obj);
extern bool obj_is_wand(struct player *p, const struct object *obj);
extern bool obj_is_rod(struct player *p, const struct object *obj);
extern bool obj_has_charges(const struct object *obj);
extern bool obj_can_zap(const struct object *obj);
extern bool inven_carry_okay(const struct object *obj);
extern bool obj_is_activatable(struct player *p, const struct object *obj);
extern bool obj_can_activate(const struct object *obj);
extern bool obj_is_food(struct player *p, const struct object *obj);
extern bool obj_is_potion(struct player *p, const struct object *obj);
extern bool obj_is_scroll(struct player *p, const struct object *obj);
extern bool item_tester_hook_fire(struct player *p, const struct object *obj);
extern bool obj_is_useable(struct player *p, const struct object *obj);
extern int need_dir(struct object *obj);
extern bool obj_can_refill(struct player *p, const struct object *obj);
extern bool obj_refill_pre(void);

#endif
