/*
 * File: cmds.h
 * Purpose: Header for game command files
 */

#ifndef INCLUDED_CMDS_H
#define INCLUDED_CMDS_H

/* channel.c */
extern void do_cmd_message(struct player *p, char *message);
extern void do_cmd_chat(struct player *p, char *buf);

/* cmd-cave.c */
extern void do_cmd_go_up(struct player *p);
extern void do_cmd_go_down(struct player *p);
extern bool search(struct player *p, struct chunk *c, bool verbose);
extern bool do_cmd_search(struct player *p);
extern void do_cmd_toggle_search(struct player *p);
extern void do_cmd_open(struct player *p, int dir, bool easy);
extern void do_cmd_close(struct player *p, int dir, bool easy);
extern bool do_cmd_tunnel(struct player *p);
extern void do_cmd_disarm(struct player *p, int dir, bool easy);
extern void do_cmd_alter(struct player *p, int dir);
extern void move_player(struct player *p, struct chunk *c, int dir, bool disarm,
    bool check_pickup, bool force);
extern void do_cmd_walk(struct player *p, int dir);
extern void do_cmd_jump(struct player *p, int dir);
extern void do_cmd_run(struct player *p, int dir);
extern bool do_cmd_rest(struct player *p, s16b resting);
extern void do_cmd_sleep(struct player *p);
extern void display_feeling(struct player *p, bool obj_only);
extern void do_cmd_purchase_house(struct player *p, int dir);
extern int wielding_cut(struct player *p);
extern bool create_house(struct player *p);
extern bool build_house(struct player *p);

/* cmd-innate.c */
extern void do_cmd_ghost(struct player *p, int ability, int dir);
extern void do_cmd_breath(struct player *p, int dir);
extern void do_cmd_mimic(struct player *p, int page, int spell_index, int dir);

/* cmd-misc.c */
extern void do_cmd_suicide(struct player *p);

/* cmd-obj.c */
extern void do_cmd_uninscribe(struct player *p, int item);
extern void do_cmd_inscribe(struct player *p, int item, const char *inscription);
extern void do_cmd_observe(struct player *p, int item);
extern void do_cmd_takeoff(struct player *p, int item);
extern void do_cmd_wield(struct player *p, int item, int slot);
extern void do_cmd_drop(struct player *p, int item, int quantity);
extern void do_cmd_destroy(struct player *p, int item, bool des);
extern void do_cmd_study(struct player *p, int book_index, int spell_index);
extern void do_cmd_cast(struct player *p, int book_index, int spell_index, int dir);
extern bool execute_effect(struct player *p, struct object **obj_address, struct effect *effect,
    int dir, bool *ident, bool *used, bool *notice);
extern void do_cmd_use_staff(struct player *p, int item);
extern void do_cmd_aim_wand(struct player *p, int item, int dir);
extern void do_cmd_zap_rod(struct player *p, int item, int dir);
extern void do_cmd_activate(struct player *p, int item, int dir);
extern void do_cmd_eat_food(struct player *p, int item);
extern void do_cmd_quaff_potion(struct player *p, int item, int dir);
extern void do_cmd_read_scroll(struct player *p, int item);
extern void do_cmd_use_any(struct player *p, int item, int dir);
extern void do_cmd_refill(struct player *p, int item);
extern bool do_cmd_read_scroll_end(struct player *p, struct object *obj, bool ident, bool used);
extern void do_cmd_use_staff_discharge(struct player *p, struct object *obj, bool ident, bool used);
extern void do_cmd_zap_rod_end(struct player *p, struct object *obj, bool ident, bool used);
extern void do_cmd_activate_end(struct player *p, struct object *obj, bool ident, bool used);

/* cmd-pickup.c */
extern byte player_pickup_item(struct player *p, struct chunk *c, int pickup, struct object *o);
extern byte do_autopickup(struct player *p, struct chunk *c, int pickup);
extern void do_cmd_pickup(struct player *p, int item);
extern void do_cmd_autopickup(struct player *p);
extern void leave_depth(struct player *p, struct chunk *c);
extern bool weight_okay(struct player *p, struct object *obj);
extern void do_cmd_hold(struct player *p, int item);

/* game-ui.c */
extern void do_cmd_master(struct player *p, s16b command, char* buf);
extern void do_cmd_social(struct player *p, const char *buf, int dir);

/* knowledge-ui.c */
extern void do_cmd_redraw(struct player *p);
extern void do_cmd_drop_gold(struct player *p, s32b amt);
extern void do_cmd_steal(struct player *p, int dir);
extern void do_cmd_locate(struct player *p, int dir);
extern void do_cmd_query_symbol(struct player *p, const char *buf);
extern void do_cmd_describe(struct player *p);
extern void do_cmd_fountain(struct player *p, int item);
extern void do_cmd_center_map(struct player *p);
extern void do_cmd_monlist(struct player *p);
extern void do_cmd_itemlist(struct player *p);
extern void do_cmd_knowledge(struct player *p, char type, int line);
extern void do_cmd_check_players(struct player *p, int line);
extern void do_cmd_check_other(struct player *p, int line);
extern void do_cmd_poly(struct player *p, struct monster_race *race, bool check_kills,
    bool domsg);
extern void do_cmd_check_poly(struct player *p, int line);
extern void do_cmd_check_socials(struct player *p, int line);
extern void do_cmd_interactive(struct player *p, int type, u32b query);

/* party.c */
extern void do_cmd_party(struct player *p, s16b command, char* buf);

#endif
