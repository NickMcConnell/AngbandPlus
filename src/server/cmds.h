/*
 * File: cmds.h
 * Purpose: "extern" declarations for command routines
 */

#ifndef __angband_cmds_h__
#define __angband_cmds_h__

/* Types of item use */
typedef enum
{
    USE_TIMEOUT,
    USE_CHARGE,
    USE_SINGLE
} use_type;

/* Command parameters */
typedef struct
{
    u32b dm_flag;
    bool player_undead;
    const char *msg_ghost;
    char p_note;
    int inv_start;
    char g_note;
    bool check_antimagic;
    use_type use;
    int snd;
    bool (*item_tester_hook)(int, object_type*);
} cmd_param;

/* attack.c */
extern void do_cmd_fire(int Ind, int dir, int item);
extern void do_cmd_fire_at_nearest(int Ind);
extern void do_cmd_throw(int Ind, int dir, int item);

/* cave.c */
extern void do_cmd_view_map(int Ind);
extern void do_cmd_wild_map(int Ind);

/* cmd2.c */
extern void do_cmd_go_up(int Ind);
extern void do_cmd_go_down(int Ind);
extern void do_cmd_search(int Ind);
extern void do_cmd_toggle_search(int Ind);
extern void do_cmd_open(int Ind, int dir, bool easy);
extern void do_cmd_close(int Ind, int dir, bool easy);
extern void do_cmd_tunnel(int Ind, int dir);
extern void do_cmd_disarm(int Ind, int dir, bool easy);
extern void do_cmd_bash(int Ind, int dir);
extern void do_cmd_alter(int Ind, int dir);
extern void do_cmd_spike(int Ind, int dir);
extern void do_cmd_walk(int Ind, int dir);
extern void do_cmd_jump(int Ind, int dir);
extern void do_cmd_run(int Ind, int dir);
extern void do_cmd_pickup(int Ind);
extern void do_cmd_autopickup(int Ind);
extern bool do_cmd_rest(int Ind, s16b resting);
extern void do_cmd_suicide(int Ind);
extern void do_cmd_purchase_house(int Ind, int dir);
extern void do_cmd_quest(int Ind);

/* cmd3.c */
extern void do_cmd_drop_gold(int Ind, s32b amt);
extern void do_cmd_steal(int Ind, int dir);
extern void do_cmd_locate(struct player *p, int dir);
extern void do_cmd_query_symbol(int Ind, const char *buf);
extern void do_cmd_describe(int Ind);
extern void do_cmd_fountain(int Ind, int item);
extern void do_cmd_center_map(int Ind);

/* cmd4.c */
extern void do_cmd_knowledge(int Ind, char type, int line);
extern void display_feeling(struct player *p, bool obj_only);
extern void do_cmd_redraw(int Ind);

/* cmd-innate.c */
extern void do_cmd_ghost(int Ind, int ability, int dir);
extern void do_cmd_breath(int Ind, int dir);
extern void do_cmd_mimic(int Ind, int page, int spell, int dir);

/* cmd-misc.c */
extern void do_cmd_monlist(int Ind);
extern void do_cmd_itemlist(int Ind);

/* cmd-obj.c */
extern void do_cmd_uninscribe(int Ind, int item);
extern void do_cmd_inscribe(int Ind, int item, const char *inscription);
extern void do_cmd_observe(int Ind, int item);
extern void do_cmd_takeoff(int Ind, int item);
extern void do_cmd_wield(int Ind, int item, int slot);
extern void do_cmd_drop(int Ind, int item, int quantity);
extern void do_cmd_destroy(int Ind, int item, bool des);
extern void do_cmd_browse(struct player *p, int book);
extern void do_cmd_study(int Ind, int book, int spell);
extern void do_cmd_cast(int Ind, int book, int spell, int dir);
extern void do_cmd_use_staff(int Ind, int item);
extern void do_cmd_aim_wand(int Ind, int item, int dir);
extern void do_cmd_zap_rod(int Ind, int item, int dir);
extern void do_cmd_activate(int Ind, int item, int dir);
extern void do_cmd_eat_food(int Ind, int item);
extern void do_cmd_quaff_potion(int Ind, int item);
extern void do_cmd_read_scroll(int Ind, int item);
extern void do_cmd_use_any(int Ind, int item, int dir);
extern void do_cmd_refill(int Ind, int item);
extern void do_cmd_read_scroll_end(int Ind, bool ident, bool used);
extern void do_cmd_use_staff_discharge(int Ind, bool ident, bool used);
extern void do_cmd_zap_rod_end(int Ind, bool ident, bool used);
extern bool use_object(int Ind, object_type *o_ptr, bool *ident, int dir);

/* party.c */
extern void do_cmd_party(int Ind, s16b command, char* buf);

/* spells1.c */
extern void do_cmd_poly(struct player *p, int number, bool check_kills, bool domsg);

/* s-util.c */
extern void do_cmd_message(int Ind, char *message);
extern void do_cmd_chat(int Ind, char *buf);

/* xtra2.c */
extern void do_cmd_master(int Ind, s16b command, char* buf);
extern void do_cmd_social(int Ind, const char *buf, int dir);

#endif
