
/* File: externs.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, z-term.h, z-rand.h)
 */


/*
 * Automatically generated "variable" declarations
 */

#ifndef INCLUDED_EXTERNS_H
#define INCLUDED_EXTERNS_H

extern int  max_macrotrigger;
extern char *macro_template;
extern char *macro_modifier_chr;
extern char *macro_modifier_name[MAX_MACRO_MOD];
extern char *macro_trigger_name[MAX_MACRO_TRIGGER];
extern char *macro_trigger_keycode[2][MAX_MACRO_TRIGGER];


/* tables.c */
extern const byte moria_class_level_adj[MORIA_MAX_CLASS][MORIA_MAX_LEV_ADJ];
extern const byte moria_blows_table[MORIA_MAX_STR_ADJ][MORIA_MAX_DEX_ADJ];
extern const s16b ddd[9];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];
extern const char hexsym[16];
extern const int adj_mag_study[];
extern const int adj_mag_mana[];
extern const byte adj_mag_fail[];
extern const int adj_mag_stat[];
extern const byte adj_chr_gold[];
extern const s16b adj_chr_charm[];
extern const byte adj_int_dev[];
extern const byte adj_wis_sav[];
extern const byte adj_dex_dis[];
extern const byte adj_int_dis[];
extern const byte adj_dex_ta[];
extern const byte adj_str_td[];
extern const byte adj_dex_th[];
extern const byte adj_str_th[];
extern const byte adj_str_wgt[];
extern const byte adj_str_hold[];
extern const byte adj_str_dig[];
extern const byte adj_str_blow[];
extern const byte adj_dex_blow[];
extern const byte adj_dex_safe[];
extern const byte adj_con_fix[];
extern const int adj_con_mhp[];
extern const byte blows_table[12][12];
extern const byte extract_energy_nppmoria[6];
extern const byte extract_energy_nppangband[200];
extern const s32b player_exp_nppangband[PY_MAX_LEVEL];
extern const s32b player_exp_nppmoria[PY_MAX_LEVEL_MORIA];
extern const player_sex sex_info[MAX_SEXES];
extern const s16b spell_list_nppmoria_mage[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK];
extern const s16b spell_list_nppmoria_priest[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_mage[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_priest[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_druid[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern cptr feeling_themed_level[LEV_THEME_TAIL];
extern const byte chest_traps[64];
extern cptr color_names[16];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr stat_names_full[A_MAX];
extern const char *window_flag_desc[32];
extern option_entry options[OPT_MAX];
extern const byte option_page_nppangband[OPT_PAGE_MAX][OPT_PAGE_PER];
extern const byte option_page_nppmoria[OPT_PAGE_MAX][OPT_PAGE_PER];
extern cptr inscrip_text[MAX_INSCRIP];
extern byte spell_info_RF4[32][5];
extern byte spell_info_RF5[32][5];
extern byte spell_info_RF6[32][5];
extern byte spell_info_RF7[32][5];
extern byte spell_desire_RF4[32][8];
extern byte spell_desire_RF5[32][8];
extern byte spell_desire_RF6[32][8];
extern byte spell_desire_RF7[32][8];
extern const byte char_tables[256][CHAR_TABLE_SLOTS];
extern const xchar_type latin1_encode[];
extern cptr squelch_status[SQUELCH_OPT_MAX];
extern const byte squelch_status_color[SQUELCH_OPT_MAX];
extern const byte arena_level_map[ARENA_LEVEL_HGT][ARENA_LEVEL_WID];
extern const byte pit_room_maps[MAX_PIT_PATTERNS][PIT_HEIGHT][PIT_WIDTH];
extern const slays_structure slays_info_nppangband[11];
extern const brands_structure brands_info_nppangband[10];
extern const slays_structure slays_info_nppmoria[4];
extern const slays_structure brands_info_nppmoria[4];
extern const mon_susceptibility_struct mon_suscept[4];


/* variable.c */
extern cptr copyright;
extern byte version_major;
extern byte version_minor;
extern byte version_patch;
extern byte version_extra;
extern byte game_mode;
extern byte sf_major;
extern byte sf_minor;
extern byte sf_patch;
extern byte sf_extra;
extern u32b sf_xtra;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern bool arg_fiddle;
extern bool arg_wizard;
extern bool arg_rebalance;
extern bool arg_sound;
extern int arg_graphics;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern s16b character_icky;
extern s16b character_xtra;
extern u32b seed_randart;
extern u32b seed_flavor;
extern u32b seed_town;
extern u32b seed_ghost;
extern s16b num_repro;
extern s16b object_level;
extern s16b monster_level;
extern char summon_kin_type;
extern monster_type *summoner;
extern s32b turn;
extern int use_graphics;
extern s16b image_count;
extern bool use_bigtile;
extern s16b signal_count;
extern bool msg_flag;
extern bool do_playtesting;
extern bool inkey_base;
extern bool inkey_xtra;
extern u32b inkey_scan;
extern bool inkey_flag;
extern s16b coin_type;
extern byte object_generation_mode;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_mark;
extern bool repair_mflag_show;
extern s16b o_max;
extern s16b o_cnt;
extern s16b mon_max;
extern s16b mon_cnt;
extern s16b x_max;
extern s16b x_cnt;
extern byte feeling;
extern bool do_feeling;
extern s16b rating;
extern u32b level_flag;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char savefile[1024];
extern s16b macro__num;
extern char **macro__pat;
extern char **macro__act;
extern term *angband_term[ANGBAND_TERM_MAX];
extern char angband_term_name[ANGBAND_TERM_MAX][16];
extern byte angband_color_table[256][4];
extern const cptr angband_sound_name[MSG_MAX];
extern int view_n;
extern u16b *view_g;
extern int fire_n;
extern u16b *fire_g;
extern int temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern u16b (*cave_info)[256];
extern byte (*cave_feat)[MAX_DUNGEON_WID];
extern s16b (*cave_o_idx)[MAX_DUNGEON_WID];
extern s16b (*cave_m_idx)[MAX_DUNGEON_WID];
extern s16b (*cave_x_idx)[MAX_DUNGEON_WID];
extern u32b mon_power_ave[MAX_DEPTH_ALL][CREATURE_TYPE_MAX];

extern dynamic_grid_type *dyna_g;
extern u16b dyna_cnt;
extern u16b dyna_next;
extern bool dyna_full;
extern byte dyna_center_y;
extern byte dyna_center_x;


extern u16b cave_cost[MAX_FLOWS][MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
extern int cost_at_center[MAX_FLOWS];

extern int sidebar_details[SIDEBAR_MAX_TYPES];
extern int sidebar_monsters[SIDEBAR_MONSTER_MAX];

extern u16b quest_indicator_timer;
extern byte quest_indicator_complete;

extern u16b panel_change_offset_y;
extern u16b panel_change_offset_x;

extern dungeon_capabilities_type *dun_cap;

#ifdef MONSTER_SMELL

extern byte (*cave_when)[MAX_DUNGEON_WID];
extern int scent_when;

#endif /*MONSTER_SMELL*/

extern s16b add_wakeup_chance;
extern s16b total_wakeup_chance;
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *mon_list;
extern monster_lore *l_list;
extern effect_type *x_list;
extern store_type *store;
extern object_type *inventory;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern s16b alloc_feat_size;
extern alloc_entry *alloc_feat_table;
extern byte color_to_attr[2][MAX_COLOR_USED];
extern char color_to_char[2][MAX_COLOR_USED];
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern char *keymap_act[KEYMAP_MODES][256];
extern const player_sex *sp_ptr;
extern const player_race *rp_ptr;
extern player_class *cp_ptr;
extern player_magic *mp_ptr;
extern player_other *op_ptr;
extern player_type *p_ptr;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern feature_type *f_info;
extern feature_lore *f_l_list;
extern char *f_name;
extern char *f_text;
extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern ghost_template *t_info;
extern char *t_name;
extern char *t_text;
extern artifact_type *a_info;
extern artifact_lore *a_l_list;
extern char *a_text;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern monster_race *r_info;
extern char *r_text;
extern player_race *p_info;
extern char *p_name;
extern char *p_text;
extern player_class *c_info;
extern char *c_name;
extern char *c_text;
extern hist_type *h_info;
extern char *h_text;
extern owner_type *b_info;
extern char *b_name;
extern char *b_text;
extern byte *g_info;
extern char *g_name;
extern char *g_text;
extern flavor_type *flavor_info;
extern char *flavor_name;
extern char *flavor_text;
extern quest_type *q_info;
extern char *q_name;
extern names_type *n_info;
extern move_moment_type *mon_moment_info;
extern u16b move_moment_num;
extern s32b object_last_wield;

extern monster_race_message *mon_msg;
extern monster_message_history *mon_message_hist;
extern u16b size_mon_msg;
extern u16b size_mon_hist;

extern quiver_group_type quiver_group[MAX_QUIVER_GROUPS];


extern const char *ANGBAND_SYS;
extern const char *ANGBAND_GRAF;

extern char *ANGBAND_DIR;
extern char *ANGBAND_DIR_APEX;
extern char *ANGBAND_DIR_BONE;
extern char *ANGBAND_DIR_DATA;
extern char *ANGBAND_DIR_EDIT;
extern char *ANGBAND_DIR_FILE;
extern char *ANGBAND_DIR_HELP;
extern char *ANGBAND_DIR_INFO;
extern char *ANGBAND_DIR_SAVE;
extern char *ANGBAND_DIR_PREF;
extern char *ANGBAND_DIR_USER;
extern char *ANGBAND_DIR_XTRA;
extern char *ANGBAND_DIR_XTRA_FONT;
extern char *ANGBAND_DIR_XTRA_GRAF;
extern char *ANGBAND_DIR_XTRA_SOUND;
extern char *ANGBAND_DIR_XTRA_HELP;
extern char *ANGBAND_DIR_XTRA_ICON;


extern bool item_tester_full;
extern byte item_tester_tval;
extern bool item_tester_swap;
extern bool (*item_tester_hook)(const object_type*);
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern bool (*get_feat_num_hook)(int k_idx);
extern void (*object_info_out_flags)(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern ang_file *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern bool use_transparency;
extern void (*sound_hook)(int);
extern char notes_fname[1024];
extern ang_file *notes_file;
extern autoinscription* inscriptions;
extern u16b inscriptionsCount;
extern byte num_trap_on_level;
extern s16b player_ghost_num;
extern s16b ghost_r_idx;
extern char player_ghost_name[80];
extern char g_vault_name[80];
extern u16b altered_inventory_counter;
extern bool allow_altered_inventory;
extern u32b dungeon_summon_mask_f7;

/* Some useful constants */
extern cptr  standard_home_letters;
extern cptr  roguelike_home_letters;
extern cptr  standard_equip_letters;
extern cptr  roguelike_equip_letters;


/* attack.c */
extern bool test_hit(int chance, int ac, int vis);
extern int rogue_shot(const object_type *o_ptr, int *plus, player_state shot_state);
extern bool check_hit(int power);
extern int critical_hit_chance(const object_type *o_ptr, player_state a_state, bool id_only);
extern int critical_hit_check(const object_type *o_ptr, int *dd, int *plus);
extern int critical_shot_chance(const object_type *o_ptr, player_state a_state, bool throw, bool id_only, u32b f3);
extern void py_attack(int y, int x);
extern void do_cmd_fire(cmd_code code, cmd_arg args[]);
extern void textui_cmd_fire(void);
extern void textui_cmd_fire_at_nearest(void);
extern int weapon_throw_adjust(const object_type *o_ptr, u32b f3, int *plus, bool id_only);
extern void do_cmd_throw(cmd_code code, cmd_arg args[]);
extern void textui_cmd_throw(void);



/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(bool quickstart_allowed);

/* button.c */
extern int button_add_text(const char *label, unsigned char keypress);
extern void basic_buttons(void);
extern int button_add(const char *label, unsigned char keypress);
extern void button_backup_all(void);
extern void button_restore(void);
extern int button_kill_text(unsigned char keypress);
extern int button_kill(unsigned char keypress);
extern void button_kill_all(void);
extern void button_init(button_add_f add, button_kill_f kill);
extern void button_free(void);
extern char button_get_key(int x, int y);
extern size_t button_print(int row, int col);

/* calcs.c*/
extern int stat_adj_moria(int stat);
extern void calc_spells(void);
extern int calc_blows(const object_type *o_ptr, player_state *new_state);
extern void calc_bonuses(object_type inventory[], player_state *state, bool id_only);
extern byte calc_energy_gain(byte speed);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void handle_stuff(void);


/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool generic_los(int y1, int x1, int y2, int x2, u16b flg);
extern bool no_light(void);
extern bool cave_valid_bold(int y, int x);
extern byte multi_hued_attr(monster_race *r_ptr);
extern bool feat_supports_lighting(u16b feat);
extern bool dtrap_edge(int y, int x);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp, bool use_default);
extern void move_cursor_relative(int y, int x);
extern void print_rel(char c, byte a, int y, int x);
extern void note_spot(int y, int x);
extern void light_spot(int y, int x);
extern void prt_map(void);
extern void display_map(int *cy, int *cx);
extern void do_cmd_view_map(void);
extern errr vinfo_init(void);
extern void forget_view(void);
extern void update_view(void);
extern void update_flows(bool full);

#ifdef MONSTER_SMELL
extern void update_smell(void);
#endif /*MONSTER_SMELL*/

extern void wiz_light(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void update_los_proj_move(int y, int x);
extern void cave_alter_feat(int y, int x, int action);
extern void cave_set_feat(int y, int x, u16b feat);
extern int  project_path(u16b *path_g, u16b * path_gx, int range, int y1, int x1, int *y2, int *x2, u32b flg);
extern byte projectable(int y1, int x1, int y2, int x2, u32b flg);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void track_object(int item);
extern void track_object_kind(int k_idx);
extern void monster_race_track(int r_idx);
extern void feature_kind_track(int f_idx);
extern void disturb(int stop_search, int unused_flag);


/* cmd0.c */
extern void cmd_init(void);
extern int click_area(ui_event_data ke);
extern void do_cmd_quit(cmd_code code, cmd_arg args[]);
extern int find_sidebar_mon_idx(ui_event_data ke);
extern void textui_process_command(bool no_request);

/* cmd1.c */
extern void search(void);
extern bool put_object_in_inventory(object_type *o_ptr);
extern void do_cmd_pickup_from_pile(bool pickup, bool message);
extern void py_pickup_gold(void);
extern void py_pickup(bool pickup);
extern s16b move_player(int dir, int jumping);

/* cmd2.c */
extern bool do_cmd_test(int y, int x, int action, bool message);
extern void do_cmd_go_up(cmd_code code, cmd_arg args[]);
extern void do_cmd_go_down(cmd_code code, cmd_arg args[]);
extern void do_cmd_search(cmd_code code, cmd_arg args[]);
extern void do_cmd_toggle_search(cmd_code code, cmd_arg args[]);
extern int count_chests(int *y, int *x, bool trapped);
extern void do_cmd_open(cmd_code code, cmd_arg args[]);
extern void textui_cmd_open(void);
extern void do_cmd_close(cmd_code code, cmd_arg args[]);
extern void textui_cmd_close(void);
extern void do_cmd_tunnel(cmd_code code, cmd_arg args[]);
extern void textui_cmd_tunnel(void);
extern void do_cmd_disarm(cmd_code code, cmd_arg args[]);
extern void textui_cmd_disarm(void);
extern void do_cmd_bash(cmd_code code, cmd_arg args[]);
extern void textui_cmd_bash(void);
extern void do_cmd_make_trap(cmd_code code, cmd_arg args[]);
extern void textui_cmd_make_trap(void);
extern void do_cmd_steal(cmd_code code, cmd_arg args[]);
extern void textui_cmd_steal(void);
extern void do_cmd_alter_aux(int dir);
extern void do_cmd_alter(cmd_code code, cmd_arg args[]);
extern void textui_cmd_alter(void);
extern void do_cmd_spike(cmd_code code, cmd_arg args[]);
extern void textui_cmd_spike(void);
extern void do_cmd_walk(cmd_code code, cmd_arg args[]);
extern void textui_cmd_walk(void);
extern void do_cmd_jump(cmd_code code, cmd_arg args[]);
extern void textui_cmd_jump(void);
extern void do_cmd_run(cmd_code code, cmd_arg args[]);
extern void textui_cmd_run(void);
extern void do_cmd_pathfind(cmd_code code, cmd_arg args[]);
extern void do_cmd_hold(cmd_code code, cmd_arg args[]);
extern void do_cmd_pickup(cmd_code code, cmd_arg args[]);
extern void do_cmd_rest(cmd_code code, cmd_arg args[]);
extern void textui_cmd_rest(void);
extern void do_cmd_suicide(cmd_code code, cmd_arg args[]);
extern void textui_cmd_suicide(void);


/* cmd3.c */
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern void wield_in_quiver(object_type *o_ptr, int slot);
extern void wield_item(object_type *o_ptr, int item, int slot);
extern bool item_tester_hook_activate(const object_type *o_ptr);
extern void do_cmd_destroy(cmd_code code, cmd_arg args[]);
extern void destroy_item(int item);
extern void textui_cmd_destroy(void);
extern void do_cmd_refill(cmd_code code, cmd_arg args[]);
extern void do_cmd_target(void);
extern void do_cmd_target_closest(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern bool ang_sort_comp_hook(const void *u, const void *v, int a, int b);
extern void ang_sort_swap_hook(void *u, void *v, int a, int b);
extern void do_cmd_query_symbol(void);
extern void py_steal(int y, int x);
extern bool make_monster_trap(void);
extern void py_set_trap(int y, int x);
extern bool py_modify_trap(int y, int x);
extern void do_cmd_center_map(void);



/* cmd4.c */
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_options_aux(void *vpage, cptr info);
extern void do_cmd_pref(void);
extern void do_cmd_visuals(void);
extern void do_cmd_colors(void);
extern void do_cmd_dictate_note(void);
extern const char *option_name(int opt);
extern const char *option_desc(int opt);
extern void option_set(int opt, bool on);
extern void option_set_defaults(void);
extern void do_cmd_options(void);
extern void do_cmd_note(char *note, int what_depth);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void do_cmd_quest(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void create_notes_file(void);
extern void delete_notes_file(void);


/* cmd5.c */
extern s16b spell_chance(int spell);
extern bool spell_okay(int spell, bool known);
extern int get_spell_menu(const object_type *o_ptr, int mode_dummy);
extern bool player_can_cast(void);
extern bool player_can_study(void);
extern bool player_can_use_book(const object_type *o_ptr, bool cast);
extern void do_cmd_study_spell(cmd_code code, cmd_arg args[]);
extern void do_cmd_study_book(cmd_code code, cmd_arg args[]);
extern void do_cmd_cast(cmd_code code, cmd_arg args[]);
extern void spell_learn(int spell);
extern s16b get_spell_from_list(s16b book, s16b spell);
extern int get_spell_index(const object_type *o_ptr, int index);



/* cmd6.c */
/*deleted entire file*/

/* cmd-know.c */
extern void desc_art_fake(int a_idx);
extern void apply_magic_fake(object_type *o_ptr);
extern void do_cmd_knowledge_objects(void *obj, const char *name);
extern void do_cmd_knowledge_notes(void);
extern void init_cmd_know(void);
extern void do_cmd_knowledge(void);



/*cmd-obj.c*/
extern void obj_uninscribe(object_type *o_ptr, int item);
extern void do_cmd_uninscribe(cmd_code code, cmd_arg args[]);
extern void do_cmd_inscribe(cmd_code code, cmd_arg args[]);
extern void obj_inscribe(object_type *o_ptr, int item);
extern void obj_examine(object_type *o_ptr, int item);
extern void do_cmd_takeoff(cmd_code code, cmd_arg args[]);
extern void do_cmd_wield(cmd_code code, cmd_arg args[]);
extern void do_cmd_drop(cmd_code code, cmd_arg args[]);
extern void do_cmd_swap_weapon(cmd_code code, cmd_arg args[]);
extern void textui_cmd_swap_weapon(void);
extern void obj_browse(object_type *o_ptr, int item);
extern void obj_study(object_type *o_ptr, int item);
extern void obj_cast(object_type *o_ptr, int item);
extern void do_cmd_use(cmd_code code, cmd_arg args[]);
extern void cmd_use_item(void);
extern void textui_cmd_uninscribe(void);
extern void textui_cmd_inscribe(void);
extern void do_cmd_observe(void);
extern void textui_cmd_takeoff(void);
extern void textui_cmd_wield(void);
extern void textui_cmd_drop(void);
extern void do_cmd_browse(void);
extern void textui_cmd_study(void);
extern void textui_cmd_cast(void);
extern void textui_cmd_pray(void);
extern void textui_cmd_use_staff(void);
extern void textui_cmd_aim_wand(void);
extern void textui_cmd_zap_rod(void);
extern void textui_cmd_activate(void);
extern void textui_cmd_eat_food(void);
extern void textui_cmd_quaff_potion(void);
extern void textui_cmd_read_scroll(void);
extern void textui_cmd_refill(void);

/* death.c */
void death_screen(void);

/* dungeon.c */
extern void dungeon_change_level(int dlev);
extern void process_player_terrain_damage(void);
extern void process_player(void);
extern void idle_update(void);
extern void play_game(void);



/* effect.c */
extern int scan_effects_grid(int *effects, int size, int y, int x);
extern void effect_prep(int x_idx, byte type, u16b f_idx, byte y, byte x, byte countdown,
							byte repeats, u16b power, s16b source, u16b flags);
extern bool set_effect_lingering_cloud(int f_idx, byte y, byte x, u16b power, s16b source, u16b flag);
extern bool set_effect_glacier(int f_idx, byte y, byte x, s16b source, u16b flag);
extern bool set_effect_shimmering_cloud(int f_idx, byte y, byte x, byte repeats, u16b power, s16b source, u16b flag);
extern bool set_effect_permanent_cloud(int f_idx, byte y, byte x,	u16b power, u16b flag);
extern bool set_effect_trap_passive(int f_idx, byte y, byte x);
extern bool set_effect_trap_smart(int f_idx, byte y, byte x, u16b flags);
extern bool set_effect_trap_player(int f_idx, byte y, byte x);
extern bool set_effect_glyph(byte y, byte x);
extern bool set_effect_inscription(int f_idx, byte y, byte x, s16b source, u16b flag);
extern void compact_effects(void);
extern void pick_and_set_trap(int y, int x, int mode);
extern void place_trap(int y, int x, byte mode);
extern void effect_near(int feat, int y, int x, byte effect_type);
extern void effect_wipe(effect_type *x_ptr);
extern void wipe_x_list(void);
extern void delete_effect_idx(int x_idx);
extern void delete_effects(int y, int x);
extern void excise_effect_idx(int x_idx);
extern s16b x_pop(void);
extern void process_effects(void);




/* feature.c */
extern void feature_desc(char *buf, size_t max, u16b feat, bool add_prefix,	bool get_mimic);
extern int feat_adjust_combat_for_player(int chance, bool being_attacked);
extern int feat_adjust_combat_for_monster(const monster_type *m_ptr,
	int chance, bool being_attacked);
extern void find_secret(int y, int x);
extern u16b feat_state(u16b feat, int action);
extern u16b feat_state_power(u16b feat, int action);
extern u16b feat_state_explicit_power(u16b feat, int action);
extern u16b fire_trap_smart(int f_idx, int y, int x, byte mode);
extern void hit_trap(int f_idx, int y, int x, byte mode);
extern void feat_near(int feat, int y, int x);
extern void count_feat_everseen(void);
extern bool vault_locked_door(int f_idx);
extern errr get_feat_num_prep(void);
extern s16b get_feat_num(int level);
extern u16b pick_trap(int y, int x, byte mode);
extern u16b get_secret_door_num(void);
extern void place_secret_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void place_boring_closed_door(int y, int x);
extern void place_open_door(int y, int x);
extern void place_broken_door(int y, int x);
extern void place_locked_door(int y, int x);
extern void place_jammed_door(int y, int x);
extern void place_random_door(int y, int x);
extern void lore_do_probe_feature(int f_idx);
extern void describe_feature(int f_idx, bool spoilers);
extern void feature_roff_top(int f_idx);
extern void screen_feature_roff(int f_idx);
extern void display_feature_roff(int f_idx);
extern dynamic_grid_type *get_dynamic_terrain(byte y, byte x);
extern void wipe_dynamic_terrain(void);
extern bool add_dynamic_terrain(byte y, byte x);
extern void remove_dynamic_terrain(byte y, byte x);
extern void process_dynamic_terrain(void);
extern s16b select_powerful_race(void);
extern void format_monster_inscription(s16b r_idx, char inscr[], size_t max);
extern void decipher_strange_inscription(int x_idx);
extern void hit_silent_watcher(int y, int x);
extern bool hit_wall(int y, int x, bool do_action);
extern void update_level_flag(void);
extern u32b get_level_flag(u16b feat);
extern u32b get_level_flag_from_race(monster_race *r_ptr);
extern void describe_one_level_flag(char *buf, size_t max, u32b flag);
extern void debug_all_level_flags(u32b flags);




/* files.c */
extern void display_player_xtra_info(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *fn);
extern void display_player_stat_info(int row, int col);
extern void display_player(int mode, bool onscreen);
extern errr file_character(cptr name, bool full);
extern void string_lower(char *buf);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void do_cmd_save_game(cmd_code code, cmd_arg args[]);
extern void save_game(void);
extern void close_game(void);
extern void exit_game_panic(void);
extern void html_screenshot(cptr name, int mode);
extern void fill_template(char buf[], int max_buf);


/* generate.c */
extern void place_random_stairs(int y, int x);
extern void get_mon_hook(byte theme);
extern byte get_nest_theme(int nestlevel, bool quest_theme);
extern byte get_pit_theme(int pitlevel, bool quest_theme);
extern void build_terrain(int y, int x, int feat);
extern byte get_level_theme(s16b orig_theme_num, bool quest_level);
extern byte max_themed_monsters(const monster_race *r_ptr, u32b max_power);
extern void update_arena_level(byte stage);
extern void generate_cave(void);
extern void set_dungeon_type(u16b dungeon_type);



/* identify.c */
extern bool easy_know(const object_type *o_ptr);
extern bool object_is_known(const object_type *o_ptr);
extern bool object_is_known_artifact(const object_type *o_ptr);
extern bool object_was_sensed(const object_type *o_ptr);
extern bool object_flavor_is_aware(const object_type *o_ptr);
extern int value_check_aux1(const object_type *o_ptr);
extern bool can_be_pseudo_ided(const object_type *o_ptr);
extern void sense_inventory(void);



#ifdef ALLOW_DATA_DUMP
/*init1.c*/
extern void get_feature_name(char *desc, size_t max, byte feature_num);

#endif /*ALLOW_DATA_DUMP*/

/* init2.c */
extern void init_file_paths(const char *configpath, const char *libpath, const char *datapath);
extern void create_needed_dirs(void);
extern bool init_angband(void);
extern void cleanup_angband(void);

/* load.c */
extern bool load_player(void);
extern void load_gamemode(void);

/* melee1.c */
extern int drain_charges(object_type *o_ptr, u32b heal);
extern bool make_attack_normal(monster_type *m_ptr);
extern int get_dam(monster_race *r_ptr, int attack);
extern int get_breath_dam(s16b hit_points, int gf_type, bool powerful);
extern int get_ball_beam_dam(int m_idx, monster_race *r_ptr, int attack, int gf_type, bool powerful);
extern void mon_cloud(int m_idx, int typ, int dam, int rad);
extern bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px);
extern void cloud_surround(int r_idx, int *typ, int *dam, int *rad);


/* melee2.c */
extern void apply_monster_trap(int f_idx, int y, int x, byte mode);
extern bool race_breathes_element(const monster_race *r_ptr, int gf_type);
extern bool race_similar_breaths(const monster_race *r_ptr, const monster_race *r2_ptr);
extern bool race_similar_monsters(int m_idx, int m2y, int m2x);



#ifdef MONSTER_SMELL
extern int get_scent(int y, int x);
#endif /*MONSTER_SMELL*/

extern bool cave_exist_mon(const monster_race *r_ptr, int y, int x,
							bool occupied_ok, bool damage_ok, bool can_dig);
extern void process_entities(void);



/* monster1.c */
extern bool mon_inc_timed(int m_idx, int idx, int v, u16b flag);
extern bool mon_dec_timed(int m_idx, int idx, int v, u16b flag);
extern bool mon_clear_timed(int m_idx, int idx, u16b flag);
extern void wake_monster_attack(monster_type *m_ptr, u16b flag);
extern bool sleep_monster_spell(monster_type *m_ptr, int v, u16b flag);
extern void describe_monster(int r_idx, bool spoilers);
extern void roff_top(int r_idx);
extern void screen_roff(int r_idx);
extern void display_roff(int r_idx);
extern void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible);
extern void prepare_ghost_name(void);
extern bool prepare_ghost(int r_idx);
extern void ghost_challenge(void);
extern void remove_player_ghost(void);
extern void delete_player_ghost_entry(void);
extern void add_player_ghost_entry(void);
extern void load_player_ghost_file(void);
extern void save_player_ghost_file(void);

/* monster2.c */
extern s16b poly_r_idx(const monster_type *m_ptr);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_mon_list(void);
extern s16b mon_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level, int y, int x, byte mp_flags);
extern void update_mon_sidebar_list(void);
extern void display_monlist(void);
extern void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode);
extern void monster_desc_race(char *desc, size_t max, int r_idx);
extern void lore_probe_monster_aux(int r_idx);
extern void lore_do_probe_monster(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, object_type *j_ptr);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern bool player_place(int y, int x);
extern void monster_hide(monster_type *m_ptr);
extern void monster_unhide(monster_type *m_ptr);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern void calc_monster_speed(int y, int x);
extern void reveal_mimic(int o_idx, bool message);
extern bool place_monster_aux(int y, int x, int r_idx, byte mp_flags);
extern bool place_monster(int y, int x, byte mp_flags);
extern bool alloc_monster(int dis, byte mp_flags);
extern bool summon_specific(int y1, int x1, int lev, int type, byte mp_flags);
extern bool multiply_monster(int m_idx, bool override);
extern void message_pain(int m_idx, int dam);
extern bool add_monster_message(const char *mon_name, int m_idx, int msg_code);
extern void flush_monster_messages(void);
extern void update_smart_learn(int m_idx, int what);



/* object1.c */
extern void strip_name(char *buf, int k_idx);
extern size_t object_desc(char *buf, size_t max, const object_type *o_ptr, byte mode);
extern void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void identify_random_gen(const object_type *o_ptr);


/* object2.c */
extern s16b get_obj_num(int level);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern s16b charge_wand(object_type *o_ptr, int percent);
extern s16b charge_staff(object_type *o_ptr, int percent);
extern void object_into_artifact(object_type *o_ptr, artifact_type *a_ptr);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool interesting);
extern void object_quantities(object_type *j_ptr);
extern bool make_object(object_type *j_ptr, bool good, bool great, int objecttype, bool interesting);
extern bool prep_store_object(int storetype);
extern bool prep_object_theme(int themetype);
extern int get_object_mimic_k_idx(const monster_race *r_ptr);
extern bool make_gold(object_type *j_ptr);
extern void place_object(int y, int x, bool good, bool great, int droptype);
extern bool place_quest_artifact(int y, int x);
extern void place_gold(int y, int x);
extern void steal_object_from_monster(int y, int x);
extern bool is_throwing_weapon(const object_type *o_ptr);
extern void object_history(object_type *o_ptr, byte origin, s16b r_idx);
extern void stack_histories(object_type *o_ptr, const object_type *j_ptr);
extern void format_object_flags(const object_type *o_ptr, char dest[], int max,
	bool only_random_powers);
extern void expand_inscription(const object_type *o_ptr, const char *src, char dest[], int max);



/* obj-info.c */
extern bool screen_out_head(const object_type *o_ptr);
extern bool object_info_out(const object_type *o_ptr, bool extra_info);
extern void object_info_screen(const object_type *o_ptr);
extern bool format_object_history(char *buf, size_t max, const object_type *o_ptr);
extern bool history_interesting(const object_type *o_ptr);


/* obj-util.c */
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern bool object_has_hidden_powers(const object_type *o_ptr);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern bool wearable_p(const object_type *o_ptr);
extern s16b wield_slot_ammo(const object_type *o_ptr);
extern s16b wield_slot(const object_type *o_ptr);
extern bool ammo_inscribed_for_quiver(const object_type *o_ptr);
extern bool weapon_inscribed_for_quiver(const object_type *o_ptr);
extern bool slot_can_wield_item(int slot, const object_type *o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_okay(const object_type *o_ptr, int obj_num);
extern int scan_floor(int *items, int size, int y, int x, int mode);
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern int count_floor_items(int y, int x, bool pickup_only);
extern object_type* get_first_object(int y, int x);
extern object_type* get_next_object(const object_type *o_ptr);
extern errr get_obj_num_prep(void);
extern bool is_blessed(const object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_copy_amt(object_type *dst, object_type *src, int amt);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern bool drop_near(object_type *j_ptr, int chance, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void create_food(void);
extern void inven_item_charges(int item);
extern void inven_item_describe(int item);
extern void inven_item_increase(int item, int num);
extern int get_tag_num(int o_idx, int cmd, byte *tag_num);
extern int quiver_space_per_unit(const object_type *o_ptr);
extern void save_quiver_size(void);
extern int compare_ammo(int slot1, int slot2);
extern byte quiver_get_group(const object_type *o_ptr);
extern int sort_quiver(int slot);
extern void open_quiver_slot(int slot);
extern bool quiver_carry_okay(const object_type *o_ptr, int num, int item);
extern void inven_item_optimize(int item);
extern void floor_item_charges(int item);
extern void floor_item_describe(int item);
extern void floor_item_increase(int item, int num);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(const object_type *o_ptr);
extern bool quiver_stack_okay(const object_type *o_ptr);
extern bool inven_stack_okay(const object_type *o_ptr, int set_limit);
extern s16b quiver_carry(object_type *o_ptr);
extern s16b inven_carry(object_type *o_ptr);
extern s16b inven_takeoff(int item, int amt);
extern void inven_drop(int item, int amt);
extern void combine_pack(void);
extern void combine_quiver(void);
extern void reorder_pack(void);
extern int get_use_device_chance(const object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern unsigned check_for_inscrip(const object_type *o_ptr, const char *inscrip);
extern s16b lookup_kind(int tval, int sval);
extern void display_object_idx_recall(s16b o_idx);
extern void display_object_kind_recall(s16b k_idx);
extern void display_itemlist(void);
extern bool obj_can_refill(const object_type *o_ptr);
extern bool obj_is_spellbook(const object_type *o_ptr);
extern bool obj_is_shovel(const object_type *o_ptr);
extern bool obj_is_bow(const object_type *o_ptr);
extern bool obj_is_staff(const object_type *o_ptr);
extern bool obj_is_wand(const object_type *o_ptr);
extern bool obj_is_rod(const object_type *o_ptr);
extern bool obj_is_potion(const object_type *o_ptr);
extern bool obj_is_scroll(const object_type *o_ptr);
extern bool obj_is_parchment(const object_type *o_ptr);
extern bool obj_is_food(const object_type *o_ptr);
extern bool obj_is_light(const object_type *o_ptr);
extern bool obj_is_ring(const object_type *o_ptr);
extern bool obj_is_chest(const object_type *o_ptr);
extern bool obj_is_openable_chest(const object_type *o_ptr);
extern bool chest_requires_disarming(const object_type *o_ptr);
extern bool obj_is_weapon(const object_type *o_ptr);
extern bool obj_is_ammo(const object_type *o_ptr);
extern bool ammo_can_fire(const object_type *o_ptr, int item);
extern bool has_correct_ammo(void);
extern bool obj_has_charges(const object_type *o_ptr);
extern bool rod_can_zap(const object_type *o_ptr);
extern bool obj_can_browse(const object_type *o_ptr);
extern bool obj_can_study(const object_type *o_ptr);
extern bool obj_can_cast(const object_type *o_ptr);
extern bool obj_can_takeoff(const object_type *o_ptr);
extern bool obj_can_wear(const object_type *o_ptr);
extern bool obj_has_inscrip(const object_type *o_ptr);
extern object_type *object_from_item_idx(int item);
extern bool obj_needs_aim(object_type *o_ptr);
extern bool obj_is_activatable(const object_type *o_ptr);
extern bool obj_can_activate(const object_type *o_ptr);
extern bool get_item_okay(int item);
extern int scan_items(int *item_list, size_t item_list_max, int mode);
extern bool item_is_available(int item, bool (*tester)(const object_type *), int mode);
extern bool pack_is_full(void);
extern bool pack_is_overfull(void);
extern void pack_overflow(void);

/* obj-ui.c */
extern bool find_object_in_use(int *item);
extern void show_inven(byte);
extern void display_equip(void);
extern void show_equip(byte);
extern void show_floor(const int *floor_list, int floor_num, byte mode);
extern bool get_item(int *cp, cptr pmt, cptr str, int mode);
extern bool get_item_beside(int *cp, cptr pmt, cptr str, int sq_y, int sq_x);
extern bool item_menu(int *cp, cptr pmt, int mode, bool *oops, int sq_y, int sq_x);


/* pathfind.c */
extern bool findpath(int y, int x);
extern void run_step(int dir);

/* prefs.c */
extern void option_dump(ang_file *fff);
extern void macro_dump(ang_file *fff);
extern void keymap_dump(ang_file *fff);
extern void dump_monsters(ang_file *fff);
extern void dump_objects(ang_file *fff);
extern void dump_features(ang_file *fff);
extern void dump_flavors(ang_file *fff);
extern void dump_colors(ang_file *fff);


bool prefs_save(const char *path, void (*dump)(ang_file *), const char *title);
s16b tokenize(char *buf, s16b num, char **tokens);
errr process_pref_file_command(char *buf);
errr process_pref_file(cptr name);

/* quest.c */
extern void plural_aux(char *name, size_t max);
extern int quest_collection_num(quest_type *q_ptr);
extern void describe_quest(char *random_name, size_t max, s16b level, int mode);
extern void show_quest_mon(int y, int x);
extern void add_reward_gold(void);
extern void get_title(char *buf, size_t max);
extern void prt_rep_guild(int rep_y, int rep_x);
extern void prt_welcome_guild(void);
extern void grant_reward_hp(void);
extern bool quest_allowed(byte j);
extern bool can_quest_at_level(void);
extern void quest_finished(quest_type *q_ptr);
extern bool guild_purchase(int choice);
extern byte quest_check(int lev);
extern int quest_num(int lev);
extern int quest_item_slot(void);
extern int quest_item_count(void);
extern s32b quest_time_remaining(void);
extern s32b quest_player_turns_remaining(void);
extern void guild_quest_wipe(bool reset_defer);
extern void write_quest_note(bool success);
extern void quest_fail(void);
extern void format_quest_indicator(char dest[], int max, byte *attr);
extern void quest_status_update(void);
extern bool quest_fixed(const quest_type *q_ptr);
extern bool quest_slot_fixed(int quest_num);
extern bool quest_multiple_r_idx(const quest_type *q_ptr);
extern bool quest_slot_multiple_r_idx(int quest_num);
extern bool quest_single_r_idx(const quest_type *q_ptr);
extern bool quest_slot_single_r_idx(int quest_num);
extern bool quest_themed(const quest_type *q_ptr);
extern bool quest_type_collection(const quest_type *q_ptr);
extern bool quest_slot_collection(int quest_num);
extern bool quest_slot_themed(int quest_num);
extern bool quest_timed(const quest_type *q_ptr);
extern bool quest_slot_timed(int quest_num);
extern bool quest_no_down_stairs(const quest_type *q_ptr);
extern bool no_down_stairs(s16b check_depth);
extern bool quest_shall_fail_if_leave_level(void);
extern bool quest_might_fail_if_leave_level(void);
extern bool quest_fail_immediately(void);
extern bool quest_might_fail_now(void);


/* randart.c */
extern void make_random_name(char *random_name, byte min, byte max);
extern s32b artifact_power(int a_idx);
extern void build_randart_tables(void);
extern void free_randart_tables(void);
extern errr do_randart(u32b randart_seed, bool full);
extern bool make_one_randart(object_type *o_ptr, int art_power, bool namechoice);
extern void make_quest_artifact(int lev);
extern void create_quest_artifact(object_type *o_ptr);
extern void artifact_wipe(int a_idx, bool quest_art_wipe);
extern bool can_be_randart(const object_type *o_ptr);

/* save.c */
extern bool save_player(void);

/* score.c */
extern long total_points(void);
extern void enter_score(time_t *death_time);
extern void predict_score(void);
extern void show_scores(void);

/*signals.c*/
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);


/* spells1.c */
extern bool teleport_away(int m_idx, int dis);
extern bool teleport_player(int dis, bool native);
extern void teleport_player_to(int ny, int nx);
extern void teleport_towards(int oy, int ox, int ny, int nx);
extern bool teleport_player_level(int who);
extern byte gf_color(int type);
extern u16b bolt_pict(int y, int x, int ny, int nx, int typ, u32b flg);
extern void take_terrain_hit(int dam, int feat, cptr kb_str);
extern void take_hit(int dam, cptr kb_str);
extern bool object_hates_feature(int feat, const object_type *o_ptr);
extern bool object_hates_location(int y, int x, const object_type *o_ptr);
extern void acid_dam(int dam, cptr kb_str);
extern void elec_dam(int dam, cptr kb_str);
extern void fire_dam(int dam, cptr kb_str);
extern void cold_dam(int dam, cptr kb_str);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, bool permanent);
extern bool res_stat(int stat);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern bool project_m(int who, int y, int x, int dam, int typ, u32b flg);
extern bool project_p(int who, int y, int x, int dam, int typ, cptr msg);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
			 u32b flg, int degrees, byte source_diameter);


/* spells2.c */
extern bool hp_player(int num);
extern bool warding_glyph(void);
extern bool create_elements(int cy, int cx, int range);
extern bool create_glacier(void);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern void do_perm_stat_boost(int stat);
extern void identify_pack(void);
extern void uncurse_object(object_type *o_ptr);
extern bool remove_curse(bool heavy);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool set_recall(void);
extern bool detect(int dist, u16b detect_checks);
extern void stair_creation(void);
extern bool item_tester_hook_wieldable_ided_weapon(const object_type *o_ptr);
extern bool item_tester_hook_wieldable_weapon(const object_type *o_ptr);
extern bool item_tester_hook_ided_weapon(const object_type *o_ptr);
extern bool item_tester_hook_weapon(const object_type *o_ptr);
extern bool item_tester_hook_ided_armour(const object_type *o_ptr);
extern bool item_tester_hook_armour(const object_type *o_ptr);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool item_tester_hook_recharge(const object_type *o_ptr);
extern void recharge_staff_wand(object_type *o_ptr, int percent);
extern bool recharge(int num, bool cannot_fail, int percent);
extern bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg, int source_diameter);
extern bool explosion(int who, int rad, int y0, int x0, int dam, int typ, u32b flg);
extern bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ);
extern bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int degrees);
extern bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
	u32b flg);
extern bool project_los(int y0, int x0, int dam, int typ);
extern void clear_temp_array(void);
extern void cave_temp_mark(int y, int x, bool room);
extern void spread_cave_temp(int y1, int x1, int range, bool room, bool pass_walls);
extern bool speed_monsters(void);
extern bool slow_monsters(int power);
extern bool sleep_monsters(int power);
extern bool banish_evil(int dist);
extern bool turn_undead(int power);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_monsters(int dam);
extern bool mass_polymorph(void);
extern bool fire_player_los(int type, int dam);
extern void aggravate_monsters(int who);
extern void mass_aggravate_monsters(int who);
extern bool banishment(void);
extern bool mass_banishment(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r);
extern void earthquake(int cy, int cx, int r, bool kill_vault);
extern void light_room(int y1, int x1);
extern void unlight_room(int y1, int x1);
extern bool light_area(int dam, int rad);
extern bool unlight_area(int dam, int rad);
extern bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg);
extern bool fire_effect_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg,
	int source_diameter);
extern bool fire_arc(int typ, int dir, int dam, int rad, int degrees);
extern bool fire_star(int typ, int dam, int rad, u32b flg);
extern void fire_storm(int who, int typ0, int y0, int x0, int dam, int rad,
	int len, byte projection, bool lingering);
extern bool beam_burst(int y, int x, int typ, int num, int dam);
extern bool fire_swarm(int num, int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam, u32b flg);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool light_line(int dir, int dam);
extern bool strong_light_line(int dir);
extern bool drain_life(int dir, int dam);
extern bool build_wall(int dir, int dam);
extern bool wall_to_mud(int dir, int dam);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool heal_monster(int dir, int dam);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir);
extern bool sleep_monster(int dir);
extern bool confuse_monster(int dir, int plev);
extern bool poly_monster(int dir);
extern bool clone_monster(int dir);
extern bool fear_monster(int dir, int plev);
extern bool teleport_monster(int dir);
extern bool door_creation(void);
extern bool trap_creation(int who);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern bool brand_object(object_type *o_ptr, byte brand_type, bool do_enchant);
extern bool brand_weapon(bool enchant);
extern bool item_tester_hook_ided_ammo(const object_type *o_ptr);
extern bool item_tester_hook_ammo(const object_type *o_ptr);
extern bool brand_ammo(bool enchant);
extern bool brand_bolts(bool enchant);
extern void ring_of_power(int dir);
extern void identify_and_squelch_pack(void);
extern bool mass_identify(int rad);
extern void identify_object(object_type *o_ptr, bool star_ident);
extern int do_ident_item(int item, object_type *o_ptr);
extern void get_spell_type_from_feature(int f_idx, int *gf_type, cptr *action);
extern bool is_player_immune(int gf_type);
extern bool read_minds(void);
extern bool master_elements(int dam, int dir);
extern bool steal_powers(int dir);
extern bool beam_chain(int gf_type, int dam, int max_hits, int decrement);
extern bool call_huorns(void);


/* squelch.c */
extern byte squelch_level[SQUELCH_BYTES];
extern int get_autoinscription_index(s16b k_idx);
extern cptr get_autoinscription(s16b kindIdx);
extern int apply_autoinscription(object_type *o_ptr);
extern int remove_autoinscription(s16b kind);
extern int add_autoinscription(s16b kind, cptr inscription);
extern void autoinscribe_ground(void);
extern void autoinscribe_pack(void);
extern const char *squelch_to_label(int squelch);
extern bool squelch_tval(int tval);
extern byte get_squelch_status(int k_idx);
extern int squelch_itemp(const object_type *o_ptr, byte feeling, bool fullid);
extern int do_squelch_item(int squelch, int item, object_type *o_ptr);
extern void rearrange_stack(int y, int x);
extern bool squelch_item_ok(const object_type *o_ptr);
extern void do_squelch_pile(int y, int x);
extern void change_squelch_setting(s16b k_idx, int change);
extern void do_cmd_squelch_autoinsc(void *unused, cptr title);


/*Store.c*/
extern s32b price_item(const object_type *o_ptr, bool store_buying);
extern void store_item_increase(int st, int item, int num);
extern void store_item_optimize(int st, int item);
extern bool keep_in_stock(const object_type *o_ptr, int which);
extern void store_delete_index(int st, int what);
extern void store_shuffle(int which);
extern void do_cmd_buy(cmd_code code, cmd_arg args[]);
extern void do_cmd_reward(cmd_code code, cmd_arg args[]);
extern void do_cmd_retrieve(cmd_code code, cmd_arg args[]);
extern bool item_tester_hook_randart(const object_type *o_ptr);
extern bool item_tester_hook_flammable_book(const object_type *o_ptr);
extern void do_cmd_sell(cmd_code code, cmd_arg args[]);
extern void do_cmd_stash(cmd_code code, cmd_arg args[]);
extern void do_cmd_store(cmd_code code, cmd_arg args[]);
extern void store_maint(int which);
extern void store_init(int which);


/* target.c */
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool is_valid_target(int mode);
extern bool valid_target_exists(int mode);
extern bool target_set_closest(int mode);
extern bool target_set_interactive(int mode, int x, int y);


/* timed.c */

extern bool redundant_timed_event(int idx);
extern bool set_timed(int idx, int v, bool notify);
extern bool inc_timed(int idx, int v, bool notify);
extern bool dec_timed(int idx, int v, bool notify);
extern bool clear_timed(int idx, bool notify);
extern bool player_can_repeat(void);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(int v);



/* util.c */
extern void text_to_ascii(char *buf, size_t len, cptr str);
extern void ascii_to_text(char *buf, size_t len, cptr str);
extern char *find_roman_suffix_start(cptr buf);
extern int int_to_roman(int n, char *roman, size_t bufsize);
extern int roman_to_int(const char *roman);
extern int macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern errr macro_free(void);
extern errr macro_trigger_free(void);
extern void flush(void);
extern void flush_fail(void);
extern ui_event_data inkey_ex(void);
extern char anykey(void);
extern char inkey(void);
extern void bell(cptr reason);
extern void sound(int val);
extern void msg_print(cptr msg);
extern void playtesting(cptr msg);
extern void msg_c_format(u16b mtype, const char *fmt, ...);
extern void msg_format(cptr fmt, ...);
extern void message(u16b message_type, s16b extra, cptr message);
extern void message_format(u16b message_type, s16b extra, cptr fmt, ...);
extern void message_flush(void);
extern void clear_message_line(void);
extern void screen_save(void);
extern void screen_load(void);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void text_out_to_screen(byte a, cptr str);
extern void text_out_to_file(byte attr, cptr str);
extern void text_out(const char *fmt, ...);
extern void text_out_c(byte a, const char *fmt, ...);
extern void text_out_e(const char *fmt, ...);
extern void clear_from(int row);
extern bool askfor_aux_keypress(char *buf, size_t buflen, size_t *curs, size_t *len, char keypress, bool firsttime);
extern bool askfor_aux(char *buf, size_t len, bool keypress_h(char *, size_t, size_t *, size_t *, char, bool));
extern bool get_name(char *buf, size_t buflen);
extern bool get_string(cptr prompt, char *buf, size_t len);
extern s16b get_quantity(cptr prompt, int max);
extern int get_check_other(cptr prompt, cptr other_text, char other, cptr explain);
extern bool get_check(cptr prompt);
extern bool (*get_file)(const char *suggested_name, char *path, size_t len);
extern char get_char(cptr prompt, const char *options, size_t len, char fallback);
extern int get_menu_choice(s16b max, char *prompt);
extern bool get_com(cptr prompt, char *command);
extern bool get_com_ex(cptr prompt, ui_event_data *command);
extern void pause_line(int row);
extern void request_command(void);
extern bool is_a_vowel(int ch);
extern int color_char_to_attr(char c);
extern int color_text_to_attr(cptr name);
extern cptr attr_to_text(byte a);
extern int effective_depth(int depth);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

extern byte get_angle_to_grid[41][41];
extern int get_angle_to_target(int y0, int x0, int y1, int x1, int dir);
extern void get_grid_using_angle(int angle, int y0, int x0,	int *ty, int *tx);
extern cptr get_ext_color_name(byte ext_color);
extern void grid_queue_create(grid_queue_type *q, size_t max_size);
extern void grid_queue_destroy(grid_queue_type *q);
extern bool grid_queue_push(grid_queue_type *q, byte y, byte x);
extern void grid_queue_pop(grid_queue_type *q);
extern int pick_random_item(int chance_values[], int max);



/* wizard1.c */
#ifdef ALLOW_SPOILERS
extern void do_cmd_spoilers(void);
#endif /* ALLOW_SPOILERS */
extern bool make_fake_artifact(object_type *o_ptr, byte art_num);



#ifdef ALLOW_DEBUG
/* wizard2.c */
extern void do_cmd_debug(void);
#endif /* ALLOW_DEBUG */


/* x-char.c */
/**** Available Functions ****/
extern void xchar_trans_hook(char *s, int encoding);
extern void xstr_trans(char *str, int encoding);
extern void escape_latin1(char *dest, size_t max, cptr src);
extern const char seven_bit_translation[128];
extern char xchar_trans(byte c);

/* x-spell.c */
extern bool spell_needs_aim(int tval, int spell);
extern cptr do_mage_spell(int mode, int spell, int dir);
extern cptr do_druid_incantation(int mode, int spell, int dir);
extern cptr do_priest_prayer(int mode, int spell, int dir);
cptr cast_spell(int mode, int tval, int index, int dir);
extern int get_player_spell_realm(void);
extern cptr get_spell_name(int tval, int spell);

/* xtra1.c */
extern s16b modify_stat_value(int value, int amount);


/* xtra2.c */
extern bool allow_player_confusion(void);
extern s32b get_experience_by_level(int level);
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern int  get_coin_type(const monster_race *r_ptr);
extern void monster_death(int m_idx, int who);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, int who);
extern bool modify_panel(term *t, int wy, int wx);
extern bool change_panel(int dir);
extern void verify_panel(void);
extern void verify_panel_int(bool centered);
extern void center_panel(void);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern int mouse_dir(ui_event_data ke, bool locating);
extern int target_dir(char ch);
bool get_aim_dir(int *dp, bool target_trap);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);


/* xtra3.c */
extern void cnv_stat(int val, char *out_val, size_t out_len);
extern void prt_field(cptr info, int row, int col);
extern void prt_stat(int stat, int row, int col);
extern cptr get_player_title(void);
extern void prt_title(int row, int col);
extern void prt_level(int row, int col);
extern void prt_exp(int row, int col);
extern void prt_gold(int row, int col);
extern void prt_equippy(int row, int col);
extern size_t prt_resistances(int row, int col);
extern void prt_ac(int row, int col);
extern void prt_hp(int row, int col);
extern void prt_sp(int row, int col);
extern void moria_speed_labels(char *buf, int speed, size_t len);
extern byte player_hp_attr(void);
extern byte player_sp_attr(void);
extern size_t prt_cut(int row, int col);
extern size_t prt_stun(int row, int col);
extern size_t prt_hunger(int row, int col);
extern size_t prt_state(int row, int col);
extern void toggle_inven_equip(void);
extern void subwindows_set_flags(u32b *new_flags, size_t n_subwindows);
/* Ask the textui core for a game command. */
extern errr textui_get_cmd(cmd_context context, bool wait);
/* Set up game event handlers for the textui. */
extern void init_display(void);




#ifdef RISCOS
/* main-ros.c */
extern char *riscosify_name(cptr path);
#endif /* RISCOS */

#if defined(MAC_MPW) || defined(MACH_O_CARBON)
/* main-mac.c, or its derivatives */
extern u32b _fcreator;
extern u32b _ftype;
# if defined(MAC_MPW) && defined(CARBON)
extern void convert_pathname(char *path);
# endif
# if defined(MACH_O_CARBON)
extern void fsetfileinfo(cptr path, u32b fcreator, u32b ftype);
# endif
#endif






#ifdef ALLOW_DATA_DUMP
/*
 *dump_items.c
 */

extern void write_r_info_txt(void);
extern void write_o_info_txt(void);
extern void write_e_info_txt(void);
extern void write_a_info_txt(void);
extern void write_f_info_txt(void);
extern void dump_artifact_power(void);
extern void write_mon_power(void);

#endif /*ALLOW_DATA_DUMP*/


extern u16b lazymove_delay;

#endif /* !INCLUDED_EXTERNS_H */
