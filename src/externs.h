
/* File: externs.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */


/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, z-term.h, z-rand.h)
 */


/*
 * Automatically generated "variable" declarations
 */

extern int  max_macrotrigger;
extern cptr macro_template;
extern cptr macro_modifier_chr;
extern cptr macro_modifier_name[MAX_MACRO_MOD];
extern cptr macro_trigger_name[MAX_MACRO_TRIGGER];
extern cptr macro_trigger_keycode[2][MAX_MACRO_TRIGGER];


/* tables.c */
extern const s16b ddd[9];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];
extern const char hexsym[16];
extern const int adj_mag_study[];
extern const byte adj_mag_fail[];
extern const int adj_mag_stat[];
extern const byte adj_int_dev[];
extern const int adj_wis_sav[];
extern const byte adj_dex_dis[];
extern const byte adj_int_dis[];
extern const byte adj_int_read[];
extern const byte adj_agi_ta[];
extern const byte adj_ste_rdark[];
extern const byte adj_con_slowdig[];
extern const byte adj_str_td_mel[];
extern const byte adj_dex_th_mel[];
extern const byte adj_per_td_mis[];
extern const byte adj_per_th_mis[];
extern const byte adj_str_wgt[];
extern const byte adj_dex_blows[];
extern const byte adj_dex_misfire_chance[];
extern const int adj_ste_noisy_chance[];
extern const int adj_str_hold[];
extern const byte adj_str_dig[];
extern const byte adj_ste_stealth[];
extern const byte adj_luc_survive[];
extern const byte adj_int_fail_rate[]; 
extern const byte adj_dex_shots[];
extern const byte adj_str_blow[];
extern const byte adj_dex_blow[];
extern const byte adj_agi_safe[];
extern const byte adj_per_telepathy[];
extern const byte adj_con_fix[];
extern const byte adj_luc_curse_chance[];
extern const int adj_luc_good_chance[];
extern const int adj_con_mhp[];
extern const int adj_wis_msp[];
extern const int adj_agi_speed[];
extern const int adj_int_escape[];
extern const int adj_agi_escape[];
extern const int adj_per_search[];
extern const byte adj_int_success[];
extern const byte adj_per_evade[];
extern const byte adj_per_pseudo_delay[];
extern const int adj_per_lite[];
extern const byte adj_per_infra[];
extern const byte extract_energy[200];
extern const s32b player_exp[PY_MAX_LEVEL];
extern const player_sex sex_info[MAX_SEXES];
extern const s16b spell_list[3][BOOKS_PER_REALM][SPELLS_PER_BOOK];
extern cptr feeling_themed_level[LEV_THEME_TAIL];
extern const byte chest_traps[64];
extern cptr color_names[16];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr stat_names_full[A_MAX];
extern cptr window_flag_desc[32];
extern option_type options[OPT_MAX];
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];
extern cptr inscrip_text[MAX_INSCRIP];
extern byte spell_info_RF4[32][5];
extern byte spell_info_RF5[32][5];
extern byte spell_info_RF6[32][5];
extern byte spell_info_RF7[32][5];
extern byte spell_desire_RF4[32][8];
extern byte spell_desire_RF5[32][8];
extern byte spell_desire_RF6[32][8];
extern byte spell_desire_RF7[32][8];


/* variable.c */
extern cptr copyright;
extern byte version_major;
extern byte version_minor;
extern byte version_patch;
extern byte version_extra;
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
extern bool arg_sound;
extern bool arg_graphics;
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
extern s16b num_repro;
extern s16b object_level;
extern s16b monster_level;
extern char summon_kin_type;
extern monster_type *summoner;
extern s32b turn;
extern bool use_sound;
extern int use_graphics;
extern s16b image_count;
extern bool use_bigtile;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
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
extern u16b level_flag;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char savefile[1024];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
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
extern u32b mon_power_ave[MAX_DEPTH][CREATURE_TYPE_MAX];

extern dynamic_grid_type *dyna_g;
extern u16b dyna_cnt;
extern u16b dyna_next;
extern bool dyna_full;
extern byte dyna_center_y;
extern byte dyna_center_x;
extern element_counter_type element_counter;


extern u16b cave_cost[MAX_FLOWS][MAX_DUNGEON_HGT][MAX_DUNGEON_WID];
extern int cost_at_center[MAX_FLOWS];

extern u16b quest_indicator_timer;

extern u16b panel_change_offset_y;
extern u16b panel_change_offset_x;

extern dungeon_capabilities_type *dun_cap;

#ifdef MONSTER_SMELL

extern byte (*cave_when)[MAX_DUNGEON_WID];
extern int scent_when;

#endif /*MONSTER_SMELL*/

extern s16b add_wakeup_chance;
extern s16b total_wakeup_chance;
extern u16b path_g[120];
extern byte path_gx[120];
extern int path_n;
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
extern byte misc_to_attr[256];
extern char misc_to_char[256];
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
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
extern artifact_type *a_info;
extern char *a_text;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern monster_race *r_info;
extern char *r_name;
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
extern new_spell_info_type *newspells;

extern monster_race_message *mon_msg;
extern u16b size_mon_msg;

extern quiver_group_type quiver_group[MAX_QUIVER_GROUPS];

extern cptr ANGBAND_SYS;
extern cptr ANGBAND_GRAF;
extern cptr ANGBAND_DIR;
extern cptr ANGBAND_DIR_APEX;
extern cptr ANGBAND_DIR_BONE;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_EDIT;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
extern cptr ANGBAND_DIR_INFO;
extern cptr ANGBAND_DIR_SAVE;
extern cptr ANGBAND_DIR_PREF;
extern cptr ANGBAND_DIR_USER;
extern cptr ANGBAND_DIR_XTRA;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook)(const object_type*);
extern bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);
extern void (*ang_sort_swap)(void *u, void *v, int a, int b);
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern bool (*get_feat_num_hook)(int k_idx);
extern void (*object_info_out_flags)(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern bool use_transparency;
extern char notes_fname[1024];
extern FILE *notes_file;
extern byte recent_failed_thefts;
extern autoinscription* inscriptions;
extern u16b inscriptionsCount;
extern byte num_trap_on_level;
extern byte bones_selector;
extern int r_ghost;
extern char ghost_name[80];
extern char g_vault_name[80];



/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool generic_los(int y1, int x1, int y2, int x2, u16b flg);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern bool feat_supports_lighting(u16b feat);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
extern void map_info_default(int y, int x, byte *ap, char *cp);
extern void move_cursor_relative(int y, int x);
extern void print_rel(char c, byte a, int y, int x);
extern void note_spot(int y, int x);
extern void lite_spot(int y, int x);
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


extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void update_los_proj_move(int y, int x);
extern void cave_set_feat(int y, int x, u16b feat);
extern void cave_alter_feat(int y, int x, int action);
extern int  project_path(int range, int y1, int x1, int *y2, int *x2, u32b flg);
extern byte projectable(int y1, int x1, int y2, int x2, u32b flg);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void feature_kind_track(int f_idx);
extern void disturb(int stop_search, int unused_flag);

extern byte challenged(void);

/* cmd1.c */
extern int dam_plus_to_percent_added(int to_d);
extern int test_hit(int chance, int ac, int vis);
extern int critical_shot(int weight, int plus, int dam);
extern int critical_norm(int weight, int plus, int dam);
extern cptr tot_dam_aux_verb(const object_type *o_ptr, const monster_type *m_ptr);
extern int melee_p_increment(const object_type *o_ptr, const monster_type *m_ptr);
extern int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr, bool is_weapon);
extern void find_secret(int y, int x);
extern void search(void);
extern void do_cmd_pickup_from_pile(void);
extern void py_pickup(int pickup);
extern int check_hit(int power);
extern void py_attack(int y, int x);
extern s16b move_player(int dir, int jumping);
extern void run_step(int dir);
extern int to_hit_weight_penalty(const object_type *o_ptr);

/* cmd2.c */
extern void potential_effect_on_stats();
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_search(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_make_trap(void);
extern void do_cmd_steal(void);
extern void do_cmd_alter(void);
extern void do_cmd_spike(void);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void do_cmd_hold(void);
extern void do_cmd_stay(void);
extern void do_cmd_rest(void);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);

/* cmd3.c */
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern void do_cmd_wield(void);
extern void do_cmd_takeoff(void);
extern void do_cmd_drop(void);
extern void do_cmd_destroy(void);
extern void destroy_item(int item);
extern void do_cmd_observe(void);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern bool ang_sort_comp_hook(const void *u, const void *v, int a, int b);
extern void ang_sort_swap_hook(void *u, void *v, int a, int b);
extern void py_steal(int y, int x);
extern bool make_monster_trap(void);
extern void py_set_trap(int y, int x);
extern bool py_modify_trap(int y, int x);

/* cmd4.c */
extern void do_cmd_redraw(void);
extern void options_birth_menu(bool adult);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_options(void);
extern void do_cmd_pref(void);
extern void do_cmd_macros(void);
extern void do_cmd_visuals(void);
extern void do_cmd_colors(void);
extern void do_cmd_note(char *note, int what_depth);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void ghost_challenge(void);
extern void do_cmd_quest(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void desc_art_fake(int a_idx);
extern void apply_magic_fake(object_type *o_ptr);
extern void do_cmd_knowledge(void);
extern FILE *create_notes_file(char name[], size_t max);
extern void delete_notes_file(void);

/* cmd5.c */
extern s16b spell_chance(int spell);
extern bool spell_okay(int spell, bool known);
extern int get_player_spell_realm(void);
extern void print_spells(const byte *spells, int num, int y, int x);
extern void display_koff(int k_idx);
extern cptr get_spell_name(int tval, int spell);
extern cptr do_druid_spell(int mode, int spell);
extern cptr do_mage_spell(int mode, int spell);
extern cptr do_priest_prayer(int mode, int spell);
extern void do_cmd_browse_aux(const object_type *o_ptr);
extern void do_cmd_browse(void);
extern void do_cmd_study(void);
extern void do_cmd_cast(bool pray);
extern void do_cmd_pray(void);
extern cptr nice_mana_cost(int raw_cost);
extern int get_mana_cost(int spell_code, int talismans_dont_help);
extern cptr nice_mana_cost(int raw_cost);
extern int get_success_prob(int spell_code, int wand_bonus);

/* cmd6.c */
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_zap_rod(void);
extern bool item_tester_hook_activate(const object_type *o_ptr);
extern void do_cmd_activate(void);

/* dungeon.c */
extern void regenmana(int percent);
extern bool can_be_pseudo_ided(const object_type *o_ptr);
extern int value_check_aux1(const object_type *o_ptr);
extern void process_player_terrain_damage(void);
extern void process_player(void);
extern void play_game(bool new_game);
extern int danger(int);

/* effects.c */
extern int scan_effects_grid(int *effects, int size, int y, int x);
extern void effect_prep(int x_idx, byte type, u16b f_idx, byte y, byte x, byte countdown,
							byte repeats, u16b power, s16b source, u16b flags);
extern bool set_effect_lingering_cloud(int f_idx, byte y, byte x, u16b power, s16b source, u16b flag);
extern bool set_effect_shimmering_cloud(int f_idx, byte y, byte x, byte repeats, u16b power, s16b source, u16b flag);
extern bool set_effect_permanent_cloud(int f_idx, byte y, byte x,	u16b power, u16b flag);
extern bool set_effect_trap_smart(int f_idx, byte y, byte x, u16b flags);
extern bool set_effect_trap_passive(int f_idx, byte y, byte x);
extern bool set_effect_trap_player(int f_idx, byte y, byte x);
extern bool set_effect_glyph(byte y, byte x);
extern bool set_effect_glacier(int f_idx, byte y, byte x, s16b source, u16b flag);
extern bool set_effect_inscription(int f_idx, byte y, byte x, s16b source, u16b flag);
extern void compact_effects(void);
extern void effect_near(int feat, int y, int x, byte effect_type);
extern void place_trap(int y, int x, byte mode);
extern void pick_and_set_trap(int y, int x, int mode);
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
extern u16b feat_state(u16b feat, int action);
extern u16b feat_state_power(u16b feat, int action);
extern u16b feat_state_explicit_power(u16b feat, int action);
extern u16b fire_trap_smart(int f_idx, int y, int x, byte mode);
extern void hit_trap(int f_idx, int y, int x, byte mode, int might_escape);
extern void feat_near(int feat, int y, int x);
extern void count_feat_everseen(void);
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
extern u16b *get_element_counter(feature_type *f_ptr);
extern s16b select_powerful_race(void);
extern void format_monster_inscription(s16b r_idx, char inscr[], size_t max);
extern void decipher_strange_inscription(int x_idx);
extern void hit_silent_watcher(int y, int x);
extern bool hit_wall(int y, int x, bool do_action);

/* files.c */
extern void html_screenshot(cptr name);
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr name);
extern errr check_time(void);
extern errr check_time_init(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *fn);
extern void display_player_stat_info(int row, int col);
extern void display_player(int mode);
extern errr file_character(cptr name, bool full);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(void);
extern long total_points(void);
extern void show_scores(void);
extern void display_scores(int from, int to);
extern void close_game(void);
extern void exit_game_panic(void);
#ifdef HANDLE_SIGNALS
extern void (*(*signal_aux)(int, void (*)(int)))(int);
#endif
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);

/* generate.c */
extern void place_random_stairs(int y, int x);
extern void get_mon_hook(byte theme);
extern byte get_nest_theme(int nestlevel);
extern byte get_pit_theme(int pitlevel);
extern byte get_level_theme(s16b orig_theme_num, bool quest_level);
extern byte max_themed_monsters(const monster_race *r_ptr, u32b max_power);
extern void generate_cave(void);
extern void build_terrain(int y, int x, int feat);

extern void set_dungeon_type(u16b dungeon_type);

#ifdef ALLOW_DATA_DUMP

/*init1.c*/
extern void get_feature_name(char *desc, size_t max, byte feature_num);

#endif /*ALLOW_DATA_DUMP*/

/* init2.c */
extern void init_file_paths(char *path);
extern void create_user_dirs(void);
extern void init_angband(void);
extern void cleanup_angband(void);
extern void build_new_spells(void);

/* load.c */
extern bool load_player(void);

/* melee1.c */
extern int get_breath_dam(s16b hit_points, int gf_type, bool powerful);
extern bool make_attack_normal(monster_type *m_ptr);
extern bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px);
extern void mon_cloud(int m_idx, int typ, int dam, int rad);
extern void cloud_surround(int r_idx, int *typ, int *dam, int *rad);


/* melee2.c */
extern void apply_monster_trap(int f_idx, int y, int x, byte mode);

#ifdef MONSTER_SMELL

extern int get_scent(int y, int x);

#endif /*MONSTER_SMELL*/

extern bool cave_exist_mon(const monster_race *r_ptr, int y, int x,
							bool occupied_ok, bool damage_ok, bool can_dig);
extern void process_entities(void);

/* monster1.c */
extern void describe_monster(int r_idx, bool spoilers);
extern void roff_top(int r_idx);
extern void screen_roff(int r_idx);
extern void display_roff(int r_idx);
extern void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible);
extern bool prepare_ghost(int r_idx, bool from_savefile);

/* monster2.c */
extern s16b poly_r_idx(const monster_type *m_ptr);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_mon_list(void);
extern s16b mon_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level, int y, int x);
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
extern void monster_hide(monster_type *m_ptr);
extern void monster_unhide(monster_type *m_ptr);
extern s16b player_place(int y, int x);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern bool mon_resist_feat(u16b feat, int r_idx);
extern int place_monster_here(int y, int x, int r_idx);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern void calc_monster_speed(int y, int x);
extern void set_monster_haste(s16b m_idx, s16b counter, bool message);
extern void set_monster_slow(s16b m_idx, s16b counter, bool message);
extern bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp);
extern bool place_monster(int y, int x, bool slp, bool grp);
extern bool alloc_monster(int dis, bool slp);
extern bool summon_specific(int y1, int x1, int lev, int type);
extern bool summon_specific_single(int y1, int x1, int lev, int type);
extern void set_mon_fear(monster_type *m_ptr, int v, bool panic);
extern bool multiply_monster(int m_idx);
extern void message_pain(int m_idx, int dam);
extern bool add_monster_message(char *mon_name, s16b r_idx, byte msg_code);
extern void flush_monster_messages(void);
extern void update_smart_learn(int m_idx, int what);
extern void delete_current_bones_file(void);


/* obj-info.c */
extern int new_get_blows_times_ten(int dex_idx);
extern int get_num_blows_times_ten(const object_type *o_ptr, u32b f1);
extern bool object_info_out(const object_type *o_ptr);
extern void object_info_screen(const object_type *o_ptr);
extern bool format_object_history(char *buf, size_t max, const object_type *o_ptr);
extern bool history_interesting(const object_type *o_ptr);

/* object1.c */
extern bool object_has_hidden_powers(const object_type *o_ptr);
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native);
extern void strip_name(char *buf, int k_idx);
extern void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void mimic_desc_object(char *buf, size_t max, s16b mimic_k_idx);
extern void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void identify_random_gen(const object_type *o_ptr);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern s16b wield_slot(const object_type *o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_okay(const object_type *o_ptr);
extern int scan_floor(int *items, int size, int y, int x, int mode);
extern void display_inven(void);
extern void display_equip(void);
extern void show_inven(void);
extern void show_equip(void);
extern void show_floor(const int *floor_list, int floor_num);
extern void toggle_inven_equip(void);
extern bool get_item(int *cp, cptr pmt, cptr str, int mode);


/* object2.c */
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern object_type* get_first_object(int y, int x);
extern object_type* get_next_object(const object_type *o_ptr);
extern errr get_obj_num_prep(void);
extern s16b get_obj_num(int level);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern bool is_blessed(const object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern void object_into_artifact(object_type *o_ptr, artifact_type *a_ptr);
extern int ac_ceiling(object_type *o_ptr);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool interesting);
extern bool make_object(object_type *j_ptr, bool good, bool great, int objecttype, bool interesting);
extern bool prep_store_object(int storetype);
extern bool prep_object_theme(int themetype);
extern bool make_gold(object_type *j_ptr);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern void drop_near(object_type *j_ptr, int chance, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_object(int y, int x, bool good, bool great, int droptype);
extern void place_quest_artifact(int y, int x);
extern void place_gold(int y, int x);
extern bool vault_locked_door(int f_idx);
extern void inven_item_charges(int item);
extern void inven_item_describe(int item);
extern void inven_item_increase(int item, int num);
extern void inven_item_optimize(int item);
extern void floor_item_charges(int item);
extern void floor_item_describe(int item);
extern void floor_item_increase(int item, int num);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(const object_type *o_ptr);
extern s16b inven_carry(object_type *o_ptr);
extern s16b inven_takeoff(int item, int amt);
extern void inven_drop(int item, int amt);
extern void combine_pack(void);
extern void reorder_pack(void);
extern void steal_object_from_monster(int y, int x);
extern void create_quest_item(int ny, int nx);
extern byte allow_altered_inventory;
extern int get_tag_num(int o_idx, int cmd, byte *tag_num);
extern void find_quiver_size(void);
extern void combine_quiver(void);
extern int reorder_quiver(int slot);
extern bool is_throwing_weapon(const object_type *o_ptr);
extern int quiver_space_per_unit(const object_type *o_ptr);
extern bool quiver_carry_okay(const object_type *o_ptr, int num, int item);
extern byte quiver_get_group(const object_type *o_ptr);
extern void object_history(object_type *o_ptr, byte origin, s16b r_idx);
extern void stack_histories(object_type *o_ptr, const object_type *j_ptr);
extern void expand_inscription(const object_type *o_ptr, const char *src, char dest[], int max);
extern void format_object_flags(const object_type *o_ptr, char dest[], int max,
	bool only_random_powers);

/* quest.c */
extern void plural_aux(char *name, size_t max);
extern void describe_quest(char *random_name, size_t max, s16b level, int mode);
extern void get_title(char *buf, size_t max);
extern void display_guild(void);
extern void guild_purchase(void);
extern byte quest_check(int lev);
extern int quest_num(int lev);
extern int quest_item_slot(void);
extern void guild_quest_wipe(void);
extern void quest_fail(void);
extern bool format_quest_indicator(char dest[], int max);


/* randart.c */
extern void make_random_name(char *random_name, size_t max);
extern s32b artifact_power(int a_idx);
extern void build_randart_tables(void);
extern void free_randart_tables(void);
extern errr do_randart(u32b randart_seed, bool full);
extern bool make_one_randart(object_type *o_ptr, int art_power, bool namechoice);
extern void artifact_wipe(int a_idx, bool quest_art_wipe);
extern void make_quest_artifact(int lev);
extern void create_quest_artifact(object_type *o_ptr);
extern bool can_be_randart(const object_type *o_ptr);

/* save.c */
extern bool save_player(void);

/* spells1.c */

extern u16b bolt_pict(int y, int x, int ny, int nx, int typ);
extern byte gf_color(int type);
extern bool hates_location(int y, int x, const object_type *o_ptr);
extern void teleport_away(int m_idx, int dis);
extern void delayed_teleport_player(int dis);
extern void teleport_player(int dis);
extern void teleport_player_to(int ny, int nx);
extern void teleport_towards(int oy, int ox, int ny, int nx);
extern void teleport_player_level(int who);
extern void take_terrain_hit(int dam, int feat, cptr kb_str);
extern void take_hit(int dam, cptr kb_str);
extern void acid_dam(int dam, cptr kb_str);
extern void elec_dam(int dam, cptr kb_str);
extern void fire_dam(int dam, cptr kb_str);
extern void cold_dam(int dam, cptr kb_str);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, bool permanent);
extern bool new_dec_stat(int stat, int amount, bool permanent);
extern bool res_stat(int stat);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern void reveal_mimic(int y, int x, bool message);
extern bool project_m(int who, int y, int x, int dam, int typ, u32b flg);
extern bool project_p(int who, int y, int x, int dam, int typ, cptr msg);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
			 u32b flg, int degrees, byte source_diameter);

/* spells2.c */
extern bool hp_player(int num);
extern bool warding_glyph(void);
extern bool create_glacier(void);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern void identify_pack(void);
extern void uncurse_object(object_type *o_ptr);
extern bool remove_curse(bool star_curse);
extern bool restore_level(void);
extern void self_knowledge(void);
extern void set_recall(void);
extern bool detect_traps(void);
extern bool detect_doors(void);
extern bool detect_stairs(void);
extern bool detect_treasure(void);
extern bool detect_objects_gold(void);
extern bool detect_objects_normal(void);
extern bool detect_objects_magic(void);
extern bool detect_monsters_normal(void);
extern bool detect_monsters_living(void);
extern bool detect_monsters_invis(void);
extern bool detect_monsters_evil(void);
extern bool detect_monsters_animal(void);
extern void create_object(int tval, int sval);
extern bool detect_terrain(void);
extern bool detect_all(void);
extern void stair_creation(void);
extern bool item_tester_hook_ided_weapon(const object_type *o_ptr);
extern bool item_tester_hook_weapon(const object_type *o_ptr);
extern bool item_tester_hook_wieldable_ided_weapon(const object_type *o_ptr);
extern bool item_tester_hook_wieldable_weapon(const object_type *o_ptr);
extern bool item_tester_hook_ided_armour(const object_type *o_ptr);
extern bool item_tester_hook_armour(const object_type *o_ptr);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern int do_ident_item(int item, object_type *o_ptr, int quiet);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool item_tester_hook_recharge(const object_type *o_ptr);
extern void recharge_staff_wand(object_type *o_ptr, int lev, int num);
extern bool speed_monsters(void);
extern bool slow_monsters(int power);
extern bool sleep_monsters(int power);
extern bool banish_evil(int dist);
extern bool turn_undead(int power);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_monsters(int dam);
extern void aggravate_monsters(int who);
extern void mass_aggravate_monsters(int who);
extern bool banishment(void);
extern bool mass_banishment(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
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
extern bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int degrees);
extern bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
	u32b flg);
extern bool project_los(int y0, int x0, int dam, int typ);
extern void clear_temp_array(void);
extern void cave_temp_mark(int y, int x, bool room);
extern void spread_cave_temp(int y1, int x1, int range, bool room, bool pass_walls);
extern bool explosion(int who, int rad, int y0, int x0, int dam, int typ, u32b flg);
extern bool lite_line(int dir, int dam);
extern bool strong_lite_line(int dir);
extern bool drain_life(int dir, int dam);
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
extern bool is_player_immune(int gf_type);


/* squlech.c */
extern byte squelch_level[SQUELCH_BYTES];
extern void do_cmd_squelch_autoinsc(void);
extern int squelch_itemp(object_type *o_ptr, byte feeling, bool fullid);
extern int do_squelch_item(int squelch, int item, object_type *o_ptr);
extern void rearrange_stack(int y, int x);
extern void do_squelch_pile(int y, int x);
extern int get_autoinscription_index(s16b k_idx);
extern void autoinscribe_ground(void);
extern void autoinscribe_pack(void);
extern int remove_autoinscription(s16b kind);
extern int add_autoinscription(s16b kind, cptr inscription);
extern int apply_autoinscription(object_type *o_ptr);
extern char *squelch_to_label(int squelch);


/* store.c */
extern bool item_tester_hook_randart(const object_type *o_ptr);
extern bool item_tester_hook_flammable_book(const object_type *o_ptr);
extern void do_cmd_store(void);
extern void store_shuffle(int which);
extern void store_maint(int which);
extern void store_init(int which);


/*use-obj.c*/
extern bool use_object(object_type *o_ptr, bool *ident);

/* util.c */
extern errr path_parse(char *buf, size_t max, cptr file);
extern errr path_build(char *buf, size_t max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern FILE *my_fopen_temp(char *buf, size_t max);
extern errr my_fclose(FILE *fff);
extern errr my_fgets(FILE *fff, char *buf, size_t n);
extern errr my_fputs(FILE *fff, cptr buf, size_t n);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, long n);
extern errr fd_read(int fd, char *buf, size_t n);
extern errr fd_write(int fd, cptr buf, size_t n);
extern errr fd_close(int fd);
extern errr check_modification_date(int fd, cptr template_file);
extern void x_fprintf(FILE *fff, int encoding, cptr fmt, ...);
extern void text_to_ascii(char *buf, size_t len, cptr str);
extern void ascii_to_text(char *buf, size_t len, cptr str);
extern int macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern errr macro_free(void);
extern errr macro_trigger_free(void);
extern void flush(void);
extern void flush_fail(void);
extern char inkey(void);
extern void bell(cptr reason);
extern void sound(int val);
extern s16b quark_add(cptr str);
extern cptr quark_str(s16b i);
extern errr quarks_init(void);
extern errr quarks_free(void);
extern s16b message_num(void);
extern cptr message_str(s16b age);
extern u16b message_type(s16b age);
extern byte message_color(s16b age);
extern errr message_color_define(u16b type, byte color);
extern void message_add(cptr str, u16b type);
extern errr messages_init(void);
extern void messages_free(void);
extern void move_cursor(int row, int col);
extern void msg_print_raw(cptr msg);
extern void msg_print(cptr msg);
extern void msg_format(cptr fmt, ...);
extern void msg_c_format(u16b mtype, const char *fmt, ...);
extern void message(u16b message_type, s16b extra, cptr message);
extern void message_format(u16b message_type, s16b extra, cptr fmt, ...);
extern void message_flush(void);
extern void screen_save(void);
extern void screen_load(void);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void text_out_to_file(byte attr, cptr str);
extern void text_out_to_screen(byte a, cptr str);
extern void text_out(cptr str);
extern void text_out_c(byte a, cptr str);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, size_t len);
extern bool get_string(cptr prompt, char *buf, size_t len);
extern s16b get_quantity(cptr prompt, int max);
extern int get_check_other(cptr prompt, char other);
extern bool get_c_check(u16b msgt, cptr prompt);
extern bool get_check(cptr prompt);
extern int get_menu_choice(s16b max, char *prompt);
extern bool get_com(cptr prompt, char *command);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern int damroll(int num, int sides);
extern int color_char_to_attr(char c);
extern int color_text_to_attr(cptr name);
extern cptr attr_to_text(byte a);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

extern byte get_angle_to_grid[41][41];
extern int get_angle_to_target(int y0, int x0, int y1, int x1, int dir);
extern void get_grid_using_angle(int angle, int y0, int x0,
	int *ty, int *tx);
extern cptr get_ext_color_name(byte ext_color);
extern void grid_queue_create(grid_queue_type *q, size_t max_size);
extern void grid_queue_destroy(grid_queue_type *q);
extern bool grid_queue_push(grid_queue_type *q, byte y, byte x);
extern void grid_queue_pop(grid_queue_type *q);

extern int pick_random_item(int chance_values[], int max);

extern errr next_line_to_number(FILE *fff, int *dest);



/* xtra1.c */
extern void cnv_stat(int val, char *out_val);
extern s16b modify_stat_value(int value, int amount);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);

/* xtra2.c */
extern bool set_blind(int v);
extern bool allow_player_confusion(void);
extern bool set_confused(int v);
extern bool set_poisoned(int v);
extern bool set_afraid(int v);
extern bool set_paralyzed(int v);
extern bool set_image(int v);
extern bool set_fast(int v);
extern bool set_slow(int v);
extern bool set_flying(int v, bool can_crash);
extern bool set_shield(int v);
extern bool set_megashield(int v);
extern bool set_slay_elements(int v);
extern bool set_blessed(int v);
extern bool set_hero(int v);
extern bool set_shero(int v);
extern bool set_protevil(int v);
extern bool set_invuln(int v);
extern bool set_tim_invis(int v);
extern bool set_tim_infra(int v);
extern bool set_oppose_conf(int v);
extern bool set_oppose_acid(int v);
extern bool set_oppose_elec(int v);
extern bool set_oppose_fire(int v);
extern bool set_oppose_cold(int v);
extern bool set_oppose_pois(int v);
extern bool set_temp_native_lava(int v);
extern bool set_temp_native_oil(int v);
extern bool set_temp_native_sand(int v);
extern bool set_temp_native_forest(int v);
extern bool set_temp_native_water(int v);
extern bool set_temp_native_mud(int v);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(int v);
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern int  get_coin_type(const monster_race *r_ptr);
extern void monster_death(int m_idx, int who);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note, int who);
extern bool modify_panel(term *t, int wy, int wx);
extern bool adjust_panel(int y, int x);
extern bool change_panel(int dir);
extern void verify_panel(void);
extern void ang_sort_aux(void *u, void *v, int p, int q);
extern void ang_sort(void *u, void *v, int n);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern int target_dir(char ch);
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool target_set_interactive(int mode);
extern bool get_aim_dir(int *dp, bool target_trap);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);


/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAVE_USLEEP
/* util.c */
extern int usleep(unsigned long usecs);
# endif /* HAVE_USLEEP */
extern void user_name(char *buf, size_t len, int id);
#endif /* SET_UID */



/* util.c */
extern int interactive_input(bool look_ahead);
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_clear(void);
extern void repeat_check(void);
extern void repeat_reset_command(int cmd);




#ifdef ALLOW_BORG


/*
 * Some global variables for the borg.
 */

extern u32b count_stop;
extern int count_change_level;
extern int count_teleport;
extern byte allowed_depth[2];
extern byte borg_dir;

extern void do_cmd_borg(void);

#endif /* ALLOW_BORG */




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



#ifdef ALLOW_DEBUG
/* wizard2.c */
extern void do_cmd_debug(void);
#endif /* ALLOW_DEBUG */


#ifdef ALLOW_BORG
/* borg.h */
extern void do_cmd_borg(void);
#endif /* ALLOW_BORG */


#ifdef ALLOW_SPOILERS

/* wizard1.c */
extern void do_cmd_spoilers(void);

#endif /* ALLOW_SPOILERS */
extern bool make_fake_artifact(object_type *o_ptr, byte art_num);



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
