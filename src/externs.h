/* File: externs.h */

/*
 * Copyright (c) 2007 Ben Harrison
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, z-term.h, random.h)
 */

/* tables.c */
extern const s16b ddd[9];
extern const s16b ddc[8];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];
extern byte grids_in_radius[6];
extern s16b nearby_grids_y[37];
extern s16b nearby_grids_x[37];
extern byte los_nearby_table[803];
extern const char hexsym[16];
extern const char index_chars[];
extern const char index_chars_lower[];
extern const byte adj_mag_mana[];
extern const byte adj_mag_fail[];
extern const byte adj_chr_gold[];
extern const byte adj_int_dev[];
extern const byte adj_wis_sav[];
extern const byte adj_dis[];
extern const byte adj_dex_ta[];
extern const byte adj_dex_dodge[];
extern const byte adj_str_td[];
extern const byte adj_dex_th[];
extern const byte adj_str_th[];
extern const byte adj_str_wgt[];
extern const byte adj_str_hold[];
extern const byte adj_str_dig[];
extern const byte adj_str_blow[];
extern const byte adj_dex_blow[];
extern const byte adj_ma[];
extern const byte adj_con_fix[];
extern const byte adj_con_mhp[];
extern const byte deadliness_conversion[151];
extern const byte blows_table[12][12];
extern const byte extract_energy[200];
extern const player_sex sex_info[MAX_SEXES];
extern const s32b player_exp[PY_MAX_POWER + 1];
extern player_race race_info[MAX_RACES];
extern const player_magic magic_info[MAX_REALM + 1];
extern cptr projection_names[];
extern set_type s_info[MAX_S_IDX];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern int max_macrotrigger;
extern cptr macro_template;
extern cptr macro_modifier_chr;
extern cptr macro_modifier_name[MAX_MACRO_MOD];
extern cptr macro_trigger_name[MAX_MACRO_TRIGGER];
extern cptr macro_trigger_keycode[2][MAX_MACRO_TRIGGER];
extern cptr window_flag_desc[32];
extern cptr option_text[OPT_MAX];
extern cptr option_desc[OPT_MAX];
extern const bool option_norm[OPT_MAX];
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];
extern cptr custom_display_text[DISPLAY_MAX];
extern cptr inscrip_text[MAX_INSCRIP];
extern byte mana_cost_RF4[32];
extern byte mana_cost_RF5[32];
extern byte mana_cost_RF6[32];
extern byte mana_cost_RF7[32];
extern byte spell_desire_RF4[32][8];
extern byte spell_desire_RF5[32][8];
extern byte spell_desire_RF6[32][8];
extern byte spell_desire_RF7[32][8];
extern byte spell_range_RF4[32];
extern byte spell_range_RF5[32];
extern byte spell_range_RF6[32];
extern byte spell_range_RF7[32];
extern const byte race_adj_cost_skill[NUM_SKILLS][MAX_RACES];
extern const skill_type skill_info[NUM_SKILLS];
extern talent_type talent_info[NUM_TALENTS];
extern flag_data flag_creation_data[128];
extern cptr precog_msg_text[PRECOG_MSG_INDEX_MAX];
extern cptr character_type_name[PCHAR_MAX];
extern cptr character_type_desc[PCHAR_MAX];
extern graphics_data_type graphics_data[GRAPHICS_MAX];
extern byte term_size_min[TERM_MAX + 1][2];
extern byte misc_graphics_info[MISC_GRAPHICS_MAX][2];
extern cptr pval_desc_text[32];

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
extern int arg_sound;
extern int arg_graphics;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern bool character_existed;
extern s16b character_silent;
extern u32b seed_randart;
extern u32b seed_flavor;
extern u32b seed_town;
extern u16b seed_detection;
extern s16b object_level;
extern s16b old_object_level;
extern s16b monster_level;
extern s16b old_monster_level;
extern s16b project_immune;
extern s32b turn;
extern s32b old_turn;
extern int use_sound;
extern bool use_mouse;
extern int use_graphics;
extern int cursor_shape;
extern bool use_special_map;
extern s16b autosave_freq;
extern color_type color_table[MAX_COLORS];
extern s16b max_system_colors;
extern byte *custom_display;
extern bool use_fullscreen_view;
extern bool use_tall_display;
extern int screen_depth;
extern int main_screen_inactive;
extern s16b map_rows;
extern s16b map_cols;
extern bool more_tall_display;
extern bool map_display_precise_fit;
extern s16b clear_y;
extern s16b clear_x;
extern s16b image_count;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern bool inkey_cursor_hack[TERM_MAX];
extern s16b coin_type;
extern bool repair_mflag_show;
extern bool shimmer_objects;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern s16b t_max;
extern char summon_kin_type;
extern int summon_index_type;
extern int detect_y;
extern int detect_x;
extern bool message_to_window_active;
extern bool allow_activate;
extern byte feeling;
extern bool no_feeling_yet;
extern s16b level_rating;
extern byte dungeon_hgt;
extern byte dungeon_wid;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern char savefile[256];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern term *angband_term[TERM_MAX];
extern char angband_term_name[TERM_MAX+1][40];
extern cptr angband_sound_name[MSG_MAX];
extern cptr angband_music_name[MUSIC_MAX];
extern int view_n;
extern u16b *view_g;
extern int temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern int lite_n;
extern u16b *lite_g;
extern int effect_grid_n;
extern effect_grid_type *effect_grid;
extern u16b (*cave_info)[256];
extern byte (*cave_feat)[DUNGEON_WID_MAX];
extern s16b (*cave_o_idx)[DUNGEON_WID_MAX];
extern s16b (*cave_m_idx)[DUNGEON_WID_MAX];
extern byte (*cave_cost)[DUNGEON_WID_MAX];
extern byte (*cave_when)[DUNGEON_WID_MAX];
extern int scent_when;
extern int flow_center_y;
extern int flow_center_x;
extern int update_center_y;
extern int update_center_x;
extern int cost_at_center;
extern u16b path_g[128];
extern byte path_gx[128];
extern int path_n;
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *m_list;
extern effect_type *x_list;
extern trap_type *t_list;
extern monster_lore *l_list;
extern store_type *store;
extern object_type *inventory;
extern object_type *quiver;
extern bool *permit_kind_table;
extern byte *chance_kind_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern s16b move_moment_num;
extern move_moment_type *move_moment;
extern proj_graphics_type *proj_graphics;
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
extern player_type *p_ptr;
extern const player_sex *sp_ptr;
extern const player_race *rp_ptr;
extern const player_magic *mp_ptr;
extern player_other *op_ptr;
extern s32b add_wakeup_chance;
extern s32b total_wakeup_chance;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern feature_type *f_info;
extern char *f_name;
extern char *f_text;
extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern artifact_type *a_info;
extern char *a_name;
extern char *a_text;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern monster_race *r_info;
extern char *r_name;
extern char *r_text;
extern quest_type *q_info;
extern char *q_name;

extern char *q_text;
extern flavor_type *flavor_info;
extern char *flavor_name;
extern char *flavor_text;
extern cptr ANGBAND_SYS;
extern char *fat_data_suffix;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook)(const object_type*);
extern bool (*slot_tester_hook)(const int slot);
extern bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);
extern void (*ang_sort_swap)(void *u, void *v, int a, int b);
extern int  monster_space;
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*old_get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern bool (*old_get_obj_num_hook)(int k_idx);
extern byte required_tval;
extern byte old_required_tval;
extern u32b obj_gen_flags;
extern s16b object_desc_flavour;
extern s16b object_desc_plural;
extern u32b alloc_race_total;
extern u32b alloc_kind_total;
extern s32b get_quantity_default;
extern errr (*switch_display_hook)(int display);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern int text_border_left;
extern void (*special_view_hook)(int cols, int rows, bool activate);
extern int highscore_fd;
extern int dump_file_fd;
extern s16b skill_being_used;
extern byte bones_selector;
extern int r_ghost;
extern char ghost_name[DESC_LEN];
extern byte num_glyph_on_level;
extern byte num_trap_on_level;
extern byte num_recent_thefts;
extern byte player_graphics[MAX_RACES][MAX_SPECIALTIES][2];


/* attack.c */
extern bool test_hit_combat(int chance, int ac, int visible);
extern int monster_evade_or_resist(object_type *o_ptr,
	monster_type *m_ptr, byte blow_type);
extern void adjust_dam(int *damage, object_type *o_ptr, monster_type *m_ptr,
	bool is_trap);
extern int get_combat_noise(int min, int max);
extern void apply_deadliness(long *die_average, int deadliness);
extern bool py_attack(int y, int x);
extern void transfer_attributes_to_missile(object_type *i_ptr,
	const object_type *o_ptr);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);
extern void do_cmd_weapon_switch(void);
extern void do_cmd_barehanded(void);

/* birth.c */
extern void player_wipe(bool full);
extern void get_extra(void);
extern bool player_birth(void);


/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool no_light(void);
extern int darkness_ratio(int radius);
extern bool cave_valid_bold(int y, int x);
extern int shimmer_object(object_type *o_ptr, u32b f1, u32b f2, u32b f3);
extern bool feat_supports_lighting(byte feat);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
extern void move_cursor_relative(int y, int x);
extern void print_rel(char c, byte a, int y, int x);
extern void note_spot(int y, int x);
extern void lite_spot(int y, int x);
extern void map_animate(void);
extern void prt_map(void);
extern void display_map(int *cy, int *cx);
extern void do_cmd_view_map(void);
extern errr vinfo_init(void);
extern void forget_view(void);
extern void update_view(void);
extern void update_noise(bool full);
extern void update_smell(void);
extern void map_area(int y, int x, bool extended);
extern void wiz_lite(bool wizard, bool glow);
extern void wiz_dark(bool douse_lights);
extern void town_illuminate(bool daytime);
extern void cave_set_feat(int y, int x, int feat);
extern void clear_temp_array(void);
extern void cave_temp_mark(int y, int x, bool room);
extern void spread_cave_temp(int y1, int x1, int range, bool room);
extern int project_path(int range, \
                        int y1, int x1, int *y2, int *x2, u32b flg);
extern byte projectable(int y1, int x1, int y2, int x2, u32b flg);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void disturb(int stop_sneaking, int unused_flag);

/* cmd1.c */
extern void search(void);
extern void search_essence(bool strong);
extern void notice_unseen_objects(void);
extern void do_cmd_search(void);
extern bool quiver_carry(object_type *o_ptr, int o_idx);
extern byte py_pickup(int pickup);
extern void move_player(int dir, int do_pickup);
extern void run_step(int dir);
extern void cancel_running();

/* cmd2.c */
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
void do_cmd_sneaking(void);
extern int check_chest_traps(const object_type *o_ptr, bool had_traps);
extern bool hit_chest_trap(int y, int x, object_type *o_ptr);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_alter(bool deliberate);
extern void do_cmd_spike_aux(int y, int x);
extern void do_cmd_spike(void);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void do_cmd_hold(void);
extern void do_cmd_pickup(void);
extern void do_cmd_rest(void);
extern bool easy_open_door(int y, int x);


/* cmd3.c */
extern bool needs_two_hands(u32b f1, int weight);
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern bool item_tester_hook_wear_shapechange(const object_type *o_ptr);
extern void do_cmd_wield(void);
extern void do_cmd_takeoff(void);
extern void do_cmd_drop(void);
extern void do_cmd_destroy(void);
extern void do_cmd_observe(object_type *o_ptr, bool in_store);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
extern bool item_tester_light_source(const object_type *o_ptr);
extern void do_cmd_light_and_douse(void);
extern void do_cmd_target(void);
extern void do_cmd_look(u16b mode);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern bool ang_sort_comp_hook(const void *u, const void *v, int a, int b);
extern void ang_sort_swap_hook(void *u, void *v, int a, int b);
extern void py_steal(int y, int x);
extern bool py_set_trap(int y, int x, int dir);


/* cmd4.c */
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_options_aux(int page, cptr info, bool *modified);
extern void do_cmd_options(void);
extern void do_cmd_pref(void);
extern void do_cmd_macros(void);
extern void do_cmd_visuals(char cmd);
extern void do_cmd_colors(void);
extern void do_cmd_note(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(bool first_time);
extern void do_cmd_quest(void);
extern void do_cmd_save_screen(void);
extern void do_cmd_knowledge_quests(FILE *fp);
extern void do_cmd_knowledge(void);


/* cmd5.c */
extern cptr spell_type(void);
extern byte realm_color(void);
extern void print_spells(int tval, int sval, int y, int x);
extern void do_cmd_browse_aux(object_type *o_ptr);
extern void display_koff(int k_idx);
extern void do_cmd_browse(void);
extern cptr do_spell(int mode, int spell);


/* cmd6.c */
extern cptr do_object(int mode, object_type *o_ptr);
extern void use_object(int tval);
extern cptr do_device(int mode, object_type *o_ptr, bool *ident,
	bool *used, bool uncontrolled);
extern void learn_details(object_type *o_ptr);
extern int device_chance(const object_type *o_ptr);
extern void use_device(int tval);
extern cptr do_activation_aux(int mode, object_type *o_ptr);
extern void do_cmd_activate(void);


/* dungeon.c */
extern void tell_story(int part);
extern void process_player(void);
extern void play_game(bool new_game);


/* effects.c */
extern int effect_prep(void);
extern bool do_effect_linger(int x_idx, int y, int x);
extern int effect_grid_idx(int y, int x);
extern int effect_grid_proj_type(int y, int x);
extern void process_effects(void);


/* files.c */
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr name);
extern void display_player(int mode, bool change_display);
extern errr file_character(cptr name, bool full);
extern errr get_rnd_line(const char *file_name, char *output);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern bool display_file(char *filename);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_quit(void);
extern void do_cmd_save_game(bool is_autosave);
extern s32b total_points(void);
extern void print_tomb(void);
extern void display_scores_aux(int from, int to, int note, high_score *score);
extern errr predict_score(void);
extern void close_game(void);
extern void exit_game_panic(void);
#ifdef HANDLE_SIGNALS
extern void (*(*signal_aux)(int, void (*)(int)))(int);
#endif
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);


/* generate.c */
extern byte get_nearby_floor(int y, int x);
extern void place_unlocked_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void place_random_door(int y, int x);
extern void mon_essence(int y, int x, int r_idx);
extern void obj_essence(int y, int x, const object_type *o_ptr);
extern void destroy_level(bool new_level);
extern void generate_cave(void);

/* history.c */
extern history_info *history_list;
void history_clear(void);
u32b history_get_num(void);
bool history_add_full(u16b type, byte a_idx, s16b dlev, s16b clev, s32b turn, const char *text);
bool history_add(const char *event, u16b type, byte a_idx);
bool history_add_artifact(byte a_idx, bool known);
void history_unmask_unknown(void);
bool history_lose_artifact(byte a_idx);
void history_display(void);
void history_init(u32b entries);
void history_dump(FILE *fff);

/* info.c */
extern cptr obj_class_info[101];
extern void object_info(char *buf, object_type *o_ptr, bool reveal_flavor);
extern cptr item_activation(object_type *o_ptr);
extern void object_details(object_type *o_ptr, bool mental, bool known);
extern void self_knowledge(bool full);
extern void dump_obj_attrib(FILE *fff, object_type *o_ptr, int know_all);


/* init2.c */
extern void init_file_paths(char *path);
extern void create_user_dirs(void);
extern void init_angband(void);
extern void cleanup_angband(void);


/* loadsave.c */
extern bool save_player(void);
extern errr load_player(bool silent);
extern void save_savefile_names(void);
extern void savefile_load(bool force_menu);


/* monattk.c */
extern bool make_attack_normal(monster_type *m_ptr, int y, int x);
extern void mad_mon_melee(int m_idx, monster_type *m_ptr, int ty, int tx);
extern void mad_mon_retarget(int y0, int x0, int *y, int *x);
extern void mon_cloud(int m_idx, int typ, int dam, int rad);
extern bool make_attack_ranged(monster_type *m_ptr, int attack);
extern void cloud_surround(int r_idx, int *typ, int *dam, int *rad);


/* monmove.c */
extern const byte side_dirs[20][8];
extern int get_scent(int y, int x);
extern bool cave_exist_mon(monster_race *r_ptr, int y, int x, bool occupied_ok, bool can_dig);
extern bool player_invis(monster_type *m_ptr, bool apply_dist);
extern void monster_free_moves(monster_type *m_ptr, int perc_move);
extern void process_entities(void);


/* monster1.c */
extern bool know_armor(int r_idx, const monster_lore *l_ptr);
extern bool know_mana(int r_idx, const monster_lore *l_ptr);
extern void describe_monster(int r_idx, bool spoilers);
extern void roff_top(int r_idx, int row);
extern void screen_roff(int r_idx);
extern void display_roff(int r_idx);
extern void display_m_list(int y, int x, bool also_list_objects);
extern void short_m_name(char *name);
extern void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible);
extern bool prepare_ghost(int r_idx, monster_type *m_ptr, bool from_savefile);


/* monster2.c */
extern s16b poly_r_idx(int r_idx);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_m_list(void);
extern s16b m_pop(void);
extern void get_mon_num_prep(void);
extern s16b get_mon_num(int level);
extern void monster_desc(char *desc, const monster_type *m_ptr, int mode);
extern int lore_do_probe(int m_idx);
extern bool update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, object_type *j_ptr);
extern void mon_adjust_energy(monster_type *m_ptr, int adjust);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern s16b player_place(int y, int x);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp);
extern bool place_monster(int y, int x, bool slp, bool grp);
extern bool alloc_monster(int dis, bool slp);
extern int summon_specific(int y1, int x1, bool scattered, int lev, int type, int num);
extern void set_mon_fear(monster_type *m_ptr, int v, bool panic);
extern bool multiply_monster(int m_idx);
extern void message_pain(int m_idx, int dam, int fear, char *hit_msg);
extern void update_smart_learn(int m_idx, int what);
extern void mon_death_effect(int m_idx);
extern bool monster_loot(int max, bool steal, monster_type *m_ptr);
extern void monster_death(int m_idx);
extern bool mon_take_hit(int m_idx, int who, int dam, bool *fear, cptr note);
long monster_exp(monster_race *r_ptr);
long monster_exp_frac(monster_race *r_ptr);


/* obj_make.c */
extern int get_essence(bool just_looking);
extern int essence_to_magic(int *adjust, int *sval);
extern bool enough_essences(const object_type *o_ptr);
extern bool use_up_essences(const object_type *o_ptr);
extern void essence_wild_magic(object_type *o_ptr, int dam);
extern bool do_alchemy(void);
extern bool make_launcher_or_ammo(void);
extern bool make_melee_weapon(void);
extern bool make_armor(void);
extern bool poison_ammo(int num);


/* object1.c */
extern void flavor_init(void);
extern void easy_know_init(void);
extern void reset_visuals(void);
extern s16b get_object_pval(const object_type *o_ptr, u32b flg);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_flags_extra(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void object_desc_store(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void strip_name(char *buf, int k_idx);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern s16b wield_slot(const object_type *o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_okay(const object_type *o_ptr);
extern void display_inven(void);
extern void display_equip(void);
extern void show_inven(void);
extern void show_equip(void);
extern bool scan_floor(int *items, int *item_num, int y, int x, int mode);
extern void show_floor(const int *floor_list, int floor_num, bool gold, bool blind);
extern void toggle_inven_equip(void);
extern bool get_item(int *cp, cptr pmt, cptr str, int mode);
extern bool check_set(int s_idx);
extern void apply_set(int s_idx);
extern cptr remove_set(int s_idx);
extern void check_item_sets(void);
extern void display_nearby_objects(int y, int x, bool also_list_monsters);


/* object2.c */
extern s16b m_bonus(int max, int level, int max_level);
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern object_type* get_first_object(int y, int x);
extern object_type* get_next_object(const object_type *o_ptr);
extern void get_obj_num_prep(void);
extern s16b get_obj_num(int level);
extern void object_mental(object_type *o_ptr);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern s32b pval_value(u32b flg, int pval);
extern s32b object_value_real(const object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *i_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern bool make_cursed_ego_item(object_type *o_ptr);
extern bool make_fake_artifact(object_type *o_ptr, int artifact_index);
extern void apply_random_qualities(object_type *o_ptr);
extern void apply_magic(object_type *o_ptr, int lev, int okay, bool good, bool great);
extern bool make_object(object_type *j_ptr, bool good, bool great, bool exact_kind);
extern void make_specific_tval(int tval, int lev, bool in_pack);
extern bool make_gold(object_type *o_ptr);
extern bool make_special_gold(object_type *o_ptr);
extern int breakage_chance(object_type *o_ptr);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern void drop_near(object_type *o_ptr, int chance, int y, int x, byte flags);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_object(int y, int x, bool good, bool great, bool exact_kind);
extern void place_gold(int y, int x);
extern void make_boulder(int y, int x, int level);
extern void make_food(int y, int x);
extern void inven_item_charges(int item);
extern void use_item_describe(int item, int mode);
extern void inven_item_describe(int item);
extern void inven_item_increase(int item, int num);
extern void inven_item_optimize(int item);
extern void floor_item_charges(int item);
extern void floor_item_describe(int item);
extern void floor_item_increase(int item, int num);
extern void inven_item_decrease(int item);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(const object_type *o_ptr);
extern bool quiver_carry_okay(const object_type *o_ptr);
extern s16b inven_carry(object_type *o_ptr);
extern void steal_object(object_type *o_ptr);
extern void give_object(object_type *o_ptr, bool allow_equip);
extern s16b inven_takeoff(int item, int amt);
extern void inven_drop(int item, int amt);
extern bool kind_fits_tval(int k_idx);
extern void combine_pack(void);
extern int reorder_pack(int slot, int store_num, bool verbose);
extern int quiver_count_item(const object_type *o_ptr, int quantity);
extern int quiver_count(void);
extern void find_quiver_size(void);
extern int process_quiver(int num_new, object_type *o_ptr);
extern bool switch_weapons(bool allow_empty);


/* quest.c */
extern void plural_aux(char *Name);
extern cptr describe_quest(s16b level, int mode);
extern void insure_quest_monsters(void);
extern cptr inn_names[11];
extern void display_inn(void);
extern void inn_purchase(int item);
extern byte quest_check(int lev);
extern int quest_num(int lev);
extern void check_quest_failure(int mode);


/* randart.c */
extern byte pval_range[32][2];
extern s16b get_cost_of_flag(int flag, int val);
extern int get_max_potential(int a_idx);
extern void design_temporary_artifact(int a_idx, int v, bool corrupted);
extern int init_temporary_artifact(int a_idx, int tval, int sval);
extern void initialize_random_artifacts(void);


/* skills.c */
extern s16b get_skill(int skill, int min, int max);
extern s16b get_skill_race(int skill, int min, int max);
extern int best_melee_skill(void);
extern int sweapon(int tval);
extern int sbow(int tval);
extern void calc_power(void);
extern int calc_exp_power(void);
extern int calc_max_power(void);
extern void adv_cost_reduce_similar(int skill, s32b *base_cost, byte mode);
extern s32b adv_cost(int skill, bool add_practice_cost);
extern bool alter_skill(int skill, int change, bool perm);
extern bool raise_skills(int amount);
extern void do_cmd_skills(void);


/* spells1.c */
extern u16b bolt_pict(int y, int x, int ny, int nx, int typ);
extern byte spell_color(int type);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int degrees, byte source_diameter);


/* spells2.c */
extern void find_target(int dir, int range, int y0, int x0, int *y1, int *x1);
extern bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg);
extern bool project_beam(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg);
extern bool project_ball(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int source_diameter);
extern bool explosion(int who, int rad, int y0, int x0, int dam, int typ);
extern bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ);
extern bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int degrees);
extern bool project_star(int who, int rad, int y0, int x0, int y1, int x1, int dam,
	int typ, u32b flg);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg,
	int source_diameter);
extern bool fire_arc(int typ, int dir, int dam, int rad, int degrees);
extern bool fire_star(int typ, int dir, int dam, int rad);
extern void fire_storm(int who, int typ0, int y0, int x0, int dam, int rad,
	int len, byte projection, bool lingering);
extern bool beam_burst(int y, int x, int typ, int num, int dam);
extern bool project_los(int y0, int x0, int dam, int typ);
extern void teleport_away(int m_idx, int dis, bool require_los);
extern void thrust_away(int who, int t_y, int t_x, int grids_away);
extern void thrust_toward(int who, int t_y, int t_x, int grids_away);
extern void teleport_player(int dis, bool safe, bool require_los);
extern void teleport_towards(int oy, int ox, int ny, int nx);
extern void teleport_player_to(int ny, int nx, int dis,
	bool allow_vault, int mode);
extern void teleport_player_level(void);
extern bool phase_warp(int range, int spread, bool wizard);
extern bool do_blink_away(int who);
extern bool passwall(int dir, bool local);
extern void acid_dam(int dam, int msg_type, cptr hit_str, cptr kb_str);
extern void elec_dam(int dam, int msg_type, cptr hit_str, cptr kb_str);
extern void fire_dam(int dam, int msg_type, cptr hit_str, cptr kb_str);
extern void cold_dam(int dam, int msg_type, cptr hit_str, cptr kb_str);
extern bool apply_disenchant(int mode);
extern int apply_draining(int power);
extern void apply_nexus(int fy, int fx, int dam);
extern void recall_player(void);
extern bool hp_player(int num);
extern bool extra_hp_player(int num);
extern bool heal_player(int perc, int min);
extern int get_heal_amount(int perc, int min);
extern bool sp_player(int num, cptr msg);
extern bool inc_stat(int stat, int points);
extern bool do_inc_stat(int stat, int points, cptr msg);
extern bool dec_stat(int stat, int points, int permanent);
extern bool do_dec_stat(int stat, int points, bool perm, cptr msg_drain,
	cptr msg_sustain);
extern bool res_stat(int stat);
extern bool do_res_stat(int stat, cptr msg);
extern bool restore_stats(void);
extern void shuffle_stats(int num);
extern void disease(int *damage);
extern bool lose_all_info(cptr msg);
extern void curse_player(int power);

extern bool speed_monsters(void);
extern bool slow_monsters(int power);
extern bool slow_undead(int power);
extern bool sleep_monsters(int power);
extern bool fear_demons(int power);
extern bool fear_monsters(int power);
extern bool confu_monsters(int power);
extern bool banishment(u32b flags, int power);
extern bool turn_undead(int power);
extern bool turn_evil_priest(int power);
extern bool dispel_monsters(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_undead(int dam);
extern bool dispel_animals(int dam);
extern bool dispel_small_monsters(int dam);
extern bool dispel_light_hating(int dam);
extern bool exorcise_monsters(int power);
extern bool judgement(int power);
extern void aggravate_monster_race(u32b race_flag, cptr weapon, cptr monster);
extern void aggravate_monsters(int who, bool entire_level, cptr msg);
extern bool make_monsters_wary(int y, int x, bool req_los, bool trap);
extern bool genocide(char typ);
extern bool mass_genocide(int y, int x);
extern bool probing(void);

extern bool lite_line(int dir);
extern bool drain_life(int dir, int dam);
extern bool heal_monster(int dir, int dam);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir, int power);
extern bool sleep_monster(int dir, int power);
extern bool confuse_monster(int dir, int power);
extern bool stun_monster(int dir, int power);
extern bool poly_monster(int dir, int power);
extern bool clone_monster(int dir);
extern bool fear_monster(int dir, int power);
extern bool curse_monster(int dir, int power);
extern bool come_hither(int dir);
extern bool smite_evil(int dir, int power);
extern bool exorcise_monster(int dir, int power);
extern bool dispel_an_undead(int dir, int dam);
extern bool dispel_a_demon(int dir, int dam);
extern bool dispel_a_dragon(int dir, int dam);
extern bool teleport_monster(int dir);
extern bool sleep_monsters_touch(int power);

extern bool detect_monsters(bool extended, bool match, u32b flags,
	int flag_set, const char *str, bool verbose);
extern bool detect_traps(bool extended, bool verbose);
extern bool detect_doors(bool extended);
extern bool detect_stairs(bool extended);
extern bool detect_treasure(bool extended);
extern bool detect_objects_gold(bool extended);
extern bool detect_objects_normal(bool extended);
extern bool detect_objects_magic(bool extended);
extern bool detect_objects_in_room(int y0, int x0);
extern bool detect_monsters_normal(bool extended, bool verbose);
extern bool detect_monsters_invis(bool extended, bool verbose);
extern bool detect_evil(bool extended, bool verbose);
extern bool detect_undead(bool extended, bool verbose);
extern bool detect_life(bool extended, bool verbose);
extern bool detect_animals(bool extended, bool verbose);
extern bool detect_all_monsters(bool extended, bool verbose);
extern bool detect_all(bool extended, bool verbose);

extern void destroy_area(int y1, int x1, int r, bool full, bool hit_center);
extern bool call_destruction(bool safe);
extern bool collapse_ceiling(int cy, int cx, int severity);
extern void earthquake(int cy, int cx, int r);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern int concentrate_light(int who, int y0, int x0, int radius, char *desc,
	size_t desc_len, bool for_real);
extern void stair_creation(void);
extern bool wall_to_mud(int dir, int dam);
extern bool destroy_door(int dir);
extern bool jam_door(int dir);
extern bool fetch_obj(int dir, int wgt);
extern bool disarm_trap(int dir);
extern bool door_creation(void);
extern bool trap_creation(int y, int x);
extern bool destroy_doors_touch(void);
extern bool force_doors_touch(void);
extern bool force_door(int dir);

extern void predict_weather(int accuracy);
extern bool change_weather(s16b humid, s16b wind, s16b temp);


/* spells3.c */
extern void food_hit_effect(int who, int y, int x, object_type *o_ptr);
extern bool potion_smash_effect(int who, int y, int x, object_type *o_ptr);
extern int scroll_read_effect(int who, int y, int x, object_type *o_ptr);
extern bool device_use_effect(int who, int power, int y, int x,
	object_type *o_ptr);
extern bool check_blanket(int mode, int dam);
extern void fire_off_devices(int chance);
extern bool hates_acid(const object_type *o_ptr);
extern bool hates_elec(const object_type *o_ptr);
extern bool hates_fire(const object_type *o_ptr);
extern bool hates_cold(const object_type *o_ptr);
extern int set_acid_destroy(const object_type *o_ptr);
extern int set_elec_destroy(const object_type *o_ptr);
extern int set_fire_destroy(const object_type *o_ptr);
extern int set_cold_destroy(const object_type *o_ptr);
typedef int (*inven_func) (const object_type *);
extern int inven_damage(inven_func typ, int perc0, int dam);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern void curse_equipment(int power);
extern void uncurse_object(object_type *o_ptr);
extern int remove_curse(void);
extern int remove_all_curse(void);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac, bool ego);
extern bool brand_missile(int ammo_type, int brand_type);
extern void sense_object(object_type *o_ptr, int slot, bool strong,
	bool force_heavy);
extern bool scan_object_priest(bool full);
extern void learn_about_wearable(object_type *o_ptr, int slot, bool strong);
extern bool sense_magic(void);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern void identify_pack(void);
extern bool recharge(int num, bool essence);
extern bool tap_magical_energy(void);

extern void doomspells(bool hurt, int skill);
extern void call_chaos(int dam);
extern void slaughterfield(int dam, object_type *o_ptr);
extern void nightfall(void);
extern int stare_into_the_palantir(void);

/* store.c */
extern byte bargain_difficulty;
extern owner_type owners[MAX_STORES][MAX_OWNERS];
extern stock_type store_stock[STORE_STOCK_SIZE];
extern byte tval_sell[MAX_STORES][10];
extern byte rgold_adj[MAX_RACES][MAX_RACES];
extern void do_cmd_store(void);
extern void store_shuffle(int which);
extern void store_maint(int num, bool full);
extern void store_init(int which);
extern void process_world_aux_home(void);

/* talents.c */
extern void pseudo_probe(void);
extern int dodging_ability(int max);
extern bool can_precog(int max_chance, int cutoff);
extern int can_use_talent(int talent, int talent_choice);
extern void do_cmd_talents(int talent_choice);

/* traps.c */
extern trap_kind t_kind_info[TRAP_KIND_MAX];
extern bool cave_trap_specific(int y, int x, int t_idx);
extern bool cave_loose_rock(int y, int x);
extern bool cave_glyph(int y, int x);
extern bool cave_pit_trap(int y, int x);
extern bool cave_monster_trap(int y, int x);
extern bool cave_visible_trap(int y, int x);
extern bool cave_invisible_trap(int y, int x);
extern bool get_trap_graphics(int y, int x, byte *a, char *c,
	bool require_visible);
extern void no_exp_traps(int y, int x);
extern bool reveal_trap(int y, int x, int chance, bool msg, bool see_loose_rocks);
extern bool get_trap(int y, int x, int *idx);
extern int nasty_traps(int y, int x, int vis);
extern bool cave_trap_allowed(int y, int x);
extern bool place_trap(int y, int x, int t_idx, int trap_level);
extern bool warding_glyph(int y, int x);
extern void wipe_t_list(void);
extern bool remove_trap(int y, int x, int t_idx);
extern void remove_trap_kind(int y, int x, int t_idx);
extern bool has_disarmable_trap(int y, int x);
extern bool magic_disarm(int y, int x, int chance);
extern int load_trap(int y, int x);
extern bool loot_trap(int y, int x, int t_idx);
extern bool hit_trap(int who, int y, int x);


/* util.c */
extern void pause_for(int msec);
extern void strlower(char *buf);
extern cptr format_literal(char *str);
extern void x_fprintf(FILE *fff, int encoding, cptr fmt, ...);
extern void text_to_ascii(char *buf, size_t len, cptr str);
extern void ascii_to_text(char *buf, size_t len, cptr str);
extern int macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern errr macro_free(void);
extern errr macro_trigger_free(void);
extern void flush(void);
extern char inkey(int allow_mouse);
extern void msg_add(cptr msg);
extern void bell(cptr reason);
extern void sound(int num);
extern void music(int num);
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
extern errr message_init(void);
extern errr messages_init(void);
extern void messages_free(void);
extern void msg_print(cptr msg);
extern void msg_format(cptr fmt, ...);
extern void debug(cptr fmt, ...);
extern void message(u16b type, s16b delay, cptr text);
extern void message_format(u16b type, s16b delay, cptr fmt, ...);
extern void message_flush(void);
extern void move_cursor(int row, int col);
extern void set_cursor(int term_idx, int show);
extern void screen_center_x(int width);
extern void screen_center_y(int height);
extern int display_width(void);
extern void screen_clear(void);
extern void screen_save(bool clear_screen);
extern void screen_load(void);
extern void display_change(u32b flags, int wid, int hgt);
extern void clear_row(int row);
extern void clear_from(int row);
extern void clear_space(int row, int col, int num);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void add_str(cptr str);
extern void center_string(char *buf, size_t buflen, cptr str, int length);
extern void text_out_to_file(byte attr, cptr str);
extern void text_out_to_screen(byte a, cptr str);
extern void text_out_c(byte a, cptr str);
extern void text_out(cptr str);
extern void c_roff(byte a, cptr str, byte indent, byte wrap);
extern void roff(cptr str, byte l_margin, byte r_margin);
extern void c_roff_centered(byte a, cptr str, int l_margin, int r_margin);
extern void get_ui_direction(char *k, byte flags, bool *shift_key);
extern bool askfor_aux(char *buf, int len, bool numpad_cursor);
extern bool get_string(cptr prompt, char *buf, size_t len);
extern s32b get_quantity(cptr prompt, s32b min, s32b max);
extern bool get_check(cptr prompt);
extern bool get_check_default(cptr prompt, bool yes);
extern bool get_com(cptr prompt, char *command);
extern int get_index(char ch, bool require_lower);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern bool insert_str(char *buf, cptr target, cptr insert);
extern bool repeat_pull(int *what);
extern void repeat_clear(void);
extern void repeat_push(int what);
extern void repeat_check(void);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

extern int color_char_to_attr(char c);
extern byte verify_color(byte attr);
extern byte translate_into_16_colors(int idx);
extern int damroll(int num, int sides);
extern void dam_to_dice(int dam, int *dice, int *sides, bool allow_random);
extern u16b rsqrt(s32b input);
extern int make_metric(int wgt);
extern s32b round_it(const s32b v, int frac);
extern byte get_angle_to_grid[41][41];
extern int get_angle_to_target(int y0, int x0, int y1, int x1, int dir);
extern void get_grid_using_angle(int angle, int y0, int x0,
	int *ty, int *tx);
extern int get_loc_of_flag(u32b flag);

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




/* wizard2.c */
#ifdef ALLOW_DEBUG
extern void do_cmd_debug(void);
extern tval_desc tvals[];
#endif


/* xtra1.c */
extern cptr get_title(int len, bool wizard, bool quotes);
extern void get_fame_desc(int *attr, char *fame_desc);
extern void calc_hitpoints(void);
extern void calc_mana(void);
extern void cnv_stat(char *out_val, size_t size, int val);
extern s16b modify_stat(int value, int amount);
extern void health_redraw_aux(monster_type *m_ptr, int row, int col);
extern bool left_panel_display(byte item, byte mode);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);
extern s16b calc_hp_regen(void);
extern s16b calc_mana_regen(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3, bool shape, bool modify);
extern void player_flags_vulnerable(u32b *f1, u32b *f2, u32b *f3, bool shape);
extern void player_flags_cancel(u32b *f1, u32b *f2, u32b *f3, bool shape);
extern int missile_bonus(u32b flag_pval, int skill);
extern int player_flags_pval(u32b flag_pval, bool shape);
extern int weapon_blows(object_type *o_ptr, bool primary);


/* xtra2.c */
extern bool set_blind(int v, cptr msg);
extern bool set_confused(int v);
extern bool set_image(int v);
extern bool set_poisoned(int v);
extern bool set_diseased(int v, cptr msg);
extern bool set_afraid(int v);
extern bool set_paralyzed(int v);
extern bool set_fast(int v);
extern bool set_slow(int v);
extern bool set_shield(int v, cptr msg);
extern bool set_steelskin(int v, cptr msg);
extern bool set_blessed(int v, cptr msg);
extern bool set_holy(int v);
extern bool set_bold(int v);
extern bool set_hero(int v);
extern bool set_berserk(int v);
extern bool set_necro_rage(int v);
extern bool set_protevil(int v);
extern bool set_wiz_prot(int v);
extern bool set_esp_evil(int v);
extern bool set_tim_esp(int v);
extern bool set_detect_inv(int v);
extern bool set_tim_infra(int v);
extern bool set_invis(int v, int p);
extern bool set_hold_weather(int v);
extern bool set_regen_hp(int v);
extern bool set_regen_mana(int v);
extern bool set_vitality(int v);
extern bool set_mania(int v);
extern bool set_res_dam(int v);
extern bool set_oppose_acid(int v);
extern bool set_oppose_elec(int v);
extern bool set_oppose_fire(int v);
extern bool set_oppose_cold(int v);
extern bool set_oppose_pois(int v);
extern bool set_oppose_ethereal(int v);
extern bool set_acid_attack(int v);
extern bool set_elec_attack(int v);
extern bool set_fire_attack(int v);
extern bool set_cold_attack(int v);
extern bool set_pois_attack(int v);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(s32b v);
extern bool set_recall(int v);
extern bool set_dancing_feet(int v, cptr msg, bool safe);
extern bool set_phasing_foes(int v, cptr msg);
extern bool set_blink_away(int v);
extern bool set_evasion(int v);
extern bool set_aura_fire(int v);
extern bool set_aura_cold(int v);
extern bool set_mental_barrier(int v);
extern bool set_forbid_summoning(int v);
extern bool set_wraithform(int v);
extern bool shapechange_pern(s16b shape);
extern bool shapechange_temp(int v, s16b shape);
extern bool set_pois_power(int v, int dur);
extern bool set_chaos_power(int v, int dur);
extern bool set_nexus_field(int v, int dam);
extern bool set_luck(int v, cptr msg);
extern bool set_unsanctified(int v);
extern bool set_self_knowledge(int v, cptr msg);
extern void shapechange(s16b shape);
extern void do_cmd_unchange(bool voluntary);
extern void practice_skill(s32b amount, s16b skill);
extern void check_experience(void);
extern s32b calc_spent_exp(void);
extern s32b calc_spent_exp_max(void);
extern void gain_exp(s32b amount, s16b skill);
extern void lose_exp(s32b amount, bool perm);
extern bool restore_level(void);
extern bool take_hit(int dam, int msg_type, cptr hit_str, cptr kb_str);
extern void calc_map_size(byte cols, byte rows);
extern int get_panel_wid(void);
extern int get_panel_hgt(void);
extern bool modify_panel(int wy, int wx);
extern bool adjust_panel(int y, int x);
extern bool change_panel(int dir);
extern void verify_panel(int dir, bool look);
extern void ang_sort_aux(void *u, void *v, int p, int q);
extern void ang_sort(void *u, void *v, int n);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern int target_dir(char ch);
extern bool target_able(int m_idx, bool use_sight);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool target_set_interactive(u16b mode);
extern bool get_aim_dir(int *dp);
extern bool get_aim_dir_target(int *dp);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);
extern void precog_msg(int precog_msg_idx);
extern void danger_music_level(bool change_right_now);


#ifdef ALLOW_BORG
/*
 * Some global variables for the "mindless" borg.
 */

extern u32b count_stop;	         /* Turns to automatic stop */
extern int count_change_level;   /* Turns to next level change */
extern int count_teleport;       /* Turns to next teleport */
extern byte allowed_depth[2];    /* Minimum and maximum depths */
extern byte borg_dir;            /* Current direction */

#endif /* ALLOW_BORG */

