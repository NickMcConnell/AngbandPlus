/* File: externs.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */


/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, term.h, random.h)
 */


/*
 * Automatically generated "variable" declarations
 */

/* tables.c */
extern const s16b ddd[9];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];
extern const char hexsym[16];
extern const byte adj_mag_study[];
extern const byte adj_mag_mana[];
extern const byte adj_mag_fail[];
extern const byte adj_mag_stat[];
extern const byte adj_chr_gold[];
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
extern const byte adj_con_mhp[];
extern const byte blows_table[12][12];
extern const byte extract_energy[200];
extern const s32b player_exp[PY_MAX_LEVEL];
extern const player_sex sex_info[MAX_SEXES];
extern const byte chest_traps[64];
extern const cptr color_names[16];
extern const cptr stat_names[A_MAX];
extern const cptr stat_names_reduced[A_MAX];
extern const cptr window_flag_desc[32];
extern const cptr option_text[OPT_MAX];
extern const cptr option_desc[OPT_MAX];
extern const bool option_norm[OPT_MAX];
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];
extern const cptr inscrip_text[MAX_INSCRIP];
extern const u32b object_xtra_base[MAX_HIDDEN];
extern const int object_xtra_what[MAX_HIDDEN];
extern const int object_xtra_size[MAX_HIDDEN];
extern const cptr object_group_text[];
extern const byte object_group_tval[];

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
extern s32b turn;
extern s32b old_turn;
extern bool use_sound;
extern bool use_graphics;
extern bool use_bigtile;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern s16b coin_type;
extern s16b food_type;
extern s16b race_drop_idx;
extern bool opening_chest;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_show;
extern bool repair_mflag_mark;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern object_type term_object;
extern bool term_obj_real;
extern byte feeling;
extern s16b rating;
extern byte level_flag;
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
extern sint view_n;
extern u16b *view_g;
extern sint temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern sint dyna_n;
extern u16b *dyna_g;
extern byte dyna_cent_y;
extern byte dyna_cent_x;
extern bool dyna_full;
extern byte (*cave_info)[256];
extern s16b (*cave_feat)[DUNGEON_WID];
extern s16b (*cave_o_idx)[DUNGEON_WID];
extern s16b (*cave_m_idx)[DUNGEON_WID];
extern byte (*cave_cost)[DUNGEON_WID];
extern byte (*cave_when)[DUNGEON_WID];
extern maxima *z_info;
extern int scent_when;
extern int flow_center_y;
extern int flow_center_x;
extern int update_center_y;
extern int update_center_x;
extern int cost_at_center;
extern room_info_type room_info[DUN_ROOMS];
extern byte dun_room[MAX_ROOMS_ROW][MAX_ROOMS_COL];
extern object_type *o_list;
extern monster_type *m_list;
extern monster_lore *l_list;
extern object_lore *a_list;
extern object_lore *e_list;
extern object_lore *k_list;
extern quest *q_list;
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
extern const player_class *cp_ptr;
extern player_other *op_ptr;
extern player_type *p_ptr;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern feature_type *f_info;
extern char *f_name;
extern char *f_text;
extern desc_type *d_info;
extern char *d_name;
extern char *d_text;
extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern artifact_type *a_info;
extern char *a_name;
extern char *a_text;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern flavor_type *x_info;
extern char *x_name;
extern char *x_text;
extern monster_race *r_info;
extern char *r_name;
extern char *r_text;
extern player_race *p_info;
extern char *p_name;
extern char *p_text;
extern player_class *c_info;
extern char *c_name;
extern char *c_text;
extern spell_type *s_info;
extern char *s_name;
extern char *s_text;
extern weapon_style *w_info;
extern rune_type *y_info;
extern char *y_name;
extern char *y_text;
extern town_type *t_info;
extern char *t_name;
extern char *t_text;
extern store_type *u_info;
extern char *u_name;
extern char *u_text;
extern hist_type *h_info;
extern char *h_text;
extern owner_type *b_info;
extern char *b_name;
extern char *b_text;
extern byte *g_info;
extern char *g_name;
extern char *g_text;
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
extern bool (*ang_sort_comp)(vptr u, vptr v, int a, int b);
extern void (*ang_sort_swap)(vptr u, vptr v, int a, int b);
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern bool (*get_feat_num_hook)(int k_idx);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern int highscore_fd;
extern bool use_transparency;


/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern sint distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern bool feat_supports_lighting(byte feat);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
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
extern void update_dyna(void);
extern void update_noise(void);
extern void update_smell(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void cave_set_feat(int y, int x, int feat);
extern int feat_state(int feat, int action);
extern void cave_alter_feat(int y, int x, int action);
extern sint project_path(u16b *gp, int range, \
                         int y1, int x1, int y2, int x2, int flg);
extern bool projectable(int y1, int x1, int y2, int x2);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void artifact_track(int a_idx);
extern void object_kind_track(int k_idx);
extern void object_actual_track(const object_type *j_ptr);
extern void disturb(int stop_search, int unused_flag);
extern bool is_quest(int level);

/* cmd1.c */
extern bool test_hit_fire(int chance, int ac, int vis);
extern bool test_hit_norm(int chance, int ac, int vis);
extern sint critical_shot(int weight, int plus, int dam);
extern sint critical_norm(int weight, int plus, int dam);
extern sint tot_dam_aux(object_type *o_ptr, int tdam, const monster_type *m_ptr);
extern void find_secret(int y, int x);
extern void search(void);
extern bool auto_pickup_ignore(const object_type *o_ptr);
extern void py_pickup(int pickup);
extern void hit_trap(int y, int x);
extern void py_attack(int y, int x);
extern bool stuck_player(int *dir);
extern void move_player(int dir, int jumping);
extern void run_step(int dir);

/* cmd2.c */
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_search(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_alter(void);
extern void do_cmd_set_trap_or_spike(void);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void do_cmd_hold(void);
extern void do_cmd_stay(void);
extern void do_cmd_rest(void);
extern int breakage_chance(object_type *o_ptr);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);

/* cmd3.c */
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern void do_cmd_wield(void);
extern void do_cmd_takeoff(void);
extern void do_cmd_drop(void);
extern void do_cmd_destroy(void);
extern void do_cmd_observe(void);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern bool ang_sort_comp_hook(vptr u, vptr v, int a, int b);
extern void ang_sort_swap_hook(vptr u, vptr v, int a, int b);

/* cmd4.c */
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_options(void);
extern void do_cmd_pref(void);
extern void do_cmd_macros(void);
extern void do_cmd_visuals(void);
extern void do_cmd_colors(void);
extern void do_cmd_note(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void do_cmd_room_desc(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void do_cmd_save_screen_html(void);
extern void do_cmd_knowledge(void);

/* cmd5.c */
extern int get_spell(int *sn, cptr prompt, object_type *o_ptr, bool known);
extern void do_cmd_browse_aux(const object_type *o_ptr);
extern void do_cmd_browse(void);
extern void do_cmd_study(void);
extern void do_cmd_cast_aux(int spell, int plev, cptr p, cptr t);
extern void do_cmd_cast(void);

/* cmd6.c */
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_activate(void);
extern void do_cmd_apply_rune(void);

/* dungeon.c */
extern bool dun_level_mon(int r_idx);
extern void play_game(bool new_game);

/* files.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr name);
extern void dump_html(void);
extern errr check_time(void);
extern errr check_time_init(void);
extern errr check_load(void);
extern errr check_load_init(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3);
extern void display_player(int mode);
extern errr file_character(cptr name, bool full);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(void);
extern long total_points(void);
extern void display_scores(int from, int to);
extern errr predict_score(void);
extern void close_game(void);
extern void exit_game_panic(void);
#ifdef HANDLE_SIGNALS
extern void (*(*signal_aux)(int, void (*)(int)))(int);
#endif
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);
extern void display_scores_aux(int from, int to, int note, high_score *score);

/* generate.c */
extern void generate_cave(void);

/* info.c */
extern bool spell_desc(const spell_type *s_ptr, const cptr intro, int level, bool detail, int target);
extern void spell_info(char *p, int spell, bool use_level);
extern bool list_object_flags(u32b f1, u32b f2, u32b f3, int mode);
extern void list_object(const object_type *o_ptr, int mode);
extern void screen_object(object_type *o_ptr, bool real);
extern void print_powers(const s16b *book, int num, int y, int x);
extern void print_spells(const s16b *book, int num, int y, int x);
extern bool make_fake_artifact(object_type *o_ptr, byte name1);
extern void display_koff(const object_type *o_ptr);
extern void object_can_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3);
extern void object_not_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3);
extern void object_may_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3);
extern void drop_may_flags(object_type *o_ptr);
extern void drop_all_flags(object_type *o_ptr);
extern void object_usage(int slot);
extern void object_guess_name(object_type *o_ptr);
extern void update_slot_flags(int slot, u32b f1, u32b f2, u32b f3);
extern void equip_can_flags(u32b f1,u32b f2,u32b f3);
extern void equip_not_flags(u32b f1,u32b f2,u32b f3);
extern void inven_drop_flags(object_type *o_ptr);

/* init2.c */
extern void init_file_paths(char *path);
extern void init_angband(void);
extern void cleanup_angband(void);

/* load.c */
extern bool load_player(void);

/* melee1.c */
extern bool make_attack_normal(int m_idx);
extern void mon_hit_trap(int m_idx, int y, int x);

/* melee2.c */
extern bool make_attack_spell_aux(int who, int y, int x, int thrown_spell);
extern bool make_attack_spell(int m_idx);
extern void process_monsters(byte minimum_energy);
extern int get_scent(int y, int x);
extern bool cave_exist_mon(int r_idx, int y, int x, bool occupied_ok);
extern void reset_monsters(void);

/* monster1.c */
extern void describe_monster(int r_idx, bool spoilers);
extern void screen_roff(int r_idx);
extern void display_roff(int r_idx);

/* monster2.c */
extern s16b poly_r_idx(int r_idx);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_m_list(void);
extern s16b m_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level);
extern void monster_desc(char *desc, const monster_type *m_ptr, int mode);
extern void lore_do_probe(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, object_type *j_ptr);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern s16b player_place(int y, int x);
extern void monster_hide(int y, int x, int mmove, monster_type *n_ptr);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern bool mon_resist_feat(int feat, int r_idx);
extern int place_monster_here(int y, int x, int r_idx);
extern bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp);
extern bool place_monster(int y, int x, bool slp, bool grp);
extern bool alloc_monster(int dis, bool slp);
extern bool summon_specific(int y1, int x1, int lev, int type);
extern bool summon_specific_one(int y1, int x1, int r_idx, bool slp);
extern bool animate_object(int item);
extern bool multiply_monster(int m_idx);
extern void message_pain(int m_idx, int dam);
extern void update_smart_learn(int m_idx, int what);

/* object1.c */
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_obvious_flags(object_type *o_ptr);
extern void object_flags_known(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void identify_random_gen(const object_type *o_ptr);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern s16b wield_slot(const object_type *o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_okay(const object_type *o_ptr);
extern sint scan_floor(int *items, int size, int y, int x, int mode);
extern void display_inven(void);
extern void display_equip(void);
extern void show_inven(void);
extern void show_equip(void);
extern void toggle_inven_equip(void);
extern bool get_item(int *cp, cptr pmt, cptr str, int mode);

/* object2.c */
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern errr get_obj_num_prep(void);
extern s16b get_obj_num(int level);
extern void object_known_store(object_type *o_ptr);
extern void object_known(object_type *o_ptr);
extern void object_bonus(object_type *o_ptr);
extern void object_mental(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great);
extern bool make_object(object_type *j_ptr, bool good, bool great);
extern bool make_chest(int *feat);
extern bool make_gold(object_type *j_ptr);
extern bool make_body(object_type *j_ptr, int r_idx);
extern bool make_head(object_type *j_ptr, int r_idx);
extern bool make_part(object_type *j_ptr, int r_idx);
extern bool make_skin(object_type *j_ptr, int r_idx);
extern bool make_feat(object_type *j_ptr, int feat);
extern void place_trapped_door(int y, int x);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern void race_near(int r_idx, int y1, int x1);
extern void drop_near(object_type *j_ptr, int chance, int y, int x);
extern void feat_near(int feat, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_object(int y, int x, bool good, bool great);
extern void place_gold(int y, int x);
extern errr get_feat_num_prep(void);
extern s16b get_feat_num(int level);
extern void place_feature(int y, int x);
extern void pick_trap(int y, int x);
extern void place_trap(int y, int x);
extern void pick_door(int y, int x);
extern void place_secret_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void place_locked_door(int y, int x);
extern void place_jammed_door(int y, int x);
extern void place_random_door(int y, int x);
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
extern void display_spell_list(void);
extern void fill_book(const object_type *o_ptr,s16b *book, int *num);
extern s16b spell_level(int spell);
extern s16b spell_power(int spell);
extern s16b spell_chance(int spell);
extern bool spell_okay(int spell, bool known);

/* save.c */
extern bool save_player(void);

/* spells1.c */
extern s16b poly_r_idx(int r_idx);
extern void teleport_away(int m_idx, int dis);
extern void teleport_player(int dis);
extern void teleport_player_to(int ny, int nx);
extern void teleport_player_level(void);
extern void take_hit(int dam, cptr kb_str);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, int permanent);
extern bool res_stat(int stat);
extern bool apply_disenchant(int mode);
extern bool project_f(int who, int rad, int y, int x, int dam, int typ);
extern bool project_m(int who, int rad, int y, int x, int dam, int typ);
extern bool project_p(int who, int rad, int y, int x, int dam, int typ);
extern bool project(int who, int rad, int y, int x, int dam, int typ, int flg);

/* spells2.c */
extern bool hp_player(int num);
extern void warding_glyph(void);
extern void warding_trap(int feat, int dir);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern void identify_pack(void);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool lose_all_info(void);
extern void set_recall(void);
extern bool detect_traps(void);
extern bool detect_doors(void);
extern bool detect_stairs(void);
extern bool detect_water(void);
extern bool detect_treasure(void);
extern bool detect_objects_gold(void);
extern bool detect_objects_normal(void);
extern bool detect_objects_magic(void);
extern bool detect_monsters_normal(void);
extern bool detect_objects_cursed(void);
extern bool detect_monsters_invis(void);
extern bool detect_monsters_evil(void);
extern bool detect_monsters_animal(void);
extern bool detect_monsters_undead(void);
extern bool detect_all(void);
extern void stair_creation(void);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool brand_item(int brand, cptr act);
extern bool ident_spell(void);
extern bool ident_spell_bonus(void);
extern bool ident_spell_rumor(void);
extern bool ident_spell_tval(int tval);
extern bool identify_fully(void);
extern bool recharge(int num);
extern void aggravate_monsters(int who);
extern bool banishment(void);
extern bool mass_banishment(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool fire_blast(int typ, int dir, int dam);
extern bool fire_touch(int typ, int dir, int dam);
extern bool trap_creation(void);
extern bool process_spell_flags(int spell, int level, bool *cancel);
extern bool process_spell_blows(int spell, int level, bool *cancel);
extern bool process_spell_types(int spell, int level, bool *cancel);
extern bool process_spell_eaten(int spell, int level, bool *cancel);
extern bool process_spell(int spell, int level, bool *cancel);

/* store.c */
extern void do_cmd_store(void);
extern void store_shuffle(int which);
extern void store_maint(int which);
extern void store_init(int which);

/* util.c */
extern errr path_parse(char *buf, int max, cptr file);
extern errr path_build(char *buf, int max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern FILE *my_fopen_temp(char *buf, int max);
extern errr my_fclose(FILE *fff);
extern errr my_fgets(FILE *fff, char *buf, size_t n);
extern errr my_fputs(FILE *fff, cptr buf, size_t n);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, long n);
extern errr fd_read(int fd, char *buf, size_t n);
extern errr fd_write(int fd, cptr buf, size_t n);
extern errr fd_close(int fd);
extern errr check_modification_date(int fd, cptr template_file);
extern void text_to_ascii(char *buf, int len, cptr str);
extern void ascii_to_text(char *buf, int len, cptr str);
extern sint macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern void flush(void);
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
extern void msg_print(cptr msg);
extern void msg_format(cptr fmt, ...);
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
extern bool askfor_aux(char *buf, int len);
extern bool get_string(cptr prompt, char *buf, int len);
extern s16b get_quantity(cptr prompt, int max);
extern bool get_check(cptr prompt);
extern bool get_com(cptr prompt, char *command);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern uint damroll(uint num, uint sides);
extern uint maxroll(uint num, uint sides);
extern bool is_a_vowel(int ch);
extern int color_char_to_attr(char c);
extern int color_text_to_attr(cptr name);
extern cptr attr_to_text(byte a);


#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

/* xtra1.c */
extern void cnv_stat(int val, char *out_val);
extern s16b modify_stat_value(int value, int amount);
#ifdef USE_CLASS_PRETTY_NAMES
extern void lookup_prettyname(char name[60], int style, int sval, bool long_name, bool short_name);
#endif
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);

/* xtra2.c */
extern bool set_blind(int v);
extern bool set_confused(int v);
extern bool set_poisoned(int v);
extern bool set_afraid(int v);
extern bool set_paralyzed(int v);
extern bool set_image(int v);
extern bool set_fast(int v);
extern bool set_slow(int v);
extern bool set_shield(int v);
extern bool set_blessed(int v);
extern bool set_hero(int v);
extern bool set_shero(int v);
extern bool set_protevil(int v);
extern bool set_invuln(int v);
extern bool set_tim_invis(int v);
extern bool set_tim_infra(int v);
extern bool set_oppose_acid(int v);
extern bool set_oppose_elec(int v);
extern bool set_oppose_fire(int v);
extern bool set_oppose_cold(int v);
extern bool set_oppose_pois(int v);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(int v);
extern bool set_rest(int v);
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern int get_food_type(const monster_race *r_ptr);
extern int get_coin_type(const monster_race *r_ptr);
extern void monster_death(int m_idx);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note);
extern bool modify_panel(int wy, int wx);
extern bool adjust_panel(int y, int x);
extern bool change_panel(int dir);
extern void verify_panel(void);
extern void display_room_info(int room);
extern void describe_room(void);
extern cptr look_mon_desc(int m_idx);
extern void ang_sort_aux(vptr u, vptr v, int p, int q);
extern void ang_sort(vptr u, vptr v, int n);
extern sint motion_dir(int y1, int x1, int y2, int x2);
extern sint target_dir(char ch);
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool target_set_interactive(int mode);
extern bool get_aim_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);
extern int min_depth(int dungeon);
extern int max_depth(int dungeon);
extern int town_depth(int dungeon);
extern void get_zone(dungeon_zone **zone_handle, int dungeon, int depth);


/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAVE_USLEEP
/* util.c */
extern int usleep(huge usecs);
# endif /* HAVE_USLEEP */
extern void user_name(char *buf, size_t len, int id);
#endif /* SET_UID */


#ifdef ALLOW_REPEAT
/* util.c */
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_clear(void);
extern void repeat_check(void);
#endif /* ALLOW_REPEAT */


#ifdef ALLOW_EASY_FLOOR
/* object1.c */
extern void show_floor(const int *floor_list, int floor_num);
#endif /* ALLOW_EASY_FLOOR */


#ifdef GJW_RANDART
/* randart.c */
extern errr do_randart(u32b randart_seed, bool full);
#endif /* GJW_RANDART */
