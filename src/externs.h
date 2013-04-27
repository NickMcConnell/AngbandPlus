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

/* tables.c */
extern const char hexsym[16];
extern const int adj_mag_study[];
extern const int adj_mag_mana[];
extern const byte adj_mag_fail[];
extern const int adj_mag_stat[];
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
extern const int adj_con_mhp[];
extern const byte blows_table[12][12];
extern const byte extract_energy[200];
extern const s32b player_exp[PY_MAX_LEVEL];
extern const byte chest_traps[64];
extern const char* const stat_names[A_MAX];
extern const char* const stat_names_reduced[A_MAX];
extern const char* const stat_names_full[A_MAX];
extern const char* const window_flag_desc[32];
extern const char* const inscrip_text[MAX_INSCRIP];

/* variable.c */
extern const char* const copyright;
extern u32b sf_xtra;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern bool arg_fiddle;
extern bool arg_wizard;
extern bool arg_sound;
extern int arg_graphics;
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
extern int use_graphics;
extern bool use_bigtile;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern bool opening_chest;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_nice;
extern bool repair_mflag_show;
extern bool repair_mflag_mark;
extern s16b o_max;
extern s16b o_cnt;
extern s16b mon_cnt;
extern byte feeling;
extern s16b rating;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char savefile[1024];
extern term* angband_term[ANGBAND_TERM_MAX];
extern char angband_term_name[ANGBAND_TERM_MAX][16];
extern byte angband_color_table[MAX_COLORS][4];
extern int temp_n;
extern coord* temp_g;
extern byte* temp_y;
extern byte* temp_x;
extern byte (*cave_info)[DUNGEON_WID];
extern byte (*cave_feat)[DUNGEON_WID];
extern s16b (*cave_o_idx)[DUNGEON_WID];
extern s16b (*cave_m_idx)[DUNGEON_WID];
extern byte (*cave_cost)[DUNGEON_WID];
extern byte (*cave_when)[DUNGEON_WID];
extern object_type *o_list;
extern monster_type *mon_list;
extern quest *q_list;
extern store_type *store;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern byte misc_to_attr[256];
extern char misc_to_char[256];
extern byte tval_to_attr[128];
extern const char* keymap_act[KEYMAP_MODES][256];
extern player_other *op_ptr;
extern player_type *p_ptr;
extern owner_type *b_info;
extern char *b_name;
extern char *b_text;
extern byte *g_info;
extern char *g_name;
extern char *g_text;
extern const char* ANGBAND_SYS;
extern const char* ANGBAND_GRAF;
extern const char* ANGBAND_DIR;
extern const char* ANGBAND_DIR_APEX;
extern const char* ANGBAND_DIR_BONE;
extern const char* ANGBAND_DIR_DATA;
extern const char* ANGBAND_DIR_EDIT;
extern const char* ANGBAND_DIR_FILE;
extern const char* ANGBAND_DIR_HELP;
extern const char* ANGBAND_DIR_INFO;
extern const char* ANGBAND_DIR_SAVE;
extern const char* ANGBAND_DIR_PREF;
extern const char* ANGBAND_DIR_USER;
extern const char* ANGBAND_DIR_XTRA;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook)(const object_type*);
extern bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);
extern void (*ang_sort_swap)(void *u, void *v, int a, int b);
extern void (*object_info_out_flags)(const object_type *o_ptr, u32b* f);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, const char* const str);
extern int text_out_wrap;
extern int text_out_indent;
extern bool use_transparency;


/*
 * Automatically generated "function declarations"
 */

/* attack.c */
extern bool test_hit(int chance, int ac, bool vis);
extern void test_hit_ratio(int chance, int ac, bool vis, s32b& numerator, s32b& denominator);
extern int critical_shot(int weight, int plus, int dam);
extern int critical_norm(int weight, int plus, int dam);
extern int tot_dam_aux(const object_type *o_ptr, int tdam, const monster_type *m_ptr);
extern void py_attack(coord g);
extern bool missile_test_hit(int chance, int ac, int vis, int distance, coord loc);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);


/* birth.c */
extern void player_birth(void);

/* cave.c */
inline int distance(int y1, int x1, int y2, int x2) {return V_distance(x1,y1,x2,y2);}	/* adapter */
extern bool los(coord g1, coord g2);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern bool feat_supports_lighting(int feat);
extern void move_cursor_relative(coord g);
extern void print_rel(char c, byte a, coord g);
extern void note_spot(coord g);
extern void lite_spot(coord g);
extern void prt_map(void);
extern void display_map(int *cy, int *cx);
extern void do_cmd_view_map(void);
extern errr vinfo_init(void);
extern void forget_view(void);
extern void update_view(void);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void cave_set_feat(int y, int x, int feat);
extern bool wall_stop(coord g);
extern bool wall_mon_stop(coord g);
extern bool projectable(coord g1, coord g2);
extern void scatter(coord& g, coord g2, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void disturb(int stop_search, int unused_flag);
extern bool is_quest(int level);

/* cmd1.c */
extern void search(void);
extern void py_pickup(int pickup);
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
extern void do_cmd_spike(void);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void do_cmd_hold(void);
extern void do_cmd_stay(void);
extern void do_cmd_rest(void);

/* cmd3.c */
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern void wield_item(object_type *o_ptr, int item);
extern void do_cmd_destroy(void);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern bool ang_sort_comp_hook(const void *u, const void *v, int a, int b);
extern void ang_sort_swap_hook(void *u, void *v, int a, int b);

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
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void do_cmd_knowledge(void);

/* cmd5.c */
extern void do_cmd_browse_aux(const object_type *o_ptr);
int get_spell(const object_type *o_ptr, const char* const prompt, bool known);
extern int spell_choose_new(const object_type *o_ptr);
extern void spell_learn(int spell);
extern bool spell_cast(int spell);

/* cmd6.c */
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_activate(void);

/* dungeon.c */
extern void apply_noise(coord src,int intensity);
#ifdef ZAIBAND_NEW_COMMAND_LOOP
extern void play_game(void);
#else
extern void play_game(bool new_game);
#endif

/* files.c */
extern void html_screenshot(const char* name);
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(const char* name);
extern errr check_time(void);
extern errr check_time_init(void);
extern void display_player(int mode);
extern errr file_character(const char* name, bool full);
extern bool show_file(const char* name, const char* what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(void);
extern long total_points(void);
extern void close_game(void);
extern void exit_game_panic(void);

/* generate.c */
extern void generate_cave(void);

/* init2.c */
extern void init_file_paths(char *path);
extern void create_user_dirs(void);
#ifdef ZAIBAND_NEW_COMMAND_LOOP
extern bool init_angband(void);
#else
extern void init_angband(void);
#endif
extern void cleanup_angband(void);

/* load.c */
extern bool load_player(void);

/* melee1.c */
extern int armor_damage_reduction(int damage,int ac);
extern bool make_attack_normal(const m_idx_type m_idx);
extern bool make_attack_ranged_physical(const m_idx_type m_idx);

/* melee2.c */
extern void process_monsters(byte minimum_energy);

/* monster1.c */
extern int race_gender_index(const monster_race& r);
extern void describe_monster(int r_idx, bool spoilers);
extern void roff_top(int r_idx);
extern void screen_roff(int r_idx);
extern void display_roff(int r_idx);

/* monster2.c */
extern void delete_monster_idx(const m_idx_type i);
extern void delete_monster(coord g);
extern void compact_monsters(int size);
extern void wipe_mon_list(void);
extern errr get_mon_num_prep(int_test* get_mon_num_hook);
extern s16b get_mon_num(int level);
extern void display_monlist(void);
extern void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode);
extern void lore_do_probe(const m_idx_type m_idx);
extern void lore_treasure(const m_idx_type m_idx, int num_item, int num_gold);
extern void update_mon(const m_idx_type m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(const m_idx_type m_idx, object_type *j_ptr);
extern void monster_swap(coord g1, coord g2);
extern s16b player_place(int y, int x);
extern s16b monster_place(coord g, monster_type *n_ptr);
extern bool place_monster_aux(coord g, int r_idx, bool slp, bool grp);
extern bool place_monster(coord g, bool slp, bool grp);
extern bool alloc_monster(int dis, bool slp);
extern bool summon_specific(coord g, int lev, int type);
extern bool multiply_monster(const m_idx_type m_idx);
extern void message_pain(const m_idx_type, int dam);
extern bool monster_has_attack(const monster_race* const r_ptr, byte this_method);


/* obj-info.c */
extern bool obj_has_activation(const object_type *o_ptr);
extern bool object_info_out(const object_type *o_ptr);
extern void object_info_screen(const object_type *o_ptr);

/* object1.c */
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern void object_flags(const object_type *o_ptr, u32b* f);
extern void object_flags_known(const object_type *o_ptr, u32b* f);
extern void object_desc(char *buf, size_t max, const object_type *o_ptr, bool pref, object_desc_mode mode);
extern void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, bool pref, object_desc_mode mode);
extern void describe_item_activation(const object_type *o_ptr);
extern void identify_random_gen(const object_type *o_ptr);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern s16b wield_slot(const object_type *o_ptr);
extern const char* mention_use(int i);
extern const char* describe_use(int i);
extern bool item_tester_okay(const object_type *o_ptr);
extern int scan_floor(int *items, int size, coord g, int mode);
extern void display_inven(void);
extern void display_equip(void);
extern void show_inven(void);
extern void show_equip(void);
extern void show_floor(const int *floor_list, int floor_num);
extern bool get_item(int *cp, const char* pmt, const char* str, int mode);

/* object2.c */
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern object_type* get_first_object(int y, int x);
extern object_type* get_next_object(const object_type *o_ptr);
extern s16b get_obj_num(int level);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern bool is_blessed(const object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(tvalsval obj_id);
extern s16b lookup_kind2(byte tval, byte sval);
extern void object_prep(object_type *o_ptr, int k_idx);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great);
extern bool make_object(object_type *j_ptr, bool good, bool great);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern void drop_near(object_type *j_ptr, int chance, coord t);
extern void acquirement(coord t, int num, bool great);
extern void place_object(int y, int x, bool good, bool great);
extern void place_gold(int y, int x);
extern void place_secret_door(int y, int x);
extern void place_closed_door(int y, int x);
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
extern s16b spell_chance(int spell);
extern bool spell_okay(int spell, bool known);
extern void print_spells(const byte *spells, int num, int y, int x);
extern void display_koff(int k_idx);
extern bool is_moronic_to_use(const object_type& o);

/* save.c */
extern bool save_player(void);

/* signals.c */
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);

/* spells1.c */
extern s16b poly_r_idx(int r_idx);
extern void teleport_away(const m_idx_type m_idx, int dis);
extern void teleport_player(int dis);
extern void teleport_player_to(coord g);
extern void teleport_player_level(void);
extern void take_hit(int dam, const char* kb_str);
extern bool set_acid_destroy(const object_type *o_ptr);
extern bool set_elec_destroy(const object_type *o_ptr);
extern bool set_fire_destroy(const object_type *o_ptr);
extern bool set_cold_destroy(const object_type *o_ptr);
extern void acid_dam(int dam, const char* kb_str);
extern void elec_dam(int dam, const char* kb_str);
extern void fire_dam(int dam, const char* kb_str);
extern void cold_dam(int dam, const char* kb_str);
extern bool inc_stat(stat_index stat);
extern bool dec_stat(stat_index stat, int amount, bool permanent);
extern bool res_stat(stat_index stat);
extern bool apply_disenchant(int mode);

/* spells2.c */
extern bool hp_player(int num);
extern void warding_glyph(void);
extern bool do_dec_stat(stat_index stat);
extern bool do_res_stat(stat_index stat);
extern bool do_inc_stat(stat_index stat);
extern void identify_pack(void);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool lose_all_info(void);
extern void set_recall(void);
extern bool detect_on_panel(coord_action* test,const char* detect_text = NULL);
extern bool detect_traps(void);
extern bool detect_doors(void);
extern bool detect_stairs(void);
extern bool detect_treasure(void);
extern bool object_scan(object_action* test,const char* detect_text = NULL);
extern bool detect_objects_gold(void);
extern bool detect_objects_normal(void);
extern bool detect_objects_magic(void);
extern bool monster_scan(monster_action* test,const char* detect_text = NULL);
extern bool detect_monsters_normal(void);
extern bool detect_monsters_invis(void);
extern bool detect_monsters_evil(void);
extern bool detect_all(void);
extern void stair_creation(void);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool recharge(int num);
extern bool speed_monsters(void);
extern bool slow_monsters(void);
extern bool sleep_monsters(void);
extern bool banish_evil(int dist);
extern bool turn_undead(void);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_monsters(int dam);
extern void aggravate_monsters(int who);
extern bool banishment(void);
extern bool mass_banishment(void);
extern bool probing(void);
extern void destroy_area(coord g, int r, bool full);
extern void earthquake(coord g, int r);
extern void lite_room(coord g);
extern void unlite_room(coord g);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool extrapolate_target(coord& g, int dir);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_swarm(int num, int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool project_los(int typ, int dam);
extern bool lite_line(int dir);
extern bool strong_lite_line(int dir);
extern bool drain_life(int dir, int dam);
extern bool wall_to_mud(int dir);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool heal_monster(int dir);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir);
extern bool sleep_monster(int dir);
extern bool confuse_monster(int dir, int plev);
extern bool poly_monster(int dir);
extern bool clone_monster(int dir);
extern bool fear_monster(int dir, int plev);
extern bool teleport_monster(int dir);
extern bool door_creation(void);
extern bool trap_creation(void);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern void brand_object(object_type *o_ptr, byte brand_type);
extern void brand_weapon(void);
extern bool brand_ammo(void);
extern bool brand_bolts(void);
extern void ring_of_power(int dir);

/* store.c */
extern void do_cmd_store(void);			/* leave here as main-mac.c wants it */

/* trap.c */
extern void pick_trap(coord g);
extern void place_trap(int y, int x);
extern void hit_trap(coord g);

/* z-type.c */ 
extern void display_panel(const data_panel *panel, int count, bool left_adj, const region *bounds);

/* util.c */
extern void text_to_ascii(char *buf, size_t len, const char* str);
extern void ascii_to_text(char *buf, size_t len, const char* str);
extern void flush(void);
extern void flush_fail(void);
extern char inkey(void);
extern ui_event_data inkey_ex(void);
extern void bell(const char* reason);
extern void sound(int val);
extern void move_cursor(int row, int col);
extern void msg_print(const char* msg);
extern void msg_format(const char* fmt, ...);
extern void message(u16b message_type, s16b extra, const char* message);
extern void message_format(u16b message_type, s16b extra, const char* fmt, ...);
extern bool check_for_inscrip(const object_type *o_ptr, const char *inscrip);
extern void message_flush(void);
extern void screen_save(void);
extern void screen_load(void);
extern void c_prt(byte attr, const char* str, int row, int col);
extern void prt(const char* str, int row, int col);
extern void text_out_to_file(byte attr, const char* str);
extern void text_out_to_screen(byte a, const char* str);
extern void text_out(const char *fmt, ...);
extern void text_out_c(byte a, const char *fmt, ...);
extern void text_out_e(const char *fmt, ...);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, size_t len);
extern bool get_string(const char* prompt, char *buf, size_t len);
extern s16b get_quantity(const char* prompt, int max);
extern bool get_check(const char* prompt);
extern bool get_com(const char* prompt, char *command);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern int color_char_to_attr(char c);
extern int color_text_to_attr(const char* name);
extern const char* attr_to_text(byte a);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

/* inline int maxroll(int num, int sides) {return num*sides;} */
inline bool is_a_vowel(char ch) {return strchr("aeiouAEIOU",ch);}

/* x-spell.c */
extern int get_spell_index(const object_type *o_ptr, int index);
extern const char* get_spell_name(int tval, int index);
extern const char* get_spell_info(int tval, int index);
extern bool cast_spell(int tval, int index);

/* xtra1.c */
extern s16b modify_stat_value(int value, int amount);
extern void player_missile_shot_analysis(const object_type& o, s16b str, s16b& num_fire, byte& ammo_tval, byte& ammo_mult);
extern void player_melee_blow_analysis(const object_type& o, s16b str, s16b dex, s16b& num_blow);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void handle_stuff(void);

/* xtra2.c */
extern unsigned int stun_level(int v);
extern unsigned int cut_level(int v);
extern unsigned int food_level(int v);
extern bool set_food(int v);
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern void monster_death(const m_idx_type m_idx);
extern bool mon_take_hit(const m_idx_type m_idx, int dam, bool *fear, const char* note);
extern bool modify_panel(term *t, int wy, int wx);
extern void ang_sort_aux(void *u, void *v, int p, int q);
extern void ang_sort(void *u, void *v, int n);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern int target_dir(char ch);
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool target_set_interactive(int mode);
extern bool get_aim_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);

/* xtra3.c */
extern void cnv_stat(int val, char *out_val, size_t out_len);
extern bool adjust_panel(coord g);
extern bool change_panel(int dir);
extern void toggle_inven_equip(void);
extern void subwindows_set_flags(u32b *new_flags, size_t n_subwindows);
extern void init_display(void);

/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAVE_USLEEP
/* util.c */
extern int usleep(unsigned long usecs);
# endif /* HAVE_USLEEP */
#endif /* SET_UID */


/* util.c */
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_clear(void);
extern void repeat_check(void);


/* randart.c */
extern errr do_randart(u32b randart_seed, bool full);

#ifdef RISCOS
/* main-ros.c */
extern char *riscosify_name(const char* path);
#endif /* RISCOS */

#if defined(MAC_MPW) || defined(MACH_O_CARBON)
/* main-mac.c, or its derivatives */
extern u32b _fcreator;
extern u32b _ftype;
# if defined(MAC_MPW) && defined(CARBON)
extern void convert_pathname(char *path);
# endif
# if defined(MACH_O_CARBON)
extern void fsetfileinfo(const char* path, u32b fcreator, u32b ftype);
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


/* wizard1.c */
#ifdef ALLOW_SPOILERS

extern void do_cmd_spoilers(void);

#endif /* ALLOW_SPOILERS */

extern bool make_fake_artifact(object_type *o_ptr, byte name1);

/*
 * Determine if a "legal" grid is within "los" of the player
 */
inline bool player_has_los_bold(byte Y, byte X) {return cave_info[Y][X] & (CAVE_VIEW);}

/*
 * Determine if a "legal" grid can be "seen" by the player
 */
inline bool player_can_see_bold(byte Y, byte X) {return cave_info[Y][X] & (CAVE_SEEN);}

/*
 * Determines if a map location is currently "on screen"
 * Note that "panel_contains(Y,X)" always implies "in_bounds(Y,X)".
 * Pre-storing this into a cave_info flag would be nice.  XXX XXX
 */
inline bool
panel_contains(byte Y,byte X)
{return ((Y - Term->offset_y) < (byte)(SCREEN_HGT)) && ((X - Term->offset_x) < (byte)(SCREEN_WID));}

inline bool
panel_contains(coord X)
{return panel_contains(X.y,X.x);}

/*
 *	Apply damage reduction factor
 */
template<int N>
inline int DR(int dam)
{	return (dam+(N-1))/N;	}
