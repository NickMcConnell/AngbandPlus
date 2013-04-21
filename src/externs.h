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
 * (z-virt.h, z-util.h, z-form.h, term.h, random.h)
 */


/*
 * Automatically generated "variable" declarations
 */

/* tables.c */
extern s16b ddd[9];
extern s16b ddx[10];
extern s16b ddy[10];
extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];
extern char hexsym[16];
extern byte adj_mag_study[];
extern byte adj_mag_mana[];
extern byte adj_mag_fail[];
extern byte adj_mag_stat[];
extern byte adj_chr_gold[];
extern byte adj_chr_pet_summon[];
extern byte adj_int_dev[];
extern byte adj_wis_sav[];
extern byte adj_dex_dis[];
extern byte adj_int_dis[];
extern byte adj_dex_ta[];
extern byte adj_str_td[];
extern byte adj_dex_th[];
extern byte adj_str_th[];
extern byte adj_str_wgt[];
extern byte adj_str_hold[];
extern byte adj_str_dig[];
extern byte adj_str_blow[];
extern byte adj_dex_blow[];
extern byte adj_dex_safe[];
extern byte adj_con_fix[];
extern byte adj_con_mhp[];
extern byte blows_table[12][12];
extern owner_type owners[MAX_STORES][MAX_OWNERS];
extern byte extract_energy[200];
extern s32b player_exp[PY_MAX_LEVEL];
extern player_sex sex_info[MAX_SEXES];
extern player_race race_info[MAX_RACES];
extern player_class class_info[MAX_CLASS];
extern cptr ingr_short_names[16];
extern recipe recipe_info[MAX_RECIPES];
extern cptr deity_niceness[10];
extern cptr deity_standing[11];
extern cptr deity_rarity[5];
extern cptr deity_affiliation[29];
extern deity deity_info[MAX_GODS];
extern shape shape_info[MAX_SHAPES];
extern cli_comm cli_info[MAX_COMMANDS];
extern cptr mutation_names[MAX_MUTS][3];
extern material materials[STUFF_MAX];
extern byte chest_traps[64];
extern cptr player_title[MAX_CLASS][PY_MAX_LEVEL / 5];
extern cptr color_names[16];
extern cptr stat_names[6];
extern cptr stat_names_reduced[6];
extern cptr window_flag_desc[32];
extern cptr option_text[OPT_MAX];
extern cptr option_desc[OPT_MAX];
extern bool option_norm[OPT_MAX];
extern byte option_page[5][22];

/* variable.c */
extern cptr copyright[5];
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
extern bool character_xtra;
extern u32b seed_flavor;
extern u32b seed_town;
extern u32b seed_dungeon;
extern u32b seed_wild;
extern s16b arena_monsters[MAX_ARENAS][MAX_ARENA_MONS];	/* -KMW- */
extern s16b num_repro;
extern s16b object_level;
extern s16b monster_level;
extern s32b turn;
extern s32b old_turn;
extern s32b old_resting_turn;
extern bool use_sound;
extern bool use_graphics;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern s16b coin_type;
extern bool opening_chest;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_born;
extern bool repair_mflag_nice;
extern bool repair_mflag_show;
extern bool repair_mflag_mark;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern s16b m_pet_num;
extern s16b m_generators;
extern s16b screen_x;
extern s16b screen_y;
extern s16b SCREEN_HGT;
extern s16b SCREEN_WID;
extern s16b feeling;
extern s16b rating;
extern s16b pet_rating;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char savefile[1024];
extern s16b lite_n;
extern byte lite_y[LITE_MAX];
extern byte lite_x[LITE_MAX];
extern s16b view_n;
extern byte view_y[VIEW_MAX];
extern byte view_x[VIEW_MAX];
extern s16b temp_n;
extern byte temp_y[TEMP_MAX];
extern byte temp_x[TEMP_MAX];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern bool *macro__cmd;
extern s16b quark__num;
extern cptr *quark__str;
extern u16b message__next;
extern u16b message__last;
extern u16b message__head;
extern u16b message__tail;
extern u16b *message__ptr;
extern byte *message__pty;
extern char *message__buf;
extern term *angband_term[8];
extern char angband_term_name[8][16];
extern byte angband_color_table[256][4];
extern char angband_sound_name[SOUND_MAX][16];
extern object_type *o_list;
extern monster_type m_list[MAX_M_IDX];
extern generator gen_list[MAX_GENERATORS];
extern byte recipe_recall[MAX_RECIPES];
extern byte quest_status[MAX_QUESTS];
extern vault_type *q_v_ptrs[MAX_QUESTS];
extern byte max_quests;
extern byte rewards[MAX_REWARDS];
extern s16b bounties[MAX_BOUNTIES][2];
extern spell spells[MAX_SPELLS];
extern spell powers[MAX_POWERS];
extern spell activations[MAX_ACTIVATIONS];
extern u16b spell_num;
extern u16b power_num;
extern u16b activation_num;
extern random_artifact random_artifacts[MAX_RANDARTS];
extern store_type *store;
extern object_type *inventory;
extern object_type *equipment[EQUIP_MAX];
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern byte misc_to_attr[128];
extern char misc_to_char[128];
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
extern player_sex *sp_ptr;
extern player_race *rp_ptr;
extern player_class *cp_ptr;
extern player_other *op_ptr;
extern player_type *p_ptr;
extern header *v_head;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern char *q_text;
extern char *vm_text;
extern header *f_head;
extern feature_type *f_info;
extern char *f_name;
extern char *f_text;
extern header *k_head;
extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern header *a_head;
extern artifact_type *a_info;
extern char *a_name;
extern char *a_text;
extern header *e_head;
extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern header *r_head;
extern monster_race *r_info;
extern char *r_name;
extern char *r_text;
extern char *sayings_text;
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
extern cptr ANGBAND_DIR_USER;
extern cptr ANGBAND_DIR_XTRA;
extern cptr ANGBAND_DIR_LUA;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool(*item_tester_hook) (object_type *);
extern bool item_tester_automatic;
extern bool(*ang_sort_comp) (vptr u, vptr v, int a, int b);
extern void (*ang_sort_swap) (vptr u, vptr v, int a, int b);
extern bool(*get_mon_num_hook) (int r_idx);
extern bool(*get_obj_num_hook) (int k_idx);
extern bool angband_keymap_flag;
extern bool hack_punish_theft;
extern byte object_desc_mode;
extern bool store_combine_flag;

extern byte cave_cost[DUNGEON_HGT][DUNGEON_WID];
extern byte cave_when[DUNGEON_HGT][DUNGEON_WID];
extern byte cave_info[DUNGEON_HGT][DUNGEON_WID];
extern byte cave_feat[DUNGEON_HGT][DUNGEON_WID];
extern object_type *cave_o_idx[DUNGEON_HGT][DUNGEON_WID];
extern s16b cave_m_idx[DUNGEON_HGT][DUNGEON_WID];


/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool player_can_see_bold(int y, int x);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern void map_info(int y, int x, byte * ap, char *cp);
extern void move_cursor_relative(int y, int x);
extern void print_rel(char c, byte a, int y, int x);
extern void note_spot(int y, int x);
extern void lite_spot(int y, int x);
extern void prt_map(void);
extern void display_map(int scale);
extern void do_cmd_view_map(void);
extern void forget_lite(void);
extern void update_lite(void);
extern void forget_view(void);
extern void update_view(void);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void cave_set_feat(int y, int x, int feat);
extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);
extern bool projectable(int y1, int x1, int y2, int x2);
extern bool target_clear(monster_type * m_ptr, int x2, int y2);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void disturb(int stop_search, int unused_flag);

/* cmd1.c */
extern bool test_hit_fire(int chance, int ac, int vis);
extern bool test_hit_norm(int chance, int ac, int vis);
extern s16b critical_shot(int weight, int plus, int dam);
extern s16b critical_norm(int weight, int plus, int dam, int wtval,
	monster_type * m_ptr); /* -KMW- */
extern s16b tot_dam_aux(object_type * o_ptr, int tdam,
	monster_type * m_ptr);
extern void search(void);
extern void py_pickup(int pickup);
extern void mon_hit_trap(int m_idx, int y, int x);
extern void hit_trap(int y, int x);
extern void py_attack(int y, int x);
extern void move_player(int dir, int do_pickup);
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
extern bool do_cmd_walk_test(int y, int x);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void check_store_entering(s16b py, s16b px);
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
extern void do_cmd_observe(void);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern bool research_mon(void);

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
extern void do_cmd_time(void);

/* cmd5.c */
extern void brand_ammo(int brand_type, int bolts_only);	/* - KMW- */
extern void brand_weapon(void);
extern void do_cmd_browse(void);
extern void do_cmd_study(void);	/* -KMW- */
extern void do_cmd_cast(void);
extern void do_cmd_pray(void);

extern bool cause_spell_effect(spell * s_ptr);
extern void spell_generate_new(int plev);
extern int spell_chance(spell * rspell);
extern spell *select_spell(bool quick);
extern void do_cmd_cast_power(void);

extern void generate_mutation(void);
extern void remove_mutation(void);

extern void add_powers(byte class);
extern void remove_powers(byte class);

/* cmd6.c */
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern void show_book_number(int num);

extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_activate(void);

extern void do_cmd_brew_stuff(void);
extern void do_cmd_sacrifice(void);

extern void do_cmd_cli(void);
extern void do_cmd_gain_helper(void);
extern void do_cmd_cast_helper(void);
extern void do_cmd_save_quit(void);
extern void do_cmd_command_help(void);
extern void do_cmd_unwalk(void);
extern void do_cmd_immovable_special(void);
extern void do_cmd_change_shape(void);
extern void do_cmd_pray(void);

extern object_type *item_effect(cptr, cptr, bool, bool, int,
	bool(hook) (object_type *), s16b);

/* dungeon.c */
extern void play_game(bool new_game);

/* files.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_aux(char *buf);
extern errr process_pref_file(cptr name);
extern errr check_time(void);
extern errr check_time_init(void);
extern errr check_load(void);
extern errr check_load_init(void);
extern void display_player(int mode);
extern errr file_character(cptr name);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(void);
extern long total_points(void);
extern void display_scores(int from, int to);
extern void open_highscore(void);
extern void rewind_highscore(void);
extern void close_highscore(void);
extern bool next_highscore(score_info * s_info);
extern void close_game(void);
extern void exit_game_panic(void);
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);

extern void init_cache(void);

extern cptr get_line(char *fname, int line);
extern cptr get_random_line(char *fname);
extern s16b get_num_lines(char *fname);

extern cptr get_line_old(char *fname, int line);
extern cptr get_random_line_old(char *fname);
extern s16b get_num_lines_old(char *fname);

/* generate.c */
extern void generate_cave(void);
extern void wilderness_gen(void); /* -KMW- */

/* init1.c */
extern errr init_v_info_txt(FILE * fp, char *buf);
extern errr init_f_info_txt(FILE * fp, char *buf);
extern errr init_k_info_txt(FILE * fp, char *buf);
extern errr init_a_info_txt(FILE * fp, char *buf);
extern errr init_e_info_txt(FILE * fp, char *buf);
extern errr init_r_info_txt(FILE * fp, char *buf);
extern s16b init_s_info_txt(byte spell_book, spell * array, int max);

/* init2.c */
extern void init_file_paths(char *path);
extern void init_angband(void);

/* load.c */
extern errr rd_savefile_new(void);

/* melee1.c */
extern cptr get_monster_saying(monster_race * r_ptr);
extern bool make_attack_normal(int m_idx);

/* melee2.c */
extern bool make_attack_spell(int m_idx);
extern void process_monsters(void);

/* monster1.c */
extern void screen_roff(int r_idx, int remember);
extern void display_roff(int r_idx);

/* monster2.c */
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_m_list(void);
extern s16b m_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level);
extern void monster_desc(char *desc, monster_type * m_ptr, int mode);
extern void lore_do_probe(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern s16b player_place(int y, int x);
extern s16b monster_place(int y, int x, monster_type * n_ptr);
extern bool place_monster_aux(int y, int x, int r_idx, int flags);
extern bool place_monster(int y, int x, int flags);
extern bool alloc_monster(int dis, int flags);
extern bool summon_specific(int y1, int x1, int lev, int type);
extern bool summon_specific_friendly(int y1, int x1, int lev, int type);
extern bool summon_avatar(int y1, int x1, int lev, byte god);
extern bool multiply_monster(int m_idx);
extern void message_pain(int m_idx, int dam);
extern void update_smart_learn(int m_idx, int what);
extern bool monster_saves(int mlev, int plev); /* -KMW- */
extern bool sacred_monster(monster_race * r_ptr);
extern bool despised_monster(monster_race * r_ptr);
extern bool aligned_monster(monster_race * r_ptr, byte god);
extern bool opposed_monster(monster_race * r_ptr, byte god);


/* object1.c */
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern void object_flags(object_type * o_ptr, u32b * f1, u32b * f2,
	u32b * f3);
extern void object_flags_known(object_type * o_ptr, u32b * f1, u32b * f2,
	u32b * f3);
extern void object_desc(char *buf, object_type * o_ptr, int pref,
	int mode);
extern void object_desc_store(char *buf, object_type * o_ptr, int pref,
	int mode);
extern bool item_activation(object_type * o_ptr, char *bugg);
extern cptr damage_status(object_type * o_ptr);
extern void identify_fully_aux(object_type * o_ptr);
extern s16b wield_slot(object_type * o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_okay(object_type * o_ptr);
extern void show_stack(object_type * stack, bool glob);
extern void show_equip(void);
extern s16b size_stack(object_type * stack, bool glob);
extern object_type *get_item(cptr pmt, cptr str, int y, int x, int mode);

/* object2.c */
extern void next_tag(object_type * o_ptr);
extern void remove_tag(object_type * o_ptr);
extern void remove_from_stack(object_type * o_ptr);
extern void remove_from_global_list(object_type * o_ptr,
	object_type ** stack);
extern object_type *object_unabsorb(object_type * o_ptr, int num);
extern void remove_object(object_type * o_ptr);
extern void wipe_o_list(void);
extern void init_tval_order(void);
extern bool insert_to_stack(object_type * o_ptr, object_type ** stack);
extern void insert_to_global_list(object_type * o_ptr,
	object_type ** stack, byte world);
extern object_type *new_object(void);
extern bool floor_carry(int y, int x, object_type * o_ptr);
extern bool inven_carry(object_type * o_ptr);
extern bool monster_inven_carry(monster_type * m_ptr, object_type * o_ptr);
extern errr get_obj_num_prep(void);
extern s16b get_obj_num(int level);
extern void object_known(object_type * o_ptr);
extern void object_aware(object_type * o_ptr);
extern void object_tried(object_type * o_ptr);
extern byte transmute_old_stuff_aux(object_type * o_ptr);
extern s32b object_value(object_type * o_ptr);
extern bool object_similar(object_type * o_ptr, object_type * j_ptr);
extern bool tval_can_stack(s16b tval);
extern void object_absorb(object_type * o_ptr, object_type * j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_prep(object_type * o_ptr, int k_idx);
extern bool make_artifact_named(object_type * o_ptr, s16b i, byte depth,
	bool sure);
extern bool make_ego_item_named(object_type * o_ptr, s16b i, byte depth,
	bool spec);
extern bool make_artifact(object_type * o_ptr, byte depth);
extern bool make_ego_item(object_type * o_ptr, byte depth);
extern void apply_magic(object_type * o_ptr, int lev, bool okay, bool good,
	bool great);
extern bool make_object(object_type * j_ptr, bool good, bool great);
extern void drop_near(object_type * j_ptr, bool do_dam, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_object(int y, int x, bool good, bool great);
extern void pick_trap(int y, int x);
extern void place_trap(int y, int x);
extern void combine_pack(void);
extern void reorder_pack(void);
extern void create_generator(s16b r_idx, s16b y, s16b x);
extern void activate_generators(void);
extern void process_generators(void);
extern bool explode_object(object_type * o_ptr, int y, int x);
extern bool object_take_hit(object_type * o_ptr, s16b dam, cptr verb);
extern bool repair_object(object_type * o_ptr, s16b dam);
extern bool transmute(object_type * o_ptr, byte stuff);
extern bool transmute_random(object_type * o_ptr, int lev);
extern int make_stack_pages(object_type * stack,
	object_type * pages[MAX_STACK_PAGES], int per_page, bool glob);
extern void show_stack_page(object_type * pages[MAX_STACK_PAGES],
	int page_cnt, int page_cur, int per_page, bool glob, int row);
extern void link_insert(object_type *a, object_type *b, object_type *c);
extern void link_remove(object_type *a);
extern void link_insert_glob(object_type *a, object_type *b, object_type *c);
extern void link_remove_glob(object_type *a);

/* save.c */
extern bool save_player(void);
extern bool load_player(void);
extern bool load_dungeon(s16b tag);
extern bool save_dungeon(s16b tag);

/* spells1.c */
extern s16b poly_r_idx(int r_idx);
extern void teleport_away(int m_idx, int dis);
extern void teleport_away_to(int m_idx, int y, int x);
extern void teleport_player(int dis);
extern void teleport_player_directed(int rad, int dir);
extern void teleport_player_to(int ny, int nx);
extern void teleport_player_level(void);
extern void take_hit(int dam, cptr kb_str);
extern void take_sanity_hit(int dam, cptr kb_str);

extern bool hates_acid(object_type * o_ptr);
extern bool hates_elec(object_type * o_ptr);
extern bool hates_fire(object_type * o_ptr);
extern bool hates_cold(object_type * o_ptr);
extern bool hates_plasma(object_type * o_ptr);
extern bool hates_meteor(object_type * o_ptr);
extern bool hates_shards(object_type * o_ptr);
extern bool hates_sound(object_type * o_ptr);
extern bool hates_impact(object_type * o_ptr);

extern void inven_damage(bool (*func)(object_type*), int dam, cptr verb);
extern void acid_dam(int dam, cptr kb_str);
extern void elec_dam(int dam, cptr kb_str);
extern void fire_dam(int dam, cptr kb_str);
extern void cold_dam(int dam, cptr kb_str);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, int permanent);
extern bool res_stat(int stat);
extern bool apply_disenchant(int mode);
extern bool project(int who, int rad, int y, int x, int dam, int typ,
	u32b flg);

/* spells2.c */
extern bool hp_player(int num);
extern bool heal_insanity(int val);
extern void warding_glyph(void);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern void identify_pack(void);
extern bool uncurse_item(object_type * o_ptr, bool full);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool lose_all_info(void);
extern void stair_creation(void);
extern bool enchant(object_type * o_ptr, int n, int eflag);
extern bool enchant_spell2(int num, int flags);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool repair_spell(int dam);
extern bool identify_fully(void);
extern bool recharge(int num);
extern bool recharge_item(int num, object_type * o_ptr);
extern bool transmute_spell(bool full);
extern bool speed_monsters(void);
extern bool slow_monsters(void);
extern bool sleep_monsters(void);
extern bool banish_evil(int dist);
extern bool turn_undead(void);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_monsters(int dam);
extern void aggravate_monsters(int who);
extern void awake_monsters(int who);
extern void hostile_monsters(int who);
extern void call_pets_toggle(void);
extern int call_pet_duration(void);
extern bool genocide(void);
extern bool mass_genocide(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void chaos_destroy_area(int y1, int x1, int r);
extern void earthquake(void);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool fire_explosion(int y, int x, int typ, int rad, int dam);
extern bool fire_godly_wrath(int y, int x, int typ, int rad, int dam,
	byte god);
extern bool fire_at_player(int typ, int dam);
extern bool fire_visible_monsters(int typ, int dam);
extern bool fire_mega_blast(int y, int x, int typ, int rad, int dam);
extern bool fire_meteor_shower(int typ, int dam);
extern bool lite_line(int dir);
extern bool drain_life(int dir, int dam);
extern bool wall_to_mud(int dir);
extern bool wall_to_chaos(int dir);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool heal_monster(int dir);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir);
extern bool sleep_monster(int dir);
extern bool confuse_monster(int dir, int plev);
extern bool fear_monsters(void); /* new illusionist spell -KMW- */
extern bool fear_monsters_touch(void); /* new illusionist spell -KMW- */
extern bool poly_monster(int dir);
extern bool clone_monster(int dir);
extern bool fear_monster(int dir, int plev);
extern bool teleport_monster(int dir);
extern bool door_creation(void);
extern bool trap_creation(void);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern void summon_pet_monster(void);
extern bool fetch_item(int wgt, int y, int x);

/* store.c */
extern void player_theft(void);
extern void do_cmd_store(void);
extern bool store_object_similar(object_type * o_ptr, object_type * j_ptr);
extern void store_object_absorb(object_type *o_ptr, object_type *j_ptr);
extern bool store_buy_item(object_type * o_ptr, store_type * st_ptr);
extern bool store_sell_item(object_type * o_ptr, store_type * st_ptr);
extern void store_shuffle(int which);
extern void store_maint(int which);
extern void store_init(int which);
extern s32b object_store_value(object_type * o_ptr);
extern void store_combine(store_type *st_ptr);
extern void init_st_info(void);

/* bldg.c -KMW- */
extern void do_cmd_bldg(void);
extern bool show_god_info(bool ext);
extern void select_bounties(void);
extern void do_cmd_quest(void);
extern void complete_quest(void);
extern void exit_quest(void);

/* util.c */
extern errr path_parse(char *buf, int max, cptr file);
extern errr path_temp(char *buf, int max);
extern errr path_build(char *buf, int max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern errr my_fclose(FILE * fff);
extern errr my_fgets(FILE * fff, char *buf, huge n);
extern errr my_fputs(FILE * fff, cptr buf, huge n);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, huge n);
extern errr fd_chop(int fd, huge n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
extern void move_cursor(int row, int col);
extern void text_to_ascii(char *buf, cptr str);
extern void ascii_to_text(char *buf, cptr str);
extern sint macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern void flush(void);
extern char inkey(void);
extern void bell(void);
extern void sound(int val);
extern s16b quark_add(cptr str);
extern cptr quark_str(s16b i);
extern s16b message_num(void);
extern cptr message_str(s16b age);
extern byte message_prior(s16b age);
extern void message_add(cptr str, byte prior);
extern void msg_print(cptr msg);
extern void mprint(byte p, cptr msg);
extern void mformat(byte p, cptr fmt, ...);
extern void msg_format(cptr fmt, ...);
extern void screen_save(void);
extern void screen_load(void);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void put_str_center(cptr str, int row);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void c_roff(byte a, cptr str);
extern void roff(cptr str);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, int len, bool complete);
extern bool get_string(cptr prompt, char *buf, int len);
extern bool get_string_cli(cptr prompt, char *buf, int len);
extern bool get_check(cptr prompt);
extern int get_three_way_check(cptr prompt);
extern bool get_com(cptr prompt, char *command);
extern s16b get_quantity(cptr prompt, int max);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern bool is_a_vowel(int ch);

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
extern bool set_confused(int v);
extern bool set_poisoned(int v);
extern bool change_shape(byte shape);
extern bool set_shape(byte shape, int v);
extern void set_grace(s32b v);
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
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern void monster_death(int m_idx);
extern bool mon_take_hit(int m_idx, int dam, bool * fear, cptr note,
	bool give_exp, bool hit_by_pet);
extern void verify_panel(void);
extern cptr look_mon_desc(int m_idx);
extern void ang_sort_aux(vptr u, vptr v, int p, int q);
extern void ang_sort(vptr u, vptr v, int n);
extern sint motion_dir(int y1, int x1, int y2, int x2);
extern sint target_dir(char ch);
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern bool target_set(int mode);
extern bool get_aim_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);

extern void great_side_effect(void);
extern void good_side_effect(void);
extern void ok_side_effect(void);
extern void neutral_side_effect(void);
extern void nasty_side_effect(void);
extern void deadly_side_effect(bool god);

extern void godly_wrath_blast(byte god);

extern void describe_attack(int type, char *r);
extern void describe_attack_fully(int type, char *r);

extern int interpret_grace(void);
extern int interpret_favor(void);

extern void hand_of_fate(void);
extern void fate_effect(int fate_given, int fate_gotten);
extern void strike_it_lucky(void);


/* lua.c */

extern errr init_lua(void);



/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifndef HAS_MEMSET
/* util.c */
extern char *memset(char *, int, huge);
#endif

#ifndef HAS_STRICMP
/* util.c */
extern int stricmp(cptr a, cptr b);
#endif

#ifdef SET_UID
# ifndef HAS_USLEEP
/* util.c */
extern int usleep(huge usecs);
# endif
extern void user_name(char *buf, int id);
#endif

#ifdef MACINTOSH
/* main-mac.c */
/* extern void main(void); */
#endif

#ifdef WINDOWS
/* main-win.c */
/* extern int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, ...); */
#endif

/* main.c */
/* extern int main(int argc, char *argv[]); */
