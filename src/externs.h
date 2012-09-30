/* File: externs.h */

/* Purpose: extern declarations (variables and functions) */

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
extern const s16b cdd[8];
extern const s16b ddx_cdd[8];
extern const s16b ddy_cdd[8];
extern const char hexsym[16];
extern cptr color_char;
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
extern const byte adj_str_wgt[];
extern const byte adj_str_hold[];
extern const byte adj_str_dig[];
extern const byte adj_str_blow[];
extern const byte adj_dex_blow[];
extern const byte adj_dex_safe[];
extern const byte adj_con_fix[];
extern const byte adj_con_mhp[];
extern const byte blows_table[12][12];
extern cptr owner_names[];
extern cptr owner_suffix[];
extern const byte extract_energy[200];
extern const s32b player_exp[PY_MAX_LEVEL];
extern player_sex sex_info[MAX_SEXES];
extern player_race race_info[MAX_RACES];
extern player_class class_info[MAX_CLASS];
extern player_magic magic_info[MAX_CLASS];
extern const u32b fake_spell_flags[4];
extern const byte realm_choices1[];
extern const byte realm_choices2[];
extern cptr realm_names[];
extern cptr spell_names[7][32];
extern const byte chest_traps[64];
extern cptr player_title[MAX_CLASS][PY_MAX_LEVEL / 5];
extern cptr color_names[16];
extern cptr color_seq[16];
extern cptr msg_names[MSG_MAX];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr window_flag_desc[WINDOW_CHOICE_MAX];
extern const int birth_options[OPT_BIRTH + 1];
extern const int server_options[OPT_BIRTH + 1];
extern cptr chaos_patrons[MAX_PATRON];
extern const int chaos_stats[MAX_PATRON];
extern const int chaos_rewards[MAX_PATRON][20];
extern const martial_arts ma_blows[MAX_MA];
extern cptr game_inscriptions[FEEL_MAX];
extern cptr silly_attacks[MAX_SILLY_ATTACK];
extern const rbm_type rbm_info[MAX_RBM];
extern const mutation_type mutations[MUT_SETS_MAX * MUT_PER_SET];
extern const mutation_type race_powers[MAX_RACE_POWERS];

/* variable.c */
extern cptr copyright[5];
extern byte version_major;
extern byte version_minor;
extern byte version_patch;
extern byte version_extra;
extern byte sf_extra;
extern u32b sf_version;
extern u32b sf_xtra;
extern byte z_major;
extern byte z_minor;
extern byte z_patch;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern bool arg_fiddle;
extern bool arg_wizard;
extern bool arg_sound;
extern byte arg_graphics;
extern bool arg_bigtile;
extern bool arg_monochrome;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern bool character_icky;
extern bool character_xtra;
extern u32b seed_flavor;
extern bool msg_flag;
extern s16b num_repro;
extern s32b turn;
extern s32b old_turn;
extern bool use_sound;
extern byte use_graphics;
extern bool use_bigtile;
extern bool use_transparency;
extern s16b signal_count;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern s16b q_max;
extern s16b fld_max;
extern s16b fld_cnt;
extern s16b rg_max;
extern s16b rg_cnt;
extern s16b hack_m_idx;
extern int total_friends;
extern s32b total_friend_levels;
extern s32b friend_align;
extern s16b store_cache_num;
extern store_type **store_cache;
extern char summon_kin_type;
extern bool track_follow;
extern bool track_target;
extern byte hitpoint_warn;
extern byte delay_factor;
extern s16b autosave_freq;
extern byte autosave_t;
extern byte autosave_l;
extern bool cheat_peek;
extern bool cheat_hear;
extern bool cheat_room;
extern bool cheat_xtra;
extern bool cheat_know;
extern bool cheat_live;
extern bool fake_monochrome;
extern byte *mp_a;
extern char *mp_c;
extern byte *mp_ta;
extern char *mp_tc;
extern int player_uid;
extern int player_egid;
extern char player_name[32];
extern char player_base[32];
extern char savefile[1024];
extern s16b view_n;
extern s16b view_y[VIEW_MAX];
extern s16b view_x[VIEW_MAX];
extern s16b temp_n;
extern s16b temp_y[TEMP_MAX];
extern s16b temp_x[TEMP_MAX];
extern s16b lite_n;
extern s16b lite_y[LITE_MAX];
extern s16b lite_x[LITE_MAX];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern bool *macro__cmd;
extern char *macro__buf;
extern option_type option_info[OPT_MAX];
extern u32b window_flag[ANGBAND_TERM_MAX];
extern u32b window_mask[ANGBAND_TERM_MAX];
extern u32b option_mask[ANGBAND_TERM_MAX];
extern term *angband_term[ANGBAND_TERM_MAX];
extern char angband_term_name[ANGBAND_TERM_MAX][16];
extern byte angband_color_table[256][4];
extern char angband_sound_name[SOUND_MAX][16];
extern cave_type *(*area_aux) (int, int);
extern pcave_type *(*parea_aux) (int, int);
extern u16b *temp_block[WILD_BLOCK_SIZE + 1];
extern blk_ptr *wild_cache;
extern int **wild_refcount;
extern u32b wc_cnt;
extern blk_ptr **wild_grid;
extern wild_type **wild;
extern u32b wild_seed;
extern wild_gen_data_type *wild_gen_data;
extern wild_choice_tree_type *wild_choice_tree;
extern bool (*in_bounds) (int, int);
extern bool (*in_bounds2) (int, int);
extern bool (*in_boundsp) (int, int);
extern region_type cave_data;
extern int cur_region;
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *m_list;
extern field_type *fld_list;
extern region_type *rg_list;
extern region_info *ri_list;
extern u16b place_count;
extern place_type *place;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern byte misc_to_attr[256];
extern char misc_to_char[256];
extern byte tval_to_attr[128];
extern char tval_to_char[128];
extern cptr keymap_act[KEYMAP_MODES][256];
extern player_type *p_ptr;
extern player_sex *sp_ptr;
extern player_race *rp_ptr;
extern player_class *cp_ptr;
extern player_magic *mp_ptr;
extern server_type *svr_ptr;
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
extern field_thaum *t_info;
extern quest_type *quest;
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_DIR;
extern cptr ANGBAND_DIR_APEX;
extern cptr ANGBAND_DIR_BONE;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_EDIT;
extern cptr ANGBAND_DIR_SCRIPT;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
extern cptr ANGBAND_DIR_INFO;
extern cptr ANGBAND_DIR_PREF;
extern cptr ANGBAND_DIR_SAVE;
extern cptr ANGBAND_DIR_USER;
extern cptr ANGBAND_DIR_XTRA;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook) (const object_type *o_ptr);
extern bool (*ang_sort_comp) (const vptr u, const vptr v, int a, int b);
extern void (*ang_sort_swap) (const vptr u, const vptr v, int a, int b);
extern s32b max_wild;
extern cptr gf_color[MAX_GF];
extern int owner_names_max;
extern int owner_suffix_max;

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern int distance(int x1, int y1, int x2, int y2);
extern bool is_build(const cave_type *c_ptr);
extern bool is_trap(const cave_type *c_ptr);
extern bool is_visible_trap(const cave_type *c_ptr);
extern bool los(int x1, int y1, int x2, int y2);
extern void mmove_init(int x1, int y1, int x2, int y2);
extern void mmove(int *x, int *y, int x1, int y1);
extern bool projectable(int x1, int y1, int x2, int y2);
extern sint project_path(coord *gp, int x1, int y1, int x2, int y2, u16b flg);
extern bool in_ball_range(int x1, int y1, int x2, int y2);
extern bool in_disintegration_range(int x1, int y1, int x2, int y2);
extern void scatter(int *xp, int *yp, int x, int y, int d);
extern bool player_can_see_bold(int x, int y);
extern bool cave_valid_grid(const cave_type *c_ptr);
extern bool no_lite(void);
extern void move_cursor_relative(int col, int row);
extern void print_rel(char c, byte a, int x, int y);
extern void note_spot(int x, int y);
extern void do_cmd_view_map(void);
extern void forget_view(void);
extern errr vinfo_init(void);
extern void update_view(void);
extern void update_mon_lite(void);
extern void clear_mon_lite(void);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void change_wiz_lite(void);
extern void wiz_dark(void);
extern void cave_set_feat(int x, int y, int feat);

/* cmd1.c */
extern int deadliness_calc(int attack_power);
extern long avg_dam(int attack_power, int dice_num, int dice_sides);
extern bool test_hit_fire(int chance, int ac, int vis);
extern bool test_hit_norm(int chance, int ac, int vis);
extern int tot_dam_aux(const object_type *o_ptr, const monster_type *m_ptr);
extern void search(void);
extern bool auto_pickup_okay(const object_type *o_ptr);
extern void py_pickup_aux(object_type *o_ptr);
extern void carry(int pickup);
extern void py_attack(int x, int y);
extern void move_player(int dir, int do_pickup);
extern void run_step(int dir);

/* cmd2.c */
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_search(void);
extern int count_traps(int *x, int *y, bool under);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_alter(void);
extern void do_cmd_spike(void);
extern void do_cmd_walk(int pickup);
extern void do_cmd_stay(int pickup);
extern void do_cmd_run(void);
extern void do_cmd_rest(void);
extern void do_cmd_fire(void);
extern void do_cmd_fire_aux(int mult, object_type *o_ptr, const object_type *j_ptr);
extern void do_cmd_throw(void);
extern void do_cmd_throw_aux(int mult);
extern bool do_cmd_open_aux(int x, int y);
extern bool do_cmd_disarm_aux(cave_type *c_ptr, int dir);

/* cmd3.c */
extern void do_cmd_inven(void);
extern void do_cmd_equip(void);
extern void do_cmd_wield(void);
extern void do_cmd_takeoff(void);
extern void do_cmd_drop(void);
extern bool destroy_item_aux(object_type *o_ptr, int amt);
extern void do_cmd_destroy(void);
extern void do_cmd_observe(void);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern bool ang_sort_comp_hook(const vptr u, const vptr v, int a, int b);
extern void ang_sort_swap_hook(const vptr u, const vptr v, int a, int b);
extern void do_cmd_query_symbol(void);
extern bool research_mon(void);

/* cmd4.c */
extern void do_cmd_redraw(void);
extern void resize_map(void);
extern void redraw_window(void);
extern void do_cmd_messages(void);
extern void init_options(byte flags);
extern void do_cmd_options(byte flags);
extern void do_cmd_pref(void);
extern errr macro_dump(cptr fname);
extern errr keymap_dump(cptr fname);
extern void do_cmd_macros(void);
extern void do_cmd_visuals(void);
extern void do_cmd_colors(void);
extern void do_cmd_note(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern bool do_cmd_knowledge_pets(int dummy);
extern void dump_town_info(FILE *fff, int town, bool ignore);
extern void do_cmd_knowledge(void);
extern void plural_aux(char *Name);
extern void do_cmd_checkquest(void);
extern void do_cmd_time(void);

/* cmd5.c */
extern void do_cmd_browse(void);
extern void do_cmd_browse_aux(const object_type *o_ptr);
extern void do_cmd_study(void);
extern void do_cmd_cast(void);
extern void do_cmd_pray(void);
extern void do_cmd_pet(void);

/* cmd6.c */
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_activate(void);
extern void do_cmd_rerate(void);
extern void ring_of_power(int dir);

/* dungeon.c */
extern void sense_item(object_type *o_ptr, bool heavy, bool wield, bool msg);
extern void notice_lite_change(object_type *o_ptr);
extern bool psychometry(void);
extern void play_game(bool new_game);

/* files.c */
extern s16b tokenize(char *buf, s16b num, char **tokens, int mode);
extern void display_player(int mode);
extern void do_cmd_character(void);
extern errr file_character(cptr name, bool full);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr fmt, ...);
extern void print_equippy(void);
extern errr check_load_init(void);
extern void likert(char *buf, uint max, cptr fmt, va_list *vp);
extern void player_flags(object_flags *of_ptr);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void change_player_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(int is_autosave);
extern void do_cmd_save_and_exit(void);
extern errr get_rnd_line(cptr file_name, int entry, char *output);
extern void get_character_name(void);

/* generate.c */
extern void place_closed_door(int x, int y);
extern void map_panel_size(void);
extern void inc_rating(int delta_rating);
extern void set_special(void);
extern void del_region(int rg_idx);
extern int unref_region(int rg_idx);
extern void incref_region(int rg_idx);
extern void set_region(int rg_idx);
extern void wipe_rg_list(void);
extern void create_region_aux(s16b *region, int x, int y, byte flags);
extern void generate_cave(void);

/* init1.c */
extern errr init_w_info_txt(FILE *fp, char *buf);
extern errr init_t_info_txt(FILE *fp, char *buf);

/* init2.c */
extern errr init_w_info(void);
extern errr init_t_info(void);
extern void init_file_paths(char *path);
extern void init_angband(void);
extern void cleanup_angband(void);
extern errr check_modification_date(int fd, cptr template_file);

/* load.c */
extern errr rd_savefile_new(void);

/* melee1.c */
/* melee2.c */
extern void flee_message(cptr m_name, u16b r_idx);
extern bool make_attack_normal(int m_idx);
extern bool make_attack_spell(int m_idx);
extern void process_monsters(int min_energy);
extern void reset_monsters(void);
extern void curse_equipment(int chance, int heavy_chance);
extern void mon_take_hit_mon(int m_idx, int dam, bool *fear, cptr note);

/* monster1.c */
extern monster_race *monst_race(int r_idx);
extern cptr mon_race_name(const monster_race *r_ptr);
extern bool mon_name_cont(const monster_race *r_ptr, cptr str);
extern void roff_mon_top(int r_idx);
extern void screen_roff_mon(int r_idx, int remember);
extern void display_roff_mon(int r_idx);
extern void display_visible(void);

/* monster2.c */
extern cptr horror_desc[MAX_SAN_HORROR];
extern cptr funny_desc[MAX_SAN_FUNNY];
extern cptr funny_comments[MAX_SAN_COMMENT];
extern void delete_monster_idx(int i);
extern void delete_monster(int x, int y);
extern void compact_monsters(int size);
extern void wipe_m_list(void);
extern void wipe_monsters(int rg_idx);
extern s16b m_pop(void);
extern void get_mon_num_prep(monster_hook_type monster_hook);
extern s16b get_mon_num(int level);
extern s16b get_filter_mon_num(int level, monster_hook_type monster_hook);
extern void monster_desc(char *desc, const monster_type *m_ptr, int mode,
						 int max);
extern void monster_fmt(char *buf, uint max, cptr fmt, va_list *vp);
extern void lore_do_probe(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon_vis(u16b r_idx, int increment);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern bool test_monster_square(cave_type *c_ptr, monster_race *r_ptr);
extern monster_type *place_monster_aux(int x, int y, int r_idx, bool slp, bool grp,
							  bool friendly, bool pet, bool summon);
extern bool place_monster(int x, int y, bool slp, bool grp, int deltalevel);
extern bool alloc_horde(int x, int y);
extern bool alloc_monster(int dis, bool slp, int delta_level);
extern bool summon_specific(int who, int x1, int y1, int lev, int type,
							bool group, bool friendly, bool pet);
extern monster_type *summon_named_creature(int x1, int y1, int r_idx, bool slp,
								  bool group_ok, bool pet);
extern monster_type *summon_cloned_creature(int x1, int y1, int r_idx, bool pet);
extern monster_type *multiply_monster(int m_idx, bool clone, bool friendly, bool pet);
extern void update_smart_learn(int m_idx, int what);
extern monster_type *place_monster_one(int x, int y, int r_idx, bool slp, bool friendly,
							  bool pet);

/* monster3.c (currently in monster1.c) */
extern void set_friendly(monster_type *m_ptr);
extern void set_pet(monster_type *m_ptr);
extern void set_hostile(monster_type *m_ptr);
extern void anger_monster(monster_type *m_ptr);
extern bool are_enemies(const monster_type *m_ptr1, const monster_type *m_ptr2);
extern bool monster_living(const monster_race *r_ptr);
extern void change_shimmer(void);
extern void change_repair(void);

/* flavor.c */
extern void get_table_name(char *out_string, bool quotes);
extern void flavor_init(void);
extern void object_desc(char *buf, const object_type *o_ptr, int pref,
						int mode, int size);
extern void object_fmt(char *buf, uint max, cptr fmt, va_list *vp);
extern void object_desc_store(char *buf, const object_type *o_ptr, int pref,
							  int mode, int size);
extern void object_store_fmt(char *buf, uint max, cptr fmt, va_list *vp);

/* object1.c */
/* object2.c */

extern void reset_visuals(void);
extern void object_flags_known(const object_type *o_ptr, object_flags *of_ptr);
extern void identify_fully_aux(const object_type *o_ptr);
extern s16b wield_slot(const object_type *o_ptr);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern bool item_tester_hook_weapon(const object_type *o_ptr);
extern bool item_tester_hook_melee_weapon(const object_type *o_ptr);
extern bool item_tester_hook_nonsword(const object_type *o_ptr);
extern bool item_tester_hook_ammo(const object_type *o_ptr);
extern bool item_tester_hook_fletcher(const object_type *o_ptr);
extern bool item_tester_hook_armour(const object_type *o_ptr);
extern bool item_tester_hook_armour_no_acid(const object_type *o_ptr);
extern bool item_tester_hook_soft_armour(const object_type *o_ptr);
extern bool item_tester_hook_hard_armour(const object_type *o_ptr);
extern bool item_tester_hook_helm(const object_type *o_ptr);
extern bool item_tester_hook_pure_hard_armour(const object_type *o_ptr);
extern bool item_tester_hook_weapon_armour(const object_type *o_ptr);
extern bool item_tester_hook_wear(const object_type *o_ptr);
extern bool item_tester_hook_recharge(const object_type *o_ptr);
extern bool item_tester_hook_jewel(const object_type *o_ptr);
extern bool item_tester_hook_tval(const object_type *o_ptr, byte tval);
extern bool item_tester_hook_is_blessed(const object_type *o_ptr);
extern bool item_tester_hook_is_good(const object_type *o_ptr);
extern bool item_tester_hook_is_great(const object_type *o_ptr);
extern bool item_tester_hook_is_book(const object_type *o_ptr);
extern bool item_tester_okay(const object_type *o_ptr);
extern void display_inven(void);
extern void display_equip(void);
extern void show_list(s16b o_list_ptr, bool store);
extern void show_equip(bool store);
extern void toggle_inven_equip(void);
extern object_type *get_item(cptr pmt, cptr str, int mode);
extern void delete_held_object(s16b *o_idx_ptr, object_type *o_ptr);
extern void delete_dungeon_object(object_type *o_ptr);
extern void delete_object(int x, int y);
extern void delete_object_list(s16b *o_idx_ptr);
extern void drop_object_list(s16b *o_idx_ptr, int x, int y);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern void wipe_objects(int rg_idx);
extern object_type *add_object_list(s16b *o_idx_ptr, object_type *o_ptr);
extern void move_object(s16b *tgt_list_ptr, s16b *cur_list_ptr,
						object_type *o_ptr);
extern void swap_objects(object_type *o1_ptr, object_type *o2_ptr);
extern void get_obj_num_prep(object_hook_type object_hook);
extern s16b get_obj_num(int level, int min_level);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern void object_mental(object_type *o_ptr);
extern s32b flag_cost(const object_type *o_ptr, int plusses);
extern s32b object_value(const object_type *o_ptr);
extern s32b object_value_real(const object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern bool object_equal(const object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern s16b *look_up_list(object_type *o_ptr);
extern void object_wipe(object_type *o_ptr);
extern object_type *object_prep(int k_idx);
extern object_type *object_dup(const object_type *o_ptr);
extern void add_ego_flags(object_type *o_ptr, byte ego);
extern void add_ego_power(int power, object_type *o_ptr);
extern s16b m_bonus(int max, int level);
extern void apply_magic(object_type *o_ptr, int lev, int lev_dif, byte flags);
extern void init_match_hook(byte tval, byte sval);
extern byte kind_is_match(int k_idx);
extern void init_match_theme(obj_theme theme);
extern byte kind_is_theme(int k_idx);
extern object_type *make_object(int level, int delta_level, obj_theme *theme);
extern void place_specific_object(int x, int y, int level, int k_idx);
extern void place_object(int x, int y, bool good, bool great, int delta_level);
extern object_type *make_gold(int level, int coin_type);
extern void place_gold(int x, int y);
extern void drop_near(object_type *o_ptr, int chance, int x, int y);
extern void acquirement(int x1, int y1, int num, bool great, bool known);
extern object_type *get_list_item(s16b list_start, int number);
extern int get_item_position(s16b list_start, object_type *o_ptr);
extern int get_list_length(s16b list_start);
extern bool floor_item(object_type *o_ptr);
extern bool player_item(object_type *o_ptr);
extern void item_charges(object_type *o_ptr);
extern void item_describe(object_type *o_ptr);
extern void item_describe_roff(object_type *o_ptr);
extern void item_describe_faux(object_type *o_ptr);
extern object_type *item_split(object_type *o_ptr, int num);
extern void item_increase(object_type *o_ptr, int num);
extern void item_increase_silent(object_type *o_ptr, int num);
extern bool inven_carry_okay(const object_type *o_ptr);
extern object_type *reorder_objects_aux(object_type *q_ptr,
										object_comp comp_func, u16b o_idx);
extern object_type *inven_carry(object_type *o_ptr);
extern object_type *inven_takeoff(object_type *o_ptr);
extern void inven_drop(object_type *o_ptr, int amt);
extern cptr item_activation(const object_type *o_ptr);
extern void combine_pack(void);
extern void reorder_pack(void);
extern object_type *combine_pack_watch(object_type *o_ptr);
extern object_type *reorder_pack_watch(object_type *o_ptr);
extern bool can_player_destroy_object(object_type *o_ptr);
extern void display_koff(int k_idx);
extern object_type *test_floor(int *num, cave_type *c_ptr, int mode);
extern void show_floor(int x, int y);

/* racial.c */
extern bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty);
extern void do_cmd_racial_power(void);

/* save.c */
extern bool save_player(void);
extern bool load_player(void);

/* spells1.c */
extern void take_hit(int damage, cptr kb_str);
extern int dist_to_line(int x, int y, int x1, int y1, int x2, int y2);
extern bool project(int who, int rad, int x, int y, int dam, int typ, u16b flg);

/* spells2.c */
extern void message_pain(int m_idx, int dam);
extern void self_knowledge(void);
extern bool detect_traps(bool ident);
extern void create_closed_door(int x, int y);
extern bool detect_doors(void);
extern bool detect_stairs(void);
extern bool detect_treasure(void);
extern bool detect_objects_gold(void);
extern bool detect_objects_normal(void);
extern bool detect_objects_magic(void);
extern bool detect_monsters_normal(void);
extern bool detect_monsters_invis(void);
extern bool detect_monsters_evil(void);
extern bool detect_monsters_xxx(u32b match_flag);
extern bool detect_monsters_string(cptr match);
extern bool detect_monsters_nonliving(void);
extern bool detect_monsters_living(void);
extern bool detect_all(void);
extern bool wall_stone(void);
extern bool speed_monsters(void);
extern bool slow_monsters(void);
extern bool sleep_monsters(void);
extern void aggravate_monsters(int who);
extern bool genocide(int player_cast);
extern bool mass_genocide(int player_cast);
extern bool probing(void);
extern bool banish_evil(int dist);
extern bool dispel_evil(int dam);
extern bool dispel_good(int dam);
extern bool dispel_undead(int dam);
extern bool dispel_monsters(int dam);
extern bool dispel_living(int dam);
extern bool dispel_demons(int dam);
extern bool raise_dead(int y, int x, bool pet);
extern bool turn_undead(void);
extern bool destroy_area(int x1, int y1, int r);
extern bool earthquake(int cx, int cy, int r);
extern void lite_room(int x1, int y1);
extern void unlite_room(int x1, int y1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern void call_chaos(void);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool lite_line(int dir, int dam);
extern bool drain_life(int dir, int dam);
extern bool drain_gain_life(int dor, int dam);
extern bool death_ray(int dir, int plev);
extern bool wall_to_mud(int dir);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool wizard_lock(int dir);
extern bool heal_monster(int dir);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir);
extern bool sleep_monster(int dir);
extern bool stasis_monster(int dir);	/* Like sleep, affects undead as well */
extern bool confuse_monster(int dir, int plev);
extern bool stun_monster(int dir, int plev);
extern bool fear_monster(int dir, int plev);
extern bool poly_monster(int dir);
extern bool clone_monster(int dir);
extern bool teleport_monster(int dir);
extern bool door_creation(void);
extern bool trap_creation(void);
extern bool glyph_creation(void);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern bool activate_ty_curse(bool stop_ty, int *count);
extern int activate_hi_summon(void);
extern int summon_cyber(int who, int x, int y);
extern void wall_breaker(void);
extern bool confuse_monsters(int dam);
extern bool charm_monsters(int dam);
extern bool charm_animals(int dam);
extern bool stun_monsters(int dam);
extern bool stasis_monsters(int dam);
extern bool banish_monsters(int dist);
extern bool turn_monsters(int dam);
extern bool turn_evil(int dam);
extern bool deathray_monsters(void);
extern bool charm_monster(int dir, int plev);
extern bool control_one_undead(int dir, int plev);
extern bool charm_animal(int dir, int plev);
extern bool starlite(void);
extern bool scatter_ball(int num, int typ, int dam, int rad);
extern void create_food(void);
extern void whirlwind_attack(void);
extern bool mindblast_monsters(int dam);
extern void report_magics(void);
extern bool teleport_swap(int dir);
extern bool project_hook(int typ, int dir, int dam, u16b flg);
extern bool project_hack(int typ, int dam);

/* spells3.c */
extern bool teleport_away(int m_idx, int dis);
extern void teleport_to_player(int m_idx);
extern void teleport_player(int dis);
extern void teleport_player_to(int nx, int ny);
extern void teleport_player_level(void);
extern bool check_down_wild(void);
extern void recall_player(int turns);
extern void word_of_recall(void);
extern bool apply_disenchant(void);
extern void mutate_player(void);
extern void apply_nexus(const monster_type *m_ptr);
extern void phlogiston(void);
extern void brand_weapon(int brand_type);
extern void call_the_(void);
extern void fetch(int dir, int wgt, bool require_los);
extern void alter_reality(void);
extern bool warding_glyph(void);
extern bool explosive_rune(void);
extern void identify_pack(void);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool alchemy(void);
extern void stair_creation(void);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool artifact_scroll(void);
extern bool ident_spell(void);
extern bool ident_scroll(int k_idx);
extern bool mundane_spell(void);
extern void identify_item(object_type *o_ptr);
extern bool identify_fully(void);
extern bool recharge(int num);
extern bool bless_weapon(void);
extern bool potion_smash_effect(int who, int x, int y, object_type *o_ptr);
extern void display_spell_list(void);
extern s16b spell_chance(int spell, int realm);
extern int spell_mana(int spell, int realm);
extern bool spell_okay(int spell, bool known, int realm);
extern void spell_info(char *p, int spell, int realm);
extern void print_spells(byte *spells, int num, int x, int y, int realm);
extern bool hates_acid(const object_type *o_ptr);
extern bool hates_elec(const object_type *o_ptr);
extern bool hates_fire(const object_type *o_ptr);
extern bool hates_cold(const object_type *o_ptr);
extern int set_acid_destroy(object_type *o_ptr);
extern int set_elec_destroy(object_type *o_ptr);
extern int set_fire_destroy(object_type *o_ptr);
extern int set_cold_destroy(object_type *o_ptr);
extern int inven_damage(inven_func typ, int perc);
extern bool rustproof(void);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern bool brand_bolts(void);
extern bool polymorph_monster(int x, int y);
extern bool dimension_door(void);
extern void map_wilderness(int radius, s32b x, s32b y);
extern void sanity_blast(const monster_type *m_ptr);

/* store.c */
extern s32b price_item(object_type *o_ptr, bool flip);
extern bool allocate_store(store_type *st_ptr);
extern store_type *get_current_store(void);
extern void do_cmd_store(const field_type *f_ptr);
extern void store_init(int town_num, int store_num, byte store);
extern void place_sb(int greed, int max_cost);
extern bool do_standard_command(s16b c);

/* bldg.c */
extern bool get_nightmare(int r_idx);
extern void have_nightmare(void);
extern bool test_gold(s32b cost);
extern bool build_has_quest(void);
extern void build_cmd_quest(int level);
extern void display_build(const field_type *f_ptr);
extern void do_cmd_bldg(const field_type *f_ptr);
extern bool compare_weapons(void);
extern bool enchant_item(s32b cost, bool to_hit, bool to_dam, bool to_ac, bool weap);
extern void building_recharge(s32b cost);
extern bool building_healer(void);
extern void record_aura(void);
extern bool building_magetower(int factor, bool display);
extern void gamble_help(void);
extern void gamble_in_between(void);
extern void gamble_craps(void);
extern void gamble_spin_wheel(void);
extern void gamble_dice_slots(void);
extern bool inn_rest(void);
extern void build_init(int town_num, int build_num, byte build_type);

/* util.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern void init_setuid(void);
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);
extern bool assert_helper(cptr expr, cptr file, int line, bool result);
extern errr path_parse(char *buf, int max, cptr file);
extern void path_build(char *buf, int max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern FILE *my_fopen_temp(char *buf, int max);
extern errr my_fgets(FILE *fff, char *buf, huge n);
extern errr my_raw_fgets(FILE *fff, char *buf, huge n);
extern void my_fclose(FILE *fff);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, huge n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
extern int count_bits(u32b x);
extern sint macro_find_exact(cptr pat);
extern void macro_add(cptr pat, cptr act);
extern void flush(void);
extern void text_to_ascii(char *buf, cptr str);
extern void ascii_to_text(char *buf, cptr str);
extern char inkey(void);
extern s16b quark_add(cptr str);
extern s16b quark_fmt(cptr str, ...);
extern void quark_remove(s16b *i);
extern void quark_dup(s16b i);
extern cptr quark_str(s16b i);
extern errr quarks_init(void);
extern errr quarks_free(void);
extern byte get_msg_type_color(byte a);
extern s16b message_num(void);
extern cptr message_str(s16b age);
extern u16b message_type(s16b age);
extern byte message_color(s16b age);
extern errr message_color_define(u16b type, byte color);
extern void message_add(cptr str, u16b type);
extern errr messages_init(void);
extern void messages_free(void);
extern void set_message_type(char *buf, uint max, cptr fmt, va_list *vp);
extern void msgf(cptr fmt, ...);
extern void msg_effect(u16b type, s16b extra);
extern void message_flush(void);
extern bool is_a_vowel(int ch);
extern void request_command(int shopping);
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_clear(void);
extern void repeat_check(void);

#ifdef PRIVATE_USER_PATH
extern void create_user_dirs(void);
#endif /* PRIVATE_USER_PATH */

/* xtra1.c */
extern s16b modify_stat_value(int value, int amount);
extern void stat_format(char *buf, uint max, cptr fmt, va_list *vp);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);
extern void change_stuff(void);
bool player_save(int power);
extern void object_bonuses(const object_type *o_ptr, bonuses_type *b);
extern void object_bonuses_known(const object_type *o_ptr, bonuses_type *b);


/* effects.c */
extern bool inc_blind(int v);
extern bool clear_blind(void);
extern bool inc_confused(int v);
extern bool clear_confused(void);
extern bool inc_poisoned(int v);
extern bool clear_poisoned(void);
extern bool inc_afraid(int v);
extern bool clear_afraid(void);
extern bool inc_paralyzed(int v);
extern bool clear_paralyzed(void);
extern bool inc_image(int v);
extern bool clear_image(void);
extern bool inc_fast(int v);
extern bool clear_fast(void);
extern bool inc_slow(int v);
extern bool clear_slow(void);
extern bool inc_shield(int v);
extern bool inc_blessed(int v);
extern bool inc_hero(int v);
extern bool inc_shero(int v);
extern bool inc_protevil(int v);
extern bool inc_wraith_form(int v);
extern bool inc_tim_esp(int v);
extern bool clear_tim_esp(void);
extern bool inc_invuln(int v);
extern bool inc_tim_invis(int v);
extern bool inc_tim_infra(int v);
extern bool inc_oppose_acid(int v);
extern bool inc_oppose_elec(int v);
extern bool inc_oppose_fire(int v);
extern bool inc_oppose_cold(int v);
extern bool inc_oppose_pois(int v);
extern int res_acid_lvl(void);
extern int res_elec_lvl(void);
extern int res_fire_lvl(void);
extern int res_cold_lvl(void);
extern int res_pois_lvl(void);
extern int resist(int dam, int (*f_func) (void));
extern bool acid_dam(int dam, cptr kb_str);
extern bool elec_dam(int dam, cptr kb_str);
extern bool fire_dam(int dam, cptr kb_str);
extern bool cold_dam(int dam, cptr kb_str);
extern bool pois_dam(int dam, cptr kb_str, int pois);
extern bool inc_stun(int v);
extern bool clear_stun(void);
extern bool inc_cut(int v);
extern bool clear_cut(void);
extern bool set_food(int v);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, int permanent);
extern bool res_stat(int stat);
extern bool hp_player(int num);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern bool restore_level(void);
extern bool lose_all_info(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern void do_poly_self(void);
extern void make_noise(byte amount);

extern void disturb(bool stop_search);
extern void notice_inven(void);
extern void notice_equip(void);
extern void notice_item(void);


/* xtra2.c */
extern void check_experience(void);
extern bool monster_death(int m_idx, bool explode);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note);
extern void get_map_size(int *x, int *y);
extern bool panel_center(int x, int y);
extern bool change_panel(int dx, int dy);
extern void verify_panel(void);
extern cptr look_mon_desc(int m_idx);
extern void ang_sort_aux(vptr u, vptr v, int p, int q);
extern void ang_sort(vptr u, vptr v, int n);
extern bool target_able(int m_idx);
extern bool target_okay(void);
extern bool target_set(int mode);
extern bool get_aim_dir(int *dp);
extern bool get_hack_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern int get_chaos_patron(void);
extern void gain_level_reward(int chosen_reward);
extern bool tgt_pt(int *x, int *y);
extern void do_poly_wounds(void);
extern int mon_damage_mod(const monster_type *m_ptr, int dam, int type);
extern void exp_for_kill(const monster_race *r_ptr, s32b *new_exp,
						 s32b *new_exp_frac);
extern int stat_cap(int stat);
extern int adjust_stat(int stat, int value, int amount);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);


/* mspells1.c */
extern bool clean_shot(int x1, int y1, int x2, int y2, bool friendly);

/* mspells2.c */
extern bool monst_spell_monst(int m_idx);

/* artifact.c */
extern bool create_artifact(object_type *o_ptr, int level, bool a_scroll);
extern void create_named_art(int a_idx, int x, int y);

/* scores.c */
extern bool display_scores_aux(int from, int to, int note,
							   const high_score *score);
extern void display_scores(int from, int to);
extern void enter_score(void);
extern void predict_score(void);
extern void race_legends(void);
extern void race_score(int race_num);
extern void show_highclass(void);
extern void ingame_score(bool *initialized, bool game_in_progress);
extern void close_game(void);
extern void exit_game_panic(void);

/* mind.c */
extern mindcraft_power mindcraft_powers[MINDCRAFT_MAX];
extern void mindcraft_info(char *p, int power);
extern void do_cmd_mindcraft(void);

/* mutation.c */
extern bool player_has_mut(int mutation);
extern bool gain_mutation(int choose_mut);
extern bool lose_mutation(int choose_mut);
extern void dump_mutations(FILE *OutFile);
extern bool do_cmd_knowledge_mutations(int dummy);
extern int count_mutations(void);
extern void mutation_power_aux(const mutation_type *mut_ptr);
extern void mutation_random_aux(const mutation_type *mut_ptr);
extern void mutation_effect(void);


/* obj_kind.c */
extern errr k_info_alloc(void);
extern errr k_info_free(void);
extern object_kind *k_info_add(object_kind *k_info_entry);
extern byte get_object_level(const object_type *o_ptr);
extern cptr get_object_name(const object_type *o_ptr);
extern bool object_is_potion(const object_type *o_ptr);
extern errr init_object_alloc(void);
extern void k_info_reset(void);

/* wild1.c and wild2.c */
extern void select_town_name(char *name, int pop);
extern const dun_gen_type *pick_dungeon_type(void);
extern void light_dark_square(int y, int x, bool daytime);
extern u16b init_choice_tree(wild_bound_box_type *bound, u16b type);
extern u16b add_node_tree_root(wild_bound_box_type *bound, u16b type);
extern void test_decision_tree(void);
extern void repopulate_wilderness(void);
extern void create_wilderness(void);
extern void move_wild(void);
extern void shift_in_bounds(int *x, int *y);
extern byte the_floor(void);
extern void change_level(int);
extern int base_level(void);
extern void wipe_all_list(void);
extern dun_type *dungeon(void);
extern void move_dun_level(int direction);
extern int max_dun_level_reached(void);
extern cptr building_name(byte build_type);
extern void building_char(byte build_type, byte *a, char *c);
extern cptr dungeon_type_name(u32b dun);


/* avatar.c */
extern cptr virtue[MAX_VIRTUE];
extern void get_virtues(void);
extern void chg_virtue(int virtue, int amount);
extern void dump_virtues(FILE *OutFile);

/* notes.c */
extern cptr notes_file(void);
extern void output_note(cptr final_note, ...);
extern void add_note(char code, cptr note, ...);
extern void add_note_type(int note_number);

/* fields.c */
extern void notice_field(field_type *f_ptr);
extern cptr field_name(const field_type *f_ptr);
extern void excise_field_idx(int fld_idx);
extern void delete_field_ptr(field_type *f_ptr);
extern void delete_field(int x, int y);
extern void delete_field_location(cave_type *c_ptr);
extern void compact_fields(int size);
extern void wipe_f_list(void);
extern void wipe_fields(int rg_idx);
extern s16b f_pop(void);
extern void field_wipe(field_type *f_ptr);
extern void field_copy(field_type *f_ptr, field_type *j_ptr);
extern s16b field_add(field_type *f_ptr, cave_type *c_ptr);
extern void field_prep(field_type *f_ptr, s16b t_idx);
extern void init_fields(void);
extern field_type *field_is_type(const cave_type *c_ptr, byte typ);
extern field_type *field_first_known(const cave_type *c_ptr, byte typ);
extern u16b fields_have_flags(const cave_type *c_ptr, u16b info);
extern bool field_detect_type(const cave_type *c_ptr, byte typ);
extern void field_destroy_type(cave_type *c_ptr, byte typ);
extern field_type *place_field(int x, int y, s16b t_idx);
extern bool field_script_single(field_type *f_ptr, int action, cptr format, ...);
extern void field_script_const(const field_type *f_ptr, int action, cptr format, ...);
extern void field_script(cave_type *c_ptr, int action, cptr format, ...);
extern bool field_script_special(cave_type *c_ptr, u16b t_idx, cptr format, ...);
extern field_type *field_script_find(cave_type *c_ptr, int action, cptr format, ...);
extern void process_fields(void);
extern void test_field_data_integrity(void);
extern void set_corpse_size(field_type *f_ptr, int size);
extern void place_trap(int x, int y);
extern void make_lockjam_door(int x, int y, int power, bool jam);
extern bool monster_can_open(monster_race *r_ptr, int power);
extern bool check_trap_hit(int power);
extern bool dont_save(int power);
extern void hit_trap(field_type *f_ptr);
extern void evil_trap(void);
extern void drop_random_item(void);
extern void drain_lite(void);
extern void drain_food(void);
extern void drain_magic(void);
extern void print_building_options(cptr strings[], int num);

/* compress.c */
extern void test_compress_module(void);

/* quest.c */
extern u16b q_pop(void);
extern void discover_wild_quest(int q_num);
extern u16b insert_dungeon_monster_quest(u16b r_idx, u16b num, u16b level);
extern cptr describe_quest_location(cptr * dirn, int x, int y, bool known);
extern void dump_castle_info(FILE *fff, int place);
extern errr init_quests(void);
extern void init_player_quests(void);
extern void quest_discovery(void);
extern bool is_special_level(int level);
extern void activate_quests(int level);
extern void trigger_quest_create(byte c_type, vptr data);
extern void trigger_quest_complete(byte x_type, vptr data);
extern quest_type *lookup_quest_building(const store_type *b_ptr);
extern void reward_quest(quest_type *q_ptr);
extern void request_quest(const store_type *b_ptr, int scale);
extern bool do_cmd_knowledge_quests(int dummy);

/* maid-grf.c */
extern void init_term_callbacks(void);
extern void free_term_callbacks(void);
extern void init_overhead_map(void);
extern void del_overhead_map(void);
extern void display_dungeon(void);
extern void lite_spot(int x, int y);
extern void prt_map(void);
extern void display_map(int *cx, int *cy);
extern void update_overhead_map(void);
extern void Term_erase_map(void);
extern void Term_move_player(void);
extern void Term_write_equipment(void);
extern void Term_write_list(s16b o_idx, byte list_type);

/* ui.c */
extern void center_string(char *buf, uint max, cptr fmt, va_list *vp);
extern void binary_fmt(char *buf, uint max, cptr fmt, va_list *vp);
extern void fmt_clean(char *buf);
extern int get_player_choice(cptr *choices, int num, int col, int wid,
                             cptr helpfile, void (*hook) (cptr));
extern int get_player_sort_choice(cptr *choices, int num, int col, int wid,
                                  cptr helpfile, void (*hook) (cptr));
extern bool display_menu(menu_type *options, int select, bool scroll,
						 int disp(int), cptr prompt);
extern void bell(cptr reason);
extern void sound(int num);
extern int color_char_to_attr(char c);
extern void screen_save(void);
extern void screen_load(void);
extern int fmt_offset(cptr str1, cptr str2);
extern void put_fstr(int col, int row, cptr str, ...);
extern void prtf(int col, int row, cptr str, ...);
extern void roff(cptr str, ...);
extern void froff(FILE *fff, cptr str, ...);
extern void clear_from(int row);
extern void clear_msg(void);
extern void clear_row(int row);
extern void clear_region(int x, int y1, int y2);
extern bool askfor_aux(char *buf, int len);
extern bool get_string(char *buf, int len, cptr str, ...);
extern bool get_check(cptr prompt, ...);
extern bool get_check_ext(bool def, bool esc, cptr prompt, ...);
extern bool get_com(cptr prompt, char *command);
extern s16b get_quantity(cptr prompt, int max);
extern void pause_line(int row);
extern int get_keymap_dir(char ch);


/* borg.c */
extern void do_cmd_borg(void);

/* script.c */
extern void deleteme(void);
extern bool player_res(u32b flag);

/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAS_USLEEP
/* util.c */
extern int usleep(huge usecs);
# endif	/* HAS_USLEEP */
extern void user_name(char *buf, int id);
#endif /* SET_UID */

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

