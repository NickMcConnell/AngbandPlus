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
extern const s16b cdd[8];
extern const s16b ddx_cdd[8];
extern const s16b ddy_cdd[8];
extern const s16b ddx_cdddouble[8];
extern const s16b ddy_cdddouble[8];
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
extern spell_book books[SV_BOOK_MAX];
extern steamware wares[MAX_STEAMWARE_PARTS];
extern const byte chest_traps[100];
extern u32b alloc_kind_total;
extern byte required_tval;
extern byte old_required_tval;
extern cptr resist_names[RS_MAX];
extern cptr resist_names_short[RS_MAX];
extern res_cap resist_caps[RS_MAX];
extern cptr color_names[16];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr window_flag_desc[32];
extern cptr option_text[OPT_MAX];
extern cptr option_desc[OPT_MAX];
extern const bool option_norm[OPT_MAX];
extern const byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];
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
extern bool character_existed;
extern s16b character_icky;
extern s16b character_xtra;
extern u32b seed_randart;
extern u32b seed_flavor;
extern u32b seed_town;
extern s16b num_repro;
extern s16b object_level;
extern s16b old_object_level;
extern s16b monster_level;
extern char summon_kin_type;
extern s32b turn;
extern s32b old_turn;
extern s16b autosave_freq;
extern bool use_sound;
extern bool use_graphics;
extern s16b image_count;
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
extern bool repair_mflag_show;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern byte feeling;
extern s16b rating;
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
extern const char angband_sound_name[MSG_MAX][16];
extern sint view_n;
extern u16b *view_g;
extern sint temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern u16b (*cave_info)[256];
extern byte (*cave_feat)[DUNGEON_WID];
extern s16b (*cave_o_idx)[DUNGEON_WID];
extern s16b (*cave_m_idx)[DUNGEON_WID];

#ifdef MONSTER_FLOW

extern byte (*cave_cost)[DUNGEON_WID];
extern byte (*cave_when)[DUNGEON_WID];
extern int scent_when;
extern int flow_center_y;
extern int flow_center_x;
extern int update_center_y;
extern int update_center_x;
extern int cost_at_center;

#endif

extern s16b add_wakeup_chance;
extern s16b total_wakeup_chance;
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *m_list;
extern monster_lore *l_list;
extern effect_type *x_list;
extern store_type *store;
extern object_type *inventory;
extern bool *permit_kind_table;
extern byte *chance_kind_table;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
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
extern quest_type *q_info;
extern char *q_name;
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
extern bool (*old_get_obj_num_hook)(int k_idx);
extern void (*text_out_hook)(byte a, cptr str);
extern int highscore_fd;
extern bool use_transparency;
extern bool hack_mutation;
extern bool can_save;
extern int total_friends;
extern s32b total_friend_levels;
extern int skill_count;
extern bool skip_msgs;

/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern bool feat_supports_lighting(byte feat);
#ifdef USE_TRANSPARENCY
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
#else /* USE_TRANSPARENCY */
extern void map_info(int y, int x, byte *ap, char *cp);
#endif /* USE_TRANSPARENCY */
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
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);
extern void town_illuminate(bool daytime);
extern void cave_set_feat(int y, int x, int feat);
extern int project_path(u16b *gp, int range, \
                         int y1, int x1, int *y2, int *x2, u32b flg);
extern byte projectable(int y1, int x1, int y2, int x2, u32b flg);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void disturb(int stop_search, int unused_flag);
extern void update_noise(bool full);
extern void update_smell(void);

/* cmd-attk.c */
extern bool test_hit_fire(int chance, int ac, int vis);
extern bool test_hit_norm(int chance, int ac, int vis);
extern sint critical_shot(int weight, int plus, int dam);
extern sint critical_norm(int weight, int plus, int dam, bool martial);
extern void py_pickup(int pickup);
extern void hit_trap(int y, int x);
extern int breakage_chance(const object_type *o_ptr);
extern void py_attack(int y, int x, int mode);
extern void do_cmd_fire(int gun_kata);
extern void do_cmd_throw(void);

/* cmd-misc.c */
extern void search(void);
extern void run_step(int dir);
extern void move_player(int dir, int jumping);
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
extern void do_cmd_wield(void);
extern void do_cmd_takeoff(void);
extern void do_cmd_drop(void);
extern void do_cmd_destroy(int item);
extern void do_cmd_observe(object_type *o_ptr, bool in_store);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_refill(void);
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
extern void do_cmd_quest(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_special_message(cptr path, cptr file);
extern void do_cmd_save_screen(void);
extern void do_cmd_knowledge(void);

/* cmd5.c */
extern void do_cmd_browse(void);
extern void do_cmd_magic(void);
extern void print_spells(int book, int y, int x);
extern bool do_power(int book, int idx, int dir, int beam, bool *obvious, int magepower, int willpower, int gearhead, int mageprot, int prayprot);
extern info_entry power_info[POW_MAX];

/* cmd6.c */
extern cptr do_object(int mode, object_type *o_ptr);
extern void use_object(int tval);
extern cptr do_device(int mode, object_type *o_ptr, bool *ident,
	bool *used, bool uncontrolled);
extern void use_device(int tval);
extern void show_book_number(int num);
extern void do_cmd_activate(void);
extern cptr do_activation_aux(int mode, object_type *o_ptr);


/* classpowers.c */
extern mind_power mind_powers[10];
extern void mindcraft_info(char *p, int use_mind, int power);
extern void do_cmd_mind(void);
/* extern void do_cmd_mind_browse(void); */


/* dungeon.c */
extern void play_game(bool new_game);


/* effects.c */
extern int effect_prep(void);
extern void process_effects(void);


/* files.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr name);
extern errr check_time(void);
extern errr check_time_init(void);
extern errr check_load(void);
extern errr check_load_init(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3);
extern void display_player_skills(void);
extern void display_player_misc_info(void);
extern void display_player_stat_info(void);
extern void display_player(int mode);
extern errr file_character(cptr name, bool full);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void do_cmd_help(void);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(bool is_autosave);
extern long total_points(void);
extern void display_scores(int from, int to);
extern errr predict_score(void);
extern void close_game(void);
extern void exit_game_panic(void);
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);
extern void display_scores_aux(int from, int to, int note, high_score *score);

/* generate.c */
extern void generate_cave(void);

/* init2.c */
extern void init_file_paths(char *path);
extern void init_angband(void);
extern void cleanup_angband(void);

/* load1.c */
 
/* level.c */
extern void do_cmd_gain_level(bool train);
extern cptr desc_stat_neg[A_MAX];

/* load2.c */
extern errr rd_savefile_new(void);

/* monattk.c */
extern bool make_attack_normal(monster_type *m_ptr);
extern bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px);
extern void mon_take_hit_mon(int m_idx, int dam, cptr note);
extern bool monst_attack_monst(int m_idx, int t_idx);
extern s16b get_dam(int av_dam, int control);
extern void cloud_surround(int r_idx, int *typ, int *dam, int *rad);
extern void cloud(int m_idx, int typ, int dam, int rad);


/* monmove.c */
extern int get_scent(int y, int x);
extern void remove_expensive_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p);
extern int choose_ranged_attack(int m_idx, int *tar_y, int *tar_x, bool archery_only, bool pet);
extern void remove_useless_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool require_los);
extern bool cave_exist_mon(monster_race *r_ptr, int y, int x, bool occupied_ok, bool can_dig);
extern void process_monsters(byte minimum_energy);
extern bool clean_shot(int y1, int x1, int y2, int x2, bool friend);
extern void reset_monsters(void);

/* monster1.c */
extern void screen_roff(int r_idx, bool table);
extern void display_roff(int r_idx);
extern void get_closest_los_monster(int n, int y0, int x0, int *ty, int *tx,
   bool require_visible);
extern void display_visible(void);


/* monster2.c */
extern s16b poly_r_idx(int r_idx);
extern void delete_monster_idx(int i);
extern void update_mon_vis(u16b r_idx, int increment);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_m_list(void);
extern s16b m_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level);
extern void monster_desc(char *desc, monster_type *m_ptr, int mode);
extern void monster_desc_race(char *desc, size_t max, int r_idx);
extern void lore_do_probe(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern bool update_mon(int m_idx, bool full, bool complete);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, object_type *j_ptr);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern s16b player_place(int y, int x);
extern s16b monster_place(int y, int x, monster_type *n_ptr, bool pet);
extern bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool pet, bool clone);
extern bool place_monster(int y, int x, bool slp, bool grp, bool pet);
extern bool alloc_monster(int dis, bool slp);
extern bool summon_specific(int y1, int x1, int lev, int type, bool pet);
extern void set_mon_fear(monster_type *m_ptr, int v, bool panic);
extern bool multiply_monster(int m_idx, bool pet, bool clone);
extern void message_pain(int m_idx, int dam);
extern void update_smart_learn(int m_idx, int what);

/* mspells1.c */
extern bool monst_spell_monst(int m_idx);
extern int apply_resistance(int dam, s16b res);

/* mutation.c */
extern bool gain_random_mutation(int choose_mut);
extern bool lose_mutation(int choose_mut);
extern void dump_mutations(FILE *OutFile);
extern void do_cmd_knowledge_mutations(void);
extern void mutation_power_aux(u32b power);
extern bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty);
extern void do_cmd_racial_power(void);
extern void process_mutations(void);
extern void calc_mutations(void);

/* object1.c */
extern cptr obj_class_info[101];
extern void flavor_init(void);
extern void reset_visuals(bool prefs);
extern s16b get_object_pval(const object_type *o_ptr, u32b flg);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_desc(char *buf, const object_type *o_ptr, int pref, int mode);
extern void object_desc_store(char *buf, const object_type *o_ptr, int pref, int mode);
extern s16b object_resist(const object_type *o_ptr, int res_type);
extern s16b object_resist_known(const object_type *o_ptr, int res_type);
extern cptr item_activation(const object_type *o_ptr);
extern int identify_random_gen(const object_type *o_ptr, cptr *info, int len);
extern bool identify_fully_aux(const object_type *o_ptr);
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
extern void object_info(char *buf, object_type *o_ptr);
extern void object_details(object_type *o_ptr, bool mental, bool known);



/* object2.c */
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern errr get_obj_num_prep(void);
extern s16b get_obj_num(int level);
extern void object_mental(object_type *o_ptr);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great);
extern bool make_object(object_type *j_ptr, bool good, bool great, bool exact_kind);
extern bool make_gold(object_type *j_ptr);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern void drop_near(object_type *j_ptr, int chance, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_object(int y, int x, bool good, bool great);
extern void place_quest_chest(int y, int x, bool good, bool great);
extern void place_gold(int y, int x);
extern void pick_trap(int y, int x);
extern void place_trap(int y, int x);
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
extern void display_koff(int k_idx);
extern void reduce_charges(object_type *o_ptr, int amt);
extern void object_mention(const object_type *o_ptr);

/* pet.c */
extern void do_cmd_pet(void);
extern bool is_friendly(monster_type *m_ptr);
extern void set_friendly(monster_type *m_ptr);
extern bool is_pet(monster_type *m_ptr);
extern void set_pet(monster_type *m_ptr);
extern bool is_hostile(monster_type *m_ptr);
extern void set_hostile(monster_type *m_ptr);
extern void anger_monster(monster_type *m_ptr);
extern bool are_enemies(monster_type *m_ptr1, monster_type *m_ptr2);
extern bool monster_living(monster_race *r_ptr);
extern void monster_drop_carried_objects(monster_type *m_ptr);

/* quest.c */
extern void plural_aux(char *name, size_t max);
extern cptr describe_quest(s16b level, int mode);
extern void display_guild(void);
extern void guild_purchase(void);
extern byte quest_check(int lev);
extern int quest_num(int lev);
extern int quest_item_slot(void);
extern void quest_fail(void);

/* save.c */
extern bool save_player(void);
extern bool load_player(void);

/* skills.c */
extern player_skills skills[N_SKILLS];
extern void skill_raceinit(void);
extern void skill_classinit(void);
extern void skill_cleanup(void);
extern void skill_max_up(int skill);
extern void print_all_skills(int selected, int mode, bool train);
extern void prt_skill_rank(int skill, int selected, int mode, bool train);
extern void skill_up(int skill);


/* spells1.c */
/* extern s16b poly_r_idx(int r_idx); */
extern void take_hit(int dam, cptr kb_str, bool wounding);
extern bool fire_dam(int dam, int typ, cptr kb_str);
extern bool earth_dam(int dam, int typ, cptr kb_str);
extern bool air_dam(int dam, int typ, cptr kb_str);
extern bool water_dam(int dam, int typ, cptr kb_str);
extern bool elec_dam(int dam, int typ, cptr kb_str);
extern bool ice_dam(int dam, int typ, cptr kb_str);
extern bool acid_dam(int dam, int typ, cptr kb_str);
extern bool poison_dam(int dam, int typ, cptr kb_str);
extern void time_dam(int dam, int typ, cptr kb_str);
extern void automata_equipment_decay(int power);
extern byte spell_color(int type);
extern bool apply_disenchant(int mode);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
				 u32b flg, int degrees, byte source_diameter);
extern bool resist_effect(s16b res);

/* spells2.c */
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
extern bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
	u32b flg);
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
	int len, int perc, byte projection, bool lingering);
extern bool beam_burst(int y, int x, int typ, int num, int dam);
extern bool fire_blast(int typ, int dir, int dd, int ds, int num, int dev, bool beam);
extern bool fire_barrage(int typ, int dir, int dd, int ds, int num, int dev, int rad);
extern bool project_los(int typ, int dam);
extern bool hp_player(int num);
extern bool wp_player(int num);
extern bool heal_player(int perc, int min);
extern int get_heal_amount(int perc, int min);
extern bool heal_player_sp(int perc, int min);
extern int get_heal_sp_amount(int perc, int min);
extern bool sp_player(int num, cptr msg);
extern void warding_glyph(void);
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
extern bool detect_monsters(bool extrarange, bool match, u32b flags, 
	int flag_set, const char *str);
extern bool detect_traps(bool extrarange);
extern bool detect_doors(bool extrarange);
extern bool detect_stairs(bool extrarange);
extern bool detect_treasure(bool extrarange);
extern bool detect_objects_gold(bool extrarange);
extern bool detect_objects_normal(bool extrarange);
extern bool detect_objects_magic(bool extrarange);
extern bool detect_monsters_normal(bool extrarange);
extern bool detect_monsters_invis(bool extrarange);
extern bool detect_monsters_charm(bool extrarange);
extern bool detect_evil(bool extrarange);
extern bool detect_undead(bool extrarange);
extern bool detect_life(bool extrarange);
extern bool detect_animals(bool extrarange);
extern bool detect_all_monsters(bool extrarange);
extern bool detect_all(void);
extern void stair_creation(void);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool recharge(int num);
extern bool speed_monsters(void);
extern bool slow_monsters(void);
extern bool sleep_monsters(void);
extern bool banish_evil(int dist);
extern bool turn_undead(int dam);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_demon(int dam);
/* extern bool dispel_monsters(int dam); */
extern void aggravate_monsters(int who);
extern bool genocide(void);
extern bool mass_genocide(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool lite_line(int dir);
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
extern bool trap_creation(int y, int x);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern bool stun_monsters(int dam);
extern bool confuse_monsters(int dam);
extern bool turn_monsters(int dam);
extern bool charm_monsters(int dam);
extern bool charm_monster(int dir, int plev);
extern bool charm_animal(int dir, int plev);
extern bool charm_animals(int dam);
extern bool manifest_god(void);
extern bool dispel_monsters(int dam);
extern void cave_temp_mark(int y, int x, bool room);
extern void spread_cave_temp(int y1, int x1, int range, bool room);
extern void clear_temp_array(void);



/* spells3.c */
extern bool item_smash_effect(int who, int y, int x, object_type *o_ptr);
extern int mechanism_read_effect(int who, int y, int x, object_type *o_ptr);
extern bool device_use_effect(int who, int power, int y, int x,
	object_type *o_ptr);
extern void teleport_away(int m_idx, int dis);
extern void thrust_away(int who, int t_y, int t_x, int grids_away);
extern void teleport_player(int dis);
extern void teleport_towards(int oy, int ox, int ny, int nx, bool charge);
extern void teleport_player_to(int ny, int nx);
extern void teleport_player_level(bool voluntary);
extern void steam_mecha_drill_level(void);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, int permanent);
extern bool res_stat(int stat);
extern bool alchemy(void);
extern void do_poly_wounds(void);
extern void fetch(int dir, int wgt, bool require_los);
extern void do_poly_self(void);
extern void mutate_player(void);
extern void do_cmd_rerate(void);
extern void brand_bolts(void);
extern void brand_weapon(void);
extern void edit_monster(int skill);
extern void hack_monster(int skill);
extern void minor_object_improvement(int skill);
extern void major_object_improvement(int skill);
extern void dimen_door(int dis, int fail);
extern bool teleport_swap(int dir);
extern void teleport_to_player(int oy, int ox, int ny, int nx);

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
extern errr my_fgets(FILE *fff, char *buf, huge n);
extern errr my_fputs(FILE *fff, cptr buf, huge n);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, long n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
extern errr check_modification_date(int fd, cptr template_file);
extern void text_to_ascii(char *buf, cptr str);
extern void ascii_to_text(char *buf, cptr str);
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
extern void text_out_to_screen(byte a, cptr str);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void c_roff(byte a, cptr str);
extern void roff(cptr str);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, int len);
extern bool get_string(cptr prompt, char *buf, int len);
extern s16b get_quantity(cptr prompt, int max);
extern s16b get_reload(cptr prompt, int max);
extern char get_check(cptr prompt);
extern bool get_pickup_check(cptr prompt);
extern bool get_com(cptr prompt, char *command);
extern void pause_line(int row);
extern int get_keymap_dir(char ch);
extern void request_command(bool shopping);
extern uint damroll(uint num, uint sides);
extern uint maxroll(uint num, uint sides);
extern bool is_a_vowel(int ch);
extern int color_char_to_attr(char c);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

extern u16b rsqrt(s32b input);
extern int make_metric(int wgt);
extern byte get_angle_to_grid[41][41];
extern int get_angle_to_target(int y0, int x0, int y1, int x1, int dir);
extern void get_grid_using_angle(int angle, int y0, int x0,
	int *ty, int *tx);


/* xtra1.c */
extern void cnv_stat(int val, char *out_val);
extern int modify_stat_value(int value, int amount);
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
extern bool set_shadow(int v);
extern bool set_tim_esp(int v);
extern bool set_tim_invis(int v);
extern bool set_tim_light(int v);
extern bool set_tim_demonspell(int v);
extern bool set_tim_demonhealth(int v);
extern bool set_tim_wormsense(int v);
extern bool set_tim_voorish(int v);
extern bool set_tim_stygian(int v);
extern bool set_tim_muscle(int v);
extern bool set_tim_vigor(int v);
extern bool set_tim_no_tele(int v);
extern bool set_tim_free_act(int v);
extern bool set_tim_anti_magic(int v);
extern bool set_tim_infra(int v);
extern bool set_tim_harding(int v);
extern bool set_tim_invisiblity(int v);
extern bool set_tim_evade(int v);
extern bool set_tim_res(int type, int v);
extern bool set_tim_eyes_research(int v);
extern bool set_tim_reflex_research(int v);
extern bool set_tim_plate_research(int v);
extern bool set_tim_core_research(int v);
extern bool set_tim_spur_research(int v);
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
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note);
extern bool modify_panel(int wy, int wx);
extern bool adjust_panel(int y, int x);
extern bool change_panel(int dir);
extern void verify_panel(void);
extern void ang_sort_aux(vptr u, vptr v, int p, int q);
extern void ang_sort(vptr u, vptr v, int n);
extern sint motion_dir(int y1, int x1, int y2, int x2);
extern int target_dir(char ch);
extern bool target_able(int m_idx, bool use_sight);
extern bool target_okay(void);
extern void target_set_monster(int m_idx);
extern void target_set_location(int y, int x);
extern bool target_set_interactive(u16b mode);
extern bool get_aim_dir(int *dp);
extern bool get_hack_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);
extern cptr look_mon_desc(char *buf, int m_idx);



/* wizard2.c */
extern void strip_name(char *buf, int k_idx);
/* nope */
/*  */

/*
 * Hack -- conditional (or "bizarre") externs
 */

#ifdef SET_UID
# ifndef HAS_USLEEP
/* util.c */
extern int usleep(huge usecs);
# endif /* HAS_USLEEP */
extern void user_name(char *buf, int id);
extern errr user_home(char *buf, int len);
#endif /* SET_UID */


#ifdef ALLOW_REPEAT
/* util.c */
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
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

#if defined(MAC_MPW) || defined(MACH_O_CARBON)
/* main-mac.c, or it's carbonised derivation */
extern u32b _fcreator;
extern u32b _ftype;

# ifdef MACH_O_CARBON
/*
 * gcc -  You have to write it by yourself... or File-Open and double-clicking
 * on savefiles will never work.
 */
extern void fsetfileinfo(cptr path, u32b fcreator, u32b ftype)
# endif
# if defined(MAC_MPW) && defined(CARBON)    
/* MPW - they chose to do something very brain-dead for pathnames :( */
extern void convert_pathname(char *path);
# endif
#endif

