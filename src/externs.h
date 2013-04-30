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
extern byte adj_str_th[A_RANGE];
extern byte adj_str_wgt[A_RANGE];
extern byte adj_str_hold[A_RANGE];
extern byte adj_str_dig[A_RANGE];
extern byte adj_dex_mob[A_RANGE];
extern byte adj_str_blow[A_RANGE];
extern byte adj_str_armor[A_RANGE];
extern byte adj_str_unarmed[A_RANGE];
extern byte adj_int_dis[A_RANGE];
extern byte adj_int_alc[A_RANGE];
extern byte adj_int_map[A_RANGE];
extern byte adj_wis_sav[A_RANGE];
extern byte adj_wis_per[A_RANGE];
extern byte adj_dex_dis[A_RANGE];
extern byte adj_dex_ta[A_RANGE];
extern byte adj_dex_th[A_RANGE];
extern byte adj_dex_safe[A_RANGE];
extern byte adj_dex_blows[A_RANGE];
extern byte adj_con_fix[A_RANGE];
extern byte adj_con_mhp[A_RANGE];
extern byte adj_chr_dev[A_RANGE];
extern byte adj_chr_range[A_RANGE];
extern byte adj_chr_gold[A_RANGE];
extern byte adj_chr_calm[A_RANGE];
extern byte adj_mag_study[A_RANGE];
extern byte adj_mag_mana[A_RANGE];
extern byte adj_mag_extra_mana[A_RANGE];
extern byte adj_mag_fail[A_RANGE];
extern byte adj_mag_stat[A_RANGE];
extern byte weapon_wgt_blows[30];
extern byte extract_energy[200];
extern byte invis_chance[31];
extern s32b player_exp[PY_MAX_LEVEL];
extern player_sex sex_info[SEX_MAX];
extern spell_book instruments[SV_MUSIC_MAX];
extern byte sub_spell_idx[MAX_SUB_TYPE][2];
extern sub_spell_type sub_spell_list[MAX_SUB_SPELL];
extern spell_book books[SV_BOOK_MAX];
extern player_race_special race_special_info[2][RACE_SPECIAL_LEVELS];
extern cptr resist_names[RS_MAX];
extern cptr resist_names_short[RS_MAX];
extern res_cap resist_caps[RS_MAX];
extern cptr color_names[16];
extern cptr stat_names[A_MAX];
extern cptr stat_names_reduced[A_MAX];
extern cptr window_flag_desc[16];
extern option_type options[OPT_NORMAL];
extern option_type options_birth[OPT_BIRTH];
extern option_type options_cheat[OPT_CHEAT];
extern option_type options_squelch[OPT_SQUELCH];
extern byte option_page[OPT_PAGE_MAX][OPT_PAGE_PER];
extern cptr inscrip_text[MAX_INSCRIP];

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
extern u32b seed_alchemy;
extern u32b seed_flavor;
extern u32b seed_town;
extern s16b num_repro;
extern s16b object_level;
extern s16b old_object_level;
extern s16b monster_level;
extern char summon_kin_type;
extern s32b turn;
extern bool use_sound;
extern bool use_graphics;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_nice;
extern bool repair_mflag_show;
extern bool repair_mflag_mark;
extern s16b o_max;
extern s16b o_cnt;
extern s16b mon_max;
extern s16b mon_cnt;
extern s16b t_max;
extern s16b t_cnt;
extern monster_race monster_temp;
extern monster_race monster_temp_fake;
extern monster_lore lore_temp;
extern s16b term_mon_race_idx;	
extern s16b term_mon_unique_idx;	
extern object_type term_object;
extern bool term_obj_real;
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
extern char angband_sound_name[MSG_MAX][16];
extern int view_n;
extern u16b *view_g;
extern int temp_n;
extern u16b *temp_g;
extern byte *temp_y;
extern byte *temp_x;
extern byte (*cave_info)[256];
extern byte (*cave_feat)[MAX_DUNGEON_WID];
extern s16b (*cave_o_idx)[MAX_DUNGEON_WID];
extern s16b (*cave_t_idx)[MAX_DUNGEON_WID];
extern s16b (*cave_m_idx)[MAX_DUNGEON_WID];
extern byte (*cave_cost)[MAX_DUNGEON_WID];
extern byte (*cave_when)[MAX_DUNGEON_WID];
extern room_info_type room_info[DUN_ROOMS];
extern byte dun_room[MAX_ROOMS_ROW][MAX_ROOMS_COL];
extern maxima *z_info;
extern object_type *o_list;
extern monster_type *mon_list;
extern trap_type *t_list;
extern monster_lore *lr_list;
extern monster_lore *lu_list;
extern store_type *store;
extern object_type *inventory;
extern bool *permit_kind_table;
extern byte *chance_kind_table;
extern u32b alloc_kind_total;
extern byte required_tval;
extern byte old_required_tval;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern byte misc_to_attr[256];
extern char misc_to_char[256];
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
extern player_sex *sp_ptr;
extern player_race *rp_ptr;
extern player_class *cp_ptr;
extern ptr_player_race_special rsp_ptr[RACE_SPECIAL_LEVELS];
extern player_other *op_ptr;
extern player_type *p_ptr;
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern feature_type *f_info;
extern char *f_name;
extern desc_type *d_info;
extern char *d_name;
extern char *d_text;
extern object_kind *k_info;
extern char *k_name;
extern artifact_type *a_info;
extern char *a_name;
extern ego_item_type *e_info;
extern char *e_name;
extern weapon_prefix_type *wpx_info;
extern char *wpx_name;
extern armor_prefix_type *apx_info;
extern char *apx_name;
extern trap_widget *w_info;
extern char *w_name;
extern monster_race *r_info;
extern char *r_name;
extern char *r_text;
extern monster_unique *u_info;
extern char *u_name;
extern char *u_text;
extern monster_special *s_info;
extern char *s_name;
extern char *s_text;
extern player_class *c_info;
extern char *c_name;
extern char *c_text;
extern player_race *p_info;
extern char *p_name;
extern hist_type *h_info;
extern char *h_text;
extern owner_type *b_info;
extern char *b_name;
extern byte *g_info;
extern quest_type *q_info;
extern char *q_name;
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_GRAF;
extern cptr ANGBAND_DIR;
extern cptr ANGBAND_DIR_APEX;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_EDIT;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
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
extern bool(*old_get_obj_num_hook)(int k_idx);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern int highscore_fd;
extern bool use_transparency;
extern bool can_save;
extern alchemy_info potion_alch[SV_POTION_MAX];

/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool cave_valid_bold(int y, int x);
extern bool feat_supports_lighting(byte feat);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
extern void map_info_default(int y, int x, byte *ap, char *cp);
extern void move_cursor_relative(int y, int x);
extern void print_rel(char c, byte a, int y, int x);
extern void note_spot(int y, int x);
extern void lite_spot(int y, int x);
extern void prt_map(void);
extern void display_map(int *cy, int *cx);
extern errr vinfo_init(void);
extern void forget_view(void);
extern void update_view(void);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(int x_adjust, int y_adjust);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void cave_set_feat(int y, int x, int feat);
extern int project_path(u16b *gp, int range, int y1, int x1, int y2, int x2, int flg);
extern bool projectable(int y1, int x1, int y2, int x2);
extern void scatter(int *yp, int *xp, int y, int x, int d);
extern void disturb(int stop_search);

/* cmd-attk.c */
extern void py_attack(int y, int x, bool show_monster_name, bool charge);
extern void run_step(int dir);
extern void create_shelf_item(int y, int x, int theme);
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_search(void);
extern void do_cmd_walk(void);
extern void do_cmd_jump(void);
extern void do_cmd_run(void);
extern void do_cmd_hold(void);
extern void do_cmd_stay(void);
extern void do_cmd_rest(void);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);

/* cmd-book.c */
extern bool literate(void);
extern bool spellcaster(void);
extern void print_spells(int book, bool music, int lev, int y, int x);
extern byte count_spells (int book);
extern void do_cmd_browse(void);
extern void do_cmd_study(void);
extern void do_play(int instrument, int lev);
extern void do_cmd_magic(void);

/* cmd-item.c */
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
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_invoke_talisman(void);
extern void do_cmd_activate(void);
extern void do_cmd_use(void);
extern void do_cmd_mix(void);

/* cmd-know.c */
extern void do_cmd_display_character(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_note(void);
extern void do_cmd_monster_list(void);
extern void do_cmd_room_description(void);
extern void do_cmd_item_list(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void do_cmd_room_desc(void);
extern void do_cmd_quest(void);
extern void do_cmd_knowledge(void);
extern void do_cmd_query_symbol(void);

/* cmd-misc.c */
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_alter(void);
extern void do_cmd_racial(void);
extern void do_cmd_view_map(void);
extern void do_cmd_proficiency(void);

/* cmd-util.c */
extern void do_cmd_redraw(void);
extern void options_birth_menu(bool adult);
extern void do_cmd_options(void);
extern void do_cmd_pref(void);
extern void do_cmd_macros(void);
extern void do_cmd_visuals(void);
extern void do_cmd_colors(void);
extern void do_cmd_save_screen_text(void);
extern void do_cmd_save_screen_html(void);
extern void do_cmd_help(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(void);

/* dungeon.c */
extern int value_check_aux1(const object_type *o_ptr);
extern int value_check_aux2(const object_type *o_ptr);
extern void play_game(bool new_game);

/* effects.c */
extern bool hp_player(int num);
extern bool heal_player(int perc, int min);
extern void damage_player(int dam, cptr kb_str);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern bool restore_exp(void);
extern void scramble_stats(void);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, bool permanent);
extern bool res_stat(int stat);
extern bool do_dec_stat(int stat, int amount, bool permanent, bool can_sustain);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern bool set_blind(int v);
extern bool set_confused(int v);
extern bool set_poisoned(int v);
extern bool set_diseased(int v);
extern bool set_taint(int v);
extern bool set_afraid(int v);
extern bool set_paralyzed(int v);
extern bool set_image(int v);
extern bool set_fast(int v);
extern bool set_slow(int v);
extern bool set_shield(int v);
extern bool set_blessed(int v);
extern bool set_safety(int v);
extern bool set_hero(int v);
extern bool set_rage(int v);
extern bool set_protevil(int v);
extern bool set_protchaos(int v);
extern bool set_flaming_hands(int v);
extern bool set_icy_hands(int v);
extern bool set_resilient(int v);
extern bool set_absorb(int v);
extern bool set_tim_see_invis(int v);
extern bool set_tim_invis(int v);
extern bool set_stability(int v);
extern bool set_tim_sp_dur(int v);
extern bool set_tim_sp_dam(int v);
extern bool set_tim_sp_inf(int v);
extern bool set_tim_bravery(int v);
extern void nullify_invis(void);
extern bool set_tim_infra(int v);
extern bool set_tim_stealth(int v);
extern bool set_tim_res(int type, int v);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(int v);
extern void display_player_status(void);

/* files.c */
extern void text_screenshot(cptr name);
extern void html_screenshot(cptr name);
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern errr process_pref_file_command(char *buf);
extern errr process_pref_file(cptr name);
extern void reset_visuals(bool unused);
extern errr check_time(void);
extern errr check_time_init(void);
extern errr check_load(void);
extern errr check_load_init(void);
extern long total_points(void);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3);
extern errr file_character(cptr name, bool full);
extern bool show_file(cptr name, cptr what, int line, int mode);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void display_scores(int from, int to);
extern void display_scores_aux(int from, int to, int note, high_score *score);
extern errr enter_score(time_t death_time);
extern void top_twenty(void);
extern errr predict_score(void);
extern void close_game(void);
extern void exit_game_panic(void);
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);

/* generate.c */
extern void generate_cave(void);

/* info.c */
extern void display_player_equippy(int y, int x);
extern void display_player(byte mode);
extern void analyze_weapon(const object_type *o_ptr);
extern void analyze_armor(const object_type *o_ptr);
extern void analyze_ammo(const object_type *o_ptr);
extern void screen_object(const object_type *o_ptr, bool real);
extern void list_object(const object_type *o_ptr, int mode);
extern void display_object_history(const object_type *o_ptr);
extern bool history_interesting(const object_type *o_ptr);
extern void display_koff(const object_type *o_ptr);

/* init2.c */
extern void init_file_paths(char *path);
extern void init_angband(void);
extern void cleanup_angband(void);

/* load.c */
extern errr rd_savefile(void);

/* melee1.c */
extern bool make_attack_normal(int m_idx);

/* melee2.c */
extern void process_monsters_action(byte minimum_energy);
extern void process_monsters_status(void);

/* monster1.c */
extern void describe_monster(int r_idx, int u_idx, bool spoilers);
extern void screen_roff(int r_idx, int u_idx);
extern void roff_top(int r_idx, int u_idx);
extern void display_roff(int r_idx, int u_idx);
extern void display_visible(void);

/* monster2.c */
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_mon_list(void);
extern s16b mon_pop(void);
extern errr get_mon_num_prep(void);
extern s16b get_mon_num(int level);
extern void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode);
extern void lore_do_probe(const monster_type *m_ptr);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, const object_type *j_ptr);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern s16b player_place(int y, int x);
extern s16b monster_place(int y, int x, const monster_type *n_ptr);
extern bool place_monster_aux(int y, int x, int r_idx, int u_idx, bool slp, bool grp, byte mode);
extern bool place_monster(int y, int x);
extern bool alloc_monster(int dis);
extern bool summon_specific(int y1, int x1, int lev, int type);
extern bool multiply_monster(int m_idx, bool clone);
extern void message_pain(int m_idx, int dam);
extern void update_smart_learn(int m_idx, int what);
extern void mon_exp(int r_idx, int s_idx, int u_idx, u32b *exint, u32b *exfrac);
extern bool monster_alive(bool true_life, const monster_type *m_ptr);

/* monster3.c */
extern cptr monster_text(int r_idx, int u_idx);
extern cptr monster_name_race(int r_idx);
extern cptr monster_name_idx(int r_idx, int s_idx, int u_idx);
extern cptr monster_name(const monster_type *m_ptr);
extern monster_race *get_monster_real(const monster_type *m_ptr);
extern monster_race *get_monster_fake(int r_idx, int s_idx, int u_idx);
extern monster_lore *get_lore_idx(int r_idx, int u_idx);
extern void lore_learn(const monster_type *m_ptr, int mode, u32b what, bool unseen);

/* object1.c */
extern byte ring_col[SV_RING_MAX];
extern byte potion_col[SV_POTION_MAX];
extern void flavor_init(void);
extern void alchemy_init(void);
extern void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern void object_desc_store(char *buf, size_t max, const object_type *o_ptr, int pref, int mode);
extern char index_to_label(int i);
extern s16b label_to_inven(int c);
extern s16b label_to_equip(int c);
extern cptr mention_use(int slot);
extern cptr describe_use(int slot);
extern bool item_tester_okay(const object_type *o_ptr);
extern int scan_floor(int *items, int size, int y, int x, int mode);
extern void display_inven(void);
extern void display_equip(void);
extern void show_inven(void);
extern void show_equip(void);
extern void toggle_inven_equip(void);
extern bool get_item(int *cp, cptr pmt, cptr str, int mode);
extern void show_floor(int *floor_list, int floor_num);
extern void strip_name(char *buf, int k_idx);

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
extern void artifact_aware(artifact_type *a_ptr);
extern void artifact_known(artifact_type *a_ptr);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern s32b object_value(const object_type *o_ptr);
extern bool destroy_check(object_type *o_ptr);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, const object_type *j_ptr);
extern s16b lookup_kind(int tval, int sval);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool real_depth);
extern bool make_object(object_type *j_ptr, bool good, bool great, bool real_depth);
extern bool make_typed(object_type *j_ptr, byte tval, bool good, bool great, bool real_depth);
extern bool make_mimic(object_type *j_ptr, byte a, char c);
extern bool make_gold(object_type *j_ptr, int coin_type);
extern void object_history(object_type *o_ptr, byte origin, s16b r_idx, s16b s_idx, s16b u_idx);
extern void stack_histories(object_type *o_ptr, const object_type *j_ptr);
extern s16b floor_carry(int y, int x, const object_type *j_ptr);
extern void drop_near(const object_type *j_ptr, int chance, int y, int x, bool room_only);
extern void acquirement(int y1, int x1, int num, bool great, bool real_depth);
extern void place_object(int y, int x, bool good, bool great);
extern void place_gold(int y, int x);
extern void place_secret_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void place_random_door(int y, int x);
extern void place_chest(int y, int x);
extern void place_quest_chest(int y, int x);
extern void distribute_charges(object_type *o_ptr, object_type *q_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern void alchemy_describe(char *buf, size_t max, int sval);
extern void inven_item_charges(int item);
extern void inven_item_describe(int item);
extern void inven_item_increase(int item, int num);
extern void inven_item_optimize(int item);
extern void floor_item_charges(int item);
extern void floor_item_describe(int item);
extern void floor_item_increase(int item, int num);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(const object_type *o_ptr);
extern s16b inven_carry(const object_type *o_ptr);
extern s16b inven_takeoff(int item, int amt);
extern void inven_drop(int item, int amt);
extern void combine_pack(void);
extern void reorder_pack(void);
extern bool make_fake_artifact(object_type *o_ptr, int a_idx);
extern void create_quest_item(int ny, int nx);

/* object3.c */
extern bool weapon_p(const object_type *o_ptr);
extern s16b wield_slot(const object_type *o_ptr);
extern bool wearable_p(const object_type *o_ptr);
extern byte object_attr(const object_type *o_ptr);
extern void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_flags_random(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern byte object_resist(const object_type *o_ptr, int res_type);
extern byte object_resist_known(const object_type *o_ptr, int res_type);
extern void weapon_slays(const object_type *o_ptr, byte *slays);
extern void weapon_slays_known(const object_type *o_ptr, byte *slays);
extern s16b object_weight(const object_type *o_ptr);
extern s16b object_to_h(const object_type *o_ptr);
extern byte object_dd(const object_type *o_ptr);
extern byte object_ds(const object_type *o_ptr);
extern s16b object_ac(const object_type *o_ptr);
extern byte object_material(const object_type *o_ptr);
extern byte bow_might(const object_type *o_ptr);
extern byte bow_range(const object_type *o_ptr);
extern bool object_hates_acid(const object_type *o_ptr);
extern bool object_hates_elec(const object_type *o_ptr);
extern bool object_hates_fire(const object_type *o_ptr);
extern bool object_hates_cold(const object_type *o_ptr);
extern bool object_hates_rust(const object_type *o_ptr);
extern bool object_hates_rot(const object_type *o_ptr);

/* powers.c */
extern info_entry power_info[POW_MAX];
extern int apply_sp_mod(int value, int modifier);
extern bool do_power(int idx, int sub, int dir, int beam, int dlev, int llev, int ilev, bool mods, bool *obvious);

/* quest.c */
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

/* spells1.c */
extern int apply_resistance(int dam, byte res);
extern bool resist_effect(byte res);
extern s16b poly_r_idx(int r_idx, int power);
extern void teleport_away(int m_idx, int dis);
extern void teleport_player(int dis);
extern void shift_player(int x_adjust, int y_adjust);
extern void teleport_player_to(int ny, int nx);
extern void teleport_monster_to(int m_idx, int ny, int nx);
extern void teleport_player_level(void);
extern void dimen_door(int dis, int fail);
extern void take_hit(int dam, cptr kb_str);
extern void acid_dam(int dam, cptr kb_str);
extern void elec_dam(int dam, cptr kb_str);
extern void fire_dam(int dam, cptr kb_str);
extern void cold_dam(int dam, cptr kb_str);
extern void rust_dam(int dam, cptr kb_str);
extern void rot_dam(int dam, cptr kb_str);
extern bool apply_disenchant(void);
extern bool project(int who, int rad, int y, int x, int dam, int typ, int flg);

/* spells2.c */
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern bool curse_minor(void);
extern bool lose_all_info(void);
extern void set_recall(void);
extern void phlogiston(void);
extern bool detect_traps(int animate, int x_adjust, int y_adjust);
extern bool detect_doors(int animate, int x_adjust, int y_adjust);
extern bool detect_stairs(int animate, int x_adjust, int y_adjust);
extern bool detect_treasure(int animate, int x_adjust, int y_adjust);
extern bool detect_objects_gold(int animate);
extern bool detect_objects_normal(int animate);
extern bool detect_objects_magic(int animate);
extern bool detect_monsters_normal(int animate);
extern bool detect_monsters_invis(int animate);
extern bool detect_monsters_evil(int animate);
extern bool detect_force(int animate, int x_adjust, int y_adjust);
extern bool detect_furniture(int animate, int x_adjust, int y_adjust);
extern bool detect_life(int animate);
extern bool detect_all(int animate);
extern void stair_creation(void);
extern bool enchant_spell(int num_hit, int num_ac);
extern bool brand_weapon(byte weapon_type, int brand_type, bool add_plus);
extern void ident_aux(int item);
extern bool ident_spell(void);
extern void identify_pack(void);
extern bool identify_fully(void);
extern bool analyse_item(void);
extern bool recharge(int num);
extern bool hypercharge(void);
extern bool project_los(int typ, int dam);
extern void aggravate_monsters(int who);
extern void genocide(void);
extern void mass_genocide(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);
extern void lite_area(int dam, int rad);
extern void unlite_area(int dam, int rad);
extern bool strike(int typ, int y, int x, int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern void fire_ball_combo(int t1, int t2, int t3, int t4, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool lite_line(int dir, int dam);
extern bool starlite_line(int dir, int dam);
extern bool wall_to_mud(int dir);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool door_creation(void);
extern bool wall_creation(void);
extern bool trap_creation(int power);
extern bool magic_lock(void);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(int plev);
extern bool item_tester_hook_spellbooks(const object_type *o_ptr);
extern bool item_tester_hook_bookmusic(const object_type *o_ptr);

/* squelch.c */
extern void do_cmd_squelch(void);
extern bool squelch_itemp(object_type *o_ptr);
extern void do_squelch_item(object_type *o_ptr);
extern void destroy_squelched_items(void);

/* store.c */
extern void do_cmd_store(void);
extern void store_shuffle(int which);
extern void store_maint(int which);
extern void store_init(int which);

/* traps.c */
extern cptr trap_name(int w_idx, int mode);
extern void wipe_t_list(void);
extern void delete_trap(int y, int x);
extern void hit_trap(int y, int x);
extern bool do_disarm_trap(int y, int x);
extern void compact_traps(int size);
extern void place_decoration(int y, int x, int decoration);
extern s16b trap_place(int y, int x, const trap_type *x_ptr);
extern bool place_trap_dungeon(int y, int x);
extern bool place_trap_chest(int y, int x);
extern bool place_trap_monster(u32b typ, int y, int x);
extern bool place_trap_player(int y, int x);
extern bool place_lock(int y, int x, bool visible, byte type);
extern bool warding_glyph(byte type);
extern bool mon_glyph_check(int m_idx, int y, int x);

/* util.c */
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_clear (void);
extern void repeat_check(void);
extern errr path_parse(char *buf, size_t max, cptr file);
extern errr path_build(char *buf, size_t max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern FILE *my_fopen_temp(char *buf, size_t max);
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
extern void text_to_ascii(char *buf, size_t len, cptr str);
extern void ascii_to_text(char *buf, size_t len, cptr str);
extern int macro_find_exact(cptr pat);
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
extern void message(u16b message_type, s16b extra, cptr msg);
extern void message_format(u16b message_type, s16b extra, cptr fmt, ...);
extern void message_flush(void);
extern void screen_save(void);
extern void screen_load(void);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void text_out_to_file(byte attr, cptr str);
extern void text_out_to_file_indent(byte attr, cptr str);
extern void text_out_to_screen(byte a, cptr str);
extern void text_out(cptr str);
extern void text_out_c(byte a, cptr str);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, size_t len);
extern bool get_string(cptr prompt, char *buf, size_t len);
extern s16b get_quantity(cptr prompt, int max);
extern bool get_check(cptr prompt);
extern bool get_com(cptr prompt, char *command);
extern void pause_line(int row);
extern void request_command(bool shopping);
extern int damroll(int num, int sides);
extern int maxroll(int num, int sides);
extern bool is_a_vowel(int ch);
extern int color_char_to_attr(char c);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

/* xtra1.c */
extern byte modify_stat_value(byte value, int amount);
extern void health_track(int m_idx);
extern void monster_track(int r_idx, int u_idx);
extern void artifact_track(int a_idx);
extern void object_kind_track(int k_idx);
extern void object_actual_track(const object_type *o_ptr);
extern byte calc_blows(const object_type *o_ptr, bool full);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);

/* xtra2.c */
extern void check_experience(void);
extern void monster_death(int m_idx);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note);
extern void display_room_info(int room);
extern void describe_room(bool force_full);
extern void verify_panel(void);
extern bool ang_mon_sort_comp_hook(const void *u, const void *v, int a, int b);
extern void ang_mon_sort_swap_hook(void *u, void *v, int a, int b);
extern void ang_sort(void *u, void *v, int n);
extern void saturate_mon_list(monster_list_entry *who, int *count, bool allow_base, bool spoil);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern int target_dir(char ch);
extern bool target_okay(int range);
extern void target_set_monster(int m_idx, int range);
extern bool target_set_interactive(int mode, int to_hit, int object_to_hit, int range);
extern bool get_aim_dir(int *dp, int to_hit, int object_to_hit, int range);
extern bool get_rep_dir(int *dp);
extern bool get_proficiency_dir(int *dp, int mapping);
extern bool confuse_dir(int *dp);
extern void do_player_death(void);

/*
 * Hack -- conditional (or "bizarre") externs
 */
 
#ifdef XML_HELP

/* xmlbulp.c */
extern int bulp_stricmp(char *s1, char *s2);

/* help.c */
extern u32b open_help(char *file);
extern u32b render_xml_file(char *file, bool clear, bool help_text, bool persist);

#endif 

#ifdef SET_UID
# ifndef HAVE_USLEEP
/* util.c */
extern int usleep(unsigned long usecs);
# endif
extern void user_name(char *buf, size_t len, int id);
#endif

#ifdef ALLOW_DEBUG

/* wizard.c */
extern void do_cmd_debug(void);

#endif /*ALLOW_DEBUG */

#ifdef ALLOW_BORG

/* Borg.c */
extern void do_cmd_borg(void);
# ifdef ALLOW_BORG_GRAPHICS
extern void init_translate_visuals(void);
# endif /* ALLOW_BORG_GRAPHICS */

#endif
