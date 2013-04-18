/* File: externs.h */

/* Purpose: extern declarations (variables and functions) */

/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, term.h, random.h)
 */


/*
 * Automatically generated "variable" declarations
 */

/* cmd1.c */
extern byte item_tester_tval;
extern byte item_tester_sval;
extern bool (*item_tester_hook)(inven_type*);

/* tables.c */
extern int ddd[9];
extern int ddx[10];
extern int ddy[10];
extern s16b adj_val_min[];
extern s16b adj_val_max[];
extern s16b adj_drag[];
extern s16b adj_mag_study[];
extern s16b adj_mag_mana[];
extern s16b adj_mag_fail[];
extern s16b adj_mag_stat[];
extern s16b adj_int_dev[];
extern s16b adj_wis_sav[];
extern s16b adj_dex_dis[];
extern s16b adj_int_dis[];
extern s16b adj_dex_ta[];
extern s16b adj_str_td[];
extern s16b adj_dex_th[];
extern s16b adj_str_th[];
extern s16b adj_chr[];
extern byte blows_table[12][14];
extern owner_type owners[MAX_STORES][MAX_OWNERS];
extern byte rgold_adj[MAX_RACES][MAX_RACES];
extern byte extract_energy[200];
extern u16b normal_table[NORMAL_TABLE_SIZE];
extern cptr ego_item_names[EGO_MAX];
extern s32b ego_item_value[EGO_MAX];
extern s32b player_exp[MAX_PLAYER_LEVEL];
extern player_race race_info[MAX_RACES];
extern player_class class_info[MAX_CLASS];
extern player_background background[MAX_BACKGROUND];
extern spell_type magic_spell[MAX_CLASS-1][64];
extern u32b spell_flags[2][9][2];
extern cptr spell_names[2][64];
extern byte chest_traps[64];
extern byte player_init[MAX_CLASS][3][2];
extern cptr player_title[MAX_CLASS][MAX_PLAYER_LEVEL];

/* variable.c */
extern cptr copyright[5];
extern int cur_version_maj;
extern int cur_version_min;
extern int cur_patch_level;
extern int cur_lite;
extern int old_lite;
extern u16b total_winner;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern bool character_icky;
extern bool character_xtra;
extern u32b randes_seed;
extern u32b town_seed;
extern int command_cmd;
extern int command_old;
extern int command_esc;
extern int command_arg;
extern int command_rep;
extern int command_dir;
extern int command_see;
extern int command_gap;
extern int command_wrk;
extern int command_new;
extern int energy_use;
extern int choice_default;
extern int create_up_stair;
extern int create_down_stair;
extern int death;
extern int find_flag;
extern int msg_flag;
extern s16b cur_hgt;
extern s16b cur_wid;
extern s16b dun_level;
extern s16b object_level;
extern s16b monster_level;
extern u32b turn;
extern u32b old_turn;
extern bool wizard;
extern bool to_be_wizard;
extern bool can_be_wizard;
extern u16b panic_save;
extern u16b noscore;
extern bool inkey_scan;
extern bool inkey_flag;
extern int coin_type;
extern int opening_chest;
extern bool use_graphics;
extern bool use_sound;
extern s16b inven_ctr;
extern s16b inven_weight;
extern s16b equip_ctr;
extern u32b sf_xtra;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern s16b i_max;
extern s16b m_max;
extern bool rogue_like_commands;
extern bool quick_messages;
extern bool other_query_flag;
extern bool carry_query_flag;
extern bool always_pickup;
extern bool always_throw;
extern bool always_repeat;
extern bool use_old_target;
extern bool new_screen_layout;
extern bool equippy_chars;
extern bool depth_in_feet;
extern bool notice_seams;
extern bool use_color;
extern bool use_screen_win;
extern bool use_recall_win;
extern bool use_choice_win;
extern bool compress_savefile;
extern bool hilite_player;
extern bool ring_bell;
extern bool find_cut;
extern bool find_examine;
extern bool find_prself;
extern bool find_bound;
extern bool find_ignore_doors;
extern bool find_ignore_stairs;
extern bool disturb_near;
extern bool disturb_move;
extern bool disturb_enter;
extern bool disturb_leave;
extern bool flush_disturb;
extern bool flush_failure;
extern bool flush_command;
extern bool fresh_before;
extern bool fresh_after;
extern bool fresh_find;
extern bool view_yellow_lite;
extern bool view_bright_lite;
extern bool view_reduce_view;
extern bool view_reduce_lite;
extern bool view_wall_memory;
extern bool view_xtra_memory;
extern bool view_perma_grids;
extern bool view_torch_grids;
extern bool flow_by_sound;
extern bool flow_by_smell;
extern bool track_follow;
extern bool track_target;
extern bool no_haggle_flag;
extern bool shuffle_owners;
extern bool show_inven_weight;
extern bool show_equip_weight;
extern bool show_store_weight;
extern bool plain_descriptions;
extern bool stack_allow_items;
extern bool stack_allow_wands;
extern bool stack_force_notes;
extern bool stack_force_costs;
extern bool dungeon_align;
extern bool dungeon_stair;
extern bool begin_maximize;
extern bool begin_preserve;
extern bool show_spell_info;
extern bool show_health_bar;
extern bool smart_learn;
extern bool smart_cheat;
extern bool recall_show_desc;
extern bool recall_show_kill;
extern bool choice_spells;
extern bool choice_inven_wgt;
extern bool choice_equip_wgt;
extern bool choice_equip_xtra;
extern bool cheat_peek;
extern bool cheat_hear;
extern bool cheat_room;
extern bool cheat_xtra;
extern bool cheat_know;
extern bool cheat_live;
extern int hitpoint_warn;
extern int delay_spd;
extern term *term_screen;
extern term *term_recall;
extern term *term_choice;
extern int feeling;
extern int rating;
extern int good_item_flag;
extern int new_level_flag;
extern int teleport_flag;
extern int teleport_dist;
extern int teleport_to_y;
extern int teleport_to_x;
extern int closing_flag;
extern s16b max_panel_rows, max_panel_cols;
extern int panel_row, panel_col;
extern int panel_row_min, panel_row_max;
extern int panel_col_min, panel_col_max;
extern int panel_col_prt, panel_row_prt;
extern s16b py;
extern s16b px;
extern int target_who;
extern int target_col;
extern int target_row;
extern int health_who;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char player_name[32];
extern char player_base[32];
extern char died_from[80];
extern char history[4][60];
extern char savefile[1024];
extern cave_type *cave[MAX_HGT];
extern char ghost_name[128];
extern inven_type inventory[INVEN_TOTAL];
extern monster_type *m_list;
extern monster_race *r_list;
extern monster_lore *l_list;
extern quest q_list[MAX_Q_IDX];
extern inven_type *i_list;
extern inven_kind *k_list;
extern inven_very *v_list;
extern inven_xtra *x_list;
extern byte tval_to_attr[128];
extern char tval_to_char[128];
extern byte keymap_cmds[256];
extern byte keymap_dirs[256];
extern player_type *p_ptr;
extern player_class *cp_ptr;
extern player_race *rp_ptr;
extern u32b spell_learned1;
extern u32b spell_learned2;
extern u32b spell_worked1;
extern u32b spell_worked2;
extern u32b spell_forgotten1;
extern u32b spell_forgotten2;
extern byte spell_order[64];
extern s16b player_hp[MAX_PLAYER_LEVEL];
extern s16b store_choice_kind[MAX_STORES-2][STORE_CHOICES];
extern store_type store[MAX_STORES];
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_DIR;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
extern cptr ANGBAND_DIR_BONE;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_SAVE;
extern cptr ANGBAND_DIR_PREF;
extern cptr ANGBAND_NEWS;




/*
 * Automatically generated "function declarations"
 */

/* birth.c */
extern void player_birth(void);

/* borg.c */
extern void borg_init(void);
extern void borg_mode(void);

/* cave.c */
extern int distance(int y1, int x1, int y2, int x2);
extern int los(int y1, int x1, int y2, int x2);
extern bool player_can_see_bold(int y, int x);
extern int no_lite(void);
extern void move_cursor_relative(int row, int col);
extern void print_rel(char c, byte a, int y, int x);
extern void lite_spot(int y, int x);
extern void prt_map(void);
extern void forget_lite(void);
extern void update_lite(void);
extern void forget_view(void);
extern void update_view(void);
extern void lite_room(int y1, int x1);
extern void unlite_room(int y1, int x1);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void screen_map(void);

/* cmd1.c */
extern int is_a_vowel(int ch);
extern int index_to_label(int i);
extern int label_to_inven(int c);
extern int label_to_equip(int c);
extern cptr mention_use(int i);
extern cptr describe_use(int i);
extern int get_tag(int *com_val, char tag);
extern void inven_item_charges(int item_val);
extern void inven_item_describe(int i_idx);
extern void combine_pack(void);
extern void inven_item_increase(int i_idx, int num);
extern void inven_item_optimize(int i_idx);
extern void floor_item_increase(int y, int x, int num);
extern void floor_item_optimize(int y, int x);
extern int find_range(int tval, int *i1, int *i2);
extern int weight_limit(void);
extern int inven_check_num(inven_type *i_ptr);
extern int inven_carry(inven_type *i_ptr);
extern bool item_tester_okay(inven_type *i_ptr);
extern void choice_inven(int r1, int r2);
extern void choice_equip(int r1, int r2);
extern void show_inven(int r1, int r2);
extern void show_equip(int s1, int s2);
extern int get_item(int *com_val, cptr pmt, int s1, int s2, bool floor);
extern int verify(cptr prompt, int item);
extern void do_cmd_inven_i(void);
extern void do_cmd_inven_e(void);
extern void do_cmd_inven_w(void);
extern void do_cmd_inven_t(void);
extern void do_cmd_inven_d(void);

/* cmd2.c */
extern void move_rec(int y1, int x1, int y2, int x2);
extern int is_quest(int level);
extern void monster_death(monster_type *m_ptr);
extern bool mon_take_hit(int m_idx, int dam, bool print_fear);
extern void py_attack(int y, int x);
extern void drop_near(inven_type *i_ptr, int chance, int y, int x);
extern void shoot(int item_val, int dir);
extern void py_bash(int y, int x);
extern void take_hit(int damage, cptr hit_from);

/* cmd3.c */
extern void do_cmd_look(void);
extern void do_cmd_examine(void);
extern void do_cmd_view_map(void);
extern void do_cmd_locate(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern int twall(int y, int x, int t1, int t2);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_spike(void);
extern void do_cmd_fire(void);
extern void do_cmd_walk(int pickup);
extern void do_cmd_stay(int pickup);
extern void do_cmd_run(void);
extern void do_cmd_search(void);
extern void do_cmd_rest(void);
extern void do_cmd_feeling(void);
extern void inscribe(inven_type *i_ptr, cptr str);
extern void do_cmd_uninscribe(void);
extern void do_cmd_inscribe(void);
extern void do_cmd_check_artifacts(void);
extern void do_cmd_check_uniques(void);

/* cmd4.c */
extern bool target_able(int m_idx);
extern void target_update(void);
extern bool target_okay(void);
extern bool target_set(void);
extern void confuse_dir(int *dir, int mode);
extern int get_a_dir(cptr prompt, int *dir, int mode);
extern int get_dir(cptr prompt, int *dir);
extern int get_dir_c(cptr prompt, int *dir);
extern void search_on(void);
extern void search_off(void);
extern void disturb(int stop_search, int flush_output);
extern void search(void);
extern void rest_off(void);
extern void panel_bounds(void);
extern void verify_panel(void);
extern void carry(int pickup);
extern void move_player(int dir, int do_pickup);
extern void find_step(void);
extern void find_init(void);
extern void end_find(void);

/* cmd5.c */
extern int spell_chance(int spell);
extern void calc_spells(int stat);
extern void calc_mana(int stat);
extern void do_cmd_browse(void);
extern void do_cmd_study(void);
extern void do_cmd_cast(void);
extern void do_cmd_pray(void);

/* cmd6.c */
extern void do_cmd_powers(void);
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_suicide(void);
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_toggle_search(void);
extern void do_cmd_refill(void);
extern void do_cmd_messages(void);
extern void do_cmd_target(void);
extern void do_cmd_options(void);
extern void do_cmd_prefs(void);
extern void do_cmd_note(void);
extern void do_cmd_save_game(void);
extern void do_cmd_destroy(void);
extern void do_cmd_observe(void);
extern void do_cmd_keymap(void);
extern void process_command(void);
extern void request_command(void);

/* creature.c */
extern void update_mon(int m_idx, bool dist);
extern void update_monsters(bool dist);
extern void process_monsters(void);

/* dungeon.c */
extern void extract_cur_lite(void);
extern void dungeon(void);

/* effects.c */
extern void add_food(int num);
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_aim_wand(void);
extern void do_cmd_use_staff(void);
extern void do_cmd_zap_rod(void);
extern void do_cmd_activate(void);

/* files.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern errr process_pref_file(cptr name);
extern void init_scorefile(void);
extern void nuke_scorefile(void);
extern errr check_time_init(void);
extern errr check_load_init(void);
extern errr check_time(void);
extern errr check_load(void);
extern void read_times(void);
extern void show_news(void);
extern void do_cmd_help(cptr name);
extern cptr title_string(void);
extern int file_character(cptr filename1);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void change_name(void);
extern long total_points(void);
extern void display_scores(int from, int to);
extern void exit_game(void);
extern void exit_game_panic(void);

/* generate.c */
extern void generate_cave(void);

/* init.c */
extern void get_file_paths(void);
extern void init_some_arrays(void);

/* io.c */
extern void fresh(void);
extern void flush(void);
extern void bell(void);
extern void sound(int num);
extern void move_cursor(int row, int col);
extern void text_to_ascii(char *buf, cptr str);
extern void ascii_to_text(char *buf, cptr str);
extern void keymap_init(void);
extern void macro_dump(cptr fname);
extern void macro_add(cptr pat, cptr act, bool cmd_flag);
extern char inkey(void);
extern uint message_num(void);
extern uint message_len(uint age);
extern cptr message_str(uint age);
extern void message_add(cptr msg, int len);
extern void message_new(cptr msg, int len);
extern void msg_print(cptr msg);
extern void message(cptr msg, int mode);
extern void do_cmd_macro(bool cmd_flag);
extern void clear_screen(void);
extern void clear_from(int row);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern int get_check(cptr prompt);
extern int get_com(cptr prompt, char *command);
extern bool askfor_aux(char *buf, int len);
extern bool askfor(char *buf, int len);
extern void pause_line(int row);
extern void save_screen(void);
extern void restore_screen(void);

/* main.c */
extern void play_game(void);
extern void play_game_mac(int ng);

/* melee.c */
extern bool make_attack_normal(int m_idx);
extern bool make_attack_spell(int m_idx);

/* misc.c */
extern void init_seeds(void);
extern void set_seed(u32b seed);
extern void reset_seed(void);
extern int randnor(int mean, int stand);
extern int bit_pos(u32b *test);
extern int damroll(int num, int sides);
extern int maxroll(int num, int sides);
extern int modify_stat(int stat, int amount);
extern void set_use_stat(int stat);
extern int inc_stat(int stat);
extern int dec_stat(int stat, int amount, int permanent);
extern int res_stat(int stat);
extern int stat_index(int stat);
extern int attack_blows(int weight);
extern int tot_dam(inven_type *i_ptr, int tdam, int r_idx);
extern int player_saves(void);
extern void cnv_stat(int my_stat, char *out_val);
extern void cut_player(int c);
extern void stun_player(int s);
extern cptr likert(int x, int y);
extern void put_character(void);
extern void put_stats(void);
extern void display_player(void);
extern void health_redraw(void);
extern void health_track(int m_idx);
extern void check_experience(void);
extern void notice_stuff(void);
extern void handle_stuff(void);

/* mon-desc.c */
extern void lore_do_probe(monster_type *m_ptr);
extern void lore_treasure(monster_type *m_ptr, int num_item, int num_gold);
extern int roff_recall(int r_idx);
extern void do_cmd_query_symbol(void);

/* monster.c */
extern void remove_monster_idx(int i);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void tighten_m_list(void);
extern int m_pop(void);
extern void wipe_m_list(void);
extern int get_mon_num(int level);
extern int place_monster(int y, int x, int r_idx, int slp);
extern int place_ghost(void);
extern int multiply_monster(int m_idx);
extern void place_group(int y, int x, int r_idx, int slp);
extern void alloc_monster(int num, int dis, int slp);
extern int summon_monster(int y1, int x1, int lev);
extern int summon_specific(int y1, int x1, int lev, int type);
extern void monster_desc(char *desc, monster_type *m_ptr, int mode);

/* obj-desc.c */
extern void flavor_init(void);
extern void reset_visuals(void);
extern void inven_known(inven_type *i_ptr);
extern void inven_aware(inven_type *i_ptr);
extern void inven_tried(inven_type *i_ptr);
extern void objdes(char *out_val, inven_type *i_ptr, int pref);
extern void objdes_store(char *buf, inven_type *i_ptr, int mode);

/* object.c */
extern void delete_object_idx(int i);
extern void delete_object(int y, int x);
extern void tighten_i_list(void);
extern void wipe_i_list(void);
extern int i_pop(void);
extern bool item_similar(inven_type *i_ptr, inven_type *j_ptr);
extern int lookup_kind(int tval, int sval);
extern void invwipe(inven_type *i_ptr);
extern void invcopy(inven_type *i_ptr, int k_idx);
extern int m_bonus(int base, int limit, int level);
extern bool make_artifact(inven_type *i_ptr);
extern void apply_magic(inven_type *i_ptr, int level, bool okay, bool good, bool great);
extern void place_object(int y, int x);
extern void place_good(int y, int x, bool great);
extern void acquirement(int y1, int x1, int num, bool great);
extern void place_trap(int y, int x);
extern void place_gold(int y, int x);
extern int get_obj_num(int level, int good);

/* save-old.c */
extern bool load_player(void);

/* save.c */
extern bool save_player(void);

/* signals.c */
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);

/* spells1.c */
extern byte spell_color(int type);
extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);
extern bool projectable(int y1, int x1, int y2, int x2);
extern bool apply_disenchant(int mode);
extern void acid_dam(int dam, cptr kb_str);
extern void elec_dam(int dam, cptr kb_str);
extern void fire_dam(int dam, cptr kb_str);
extern void cold_dam(int dam, cptr kb_str);
extern void poison_gas(int dam, cptr kb_str);
extern bool project(int who, int rad, int y, int x, int dam, int typ, int flg);

/* spells2.c */
extern void aggravate_monsters(int who);
extern int poly_r_idx(int r_idx);
extern void teleport_away(int m_idx, int dis);
extern int mass_genocide(int spell);
extern int genocide(int spell);
extern int speed_monsters(void);
extern int slow_monsters(void);
extern int sleep_monsters2(void);
extern int hp_player(int num);
extern int cure_confusion(void);
extern int cure_blindness(void);
extern int cure_poison(void);
extern int remove_fear(void);
extern int protect_evil(void);
extern void satisfy_hunger(void);
extern int banish_evil(int dist);
extern int probing(void);
extern int dispel_monster(int m_idx, int dam);
extern int dispel_monsters(int damage, bool only_evil, bool only_undead);
extern int turn_undead(void);
extern void warding_glyph(void);
extern void lose_exp(s32b amount);
extern int slow_poison(void);
extern void bless(int amount);
extern void detect_inv2(int amount);
extern void message_pain(int m_idx, int dam);
extern int remove_curse(void);
extern int remove_all_curse(void);
extern int restore_level(void);
extern void self_knowledge(void);
extern void tele_level(void);
extern int sleep_monsters1(int y, int x);
extern int lose_all_info(void);
extern int detect_treasure(void);
extern int detect_magic(void);
extern int detect_invisible(void);
extern int detect_evil(void);
extern int detect_monsters(void);
extern int detection(void);
extern int detect_object(void);
extern int detect_trap(void);
extern void stair_creation(void);
extern int detect_sdoor(void);
extern cptr item_activation(inven_type *i_ptr);
extern bool identify_fully_aux(inven_type *i_ptr);
extern bool enchant(inven_type *i_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool recharge(int num);
extern bool project_hook(int typ, int dir, int dam, int flg);
extern bool fire_bolt(int typ, int dir, int y, int x, int dam);
extern bool line_spell(int typ, int dir, int y, int x, int dam);
extern bool lite_line(int dir, int y, int x);
extern bool drain_life(int dir, int y, int x, int dam);
extern bool wall_to_mud(int dir, int y, int x);
extern bool td_destroy2(int dir, int y, int x);
extern bool disarm_all(int dir, int y, int x);
extern bool td_destroy(void);
extern bool door_creation(void);
extern bool trap_creation(void);
extern bool heal_monster(int dir, int y, int x);
extern bool speed_monster(int dir, int y, int x);
extern bool slow_monster(int dir, int y, int x);
extern bool sleep_monster(int dir, int y, int x);
extern bool confuse_monster(int dir, int y, int x, int plev);
extern bool fear_monster(int dir, int y, int x, int plev);
extern bool poly_monster(int dir, int y, int x);
extern bool clone_monster(int dir, int y, int x);
extern bool teleport_monster(int dir, int y, int x);
extern bool lite_area(int y, int x, int dam, int rad);
extern bool unlite_area(int y, int x);
extern bool fire_ball(int typ, int dir, int ppy, int ppx, int dam_hp, int max_dis);
extern bool starball(int y, int x);
extern bool starlite(int y, int x);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);

/* store.c */
extern s32b item_value(inven_type *i_ptr);
extern void store_enter(int which);
extern void store_acquire(int which, inven_type *i_ptr);
extern void store_shuffle(void);
extern void store_maint(void);
extern void store_init(void);

/* util.c */
extern void delay(int t);
extern void user_name(char *buf, int id);
extern FILE *my_tfopen(cptr file, cptr mode);
extern int my_topen(cptr file, int flags, int mode);

/* wizard.c */
extern int enter_wiz_mode(void);
extern int do_wiz_command(void);



/*
 * Hack -- conditional externs
 */

#ifdef WINDOWS
/* main-win.c */
extern void get_lib_path(char *);
#endif

#ifdef AMIGA
/* util.c */
extern int getuid(void);
extern void umask(int x);
#endif

#ifndef HAS_USLEEP
/* util.c */
extern int usleep(huge microSeconds);
#endif

#ifdef MACINTOSH
/* main-mac.c */
extern int mac_file_character(void);
#endif


/*
 * Hack -- Unused extern declarations
 */

#if 0

/* main-cap.c */
extern int rows, cols;
extern void term_cl(void);
extern void term_ce(void);
extern void curs_set(int vis);
extern void hilite_on(void);
extern void hilite_off(void);
extern void term_cs(int y1, int y2);
extern void term_cm(int x, int y);
extern void term_move(int x1, int y1, int x2, int y2);
extern void Term_terminal_set(void);
extern void Term_terminal_get(void);
extern void Term_terminal_raw(void);
extern void unix_suspend_curses(int go);
extern void Term_curs(int x, int y, int z);
extern void Term_wipe(int x, int y, int w, int h);
extern void Term_text(int x, int y, int n, byte a, cptr s);
extern void Term_scan(int n);
extern void Term_xtra(int n);
extern errr term_nuke(void);
extern errr term_init (void);
extern int term_init (void);
extern void hack(int x, int y, int a, cptr s);
extern int main(int argc, char *argv[]);

/* main-cur.c */
extern errr init_cur(void);
extern void shell_out(void);
extern int system_cmd(cptr p);

/* main-emx.c */
extern errr init_emx(void);

/* main-gcu.c */
extern errr init_gcu(void);

/* main-ibm.c */
extern errr init_ibm(void);

/* main-mac.c */
extern term_data screen;
extern term_data recall;
extern term_data choice;
extern FileFilterYDUPP foldersonlyfilterUPP;
extern DlgHookYDUPP findlibdialoghookUPP;
extern ModalFilterUPP ynfilterUPP;
extern void delay(int x);
extern int mac_file_character(void);
extern void main(void);

/* main-ncu.c */
extern errr init_ncu(void);

/* main-win.c */
extern int stricmp(const char *, const char *);
extern unsigned _cdecl _dos_getfileattr(const char *, unsigned *);
extern term_data screen;
extern term_data recall;
extern term_data choice;
extern void term_window_resize(term_data *td);
extern void term_change_font(term_data *td);
extern void delay(int x);
extern void sleep(unsigned x);
extern void process_menus(WPARAM wParam);
extern LRESULT FAR PASCAL _export AngbandWndProc(HWND hWnd, UINT uMsg, ...);
extern LRESULT FAR PASCAL _export AngbandListProc(HWND hWnd, UINT uMsg, ...);
extern char *get_lib_path(void);
extern int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, ...);

/* main-x11.c */
extern errr init_x11(void);

/* main.c */
extern int main(int argc, char *argv[]);

#endif
