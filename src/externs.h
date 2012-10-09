/* extern declarations (variables and functions) */
/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, term.h, random.h)
 */

/*
 * Automatically generated "variable" declarations
 */

/* compres1.c */
extern int lzw_compress_file(int action, const char *infilename, const char *outfilename);

/* compres2.c */
extern int rle_compress_file(int action, const char *infilename, const char *outfilename);

/* tables.c */
extern s16b ddd[9];
extern s16b ddx[10];
extern s16b ddy[10];
/* jk */
extern debug_flag_type debug_flag[32];
extern cptr dirstr[10];
extern cptr knowstr[10];
extern cptr slidingstr[10];
extern cptr arena_welcome [7][4];
extern cptr arena_leave_items [7][4];
extern tactic_info_type tactic_info[9];
extern move_info_type move_info[9];
extern cptr object_flag_names[3][64];

extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];
extern char hexsym[16];
extern byte adj_val_min[];
extern byte adj_val_max[];
extern byte adj_mag_mana[];
extern byte adj_mag_fail[];
extern byte adj_mag_stat[];
extern byte adj_chr_gold[];
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
extern store_table_type store_accepts[MAX_STORES][STORE_CHOICES];

extern byte extract_energy[200];
extern s32b player_exp[PY_MAX_LEVEL];
extern byte teach_class_pref[MAX_CLASS][MAX_CLASS];
extern byte teach_race_pref[MAX_RACES][MAX_CLASS];
extern byte teach_stat_pref[MAX_CLASS][4];
extern player_sex sex_info[MAX_SEXES];
extern player_race race_info[MAX_RACES];
extern player_class class_info[MAX_CLASS];
extern u32b spell_flags[2][9][2];
extern cptr spell_names[2][64];
extern cptr player_title[MAX_CLASS][PY_MAX_LEVEL/5];
extern cptr color_names[16];
extern cptr stat_names[6];
extern cptr stat_names_reduced[6];
cptr window_flag_desc[32];
extern option_type options[];

extern flavor_type flavor[MAX_FLAVORS];
extern cptr syllables[MAX_SYLLABLES];

/* variable.c */
extern cptr copyright[5];
extern u32b debuglevel;
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
extern char sf_lastfile_name[1024];
extern bool arg_wizard;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern bool character_icky;
extern bool character_xtra;
/* jk */
extern s16b command_dir_slide;
extern char cmd_sliding;
extern s16b sliding_dir;
extern s16b max_reading;

extern s16b command_see;
extern s16b command_gap;
extern s16b command_wrk;
extern s16b command_new;
extern s16b energy_use;
extern s16b choose_default;
extern bool create_up_stair;
extern bool create_down_stair;
extern bool msg_flag;
extern bool alive;
extern bool death;
extern bool suicide;
/* jk */
extern bool used_stairs;

extern s16b cur_hgt;
extern s16b cur_wid;

extern s16b num_repro;
extern s16b object_level;
extern s16b monster_level;
extern s16b baselevel;
extern s16b sublevel;
extern s32b turn;
extern s32b old_turn;
extern bool wizard;
extern bool to_be_wizard;
extern bool can_be_wizard;
extern u32b wizard_target;
extern u16b total_winner;
extern u16b panic_save;
extern u16b noscore;
extern s16b signal_count;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern s16b coin_type;
extern bool opening_chest;
extern bool arg_graphics;
extern bool arg_sound;
extern bool use_graphics;
extern bool use_sound;

/* jk */
extern bool force_templates;
extern char ANGBAND_ERRLOG_FILE[1024];
/*@null@*/ extern FILE *errlog;
extern FILE *wizardfile;
extern ghost_type ghost_info[MAX_GHOSTS];
extern s16b error_idx;
extern s16b error_line;
extern bool read_options;
extern char ANGBAND_OPTION_FILE[1024];
extern char prev_message[250];
extern u16b prev_message_cnt;
extern byte required_tval;
extern char required_race;

extern bool scan_monsters;
extern bool scan_objects;
extern s16b inven_nxt;
extern s16b inven_cnt;
extern s16b equip_cnt;
extern s16b i_nxt;
extern s16b mn_nxt;
extern s16b i_max;
extern s16b mn_max;
extern s16b i_top;
extern s16b mn_top;
/* jk */
extern s16b t_max;
extern s16b is_max;

extern bool rogue_like_commands;
extern bool quick_messages;
extern bool other_query_flag;
/* jk */
extern bool display_coords;
extern bool print_experience_advance;
extern bool carry_cursed_flag;

extern bool always_pickup;
extern bool always_throw;
extern bool always_repeat;
extern bool use_old_target;
extern bool show_equip_label;
extern bool depth_in_feet;
extern bool use_color;
/* jk */
extern bool compress_savefile;
extern bool kill_savefile_interrupt;
extern bool remove_levelfiles;
extern bool smooth_scroll_panels;
extern bool number_hit_messages;
extern bool show_key_help;

extern bool hilite_player;
extern bool ring_bell;
extern bool find_ignore_stairs;
extern bool find_ignore_doors;
extern bool find_cut;
extern bool find_examine;
extern bool disturb_near;
extern bool disturb_move;
extern bool disturb_enter;
extern bool disturb_leave;
extern bool disturb_panel;
extern bool disturb_other;
extern bool flush_disturb;
extern bool flush_failure;
extern bool flush_command;
extern bool fresh_before;
extern bool fresh_after;
extern bool fresh_message;
extern bool save_messages;
extern bool alert_hitpoint;
extern bool alert_failure;
extern bool corpse_messages;
extern bool fear_messages;
extern bool drop_messages;
extern bool ask_before_traps;
extern bool view_yellow_lite;
extern bool view_bright_lite;
/* jk */
extern bool view_all_squares;

extern bool view_granite_lite;
extern bool view_perma_grids;
extern bool view_torch_grids;
extern bool flow_by_sound;
extern bool flow_by_smell;
extern bool track_follow;
extern bool track_target;
extern bool no_haggle_flag;
/* jk */
extern bool kill_cursed_floor;
extern bool kill_cursed_pack;
extern bool pickup_add_to_ammo;
extern bool ask_for_other_ammo;
extern bool stack_ignore_logs;
extern bool stacking_wipes_logs;
extern bool show_spell_numbers;
extern bool show_full_name_on_destroy;
extern bool color_known_items;
extern bool auto_open;
extern bool auto_target_only_monster;
extern bool create_corpses;
extern bool good_store_items;
extern bool pick_up_gold;
extern bool pick_up_absorbable;

extern bool shuffle_owners;
extern bool show_inven_weight;
/* jk */
extern bool show_floor_weight;
extern bool show_equip_weight;
extern bool show_store_weight;
extern bool stack_allow_items;
extern bool stack_allow_corpses;
extern bool stack_force_notes;
extern bool stack_force_costs;
extern bool view_reduce_lite;
extern bool view_reduce_view;
extern bool optimize_display;
extern bool optimize_various;
extern bool save_levels;
extern bool scum_always;
extern bool scum_sometimes;
/* jk */
extern bool scum_verygood;

extern bool dungeon_align;
extern bool dungeon_connected;
extern bool generate_large_levels;
extern bool show_health_bar;
extern bool monster_flee_exits;
extern bool monster_know_exits;
extern bool smart_learn;
extern bool smart_cheat;
extern bool recall_show_desc;
extern bool recall_show_kill;
extern bool show_unkilled;
extern bool use_mirror_debug;
extern bool use_mirror_around;
extern bool use_mirror_recent;
extern bool use_mirror_normal;
extern bool use_mirror_choose;
extern bool use_mirror_spells;
extern bool use_recall_recent;
extern bool use_choice_normal;
extern bool use_choice_choose;
extern bool use_choice_spells;
extern bool show_choose_info;
extern bool show_choose_prompt;
extern bool show_choose_weight;
extern bool show_choose_label;
extern bool cheat_spell_info;
extern bool cheat_peek;
extern bool cheat_hear;
extern bool cheat_room;
extern bool cheat_xtra;
extern bool cheat_know;
extern bool cheat_live;
extern bool cheat_hitpoints;
extern bool cheat_mode;
extern bool cheat_numeric_skills;
extern s16b hitpoint_warn;
extern s16b delay_spd;
/* jk */
extern bool term_initialized;

extern bool store_built;
extern s16b feeling;
extern s16b rating;
extern bool monsters_with_artifacts;
extern bool good_item_flag;
extern bool new_level_flag;
extern bool closing_flag;
extern s16b panel_max_rows, panel_max_cols;
extern s16b panel_min_row, panel_max_row;
extern s16b panel_min_col, panel_max_col;
extern s16b panel_prt_col, panel_prt_row;
extern s16b py;
extern s16b px;
extern s16b target_who;
extern s16b target_col;
extern s16b target_row;
extern s16b health_who;
extern monster_move_type mmove[9];
extern s16b player_uid;
extern s16b player_euid;
extern s16b player_egid;
extern char player_name[32];
extern char player_base[32];
extern char died_from[80];
extern coord last_kills[25];
extern char history[MAX_HIST][70];
extern char savefile[1024];
/* jk */
s16b teach_stat[6];
char teacher[4][80];
extern char levelfile[1024];
extern bool been_in_arena;
extern s32b arena_reward;
extern s32b arena_previous_monsters;
extern s32b arena_monsters;
extern s32b arena_monster_level;
extern s16b arena_visit_level[PY_MAX_LEVEL+1];
extern u32b compress_length;

extern s16b lite_n;
extern s16b lite_y[LITE_MAX];
extern s16b lite_x[LITE_MAX];
extern s16b view_n;
extern s16b view_y[VIEW_MAX];
extern s16b view_x[VIEW_MAX];
extern u16b view_g[VIEW_MAX];
extern s16b temp_n;
extern s16b old_lite_n;
extern s16b temp_y[TEMP_MAX];
extern s16b temp_x[TEMP_MAX];
extern u16b old_lite_g[TEMP_MAX];
/* jk */
extern s16b tmp_x[TEMP_MAX];
extern s16b tmp_y[TEMP_MAX];
extern s16b tmp_i[TEMP_MAX];

extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern bool *macro__cmd;
extern char *macro__buf;
extern s16b quark__num;
extern cptr *quark__str;
/* jk - from 16 to 32 */
extern u32b message__next;
extern u32b message__last;
extern u32b message__head;
extern u32b message__prev;
extern u32b message__tail;
extern u32b *message__ptr;
extern char *message__buf;
extern byte *message__cnt;
extern term *angband_term[8];
extern char angband_term_name[8][16];
extern byte angband_color_table[256][4];
extern char angband_sound_name[SOUND_MAX][16];
extern s16b i_fast[MAX_I_IDX];
extern s16b mn_fast[MAX_M_IDX];
/* jk */
extern s16b t_fast[MAX_TR_IDX];
extern level_info_type level_info[MAX_LEVEL];
extern dungeon_info_type dungeon;

extern object_type *i_list;
extern spell_set_type *s_list;
extern monster_type *mn_list;
/* jk */
extern trap_item_type *t_list;
extern item_set_type *is_list;

extern quest q_list[MAX_Q_IDX];
/* jk */
extern u16b num_stores;
extern store_type store[MAX_STORES_LEVEL];
extern object_type *inventory;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_race_size;
extern byte misc_to_attr[MAX_FLAVORS];
extern char misc_to_char[MAX_FLAVORS];
extern alloc_entry *alloc_race_table;
extern byte tval_to_attr[128];
extern char tval_to_char[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
extern byte color_table[256][4];
extern player_other *op_ptr;
extern player_type *p_ptr;
extern player_sex *sp_ptr;
extern player_race *rp_ptr;
extern player_class *cp_ptr;
extern s16b player_hp[PY_MAX_LEVEL];
extern u32b level_reached[PY_MAX_LEVEL];
extern vault_type *v_info;
extern char *v_name;
extern char *v_text;
extern u32b v_name_size;
extern u32b v_text_size;
extern u16b v_number;

extern feature_type *f_info;
extern char *f_name;
extern char *f_text;
extern u32b f_name_size;
extern u32b f_text_size;
extern u16b f_number;
/* jk */
u16b f_info_index[MAX_F_IDX];

extern object_kind *k_info;
extern char *k_name;
extern char *k_text;
extern u32b k_name_size;
extern u32b k_text_size;
extern u16b k_number;

extern artifact_type *a_info;
extern char *a_name;
extern char *a_text;
extern u32b a_name_size;
extern u32b a_text_size;
extern u16b a_number;

extern ego_item_type *e_info;
extern char *e_name;
extern char *e_text;
extern u32b e_name_size;
extern u32b e_text_size;
extern u16b e_number;
/* jk */
extern trap_type *t_info;
extern char *t_name;
extern char *t_text;
extern u32b t_name_size;
extern u32b t_text_size;
extern u16b t_number;

extern spell_type *s_info;
extern char *s_name;
extern char *s_text;
extern u32b s_name_size;
extern u32b s_text_size;
extern u16b s_number;

extern monster_race *r_info;
extern char *r_name;
extern char *r_text;
extern u32b r_name_size;
extern u32b r_text_size;
extern u32b r_name_size_total;
extern u32b r_text_size_total;
extern u16b r_number;
extern u16b r_number_total; /* include ghosts */

extern cptr ANGBAND_SYS;
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
/* jk */
extern cptr ANGBAND_DIR_TEMP;
extern bool item_tester_full;
extern byte item_tester_tval;
extern bool (*item_tester_hook)(object_type *i_ptr);
extern bool (*ang_sort_comp)(vptr u, vptr v, s16b a, s16b b);
extern void (*ang_sort_swap)(vptr u, vptr v, s16b a, s16b b);
extern bool (*get_mon_num_hook)(s16b r_idx);
extern s16b summon_specific_type;
extern bool (*get_obj_num_hook)(s16b k_idx);
extern bool (*get_obj_num_hook2)(s16b k_idx);
extern event_type event[MAX_EVENTS];

/*
 * Function Declarations
 */

/* birth.c */
extern void player_birth(void);

/* cave.c */
extern void point_stair_to_level(s16b x, s16b y, s16b to_baselevel, s16b to_sublevel);
extern void get_stair_target(s16b x, s16b y, s16b *new_baselevel, s16b *new_sublevel);
extern errr vinfo_init(void);
extern u16b distance(s16b x1, s16b y1, s16b x2, s16b y2);
extern bool los(s16b x1, s16b y1, s16b x2, s16b y2);
extern bool player_can_see_bold(s16b x, s16b y);
extern bool no_lite(void);
extern void move_cursor_relative(s16b col, s16b row);
extern void print_rel(char c, byte a, s16b x, s16b y);
extern void remember_grid(s16b x, s16b y);
extern void note_spot(s16b x, s16b y);
extern void lite_spot(s16b x, s16b y);
extern void lite_spot_detect(s16b x, s16b y);
extern void prt_map(void);
extern void do_cmd_view_map(bool interactive);
extern void forget_lite(void);
extern void update_lite(void);
extern void forget_view(void);
extern void update_view(void);
extern void forget_flow(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void mmove2(s16b *x, s16b *y, s16b x1, s16b y1, s16b x2, s16b y2);
extern bool projectable(s16b x1, s16b y1, s16b x2, s16b y2);
extern sint project_path(u16b *gp, int range, int x1, int y1, int x2, int y2, int flg);
extern void scatter(s16b *xp, s16b *yp, s16b x, s16b y, s16b d, s16b m);
extern void health_track(s16b m_idx);
extern void monster_race_track(s16b r_idx);
extern void object_kind_track(s16b r_idx);
extern void disturb(s16b stop_search, s16b flush_output);
extern bool is_quest(s16b level);
/* jk */
extern bool ang_sort_comp_hook_longs(vptr u, vptr v, s16b a, s16b b);
extern void ang_sort_swap_hook_longs(vptr u, vptr v, s16b a, s16b b);
extern u16b get_f_idx(u16b mtyp, u16b styp);
extern s16b get_f_idx_tolerant(u16b mtyp, u16b styp);
extern void set_grid_idx(s16b x, s16b y,u16b f_idx, byte type, u32b flags);
extern u16b set_grid_type(s16b x, s16b y,u16b mtyp, u16b styp, byte type, u32b flags);
extern s16b wall_art(s16b styp);
extern bool inner_wall(s16b mtyp, s16b styp);
extern bool hidden_treasure(s16b x, s16b y);
extern bool treasure(s16b x, s16b y);
extern bool known_treasure(s16b x, s16b y);
extern void wipe_old_level(s16b level);

/* jk */
extern s16b objects_on_floor(s16b x, s16b y);
extern s16b floor_free_index(s16b x, s16b y);
extern bool clean_enough_floor(s16b x, s16b y);
extern bool clean_enough_floor_emergency(s16b x, s16b y);
extern bool valid_grid_bold(s16b x,s16b y);
extern bool clean_enough_floor_absorb(s16b x, s16b y,
                                      object_type *i_ptr,bool emergency);
extern s16b objects_on_floor_absorb(object_type *i_ptr,s16b x, s16b y);
extern bool wiz_forget_some_map(s16b chance);
extern void lava_cool_down(void);

/* cmd1.c */
extern bool test_hit_fire(s16b chance, s16b ac, s16b vis);
extern bool test_hit_norm(s16b chance, s16b ac, s16b vis);
/* jk - msg_show added, to keep from showing unneeded messages in */
/* do_cmd_wiz_rateweapon */
extern s16b critical_shot(s16b weight, s16b plus, s16b dam, bool msg_show);
extern s16b critical_norm(s16b weight, s16b plus, s16b dam, bool msg_show);
extern s16b tot_dam_aux(object_type *i_ptr, s16b tdam, monster_type *m_ptr);
/* jk */
extern void find_one_trap(s16b x, s16b y);
extern void find_other_trap(s16b x, s16b y);
extern void find_all_other_traps(s16b x, s16b y);

extern void search(void);
extern void carry(s16b pickup, bool specific);
extern void test_new_object(object_type *i_ptr);
extern s16b py_attack(s16b x, s16b y);
extern void move_player(s16b dir, s16b do_pickup, bool voluntary);
extern void run_step(s16b dir);
extern void do_cmd_run(void);
/* jk */
extern void crush_cursed_items_here(s16b x, s16b y);
extern bool worthless(object_type *i_ptr);
extern bool test_fire_brand_extra(void);
extern bool test_elec_brand_extra(void);
extern bool test_cold_brand_extra(void);
extern bool test_acid_brand_extra(void);

/* cmd2.c */
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_search(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
/* jk */
extern bool twall(s16b x, s16b y);

extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_spike(void);
extern void do_cmd_walk(s16b pickup);
extern void do_cmd_hold(void);
extern void do_cmd_stay(void);
extern void do_cmd_rest(void);
extern void do_cmd_fire(void);
extern void do_cmd_throw(void);
/* jk */
extern bool open_something(s16b x, s16b y, s16b item);
extern s16b drop_how(object_type *i_ptr);
extern s16b breakage_chance(object_type *i_ptr, bool hit_body);
extern s16b what_dir1(s16b x, s16b y, s16b nx, s16b ny);
extern s16b what_dir(s16b x, s16b y, s16b nx, s16b ny);

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
/* jk */
extern bool could_2h(object_type *i_ptr);
extern bool must_2h(object_type *i_ptr);
extern void do_cmd_change_tactic(bool up);
extern void do_cmd_change_movement(bool up);

/* cmd4.c */
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void do_cmd_system_command(void);
extern void do_cmd_note(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(void);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void do_cmd_check(void);
extern void do_cmd_check_artifacts(cptr filename, bool interaction);
extern void do_cmd_check_items(cptr filename, bool interaction);
extern void do_cmd_check_uniques(cptr filename, bool interaction);
extern void do_cmd_check_spells(cptr filename, bool interaction);
extern void do_cmd_check_progress(cptr filename, bool interaction);
extern void do_cmd_check_kills(cptr filename, bool interaction);
extern void do_cmd_check_intrinsics(cptr filename, bool interaction);
/* jk */
extern void read_text_options_file(cptr filename);
extern void read_text_options(void);
extern void write_text_options(void);

/* cmd5.c */
extern void do_cmd_browse(void);
extern void do_cmd_study(void);
extern void do_cmd_cast(void);
extern void do_cmd_cast_spell(void);
extern void do_cmd_pray(void);
/* jk */
extern s16b page_chance(s16b spellno);
extern void print_spells(s16b *index, s16b count);
extern void read_spell(object_type *i_ptr, s16b item);
extern bool exec_page(s16b spellno);
extern bool exec_spell(s16b spellno);
extern void extra_new_spell_info(char *p, s16b spellno);

/* cmd6.c */
extern void do_cmd_eat_food(void);
extern void do_cmd_quaff_potion(void);
extern void do_cmd_read_scroll(void);
extern void do_cmd_zap(void);
extern void do_cmd_activate(void);
/* jk - these 2 functions did already exist */
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern s16b get_rod_charge(object_type *i_ptr);

/* dungeon.c */
extern void play_game(bool new_game);

/* files.c */
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern s16b tokenize(char *buf, s16b num, char **tokens);
extern void display_player(s16b mode, bool interactive);
extern void print_equippy(void);
extern errr file_character(cptr name, bool full);
extern errr process_pref_file_aux(char *buf);
extern errr process_pref_file(cptr name);
extern errr check_time_init(void);
extern errr check_load_init(void);
extern errr check_time(void);
extern errr check_load(void);
extern void read_times(void);
extern errr show_file(cptr name, cptr what);
extern void do_cmd_help(cptr name);
extern bool browse_file(cptr name, bool start_on_last_line, cptr what);
extern void process_player_name(bool sf);
extern void get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(bool with_level);
extern long total_points(void);
extern void display_scores(s16b from, s16b to);
extern void close_game(bool with_level);
extern void exit_game_panic(void);
extern void signals_ignore_tstp(void);
extern void signals_handle_tstp(void);
extern void signals_init(void);
extern void dump_stack(FILE *fff);

/* generate.c */
extern void generate_cave(void);
extern void place_traps_door(s16b x, s16b y, s16b level,
                             s16b chance_one, s16b chance_all);
extern bool kind_is_chest(s16b k_idx);
extern void alloc_stairs(s16b typ, s16b num, s16b walls, s16b town);
extern void place_main_up_stair(s16b x, s16b y,bool deep);
extern void place_main_down_stair(s16b x, s16b y,bool deep);
extern void new_player_spot(bool in_town);
extern void place_wall(s16b x, s16b y);
extern void allocate_sublevel(s16b sublevel);
extern bool ang_sort_comp_hook_longs(vptr u, vptr v, s16b a, s16b b);
extern void ang_sort_swap_hook_longs(vptr u, vptr v, s16b a, s16b b);

/* gentown.c */
extern void town_gen(void);
extern void town_recrowd_arena(void);

/* special.c */
extern void move_special_location(s16b mlevel, s16b slevel, s16b oldx, s16b oldy,
                                  s16b newx, s16b newy);
extern bool arena_monsters_left(void);
extern void kill_arena_monsters(void);

/* init-txt.c */
extern errr init_v_info_txt(FILE *fp, char *buf);
extern errr init_f_info_txt(FILE *fp, char *buf);
extern errr init_k_info_txt(FILE *fp, char *buf);
extern errr init_a_info_txt(FILE *fp, char *buf);
extern errr init_e_info_txt(FILE *fp, char *buf);
/* jk */
extern errr init_t_info_txt(FILE *fp, char *buf);
extern errr init_s_info_txt(FILE *fp, char *buf);
extern errr init_r_info_txt(FILE *fp, char *buf);

/* init1.c */
extern void init_file_paths(char *path);

/* init2.c */
extern void init_some_arrays(void);
extern void init_angband(void);

/* melee1.c */
extern bool make_attack_normal(s16b m_idx);

/* melee2.c */
extern bool make_attack_spell(s16b m_idx);
extern void process_monsters(void);

/* monster1.c */
extern void screen_roff(s16b r_idx);
extern void display_roff(s16b r_idx);

/* monster2.c */
extern void delete_monster_idx(s16b i);
extern void monster_desc(char *desc, monster_type *m_ptr, u16b mode);
extern void delete_monster(s16b x, s16b y);
extern void compact_monsters(s16b size, bool inven_too);
extern void wipe_mn_list(void);
extern s16b mn_pop(void);
extern s16b get_mon_num(s16b level);
extern void lore_treasure(s16b m_idx, s16b num_item, s16b num_gold);
extern void lore_do_probe(s16b m_idx);
extern void update_mon(s16b m_idx, bool dist);
extern void update_monsters(bool dist);
extern bool place_monster_aux(s16b x, s16b y, s16b r_idx, bool slp,
                              bool grp, s16b extra_drop, s16b max_group);
extern bool place_monster_one(s16b x, s16b y, s16b r_idx, bool slp, s16b extra_drop,
                              bool no_check);
extern errr get_mon_num_prep(void);
extern bool place_monster(s16b x, s16b y, bool slp, bool grp, s16b extra_drop);
extern bool alloc_monster(s16b dis, s16b slp, s16b town);
extern bool summon_specific(s16b x1, s16b y1, s16b lev, s16b type, s16b max_group);
extern bool summon_specific_okay(s16b r_idx);
extern bool multiply_monster(s16b m_idx);
extern void update_smart_learn(s16b m_idx, s16b what);
extern cptr item_activation(object_type *i_ptr);
extern void message_pain(s16b m_idx, s16b dam);
extern bool summon_ghost(s16b x1, s16b y1);

/* monster3.c */
extern void create_monster_inventory(s16b m_idx, s16b drop);
extern void create_ghost_inventory(s16b m_idx);
extern s16b item_set_this_monster(s16b m_idx);
extern s16b items_in_set(s16b is_idx);
extern s16b which_item_in_set(s16b is_idx, s16b i_list_idx);
extern s16b object_in_monster_inventory(s16b i_idx);
extern void monster_inven_increase(s16b m_idx, s16b is_idx, s16b item_in_index_set, s16b amt);
extern void monster_inven_optimize(s16b m_idx, s16b is_idx);
extern s16b monster_inven_absorb(s16b m_idx, object_type *i_ptr);
extern s16b monster_inven_carry(s16b m_idx, object_type *i_ptr);
extern void show_monster_inventory(s16b is_idx);
extern void test_monster_inven_damage(s16b m_idx, s16b typ, s16b dam);
extern bool correct_item(s16b r_idx, object_type *i_ptr);

/* object1.c */
extern bool object_has_flavor(s16b k_idx);
extern void object_material_init();
extern void flavor_init(void);
extern void races_init(void);
extern void object_desc(char *buf, object_type *i_ptr, bool pref, s16b mode);
extern void object_desc_store(char *buf, object_type *i_ptr, bool pref, s16b mode);
extern bool identify_fully_aux(object_type *i_ptr, bool only_log);
extern bool object_easy_know(s16b k_idx);
extern bool item_easy_know(object_type *i_ptr);
extern s16b index_to_label(s16b i);
extern s16b label_to_inven(s16b c);
extern s16b label_to_equip(s16b c);
extern s16b wield_slot(object_type *i_ptr);
extern cptr mention_use(s16b i);
extern cptr describe_use(s16b i);
extern void display_inven(void);
extern void display_equip(void);
extern void display_floor(s16b x, s16b y);
extern void show_inven(void);
extern void show_equip(void);
extern void expand_item_log(item_log_type *l_ptr, char *result);
extern s16b count_items(s16b *first_item, bool equip, bool inven, bool floor);
extern s16b weight_color(void);
extern bool get_item(s16b *cp, s16b *amt, cptr pmt, bool equip, bool inven, bool floor);
extern bool item_tester_okay(object_type *i_ptr);
/* jk */
extern void show_floor(s16b x, s16b y);
extern char *object_desc_str(char *t, cptr s);

/* object2.c */
extern void item_charges(s16b item);
extern void item_describe(s16b item, s16b x, s16b y);
extern errr get_obj_num_prep(void);
extern s16b get_obj_num(s16b level);
extern void object_known(object_type *i_ptr);
extern void object_aware(object_type *i_ptr);
extern void object_tried(object_type *i_ptr);
extern void sense_inventory(void);
extern void recharge_inventory(void);
extern void charge_staff(object_type *i_ptr);
extern void charge_wand(object_type *i_ptr);
extern void burn_light(void);

extern s32b object_value(object_type *i_ptr);
extern s32b object_value_real(object_type *i_ptr);
extern bool drop_near(object_type *i_ptr, s16b break_number, s16b x, s16b y,
                      s16b actionflag, bool hit_body, bool hit_wall);
extern void apply_magic(object_type *i_ptr, s16b lev, bool okay, bool good, bool great);
extern void acquirement(s16b x1, s16b y1, s16b num, bool great, bool wizard);
/* jk */
extern bool has_spell(object_type *i_ptr, s16b i);
extern void set_spell(object_type *i_ptr, s16b i);
extern void clr_spell(object_type *i_ptr, s16b i);
extern void test_equipment(object_type *i_ptr, bool easy);
extern bool need_tries(object_type *i_ptr);
extern void create_item(object_type *i_ptr, bool good, bool great, bool exact_kind);
extern void place_object(s16b x, s16b y, bool good, bool great, bool exact_kind);
extern void place_object_known(s16b *x, s16b *y, bool good, bool great,
                               bool exact_kind, bool wizard);
extern void create_gold_item(object_type *i_ptr);
extern void place_gold(s16b x, s16b y);
extern void place_gold_known(s16b *x, s16b *y);
extern void give_monster_gold(s16b m_idx, s32b gold);

/* jk */
/* function did exist, just wasn't mentioned here */
extern bool make_artifact(object_type *i_ptr, bool wizard);
extern bool make_artifact_special(object_type *i_ptr, bool wizard);
/* jk */
extern bool new_scatter(object_type *i_ptr, s16b x, s16b y, s16b *nx, s16b *ny);
extern void display_koff(int k_idx);

/* object3.c */
extern void init_trap(trap_item_type *tr_ptr);
extern s16b t_pop(void);
extern s16b is_pop(void);
s16b trap_experience(trap_type *t_ptr);
extern void wipe_is_list(void);
extern void wipe_t_list(void);
extern void compact_traps(void);
extern void compact_item_sets_special(void);
extern bool get_trap(trap_item_type *tr_ptr,s16b trap);
extern bool set_trap(trap_item_type *tr_ptr,s16b trap,bool on);
extern s16b num_traps_ptr(trap_item_type *tr_ptr, s16b found_status);
extern s16b num_traps_xy(s16b x, s16b y, s16b found_status);
extern s16b first_trap(trap_item_type *tr_ptr);
extern s16b first_found_trap(trap_item_type *tr_ptr);
extern bool player_execute_trap(trap_item_type *tr_ptr, s16b item, s16b x, s16b y, bool bash);
extern bool monster_execute_trap(s16b m_idx, trap_item_type *tr_ptr, s16b item, s16b x, s16b y, bool bash);
extern bool describe_trap(char *buf, trap_item_type *tr_ptr);
extern bool inspect_trap_chest(object_type *i_ptr);
extern void place_trap(s16b x, s16b y, s16b level,
                       s16b chance_one, s16b chance_all);
extern void set_traps_chest(object_type *i_ptr,s16b level,
                       s16b chance_one, s16b chance_all);
extern bool set_traps(trap_item_type *tr_ptr, s16b *p1valinc, s16b level,
                      u32b flags, s16b chance_one, s16b chance_all);
extern bool trap_found_xy(s16b x, s16b y, s16b trap);
extern bool trap_found_ptr(trap_item_type *tr_ptr, s16b trap);
extern void set_trap_found_ptr(trap_item_type *tr_ptr, s16b trap);
extern void set_trap_notfound_ptr(trap_item_type *tr_ptr, s16b trap);
extern s16b get_random_trap(trap_item_type *tr_ptr, s16b found_status);
extern bool do_player_trap_call_out(void);
extern bool make_trap_from_spell(s16b level, bool complex);
extern void handle_floor_with_traps(trap_item_type *tr_ptr, s16b x, s16b y);

/* object4.c */
/* jk */
extern vptr get_item_pointer(s16b item);
extern vptr get_item_pointer_xy(s16b item,s16b x, s16b y);
extern vptr get_item_pointer_floor(s16b item);
extern object_type *get_item_pointer_floor_xy(s16b item,s16b x, s16b y);
extern bool floor_item(s16b item);
extern bool floor_item_xy(s16b item, s16b x, s16b y);
extern void wipe_floor_items_traps(s16b x, s16b y);
extern void set_floor_item(s16b item, s16b value);
extern void set_floor_item_xy(s16b item, s16b value, s16b x, s16b y);
extern s16b get_floor_item(s16b item);
extern s16b get_floor_item_xy(s16b item, s16b x, s16b y);

extern void process_objects(void);
extern void combine_pack(void);
extern void reorder_pack(void);
extern void invwipe(object_type *i_ptr);
extern void invcopy(object_type *i_ptr, s16b k_idx);
extern void item_increase(s16b item, s16b num, s16b x, s16b y);
extern void item_optimize(s16b item, s16b x, s16b y);
extern bool inven_carry_okay(object_type *i_ptr);
extern s16b inven_carry(object_type *i_ptr,s16b amt);
/* jk */
extern s16b floor_carry(object_type *i_ptr,s16b x, s16b y);
extern void optimize_floor(s16b x, s16b y);

extern void delete_object_idx(s16b i_idx);
extern void delete_object(s16b x, s16b y, s16b item);
extern void compact_objects(s16b size,bool monsters_too);
extern void wipe_i_list(void);
extern s16b i_pop(void);
extern s16b s_pop(void);
extern void reset_visuals(void);
extern void object_flags(object_type *i_ptr, u64b *f1, u64b *f2, u64b *f3);
extern void object_absorb(object_type *i_ptr, object_type *j_ptr, s16b amt);
extern bool object_similar(object_type *i_ptr, object_type *j_ptr);
extern s16b lookup_kind(s16b tval, s16b sval);

/* save.c */
extern void compress_progress_indicator(int doing_compress,
                                        unsigned long int compress_progress,
                                        unsigned long int compress_total);
extern bool save_player(void);
extern bool load_player(void);
extern bool wr_level(void);
extern void make_extension(char *string, s16b number);
extern void write_info_header(char *file, u16b number, u32b name_size, u32b text_size);
extern void write_info_data(char *file, char *array, u16b number, u16b itemsize,
                            char *name, u32b name_size, char *text, u32b text_size);
extern void rm_level(s16b level);
extern void rm_savefile(void);
extern void wr_ghost(FILE *fff, s16b r_idx);
extern void remove_ghost_file(s16b num);

/* load.c */
extern errr rd_savefile_new(void);
/* jk */
extern errr rd_level(void);
extern errr read_info_header(char *file, u16b *number, u16b max_num,
                             u32b *name_size, u32b *text_size);
extern errr read_info_data(char *file, char *array, u16b number, u16b itemsize,
                    char *name, u32b name_size, char *text, u32b text_size);
extern bool make_new_ghost(void);
extern bool rd_ghost_files(void);

/* spells1.c */
extern s16b poly_r_idx(s16b r_idx);
extern void teleport_away(s16b m_idx, s16b dis);
extern void teleport_player(s16b dis);
extern bool teleport_monster_to(s16b m_idx, s16b x, s16b y);
extern void teleport_player_to(s16b nx, s16b ny);
extern void teleport_player_level(void);
extern void take_hit(s16b damage, cptr kb_str);
extern bool poison_dam(s16b dam);
extern void acid_dam(s16b dam, cptr kb_str);
extern void elec_dam(s16b dam, cptr kb_str);
extern void fire_dam(s16b dam, cptr kb_str);
extern void cold_dam(s16b dam, cptr kb_str);
extern bool inc_stat(s16b stat, bool only_temporary);
extern bool dec_stat(s16b stat, s16b amount, s16b mode);
extern bool res_stat(s16b stat);
extern bool apply_disenchant(s16b mode);
extern bool project(project_who_type *who, s16b rad, s16b x, s16b y, s16b dam, s16b typ, u16b flg);

/* jk */
extern bool set_cold_destroy(object_type *i_ptr);
extern bool set_acid_destroy(object_type *i_ptr);
extern bool set_fire_destroy(object_type *i_ptr);
extern bool set_elec_destroy(object_type *i_ptr);

/* spells2.c */
extern bool hp_player(s16b num);
extern void warding_glyph(void);
extern bool do_dec_stat(s16b stat, s16b mode);
extern bool do_res_stat(s16b stat);
extern bool do_inc_stat(s16b stat, bool only_temporary);
extern void identify_pack(void);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool lose_all_info(void);
/* jk */
extern bool lose_some_info(s16b chance);
extern void forget_item(object_type *i_ptr);

extern bool detect_treasure_xy(s16b x, s16b y);
extern bool detect_treasure(void);
extern bool detect_magic(void);
extern bool detect_invisible(void);
extern bool detect_evil(void);
extern bool detect_monsters(void);
extern bool detection(void);
extern bool detect_object_xy(s16b x, s16b y);
extern bool detect_object(void);
extern bool detect_trap_xy(s16b x, s16b y);
extern bool detect_trap(void);
extern bool detect_sdoor_xy(s16b x, s16b y);
extern bool detect_sdoor(void);
extern void stair_creation(void);
extern void brand_weapon(void);
extern bool enchant(object_type *i_ptr, s16b n, s16b eflag);
extern bool enchant_spell(s16b num_hit, s16b num_dam, s16b num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern bool recharge(s16b num);
/* jk */
extern bool item_tester_hook_recharge(object_type *i_ptr);

extern bool speed_monsters(void);
extern bool slow_monsters(void);
extern bool slow_monsters_extra(s16b level);
extern bool sleep_monsters(void);
extern bool sleep_monsters_extra(s16b level);
extern bool aggravate_monsters(project_who_type *who, s16b how_much);
extern bool genocide(void);
extern bool mass_genocide(void);
extern bool probing(void);
extern bool banish_evil(s16b dist);
extern bool dispel_evil(s16b dam);
extern bool dispel_undead(s16b dam);
extern bool dispel_monsters(s16b dam);
extern bool turn_undead(void);
extern void destroy_area(s16b x1, s16b y1, s16b r, bool full);
extern void earthquake(s16b cx, s16b cy, s16b r);
extern void lite_room(s16b x1, s16b y1);
extern void unlite_room(s16b x1, s16b y1);
extern bool lite_area(s16b dam, s16b rad);
extern bool unlite_area(s16b dam, s16b rad);
extern bool fire_ball(s16b typ, s16b dir, s16b dam, s16b rad);
extern bool fire_bolt(s16b typ, s16b dir, s16b dam);
extern bool fire_beam(s16b typ, s16b dir, s16b dam);
extern bool fire_bolt_or_beam(s16b prob, s16b typ, s16b dir, s16b dam);
extern bool lite_line(s16b dir);
extern bool drain_life(s16b dir, s16b dam);
extern bool wall_to_mud(s16b dir);
extern bool destroy_door(s16b dir);
extern bool disarm_trap(s16b dir);
extern bool heal_monster(s16b dir);
extern bool speed_monster(s16b dir);
extern bool slow_monster(s16b dir);
extern bool sleep_monster(s16b dir);
extern bool confuse_monster(s16b dir, s16b plev);
extern bool fear_monster(s16b dir, s16b plev);
extern bool poly_monster(s16b dir);
extern bool clone_monster(s16b dir);
extern bool teleport_monster(s16b dir);
extern bool door_creation(void);
extern bool trap_creation(project_who_type *who, s16b x, s16b y);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
/* JK */
extern bool ident_trap(s16b dir);

/* store.c */
extern s16b store_flavor_type(s16b type);
extern void do_cmd_store(void);
extern void store_shuffle(void);
extern void store_maint(s16b which);
extern void store_init(s16b which);
extern void create_town_store(s16b which, s16b x, s16b y);
extern void create_store(s16b type, s16b x, s16b y);

/* util.c */
#ifndef __DJGPP__
extern void delay(int t);
#else
extern void delay(unsigned t);
#endif
extern errr path_build(char *buf, int max, cptr path, cptr file);
extern errr path_parse(char *buf, int max, cptr file);
extern errr path_temp(char *buf, int max);
extern FILE *my_fopen(cptr file, cptr mode);
extern errr my_fgets(FILE *fff, char *buf, huge n);
extern errr my_fputs(FILE *fff, cptr buf, huge n);
extern errr my_fclose(FILE *fff);
extern errr my_fseek(FILE *fff, long offset, int mode);
extern u32b my_ftell(FILE *fff);
extern s32b my_flength(FILE *fff);
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr file, cptr what);
extern s16b fd_make(cptr file, int mode);
extern s16b fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, huge n);
extern errr fd_chop(int fd, huge n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
extern void flush(void);
extern void bell(cptr reason);
extern void sound(int num);
extern void move_cursor(int col, int row);
extern void text_to_ascii(char *buf, cptr str);
extern void ascii_ttext(char *buf, cptr str);
extern sint macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern char inkey(void);
extern cptr quark_str(s16b num);
extern s16b quark_add(cptr str);
extern u32b message_num(void);
extern byte message_cnt(u32b age);
extern cptr message_str(u32b age);
extern cptr message_str_clean(u32b age);
/* jk - show added to prevent showing when loading */
extern void message_add(cptr msg);
extern void message_flush(void);
extern void msg_print(cptr msg);
extern void msg_format(cptr fmt, ...);
extern void add_msg(cptr msg);
extern void add_msg_format(cptr fmt, ...);
extern void clear_screen(void);
extern void c_roff(byte a, cptr str);
extern void roff(cptr str);
extern void clear_from(int row);
extern void c_put_str(byte attr, cptr str, int col, int row);
extern void put_str(cptr str, int col, int row);
extern void prt(cptr str, int col, int row);
extern bool askfor_aux(char *buf, int len);
extern bool get_string(cptr prompt, char *buf, int len);
extern bool get_check(cptr prompt);
extern bool get_com(cptr prompt, char *command);
extern s32b get_quantity(cptr prompt, u32b max, u32b standard);
extern void pause_line(int row);
extern void request_command(void);
extern bool is_a_vowel(int ch);
extern bool cmp_strngs(cptr string1,cptr string2);
extern cptr istrstr(cptr string1,cptr string2);
extern void dlog(u32b debugflag, char *fmt, ...);
extern void debug_dump_screen_part(int size);
extern bool stat_sort_comp(vptr u, vptr v, s16b a, s16b b);
extern void stat_sort_swap(vptr u, vptr v, s16b a, s16b b);

/* wizard2.c */
extern void do_cmd_toggle_wizard_mode(void);
extern void wiz_create_crash(void);
extern bool enter_wizard_mode(void);

#ifdef ALLOW_WIZARD
extern void do_cmd_wizard(void);
#endif
extern void do_cmd_wiz_cure_all();
extern void wiz_stat_statistics();

/* xtra1.c */
extern s32b set_depth(u16b to_baselevel, u16b to_sublevel);
extern void get_depth(s32b depth, u16b *new_baselevel, u16b *new_sublevel);
extern s16b weight_limit(void);
extern void cnv_stat(s16b val, char *out_val);
extern s16b modify_stat_value(s16b value, s16b amount);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void window_stuff(void);
extern void handle_stuff(void);

/* xtra2.c */
extern bool set_blind(s16b v);
extern bool set_confused(s16b v);
extern bool set_poisoned(s16b v);
extern bool set_afraid(s16b v);
extern bool set_paralyzed(s16b v);
extern bool set_image(s16b v);
extern bool set_fast(s16b v);
extern bool set_slow(s16b v);
extern bool set_shield(s16b v);
extern bool set_blessed(s16b v);
extern bool set_hero(s16b v);
extern bool set_shero(s16b v);
extern bool set_protevil(s16b v);
extern bool set_invuln(s16b v);
extern bool set_tim_invis(s16b v);
extern bool set_tim_infra(s16b v);
extern bool set_oppose_acid(s16b v);
extern bool set_oppose_elec(s16b v);
extern bool set_oppose_fire(s16b v);
extern bool set_oppose_cold(s16b v);
extern bool set_oppose_pois(s16b v);
extern bool set_stun(s16b v);
extern bool set_cut(s16b v);
extern bool set_food(s32b v);
/* jk */
extern bool set_sliding(s16b v);
extern bool set_reflecting(s16b delta_v);
extern bool set_throat(s16b v);
extern bool set_fire(s16b v);
extern bool set_cold(s16b v);
extern bool set_acid(s16b v);
extern bool set_elec(s16b v);
extern bool set_lift(s16b v);

/* jk - function existed already */
extern s16b get_coin_type(monster_race *r_ptr);

extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void set_arena_state(byte new_state);
extern void lose_exp(s32b amount);
extern void digest_food(void);
extern void monster_death(project_who_type *who, s16b m_idx);
extern bool mon_take_hit(project_who_type *who, s16b m_idx, s16b dam, bool *fear, cptr note);
extern void panel_bounds(void);
extern void verify_panel(void);
extern void ang_sort_aux(vptr u, vptr v, s16b p, s16b q);
extern void ang_sort(vptr u, vptr v, s16b n);
extern void ang_sort_swap_distance(vptr u, vptr v, s16b a, s16b b);
extern bool ang_sort_comp_distance(vptr u, vptr v, s16b a, s16b b);
extern s16b target_dir(char ch);
extern bool target_able(s16b m_idx);
extern bool target_okay(void);
extern s16b target_pick(s16b x1, s16b y1, s16b dx, s16b dy);
extern bool target_set(void);
extern bool get_aim_dir(s16b *dp);
extern bool get_rep_dir(s16b *dp);


/*
 * Hack -- conditional (or "bizarre") externs
 */
#ifdef SET_UID
/* util.c */
extern void user_name(char *buf, int id);
#endif

#ifndef HAS_MEMSET
/* util.c */
extern char *memset(char*, int, huge);
#endif

#ifndef HAS_STRICMP
/* util.c */
extern int stricmp(cptr a, cptr b);
#endif

#ifdef MACINTOSH
/* main-mac.c */
/* extern void delay(int x); */
/* extern void main(void); */
#endif

#ifdef WINDOWS
/* main-win.c */
/* extern void delay(int x); */
/* extern int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, ...); */
#endif

#ifdef ACORN
/* main-acn.c */
/* extern void delay(int x); */
#endif
