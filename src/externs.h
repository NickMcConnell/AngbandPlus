// File: externs.h

// Purpose: extern declarations (variables and functions)



/*

 * Note that some files have their own header files (random.h)

 */





/*

 * Variable declarations

 */

// tables.cpp

extern s16b ddd[9];

extern s16b dx[10];

extern s16b dy[10];

extern s16b ddx[10];

extern s16b ddy[10];

extern char hexsym[16];

extern byte adj_val_min[];

extern byte adj_val_max[];

extern byte adj_mag_study[];

extern byte adj_mag_sp[];

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

extern byte extract_energy[200];

extern s32b player_exp[PY_MAX_LEVEL];

extern player_race race_info[MAX_RACES];

extern player_class class_info[MAX_CLASS];

extern player_magic magic_info[MAX_CLASS];

extern u32b spell_flags[2][9][2];

extern char *spell_names[2][64];

extern byte chest_traps[64];

extern char *color_names[16];

extern char *stat_names[6];

extern char *stat_names_reduced[6];

extern char *stat_names_full[6];

extern option_type options[];



// variable.cpp

extern char *copyright;

extern byte version_major;

extern byte version_minor;

extern byte version_patch;

extern byte version_type;

extern byte sf_major;

extern byte sf_minor;

extern byte sf_patch;

extern byte sf_extra;

extern bool arg_wizard;

extern bool arg_fiddle;

extern bool character_generated;

extern bool character_dungeon;

extern bool character_loaded;

extern bool character_saved;

extern bool character_icky;

extern bool character_xtra;

extern u32b seed_flavor;

extern u32b seed_town;

extern s16b command_see;

extern bool create_up_stair;

extern bool create_down_stair;

extern bool force_enter_store;

extern bool msg_flag;

extern bool alive;

extern bool death;

extern s16b cur_hgt;

extern s16b cur_wid;

extern s16b dun_level;

extern s16b num_repro;

extern s32b game_turn;

extern bool wizard;

extern bool to_be_wizard;

extern bool can_be_wizard;

extern u16b total_winner;

extern u16b panic_save;

extern s16b signal_count;

extern bool inkey_scan;

extern s16b coin_type;

extern bool opening_chest;

extern s16b inven_cnt;

extern s16b equip_cnt;

extern CItem *inventory;

extern bool other_query_flag;

extern bool carry_query_flag;

extern bool show_equip_label;

extern bool ring_bell;

extern bool alert_hitpoint;

extern bool alert_failure;

extern bool show_inven_weight;

extern bool show_equip_weight;

extern bool show_store_weight;

extern bool stack_allow_items;

extern bool stack_allow_wands;

extern bool stack_force_notes;

extern bool stack_force_costs;

extern bool generate_artifacts;

extern byte game_speed;

extern byte hitpoint_warn;

extern bool new_level_flag;

extern char player_name[32];

extern char died_from[80];

extern int char_idx;

extern s16b lite_n;

extern CCoord lite[LITE_MAX];

extern s16b view_n;

extern CCoord view[VIEW_MAX];

extern s16b temp_n;

extern byte temp_y[TEMP_MAX];

extern byte temp_x[TEMP_MAX];

extern CGrid *cave[MAX_HGT];

extern quest q_list[MAX_Q_IDX];

extern store_type *store;

extern s16b alloc_kind_size;

extern s16b *alloc_kind_index;

extern kind_entry *alloc_kind_table;

extern s16b alloc_race_size;

extern s16b *alloc_race_index;

extern race_entry *alloc_race_table;

extern CPlayer *p_ptr;

extern player_race *rp_ptr;

extern player_class *cp_ptr;

extern player_magic *mp_ptr;

extern bool spell_learned[64];

extern bool spell_worked[64];

extern bool spell_forgotten[64];

extern byte spell_order[64];

extern header *v_head;

extern vault_type *v_info;

extern char *v_name;

extern char *v_text;

extern header *f_head;

extern feature_type *f_info;

extern char *f_name;

extern CObjectKind *k_info;

extern header *a_head;

extern artifact_type *a_info;

extern char *a_name;

extern header *e_head;

extern ego_item_type *e_info;

extern char *e_name;

extern char *e_text;

extern header *r_head;

extern CMonsterRace *r_info;

extern char *r_name;

extern char *r_text;

extern bool item_tester_full;

extern byte item_tester_tval;

extern CItem *gi_i_ptr;

extern bool (*item_tester_hook)(CItem *i_ptr);

extern bool (*ang_sort_comp)(vptr u, vptr v, int a, int b);

extern void (*ang_sort_swap)(vptr u, vptr v, int a, int b);

extern bool (*get_mon_num_hook)(int r_idx);

extern bool (*get_obj_num_hook)(int k_idx);



// Other

extern bool show_minimap;

extern bool turn_based;

extern int current_spell, current_spell_type;

extern int show_stuff, show_param;

extern u32b frames, fps_ticker;

extern char cur_console_line[128];

extern CArrow *arrows[MAX_PROJECTILES];

extern bool resting;



/*

 * Function declarations

 */



// birth.c

extern bool player_birth(void);



// map.cpp

extern void memorize_all(int x, int y);

extern void forget_all(int x, int y);

extern bool valid_grid(int y, int x);

extern int distance(int x1, int y1, int x2, int y2);

extern bool los(int y1, int x1, int y2, int x2);

extern bool no_lite(void);

extern void note_spot(int y, int x);

extern void draw_layer_1(int x, int y, int ix, int iy, bool hilited);

extern void add_sprite(s32b depth, int px, int py, int idx, bool glowing, char *scene,

    int view, int frame);

extern void add_sprite(s32b depth, int px, int py, char *name, bool glowing, char *scene,

    int view, int frame);

extern void start_layer_2(void);

extern int compare_depth(const void *a, const void *b);

extern void end_layer_2(void);

extern void draw_layer_2(int x, int y, int ix, int iy, int player_x, int player_y);

extern void forget_lite(void);

extern void update_lite(void);

extern void forget_view(void);

extern void update_view(void);

extern void map_area(void);

extern void wiz_lite(void);

extern void forget_map(void);

extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);

extern bool projectable(int y1, int x1, int y2, int x2);

extern void scatter(int *yp, int *xp, int y, int x, int d, int m);

extern bool is_quest(int level);

extern void subtile_to_pixel(int sx, int sy, int *px, int *py);



// cmd1.c

extern bool test_hit(int chance, int ac, int vis);

extern s16b critical_shot(int weight, int plus, int dam);

extern s16b critical_norm(int weight, int plus, int dam, CMonster *m_ptr);

extern s16b tot_dam_aux(CItem *i_ptr, int tdam, CMonster *m_ptr);

extern void search(void);

extern void carry(void);

extern void move_player(int dir);

extern void run_step(int dir);

extern bool do_cmd_walk(int dir);



// cmd2.c

extern bool do_cmd_search(void);

extern void do_alter(int dir);

extern void do_fire(int tx, int ty);

extern void do_cmd_fire(void);



// cmd3.c

extern void draw_inven_stuff(void);

extern void draw_equip_stuff(void);

extern bool do_cmd_wield(void);

extern bool do_cmd_takeoff(void);

extern bool do_cmd_drop(void);

extern bool do_cmd_destroy(void);

extern void do_cmd_observe(void);

extern void do_cmd_query_symbol(void);

extern void do_cmd_test_monsters(void);

extern void do_cmd_test_objects(void);



// cmd4.c

extern void do_cmd_pause(void);

extern void do_cmd_options(void);

extern void do_cmd_version(void);

extern void do_cmd_check_artifacts(void);

extern void do_cmd_check_uniques(void);

extern void do_cmd_identified_objs(void);

extern bool scan_valid(int c);

extern void do_cmd_bind(char *from, char *to);

extern void get_command(int scan, char *buf);

extern void init_bind(void);



// cmd5.c

extern void do_cmd_browse(void);

extern bool do_cmd_study(void);

extern void do_spell(int j, int tx, int ty);

extern void do_cmd_magic(void);



// cmd6.cpp

extern bool do_cmd_use(void);

extern void do_zap(int tval, int sval, int tx, int ty);

extern void do_cmd_zap(void);

extern void do_activate(int item, int tx, int ty);

extern void do_cmd_activate(void);



// console.cpp

extern void flush_console_buffer(void);

extern void buffer_console(char *cmd);

extern void do_cmd_console(void);

extern void init_console(void);

extern void console_print(char *str);

extern void console_format(char *fmt, ...);

extern void draw_console(void);



// dungeon.c

extern void play_game(void);



// debug.cpp

extern void do_cmd_wiz_change(void);

extern void do_cmd_wiz_play(void);

extern void do_cmd_wiz_item(void);

extern void do_cmd_wiz_cure_all(void);

extern void do_cmd_wiz_jump(void);

extern void do_cmd_wiz_learn(void);

extern void do_cmd_wiz_summon(void);

extern void do_cmd_wiz_named(void);

extern void do_cmd_wiz_zap(void);

extern void wiz_create_item(void);



// entity.cpp

extern void process_projectiles(void);

extern void add_projectile(CArrow *proj);



// files.cpp

extern void display_player(void);

//extern errr file_character(char *name, bool full);

extern void read_times(void);

extern void show_splash(void);

extern errr show_file(char *name, char *what);

extern void do_cmd_help(void);

extern void set_savefile_exist(int idx, int to);

extern void do_cmd_suicide(void);

extern long total_points(void);

extern void close_game(void);

extern void signals_ignore_tstp(void);

extern void signals_handle_tstp(void);

extern void signals_init(void);



/* generate.c */

extern void generate_cave(void);



/* init-txt.c */

extern errr init_v_info_txt(FILE *fp, char *buf);

extern errr init_f_info_txt(FILE *fp, char *buf);

extern errr init_k_info_txt(FILE *fp, char *buf);

extern errr init_a_info_txt(FILE *fp, char *buf);

extern errr init_e_info_txt(FILE *fp, char *buf);

extern errr init_r_info_txt(FILE *fp, char *buf);



/* init.c */

extern void init_some_arrays(void);

extern void init_leftover(void);



/* load.c */

extern errr rd_savefile_new(void);

extern void get_player_summary(player_summary *ps);



/* mon-desc.c */

extern void screen_roff(int r_idx);



/* monster.c */

extern void delete_monster(CMonster *m_ptr);

extern void delete_monster(int y, int x);

extern s16b get_mon_num(int level);

extern void lore_treasure(CMonster *m_ptr, int num_item, int num_gold);

extern void update_monsters(void);

extern bool place_monster_aux(int y, int x, int r_idx, byte flags);

extern bool place_monster(int y, int x, int mlev, byte flags);

extern bool alloc_monster(int dis, int slp, int mlev);

extern bool summon_specific(int y1, int x1, int lev, int type);

extern bool multiply_monster(CMonster *m_ptr);

extern void message_pain(CMonster *m_ptr, int dam);

extern bool monster_saves(int mlev, int plev);

extern void process_monsters(void);



/* obj-desc.cpp */

/* object.cpp */

extern const char *flavor_desc(CObjectKind *k_ptr);

extern void flavor_init(void);

extern bool identify_fully_aux(CItem *i_ptr);

extern s16b index_to_label(int i);

extern s16b label_to_inven(int c);

extern s16b label_to_equip(int c);

extern char *mention_use(int i);

extern char *describe_use(int i);

extern void inven_item_charges(int item);

extern void inven_item_describe(int item);

extern void inven_item_increase(int item, int num);

extern void inven_item_optimize(int item);

extern void floor_item_charges(CItem *i_ptr);

extern void floor_item_describe(CItem *i_ptr);

extern void floor_item_increase(CItem *i_ptr, int num);

extern void floor_item_optimize(CItem *i_ptr);

extern bool inven_carry_okay(CItem *i_ptr);

extern s16b inven_carry(CItem *i_ptr);

extern bool item_tester_okay(CItem *i_ptr);

extern void show_inven(void);

extern void show_equip(void);

extern bool get_item(int *cp, char *pmt, byte flags);

extern void delete_object(CItem *i_ptr);

extern void delete_object(int y, int x);

extern void delete_objects(int y, int x);

extern void wipe_im_lists(void);

extern s16b get_obj_num(int level);

extern bool object_similar(CItem *i_ptr, CItem *j_ptr);

extern void object_absorb(CItem *i_ptr, CItem *j_ptr);

extern s16b lookup_kind(int tval, int sval);

extern void place_object(int y, int x, bool good, bool great, int olev);

extern void acquirement(int y1, int x1, int num, bool great);

extern void place_trap(int y, int x);

extern void place_gold(int y, int x, int olev);

extern void drop_near(CItem *i_ptr, int chance, int y, int x);

extern void pick_trap(int y, int x);

extern char *item_activation(CItem *i_ptr);

extern void process_objects(void);



/* save.cpp */

extern bool save_player(void);

extern bool load_player(void);



/* spells1.cpp */

extern s16b poly_r_idx(int r_idx);

extern void teleport_away(CMonster *m_ptr, int dis);

extern void teleport_player(int dis);

extern void teleport_player_to(int ny, int nx);

extern void teleport_player_level(void);

extern void acid_dam(int dam, char *kb_str);

extern void elec_dam(int dam, char *kb_str);

extern void fire_dam(int dam, char *kb_str);

extern void cold_dam(int dam, char *kb_str);

extern bool inc_stat(int stat);

extern bool dec_stat(int stat, int amount, int permanent);

extern bool res_stat(int stat);

extern bool apply_disenchant(int mode);

extern bool project(CLiving *who, int rad, int y, int x, int dam, int typ, int flg);



/* spells2.cpp */

extern void warding_glyph(void);

extern bool do_dec_stat(int stat);

extern bool do_res_stat(int stat);

extern bool do_inc_stat(int stat);

extern void identify_pack(void);

extern bool remove_curse(void);

extern bool remove_all_curse(void);

extern void self_knowledge(void);

extern bool lose_all_info(void);

extern bool detect_treasure(void);

extern bool detect_magic(void);

extern bool detect_invisible(void);

extern bool detect_evil(void);

extern bool detect_monsters(void);

extern bool detection(void);

extern bool detect_object(void);

extern bool detect_trap(void);

extern bool detect_sdoor(void);

extern void stair_creation(void);

extern bool enchant(CItem *i_ptr, int n, int eflag);

extern bool enchant_spell(int num_hit, int num_dam, int num_ac);

extern bool ident_spell(void);

extern bool identify_fully(void);

extern bool recharge(int num);

extern bool speed_monsters(void);

extern bool slow_monsters(void);

extern bool sleep_monsters(void);

extern void aggravate_monsters(CMonster *who);

extern bool genocide(void);

extern bool mass_genocide(void);

extern bool probing(void);

extern bool banish_evil(int dist);

extern bool dispel_evil(int dam);

extern bool dispel_undead(int dam);

extern bool dispel_monsters(int dam);

extern bool turn_undead(void);

extern void destroy_area(int y1, int x1, int r, bool full);

extern void earthquake(int cy, int cx, int r);

extern void lite_room(int y1, int x1);

extern void unlite_room(int y1, int x1);

extern bool lite_area(int dam, int rad);

extern bool unlite_area(int dam, int rad);

extern bool fire_ball(int typ, int tx, int ty, int dam, int rad);

extern bool fire_bolt(int typ, int tx, int ty, int dam);

extern bool fire_beam(int typ, int tx, int ty, int dam);

extern bool fire_bolt_or_beam(int prob, int typ, int tx, int ty, int dam);

extern bool lite_line(int tx, int ty);

extern bool drain_life(int tx, int ty, int dam);

extern bool wall_to_mud(int tx, int ty);

extern bool destroy_door(int tx, int ty);

extern bool disarm_trap(int tx, int ty);

extern bool heal_monster(int tx, int ty);

extern bool speed_monster(int tx, int ty);

extern bool slow_monster(int tx, int ty);

extern bool sleep_monster(int tx, int ty);

extern bool confuse_monster(int tx, int ty, int plev);

extern bool fear_monster(int tx, int ty, int plev);

extern bool poly_monster(int tx, int ty);

extern bool clone_monster(int tx, int ty);

extern bool teleport_monster(int tx, int ty);

extern bool door_creation(void);

extern bool trap_creation(void);

extern bool destroy_doors_touch(void);

extern bool sleep_monsters_touch(void);

extern void brand_weapon(void);

extern bool curse_armor(void);

extern bool curse_weapon(void);



/* store.c */

extern void do_cmd_store(void);

extern void store_shuffle(void);

extern void store_maint(void);

extern void store_init(void);



// gui.cpp

extern void draw_pane(int x1, int y1, int x2, int y2, byte color);

extern void draw_window_border(int x1, int y1, int x2, int y2, bool depressed);

extern void draw_window(int x1, int y1, int x2, int y2, char *title);

extern void mini_message_box(char *title, char *msg);

extern void gui_draw(CComponent *base);



// util.cpp

extern void debug_log(char *str);

extern void write_options(void);

extern void read_options(void);

extern bool streq(char *s, char *t);

extern bool prefix(char *s, char *t);

extern bool suffix(char *s, char *t);

extern errr path_parse(char *buf, int max, char *file);

extern errr path_temp(char *buf, int max);

extern FILE *my_fopen(char *file, char *mode);

extern errr my_fgets(FILE *fff, char *buf, huge n);

extern errr fd_kill(char *file);

extern errr fd_move(char *file, char *what);

extern errr fd_copy(char *file, char *what);

extern int fd_make(char *file, int mode);

extern int fd_open(char *file, int flags);

extern errr fd_lock(int fd, int what);

extern errr fd_seek(int fd, huge n);

extern errr fd_read(int fd, char *buf, huge n);

extern errr fd_write(int fd, char *buf, huge n);

extern errr fd_close(int fd);

extern void bell(void);

extern void msg_print(char *msg);

extern void msg_format(char *fmt, ...);

extern char *get_old_message(int i);

extern bool askfor_aux(char *buf, int len, int x, int y);

extern bool get_string(char *prompt, char *buf, int len);

extern bool get_check(char *prompt);

extern bool get_check_old(char *prompt);

extern bool get_com(char *prompt, char *command);

extern s16b get_quantity(char *prompt, int max);

extern bool is_a_vowel(int ch);

extern byte get_pct_color(int cur, int max);

extern void mouse_refresh(void);

extern void format_text(int x1, int x2, int y, char *str, byte color, byte font);

extern void put_text_format(int x, int y, char *c, byte color, int font, int justify);

extern void wait_for_key(void);

extern int ascii_inkey(void);

extern int scan_inkey(void);



// xtra1.c

extern void cnv_stat(int val, char *out_val);

extern void cnv_stat_left(int val, char *out_val);

extern s16b modify_stat_value(int value, int amount);

extern void notice_stuff(void);

extern void update_stuff(void);



// xtra2.c

extern void ang_sort_aux(vptr u, vptr v, int p, int q);

extern void ang_sort(vptr u, vptr v, int n);

extern void ang_sort_swap_distance(vptr u, vptr v, int a, int b);

extern bool ang_sort_comp_distance(vptr u, vptr v, int a, int b);



// z-form.cpp

extern uint vstrnfmt(char *buf, uint max, char *fmt, va_list vp);

extern uint strnfmt(char *buf, uint max, char *fmt, ...);

extern char *format(char *fmt, ...);



// System-specific code

extern void quit(char *str);



extern void screen_refresh(void);

extern void partial_screen_refresh(int x,int y, int width, int height);



extern void set_palette_entry(int c, int r, int g, int b);

extern void set_default_palette(void);



extern void set_clip_rect(int x1, int y1, int x2, int y2);

extern void clear_clip_rect(void);



extern void box(int x1, int y1, int x2, int y2, byte color);

extern void blank_screen(byte color);

extern void horiz_line(int x1, int x2, int y, byte color);

extern void vert_line(int x, int y1, int y2, byte color);

extern void rectangle(int x1, int y1, int x2, int y2, byte color);

extern void put_string(int x, int y, char *c, byte color);

extern void put_text(int x, int y, char *c, byte color, int font);

extern int get_char_width(int c, int font);

extern int string_width(char *c, int font);

extern void start_pixel_draw(void);

extern void draw_pixel(int x, int y, byte c);

extern void end_pixel_draw(void);

extern void load_tile(char *filename);

extern void sort_tiles(void);

extern int locate_tile(char *name);

extern void draw_tile_idx(int off_x, int off_y, int index, bool darken, char *scene_name,

    int view, int frame);

extern void draw_tile(int off_x, int off_y, char *tile_name, bool darken, char *scene_name,

    int view, int frame);



byte *save_screen(void);

extern void restore_screen(byte *from);

extern void dump_screen(void);



extern void bell(void);



extern bool get_shift(void);

extern bool get_capslock(void);

extern int ascii_inkey_scan(void);

extern int scan_inkey_scan(void);

extern int convert(int scan, bool shift, bool caps);



extern void get_mouse_status(int *x, int *y, bool *left);

extern bool get_last_left_button_release(int *rx, int *ry);

extern bool get_last_right_button_release(int *rx, int *ry);

extern void virt_draw_mouse(int x, int y);

extern void virt_kill_mouse(int x, int y);



extern u32b get_timer_value(void);

extern void reset_timer(void);



