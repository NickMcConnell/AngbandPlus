/*
 * externs.h
 */

/*
 * Automatically generated function definitions
 */

#ifndef INCLUDED_EXTERNS_H
#define INCLUDED_EXTERNS_H

/* birth.c */

#if (defined(ANGBAND_H))
extern int maxstat(int race, int temp, int stat);
extern void create_random_name(cptr **syl, char *name);
extern void player_birth(void);
#endif

/* cave.c */

#if (defined(ANGBAND_H))
extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int y1, int x1, int y2, int x2);
extern bool player_can_see_bold(int y, int x);
extern bool no_lite(void);
extern bool cave_valid_bold(int y, int x);
extern void map_info(int y, int x, byte *ap, char *cp, byte *tap, char *tcp);
extern void move_cursor_relative(int row, int col);
extern void print_rel(char c, byte a, int y, int x);
extern void highlight_square(int win, int y, int x);
extern void note_spot(int y, int x);
extern void lite_spot(int y, int x);
extern void prt_map(bool reset);
extern void display_map(int *cy, int *cx, int *my, int *mx);
extern void display_wild_map(uint xmin);
extern void do_cmd_view_map(void);
extern void forget_lite(void);
extern void update_lite(void);
extern void forget_view(void);
extern void update_view(void);
extern void update_flow(void);
extern void map_area(void);
extern void wiz_lite(void);
extern void wiz_dark(void);
extern void cave_set_feat(int y, int x, int feat);
extern void mmove2(int *y, int *x, int y1, int x1, int y2, int x2);
extern bool move_in_direction(int *xx, int *yy, int x1, int y1, int x2, int y2, int (*okay)(int, int, int));
extern bool projectable(int y1, int x1, int y2, int x2);
extern bool scatter(int *yp, int *xp, int y, int x, int d, bool (*accept)(int, int));
extern void health_track(int m_idx);
extern void monster_race_track(int r_idx);
extern void object_kind_track(int k_idx);
extern void object_track(object_type *o_ptr);
extern void cave_track(const int y, const int x);
extern void disturb(int stop_stealth);
extern bool is_quest(int level);
#endif

/* cmd1.c */

#if (defined(ANGBAND_H))
extern void py_attack(int y, int x);
extern void do_cmd_attack(void);
extern void do_cmd_fire(object_type *o_ptr);
extern void do_cmd_throw(object_type *o_ptr);
extern void do_cmd_throw_hard(int mult);
extern int power_cost(const race_power *pw_ptr, int lev);
extern void do_cmd_racial_power(void);
#endif

/* cmd2.c */

#if (defined(ANGBAND_H))
extern void do_cmd_go_up(void);
extern void do_cmd_go_down(void);
extern void cnv_arg_to_rep(void);
extern void do_cmd_search(void);
extern void do_cmd_toggle_sneak(void);
extern void do_cmd_open(void);
extern void do_cmd_close(void);
extern void do_cmd_tunnel(void);
extern void do_cmd_disarm(void);
extern void do_cmd_bash(void);
extern void do_cmd_alter(void);
extern void do_cmd_spike(void);
extern void do_cmd_walk(int pickup);
extern void do_cmd_stay(int pickup);
extern void do_cmd_rest(void);
extern void move_to(s16b y, s16b x);
extern void do_cmd_run(void);
#endif

/* cmd3.c */

#if (defined(ANGBAND_H))
extern void do_cmd_inven(bool equip);
extern bool PURE item_tester_hook_wear(object_ctype *o_ptr);
extern void do_cmd_wield(object_type *o_ptr);
extern void do_cmd_takeoff(object_type *o_ptr);
extern void do_cmd_drop(object_type *o_ptr);
extern errr do_cmd_destroy_aux(cptr verb, cptr dative, object_type *q_ptr, object_type *o_ptr);
extern void do_cmd_destroy(object_type *o_ptr);
extern void do_cmd_hide_object(object_type *o_ptr);
extern void do_cmd_unhide_objects(void);
extern void destroy_pack(void);
extern void do_cmd_observe(object_type *o_ptr);
extern void do_cmd_uninscribe(object_type *o_ptr);
extern void do_cmd_inscribe(object_type *o_ptr);
extern void do_cmd_refill(object_type *j_ptr);
extern void do_cmd_target(void);
extern void do_cmd_look(void);
extern void do_cmd_locate(void);
extern void do_cmd_query_symbol(void);
extern void do_cmd_handle(object_type *o_ptr);
#endif

/* cmd4.c */

#if (defined(ANGBAND_H))
extern void do_cmd_redraw(void);
extern void do_cmd_change_name(void);
extern void do_cmd_message_one(void);
extern void do_cmd_messages(void);
extern void print_bool_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern void print_s16b_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern bool parse_bool(void *out, cptr in);
extern bool parse_s16b(void *out, cptr in);
extern bool showfile(cptr name, int y);
extern void opt_special_effect(const option_type * const op_ptr);
extern void do_cmd_options_aux(int page, cptr info, cptr file);
extern void clear_f0(char *buf, uint max, cptr UNUSED fmt, va_list UNUSED *vp);
extern void do_cmd_options(void);
extern void do_cmd_pref(void);
#endif
#if (defined(ALLOW_VISUALS)) && (defined(ANGBAND_H))
extern void init_visuals(void);
#endif
#if (defined(ANGBAND_H))
extern void do_cmd_note(void);
extern void do_cmd_version(void);
extern void do_cmd_feeling(bool FeelingOnly);
extern void do_cmd_load_screen(void);
extern void do_cmd_save_screen(void);
extern void get_symbol_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern long PURE num_kills(void);
extern void do_cmd_knowledge_chaos_features(void);
extern bool shops_good(int town);
extern void shops_display(int town);
extern void do_cmd_knowledge(void);
#endif

/* cmd5.c */

#if (defined(ANGBAND_H))
extern magic_type *num_to_spell(int i);
extern int spell_skill(const magic_type *s_ptr);
extern void evaluate_text_f3(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern u16b spell_energy(u16b skill,u16b min);
extern int get_spirit(int *sn, cptr prompt, bool call);
extern bool PURE item_tester_spells(object_ctype *o_ptr);
extern void display_spells(int y, int x, object_ctype *o_ptr);
extern void do_cmd_browse(object_type *o_ptr);
extern void do_cmd_study(object_type *o_ptr);
extern void do_cmd_cast(object_type *o_ptr);
extern void do_cmd_cantrip(object_type *o_ptr);
extern void do_cmd_invoke(void);
extern void do_cmd_mindcraft(void);
extern void display_spell_list(void);
#endif
#if (defined(CHECK_ARRAYS)) && (defined(ANGBAND_H))
extern void check_magic_info(void);
#endif

/* cmd6.c */

#if (defined(ANGBAND_H))
extern cptr describe_object_power(object_ctype *o_ptr);
extern s16b item_use_energy(object_ctype *o_ptr);
extern void do_cmd_eat_food(object_type *o_ptr);
extern void do_cmd_quaff_potion(object_type *o_ptr);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern void do_cmd_read_scroll(object_type *o_ptr);
extern void get_device_chance(object_ctype *o_ptr, int *num, int *denom);
extern void do_cmd_use_staff(object_type *o_ptr);
extern void do_cmd_aim_wand(object_type *o_ptr);
extern void do_cmd_zap_rod(object_type *o_ptr);
#endif
#if (defined(CHECK_ARRAYS)) && (defined(ANGBAND_H))
extern void check_activation_info(void);
#endif
#if (defined(ANGBAND_H))
extern void do_cmd_activate(object_type *o_ptr);
#endif

/* dungeon.c */

#if (defined(ANGBAND_H))
extern u16b ident_power(object_ctype *o_ptr);
extern bool PURE k_can_curse(int k_idx);
extern int PURE find_feeling(object_ctype *o_ptr);
extern bool k_can_sense(int k_idx);
extern void change_level(s16b new_level, byte come_from);
extern bool psychometry(void);
extern void curse(object_type *o_ptr);
extern void process_command(void);
extern void process_some_user_pref_files(void);
extern void play_game(bool new_game);
#endif

/* files.c */

#if (defined(ANGBAND_H))
extern void safe_setuid_drop(void);
extern void safe_setuid_grab(void);
extern cptr add_stats(s16b sex, s16b race, s16b template, bool maximise, s16b *stat, cptr name);
extern cptr process_pref_file_aux(char *buf, u16b *sf_flags);
extern errr process_pref_file(cptr name);
extern errr check_time(void);
extern errr check_time_init(void);
extern errr check_load(void);
extern errr check_load_init(void);
extern void prt_nums(cptr txt, int y, int x, int l, int cur, int max);
extern void weapon_stats(object_type *o_ptr, int slay, s16b *tohit, s16b *todam, s16b *weap_blow, s16b *mut_blow, s32b *damage);
extern void equippy_f0(char *buf, uint max, cptr UNUSED fmt, va_list UNUSED *vp);
extern int equip_mod(int i);
extern bool PURE player_has_flag_known(int set, u32b flag);
extern void dump_history(FILE *fff);
extern bool display_player(int mode);
extern void file_character(cptr name);
extern void do_cmd_help(cptr name);
extern int color_char_to_attr(char c);
extern cptr cur_help_str(void);
extern void help_track(cptr str);
extern void show_file(cptr name, cptr what);
extern void show_link(cptr link);
extern void init_help_files(void);
extern void display_help_page(cptr str);
extern void process_player_name(void);
extern bool get_name(void);
extern void do_cmd_suicide(void);
extern void do_cmd_save_game(bool is_autosave);
extern int highscore_fd;
extern void display_scores_aux(int from, int to, int note, high_score *score);
extern void display_scores(int from, int to);
extern void template_score(int ptemplate);
extern void race_score(int race_num);
extern errr predict_score(void);
extern void close_game(void);
extern void exit_game_panic(void);
extern void get_rnd_line_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
#endif
#if (defined(HANDLE_SIGNALS)) && (defined(ANGBAND_H))
extern Signal_Handler_t (*signal_aux)(int, Signal_Handler_t);
#endif
#if (defined(ANGBAND_H))
extern void signals_init(void);
#endif
#if (defined(ANGBAND_H) || defined(H_DEFINE_H))
extern void assert_fail(cptr error, cptr file, int line);
#endif

/* generate.c */

#if (defined(ANGBAND_H))
extern void generate_spirit_name(spirit_type *s_ptr);
extern void generate_spirit_names(void);
extern bool PURE cave_empty_bold_p(int y, int x);
extern bool PURE daytime_p(void);
extern void replace_secret_door(int y, int x);
extern void generate_cave(void);
#endif

/* init1.c */

#if (defined(ALLOW_TEMPLATES)) && (defined(ANGBAND_H))
extern errr parse_r_event(char *buf, header *head, vptr *extra);
extern errr parse_z_info(char *buf, header *head, vptr UNUSED *extra);
extern errr parse_f_info(char *buf, header *head, vptr *extra);
extern errr parse_v_info(char *buf, header *head, vptr *extra);
extern errr parse_k_info(char *buf, header *head, vptr *extra);
extern errr parse_o_base(char *buf, header *head, vptr *extra);
extern errr parse_u_info(char *buf, header *head, vptr *extra);
extern errr parse_a_info(char *buf, header *head, vptr *extra);
extern errr parse_e_info(char *buf, header *head, vptr *extra);
extern errr parse_r_info(char *buf, header *head, vptr *extra);
extern errr parse_dun_defs(char *buf, header *head, vptr *extra);
extern errr parse_town_defs(char *buf, header *head, vptr *extra);
extern errr parse_q_list(char *buf, header *head, vptr *extra);
extern errr parse_s_info(char *buf, header *head, vptr *extra);
extern errr parse_template(char *buf, header *head, vptr *extra);
extern errr parse_macro_info(char *buf, header *head, vptr *extra);
extern errr init_info_txt(FILE *fp, char *buf, header *head);
#endif

/* init2.c */

#if (defined(ANGBAND_H))
extern void init_file_paths(cptr path);
#endif
#if (defined(ALLOW_TEMPLATES)) && (defined(ANGBAND_H))
extern s16b error_idx;
extern s16b error_line;
#endif
#if (defined(ANGBAND_H))
extern errr (*check_modification_date_hook)(int fd, cptr template_file);
extern void init_angband(void);
extern void cleanup_angband(void);
#endif

/* load.c */

#if (defined(ANGBAND_H))
extern bool load_player(bool new_game);
#endif

/* loadsave.c */

#if (defined(ANGBAND_H))
extern byte sf_major;
extern byte sf_minor;
extern byte sf_patch;
extern u16b sf_flags_sf[MAX_SF_VAR];
extern u32b sf_xtra;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern const u16b sf_flags_now[MAX_SF_VAR];
extern s16b convert_k_idx(s16b idx, const u16b *from_v, const u16b *to_v);
extern s16b convert_r_idx(s16b idx, const u16b *from_v, const u16b *to_v);
extern s16b convert_owner(s16b idx, const u16b *from_v, const u16b *to_v);
extern void current_flags(u16b *flags);
extern void current_version(u16b *flags, byte *major, byte *minor, byte *patch);
extern bool has_flag(int flag);
#endif

/* maid-x11.c */

#if ((defined(USE_X11) || defined(USE_XAW) || defined(USE_XPJ) || defined(USE_GTK))) && (defined(MAID_X11_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C))
extern u32b create_pixel(Display *dpy, byte red, byte green, byte blue);
#endif
#if ((defined(USE_X11) || defined(USE_XAW) || defined(USE_XPJ) || defined(USE_GTK))) && (defined(MAID_X11_C) || defined(MAIN_GTK_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C))
extern cptr get_default_font(int term_num);
#endif
#if (((defined(USE_X11) || defined(USE_XAW) || defined(USE_XPJ) || defined(USE_GTK))) && defined(USE_GRAPHICS)) && (defined(MAID_X11_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C))
extern XImage *ReadBMP(Display *dpy, char *Name);
extern bool smoothRescaling;
extern XImage *ResizeImage(Display *dpy, XImage *Im, int ix, int iy, int ox, int oy);
#endif

/* main-ami.c */

#if (defined(USE_AMI)) && (defined(MAIN_AMI_C))
extern int GFXW, GFXH, GFXB;
extern char modestr[ 256 ];
extern struct Library *GadToolsBase;
extern struct Library *AslBase;
#endif
#if ((defined(USE_AMI)) && defined(__GNUC__)) && (defined(MAIN_AMI_C))
extern struct ReqToolsBase *ReqToolsBase;
#endif
#if (defined(USE_AMI)) && (defined(MAIN_AMI_C))
extern struct Library *DiskfontBase;
extern struct IntuitionBase *IntuitionBase;
extern struct GfxBase *GfxBase;
extern struct Library *IFFBase;
extern struct Library *CyberGfxBase;
extern struct Library *DataTypesBase;
extern struct Device *ConsoleDev;
extern struct Device *ConsoleDevice;
extern bool use_mask;
extern bool use_bkg;
extern bool nasty_optimise_gfx;
extern bool amiga_palette;
extern bool block_nasty_gfx;
extern struct NewMenu post_item[];
#endif
#if (defined(USE_AMI)) && (defined(MAIN_AMI_C) || defined(MAIN_ROS_C))
extern struct NewMenu menu_ptr[MENUMAX];
#endif
#if (defined(USE_AMI)) && (defined(MAIN_AMI_C))
extern struct NewMenu window_menu[];
#endif
#if (defined(USE_AMI)) && (defined(MAIN_AMI_C) || defined(MAIN_C))
extern const char help_ami[];
extern errr init_ami(int argc, char **argv);
#endif
#if (defined(USE_AMI)) && (defined(MAIN_AMI_C))
extern void amiga_open_libs( void );
extern void open_term( int n, bool doall );
extern void close_term( int n );
extern int read_menus( void );
extern int read_prefs( void );
extern errr amiga_event( int v );
extern void tomb_str( int y, char *str );
extern void handle_rawkey( UWORD code, UWORD qual, APTR addr );
extern void handle_menupick( int mnum, int term );
extern void load_palette( void );
extern ULONG trans( byte g );
extern int create_menus( void );
extern void update_menus( void );
extern void free_sound( void );
extern void put_gfx_map( term_data *td, int x, int y, int c, int a );
extern struct BitMap *alloc_bitmap( int width, int height, int depth, ULONG flags, struct BitMap *friend );
extern void free_bitmap( struct BitMap *bitmap );
extern void scale_bitmap( struct BitMap *srcbm, int srcw, int srch, struct BitMap *dstbm, int dstw, int dsth );
extern void remap_bitmap( struct BitMap *srcbm, struct BitMap *dstbm, long *pens, int width, int height );
extern int depth_of_bitmap( struct BitMap *bm );
extern void amiga_show( char *str );
extern void amiga_redefine_colours( void );
extern void amiga_makepath( char *name );
extern void amiga_save_palette( void );
extern void amiga_load_palette( void );
extern void amiga_hs_to_ascii(void);
extern void amiga_user_name( char *buf );
extern void amiga_write_user_name( char *name );
#endif

/* main-cap.c */

#if ((defined(USE_CAP)) && defined(USE_HARDCODE)) && (defined(MAIN_CAP_C) || defined(MAIN_C))
extern const char help_cap[];
#endif
#if ((defined(USE_CAP)) && !(defined(USE_HARDCODE))) && (defined(MAIN_CAP_C) || defined(MAIN_C))
extern const char help_cap[];
#endif
#if (defined(USE_CAP)) && (defined(MAIN_CAP_C) || defined(MAIN_C))
extern errr init_cap(int argc, char **argv);
#endif

/* main-crb.c */

#if (((defined(MACINTOSH) || defined(MACH_O_CARBON))) && (defined(MACH_O_CARBON) || defined(MAC_MPW))) && (defined(MAIN_CRB_C) || defined(MAIN_MAC_C))
extern u32b _fcreator;
#endif
#if (((defined(MACINTOSH) || defined(MACH_O_CARBON))) && (defined(MACH_O_CARBON) || defined(MAC_MPW))) && (defined(H_CONFIG_H) || defined(MAIN_CRB_C) || defined(MAIN_MAC_C))
extern u32b _ftype;
#endif
#if (((defined(MACINTOSH) || defined(MACH_O_CARBON))) && defined(MACH_O_CARBON)) && (defined(MAIN_CRB_C))
extern void open_aux_crb(cptr pathname);
#endif
#if (((defined(MACINTOSH) || defined(MACH_O_CARBON))) && defined(MAC_MPW)) && (defined(MAIN_CRB_C))
extern void convert_pathname(char* path);
#endif

/* main-dos.c */

#if (defined(USE_DOS)) && (defined(MAIN_DOS_C) || defined(MAIN_C))
extern const char help_dos[];
extern errr init_dos(int argc, char **argv);
#endif

/* main-emx.c */

#if (((defined(USE_EMX)) && !(defined(EMXPM))) && !(defined(__EMX__CLIENT__))) && (defined(MAIN_EMX_C) || defined(MAIN_C))
extern const char help_emx[];
extern errr init_emx(int argc, char **argv);
#endif
#if (((defined(USE_EMX)) && !(defined(EMXPM))) && defined(__EMX__CLIENT__)) && (defined(MAIN_EMX_C))
extern void moveClientWindow(int x, int y);
#endif
#if ((defined(USE_EMX)) && defined(EMXPM)) && (defined(MAIN_EMX_C))
extern void emx_init_term(term *t, void *main_instance, int n);
#endif
#if ((defined(USE_EMX)) && defined(EMXPM)) && (defined(MAIN_EMX_C) || defined(MAIN_C))
extern errr init_emx(int argc, char **argv);
#endif
#if ((defined(USE_EMX)) && defined(EMXPM)) && (defined(MAIN_EMX_C))
extern void angbandThread(void *arg);
#endif

/* main-gcu.c */

#if ((defined(USE_GCU)) && defined(USE_NCURSES)) && (defined(MAIN_GCU_C) || defined(MAIN_C))
extern const char help_gcu[];
#endif
#if ((defined(USE_GCU)) && !(defined(USE_NCURSES))) && (defined(MAIN_GCU_C) || defined(MAIN_C))
extern const char help_gcu[];
#endif
#if (defined(USE_GCU)) && (defined(MAIN_GCU_C) || defined(MAIN_C))
extern errr init_gcu(int argc, char **argv);
#endif

/* main-gtk.c */

#if (defined(USE_GTK)) && (defined(MAIN_GTK_C) || defined(MAIN_C))
extern const char help_gtk[];
extern errr init_gtk(int argc, char **argv);
#endif

/* main-ibm.c */

#if (defined(USE_IBM)) && (defined(MAIN_IBM_C) || defined(MAIN_C))
extern const char help_ibm[];
extern errr init_ibm(int argc, char **argv);
#endif

/* main-lsl.c */

#if (defined(USE_LSL)) && (defined(MAIN_LSL_C) || defined(MAIN_C))
extern const char help_lsl[];
extern errr init_lsl(int argc, char **argv);
#endif

/* main-mac.c */

#if (defined(MACINTOSH)) && (defined(MAIN_CRB_C) || defined(MAIN_MAC_C))
extern Boolean open_when_ready;
extern Boolean quit_when_ready;
#endif
#if ((defined(MACINTOSH)) && defined(USE_SFL_CODE)) && (defined(MAIN_MAC_C))
extern AEEventHandlerUPP AEH_Start_UPP;
extern AEEventHandlerUPP AEH_Quit_UPP;
extern AEEventHandlerUPP AEH_Print_UPP;
extern AEEventHandlerUPP AEH_Open_UPP;
#endif
#if ((defined(MACINTOSH) || defined(MACH_O_CARBON))) && (defined(MAIN_CRB_C) || defined(MAIN_MAC_C))
extern void get_version_mac(int *v);
#endif

/* main-ros.c */

#if (defined(ACORN)) && (defined(FILES_C) || defined(LOAD_C) || defined(MAIN_ROS_C) || defined(SAVE_C) || defined(UTIL_C))
extern FILE *my_fopen(cptr f, cptr m);
#endif
#if (defined(ACORN)) && (defined(BIRTH_C) || defined(CMD4_C) || defined(FILES_C) || defined(INIT2_C) || defined(LOAD_C) || defined(MAIN_MAC_C) || defined(MAIN_ROS_C) || defined(MAIN_XPJ_C) || defined(SAVE_C) || defined(UTIL_C) || defined(WIZARD1_C))
extern errr my_fclose(FILE *fp);
#endif
#if (defined(ACORN)) && (defined(INIT2_C) || defined(MAIN_ROS_C) || defined(SAVE_C) || defined(UTIL_C))
extern int fd_make(cptr file, int mode);
#endif
#if (defined(ACORN)) && (defined(CMD4_C) || defined(FILES_C) || defined(LOAD_C) || defined(MAIN_ROS_C) || defined(SAVE_C) || defined(UTIL_C) || defined(XTRA2_C))
extern errr fd_kill(cptr file);
#endif
#if (defined(ACORN)) && (defined(MAIN_ROS_C) || defined(SAVE_C) || defined(UTIL_C))
extern errr fd_move(cptr old, cptr new);
#endif
#if (defined(ACORN)) && (defined(FILES_C) || defined(INIT2_C) || defined(LOAD_C) || defined(MAIN_CRB_C) || defined(MAIN_GTK_C) || defined(MAIN_MAC_C) || defined(MAIN_ROS_C) || defined(MAIN_WIN_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C) || defined(UTIL_C))
extern int fd_open(cptr path, int flags);
#endif
#if (defined(ACORN)) && (defined(FILES_C) || defined(INIT2_C) || defined(LOAD_C) || defined(MAIN_CRB_C) || defined(MAIN_GTK_C) || defined(MAIN_MAC_C) || defined(MAIN_ROS_C) || defined(MAIN_WIN_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C) || defined(SAVE_C) || defined(UTIL_C))
extern errr fd_close(int handle);
#endif
#if (defined(ACORN)) && (defined(FILES_C) || defined(INIT2_C) || defined(LOAD_C) || defined(MAIN_ROS_C) || defined(UTIL_C))
extern errr fd_read(int handle, char *buf, size_t nbytes);
#endif
#if (defined(ACORN)) && (defined(FILES_C) || defined(INIT2_C) || defined(MAIN_ROS_C) || defined(UTIL_C))
extern errr fd_write(int handle, const char *buf, size_t nbytes);
#endif
#if (defined(ACORN)) && (defined(FILES_C) || defined(MAIN_ROS_C) || defined(UTIL_C))
extern errr fd_seek(int handle, long offset);
extern errr fd_lock(int handle, int what);
#endif
#if (defined(ACORN)) && (defined(MAIN_ROS_C) || defined(UTIL_C))
extern errr path_temp(char *buf, int max);
#endif
#if ((defined(ACORN)) && defined(USE_FILECACHE)) && (defined(MAIN_ROS_C))
extern FILE *cached_fopen(cptr name, cptr mode);
extern errr cached_fclose(FILE *fch_);
extern errr cached_fgets(FILE *fch_, char *buffer, size_t max_len);
#endif

/* main-sla.c */

#if (defined(USE_SLA)) && (defined(MAIN_SLA_C) || defined(MAIN_C))
extern const char help_sla[];
extern errr init_sla(int argc, char **argv);
#endif

/* main-vcs.c */

#if (defined(USE_VCS)) && (defined(MAIN_VCS_C) || defined(MAIN_C))
extern const char help_vcs[];
extern errr init_vcs(int argc, char** argv);
#endif

/* main-vme.c */

#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VME_C))
extern char e2a[];
extern char a2e[];
extern char DISP[26];
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_AMI_C) || defined(MAIN_CAP_C) || defined(MAIN_CRB_C) || defined(MAIN_DOS_C) || defined(MAIN_GCU_C) || defined(MAIN_GTK_C) || defined(MAIN_IBM_C) || defined(MAIN_MAC_C) || defined(MAIN_VME_C) || defined(MAIN_WIN_C) || defined(MAIN_X11_C) || defined(MAIN_XAW_C) || defined(MAIN_XPJ_C))
extern int rows, cols;
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_CAP_C) || defined(MAIN_VME_C))
extern int curx;
extern int cury;
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_IBM_C) || defined(MAIN_VME_C))
extern byte VirtualScreen[2048];
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VME_C))
extern byte ScreenAttr[2048];
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_IBM_C) || defined(MAIN_VME_C))
extern byte wiper[256];
extern void ScreenUpdateLine(int line);
extern void ScreenClear(void);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VME_C) || defined(MAIN_C))
extern const char help_vme[];
extern errr init_vme(int argc, char **argv);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VME_C))
extern getkey(void);
extern getkeybuf(void);
extern char *cons;
extern int CNSINTR;
extern void InitConsole(void);
extern void TerminateConsole(void);
extern void ResetScrBuf(void);
extern void AddScrBuf(char * ptr, int len);
extern void GetAddr(int y, int x, char *stream);
extern char InKey(void);
extern char InKeyBuf(void);
extern void ResetDISP(void);
extern int kbhit(void);
extern void ShowLine(int y, int x, int len);
extern void LoadProfile(void);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VCS_C) || defined(MAIN_VME_C) || defined(UTIL_C))
extern open(char *name, int flags, int mode);
extern close(int fd);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_CAP_C) || defined(MAIN_GCU_C) || defined(MAIN_LSL_C) || defined(MAIN_ROS_C) || defined(MAIN_VCS_C) || defined(MAIN_VME_C) || defined(UTIL_C))
extern read(int fd, char *buff, int bytes);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_CAP_C) || defined(MAIN_DOS_C) || defined(MAIN_GCU_C) || defined(MAIN_IBM_C) || defined(MAIN_ROS_C) || defined(MAIN_VCS_C) || defined(MAIN_VME_C) || defined(UTIL_C))
extern write(int fd, char *buff, int bytes);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VCS_C) || defined(MAIN_VME_C) || defined(UTIL_C))
extern lseek(int fd, long pos, int set);
#endif
#if ((defined(USE_VME) || defined(VM))) && (defined(MAIN_VME_C))
extern unlink(char *filename);
#endif

/* main-win.c */

#if (defined(WINDOWS)) && (defined(MAIN_CRB_C) || defined(MAIN_GTK_C) || defined(MAIN_MAC_C) || defined(MAIN_ROS_C) || defined(MAIN_WIN_C))
extern bool game_in_progress;
#endif
#if (defined(WINDOWS)) && (defined(MAIN_CRB_C) || defined(MAIN_MAC_C) || defined(MAIN_WIN_C) || defined(Z_RAND_C))
extern bool initialized;
#endif
#if (defined(WINDOWS)) && (defined(MAIN_WIN_C))
extern bool paletted;
extern bool colors16;
#endif
#if ((defined(WINDOWS)) && defined(USE_SAVER)) && (defined(MAIN_WIN_C))
extern LRESULT FAR PASCAL AngbandSaverProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam);
#endif
#if (defined(WINDOWS)) && (defined(MAIN_WIN_C))
extern int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPSTR lpCmdLine, int nCmdShow);
#endif

/* main-x11.c */

#if (defined(USE_X11)) && (defined(MAIN_X11_C) || defined(MAIN_C))
extern const char help_x11[];
extern errr init_x11(int argc, char **argv);
#endif

/* main-xaw.c */

#if (defined(USE_XAW)) && (defined(MAIN_XAW_C) || defined(MAIN_C))
extern const char help_xaw[];
extern errr init_xaw(int argc, char **argv);
#endif

/* main-xpj.c */

#if (defined(USE_XPJ)) && (defined(MAIN_XPJ_C) || defined(MAIN_C))
extern const char help_xpj[];
extern errr init_xpj(int argc, char **argv);
#endif

/* main-xxx.c */

#if (defined(USE_XXX)) && (defined(MAIN_XXX_C))
extern cptr help_xxx;
extern errr init_xxx(int argc, char **argv);
#endif

/* melee1.c */

#if (defined(ANGBAND_H))
extern int check_hit(int power, int level);
extern blow_method_type *get_blow_method(byte idx);
extern bool make_attack_normal(int m_idx);
#endif

/* melee2.c */

#if (defined(ANGBAND_H))
extern void curse_equipment(int chance, int heavy_chance);
extern void process_monsters(void);
#endif

/* monster1.c */

#if (defined(ANGBAND_H))
extern void roff_top(int r_idx);
extern void screen_roff(int r_idx);
#endif

/* monster2.c */

#if (defined(ANGBAND_H))
extern bool place_ghost(void);
extern void delete_monster_idx(int i,bool visibly);
extern void delete_monster(int y, int x);
extern bool grow_m_list(void);
extern void compact_monsters(int size);
extern void remove_non_pets(void);
extern s16b m_pop(void);
extern void get_mon_num_prep(bool (*hook)(int, int), int p);
extern s16b get_mon_num(int level);
extern void monster_desc_aux_f3(char *buf, uint max, cptr fmt, va_list *vp);
extern void monster_desc_f2(char *buf, uint max, cptr fmt, va_list *vp);
extern void lore_do_probe(int r_idx);
extern void lore_treasure(int r_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern bool live_monster_p(monster_race *r_ptr);
extern bool live_monster_wide_p(monster_race *r_ptr);
extern monster_type *place_monster_one(int y, int x, int r_idx, bool slp, bool charm, bool force);
extern monster_type *place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool charm, bool force);
extern bool place_monster(int y, int x, int level, bool slp, bool grp);
extern bool put_quest_monster(int r_idx);
#endif
#if (defined(MONSTER_HORDES)) && (defined(ANGBAND_H))
extern bool alloc_horde(int y, int x, int level);
#endif
#if (defined(ANGBAND_H))
extern void alloc_monster(int dis, int level, bool slp);
extern bool summon_specific_aux(int y1, int x1, int lev, int type, bool Group_ok, bool charm);
extern bool summon_specific(int y1, int x1, int lev, int type);
extern bool summon_specific_friendly(int y1, int x1, int lev, int type, bool Group_ok);
extern bool multiply_monster(monster_type *m_ptr, bool charm, bool clone);
extern void message_pain(monster_type *m_ptr, int dam);
extern void update_smart_learn(monster_type *m_ptr, int what);
#endif

/* object1.c */

#if (defined(ANGBAND_H))
extern s16b lookup_unident(byte p_id, byte s_id);
extern void flavor_init(void);
extern void reset_visuals(void);
extern void object_flags(object_ctype *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern void object_info_known(object_type *j_ptr, object_ctype *o_ptr);
extern void object_flags_known(object_ctype *o_ptr, u32b *f1, u32b *f2, u32b *f3);
extern cptr PURE get_inscription(object_ctype *o_ptr);
extern cptr find_next_good_flag(cptr s, byte reject, byte require);
extern void object_desc_f3(char *buf, uint max, cptr fmt, va_list *vp);
extern void object_k_name_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern cptr list_flags(cptr init, cptr conj, cptr *flags, int total);
extern object_ctype PURE *get_real_obj(object_ctype *o_ptr);
extern bool PURE is_worn_p(object_ctype *o_ptr);
extern bool PURE is_inventory_p(object_ctype *o_ptr);
extern int PURE get_bow_mult(object_ctype *o_ptr);
extern s16b PURE launcher_type(object_ctype *o_ptr);
extern byte PURE ammunition_type(object_ctype *o_ptr);
extern bool identify_fully_aux(object_ctype *o_ptr, byte flags);
extern void identify_fully_file(object_ctype *o_ptr, FILE *fff, bool spoil);
extern s16b PURE index_to_label(object_ctype *o_ptr);
extern s16b PURE wield_slot(object_ctype *o_ptr);
extern cptr PURE describe_use(object_ctype *o_ptr);
extern byte get_i_attr(object_type *o_ptr);
extern void display_inven(bool equip);
extern void show_inven(bool equip, bool all);
extern void next_object(object_type **o_ptr);
extern object_type *get_item(errr *err, cptr pmt, bool equip, bool inven, bool floor);
extern bool PURE item_tester_hook_destroy(object_ctype *o_ptr);
extern bool PURE item_tester_okay_cmd(void (*func)(object_type *), object_ctype *o_ptr);
extern object_type *get_object_from_function(void (*func)(object_type *));
extern bool do_cmd_use_object(s16b cmd);
#endif

/* object2.c */

#if (defined(ANGBAND_H))
extern void excise_dun_object(object_type *j_ptr);
extern void delete_dun_object(object_type *j_ptr);
extern void delete_object(int y, int x);
extern void do_cmd_rotate_stack(void);
extern bool grow_o_list(void);
extern void compact_objects(int size);
extern void wipe_o_list(bool preserve);
extern object_type *o_pop(void);
extern s16b get_obj_num(int level);
extern void object_known(object_type *o_ptr);
extern void object_aware(object_type *o_ptr);
extern void object_tried(object_type *o_ptr);
extern void object_touch(object_type *o_ptr);
extern s32b PURE flag_cost(object_ctype *o_ptr, bool all);
extern s32b PURE object_value(object_ctype *o1_ptr, bool full);
extern void set_stack_number(object_type *o_ptr);
extern int PURE object_similar_2(object_ctype *o_ptr, object_ctype *j_ptr);
extern bool PURE object_similar(object_ctype *o_ptr, object_ctype *j_ptr);
extern bool store_object_absorb(object_type *j_ptr, object_type *o_ptr);
extern bool object_absorb(object_type *o_ptr, object_type *j_ptr);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, object_ctype *j_ptr);
extern void object_prep(object_type *o_ptr, int k_idx);
extern bool make_fake_artifact(object_type *o_ptr, int name1);
extern void artefact_name_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
#endif
#if (defined(CHECK_ARRAYS)) && (defined(ANGBAND_H))
extern void check_bonus_table(void);
#endif
#if (defined(ANGBAND_H))
extern void apply_magic_2(object_type *o_ptr, const int lev);
extern void set_object_found(object_type *o_ptr, int how, int idx);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, int how, int idx);
extern void make_item(object_type *o_ptr, make_item_type *i_ptr, cptr names, int how, int idx);
extern bool PURE magic_can_curse(int k_idx);
extern bool make_object(object_type *j_ptr, bool good, bool great, int how, int idx);
extern void place_object(int y, int x, bool good, bool great, int how, int idx);
extern bool make_gold(object_type *j_ptr, int how, int idx, int coin_type);
extern void place_gold(int y, int x, int how, int idx);
extern object_type *drop_near(object_type *j_ptr, int chance, int y, int x);
extern void acquirement(int y1, int x1, int num, bool great);
extern void pick_trap(int y, int x);
extern void place_trap(int y, int x);
extern void item_charges(object_ctype *o_ptr);
extern void item_increase(object_type *o_ptr, int num);
extern void item_describe(object_ctype *o_ptr);
extern void item_optimize(object_type *o_ptr);
extern bool PURE inven_carry_okay(object_ctype *o_ptr);
extern object_type *inven_carry(object_type *o_ptr);
extern object_type *inven_takeoff(object_type *o_ptr, int amt);
extern void inven_drop(object_type *o_ptr, int amt);
extern void combine_pack(void);
extern void reorder_pack(void);
extern void display_koff(int k_idx);
extern void object_hide(object_type *o_ptr);
extern void init_easy_know(void);
#endif

/* powers.c */

#if (defined(ANGBAND_H))
extern cptr describe_power(int power, int lev);
extern void do_poly_wounds(int cause);
extern void do_poly_self(void);
extern void wild_magic(int spell);
extern bool use_object_power(const int power, int dir, bool *ident, bool *use);
extern bool use_known_power(int power, int plev);
#endif

/* quest.c */

#if (defined(ANGBAND_H))
extern quest_type *get_quest(void);
extern int get_quest_monster(void);
extern void print_quest_message(void);
extern void quest_discovery(bool new);
extern void set_guardians(void);
extern quest_type *cnv_monster_to_quest(monster_race *r_ptr);
#endif

/* readdib.c */

#if ((defined(WINDOWS) && defined(USE_GRAPHICS))) && (defined(READDIB_H) || defined(MAIN_WIN_C) || defined(READDIB_C))
extern BOOL ReadDIB(HWND hWnd, LPSTR lpFileName, DIBINIT *pInfo);
#endif

/* save.c */

#if (defined(ANGBAND_H))
extern bool save_player(bool as_4_1_0);
#endif

/* spells1.c */

#if (defined(ANGBAND_H))
extern s16b poly_r_idx(int r_idx);
extern void teleport_away(int m_idx, int dis);
extern void teleport_player(int dis);
extern void teleport_player_to(int ny, int nx);
extern void teleport_player_level(void);
extern gf_type *lookup_gf(int type);
extern void take_hit(int damage, cptr hit_from, int monster);
extern void acid_dam(int dam, cptr kb_str, int monster);
extern void elec_dam(int dam, cptr kb_str, int monster);
extern void fire_dam(int dam, cptr kb_str, int monster);
extern void cold_dam(int dam, cptr kb_str, int monster);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, int permanent);
extern bool res_stat(int stat);
extern bool apply_disenchant(int mode);
extern void chaos_feature_shuffle(void);
extern bool project(monster_type *mw_ptr, int rad, int y, int x, int dam, int typ, int flg);
extern void potion_smash_effect(monster_type *m_ptr, int y, int x, int o_kidx);
#endif

/* spells2.c */

#if (defined(ANGBAND_H))
extern bool dimension_door(int plev, int fail_dis);
extern bool hp_player(int num);
extern void warding_glyph(void);
extern void explosive_rune(void);
extern bool do_dec_stat(int stat);
extern void do_dec_stat_time(int stat, bool msg);
extern bool do_res_stat(int stat);
extern bool do_res_stats(void);
extern bool do_inc_stat(int stat);
extern void identify_pack(void);
extern bool remove_curse(void);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern bool alchemy(void);
extern void self_knowledge(void);
extern bool lose_all_info(void);
extern PURE bool detect_traps_p(void);
extern bool detect_traps(void);
extern bool detect_doors(void);
extern bool detect_stairs(void);
extern bool detect_treasure(void);
extern bool detect_objects_gold(void);
extern bool detect_objects_normal(void);
extern bool detect_objects_magic(void);
extern bool detect_monsters_normal(void);
extern bool detect_monsters_invis(void);
extern bool detect_monsters_evil(void);
extern bool detect_all(void);
extern void stair_creation(void);
extern bool PURE item_tester_hook_armour(object_ctype *o_ptr);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern void add_resistance(object_type *o_ptr, int min, int max);
extern bool create_artifact(object_type *o_ptr, bool a_scroll);
extern bool artifact_scroll(void);
extern bool ident_spell(void);
extern void do_identify_fully(object_type *o_ptr);
extern bool identify_fully(void);
extern bool PURE item_tester_hook_recharge(object_ctype *o_ptr);
extern bool recharge(int num);
extern bool speed_monsters(void);
extern bool slow_monsters(int dam);
extern bool sleep_monsters(int dam);
extern bool banish_evil(int dist);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_good(int dam);
extern bool dispel_monsters(int dam);
extern bool dispel_living(int dam);
extern bool dispel_demons(int dam);
extern void aggravate_monsters(monster_type *mw_ptr);
extern errr genocide(bool player_cast);
extern errr mass_genocide(bool player_cast);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r, bool full);
extern void earthquake(int cy, int cx, int r);
extern void unlite_room(int y1, int x1);
extern bool lite_area(int dam, int rad);
extern bool unlite_area(int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool lite_line(int dir);
extern bool wall_to_mud(int dir);
extern bool wizard_lock(int dir);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool door_creation(void);
extern bool trap_creation(void);
extern bool glyph_creation(void);
extern bool wall_stone(void);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(int dam);
extern void call_chaos(int plev);
extern void activate_ty_curse(void);
extern void activate_hi_summon(void);
extern void summon_reaver(void);
extern void wall_breaker(int plev);
extern void bless_weapon(void);
extern bool detect_monsters_nonliving(void);
extern void set_recall(bool spell);
extern bool confuse_monsters(int dam);
extern bool charm_monsters(int dam);
extern bool charm_animals(int dam);
extern bool stun_monsters(int dam);
extern bool stasis_monsters(int dam);
extern bool mindblast_monsters(int dam);
extern bool banish_monsters(int dist);
extern bool turn_evil(int dam);
extern bool turn_monsters(int dam);
extern bool charm_monster(int dir, int plev);
extern bool control_one_undead(int dir, int plev);
extern bool charm_animal(int dir, int plev);
extern void report_magics(void);
extern void teleport_swap(int dir);
extern void alter_reality(void);
extern void do_cmd_rerate(void);
#endif

/* squelch.c */

#if (defined(ANGBAND_H))
extern void squelch_grid(void);
extern void squelch_inventory(void);
#endif

/* store.c */

#if (defined(ANGBAND_H))
extern void store_title_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern store_type *find_house(int town);
extern void do_cmd_store(void);
extern bool store_shuffle(int which);
extern void store_maint(int which);
extern void store_init(int which);
#endif

/* tables.c */

#if (defined(ANGBAND_H))
extern s16b ddd[9];
extern s16b ddx[10];
extern s16b ddy[10];
extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];
extern char hexsym[16];
extern const byte adj_mag_study[NUM_STAT_INDICES];
extern const byte adj_mag_mana[NUM_STAT_INDICES];
extern const byte adj_mag_fail[NUM_STAT_INDICES];
extern const byte adj_mag_stat[NUM_STAT_INDICES];
extern const byte adj_chr_gold[NUM_STAT_INDICES];
extern const byte adj_int_dev[NUM_STAT_INDICES];
extern const byte adj_wis_sav[NUM_STAT_INDICES];
extern const byte adj_dex_dis[NUM_STAT_INDICES];
extern const byte adj_int_dis[NUM_STAT_INDICES];
extern const byte adj_dex_ta[NUM_STAT_INDICES];
extern const byte adj_str_td[NUM_STAT_INDICES];
extern const byte adj_dex_th[NUM_STAT_INDICES];
extern const byte adj_str_th[NUM_STAT_INDICES];
extern const byte adj_str_wgt[NUM_STAT_INDICES];
extern const byte adj_str_hold[NUM_STAT_INDICES];
extern const byte adj_str_dig[NUM_STAT_INDICES];
extern const byte adj_str_blow[NUM_STAT_INDICES];
extern const byte adj_dex_blow[NUM_STAT_INDICES];
extern const byte adj_dex_safe[NUM_STAT_INDICES];
extern const byte adj_con_fix[NUM_STAT_INDICES];
extern const byte adj_con_mhp[NUM_STAT_INDICES];
extern byte blows_table[12][12];
extern u16b extract_energy[200];
extern player_sex sex_info[MAX_SEXES];
extern cptr *elf_syllables[];
extern cptr *hobbit_syllables[];
extern player_race race_info[MAX_RACES];
extern book_type book_info[MAX_BK];
extern player_skill skill_set[MAX_SKILLS];
extern byte chest_traps[64];
extern cptr color_names[16];
extern cptr atchar;
extern cptr stat_names[6];
extern cptr stat_names_reduced[6];
extern option_type option_info[];
extern force_type option_force[];
extern option_special autosave_info[6];
extern cptr chaos_patron_shorts[MAX_PATRON];
extern int chaos_stats[MAX_PATRON];
extern int chaos_rewards[MAX_PATRON][20];
extern martial_arts ma_blows[MAX_MA+1];
extern window_type windows[ANGBAND_TERM_MAX];
extern moncol_type moncol[MAX_MONCOL];
extern wild_type wild_grid[12][12];
extern spirit_type spirits[MAX_SPIRITS];
extern blow_method_type blow_methods[NUM_BLOW_METHODS];
extern redraw_type screen_coords[NUM_SCREEN_COORDS];
extern gf_type gf_info[71];
extern cptr coin_types[];
extern name_centry ident_info[];
extern cptr_ch feeling_str[SENSE_MAX];
extern cptr option_chars;
extern natural_attack natural_attacks[5];
#endif

/* ui.c */

#if (defined(ANGBAND_H))
extern int display_entry_list_bounded(name_centry *list, int num, int truncate, int minx, int miny, int maxx, int maxy);
extern int display_entry_list(name_centry *list, int num, int maxy, bool truncate);
extern int build_choice_list_1(name_entry *list, name_centry *start, int listm, int items, bool (*item_good)(int, int));
extern int build_choice_list_2(name_entry *list, int idx, int listm, int items, bool (*item_good)(int, int));
#endif

/* util.c */

#if (!(defined(HAS_MEMSET))) && (defined(ANGBAND_H) || defined(Z_VIRT_H))
extern char *memset(char *s, int c, huge n);
#endif
#if (!(defined(HAS_STRICMP))) && (defined(ANGBAND_H) || defined(H_CONFIG_H))
extern int stricmp(cptr a, cptr b);
#endif
#if ((defined(SET_UID)) && !(defined(HAS_USLEEP))) && (defined(ANGBAND_H))
extern int usleep(huge usecs);
#endif
#if (defined(SET_UID)) && (defined(ANGBAND_H))
extern void user_name(char *buf, int id);
#endif
#if (!(defined(ACORN))) && (defined(ANGBAND_H))
extern FILE *my_fopen(cptr file, cptr mode);
extern errr my_fclose(FILE *fff);
#endif
#if (defined(ANGBAND_H))
extern void path_build_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern FILE *my_fopen_path(cptr path, cptr file, cptr mode);
#endif
#if (defined(HAVE_MKSTEMP)) && (defined(ANGBAND_H))
extern FILE *my_fopen_temp(char *buf, uint max);
#endif
#if (!(defined(HAVE_MKSTEMP))) && (defined(ANGBAND_H))
extern FILE *my_fopen_temp(char *buf, int max);
#endif
#if (defined(ANGBAND_H))
extern errr my_fgets(FILE *fff, char *buf, size_t n);
extern errr my_fgets_long(char *buf, size_t n, FILE *fff);
extern int my_fprintf(FILE *fff, cptr fmt, ...);
#endif
#if (!(defined(ACORN))) && (defined(ANGBAND_H))
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr out, cptr in);
#endif
#if ((!(defined(ACORN))) && defined(PRIVATE_USER_PATH)) && (defined(ANGBAND_H))
extern errr my_mkdir(cptr path, uint mode);
#endif
#if (!(defined(ACORN))) && (defined(ANGBAND_H))
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, huge n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
#endif
#if (defined(ANGBAND_H))
extern void move_cursor(int row, int col);
extern void init_ascii_text_conv(void);
extern void text_to_ascii_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern void ascii_to_text_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern void s16b_to_string_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern sint macro_find_exact(cptr pat);
extern errr macro_add(cptr pat, cptr act);
extern void flush(void);
extern void bell(cptr fmt, ...);
extern void sound(int val);
extern bool screen_is_icky(void);
extern void set_gnext(cptr next);
extern bool gnext_clear(void);
extern bool is_keymap_or_macro(void);
extern char inkey(void);
extern u16b quark_add(cptr str);
extern cptr quark_str(u16b i);
extern s16b message_num(void);
extern cptr message_str(s16b age);
extern void message_add(cptr str);
extern bool no_msg_print;
extern void msg_print(cptr msg);
extern void msg_format(cptr fmt, ...);
extern void c_put_str(byte attr, cptr str, int row, int col);
extern void put_str(cptr str, int row, int col);
extern void prt(cptr str, int row, int col);
extern void mc_roff_xy(int x, int y, cptr s);
extern void c_roff(byte a, cptr str);
extern void mc_put_str(const int y, const int x, cptr str);
extern void mc_add_fmt(cptr fmt, ...);
extern void mc_put_fmt(const int y, const int x, cptr fmt, ...);
extern void mc_put_lfmt(const int y, const int x, const int l, cptr fmt, ...);
extern void clear_from(int row);
extern bool askfor_aux(char *buf, int len);
extern bool get_string(cptr prompt, char *buf, int len);
extern char get_check_aux(cptr prompt, cptr text, cptr conv_from, cptr conv_to);
extern bool get_check(cptr prompt);
extern bool get_com(char *command, cptr fmt, ...);
extern s16b get_quantity(cptr prompt, int max,bool allbydefault);
extern void pause_line(void);
extern int keymap_mode(void);
extern void request_command(bool shopping);
extern bool is_a_vowel(int ch);
extern int get_keymap_dir(char ch);
#endif
#if (defined(ALLOW_REPEAT)) && (defined(ANGBAND_H))
extern void repeat_push(int what);
extern bool repeat_pull(int *what);
extern void repeat_check(void);
#endif
#if (defined(SUPPORT_GAMMA)) && (defined(ANGBAND_H))
extern byte gamma_table[256];
extern void build_gamma_table(int gamma);
#endif
#if (defined(ANGBAND_H))
extern errr add_resize_hook(void (*resize_hook)(void));
extern errr delete_resize_hook(void (*resize_hook)(void));
extern void resize_main_term(void);
extern void repeat_string_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern errr type_string(cptr str, uint len);
#endif

/* variable.c */

#if (defined(ANGBAND_H))
extern cptr copyright[5];
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
extern bool character_icky;
extern bool character_xtra;
extern u32b seed_flavor;
extern u32b seed_wild;
extern s16b command_cmd;
extern s16b command_arg;
extern s16b command_rep;
extern s16b command_dir;
extern s16b command_see;
extern s16b command_wrk;
extern s16b command_gap;
extern s16b command_new;
extern s16b energy_use;
extern s16b old_energy_use;
extern bool msg_flag;
extern bool alive;
extern bool death;
extern s16b cur_hgt;
extern s16b cur_wid;
extern s16b dun_level;
extern s16b dun_offset;
extern u16b dun_bias;
extern byte cur_town;
extern byte cur_dungeon;
extern byte recall_dungeon;
extern byte came_from;
extern s16b num_repro;
extern s16b object_level;
extern s32b turn;
extern s32b old_turn;
extern s32b curse_turn;
extern bool cheat_wzrd;
extern bool use_sound;
#endif
#if (defined(ANGBAND_H) || defined(Z_TERM_C))
extern bool use_graphics;
#endif
#if (defined(ANGBAND_H))
extern u16b total_winner;
extern u16b noscore;
extern bool use_transparency;
extern s16b signal_count;
extern bool inkey_base;
extern bool inkey_scan;
extern bool inkey_flag;
extern bool shimmer_monsters;
extern bool repair_monsters;
extern s16b total_weight;
extern bool hack_mind;
extern bool hack_chaos_feature;
extern s16b o_max;
extern s16b o_cnt;
extern s16b m_max;
extern s16b m_cnt;
extern bool multi_rew;
extern bool rogue_like_commands;
extern bool quick_messages;
extern bool quick_prompt;
extern bool other_query_flag;
extern bool carry_query_flag;
extern bool use_old_target;
extern bool always_pickup;
extern bool always_repeat;
extern bool depth_in_feet;
extern bool stack_force_notes;
extern bool stack_force_notes_all;
extern bool stack_force_costs;
extern bool show_labels;
extern bool show_weights;
extern bool show_choices;
extern bool show_details;
extern bool show_choices_main;
extern bool ring_bell;
#endif
#if (defined(ANGBAND_H) || defined(Z_TERM_C))
extern bool use_color;
#endif
#if (defined(ANGBAND_H))
extern bool verbose_haggle;
extern bool scroll_edge;
extern bool show_piles;
extern bool beginner_help;
extern bool allow_fake_colour;
extern bool find_ignore_stairs;
extern bool find_ignore_doors;
extern bool find_cut;
extern bool stop_corner;
extern bool find_examine;
extern bool disturb_move;
extern bool disturb_near;
extern bool disturb_panel;
extern bool disturb_state;
extern bool disturb_dawn;
extern bool disturb_minor;
extern bool alert_failure;
extern bool last_words;
extern bool small_levels;
extern bool empty_levels;
extern bool equippy_chars;
extern bool skip_chaos_features;
extern bool plain_descriptions;
extern bool stupid_monsters;
extern bool auto_destroy;
extern bool confirm_stairs;
extern bool wear_confirm;
extern bool confirm_wear_all;
extern bool disturb_allies;
extern bool multi_stair;
extern bool unify_commands;
extern bool centre_view;
extern bool macro_edit;
extern bool no_centre_run;
extern bool track_mouse;
extern bool auto_more;
extern bool preserve_mode;
extern bool maximise_mode;
extern bool use_autoroller;
extern bool spend_points;
extern bool ironman_shop;
extern bool ironman_feeling;
extern bool speak_unique;
#endif
#if (defined(SCORE_QUITTERS)) && (defined(ANGBAND_H))
extern bool score_quitters;
#endif
#if (defined(ANGBAND_H))
extern bool chaos_patrons;
extern bool auto_haggle;
extern bool auto_scum;
extern bool stack_allow_items;
extern bool stack_allow_wands;
extern bool expand_look;
extern bool expand_list;
extern bool view_perma_grids;
extern bool view_torch_grids;
extern bool dungeon_align;
extern bool dungeon_stair;
extern bool dungeon_small;
extern bool flow_by_sound;
extern bool flow_by_smell;
extern bool smart_learn;
extern bool smart_cheat;
extern bool view_reduce_lite;
extern bool view_reduce_view;
extern bool avoid_abort;
extern bool avoid_other;
extern bool flush_error;
extern bool flush_failure;
extern bool flush_disturb;
extern bool fresh_before;
extern bool fresh_after;
extern bool fresh_message;
extern bool compress_savefile;
extern bool hilite_player;
extern bool view_yellow_lite;
extern bool view_bright_lite;
extern bool view_granite_lite;
extern bool view_special_lite;
extern bool testing_stack;
extern bool testing_carry;
extern bool spoil_art;
extern bool spoil_mon;
extern bool spoil_ego;
extern bool spoil_value;
extern bool spoil_base;
extern bool spoil_stat;
extern bool spoil_dam;
extern bool spoil_flag;
extern bool cheat_peek;
extern bool cheat_hear;
extern bool cheat_room;
extern bool cheat_xtra;
extern bool cheat_item;
extern bool cheat_live;
extern bool cheat_skll;
extern bool cheat_save;
extern bool allow_quickstart;
#endif
#if (defined(USE_MAIN_C)) && (defined(ANGBAND_H))
extern bool display_credits;
#endif
#if (defined(ANGBAND_H))
extern bool allow_pickstats;
extern s16b hitpoint_warn;
extern s16b delay_factor;
extern bool autosave_l;
extern bool autosave_t;
extern bool autosave_q;
extern s16b autosave_freq;
extern bool allow_squelch;
extern s16b feeling;
extern s16b rating;
extern bool new_level_flag;
extern int full_grid;
extern s16b max_panel_rows;
extern s16b max_panel_cols;
extern s16b panel_row;
extern s16b panel_col;
extern s16b panel_row_min;
extern s16b panel_row_max;
extern s16b panel_col_min;
extern s16b panel_col_max;
extern s16b panel_col_prt;
extern s16b panel_row_prt;
extern s16b py;
extern s16b px;
extern s16b wildx;
extern s16b wildy;
extern s16b target_who;
extern s16b health_who;
extern s16b monster_race_idx;
extern s16b object_kind_idx;
extern object_type *tracked_o_ptr;
extern co_ord tracked_co_ord;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char player_name[NAME_LEN];
extern char player_base[NAME_LEN];
extern cptr died_from;
extern char history[4][60];
extern char savefile[1024];
extern s16b temp_n;
extern byte temp_y[TEMP_MAX];
extern byte temp_x[TEMP_MAX];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern char *macro__buf;
extern cptr *quark__str;
extern u16b *message__ptr;
extern char *message__buf;
extern byte angband_color_table[256][4];
extern char angband_sound_name[SOUND_MAX][16];
extern cave_type *cave[MAX_HGT];
extern object_type *o_list;
extern monster_type *m_list;
extern store_type *store;
extern object_type *inventory;
extern s16b alloc_kind_size;
extern alloc_entry *alloc_kind_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern cptr keymap_act[KEYMAP_MODES][256];
extern player_type p_body;
extern player_type *p_ptr;
extern player_sex *sp_ptr;
extern player_race *rp_ptr;
extern player_template *cp_ptr;
extern byte spell_order[128];
extern s16b player_hp[100];
extern maxima *z_info;
extern vault_type *v_info;
extern cptr v_name;
extern cptr v_text;
extern feature_type *f_info;
extern cptr f_name;
extern feature_type **priority_table;
extern int feature_priorities;
extern object_kind *k_info;
extern cptr k_name;
extern cptr k_text;
extern unident_type *u_info;
extern char *u_name;
extern o_base_type *o_base;
extern cptr ob_name;
extern artifact_type *a_info;
extern cptr a_name;
extern ego_item_type *e_info;
extern cptr e_name;
extern monster_race *r_info;
extern char *r_name;
extern cptr r_text;
extern death_event_type *death_event;
extern cptr event_name;
extern cptr event_text;
extern dun_type *dun_defs;
extern cptr dun_name;
extern town_type *town_defs;
extern cptr town_name;
extern quest_type *q_list;
extern owner_type *owners;
extern cptr s_name;
extern player_template *template_info;
extern cptr tp_name;
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_GRAF;
extern cptr ANGBAND_DIR_APEX;
extern cptr ANGBAND_DIR_BONE;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_EDIT;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
extern cptr ANGBAND_DIR_INFO;
extern cptr ANGBAND_DIR_PREF;
extern cptr ANGBAND_DIR_SAVE;
extern cptr ANGBAND_DIR_USER;
extern cptr ANGBAND_DIR_XTRA;
extern bool (*item_tester_hook)(object_ctype*);
extern bool violet_uniques;
#endif
#if (defined(ALLOW_EASY_OPEN)) && (defined(ANGBAND_H))
extern bool easy_open;
#endif
#if (defined(ALLOW_EASY_DISARM)) && (defined(ANGBAND_H))
extern bool easy_disarm;
#endif
#if (defined(ANGBAND_H))
extern stat_default_type *stat_default;
extern s16b stat_default_total;
#endif
#if (defined(ALLOW_TEMPLATES)) && (defined(ANGBAND_H))
extern init_macro_type *macro_info;
extern char *macro_name;
extern char *macro_text;
extern u16b rebuild_raw;
#endif
#if (defined(ANGBAND_H))
extern byte object_skill_count;
#endif

/* wizard1.c */

#if (defined(ALLOW_SPOILERS)) && (defined(ANGBAND_H))
extern void do_cmd_spoilers(void);
#endif

/* wizard2.c */

#if (defined(ALLOW_WIZARD)) && (defined(ANGBAND_H))
extern void do_cmd_wiz_hack_ben(void);
#endif
#if ((defined(ALLOW_WIZARD)) && defined(MONSTER_HORDES)) && (defined(ANGBAND_H))
extern void do_cmd_summon_horde(void);
#endif
#if (defined(ALLOW_WIZARD)) && (defined(ANGBAND_H))
extern void do_cmd_wiz_bamf(void);
extern void do_cmd_wiz_change(void);
extern void wiz_create_named_art(int a_idx);
extern void do_cmd_wiz_play(object_type *o_ptr);
extern void wiz_create_item(int k_idx);
extern void do_cmd_wiz_cure_all(void);
extern void do_cmd_wiz_jump(int level);
extern void do_cmd_wiz_learn(int max_level);
extern void do_cmd_wiz_summon(int num);
extern void do_cmd_wiz_named(int r_idx, bool slp);
extern void do_cmd_wiz_named_friendly(int r_idx, bool slp);
extern void do_cmd_wiz_zap(void);
extern void do_cmd_magebolt(void);
extern void do_cmd_debug(void);
#endif

/* xtra1.c */

#if (defined(ANGBAND_H))
extern void day_to_date_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern void cnv_stat_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern s16b modify_stat_value(int value, int amount);
extern bool PURE cumber_glove(object_ctype *o_ptr);
extern bool PURE cumber_helm(object_ctype *o_ptr);
extern int PURE wield_skill(object_ctype *o_ptr);
extern bool PURE player_has_flag(int set, u32b flag);
extern void player_flags(u32b *f1, u32b *f2, u32b *f3);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern display_func_type display_func[NUM_DISPLAY_FUNCS+1];
extern void window_stuff(void);
extern void toggle_inven_equip(void);
#endif
#if (defined(ANGBAND_H) || defined(Z_TERM_C))
extern void event_stuff(void);
#endif
#if (defined(ANGBAND_H))
extern void resize_window(void);
extern void handle_stuff(void);
extern bool ma_empty_hands(void);
extern bool PURE skill_check_possible(player_skill *sk_ptr);
extern void skill_exp(int index);
extern int find_object(object_ctype *o_ptr);
extern void update_objects(int where);
extern void update_object(object_type *o_ptr);
#endif

/* xtra2.c */

#if (defined(CHECK_ARRAYS)) && (defined(ANGBAND_H))
extern void check_temp_effects(void);
#endif
#if (defined(ANGBAND_H))
extern bool add_flag(int flag, int v);
extern bool set_flag(int flag, int v);
extern cptr PURE prt_flag(int flag);
extern cptr PURE prt_flag_long(int flag);
extern void prt_timers(void);
extern void gain_exp(s32b amount);
extern void gain_skills(s32b amount);
extern void lose_skills(s32b amount);
extern void monster_death(int m_idx);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, cptr note);
extern void panel_bounds(void);
extern void panel_bounds_center(void);
extern void verify_panel(bool force);
extern void resize_map(void);
extern void resize_inkey(void);
extern void ang_sort(vptr u, vptr v, int n, bool (*comp)(vptr, vptr, int, int), void (*swap)(vptr, vptr, int, int));
extern bool target_set(int mode);
extern bool get_aim_dir(int *dp);
extern bool get_hack_dir(int *dp);
extern bool get_rep_dir(int *dp);
extern bool get_rep_target(int *x, int *y);
extern void get_dir_target(int *x, int *y, int dir, int (*okay)(int, int, int));
extern void convert_articles(char *str);
extern void feature_desc_f2(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern void gain_level_reward(int effect, int skill);
extern void init_chaos(void);
extern bool PURE p_mutated(void);
extern void p_clear_mutations(void);
extern bool PURE p_has_mutation(int idx);
extern bool gain_chaos_feature(int choose_mut);
extern bool lose_chaos_feature(int choose_mut);
extern int add_chaos_features(cptr *info, bool (*reject)(int));
extern void dump_chaos_features(FILE * OutFile);
#endif

/* z-form.c */

#if (defined(ANGBAND_H) || defined(Z_FORM_C))
extern uint vstrnfmt(char *buf, uint max, cptr fmt, va_list vp);
extern void format_fn(char *buf, uint max, cptr UNUSED fmt, va_list *vp);
extern char *vformat(cptr fmt, va_list vp);
extern uint strnfmt(char *buf, uint max, cptr fmt, ...);
extern char *format(cptr fmt, ...);
extern void plog_fmt(cptr fmt, ...);
extern void quit_fmt(cptr fmt, ...);
extern void core_fmt(cptr fmt, ...);
#endif

/* z-rand.c */

#if (defined(ANGBAND_H) || defined(Z_RAND_C))
extern bool rand_unbiased;
extern bool Rand_quick;
extern u32b Rand_value;
extern u16b Rand_place;
extern u32b Rand_state[RAND_DEG];
extern void Rand_state_init(u32b seed);
extern s16b randnor(int mean, int stand);
#endif
#if (defined(ANGBAND_H) || defined(Z_RAND_H) || defined(Z_RAND_C))
extern s32b rand_int(u32b m);
#endif
#if (defined(ANGBAND_H) || defined(Z_RAND_C))
extern bool percent(int m);
extern s16b damroll(int num, int sides);
extern s16b maxroll(int num, int sides);
extern u32b Rand_simple(u32b m);
#endif

/* z-term.c */

#if (defined(ANGBAND_H) || defined(Z_TERM_C))
extern term *Term;
extern void Term_user(void);
extern void Term_xtra(int n, int v);
extern void Term_queue_char(int x, int y, byte a, char c, byte ta, char tc);
extern void Term_fresh(void);
extern void Term_set_cursor(bool v);
extern errr Term_gotoxy(int x, int y);
extern void Term_draw(int x, int y, byte a, char c);
extern void Term_addch(byte a, char c);
extern void Term_addstr(int n, byte a, cptr s);
extern void Term_putch(int x, int y, byte a, char c);
extern void Term_putstr(int x, int y, int n, byte a, cptr s);
extern errr Term_erase(int x, int y, int n);
extern void Term_clear(void);
extern void Term_redraw(void);
extern void Term_redraw_section(int x1, int y1, int x2, int y2);
extern void Term_get_cursor(bool *v);
extern void Term_get_size(int *w, int *h);
extern void Term_locate(int *x, int *y);
extern void Term_what(int x, int y, byte *a, char *c);
extern void Term_flush(void);
extern errr Term_keypress(int k);
extern errr Term_key_push(int k);
extern int Term_queue_space(void);
extern errr Term_inkey(char *ch, bool wait, bool take);
extern void init_term_wins(void);
extern void Term_release(int win);
extern int Term_save_aux(void);
extern void Term_save(void);
extern void Term_load_aux(int win);
extern void Term_load(void);
extern void Term_resize(int w, int h);
extern void Term_activate(term *t);
extern void term_nuke(term *t);
extern void term_init(term *t, int w, int h, int k);
#endif

/* z-util.c */

#if (defined(ANGBAND_H) || defined(Z_UTIL_C))
extern cptr argv0;
extern void func_nothing(void);
extern bool func_true(void);
extern bool func_false(void);
extern bool suffix(cptr s, cptr t);
extern bool prefix(cptr s, cptr t);
extern void (*plog_aux)(cptr);
#endif
#if (defined(ANGBAND_H) || defined(Z_FORM_C) || defined(Z_UTIL_C))
extern void plog(cptr str);
#endif
#if (defined(ANGBAND_H) || defined(Z_UTIL_C))
extern void (*quit_aux)(cptr);
#endif
#if (defined(ANGBAND_H) || defined(Z_FORM_C) || defined(Z_UTIL_C) || defined(Z_VIRT_C))
extern void quit(cptr str);
#endif
#if (defined(ANGBAND_H) || defined(Z_UTIL_C))
extern void (*core_aux)(cptr);
#endif
#if (defined(ANGBAND_H) || defined(Z_FORM_C) || defined(Z_UTIL_C) || defined(Z_VIRT_C))
extern void core(cptr str);
#endif

/* z-virt.c */

#if (defined(ANGBAND_H) || defined(Z_VIRT_C))
extern vptr (*rnfree_aux)(vptr);
#endif
#if (defined(Z_VIRT_H) || defined(Z_VIRT_C))
extern vptr rnfree(vptr p);
#endif
#if (defined(ANGBAND_H) || defined(Z_VIRT_C))
extern vptr rpanic_none(huge UNUSED len);
extern vptr (*rpanic_aux)(huge);
extern vptr (*ralloc_aux)(huge);
#endif
#if (defined(ANGBAND_H) || defined(Z_VIRT_H) || defined(Z_VIRT_C))
extern vptr ralloc(huge len);
#endif
#if (defined(ANGBAND_H) || defined(Z_VIRT_C))
extern char *string_make(cptr str);
extern void safe_free(vptr p);
extern cptr safe_string_make(cptr str);
#endif
#endif /* INCLUDED_EXTERNS_H */
