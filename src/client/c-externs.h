/*
 * File: c-externs.h
 * Purpose: Extern declarations (variables and functions)
 */

#ifndef INCLUDED_C_EXTERNS_H
#define INCLUDED_C_EXTERNS_H

/* c-birth.c */
extern void player_birth(void);
extern bool get_server_name(void);
extern void get_account_name(void);
extern void get_char_name(void);

/* c-cmd0.c */
extern bool cmd_target_interactive(int mode);
extern void cmd_chat_close(int i);
extern void cmd_chat_cycle(int dir);
extern void free_command_menu(void);

/* c-cmd-obj.c */
extern bool obj_can_wear(struct player *p, const object_type *o_ptr);
extern int textui_obj_wield(int item);
extern int textui_cmd_drop(int item);
extern bool obj_can_browse(struct player *p, const object_type *o_ptr);
extern bool obj_browse_pre(void);
extern bool obj_can_study(struct player *p, const object_type *o_ptr);
extern bool obj_study_pre(void);
extern bool obj_can_cast_from(struct player *p, const object_type *o_ptr);
extern bool obj_cast_pre(void);
extern bool obj_is_staff(struct player *p, const object_type *o_ptr);
extern bool obj_is_wand(struct player *p, const object_type *o_ptr);
extern bool obj_is_rod(struct player *p, const object_type *o_ptr);
extern bool obj_can_activate(struct player *p, const object_type *o_ptr);
extern bool obj_is_food(struct player *p, const object_type *o_ptr);
extern bool obj_is_potion(struct player *p, const object_type *o_ptr);
extern bool obj_is_scroll(struct player *p, const object_type *o_ptr);
extern bool item_tester_hook_fire(struct player *p, const object_type *o_ptr);
extern bool obj_is_useable(struct player *p, const object_type *o_ptr);
extern int textui_cmd_zap_rod(int item);
extern int textui_cmd_activate(int item);
extern int textui_cmd_use_any(int item);
extern bool obj_can_refill(struct player *p, const object_type *o_ptr);
extern bool obj_refill_pre(void);

/* c-files.c */
extern void init_file_paths(const char *configpath, const char *libpath, const char *datapath);
extern void init_extra_paths(void);
extern void free_file_paths(void);
extern void show_splashscreen(void);
extern bool peruse_file(void);
extern void conf_init(void* param);
extern void conf_save(void);
extern bool conf_section_exists(const char *section);
extern const char *conf_get_string(const char *section, const char *name, const char *default_value);
extern s32b conf_get_int(const char *section, const char *name, s32b default_value);
extern void conf_set_string(const char *section, const char *name, const char *value);
extern void conf_set_int(const char *section, const char *name, s32b value);
extern void conf_append_section(const char *sectionFrom, const char *sectionTo, const char *filename);
extern void conf_timer(int ticks);
extern bool conf_exists(void);
extern void print_tomb(void);
extern void display_winner(void);

/* c-init.c */
extern void initialize_all_pref_files(void);
extern void client_init(char *argv1);
extern bool client_ready(void);
extern bool gather_settings();
extern void cleanup_angband(void);
extern void init_stuff(void);

/* c-obj-ui.c */
extern void show_inven(olist_detail_t mode);
extern void show_equip(olist_detail_t mode);
extern void show_floor(olist_detail_t mode);
extern object_type *object_from_item_idx(int item, char *o_name, size_t len);

/* c-store.c */
extern void do_cmd_store(void);
extern void store_prt_gold(void);
extern void store_prt_frame(void);
extern void store_sell_accept(s32b price, s16b reset);
extern void store_purchase_end(void);
extern void store_sell_end(void);
extern void store_leave(void);
extern bool check_store_leave(bool refresh);

/* c-tables.c */
extern s16b ddx[10];
extern s16b ddy[10];
extern const char *window_flag_desc[PW_MAX_FLAGS];

/* c-util.c */
extern bool inkey_xtra;
extern char inkey_scan;
extern bool inkey_flag;
extern struct keypress *inkey_next;
extern void flush(void);
extern ui_event inkey_ex(void);
extern struct keypress inkey(void);
extern void bell(const char *reason);
extern void sound(int val);
extern void c_msg_print_aux(const char *msg, u16b type);
extern void c_msg_print(const char *msg);
extern void screen_save(void);
extern void screen_load(bool flush);
extern void c_put_str(byte attr, const char *str, int row, int col);
extern void put_str(const char *str, int row, int col);
extern void c_prt(byte attr, const char *str, int row, int col);
extern void prt(const char *str, int row, int col);
extern void prt_icky(const char *str, int row, int col);
extern void clear_from(int row);
extern bool askfor_aux_keypress(char *buf, size_t buflen, size_t *curs, size_t *len,
    struct keypress keypress, bool firsttime);
extern bool askfor_aux(char *buf, int len, keypress_handler keypress_h);
extern int askfor_ex(char *buf, int len, keypress_handler keypress_h, bool priv);
extern bool get_string(const char *prompt, char *buf, int len);
extern int get_string_ex(const char *prompt, char *buf, int len, bool priv);
extern s32b get_quantity(const char *prompt, s32b max);
extern s32b get_quantity_ex(const char *prompt, s32b max);
extern bool get_check(const char *prompt);
extern int get_check_ex(const char *prompt);
extern bool get_com(const char *prompt, struct keypress *command);
extern bool get_com_ex(const char *prompt, ui_event *command);
extern void flush_now(void);
extern void flush_hack(void);
extern int target_dir(struct keypress ch);
extern bool get_aim_dir(int *dp);
extern int get_aim_dir_ex(int *dp);
extern void prt_num(const char *header, int num, int row, int col, byte color);
extern void prt_lnum(const char *header, s32b num, int row, int col, byte color);
extern void caveprt(cave_view_type* src, int len, s16b x, s16b y);
extern void cavestr(cave_view_type* dest, const char *src, byte attr, int max_col);
extern const char *extract_file_name(const char *s);
extern ui_event Net_loop(errr (*inkey_handler)(ui_event*, bool, bool),
    void (*callback_begin)(ui_event*), void (*callback_end)(void), char scan_cutoff);
extern void text_out_e(const char *buf, int y);
extern void reset_visuals(bool load_prefs);
extern void turn_off_numlock(void);

/* c-variable.c */
extern term *angband_term[ANGBAND_TERM_MAX];
extern const char *angband_term_name[ANGBAND_TERM_MAX];
extern byte angband_color_table[MAX_COLORS][4];
extern const char *angband_sound_name[MSG_MAX];
extern player_type player;
extern player_type *p_ptr;
extern char title[NORMAL_WID];
extern maxima z_info_struct;
extern u16b flavor_max;
extern u16b v_max;
extern const char *ANGBAND_SYS;
extern const char *ANGBAND_GRAF;
extern char *ANGBAND_DIR_XTRA;
extern char *ANGBAND_DIR_XTRA_FONT;
extern char *ANGBAND_DIR_XTRA_GRAF;
extern char *ANGBAND_DIR_XTRA_ICON;
extern char *ANGBAND_DIR_XTRA_SOUND;
extern void (*sound_hook)(int);
extern byte lazymove_delay;
extern char meta_address[NORMAL_WID];
extern char nick[NORMAL_WID];
extern char pass[NORMAL_WID];
extern char stored_pass[NORMAL_WID];
extern char real_name[NORMAL_WID];
extern char server_name[NORMAL_WID];
extern int server_port;
extern char **inventory_name;
extern byte inven_pack;
extern byte inven_wield;
extern byte inven_bow;
extern byte inven_left;
extern byte inven_light;
extern byte inven_total;
extern byte quiver_start;
extern byte all_inven_total;
extern byte max_stack_size;
extern char **eq_name;
extern struct store current_store;
extern char store_name[NORMAL_WID];
extern char store_names[STORE_INVEN_MAX][NORMAL_WID];
extern object_type floor_item[MAX_FLOOR_STACK];
extern char floor_name[MAX_FLOOR_STACK][NORMAL_WID];
extern byte floor_num;
extern char spell_info[BOOKS_PER_REALM][SPELLS_PER_BOOK][NORMAL_WID];
extern spell_flags spell_flag[BOOKS_PER_REALM][SPELLS_PER_BOOK];
extern char spell_desc[BOOKS_PER_REALM][SPELLS_PER_BOOK][MSG_LEN];
extern char party_info[160];
extern server_setup_t Setup;
extern client_setup_t Client_setup;
extern bool shopping;
extern bool map_active;
extern s16b last_line_info;
extern s16b max_line;
extern s16b cur_line;
extern int health_amt;
extern byte health_attr;
extern u32b lag_mark;
extern byte trap_indicator;
extern int special_line_type;
extern char special_line_header[ANGBAND_TERM_MAX][NORMAL_WID];
extern bool first_escape;
extern u32b window_flag[ANGBAND_TERM_MAX];
extern s16b stat_roll[A_MAX + 1];
extern bool topline_icky;
extern bool party_mode;
extern s16b section_icky_col;
extern byte section_icky_row;
extern int dis_to_mhit;
extern int dis_to_mdam;
extern int dis_to_shit;
extern int dis_to_sdam;
extern channel_type channels[MAX_CHANNELS];
extern s16b view_channel;
extern cave_view_type remote_info[ANGBAND_TERM_MAX][MAX_TXT_INFO][NORMAL_WID];
extern s16b last_remote_line[ANGBAND_TERM_MAX];
extern u16b char_num;
extern char **char_name;
extern char *char_expiry;
extern bool full_icky_screen;
extern bool target_icky_screen;

/* c-xtra.c */
extern void display_player_screen(bool mode);
extern void redraw_stuff(void);
extern int find_whisper_tab(const char *msg, char *text, size_t len);
extern void message_color_hack(const char *msg, byte *ap);
extern void subwindows_set_flags(u32b *new_flags, size_t n_subwindows);
extern void subwindows_init_flags(void);
extern void subwindows_reinit_flags(void);

/* cmd-context.c */
extern int context_menu_object(const object_type *o_ptr, const int slot, const char *desc);

/* set_focus.c */
extern void set_chat_focus(void);
extern void unset_chat_focus(void);
extern void stretch_chat_ctrl(void);

/* ui-options.c */
extern void do_cmd_keymaps(const char *title, int row);
extern void do_cmd_options(void);
extern void free_option_menus(void);

/* ui-spell.c */
extern bool spell_okay_to_study(int book, int spell);
extern bool spell_okay_to_cast(int book, int spell);

#ifdef SUPPORT_GAMMA
extern void build_gamma_table(int gamma);
extern byte gamma_table[256];
#endif /* SUPPORT_GAMMA */

#endif /* INCLUDED_C_EXTERNS_H */
