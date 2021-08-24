/*
 * File: netclient.h
 * Purpose: The client side of the networking stuff
 */

extern bool send_quit;
extern struct angband_constants z_info_struct;
extern u16b flavor_max;
extern s16b section_icky_col;
extern byte section_icky_row;
extern bool allow_disturb_icky;

/*** Utilities ***/
extern int Flush_queue(void);
extern void do_keepalive(void);
extern void check_term_resize(bool main_win, int *cols, int *rows);
extern void net_term_resize(int cols, int rows, int max_rows);

/*** Sending ***/
extern int Send_features(int lighting, int off);
extern int Send_verify(int type);
extern int Send_icky(void);
extern int Send_symbol(const char *buf);
extern int Send_poly_race(const char *buf);
extern int Send_breath(struct command *cmd);
extern int Send_walk(struct command *cmd);
extern int Send_run(struct command *cmd);
extern int Send_tunnel(struct command *cmd);
extern int Send_aim(struct command *cmd);
extern int Send_drop(struct command *cmd);
extern int Send_fire(struct command *cmd);
extern int Send_pickup(struct command *cmd);
extern int Send_autopickup(struct command *cmd);
extern int Send_hold(struct command *cmd);
extern int Send_destroy(struct object *obj, bool des);
extern int Send_target_closest(int mode);
extern int Send_cast(int book, int spell, int dir);
extern int Send_open(struct command *cmd);
extern int Send_quaff(struct command *cmd);
extern int Send_read(struct command *cmd);
extern int Send_take_off(struct command *cmd);
extern int Send_use(struct command *cmd);
extern int Send_throw(struct command *cmd);
extern int Send_wield(struct command *cmd);
extern int Send_zap(struct command *cmd);
extern int Send_target_interactive(int mode, keycode_t query);
extern int Send_inscribe(struct command *cmd);
extern int Send_uninscribe(struct command *cmd);
extern int Send_activate(struct command *cmd);
extern int Send_disarm(struct command *cmd);
extern int Send_eat(struct command *cmd);
extern int Send_fill(struct command *cmd);
extern int Send_locate(int dir);
extern int Send_map(byte mode);
extern int Send_toggle_stealth(struct command *cmd);
extern int Send_quest(void);
extern int Send_close(struct command *cmd);
extern int Send_gain(struct command *cmd);
extern int Send_go_up(struct command *cmd);
extern int Send_go_down(struct command *cmd);
extern int Send_drop_gold(s32b amt);
extern int Send_redraw(void);
extern int Send_rest(s16b resting);
extern int Send_ghost(int ability, int dir);
extern int Send_suicide(void);
extern int Send_steal(struct command *cmd);
extern int Send_master(s16b command, const char *buf);
extern int Send_mimic(int page, int spell, int dir);
extern int Send_clear(void);
extern int Send_observe(struct command *cmd);
extern int Send_store_examine(int item, bool describe);
extern int Send_alter(struct command *cmd);
extern int Send_fire_at_nearest(void);
extern int Send_jump(struct command *cmd);
extern int Send_social(const char *buf, int dir);
extern int Send_monlist(void);
extern int Send_feeling(void);
extern int Send_interactive(int type, keycode_t ch);
extern int Send_fountain(int item);
extern int Send_time(void);
extern int Send_objlist(void);
extern int Send_center_map(void);
extern int Send_toggle_ignore(void);
extern int Send_use_any(struct command *cmd);
extern int Send_store_order(const char *buf);
extern int Send_track_object(int item);
extern int Send_play(int mode);
extern int Send_text_screen(int type, s32b off);
extern int Send_keepalive(void);
extern int Send_char_info(void);
extern int Send_options(bool settings);
extern int Send_char_dump(void);
extern int Send_msg(const char *message);
extern int Send_item(int item, int curse);
extern int Send_store_sell(int item, int amt);
extern int Send_party(s16b command, const char *buf);
extern int Send_special_line(int type, int line);
extern int Send_fullmap(void);
extern int Send_poly(int number);
extern int Send_store_purchase(int item, int amt);
extern int Send_purchase_house(int dir);
extern int Send_store_leave(void);
extern int Send_store_confirm(void);
extern int Send_ignore(void);
extern int Send_flush(void);
extern int Send_chan(const char *channel);
extern int Send_history(int line, const char *hist);
extern int Send_autoinscriptions(void);

/*** Commands ***/
extern int textui_spell_browse(struct command *cmd);
extern int cmd_cast(struct command *cmd);
extern int cmd_project(struct command *cmd);

/*** General network functions ***/
extern int Net_packet(void);
extern int Net_init(int fd);
extern void Net_cleanup(void);
extern int Net_flush(void);
extern int Net_fd(void);
extern int Net_input(void);
extern bool Net_Send(int Socket, sockbuf_t* ibuf);
extern bool Net_WaitReply(int Socket, sockbuf_t* ibuf, int retries);


