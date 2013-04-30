/*
 * File: netclient.h
 * Purpose: The client side of the networking stuff
 */

#include "game-cmd.h"

#define MIN_RECEIVE_WINDOW_SIZE     1
#define MAX_RECEIVE_WINDOW_SIZE     4

#define CONN_PLAYING    1
#define CONN_SETUP      2

extern bool send_quit;

extern int Flush_queue(void);
extern void do_keepalive();
extern void check_term_resize(bool main_win, int *cols, int *rows);
extern void net_term_resize(int cols, int rows, int max_rows);

extern int Net_packet(void);
extern int Net_verify(void);
extern int Net_init(int fd);
extern void Net_cleanup(void);
extern int Net_flush(void);
extern int Net_fd(void);
extern int Net_input(void);
extern bool Net_Send(int Socket, sockbuf_t* ibuf);
extern bool Net_WaitReply(int Socket, sockbuf_t* ibuf, int retries);

extern int Send_search(cmd_arg args[]);
extern int Send_walk(cmd_arg args[]);
extern int Send_jump(cmd_arg args[]);
extern int Send_run(cmd_arg args[]);
extern int Send_drop(cmd_arg args[]);
extern int Send_drop_gold(s32b amt);
extern int Send_tunnel(cmd_arg args[]);
extern int Send_pickup(cmd_arg args[]);
extern int Send_quest(void);
extern int Send_toggle_search(cmd_arg args[]);
extern int Send_rest(s16b resting);
extern int Send_go_up(cmd_arg args[]);
extern int Send_go_down(cmd_arg args[]);
extern int Send_open(cmd_arg args[]);
extern int Send_close(cmd_arg args[]);
extern int Send_bash(cmd_arg args[]);
extern int Send_alter(cmd_arg args[]);
extern int Send_spike(cmd_arg args[]);
extern int Send_disarm(cmd_arg args[]);
extern int Send_wield(cmd_arg args[]);
extern int Send_take_off(cmd_arg args[]);
extern int Send_destroy(int item, bool des);
extern int Send_ignore(void);
extern int Send_inscribe(cmd_arg args[]);
extern int Send_uninscribe(cmd_arg args[]);
extern int Send_steal(cmd_arg args[]);
extern int Send_quaff(cmd_arg args[]);
extern int Send_read(cmd_arg args[]);
extern int Send_aim(cmd_arg args[]);
extern int Send_use(cmd_arg args[]);
extern int Send_zap(cmd_arg args[]);
extern int Send_fill(cmd_arg args[]);
extern int Send_eat(cmd_arg args[]);
extern int Send_activate(cmd_arg args[]);
extern int Send_target_interactive(int mode, keycode_t query);
extern int Send_target_closest(int mode);
extern int Send_msg(const char *message);
extern int Send_fire(cmd_arg args[]);
extern int Send_fire_at_nearest(void);
extern int Send_throw(int item, int dir);
extern int Send_item(int item);
extern int Send_gain(cmd_arg args[]);
extern int Send_cast(int book, int spell, int dir);
extern int Send_pray(int book, int spell, int dir);
extern int Send_ghost(int ability, int dir);
extern int Send_map(byte mode);
extern int Send_fullmap(void);
extern int Send_locate(int dir);
extern int Send_store_purchase(int item, int amt);
extern int Send_store_examine(int item);
extern int Send_store_order(const char *buf);
extern int Send_store_sell(int item, int amt);
extern int Send_store_leave(void);
extern int Send_store_confirm(void);
extern int Send_redraw(void);
extern int Send_special_line(int type, int line);
extern int Send_party(s16b command, const char *buf);
extern int Send_purchase_house(int dir);
extern int Send_suicide(void);
extern int Send_options(bool settings);
extern int Send_master(s16b command, const char *buf);
extern int Send_observe(cmd_arg args[]);
extern int Send_clear();
extern int Send_pass(const char *newpass);
extern int Send_symbol(const char *buf);
extern int Send_objlist();
extern int Send_monlist();
extern int Send_text_screen(int type, s32b off);
extern int Send_play(int mode);
extern int Send_poly(int number);
extern int Send_social(const char *buf, int dir);
extern int Send_feeling(void);
extern int Send_breath(cmd_arg args[]);
extern int Send_mimic(int page, int spell, int dir);
extern int Send_chan(const char *channel);
extern int Send_interactive(int type, keycode_t ch);
extern int Send_fountain(int item);
extern int Send_icky(void);
extern int Send_center_map(void);
extern int Send_use_any(cmd_arg args[]);


