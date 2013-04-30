/*
 * File: netserver.h
 * Purpose: The server side of the network stuff
 */

#ifndef __Netserver_h
#define __Netserver_h

#define CONN_FREE       0x00
#define CONN_XXX1       0x01
#define CONN_SETUP      0x02
#define CONN_XXX2       0x04
#define CONN_PLAYING    0x08
#define CONN_QUIT       0x10
#define CONN_CONSOLE    0x20

#define FREE_TIMEOUT    15
#define SETUP_TIMEOUT   180
#define PLAY_TIMEOUT    30
#define QUIT_TIMEOUT    5

typedef struct
{
    int             state;
    sockbuf_t       r;
    sockbuf_t       w;
    sockbuf_t       c;
    sockbuf_t       q;
    hturn           start;
    long            timeout;
    int             client_setup;
    u16b            conntype;
    byte            char_state;
    int             id;
    unsigned        version;
    char            *real;
    char            *nick;
    char            *addr;
    char            *host;
    char            *pass;
    byte            ridx;
    byte            cidx;
    byte            psex;
    s16b            stat_roll[A_MAX+1];
    client_setup_t  Client_setup;
    bool            console_authenticated;
    bool            console_listen;
    byte            console_channels[MAX_CHANNELS];
    u32b            account;
} connection_t;

extern connection_t *get_connection(long idx);
extern long get_player_index(connection_t *connp);
extern void set_player_index(connection_t *connp, long idx);

extern int Setup_net_server(void);
extern void setup_contact_socket(void);
extern bool Report_to_meta(int flag);
extern bool Destroy_connection(int Ind, char *reason);
extern void Stop_net_server(void);
extern int process_pending_commands(int Ind);
extern void* console_buffer(int ind, bool read);
extern bool Conn_is_alive(int ind);
extern void Conn_set_console_setting(int ind, int set, bool val);
extern bool Conn_get_console_setting(int ind, int set);
extern int Init_setup(void);
extern byte* Conn_get_console_channels(int ind);

extern int Net_input(void);
extern int Net_output(void);
extern int Net_output_p(int Ind);

extern int Send_game_start_conn(int ind);
extern int Send_text_screen(int ind, int type, s32b offset);
extern int Send_basic_info_conn(int ind);
extern int Send_death_cause(struct player *p);
extern int Send_winner(struct player *p);
extern int Send_limits_info_conn(int ind);
extern int Send_inven_info_conn(int ind);
extern int Send_race_info_conn(int ind);
extern int Send_class_info_conn(int ind);
extern int Send_socials_info_conn(int ind);
extern int Send_kind_info_conn(int ind);
extern int Send_hints_info_conn(int ind);
extern int Send_rinfo_info_conn(int ind);
extern int Send_char_info_conn(int ind);
extern int Send_plusses(struct player *p, int tofhit, int tofdam, int tomhit, int tomdam,
    int toshit, int tosdam);
extern int Send_ac(struct player *p, int base, int plus);
extern int Send_lvl(struct player *p, int lev, int mlev);
extern int Send_exp(struct player *p, s32b max, s32b cur, s16b expfact);
extern int Send_gold(struct player *p, s32b gold);
extern int Send_hp(struct player *p, int mhp, int chp);
extern int Send_sp(struct player *p, int msp, int csp);
extern int Send_char_info(struct player *p, byte ridx, byte cidx, byte psex);
extern int Send_various(int Ind, int height, int weight, int age, int sc);
extern int Send_stat(struct player *p, int stat, int stat_top, int stat_use, int stat_max,
    int stat_add, int stat_cur);
extern int Send_objflags(struct player *p, int line);
extern int Send_history(int Ind, int line, const char *hist);
extern int Send_inven(struct player *p, char pos, byte attr, int wgt, s32b price, int amt,
    byte tval, byte sval, byte act, byte fuel, byte fail, int slot, const char *name);
extern int Send_equip(struct player *p, char pos, byte attr, int wgt, s32b price, int amt,
    byte tval, byte sval, byte act, byte fuel, byte fail, int slot, const char *name);
extern int Send_title(struct player *p, const char *title);
extern int Send_turn(int Ind, u32b game_turn, u32b player_turn, u32b active_turn);
extern int Send_depth(struct player *p, int depth, int maxdepth);
extern int Send_food(struct player *p, int food);
extern int Send_status(struct player *p, s16b *effects);
extern int Send_speed(struct player *p, int speed);
extern int Send_dtrap(struct player *p, byte dtrap);
extern int Send_study(struct player *p, int study, bool can_study_book);
extern int Send_message(struct player *p, const char *msg, u16b typ);
extern int Send_char(struct player *p, int x, int y, byte a, char c, byte ta, char tc);
extern int Send_spell_info(struct player *p, int book, int i, const char *out_val,
    spell_flags *flags);
extern int Send_spell_desc(struct player *p, int book, int i, char *out_val);
extern int Send_item_request(struct player *p, byte tester_tval, byte tester_hook);
extern int Send_recall(struct player *p, s16b word_recall);
extern int Send_state(struct player *p, bool searching, bool resting, bool unignoring);
extern int Send_flush(struct player *p, bool fresh, bool delay);
extern int Send_line_info(struct player *p, int y);
extern int Send_fullmap(int Ind, int y);
extern int Send_mini_map(struct player *p, int y, s16b w);
extern int Send_store(int ind, char pos, byte attr, s16b wgt, byte number,
    byte owned, s32b price, byte tval, byte max, const char *name);
extern int Send_store_info(int Ind, int num, char *name, char *owner, int items,
    s32b purse);
extern int Send_store_leave(struct player *p);
extern int Send_store_sell(int Ind, s32b price, bool reset);
extern int Send_target_info(int ind, int x, int y, bool dble, const char *buf);
extern int Send_sound(struct player *p, int sound);
extern int Send_special_line(int ind, int max, int last, int line, byte attr,
    const char *buf);
extern int Send_floor(struct player *p, byte num, int o_idx, byte attr, int amt, byte tval,
    byte sval, byte act, byte fuel, byte fail, int slot, const char *name);
extern int Send_show_floor(int ind, byte mode);
extern int Send_quiver_size(struct player *p, u16b quiver_size, u16b quiver_slots,
    u16b quiver_remainder);
extern int Send_party(struct player *p);
extern int Send_special_other(struct player *p, char *header, byte peruse, bool protect);
extern int Send_skills(struct player *p);
extern int Send_pause(struct player *p);
extern int Send_monster_health(struct player *p, int num, byte attr);
extern int Send_cursor(struct player *p, char vis, char x, char y);
extern int Send_poly(struct player *p, int r_idx);
extern int Send_weight(struct player *p, int weight, int max_weight);
extern int Send_channel(int ind, byte n, const char *virt);
extern int Send_term_info(struct player *p, int mode, u16b arg);
extern int Send_remote_line(struct player *p, int y);
extern int Send_reliable(int ind);
extern int Send_player_pos(struct player *p);

#endif
