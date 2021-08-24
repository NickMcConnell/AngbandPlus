/*
 * File: netserver.h
 * Purpose: The server side of the network stuff
 */

#ifndef __Netserver_h
#define __Netserver_h

#define FREE_TIMEOUT    15
#define SETUP_TIMEOUT   180
#define PLAY_TIMEOUT    30
#define QUIT_TIMEOUT    5

/*
 * The types of communication that we send to the metaserver
 */
#define META_START  0x01
#define META_DIE    0x02
#define META_UPDATE 0x04

/*
 * Special codes corresponding to action request.
 */
#define ACTION_PICKUP   1
#define ACTION_GO_DOWN  2

/* Mental links */
#define LINK_NONE       0
#define LINK_DOMINANT   1
#define LINK_DOMINATED  2

typedef struct
{
    int             state;
    sockbuf_t       r;
    sockbuf_t       w;
    sockbuf_t       c;
    sockbuf_t       q;
    hturn           start;
    long            timeout;
    bool            has_setup;
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
    s16b            stat_roll[STAT_MAX + 1];
    client_setup_t  Client_setup;
    bool            console_authenticated;
    bool            console_listen;
    byte            console_channels[MAX_CHANNELS];
    u32b            account;
} connection_t;

/*** Player connection/index wrappers ***/
extern connection_t *get_connection(long idx);
extern long get_player_index(connection_t *connp);
extern void set_player_index(connection_t *connp, long idx);

/*** General utilities ***/
extern int Setup_net_server(void);
extern void setup_contact_socket(void);
extern bool Report_to_meta(int flag);
extern bool Destroy_connection(int ind, char *reason);
extern void Stop_net_server(void);
extern void* console_buffer(int ind, bool read);
extern bool Conn_is_alive(int ind);
extern void Conn_set_console_setting(int ind, int set, bool val);
extern bool Conn_get_console_setting(int ind, int set);
extern int Init_setup(void);
extern byte* Conn_get_console_channels(int ind);

/*** Sending ***/
extern int Send_basic_info(int ind);
extern int Send_limits_struct_info(int ind);
extern int Send_race_struct_info(int ind);
extern int Send_class_struct_info(int ind);
extern int Send_body_struct_info(int ind);
extern int Send_socials_struct_info(int ind);
extern int Send_kind_struct_info(int ind);
extern int Send_ego_struct_info(int ind);
extern int Send_hints_struct_info(int ind);
extern int Send_rinfo_struct_info(int ind);
extern int Send_rbinfo_struct_info(int ind);
extern int Send_death_cause(struct player *p);
extern int Send_winner(struct player *p);
extern int Send_lvl(struct player *p, int lev, int mlev);
extern int Send_weight(struct player *p, int weight, int max_weight);
extern int Send_plusses(struct player *p, int dd, int ds, int mhit, int mdam, int shit,
    int sdam);
extern int Send_ac(struct player *p, int base, int plus);
extern int Send_exp(struct player *p, s32b max, s32b cur, s16b expfact);
extern int Send_gold(struct player *p, s32b gold);
extern int Send_hp(struct player *p, int mhp, int chp);
extern int Send_sp(struct player *p, int msp, int csp);
extern int Send_various(struct player *p, int height, int weight, int age);
extern int Send_stat(struct player *p, int stat, int stat_top, int stat_use, int stat_max,
    int stat_add, int stat_cur);
extern int Send_history(struct player *p, int line, const char *hist);
extern int Send_index(struct player *p, int i, int index, byte type);
extern int Send_item_request(struct player *p, byte tester_hook);
extern int Send_title(struct player *p, const char *title);
extern int Send_turn(struct player *p, u32b game_turn, u32b player_turn, u32b active_turn);
extern int Send_depth(struct player *p, int depth, int maxdepth);
extern int Send_food(struct player *p, int food);
extern int Send_status(struct player *p, s16b *effects);
extern int Send_recall(struct player *p, s16b word_recall, s16b deep_descent);
extern int Send_state(struct player *p, bool searching, bool resting, bool unignoring);
extern int Send_line_info(struct player *p, int y);
extern int Send_remote_line(struct player *p, int y);
extern int Send_speed(struct player *p, int speed);
extern int Send_study(struct player *p, int study, bool can_study_book);
extern int Send_count(struct player *p, byte type, s16b count);
extern int Send_show_floor(struct player *p, byte mode);
extern int Send_char(struct player *p, int x, int y, u16b a, char c, u16b ta, char tc);
extern int Send_spell_info(struct player *p, int book, int i, const char *out_val,
    spell_flags *flags);
extern int Send_floor(struct player *p, byte num, const struct object *obj,
    struct object_xtra *info_xtra);
extern int Send_special_other(struct player *p, char *header, byte peruse, bool protect);
extern int Send_store(struct player *p, char pos, byte attr, s16b wgt, byte number,
    byte owned, s32b price, byte tval, byte max, const char *name);
extern int Send_store_info(struct player *p, int num, char *name, char *owner, int items,
    s32b purse);
extern int Send_target_info(struct player *p, int x, int y, bool dble, const char *buf);
extern int Send_sound(struct player *p, int sound);
extern int Send_mini_map(struct player *p, int y, s16b w);
extern int Send_skills(struct player *p);
extern int Send_pause(struct player *p);
extern int Send_monster_health(struct player *p, int num, byte attr);
extern int Send_aware(struct player *p, u16b num);
extern int Send_everseen(struct player *p, u16b num);
extern int Send_ego_everseen(struct player *p, u16b num);
extern int Send_cursor(struct player *p, char vis, char x, char y);
extern int Send_objflags(struct player *p, int line);
extern int Send_spell_desc(struct player *p, int book, int i, char *out_val);
extern int Send_dtrap(struct player *p, byte dtrap);
extern int Send_term_info(struct player *p, int mode, u16b arg);
extern int Send_player_pos(struct player *p);
extern int Send_play(int ind);
extern int Send_text_screen(int ind, int type, s32b offset);
extern int Send_char_info_conn(int ind);
extern int Send_char_info(struct player *p, byte ridx, byte cidx, byte psex);
extern int Send_birth_options(int ind, struct birth_options *options);
extern bool Send_dump_character(connection_t *connp, const char *dumpname, bool dump_only);
extern int Send_message(struct player *p, const char *msg, u16b typ);
extern int Send_item(struct player *p, const struct object *obj, int wgt, s32b price,
    struct object_xtra *info_xtra);
extern int Send_store_sell(struct player *p, s32b price, bool reset);
extern int Send_party(struct player *p);
extern int Send_special_line(struct player *p, int max, int last, int line, byte attr,
    const char *buf);
extern int Send_fullmap(struct player *p, int y);
extern int Send_poly(struct player *p, int race);
extern int Send_store_leave(struct player *p);
extern int Send_ignore(struct player *p);
extern int Send_flush(struct player *p, bool fresh, bool delay);
extern int Send_channel(struct player *p, byte n, const char *virt);

/*** Commands ***/
extern int cmd_ignore_drop(struct player *p);
extern int cmd_run(struct player *p, int dir);
extern int cmd_rest(struct player *p, s16b resting);
extern int cmd_search(struct player *p);
extern int cmd_tunnel(struct player *p);

/*** General network functions ***/
extern bool process_pending_commands(int ind);
extern int Net_input(void);
extern int Net_output(void);
extern int Net_output_p(struct player *p);
extern bool process_turn_based(void);

/* account.c */
extern u32b get_account(const char *name, const char *pass);

/* control.c */
extern void NewConsole(int fd, int arg);
extern bool InitNewConsole(int write_fd);
extern void console_print(char *msg, int chan);

#endif
