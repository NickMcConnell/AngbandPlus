/*
 * File: s-externs.h
 * Purpose: Extern declarations (variables and functions)
 */

#ifndef INCLUDED_S_EXTERNS_H
#define INCLUDED_S_EXTERNS_H

/* account.c */
extern u32b get_account(const char *name, const char *pass);

/* birth.c */
extern void clear_cave_info(struct player *p, bool full);
extern struct player *player_birth(int id, u32b account, const char *name, const char *pass,
    int conn, byte ridx, byte cidx, byte psex, s16b* stat_roll);
extern void server_birth(void);
extern u16b connection_type_ok(u16b conntype);
extern void player_free(struct player *p);

/* cmd1.c */
extern bool search(int Ind, bool verbose);
extern byte do_autopickup(int Ind, int pickup);
extern byte py_pickup(int Ind, int pickup);
extern void move_player(int Ind, int dir, bool disarm, bool check_pickup, bool force);
extern bool quest_done(struct player *p, int depth);
extern void leave_depth(struct player *p);
extern void use_energy(int Ind);
extern bool has_energy(int Ind);
extern bool weight_okay(struct player *p, object_type *o_ptr);

/* cmd2.c */
extern bool create_house(struct player *p);
extern int pick_house(int depth, int y, int x);
extern int wielding_cut(struct player *p);

/* cmd4.c */
extern void do_cmd_check_players(int Ind, int line);
extern void do_cmd_check_other(int Ind, int line);
extern void do_cmd_check_poly(int Ind, int line);
extern void do_cmd_check_socials(int Ind, int line);
extern void do_cmd_interactive(int Ind, int type, u32b query);

/* cmd-obj.c */
extern void wield_item(int Ind, object_type *o_ptr, int item, int slot, bool take_turn);

/* control.c */
extern void NewConsole(int fd, int arg);
extern bool InitNewConsole(int write_fd);
extern void console_print(char *msg, int chan);

/* dungeon.c */
extern void dungeon_change_level(struct player *p, int new_depth, byte new_level_method);
extern void play_game(void);
extern void shutdown_server(void);
extern void dungeon(void);
extern bool check_special_level(s16b special_depth);
extern bool forbid_special(s16b special_depth);
extern bool random_level(s16b depth);
extern int find_player(s32b id);
extern bool recall_player(int Ind);

/* melee.c */
extern void process_monsters(struct cave *c);
extern bool resist_undead_attacks(player_type *p_ptr, monster_race *r_ptr);
extern int get_power(int effect);
extern int get_cut(int d_dice, int d_side, int d_dam);
extern int get_stun(int d_dice, int d_side, int d_dam);
extern int get_thrown_spell(int Ind, int who, int depth, int m_idx, int target_m_dis,
    int py, int px);
extern void compute_moves(int x, int y, int mm[5]);
extern bool is_staggering(int Ind, int m_idx);
extern bool monster_stunned(int Ind, int m_idx);
extern void monster_effects(int Ind, int m_idx);
extern void process_move(int Ind, int m_idx, int oy, int ox, int mm[5],
    bool stagger, int target_m_idx, bool did_what[3]);
extern void process_move_end(int Ind, int m_idx, bool did_what[3]);
extern bool player_invis(int Ind, monster_type *m_ptr);
extern bool is_closest(int Ind, monster_type *m_ptr, bool blos, bool new_los, int j,
    int dis_to_closest, int lowhp);

/* mvm.c */
extern bool unlight_area_MvM(int target_m_idx, int depth, int m_idx);
extern bool trap_creation_MvM(int target_m_idx, int depth, int m_idx);
extern void check_monster_MvM(int Ind, int m_idx);
extern int check_hit_MvM(int ac, int power, int level);
extern bool make_attack_normal_MvM(int Ind, int target_m_idx, int m_idx);
extern bool take_hit_MvM(int who, struct monster *m_ptr, int dam, byte note);

/* party.c */
extern struct player *player_lookup(const char *name);
extern bool player_in_party(int party_id, struct player *p);
extern bool in_party(struct player *p, int party_id);
extern bool is_party_owner(struct player *p, struct player *q);
extern bool master_in_party(s16b p1_id, s16b p2_id);
extern bool party_create(int Ind, const char *name);
extern bool party_add(int adder, const char *name);
extern bool party_remove(int remover, const char *name);
extern void party_leave(int Ind);
extern void party_msg_format(int party_id, const char *fmt, ...);
extern bool party_share_with(int Ind, int party_id, int Ind2);
extern void party_exp_gain(int Ind, int party_id, s32b amount);
extern bool pvp_check(struct player *attacker, struct player *target, int mode, bool silent,
    byte feat);
extern bool pvm_check(int attacker, int target);
extern void party_msg_near(struct player *p, const char *msg);
extern void add_player_name(int id, u32b account, const char *name, hturn *death_turn);
extern void remove_player_name(const char *name);
extern void delete_player_name(const char *name);
extern u32b player_id_count(u32b account);
extern u32b player_id_list(int **list, u32b account);
extern void purge_player_names(void);
extern void wipe_player_names(void);
extern int player_expiry(hturn *death_turn);

/* pathfind.c */
extern void run_step(int Ind);

/* randart.c */
extern void do_randart_name(s32b randart_seed, char *buffer, int len);
extern int get_new_esp(bitflag flags[OF_SIZE]);
extern artifact_type* do_randart(s32b randart_seed, struct artifact *artifact);
extern void init_randart_generator(void);
extern void free_randart_generator(void);
extern artifact_type* get_artifact(const object_type *o_ptr);

/* s-variable.c */
extern int NumPlayers;
extern bool server_generated;
extern bool server_state_loaded;
extern u32b seed_flavor;
extern u32b seed_town;
extern hturn turn;
extern s16b o_max;
extern s16b o_top;
extern s16b o_nxt;
extern struct store *stores;
extern s16b alloc_ego_size;
extern alloc_entry *alloc_ego_table;
extern s16b alloc_race_size;
extern alloc_entry *alloc_race_table;
extern byte gf_to_attr[GF_MAX][BOLT_MAX];
extern char gf_to_char[GF_MAX][BOLT_MAX];
extern byte tval_to_attr[128];
extern s32b player_id;
extern artifact_type *a_info;
extern feature_type *f_info;
extern object_base *kb_info;
extern ego_item_type *e_info;
extern monster_base *rb_info;
extern monster_pain *pain_messages;
extern struct vault *vaults;
extern struct flavor *flavors;
extern spell_type *s_info;
extern struct pit_profile *pit_info;
extern char *ANGBAND_DIR_APEX;
extern char *ANGBAND_DIR_HELP;
extern char *ANGBAND_DIR_SAVE;
extern bool (*get_mon_num_hook)(int r_idx);
extern byte trees_in_town;
extern byte level_up_y[MAX_DEPTH];
extern byte level_up_x[MAX_DEPTH];
extern byte level_down_y[MAX_DEPTH];
extern byte level_down_x[MAX_DEPTH];
extern byte level_rand_y[MAX_DEPTH];
extern byte level_rand_x[MAX_DEPTH];
extern s16b *players_on_depth;
extern s16b special_levels[MAX_SPECIAL_LEVELS];
extern cave_view_type player_presets[MAX_PRESETS][MAX_CLASS][MAX_RACES][MAX_SEXES];
extern cave_view_type player_numbers[MAX_PRESETS][7];
extern bool cfg_report_to_meta;
extern char *cfg_meta_address;
extern char *cfg_bind_name;
extern char *cfg_report_address;
extern char *cfg_console_password;
extern char *cfg_dungeon_master;
extern bool cfg_secret_dungeon_master;
extern bool cfg_no_steal;
extern bool cfg_newbies_cannot_drop;
extern s32b cfg_level_unstatic_chance;
extern s32b cfg_retire_timer;
extern bool cfg_random_artifacts;
extern s16b cfg_limit_stairs;
extern bool cfg_no_recall;
extern bool cfg_no_ghost;
extern bool cfg_more_towns;
extern bool cfg_artifact_drop_shallow;
extern bool cfg_limit_player_connections;
extern s16b cfg_max_townies;
extern s16b cfg_max_trees;
extern s32b cfg_tcp_port;
extern bool cfg_chardump_color;
extern bool cfg_town_wall;
extern s16b cfg_pvp_hostility;
extern bool cfg_base_monsters;
extern bool cfg_extra_monsters;
extern s16b cfg_max_houses;
extern bool cfg_ghost_diving;
extern bool cfg_console_local_only;
extern char *cfg_load_pref_file;
extern s16b cfg_preserve_artifacts;
extern bool cfg_safe_recharge;
extern s16b cfg_party_sharelevel;
extern bool cfg_small_range;
extern bool cfg_ai_packs;
extern bool cfg_ai_smart;
extern bool cfg_ai_smell;
extern bool cfg_ai_learn;
extern bool cfg_ai_cheat;
extern party_type parties[MAX_PARTIES];
extern channel_type channels[MAX_CHANNELS];
extern int chan_audit;
extern int chan_debug;
extern int chan_cheat;
extern arena_type arenas[MAX_ARENAS];
extern u16b num_arenas;
extern house_type houses[MAX_HOUSES];
extern u16b num_houses;
extern s16b *m_fast;
extern wilderness_type *wild_info;
extern char (*f_char_s)[FEAT_LIGHTING_MAX];
extern byte (*f_attr_s)[FEAT_LIGHTING_MAX];
extern char *r_char_s;
extern byte *r_attr_s;
extern void (*master_move_hook)(int Ind, char *args);

/* savefile.c */
extern bool save_player(int Ind);
extern void save_dungeon_special(int depth);
extern bool save_server_info(void);
extern bool load_player(struct player *p);
extern int scoop_player(char *nick, char *pass, byte *pridx, byte *pcidx, byte *psex);
extern bool load_server_info(void);

/* sched.c */
extern void install_timer_tick(void (*func)(void), int freq);
extern void install_input(void (*func)(int, int), int fd, int arg);
extern void remove_input(int fd);
extern void sched(void);
extern void free_input(void);
extern void remove_timer_tick(void);

/* score.c */
extern long total_points(struct player *p, s32b max_exp, s16b max_depth);
extern void enter_score(int Ind, time_t *death_time);
extern void do_cmd_knowledge_scores(int Ind, int line);

/* tables.c */
extern s16b ddd[9];
extern s16b ddx[10];
extern s16b ddy[10];
extern s16b ddx_ddd[9];
extern s16b ddy_ddd[9];
extern const int adj_mag_stat[];
extern u16b level_speeds[128];
extern magic_type ghost_spells[GHOST_SPELLS];
extern const char *ghost_spell_names[GHOST_SPELLS];
extern byte ghost_spell_dirs[GHOST_SPELLS];
extern byte ghost_spell_projs[GHOST_SPELLS];
extern const char *ghost_spell_descs[GHOST_SPELLS];
extern magic_type mimic_spells[MIMIC_SPELLS];
extern const char *mimic_spell_names[MIMIC_SPELLS];
extern byte mimic_spell_dirs[MIMIC_SPELLS];
extern byte mimic_spell_projs[MIMIC_SPELLS];
extern const char *mimic_spell_descs[MIMIC_SPELLS];
extern martial_arts ma_blows[MAX_MA];
extern const char *store_names[MAX_STORES];
extern const char *inscrip_text[];

/* util.c */
extern void sound(struct player *p, int num);
extern bool inscription_prevent(quark_t quark, char what, bool is_harmless);
extern void msg_broadcast(struct player *p, const char *msg);
extern void msg(struct player *p, const char *fmt, ...);
extern void msg_print_complex_near(struct player *p, struct player *q, u16b type, const char *msg);
extern void msg_format_complex_near(struct player *p, u16b type, const char *fmt, ...);
extern void msg_print_near(struct player *p, u16b type, const char *msg);
extern void msg_format_near(struct player *p, u16b type, const char *fmt, ...);
extern void text_out_init(struct player *p);
extern void text_out(struct player *p, const char *fmt, ...);
extern void text_out_c(struct player *p, byte a, const char *fmt, ...);
extern void text_out_done(struct player *p);
extern void msg_print_aux(struct player *p, const char *msg, u16b type);
extern void channel_join(int Ind, const char *channel, bool quiet);
extern void channel_leave(int Ind, const char *channel);
extern void channels_leave(int Ind);
extern void msgt(struct player *p, unsigned int type, const char *fmt, ...);
extern void message_flush(struct player *p);
extern void msg_channel(int chan, const char *msg);
extern void fill_prevent_inscription(bool *arr, quark_t quark);
extern void update_prevent_inscriptions(struct player *p);
extern void alloc_info_icky(struct player *p);
extern s16b get_last_info_line(int Ind);
extern cave_view_type* get_info(int Ind, int y, int x);
extern void free_info_icky(int Ind);
extern void alloc_header_icky(struct player *p, const char *header);
extern const char *get_header(struct player *p, const char *header);
extern void free_header_icky(int Ind);
extern void set_ghost_flag(struct player *p, s16b flag, bool report);
extern void notify_player(int Ind, char *header, u16b term, bool symbol);
extern void notify_player_popup(struct player *p, char *header, u16b term, u16b pop);
extern char color_attr_to_char(int a);
extern int stricmp(const char *a, const char *b);
extern const char *player_poss(struct player *p);
extern const char *player_self(struct player *p);

/* xtra2.c */
extern void set_recall(struct player *p, quark_t note, bool fast);
extern void player_death(int Ind);
extern void resurrect_player(int Ind);
extern bool modify_panel(struct player *p, int wy, int wx);
extern bool adjust_panel(int Ind, int y, int x);
extern bool change_panel(struct player *p, int dir);
extern void verify_panel(struct player *p);
extern void center_panel(int Ind);
extern bool do_scroll_life(struct player *p);
extern int level_speed(int Ind);
extern void reset_house(int house, int depth, int y, int x);
extern bool house_inside(struct player *p, int house);
extern bool house_owned_by(struct player *p, int house);
extern int time_factor(int Ind);
extern int base_time_factor(struct player *p, int slowest);
extern int pick_arena(int depth, int y, int x);
extern void access_arena(int Ind, int y, int x);
extern int motion_dir(int y1, int x1, int y2, int x2);
extern void describe_player(struct player *p, int who);
extern void player_dump(int Ind);

/* xtra3.c */
extern void player_flags(struct player *p, bitflag f[OF_SIZE], object_type *inven);
extern void redraw_stuff(struct player *p);

#endif /* INCLUDED_S_EXTERNS_H */
