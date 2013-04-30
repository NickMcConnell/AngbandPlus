/*
 * File: player.h
 * Purpose: Player interface
 */

#ifndef PLAYER_PLAYER_H
#define PLAYER_PLAYER_H

/* calcs.c */
extern const byte adj_chr_gold[STAT_RANGE];
extern const byte adj_str_hold[STAT_RANGE];
extern const byte adj_str_blow[STAT_RANGE];
extern const byte adj_dex_safe[STAT_RANGE];
extern const byte adj_con_fix[STAT_RANGE];
extern int weight_limit(player_state *state);
extern int weight_remaining(struct player *p);
extern void calc_bonuses(struct player *p, object_type inventory[], player_state *state,
    bool id_only);
extern void notice_stuff(struct player *p);
extern void update_stuff(struct player *p);
extern void handle_stuff(struct player *p);
extern void refresh_stuff(int Ind);
extern bool monk_armor_ok(struct player *p, object_type *inven);

/* player.c */
extern bool player_stat_inc(struct player *p, int stat);
extern bool player_stat_dec(struct player *p, int stat, bool permanent);
extern void player_exp_gain(struct player *p, s32b amount);
extern void player_exp_lose(struct player *p, s32b amount, bool permanent);
extern void init_players(void);
extern void free_players(void);
extern struct player *player_get(int Ind);
extern void player_set(int Ind, struct player *p);
extern void player_death_info(struct player *p, const char *died_from);

/* p-util.c */
extern s16b modify_stat_value(int value, int amount);
extern bool player_confuse_dir(struct player *p, int *dp);
extern bool player_can_cast(struct player *p);
extern bool player_can_cast_msg(struct player *p);
extern bool player_can_study_book(struct player *p);

/* spell.c */
extern void show_ghost_spells(struct player *p);
extern bool check_antimagic(struct player *p, struct monster *m);
extern bool check_antisummon(struct player *p, struct monster *m);
extern void show_mimic_spells(struct player *p);
extern void cast_spell_end(int Ind);

/* timed.c */
extern bool player_set_timed(struct player *p, int idx, int v, bool notify);
extern bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check);
extern bool player_dec_timed(struct player *p, int idx, int v, bool notify);
extern bool player_clear_timed(struct player *p, int idx, bool notify);
extern bool player_inc_timed_nostack(struct player *p, int idx, int v, int iv, bool notify);
extern bool player_set_food(struct player *p, int v);

#endif /* PLAYER_PLAYER_H */
