/*
 * File: player-timed.h
 * Purpose: Timed effects handling
 */

#ifndef PLAYER_TIMED_H
#define PLAYER_TIMED_H

/*
 * Player cut timer values
 */
#define TMD_CUT_NONE    0
#define TMD_CUT_GRAZE   10
#define TMD_CUT_LIGHT   25
#define TMD_CUT_BAD     50
#define TMD_CUT_NASTY   100
#define TMD_CUT_SEVERE  200
#define TMD_CUT_DEEP    1000

/*
 * Effect failure flag types
 */
enum
{
    TMD_FAIL_FLAG_OBJECT = 1,
    TMD_FAIL_FLAG_RESIST,
    TMD_FAIL_FLAG_VULN
};

extern int timed_name_to_idx(const char *name);
extern const char *timed_idx_to_name(int type);
extern const char *timed_idx_to_desc(int type);
extern int timed_protect_flag(int type);
extern bool player_set_timed(struct player *p, int idx, int v, bool notify);
extern bool player_inc_timed_aux(struct player *p, struct monster *mon, int idx, int v,
    bool notify, bool check);
extern bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check);
extern bool player_dec_timed(struct player *p, int idx, int v, bool notify);
extern bool player_clear_timed(struct player *p, int idx, bool notify);
extern bool player_set_food(struct player *p, int v);

#endif /* PLAYER_TIMED_H */
