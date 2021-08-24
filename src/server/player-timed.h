/*
 * File: player-timed.h
 * Purpose: Timed effects handling
 */

#ifndef PLAYER_TIMED_H
#define PLAYER_TIMED_H

/*
 * Effect failure flag types
 */
enum
{
    TMD_FAIL_FLAG_OBJECT = 1,
    TMD_FAIL_FLAG_RESIST,
    TMD_FAIL_FLAG_VULN
};

/*
 * Data struct
 */
struct timed_effect_data
{
    const char *name;
    u32b flag_redraw;
    u32b flag_update;

    int index;
    char *desc;
    struct timed_grade *grade;
    char *on_end;
    char *on_increase;
    char *on_decrease;
    char *near_begin;
    char *near_end;
    int msgt;
    int fail_code;
    int fail;
};

/*
 * Player food values
 */
extern int PY_FOOD_MAX;
extern int PY_FOOD_FULL;
extern int PY_FOOD_HUNGRY;
extern int PY_FOOD_WEAK;
extern int PY_FOOD_FAINT;
extern int PY_FOOD_STARVE;

extern struct file_parser player_timed_parser;
extern struct timed_effect_data timed_effects[];

extern int timed_name_to_idx(const char *name);
extern bool player_set_timed(struct player *p, int idx, int v, bool notify);
extern bool player_inc_check(struct player *p, struct monster *mon, int idx, bool lore);
extern bool player_inc_timed_aux(struct player *p, struct monster *mon, int idx, int v,
    bool notify, bool check);
extern bool player_inc_timed(struct player *p, int idx, int v, bool notify, bool check);
extern bool player_dec_timed(struct player *p, int idx, int v, bool notify);
extern bool player_clear_timed(struct player *p, int idx, bool notify);
extern bool player_timed_grade_eq(struct player *p, int idx, char *match);

#endif /* PLAYER_TIMED_H */
