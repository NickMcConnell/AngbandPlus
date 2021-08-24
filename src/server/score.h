/*
 * File: score.h
 * Purpose: Highscore handling
 */

#ifndef INCLUDED_SCORE_H
#define INCLUDED_SCORE_H

/*
 * Maximum number of high scores in the high score file
 */
#define MAX_HISCORES    100

/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */
typedef struct
{
    char what[8];       /* Version info (string) */
    char pts[10];       /* Total Score (number) */
    char gold[10];      /* Total Gold (number) */
    char turns[20];     /* Turns Taken (number) */
    char day[10];       /* Time stamp (string) */
    char who[16];       /* Player Name (string) */
    char uid[8];        /* Player UID (number) */
    char sex[2];        /* Player Sex (string) */
    char p_r[3];        /* Player Race (number) */
    char p_c[3];        /* Player Class (number) */
    char cur_lev[4];    /* Current Player Level (number) */
    char cur_dun[4];    /* Current Dungeon Level (number) */
    char max_lev[4];    /* Max Player Level (number) */
    char max_dun[4];    /* Max Dungeon Level (number) */
    char how[32];       /* Method of death (string) */
} high_score;

extern size_t highscore_read(high_score scores[], size_t sz);
extern size_t highscore_where(const high_score *entry, const high_score scores[], size_t sz);
extern size_t highscore_add(const high_score *entry, high_score scores[], size_t sz);
extern void build_score(struct player *p, high_score *entry, const char *died_from,
    time_t *death_time);
extern void enter_score(struct player *p, time_t *death_time);
extern long total_points(struct player *p, s32b max_exp, s16b max_depth);

#endif /* INCLUDED_SCORE_H */
