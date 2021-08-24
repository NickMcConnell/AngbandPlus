/*
 * File: mon-summon.h
 * Purpose: Monster summoning
 */

#ifndef MONSTER_SUMMON_H
#define MONSTER_SUMMON_H

/* Flags for "summon_specific()" */
enum summon_flag
{
    #define S(a, b, c, d, e, f, g, h) S_##a,
    #include "list-summon-types.h"
    #undef S
    S_MAX
};

/** Variables **/
extern struct monster_base *kin_base;

/** Functions **/
extern int summon_name_to_idx(const char *name);
extern const char *summon_desc(int type);
extern int summon_message_type(int summon_type);
extern int summon_specific(struct player *p, struct chunk *c, int y1, int x1, int lev, int type,
    bool delay, bool call, int chance);
extern bool summon_specific_race_aux(struct player *p, struct chunk *c, int y1, int x1,
    struct monster_race *race, unsigned char size, bool pet);
extern bool summon_specific_race(struct player *p, struct chunk *c, int y1, int x1,
    struct monster_race *race, unsigned char size);
extern bool summon_specific_race_somewhere(struct player *p, struct chunk *c,
    struct monster_race *race, unsigned char size);
extern int summon_monster_aux(struct player *p, struct chunk *c, int y, int x, int flag, int rlev,
    int max, int chance);
extern bool summon_location(struct chunk *c, int *yp, int *xp, int y1, int x1, int tries);

#endif /* MONSTER_SUMMON_H */
