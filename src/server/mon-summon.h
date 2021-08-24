/*
 * File: mon-summon.h
 * Purpose: Monster summoning
 */

#ifndef MONSTER_SUMMON_H
#define MONSTER_SUMMON_H

/*
 * Monster base for a summon
 */
struct monster_base_list
{
     struct monster_base_list *next;
     struct monster_base *base;
};

struct summon
{
    struct summon *next;
    char *name;
    int message_type;
    bool unique_allowed;
    struct monster_base_list *bases;
    int race_flag;
    char *fallback_name;
    int fallback;
    char *desc;
};

/** Variables **/
extern struct monster_base *kin_base;

extern struct file_parser summon_parser;

/** Functions **/
extern int summon_name_to_idx(const char *name);
extern int summon_message_type(int summon_type);
extern int summon_fallback_type(int summon_type);
extern const char *summon_desc(int type);
extern int summon_specific(struct player *p, struct chunk *c, struct loc *grid, int lev, int type,
    bool delay, bool call, int chance, struct monster *summoner);
extern bool summon_specific_race_aux(struct player *p, struct chunk *c, struct loc *grid,
    struct monster_race *race, unsigned char size, bool pet);
extern bool summon_specific_race(struct player *p, struct chunk *c, struct loc *grid,
    struct monster_race *race, unsigned char size);
extern bool summon_specific_race_somewhere(struct player *p, struct chunk *c,
    struct monster_race *race, unsigned char size);
extern int summon_monster_aux(struct player *p, struct chunk *c, struct loc *grid, int flag,
    int rlev, int max, int chance, struct monster *mon);
extern bool summon_location(struct chunk *c, struct loc *place, struct loc *grid, int tries);
extern struct monster_race *select_shape(struct player *p, struct monster *mon, int type);

#endif /* MONSTER_SUMMON_H */
