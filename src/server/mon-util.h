/*
 * File: mon-util.h
 * Purpose: Functions for monster utilities.
 */

#ifndef MONSTER_UTILITIES_H
#define MONSTER_UTILITIES_H

/** Constants **/

/*
 * Maximum number of picked up/stolen objects a monster can carry
 */
#define MAX_MONSTER_BAG 25

/* Monster status (hostile by default) */
#define MSTATUS_HOSTILE     0   /* hostile */
#define MSTATUS_SUMMONED    1   /* hostile, summoned by the player */
#define MSTATUS_GUARD       2   /* guard, controlled by the player */
#define MSTATUS_FOLLOW      3   /* follower, controlled by the player */
#define MSTATUS_ATTACK      4   /* attacker, controlled by the player */

/** Macros **/

/** Structures **/

/** Variables **/

/** Functions **/
extern bool match_monster_bases(const struct monster_base *base, ...);
extern bool monster_is_nonliving(struct monster_race *race);
extern bool monster_is_unusual(struct monster_race *race);
extern void player_desc(struct player *p, char *desc, size_t max, struct player *q,
    bool capitalize);
extern void update_mon(struct monster *mon, struct chunk *c, bool full);
extern void update_monsters(struct chunk *c, bool full);
extern bool monster_carry(struct monster *mon, struct object *obj, bool force);
extern void monster_swap(struct chunk *c, int y1, int x1, int y2, int x2);
extern void aware_player(struct player *p, struct player *q);
extern void become_aware(struct player *p, struct chunk *c, struct monster *mon);
extern bool is_mimicking(struct monster *mon);
extern void update_smart_learn(struct monster *mon, struct player *p, int flag, int pflag,
    int element);
extern void update_player(struct player *q);
extern void update_players(void);
extern bool is_humanoid(const struct monster_race *race);
extern bool is_half_humanoid(const struct monster_race *race);
extern void update_monlist(struct monster *mon);
extern bool resist_undead_attacks(struct player *p, struct monster_race *race);

#endif /* MONSTER_UTILITIES_H */
