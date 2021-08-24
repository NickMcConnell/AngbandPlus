/*
 * File: mon-timed.h
 * Purpose: Structures and functions for monster timed effects.
 */

#ifndef MONSTER_TIMED_H
#define MONSTER_TIMED_H

/*
 * Monster timed effect constants
 */
#define STUN_MISS_CHANCE    10  /* 1 in __ chance of missing turn when stunned */
#define STUN_HIT_REDUCTION  25  /* Percentage reduction in accuracy for combat */
#define STUN_DAM_REDUCTION  25  /* Percentage reduction in damage */

#define CONF_ERRATIC_CHANCE 30  /* Percentage chance of erratic movement when confused */
#define CONF_HIT_REDUCTION  20  /* Percentage reduction in accuracy for spells */
#define CONF_RANDOM_CHANCE  2   /* 1 in __ chance of an aimed spell going in random direction */

#define DEBUFF_CRITICAL_HIT 10  /* Effective increase in to-hit for critical hit calcs */

/*
 * Flags for the monster timed functions
 */
#define MON_TMD_FLG_NOTIFY      0x01    /* Give notification */
#define MON_TMD_FLG_NOMESSAGE   0x02    /* Never show a message */
#define MON_TMD_FLG_NOFAIL      0x04    /* Never fail the tests. */

extern int mon_timed_name_to_idx(const char *name);
extern bool mon_inc_timed(struct player *p, struct monster *mon, int effect_type, int timer,
    int flag);
extern bool mon_dec_timed(struct player *p, struct monster *mon, int effect_type, int timer,
    int flag);
extern bool mon_clear_timed(struct player *p, struct monster *mon, int effect_type, int flag);

#endif /* MONSTER_TIMED_H */
