/* File: borg6.h */

/* Purpose: Header file for "borg6.c" -BEN- */

#ifndef INCLUDED_BORG6_H
#define INCLUDED_BORG6_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg6.c".
 */

#include "dborg1.h"
#include "dborg2.h"
#include "dborg3.h"

/*
 * Possible values of "goal"
 */
#define GOAL_KILL   1       /* Monsters */
#define GOAL_TAKE   2       /* Objects */
#define GOAL_MISC   3       /* Stores */
#define GOAL_DARK   4       /* Exploring */
#define GOAL_XTRA   5       /* Searching */
#define GOAL_BORE   6       /* Leaving */
#define GOAL_FLEE   7       /* Fleeing */


/*
 * Minimum "harmless" food
 */

#define SV_FOOD_MIN_OKAY    SV_FOOD_CURE_POISON



/*
 * Attempt to induce "word of recall"
 */
extern bool borg_recall(void);

/*
 * Low level goals
 */
extern bool borg_caution(void);
extern bool borg_attack(void);
extern bool borg_recover(void);

extern bool borg_offset_ball(void);
extern bool borg_defend(int p);
extern bool borg_perma_spell(void);

extern bool borg_check_rest(void);

/*
 * Twitchy goals
 */
extern bool borg_charge_kill(void);
extern bool borg_charge_take(void);
extern bool borg_twitchy(void);



/*
 * Continue a high level goal
 */
extern bool borg_flow_old(int why);

/*
 * Flow to stairs
 */
extern bool borg_flow_stair_both(int why);
extern bool borg_flow_stair_less(int why);
extern bool borg_flow_stair_more(int why);


extern bool borg_flow_glyph(int why);
extern bool borg_flow_light(int why);
extern bool borg_check_lite_only(void);
extern bool borg_backup_swap(int p);

/*
 * Flow to shops
 */
extern bool borg_flow_shop_visit(void);
extern bool borg_flow_shop_entry(int n);

/*
 * Flow towards monsters/objects
 */
extern bool borg_flow_kill(bool viewable, int nearness);
extern bool borg_flow_kill_aim(bool viewable);
extern bool borg_flow_kill_corridor(bool viewable);
extern bool borg_flow_take(bool viewable, int nearness);
extern void borg_flow_direct_dig(int m_y, int m_x);

/*
 * Flow towards "interesting" grids
 */
extern bool borg_flow_dark(bool neer);

/*
 * Search for secret doors
 */
extern bool borg_flow_spastic(bool bored);

extern bool borg_target(int y, int x);
extern int borg_launch_damage_one(int i, int dam, int typ);
extern int borg_attack_aux_thrust(void);


extern void borg_log_battle(bool);
extern bool borg_target_unknown_wall(int g_y,int g_x);

/*
 * Initialize this file
 */
extern void borg_init_6(void);




#endif

#endif

