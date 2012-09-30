/* File: zborg6.h */

/* Purpose: Header file for "zborg6.c" */

#ifndef INCLUDED_BORG6_H
#define INCLUDED_BORG6_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "zborg6.c".
 */

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"


/*
 * Determine "twice" the distance between two points
 * This results in "diagonals" being "correctly" ranged,
 * that is, a diagonal appears "furthur" than an adjacent.
 */
#define double_distance(Y1,X1,Y2,X2) \
    (distance(((int)(Y1))<<1,((int)(X1))<<1,((int)(Y2))<<1,((int)(X2))<<1))


/*
 * Attempt to induce "word of recall"
 */
extern bool borg_recall(void);

/*
 * Low level goals
 */
extern bool borg_caution(void);
extern bool borg_attack(bool boosted_bravery);
extern bool borg_flow_non_hurt(void);
extern bool borg_recover(void);

extern bool borg_offset_ball(void);
extern bool borg_defend(int p);
extern bool borg_perma_spell(void);

extern bool borg_eat_cure_poison(void);
extern bool borg_check_rest(void);
extern bool borg_on_safe_feat(byte feat);

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
extern int borg_extract_dir(int x1, int y1, int x2, int y2);
extern bool borg_flow_stair_both(int why);
extern bool borg_flow_stair_less(int why);
extern bool borg_flow_stair_more(int why);

extern bool borg_flow_glyph(int why);
extern bool borg_flow_light(int why);
extern bool borg_check_lite_only(void);

/*
 * Flow to shops
 */
extern bool borg_flow_shop_entry(int n);
extern void borg_flow_direct(int y, int x);

/*
 * Flow towards monsters/objects
 */
extern bool borg_flow_kill(bool viewable, int nearness);
extern bool borg_flow_kill_aim(bool viewable);
extern bool borg_flow_kill_corridor(bool viewable);
extern bool borg_flow_take(bool viewable, int nearness);

/* Flow in the wilderness */
extern bool borg_choose_shop(void);
extern bool borg_find_shop(void);
extern bool borg_find_town(void);
extern bool borg_find_dungeon(void);
extern bool borg_find_home(void);
extern bool borg_waits_daylight(void);
extern bool borg_flow_dark_wild(void);
extern void borg_flow_goal_wild(void);
extern void borg_leave_surface(void);

/* Flow towards unexplored grids */
extern bool borg_flow_dark(bool neer);

/*
 * Search for secret doors
 */
extern bool borg_flow_spastic(bool bored);

extern bool borg_lite_beam(bool simulation, int *dir);
extern bool borg_caution_phase(int emergency, int turns);

/*
 * Initialize this file
 */
extern void borg_init_6(void);




#endif

#endif
