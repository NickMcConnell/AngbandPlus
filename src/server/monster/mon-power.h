/*
 * File: mon-power.h
 * Purpose: Structures and functions for monster power
 */

#ifndef MONSTER_POWER_H
#define MONSTER_POWER_H

/** Variables **/
extern s32b tot_mon_power;
extern bool arg_rebalance;

/** Functions **/
extern errr eval_r_power(struct monster_race *races);

#endif /* MONSTER_POWER_H */
