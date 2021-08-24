/*
 * File: mon-power.h
 * Purpose: Structures and functions for monster power
 */

#ifndef MONSTER_POWER_H
#define MONSTER_POWER_H

extern bool arg_power;
extern bool arg_rebalance;

/** Functions **/
extern errr eval_monster_power(struct monster_race *racelist);

#endif /* MONSTER_POWER_H */
