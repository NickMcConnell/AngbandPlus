/*
 * File: monster.h
 * Purpose: Structures and functions for monsters
 */

#ifndef MONSTER_MONSTER_H
#define MONSTER_MONSTER_H

#include "../cave.h"

/** Functions **/

/* melee.c */
extern bool check_hit(struct player *p, int power, int level);

#endif /* MONSTER_MONSTER_H */
