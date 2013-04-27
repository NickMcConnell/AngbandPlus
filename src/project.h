/* File: project.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "types2.h"

/*
 * Bit flags for the "project()" function
 *
 *   JUMP: Jump directly to the target location (this is a hack)
 *   BEAM: Work as a beam weapon (affect every grid passed through)
 *   THRU: Continue "through" the target (used for "bolts"/"beams")
 *   STOP: Stop as soon as we hit a monster (used for "bolts")
 *   GRID: Affect each grid in the "blast area" in some way
 *   ITEM: Affect each object in the "blast area" in some way
 *   KILL: Affect each monster in the "blast area" in some way
 *   HIDE: Hack -- disable "visual" feedback from projection
 */
#define PROJECT_JUMP	0x01
#define PROJECT_BEAM	0x02
#define PROJECT_THRU	0x04
#define PROJECT_STOP	0x08
#define PROJECT_GRID	0x10
#define PROJECT_ITEM	0x20
#define PROJECT_KILL	0x40
#define PROJECT_HIDE	0x80

/*
 * Spell types used by project(), and related functions.
 */
#define GF_XXX1			1
#define GF_ARROW        2
#define GF_MISSILE      3
#define GF_MANA         4
#define GF_HOLY_ORB     5
#define GF_LITE_WEAK	6
#define GF_DARK_WEAK	7
#define GF_WATER        8
#define GF_PLASMA       9
#define GF_METEOR       10
#define GF_ICE          11
#define GF_GRAVITY      12
#define GF_INERTIA      13
#define GF_FORCE        14
#define GF_TIME         15
#define GF_ACID         16
#define GF_ELEC         17
#define GF_FIRE         18
#define GF_COLD         19
#define GF_POIS         20
#define GF_XXX2			21
#define GF_LITE         22
#define GF_DARK         23
#define GF_XXX3			24
#define GF_CONFUSION    25
#define GF_SOUND        26
#define GF_SHARD        27
#define GF_NEXUS        28
#define GF_NETHER       29
#define GF_CHAOS        30
#define GF_DISENCHANT   31
#define GF_XXX4			32
#define GF_KILL_WALL	33
#define GF_KILL_DOOR	34
#define GF_KILL_TRAP	35
#define GF_MAKE_WALL	36
#define GF_MAKE_DOOR	37
#define GF_MAKE_TRAP	38
#define GF_XXX5			39
#define GF_XXX6			40
#define GF_AWAY_UNDEAD	41
#define GF_AWAY_EVIL	42
#define GF_AWAY_ALL		43
#define GF_TURN_UNDEAD	44
#define GF_TURN_EVIL	45
#define GF_TURN_ALL		46
#define GF_DISP_UNDEAD	47
#define GF_DISP_EVIL	48
#define GF_DISP_ALL		49
#define GF_XXX7			50
#define GF_OLD_CLONE	51
#define GF_OLD_POLY		52
#define GF_OLD_HEAL		53
#define GF_OLD_SPEED	54
#define GF_OLD_SLOW		55
#define GF_OLD_CONF		56
#define GF_OLD_SLEEP	57
#define GF_OLD_DRAIN	58
#define GF_XXX8			59

/* spells1.c */
extern bool project(int who, int rad, coord g, int dam, int typ, int flg);

