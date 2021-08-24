/*
 * File: list-object-modifiers.h
 * Purpose: Object modifiers (plusses and minuses) for all objects
 *
 * index: the mod number
 * power: base power rating for the mod (0 means it is unused or derived)
 * mod_mult: weight of this modifier relative to others
 * name: mod name
 */

/* index  power  mod_mult  name */
OBJ_MOD(MANA,      5,  10, "mana capacity")
OBJ_MOD(STEALTH,   8,  12, "stealth")
OBJ_MOD(SEARCH,    2,  5,  "searching skill")
OBJ_MOD(INFRA,     4,  8,  "infravision")
OBJ_MOD(TUNNEL,    3,  8,  "tunneling")
OBJ_MOD(SPEED,     20, 6,  "speed")
OBJ_MOD(BLOWS,     0,  50, "attack speed")
OBJ_MOD(SHOTS,     0,  50, "shooting speed")
OBJ_MOD(MIGHT,     0,  30, "shooting power")
OBJ_MOD(LIGHT,     3,  6,  "")
OBJ_MOD(POLY_RACE, 0,  0,  "")
