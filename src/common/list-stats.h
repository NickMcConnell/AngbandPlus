/*
 * File: list-stats.h
 * Purpose: Player stats
 *
 * index: the stat number
 * power: base power rating for the stat as an object modifier
 * sustain: the corresponding sustain flag
 * sust_p: base power rating for the sustain as an object modifier
 * mod_mult: weight of this stat as an object modifier relative to others
 * adjective: description for increasing stat
 * neg adjective: description for decreasing stat
 * name: stat name
 */

/* index  power  sustain  sust_p  mod_mult  adjective  neg adjective  name */
STAT(STR, 9,  SUST_STR, 9, 13, "strong",   "weak",   "strength")
STAT(INT, 5,  SUST_INT, 4, 10, "smart",    "stupid", "intelligence")
STAT(WIS, 5,  SUST_WIS, 4, 10, "wise",     "naive",  "wisdom")
STAT(DEX, 8,  SUST_DEX, 7, 10, "dextrous", "clumsy", "dexterity")
STAT(CON, 12, SUST_CON, 8, 15, "healthy",  "sickly", "constitution")
