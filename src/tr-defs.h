/**
 * \file tr-defs.h
 * \brief #defines for tuning tr patch
 *
 * Copyright (c) 2018 James Banks
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


/* multiplies by player->depth in ambient_xp_of() in game-world.c */
#define TR_DEPTH_XP_MULT 2

/* average wait time for ambient XP gains.  100 is a guess */
#define TR_AVG_XP_WAIT 100

/* When you move, you have a 1 in (TR_AVG_XP_WAIT * TR_MOVE_XP_MULT)
 * chance of gaining ambient XP.  See cmd-cave.c / do_cmd_walk()
 */
#define TR_MOVE_XP_MULT 10

/* Delay to slow down ap_sorrow processing */
#define TR_AP_SORROW_DELAY 10


/* chance of incrementing hidden sorrow in a given turn equals natural 
 *  log of apparent sorrow divided by TR_AP_BREAKING_PT 
 * ap_sorrow gives 100% chance of hid inc around 10,000 ap_sorrow at
 *  TR_AP_BREAKING_PT == 10
 * use found in process_world() in game-world.c
 */
#define TR_AP_BREAKING_PT 10

/* maximum apparent sorrow
 * (2^31)/2 == 1073741824
 * this limit is based on player->ap_sorrow being s32b
 * more headroom than is necessary
 */
#define TR_MAX_AP_SORROW 1073741824

/* maximum hidden sorrow
 * (2^15)/2
 * this limit is based on player->hid_sorrow being s16b
 * more headroom than is necessary
 */

#define TR_MAX_HID_SORROW 16384

/* maximum deep sorrow
 * (2^15)/2
 * this limit is based on player->deep_sorrow being u16b
 * more headroom than is necessary
 */

#define TR_MAX_DEEP_SORROW 16384

/* modifies how often outcry over hidden sorrow should be
 * see game-world.c / process_world()
 */

#define TR_OUTCRY_PT 1000

/* Every 1 in TR_OUTCRY_DELAY turns, try to outcry
 * see game-world.c / is_hid_outcry_time()
 */
#define TR_OUTCRY_DELAY 100

/* if hid_sorrow is over 1000, there starts to be a chance
 * of shutdown.  1000 is just a guess.
 */

#define TR_HID_BREAKING_PT 1000

/* Minimum and maximum hid_sorrow_sensitivity values
 * see game-world.c / process_world()
 */

#define TR_HSS_MAX 2.0
#define TR_HSS_MIN 0.5

/* Avg. interval between hid_sorrow_sensitivity changes
 * see game-world.c / process_world()
 * 50000 is a guess
 */

#define TR_HID_SORROW_SENSITIVITY_INT 50000


/* These are the thresholds for where apparent sorrow processing bonuses
 * and penalties kick in (see player_process_ap_sorrow() in player.c)
 * 18, 18, 18, and 15 are all guesses
 */

#define TR_WIS_BONUS 18
#define TR_CON_BONUS 18
#define TR_CLEV_BONUS 15
#define TR_INT_PENALTY 18

/* Every turn the player can, he/she/? loses 1 apparent sorrow 
 * (see player.c / player_process_ap_sorrow()
 */

#define TR_AP_SOR_PROC_BASE 1

/* How long does it take on average for hidden sorrow to decrease, when 
 * apparent sorrow is gone? see game-world.c / process_world()
 */

#define TR_HID_SORROW_RECOVER 10

/* Thresholds for deep sorrow to become likely to increment (guesses, both)
 * based on level of apparent sorrow and hidden sorrow.  See game-world.c /
 * process_world().
 */

#define TR_DEEP_AP_INC 1000
#define TR_DEEP_HID_INC 100

/* Any monster with base XP (from monster.txt) higher than this causes no
 * apparent sorrow gain when player kills it.  60000 is base XP of Morgoth.
 * See mon-util.c
 */
#define TR_UNSAD_XP 60000

/* The amount of ap_sorrow incurred by killing a monster is given by
 * log((TR_UNSAD_XP / (monster base XP + 1)) * TR_SORROW_MULT  
 */

#define TR_SORROW_MULT 20

/* How long between deep sorrow outcry messages on average, see game-world.c /
 * process_world()
 */

#define TR_DEEP_SORROW_OUTCRY_INTERVAL 5000000

/* Chance of initiating permaterror (a NUM out of DENOM chance)
 * See mon-util.c / mon_take_hit()
 */
#define TR_PERMA_T_INIT_DENOM 100
#define TR_PERMA_T_INIT_NUM 20

/* Factor modifying how much apparent sorrow a player gains from causing
 * permaterror.  See mon-util.c / mon_take_hit()
 */

#define TR_PERMA_T_AS_FACTOR 0.02


