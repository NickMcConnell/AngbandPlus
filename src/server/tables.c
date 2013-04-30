/*
 * File: tables.c
 * Purpose: Finely-tuned constants for the game Angband
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
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


#include "s-angband.h"


/*
 * Global array for looping through the "keypad directions"
 */
s16b ddd[9] =
{ 2, 8, 6, 4, 3, 1, 9, 7, 5 };


/*
 * Global arrays for converting "keypad direction" into offsets
 */
s16b ddx[10] =
{ 0, -1, 0, 1, -1, 0, 1, -1, 0, 1 };

s16b ddy[10] =
{ 0, 1, 1, 1, 0, 0, 0, -1, -1, -1 };


/*
 * Global arrays for optimizing "ddx[ddd[i]]" and "ddy[ddd[i]]"
 */
s16b ddx_ddd[9] =
{ 0, 0, 1, -1, 1, -1, 1, -1, 0 };

s16b ddy_ddd[9] =
{ 1, -1, 0, 0, 1, 1, -1, -1, 0 };


/*
 * Stat Table (INT/WIS) -- failure rate adjustment
 */
const int adj_mag_stat[] =
{
    -5  /* 3 */,
    -4  /* 4 */,
    -3  /* 5 */,
    -3  /* 6 */,
    -2  /* 7 */,
    -1  /* 8 */,
     0  /* 9 */,
     0  /* 10 */,
     0  /* 11 */,
     0  /* 12 */,
     0  /* 13 */,
     1  /* 14 */,
     2  /* 15 */,
     3  /* 16 */,
     4  /* 17 */,
     5  /* 18/00-18/09 */,
     6  /* 18/10-18/19 */,
     7  /* 18/20-18/29 */,
     8  /* 18/30-18/39 */,
     9  /* 18/40-18/49 */,
    10  /* 18/50-18/59 */,
    11  /* 18/60-18/69 */,
    12  /* 18/70-18/79 */,
    15  /* 18/80-18/89 */,
    18  /* 18/90-18/99 */,
    21  /* 18/100-18/109 */,
    24  /* 18/110-18/119 */,
    27  /* 18/120-18/129 */,
    30  /* 18/130-18/139 */,
    33  /* 18/140-18/149 */,
    36  /* 18/150-18/159 */,
    39  /* 18/160-18/169 */,
    42  /* 18/170-18/179 */,
    45  /* 18/180-18/189 */,
    48  /* 18/190-18/199 */,
    51  /* 18/200-18/209 */,
    54  /* 18/210-18/219 */,
    57  /* 18/220+ */
};


/*
 * This table provides for different game speeds at different
 * dungeon depths.  Shallower depths are faster, allowing for
 * easier town navigation.  Deeper depths are slow, hopefully
 * make deep combat less of a test of reflexes.
 */
u16b level_speeds[128] =
{
     7500,  9000,  9100,  9200,  9300,  9400,  9500,  9600,  9700,  9800,   /* Town - 450' */
     9900, 10000, 10000, 10000, 10000, 10000, 10100, 10200, 10300, 10400,   /* 500' - 950' */
    10500, 10600, 10700, 10800, 10900, 11000, 11100, 11200, 11300, 11400,   /* 1000' - 1450' */
    11500, 11600, 11700, 11800, 11900, 12000, 12100, 12200, 12300, 12400,   /* 1500' - 1950' */
    12500, 12600, 12700, 12800, 12900, 13000, 13100, 13200, 13300, 13400,   /* 2000' - 2450' */
    13500, 13700, 13800, 13900, 14000, 14200, 14300, 14400, 14600, 14800,   /* 2500' - 2950' */
    15000, 15200, 15400, 15600, 15800, 16000, 16200, 16400, 16600, 16800,   /* 3000' - 3450' */
    17000, 17200, 17400, 17600, 17800, 18000, 18200, 18400, 18600, 18800,   /* 3500' - 3950' */
    19000, 19200, 19400, 19600, 19800, 20000, 20000, 20000, 20000, 20000,   /* 4000' - 4450' */
    20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000,   /* 4500' - 4950' */
    20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000,   /* 5000' - 5450' */
    20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000,   /* 5500' - 5950' */
    20000, 20000, 20000, 20000, 20000, 20000, 20000, 20000                  /* 6000' - 6350' */
};


/*
 * Ghost spells
 */
magic_type ghost_spells[GHOST_SPELLS] =
{
    {  1,   1, 0, 0},
    { 10,   2, 0, 0},
    { 15,   3, 0, 0},
    { 20,   5, 0, 0},
    { 25,  10, 0, 0},
    { 30,  20, 0, 0},
    { 35,  60, 0, 0},
    { 45, 100, 0, 0}
};


/*
 * Names of ghost spells
 */
const char *ghost_spell_names[GHOST_SPELLS] =
{
    "Blink",
    "Terrify",
    "Confuse",
    "Teleport",
    "Nether Bolt",
    "Disenchantment Bolt",
    "Nether Ball",
    "Darkness Storm"
};


/*
 * Directional info for ghost spells
 */
byte ghost_spell_dirs[GHOST_SPELLS] =
{
    0, 1, 1, 0, 1, 1, 1, 1
}; 


/*
 * Projectable info for ghost spells
 */
byte ghost_spell_projs[GHOST_SPELLS] =
{
    0, 0, 0, 0, 0, 0, 0, 0
};


/*
 * Descriptions of ghost spells
 */
const char *ghost_spell_descs[GHOST_SPELLS] =
{
    "Teleports you randomly up to 10 squares away.",
    "Attempts to scare a single monster for a level-dependant duration. Uniques and monsters that resist fear are not affected.",
    "Attempts to confuse a single monster for a level-dependant duration. Uniques and monsters that resist confusion are not affected.",
    "Teleports you randomly within the current level.",
    "Fires a nether bolt that always hits its target. Sometimes a beam is fired instead that hurts each monster in its path. The chance to get a beam goes up with your character level.",
    "Fires a disenchantment bolt that always hits its target. Sometimes a beam is fired instead that hurts each monster in its path. The chance to get a beam goes up with your character level.",
    "Shoots a radius-2 nether ball.",
    "Shoots a radius-3 ball of darkness."
};


/*
 * Mimic spells
 */
magic_type mimic_spells[MIMIC_SPELLS] =
{
    /* 0, mana, fail, spell flag */
    {0, 1, 1, RSF_SHRIEK},
    {0, 0, 8, RSF_ARROW_X},
    {0, 0, 1, RSF_ARROW_1},
    {0, 0, 3, RSF_ARROW_2},
    {0, 0, 5, RSF_ARROW_3},
    {0, 0, 6, RSF_ARROW_4},
    {0, 0, 14, RSF_BOULDER},
    {0, 17, 24, RSF_BA_ACID},
    {0, 9, 12, RSF_BA_ELEC},
    {0, 19, 27, RSF_BA_FIRE},
    {0, 9, 13, RSF_BA_COLD},
    {0, 3, 4, RSF_BA_POIS},
    {0, 20, 29, RSF_BA_NETH},
    {0, 18, 25, RSF_BA_WATE},
    {0, 35, 50, RSF_BA_MANA},
    {0, 35, 50, RSF_BA_DARK},
    {0, 3, 6, RSF_DRAIN_MANA},
    {0, 7, 10, RSF_MIND_BLAST},
    {0, 18, 26, RSF_BRAIN_SMASH},
    {0, 3, 4, RSF_CAUSE_1},
    {0, 7, 10, RSF_CAUSE_2},
    {0, 15, 22, RSF_CAUSE_3},
    {0, 23, 33, RSF_CAUSE_4},
    {0, 8, 11, RSF_BO_ACID},
    {0, 5, 7, RSF_BO_ELEC},
    {0, 9, 13, RSF_BO_FIRE},
    {0, 7, 10, RSF_BO_COLD},
    {0, 13, 19, RSF_BO_NETH},
    {0, 15, 22, RSF_BO_WATE},
    {0, 23, 33, RSF_BO_MANA},
    {0, 12, 17, RSF_BO_PLAS},
    {0, 9, 13, RSF_BO_ICEE},
    {0, 3, 4, RSF_MISSILE},
    {0, 5, 10, RSF_SCARE},
    {0, 4, 8, RSF_BLIND},
    {0, 4, 8, RSF_CONF},
    {0, 9, 18, RSF_SLOW},
    {0, 5, 10, RSF_HOLD},
    {0, 14, 28, RSF_HASTE},
    {0, 16, 32, RSF_HEAL},
    {0, 2, 4, RSF_BLINK},
    {0, 6, 12, RSF_TPORT},
    {0, 12, 24, RSF_TELE_TO},
    {0, 12, 24, RSF_TELE_AWAY},
    {0, 18, 36, RSF_TELE_LEVEL},
    {0, 3, 6, RSF_DARKNESS},
    {0, 10, 20, RSF_TRAPS},
    {0, 8, 16, RSF_FORGET},
    {0, 7, 14, RSF_ANIM_DEAD},
    {0, 35, 47, RSF_S_KIN},
    {0, 40, 50, RSF_S_HI_DEMON},
    {0, 20, 40, RSF_S_MONSTER},
    {0, 35, 47, RSF_S_MONSTERS},
    {0, 30, 45, RSF_S_ANIMAL},
    {0, 30, 45, RSF_S_SPIDER},
    {0, 35, 47, RSF_S_HOUND},
    {0, 30, 45, RSF_S_HYDRA},
    {0, 25, 42, RSF_S_AINU},
    {0, 25, 42, RSF_S_DEMON},
    {0, 25, 42, RSF_S_UNDEAD},
    {0, 25, 42, RSF_S_DRAGON},
    {0, 40, 50, RSF_S_HI_UNDEAD},
    {0, 40, 50, RSF_S_HI_DRAGON},
    {0, 40, 50, RSF_S_WRAITH},
    {0, 40, 50, RSF_S_UNIQUE}
};


/*
 * Names of mimic spells
 */
const char *mimic_spell_names[MIMIC_SPELLS] =
{
    "Shriek",
    "Seeker Arrow",
    "Shot",
    "Arrow",
    "Bolt",
    "Missile",
    "Boulder",
    "Acid Ball",
    "Lightning Ball",
    "Fire Ball",
    "Frost Ball",
    "Stinking Cloud",
    "Nether Ball",
    "Water Ball",
    "Mana Storm",
    "Darkness Storm",
    "Drain Mana",
    "Mind Blast",
    "Brain Smash",
    "Cause Light Wounds",
    "Cause Serious Wounds",
    "Cause Critical Wounds",
    "Cause Mortal Wounds",
    "Acid Bolt",
    "Lightning Bolt",
    "Fire Bolt",
    "Frost Bolt",
    "Nether Bolt",
    "Water Bolt",
    "Mana Bolt",
    "Plasma Bolt",
    "Ice Bolt",
    "Magic Missile",
    "Scare",
    "Blind",
    "Confuse",
    "Slow",
    "Hold",
    "Haste",
    "Heal",
    "Blink",
    "Teleport",
    "Teleport To",
    "Teleport Away",
    "Teleport Level",
    "Darkness",
    "Traps",
    "Forget",
    "Anime Dead",
    "Summon Kin",
    "Summon High Demons",
    "Summon Monster",
    "Summon Monsters",
    "Summon Animals",
    "Summon Spiders",
    "Summon Hounds",
    "Summon Hydras",
    "Summon Ainu",
    "Summon Demon",
    "Summon Undead",
    "Summon Dragon",
    "Summon High Undead",
    "Summon High Dragons",
    "Summon Wraiths",
    "Summon Uniques"
};


/*
 * Directional info for mimic spells
 */
byte mimic_spell_dirs[MIMIC_SPELLS] =
{
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};    


/*
 * Projectable info for mimic spells
 */
byte mimic_spell_projs[MIMIC_SPELLS] =
{
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};


/*
 * Descriptions of mimic spells
 */
const char *mimic_spell_descs[MIMIC_SPELLS] =
{
    "Awakens all sleeping monsters within 40 squares of you, and hastes all monsters within line of sight unless they are already hasted",
    "Fires a Seeker Arrow that always hits its target",
    "Fires a Shot that always hits its target",
    "Fires an Arrow that always hits its target",
    "Fires a Bolt that always hits its target",
    "Fires a Missile that always hits its target",
    "Fires a Boulder that always hits its target",
    "Shoots a radius-2 or radius-3 acid ball",
    "Shoots a radius-2 or radius-3 lightning ball",
    "Shoots a radius-2 or radius-3 fire ball",
    "Shoots a radius-2 or radius-3 frost ball",
    "Shoots a radius-2 or radius-3 poison ball",
    "Shoots a radius-2 or radius-3 nether ball",
    "Shoots a radius-2 or radius-3 water ball",
    "Shoots a radius-2 or radius-3 mana ball",
    "Shoots a radius-2 or radius-3 ball of darkness",
    "Drains mana from its target and cures an amount of damage points equal to 6 times the amount of mana drained",
    "Fires a mental blast that will damage minded creatures. This also has a chance to stun, confuse, put to sleep or scare affected monsters",
    "Fires a powerful mental blast that will damage minded creatures. This also has a chance to stun, confuse, put to sleep or scare affected monsters",
    "Cause 3d8 points of damage to its target",
    "Cause 8d8 points of damage to its target",
    "Cause 10d15 points of damage to its target",
    "Cause 15d15 points of damage to its target. This also has a chance to cause open wounds",
    "Fires an acid bolt that always hits its target",
    "Fires a lightning bolt that always hits its target",
    "Fires a fire bolt that always hits its target",
    "Fires a frost bolt that always hits its target",
    "Fires a nether bolt that always hits its target",
    "Fires a water bolt that always hits its target",
    "Fires a mana bolt that always hits its target",
    "Fires a plasma bolt that always hits its target",
    "Fires an ice bolt that always hits its target",
    "Fires a magic missile that always hits its target",
    "Attempts to scare a single monster for a level-dependant duration. Uniques and monsters that resist fear are not affected",
    "Attempts to blind a single monster for a level-dependant duration. Uniques and monsters that resist blindness are not affected",
    "Attempts to confuse a single monster for a level-dependant duration. Uniques and monsters that resist confusion are not affected",
    "Attempts to slow a single monster. Uniques are not affected",
    "Attempts to put to sleep a single monster. Uniques and monsters that resist sleep are not affected",
    "Hastes you (+10 to speed) for a level-dependant duration",
    "Cures a level-dependant amount of damage points, removes fear, neutralizes poison and heals all cut damage",
    "Teleports you randomly up to 10 squares away",
    "Teleports you randomly within the current level",
    "Teleports its target next to you",
    "Produces a beam that teleports each monster in its path up to 100 squares away",
    "Teleports you 1 level up or 1 level down (chosen at random)",
    "Unlights an area around you",
    "A trap is created on each empty floor space that is directly adjacent to you. This spell has no effect when used in the town",
    "Attempts to cause amnesia to a single monster for a level-dependant duration. Uniques and monsters that resist are not affected",
    "This power makes humanoid corpses and skeletons in a small radius around the caster rise as undead monsters",
    "Attempts to summon up to 6 similar monsters around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 greater demons around you. This spell has no effect when used in the town",
    "Attempts to summon one monster around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 monsters around you. This spell has no effect when used in the town",
    "Attempts to summon up to 6 animals around you. This spell has no effect when used in the town",
    "Attempts to summon up to 6 spiders around you. This spell has no effect when used in the town",
    "Attempts to summon up to 6 hounds around you. This spell has no effect when used in the town",
    "Attempts to summon up to 6 hydras around you. This spell has no effect when used in the town",
    "Attempts to summon an ainu around you. This spell has no effect when used in the town",
    "Attempts to summon a demon around you. This spell has no effect when used in the town",
    "Attempts to summon an undead monster around you. This spell has no effect when used in the town",
    "Attempts to summon a dragon around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 greater undead monsters around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 ancient dragons around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 wraiths around you. This spell has no effect when used in the town",
    "Attempts to summon up to 8 unique monsters around you. This spell has no effect when used in the town"
};


/* Martial arts */
martial_arts ma_blows[MAX_MA] =
{
    {"punch",       "",                         1,  0,  1, 4,  0},
    {"kick",        "",                         2,  1,  1, 5,  0},
    {"strike",      "",                         3,  2,  1, 6,  0},
    {"hit",         " with your knee",          5,  3,  2, 3,  MA_KNEE},
    {"hit",         " with your elbow",         7,  4,  1, 7,  0},
    {"butt",        "",                         9,  5,  2, 4,  0},
    {"kick",        "",                         11, 6,  3, 3,  MA_SLOW},
    {"uppercut",    "",                         13, 7,  3, 4,  MA_STUN},
    {"double-kick", "",                         16, 8,  4, 4,  MA_STUN},
    {"hit",         " with a Cat's Claw",       20, 10, 4, 5,  0},
    {"hit",         " with a jump kick",        25, 13, 5, 5,  MA_STUN},
    {"hit",         " with an Eagle's Claw",    29, 15, 4, 8,  0},
    {"hit",         " with a circle kick",      33, 17, 6, 6,  MA_STUN},
    {"hit",         " with an Iron Fist",       37, 19, 5, 9,  MA_STUN},
    {"hit",         " with a flying kick",      41, 21, 7, 8,  MA_STUN},
    {"hit",         " with a Dragon Fist",      45, 23, 6, 11, MA_STUN},
    {"hit",         " with a Crushing Blow",    48, 24, 9, 9,  MA_STUN}
};


const char *store_names[MAX_STORES] =
{
    "General Store",
    "Armoury",
    "Weapon Smith",
    "Temple",
    "Alchemy Shop",
    "Magic Shop",
    "House of Arcanes",
    "Black Market",
    "Expensive Black Market",
    "Tavern",
    "Player Shop"
};


const char *inscrip_text[] =
{
    NULL,
    "average",
    "magical",
    "strange",
    "magical",
    "splendid",
    "excellent",
    "special"
};
