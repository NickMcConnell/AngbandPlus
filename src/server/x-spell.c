/*
 * File: x-spell.c
 * Purpose: Spell effect definitions and information about them
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
#include "../common/tvalsval.h"
#include "effects.h"
#include "generate.h"
#include "init.h"
#include "monster/mon-spell.h"
#include "monster/mon-util.h"
#include "s-spells.h"
#include "target.h"


/*
 * The defines below must match the spell numbers in spell.txt
 * if they don't, "interesting" things will probably happen.
 *
 * It would be nice if we could get rid of this dependency.
 */

/* Mage spells */
#define SPELL_MAGIC_MISSILE         0
#define SPELL_DETECT_MONSTERS       1
#define SPELL_PHASE_DOOR            2
#define SPELL_LIGHT_AREA            3
#define SPELL_CURE_LIGHT_WOUNDS     4
#define SPELL_FIND_TRAPS_DOORS      5
#define SPELL_STINKING_CLOUD        6
#define SPELL_CONFUSE_MONSTER       7
#define SPELL_LIGHTNING_BOLT        8
#define SPELL_TRAP_DOOR_DESTRUCTION 9
#define SPELL_CURE_POISON           10
#define SPELL_SLEEP_MONSTER         11
#define SPELL_TELEPORT_SELF         12
#define SPELL_SPEAR_OF_LIGHT        13
#define SPELL_FROST_BOLT            14
#define SPELL_WONDER                15
#define SPELL_SATISFY_HUNGER        16
#define SPELL_RECHARGE_ITEM_I       17
#define SPELL_TURN_STONE_TO_MUD     18
#define SPELL_FIRE_BOLT             19
#define SPELL_POLYMORPH_OTHER       20
#define SPELL_IDENTIFY              21
#define SPELL_DETECT_INVISIBLE      22
#define SPELL_ACID_BOLT             23
#define SPELL_SLOW_MONSTER          24
#define SPELL_FROST_BALL            25
#define SPELL_TELEPORT_OTHER        26
#define SPELL_HASTE_SELF            27
#define SPELL_MASS_SLEEP            28
#define SPELL_FIRE_BALL             29
#define SPELL_TREASURE_DETECTION    30
#define SPELL_RESIST_COLD           31
#define SPELL_RESIST_FIRE           32
#define SPELL_RESIST_POISON         33
#define SPELL_RESISTANCE            34
#define SPELL_SHIELD                35
#define SPELL_SHOCK_WAVE            36
#define SPELL_EXPLOSION             37
#define SPELL_CLOUD_KILL            38
#define SPELL_ACID_BALL             39
#define SPELL_ICE_STORM             40
#define SPELL_METEOR_SWARM          41
#define SPELL_RIFT                  42
#define SPELL_DOOR_CREATION         43
#define SPELL_STAIR_CREATION        44
#define SPELL_TELEPORT_LEVEL        45
#define SPELL_WORD_OF_RECALL        46
#define SPELL_RUNE_OF_PROTECTION    47
#define SPELL_HEROISM               48
#define SPELL_BERSERKER             49
#define SPELL_ENCHANT_ARMOR         50
#define SPELL_ENCHANT_WEAPON        51
#define SPELL_RECHARGE_ITEM_II      52
#define SPELL_ELEMENTAL_BRAND       53
#define SPELL_SHIELD_GOI            54
#define SPELL_EARTHQUAKE            55
#define SPELL_BEDLAM                56
#define SPELL_REND_SOUL             57
#define SPELL_BANISHMENT            58
#define SPELL_WORD_OF_DESTRUCTION   59
#define SPELL_MASS_BANISHMENT       60
#define SPELL_CHAOS_STRIKE          61
#define SPELL_MANA_STORM            62

/* Priest spells */
#define PRAYER_DETECT_EVIL          0
#define PRAYER_CURE_LIGHT_WOUNDS    1
#define PRAYER_BLESS                2
#define PRAYER_REMOVE_FEAR          3
#define PRAYER_CALL_LIGHT           4
#define PRAYER_FIND_TRAPS_DOORS     5
#define PRAYER_SLOW_POISON          7
#define PRAYER_SCARE_MONSTER        8
#define PRAYER_PORTAL               9
#define PRAYER_CURE_SERIOUS_WOUNDS  10
#define PRAYER_CHANT                11
#define PRAYER_SANCTUARY            12
#define PRAYER_SATISFY_HUNGER       13
/*#define PRAYER_REMOVE_CURSE         14*/
#define PRAYER_RESIST_HEAT_COLD     15
#define PRAYER_NEUTRALIZE_POISON    16
#define PRAYER_ORB_OF_DRAINING      17
#define PRAYER_CURE_CRITICAL_WOUNDS 18
#define PRAYER_SENSE_INVISIBLE      19
#define PRAYER_PROTECTION_FROM_EVIL 20
#define PRAYER_EARTHQUAKE           21
#define PRAYER_SENSE_SURROUNDINGS   22
#define PRAYER_CURE_MORTAL_WOUNDS   23
#define PRAYER_TURN_UNDEAD          24
#define PRAYER_PRAYER               25
#define PRAYER_DISPEL_UNDEAD        26
#define PRAYER_HEAL                 27
#define PRAYER_DISPEL_EVIL          28
#define PRAYER_GLYPH_OF_WARDING     29
#define PRAYER_HOLY_WORD            30
#define PRAYER_BLINK                31
#define PRAYER_TELEPORT_SELF        32
#define PRAYER_TELEPORT_OTHER       33
#define PRAYER_TELEPORT_LEVEL       34
#define PRAYER_WORD_OF_RECALL       35
#define PRAYER_ALTER_REALITY        36
#define PRAYER_DETECT_MONSTERS      37
#define PRAYER_DETECTION            38
#define PRAYER_PERCEPTION           39
#define PRAYER_PROBING              40
#define PRAYER_CLAIRVOYANCE         41
#define PRAYER_CURE_WOUNDS          42
#define PRAYER_HEALING              43
#define PRAYER_EXTRA_HEALING        44
#define PRAYER_RESTORATION          45
#define PRAYER_REMEMBRANCE          46
#define PRAYER_RESURRECTION         47
#define PRAYER_UNBARRING_WAYS       48
#define PRAYER_RECHARGING           49
/*#define PRAYER_DISPEL_CURSE         50*/
#define PRAYER_ENCHANT_WEAPON       51
#define PRAYER_ENCHANT_ARMOUR       52
#define PRAYER_ELEMENTAL_BRAND      53
#define PRAYER_DISPEL_UNDEAD2       54
#define PRAYER_DISPEL_EVIL2         55
#define PRAYER_BANISH_EVIL          56
#define PRAYER_WORD_OF_DESTRUCTION  57
#define PRAYER_ANNIHILATION         58

/* Sorceror spells */
#define SSPELL_MAGIC_MISSILE    0
#define SSPELL_DETECT_MONSTERS  1
#define SSPELL_PHASE_DOOR       2
#define SSPELL_LIGHT_AREA       3
#define SSPELL_FIND_TRAPS_DOORS 4
#define SSPELL_NOXIOUS_CLOUD    5
#define SSPELL_DETECT_TREASURE  6
#define SSPELL_CONFUSE_MONSTER  7
#define SSPELL_LIGHTNING_BOLT   8
#define SSPELL_TRAP_DOOR_DESTR  9
#define SSPELL_SLEEP_MONSTER    10
#define SSPELL_TELEPORT_SELF    11
#define SSPELL_BEAM_LIGHT       12
#define SSPELL_FROST_BOLT       13
#define SSPELL_CREATE_FOOD      14
#define SSPELL_STONE_MUD        15
#define SSPELL_FIRE_BOLT        16
#define SSPELL_POLY_OTHER       17
#define SSPELL_IDENTIFY_ITEM    18
#define SSPELL_ETHEREAL_EYE     19
#define SSPELL_RADIATE_FEAR     20
#define SSPELL_DETECT_INVIS     21
#define SSPELL_ELEMENTAL_BOLT   22
#define SSPELL_MEDITATE_MANA    23
#define SSPELL_TELEPORT_AWAY    24
#define SSPELL_RECHARGE_ITEM    25
#define SSPELL_HASTE_SELF       26
#define SSPELL_FIRE_STORM       27
#define SSPELL_SUN_FIRE         28
#define SSPELL_SAFE_GUARD       29
#define SSPELL_CREATE_WALLS     30
#define SSPELL_CREATE_STAIRS    31
#define SSPELL_ELEM_SHIELD      32
#define SSPELL_WORD_RECALL      33
#define SSPELL_FORCE_SHIELD     34
#define SSPELL_CHAOS_BLAST      35
#define SSPELL_WRAITH_FORM      36
#define SSPELL_MANA_SHIELD      37
#define SSPELL_PLASMA_BLAST     38
#define SSPELL_MANA_BLAST       39
#define SSPELL_MIND_VISION      40
#define SSPELL_TELE_OBJECT      41
#define SSPELL_IDENTIFY_FULL    42
#define SSPELL_TIMED_ESP        43
#define SSPELL_ENLIGHT_LEVEL    44
#define SSPELL_EARTH_QUAKE      45
#define SSPELL_WIPE_AREA        46
#define SSPELL_WORD_DESTRUCTION 47
#define SSPELL_OBLIVION_BLAST   48
#define SSPELL_ANNIHIL_BOLT     49
#define SSPELL_MANA_STRIKE      50
#define SSPELL_TIDAL_WAVE       51
#define SSPELL_ANARCHY_FORCE    52

/* Rogue spells */
#define RSPELL_DETECT_MONSTERS  0
#define RSPELL_TELEPORT_BLINK   1
#define RSPELL_DETECT_FEATURES  2
#define RSPELL_LIGHT_AREA       3
#define RSPELL_CREATE_POISON    4
/*#define RSPELL_REMOVE_CURSE     5*/
#define RSPELL_OBJECT_DETECTION 6
#define RSPELL_DESTROY_FEATURES 7
#define RSPELL_CURE_POISON      8
#define RSPELL_IDENTIFY_ITEM    9
#define RSPELL_DETECT_INVIS     10
#define RSPELL_STINKING_CLOUD   11
#define RSPELL_TELEPORT_SELF    12
#define RSPELL_SPEAR_LIGHT      13
#define RSPELL_STONE_MUD        14
#define RSPELL_SAT_HUNGER       15
#define RSPELL_SLEEP_MONSTERS   16
#define RSPELL_DEEP_NIGHTS      17
#define RSPELL_CLOAK_CHANGT     18
#define RSPELL_CONF_BALL        19
#define RSPELL_CREATE_TRAPS     20
#define RSPELL_SCARE_MONSTERS   21
#define RSPELL_STUN_MONSTERS    22
#define RSPELL_SUMMON_MONSTERS  23
#define RSPELL_CREATE_DOORS     24
#define RSPELL_INNER_SIGHT      25
#define RSPELL_FADE_SHADOWS     26
#define RSPELL_CREATE_STAIRS    27
#define RSPELL_TELEPORT_LEVEL   28
#define RSPELL_TELEPORT_TARGET  29
#define RSPELL_TELEPORT_OTHER   30
#define RSPELL_FAST_RECALL      31
#define RSPELL_DISARM_RAY       32
#define RSPELL_IDENTIFY_FLOOR   33
#define RSPELL_RECHARGE_ITEM    34
#define RSPELL_ENCHANT_ARMOR    35
/*#define RSPELL_BANISH_CURSE     36*/
#define RSPELL_ENCHANT_WEAPON   37
#define RSPELL_ELEM_BRAND       38
#define RSPELL_IDENTIFY_FULLY   39
#define RSPELL_CLOUD_KILL       40
#define RSPELL_RESIST_ELEMENTS  41
#define RSPELL_DAY_MISRULE      42
#define RSPELL_ARMOR_NIGHT      43
#define RSPELL_SHOCK_WAVE       44
#define RSPELL_AVOID_TRAPS      45

/* Archer spells */
enum
{
    ASPELL_SHOT_ELEC = 0,
    ASPELL_DETECT_MONSTERS,
    ASPELL_SHOT_COLD,
    ASPELL_LIGHT_AREA,
    ASPELL_SHOT_FIRE,
    ASPELL_FARSIGHT,
    ASPELL_SHOT_ACID,
    ASPELL_SHOT_THUNDER,
    ASPELL_SHOT_CONF,
    ASPELL_SHOT_POISON,
    ASPELL_SHOT_PIERCE,
    ASPELL_SHOT_SHARDS,
    ASPELL_SHOT_ICE,
    ASPELL_SHOT_FLAME,
    ASPELL_SHOT_WATER,
    ASPELL_SHOT_POWER,
    ASPELL_SHOT_SONIC
};

/* Telepath spells */
#define TSPELL_MIND_BLAST       0
#define TSPELL_DETECT_MONSTERS  1
#define TSPELL_TELEPORT_BLINK   2
#define TSPELL_LIGHT_AREA       3
#define TSPELL_DETECT_FEATURES  4
#define TSPELL_CONFUSE_MONSTER  5
#define TSPELL_CELL_ADJUST      6
#define TSPELL_TELEPORT_SELF    7
#define TSPELL_STONE_MUD        8
#define TSPELL_MIND_VISION      9
#define TSPELL_SLEEP_MONSTERS   10
#define TSPELL_SENSE_SURROUND   11
#define TSPELL_IDENTIFY_ITEM    12
#define TSPELL_SAT_HUNGER       13
#define TSPELL_STUN_MONSTERS    14
#define TSPELL_KINETIC_BLAST    15
#define TSPELL_PSI_DRAIN        16
#define TSPELL_MIND_WAVE        17
#define TSPELL_DETECTION        18
#define TSPELL_TELE_OBJECT      19
#define TSPELL_FORCE_SHIELD     20
#define TSPELL_ELEM_SHIELD      21
#define TSPELL_TIMED_ESP        22
#define TSPELL_SONIC_WAVE       23
#define TSPELL_WORD_RECALL      24
#define TSPELL_SPACE_TIME       25
#define TSPELL_TELEPORT_AWAY    26
#define TSPELL_PROBA_TRAVEL     27
#define TSPELL_INERTIA_WAVE     28
#define TSPELL_TIME_BOLT        29
#define TSPELL_BIOFEEDBACK      30
#define TSPELL_ENLIGHT_LEVEL    31
#define TSPELL_KINETIC_WAVE     32
#define TSPELL_EXTRA_HEALING    33
#define TSPELL_ADRENALINE       34

/* Necromancer spells */
#define NSPELL_HORRIFY              0
#define NSPELL_DETECT_MONSTERS      1
#define NSPELL_PHASE_DOOR           2
#define NSPELL_REMOVE_FEAR          3
#define NSPELL_FIND_TRAPS_DOORS     4
#define NSPELL_RAISE_DEAD           5
#define NSPELL_CONFUSE_MONSTER      6
#define NSPELL_FROST_BOLT           7
#define NSPELL_TRAP_DOOR_DESTRUCT   8
#define NSPELL_RESIST_COLD          9
#define NSPELL_TELEPORT_SELF        10
#define NSPELL_TURN_STONE_TO_MUD    11
#define NSPELL_SHADOW_TOUCH         12
#define NSPELL_SATISFY_HUNGER       13
#define NSPELL_GUARD                14
#define NSPELL_IDENTIFY             15
#define NSPELL_FROST_BALL           16
#define NSPELL_RECHARGE_ITEM        17
#define NSPELL_DETECT_INVISIBLE     18
#define NSPELL_ABSORB_SOUL          19
#define NSPELL_HORRIFY_BEAM         20
#define NSPELL_TURN_UNDEAD          21
#define NSPELL_DISPEL_UNDEAD        22
#define NSPELL_NETHER_BOLT          23
#define NSPELL_FOLLOW               24
#define NSPELL_ICE_STORM            25
#define NSPELL_HASTE_SELF           26
#define NSPELL_RESISTANCE           27
#define NSPELL_UNDEAD_FORM          28
#define NSPELL_DRAIN_LIFE           29
#define NSPELL_TELEPORT_LEVEL       30
#define NSPELL_ELEMENTAL_BRAND      31
#define NSPELL_TREASURE_DETECTION   32
#define NSPELL_WORD_OF_RECALL       33
#define NSPELL_SHIELD               34
#define NSPELL_WORD_OF_DESTRUCTION  35
#define NSPELL_NETHER_BALL          36
#define NSPELL_HORRIFY_BALL         37
#define NSPELL_RUNE_OF_PROTECTION   38
#define NSPELL_CLOUD_DRAINING       39
#define NSPELL_ATTACK               40
#define NSPELL_DARKNESS_STORM       41
#define NSPELL_DEATH                42
#define NSPELL_MASS_HORRIFY         43
#define NSPELL_TOUCH_DEATH          44

/* Elementalist spells */
#define ESPELL_SHOCKING_GRASP   0
#define ESPELL_LIGHTNING_BOLT   1
#define ESPELL_RESIST_ELEC      2
#define ESPELL_RESIST_POISON    3
#define ESPELL_LIGHTNING_BALL   4
#define ESPELL_TOXIC_CLOUD      5
#define ESPELL_SHOCK_WAVE       6
#define ESPELL_THUNDERSTORM     7
#define ESPELL_BURNING_HANDS    8
#define ESPELL_FIRE_BOLT        9
#define ESPELL_ILLUMINATION     10
#define ESPELL_RESIST_FIRE      11
#define ESPELL_FIRE_BALL        12
#define ESPELL_FIREFLASH        13
#define ESPELL_PLASMA_BALL      14
#define ESPELL_INFERNO          15
#define ESPELL_STONE_MUD        16
#define ESPELL_ACID_BOLT        17
#define ESPELL_RESIST_ACID      18
#define ESPELL_WALL_CREATION    19
#define ESPELL_ACID_BALL        20
#define ESPELL_STONE_SKIN       21
#define ESPELL_EXPLOSION        22
#define ESPELL_WALL_FORCE       23
#define ESPELL_FREEZING_AURA    24
#define ESPELL_FROST_BOLT       25
#define ESPELL_RESIST_COLD      26
#define ESPELL_POTION_VITALITY  27
#define ESPELL_FROST_BALL       28
#define ESPELL_ICE_STORM        29
#define ESPELL_TIDAL_WAVE       30
#define ESPELL_MAELSTROM        31
#define ESPELL_ELEM_POWER       32
#define ESPELL_ELEM_BOLT        33
#define ESPELL_MINOR_HAVOC      34
#define ESPELL_RESISTANCE       35
#define ESPELL_ELEM_BALL        36
#define ESPELL_ELEM_BRAND       37
#define ESPELL_MAJOR_HAVOC      38
#define ESPELL_ELEM_STORM       39

/* Summoner spells */
#define ZSPELL_SUMMON_MONSTER           0
#define ZSPELL_DETECT_MONSTERS          1
#define ZSPELL_BLINK                    2
#define ZSPELL_CALL_LIGHT               3
#define ZSPELL_CURE_LIGHT_WOUNDS        4
#define ZSPELL_FIND_TRAPS_DOORS         5
#define ZSPELL_TRAP_DOOR_DESTRUCTION    6
#define ZSPELL_FARSIGHT                 7
#define ZSPELL_SENSE_SURROUNDINGS       8
#define ZSPELL_CHARM                    9
#define ZSPELL_HEALING                  10
#define ZSPELL_REGENERATION             11
#define ZSPELL_RECOVERY                 12
#define ZSPELL_HARMONY                  13
#define ZSPELL_SUMMON_JELLY             14
#define ZSPELL_SUMMON_GOLEM             15
#define ZSPELL_SUMMON_VORTEX            16
#define ZSPELL_SUMMON_HYDRA             17
#define ZSPELL_CONTROL                  18
#define ZSPELL_RESILIENCE               19
#define ZSPELL_THUNDERSTORM             20
#define ZSPELL_DETONATE                 21
#define ZSPELL_SUPPRESS_SUMMONING       22
#define ZSPELL_BANISHMENT               23
#define ZSPELL_MASS_BANISHMENT          24

/* Ghost spells */
#define GSPELL_TELEPORT_BLINK   0
#define GSPELL_SCARE_MONSTERS   1
#define GSPELL_CONFUSE_MONSTERS 2
#define GSPELL_TELEPORT_SELF    3
#define GSPELL_NETHER_BOLT      4
#define GSPELL_DISEN_BOLT       5
#define GSPELL_NETHER_BALL      6
#define GSPELL_DARK_BALL        7


#define msg_prayer(P, A) msg_print_near((P), MSG_PY_PRAYER, (A))


const char *get_spell_name(int tval, int spell)
{
    return s_info[spell + PY_MAX_SPELLS * (tval - TV_MAGIC_BOOK)].name;
}


byte get_spell_dir(int tval, int spell)
{
    return s_info[spell + PY_MAX_SPELLS * (tval - TV_MAGIC_BOOK)].sdir;
}   


byte get_spell_proj(int tval, int spell)
{
    return s_info[spell + PY_MAX_SPELLS * (tval - TV_MAGIC_BOOK)].sproj;
}


const char *get_spell_desc(int tval, int spell)
{
    return s_info[spell + PY_MAX_SPELLS * (tval - TV_MAGIC_BOOK)].text;
}


void get_spell_info(struct player *p, int spell, char *buf, size_t len)
{
    int tval = p->clazz->spell_book;
    int plev = p->lev;

    /* Blank 'buf' first */
    buf[0] = '\0';

    /* Mage spells */
    if (tval == TV_MAGIC_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case SPELL_MAGIC_MISSILE:
                strnfmt(buf, len, " dam %dd4", 3 + (plev - 1) / 5);
                break;
            case SPELL_PHASE_DOOR:
                my_strcpy(buf, " range 10", len);
                break;
            case SPELL_LIGHT_AREA:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case SPELL_CURE_LIGHT_WOUNDS:
                my_strcpy(buf, " heal 20", len);
                break;
            case SPELL_STINKING_CLOUD:
                strnfmt(buf, len, " dam %d", 10 + plev / 2);
                break;
            case SPELL_LIGHTNING_BOLT:
                strnfmt(buf, len, " dam %dd6", 3 + (plev - 5) / 6);
                break;
            case SPELL_FROST_BOLT:
                strnfmt(buf, len, " dam %dd8", 5 + (plev - 5) / 4);
                break;
            case SPELL_ACID_BOLT:
                strnfmt(buf, len, " dam %dd8", 8 + (plev - 5) / 4);
                break;
            case SPELL_FIRE_BOLT:
                strnfmt(buf, len, " dam %dd8", 6 + (plev - 5) / 4);
                break;
            case SPELL_SPEAR_OF_LIGHT:
                my_strcpy(buf, " dam 6d8", len);
                break;
            case SPELL_HEROISM:
                my_strcpy(buf, " dur 25+d25", len);
                break;
            case SPELL_BERSERKER:
                my_strcpy(buf, " dur 25+d25", len);
                break;
            case SPELL_HASTE_SELF:
                strnfmt(buf, len, " dur %d+d20", plev);
                break;
            case SPELL_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", plev * 5);
                break;
            case SPELL_SHOCK_WAVE:
                strnfmt(buf, len, " dam %d", 10 + plev);
                break;
            case SPELL_EXPLOSION:
                strnfmt(buf, len, " dam %d", 20 + plev * 2);
                break;
            case SPELL_CLOUD_KILL:
                strnfmt(buf, len, " dam %d", 40 + plev / 2);
                break;
            case SPELL_REND_SOUL:
                strnfmt(buf, len, " dam 11d%d", plev);
                break;
            case SPELL_CHAOS_STRIKE:
                strnfmt(buf, len, " dam 13d%d", plev);
                break;
            case SPELL_RESIST_COLD:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SPELL_RESIST_FIRE:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SPELL_RESIST_POISON:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SPELL_RESISTANCE:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SPELL_SHIELD:
                my_strcpy(buf, " dur 30+d20", len);
                break;
            case SPELL_FROST_BALL:
                strnfmt(buf, len, " dam %d", 30 + plev);
                break;
            case SPELL_ACID_BALL:
                strnfmt(buf, len, " dam %d", 40 + plev);
                break;
            case SPELL_FIRE_BALL:
                strnfmt(buf, len, " dam %d", 55 + plev);
                break;
            case SPELL_ICE_STORM:
                strnfmt(buf, len, " dam %d", 50 + plev * 2);
                break;
            case SPELL_METEOR_SWARM:
                strnfmt(buf, len, " dam %dx%d", 30 + plev / 2, 2 + plev / 20);
                break;
            case SPELL_RIFT:
                strnfmt(buf, len, " dam 40+%dd7", plev);
                break;
            case SPELL_MANA_STORM:
                strnfmt(buf, len, " dam %d", 300 + plev * 2);
                break;
            case SPELL_SHIELD_GOI:
                my_strcpy(buf, " dur 8+d10", len);
                break;
        }
    }

    /* Priest spells */
    if (tval == TV_PRAYER_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case PRAYER_CURE_LIGHT_WOUNDS:
                my_strcpy(buf, " heal 20", len);
                break;
            case PRAYER_BLESS:
                my_strcpy(buf, " dur 12+d12", len);
                break;
            case PRAYER_CALL_LIGHT:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case PRAYER_PORTAL:
                strnfmt(buf, len, " range %d", 3 * plev);
                break;
            case PRAYER_CURE_SERIOUS_WOUNDS:
                my_strcpy(buf, " heal 40", len);
                break;
            case PRAYER_CHANT:
                my_strcpy(buf, " dur 24+d24", len);
                break;
            case PRAYER_RESIST_HEAT_COLD:
                my_strcpy(buf, " dur 10+d10", len);
                break;
            case PRAYER_ORB_OF_DRAINING:
                strnfmt(buf, len, " %d+3d6", plev + plev / (player_has(p, PF_ZERO_FAIL)? 2: 4));
                break;
            case PRAYER_CURE_CRITICAL_WOUNDS:
                my_strcpy(buf, " heal 60", len);
                break;
            case PRAYER_SENSE_INVISIBLE:
                my_strcpy(buf, " dur 24+d24", len);
                break;
            case PRAYER_PROTECTION_FROM_EVIL:
                strnfmt(buf, len, " dur %d+d25", 3 * plev);
                break;
            case PRAYER_CURE_MORTAL_WOUNDS:
                my_strcpy(buf, " heal 80", len);
                break;
            case PRAYER_PRAYER:
                my_strcpy(buf, " dur 48+d48", len);
                break;
            case PRAYER_DISPEL_UNDEAD:
                strnfmt(buf, len, " dam 1d%d", 3 * plev);
                break;
            case PRAYER_HEAL:
                my_strcpy(buf, " heal 35%", len);
                break;
            case PRAYER_DISPEL_EVIL:
                strnfmt(buf, len, " dam 1d%d", 3 * plev);
                break;
            case PRAYER_HOLY_WORD:
                my_strcpy(buf, " heal 1000", len);
                break;
            case PRAYER_CURE_WOUNDS:
                strnfmt(buf, len, " heal %d", (3 + plev / 10) * 10);
                break;
            case PRAYER_HEALING:
                my_strcpy(buf, " heal 100", len);
                break;
            case PRAYER_EXTRA_HEALING:
                my_strcpy(buf, " heal 2000", len);
                break;
            case PRAYER_DISPEL_UNDEAD2:
                strnfmt(buf, len, " dam 1d%d", 4 * plev);
                break;
            case PRAYER_DISPEL_EVIL2:
                strnfmt(buf, len, " dam 1d%d", 4 * plev);
                break;
            case PRAYER_ANNIHILATION:
                my_strcpy(buf, " dam 200", len);
                break;
            case PRAYER_BLINK:
                my_strcpy(buf, " range 10", len);
                break;
            case PRAYER_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", 8 * plev);
                break;
        }
    }
    
    /* Sorceror spells */
    if (tval == TV_SORCERY_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case SSPELL_MAGIC_MISSILE:
                strnfmt(buf, len, " dam %dd%d", 3 + (plev - 1) / 5, 4 + plev / 10);
                break;
            case SSPELL_PHASE_DOOR:
                my_strcpy(buf, " range 10", len);
                break;
            case SSPELL_LIGHT_AREA:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case SSPELL_NOXIOUS_CLOUD:
                strnfmt(buf, len, " dam %d", 10 + plev / 2);
                break;
            case SSPELL_LIGHTNING_BOLT:
                strnfmt(buf, len, " dam %dd6", 4 + (plev - 5) / 6);
                break;
            case SSPELL_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", plev * 5);
                break;
            case SSPELL_BEAM_LIGHT:
                my_strcpy(buf, " dam 6d8", len);
                break;
            case SSPELL_FROST_BOLT:
                strnfmt(buf, len, " dam %dd8", 6 + (plev - 5) / 4);
                break;
            case SSPELL_FIRE_BOLT:
                strnfmt(buf, len, " dam %dd8", 9 + (plev - 5) / 4);
                break;
            case SSPELL_ELEMENTAL_BOLT:
                strnfmt(buf, len, " dam 4x%dd8", 5 + (plev - 5) / 4);
                break;
            case SSPELL_MEDITATE_MANA:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SSPELL_HASTE_SELF:
                strnfmt(buf, len, " dur %d+d%d", plev, 20 + plev);
                break;
            case SSPELL_FIRE_STORM:
                strnfmt(buf, len, " dam %d", 110 + plev * 2);
                break;
            case SSPELL_SUN_FIRE:
                strnfmt(buf, len, " dam %d", 130 + plev * 2);
                break;
            case SSPELL_ELEM_SHIELD:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case SSPELL_FORCE_SHIELD:
                my_strcpy(buf, " dur 30+d20", len);
                break;
            case SSPELL_CHAOS_BLAST:
                strnfmt(buf, len, " dam %d", 100 + plev * 2);
                break;
            case SSPELL_WRAITH_FORM:
                strnfmt(buf, len, " dur %d+d20", 10 + plev);
                break;
            case SSPELL_MANA_SHIELD:
                my_strcpy(buf, " dur 8+d10", len);
                break;
            case SSPELL_PLASMA_BLAST:
                strnfmt(buf, len, " dam %d", 250 + plev * 2);
                break;
            case SSPELL_MANA_BLAST:
                strnfmt(buf, len, " dam %d", 350 + plev * 2);
                break;
            case SSPELL_TIMED_ESP:
                strnfmt(buf, len, " dur %d+d10", plev);
                break;
            case SSPELL_ANNIHIL_BOLT:
                strnfmt(buf, len, " dam %d", 400 + plev * 2);
                break;
            case SSPELL_MANA_STRIKE:
                strnfmt(buf, len, " dam %d", 350 + plev * 2);
                break;
            case SSPELL_TIDAL_WAVE:
                strnfmt(buf, len, " dam %d", 450 + plev * 2);
                break;
            case SSPELL_ANARCHY_FORCE:
                strnfmt(buf, len, " dam 8x%d", 100 + plev * 2);
                break;
        }
    }

    /* Rogue spells */
    if (tval == TV_SHADOW_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case RSPELL_TELEPORT_BLINK:
                my_strcpy(buf, " range 10", len);
                break;
            case RSPELL_LIGHT_AREA:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case RSPELL_STINKING_CLOUD:
                strnfmt(buf, len, " dam %d", 10 + plev / 2);
                break;
            case RSPELL_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", plev * 5);
                break;
            case RSPELL_SPEAR_LIGHT:
                my_strcpy(buf, " dam 6d8", len);
                break;
            case RSPELL_CLOAK_CHANGT:
                strnfmt(buf, len, " dur %d+d20", 20 + plev);
                break;
            case RSPELL_FADE_SHADOWS:
                strnfmt(buf, len, " dur %d+d20", 20 + plev);
                break;
            case RSPELL_TELEPORT_TARGET:
                strnfmt(buf, len, " range %d", 10 + plev / 5);
                break;
            case RSPELL_FAST_RECALL:
                strnfmt(buf, len, " delay %d+d%d", 50 - plev, 56 - plev);
                break;
            case RSPELL_CLOUD_KILL:
                strnfmt(buf, len, " dam %d", 40 + plev / 2);
                break;
            case RSPELL_RESIST_ELEMENTS:
                my_strcpy(buf, " dam 20+d20", len);
                break;
            case RSPELL_DAY_MISRULE:
                my_strcpy(buf, " dam 30+d30", len);
                break;
            case RSPELL_ARMOR_NIGHT:
                my_strcpy(buf, " dam 30+d20", len);
                break;
            case RSPELL_SHOCK_WAVE:
                strnfmt(buf, len, " dam %d", 10 + plev);
                break;
            case RSPELL_AVOID_TRAPS:
                strnfmt(buf, len, " dur %d+d20", 30 + plev);
                break;
        }
    }

    /* Archer spells */
    if (tval == TV_HUNT_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case ASPELL_SHOT_ELEC:
                my_strcpy(buf, " dam 3/shot", len);
                break;
            case ASPELL_SHOT_COLD:
                my_strcpy(buf, " dam 5/shot", len);
                break;
            case ASPELL_LIGHT_AREA:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case ASPELL_SHOT_FIRE:
                my_strcpy(buf, " dam 7/shot", len);
                break;
            case ASPELL_FARSIGHT:
                strnfmt(buf, len, " %+d", (plev - 7) / 10);
                break;
            case ASPELL_SHOT_ACID:
                my_strcpy(buf, " dam 9/shot", len);
                break;
            case ASPELL_SHOT_THUNDER:
                my_strcpy(buf, " dam 20", len);
                break;
            case ASPELL_SHOT_POISON:
                my_strcpy(buf, " dam 11/shot", len);
                break;
            case ASPELL_SHOT_SHARDS:
                my_strcpy(buf, " dam 10", len);
                break;
            case ASPELL_SHOT_ICE:
                my_strcpy(buf, " dam 30", len);
                break;
            case ASPELL_SHOT_FLAME:
                my_strcpy(buf, " dam 35", len);
                break;
            case ASPELL_SHOT_WATER:
                my_strcpy(buf, " dam 40", len);
                break;
            case ASPELL_SHOT_POWER:
                my_strcpy(buf, " dam 20/shot", len);
                break;
            case ASPELL_SHOT_SONIC:
                my_strcpy(buf, " dam 25", len);
                break;
        }
    }

    /* Telepath spells */
    if (tval == TV_PSI_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case TSPELL_MIND_BLAST:
                strnfmt(buf, len, " dam %dd%d", 3 + (plev - 1) / 4, 3 + plev / 15);
                break;
            case TSPELL_TELEPORT_BLINK:
                strnfmt(buf, len, " range %d", 7 + plev);
                break;
            case TSPELL_LIGHT_AREA:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case TSPELL_CELL_ADJUST:
                strnfmt(buf, len, " heal %dd6", plev / 2);
                break;
            case TSPELL_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", plev * 5);
                break;
            case TSPELL_KINETIC_BLAST:
                strnfmt(buf, len, " dam %d+d%d", 50 + plev, plev * 2);
                break;
            case TSPELL_PSI_DRAIN:
                strnfmt(buf, len, " dam %dd6", plev / 2);
                break;
            case TSPELL_MIND_WAVE:
                strnfmt(buf, len, " dam %d", 3 * plev / 2);
                break;
            case TSPELL_FORCE_SHIELD:
                my_strcpy(buf, " dur 30+d20", len);
                break;
            case TSPELL_ELEM_SHIELD:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case TSPELL_TIMED_ESP:
                strnfmt(buf, len, " dur %d+d10", plev);
                break;
            case TSPELL_SONIC_WAVE:
                strnfmt(buf, len, " dam %d", 3 * plev / 2);
                break;
            case TSPELL_SPACE_TIME:
                strnfmt(buf, len, " dur %d+d20", plev);
                break;
            case TSPELL_PROBA_TRAVEL:
                strnfmt(buf, len, " dur %d+d20", plev);
                break;
            case TSPELL_INERTIA_WAVE:
                strnfmt(buf, len, " dam %d", 3 * plev / 2);
                break;
            case TSPELL_TIME_BOLT:
                strnfmt(buf, len, " dam %dd%d", 10 + plev / 12, plev);
                break;
            case TSPELL_BIOFEEDBACK:
                strnfmt(buf, len, " dur 1d%d", plev + 10);
                break;
            case TSPELL_KINETIC_WAVE:
                strnfmt(buf, len, " dam %d", plev * 4);
                break;
            case TSPELL_EXTRA_HEALING:
                my_strcpy(buf, " heal 1000", len);
                break;
            case TSPELL_ADRENALINE:
                my_strcpy(buf, " dur 10+d10", len);
                break;
        }
    }

    /* Necromancer spells */
    if (tval == TV_DEATH_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case NSPELL_PHASE_DOOR:
                my_strcpy(buf, " range 10", len);
                break;
            case NSPELL_RESIST_COLD:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case NSPELL_TELEPORT_SELF:
                strnfmt(buf, len, " range %d", plev * 5);
                break;
            case NSPELL_FROST_BOLT:
                strnfmt(buf, len, " dam %dd8", 5 + (plev - 5) / 4);
                break;
            case NSPELL_SHADOW_TOUCH:
                strnfmt(buf, len, " dur %d+d%d", 20 + plev * 3 / 10, 10 + plev * 4 / 10);
                break;
            case NSPELL_ABSORB_SOUL:
                strnfmt(buf, len, " dur %d+d%d", 50 + plev, 30 + plev * 2);
                break;
            case NSPELL_FROST_BALL:
                strnfmt(buf, len, " dam %d", 30 + plev);
                break;
            case NSPELL_DISPEL_UNDEAD:
                strnfmt(buf, len, " dam 1d%d", 3 * plev);
                break;
            case NSPELL_HASTE_SELF:
                strnfmt(buf, len, " dur %d+d20", plev);
                break;
            case NSPELL_RESISTANCE:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case NSPELL_NETHER_BOLT:
                strnfmt(buf, len, " dam 11d%d", plev);
                break;
            case NSPELL_DRAIN_LIFE:
                my_strcpy(buf, " dam 100", len);
                break;
            case NSPELL_ICE_STORM:
                strnfmt(buf, len, " dam %d", 50 + plev * 2);
                break;
            case NSPELL_SHIELD:
                my_strcpy(buf, " dur 30+d20", len);
                break;
            case NSPELL_NETHER_BALL:
                strnfmt(buf, len, " dam %d", 100 + 2 * plev);
                break;
            case NSPELL_CLOUD_DRAINING:
                my_strcpy(buf, " dam 100", len);
                break;
            case NSPELL_DARKNESS_STORM:
                strnfmt(buf, len, " dam %d+10d10", plev * 5);
                break;
            case NSPELL_TOUCH_DEATH:
                my_strcpy(buf, " dur 12+d12", len);
                break;
        }
    }

    /* Elementalist spells */
    if (tval == TV_ELEM_BOOK)
    {
        int spell_power = p->spell_power[spell];

        /* Analyze the spell */
        switch (spell)
        {
            case ESPELL_SHOCKING_GRASP:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 5, spell_power * 5);
                break;
            case ESPELL_LIGHTNING_BOLT:
                strnfmt(buf, len, " dam %dd%d", 7 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_RESIST_ELEC:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_RESIST_POISON:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_LIGHTNING_BALL:
                strnfmt(buf, len, " dam %d", 25 + plev + spell_power * 10);
                break;
            case ESPELL_TOXIC_CLOUD:
                strnfmt(buf, len, " dam %d", plev * 2 + spell_power * 20 - 20);
                break;
            case ESPELL_SHOCK_WAVE:
                strnfmt(buf, len, " dam %d", 125 + plev * 2 + spell_power * 20);
                break;
            case ESPELL_THUNDERSTORM:
                strnfmt(buf, len, " dam %d+8x%dd%d", 225 + plev * 2 + spell_power * 20,
                    7 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_BURNING_HANDS:
                strnfmt(buf, len, " dam %dd%d", 4 + (plev - 1) / 4, 3 + spell_power);
                break;
            case ESPELL_FIRE_BOLT:
                strnfmt(buf, len, " dam %dd%d", 6 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_ILLUMINATION:
                strnfmt(buf, len, " range %d", (1 + spell_power) * 3 / 2);
                break;
            case ESPELL_RESIST_FIRE:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_FIRE_BALL:
                strnfmt(buf, len, " dam %d", 20 + plev + spell_power * 10);
                break;
            case ESPELL_FIREFLASH:
                strnfmt(buf, len, " dam %d", plev * 2 + spell_power * 20 - 10);
                break;
            case ESPELL_PLASMA_BALL:
                strnfmt(buf, len, " dam %d", 150 + plev * 2 + spell_power * 20);
                break;
            case ESPELL_INFERNO:
                strnfmt(buf, len, " dam %d+8x%dd%d", 250 + plev * 2 + spell_power * 20,
                    6 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_STONE_MUD:
                strnfmt(buf, len, " x%d", (1 + spell_power) / 2);
                break;
            case ESPELL_ACID_BOLT:
                strnfmt(buf, len, " dam %dd%d", 8 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_RESIST_ACID:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_WALL_CREATION:
                strnfmt(buf, len, " x%d", spell_power * 3 / 2);
                break;
            case ESPELL_ACID_BALL:
                strnfmt(buf, len, " dam %d", 30 + plev + spell_power * 10);
                break;
            case ESPELL_STONE_SKIN:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_EXPLOSION:
                strnfmt(buf, len, " dam %d", 10 + plev * 2 + spell_power * 20);
                break;
            case ESPELL_WALL_FORCE:
                strnfmt(buf, len, " dam %d", 200 + plev * 2 + spell_power * 20);
                break;
            case ESPELL_FREEZING_AURA:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 5, spell_power * 5);
                break;
            case ESPELL_FROST_BOLT:
                strnfmt(buf, len, " dam %dd%d", 5 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_RESIST_COLD:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_POTION_VITALITY:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 10, spell_power * 10);
                break;
            case ESPELL_FROST_BALL:
                strnfmt(buf, len, " dam %d", 15 + plev + spell_power * 10);
                break;
            case ESPELL_ICE_STORM:
                strnfmt(buf, len, " dam %d", plev * 2 + spell_power * 20);
                break;
            case ESPELL_TIDAL_WAVE:
                strnfmt(buf, len, " dam %d", 175 + plev * 2 + spell_power * 20);
                break;
            case ESPELL_MAELSTROM:
                strnfmt(buf, len, " dam %d+8x%dd%d", 275 + plev * 2 + spell_power * 20,
                    5 + (plev - 5) / 4, 6 + spell_power);
                break;
            case ESPELL_ELEM_POWER:
                strnfmt(buf, len, " power +%d%%", spell_power * 5);
                break;
            case ESPELL_ELEM_BOLT:
                strnfmt(buf, len, " dam 4x%dd%d", 4 + (plev - 5) / 4, 6 + spell_power / 2);
                break;
            case ESPELL_MINOR_HAVOC:
                strnfmt(buf, len, " radius %d", 5 + spell_power / 2);
                break;
            case ESPELL_RESISTANCE:
                strnfmt(buf, len, " dur %d+d%d", spell_power * 5, spell_power * 5);
                break;
            case ESPELL_ELEM_BALL:
                strnfmt(buf, len, " dam 4x%d", 10 + plev + spell_power * 5);
                break;
            case ESPELL_ELEM_BRAND:
                strnfmt(buf, len, " x%d", 1 + spell_power / 2);
                break;
            case ESPELL_MAJOR_HAVOC:
                strnfmt(buf, len, " radius %d", 10 + spell_power);
                break;
            case ESPELL_ELEM_STORM:
                strnfmt(buf, len, " dam %d+8x%dd%d", 200 + plev * 2 + spell_power * 10,
                    4 + (plev - 5) / 4, 6 + spell_power / 2);
                break;
        }
    }

    /* Summmoner spells */
    if (tval == TV_SUMMON_BOOK)
    {
        /* Analyze the spell */
        switch (spell)
        {
            case ZSPELL_BLINK:
                my_strcpy(buf, " range 10", len);
                break;
            case ZSPELL_CALL_LIGHT:
                strnfmt(buf, len, " dam 2d%d", plev / 2);
                break;
            case ZSPELL_CURE_LIGHT_WOUNDS:
                my_strcpy(buf, " heal 20", len);
                break;
            case ZSPELL_FARSIGHT:
                strnfmt(buf, len, " %+d", plev / 4);
                break;
            case ZSPELL_HEALING:
                strnfmt(buf, len, " heal %d", plev * 6);
                break;
            case ZSPELL_REGENERATION:
                my_strcpy(buf, " dur 20+d20", len);
                break;
            case ZSPELL_HARMONY:
                my_strcpy(buf, " dur 10+d10", len);
                break;
            case ZSPELL_THUNDERSTORM:
                strnfmt(buf, len, " dam %d", 200 + plev * 2);
                break;
            case ZSPELL_SUPPRESS_SUMMONING:
                strnfmt(buf, len, " dur %d+d20", plev);
                break;
        }
    }
}


void get_ghost_spell_info(struct player *p, int spell, char *buf, size_t len)
{
    int plev = p->lev;

    /* Blank 'buf' first */
    buf[0] = '\0';

    /* Analyze the spell */
    switch (spell)
    {
        case GSPELL_TELEPORT_BLINK:
            my_strcpy(buf, " range 10", len);
            break;
        case GSPELL_TELEPORT_SELF:
            strnfmt(buf, len, " range %d", plev * 8);
            break;
        case GSPELL_NETHER_BOLT:
            strnfmt(buf, len, " dam %d+5d5", 50 + plev);
            break;
        case GSPELL_DISEN_BOLT:
            strnfmt(buf, len, " dam %d+5d5", 75 + plev);
            break;
        case GSPELL_NETHER_BALL:
            strnfmt(buf, len, " dam %d", 100 + 2 * plev);
            break;
        case GSPELL_DARK_BALL:
            strnfmt(buf, len, " dam %d+10d10", plev * 5);
            break;
    }
}


void get_mimic_spell_info(struct player *p, int spell, char *buf, size_t len)
{
    int plev = p->lev;

    /* Blank 'buf' first */
    buf[0] = '\0';

    /* Analyze the spell */
    switch (spell)
    {
        case RSF_ARROW_X:
            my_strcpy(buf, " dam 9d6", len);
            break;
        case RSF_ARROW_1:
            my_strcpy(buf, " dam 1d6", len);
            break;
        case RSF_ARROW_2:
            my_strcpy(buf, " dam 3d6", len);
            break;
        case RSF_ARROW_3:
            my_strcpy(buf, " dam 5d6", len);
            break;
        case RSF_ARROW_4:
            my_strcpy(buf, " dam 7d6", len);
            break;
        case RSF_BOULDER:
            strnfmt(buf, len, " dam %dd12", 1 + plev / 7);
            break;
        case RSF_BA_ACID:
            strnfmt(buf, len, " dam 15+d%d", plev * 3);
            break;
        case RSF_BA_ELEC:
            strnfmt(buf, len, " dam 8+d%d", plev * 3 / 2);
            break;
        case RSF_BA_FIRE:
            strnfmt(buf, len, " dam 10+d%d", plev * 7 / 2);
            break;
        case RSF_BA_COLD:
            strnfmt(buf, len, " dam 10+d%d", plev * 3 / 2);
            break;
        case RSF_BA_POIS:
            my_strcpy(buf, " dam 12d2", len);
            break;
        case RSF_BA_NETH:
            strnfmt(buf, len, " dam %d+10d10", 50 + plev);
            break;
        case RSF_BA_WATE:
            strnfmt(buf, len, " dam 50+d%d", plev * 5 / 2);
            break;
        case RSF_BA_MANA:
        case RSF_BA_DARK:
            strnfmt(buf, len, " dam %d+10d10", plev * 5);
            break;
        case RSF_DRAIN_MANA:
            strnfmt(buf, len, " heal 6+3d%d", plev);
            break;
        case RSF_MIND_BLAST:
        case RSF_CAUSE_2:
            my_strcpy(buf, " dam 8d8", len);
            break;
        case RSF_BRAIN_SMASH:
            my_strcpy(buf, " dam 12d15", len);
            break;
        case RSF_CAUSE_1:
            my_strcpy(buf, " dam 3d8", len);
            break;
        case RSF_CAUSE_3:
            my_strcpy(buf, " dam 10d15", len);
            break;
        case RSF_CAUSE_4:
            my_strcpy(buf, " dam 15d15", len);
            break;
        case RSF_BO_ACID:
            strnfmt(buf, len, " dam %d+7d8", plev / 3);
            break;
        case RSF_BO_ELEC:
            strnfmt(buf, len, " dam %d+4d8", plev / 3);
            break;
        case RSF_BO_FIRE:
            strnfmt(buf, len, " dam %d+9d8", plev / 3);
            break;
        case RSF_BO_COLD:
            strnfmt(buf, len, " dam %d+6d8", plev / 3);
            break;
        case RSF_BO_NETH:
            strnfmt(buf, len, " dam %d+5d5", 30 + plev * 3 / 2);
            break;
        case RSF_BO_WATE:
            strnfmt(buf, len, " dam %d+10d10", plev);
            break;
        case RSF_BO_MANA:
            strnfmt(buf, len, " dam 50+d%d", plev * 7 / 2);
            break;
        case RSF_BO_PLAS:
            strnfmt(buf, len, " dam %d+8d7", 10 + plev);
            break;
        case RSF_BO_ICEE:
            strnfmt(buf, len, " dam %d+6d6", plev);
            break;
        case RSF_MISSILE:
            strnfmt(buf, len, " dam %d+2d6", plev / 3);
            break;
        case RSF_HASTE:
            strnfmt(buf, len, " dur %d+d20", plev);
            break;
        case RSF_HEAL:
            strnfmt(buf, len, " heal %d", plev * 6);
            break;
        case RSF_BLINK:
            my_strcpy(buf, " range 10", len);
            break;
        case RSF_TPORT:
            strnfmt(buf, len, " range %d", MAX_SIGHT_LGE * 2 + 5);
            break;
    }
}


static int beam_chance(player_type* p_ptr)
{
    int plev = p_ptr->lev;
    return (player_has(p_ptr, PF_BEAM)? plev: plev / 2);
}


static void set_bow_brand(struct player *p, byte t, int dam)
{
    byte old_bow_brand_t = p->bow_brand_t;
    int plev = p->lev;

    /* Set brand type and damage */
    p->bow_brand_t = t;
    p->bow_brand_d = dam;

    /* Branding of the same type stacks */
    if (has_bowbrand(p, old_bow_brand_t)) player_inc_timed(p, TMD_BOWBRAND, plev, TRUE, TRUE);

    /* Apply new branding */
    else
    {
        /* Force the message display */
        p->timed[TMD_BOWBRAND] = 0;
        player_set_timed(p, TMD_BOWBRAND, plev + randint1(20), TRUE);
    }
}


static bool cast_mage_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;
    int beam = beam_chance(p);

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_MAGE_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case SPELL_MAGIC_MISSILE:
        {
            msg_spell(p, " fires a magic missile.");
            fire_bolt_or_beam(p, beam - 10, GF_MISSILE, dir, damroll(3 + (plev - 1) / 5, 4));
            break;
        }
        case SPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case SPELL_PHASE_DOOR:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case SPELL_LIGHT_AREA:
        {
            msg_spell(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case SPELL_CURE_LIGHT_WOUNDS:
        {
            bool dummy;

            effect_light(p, &dummy);
            break;
        }
        case SPELL_FIND_TRAPS_DOORS:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case SPELL_STINKING_CLOUD:
        {
            msg_spell(p, " casts a stinking cloud.");
            fire_ball(p, GF_POIS, dir, 10 + plev / 2, 2);
            break;
        }
        case SPELL_CONFUSE_MONSTER:
        {
            msg_spell(p, " makes a complicated gesture.");
            confuse_monster(p, dir, plev, TRUE);
            break;
        }
        case SPELL_LIGHTNING_BOLT:
        {
            msg_spell(p, " casts a lightning bolt.");
            fire_beam(p, GF_ELEC, dir, damroll(3 + (plev - 5) / 6, 6));
            break;
        }
        case SPELL_TRAP_DOOR_DESTRUCTION:
        {
            msg_spell(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case SPELL_CURE_POISON:
        {
            player_clear_timed(p, TMD_POISONED, TRUE);
            break;
        }
        case SPELL_SLEEP_MONSTER:
        {
            msg_spell(p, " gestures and mumbles calmly.");
            sleep_monster(p, dir, TRUE);
            break;
        }
        case SPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 5);
            break;
        }
        case SPELL_SPEAR_OF_LIGHT:
        {
            msg(p, "A line of blue shimmering light appears.");
            msg_spell(p, "'s hands project a line of blue shimmering light.");
            light_line(p, dir);
            break;
        }
        case SPELL_FROST_BOLT:
        {
            msg_spell(p, " casts a frost bolt.");
            fire_bolt_or_beam(p, beam - 10, GF_COLD, dir, damroll(5 + (plev - 5) / 4, 8));
            break;
        }
        case SPELL_WONDER:
        {
            effect_wonder(p, dir, randint1(100) + plev / 5, beam);
            break;
        }
        case SPELL_SATISFY_HUNGER:
        {
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        case SPELL_RECHARGE_ITEM_I:
        {
            return recharge(p, 2 + plev / 5);
        }
        case SPELL_TURN_STONE_TO_MUD:
        {
            msg_spell(p, " makes a moving gesture.");
            wall_to_mud(p, dir);
            break;
        }
        case SPELL_FIRE_BOLT:
        {
            msg_spell(p, " casts a fire bolt.");
            fire_bolt_or_beam(p, beam, GF_FIRE, dir, damroll(6 + (plev - 5) / 4, 8));
            break;
        }
        case SPELL_POLYMORPH_OTHER:
        {
            msg_spell(p, " discharges an everchanging blast of energy.");
            poly_monster(p, dir, TRUE);
            break;
        }
        case SPELL_IDENTIFY:
        {
            return ident_spell(p);
        }
        case SPELL_DETECT_INVISIBLE:
        {
            reveal_monsters(p, TRUE);
            break;
        }
        case SPELL_ACID_BOLT:
        {
            msg_spell(p, " casts an acid bolt.");
            fire_bolt_or_beam(p, beam, GF_ACID, dir, damroll(8 + (plev - 5) / 4, 8));
            break;
        }
        case SPELL_SLOW_MONSTER:
        {
            msg_spell(p, " makes a lengthy gesture.");
            slow_monster(p, dir, TRUE);
            break;
        }
        case SPELL_FROST_BALL:
        {
            msg_spell(p, " casts a frost ball.");
            fire_ball(p, GF_COLD, dir, 30 + plev, 2);
            break;
        }
        case SPELL_TELEPORT_OTHER:
        {
            msg_spell(p, " makes a rush gesture.");
            teleport_monster(p, dir);
            break;
        }
        case SPELL_HASTE_SELF:
        {
            player_inc_timed_nostack(p, TMD_FAST, randint1(20) + plev, randint1(5), TRUE);
            break;
        }
        case SPELL_MASS_SLEEP:
        {
            sleep_monsters(p, TRUE);
            break;
        }
        case SPELL_FIRE_BALL:
        {
            msg_spell(p, " casts a fire ball.");
            fire_ball(p, GF_FIRE, dir, 55 + plev, 2);
            break;
        }
        case SPELL_TREASURE_DETECTION:
        {
            detect_treasure(p, TRUE, FALSE);
            break;
        }
        case SPELL_RESIST_COLD:
        {
            player_inc_timed(p, TMD_OPP_COLD, randint1(20) + 20, TRUE, TRUE);
            break;
        }
        case SPELL_RESIST_FIRE:
        {
            player_inc_timed(p, TMD_OPP_FIRE, randint1(20) + 20, TRUE, TRUE);
            break;
        }
        case SPELL_RESIST_POISON:
        {
            player_inc_timed(p, TMD_OPP_POIS, randint1(20) + 20, TRUE, TRUE);
            break;
        }
        case SPELL_RESISTANCE:
        {
            int time = randint1(20) + 20;

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case SPELL_SHIELD:
        {
            player_inc_timed(p, TMD_SHIELD, randint1(20) + 30, TRUE, TRUE);
            break;
        }
        case SPELL_SHOCK_WAVE:
        {
            msg_spell(p, " casts a sonic wave.");
            fire_ball(p, GF_SOUND, dir, 10 + plev, 2);
            break;
        }
        case SPELL_EXPLOSION:
        {
            msg_spell(p, " fires an exploding missile.");
            fire_ball(p, GF_SHARD, dir, 20 + plev * 2, 2);
            break;
        }
        case SPELL_CLOUD_KILL:
        {
            msg_spell(p, " casts a cloud of death.");
            fire_ball(p, GF_POIS, dir, 40 + plev / 2, 3);
            break;
        }
        case SPELL_ACID_BALL:
        {
            msg_spell(p, " casts an acid ball.");
            fire_ball(p, GF_ACID, dir, 40 + plev, 2);
            break;
        }
        case SPELL_ICE_STORM:
        {
            msg_spell(p, " invokes an ice storm.");
            fire_ball(p, GF_ICE, dir, 50 + plev * 2, 3);
            break;
        }
        case SPELL_METEOR_SWARM:
        {
            msg_spell(p, " casts a swarm of meteors.");
            fire_swarm(p, 2 + plev / 20, GF_METEOR, dir, 30 + plev / 2, 1);
            break;
        }
        case SPELL_RIFT:
        {
            msg_spell(p, " fires a gravity beam.");
            fire_beam(p, GF_GRAVITY, dir, 40 + damroll(plev, 7));
            break;
        }
        case SPELL_DOOR_CREATION:
        {
            door_creation(p);
            break;
        }
        case SPELL_STAIR_CREATION:
        {
            stair_creation(p);
            break;
        }
        case SPELL_TELEPORT_LEVEL:
        {
            teleport_player_level(p);
            break;
        }
        case SPELL_WORD_OF_RECALL:
        {
            set_recall(p, note, FALSE);
            break;
        }
        case SPELL_RUNE_OF_PROTECTION:
        {
            warding_glyph(p);
            break;
        }
        case SPELL_HEROISM:
        {
            bool dummy;

            effect_hero(p, randint1(25) + 25, &dummy);
            break;
        }
        case SPELL_BERSERKER:
        {
            bool dummy;

            effect_berserk(p, randint1(25) + 25, &dummy);
            break;
        }
        case SPELL_ENCHANT_ARMOR:
        {
            return enchant_spell(p, 0, 0, randint0(3) + plev / 20);
        }
        case SPELL_ENCHANT_WEAPON:
        {
            return enchant_spell(p, randint0(4) + plev / 20, randint0(4) + plev / 20, 0);
        }
        case SPELL_RECHARGE_ITEM_II:
        {
            return recharge(p, 50 + plev);
        }
        case SPELL_ELEMENTAL_BRAND:
        {
            return brand_ammo(p);
        }
        case SPELL_SHIELD_GOI:
        {
            player_inc_timed(p, TMD_INVULN, 8 + randint1(10), TRUE, TRUE);
            break;
        }
        case SPELL_EARTHQUAKE:
        {
            earthquake(p, p->depth, p->py, p->px, 10);
            break;
        }
        case SPELL_BEDLAM:
        {
            msg_spell(p, " casts a confusion ball.");
            fire_ball_hack(p, GF_OLD_CONF, dir, plev, 4);
            break;
        }
        case SPELL_REND_SOUL:
        {
            msg_spell(p, " casts a nether bolt.");
            fire_bolt_or_beam(p, beam / 4, GF_NETHER, dir, damroll(11, plev));
            break;
        }
        case SPELL_BANISHMENT:
        {
            return banishment(p);
        }
        case SPELL_WORD_OF_DESTRUCTION:
        {
            destroy_area(p, p->depth, p->py, p->px, 15, TRUE);
            break;
        }
        case SPELL_MASS_BANISHMENT:
        {
            mass_banishment(p);
            break;
        }
        case SPELL_CHAOS_STRIKE:
        {
            msg_spell(p, " casts a chaos bolt.");
            fire_bolt_or_beam(p, beam, GF_CHAOS, dir, damroll(13, plev));
            break;
        }
        case SPELL_MANA_STORM:
        {
            msg_spell(p, " invokes a mana storm.");
            fire_ball(p, GF_MANA, dir, 300 + plev * 2, 3);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static void alter_reality(struct player *p)
{
    int i;

    /* Only on random levels */
    if (!random_level(p->depth)) return;

    /* Search for players on this depth */
    for (i = 1; i < NumPlayers + 1; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Only players on this depth */
        if (q_ptr->depth != p->depth) continue;

        /* Tell the player about it */
        msg(q_ptr, "The world changes!");

        /* Generate a new level (later) */
        q_ptr->new_level_flag = TRUE;
        q_ptr->new_level_method = LEVEL_RAND;
    }

    /* Deallocate the level */
    dealloc_dungeon_level(p->depth);
}


static bool cast_priest_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_PRIEST_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Prayers */
    switch (spell)
    {
        case PRAYER_DETECT_EVIL:
        {
            detect_monsters_evil(p, TRUE);
            break;
        }
        case PRAYER_CURE_LIGHT_WOUNDS:
        {
            bool dummy;

            effect_light(p, &dummy);
            break;
        }
        case PRAYER_BLESS:
        {
            player_inc_timed(p, TMD_BLESSED, randint1(12) + 12, TRUE, TRUE);
            break;
        }
        case PRAYER_REMOVE_FEAR:
        {
            player_clear_timed(p, TMD_AFRAID, TRUE);
            break;
        }
        case PRAYER_CALL_LIGHT:
        {
            msg_prayer(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case PRAYER_FIND_TRAPS_DOORS:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case PRAYER_SLOW_POISON:
        {
            player_set_timed(p, TMD_POISONED, p->timed[TMD_POISONED] / 2, TRUE);
            break;
        }
        case PRAYER_SCARE_MONSTER:
        {
            fear_monster(p, dir, plev, TRUE);
            break;
        }
        case PRAYER_PORTAL:
        {
            msg_prayer(p, " blinks away!");
            teleport_player(p, plev * 3);
            break;
        }
        case PRAYER_CURE_SERIOUS_WOUNDS:
        {
            bool dummy;

            effect_serious(p, &dummy);
            break;
        }
        case PRAYER_CHANT:
        {
            player_inc_timed(p, TMD_BLESSED, randint1(24) + 24, TRUE, TRUE);
            break;
        }
        case PRAYER_SANCTUARY:
        {
            msg_prayer(p, " is briefly enclosed by a deep blue aura.");
            sleep_monsters_touch(p, TRUE);
            break;
        }
        case PRAYER_SATISFY_HUNGER:
        {
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        /*case PRAYER_REMOVE_CURSE:
        {
            remove_curse(p);
            break;
        }*/
        case PRAYER_RESIST_HEAT_COLD:
        {
            player_inc_timed(p, TMD_OPP_FIRE, randint1(10) + 10, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, randint1(10) + 10, TRUE, TRUE);
            break;
        }
        case PRAYER_NEUTRALIZE_POISON:
        {
            player_clear_timed(p, TMD_POISONED, TRUE);
            break;
        }
        case PRAYER_ORB_OF_DRAINING:
        {
            msg_prayer(p, " fires a holy orb!");
            fire_ball(p, GF_HOLY_ORB, dir,
                damroll(3, 6) + plev + plev / (player_has(p, PF_ZERO_FAIL)? 2: 4),
                ((plev < 30)? 2: 3));
            break;
        }
        case PRAYER_CURE_CRITICAL_WOUNDS:
        {
            bool dummy;

            effect_critical(p, &dummy);
            break;
        }
        case PRAYER_SENSE_INVISIBLE:
        {
            player_inc_timed(p, TMD_SINVIS, randint1(24) + 24, TRUE, TRUE);
            break;
        }
        case PRAYER_PROTECTION_FROM_EVIL:
        {
            player_inc_timed(p, TMD_PROTEVIL, randint1(25) + 3 * p->lev, TRUE, TRUE);
            break;
        }
        case PRAYER_EARTHQUAKE:
        {
            earthquake(p, p->depth, p->py, p->px, 10);
            break;
        }
        case PRAYER_SENSE_SURROUNDINGS:
        {
            map_area(p);
            break;
        }
        case PRAYER_CURE_MORTAL_WOUNDS:
        {
            bool dummy;

            effect_mortal(p, &dummy);
            break;
        }
        case PRAYER_TURN_UNDEAD:
        {
            msg_prayer(p, " tries to turn undead.");
            turn_undead(p);
            break;
        }
        case PRAYER_PRAYER:
        {
            player_inc_timed(p, TMD_BLESSED, randint1(48) + 48, TRUE, TRUE);
            break;
        }
        case PRAYER_DISPEL_UNDEAD:
        {
            msg_prayer(p, " dispels undead.");
            dispel_undead(p, randint1(plev * 3), TRUE);
            break;
        }
        case PRAYER_HEAL:
        {
            bool dummy;

            effect_heal(p, &dummy);
            break;
        }
        case PRAYER_DISPEL_EVIL:
        {
            msg_prayer(p, " dispels evil.");
            dispel_evil(p, randint1(plev * 3), TRUE);
            break;
        }
        case PRAYER_GLYPH_OF_WARDING:
        {
            warding_glyph(p);
            break;
        }
        case PRAYER_HOLY_WORD:
        {
            msg_prayer(p, " shouts the holy word.");
            dispel_evil(p, randint1(plev * 4), TRUE);
            hp_player(p, 1000);
            player_clear_timed(p, TMD_AFRAID, TRUE);
            player_clear_timed(p, TMD_POISONED, TRUE);
            player_clear_timed(p, TMD_STUN, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            player_clear_timed(p, TMD_AMNESIA, TRUE);
            break;
        }
        case PRAYER_BLINK:
        {
            msg_prayer(p, " blinks away!");
            teleport_player(p, 10);
            break;
        }
        case PRAYER_TELEPORT_SELF:
        {
            msg_prayer(p, " teleports away!");
            teleport_player(p, plev * 8);
            break;
        }
        case PRAYER_TELEPORT_OTHER:
        {
            msg_prayer(p, " prays for divine intervention.");
            teleport_monster(p, dir);
            break;
        }
        case PRAYER_TELEPORT_LEVEL:
        {
            teleport_player_level(p);
            break;
        }
        case PRAYER_WORD_OF_RECALL:
        {
            set_recall(p, note, FALSE);
            break;
        }
        case PRAYER_ALTER_REALITY:
        {
            alter_reality(p);
            break;
        }
        case PRAYER_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case PRAYER_DETECTION:
        {
            detect_all(p, TRUE);
            break;
        }
        case PRAYER_PERCEPTION:
        {
            return ident_spell(p);
        }
        case PRAYER_PROBING:
        {
            probing(p);
            break;
        }
        case PRAYER_CLAIRVOYANCE:
        {
            wiz_light(p, FALSE);
            break;
        }
        case PRAYER_CURE_WOUNDS:
        {
            hp_player(p, (3 + plev / 10) * 10);
            player_clear_timed(p, TMD_CUT, TRUE);
            break;
        }
        case PRAYER_HEALING:
        {
            hp_player(p, 100);
            player_clear_timed(p, TMD_STUN, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            player_clear_timed(p, TMD_AMNESIA, TRUE);
            break;
        }
        case PRAYER_EXTRA_HEALING:
        {
            hp_player(p, 2000);
            player_clear_timed(p, TMD_STUN, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            player_clear_timed(p, TMD_AMNESIA, TRUE);
            break;
        }
        case PRAYER_RESTORATION:
        {
            do_res_stat(p, A_STR);
            do_res_stat(p, A_INT);
            do_res_stat(p, A_WIS);
            do_res_stat(p, A_DEX);
            do_res_stat(p, A_CON);
            do_res_stat(p, A_CHR);
            break;
        }
        case PRAYER_REMEMBRANCE:
        {
            restore_level(p);
            break;
        }
        case PRAYER_RESURRECTION:
        {
            do_scroll_life(p);
            break;
        }
        case PRAYER_UNBARRING_WAYS:
        {
            msg_prayer(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case PRAYER_RECHARGING:
        {
            return recharge(p, 15);
        }
        /*case PRAYER_DISPEL_CURSE:
        {
            remove_all_curse(p);
            break;
        }*/
        case PRAYER_ENCHANT_WEAPON:
        {
            return enchant_spell(p, randint1(4), randint1(4), 0);
        }
        case PRAYER_ENCHANT_ARMOUR:
        {
            return enchant_spell(p, 0, 0, 1 + randint1(3));
        }
        case PRAYER_ELEMENTAL_BRAND:
        {
            brand_weapon(p, TRUE);
            break;
        }
        case PRAYER_DISPEL_UNDEAD2:
        {
            msg_prayer(p, " dispels undead.");
            dispel_undead(p, randint1(plev * 4), TRUE);
            break;
        }
        case PRAYER_DISPEL_EVIL2:
        {
            msg_prayer(p, " dispels evil.");
            dispel_evil(p, randint1(plev * 4), TRUE);
            break;
        }
        case PRAYER_BANISH_EVIL:
        {
            if (banish_evil(p, 100))
            {
                msg(p, "The power of your god banishes evil!");
                msg_prayer(p, " speaks a holy curse on nearby evil!");
            }
            break;
        }
        case PRAYER_WORD_OF_DESTRUCTION:
        {
            destroy_area(p, p->depth, p->py, p->px, 15, TRUE);
            break;
        }
        case PRAYER_ANNIHILATION:
        {
            msg_prayer(p, " fires a massive bolt filled with pure energy!");
            drain_life(p, dir, 200);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_sorc_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;
    int beam = beam_chance(p);

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_SORC_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case SSPELL_MAGIC_MISSILE:
        {
            msg_spell(p, " fires a magic missile.");
            fire_bolt_or_beam(p, beam - 10, GF_MISSILE, dir,
                damroll(3 + (plev - 1) / 5, 4 + plev / 10));
            break;
        }
        case SSPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case SSPELL_PHASE_DOOR:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case SSPELL_LIGHT_AREA:
        {
            msg_spell(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case SSPELL_FIND_TRAPS_DOORS:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case SSPELL_NOXIOUS_CLOUD:
        {
            msg_spell(p, " casts a stinking cloud.");
            fire_ball(p, GF_POIS, dir, 10 + plev / 2, 2);
            break;
        }
        case SSPELL_DETECT_TREASURE:
        {
            detect_treasure(p, TRUE, FALSE);
            break;
        }
        case SSPELL_CONFUSE_MONSTER:
        {
            msg_spell(p, " makes a complicated gesture.");
            confuse_monster(p, dir, plev, TRUE);
            break;
        }
        case SSPELL_LIGHTNING_BOLT:
        {
            msg_spell(p, " casts a lightning bolt.");
            fire_beam(p, GF_ELEC, dir, damroll(4 + (plev - 5) / 6, 6));
            break;
        }
        case SSPELL_TRAP_DOOR_DESTR:
        {
            msg_spell(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case SSPELL_SLEEP_MONSTER:
        {
            msg_spell(p, " gestures and mumbles calmly.");
            sleep_monster(p, dir, TRUE);
            break;
        }
        case SSPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 5);
            break;
        }
        case SSPELL_BEAM_LIGHT:
        {
            msg(p, "A line of blue shimmering light appears.");
            msg_spell(p, "'s hands project a line of blue shimmering light.");
            light_line(p, dir);
            break;
        }
        case SSPELL_FROST_BOLT:
        {
            msg_spell(p, " casts a frost bolt.");
            fire_bolt_or_beam(p, beam - 10, GF_COLD, dir, damroll(6 + (plev - 5) / 4, 8));
            break;
        }
        case SSPELL_CREATE_FOOD:
        {
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        case SSPELL_STONE_MUD:
        {
            msg_spell(p, " makes a moving gesture.");
            wall_to_mud(p, dir);
            break;
        }
        case SSPELL_FIRE_BOLT:
        {
            msg_spell(p, " casts a fire bolt.");
            fire_bolt_or_beam(p, beam, GF_FIRE, dir, damroll(9 + (plev - 5) / 4, 8));
            break;
        }
        case SSPELL_POLY_OTHER:
        {
            msg_spell(p, " discharges an everchanging blast of energy.");
            poly_monster(p, dir, TRUE);
            break;
        }
        case SSPELL_IDENTIFY_ITEM:
        {
            return ident_spell(p);
        }
        case SSPELL_ETHEREAL_EYE:
        {
            map_area(p);
            break;
        }
        case SSPELL_RADIATE_FEAR:
        {
            msg_spell(p, " emits a blast of terror.");
            fire_ball_hack(p, GF_TURN_ALL, 0, plev, 5);
            break;
        }
        case SSPELL_DETECT_INVIS:
        {
            reveal_monsters(p, TRUE);
            break;
        }
        case SSPELL_ELEMENTAL_BOLT:
        {
            msg_spell(p, " casts an elemental bolt.");
            fire_bolt_or_beam(p, beam / 2, GF_ELEC, dir, damroll(5 + (plev - 5) / 4, 8));
            fire_bolt_or_beam(p, beam / 2, GF_COLD, dir, damroll(5 + (plev - 5) / 4, 8));
            fire_bolt_or_beam(p, beam / 2, GF_FIRE, dir, damroll(5 + (plev - 5) / 4, 8));
            fire_bolt_or_beam(p, beam / 2, GF_ACID, dir, damroll(5 + (plev - 5) / 4, 8));
            break;
        }
        case SSPELL_MEDITATE_MANA:
        {
            player_inc_timed(p, TMD_MEDITATE, 20 + randint1(20), TRUE, TRUE);
            break;
        }
        case SSPELL_TELEPORT_AWAY:
        {
            msg_spell(p, " makes a rush gesture.");
            teleport_monster(p, dir);
            break;
        }
        case SSPELL_RECHARGE_ITEM:
        {
            return recharge(p, 40);
        }
        case SSPELL_HASTE_SELF:
        {
            player_inc_timed_nostack(p, TMD_FAST, randint1(20 + plev) + plev, randint1(5), TRUE);
            break;
        }
        case SSPELL_FIRE_STORM:
        {
            msg_spell(p, " casts a fire ball.");
            fire_ball(p, GF_FIRE, dir, 110 + plev * 2, 2);
            break;
        }
        case SSPELL_SUN_FIRE:
        {
            msg_spell(p, " invokes the sunfire.");
            fire_ball(p, GF_LIGHT, dir, 130 + plev * 2, 2);
            break;
        }
        case SSPELL_SAFE_GUARD:
        {
            sea_runes(p);
            break;
        }
        case SSPELL_CREATE_WALLS:
        {
            wall_creation(p, 0);
            break;
        }
        case SSPELL_CREATE_STAIRS:
        {
            stair_creation(p);
            break;
        }
        case SSPELL_ELEM_SHIELD:
        {
            int time = randint1(20) + 20;

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case SSPELL_WORD_RECALL:
        {
            set_recall(p, note, FALSE);
            break;
        }
        case SSPELL_FORCE_SHIELD:
        {
            player_inc_timed(p, TMD_SHIELD, randint1(20) + 30, TRUE, TRUE);
            break;
        }
        case SSPELL_CHAOS_BLAST:
        {
            msg_spell(p, " emits a chaos blast.");
            fire_ball(p, GF_CHAOS, 0, 100 + plev * 2, 3);
            break;
        }
        case SSPELL_WRAITH_FORM:
        {
            player_inc_timed(p, TMD_WRAITH, 10 + plev + randint1(20), TRUE, TRUE);
            break;
        }
        case SSPELL_MANA_SHIELD:
        {
            player_inc_timed(p, TMD_MANASHIELD, randint1(10) + 8, TRUE, TRUE);
            break;
        }
        case SSPELL_PLASMA_BLAST:
        {
            msg_spell(p, " emits a plasma blast.");
            fire_ball(p, GF_PLASMA, 0, 250 + plev * 2, 3);
            break;
        }
        case SSPELL_MANA_BLAST:
        {
            msg_spell(p, " emits a mana blast.");
            fire_ball(p, GF_MANA, 0, 350 + plev * 2, 3);
            break;
        }
        case SSPELL_MIND_VISION:
        {
            mind_vision(p, note);
            break;
        }
        case SSPELL_TELE_OBJECT:
        {
            return telekinesis(p, note);
        }
        case SSPELL_IDENTIFY_FULL:
        {
            identify_pack(p);
            break;
        }
        case SSPELL_TIMED_ESP:
        {
            player_inc_timed(p, TMD_ESP, randint1(10) + plev, TRUE, TRUE);
            break;
        }
        case SSPELL_ENLIGHT_LEVEL:
        {
            wiz_light(p, FALSE);
            break;
        }
        case SSPELL_EARTH_QUAKE:
        {
            earthquake(p, p->depth, p->py, p->px, 10);
            break;
        }
        case SSPELL_WIPE_AREA:
        {
            if (wipe_spell(p->depth, p->py, p->px, 10))
                msg_spell(p, " wrecks havoc.");
            break;
        }
        case SSPELL_WORD_DESTRUCTION:
        {
            destroy_area(p, p->depth, p->py, p->px, 15, TRUE);
            break;
        }
        case SSPELL_OBLIVION_BLAST:
        {
            mass_banishment(p);
            break;
        }
        case SSPELL_ANNIHIL_BOLT:
        {
            msg_spell(p, " fires an annihilation bolt.");
            fire_bolt(p, GF_MISSILE, dir, 400 + plev * 2);
            break;
        }
        case SSPELL_MANA_STRIKE:
        {
            msg_spell(p, " invokes a mana storm.");
            fire_ball(p, GF_MANA, dir, 350 + plev * 2, 3);
            break;
        }
        case SSPELL_TIDAL_WAVE:
        {
            msg_spell(p, " invokes a tidal wave.");
            fire_ball(p, GF_WATER, dir, 450 + plev * 2, 3);
            break;
        }
        case SSPELL_ANARCHY_FORCE:
        {
            int y;

            msg_spell(p, " invokes a chaos storm.");
            for (y = 0; y < 8; y++)
                fire_ball(p, GF_CHAOS, ddd[y], 100 + plev * 2, 3);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_shad_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_SHAD_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case RSPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case RSPELL_TELEPORT_BLINK:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case RSPELL_DETECT_FEATURES:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case RSPELL_LIGHT_AREA:
        {
            msg_spell(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case RSPELL_CREATE_POISON:
        {
            return create_poison(p);
        }
        /*case RSPELL_REMOVE_CURSE:
        {
            remove_curse(p);
            break;
        }*/
        case RSPELL_OBJECT_DETECTION:
        {
            detect_treasure(p, TRUE, TRUE);
            break;
        }
        case RSPELL_DESTROY_FEATURES:
        {
            msg_spell(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case RSPELL_CURE_POISON:
        {
            player_clear_timed(p, TMD_POISONED, TRUE);
            break;
        }
        case RSPELL_IDENTIFY_ITEM:
        {
            return ident_spell(p);
        }
        case RSPELL_DETECT_INVIS:
        {
            reveal_monsters(p, TRUE);
            break;
        }
        case RSPELL_STINKING_CLOUD:
        {
            msg_spell(p, " casts a stinking cloud.");
            fire_ball(p, GF_POIS, dir, 10 + plev / 2, 2);
            break;
        }
        case RSPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 5);
            break;
        }
        case RSPELL_SPEAR_LIGHT:
        {
            msg(p, "A line of blue shimmering light appears.");
            msg_spell(p, "'s hands project a line of blue shimmering light.");
            light_line(p, dir);
            break;
        }
        case RSPELL_STONE_MUD:
        {
            msg_spell(p, " makes a moving gesture.");
            wall_to_mud(p, dir);
            break;
        }
        case RSPELL_SAT_HUNGER:
        {
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        case RSPELL_SLEEP_MONSTERS:
        {
            sleep_monsters(p, TRUE);
            break;
        }
        case RSPELL_DEEP_NIGHTS:
        {
            deep_nights(p);
            break;
        }
        case RSPELL_CLOAK_CHANGT:
        {
            int what;
            int i, tries = 200;

            while (--tries)
            {
                player_type *q_ptr;

                /* 1 < i < NumPlayers */
                i = randint1(NumPlayers);

                q_ptr = player_get(i);

                /* Disguising into a rogue is .. mhh ... stupid */
                if (q_ptr->clazz->cidx == p->clazz->cidx) continue;

                /* Ok we found a good class lets mimic */
                what = q_ptr->clazz->cidx;
                break;
            }

            /* Arg nothing .. bah be a warrior */
            if (!tries) what = 0;

            p->tim_mimic_what = what;
            player_set_timed(p, TMD_MIMIC, 20 + randint1(20) + plev, TRUE);
            break;
        }
        case RSPELL_CONF_BALL:
        {
            msg_spell(p, " casts a confusion ball.");
            fire_ball_hack(p, GF_OLD_CONF, dir, plev, 4);
            break;
        }
        case RSPELL_CREATE_TRAPS:
        {
            trap_creation(p, FALSE);
            break;
        }
        case RSPELL_SCARE_MONSTERS:
        {
            fear_monsters(p);
            break;
        }
        case RSPELL_STUN_MONSTERS:
        {
            stun_monsters(p);
            break;
        }
        case RSPELL_SUMMON_MONSTERS:
        {
            msg_spell(p, " summons some monsters.");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_MONSTERS, p->lev, 8, 5);
            break;
        }
        case RSPELL_CREATE_DOORS:
        {
            door_creation(p);
            break;
        }
        case RSPELL_INNER_SIGHT:
        {
            map_area(p);
            break;
        }
        case RSPELL_FADE_SHADOWS:
        {
            player_inc_timed_nostack(p, TMD_INVIS, 20 + randint1(20) + plev, randint1(5), TRUE);
            break;
        }
        case RSPELL_CREATE_STAIRS:
        {
            stair_creation(p);
            break;
        }
        case RSPELL_TELEPORT_LEVEL:
        {
            teleport_player_level(p);
            break;
        }
        case RSPELL_TELEPORT_TARGET:
        {
            int rad = 10 + plev / 5;
            s16b ty, tx;

            /* Hack -- Use an actual "target" */
            if ((dir == 5) && target_okay(p))
            {
                target_get(p, &tx, &ty);

                if (distance(ty, tx, p->py, p->px) > rad)
                {
                    msg(p, "You cannot blink that far.");
                    return FALSE;
                }
            }
            else
            {
                msg(p, "You must have a target.");
                return FALSE;
            }

            teleport_player_to(p, ty, tx);
            break;
        }
        case RSPELL_TELEPORT_OTHER:
        {
            msg_spell(p, " makes a rush gesture.");
            teleport_monster(p, dir);
            break;
        }
        case RSPELL_FAST_RECALL:
        {
            set_recall(p, note, TRUE);
            break;
        }
        case RSPELL_DISARM_RAY:
        {
            disarm_trap(p, dir);
            break;
        }
        case RSPELL_IDENTIFY_FLOOR:
        {
            fire_ball(p, GF_IDENTIFY, 0, 1, 2 + plev / 40);
            break;
        }
        case RSPELL_RECHARGE_ITEM:
        {
            return recharge(p, 30 + plev / 2);
        }
        case RSPELL_ENCHANT_ARMOR:
        {
            return enchant_spell(p, 0, 0, randint0(3) + plev / 20);
        }
        /*case RSPELL_BANISH_CURSE:
        {
            remove_all_curse(p);
            break;
        }*/
        case RSPELL_ENCHANT_WEAPON:
        {
            return enchant_spell(p, randint0(4) + plev / 20, randint0(4) + plev / 20, 0);
        }
        case RSPELL_ELEM_BRAND:
        {
            return brand_ammo(p);
        }
        case RSPELL_IDENTIFY_FULLY:
        {
            identify_pack(p);
            break;
        }
        case RSPELL_CLOUD_KILL:
        {
            msg_spell(p, " casts a cloud of death.");
            fire_ball(p, GF_POIS, dir, 40 + plev / 2, 3);
            break;
        }
        case RSPELL_RESIST_ELEMENTS:
        {
            int time = randint1(20) + 20;

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case RSPELL_DAY_MISRULE:
        {
            bool dummy;
            int i = randint1(30) + 30;
            const char *pm;

            switch (p->psex)
            {
                case SEX_FEMALE: pm = "Daughter"; break;
                case SEX_MALE: pm = "Son"; break;
                default: pm = "Creature"; break;
            }

            msg(p, "%s of the Night rejoice! It's the Day of the Misrule!", pm);
            player_set_timed(p, TMD_FAST, i, TRUE);
            effect_berserk(p, i, &dummy);
            break;
        }
        case RSPELL_ARMOR_NIGHT:
        {
            player_inc_timed(p, TMD_SHIELD, randint1(20) + 30, TRUE, TRUE);
            break;
        }
        case RSPELL_SHOCK_WAVE:
        {
            msg_spell(p, " casts a sonic wave.");
            fire_ball(p, GF_SOUND, dir, 10 + plev, 2);
            break;
        }
        case RSPELL_AVOID_TRAPS:
        {
            player_inc_timed(p, TMD_TRAPS, 30 + randint1(20) + plev, TRUE, TRUE);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_hunt_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_HUNT_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case ASPELL_SHOT_ELEC:
        {
            set_bow_brand(p, BOW_BRAND_ELEC, 3);
            break;
        }
        case ASPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case ASPELL_SHOT_COLD:
        {
            set_bow_brand(p, BOW_BRAND_COLD, 5);
            break;
        }
        case ASPELL_LIGHT_AREA:
        {
            msg_spell(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case ASPELL_SHOT_FIRE:
        {
            set_bow_brand(p, BOW_BRAND_FIRE, 7);
            break;
        }
        case ASPELL_FARSIGHT:
        {
            player_inc_timed(p, TMD_FARSIGHT, plev + randint1(20), TRUE, TRUE);
            break;
        }
        case ASPELL_SHOT_ACID:
        {
            set_bow_brand(p, BOW_BRAND_ACID, 9);
            break;
        }
        case ASPELL_SHOT_THUNDER:
        {
            set_bow_brand(p, BOW_BRAND_THUNDER, 0);
            break;
        }
        case ASPELL_SHOT_CONF:
        {
            set_bow_brand(p, BOW_BRAND_CONF, 0);
            break;
        }
        case ASPELL_SHOT_POISON:
        {
            set_bow_brand(p, BOW_BRAND_POISON, 11);
            break;
        }
        case ASPELL_SHOT_PIERCE:
        {
            set_bow_brand(p, BOW_BRAND_PIERCE, 0);
            break;
        }
        case ASPELL_SHOT_SHARDS:
        {
            set_bow_brand(p, BOW_BRAND_SHARDS, 0);
            break;
        }
        case ASPELL_SHOT_ICE:
        {
            set_bow_brand(p, BOW_BRAND_ICE, 0);
            break;
        }
        case ASPELL_SHOT_FLAME:
        {
            set_bow_brand(p, BOW_BRAND_FLAME, 0);
            break;
        }
        case ASPELL_SHOT_WATER:
        {
            set_bow_brand(p, BOW_BRAND_WATER, 0);
            break;
        }
        case ASPELL_SHOT_POWER:
        {
            set_bow_brand(p, BOW_BRAND_POWER, 20);
            break;
        }
        case ASPELL_SHOT_SONIC:
        {
            set_bow_brand(p, BOW_BRAND_SONIC, 0);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_psi_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;
    int beam = beam_chance(p);

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_PSI_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case TSPELL_MIND_BLAST:
        {
            msg_spell(p, " hurls a mind blast.");
            fire_bolt_or_beam(p, beam * 2, GF_PSI, dir, damroll(3 + (plev - 1) / 4, 3 + plev / 15));
            break;
        }
        case TSPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case TSPELL_TELEPORT_BLINK:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 7 + plev);
            break;
        }
        case TSPELL_LIGHT_AREA:
        {
            msg_spell(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case TSPELL_DETECT_FEATURES:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case TSPELL_CONFUSE_MONSTER:
        {
            msg_spell(p, " hurls a confusing blast.");
            confuse_monster(p, dir, plev * 3 / 2, TRUE);
            break;
        }
        case TSPELL_CELL_ADJUST:
        {
            hp_player(p, damroll(plev / 2, 6));
            player_dec_timed(p, TMD_CUT, 15, TRUE);
            break;
        }
        case TSPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 5);
            break;
        }
        case TSPELL_STONE_MUD:
        {
            msg_spell(p, " makes a moving gesture.");
            wall_to_mud(p, dir);
            break;
        }
        case TSPELL_MIND_VISION:
        {
            mind_vision(p, note);
            break;
        }
        case TSPELL_SLEEP_MONSTERS:
        {
            sleep_monsters(p, TRUE);
            break;
        }
        case TSPELL_SENSE_SURROUND:
        {
            map_area(p);
            break;
        }
        case TSPELL_IDENTIFY_ITEM:
        {
            return ident_spell(p);
        }
        case TSPELL_SAT_HUNGER:
        {
            msg(p, "You focus your mind to convert energy into food.");
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        case TSPELL_STUN_MONSTERS:
        {
            stun_monsters(p);
            break;
        }
        case TSPELL_KINETIC_BLAST:
        {
            msg_spell(p, " hurls a kinetic blast.");
            fire_beam(p, GF_FORCE, dir, 50 + plev + randint1(plev * 2));
            break;
        }
        case TSPELL_PSI_DRAIN:
        {
            msg_spell(p, " hurls a psychic bolt.");
            fire_bolt(p, GF_PSI_DRAIN, dir, damroll(plev / 2, 6));
            break;
        }
        case TSPELL_MIND_WAVE:
        {
            msg_spell(p, " radiates a powerful mind wave.");
            fire_ball(p, GF_PSI, 0, 3 * plev / 2, 2 + plev / 10);
            break;
        }
        case TSPELL_DETECTION:
        {
            detect_all(p, TRUE);
            break;
        }
        case TSPELL_TELE_OBJECT:
        {
            return telekinesis(p, note);
        }
        case TSPELL_FORCE_SHIELD:
        {
            player_inc_timed(p, TMD_SHIELD, randint1(20) + 30, TRUE, TRUE);
            break;
        }
        case TSPELL_ELEM_SHIELD:
        {
            int time = randint1(20) + 20;

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case TSPELL_TIMED_ESP:
        {
            player_inc_timed(p, TMD_ESP, randint1(10) + plev, TRUE, TRUE);
            break;
        }
        case TSPELL_SONIC_WAVE:
        {
            msg_spell(p, " radiates a powerful sonic wave.");
            fire_ball(p, GF_SOUND, 0, 3 * plev / 2, 2 + plev / 10);
            break;
        }
        case TSPELL_WORD_RECALL:
        {
            set_recall(p, note, FALSE);
            break;
        }
        case TSPELL_SPACE_TIME:
        {
            player_inc_timed(p, TMD_ANCHOR, plev + randint1(20), TRUE, TRUE);
            break;
        }
        case TSPELL_TELEPORT_AWAY:
        {
            msg_spell(p, " makes a rush gesture.");
            fire_ball(p, GF_AWAY_ALL, 0, MAX_SIGHT_LGE * 5, 2 + plev / 10);
            break;
        }
        case TSPELL_PROBA_TRAVEL:
        {
            player_inc_timed(p, TMD_PROBTRAVEL, plev + randint1(20), TRUE, TRUE);
            break;
        }
        case TSPELL_INERTIA_WAVE:
        {
            msg_spell(p, " radiates a powerful inertia wave.");
            fire_ball(p, GF_INERT, 0, 3 * plev / 2, 2 + plev / 10);
            break;
        }
        case TSPELL_TIME_BOLT:
        {
            msg_spell(p, " breaks the space/time continuum.");
            if (p->timed[TMD_ANCHOR])
            {
                if (one_in_(3))
                {
                    msg(p, "The space/time anchor stops your time bolt!");
                    break;
                }
                else if (one_in_(3))
                    player_clear_timed(p, TMD_ANCHOR, TRUE);
            }
            fire_bolt_or_beam(p, beam, GF_TIME, dir, damroll(10 + plev / 12, plev));
            break;
        }
        case TSPELL_BIOFEEDBACK:
        {
            player_inc_timed(p, TMD_BIOFEEDBACK, randint1(10 + plev), TRUE, TRUE);
            break;
        }
        case TSPELL_ENLIGHT_LEVEL:
        {
            wiz_light(p, FALSE);
            break;
        }
        case TSPELL_KINETIC_WAVE:
        {
            msg_spell(p, " radiates a powerful kinetic wave.");
            fire_ball(p, GF_FORCE, 0, plev * 4, 3 + plev / 10);
            break;
        }
        case TSPELL_EXTRA_HEALING:
        {
            msg(p, "Your body begins to rapidly recover.");
            hp_player(p, 1000);
            player_clear_timed(p, TMD_POISONED, TRUE);
            player_clear_timed(p, TMD_STUN, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            player_clear_timed(p, TMD_AMNESIA, TRUE);
            break;
        }
        case TSPELL_ADRENALINE:
        {
            bool dummy;
            int time = randint1(10) + 10;

            effect_berserk(p, time, &dummy);
            player_inc_timed(p, TMD_ADRENALINE, time, TRUE, TRUE);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_death_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;
    int beam = beam_chance(p);

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_DEATH_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case NSPELL_HORRIFY:
        {
            int dam = damroll(2 + plev * 2 / 3, 4);

            msg_spell(p, " creates visions of terror!");
            fire_bolt_hack(p, GF_STUN, dir, dam);
            fire_bolt_hack(p, GF_TURN_ALL, dir, dam);
            break;
        }
        case NSPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case NSPELL_PHASE_DOOR:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case NSPELL_REMOVE_FEAR:
        {
            player_clear_timed(p, TMD_AFRAID, TRUE);
            break;
        }
        case NSPELL_FIND_TRAPS_DOORS:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case NSPELL_RAISE_DEAD:
        {
            msg_spell(p, " mumbles coldly.");
            fire_ball(p, GF_RAISE, 0, 1, 1 + plev / 10);
            break;
        }
        case NSPELL_CONFUSE_MONSTER:
        {
            msg_spell(p, " makes a complicated gesture.");
            confuse_monster(p, dir, plev, TRUE);
            break;
        }
        case NSPELL_TRAP_DOOR_DESTRUCT:
        {
            msg_spell(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case NSPELL_RESIST_COLD:
        {
            player_inc_timed(p, TMD_OPP_COLD, randint1(20) + 20, TRUE, TRUE);
            break;
        }
        case NSPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 5);
            break;
        }
        case NSPELL_FROST_BOLT:
        {
            msg_spell(p, " casts a frost bolt.");
            fire_bolt_or_beam(p, beam - 10, GF_COLD, dir, damroll(5 + (plev - 5) / 4, 8));
            break;
        }
        case NSPELL_TURN_STONE_TO_MUD:
        {
            msg_spell(p, " makes a moving gesture.");
            wall_to_mud(p, dir);
            break;
        }
        case NSPELL_SHADOW_TOUCH:
        {
            player_inc_timed(p, TMD_TOUCH, randint1(10 + plev * 4 / 10) + 20 + plev * 3 / 10, TRUE,
                TRUE);
            break;
        }
        case NSPELL_SATISFY_HUNGER:
        {
            player_set_food(p, PY_FOOD_MAX - 1);
            break;
        }
        case NSPELL_GUARD:
        {
            msg_spell(p, " emits a silent order.");
            fire_bolt(p, GF_GUARD, dir, plev * 2);
            break;
        }
        case NSPELL_IDENTIFY:
        {
            return ident_spell(p);
        }
        case NSPELL_RECHARGE_ITEM:
        {
            return recharge(p, 2 + plev / 5);
        }
        case NSPELL_DETECT_INVISIBLE:
        {
            reveal_monsters(p, TRUE);
            break;
        }
        case NSPELL_ABSORB_SOUL:
        {
            player_set_timed(p, TMD_SOUL, randint1(30 + plev * 2) + 50 + plev, TRUE);
            break;
        }
        case NSPELL_HORRIFY_BEAM:
        {
            int dam = damroll(2 + plev * 2 / 3, 4);

            msg_spell(p, " creates visions of terror!");
            fire_beam_hack(p, GF_STUN, dir, dam);
            fire_beam_hack(p, GF_TURN_ALL, dir, dam);
            break;
        }
        case NSPELL_TURN_UNDEAD:
        {
            msg_spell(p, " tries to turn undead.");
            turn_undead(p);
            break;
        }
        case NSPELL_FROST_BALL:
        {
            msg_spell(p, " casts a frost ball.");
            fire_ball(p, GF_COLD, dir, 30 + plev, 2);
            break;
        }
        case NSPELL_DISPEL_UNDEAD:
        {
            msg_spell(p, " dispels undead.");
            dispel_undead(p, randint1(plev * 3), TRUE);
            break;
        }
        case NSPELL_FOLLOW:
        {
            msg_spell(p, " emits a silent order.");
            fire_bolt(p, GF_FOLLOW, dir, plev * 2);
            break;
        }
        case NSPELL_HASTE_SELF:
        {
            player_inc_timed_nostack(p, TMD_FAST, randint1(20) + plev, randint1(5), TRUE);
            break;
        }
        case NSPELL_RESISTANCE:
        {
            int time = randint1(20) + 20;

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case NSPELL_NETHER_BOLT:
        {
            msg_spell(p, " casts a nether bolt.");
            fire_bolt_or_beam(p, beam / 4, GF_NETHER, dir, damroll(11, plev));
            break;
        }
        case NSPELL_UNDEAD_FORM:
        {
            if (p->state.stat_use[A_INT] < 18+70)
            {
                /* Won't work... let's preserve the char */
                msg(p, "You're not smart enough to turn into Undead Form.");
                return FALSE;
            }
            my_strcpy(p->died_flavor, "was turned into an undead being", sizeof(p->died_flavor));
            take_hit(p, 5000, "the spell of Undead Form", FALSE);
            break;
        }
        case NSPELL_DRAIN_LIFE:
        {
            msg_spell(p, " casts a vampiric bolt.");
            fire_bolt(p, GF_DRAIN, dir, 100);
            break;
        }
        case NSPELL_TELEPORT_LEVEL:
        {
            teleport_player_level(p);
            break;
        }
        case NSPELL_ICE_STORM:
        {
            msg_spell(p, " invokes an ice storm.");
            fire_ball(p, GF_ICE, dir, 50 + plev * 2, 3);
            break;
        }
        case NSPELL_ELEMENTAL_BRAND:
        {
            brand_weapon(p, FALSE);
            break;
        }
        case NSPELL_TREASURE_DETECTION:
        {
            detect_treasure(p, TRUE, FALSE);
            break;
        }
        case NSPELL_WORD_OF_RECALL:
        {
            set_recall(p, note, FALSE);
            break;
        }
        case NSPELL_SHIELD:
        {
            player_inc_timed(p, TMD_SHIELD, randint1(20) + 30, TRUE, TRUE);
            break;
        }
        case NSPELL_WORD_OF_DESTRUCTION:
        {
            destroy_area(p, p->depth, p->py, p->px, 15, TRUE);
            break;
        }
        case NSPELL_NETHER_BALL:
        {
            msg_spell(p, " casts a nether ball.");
            fire_ball(p, GF_NETHER, dir, 100 + 2 * plev, 2);
            break;
        }
        case NSPELL_HORRIFY_BALL:
        {
            int dam = damroll(2 + plev * 2 / 3, 4);

            msg_spell(p, " creates visions of terror!");
            fire_ball_hack(p, GF_STUN, dir, dam, 3 + plev / 10);
            fire_ball_hack(p, GF_TURN_ALL, dir, dam, 3 + plev / 10);
            break;
        }
        case NSPELL_RUNE_OF_PROTECTION:
        {
            warding_glyph(p);
            break;
        }
        case NSPELL_CLOUD_DRAINING:
        {
            msg_spell(p, " casts a cloud of draining.");
            fire_ball(p, GF_DRAIN, dir, 100, 2);
            break;
        }
        case NSPELL_ATTACK:
        {
            msg_spell(p, " emits a silent order.");
            fire_bolt(p, GF_ATTACK, dir, plev * 2);
            break;
        }
        case NSPELL_DARKNESS_STORM:
        {
            msg_spell(p, " casts a darkness storm.");
            fire_ball(p, GF_DARK, dir, plev * 5 + damroll(10, 10), 3);
            break;
        }
        case NSPELL_DEATH:
        {
            msg_spell(p, " calls upon Death herself!");
            fire_bolt(p, GF_DEATH, dir, 1);
            my_strcpy(p->died_flavor, "called upon Death", sizeof(p->died_flavor));
            take_hit(p, 5000, "the Death spell", FALSE);
            break;
        }
        case NSPELL_MASS_HORRIFY:
        {
            int dam = damroll(2 + plev * 2 / 3, 4);

            msg_spell(p, " creates visions of terror!");
            project_los(p, GF_STUN, dam, TRUE);
            project_los(p, GF_TURN_ALL, dam, TRUE);
            break;
        }
        case NSPELL_TOUCH_DEATH:
        {
            if (p->state.stat_use[A_STR] < 18+120)
            {
                msg(p, "You're not strong enough to use the Touch of Death.");
                return FALSE;
            }
            if (p->state.stat_use[A_DEX] < 18+120)
            {
                msg(p, "You're not dextrous enough to use the Touch of Death.");
                return FALSE;
            }
            player_set_timed(p, TMD_DEADLY, 12 + randint1(12), TRUE);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


#define EBOOST(dam) (((dam)) * (20 + elem_power) / 20)
static bool cast_elem_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;
    int beam = beam_chance(p);
    int spell_power, elem_power;

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_ELEM_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Chance of getting a beam */
    spell_power = p->spell_power[spell];
    if (spell <= ESPELL_MAELSTROM)
        beam += spell_power * 10;
    else
        beam += spell_power * 5;

    /* Spell power */
    elem_power = 0;
    if (p->timed[TMD_EPOWER])
        elem_power = p->spell_power[ESPELL_ELEM_POWER];

    /* Spells */
    switch (spell)
    {
        case ESPELL_SHOCKING_GRASP:
        {
            player_inc_timed(p, TMD_SGRASP, EBOOST(randint1(spell_power * 5) + spell_power * 5),
                TRUE, TRUE);
            break;
        }
        case ESPELL_LIGHTNING_BOLT:
        {
            msg_spell(p, " casts a lightning bolt.");
            fire_bolt_or_beam(p, beam, GF_ELEC, dir,
                EBOOST(damroll(7 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_RESIST_ELEC:
        {
            bool dummy;

            effect_do(p, EF_RESIST_ELEC, &dummy, TRUE, 0,
                EBOOST(randint1(spell_power * 10) + spell_power * 10), 0);
            break;
        }
        case ESPELL_RESIST_POISON:
        {
            player_inc_timed(p, TMD_OPP_POIS, EBOOST(randint1(spell_power * 10) + spell_power * 10),
                TRUE, TRUE);
            break;
        }
        case ESPELL_LIGHTNING_BALL:
        {
            msg_spell(p, " casts a lightning ball.");
            fire_ball(p, GF_ELEC, dir, EBOOST(25 + plev + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            break;
        }
        case ESPELL_TOXIC_CLOUD:
        {
            msg_spell(p, " casts a toxic cloud.");
            fire_ball(p, GF_POIS, dir, EBOOST(plev * 2 + spell_power * 20 - 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_SHOCK_WAVE:
        {
            msg_spell(p, " casts a sonic wave.");
            fire_ball(p, GF_SOUND, dir, EBOOST(125 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_THUNDERSTORM:
        {
            int y;

            msg_spell(p, " invokes a thunderstorm!");
            fire_ball(p, GF_SOUND, 0, EBOOST(225 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            for (y = 0; y < 8; y++)
                fire_beam(p, GF_ELEC, ddd[y], EBOOST(damroll(7 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_BURNING_HANDS:
        {
            msg_spell(p, "'s hands are covered with fire.");
            fire_melee(p, GF_FIRE, dir, EBOOST(damroll(4 + (plev - 1) / 4, 3 + spell_power)));
            break;
        }
        case ESPELL_FIRE_BOLT:
        {
            msg_spell(p, " casts a fire bolt.");
            fire_bolt_or_beam(p, beam, GF_FIRE, dir,
                EBOOST(damroll(6 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_ILLUMINATION:
        {
            msg_spell(p, " casts a minor fire ball.");
            light_area(p, 0, EBOOST((1 + spell_power) * 3 / 2));
            break;
        }
        case ESPELL_RESIST_FIRE:
        {
            player_inc_timed(p, TMD_OPP_FIRE, EBOOST(randint1(spell_power * 10) + spell_power * 10),
                TRUE, TRUE);
            break;
        }
        case ESPELL_FIRE_BALL:
        {
            msg_spell(p, " casts a fire ball.");
            fire_ball(p, GF_FIRE, dir, EBOOST(20 + plev + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            break;
        }
        case ESPELL_FIREFLASH:
        {
            msg_spell(p, " casts a fire flash.");
            fire_ball(p, GF_FIRE, dir, EBOOST(plev * 2 + spell_power * 20 - 10),
                EBOOST(1 + spell_power));
            fire_ball_hack(p, GF_STUN, dir, damroll(2 + plev * 2 / 3, 4), EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_PLASMA_BALL:
        {
            msg_spell(p, " casts a plasma ball.");
            fire_ball(p, GF_PLASMA, dir, EBOOST(150 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_INFERNO:
        {
            int y;

            msg_spell(p, " invokes a hellish storm.");
            fire_ball(p, GF_PLASMA, 0, EBOOST(250 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            for (y = 0; y < 8; y++)
                fire_beam(p, GF_FIRE, ddd[y], EBOOST(damroll(6 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_STONE_MUD:
        {
            int y;

            msg_spell(p, " makes a moving gesture.");
            for (y = 0; y < EBOOST((1 + spell_power) / 2); y++)
                wall_to_mud(p, dir);
            break;
        }
        case ESPELL_ACID_BOLT:
        {
            msg_spell(p, " casts an acid bolt.");
            fire_bolt_or_beam(p, beam, GF_ACID, dir,
                EBOOST(damroll(8 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_RESIST_ACID:
        {
            bool dummy;

            effect_do(p, EF_RESIST_ACID, &dummy, TRUE, 0,
                EBOOST(randint1(spell_power * 10) + spell_power * 10), 0);
            break;
        }
        case ESPELL_WALL_CREATION:
        {
            int y;

            for (y = 0; y < EBOOST(spell_power * 3 / 2); y++)
                wall_creation(p, ddd[randint0(8)]);
            break;
        }
        case ESPELL_ACID_BALL:
        {
            msg_spell(p, " casts an acid ball.");
            fire_ball(p, GF_ACID, dir, EBOOST(30 + plev + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            break;
        }
        case ESPELL_STONE_SKIN:
        {
            player_inc_timed(p, TMD_SHIELD, EBOOST(randint1(spell_power * 10) + spell_power * 10),
                TRUE, TRUE);
            break;
        }
        case ESPELL_EXPLOSION:
        {
            msg_spell(p, " fires an exploding missile.");
            fire_ball(p, GF_SHARD, dir, EBOOST(10 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_WALL_FORCE:
        {
            msg_spell(p, " casts a force ball.");
            fire_ball(p, GF_FORCE, dir, EBOOST(200 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_FREEZING_AURA:
        {
            player_inc_timed(p, TMD_ICY_AURA, EBOOST(randint1(spell_power * 5) + spell_power * 5),
                TRUE, TRUE);
            break;
        }
        case ESPELL_FROST_BOLT:
        {
            msg_spell(p, " casts a frost bolt.");
            fire_bolt_or_beam(p, beam, GF_COLD, dir,
                EBOOST(damroll(5 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_RESIST_COLD:
        {
            player_inc_timed(p, TMD_OPP_COLD, EBOOST(randint1(spell_power * 10) + spell_power * 10),
                TRUE, TRUE);
            break;
        }
        case ESPELL_POTION_VITALITY:
        {
            bool dummy;

            player_set_food(p, PY_FOOD_MAX - 1);
            effect_hero(p, EBOOST(randint1(spell_power * 10) + spell_power * 10), &dummy);
            break;
        }
        case ESPELL_FROST_BALL:
        {
            msg_spell(p, " casts a frost ball.");
            fire_ball(p, GF_COLD, dir, EBOOST(15 + plev + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            break;
        }
        case ESPELL_ICE_STORM:
        {
            msg_spell(p, " invokes an ice storm.");
            fire_ball(p, GF_ICE, dir, EBOOST(plev * 2 + spell_power * 20), EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_TIDAL_WAVE:
        {
            msg_spell(p, " invokes a tidal wave.");
            fire_ball(p, GF_WATER, dir, EBOOST(175 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            break;
        }
        case ESPELL_MAELSTROM:
        {
            int y;

            msg_spell(p, " invokes a maelstrom!");
            fire_ball(p, GF_WATER, 0, EBOOST(275 + plev * 2 + spell_power * 20),
                EBOOST(1 + spell_power));
            for (y = 0; y < 8; y++)
                fire_beam(p, GF_COLD, ddd[y], EBOOST(damroll(5 + (plev - 5) / 4, 6 + spell_power)));
            break;
        }
        case ESPELL_ELEM_POWER:
        {
            player_inc_timed(p, TMD_EPOWER, randint1(20) + 20, TRUE, TRUE);
            break;
        }
        case ESPELL_ELEM_BOLT:
        {
            msg_spell(p, " casts an elemental bolt.");
            fire_bolt_or_beam(p, beam / 2, GF_ELEC, dir,
                EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
            fire_bolt_or_beam(p, beam / 2, GF_COLD, dir,
                EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
            fire_bolt_or_beam(p, beam / 2, GF_FIRE, dir,
                EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
            fire_bolt_or_beam(p, beam / 2, GF_ACID, dir,
                EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
            break;
        }
        case ESPELL_MINOR_HAVOC:
        {
            earthquake(p, p->depth, p->py, p->px, EBOOST(5 + spell_power / 2));
            break;
        }
        case ESPELL_RESISTANCE:
        {
            int time = EBOOST(randint1(spell_power * 5) + spell_power * 5);

            player_inc_timed(p, TMD_OPP_ACID, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_ELEC, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_FIRE, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_COLD, time, TRUE, TRUE);
            player_inc_timed(p, TMD_OPP_POIS, time, TRUE, TRUE);
            break;
        }
        case ESPELL_ELEM_BALL:
        {
            msg_spell(p, " casts an elemental ball.");
            fire_ball(p, GF_ELEC, dir, EBOOST(10 + plev + spell_power * 5),
                EBOOST((7 + spell_power) / 4));
            fire_ball(p, GF_COLD, dir, EBOOST(10 + plev + spell_power * 5),
                EBOOST((7 + spell_power) / 4));
            fire_ball(p, GF_FIRE, dir, EBOOST(10 + plev + spell_power * 5),
                EBOOST((7 + spell_power) / 4));
            fire_ball(p, GF_ACID, dir, EBOOST(10 + plev + spell_power * 5),
                EBOOST((7 + spell_power) / 4));
            break;
        }
        case ESPELL_ELEM_BRAND:
        {
            elemental_brand(p, 1 + spell_power / 2);
            break;
        }
        case ESPELL_MAJOR_HAVOC:
        {
            destroy_area(p, p->depth, p->py, p->px, EBOOST(10 + spell_power), TRUE);
            break;
        }
        case ESPELL_ELEM_STORM:
        {
            int y;

            msg_spell(p, " invokes an elemental storm.");
            fire_ball(p, GF_ELEC, 0, EBOOST(200 + plev * 2 + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            fire_ball(p, GF_COLD, 0, EBOOST(200 + plev * 2 + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            fire_ball(p, GF_FIRE, 0, EBOOST(200 + plev * 2 + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            fire_ball(p, GF_ACID, 0, EBOOST(200 + plev * 2 + spell_power * 10),
                EBOOST((3 + spell_power) / 2));
            for (y = 0; y < 8; y++)
            {
                fire_beam(p, GF_ELEC, ddd[y],
                    EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
                fire_beam(p, GF_COLD, ddd[y],
                    EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
                fire_beam(p, GF_FIRE, ddd[y],
                    EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
                fire_beam(p, GF_ACID, ddd[y],
                    EBOOST(damroll(4 + (plev - 5) / 4, 6 + spell_power / 2)));
            }
            break;
        }
    }

    /* Success */
    return (TRUE);
}


static bool cast_summon_spell(struct player *p, int spell, quark_t note, int dir)
{
    int plev = p->lev;

    /* Projected */
    if (spell >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_SUMMON_PROJECT, dir, spell - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return (TRUE);
    }

    /* Spells */
    switch (spell)
    {
        case ZSPELL_SUMMON_MONSTER:
        {
            msg_spell(p, " summons a monster.");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_specific(p, p->py, p->px, p->depth, S_MONSTER, 0, 100);
            break;
        }
        case ZSPELL_DETECT_MONSTERS:
        {
            detect_monsters_normal(p, TRUE, TRUE);
            break;
        }
        case ZSPELL_BLINK:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case ZSPELL_CALL_LIGHT:
        {
            msg_prayer(p, " calls light.");
            light_area(p, damroll(2, plev / 2), plev / 10 + 1);
            break;
        }
        case ZSPELL_CURE_LIGHT_WOUNDS:
        {
            bool dummy;

            effect_light(p, &dummy);
            break;
        }
        case ZSPELL_FIND_TRAPS_DOORS:
        {
            detect_traps(p);
            detect_doorstairs(p, TRUE);
            break;
        }
        case ZSPELL_TRAP_DOOR_DESTRUCTION:
        {
            msg_spell(p, " sways his hands.");
            destroy_doors_touch(p);
            break;
        }
        case ZSPELL_FARSIGHT:
        {
            player_inc_timed(p, TMD_ZFARSIGHT, plev + randint1(20), TRUE, TRUE);
            break;
        }
        case ZSPELL_SENSE_SURROUNDINGS:
        {
            map_area(p);
            break;
        }
        case ZSPELL_CHARM:
        {
            msg_spell(p, " makes an hypnotic move.");
            project_los(p, GF_CONTROL, 1 + randint1(2), TRUE);
            break;
        }
        case ZSPELL_HEALING:
        {
            hp_player(p, plev * 6);
            player_clear_timed(p, TMD_BLIND, TRUE);
            player_clear_timed(p, TMD_CONFUSED, TRUE);
            player_clear_timed(p, TMD_AMNESIA, TRUE);
            player_clear_timed(p, TMD_POISONED, TRUE);
            player_clear_timed(p, TMD_STUN, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            break;
        }
        case ZSPELL_REGENERATION:
        {
            player_inc_timed(p, TMD_REGEN, 20 + randint1(20), TRUE, TRUE);
            break;
        }
        case ZSPELL_RECOVERY:
        {
            do_res_stat(p, A_STR);
            do_res_stat(p, A_INT);
            do_res_stat(p, A_WIS);
            do_res_stat(p, A_DEX);
            do_res_stat(p, A_CON);
            do_res_stat(p, A_CHR);
            restore_level(p);
            break;
        }
        case ZSPELL_HARMONY:
        {
            int time = randint1(10) + 10;

            player_inc_timed(p, TMD_BLESSED, time, TRUE, TRUE);
            player_inc_timed(p, TMD_HARMONY, time, TRUE, TRUE);
            break;
        }
        case ZSPELL_SUMMON_JELLY:
        {
            msg_spell(p, " summons a jelly.");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_specific(p, p->py, p->px, p->depth, S_JELLY, 0, 100);
            break;
        }
        case ZSPELL_SUMMON_GOLEM:
        {
            msg_spell(p, " summons a golem.");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_specific(p, p->py, p->px, p->depth, S_GOLEM, 0, 100);
            break;
        }
        case ZSPELL_SUMMON_VORTEX:
        {
            msg_spell(p, " summons a vortex.");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_specific(p, p->py, p->px, p->depth, S_VORTEX, 0, 100);
            break;
        }
        case ZSPELL_SUMMON_HYDRA:
        {
            msg_spell(p, " summons a hydra.");
            sound(p, MSG_SUM_HYDRA);
            if (check_antisummon(p, NULL)) break;
            summon_specific(p, p->py, p->px, p->depth, S_HYDRA, 0, 100);
            break;
        }
        case ZSPELL_CONTROL:
        {
            msg_spell(p, " screams a raging order.");
            project_los(p, GF_CONTROL, MSTATUS_ATTACK, TRUE);
            break;
        }
        case ZSPELL_RESILIENCE:
        {
            int i;
            monster_type *m_ptr;

            /* Expand the lifespan of slaves */
            for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
            {
                m_ptr = cave_monster(cave_get(p->depth), i);

                /* Skip dead monsters */
                if (!m_ptr->r_idx) continue;

                /* Skip non slaves */
                if (p->id != m_ptr->master) continue;

                /* Double the lifespan (cap the value depending on monster level) */
                m_ptr->lifespan = MIN(m_ptr->lifespan * 2, m_ptr->level * 2 + 20);
            }
            break;
        }
        case ZSPELL_THUNDERSTORM:
        {
            int y;

            msg_spell(p, " invokes a thunderstorm!");
            for (y = 0; y < 8; y++)
            {
                fire_beam(p, GF_ELEC, ddd[y], (200 + plev * 2) / 3);
                fire_beam(p, GF_SOUND, ddd[y], (200 + plev * 2) / 3);
                fire_beam(p, GF_LIGHT, ddd[y], (200 + plev * 2) / 3);
            }
            break;
        }
        case ZSPELL_DETONATE:
        {
            int i;
            monster_type *m_ptr;
            monster_race *r_ptr;
            int p_flag = PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
            struct cave *c = cave_get(p->depth);

            /* Make all controlled jellies and vortices explode */
            for (i = cave_monster_max(c) - 1; i >= 1; i--)
            {
                m_ptr = cave_monster(c, i);

                /* Skip dead monsters */
                if (!m_ptr->r_idx) continue;

                /* Skip non slaves */
                if (p->id != m_ptr->master) continue;

                /* Jellies explode with a slowing effect */
                r_ptr = &r_info[m_ptr->r_idx];
                if (match_monster_bases(r_ptr->base, "jelly", "mold", NULL))
                {
                    project(i, 2, p->depth, m_ptr->fy, m_ptr->fx, 20, GF_OLD_SLOW, p_flag, "killed");

                    /* Delete the monster */
                    delete_monster_idx(c, i);
                }

                /* Vortices explode with a ball effect */
                else if (match_monster_bases(r_ptr->base, "vortex", NULL))
                {
                    bitflag f[RSF_SIZE];
                    int num = 0, j;
                    byte spells[RSF_MAX];

                    /* Extract the racial spell flags */
                    rsf_copy(f, r_ptr->spell_flags);

                    /* Require breath attacks */
                    set_spells(f, RST_BREATH);

                    /* Extract spells */
                    for (j = FLAG_START; j < RSF_MAX; j++)
                    {
                        if (rsf_has(f, j)) spells[num++] = j;
                    }

                    /* Pick at random */
                    project(i, 2, p->depth, m_ptr->fy, m_ptr->fx, m_ptr->level,
                        spell_effect(spells[randint0(num)]), p_flag, "killed");

                    /* Delete the monster */
                    delete_monster_idx(c, i);
                }
            }
            break;
        }
        case ZSPELL_SUPPRESS_SUMMONING:
        {
            player_inc_timed(p, TMD_ANTISUMMON, plev + randint1(20), TRUE, TRUE);
            break;
        }
        case ZSPELL_BANISHMENT:
        {
            return banishment(p);
        }
        case ZSPELL_MASS_BANISHMENT:
        {
            mass_banishment(p);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


bool cast_spell(struct player *p, int index, quark_t note, int dir)
{
    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p)) return FALSE;

    switch (p->clazz->spell_book)
    {
        case TV_MAGIC_BOOK: return cast_mage_spell(p, index, note, dir);
        case TV_PRAYER_BOOK: return cast_priest_spell(p, index, note, dir);
        case TV_SORCERY_BOOK: return cast_sorc_spell(p, index, note, dir);
        case TV_SHADOW_BOOK: return cast_shad_spell(p, index, note, dir);
        case TV_HUNT_BOOK: return cast_hunt_spell(p, index, note, dir);
        case TV_PSI_BOOK: return cast_psi_spell(p, index, note, dir);
        case TV_DEATH_BOOK: return cast_death_spell(p, index, note, dir);
        case TV_ELEM_BOOK: return cast_elem_spell(p, index, note, dir);
        case TV_SUMMON_BOOK: return cast_summon_spell(p, index, note, dir);
    }
    return FALSE;
}


bool cast_ghost_spell(struct player *p, int ability, int dir)
{
    int plev = p->lev;

    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p)) return FALSE;

    /* Projected */
    if (ability >= PY_MAX_SPELLS)
    {
        project_hook(p, GF_GHOST_PROJECT, dir, ability - PY_MAX_SPELLS, PROJECT_STOP | PROJECT_KILL,
            "killed");
        return TRUE;
    }

    /* Spell effects */
    switch (ability)
    {
        case GSPELL_TELEPORT_BLINK:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }
        case GSPELL_SCARE_MONSTERS:
        {
            fear_monster(p, dir, plev, TRUE);
            break;
        }
        case GSPELL_CONFUSE_MONSTERS:
        {
            msg_spell(p, " makes a complicated gesture.");
            confuse_monster(p, dir, plev, TRUE);
            break;
        }
        case GSPELL_TELEPORT_SELF:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, plev * 8);
            break;
        }
        case GSPELL_NETHER_BOLT:
        {
            msg_spell(p, " casts a nether bolt.");
            fire_bolt_or_beam(p, plev * 2, GF_NETHER, dir, 50 + damroll(5, 5) + plev);
            break;
        }
        case GSPELL_DISEN_BOLT:
        {
            msg_spell(p, " casts a disenchantment bolt.");
            fire_bolt_or_beam(p, plev * 2, GF_DISEN, dir, 75 + damroll(5, 5) + plev);
            break;
        }
        case GSPELL_NETHER_BALL:
        {
            msg_spell(p, " casts a nether ball.");
            fire_ball(p, GF_NETHER, dir, 100 + 2 * plev, 2);
            break;
        }
        case GSPELL_DARK_BALL:
        {
            msg_spell(p, " invokes a darkness storm.");
            fire_ball(p, GF_DARK, dir, plev * 5 + damroll(10, 10), 3);
            break;
        }
    }

    /* Success */
    return TRUE;
}


static int spell_dam_typ(int typ, int rlev, aspect dam_aspect)
{
    switch (typ)
    {
        case RSF_ARROW_X: return ARROWX_DMG(rlev, dam_aspect);
        case RSF_ARROW_1: return ARROW1_DMG(rlev, dam_aspect);
        case RSF_ARROW_2: return ARROW2_DMG(rlev, dam_aspect);
        case RSF_ARROW_3: return ARROW3_DMG(rlev, dam_aspect);
        case RSF_ARROW_4: return ARROW4_DMG(rlev, dam_aspect);
        case RSF_BOULDER: return BOULDER_DMG(rlev, dam_aspect);
        case RSF_BA_ACID: return BA_ACID_DMG(rlev, dam_aspect);
        case RSF_BA_ELEC: return BA_ELEC_DMG(rlev, dam_aspect);
        case RSF_BA_FIRE: return BA_FIRE_DMG(rlev, dam_aspect);
        case RSF_BA_COLD: return BA_COLD_DMG(rlev, dam_aspect);
        case RSF_BA_POIS: return BA_POIS_DMG(rlev, dam_aspect);
        case RSF_BA_NETH: return BA_NETH_DMG(rlev, dam_aspect);
        case RSF_BA_WATE: return BA_WATE_DMG(rlev, dam_aspect);
        case RSF_BA_MANA: return BA_MANA_DMG(rlev, dam_aspect);
        case RSF_BA_DARK: return BA_DARK_DMG(rlev, dam_aspect);
        case RSF_MIND_BLAST: return MIND_BLAST_DMG(rlev, dam_aspect);
        case RSF_BRAIN_SMASH: return BRAIN_SMASH_DMG(rlev, dam_aspect);
        case RSF_CAUSE_1: return CAUSE_1_DMG(rlev, dam_aspect);
        case RSF_CAUSE_2: return CAUSE_2_DMG(rlev, dam_aspect);
        case RSF_CAUSE_3: return CAUSE_3_DMG(rlev, dam_aspect);
        case RSF_CAUSE_4: return CAUSE_4_DMG(rlev, dam_aspect);
        case RSF_BO_ACID: return BO_ACID_DMG(rlev, dam_aspect);
        case RSF_BO_ELEC: return BO_ELEC_DMG(rlev, dam_aspect);
        case RSF_BO_FIRE: return BO_FIRE_DMG(rlev, dam_aspect);
        case RSF_BO_COLD: return BO_COLD_DMG(rlev, dam_aspect);
        case RSF_BO_NETH: return BO_NETH_DMG(rlev, dam_aspect);
        case RSF_BO_WATE: return BO_WATE_DMG(rlev, dam_aspect);
        case RSF_BO_MANA: return BO_MANA_DMG(rlev, dam_aspect);
        case RSF_BO_PLAS: return BO_PLAS_DMG(rlev, dam_aspect);
        case RSF_BO_ICEE: return BO_ICEE_DMG(rlev, dam_aspect);
        case RSF_MISSILE: return MISSILE_DMG(rlev, dam_aspect);
    }

    return 0;
}


bool cast_mimic_spell(struct player *p, int flag, int dir)
{
    int plev = p->lev;
    monster_race *r_ptr = &r_info[p->r_idx];
    int rad = (rf_has(r_ptr->flags, RF_POWERFUL)? 3: 2);

    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p)) return (FALSE);

    /* Unaware players casting spells reveal themselves */
    if (p->k_idx) aware_player(p, p);

    /* Projected */
    if (flag < 0)
    {
        project_hook(p, GF_MIMIC_PROJECT, dir, 0 - flag, PROJECT_STOP | PROJECT_KILL, "killed");
        return (TRUE);
    }

    /* Spell effects */
    switch (flag)
    {
        case RSF_SHRIEK:
        {
            aggravate_monsters(p, 0);
            break;
        }

        case RSF_ARROW_X:
        case RSF_ARROW_1:
        case RSF_ARROW_2:
        case RSF_ARROW_3:
        case RSF_ARROW_4:
        {
            const char *what[] = {"a seeker arrow", "a shot", "an arrow", "a bolt", "a missile"};
            int type[] = {GF_ARROW_X, GF_ARROW_1, GF_ARROW_2, GF_ARROW_3, GF_ARROW_4};

            msg_format_near(p, MSG_PY_SPELL, " fires %s.", what[flag - RSF_ARROW_X]);
            fire_bolt(p, type[flag - RSF_ARROW_X], dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BR_ACID:
        case RSF_BR_ELEC:
        case RSF_BR_FIRE:
        case RSF_BR_COLD:
        case RSF_BR_POIS:
        case RSF_BR_NETH:
        case RSF_BR_LIGHT:
        case RSF_BR_DARK:
        case RSF_BR_SOUN:
        case RSF_BR_CHAO:
        case RSF_BR_DISE:
        case RSF_BR_NEXU:
        case RSF_BR_TIME:
        case RSF_BR_INER:
        case RSF_BR_GRAV:
        case RSF_BR_SHAR:
        case RSF_BR_PLAS:
        case RSF_BR_WALL:
        case RSF_BR_MANA:
        case RSF_BR_WATE:
        {
            /* Breathing is handled by the corresponding command */
            break;
        }

        case RSF_BOULDER:
        {
            msg_spell(p, " hurls a boulder.");
            fire_bolt(p, GF_BOULDER, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BA_ACID:
        {
            msg_spell(p, " casts an acid ball.");
            fire_ball(p, GF_ACID, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_ELEC:
        {
            msg_spell(p, " casts a lightning ball.");
            fire_ball(p, GF_ELEC, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_FIRE:
        {
            msg_spell(p, " casts a fire ball.");
            fire_ball(p, GF_FIRE, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_COLD:
        {
            msg_spell(p, " casts a frost ball.");
            fire_ball(p, GF_COLD, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_POIS:
        {
            msg_spell(p, " casts a stinking cloud.");
            fire_ball(p, GF_POIS, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_NETH:
        {
            msg_spell(p, " casts a nether ball.");
            fire_ball(p, GF_NETHER, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_WATE:
        {
            msg_spell(p, " gestures fluidly.");
            fire_ball(p, GF_WATER, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_MANA:
        {
            msg_spell(p, " invokes a mana storm.");
            fire_ball(p, GF_MANA, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_BA_DARK:
        {
            msg_spell(p, " invokes a darkness storm.");
            fire_ball(p, GF_DARK, dir, spell_dam_typ(flag, plev, RANDOMISE), rad);
            break;
        }

        case RSF_DRAIN_MANA:
        {
            fire_bolt(p, GF_DRAIN_MANA, dir, plev);
            break;
        }

        case RSF_MIND_BLAST:
        {
            msg_spell(p, " hurls a mind blast.");
            fire_bolt(p, GF_BLAST, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BRAIN_SMASH:
        {
            msg_spell(p, " hurls a powerful mind blast.");
            fire_bolt(p, GF_SMASH, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_CAUSE_1:
        {
            msg_spell(p, " curses.");
            fire_bolt(p, GF_CURSE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_CAUSE_2:
        {
            msg_spell(p, " curses horribly.");
            fire_bolt(p, GF_CURSE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_CAUSE_3:
        {
            msg_spell(p, " mumbles loudly, incanting terribly.");
            fire_bolt(p, GF_CURSE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_CAUSE_4:
        {
            msg_spell(p, " screams the word DIE!");
            fire_bolt(p, GF_CURSE2, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_ACID:
        {
            msg_spell(p, " casts an acid bolt.");
            fire_bolt(p, GF_ACID, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_ELEC:
        {
            msg_spell(p, " casts a lightning bolt.");
            fire_bolt(p, GF_ELEC, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_FIRE:
        {
            msg_spell(p, " casts a fire bolt.");
            fire_bolt(p, GF_FIRE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_COLD:
        {
            msg_spell(p, " casts a frost bolt.");
            fire_bolt(p, GF_COLD, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_NETH:
        {
            msg_spell(p, " casts a nether bolt.");
            fire_bolt(p, GF_NETHER, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_WATE:
        {
            msg_spell(p, " casts a water bolt.");
            fire_bolt(p, GF_WATER, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_MANA:
        {
            msg_spell(p, " casts a mana bolt.");
            fire_bolt(p, GF_MANA, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_PLAS:
        {
            msg_spell(p, " casts a plasma bolt.");
            fire_bolt(p, GF_PLASMA, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_BO_ICEE:
        {
            msg_spell(p, " casts an ice bolt.");
            fire_bolt(p, GF_ICE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_MISSILE:
        {
            msg_spell(p, " casts a magic missile.");
            fire_bolt(p, GF_MISSILE, dir, spell_dam_typ(flag, plev, RANDOMISE));
            break;
        }

        case RSF_SCARE:
        {
            fear_monster(p, dir, plev, TRUE);
            break;
        }

        case RSF_BLIND:
        {
            blind_monster(p, dir, plev);
            break;
        }

        case RSF_CONF:
        {
            msg_spell(p, " makes a complicated gesture.");
            confuse_monster(p, dir, plev, TRUE);
            break;
        }

        case RSF_SLOW:
        {
            msg_spell(p, " makes a lengthy gesture.");
            slow_monster(p, dir, TRUE);
            break;
        }

        case RSF_HOLD:
        {
            /* Hmm... just put it to sleep for now */
            msg_spell(p, " gestures and mumbles calmly.");
            sleep_monster(p, dir, TRUE);
            break;
        }

        case RSF_HASTE:
        {
            player_inc_timed_nostack(p, TMD_FAST, randint1(20) + plev, randint1(5), TRUE);
            break;
        }

        case RSF_HEAL:
        {
            const char *poss = player_poss(p);

            msg_format_near(p, MSG_PY_SPELL, " concentrates on %s wounds.", poss);

            hp_player(p, plev * 6);
            player_clear_timed(p, TMD_AFRAID, TRUE);
            player_clear_timed(p, TMD_POISONED, TRUE);
            player_clear_timed(p, TMD_CUT, TRUE);
            break;
        }

        case RSF_BLINK:
        {
            msg_spell(p, " blinks away.");
            teleport_player(p, 10);
            break;
        }

        case RSF_TPORT:
        {
            msg_spell(p, " teleports away.");
            teleport_player(p, MAX_SIGHT_LGE * 2 + 5);
            break;
        }

        case RSF_TELE_TO:
        {
            fire_bolt(p, GF_TELE_TO, dir, 0);
            break;
        }

        case RSF_TELE_AWAY:
        {
            msg_spell(p, " makes a rush gesture.");
            teleport_monster(p, dir);
            break;
        }

        case RSF_TELE_LEVEL:
        {
            fire_bolt(p, GF_TELE_LEVEL, dir, 0);
            break;
        }

        case RSF_DARKNESS:
        {
            msg_spell(p, " gestures in shadow.");
            unlight_area(p, 0, 3);
            break;
        }

        case RSF_TRAPS:
        {
            /* Only on random levels */
            if (!random_level(p->depth))
                msg(p, "You cannot create traps here...");
            else
            {
                msg_spell(p, " casts a spell and cackles evilly.");
                fire_ball(p, GF_MAKE_TRAP, dir, 0, 1);
            }
            break;
        }

        case RSF_FORGET:
        {
            fire_bolt_hack(p, GF_FORGET, dir, plev);
            break;
        }

        case RSF_ANIM_DEAD:
        {
            int Ind = get_player_index(get_connection(p->conn));

            msg_spell(p, " mumbles coldly.");
            p->current_sound = -2;
            project(0 - Ind, 1 + plev / 10, p->depth, p->py, p->px, 0, GF_RAISE,
                PROJECT_ITEM | PROJECT_HIDE, "killed");
            p->current_sound = -1;
            break;
        }

        case RSF_S_KIN:
        {
            const char *poss = player_poss(p);

            msg_format_near(p, MSG_PY_SPELL, " magically summons %s kin.", poss);
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_kin_type = r_ptr->d_char;
            summon_monster_aux(p, p->py, p->px, S_KIN, plev, 6, 10);
            break;
        }

        case RSF_S_HI_DEMON:
        {
            msg_spell(p, " magically summons greater demons!");
            sound(p, MSG_SUM_HI_DEMON);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_HI_DEMON, plev, 8, 10);
            break;
        } 

        case RSF_S_MONSTER:
        {
            msg_spell(p, " magically summons a monster!");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_MONSTER, plev, 1, 10);
            break;
        }

        case RSF_S_MONSTERS:
        {
            msg_spell(p, " magically summons monsters!");
            sound(p, MSG_SUM_MONSTER);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_MONSTERS, plev, 8, 10);
            break;
        }       

        case RSF_S_ANIMAL:
        {
            msg_spell(p, " magically summons animals.");
            sound(p, MSG_SUM_ANIMAL);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_ANIMAL, plev, 6, 10);
            break;
        }

        case RSF_S_SPIDER:
        {
            msg_spell(p, " magically summons spiders.");
            sound(p, MSG_SUM_SPIDER);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_SPIDER, plev, 6, 10);
            break;
        }   

        case RSF_S_HOUND:
        {
            msg_spell(p, " magically summons hounds.");
            sound(p, MSG_SUM_HOUND);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_HOUND, plev, 6, 10);
            break;
        }

        case RSF_S_HYDRA:
        {
            msg_spell(p, " magically summons hydras.");
            sound(p, MSG_SUM_HYDRA);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_HYDRA, plev, 6, 10);
            break;
        }

        case RSF_S_AINU:
        {
            msg_spell(p, " magically summons an ainu!");
            sound(p, MSG_SUM_AINU);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_AINU, plev, 1, 10);
            break;
        }

        case RSF_S_DEMON:
        {
            msg_spell(p, " magically summons a demon!");
            sound(p, MSG_SUM_DEMON);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_DEMON, plev, 1, 10);
            break;
        }    

        case RSF_S_UNDEAD:
        {
            msg_spell(p, " magically summons an undead monster!");
            sound(p, MSG_SUM_UNDEAD);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_UNDEAD, plev, 1, 10);
            break;
        }         

        case RSF_S_DRAGON:
        {
            msg_spell(p, " magically summons a dragon!");
            sound(p, MSG_SUM_DRAGON);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_DRAGON, plev, 1, 10);
            break;
        }             

        case RSF_S_HI_UNDEAD:
        {
            msg_spell(p, " magically summons greater undead!");
            sound(p, MSG_SUM_HI_UNDEAD);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_HI_UNDEAD, plev, 8, 10);
            break;
        }                 

        case RSF_S_HI_DRAGON:
        {
            msg_spell(p, " magically summons ancient dragons!");
            sound(p, MSG_SUM_HI_DRAGON);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_HI_DRAGON, plev, 8, 10);
            break;
        }

        case RSF_S_WRAITH:
        {
            msg_spell(p, " magically summons mighty undead monsters!");
            sound(p, MSG_SUM_WRAITH);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_WRAITH, plev, 8, 10);
            break;
        } 

        case RSF_S_UNIQUE:
        {
            msg_spell(p, " magically summons special opponents!");
            sound(p, MSG_SUM_UNIQUE);
            if (check_antisummon(p, NULL)) break;
            summon_monster_aux(p, p->py, p->px, S_UNIQUE, plev, 8, 0);
            break;
        }
    }

    /* Success */
    return (TRUE);
}


bool cast_spell_proj(struct player *p, int book, int index)
{
    /* Clear current */
    current_clear(p);

    /* Set current spell */
    p->current_spell = index;

    /* Save book */
    p->current_item = (s16b)(0 - book);

    switch (book)
    {
        case 1: return cast_ghost_spell(p, index, 0);
        case 2: return cast_mimic_spell(p, index, 0);
        case TV_MAGIC_BOOK: return cast_mage_spell(p, index, 0, 0);
        case TV_PRAYER_BOOK: return cast_priest_spell(p, index, 0, 0);
        case TV_SORCERY_BOOK: return cast_sorc_spell(p, index, 0, 0);
        case TV_SHADOW_BOOK: return cast_shad_spell(p, index, 0, 0);
        case TV_HUNT_BOOK: return cast_hunt_spell(p, index, 0, 0);
        case TV_PSI_BOOK: return cast_psi_spell(p, index, 0, 0);
        case TV_DEATH_BOOK: return cast_death_spell(p, index, 0, 0);
        case TV_ELEM_BOOK: return cast_elem_spell(p, index, 0, 0);
        case TV_SUMMON_BOOK: return cast_summon_spell(p, index, 0, 0);
    }

    return FALSE;
}
