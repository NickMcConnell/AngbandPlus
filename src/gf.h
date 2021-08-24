#ifndef INCLUDED_GF_H
#define INCLUDED_GF_H

/* Player and monster effects. GF_ is a mysterious historical abbreviation */
enum {
    GF_NONE = 0,  /* if (type) project(..., type,...) */

    /* Resistable Effects */
    GF_ACID = 1,
    GF_ELEC,
    GF_FIRE,
    GF_COLD,
    GF_POIS,
    GF_LIGHT,
    GF_DARK,
    GF_CONFUSION,
    GF_NETHER,
    GF_NEXUS,
    GF_SOUND,
    GF_SHARDS,
    GF_CHAOS,
    GF_DISENCHANT,
    GF_TIME,
    GF_WATER,
    GF_PLASMA,
    GF_FORCE,
    GF_INERTIA,
    GF_GRAVITY,
    GF_DISINTEGRATE,  /* XXX Vuln => HURT_ROCK */
    GF_BLIND,
    GF_FEAR,
    GF_STUN,
    GF_TELEPORT,
    GF_SLEEP,         /* plr->free_act */
    GF_SLOW,

    GF_RES_MIN = 1,
    GF_RES_MAX = GF_SLOW,
    GF_RES_COUNT = GF_RES_MAX+1, /* plr->resist[GF_RES_COUNT] */

    GF_MANA = 32,
    GF_NUKE,
    GF_STORM,
    GF_HOLY_FIRE,
    GF_HELL_FIRE,
    GF_ICE,
    GF_ROCKET,
    GF_METEOR,
    GF_ROCK,
    GF_ARROW,
    GF_SUPER_ARROW,
    GF_MISSILE,

    /* Curses */
    GF_CAUSE_1,
    GF_CAUSE_2,
    GF_CAUSE_3,
    GF_CAUSE_4,
    GF_HAND_DOOM,
    GF_BLOOD_CURSE,

    /* Mental Attacks */
    GF_PSY_SPEAR,
    GF_PSI,
    GF_PSI_DRAIN,
    GF_PSI_BRAIN_SMASH,
    GF_PSI_STORM,
    GF_TELEKINESIS,
    GF_DOMINATION,
    GF_SUBJUGATION,
    GF_DRAIN_MANA,
    GF_MIND_BLAST,
    GF_BRAIN_SMASH,
    GF_AMNESIA,

    /* Status Effects */
    GF_OLD_CLONE,
    GF_OLD_POLY,
    GF_OLD_HEAL,
    GF_STAR_HEAL,  /* smash potion effect */
    GF_OLD_SPEED,
    GF_OLD_CONF,
    GF_OLD_DRAIN,
    GF_STASIS,
    GF_STASIS_EVIL,
    GF_PARALYSIS,
    GF_ELDRITCH,
    GF_ANTIMAGIC,
    GF_CRUSADE,
    GF_UNHOLY_WORD,
    GF_UNLIFE,

    /* Terrain Effects */
    GF_LIGHT_WEAK,
    GF_DARK_WEAK,
    GF_KILL_WALL,
    GF_KILL_DOOR,
    GF_KILL_TRAP,
    GF_REMOVE_OBSTACLE,
    GF_MAKE_DOOR,
    GF_MAKE_TRAP,
    GF_MAKE_TREE,
    GF_MAKE_GLYPH,
    GF_MAKE_WALL,
    GF_JAM_DOOR,
    GF_WATER_FLOW,
    GF_WATER2,
    GF_LAVA_FLOW,
    GF_WEB,
    GF_QUAKE,

    /* Turning, Dispelling, Controlling, etc */
    GF_AWAY_UNDEAD,
    GF_AWAY_EVIL,
    GF_ISOLATION,
    GF_TURN_UNDEAD,
    GF_TURN_EVIL,
    GF_DISP_UNDEAD,
    GF_DISP_EVIL,
    GF_DISP_GOOD,
    GF_DISP_DEMON,
    GF_DISP_LIVING,
    GF_DISP_ALL,
    GF_CONTROL_UNDEAD,
    GF_CONTROL_DEMON,
    GF_CONTROL_ANIMAL,
    GF_CONTROL_LIVING,
    GF_CONTROL_PACT_MONSTER,
    GF_CHARM,
    GF_CHARM_RING_BEARER,
    GF_CAPTURE,
    GF_ANIM_DEAD,
    GF_DEATH_RAY,
    GF_GENOCIDE,

    /* Object Effects */
    GF_IDENTIFY,

    /* Class Specific */
    GF_ATTACK,
    GF_ENGETSU,
    GF_SEEKER,
    GF_SUPER_RAY,
    GF_BLOOD,
    GF_ELDRITCH_STUN,
    GF_ELDRITCH_DRAIN,
    GF_ELDRITCH_DISPEL,
    GF_ELDRITCH_CONFUSE,
    GF_ELDRITCH_HOWL,
    GF_ENTOMB,
    GF_MANA_CLASH,
    GF_PHARAOHS_CURSE,
    GF_DRAINING_TOUCH,
    GF_DEATH_TOUCH,
    GF_STEAL,
    GF_LICH_DRAIN,
    GF_LICH_GENOCIDE,
    GF_LICH_WORD,

    /* New Stuff (unsorted) */
    GF_FRIENDSHIP,
    GF_OBEDIENCE,
    GF_EXORCISM,
    GF_REPENTANCE,

    /* Add new effects here. Reorganize later for next major version upgrade. */

    GF_COUNT  /* enumerate 0 <= i < GF_COUNT */
};            /* allocate gf[GF_COUNT] */
/* XXX Temp */
#define GF_CONF GF_CONFUSION
#define GF_DISEN GF_DISENCHANT

/* GF_* flags (GFF_*) provide a loose classification of effects.
 * Mostly, we need to know when to display damage numbers or apply
 * auras to riding players, etc. */
#define GFF_ELEMENTAL   0x00000001
#define GFF_CURSE       0x00000002
#define GFF_MENTAL      0x00000004
#define GFF_STATUS      0x00000008  /* confuse, sleep, fear, etc. */
#define GFF_TERRAIN     0x00000010  /* wall of stone, summon tree, stone to mud, etc */
#define GFF_TELEPORT    0x00000020
#define GFF_CHARM       0x00000040
#define GFF_OBJECT      0x00000080  /* identify (bard?) */
#define GFF_SPECIAL     0x00000100  /* class specific special effect */


#define GFF_DAMAGE      0x00010000  /* causes physical damage */
#define GFF_RIDING      0x00020000  /* apply aura to player when riding */
#define GFF_HIDE        0x00040000  /* no animation during spell projection (e.g. ball GF_DRAIN_MANA) */
#define GFF_TARGET_PET  0x00080000
#define GFF_NO_REFLECT  0x00100000
#define GFF_RESIST      0x00200000
#define GFF_RESIST_HI   0x00400000
#define GFF_DISPLAY     0x00800000  /* display on character sheet (for resists) */

typedef struct {
    int  id;
    cptr name;
    byte color;
    cptr parse;
    int  flags;
    int  resist;
} gf_info_t, *gf_info_ptr;

extern gf_info_ptr gf_parse_name(cptr token);
extern gf_info_ptr gf_lookup(int id);
extern int         gf_resist(int id);

/* Directly damage the player or a monster with a GF_* attack type, 
 * bypassing the project() code. This is needed for monster melee,
 * monster auras, and player innate attacks where project() is overly
 * complicated and error prone if flg is incorrectly set. */

/* We also need information on whether the effect is spell/breath based,
 * or whether it is the result of melee contact. Mostly, this if for message
 * purposes, but, occasionally, it has gameplay effects. For example, GF_DISENCHANT
 * will usually dispel_player() on a touch effect (GF_AFFECT_ATTACK or _AURA)
 * while it always apply_disenchant()s on a breath (GF_AFFECT_SPELL).
 * The flags param only contains the following GF_AFFECT_* flags, and never
 * needs any of the PROJECT_* stuff. At the moment, using bits is inappropriate
 * as the options are mutually exclusive, but other options may follow in the future.
 */
#define GF_AFFECT_SPELL  0x01 /* Monster spell or breath from a project() */
#define GF_AFFECT_ATTACK 0x02 /* Monster melee B:HIT:HURT(10d10):DISENCHANT */
#define GF_AFFECT_AURA   0x04 /* Monster aura  A:DISENCHANT(3d5) */
#define GF_AFFECT_TRAP   0x08
#define GF_AFFECT_QUIET  0x10 /* stop "It resists" and "It is unaffected" message spam */
#define GF_AFFECT_PROJECT 0x20 /* hack for "monster_target" (cf project) */
extern int gf_affect_p(who_t who, int type, int dam, int flags); /* returns damage for "revenge" */
extern bool gf_affect_m(who_t who, mon_ptr mon, int type, int dam, int flags);
extern bool gf_affect_f(who_t who, point_t where, int type, int dam, int flags);
extern bool gf_affect_o(who_t who, point_t where, int type, int dam, int flags);

/* exposed for the sake of wizard commands: calculate damage based
 * on the player's alignment */
extern int plr_holy_dam(int dam);
extern int plr_hell_dam(int dam);

#endif
