/*
 * File: z-msg.h
 * Purpose: Message handling
 */

#ifndef INCLUDED_Z_MSG_H
#define INCLUDED_Z_MSG_H

/*** Constants ***/

/*** Message constants ***/

enum
{
    MSG_GENERIC = 0,
    MSG_HIT,
    MSG_MISS,
    MSG_FLEE,
    MSG_DROP,
    MSG_KILL,
    MSG_LEVEL,
    MSG_DEATH,
    MSG_STUDY,
    MSG_TELEPORT,
    MSG_SHOOT,
    MSG_QUAFF,
    MSG_ZAP_ROD,
    MSG_WALK,
    MSG_TPOTHER,
    MSG_HITWALL,
    MSG_EAT,
    MSG_STORE1,
    MSG_STORE2,
    MSG_STORE3,
    MSG_STORE4,
    MSG_DIG,
    MSG_OPENDOOR,
    MSG_SHUTDOOR,
    MSG_TPLEVEL,
    MSG_BELL,
    MSG_NOTHING_TO_OPEN,
    MSG_LOCKPICK_FAIL,
    MSG_STAIRS_DOWN,
    MSG_HITPOINT_WARN,
    MSG_ACT_ARTIFACT,
    MSG_USE_STAFF,
    MSG_DESTROY,
    MSG_MON_HIT,
    MSG_MON_TOUCH,
    MSG_MON_PUNCH,
    MSG_MON_KICK,
    MSG_MON_CLAW,
    MSG_MON_BITE,
    MSG_MON_STING,
    MSG_MON_BUTT,
    MSG_MON_CRUSH,
    MSG_MON_ENGULF,
    MSG_MON_CRAWL,
    MSG_MON_DROOL,
    MSG_MON_SPIT,
    MSG_MON_GAZE,
    MSG_MON_WAIL,
    MSG_MON_SPORE,
    MSG_MON_BEG,
    MSG_MON_INSULT,
    MSG_MON_MOAN,
    MSG_RECOVER,
    MSG_BLIND,
    MSG_CONFUSED,
    MSG_POISONED,
    MSG_AFRAID,
    MSG_PARALYZED,
    MSG_DRUGGED,
    MSG_SPEED,
    MSG_SLOW,
    MSG_SHIELD,
    MSG_BLESSED,
    MSG_HERO,
    MSG_BERSERK,
    MSG_BOLD,
    MSG_PROT_EVIL,
    MSG_INVULN,
    MSG_SEE_INVIS,
    MSG_INFRARED,
    MSG_RES_ACID,
    MSG_RES_ELEC,
    MSG_RES_FIRE,
    MSG_RES_COLD,
    MSG_RES_POIS,
    MSG_STUN,
    MSG_CUT,
    MSG_STAIRS_UP,
    MSG_STORE_ENTER,
    MSG_STORE_LEAVE,
    MSG_STORE_HOME,
    MSG_MONEY1,
    MSG_MONEY2,
    MSG_MONEY3,
    MSG_SHOOT_HIT,
    MSG_STORE5,
    MSG_LOCKPICK,
    MSG_DISARM,
    MSG_IDENT_BAD,
    MSG_IDENT_EGO,
    MSG_IDENT_ART,
    MSG_BR_ELEMENTS,
    MSG_BR_FROST,
    MSG_BR_ELEC,
    MSG_BR_ACID,
    MSG_BR_GAS,
    MSG_BR_FIRE,
    MSG_BR_WATE,
    MSG_BR_DISEN,
    MSG_BR_CHAOS,
    MSG_BR_SHARDS,
    MSG_BR_SOUND,
    MSG_BR_LIGHT,
    MSG_BR_DARK,
    MSG_BR_NETHER,
    MSG_BR_NEXUS,
    MSG_BR_TIME,
    MSG_BR_INERTIA,
    MSG_BR_GRAVITY,
    MSG_BR_PLASMA,
    MSG_BR_FORCE,
    MSG_SUM_MONSTER,
    MSG_SUM_AINU,
    MSG_SUM_UNDEAD,
    MSG_SUM_ANIMAL,
    MSG_SUM_SPIDER,
    MSG_SUM_HOUND,
    MSG_SUM_HYDRA,
    MSG_SUM_DEMON,
    MSG_SUM_DRAGON,
    MSG_SUM_HI_UNDEAD,
    MSG_SUM_HI_DRAGON,
    MSG_SUM_HI_DEMON,
    MSG_SUM_WRAITH,
    MSG_SUM_UNIQUE,
    MSG_WIELD,
    MSG_CURSED,
    MSG_PSEUDOID,
    MSG_HUNGRY,
    MSG_NOTICE,
    MSG_AMBIENT_DAY,
    MSG_AMBIENT_NITE,
    MSG_AMBIENT_DNG1,
    MSG_AMBIENT_DNG2,
    MSG_AMBIENT_DNG3,
    MSG_AMBIENT_DNG4,
    MSG_AMBIENT_DNG5,
    MSG_CREATE_TRAP,
    MSG_SHRIEK,
    MSG_CAST_FEAR,
    MSG_HIT_GOOD,
    MSG_HIT_GREAT,
    MSG_HIT_SUPERB,
    MSG_HIT_HI_GREAT,
    MSG_HIT_HI_SUPERB,
    MSG_SPELL,
    MSG_PRAYER,
    MSG_KILL_UNIQUE,
    MSG_KILL_KING,
    MSG_DRAIN_STAT,
    MSG_MULTIPLY,
    MSG_WILD_SHORE,
    MSG_WILD_GRASS,
    MSG_WILD_WOOD,
    MSG_WILD_SWAMP,
    MSG_WILD_WASTE,
    MSG_WILD_MOUNTAIN,
    MSG_WILD_VOLCANO,
    MSG_AMBIENT_SAURON,
    MSG_AMBIENT_MORGOTH,
    MSG_AMBIENT_DNG6,
    MSG_AMBIENT_SENYA,
    MSG_AMBIENT_XAKAZE,
    MSG_HIT_HI_CRITICAL,

    MSG_MAX,

    /* PWMAngband-specific */
    MSG_VERSION = 249,

    /* MAngband-specific */
    MSG_TALK = 250,
    MSG_SOCIAL = 251,
    MSG_PY_SPELL = 252,
    MSG_PY_PRAYER = 253,
    MSG_PY_MISC = 254,
    MSG_LOCAL = 255,
    MSG_WHISPER = 256,
    MSG_CHAT = 257
};

/*** Functions ***/

/** Initialisation/exit **/

/*
 * Initialise the messages package.  Should be called before using any other
 * functions in the package.
 */
errr messages_init(void);

/*
 * Free the message package.
 */
void messages_free(void);

/** General info **/

/*
 * Return the current number of messages stored.
 */
u16b messages_num(void);

/** Individual message handling **/

/*
 * Save a new message into the memory buffer, with text `str` and type `type`.
 * The type should be one of the MSG_ constants defined above.
 *
 * The new message may not be saved if it is identical to the one saved before
 * it, in which case the "count" of the message will be increased instead.
 * This count can be fetched using the message_count() function.
 */
void message_add(const char *str, u16b type);

/*
 * Returns the text of the message of age `age`.  The age of the most recently
 * saved message is 0, the one before that is of age 1, etc.
 *
 * Returns the empty string if the no messages of the age specified are
 * available.
 */
const char *message_str(u16b age);

/*
 * Returns the number of times the message of age `age` was saved. The age of
 * the most recently saved message is 0, the one before that is of age 1, etc.
 *
 * In other words, if message_add() was called five times, one after the other,
 * with the message "The orc sets your hair on fire.", then the text will only
 * have one age (age = 0), but will have a count of 5.
 */
u16b message_count(u16b age);

/*
 * Returns the type of the message of age `age`.  The age of the most recently
 * saved message is 0, the one before that is of age 1, etc.
 *
 * The type is one of the MSG_ constants, defined above.
 */
u16b message_type(u16b age);

/*
 * Returns the display colour of the message memorised `age` messages ago.
 * (i.e. age = 0 represents the last memorised message, age = 1 is the one
 * before that, etc).
 */
byte message_color(u16b age);

/** Message type changes **/

/*
 * Returns the colour for the message type `type`.
 */
byte message_type_color(u16b type);

/*
 * Defines the color `color` for the message type `type`.
 */
void message_color_define(u16b type, byte color);

/** Extra functions **/

/* Get last non local message */
const char *message_last(void);

/* Hide message */
void message_del(u16b age);

/* Message iterator */
typedef struct
{
    char *str;
    u16b type;
    u16b count;
    byte color;
} message_iter;

/* First message */
void message_first(message_iter *iter);

/* Next message */
void message_next(message_iter *iter);

#endif /* INCLUDED_Z_MSG_H */
