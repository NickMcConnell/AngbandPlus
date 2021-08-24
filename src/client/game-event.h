/*
 * File: game-event.h
 * Purpose: Allows the registering of handlers to be told about game events.
 */

#ifndef INCLUDED_GAME_EVENT_H
#define INCLUDED_GAME_EVENT_H

/*
 * The various events we can send signals about.
 */
typedef enum
{
    EVENT_MAP = 0,          /* Some part of the map has changed. */

    EVENT_STATS,            /* One or more of the stats. */
    EVENT_HP,               /* HP or Max HP. */
    EVENT_MANA,             /* Mana or Max Mana. */
    EVENT_AC,               /* Armour Class. */
    EVENT_EXPERIENCE,       /* Experience or Max Experience. */
    EVENT_PLAYERLEVEL,      /* Player's level has changed */
    EVENT_PLAYERTITLE,      /* Player's title has changed */
    EVENT_GOLD,             /* Player's gold amount. */
    EVENT_MONSTERHEALTH,    /* Observed monster's health level. */
    EVENT_DUNGEONLEVEL,     /* Dungeon depth */
    EVENT_PLAYERSPEED,      /* Player's speed */
    EVENT_RACE_CLASS,       /* Race or Class */
    EVENT_STUDYSTATUS,      /* "Study" availability */
    EVENT_STATUS,           /* Status */
    EVENT_DETECTIONSTATUS,  /* Trap detection status */
    EVENT_STATE,            /* Resting and Stealth Mode (+ Light level) */
    EVENT_PLUSSES,          /* Plusses to hit/dam */
    EVENT_OTHER,            /* Other info (skills, history, ...) */
    EVENT_LAG,              /* Lag meter */

    EVENT_INVENTORY,
    EVENT_EQUIPMENT,
    EVENT_ITEMLIST,
    EVENT_MONSTERLIST,
    EVENT_MONSTERTARGET,
    EVENT_OBJECTTARGET,
    EVENT_MESSAGE,
    EVENT_MESSAGE_CHAT,
    EVENT_SPELL,
    EVENT_SPECIAL_INFO,

    EVENT_SOUND,
    EVENT_BELL,

    EVENT_INPUT_FLUSH,

    EVENT_END             /* It's the end of a "set" of events, so safe to update */
} game_event_type;

#define N_GAME_EVENTS (EVENT_END + 1)

typedef union
{
    struct loc point;
    int type;
} game_event_data;

/*
 * A function called when a game event occurs - these are registered to be
 * called by event_add_handler or event_add_handler_set, and deregistered
 * when they should no longer be called through event_remove_handler or
 * event_remove_handler_set.
 */
typedef void game_event_handler(game_event_type type, game_event_data *data, void *user);

extern void event_add_handler(game_event_type type, game_event_handler *fn, void *user);
extern void event_remove_handler(game_event_type type, game_event_handler *fn, void *user);
extern void event_remove_all_handlers(void);
extern void event_add_handler_set(game_event_type *type, size_t n_types,
    game_event_handler *fn, void *user);
extern void event_remove_handler_set(game_event_type *type, size_t n_types,
    game_event_handler *fn, void *user);

extern void event_signal(game_event_type type);
extern void event_signal_point(game_event_type type, int x, int y);
extern void event_signal_type(game_event_type type, int t);

#endif /* INCLUDED_GAME_EVENT_H */
