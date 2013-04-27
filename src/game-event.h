
#ifndef INCLUDED_GAME_EVENT_H
#define INCLUDED_GAME_EVENT_H

/* The various events we can send signals about. */
enum game_event_type
{
	EVENT_NO_CHANGE = 0,	/* for C++ */
	EVENT_MAP,		/* Some part of the map has changed. */

	EVENT_STATS,  	/* One or more of the stats. */
	EVENT_HP,	   	/* HP or MaxHP. */
	EVENT_MANA,	/* Mana or MaxMana. */
	EVENT_AC,		/* Armour Class. */
	EVENT_EXPERIENCE,	/* Experience or MaxExperience. */
	EVENT_PLAYERLEVEL,	/* Player's level has changed */
	EVENT_PLAYERTITLE,	/* Player's title has changed */
	EVENT_GOLD,	/* Player's gold amount. */
	EVENT_MONSTERHEALTH,	/* Observed monster's health level. */
	EVENT_DUNGEONLEVEL,	/* Dungeon depth */
	EVENT_SPEED,	/* Player's speed */
	EVENT_RACE_CLASS,	/* Race or Class */
	EVENT_STUDYSTATUS,	/* "Study" availability */
	EVENT_STATUS,	/* Status */
	EVENT_DETECTIONSTATUS,/* Trap detection status */
	EVENT_STATE,	/* The three 'R's: Resting, Repeating and
				   Searching */

	EVENT_PLAYERMOVED,

	EVENT_INVENTORY,
	EVENT_EQUIPMENT,
	EVENT_MONSTERLIST,
	EVENT_MONSTERTARGET,
	EVENT_OBJECTTARGET,
	EVENT_MESSAGES,

	EVENT_INITSTATUS,		/* New status message for initialisation */

	/* Changing of the game state/context. */
	EVENT_ENTER_INIT,
	EVENT_ENTER_BIRTH,
	EVENT_ENTER_GAME,
	EVENT_ENTER_STORE,
	EVENT_ENTER_DEATH,

	EVENT_END	/* can be sent at end of a series of events */
};

typedef union
{
	struct 
	{
		int x;
		int y;
	} point;

	const char* string;

} game_event_data;

typedef void game_event_handler(game_event_type type, game_event_data *data, void *user);

void event_register(game_event_type type, game_event_handler *fn, void *user);
void event_deregister(game_event_type type, game_event_handler *fn, void *user);
void event_register_set(game_event_type *type, size_t n_types, game_event_handler *fn, void *user);
void event_deregister_set(game_event_type *type, size_t n_types, game_event_handler *fn, void *user);

void event_signal_point(game_event_type, int x, int y);
void event_signal_string(game_event_type, const char *s);
void event_signal(game_event_type);

void game_event_init(void);

#endif /* INCLUDED_UI_EVENT_H */
