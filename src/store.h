/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* store.h: stores-related definitions */

#ifndef STORE_H_INCLUDED
#define STORE_H_INCLUDED

/*
 * Total number of stores (see "store.c", etc)
 */
#define MAX_STORES		9

/*
 * Total number of stores (see "store.c", etc)
 */
#define MAX_INVENTORY_HOME	24

/*
 * Number of required items per store
 */
#define MAX_PREREQ_ITEMS	7

/*
 * Store index definitions (see "store.c", etc)
 */
enum
{
        STORE_GENERAL,
        STORE_ARMOR,
        STORE_WEAPON,
        STORE_TEMPLE,
        STORE_ALCHEMY,
        STORE_MAGIC,
        STORE_B_MARKET,
        STORE_HOME,
        STORE_GUILD
};

/*
 * Store constants
 */
#define STORE_INVEN_MAX	24		/* Max number of discrete objs in inven */
#define STORE_OBJ_LEVEL	5		/* Magic Level for normal stores */
#define STORE_TURNOVER	9		/* Normal shop turnover, per day */
#define STORE_MIN_KEEP	6		/* Min slots to "always" keep full */
#define STORE_MAX_KEEP	18		/* Max slots to "always" keep full */
#define STORE_SHUFFLE	25		/* 1/Chance (per day) of an owner changing */
#define STORE_TURNS	1000		/* Number of turns between turnovers */

/*
 * A store owner
 */
struct owner_type
{
	u32b owner_name;	/* Name (offset) */

	s16b max_cost;		/* Purse limit */

	byte max_inflate;	/* Inflation (max) */
	byte min_inflate;	/* Inflation (min) */

	byte haggle_per;	/* Haggle unit */

	byte insult_max;	/* Insult limit */

	byte owner_race;	/* Owner race */
};

/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type
{
	byte owner;		/* Owner index */

	s16b insult_cur;	/* Insult counter */

	s16b good_buy;		/* Number of "good" buys */
	s16b bad_buy;		/* Number of "bad" buys */

	s32b store_open;	/* Closed until this turn */

	byte stock_num;		/* Stock -- Number of entries */
	s16b stock_size;	/* Stock -- Total Size of Array */
	object_type *stock;	/* Stock -- Actual stock items */

	s16b table_num;		/* Table -- Number of entries */
	s16b table_size;	/* Table -- Total Size of Array */
	s16b *table;		/* Table -- Legal item kinds */
};

extern byte store_req_items[MAX_STORES][MAX_PREREQ_ITEMS][2];

extern store_type *store;
extern owner_type *b_info;
extern char *b_name;
extern char *b_text;
extern byte *g_info;
extern char *g_name;
extern char *g_text;

/* store.c */
void do_cmd_store(void);
void store_shuffle(int which);
void store_maint(int which);
void store_init(int which);

#endif /* STORE_H_INCLUDED */
