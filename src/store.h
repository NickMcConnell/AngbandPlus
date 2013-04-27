/* File: store.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/**
 * Store index definitions (see "store.c", etc)
 */
enum store_indexes	{
	STORE_GENERAL = 0,
	STORE_ARMOR,
	STORE_WEAPON,
	STORE_TEMPLE,
	STORE_ALCHEMY,
	STORE_MAGIC,
	STORE_B_MARKET,
	STORE_HOME
};

/**
 * Total number of stores (see "store.c", etc)
 */
#define MAX_STORES	(STORE_HOME+1)

/*
 * Store constants
 */
#define STORE_INVEN_MAX	24		/**< Max number of discrete objs in inven */
#define STORE_SHUFFLE	25		/**< 1/Chance (per day) of an owner changing */

extern void store_shuffle(store_indexes which);
extern void store_maint(store_indexes which);
extern void store_init(store_indexes which);


