#ifndef STORE_H
#define STORE_H

#include <src/structures.h>
#include <src/object_classes.h>

/*** Constants ***/

/*
 * Total number of stores (see "store.c", etc)
 */
#define MAX_STORES	10

#define MAX_STORES_MORIA	6

// Max number of items per inventory slot
#define STORE_MAX_ITEM			99

/*
 * Total number of stores (see "store.c", etc)
 */
#define MAX_INVENTORY_HOME	24

#define TOWN_DAWN		10000	/* Number of turns from dawn to dawn XXX */


/*
 * Store constants
 */
#define STORE_INVEN_MAX	24		/* Max number of discrete objs in inven */
#define STORE_SHUFFLE	25		/* 1/Chance (per day) of an owner changing */
#define STORE_TURNS		1000	/* Number of turns between turnovers */
#define STORE_TURNOVER_NPPANGBAND	12		/* Normal shop turnover, per day */
#define STORE_MIN_KEEP_NPPANGBAND	8		/* Min slots to "always" keep full */
#define STORE_MAX_KEEP_NPPANGBAND	20		/* Max slots to "always" keep full */
#define STORE_TURNOVER_NPPMORIA		9		/* Normal shop turnover, per day */
#define STORE_MIN_KEEP_NPPMORIA		10		/* Min slots to "always" keep full */
#define STORE_MAX_KEEP_NPPMORIA		18		/* Max slots to "always" keep full */
#define BLACK_MARKET_ADJUST         5       // Try to give the black market a little more inventory


/*
 * Store index definitions (see "store.c", etc)
 */
#define STORE_GENERAL	0
#define STORE_ARMOR		1
#define STORE_WEAPON	2
#define STORE_TEMPLE	3
#define STORE_ALCHEMY	4
#define STORE_MAGIC		5
#define STORE_B_MARKET	6
#define STORE_HOME		7
#define STORE_GUILD		8
#define STORE_BOOKSHOP	9


/*** Types ***/

/*
 * A store owner
 */

struct owner_type
{
    QString owner_name;	/* Name  */
    s32b max_cost;		/* Purse limit */
};


/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type
{
    byte owner;				/* Owner index */

    byte stock_num;			/* Stock -- Number of entries */
    s16b stock_size;		/* Stock -- Total Size of Array */
    object_type *stock;		/* Stock -- Actual stock items */
};



enum
{
    SERVICE_ENCHANT_ARMOR	= 0,
    SERVICE_ENCHANT_TO_HIT,
    SERVICE_ENCHANT_TO_DAM,
    SERVICE_ELEM_BRAND_WEAP,
    SERVICE_ELEM_BRAND_AMMO,
    SERVICE_RECHARGING,
    SERVICE_IDENTIFY,
    SERVICE_IDENTIFY_FULLY,
    SERVICE_CURE_CRITICAL,
    SERVICE_RESTORE_LIFE_LEVELS,
    SERVICE_REMOVE_CURSE,
    SERVICE_REMOVE_HEAVY_CURSE,
    SERVICE_RESTORE_STAT,
    SERVICE_INCREASE_STAT,
    SERVICE_CREATE_RANDART,
    SERVICE_PROBE_QUEST_MON,
    SERVICE_BUY_HEALING_POTION,
    SERVICE_BUY_LIFE_POTION,
    SERVICE_BUY_SCROLL_BANISHMENT,
    SERVICE_FIREPROOF_BOOK,
    SERVICE_QUEST_DEFER_REWARD,
    SERVICE_ABANDON_QUEST,
    SERVICE_QUEST_REWARD_RANDART,
    SERVICE_QUEST_REWARD_INC_HP,
    SERVICE_QUEST_REWARD_INC_STAT,
    SERVICE_QUEST_REWARD_AUGMENTATION,

    STORE_SERVICE_MAX
};


// Various service processing functions.
extern bool do_service_enchant(byte choice, u32b price);
extern bool do_service_brand(byte choice, u32b price);
extern bool do_service_recharge(byte choice, u32b price);
extern bool do_service_identify(byte choice, u32b price);
extern bool do_service_cure_critical(byte choice, u32b price);
extern bool do_service_restore_life(byte choice, u32b price);
extern bool do_service_remove_curse(byte choice, u32b price);
extern bool do_service_stat(byte choice, u32b price);
extern bool do_service_make_randart(byte choice, u32b price);
extern bool do_service_probing(byte choice, u32b price);
extern bool do_service_buy_object(byte choice, u32b price);
extern bool do_service_fireproof(byte choice, u32b price);
extern bool do_service_defer_reward(byte choice, u32b price);
extern bool do_service_abandon_quest(byte choice, u32b price);
extern bool do_service_quest_art_reward(byte choice, u32b price);
extern bool do_service_hp_inc(byte choice, u32b price);
extern bool do_service_augment(byte choice, u32b service_price);

#endif // STORE_H
