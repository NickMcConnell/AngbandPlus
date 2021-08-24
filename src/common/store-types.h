/*
 * File: store-types.h
 * Purpose: Global store type declarations
 */

#ifndef INCLUDED_STORE_TYPES_H
#define INCLUDED_STORE_TYPES_H

/*** Constants ***/

/* List of store indexes */
enum
{
    STORE_NONE = -1,
    STORE_GENERAL = 0,
    STORE_ARMOR = 1,
    STORE_WEAPON = 2,
    STORE_TEMPLE = 3,
    STORE_ALCHEMY = 4,
    STORE_MAGIC = 5,
    STORE_LIBRARY = 6,
    STORE_B_MARKET = 7,
    STORE_XBM = 8,
    STORE_TAVERN = 9,
    STORE_HOME = 10,
    STORE_PLAYER = 11,

    MAX_STORES = 12
};

/*** Types ***/

struct object_buy
{
    struct object_buy *next;
    byte tval;
    size_t flag;
};

/*
 * A store owner
 */
struct owner
{
    char *name;         /* Name */
    unsigned int oidx;  /* Index */
    struct owner *next;
    s32b max_cost;      /* Purse limit */
};

struct normal_entry
{
    struct object_kind *kind;
    s16b rarity;
    s16b factor;
};

/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store
{
    struct owner *owners;       /* Owners */
    struct owner *owner;        /* Current owner */
    unsigned int sidx;          /* Index */
    char *name;
    struct store *next;
    s16b stock_num;             /* Stock -- number of entries */
    s16b stock_size;            /* Stock -- size of array (transient) */
    struct object *stock;       /* Stock -- actual stock items */

    /* Always stock these items */
    size_t always_size;
    size_t always_num;
    struct object_kind **always_table;

    /* Select a number of these items to stock */
    size_t normal_size;
    size_t normal_num;
    struct normal_entry *normal_table;

    /* Buy these items */
    struct object_buy *buy;

    int turnover;
    int normal_stock_min;
    int normal_stock_max;

    s16b max_depth;             /* Max level of last customer */
};

#endif /* INCLUDED_STORE_TYPES_H */
