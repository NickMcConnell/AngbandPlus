/*
 * File: store-types.h
 * Purpose: Global store type declarations
 */

#ifndef INCLUDED_STORE_TYPES_H
#define INCLUDED_STORE_TYPES_H

/*** Constants ***/

/* Number of welcome messages (1 for every 5 clvl starting at clvl 6) */
#define N_WELCOME   9

/* List of store types */
enum
{
    STORE_OTHER = 0,
    STORE_GENERAL,
    STORE_TEMPLE,
    STORE_B_MARKET,
    STORE_XBM,
    STORE_TAVERN,
    STORE_HOME,
    STORE_PLAYER
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
    int sidx;                   /* Index */
    int type;                   /* Type */
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

    char comment_welcome[N_WELCOME][NORMAL_WID];
};

#endif /* INCLUDED_STORE_TYPES_H */
