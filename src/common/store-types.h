/*
 * File: store-types.h
 * Purpose: Global store type declarations
 */

#ifndef INCLUDED_STORE_TYPES_H
#define INCLUDED_STORE_TYPES_H

/*** Constants ***/

#define STORE_INVEN_MAX 24  /* Max number of discrete objs in inven */

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
    STORE_PLAYER = 10,

    MAX_STORES = 11
};

/*** Types ***/

/*
 * A store owner
 */
typedef struct owner
{
    char *name;         /* Name */
    unsigned int oidx;  /* Index */
    struct owner *next;
    s32b max_cost;      /* Purse limit */
} owner_type;

/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store
{
    struct owner *owners;       /* Owners */
    struct owner *owner;        /* Current owner */
    unsigned int sidx;          /* Index */
    struct store *next;
    s16b stock_num;             /* Stock -- Number of entries */
    s16b stock_size;            /* Stock -- Total Size of Array (transient) */
    object_type *stock;         /* Stock -- Actual stock items */
    unsigned int table_num;     /* Table -- Number of entries (transient) */
    unsigned int table_size;    /* Table -- Total Size of Array (transient) */
    object_kind **table;        /* Table -- Legal item kinds (transient) */
    s16b max_depth;             /* Max level of last customer */
};

#endif /* INCLUDED_STORE_TYPES_H */
