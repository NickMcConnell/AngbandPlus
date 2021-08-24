/*
 * File: party.h
 * Purpose: Support for the "party" system
 */

#ifndef INCLUDED_PARTY_H
#define INCLUDED_PARTY_H

/*
 * Expiration delay for deceased characters
 */
#define EXPIRY_DELAY_DEAD 7

/*
 * Number of entries in the player name hash table.
 * This must be a power of 2!
 */
#define NUM_HASH_ENTRIES    1024

/*
 * The following is a simple hash table, which is used to maintain the player
 * database.
 *
 * The hash function is going to be h(x) = x % n, where n is the length of
 * the table.  For efficiency reasons, n will be a power of 2, thus the
 * hash function can be a bitwise "and" and get the relevant bits off the end.
 *
 * If any two IDs map to the same hash slot, they will be chained in a linked
 * list.
 */

/* The struct to hold a data entry */
typedef struct _hash_entry
{
    int id;                     /* The ID */
    u32b account;               /* Account ID */
    char *name;                 /* Player name */
    hturn death_turn;           /* Time of death */
    struct _hash_entry *next;   /* Next entry in the chain */
} hash_entry;

/* Lookup functions */
extern hash_entry *lookup_player(int id);
extern hash_entry *lookup_player_by_name(const char *name);

/* Information about a "party" */
typedef struct
{
    char name[NORMAL_WID];  /* Name of the party */
    char owner[20];         /* Owner's name */
    s32b num;               /* Number of people in the party */
    hturn created;          /* Creation (or disband) time */
} party_type;

/*
 * Maximum number of parties to allow.  If, while trying to create a new
 * party, you get a "No empty party slot" or somesuch message, increase
 * this number.  However, you should NEVER decrease this number after a
 * server has been run, or all sorts of bad things could happen.
 */
#define MAX_PARTIES 256

/* PvP modes */
#define PVP_CHECK_ONE   0
#define PVP_CHECK_BOTH  1
#define PVP_DIRECT      2
#define PVP_INDIRECT    3
#define PVP_ADD         4
#define PVP_REMOVE      5

#define PVP_BRUTAL      0
#define PVP_DANGEROUS   1
#define PVP_NORMAL      2
#define PVP_SAFE        3
#define PVP_DISABLED    4

extern party_type parties[MAX_PARTIES];

extern struct player *player_lookup(const char *name);
extern bool player_in_party(int party_id, struct player *p);
extern bool in_party(struct player *p, int party_id);
extern bool is_party_owner(struct player *p, struct player *q);
extern bool master_in_party(s16b p1_id, s16b p2_id);
extern void party_leave(struct player *p);
extern void party_msg_format(int party_id, const char *fmt, ...);
extern bool party_share_with(struct player *p, int party_id, struct player *q);
extern void party_exp_gain(struct player *p, int party_id, s32b amount);
extern bool pvp_check(struct player *attacker, struct player *target, int mode, bool silent,
    byte feat);
extern bool pvm_check(struct player *p, struct monster *mon);
extern void party_msg_near(struct player *p, const char *msg);
extern void add_player_name(int id, u32b account, const char *name, hturn *death_turn);
extern void remove_player_name(const char *name);
extern void delete_player_name(const char *name);
extern u32b player_id_count(u32b account);
extern u32b player_id_list(int **list, u32b account);
extern void purge_player_names(void);
extern void wipe_player_names(void);
extern int player_expiry(hturn *death_turn);

#endif /* INCLUDED_PARTY_H */
