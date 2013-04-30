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

#endif /* INCLUDED_PARTY_H */
