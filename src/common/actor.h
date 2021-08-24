/*
 * File: actor.h
 * Purpose: Actor definition
 */

#ifndef ACTOR_H
#define ACTOR_H

/* An "actor" structure defining either a monster or a player */

struct actor
{
    int idx;
    struct player *player;
    struct monster *mon;
};

/* Setters */

#define ACTOR_PLAYER(A, I, P) \
    (A)->idx = (I); \
    (A)->player = (P); \
    (A)->mon = NULL

#define ACTOR_MONSTER(A, M) \
    (A)->idx = (M)->midx; \
    (A)->player = NULL; \
    (A)->mon = (M)

#define ACTOR_WHO(A, I, P, M) \
    (A)->idx = abs(I); \
    (A)->player = (((I) < 0)? (P): NULL); \
    (A)->mon = (((I) > 0)? (M): NULL)

#define ACTOR_BOTH(A, P, M) \
    (A)->idx = 0; \
    (A)->player = (P); \
    (A)->mon = (M)

/* Macros */

#define ACTOR_NULL(A) \
    (((A) == NULL) || (((A)->player == NULL) && ((A)->mon == NULL)))

#define ACTOR_EQUAL(A1, A2) \
    (((A1)->player == (A2)->player) && ((A1)->mon == (A2)->mon))

/* An "actor race" structure defining either a monster race or a player ID */

struct actor_race
{
    struct player *player;
    struct monster_race *race;
};

/* Macros */

#define ACTOR_RACE_NULL(A) \
    (((A) == NULL) || (((A)->player == NULL) && ((A)->race == NULL)))

#define ACTOR_RACE_EQUAL(A1, A2) \
    ((A1)->race && ((A1)->race == (A2)->race))

#define ACTOR_PLAYER_EQUAL(A1, A2) \
    ((A1)->player && ((A1)->player == (A2)->player))

#endif /* ACTOR_H */
