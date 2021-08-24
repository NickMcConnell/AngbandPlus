/*
 * File: object.h
 * Purpose: Basic object structs and enums
 */

#ifndef INCLUDED_OBJECT_H
#define INCLUDED_OBJECT_H

/*** Game constants ***/

/* Object origin kinds */
enum
{
    #define ORIGIN(a, b, c) ORIGIN_##a,
    #include "list-origins.h"
    #undef ORIGIN

    ORIGIN_MAX
};

/* Object inscription kinds */
enum
{
    INSCRIPTION_FIRE = 0,
    INSCRIPTION_THROW,
    INSCRIPTION_DROP,
    INSCRIPTION_DESTROY,
    INSCRIPTION_STUDY,
    INSCRIPTION_CAST,
    INSCRIPTION_EAT,
    INSCRIPTION_QUAFF,
    INSCRIPTION_READ,
    INSCRIPTION_USE,
    INSCRIPTION_AIM,
    INSCRIPTION_ZAP,
    INSCRIPTION_ACTIVATE,
    INSCRIPTION_UP,
    INSCRIPTION_DOWN,
    INSCRIPTION_CLOSE,
    INSCRIPTION_TUNNEL,
    INSCRIPTION_DISARM,
    INSCRIPTION_ALTER,
    INSCRIPTION_WALK,
    INSCRIPTION_RUN,
    INSCRIPTION_HOUSE,
    INSCRIPTION_STEAL,
    INSCRIPTION_FOUNTAIN,
    INSCRIPTION_RETALIATE,
    INSCRIPTION_PICKUP,
    INSCRIPTION_TAKEOFF,
    INSCRIPTION_WIELD,
    INSCRIPTION_REFILL,
    INSCRIPTION_PURCHASE,
    INSCRIPTION_SELL,
    INSCRIPTION_INSCRIBE,
    INSCRIPTION_UNINSCRIBE,

    INSCRIPTION_MAX
};

/*
 * Special return codes corresponding to item request.
 */
#define ITEM_REQUEST    102
#define ITEM_PENDING    103

/*** Macros ***/

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_attr_default(kind) \
    (kind->flavor? kind->flavor->d_attr: kind->d_attr)

/*
 * Is that object shimmering?
 */
#define object_shimmer(T) \
    (object_attr_default((T)->kind) == COLOUR_MULTI)

/*
 * True artifacts
 */
#define true_artifact_p(T) \
    ((T)->artifact && !(T)->randart_seed)

/*
 * Worthless items
 */
#define worthless_p(T) \
    (object_value_real(NULL, T, 1) < 1)

#define MAX_WEAPON_DICE 11
#define MAX_AMMO_DICE   9

/*** Structures ***/

extern struct object_base *kb_info;
extern struct artifact *a_info;
extern struct flavor *flavors;

#endif /* INCLUDED_OBJECT_H */
