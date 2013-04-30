/*
 * File: slays.h
 * Purpose: Structures and functions for dealing with slays and brands
 */

#ifndef INCLUDED_SLAYS_H
#define INCLUDED_SLAYS_H

/* Types of slay (including brands) */
typedef enum
{
    #define SLAY(a, b, c, d, e, f, g, h, i, j, k, l)    SL_##a,
    #include "list-slays.h"
    #undef SLAY
    SL_MAX
} slay_type;

/*
 * Slay type.  Used for the table of brands/slays and their effects.
 */
struct slay
{
    u16b index;                 /* Numerical index */
    int object_flag;            /* Object flag for the slay */
    int monster_flag;           /* Which monster flag(s) make it vulnerable */
    int resist_flag;            /* Which monster flag(s) make it resist */
    int mult;                   /* Slay multiplier */
    const char *range_verb;     /* Attack verb for ranged hits */
    const char *melee_verb;     /* Attack verb for melee hits */
    const char *active_verb;    /* Verb for when the object is active */
    const char *desc;           /* Description of vulnerable creatures */
    const char *brand;          /* Name of brand */
    int esp_chance;             /* Chance of ESP */
    int esp_flag;               /* ESP flag for the slay */
};

/*** Functions ***/

extern int dedup_slays(bitflag *flags);
extern const struct slay *random_slay(const bitflag mask[OF_SIZE]);
extern int list_slays(const bitflag flags[OF_SIZE], const bitflag mask[OF_SIZE],
    const char *desc[], const char *brand[], int mult[], bool dedup);
extern void object_notice_slays(struct player *p, object_type *o_ptr,
    const bitflag mask[OF_SIZE]);
extern void improve_attack_modifier(struct player *p, object_type *o_ptr, int m_idx,
    bool is_ranged, int r_idx, const struct slay **best_s_ptr, bool known_only);
extern void react_to_slay(bitflag *obj_flags, bitflag *mon_flags);
extern s32b check_slay_cache(bitflag *index);
extern void fill_slay_cache(bitflag *index, s32b value);
extern void create_slay_cache(void);
extern void free_slay_cache(void);

#endif /* INCLUDED_SLAYS_H */
