/*
 * File: effects.h
 * Purpose: List of effect types
 */

#ifndef INCLUDED_EFFECTS_H
#define INCLUDED_EFFECTS_H

/* Types of effect */
typedef enum
{
    #define EFFECT(x, y, r, z) EF_##x,
    #include "list-effects.h"
    #undef EFFECT
    EF_MAX
} effect_type;

/* Functions */
extern bool effect_do(struct player *p, effect_type effect, bool *ident, bool aware, int dir,
    int beam, int boost);
extern bool effect_aim(effect_type effect);
extern u16b effect_power(effect_type effect);
extern const char *effect_desc(effect_type effect);
extern bool effect_obvious(effect_type effect);
extern bool effect_wonder(struct player *p, int dir, int die, int beam);
extern void effect_light(struct player *p, bool *ident);
extern void effect_serious(struct player *p, bool *ident);
extern void effect_critical(struct player *p, bool *ident);
extern void effect_mortal(struct player *p, bool *ident);
extern void effect_heal(struct player *p, bool *ident);
extern void effect_hero(struct player *p, int dur, bool *ident);
extern void effect_berserk(struct player *p, int dur, bool *ident);

#endif /* INCLUDED_EFFECTS_H */
