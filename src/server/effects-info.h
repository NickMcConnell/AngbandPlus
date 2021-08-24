/*
 * File: effects-info.h
 * Purpose: Implement interfaces for displaying information about effects
 */


#ifndef EFFECTS_INFO_H
#define EFFECTS_INFO_H

extern void print_effect(struct player *p, const char *d);
extern bool effect_describe(struct player *p, const struct object *obj, const struct effect *e);

#endif /* EFFECTS_INFO_H */
