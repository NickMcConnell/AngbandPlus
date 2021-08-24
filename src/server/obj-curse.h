/*
 * File: obj-curse.h
 * Purpose: Functions to deal with object curses
 */

#ifndef INCLUDED_OBJ_CURSE_H
#define INCLUDED_OBJ_CURSE_H

extern int lookup_curse(const char *name);
extern void copy_curses(struct object *obj, int *source);
extern bool curses_are_equal(const struct object *obj1, const struct object *obj2);
extern int append_object_curse(struct object *obj, int lev, int tval);
extern int append_artifact_curse(struct artifact *art, int lev, int tval);
extern bool do_curse_effect(struct player *p, int i);
extern bool append_curse(struct object *obj, struct object *source, int i);

#endif /* INCLUDED_OBJ_CURSE_H */
