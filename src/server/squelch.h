/*
 * File: squelch.h
 * Purpose: Squelch interface
 */

#ifndef SQUELCH_H
#define SQUELCH_H

/* squelch.c */
extern bool squelch_item_ok(struct player *p, const object_type *o_ptr);
extern void squelch_drop(struct player *p);

#endif /* SQUELCH_H */
