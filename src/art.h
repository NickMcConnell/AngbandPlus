#ifndef INCLUDED_ART_H
#define INCLUDED_ART_H

#include "obj.h"

/* Create a standard artifact as defined in ../lib/edit/a_info.txt (a_info).
 * obj will be wiped and prepped as the appropriate kind. */
extern bool art_create_std(obj_ptr obj, art_ptr art, int mode);

/* Create a random artifact. obj should already be prepped, but should not
 * have been enchanted yet. We'll nuke any magic if it has and start a new
 * artifact from scratch. Use AM_CRAFTING for ?ArtifactCreation. Use AM_CURSED
 * to force something "bad". */
extern void art_create_random(obj_ptr obj, int level, int mode);

/* Create a random artifact by extending an ego item. This will give better
 * results than art_create_random since egos usually provide a themed core item.
 * This is a helper for ego.c */
extern void art_create_ego(obj_ptr obj, int level, int mode);

/* Create a replacement artifact based on the standard. The replacement is
 * truly random, but we try to match the quality of the original. obj will
 * be wiped and prepped as the appropriate kind. */
extern bool art_create_replacement(obj_ptr obj, art_ptr art, int mode);

/* Reforge an artifact into a new nameless item, not necessarily of the same
 * kind. Power is matched ... sort of ... but there is no effort to maintain
 * the flavor of the original. Results are unreliable and generally you
 * get an object worse than the original. This should probably be removed. */
extern bool art_reforge(obj_ptr src, obj_ptr dest, int fame);

extern int get_slot_power(obj_ptr obj);

/* art_name.c
 * Naming artifacts should avoid duplicate names as much as possible.
 * However, many places in the code create artifacts inside a loop
 * in order to enforce object quality restrictions, and these would
 * quickly exhaust all the names if we were not careful. The downside
 * is that clients will have to call art_remember_name() manually to
 * avoid duplicates. */
extern cptr art_get_name(obj_ptr obj, int bias);
extern cptr art_get_name_ego(vec_ptr names);
extern void art_remember_name(cptr name);
extern int  art_name_count(cptr name);

extern void art_names_reset(void); /* player_wipe */
extern void art_names_save(savefile_ptr file);
extern void art_names_load(savefile_ptr file);

/* a_info: global list of all artifacts parsed from a_info.txt */
extern art_ptr art_alloc(sym_t id);
extern void    art_free(art_ptr a);
extern bool    arts_init(void); /* init_angband */
extern art_ptr arts_lookup(sym_t id);
extern art_ptr arts_parse(cptr token);
extern void    arts_reset(void); /* player_wipe */
extern void    arts_save(savefile_ptr file);
extern void    arts_load(savefile_ptr file);
extern vec_ptr arts_filter(obj_ptr obj);
extern vec_ptr arts_filter_special(void);
extern vec_ptr arts_filter_ex(bool (*f)(int id, art_ptr art));

#endif
