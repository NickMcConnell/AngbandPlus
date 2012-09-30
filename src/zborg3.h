/* File: zborg3.h */
/* Purpose: Header file for "borg3.c" -BEN- */

#ifndef INCLUDED_BORG3_H
#define INCLUDED_BORG3_H

#include "angband.h"

#ifdef ALLOW_BORG

/*
 * This file provides support for "borg3.c".
 */

#include "zborg1.h"

/*
 * Determine if a given inventory item is "known"
 * Test One -- Check for special "known" tag
 * Test Two -- Check for "Easy Know" + "Aware"
 * Test Three -- Check for Mind Blast attack, which loses 'awareness'.
 */
#define borg_obj_known_p(T) \
    ((((T)->info & (OB_KNOWN)) || \
     ((T)->k_idx && k_info[(T)->k_idx].easy_know)) && \
	 !((T)->k_idx && k_info[(T)->k_idx].flavor && !k_info[(T)->k_idx].aware))

/*
 * Is the object fully known?
 */
#define borg_obj_known_full(T) \
	((T)->info & (OB_MENTAL))

/*
 * Is the object an ego item or artifact?
 */
#define borg_obj_is_ego_art(T) \
	((T)->xtra_name && (*(T)->xtra_name))

/* Macro to determine if the borg can use a certain realm */
#define borg_has_realm(realm)	((bp_ptr->realm1 == (realm)) ? TRUE : \
								(bp_ptr->realm2 == (realm)) ? TRUE : FALSE)

/*
 * Spell method values
 */

#define BORG_MAGIC_ICK      0	/* Spell is illegible */
#define BORG_MAGIC_NOP      1	/* Spell takes no arguments */
#define BORG_MAGIC_EXT      2	/* Spell needs 'space' after cast */
#define BORG_MAGIC_AIM      3	/* Spell requires a direction */
#define BORG_MAGIC_OBJ      4	/* Spell requires a pack object */
#define BORG_MAGIC_WHO      5	/* Spell requires a monster symbol */


/*
 * Spell status values
 */

#define BORG_MAGIC_ICKY     0	/* Spell is illegible */
#define BORG_MAGIC_LOST     1	/* Spell is forgotten */
#define BORG_MAGIC_HIGH     2	/* Spell is high level */
#define BORG_MAGIC_OKAY     3	/* Spell is learnable */
#define BORG_MAGIC_TEST     4	/* Spell is untried */
#define BORG_MAGIC_KNOW     5	/* Spell is known */

/*
 * Define some MindCraft Spells
 */
#define MIND_NEURAL_BL   0
#define MIND_PRECOGNIT   1
#define MIND_MINOR_DISP  2
#define MIND_MAJOR_DISP  3
#define MIND_DOMINATION  4
#define MIND_PULVERISE   5
#define MIND_CHAR_ARMOUR 6
#define MIND_PSYCHOMETRY 7
#define MIND_MIND_WAVE   8
#define MIND_ADRENALINE  9
#define MIND_PSYCHIC_DR	10
#define MIND_TELE_WAVE	11


/*
 * Forward declare
 */
typedef struct borg_magic borg_magic;


/*
 * A spell/prayer in a book
 */
struct borg_magic
{
	cptr name;	/* Textual name */

	cptr realm_name;	/* Text name of realm */

	byte realm;	/* number Realm, see defines.h */

	byte status;	/* Status (see above) */

	byte method;	/* Method (see above) */

	byte rating;	/* Usefulness */

	byte level;	/* Required level */

	byte power;	/* Required power */

	byte sfail;	/* Minimum chance of failure */

	byte cheat;	/* Actual "spell index" (or 99) */

	s32b times;	/* Times this spell was cast */
};

/*
 * A spell/prayer in a book
 */

typedef struct borg_mind borg_mind;

struct borg_mind
{
	cptr name;	/* Textual name */

	byte level;	/* Required level */

	byte power;	/* Required power --mana cost */

	byte sfail;	/* Minimum chance of failure */

	char letter;	/* Actual "spell index" (a,b,c...) */

	s32b times;	/* Times this spell was cast */
};

/*
 * Spell casting information
 */

extern borg_magic borg_magics[8][4][8];	/* Spell info, including realm */
extern borg_mind borg_minds[MINDCRAFT_MAX];


/* Functions */

extern int borg_wield_slot(list_item *item);
extern int borg_count(int tval, int sval);
extern list_item *borg_slot(int tval, int sval);
extern int borg_slot_from(int tval, int sval, int from);
extern object_kind *borg_get_kind(int tval, int sval);
extern int look_up_index(list_item *l_ptr);
extern bool borg_refuel_torch(void);
extern bool borg_refuel_lantern(void);
extern bool borg_eat_food(int sval);
extern bool borg_quaff_crit(bool no_check);
extern bool borg_quaff_potion(int sval);
extern bool borg_eat_unknown(void);
extern bool borg_use_unknown(void);
extern bool borg_quaff_unknown(void);
extern bool borg_read_unknown(void);
extern bool borg_read_scroll_fail(int sval);
extern bool borg_read_scroll(int sval);
extern bool borg_use_item_fail(list_item *l_ptr, bool risky);
extern bool borg_equips_rod_fail(int sval);
extern bool borg_equips_rod(int sval);
extern bool borg_zap_rod(int sval);
extern bool borg_aim_wand(int sval);
extern bool borg_equips_wand_fail(int sval);
extern bool borg_use_staff(int sval);
extern bool borg_use_staff_fail(int sval);
extern bool borg_equips_staff_fail(int sval);
extern bool borg_equips_staff(int sval);
extern bool borg_activate_artifact(int name1, bool secondary);	/* apw */
extern bool borg_activate_rand_art(int effect);
extern bool borg_check_artifact(list_item *l_ptr, bool real_use);

extern int borg_reserve_mana(void);
extern byte borg_spell_mana(int realm, int book, int spell);
extern bool borg_spell_legal(int realm, int book, int what);
extern bool borg_spell_okay(int realm, int book, int what);
extern int  borg_spell_fail_rate(int realm, int book, int what);
extern bool borg_spell(int realm, int book, int what);
extern bool borg_spell_fail(int realm, int book, int what, int allow_fail);
extern bool borg_spell_okay_fail(int realm, int book, int what, int allow_fail);
extern bool borg_spell_legal_fail(int realm, int book, int what,
								  int allow_fail);
extern bool borg_uses_book(int realm, int book);
extern bool borg_mindcr_legal(int spell, int level);
extern bool borg_mindcr_okay(int spell, int level);
extern int  borg_mindcr_fail_rate(int spell, int level);
extern bool borg_mindcr(int spell, int level);
extern bool borg_mindcr_fail(int spell, int level, int allow_fail);
extern bool borg_mindcr_okay_fail(int spell, int level, int allow_fail);
extern bool borg_mindcr_legal_fail(int spell, int level, int allow_fail);
extern bool borg_racial_check(int race, bool check_fail);
extern bool borg_racial(int race);
extern int  borg_count_racial(int race);
extern bool borg_mutation_check(u32b mutation, bool check_fail);
extern bool borg_mutation(u32b mutation);
extern void borg_cheat_spell(int realm);
extern void prepare_race_class_info(void);

/*
 * Initialize this file
 */
extern void borg_init_3(void);


#endif

#endif
