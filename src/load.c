/* File: load.c */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"


/*
 * This file loads savefiles from Angband 2.8.X
 *
 * Ancient savefiles (pre-2.8.0) are loaded by another file.
 *
 * Note that Angband 2.7.0 through 2.7.8 are now officially obsolete,
 * and savefiles from those versions may not be successfully converted.
 *
 * We attempt to prevent corrupt savefiles from inducing memory errors.
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 *
 * This file assumes that the monster/object records are initialized
 * to zero, and the race/kind tables have been loaded correctly.  The
 * order of object stacks is currently not saved in the savefiles, but
 * the "next" pointers are saved, so all necessary knowledge is present.
 *
 * We should implement simple "savefile extenders" using some form of
 * "sized" chunks of bytes, with a {size,type,data} format, so everyone
 * can know the size, interested people can know the type, and the actual
 * data is available to the parsing routines that acknowledge the type.
 *
 * Consider changing the "globe of invulnerability" code so that it
 * takes some form of "maximum damage to protect from" in addition to
 * the existing "number of turns to protect for", and where each hit
 * by a monster will reduce the shield by that amount.
 *
 * XXX XXX XXX
 */



/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100


/*
 * Local "savefile" pointer
 */
static FILE *fff;

/*
 * Hack -- old "encryption" byte
 */
static byte xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b x_check = 0L;


/*
 * The above function, adapted for Zangband
 */
static bool z_older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
	if (z_major < x) return (TRUE);
	if (z_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (z_minor < y) return (TRUE);
	if (z_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (z_patch < z) return (TRUE);
	if (z_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}


/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, 0, y);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
static bool wearable_p(const object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Hack -- determine if an item is a "weapon" (or a missile)
 */
static bool is_weapon(const object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info for 2.7.0+
 */

static byte sf_get(void)
{
	byte c, v;

	/* Get a character, decode the value */
	c = getc(fff) & 0xFF;
	v = c ^ xor_byte;
	xor_byte = c;

	/* Maintain the checksum info */
	v_check += v;
	x_check += xor_byte;

	/* Return the value */
	return (v);
}

static void rd_byte(byte *ip)
{
	*ip = sf_get();
}

static void rd_u16b(u16b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u16b)(sf_get()) << 8);
}

static void rd_s16b(s16b *ip)
{
	rd_u16b((u16b *)ip);
}

static void rd_u32b(u32b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u32b)(sf_get()) << 8);
	(*ip) |= ((u32b)(sf_get()) << 16);
	(*ip) |= ((u32b)(sf_get()) << 24);
}

static void rd_s32b(s32b *ip)
{
	rd_u32b((u32b *)ip);
}


/*
 * Hack -- read a string
 */
static void rd_string(char *str, int max)
{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
	{
		byte tmp8u;

		/* Read a byte */
		rd_byte(&tmp8u);

		/* Collect string while legal */
		if (i < max) str[i] = tmp8u;

		/* End of string */
		if (!tmp8u) break;
	}

	/* Terminate */
	str[max - 1] = '\0';
}


/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	byte tmp8u;

	/* Strip the bytes */
	while (n--) rd_byte(&tmp8u);
}


/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 *
 * Note that Angband 2.7.9 introduced a new method for object "flags"
 * in which the "flags" on an object are actually extracted when they
 * are needed from the object kind, artifact index, ego-item index,
 * and two special "xtra" fields which are used to encode any "extra"
 * power of certain ego-items.  This had the side effect that items
 * imported from pre-2.7.9 savefiles will lose any "extra" powers they
 * may have had, and also, all "uncursed" items will become "cursed"
 * again, including Calris, even if it is being worn at the time.  As
 * a complete hack, items which are inscribed with "uncursed" will be
 * "uncursed" when imported from pre-2.7.9 savefiles.
 *
 * Zangband 2.5.3 changed the ego and artifact interface from the object
 * data type.  The new system makes each treatable like the random
 * artifacts.  This increases memory usage, but simplifies large amounts
 * of code.  It also makes some effects possible that would otherwise
 * require slow script hooks.
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	byte tmpbyte;
	s16b tmps16b;

	object_kind *k_ptr;

	char buf[128];

	/* Old flags from pre [Z] 2.5.3 */
	byte name1, name2, xtra1, xtra2;


	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	if (sf_version < 6)
	{
		/* Location */
		rd_byte(&tmpbyte);
		o_ptr->iy = tmpbyte;
		rd_byte(&tmpbyte);
		o_ptr->ix = tmpbyte;
	}
	else
	{
		/* Location */
		rd_s16b(&o_ptr->iy);
		rd_s16b(&o_ptr->ix);
	}

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	/* New method - old method removed. */
	rd_byte(&o_ptr->discount);
	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);

	if (sf_version < 19)
	{
		/* Old ego and artifact number */
		rd_byte(&name1);
		rd_byte(&name2);
	}

	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&old_dd);
	rd_byte(&old_ds);

	rd_byte(&o_ptr->info);

	if (sf_version < 35)
	{
		/* Old "marked" flag */
		rd_byte(&tmpbyte);

		if (tmpbyte) o_ptr->info |= OB_SEEN;
	}

	/* Old flags */
	rd_u32b(&o_ptr->flags1);
	rd_u32b(&o_ptr->flags2);
	rd_u32b(&o_ptr->flags3);

	/* Lites changed in [Z] 2.6.0 */
	if ((sf_version < 25) && (o_ptr->tval == TV_LITE))
	{
		/* Torches and lanterns use timeout now */
		if ((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN))
		{
			o_ptr->timeout = o_ptr->pval;
			o_ptr->pval = 0;
		}
		else
		{
			/* Other lites are everburning. */
			o_ptr->flags3 |= TR3_LITE;
		}
	}

	if (sf_version > 30)
	{
		/* Link to next object in the list */
		rd_s16b(&o_ptr->next_o_idx);
	}

	if (sf_version < 36)
	{
		/* Monster holding object */
		rd_s16b(&tmps16b);

		if (tmps16b)
		{
			o_ptr->allocated = TRUE;
		}
		else
		{
			o_ptr->allocated = FALSE;
		}
	}
	else
	{
		rd_byte((byte *)(&o_ptr->allocated));
	}

	if (sf_version < 19)
	{
		/* Special powers */
		rd_byte(&xtra1);
		rd_byte(&xtra2);
	}

	/* Feeling - from 2.3.1, "savefile version 1" */
	if (sf_version >= 1)
	{
		rd_byte(&o_ptr->feeling);
	}

	/* Inscription */
	rd_string(buf, 128);

	/* If this savefile is old, maybe we need to translate the feeling */
	if (sf_version < 1)
	{
		byte i;

		for (i = 0; i <= 9; i++)
		{
			if (game_inscriptions[i] == NULL)
			{
				continue;
			}

			if (streq(buf, game_inscriptions[i]))
			{
				o_ptr->feeling = i;
				buf[0] = 0;
				break;
			}
		}
	}

	/* Save the inscription */
	if (buf[0]) o_ptr->inscription = quark_add(buf);

	rd_string(buf, 128);
	if (buf[0]) o_ptr->xtra_name = quark_add(buf);

	/* The Python object */
	if (!z_older_than(2, 2, 4))
	{
		s32b tmp32s;

		rd_s32b(&tmp32s);
		strip_bytes(tmp32s);
	}

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* For rod-stacking */
	if (z_older_than(2, 2, 5) && (o_ptr->tval == TV_ROD))
	{
		o_ptr->timeout = o_ptr->pval * o_ptr->number;
		o_ptr->pval = k_ptr->pval * o_ptr->number;
	}

	/* Mega-Hack... Corpses became fields */
	if ((o_ptr->tval == 10) && (sf_version < 15))
	{
		/* Hack - get rid of it. */
		o_ptr->k_idx = 0;
		return;
	}

	/* The new flags as of [Z] 2.5.3 */
	if (sf_version > 18)
	{
		/* The new flags */
		rd_s32b(&o_ptr->cost);

		rd_byte(&o_ptr->activate);

		rd_u32b(&o_ptr->kn_flags1);
		rd_u32b(&o_ptr->kn_flags2);
		rd_u32b(&o_ptr->kn_flags3);
	}
	else
	{
		/* Set the cost to something reasonable */
		o_ptr->cost = k_ptr->cost;
	}

	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr))
	{
		/* Acquire correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Acquire correct fields */
		if (o_ptr->tval != TV_WAND)
		{
			/* Hack - Wands count "used" charges */
			o_ptr->ac = k_ptr->ac;
		}
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Acquire correct weight */
		o_ptr->weight = k_ptr->weight;

		/* Paranoia */
		o_ptr->activate = 0;

		/* Reset flags */
		o_ptr->flags1 = k_ptr->flags1;
		o_ptr->flags2 = k_ptr->flags2;
		o_ptr->flags3 = k_ptr->flags3;

		/* All done */
		return;
	}

	/* Hack -- extract the "broken" flag */
	if (o_ptr->pval < 0) o_ptr->cost = 0;

	if (sf_version < 19)
	{
		/* Convert old ego items to current format */
		if (name2)
		{
			/* Obtain the ego-item info */
			ego_item_type *e_ptr = &e_info[name2];

			/* Use that ego-item */
			if (e_ptr->name)
			{
				/* Save the flags */
				add_ego_flags(o_ptr, name2);

				/* Keep the damage dice */
				o_ptr->dd = old_dd;
				o_ptr->ds = old_ds;

				if (name2 == EGO_TRUMP)
				{
					/* Mega-Hack -- set activation */
					o_ptr->activate = ACT_TELEPORT_1;
				}

				/* Change the price */
				if (!e_ptr->cost)
				{
					o_ptr->cost = 0L;
				}
				else
				{
					o_ptr->cost += e_ptr->cost;
				}

				/* Note: the xtra1 value is ignored here. */
			}
		}

		/* Convert old artifacts to current format */
		else if (name1)
		{
			artifact_type *a_ptr;

			/* Obtain the artifact info */
			a_ptr = &a_info[name1];

			/* Acquire new artifact "pval" */
			o_ptr->pval = a_ptr->pval;

			/* Acquire new artifact fields */
			o_ptr->ac = a_ptr->ac;
			o_ptr->dd = a_ptr->dd;
			o_ptr->ds = a_ptr->ds;

			/* Acquire new artifact weight */
			o_ptr->weight = a_ptr->weight;

			/* Save the artifact flags */
			o_ptr->flags1 |= a_ptr->flags1;
			o_ptr->flags2 |= a_ptr->flags2;
			o_ptr->flags3 |= a_ptr->flags3;

			/* Mega-Hack -- set activation */
			o_ptr->activate = name1 + 128;

			/* Save the inscription */
			o_ptr->xtra_name = quark_add(a_name + a_ptr->name);

			/* Set the cost */
			if (!a_ptr->cost)
			{
				/* Hack -- "worthless" artifacts */
				o_ptr->cost = 0L;
			}
			else
			{
				/* Hack - use the artifact price */
				o_ptr->cost = a_ptr->cost;
			}
		}
		/* Convert Random artifacts */
		else if (o_ptr->xtra_name)
		{
			o_ptr->activate = xtra2;

			/* Make the object an artifact */
			o_ptr->flags3 |= TR3_INSTA_ART;

			/* Set the cost */
			o_ptr->cost = k_info[o_ptr->k_idx].cost +
				flag_cost(o_ptr, o_ptr->pval);
		}
		/* Convert normal items */
		else
		{
			/* Set cost for normal items */
			o_ptr->cost = k_info[o_ptr->k_idx].cost;
		}

		/* Identification status */
		if (o_ptr->info & (OB_MENTAL))
		{
			o_ptr->kn_flags1 = o_ptr->flags1;
			o_ptr->kn_flags2 = o_ptr->flags2;
			o_ptr->kn_flags3 = o_ptr->flags3;
		}
	}

	if (o_ptr->xtra_name)		/* Artifacts and ego items */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}
	else
	{
		/* Acquire standard fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Acquire standard weight */
		o_ptr->weight = k_ptr->weight;


		/* Obtain tval/sval from k_info */
		o_ptr->tval = k_ptr->tval;
		o_ptr->sval = k_ptr->sval;
	}

	/* Change shattered weapons from 0d0 to 1d1 */
	if (is_weapon(o_ptr))
	{
		if (o_ptr->dd == 0) o_ptr->dd = 1;
		if (o_ptr->ds == 0) o_ptr->ds = 1;
	}
}




/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	byte tmp8u;

	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

	/* Read the other information */
	if (sf_version < 6)
	{
		/* Location */
		rd_byte(&tmp8u);
		m_ptr->fy = tmp8u;
		rd_byte(&tmp8u);
		m_ptr->fx = tmp8u;
	}
	else
	{
		/* Location */
		rd_s16b(&m_ptr->fy);
		rd_s16b(&m_ptr->fx);
	}

	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);

	/* Monster invulnerability introduced from 2.3.2+ */
	if (sf_version < 2)
		m_ptr->invulner = 0;
	else
		rd_byte(&m_ptr->invulner);

	if (!(z_major == 2 && z_minor == 0 && z_patch == 6))
		rd_u32b(&m_ptr->smart);
	else
		m_ptr->smart = 0;

	if (sf_version < 31)
	{
		rd_byte(&tmp8u);
	}
	else
	{
		rd_s16b(&m_ptr->hold_o_idx);
	}
}



static void rd_field(field_type *f_ptr)
{
	s32b tmp32s;
	int i;

	s16b t_idx;

	/* Type */
	rd_s16b(&t_idx);

	/* Prepare the field */
	field_prep(f_ptr, t_idx);

	/* Location */
	rd_s16b(&f_ptr->fy);
	rd_s16b(&f_ptr->fx);

	/* Info flags */
	rd_u16b(&f_ptr->info);

	/* Counter */
	rd_s16b(&f_ptr->counter);

	/* Data */
	for (i = 0; i < 8; i++)
	{
		rd_byte(&f_ptr->data[i]);
	}

	rd_s32b(&tmp32s);
	strip_bytes(tmp32s);
}


/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];

	/* Pre-2.2.0 (old r_info.txt) */
	if (z_older_than(2, 2, 0))
	{
		/* Throw away old info */
		strip_bytes(48);
	}

	/* Current */
	else
	{
		/* Count sights/deaths/kills */
		rd_s16b(&r_ptr->r_sights);
		rd_s16b(&r_ptr->r_deaths);
		rd_s16b(&r_ptr->r_pkills);
		rd_s16b(&r_ptr->r_tkills);

		/* Count wakes and ignores */
		rd_byte(&r_ptr->r_wake);
		rd_byte(&r_ptr->r_ignore);

		/* Extra stuff */
		rd_byte(&r_ptr->r_xtra1);
		rd_byte(&r_ptr->r_xtra2);

		/* Count drops */
		rd_byte(&r_ptr->r_drop_gold);
		rd_byte(&r_ptr->r_drop_item);

		/* Count spells */
		rd_byte(&r_ptr->r_cast_inate);
		rd_byte(&r_ptr->r_cast_spell);

		/* Count blows of each type */
		rd_byte(&r_ptr->r_blows[0]);
		rd_byte(&r_ptr->r_blows[1]);
		rd_byte(&r_ptr->r_blows[2]);
		rd_byte(&r_ptr->r_blows[3]);

		/* Memorize flags */
		rd_u32b(&r_ptr->r_flags1);
		rd_u32b(&r_ptr->r_flags2);
		rd_u32b(&r_ptr->r_flags3);
		rd_u32b(&r_ptr->r_flags4);
		rd_u32b(&r_ptr->r_flags5);
		rd_u32b(&r_ptr->r_flags6);


		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Later (?) */
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Repair the lore flags */
	r_ptr->r_flags1 &= r_ptr->flags1;
	r_ptr->r_flags2 &= r_ptr->flags2;
	r_ptr->r_flags3 &= r_ptr->flags3;
	r_ptr->r_flags4 &= r_ptr->flags4;
	r_ptr->r_flags5 &= r_ptr->flags5;
	r_ptr->r_flags6 &= r_ptr->flags6;
}




/*
 * Read a store
 */
static void rd_store(int town_num, int store_num)
{
	store_type *st_ptr = &place[town_num].store[store_num];

	int j;

	s16b data;

	byte allocated, owner, type = 0;

	/* Read the basic info */
	if (sf_version < 34)
	{
		strip_bytes(4);
	}

	rd_s16b(&data);
	rd_byte(&owner);
	rd_byte(&allocated);

	if (sf_version < 34)
	{
		strip_bytes(4);
	}

	if (sf_version > 20)
	{
		rd_u16b(&st_ptr->x);
		rd_u16b(&st_ptr->y);

		/* Hack - only listen to 'type' in recent savefiles */
		rd_byte(&type);
	}


	/* Hack - Initialise the store (even if not really a store) */
	store_init(town_num, store_num, type);


	/* Restore the saved parameters */
	st_ptr->data = data;
	st_ptr->owner = owner;

	/* Read last visit */
	rd_s32b(&st_ptr->last_visit);

	/*
	 * Hack - allocate store if it has stock
	 * Note that this will change the order that
	 * stores are removed from the cache.
	 * The resulting list can be sorted... but it
	 * doesn't really matter.
	 */
	if (allocated)
	{
		(void)allocate_store(st_ptr);
	}

	if (sf_version < 38)
	{
		/* Read the items */
		for (j = 0; j < allocated; j++)
		{
			object_type forge;
			object_type *q_ptr;

			/* Get local object */
			q_ptr = &forge;

			/* Read the item */
			rd_item(q_ptr);

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Ignore the item */
		}
	}
	else
	{
		/* Get pointer to item list in o_list[] */
		rd_s16b(&st_ptr->stock);
	}
}



/*
 * Read RNG state
 */
static void rd_randomizer(void)
{
	int i;

	u16b tmp16u;

	/* Tmp */
	rd_u16b(&tmp16u);

	/* Place */
	rd_u16b(&Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		rd_u32b(&Rand_state[i]);
	}

	/* Accept */
	Rand_quick = FALSE;
}



/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(void)
{
	int i, n;

	byte b;

	u16b c;

	u32b flag[8], mask[8];


	/*** Oops ***/

	/* Ignore old options */
	strip_bytes(16);


	/*** Special info */

	/* Read "delay_factor" */
	rd_byte(&b);
	delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	hitpoint_warn = b;


	/*** Cheating options ***/

	rd_u16b(&c);

	if (c & 0x0002) p_ptr->wizard = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;

	/* Autosave options */
	rd_byte(&autosave_l);
	rd_byte(&autosave_t);
	rd_s16b(&autosave_freq);


	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the option masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			if ((mask[n] & (1L << i)) && (option_mask[n] & (1L << i)))
			{
				/* Set */
				if (flag[n] & (1L << i))
				{
					/* Set */
					option_info[n * 32 + i].o_val = TRUE;
				}

				/* Clear */
				else
				{
					/* Clear */
					option_info[n * 32 + i].o_val = FALSE;
				}
			}
		}
	}

	/* Set the options */
	init_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER | OPT_FLAG_PLAYER);


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < ANGBAND_TERM_MAX; n++) rd_u32b(&flag[n]);

	/* Read the window masks */
	for (n = 0; n < ANGBAND_TERM_MAX; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (window_mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						window_flag[n] |= (1L << i);
					}

					/* Clear */
					else
					{
						/* Clear */
						window_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}
}





/*
 * Hack -- strip the "ghost" info
 *
 * XXX XXX XXX This is such a nasty hack it hurts.
 */
static void rd_ghost(void)
{
	char buf[64];

	/* Strip name */
	rd_string(buf, 64);

	/* ghosts */

	/* Strip old data */
	strip_bytes(60);
}


static bool player_detected = FALSE;


/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i;

	byte tmp8u;
	s16b tmp16s;
	s16b dummy;

	rd_string(player_name, 32);

	rd_string(p_ptr->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(p_ptr->history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&p_ptr->realm1);
	rd_byte(&p_ptr->realm2);
	rd_byte(&tmp8u);			/* oops */

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cur[i]);

	/* Fix up stats for old savefiles */
	if (sf_version < 39)
	{
		/* This will be initialized again later, but we need it now for adjust_stat to work */
		rp_ptr = &race_info[p_ptr->prace];
		cp_ptr = &class_info[p_ptr->pclass];

		for (i = 0; i < 6; i++)
		{
			int bonus = race_info[p_ptr->prace].r_adj[i] +
				class_info[p_ptr->pclass].c_adj[i];

			p_ptr->stat_max[i] = adjust_stat(i, p_ptr->stat_max[i], bonus);
			/* Hack - Restore all stats... */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];
		}
	}

	strip_bytes(24);			/* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	rd_s16b(&p_ptr->place_num);

	/* Read arena and rewards information */
	strip_bytes(12);

	rd_s16b(&tmp16s);
	for (i = 0; i < tmp16s; i++) rd_s16b(&dummy);

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_lev);
	rd_s16b(&p_ptr->max_depth);

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

	/* Repair maximum dungeon level */
	if (p_ptr->max_depth < 0) p_ptr->max_depth = 1;

	/* More info */
	strip_bytes(8);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);				/* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4);				/* Old "food_digested" / "protection" */
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->invuln);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->shero);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);

	/* Old savefiles do not have the following fields... */
	if ((z_major == 2) && (z_minor == 0) && (z_patch == 6))
	{
		p_ptr->tim_esp = 0;
		p_ptr->wraith_form = 0;
		p_ptr->resist_magic = 0;
		p_ptr->chaos_patron = get_chaos_patron();
		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;

		get_virtues();
	}
	else
	{
		rd_s16b(&p_ptr->tim_esp);
		rd_s16b(&p_ptr->wraith_form);
		rd_s16b(&p_ptr->resist_magic);
		if (sf_version < 32)
		{
			/* Ignore unused counters */
			strip_bytes(16);
		}
		rd_s16b(&p_ptr->chaos_patron);
		rd_u32b(&p_ptr->muta1);
		rd_u32b(&p_ptr->muta2);
		rd_u32b(&p_ptr->muta3);

		if (sf_version < 5)
		{
			get_virtues();
		}
		else
		{
			for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
			{
				rd_s16b(&p_ptr->virtues[i]);
			}

			for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
			{
				rd_s16b(&p_ptr->vir_types[i]);
			}
		}
	}

	/* Calc the regeneration modifier for mutations */
	mutant_regenerate_mod = calc_mutant_regenerate_mod();

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u);			/* oops */
	rd_byte(&tmp8u);			/* oops */
	rd_byte(&tmp8u);			/* oops */
	rd_byte((byte *)&p_ptr->searching);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);

	/* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);

	if (sf_version < 24)
	{
		/* No more seed_town */
		strip_bytes(4);
	}

	/* Special stuff */
	rd_u16b(&p_ptr->panic_save);
	rd_u16b(&p_ptr->total_winner);
	rd_u16b(&p_ptr->noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	p_ptr->is_dead = tmp8u;

	/* Read "feeling" */
	rd_byte(&tmp8u);
	dun_ptr->feeling = tmp8u;

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);

	if (sf_version > 17)
	{
		/* Get trap detection status */
		rd_byte((byte *)&player_detected);

		if (sf_version < 33)
		{
			/* oops */
			strip_bytes(4);
		}
		else
		{
			rd_s16b(&p_ptr->inventory);
		}
	}
}


/*
 * Mega-Hack , in order to load old savefiles, we
 * need to save the player's inventory temporarily.
 */
static object_type old_inventory[24];

/*
 * Read the player inventory
 *
 * Note that the inventory changed in Angband 2.7.4.  Two extra
 * pack slots were added and the equipment was rearranged.  Note
 * that these two features combine when parsing old save-files, in
 * which items from the old "aux" slot are "carried", perhaps into
 * one of the two new "inventory" slots.
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(void)
{
	object_type forge;
	object_type *q_ptr = &forge;

	/* Wipe the structure */
    (void)WIPE(q_ptr, object_type);

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Read the item */
		rd_item(q_ptr);

		/* Hack - assume not allocated in o_list[] */
		q_ptr->allocated = FALSE;

		/* Hack -- verify item */
		if (!q_ptr->k_idx) return (53);

		if (sf_version > 36)
		{
			/* Wield equipment */
			if (n < EQUIP_MAX)
			{
				/* Copy object */
				swap_objects(&p_ptr->equipment[n], q_ptr);
			}
		}
		else
		{
			/* Wield equipment */
			if (n >= 24)
			{
				/* Copy object */
				swap_objects(&p_ptr->equipment[n - 24], q_ptr);
			}

			/* Carry inventory */
			else
			{
				/* Copy object into temp structure */
				swap_objects(&old_inventory[n], q_ptr);
			}
		}
	}

	/* Success */
	return (0);
}



/*
 * Read the saved messages
 */
static void rd_messages(void)
{
	int i;
	char buf[128];
	byte tmp8u;

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);

		/* Read the color */
		if (sf_version > 10)
			rd_byte(&tmp8u);
		else
			tmp8u = MSG_GENERIC;

		/* Save the message */
		message_add(buf, tmp8u);
	}
}


static void fix_tile(cave_type *c_ptr)
{
	/* Get rid of pre-fields terrain */
	if (sf_version < 17)
	{
		/* Invisible wall */
		if (c_ptr->feat == 0x5B)
		{
			/* Get rid of it */
			c_ptr->feat = FEAT_FLOOR;
		}

		/* Glyph of warding */
		if (c_ptr->feat == 0x03)
		{
			/* Get rid of it */
			c_ptr->feat = FEAT_FLOOR;
		}

		/* Explosive Rune */
		if (c_ptr->feat == 0x40)
		{
			/* Get rid of it */
			c_ptr->feat = FEAT_FLOOR;
		}

		/* Traps */
		if ((c_ptr->feat == 0x02) ||
			(c_ptr->feat >= 0x10 && c_ptr->feat <= 0x1F) ||
			(c_ptr->feat == 0x5A))
		{
			/* Get rid of it */
			c_ptr->feat = FEAT_FLOOR;
		}

		/* Doors */
		if ((c_ptr->feat > 0x20) && (c_ptr->feat < 0x28))
		{
			/* Locked door -> closed door */
			c_ptr->feat = FEAT_CLOSED;
		}

		if ((c_ptr->feat >= 0x28) && (c_ptr->feat <= 0x2F))
		{
			/* Stuck door -> closed door */
			c_ptr->feat = FEAT_CLOSED;
		}
	}
}


/*
 * Load dungeon or wilderness map
 */
static void load_map(int xmin, int ymin, int xmax, int ymax)
{
	int i, y, x;
	byte count;
	byte tmp8u;
	s16b tmp16s;

	cave_type *c_ptr;
	pcave_type *pc_ptr;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = area(x, y);
			pc_ptr = parea(x, y);

			/* Extract "info" (without the CAVE_ROOM flag set) */
			c_ptr->info = (tmp8u & (CAVE_GLOW | CAVE_ICKY));

			/* Extract the player data */
			if (sf_version < 27)
			{
				/*
				 * Set old CAVE_MARK and flag (used below)
				 * (Ignore the CAVE_VIEW flag)
				 */
				pc_ptr->player = tmp8u & (0x01);
			}

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = xmin;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	if (sf_version > 26)
	{
		/*** Run length decoding ***/

		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Access the cave */
				pc_ptr = parea(x, y);

				/* Extract "player info" (only use detect grid data) */
				pc_ptr->player = tmp8u & (GRID_DTCT);

				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}

	if (sf_version > 28)
	{
		/*** Run length decoding ***/

		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Access the cave */
				pc_ptr = parea(x, y);

				/* Extract "feat" */
				pc_ptr->feat = tmp8u;

				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = area(x, y);

			/* Extract "feat" */
			c_ptr->feat = tmp8u;

			/* Quick hack to fix various removed features */
			fix_tile(c_ptr);

			/* Fix player memory for old savefiles */
			if (sf_version < 28)
			{
				pc_ptr = parea(x, y);

				/* Old CAVE_MARK flag set? */
				if (pc_ptr->player & 0x01)
				{
					/* Remember square */
					pc_ptr->feat = c_ptr->feat;
				}
			}

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = xmin;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}


	/*** Run length decoding ***/

	if (sf_version < 28)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Ignore this (The mimic field has been removed) */

				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}

	/*** Run length decoding ***/

	/* This isn't stored in later versions. */
	if (sf_version < 15)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_s16b(&tmp16s);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Access the cave */
				c_ptr = area(x, y);

				/* Extract field */
				c_ptr->fld_idx = 0;

				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}
}


/*
 * Strip old dungeon or wilderness map from the savefile
 */
static void strip_map(int xmin, int ymin, int xmax, int ymax)
{
	int i, y, x;
	byte count;
	byte tmp8u;
	s16b tmp16s;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = xmin;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax;)
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = xmin;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	if (sf_version > 26)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}

	if (sf_version > 28)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}


	if (sf_version < 29)
	{
		/*** Run length decoding ***/

		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_byte(&tmp8u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{

				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}

	/*** Run length decoding ***/

	/* This isn't stored in later versions. */
	if (sf_version < 15)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax;)
		{
			/* Grab RLE info */
			rd_byte(&count);
			rd_s16b(&tmp16s);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Advance/Wrap */
				if (++x >= xmax)
				{
					/* Wrap */
					x = xmin;

					/* Advance/Wrap */
					if (++y >= ymax) break;
				}
			}
		}
	}
}


/*
 * Size of the wilderness to load
 */
static s32b wild_x_size;
static s32b wild_y_size;

/*
 * Load wilderness data
 */
static void load_wild_data(void)
{
	int i, j;
	u16b tmp_u16b;
	byte tmp_byte;

	if (sf_version < 28)
	{
		/* Load bounds */
		rd_u16b(&p_ptr->max_hgt);
		rd_u16b(&p_ptr->max_wid);
		rd_u16b(&p_ptr->min_hgt);
		rd_u16b(&p_ptr->min_wid);
		rd_byte(&tmp_byte);
		rd_byte(&tmp_byte);

		/* Load cache status */
		rd_byte(&tmp_byte);
	}

	/* Load wilderness seed */
	rd_u32b(&wild_seed);

	/* Load wilderness map */
	for (i = 0; i < wild_x_size; i++)
	{
		for (j = 0; j < wild_y_size; j++)
		{
			if (sf_version < 8)
			{
				/* Terrain */
				rd_u16b(&wild[j][i].done.wild);

				/* Places */
				rd_u16b(&tmp_u16b);
				wild[j][i].done.place = (byte)tmp_u16b;

				/* Info flag */
				rd_byte(&wild[j][i].done.info);

				/* Monster Gen type */
				rd_byte(&tmp_byte);
				wild[j][i].done.mon_gen = tmp_byte;
			}
			else
			{
				/* Changed size of data types. */

				/* Terrain */
				rd_u16b(&wild[j][i].done.wild);

				/* Places */
				rd_byte(&wild[j][i].done.place);

				/* Info flag */
				rd_byte(&wild[j][i].done.info);

				/* Monster Gen type */
				rd_byte(&wild[j][i].done.mon_gen);

				/* Monster Probability */
				rd_byte(&wild[j][i].done.mon_prob);
			}
		}
	}
}

/* The version when the format of the wilderness last changed */
#define VERSION_CHANGE_WILD		38


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 */
static errr rd_dungeon(void)
{
	int i;
	s16b py, px;
	int wid, hgt;
	u16b limit;
	cave_type *c_ptr;
	u16b dun_level_backup, px_back, py_back;

	bool ignore_stuff = FALSE;

	s16b cur_wid, cur_hgt;

	/* Hack - Reset the object theme */
	dun_theme.treasure = 20;
	dun_theme.combat = 20;
	dun_theme.magic = 20;
	dun_theme.tools = 20;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&p_ptr->depth);

	/* Set the base level for old versions */
	base_level = p_ptr->depth;

	/* Read the base level */
	if (!z_older_than(2, 2, 2))
	{
		rd_s16b(&base_level);
	}

	rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);
	rd_s16b(&max_panel_rows);
	rd_s16b(&max_panel_cols);

	/* The player may not be in the dungeon */
	character_dungeon = FALSE;

	/* Assume we are in the dungeon */
	p_ptr->max_hgt = cur_hgt;
	p_ptr->min_hgt = 0;
	p_ptr->max_wid = cur_wid;
	p_ptr->min_wid = 0;

	if (sf_version < 12)
	{
		max_panel_cols = max_panel_cols * (wid - COL_MAP - 1) / 2;
		max_panel_rows = max_panel_rows * (hgt - ROW_MAP - 1) / 2;

		/* Reset the panel */
		panel_row_min = max_panel_rows;
		panel_col_min = max_panel_cols;
	}

	if (sf_version < 7)
	{
		/* Make the wilderness */
		dun_level_backup = p_ptr->depth;
		p_ptr->depth = 0;

		/* Save player location */
		px_back = px;
		py_back = py;

		create_wilderness();

		p_ptr->depth = dun_level_backup;

		/* if in the dungeon - restore the player location */
		if (p_ptr->depth)
		{
			px = px_back;
			py = py_back;
		}

		/* Hack - do not load data into wilderness */
		change_level(1);

		/* Get the new region */
		dun_ptr->region = (s16b)create_region(cur_wid, cur_hgt, REGION_CAVE);
		incref_region(cur_region);

		/* Load dungeon map */
		load_map(0, 0, cur_wid, cur_hgt);

		/* Restore the bounds */
		p_ptr->max_hgt = cur_hgt;
		p_ptr->min_hgt = 0;
		p_ptr->max_wid = cur_wid;
		p_ptr->min_wid = 0;
	}
	/* The wilderness + dungeon format changed here */
	else if (sf_version < 28)
	{
		/* Load wilderness data */
		load_wild_data();

		if (p_ptr->depth)
		{
			dun_level_backup = p_ptr->depth;

			change_level(p_ptr->depth);

			/* Save player location */
			px_back = px;
			py_back = py;

			create_wilderness();

			p_ptr->depth = dun_level_backup;

			change_level(p_ptr->depth);

			/* Get the new region */
			dun_ptr->region = (s16b)create_region(cur_wid, cur_hgt,
												  REGION_CAVE);
			incref_region(cur_region);

			/* Load dungeon map */
			load_map(0, 0, cur_wid, cur_hgt);

			/*
			 * Strip the wilderness map
			 * A square WILD_BLOCK_SIZE * WILD_VIEW in width.
			 */
			strip_map(0, 0, 9 * 16, 9 * 16);

			px = px_back;
			py = py_back;

			/* Restore the bounds */
			p_ptr->max_hgt = cur_hgt;
			p_ptr->min_hgt = 0;
			p_ptr->max_wid = cur_wid;
			p_ptr->min_wid = 0;
		}
		else
		{
			/* Strip the wilderness map */
			strip_map(p_ptr->min_wid, p_ptr->min_hgt,
					  p_ptr->max_wid, p_ptr->max_hgt);

			/* Make a new wilderness */
			create_wilderness();
			
			/* Save location */
			px = p_ptr->px;
			py = p_ptr->py;
		}
	}
	/* This doesn't do anything at the moment - but will in the future */
	else if (sf_version < VERSION_CHANGE_WILD)
	{
		/* Load wilderness data */
		load_wild_data();

		if (p_ptr->depth)
		{
			dun_level_backup = p_ptr->depth;
			
			change_level(p_ptr->depth);

			/* Save player location */
			px_back = px;
			py_back = py;

			create_wilderness();

			p_ptr->depth = dun_level_backup;

			change_level(p_ptr->depth);

			/* Get the new region */
			dun_ptr->region = (s16b)create_region(cur_wid, cur_hgt,
												  REGION_CAVE);
			incref_region(cur_region);

			/* Load dungeon map */
			load_map(0, 0, cur_wid, cur_hgt);

			px = px_back;
			py = py_back;

			/* Restore the bounds */
			p_ptr->max_hgt = cur_hgt;
			p_ptr->min_hgt = 0;
			p_ptr->max_wid = cur_wid;
			p_ptr->min_wid = 0;
		}
		else
		{
			/*
			 * Strip the wilderness map
			 * A square WILD_BLOCK_SIZE * WILD_VIEW in width.
			 */
			strip_map(0, 0, 9 * 16, 9 * 16);

			/* Make a new wilderness */
			create_wilderness();
			
			/* Save location */
			px = p_ptr->px;
			py = p_ptr->py;
		}
	}
	else
	{
		/* Load wilderness data */
		load_wild_data();

		change_level(p_ptr->depth);

		if (p_ptr->depth)
		{
			/* Get the new region */
			dun_ptr->region = (s16b)create_region(cur_wid, cur_hgt,
												  REGION_CAVE);
			incref_region(cur_region);

			/* Load dungeon map */
			load_map(0, 0, cur_wid, cur_hgt);

			/* Restore the bounds, overwritten in change_level */
			p_ptr->max_hgt = cur_hgt;
			p_ptr->min_hgt = 0;
			p_ptr->max_wid = cur_wid;
			p_ptr->min_wid = 0;
		}
		else
		{
			/* Load the wilderness */
			load_map(p_ptr->min_wid, p_ptr->min_hgt, p_ptr->max_wid,
					 p_ptr->max_hgt);
		}
	}


	/* Ignore stuff if loading old savefiles */
	if (sf_version < VERSION_CHANGE_WILD) ignore_stuff = TRUE;

	/* Hack - restore player position */
	p_ptr->px = px;
	p_ptr->py = py;

	/* Notice position */
	Term_move_player();

	/* Hack -  wipe the stuff on this level... */
	wipe_monsters(cur_region);

	/*
	 * Objects are deleted after the monsters,
	 * because monsters carry them.
	 */
	wipe_objects(cur_region);
	wipe_fields(cur_region);

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > z_info->o_max)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
	}

	/*
	 * No objects yet
	 */
	o_cnt = 0;

	/* Read the dungeon items */
	for (o_max = 1; o_max < limit; o_max++)
	{
		object_type *o_ptr;

		/* Acquire place */
		o_ptr = &o_list[o_max];

		/* Read the item */
		rd_item(o_ptr);

		/* Hack - import player inventory properly */
		o_ptr->allocated = TRUE;

		/* Real item? */
		if (o_ptr->k_idx)
		{
			/* Count objects */
			o_cnt++;

			/* Dungeon items */
			if (!ignore_stuff && (o_ptr->ix || o_ptr->iy))
			{
				if (!in_bounds(o_ptr->ix, o_ptr->iy))
				{
					note(format
						 ("Object placement error (%d,%d)", o_ptr->ix,
						  o_ptr->iy));
					return (152);
				}

				/* Access the item location */
				c_ptr = area(o_ptr->ix, o_ptr->iy);

				/*
				 * This is so much of a hack it hurts.  We really need
				 * to have a loop... or something.
				 */

				/* XXX XXX Mega-hack - build a stack */
				o_ptr->next_o_idx = c_ptr->o_idx;

				/* Place the object */
				c_ptr->o_idx = o_max;

				/* Hack - set region of object */
				o_ptr->region = cur_region;
			}
		}
	}

	/* Expand object array */
	o_max++;

	/* Repair inventory information */
	if (sf_version < 37)
	{
		object_type *o_ptr;

		for (i = 0; i < 24; i++)
		{
			o_ptr = &old_inventory[i];

			/* Do we have a real object? */
			if (o_ptr->k_idx)
			{
				/* Carry it */
				(void)inven_carry(o_ptr);
			}
		}
	}

	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit > z_info->m_max)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		int m_idx;

		monster_type *m_ptr;

		monster_race *r_ptr;


		/* Get a new record */
		m_idx = m_pop();

		/* Acquire monster */
		m_ptr = &m_list[m_idx];

		/* Read the monster */
		rd_monster(m_ptr);

		/* Hack - set region of monster */
		m_ptr->region = cur_region;

		if (!ignore_stuff)
		{
			/* Oops */
			if (i != m_idx)
			{
				note(format("Monster allocation error (%d <> %d)", i, m_idx));
				return (162);
			}

			if (!in_bounds(m_ptr->fx, m_ptr->fy))
			{
				note(format
					 ("Monster placement error (%d,%d)", m_ptr->fx, m_ptr->fy));
				return (162);
			}

			/* Access grid */
			c_ptr = area(m_ptr->fx, m_ptr->fy);

			/* Mark the location */
			c_ptr->m_idx = m_idx;
		}

		/* Access race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
	}

	if (sf_version > 11)
	{

		/*** Fields ***/

		/* Read the field count */
		rd_u16b(&limit);

		/* Verify maximum */
		if (limit > z_info->fld_max)
		{
			note(format("Too many (%d) field entries!", limit));
			return (151);
		}

		/* Read the fields */
		for (i = 1; i < limit; i++)
		{
			int fld_idx;

			field_type temp_field;
			field_type *f_ptr = &temp_field;

			/* Read the field */
			rd_field(f_ptr);

			/* Hack - set region of field */
			f_ptr->region = cur_region;

			if (!ignore_stuff)
			{
				/* Access the fields location */
				c_ptr = area(f_ptr->fx, f_ptr->fy);

				/* Build a stack */
				fld_idx = field_add(f_ptr, &c_ptr->fld_idx);

				/* Oops */
				if (i != fld_idx)
				{
					note(format
						 ("Field allocation error (%d <> %d)", i, fld_idx));
					return (152);
				}
			}
		}
	}


	/*** Success ***/

	/* Regenerate the dungeon for old savefiles and corrupted panic-saves */
	if ((py == 0) || (px == 0))
	{
		character_dungeon = FALSE;
	}
	else
	{
		/* The dungeon is ready */
		character_dungeon = TRUE;
	}

	/* Hack - make new level only after objects + monsters are loaded */
	if (sf_version < 7)
	{
		/* enter the level */
		change_level(p_ptr->depth);

		if (p_ptr->depth)
		{
			/* Restore the bounds */
			p_ptr->max_hgt = cur_hgt;
			p_ptr->min_hgt = 0;
			p_ptr->max_wid = cur_wid;
			p_ptr->min_wid = 0;
		}
		else
		{
			character_dungeon = FALSE;
		}
	}

	/* 
	 * Set the trap detected flag.
	 *
	 * This is done here because it needs to be below all calls
	 * to "change_level()"
	 */
	p_ptr->detected = player_detected;

	/* Success */
	return (0);
}

/*
 * Strip old (Pre 2.7.0) quest info from the savefile
 */
static void strip_quests(u16b num)
{
	int i;
	s16b status;

	for (i = 0; i < num; i++)
	{
		if (i < z_info->q_max)
		{
			rd_s16b(&status);

			if (!z_older_than(2, 2, 0))
			{
				strip_bytes(2);
			}

			/* Load quest status if quest is running */
			if (status == 1)
			{
				strip_bytes(6);

				if (z_older_than(2, 2, 0))
				{
					strip_bytes(2);
				}

				strip_bytes(2);

				/* Load quest item index */
				if (!z_older_than(2, 2, 1))
				{
					strip_bytes(2);
				}

				/* Load quest flags */
				if (!z_older_than(2, 2, 3))
				{
					strip_bytes(1);
				}

				if (z_older_than(2, 2, 0))
				{
					strip_bytes(40);
				}
			}
		}
		/* Ignore the empty quests from old versions */
		else
		{
			/* Ignore quest status */
			strip_bytes(2);

			/* Ignore quest level */
			if (!z_older_than(2, 2, 0))
			{
				strip_bytes(2);
			}

			/*
			 * We don't have to care about the other info,
			 * since status should be 0 for these quests anyway
			 */
		}
	}
}

/*
 * Load the quests
 */
static void rd_quests(int max_quests)
{
	int i, q_idx;

	quest_type *q_ptr;

	for (i = 1; i < max_quests; i++)
	{
		q_idx = q_pop();

		/* Paranoia (We've already tested < q_max) */
		if (!q_idx) quit("Trying to load too many quests!");

		q_ptr = &quest[q_idx];

		/* Generic information */
		rd_byte(&q_ptr->status);
		rd_byte(&q_ptr->flags);
		rd_byte(&q_ptr->type);
		rd_byte(&q_ptr->item);

		rd_u16b(&q_ptr->place);
		rd_u16b(&q_ptr->shop);
		rd_u16b(&q_ptr->reward);

		rd_byte(&q_ptr->c_type);
		rd_byte(&q_ptr->x_type);

		rd_u32b(&q_ptr->timeout);
		rd_string(q_ptr->name, 60);

		/* Data - quest-type specific */
		switch (q_ptr->type)
		{
				/* Un-initialised quests */
			case QUEST_TYPE_UNKNOWN: break;

				/* General quests */
			case QUEST_TYPE_GENERAL:
			{
				rd_u16b(&q_ptr->data.gen.place);
				rd_u16b(&q_ptr->data.gen.shop);
				rd_u16b(&q_ptr->data.gen.r_idx);
				rd_u16b(&q_ptr->data.gen.cur_num);
				rd_u16b(&q_ptr->data.gen.max_num);
				break;
			}

				/* Dungeon quests */
			case QUEST_TYPE_DUNGEON:
			{
				rd_u16b(&q_ptr->data.dun.r_idx);
				rd_u16b(&q_ptr->data.dun.level);

				rd_s16b(&q_ptr->data.dun.cur_num);
				rd_s16b(&q_ptr->data.dun.max_num);
				rd_s16b(&q_ptr->data.dun.num_mon);
				break;
			}

				/* Wilderness quests */
			case QUEST_TYPE_WILD:
			{
				rd_u16b(&q_ptr->data.wld.place);
				rd_u16b(&q_ptr->data.wld.data);
				rd_byte(&q_ptr->data.wld.depth);
				break;
			}

			default:
			{
				/* Unknown quest type... panic */
				quit("Loading unknown quest type.");
			}
		}
	}
}


/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i, j;

	int tempx, tempy;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif

	u16b max_towns_load;
	u16b max_quests_load;


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...", z_major, z_minor, z_patch));

	/* Strip the version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;


	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

#if SAVEFILE_VERSION
	/* Read the version number of the savefile */
	if (!z_older_than(2, 2, 8) &&
		!(z_major == 2 && z_minor == 3 && z_patch == 0))
		rd_u32b(&sf_version);
#endif /* SAVEFILE_VERSION */

	/* Operating system info */
	rd_u32b(&sf_xtra);

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);


	/* Later use (always zero) */
	rd_u32b(&tmp32u);

	/* Later use (always zero) */
	rd_u32b(&tmp32u);


	/* Read RNG state */
	rd_randomizer();
	if (arg_fiddle) note("Loaded Randomizer Info");


	/* Then the options */
	rd_options();
	if (arg_fiddle) note("Loaded Option Flags");

	/* Switch streams on for old savefiles */
	if (z_older_than(2, 2, 7))
		terrain_streams = TRUE;

	/*
	 * Munchkin players are marked
	 *
	 * XXX - should be replaced with a better method,
	 * after the new scorefile-handling is implemented.
	 */
	if (munchkin_death)
	{
		/* Mark savefile */
		p_ptr->noscore |= 0x0001;
	}

	/* Then the "messages" */
	rd_messages();
	if (arg_fiddle) note("Loaded Messages");


	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->r_max)
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (21);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		/* Read the lore */
		rd_lore(i);
	}

	/* Pre 2.2.0 version (old r_info.txt) */
	if (z_older_than(2, 2, 0))
	{
		monster_race *r_ptr;

		for (i = 0; i < z_info->r_max; i++)
		{
			/* Access that monster */
			r_ptr = &r_info[i];

			/* Hack -- Reset the death counter */
			r_ptr->max_num = 100;
			if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
			if (r_ptr->flags3 & RF3_UNIQUE_7) r_ptr->max_num = 7;
		}
	}

	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->k_max)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (22);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		object_kind *k_ptr = &k_info[i];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE : FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE : FALSE;
	}
	if (arg_fiddle) note("Loaded Object Memory");



	/* Number of towns */
	rd_u16b(&max_towns_load);

	/* 2.2.2 or older version */
	if (z_older_than(2, 2, 3))
	{
		/* Ignore higher numbers of towns */
		if (max_towns_load > z_info->wp_max)
			max_towns_load = z_info->wp_max;
	}

	/* Incompatible save files */
	if (max_towns_load > z_info->wp_max)
	{
		note(format("Too many (%u) towns!", max_towns_load));
		return (23);
	}

	/* Number of quests */
	rd_u16b(&max_quests_load);

	/* Ignore old quests */
	if (sf_version < 30)
	{
		strip_quests(max_quests_load);
	}

	/* Newer versions */
	else
	{
		/* Incompatible save files */
		if (max_quests_load > z_info->q_max)
		{
			note(format("Too many (%u) quests!", max_quests_load));
			return (23);
		}

		rd_quests(max_quests_load);
	}

	/* Only in 2.2.1 and 2.2.2 */
	if (!z_older_than(2, 2, 1) && z_older_than(2, 2, 3))
	{
		/* "Hard quests" flag */
		rd_byte((byte *)&ironman_hard_quests);

		/* Inverted "Wilderness" flag */
		rd_byte((byte *)&vanilla_town);
		vanilla_town = !vanilla_town;
	}

	/* Position in the wilderness */
	rd_s32b(&p_ptr->wilderness_x);
	rd_s32b(&p_ptr->wilderness_y);

	/* Size of the wilderness */
	rd_s32b(&wild_x_size);
	rd_s32b(&wild_y_size);

	/* Incompatible save files */
	if ((wild_x_size > WILD_SIZE) || (wild_y_size > WILD_SIZE))
	{
		note(format
			 ("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size));
		return (23);
	}

	/* Hack - if size is zero - set to WILD_SIZE */
	if ((wild_x_size == 0) && (wild_y_size == 0))
	{
		wild_x_size = WILD_SIZE;
		wild_y_size = WILD_SIZE;
	}

	/* Hack - set size of wilderness to x size only */
	max_wild = wild_x_size;

	tempx = (int)p_ptr->wilderness_x / 16;
	tempy = (int)p_ptr->wilderness_y / 16;

	/* Get corner of visible region */
	shift_in_bounds(&tempx, &tempy);

	/* Set corner of visible region */
	p_ptr->old_wild_x = tempx;
	p_ptr->old_wild_y = tempy;

	/* Ignore the seeds from old versions */
	if (sf_version < 9)
	{
		/* Load the wilderness seeds */
		for (i = 0; i < wild_x_size; i++)
		{
			for (j = 0; j < wild_y_size; j++)
			{
				/* Ignore seeds */
				rd_u32b(&tmp32u);
			}
		}
	}

	/*
	 * Reinitialise the quests when loading an old version
	 */
	if (sf_version < 30)
	{
		(void)get_player_quests();
	}

	if (arg_fiddle) note("Loaded Quests");

	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->a_max)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (24);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		a_info[i].cur_num = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle) note("Loaded Artifacts");


	/* Read the extra stuff */
	rd_extra();
	if (arg_fiddle) note("Loaded extra information");

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->pclass];


	/* Read spell info */
	rd_u32b(&p_ptr->spell_learned1);
	rd_u32b(&p_ptr->spell_learned2);
	rd_u32b(&p_ptr->spell_worked1);
	rd_u32b(&p_ptr->spell_worked2);
	rd_u32b(&p_ptr->spell_forgotten1);
	rd_u32b(&p_ptr->spell_forgotten2);

	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		rd_byte(&p_ptr->spell_order[i]);
	}


	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Read number of towns */
	rd_u16b(&tmp16u);
	place_count = tmp16u;

	/* Paranoia */
	if (place_count > z_info->wp_max)
	{
		note("Error - increase number of towns in misc.txt");
		return (33);
	}

	/* Empty the store stock cache */
	store_cache_num = 0;

	if (sf_version < 8)
	{
		/* Read the stores */
		rd_u16b(&tmp16u);

		for (i = 1; i < place_count; i++)
		{
			place[i].numstores = tmp16u;

			/* Allocate the stores */
			C_MAKE(place[i].store, place[i].numstores, store_type);

			/* HACK - ignore the empty towns */
			if (z_older_than(2, 2, 3) && (i >= 6))
			{
				for (j = 0; j < tmp16u; j++)
				{
					/* Read the info into the empty town 5 (R'Lyeh) */
					rd_store(5, j);
				}
			}
			else
			{
				for (j = 0; j < tmp16u; j++)
				{
					rd_store(i, j);
				}
			}
		}
	}
	else
	{
		/* Get the town data */
		for (i = 1; i < place_count; i++)
		{
			place_type *pl_ptr = &place[i];

			/* RNG seed */
			rd_u32b(&pl_ptr->seed);

			/* Number of stores */
			rd_byte(&pl_ptr->numstores);

			/* Type */
			rd_u16b(&tmp16u);

			/* Hack.... used to be a u16b, but only ever used a bytes worth */
			pl_ptr->type = (byte)tmp16u;

			if (sf_version > 21)
			{
				rd_byte(&pl_ptr->data);
			}

			/* Gates */
			if (sf_version > 22)
			{
				rd_byte(&pl_ptr->gates_x[0]);
				rd_byte(&pl_ptr->gates_x[1]);
				rd_byte(&pl_ptr->gates_x[2]);
				rd_byte(&pl_ptr->gates_x[3]);

				rd_byte(&pl_ptr->gates_y[0]);
				rd_byte(&pl_ptr->gates_y[1]);
				rd_byte(&pl_ptr->gates_y[2]);
				rd_byte(&pl_ptr->gates_y[3]);
			}

			/* Locatation */
			rd_byte(&pl_ptr->x);
			rd_byte(&pl_ptr->y);

			/* Size */
			if (sf_version > 29)
			{
				rd_byte(&pl_ptr->xsize);
				rd_byte(&pl_ptr->ysize);

				rd_u16b(&pl_ptr->quest_num);
				rd_byte(&pl_ptr->monst_type);
			}
			else
			{
				/* Need to create town size as default */
				pl_ptr->xsize = 8;
				pl_ptr->ysize = 8;
			}

			/* Name */
			if (sf_version < 26)
			{
				char temp_buffer[32];

				rd_string(temp_buffer, 32);

				/* Get a new name */
				if (vanilla_town)
				{
					strcpy(pl_ptr->name, "Town");
				}
				else
				{
					select_town_name(pl_ptr->name, pl_ptr->data);
				}
			}
			else
			{
				rd_string(pl_ptr->name, T_NAME_LEN);
			}

			/* Allocate the stores */
			C_MAKE(pl_ptr->store, pl_ptr->numstores, store_type);

			/* Get the stores of all towns */
			for (j = 0; j < pl_ptr->numstores; j++)
			{
				rd_store(i, j);
			}
		}
	}

	/* Read the pet command settings */
	if (sf_version > 2)
	{
		rd_s16b(&p_ptr->pet_follow_distance);
		rd_byte(&p_ptr->pet_open_doors);
		rd_byte(&p_ptr->pet_pickup_items);
	}
	else if (!z_older_than(2, 2, 3))
	{
		rd_byte(&tmp8u);

		p_ptr->pet_follow_distance = tmp8u;

		rd_byte(&p_ptr->pet_open_doors);
		rd_byte(&p_ptr->pet_pickup_items);
	}
	else
	{
		/* Default pet command settings */
		p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
		p_ptr->pet_open_doors = FALSE;
		p_ptr->pet_pickup_items = FALSE;
	}

	/* I'm not dead yet... */
	if (!p_ptr->is_dead)
	{
		/* Dead players have no dungeon */
		note("Restoring Dungeon...");
		if (rd_dungeon())
		{
			note("Error reading dungeon data");
			return (34);
		}

		/* Read the ghost info */
		rd_ghost();

		if (!z_older_than(2, 2, 4))
		{
			s32b tmp32s;

			rd_s32b(&tmp32s);
			strip_bytes(tmp32s);
		}
	}


#ifdef VERIFY_CHECKSUMS

	/* Save the checksum */
	n_v_check = v_check;

	/* Read the old checksum */
	rd_u32b(&o_v_check);

	/* Verify */
	if (o_v_check != n_v_check)
	{
		note("Invalid checksum");
		return (11);
	}


	/* Save the encoded checksum */
	n_x_check = x_check;

	/* Read the checksum */
	rd_u32b(&o_x_check);


	/* Verify */
	if (o_x_check != n_x_check)
	{
		note("Invalid encoded checksum");
		return (11);
	}

#endif

	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;

	/* Grab permissions */
	safe_setuid_grab();

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

	/* Drop permissions */
	safe_setuid_drop();

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = rd_savefile_new_aux();

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Result */
	return (err);
}
