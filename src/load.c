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
static FILE	*fff;

/*
 * Hack -- old "encryption" byte
 */
static byte	xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b	v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b	x_check = 0L;



/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
static bool older_than(byte x, byte y, byte z)
{
	/* Much older, or much more recent */
	if (sf_major < x) return (TRUE);
	if (sf_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (sf_minor < y) return (TRUE);
	if (sf_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (sf_patch < z) return (TRUE);
	if (sf_patch > z) return (FALSE);

	/* Identical versions */
	return (FALSE);
}


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
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
static bool wearable_p(object_type *o_ptr)
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
static bool is_weapon(object_type *o_ptr)
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
	rd_u16b((u16b*)ip);
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
	rd_u32b((u32b*)ip);
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
	str[max-1] = '\0';
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
 * require slow python hooks.
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	byte tmpbyte;

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

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	/* Old flags */
	rd_u32b(&o_ptr->flags1);
	rd_u32b(&o_ptr->flags2);
	rd_u32b(&o_ptr->flags3);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

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
#ifdef USE_SCRIPT
		if (tmp32s)
		{
			char *python_object = (char*) malloc(tmp32s + 1);
			rd_string(python_object, tmp32s + 1);
			o_ptr->python = object_load_callback(python_object);
			free(python_object);
		}
#else /* USE_SCRIPT */
		strip_bytes(tmp32s);
#endif /* USE_SCRIPT */
	}

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* For rod-stacking */
	if (z_older_than(2, 2, 5) && (o_ptr->tval == TV_ROD))
	{
		o_ptr->timeout = o_ptr->pval * o_ptr->number;
		o_ptr->pval = k_ptr->pval * o_ptr->number;
	}

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

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

		/* All done */
		return;
	}

	/* Hack -- extract the "broken" flag */
	if (o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);
	
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

				/* Hack -- extract the "broken" flag */
				if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
				
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

			/* Hack -- extract the "broken" flag */
			if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
			
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
		if (o_ptr->ident & (IDENT_MENTAL))
		{
			o_ptr->kn_flags1 = o_ptr->flags1;
			o_ptr->kn_flags2 = o_ptr->flags2;
			o_ptr->kn_flags3 = o_ptr->flags3;
		}
	}

	if (o_ptr->xtra_name) /* Artifacts and ego items */
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
	rd_byte(&tmp8u);
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
#ifdef USE_SCRIPT
	if (tmp32s)
	{
		char *python_field = (char*) malloc(tmp32s + 1);
		rd_string(python_field, tmp32s + 1);
		f_ptr->python = field_load_callback(python_field);
		free(python_field);
	}
#else /* USE_SCRIPT */
	strip_bytes(tmp32s);
#endif /* USE_SCRIPT */

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
static errr rd_store(int town_number, int store_number)
{
	store_type *st_ptr = &town[town_number].store[store_number];

	int j;

	s16b good_buy, bad_buy, insult_cur;
	s32b store_open;
	
	byte num, owner, type = 0;

	/* Read the basic info */
	rd_s32b(&store_open);
	rd_s16b(&insult_cur);
	rd_byte(&owner);
	rd_byte(&num);
	rd_s16b(&good_buy);
	rd_s16b(&bad_buy);
	
	if (sf_version > 20)
	{
		rd_u16b(&st_ptr->x);
		rd_u16b(&st_ptr->y);
		
		/* Hack - only listen to 'type' in recent savefiles */
		rd_byte(&type);
	}
	
	
	/* Initialise the store */
	if (build_is_store(type))
	{
		store_init(town_number, store_number, type);
	}
	else
	{
		st_ptr->type = type;
	}
	
	/* Restore the saved parameters */
	st_ptr->store_open = store_open;
	st_ptr->insult_cur = insult_cur;
	st_ptr->owner = owner;
	st_ptr->good_buy = good_buy;
	st_ptr->bad_buy = bad_buy;
	

	if (!z_older_than(2, 1, 3))
	{
		/* Read last visit */
		rd_s32b(&st_ptr->last_visit);
	}
	else
	{
		/* Reset last visit to the current turn */
		st_ptr->last_visit = turn;
	}

	

	/*
	 * Hack - allocate store if it has stock
	 * Note that this will change the order that
	 * stores are removed from the cache.
	 * The resulting list can be sorted... but it
	 * doesn't really matter.
	 */
	if (num)
	{
		(void)allocate_store(st_ptr);
	}

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type forge;
		object_type *q_ptr;

		/* Get local object */
		q_ptr = &forge;

		/* Wipe the object */
		object_wipe(q_ptr);

		/* Read the item */
		rd_item(q_ptr);

		/* Acquire valid items */
		if (st_ptr->stock_num < STORE_INVEN_MAX)
		{
			int k = st_ptr->stock_num++;

			/* Acquire the item */
			object_copy(&st_ptr->stock[k], q_ptr);
		}
	}

	/* Success */
	return (0);
}



/*
 * Read RNG state (added in 2.8.0)
 */
static void rd_randomizer(void)
{
	int i;

	u16b tmp16u;

	/* Old version */
	if (older_than(2, 8, 0)) return;

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

	/* Pre-2.8.0 savefiles are done */
	if (older_than(2, 8, 0)) return;

	if (z_older_than(2, 1, 0))
	{
		autosave_t = autosave_l = 0;
		autosave_freq = 0;
	}
	else
	{
		rd_byte(&autosave_l);
		rd_byte(&autosave_t);
		rd_s16b(&autosave_freq);
	}


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
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the window masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

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
	rd_byte(&tmp8u); /* oops */

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

	strip_bytes(24); /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	/* Current version */
	if (!z_older_than(2, 1, 3))
	{
		rd_s16b(&p_ptr->town_num);

		/* Read arena and rewards information */
		strip_bytes(4); /* oops */
		rd_s16b(&p_ptr->inside_quest);
		strip_bytes(6); /* oops */

		rd_s16b(&tmp16s);

		for (i = 0; i < tmp16s; i++) rd_s16b(&dummy);
	}
	/* 2.1.2 beta version */
	else if (z_major == 2 && z_minor == 1 && z_patch == 2)
	{
		/* Town index */
		rd_s16b(&tmp16s);
		p_ptr->town_num = 1;

		strip_bytes(4); /* oops */
		rd_s16b(&p_ptr->inside_quest);
		strip_bytes(2); /* oops */

		/* Throw away old quest informations */
		for (i = 0; i < 100; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 10; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 10; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 5; i++) rd_s16b(&tmp16s);
		for (i = 0; i < 5; i++) rd_s16b(&tmp16s);
	}
	else /* 2.1.0 or older */
	{
		p_ptr->town_num = 1;

		/* Initialize quest information -KMW- */
		p_ptr->inside_quest = 0;
	}

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
	strip_bytes(2); /* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4); /* Old "food_digested" / "protection" */
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
		p_ptr->tim_xtra1 = 0;
		p_ptr->tim_xtra2 = 0;
		p_ptr->tim_xtra3 = 0;
		p_ptr->tim_xtra4 = 0;
		p_ptr->tim_xtra5 = 0;
		p_ptr->tim_xtra6 = 0;
		p_ptr->tim_xtra7 = 0;
		p_ptr->tim_xtra8 = 0;
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
		rd_s16b(&p_ptr->tim_xtra1);
		rd_s16b(&p_ptr->tim_xtra2);
		rd_s16b(&p_ptr->tim_xtra3);
		rd_s16b(&p_ptr->tim_xtra4);
		rd_s16b(&p_ptr->tim_xtra5);
		rd_s16b(&p_ptr->tim_xtra6);
		rd_s16b(&p_ptr->tim_xtra7);
		rd_s16b(&p_ptr->tim_xtra8);
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
	rd_byte(&tmp8u); /* oops */
	rd_byte(&tmp8u); /* oops */
	rd_byte(&tmp8u); /* oops */
	rd_byte((byte*) &p_ptr->searching);
	rd_byte((byte*) &maximize_mode);
	rd_byte((byte*) &preserve_mode);
	rd_byte(&tmp8u);

	/* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);


	/* Special stuff */
	rd_u16b(&p_ptr->panic_save);
	rd_u16b(&p_ptr->total_winner);
	rd_u16b(&p_ptr->noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	p_ptr->is_dead = tmp8u;

	/* Read "feeling" */
	rd_byte(&tmp8u);
	feeling = tmp8u;

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);
	
	if (sf_version > 17)
	{
		/* Get trap detection status */
		rd_byte((byte *)&player_detected);
		
		/* Get location of detection */
		rd_s16b(&p_ptr->detecty);
		rd_s16b(&p_ptr->detectx);
	}
}




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
	int slot = 0;

	object_type forge;
	object_type *q_ptr;

	/* No weight */
	p_ptr->total_weight = 0;

	/* No items */
	p_ptr->inven_cnt = 0;
	p_ptr->equip_cnt = 0;

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

		/* Hack -- verify item */
		if (!q_ptr->k_idx) return (53);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* Add the weight */
			p_ptr->total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			p_ptr->equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (p_ptr->inven_cnt == INVEN_PACK)
		{
			/* Oops */
			note("Too many items in the inventory!");

			/* Fail */
			return (54);
		}

		/* Carry inventory */
		else
		{
			/* Get a slot */
			n = slot++;

			/* Copy object */
			object_copy(&inventory[n], q_ptr);

			/* Add the weight */
			p_ptr->total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			p_ptr->inven_cnt++;
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
			tmp8u = TERM_WHITE;

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
			(c_ptr->feat >= 0x10 && c_ptr->feat <=0x1F) ||
			(c_ptr->feat == 0x5A))
		{
			/* Get rid of it */
			c_ptr->feat = FEAT_FLOOR;
		}
				
		/* Doors */
		if ((c_ptr->feat > 0x20) && (c_ptr->feat < 0x28))
		{
			/* Locked door -> closed door*/
			c_ptr->feat = FEAT_CLOSED;
		}
				
		if ((c_ptr->feat >= 0x28) && (c_ptr->feat <= 0x2F))
		{
			/* Stuck door -> closed door*/
			c_ptr->feat = FEAT_CLOSED;
		}
	}
}


/*
 * Load dungeon or wilderness map
 */
static void load_map(int ymax, int ymin, int xmax, int xmin)
{
	int i, y, x;
	byte count;
	byte tmp8u;
	s16b tmp16s;
	cave_type *c_ptr;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = area(y,x);

			/* Extract "info" (without the CAVE_ROOM flag set)*/
			c_ptr->info = (tmp8u & ~(CAVE_MNLT | CAVE_VIEW));

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
	for (x = xmin, y = ymin; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = area(y,x);

			/* Extract "feat" */
			c_ptr->feat = tmp8u;
			
			/* Quick hack to fix various removed features */
			fix_tile(c_ptr);

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


	if (!z_older_than(2, 1, 3))
	{
		/*** Run length decoding ***/

		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax; )
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

		/*** Run length decoding ***/

		/* This isn't stored in later versions. */
		if (sf_version < 15)
		{
			/* Load the dungeon data */
			for (x = xmin, y = ymin; y < ymax; )
			{
				/* Grab RLE info */
				rd_byte(&count);
				rd_s16b(&tmp16s);

				/* Apply the RLE info */
				for (i = count; i > 0; i--)
				{
					/* Access the cave */
					c_ptr = area(y,x);

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
}


/*
 * Strip old dungeon or wilderness map from the savefile
 */
static void strip_map(int ymax, int ymin, int xmax, int xmin)
{
	int i, y, x;
	byte count;
	byte tmp8u;
	s16b tmp16s;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = xmin, y = ymin; y < ymax; )
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
	for (x = xmin, y = ymin; y < ymax; )
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
	for (x = xmin, y = ymin; y < ymax; )
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

	/* This isn't stored in later versions. */
	if (sf_version < 15)
	{
		/* Load the dungeon data */
		for (x = xmin, y = ymin; y < ymax; )
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
static	s32b wild_x_size;
static	s32b wild_y_size;

/*
 * Load wilderness data
 */
static void load_wild_data(void)
{
	int i, j;
	u16b tmp_u16b;
	byte tmp_byte;

	/* Load bounds */
	rd_u16b(&wild_grid.y_max);
	rd_u16b(&wild_grid.x_max);
	rd_u16b(&wild_grid.y_min);
	rd_u16b(&wild_grid.x_min);
	rd_byte(&wild_grid.y);
	rd_byte(&wild_grid.x);

	/* Load cache status */
	rd_byte(&wild_grid.cache_count);

	/* Load wilderness seed */
	rd_u32b(&wild_grid.wild_seed);

	/* Load wilderness map */
	for (i = 0; i < wild_x_size; i++)
	{
		for (j = 0; j < wild_y_size; j++)
		{
			if (sf_version < 8)
			{
				/* Terrain */
				rd_u16b(&wild[j][i].done.wild);

				/* Town / Dungeon / Specials */
				rd_u16b(&tmp_u16b);
				wild[j][i].done.town = (byte)tmp_u16b;

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

				/* Town / Dungeon / Specials */
				rd_byte(&wild[j][i].done.town);

				/* Info flag */
				rd_byte(&wild[j][i].done.info);

				/* Monster Gen type */
				rd_byte(&wild[j][i].done.mon_gen);

				/* Monster Probability */
				rd_byte(&wild[j][i].done.mon_prob);
			}
		}
	}

	/* Allocate blocks around player */
	for (i = 0; i < WILD_GRID_SIZE; i++)
	{
		for (j = 0; j < WILD_GRID_SIZE; j++)
		{
			/* Allocate block and link to the grid */
			wild_grid.block_ptr[j][i] =
				wild_cache[i + WILD_GRID_SIZE * j];
		}
	}
	
	/* If not in dungeon - reset the bounds */
	if (!p_ptr->depth)
	{
		min_hgt = wild_grid.y_min;
		max_hgt = wild_grid.y_max;
		min_wid = wild_grid.x_min;
		max_wid = wild_grid.x_max;
	}
}

/* The version when the format of the wilderness last changed */
#define VERSION_CHANGE_WILD		23


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
	max_hgt = cur_hgt;
	min_hgt = 0;
	max_wid = cur_wid;
	min_wid = 0;
	
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

		wipe_m_list();
		wipe_o_list();

		/* Hack - do not load data into wilderness */
		change_level(1);

		p_ptr->depth = dun_level_backup;

		/* if in the dungeon - restore the player location */
		if (p_ptr->depth)
		{
			px = px_back;
			py = py_back;
		}

		/* Load dungeon map */
		load_map(cur_hgt, 0, cur_wid, 0);
	}
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
			
			wipe_o_list();
			wipe_m_list();
			wipe_f_list();
			
			p_ptr->depth = dun_level_backup;
			
			change_level(p_ptr->depth);
			
			/* Load dungeon map */
			load_map(cur_hgt, 0, cur_wid, 0);

			/* Strip the wilderness map */
			strip_map(wild_grid.y_max, wild_grid.y_min,
			         wild_grid.x_max, wild_grid.x_min);
			
			px = px_back;
			py = py_back;
			
			/* Restore the bounds */
			max_hgt = cur_hgt;
			min_hgt = 0;
			max_wid = cur_wid;
			min_wid = 0;
		}
		else
		{
			/* Strip the wilderness map */
			strip_map(wild_grid.y_max, wild_grid.y_min,
			         wild_grid.x_max, wild_grid.x_min);

			/* Make a new wilderness */
			create_wilderness();
			
			wipe_m_list();
			wipe_o_list();
			wipe_f_list();
		}
	}
	else
	{
		/* Load wilderness data */
		load_wild_data();

		if (p_ptr->depth)
		{
			change_level(p_ptr->depth);

			/* Load dungeon map */
			load_map(cur_hgt, 0, cur_wid, 0);

			/* Set pointers to wilderness - but do not make towns */
			change_level(0);

			/* Load wilderness map */
			load_map(wild_grid.y_max, wild_grid.y_min,
			         wild_grid.x_max, wild_grid.x_min);

			change_level(p_ptr->depth);
			
			/* Restore the bounds */
			max_hgt = cur_hgt;
			min_hgt = 0;
			max_wid = cur_wid;
			min_wid = 0;
		}
		else
		{
			/* Hack - move to level without creating it */
			p_ptr->depth = 1;
			change_level(0);

			/* Load the wilderness */
			load_map(wild_grid.y_max, wild_grid.y_min,
			         wild_grid.x_max, wild_grid.x_min);

			/* Reset level */
			p_ptr->depth = 0;
		}
	}


	/* Hack - restore player position */
	p_ptr->px = px;
	p_ptr->py = py;

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > max_o_idx)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		int o_idx;

		object_type *o_ptr;


		/* Get a new record */
		o_idx = o_pop();

		/* Oops */
		if (i != o_idx)
		{
			note(format("Object allocation error (%d <> %d)", i, o_idx));
			return (152);
		}


		/* Acquire place */
		o_ptr = &o_list[o_idx];

		/* Read the item */
		rd_item(o_ptr);


		/* XXX XXX XXX XXX XXX */

		/* Monster */
		if (o_ptr->held_m_idx)
		{
			monster_type *m_ptr;

			/* Monster */
			m_ptr = &m_list[o_ptr->held_m_idx];

			/* Build a stack */
			o_ptr->next_o_idx = m_ptr->hold_o_idx;

			/* Place the object */
			m_ptr->hold_o_idx = o_idx;
		}

		/* Dungeon */
		else if (!((sf_version < VERSION_CHANGE_WILD) && (p_ptr->depth == 0)))
		{
			/* Access the item location */
			c_ptr = area(o_ptr->iy,o_ptr->ix);

			/* Build a stack */
			o_ptr->next_o_idx = c_ptr->o_idx;

			/* Place the object */
			c_ptr->o_idx = o_idx;
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit > max_m_idx)
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

		/* Oops */
		if (i != m_idx)
		{
			note(format("Monster allocation error (%d <> %d)", i, m_idx));
			return (162);
		}


		/* Acquire monster */
		m_ptr = &m_list[m_idx];

		/* Read the monster */
		rd_monster(m_ptr);

		if (!((sf_version < VERSION_CHANGE_WILD) && (p_ptr->depth == 0)))
		{
			/* Access grid */
			c_ptr = area(m_ptr->fy,m_ptr->fx);

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
		if (limit > max_fld_idx)
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

			if (!((sf_version < VERSION_CHANGE_WILD) && (p_ptr->depth == 0)))
			{
				/* Access the fields location */
				c_ptr = area(f_ptr->fy, f_ptr->fx);

				/* Build a stack */
				fld_idx = field_add(f_ptr, &c_ptr->fld_idx);

				/* Oops */
				if (i != fld_idx)
				{
					note(format("Field allocation error (%d <> %d)", i, fld_idx));
					return (152);
				}
			}
		}
	}


	/*** Success ***/

	/* Regenerate the dungeon for old savefiles and corrupted panic-saves */
	if (z_older_than(2, 1, 3) || (py == 0) || (px == 0))
	{
		character_dungeon = FALSE;
	}
	else
	{
		/* The dungeon is ready */
		character_dungeon = TRUE;
	}

	/* Hack - make new level only after objects + monsters are loaded */
	if (sf_version < VERSION_CHANGE_WILD)
	{
		if (p_ptr->depth)
		{
			/* Restore the bounds */
			max_hgt = cur_hgt;
			min_hgt = 0;
			max_wid = cur_wid;
			min_wid = 0;
		}
		else
		{
			character_dungeon = FALSE;
			wipe_m_list();
			wipe_o_list();
		}
		
		/* enter the level */
		change_level(p_ptr->depth);
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
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i, j;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
		z_major, z_minor, z_patch));


	/* Hack -- Warn about "obsolete" versions */
	if (older_than(2, 8, 0))
	{
		note("Warning -- converting obsolete save file.");
	}


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
	if (tmp16u > max_r_idx)
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

		for (i = 0; i < max_r_idx; i++)
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
	if (tmp16u > max_k_idx)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (22);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;
		object_kind *k_ptr = &k_info[i];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
	}
	if (arg_fiddle) note("Loaded Object Memory");

	/* 2.1.3 or newer version */
	if (!z_older_than(2, 1, 3))
	{
		u16b max_towns_load;
		u16b max_quests_load;

		/* Number of towns */
		rd_u16b(&max_towns_load);

		/* 2.2.2 or older version */
		if (z_older_than(2, 2, 3))
		{
			/* Ignore higher numbers of towns */
			if (max_towns_load > max_towns)
				max_towns_load = max_towns;
		}

		/* Incompatible save files */
		if (max_towns_load > max_towns)
		{
			note(format("Too many (%u) towns!", max_towns_load));
			return (23);
		}

		/* Number of quests */
		rd_u16b(&max_quests_load);

		/* 2.2.3 or newer version */
		if (!z_older_than(2, 2, 3))
		{
			/* Incompatible save files */
			if (max_quests_load > max_quests)
			{
				note(format("Too many (%u) quests!", max_quests_load));
				return (23);
			}
		}

		for (i = 0; i < max_quests_load; i++)
		{
			if (i < max_quests)
			{
				rd_s16b(&quest[i].status);

				if (!z_older_than(2, 2, 0))
				{
					rd_s16b(&quest[i].level);
				}

				/* Load quest status if quest is running */
				if (quest[i].status == QUEST_STATUS_TAKEN)
				{
					rd_s16b(&quest[i].cur_num);
					rd_s16b(&quest[i].max_num);
					rd_s16b(&quest[i].type);

					if (z_older_than(2, 2, 0))
					{
						strip_bytes(2);
					}

					/* Load quest monster index */
					rd_s16b(&quest[i].r_idx);

					/* Load quest item index */
					if (!z_older_than(2, 2, 1))
					{
						rd_s16b(&quest[i].k_idx);

						if (quest[i].k_idx)
							a_info[quest[i].k_idx].flags3 |= TR3_QUESTITEM;
					}

					/* Load quest flags */
					if (!z_older_than(2, 2, 3))
					{
						rd_byte(&quest[i].flags);
					}

					if (z_older_than(2, 2, 0))
					{
						strip_bytes(40);
					}

					/* Mark uniques */
					if (r_info[quest[i].r_idx].flags1 & RF1_UNIQUE)
							r_info[quest[i].r_idx].flags1 |= RF1_QUESTOR;
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

		/* Only in 2.2.1 and 2.2.2 */
		if (!z_older_than(2, 2, 1) && z_older_than(2, 2, 3))
		{
			/* "Hard quests" flag */
			rd_byte((byte*) &ironman_hard_quests);

			/* Inverted "Wilderness" flag */
			rd_byte((byte*) &vanilla_town);
			vanilla_town = !vanilla_town;
		}

		/* Position in the wilderness */
		rd_s32b(&p_ptr->wilderness_x);
		rd_s32b(&p_ptr->wilderness_y);

		/* Size of the wilderness */
		rd_s32b(&wild_x_size);
		rd_s32b(&wild_y_size);

		/* Incompatible save files */
		if ((wild_x_size > max_wild_size) || (wild_y_size > max_wild_size))
		{
			note(format("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size));
			return (23);
		}

		/* Hack - if size is zero - set to max_wild_size */
		if ((wild_x_size == 0) && (wild_y_size == 0))
		{
			wild_x_size = max_wild_size;
			wild_y_size = max_wild_size;
		}

		/* Hack - set size of wilderness to x size only */
		max_wild = wild_x_size;

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
	}
	/* rr9: Load old savegame without the quest infos */
	else if (z_older_than(2, 1, 1))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		/* Ignore all infos */
		for (i = 0; i < tmp16u; i++)
		{
			strip_bytes(4);
		}
	}
	/* rr9: Load 2.1.1 savegame quest infos */
	else if (z_older_than(2, 1, 2))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		j = tmp16u;

		/* Ignore the quests */
		for (i = 0; i < j; i++)
		{
			strip_bytes(5);
		}
	}
	/* 2.1.2 beta version */
	else if (z_older_than(2, 1, 3))
	{
		/* Load the number of quests */
		rd_u16b(&tmp16u);

		/* Incompatible save files */
		if (tmp16u > 20)
		{
			note(format("Too many (%u) quests!", tmp16u));
			return (23);
		}

		/* Load the quest information */
		for (i = 0; i < tmp16u; i++)
		{
			/* Throw it away */
			strip_bytes(14);
		}
	}

	/*
	 * Select the number of random quests
	 * when importing old savefiles.
	 */
	if (z_older_than(2, 2, 0))
	{
		char inp[80];
		int i, v;

		/* Wipe the quests */
		for (i = 0; i < max_quests; i++)
		{
			quest[i].status = QUEST_STATUS_UNTAKEN;

			quest[i].cur_num = 0;
			quest[i].max_num = 0;
			quest[i].type = 0;
			quest[i].level = 0;
			quest[i].r_idx = 0;
		}

		/* Clean up */
		clear_from(10);

		/*** User enters number of quests ***/
		/* Heino Vander Sanden and Jimmy De Laet */

		/* Extra info */
		Term_putstr(5, 15, -1, TERM_WHITE,
			"You can input yourself the number of quest you'd like to");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"perform next to two obligatory ones ( Oberon and the Serpent of Chaos )");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"In case you do not want any additional quest, just enter 0");

		/* Ask the number of additional quests */
		while (TRUE)
		{
			put_str(format("Number of additional quest? (<%u) ", MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 2), 20, 2);

			/* Get a the number of additional quest */
			while (TRUE)
			{
				/* Move the cursor */
				put_str("", 20, 37);

				/* Default */
				strcpy(inp, "20");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 2)) inp[0] = '\0';
				v = atoi(inp);

				/* Break on valid input */
				if ((v <= MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1) && (v >= 0)) break;
			}
			break;
		}

		/* Clear */
		clear_from(15);

		/* Init the random quests */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = MIN_RANDOM_QUEST;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		p_ptr->inside_quest = 0;

		/* Prepare allocation table */
		get_mon_num_prep(monster_quest, NULL);

		/* Generate quests */
		for (i = MIN_RANDOM_QUEST + v - 1; i >= MIN_RANDOM_QUEST; i--)
		{
			quest_type *q_ptr = &quest[i];
			monster_race *r_ptr = NULL;

			q_ptr->status = QUEST_STATUS_TAKEN;

			for (j = 0; j < MAX_TRIES; j++)
			{
				/* Random monster 5 - 10 levels out of depth */
				q_ptr->r_idx = get_mon_num(q_ptr->level + 4 + randint1(6));

				r_ptr = &r_info[q_ptr->r_idx];

				/* Accept only monsters that are out of depth */
				if (r_ptr->level > q_ptr->level) break;
			}

			/* Get the number of monsters */
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				/* Mark uniques */
				r_ptr->flags1 |= RF1_QUESTOR;

				q_ptr->max_num = 1;
			}
			else
			{
				q_ptr->max_num = 5 + (s16b)randint0(q_ptr->level/3 + 5);
			}
		}

		/* Init the two main quests (Oberon + Serpent) */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = QUEST_OBERON;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		quest[QUEST_OBERON].status = QUEST_STATUS_TAKEN;

		p_ptr->inside_quest = QUEST_SERPENT;
		process_dungeon_file("q_info.txt", 0, 0, 0, 0);
		quest[QUEST_SERPENT].status = QUEST_STATUS_TAKEN;
		p_ptr->inside_quest = 0;
	}

	/*
	 * Select 'hard random quests mode'
	 * when importing old savefiles.
	 */
	if (z_older_than(2, 2, 1))
	{
		char c;

		/* Clear */
		clear_from(15);

		/*** Hard quests mode ***/

		/* Extra info */
		Term_putstr(5, 14, -1, TERM_WHITE,
			"Using 'hard quests' mode makes the random quests harder, because");
		Term_putstr(5, 15, -1, TERM_WHITE,
			"you have to kill all monsters at the same visit to the quest level.");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"If you leave the level while some quest monsters are still alive,");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"then all killed quest monsters are revived on your next visit");
		Term_putstr(5, 18, -1, TERM_WHITE,
			"to this level.");

		/* Ask about "hard quests" mode */
		while (1)
		{
			put_str("Use 'Hard quests'? (y/n/*) ", 20, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			if (c == '*')
			{
				c = 'y';
				if (randint1(2) == 1)
					c = 'n';
				break;
			}
			if (c == ESCAPE) break;
			if ((c == 'y') || (c == 'n')) break;
			if (c == '?') do_cmd_help();
			else bell();
		}

		/* Set "hard quests" mode */
		ironman_hard_quests = (c == 'y');

		/* Clear */
		clear_from(15);
	}

	if (arg_fiddle) note("Loaded Quests");

	/* A version without the wilderness */
	if (z_older_than(2, 1, 2))
	{
		char c;

		/* Clear */
		clear_from(14);

		/*** Wilderness mode ***/

		/* Extra info */
		Term_putstr(5, 14, -1, TERM_WHITE,
			"'Wilderness' mode enables the extended wilderness of ZAngband");
		Term_putstr(5, 15, -1, TERM_WHITE,
			"giving you a wilderness and several new towns to explore.");
		Term_putstr(5, 16, -1, TERM_WHITE,
			"Switching off 'wilderness' mode is recommended for slower computers,");
		Term_putstr(5, 17, -1, TERM_WHITE,
			"because the wilderness slows down the system a bit.");

		/* Ask about "wilderness" mode */
		while (1)
		{
			put_str("Use 'wilderness'? (y/n/*) ", 20, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			if (c == '*')
			{
				c = 'y';
				if (randint1(2) == 1)
					c = 'n';
				break;
			}
			if (c == ESCAPE) break;
			if ((c == 'y') || (c == 'n')) break;
			if (c == '?') do_cmd_help();
			else bell();
		}

		/* Set "wilderness" mode */
		vanilla_town = (c == 'y');

		/* Clear */
		clear_from(14);
	}


	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > max_a_idx)
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

	for (i = 0; i < 64; i++)
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
	if (!z_older_than(2, 1, 3))
	{
		rd_u16b(&tmp16u);
		town_count = tmp16u;
	}
	else
	{
		/* Only one town */
		town_count = 2;
	}

	/* Paranoia */
	if (town_count > max_towns)
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
		
		for (i = 1; i < town_count; i++)
		{
			town[i].numstores = tmp16u;
		
			/* Allocate the stores */
			C_MAKE(town[i].store, town[i].numstores, store_type);
			
			/* HACK - ignore the empty towns */
			if (z_older_than(2, 2, 3) && (i >= 6))
			{
				for (j = 0; j < tmp16u; j++)
				{
					/* Read the info into the empty town 5 (R'Lyeh) */
					if (rd_store(5, j)) return (22);
				}
			}
			else
			{
				for (j = 0; j < tmp16u; j++)
				{
					if (rd_store(i, j)) return (22);
				}
			}
		}
	}
	else
	{
		/* Get the town data */
		for (i = 1; i < town_count; i++)
		{
			/* RNG seed */
			rd_u32b(&town[i].seed);

			/* Number of stores */
			rd_byte(&town[i].numstores);

			/* Type */
			rd_u16b(&town[i].type);
			
			if (sf_version > 21)
			{
				rd_byte(&town[i].pop);
			}

			/* Gates */
			if (sf_version > 22)
			{
				rd_byte(&town[i].gates_x[0]);
				rd_byte(&town[i].gates_x[1]);
				rd_byte(&town[i].gates_x[2]);
				rd_byte(&town[i].gates_x[3]);
				
				rd_byte(&town[i].gates_y[0]);
				rd_byte(&town[i].gates_y[1]);
				rd_byte(&town[i].gates_y[2]);
				rd_byte(&town[i].gates_y[3]);
			}

			/* Locatation */
			rd_byte(&town[i].x);
			rd_byte(&town[i].y);

			/* Name */
			rd_string(town[i].name, 32);
			
			/* Allocate the stores */
			C_MAKE(town[i].store, town[i].numstores, store_type);

			/* Get the stores of all towns */
			for (j = 0; j < town[i].numstores; j++)
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
#ifdef USE_SCRIPT
			if (tmp32s)
			{
				char *callbacks = (char*) malloc(tmp32s + 1);
				rd_string(callbacks, tmp32s + 1);
				load_game_callback(callbacks);
				free(callbacks);
			}
#else /* USE_SCRIPT */
			strip_bytes(tmp32s);
#endif /* USE_SCRIPT */
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

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

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
