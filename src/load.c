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
 * Verbal checksum
 */
static u32b checksum_base;
static u32b checksum;


/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msgf()".
 */
static void note(cptr fmt, ...)
{
	static int y = 2;

	va_list vp;

	char msg[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(msg, 1024, fmt, &vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Draw the message */
	prtf(0, y, msg);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	Term_fresh();

	/* End the Varargs Stuff */
	va_end(vp);
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
	checksum ^= (u32b)(*ip);
}

static void rd_u16b(u16b *ip)
{
	(*ip) = sf_get();
	(*ip) |= ((u16b)(sf_get()) << 8);
	checksum ^= (u32b)(*ip);
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
	checksum ^= *ip;
}

static void rd_s32b(s32b *ip)
{
	rd_u32b((u32b *)ip);
}

static void rd_checksum(cptr msg)
{
	u32b test;

	/* Read the checksum out of the file */
	test = (getc(fff) & 0xFF);
	test |= ((u32b)((getc(fff) & 0xFF)) << 8);
	test |= ((u32b)((getc(fff) & 0xFF)) << 16);
	test |= ((u32b)((getc(fff) & 0xFF)) << 24);

	/* Check it */
	if (test != checksum && SAVEFILE_VERSION != sf_version)
	{
		get_check("Verbal checksum failure (%i, %i): %s", test, checksum, msg);
		quit ("Savefile unreadable: checksum failure");
	}

	/* Reset the checksum */
	checksum = checksum_base;
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

#ifdef UNUSED_FUNC

/*
 * Hack -- strip a string
 */
static void strip_string(void)
{
	byte tmp8u;

	/* Read the string */
	do
	{
		/* Read a byte */
		rd_byte(&tmp8u);
	}
	/* End of string */
	while (tmp8u);
}

#endif /* UNUSED_FUNC */

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

	object_kind *k_ptr;

	char buf[1024];
	int i;

	/* Number of object flags */
	byte n_flags;

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_s16b(&o_ptr->iy);
	rd_s16b(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	rd_byte(&o_ptr->discount);
	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);

	rd_s32b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&old_dd);
	rd_byte(&old_ds);

	rd_byte(&o_ptr->info);

	rd_byte(&n_flags);
	if (n_flags > NUM_TR_SETS)
		abort();

	/* Object flags */
	for (i = 0; i < n_flags; i++)
		rd_u32b(&o_ptr->flags[i]);
	for (i = n_flags; i < NUM_TR_SETS; i++)
		o_ptr->flags[i] = 0;

	rd_s16b(&o_ptr->next_o_idx);

	if (o_ptr->tval == TV_CONTAINER && sf_version >= 56)
		rd_s16b(&o_ptr->contents_o_idx);

	rd_byte((byte *)(&o_ptr->allocated));

	rd_byte(&o_ptr->feeling);

	/* Inscription */
	rd_string(buf, 1024);

	/* Save the inscription */
	if (buf[0]) o_ptr->inscription = quark_add(buf);

	rd_string(buf, 1024);
	if (buf[0]) o_ptr->xtra_name = quark_add(buf);

	rd_byte(&tmpbyte);

	while (tmpbyte != 255)
	{
		rd_string(buf, 1024);

		if (tmpbyte < MAX_TRIGGER)
		{
			o_ptr->trigger[tmpbyte] = quark_add(buf);
		}
		else
		{
			/* XXX Error */
		}

		rd_byte(&tmpbyte);
	}

	/* The Python object */
	{
		s32b tmp32s;

		rd_s32b(&tmp32s);
		strip_bytes(tmp32s);
	}

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

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

		rd_u16b(&o_ptr->a_idx);
		rd_u16b(&o_ptr->e_idx);

		for (i = 0; i < n_flags; i++)
			rd_u32b(&o_ptr->kn_flags[i]);
		for (i = n_flags; i < NUM_TR_SETS; i++)
			o_ptr->kn_flags[i] = 0;

		if (o_ptr->a_idx && sf_version < 46)
		{
			if (o_ptr->a_idx < 128)
			{
				/* Remove old randart activations */
				o_ptr->a_idx = 0;
				o_ptr->flags[2] &= ~TR2_ACTIVATE;
			}
			else
			{
				/* Now just use a_idx */
				o_ptr->a_idx -= 128;
			}
		}

		/*
		 * Add appropriate scripts to artifacts that are missing them,
		 * for older savefiles
		 */
		if (o_ptr->a_idx)
		{
			int i;

			for (i = 0; i < MAX_TRIGGER; i++)
			{
				if (a_info[o_ptr->a_idx].trigger[i] &&
						!o_ptr->trigger[i])
				{
					o_ptr->trigger[i] = quark_add(
						a_text + a_info[o_ptr->a_idx].trigger[i]);
				}
			}
		}

		/*
		 * XXX Some older buggy versions set TR2_PERMA_CURSE
		 * on items where it shouldn't have been set.
		 */
		o_ptr->kn_flags[2] &= o_ptr->flags[2] |
			~(TR2_HEAVY_CURSE | TR2_PERMA_CURSE);

		/* Object memory stuff added at this point */
		if (sf_version > 53)
		{
			rd_byte(&o_ptr->mem.type);
			rd_u16b(&o_ptr->mem.place_num);
			rd_byte(&o_ptr->mem.depth);
			rd_u32b(&o_ptr->mem.data);
		}
		else
		{
			o_ptr->mem.type = OM_NONE;
			o_ptr->mem.place_num = 0;
			o_ptr->mem.depth = 0;
			o_ptr->mem.data = 0;
		}

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
		o_ptr->a_idx = 0;

		/* Reset flags */
		for (i = 0; i < NUM_TR_SETS; i++)
			o_ptr->flags[i] = k_ptr->flags[i];

		/* All done */
		return;
	}

	/* Hack -- extract the "broken" flag */
	if (o_ptr->pval < 0) o_ptr->cost = 0;

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
	rd_u16b(&m_ptr->unsummon);

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

	if (sf_version >= 57)
	{
		rd_byte(&m_ptr->silenced);
		rd_byte(&m_ptr->imprisoned);
	}
	else
	{
		m_ptr->silenced = 0;
		m_ptr->imprisoned = 0;
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

	/* Current */
	{
		if (sf_version >= 58 && r_idx >= HERO_MIN)
		{
			rd_s16b(&h_list[r_idx-HERO_MIN].r_idx);
			rd_byte(&h_list[r_idx-HERO_MIN].flags);
			rd_byte(&h_list[r_idx-HERO_MIN].offset);
			rd_u32b(&h_list[r_idx-HERO_MIN].seed);

			/* restore the hero now, so lore overwrites
			   the stuff placed there automatically */
			restore_hero(r_idx-HERO_MIN);
		}

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
		rd_u32b(&r_ptr->r_flags[0]);
		rd_u32b(&r_ptr->r_flags[1]);
		rd_u32b(&r_ptr->r_flags[2]);
		rd_u32b(&r_ptr->r_flags[3]);
		rd_u32b(&r_ptr->r_flags[4]);
		rd_u32b(&r_ptr->r_flags[5]);
		if (sf_version > 51) rd_u32b(&r_ptr->r_flags[6]);


		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Later (?) */
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}

	/* Repair the lore flags */
	r_ptr->r_flags[0] &= r_ptr->flags[0];
	r_ptr->r_flags[1] &= r_ptr->flags[1];
	r_ptr->r_flags[2] &= r_ptr->flags[2];
	r_ptr->r_flags[3] &= r_ptr->flags[3];
	r_ptr->r_flags[4] &= r_ptr->flags[4];
	r_ptr->r_flags[5] &= r_ptr->flags[5];

	rd_checksum ("Lore record");
}




/*
 * Read a store
 */
static void rd_store(int town_num, int store_num)
{
	store_type *st_ptr = &place[town_num].store[store_num];

	int j;

	s16b data;

	byte allocated, type = 0, owner = 0;
	s16b max_cost;
	byte greed;

	char buf[256];

	/* Read the basic info */
	if (sf_version < 34)
	{
		strip_bytes(4);
	}

	rd_s16b(&data);
	if (sf_version < 49)
	{
		rd_byte(&owner);
	}
	else
	{
		rd_string(buf, 256);
		st_ptr->owner_name = quark_add(buf);
		rd_s16b(&max_cost);
		rd_byte(&greed);
	}

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

	/* Finish initialisation */
	if (sf_version >= 49)
	{
		st_ptr->owner_name = quark_add(buf);
		st_ptr->max_cost = max_cost;
		st_ptr->greed = greed;
	}

	/* Restore the saved parameters */
	st_ptr->data = data;

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

	/* Checksum */
	rd_checksum("Randomizer information");
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

	if (c & 0x0002) p_ptr->state.wizard = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;

	/* Autosave options */
	rd_byte(&autosave_l);
	rd_byte(&autosave_t);
	if (sf_version >= 56) rd_byte(&autosave_b);
	else autosave_b = TRUE;
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

	/* Read the squelches */
	for (n = 0; n < SQUELCHMAX/32; n++) rd_u32b(&(p_ptr->squelch[n]));

	/* Checksum */
	rd_checksum("Game options");

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

	/* Set all auto_destroy options to FALSE for old savefiles. */
	if (sf_version <= 54)
	{
		auto_destroy_bad = FALSE;
		auto_destroy_weap = FALSE;
		auto_destroy_arm = FALSE;
		auto_destroy_cloak = FALSE;
		auto_destroy_shield = FALSE;
		auto_destroy_helm = FALSE;
		auto_destroy_gloves = FALSE;
		auto_destroy_boots = FALSE;
		quick_destroy_avg = FALSE;
		quick_destroy_good = FALSE;
		quick_destroy_all = FALSE;
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

static void rd_rebirth(void)
{
	int i;
	
	if (sf_version >= 60)
	{
		rd_s16b(&rebirth_ptr->rp.age);
		rd_s16b(&rebirth_ptr->rp.ht);
		rd_s16b(&rebirth_ptr->rp.wt);
		rd_s16b(&rebirth_ptr->rp.sc);
		rd_byte(&rebirth_ptr->rp.psex);
		rd_byte(&rebirth_ptr->rp.prace);
		rd_byte(&rebirth_ptr->rp.pclass);
		rd_byte(&rebirth_ptr->realm[0]);
		rd_byte(&rebirth_ptr->realm[1]);
		for (i = 0; i < A_MAX; i++)
		{
			rd_s16b(&rebirth_ptr->stat[i]);
		}
		for (i = 0; i < PY_MAX_LEVEL; i++)
		{
			rd_s16b(&rebirth_ptr->player_hp[i]);
		}
		rd_s16b(&rebirth_ptr->chaos_patron);
		rd_s32b(&rebirth_ptr->au);
		rd_u32b(&rebirth_ptr->world_seed);
		rd_byte(&rebirth_ptr->can_rebirth);
		rd_checksum("Rebirth info");
	}
	else
		rebirth_ptr->can_rebirth = FALSE;
}


/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i;

	byte tmp8u;
	s16b tmp16s;
	s16b dummy;

	char old_history[60];

	rd_string(player_name, 32);

	rd_string(p_ptr->state.died_from, 80);

	/* Read and ignore old history data */
	for (i = 0; i < 4; i++)
	{
		rd_string(old_history, 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->rp.prace);
	rd_byte(&p_ptr->rp.pclass);
	rd_byte(&p_ptr->rp.psex);
	rd_byte(&p_ptr->spell.realm[0]);
	rd_byte(&p_ptr->spell.realm[1]);
	if (sf_version >= 57)
	{
		for (i = 0; i < SPELL_LAYERS; i++)
		{
			rd_s16b(&p_ptr->spell_slots[i]);
		}
	}
	else
	{
		/* Forgotten all spells, so assign spell_slots as in birth routine. */
		for (i = 0; i < SPELL_LAYERS; i++)
		{
			p_ptr->spell_slots[i] =
				magic_info[p_ptr->rp.pclass].max_spells[i];
		}
	}
	rd_byte(&tmp8u);			/* oops */

	/* Special Race/Class info */
	rd_byte(&p_ptr->rp.hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->rp.age);
	rd_s16b(&p_ptr->rp.ht);
	rd_s16b(&p_ptr->rp.wt);

	/* Read the stat info */
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat[i].max);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat[i].cur);

	/* Fix up stats for old savefiles */
	if (sf_version < 39)
	{
		/* This will be initialized again later, but we need it now for adjust_stat to work */
		rp_ptr = &race_info[p_ptr->rp.prace];
		cp_ptr = &class_info[p_ptr->rp.pclass];

		for (i = 0; i < 6; i++)
		{
			int bonus = race_info[p_ptr->rp.prace].r_adj[i] +
				class_info[p_ptr->rp.pclass].c_adj[i];

			p_ptr->stat[i].max = adjust_stat(i, p_ptr->stat[i].max, bonus);
			/* Hack - Restore all stats... */
			p_ptr->stat[i].cur = p_ptr->stat[i].max;
		}
    }
    if (sf_version < 40)
    {
    	for (i = 0; i < 6; i++)
    	{
    		if (p_ptr->stat[i].max < 18)
    			p_ptr->stat[i].max *= 10;
    		else
    			p_ptr->stat[i].max += 180-18;
    		if (p_ptr->stat[i].cur < 18)
    			p_ptr->stat[i].cur *= 10;
    		else
    			p_ptr->stat[i].cur += 180-18;
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
	strip_bytes(2);				/* Old "max_depth" */

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

	/* More info */
	strip_bytes(8);
	rd_s16b(&p_ptr->rp.sc);
	strip_bytes(2);

	/* Read the flags */
	if (sf_version <= 56)
	{
		strip_bytes(2);				/* Old "rest" */
		rd_s16b(&p_ptr->tim.blind);
		rd_s16b(&p_ptr->tim.paralyzed);
		rd_s16b(&p_ptr->tim.confused);
		rd_s16b(&p_ptr->food);
		strip_bytes(4);				/* Old "food_digested" / "protection" */
		rd_s16b(&p_ptr->energy);
		rd_s16b(&p_ptr->tim.fast);
		rd_s16b(&p_ptr->tim.slow);
		rd_s16b(&p_ptr->tim.afraid);
		rd_s16b(&p_ptr->tim.cut);
		rd_s16b(&p_ptr->tim.stun);
		rd_s16b(&p_ptr->tim.poisoned);
		rd_s16b(&p_ptr->tim.image);
		rd_s16b(&p_ptr->tim.protevil);
		rd_s16b(&p_ptr->tim.invuln);
		rd_s16b(&p_ptr->tim.hero);
		rd_s16b(&p_ptr->tim.shero);
		rd_s16b(&p_ptr->tim.shield);
		rd_s16b(&p_ptr->tim.blessed);
		rd_s16b(&p_ptr->tim.see_invis);
		rd_s16b(&p_ptr->tim.word_recall);
		rd_s16b(&p_ptr->see_infra);
		rd_s16b(&p_ptr->tim.infra);
		rd_s16b(&p_ptr->tim.oppose_fire);
		rd_s16b(&p_ptr->tim.oppose_cold);
		rd_s16b(&p_ptr->tim.oppose_acid);
		rd_s16b(&p_ptr->tim.oppose_elec);
		rd_s16b(&p_ptr->tim.oppose_pois);

		/* Old savefiles do not have the following fields... */
		if ((z_major == 2) && (z_minor == 0) && (z_patch == 6))
		{
			p_ptr->tim.esp = 0;
			p_ptr->tim.wraith_form = 0;
			p_ptr->tim.resist_magic = 0;
			p_ptr->chaos_patron = get_chaos_patron();
			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;
			p_ptr->tim.oppose_conf = 0;
			p_ptr->tim.oppose_blind = 0;
			p_ptr->tim.etherealness = 0;

			get_virtues();
		}
		else
		{
			rd_s16b(&p_ptr->tim.oppose_conf);
			rd_s16b(&p_ptr->tim.oppose_blind);
			rd_s16b(&p_ptr->tim.etherealness);
			rd_s16b(&p_ptr->tim.esp);
			rd_s16b(&p_ptr->tim.wraith_form);
			rd_s16b(&p_ptr->tim.resist_magic);
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
	}
	else
	{
		rd_s16b(&p_ptr->food);
		rd_s16b(&p_ptr->energy);
		rd_s16b(&p_ptr->see_infra);
		rd_s16b(&p_ptr->chaos_patron);
		rd_u32b(&p_ptr->muta1);
		rd_u32b(&p_ptr->muta2);
		rd_u32b(&p_ptr->muta3);

		for (i = 0; i < MAX_TIMED_RESERVED; i++)
		{
			s16b *tim_ptr;

			if (i >= MAX_TIMED)
			{
				strip_bytes(2);
				continue;
			}

			tim_ptr = get_timed_ptr(i);
			if (!tim_ptr) strip_bytes(2);
			else rd_s16b(tim_ptr);
		}

		for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
		{
			rd_s16b(&p_ptr->virtues[i]);
		}

		for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
		{
			rd_s16b(&p_ptr->vir_types[i]);
		}
	}

	rd_byte(&p_ptr->state.confusing);
	rd_byte(&p_ptr->state.lich);
	rd_byte(&tmp8u);			/* oops */
	rd_byte(&tmp8u);			/* oops */
	rd_byte((byte *)&p_ptr->state.searching);
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
	rd_u16b(&p_ptr->state.panic_save);
	rd_u16b(&p_ptr->state.total_winner);
	rd_u16b(&p_ptr->state.noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	p_ptr->state.is_dead = tmp8u;

	/* Read "feeling" */
	rd_byte(&p_ptr->state.feeling);

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);

	if (sf_version > 60)
		rd_s32b(&turn_offset);
	else
	{
		/* Adjust turn to reflect new day length */
		s32b new_turn;
		int days = turn / OLD_TOWN_DAWN;
		int curtime = turn % OLD_TOWN_DAWN;
		/* Calculate number of turns into the current day we are */
		/* Hack: take advantage of the fact that both day spans are multiples of 100, so
		 * we don't have magic constants.
		 */
		new_turn = curtime + (curtime * ((TOWN_DAY-OLD_TOWN_DAWN)/100))/(OLD_TOWN_DAWN/100);
		/* Add in turns for all past days */
		new_turn += days * TOWN_DAY;
		/* These extra turns don't "count" */
		turn_offset = new_turn - turn;
		turn = new_turn;
	}
	
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

	rd_checksum("Extra information");
}


/*
 * Mega-Hack , in order to load old savefiles, we
 * need to save the player's inventory temporarily.
 */
static object_type old_inventory[24];

/*
 * Read the player inventory
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

	rd_checksum("Inventory information");

	/* Success */
	return (0);
}



/*
 * Read the saved messages
 */
static void rd_messages(void)
{
	int i;
	char buf[1024];
	byte tmp8u;

	u16b num;

	/* Total */
	rd_u16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 1024);

		/* Read the color */
		if (sf_version > 10)
			rd_byte(&tmp8u);
		else
			tmp8u = MSG_GENERIC;

		/* Save the message */
		message_add(buf, tmp8u);
	}

	rd_checksum("Recent messages");
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

				/* Extract "player info" (only use detect grid and known grid data) */
				pc_ptr->player = tmp8u & (GRID_DTCT | GRID_KNOWN);

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
#define VERSION_CHANGE_WILD		48


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
	monster_race *r_ptr;

	bool ignore_stuff = FALSE;

	dun_type *dundata = place[p_ptr->place_num].dungeon;

	s16b cur_wid, cur_hgt;



	/* Get size */
	Term_get_size(&wid, &hgt);

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&p_ptr->depth);

	rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);

	rd_s16b(&p_ptr->panel_x1);
	rd_s16b(&p_ptr->panel_y1);
	rd_s16b(&p_ptr->panel_x2);
	rd_s16b(&p_ptr->panel_y2);

	/* The player may not be in the dungeon */
	character_dungeon = FALSE;

	/* Assume we are in the dungeon */
	p_ptr->max_hgt = cur_hgt;
	p_ptr->min_hgt = 0;
	p_ptr->max_wid = cur_wid;
	p_ptr->min_wid = 0;

	/* Load wilderness data */
	load_wild_data();

	/* Reinsert to fix broken autosave files */
	/* p_ptr->depth = 0; */

	change_level(p_ptr->depth);

	if (p_ptr->depth)
	{
		/* Get the new region */
		create_region(dundata, cur_wid, cur_hgt, REGION_CAVE);
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

	/* Set current_quest if applicable */
	if (place[p_ptr->place_num].quest_num && p_ptr->depth)
	{
		current_quest = &quest[place[p_ptr->place_num].quest_num];
	}
	else
		current_quest = NULL;


	rd_checksum("Current dungeon: map & basic info");

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

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > z_info->o_max)
	{
		note("Too many (%d) object entries!", limit);
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
			/* Hack - ignore items */
			if (ignore_stuff && (o_ptr->ix || o_ptr->iy) &&
				 !in_bounds2(o_ptr->ix, o_ptr->iy))
			{
				object_wipe(o_ptr);
				continue;
			}

			/* Count objects */
			o_cnt++;

			/* Dungeon items */
			if (o_ptr->ix || o_ptr->iy)
			{
				/* Hack - set region of object if is in the dungeon */
				o_ptr->region = cur_region;

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
			}
		}
	}

	rd_checksum("Current dungeon: object info");

	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit > z_info->m_max)
	{
		note("Too many (%d) monster entries!", limit);
		return (161);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		int m_idx;

		monster_type *m_ptr;

		/* Get a new record */
		m_idx = m_pop();

		/* Acquire monster */
		m_ptr = &m_list[m_idx];

		/* Read the monster */
		rd_monster(m_ptr);

		/* Hack - set region of monster */
		m_ptr->region = cur_region;

		if (!ignore_stuff && in_bounds2(m_ptr->fx, m_ptr->fy))
		{
			/* Oops */
			if (i != m_idx)
			{
				note("Monster allocation error (%d <> %d)", i, m_idx);
				return (162);
			}

			/* Access grid */
			c_ptr = area(m_ptr->fx, m_ptr->fy);

			/* Mark the location */
			c_ptr->m_idx = m_idx;

			/* Access race */
			r_ptr = &r_info[m_ptr->r_idx];

			/* Count XXX XXX XXX */
			r_ptr->cur_num++;


		}
		else
		{
			/* Delete objects */
			delete_object_list(&m_ptr->hold_o_idx);

			/* Hack - just delete the monster */
			(void)WIPE(m_ptr, monster_type);

			/* Count monsters */
			m_cnt--;
		}
	}

	/* Dump info about the player clone */
	if (sf_version >= 57)
	{
	    r_ptr = &r_info[QW_CLONE];
		rd_s16b(&r_ptr->hdice);
		rd_s16b(&r_ptr->hside);
		rd_s16b(&r_ptr->ac);
		rd_s16b(&r_ptr->sleep);
		rd_byte(&r_ptr->aaf);
		rd_byte(&r_ptr->speed);
		rd_byte(&r_ptr->freq_inate);
		rd_byte(&r_ptr->freq_spell);

		for (i = 0; i < 9; i++)
		{
			rd_u32b(&r_ptr->flags[i]);
		}

		for (i = 0; i < 4; i++)
		{
			rd_byte(&r_ptr->blow[i].method);
			rd_byte(&r_ptr->blow[i].effect);
			rd_byte(&r_ptr->blow[i].d_dice);
			rd_byte(&r_ptr->blow[i].d_side);
		}
	}
	else
	{
		create_clone(FALSE);
	}

	rd_checksum("Current dungeon: monster info");

	/*** Fields ***/
	/* Read the field count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > z_info->fld_max)
	{
		note("Too many (%d) field entries!", limit);
		return (151);
	}

	/* Read the fields */
	for (i = 1; i < limit; i++)
	{
		field_type temp_field;
		field_type *f_ptr = &temp_field;

		/* Read the field */
		rd_field(f_ptr);

		/* Hack - set region of field */
		f_ptr->region = cur_region;

		if (!ignore_stuff && in_bounds2(f_ptr->fx, f_ptr->fy))
		{
			/* Access the fields location */
			c_ptr = area(f_ptr->fx, f_ptr->fy);

			/* Build a stack */
			field_add(f_ptr, c_ptr);
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

	/*
	 * Set the trap detected flag.
	 *
	 * This is done here because it needs to be below all calls
	 * to "change_level()"
	 */
	p_ptr->state.detected = player_detected;

	rd_checksum("Current dungeon: field info");

	/* Success */
	return (0);
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

		rd_s32b(&q_ptr->timeout);

		rd_byte(&q_ptr->level);

		rd_string(q_ptr->name, 256);

		/* Data - quest-type specific */
		switch (q_ptr->type)
		{
			case QUEST_TYPE_NONE:
			{
				/* Un-initialised quests */
				break;
			}

			case QUEST_TYPE_BOUNTY:
			case QUEST_TYPE_DEFEND:
			{
				/* Bounty quests */
				rd_u16b(&q_ptr->data.bnt.r_idx);
				rd_u16b(&q_ptr->data.bnt.cur_num);
				rd_u16b(&q_ptr->data.bnt.max_num);
				break;
			}

			case QUEST_TYPE_DUNGEON:
			{
				/* Dungeon quests */
				rd_u16b(&q_ptr->data.dun.r_idx);
				rd_u16b(&q_ptr->data.dun.level);

				rd_s16b(&q_ptr->data.dun.cur_num);
				rd_s16b(&q_ptr->data.dun.max_num);
				rd_s16b(&q_ptr->data.dun.num_mon);
				break;
			}

			case QUEST_TYPE_WILD:
			{
				/* Wilderness quests */
				rd_u16b(&q_ptr->data.wld.place);
				rd_u16b(&q_ptr->data.wld.data);
				rd_byte(&q_ptr->data.wld.depth);
				break;
			}

			case QUEST_TYPE_MESSAGE:
			case QUEST_TYPE_LOAN:
			{
				/* Message quests */
				rd_u16b(&q_ptr->data.msg.place);
				rd_u16b(&q_ptr->data.msg.shop);
				break;
			}

			case QUEST_TYPE_FIND_ITEM:
			{
				/* Find item quests */
				rd_u16b(&q_ptr->data.fit.a_idx);
				rd_u16b(&q_ptr->data.fit.place);

				/* The artifact is a quest item */
				SET_FLAG(&a_info[q_ptr->data.fit.a_idx], TR_QUESTITEM);
				break;
			}

			case QUEST_TYPE_FIND_PLACE:
			{
				/* Find place quests */
				rd_u16b(&q_ptr->data.fpl.place);
				break;
			}

			case QUEST_TYPE_FIXED_KILL:
			{
				rd_u32b(&q_ptr->data.fix.d_type);
				rd_u32b(&q_ptr->data.fix.d_flags);
				rd_u32b(&q_ptr->data.fix.seed);
				rd_s32b(&q_ptr->data.fix.x);
				rd_s32b(&q_ptr->data.fix.y);
				rd_byte(&q_ptr->data.fix.min_level);
				rd_s32b(&q_ptr->data.fix.attempts);
				rd_u16b(&q_ptr->data.fix.data.kill.r_idx);
				break;
			}
			case QUEST_TYPE_FIXED_BOSS:
			{
				rd_u32b(&q_ptr->data.fix.d_type);
				rd_u32b(&q_ptr->data.fix.d_flags);
				rd_u32b(&q_ptr->data.fix.seed);
				rd_s32b(&q_ptr->data.fix.x);
				rd_s32b(&q_ptr->data.fix.y);
				rd_byte(&q_ptr->data.fix.min_level);
				rd_s32b(&q_ptr->data.fix.attempts);
				rd_u16b(&q_ptr->data.fix.data.boss.r_idx);
				break;
			}
			case QUEST_TYPE_FIXED_CLEAROUT:
			{
				rd_u32b(&q_ptr->data.fix.d_type);
				rd_u32b(&q_ptr->data.fix.d_flags);
				rd_u32b(&q_ptr->data.fix.seed);
				rd_s32b(&q_ptr->data.fix.x);
				rd_s32b(&q_ptr->data.fix.y);
				rd_byte(&q_ptr->data.fix.min_level);
				rd_s32b(&q_ptr->data.fix.attempts);
				rd_byte(&q_ptr->data.fix.data.clearout.levels);
				rd_byte(&q_ptr->data.fix.data.clearout.cleared);
				break;
			}
			case QUEST_TYPE_FIXED_DEN:
			{
				rd_u32b(&q_ptr->data.fix.d_type);
				rd_u32b(&q_ptr->data.fix.d_flags);
				rd_u32b(&q_ptr->data.fix.seed);
				rd_s32b(&q_ptr->data.fix.x);
				rd_s32b(&q_ptr->data.fix.y);
				rd_byte(&q_ptr->data.fix.min_level);
				rd_s32b(&q_ptr->data.fix.attempts);
				rd_u16b(&q_ptr->data.fix.data.den.mg_idx);
				rd_byte(&q_ptr->data.fix.data.den.levels);
				rd_byte(&q_ptr->data.fix.data.den.cleared);
				break;
			}

			default:
			{
				/* Unknown quest type... panic */
				quit("Loading unknown quest type.");
			}
		}
		rd_checksum("Quest record");

		/* Repair broken quests if necessary */
		{
			monster_race *r_ptr = NULL;
			int r_idx = 0;

			/* Find the target monster race */
			if (q_ptr->type == QUEST_TYPE_BOUNTY || q_ptr->type == QUEST_TYPE_DEFEND)
			{
				r_idx = q_ptr->data.bnt.r_idx;
			}
			else if (q_ptr->type == QUEST_TYPE_FIXED_KILL)
			{
				r_idx = q_ptr->data.fix.data.kill.r_idx;
			}
			else if (q_ptr->type == QUEST_TYPE_FIXED_BOSS)
			{
				r_idx = q_ptr->data.fix.data.boss.r_idx;
			}
			else if (q_ptr->type == QUEST_TYPE_DUNGEON)
			{
				r_idx = q_ptr->data.dun.r_idx;
			}
			else
				r_idx = -1;

			if (r_idx != -1)
				r_ptr = &r_info[r_idx];

			/* Quests with an inappropriate target, mark as finished. */
			if (r_idx == 0)
			{
				q_ptr->status = QUEST_STATUS_FINISHED;
			}

			/* Check for a quest with a dead, unique boss */
			if (r_ptr && FLAG(r_ptr, RF_UNIQUE) && r_ptr->max_num == 0)
			{
				/* Make sure the quest is not open */
				if (q_ptr->status == QUEST_STATUS_TAKEN)
					q_ptr->status = QUEST_STATUS_COMPLETED;
				else if (q_ptr->status == QUEST_STATUS_UNTAKEN)
					q_ptr->status = QUEST_STATUS_FINISHED;
			}

			/* Check for a kill quest with an inappropriate, escorted boss */
			if (r_ptr && (FLAG(r_ptr, RF_ESCORT) || FLAG(r_ptr, RF_ESCORTS)) &&
				q_ptr->type == QUEST_TYPE_FIXED_KILL)
				/* Hack: should work, since the boss data and kill data are the same right now. */
				q_ptr->type = QUEST_TYPE_FIXED_BOSS;

			/* Check for quests off the edge of the map. */
			if (q_ptr->type == QUEST_TYPE_FIXED_BOSS ||
				q_ptr->type == QUEST_TYPE_FIXED_KILL ||
				q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ||
				q_ptr->type == QUEST_TYPE_FIXED_DEN)
			{
				/* Find the quest stairs */
				int j;
				place_type * pl_ptr;

				for (j = 0; j < place_count; j++)
				{
					pl_ptr = &place[j];

					if (pl_ptr->quest_num == i)
					{
						if (pl_ptr->x >= max_wild-1) pl_ptr->x = max_wild-2;
						if (pl_ptr->y >= max_wild-1) pl_ptr->y = max_wild-2;
					}

					/* This will also redraw quests near the player that should show up and used to not. */
					refresh_quest_stair(pl_ptr);
				}
			}

			/* Avoid lvl 0 dungeon quests */
			if (q_ptr->type == QUEST_TYPE_DUNGEON && q_ptr->data.dun.level == 0)
				q_ptr->data.dun.level++;

			/* Avoid lvl >= 100 in quests */
			if (q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ||
				q_ptr->type == QUEST_TYPE_FIXED_DEN)
			{
				byte *n = (q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ? &q_ptr->data.fix.data.clearout.levels :
												&q_ptr->data.fix.data.den.levels);

				if (q_ptr->data.fix.min_level > 99)
					q_ptr->data.fix.min_level = 99;

				if (q_ptr->data.fix.min_level + *n > 100)
					q_ptr->data.fix.min_level = 99-*n;

				if (q_ptr->data.fix.min_level == 0)
					q_ptr->data.fix.min_level = 1;

				/* Check if the quest is now complete, just in case */
				if ((q_ptr->type == QUEST_TYPE_FIXED_CLEAROUT ? q_ptr->data.fix.data.clearout.cleared :
						q_ptr->data.fix.data.den.cleared) >= *n)
				{
					if (q_ptr->status <= QUEST_STATUS_TAKEN)
						q_ptr->status = QUEST_STATUS_FINISHED;
				}
			}

			/* Upgrade kill & boss quests to heroes */
			if (q_ptr->type == QUEST_TYPE_FIXED_BOSS ||
				q_ptr->type == QUEST_TYPE_FIXED_KILL)
			{
				int h_r_idx = create_hero(r_idx, damroll(4,2)-3, TRUE);

				if (h_r_idx)
				{
					if (q_ptr->type == QUEST_TYPE_FIXED_BOSS)
						q_ptr->data.fix.data.boss.r_idx = h_r_idx;
					else
						q_ptr->data.fix.data.kill.r_idx = h_r_idx;
				}
			}
		}
	}
}

static void convert_spells(void)
{
	int i, j, r, lev, slot;
	bool found;
	int num_lost = 0;
	spell_external sp;

	/* For earlier versions, no converting. */
	if (sf_version < 57)
	{
		msgf ("Warning: You have forgotten all of your %s.",
			mp_ptr->spell_book == TV_LIFE_BOOK ? "prayers" : "spells");
		pause_line(1);
		return;
	}

	/* Go through the player's spells looking for dead spells. */
	for (i = p_ptr->spell.spell_max - 1; i >= 0; i--)
	{
		found = FALSE;
		for (r = 0; r < 2; r++)
		{
			for (j = 0; j < NUM_SPELLS; j++)
			{
				if (p_ptr->spell.data[i].s_idx != s_info[p_ptr->spell.realm[r]-1][j].s_idx) continue;
				if (p_ptr->spell.data[i].realm != s_info[p_ptr->spell.realm[r]-1][j].realm) continue;

				/* Skip "fake" spells */
				if (s_info[p_ptr->spell.realm[r]-1][j].sval > 3) continue;

				/* Found it */
				found = TRUE;
				break;
			}
		}

		/* Spells never found must be removed */
		if (!found)
		{
			for (j = i; j < p_ptr->spell.spell_max - 1; j++)
			{
				/* Shift next spell one slot back */
				p_ptr->spell.data[j].s_idx = p_ptr->spell.data[j+1].s_idx;
				p_ptr->spell.data[j].realm = p_ptr->spell.data[j+1].realm;
				p_ptr->spell.data[j].flags = p_ptr->spell.data[j+1].flags;
				p_ptr->spell.data[j].focus = p_ptr->spell.data[j+1].focus;
				p_ptr->spell.data[j].spell[0] = p_ptr->spell.data[j+1].spell[0];
				p_ptr->spell.data[j].spell[1] = p_ptr->spell.data[j+1].spell[1];
			}

			/* Blank out the entry at the end */
			WIPE(&p_ptr->spell.data[p_ptr->spell.spell_max-1], player_spell_learned);

			/* Lost one */
			num_lost++;

			p_ptr->spell.spell_max--;

			/* Don't do spell slots now; instead, if we lose any, reoptimize completely later. */
		}
	}

	if (num_lost)
	{
		msgf ("Warning: You have forgotten %d %s%s.", num_lost,
				mp_ptr->spell_book == TV_LIFE_BOOK ? "prayer" : "spell", num_lost > 1 ? "s" : "");
		pause_line(1);

		/* Reset spell slots to initial values */
		for (i = 0; i < SPELL_LAYERS; i++)
		{
			p_ptr->spell_slots[i] =
				magic_info[p_ptr->rp.pclass].max_spells[i];
		}

		/* Optimize slot use */
		for (i = 0; i < p_ptr->spell.spell_max; i++)
		{
			/* Find the (external) spell. */
			if (p_ptr->spell.data[i].flags & SP_PRESENT_1 &&
			    p_ptr->spell.data[i].flags & SP_PRESENT_2)
			{
				/* For spells in both realms, use lower level one */
				if (s_info[p_ptr->spell.realm[0]-1][p_ptr->spell.data[i].spell[0]].info[p_ptr->rp.pclass].slevel <
				    s_info[p_ptr->spell.realm[1]-1][p_ptr->spell.data[i].spell[1]].info[p_ptr->rp.pclass].slevel)
				{
					sp.r = p_ptr->spell.realm[0]-1;
					sp.s = p_ptr->spell.data[i].spell[0];
				}
				else
				{
					sp.r = p_ptr->spell.realm[1]-1;
					sp.s = p_ptr->spell.data[i].spell[1];
				}
			}
			else if (p_ptr->spell.data[i].flags & SP_PRESENT_1)
			{
				sp.r = p_ptr->spell.realm[0]-1;
				sp.s = p_ptr->spell.data[i].spell[0];
			}
			else
			{
				sp.r = p_ptr->spell.realm[1]-1;
				sp.s = p_ptr->spell.data[i].spell[1];
			}

			/* Level needed for this spell at this focus level */
			lev = s_info[sp.r][sp.s].info[p_ptr->rp.pclass].slevel +
					((p_ptr->spell.data[i].focus-1)*mp_ptr->focus_offset);

			/* Slot to use */
			slot = (lev - mp_ptr->spell_first)*SPELL_LAYERS/PY_MAX_LEVEL;

			/* Downgrade if needed */
			while (!p_ptr->spell_slots[slot])
			{
				slot++;
				if (slot >= SPELL_LAYERS)
				{
					msgf (CLR_RED "Warning: spell knowledge corrupted!");
					break;
				}
			}

			/* Use the slot */
			p_ptr->spell_slots[slot]--;
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
	u16b tmp16u, tmp16u2;
	u32b tmp32u;

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	u16b max_towns_load;
	u16b max_quests_load;

	/* Mention the savefile version */
	note("Loading a %d.%d.%d savefile...", z_major, z_minor, z_patch);

	/* Debug */
	/* arg_fiddle = TRUE; */

	/* Strip the version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;

	/* Setup the "verbal" checksum */
	checksum_base = 0;
	checksum = checksum_base;

	/* Check */
	rd_checksum("Start information");

	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

	/* Read the version number of the savefile */
	rd_u32b(&sf_version);

	/* Check */
	rd_checksum("Header information");

	/* Operating system info */
	rd_u32b(&sf_xtra);

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);

	/* Read RNG state */
	rd_randomizer();
	if (arg_fiddle) note("Loaded Randomizer Info");

	/* Then the options */
	rd_options();
	if (arg_fiddle) note("Loaded Option Flags");

	/*
	 * Munchkin players are marked
	 *
	 * XXX - should be replaced with a better method,
	 * after the new scorefile-handling is implemented.
	 */
	if (munchkin_death)
	{
		/* Mark savefile */
		p_ptr->state.noscore |= 0x0001;
	}

	/* Then the "messages" */
	rd_messages();

	if (arg_fiddle) note("Loaded Messages");

	/* Monster Memory */
	rd_u16b(&tmp16u2);

	/* Incompatible save files */
	if (tmp16u2 > z_info->r_max)
	{
		note("Too many (%u) monster races!", tmp16u2);
		return (21);
	}

	if (sf_version >= 58)
	{
		rd_u16b(&tmp16u);

		if (tmp16u > z_info->h_max || (HERO_MIN < z_info->r_max - tmp16u))
		{
			note("Too many (%u) heroes!", tmp16u);
			return (21);
		}
	}

	/* Read the available records */
	for (i = 0; i < tmp16u2; i++)
	{
		/* Adjust for changes in h_max across versions */
		if (sf_version >= 58 && i == z_info->r_max - tmp16u)
		{
			/* Treat first written hero as first hero */
			i = HERO_MIN;
		}

		/* Read the lore */
		rd_lore(i);
	}

	/* Initialize heroes for old savefiles */
	if (sf_version < 58)
	{
		for (i = 0; i < z_info->h_max; i++)
		{
			h_list[i].r_idx = 0;
			h_list[i].offset = 0;
			h_list[i].flags = 0;
			h_list[i].seed = 0;
		}
	}

	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->k_max)
	{
		note("Too many (%u) object kinds!  Ignoring extraneous info.", tmp16u);
	}

	/* Read the object memory */
	for (i = 0; i <= tmp16u; i++)
	{
		/* Fix a version-specific bug with savefiles */
		if (sf_version <= 58 && i == tmp16u)
			break;

	   	if (i > z_info->k_max)
		{
			rd_byte(&tmp8u);
			continue;
		}

		rd_byte(&k_info[i].info);
	}
	if (arg_fiddle) note("Loaded Object Memory");

	rd_checksum("Object memory");

	/* Number of towns */
	rd_u16b(&max_towns_load);

	/* Incompatible save files */
	if (max_towns_load > z_info->wp_max)
	{
		note("Too many (%u) towns!", max_towns_load);
		return (23);
	}

	/* Number of quests */
	rd_u16b(&max_quests_load);

	if (max_quests_load > z_info->q_max)
	{
		note("Too many (%u) quests!", max_quests_load);
		return (23);
	}

	rd_quests(max_quests_load);

	if (arg_fiddle) note("Loaded Quests");

	/* Position in the wilderness */
	rd_s32b(&p_ptr->wilderness_x);
	rd_s32b(&p_ptr->wilderness_y);

	/* Size of the wilderness */
	rd_s32b(&wild_x_size);
	rd_s32b(&wild_y_size);

	rd_checksum("Wilderness basic data");

	/* Incompatible save files */
	if ((wild_x_size > WILD_SIZE) || (wild_y_size > WILD_SIZE))
	{
		note("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size);
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

	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->a_max)
	{
		note("Too many (%u) artifacts!", tmp16u);
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

	rd_checksum("Artifact data");

	/* Read the extra stuff */
	rd_extra();
	if (arg_fiddle) note("Loaded extra information");

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u != PY_MAX_LEVEL)
	{
		note("Too %s (%u) hitpoint entries!", tmp16u > PY_MAX_LEVEL ? "many" : "few", tmp16u);
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->rp.psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->rp.prace];
	cp_ptr = &class_info[p_ptr->rp.pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->rp.pclass];

	/* Read spell info */
	if (sf_version <= 56)
	{
		strip_bytes(24);
		strip_bytes(PY_MAX_SPELLS_OLD);

	}
	else
	{
		rd_byte(&p_ptr->spell.spell_max);

		for (i = 0; i < PY_MAX_SPELLS; i++)
		{
			rd_byte(&p_ptr->spell.data[i].s_idx);
			rd_byte(&p_ptr->spell.data[i].realm);
			rd_byte(&p_ptr->spell.data[i].focus);
			rd_byte(&p_ptr->spell.data[i].flags);

			for (j = 0; j < NUM_SPELLS; j++)
			{
				/* Find the "external" spell numbers */
				if (s_info[p_ptr->spell.realm[0]-1][j].s_idx ==
					p_ptr->spell.data[i].s_idx &&
					s_info[p_ptr->spell.realm[0]-1][j].realm ==
					p_ptr->spell.data[i].realm)
						p_ptr->spell.data[i].spell[0] = j;
				if (s_info[p_ptr->spell.realm[1]-1][j].s_idx ==
					p_ptr->spell.data[i].s_idx &&
					s_info[p_ptr->spell.realm[1]-1][j].realm ==
					p_ptr->spell.data[i].realm)
						p_ptr->spell.data[i].spell[1] = j;
			}
		}
	}

	/* Convert spells that changed */
	convert_spells();

	/* Checksum */
	rd_checksum ("Hit point and spell info");

	/* Read rebirth info, if available */
	rd_rebirth();
	
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
			place[i].numstores = (byte) tmp16u;

			/* Allocate the stores */
			C_MAKE(place[i].store, place[i].numstores, store_type);

			for (j = 0; j < tmp16u; j++)
			{
				rd_store(i, j);
			}

			/* Assume we have a dungeon here */
			MAKE(place[i].dungeon, dun_type);
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
			
			if (sf_version >= 60)
			{
				rd_byte(&pl_ptr->seen);
			}

			if (sf_version < 42)
			{
				/* Assume we have a dungeon here */
				MAKE(place[i].dungeon, dun_type);
			}
			else
			{
				byte dungeon;

				rd_byte(&dungeon);

				if (dungeon)
				{
					dun_type *dun_ptr;

					/* Create a dungeon here */
					MAKE(place[i].dungeon, dun_type);

					dun_ptr = place[i].dungeon;

					/* Object theme */
					rd_byte(&dun_ptr->theme.treasure);
					rd_byte(&dun_ptr->theme.combat);
					rd_byte(&dun_ptr->theme.magic);
					rd_byte(&dun_ptr->theme.tools);

					/* Habitat */
					rd_u32b(&dun_ptr->habitat);

					/* Levels in dungeon */
					rd_byte(&dun_ptr->min_level);
					rd_byte(&dun_ptr->max_level);

					if (vanilla_town)
					{
						dun_ptr->min_level = 1;
						dun_ptr->max_level = MAX_DEPTH - 1;
					}


					/* Rating */
					rd_s16b(&dun_ptr->rating);


					/* Extra dungeon info */
					if (sf_version > 43)
					{
						rd_u16b(&dun_ptr->rooms);

						rd_byte(&dun_ptr->floor);
						rd_byte(&dun_ptr->wall);
						rd_byte(&dun_ptr->perm_wall);

						for (j = 0; j < 2; j++) {
							rd_byte(&dun_ptr->vein[j].deep);
							rd_byte(&dun_ptr->vein[j].size);
							rd_byte(&dun_ptr->vein[j].number);
						}

						for (j = 0; j < 2; j++) {
							rd_byte(&dun_ptr->river[j].shal);
							rd_byte(&dun_ptr->river[j].deep);
							rd_byte(&dun_ptr->river[j].rarity);
							rd_byte(&dun_ptr->river[j].size);
						}

						rd_byte(&dun_ptr->lake.shal);
						rd_byte(&dun_ptr->lake.deep);
						rd_byte(&dun_ptr->lake.rarity);
						rd_byte(&dun_ptr->lake.size);

						rd_byte(&dun_ptr->freq_monsters);
						rd_byte(&dun_ptr->freq_objects);
						rd_byte(&dun_ptr->freq_doors);
						rd_byte(&dun_ptr->freq_traps);
						rd_byte(&dun_ptr->freq_rubble);
						rd_byte(&dun_ptr->freq_treasure);
						rd_byte(&dun_ptr->freq_stairs);
						rd_byte(&dun_ptr->freq_arena);
						rd_byte(&dun_ptr->freq_cavern);
						rd_byte(&dun_ptr->freq_tunnel);

						rd_byte(&dun_ptr->room_limit);

						rd_u32b(&dun_ptr->flags);

						/* Recall depth */
						if (sf_version > 46)
						{
							rd_byte(&dun_ptr->recall_depth);
						}
						else
						{
							/* Hack - use old one-dungeon depth */
							dun_ptr->recall_depth = (byte) p_ptr->depth;

							/* Make sure the value is in bounds */
							if (dun_ptr->recall_depth < dun_ptr->min_level)
							{
								dun_ptr->recall_depth = dun_ptr->min_level;
							}

							if (dun_ptr->recall_depth > dun_ptr->max_level)
							{
								dun_ptr->recall_depth = dun_ptr->max_level;
							}
						}
					}
				}
			}

			/* Allocate the stores */
			C_MAKE(pl_ptr->store, pl_ptr->numstores, store_type);

			/* Get the stores of all towns */
			for (j = 0; j < pl_ptr->numstores; j++)
			{
				rd_store(i, j);
			}

			rd_checksum("Town data");
		}
	}


	/* Read the pet command settings */
	if (sf_version > 2)
	{
		rd_s16b(&p_ptr->pet_follow_distance);
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
	if (!p_ptr->state.is_dead)
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

		{
			s32b tmp32s;

			rd_s32b(&tmp32s);
			strip_bytes(tmp32s);
		}

		rd_checksum("Life information");
	}

	/* Repair "find artifact" quests that are out of target
	   dungeon range.  Must be done not in the normal place
	   because dungeon data isn't loaded at that point. */
	for (i = 0; i < q_max; i++)
	{
		quest_type * q_ptr = &quest[i];

		/* Find item quests: if out of range of target dungeon, mark
		   as finished. */
		if (q_ptr->type == QUEST_TYPE_FIND_ITEM)
		{
			if (place[q_ptr->data.fit.place].dungeon->max_level <
				q_ptr->level)
			{
				q_ptr->status = QUEST_STATUS_FINISHED;
			}
		}
	}


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
