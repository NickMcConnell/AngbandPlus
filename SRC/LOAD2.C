/* File: load2.c */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"


/*
 * This file loads savefiles from Angband 2.7.X and 2.8.X
 *
 * Ancient savefiles (pre-2.7.0) are loaded by another file.
 *
 * Note that Angband 2.7.0 through 2.7.3 are now officially obsolete,
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
static FILE     *fff;

/*
 * Hack -- old "encryption" byte
 */
static byte     xor_byte;

/*
 * Hack -- simple "checksum" on the actual values
 */
static u32b     v_check = 0L;

/*
 * Hack -- simple "checksum" on the encoded bytes
 */
static u32b     x_check = 0L;



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
		case TV_TWO_HANDED:
		case TV_DAGGER:
		case TV_AXE:
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
		case TV_FOCUS:
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

static void rd_u64b(u64b *ip)
{
   (*ip) = sf_get();
   (*ip) |= ((u64b)(sf_get()) << 8);
   (*ip) |= ((u64b)(sf_get()) << 16);
   (*ip) |= ((u64b)(sf_get()) << 24);
   (*ip) |= ((u64b)(sf_get()) << 32);
   (*ip) |= ((u64b)(sf_get()) << 40);
   (*ip) |= ((u64b)(sf_get()) << 48);
   (*ip) |= ((u64b)(sf_get()) << 56);
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
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;
	int j;

	u64b f1, f2, f3;

	object_kind *k_ptr;

	char buf[128];


	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	rd_byte(&o_ptr->discount);
	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);
	rd_s16b(&o_ptr->chp);
	rd_s16b(&o_ptr->mhp);

	rd_byte(&o_ptr->name1);
	rd_byte(&o_ptr->name2);
	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&old_dd);
	rd_byte(&old_ds);

        /* Read stat bonuses */
        for (j = 0; j < 7; j++)
        {
                rd_s16b(&o_ptr->to_stat[j]);
        }

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	/* Artifact flags */
	rd_u64b(&o_ptr->art_flags1);
	rd_u64b(&o_ptr->art_flags2);
	rd_u64b(&o_ptr->art_flags3);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	rd_string(buf, 128);
	if (buf[0]) o_ptr->art_name = quark_add(buf);

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;


	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr))
	{
		/* Acquire correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Acquire correct fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Paranoia */
		o_ptr->name1 = o_ptr->name2 = 0;

		/* All done */
		return;
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}


	/* Acquire standard fields */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Acquire standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

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

		/* Hack -- assume "curse" */
		if (older_than(2, 7, 9))
		{
			/* Hack -- assume cursed */
			if (a_ptr->flags3 & (TR3_CURSED)) o_ptr->ident |= (IDENT_CURSED);
		}
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
	}

	if (o_ptr->art_name) /* A random artifact */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}

	/* Hack -- keep some old fields.  Moved from ego-item area. -LM- */
	if (o_ptr->ds != old_ds)
	{
		/* Keep old enhanced damage sides. */
		o_ptr->ds = old_ds;
	}

	if (o_ptr->dd != old_dd) 
	{
		/* Keep old enhanced damage dice. */
		o_ptr->dd = old_dd;
	}

}




/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{

	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
	rd_u32b(&m_ptr->smart);
	rd_byte(&m_ptr->monpois);
}





/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;
        s16b tmp16s;

	monster_race *r_ptr = &r_info[r_idx];


	{
		/* Count sights/deaths/kills */
		rd_s16b(&r_ptr->r_sights);
		rd_s16b(&r_ptr->r_deaths);
		rd_s16b(&r_ptr->r_pkills);
		rd_s16b(&r_ptr->r_tkills);

		/* Count wakes and ignores */
		rd_byte(&r_ptr->r_wake);
		rd_byte(&r_ptr->r_ignore);

		/* Extra stuff XXX XXX XXX */
                rd_s16b (&tmp16s);
                if (tmp16s) r_ptr->flags2 |= RF2_EXTINCT;

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
		rd_u32b(&r_ptr->r_flags7);
		rd_u32b(&r_ptr->r_flags8);
		rd_u32b(&r_ptr->r_flags9);


		/* Read the "Racial" monster limit per level */
		rd_byte(&r_ptr->max_num);

		/* Later (?) */
		strip_bytes(2);
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

	byte own, num;

	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&own);
	rd_byte(&num);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

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

	/* Extract the owner (see above) */
	st_ptr->owner = own;

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

	u32b flag[8];
	u32b mask[8];


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

	if (c & 0x0002) wizard = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;
	cheat_invul = (c & 0x4000) ? TRUE : FALSE;
	debug_vaults = (c & 0x8000) ? TRUE : FALSE;

	/* Pre-2.8.0 savefiles are done */
	if (older_than(2, 8, 0)) return;

	if (z_older_than(2,1,0))
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
			/* Process valid flags */
			if (mask[n] & (1L << i))
			{
				/* Process valid flags */
				if (option_mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
					{
						/* Set */
						option_flag[n] |= (1L << i);
					}

					/* Clear */
					else
					{
						/* Clear */
						option_flag[n] &= ~(1L << i);
					}
				}
			}
		}
	}


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

	/* Older ghosts */
	if (older_than(2, 7, 7))
	{
		/* Strip old data */
		strip_bytes(52);
	}

	/* Newer ghosts */
	else
	{
		/* Strip old data */
		strip_bytes(60);
	}
}




/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i;

	byte tmp8u;
	s16b tmp16s;

	rd_string(player_name, 32);

	rd_string(died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&p_ptr->realm1);
	rd_byte(&p_ptr->realm2);
	rd_byte(&player_desc);        /* oops */

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < 7; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 7; i++) rd_s16b(&p_ptr->stat_cur[i]);
        for (i = 0; i < 7; ++i) rd_s16b(&p_ptr->stat_cnt[i]);
        for (i = 0; i < 7; ++i) rd_s16b(&p_ptr->stat_los[i]);

	strip_bytes(20);        /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	rd_s16b(&p_ptr->town_num);

	/* Read arena and rewards information */
	rd_s16b(&p_ptr->align_law);
	rd_s16b(&p_ptr->align_good);
	rd_s16b(&p_ptr->inside_quest);
	rd_byte(&p_ptr->exit_bldg);
	rd_byte(&p_ptr->leftbldg);

	rd_s16b(&p_ptr->oldpx);
	rd_s16b(&p_ptr->oldpy);

	rd_s16b(&tmp16s);

	if (tmp16s > MAX_BACT)
	{
		note(format("Too many (%d) building rewards!", tmp16s));
	}

	for (i = 0; i < tmp16s; i++) rd_s16b(&p_ptr->rewards[i]);
	

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_plv);
	rd_s16b(&p_ptr->max_dlv);

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;

	/* Repair maximum dungeon level */
	if (p_ptr->max_dlv < 0) p_ptr->max_dlv = 1;

	/* More info */
	rd_s16b(&p_ptr->msane);
	rd_s16b(&p_ptr->csane);
	rd_u16b(&p_ptr->csane_frac);

	strip_bytes(1);
	rd_byte(&town_night);
	rd_s16b(&p_ptr->sc);
	rd_byte(&p_ptr->sign);

	/* Read the flags */
	strip_bytes(3); /* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	rd_u32b(&p_ptr->mind);
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
	rd_s16b(&p_ptr->tim_esp);
	rd_s16b(&p_ptr->wraith_form);
	rd_s16b(&p_ptr->resist_magic);
	rd_s16b(&p_ptr->stoning);
	rd_s16b(&p_ptr->tim_res_conf);
	rd_s16b(&p_ptr->tim_res_blind);
	rd_s16b(&p_ptr->tim_invisible);
	rd_s16b(&p_ptr->tim_polymon);
	rd_s16b(&p_ptr->tim_xtra6);
	rd_s16b(&p_ptr->tim_xtra7);
	rd_s16b(&p_ptr->polymon);
	rd_s16b(&p_ptr->chaos_patron);
	rd_u32b(&p_ptr->muta1);
	rd_u32b(&p_ptr->muta2);
	rd_u32b(&p_ptr->muta3);

	/* Calc the regeneration modifier for mutations */
	mutant_regenerate_mod = calc_mutant_regenerate_mod();

	rd_byte(&p_ptr->confusing);
	strip_bytes(1);
	rd_byte(&p_ptr->tactic);
	rd_byte(&p_ptr->align);
	rd_byte(&p_ptr->searching);
	strip_bytes(2);
	rd_byte(&p_ptr->allow_one_death);

	/* Find out how many thefts have already occured on this level. -LM- */
	rd_byte(&number_of_thefts_on_level);

	/* Gods */

	rd_s32b(&p_ptr->grace);
	rd_byte(&p_ptr->champion);
	rd_s16b(&avg_rating);
	rd_byte(&vanilla_town);
	rd_byte(&p_ptr->pgod);

	/* Last ghost created */
	rd_byte(&bones_selector);

	rd_s16b(&p_ptr->precog);

	/* Future use */
	for (i = 0; i < 35; i++) rd_byte(&tmp8u);

	/* Scalar time */
	rd_u32b(&sc_time);

	rd_u16b(&p_ptr->birthday);

	/* Skip the flags */
	strip_bytes(2);


	/* Hack -- the three "special seeds" */
	rd_u32b(&seed_dungeon);
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);


	/* Special stuff */
	rd_u16b(&panic_save);
	rd_u16b(&total_winner);
	rd_u16b(&noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	death = tmp8u;

	/* Read "feeling" */
	rd_byte(&tmp8u);
	feeling = tmp8u;

	/* Turn of last "feeling" */
	rd_s32b(&old_turn);

	/* Current turn */
	rd_s32b(&turn);
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
	total_weight = 0;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

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
			total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (inven_cnt == INVEN_PACK)
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
			total_weight += (q_ptr->number * q_ptr->weight);

			/* One more item */
			inven_cnt++;
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

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);

		/* Save the message */
		message_add(buf);
	}
}



/*
 * Old "cave grid" flags -- saved in savefile
 */
#define OLD_GRID_W_01   0x0001  /* Wall type (bit 1) */
#define OLD_GRID_W_02   0x0002  /* Wall type (bit 2) */
#define OLD_GRID_PERM   0x0004  /* Wall type is permanent */
#define OLD_GRID_QQQQ   0x0008  /* Unused */
#define OLD_GRID_MARK   0x0010  /* Grid is memorized */
#define OLD_GRID_GLOW   0x0020  /* Grid is illuminated */
#define OLD_GRID_ROOM   0x0040  /* Grid is part of a room */
#define OLD_GRID_ICKY   0x0080  /* Grid is anti-teleport */

/*
 * Masks for the new grid types
 */
#define OLD_GRID_WALL_MASK      0x0003  /* Wall type */

/*
 * Legal results of OLD_GRID_WALL_MASK
 */
#define OLD_GRID_WALL_NONE              0x0000  /* No wall */
#define OLD_GRID_WALL_MAGMA             0x0001  /* Magma vein */
#define OLD_GRID_WALL_QUARTZ    0x0002  /* Quartz vein */
#define OLD_GRID_WALL_GRANITE   0x0003  /* Granite wall */


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 */
static errr rd_dungeon(void)
{
	int i, y, x;

	int ymax, xmax;

	byte count;
	byte tmp8u;
	s16b tmp16s;

	u16b limit;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&dun_level);

	/* Set the base level for old versions */
	base_level = dun_level;

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

	if (!dun_level && !p_ptr->inside_quest)
	{
		/* Init the wilderness */
		process_dungeon_file("w_info.txt", 0, 0, max_wild_y, max_wild_x);

		/* Init the town */
		init_flags = INIT_ONLY_BUILDINGS;
		process_dungeon_file("t_info.txt", 0, 0, MAX_HGT, MAX_WID);
	}

	/* Maximal size */
	ymax = cur_hgt;
	xmax = cur_wid;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_s16b(&tmp16s);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "info" */
			c_ptr->info = tmp16s;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}



	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->feat = tmp8u;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->mimic = tmp8u;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_s16b(&tmp16s);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
			c_ptr->special = tmp16s;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < ymax; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_s16b(&tmp16s);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "feat" */
                        c_ptr->t_idx = tmp16s;

			/* Advance/Wrap */
			if (++x >= xmax)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= ymax) break;
			}
		}
	}

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= max_o_idx)
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
		else
		{
			/* Access the item location */
			c_ptr = &cave[o_ptr->iy][o_ptr->ix];

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
	if (limit >= max_m_idx)
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


		/* Access grid */
		c_ptr = &cave[m_ptr->fy][m_ptr->fx];

		/* Mark the location */
		c_ptr->m_idx = m_idx;


		/* Access race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* If a player ghost, some special features need to be added. -LM- */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			prepare_ghost(m_ptr->r_idx, m_ptr, TRUE);
		}


		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
	}

	/*** Success ***/

	/* The dungeon is ready */
	if (z_older_than(2, 1, 3))
		character_dungeon = FALSE;
	else
		character_dungeon = TRUE;

	/* Success */
	return (0);
}



/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i, j;
	int town_count;

	s32b wild_x_size;
	s32b wild_y_size;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
		sf_major, sf_minor, sf_patch));


	/* Hack -- Warn about "obsolete" versions */
	if (older_than(2, 7, 4))
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
		monster_race *r_ptr;

		/* Read the lore */
		rd_lore(i);

		/* Access that monster */
		r_ptr = &r_info[i];

		/* XXX XXX Hack -- repair old savefiles */
		if (older_than(2, 7, 6))
		{
			/* Assume no kills */
			r_ptr->r_pkills = 0;

			/* Hack -- no previous lives */
			if (sf_lives == 0)
			{
				/* All kills by this life */
				r_ptr->r_pkills = r_ptr->r_tkills;
			}

			/* Hack -- handle uniques */
			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				/* Assume no kills */
				r_ptr->r_pkills = 0;

				/* Handle dead uniques */
				if (r_ptr->max_num == 0) r_ptr->r_pkills = 1;
			}
		}
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

#if 0
	/*
	 * Initialize arena and rewards information
	 */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->inside_quest = 0;
	p_ptr->leftbldg = FALSE;
	p_ptr->exit_bldg = TRUE;

	/* Start in town 1 */
	p_ptr->town_num = 1;

	p_ptr->wilderness_x = 4;
	p_ptr->wilderness_y = 4;

#endif

	/* Init the wilderness seeds */
	for (i = 0; i < max_wild_x; i++)
	{
		for (j = 0; j < max_wild_y; j++)
		{
			wilderness[j][i].seed = rand_int(0x10000000);
		}
	}

	/* 2.1.3 or newer version */
	if (!z_older_than(2, 1, 3))
	{
		u16b max_towns_load;
		u16b max_quests_load;

		/* Number of towns */
		rd_u16b(&max_towns_load);

		/* Incompatible save files */
		if (max_towns_load > max_towns)
		{
			note(format("Too many (%u) towns!", max_towns_load));
			return (23);
		}

		/* Number of quests */
		rd_u16b(&max_quests_load);


		/* Incompatible save files */
		if (max_quests_load > max_quests)
		{
			note(format("Too many (%u) quests!", max_quests_load));
			return (23);
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
				if (quest[i].status == 1)
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
					rd_s16b(&quest[i].k_idx);
					rd_byte(&quest[i].flags);

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

		/* Position in the wilderness */
		rd_s32b(&p_ptr->wilderness_x);
		rd_s32b(&p_ptr->wilderness_y);

		/* Size of the wilderness */
		rd_s32b(&wild_x_size);
		rd_s32b(&wild_y_size);

		/* Incompatible save files */
		if ((wild_x_size > max_wild_x) || (wild_y_size > max_wild_y))
		{
			note(format("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size));
			return (23);
		}

		/* Load the wilderness seeds */
		for (i = 0; i < wild_x_size; i++)
		{
			for (j = 0; j < wild_y_size; j++)
			{
				rd_u32b(&wilderness[j][i].seed);
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
				if ((v <= MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1) && ( v >= 0 )) break;
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

			monster_race *r_ptr;

			q_ptr->status = QUEST_STATUS_TAKEN;

			for (j = 0; j < MAX_TRIES; j++)
			{
				/* Random monster 5 - 10 levels out of depth */
				q_ptr->r_idx = get_mon_num(q_ptr->level + 4 + randint(6));

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
				q_ptr->max_num = 5 + (s16b)rand_int(q_ptr->level/3 + 5);
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
				if (randint(2) == 1)
					c = 'n';
				break;
			}
			if (c == ESCAPE) break;
			if ((c == 'y') || (c == 'n')) break;
			if (c == '?') do_cmd_help();
			else bell();
		}

		/* Set "maximize" mode */
		p_ptr->hard_quests = (c == 'y');

		/* Clear */
		clear_from(15);
	}

	if (arg_fiddle) note("Loaded Quests");


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

        /* Load the Traps */
        rd_u16b(&tmp16u);

        /* Incompatible save files */
        if (tmp16u > max_t_idx)
        {
                note(format("Too many (%u) traps!", tmp16u));
                return (24);
        }
        if (arg_fiddle) note("Loaded Traps");

        /* Read the fate flags */
        for (i = 0; i < tmp16u; i++)
        {
                rd_byte(&t_info[i].ident);
        }
        if (arg_fiddle) note("Loaded Traps");

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
		rd_s16b(&player_hp[i]);
	}


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->pclass];


	/* Read spell info */
	rd_u32b(&spell_learned1);
	rd_u32b(&spell_learned2);
	rd_u32b(&spell_worked1);
	rd_u32b(&spell_worked2);
	rd_u32b(&spell_forgotten1);
	rd_u32b(&spell_forgotten2);

	for (i = 0; i < 64; i++)
	{
		rd_byte(&spell_order[i]);
	}


	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Read number of towns */
	if (!z_older_than(2,1,3))
	{
		rd_u16b(&tmp16u);
		town_count = tmp16u;
	}
	else
	{
		/* Only one town */
		town_count = 2;
	}

	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 1; i < town_count; i++)
	{
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

#if 0
	if (z_older_than(2,1,0))
	{
		msg_print("Reallocating flavours...");
		flavor_init();
	}
#endif

	rd_s16b(&p_ptr->pet_follow_distance);
	rd_byte(&p_ptr->pet_open_doors);
	rd_byte(&p_ptr->pet_pickup_items);

	/* I'm not dead yet... */
	if (!death)
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


	/* Hack -- no ghosts */
	r_info[max_r_idx-1].max_num = 0;


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


