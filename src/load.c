/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */
#include "angband.h"


/*
 * This file loads savefiles from Angband 2.9.X.
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
 * by a monster will reduce the shield by that amount.  XXX XXX XXX
 */





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
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
bool older_than(int x, int y, int z)
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
                case TV_INSTRUMENT:
		case TV_STAFF:
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
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info.
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
 */
static errr rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	u32b f1, f2, f3;

	object_kind *k_ptr;

	char buf[128];


	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Paranoia */
	if ((o_ptr->k_idx < 0) || (o_ptr->k_idx >= z_info->k_max))
		return (-1);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	/* Special stack counter */
	if ((variant_pval_stacks)||(variant_time_stacks)) rd_byte(&o_ptr->stackc);

	rd_byte(&o_ptr->discount);

	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);

	rd_byte(&o_ptr->name1);
	rd_byte(&o_ptr->name2);

	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&old_dd);
	rd_byte(&old_ds);

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	/* Hack -- fix rod/ring/dragon armor pval so that timeout set correctly in future */
	if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_DRAG_ARMOR) || ((o_ptr->tval == TV_RING) &&
		((o_ptr->sval == SV_RING_FLAMES) || (o_ptr->sval == SV_RING_ACID) || (o_ptr->sval == SV_RING_ICE) ||
			(o_ptr->sval == SV_RING_LIGHTNING))))
	{
		o_ptr->pval = k_info[o_ptr->k_idx].pval;
	}

	/* Hack -- remove chests */
	if (o_ptr->tval == TV_CHEST) o_ptr->k_idx = 0;

	/* Old flags */
	strip_bytes(12);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Flags we have learnt about an item */
	if (variant_learn_id)
	{
		/* Knowledge */
		rd_u32b(&o_ptr->can_flags1);
		rd_u32b(&o_ptr->can_flags2);
		rd_u32b(&o_ptr->can_flags3);

		rd_u32b(&o_ptr->may_flags1);
		rd_u32b(&o_ptr->may_flags2);
		rd_u32b(&o_ptr->may_flags3);

		rd_u32b(&o_ptr->not_flags1);
		rd_u32b(&o_ptr->not_flags2);
		rd_u32b(&o_ptr->not_flags3);
	}

	/* Times we have used an item */
	if (variant_usage_id) rd_s16b(&o_ptr->usage);

	/* Guessed an item as */
	if (variant_guess_id)
	{
		rd_byte(&o_ptr->guess1);
		rd_byte(&o_ptr->guess2);
	}

	/* Item has a monster 'flavor' */
	if (variant_drop_body)
	{
		rd_s16b(&o_ptr->name3);
	}

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

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
		/* Get the correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_d = k_ptr->to_d;
		o_ptr->to_a = k_ptr->to_a;

		/* Get the correct fields */
		o_ptr->ac = k_ptr->ac;
		o_ptr->dd = k_ptr->dd;
		o_ptr->ds = k_ptr->ds;

		/* Get the correct weight */
		o_ptr->weight = k_ptr->weight;

		/* Paranoia */
		o_ptr->name1 = o_ptr->name2 = 0;

		/* All done */
		return (0);
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Paranoia */
		if (o_ptr->name1 >= 256) return (-1);

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Paranoia */
		if (o_ptr->name2 >= z_info->e_max) return (-1);

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}


	/* Get the standard fields */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Get the standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Get the new artifact "pval" */
		o_ptr->pval = a_ptr->pval;

		/* Get the new artifact fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;

		/* Get the new artifact weight */
		o_ptr->weight = a_ptr->weight;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Hack -- keep some old fields */
		if ((o_ptr->dd < old_dd) && (o_ptr->ds == old_ds))
		{
			/* Keep old boosted damage dice */
			o_ptr->dd = old_dd;
		}

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- enforce legal pval */
		if (e_ptr->flags1 & (TR1_PVAL_MASK))
		{
			/* Force a meaningful pval */
			if (!o_ptr->pval) o_ptr->pval = 1;
		}

		/* Mega-Hack - Enforce the special broken items */
		if ((o_ptr->name2 == EGO_BLASTED) ||
			(o_ptr->name2 == EGO_SHATTERED))
		{
			/* These were set to k_info values by preceding code */
			o_ptr->ac = 0;
			o_ptr->dd = 0;
			o_ptr->ds = 0;
		}
	}

	/* Success */
	return (0);
}




/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	byte tmp8u;
	s16b tmp16s;

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

	/* Summoned monsters timeout */
	if (variant_unsummon)
	{
		if (older_than(3,0,1))
		{
			rd_s16b(&tmp16s);
			m_ptr->summoned = (byte)tmp16s;
			rd_byte(&tmp8u);
			m_ptr->mflag = tmp8u;
		}
		else
		{
			rd_byte(&m_ptr->summoned);
			rd_u16b(&m_ptr->mflag);
		}

		if (!(older_than(2,9,6)))
		{
			rd_byte(&m_ptr->min_range);
			rd_byte(&m_ptr->best_range);
			rd_byte(&m_ptr->ty);
			rd_byte(&m_ptr->tx);
		}
	}

	/* Saved mflags - should really be another option XXX */
	if ((variant_drop_body) && (older_than(2,9,6)))
	{
		rd_byte(&tmp8u);
		m_ptr->mflag = tmp8u;
	}

	rd_byte(&tmp8u);
}





/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];


	/* Count sights/deaths/kills */
	rd_s16b(&l_ptr->sights);
	rd_s16b(&l_ptr->deaths);
	rd_s16b(&l_ptr->pkills);
	rd_s16b(&l_ptr->tkills);

	/* Count wakes and ignores */
	rd_byte(&l_ptr->wake);
	rd_byte(&l_ptr->ignore);

	/* Extra stuff */
	rd_byte(&l_ptr->xtra1);
	rd_byte(&l_ptr->xtra2);

	/* Count drops */
	rd_byte(&l_ptr->drop_gold);
	rd_byte(&l_ptr->drop_item);

	/* Count spells */
	rd_byte(&l_ptr->cast_innate);
	rd_byte(&l_ptr->cast_spell);

	/* Count blows of each type */
	rd_byte(&l_ptr->blows[0]);
	rd_byte(&l_ptr->blows[1]);
	rd_byte(&l_ptr->blows[2]);
	rd_byte(&l_ptr->blows[3]);

	/* Memorize flags */
	rd_u32b(&l_ptr->flags1);
	rd_u32b(&l_ptr->flags2);
	rd_u32b(&l_ptr->flags3);
	rd_u32b(&l_ptr->flags4);
	rd_u32b(&l_ptr->flags5);
	rd_u32b(&l_ptr->flags6);
	if (variant_drop_body && !(older_than(2,9,6))) rd_u32b(&l_ptr->flags7);

	/* Read the "Racial" monster limit per level */
	rd_byte(&r_ptr->max_num);

	/* Later (?) */
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);


	/* Repair the lore flags */
	l_ptr->flags1 &= r_ptr->flags1;
	l_ptr->flags2 &= r_ptr->flags2;
	l_ptr->flags3 &= r_ptr->flags3;
	l_ptr->flags4 &= r_ptr->flags4;
	l_ptr->flags5 &= r_ptr->flags5;
	l_ptr->flags6 &= r_ptr->flags6;
}




/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	byte own, num;


	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&own);
	rd_byte(&num);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

	/* Paranoia */
	if (own >= z_info->b_max)
	{
		note("Illegal store owner!");
		return (-1);
	}

	st_ptr->owner = own;

	/* Read the items */
	for (j = 0; j < num; j++)
	{
		object_type *i_ptr;
		object_type object_type_body;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		if (rd_item(i_ptr))
		{
			note("Error reading item");
			return (-1);
		}

		/* Accept any valid items */
		if (st_ptr->stock_num < STORE_INVEN_MAX)
		{
			int k = st_ptr->stock_num++;

			/* Accept the item */
			object_copy(&st_ptr->stock[k], i_ptr);
		}
	}

	/* Success */
	return (0);
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
 * Read options
 *
 * Note that the normal options are stored as a set of 256 bit flags,
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

	u16b tmp16u;

	u32b flag[8];
	u32b mask[8];
	u32b window_flag[ANGBAND_TERM_MAX];
	u32b window_mask[ANGBAND_TERM_MAX];

        /* Hack -- unset all Save File Options for compatibility */
	for (i = 0; i < OPT_PAGE_PER; i++)
	{
		/* Collect options on this "page" */
		if (option_page[8][i] != 255)
		{
			op_ptr->opt[option_page[8][i]] = FALSE;
		}
	}

	/*** Oops ***/

	/* Ignore old options */
	strip_bytes(16);


	/*** Special info */

	/* Read "delay_factor" */
	rd_byte(&b);
	op_ptr->delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	op_ptr->hitpoint_warn = b;

	/* Old cheating options */
	rd_u16b(&tmp16u);


	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

	/* Read the option masks */
	for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_MAX; i++)
	{
		int os = i / 32;
		int ob = i % 32;

		/* Process real entries */
		if (option_text[i])
		{
			/* Process saved entries */
			if (mask[os] & (1L << ob))
			{
				/* Set flag */
				if (flag[os] & (1L << ob))
				{
					/* Set */
					op_ptr->opt[i] = TRUE;
				}

				/* Clear flag */
				else
				{
					/* Set */
					op_ptr->opt[i] = FALSE;
				}
			}
		}
	}


	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < ANGBAND_TERM_MAX; n++)
	{
		rd_u32b(&window_flag[n]);
	}

	/* Read the window masks */
	for (n = 0; n < ANGBAND_TERM_MAX; n++)
	{
		rd_u32b(&window_mask[n]);
	}

	/* Analyze the options */
	for (n = 0; n < ANGBAND_TERM_MAX; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 32; i++)
		{
			/* Process valid flags */
			if (window_flag_desc[i])
			{
				/* Process valid flags */
				if (window_mask[n] & (1L << i))
				{
					/* Set */
					if (window_flag[n] & (1L << i))
					{
						/* Set */
						op_ptr->window_flag[n] |= (1L << i);
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

	/* Strip old data */
	strip_bytes(60);
}


static u32b randart_version;


/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;

	int max_spells = PY_MAX_SPELLS;

	if (!variant_study_more) max_spells = 64;

	rd_string(op_ptr->full_name, 32);

	rd_string(p_ptr->died_from, 80);

	if (older_than(3, 0, 1))
	{
		char *hist = p_ptr->history;

		for (i = 0; i < 4; i++)
		{
			/* Read a part of the history */
			rd_string(hist, 60);

			/* Advance */
			hist += strlen(hist);

			/* Separate by spaces */
			hist[0] = ' ';
			hist++;
		}

		/* Make sure it is terminated */
		hist[0] = '\0';
	}
	else
	{
		rd_string(p_ptr->history, 250);
	}

	/* Player race */
	rd_byte(&p_ptr->prace);

	/* Verify player race */
	if (p_ptr->prace >= z_info->p_max)
	{
		note(format("Invalid player race (%d).", p_ptr->prace));
		return (-1);
	}

	/* Player class */
	rd_byte(&p_ptr->pclass);

	/* Verify player class */
	if (p_ptr->pclass >= z_info->c_max)
	{
		note(format("Invalid player class (%d).", p_ptr->pclass));
		return (-1);
	}

	/* Player gender */
	rd_byte(&p_ptr->psex);

	/* Hack -- record style in stripped bytes */
	rd_byte(&tmp8u); /* Oops */

	/* Ignore old redundant info */
	p_ptr->pstyle=tmp8u;

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_byte(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_cur[i]);

	strip_bytes(24);	/* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	/* Verify player level */
	if ((p_ptr->lev < 1) || (p_ptr->lev > PY_MAX_LEVEL))
	{
		note(format("Invalid player level (%d).", p_ptr->lev));
		return (-1);
	}

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_lev);
	rd_s16b(&p_ptr->max_depth);

	/* Hack -- Repair maximum player level */
	if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

	/* Hack -- Repair maximum dungeon level */
	if (p_ptr->max_depth < 0) p_ptr->max_depth = 1;

	/* More info */
	/* Hack --- Get psval information. */
	rd_byte(&tmp8u); /* Oops */

	/* Ignore old redundant info */
	p_ptr->psval=tmp8u;

	/* Hack -- set styles for vanilla characters */
	/* Mega Hack -- use psval == 1 to indicate we have applied this hack  */
	if ((!p_ptr->pstyle) && (!p_ptr->psval))
	{
		switch(p_ptr->pclass)
		{
			case 2: /* CLASS_PRIEST */
				p_ptr->pstyle = WS_HAFTED;
				break;
			case 4: /* CLASS_RANGER */
				p_ptr->pstyle = WS_BOW;
				break;
			default:
				p_ptr->psval = 1;
			break;
		}
	}

	/* Hack --- Get held_song information. */
	rd_byte(&tmp8u); /* Oops */

	/* Ignore old redundant info */
	p_ptr->held_song=tmp8u;

	strip_bytes(6); /* Was strip bytes(8) */
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);	/* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);

	/* Read in 'rest' */
	if (variant_fast_moves) rd_s16b(&p_ptr->rest);
	else
	{
		strip_bytes(2);
		p_ptr->rest = PY_REST_FULL;
	}
	strip_bytes(2); /* Old "food_digested" / "protection" */

	/* Read more flags */
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

	rd_byte(&tmp8u);  /* Was p_ptr->confusing */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&p_ptr->searching);
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */

	/* Future use */
	strip_bytes(40);

	/* Read the randart version */
	rd_u32b(&randart_version);

	/* Read the randart seed */
	rd_u32b(&seed_randart);

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


	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > PY_MAX_LEVEL)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (-1);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


	/* Read spell info */
	rd_u32b(&p_ptr->spell_learned1);
	rd_u32b(&p_ptr->spell_learned2);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_learned3);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_learned4);
	rd_u32b(&p_ptr->spell_worked1);
	rd_u32b(&p_ptr->spell_worked2);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_worked3);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_worked4);
	rd_u32b(&p_ptr->spell_forgotten1);
	rd_u32b(&p_ptr->spell_forgotten2);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_forgotten3);
	if (variant_more_spells) rd_u32b(&p_ptr->spell_forgotten4);

	/* Read in the spells */
	for (i = 0; i < max_spells; i++)
	{
		if (variant_more_spells) rd_s16b(&p_ptr->spell_order[i]);
		else
		{
			rd_byte(&tmp8u);

			p_ptr->spell_order[i] = tmp8u;
		}
	} 

	return (0);
}


/*
 * Read the random artifacts
 */
static errr rd_randarts(void)
{

#ifdef GJW_RANDART

	int i;
	byte tmp8u;
	s16b tmp16s;
	u16b tmp16u;
	u16b artifact_count;
	s32b tmp32s;
	u32b tmp32u;

	if (older_than(2, 9, 3))
	{
		/*
		 * XXX XXX XXX
		 * Importing old savefiles with random artifacts is dangerous
		 * since the randart-generators differ and produce different
		 * artifacts from the same random seed.
		 *
		 * Switching off the check for incompatible randart versions
		 * allows to import such a savefile - do it at your own risk.
		 */

		/* Check for incompatible randart version */
		if (randart_version != RANDART_VERSION)
		{
			note(format("Incompatible random artifacts version!"));
			return (-1);
		}

		/* Alive or cheating death */
		if (!p_ptr->is_dead || arg_wizard)
		{
			/* Initialize randarts */
			do_randart(seed_randart, TRUE);
		}	
	}
	else
	{
		/* Read the number of artifacts */
		rd_u16b(&artifact_count);

		/* Alive or cheating death */
		if (!p_ptr->is_dead || arg_wizard)
		{
			/* Initialize randarts */
			do_randart(seed_randart, TRUE);

			/* Incompatible save files */
			if (artifact_count > z_info->a_max)
			{
				note(format("Too many (%u) random artifacts!", artifact_count));
				return (-1);
			}

			/* Read the artifacts */
			for (i = 0; i < artifact_count; i++)
			{
				artifact_type *a_ptr = &a_info[i];

				rd_byte(&a_ptr->tval);
				rd_byte(&a_ptr->sval);
				rd_s16b(&a_ptr->pval);

				rd_s16b(&a_ptr->to_h);
				rd_s16b(&a_ptr->to_d);
				rd_s16b(&a_ptr->to_a);
				rd_s16b(&a_ptr->ac);

				rd_byte(&a_ptr->dd);
				rd_byte(&a_ptr->ds);

				rd_s16b(&a_ptr->weight);

				rd_s32b(&a_ptr->cost);

				rd_u32b(&a_ptr->flags1);
				rd_u32b(&a_ptr->flags2);
				rd_u32b(&a_ptr->flags3);

				rd_byte(&a_ptr->level);
				rd_byte(&a_ptr->rarity);

				/* Hack -- Unangband requires 2 bytes to store activation */
				if (z_info->a_max == 256) rd_s16b(&a_ptr->activation);
				else rd_byte(&tmp8u);

				rd_u16b(&a_ptr->time);
				rd_u16b(&a_ptr->randtime);
			}
		}
		else
		{
			/* Read the artifacts */
			for (i = 0; i < artifact_count; i++)
			{
				rd_byte(&tmp8u); /* a_ptr->tval */
				rd_byte(&tmp8u); /* a_ptr->sval */
				rd_s16b(&tmp16s); /* a_ptr->pval */

				rd_s16b(&tmp16s); /* a_ptr->to_h */
				rd_s16b(&tmp16s); /* a_ptr->to_d */
				rd_s16b(&tmp16s); /* a_ptr->to_a */
				rd_s16b(&tmp16s); /* a_ptr->ac */

				rd_byte(&tmp8u); /* a_ptr->dd */
				rd_byte(&tmp8u); /* a_ptr->ds */

				rd_s16b(&tmp16s); /* a_ptr->weight */

				rd_s32b(&tmp32s); /* a_ptr->cost */

				rd_u32b(&tmp32u); /* a_ptr->flags1 */
				rd_u32b(&tmp32u); /* a_ptr->flags2 */
				rd_u32b(&tmp32u); /* a_ptr->flags3 */

				rd_byte(&tmp8u); /* a_ptr->level */
				rd_byte(&tmp8u); /* a_ptr->rarity */

				/* Hack -- Unangband requires 2 bytes to store activation */
				if (artifact_count == 256) rd_u16b(&tmp16u);
				else rd_byte(&tmp8u);

				rd_u16b(&tmp16u); /* a_ptr->time */
				rd_u16b(&tmp16u); /* a_ptr->randtime */
			}
		}

	}

	return (0);

#else /* GJW_RANDART */

	note("Random artifacts are disabled in this binary.");
	return (-1);

#endif /* GJW_RANDART */

}




/*
 * Read the player inventory
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(void)
{
	int slot = 0;

	object_type *i_ptr;
	object_type object_type_body;

	int inven_total = INVEN_TOTAL;

	if (variant_belt_slot) inven_total++;

	/* Read until done */
	while (1)
	{
		u16b n;

		/* Get the next item index */
		rd_u16b(&n);

		/* Nope, we reached the end */
		if (n == 0xFFFF) break;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		if (rd_item(i_ptr))
		{
			note("Error reading item");
			return (-1);
		}

		/* Hack -- verify item */
		if (!i_ptr->k_idx) return (-1);

		/* Verify slot */
		if (n >= inven_total) return (-1);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* Add the weight */
			p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

			/* One more item */
			p_ptr->equip_cnt++;
		}

		/* Warning -- backpack is full */
		else if (p_ptr->inven_cnt == INVEN_PACK)
		{
			/* Oops */
			note("Too many items in the inventory!");

			/* Fail */
			return (-1);
		}

		/* Carry inventory */
		else
		{
			/* Get a slot */
			n = slot++;

			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* Add the weight */
			p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

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
	u16b tmp16u;

	s16b num;

	/* Total */
	rd_s16b(&num);

	/* Read the messages */
	for (i = 0; i < num; i++)
	{
		/* Read the message */
		rd_string(buf, 128);

		/* Read the message type */
		if (!older_than(2, 9, 1))
			rd_u16b(&tmp16u);
		else
			tmp16u = MSG_GENERIC;

		/* Save the message */
		message_add(buf, tmp16u);
	}
}


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 *
 * Note that dungeon objects, including objects held by monsters, are
 * placed directly into the dungeon, using "object_copy()", which will
 * copy "iy", "ix", and "held_m_idx", leaving "next_o_idx" blank for
 * objects held by monsters, since it is not saved in the savefile.
 *
 * After loading the monsters, the objects being held by monsters are
 * linked directly into those monsters.
 */
static errr rd_dungeon(void)
{
	int i, y, x;
int by, bx;

	s16b town;
	s16b dungeon;
	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	byte count;
	byte tmp8u;
	u16b tmp16u;

u16b limit;

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_s16b(&dungeon);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&ymax);
	rd_s16b(&xmax);
	rd_s16b(&town);
	rd_u16b(&tmp16u);


	/* Ignore illegal dungeons */
	if (town>=z_info->t_max)
	{
		note(format("Ignoring illegal dungeon (%d)", dungeon));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth > max_depth(dungeon)))
	{
		note(format("Ignoring illegal dungeon depth (%d)", depth));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((ymax != DUNGEON_HGT) || (xmax != DUNGEON_WID))
	{
		/* XXX XXX XXX */
		note(format("Ignoring illegal dungeon size (%d,%d).", ymax, xmax));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) ||
	    (py < 0) || (py >= DUNGEON_HGT))
	{
		note(format("Ignoring illegal player location (%d,%d).", py, px));
		return (1);
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Extract "info" */
			cave_info[y][x] = tmp8u;

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}

	/* Hack -- not fully dynamic */
	dyna_full = FALSE;

	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < DUNGEON_HGT; )
	{
		/* Grab RLE info */
		rd_byte(&count);

		if (variant_save_feats) rd_u16b(&tmp16u);
		else rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Save the feat */
			if (variant_save_feats) cave_feat[y][x] = tmp16u;
			else cave_feat[y][x] = tmp8u;

			/* Check for los flag set*/
			if (f_info[cave_feat[y][x]].flags1 & (FF1_LOS))
			{
				cave_info[y][x] &= ~(CAVE_WALL);
			}

			/* Handle wall grids */
			else
			{
				cave_info[y][x] |= (CAVE_WALL);
			}

			/* Handle dynamic grids */
			if (f_info[cave_feat[y][x]].flags3 & (FF3_DYNAMIC_MASK))
			{
				if (dyna_n < (DYNA_MAX-1))
				{
					dyna_g[dyna_n++] = GRID(y,x);
				}
				else
				{
					dyna_full = TRUE;
					dyna_cent_y = 255;
					dyna_cent_x = 255;
				}
			}

			/* Advance/Wrap */
			if (++x >= DUNGEON_WID)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= DUNGEON_HGT) break;
			}
		}
	}

	/*** Player ***/

	/* Fix depth */
	if (depth < 1) depth = min_depth(dungeon);

	/* Fix depth */
	if (depth > max_depth(dungeon)) depth = max_depth(dungeon);

	/* Load depth */
	p_ptr->depth = depth;
	p_ptr->dungeon = dungeon;
	p_ptr->town = town;


	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (-1);
	}


	/*** Room descriptions ***/
   
	if (!variant_room_info)
	{
		/* Initialize the room table */
		for (by = 0; by < MAX_ROOMS_ROW; by++)
		{
			for (bx = 0; bx < MAX_ROOMS_COL; bx++)
			{
				dun_room[by][bx] = 0;
			}
		}

		/* Initialise 'zeroeth' room description */
		room_info[0].flags = 0;
	}
	else
	{

		for (bx = 0; bx < MAX_ROOMS_ROW; bx++)
		{
			for (by = 0; by < MAX_ROOMS_COL; by++)
			{
				rd_byte(&dun_room[bx][by]);
			}
		}

		for (i = 1; i < DUN_ROOMS; i++)
		{
			int j;

			rd_byte(&room_info[i].type);
			rd_byte(&room_info[i].flags);

			if (room_info[i].type == ROOM_NORMAL)
			{
				for (j = 0; j < ROOM_DESC_SECTIONS; j++)
				{
					rd_s16b(&room_info[i].section[j]);

					if (room_info[i].section[j] == -1) break;
				}
			}
		}
	}

	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > z_info->o_max)
	{
		note(format("Too many (%d) object entries!", limit));
		return (-1);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type *i_ptr;
		object_type object_type_body;

		s16b o_idx;
		object_type *o_ptr;


		/* Get the object */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		if (rd_item(i_ptr))
		{
			note("Error reading item");
			return (-1);
		}

		/* Make an object */
		o_idx = o_pop();

		/* Paranoia */
		if (o_idx != i)
		{
			note(format("Cannot place object %d!", i));
			return (-1);
		}

		/* Get the object */
		o_ptr = &o_list[o_idx];

		/* Structure Copy */
		object_copy(o_ptr, i_ptr);

		/* Dungeon floor */
		if (!i_ptr->held_m_idx)
		{
			int x = i_ptr->ix;
			int y = i_ptr->iy;

			/* ToDo: Verify coordinates */

			/* Link the object to the pile */
			o_ptr->next_o_idx = cave_o_idx[y][x];

			/* Link the floor to the object */
			cave_o_idx[y][x] = o_idx;
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit > z_info->m_max)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (-1);
	}

	/* Read the monsters */
	for (i = 1; i < limit; i++)
	{
		monster_type *n_ptr;
		monster_type monster_type_body;


		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		(void)WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);


		/* Place monster in dungeon */
		if (monster_place(n_ptr->fy, n_ptr->fx, n_ptr) != i)
		{
			note(format("Cannot place monster %d", i));
			return (-1);
		}
	}


	/*** Holding ***/

	/* Reacquire objects */
	for (i = 1; i < o_max; ++i)
	{
		object_type *o_ptr;

		monster_type *m_ptr;

		/* Get the object */
		o_ptr = &o_list[i];

		/* Ignore dungeon objects */
		if (!o_ptr->held_m_idx) continue;

		/* Verify monster index */
		if (o_ptr->held_m_idx >= z_info->m_max)
		{
			note("Invalid monster index");
			return (-1);
		}

		/* Get the monster */
		m_ptr = &m_list[o_ptr->held_m_idx];

		/* Link the object to the pile */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Link the monster to the object */
		m_ptr->hold_o_idx = i;
	}


	/*** Success ***/

	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Regenerate town in post 2.9.3 versions */
	if (older_than(2, 9, 4) && (p_ptr->depth == 0))
		character_dungeon = FALSE;

	/* Success */
	return (0);
}



/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

	int a_max;

	artifact_type *a_info_new;
	object_lore *a_list_new;

#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
	    sf_major, sf_minor, sf_patch));

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
	if (tmp16u > z_info->r_max)
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (-1);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		/* Read the lore */
		rd_lore(i);
	}
	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->k_max)
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (-1);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		byte tmp8u;

		object_kind *k_ptr = &k_info[i];
		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u == 1) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u >= 2) ? TRUE: FALSE;

		if ((variant_guess_id) && (tmp8u > 2)) k_ptr->guess = (tmp8u - 2);

		/* Activations */
		if ((variant_usage_id) && !(older_than(2,9,6))) rd_s16b(&k_ptr->used);

	}
	if (arg_fiddle) note("Loaded Object Memory");


	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_Q_IDX)
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (-1);
	}

	/* Load the Quests */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		q_list[i].level = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle) note("Loaded Quests");


	/* Load the Artifacts */
	rd_u16b(&tmp16u);	

	a_max = tmp16u;

	/* Incompatible save files */
	if (a_max > 256)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (-1);
	}

	/* Allocate the new artifact range */
	C_MAKE(a_info_new, a_max, artifact_type);

	/* Allocate the new artifact range */
	C_MAKE(a_list_new, a_max, object_lore);

	/* Read the artifact flags */
	for (i = 0; i < a_max; i++)
	{
		object_lore *n_ptr = &a_list_new[i];

		rd_byte(&tmp8u);
		a_info_new[i].cur_num = (tmp8u != 0);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);

		if (variant_learn_id)
		{
			/* Knowledge */
			rd_u32b(&n_ptr->can_flags1);
			rd_u32b(&n_ptr->can_flags2);
			rd_u32b(&n_ptr->can_flags3);

			rd_u32b(&n_ptr->not_flags1);
			rd_u32b(&n_ptr->not_flags2);
			rd_u32b(&n_ptr->not_flags3);

		}

		/* Activations */
		if (variant_usage_id) rd_s16b(&a_info_new[i].activated);

		/* Oops */
		if (variant_learn_id) rd_byte(&tmp8u);
		if (variant_learn_id) rd_byte(&tmp8u);
	}

	if (arg_fiddle) note("Loaded Artifacts");

	if (variant_learn_id || variant_usage_id)
	{
		/* Load the Ego items */
		rd_u16b(&tmp16u);

		/* Incompatible save files */
		if (tmp16u > z_info->e_max)
		{
			note(format("Too many (%u) ego items!", tmp16u));
			return (24);
		}

		/* Read the ego item flags */
		for (i = 0; i < tmp16u; i++)
		{
			object_lore *n_ptr = &e_list[i];

			/* Knowledge */
			if (variant_learn_id)
			{
				rd_u32b(&n_ptr->can_flags1);
				rd_u32b(&n_ptr->can_flags2);
				rd_u32b(&n_ptr->can_flags3);

				rd_u32b(&n_ptr->may_flags1);
				rd_u32b(&n_ptr->may_flags2);
				rd_u32b(&n_ptr->may_flags3);

				rd_u32b(&n_ptr->not_flags1);
				rd_u32b(&n_ptr->not_flags2);
				rd_u32b(&n_ptr->not_flags3);
			}

			/* Oops */
			if (variant_usage_id) rd_byte(&e_info[i].aware);
			if (variant_usage_id) rd_byte(&tmp8u);

			/* Oops */
			if (variant_learn_id) rd_byte(&tmp8u);
			if (variant_learn_id) rd_byte(&tmp8u);
		}

	}

	if (arg_fiddle) note("Loaded Ego Items");


	/* Read the extra stuff */
	if (rd_extra()) return (-1);
	if (arg_fiddle) note("Loaded extra information");


	/* Read random artifacts */
	if ((adult_rand_artifacts) || (a_max == 256))
	{
		if (rd_randarts()) return (-1);
		if (arg_fiddle) note("Loaded Random Artifacts");
	}

	/* Only restore fixed arts if dead */
	if (a_max > z_info->a_max) a_max = z_info->a_max;

	/* Don't restore fixed art knowledge if all random */
	else if (p_ptr->is_dead) a_max = 0;

	/* Copy over the artifact flags */
	for (i = 0; i < a_max; i++)
	{
		object_lore *n_ptr = &a_list[i];
		object_lore *n2_ptr = &a_list_new[i];

		a_info[i].cur_num = a_info_new[i].cur_num;

		if (variant_learn_id)
		{
			/* Knowledge */
			n_ptr->can_flags1 = n2_ptr->can_flags1;
			n_ptr->can_flags2 = n2_ptr->can_flags2;
			n_ptr->can_flags3 = n2_ptr->can_flags3;

			n_ptr->not_flags1 = n2_ptr->not_flags1;
			n_ptr->not_flags2 = n2_ptr->not_flags2;
			n_ptr->not_flags3 = n2_ptr->not_flags3;
		}

		/* Activations */
		if (variant_usage_id) a_info[i].activated = a_info_new[i].activated;
	}

	FREE(a_info_new);
	FREE(a_list_new);

	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &p_info[p_ptr->prace];
	cp_ptr = &c_info[p_ptr->pclass];

	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (-1);
	}


	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i)) return (-1);
	}


	/* I'm not dead yet... */
	if (!p_ptr->is_dead)
	{
		/* Dead players have no dungeon */
		note("Restoring Dungeon...");
		if (rd_dungeon())
		{
			note("Error reading dungeon data");
			return (-1);
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
		return (-1);
	}

	/* Save the encoded checksum */
	n_x_check = x_check;

	/* Read the checksum */
	rd_u32b(&o_x_check);

	/* Verify */
	if (o_x_check != n_x_check)
	{
		note("Invalid encoded checksum");
		return (-1);
	}

#endif


	/* Hack -- no ghosts */
	r_info[z_info->r_max-1].max_num = 0;

        /* Set important Save-File Option */
        variant_save_feats = TRUE;


	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile(void)
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


/*
 * Attempt to Load a "savefile"
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Note that we always try to load the "current" savefile, even if
 * there is no such file, so we must check for "empty" savefile names.
 */
bool load_player(void)
{
	int fd = -1;

	errr err = 0;

	byte vvv[4];

#ifdef VERIFY_TIMESTAMP
	struct stat	statbuf;
#endif /* VERIFY_TIMESTAMP */

	cptr what = "generic";


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the savefile */
	fd = fd_open(savefile, O_RDONLY);

	/* Drop permissions */
	safe_setuid_drop();

	/* No file */
	if (fd < 0)
	{
		/* Give a message */
		msg_print("Savefile does not exist.");
		message_flush();

		/* Allow this */
		return (TRUE);
	}

	/* Close the file */
	fd_close(fd);


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (!err)
	{
		FILE *fkk;

		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Grab permissions */
		safe_setuid_grab();

		/* Check for lock */
		fkk = my_fopen(temp, "r");

		/* Drop permissions */
		safe_setuid_drop();

		/* Oops, lock exists */
		if (fkk)
		{
			/* Close the file */
			my_fclose(fkk);

			/* Message */
			msg_print("Savefile is currently in use.");
			message_flush();

			/* Oops */
			return (FALSE);
		}

		/* Grab permissions */
		safe_setuid_grab();

		/* Create a lock file */
		fkk = my_fopen(temp, "w");

		/* Drop permissions */
		safe_setuid_drop();

		/* Dump a line of info */
		fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

		/* Close the lock file */
		my_fclose(fkk);
	}

#endif /* VERIFY_SAVEFILE */


	/* Okay */
	if (!err)
	{
		/* Grab permissions */
		safe_setuid_grab();

		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* Drop permissions */
		safe_setuid_drop();

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{

#ifdef VERIFY_TIMESTAMP

		/* Grab permissions */
		safe_setuid_grab();

		/* Get the timestamp */
		(void)fstat(fd, &statbuf);

		/* Drop permissions */
		safe_setuid_drop();

#endif /* VERIFY_TIMESTAMP */

		/* Read the first four bytes */
		if (fd_read(fd, (char*)(vvv), sizeof(vvv))) err = -1;

		/* What */
		if (err) what = "Cannot read savefile";

		/* Close the file */
		fd_close(fd);
	}

	/* Process file */
	if (!err)
	{
		/* Extract version */
		sf_major = vvv[0];
		sf_minor = vvv[1];
		sf_patch = vvv[2];
		sf_extra = vvv[3];

		/* Clear screen */
		Term_clear();

		if (older_than(OLD_VERSION_MAJOR, OLD_VERSION_MINOR, OLD_VERSION_PATCH))
		{
			err = -1;
			what = "Savefile is too old";
		}
		else if (!older_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH + 1))
		{
			err = -1;
			what = "Savefile is from the future";
		}
		else
		{
			/* Attempt to load */
			err = rd_savefile();

			/* Message (below) */
			if (err) what = "Cannot parse savefile";
		}
	}

	/* Paranoia */
	if (!err)
	{
		/* Invalid turn */
		if (!turn) err = -1;

		/* Message (below) */
		if (err) what = "Broken savefile";
	}

#ifdef VERIFY_TIMESTAMP
	/* Verify timestamp */
	if (!err && !arg_wizard)
	{
		/* Hack -- Verify the timestamp */
		if (sf_when > (statbuf.st_ctime + 100) ||
		    sf_when < (statbuf.st_ctime - 100))
		{
			/* Message */
			what = "Invalid timestamp";

			/* Oops */
			err = -1;
		}
	}
#endif /* VERIFY_TIMESTAMP */


	/* Okay */
	if (!err)
	{
		/* Give a conversion warning */
		if ((version_major != sf_major) ||
		    (version_minor != sf_minor) ||
		    (version_patch != sf_patch))
		{
			/* Message */
			msg_format("Converted a %d.%d.%d savefile.",
			   sf_major, sf_minor, sf_patch);
			message_flush();
		}

		/* Player is dead */
		if (p_ptr->is_dead)
		{
			/* Forget death */
			p_ptr->is_dead = FALSE;

			/* Cheat death */
			if (arg_wizard)
			{
				/* A character was loaded */
				character_loaded = TRUE;

				/* Done */
				return (TRUE);
			}

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = old_turn = 0;

			/* Done */
			return (TRUE);
		}

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			strcpy(p_ptr->died_from, "(alive and well)");
		}

		/* Success */
		return (TRUE);
	}


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (TRUE)
	{
		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove lock */
		fd_kill(temp);

		/* Drop permissions */
		safe_setuid_drop();
	}

#endif /* VERIFY_SAVEFILE */


	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
	   what, sf_major, sf_minor, sf_patch);
	message_flush();

	/* Oops */
	return (FALSE);
}
