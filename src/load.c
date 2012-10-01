/* File: load2.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

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
		case TV_LITE_SPECIAL:
		case TV_AMULET:
		case TV_RING:
		case TV_MUSIC:
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
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	u16b save_flags;

	u32b f1, f2, f3, f4;

	object_kind *k_ptr;

	char buf[128];

	rd_u16b(&save_flags);

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	rd_byte(&o_ptr->sval);

	/* Special pval */
	if (save_flags & 0x0001) rd_s16b(&o_ptr->pval);

	if (save_flags & 0x0002) rd_byte(&o_ptr->discount);

	rd_byte(&o_ptr->number);
	rd_s16b(&o_ptr->weight);

	if (save_flags & 0x0004) rd_byte(&o_ptr->name1); 
	if (save_flags & 0x0008) rd_byte(&o_ptr->name2);

	if (save_flags & 0x0010) rd_s16b(&o_ptr->timeout);

	if (save_flags & 0x0020) rd_s16b(&o_ptr->to_h);
	if (save_flags & 0x0040) rd_s16b(&o_ptr->to_d);
	if (save_flags & 0x0080) rd_s16b(&o_ptr->to_a);

	if (save_flags & 0x0100) rd_s16b(&o_ptr->ac);

	if (save_flags & 0x0200) rd_byte(&old_dd);
	if (save_flags & 0x0400) rd_byte(&old_ds);

	if (save_flags & 0x0800) rd_byte(&o_ptr->ident);

	if (save_flags & 0x1000) rd_byte(&o_ptr->marked);

	/* Monster holding object */
	if (save_flags & 0x2000) rd_s16b(&o_ptr->held_m_idx);

	if (save_flags & 0x4000) rd_byte(&o_ptr->xtra1);
	if (save_flags & 0x8000) rd_byte(&o_ptr->xtra2);

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Repair non "wearable" items */
	if (!wearable_p(o_ptr) && !((o_ptr->tval==TV_MAGIC_BOOK) &&
		books[o_ptr->sval].flags & SBF_ARTIFACT))
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
		return;
	}

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

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

	/* Get the standard fields */
	o_ptr->ac = k_ptr->ac;
	o_ptr->dd = k_ptr->dd;
	o_ptr->ds = k_ptr->ds;

	/* Get the standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);

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
	rd_byte(&m_ptr->blinded);
	rd_byte(&m_ptr->calmed);

	rd_u16b(&m_ptr->bleeding);
	rd_u16b(&m_ptr->poisoned);
}

/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	u16b save_flags;

	rd_u16b(&save_flags);

	/* Count sights/deaths/kills */
	if (save_flags & 0x0001) rd_s16b(&l_ptr->r_sights);
	if (save_flags & 0x0002) rd_s16b(&l_ptr->r_deaths);
	if (save_flags & 0x0004) rd_s16b(&l_ptr->r_pkills);
	if (save_flags & 0x0008) rd_s16b(&l_ptr->r_tkills);

	/* Count wakes and ignores */
	rd_byte(&l_ptr->r_wake);
	rd_byte(&l_ptr->r_ignore);

	/* Count drops */
	rd_byte(&l_ptr->r_drop_gold);
	rd_byte(&l_ptr->r_drop_item);

	/* Count spells */
	if (save_flags & 0x0010) rd_byte(&l_ptr->r_cast_inate);
	if (save_flags & 0x0020) rd_byte(&l_ptr->r_cast_spell);

	/* Count blows of each type */
	if (save_flags & 0x0040) rd_byte(&l_ptr->r_blows[0]);
	if (save_flags & 0x0080) rd_byte(&l_ptr->r_blows[1]);
	if (save_flags & 0x0100) rd_byte(&l_ptr->r_blows[2]);
	if (save_flags & 0x0200) rd_byte(&l_ptr->r_blows[3]);

	/* Memorize flags */
	if (save_flags & 0x0400) rd_u32b(&l_ptr->r_flags1);
	if (save_flags & 0x0800) rd_u32b(&l_ptr->r_flags2);
	if (save_flags & 0x1000) rd_u32b(&l_ptr->r_flags3);
	if (save_flags & 0x2000) rd_u32b(&l_ptr->r_flags4);
	if (save_flags & 0x4000) rd_u32b(&l_ptr->r_flags5);
	if (save_flags & 0x8000) rd_u32b(&l_ptr->r_flags6);

	/* Read the "Racial" monster limit per level */
	rd_byte(&r_ptr->max_num);

	/* Repair the lore flags */
	l_ptr->r_flags1 &= r_ptr->flags1;
	l_ptr->r_flags2 &= r_ptr->flags2;
	l_ptr->r_flags3 &= r_ptr->flags3;
	l_ptr->r_flags4 &= r_ptr->flags4;
	l_ptr->r_flags5 &= r_ptr->flags5;
	l_ptr->r_flags6 &= r_ptr->flags6;
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

	/* Extract the owner (see above) */
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
		rd_item(i_ptr);

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

	u16b flag16[8];

	/*** Special info ***/

	/* Read "delay_factor" */
	rd_byte(&b);
	op_ptr->delay_factor = b;

	/* Read "hitpoint_warn" */
	rd_byte(&b);
	op_ptr->hitpoint_warn = b;

	/*** Normal Options ***/

	/* Read the option flags */
	for (n = 0; n < 4; n++) rd_u16b(&flag16[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_NORMAL; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if (options[i].text)
		{
			/* Set flag */
			if (flag16[os] & (1L << ob)) op_ptr->opt[i] = TRUE;
	
			/* Clear flag */
			else op_ptr->opt[i] = FALSE;
		}
	}

	/*** Birth and Adult Options ***/

	/* Read the option flags */
	rd_u16b(&flag16[0]);
	rd_u16b(&flag16[1]);

	/* Analyze the options */
	for (i = 0; i < OPT_BIRTH; i++)
	{
		int ob = i % 16;

		/* Process real entries */
		if (options_birth[i].text)
		{
			/* Set flag */
			if (flag16[0] & (1L << ob)) op_ptr->opt_birth[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_birth[i] = FALSE;

			/* Set flag */
			if (flag16[1] & (1L << ob)) op_ptr->opt_adult[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_adult[i] = FALSE;
		}
	}

	/*** Cheating and scoring Options ***/

	/* Read the option flags */
	rd_u16b(&flag16[0]);

	/* Analyze the options */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		int ob = i % 8;

		/* Process real entries */
		if (options_cheat[i].text)
		{
			/* Set flag */
			if (flag16[0] & (1L << ob)) op_ptr->opt_cheat[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_cheat[i] = FALSE;
			/* Set flag */
			if (flag16[0] & (1L << (ob+8))) op_ptr->opt_score[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_score[i] = FALSE;
		}
	}

	/*** Window Options ***/

	/* Read the window flags */
	for (n = 0; n < 8; n++) rd_u16b(&flag16[n]);

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		/* Analyze the options */
		for (i = 0; i < 16; i++)
		{
			/* Process valid flags */
			if (window_flag_desc[i])
			{
				/* Set */
				if (flag16[n] & (1L << i)) op_ptr->window_flag[n] |= (1L << i);
			}
		}
	}
}

/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;

	u32b randart_version;

	rd_string(op_ptr->full_name, 32);

	rd_string(p_ptr->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(p_ptr->history[i], 60);
	}

	/* Class/Race/Gender */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);

	/* Repair psex */
	if (p_ptr->psex > MAX_SEXES - 1) p_ptr->psex = MAX_SEXES - 1;

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_byte(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < A_MAX; i++) 
	{
		rd_s16b(&p_ptr->stat_max[i]);
		rd_s16b(&p_ptr->stat_cur[i]);
	}

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

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
	rd_s16b(&p_ptr->sc);

	/* Read the flags */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->diseased);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->resilient);
	rd_s16b(&p_ptr->absorb);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->rage);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_see_invis);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);
	rd_s16b(&p_ptr->tim_res_lite);
	rd_s16b(&p_ptr->tim_res_dark);
	rd_s16b(&p_ptr->tim_res_confu);
	rd_s16b(&p_ptr->tim_res_sound);
	rd_s16b(&p_ptr->tim_res_shard);
	rd_s16b(&p_ptr->tim_res_nexus);
	rd_s16b(&p_ptr->tim_res_nethr);
	rd_s16b(&p_ptr->tim_res_chaos);
	rd_s16b(&p_ptr->tim_res_disease);
	rd_s16b(&p_ptr->tim_res_water);
	rd_s16b(&p_ptr->racial_power);

	rd_byte(&p_ptr->confusing);
	rd_byte(&p_ptr->searching);

	/* Squelch bytes */
	for (i = 0; i < 24; i++) rd_byte(&squelch_level[i]);
	rd_byte(&auto_destroy);

	/* Read the randart version */
	if (adult_rand_artifacts) rd_u32b(&randart_version);

	/* Read the randart seed */
	if (adult_rand_artifacts) rd_u32b(&seed_randart);

	/* Hack -- the three "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_alchemy);
	rd_u32b(&seed_town);

	/* Special stuff */
	rd_u16b(&p_ptr->panic_save);
	rd_u16b(&p_ptr->total_winner);
	rd_u16b(&p_ptr->noscore);


	/* Read "death" */
	rd_byte(&tmp8u);
	p_ptr->is_dead = tmp8u;

	/* Initialize random artifacts */
	if (adult_rand_artifacts && !(p_ptr->is_dead))
	{
#ifdef GJW_RANDART

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
			return (25);
		}

		/* Initialize randarts */
		do_randart(seed_randart);

#else /* GJW_RANDART */

		note("Random artifacts are disabled in this binary.");
		return (25);

#endif /* GJW_RANDART */

	}

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
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++) rd_s16b(&p_ptr->player_hp[i]);

	return (0);
}

/* 
 * Read the spell information
 */
static errr rd_spells(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u1,tmp16u2;

	/* Legal Spellbooks */
	rd_u16b(&tmp16u1);
	/* Ordered Spells */
	rd_u16b(&tmp16u2);

	/* Incompatible save files */
	if (tmp16u1 > SV_MAX_BOOKS)
	{
		note(format("Too many (%u) spellbook entries!", tmp16u1));
		return (25);
	}
	if (tmp16u2 > MAX_BOOK_SPELLS*SV_MAX_BOOKS)
	{
		note(format("Too many (%u) ordered spells!", tmp16u2));
		return (25);
	}

	/* Read spell info */
	for (i =0; i < tmp16u1; i++)
	{
		rd_byte(&tmp8u); /* The spellbook's index */
		rd_u16b(&p_ptr->spell_learned[tmp8u]);
		rd_u16b(&p_ptr->spell_worked[tmp8u]);
		rd_u16b(&p_ptr->spell_forgotten[tmp8u]);
	}
	for (i = 0; i < MAX_BOOK_SPELLS*SV_MAX_BOOKS; i++)
	{
		if (i<tmp16u2) 
		{
			rd_byte(&p_ptr->spell_order[i][0]);
			rd_byte(&p_ptr->spell_order[i][1]);
		}
		else 
		/* Fill in the array */
		{ 
			p_ptr->spell_order[i][0] = 99;
			p_ptr->spell_order[i][1] = 99;
		}

	}

	return (0);
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

	object_type *i_ptr;
	object_type object_type_body;

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
		rd_item(i_ptr);

		/* Hack -- verify item */
		if (!i_ptr->k_idx) return (53);

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
			return (54);
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

		rd_u16b(&tmp16u);

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

	s16b depth;
	s16b py, px;
	s16b ymax, xmax;

	byte count;
	byte tmp8u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&ymax);
	rd_s16b(&xmax);

	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
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
			/* Extract "feat" */
			cave_set_feat(y, x, tmp8u);

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

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (162);
	}


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= z_info->o_max)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
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
		rd_item(i_ptr);


		/* Make an object */
		o_idx = o_pop();

		/* Paranoia */
		if (o_idx != i)
		{
			note(format("Cannot place object %d!", i));
			return (152);
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

			/* Link the object to the pile */
			o_ptr->next_o_idx = cave_o_idx[y][x];

			/* Link the floor to the object */
			cave_o_idx[y][x] = o_idx;

			/* Rearrange stack if needed */
			rearrange_stack(y, x);
		
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= z_info->m_max)
	{
		note(format("Too many (%d) monster entries!", limit));
		return (161);
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
			return (162);
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

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

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
		return (21);
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		monster_race *r_ptr;
		monster_lore *l_ptr;

		/* Read the lore */
		rd_lore(i);

		/* Get the monster race */
		r_ptr = &r_info[i];
		l_ptr = &l_list[i];

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
		byte tmp8u;

		object_kind *k_ptr = &k_info[i];

		rd_byte(&tmp8u);

		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
		k_ptr->squelch = (tmp8u & 0x04) ? TRUE: FALSE;
		k_ptr->everseen = (tmp8u & 0x08) ? TRUE: FALSE;
	}

	if (arg_fiddle) note("Loaded Object Memory");

	/* Load the alchemy info */
	rd_u16b(&tmp16u);
	if (tmp16u != SV_MAX_POTIONS)
	{
		note(format("Wrong amount (%u) of alchemy info!", tmp16u));
		return (23);
	}

	for (i = 0; i < tmp16u; i++) 
	{
		rd_byte(&tmp8u);
		potion_alch[i].known1 = (tmp8u & 0x01) ? TRUE: FALSE;
		potion_alch[i].known2 = (tmp8u & 0x02) ? TRUE: FALSE;
	}

	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->q_max)
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (23);
	}

	/* Load the Quests */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&q_info[i].type);

		if (q_info[i].type == QUEST_FIXED)
		{
			rd_byte(&q_info[i].active_level);
			rd_s16b(&q_info[i].cur_num);
		}
		else if (q_info[i].type == QUEST_GUILD)
		{
			rd_byte(&q_info[i].reward);
			rd_byte(&q_info[i].active_level);
			rd_byte(&q_info[i].base_level);

			rd_s16b(&q_info[i].r_idx);
			rd_s16b(&q_info[i].cur_num);
			rd_s16b(&q_info[i].max_num);
		}
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
	}
	if (arg_fiddle) note("Loaded Artifacts");

	/* Read the extra stuff */
	if (rd_extra()) return (25);
	if (arg_fiddle) note("Loaded extra information");

	/* Read the spell stuff */
	if (rd_spells()) return (25);
	if (arg_fiddle) note("Loaded spell information");

	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &p_info[p_ptr->prace];
	for (j = 0; j < RACE_SPECIAL_LEVELS; j++)
		rsp_ptr[j] = &race_special_info[(rp_ptr->special)-1][j];
	cp_ptr = &c_info[p_ptr->pclass];

	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}


	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i)) return (22);
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
