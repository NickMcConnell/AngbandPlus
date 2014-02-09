/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, Jeff Greene, Diego Gonzalez, and others
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "init.h"


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
static ang_file	*fff;

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

static u16b new_artifacts;
static u16b art_norm_count;


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
	(void)Term_fresh();
}


/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
static bool older_than(int x, int y, int z)
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
 * The following functions are used to load the basic building blocks
 * of savefiles.  They also maintain the "checksum" info.
 */

static byte sf_get(void)
{
	byte c, v;

	/* Get a character, decode the value */
	c = file_getc(fff) & 0xFF;
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

	u32b f1, f2, f3, fn;

	object_kind *k_ptr;

	char buf[128];

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Paranoia */
	if ((o_ptr->k_idx < 0) || (o_ptr->k_idx >= z_info->k_max))
	{
		return (-1);
	}

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

	rd_byte(&o_ptr->art_num);
	rd_byte(&o_ptr->ego_num);

	rd_s16b(&o_ptr->timeout);

	rd_s16b(&o_ptr->to_h);
	rd_s16b(&o_ptr->to_d);
	rd_s16b(&o_ptr->to_a);

	rd_s16b(&o_ptr->ac);

	rd_byte(&old_dd);
	rd_byte(&old_ds);

	rd_u32b(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	rd_s16b(&o_ptr->mimic_r_idx);

	/* Old flags */
	strip_bytes(12);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_u32b(&o_ptr->xtra2);

	/* Inscription */
	rd_string(buf, sizeof(buf));

	/* Save the inscription */
	if (buf[0]) o_ptr->obj_note = quark_add(buf);

	/* Object history */
	rd_byte(&o_ptr->origin_nature);
	rd_s16b(&o_ptr->origin_dlvl);
	rd_s16b(&o_ptr->origin_r_idx);
	rd_string(buf, sizeof(buf));
	if (buf[0]) o_ptr->origin_m_name = quark_add(buf);

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
	o_ptr->sval = k_ptr->sval;

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

 	/* Ensure that rods and wands get the appropriate pvals,
	 * and transfer rod charges to timeout.
	 * this test should only be passed once, the first
	 * time the file is open with ROD/WAND stacking code
	 * It could change the timeout improperly if the PVAL (time a rod
	 * takes to charge after use) is changed in object.txt.
	 * But this is nothing a little resting won't solve.
	 *
	 * -JG-
	 */
	if ((o_ptr->tval == TV_ROD) && (o_ptr->pval - (k_ptr->pval * o_ptr->number) != 0))
	{

		o_ptr->timeout = o_ptr->pval;
		o_ptr->pval = k_ptr->pval * o_ptr->number;

	}

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

		if ((o_ptr->tval != TV_MAGIC_BOOK) &&
		    (o_ptr->tval != TV_PRAYER_BOOK) &&
		    (o_ptr->tval != TV_DRUID_BOOK))
		{

			/* Paranoia */
			o_ptr->art_num = o_ptr->ego_num = 0;

			/* All done */
			return (0);
		}

		/*spellbooks can now have an ego-item*/
		else o_ptr->art_num = 0;
	}

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* Paranoia */
	if (o_ptr->art_num)
	{
		artifact_type *a_ptr;

		/*hack - adjust if new artifact*/
		if (o_ptr->art_num >= art_norm_count)
		{

			o_ptr->art_num += new_artifacts;
		}

		/* Paranoia */
		if (o_ptr->art_num >= z_info->art_max)
		{
	    	return (-1);
		}

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->art_num];

		/* Verify that artifact */
		if (a_ptr->tval + a_ptr->sval == 0)
		{
			o_ptr->art_num = 0;
		}
	}

	/* Paranoia */
	if (o_ptr->ego_num)
	{
		ego_item_type *e_ptr;

		/* Paranoia */
		if (o_ptr->ego_num >= z_info->e_max)
		{
		    return (-1);
		}

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->ego_num];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->ego_num = 0;
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
	if (o_ptr->art_num)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->art_num];

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
	if (o_ptr->ego_num)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->ego_num];

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- enforce legal pval */
		if (e_ptr->flags1 & (TR1_PVAL_MASK))
		{
			/* Force a meaningful pval */
			if (!o_ptr->pval) o_ptr->pval = 1;
		}

		/* Mega-Hack - Enforce the special broken items */
		if ((o_ptr->ego_num == EGO_BLASTED) ||
			(o_ptr->ego_num == EGO_SHATTERED))
		{
			/* These were set to k_info values by preceding code */
			o_ptr->ac = 0;
			o_ptr->dd = 0;
			o_ptr->ds = 0;
		}
	}

	/* Hack -- keep boosted damage dice and sides */
	if (o_ptr->dd < old_dd) o_ptr->dd = old_dd;
	if (o_ptr->ds < old_ds) o_ptr->ds = old_ds;

	/* Hack -- *Identified* artifacts are known in future games */
	if ((o_ptr->ident & (IDENT_MENTAL)) && ARTIFACT_EASY_MENTAL(o_ptr))
	{
		/* Mark as *identified* */
		a_l_list[o_ptr->art_num].was_fully_identified = TRUE;
	}

	/* Success */
	return (0);
}




/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	byte num;
	int i;

	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_byte(&m_ptr->m_speed);
	rd_s16b(&m_ptr->m_energy);

	/* Find the number of monster timed effects */
	rd_byte(&num);

	if (num == MON_TMD_MAX)
	{
		/* Read all the effects */
		for (i = 0; i < num; i++) rd_s16b(&m_ptr->m_timed[i]);
	}
	else
	{
		s16b dummy;
		/* Probably in trouble anyway */
		for (i = 0; i < num; i++) rd_s16b(&dummy);

		note("Discarding unsupported monster timed effects");
	}

	rd_u32b(&m_ptr->mflag);
	rd_u32b(&m_ptr->smart);
	rd_byte(&m_ptr->target_y);
	rd_byte(&m_ptr->target_x);
	rd_byte(&m_ptr->mana);

	strip_bytes(1);
}


/*
 * Read and discard all fields of an unknown extension.
 */
static errr rd_unknown_extension(void)
{
	byte tmp8u;
	u16b tmp16u;
	s16b tmp16s;
	u32b tmp32u;
	s32b tmp32s;
	char string[1024];

	while (TRUE)
	{
		/* Read field type */
		rd_byte(&tmp8u);

		/* End mark? */
		if (tmp8u == EXTENSION_TYPE_END) break;

		/* Discard field depending on type */
		switch(tmp8u)
		{
			case EXTENSION_TYPE_U32B: rd_u32b(&tmp32u); break;
			case EXTENSION_TYPE_S32B: rd_s32b(&tmp32s); break;
			case EXTENSION_TYPE_U16B: rd_u16b(&tmp16u); break;
			case EXTENSION_TYPE_S16B: rd_s16b(&tmp16s); break;
			case EXTENSION_TYPE_STRING: rd_string(string, sizeof(string)); break;
			case EXTENSION_TYPE_BYTE: rd_byte(&tmp8u); break;
			/* Garbage? */
			default: return (-1);
		}
	}

	/* Success */
	return (0);
}


/*
 * Process variable extensions.
 */
static errr rd_extensions(void)
{
	s16b extension;

	while (TRUE)
	{
		/* Read extension type */
		rd_s16b(&extension);

		/* End mark? */
		if (extension == END_EXTENSIONS) break;

		/*printf("Extension: %d\n", (int)extension);*/

		/* Process extensions */
		switch (extension)
		{
			/* Unknown. Discard */
			default:
			{
				if (rd_unknown_extension()) return (-1);
				break;
			}
		}
	}

	/* Success */
	return (0);
}


/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 */
static errr rd_effect(void)
{

	int x_idx;
	byte type;
	u16b f_idx;
	byte y;
	byte x;
	byte countdown;
	byte repeats;
	u16b power;
	s16b source;
	u16b flags;

	x_idx = x_pop();

	/*something is wrong*/
	if (!x_idx) return (-1);

	/*Read the effect*/
	rd_byte(&type);
	rd_u16b(&f_idx);


	rd_byte(&y);
	rd_byte(&x);

	rd_byte(&countdown);

	rd_byte(&repeats);

	rd_u16b(&power);

	rd_s16b(&source);

	rd_u16b(&flags);

	/*Write it, unless it is an empty effect*/
	if (type) effect_prep(x_idx, type, f_idx, y, x, countdown, repeats, power, source, flags);

	/* Read a new field, a monster race for inscriptions */
	if (!older_than(0,4,8))
	{
		s16b r_idx;

		rd_s16b(&r_idx);

		x_list[x_idx].x_r_idx = (type ? r_idx: 0);
	}

	/* Success */
	return (0);
}




/*
 * Read the monster lore
 */
static void rd_monster_lore(int r_idx)
{
	byte tmp8u;

	int i;

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

	rd_byte(&l_ptr->ranged);

	/* Count blows of each type */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
		rd_byte(&l_ptr->blows[i]);

	/* Memorize flags */
	rd_u32b(&l_ptr->r_l_flags1);
	rd_u32b(&l_ptr->r_l_flags2);
	rd_u32b(&l_ptr->r_l_flags3);
	rd_u32b(&l_ptr->r_l_flags4);
	rd_u32b(&l_ptr->r_l_flags5);
	rd_u32b(&l_ptr->r_l_flags6);
	rd_u32b(&l_ptr->r_l_flags7);
	rd_u32b(&l_ptr->r_l_native);

	/* Read the "Racial" monster limit per level */
	rd_byte(&r_ptr->max_num);

	/* Hack - allow for new monsters from a modified monster list to appear in a current game. */

	/* In case of a monster entry that wasn't a unique is now made a unique.*/
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		if (r_ptr->max_num > 1) r_ptr->max_num = 1;
	}
	/* Not a unique, but a new monster entry in the current game. */
	else if (r_ptr->max_num == 0) r_ptr->max_num = 100;

	/* Later (?) */
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);

	/* Repair the lore flags */
	l_ptr->r_l_flags1 &= r_ptr->flags1;
	l_ptr->r_l_flags2 &= r_ptr->flags2;
	l_ptr->r_l_flags3 &= r_ptr->flags3;
	l_ptr->r_l_flags4 &= r_ptr->flags4;
	l_ptr->r_l_flags5 &= r_ptr->flags5;
	l_ptr->r_l_flags6 &= r_ptr->flags6;
	l_ptr->r_l_flags7 &= r_ptr->flags7;
	l_ptr->r_l_native &= r_ptr->r_native;

}




/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	byte own, num;

	u32b extra32;
	s16b extra16;


	/* Read the basic info */
	rd_u32b(&extra32);
	rd_s16b(&extra16);
	rd_byte(&own);
	rd_byte(&num);
	rd_s16b(&extra16);
	rd_s16b(&extra16);

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
 * Read artifact lore
 */
static errr rd_artifact_lore(int a_idx)
{
	byte tmp8u;

	/* Read flags */
	rd_byte(&tmp8u);

	/* The artifact was fully identified */
	a_l_list[a_idx].was_fully_identified = ((tmp8u & 0x01) != 0);

	/* For future use */
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);
	rd_byte(&tmp8u);

	return (0);
}


/*
 * Read terrain lore
 */
static errr rd_feature_lore(int f_idx)
{
	/* Get the feature */
	feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];
	byte tmp8u;
	int i;

	rd_byte(&tmp8u);

	/* Activate the "everseen" flag, if needed */
	f_ptr->f_everseen = (tmp8u & 0x01);

	/*Success, for older savefiles*/
	if (older_than(0, 4, 3)) return (0);

	/* Write the terrain_lore memory*/
	rd_byte(&f_l_ptr->f_l_sights);

	/*Write the lore flags*/
	rd_u32b(&f_l_ptr->f_l_flags1);
	rd_u32b(&f_l_ptr->f_l_flags2);
	rd_u32b(&f_l_ptr->f_l_flags3);

	/* Repair the lore flags */
	f_l_ptr->f_l_flags1 &= f_ptr->f_flags1;
	f_l_ptr->f_l_flags2 &= f_ptr->f_flags2;
	f_l_ptr->f_l_flags3 &= f_ptr->f_flags3;

	rd_byte(&f_l_ptr->f_l_defaults);

	/*record the max amount of feat states*/
	rd_byte(&tmp8u);

	/*Failure, if the # of MAX FEAT STATES is desceased*/
	if (tmp8u > MAX_FEAT_STATES) return (-1);

	for (i = 0; i < tmp8u; i++)
	{
		rd_byte(&f_l_ptr->f_l_state[i]);
	}

	rd_byte(&f_l_ptr->f_l_power);

	rd_byte(&f_l_ptr->f_l_dam_non_native);
	rd_byte(&f_l_ptr->f_l_native_moves);
	rd_byte(&f_l_ptr->f_l_non_native_moves);
	rd_byte(&f_l_ptr->f_l_native_to_hit_adj);
	rd_byte(&f_l_ptr->f_l_non_native_to_hit_adj);
	rd_byte(&f_l_ptr->f_l_stealth_adj);


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
		if (!options[i].name) continue;

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
				/* Blank invalid flags */
				if (!(window_mask[n] & (1L << i)))
				{
					window_flag[n] &= ~(1L << i);
				}
			}
		}
	}

	/* Set up the subwindows */
	subwindows_set_flags(window_flag, ANGBAND_TERM_MAX);

}

static errr rd_player_spells(void)
{
	int i;
	u16b tmp16u;

	/* Read the number of spells */
	rd_u16b(&tmp16u);
	if (tmp16u > PY_MAX_SPELLS)
	{
		note(format("Too many player spells (%d).", tmp16u));
		return (-1);
	}

	/* Read the spell flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&p_ptr->spell_flags[i]);
	}

	/* Read the spell order */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&p_ptr->spell_order[i]);
	}

	/* Success */
	return (0);
}


/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;
	u16b file_e_max;
	byte num;


	rd_string(op_ptr->full_name, sizeof(op_ptr->full_name));

	rd_string(p_ptr->died_from, 80);

	rd_string(p_ptr->history, 250);

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

	strip_bytes(1);

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
	for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_birth[i]);
	for (i = 0; i < A_MAX; ++i) rd_s16b(&p_ptr->stat_quest_add[i]);

	rd_s16b(&p_ptr->ht_birth);
	rd_s16b(&p_ptr->wt_birth);
	rd_s16b(&p_ptr->sc_birth);
	rd_s32b(&p_ptr->au_birth);

	strip_bytes(24);	/* oops */

	rd_u16b(&p_ptr->q_fame);
	rd_u16b(&p_ptr->deferred_rewards);

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);

	rd_s32b(&p_ptr->exp);

	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

	/* Verify player level */
	if ((p_ptr->lev < 1) || (p_ptr->lev > z_info->max_level))
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

	rd_s16b(&p_ptr->recall_depth);

	/* Hack -- Repair maximum player level */
	if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

	/* Hack -- Repair maximum dungeon level */
	if (p_ptr->max_depth < 0) p_ptr->max_depth = 1;

	/* Hack -- Repair recall dungeon level */
	if (p_ptr->recall_depth < 0) p_ptr->recall_depth = 1;

	rd_s16b(&p_ptr->quest_depth);

	/* Hack -- Repair max quest level */
	if ((p_ptr->max_depth > 1) &&
		(p_ptr->max_depth > p_ptr->quest_depth))
	{
		p_ptr->quest_depth = p_ptr->max_depth;
	}

	/* More info */
	strip_bytes(6);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);	/* Old "rest" */
	rd_s16b(&p_ptr->food);

	strip_bytes(4);	/* Old "food_digested" / "protection" */
	rd_s16b(&p_ptr->p_energy);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->state.see_infra);

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u);
	rd_byte(&p_ptr->searching);
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */

	/* Find the number of timed effects */
	rd_byte(&num);

	if (num == TMD_MAX)
	{
		/* Read all the effects */
		for (i = 0; i < num; i++)  rd_s16b(&p_ptr->timed[i]);
	}
	else
	{
		s16b dummy2;

		/* Probably in trouble anyway */
		for (i = 0; i < TMD_MAX; i++) rd_s16b(&dummy2);

		/* Discard unused entries */
		note("Discarding unsupported timed effects");
	}

	rd_s16b(&p_ptr->base_wakeup_chance);
	rd_s16b(&total_wakeup_chance);

	/* Read item-quality squelch sub-menu */
 	for (i = 0; i < SQUELCH_BYTES; i++) rd_byte(&squelch_level[i]);

 	/* Load the name of the current greater vault */
	rd_string(g_vault_name, sizeof(g_vault_name));

	/* Read the number of saved ego-item types */
	rd_u16b(&file_e_max);

	/* Read ego-item squelch settings */
	for (i = 0; i < z_info->e_max; i++)
	{
		ego_item_type *e_ptr = &e_info[i];

		tmp8u = 0;

		if (i < file_e_max) rd_byte(&tmp8u);

		e_ptr->squelch |= (tmp8u & 0x01);
		e_ptr->everseen |= (tmp8u & 0x02);

		/* Hack - Repair the savefile */
		if (!e_ptr->everseen) e_ptr->squelch = FALSE;
	}

	/* Read possible extra elements */
	while (i < file_e_max)
	{
		rd_byte(&tmp8u);
		i++;
	}

	/*Read the current number of auto-inscriptions*/
	rd_u16b(&inscriptionsCount);

	/*Read the autoinscriptions array*/
	for(i = 0; i < inscriptionsCount; i++)
	{
		char tmp[80];

		rd_s16b(&inscriptions[i].kindIdx);

		rd_string(tmp, 80);

		inscriptions[i].inscriptionIdx = quark_add(tmp);
	}

	/*
	 * The number of the bone file (if any) that player ghosts should use to
	 * derive the ghost name, sex, class, race, and level.
	 */
	rd_s16b(&player_ghost_num);

	/* Find out how many thefts have recently occurred. */
	strip_bytes(1);

	/* Read number of monster traps on level. */
	rd_byte(&num_trap_on_level);

	/* Future use */
	strip_bytes(13);

	/* Read the summon spells that have already failed on the level */
	rd_u32b(&dungeon_summon_mask_f7);

	/* Read the randart seed */
	rd_u32b(&seed_randart);

	/* Skip the flags */
	strip_bytes(12);

	/* Hack -- the three "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);
	rd_u32b(&seed_ghost);



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

	/*read the level feeling*/
	rd_byte(&tmp8u);
	do_feeling = tmp8u;

	/* Current turn */
	rd_s32b(&turn);

	/*Current Player Turn*/
	rd_s32b(&p_ptr->p_turn);

	/* Turn count for quest indicator */
	rd_u16b(&quest_indicator_timer);

	/* Check if the quest indicator must flash the victory sign */
	if (quest_indicator_timer & (QUEST_INDICATOR_COMPLETE_BIT))
	{
		/* We won the quest */
		quest_indicator_complete = TRUE;
		/* Clear the mark from the timer */
		quest_indicator_timer &= ~(QUEST_INDICATOR_COMPLETE_BIT);
	}

	/* Panel change offsets */
	rd_u16b(&panel_change_offset_y);
	rd_u16b(&panel_change_offset_x);

	/* Check bounds */
	if (panel_change_offset_y < MIN_PANEL_CHANGE_OFFSET_Y)
	{
		panel_change_offset_y = MIN_PANEL_CHANGE_OFFSET_Y;
	}

	if (panel_change_offset_x < MIN_PANEL_CHANGE_OFFSET_X)
	{
		panel_change_offset_x = MIN_PANEL_CHANGE_OFFSET_X;
	}

	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->max_level)
	{
		note(format("Too many (%u) hitpoint entries!", tmp16u));
		return (-1);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}

	/* Read the player spells */
	if (rd_player_spells()) return (-1);

	return (0);
}

/*
 * Read the random artifacts
 */
static errr rd_randarts(void)
{

	int i;
	byte tmp8u;
	s16b tmp16s;
	u16b tmp16u;
	u16b artifact_count, begin;
	s32b tmp32s;
	u32b tmp32u;

	/* Read the number of artifacts */
	rd_u16b(&begin);
	rd_u16b(&artifact_count);

	rd_u16b(&art_norm_count);

	/* Alive or cheating death */
	if (!p_ptr->is_dead || arg_wizard)
	{
		/* Incompatible save files */
		if ((artifact_count > z_info->art_max) || (art_norm_count > z_info->art_norm_max))
		{
			note(format("Too many (%u) artifacts!", artifact_count));
			return (-1);
		}
		/*Mark any new added artifacts*/
		if (art_norm_count < z_info->art_norm_max)
		{
			new_artifacts = z_info->art_norm_max - art_norm_count;
		}
		else new_artifacts = 0;

		/* Mark the old artifacts as "empty" */
		for (i = begin; i < z_info->art_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			/*hack - if a new "normal artifact has been added in mid-game, don't erase it*/
			if ((i >= art_norm_count) && (i < z_info->art_norm_max)) continue;

			a_ptr->tval = 0;
			a_ptr->sval = 0;
			a_ptr->name[0] = '\0';
		}

		/* Read the artifacts */
		for (i = begin; i < artifact_count; i++)
		{

			artifact_type *a_ptr = &a_info[i];

			/*hack - if a new "normal artifact has been added in mid-game, don't erase it*/
			if ((i >= art_norm_count) && (i < z_info->art_norm_max)) continue;

			rd_string (a_ptr->name, MAX_LEN_ART_NAME);

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
			rd_u32b(&a_ptr->a_flags1);
			rd_u32b(&a_ptr->a_flags2);
			rd_u32b(&a_ptr->a_flags3);
			rd_u32b(&a_ptr->a_native);
			rd_byte(&a_ptr->a_level);
			rd_byte(&a_ptr->a_rarity);
			rd_byte(&a_ptr->activation);
			rd_u16b(&a_ptr->time);
			rd_u16b(&a_ptr->randtime);
		}
	}
	else
	{
		/* Strip the the artifacts for a dead/new character*/
		for (i = begin; i < artifact_count; i++)
		{
			char tmpstr[MAX_LEN_ART_NAME];
			rd_string (tmpstr, sizeof (tmpstr)); /*a_ptr->name*/
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
			rd_u32b(&tmp32u); /* a_ptr->a_native */
			rd_byte(&tmp8u); /* a_ptr->level */
			rd_byte(&tmp8u); /* a_ptr->rarity */

			rd_byte(&tmp8u); /* a_ptr->activation */
			rd_u16b(&tmp16u); /* a_ptr->time */
			rd_u16b(&tmp16u); /* a_ptr->randtime */
		}
	}

	return (0);

}

/*
 * Read the notes. Every new savefile has at least NOTES_MARK.
 */
static bool rd_notes(void)
{
	int alive = (!p_ptr->is_dead || arg_wizard);
	char tmpstr[100];

	if (alive && adult_take_notes)
	{
		/* Create the tempfile (notes_file & notes_fname are global) */
		create_notes_file();

		if (!notes_file)
		{
			note("Can't create a temporary file for notes");
			return (TRUE);
		}

		/* Append the notes in the savefile to the tempfile*/
		while (TRUE)
		{

			rd_string(tmpstr, sizeof(tmpstr));
			/* Found the end? */
			if (strstr(tmpstr, NOTES_MARK))
			break;
			file_putf(notes_file, "%s\n", tmpstr);
		}

		/* Paranoia. Remove the notes from memory */
		file_flush(notes_file);

	}
	/* Ignore the notes */
	else
	{
		while (TRUE)
		{

			rd_string(tmpstr, sizeof(tmpstr));

			/* Found the end? */
			if (strstr(tmpstr, NOTES_MARK))
			{
				break;
			}
		}
	}

	return (FALSE);
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
		if (!i_ptr->k_idx)	return (-1);

		/* Verify slot */
		if (n >= ALL_INVEN_TOTAL) return (-1);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* One more item */
			if (!IS_QUIVER_SLOT(n)) p_ptr->equip_cnt++;
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

			/* One more item */
			p_ptr->inven_cnt++;
		}

		/* Update "p_ptr->pack_size_reduce" */
		save_quiver_size();
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
		rd_string(buf, sizeof(buf));

		/* Read the message type */
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

	byte count;
	byte tmp8u;
	u16b tmp16u;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_u16b(&p_ptr->dungeon_type);
	/* Get dungeon capabilities */
	set_dungeon_type(p_ptr->dungeon_type);

	rd_s16b(&py);
	rd_s16b(&px);
	rd_byte(&p_ptr->cur_map_hgt);
	rd_byte(&p_ptr->cur_map_wid);
	rd_u16b(&altered_inventory_counter);
	/* Paranoia */
	allow_altered_inventory = FALSE;

	rd_u16b(&tmp16u);


	/* Ignore illegal dungeons */
	if ((depth < 0) || (depth >= MAX_DEPTH))
	{
		note(format("Ignoring illegal dungeon depth (%d)", depth));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((p_ptr->cur_map_hgt > MAX_DUNGEON_HGT) || (p_ptr->cur_map_wid > MAX_DUNGEON_WID))
	{
		/* XXX XXX XXX */
		note(format("Ignoring illegal dungeon size (%d,%d).", p_ptr->cur_map_hgt, p_ptr->cur_map_wid));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= p_ptr->cur_map_wid) ||
		(py < 0) || (py >= p_ptr->cur_map_hgt))
	{
		note(format("Ignoring illegal player location (%d,%d).", py, px));
		return (1);
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < p_ptr->cur_map_hgt; )
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
			if (++x >= p_ptr->cur_map_wid)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= p_ptr->cur_map_hgt) break;
			}
		}
	}


	/*** Run length decoding ***/

	/* Load the dungeon data */
	for (x = y = 0; y < p_ptr->cur_map_hgt; )
	{
		/* Grab RLE info */
		rd_byte(&count);
		rd_byte(&tmp8u);

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			feature_type *f_ptr;

			/* Extract "feat" */
			cave_feat[y][x] = tmp8u;

			update_los_proj_move(y, x);

			/* Get fast access to feature */
			f_ptr = &f_info[tmp8u];

			/* Handle glowing grids */
			if (_feat_ff2_match(f_ptr, FF2_GLOW))
			{
				int d;

				/* Turn on super glow */
				cave_info[y][x] |= (CAVE_HALO);

				/* Spread super glow through adjacent grids */
				for (d = 0; d < 8; d++)
				{
					/* Get coordinates */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Ignore annoying locations */
					if (!in_bounds_fully(yy, xx))
					{
						continue;
					}

					/* Turn on super glow */
					cave_info[yy][xx] |= (CAVE_HALO);
				}
			}

			/* Register dynamic features */
			if (_feat_ff3_match(f_ptr, FF3_DYNAMIC))
			{
				(void)add_dynamic_terrain(y, x);
			}

			/* Update the flags of the current level */
			if (_feat_ff3_match(f_ptr, TERRAIN_MASK))
			{
				level_flag |= get_level_flag((u16b)(f_ptr - f_info));
			}

			/* Advance/Wrap */
			if (++x >= p_ptr->cur_map_wid)
			{
				/* Wrap */
				x = 0;

				/* Advance/Wrap */
				if (++y >= p_ptr->cur_map_hgt) break;
			}
		}
	}


	/*** Player ***/

	/* Load depth */
	p_ptr->depth = depth;


	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (-1);
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

			/* Rearrange stack if needed */
			rearrange_stack(y, x);
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
		monster_race *r_ptr;

		int r_idx;

		/* Get local monster */
		n_ptr = &monster_type_body;

		/* Clear the monster */
		(void)WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);

		/* Access the "r_idx" of the chosen monster */
		r_idx = n_ptr->r_idx;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* If a player ghost, some special features need to be added. */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			(void)prepare_ghost(n_ptr->r_idx);
		}

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
		if (o_ptr->held_m_idx > z_info->m_max)
		{
			note("Invalid monster index");
			return (-1);
		}

		/* Get the monster */
		m_ptr = &mon_list[o_ptr->held_m_idx];

		/* Link the object to the pile */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Link the monster to the object */
		m_ptr->hold_o_idx = i;
	}

	/*** Effects ***/
	/* Read the effect count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit > z_info->x_max)
	{
		note(format("Too many (%d) effect entries!", limit));
		return (-1);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{

		/* Read the item */
		if (rd_effect())
		{
			note("Error reading effect");
			return (-1);
		}
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
	int i;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
	            sf_major, sf_minor, sf_patch));

	/* Strip the version bytes, and the game_mode byte */
	strip_bytes(5);

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
		rd_monster_lore(i);
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

		k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
		k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
		k_ptr->everseen = (tmp8u & 0x08) ? TRUE: FALSE;

		rd_byte(&k_ptr->squelch);

		/* Hack - Repair the savefile */
		if (!k_ptr->everseen) k_ptr->squelch = SQUELCH_NEVER;

	}
	if (arg_fiddle) note("Loaded Object Memory");

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
		quest_type *q_ptr = &q_info[i];

		rd_byte(&q_ptr->q_type);
		/* Only limited info for permanent quests.  The rest is detailed in quest.txt */
		if (q_ptr->q_type == QUEST_PERMANENT)
		{
			rd_byte(&q_ptr->q_flags);
			rd_s16b(&q_ptr->q_num_killed);
			continue;
		}

		rd_u16b(&q_ptr->q_reward);
		rd_u16b(&q_ptr->q_fame_inc);
		rd_byte(&q_ptr->base_level);
		rd_byte(&q_ptr->q_theme);
		rd_s16b(&q_ptr->mon_idx);
		rd_s32b(&q_ptr->turn_counter);
		rd_s16b(&q_ptr->q_num_killed);
		rd_s16b(&q_ptr->q_max_num);
		rd_byte(&q_ptr->q_flags);
	}

	if (arg_fiddle) note("Loaded Quests");

	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > z_info->art_max)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (-1);
	}

	/* Read the artifact flags */
	for (i = 0; i < tmp16u; i++)
	{
		rd_byte(&tmp8u);
		a_info[i].a_cur_num = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	if (arg_fiddle) note("Loaded Artifacts");


	/* Read the extra stuff */
	if (rd_extra()) return (-1);
	if (arg_fiddle) note("Loaded extra information");

	if (rd_randarts()) return (-1);
	if (arg_fiddle) note("Loaded Random Artifacts");

	if (rd_notes()) return (-1);
	if (arg_fiddle) note("Loaded Notes");

	if (rd_extensions()) return (-1);
	if (arg_fiddle) note("Loaded Extensions");

	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &p_info[p_ptr->prace];
	cp_ptr = &c_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &cp_ptr->spells;

	/* Hack - In NPP 050, we moved a spell out of ironman book.*/
	if (cp_ptr->spell_book == TV_MAGIC_BOOK)  p_ptr->spell_flags[SPELL_FLIGHT] &= ~(PY_SPELL_IRONMAN);

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


	/* Read the stored number of terrain features */
	rd_u16b(&tmp16u);

	/* Check bounds */
	if (tmp16u > z_info->f_max)
	{
		note(format("Too many (%u) terrain features!", tmp16u));
		return (-1);
	}

	/* Read terrain lore */
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_feature_lore(i)) return (-1);
	}

	/* Artifact lore */
	/* Read the stored number of artifacts (normal + special) */
	rd_u16b(&tmp16u);

	/* Check bounds */
	if (tmp16u > z_info->art_norm_max)
	{
		note(format("Too many (%u) artifacts!", tmp16u));
		return (-1);
	}

	/* Read artifact lore */
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_artifact_lore(i)) return (-1);
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

	}

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

	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
static errr rd_savefile(void)
{
	errr err;

	/* Grab permissions */
	safe_setuid_grab();

	/* The savefile is a binary file */
	fff = file_open(savefile, MODE_READ, -1);

	/* Drop permissions */
	safe_setuid_drop();

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = rd_savefile_new_aux();

	/* Check for errors */
	if (file_error(fff)) err = -1;

	/* Close the file */
	file_close(fff);

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
	byte savefile_game;

	cptr what = "generic";

	/* Paranoia */
	turn = 0;

	p_ptr->p_turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;

	load_player_ghost_file();

	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);

	/* Grab permissions */
	safe_setuid_grab();

	/* Open the savefile */
	fff = file_open(savefile, MODE_READ, -1);
	if (fff) fd = 0;
	else fd = -1;

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
	file_close(fff);

	/* Okay */
	if (!err)
	{
		/* Grab permissions */
		safe_setuid_grab();

		/* Open the savefile */
		fff = file_open(savefile, MODE_READ, -1);
		if (fff)
		{
			fd = 0;
		}
		else fd = -1;

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

		/* Read the first four bytes */
		if (!file_read(fff, (char*)(vvv), 4)) err = -1;

		/* What */
		if (err) what = "Cannot read savefile";

		rd_byte(&savefile_game);
		
		/* Close the file */
		file_close(fff);
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
		else if (game_mode != savefile_game)
		{
			err = -1;
			if (game_mode == GAME_NPPMORIA) what = "Not a NPPMoria savefile";
			else if (game_mode == GAME_NPPANGBAND) what = "Not a NPPAngband savefile";
			else what = "Unknown savefile type";
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
			/*note, add or_true to the arg wixard if statement to resurrect character*/
			/* Cheat death (unless the character retired) */
			if (arg_wizard)
			{
				/*heal the player*/
				hp_player(2000);

				/* Forget death */
				p_ptr->is_dead = FALSE;

				/* A character was loaded */
				character_loaded = TRUE;

				/* Done */
				return (TRUE);
			}

			/* Forget death */
			p_ptr->is_dead = FALSE;

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = 0;

			p_ptr->p_turn = 0;

			/* Done */
			return (TRUE);
		}

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			my_strcpy(p_ptr->died_from, "(alive and well)", sizeof(p_ptr->died_from));
		}

		/* Success */
		return (TRUE);
	}

	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
	           what, sf_major, sf_minor, sf_patch);
	message_flush();

	/* Oops */
	return (FALSE);
}

/*
 * Attempt to Load a gamemode
 *
 * That is, load just enough of the save file to determine whether
 * we are playing Angband or Moria (and thus set game_mode) before
 * returning
 *
 */
void load_gamemode(void)
{
	int fd = -1;

	errr err = 0;

	byte vvv[4];
	byte savefile_game=0;

	/* Allow empty savefile name */
	if (!savefile[0]) return;

	/* Okay */
	if (!err)
	{
		/* Grab permissions */
		safe_setuid_grab();

		/* Open the savefile */
		fff = file_open(savefile, MODE_READ, -1);
		if (fff)
		{
			fd = 0;
		}
		else fd = -1;

		/* Drop permissions */
		safe_setuid_drop();

		/* No file */
		if (fd < 0) err = -1;
	}

	/* Process file */
	if (!err)
	{
		/* Read the first four bytes */
		if (!file_read(fff, (char*)(vvv), 4)) err = -1;

		rd_byte(&savefile_game);
		
		/* Close the file */
		file_close(fff);
	}
	
	/* Need to clear checksums so they wont glitch out the real load */
	xor_byte = 0;
	v_check = 0L;
	x_check = 0L;
	
	if (err)
	{
		return;	
	}

	if (savefile_game == GAME_NPPMORIA)
	{
		game_mode = GAME_NPPMORIA;
	}
	else if (savefile_game == GAME_NPPANGBAND)
	{
		game_mode = GAME_NPPANGBAND;
	}

	return;
}
