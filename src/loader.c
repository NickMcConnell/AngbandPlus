/* File: loader.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * This file loads savefiles from Ingband 0.1
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
 */
static void rd_item(object_type *o_ptr)
{
	byte old_dd;
	byte old_ds;

	u32b f1, f2, f3;

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

	/* Old flags */
	strip_bytes(12);

	/* Monster holding object */
	rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);


	/* Mega-Hack -- handle "dungeon objects" later */
	if ((o_ptr->k_idx >= 445) && (o_ptr->k_idx <= 479)) return;


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

		/* Acquire correct weight */
		o_ptr->weight = k_ptr->weight;

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
	byte tmp8u;

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
	rd_byte(&tmp8u);
}





/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
	byte tmp8u;

	monster_race *r_ptr = &r_info[r_idx];


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

	/* Repair the lore flags */
	r_ptr->r_flags1 &= r_ptr->flags1;
	r_ptr->r_flags2 &= r_ptr->flags2;
	r_ptr->r_flags3 &= r_ptr->flags3;
	r_ptr->r_flags4 &= r_ptr->flags4;
	r_ptr->r_flags5 &= r_ptr->flags5;
	r_ptr->r_flags6 &= r_ptr->flags6;
}




/*
 * Read a home
 */
static errr rd_home(void)
{
	int j;

	byte num;

	/* Read the basic info - just an item count! */
	rd_byte(&num);

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

		/* Acquire valid items */
		if (home.stock_num < STORE_INVEN_MAX)
		{
			int k = home.stock_num++;

			/* Acquire the item */
			object_copy(&home.stock[k], i_ptr);
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

	u16b c;

	u32b flag[8];
	u32b mask[8];


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


	/*** Cheat flags ***/

	rd_u16b(&c);

	if (c & 0x0002) p_ptr->wizard = TRUE;


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
			if (window_flag_desc[i])
			{
				/* Process valid flags */
				if (mask[n] & (1L << i))
				{
					/* Set */
					if (flag[n] & (1L << i))
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




/*
 * Read the "extra" information
 */
static errr rd_extra(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;

	rd_string(op_ptr->full_name, 32);

	rd_string(p_ptr->died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(p_ptr->history[i], 60);
	}

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
	rd_byte(&tmp8u);	/* oops */

	/* Repair psex (2.8.1 beta) */
	if (p_ptr->psex > MAX_SEXES - 1) p_ptr->psex = MAX_SEXES - 1;

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_byte(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);

	/* Read the stat info */
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cur[i]);

	strip_bytes(24);	/* oops */

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
	strip_bytes(8);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);	/* Old "rest" */
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4);	/* Old "food_digested" / "protection" */
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
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&tmp8u);	/* oops */
	rd_byte(&p_ptr->searching);
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
	for (i = 0; i < tmp16u; i++)
	{
		rd_s16b(&p_ptr->player_hp[i]);
	}


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
#define OLD_GRID_W_01	0x0001	/* Wall type (bit 1) */
#define OLD_GRID_W_02	0x0002	/* Wall type (bit 2) */
#define OLD_GRID_PERM	0x0004	/* Wall type is permanent */
#define OLD_GRID_QQQQ	0x0008	/* Unused */
#define OLD_GRID_MARK	0x0010	/* Grid is memorized */
#define OLD_GRID_GLOW	0x0020	/* Grid is illuminated */
#define OLD_GRID_ROOM	0x0040	/* Grid is part of a room */
#define OLD_GRID_ICKY	0x0080	/* Grid is anti-teleport */

/*
 * Masks for the new grid types
 */
#define OLD_GRID_WALL_MASK	0x0003	/* Wall type */

/*
 * Legal results of OLD_GRID_WALL_MASK
 */
#define OLD_GRID_WALL_NONE		0x0000	/* No wall */
#define OLD_GRID_WALL_MAGMA		0x0001	/* Magma vein */
#define OLD_GRID_WALL_QUARTZ	0x0002	/* Quartz vein */
#define OLD_GRID_WALL_GRANITE	0x0003	/* Granite wall */


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 */
static errr rd_dungeon(void)
{
	int i, y, x;

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
	rd_u16b(&tmp16u);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&ymax);
	rd_s16b(&xmax);
	rd_u16b(&tmp16u);
	rd_u16b(&tmp16u);


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
		note(format("Ignoring illegal dungeon size (%d,%d).", xmax, ymax));
		return (0);
	}

	/* Ignore illegal dungeons */
	if ((px < 0) || (px >= DUNGEON_WID) ||
	    (py < 0) || (py >= DUNGEON_HGT))
	{
		note(format("Ignoring illegal player location (%d,%d).", px, py));
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
			cave_feat[y][x] = tmp8u;

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
	if (limit >= MAX_O_IDX)
	{
		note(format("Too many (%d) object entries!", limit));
		return (151);
	}

	/* Read the dungeon items */
	for (i = 1; i < limit; i++)
	{
		object_type *i_ptr;
		object_type object_type_body;


		/* Acquire place */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);


		/* Monster */
		if (i_ptr->held_m_idx)
		{
			/* Give the object to the monster */
			if (!monster_carry(i_ptr->held_m_idx, i_ptr))
			{
				note(format("Cannot place object %d!", o_max));
				return (152);
			}
		}

		/* Dungeon */
		else
		{
			/* Give the object to the floor */
			if (!floor_carry(i_ptr->iy, i_ptr->ix, i_ptr))
			{
				note(format("Cannot place object %d!", o_max));
				return (152);
			}
		}
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= MAX_M_IDX)
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
		WIPE(n_ptr, monster_type);

		/* Read the monster */
		rd_monster(n_ptr);


		/* Place monster in dungeon */
		if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
		{
			note(format("Cannot place monster %d", i));
			return (162);
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


#ifdef VERIFY_CHECKSUMS
	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;
#endif


	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...",
	            sf_major, sf_minor, sf_patch));



	/* Strip the version bytes */
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
	if (tmp16u > MAX_R_IDX)
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

	}
	if (arg_fiddle) note("Loaded Monster Memory");


	/* Object Memory */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > MAX_K_IDX)
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


	/* Load the Quests */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > 4)
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (23);
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

	/* Incompatible save files */
	if (tmp16u > MAX_A_IDX)
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
	if (rd_extra()) return (25);
	if (arg_fiddle) note("Loaded extra information");


	/* Important -- Initialize the sex */
	sp_ptr = &sex_info[p_ptr->psex];

	/* Important -- Initialize the race/class */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->pclass];


	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}


	/* Read the home */
	if (rd_home()) return (22);


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
	r_info[MAX_R_IDX-1].max_num = 0;


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


