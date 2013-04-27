/* File: load.c */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"


/*
 * This file loads savefiles from Zceband.
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
 * Hack -- fix broken crushers
 */
static bool crusher_flag = FALSE;


#define older_than(X) \
    (zce_version < (X))


/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
void note(cptr msg)
{
	static int y = 2;

	/* Draw the message */
	prt(msg, y, 0);

	/* Advance one line (wrap if needed) */
	if (++y > 20)
	{
		c_prt(TERM_L_BLUE, "-- Press a key --", y, 0);
		inkey();
		y = 2;
	}

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
static void rd_item(object_type *o_ptr)
{
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
	rd_byte(&o_ptr->dd);
	rd_byte(&o_ptr->ds);

	rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->marked);

	/* Old flags */
	rd_u32b(&o_ptr->art_flags1);
	rd_u32b(&o_ptr->art_flags2);
	rd_u32b(&o_ptr->art_flags3);

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
	if (normart_p(o_ptr))
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (ego_item_p(o_ptr))
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->name2 = 0;
	}

	/* Acquire standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);


	/* Artifacts */
	if (normart_p(o_ptr))
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
	if (ego_item_p(o_ptr))
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
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
	rd_u32b(&m_ptr->smart);
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
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	byte num;

   s16b tmp16s;

	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);

   if (older_than(1)) rd_s16b(&tmp16s);

	rd_byte(&st_ptr->owner);
	rd_byte(&num);

	if (older_than(1))
	{
		 rd_s16b(&tmp16s);
		 rd_s16b(&tmp16s);
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

	u32b tmp32u;

	/* State */
	rd_u32b(&tmp32u);

	rand_seed(tmp32u);

	/* State */
	for (i = 0; i < 63; i++) rd_u32b(&tmp32u);
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

	if (c & 0x0002) p_ptr->wizard = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_know = (c & 0x1000) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;

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

	/* Strip old data */
	strip_bytes(60);
}


/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i;

	byte tmp8u;

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
	rd_byte(&tmp8u);        /* oops */

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

	strip_bytes(24);        /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->level);

	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->max_level);
	rd_s16b(&p_ptr->max_depth);

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_level < p_ptr->level) p_ptr->max_level = p_ptr->level;

	/* Repair maximum dungeon level */
	if (p_ptr->max_depth < 0) p_ptr->max_depth = 0;

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

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u);        /* oops */
	rd_byte(&tmp8u);        /* oops */
	rd_byte(&tmp8u);        /* oops */
	rd_byte(&p_ptr->searching);
	rd_byte(&p_ptr->maximize);
	rd_byte(&p_ptr->preserve);
	rd_byte(&tmp8u);

	/* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_town);


	/* Special stuff */
	rd_u16b(&panic_save);
	rd_u16b(&total_winner);
	rd_u16b(&p_ptr->noscore);


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
		if ((n == 0xFFFF) || (n == 0xCCCC)) return n;

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
			return (0);
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

	s16b py, px;

	u16b limit;

	int q_idx;

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&p_ptr->depth);
	rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);

	player_place(py, px);

	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);
	rd_s16b(&max_panel_rows);
	rd_s16b(&max_panel_cols);

	/* Maximal size */
	ymax = cur_hgt;
	xmax = cur_wid;


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
			/* Extract "info" */
			cave_info[y][x] = tmp8u;

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
			/* Extract "feat" */
			cave_feat[y][x] = tmp8u;

			/* Fix broken crushers */
			if (!crusher_flag &&
				 ((tmp8u == FEAT_CRUSHER_UP) || (tmp8u == FEAT_CRUSHER_DOWN)))
			{
				crusher_t[crusher_n] = turn;
				crusher_y[crusher_n] = y;
				crusher_x[crusher_n] = x;
				crusher_n++;
			}


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

		s16b o_idx;

		/* Acquire place */
		i_ptr = &object_type_body;

		/* Wipe the object */
		object_wipe(i_ptr);

		/* Read the item */
		rd_item(i_ptr);

		/* Monster */
		if (i_ptr->held_m_idx)
		{
			/* Get the next object */
			o_idx = o_pop();

			/* Save it for later */
			object_copy(&o_list[o_idx], i_ptr);
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

	/* Read the dungeon items */
	for (i = 1; i < o_max; i++)
	{
		object_type *i_ptr = &o_list[i];
		object_type *o_ptr;

		object_type object_type_body;

		o_ptr = &object_type_body;

		/* Monster */
		if (i_ptr->held_m_idx)
		{
			/* Acquire object */
			object_copy(o_ptr, i_ptr);

			/* Reset object */
			object_wipe(i_ptr);

			o_cnt--;

			/* Give the object to the monster */
			if (!monster_carry(o_ptr->held_m_idx, o_ptr))
			{
				note(format("Cannot place object %d!", i));
				return (152);
			}
		}
	}

	q_idx = level_quest[p_ptr->depth];

	/*
	 * Re-enable quest
	 * Heino Vander Sanden
	 */
	if (q_idx >= 0)
	{
		quest_type *q_ptr = &q_list[q_idx];
		monster_race *r_ptr = &r_info[q_ptr->r_idx];

		r_ptr->flags1 |= (RF1_QUESTOR);
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
static errr rd_savefile_aux(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;
	u32b tmp32u;

   char *when;

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	time_t tmp_time;

	/* Strip the version bytes */
	strip_bytes(4);

	/* Hack -- decrypt */
	xor_byte = sf_extra;


	/* Clear the checksums */
	v_check = 0L;
	x_check = 0L;

	/* Read the version */
	rd_u32b(&zce_version);

	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile (type %d)...", sf_major, sf_minor, sf_patch, zce_version));

	/* Time of savefile creation */
	rd_u32b(&sf_when);

	tmp_time = sf_when;

	when = ctime(&tmp_time);
   when[strlen(when)-1] = '\0';
   note(format("Saved on %s", when));

	/* Number of resurrections */
	rd_u16b(&sf_lives);

	/* Number of times played */
	rd_u16b(&sf_saves);

   note(format("Restarted %d times and saved %d times.", sf_lives, sf_saves));


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
	if (tmp16u > MAX_Q_IDX)
	{
		 note(format("Too many (%u) quests!", tmp16u));
		 return (23);
	}
	else
	{
		 total_quests = tmp16u;
	}

	/* Reset quests */
	for (i = 0; i < MAX_DEPTH; i++)
	{
		 level_quest[i] = -1;
	}

	/* Load the Quests */
	for (i = 0; i < total_quests; i++)
	{
		 rd_byte(&tmp8u);
		 q_list[i].level = tmp8u;

		 /* Note this quest */
		 if (tmp8u) level_quest[tmp8u] = i;

		 rd_u16b(&tmp16u);
		 q_list[i].r_idx = tmp16u;
		 rd_byte(&tmp8u);
		 q_list[i].cur_num = tmp8u;
		 rd_byte(&tmp8u);
		 q_list[i].max_num = tmp8u;
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
	tmp16u = rd_inventory();
	if (!tmp16u)
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Reset crushers */
	crusher_n = 0;

	/* Read crushers if proper sentinel */
	if (tmp16u == 0xCCCC)
	{
		rd_s16b(&crusher_n);

		/* Note valid crushers */
		if (crusher_n) crusher_flag = TRUE;

		/* Read the crushers */
		for (i = 0; i < crusher_n; i++)
		{
			rd_s32b(&crusher_t[i]);
			rd_byte(&crusher_y[i]);
			rd_byte(&crusher_x[i]);
		}

		/* Get the sentinel */
		rd_u16b(&tmp16u);

		if (tmp16u != 0xFFFF)
		{
			note("Unable to read crushers");
			return (34);
		}
	}

	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 0; i < tmp16u; i++)
	{
		if (rd_store(i)) return (22);
	}

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
		if (older_than(1)) rd_ghost();
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
errr rd_savefile(void)
{
	errr err;

	/* The savefile is a binary file */
	fff = my_fopen(savefile, "rb");

	/* Paranoia */
	if (!fff) return (-1);

	/* Call the sub-function */
	err = rd_savefile_aux();

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	my_fclose(fff);

	/* Result */
	return (err);
}


