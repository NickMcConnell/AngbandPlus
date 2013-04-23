#define LOAD_C
/* File: load2.c */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"
#include "loadsave.h"


/*
 * This file loads savefiles from Cthangband 3.0.0 onwards
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
 * Note that MAX_CAVES, MAX_TOWNS, MAX_SCHOOL and MAX_SPIRITS are
 * assumed to be constant here.
 *
 * XXX XXX XXX
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
 * This function determines if certain features are present in the savefile.
 */
bool has_flag(u16b flag)
{
	return ((sf_flags & flag) != 0);
}


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

static void rd_char(char *ip)
{
	rd_byte((byte*)ip);
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
	byte old_dd;
	byte old_ds;

	u32b f1, f2, f3;

	object_kind *k_ptr;

	char buf[128];


	/* Kind */
	rd_s16b(&o_ptr->k_idx);
	o_ptr->k_idx = convert_k_idx(o_ptr->k_idx, sf_flags, sf_flags_now);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
	rd_byte(&o_ptr->tval);
	strip_bytes(1); /* Was sval. */

	/* Special pval */
	rd_s16b(&o_ptr->pval);

	/* New method */
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

#ifdef SF_16_IDENT
		/* ident enlarged to enable splitting of IDENT_SENSE */
		if (has_flag(SF_16_IDENT))
		{
			rd_u16b(&o_ptr->ident);
			if (o_ptr->discount)
			{
				o_ptr->ident |= IDENT_SENSE;
			}
		}
		else
		{
			byte temp;
			rd_byte(&temp);
			o_ptr->ident = (u32b)(temp & ~0x01);
			if ((temp & 0x01) || (o_ptr->discount)) (o_ptr->ident)+=IDENT_SENSE;
		}
#else
		rd_byte(&o_ptr->ident);
#endif

		rd_byte(&o_ptr->marked);

	/* Old flags */
	rd_u32b(&o_ptr->flags1);
	rd_u32b(&o_ptr->flags2);
	rd_u32b(&o_ptr->flags3);

	/* New version */
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

	if (o_ptr->k_idx < 0 || o_ptr->k_idx >= MAX_K_IDX)
	{
		note("Destroying object with a bad k_idx.");
		excise_dun_object(o_ptr);
		object_wipe(o_ptr);
		return;
	}

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval from k_info */
	o_ptr->tval = k_ptr->tval;


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
		o_ptr->name1 = o_ptr->name2 = o_ptr->art_name = 0;

		/* All done */
		return;
	}


	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Paranoia */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Treat an out of bounds name1 as mundane. */
		if (o_ptr->name1 >= MAX_A_IDX) o_ptr->name1 = 0;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->name1 = 0;
	}

	/* Paranoia */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Treat an out of bounds name2 as mundane. */
		if (o_ptr->name2 >= MAX_E_IDX) o_ptr->name2 = 0;

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


		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

	}

	if (o_ptr->art_name) /* A random artifact */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
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

	m_ptr->r_idx = convert_r_idx(m_ptr->r_idx, sf_flags, sf_flags_now);

	/* Read the other information */
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_byte(&m_ptr->generation);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_s16b(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
    rd_u32b(&m_ptr->smart);
	rd_byte(&tmp8u);

	if (m_ptr->r_idx < 0 || m_ptr->r_idx >= MAX_R_IDX ||
		!r_info[m_ptr->r_idx].name)
	{
		note("Deleting monster with an unknown race.");
		delete_monster_idx(m_ptr-m_list, FALSE);
	}
}





/*
 * Read the monster lore
 */
static void rd_lore(monster_race *r_ptr)
{
	byte tmp8u;

		/* Count sights/deaths/kills */
		rd_s16b(&r_ptr->r_sights);
		rd_s16b(&r_ptr->r_deaths);
		rd_s16b(&r_ptr->r_pkills);
		rd_s16b(&r_ptr->r_tkills);

		/* Count wakes and ignores */
		rd_byte(&r_ptr->r_wake);
		rd_byte(&r_ptr->r_ignore);

		/* Extra stuff */
#if 0
		rd_byte(&r_ptr->r_xtra1);
		rd_byte(&r_ptr->r_xtra2);
#else
		strip_bytes(2);
#endif

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




#ifdef SF_DEATHEVENTTEXT
/*
 * Read the death events
 */
static void rd_death(void)
{
	int i,j;
	u16b tmp16u;
	for (i = 0;; i++)
	{
		rd_u16b(&tmp16u);
		for (j = 0; j < 15; i++)
		{
			death_event_type *d_ptr = &death_event[15*i+j];
			/* Don't leave the array */
			if (15*i+j >= MAX_DEATH_EVENTS) break;
			if (tmp16u & 1<<i) d_ptr->flags |= EF_KNOWN;
		}
		if (tmp16u & 1<<15) break;
	}
}
#endif




/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;
	byte num;

	/* Read the basic info */
	rd_byte(&st_ptr->type);
	rd_byte(&st_ptr->x);
	rd_byte(&st_ptr->y);
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&st_ptr->bought);
#ifdef SF_QUEST_DIRECT
	if (has_flag(SF_QUEST_DIRECT))
	{
		rd_s16b(&st_ptr->owner);
	}
	else
	{
		byte z;
		rd_byte(&z);
		st_ptr->owner = convert_owner(n*MAX_OWNERS+z, sf_flags, sf_flags_now);
	}

	/* Pick a new owner if the current one has been removed. */
	if (st_ptr->owner < 0 || st_ptr->owner > NUM_OWNERS)
	{
		msg_format("Strange shopkeeper in shop %d - finding a new one.", n);
		store_shuffle(n);
	}

#else /* SF_QUEST_DIRECT */
	rd_byte(&st_ptr->owner);
#endif /* SF_QUEST_DIRECT */

	rd_byte(&num);
	rd_s16b(&st_ptr->good_buy);
	rd_s16b(&st_ptr->bad_buy);

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
	byte flagw[8][32];


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

	if (c & 0x0002) cheat_wzrd = TRUE;

	cheat_peek = (c & 0x0100) ? TRUE : FALSE;
	cheat_hear = (c & 0x0200) ? TRUE : FALSE;
	cheat_room = (c & 0x0400) ? TRUE : FALSE;
	cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
	cheat_live = (c & 0x2000) ? TRUE : FALSE;
	cheat_skll = (c & 0x4000) ? TRUE : FALSE;

		rd_byte(&autosave_l);
		rd_byte(&autosave_t);
#ifdef SF_Q_SAVE
		if (has_flag(SF_Q_SAVE))
		{
			autosave_q = autosave_t & 0x02;
			autosave_t &= 0x01;
		}
#endif
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

	/* Copy across the birth options if they weren't previously known. */
	{
		option_type *op_ptr, *o2_ptr;
		for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
		{
			if (op_ptr->o_page == OPTS_BIRTHR) break;
		}

		/* Only do something if this option wasn't in the mask. */
		if (!(mask[op_ptr->o_set] & (1L << op_ptr->o_bit)))
		{
			for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
			{
				if (op_ptr->o_page != OPTS_BIRTH) continue;
				o2_ptr = op_ptr+1;

				/* Only some birth options affect the game afterwards. */
				if (o2_ptr->o_page != OPTS_BIRTHR) continue;

				/* Copy the BIRTH option to the BIRTHR equivalent. */
				if (option_flag[op_ptr->o_set] & 1L << (op_ptr->o_bit))
				{
					option_flag[o2_ptr->o_set] |= 1L << (o2_ptr->o_bit);
				}
				else
				{
					option_flag[o2_ptr->o_set] &= ~(1L << (o2_ptr->o_bit));
				}
			}
		}
	}

	/*** Window Options ***/

#ifdef SF_3D_WINPRI
	/* Read the window flags */
	if (has_flag(SF_3D_WINPRI))
	{
	for (n = 0; n < 8; n++)
	{
		for (i = 0; i < 32; i++)
		{
				rd_byte(&flagw[n][i]);
			}
		}
					}
					else
					{
		for (n = 0; n < 8; n++)
		{
			u32b temp;
			rd_u32b(&temp);
			for (i = 0; i < 32; i++)
			{
				if (temp & 1<<i)
				{
					flagw[n][i] = 0x55;
				}
				else
				{
					flagw[n][i] = 0x00;
				}
					}
					}
				}                           


	/* Read the window masks */
	for (n = 0; n < 8; n++)
	{
		rd_u32b(&mask[n]);

		/* Was &=, but this seemed to just prohibit adding new options. */
		mask[n] = windows[n].mask;
	}

	/* Analyze the options */
	for (n = 0; n < 8; n++)
	{
		for (c = 0; c < 32; c++)
		{
			/* Set */
			if (flagw[n][c] && mask[n] & (1L << c))
			{
				/* Set */
				windows[n].pri[c] = flagw[n][c] % 16;
				windows[n].rep[c] = flagw[n][c] / 16;
			}
		}
	}
#else
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
#endif
}





/*
 * Hack -- read the "ghost" info
 *
 */
static void rd_ghost(void)
{
	int i;
    
	monster_race *r_ptr = r_info+MON_PLAYER_GHOST;

	/* Name */
	rd_string(r_name + r_ptr->name, 64);

	/* Visuals */
	rd_byte((byte *)&r_ptr->d_char);
	rd_byte(&r_ptr->d_attr);

	/* Level/Rarity */
	rd_byte(&r_ptr->level);
	rd_byte(&r_ptr->rarity);
	rd_byte(&r_ptr->cur_num);
	rd_byte(&r_ptr->max_num);

	/* Misc info */
	rd_byte(&r_ptr->hdice);
	rd_byte(&r_ptr->hside);
	rd_s16b(&r_ptr->ac);
	rd_s16b(&r_ptr->sleep);
	rd_byte(&r_ptr->aaf);
	rd_byte(&r_ptr->speed);

	/* Experience */
	rd_s32b(&r_ptr->mexp);

	/* Extra */
#if 0
	rd_s16b(&r_ptr->extra);
#else
	strip_bytes(2);
#endif

	/* Frequency */
	rd_byte(&r_ptr->freq_inate);
	rd_byte(&r_ptr->freq_spell);
	rd_byte(&r_ptr->num_blows);

	/* Flags */
	rd_u32b(&r_ptr->flags1);
	rd_u32b(&r_ptr->flags2);
	rd_u32b(&r_ptr->flags3);
	rd_u32b(&r_ptr->flags4);
	rd_u32b(&r_ptr->flags5);
	rd_u32b(&r_ptr->flags6);

	/* Attacks */
	for (i = 0; i < 4; i++)
	{
		rd_byte(&r_ptr->blow[i].method);
		rd_byte(&r_ptr->blow[i].effect);
		rd_byte(&r_ptr->blow[i].d_dice);
		rd_byte(&r_ptr->blow[i].d_side);
	}

	/* Hack -- set the "graphic" info */
	r_ptr->x_attr = r_ptr->d_attr;
	r_ptr->x_char = r_ptr->d_char;

}



/*
 * Copy the variable parts of skill_set[from] to skill_set[to].
 */
static void skill_copy(int to, int from)
{
	player_skill *sf_ptr = skill_set+from;
	player_skill *st_ptr = skill_set+to;
	
	st_ptr->value = sf_ptr->value;
	st_ptr->max_value = sf_ptr->max_value;
#ifdef SF_SKILL_BASE
	st_ptr->base = sf_ptr->base;
	st_ptr->ceiling = sf_ptr->ceiling;
#endif
#if 0 /* exp_to_raise is only set at program start. */
	st_ptr->exp_to_raise = sf_ptr->exp_to_raise;
#endif
	st_ptr->experience = sf_ptr->experience;
}


/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
	int i,j;

	byte tmp8u;
	u32b tmp32u;
	rd_string(player_name, 32);

	{
		char buf[1024];
		rd_string(buf, 1024);
		died_from = string_make(buf);
	}

	for (i = 0; i < 4; i++)
	{
		rd_string(history[i], 60);
	}

	/* Template/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);
	rd_byte(&p_ptr->ptemplate);
	rd_byte(&p_ptr->psex);
	rd_byte(&tmp8u);        /* oops */

	/* Special Race info */
	rd_byte(&p_ptr->hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->wt);
	rd_s16b(&p_ptr->birthday);
	rd_s16b(&p_ptr->startdate);

	/* Read the stat info */
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_max[i]);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cur[i]);

	strip_bytes(24);        /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

#ifdef SF_SAVE_MAX_SKILLS
	if (has_flag(SF_SAVE_MAX_SKILLS))
	{
		rd_byte(&tmp8u);
	}
	else
#endif /* SF_SAVE_MAX_SKILLS */
	{
		tmp8u = 27;
	}

	for (i=0; i < tmp8u && i < MAX_SKILLS; i++)
	{
		rd_byte(&(skill_set[i].value));
		rd_byte(&(skill_set[i].max_value));
#ifdef SF_SKILL_BASE
		if (has_flag(SF_SKILL_BASE)) {
			rd_byte(&(skill_set[i].base));
			rd_byte(&(skill_set[i].ceiling));
		} else {
			skill_set[i].base = 0;
			skill_set[i].ceiling = 100;
		}
#endif
		rd_u16b(&(skill_set[i].exp_to_raise));
		rd_u16b(&(skill_set[i].experience));
	}

	/* If too many skills were saved. strip the extras. */
	if (MAX_SKILLS < tmp8u)
	{
		strip_bytes(6*(tmp8u-MAX_SKILLS));
#ifdef SF_SKILL_BASE
		if (has_flag(SF_SKILL_BASE)) strip_bytes(2*(tmp8u-MAX_SKILLS));
#endif /* SF_SKILL_BASE */
	}
	
#ifdef SF_SKILL_BASE
# ifdef SKILL_PSEUDOID
	if (tmp8u < SKILL_PSEUDOID) skill_copy(SKILL_PSEUDOID, SKILL_DEVICE);
# endif /* SKILL_PSEUDOID */
#endif /* SF_SKILL_BASE */


	rd_s16b(&p_ptr->mhp);
	rd_s16b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

	rd_s16b(&p_ptr->mchi);
	rd_s16b(&p_ptr->cchi);
	rd_u16b(&p_ptr->chi_frac);

#ifdef SF_QUEST_DIRECT
	if (has_flag(SF_QUEST_DIRECT))
	{
		rd_s16b(&p_ptr->max_dlv);
	}
	else
	{
		for (i = 0; i < 20; i++)
		{
			s16b tmp;
			if (cur_dungeon == i) rd_s16b(&p_ptr->max_dlv);
			else rd_s16b(&tmp);
		}
	}
#else /* SF_QUEST_DIRECT */
	for(i=0;i<20;i++)
	{
		rd_s16b(&p_ptr->max_dlv[i]);
	}
#endif /* SF_QUEST_DIRECT */

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
	strip_bytes(18);
	rd_s16b(&p_ptr->chaos_patron);
	rd_u32b(&p_ptr->muta1);
	rd_u32b(&p_ptr->muta2);
	rd_u32b(&p_ptr->muta3);

	rd_byte(&p_ptr->confusing);
#ifdef SF_QUEST_DIRECT
	if (!has_flag(SF_QUEST_DIRECT)) strip_bytes(8);
#else /* SF_QUEST_DIRECT */
	for (i=0;i<8;i++) rd_byte(&p_ptr->house[i]);
#endif /* SF_QUEST_DIRECT */

	rd_byte(&p_ptr->ritual);

#ifdef SF_QUEST_DIRECT
	/* Use a constant "no ritual" value. */
	if (!has_flag(SF_QUEST_DIRECT) && p_ptr->ritual == 9)
		p_ptr->ritual = TOWN_NONE;
#endif /* SF_QUEST_DIRECT */

	rd_byte(&p_ptr->sneaking);
	rd_byte(&tmp8u);

	/* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

	/* Skip the flags */
	strip_bytes(12);


	/* Hack -- the two "special seeds" */
	rd_u32b(&seed_flavor);
	rd_u32b(&seed_wild);
	/* Now read the wilderness grid */
	for(i=0;i<12;i++)
	{
		for(j=0;j<12;j++)
		{
			rd_u32b(&tmp32u);
			wild_grid[i][j].seed=tmp32u;
			rd_byte(&tmp8u);
#ifdef SF_QUEST_DIRECT
			/* TOWN_NONE indicates no dungeon as MAX_CAVES can move. */
			if (!has_flag(SF_QUEST_DIRECT) && tmp8u >= 20) tmp8u = TOWN_NONE;
#endif /* SF_QUEST_DIRECT */
			wild_grid[i][j].dungeon=tmp8u;

			/* Fill in the town and dungeon locations if possible */
			
			if(tmp8u < MAX_CAVES)
			{
				dun_defs[tmp8u].y=i;
				dun_defs[tmp8u].x=j;
			}
			if(tmp8u < MAX_TOWNS)
			{
				town_defs[tmp8u].y=i;
				town_defs[tmp8u].x=j;
			}
			rd_byte(&tmp8u);
			wild_grid[i][j].road_map=tmp8u;
		}
	}

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

#ifdef SF_CURSE
	/* Turn on which auto-cursing will next occur */
	if (has_flag(SF_CURSE))
		rd_s32b(&curse_turn);
#endif
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
		}

		/* Warning -- backpack is full */
		else if (slot == INVEN_PACK)
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
	u16b tmp16u;

	u16b limit;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&dun_level);
	rd_s16b(&dun_offset);
	rd_u16b(&dun_bias);
	rd_byte (&cur_town);
	rd_byte (&cur_dungeon);
	rd_byte (&recall_dungeon);
	rd_byte (&came_from);
	rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&wildx);
	rd_s16b(&wildy);
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
#ifdef SF_16_CAVE_FLAG
		if (has_flag(SF_16_CAVE_FLAG))
#else
		if (FALSE)
#endif
		{
			rd_u16b(&tmp16u);
		}
		else
		{
			rd_byte(&tmp8u);
			tmp16u = tmp8u;
		}

		/* Apply the RLE info */
		for (i = count; i > 0; i--)
		{
			/* Access the cave */
			c_ptr = &cave[y][x];

			/* Extract "info" */
			c_ptr->info = tmp16u;

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
func_false();


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


	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= MAX_O_IDX)
	{
		msg_format("Too many (%d) object entries! Killing a few.", limit);
	}

	/* Read the dungeon items */
	for (i = 1; i < MIN(limit, MAX_O_IDX); i++)
	{
		
		int o_idx;

		object_type *o_ptr;


		/* Get a new record */
		o_ptr = o_pop();

		/* Note index. */
		o_idx = o_ptr - o_list;

		/* Oops */
		if (i != o_idx)
		{
			note(format("Object allocation error (%d <> %d)", i, o_idx));
			return (152);
		}


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
	/* Strip extra items from the end if necessary. */
	while (limit-- > MAX_O_IDX)
	{
		object_type dummy;
		rd_item(&dummy);
	}


	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Too many monsters. */
	if (limit >= MAX_M_IDX)
	{
		msg_format("Too many (%d) monster entries! Killing a few.", limit);
	}

	/* Read the monsters */
	for (i = 1; i < MIN(MAX_M_IDX, limit); i++)
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

		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
	}
	/* Strip extra items from the end if necessary. */
	while (limit-- > MAX_M_IDX)
	{
		monster_type dummy;
		rd_monster(&dummy);
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
	bool warn;
	quest_type *q_list_new;
	int max_q_idx_new;

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

	/* A savefile with the SFM_SPECIAL bit set has the required flags saved
	 * in a simple way. */
	if (sf_major & SFM_SPECIAL)
	{
		sf_flags = sf_minor*256+sf_patch;
	}
	else if(older_than(4,0,0))
	{
		note("Pre-v4.0.0 savefiles are no longer valid.");
		note ("");
		note("The change from a 'level-based' system to a 'skill-based'");
		note("system is too great to allow conversion of older files.");
		note("");
		note("(sorry)");
		return (1);
	}
	else if(older_than(4,0,1))
	{
		note("v4.0.0 savefiles are no longer valid.");
		note ("");
		note("Version 4.0.0 was an unfinished 'beta' release and had");
		note("serious playbalance issues.");
		note("");
		note("(sorry)");
		return (1);
	}
	/* For other save files, we must rely on the version number. */
	else
	{
		sf_flags = 0;
#ifdef SF_SKILL_BASE
		if (!older_than(4,1,0)) sf_flags |= SF_SKILL_BASE;
#endif

#ifdef SF_16_IDENT
		if (!older_than(4,1,1)) sf_flags |= SF_16_IDENT;
#endif
#ifdef SF_CURSE
		if (!older_than(4,1,1)) sf_flags |= SF_CURSE;
#endif
#ifdef SF_Q_SAVE
		if (!older_than(4,1,1)) sf_flags |= SF_Q_SAVE;
#endif
#ifdef SF_SENSE_FROM_DISCOUNT
		if (!older_than(4,1,2)) sf_flags |= SF_SENSE_FROM_DISCOUNT;
#endif
#ifdef SF_DEATHEVENTTEXT
		if (!older_than(4,1,3)) sf_flags |= SF_DEATHEVENTTEXT;
#endif
#ifdef SF_QUEST_UNKNOWN
		if (!older_than(4,1,4)) sf_flags |= SF_QUEST_UNKNOWN;
#endif
#ifdef SF_3D_WINPRI
		if (!older_than(4,1,5)) sf_flags |= SF_3D_WINPRI;
#endif
#ifdef SF_16_CAVE_FLAG
		if (!older_than(4,1,6)) sf_flags |= SF_16_CAVE_FLAG;
#endif
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


	warn = FALSE;

	/* Monster Memory */
	rd_u16b(&tmp16u);

	/* Hack - set default max_num fields for new monsters. */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = r_info+i;
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
		else r_ptr->max_num = 100;
	}

	/* Read the available records */
	for (i = 0; i < tmp16u; i++)
	{
		int j = convert_r_idx(i, sf_flags, sf_flags_now);

		/* No such monster. */
		if (j < 0 || j >= MAX_R_IDX)
		{
			monster_race dummy;
			rd_lore(&dummy);
			if (i > MON_MAX_DISTRO) warn = TRUE;
			printf("%d=%d ", i,j);
		}
		/* Read the lore */
		else
		{
			rd_lore(r_info+j);
		}
	}

	/* r_info has shrunk. */
	if (warn)
	{
		msg_format("Too many (%u) monster memories. Killing a few.", tmp16u);
	}

	if (arg_fiddle) note("Loaded Monster Memory");

	/* Death Events */
	if (has_flag(SF_DEATHEVENTTEXT)) rd_death();

	/* Object Memory */
	rd_u16b(&tmp16u);

	/* k_info has shrunk (user area only). */
	if (tmp16u > MAX_K_IDX +
		convert_k_idx(OBJ_MAX_DISTRO, sf_flags, sf_flags_now))
	{
		msg_format("Too many (%u) object kinds. Killing a few.", tmp16u);
	}

	/* Read the object memory */
	for (i = 0; i < tmp16u; i++)
	{
		int j = convert_k_idx(i, sf_flags, sf_flags_now);
		object_kind *k_ptr = (j >= 0 && j < MAX_K_IDX) ? &k_info[j] : 0;

		rd_byte(&tmp8u);

		if (k_ptr)
		{
			k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
			k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;
		}
	}

	if (arg_fiddle) note("Loaded Object Memory");


		/* Load the Quests */
		rd_u16b(&tmp16u);

#ifdef SF_QUEST_DIRECT

		/* Store the quest list from the save file. */
		C_MAKE(q_list_new, tmp16u, quest_type);

#else /* SF_QUEST_DIRECT */

		/* Incompatible save files */
		if (tmp16u > MAX_QUESTS)
		{
			note(format("Too many (%u) quests!", tmp16u));
			return (23);
		}

#endif /* SF_QUEST_DIRECT */

		/* Set the number of quests globally. */
		max_q_idx_new = tmp16u;

		/* Load the Quests */
		for (i = 0; i < max_q_idx_new; i++)
		{
			quest_type *q_ptr = q_list_new+i;

			rd_byte(&tmp8u);
			q_ptr->level = tmp8u;
			rd_s16b((short *)&tmp16u);
			q_ptr->r_idx = convert_r_idx(tmp16u, sf_flags, sf_flags_now);
			rd_byte(&q_ptr->dungeon);
			rd_byte(&tmp8u);
			q_ptr->cur_num = tmp8u;
			rd_byte(&tmp8u);
			q_ptr->max_num = tmp8u;
#ifdef SF_QUEST_DIRECT
			/* Check that the quest is inside the dungeon. */
			if (q_ptr->dungeon >= MAX_CAVES)
			{
				msg_print("Removing quest in vanished dungeon.");
				q_ptr->level = q_ptr->max_num = 0;
			}
			else if (q_ptr->level > dun_defs[q_ptr->dungeon].max_level)
			{
				msg_print("Removing quest on impossible level.");
				q_ptr->level = q_ptr->max_num = 0;
			}
#endif /* SF_QUEST_DIRECT */
#ifdef SF_QUEST_UNKNOWN
			if (has_flag(SF_QUEST_UNKNOWN))
			{
				rd_byte(&tmp8u);
				q_ptr->cur_num_known = tmp8u;
			}
			else
			{
				q_ptr->cur_num_known = 0;
			}
#endif
#ifdef SF_QUEST_KNOWN
			if (has_flag(SF_QUEST_KNOWN))
			{
				rd_byte(&tmp8u);
				q_ptr->known = tmp8u;
			}
			else
			{
				/* The default set of known quests is the set of fixed quests. */
				q_ptr->known = FALSE;
#ifndef SF_QUEST_DIRECT /* The game doesn't track fixed quests. */
				for (j = 0; j < MAX_CAVES; j++)
				{
					dun_type *d_ptr = dun_defs+j;

					/* Wrong dungeon. */
					if (q_ptr->dungeon != j) continue;

					/* Wrong level. */
					
					if (d_ptr->first_level != q_ptr->level &&
						d_ptr->second_level != q_ptr->level) continue;

					/* Everyone knows this quest. */
					q_ptr->known = TRUE;

					/* Found it. */
					break;
				}
#endif /* SF_QUEST_DIRECT */
			}
#endif /* SF_QUEST_KNOWN */
		}


	if (arg_fiddle) note("Loaded Quests");


	/* Load the Artifacts */
	rd_u16b(&tmp16u);

	/* a_info has shrunk. */
	if (tmp16u > MAX_A_IDX)
	{
		msg_format("Too many (%u) artifact. Killing a few.", tmp16u);
	}

	/* Read the artifact flags */
	for (i = 0; i < MIN(MAX_A_IDX, tmp16u); i++)
	{
		rd_byte(&tmp8u);
		a_info[i].cur_num = tmp8u;
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
		rd_byte(&tmp8u);
	}
	/* Forget vanished artefacts. */
	while (tmp16u-- > MAX_A_IDX) strip_bytes(4);

	if (arg_fiddle) note("Loaded Artifacts");


	/* Read the extra stuff */
	rd_extra();
	if (arg_fiddle) note("Loaded extra information");


	/* Read the player_hp array */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
	if (tmp16u > 100)
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

	/* Important -- Initialize the race/template */
	rp_ptr = &race_info[p_ptr->prace];
	cp_ptr = &template_info[p_ptr->ptemplate];


	/* Read spell info */
	for (i=0;i<MAX_SCHOOL;i++)
	{
		rd_u32b(&spell_learned[i]);
		rd_u32b(&spell_worked[i]);
		rd_u32b(&spell_forgotten[i]);
	}

	for (i = 0; i < 128; i++)
	{
		rd_byte(&spell_order[i]);
	}

	/* Read spirit info */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		char temp[20];

		rd_u16b(&tmp16u);
		rd_u32b(&tmp32u);
		rd_string(temp, 20);

		/*
		 * Ignore for dead characters as it does nothing.
		 * This should be changed next time the version number
		 * changes so that dead characters' save files don't store
		 * this useless information, but it does no real harm.
		 */
		if (death) continue;

		spirits[i].pact = tmp16u;
		spirits[i].annoyance = tmp32u;
		strcpy(spirits[i].name, temp);
	}

	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}


	/* Read the stores */
	rd_u16b(&tmp16u);

	/* Too many stores, so delete the first few. */
	if (tmp16u > MAX_STORES_TOTAL)
	{
		msg_format("Deleting %u unrecognised stores", tmp16u - MAX_STORES_TOTAL);
		for (i = 0; i < tmp16u - MAX_STORES_TOTAL; i++) rd_store(0);
	}
	
	for (i = 0; i < MIN(MAX_STORES_TOTAL, tmp16u); i++)
	{
		if (rd_store(i)) return (22);
	}
	
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

	/* New characters use the base quest list. */
	if (death)
	{
		FREE(q_list_new);
	}
	/* Existing ones use the quest list from the save file. */
	else
	{
		FREE(q_list);
		q_list = q_list_new;
		MAX_Q_IDX = max_q_idx_new;
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
static errr rd_savefile_new(void)
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



/*
 * Attempt to Load a "savefile"
 *
 * Version 2.7.0 introduced a slightly different "savefile" format from
 * older versions, requiring a completely different parsing method.
 *
 * Note that savefiles from 2.7.0 - 2.7.2 are completely obsolete.
 *
 * Pre-2.8.0 savefiles lose some data, see "load2.c" for info.
 *
 * Pre-2.7.0 savefiles lose a lot of things, see "load1.c" for info.
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
	int             fd = -1;

	errr    err = 0;

	byte    vvv[4];

#ifdef VERIFY_TIMESTAMP
	struct stat     statbuf;
#endif

	cptr    what = "generic";


	/* Paranoia */
	turn = 0;

	/* Paranoia */
	death = FALSE;


	/* Allow empty savefile name */
	if (!savefile[0]) return (TRUE);


#if !defined(MACINTOSH) && !defined(WINDOWS) && !defined(VM)

	/* XXX XXX XXX Fix this */

	/* Verify the existance of the savefile */
	if (access(savefile, 0) < 0)
	{
		/* Give a message */
		msg_print("Savefile does not exist.");
		msg_print(NULL);

		/* Allow this */
		return (TRUE);
	}

#endif


#ifdef VERIFY_SAVEFILE

	/* Verify savefile usage */
	if (!err)
	{
		FILE *fkk;

		char temp[1024];

		/* Extract name of lock file */
		strcpy(temp, savefile);
		strcat(temp, ".lok");

		/* Check for lock */
		fkk = my_fopen(temp, "r");

		/* Oops, lock exists */
		if (fkk)
		{
			/* Close the file */
			my_fclose(fkk);

			/* Message */
			msg_print("Savefile is currently in use.");
			msg_print(NULL);

			/* Oops */
			return (FALSE);
		}

		/* Create a lock file */
		fkk = my_fopen(temp, "w");

		/* Dump a line of info */
		fprintf(fkk, "Lock file for savefile '%s'\n", savefile);

		/* Close the lock file */
		my_fclose(fkk);
	}

#endif


	/* Okay */
	if (!err)
	{
		/* Open the savefile */
		fd = fd_open(savefile, O_RDONLY);

		/* No file */
		if (fd < 0) err = -1;

		/* Message (below) */
		if (err) what = "Cannot open savefile";
	}

	/* Process file */
	if (!err)
	{

#ifdef VERIFY_TIMESTAMP
		/* Get the timestamp */
		(void)fstat(fd, &statbuf);
#endif

		/* Read the first four bytes */
		if (fd_read(fd, (char*)(vvv), 4)) err = -1;

		/* What */
		if (err) what = "Cannot read savefile";

		/* Close the file */
		(void)fd_close(fd);
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

		/* Attempt to load */
		err = rd_savefile_new();
		/* Message (below) */
		if (err) what = "Cannot parse savefile";
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
	if (!err)
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
#endif


	/* Okay */
	if (!err)
	{
		u16b cur_flags;
		byte cur[3];
		current_version(&cur_flags, cur, cur+1, cur+2);
		/* Give a conversion warning */
        if ((cur[0] != sf_major) ||
            (cur[1] != sf_minor) ||
            (cur[2] != sf_patch))
		{
			/* Message */
            msg_format("Converted a %d.%d.%d savefile.",
                       sf_major, sf_minor, sf_patch);
			msg_print(NULL);
		}

		/* Player is dead */
		if (death)
		{
			/* Player is no longer "dead" */
			death = FALSE;

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
			/* Froget the cause of death from the save file. */
			FREE(died_from);

			/* Reset cause of death */
			died_from = "(alive and well)";

			/* Accept the quest list. */
			
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

		/* Remove lock */
		fd_kill(temp);
	}

#endif


	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
		   what, sf_major, sf_minor, sf_patch);
	msg_print(NULL);

	/* Oops */
	return (FALSE);
}


