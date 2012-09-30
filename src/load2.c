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


#if 0 /* DG -- Not used anymore  */
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
#endif

/*
 * The above function, adapted for PernAngband
 */
static bool p_older_than(byte x, byte y, byte z)
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
                case TV_WAND:
                case TV_STAFF:
                case TV_ROD:
                case TV_ROD_MAIN:
                case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
                case TV_BOOMERANG:
		case TV_BOW:
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
                case TV_MSTAFF:
		case TV_SWORD:
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
                case TV_HYPNOS:
                case TV_INSTRUMENT:
                case TV_DAEMON_BOOK:
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

void rd_s16b(s16b *ip)
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

        u32b f1, f2, f3, f4, esp;

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
        rd_s32b(&o_ptr->pval);

        /* Special pval */
        rd_s16b(&o_ptr->pval2);

        /* Special pval */
        if (!p_older_than(4, 2, 2))
        {
                rd_s32b(&o_ptr->pval3);
        }
        else
        {
                s16b tmp;

                rd_s16b(&tmp);
                o_ptr->pval3 = tmp;
        }

        rd_byte(&o_ptr->discount);
        rd_byte(&o_ptr->number);
        rd_s32b(&o_ptr->weight);

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
	rd_u32b(&o_ptr->art_flags1);
	rd_u32b(&o_ptr->art_flags2);
	rd_u32b(&o_ptr->art_flags3);
        rd_u32b(&o_ptr->art_flags4);

        /* Monster holding object */
        rd_s16b(&o_ptr->held_m_idx);

	/* Special powers */
	rd_byte(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &esp);

        if (!p_older_than(4,0,3))
        {
                rd_byte(&o_ptr->elevel);
                rd_s32b(&o_ptr->exp);
        }
        else
        {
                if(f4 & TR4_LEVELS)
                {
                        k_ptr = &k_info[o_ptr->k_idx];

                        o_ptr->elevel = (k_ptr->level / 10) + 1;
                        o_ptr->exp = player_exp[o_ptr->elevel - 1];
                }
        }

        /* Read the pseudo-id */
        if (!p_older_than(4,1,2))
        {
                rd_byte(&o_ptr->sense);
        }
        else
        {
                o_ptr->sense = SENSE_NONE;
        }

	/* Inscription */
	rd_string(buf, 128);

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	rd_string(buf, 128);
	if (buf[0]) o_ptr->art_name = quark_add(buf);

	/* Mega-Hack -- handle "dungeon objects" later */
	if ((o_ptr->k_idx >= 445) && (o_ptr->k_idx <= 479)) return;


	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Obtain tval/sval from k_info */
	o_ptr->tval = k_ptr->tval;
        if(o_ptr->tval != TV_RANDART) o_ptr->sval = k_ptr->sval;


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
                if((o_ptr->tval != TV_CORPSE)&&(o_ptr->tval != TV_EGG)) o_ptr->weight = k_ptr->weight;

		/* Paranoia */
		o_ptr->name1 = o_ptr->name2 = 0;

		/* All done */
		return;
	}


	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &esp);

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


	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->name1];

		/* Acquire new artifact fields */
		o_ptr->ac = a_ptr->ac;
		o_ptr->dd = a_ptr->dd;
		o_ptr->ds = a_ptr->ds;

		/* Acquire new artifact weight */
		o_ptr->weight = a_ptr->weight;
	}

	/* Ego items */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->name2];


		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
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
	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);

        if (!p_older_than(4, 1, 5))
        {
                rd_u16b(&m_ptr->ego);
        }
        else
        {
                m_ptr->ego = 0;
        }

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
        rd_byte(&m_ptr->imprinted);
        if (!p_older_than(4, 1, 7))
                rd_s16b(&m_ptr->possessor);
        else
                m_ptr->possessor = 0;
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
        rd_u32b(&r_ptr->r_flags7);
        rd_u32b(&r_ptr->r_flags8);
        rd_u32b(&r_ptr->r_flags9);

        /* Read the "Racial" monster limit per level */
        rd_s16b(&r_ptr->max_num);

        if(!p_older_than(4, 0, 9))
        {
                rd_byte(&r_ptr->on_saved);
        }

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

        /* Read last visit */
        rd_s32b(&st_ptr->last_visit);

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
        int i;

        if (p_older_than(4, 1, 1))
        {
                /* Strip name */
                rd_string(buf, 64);

                /* Strip old data */
                strip_bytes(60);
        }
        else
        {
                monster_race *r_ptr = &r_info[max_r_idx - 1];

		/* Name */
		rd_string(r_name + r_ptr->name, 64);

		/* Visuals */
		rd_byte(&r_ptr->d_char);
		rd_byte(&r_ptr->d_attr);

		/* Level/Rarity */
		rd_byte(&r_ptr->level);
		rd_byte(&r_ptr->rarity);

		/* Misc info */
		rd_byte(&r_ptr->hdice);
		rd_byte(&r_ptr->hside);
		rd_s16b(&r_ptr->ac);
		rd_s16b(&r_ptr->sleep);
		rd_byte(&r_ptr->aaf);
		rd_byte(&r_ptr->speed);

		/* Experience */
		rd_s32b(&r_ptr->mexp);

		/* Frequency */
		rd_byte(&r_ptr->freq_inate);
		rd_byte(&r_ptr->freq_spell);

		/* Flags */
		rd_u32b(&r_ptr->flags1);
		rd_u32b(&r_ptr->flags2);
		rd_u32b(&r_ptr->flags3);
		rd_u32b(&r_ptr->flags4);
		rd_u32b(&r_ptr->flags5);
		rd_u32b(&r_ptr->flags6);
                rd_u32b(&r_ptr->flags7);
                rd_u32b(&r_ptr->flags8);
                rd_u32b(&r_ptr->flags9);

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
}

/* Load the random spells info */
static void rd_spells(int i)
{
        random_spell *s_ptr = &random_spells[i];
        rd_string(s_ptr->name, 30);
        rd_string(s_ptr->desc, 30);
        rd_s16b(&s_ptr->mana);
        rd_s16b(&s_ptr->fail);
        rd_u32b(&s_ptr->proj_flags);
        rd_byte(&s_ptr->GF);
        rd_byte(&s_ptr->radius);
        rd_byte(&s_ptr->dam_sides);
        rd_byte(&s_ptr->dam_dice);
        rd_byte(&s_ptr->level);
        rd_byte(&s_ptr->untried);
}

/*
 * Read the "extra" information
 */
static void rd_extra(void)
{
        int i, j;

	byte tmp8u;
	s16b tmp16s;

	rd_string(player_name, 32);

	rd_string(died_from, 80);

	for (i = 0; i < 4; i++)
	{
		rd_string(history[i], 60);
	}

        /* Read the special level flags */

        rd_byte(&tmp8u);
        if (tmp8u > max_d_idx)
        {
                note(format("Too many (%d) dungeon types!", tmp8u));
        }

        rd_s16b(&tmp16s);
        if (tmp16s > MAX_DUNGEON_DEPTH)
        {
                note(format("Too many (%d) max level by dungeon type!", tmp16s));
        }

        for (i = 0; i < tmp8u; i++)
                for (j = 0; j < tmp16s; j++)
                        rd_byte(&spec_history[j][i]);

	/* Class/Race/Gender/Spells */
	rd_byte(&p_ptr->prace);

        if (!p_older_than(4, 1, 8))
        {
                rd_byte(&p_ptr->pracem);
        }
        else
        {
                p_ptr->pracem = 0;

                /* Convert the old barbarian */
                if (p_ptr->prace == 10)
                {
                        p_ptr->prace = RACE_HUMAN;
                        p_ptr->pracem = RMOD_BARBARIAN;
                }
                /* Convert the old vampire */
                else if (p_ptr->prace == 16)
                {
                        p_ptr->prace = RACE_HUMAN;
                        p_ptr->pracem = RMOD_VAMPIRE;
                }
                /* Convert the old spectre */
                else if (p_ptr->prace == 17)
                {
                        p_ptr->prace = RACE_HUMAN;
                        p_ptr->pracem = RMOD_SPECTRE;
                }
                /* Convert the the races to match their new position */
                else
                {
                        if (p_ptr->prace > 10) p_ptr->prace--;
                        if (p_ptr->prace > 17) p_ptr->prace -= 2;
                }
        }
        rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
        rd_u16b(&p_ptr->realm1);
        rd_u16b(&p_ptr->realm2);
        rd_byte(&p_ptr->mimic_form);
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
        if(!p_older_than(4,0,2))
        {
                for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cnt[i]);
                for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_los[i]);
        }
        else
        {
                for (i = 0; i < 6; ++i) p_ptr->stat_cnt[i] = 0;
                for (i = 0; i < 6; ++i) p_ptr->stat_los[i] = 0;
        }

	strip_bytes(24);        /* oops */

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

        rd_s16b(&p_ptr->town_num);

        /* Read arena and rewards information */
        rd_s16b(&p_ptr->arena_number);
        rd_s16b(&p_ptr->inside_arena);
        rd_s16b(&p_ptr->inside_quest);
        rd_byte(&p_ptr->exit_bldg);
        rd_byte(&tmp8u);

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
        if (!p_older_than(4, 1, 6))
        {
                rd_s16b(&p_ptr->hp_mod);
        }
        else
        {
                p_ptr->hp_mod = 0;
        }

        rd_s16b(&p_ptr->msane);
        rd_s16b(&p_ptr->csane);
        rd_u16b(&p_ptr->csane_frac);

	rd_s16b(&p_ptr->msp);
	rd_s16b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

        rd_s16b(&p_ptr->mtp);
        rd_s16b(&p_ptr->ctp);
        rd_s16b(&p_ptr->tp_aux1);
        rd_s16b(&p_ptr->tp_aux2);

        /* Gods */
        rd_s32b(&p_ptr->grace);
        rd_s32b(&p_ptr->god_favor);
        rd_byte(&p_ptr->pgod);

	rd_s16b(&p_ptr->max_plv);
        {
                byte max = max_d_idx;

                rd_byte(&max);

                for(i = 0; i < max; i++)
                        rd_s16b(&max_dlv[i]);
        }

	/* Repair maximum player level XXX XXX XXX */
	if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;

	/* More info */
	strip_bytes(8);
	rd_s16b(&p_ptr->sc);
	strip_bytes(2);

	/* Read the flags */
	strip_bytes(2);
	rd_s16b(&p_ptr->blind);
	rd_s16b(&p_ptr->paralyzed);
	rd_s16b(&p_ptr->confused);
	rd_s16b(&p_ptr->food);
	strip_bytes(4); /* Old "food_digested" / "protection" */
        if(!p_older_than(4,0,5))
        {
                rd_s32b(&p_ptr->energy);
        }else{
                s16b e;
                rd_s16b(&e);
                p_ptr->energy = e;
        }
        rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
        rd_s16b(&p_ptr->protundead);
	rd_s16b(&p_ptr->invuln);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->shero);
	rd_s16b(&p_ptr->shield);
        rd_s16b(&p_ptr->shield_power);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->word_recall);
        rd_s16b(&p_ptr->recall_dungeon);
        rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->oppose_fire);
	rd_s16b(&p_ptr->oppose_cold);
	rd_s16b(&p_ptr->oppose_acid);
	rd_s16b(&p_ptr->oppose_elec);
	rd_s16b(&p_ptr->oppose_pois);
        rd_s16b(&p_ptr->oppose_ld);
        rd_s16b(&p_ptr->oppose_cc);
        rd_s16b(&p_ptr->oppose_ss);
        rd_s16b(&p_ptr->oppose_nex);

        rd_s16b(&p_ptr->tim_esp);
        rd_s16b(&p_ptr->wraith_form);
        rd_s16b(&p_ptr->tim_ffall);
        rd_s16b(&p_ptr->tim_fire_aura);
        rd_s16b(&p_ptr->resist_magic);
        rd_s16b(&p_ptr->tim_invisible);
        rd_s16b(&p_ptr->tim_inv_pow);
        rd_s16b(&p_ptr->tim_mimic);
        rd_s16b(&p_ptr->lightspeed);
        rd_s16b(&p_ptr->tim_lite);
        rd_s16b(&p_ptr->holy);
        rd_s16b(&p_ptr->walk_water);
        rd_s16b(&p_ptr->tim_mental_barrier);
        rd_s16b(&p_ptr->strike);
        rd_s16b(&p_ptr->meditation);
        rd_s16b(&p_ptr->tim_reflect);
        rd_s16b(&p_ptr->tim_res_time);

        rd_s16b(&p_ptr->immov_cntr);

        rd_s16b(&p_ptr->chaos_patron);
        rd_u32b(&p_ptr->muta1);
        rd_u32b(&p_ptr->muta2);
        rd_u32b(&p_ptr->muta3);

	rd_byte(&p_ptr->confusing);
        rd_byte(&p_ptr->black_breath);  /* Status of Black Breath. */
	rd_byte(&tmp8u);        /* oops */
        rd_byte(&fate_flag);
	rd_byte(&p_ptr->searching);
	rd_byte(&p_ptr->maximize);
	rd_byte(&p_ptr->preserve);
	rd_byte(&p_ptr->special);
	rd_byte(&special_flag);
        rd_byte(&p_ptr->allow_one_death);
        rd_s16b(&p_ptr->xtra_spells);


        rd_byte(&vanilla_town);

        rd_u16b(&no_breeds);

	rd_s16b(&p_ptr->protgood);
	if(p_older_than(4,1,0)) p_ptr->protgood=0;
	
        /* Future use */
	for (i = 0; i < 46; i++) rd_byte(&tmp8u);

        /* Aux variables */
        rd_u32b(&p_ptr->class_extra1);
        rd_u32b(&p_ptr->class_extra2);
        rd_u32b(&p_ptr->class_extra3);
        rd_u32b(&p_ptr->class_extra4);
        rd_u32b(&p_ptr->class_extra5);
        rd_u32b(&p_ptr->class_extra6);
        rd_u32b(&p_ptr->class_extra7);

        rd_u32b(&p_ptr->race_extra1);
        rd_u32b(&p_ptr->race_extra2);
        rd_u32b(&p_ptr->race_extra3);
        rd_u32b(&p_ptr->race_extra4);
        rd_u32b(&p_ptr->race_extra5);
        rd_u32b(&p_ptr->race_extra6);
        rd_u32b(&p_ptr->race_extra7);

        /* Read the incarnation things */
        rd_u16b(&p_ptr->body_monster);
        rd_byte(&p_ptr->disembodied);

        /* Astral being */
        if (!p_older_than(4, 1, 9))
        {
                rd_byte(&p_ptr->astral);
        }
        else
        {
                p_ptr->astral = FALSE;
        }

        /* Read the music */
        rd_byte(&p_ptr->music);

        /* Read the tactic */
        rd_byte(&p_ptr->tactic);

        /* Read the movement */
        rd_byte(&p_ptr->movement);

        /* Read the fate */
        rd_byte(&p_ptr->no_mortal);

        for (i = 0; i < MAX_BOUNTIES; i++)
        {
                rd_s16b(&bounties[i][0]);
                rd_s16b(&bounties[i][1]);
        }

        rd_u32b(&total_bounties);

        rd_s16b(&spell_num);
        for (i = 0; i < MAX_SPELLS; i++)
        {
                rd_spells(i);
        }

        if (!p_older_than(4, 2, 0))
        {
                rd_s16b(&rune_num);
                for (i = 0; i < MAX_RUNES; i++)
                {
                        rd_string(rune_spells[i].name, 30);
                        rd_s16b(&(rune_spells[i].type));
                        rd_s16b(&(rune_spells[i].rune2));
                        rd_s16b(&(rune_spells[i].mana));
                }
        }
        else
        {
                rune_num = 0;
                for (i = 0; i < MAX_RUNES; i++)
                {
                        strcpy(rune_spells[i].name, "");
                        rune_spells[i].type = 0;
                        rune_spells[i].rune2 = 0;
                        rune_spells[i].mana = 0;
                }
        }

	/* Skip the flags */
	strip_bytes(12);


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

	int xstart = 0;
	int ystart = 0;

	byte count;
	byte tmp8u;
	s16b tmp16s;

	u16b limit;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Header info */
	rd_s16b(&dun_level);
        rd_byte(&dungeon_type);
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
		process_dungeon_file("w_info.txt", &ystart, &xstart, cur_hgt, cur_wid);

		/* Init the town */
		xstart = 0;
		ystart = 0;
		init_flags = 0;
		process_dungeon_file("t_info.txt", &ystart, &xstart, cur_hgt, cur_wid);
	}


	/* Maximal size */
	ymax = cur_hgt;
	xmax = cur_wid;


        if(!p_older_than(4,0,2))
        {
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
        }
        else
        {
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

			/* Extract "info" */
			c_ptr->info = tmp8u;

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

        if (!p_older_than(4, 1, 3))
        {
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
                                c_ptr->special2 = tmp16s;

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
         }

                if(!p_older_than(4,0,2))
                {
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

                                /* Extract "inscription" */
                                c_ptr->inscription = tmp16s;

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
                              c_ptr->mana = tmp8u;

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

		/* Count XXX XXX XXX */
		r_ptr->cur_num++;
	}

	/*** Success ***/

	/* The dungeon is ready */
        character_dungeon = TRUE;

	/* Success */
	return (0);
}

/* Returns TRUE if we successfully load the dungeon */
bool load_dungeon(char *ext)
{
        char tmp[16];
        char name[1024];
        byte old_dungeon_type = dungeon_type;
        s16b old_dun = dun_level;
  
        /* Construct name */
        sprintf(tmp, "%s.%s", player_base, ext);
        path_build(name, 1024, ANGBAND_DIR_SAVE, tmp);

        /* Open the file */
        fff = my_fopen(name, "rb");
        if (fff == NULL)
        {
                dun_level = old_dun;
                dungeon_type = old_dungeon_type;

                my_fclose(fff);
                return(FALSE);
        }

        /* Read the dungeon */
        if (rd_dungeon())
        {
                dun_level = old_dun;
                dungeon_type = old_dungeon_type;

                my_fclose(fff);
                return(FALSE);
        }

        dun_level = old_dun;
        dungeon_type = old_dungeon_type;

        /* Done */
        my_fclose(fff);
        return(TRUE);
}

void rd_fate(int i)
{
        if (i >= MAX_FATES) i = MAX_FATES - 1;

        rd_byte(&fates[i].fate);
        rd_byte(&fates[i].level);
        rd_byte(&fates[i].serious);
        rd_s16b(&fates[i].o_idx);
        rd_s16b(&fates[i].e_idx);
        rd_s16b(&fates[i].a_idx);
        rd_s16b(&fates[i].v_idx);
        rd_s16b(&fates[i].r_idx);
        rd_s16b(&fates[i].count);
        rd_s16b(&fates[i].time);
        rd_byte(&fates[i].know);
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

        byte tmp8b;
        s16b tmp16b;
        s32b tmp32b;

        char dummy[400];

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
	}

	if (arg_fiddle) note("Loaded Monster Memory");

        /* Old hackish, buggish ghost code */
        if (p_older_than(4, 1, 0))
        {
                /* Init the ghost & player monsters */
                for(i = 0; i < MAX_GHOSTS; i++)
                        rd_string(dummy, 20);

                for(i = 0; i < MAX_GHOSTS; i++)
                {
                        rd_string(dummy, 80);
                        rd_string(dummy, 320);

                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_s16b(&tmp16b);

                        rd_s16b(&tmp16b);
                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_s32b(&tmp32b);
                        rd_s32b(&tmp32b);

                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);
                        rd_u32b(&tmp32b);

                        for(j = 0; j < 4; j++)
                        {
                                rd_byte(&tmp8b);
                                rd_byte(&tmp8b);
                                rd_byte(&tmp8b);
                                rd_byte(&tmp8b);
                        }

                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_byte(&tmp8b);
                        rd_byte(&tmp8b);

                        rd_s16b(&tmp16b);
                        rd_byte(&tmp8b);
                }
        }

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
                k_ptr->know  = (tmp8u & 0x04) ? TRUE: FALSE;
                if (tmp8u & 0x08) k_ptr->squeltch = 1;
                if (tmp8u & 0x10) k_ptr->squeltch = 2;
                if (tmp8u & 0x20) k_ptr->squeltch = 3;
                if (tmp8u & 0x40) k_ptr->squeltch = 4;
                k_ptr->artifact = (tmp8u & 0x80) ? TRUE: FALSE;
	}
	if (arg_fiddle) note("Loaded Object Memory");

	/*
	 * Initialize arena and rewards information
	 */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->inside_quest = 0;
	p_ptr->exit_bldg = TRUE;

	/* Start in town 1 */
	p_ptr->town_num = 1;

	p_ptr->wilderness_x = 4;
	p_ptr->wilderness_y = 4;

	/* Init the wilderness seeds */
	for (i = 0; i < max_wild_x; i++)
	{
		for (j = 0; j < max_wild_y; j++)
		{
                        wild_map[j][i].seed = rand_int(0x10000000);
		}
	}

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

                if (!p_older_than(4, 2, 1))
                {
                        /* Min of random towns */
                        rd_u16b(&max_towns_load);

                        /* Incompatible save files */
                        if (max_towns_load != TOWN_RANDOM)
                        {
                                note(format("Different random towns base (%u)!", max_towns_load));
                                return (23);
                        }

                        for (i = TOWN_RANDOM; i < max_towns; i++)
                        {
                                rd_u32b(&town[i].seed);
                                rd_byte(&town[i].numstores);
                                rd_byte(&town[i].real);
                        }

                        /* Number of dungeon */
                        rd_u16b(&max_towns_load);

                        /* Incompatible save files */
                        if (max_towns_load > max_d_idx)
                        {
                                note(format("Too many dungeon types (%u)!", max_towns_load));
                                return (23);
                        }

                        /* Number of towns per dungeon */
                        rd_u16b(&max_quests_load);

                        /* Incompatible save files */
                        if (max_quests_load > max_d_idx)
                        {
                                note(format("Too many town per dungeons (%u)!", max_quests_load));
                                return (23);
                        }

                        for (i = 0; i < max_towns_load; i++)
                        {
                                for (j = 0; j < max_quests_load; j++)
                                {
                                        rd_s16b(&(d_info[i].t_idx[j]));
                                        rd_s16b(&(d_info[i].t_level[j]));
                                }
                                rd_s16b(&(d_info[i].t_num));
                        }
                }
                else
                {
                        for (i = 0; i < max_d_idx; i++)
                        {
                                for (j = 0; j < TOWN_DUNGEON; j++)
                                {
                                        d_info[i].t_idx[j] = 0;
                                        d_info[i].t_level[j] = 0;
                                }
                                d_info[i].t_num = 0;
                        }
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
			rd_s16b(&quest[i].status);

                        rd_s16b(&quest[i].level);

			/* Load quest status if quest is running */
			if (quest[i].status == 1)
			{
                                rd_s16b(&quest[i].k_idx);
                                if (quest[i].k_idx) a_info[quest[i].k_idx].flags3 |= TR3_QUESTITEM;
				rd_s16b(&quest[i].cur_num);
				rd_s16b(&quest[i].max_num);
				rd_s16b(&quest[i].type);

				rd_s16b(&quest[i].r_idx);

				/* Mark uniques */
				if (r_info[quest[i].r_idx].flags1 & RF1_UNIQUE)
						r_info[quest[i].r_idx].flags1 |= RF1_QUESTOR;
			}
		}

		/* Position in the wilderness */
		rd_s32b(&p_ptr->wilderness_x);
		rd_s32b(&p_ptr->wilderness_y);
                if(!p_older_than(4,0,4))
                {
                        rd_byte(&p_ptr->wild_mode);
                }else{
                        p_ptr->wild_mode = FALSE;
                }

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
                                rd_u32b(&wild_map[j][i].seed);

                                if(!p_older_than(4,0,7))
                                {
                                        rd_u16b(&tmp16u);
                                        wild_map[j][i].entrance = tmp16u;
                                }

                                if(!p_older_than(4,1,4))
                                {
                                        rd_byte(&wild_map[j][i].known);
                                }
			}
		}
        }

	if (arg_fiddle) note("Loaded Quests");

        /* Load the random artifacts. */
	rd_u16b(&tmp16u);
        if (tmp16u > MAX_RANDARTS)
        {
                note(format("Too many (%u) random artifacts!", tmp16u));
                return 23;
	}

	for (i = 0; i < tmp16u; i++) {
                random_artifact* ra_ptr = &random_artifacts[i];

                rd_string(ra_ptr->name_full, 80);
                rd_string(ra_ptr->name_short, 80);
                rd_byte(&ra_ptr->level);
                rd_byte(&ra_ptr->attr);
                rd_u32b(&ra_ptr->cost);
                rd_byte(&ra_ptr->activation);
                rd_byte(&ra_ptr->generated);
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

        /* Load the Fates */
        rd_u16b(&tmp16u);

        /* Incompatible save files */
        if (tmp16u > MAX_FATES)
        {
                note(format("Too many (%u) fates!", tmp16u));
                return (24);
        }

        /* Read the fate flags */
        for (i = 0; i < tmp16u; i++)
        {
                rd_fate(i);
        }
        if (arg_fiddle) note("Loaded Fates");

        if(!p_older_than(4,0,2))
        {
        /* Load the Traps */
        rd_u16b(&tmp16u);

        /* Incompatible save files */
        if (tmp16u > max_t_idx)
        {
                note(format("Too many (%u) traps!", tmp16u));
                return (24);
        }

        /* Read the fate flags */
        for (i = 0; i < tmp16u; i++)
        {
                rd_byte(&t_info[i].ident);
        }
        if (arg_fiddle) note("Loaded Traps");
        }

        /* Load the inscription knowledge */
        rd_u16b(&tmp16u);

        /* Incompatible save files */
        if (tmp16u > MAX_INSCRIPTIONS)
        {
                note(format("Too many (%u) inscriptions!", tmp16u));
                return (24);
        }

        /* Read the inscription flag */
        for (i = 0; i < tmp16u; i++)
        {
                rd_byte(&inscription_info[i].know);
        }
        if (arg_fiddle) note("Loaded Inscriptions");


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
        rmp_ptr = &race_mod_info[p_ptr->pracem];
	cp_ptr = &class_info[p_ptr->pclass];

	/* Important -- Initialize the magic */
	mp_ptr = &magic_info[p_ptr->pclass];


	/* Read spell info */
	rd_u16b(&tmp16u);

	/* Incompatible save files */
        if (tmp16u > MAX_REALM)
	{
                note(format("Too many (%u) realm entries!", tmp16u));
		return (25);
	}

	/* Read the player_hp array */
	for (i = 0; i < tmp16u; i++)
        {
                rd_u32b(&spell_learned[i][0]);
                rd_u32b(&spell_learned[i][1]);
                rd_u32b(&spell_worked[i][0]);
                rd_u32b(&spell_worked[i][1]);
                rd_u32b(&spell_forgotten[i][0]);
                rd_u32b(&spell_forgotten[i][1]);
        }

	for (i = 0; i < 64; i++)
	{
                rd_byte(&realm_order[i]);
		rd_byte(&spell_order[i]);
	}

	/* Read the pet command settings */
        rd_byte(&p_ptr->pet_follow_distance);
        rd_byte(&p_ptr->pet_open_doors);
        rd_byte(&p_ptr->pet_pickup_items);

	/* Read the inventory */
	if (rd_inventory())
	{
		note("Unable to read inventory");
		return (21);
	}

	/* Read number of towns */
        rd_u16b(&tmp16u);
        town_count = tmp16u;

	/* Read the stores */
	rd_u16b(&tmp16u);
	for (i = 1; i < town_count; i++)
	{
		for (j = 0; j < tmp16u; j++)
		{
			if (rd_store(i, j)) return (22);
		}
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


        /* Hack -- ghosts */
        r_info[max_r_idx - 1].max_num = 1;


	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;
#ifdef SAFER_PANICS
	char panic_fname[1024];

        if (!panicload)
        {
#endif /* SAFER_PANICS */
                /* The savefile is a binary file */
                fff = my_fopen(savefile, "rb");
#ifdef SAFER_PANICS
	}
        if (panicload)
        {
		strcpy(panic_fname, savefile);
                strcat(panic_fname, ".pnc");
		fff = my_fopen(panic_fname, "rb");
        }
#endif /* SAFER_PANICS */

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


