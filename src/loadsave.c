/* File: loadsave.c */

/*
 * Savefile management.  Save, load, convert, extract information from, and
 * print error messages about savefiles.  In-game tools to manage savefiles.
 *
 * Copyright (c) 2007 Ben Harrison, DarkGod and Improv, and others
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"
#include "init.h"


/*
 * (Sangband 1.0.0-specific notes)
 * This file handles savefiles from Sangband 0.9.9 and later.  As in
 * ToME, the saving and loading code has been unified in order to
 * make it easier to keep savefiles up to date.  When needing to handle
 * savefiles from previous versions, use the "older_than()" function to
 * control any changes.
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
 * The unified saving and loading code has no explicit safeguards against
 * data being changed during the writing of a savefile.  At present, the
 * low-level code is extremely simple, and will not fail when used cor-
 * rectly.  If new features are added, like compression or new methods of
 * encryption, care must be taken to avoid this danger.
 */



/*
 * Some "local" parameters, used to help write savefiles
 */


/*
 * Assume we're saving, not loading a file.  Setting this variable to
 * TRUE allows data to be changed.
 */
static bool load_file = FALSE;


/*
 * Local "savefile" pointer
 */
static FILE *fff;

/*
 * Hack -- old "encryption" byte  */
static byte xor_byte;


static u32b v_check = 0L;    /* A simple "checksum" on the actual values */
static u32b x_check = 0L;    /* A simple "checksum" on the encoded bytes */
static u32b v_stamp = 0L;    /* A simple "checksum" on the actual values */
static u32b x_stamp = 0L;    /* A simple "checksum" on the encoded bytes */


/*
 * Empty values of various sizes for inserting blank spaces into the
 * savefile.
 */
static byte blank_u8b  = 0;
static u16b blank_u16b = 0;

/*
 * The following functions are used to handle the basic building blocks
 * of savefiles.  They also maintain the "checksum".
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

static void sf_put(byte v)
{
	/* Encode the value, write a character */
	xor_byte ^= v;
	(void)putc((int)xor_byte, fff);

	/* Maintain the checksum info */
	v_stamp += v;
	x_stamp += xor_byte;
}


/*
 * Size-aware read/write routines for the savefile.  They do all their
 * work through sf_get and sf_put.
 *
 * Be very careful not to allow data to be changed unless "load_file"
 * is TRUE!
 */
static void do_byte(byte *v)
{
	if (load_file) *v = sf_get();
	else
	{
		byte val = *v;
		sf_put(val);
	}
}

static void do_char(char *v)
{
	if (load_file) *v = sf_get();
	else
	{
		char val = *v;
		sf_put(val);
	}
}

static void do_u16b(u16b *v)
{
	if (load_file)
	{
		(*v) = sf_get();
		(*v) |= ((u16b)(sf_get()) << 8);
	}
	else
	{
		u16b val;
		val = *v;
		sf_put((byte)(val & 0xFF));
		sf_put((byte)((val >> 8) & 0xFF));
	}
}

static void do_s16b(s16b *ip)
{
	if (load_file)
	{
		do_u16b((u16b*)ip);
	}
	else
	{
		s16b val;
		val = *ip;
		do_u16b((u16b*)ip);
	}
}

static void do_u32b(u32b *ip)
{
	if (load_file)
	{
		(*ip) = sf_get();
		(*ip) |= ((u32b)(sf_get()) <<  8);
		(*ip) |= ((u32b)(sf_get()) << 16);
		(*ip) |= ((u32b)(sf_get()) << 24);
	}
	else
	{
		u32b val = *ip;
		sf_put( (byte)(val & 0xFF));
		sf_put( (byte)((val >>  8) & 0xFF));
		sf_put( (byte)((val >> 16) & 0xFF));
		sf_put( (byte)((val >> 24) & 0xFF));
	}
}

static void do_s32b(s32b *ip)
{
	do_u32b((u32b*)ip);
}

static void do_string(char *str, int max)
{
	/* Save string */
	if (!load_file)
	{
		/* Write string */
		while (*str)
		{
			do_byte((byte*)str);
			str++;
		}

		/* Write a terminator */
		do_byte((byte*)str);
	}

	/* Load string */
	else
	{
		int i;

		/* Read the string */
		for (i = 0; TRUE; i++)
		{
			byte tmp8u;

			/* Read a byte */
			do_byte(&tmp8u);

			/* Collect string while legal */
			if (i < max) str[i] = tmp8u;

			/* End of string */
			if (!tmp8u) break;
		}

		/* Terminate */
		str[max-1] = '\0';
	}
}



/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
	byte tmp8u;

	/* Strip the bytes */
	while (n--) do_byte(&tmp8u);
}


/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z, beta v".
 *
 * Note that we use Sangband reckoning exclusively.
 */
static bool older_than(byte x, byte y, byte z, byte v)
{
	/* Paranoia */
	if (!load_file) return (FALSE);

	/* Much older, or much more recent */
	if (sf_major < x) return (TRUE);
	if (sf_major > x) return (FALSE);

	/* Distinctly older, or distinctly more recent */
	if (sf_minor < y) return (TRUE);
	if (sf_minor > y) return (FALSE);

	/* Barely older, or barely more recent */
	if (sf_patch < z) return (TRUE);
	if (sf_patch > z) return (FALSE);

	/* Fractionally older or more recent */
	if (sf_extra < v) return (TRUE);
	if (sf_extra > v) return (FALSE);

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
	prt(msg, y, (Term->cols - 80) / 2);

	/* Advance one line (wrap if needed) */
	if (++y >= 24) y = 2;

	/* Flush it */
	(void)Term_fresh();
}


void do_history(void)
{
	size_t i;
	u32b tmp32u;

	/* Handle count of history items */
	if (!load_file) tmp32u = history_get_num();
	do_u32b(&tmp32u);
	if (load_file) history_init(tmp32u);

	for (i = 0; i < tmp32u; i++)
	{
		do_u16b(&history_list[i].type);
		do_s32b(&history_list[i].turn);
		do_s16b(&history_list[i].dlev);
		do_s16b(&history_list[i].clev);
		do_byte(&history_list[i].a_idx);
		do_string(history_list[i].event, 200);
	}
}


/*
 * Handle an object
 */
static errr do_item(object_type *o_ptr)
{
	object_kind *k_ptr;

	char buf[DESC_LEN];


	/* Kind */
	do_s16b(&o_ptr->k_idx);

	/* Paranoia */
	if ((o_ptr->k_idx < 0) || (o_ptr->k_idx >= z_info->k_max)) return (-2);

	do_byte(&o_ptr->iy);
	do_byte(&o_ptr->ix);

	do_byte(&o_ptr->tval);
	do_byte(&o_ptr->sval);

	do_s16b(&o_ptr->pval);
	do_u32b(&o_ptr->flags_pval1);
	do_s16b(&o_ptr->pval2);
	do_u32b(&o_ptr->flags_pval2);
	do_s16b(&o_ptr->pval3);
	do_u32b(&o_ptr->flags_pval3);

	do_u32b(&o_ptr->flags1);
	do_u32b(&o_ptr->flags2);
	do_u32b(&o_ptr->flags3);

	do_s16b(&o_ptr->to_h);
	do_s16b(&o_ptr->to_d);
	do_s16b(&o_ptr->to_a);
	do_s16b(&o_ptr->ac);
	do_byte(&o_ptr->dd);
	do_byte(&o_ptr->ds);

	do_s16b(&o_ptr->weight);
	do_byte(&o_ptr->number);

	do_byte(&o_ptr->artifact_index);
	do_byte(&o_ptr->ego_item_index);

	do_s16b(&o_ptr->timeout);
	do_byte(&o_ptr->activate);

	do_s32b(&o_ptr->b_cost);
	do_byte(&o_ptr->cost_adjust);

	do_s16b(&o_ptr->drop_depth);
	do_byte(&o_ptr->quivered);

	/* Space available for expansion */
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);


	do_byte(&o_ptr->ident);
	do_byte(&o_ptr->marked);
	do_byte(&o_ptr->inscrip);

	/* Held by monster index */
	do_s16b(&o_ptr->held_m_idx);

	/* Next object index */
	do_s16b(&o_ptr->next_o_idx);

	/* Inscription */
	if (!load_file)
	{
		/* Save the inscription (if any) */
		if (o_ptr->note) do_string((char *)quark_str(o_ptr->note), 0);
		else             do_string((char *)"", 0);
	}
	else
	{
		do_string(buf, 128);

		/* Save the inscription */
		if (buf[0]) o_ptr->note = quark_add(buf);
	}


	/* Loading a file -- read object, make corrections */
	if (load_file)
	{
		u32b f1, f2, f3;

		/* Obtain the "kind" template */
		k_ptr = &k_info[o_ptr->k_idx];

		/* Hack -- fix some tvals */
		if (older_than(0, 9, 9, 8))
		{
			if      (o_ptr->tval ==  3) o_ptr->tval = 2;
			else if (o_ptr->tval ==  5) o_ptr->tval = 3;
			else if (o_ptr->tval ==  7) o_ptr->tval = 10;
			else if (o_ptr->tval == 85) o_ptr->tval = 5;
			else if (o_ptr->tval == 86) o_ptr->tval = 6;
			else if (o_ptr->tval == 87) o_ptr->tval = 7;
		}

		/* Hack -- fix some scrolls */
		if (older_than(0, 9, 9, 9) && (o_ptr->tval == 70))
		{
			if      (o_ptr->sval == 31) o_ptr->sval = 12;
			else if (o_ptr->sval == 12) o_ptr->sval = 13;
			else if (o_ptr->sval == 13) o_ptr->sval = 14;
			else if (o_ptr->sval == 14) o_ptr->sval = 15;
			else if (o_ptr->sval == 15) o_ptr->sval = 16;
			else if (o_ptr->sval == 16) o_ptr->sval = 17;
			else if (o_ptr->sval == 17) o_ptr->sval = 18;
			else if (o_ptr->sval == 18) o_ptr->sval = 19;
			else if (o_ptr->sval == 19) o_ptr->sval = 20;
			else if (o_ptr->sval == 20) o_ptr->sval = 21;
			else if (o_ptr->sval == 21) o_ptr->sval = 22;
			else if (o_ptr->sval == 22) o_ptr->sval = 23;
			else if (o_ptr->sval == 23) o_ptr->sval = 31;
		}

		/* Repair old indexes */
		if (older_than(0, 9, 9, 9))
		{
			o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);
			if (object_known_p(o_ptr)) object_aware(o_ptr);
		}

		/* Handle older missile weapons */
		if ((older_than(0, 9, 9, 21)) && (o_ptr->tval == 19))
		{
			if      (o_ptr->sval ==  2) o_ptr->tval = TV_SLING;
			else if (o_ptr->sval == 12) o_ptr->tval = TV_BOW;
			else if (o_ptr->sval == 13) o_ptr->tval = TV_BOW;
			else if (o_ptr->sval == 22) o_ptr->tval = TV_CROSSBOW;
			else if (o_ptr->sval == 23) o_ptr->tval = TV_CROSSBOW;
			else if (o_ptr->sval == 24) o_ptr->tval = TV_CROSSBOW;
			else
			{
				note(format("Object sval (%d) not recognized.", o_ptr->sval));
				return(1);
			}
		}

		/* Hack -- undo the "ring of vulnerability" hack */
		if (older_than(0, 9, 9, 22) && !older_than(0, 9, 9, 19))
		{
			if (o_ptr->tval == 40)
			{
				if (o_ptr->sval == 14) o_ptr->sval = SV_AMULET_INTELLIGENCE;
				if (o_ptr->sval == 15) o_ptr->sval = SV_AMULET_WISDOM;
				if (o_ptr->sval == 16) o_ptr->sval = SV_AMULET_CHARISMA;
				o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);
			}
			if (o_ptr->tval == 45)
			{
				if (o_ptr->sval == 31) o_ptr->sval = SV_RING_STR;
				if (o_ptr->sval == 32) o_ptr->sval = SV_RING_DEX;
				if (o_ptr->sval == 33) o_ptr->sval = SV_RING_CON;
				o_ptr->k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);
			}
		}

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Paranoia */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr;

			/* Paranoia */
			if (o_ptr->artifact_index >= z_info->a_max) return (-1);

			/* Obtain the artifact info */
			a_ptr = &a_info[o_ptr->artifact_index];

			/* Verify that artifact */
			if (!a_ptr->name) o_ptr->artifact_index = 0;
		}

		/* Fix some artifacts */
		if (older_than(0, 9, 9, 11))
		{
			if (o_ptr->artifact_index)
			{
				artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

				o_ptr->pval = a_ptr->pval1;
				o_ptr->flags_pval1 = a_ptr->flags_pval1;

				o_ptr->pval2 = a_ptr->pval2;
				o_ptr->flags_pval2 = a_ptr->flags_pval2;

				o_ptr->pval3 = a_ptr->pval3;
				o_ptr->flags_pval3 = a_ptr->flags_pval3;

				o_ptr->flags1 = a_ptr->flags1;
				o_ptr->flags2 = a_ptr->flags2;
				o_ptr->flags3 = a_ptr->flags3;

				o_ptr->to_h = a_ptr->to_h;
				o_ptr->to_d = a_ptr->to_d;
				o_ptr->to_a = a_ptr->to_a;

				o_ptr->ac = a_ptr->ac;

				o_ptr->dd = a_ptr->dd;
				o_ptr->ds = a_ptr->ds;

				o_ptr->weight = a_ptr->weight;
			}
		}

		/* Paranoia */
		if (o_ptr->ego_item_index)
		{
			ego_item_type *e_ptr;

			/* Paranoia */
			if (o_ptr->ego_item_index >= z_info->e_max) return (-1);

			/* Obtain the ego-item info */
			e_ptr = &e_info[o_ptr->ego_item_index];

			/* Verify that ego-item */
			if (!e_ptr->name) o_ptr->ego_item_index = 0;
		}

		/* Insure that ordinary rods all recharge at the same speed */
		if ((o_ptr->tval == TV_ROD) &&
		    (!o_ptr->ego_item_index) && (!o_ptr->artifact_index))
		{
			if (o_ptr->pval != k_ptr->pval * o_ptr->number)
			{
				o_ptr->pval = k_ptr->pval * o_ptr->number;
			}
		}

		/* XXX fix b_cost */
		if (older_than(0, 9, 9, 4))
		{
			o_ptr->b_cost = k_ptr->cost;

			if (o_ptr->ego_item_index)
			{
				if (!e_info[o_ptr->ego_item_index].cost) o_ptr->b_cost = 0;
				else o_ptr->b_cost += e_info[o_ptr->ego_item_index].cost;
			}
		}
	}

	/* Success */
	return (0);
}

/*
 * Special monster flags that get saved in the savefile
 */
#define SAVE_MON_FLAGS \
(MFLAG_MIME | MFLAG_ACTV | MFLAG_TOWN | MFLAG_MADD | MFLAG_WARY | MFLAG_BLBR)



/*
 * Handle a monster
 */
static void do_monster(monster_type *m_ptr)
{
	u16b tmp16u;
	byte tmp8u;

	/* Monster race */
	do_s16b(&m_ptr->r_idx);

	/* Read the other information */
	do_byte(&m_ptr->fy);
	do_byte(&m_ptr->fx);
	do_s16b(&m_ptr->hp);
	do_s16b(&m_ptr->maxhp);
	do_byte(&m_ptr->csleep);
	do_byte(&m_ptr->mspeed);
	do_byte(&m_ptr->energy);
	do_byte(&m_ptr->stunned);
	do_byte(&m_ptr->confused);
	do_byte(&m_ptr->monfear);
	if (!older_than(0, 9, 9, 16))
	{
		do_byte(&m_ptr->slowed);
		do_byte(&m_ptr->hasted);
	}
	if (older_than(0, 9, 9, 19))
	{
		do_byte(&tmp8u);
		if (tmp8u) m_ptr->mflag |= (MFLAG_BLBR);
	}

	if (load_file)
	{
		do_u16b(&m_ptr->mflag);
	}
	else
	{
		tmp16u = m_ptr->mflag & (SAVE_MON_FLAGS);
		do_u16b(&tmp16u);
	}

	do_u32b(&m_ptr->smart);
	do_byte(&m_ptr->ty);
	do_byte(&m_ptr->tx);
	do_byte(&m_ptr->mana);
	do_s16b(&m_ptr->hold_o_idx);


	/* Space available for expansion */
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
}


/*
 * Handle a trap
 */
static void do_trap(trap_type *t_ptr)
{
	do_byte(&t_ptr->t_idx);

	do_byte(&t_ptr->fy);
	do_byte(&t_ptr->fx);

	do_byte(&t_ptr->xtra);
	do_s16b(&t_ptr->hold_o_idx);
	do_u16b(&t_ptr->flags);

	/* Space available for expansion */
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
}


/*
 * Read the monster lore
 */
static void do_mon_lore(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	/* Count sights/deaths/kills */
	do_s16b(&l_ptr->sights);
	do_s16b(&l_ptr->deaths);
	do_s16b(&l_ptr->pkills);
	do_s16b(&l_ptr->tkills);

	/* Count wakes and ignores */
	do_byte(&l_ptr->wake);
	do_byte(&l_ptr->ignore);

	/* Extra stuff */
	do_byte(&l_ptr->flags);
	do_byte(&l_ptr->xtra2);

	/* Count drops */
	do_byte(&l_ptr->drop_gold);
	do_byte(&l_ptr->drop_item);

	/* Count spells */
	do_byte(&l_ptr->ranged);

	/* Count blows of each type */
	do_byte(&l_ptr->blows[0]);
	do_byte(&l_ptr->blows[1]);
	do_byte(&l_ptr->blows[2]);
	do_byte(&l_ptr->blows[3]);

	/* Memorize flags */
	do_u32b(&l_ptr->flags1);
	do_u32b(&l_ptr->flags2);
	do_u32b(&l_ptr->flags3);
	do_u32b(&l_ptr->flags4);
	do_u32b(&l_ptr->flags5);
	do_u32b(&l_ptr->flags6);
	do_u32b(&l_ptr->flags7);

	/* "Racial" monster limit per level */
	do_byte(&r_ptr->max_num);

	/* Later (?) */
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);


	/* Loading a file -- read monster */
	if (load_file)
	{
		/* Repair the lore flags */
		l_ptr->flags1 &= r_ptr->flags1;
		l_ptr->flags2 &= r_ptr->flags2;
		l_ptr->flags3 &= r_ptr->flags3;
		l_ptr->flags4 &= r_ptr->flags4;
		l_ptr->flags5 &= r_ptr->flags5;
		l_ptr->flags6 &= r_ptr->flags6;
		l_ptr->flags7 &= r_ptr->flags7;
	}
}



/*
 * Read the object lore
 */
static void do_obj_lore(int k_idx)
{
	byte tmp8u = 0;

	object_kind *k_ptr = &k_info[k_idx];

	/* Write object lore */
	if (!load_file)
	{
		if (k_ptr->special & (SPECIAL_AWARE))        tmp8u |= 0x01;
		if (k_ptr->special & (SPECIAL_TRIED))        tmp8u |= 0x02;
		if (k_ptr->special & (SPECIAL_KNOWN_EFFECT)) tmp8u |= 0x04;
		if (k_ptr->special & (SPECIAL_MESSAGE))      tmp8u |= 0x08;
		if (k_ptr->special & (SPECIAL_EVER_SEEN))    tmp8u |= 0x10;

		do_byte(&tmp8u);
	}

	/* Read object lore */
	if (load_file)
	{
		do_byte(&tmp8u);

		if (tmp8u & (0x01)) k_ptr->special |= (SPECIAL_AWARE);
		if (tmp8u & (0x02)) k_ptr->special |= (SPECIAL_TRIED);
		if (tmp8u & (0x04)) k_ptr->special |= (SPECIAL_KNOWN_EFFECT);
		if (tmp8u & (0x08)) k_ptr->special |= (SPECIAL_MESSAGE);
		if (tmp8u & (0x10)) k_ptr->special |= (SPECIAL_EVER_SEEN);
	}
}



/*
 * Handle a store
 */
static errr do_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	/* Read the basic info */
	do_s32b(&st_ptr->store_open);
	do_s16b(&st_ptr->insult_cur);
	do_byte(&st_ptr->owner);
	do_byte(&st_ptr->stock_num);
	do_s16b(&st_ptr->good_buy);
	do_s16b(&st_ptr->bad_buy);
	do_s32b(&st_ptr->total_buy);


	/* Save the stock */
	if (!load_file)
	{
		for (j = 0; j < st_ptr->stock_num; j++)
		{
			/* Save each item in stock */
			(void)do_item(&st_ptr->stock[j]);
		}
	}

	/* Load the stock */
	else
	{
		for (j = 0; j < st_ptr->stock_num; j++)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Read the item */
			if (do_item(i_ptr))
			{
				note("Error reading item (store stock)");
				return (-1);
			}

			/* Skip missing items */
			if (!i_ptr->k_idx)
			{
				st_ptr->stock_num--;
				j--;
				continue;
			}

			/* Load item - add to inventory */
			if (j < STORE_INVEN_MAX)
			{
				/* Accept the item */
				object_copy(&st_ptr->stock[j], i_ptr);
			}
		}
	}


	/* Space available for expansion */
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);
	do_byte(&blank_u8b);

	/* Success */
	return (0);
}


/*
 * Handle RNG state
 */
static void do_randomizer(void)
{
	int i;

	/* Temporary */
	do_u16b(&blank_u16b);

	/* Place */
	do_u16b(&Rand_place);

	/* State */
	for (i = 0; i < RAND_DEG; i++)
	{
		do_u32b(&Rand_state[i]);
	}

	/* Accept */
	if (load_file) Rand_quick = FALSE;
}


/*
 * Handle options
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
static void do_options(void)
{
	int i, k, n;

	byte tmp, max, dummy;
	u32b flag[8];
	u32b mask[8];
	u32b window_flag[TERM_MAX];
	u32b window_mask[TERM_MAX];


	/*** Special info */

	/* Read "delay_factor" */
	do_byte(&op_ptr->delay_factor);

	/* Read "hitpoint_warn" */
	do_byte(&op_ptr->hitpoint_warn);

	/* Space for special options */
	if (!older_than(0, 9, 9, 6))
	{
		/* Read "autosave frequency */
		do_s16b(&autosave_freq);

		/* Space for expansion */
		for (i = 0; i < 19; i++) do_u16b(&blank_u16b);
	}


	/*** Normal Options ***/

	/* Saving a file -- convert option settings into bitflags */
	if (!load_file)
	{
		/* Reset */
		for (i = 0; i < 8; i++)
		{
			flag[i] = 0L;
			mask[i] = 0L;
		}

		/* Analyze the options */
		for (i = 0; i < OPT_MAX; i++)
		{
			int os = i / 32;
			int ob = i % 32;

			/* Process real entries */
			if (option_text[i])
			{
				/* Set flag */
				if (op_ptr->opt[i])
				{
					/* Set */
					flag[os] |= (1L << ob);
				}

				/* Set mask */
				mask[os] |= (1L << ob);
			}
		}
	}


	/* Option flags */
	for (n = 0; n < 8; n++) do_u32b(&flag[n]);

	/* Option masks */
	for (n = 0; n < 8; n++) do_u32b(&mask[n]);


	/* Loading a file -- Set the options */
	if (load_file)
	{
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
	}

	/*** Window Options ***/

	/* Saving a file -- convert window settings into bitflags */
	if (!load_file)
	{
		/* Analyze the sub-windows */
		for (i = TERM_SUBWINDOW; i < TERM_MAX; i++)
		{
			/* Flags */
			window_flag[i] = op_ptr->window_flag[i];

			/* Mask */
			window_mask[i] = 0L;

			/* Build the mask */
			for (k = 0; k < 32; k++)
			{
				/* Set mask */
				if (window_flag_desc[k])
				{
					window_mask[i] |= (1L << k);
				}
			}
		}
	}


	if (!older_than(0, 9, 9, 24))
	{
		/* Window flags */
		for (n = 2; n < 10; n++) do_u32b(&window_flag[n]);

		/* Window masks */
		for (n = 2; n < 10; n++) do_u32b(&window_mask[n]);
	}
	else
	{
		/* Convert old window flags */
		for (n = 0; n < 8; n++) do_u32b(&window_flag[n + TERM_SUBWINDOW]);

		/* Convert old window masks */
		for (n = 0; n < 8; n++) do_u32b(&window_mask[n + TERM_SUBWINDOW]);
	}

	/* Loading a file -- Set the windows */
	if (load_file)
	{
		/* Analyze the windows */
		for (n = TERM_SUBWINDOW; n < TERM_MAX; n++)
		{
			op_ptr->window_flag[n] = 0L;

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

	/* Get maximum size of custom_display */
	tmp = max = CUSTOM_DISPLAY_ROWS;

	/* Get or save the number of customized displays */
	do_byte(&tmp);

	/*** Customized left panel ***/
	for (i = 0; i < tmp; i++)
	{
		/* Ignore excess saved custom displays */
		if (i > max) continue;

		/* Save or restore this custom display */
		if ((older_than(0, 9, 9, 17)) && (load_file)) do_byte(&dummy);
		else do_byte(&custom_display[i]);
	}

	/* Screen options */
	if (!older_than(0, 9, 9, 13))
	{
		do_byte(&blank_u8b);
		do_byte((byte *)&more_tall_display);

		if (!older_than(0, 9, 9, 25)) do_byte((byte *)&map_display_precise_fit);
		else do_u16b(&blank_u16b);

		do_s16b(&clear_y);
		do_s16b(&clear_x);

		/* Space for expansion */
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
	}

	/* Get character type (old savefile) */
	if (older_than(0, 9, 9, 21))
	{
		/* Adjust character type as requested */
		if (beginner_play)      p_ptr->character_type = PCHAR_BEGINNER;
		else if (ironman_play)  p_ptr->character_type = PCHAR_IRONMAN;
		else                    p_ptr->character_type = PCHAR_NORMAL;
	}
}



/* String-handling function from Greg Wooledge's random artifact generator. */
static char *my_strdup(const char *s)
{
	char *t = (char*) malloc(strlen(s) + 1);
	if (t) strcpy(t, s);
	return (t);
}


/*
 * Read the saved random artifacts from a savefile, and add them to the
 * a_name structure.  From Greg Wooledge's random artifacts patch.
 *
 * This code is somewhat ugly.  XXX XXX
 */
static int convert_saved_names(void)
{
	size_t name_size;
	char *a_base;
	char *a_next;
	char temp[64];

	int i;

	/* Temporary space for names, while reading and randomizing them. */
	char *names[ART_LIST_SIZE];


	/* Add the permanent artifact names to the temporary array. */
	for (i = 0; i < ART_MIN_RANDOM; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		names[i] = a_name + a_ptr->name;
	}

	/* Add the random artifact names to the temporary array. */
	for (i = ART_MIN_RANDOM; i < z_info->a_max; i++)
	{
		do_string(temp, 64);

		names[i] = my_strdup(temp);
	}

	/* Convert our names array into an a_name structure for later use. */
	name_size = 0;
	for (i = 0; i < z_info->a_max; i++)
	{
		name_size += strlen (names[i]) + 2;	/* skip first char */
	}

	C_MAKE(a_base, name_size, char);

	a_next = a_base + 1;	/* skip first char */

	for (i = 0; i < z_info->a_max; i++)
	{
		strcpy(a_next, names[i]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i]) + 1;
	}


	/* Free the old names */
	FREE(a_name);

	for (i = ART_MIN_RANDOM; i < z_info->a_max; i++)
	{
		(void)string_free(names[i]);
	}

	a_name = a_base;
	a_head.name_ptr = a_base;
	a_head.name_size = name_size;

	return (0);
}



/*
 * Handle random artifacts
 */
static errr do_artifacts(void)
{
	u16b total_artifacts  = z_info->a_max;
	u16b random_artifacts = z_info->a_max - ART_MIN_RANDOM;
	u16b num_of_names     = z_info->a_max - ART_MIN_RANDOM;

	int i;
	int err = 0;


	/* Total number of artifacts */
	do_u16b(&total_artifacts);

	/* Number of random artifacts */
	do_u16b(&random_artifacts);


	/* Loading a file -- Incompatible save files */
	if ((load_file) && (total_artifacts > z_info->a_max))
	{
		note(format("Savefile contains more artifacts (%d) than your copy of the game (%d)", total_artifacts, z_info->a_max));
		return (24);
	}

	/* Artifact info */
	for (i = 0; i < total_artifacts; i++)
	{
		/* Load standard artifacts */
		if (i < ART_MIN_RANDOM)
		{
			do_byte(&a_info[i].cur_num);
			do_byte(&blank_u8b);
			do_byte(&blank_u8b);
			do_byte(&blank_u8b);
		}

		/* Random artifacts are specific to each player. */
		else
		{
			do_u16b(&a_info[i].name);
			do_u32b(&a_info[i].text);

			do_byte(&a_info[i].tval);
			do_byte(&a_info[i].sval);

			do_s16b(&a_info[i].to_h);
			do_s16b(&a_info[i].to_d);
			do_s16b(&a_info[i].to_a);

			do_byte(&a_info[i].dd);
			do_byte(&a_info[i].ds);

			do_s16b(&a_info[i].ac);
			do_s16b(&a_info[i].weight);

			do_s32b(&a_info[i].cost);

			do_s16b(&a_info[i].pval1);
			do_u32b(&a_info[i].flags_pval1);
			do_s16b(&a_info[i].pval2);
			do_u32b(&a_info[i].flags_pval2);
			do_s16b(&a_info[i].pval3);
			do_u32b(&a_info[i].flags_pval3);

			do_u32b(&a_info[i].flags1);
			do_u32b(&a_info[i].flags2);
			do_u32b(&a_info[i].flags3);

			do_byte(&a_info[i].level);
			do_byte(&a_info[i].rarity);
			do_u16b(&a_info[i].xtra);
			do_byte(&blank_u8b);
			do_byte(&a_info[i].cur_num);
			do_byte(&a_info[i].activate);

			/* Extra space. */
			do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);


			/* Handle translations */
			if (load_file)
			{
				/* Handle older artifact missile weapons */
				if ((older_than(0, 9, 9, 21)) && (a_info[i].tval == 19))
				{
					if      (a_info[i].sval ==  2) a_info[i].tval = TV_SLING;
					else if (a_info[i].sval == 12) a_info[i].tval = TV_BOW;
					else if (a_info[i].sval == 13) a_info[i].tval = TV_BOW;
					else if (a_info[i].sval == 22) a_info[i].tval = TV_CROSSBOW;
					else if (a_info[i].sval == 23) a_info[i].tval = TV_CROSSBOW;
					else if (a_info[i].sval == 24) a_info[i].tval = TV_CROSSBOW;
					else
					{
						note(format("Artifact (#%d) sval (%d) not recognized.", i, a_info[i].sval));
						err = 1;
					}
				}
			}
		}
	}


	/* Number of random artifact names */
	do_u16b(&num_of_names);


	/* Saving a file -- Write the list of random artifact names. */
	if (!load_file)
	{
		for (i = ART_MIN_RANDOM; i < z_info->a_max; i++)
		{
			artifact_type *a_ptr = &a_info[i];
			do_string(format("%s", a_name + a_ptr->name), 64);
		}
	}

	/* Loading a file -- read and verify names */
	else
	{
		/*
		 * Verify number of names, and warn the chap who modified the game
		 * that he still has work to do on failure.
		 */
		if (num_of_names != (z_info->a_max - ART_MIN_RANDOM))
		{
			note(format("Number of stored random artifact names (%d) does not match the", num_of_names));
			note(format("number of random artifacts in your copy of %s (%d).", VERSION_STRING, z_info->a_max - ART_MIN_RANDOM));

			err = 1;
		}

		/* Otherwise, add the new names to the a_name structure. */
		else
		{
			err = convert_saved_names();

			/* Complain if naming fails. */
			if (err) note("Warning - random artifact naming failed!");
		}
	}

	/* Return any errors */
	return (err);
}



/*
 * Handle character info
 */
static errr do_character(void)
{
	int i;

	s16b tmp16s;

	do_string(op_ptr->full_name, 32);

	do_string(p_ptr->died_from, 80);

	do_string(p_ptr->history, 250);

	/* Character play style */
	if (!older_than(0, 9, 9, 21)) do_byte(&p_ptr->character_type);

	/* Race */
	do_byte(&p_ptr->prace);

	/* When loading a savefile, verify player race */
	if ((load_file) && (p_ptr->prace >= MAX_RACES))
	{
		note(format("Invalid player race (%d).", p_ptr->prace));
		return (-1);
	}

	/* Gender */
	do_byte(&p_ptr->psex);

	/* Spell realm */
	do_byte(&p_ptr->realm);

	/* Hitdice */
	do_byte(&p_ptr->hitdie);

	/* Age/Height/Weight */
	do_s16b(&p_ptr->age);
	do_s16b(&p_ptr->ht);
	do_s16b(&p_ptr->wt);

	/* Stat information (starting, maximum, current) */
	for (i = 0; i < A_MAX; i++)
	{
		do_s16b(&p_ptr->stat_birth[i]);
		do_s16b(&p_ptr->stat_max[i]);
		do_s16b(&p_ptr->stat_cur[i]);
	}

	do_s16b(&p_ptr->luck);
	do_s16b(&p_ptr->fame);

	/* Character power, final score, gold, starting gold */
	do_byte(&p_ptr->power);
	do_s32b(&p_ptr->final_score);
	do_s32b(&p_ptr->au);
	do_s16b(&p_ptr->au_birth);

	/* Experience */
	do_s32b(&p_ptr->exp);
	do_u16b(&p_ptr->exp_frac);

	/* Available for use */
	do_u16b(&blank_u16b);
	do_u16b(&blank_u16b);

	/* Hitpoints */
	do_s16b(&p_ptr->mhp);
	do_s16b(&p_ptr->chp);
	do_u16b(&p_ptr->chp_frac);

	/* Spellpoints */
	do_s16b(&p_ptr->msp);
	do_s16b(&p_ptr->csp);
	do_u16b(&p_ptr->csp_frac);


	do_s16b(&p_ptr->max_depth);

	do_s32b(&p_ptr->total_kills);

	/* Social class */
	do_s16b(&p_ptr->sc);

    /* Damage and hit-rate running totals */
	do_s16b(&p_ptr->avg_dam);
	do_s16b(&p_ptr->avg_dam_offhand);

	do_s16b(&p_ptr->avg_hit);
	do_s16b(&p_ptr->avg_hit_offhand);

	/* Only save monster targets -- grid targets can cause real trouble */
	if (!load_file)
	{
		if (p_ptr->target_who <= 0)
		{
			p_ptr->target_set = 0;
		}
	}

	do_s16b(&p_ptr->target_set);
	do_s16b(&p_ptr->target_who);
	do_s16b(&p_ptr->health_who);
	do_u16b(&blank_u16b);


	/* Various temporary conditions */
	do_s16b(&p_ptr->blind);
	do_s16b(&p_ptr->paralyzed);
	do_s16b(&p_ptr->confused);
	do_byte((byte *)&p_ptr->black_breath);
	do_s32b(&p_ptr->food);
	do_s16b(&p_ptr->energy);
	do_s16b(&p_ptr->fast);
	do_s16b(&p_ptr->slow);
	do_s16b(&p_ptr->afraid);
	do_s16b(&p_ptr->cut);
	do_s16b(&p_ptr->stun);
	do_s16b(&p_ptr->poisoned);
	do_s16b(&p_ptr->image);
	do_s16b(&p_ptr->protevil);
	do_s16b(&p_ptr->hero);
	do_s16b(&p_ptr->berserk);
	do_s16b(&p_ptr->necro_rage);
	do_s16b(&p_ptr->shield);
	if (!older_than(0, 9, 9, 9)) do_s16b(&p_ptr->steelskin);
	do_s16b(&p_ptr->blessed);
	do_s16b(&p_ptr->holy);
	do_s16b(&p_ptr->unsanctified);
	do_s16b(&p_ptr->tim_invis);
	do_s16b(&p_ptr->tim_inv_pow);
	do_s16b(&p_ptr->tim_infra);
	do_s16b(&p_ptr->detect_inv);
	do_s16b(&p_ptr->esp_evil);
	do_s16b(&p_ptr->tim_esp);
	do_s16b(&p_ptr->regen_hp);
	do_s16b(&p_ptr->regen_mana);
	do_s16b(&p_ptr->vitality);
	do_s16b(&p_ptr->mania);
	do_s16b(&p_ptr->res_dam);
	do_s16b(&p_ptr->forbid_summoning);
	do_s16b(&p_ptr->word_recall);

	do_s16b(&p_ptr->wraithform);
	do_s16b(&p_ptr->form_dur);
	do_u16b(&blank_u16b);

	do_byte((byte *)&p_ptr->suppress_bottle);
	do_byte((byte *)&p_ptr->move_dir);

	do_u16b(&blank_u16b);
	do_u16b(&blank_u16b);
	do_u16b(&blank_u16b);

	if (!older_than(0, 9, 9, 21)) do_s16b(&p_ptr->self_knowledge);
	else do_u16b(&blank_u16b);

	/* Turn counters */
	do_s32b(&p_ptr->resting_turns);
	do_s32b(&p_ptr->total_turns);
	do_u16b(&blank_u16b);

	do_s16b(&p_ptr->life_recovery_value);
	do_s16b(&p_ptr->mana_recovery_value);

	do_s16b(&p_ptr->dancing_feet);
	do_byte((byte *)&p_ptr->dancing_feet_safe);
	do_s16b(&p_ptr->phasing_foes);
	if (!older_than(0, 9, 9, 13))
	{
		do_s16b(&p_ptr->blink_away);
		do_s16b(&p_ptr->evasion);
	}
	do_s16b(&p_ptr->aura_cold);
	do_s16b(&p_ptr->aura_fire);
	do_s16b(&p_ptr->wiz_prot);
	do_s16b(&p_ptr->mental_barrier);
	do_s16b(&p_ptr->pois_power);
	do_s16b(&p_ptr->pois_power_dur);
	do_s16b(&p_ptr->chaos_power);
	do_s16b(&p_ptr->chaos_power_dur);

	do_s16b(&p_ptr->nexus_field);
	do_s16b(&p_ptr->nexus_field_strength);

	do_u16b(&blank_u16b);
	do_u16b(&blank_u16b);

	do_s16b(&p_ptr->oppose_fire);
	do_s16b(&p_ptr->oppose_cold);
	do_s16b(&p_ptr->oppose_acid);
	do_s16b(&p_ptr->oppose_elec);
	do_s16b(&p_ptr->oppose_pois);
	do_s16b(&p_ptr->oppose_ethereal);

	do_u16b(&p_ptr->special_attack);

	if (!older_than(0, 9, 9, 16))
	{
		do_s16b(&p_ptr->acid_attack);
		do_s16b(&p_ptr->elec_attack);
		do_s16b(&p_ptr->fire_attack);
		do_s16b(&p_ptr->cold_attack);
		do_s16b(&p_ptr->pois_attack);
	}

	do_s16b(&p_ptr->soul_reserve);

	do_byte(&p_ptr->trap_set.time);
	do_byte(&p_ptr->trap_set.y);
	do_byte(&p_ptr->trap_set.x);
	do_byte(&p_ptr->trap_set.tmp);

	do_byte(&p_ptr->schange);

	do_byte(&p_ptr->sneaking);
	do_s16b(&p_ptr->base_wakeup_chance);

	do_byte(&p_ptr->barehand);

	do_s16b(&p_ptr->tim_weath);
	do_s16b(&p_ptr->hold_weath);
	do_s16b(&p_ptr->humid);
	do_s16b(&p_ptr->wind);
	do_s16b(&p_ptr->temp);
	do_s16b(&p_ptr->humid_forecast);
	do_s16b(&p_ptr->wind_forecast);
	do_s16b(&p_ptr->temp_forecast);

	/* Number of glyphs and traps on level, and recent thefts */
	do_byte(&num_glyph_on_level);
	do_byte(&num_trap_on_level);
	do_byte(&num_recent_thefts);

	/* Food values */
	do_s32b(&p_ptr->food_bloated);
	do_s32b(&p_ptr->food_full);
	do_s32b(&p_ptr->food_hungry);
	do_s32b(&p_ptr->food_weak);
	do_s32b(&p_ptr->food_fainting);
	do_s32b(&p_ptr->food_starving);

	/* Number of times character was rolled up */
	if (!older_than(0, 9, 9, 11)) do_s32b(&p_ptr->birth_roll_requirement);
	else p_ptr->birth_roll_requirement = 0L;

	/* Skills */
	do_byte(&p_ptr->lastadv);

	/* Skills */
	for (i = 0; i < NUM_SKILLS; i++)
	{
		do_u16b(&(p_ptr->pskills[i].cur));
		do_u16b(&(p_ptr->pskills[i].max));
		do_s32b(&(p_ptr->pskills[i].practice_exp));
	}

	/* Handle oaths */
	do_byte(&p_ptr->oath);

	/* Saving a file -- Get current number of talents */
	if (!load_file) tmp16s = NUM_TALENTS;

	/* Number of talents */
	do_s16b(&tmp16s);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16s != NUM_TALENTS))
	{
		note(format("Number of talents in your savefile (%d) does not match the number in the game (%d)!", tmp16s, NUM_TALENTS));
		return (-1);
	}

	/* Talent time-outs, marked status */
	for (i = 0; i < tmp16s; i++)
	{
		do_byte(&p_ptr->ptalents[i].marked);
		do_byte(&p_ptr->ptalents[i].empty);
		do_s16b(&p_ptr->ptalents[i].count);
	}


	/* Saving a file -- Get current number of essence types */
	if (!load_file) tmp16s = NUM_ESSENCE;

	/* Number of essence types */
	do_s16b(&tmp16s);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16s != NUM_ESSENCE))
	{
		note(format("Number of essence types in your savefile (%d) does not match the number in the game (%d)!", tmp16s, NUM_ESSENCE));
		return (-1);
	}

	/* Pouch of essences */
	for (i = 0; i < tmp16s; i++) do_byte(&p_ptr->essence[i]);


	/* Hack -- the two "special RNG seeds" */
	do_u32b(&seed_flavor);
	do_u32b(&seed_town);


	/* Special stuff */
	do_u16b(&p_ptr->panic_save);
	do_s32b(&p_ptr->score);
	do_u16b(&p_ptr->total_winner);
	do_u16b(&p_ptr->noscore);
	do_byte((byte *)&p_ptr->wizard);

	/* Deaths (convert "age" in old savefiles) */
	if (!older_than(0, 9, 9, 18)) do_s16b(&p_ptr->deaths);
	else if (load_file)
	{
		if (p_ptr->noscore & (CHEAT_DEATH)) p_ptr->deaths = p_ptr->age;
		p_ptr->deaths = 0;
	}

	/* Place stairs */
	if (!older_than(0, 9, 9, 10)) do_s16b(&p_ptr->create_stair);

	/* Bones file selector */
	do_byte(&bones_selector);

	/* Read "death" */
	do_byte((byte *)&p_ptr->is_dead);

	/* Read "feeling" */
	do_byte(&feeling);
	do_byte((byte *)&no_feeling_yet);

	/* Read "noise" */
	if      (!older_than(0, 9, 9, 17)) do_s32b(&total_wakeup_chance);
	else if (!older_than(0, 9, 9,  9))
	{
		do_s16b(&tmp16s);
		total_wakeup_chance = tmp16s;
	}

	/* Save index for Inn name */
	do_s16b(&p_ptr->inn_name);

	/* Turn of last "feeling" */
	do_s32b(&old_turn);

	/* Current turn */
	do_s32b(&turn);

	/* Blank */
	do_byte(&blank_u8b);


	/* Empty space for anything */
	if (!older_than(0, 9, 9, 13))
	{
		do_byte(&p_ptr->last_set_options_screen);
		do_byte(&blank_u8b);

		do_u32b(&p_ptr->dungeon_flags);
		do_byte(&p_ptr->schange_skill);
		do_byte(&p_ptr->schange_min_skill);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);
		do_u16b(&blank_u16b);

		/* "640K memory is enough for ANYONE" */
	}


	/* Saving a file -- Get current number of HP entries */
	if (!load_file) tmp16s = PY_MAX_POWER;

	/* HP entries */
	do_s16b(&tmp16s);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16s < PY_MAX_POWER))
	{
		note(format("Too few (%u) hitpoint entries!", tmp16s));
		return (-1);
	}

	/* The player_hp array */
	for (i = 0; i < tmp16s; i++)
	{
		do_s16b(&p_ptr->player_hp[i]);
	}

	/* Spell data */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		do_byte(&p_ptr->spell_flags[i]);
	}

	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Ignore this information */
		if (older_than(0, 9, 9, 21)) do_byte(&blank_u8b);
	}


	/* Loading a file -- Initialize some stuff */
	if (load_file)
	{
		/* Important -- Initialize the sex */
		sp_ptr = &sex_info[p_ptr->psex];

		/* Important -- Initialize the race/class */
		rp_ptr = &race_info[p_ptr->prace];

		/* Important -- Initialize the magic */
		mp_ptr = &magic_info[p_ptr->realm];

	}

	/* Success */
	return (0);
}

/*
 * Handle the character's inventory
 *
 * Note that the inventory is re-sorted later by "dungeon()".
 */
static errr do_inventory(void)
{
	int slot = 0;
	u16b tmp16u;

	s16b i;

	object_type *i_ptr;
	object_type object_type_body;

	/* Saving a file */
	if (!load_file)
	{
		/* Write the inventory */
		for (i = 0; i < INVEN_TOTAL; i++)
		{
			i_ptr = &inventory[i];

			/* Skip non-objects */
			if (!i_ptr->k_idx) continue;

			/* Dump index */
			do_s16b(&i);

			/* Dump object */
			(void)do_item(i_ptr);
		}

		/* Add a sentinel to mark the end of the list */
		tmp16u = 0xFFFF;
		do_u16b(&tmp16u);
	}

	/* Loading */
	else
	{
		/* Read until done */
		while (TRUE)
		{
			u16b n;

			/* Get the next item index */
			do_u16b(&n);

			/* Sentinel marks the end of the list */
			if (n == 0xFFFF) break;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Read the item */
			if (do_item(i_ptr))
			{
				note(format("Error reading item (%d)", n));
				return (-1);
			}

			/* Hack -- verify item */
			if (!i_ptr->k_idx)
			{
				note("k_idx is 0");
				return (-1);
			}

			/* Verify slot */
			if (n >= INVEN_TOTAL)
			{
				note("Inventory slot is too high.");
				return (-1);
			}

			/* Wield equipment */
			if (n >= INVEN_WIELD)
			{
				/* Copy object */
				object_copy(&inventory[n], i_ptr);

				/* Add the weight */
				p_ptr->total_weight += (i_ptr->number * i_ptr->weight);

				/* One more item */
				p_ptr->equip_cnt++;

				/* Light sources are explicitly lit */
				if ((older_than(0, 9, 9, 19)) && (n == INVEN_LITE))
				{
					i_ptr->flags3 |= (TR3_IS_LIT);
				}
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
	}

	/* Success */
	return (0);
}



/*
 * Read the saved messages
 */
static void do_messages(void)
{
	s16b i, num;
	char buf[1024];
	u16b tmp16u;


	/* Saving a file -- Determine how many messages to save */
	if (!load_file)
	{
		num = message_num();
		if ((compress_messages) && (num > 40)) num = 40;
	}

	/* Messages */
	do_s16b(&num);

	/* Saving a file -- dump messages */
	if (!load_file)
	{
		/* Dump the messages and types (oldest first!) */
		for (i = num - 1; i >= 0; i--)
		{
			do_string((char*)message_str(i), 0);
			tmp16u = message_type(i);
			do_u16b(&tmp16u);
		}
	}

	/* Loading a file -- read and save messages */
	else
	{
		/* Read the messages */
		for (i = 0; i < num; i++)
		{
			/* Read the message */
			do_string(buf, 1024);

			/* Read the message type */
			do_u16b(&tmp16u);

			/* Save the message */
			message_add(buf, tmp16u);
		}
	}
}


/*
 * Handle the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that dungeon objects, including objects held by monsters and
 * traps, are placed directly into the dungeon, using "object_copy()",
 * which will copy "iy", "ix", and "held_m_idx", leaving "next_o_idx"
 * blank for objects held by monsters, since it is not saved in the
 * savefile.
 *
 * After loading the monsters, the objects being held by monsters are
 * linked directly into those monsters.
 */
static errr do_dungeon(void)
{
	int i, y, x;

	s16b depth;
	s16b py, px;

	byte count;
	byte tmp8u;
	u16b tmp16u;

	byte prev_char;
	u16b prev_int;

	u16b limit;


	/*** Basic info ***/

	/* Header info */
	do_s16b(&p_ptr->depth);
	do_u16b(&blank_u16b);
	do_s16b(&p_ptr->py);
	do_s16b(&p_ptr->px);
	do_byte(&dungeon_hgt);
	do_byte(&dungeon_wid);
	do_u16b(&blank_u16b);
	do_u16b(&blank_u16b);

	/* Note character position and depth */
	py = p_ptr->py;
	px = p_ptr->px;
	depth = p_ptr->depth;

	/* Loading a file -- sanity checks */
	if (load_file)
	{
		/* Ignore illegal dungeons */
		if ((depth < -1) || (depth >= MAX_DEPTH))
		{
			note(format("Ignoring illegal dungeon depth (%d)", depth));
			return (0);
		}

		/* Ignore illegal dungeons */
		if ((py < 0) || (py >= dungeon_hgt) || (px < 0) || (px >= dungeon_wid))
		{
			note(format("Ignoring illegal player location (%d,%d).", py, px));
			return (1);
		}
	}


	/* Saving a file -- run-length encode cave_info flags */
	if (!load_file)
	{
		/*** Simple "Run-Length-Encoding" of cave ***/

		/* Note that this will induce two wasted bytes */
		count = 0;
		prev_int = 0;

		/* Dump the cave */
		for (y = 0; y < dungeon_hgt; y++)
		{
			for (x = 0; x < dungeon_wid; x++)
			{
				/* Extract the important cave_info flags */
				tmp16u = (cave_info[y][x] & (SAVE_CAVE_FLAGS));

				/* If the run is broken, or too full, flush it */
				if ((tmp16u != prev_int) || (count == MAX_UCHAR))
				{
					do_byte(&count);
					do_u16b(&prev_int);
					prev_int = tmp16u;
					count = 1;
				}

				/* Continue the run */
				else
				{
					count++;
				}
			}
		}

		/* Flush the data (if any) */
		if (count)
		{
			do_byte(&count);
			do_u16b(&prev_int);
		}

		prev_int = 0xFFFF;
		do_u16b(&prev_int);
	}


	/* Loading a file -- run-length decode cave_info flags */
	if (load_file)
	{
		/* Load the dungeon data */
		for (x = y = 0; y < dungeon_hgt;)
		{
			/* Grab RLE info */
			do_byte(&count);
			do_u16b(&tmp16u);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Extract "info" */
				cave_info[y][x] = tmp16u;

				/* Advance/Wrap */
				if (++x >= dungeon_wid)
				{
					/* Wrap */
					x = 0;

					/* Advance/Wrap */
					if (++y >= dungeon_hgt) break;
				}
			}
		}

		do_u16b(&tmp16u);
		if (tmp16u != 0xFFFF) quit("cave_feat sentinel failed!");
	}


	/* Saving a file -- run-length encode features */
	if (!load_file)
	{
		/* Note that this will induce two wasted bytes */
		count = 0;
		prev_char = 0;

		/* Dump the cave */
		for (y = 0; y < dungeon_hgt; y++)
		{
			for (x = 0; x < dungeon_wid; x++)
			{
				/* Extract a byte */
				tmp8u = cave_feat[y][x];

				/* If the run is broken, or too full, flush it */
				if ((tmp8u != prev_char) || (count == MAX_UCHAR))
				{
					do_byte(&count);
					do_byte(&prev_char);
					prev_char = tmp8u;
					count = 1;
				}

				/* Continue the run */
				else
				{
					count++;
				}
			}
		}

		/* Flush the data (if any) */
		if (count)
		{
			do_byte(&count);
			do_byte(&prev_char);
		}

		prev_char = 0xFF;
		do_byte(&prev_char);
	}


	/* Loading a file -- run-length decode features */
	if (load_file)
	{
		/* Load the dungeon data */
		for (x = y = 0; y < dungeon_hgt;)
		{
			/* Grab RLE info */
			do_byte(&count);
			do_byte(&tmp8u);

			/* Special translation (store features were moved) */
			if ((older_than(0, 9, 9, 21)) && (tmp8u >= 64) && (tmp8u <= 72))
			{
				tmp8u += 20;
			}

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Extract "feat" */
				cave_set_feat(y, x, tmp8u);

				/* Advance/Wrap */
				if (++x >= dungeon_wid)
				{
					/* Wrap */
					x = 0;

					/* Advance/Wrap */
					if (++y >= dungeon_hgt) break;
				}
			}
		}

		do_byte(&tmp8u);
		if (tmp8u != 0xFF) quit("cave_info sentinel failed!");
	}


	/* Loading a file -- place player */
	if (load_file)
	{
		/*** Player ***/

		/* Save depth */
		p_ptr->depth = depth;

		/* Place player in dungeon */
		if (!player_place(py, px))
		{
			note(format("Cannot place player (%d, %d)!", py, px));
			return (-1);
		}
	}

	/* Saving a file -- compact object and monster lists */
	if (!load_file)
	{
		/* Compact the objects */
		compact_objects(0);

		/* Compact the monsters */
		compact_monsters(0);
	}

	/*** Objects ***/

	/* Total objects */
	if (!load_file) limit = o_max;
	do_u16b(&limit);

	/* Saving a file -- write objects */
	if (!load_file)
	{
		/* Dump the objects */
		for (i = 1; i < limit; i++)
		{
			object_type *o_ptr = &o_list[i];

			/* Dump it */
			(void)do_item(o_ptr);
		}
	}

	/* Loading a file -- read objects */
	if (load_file)
	{
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

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Read the item */
			if (do_item(i_ptr))
			{
				note("Error reading item");
				return (-1);
			}

			/* Monster */
			if (i_ptr->held_m_idx > 0)
			{
				/* Give the object to the monster */
				if (!monster_carry(i_ptr->held_m_idx, i_ptr))
				{
					note(format("Cannot place object %d!", o_max));
					return (-1);
				}
			}

			/* Trap */
			else if (i_ptr->held_m_idx < 0)
			{
				/* Make an object */
				int o_idx = o_pop();

				/* Success */
				if ((o_idx) && (ABS(i_ptr->held_m_idx) < z_info->t_max))
				{
					object_type *o_ptr;

					/* Get new object */
					o_ptr = &o_list[o_idx];

					/* Copy object */
					COPY(o_ptr, i_ptr, object_type);
				}
				else
				{
					note(format("Cannot place object %d!", o_max));
					return (-1);
				}
			}

			/* Dungeon */
			else
			{
				/* Give the object to the floor */
				if (!floor_carry(i_ptr->iy, i_ptr->ix, i_ptr))
				{
					note(format("Cannot place object %d!", o_max));
					return (-1);
				}
			}
		}
	}


	/*** Monsters ***/

	/* Saving a file -- use current # of monsters */
	if (!load_file) limit = m_max;

	/* Monsters */
	do_u16b(&limit);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (limit > z_info->r_max))
	{
		note(format("Too many (%d) monsters in the dungeon!", limit));
		return (-1);
	}

	/* Handle monster memory */
	for (i = 0; i < limit; i++) do_mon_lore(i);

	/* Saving a file -- write monsters */
	if (!load_file)
	{
		/* Dump the monsters */
		for (i = 1; i < limit; i++)
		{
			monster_type *m_ptr = &m_list[i];

			/* Dump it */
			do_monster(m_ptr);
		}
	}

	/* Saving a file -- read, convert, process monsters */
	else
	{
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
			do_monster(n_ptr);


			/* Hack -- ignore "broken" monsters */
			if (n_ptr->r_idx <= 0) continue;

			/* Hack -- no illegal monsters. */
			if (n_ptr->r_idx >= z_info->r_max) continue;


			/* Access the "r_idx" of the chosen monster */
			r_idx = n_ptr->r_idx;

			/* Access the actual race */
			r_ptr = &r_info[r_idx];

			/* If a player ghost, some special features need to be added. */
			if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
			{
				prepare_ghost(n_ptr->r_idx, n_ptr, TRUE);
			}

			/* Place monster in dungeon */
			if (!monster_place(n_ptr->fy, n_ptr->fx, n_ptr))
			{
				note(format("Cannot place monster %d", i));
				return (-1);
			}

			/* Mark minimum range for recalculation */
			n_ptr->min_range = 0;
		}


		/*** Holding ***/

		/* Reacquire objects */
		for (i = 1; i < o_max; ++i)
		{
			object_type *o_ptr;

			/* Get the object */
			o_ptr = &o_list[i];

			/* Ignore dungeon objects (and those held by traps) */
			if (o_ptr->held_m_idx <= 0) continue;

			/* Verify monster index */
			if (o_ptr->held_m_idx >= z_info->m_max)
			{
				note("Invalid monster index");
				return (-1);
			}
		}


		if (arg_fiddle) note("Loaded Dungeon Monsters");
	}


	/*** Effects ***/
	if (!older_than(0, 9, 9, 8))
	{
		effect_type *x_ptr;

		if (!load_file) limit = z_info->x_max;
		do_u16b(&limit);

		/* Verify maximum */
		if (load_file)
		{
			if (limit > z_info->x_max)
			{
				note(format("Too many (%d) effects!", limit));
				return (-1);
			}
		}

		/* Scan all effects */
		for (i = 0; i < limit; i++)
		{
			/* Get this effect */
			x_ptr = &x_list[i];

			do_byte(&x_ptr->index);
			do_byte(&x_ptr->type);

			do_byte(&x_ptr->y0);
			do_byte(&x_ptr->x0);

			do_s16b(&x_ptr->y1);
			do_s16b(&x_ptr->x1);

			do_char(&x_ptr->time_count);
			do_char(&x_ptr->time_delay);

			do_byte(&x_ptr->age);
			do_byte(&x_ptr->lifespan);

			do_s16b(&x_ptr->power);
			do_s16b(&x_ptr->power2);

			do_u16b(&x_ptr->flags);

			/* Expansion space */
			if (!older_than(0, 9, 9, 13)) do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);
		}
	}

	/*** Effect grids ***/
	if (!older_than(0, 9, 9, 23))
	{
		if (!load_file) limit = effect_grid_n;
		do_u16b(&limit);

		/* Verify maximum */
		if (load_file)
		{
			if (limit > EFFECT_GRID_MAX)
			{
				note(format("Too many (%d) effect grids!", limit));
				return (-1);
			}
			effect_grid_n = limit;
		}

		/* Scan all effect grids */
		for (i = 0; i < limit; i++)
		{
			do_byte(&effect_grid[i].y);
			do_byte(&effect_grid[i].x);

			do_byte(&effect_grid[i].x_idx);
			do_byte(&effect_grid[i].xtra);

			/* Expansion space */
			do_u16b(&blank_u16b);
			do_u16b(&blank_u16b);
		}

		if (arg_fiddle) note("Loaded Effects");
	}


	/*** Traps ***/

	/* Total traps */
	if (!load_file) limit = t_max;
	do_u16b(&limit);

	/* Saving a file -- write traps */
	if (!load_file)
	{
		/* Dump the traps */
		for (i = 0; i < limit; i++)
		{
			/* Dump it */
			do_trap(&t_list[i]);
		}
	}

	/* Loading a file -- read traps */
	if (load_file)
	{
		/* Verify maximum */
		if (limit >= z_info->t_max)
		{
			note(format("Too many (%d) traps!", limit));
			return (-1);
		}
		else t_max = limit;

		/* Read the dungeon traps */
		for (i = 0; i < t_max; i++)
		{
			/* Wipe the trap */
			(void)WIPE(&t_list[i], trap_type);

			/* Read the trap */
			do_trap(&t_list[i]);
		}

		/* Mark all the trapped grids (skip the first trap slot) */
		for (i = 1; i < t_max; i++)
		{
			/* Point to this trap */
			trap_type *t_ptr = &t_list[i];

			/* Verify coordinates */
			if (!in_bounds(t_ptr->fy, t_ptr->fx))
			{
				note(format("Trap #%d is out of bounds (%d, %d)!",
					i, t_ptr->fy, t_ptr->fx));
				return (-1);
			}

			/* Mark the grid this trap is in */
			cave_info[t_ptr->fy][t_ptr->fx] |= (CAVE_TRAP);

			/* Note if visible */
			if (t_ptr->flags & (TRAP_VISIBLE))
				cave_info[t_ptr->fy][t_ptr->fx] |= (CAVE_MARK);
		}
	}


	/* The dungeon is ready */
	character_dungeon = TRUE;

	/* Success */
	return (0);
}


/*
 * Actually save or load a savefile
 */
static errr do_savefile(void)
{
	int i;

	byte tmp8u;
	u16b tmp16u;


	/* Saving a file -- Store version information */
	if (!load_file)
	{
		/* No encryption of version # */
		xor_byte = 0;

		/* Store the current version of Sangband */
		xor_byte = 0;
		do_byte(&version_major);
		xor_byte = 0;
		do_byte(&version_minor);
		xor_byte = 0;
		do_byte(&version_patch);
		xor_byte = 0;
		do_byte(&version_extra);

		/* Note patch */
		sf_extra = version_extra;
	}

	/* Loading a file -- Strip the version bytes */
	else
	{
		strip_bytes(4);

		/* Mention the savefile version, if old */
		if (older_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, 0))
		{
			note(format("Loading a %d.%d.%d savefile...",
				sf_major, sf_minor, sf_patch));
		}
	}


	/* Reset the checksum */
	v_stamp = 0L;
	x_stamp = 0L;

	/* Hack -- decrypt */
	/* if (load_file) xor_byte = sf_extra; */


	/* Note the operating system (not used) */
	sf_xtra = 0L;

	/* Operating system info */
	do_u32b(&sf_xtra);


	/* Saving a file -- Note when the file was saved */
	if (!load_file) sf_when = time((time_t *) 0);

	/* Time of savefile creation */
	do_u32b(&sf_when);


	/* Number of past lives with this savefile */
	do_u16b(&sf_lives);


	/* Saving a file -- increment number of times saved */
	if (!load_file) sf_saves++;

	/* Number of times saved */
	do_u16b(&sf_saves);


	/* RNG state */
	do_randomizer();
	if ((load_file) && (arg_fiddle)) note("Loaded Randomizer Info");

	/* Handle the options */
	do_options();
	if ((load_file) && (arg_fiddle)) note("Loaded Option Flags");

	/* Handle the messages */
	do_messages();
	if ((load_file) && (arg_fiddle)) note("Loaded Messages");


	/* Saving a file -- use current # of monster races */
	if (!load_file) tmp16u = z_info->r_max;

	/* Monster memory */
	do_u16b(&tmp16u);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16u > z_info->r_max))
	{
		note(format("Too many (%u) monster races!", tmp16u));
		return (-1);
	}

	/* Handle monster memory */
	for (i = 0; i < tmp16u; i++) do_mon_lore(i);

	if ((load_file) && (arg_fiddle)) note("Loaded Monster Memory");


	/* Saving a file -- use current # of object types */
	if (!load_file) tmp16u = z_info->k_max;

	/* Object memory */
	do_u16b(&tmp16u);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16u > z_info->k_max))
	{
		note(format("Too many (%u) object kinds!", tmp16u));
		return (-1);
	}

	/* Handle object memory */
	for (i = 0; i < tmp16u; i++) do_obj_lore(i);

	if ((load_file) && (arg_fiddle)) note("Loaded Object Memory");


	/* Saving a file -- use current # of quests */
	if (!load_file) tmp16u = z_info->q_max;

	/* Quests */
	do_u16b(&tmp16u);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16u > z_info->q_max))
	{
		note(format("Too many (%u) quests!", tmp16u));
		return (23);
	}

	/* Handle Quests */
	for (i = 0; i < tmp16u; i++)
	{
		do_byte(&q_info[i].type);

		if (q_info[i].type == QUEST_FIXED)
		{
			do_byte(&q_info[i].active_level);
			do_s16b(&q_info[i].cur_num);
		}
		else if (q_info[i].type == QUEST_RANDOM)
		{
			do_byte(&q_info[i].reward);
			do_byte(&q_info[i].active_level);
			do_byte(&q_info[i].base_level);

			do_s16b(&q_info[i].r_idx);
			do_s16b(&q_info[i].cur_num);
			do_s16b(&q_info[i].max_num);

			if (!load_file) do_byte((byte *)&q_info[i].started);
			else
			{
				do_byte(&tmp8u);
				q_info[i].started = (tmp8u) ? TRUE : FALSE;
			}

			if (!older_than(0, 9, 9, 13)) do_byte(&q_info[i].slack);

			if (!older_than(0, 9, 9, 20)) do_byte(&q_info[i].diff);
			else                q_info[i].diff = 1;

			/* Loading a file -- Activate current quest */
			if ((load_file) && (q_info[i].active_level || q_info[i].reward))
			{
				p_ptr->cur_quest = q_info[i].base_level;
			}

			if (!older_than(0, 9, 9, 21)) do_byte(&q_info[i].flags);
		}
	}

	if ((load_file) && (arg_fiddle)) note("Loaded Quests");


	/* Saving a file -- use current # of quest memory areas */
	if (!older_than(0, 9, 9, 9))
	{
		if (!load_file) tmp16u = MAX_QM_IDX;

		/* Quest memory */
		do_u16b(&tmp16u);

		/* Loading a file -- Incompatible save files XXX XXX */
		if ((load_file) && (tmp16u > MAX_QM_IDX))
		{
			note(format("Too many (%u) quest memories!", tmp16u));
			return (-1);
		}

		/* Handle quest memory */
		for (i = 0; i < tmp16u; i++)
		{
			do_byte(&p_ptr->quest_memory[i].type);
			do_byte(&p_ptr->quest_memory[i].level);
			do_s16b(&p_ptr->quest_memory[i].r_idx);
			do_s16b(&p_ptr->quest_memory[i].max_num);
			do_byte((byte *)&p_ptr->quest_memory[i].succeeded);
			do_byte(&p_ptr->quest_memory[i].extra);
		}
	}
	else
	{
		/* Clear quest memory */
		for (i = 0; i < MAX_QM_IDX; i++)
		{
			p_ptr->quest_memory[i].type = 0;
		}
	}

	/* Handle artifacts -- old method */
	if (older_than(0, 9, 9, 11))
	{
		/* Saving a file -- use current # of artifacts */
		if (!load_file) tmp16u = z_info->a_max;

		/* Artifacts */
		do_u16b(&tmp16u);

		/* Loading a file -- Incompatible save files XXX XXX */
		if ((load_file) && (tmp16u > z_info->a_max))
		{
			note(format("Too many (%u) artifacts!", tmp16u));
			return (-1);
		}

		for (i = 0; i < tmp16u; i++)
		{
			do_byte(&a_info[i].cur_num);
			do_byte(&blank_u8b);
			do_byte(&blank_u8b);
			do_byte(&blank_u8b);
		}
		if ((load_file) && (arg_fiddle)) note("Loaded Artifacts");
	}

	/* Handle character information */
	if (do_character()) return (-1);
	if ((load_file) && (arg_fiddle)) note("Loaded extra information");

	/* Handle artifacts */
	if (do_artifacts()) return (-1);
	if ((load_file) && (arg_fiddle)) note("Loaded artifacts");

	/* Read the inventory */
	if (do_inventory())
	{
		note("Unable to read inventory");
		return (-1);
	}

	/* Saving a file -- use current # of stores */
	if (!load_file) tmp16u = MAX_STORES;

	/* Stores */
	do_u16b(&tmp16u);

	/* Loading a file -- Incompatible save files XXX XXX */
	if ((load_file) && (tmp16u > MAX_STORES))
	{
		note(format("Too many (%u) stores!", tmp16u));
		return (-1);
	}


	/* Handle stores */
	for (i = 0; i < tmp16u; i++)
	{
		if (do_store(i)) return (-1);
	}

	/* I'm not dead yet... */
	if (!p_ptr->is_dead)
	{
		/* Handle the dungeon (dead players have no dungeon) */
		if (load_file) note("Restoring Dungeon...");
		if (do_dungeon())
		{
			if (load_file) note("Error reading dungeon data");
			else           note("Error writing dungeon data");
			return (-1);
		}
	}

	/* Handle history */
	do_history();


	/* Saving a file -- write checksums (always) */
	if (!load_file)
	{
		/* Write the "value check-sum" */
		do_u32b(&v_stamp);

		/* Write the "encoded checksum" */
		do_u32b(&x_stamp);

		/* Error in save */
		if (ferror(fff) || (fflush(fff) == EOF)) return (-1);

		/* Successful save */
		return (0);
	}

	/* Success */
	return (0);
}



/*
 * Medium level player saver
 */
static bool save_player_aux(cptr name)
{
	bool ok = FALSE;

	int fd;
	int mode = 0644;


	/* No file yet */
	fff = NULL;


	/* File type is "SAVE" */
	FILE_TYPE(FILE_TYPE_SAVE);

	/* Grab permissions */
	safe_setuid_grab();

	/* Create the savefile */
	fd = fd_make(name, mode);

	/* Drop permissions */
	safe_setuid_drop();

	/* File is okay */
	if (fd >= 0)
	{
		/* Close the "fd" */
		(void)fd_close(fd);

		/* Grab permissions */
		safe_setuid_grab();

		/* Open the savefile */
		fff = my_fopen(name, "wb");

		/* Drop permissions */
		safe_setuid_drop();

		/* Successful open */
		if (fff)
		{
			ok = TRUE;

			/* Saving, not loading a file */
			load_file = FALSE;

			/* Write the savefile */
			if (do_savefile()) ok = FALSE;

			/* Attempt to close it */
			if (my_fclose(fff)) ok = FALSE;
		}

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove "broken" files */
		if (!ok) (void)fd_kill(name);

		/* Drop permissions */
		safe_setuid_drop();
	}


	/* Failure */
	if (!ok) return (FALSE);

	/* Successful save */
	character_saved = TRUE;

	/* Success */
	return (TRUE);
}


/*
 * Attempt to save the player in a savefile
 */
bool save_player(void)
{
	int result = FALSE;

	char safe[1024];


	/* We are saving a file */
	load_file = FALSE;

	/* New savefile */
	strcpy(safe, savefile);
	strcat(safe, ".new");

	/* Grab permissions */
	safe_setuid_grab();

	/* Remove it */
	(void)fd_kill(safe);

	/* Drop permissions */
	safe_setuid_drop();

	/* Attempt to save the player */
	if (save_player_aux(safe))
	{
		char temp[1024];

		/* Old savefile */
		strcpy(temp, savefile);
		strcat(temp, ".old");

		/* Grab permissions */
		safe_setuid_grab();

		/* Remove it */
		(void)fd_kill(temp);

		/* Preserve old savefile */
		(void)fd_move(savefile, temp);

		/* Activate new savefile */
		(void)fd_move(safe, savefile);

		/* Remove preserved savefile */
		(void)fd_kill(temp);

		/* Drop permissions */
		safe_setuid_drop();

		/* Hack -- Pretend the character was loaded */
		character_loaded = TRUE;

		/* Success */
		result = TRUE;
	}

	/* Write to the savefile list */
	save_savefile_names();

	/* Return the result */
	return (result);
}



/*
 * Actually read the savefile
 */
static errr load_player_aux(void)
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

	/* Loading a file */
	load_file = TRUE;

	/* Load the savefile */
	err = do_savefile();

	/* Check for errors */
	if (ferror(fff)) err = -1;

	/* Close the file */
	(void)my_fclose(fff);

	/* Result */
	return (err);
}


/*
 * Attempt to load a savefile.
 *
 * Sangband 1.0.0 and later cannot read savefiles from Sangband 0.9.5 and
 * earlier.
 *
 * We return -1 if the file does not exist, 0 if it does and loads correctly,
 * and 1 if it exists but is unreadable.
 *
 * We set the global flag "character_loaded" if a real, living, character
 * was loaded.
 *
 * On multi-user systems, you may only "read" a savefile if you will be
 * allowed to "write" it later, this prevents painful situations in which
 * the player loads a savefile belonging to someone else, and then is not
 * allowed to save his game when he quits.
 */
errr load_player(bool silent)
{
	int fd = -1;

	errr err = 0;

	byte vvv[4];

	cptr what = "generic";

	/* Clear */
	character_existed = FALSE;

	/* Paranoia */
	turn = 0;

	/* Paranoia */
	p_ptr->is_dead = FALSE;

	/* We are loading a file */
	load_file = TRUE;

	/* Require a savefile name */
	if (!savefile[0])
	{
		msg_print("No savefile specified.");
		message_flush();
		return (-1);
	}

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
		if (!silent)
		{
			msg_format("Savefile %s does not exist.", savefile);
			message_flush();
		}

		/* Report failure */
		return (-1);
	}

	/* Close the file */
	(void)fd_close(fd);


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
		/* Read the first four bytes */
		if (fd_read(fd, (char *)(vvv), 4)) err = -1;

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
		(void)Term_clear();

		/* Attempt to load */
		err = load_player_aux();

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

	/* Okay */
	if (!err)
	{
		/* Give a conversion warning */
		if ((version_major != sf_major) ||
		    (version_minor != sf_minor) ||
		    (version_patch != sf_patch))
		{
			if ((sf_major == 0) && ((sf_minor * 10 + sf_patch) < 99))
			{
				msg_format("Sangband %d.%d.%d cannot read pre-0.9.9 save files.",
					version_major, version_minor, version_patch);
				message_flush();
				return (1);
			}
			else
			{
				msg_format("Converted a %d.%d.%d savefile.",
					sf_major, sf_minor, sf_patch);
				message_flush();
			}
		}

		/* A character exists on this savefile */
		character_existed = TRUE;

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

				/* Check the character for completed item sets */
				check_item_sets();

				/* Done */
				return (0);
			}

			/* Count lives */
			sf_lives++;

			/* Forget turns */
			turn = old_turn = 0;

			/* Done */
			return (0);
		}

		/* Check the character for completed item sets */
		check_item_sets();

		/* A character was loaded */
		character_loaded = TRUE;

		/* Still alive */
		if (p_ptr->chp >= 0)
		{
			/* Reset cause of death */
			strcpy(p_ptr->died_from, "(alive and well)");
		}

		/* Success */
		return (0);
	}

	/* Message */
	msg_format("Error (%s) reading %d.%d.%d savefile.",
		what, sf_major, sf_minor, sf_patch);

	message_flush();

	/* Oops */
	return (1);
}




/**************************************************************/
/*                                                            */
/*                  Savefile Management Code                  */
/*                                                            */
/**************************************************************/


/*
 * Temporary storage of savefile paths, descriptions, and dead/alive status.
 */
char savefile_names[46][40];
char savefile_desc[46][DESC_LEN];
bool savefile_alive[46];

/*
 * Read the savefile record.  -DG-
 */
static int load_savefile_names(void)
{
	char buf[1024];
	char tmp[50];
	int max = 0;
	int fd;

	/* Build the filename */
#ifdef SAVEFILE_USE_UID
	(void)strnfmt(tmp, sizeof(tmp), "user.%d.svg", player_uid);
#else
	(void)strnfmt(tmp, sizeof(tmp), "global.svg");
#endif /* SAVEFILE_USE_UID */

	/* Attempt to load the savefile record */
	if (path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp)) return (0);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Grab permissions */
	safe_setuid_grab();

	/* Read the file */
	fff = my_fopen(buf, "r");

	/* Drop permissions */
	safe_setuid_drop();

	/* Failure */
	if (!fff) return (0);

	/* Parse the savefile record */
	while (0 == my_fgets(fff, buf, sizeof(buf)))
	{
		int i = 1;

		/* Read "dead/alive" */
		if      (buf[0] == '0') savefile_alive[max] = FALSE;
		else if (buf[0] == '1') savefile_alive[max] = TRUE;

		/* Read the savefile name */
		for (i = 1;; i++)
		{
			/* Build path */
			if (buf[i] != '@') savefile_names[max][i - 1] = buf[i];
			else
			{
				/* Terminate */
				savefile_names[max][i - 1] = '\0';
				break;
			}
		}

		/* Skip the '@' */
		i++;

		/* Read the character description */
		(void)strnfmt(savefile_desc[max], sizeof(savefile_desc[max]), buf + i);

		/* Append user ID to filename if necessary */
#ifdef SAVEFILE_USE_UID
		(void)strnfmt(tmp, sizeof(tmp),"%d.%s", player_uid, savefile_names[max]);
#else
		(void)strnfmt(tmp, sizeof(tmp), "%s", savefile_names[max]);
#endif

		/* Confirm that file still exists */
		(void)path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp);

		/* Grab permissions */
		safe_setuid_grab();

		/* File type is "SAVE" */
		FILE_TYPE(FILE_TYPE_SAVE);

		/* Check for file */
		fd = fd_open(buf, O_RDONLY);

		/* Drop permissions */
		safe_setuid_drop();

		/* File exists */
		if (fd >= 0)
		{
			(void)fd_close(fd);

			/* Increment file count, go to next record */
			max++;
		}
	}

	/* Close the savefile record */
	(void)my_fclose(fff);

	/* Return number of valid savefiles */
	return (max);
}


/*
 * Write the savefile record.  -DG-
 */
void save_savefile_names(void)
{
	char buf[1024];
	char tmp[DESC_LEN];
	char name[DESC_LEN];
	int i;


	/* Load existing savefile records */
	int max = load_savefile_names();

	/* Build the filename - use user index if necessary */
#ifdef SAVEFILE_USE_UID
	(void)strnfmt(tmp, sizeof(tmp),"user.%d.svg", player_uid);
#else
	(void)strnfmt(tmp, sizeof(tmp), "global.svg");
#endif /* SAVEFILE_USE_UID */

	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_SAVE, tmp);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Read the file */
	fff = my_fopen(buf, "w");

	/* Failure */
	if (!fff) return;


	/* Copy character name to a rewriteable string */
	(void)my_strcpy(name, op_ptr->full_name, sizeof(name));

	/* Add Latin-1 encodes */
	xstr_encode(name, sizeof(name));

	/* Save information about the current character savefile */
	fprintf(fff, "%c%s@%s, the %s is %s\n", ((p_ptr->is_dead)? '0' : '1'),
		op_ptr->base_name, name, race_info[p_ptr->prace].title,
		(!p_ptr->is_dead)?"alive":"dead");

	/* Rewrite all other savefile records - do not exceed 46 records */
	for (i = 0; i < MIN(max, 45); i++)
	{
		/* Do not record the current savefile more than once */
		if (!strcmp(savefile_names[i], op_ptr->base_name)) continue;

		/* Write a savefile record */
		fprintf(fff, "%c%s@%s\n", (savefile_alive[i])?'1':'0',
			savefile_names[i], savefile_desc[i]);
	}

	/* Close */
	(void)my_fclose(fff);
}


/*
 * Interact with the savefile management screen.  -DG-
 */
static bool savefile_menu(void)
{
	int k, sel, max;

	int i;

	byte x, y;

	char buf[256];

	char ind;

	/* No savefile has been loaded yet */
	character_loaded = FALSE;

	/* Wipe the player */
	player_wipe(TRUE);

	/* Load savefile records, leave room for two miscellaneous menu choices */
	max = load_savefile_names() + 2;

	/* Highlight the most recent savefile, or "create new character" */
	if (max > 2) sel = 2;
	else         sel = 0;


	/* Clear screen */
	(void)Term_clear();

	/* Interact with the menu */
	while (TRUE)
	{
		/* Build a header, center it */
		center_string(buf, sizeof(buf),
			format("Welcome to %s.  To play you will need a character.", VERSION_NAME),
			display_width());

		/* Display the header */
		c_put_str(TERM_L_BLUE, buf, 1, 1);

		/* Show available commands */
		c_put_str(TERM_L_BLUE, "  8/2/4/6) Movement keys            RETURN) Select",
			22, 11);
		c_put_str(TERM_L_BLUE, format("Backspace) Delete a savefile           ESC) Exit %s",
			VERSION_NAME), 23, 11);

		/* Display the menu choices */
		for (i = 0; i < max; i++)
		{
			ind = I2A(i % 26);
			if (i >= 26) ind = my_toupper(ind);

			/* Get text for this menu option */
			if      (i == 0) (void)strnfmt(buf, sizeof(buf), "%c) New Character", ind);
			else if (i == 1) (void)strnfmt(buf, sizeof(buf), "%c) Load Savefile", ind);
			else             (void)strnfmt(buf, sizeof(buf), "%c) %s", ind, savefile_names[i - 2]);

			/* Display this menu option */
			y = 6 + (i / 4);
			x = 20 * (i % 4) + 1;
			c_put_str(((sel == i) ? TERM_L_BLUE : TERM_WHITE), buf, y, x);

			/* This menu choice is selected */
			if (sel == i)
			{
				/* Load an existing savefile in the list */
				if (i >= 2)
				{
					/* Color depending on dead or alive */
					int attr = ((savefile_alive[i - 2]) ? TERM_L_GREEN : TERM_L_RED);

					c_prt(attr, format("%s", savefile_desc[i - 2]), 4, 1);
				}

				/* Load an existing savefile not in the list */
				else if (i == 1)
				{
					c_prt(TERM_WHITE, "Load an existing savefile not in the list", 4, 1);
				}

				/* Create a new character */
				else
				{
					c_prt(TERM_WHITE, "Create a new character", 4, 1);
				}
			}
		}

		y = 6 + (sel / 4);
		x = 20 * (sel % 4) + 1;

		/* Move the cursor to the letter of the selection */
		(void)Term_gotoxy(x, y);

		/* Get response */
		k = inkey(FALSE);

		/* Process commands */
		if (k == ESCAPE)
		{
			quit(NULL);
		}
		else if ((k == '6') || ((rogue_like_commands) && (k == 'l')))
		{
			/* Go forward one menu item, wrap */
			if (++sel >= max) sel = 0;
			continue;
		}
		else if ((k == '4') || ((rogue_like_commands) && (k == 'h')))
		{
			/* Go back one menu item, wrap */
			if (--sel < 0) sel = max - 1;
			continue;
		}
		else if ((k == '2') || ((rogue_like_commands) && (k == 'j')))
		{
			/* Go down one line of the menu */
			if (sel < max - 4) sel += 4;
			continue;
		}
		else if ((k == '8') || ((rogue_like_commands) && (k == 'k')))
		{
			/* Go up one line of the menu */
			if (sel >= 4) sel -= 4;
			continue;
		}
		else if ((k == '\r') || (k == '\n'))
		{
			/* Choose this menu option */
			if (sel < 26) k = I2A(sel);
			else k = toupper(I2A(sel));
		}

		/* Delete savefile */
		else if (((k == 0x7F) || (k == '\010')) && (sel >= 2))
		{
			char filename[1024];
			char tmp[50];

			if (!get_check(format("Really delete '%s'?", savefile_names[sel - 2]))) continue;

			/* Append user ID to filename if necessary */
#ifdef SAVEFILE_USE_UID
			(void)strnfmt(tmp, sizeof(tmp),"%d.%s", player_uid, savefile_names[sel - 2]);
#else
			(void)strnfmt(tmp, sizeof(tmp), "%s", savefile_names[sel - 2]);
#endif

			/* Build the file name */
			(void)path_build(filename, sizeof(filename), ANGBAND_DIR_SAVE, tmp);

			/* Delete this file */
			(void)fd_kill(filename);

			/* Reload the savefile record */
			max = load_savefile_names() + 2;
			if (max > 2) sel = 2;
			else         sel = 0;

			/* Clear screen */
			(void)Term_clear();

			continue;
		}

		if (k == 'a')
		{
			return (TRUE);
		}
		if (k == 'b')
		{
			/* Display prompt */
			prt("Enter the name of a savefile: ", 23, 0);

			/* Ask the user for a string */
			if (!askfor_aux(op_ptr->full_name, 30, FALSE)) continue;

			/* Process the player name, change savefile name */
			process_player_name(TRUE);

			return (FALSE);
		}
		else
		{
			int x;

			/* Process command */
			if (islower(k)) x = A2I(k);
			else x = A2I(my_tolower(k)) + 26;

			/* Stay legal */
			if ((x < 2) || (x >= max)) continue;

			/* Get player name */
			(void)strnfmt(op_ptr->full_name, sizeof(op_ptr->full_name), "%s", savefile_names[x - 2]);

			/* Process the player name, change savefile name */
			process_player_name(TRUE);

			return (FALSE);
		}
	}

	/* Return */
	return (FALSE);
}


/*
 * Load a character.
 *
 * When the game starts, this function is called to handle savefile loading.
 * We automatically load the most recent savefile recorded in our savefile
 * record.  If this fails, we load any savefile named "player" (this
 * replicates the old behavior).  Otherwise we load nothing and show the
 * savefile management screen.  -LM-
 *
 * An option exists to go directly to the savefile management menu.  This is
 * used if the player deliberately calls this function.
 *
 * As has traditionally been the case, we use "PLAYER" as a default savefile
 * name.
 *
 * This function must cooperate with the menu commands offered by various
 * ports.  XXX XXX XXX
 */
void savefile_load(bool force_menu)
{
	bool new_game;

	/* Load the savefile record */
	int max = load_savefile_names();


	/* No savefile has been loaded yet */
	character_loaded = FALSE;

	/* We're not forcing menus */
	if (!force_menu)
	{
		/* Non-empty savefile record, most recent char is alive */
		if ((max > 0) && (savefile_alive[0]))
		{
			/* Store the base name of the most recent savefile entry */
			strcpy(op_ptr->full_name, savefile_names[0]);

			/* Process this base name, change savefile name */
			process_player_name(TRUE);

			/* Try to load this savefile */
			if (load_player(TRUE) == 0) return;
		}

		/* We'll always load a savefile named "PLAYER" */

		/* Hack -- use "PLAYER" as the base name */
		strcpy(op_ptr->base_name, "PLAYER");

		/* Process this base name, change savefile name */
		process_player_name(TRUE);

		/* Try to load the file "PLAYER" */
		if (load_player(TRUE) == 0) return;
	}

	/* We're forcing a menu, or haven't found an obvious savefile */
	if (TRUE)
	{
		/* Repeat until satisfied */
		while (TRUE)
		{
			int error;

			/* Show the menu immediately, store chosen name */
			new_game = savefile_menu();

			/* We want to start a new game (not using any previous savefile) */
			if (new_game)
			{
				/* Hack -- Default base_name */
				strcpy(op_ptr->base_name, "PLAYER");

				/* Return */
				return;
			}

			/* Try to load this character */
			error = load_player(FALSE);

			/* File is unreadable */
			if (error)
			{
				/* Oops */
				if (error > 0)
				{
					msg_format("Sorry, this savefile cannot be read by %s %s.",
					VERSION_NAME, VERSION_STRING);
				}

				/* Oops */
				msg_print("Please choose another file or start a new game.");

				/* Wait for it */
				(void)inkey(ALLOW_CLICK);
			}

			/* Successfully loaded an old character (alive or dead) */
			else
			{
				/* Return */
				return;
			}
		}
	}
}
