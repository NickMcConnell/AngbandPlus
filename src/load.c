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
 * Avoid the top two lines, to avoid interference with "message()".
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
static void rd_string(char *str, size_t max)
{
	int i;

	/* Read the string */
	for (i = 0; TRUE; i++)
	{
		byte tmp8u;

		/* Read a byte */
		rd_byte(&tmp8u);

		/* Collect string while legal */
		if (i < (int)max) str[i] = tmp8u;

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

/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 *
 */
static errr rd_item(object_type *o_ptr)
{
	u16b save_flags;

	u32b f1, f2, f3;

	object_kind *k_ptr;

	char buf[128];

	rd_u16b(&save_flags);

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Paranoia */
	if ((o_ptr->k_idx < 0) || (o_ptr->k_idx >= z_info->k_max)) return (-1);

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

	if (save_flags & 0x0004) rd_byte(&o_ptr->a_idx); 
	if (save_flags & 0x0008) rd_byte(&o_ptr->e_idx);
	if (save_flags & 0x0010) rd_byte(&o_ptr->pfx_idx);

	if (save_flags & 0x0020) rd_s16b(&o_ptr->timeout);

	if (save_flags & 0x0040) rd_s16b(&o_ptr->to_h);

	if (save_flags & 0x0080) rd_s16b(&o_ptr->to_a);

	if (save_flags & 0x0100) rd_byte(&o_ptr->ident);

	rd_byte(&o_ptr->origin_nature);
	rd_s16b(&o_ptr->origin_dlvl);
	if (save_flags & 0x0200) rd_s16b(&o_ptr->origin_r_idx);
	if (save_flags & 0x0400) rd_s16b(&o_ptr->origin_s_idx);
	if (save_flags & 0x0800) rd_s16b(&o_ptr->origin_u_idx);
	
	if (save_flags & 0x1000) rd_byte(&o_ptr->marked);

	/* Monster holding object */
	if (save_flags & 0x2000) rd_s16b(&o_ptr->held_m_idx);

	if (save_flags & 0x4000) rd_byte(&o_ptr->xtra1);
	if (save_flags & 0x8000) rd_byte(&o_ptr->xtra2);

	/* Inscription */
	rd_string(buf, sizeof(buf));

	/* Save the inscription */
	if (buf[0]) o_ptr->note = quark_add(buf);

	/* Obtain the "kind" template */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

	/* Repair simple items - non wearable, non ammo, not artifact books*/
	if (!wearable_p(o_ptr) && (o_ptr->tval != TV_ARROW) && (o_ptr->tval != TV_QUEST) && 
		!((o_ptr->tval==TV_MAGIC_BOOK) && books[o_ptr->sval].flags & SBF_ARTIFACT))
	{
		/* Get the correct fields */
		o_ptr->to_h = k_ptr->to_h;
		o_ptr->to_a = k_ptr->to_a;

		/* Paranoia */
		o_ptr->a_idx = o_ptr->e_idx = 0;
		return (0);
	}

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Paranoia */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr;

		/* Paranoia */
		if (o_ptr->a_idx >= z_info->a_max) return (-1);

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->a_idx];

		/* Paranoia - artifact was created */
		a_ptr->status |= A_STATUS_CREATED;

		/* Verify that artifact */
		if (!a_ptr->name) o_ptr->a_idx = 0;
	}

	/* Paranoia */
	if (o_ptr->e_idx)
	{
		ego_item_type *e_ptr;

		/* Paranoia */
		if (o_ptr->e_idx >= z_info->e_max) return (-1);

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->e_idx];

		/* Verify that ego-item */
		if (!e_ptr->name) o_ptr->e_idx = 0;
	}

	/* Hack -- extract the "broken" flag */
	if ((o_ptr->tval != TV_ARROW) && (o_ptr->pval < 0)) o_ptr->ident |= (IDENT_BROKEN);

	/* Artifacts */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr;

		/* Obtain the artifact info */
		a_ptr = &a_info[o_ptr->a_idx];

		/* Get the new artifact "pval" */
		o_ptr->pval = a_ptr->pval;

		/* Get the new artifact prefix */
		o_ptr->pfx_idx = a_ptr->prefix_idx;

		/* Hack -- extract the "broken" flag */
		if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
	}

	/* Ego items */
	if (o_ptr->e_idx)
	{
		ego_item_type *e_ptr;

		/* Obtain the ego-item info */
		e_ptr = &e_info[o_ptr->e_idx];

		/* Hack -- extract the "broken" flag */
		if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

		/* Hack -- enforce legal pval */
		if (e_ptr->flags1 & (TR1_PVAL_MASK))
		{
			/* Force a meaningful pval */
			if (!o_ptr->pval) o_ptr->pval = 1;
		}
	}

	return (0);
}

/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
	/* Read the monster race */
	rd_s16b(&m_ptr->r_idx);
	rd_s16b(&m_ptr->s_idx);
	rd_s16b(&m_ptr->u_idx);

	/* Read the other information */
	rd_byte(&m_ptr->attr);
	rd_byte(&m_ptr->fy);
	rd_byte(&m_ptr->fx);
	rd_s16b(&m_ptr->hp);
	rd_s16b(&m_ptr->maxhp);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->bspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
	rd_byte(&m_ptr->blinded);
	rd_byte(&m_ptr->calmed);
	rd_byte(&m_ptr->cursed);
	rd_byte(&m_ptr->earthbound);
	rd_byte(&m_ptr->mist);

	rd_u16b(&m_ptr->sleep);
	rd_u16b(&m_ptr->bleeding);
	rd_u16b(&m_ptr->poisoned);
}

/*
 * Read the monster lore
 */
static void rd_lore(bool unique, int idx)
{
	monster_lore *l_ptr = (unique ? &lu_list[idx] : &lr_list[idx]);

	u16b save_flags;

	rd_u16b(&save_flags);

	/* Count sights/deaths/kills */
	if (save_flags & 0x0001) rd_s16b(&l_ptr->r_sights);
	if (save_flags & 0x0002) rd_s16b(&l_ptr->r_deaths);
	if (save_flags & 0x0004) rd_s16b(&l_ptr->r_pkills);
	if (save_flags & 0x0008) rd_s16b(&l_ptr->r_tkills);

	/* Count wakes and ignores */
	if (save_flags & 0x0010) rd_byte(&l_ptr->r_wake);
	rd_byte(&l_ptr->r_ignore);

	/* Count drops */
	rd_byte(&l_ptr->r_drop_gold);
	rd_byte(&l_ptr->r_drop_item);

	/* Count spells */
	if (save_flags & 0x0020) rd_byte(&l_ptr->r_cast);

	/* Count blows of each type */
	if (save_flags & 0x0040) rd_byte(&l_ptr->r_blows[0]);
	if (save_flags & 0x0080) rd_byte(&l_ptr->r_blows[1]);
	if (save_flags & 0x0100) rd_byte(&l_ptr->r_blows[2]);
	if (save_flags & 0x0200) rd_byte(&l_ptr->r_blows[3]);

	/* Memorize flags */
	if (save_flags & 0x0400) rd_u32b(&l_ptr->flags1);
	if (save_flags & 0x0800) rd_u32b(&l_ptr->flags2);
	if (save_flags & 0x1000) rd_u32b(&l_ptr->flags3);
	if (save_flags & 0x2000) rd_u32b(&l_ptr->s_flags1);
	if (save_flags & 0x4000) rd_u32b(&l_ptr->s_flags2);
	if (save_flags & 0x8000) rd_u32b(&l_ptr->s_flags3);

	if (!unique)
	{
		monster_race *r_ptr = &r_info[idx];

		/* Repair the lore flags */
		l_ptr->flags1 &= r_ptr->flags1;
		l_ptr->flags2 &= r_ptr->flags2;
		l_ptr->flags3 &= r_ptr->flags3;
		l_ptr->flags4 &= r_ptr->flags4;
		l_ptr->s_flags1 &= r_ptr->s_flags1;
		l_ptr->s_flags2 &= r_ptr->s_flags2;
		l_ptr->s_flags3 &= r_ptr->s_flags3;

		rd_byte(&r_ptr->cur_unique);
	}
	else
	{
		monster_unique *u_ptr = &u_info[idx];

		/* Repair the lore flags */
		l_ptr->flags1 &= u_ptr->flags1;
		l_ptr->flags2 &= u_ptr->flags2;
		l_ptr->flags3 &= u_ptr->flags3;
		l_ptr->flags4 &= u_ptr->flags4;
		l_ptr->s_flags1 &= u_ptr->s_flags1;
		l_ptr->s_flags2 &= u_ptr->s_flags2;
		l_ptr->s_flags3 &= u_ptr->s_flags3;
	}
}

/*
 * Read a "trap" record
 */
static void rd_trap(trap_type *t_ptr)
{
	byte tmp8u;

	rd_s16b(&t_ptr->w_idx);
	rd_byte(&t_ptr->fy);
	rd_byte(&t_ptr->fx);
	rd_byte(&t_ptr->charges);
	rd_byte(&t_ptr->spot_factor);

	rd_byte(&tmp8u);
	t_ptr->visible = (tmp8u & 0x01) ? TRUE: FALSE;
}

/*
 * Read a store
 */
static errr rd_store(int n)
{
	store_type *st_ptr = &store[n];

	int j;

	byte num;

	/* Read the basic info */
	rd_byte(&st_ptr->owner);
	rd_byte(&num);

	/* Paranoia */
	if (st_ptr->owner >= z_info->b_max)
	{
		note("Illegal store owner!");
		return (-1);
	}

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
	for (n = 0; n < 4; n++) rd_u16b(&flag16[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_BIRTH; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if (options_birth[i].text)
		{
			/* Set flag */
			if (flag16[os] & (1L << ob)) op_ptr->opt_birth[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_birth[i] = FALSE;

			/* Set flag */
			if (flag16[os + 2] & (1L << ob)) op_ptr->opt_adult[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_adult[i] = FALSE;
		}
	}

	/*** Cheating and scoring Options ***/

	/* Read the option flags */
	for (n = 0; n < 2; n++) rd_u16b(&flag16[n]);

	/* Analyze the options */
	for (i = 0; i < OPT_CHEAT; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if (options_cheat[i].text)
		{
			/* Set flag */
			if (flag16[os] & (1L << ob)) op_ptr->opt_cheat[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_cheat[i] = FALSE;

			/* Set flag */
			if (flag16[os+1] & (1L << ob)) op_ptr->opt_score[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_score[i] = FALSE;
		}
	}
	
	/* Squelch bytes */
	for (i = 0; i < MAX_SQ_TYPES; i++) rd_byte(&op_ptr->squelch_level[i]);

	/* Read the option flags */
	rd_u16b(&flag16[0]);

	/* Analyze the options */
	for (i = 0; i < OPT_SQUELCH; i++)
	{
		int os = i / 16;
		int ob = i % 16;

		/* Process real entries */
		if (options_squelch[i].text)
		{
			/* Set flag */
			if (flag16[os] & (1L << ob)) op_ptr->opt_squelch[i] = TRUE;
			/* Clear flag */
			else op_ptr->opt_squelch[i] = FALSE;
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

	rd_string(op_ptr->full_name, sizeof(op_ptr->full_name));
	rd_string(p_ptr->died_from, sizeof(p_ptr->died_from));
	rd_string(p_ptr->history, sizeof(p_ptr->history));

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

	/* Special Race/Class info */
	rd_byte(&p_ptr->hitdie);
	rd_u16b(&p_ptr->expfact);

	/* Age/Height/Weight */
	rd_s16b(&p_ptr->age);
	rd_s16b(&p_ptr->ht);
	rd_s16b(&p_ptr->ht_birth);
	rd_s16b(&p_ptr->wt);
	rd_s16b(&p_ptr->wt_birth);

	/* Read the stat info */
	for (i = 0; i < A_MAX; i++) 
	{
		rd_byte(&p_ptr->stat_max[i]);
		rd_byte(&p_ptr->stat_cur[i]);
		rd_byte(&p_ptr->stat_birth[i]);
	}

	rd_u16b(&p_ptr->fame);

	rd_s32b(&p_ptr->au);
	rd_s32b(&p_ptr->au_birth);

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
	
	rd_s16b(&p_ptr->wound_vigor);
	rd_s16b(&p_ptr->wound_wit);
	rd_s16b(&p_ptr->wound_grace);

	rd_s16b(&p_ptr->lore_uses);
	rd_s16b(&p_ptr->reserves_uses);
	rd_s16b(&p_ptr->escapes_uses);

	rd_s16b(&p_ptr->obsession_bonus_a);
	rd_s16b(&p_ptr->conflict_bonus_a);
	rd_s16b(&p_ptr->purity_bonus_a);
	rd_s16b(&p_ptr->transformation_bonus_a);
	rd_s16b(&p_ptr->deceit_bonus_a);

	rd_s16b(&p_ptr->obsession_bonus_b);
	rd_s16b(&p_ptr->conflict_bonus_b);
	rd_s16b(&p_ptr->purity_bonus_b);
	rd_s16b(&p_ptr->transformation_bonus_b);
	rd_s16b(&p_ptr->deceit_bonus_b);

	rd_s16b(&p_ptr->obsession_status);
	rd_s16b(&p_ptr->conflict_status);
	rd_s16b(&p_ptr->purity_status);
	rd_s16b(&p_ptr->transformation_status);
	rd_s16b(&p_ptr->deceit_status);

	rd_s16b(&p_ptr->shape);
	rd_s16b(&p_ptr->shape_timer);

	rd_s16b(&p_ptr->max_lev);
	rd_s16b(&p_ptr->max_depth);
	rd_s16b(&p_ptr->min_depth);

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
	rd_s16b(&p_ptr->energy);
	rd_s16b(&p_ptr->fast);
	rd_s16b(&p_ptr->slow);
	rd_s16b(&p_ptr->afraid);
	rd_s16b(&p_ptr->cut);
	rd_s16b(&p_ptr->stun);
	rd_s16b(&p_ptr->poisoned);
	rd_s16b(&p_ptr->diseased);
	rd_s16b(&p_ptr->taint);
	rd_s16b(&p_ptr->image);
	rd_s16b(&p_ptr->protevil);
	rd_s16b(&p_ptr->protchaos);
	rd_s16b(&p_ptr->flaming_hands);
	rd_s16b(&p_ptr->icy_hands);
	rd_s16b(&p_ptr->resilient);
	rd_s16b(&p_ptr->absorb);
	rd_s16b(&p_ptr->hero);
	rd_s16b(&p_ptr->rage);
	rd_s16b(&p_ptr->shield);
	rd_s16b(&p_ptr->blessed);
	rd_s16b(&p_ptr->tim_see_invis);
	rd_s16b(&p_ptr->safety);
	rd_s16b(&p_ptr->tim_sp_dam);
	rd_s16b(&p_ptr->tim_sp_dur);
	rd_s16b(&p_ptr->word_recall);
	rd_s16b(&p_ptr->see_infra);
	rd_s16b(&p_ptr->tim_infra);
	rd_s16b(&p_ptr->tim_stealth);
	rd_s16b(&p_ptr->tim_invis);
	rd_s16b(&p_ptr->tim_bravery);
	rd_s16b(&p_ptr->stability);
	rd_s16b(&p_ptr->racial_power);
	rd_s16b(&p_ptr->mapping_bonus);
	rd_s16b(&p_ptr->phlogiston);
	rd_s16b(&p_ptr->fortification);
	rd_s16b(&p_ptr->nightsight);
	rd_s16b(&p_ptr->fencing);
	rd_s16b(&p_ptr->archery);
	rd_s16b(&p_ptr->alertness);
	rd_s16b(&p_ptr->recall_y);
	rd_s16b(&p_ptr->recall_x);
	rd_s16b(&p_ptr->nexus_y);
	rd_s16b(&p_ptr->nexus_x);

	rd_s16b(&p_ptr->monster_counter);
	rd_s16b(&p_ptr->stylea);
	rd_s16b(&p_ptr->styleb);
	rd_s16b(&p_ptr->stylec);
	rd_s16b(&p_ptr->styled);
	rd_s16b(&p_ptr->stylee);
	rd_s16b(&p_ptr->stylef);

	/* Read resistances */
	for (i = 0; i < RS_MAX; i++) rd_s16b(&p_ptr->tim_res[i]);

	rd_s16b(&p_ptr->tim_see_invis_perm);
	rd_s16b(&p_ptr->tim_invis_perm);
	rd_s16b(&p_ptr->tim_infra_perm);
	rd_s16b(&p_ptr->tim_stealth_perm);
	rd_s16b(&p_ptr->fast_perm);
	rd_s16b(&p_ptr->absorb_perm);
	rd_s16b(&p_ptr->protevil_perm);
	rd_s16b(&p_ptr->protchaos_perm);
	rd_s16b(&p_ptr->flaming_hands_perm);
	rd_s16b(&p_ptr->icy_hands_perm);
	rd_s16b(&p_ptr->resilient_perm);
	rd_s16b(&p_ptr->hero_perm);
	rd_s16b(&p_ptr->rage_perm);
	rd_s16b(&p_ptr->blessed_perm);
	rd_s16b(&p_ptr->safety_perm);
	rd_s16b(&p_ptr->shield_perm);
	rd_s16b(&p_ptr->stability_perm);
	rd_s16b(&p_ptr->tim_bravery_perm);
	rd_s16b(&p_ptr->sp_dur_perm);
	rd_s16b(&p_ptr->tim_sp_dam_perm);
	rd_s16b(&p_ptr->tim_sp_inf_perm);
	for (i = 0; i < RS_MAX; i++) rd_s16b(&p_ptr->tim_res_perm[i]);

	rd_byte(&tmp8u);

	p_ptr->searching = (tmp8u & 0x01) ? TRUE: FALSE;
	p_ptr->hear_invis = (tmp8u & 0x02) ? TRUE: FALSE;

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

	/* Read "feeling" */
	rd_byte(&p_ptr->feeling);

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
	if (tmp16u1 > SV_BOOK_MAX)
	{
		note(format("Too many (%u) spellbook entries!", tmp16u1));
		return (-1);
	}
	if (tmp16u2 > SV_BOOK_MAX * MAX_BOOK_SPELLS)
	{
		note(format("Too many (%u) ordered spells!", tmp16u2));
		return (-1);
	}

	/* Read spell info */
	for (i =0; i < tmp16u1; i++)
	{
		rd_byte(&tmp8u); /* The spellbook's index */
		rd_u16b(&p_ptr->spell_learned[tmp8u]);
		rd_u16b(&p_ptr->spell_forgotten[tmp8u]);
	}
	for (i = 0; i < SV_BOOK_MAX * MAX_BOOK_SPELLS; i++)
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
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(void)
{
	int slot = 0;

	object_type *i_ptr;
	object_type object_type_body;

	/* Read until done */
	while (TRUE)
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
		if (n >= INVEN_MAX) return (-1);

		/* Wield equipment */
		if (n >= INVEN_WIELD)
		{
			/* Copy object */
			object_copy(&inventory[n], i_ptr);

			/* Add the weight */
			p_ptr->total_weight += (i_ptr->number * object_weight(i_ptr));

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
			p_ptr->total_weight += (i_ptr->number * object_weight(i_ptr));

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
		rd_string(buf, sizeof(buf));

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
	int i, j, y, x;

	s16b depth;
	s16b py, px;

	byte count;
	byte tmp8u;

	u16b limit;

	/*** Basic info ***/

	/* Header info */
	rd_s16b(&depth);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_byte(&p_ptr->cur_map_hgt);
	rd_byte(&p_ptr->cur_map_wid);

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
	if ((px < 0) || (px >= p_ptr->cur_map_wid) || (py < 0) || (py >= p_ptr->cur_map_hgt))
	{
		note(format("Ignoring illegal player location (%d,%d).", py, px));
		return (-1);
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
			/* Extract "feat" */
			cave_set_feat(y, x, tmp8u);

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

	/* Save depth */
	p_ptr->depth = depth;

	/* Place player in dungeon */
	if (!player_place(py, px))
	{
		note(format("Cannot place player (%d,%d)!", py, px));
		return (-1);
	}

	/*** Room Descriptions ***/

	for (x = 0; x < MAX_ROOMS_ROW; x++)
	{
		for (y = 0; y < MAX_ROOMS_COL; y++)
		{
			rd_byte(&dun_room[x][y]);
		}
	}

	for (i = 1; i < DUN_ROOMS; i++)
	{
		rd_byte(&room_info[i].type);
		rd_byte(&tmp8u);

		room_info[i].seen = (tmp8u & 0x01) ? TRUE: FALSE;

		if (room_info[i].type == ROOM_NORMAL)
		{
			for (j = 0; j < ROOM_DESC_SECTIONS; j++)
			{
				rd_s16b(&room_info[i].section[j]);

				if (room_info[i].section[j] == -1) break;
			}
		}
	}
	
	/*** Objects ***/

	/* Read the item count */
	rd_u16b(&limit);

	/* Verify maximum */
	if (limit >= z_info->o_max)
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
		}
	}

	/*** Monsters ***/

	/* Read the monster count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= z_info->m_max)
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
		m_ptr = &mon_list[o_ptr->held_m_idx];

		/* Link the object to the pile */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Link the monster to the object */
		m_ptr->hold_o_idx = i;
	}

	/*** Traps ***/

	/* Read the trap count */
	rd_u16b(&limit);

	/* Hack -- verify */
	if (limit >= z_info->t_max)
	{
		note(format("Too many (%d) trap entries!", limit));
		return (-1);
	}

	/* Read the traps */
	for (i = 1; i < limit; i++)
	{
		trap_type *t_ptr;
		trap_type trap_type_body;

		/* Get local trap */
		t_ptr = &trap_type_body;

		/* Clear the trap */
		(void)WIPE(t_ptr, trap_type);

		/* Read the trap */
		rd_trap(t_ptr);

		/* Place trap in dungeon */
		if (trap_place(t_ptr->fy, t_ptr->fx, t_ptr) != i)
		{
			note(format("Cannot place trap %d", i));
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
	int i, j;

	byte tmp8u;
	u16b tmp16u;

	u32b n_x_check, n_v_check;
	u32b o_x_check, o_v_check;

	/* Mention the savefile version */
	note(format("Loading a %d.%d.%d savefile...", sf_major, sf_minor, sf_patch));

	if (older_than(0,5,1))
	{
		note("Not compatible with 0.5.0 or older savefiles!");
		return (-1);
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
	if (tmp16u > z_info->u_max)
	{
		note(format("Too many (%u) unique monsters!", tmp16u));
		return (-1);
	}

	/* Read the unique info */
	for (i = 0; i < tmp16u; i++)
	{
		monster_unique *u_ptr = &u_info[i];

		rd_byte(&tmp8u);
		u_ptr->dead = (tmp8u & 0x01) ? TRUE: FALSE;

		rd_s16b(&u_ptr->depth);

		rd_lore(TRUE, i);
	}
	
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
		rd_lore(FALSE, i);
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
		k_ptr->squelch = (tmp8u & 0x04) ? TRUE: FALSE;
		k_ptr->everseen = (tmp8u & 0x08) ? TRUE: FALSE;
	}

	if (arg_fiddle) note("Loaded Object Memory");

	/* Load the alchemy info */
	rd_u16b(&tmp16u);
	if (tmp16u != SV_POTION_MAX)
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

		if ((q_info[i].type == QUEST_FIXED) || (q_info[i].type == QUEST_FIXED_U))
		{
			rd_byte(&q_info[i].active_level);
			rd_s16b(&q_info[i].cur_num);
		}
		else if ((q_info[i].type == QUEST_GUILD) || (q_info[i].type == QUEST_UNIQUE))
		{
			rd_byte(&q_info[i].reward);
			rd_byte(&q_info[i].active_level);
			rd_byte(&q_info[i].base_level);

			rd_s16b(&q_info[i].mon_idx);
			rd_s16b(&q_info[i].cur_num);
			rd_s16b(&q_info[i].max_num);

			rd_byte(&tmp8u);
			q_info[i].started = (tmp8u) ? TRUE : FALSE;

			/* Set current quest */
			if (q_info[i].active_level || q_info[i].reward) 
				p_ptr->cur_quest = q_info[i].base_level;
		}
		else if (q_info[i].type == QUEST_VAULT)
		{
			rd_byte(&q_info[i].reward);
			rd_byte(&q_info[i].active_level);
			rd_byte(&q_info[i].base_level);

			/* Set current quest */
			if (q_info[i].active_level || q_info[i].reward) 
				p_ptr->cur_quest = q_info[i].base_level;
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
		rd_byte(&a_info[i].status);
	}
	if (arg_fiddle) note("Loaded Artifacts");

	/* Read the extra stuff */
	if (rd_extra()) return (-1);
	if (arg_fiddle) note("Loaded extra information");

	/* Read the spell stuff */
	if (rd_spells()) return (-1);
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
		message(MSG_GENERIC, 0, "Savefile does not exist.");
		message_flush();

		/* Allow this */
		return (TRUE);
	}

	/* Close the file */
	fd_close(fd);

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
		if (fd_read(fd, (char*)(vvv), 4)) err = -1;

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

		err = rd_savefile();

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
			/* Message */
			message_format(MSG_GENERIC, 0, "Converted a %d.%d.%d savefile.",
			           sf_major, sf_minor, sf_patch);
			message_flush();
		}

		/* Player is dead */
		if (p_ptr->is_dead)
		{
			/* In wizard mode, allow loading of tombstones */
			if (cheat_wizard)
			{
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

			/* A character once existed on this savefile */
			character_existed = TRUE;

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

	/* Message */
		message_format(MSG_GENERIC, 0, "Error (%s) reading %d.%d.%d savefile.",
			what, sf_major, sf_minor, sf_patch);	message_flush();

	/* Oops */
	return (FALSE);
}

