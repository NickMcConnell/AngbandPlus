#define OBJECT1_C
/* File: object1.c */

/* Purpose: Object code, part 1 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Determine the u_idx of an item with a given k_idx.
 *
 * Return -2 if no match is found.
 */
s16b lookup_unident(byte p_id, byte s_id)
{
	s16b i;

	for (i = 0; i < MAX_U_IDX; i++)
		{
		unident_type *u_ptr = &u_info[i];
		if (u_ptr->p_id != p_id) continue;
		if (u_ptr->s_id != s_id) continue;
		return i;
	}
	return -2;
}


/*
 * Give a name to a scroll.
 *
 * out_string must be able to accept length characters.
 */
static void get_scroll_name(char * out_string, uint length)
{
	(*out_string) = '\0';
	/* Collect words until done */
	while (1)
	{
		cptr str = "", file = "scroll.txt";
		int num = ((rand_int(100) < 30) ? 1 : 2);

		/* Add a one or two syllable word */
		while (num--)
		{
			/* Add the syllable */
			str = format("%s%v", str, get_rnd_line_f1, file);
		}

		/* Stop before getting too long */
		if (strlen(out_string) + 1 + strlen(str) > length) return;

		/* Add a space */
		if (strlen(out_string)) strcat(out_string, " ");

		/* Add the word */
		strcat(out_string, str);
	}
}

/*
 * Randomly generate names where appropriate.
 */
static void name_scrolls(void)
{
	int i, j, scrolls;
	C_TNEW(scroll_idx, z_info->u_max, u16b);

	/* Iterate through the scrolls, renaming each in turn. */
	for (i = 0, scrolls = -1; i < z_info->u_max; i++)
	{
		unident_type *u_ptr = &u_info[i];
		uint length;
		char *s = u_name+u_ptr->name;

		/* Ignore non-scrolls. */
		if (*s != CH_SCROLL) continue;

		/* Recognise IS_BASE scrolls. */
		if (*(s+1) == CH_IS_BASE)
		{
			*(s++) = CH_IS_BASE;
		}

		/* Make a note of this scroll. */
		scroll_idx[++scrolls] = i;

		/* Notice the maximum length. */
		length = strlen(s);

		do
		{
			/* Get a name */
			get_scroll_name(s, length);

			/* Check for duplicate names (first 4 characters) */
			for (j = 0; j < scrolls; j++)
			{
				unident_type *u2_ptr = &u_info[scroll_idx[j]];

				/* Ignore different scrolls. */
				if (strncmp(u_name+u_ptr->name, u_name+u2_ptr->name, 4)) continue;

				/* Too close. */
				break;
			}
		} while (j < scrolls);
	}

	TFREE(scroll_idx);
}



/*
 * Assign unidentified descriptions to objects.
 */
void flavor_init(void)
{
	int i,j;
	int base_only[256];
	C_TNEW(k_ptr_p_id, z_info->k_max, int);

	/* The default value of base_only is -1 so that a map from [0,255]
	 * is obvious. */
	for (i = 0; i < 256; i++) base_only[i] = -1;

	/* The p_id is stored in the u_idx field in init_k_info,
	 * so extract it to a separate array to avoid confusion. */
	for (i = 0; i < z_info->k_max; i++)
	{
		k_ptr_p_id[i] = k_info[i].u_idx;
	}

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant flavors */
	Rand_value = seed_flavor;

	/* Give names to the scroll entries in u_info. */
	name_scrolls();

	/*
	 * Then set the u_idxs of flavourless items appropriately.
	 */
	for (i = 0; i < z_info->u_max; i++)
	{
		unident_type *u_ptr = &u_info[i];

		if (!u_ptr->name)
			base_only[u_ptr->p_id] = i;
	}
	for (i = 0; i < z_info->k_max; i++)
	{
		if (base_only[k_ptr_p_id[i]] != -1)
			k_info[i].u_idx = base_only[k_ptr_p_id[i]];
	}
	/*
	 * Finally, give each k_idx with a variable description a valid s_id.
	 * We've already checked that there are enough to go around.
	 */
	for (i = 0; i < 256; i++)
	{
		s16b k_top, u_top;
		C_TNEW(k_idx, z_info->k_max, s16b);
		C_TNEW(u_idx, z_info->u_max, s16b);

		/* Ignore flavourless p_ids */
		if (base_only[i] != -1) continue;

		/* Count all entried with this p_id in k_info */
		for (j = 0, k_top = 0; j < z_info->k_max; j++)
		{
			if (k_ptr_p_id[j] == i) k_idx[k_top++] = j;
		}

		/* And all those in u_info */
		for (j = 0, u_top = 0; j < z_info->u_max; j++)
		{
			if (u_info[j].p_id == i) u_idx[u_top++] = j;
		}

		for (j = 0; j < k_top; j++)
		{
			/* Pick a description from the list. */
			s16b k = rand_int(u_top--);

			/* Assign it to the k_idx in question */
			k_info[k_idx[j]].u_idx = u_idx[k];

			/* Remove it from further consideration. */
			u_idx[k] = u_idx[u_top];
		}

		TFREE(k_idx);
		TFREE(u_idx);
	}

	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	TFREE(k_ptr_p_id);
}


/*
 * Hack -- prepare the default object attr codes by tval
 *
 * XXX XXX XXX Off-load to "pref.prf" file
 */
static byte default_tval_to_attr(int tval)
{
	switch (tval)
	{
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		{
			return (TERM_WHITE);
		}

		case TV_CHEST:
		{
			return (TERM_SLATE);
		}

		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			return (TERM_L_UMBER);
		}

		case TV_LITE:
		{
			return (TERM_YELLOW);
		}

		case TV_SPIKE:
		{
			return (TERM_SLATE);
		}

		case TV_BOW:
		{
			return (TERM_UMBER);
		}

		case TV_DIGGING:
		{
			return (TERM_SLATE);
		}

		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (TERM_L_WHITE);
		}

		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_CLOAK:
		{
			return (TERM_L_UMBER);
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			return (TERM_SLATE);
		}

		case TV_AMULET:
		{
			return (TERM_ORANGE);
		}

		case TV_RING:
		{
	    return (TERM_ORANGE);
		}

		case TV_STAFF:
		{
	    return (TERM_VIOLET);
		}

		case TV_WAND:
		{
	    return (TERM_VIOLET);
		}

		case TV_ROD:
		{
	    return (TERM_VIOLET);
		}

		case TV_SCROLL:
		{
			return (TERM_WHITE);
		}

		case TV_POTION:
		{
	    return (TERM_BLUE);
		}

		case TV_FLASK:
		{
			return (TERM_YELLOW);
		}

		case TV_FOOD:
		{
	    return (TERM_GREEN);
		}

	case TV_SORCERY_BOOK:
		{
	    return (TERM_L_BLUE);
		}

	case TV_THAUMATURGY_BOOK:
		{
			return (TERM_L_RED);
		}

    case TV_CONJURATION_BOOK:
        {
            return (TERM_ORANGE);
        }
    case TV_NECROMANCY_BOOK:
        {
            return (TERM_SLATE);
        }
	}

	return (TERM_WHITE);
}


/*
 * Hack -- prepare the default object char codes by tval
 *
 * XXX XXX XXX Off-load to "pref.prf" file (?)
 */
static byte default_tval_to_char(int tval)
{
	int i;

	/* Hack -- Guess at "correct" values for tval_to_char[] */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Use the first value we find */
		if (k_ptr->tval == tval) return (k_ptr->d_char);
	}

	/* Default to space */
	return (' ');
}



/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default"
 * state, and then loading the appropriate "user pref file"
 * based on the "use_graphics" flag.
 *
 * This is useful for switching "graphics" on/off
 */
void reset_visuals(void)
{
	int i;

	u16b sf_flags[MAX_SF_VAR];

	char buf[1024];

	current_flags(sf_flags);

	/* Reset various attr/char maps. */
	process_pref_file_aux((char*)"F:---reset---", sf_flags);
	process_pref_file_aux((char*)"K:---reset---", sf_flags);
	process_pref_file_aux((char*)"U:---reset---", sf_flags);
	process_pref_file_aux((char*)"R:---reset---", sf_flags);

	/* Extract some info about monster memory colours. */
	for (i = 0; i < MAX_MONCOL; i++)
	{
		/* Hack - always default to white */
		moncol[i].attr = TERM_WHITE;
	}
	/* Extract attr/chars for equippy items (by tval) */
	for (i = 0; i < 128; i++)
	{
		/* Extract a default attr */
		tval_to_attr[i] = default_tval_to_attr(i);

		/* Extract a default char */
		tval_to_char[i] = default_tval_to_char(i);
	}

	/* Access the "font" or "graf" pref file, based on "use_graphics" */
	sprintf(buf, "%s-%s.prf", (use_graphics ? "graf" : "font"), ANGBAND_SYS);

	/* Process that file */
	process_pref_file(buf);
}









/*
 * Obtain the "flags" for an item
 */
void object_flags(object_ctype *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Base object */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		(*f1) = a_ptr->flags1;
		(*f2) = a_ptr->flags2;
		(*f3) = a_ptr->flags3;
	}

	/* Ego-item */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		(*f1) |= e_ptr->flags1;
		(*f2) |= e_ptr->flags2;
		(*f3) |= e_ptr->flags3;
	}

    /* Random artifact ! */
    if (o_ptr->flags1 || o_ptr->flags2 || o_ptr->flags3)
    {
		(*f1) |= o_ptr->flags1;
		(*f2) |= o_ptr->flags2;
		(*f3) |= o_ptr->flags3;

		/* Hack - cursing an object via flags3 totally supercedes default values */
		if ((o_ptr->flags3 & TR3_CURSED) && !(o_ptr->flags3 & TR3_HEAVY_CURSE))
			*f3 &= ~TR3_HEAVY_CURSE;
	}
}

/* A custom type to tell the player what he knows about an object. */
typedef struct obj_know_type obj_know_type;
struct obj_know_type
{
	object_type obj[1]; /* The fields of an object_type are known. */
	bool u_idx; /* The unidentified representation is known. */
	bool p_id; /* The unidentified description category is known. */
};

/*
 * This uses the obj_know_type structure to indicate the knowledge the player
 * has of o_ptr.
 *
 * know_ptr will not contain a real object, but marks each value according to
 * the knowledge the player has about it.
 *
 * If a value is set to 1, the player knows that it has its current state.
 * If a value is set to 0, the player knows nothing about it.
 *
 * The flags[n] fields are described on a bit-by-bit basis, and refer to the
 * information derived from object_flags() rather than merely those stored in
 * o_ptr.
 */
static void object_knowledge(obj_know_type *ok_ptr, object_ctype *o_ptr)
{
	int i;
	bool cheat = cheat_item;
	bool full = (o_ptr->ident & IDENT_MENTAL);
	bool known = object_known_p(o_ptr);
	bool aware = object_aware_p(o_ptr);
	bool shop = (o_ptr->ident & IDENT_STORE) != 0;
	object_type *j_ptr = ok_ptr->obj;

	/* Restrict cheating with a couple of compile-time options. */
#ifndef SPOIL_ARTIFACTS
	if (allart_p(o_ptr)) cheat = FALSE;
#endif
#ifndef SPOIL_EGO_ITEMS
	if (ego_item_p(o_ptr)) cheat = FALSE;
#endif

	/* Cheaters know everything. */
	if (cheat) full = TRUE;

	/* Fully known objects are always known. */
	if (full) known = TRUE;

	/* The player is always aware of known items. */
	if (known) aware = TRUE;

	/* Shop items are fully known, but not generally aware. */
	if (shop) full = known = TRUE;

	/* Assume complete ignorance by default. */
	WIPE(ok_ptr, obj_know_type);

	ok_ptr->p_id = 1;

	ok_ptr->u_idx = aware || !shop;

	j_ptr->k_idx = aware || shop;

	j_ptr->iy = 1;
	j_ptr->ix = 1;
	j_ptr->tval = (aware ||
		o_base[u_info[k_info[o_ptr->k_idx].u_idx].p_id].tval == o_ptr->tval);
	j_ptr->discount = 1;
	j_ptr->number = 1;
	j_ptr->marked = 1;

	j_ptr->name1 = known;
	j_ptr->name2 = known;

	j_ptr->to_h = known;
	j_ptr->to_d = known;

	j_ptr->to_a = known;

	j_ptr->ac = aware;

	j_ptr->dd = j_ptr->ds = aware;

	j_ptr->pval = known;

	/* Hack - this should distinguish between knowing timeout and knowing
	 * whether timeout is 0. Only the latter actually happens, so I've kept
	 * it simple. */
	j_ptr->timeout = 1;

	j_ptr->weight = 1;

	j_ptr->ident = IDENT_FIXED | IDENT_EMPTY | IDENT_KNOWN | IDENT_STOREB |
		IDENT_MENTAL | IDENT_TRIED | IDENT_STORE | IDENT_SENSE_CURSED |
		IDENT_SENSE_VALUE | IDENT_SENSE_HEAVY | IDENT_TRIED | IDENT_HIDDEN;

	/* Hack - curses are stored in ident, but are unconnected. */
	if (o_ptr->ident & IDENT_SENSE_CURSED) j_ptr->ident |= IDENT_CURSED;

	/* Hack - IDENT_BROKEN is fiddled with at various points during the game. */
	if (o_ptr->ident & IDENT_SENSE_VALUE) j_ptr->ident |= IDENT_BROKEN;

	j_ptr->note = 1;

	j_ptr->art_name = known;

	j_ptr->next_o_idx = 1;
	j_ptr->held_m_idx = 1;

	if (full)
	{
		/* The player knows everything except that the object has extra random
		 * (and now known) flags. */
		j_ptr->flags1 = ~(0L);
		j_ptr->flags2 = ~(TR2_RAND_RESIST | TR2_RAND_POWER | TR2_RAND_EXTRA);
		j_ptr->flags3 = ~(0L);

		j_ptr->activation = j_ptr->pval = 1;
	}
	else
	{
		u32b f1, f2, f3;

		/* Obtain the full flags for o_ptr first. */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Start from no knowledge. */
		j_ptr->flags1 = 0;
		j_ptr->flags2 = 0;
		j_ptr->flags3 = 0;

		/* There are some flags unaware items can be known to have. */
		if (spoil_base && !aware)
		{
			o_base_type *ptr = o_base+u_info[k_info[o_ptr->k_idx].u_idx].p_id;
			j_ptr->flags1 |= ptr->flags1;
			j_ptr->flags2 |= ptr->flags2;
			j_ptr->flags3 |= ptr->flags3;
		}
		/* Flags known from the base object. */
		if (spoil_base && aware)
		{
			object_kind *ptr = k_info+o_ptr->k_idx;
			j_ptr->flags1 |= ptr->flags1;
			j_ptr->flags2 |= ptr->flags2;
			j_ptr->flags3 |= ptr->flags3;
		}
		/* Flags known from the ego type. */
		if (spoil_ego && known)
		{
			ego_item_type *ptr = e_info+o_ptr->name2;
			j_ptr->flags1 |= ptr->flags1;
			j_ptr->flags2 |= ptr->flags2;
			j_ptr->flags3 |= ptr->flags3;
		}
		/* Flags known from the artefact type. */
		if (spoil_art && known)
		{
			artifact_type *ptr = a_info+o_ptr->name1;
			j_ptr->flags1 |= ptr->flags1;
			j_ptr->flags2 |= ptr->flags2;
			j_ptr->flags3 |= ptr->flags3;
		}
		/* Hack - flags known for random artefacts. */
		if (o_ptr->art_name && spoil_art)
		{
			/* All that is known about randarts is that they ignore elements. */
			j_ptr->flags3 |= TR3_IGNORE_ALL;
		}

		/* Hack - this is all a poor attempt to determine if it changed anything
		 * when the player tried it. It should track this sort of thing... */
		if (spoil_flag && o_ptr->ident & IDENT_TRIED)
		{
			/* Obvious stat effects. */
			for (i = 0; i < A_MAX; i++)
			{
				if (p_ptr->stat_top[i] != 3 || equip_mod(i) < 0)
				{
					j_ptr->flags1 |= TR1_STR << i;
				}
			}

			/* Obvious extra blows. */
			if ((o_ptr->pval < 0 && p_ptr->num_blow != 600) ||
				(o_ptr->pval > 0 && p_ptr->num_blow != 60))
			{
				j_ptr->flags1 |= TR1_BLOWS;
			}

			/* Hack - assume that some other flags are always obvious. */
			j_ptr->flags3 |= TR3_LITE | TR3_XTRA_SHOTS | TR3_TELEPATHY;

			/* Don't assume the normal multiplier is always known, though. */
			if (spoil_base) j_ptr->flags3 |= TR3_XTRA_MIGHT;

			/* Hack - Set flags known through cumber_*(). Luckily, a mysterious
			 * failure to encumber the player can only mean one thing. */
			if (wield_slot(o_ptr) == INVEN_HANDS)
			{
				/* See cumber_glove(). The object is known not to boost dex. */
				if ((j_ptr->flags1 & ~f1 & TR1_DEX) ||
					(j_ptr->pval && o_ptr->pval <= 0))
					j_ptr->flags2 |= TR2_FREE_ACT;
			}
			else if (i == INVEN_HEAD)
			{
				/* See cumber_helm(). The object is known not to boost wis. */
				if ((j_ptr->flags1 & ~f1 & TR1_WIS) ||
					(j_ptr->pval && o_ptr->pval <= 0))
					j_ptr->flags3 |= TR3_TELEPATHY;
			}
		}

		/* Cursing uses a separate ident flag. */
		if (o_ptr->ident & IDENT_SENSE_CURSED)
		{
			j_ptr->flags3 |= TR3_CURSED;

			/* Uncursed items are uncursed all the way. */
			if (!cursed_p(o_ptr))
			{
				j_ptr->flags3 |= TR3_HEAVY_CURSE | TR3_PERMA_CURSE;
			}
		}

		/* If a pval flag is known to be set, the pval itself is known. */
		if (f1 & TR1_PVAL_MASK & j_ptr->flags1) j_ptr->pval = 1;

		/* If an activation is known to exist, magically know what it is. */
		j_ptr->activation = !!(f3 & TR3_ACTIVATE);

		/* Special "always show these" flags. */
		if (f3 & TR3_SHOW_ARMOUR) j_ptr->ac = 1;
		if (f3 & TR3_SHOW_MODS) j_ptr->dd = j_ptr->ds = 1;

		/* Hack - resist_chaos by any means gives resist_conf. */
		if (j_ptr->flags2 & f2 & TR2_RES_CHAOS) j_ptr->flags2 |= TR2_RES_CONF;
	}
}

/* 
 * A macro used below to clear a j_ptr field if the corresponding q_ptr field
 * is set to 0.
 */
#define OIK_MASK(FLAG) \
	if (!ok_ptr->obj->FLAG) j_ptr->FLAG = 0;

/* As above, but using a special "unknown" value. */
#define OIK_MASK_SPECIAL(FLAG, VALUE) \
	if (!ok_ptr->obj->FLAG) j_ptr->FLAG = (VALUE);

/*
 * Create an object containing the known information about an object based
 * on the contents of ok_ptr and the object itself, called o_ptr.
 *
 * This is intended to produce objects which can be used in the same way as
 * the original object, except that "does this object do this?" is replaced by
 * "does the player expect this object to do this?".
 *
 * As far as is practical, the functions which inspect objects should work with
 * objects created by this function in a reasonable way.
 */
static void set_known_fields(object_type *j_ptr, object_ctype *o_ptr,
	const obj_know_type *ok_ptr)
{
	/* Start with everything known. */
	object_copy(j_ptr, o_ptr);

	/* Acquire the composite flags. */
	object_flags(o_ptr, &j_ptr->flags1, &j_ptr->flags2, &j_ptr->flags3);

	/* Mask things the object is not known to possess. */
	j_ptr->flags1 &= ok_ptr->obj->flags1;
	j_ptr->flags2 &= ok_ptr->obj->flags2;
	j_ptr->flags3 &= ok_ptr->obj->flags3;

	j_ptr->ident &= ok_ptr->obj->ident;

	/* Clear parameters which are not treated as bit fields as required.
	 * As some of these are bit fields, they should be changed if the
	 * behaviour of object_knowledge() becomes more complex.
	 */
	OIK_MASK_SPECIAL(k_idx, OBJ_UNKNOWN)
	OIK_MASK(iy)
	OIK_MASK(ix)
	OIK_MASK_SPECIAL(tval, TV_UNKNOWN)
	OIK_MASK(discount)
	OIK_MASK_SPECIAL(number, UNKNOWN_OBJECT_NUMBER)
	OIK_MASK(marked)
	OIK_MASK(name1)
	OIK_MASK(name2)
	OIK_MASK(activation)

	/* OBJ_UNKNOWN has a lot of things set to safe values. */
	OIK_MASK_SPECIAL(to_h, k_info[j_ptr->k_idx].to_h)
	OIK_MASK_SPECIAL(to_d, k_info[j_ptr->k_idx].to_d)
	OIK_MASK_SPECIAL(to_a, k_info[j_ptr->k_idx].to_a)
	OIK_MASK_SPECIAL(ac, k_info[j_ptr->k_idx].ac)
	OIK_MASK_SPECIAL(pval, k_info[j_ptr->k_idx].pval)
	OIK_MASK(timeout)
	OIK_MASK(weight)
	OIK_MASK_SPECIAL(dd, k_info[j_ptr->k_idx].dd)
	OIK_MASK_SPECIAL(ds, k_info[j_ptr->k_idx].ds)
	OIK_MASK(note)
	OIK_MASK(art_name)
	OIK_MASK(next_o_idx)
	OIK_MASK(held_m_idx)
}

/*
 * Copy o_ptr to j_ptr, setting the fields which are unknown to special values
 * (usually 0).
 */
void object_info_known(object_type *j_ptr, object_ctype *o_ptr)
{
	obj_know_type ok_ptr[1];
	object_knowledge(ok_ptr, o_ptr);
	set_known_fields(j_ptr, o_ptr, ok_ptr);
}


/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(object_ctype *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_type j;
	object_info_known(&j, o_ptr);
	(*f1) = j.flags1;
	(*f2) = j.flags2;
	(*f3) = j.flags3;
}





/*
 * Parse through the string until the combination of
 * flags is compatible with the current rejection
 * criteria or the end of the string is reached.
 * Return the first "safe" character.
 */
cptr find_next_good_flag(cptr s, byte reject, byte require)
{
	int bad;

	/* If this string doesn't actually start with a flag-altering
	 * character, there is nothing to do. */
	if (!is_cx_char(*s))
	{
		return s;
	}

	for (bad = 0;;s++)
	{
		char flag = 1<<find_ci(*s);
		char mod = find_cm(*s);

		/* Ordinary characters do nothing. */
		if (*s & 0xE0);
				
		/* NULs are parsed elewhere. */
		else if (*s == '\0') return s;

		/* CH_NORM always reduces restrictions */
		else if (mod == CM_NORM)
		{
			bad &= ~(flag);
		}

		/*
		 * If a flag which isn't present is required
		 * or a flag which is present is forbidden,
		 * the set of "wrong" flags increases.
		 */
		else if (((mod == CM_TRUE) && (reject & flag)) ||
			((mod == CM_FALSE) && (require & flag)))
		{
			bad |= flag;
		}
		/*
		 * If a flag which is present is required
		 * or a flag which isn't present is forbidden,
		 * the set of "wrong" flags decreases.
		 */
		else if (((mod == CM_TRUE) && (require & flag)) ||
			((mod == CM_FALSE) && (reject & flag)))
		{
			bad &= ~flag;
		}

		if (!bad) break;
	}

	/* The loop finished with a flag-altering character, but
	 * the character after it may be printable. */
 	return s+1;
}

/*
 * Print a char "c" into a string "t", as if by sprintf(t, "%c", c),
 * and return a pointer to the terminator (t + 1).
 */
static char *object_desc_chr(char *t, char c)
{
	/* Copy the char */
	*t++ = c;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


/*
 * Print a string "s" into a string "t", as if by strcpy(t, s),
 * and return a pointer to the terminator.
 */
static char *object_desc_str(char *t, cptr s)
{
	/* Copy the string */
	while (*s) *t++ = *s++;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


static const char hidden_name[] = {CH_ARTICLE,
	'h','i','d','d','e','n',' ','t','h','i','n','g',
	CI_PLURAL+CM_TRUE, 's', CI_PLURAL+CM_NORM, '\0'};

/*
 * Creates a description of the item "o_ptr", and stores it in "out_val".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * If the name is longer than len-1 characters long, an effort is made to
 * reduce it in a nice way, e.g. bg removing the characters at the end of the
 * inscription before the terminating }.
 *
 * If "pref" then a "numeric" prefix will be pre-pended.
 *
 * Mode:
 *   0 -- The Cloak of Death
 *   1 -- The Cloak of Death [1,+3]
 *   2 -- The Cloak of Death [1,+3] (+2 to Stealth)
 *   3 -- The Cloak of Death [1,+3] (+2 to Stealth) {nifty}
 *
 * Flags:
 * Setting OD_ART gives "a Cloak" rather than "Cloak" as a description.
 * Setting OD_SHOP sets various things to reflect shop items. Object kind names
 * are always shown, and flavours and inscriptions are omitted.
 */

/* The number of strings which can be in the process of being printed at once. */
#define MAX_NAME_LEVEL	8

static void object_desc(char *buf, uint len, object_ctype *o1_ptr, byte flags,
	int mode)
{
	bool pref = (flags & OD_ART) != 0;
	bool in_shop = (flags & OD_SHOP) || (o1_ptr->ident & IDENT_STORE);

	/* The strings from which buf is compiled. */
	cptr strings[8] = {0,0,0,0,0,0,0,0};

/* If a CH_ARTICLE would cause an article to be prepended, this is set
 * to ARTICLE_REQUEST. If one is found, it is changed to ARTICLE_LETTER.
 * Once the initial name-writing loop is finished, the program searches
 * the output string for the first letter or number, and adds "a" or "an"
 * to the beginning as appropriate. */

#define ARTICLE_NO	0
#define ARTICLE_REQUEST	1
#define ARTICLE_LETTER	2

	int wants_article = ARTICLE_NO;

	int this_level = 0;

	byte reject, current;


	cptr s;

	int                     power,i;

	cptr	r[MAX_NAME_LEVEL];
	char            *t;

	/* tmp_val_base is twice as large as necessary as little bounds checking
	 * is performed. This probably isn't good enough. */
	C_TNEW(tmp_val_base, len+80, char);
	char		*tmp_val = tmp_val_base;

	object_kind	*k1_ptr;
	unident_type *u1_ptr;
	o_base_type *ob1_ptr;
	obj_know_type know_ptr[1];
	object_type o_ptr[1], *ok_ptr = know_ptr->obj;

	/* Hack - place the in_shop flag inside o_ptr. */
	if (in_shop)
	{
		/* Give the object the IDENT_STORE flag for a moment. */
		object_copy(o_ptr, o1_ptr);
		o_ptr->ident |= IDENT_STORE;

		/* Extract the known information. */
		object_knowledge(know_ptr, o_ptr);
	}
	else
	{
		/* Simply extract the known information. */
		object_knowledge(know_ptr, o1_ptr);
	}

	/* Set o_ptr according to the known information. */
	set_known_fields(o_ptr, o1_ptr, know_ptr);

	k1_ptr = &k_info[o1_ptr->k_idx];
	u1_ptr = &u_info[k1_ptr->u_idx];
	ob1_ptr = &o_base[u1_ptr->p_id];

	reject = current = 0;

	/* Acquire the names if known. */
	strings[CI_K_IDX] = (ok_ptr->k_idx) ? k_name+k1_ptr->name : "";
	strings[CI_FLAVOUR] = (know_ptr->u_idx) ? u_name+u1_ptr->name : "";
	strings[CI_BASE] = (know_ptr->p_id) ? ob_name+ob1_ptr->name : "";
	strings[CI_EGO] = (ok_ptr->name2) ? e_name+e_info[o_ptr->name2].name : "";
 	strings[CI_ARTEFACT] = (o_ptr->name1) ? a_name+a_info[o_ptr->name1].name :
 		(o_ptr->art_name) ? quark_str(o_ptr->art_name) : "";

	/* If any of the above remain unset, they shall be ignored. */
	for (i = 0; i < 8; i++)
	{
		if (strings[i] && !*strings[i]) reject |= 1 <<i;
	}

	/* Identified artefacts, and all identified objects if
	 * show_flavors is unset, do not include a FLAVOUR string. */
	if ((~reject & CI_K_IDX) && (plain_descriptions || in_shop ||
		(o_ptr->ident & IDENT_STOREB)))
		reject |= 1 << CI_FLAVOUR;

	/* Singular objects take no plural. */
	if (o_ptr->number == 1) reject |= 1 << CI_PLURAL;

	/* Hack - set k1_ptr->seen if the kind is recognised. */
	if (*strings[CI_K_IDX]) k1_ptr->seen = TRUE;

	/* Start dumping the result */
	t = tmp_val;

	/* Find start, looking for first CH_IS_BASE. */
	for (power = 7;; power--)
	{
		/* Default to CI_BASE. */
		if (power == -1)
		{
			current |= 1<< CI_BASE;
			r[this_level] = strings[CI_BASE];
			break;
		}

		s = strings[power];

		/* Reject non-strings. */
		if (!s) continue;

		/* Reject rejected strings. */
		if (reject & 1<<power) continue;

		/* Accept if initial character is CH_IS_BASE */
		if (*s == CH_IS_BASE)
		{
			current |= 1<< power;
			r[this_level] = strings[power]+1;
			break;
		}
	}

	/* Paranoia - no known starting string. */
	if (!r[this_level]) r[this_level] = "Mystery";

	/* Hack - hidden objects have a special name. */
	if (o_ptr->ident & IDENT_HIDDEN) r[this_level] = hidden_name;

	/*
	 * Add an article if required. This is only possible at the start
	 * of the output string because of the grammatical problems
	 * that are created if I do otherwise. It also makes it easier
	 * to work with.
	 */

	/* No prefix */
	if (!pref)
	{
		/* Nothing */
	}

	/* Hack -- None left */
	else if (o_ptr->number <= 0 && o_ptr->k_idx)
	{
		t = object_desc_str(t, "no more ");
	}

	else if (o_ptr->number == UNKNOWN_OBJECT_NUMBER)
	{
		t = object_desc_str(t, "some ");
	}

	/* Extract the number */
	else if (o_ptr->number > 1)
	{
		t += sprintf(t, "%d ", o_ptr->number);
	}

	/* Hack -- The only one of its kind */
	else if (allart_p(o_ptr))
	{
		t = object_desc_str(t, "the ");
	}
	/* A single one (later) */
	else
	{
		wants_article = ARTICLE_REQUEST;

		/* Leave enough space to prepend "An ". */
		tmp_val += 3;
		t += 3;
	}

	/* Copy the string */
	while (TRUE)
	{
		/* Normal */
		if (*r[this_level] & 0xE0)
		{
			/* Copy */
			*t++ = *r[this_level]++;
		}
		/* Return to original string or finish. */
		else if (*r[this_level] == '\0')
		{
			if (this_level)
			{
				r[--this_level]++;
			}
			else
			{
				*t = '\0';
				break;
			}
		}
		/* Handle CH_ARTICLE later. */
		else if (*r[this_level] == CH_ARTICLE)
		{
			r[this_level]++;
			if (wants_article == ARTICLE_REQUEST)
				wants_article = ARTICLE_LETTER;
		}
		/* Normalise flag */
		else if (find_cm(*r[this_level]) == CM_NORM)
		{
			r[this_level]++;
		}
		/* Change flags. As this returns a switch flag,
		 * advance to the next potential active character. */
		else if (find_cm(*r[this_level]) != CM_ACT)
		{
			r[this_level] = find_next_good_flag(r[this_level], reject | current, ~(reject | current));
	  	}
		/* Paranoia - too many levels.
		 * As "current" prevents the same string from being
		 * started recursively, this can't happen. */
		else if (this_level == MAX_NAME_LEVEL-1)
		{
			r[this_level]++;
		}
		/* Switch to another string */
		else if (strings[find_ci(*r[this_level])])
		{
			int temp = find_ci(*r[this_level]);
			current |= 1<<temp;
			r[++this_level] = strings[temp];
		}
		/* Paranoia - nothing else makes sense. */
		else
		{
			r[this_level]++;
		}
	}

	/* Replace articles now the following string is known. */
	if (wants_article == ARTICLE_LETTER)
	{
		/* Find the first letter/number. */
		for (s = tmp_val; !isalnum(*s) && *s; s++);

		/* And copy an article to it without termination if one is found. */
		if (is_a_vowel(*s) || *s == '8')
		{
			tmp_val -= 3;
			strncpy(tmp_val, "an ", 3);
		}
		else if (isalnum(*s))
		{
			tmp_val -= 2;
			strncpy(tmp_val, "a ", 2);
  		}
	}

	/* No more details wanted */
	if (mode < 1) goto copyback;


	/* Hack -- Chests must be described in detail */
	if (o_ptr->tval == TV_CHEST)
	{
		/* Not searched yet */
		if (!ok_ptr->pval)
		{
			/* Nothing */
		}

		/* May be "empty" */
		else if (!o_ptr->pval)
		{
			t = object_desc_str(t, " (empty)");
		}

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			if (chest_traps[o_ptr->pval])
			{
				t = object_desc_str(t, " (disarmed)");
			}
			else
			{
				t = object_desc_str(t, " (unlocked)");
			}
		}

		/* Describe the traps, if any */
		else
		{
			/* Describe the traps */
			switch (chest_traps[o_ptr->pval])
			{
				case 0:
				{
					t = object_desc_str(t, " (Locked)");
					break;
				}
				case CHEST_LOSE_STR:
				case CHEST_LOSE_CON:
				{
					t = object_desc_str(t, " (Poison Needle)");
					break;
				}
				case CHEST_POISON:
				case CHEST_PARALYZE:
				{
					t = object_desc_str(t, " (Gas Trap)");
					break;
				}
				case CHEST_EXPLODE:
				{
					t = object_desc_str(t, " (Explosion Device)");
					break;
				}
				case CHEST_SUMMON:
				{
					t = object_desc_str(t, " (Summoning Runes)");
					break;
				}
				default:
				{
					t = object_desc_str(t, " (Multiple Traps)");
					break;
				}
			}
		}
	}

	/* Bows are displayed as (e.g.) (x2) */
	if ((power = get_bow_mult(o_ptr)))
	{
		/* Apply the "Extra Might" flag */
		if (o_ptr->flags3 & (TR3_XTRA_MIGHT)) power++;

		/* Append a special "damage" string */
		t += sprintf(t, " (x%d)", power);
	}
	/* Melee and throwing weapons are displayed as (e.g.) (1d2).
	 * Hack - Throwing weapons which have no dice are omitted.
	 */
	else if ((o_ptr->dd && o_ptr->ds) || wield_slot(o_ptr) == INVEN_WIELD)
	{
		t += sprintf(t, " (%dd%d)", o_ptr->dd, o_ptr->ds);
	}

	/* Add the weapon bonuses */
	if (ok_ptr->to_h || ok_ptr->to_d)
	{
		/* Show the tohit/todam on request */
		if ((o_ptr->flags3 & TR3_SHOW_MODS) || (o_ptr->to_h && o_ptr->to_d))
		{
			t += sprintf(t, " (%+d,%+d)", o_ptr->to_h, o_ptr->to_d);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			t += sprintf(t, " (%+d)", o_ptr->to_h);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			t += sprintf(t, " (%+d)", o_ptr->to_d);
		}
	}


	/* Show the armor class info */
	if (o_ptr->flags3 & TR3_SHOW_ARMOUR || o_ptr->ac)
	{
		/* Add the armor bonuses */
		if (ok_ptr->to_a)
		{
			t += sprintf(t, " [%d,%+d]", o_ptr->ac, o_ptr->to_a);
		}
		else
		{
			t += sprintf(t, " [%d]", o_ptr->ac);
		}
	}
	/* No base armor, but does increase armor */
	else if (o_ptr->to_a)
	{
		t += sprintf(t, " [%+d]", o_ptr->to_a);
	}

	/* No more details wanted */
	if (mode < 2) goto copyback;


	/* Hack -- Wands and Staffs have charges */
	if (ok_ptr->pval &&
	    ((o_ptr->tval == TV_STAFF) ||
	     (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		t += sprintf(t, " (%d charge%s)",
			o_ptr->pval, o_ptr->pval == 1 ? "" : "s");
	}

	/* Hack -- Process Lanterns/Torches */
	else if ((o_ptr->tval == TV_LITE) && ok_ptr->pval && (!allart_p(o_ptr)))
	{
		/* Hack -- Turns of light for normal lites */
		t += sprintf(t, " (with %d turns of light)", o_ptr->pval);
	}


	/* Dump "pval" flags for wearable items */
	if (o_ptr->flags1 & TR1_PVAL_MASK && o_ptr->pval)
	{
		/* Dump the pval. */
		t += sprintf(t, " (%+d", o_ptr->pval);

		/* Hack - Differentiate known pvals from deduced ones. */
		if (!object_known_p(o_ptr)) t = object_desc_chr(t, '?');

		/* Do not display the "pval" flags */
		if (o_ptr->flags3 & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}
		else
		{

/* What actually needs to be done with the strings below. */
#define ADD_PV(X) \
	t = object_desc_str(t, " " X)

			/* Give details about a single bonus. */
			switch (o_ptr->flags1 & TR1_PVAL_MASK)
			{
				case TR1_STR: ADD_PV("str"); break;
				case TR1_INT: ADD_PV("int"); break;
				case TR1_WIS: ADD_PV("wis"); break;
				case TR1_DEX: ADD_PV("dex"); break;
				case TR1_CON: ADD_PV("con"); break;
				case TR1_CHR: ADD_PV("chr"); break;
				case TR1_STEALTH: ADD_PV("stealth"); break;
				case TR1_SEARCH: ADD_PV("search"); break;
				case TR1_INFRA: ADD_PV("infra"); break;
				case TR1_TUNNEL: ADD_PV("dig"); break;
				case TR1_SPEED: ADD_PV("movement speed"); break;
				case TR1_BLOWS: ADD_PV("attack speed"); break;
			}
		}

		/* Finish the display */
		t = object_desc_chr(t, ')');
	}

	/* Indicate "charging" things */
	if (o_ptr->timeout)
	{
		/* Hack -- Dump " (charging)" if relevant */
		t = object_desc_str(t, " (charging)");
	}


	/* Combine the user-defined and automatic inscriptions if required. */
	if (mode >= 3)
	{
		cptr k[4];
		int i = 0;

		/* This is long enough for "(2147483647)" and for "100% off".*/
		char tmp2[2][13];

		/* Find the sections of inscription. */

		/* Obtain the value this would have if uncursed if allowed. If this
		 * differs from the present value, put parentheses around the number.
		 * Hack - suppress for empty slots.
		 */
		if (spoil_value && spoil_base && o_ptr->k_idx &&
			(!auto_haggle || !in_shop))
		{
			s32b value;
			bool worthless;

			value = object_value(o_ptr, FALSE);
			worthless = !value;
			if (worthless && cursed_p(o_ptr))
			{
				object_type j_ptr[1];
				object_copy(j_ptr, o_ptr);

				j_ptr->ident &= ~(IDENT_CURSED);
				j_ptr->flags3 &= ~(TR3_CURSED | TR3_HEAVY_CURSE);
	 			value = object_value(j_ptr, FALSE);
			}

			/* Let the player know when a known cursed item is not broken. */
			if (worthless && value)
			{
	 			sprintf(tmp2[0], "(%ld)", value);
			}
			else
			{
	 			sprintf(tmp2[0], "%ld", value);
			}
			k[i++] = tmp2[0];
		}

		/* Hack - this must be <9 character long. See above. */
		if (o_ptr->discount)
		{
			sprintf(tmp2[1], "%d%% off", o_ptr->discount);
			k[i++] = tmp2[1];
		}

		/* find_feeling() gives hints about the real object. */
		if (!in_shop)
		{
			int feel = find_feeling(o1_ptr);
			if (feel != SENSE_NONE) k[i++] = feeling_str[feel].str;
		}

		if (o_ptr->note) k[i++] = quark_str(o_ptr->note);

		if (i && t < tmp_val+len-4)
		{
			t = object_desc_str(t, " {");

			/* Append the inscriptions from bottom to top. */
			while (i-- && t < tmp_val+len-2)
			{ 
				/* This leaves enough space for formatting. */
				sprintf(t, "%.*s", MAX(0, len-(t-tmp_val)-2), k[i]);
				t = strchr(t, '\0');

				/* Put a comma if there is room for at least one character after it. */
				if (i && t < tmp_val+len-4) t = object_desc_str(t, ", ");
			}
			t = object_desc_chr(t, '}');
		}
	}

copyback:
	/* Here's where we dump the built string into buf. */
	tmp_val[len-1] = '\0';
	t = tmp_val;
	while((*(buf++) = *(t++))) ; /* copy the string over */

	TFREE(tmp_val_base);
}

void object_desc_f3(char *buf, uint max, cptr fmt, va_list *vp)
{
	object_type *o_ptr = va_arg(*vp, object_type *);
	int pref = va_arg(*vp, int);
	int mode = va_arg(*vp, int);

	/* Use a length of ONAME_MAX as a default. */
	if (!strchr(fmt, '.'))
	{
		max = MIN(ONAME_MAX, max);
	}

	object_desc(buf, max, o_ptr, pref, mode);
}

/*
 * Return the name of an object specified by k_idx in buf.
 */
void object_k_name_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int n = va_arg(*vp, int);
	object_type q_ptr[1];

	object_prep(q_ptr, n);

	strnfmt(buf, max, "%v", object_desc_f3, q_ptr, OD_SHOP, 0);
}

/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr PURE item_activation(object_ctype *o_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Require activation ability */
	if (!(f3 & (TR3_ACTIVATE))) return "nothing";

	/* Randarts store their activations in object_type. */
        switch (o_ptr->activation)
        {
            case ACT_SUNLIGHT:
                return "beam of sunlight every 10 turns";
            case ACT_BO_MISS_1:
                return "magic missile (2d6) every 2 turns";
            case ACT_BA_POIS_1:
                return "stinking cloud (12), rad. 3, every 4+d4 turns";
            case ACT_BO_ELEC_1:
                return "lightning bolt (4d8) every 6+d6 turns";
            case ACT_BO_ACID_1:
                return "acid bolt (5d8) every 5+d5 turns";
            case ACT_BO_COLD_1:
                return "frost bolt (6d8) every 7+d7 turns";
            case ACT_BO_FIRE_1:
                return "fire bolt (9d8) every 8+d8 turns";
            case ACT_BA_COLD_1:
                return "ball of cold (48) every 400 turns";
            case ACT_BA_FIRE_1:
                return "ball of fire (72) every 400 turns";
            case ACT_DRAIN_1:
                return "drain life (100) every 100+d100 turns";
            case ACT_BA_COLD_2:
                return "ball of cold (100) every 300 turns";
            case ACT_BA_ELEC_2:
                return "ball of lightning (100) every 500 turns";
            case ACT_DRAIN_2:
                return "drain life (120) every 400 turns";
            case ACT_VAMPIRE_1:
                return "vampiric drain (3*50) every 400 turns";
            case ACT_BO_MISS_2:
                return "arrows (150) every 90+d90 turns";
            case ACT_BA_FIRE_2:
                return "fire ball (120) every 225+d225 turns";
            case ACT_BA_COLD_3:
                return "ball of cold (200) every 325+d325 turns";
            case ACT_WHIRLWIND:
                return "whirlwind attack every 250 turns";
            case ACT_VAMPIRE_2:
                return "vampiric drain (3*100) every 400 turns";
            case ACT_CALL_CHAOS:
                return "call chaos every 350 turns";
            case ACT_SHARD:
                return "shard ball (120+level) every 400 turns";
            case ACT_DISP_EVIL:
                return "dispel evil (level*5) every 300+d300 turns";
            case ACT_DISP_GOOD:
                return "dispel good (level*5) every 300+d300 turns";
            case ACT_BA_MISS_3:
                return "elemental breath (300) every 500 turns";
            case ACT_CONFUSE:
                return "confuse monster every 15 turns";
            case ACT_SLEEP:
                return "sleep nearby monsters every 55 turns";
            case ACT_QUAKE:
                return "earthquake (rad 10) every 50 turns";
            case ACT_TERROR:
                return "terror every 3 * (level+10) turns";
            case ACT_TELE_AWAY:
                return "teleport away every 200 turns";
            case ACT_BANISH_EVIL:
                return "banish evil every 250+d250 turns";
            case ACT_GENOCIDE:
                return "genocide every 500 turns";
            case ACT_MASS_GENO:
                return "mass genocide every 1000 turns";
            case ACT_CHARM_ANIMAL:
                return "charm animal every 300 turns";
            case ACT_CHARM_UNDEAD:
                return "enslave undead every 333 turns";
            case ACT_CHARM_OTHER:
                return "charm monster every 400 turns";
            case ACT_CHARM_ANIMALS:
                return "animal friendship every 500 turns";
            case ACT_CHARM_OTHERS:
                return "mass charm every 750 turns";
            case ACT_SUMMON_ANIMAL:
                return "summon animal every 200+d300 turns";
            case ACT_SUMMON_PHANTOM:
                return "summon phantasmal servant every 200+d200 turns";
            case ACT_SUMMON_ELEMENTAL:
                return "summon elemental every 750 turns";
            case ACT_SUMMON_DEMON:
                return "summon demon every 666+d333 turns";
            case ACT_SUMMON_UNDEAD:
                return "summon undead every 666+d333 turns";
            case ACT_CURE_LW:
                return "remove fear & heal 30 hp every 10 turns";
            case ACT_CURE_MW:
                return "heal 4d8 & wounds every 3+d3 turns";
            case ACT_CURE_POISON:
                return "remove fear and cure poison every 5 turns";
            case ACT_REST_LIFE:
                return "restore life levels every 450 turns";
            case ACT_REST_ALL:
                return "restore stats and life levels every 750 turns";
            case ACT_CURE_700:
                return "heal 700 hit points every 250 turns";
            case ACT_CURE_1000:
                return "heal 1000 hit points every 888 turns";
            case ACT_ESP:
                return "temporary ESP (dur 25+d30) every 200 turns";
            case ACT_BERSERK:
                return "heroism and berserk (dur 50+d50) every 100+d100 turns";
            case ACT_PROT_EVIL:
                return "protect evil (dur level*3 + d25) every 225+d225 turns";
            case ACT_RESIST_ALL:
                return "resist elements (dur 40+d40) every 200 turns";
            case ACT_SPEED:
                return "speed (dur 20+d20) every 250 turns";
            case ACT_XTRA_SPEED:
                return "speed (dur 75+d75) every 200+d200 turns";
            case ACT_WRAITH:
                return "wraith form (level/2 + d(level/2)) every 1000 turns";
            case ACT_INVULN:
                return "invulnerability (dur 8+d8) every 1000 turns";
            case ACT_LIGHT:
                return "light area (dam 2d15) every 10+d10 turns";
            case ACT_MAP_LIGHT:
                return "light (dam 2d15) & map area every 50+d50 turns";
            case ACT_DETECT_ALL:
                return "detection every 55+d55 turns";
            case ACT_DETECT_XTRA:
                return "detection, probing and identify true every 1000 turns";
            case ACT_ID_FULL:
                return "identify true every 750 turns";
            case ACT_ID_PLAIN:
                return "identify spell every 10 turns";
            case ACT_RUNE_EXPLO:
                return "explosive rune every 200 turns";
            case ACT_RUNE_PROT:
                return "rune of protection every 400 turns";
            case ACT_SATIATE:
                return "satisfy hunger every 200 turns";
            case ACT_DEST_DOOR:
                return "destroy doors every 10 turns";
            case ACT_STONE_MUD:
                return "stone to mud every 5 turns";
            case ACT_RECHARGE:
                return "recharging every 70 turns";
            case ACT_ALCHEMY:
                return "alchemy every 500 turns";
            case ACT_DIM_DOOR:
                return "dimension door every 100 turns";
            case ACT_TELEPORT:
                return "teleport (range 100) every 45 turns";
			case ACT_TELEPORT_WAIT:
				return "teleport (range 100) every 50+d50 turns";
            case ACT_RECALL:
                return "word of recall every 200 turns";
			case 0:
				break;
            default:
                return "a bad randart activation";
        }

	/* Some artifacts can be activated */
	switch (o_ptr->name1)
	{
		case ART_FAITH:
			return "fire bolt (9d8) every 8+d8 turns";
		case ART_HOPE:
			return "frost bolt (6d8) every 7+d7 turns";
		case ART_CHARITY:
			return "lightning bolt (4d8) every 6+d6 turns";
		case ART_THOTH:
			return "stinking cloud (12) every 4+d4 turns";
		case ART_ICICLE:
			return "frost ball (48) every 5+d5 turns";
		case ART_DANCING:
			return "remove fear and cure poison every 5 turns";
		case ART_STARLIGHT:
			return "frost ball (100) every 300 turns";
        case ART_DAWN:
            return "summon a Black Reaver every 500+d500 turns";
		case ART_EVERFLAME:
			return "fire ball (72) every 400 turns";
		case ART_FIRESTAR:
			return "large fire ball (72) every 100 turns";
		case ART_ITHAQUA:
			return "haste self (20+d20 turns) every 200 turns";
		case ART_THEODEN:
			return "drain life (120) every 400 turns";
		case ART_JUSTICE:
			return "drain life (90) every 70 turns";
		case ART_OGRELORDS:
			return "door and trap destruction every 10 turns";
		case ART_GHARNE:
			return "word of recall every 200 turns";
		case ART_THUNDER:
			return "haste self (20+d20 turns) every 100+d100 turns";
		case ART_ERIRIL:
			return "identify every 10 turns";
		case ART_ATAL:
            return "probing, detection and full id  every 1000 turns";
		case ART_TROLLS:
			return "mass genocide every 1000 turns";
		case ART_SPLEENSLICER:
			return "cure wounds (4d7) every 3+d3 turns";
		case ART_DEATH:
			return "fire branding of bolts every 999 turns";
        case ART_KARAKAL:
            return "a getaway every 35 turns";
		case ART_ODIN:
            return "lightning ball (100) every 500 turns";
		case ART_DESTINY:
			return "stone to mud every 5 turns";
		case ART_SOULKEEPER:
			return "heal (1000) every 888 turns";
		case ART_VAMPLORD:
            return ("heal (777), curing and heroism every 300 turns");
		case ART_ORCS:
			return "genocide every 500 turns";
		case ART_NYOGTHA:
			return "restore life levels every 450 turns";
		case ART_GNORRI:
			return "teleport away every 150 turns";
		case ART_BARZAI:
			return "resistance (20+d20 turns) every 111 turns";
		case ART_DARKNESS:
			return "Sleep II every 55 turns";
		case ART_SWASHBUCKLER:
			return "recharge item I every 70 turns";
		case ART_SHIFTER:
			return "teleport every 45 turns";
		case ART_TOTILA:
			return "confuse monster every 15 turns";
		case ART_LIGHT:
			return "magic missile (2d6) every 2 turns";
		case ART_IRONFIST:
			return "fire bolt (9d8) every 8+d8 turns";
		case ART_GHOULS:
			return "frost bolt (6d8) every 7+d7 turns";
		case ART_WHITESPARK:
			return "lightning bolt (4d8) every 6+d6 turns";
		case ART_DEAD:
			return "acid bolt (5d8) every 5+d5 turns";
		case ART_COMBAT:
			return "a magical arrow (150) every 90+d90 turns";
		case ART_SKULLKEEPER:
			return "detection every 55+d55 turns";
		case ART_SUN:
            return "heal (700) every 250 turns";
		case ART_RAZORBACK:
			return "star ball (150) every 1000 turns";
		case ART_BLADETURNER:
            return "breathe elements (300), berserk rage, bless, and resistance";
		case ART_POLARIS:
			return "illumination every 10+d10 turns";
		case ART_XOTH:
            return "magic mapping and light every 50+d50 turns";
		case ART_TRAPEZOHEDRON:
            return "clairvoyance and recall, draining you";
		case ART_ALHAZRED:
			return "dispel evil (x5) every 300+d300 turns";
		case ART_LOBON:
			return "protection from evil every 225+d225 turns";
        case ART_MAGIC:
            return "a strangling attack (100) every 100+d100 turns";
		case ART_BAST:
			return "haste self (75+d75 turns) every 150+d150 turns";
		case ART_ELEMFIRE:
			return "large fire ball (120) every 225+d225 turns";
		case ART_ELEMICE:
			return "large frost ball (200) every 325+d325 turns";
		case ART_ELEMSTORM:
			return "large lightning ball (250) every 425+d425 turns";
		case ART_NYARLATHOTEP:
			return "bizarre things every 450+d450 turns";
        case ART_POWER: case ART_MASK:
            return "rays of fear in every direction";
	}


	/* Rings and DSM. */
	switch (o_ptr->k_idx)
	{
		case OBJ_RING_FIRE:
			return "ball of fire and resist fire";
		case OBJ_RING_ICE:
			return "ball of cold and resist cold";
		case OBJ_RING_ACID:
			return "ball of acid and resist acid";
		case OBJ_DSM_BLUE:
			return "breathe lightning (100) every 450+d450 turns";
		case OBJ_DSM_WHITE:
			return "breathe frost (110) every 450+d450 turns";
		case OBJ_DSM_BLACK:
			return "breathe acid (130) every 450+d450 turns";
		case OBJ_DSM_GREEN:
			return "breathe poison gas (150) every 450+d450 turns";
		case OBJ_DSM_RED:
			return "breathe fire (200) every 450+d450 turns";
		case OBJ_DSM_MULTI_HUED:
			return "breathe multi-hued (250) every 225+d225 turns";
		case OBJ_DSM_BRONZE:
			return "breathe confusion (120) every 450+d450 turns";
		case OBJ_DSM_GOLD:
			return "breathe sound (130) every 450+d450 turns";
		case OBJ_DSM_CHAOS:
			return "breathe chaos/disenchant (220) every 300+d300 turns";
		case OBJ_DSM_LAW:
			return "breathe sound/shards (230) every 300+d300 turns";
		case OBJ_DSM_BALANCE:
			return "You breathe balance (250) every 300+d300 turns";
		case OBJ_DSM_PSEUDO:
			return "breathe light/darkness (200) every 300+d300 turns";
		case OBJ_DSM_POWER:
			return "breathe the elements (300) every 300+d300 turns";
	}

	/* Error types */
	switch (o_ptr->tval)
	{
		case TV_RING:
			return "a bad ring activation";
		case TV_DRAG_ARMOR:
			return "a bad dragon scale mail activation";
		default:
			return "a bad miscellaneous activation";
	}
}


/*
 * Allocate and return a string listing a set of flags in a specified format.
 * This may give incorrect output on strings >1024 characters long.
 */
cptr list_flags(cptr init, cptr conj, cptr *flags, int total)
{
	cptr s;

	/* Paranoia. */
	if (!init || !conj) return "";

	s = format("%s ", init);
	if (total > 2)
	{
		int i;
		for (i = 0; i < total-2; i++)
		{
			s = format("%s%s, ", s, flags[i]);
		}
	}
	if (total > 1)
	{
		s = format("%s%s %s ", s, flags[total-2], conj);
	}
	if (total)
	{
		s = format("%s%s.", s, flags[total-1]);
	}
	else
	{
		s = format("%snothing.", s);
	}
	return s;
}

/* The maximum number of strings allowed for identify_fully_aux().
 * This should always be greater than the greatest possible number, as one is
 * currently used for termination. */
#define MAX_IFA	128

/* Information passed from identify_fully_get() to the output functions. */
typedef struct ifa_type ifa_type;

struct ifa_type
{
	cptr txt;
	bool alloc;
	byte attr;
};

/*
 * Set i_ptr->txt as an allocated string, remember the fact.
 */
static void alloc_ifa(ifa_type *i_ptr, cptr str, ...)
{
	va_list vp;

	/*
	 * Hack - Convert an initial $x to a colour code. 
	 * I'll change it once I've checked what happens to the string.
	 */
	if (str[0] == '$' && strchr(atchar, str[1]))
	{
		i_ptr->attr = color_char_to_attr(str[1]);
		str += 2;
	}
	else
	{
		i_ptr->attr = TERM_WHITE;
	}

	/* Begin the Varargs Stuff */
	va_start(vp, str);

	/* Format and allocate the input string. */
	i_ptr->txt = string_make(vformat(str, vp));

	/* Remember that it has been allocated. */
	i_ptr->alloc = TRUE;

	/* End the Varargs Stuff */
	va_end(vp);
}

/* Shorthand notation for res_stat_details() */

/* Flags for an item which affects all stats (for convenience). */
#define A_ALL ((1<<A_STR)+(1<<A_INT)+(1<<A_WIS)+(1<<A_DEX)+(1<<A_CON)+(1<<A_CHR))

/* A basic test */
#define test(x,y) (x[stat_res[y]] - x[p_ptr->stat_ind[y]])

/* The special test required to calculate blows. */
#define blows(x) (blows_table[MIN(adj_str_blow[x[A_STR]]*mul/div,11)][MIN(adj_dex_blow[x[A_DEX]], 11)])

#define DIF ABS(dif)
#define DIF_INC ((dif > 0) ? "increases" : "decreases")
#define DIF_DEC ((dif < 0) ? "increases" : "decreases")
#define DIF_MOR ((dif > 0) ? "more" : "less")
#define DIF_LES ((dif < 0) ? "more" : "less")

#define CERT ((act & (A_INCREASE | A_DEC10 | A_DEC25)) ? "at least " : "")

#define A_RESTORE	0x01
#define A_INCREASE	0x02
#define A_INCRES	(A_INCREASE | A_RESTORE)
#define A_DEC10	0x04	/* A small temporary decrease, as do_dec_stat(). */
#define A_DEC25	0x08	/* A large permanent decrease, as a potion of ruination. */
#define A_WIELD	0x10	/* Hack - indicates a melee weapon. */

/* Just in case birthrand.diff isn't applied. */
#ifndef A_MAX
#define A_MAX 6
#endif

#define CMPU(X) (pn_ptr->X - po_ptr->X)
#define CMPUU(X) (ABS(CMPU(X)))
#define CMP(X) ((dif = CMPU(X)))
#define CMPUJ(X) CMPU(X[j])
#define CMPJ(X) ((dif = CMPUJ(X)))
#define CMPS(X) ((dif = X[pn_ptr->stat_ind[j]] - X[po_ptr->stat_ind[j]]))
#define CMPT(X,Y) ((dif = X[pn_ptr->stat_ind[Y]] - X[po_ptr->stat_ind[Y]]))

/*
 * Actually describe a stat modifying item.
 * As the modifications have taken place, this just queries the old and new
 * versions of p_ptr to see what has changed.
 */
static void res_stat_details_comp(player_type *pn_ptr, player_type *po_ptr, int *i, ifa_type *info, byte act)
{
	int j,dif;
	byte attr;
	cptr stats[6] = {"strength", "intelligence", "wisdom", "dexterity", "constitution", "charisma"};

	for (j = 0; j < A_MAX; j++)
	{
		/* Give the modifier in white. */
		attr = TERM_WHITE;

		if (CMPJ(stat_max) > 0)
		{
			alloc_ifa(info+(*i)++, "It adds %s%d to your %s.", CERT, dif, stats[j]);
		}
		else if (dif)
		{
			alloc_ifa(info+(*i)++, "It removes %s%d from your %s.", CERT, -dif, stats[j]);
		}
		else if (CMPJ(stat_cur))
		{
			alloc_ifa(info+(*i)++, "It restores your %s.", stats[j]);
		}
		else if (CMPJ(stat_add) > 0)
		{
			alloc_ifa(info+(*i)++, "It adds %d to your %s.", dif, stats[j]);
		}
		else if (dif)
		{
			alloc_ifa(info+(*i)++, "It removes %d from your %s.", -dif, stats[j]);
		}

		/* No effect, so boring. */
		if (!CMPJ(stat_use)) continue;

		/* Give its effects in grey. */
		attr = TERM_L_WHITE;

		switch (j)
		{
			case A_CHR:
			if (!pn_ptr->anti_magic && CMPS(adj_mag_study))
				alloc_ifa(info+(*i)++, "$W  It causes you to annoy spirits %s.", DIF_LES);
			if (!pn_ptr->anti_magic && CMPS(adj_mag_fail))
				alloc_ifa(info+(*i)++, "$W  It %s your maximum spiritual success rate by %d%%.", DIF_DEC, DIF);
			if (!pn_ptr->anti_magic && CMPS(adj_mag_stat))
				alloc_ifa(info+(*i)++, "$W  It %s your spiritual success rates.", DIF_INC);
			if (CMPS(adj_chr_gold)) alloc_ifa(info+(*i)++, "$W  It %s your bargaining power.", DIF_DEC);
			break;
			case A_WIS:
			if (CMPS(adj_mag_mana))
				alloc_ifa(info+(*i)++, "$W  It gives you %d %s chi at 100%% skill (%d now).", DIF*25, DIF_MOR, CMPUU(mchi));
			if (!pn_ptr->anti_magic && CMPS(adj_mag_fail))
				alloc_ifa(info+(*i)++, "$W  It %s your maximum mindcraft success rate by %d%%.", DIF_DEC, DIF);
			if (!pn_ptr->anti_magic && CMPS(adj_mag_stat))
				alloc_ifa(info+(*i)++, "$W  It %s your mindcraft success rates.", DIF_INC);
			if (CMP(skill_sav))
				alloc_ifa(info+(*i)++, "$W  It %s your saving throw by %d%%.", DIF_INC, DIF);
			break;
			case A_INT: /* Rubbish in the case of icky gloves or heavy armour. */
			if (CMPS(adj_mag_study)) alloc_ifa(info+(*i)++, "$W  It allows you to learn %d %s spells at 100%% skill.", DIF*25, DIF_MOR);
			if (CMPS(adj_mag_mana)) alloc_ifa(info+(*i)++, "$W  It gives you %d %s mana at 100%% skill (%d now).", DIF*25, DIF_MOR, CMPUU(msp));
			if (!pn_ptr->anti_magic && CMPS(adj_mag_fail))
				alloc_ifa(info+(*i)++, "$W  It %s your maximum spellcasting success rate by %d%%.", DIF_DEC, DIF);
			if (!pn_ptr->anti_magic && CMPS(adj_mag_stat))
				alloc_ifa(info+(*i)++, "$W  It %s your spellcasting success rates.", DIF_INC);
			if (CMP(skill_dev)) alloc_ifa(info+(*i)++, "$W  It %s your success rate with magical devices.", DIF_INC);
			break;
			case A_CON:
			if (CMPS(adj_con_fix)) alloc_ifa(info+(*i)++, "$W  It %s your regeneration rate.", DIF_INC);
			if (CMPS(adj_con_mhp)) alloc_ifa(info+(*i)++, "$W  It gives you %d %s hit points at 100%% skill (%d now).", DIF*25, DIF_MOR, CMPUU(mhp));
			break;
			case A_DEX:
			if (CMP(dis_to_a)) alloc_ifa(info+(*i)++, "$W  It %s your AC by %d.", DIF_INC, DIF);
			if (CMP(dis_to_h)) alloc_ifa(info+(*i)++, "$W  It %s your chance to hit opponents by %d.", DIF_INC, DIF);
			/* Fix me - this also covers stunning, but is affected by saving throw. */
			if (CMPS(adj_dex_safe)) alloc_ifa(info+(*i)++, "$W  It makes you %d%% %s resistant to theft.", DIF, DIF_MOR);
			break;
			case A_STR:
			if (CMP(dis_to_d)) alloc_ifa(info+(*i)++, "$W  It %s your ability to damage opponents by %d.", DIF_INC, DIF);
			if (CMPS(adj_str_wgt)) alloc_ifa(info+(*i)++, "$W  It %s your maximum carrying capacity by %d.", DIF_INC, DIF);
			if (CMPS(adj_str_hold)) alloc_ifa(info+(*i)++, "$W  It makes you %s able to use heavy weapons.", DIF_MOR);
			if (CMP(skill_dig)) alloc_ifa(info+(*i)++, "$W  It allows you to dig %s effectively.", DIF_MOR);
		}

		/* A couple of things which depend on two stats. */
		if (j == A_DEX || (j == A_STR && !CMPU(stat_ind[A_DEX])))
		{
			if ((~act & A_WIELD) && CMP(num_blow))
			{
				alloc_ifa(info+(*i)++,
					"$W  It %s your number of blows by %d,%d",
					DIF_INC, DIF/60, DIF%60);
			}
		}
		if (j == A_DEX || (j == A_INT && !CMPU(stat_ind[A_DEX])))
		{
			int dif2;
			if ((dif2 = CMPS(adj_dex_dis)+CMPT(adj_int_dis, A_INT)))
			{
				dif = dif2;
				alloc_ifa(info+(*i)++, "$W  It %s your disarming skill.", DIF_INC);
			}
		}
	}
}

/*
 * Increase a stat by the minimum amount possible.
 */
static void change_stat_min(int stat, int act)
{
	int i;
	for (i = 0; i < A_MAX; i++)
	{
		/* Doesn't affect this stat. */
		if (~stat & 1<<i) continue;
		
		/* Restores this stat. */
		if (act & A_RESTORE) (void)res_stat(i);

		/* Affect this stat (as inc_stat(), but minimal). */
		if ((act & A_INCREASE) && (p_ptr->stat_cur[i] < 118))
		{
			int gain, value = p_ptr->stat_cur[i];
			if (value < 18 || value > 115) gain = 1;
			else gain = ((118-value)/2+3)/2+1;
			p_ptr->stat_cur[i] += gain;
			p_ptr->stat_max[i] += gain;
		}
		
		/* A temporary loss of 10 */
		if ((act & A_DEC10) && (p_ptr->stat_cur[i] > 3))
		{
			int loss, cur = p_ptr->stat_cur[i];
			if (cur < 19) loss = 1;

			else loss =
#if 0 /* It's always 5, so don't waste time calculating it. */
			MAX((((cur-18)/2+1)/2+2)*10/100, 10/2);
#else
			5;
#endif
			if (cur > 18 && cur-loss <= 18)
				p_ptr->stat_cur[i] = 18;
			else
				p_ptr->stat_cur[i] -= loss;
		}
		/* A permanent loss of 25 */
		if ((act & A_DEC25) && (p_ptr->stat_max[i] > 3))
		{
			int loss, cur = p_ptr->stat_max[i];
			if (cur < 19) loss = 1;
			else loss =
#if 0 /* It's always 10, so don't waste time calculating it. */
			MAX((((cur-18)/2+1)/2+2)*25/100, 25/2);
#else
			12;
#endif
			if (p_ptr->stat_cur[i] > 18 && p_ptr->stat_cur[i]-loss <= 18)
				p_ptr->stat_cur[i] = 18;
			else
				p_ptr->stat_cur[i] = MAX(p_ptr->stat_cur[i]-loss, 3);

			if (p_ptr->stat_max[i] > 18 && p_ptr->stat_max[i]-loss <= 18)
				p_ptr->stat_max[i] = 18;
			else
				p_ptr->stat_max[i] -= loss;
		}
	}
}

static void get_stat_flags(object_type *o_ptr, byte *stat, byte *act, s16b *pval)
{
	int j;
	struct convtype {
	s16b k_idx;
	byte act;
	byte stat;
	} conv[] = {
		/* Increase stat potions */
		{OBJ_POTION_INC_STR, A_INCRES, 1<<A_STR},
		{OBJ_POTION_INC_INT, A_INCRES, 1<<A_INT},
		{OBJ_POTION_INC_WIS, A_INCRES, 1<<A_WIS},
		{OBJ_POTION_INC_DEX, A_INCRES, 1<<A_DEX},
		{OBJ_POTION_INC_CON, A_INCRES, 1<<A_CON},
		{OBJ_POTION_INC_CHR, A_INCRES, 1<<A_CHR},
		{OBJ_POTION_STAR_ENLIGHTENMENT, A_INCRES, (1<<A_INT | 1<<A_WIS)},
		{OBJ_POTION_AUGMENTATION, A_INCRES, A_ALL},
		/* Restore stat potions */
		{OBJ_POTION_RES_STR, A_RESTORE, 1<<A_STR},
		{OBJ_POTION_RES_INT, A_RESTORE, 1<<A_INT},
		{OBJ_POTION_RES_WIS, A_RESTORE, 1<<A_WIS},
		{OBJ_POTION_RES_DEX, A_RESTORE, 1<<A_DEX},
		{OBJ_POTION_RES_CON, A_RESTORE, 1<<A_CON},
		{OBJ_POTION_RES_CHR, A_RESTORE, 1<<A_CHR},
		{OBJ_POTION_LIFE, A_RESTORE, A_ALL},
		/* Restore stat mushrooms */
		{OBJ_FOOD_RES_STR, A_RESTORE, 1<<A_STR},
		{OBJ_FOOD_RES_CON, A_RESTORE, 1<<A_CON},
		{OBJ_FOOD_RESTORING, A_RESTORE, A_ALL},
		/* Other restore stat items */
		{OBJ_STAFF_THE_MAGI, A_RESTORE, 1<<A_INT},
		{OBJ_ROD_RESTORATION, A_RESTORE, A_ALL},
		/* Decrease stat potions */
		{OBJ_POTION_DEC_STR, A_DEC10, 1<<A_STR},
		{OBJ_POTION_DEC_INT, A_DEC10, 1<<A_INT},
		{OBJ_POTION_DEC_WIS, A_DEC10, 1<<A_WIS},
		{OBJ_POTION_DEC_DEX, A_DEC10, 1<<A_DEX},
		{OBJ_POTION_DEC_CON, A_DEC10, 1<<A_CON},
		{OBJ_POTION_DEC_CHR, A_DEC10, 1<<A_CHR},
		{OBJ_POTION_RUINATION, A_DEC25, A_ALL},
		/* Decrease stat food */
		{OBJ_FOOD_DEC_STR, A_DEC10, A_STR},
		{OBJ_FOOD_SICKNESS, A_DEC10, A_CON},
		{OBJ_FOOD_DEC_INT, A_DEC10, A_INT},
		{OBJ_FOOD_DEC_WIS, A_DEC10, A_WIS},
		{OBJ_FOOD_UNHEALTH, A_DEC10, A_CON},
		{OBJ_FOOD_DISEASE, A_DEC10, A_STR},
		{0,0,0},
	};

	*stat = 0;

	/* Start with the permanent modifiers items in */
	if (o_ptr->flags1 & TR1_STR) *stat |= 1<<A_STR;
	if (o_ptr->flags1 & TR1_INT) *stat |= 1<<A_INT;
	if (o_ptr->flags1 & TR1_WIS) *stat |= 1<<A_WIS;
	if (o_ptr->flags1 & TR1_DEX) *stat |= 1<<A_DEX;
	if (o_ptr->flags1 & TR1_CON) *stat |= 1<<A_CON;
	if (o_ptr->flags1 & TR1_CHR) *stat |= 1<<A_CHR;

	/* If one of these flags exist, the modifier refers to it. */
	if (*stat) *pval = o_ptr->pval;
	else *pval = 0;

	/* Look for stat-affecting potions, food, etc..*/
	for (j = 0, *act = 0; conv[j].k_idx; j++)
	{
		if (conv[j].k_idx == o_ptr->k_idx)
		{
			*stat |= conv[j].stat;
			*act |= conv[j].act;
		}
	}
	/* Hack - res_stat_details_comp() needs to recognise melee weapons, so
	 * set it up here. */
	if (wield_slot(o_ptr) == INVEN_WIELD) *act |= A_WIELD;
}

/*
 * Return the original if given For objects produced by object_info_known().
 * Return the input otherwise.
 */
object_ctype PURE *get_real_obj(object_ctype *o_ptr)
{
	if (!o_ptr->iy && o_ptr->ix) return inventory+o_ptr->ix-1;
	else return o_ptr;
}

/*
 * Inform the player if an object is currently being worn.
 */
bool PURE is_worn_p(object_ctype *o_ptr)
{
	o_ptr = get_real_obj(o_ptr);
	return (o_ptr >= inventory+INVEN_WIELD && o_ptr < inventory+INVEN_TOTAL);
}

/*
 * Find out what, if any, stat changes the given item causes.
 */
static void res_stat_details(object_type *o_ptr, int real_k_idx, int *i, ifa_type *info)
{
	byte stat, act;
	s16b pval;
	object_type j_ptr[1];
	player_type p2_ptr[1];

	/* Find out the stat flags and pval (if relevant). */
	get_stat_flags(o_ptr, &stat, &act, &pval);

	/* Boring */
	if (!stat) return;

	/* As update_stuff() can cause potentially irreversible changes,
	* use a copy instead. */

	COPY(p2_ptr, p_ptr, player_type);
	p_ptr = p2_ptr;

	/*
	 * Carry out a pessimistic version of any restore/increase the item
	 * gives.
	 */
	if (act & (A_RESTORE | A_INCREASE)) change_stat_min(stat, act);

	/* An item that gives no effects for being worn. */
	if (!pval)
	{
		/* Correct bonuses quietly. */
		p_ptr->update = PU_BONUS | PU_QUIET;
		update_stuff();

		/* Compare the original with the same with this object wielded. */
		res_stat_details_comp(p2_ptr, &p_body, i, info, act);
	}
	/* A worn item. */
	else if (is_worn_p(o_ptr))
	{
		/* Hack - use the original o_ptr. */
		o_ptr = inventory+o_ptr->ix-1;

		/* Put o_ptr somewhere safe. */
		object_copy(j_ptr, o_ptr);
		object_wipe(o_ptr);

		/* Correct bonuses quietly. */
		p_ptr->update = PU_BONUS | PU_QUIET;
		update_stuff();

		/* Compare the original with the same with the item removed. */
		res_stat_details_comp(&p_body, p2_ptr, i, info, act);

		/* Replace o_ptr. */
		object_copy(o_ptr, j_ptr);
	}
	/* A wearable item. */
	else
	{
		int slot = wield_slot(o_ptr);

		/* Put the currently worn item somewhere safe. */
		object_copy(j_ptr, inventory+slot);

		/* Hack - the player should know how to wear everything. */
		if (slot == -1)
		{
			object_type tmp;
			object_prep(&tmp, real_k_idx);
			slot = wield_slot(&tmp);

			/* Paranoia - not a wearable item? */
			if (slot == -1)
			{
				p_ptr = &p_body;
				return;
			}
		}

		/* Put o_ptr in its place. */
		object_copy(inventory+slot, o_ptr);

		/* Correct bonuses quietly. */
		p_ptr->update = PU_BONUS | PU_QUIET;
		update_stuff();

		/* Compare the original with the same with this object wielded. */
		res_stat_details_comp(p2_ptr, &p_body, i, info, act);

			/* Replace inveotory+slot */
			object_copy(inventory+slot, j_ptr);
	}

	/* Return p_ptr to normal. */
	p_ptr = &p_body;
}


/* Offset for a continued string, -1 as these start with a space anyway. */
#define IFD_OFFSET	3

/*
 * Find the number of characters in in before the last space before the
 * maxth character.
 */
static int wrap_str(cptr in, int max)
{
	int len = strlen(in);
	cptr t;
	
	/* Short enough anyway. */
	if (len < max) return len;
	
	/* Find the last space before (or including) the max character. */
	for (t = in+max-1; *t != ' '; t--);

	/* Return the offset. */
	return (int)(t-in);
}

/*
 * Show the flags an object has in an interactive way.
 */
static void identify_fully_show(ifa_type *i_ptr)
{
	int k, len, minx = 15, xlen = Term->wid - minx+1, maxy = Term->hgt;
	cptr s = "";

	/* Save the screen */
	Term_save();

	/* Erase the screen */
	for (k = 1; k < maxy; k++) prt("", k, (minx > 2) ? minx-2 : 0);

	/* Label the information */
	prt("     Item Attributes:", 1, minx);

	/* We will print on top of the map. */
	for (k = 2;;)
	{
		/* Hack - turn the default colour (black) into white. */
		byte attr = (i_ptr->attr) ? i_ptr->attr : TERM_WHITE;
		
		/* Grab the next string. */
		if (*s == '\0')
		{
			s = (i_ptr++)->txt;

			/* Finished. */
			if (!s) break;
		}

		/* Show the info */

		/* A space means that this is a continuation, so give an offset. */
		if (*s == ' ')
		{
			len = wrap_str(s, xlen-IFD_OFFSET);
			c_prt(attr, format("%.*s", len, s), k++, minx+IFD_OFFSET);
		}
		/* If not, don't. */
		else
		{
			len = wrap_str(s, xlen);
			c_prt(attr, format("%.*s", len, s), k++, minx);
		}

		/* Find the next segment. */
		s += len;

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == maxy-1) && (i_ptr->txt))
		{
			prt("-- more --", k, minx);
			inkey();
			for (; k > 2; k--) prt("", k, minx);
		}
	}

	/* Wait for it */
	prt("[Press any key to continue]", k, minx);
	inkey();

	/* Restore the screen */
	Term_load();
}


/*
 * Show the first maxy-1 lines of the information stored in info.
 */
static void identify_fully_dump(ifa_type *i_ptr)
{
	int j,k, len, minx = 0, xlen = Term->wid - minx+1, maxy = Term->hgt;
	cptr s = "";

	/* Label the information */
	prt("     Item Attributes:", 1, minx);

	for (k = 2, j = 0; k <= maxy; k++)
	{
		/* Hack - turn the default colour (black) into white. */
		byte attr = (i_ptr->attr) ? i_ptr->attr : TERM_WHITE;
		
		/* Grab the next string. */
		if (*s == '\0')
		{
			s = (i_ptr++)->txt;

			/* Finished. */
			if (!s) break;
		}

		/* Show the info */

		/* A space means that this is a continuation, so give an offset. */
		if (*s == ' ')
		{
			len = wrap_str(s, xlen-IFD_OFFSET);
			c_prt(attr, format("%.*s", len, s), k, minx+IFD_OFFSET);
		}
		/* If not, don't. */
		else
		{
			len = wrap_str(s, xlen);
			c_prt(attr, format("%.*s", len, s), k, minx);
		}
		
		/* Find the next segment. */
		s += len;
	}
}

#define IFDF_OFFSET 5

/*
 * Dump the information stored in info to a specified file.
 */
static void identify_fully_dump_file(FILE *fff, ifa_type *i_ptr)
{
	int j,k, len, x, xlen = 80;
	cptr s = "";

	for (k = 2, j = 0;;)
	{
		/* Grab the next string. */
		if (*s == '\0')
		{
			s = (i_ptr++)->txt;
			if (!s) break;
		}

		/* Show the info */

		/* Standard description offset. */
		x = IFDF_OFFSET;

		/* Give an extra offset for a continuation. */
		if (*s == ' ') x += IFD_OFFSET;

		len = wrap_str(s, xlen-x);
		fprintf(fff, "%*s%.*s\n", x, "", len, s);
		
		/* Find the next segment. */
		s += len;
	}
}


/*
 * Clear any allocated strings in info[].
 */
static void identify_fully_clear(ifa_type *i_ptr)
{
	do
	{
		if (i_ptr->alloc) FREE(i_ptr->txt);
	}
	while ((++i_ptr)->txt);
}

/*
 * Hack - Determine the multiplier for a bow.
 * This is stored in a non-traditional field, and relies upon the bow doing
 * no damage when thrown.
 */
int PURE get_bow_mult(object_ctype *o_ptr)
{
	if (o_ptr->ds && !o_ptr->dd)
	{
		return o_ptr->ds;
	}
	else
	{
		return 0;
	}
}

/*
 * Find the k_idx of a launcher which can fire a given missile.
 * Always returns the first such launcher in k_info.
 */
s16b PURE launcher_type(object_ctype *o_ptr)
{
	s16b k_idx;
	for (k_idx = 0; k_idx < MAX_K_IDX; k_idx++)
	{
		/* Not a launcher. */
		if (k_info[k_idx].tval != TV_BOW) continue;

		/* Not the right sort of launcher. */
		if (k_info[k_idx].extra != o_ptr->tval) continue;
		
		/* Success. */
		return k_idx;
	}

	/* Nothing */
	return OBJ_NOTHING;
}


/*
 * Return the chance of using a device successfully.
 * Returns 0 if it has no associated skill-based uses.
 * NB: This assumes that the object contains its flags.
 */
static int get_device_chance_dec(object_type *o_ptr)
{
	int num,denom;
	if (!is_wand_p(k_info+o_ptr->k_idx) && (~o_ptr->flags3 & TR3_ACTIVATE))
	{
		/* Not a wand, rod, staff or activatable object. */
		return 0;
	}
	else
	{
		/* Work it out. */
		get_device_chance(o_ptr, &num, &denom);
		return 1000*num/denom;
	}
}

/*
 * Find the ammunition tval used by a given launcher.
 * Do not check that this is a missile launcher.
 */
byte PURE ammunition_type(object_ctype *o_ptr)
{
	return k_info[o_ptr->k_idx].extra;
}

/* A wrapper around list_flags() for identify_fully_get(), provided for
 * clarity and maintainability. */
#define do_list_flags(init, conj, flags, total) \
	alloc_ifa(info+i++, list_flags(init, conj, flags, total))

/*
 * Find the strings which describe the flags of o_ptr, place them in info
 * and note whether each was allocated.
 *
 * Return the total number of strings.
 *
 * If brief is set, various less interesting things are omitted.
 */
static void identify_fully_get(object_ctype *o1_ptr, ifa_type *info, bool brief)
{
	int                     i = 0, j;

	cptr board[16];

	object_type o_ptr[1];

	/* Paranoia - no object. */
	if (!o1_ptr || !o1_ptr->k_idx) return;

	/* Extract the known info */
	object_info_known(o_ptr, o1_ptr);

	/* Mega-Hack -- describe activation */
	if (o_ptr->flags3 & (TR3_ACTIVATE))
	{
		info[i++].txt = "It can be activated for...";
		info[i++].txt = item_activation(o_ptr);
		info[i++].txt = "...if it is being worn.";
	}

	/* Describe use of the base object, if any. */
	if (spoil_base && !brief)
	{
		/* There's a description in k_info.txt. */
		if (k_info[o_ptr->k_idx].text)
		{
			info[i++].txt = k_text+k_info[o_ptr->k_idx].text;
		}
		/* Give an effect-based description if there was none. */
		else
		{
			cptr s = describe_object_power(o_ptr);
			if (s && *s) alloc_ifa(info+i++, "When used: %s", s);
		}
	}

	j = get_device_chance_dec(o_ptr);
	if (j) alloc_ifa(info+i++,
		"You have a %d.%d%% chance of successfully using it %s.",
		j/10, j%10, ((o_ptr->flags3 & TR3_ACTIVATE) && !is_worn_p(o_ptr))
		? "if you wear it" : "at present");

	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE && k_info[o_ptr->k_idx].extra)
	{
		alloc_ifa(info+i++, "It provides light (radius %d) %s.",
			k_info[o_ptr->k_idx].extra,
			((allart_p(o_ptr))) ? "forever" : "when fueled.");
	}

	/* Hack - describe the wield skill of weaponry. */
	switch (wield_skill(o_ptr))
	{
 	case SKILL_CLOSE:
 		info[i++].txt="It trains your close combat skill.";
 				break;
	case SKILL_CRUSH:
 		info[i++].txt="It trains your crushing weapons skill.";
 				break;
 	case SKILL_STAB:
 		info[i++].txt="It trains your stabbing weapons skill.";
 		break;
	case SKILL_SLASH:
 		info[i++].txt="It trains your slashing weapons skill.";
 			break;
	case  SKILL_MISSILE:
 		info[i++].txt="It trains your missile skill.";
 		break;
 	}


	/* And then describe it fully */

	j = 0;
	
	/* Recognise items which affect stats (detailed) */
	if (!brief && spoil_stat)
	{
		res_stat_details(o_ptr, o1_ptr->k_idx, &i, info);
	}
	/* If brevity is required or spoilers are not, put stats with the other
	 * pval effects. */
	else
	{
		if (o_ptr->flags1 & (TR1_STR)) board[j++] = "strength";
		if (o_ptr->flags1 & (TR1_INT)) board[j++] = "intelligence";
		if (o_ptr->flags1 & (TR1_WIS)) board[j++] = "wisdom";
		if (o_ptr->flags1 & (TR1_DEX)) board[j++] = "dexterity";
		if (o_ptr->flags1 & (TR1_CON)) board[j++] = "constitution";
		if (o_ptr->flags1 & (TR1_CHR)) board[j++] = "charisma";
	}
	if (o_ptr->flags1 & (TR1_STEALTH)) board[j++] = "stealth";
	if (o_ptr->flags1 & (TR1_SEARCH)) board[j++] = "searching";
	if (o_ptr->flags1 & (TR1_INFRA)) board[j++] = "infravision";
	if (o_ptr->flags1 & (TR1_TUNNEL)) board[j++] = "ability to tunnel";
	if (o_ptr->flags1 & (TR1_SPEED)) board[j++] = "movement speed";
	if (o_ptr->flags1 & (TR1_BLOWS)) board[j++] = "attack speed";
	if (j)
	{
		if (o_ptr->pval > 0)
		{
			board[j] = format("It adds %d to your", o_ptr->pval);
		}
		else if (o_ptr->pval < 0)
		{
			board[j] = format("It removes %d from your", -o_ptr->pval);
		}
		else
		{
			board[j] = "It affects your";
		}
		do_list_flags(board[j], "and", board, j);
	}


    if (o_ptr->flags1 & (TR1_CHAOTIC))
	{
        info[i++].txt = "It produces chaotic effects.";
	}

    if (o_ptr->flags1 & (TR1_VAMPIRIC))
	{
        info[i++].txt = "It drains life from your foes.";
	}

    if (o_ptr->flags1 & (TR1_IMPACT))
	{
		info[i++].txt = "It can cause earthquakes.";
	}

    if (o_ptr->flags1 & (TR1_VORPAL))
	{
        info[i++].txt = "It is very sharp and can cut your foes.";
	}

	/* Calculate actual damage of weapons. 
	 * This only considers slays and brands at the moment. */
	if (spoil_dam)
	{
		cptr s;
		s16b tohit, todam, weap_blow, mut_blow;
		s32b dam;
		bool slay = FALSE;

		/* Notice whether the item is of a category usually used as a weapon. */
		bool weapon = ((wield_slot(o_ptr) == INVEN_WIELD) ||
			(wield_slot(o_ptr) == INVEN_BOW) ||
			(launcher_type(o_ptr) != OBJ_NOTHING));

		/* Calculate x15 slays. */
		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_X15_DRAGON)
			board[j++]  = "dragons";
		if (j)
		{
			weapon_stats(o_ptr, 15, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			slay = TRUE;
			if (dam%60) s = "It causes %ld %ld/60 damage to";
				else s = "It causes %ld damage to";
			do_list_flags(format(s, dam/60, dam%60), "and", board, j);
		}

		/* Calculate x5 slays. */
		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_KILL_DRAGON)
			board[j++] = "dragons";

		if (j)
		{
			weapon_stats(o_ptr, 5, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			slay = TRUE;
			if (dam%60) s = "It causes %ld %ld/60 damage to";
				else s = "It causes %ld damage to";
			do_list_flags(format(s, dam/60, dam%60), "and", board, j);
		}

		/* Calculate x3 slays */
		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_SLAY_DRAGON)
			board[j++] = "dragons";
		if (o_ptr->flags1 & TR1_SLAY_ORC) board[j++] = "orcs";
		if (o_ptr->flags1 & TR1_SLAY_TROLL) board[j++] = "trolls";
		if (o_ptr->flags1 & TR1_SLAY_GIANT) board[j++] = "giants";
		if (o_ptr->flags1 & TR1_SLAY_DEMON) board[j++] = "demons";
		if (o_ptr->flags1 & TR1_SLAY_UNDEAD) board[j++] = "undead";
		if (j)
		{
			weapon_stats(o_ptr, 3, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			slay = TRUE;
			if (dam%60) s = "It causes %ld %ld/60 damage to";
				else s = "It causes %ld damage to";
			do_list_flags(format(s, dam/60, dam%60), "and", board, j);
		}
		/* Calculate brands */
		j = 0;
		if (o_ptr->flags1 & TR1_BRAND_FIRE) board[j++] = "fire";
		if (o_ptr->flags1 & TR1_BRAND_COLD) board[j++] = "cold";
		if (o_ptr->flags1 & TR1_BRAND_ELEC) board[j++] = "electricity";
		if (o_ptr->flags1 & TR1_BRAND_ACID) board[j++] = "acid";
		if (o_ptr->flags1 & TR1_BRAND_POIS) board[j++] = "poison";
		if (j)
		{
			weapon_stats(o_ptr, 3, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			slay = TRUE;
			if (dam%60) s = "It causes %ld %ld/60 damage via";
				else s = "It causes %ld damage via";
			do_list_flags(format(s, dam/60, dam%60), "and", board, j);
		}
		/* Calculate x2 slays */
		j = 0;
		if (o_ptr->flags1 & TR1_SLAY_EVIL) board[j++] = "evil monsters";
		if (o_ptr->flags1 & TR1_SLAY_ANIMAL) board[j++] = "animals";
		if (j)
		{
			weapon_stats(o_ptr, 2, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			slay = TRUE;
			if (dam%60) s = "It causes %ld %ld/60 damage to";
				else s = "It causes %ld damage to";
			do_list_flags(format(s, dam/60, dam%60), "and", board, j);
		}

		/* Give the damage a weapon does. Exclude throwing weapons in brief
		 * mode as they are usually less interesting.
		 */
		if (!brief || weapon)
		{
			cptr sfx, desc;
			weapon_stats(o_ptr, 1, &tohit, &todam, &weap_blow, &mut_blow, &dam);
			j = 0;
			if (slay) desc = "all other monsters";
			else if (dam) desc = "all monsters";
			else goto nextbit;

			/* Mention throwing for non-weapons as they may cause damage
			 * under other circumstances.
			 */
			if (!weapon) sfx = " when thrown";
			else sfx = "";

			if (dam%60) s = "It causes %ld %ld/60 damage to %s%s.";
				else s = "It causes %ld damage to %s%s.";
			alloc_ifa(info+i++, format(s, dam/60, dam%60, desc, sfx));
		}
nextbit:

		/* Describe blows per turn. */
		switch (wield_slot(o_ptr))
		{
			case INVEN_WIELD: *board = "blows"; break;
			case INVEN_BOW: *board = "shots"; break;
			default: *board = 0;
		}
		if (*board)
		{
			alloc_ifa(info+i++, "It gives you %d,%d %s per turn",
				weap_blow/60, weap_blow%60, *board);
		}
	}
	/* Without spoil_dam, simply list the slays. */
	else
	{
		/* Calculate brands */
		j = 0;
		if (o_ptr->flags1 & TR1_BRAND_FIRE) board[j++] = "fire";
		if (o_ptr->flags1 & TR1_BRAND_COLD) board[j++] = "cold";
		if (o_ptr->flags1 & TR1_BRAND_ELEC) board[j++] = "electricity";
		if (o_ptr->flags1 & TR1_BRAND_ACID) board[j++] = "acid";
		if (o_ptr->flags1 & TR1_BRAND_POIS) board[j++] = "poison";
		if (j)
		{
			do_list_flags("It causes extra damage via", "and", board, j);
		}
		
		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_X15_DRAGON)
		{
			board[j++] = "dragons";
		}
		if (j)
		{
			do_list_flags("It causes a huge amount of extra damage to", "and",
				board, j);
		}

		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_KILL_DRAGON)
		{
			board[j++] = "dragons";
		}
		if (j)
		{
			do_list_flags("It causes a lot of extra damage to", "and", board,
				j);
		}

		j = 0;
		if ((o_ptr->flags1 & TR1_ALL_SLAY_DRAGON) == TR1_SLAY_DRAGON)
			board[j++] = "dragons";
		if (o_ptr->flags1 & TR1_SLAY_ORC) board[j++] = "orcs";
		if (o_ptr->flags1 & TR1_SLAY_TROLL) board[j++] = "trolls";
		if (o_ptr->flags1 & TR1_SLAY_GIANT) board[j++] = "giants";
		if (o_ptr->flags1 & TR1_SLAY_DEMON) board[j++] = "demons";
		if (o_ptr->flags1 & TR1_SLAY_UNDEAD) board[j++] = "undead";
		if (o_ptr->flags1 & TR1_SLAY_EVIL) board[j++] = "evil monsters";
		if (o_ptr->flags1 & TR1_SLAY_ANIMAL) board[j++] = "animals";
		if (j)
		{
			do_list_flags("It causes extra damage to", "and", board, j);
		}
	}

	j = 0;
	if (o_ptr->flags2 & (TR2_SUST_STR)) board[j++] = "strength";
	if (o_ptr->flags2 & (TR2_SUST_INT)) board[j++] = "intelligence";
	if (o_ptr->flags2 & (TR2_SUST_WIS)) board[j++] = "wisdom";
	if (o_ptr->flags2 & (TR2_SUST_DEX)) board[j++] = "dexterity";
	if (o_ptr->flags2 & (TR2_SUST_CON)) board[j++] = "constitution";
	if (o_ptr->flags2 & (TR2_SUST_CHR)) board[j++] = "charisma";
	if (j == 6)
	{
		info[i++].txt = "It sustains all of your stats.";
	}
	else if (j)
	{
		do_list_flags("It sustains your", "and", board, j);
	}

	j = 0;
	if (o_ptr->flags2 & (TR2_IM_ACID)) board[j++] = "acid";
	if (o_ptr->flags2 & (TR2_IM_ELEC)) board[j++] = "electricity";
	if (o_ptr->flags2 & (TR2_IM_FIRE)) board[j++] = "fire";
	if (o_ptr->flags2 & (TR2_IM_COLD)) board[j++] = "cold";
	if (o_ptr->flags2 & (TR2_FREE_ACT)) board[j++] = "paralysis";
	if (o_ptr->flags2 & (TR2_RES_FEAR)) board[j++] = "fear";
	if (j)
	{
		do_list_flags("It provides immunity to", "and", board, j);
	}

	j = 0;
	if (o_ptr->flags2 & (TR2_RES_ACID)) board[j++] = "acid";
	if (o_ptr->flags2 & (TR2_RES_ELEC)) board[j++] = "electricity";
	if (o_ptr->flags2 & (TR2_RES_FIRE)) board[j++] = "fire";
	if (o_ptr->flags2 & (TR2_RES_COLD)) board[j++] = "cold";
	if (o_ptr->flags2 & (TR2_RES_POIS)) board[j++] = "poison";
	if (o_ptr->flags2 & (TR2_RES_LITE)) board[j++] = "light";
	if (o_ptr->flags2 & (TR2_RES_DARK)) board[j++] = "dark";
	if (o_ptr->flags2 & (TR2_HOLD_LIFE)) board[j++] = "life draining";
	if (o_ptr->flags2 & (TR2_RES_BLIND)) board[j++] = "blindness";
	if (o_ptr->flags2 & (TR2_RES_CONF)) board[j++] = "confusion";
	if (o_ptr->flags2 & (TR2_RES_SOUND)) board[j++] = "sound";
	if (o_ptr->flags2 & (TR2_RES_SHARDS)) board[j++] = "shards";
	if (o_ptr->flags2 & (TR2_RES_NETHER)) board[j++] = "nether";
	if (o_ptr->flags2 & (TR2_RES_NEXUS)) board[j++] = "nexus";
	if (o_ptr->flags2 & (TR2_RES_CHAOS)) board[j++] = "chaos";
	if (o_ptr->flags2 & (TR2_RES_DISEN)) board[j++] = "disenchantment";
	if (j)
	{
		do_list_flags("It provides resistance to", "and", board, j);
	}

    if (o_ptr->flags3 & (TR3_WRAITH))
    {
        info[i++].txt = "It renders you incorporeal.";
    }
	if (o_ptr->flags3 & (TR3_FEATHER))
	{
        info[i++].txt = "It allows you to levitate.";
	}
	if (o_ptr->flags3 & (TR3_LITE))
	{
		info[i++].txt = "It provides permanent light.";
	}
	if (o_ptr->flags3 & (TR3_SEE_INVIS))
	{
		info[i++].txt = "It allows you to see invisible monsters.";
	}
	if (o_ptr->flags3 & (TR3_TELEPATHY))
	{
		info[i++].txt = "It gives telepathic powers.";
	}
	if (o_ptr->flags3 & (TR3_SLOW_DIGEST))
	{
		info[i++].txt = "It slows your metabolism.";
	}
	if (o_ptr->flags3 & (TR3_REGEN))
	{
		info[i++].txt = "It speeds your regenerative powers.";
	}
    if (o_ptr->flags2 & (TR2_REFLECT))
    {
        info[i++].txt = "It reflects bolts and arrows.";
    }
    if (o_ptr->flags3 & (TR3_SH_FIRE))
    {
        info[i++].txt = "It produces a fiery sheath.";
    }
    if (o_ptr->flags3 & (TR3_SH_ELEC))
    {
        info[i++].txt = "It produces an electric sheath.";
    }
    if (o_ptr->flags3 & (TR3_NO_MAGIC))
    {
        info[i++].txt = "It produces an anti-magic shell.";
    }
    if (o_ptr->flags3 & (TR3_NO_TELE))
    {
        info[i++].txt = "It prevents teleportation.";
    }
	if (o_ptr->flags3 & (TR3_XTRA_MIGHT))
	{
		info[i++].txt = "It fires missiles with extra might.";
	}
	if (o_ptr->flags3 & (TR3_XTRA_SHOTS))
	{
		info[i++].txt = "It fires missiles excessively fast.";
	}

	if (!(o_ptr->flags3 & TR3_NO_MAGIC) && cumber_glove(o_ptr))
	{
		if (o_ptr->ident & (IDENT_MENTAL | IDENT_TRIED))
		{
			/* Hack - the "no encumbrance" flag isn't set without spoilers.
			 * so double-check as the player has been told. */
			if (cumber_glove(o1_ptr))
				info[i++].txt = "It inhibits spellcasting.";
		}
		else
		{
			info[i++].txt = "It may inhibit spellcasting.";
		}
	}

	if (!(o_ptr->flags3 & TR3_NO_MAGIC) && cumber_helm(o_ptr))
	{
		if (o_ptr->ident & (IDENT_MENTAL | IDENT_TRIED))
		{
			/* Hack - the "no encumbrance" flag isn't set without spoilers,
			 * so double-check as the player has been told. */
			if (cumber_helm(o1_ptr))
				info[i++].txt = "It inhibits mindcrafting.";
		}
		else
		{
			info[i++].txt = "It may inhibit mindcrafting.";
		}
	}

	if (o_ptr->flags3 & (TR3_DRAIN_EXP))
	{
		info[i++].txt = "It drains experience.";
	}
	if (o_ptr->flags3 & (TR3_TELEPORT))
	{
		info[i++].txt = "It induces random teleportation.";
	}
	if (o_ptr->flags3 & (TR3_AGGRAVATE))
	{
		info[i++].txt = "It aggravates nearby creatures.";
	}

	if (o_ptr->flags3 & (TR3_BLESSED))
	{
		info[i++].txt = "It has been blessed by the gods.";
	}

	/* Describe random possibilities if not *identified*. 
	 * Note that this only has a precise meaning for artefacts. 
	 * Bug - LITE can be mentioned both here and in its own right. Unfortunately it's not easy to fix. */

	j = 0;
	if (o_ptr->flags2 & (TR2_RAND_RESIST))
	{
		board[j++] = "a random resistance";
	}
	if (o_ptr->flags2 & (TR2_RAND_POWER))
	{
		board[j++] = "a random power";
	}
	if (o_ptr->flags2 & (TR2_RAND_EXTRA))
	{
		if (j)
			board[j++] = "a further random power or resistance";
		else
			board[j++] = "a random power or resistance";
	}

	if (j)
	{
		cptr pref = (o_ptr->name1) ? "It gives you" : "It may give you";
		do_list_flags(pref, "and", board, j);
	}
	

		if (o_ptr->flags3 & (TR3_PERMA_CURSE))
		{
			info[i++].txt = "It is permanently cursed.";
		}
		else if (o_ptr->flags3 & (TR3_HEAVY_CURSE))
		{
			info[i++].txt = "It is heavily cursed.";
		}
	else if (o_ptr->flags3 & (TR3_CURSED))
		{
			info[i++].txt = "It is cursed.";
		}

	if (o_ptr->flags3 & (TR3_AUTO_CURSE))
	{
		info[i++].txt = "It will curse itself.";
	}

    if (o_ptr->flags3 & (TR3_TY_CURSE))
    {
        info[i++].txt = "It carries an ancient foul curse.";
    }

	/* Hack - simple items don't get ignore flags listed. */
	if (brief && o_ptr->flags3 & TR3_EASY_KNOW);
	
	else if ((o_ptr->flags3 & (TR3_IGNORE_ALL)) == TR3_IGNORE_ALL)
	{
		info[i++].txt = "It cannot be harmed by the elements.";
	}
	else if (!(o_ptr->flags3 & (TR3_IGNORE_ALL)))
	{
		/* Say nothing. */
	}
	/* State the missing flags. */
	else
	{
		j = 0;
		if (~o_ptr->flags3 & (TR3_IGNORE_ACID)) board[j++] = "acid";
		if (~o_ptr->flags3 & (TR3_IGNORE_ELEC)) board[j++] = "electricity";
		if (~o_ptr->flags3 & (TR3_IGNORE_FIRE)) board[j++] = "fire";
		if (~o_ptr->flags3 & (TR3_IGNORE_COLD)) board[j++] = "cold";

		if (o_ptr->ident & IDENT_MENTAL || cheat_item)
		{
			board[j] = "can";
		}
		else
		{
			board[j] = "may be able to be";
		}
		if (j)
		{
			do_list_flags(format("It %s be harmed by", board[j]), "and", board, j);
		}
	}
}




/*
 * Describe a "fully identified" item
 * If full is set, the game describes the item as if it was fully identified.
 */
bool identify_fully_aux(object_ctype *o_ptr, byte flags)
{
	bool full = (flags & 0x01) != 0;
	bool paged = (flags & 0x02) == 0;

	ifa_type info[MAX_IFA];

	C_WIPE(info, MAX_IFA, ifa_type);

	if (full)
	{
		object_type q_ptr[1];

		object_copy(q_ptr, o_ptr);
		object_known(q_ptr);
		object_aware(q_ptr);

		identify_fully_get(o_ptr, info, FALSE);
	}
	else
	{
		identify_fully_get(o_ptr, info, FALSE);
	}

	/* Nothing was revealed, so show nothing. */
	if (!info[0].txt) return FALSE;

	if (paged)
	{
		identify_fully_show(info);
	}
	else
	{
		identify_fully_dump(info);
	}

	/* Clear any allocated strings. */
	identify_fully_clear(info);

	return TRUE;
}


/*
 * Describe item details to a specified stream.
 */
void identify_fully_file(object_ctype *o_ptr, FILE *fff)
{
	ifa_type info[MAX_IFA];

	C_WIPE(info, MAX_IFA, ifa_type);

	/* Grab the flags. */
	identify_fully_get(o_ptr, info, TRUE);

	/* Dump the flags, wrapping at 80 characters. */
	identify_fully_dump_file(fff, info);

	/* Clear any allocated strings. */
	identify_fully_clear(info);
}

/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
s16b PURE index_to_label(object_ctype *o_ptr)
{
	int i = o_ptr - inventory;

	/* Indexes for "inven" are easy */
	if (i < INVEN_WIELD) return (I2A(i));

	/* Indexes for "equip" are offset */
	return (I2A(i - INVEN_WIELD));
}



/*
 * Convert a label into the index of an item in the "inven"
 * Return "-1" if the label does not indicate a real item
 */
static object_type PURE *label_to_inven(int c)
{
	object_type *o_ptr;
	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i > INVEN_PACK)) return NULL;

	/* Get the object. */
	o_ptr = inventory+i;

	/* Empty slots can never be chosen */
	if (!o_ptr->k_idx) return NULL;

	/* Return the object */
	return o_ptr;
}


/*
 * Convert a label into the index of a item in the "equip"
 * Return "-1" if the label does not indicate a real item
 */
static object_type PURE *label_to_equip(int c)
{
	object_type *o_ptr;
	int i;

	/* Convert */
	i = (islower(c) ? A2I(c) : -1) + INVEN_WIELD;

	/* Verify the index */
	if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return NULL;

	/* Get the object. */
	o_ptr = inventory+i;

	/* Empty slots can never be chosen */
	if (!o_ptr->k_idx) return NULL;

	/* Return the index */
	return o_ptr;
}



/*
 * Determine which equipment slot (if any) an item likes
 */
s16b PURE wield_slot(object_ctype *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (INVEN_WIELD);
		}

		case TV_BOW:
		{
			return (INVEN_BOW);
		}

		case TV_RING:
		{
			/* Use the right hand if unused */
			if (!inventory[INVEN_RIGHT].k_idx) return (INVEN_RIGHT);

			/* Use the left hand for swapping if possible */
			if (!inventory[INVEN_LEFT].k_idx || !(inventory[INVEN_LEFT].ident & IDENT_CURSED)) return (INVEN_LEFT);

			/* Use the right otherwise */
			return (INVEN_RIGHT);
		}

		case TV_AMULET:
		{
			return (INVEN_NECK);
		}

		case TV_LITE:
		{
			return (INVEN_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (INVEN_BODY);
		}

		case TV_CLOAK:
		{
			return (INVEN_OUTER);
		}

		case TV_SHIELD:
		{
			return (INVEN_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (INVEN_HEAD);
		}

		case TV_GLOVES:
		{
			return (INVEN_HANDS);
		}

		case TV_BOOTS:
		{
			return (INVEN_FEET);
		}

		case TV_POTION:
		case TV_SCROLL:
		case TV_WAND:
		case TV_CHARM:
		{
			/* Use the pouch 1 first */
			if (!inventory[INVEN_POUCH_1].k_idx) return (INVEN_POUCH_1);
			if (!inventory[INVEN_POUCH_2].k_idx) return (INVEN_POUCH_2);
			if (!inventory[INVEN_POUCH_3].k_idx) return (INVEN_POUCH_3);
			if (!inventory[INVEN_POUCH_4].k_idx) return (INVEN_POUCH_4);
			if (!inventory[INVEN_POUCH_5].k_idx) return (INVEN_POUCH_5);
			return (INVEN_POUCH_6);
		}
	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
static cptr mention_use(int i)
{
	cptr p;

	/* Examine the location */
	switch (i)
	{
		case INVEN_WIELD: p = "Wielding"; break;
		case INVEN_BOW:   p = "Shooting"; break;
		case INVEN_LEFT:  p = "On left hand"; break;
		case INVEN_RIGHT: p = "On right hand"; break;
		case INVEN_NECK:  p = "Around neck"; break;
		case INVEN_LITE:  p = "Light source"; break;
		case INVEN_BODY:  p = "On body"; break;
		case INVEN_OUTER: p = "About body"; break;
		case INVEN_ARM:   p = "On arm"; break;
		case INVEN_HEAD:  p = "On head"; break;
		case INVEN_HANDS: p = "On hands"; break;
		case INVEN_FEET:  p = "On feet"; break;
		case INVEN_POUCH_1: p = "In pouch";break;
		case INVEN_POUCH_2: p = "In pouch";break;
		case INVEN_POUCH_3: p = "In pouch";break;
		case INVEN_POUCH_4: p = "In pouch";break;
		case INVEN_POUCH_5: p = "In pouch";break;
		case INVEN_POUCH_6: p = "In pouch";break;
		default:          p = "In pack"; break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == INVEN_BOW)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just holding";
		}
	}

	/* Return the result */
	return (p);
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr PURE describe_use(object_ctype *o_ptr)
{
	switch (o_ptr - inventory)
	{
		case INVEN_WIELD:
		{
			if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
			{
				return "just lifting";
			}
			else
			{
				return "attacking monsters with";
			}
		}
		case INVEN_BOW:
		{
			if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
			{
				return "just holding";
			}
			else
			{
				return "shooting missiles with";
			}
		}

		case INVEN_LEFT:  return "wearing on your left hand";
		case INVEN_RIGHT: return "wearing on your right hand";
		case INVEN_NECK:  return "wearing around your neck";
		case INVEN_LITE:  return "using to light the way";
		case INVEN_BODY:  return "wearing on your body";
		case INVEN_OUTER: return "wearing on your back";
		case INVEN_ARM:   return "wearing on your arm";
		case INVEN_HEAD:  return "wearing on your head";
		case INVEN_HANDS: return "wearing on your hands";
		case INVEN_FEET:  return "wearing on your feet";
		case INVEN_POUCH_1: return "carrying in a pouch";
		case INVEN_POUCH_2: return "carrying in a pouch";
		case INVEN_POUCH_3: return "carrying in a pouch";
		case INVEN_POUCH_4: return "carrying in a pouch";
		case INVEN_POUCH_5: return "carrying in a pouch";
		case INVEN_POUCH_6: return "carrying in a pouch";
		default:          return "carrying in your pack";
	}
}

/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_ctype *o_ptr)
{
	/* Hack -- allow listing empty slots */
	if (item_tester_full) return (TRUE);

	/* Require an item */
	if (!o_ptr->k_idx) return (FALSE);

	/* Hack -- ignore "gold" */
	if (o_ptr->tval == TV_GOLD) return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		if (item_tester_tval != o_ptr->tval) return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook)(o_ptr)) return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}




/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	int i, wid, z = 0;

	object_type *o_ptr;

	byte    attr = TERM_WHITE;


	Term_get_size(&wid, &i);

	/* Find the "final" slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the pack */
	for (i = 0; i < z; i++)
	{
		/* Examine the item */
		o_ptr = &inventory[i];

		/* Display the index if this item is "acceptable" */
		if (item_tester_okay(o_ptr))
			put_str(format("%c) ", index_to_label(o_ptr)), i, 0);

		/* Get a color */
		attr = tval_to_attr[o_ptr->tval % 128];

		/* Display the entry itself */
		c_put_str(attr, format("%.*v", (show_weights) ? wid-12 : wid-3,
			object_desc_f3, o_ptr, TRUE, 3), i, 3);

		/* Display the weight if needed */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			put_str(format("%3d.%1d lb", wgt / 10, wgt % 10), i, wid-9);
		}
	}
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	int i, wid;
	object_type *o_ptr;
	byte    attr = TERM_WHITE;

	Term_get_size(&wid, &i);

	/* Display the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Examine the item */
		o_ptr = &inventory[i];

		/* Display the index if this item is "acceptable" */
		if (item_tester_okay(o_ptr))
			put_str(format("%c) ", index_to_label(o_ptr)), i, 0);

		/* Get the color */
		attr = tval_to_attr[o_ptr->tval % 128];

		/* Display the entry itself */
		c_put_str(attr, format("%.*v",
			wid-3 - (show_weights) ? 9 : 0 - (show_labels) ? 19 : 0,
			object_desc_f3, o_ptr, TRUE, 3), i-INVEN_WIELD, 3);

		/* Display the slot description (if needed) */
		if (show_labels)
		{
			put_str(format("<--- %s", mention_use(i)), i-INVEN_WIELD, wid-19);
		}

		/* Display the weight (if needed) */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			put_str(format("%3d.%1d lb", wgt / 10, wgt % 10), i-INVEN_WIELD,
				(show_labels) ? wid-9 : wid-28);
		}
	}
}






/*
 * Display the inventory.
 *
 * Hack -- do not display "trailing" empty slots
 */
void show_inven(void)
{
	int             i, j, k, l, z = 0;
	int             col, len, lim, wid;

	object_type     *o_ptr;

	char    tmp_val[80];

	int             out_index[23];
	byte    out_color[23];
	cptr	out_desc[23], o_name;

	/* Ensure that unset out_desc strings are NULL. */
	C_WIPE(out_desc, 23, char*);

	/* Starting column */
	col = command_gap;

	/* Get size */
	Term_get_size(&wid, &i);

	/* Default "max-length" */
	len = wid - 1 - col;

	/* Maximum space allowed for descriptions */
	lim = wid - 1 - 3;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Require space for icon */
	if (equippy_chars) lim -= 2;

	/* Respect the maximum name length. */
	if (lim > ONAME_MAX) lim = ONAME_MAX;

	/* Find the "final" slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the inventory */
	for (k = 0, i = 0; i < z; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		o_name = format("%.*v", lim, object_desc_f3, o_ptr, TRUE, 3);

		/* Save the object index, color, and description */
		out_index[k] = i;
		out_color[k] = tval_to_attr[o_ptr->tval % 128];
		out_desc[k] = string_make(o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;

		/* Account for icon if displayed */
		if (equippy_chars) l += 2;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}

	/* Find the column to start in */
	col = (len > wid - 4) ? 0 : (wid - 1 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(o_ptr));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j + 1, col);

		/* Display graphics for object, if desired */
		if (equippy_chars)
		{
			byte  a = object_attr(o_ptr);
			char c = object_char(o_ptr);
			
#ifdef AMIGA
			if (a & 0x80) a |= 0x40;
#endif

			Term_draw(col + 3, j + 1, a, c);
		}

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, equippy_chars ? (col + 5) : (col + 3));

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			(void)sprintf(tmp_val, " %3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j + 1, wid-9);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

	/* Save the new column */
	command_gap = col;

	/* Clean up. */
	for (k = 0; k < 23; k++) FREE(out_desc[k]);
}



/*
 * Display the equipment.
 */
void show_equip(void)
{
	int                     i, j, k, l;
	int                     col, len, lim, wid;

	object_type             *o_ptr;

	char            tmp_val[80];

	int                     out_index[23];
	byte            out_color[23];
	cptr out_desc[23], o_name;


	/* Ensure that unset out_desc strings are NULL. */
	C_WIPE(out_desc, 23, char*);

	/* Starting column */
	col = command_gap;

	/* Get size */
	Term_get_size(&wid, &i);

	/* Maximal length */
	len = wid - 1 - col;

	/* Maximum space allowed for descriptions */
	lim = wid - 1 - 3;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;
	if (equippy_chars) lim -= 2;

	/* Respect the maximum name length. */
	if (lim > ONAME_MAX) lim = ONAME_MAX;

	/* Scan the equipment list */
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		o_name = format("%.*v", lim, object_desc_f3, o_ptr, TRUE, 3);

		/* Save the color */
		out_index[k] = i;
		out_color[k] = tval_to_attr[o_ptr->tval % 128];
		out_desc[k] = string_make(o_name);

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;
		if (equippy_chars) l += 2;

		/* Maintain the max-length */
		if (l > len) len = l;

		/* Advance the entry */
		k++;
	}

	/* Hack -- Find a column to start in */
	col = (len > wid - 4) ? 0 : (wid - 1 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(o_ptr));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

				if (equippy_chars)
		{
			byte a = object_attr(o_ptr);
			char c = object_char(o_ptr);
			
#ifdef AMIGA
			if (a & 0x80) a |= 0x40;
#endif

			Term_draw(col + 3, j + 1, a, c);
		}

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			(void)sprintf(tmp_val, "%-14s: ", mention_use(i));
			put_str(tmp_val, j+1, equippy_chars ? col + 5 : col + 3);

			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, equippy_chars ? col + 21 : col + 19);
		}

		/* No labels */
		else
		{
			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, equippy_chars ? col + 5 : col + 3);
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			(void)sprintf(tmp_val, " %3d.%d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j+1, wid-9);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

	/* Save the new column */
	command_gap = col;

	/* Clean up. */
	for (k = 0; k < 23; k++) FREE(out_desc[k]);
}



/*
 * Returns the object referred to by the given object_idx.
 */
static object_type PURE *cnv_idx_to_obj(s16b index)
{
	if (index < 0)
	{
		return &o_list[-index];
	}
	else if (index < INVEN_TOTAL)
	{
		return &inventory[index];
	}
	else
	{
		return 0;
	}
}



/*
 * Returns the object_idx appropriate for a given item.
 */
static s16b PURE cnv_obj_to_idx(object_ctype *o_ptr)
{
	if (o_ptr >= inventory && o_ptr-inventory < INVEN_TOTAL)
	{
		return o_ptr-inventory;
	}
	else if (o_ptr >= o_list && o_ptr-o_list < MAX_O_IDX)
	{
		return o_list-o_ptr;
	}
	else
	{
		return INVEN_TOTAL;
	}
}






/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, object_ctype *o_ptr)
{
	char    out_val[256];

	/* Prompt */
	(void)strnfmt(out_val, 256, "%s %v? ", prompt,
		object_desc_f3, o_ptr, TRUE, 3);

	/* Query */
	return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_allow(object_ctype *o_ptr)
{
	cptr s;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == command_cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify("Really try", o_ptr)) return (FALSE);
		}

		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
}



/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(object_ctype *o_ptr)
{
	/* Illegal items */
	if (!is_inventory_p(o_ptr) && 
		(o_ptr->ix != px || o_ptr->iy != py)) return (FALSE);

	/* Verify the item */
	if (!item_tester_okay(o_ptr)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}

/*
 * Allow the game and the player to reject an item at a prompt.
 */
static void get_item_valid(object_type **o_ptr, bool *done, bool ver)
{
	/* Validate the item */
	if (!*o_ptr || !get_item_okay(*o_ptr))
	{
		bell(0);
		*done = FALSE;
	}

	/* Allow the player to refuse the item. */
	else if ((ver && !verify("Try", *o_ptr)) || !get_item_allow(*o_ptr))
	{
		*o_ptr = NULL;
		*done = TRUE;
	}

	/* Good, so accept the item and finish. */
	else
	{
		*done = TRUE;
	}
}


/*
 * Set *o_ptr to the next object in a stack, if known.
 * Set *o_ptr to NULL if none known.
 */
void next_object(object_type **o_ptr)
{
	/* Get the next object to squelch, if any. */
	if (is_inventory_p(*o_ptr))
	{
		/* Search the inventory backwards to minimise reordering problems. */
		if (*o_ptr > inventory) (*o_ptr)--;
		else *o_ptr = NULL;
	}
	else
	{
		if (o_ptr[0]->next_o_idx) *o_ptr = o_list+o_ptr[0]->next_o_idx;
		else *o_ptr = NULL;
	}
}



/*
 * Find an inventory object with the given "tag" in its inscription in the
 * player's inventory or equipment.
 *
 * If there is an object with "@xn" in its inscription, set o_ptr to the first
 * such object and return TRUE.
 *
 * If there is not, but there is one inscribed with "@n", set o_ptr to the first
 * such object and return FALSE.
 *
 * Otherwise, leave o_ptr as it is and return FALSE.
 *
 * "o_ptr" is set to the selected item (or is left alone if none is chosen).
 * "tag" is the "n" in the above description.
 * "cmd" is the "x" in the above description.
 * "first" is the first object to check (it's either the last object in this
 * part of the inventory or the first object on the floor).
 */
static bool get_tag(object_type **o_ptr, char tag, s16b cmd, object_type *first)
{
	char cmd_str[3] = "", buf[2*MAX_ASCII_LEN+1];
	int len;
	cptr s;

	object_type *j_ptr;

	/* Turn the command into an ASCII representation. */
	if (cmd & 0xFF00)
		sprintf(cmd_str, "%c%c", ((cmd & 0xFF00) >> 8), (cmd & 0x00FF));
	else
		sprintf(cmd_str, "%c", cmd);

	strnfmt(buf, sizeof(buf), "%v", ascii_to_text_f1, cmd_str);
	len = strlen(buf);

	/* Check every object */
	for (j_ptr = first; j_ptr; next_object(&j_ptr))
	{
		/* Never check the overflow slot. */
		if (j_ptr == inventory+INVEN_PACK) break;

		/* Skip non-objects */
		if (!j_ptr->k_idx) continue;

		/* Get the inscription. */
		s = quark_str(j_ptr->note);

		/* Process all tags */
		while ((s = strchr(s, '@')))
		{
			/* Advance and check for short tags. */
			if (*++s == tag)
			{
				/* Success */
				if (!*o_ptr) *o_ptr = j_ptr;
			}

			/* Check the specific tags */
			else if (s[len] == tag && prefix(s, buf))
			{
				/* Success */
				*o_ptr = j_ptr;
				return TRUE;
			}
		}
	}

	/* No @xn tag. */
	return FALSE;
}


#define GET_ITEM_ERROR_ABORT	-1 /* User hit escape */
#define GET_ITEM_ERROR_NO_ITEMS	-2 /* No legal items to choose */

/*
 * Let the user select an item, return its "index"
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.  Note that
 * the equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * If a legal item is selected, we set "err" to 0 and return a pointer to it.
 * Otherwise, we return NULL, and set "err" to an error code above.
 *
 * Global "command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "command_see" may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * Global "command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * Global "command_gap" is used to indent the inven/equip tables, and
 * to provide some consistancy over time.  It shrinks as needed to hold
 * the various tables horizontally, and can only be reset by calling this
 * function with "command_see" being FALSE, or by pressing ESCAPE from
 * this function, or by hitting "escape" while viewing the inven/equip.
 *
 * We always erase the prompt when we are done.
 *
 * Note that "Term_save()" / "Term_load()" blocks must not overlap.
 */
object_type *get_item(errr *err, cptr pmt, bool equip, bool inven, bool floor)
{
	cave_type *c_ptr = &cave[py][px];

	s16b this_o_idx, next_o_idx = 0;
	object_type * UNREAD(o_ptr);

	char which = ' ';

	int k;

	object_type *i1, *i2, *e1, *e2;

    bool done;

    bool allow_floor = FALSE;

	char tmp_val[160];

	int nterm = 0;

	char *t;

#ifdef ALLOW_REPEAT
     
     /* Get the item index */
     if (repeat_pull(&k)) {
         
		o_ptr = cnv_idx_to_obj(k);

         /* Floor item? */
         if (o_ptr >= o_list && o_ptr < o_list + MAX_O_IDX) {

 			/* Scan all objects in the grid */
 			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
 			{
 				/* Acquire object */
 				o_ptr = &o_list[this_o_idx];
 
 				/* Acquire next object */
 				next_o_idx = o_ptr->next_o_idx;
 
 				/* Validate the item */
 				if (!item_tester_okay(o_ptr)) continue;
 
 				/* Forget the item_tester_tval restriction */
 				item_tester_tval = 0;
 		
 				/* Forget the item_tester_hook restriction */
 				item_tester_hook = NULL;
 				
 				/* Success */
				return o_ptr;
 	        }
         }
         
         /* Verify the item */
         else if (get_item_okay(o_ptr)) {
         
 	        /* Forget the item_tester_tval restriction */
 	        item_tester_tval = 0;
 	
 	        /* Forget the item_tester_hook restriction */
 	        item_tester_hook = NULL;
 	        
 	        /* Success */
			return o_ptr;
         }
		/* Invalid item. */
		 else
		{
			o_ptr = NULL;
		}
     }
 
 #endif /* ALLOW_REPEAT -- TNB */

	/* Paranoia XXX XXX XXX */
	msg_print(NULL);


	/* Not done */
	done = FALSE;

	/* Default to "no error" */
	*err = SUCCESS;


	/* Full inventory */
	i1 = inventory;
	i2 = inventory + INVEN_PACK - 1;

	/* Forbid inventory */
	if (!inven) i2 = NULL;

	/* Restrict inventory indexes */
	while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
	while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


	/* Full equipment */
	e1 = inventory + INVEN_WIELD;
	e2 = inventory + INVEN_TOTAL - 1;

	/* Forbid equipment */
	if (!equip) e2 = NULL;

	/* Restrict equipment indexes */
	while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
	while ((e1 <= e2) && (!get_item_okay(e2))) e2--;


	/* Restrict floor usage */
	if (floor)
	{
		/* Scan all objects in the grid */
		for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
		{
			/* Acquire object */
			o_ptr = &o_list[this_o_idx];

			/* Acquire next object */
			next_o_idx = o_ptr->next_o_idx;

			/* Accept the item on the floor if legal */
			if (item_tester_okay(o_ptr)) allow_floor = TRUE;
		}
	}


	/* Require at least one legal choice */
	if (!allow_floor && (i1 > i2) && (e1 > e2))
	{
		/* Cancel command_see */
		command_see = FALSE;

		/* Hack -- Nothing to choose */
		*err = GET_ITEM_ERROR_NO_ITEMS;
		o_ptr = NULL;

		/* Done */
		done = TRUE;
	}

	/* Analyze choices */
	else
	{
		/* Hack -- Reset display width */
		if (!command_see) command_gap = 50;

		/* Hack -- Start on equipment if requested */
		if (command_see && command_wrk && equip)
		{
			command_wrk = TRUE;
		}

		/* Use inventory if allowed */
		else if (inven)
		{
			command_wrk = FALSE;
		}

		/* Use equipment if allowed */
		else if (equip)
		{
			command_wrk = TRUE;
		}

		/* Use inventory for floor */
		else
		{
			command_wrk = FALSE;
		}
	}


	/* Add a resize hook. */
	add_resize_hook(resize_inkey);

	/* Allow the user to choose to see everything. */
	command_see |= show_choices_main;
 
	/* Hack -- start out in "display" mode */
	if (command_see) nterm = Term_save_aux();


	/* Repeat until done */
	while (!done)
	{
		/* Show choices */
		if (show_choices)
		{
			/* Update */
			p_ptr->window |= (PW_INVEN | PW_EQUIP);
		}

		/* Only show a prompt. */
		if (!command_see);

		/* Inventory screen */
		else if (!command_wrk)
		{
			show_inven();
		}
		/* Equipment screen */
		else
		{
			show_equip();
		}

		t = tmp_val;
		t += sprintf(t, "(%s: ", (command_wrk) ? "Equip" : "Inven");

		if (!command_wrk && i1 <= i2)
		{
			/* Display the options. */
			t += sprintf(t, "%c-%c", index_to_label(i1),
				index_to_label(i2));
		}
		if (command_wrk && e1 <= e2)
		{
			/* Display the options. */
			t += sprintf(t, "%c-%c", index_to_label(e1),
				index_to_label(e2));
		}

		/* Indicate ability to "view" */
		t += sprintf(t, ", * to %s,", (command_see) ? "hide" : "see");

		/* Indicate legality of the other selection. */
		if (inven && equip)
			t += sprintf(t, " / for %s", (command_wrk) ? "Inven" : "Equip");

		/* Indicate legality of the "floor" items. */
		if (allow_floor) t += sprintf(t, " - for floor,");

		/* Finish the prompt */
		sprintf(t, " ESC) %s", pmt);

		/* Show the prompt */
		prt(tmp_val, 0, 0);


		/* Get a key */
		which = inkey();

		/* Parse it */
		switch (which)
		{
			case RESIZE_INKEY_KEY:
			{
				/* Put this screen in term */
				Term_release(nterm);
				nterm = Term_save_aux();
				break;
			}
			case ESCAPE:
			{
				command_gap = 50;
				done = TRUE;
				*err = GET_ITEM_ERROR_ABORT;
				o_ptr = NULL;
				break;
			}

			case '*':
			case '?':
			case ' ':
			{
				/* Show/hide the list */
				if (!command_see)
				{
					if (!nterm) nterm = Term_save_aux();
					command_see = TRUE;
				}
				else
				{
					Term_load_aux(nterm);
					command_see = FALSE;
				}
				break;
			}

			case '/':
			{
				/* Verify legality */
				if (!inven || !equip)
				{
					bell(0);
					break;
				}

				/* Fix screen */
				if (command_see)
				{
					Term_load_aux(nterm);
				}

				/* Switch inven/equip */
				command_wrk = !command_wrk;

				/* Need to redraw */
				break;
			}

			case '-':
			{
				/* Use floor item */
				if (allow_floor)
				{
                    /* Scan all objects in the grid */
					for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
					{
						/* Acquire object */
						o_ptr = &o_list[this_o_idx];

						/* Display description if needed. */
						object_track(o_ptr);

						/* Acquire next object */
						next_o_idx = o_ptr->next_o_idx;

						/* Skip hidden items */
						if (hidden_p(o_ptr)) continue;

						/* Skip illegal items */
						if (!item_tester_okay(o_ptr)) continue;

						/* Skip non-verified items */
						if (other_query_flag && !verify("Try", o_ptr)) continue;

						/* Skip non-acceptable items */
						if (other_query_flag && !get_item_allow(o_ptr)) continue;

						done = TRUE;
						break;
					}

					/* Outer break */
					if (done) break;
				}

				/* Oops */
				bell(0);
				break;
			}

			case '0':
			case '1': case '2': case '3':
			case '4': case '5': case '6':
			case '7': case '8': case '9':
			{
				bool tmp;
				object_type *ot;

				/* Assume no object by default. */
				o_ptr = NULL;

				ot = inventory + ((command_wrk) ? INVEN_TOTAL : INVEN_PACK) -1;

				/* XXX XXX Look up that tag */
				tmp = get_tag(&o_ptr, which, command_cmd, ot);

				ot = inventory + ((command_wrk) ? INVEN_PACK : INVEN_TOTAL) -1;

				/* Look it up in the rest of the inventory if allowed. */
				if (!tmp && equip && inven)
					tmp = get_tag(&o_ptr, which, command_cmd, ot);

				ot = o_list + c_ptr->o_idx;

				/* Look it up on the floor if allowed. */
				if (!tmp && allow_floor)
					tmp = get_tag(&o_ptr, which, command_cmd, ot);

				if (!o_ptr)
				{
					bell("No such tag");
					break;
				}

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, isupper(which));

				break;
			}

			case '\n':
			case '\r':
			{
				/* Choose "default" inventory item */
				if (!command_wrk && i1 == i2)
				{
					o_ptr = i1;
				}

				/* Choose "default" equipment item */
				else if (command_wrk && e1 == e2)
				{
					o_ptr = e1;
				}
				/* No "default" item. */
				else
				{
					bell(0);
					break;
				}

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, FALSE);

				break;
			}

			/*
			 * Select the tracked object if possible.
			 */
			case '!':
			{
				/* Get the object currently being tracked. */
				o_ptr = tracked_o_ptr;

				if (!o_ptr || !o_ptr->k_idx)
				{
					bell("No tracked object.");
					break;
				}
				else if (!is_inventory_p(o_ptr))
				{
					if (!floor)
					{
						bell("Floor items are not allowed.");
						break;
					}
				}
				else if (o_ptr < inventory+INVEN_PACK)
				{
					if (!inven)
					{
						bell("Pack items are not allowed.");
						break;
					}
				}
				else
				{
					if (!equip)
					{
						bell("Equipped items are not allowed.");
						break;
					}
				}

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, FALSE);
				break;
			}

			/*
			 * Select a worthless item if possible.
			 * Otherwise select a cursed item or nothing.
			 */
			case 'z': case 'Z':
			{
				object_type *start, *end, j_ptr[1];
				object_type *cursed = NULL, *broken = NULL, *hidden = NULL;
				/* Find the range. */
				if (command_wrk)
				{
					start = inventory+INVEN_WIELD-1;
					end = inventory+INVEN_TOTAL;
				}
				else
				{
					start = inventory;
					end = inventory+INVEN_PACK;
				}
				/* Search the items for something cursed or worthless. */
				for (o_ptr = start ; o_ptr < end; o_ptr++)
				{
					object_info_known(j_ptr, o_ptr);

					/* Skip invalid objects */
					if (!get_item_okay(o_ptr)) continue;

					/* Skip specified objects */
					if (strstr(quark_str(o_ptr->note), "!k")) continue;
					if (strstr(quark_str(o_ptr->note), "!K")) continue;

					/* Found a cursed item. */
					if (cursed_p(o_ptr)) cursed = o_ptr;

					/* Found an uncursed worthless item. */
					else if (!object_value(o_ptr, FALSE)) broken = o_ptr;

					/* Found a hidden item. */
					if (hidden_p(j_ptr)) hidden = o_ptr;
				}

				/* Select from each category in order. */
				if (hidden) o_ptr = hidden;
				else if (broken) o_ptr = broken;
				else if (cursed) o_ptr = cursed;

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, isupper(which));

				break;
			}

			/* Select the most/least valuable object in the selection. */
			case 'x': case 'X': case 'y': case 'Y':
			{
				object_type *start, *end, *j_ptr;
				bool high = strchr("Xx", which) != NULL;
				bool upper = isupper(which);
				s32b UNREAD(best_price);

				/* Can only judge value with spoil_value set. */
				if (!spoil_value) break;

				/* Find the range. */
				if (command_wrk)
				{
					start = inventory+INVEN_WIELD-1;
					end = inventory+INVEN_TOTAL;
				}
				else
				{
					start = inventory;
					end = inventory+INVEN_PACK;
				}

				/* Look through the items, finding the best value. */
				for (j_ptr = start, o_ptr = NULL; j_ptr < end; j_ptr++)
				{
					s32b this_price;
					if (!get_item_okay(j_ptr)) continue;
					this_price = object_value(j_ptr, FALSE) * j_ptr->number;
					if (o_ptr >= start)
					{
						if (high)
						{
							if (this_price < best_price) continue;
						}
						else
						{
							if (this_price > best_price) continue;
						}	
					}
					o_ptr = j_ptr;
					best_price = this_price;
				}
				/* Paranoia */
				if (!o_ptr)
				{
					bell(0);
					break;
				}

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, upper);

				break;
			}
			default:
			{
				/* Extract "query" setting */
				bool upper = isupper(which);
				if (upper) which = tolower(which);

				/* Convert letter to inventory index */
				if (!command_wrk)
				{
					o_ptr = label_to_inven(which);
				}

				/* Convert letter to equipment index */
				else
				{
					o_ptr = label_to_equip(which);
				}

				/* Check that the item is suitable in various ways. */
				get_item_valid(&o_ptr, &done, upper);

				break;
			}
		}
	}


	/* Reset the resize hook. */
	delete_resize_hook(resize_inkey);

	/* Fix the screen if necessary */
	if (command_see) Term_load_aux(nterm);

	/* Forget the saved screen, if any. */
	Term_release(nterm);

	/* Hack -- Cancel "display" */
	command_see = FALSE;


	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;

	/* Show item if possible. */
	if (o_ptr)
	{
		object_track(o_ptr);
	}

	/* Clean up */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}


	/* Clear the prompt line */
	prt("", 0, 0);
 
 #ifdef ALLOW_REPEAT
 
     if (o_ptr) repeat_push(cnv_obj_to_idx(o_ptr));
     
 #endif /* ALLOW_REPEAT */

	/* Return the object if something was picked. */
	return o_ptr;
}


