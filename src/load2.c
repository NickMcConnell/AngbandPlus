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
        /* if (z_major < x) return (TRUE); */
        /* if (z_major > x) return (FALSE); */

	/* Distinctly older, or distinctly more recent */
        /* if (z_minor < y) return (TRUE); */
        /* if (z_minor > y) return (FALSE); */

	/* Barely older, or barely more recent */
        /* if (z_patch < z) return (TRUE); */
        /* if (z_patch > z) return (FALSE); */

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
		case TV_AMMO:
                case TV_THROWING:
		case TV_RANGED:
		case TV_DIGGING:
		case TV_WEAPON:
                case TV_MSTAFF:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
                case TV_ARM_BAND:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_LITE:
		case TV_AMULET:
		case TV_RING:
                case TV_HYPNOS:
                case TV_INSTRUMENT:
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
	s16b old_dd;
	s16b old_ds;

	int i;

        u32b f1, f2, f3, f4;

	object_kind *k_ptr;

	char buf[128];

	/* Kind */
	rd_s16b(&o_ptr->k_idx);

	/* Location */
	rd_byte(&o_ptr->iy);
	rd_byte(&o_ptr->ix);

	/* Type/Subtype */
        rd_byte(&o_ptr->tval);
	rd_s16b(&o_ptr->sval);

	/* Special pval */
        rd_s32b(&o_ptr->pval);

        /* Special pval */
        rd_s32b(&o_ptr->pval2);

        /* Special pval */
        rd_s32b(&o_ptr->pval3);

	/* Resistances! */
	for (i = 0; i < MAX_RESIST; i++)
	{
		rd_s16b(&o_ptr->resistances[i]);
	}

	/* Stats bonus! */
	for (i = 0; i < 6; i++)
	{
		rd_s16b(&o_ptr->statsbonus[i]);
	}

	/* Skills bonus! */
	for (i = 0; i < SKILL_MAX; i++)
	{
		rd_s16b(&o_ptr->skillsbonus[i]);
	}

	/* Other bonus. */
	rd_s16b(&o_ptr->itemtype);
	rd_s16b(&o_ptr->itemskill);
	rd_s16b(&o_ptr->extrablows);
	rd_s16b(&o_ptr->extrashots);
	rd_s16b(&o_ptr->speedbonus);
	rd_s16b(&o_ptr->lifebonus);
	rd_s16b(&o_ptr->manabonus);
	rd_s16b(&o_ptr->infravision);
	rd_s16b(&o_ptr->spellbonus);
	rd_s16b(&o_ptr->invisibility);
	rd_s16b(&o_ptr->light);
	rd_s16b(&o_ptr->extra1);
	rd_s16b(&o_ptr->extra2);
	rd_s16b(&o_ptr->extra3);
	rd_s16b(&o_ptr->extra4);
	rd_s16b(&o_ptr->extra5);
	rd_s16b(&o_ptr->reflect);
	rd_s16b(&o_ptr->cursed);

	rd_s16b(&o_ptr->tweakpoints);
	rd_s16b(&o_ptr->disabled);

	rd_s16b(&o_ptr->brandtype);
	rd_s32b(&o_ptr->branddam);
	rd_s16b(&o_ptr->brandrad);

        rd_byte(&o_ptr->discount);
        rd_byte(&o_ptr->number);
        rd_s32b(&o_ptr->weight);

        rd_s16b(&o_ptr->name1);
        rd_s16b(&o_ptr->name2);
        rd_s16b(&o_ptr->timeout);

        rd_s32b(&o_ptr->to_h);
        rd_s32b(&o_ptr->to_d);
        rd_s32b(&o_ptr->to_a);

        rd_s32b(&o_ptr->ac);

        rd_s16b(&old_dd);
        rd_s16b(&old_ds);

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
        rd_s32b(&o_ptr->xtra1);
	rd_byte(&o_ptr->xtra2);

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4);

        rd_s16b(&o_ptr->level);
        rd_s32b(&o_ptr->kills);

	/* Read spells */
	for (i = 0; i < 20; i++)
	{
		rd_string(o_ptr->spell[i].name, 80);
                rd_string(o_ptr->spell[i].act, 80);
		rd_s16b(&o_ptr->spell[i].type);
		rd_s16b(&o_ptr->spell[i].power);
		rd_s16b(&o_ptr->spell[i].special1);
		rd_s16b(&o_ptr->spell[i].special2);
		rd_s16b(&o_ptr->spell[i].special3);
		rd_byte(&o_ptr->spell[i].summchar);
		rd_s16b(&o_ptr->spell[i].cost);
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


	/* Hack -- notice "broken" items */
	if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

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


	/* Acquire standard fields */
        /* Hack to let damages to armor be permanents. Am I evil? ;) */
        if (o_ptr->ac <= 0)
        {
                o_ptr->ac = k_ptr->ac; 
        }
        if (o_ptr->dd <= 0)
        {
                o_ptr->dd = k_ptr->dd;
        }
        if (o_ptr->ds <= 0)
        {
                o_ptr->ds = k_ptr->ds;
        }
        o_ptr->dd = old_dd;
        o_ptr->ds = old_ds;
                

	/* Acquire standard weight */
	o_ptr->weight = k_ptr->weight;

	/* Hack -- extract the "broken" flag */
	if (!o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);

	if (o_ptr->art_name) /* A random artifact */
	{
		o_ptr->dd = old_dd;
		o_ptr->ds = old_ds;
	}
	
        /* OOOHHHHH NOO!!!!!! YOU'RE DEAD!!! TOOOO BAD! :) */
        if (death)
        {
                no_more_items();
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
        rd_s32b(&m_ptr->hp);
        rd_s32b(&m_ptr->maxhp);
	rd_s16b(&m_ptr->csleep);
	rd_byte(&m_ptr->mspeed);
	rd_byte(&m_ptr->energy);
	rd_byte(&m_ptr->stunned);
	rd_byte(&m_ptr->confused);
	rd_byte(&m_ptr->monfear);
        rd_u32b(&m_ptr->smart);
        rd_byte(&m_ptr->imprinted);
        rd_s16b(&m_ptr->level);
        rd_s16b(&m_ptr->angered_pet);
        rd_s16b(&m_ptr->boss);
        rd_u32b(&m_ptr->abilities);
        rd_s16b(&m_ptr->friend);
        rd_s32b(&m_ptr->hitrate);
        rd_s32b(&m_ptr->defense);
        rd_byte(&m_ptr->animated);
        rd_s16b(&m_ptr->animdam_d);
        rd_s16b(&m_ptr->animdam_s);
        rd_s16b(&m_ptr->seallight);
	rd_s32b(&m_ptr->str);
	rd_s32b(&m_ptr->dex);
	rd_s32b(&m_ptr->mind);
	rd_s32b(&m_ptr->skill_attack);
	rd_s32b(&m_ptr->skill_ranged);
	rd_s32b(&m_ptr->skill_magic);
	rd_s32b(&m_ptr->mana);
	rd_s16b(&m_ptr->hasted);
	rd_s16b(&m_ptr->boosted);
	rd_s16b(&m_ptr->spoke);
	rd_s16b(&m_ptr->lives);
	rd_s16b(&m_ptr->summoned);
	rd_byte(&m_ptr->no_experience);
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
	rd_byte(&r_ptr->r_blows[4]);
        rd_byte(&r_ptr->r_blows[5]);
        rd_byte(&r_ptr->r_blows[6]);
        rd_byte(&r_ptr->r_blows[7]);
	rd_byte(&r_ptr->r_blows[8]);
        rd_byte(&r_ptr->r_blows[9]);
        rd_byte(&r_ptr->r_blows[10]);
        rd_byte(&r_ptr->r_blows[11]);
	rd_byte(&r_ptr->r_blows[12]);
        rd_byte(&r_ptr->r_blows[13]);
        rd_byte(&r_ptr->r_blows[14]);
        rd_byte(&r_ptr->r_blows[15]);
	rd_byte(&r_ptr->r_blows[16]);
        rd_byte(&r_ptr->r_blows[17]);
        rd_byte(&r_ptr->r_blows[18]);
        rd_byte(&r_ptr->r_blows[19]);

	/* Resistances seen of each type */
        rd_byte(&r_ptr->r_resist[0]);
        rd_byte(&r_ptr->r_resist[1]);
        rd_byte(&r_ptr->r_resist[2]);
        rd_byte(&r_ptr->r_resist[3]);
	rd_byte(&r_ptr->r_resist[4]);
        rd_byte(&r_ptr->r_resist[5]);
        rd_byte(&r_ptr->r_resist[6]);
        rd_byte(&r_ptr->r_resist[7]);
	rd_byte(&r_ptr->r_resist[8]);
        rd_byte(&r_ptr->r_resist[9]);
        rd_byte(&r_ptr->r_resist[10]);
        rd_byte(&r_ptr->r_resist[11]);
	rd_byte(&r_ptr->r_resist[12]);
        rd_byte(&r_ptr->r_resist[13]);
        rd_byte(&r_ptr->r_resist[14]);
        rd_byte(&r_ptr->r_resist[15]);
	rd_byte(&r_ptr->r_resist[16]);
        rd_byte(&r_ptr->r_resist[17]);
        rd_byte(&r_ptr->r_resist[18]);
        rd_byte(&r_ptr->r_resist[19]);

	/* Known spells */
        rd_byte(&r_ptr->r_spells[0]);
        rd_byte(&r_ptr->r_spells[1]);
        rd_byte(&r_ptr->r_spells[2]);
        rd_byte(&r_ptr->r_spells[3]);
	rd_byte(&r_ptr->r_spells[4]);
        rd_byte(&r_ptr->r_spells[5]);
        rd_byte(&r_ptr->r_spells[6]);
        rd_byte(&r_ptr->r_spells[7]);
	rd_byte(&r_ptr->r_spells[8]);
        rd_byte(&r_ptr->r_spells[9]);
        rd_byte(&r_ptr->r_spells[10]);
        rd_byte(&r_ptr->r_spells[11]);
	rd_byte(&r_ptr->r_spells[12]);
        rd_byte(&r_ptr->r_spells[13]);
        rd_byte(&r_ptr->r_spells[14]);
        rd_byte(&r_ptr->r_spells[15]);
	rd_byte(&r_ptr->r_spells[16]);
        rd_byte(&r_ptr->r_spells[17]);
        rd_byte(&r_ptr->r_spells[18]);
        rd_byte(&r_ptr->r_spells[19]);

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

static errr rd_store(int store_number)
{
	store_type *st_ptr = &stores[store_number];

	int j;

	byte own;
	s16b num;

	/* Read the basic info */
	rd_s32b(&st_ptr->store_open);
	rd_s16b(&st_ptr->insult_cur);
	rd_byte(&own);
	rd_s16b(&num);
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
		if (store_number == 7)
		{
			if (st_ptr->stock_num < HOME_INVEN_MAX)
			{
				int k = st_ptr->stock_num++;

				/* Acquire the item */
				object_copy(&st_ptr->stock[k], q_ptr);
			}
		}
		else
		{
			if (st_ptr->stock_num < STORE_INVEN_MAX)
			{
				int k = st_ptr->stock_num++;

				/* Acquire the item */
				object_copy(&st_ptr->stock[k], q_ptr);
			}
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

	/* Strip name */
	rd_string(buf, 64);

        /* Newer ghost */
        /* Strip old data */
        strip_bytes(60);
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
	rd_byte(&p_ptr->pclass);
	rd_byte(&p_ptr->psex);
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
        for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_cnt[i]);
        for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_los[i]);
	for (i = 0; i < 6; i++) rd_s16b(&p_ptr->stat_mut[i]);

	strip_bytes(24);

	rd_s32b(&p_ptr->au);

	rd_s32b(&p_ptr->max_exp);
	rd_s32b(&p_ptr->exp);
	rd_u16b(&p_ptr->exp_frac);

	rd_s16b(&p_ptr->lev);

        rd_s16b(&p_ptr->town_num);
	rd_string(p_ptr->town_name, 80);

        /* Read arena and rewards information */
        rd_s16b(&p_ptr->inside_quest);
	rd_string(p_ptr->quest_name, 80);
	rd_s16b(&p_ptr->death_dialog);
	rd_s16b(&p_ptr->eventdeath);
	rd_s16b(&p_ptr->eventdeathset);

        rd_s16b(&p_ptr->oldpx);
        rd_s16b(&p_ptr->oldpy);

        rd_s32b(&p_ptr->mhp);
        rd_s32b(&p_ptr->chp);
	rd_u16b(&p_ptr->chp_frac);

        rd_s32b(&p_ptr->msp);
        rd_s32b(&p_ptr->csp);
	rd_u16b(&p_ptr->csp_frac);

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

        rd_s16b(&p_ptr->tim_esp);
        rd_s16b(&p_ptr->wraith_form);
        rd_s16b(&p_ptr->tim_ffall);
        rd_s16b(&p_ptr->tim_invisible);
        rd_s16b(&p_ptr->tim_inv_pow);

        rd_u32b(&p_ptr->muta1);
        rd_u32b(&p_ptr->muta2);
        rd_u32b(&p_ptr->muta3);

	rd_byte(&p_ptr->confusing);
	rd_byte(&tmp8u);        /* oops */
	rd_byte(&p_ptr->searching);
	rd_byte(&p_ptr->maximize);
	rd_byte(&p_ptr->preserve);
	rd_byte(&p_ptr->special);
	rd_byte(&special_flag);
        rd_byte(&p_ptr->allow_one_death);

        rd_u16b(&no_breeds);

        /* Future use */
	for (i = 0; i < 48; i++) rd_byte(&tmp8u);

        /* Read the incarnation things */
        rd_u16b(&p_ptr->body_monster);
        rd_byte(&p_ptr->disembodied);

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

        /* Magic Spells! */
        rd_magic_spells();

	/* Monster Magics! */
	rd_monster_magics();

	/* Random dungeon! */
	rd_random_dungeon();

	/* Random boss! */
	rd_random_boss();

	/* Random monsters! */
	rd_random_monsters();

	/* Read global object */
	rd_global_object();

	/* Read songs. */
	rd_songs();

	/* Read stores inventories. */
	for (i = 0; i < MAX_STORES; i++)
	{
		rd_store(i);
	}

        /* Player skills/abilities */
        rd_s32b(&p_ptr->ability_points);
        rd_s16b(&p_ptr->memorized);
        rd_s16b(&p_ptr->elemlord);
        rd_s16b(&p_ptr->statpoints);
        rd_s16b(&p_ptr->skillpoints);

	for (i = 0; i < SKILL_MAX; i++)
	{
		rd_s16b(&p_ptr->skill_base[i]);
	}

        rd_s16b(&p_ptr->str_boost);
        rd_s16b(&p_ptr->str_boost_dur);
        rd_s16b(&p_ptr->int_boost);
        rd_s16b(&p_ptr->int_boost_dur);
        rd_s16b(&p_ptr->wis_boost);
        rd_s16b(&p_ptr->wis_boost_dur);
        rd_s16b(&p_ptr->dex_boost);
        rd_s16b(&p_ptr->dex_boost_dur);
        rd_s16b(&p_ptr->con_boost);
        rd_s16b(&p_ptr->con_boost_dur);
        rd_s16b(&p_ptr->chr_boost);
        rd_s16b(&p_ptr->chr_boost_dur);
        rd_s16b(&p_ptr->pres);
        rd_s16b(&p_ptr->pres_dur);
        rd_s16b(&p_ptr->mres);
        rd_s16b(&p_ptr->mres_dur);
        rd_s32b(&p_ptr->ac_boost);
        rd_s32b(&p_ptr->ac_boost_dur);
        rd_s16b(&p_ptr->elem_shield);

        rd_s16b(&p_ptr->elemental);
        rd_u32b(&p_ptr->elemental_effects);
        rd_s16b(&p_ptr->alteration);
        rd_u32b(&p_ptr->alteration_effects);
        rd_s16b(&p_ptr->healing);
        rd_u32b(&p_ptr->healing_effects);
        rd_s16b(&p_ptr->conjuration);
        rd_u32b(&p_ptr->conjuration_effects);
        rd_s16b(&p_ptr->divination);
        rd_u32b(&p_ptr->divination_effects);

        for (i = 0; i < MAX_CLASS; ++i) rd_s16b(&p_ptr->class_level[i]);
        for (i = 0; i < MAX_CLASS; ++i) rd_s16b(&p_ptr->class_kills[i]);
        for (i = 0; i < MAX_ABILITIES; ++i) rd_s16b(&p_ptr->abilities[i]);
        rd_s16b(&p_ptr->num_abilities);
	for (i = 0; i < 36; ++i) rd_s16b(&p_ptr->abilities_powers[i]);
	for (i = 0; i < 20; ++i) rd_s16b(&p_ptr->abilities_monster_attacks[i]);
	for (i = 0; i < 20; ++i) rd_s16b(&p_ptr->abilities_monster_spells[i]);
	rd_u32b(&p_ptr->boss_abilities);
        rd_s16b(&p_ptr->magic_mode);
        rd_byte(&p_ptr->auraon);
        rd_s32b(&p_ptr->deathcount);
        rd_s16b(&p_ptr->guardconfuse);
	rd_byte(&p_ptr->learning);
	rd_s16b(&p_ptr->startx);
	rd_s16b(&p_ptr->starty);
	rd_s16b(&p_ptr->cur_wid);
	rd_s16b(&p_ptr->cur_hgt);
	rd_s16b(&p_ptr->alignment);
	rd_s16b(&p_ptr->cursed);
	rd_s16b(&p_ptr->dualwield);
	for (i = 0; i < 30000; ++i) rd_s16b(&p_ptr->events[i]);
	for (i = 0; i < 30000; ++i) rd_s16b(&p_ptr->towns[i]);
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
        s32b tmp32s;

	u16b limit;

	cave_type *c_ptr;


	/*** Basic info ***/

	/* Header info */
        rd_s16b(&dun_level);
        rd_s16b(&dungeon_type);
        rd_s16b(&num_repro);
	rd_s16b(&py);
	rd_s16b(&px);
	rd_s16b(&cur_hgt);
	rd_s16b(&cur_wid);
	rd_s16b(&max_panel_rows);
	rd_s16b(&max_panel_cols);

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
		/*** Run length decoding ***/

		/* Load the dungeon data */
		for (x = y = 0; y < ymax; )
		{
			/* Grab RLE info */
			rd_byte(&count);
                        rd_s32b(&tmp32s);

			/* Apply the RLE info */
			for (i = count; i > 0; i--)
			{
				/* Access the cave */
				c_ptr = &cave[y][x];

				/* Extract "feat" */
                                c_ptr->field_damage = tmp32s;

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
				c_ptr->event = tmp16s;

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
				c_ptr->eventtype = tmp16s;

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
				c_ptr->eventextra = tmp16s;

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
				c_ptr->eventextra2 = tmp16s;

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
				c_ptr->eventcond = tmp16s;

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
				c_ptr->eventcondval = tmp16s;

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
				c_ptr->eventset = tmp16s;

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
				c_ptr->eventsetval = tmp16s;

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
				c_ptr->script = tmp16s;

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
				c_ptr->owner = tmp16s;

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

		/* Read scripts! */
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				/* Get the cave */
				c_ptr = &cave[y][x];
			
				rd_string(c_ptr->script_name, 120);
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
        /*rd_options();*/
        /*if (arg_fiddle) note("Loaded Option Flags");*/


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

        /* Init the ghost & player monsters */
        /*for(i = 0; i < MAX_GHOSTS; i++)
                rd_string(ghost_file[i], 20);

        for(i = 0; i < MAX_GHOSTS; i++)
        {
                monster_race *r_ptr;

                r_ptr = &r_info[GHOST_R_IDX_HEAD + i];

                rd_string(r_name + r_ptr->name, 80);
                rd_string(r_text + r_ptr->text, 320);

                rd_byte(&r_ptr->hdice);
                rd_byte(&r_ptr->hside);

                rd_s16b(&r_ptr->ac);

                rd_s16b(&r_ptr->sleep);
                rd_byte(&r_ptr->aaf);
                rd_byte(&r_ptr->speed);

                rd_s32b(&r_ptr->mexp);
                rd_s32b(&r_ptr->weight);

                rd_byte(&r_ptr->freq_inate);
                rd_byte(&r_ptr->freq_spell);

                rd_u32b(&r_ptr->flags1);
                rd_u32b(&r_ptr->flags2);
                rd_u32b(&r_ptr->flags3);
                rd_u32b(&r_ptr->flags4);
                rd_u32b(&r_ptr->flags5);
                rd_u32b(&r_ptr->flags6);
                rd_u32b(&r_ptr->flags7);
                rd_u32b(&r_ptr->flags8);
                rd_u32b(&r_ptr->flags9);

                for(j = 0; j < 4; j++)
                {
                        rd_byte(&r_ptr->blow[j].method);
                        rd_byte(&r_ptr->blow[j].effect);
                        rd_byte(&r_ptr->blow[j].d_dice);
                        rd_byte(&r_ptr->blow[j].d_side);
                }

                rd_byte(&r_ptr->level);
                rd_byte(&r_ptr->rarity);

                rd_byte(&r_ptr->d_attr);
                rd_byte(&r_ptr->d_char);

                rd_byte(&r_ptr->x_attr);
                rd_byte(&r_ptr->x_char);

                rd_s16b(&r_ptr->max_num);
                rd_byte(&r_ptr->cur_num);

                if(ghost_file[i][0] == 0)
                {
                        init_ghost_info(i);
                }
        } */
#ifndef USE_GHOSTS
        /*for(i = 0; i < MAX_GHOSTS; i++)
                ghost_file[i][0] = 0;*/
#endif /* USE_GHOSTS */

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
	}
	if (arg_fiddle) note("Loaded Object Memory");

	/*
	 * Initialize arena and rewards information
	 */
	p_ptr->inside_quest = 0;

	/* Start in town 1 */
	p_ptr->town_num = 1;

        {

		/* Position in the wilderness */
		rd_s32b(&p_ptr->wild_x);
		rd_s32b(&p_ptr->wild_y);
		rd_s16b(&p_ptr->wild_startx);
		rd_s16b(&p_ptr->wild_starty);
		rd_byte(&p_ptr->wild_mode);
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

        /* Read the traps flags */
        for (i = 0; i < tmp16u; i++)
        {
                rd_byte(&t_info[i].ident);
        }
        if (arg_fiddle) note("Loaded Traps");
        }


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
        else if (death)
	{
		int x, y;
		/* Dead players have no dungeon */
                note("You have lost all your items!!!");
                no_more_items();
                p_ptr->au = 0;
                p_ptr->chp = 1;
                p_ptr->inside_quest = 0;
                p_ptr->cut = 0;
                p_ptr->stun = 0;
                p_ptr->poisoned = 0;
		p_ptr->confused = 0;
		p_ptr->afraid = 0;
		p_ptr->blind = 0;
                p_ptr->word_recall = 0;
                /* max_dlv[p_ptr->recall_dungeon] -= 2;*/
                /* if (max_dlv[p_ptr->recall_dungeon] < 0) max_dlv[p_ptr->recall_dungeon] = 0;*/
                restore_level();
                dun_level = 0;

		p_ptr->town_num = p_ptr->events[29998];

		/* Restart to the latest town's startx/starty */
		for (x = 0; x < wild_max_x; x++)
		{
			for (y = 0; y < wild_max_y; y++)
			{
				if (wild[x][y].town == p_ptr->town_num)
				{
					p_ptr->wild_x = x;
					p_ptr->wild_y = y;
				}
			}
		}
		/*if (p_ptr->wild_mode || p_ptr->recall_dungeon == 200)*/
		/*{*/
			p_ptr->startx = get_town_startx(p_ptr->town_num);
			p_ptr->starty = get_town_starty(p_ptr->town_num);
			if (p_ptr->recall_dungeon == 200) p_ptr->recall_dungeon = 0;
		/*}*/

                /* If a vampire, revive at night! */
                if (p_ptr->prace == RACE_VAMPIRE)
                {
                        turn = (10L * TOWN_DAWN / 2) + 1;
                }

                p_ptr->leaving = TRUE;
		/*generate_cave();*/

                death = FALSE;

		/* Read the ghost info */
		rd_ghost();
	}
        

#ifdef VERIFY_CHECKSUMS

	/* Save the checksum */
	n_v_check = v_check;

	/* Read the old checksum */
	rd_u32b(&o_v_check);

	/* Verify */
        /* NOT IN NEWANGBAND!!!! :) */
        /* if (o_v_check != n_v_check) */
        /* {                           */
                /* note("Invalid checksum"); */
                /* return (11); */
        /* } */


	/* Save the encoded checksum */
	n_x_check = x_check;

	/* Read the checksum */
	rd_u32b(&o_x_check);


	/* Verify */
        /* f (o_x_check != n_x_check) */
        /* {                          */
           /*     note("Invalid encoded checksum"); */
           /*     return (11); */
        /* }                   */

#endif


	/* Hack -- no ghosts */
	r_info[max_r_idx-1].max_num = 0;

	/* Reset current weapon. */
	current_weapon = &inventory[INVEN_WIELD];

	/* Success */
	return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
	errr err;

        /* First, load the options */
        load_options();

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

/* Read the spells! */
void rd_magic_spells()
{
        int x, i;
        for (x = 0; x <= 30; x++)
        {
                magic_spells *spell_ptr = &magic_spell[x];
                rd_string(spell_ptr->name, 30);
                for (i = 0; i < 5; ++i) rd_s16b(&spell_ptr->school[i]);
                for (i = 0; i < 5; ++i) rd_s16b(&spell_ptr->effect[i]);
                for (i = 0; i < 5; ++i) rd_s16b(&spell_ptr->shape[i]);
                for (i = 0; i < 5; ++i) rd_s32b(&spell_ptr->power[i]);
                for (i = 0; i < 5; ++i) rd_s16b(&spell_ptr->radius[i]);
                for (i = 0; i < 5; ++i) rd_s16b(&spell_ptr->type[i]);
                for (i = 0; i < 5; ++i) rd_s32b(&spell_ptr->manacost[i]);
                rd_byte(&spell_ptr->schar1);
                rd_byte(&spell_ptr->schar2);
                rd_byte(&spell_ptr->schar3);
                rd_byte(&spell_ptr->schar4);
                rd_byte(&spell_ptr->schar5);
                rd_string(spell_ptr->sspeci1, 30);
                rd_string(spell_ptr->sspeci2, 30);
                rd_string(spell_ptr->sspeci3, 30);
                rd_string(spell_ptr->sspeci4, 30);
                rd_string(spell_ptr->sspeci5, 30);
                rd_s32b(&spell_ptr->finalcost);
                rd_byte(&spell_ptr->created);
        }
}

/* Read monster magics! */
void rd_monster_magics()
{
        int x;
        for (x = 0; x <= 15; x++)
        {
                monster_magics *mspell_ptr = &monster_magic[x];
                rd_string(mspell_ptr->name, 30);
		rd_string(mspell_ptr->act, 30);
                rd_s16b(&mspell_ptr->type);
                rd_s16b(&mspell_ptr->power);
		rd_s16b(&mspell_ptr->special1);
		rd_s16b(&mspell_ptr->special2);
		rd_s16b(&mspell_ptr->special3);
                rd_byte(&mspell_ptr->summchar);
		rd_s16b(&mspell_ptr->cost);
        }
}

/* Load the user options */
void load_options(void)
{
        char name[1024];
  
        /* Construct name */
        path_build(name, 1024, ANGBAND_DIR_PREF, "options.prf");

        /* Open the file */
        fff = my_fopen(name, "rb");

        /* Read the options */
        rd_options();

        /* Done */
        my_fclose(fff);
}

/* Load the random dungeon! */
void rd_random_dungeon()
{
	dungeon_info_type *d_ptr;
	d_ptr = &d_info[200];
	
	rd_u32b(&d_ptr->name);
	rd_u32b(&d_ptr->text);
        rd_s16b(&d_ptr->floor1);
        rd_byte(&d_ptr->floor_percent1);
        rd_s16b(&d_ptr->floor2);
        rd_byte(&d_ptr->floor_percent2);
        rd_s16b(&d_ptr->floor3);
        rd_byte(&d_ptr->floor_percent3);
        rd_s16b(&d_ptr->outer_wall);
        rd_s16b(&d_ptr->inner_wall);
        rd_s16b(&d_ptr->fill_type1);
        rd_byte(&d_ptr->fill_percent1);
        rd_s16b(&d_ptr->fill_type2);
        rd_byte(&d_ptr->fill_percent2);
        rd_s16b(&d_ptr->fill_type3);
        rd_byte(&d_ptr->fill_percent3);
        rd_s16b(&d_ptr->mindepth);
        rd_s16b(&d_ptr->maxdepth);
        rd_byte(&d_ptr->principal);
        rd_byte(&d_ptr->next);
        rd_byte(&d_ptr->min_plev);
        rd_byte(&d_ptr->mode);

        rd_s16b(&d_ptr->min_m_alloc_level);
        rd_s16b(&d_ptr->max_m_alloc_chance);

        rd_u32b(&d_ptr->flags1);

        /*d_ptr->mflags1;
        d_ptr->mflags2;
        d_ptr->mflags3;
        d_ptr->mflags4;
        d_ptr->mflags5;
        d_ptr->mflags6;
        d_ptr->mflags7;
        d_ptr->mflags8;
        d_ptr->mflags9;*/

        /* d_ptr->r_char[5]; */
        rd_s16b(&d_ptr->final_artifact);
        rd_s16b(&d_ptr->final_guardian);

        rd_byte(&d_ptr->special_percent);
	rd_s16b(&d_ptr->quest);

	d_ptr = &d_info[201];
	
	rd_u32b(&d_ptr->name);
	rd_u32b(&d_ptr->text);
        rd_s16b(&d_ptr->floor1);
        rd_byte(&d_ptr->floor_percent1);
        rd_s16b(&d_ptr->floor2);
        rd_byte(&d_ptr->floor_percent2);
        rd_s16b(&d_ptr->floor3);
        rd_byte(&d_ptr->floor_percent3);
        rd_s16b(&d_ptr->outer_wall);
        rd_s16b(&d_ptr->inner_wall);
        rd_s16b(&d_ptr->fill_type1);
        rd_byte(&d_ptr->fill_percent1);
        rd_s16b(&d_ptr->fill_type2);
        rd_byte(&d_ptr->fill_percent2);
        rd_s16b(&d_ptr->fill_type3);
        rd_byte(&d_ptr->fill_percent3);
        rd_s16b(&d_ptr->mindepth);
        rd_s16b(&d_ptr->maxdepth);
        rd_byte(&d_ptr->principal);
        rd_byte(&d_ptr->next);
        rd_byte(&d_ptr->min_plev);
        rd_byte(&d_ptr->mode);

        rd_s16b(&d_ptr->min_m_alloc_level);
        rd_s16b(&d_ptr->max_m_alloc_chance);

        rd_u32b(&d_ptr->flags1);

        /*d_ptr->mflags1;
        d_ptr->mflags2;
        d_ptr->mflags3;
        d_ptr->mflags4;
        d_ptr->mflags5;
        d_ptr->mflags6;
        d_ptr->mflags7;
        d_ptr->mflags8;
        d_ptr->mflags9;*/

        /* d_ptr->r_char[5]; */
        rd_s16b(&d_ptr->final_artifact);
        rd_s16b(&d_ptr->final_guardian);

        rd_byte(&d_ptr->special_percent);
	rd_s16b(&d_ptr->quest);
}

/* Read the random boss! */
void rd_random_boss()
{
	int i;
	monster_race *r_ptr;
	r_ptr = &r_info[1030];
	
	/*rd_u32b(&r_ptr->name);*/
	/*rd_u32b(&r_ptr->text);*/
	rd_string(&r_ptr->name_char, 200);

	rd_byte(&r_ptr->hdice);
	rd_byte(&r_ptr->hside);

	rd_s16b(&r_ptr->ac);

	rd_s16b(&r_ptr->sleep);
	rd_byte(&r_ptr->aaf);
	rd_byte(&r_ptr->speed);

	rd_s32b(&r_ptr->mexp);

        rd_s32b(&r_ptr->weight);

	rd_byte(&r_ptr->freq_inate);
	rd_byte(&r_ptr->freq_spell);

	rd_u32b(&r_ptr->flags1);
	rd_u32b(&r_ptr->flags2);
	rd_u32b(&r_ptr->flags3);
	rd_u32b(&r_ptr->flags4);
	rd_u32b(&r_ptr->flags5);
	rd_u32b(&r_ptr->flags6);
	rd_u32b(&r_ptr->flags7);
	rd_u32b(&r_ptr->flags8);
	rd_u32b(&r_ptr->flags9);

        rd_byte(&r_ptr->level);
	rd_byte(&r_ptr->rarity);


	rd_byte(&r_ptr->d_attr);
	rd_byte(&r_ptr->d_char);


	rd_byte(&r_ptr->x_attr);
	rd_byte(&r_ptr->x_char);


        rd_s16b(&r_ptr->max_num);

	rd_byte(&r_ptr->cur_num);


	rd_s16b(&r_ptr->r_sights);
	rd_s16b(&r_ptr->r_deaths);

	rd_s16b(&r_ptr->r_pkills);
	rd_s16b(&r_ptr->r_tkills);

	rd_byte(&r_ptr->r_wake);
	rd_byte(&r_ptr->r_ignore);

	rd_byte(&r_ptr->r_xtra1);
	rd_byte(&r_ptr->r_xtra2);

	rd_byte(&r_ptr->r_drop_gold);
	rd_byte(&r_ptr->r_drop_item);

	rd_byte(&r_ptr->r_cast_inate);
	rd_byte(&r_ptr->r_cast_spell);

	for (i = 0; i < 20; i++)
	{
		rd_byte(&r_ptr->r_blows[i]);
		rd_byte(&r_ptr->r_spells[i]);
	}
	for (i = 0; i < MAX_RESIST; i++)
	{
		rd_byte(&r_ptr->r_resist[i]);
	}
	rd_u32b(&r_ptr->r_flags1);
	rd_u32b(&r_ptr->r_flags2);
	rd_u32b(&r_ptr->r_flags3);
	rd_u32b(&r_ptr->r_flags4);
	rd_u32b(&r_ptr->r_flags5);
	rd_u32b(&r_ptr->r_flags6);
	rd_u32b(&r_ptr->r_flags7);
        rd_u32b(&r_ptr->r_flags8);
        rd_u32b(&r_ptr->r_flags9);

        rd_byte(&r_ptr->on_saved);
	
	rd_s16b(&r_ptr->str);
	rd_s16b(&r_ptr->dex);
	rd_s16b(&r_ptr->mind);
	rd_s16b(&r_ptr->skill_attack);
	rd_s16b(&r_ptr->skill_ranged);
	rd_s16b(&r_ptr->skill_magic);
	
	rd_s16b(&r_ptr->countertype);
	rd_s16b(&r_ptr->counterchance);

	for (i = 0; i < MAX_RESIST; i++)
	{
		rd_s16b(&r_ptr->resistances[i]);
	}

	rd_s16b(&r_ptr->spellchance);

	rd_s16b(&r_ptr->attacks);
	for (i = 0; i < 20; i++)
	{
		monster_attack *att_ptr = &r_ptr->attack[i];
                rd_string(att_ptr->name, 80);
		rd_string(att_ptr->act, 80);
		rd_s16b(&att_ptr->type);
		rd_s16b(&att_ptr->effect);
		rd_s16b(&att_ptr->ddice);
		rd_s16b(&att_ptr->dside);
		rd_s16b(&att_ptr->element);
		rd_s16b(&att_ptr->special1);
		rd_s16b(&att_ptr->special2);
	}
	rd_s16b(&r_ptr->spells);
	for (i = 0; i < 20; i++)
	{
		monster_spell *sp_ptr = &r_ptr->spell[i];
                rd_string(sp_ptr->name, 80);
		rd_string(sp_ptr->act, 80);
		rd_s16b(&sp_ptr->type);
		rd_s16b(&sp_ptr->power);
		rd_s16b(&sp_ptr->special1);
		rd_s16b(&sp_ptr->special2);
		rd_s16b(&sp_ptr->special3);
		rd_byte(&sp_ptr->summchar);
		rd_s16b(&sp_ptr->cost);
	}
	
	rd_s16b(&r_ptr->treasuretval);
	rd_s16b(&r_ptr->treasuresval);
	rd_s16b(&r_ptr->treasurechance);
	rd_s16b(&r_ptr->treasuremagic);
	rd_s16b(&r_ptr->event);
	rd_s16b(&r_ptr->extra1);
	rd_s16b(&r_ptr->extra2);
	rd_s16b(&r_ptr->fixedlevel);
	rd_s16b(&r_ptr->townnum);
	rd_s16b(&r_ptr->dunnum);
	rd_s16b(&r_ptr->lives);

	/* Save the name. */
	/*r_ptr->name = ++r_head->name_size;*/

	/* Append chars to the name */
	/*strcpy(r_name + r_head->name_size, r_ptr->name_char);*/
}

/* Read random monsters */
void rd_random_monsters()
{
	int i;
	int j;
	monster_race *r_ptr;
	for (j = 2050; j <= 2099; j++)
	{
		r_ptr = &r_info[j];
	
		/*rd_u32b(&r_ptr->name);*/
		/*rd_u32b(&r_ptr->text);*/
		rd_string(&r_ptr->name_char, 200);

		rd_byte(&r_ptr->hdice);
		rd_byte(&r_ptr->hside);

		rd_s16b(&r_ptr->ac);

		rd_s16b(&r_ptr->sleep);
		rd_byte(&r_ptr->aaf);
		rd_byte(&r_ptr->speed);

		rd_s32b(&r_ptr->mexp);

        	rd_s32b(&r_ptr->weight);

		rd_byte(&r_ptr->freq_inate);
		rd_byte(&r_ptr->freq_spell);

		rd_u32b(&r_ptr->flags1);
		rd_u32b(&r_ptr->flags2);
		rd_u32b(&r_ptr->flags3);
		rd_u32b(&r_ptr->flags4);
		rd_u32b(&r_ptr->flags5);
		rd_u32b(&r_ptr->flags6);
		rd_u32b(&r_ptr->flags7);
		rd_u32b(&r_ptr->flags8);
		rd_u32b(&r_ptr->flags9);

        	rd_byte(&r_ptr->level);
		rd_byte(&r_ptr->rarity);


		rd_byte(&r_ptr->d_attr);
		rd_byte(&r_ptr->d_char);


		rd_byte(&r_ptr->x_attr);
		rd_byte(&r_ptr->x_char);


        	rd_s16b(&r_ptr->max_num);

		rd_byte(&r_ptr->cur_num);


		rd_s16b(&r_ptr->r_sights);
		rd_s16b(&r_ptr->r_deaths);

		rd_s16b(&r_ptr->r_pkills);
		rd_s16b(&r_ptr->r_tkills);

		rd_byte(&r_ptr->r_wake);
		rd_byte(&r_ptr->r_ignore);

		rd_byte(&r_ptr->r_xtra1);
		rd_byte(&r_ptr->r_xtra2);

		rd_byte(&r_ptr->r_drop_gold);
		rd_byte(&r_ptr->r_drop_item);

		rd_byte(&r_ptr->r_cast_inate);
		rd_byte(&r_ptr->r_cast_spell);

		for (i = 0; i < 20; i++)
		{
			rd_byte(&r_ptr->r_blows[i]);
			rd_byte(&r_ptr->r_spells[i]);
		}
		for (i = 0; i < MAX_RESIST; i++)
		{
			rd_byte(&r_ptr->r_resist[i]);
		}
		rd_u32b(&r_ptr->r_flags1);
		rd_u32b(&r_ptr->r_flags2);
		rd_u32b(&r_ptr->r_flags3);
		rd_u32b(&r_ptr->r_flags4);
		rd_u32b(&r_ptr->r_flags5);
		rd_u32b(&r_ptr->r_flags6);
		rd_u32b(&r_ptr->r_flags7);
        	rd_u32b(&r_ptr->r_flags8);
        	rd_u32b(&r_ptr->r_flags9);

        	rd_byte(&r_ptr->on_saved);
	
		rd_s16b(&r_ptr->str);
		rd_s16b(&r_ptr->dex);
		rd_s16b(&r_ptr->mind);
		rd_s16b(&r_ptr->skill_attack);
		rd_s16b(&r_ptr->skill_ranged);
		rd_s16b(&r_ptr->skill_magic);
	
		rd_s16b(&r_ptr->countertype);
		rd_s16b(&r_ptr->counterchance);

		for (i = 0; i < MAX_RESIST; i++)
		{
			rd_s16b(&r_ptr->resistances[i]);
		}

		rd_s16b(&r_ptr->spellchance);

		rd_s16b(&r_ptr->attacks);
		for (i = 0; i < 20; i++)
		{
			monster_attack *att_ptr = &r_ptr->attack[i];
                	rd_string(att_ptr->name, 80);
			rd_string(att_ptr->act, 80);
			rd_s16b(&att_ptr->type);
			rd_s16b(&att_ptr->effect);
			rd_s16b(&att_ptr->ddice);
			rd_s16b(&att_ptr->dside);
			rd_s16b(&att_ptr->element);
			rd_s16b(&att_ptr->special1);
			rd_s16b(&att_ptr->special2);
		}
		rd_s16b(&r_ptr->spells);
		for (i = 0; i < 20; i++)
		{
			monster_spell *sp_ptr = &r_ptr->spell[i];
                	rd_string(sp_ptr->name, 80);
			rd_string(sp_ptr->act, 80);
			rd_s16b(&sp_ptr->type);
			rd_s16b(&sp_ptr->power);
			rd_s16b(&sp_ptr->special1);
			rd_s16b(&sp_ptr->special2);
			rd_s16b(&sp_ptr->special3);
			rd_byte(&sp_ptr->summchar);
			rd_s16b(&sp_ptr->cost);
		}
	
		rd_s16b(&r_ptr->treasuretval);
		rd_s16b(&r_ptr->treasuresval);
		rd_s16b(&r_ptr->treasurechance);
		rd_s16b(&r_ptr->treasuremagic);
		rd_s16b(&r_ptr->event);
		rd_s16b(&r_ptr->extra1);
		rd_s16b(&r_ptr->extra2);
		rd_s16b(&r_ptr->fixedlevel);
		rd_s16b(&r_ptr->townnum);
		rd_s16b(&r_ptr->dunnum);
		rd_s16b(&r_ptr->lives);

		/* Save the name. */
		/*r_ptr->name = ++r_head->name_size;*/

		/* Append chars to the name */
		/*strcpy(r_name + r_head->name_size, r_ptr->name_char);*/
	}
}

void rd_global_object()
{
	object_type *o_ptr = &global_object;

	rd_item(o_ptr);
}

/* Read the songs! */
void rd_songs()
{
        int x;
        for (x = 0; x <= 15; x++)
        {
                music_songs *song_ptr = &music_song[x];
                rd_string(song_ptr->name, 80);
                rd_s16b(&song_ptr->type);
                rd_s16b(&song_ptr->power);
		rd_s16b(&song_ptr->element);
		rd_s16b(&song_ptr->radius);
		rd_s16b(&song_ptr->cost);
                rd_byte(&song_ptr->created);
        }
}