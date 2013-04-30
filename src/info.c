/* File: info.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.6
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"

/*
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */
#define OBJECT_FLAGS_RANDOM 3 /* Only known random flags */


/*
 * Modes of spell_desc()
 */
#define SPELL_TARGET_NORMAL   1 /* Target selected normally */
#define SPELL_TARGET_SELF     2 /* Always targets self */
#define SPELL_TARGET_AIMED    3 /* Always targets aimed target */
#define SPELL_TARGET_COATED   4 /* Target applied from a weapon attack */
#define SPELL_TARGET_EXPLODE   5 /* Always targets radius 1 ball attack */


/*
 * Modes of list_object_flags()
 */
#define LIST_FLAGS_CAN   1 /* Target selected normally */
#define LIST_FLAGS_MAY     2 /* Always targets self */
#define LIST_FLAGS_NOT    3 /* Always targets aimed target */


/*
 * Calculate the multiplier we'll get with a given bow type.
 */
int bow_multiplier(int sval)
{
	switch (sval)
	{
		case SV_SLING:
		case SV_SHORT_BOW:
		case SV_HAND_XBOW:
			return (2);
		case SV_LONG_BOW:
		case SV_LIGHT_XBOW:
			return (3);
		case SV_HEAVY_XBOW:
			return (4);
		default:
			return (0); /* instruments, etc. */
	}
}


/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4)
{
	object_kind *k_ptr;

	if (mode != OBJECT_FLAGS_FULL)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = (*f4) = 0L;

		if (mode != OBJECT_FLAGS_RANDOM)
		{
			/* Add flags object is known to have */
			*f1 |= o_ptr->can_flags1;
			*f2 |= o_ptr->can_flags2;
			*f3 |= o_ptr->can_flags3;
			*f4 |= o_ptr->can_flags4;

			/* Hack: throwing is always obvious */
			if (k_info[o_ptr->k_idx].flags3 & TR3_THROWING)
			  *f3 |= TR3_THROWING;

			return;
		}

		/* Must be identified */
		if (!object_named_p(o_ptr)) return;
	}

	if (mode != OBJECT_FLAGS_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;
		(*f4) = k_ptr->flags4;

		if (mode == OBJECT_FLAGS_FULL)
		{
			/* Artifact */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &a_info[o_ptr->name1];

				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
				(*f4) = a_ptr->flags4;
			}

			/* Ego-item */
			if (o_ptr->name2)
			{
				ego_item_type *e_ptr = &e_info[o_ptr->name2];

				(*f1) |= e_ptr->flags1;
				(*f2) |= e_ptr->flags2;
				(*f3) |= e_ptr->flags3;
				(*f4) |= e_ptr->flags4;
			}
		}

		if (mode == OBJECT_FLAGS_KNOWN)
		{
			/* Obvious artifact flags */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &a_info[o_ptr->name1];

				/* Obvious flags (pval) */
				(*f1) |= (a_ptr->flags1 & (TR1_PVAL_MASK));

				(*f2) |= (a_ptr->flags2 & (TR2_IGNORE_MASK));
			}
		}
	}

	if (mode != OBJECT_FLAGS_FULL)
	{
		bool spoil = FALSE;

#ifdef SPOIL_ARTIFACTS
		/* Full knowledge for some artifacts */
		if (artifact_p(o_ptr) || cheat_lore) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
		/* Full knowledge for some ego-items */
		if (ego_item_p(o_ptr) || cheat_lore) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

		/* Need full knowledge or spoilers */
		if (!spoil && !(o_ptr->ident & IDENT_MENTAL)) return;

		/* Artifact */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			(*f1) = a_ptr->flags1;
			(*f2) = a_ptr->flags2;
			(*f3) = a_ptr->flags3;
			(*f4) = a_ptr->flags4;

			if (mode == OBJECT_FLAGS_RANDOM)
			{
				/* Hack - remove 'ignore' flags */
				(*f2) &= ~(TR2_IGNORE_MASK);
			}
		}

		/* Ego Item */
		if ((o_ptr->name2) && (mode != OBJECT_FLAGS_RANDOM))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			(*f1) = e_ptr->flags1;
			(*f2) = e_ptr->flags2;
			(*f3) = e_ptr->flags3;
			(*f4) = e_ptr->flags4;

		}

		/* Full knowledge for *identified* objects */
		if (!(o_ptr->ident & IDENT_MENTAL)) return;
	}

	/* Coating */
	if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_COATS)
	{
		/* No extra powers */
	}
	/* Rune powers */
	else if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_RUNES)
	{
		int rune = o_ptr->xtra1 - OBJECT_XTRA_MIN_RUNES;
		int i;

		for (i = 0;i<MAX_RUNE_FLAGS;i++)
		{
			if ((y_info[rune].count[i]) && (y_info[rune].count[i]<= o_ptr->xtra2))
			{
				if (y_info[rune].flag[i] < 32) (*f1) |= (1L << y_info[rune].flag[i]);
			
				if ((y_info[rune].flag[i] >= 32)
				 && (y_info[rune].flag[i] < 64)) (*f2) |= (1L << (y_info[rune].flag[i]-32));

				if ((y_info[rune].flag[i] >= 64)
				 && (y_info[rune].flag[i] < 96)) (*f3) |= (1L << (y_info[rune].flag[i]-64));

				if ((y_info[rune].flag[i] >= 96)
				 && (y_info[rune].flag[i] < 128)) (*f4) |= (1L << (y_info[rune].flag[i]-96));
			}
		}
	}
	/* Extra powers */
	else
	{
		if (object_xtra_what[o_ptr->xtra1] == 1)
		{
			(*f1) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);

			/* Guarantee some other flags */
			switch (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2)
			{
				case TR1_STR:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_STR;
					break;
				case TR1_INT:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_INT;
					break;
				case TR1_WIS:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_WIS;
					break;
				case TR1_DEX:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_DEX;
					break;
				case TR1_CON:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_CON;
					break;
				case TR1_CHR:
					if (o_ptr->pval > 0) (*f2) |= TR2_SUST_CHR;
					break;
				case TR1_BRAND_ACID:
					(*f2) |= TR2_IGNORE_ACID;
					break;
				case TR1_BRAND_FIRE:
					(*f2) |= TR2_IGNORE_FIRE;
					break;
				case TR1_BRAND_ELEC:
					(*f2) |= TR2_IGNORE_ELEC;
					break;
				case TR1_BRAND_COLD:
					(*f2) |= TR2_IGNORE_COLD;
					break;
			}
		}

		else if (object_xtra_what[o_ptr->xtra1] == 2)
		{
			(*f2) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);

			/* Guarantee some other flags */
			switch (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2)
			{
				case TR2_IM_ACID:
				case TR2_RES_ACID:
					(*f2) |= TR2_IGNORE_ACID;
					break;
				case TR2_IM_FIRE:
				case TR2_RES_FIRE:
					(*f2) |= TR2_IGNORE_FIRE;
					break;
				case TR2_IM_ELEC:
				case TR2_RES_ELEC:
					(*f2) |= TR2_IGNORE_ELEC;
					break;
				case TR2_IM_COLD:
				case TR2_RES_COLD:
					(*f2) |= TR2_IGNORE_COLD;
					break;
			}
		}
		else if (object_xtra_what[o_ptr->xtra1] == 3)
		{
			(*f3) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);

			/* Guarantee some other flags */
			switch (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2)
			{
				case TR3_PERMA_CURSE:
					(*f3) |= TR3_HEAVY_CURSE;
					/* Fall through */
				case TR3_HUNGER:
				case TR3_UNCONTROLLED:
				case TR3_DRAIN_MANA:
				case TR3_DRAIN_HP:
				case TR3_DRAIN_EXP:
				case TR3_AGGRAVATE:
				case TR3_HEAVY_CURSE:
					(*f3) |= TR3_LIGHT_CURSE;
					break;
			}
		}
		else if (object_xtra_what[o_ptr->xtra1] == 4)
		{
			(*f4) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);

			/* Guarantee some other flags */
			switch (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2)
			{
				case TR4_BRAND_LITE:
					(*f3) |= TR3_LITE;
					break;
				case TR4_RES_WATER:
					(*f2) |= TR2_IGNORE_WATER;
					break;
				case TR4_VAMP_HP:
					(*f3) |= (TR3_DRAIN_HP | TR3_LIGHT_CURSE);
					break;
				case TR4_VAMP_MANA:
					(*f3) |= (TR3_DRAIN_MANA | TR3_LIGHT_CURSE);
					break;
				case TR4_HURT_WATER:
				case TR4_HURT_LITE:
				case TR4_ANCHOR:
				case TR4_SILENT:
				case TR4_STATIC:
				case TR4_WINDY:
				case TR4_HURT_POIS:
				case TR4_HURT_ACID:
				case TR4_HURT_ELEC:
				case TR4_HURT_FIRE:
				case TR4_HURT_COLD:
				case TR4_UNDEAD:
					(*f3) |= TR3_LIGHT_CURSE;
					break;
				case TR4_DEMON:
					(*f4) |= TR4_EVIL;
					break;
			}
		}
	}
}




/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f1, f2, f3, f4);
}


/*
 * Set obvious flags for items 
 */
void object_obvious_flags(object_type *o_ptr, bool floor)
{
        u32b f1, f2, f3, f4;

	/* Spoil the object */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Fully identified */
        if (o_ptr->ident & (IDENT_MENTAL))
        {
                object_can_flags(o_ptr, f1, f2, f3, f4, floor);

                object_not_flags(o_ptr, ~(f1), ~(f2), ~(f3), ~(f4), floor);

		return;
        }

	/* Abilities of base item are always known if aware */
	if (object_aware_p(o_ptr))
	{
        	o_ptr->can_flags1 |= k_info[o_ptr->k_idx].flags1;
                o_ptr->can_flags2 |= k_info[o_ptr->k_idx].flags2;
                o_ptr->can_flags3 |= k_info[o_ptr->k_idx].flags3;
                o_ptr->can_flags4 |= k_info[o_ptr->k_idx].flags4;
	}
	/* Learnt abilities of flavored items are added if not aware */
	else if (k_info[o_ptr->k_idx].flavor)
	{
		object_can_flags(o_ptr,x_list[k_info[o_ptr->k_idx].flavor].can_flags1,
				x_list[k_info[o_ptr->k_idx].flavor].can_flags2,
				x_list[k_info[o_ptr->k_idx].flavor].can_flags3,
				x_list[k_info[o_ptr->k_idx].flavor].can_flags4, floor);

		object_not_flags(o_ptr,x_list[o_ptr->name1].not_flags1,
				x_list[k_info[o_ptr->k_idx].flavor].not_flags2,
				x_list[k_info[o_ptr->k_idx].flavor].not_flags3,
				x_list[k_info[o_ptr->k_idx].flavor].not_flags4, floor);	
	}

	/* Identified name */
	if (object_named_p(o_ptr))
	{
		/* Now we know what it is, update what we know about it from our artifact memory */
		if (o_ptr->name1)
		{
			object_can_flags(o_ptr,a_list[o_ptr->name1].can_flags1,
					a_list[o_ptr->name1].can_flags2,
					a_list[o_ptr->name1].can_flags3,
					a_list[o_ptr->name1].can_flags4, floor);

			object_not_flags(o_ptr,a_list[o_ptr->name1].not_flags1,
					a_list[o_ptr->name1].not_flags2,
					a_list[o_ptr->name1].not_flags3,
					a_list[o_ptr->name1].not_flags4, floor);
		}
		/* Now we know what it is, update what we know about it from our ego item memory */
		else if (o_ptr->name2)
		{
			/* Obvious flags */
			object_can_flags(o_ptr,e_info[o_ptr->name2].obv_flags1,
					 e_info[o_ptr->name2].obv_flags2,
					 e_info[o_ptr->name2].obv_flags3,
					 e_info[o_ptr->name2].obv_flags4, floor);

			/* Known flags */
			object_can_flags(o_ptr,e_list[o_ptr->name2].can_flags1,
					 e_list[o_ptr->name2].can_flags2,
					 e_list[o_ptr->name2].can_flags3,
					 e_list[o_ptr->name2].can_flags4, floor);
			
			object_not_flags(o_ptr,e_list[o_ptr->name2].not_flags1,
					 e_list[o_ptr->name2].not_flags2,
					 e_list[o_ptr->name2].not_flags3,
					 e_list[o_ptr->name2].not_flags4, floor);
		}
		/* Hack -- Magic items have an 'obvious' ability for which they are named */
		else if ((o_ptr->xtra1) && (o_ptr->xtra1 < OBJECT_XTRA_MIN_RUNES) && (o_ptr->feeling < INSCRIP_MIN_HIDDEN))
		{
			if (object_xtra_what[o_ptr->xtra1] == 1)
				(o_ptr->can_flags1) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			else if (object_xtra_what[o_ptr->xtra1] == 2)
				(o_ptr->can_flags2) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			else if (object_xtra_what[o_ptr->xtra1] == 3)
				(o_ptr->can_flags3) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			else if (object_xtra_what[o_ptr->xtra1] == 4)
				(o_ptr->can_flags4) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
		}
               	/* Non-ego, non-magical, non-runed average item have no more hidden abilities */
		else if (object_aware_p(o_ptr))
		{
			object_not_flags(o_ptr, ~(o_ptr->can_flags1), 
				~(o_ptr->can_flags2),
				~(o_ptr->can_flags3),
				~(o_ptr->can_flags4), floor);
		}
	}

	/* Throwing is always obvious */
	if (f3 & TR3_THROWING)
	{
		object_can_flags(o_ptr,0x0L,0x0L,TR3_THROWING,0x0L, floor);
	}
	else object_not_flags(o_ptr,0x0L,0x0L,TR3_THROWING,0x0L, floor);
}


/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4)
{
	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f1, f2, f3, f4);
}


/*
 * Describe an item's random attributes for "character dumps"
 */
void identify_random_gen(const object_type *o_ptr)
{
	/* Set the indent/wrap */
	text_out_indent = 3;
	text_out_wrap = 75;

	list_object(o_ptr, OBJECT_FLAGS_RANDOM);

	/* Reset indent/wrap */
	text_out_indent = 0;
	text_out_wrap = 0;
}

/* Hack -- must not collide with existing ident flags */
#define SF1_IDENT_PACK	0x00000001L

/*
 * Hack -- Get spell description for effects on you.
 */
static bool spell_desc_flags(const spell_type *s_ptr, const cptr intro, int level, bool detail, int target, bool introduced)
{
	int vn;

	int n,r;
	cptr vp[64];

	u32b id_flags = s_ptr->flags1;

	(void)level;

	/* Only apply effects to player */
	if ((target != SPELL_TARGET_NORMAL) && (target != SPELL_TARGET_SELF)) return (FALSE);
	
	/* Collect detects */
	vn = 0;
	if (s_ptr->flags1 & (SF1_DETECT_DOORS))	vp[vn++] = "doors";
	if (s_ptr->flags1 & (SF1_DETECT_TRAPS))	vp[vn++] = "traps";
	if (s_ptr->flags1 & (SF1_DETECT_STAIRS))	vp[vn++] = "stairs";
	if (s_ptr->flags1 & (SF1_DETECT_WATER))	vp[vn++] = "running water";
	if (s_ptr->flags1 & (SF1_DETECT_GOLD))	vp[vn++] = "gold, including hidden treasures";
	if (s_ptr->flags1 & (SF1_DETECT_OBJECT))	vp[vn++] = "objects, including hidden objects";
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC))	vp[vn++] = "magic objects, and senses their power";
	if (s_ptr->flags1 & (SF1_DETECT_POWER))	vp[vn++] = "powerful objects, and senses their power";
	if (s_ptr->flags1 & (SF1_DETECT_CURSE))	vp[vn++] = "cursed objects, and senses their power";
	if (s_ptr->flags1 & (SF1_DETECT_MONSTER))	vp[vn++] = "visible monsters";
	if (s_ptr->flags1 & (SF1_DETECT_EVIL))	vp[vn++] = "evil monsters";
	if (s_ptr->flags1 & (SF1_DETECT_LIFE))	vp[vn++] = "living monsters";
	if (s_ptr->type == SPELL_DETECT_MIND)	vp[vn++] = "minds";	

	/* Describe detection spells */
	if (vn)
	{
		/* Intro */
		text_out(intro);

		introduced = TRUE;

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("detects all ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(format(" within %d grids", 2 * MAX_SIGHT));
	}

	/* Some identifies assume earlier IDs */
	if (s_ptr->flags1 & (SF1_IDENT_FULLY)) id_flags |= SF1_IDENT;
	if (s_ptr->type == SPELL_IDENT_PACK) id_flags |= (SF1_IDENT | SF1_IDENT_PACK);
	if (s_ptr->flags1 & (SF1_DETECT_CURSE)) id_flags |= SF1_IDENT_PACK;
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC)) id_flags |= SF1_IDENT_PACK;
	if (s_ptr->flags1 & (SF1_DETECT_POWER)) id_flags |= SF1_IDENT_PACK;
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC)) id_flags |= SF1_DETECT_CURSE;
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC)) id_flags |= SF1_IDENT_SENSE;
	if (s_ptr->flags1 & (SF1_DETECT_POWER)) id_flags |= SF1_IDENT_SENSE;
	if (s_ptr->flags1 & (SF1_FORGET)) id_flags |= SF1_IDENT_PACK;

	/* Collect identifies */
	vn = 0;
	if (id_flags & (SF1_DETECT_CURSE)) vp[vn++]="curses";
	if (id_flags & (SF1_IDENT_SENSE)) vp[vn++]="the general power level";
	if (id_flags & (SF1_IDENT_MAGIC)) vp[vn++]="a magical attribute";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="the bonuses to hit, damage and armour class";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="the number of charges";
	if (id_flags & (SF1_IDENT_VALUE)) vp[vn++]="the value";
	if (id_flags & (SF1_IDENT_RUNES)) vp[vn++]="the types of runes";
	if ((id_flags & (SF1_IDENT)) || (s_ptr->type == SPELL_IDENT_TVAL) || (s_ptr->type == SPELL_IDENT_NAME)) vp[vn++]="the kind, ego-item and artifact names";
	if ((id_flags & (SF1_IDENT)) || (s_ptr->type == SPELL_IDENT_TVAL)) vp[vn++]="all bonuses";
	if (id_flags & (SF1_IDENT_RUMOR)) vp[vn++]="some hidden powers";
	if (id_flags & (SF1_IDENT_FULLY)) vp[vn++]="all hidden powers";
	if (id_flags & (SF1_FORGET)) vp[vn++]="all information";

	/* Describe enchantment spells */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);
	
			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Intro */
		if (id_flags & (SF1_IDENT_PACK)) text_out("will ");
		else text_out ("can ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if ((n == 0) && (id_flags & (SF1_IDENT_SENSE))) text_out("sense ");
			else if ((n == 0) && (id_flags & (SF1_FORGET))) text_out("forget ");
			else if (n == 0) text_out("identify ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* Intro */
		if (id_flags & (SF1_IDENT_PACK)) text_out(" on all ");
		else text_out(" on one ");

	}

	/* Collect identifies */
	vn = 0;

	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="weapon";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="wearable item";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="wand";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="staff";
	if (id_flags & (SF1_IDENT | SF1_IDENT_SENSE | SF1_IDENT_MAGIC)) vp[vn++]="unknown item";
	if (id_flags & (SF1_IDENT_RUMOR | SF1_IDENT_FULLY | SF1_FORGET | SF1_IDENT_MAGIC)) vp[vn++]="known item";
	if (id_flags & (SF1_DETECT_CURSE)) vp[vn++]="cursed item";
	if (id_flags & (SF1_DETECT_MAGIC)) vp[vn++]="magic item";
	if (id_flags & (SF1_IDENT_VALUE | SF1_IDENT_RUNES)) vp[vn++]="item";

	if (s_ptr->type == SPELL_IDENT_TVAL)
	{
		int i=0;

		while ((object_group_tval[i]) && (object_group_tval[i] != s_ptr->param)) i++;

		if (object_group_tval[i] == s_ptr->param) vp[vn++]=object_group_text[i];

	}

	/* Describe identify spells */
	if (vn)
	{

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) { }
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);

			/* Intro */
			if (id_flags & (SF1_IDENT_PACK)) text_out("s");

		}

		/* Intro */
		if (id_flags & (SF1_IDENT_PACK)) text_out(" in your pack");

		if (vn > 1) text_out(" as appropriate");

		if (id_flags & (SF1_IDENT_FULLY)) text_out(" and prevents you from magically forgetting this information");
		if (id_flags & (SF1_FORGET)) text_out(" except when the item is fully identified");

	}


	/* Collect enchantments */
	vn = 0;
	if ((s_ptr->flags1 & (SF1_ENCHANT_TOH | SF1_ENCHANT_TOD))
		|| (s_ptr->type == SPELL_BRAND_WEAPON)) vp[vn++]="weapon";
	if ((s_ptr->flags1 & (SF1_ENCHANT_TOH | SF1_ENCHANT_TOD))
		|| (s_ptr->type == SPELL_BRAND_AMMO)) vp[vn++]="missile";
	if ((s_ptr->flags1 & (SF1_ENCHANT_TOA))
		|| (s_ptr->type == SPELL_BRAND_ARMOR)) vp[vn++]="piece of armor";
	if (s_ptr->type == SPELL_BRAND_ITEM) vp[vn++]="item";

	if (s_ptr->type == SPELL_ENCHANT_TVAL)
	{
		int i=0;

		while ((object_group_tval[i]) && (object_group_tval[i] != s_ptr->param)) i++;

		if (object_group_tval[i] == s_ptr->param) vp[vn++]=object_group_text[i];
			
	}

	/* Describe enchantments spells */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);
			text_out("can enchant one ");

			introduced = TRUE;

		}
		else
		{
			text_out(", and can enchant one ");
		}
		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) { }
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);

		}

	}

	/* Collect enchantments */
	vn = 0;
	if (s_ptr->flags1 & (SF1_ENCHANT_TOH)) vp[vn++]="improve accuracy to hit";
	if (s_ptr->flags1 & (SF1_ENCHANT_TOD)) vp[vn++]="increase damage";
	if (s_ptr->flags1 & (SF1_ENCHANT_TOA)) vp[vn++]="increase armor class";

	if ((s_ptr->type == SPELL_BRAND_WEAPON) ||
	    (s_ptr->type == SPELL_BRAND_ARMOR) ||
	    (s_ptr->type == SPELL_BRAND_AMMO) ||
	    (s_ptr->type == SPELL_BRAND_ITEM))
	{
		vp[vn++]=format("become %s",inscrip_text[INSCRIP_MIN_HIDDEN-INSCRIP_NULL+s_ptr->param-1]);
	}

	if (s_ptr->type == SPELL_ENCHANT_TVAL) vp[vn++]="change its kind";

	/* Describe enchantment spells */
	if (vn)
	{

		if (s_ptr->flags1 & (SF1_ENCHANT_HIGH)) vp[vn++]="highly ";

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" to ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

	}

	/* Hack */
	r = 0;



	/* Collect timed effects */
	vn = 0;
	if (s_ptr->flags2 & (SF2_OPP_FIRE)) vp[vn++]="fire";
	if (s_ptr->flags2 & (SF2_OPP_COLD)) vp[vn++]="cold";
	if (s_ptr->flags2 & (SF2_OPP_ACID)) vp[vn++]="acid";
	if (s_ptr->flags2 & (SF2_OPP_ELEC)) vp[vn++]="lightning";
	if (s_ptr->flags2 & (SF2_OPP_POIS)) vp[vn++]="poison";

	/* Describe timed effects */
	if (vn)
	{
		/* Hack -- continue sentence */
		r = 1;

		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("provides temporary ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		text_out(" resistance");
	}

	/* Collect timed effects */
	vn = 0;

	if (s_ptr->flags2 & (SF2_INFRA)) vp[vn++]="extends your infravision by 50 feet";
	if (s_ptr->flags2 & (SF2_HERO)) vp[vn++]="makes you heroic";
	if (s_ptr->flags2 & (SF2_SHERO)) vp[vn++]="makes you go berserk";
	if (s_ptr->flags2 & (SF2_BLESS)) vp[vn++]="blesses you";
	if (s_ptr->flags2 & (SF2_SHIELD)) vp[vn++]="shields you";
	if (s_ptr->flags2 & (SF2_INVULN)) vp[vn++]="makes you invulnerable to damage";
	if (s_ptr->flags3 & (SF3_FREE_ACT)) vp[vn++] = "protects you from paralysis and magical slowness";
	if (s_ptr->flags2 & (SF2_SEE_INVIS)) vp[vn++]="allows you to see invisible monsters";
	if (s_ptr->flags2 & (SF2_PROT_EVIL)) vp[vn++]="protects your from evil monsters";
	if (s_ptr->flags2 & (SF2_HASTE)) vp[vn++]="hastes you";
	if (s_ptr->flags2 & (SF2_SLOW)) vp[vn++]="slows you";
	if (s_ptr->flags2 & (SF2_CUT)) vp[vn++]="makes you bleed";
	if (s_ptr->flags2 & (SF2_STUN)) vp[vn++]="stuns you";
	if (s_ptr->flags2 & (SF2_POISON)) vp[vn++]="poisons you";
	if (s_ptr->flags2 & (SF2_BLIND)) vp[vn++]="blinds you";
	if (s_ptr->flags2 & (SF2_FEAR)) vp[vn++]="makes you afraid";
	if (s_ptr->flags2 & (SF2_CONFUSE)) vp[vn++]="confuses you";
	if (s_ptr->flags2 & (SF2_HALLUC)) vp[vn++]="makes you hallucinate";
	if (s_ptr->flags2 & (SF2_PARALYZE)) vp[vn++]="paralyzes you";
	if (s_ptr->type ==SPELL_INVEN_WIELD) vp[vn++]="creates a magical weapon";
	if (s_ptr->type ==SPELL_INVEN_BOW) vp[vn++]="creates a magical bow";
	if (s_ptr->type ==SPELL_INVEN_LEFT) vp[vn++]="creates a magical ring";
	if (s_ptr->type ==SPELL_INVEN_RIGHT) vp[vn++]="creates a magical ring";
	if (s_ptr->type ==SPELL_INVEN_NECK) vp[vn++]="creates a magical amulet";
	if (s_ptr->type ==SPELL_INVEN_LITE) vp[vn++]="creates a magical light";
	if (s_ptr->type ==SPELL_INVEN_BODY) vp[vn++]="creates magical armor";
	if (s_ptr->type ==SPELL_INVEN_OUTER) vp[vn++]="creates a magical cloak";
	if (s_ptr->type ==SPELL_INVEN_ARM) vp[vn++]="creates a magical shield";
	if (s_ptr->type ==SPELL_INVEN_HEAD) vp[vn++]="creates magical headgear";
	if (s_ptr->type ==SPELL_INVEN_HANDS) vp[vn++]="creates magical gloves";
	if (s_ptr->type ==SPELL_INVEN_FEET) vp[vn++]="creates magical boots";

	/* Describe timed effects */
	if (vn)
	{

		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else if (r)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) {}
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Collect stat gain effects */
	vn = 0;

	if (s_ptr->flags3 & (SF3_INC_STR)) vp[vn++]="strength and size";
	if (s_ptr->flags3 & (SF3_INC_INT)) vp[vn++]="intelligence";
	if (s_ptr->flags3 & (SF3_INC_WIS)) vp[vn++]="wisdom";
	if (s_ptr->flags3 & (SF3_INC_DEX)) vp[vn++]="dexterity and agility";
	if (s_ptr->flags3 & (SF3_INC_CON)) vp[vn++]="constitution";
	if (s_ptr->flags3 & (SF3_INC_CHR)) vp[vn++]="charisma";
	if (s_ptr->flags3 & (SF3_INC_EXP)) vp[vn++]="experience";

	/* Describe stat effects */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("increases your ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Roll out the duration */
	if (!detail)
	{
		/* Nothing */
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side) && (s_ptr->l_plus))
	{
		/* End */
		text_out(format(" for %dd%d+%d turns",s_ptr->l_dice,s_ptr->l_side,s_ptr->l_plus));
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side) && (s_ptr->l_side == 1))
	{
		/* End */
		text_out(format(" for %d turn%s",s_ptr->l_dice, s_ptr->l_dice != 1 ? "s" : ""));
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side))
	{
		/* End */
		text_out(format(" for %dd%d turns",s_ptr->l_dice,s_ptr->l_side));
	}
	else if (s_ptr->l_plus)
	{
		/* End */
		text_out(format(" for %d turn%s",s_ptr->l_plus, s_ptr->l_plus != 1 ? "s" : ""));
	}

	/* Collect cure effects */
	vn = 0;
	if (s_ptr->flags3 & (SF3_CURE_CUTS)) vp[vn++]="cuts";
	if (s_ptr->flags3 & (SF3_CURE_STUN)) vp[vn++]="stun damage";
	if (s_ptr->flags3 & (SF3_CURE_POIS)) vp[vn++]="poison";
	if (s_ptr->flags3 & (SF3_CURE_FOOD)) vp[vn++]="hunger";
	if (s_ptr->flags3 & (SF3_CURE_BLIND)) vp[vn++]="blindness";
	if (s_ptr->flags3 & (SF3_CURE_IMAGE)) vp[vn++]="hallucinations";
	if (s_ptr->flags3 & (SF3_CURE_CONF)) vp[vn++]="confusion";
	if (s_ptr->flags3 & (SF3_CURE_FEAR)) vp[vn++]="fear";
	if (s_ptr->flags3 & (SF3_CURE_FEAR)) vp[vn++]="petrification";
	if (s_ptr->flags3 & (SF3_FREE_ACT)) vp[vn++]="slowness";
	if (s_ptr->flags3 & (SF3_CURE_MEM)) vp[vn++]="amnesia";
	if (s_ptr->flags3 & (SF3_SLOW_CURSE | SF3_CURE_CURSE)) vp[vn++]="curses";
	if (s_ptr->type == SPELL_CURE_DISEASE) vp[vn++] = disease_name[s_ptr->param];

	/* Hack -- cure disease also cures minor diseases */
	if ((s_ptr->type == SPELL_CURE_DISEASE) && (s_ptr->param == 32)) vp[vn++] = disease_name[29];

	/* Describe cure effects */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("cures you of ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Collect slow effects */
	vn = 0;
	if (s_ptr->type == SPELL_SLOW_POIS) vp[vn++]="poison";
	if (s_ptr->type == SPELL_SLOW_CONF) vp[vn++]="confusion";
	if (s_ptr->flags3 & (SF3_SLOW_CUTS)) vp[vn++]="cuts";
	if (s_ptr->flags3 & (SF3_SLOW_STUN)) vp[vn++]="stun damage";

	/* Describe cure effects */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("partially cures you of ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Collect restore effects */
	vn = 0;
	if (s_ptr->flags3 & (SF3_CURE_STR)) vp[vn++]="strength and size";
	if (s_ptr->flags3 & (SF3_CURE_INT)) vp[vn++]="intelligence";
	if (s_ptr->flags3 & (SF3_CURE_WIS)) vp[vn++]="wisdom";
	if (s_ptr->flags3 & (SF3_CURE_DEX)) vp[vn++]="dexterity and agility";
	if (s_ptr->flags3 & (SF3_CURE_CON)) vp[vn++]="constitution";
	if (s_ptr->flags3 & (SF3_CURE_CHR)) vp[vn++]="charisma";
	if (s_ptr->flags3 & (SF3_CURE_EXP)) vp[vn++]="experience";

	/* Describe stat effects */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("restores your ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	/* Collect miscellaneous */
	vn = 0;

	if (s_ptr->flags3 & (SF3_DEC_FOOD)) vp[vn++] = "makes you weak from hunger";
	if (s_ptr->flags2 & (SF2_CURSE_WEAPON)) vp[vn++] = "curses your weapon";
	if (s_ptr->flags2 & (SF2_CURSE_ARMOR)) vp[vn++] = "curses your armor";
	if (s_ptr->flags2 & (SF2_AGGRAVATE)) vp[vn++] = "wakes up nearby monsters and hastes those in line of sight";
	if (s_ptr->type == SPELL_SUMMON)
	{
		switch(s_ptr->param)
		{
			case SUMMON_KIN: vp[vn++] = "summons kindred monsters"; break;
			case SUMMON_PLANT: vp[vn++] = "summons plants"; break;
			case SUMMON_INSECT: vp[vn++] = "summons insects"; break;
			case SUMMON_ANIMAL: vp[vn++] = "summons animals"; break;
			case SUMMON_HOUND: vp[vn++] = "summons hounds"; break;
			case SUMMON_SPIDER: vp[vn++] = "summons spiders"; break;
			case SUMMON_CLASS: vp[vn++] = "summons related classes"; break;
			case SUMMON_RACE: vp[vn++] = "summons related races"; break;
			case SUMMON_GROUP: vp[vn++] = "summons related monsters"; break;
			case SUMMON_FRIEND: vp[vn++] = "summons related monsters"; break;
			case SUMMON_UNIQUE_FRIEND : vp[vn++] = "summons related uniques"; break;
			case SUMMON_ORC: vp[vn++] = "summons orcs"; break;
			case SUMMON_TROLL: vp[vn++] = "summons trolls"; break;
			case SUMMON_GIANT: vp[vn++] = "summons giants"; break;
			case SUMMON_DRAGON: vp[vn++] = "summons dragons"; break;
			case SUMMON_HI_DRAGON: vp[vn++] = "summons high dragons"; break;
			case SUMMON_DEMON: vp[vn++] = "summons demons"; break;
			case SUMMON_HI_DEMON: vp[vn++] = "summons high demons"; break;
			case SUMMON_UNIQUE: vp[vn++] = "summons uniques"; break;
			case SUMMON_HI_UNIQUE: vp[vn++] = "summons high uniques"; break;
			case SUMMON_UNDEAD: vp[vn++] = "summons undead"; break;
			case SUMMON_HI_UNDEAD: vp[vn++] = "summons high undead"; break;
			case SUMMON_WRAITH: vp[vn++] = "summons wraiths"; break;
			default: vp[vn++] = "summons monsters"; break;
		}
	}
	if (s_ptr->type == SPELL_SUMMON_RACE) vp[vn++] = format("summons %s%s",
		is_a_vowel((r_name+r_info[s_ptr->param].name)[0])?"an ":"a ",
		r_name+r_info[s_ptr->param].name);
	if (s_ptr->type == SPELL_SUMMON_GROUP_IDX) vp[vn++] = "summons related monsters";
	if (s_ptr->type == SPELL_CREATE_KIND) vp[vn++] = "creates gold";
	if (s_ptr->flags2 & (SF2_CREATE_STAIR)) vp[vn++] = "creates a staircase under you";
	if (s_ptr->type == SPELL_WARD_GLYPH) vp[vn++] = "creates a glyph of warding under you";
	if (s_ptr->type == SPELL_WARD_TRAP) vp[vn++] = format("creates %s%s next to you",
		is_a_vowel((f_name+f_info[s_ptr->param].name)[0])?"an ":"a ",
		f_name+f_info[s_ptr->param].name);
	if (s_ptr->flags1 & (SF1_STAR_ACQUIREMENT)) vp[vn++] = "creates several excellent, superb or special items";
	else if (s_ptr->flags1 & (SF1_ACQUIREMENT)) vp[vn++] = "creates an excellent, superb or special item";
	if (s_ptr->flags2 & (SF2_TELE_LEVEL)) vp[vn++] = "pushes you through floor or ceiling";
	if (s_ptr->flags2 & (SF2_RECALL)) vp[vn++]="returns you to the surface, or teleports you into the depths";
	if (s_ptr->flags2 & (SF2_ALTER_LEVEL)) vp[vn++] = "alters the level you are on";
	if (s_ptr->type == SPELL_EARTHQUAKE) vp[vn++] = format("creates a radius %d earthquake",s_ptr->param);
	if (s_ptr->type == SPELL_DESTRUCTION) vp[vn++] = format("destroys a radius %d area",s_ptr->param);
	if (s_ptr->flags2 & (SF2_BANISHMENT)) vp[vn++] = "allows you to remove a monster type from a level (1d4 damage per monster)";
	if (s_ptr->flags2 & (SF2_MASS_BANISHMENT)) vp[vn++] = "removes all nearby monsters";
	if (s_ptr->flags3 & (SF3_SLOW_CURSE)) vp[vn++] = "removes a normal curse from an item";
	if (s_ptr->flags3 & (SF3_CURE_CURSE)) vp[vn++] = "removes all normal and some heavy curses from all items you are wearing or wielding";
	if (s_ptr->type == SPELL_RECHARGE) vp[vn++] = format("recharges one staff or wand for %d power", s_ptr->param);	
	if (s_ptr->flags1 & (SF1_MAP_AREA)) vp[vn++] = "maps your surroundings";
	if (s_ptr->flags1 & (SF1_WIZ_LITE)) vp[vn++] = "lights up and maps the entire level";
	if (s_ptr->flags1 & (SF1_LITE_ROOM)) vp[vn++] = "lights up the room you are in";
	if (s_ptr->flags1 & (SF1_DARK_ROOM)) vp[vn++] = "plunges the room you are in into darkness";
	if (s_ptr->flags1 & (SF1_FORGET)) vp[vn++] = "erases the knowledge of the entire level from your mind";
	if (s_ptr->flags1 & (SF1_SELF_KNOW)) vp[vn++] = "reveals all knowledge of yourself";
	if (s_ptr->type == SPELL_CONCENTRATE_LITE) vp[vn++] = "concentrates light around you";
	if (s_ptr->type == SPELL_CONCENTRATE_LIFE) vp[vn++] = "concentrates life around you";
	if (s_ptr->type == SPELL_CONCENTRATE_WATER) vp[vn++] = "concentrates water around you";
	if (s_ptr->type == SPELL_RELEASE_CURSE) vp[vn++] = "releases a curse from an item";
	if (s_ptr->type == SPELL_SET_RETURN) vp[vn++] = "marks this grid as a destination for later return";
	if (s_ptr->type == SPELL_SET_OR_MAKE_RETURN) vp[vn++] = "marks this grid as a destination for later return or returns you to a marked grid";
	if (s_ptr->type == SPELL_BLOOD_BOND) vp[vn++] = "bonds you with a living creature to share damage and healing";
	if (s_ptr->type == SPELL_MINDS_EYE) vp[vn++] = "bonds you with a mind to allow you to see through its eyes";
	if (s_ptr->type == SPELL_LIGHT_CHAMBERS) vp[vn++] = "lights all rooms on the level, except vaults";
	if (s_ptr->type == SPELL_CHANGE_SHAPE) vp[vn++] = format("changes you into a %s",p_name + p_info[s_ptr->param].name);
	if (s_ptr->type == SPELL_REVERT_SHAPE) vp[vn++] = "returns you to your normal form";
	if (s_ptr->type == SPELL_REFUEL) vp[vn++] = "fuels a torch";
	if (s_ptr->type == SPELL_MAGIC_BLOW) vp[vn++] = "increases the effectiveness of a single round of blows";
	if (s_ptr->type == SPELL_MAGIC_SHOT) vp[vn++] = "increases the effectiveness of a single round of firing";
	if (s_ptr->type == SPELL_MAGIC_HURL) vp[vn++] = "increases the effectiveness of a single round of thrown weapons";
	if (s_ptr->type == SPELL_ACCURATE_BLOW) vp[vn++] = "increases the accuracy of a single round of blows";
	if (s_ptr->type == SPELL_ACCURATE_SHOT) vp[vn++] = "increases the accuracy of a single round of firing";
	if (s_ptr->type == SPELL_ACCURATE_HURL) vp[vn++] = "increases the accuracy of a single round of thrown weapons";
	if (s_ptr->type == SPELL_DAMAGING_BLOW) vp[vn++] = "increases the damage of a single round of blows";
	if (s_ptr->type == SPELL_DAMAGING_SHOT) vp[vn++] = "increases the damage of a single round of firing";
	if (s_ptr->type == SPELL_DAMAGING_HURL) vp[vn++] = "increases the damage of a single round of thrown weapons";
	if (s_ptr->type == SPELL_REFUEL) vp[vn++] = "fuels a torch";
	

	/* Describe miscellaneous effects */
	if (vn)
	{
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;

		}
		else
		{
			text_out(", and ");
		}

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) { }
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}
	
	/* Note for the player */
	if ((s_ptr->type >= SPELL_MAGIC_BLOW) && (s_ptr->type <= SPELL_DAMAGING_HURL))
	{
		text_out(".  ");
	}

	/* Hack -- describe the blow/shot/hurl effect */
	if ((s_ptr->type >= SPELL_MAGIC_BLOW) && (s_ptr->type <= SPELL_MAGIC_HURL))
	{
		u32b f1 = 0L;
		u32b f2 = 0L;
		u32b f3 = 0L;
		u32b f4 = 0L;
		
		int param = s_ptr->param;
		int pval = (p_ptr->lev + 19) / 20;
		
		/* Hack - upgrade slays */
		switch (param)
		{
			case 19:
			{
				if (p_ptr->lev >= 30) param = 28;
				break;
			}
			case 20:
			{
				if (p_ptr->lev >= 30) param = 27;
				break;
			}
			case 25:
			{
				if (p_ptr->lev >= 30) param = 26;
				break;
			}
		}
		
		if (param < 33) f1 |= 1L << (param - 1);
		else if (param < 65) f2 |= 1L << (param - 33);
		else if (param < 97) f3 |= 1L << (param - 65);
		else if (param < 129) f4 |= 1L << (param - 97);
		
		/* Hack - might for blows modifies charging */
		if ((f1 == TR1_MIGHT) && (s_ptr->type == SPELL_MAGIC_BLOW))
		{
			/* Message */
			text_out(format("It increases your charging by x%d.  ", pval));
		}
		/* Hack - shots for throws modifies number of throws */
		else if ((f1 == TR1_SHOTS) && (s_ptr->type == SPELL_MAGIC_HURL))
		{
			/* Message */
			text_out(format("It increases your hurls by %d.  ", pval));
		}
		else
		{
			/* List the flags */
			list_object_flags(f1, f2, f3, f4, pval, LIST_FLAGS_CAN);
		}
	}
	
	/* Note for the player */
	if ((s_ptr->type >= SPELL_MAGIC_BLOW) && (s_ptr->type <= SPELL_DAMAGING_HURL))
	{
		text_out("This initiates the attack if successful");
	}

	return (introduced);
}



/*
 * Hack -- Get spell description for effects on target based on blow.
 */
static bool spell_desc_blows(const spell_type *s_ptr, const cptr intro, int level, bool detail, int target, bool introduced)
{
	int m,n,r;

	cptr p, q, s, t, u, v;
	
	/* Preposition */
	v = "for";
	
	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!s_ptr->blow[m].method) continue;

		/* Count known attacks */
		n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < 4; m++)
	{
		int method, effect, d1, d2, d3, rad, arc, rng;

		/* Skip non-attacks */
		if (!s_ptr->blow[m].method) continue;

		/* Extract the attack info */
		method = s_ptr->blow[m].method;
		effect = s_ptr->blow[m].effect;
		d1 = s_ptr->blow[m].d_dice;
		d2 = s_ptr->blow[m].d_side;
		d3 = s_ptr->blow[m].d_plus;
		rad = 0;
		arc = 0;
		rng = 0;

		/* Hack -- use level as modifier */
		if ((!d2) && (!level))
		{
			d3 += 25 * d1;
		}
		/* Hack -- use level as modifier */
		else if (!d2)
		{
			d3 += level * d1;
		}

		/* No method yet */
		p = NULL;

		/* No method yet */
		s = NULL;

		/* No target yet */
		t = NULL;

		/* Hack -- potions and food */
		switch(target)
		{
			case SPELL_TARGET_SELF:
				if ((method!= RBM_SPIT) && (method != RBM_BREATH) && (method!= RBM_VOMIT)) method = RBM_SELF;
				break;

			case SPELL_TARGET_AIMED:
				method = RBM_AIM;
				break;

			case SPELL_TARGET_COATED:
				method = RBM_HIT;
				break;

			case SPELL_TARGET_EXPLODE:
				method = RBM_FLASK;
				break;

		}

		/* Get the method */
		switch (method)
		{
			case RBM_HIT: p = "hits"; t = "one target"; break;
			case RBM_SPIT: p = "spits"; t = "one target"; break;
			case RBM_AURA: p = "surrounds you with an aura";  t = "your enemies"; rad = 2; break;
			case RBM_AURA_MINOR: p = "surrounds you with an aura";  t = "your enemies"; rad = 1; break;
			case RBM_SELF: t = "you";break;
			case RBM_EXPLODE: t = "you and all enemies adjacent to you"; break;
			case RBM_ADJACENT: t = "all enemies adjacent to you"; break;
			case RBM_HANDS: t = "a beam"; rng = 3; if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_MISSILE: p = "creates a missile"; t = "your enemies"; if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_BOLT_10: p = "creates a bolt"; t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BOLT: p = "creates a powerful bolt";  t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BEAM: p = "creates a beam"; t = "your enemies";if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BLAST: p = "creates an adjacent blast"; t = "your enemies"; d3 += level;break;
			case RBM_WALL: p = "creates a wall"; t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BALL: p = "creates a ball"; t = "your enemies"; rad = 2; break;
			case RBM_BALL_II: p = "creates a ball"; t = "your enemies"; rad = 3; break;
			case RBM_BALL_III: p = "creates a ball"; t = "your enemies"; rad = 4; break;
			case RBM_CLOUD: p = "creates a cloud"; t = "your enemies"; rad = 2; d3 += level/2; break;
			case RBM_STORM: p = "creates a storm"; t = "your enemies"; rad = 3; break;
			case RBM_BREATH: p = "breathes";  t = "your enemies"; d3 = p_ptr->chp * d3 / 300; break;
			case RBM_AREA: p = "surrounds you with magic"; rad = (level/10)+1; break;
			case RBM_AIM_AREA: p = "affects an area"; rad = (level/10)+1; break;
			case RBM_LOS: t = "all your enemies in line of sight"; break;
			case RBM_LINE: t = "one direction"; break;
			case RBM_AIM: t = "one target"; break;
			case RBM_PANEL: t = "the current panel"; break;
			case RBM_LEVEL: t = "the current level"; break;
			case RBM_ORB: p = "creates an orb"; t = "your enemies"; rad = (level < 30 ? 2 : 3); d3 += level/2; break;
			case RBM_CROSS: p = "surrounds you with a cross"; t = "your enemies"; break;
			case RBM_STRIKE: p = "strikes"; t = "your enemy"; if ((level > 5) && (d2)) d1+= (level-1)/5; break;
			case RBM_STAR: p = "surrounds you with a star"; t = "your enemies"; break;
			case RBM_SPHERE: p = "creates a sphere";  t = "your enemies";  rad = (level/10)+2;break;
			case RBM_ARROW: p = "creates an arrow"; t="one target"; break;
			case RBM_XBOLT: p = "creates a crossbow bolt"; t="one target"; break;
			case RBM_SPIKE: p = "creates a spike"; t="one target"; break;
			case RBM_DART: p = "creates a dart"; t="one target"; break;
			case RBM_SHOT: p = "creates a shot"; t="one target"; break;
			case RBM_ARC_20: p = "creates an arc"; arc = 20; break;
			case RBM_ARC_30: p = "creates an arc"; arc = 30; break;
			case RBM_ARC_40: p = "creates an arc"; arc = 40; break;
			case RBM_ARC_50: p = "creates an arc"; arc = 50; break;
			case RBM_ARC_60: p = "creates an arc"; arc = 60; break;
			case RBM_FLASK: p = "creates a ball"; t = "your enemies"; rad = 1; break;
			case RBM_BOLT_MINOR: p = "creates a bolt"; t = "your enemies"; rng = 4; if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_BALL_MINOR: p = "throws a ball"; t = "your enemies"; rad = 1; break;
			case RBM_8WAY: p = "creates a beam in 8 directions"; t = "your enemies"; rad = 2; break;
			case RBM_8WAY_II: p = "creates a beam in 8 directions"; t = "your enemies"; rad = 3; break;
			case RBM_8WAY_III: p = "creates a beam in 8 directions"; t = "your enemies"; rad = 4; break;
			case RBM_SWARM: p = "creates multiple balls"; t = "your enemies"; rad = 1; d3 += level / 2; break;
			case RBM_SCATTER: p = "scatters magic around you"; t = "your enemies"; break;
			default: t = "one adjacent target"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
		}


		/* Default technique */
		q = NULL;

		/* Default flavor */
		u = NULL;

		/* Get the effect */
		switch (effect)
		{
			case GF_NOTHING: q = "do"; u = "nothing"; break;
			case GF_STORM: q= "lash"; u = "with wind, rain and lightning"; break;
			case GF_WIND: q= "blast"; u = "with wind"; break;
			case GF_HELLFIRE: q="blast"; u = "with hellfire";break;
			case GF_MANA: q="blast"; u = "with magic";break;       
			case GF_HOLY_ORB: q="blast"; u = "with holy magic";break;
			case GF_LITE_WEAK: q="light"; s ="up";break;
			case GF_DARK_WEAK: q="plunge"; s ="into darkness"; break;
			case GF_WATER_WEAK: q="soak"; u = "with water";break;
			case GF_PLASMA: q="blast"; u = "with plasma";break;
			case GF_METEOR: q="blast"; u = "with meteors";break;
			case GF_ICE: q="cover"; u = "with ice";break;
			case GF_GRAVITY: q="crush"; u = "with gravity";break;
			case GF_INERTIA: q="slow"; u = "with inertia";break;
			case GF_FORCE: q="impact"; u = "with force";break;
			case GF_TIME: q="take"; u = "back in time";break;
			case GF_ACID:   q = "dissolve"; break;
			case GF_ELEC:   q = "electrify"; break;
			case GF_FIRE:   q = "burn"; break;
			case GF_COLD:   q = "freeze"; break;
			case GF_POIS: q = "poison"; break;
			case GF_ANIM_DEAD: q = "animate dead"; break;
			case GF_LITE: q = "blast"; u = "with powerful light";break;
			case GF_DARK: q = "blast"; u = "with powerful darkness";break;
			case GF_WATER: q="blast"; u = "with water";break;
		        case GF_CONFUSION:      q = "confuse"; break;
			case GF_SOUND: q = "deafen";break;
			case GF_SHARD: q = "blast"; u = "with shards";break;
			case GF_NEXUS: q = "blast"; u = "with nexus";break;
			case GF_NETHER: q = "blast"; u = "with nether";break;
			case GF_CHAOS: q = "blast"; u = "with chaos";break;
			case GF_DISENCHANT: q = "blast"; u = "with disenchantment";break;
			case GF_KILL_WALL: q = "remove"; s = "rock from"; break;
			case GF_KILL_DOOR: q = "remove"; s = "doors from"; break;
			case GF_KILL_TRAP: q = "remove"; s = "traps from"; break;
			case GF_MAKE_WALL: q = "create"; s = "walls around"; break;
			case GF_MAKE_DOOR: q = "create"; s = "doors around"; break;
			case GF_MAKE_TRAP: q = "create"; s = "traps around"; break;
			case GF_BRIDGE: q = "create"; s = "a stone bridge under"; break;
			case GF_ANIM_ELEMENT: q = "animate elements"; break;
			case GF_AWAY_UNDEAD: q = "teleport"; u="away if undead";break;
			case GF_AWAY_EVIL: q = "teleport"; u="away if evil";break;
			case GF_AWAY_ALL: q = "teleport"; u = "away";break;
			case GF_AWAY_DARK: q = "teleport"; u = "away only in darkness";break;
			case GF_AWAY_NATURE: q = "teleport"; u = "away if adjacent to water or nature"; break;
			case GF_AWAY_FIRE: q = "teleport"; u = "away if adjacent to fire, smoke or lava"; break;			
			case GF_AWAY_JUMP: q = "jump"; u = "away to a location still in line of fire"; break;
			case GF_TURN_UNDEAD: q = "turn"; u="if undead"; break;
			case GF_TURN_EVIL: q = "turn"; u="if evil"; break;
			case GF_FEAR_WEAK: q = "scare";break;
			case GF_DISP_UNDEAD: q = "dispel"; u="if undead"; break;
			case GF_DISP_EVIL: q = "dispel"; u="if evil"; break;
			case GF_DISP_ALL: q = "dispel";break;
			case GF_ANIM_OBJECT: q = "animate objects"; break;
			case GF_CLONE: q = "clone";break;
			case GF_POLY: q = "polymorph";break;
			case GF_HEAL: case GF_HEAL_PERC: q = "heal";break;
			case GF_HASTE: q = "hasten";break;
			case GF_SLOW_WEAK: q = "slow";break;
			case GF_CONF_WEAK: q = "confuse";break;
			case GF_SLEEP: q = "send"; u="to sleep";break;
			case GF_DRAIN_LIFE: q = "drain"; s="life from";break;
			case GF_LAVA: q = "burn"; u="with lava";break;
			case GF_BWATER: q = "scald"; u="with boiling water";break;
			case GF_BMUD: q = "splash"; u="with boiling mud";break;
			case GF_HURT:   q = "attack"; break;
			case GF_UN_BONUS:       q = "disenchant"; break;
			case GF_UN_POWER:       q = "drain charges from"; break;
			case GF_EAT_GOLD:       q = "steal gold from"; break;
			case GF_EAT_ITEM:       q = "steal items from"; break;
			case GF_EAT_FOOD:       q = "eat"; u="r food"; break;
			case GF_EAT_LITE:       q = "absorb light from"; break;
			case GF_FALL: q = "drop"; u="into a pit";break;
			case GF_FALL_MORE: q = "drop"; u="through the floor";break;
			case GF_FALL_LESS: q = "rise"; u="through the ceiling";break;
			case GF_FALL_SPIKE: q = "drop"; u="into a spiked pit";break;
			case GF_FALL_POIS: q = "drop"; u="into a poison spiked pit";break;
			case GF_BLIND:  q = "blind"; break;
			case GF_TERRIFY:	q = "terrify"; break;
			case GF_PARALYZE:       q = "paralyze"; break;
			case GF_LOSE_STR:       q = "reduce"; s="strength and size from"; break;
			case GF_LOSE_INT:       q = "reduce"; s="intelligence from"; break;
			case GF_LOSE_WIS:       q = "reduce"; s="wisdom from"; break;
			case GF_LOSE_DEX:       q = "reduce"; s="dexterity and agility from"; break;
			case GF_LOSE_CON:       q = "reduce"; s="constitution from"; break;
			case GF_LOSE_CHR:       q = "reduce"; s="charisma from"; break;
			case GF_LOSE_ALL:       q = "reduce"; s="all stats from"; break;
			case GF_SHATTER:	q = "shatter"; break;
			case GF_EXP_10: q = "drain"; s="experience (by 10d6+) from"; break;
			case GF_EXP_20: q = "drain"; s="experience (by 20d6+) from"; break;
			case GF_EXP_40: q = "drain"; s="experience (by 40d6+) from"; break;
			case GF_EXP_80: q = "drain"; s="experience (by 80d6+) from"; break;
			case GF_RAISE:      q = "raise"; s = "water around"; break;
			case GF_LOWER:		q = "lower"; s = "water around"; break;
			case GF_PROBE: q = "probe"; q = NULL; break;
			case GF_LOCK_DOOR: q = "magically lock"; s = "doors on"; break;
			case GF_HALLU: q = "space out"; break;
			case GF_STEAM:	q = "scald"; u="with steam"; break;
			case GF_VAPOUR:	q = "dissolve"; u="with acidic vapour"; break;
			case GF_SMOKE:	q = "burn"; u="with smoke"; break;
			case GF_SUFFOCATE:	q = "suffocate"; break;
			case GF_HUNGER:		q = "starve"; break;
			case GF_DISEASE:	q = "infect"; u="with disease"; break;
			case GF_LOSE_MANA:	q = "drain"; s= "mana from"; break;
			case GF_WOUND:		q = "wound"; break;
			case GF_BATTER:		q = "batter"; break;
			case GF_BLIND_WEAK:		q = "blind"; break;
			case GF_RAISE_DEAD: q = "raise dead"; break;
			case GF_GAIN_MANA: case GF_GAIN_MANA_PERC:	q = "add"; s= "mana to"; break;
			case GF_FORGET:		q = "forget"; break;
			case GF_CURSE:		q = "curse"; break;
			case GF_DISPEL:		q = "remove"; s = "enchantments from"; break;
			case GF_STASTIS:	q = "trap"; u= "in time loops"; break;
			case GF_PETRIFY:	q = "petrify"; break;
			case GF_WEB:		q = "build"; s = "webs around"; break;
			case GF_BLOOD:		q = "cover"; u = "in blood"; break;
			case GF_SLIME:		q = "cover"; u = "in slime"; break;
			case GF_HURT_WOOD:	q = "warp wood out of shape"; break;
			case GF_ANIM_TREE:	q = "animate trees"; break;
			case GF_CHARM_INSECT:	q = "charm insects"; break;
			case GF_CHARM_REPTILE:	q = "charm reptiles or amphibians"; break;
			case GF_CHARM_ANIMAL:	q = "charm birds or mammals"; break;
			case GF_CHARM_MONSTER:	q = "charm living monsters other than dragons"; break;
			case GF_CHARM_PERSON:	q = "charm elves, dwarves, humans, orcs, trolls or giants"; break;
			case GF_BIND_DEMON:		q = "bind demons to a cursed item"; break;
			case GF_BIND_DRAGON:	q = "bind dragons to a cursed item"; break;
			case GF_BIND_UNDEAD:	q = "bind undead to a cursed item"; break;
			case GF_BIND_FAMILIAR:	q = "bind a familiar to you"; break;
			case GF_VAMP_DRAIN:	q = "drain"; s = "health from"; break;
			case GF_MANA_DRAIN:	q = "drain"; s = "mana from"; break;
			case GF_SNUFF:		q = "snuff"; s = "the life from"; u ="if they have less than a maximum"; v="of";break;
			case GF_RAGE:		q = "enrage"; break;
			case GF_MENTAL:		q = "blast"; u = "with mental energy"; break;
			case GF_TANGLE:		q = "entangle"; u = "with nearby plants or waterweeds"; break;

			/* Hack -- handle features */
			case GF_FEATURE:
			{
				char buf[80];
				cptr name = f_name + f_info[f_info[d3].mimic].name;

				q = "magically create";
				s = buf;
				sprintf(buf,"%ss %s",name,f_info[f_info[d3].mimic].flags1 & (FF1_MOVE) ? "under" : "around" );
				if ((f_info[d3].flags1 & (FF1_MOVE)) == 0)
				{
					d1 = 4;
					d2 = 8;
				}
				else
				{
					d1 = 0;
					d2 = 0;
				}
				d3 = 0;

			}

		}

		/* Introduce the attack description */
		if (!introduced)
		{
			/* Intro */
			text_out(intro);

			introduced = TRUE;
		}
		else if (!r)
		{
			text_out(", and ");
		}
		else if (r < n-1)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}

		/* Describe the method */
		if (p)
		{
			text_out(p);
			if (rng || arc || rad) text_out(" of ");
			if (rng) text_out (format( "range %d",rng));
			if (rng && (arc || rad)) text_out(" and ");
			if (arc) text_out (format( "%d degrees",arc));
			if ((rng || arc) && rad) text_out(" and ");
			if (rad) text_out (format( "radius %d",rad));

			text_out(" to");
			if (q)
			{
				text_out(" ");
				text_out(q);
			}
			if (s)
			{
				text_out(" ");
				text_out(s);
			}
			if (t)
			{
				text_out(" ");
				text_out(t);
			}
		}
		else
		{
			if (q)
			{
				text_out(q);
				text_out("s");
			}		
			else text_out("affects");

			if (rng) text_out (format( " at up to range %d",rng));

			if (s)
			{
				text_out(" ");
				text_out(s);
			}
			if (t)
			{
				text_out(" ");
				text_out(t);
			}
			if (rad) text_out (format( " of radius %d",rad));
		}

		if (u)
		{
			text_out(" ");
			text_out(u);
		}

		/* Display the damage */
		/* Roll out the damage */
		if (!detail)
		{
			/* Nothing */
		}
		else if (target == SPELL_TARGET_COATED)
		{
			int min = (d1 + d3) / 5;
			int max = (d1 * d2 + d3) / 5;

			if (max)
			{
				/* End */
				if (max != min) text_out(format(" %s %d-%d ", v, min, max));
				else text_out(format(" %s %d ", v, max));
			}
			else
			{
				/* Hack */
				d1 = d2 = d3 = 0;
			}
		}
		else if ((d1) && (d2) && (d3))
		{
			/* End */
			text_out(format(" %s %dd%d+%d ",v, d1,d2,d3));
		}
		else if ((d1) && (d2) && (d2 == 1))
		{
			/* End */
			text_out(format(" %s %d ",v, d1));
		}
		else if ((d1) && (d2))
		{
			/* End */
			text_out(format(" %s %dd%d ",v, d1,d2));
		}
		else if (d3)
		{
			/* End */
			text_out(format(" %s %d ",v, d3));
		}

		/* Get the effect */
		if ((d1 || d2 || d3) && (detail)) switch (effect)
		{
			case GF_FEATURE: text_out("damage, destroying living monsters that cannot escape to an adjacent grid"); break;
			case GF_WIND: text_out("damage against flying or climbing monsters, less against others"); break;
			case GF_LITE_WEAK: text_out("damage to monsters vulnerable to light"); break;
			case GF_KILL_WALL: text_out("damage to monsters made from rock"); break;
			case GF_HURT_WOOD: text_out("damage to monsters made from wood"); break;			
			case GF_RAISE: case GF_LOWER: text_out("damage to monsters made from water"); break;
			case GF_HOLY_ORB: text_out("damage, doubled against evil monsters"); break;
			case GF_BLIND: text_out("damage, doubled against eye monsters"); break;
			case GF_HASTE: 
			case GF_SLOW_WEAK: 
			case GF_CONF_WEAK: 
			case GF_SLEEP: 
			case GF_BLIND_WEAK:
			case GF_TURN_UNDEAD: 
			case GF_TURN_EVIL: 
			case GF_FEAR_WEAK: 
			case GF_CLONE: 
			case GF_POLY:
			case GF_CHARM_INSECT:
			case GF_CHARM_REPTILE:
			case GF_CHARM_ANIMAL:
			case GF_CHARM_MONSTER:
			case GF_CHARM_PERSON:
			case GF_BIND_DEMON:
			case GF_BIND_DRAGON:
			case GF_BIND_UNDEAD:
			case GF_BIND_FAMILIAR:
			case GF_RAGE:
			case GF_DARK_WEAK:
			case GF_TANGLE:
								text_out("power"); break;
			case GF_HEAL_PERC: text_out("percent of ");
			case GF_SNUFF: 
			case GF_HEAL: text_out("hit points"); break;
			case GF_AWAY_ALL:
			case GF_AWAY_EVIL:
			case GF_AWAY_UNDEAD:
							text_out("distance on average"); break;
			case GF_AWAY_JUMP:
			case GF_AWAY_DARK:
			case GF_AWAY_NATURE:
			case GF_AWAY_FIRE:
			{
				text_out("distance on average");

				/* Hack for questionable efficiency */
				if (strstr(u, " "))
				{
					/* Skip two words */
					u = strstr(u, " ");
					u++;
					u = strstr(u, " ");
					u++;
					
					text_out(format(".  The destination will also be %s", u));
				}
				break;
			}
			case GF_GAIN_MANA_PERC: text_out("percent of ");
			case GF_LOSE_MANA:
			case GF_MANA_DRAIN:
			case GF_GAIN_MANA: text_out("spell points"); break;
			default: text_out("damage"); break;
		}
		r++;
	}

	return (introduced);

}


/*
 * Update spell details if required.
 * 
 * This analyses the surrounding area and modifies the spell blow.
 */
static void spell_update_power(spell_type *s_ptr, int level)
{
	int power = 0;
	
	if (character_dungeon) switch(s_ptr->type)
	{
		case SPELL_CONCENTRATE_LITE:
		{
			power = concentrate_power(SOURCE_PLAYER_START, p_ptr->py, p_ptr->px,
					5 + level / 10, FALSE, TRUE, concentrate_light_hook);
			
			if (s_ptr->l_dice && !s_ptr->l_side) s_ptr->l_side = power;
			else s_ptr->l_plus = power;
			break;
		}
	
		case SPELL_CONCENTRATE_LIFE:
		{
			power = s_ptr->l_plus = concentrate_power(SOURCE_PLAYER_START, p_ptr->py, p_ptr->px,
					5 + level / 10, FALSE, FALSE, concentrate_life_hook);
			
			if (s_ptr->l_dice && !s_ptr->l_side) s_ptr->l_side = power;
			else s_ptr->l_plus = power;
			break;
		}
		
		case SPELL_CONCENTRATE_WATER:
		{
			power = concentrate_power(SOURCE_PLAYER_START, p_ptr->py, p_ptr->px,
					5 + level / 10, FALSE, FALSE, concentrate_water_hook);
			
			if (s_ptr->l_dice && !s_ptr->l_side) s_ptr->l_side = power;
			else s_ptr->l_plus = power;
			break;
		}
	}
	
	/* Hack -- modify spell power */
	if (power)
	{
		if (s_ptr->l_dice && !s_ptr->l_side) s_ptr->l_side = power;
		else s_ptr->l_plus += power;
	}
}


/*
 * Hack -- Get spell description.
 */
bool spell_desc(spell_type *s_ptr, const cptr intro, int level, bool detail, int target)
{
	bool anything = FALSE;
	
	spell_update_power(s_ptr, level);
	anything |= spell_desc_flags(s_ptr, intro, level, detail, target, anything);
	anything |= spell_desc_blows(s_ptr, intro, level, detail, target, anything);

	return (anything);
}


/*
 * Extra information on a spell		-DRS-
 *
 * We can use up to 14 characters of the buffer 'p'
 *
 * The strings in this function were extracted from the code in the
 * functions "do_cmd_cast()" and "do_cmd_pray()" and may be dated.
 *
 * Note they do not take account of modifiers to player level.
 */
void spell_info(char *p, int p_s, int spell, bool use_level)
{
	cptr q;

	spell_type *s_ptr = &s_info[spell];

	int m,n,rad;

	int level = spell_power(spell);

	if (!use_level) level = 0;

	/* Update spell if required */
	spell_update_power(s_ptr, use_level);
	
	/* Default */
	my_strcpy(p, "", p_s);

	/* Roll out the duration */
	if ((s_ptr->l_dice) && (s_ptr->l_side) && (s_ptr->l_plus))
	{
		/* End */
		my_strcpy(p,format(" dur %dd%d+%d",s_ptr->l_dice,s_ptr->l_side,s_ptr->l_plus), p_s);
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side) && (s_ptr->l_side == 1))
	{
		/* End */
		my_strcpy(p,format(" dur %d",s_ptr->l_dice), p_s);
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side))
	{
		/* End */
		my_strcpy(p,format(" dur %dd%d",s_ptr->l_dice,s_ptr->l_side), p_s);
	}
	else if (s_ptr->l_plus)
	{
		/* End */
		my_strcpy(p,format(" dur %d",s_ptr->l_plus), p_s);
	}

	rad = 0;

	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < 4; m++)
	{
		/* Skip non-attacks */
		if (!s_ptr->blow[m].method) continue;

		/* Count known attacks */
		n++;
	}

	/* Examine (and count) the actual attacks */
	for (m = 0; m < 4; m++)
	{
		int method, effect, d1, d2, d3, rad;

		/* Skip non-attacks */
		if (!s_ptr->blow[m].method) continue;

		/* Extract the attack info */
		method = s_ptr->blow[m].method;
		effect = s_ptr->blow[m].effect;
		d1 = s_ptr->blow[m].d_dice;
		d2 = s_ptr->blow[m].d_side;
		d3 = s_ptr->blow[m].d_plus;
		rad = 0;

		/* Hack -- heroism/berserk strength */
		if (((s_ptr->l_dice) || (s_ptr->l_side) || (s_ptr->l_plus)) && (effect == GF_HEAL)) continue;

		/* Hack -- use level as modifier */
		if ((!d2) && (!level))
		{
			d3 += 25 * d1;
		}

		/* Hack -- use level as modifier */
		else if (!d2)
		{
			d3 += level * d1;
		}

		/* Get the method */
		switch (method)
		{
			case RBM_TOUCH: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_HANDS: if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_MISSILE: if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_BOLT_10: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BOLT: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BOLT_MINOR: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BEAM: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BLAST: d3 += level;break;
			case RBM_WALL: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BALL_MINOR: rad = 2; break;
			case RBM_BALL: rad = 2; break;
			case RBM_BALL_II: rad = 3; break;
			case RBM_BALL_III: rad = 4; break;
			case RBM_8WAY: rad = 2; break;
			case RBM_8WAY_II: rad = 3; break;
			case RBM_8WAY_III: rad = 4; break;
			case RBM_CLOUD: rad = 2; d3 += level/2; break;
			case RBM_STORM: rad = 3; break;
			case RBM_AREA: rad = (level/10)+2; break;
			case RBM_ORB: rad = (level < 30 ? 2 : 3); d3 += level/2; break;
			case RBM_SWARM: d3 += level / 2; rad = 1; break;
			case RBM_BREATH: d3 = p_ptr->chp * d3 / 300; break;
			case RBM_STRIKE: if ((level > 5) && (d2)) d1+= (level-1)/5; break;
			case RBM_SPHERE: rad = (level/10)+2;break;
			case RBM_FLASK: rad = 1; break;
		}

		/* Default */
		q = NULL;

		/* Get the effect */
		if (d1 || d2 || d3) switch (effect)
		{
			case GF_HASTE: 
			case GF_SLOW_WEAK: 
			case GF_CONF_WEAK: 
			case GF_SLEEP: 
			case GF_BLIND_WEAK:
			case GF_TURN_UNDEAD: 
			case GF_TURN_EVIL: 
			case GF_FEAR_WEAK: 
			case GF_CLONE: 
			case GF_POLY:
			case GF_CHARM_INSECT:
			case GF_CHARM_REPTILE:
			case GF_CHARM_ANIMAL:
			case GF_CHARM_MONSTER:
			case GF_CHARM_PERSON:
			case GF_BIND_DEMON:
			case GF_BIND_DRAGON:
			case GF_BIND_UNDEAD:
			case GF_BIND_FAMILIAR:
			case GF_RAGE:
			case GF_DARK_WEAK:
			case GF_TANGLE:
								q = "pow"; break;
			case GF_SNUFF: q = "max hp"; break;
			case GF_HEAL: q = "heal"; break;
			case GF_HEAL_PERC: q = "heal %"; break;			
			case GF_AWAY_ALL:
			case GF_AWAY_JUMP:
			case GF_AWAY_DARK:
			case GF_AWAY_NATURE:
			case GF_AWAY_FIRE:
			case GF_AWAY_EVIL:
			case GF_AWAY_UNDEAD:
								q = "range"; break;
			case GF_LOSE_MANA:
			case GF_MANA_DRAIN:
			case GF_GAIN_MANA: q = "mana"; break;
			case GF_GAIN_MANA_PERC: q = "mana %"; break;
			default: q="dam"; break;
		}

		/* Display the damage */
		/* Roll out the damage */
		if ((d1) && (d2) && (d3))
		{
			/* End */
			my_strcpy(p,format(" %s %dd%d+%d ",q,d1,d2,d3), p_s);
		}
		else if ((d1) && (d2) && (d2 == 1))
		{
			/* End */
			my_strcpy(p,format(" %s %d ",q,d1), p_s);
		}
		else if ((d1) && (d2))
		{
			/* End */
			my_strcpy(p,format(" %s %dd%d ",q,d1,d2), p_s);
		}
		else if (d3)
		{
			/* End */
			my_strcpy(p,format(" %s %d ",q,d3), p_s);
		}

	}

}



/*
 * Pair together a constant flag with a textual description.
 *
 * Note that it sometimes more efficient to actually make an array
 * of textual names, where entry 'N' is assumed to be paired with
 * the flag whose value is "1L << N", but that requires hard-coding.
 */
typedef struct o_flag_desc
{
	u32b flag;
	cptr desc;
} o_flag_desc;

/*
 * These are used for "+3 to STR, DEX", etc. These are separate from
 * the other pval affected traits to simplify the case where an object
 * affects all stats.  In this case, "All stats" is used instead of
 * listing each stat individually.
 */
static const o_flag_desc stat_flags_desc[A_MAX - 2 /* disregarding AGI and SIZ */] =
{
	{ TR1_STR,	"strength and size" },
	{ TR1_INT,	"intelligence" },
	{ TR1_WIS,	"wisdom" },
	{ TR1_DEX,	"dexterity and agility" },
	{ TR1_CON,	"constitution" },
	{ TR1_CHR,	"charisma" }
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */
static const o_flag_desc pval_flags1_desc[] =
{
	{ TR1_SAVE,       "spell resistance" },
	{ TR1_DEVICE,     "magical devices" },
	{ TR1_STEALTH,    "stealth" },
	{ TR1_SEARCH,     "searching" },
	{ TR1_INFRA,      "infravision" },
	{ TR1_TUNNEL,     "tunneling" },
	{ TR1_SPEED,      "speed" },
	{ TR1_BLOWS,      "attacks" },
	{ TR1_SHOTS,      "shots" },
	{ TR1_MIGHT,      "might" }
};


/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */
static const o_flag_desc pval_flags3_desc[] =
{
	{ TR3_LITE,       "light radius" },
	{ TR3_REGEN_HP,       "hitpoint regeneration" },
	{ TR3_REGEN_MANA,     "mana regeneration" },
};


/*
 * Slays(x3 damage) for weapons
 */
static const o_flag_desc slayx3_flags1_desc[] =
{
	{ TR1_SLAY_UNDEAD,	"undead" },
	{ TR1_SLAY_DEMON,	"demons" },
	{ TR1_SLAY_DRAGON,	"dragons" }
};


/*
 * Slays(x4 damage) for weapons
 */
static const o_flag_desc slayx4_flags1_desc[] =
{
	{ TR1_SLAY_NATURAL,	"natural creatures" },
	{ TR1_SLAY_ORC,		"orcs" },
	{ TR1_SLAY_TROLL,	"trolls" },
	{ TR1_SLAY_GIANT,	"giants" }
};

/*
 * Slays(x4 damage) for weapons
 */
static const o_flag_desc slayx4_flags4_desc[] =
{
	{ TR4_SLAY_MAN,	"men" },
	{ TR4_SLAY_ELF,	"elves" },
	{ TR4_SLAY_DWARF,	"dwarves" }
};

/*
 * Slays(x5 damage) for weapons
 */
static const o_flag_desc slayx5_flags1_desc[] =
{
	{ TR1_KILL_UNDEAD,	"undead" },
	{ TR1_KILL_DEMON,	"demons" },
	{ TR1_KILL_DRAGON,	"dragons" }
};


/*
 * Brands(x3 damage) for weapons
 */
static const o_flag_desc brandx3_flags1_desc[] =
{
	{ TR1_BRAND_POIS,       "poison" },
	{ TR1_BRAND_ACID,       "acid" },
	{ TR1_BRAND_ELEC,       "electricity" },
	{ TR1_BRAND_FIRE,       "fire" },
	{ TR1_BRAND_COLD,       "cold" },
	{ TR1_BRAND_HOLY,	"holiness" }
};

/*
 * Brands(x3 damage) for weapons
 */
static const o_flag_desc brandx3_flags4_desc[] =
{
	{ TR4_BRAND_LITE,   "light" },
	{ TR4_BRAND_DARK,   "darkness" }
};


/*
 * Brands(extra damage unspecified) for weapons
 */
static const o_flag_desc brand_flags3_desc[] =
{
	{ TR3_IMPACT,   "earthquakes" }
};


/*
 * Vampirism for weapons
 */
static const o_flag_desc vamp_flags4_desc[] =
{
	{ TR4_VAMP_HP,		"blood" },
	{ TR4_VAMP_MANA,	"mana" }
};


/*
 * Sustain stats -  these are given their "own" line in the
 * spoiler file, mainly for simplicity
 */
static const o_flag_desc sustain_flags_desc[] =
{
	{ TR2_SUST_STR,   "strength and size" },
	{ TR2_SUST_INT,   "intelligence" },
	{ TR2_SUST_WIS,   "wisdom" },
	{ TR2_SUST_DEX,   "dexterity and agility" },
	{ TR2_SUST_CON,   "constitution" },
	{ TR2_SUST_CHR,   "charisma" }
};

/*
 * Resistances
 */
static const o_flag_desc resist_flags2_desc[] =
{
	{ TR2_RES_ACID,	"acid" },
	{ TR2_RES_ELEC, "electricity" },
	{ TR2_RES_FIRE, "fire" },
	{ TR2_RES_COLD, "cold" },
	{ TR2_RES_POIS, "poison" },
	{ TR2_RES_FEAR, "fear" },
	{ TR2_RES_LITE, "powerful light" },
	{ TR2_RES_DARK, "powerful darkness" },
	{ TR2_RES_CONFU, "powerful confusion" },
	{ TR2_RES_SOUND, "sound" },
	{ TR2_RES_SHARD, "shards" },
	{ TR2_RES_NEXUS, "nexus" },
	{ TR2_RES_NETHR, "nether" },
	{ TR2_RES_CHAOS, "chaos" },
	{ TR2_RES_DISEN, "disenchantment" }
};

/*
 * Resistances
 */
static const o_flag_desc resist_flags3_desc[] =
{
	{ TR3_HOLD_LIFE,	"losing experience" }
};


/*
 * Resistances
 */
static const o_flag_desc resist_flags4_desc[] =
{
	{ TR4_RES_DISEASE,	"disease" }	
};


/*
 * Immunities
 */
static const o_flag_desc immune_flags2_desc[] =
{
	{ TR2_IM_ACID,  "acid" },
	{ TR2_IM_ELEC,  "electricity" },
	{ TR2_IM_FIRE,  "fire" },
	{ TR2_IM_COLD,  "cold" }
};


/*
 * Immunities
 */
static const o_flag_desc immune_flags4_desc[] =
{
	{ TR4_IM_POIS,  "poison" }
};


/*
 * Protections
 */
static const o_flag_desc protect_flags2_desc[] =
{
	{ TR2_RES_POIS,	 	"the effects of poison" },
	{ TR2_RES_BLIND,	"blindness" },
	{ TR2_RES_SOUND,	"stunning" },
	{ TR2_RES_SHARD,	"cuts" },
	{ TR2_RES_CHAOS,	"hallucination" },
	{ TR2_RES_CONFU,	"confusion" }
};


/*
 * Protections
 */
static const o_flag_desc protect_flags3_desc[] =
{
	{ TR3_FREE_ACT, "paralysis" },
	{ TR3_FREE_ACT, "magical slowness" }
};


/*
 * Protections
 */
static const o_flag_desc protect_flags4_desc[] =
{
	{ TR4_ANCHOR,	 	"involuntary teleporation" }
};


/*
 * Vulnerability
 */
static const o_flag_desc vulner_flags4_desc[] =
{
	{ TR4_HURT_LITE,  	"light" },
	{ TR4_HURT_WATER,  	"water" },
	{ TR4_HURT_POIS,  	"poison" },
	{ TR4_HURT_ACID,  	"acid" },
	{ TR4_HURT_ELEC,	"elec" },
	{ TR4_HURT_FIRE,	"fire" },
	{ TR4_HURT_COLD,	"cold" }
};

/*
 * Miscellaneous magic given by an object's "flags3" field
 */
static const o_flag_desc misc_flags3_desc[] =
{
	{ TR3_SLOW_DIGEST,	"slow digestion" },
	{ TR3_FEATHER,		"feather falling" },
	{ TR3_TELEPATHY,	"telepathy" },
	{ TR3_SEE_INVIS,	"see invisible" }
};

/*
 * Sensing magic given by an object's "flags3" field
 */
static const o_flag_desc sense_flags3_desc[] =
{
	{ TR3_ESP_DEMON,	"demons" },
	{ TR3_ESP_DRAGON,	"dragons" },
	{ TR3_ESP_GIANT,	"giants" },
	{ TR3_ESP_ORC,		"orcs" },
	{ TR3_ESP_TROLL,	"trolls" },
	{ TR3_ESP_UNDEAD,	"undead" },
	{ TR3_ESP_NATURE,	"natural creatures" }
};

/*
 * Racial magic given by an object's "flags4" field
 */
static const o_flag_desc racial_flags4_desc[] =
{
	{ TR4_ANIMAL,	"an animal" },
	{ TR4_EVIL,	"evil" },
	{ TR4_UNDEAD,	"undead" },
	{ TR4_DEMON,	"demonic" },
	{ TR4_ORC,	"orcish" },
	{ TR4_TROLL,	"a troll" },
	{ TR4_GIANT,	"gigantic" },
	{ TR4_DRAGON,	"draconic" },
	{ TR4_MAN,	"a man" },
	{ TR4_DWARF,	"dwarven" },
	{ TR4_ELF,	"elven" }
};

/*
 * Ignores given by an object's "flags2" field
 *
 * Note that cursed artifacts and objects with permanent light
 * are handled "directly" -- see analyze_misc_magic()
 */
static const o_flag_desc ignore_flags2_desc[] =
{
	{ TR2_IGNORE_ACID,	"acid" },
	{ TR2_IGNORE_ELEC,      "electricity" },
	{ TR2_IGNORE_FIRE,      "fire" },
	{ TR2_IGNORE_COLD,      "cold" },
	{ TR2_IGNORE_WATER,     "water" }
};


/*
 * "Bad" magic given by an object's "flags3" field
 *
 * Note that cursed artifacts and objects with permanent light
 * are handled "directly" -- see analyze_misc_magic()
 */
static const o_flag_desc bad_flags3_desc[] =
{
	{ TR3_HUNGER,		"hunger" },
	{ TR3_UNCONTROLLED,		"random and uncontrolled activation" },
	{ TR3_AGGRAVATE,	"aggravation" },
	{ TR3_DRAIN_HP,		"health drain" },
	{ TR3_DRAIN_MANA,       "mana drain" },
	{ TR3_DRAIN_EXP,	"experience drain" }
};

/*
 * "Bad" magic given by an object's "flags4" field
 *
 */
static const o_flag_desc bad_flags4_desc[] =
{
	{ TR4_ANCHOR,		"inability to teleport" },
	{ TR4_SILENT,		"inability to cast spells" },
	{ TR4_STATIC,       	"inability to use magical devices" },
	{ TR4_WINDY,		"inability to use ranged weapons" }
};



/*
 * This function does most of the actual "analysis". Given a set of bit flags
 * (which will be from one of the flags fields from the object in question),
 * a "flag description structure", a "description list", and the number of
 * elements in the "flag description structure", this function sets the
 * "description list" members to the appropriate descriptions contained in
 * the "flag description structure".
 *
 * The possibly updated description pointer is returned.
 */

static cptr *spoiler_flag_aux(const u32b art_flags, const o_flag_desc *flag_x_ptr,
			      cptr *desc_x_ptr, const int n_elmnts)
{
	int i;

	for (i = 0; i < n_elmnts; ++i)
	{
		if (art_flags & flag_x_ptr[i].flag)
		{
			*desc_x_ptr++ = flag_x_ptr[i].desc;
		}
	}

	return desc_x_ptr;
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void obj_top(const object_type *o_ptr)
{
	char o_name[80];

	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 1);

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, o_name);
}

/*
 * Display an object at the top of the screen
 */
void screen_object(object_type *o_ptr)
{
	/* Flush messages */
	message_flush();

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Actually display the item */
        if (o_ptr->ident & (IDENT_MENTAL)) list_object(o_ptr, OBJECT_FLAGS_FULL);
        else list_object(o_ptr, OBJECT_FLAGS_KNOWN);

	/* Display monster attributes */
	if ((o_ptr->name3) && ((o_ptr->tval != TV_HOLD) || (object_named_p(o_ptr)))) screen_roff(&r_info[o_ptr->name3],&l_list[o_ptr->name3]);

	/* Display item name */
	obj_top(o_ptr);
}


/*
 * Display an object at the top of the screen that is part of the players shape.
 */
void screen_self_object(object_type *o_ptr, int slot)
{
	/* Flush messages */
	message_flush();

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Display item */
	text_out("This is a part of your current shape.  ");
	text_out("You cannot take it off, but it will be removed if you change shape.  ");

	/* Display spell item full flags */
        if (o_ptr->tval == TV_SPELL)
	{
		list_object(o_ptr, OBJECT_FLAGS_FULL);
	}
	/* Hack -- body parts and other objects */
	else
	{
		bool attack = FALSE;
		bool unarmed = FALSE;
		bool charging = FALSE;

		switch (slot)
		{
			case INVEN_WIELD:
			{
				if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM || o_ptr->tval == TV_HAFTED ||
					o_ptr->tval == TV_DIGGING) attack = TRUE;
				else
					text_out("You must fight using unarmed combat in this shape.  ");
				break;
			}
			case INVEN_HANDS:
			{
				attack = TRUE;
				unarmed = TRUE;
				break;
			}
			case INVEN_FEET:
			{
				charging = TRUE;
				unarmed = TRUE;
				break;
			}
		}

		if ((attack) || (charging))
		{
			if (attack) text_out("When attacking");
			else if (charging) text_out("When charging");
			if (attack && charging) text_out(" or charging");
			else if (attack) text_out(" unless charging");
			if (unarmed) text_out(" unarmed");
			text_out(format(", it does %dd%d", o_ptr->dd, o_ptr->ds));
			if (o_ptr->to_d > 0) text_out(format("+%d", o_ptr->to_d));
			else if (o_ptr->to_d < 0) text_out(format("%d", o_ptr->to_d));
			text_out(" ");
			text_out((o_ptr->tval == TV_SPELL) ? "magical" : ((o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM ||
				o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT)
				? "edged" : "blunt"));
			text_out(" damage.  ");
		}
	}

	/* Display item name */
	obj_top(o_ptr);
}


/*
 * This function displays lists of properties
 */
static bool outlist_pval(cptr header, const cptr *list, byte attr, int pval)
{
	/* Ignore an empty list */
	if (*list == NULL) return (FALSE);

	/* Create header (if one was given) */
	if (header && (header[0]))
	{
		text_out_c(attr, header);
		text_out_c(attr, " ");
	}

	/* Now begin the tedious task */
	while (1)
	{
		/* Print the current item */
		text_out_c(attr, *list);

		/*
		 * If there is an item following this one, pad with separator and a space
		 */
		if (list[1])
		{
			/* If there are two items, use a comma. */
			if (list[2]) text_out_c(attr, ", ");
			/* Otherwise, use "and" */
			else text_out_c(attr, " and ");
		}

		/* Advance, with break */
		if (!*++list) break;
	}
	
	if (pval)
	{
		text_out_c(attr, format(" by %d", ABS(pval)));
	}

	/* End the current list */
	text_out_c(attr, ".  ");

	/* Something was printed */
	return (TRUE);
}


/*
 * This function displays a list of non-pval dependent properties
 */
static bool outlist(cptr header, const cptr *list, byte attr)
{
	return (outlist_pval(header, list, attr, 0));
}


/* 
 * Create a spoiler file entry for an artifact.
 * We use this to list the flags.
 */
bool list_object_flags(u32b f1, u32b f2, u32b f3, u32b f4, int pval, int mode)
{
	const u32b all_stats = (TR1_STR | TR1_INT | TR1_WIS |
							TR1_DEX | TR1_CON | TR1_CHR);
	const u32b all_sustains = (TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS |
							   TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR);

	bool anything = FALSE; /* Printed anything at all */

	cptr list[40];
	cptr *list_ptr;

	/* Slays - x5 damage */
	if (f1)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, slayx5_flags1_desc, list_ptr, N_ELEMENTS(slayx5_flags1_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It does x5 damage against", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may do x5 damage against", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not do x5 damage against", list, TERM_SLATE);
				break;
		} 
	}


	/* Slays - x4 damage */
	if ((f1) || (f4))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, slayx4_flags1_desc, list_ptr, N_ELEMENTS(slayx4_flags1_desc));
		list_ptr = spoiler_flag_aux(f4, slayx4_flags4_desc, list_ptr, N_ELEMENTS(slayx4_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It does x4 damage against", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may do x4 damage against", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not do x4 damage against", list, TERM_SLATE);
				break;
		} 
	}

	/* Brands - x3 damage */
	if (f1 || f4)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, brandx3_flags1_desc, list_ptr, N_ELEMENTS(brandx3_flags1_desc));
		list_ptr = spoiler_flag_aux(f4, brandx3_flags4_desc, list_ptr, N_ELEMENTS(brandx3_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;

		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It does x3 damage from", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may do x3 damage from", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not do x3 damage from", list, TERM_SLATE);
				break;
		} 

	}

	/* Slays - x3 damage */
	if (f1)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, slayx3_flags1_desc, list_ptr, N_ELEMENTS(slayx3_flags1_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It does x3 damage against", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may do x3 damage against", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not do x3 damage against", list, TERM_SLATE);
				break;
		} 
	}

	/* Brands - extra damage */
	if (f3)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f3, brand_flags3_desc, list_ptr, N_ELEMENTS(brand_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;

		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It does extra damage from", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may do extra damage from", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does no extra damage from", list, TERM_SLATE);
				break;
		} 

	}


	/* Vampirism */
	if (f4)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f4, vamp_flags4_desc, list_ptr, N_ELEMENTS(vamp_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It feeds you stolen", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may feed you stolen", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not feed you stolen", list, TERM_SLATE);
				break;
		} 
	}


	/* Pval-affected flags */
	if (f1)
	{
		list_ptr = list;

		/* First, check to see if the pval affects all stats */
		if ((f1 & all_stats) == all_stats)
		{
		switch (mode)
		{
			case LIST_FLAGS_CAN:
			case LIST_FLAGS_MAY:
				*list_ptr++ = "all stats";
				break;
			case LIST_FLAGS_NOT:
				*list_ptr++ = "any stats";
				break;
		} 
		}

		/* Are any stats affected? */
		else if (f1 & all_stats)
		{
			list_ptr = spoiler_flag_aux(f1, stat_flags_desc, list_ptr, N_ELEMENTS(stat_flags_desc));
		}

		/* And now the "rest" */
		list_ptr = spoiler_flag_aux(f1, pval_flags1_desc, list_ptr, N_ELEMENTS(pval_flags1_desc));

		/* And now the "rest" */
		list_ptr = spoiler_flag_aux(f3, pval_flags3_desc, list_ptr, N_ELEMENTS(pval_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;

		/* Print the Pval */
		if (!(*list == NULL))
		{
			byte attr = TERM_WHITE;
			switch (mode)
			{
				case LIST_FLAGS_CAN:
					if (pval > 0) text_out_c(TERM_WHITE, "It increases your ");
					else if (pval < 0) text_out_c(TERM_WHITE, "It decreases your ");
					else text_out_c(TERM_WHITE,"It modifies your ");
					break;
				case LIST_FLAGS_MAY:
					text_out_c(TERM_L_WHITE,"It may modify your ");
					attr= TERM_L_WHITE;
					break;
				case LIST_FLAGS_NOT:
					text_out_c(TERM_SLATE,"It does not modify your ");
					attr = TERM_SLATE;
					break;
			} 
			anything |= outlist_pval(NULL, list,attr, mode == LIST_FLAGS_CAN ? pval : 0);
		}
	}

	/* Sustains */
	if (f2 & all_sustains)
	{
		list_ptr = list;

		/* Simplify things if an item sustains all stats */
		if ((f2 & all_sustains) == all_sustains)
		{
			switch (mode)
			{
				case LIST_FLAGS_CAN:
				case LIST_FLAGS_MAY:
					*list_ptr++ = "all stats";
					break;
				case LIST_FLAGS_NOT:
					*list_ptr++ = "any stats";
					break;
			} 
		}

		/* Should we bother? */
		else if ((f2 & all_sustains))
		{
			list_ptr = spoiler_flag_aux(f2, sustain_flags_desc, list_ptr, N_ELEMENTS(sustain_flags_desc));
		}

		/* Terminate the description list */
		*list_ptr = NULL;

		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It sustains", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may sustain", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not sustain", list, TERM_SLATE);
				break;
		} 
	}

	/* Resistance flags */
	if ((f2) || (f3) || (f4))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, resist_flags2_desc, list_ptr, N_ELEMENTS(resist_flags2_desc));
		list_ptr = spoiler_flag_aux(f3, resist_flags3_desc, list_ptr, N_ELEMENTS(resist_flags3_desc));
		list_ptr = spoiler_flag_aux(f4, resist_flags4_desc, list_ptr, N_ELEMENTS(resist_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It provides resistance to", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may provide resistance to", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not provide resistance to", list, TERM_SLATE);
				break;
		} 
	}

	/* Immunity flags */
	if ((f2) || (f4))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, immune_flags2_desc, list_ptr, N_ELEMENTS(immune_flags2_desc));
		list_ptr = spoiler_flag_aux(f4, immune_flags4_desc, list_ptr, N_ELEMENTS(immune_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It provides immunity to", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may provide immunity to", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not provide immunity to", list, TERM_SLATE);
				break;
		} 
	}

	/* Protects flags */
	if ((f2) || (f3) || (f4))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, protect_flags2_desc, list_ptr, N_ELEMENTS(protect_flags2_desc));
		list_ptr = spoiler_flag_aux(f3, protect_flags3_desc, list_ptr, N_ELEMENTS(protect_flags3_desc));
		list_ptr = spoiler_flag_aux(f4, protect_flags4_desc, list_ptr, N_ELEMENTS(protect_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It protects you from", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may protect you from", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not protect you from", list, TERM_SLATE);
				break;
		}
	}

	/* Vulnerability flags */
	if (f4)
	{
		list_ptr = list;
				
		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f4, vulner_flags4_desc, list_ptr, N_ELEMENTS(vulner_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It makes you vulnerable to", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may make you vulnerable to", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not make you vulnerable to", list, TERM_SLATE);
				break;
		} 
	}

	/* Miscellenious Abilities */
	if (f3)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, misc_flags3_desc, list_ptr,
					     N_ELEMENTS(misc_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It gives its wielder", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may give its wielder", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not give its wielder", list, TERM_SLATE);
				break;
		} 

		/* Note that blessed weapons have special treatment */
		if (f3 & TR3_BLESSED)
		{
			switch (mode)
			{
				case LIST_FLAGS_CAN:
					text_out_c(TERM_WHITE, "It is blessed by the gods, allowing priests to wield it.  ");
					break;
				case LIST_FLAGS_MAY:
					text_out_c(TERM_L_WHITE, "It might be blessed by the gods.  ");
					break;
				case LIST_FLAGS_NOT:
					text_out_c(TERM_SLATE, "It is not blessed by the gods.  ");
					break;
			} 
			anything = TRUE;
		}

		/* Note that throwing weapons have special treatment */
		if (f3 & TR3_THROWING)
		{
			switch (mode)
			{
				case LIST_FLAGS_CAN:
					text_out_c(TERM_WHITE, "It is balanced for throwing.  ");
					break;
				case LIST_FLAGS_MAY:
					text_out_c(TERM_L_WHITE, "It might be balanced for throwing.  ");
					break;
				case LIST_FLAGS_NOT:
					text_out_c(TERM_SLATE, "It is not balanced for throwing.  ");
					break;
			} 
			anything = TRUE;
		}
	}

	/* Miscellenious Abilities */
	if (f3)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, sense_flags3_desc, list_ptr,
					     N_ELEMENTS(sense_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It senses", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may sense", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not sense", list, TERM_SLATE);
				break;
		} 

	}

	/* Miscellenious Abilities */
	if (f2)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f2, ignore_flags2_desc, list_ptr, N_ELEMENTS(ignore_flags2_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It is not damaged by", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It might be damaged by", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It is damaged by", list, TERM_SLATE);
				break;
		} 

		/* Note that blessed weapons have special treatment */
		if (f2 & TR2_IGNORE_THEFT)
		{
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				text_out_c(TERM_WHITE, "It cannot be stolen from your inventory.  ");
				break;
			case LIST_FLAGS_MAY:
				text_out_c(TERM_L_WHITE, "It might be stolen from your inventory.  ");
				break;
			case LIST_FLAGS_NOT:
				text_out_c(TERM_SLATE, "It can be stolen from your inventory.  ");
				break;
		} 
		anything = TRUE;
		}
	}

	/* Racial effects */
	if (f4)
	{
		list_ptr = list;
				
		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f4, racial_flags4_desc, list_ptr, N_ELEMENTS(racial_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It marks you as", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may mark you as", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not mark you as", list, TERM_SLATE);
				break;
		} 
	}

	/* Negative effects */
	if (f3 || f4)
	{
		list_ptr = list;
				
		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, bad_flags3_desc, list_ptr, N_ELEMENTS(bad_flags3_desc));
		list_ptr = spoiler_flag_aux(f4, bad_flags4_desc, list_ptr, N_ELEMENTS(bad_flags4_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It burdens you with", list, TERM_ORANGE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may burden you with", list, TERM_YELLOW);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not burden you with", list, TERM_SLATE);
				break;
		} 
	}

	return (anything);

}

const cptr inscrip_info[] =
{
		NULL,
		"It is a cursed artifact of some kind, that you may be able to enchant back to full strength.  ",
		"It is a cursed ego item of some kind.  ",
		"It is cursed, with negative effects if you attempt to use it.  ",
		"It is broken, having been blasted by powerful magic.  ",
		"It needs recharging.  ",
		"It is of good quality, but with no additional powers.  ",
		"It is a useful ego item.  ",
		"It is a useful artifact.  ",
		"It is of average quality or better, and not cursed.  It may be an ego item or artifact.  ",
		"It is of very good quality, but with no additional powers.  ",
		"It is of great quality, but with no additonal powers.  ",
		"It is a useful ego item, with a random hidden ability.  ",
		"It is an artifact that has resisted your destruction.  ",
		"It is an ego item or artifact that resisted being picked up or used.  ",
		"It is of average quality or worse, and may be cursed.  It may be an cursed ego item or artifact.  ",
		"It is better than average quality, and not cursed.  It may be an ego item or artifact.  ",
		"It is an ego item or artifact.  ",
		"It is an ego item, but may or may not be cursed.  ",
		"It is an ego item, with a random hidden ability, but may or may not be cursed.  ",
		"It is an artifact, but may or may not be cursed.  ",
		"There are no runes on it.  ",
		"There are runes on it.  It may be an ego item or artifact.  ",
		"It is of average quality, but may be damaged by wear and tear.  ",
		"It is valuable, but may or may not be cursed.  ",
		"It is better than average quality, but may or may not be cursed.  It may be an ego item or artifact.  ",
		"It is coated with a substance.  ",
		"It has a magically applied enchantment.  "
};


/* 
 * Create a spoiler file entry for an artifact 
 */
void list_object(const object_type *o_ptr, int mode)
{
	int i, n;

	u32b f1, f2, f3, f4;

	bool anything = FALSE;
	bool charge = FALSE;
	bool detail = FALSE;
	bool powers = FALSE;

	s16b book[26];

	int num;

	int time = 0;
	int randtime = 0;

	bool random = (mode == OBJECT_FLAGS_RANDOM) ? TRUE : FALSE;
	bool spoil = (mode == OBJECT_FLAGS_FULL) ? TRUE : FALSE;

	/* Basic stats */
	if ((!random) && (o_ptr->tval < TV_SERVICE)) for (i = 0; object_group_tval[i]; i++)
	{
		if (o_ptr->tval == object_group_tval[i])
		{
			anything = TRUE;

			if (o_ptr->number == 1)
			{
				text_out("This ");

				/* Hack -- sets of plural items */
				if (object_group_text[i][strlen(object_group_text[i])-1] == 's')
					text_out("set of ");

				/* 'Basic' object */
				text_out(object_group_text[i]);
			}
			else
			{
				text_out("These ");

				/* Hack -- sets of plural items */
				if (object_group_text[i][strlen(object_group_text[i])-1] == 's')
				{
					text_out("sets of ");

					/* 'Basic' object */
					text_out(object_group_text[i]);
				}
				else
				{
					/* 'Basic' object */
					text_out(object_group_text[i]);

					text_out("s");
				}
			}

			if (o_ptr->number == 1) text_out(" weighs ");
			else text_out(" weigh ");

			/* Weight */
			text_out(format("%d.%d lbs",o_ptr->weight/10,o_ptr->weight%10));

			if (o_ptr->number == 1) text_out(".  ");
			else text_out(" each.  ");

		}

	}

	/* Basic abilities -- tool use */
	if (!random)
	{
		cptr build_bridge = "You can build a bridge across a chasm with this ";

		switch(o_ptr->tval)
		{
			case TV_DIGGING:
				text_out("You can dig pits with this to trap monsters.  ");
				anything = TRUE;
				/* Fall through */
			case TV_SWORD:
			case TV_HAFTED:
			case TV_POLEARM:
				if (cp_ptr)
				{ 
					if (o_ptr->weight >= 2 * cp_ptr->chg_weight)
					{
						text_out(format("It does x%d damage when charging.  ", o_ptr->weight / cp_ptr->chg_weight));
						anything = TRUE;
					}
		        } 
		        else  
				{
					if (o_ptr->weight >= 66)
					{
						text_out(format("It does x%d damage when charging.  ", o_ptr->weight / 33));
						anything = TRUE;
					}
				}
				break;
			case TV_SPIKE:
				text_out("You can spike doors shut with this.  ");
				text_out(format("%sand rope or chain.  ", build_bridge));
				anything = TRUE;
				break;
			case TV_ROPE:
				text_out(format("%sand spikes or grapples.  ", build_bridge));
				anything = TRUE;
				break;
			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
				if (o_ptr->sval == SV_AMMO_GRAPPLE)
				{
					text_out(format("%sand rope", build_bridge));
					if (o_ptr->tval == TV_BOLT) text_out(" or chain");
					text_out(".  ");
					anything = TRUE;
				}
				break;
			case TV_RUNESTONE:
				text_out("You can apply it to different objects to change them or enchant them with additional powers.  ");
				/* Fall through */
			case TV_MAGIC_BOOK:
			case TV_PRAYER_BOOK:
			case TV_SONG_BOOK:
				text_out("You can study this to learn new spells.  ");
				text_out("You can cast spells from this that you have learnt.  ");
				text_out("You can sell this to a shopkeeper for them to offer additional services.  ");
				anything = TRUE;
				break;
			case TV_STUDY:
				text_out("You can study this to research a new field of magic.  ");
				anything = TRUE;
				break;
			case TV_LITE:
				text_out("You can wield this as a source of light.  ");
				anything = TRUE;
				break;
			case TV_STATUE:
				text_out("You're no art critic, but this might be worth something to sell.  ");
				anything = TRUE;
				break;
			case TV_ASSEMBLY:
				text_out("You can assemble this together to make something.  ");
				anything = TRUE;
				break;
			case TV_MAP:
			  {
			    char str[46];

			    long_level_name(str, o_ptr->sval, 0);

			    text_out("You can travel to ");
			    text_out(str);
			    text_out(format(" (%d", min_depth(o_ptr->sval)));
			    if (max_depth(o_ptr->sval) > min_depth(o_ptr->sval)) text_out(format("-%d)",max_depth(o_ptr->sval)));
			    text_out(" with this.  ");
			    anything = TRUE;
			    break;
			  }
			case TV_BAG:
				text_out("You can carry numerous objects inside it.  ");
				anything = TRUE;
				break;
		}
	}

	/* Extract the flags */
	object_flags_aux(mode, o_ptr, &f1, &f2, &f3, &f4);

	/* Display the flags */
	anything |= list_object_flags(f1, f2, f3, f4, spoil || (o_ptr->ident & (IDENT_PVAL | IDENT_MENTAL | IDENT_KNOWN)) ? o_ptr->pval : 0, LIST_FLAGS_CAN); 

	/*
	 * Handle cursed objects here to avoid redundancies such as noting
	 * that a permanently cursed object is heavily cursed as well as
	 * being "lightly cursed".
	 */
	if (!random && cursed_p(o_ptr))
	{
		if (f3 & (TR3_PERMA_CURSE))
		{
			text_out_c(TERM_RED, "It is permanently cursed.  ");
			anything = TRUE;
		}
		else if (f3 & (TR3_HEAVY_CURSE))
		{
			text_out_c(TERM_RED, "It is heavily cursed.  ");
			anything = TRUE;
		}
		else if (object_known_p(o_ptr) || (o_ptr->feeling == INSCRIP_CURSED))
		{
			text_out_c(TERM_RED, "It is cursed.  ");
			anything = TRUE;
		}
	}

	/* Basic abilities -- damage/ damage multiplier */
	if (!random && o_ptr->dd && o_ptr->ds)
	{
		bool throw = TRUE;

		/* Handle melee & throwing weapon damage. Ammunition handled later. */
		switch (o_ptr->tval)
		{
			case TV_SWORD:
			case TV_STAFF:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_DIGGING:
				text_out(format("When attacking or %sthrown, it ", (f3 & TR3_THROWING) ? "easily " : ""));
				break;
			case TV_LITE:
			case TV_POTION:
			case TV_FLASK:
				/* Hack -- display throwing damage later */
				throw = FALSE;
				break;
			case TV_SPELL:
				/* Never thrown */
				throw = FALSE;
				break;
			default:
				text_out(format("When %sthrown, it ", (f3 & TR3_THROWING) ? "easily " : ""));
				break;
		}

		if (throw)
		{
			text_out(format("does %dd%d", o_ptr->dd, o_ptr->ds));
			if (object_bonus_p(o_ptr) || spoil)
			{
				if (o_ptr->to_d > 0) text_out(format("+%d", o_ptr->to_d));
				else if (o_ptr->to_d < 0) text_out(format("%d", o_ptr->to_d));
			}
			text_out(" ");
			text_out((o_ptr->tval == TV_SPELL) ? "magical" : ((o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM ||
						o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT)
						? "edged" : "blunt"));
			text_out(" damage.  ");
			anything = TRUE;
		}
	}

	/* Bows */
	if (!random && (o_ptr->tval == TV_BOW))
	{
		int mult = bow_multiplier (o_ptr->sval);

		text_out("When shooting or set in a trap, it multiplies the base damage of ");

		/* Affect Might */
		if (f1 & (TR1_MIGHT)) mult += o_ptr->pval;

		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			/* Sling and ammo */
			case SV_SLING:
			{
				/* Hack -- slings now act like 'throwers' */
				text_out("shots or thrown items");
				break;
			}

			/* Short Bow and Arrow */
			case SV_SHORT_BOW:
			case SV_LONG_BOW:
			{
				text_out("arrows");
				break;
			}

			/* Light Crossbow and Bolt */
			case SV_HAND_XBOW:
			case SV_LIGHT_XBOW:
			case SV_HEAVY_XBOW:
			{
				text_out("bolts");
				break;
			}
		}
		text_out(format(" by %d.  ", mult));
	}

	/* Extra powers */
	if (!random && ((object_aware_p(o_ptr)) || (spoil) || (o_ptr->ident & (IDENT_STORE)))
		&& (o_ptr->tval !=TV_MAGIC_BOOK) && (o_ptr->tval != TV_PRAYER_BOOK)
		&& (o_ptr->tval !=TV_SONG_BOOK) && (o_ptr->tval != TV_STUDY))
	{
		int vn = 0;
		cptr vp[128];
		int vt[128];
		bool vd[128];
		bool fired = FALSE;

		cptr vp_set_trap = "When set in a trap, it ";
		cptr vp_throw = "When thrown, it ";
		cptr vp_coating = "When applied to coat an arrow, bolt, sword or polearm, it ";
		cptr vp_activate = "When activated, it ";
		cptr vp_activate_throw = "When inscribed with {=A} and attacking or thrown against an opponent, it ";
		cptr vp_activate_attack = "When inscribed with {=A} and attacking an opponent, it ";
		cptr vp_player_eat = "When eaten, it ";
		cptr vp_monster_eat = "When eaten by monsters, it ";

		vn = 0;

		/* Detailled explaination */
		detail = (k_info[o_ptr->k_idx].used > 4 * k_info[o_ptr->k_idx].level * num) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));

		/* Activates */
		if (f3 & TR3_ACTIVATE)
		{
			vp[vn] = vp_activate; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
			charge = (k_info[o_ptr->k_idx].used > o_ptr->charges) || (o_ptr->ident & (IDENT_MENTAL)) || (spoil);
			time = 0;
			randtime = o_ptr->charges;

			switch (o_ptr->tval)
			{
				case TV_STAFF:
				case TV_SWORD:
				case TV_POLEARM:
				case TV_HAFTED:
				case TV_DIGGING:
				{
					if (f3 & (TR3_THROWING)) vp[vn] = vp_activate_throw;
					else vp[vn] = vp_activate_attack;
					vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED; break;
				}
			}
		}

		/* Other attacks based on type */
		switch(o_ptr->tval)
		{
			case TV_SCROLL:
				vp[vn] = "When read, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_BODY:
			case TV_BONE:
				vp[vn] = vp_player_eat; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_SELF;
				vp[vn] = vp_monster_eat; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_FOOD:
				vp[vn] = vp_player_eat; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_SELF;
				vp[vn] = vp_monster_eat; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				if (o_ptr->sval < SV_FOOD_MIN_FOOD)
				{
					vp[vn] = vp_coating; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_COATED;
					vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				}
				break;

			case TV_ROD:
				vp[vn] = "When zapped, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				charge = (k_info[o_ptr->k_idx].used > o_ptr->charges) || (o_ptr->ident & (IDENT_MENTAL)) || (spoil);
				break;

			case TV_STAFF:
				vp[vn] = "When used, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_WAND:
				vp[vn] = "When aimed, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_BOLT:
				vp[vn] = "When fired from a crossbow, it "; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				vp[vn] = vp_set_trap; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				fired = TRUE;
				break;

			case TV_ARROW:
				vp[vn] = "When fired from a bow, it "; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				vp[vn] = vp_set_trap; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				fired = TRUE;
				break;

			case TV_SHOT:
				vp[vn] = "When fired from a sling, it "; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				vp[vn] = vp_set_trap; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				fired = TRUE;
				break;

			case TV_SERVICE:
				vp[vn] = "When purchased, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_NORMAL;
				break;

			case TV_POTION:
				vp[vn] = "When quaffed, it "; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_SELF;
				/* Fall through */
			case TV_FLASK:
				if ((o_ptr->tval == TV_FLASK) && (o_ptr->sval == SV_FLASK_BLOOD))
				{
					vp[vn] = vp_player_eat; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_SELF;					
				}
				vp[vn] = vp_coating; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_COATED;
				vp[vn] = vp_throw; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_EXPLODE;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_LITE:
				vp[vn] = vp_throw; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_SWORD:
			case TV_POLEARM:
			case TV_HAFTED:
				vp[vn] = vp_set_trap; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				break;

			case TV_DRAG_ARMOR:
				vp[vn] = vp_set_trap; vd[vn] = FALSE; vt[vn++] = SPELL_TARGET_AIMED;
				charge = (k_info[o_ptr->k_idx].used > o_ptr->charges) || (o_ptr->ident & (IDENT_MENTAL)) || (spoil);
				break;

			case TV_RUNESTONE:
				vp[vn] = vp_set_trap; vd[vn] = TRUE; vt[vn++] = SPELL_TARGET_AIMED;
				break;
		}

		/* Artifacts */
		if ((o_ptr->name1) && ((object_known_p(o_ptr)) || (spoil)) && (f3 & TR3_ACTIVATE))
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			charge = (a_ptr->activated > a_ptr->randtime) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));
			detail = (a_ptr->activated > 8 * a_ptr->level) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));
			time = a_ptr->time;
			randtime = a_ptr->randtime;
		}

		if (vn)
		{
			/* Scan */
			for (n = 0; n < vn; n++)
			{
				/* Reset powers count */
				powers = FALSE;

				/* Display damage if required */
				if (vd[n])
				{
					text_out(vp[n]);
					text_out(format("does %dd%d", o_ptr->dd, o_ptr->ds));
					if (object_bonus_p(o_ptr) || spoil)
					{
						if (o_ptr->to_d > 0) text_out(format("+%d", o_ptr->to_d));
						else if (o_ptr->to_d < 0) text_out(format("%d", o_ptr->to_d));
					}
					text_out(" ");
					text_out((o_ptr->tval == TV_SPELL) ? "magical" : ((o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM ||
							o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT)
							? "edged" : "blunt"));
					text_out(" damage");
					if (vp[n] != vp_throw)
					{
						switch(o_ptr->tval)
						{
							case TV_BOLT:
								text_out(" times the crossbow multiplier");
								break;
		
							case TV_ARROW:
								text_out(" times the bow multiplier");
								break;
		
							case TV_SHOT:
								text_out(" times the sling multiplier");
								break;
						}
					}
					anything = TRUE;
					powers = TRUE;
				}

				/* Hack -- food feeds player */
				if ((vp[n] == vp_player_eat) && (o_ptr->charges))
				{
					text_out(vp[n]);
					text_out(k_info[o_ptr->k_idx].used >= 10 ? format("provides %d units of nourishment", o_ptr->charges * o_ptr->weight) : "provides nourishment");
					switch (o_ptr->tval)
					{
						case TV_FOOD:
							break;
						case TV_EGG:
							text_out(" to hungry players");
							break;
						default:
							text_out(" to starving players");
							break;
					}
					vd[n] = TRUE;
					anything = TRUE;
					powers = TRUE;
				}

				/* Hack -- monster food recovers monsters */
				if (vp[n] == vp_monster_eat)
				{
					text_out(vp[n]);
					text_out("helps them recover");
					vd[n] = TRUE;
					anything = TRUE;
					powers = TRUE;
				}

				/* Fill the book with spells */
				fill_book(o_ptr,book,&num);

				/* Hack -- fill book with artifact activation */
				if ((o_ptr->name1) && ((object_known_p(o_ptr)) || (spoil)) && (f3 & TR3_ACTIVATE))
				{
					book[0] = a_info[o_ptr->name1].activation;
					num = 1;
				}

				/* Display powers */
				for (i = 0; i < num; i++)
				{
					/* List powers */
					powers |= spell_desc(&s_info[book[i]],(i==0) ? (vd[n] ? " and ": vp[n]) : ", or ",0,detail, vt[n]);
				}

				if ((charge) && (powers))
				{
					if ((time) && (randtime)) text_out(format(", recharging in d%d+%d turns.  ",randtime, time));
					else if (randtime) text_out(format(", recharging in d%d turns.  ",randtime));
					else if (time) text_out(format(", recharging in %d turns.  ",time));
				}
				else if (powers) text_out(".  ");

				anything |= powers;
			}
		}
	}

	/* Basic abilities -- armor class */
	if (!random)
	{
		int armor = o_ptr->ac;

		if (object_bonus_p(o_ptr) || spoil) armor += o_ptr->to_a;

		if (armor)
		{
			text_out(format("It %s your armor class by %d.  ", armor > 0 ? "increases" : "decreases" , armor > 0 ? armor : -armor));

			anything = TRUE;
		}
	}

	/* Have sensed something about this item */
	if ((o_ptr->feeling) || (object_known_p(o_ptr)))
	{
		int feeling = o_ptr->feeling;
		if (!feeling)
		{
			feeling = value_check_aux1(o_ptr);
			
			/* Hack -- exclude 'average' feelings for known items */
			if ((feeling == INSCRIP_AVERAGE) && (object_known_p(o_ptr))) feeling = 0;
		}
		
		if (feeling)
		{
			if (feeling <= INSCRIP_COATED)
			{
				text_out(inscrip_info[feeling]);
				anything = TRUE;
			}
			else if (feeling >= MAX_INSCRIP)
			{
				int bag = lookup_kind(TV_BAG, feeling - MAX_INSCRIP);

				if (bag)
				{
					text_out(format("You sense it belongs with %s.  ",k_name + k_info[bag].name));
					anything = TRUE;
				}
			}
			else
			{
				text_out(inscrip_info[INSCRIP_COATED + 1]);
			}
		}
	}

	/* Can enchant this further? */
	if (!spoil && !random && object_named_p(o_ptr) && !(o_ptr->xtra1) && !artifact_p(o_ptr) && (o_ptr->tval != TV_SERVICE) )
	{
		text_out_c(TERM_VIOLET,"\nYou can apply runes to it or enchant it with additional powers.  ");
		anything = TRUE;
	}

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (!spoil && !random && object_named_p(o_ptr) && (((o_ptr->xtra1) && (o_ptr->name2)) || artifact_p(o_ptr)) )
	{
		bool hidden = TRUE;
	
		if (o_ptr->ident & (IDENT_MENTAL)) hidden = FALSE;
	    
		if (hidden)
		{
			text_out_c(TERM_VIOLET,"\nIt may have undiscovered powers.  ");
			anything = TRUE;
		}
	}

	if (!random && !spoil)
	{
		/* Display the flags */
		anything |= list_object_flags(o_ptr->may_flags1, o_ptr->may_flags2, o_ptr->may_flags3, o_ptr->may_flags4, o_ptr->ident & (IDENT_PVAL | IDENT_MENTAL | IDENT_KNOWN) ? o_ptr->pval : 0, LIST_FLAGS_MAY); 

#if 0
                /* Equipment only */
                if (wield_slot(o_ptr) >= INVEN_WIELD)
                        /* Display the flags */
                        anything |= list_object_flags(o_ptr->not_flags1, o_ptr->not_flags2, o_ptr->not_flags3, o_ptr->not_flags4, o_ptr->ident & (IDENT_PVAL | IDENT_MENTAL | IDENT_KNOWN) ? o_ptr->pval : 0, LIST_FLAGS_NOT);
#endif
	}

	/* Display bag contents */
	if (o_ptr->tval == TV_BAG)
	{
		object_type object_type_body;
		object_type *i_ptr = &object_type_body;

		bool intro = FALSE;

		char o_name[120];

		/* Display fake objects */
		for (i = 0; i < INVEN_BAG_TOTAL; i++)
		{
			/* Empty slot */
			if (!(bag_holds[o_ptr->sval][i][0])) continue;

			/* Nothing in slot */
			if (!(bag_contents[o_ptr->sval][i]) && !spoil) continue;

			/* Fake the item */
			fake_bag_item(i_ptr, o_ptr->sval, i);

			/* Intro */
			if (!intro && !random)
			{
				text_out("\nIt contains:\n");
			}

			/* Describe */
			object_desc(o_name, sizeof(o_name), i_ptr, TRUE, 1);

			text_out(format("%s %c) %s\n",random ? (!intro ? "with": "    ") : " ", 'a'+i, o_name));

			intro = TRUE;
		}
	}

	/* *Identified* object */
	if (spoil && anything) text_out("You know everything about this item.  ");

	/* Nothing was printed */
	if (!random && !anything) text_out("You know nothing worth noting about this item.  ");

	/* End */
	if (!random) text_out("\n");
	else if (anything) text_out("\n");
}


/*
 * Print a list of powers (for selection).
 */
void print_powers(const s16b *book, int num, int y, int x)
{
	int i, spell;

	cptr comment;

	char info[80];

	char out_val[160];

	byte line_attr;

	spell_type *s_ptr;

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Get the spell index */
		spell = book[i];

		/* Get the spell info */
		s_ptr = &s_info[spell];

		/* Get extra info */
		spell_info(info, sizeof(info), spell, FALSE);

		/* Use that info */
		comment = info;

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s %s",
			I2A(i), s_name + s_ptr->name,
			comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}


/*
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(const s16b *sn, int num, int y, int x)
{
	int i, ii, spell, level;

	cptr comment;

	char info[80];

	char out_val[160];

	byte line_attr;

	spell_type *s_ptr;

	spell_cast *sc_ptr = NULL;

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Get the spell index */
		spell = sn[i];

		/* Skip missing spells */
		if (!spell)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(missing)");
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* Get the spell info */
		s_ptr = &s_info[spell];

		/* Get the spell details; warriors (class 0) have no spells */
		sc_ptr = spell_cast_details(spell);

		/* Skip illegible spells */
		if ((!spell_legible(spell)) || !(sc_ptr))
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), s_name+s_info[0].name);
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, sizeof(info), spell, TRUE);

		/* Get level */
		level = spell_level(spell);

		/* Use that info */
		comment = info;

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Get the spell knowledge*/
		for (ii=0;ii<PY_MAX_SPELLS;ii++)
		{

			if (p_ptr->spell_order[ii] == spell) break;

		}

		/* Analyze the spell */		
		if (ii==PY_MAX_SPELLS)
		{
			if (level <= p_ptr->lev)
			{
				if (spell_okay(spell, FALSE))
				{
					comment = " unknown";
					line_attr = TERM_L_BLUE;
				}
				else
				{
					comment = " prerequisite";
					line_attr = TERM_VIOLET;
				}
			}
			else
			{
				comment = " difficult";
				line_attr = TERM_RED;
			}
		}
		else if ((ii < 32) ? (p_ptr->spell_forgotten1 & (1L << ii)) :
		      ((ii < 64) ? (p_ptr->spell_forgotten2 & (1L << (ii - 32))) :
		      ((ii < 96) ? (p_ptr->spell_forgotten3 & (1L << (ii - 64))) :
		      (p_ptr->spell_forgotten4 & (1L << (ii - 96))))))
		{
			comment = " forgotten";
			line_attr = TERM_YELLOW;
		}
		else if (!((ii < 32) ? (p_ptr->spell_learned1 & (1L << ii)) :
		      ((ii < 64) ? (p_ptr->spell_learned2 & (1L << (ii - 32))) :
		      ((ii < 96) ? (p_ptr->spell_learned3 & (1L << (ii - 64))) :
		      (p_ptr->spell_learned4 & (1L << (ii - 96)))))))
		{
			if (level <= p_ptr->lev)
			{
				comment = " unknown";
				line_attr = TERM_L_BLUE;
			}
			else
			{
				comment = " difficult";
				line_attr = TERM_RED;
			}
		}
		else if (!((ii < 32) ? (p_ptr->spell_worked1 & (1L << ii)) :
		      ((ii < 64) ? (p_ptr->spell_worked2 & (1L << (ii - 32))) :
		      ((ii < 96) ? (p_ptr->spell_worked3 & (1L << (ii - 64))) :
		      (p_ptr->spell_worked4 & (1L << (ii - 96)))))))
		{
			comment = " untried";
			line_attr = TERM_L_GREEN;
		}

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-30s%2d %4d %3d%%%s",
			I2A(i), s_name + s_ptr->name,
			level, sc_ptr->mana, spell_chance(spell), comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}


	/* Clear the bottom line */
	prt("", y + i + 1, x);
}

/*
 * Hack -- Create a "forged" artifact
 */
bool make_fake_artifact(object_type *o_ptr, byte name1)
{
	int i;

	artifact_type *a_ptr = &a_info[name1];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return FALSE;

	/* Get the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->name1 = name1;

	/* Extract the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;

	if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

	/* Success */
	return (TRUE);
}


/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(const object_type *o_ptr)
{
	int y;

	int i,ii,iii;

	char o_name[80];

	bool browse;

	spell_type *s_ptr;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}


	/* No info */
	if (!o_ptr->k_idx) return;

	/* Fully describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE,1);

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	text_out("\n");

	/* Actually display the item */
	list_object(o_ptr, OBJECT_FLAGS_KNOWN);

	/* Display monster attributes */
	if ((o_ptr->name3) && ((o_ptr->tval != TV_HOLD) || (object_named_p(o_ptr)))) screen_roff(&r_info[o_ptr->name3], &l_list[o_ptr->name3]);

	/* Display item name */
	obj_top(o_ptr);

	/* Warriors are illiterate */
	if ((c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL)
		&& (p_ptr->pstyle != WS_MAGIC_BOOK) && (p_ptr->pstyle != WS_PRAYER_BOOK) && (p_ptr->pstyle != WS_SONG_BOOK)) return;

	browse = FALSE;

	for (i=0;i<z_info->s_max;i++)
	{
		s_ptr=&s_info[i];

		for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
		{
			if ((s_ptr->appears[ii].tval == o_ptr->tval) &&
			    (s_ptr->appears[ii].sval == o_ptr->sval))
			{
			  /* Warriors (class 0) have no spells */
			  if (p_ptr->pclass)
			    for (iii = 0; iii < MAX_SPELL_CASTERS; iii++)
			      {
				if (s_ptr->cast[iii].class == p_ptr->pclass) browse=TRUE;
			      }
			}
		}

	}

	if (browse)
	{
		s16b book[26];

		int num;

		/* Fill the book with spells */
		fill_book(o_ptr, book,&num);

		/* Print spells */
		print_spells(book, num, 2, 0);

	}

}


/*
 * Attempt to guess an item name or sval
 */
void object_guess_name(object_type *o_ptr)
{
	int i,ii;

	int score;
	int high = 0;

	byte guess1=0;
	byte guess2=0;
	byte guess3=0;
	
	u32b f1 = o_ptr->can_flags1;
	u32b f2 = o_ptr->can_flags2;
	u32b f3 = o_ptr->can_flags3;
	u32b f4 = o_ptr->can_flags4;
	
	/* Do not guess identified items */
	if (object_named_p(o_ptr)) return;

	/* Remove flags on aware objects */
	if (k_info[o_ptr->k_idx].aware)
	{
		f1 &= ~(k_info[o_ptr->k_idx].flags1);
		f2 &= ~(k_info[o_ptr->k_idx].flags2);
		f3 &= ~(k_info[o_ptr->k_idx].flags3);
		f4 &= ~(k_info[o_ptr->k_idx].flags4);
	}

	/* Hack -- remove throwing flag */
	f3 &= ~(TR3_THROWING);

	/* No properties beyond base kind */
	if (!f1 && !f2 && !f3 && !f4) return;

	/* Check the ego item list */
	/* Hack -- exclude artifacts and potions */
	if ((o_ptr->feeling != INSCRIP_SPECIAL) &&
	       (o_ptr->feeling != INSCRIP_ARTIFACT) &&
	       (o_ptr->feeling != INSCRIP_TERRIBLE) &&
	       (o_ptr->feeling != INSCRIP_UNBREAKABLE))
	for (i = 1; i < z_info->e_max; i++)
	{
		ego_item_type *e_ptr = &e_info[i];
		object_lore *n_ptr = &e_list[i];

		bool legal;

		legal = FALSE;

		/* Skip "empty" items */
		if (!e_ptr->name) continue;

		/* Test if this is a legal ego-item type for this object */
		for (ii = 0; ii < 3; ii++)
		{
			/* Require identical base type */
			if ((o_ptr->tval == e_ptr->tval[ii])
				&& (o_ptr->sval > e_ptr->min_sval[ii])
				&& (o_ptr->sval < e_ptr->max_sval[ii]))
			{
				legal = TRUE;
			}
		}

		/* Legal ego item */
		if (!legal) continue;

		/* Must possess powers */
		if (o_ptr->not_flags1 & n_ptr->can_flags1) continue;
		if (o_ptr->not_flags2 & n_ptr->can_flags2) continue;
		if (o_ptr->not_flags3 & n_ptr->can_flags3) continue;
		if (o_ptr->not_flags4 & n_ptr->can_flags4) continue;

		/* Must not have excepted powers */
		if (f1 & n_ptr->not_flags1) continue;
		if (f2 & n_ptr->not_flags2) continue;
		if (f3 & n_ptr->not_flags3) continue;
		if (f4 & n_ptr->not_flags4) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((f1 & (1L<<ii)) && (e_ptr->flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (e_ptr->flags1 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((f2 & (1L<<ii)) && (e_ptr->flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (e_ptr->flags2 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			/* Hack -- don't match on curse flags */
			if ((1L << ii) >= TR3_LIGHT_CURSE) continue;

			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((f3 & (1L<<ii)) && (e_ptr->flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (e_ptr->flags3 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((f4 & (1L<<ii)) && (e_ptr->flags4 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags4 & (1L<<ii)) && (e_ptr->flags4 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f4 & (1L<<ii)) && (n_ptr->can_flags4 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags4 & (1L<<ii)) && (n_ptr->can_flags4 & (1L<<ii))) score +=1;
			}
		}

		/* Do we have a match? */
		if (score > high)
		{
			high = score;
			guess2 = i;
		}
		/* Hack -- force lowest depth items */
		else if ((score) && (score == high) && (guess2))
		{
			ego_item_type *e2_ptr = &e_info[guess2];

			if (e_ptr->level < e2_ptr->level) guess2 = i;
		}
	}

	/* This should be here to guess for rings/amulets etc. */

	/* Check the normal item list */
	/* Hack -- exclude artifacts and potions */
	if (o_ptr->tval != TV_POTION
	    && o_ptr->feeling != INSCRIP_SPECIAL
	    && o_ptr->feeling != INSCRIP_ARTIFACT
	    && o_ptr->feeling != INSCRIP_TERRIBLE
	    && o_ptr->feeling != INSCRIP_UNBREAKABLE)
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" items */
		if (!k_ptr->name) continue;

		/* Must not already be aware? */
		if (k_ptr->aware) continue;

		/* Must be flavored */
		if (!k_ptr->flavor) continue;

		/* Must be the same tval */
		if (k_ptr->tval != o_ptr->tval) continue;

		/* Must possess powers */
		if (o_ptr->not_flags1 & k_ptr->flags1) continue;
		if (o_ptr->not_flags2 & k_ptr->flags2) continue;
		if (o_ptr->not_flags3 & k_ptr->flags3) continue;
		if (o_ptr->not_flags4 & k_ptr->flags4) continue;

		/* Must not have excepted powers */
		if (f1 & ~(k_ptr->flags1)) continue;
		if (f2 & ~(k_ptr->flags2)) continue;
		if (f3 & ~(k_ptr->flags3)) continue;
		if (f4 & ~(k_ptr->flags4)) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((f1 & (1L<<ii)) && (k_ptr->flags1 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags1 & (1L<<ii)) && (k_ptr->flags1 & (1L<<ii))) score +=1;
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((f2 & (1L<<ii)) && (k_ptr->flags2 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags2 & (1L<<ii)) && (k_ptr->flags2 & (1L<<ii))) score +=1;
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			/* Hack -- don't match on curse flags */
			if ((1L << ii) >= TR3_LIGHT_CURSE) continue;

			if ((f3 & (1L<<ii)) && (k_ptr->flags3 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags3 & (1L<<ii)) && (k_ptr->flags3 & (1L<<ii))) score +=1;
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((f4 & (1L<<ii)) && (k_ptr->flags4 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags4 & (1L<<ii)) && (k_ptr->flags4 & (1L<<ii))) score +=1;
		}


		/* Do we have a match? */
		if (score > high)
		{
			high = score;
			guess3 = k_ptr->sval+1;
		}
		/* Hack -- force lowest depth items */
		else if ((score) && (score == high) && (guess3))
		{
			object_kind *k2_ptr = &k_info[lookup_kind(o_ptr->tval,guess3-1)];

			if (k_ptr->level < k2_ptr->level) guess3 = k_ptr->sval+1;
		}

	}

	/* Check the artifact list */
	/* Hack -- exclude ego items */
	if ((o_ptr->feeling != INSCRIP_EXCELLENT) &&
	       (o_ptr->feeling != INSCRIP_SUPERB) &&
	       (o_ptr->feeling != INSCRIP_HIGH_EGO_ITEM) &&
	       (o_ptr->feeling != INSCRIP_EGO_ITEM) &&
	       (o_ptr->feeling != INSCRIP_WORTHLESS))
		for (i = 1; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		object_info *n_ptr = &a_list[i];

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* Must have the correct fields */
		if (a_ptr->tval != o_ptr->tval) continue;

		/* Must have the correct sval or be a special artifact */
		if ((i<ART_MIN_NORMAL) || (a_ptr->sval != o_ptr->sval)) continue;

		/* Must possess powers */
		if (o_ptr->not_flags1 & n_ptr->can_flags1) continue;
		if (o_ptr->not_flags2 & n_ptr->can_flags2) continue;
		if (o_ptr->not_flags3 & n_ptr->can_flags3) continue;
		if (o_ptr->not_flags4 & n_ptr->can_flags4) continue;

		/* Must not have excepted powers */
		if (f1 & n_ptr->not_flags1) continue;
		if (f2 & n_ptr->not_flags2) continue;
		if (f3 & n_ptr->not_flags3) continue;
		if (f4 & n_ptr->not_flags4) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((f1 & (1L<<ii)) && (a_ptr->flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (a_ptr->flags1 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((f2 & (1L<<ii)) && (a_ptr->flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (a_ptr->flags2 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=1;
			}					      
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((f3 & (1L<<ii)) && (a_ptr->flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (a_ptr->flags3 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((f4 & (1L<<ii)) && (a_ptr->flags4 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags4 & (1L<<ii)) && (a_ptr->flags4 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((f4 & (1L<<ii)) && (n_ptr->can_flags4 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags4 & (1L<<ii)) && (n_ptr->can_flags4 & (1L<<ii))) score +=1;
			}
		}

		/* Artifacts have a minimum match threshold */
		if ((score > 6) && (score > high))
		{
			high = score;
			guess1 = i;
			guess2 = 0;
		}
		/* Hack -- force lowest depth items */
		else if ((score > 6) && (score == high) && (guess1))
		{
			artifact_type *a2_ptr = &a_info[guess1];

			if (a_ptr->level < a2_ptr->level) guess1 = i;
		}

	}

	/* Guessed an artifact? */
	if ((guess1) && (guess1 != o_ptr->guess1))
	{
		char o_name[80];

		/* Guess an artifact */
		o_ptr->guess1 = guess1;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
		
	}

	/* Guessed an ego item */
	else if ((guess2) && (guess2 != o_ptr->guess2))
	{
		char o_name[80];

		/* Guess an artifact */
		o_ptr->guess2 = guess2;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Guessed a kind */
	else if ((guess3) && (guess3 != k_info[o_ptr->k_idx].guess))
	{
		char o_name[80];

		/* Guess an artifact */
		k_info[o_ptr->k_idx].guess = guess3;

		/* Guess is tried */
		k_info[o_ptr->k_idx].tried = TRUE;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);	
	}
}


/*
 * Object does have flags.
 *
 * XXX The function object_can_flags assumes that the object we notice has a particular
 * set of flags is either wielded or in the inventory, and therefore we clear the 
 * 'may_flags' that we use to track which objects have information with which we may
 * be able to deduce what flags that object has, and which do not.
 *
 * However, for instance, objects that are thrown from the floor, or are damaged by fire
 * but ignore it, or are damaging to a particular type of monster which fails to pick it
 * up, are some of the instances where we call this routine for objects other than those
 * held in the inventory.
 *
 * Currently this is fine as the flags are not those kind we attempt to track anyway eg
 * weapon flags, ignore flags and so on.
 *
 * XXX We might want to return TRUE from this routine if the flags we are adding are not
 * already known.
 *
 */
void object_can_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3, u32b f4, bool floor)
{
	u32b xf1 = 0, xf2 = 0, xf3 = 0, xf4 = 0;
	int i;

	u32b if1 = o_ptr->may_flags1 & (f1);
	u32b if2 = o_ptr->may_flags2 & (f2);
	u32b if3 = o_ptr->may_flags3 & (f3);
	u32b if4 = o_ptr->may_flags4 & (f4);

	/* Clear not flags */
	o_ptr->not_flags1 &= ~(f1);
	o_ptr->not_flags2 &= ~(f2);
	o_ptr->not_flags3 &= ~(f3);
	o_ptr->not_flags4 &= ~(f4);

	/* Clear may flags on all kit - include inventory */
	if (!floor) for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear may flags */
		i_ptr->may_flags1 &= ~(if1);
		i_ptr->may_flags2 &= ~(if2);
		i_ptr->may_flags3 &= ~(if3);
		i_ptr->may_flags4 &= ~(if4);
	}

	/* Mark can flags */
	o_ptr->can_flags1 |= (f1);
	o_ptr->can_flags2 |= (f2);
	o_ptr->can_flags3 |= (f3);
	o_ptr->can_flags4 |= (f4);

	/* If object flavored, learn flags about that flavor */
	if (!object_aware_p(o_ptr) && (k_info[o_ptr->k_idx].flavor))
	{
		/* Learn for base flavor */
		x_list[k_info[o_ptr->k_idx].flavor].can_flags1 |= (f1);
		x_list[k_info[o_ptr->k_idx].flavor].can_flags2 |= (f2);
		x_list[k_info[o_ptr->k_idx].flavor].can_flags3 |= (f3);
		x_list[k_info[o_ptr->k_idx].flavor].can_flags4 |= (f4);

		/* Process inventory */
		for (i = 0; i < INVEN_TOTAL; i++)
		{
			object_type *i_ptr = &inventory[i];

			/* Skip non-objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching kind */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			i_ptr->can_flags1 |= (f1);
			i_ptr->can_flags2 |= (f2);
			i_ptr->can_flags3 |= (f3);
			i_ptr->can_flags4 |= (f4);

			o_ptr->can_flags1 |= i_ptr->can_flags1;
			o_ptr->can_flags2 |= i_ptr->can_flags2;
			o_ptr->can_flags3 |= i_ptr->can_flags3;
			o_ptr->can_flags4 |= i_ptr->can_flags4;	

			/* Guess name */
			object_guess_name(i_ptr);
		}

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching kind */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			i_ptr->can_flags1 |= (f1);
			i_ptr->can_flags2 |= (f2);
			i_ptr->can_flags3 |= (f3);
			i_ptr->can_flags4 |= (f4);

			o_ptr->can_flags1 |= i_ptr->can_flags1;
			o_ptr->can_flags2 |= i_ptr->can_flags2;
			o_ptr->can_flags3 |= i_ptr->can_flags3;
			o_ptr->can_flags4 |= i_ptr->can_flags4;

			/* Guess name */
			object_guess_name(i_ptr);
		}

		/* Guess name */
		object_guess_name(o_ptr);
	}

	/* Must be identified to continue */
	if (!object_named_p(o_ptr))
	{
		object_guess_name(o_ptr);

		return;
	}

	/* Hack -- Remove kind flags */
	/* This prevents Blades of Chaos 'tainting' ego items etc. */
	f1 &= ~(k_info[o_ptr->k_idx].flags1);
	f2 &= ~(k_info[o_ptr->k_idx].flags2);
	f3 &= ~(k_info[o_ptr->k_idx].flags3);
	f4 &= ~(k_info[o_ptr->k_idx].flags4);

	/* Hack -- Remove 'user' enchanted hidden flags */
	/* This prevents runes and enchantment spells 'tainting' ego items */

	/* Ignore coatings */
	if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_COATS)
	{
		/* No extra powers */
	}
	else if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_RUNES)
	{
		int rune = o_ptr->xtra1 - OBJECT_XTRA_MIN_RUNES;
		int i;

		for (i = 0;i<MAX_RUNE_FLAGS;i++)
		{
			if ((y_info[rune].count[i]) && (y_info[rune].count[i]<= o_ptr->xtra2))
			{
				if (y_info[rune].flag[i] < 32) (f1) &= ~(1L << y_info[rune].flag[i]);
			
				if ((y_info[rune].flag[i] >= 32)
				 && (y_info[rune].flag[i] < 64)) (f2) &= ~(1L << (y_info[rune].flag[i]-32));

				if ((y_info[rune].flag[i] >= 64)
				 && (y_info[rune].flag[i] < 96)) (f3) &= ~(1L << (y_info[rune].flag[i]-64));

				if ((y_info[rune].flag[i] >= 96)
				 && (y_info[rune].flag[i] < 128)) (f4) &= ~(1L << (y_info[rune].flag[i]-96));
			}
		}
	}
	/* Extra powers */
	else
	{
		if (object_xtra_what[o_ptr->xtra1] == 1)
		{
			xf1 = (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			f1 &= ~xf1;
		}
		else if (object_xtra_what[o_ptr->xtra1] == 2)
		{
			xf2 = (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			f2 &= ~xf2;
		}
		else if (object_xtra_what[o_ptr->xtra1] == 3)
		{
			xf3 = (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			f3 &= ~xf3;
		}
		else if (object_xtra_what[o_ptr->xtra1] == 4)
		{
			xf4 = (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
			f4 &= ~xf4;
		}
	}

	/* Artifact */
	if (o_ptr->name1)
	{
		object_info *n_ptr = &a_list[o_ptr->name1];

		n_ptr->not_flags1 &= ~(f1);
		n_ptr->not_flags2 &= ~(f2);
		n_ptr->not_flags3 &= ~(f3);
		n_ptr->not_flags4 &= ~(f4);

		/* Fixed flags */
		n_ptr->can_flags1 |= (f1);
		n_ptr->can_flags2 |= (f2);
		n_ptr->can_flags3 |= (f3);
		n_ptr->can_flags4 |= (f4);
	}

	/* Ego item */
	else if (o_ptr->name2)
	{
		object_lore *n_ptr = &e_list[o_ptr->name2];

		n_ptr->not_flags1 &= ~(f1);
		n_ptr->not_flags2 &= ~(f2);
		n_ptr->not_flags3 &= ~(f3);
		n_ptr->not_flags4 &= ~(f4);

		/* Fixed flags */
		n_ptr->can_flags1 |= (f1);
		n_ptr->can_flags2 |= (f2);
		n_ptr->can_flags3 |= (f3);
		n_ptr->can_flags4 |= (f4);

		/* Extra flags */
		n_ptr->may_flags1 |= xf1;
		n_ptr->may_flags2 |= xf2;
		n_ptr->may_flags3 |= xf3;
		n_ptr->may_flags4 |= xf4;

		/* Exclude fixed flags */
		n_ptr->may_flags1 &= ~(n_ptr->can_flags1);
		n_ptr->may_flags2 &= ~(n_ptr->can_flags2);
		n_ptr->may_flags3 &= ~(n_ptr->can_flags3);
		n_ptr->may_flags4 &= ~(n_ptr->can_flags4);
	}
}

/*
 * Check if inventory (including equipment) has only 1 item that may flags
 */
static void inven_may_flags()
{
	int i, j;

	u32b f1 = 0x0L;
	u32b f2 = 0x0L;
	u32b f3 = 0x0L;
	u32b f4 = 0x0L;

	u32b nf1 = 0x0L;
	u32b nf2 = 0x0L;
	u32b nf3 = 0x0L;
	u32b nf4 = 0x0L;

	object_type *i_ptr;

	/* Check inventory may flags*/
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags1 & (1L<<j))
			{
				if (!(nf1 & (1L<<j))) { nf1 |= (1L<<j); f1 |= (1L<<j); }
				else f1 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags2 & (1L<<j))
			{
				if (!(nf2 & (1L<<j))) { nf2 |= (1L<<j); f2 |= (1L<<j); }
				else f2 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags3 & (1L<<j))
			{
				if (!(nf3 & (1L<<j))) { nf3 |= (1L<<j); f3 |= (1L<<j); }
				else f3 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags4 & (1L<<j))
			{
				if (!(nf4 & (1L<<j))) { nf4 |= (1L<<j); f4 |= (1L<<j); }
				else f4 &= ~(1L<<j);
			}
		}
	}

	/* Check inventory may flags*/
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		if ((f1 & (i_ptr->may_flags1)) || (f2 & (i_ptr->may_flags2))
			|| (f3 & (i_ptr->may_flags3)) || (f4 & (i_ptr->may_flags4)))
			update_slot_flags(i,f1 & (i_ptr->may_flags1),f2 & (i_ptr->may_flags2),f3 & (i_ptr->may_flags3),f4 & (i_ptr->may_flags4));
	}
}

/*
 * Check if equipment has only 1 item that may flags
 */
static void equip_may_flags(u32b f1, u32b f2, u32b f3, u32b f4)
{
	int i, j;

	u32b if1 = 0x0L;
	u32b if2 = 0x0L;
	u32b if3 = 0x0L;
	u32b if4 = 0x0L;

	u32b nf1 = 0x0L;
	u32b nf2 = 0x0L;
	u32b nf3 = 0x0L;
	u32b nf4 = 0x0L;

	object_type *i_ptr;

	/* Check inventory may flags*/
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags1 & (1L<<j))
			{
				if (!(nf1 & (1L<<j))) { nf1 |= (1L<<j); if1 |= (1L<<j); }
				else if1 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags2 & (1L<<j))
			{
				if (!(nf2 & (1L<<j))) { nf2 |= (1L<<j); if2 |= (1L<<j); }
				else if2 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags3 & (1L<<j))
			{
				if (!(nf3 & (1L<<j))) { nf3 |= (1L<<j); if3 |= (1L<<j); }
				else if3 &= ~(1L<<j);
			}
		}

		for (j = 0; j< 32; j++)
		{
			if (i_ptr->may_flags4 & (1L<<j))
			{
				if (!(nf4 & (1L<<j))) { nf4 |= (1L<<j); if4 |= (1L<<j); }
				else if4 &= ~(1L<<j);
			}
		}
	}

	/* Only check passed flags */
	if1 &= (f1);
	if2 &= (f2);
	if3 &= (f3);
	if4 &= (f4);

	if (!(if1) && !(if2) && !(if3) && !(if4)) return;

	/* Check equipment may flags*/
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		if ((if1 & (i_ptr->may_flags1)) || (if2 & (i_ptr->may_flags2))
			|| (if3 & (i_ptr->may_flags3)) || (if4 & (i_ptr->may_flags4)))
			update_slot_flags(i,if1 & (i_ptr->may_flags1),if2 & (i_ptr->may_flags2),if3 & (i_ptr->may_flags3),if4 & (i_ptr->may_flags4));
	}
}


/*
 * Object does not have flags.
 */
void object_not_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3, u32b f4, bool floor)
{
	int i;

	/* No change */
	if (!(f1 & ~(o_ptr->not_flags1)) && !(f2 & ~(o_ptr->not_flags2)) && !(f3 & ~(o_ptr->not_flags3)) && !(f4 & ~(o_ptr->not_flags4))) return;
	
	/* Mark not flags */
	o_ptr->not_flags1 |= (f1);
	o_ptr->not_flags2 |= (f2);
	o_ptr->not_flags3 |= (f3);
	o_ptr->not_flags4 |= (f4);

	/* Clear may flags */
	o_ptr->may_flags1 &= ~(f1);
	o_ptr->may_flags2 &= ~(f2);
	o_ptr->may_flags3 &= ~(f3);
	o_ptr->may_flags4 &= ~(f4);

	/* Oops */
	if ( (f1 & (o_ptr->can_flags1)) || (f2 & (o_ptr->can_flags2)) || (f3 & (o_ptr->can_flags3)) || (f4 & (o_ptr->can_flags4)))
	{
		msg_print("BUG: Forgetting something on an object we shouldn't forget. Please report.");

		/* Set text_out hook */
		text_out_hook = text_out_to_screen;

		/* Load screen */
		screen_save();

		/* Begin recall */
		Term_gotoxy(0, 1);

		/* Actually display the item */
		list_object_flags(f1 & (o_ptr->can_flags1), f2 & (o_ptr->can_flags2), f3 & (o_ptr->can_flags3), f4 & (o_ptr->can_flags4), o_ptr->pval, LIST_FLAGS_CAN);

		(void)anykey();
	
		/* Load screen */
		screen_load();

		msg_format("%ld:%ld:%ld:%ld", f1 & (o_ptr->can_flags1), f2 & (o_ptr->can_flags2), f3 & (o_ptr->can_flags3), f4 & (o_ptr->can_flags4));
	}

	/* Clear can flags */
	o_ptr->can_flags1 &= ~(f1);
	o_ptr->can_flags2 &= ~(f2);
	o_ptr->can_flags3 &= ~(f3);
	o_ptr->can_flags4 &= ~(f4);

	/* If object flavored, learn flags about that flavor */
	if (!object_aware_p(o_ptr) && (k_info[o_ptr->k_idx].flavor))
	{
		x_list[k_info[o_ptr->k_idx].flavor].not_flags1 |= (f1);
		x_list[k_info[o_ptr->k_idx].flavor].not_flags2 |= (f2);
		x_list[k_info[o_ptr->k_idx].flavor].not_flags3 |= (f3);
		x_list[k_info[o_ptr->k_idx].flavor].not_flags4 |= (f4);

		/* Process inventory */
		for (i = 0; i < INVEN_TOTAL; i++)
		{
			object_type *i_ptr = &inventory[i];

			/* Skip non-objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching kind */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			i_ptr->not_flags1 |= (f1);
			i_ptr->not_flags2 |= (f2);
			i_ptr->not_flags3 |= (f3);
			i_ptr->not_flags4 |= (f4);

			/* Important -- have to clear may flags on inventory */
			i_ptr->may_flags1 &= ~(f1);
			i_ptr->may_flags2 &= ~(f2);
			i_ptr->may_flags3 &= ~(f3);
			i_ptr->may_flags4 &= ~(f4);

			/* Guess name */
			object_guess_name(i_ptr);
		}

		/* Process objects */
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *i_ptr = &o_list[i];

			/* Skip dead objects */
			if (!i_ptr->k_idx) continue;

			/* Not matching kind */
			if (i_ptr->k_idx != o_ptr->k_idx) continue;

			i_ptr->not_flags1 |= (f1);
			i_ptr->not_flags2 |= (f2);
			i_ptr->not_flags3 |= (f3);
			i_ptr->not_flags4 |= (f4);

			/* Guess name */
			object_guess_name(i_ptr);
		}
	}

	/* Check inventory */
	if (!floor) inven_may_flags();

	/* Must be identified to continue */
	if (!object_named_p(o_ptr))
	{
		object_guess_name(o_ptr);

		return;
	}

	/* Artifact */
	if (o_ptr->name1) 
	{
		object_info *n_ptr = &a_list[o_ptr->name1];

		n_ptr->not_flags1 |= f1;
		n_ptr->not_flags2 |= f2;
		n_ptr->not_flags3 |= f3;
		n_ptr->not_flags4 |= f4;

		n_ptr->can_flags1 &= ~(f1);
		n_ptr->can_flags2 &= ~(f2);
		n_ptr->can_flags3 &= ~(f3);
		n_ptr->can_flags4 &= ~(f4);
	}

	/* Ego item */
	else if (o_ptr->name2)
	{
		object_lore *n_ptr = &e_list[o_ptr->name2];

		n_ptr->not_flags1 |= f1;
		n_ptr->not_flags2 |= f2;
		n_ptr->not_flags3 |= f3;
		n_ptr->not_flags4 |= f4;

		n_ptr->can_flags1 &= ~(f1);
		n_ptr->can_flags2 &= ~(f2);
		n_ptr->can_flags3 &= ~(f3);
		n_ptr->can_flags4 &= ~(f4);
	}
}

/*
 * Object may have these flags. If only object in equipment
 * to do so, will have these flags. Use for object absorbtion.
 */
void object_may_flags(object_type *o_ptr, u32b f1,u32b f2,u32b f3, u32b f4, bool floor)
{
	/* Important: Object must be in inventory */
	if (floor) return;
	
	/* Clear bits with not flags */
	f1 &= ~(o_ptr->not_flags1);
	f2 &= ~(o_ptr->not_flags2);
	f3 &= ~(o_ptr->not_flags3);
	f4 &= ~(o_ptr->not_flags4);

	/* Clear bits with can flags */
	f1 &= ~(o_ptr->can_flags1);
	f2 &= ~(o_ptr->can_flags2);
	f3 &= ~(o_ptr->can_flags3);
	f4 &= ~(o_ptr->can_flags4);

	/* Mark may flags */
	o_ptr->may_flags1 |= (f1);
	o_ptr->may_flags2 |= (f2);
	o_ptr->may_flags3 |= (f3);
	o_ptr->may_flags4 |= (f4);

	/* Check the inventory */
	inven_may_flags();

	/* Must be identified to continue */
	if (!object_named_p(o_ptr))
	{
		object_guess_name(o_ptr);
	}
}

/*
 * Object forgets all may flags
 */
void drop_may_flags(object_type *o_ptr)
{
	/* Clear may flags */
	o_ptr->may_flags1 = 0L;
	o_ptr->may_flags2 = 0L;
	o_ptr->may_flags3 = 0L;
	o_ptr->may_flags4 = 0L;

	return;
}

/*
 * Object forgets all flags
 */
void drop_all_flags(object_type *o_ptr)
{
	/* Clear may flags */
	o_ptr->can_flags1 = 0L;
	o_ptr->can_flags2 = 0L;
	o_ptr->can_flags3 = 0L;
	o_ptr->can_flags4 = 0L;

	/* Clear may flags */
	o_ptr->may_flags1 = 0L;
	o_ptr->may_flags2 = 0L;
	o_ptr->may_flags3 = 0L;
	o_ptr->may_flags4 = 0L;

	/* Clear may flags */
	o_ptr->not_flags1 = 0L;
	o_ptr->not_flags2 = 0L;
	o_ptr->not_flags3 = 0L;
	o_ptr->not_flags4 = 0L;

	return;	
}

/*
 * Usage count for an object
 */
void object_usage(int slot)
{
	object_type *o_ptr;

	char o_name[80];

	if (slot >=0) o_ptr =&inventory[slot];
	else o_ptr=&o_list[0-slot];

	if (!o_ptr->k_idx) return;

	/* No sensing when confused */
	if (p_ptr->confused) return;

	/* No sensing when hallucinating */
	if (p_ptr->image) return;

	if ((o_ptr->usage)<MAX_SHORT) o_ptr->usage++;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Don't identify if fully known */
	if (o_ptr->ident & (IDENT_MENTAL)) return;

	/* Don't identify if identified */
	if (object_known_p(o_ptr)) return;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Calculate object bonus */
	if (!object_bonus_p(o_ptr) && (o_ptr->usage > 30) && (o_ptr->usage % 30 == 0) && (rand_int(100) < 30))
	{
		/* Describe what we know */
		msg_format("You feel you know more about the %s you are %s.",o_name,describe_use(slot));

		/* Mark the item as partially known */
		object_bonus(o_ptr, slot < 0);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	}
	else if ((!o_ptr->feeling) && (!o_ptr->ident & (IDENT_SENSE)) && (o_ptr->usage > 5) && (o_ptr->usage % 5 == 0)
		&& (rand_int(100) < 30))
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
			case TV_INSTRUMENT:
			case TV_STAFF:
			{
				/* Check for a feeling */
				int feel = value_check_aux1(o_ptr);

				if (feel)
				{
					/* Describe what we know */
					msg_format("You feel you know more about the %s you are %s.",o_name,describe_use(slot));					

					/* Sense the object */
					o_ptr->feeling = feel;

					/* The object has been "sensed" */
					o_ptr->ident |= (IDENT_SENSE);

					/* Combine / Reorder the pack (later) */
					p_ptr->notice |= (PN_COMBINE | PN_REORDER);

					/* Window stuff */
					p_ptr->window |= (PW_INVEN | PW_EQUIP);

				}
				break;
			}
		}
	}
}

/*
 * Slot holds an object with these flags. Inform the player.
 */
void update_slot_flags(int slot, u32b f1, u32b f2, u32b f3, u32b f4)
{
	char o_name[80];

	object_type *i_ptr;

	/* Get the item */
	if (slot >=0) i_ptr = &inventory[slot];
	else i_ptr= &o_list[0-slot];

	/* Update the object */
	object_can_flags(i_ptr,f1,f2,f3,f4, slot < 0);

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), i_ptr, FALSE, 0);

	/* Describe what we now know */
	msg_format("You feel you know the %s you are %s better...",o_name, describe_use(slot));

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Load screen */
	screen_save();

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Actually display the item */
	list_object_flags(f1, f2, f3, f4, i_ptr->ident & (IDENT_PVAL | IDENT_MENTAL | IDENT_KNOWN) ? i_ptr->pval : 0, LIST_FLAGS_CAN);

	(void)anykey();
	
	/* Load screen */
	screen_load();

}

/*
 * Equipment must have these flags.
 * Note with ALLOW_OBJECT_INFO_MORE, this is
 * an efficiency bottleneck.
 */
void equip_can_flags(u32b f1,u32b f2,u32b f3, u32b f4)
{
	u32b nf1;
	u32b nf2;
	u32b nf3;
	u32b nf4;

	int i;

	object_type *i_ptr;

	/* Hack --- exclude player flags */
	player_flags(&nf1,&nf2,&nf3,&nf4);

	f1 &= ~(nf1);
	f2 &= ~(nf2);
	f3 &= ~(nf3);
	f4 &= ~(nf4);

	/* Hack --- exclude temporary effect flags */
	if (p_ptr->tim_infra) f1 &= ~(TR1_INFRA);
	if (p_ptr->hero || p_ptr->shero) f2 &= ~(TR2_RES_FEAR);
	if (p_ptr->tim_invis) f3 &= ~(TR3_SEE_INVIS);
	if (p_ptr->blessed) f3 &= ~(TR3_HOLD_LIFE);

	/* Exclude known flags */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear bits with can flags */
		f1 &= ~(i_ptr->can_flags1);
		f2 &= ~(i_ptr->can_flags2);
		f3 &= ~(i_ptr->can_flags3);
		f4 &= ~(i_ptr->can_flags4);
	}

	/* Nothing unknown */
	if (!f1 && !f2 && !f3 && !f4) return;

	/* Check for flags */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		u32b if1 = f1;
		u32b if2 = f2;
		u32b if3 = f3;
		u32b if4 = f4;

		bool guess = FALSE;

		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear bits with not flags */
		if1 &= ~(i_ptr->not_flags1);
		if2 &= ~(i_ptr->not_flags2);
		if3 &= ~(i_ptr->not_flags3);
		if4 &= ~(i_ptr->not_flags4);

		/* Do we guess again ? */
		guess |= (if1 & ~(i_ptr->may_flags1)) || (if2 & ~(i_ptr->may_flags2)) || (if3 & ~(i_ptr->may_flags3)) || (if4 & ~(i_ptr->may_flags4));

		/* Mark may flags */
		i_ptr->may_flags1 |= (if1);
		i_ptr->may_flags2 |= (if2);
		i_ptr->may_flags3 |= (if3);
		i_ptr->may_flags4 |= (if4);

		/* Must be identified to continue */
		if ((guess) && (!object_named_p(i_ptr)))
		{
			object_guess_name(i_ptr);
		}
	}

	/* Check inventory */
	equip_may_flags(f1, f2, f3, f4);
}

/*
 * Equipment does not have these flags
 */
void equip_not_flags(u32b f1,u32b f2,u32b f3, u32b f4)
{
	int i;

	object_type *i_ptr;

	/* Mark equipment with not flags*/
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		object_not_flags(i_ptr,f1,f2,f3,f4, TRUE);
	}
}


/*
 * Equipment dropped off (forget all equipped/inventory may flags
 * on objects still held)
 */
void inven_drop_flags(object_type *o_ptr)
{
	int i;
	u32b f1 = o_ptr->may_flags1;
	u32b f2 = o_ptr->may_flags2;
	u32b f3 = o_ptr->may_flags3;
	u32b f4 = o_ptr->may_flags4;

	object_type *i_ptr;

	if (!(f1) && !(f2) && !(f3) && !(f4)) return;

	/* Clear equipment may flags*/
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		i_ptr->may_flags1 &= ~(f1);
		i_ptr->may_flags2 &= ~(f2);
		i_ptr->may_flags3 &= ~(f3);
		i_ptr->may_flags4 &= ~(f4);
	}
}


#define sign(x)	((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))

/*
 * Average damage for good ego ammo of various types, used for balance
 * The current values assume normal (non-seeker) ammo enchanted to +9
 */

#define AVG_SLING_AMMO_DAMAGE 11
#define AVG_BOW_AMMO_DAMAGE 12
#define AVG_XBOW_AMMO_DAMAGE 12


/*
 * Calculate the rating for a given slay combination.
 *
 * The returned value needs to be divided by tot_mon_power to
 * give a normalised value.
 *
 * XXX Note we should be careful using this function, as it
 * is expensive. When we look it up frequently, such as during
 * initialisation and generation of random artifacts we
 * maintain a cache of values, so that we minimise the number
 * of calculations required.
 *
 * After this, we should never have to call it again, as we
 * cache the relevant values for magic items and ego items,
 * and store the computed artifact power with the artifact.
 */
s32b slay_power(u32b s_index)
{
	s32b sv;
	int i;
	int mult;
	monster_race *r_ptr;

	/* s_index combines the slay bytes into an index value
	 * For now we do not support the two undefined slays (XXX),
	 * but this could be added
	 */

	/* Check the cache */
	if (slays)
	{
		/* Look in the cache to see if we know this one yet */
		sv = slays[s_index];

		/* If it's cached, return its value */
		if (sv) return slays[s_index];
	}

	/* Otherwise we need to calculate the expected average multiplier
	 * for this combination (multiplied by the total number of
	 * monsters, which we'll divide out later).
	 */

	sv = 0;

	for(i = 0; i < z_info->r_max; i++) {

		mult = 1;

		r_ptr = &r_info[i];

		/*
		 * Do the following in ascending order so that the best
		 * multiple is retained
		 */

		/* Armoured monsters get partial resistance to resist acid */
		if ( !(r_ptr->flags3 & RF3_IM_ACID) && (r_ptr->flags2 & RF2_ARMOR)
			&& (s_index & 0x00001000L) )
				mult = 2;

		/* Holy brand -- acts like slay */
		if ( (r_ptr->flags3 & RF3_EVIL)
			&& (s_index & 0x00000002L) )
				mult = 3;

		/* New brand - brand_lite - acts like slay */
		if ( (r_ptr->flags3 & (RF3_HURT_LITE))
			&& (s_index & 0x00010000L) )
				mult = 3;

		/* Brands get the multiple if monster is NOT immune / resistant */
		if ( !(r_ptr->flags9 & RF9_RES_DARK)
			&& (s_index & 0x00020000L) )
				mult = 3;
		if ( !(r_ptr->flags3 & RF3_IM_ACID) && !(r_ptr->flags2 & RF2_ARMOR)
			&& (s_index & 0x00001000L) )
				mult = 3;
		if ( !(r_ptr->flags3 & RF3_IM_ELEC)
			&& (s_index & 0x00002000L) )
				mult = 3;
		if ( !(r_ptr->flags3 & RF3_IM_FIRE)
			&& (s_index & 0x00004000L) )
				mult = 4;
		if ( !(r_ptr->flags3 & RF3_IM_COLD)
			&& (s_index & 0x00008000L) )
				mult = 3;
		if ( !(r_ptr->flags3 & RF3_IM_POIS)
			&& (s_index & 0x00000800L) )
				mult = 3;

		/* Original slays */
		if ( (r_ptr->flags3 & RF3_UNDEAD)
			&& (s_index & 0x00000004L) )
				mult = 3;
		if ( (r_ptr->flags3 & RF3_DEMON)
			&& (s_index & 0x00000008L) )
				mult = 3;
		if ( (r_ptr->flags3 & RF3_DRAGON)
			&& (s_index & 0x00000080L) )
				mult = 3;

		/* Increased slays */
		if ( (r_ptr->flags3 & (RF3_ANIMAL | RF3_INSECT | RF3_PLANT))
			&& (s_index & 0x00000001L) )
				mult = 4;
		if ( (r_ptr->flags3 & RF3_ORC)
			&& (s_index & 0x00000010L) )
				mult = 4;
		if ( (r_ptr->flags3 & RF3_TROLL)
			&& (s_index & 0x00000020L) )
				mult = 4;
		if ( (r_ptr->flags3 & RF3_GIANT)
			&& (s_index & 0x00000040L) )
				mult = 4;
		if ( (r_ptr->flags9 & RF9_MAN)
			&& (s_index & 0x00040000L) )
				mult = 4;
		if ( (r_ptr->flags9 & RF9_ELF)
			&& (s_index & 0x00080000L) )
				mult = 4;
		if ( (r_ptr->flags9 & RF9_DWARF)
			&& (s_index & 0x00100000L) )
				mult = 4;

		/* Do kill flags last since they have the highest multiplier */

		if ( (r_ptr->flags3 & RF3_DRAGON)
			&& (s_index & 0x00000100L) )
				mult = 5;
		if ( (r_ptr->flags3 & RF3_DEMON)
			&& (s_index & 0x00000200L) )
				mult = 5;
		if ( (r_ptr->flags3 & RF3_UNDEAD)
			&& (s_index & 0x00000400L) )
				mult = 5;

		/* Add the multiple to sv */
		sv += mult * r_info[i].power;

		/* End loop */
	}

	/* Caching the values? */
	if (slays)
	{
		/* Add to the cache */
		slays[s_index] = sv;
	}

	return sv;

	/* End method */
}

/*
 * Convert all slay and brand flags into a single index value. This is used in randart.c.
 */
u32b slay_index(const u32b f1, const u32b f2, const u32b f3, const u32b f4)
{
	u32b s_index = 0x00000000L;

	(void)f2; (void)f3;

	if (f1 & TR1_SLAY_NATURAL) s_index |= 0x00000001;
	if (f1 & TR1_BRAND_HOLY) s_index |= 0x00000002;
	if (f1 & TR1_SLAY_UNDEAD) s_index |= 0x00000004;
	if (f1 & TR1_SLAY_DEMON) s_index |= 0x00000008;
	if (f1 & TR1_SLAY_ORC) s_index |= 0x00000010;
	if (f1 & TR1_SLAY_TROLL) s_index |= 0x00000020;
	if (f1 & TR1_SLAY_GIANT) s_index |= 0x00000040;
	if (f1 & TR1_SLAY_DRAGON) s_index |= 0x00000080;
	if (f1 & TR1_KILL_DRAGON) s_index |= 0x00000100;
	if (f1 & TR1_KILL_DEMON) s_index |= 0x00000200;
	if (f1 & TR1_KILL_UNDEAD) s_index |= 0x00000400;

	if (f1 & TR1_BRAND_POIS) s_index |= 0x00000800;
	if (f1 & TR1_BRAND_ACID) s_index |= 0x00001000;
	if (f1 & TR1_BRAND_ELEC) s_index |= 0x00002000;
	if (f1 & TR1_BRAND_FIRE) s_index |= 0x00004000;
	if (f1 & TR1_BRAND_COLD) s_index |= 0x00008000;

	if (f4 & TR4_BRAND_LITE) s_index |= 0x00010000;
	if (f4 & TR4_BRAND_DARK) s_index |= 0x00020000;

	if (f4 & TR4_SLAY_MAN) s_index |= 0x00040000;
	if (f4 & TR4_SLAY_ELF) s_index |= 0x00080000;
	if (f4 & TR4_SLAY_DWARF) s_index |= 0x00100000;

	return s_index;
} 



#define ADD_POWER(string, val, flag, flgnum, extra) \
	if (((f##flgnum & flag) != 0) && ((kf##flgnum & flag) == 0)) { \
		p += (val); \
		extra; \
	}

/*
 * Evaluate the objects's overall power level.
 *
 * Adopted from the randart.c patch by Chris Carr / Chris Robertson.
 */
s32b object_power(const object_type *o_ptr)
{
	s32b p = 0;
	s16b k_idx;
	object_kind *k_ptr;
	int immunities = 0;
	int sustains = 0;
	int low_resists = 0;
	int high_resists = 0;
	u32b kf1, kf2, kf3, kf4;
	u32b f1, f2, f3, f4;

	/* If artifact, already computed */
	if (o_ptr->name1) return (a_info[o_ptr->name1].power);

	/* Get the flags */
	object_flags(o_ptr,&f1,&f2,&f3,&f4);

	/* Lookup the item if not yet cached */
	k_idx = lookup_kind(o_ptr->tval, o_ptr->sval);

	/* Get the object */
	k_ptr = &k_info[k_idx];

	/* Set the base kind flags */
	kf1 = k_info[o_ptr->k_idx].flags1;
	kf2 = k_info[o_ptr->k_idx].flags2;
	kf3 = k_info[o_ptr->k_idx].flags3;
	kf4 = k_info[o_ptr->k_idx].flags4;

	/* Evaluate certain abilities based on type of object. */
	switch (o_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			/*
			 * Damage multiplier for bows should be weighted less than that
			 * for melee weapons, since players typically get fewer shots
			 * than hits (note, however, that the multipliers are applied
			 * afterwards in the bow calculation, not before as for melee
			 * weapons, which tends to bring these numbers back into line).
			 */

			/*
			 * Add the average damage of fully enchanted (good) ammo for this
			 * weapon.  Could make this dynamic based on k_info if desired.
			 */

			if (o_ptr->sval == SV_SLING)
			{
				p += AVG_SLING_AMMO_DAMAGE;
			}
			else if (o_ptr->sval == SV_SHORT_BOW ||
					 o_ptr->sval == SV_LONG_BOW)
			{
				p += AVG_BOW_AMMO_DAMAGE;
			}
			else if (o_ptr->sval == SV_HAND_XBOW ||
					 o_ptr->sval == SV_LIGHT_XBOW ||
					 o_ptr->sval == SV_HEAVY_XBOW)
			{
				p += AVG_XBOW_AMMO_DAMAGE;
			}

			mult = bow_multiplier(o_ptr->sval);

			if (f1 & TR1_MIGHT)
			{
				if (o_ptr->pval > 3 || o_ptr->pval < 0)
				{
					p += 20000;	/* inhibit */
					mult = 1;	/* don't overflow */
				}
				else
				{
					mult += o_ptr->pval;
				}
			}
			p *= mult;

			/* Increase power for to-dam */
			if (o_ptr->to_d > 9)
			{
				p += o_ptr->to_d;
			}
			else p += 9;

			if (f1 & TR1_SHOTS)
			{
				/*
				 * Extra shots are calculated differently for bows than for
				 * slings or crossbows, because of rangers ... not any more CC 13/8/01
				 */

				if (o_ptr->pval > 3 || o_ptr->pval < 0)
				{
					p += 20000;	/* inhibit */
				}
				else if (o_ptr->pval > 0)
				{
					if (o_ptr->sval == SV_SHORT_BOW ||
						o_ptr->sval == SV_LONG_BOW)
					{
						p = (p * (1 + o_ptr->pval));
					}
					else
					{
						p = (p * (1 + o_ptr->pval));
					}
				}

			}

			if (o_ptr->to_h > 9)
			{
				p+= (o_ptr->to_h) * 2 / 3;
			}
			else p += 6;

			/* Normalise power back */
			/* We now only count power as 'above' having the basic weapon at the same level */
			if (o_ptr->sval < 10)
			{
				int q = AVG_SLING_AMMO_DAMAGE * bow_multiplier(k_ptr->sval) + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}
			else if (o_ptr->sval < 20)
			{
				int q = AVG_BOW_AMMO_DAMAGE * bow_multiplier(k_ptr->sval) + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}
			else if (o_ptr->sval < 30)
			{
				int q = AVG_XBOW_AMMO_DAMAGE * bow_multiplier(k_ptr->sval) + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}

			/*
			 * Correction to match ratings to melee damage ratings.
			 * We multiply all missile weapons by 1.5 in order to compare damage.
			 * (CR 11/20/01 - changed this to 1.25).
			 * Melee weapons assume 5 attacks per turn, so we must also divide
			 * by 5 to get equal ratings.
			 */

			if (o_ptr->sval == SV_SHORT_BOW ||
				o_ptr->sval == SV_LONG_BOW)
			{
				p = sign(p) * (ABS(p) / 4);
			}
			else
			{
				p = sign(p) * (ABS(p) / 4);
			}

			if (o_ptr->weight < k_ptr->weight)
			{
				p++;
			}

			/* Slight bonus as we may choose to use a swap bow */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_ACID)) != 0) && ((kf2 & (TR2_IGNORE_ACID)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;

			if (((f2 & (TR2_IGNORE_FIRE)) != 0) && ((kf2 & (TR2_IGNORE_FIRE)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;

			if (((f2 & (TR2_IGNORE_THEFT)) != 0) && ((kf2 & (TR2_IGNORE_THEFT)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;
			break;
		}

		case TV_HAFTED:
		case TV_STAFF:
		case TV_DIGGING:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* Note this is 'uncorrected' */
			p += o_ptr->dd * (o_ptr->ds + 1);

			/* Apply the correct ego slay multiplier */
			if (o_ptr->name2)
			{
				p = (p * e_info[o_ptr->name2].slay_power) / tot_mon_power;

				/* Hack -- we may use as a swap weapon */
				if (e_info[o_ptr->name2].slay_power > tot_mon_power) p = p * 5 / 4 + 1;
			}

			/* Hack -- For efficiency, compute for first slay or brand flag only */
			else
			{
				int i;
				u32b j, s_index;

				s_index = slay_index(f1, f2, f3, f4);

				for (i = 0, j = 0x00000001L; (i < 32) && (j != s_index); i++, j<<=1);

				if (i < 32)
				{
					p = (p * magic_slay_power[i]) / tot_mon_power;

					/* Hack -- we may use as a swap weapon */
					if (magic_slay_power[i] > tot_mon_power) p = p * 5 / 4 + 1;
				}
			}

			/* Correction factor for damage */
			p /= 2;

			if (o_ptr->to_d > o_ptr->dd * o_ptr->ds)
			{
				p += o_ptr->to_d;
			}
			else p += o_ptr->dd * o_ptr->ds;

			if (f1 & TR1_BLOWS)
			{
				if (o_ptr->pval > 3 || o_ptr->pval < 0)
				{
					p += 20000;	/* inhibit */
				}
				else if (o_ptr->pval > 0)
				{
					p = sign(p) * ((ABS(p) * (5 + o_ptr->pval)) / 5);
					/* Add an extra +5 per blow to account for damage rings */
					/* (The +5 figure is a compromise here - could be adjusted) */
					p += 5 * o_ptr->pval;
				}
			}

			if (o_ptr->to_h > 9)
			{
				p += (o_ptr->to_h) * 2 / 3;
			}
			else p += 6;

			/* Normalise power back */
			/* We remove the weapon base damage to get 'true' power */
			/* This makes e.g. a sword that provides fire immunity 
			   the same value as a ring that provides fire immunity */
			if (ABS(p) > k_ptr->dd * (k_ptr->ds + 1) / 2 + 6 + o_ptr->dd * o_ptr->ds)
				p -= sign(p) * (k_ptr->dd * (k_ptr->ds + 1) / 2 + 6 + k_ptr->dd * k_ptr->ds);
			else
				p = 0;

			/* Hack -- small swords can be used as secondary weapons */
			if ((p > 0) && (o_ptr->tval == TV_SWORD) && (o_ptr->weight < 100)) p++;

			if (o_ptr->ac != k_ptr->ac)
			{
				p += o_ptr->ac - k_ptr->ac;
			}

			/* Remember, weight is in 0.1 lb. units. */
			if (o_ptr->weight != k_ptr->weight)
			{
				p += (k_ptr->weight - o_ptr->weight) / 20;
			}

			/* Bonus as we may use a staff or hafted weapon as swap weapon */
			if ((o_ptr->tval == TV_STAFF) && (f2 & (TR2_IGNORE_FIRE))) p+= 2;
			if ((o_ptr->tval == TV_HAFTED) && (f2 & (TR2_IGNORE_FIRE))) p++;

			/* Bonuses as we may choose to use a swap weapon */
			if (((f2 & (TR2_IGNORE_ACID)) != 0) && ((kf2 & (TR2_IGNORE_ACID)) == 0)) p++;
			if (((f2 & (TR2_IGNORE_THEFT)) != 0) && ((kf2 & (TR2_IGNORE_THEFT)) == 0)) p++;

			/* Bonus for an extra throwing flag */
			if (((f3 & (TR3_THROWING)) != 0) && ((kf3 & (TR3_THROWING)) == 0))
				p += 2;

			/* Add some specific powers here only */
			ADD_POWER("blessed",		 1, TR3_BLESSED, 3,);
			ADD_POWER("blood vampire",	 25, TR4_VAMP_HP, 4,);
			ADD_POWER("mana vampire",	 14, TR4_VAMP_MANA, 4,);
			break;
		}

		case TV_ARROW:
		case TV_SHOT:
		case TV_BOLT:
		{
			/* Not this is 'uncorrected' */
			p += o_ptr->dd * (o_ptr->ds + 1);

			/* Apply the correct ego slay multiplier */
			if (o_ptr->name2)
			{
				p = (p * e_info[o_ptr->name2].slay_power) / tot_mon_power;

				/* Hack -- we usually have multiple stacks of ammo */
				if (e_info[o_ptr->name2].slay_power > tot_mon_power) p = p * 5 / 4 + 1;
			}

			/* Hack -- For efficiency, compute for first slay or brand flag only */
			else
			{
				int i;
				u32b j, s_index;

				s_index = slay_index(f1, f2, f3, f4);

				for (i = 0, j = 0x00000001L;(i < 32) && (j != s_index); i++, j<<=1);

				if (i < 32)
				{
					p = (p * magic_slay_power[i]) / tot_mon_power;

					/* Hack -- we usually have multiple stacks of ammo */
					if (magic_slay_power[i] > tot_mon_power) p = p * 5 / 4 + 1;
				}
			}

			/* Correct damage */
			p /= 2;

			if (o_ptr->tval == TV_SHOT)
			{
				p *= 2;
			}
			else if (o_ptr->tval == TV_ARROW)
			{
				p *= 5 / 2;
			}
			else if (o_ptr->tval == TV_BOLT)
			{
				p *= 7 / 2;
			}

			if (o_ptr->to_d > 9)
			{
				p += o_ptr->to_d;
			}
			else if (o_ptr->to_d > -10)
			{
				p += 9;
			}
			else
			{
				p += o_ptr->to_d + 9;
			}

			if (o_ptr->to_h > 9)
			{
				p+= (o_ptr->to_h) * 2 / 3;
			}
			else if (o_ptr->to_h > -12)
			{
				p += 6;
			}
			else
			{
				p += (o_ptr->to_d) * 2 / 3 + 6;
			}

			/* Normalise power back */
			/* We remove the ammo base damage to get 'true' power */
			if (o_ptr->tval == TV_SHOT)
			{
				int q = (k_ptr->dd * (k_ptr->ds + 1) / 2) * 2 + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}
			else if (o_ptr->tval == TV_ARROW)
			{
				int q = (k_ptr->dd * (k_ptr->ds + 1) / 2) * 5 / 2 + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}
			else if (o_ptr->tval == TV_BOLT)
			{
				int q = (k_ptr->dd * (k_ptr->ds + 1) / 2) * 7 / 2 + 15;

				if (ABS(p) > q)
					p -= sign(p) * q;
				else
					p = 0;
			}

			if (o_ptr->weight < k_ptr->weight)
			{
				p++;
			}

			/* Bonus as we carry arrows in inventory and fire them */
			if ((o_ptr->tval == TV_ARROW) && ((f2 & (TR2_IGNORE_FIRE)) != 0) && ((kf2 & (TR2_IGNORE_FIRE)) == 0)) p += 2;

			/* Bonus as we carry ammo in inventory and fire them */
			if (((f2 & (TR2_IGNORE_ACID)) != 0) && ((kf2 & (TR2_IGNORE_ACID)) == 0)) p += 2;
			if (((f2 & (TR2_IGNORE_THEFT)) != 0) && ((kf2 & (TR2_IGNORE_THEFT)) == 0)) p++;

			break;
		}

		case TV_BOOTS:
		{
			/* Bonus for resistance on boots due to terrain protection */
			ADD_POWER("resist acid",	 3, TR2_RES_ACID, 2, );
#if 0
			ADD_POWER("resist elec",	 5, TR2_RES_ELEC, 2, );
#endif
			ADD_POWER("resist fire",	 3, TR2_RES_FIRE, 2, );
			ADD_POWER("resist cold",	 1, TR2_RES_COLD, 2, );
			ADD_POWER("resist water",	 3, TR2_RES_COLD, 4, );

			/* Fall through */
		}

		case TV_GLOVES:
		{
			/* Note this is 'uncorrected' */
			p += o_ptr->dd * (o_ptr->ds + 1);

			/* Apply the correct ego slay multiplier */
			if (o_ptr->name2)
			{
				p = (p * e_info[o_ptr->name2].slay_power) / tot_mon_power;
			}

			/* Hack -- For efficiency, compute for first slay or brand flag only */
			else
			{
				int i;
				u32b j, s_index;

				s_index = slay_index(f1, f2, f3, f4);

				for (i = 0, j = 0x00000001L;(i < 32) && (j != s_index); i++, j<<=1);

				if (i < 32) p = (p * magic_slay_power[i]) / tot_mon_power;
			}

			/* Correction factor for damage */
			p /= 2;

			/* Normalise power back */
			/* We remove the weapon base damage to get 'true' power */
			/* This makes e.g. a sword that provides fire immunity the same value as
			   a ring that provides fire immunity */
			if (ABS(p) > k_ptr->dd * (k_ptr->ds + 1) / 2)
				p -= sign(p) * (k_ptr->dd * (k_ptr->ds + 1) / 2);
			else
				p = 0;

			/* Fall through */

		}

		case TV_CLOAK:
		{
			/* Bonus as we may choose to use a swap armour */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_FIRE)) != 0) && ((kf2 & (TR2_IGNORE_FIRE)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;

			/* Fall through */
		}

		case TV_SOFT_ARMOR:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			if (o_ptr->ac != k_ptr->ac)
			{
				p += o_ptr->ac - k_ptr->ac;
			}			
			
			p += sign(o_ptr->to_h) * ((ABS(o_ptr->to_h) * 2) / 3);

			p += o_ptr->to_d * 2;

			/* We assume rings of damage +5 */
			if (o_ptr->to_d > 5)
			{
				p += (o_ptr->to_d - 5) * 2;
			}

			if (o_ptr->weight < k_ptr->weight)
			{
				p += (k_ptr->weight - o_ptr->weight) / 10;
			}

			/* Big bonus as it protects against acid damage */
			if (((f2 & (TR2_IGNORE_ACID)) != 0) && ((kf2 & (TR2_IGNORE_ACID)) == 0)) p += 3;

			/* Bonus as we may choose to use a swap armour */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_THEFT)) != 0) && ((kf2 & (TR2_IGNORE_THEFT)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;

			break;
		}

		case TV_LITE:
		{
			p += sign(o_ptr->to_h) * ((ABS(o_ptr->to_h) * 2) / 3);

			p += o_ptr->to_d * 2;

			/* We assume rings of damage +5 */
			if (o_ptr->to_d > 5)
			{
				p += (o_ptr->to_d - 5) * 2;
			}

			/* Bonuses as we may choose to use a swap light */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_FIRE)) != 0) && ((kf2 & (TR2_IGNORE_FIRE)) == 0)) p++;
			if (f2 & (TR2_IGNORE_THEFT)) p++;

			/* Bonus as light will not go out in water */
			if (((f2 & (TR2_IGNORE_WATER)) != 0) && ((kf2 & (TR2_IGNORE_WATER)) == 0)) p += 3;
			break;
		}

		case TV_RING:
		{
			/* Bonus as we may choose to use a swap armour */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_ELEC)) != 0) && ((kf2 & (TR2_IGNORE_ELEC)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;

			/* Fall through */
		}
		case TV_AMULET:
		{

			p += sign(o_ptr->to_h) * ((ABS(o_ptr->to_h) * 2) / 3);

			p += o_ptr->to_d * 2;

			/* We assume rings of damage +5 */
			if (o_ptr->to_d > 5)
			{
				p += (o_ptr->to_d - 5) * 2;
			}

			/* Bonus as we may choose to use a swap armour */
			/* Hack -- only if it has other flags though */
			if (((f2 & (TR2_IGNORE_THEFT)) != 0) && ((kf2 & (TR2_IGNORE_THEFT)) == 0)
				&& ( (f1 & ~(kf1)) || (f2 & ~(TR2_IGNORE_MASK) & ~(kf2)) || (f3 & ~(kf3)) || (f4 & ~(kf4)) ) ) p++;
			break;
		}

		case TV_WAND:
		{
			if (f2 & (TR2_IGNORE_ELEC)) p+= 2;
			if (f2 & (TR2_IGNORE_THEFT)) p++;

			break;
		}


		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		case TV_SONG_BOOK:
		case TV_INSTRUMENT:
		case TV_SCROLL:
		case TV_MAP:
		{
			/* Bonus as we carry items in inventory */
			if (((f2 & (TR2_IGNORE_FIRE)) != 0) && ((kf2 & (TR2_IGNORE_FIRE)) == 0)) p += 2;

			/* Fall through */
		}
		case TV_FOOD:
		{
			if (((f2 & (TR2_IGNORE_WATER)) != 0) && ((kf2 & (TR2_IGNORE_WATER)) == 0)) p += 2;
			if (f2 & (TR2_IGNORE_THEFT)) p++;

			break;
		}

		case TV_FLASK:
		case TV_POTION:
		{
			if (f2 & (TR2_IGNORE_COLD)) p += 2;
			if (f2 & (TR2_IGNORE_THEFT)) p++;

			break;
		}

	}

	/* Compute ac bonuses */

	/* Evaluate ac bonus differently for armour and non-armour. */
	switch (o_ptr->tval)
	{
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			if (o_ptr->to_a > o_ptr->ac)
			{
				p+= (o_ptr->to_a - o_ptr->ac);
			}

			if (o_ptr->to_a > o_ptr->ac + 10)
			{
				p += (o_ptr->to_a - o_ptr->ac - 10);
			}

			if (o_ptr->to_a > o_ptr->ac + 20)
			{
				p += (o_ptr->to_a - o_ptr->ac - 20);
			}

			if (o_ptr->to_a > 39)
			{
				p += 20000;	/* inhibit */
			}
			break;
		}
		case TV_CROWN:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_STAFF:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_BOW:
		case TV_INSTRUMENT:
		case TV_LITE:
		case TV_RING:
		case TV_AMULET:
		{
			p += sign(o_ptr->to_a) * (ABS(o_ptr->to_a) / 2);

			if (o_ptr->to_a > 9)
			{
				p+= (o_ptr->to_a - 9);
			}

			if (o_ptr->to_a > 19)
			{
				p += (o_ptr->to_a - 19);
			}

			if (o_ptr->to_a > 29)
			{
				p += 20000;	/* inhibit */
			}
			break;
		}
		default:
			return(p);
	}

	/* Other abilities are evaluated independent of the object type. */
	if (o_ptr->pval > 0)
	{
		if (f1 & TR1_STR)
		{
			p += 3 * o_ptr->pval * o_ptr->pval / 4;  /* Was 3 * o_ptr->pval */
		}
		if (f1 & TR1_INT)
		{
			p += o_ptr->pval * o_ptr->pval / 2;  /* Was 2 * o_ptr->pval */
		}
		if (f1 & TR1_WIS)
		{
			p += o_ptr->pval * o_ptr->pval / 2;  /* Was 2 * o_ptr->pval */
		}
		if (f1 & TR1_DEX)
		{
			p += o_ptr->pval * o_ptr->pval;  /* Was 3 * o_ptr->pval */
		}
		if (f1 & TR1_CON)
		{
			p += o_ptr->pval * o_ptr->pval;  /* Was 4 * o_ptr->pval */
		}
		if (f1 & TR1_CHR)
		{
			p += o_ptr->pval * o_ptr->pval / 4; /* Was o_ptr->pval */
		}
		if (f1 & TR1_SAVE)
		{
			p += o_ptr->pval * o_ptr->pval / 4; /* Was o_ptr->pval */
		}
		if (f1 & TR1_DEVICE)
		{
			p += o_ptr->pval * o_ptr->pval / 6; /* Was o_ptr->pval */
		}
		if (f1 & TR1_STEALTH)
		{
			p += o_ptr->pval * o_ptr->pval / 4; /* Was o_ptr->pval */
		}
		if (f1 & TR1_TUNNEL)
		{
			p += o_ptr->pval * o_ptr->pval / 6; /* Was o_ptr->pval */
		}
		/* For now add very small amount for searching */
		if (f1 & TR1_SEARCH)
		{
			p += o_ptr->pval * o_ptr->pval / 12;
		}
		if (f3 & TR3_REGEN_HP)
		{
			p += (o_ptr->pval * o_ptr->pval) * 3 / 2; /* Was constant 3 */
		}
		if (f3 & TR3_REGEN_MANA)
		{
			p += (o_ptr->pval * o_ptr->pval) * 3 / 2; /* Was constant 3 */
		}
		if (f3 & TR3_LITE)
		{
			p += (o_ptr->pval * o_ptr->pval) * 3; /* Was constant 3 */
		}
	}

	else if (o_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (f1 & TR1_STR) p += 4 * o_ptr->pval;
		if (f1 & TR1_INT) p += 2 * o_ptr->pval;
		if (f1 & TR1_WIS) p += 2 * o_ptr->pval;
		if (f1 & TR1_DEX) p += 4 * o_ptr->pval;
		if (f1 & TR1_CON) p += 4 * o_ptr->pval;
		if (f1 & TR1_CHR) p += o_ptr->pval;
		if (f1 & TR1_SAVE) p += o_ptr->pval;
		if (f1 & TR1_DEVICE) p += o_ptr->pval;
		if (f1 & TR1_STEALTH) p += o_ptr->pval;
		if (f1 & TR1_TUNNEL) p += o_ptr->pval;
		if (f1 & TR1_SEARCH) p += o_ptr->pval;
		if (f1 & TR1_INFRA) p += o_ptr->pval;
		if (f3 & TR3_REGEN_HP) p -= 16;
		if (f3 & TR3_REGEN_MANA) p -= 8;
		if (f3 & TR3_LITE) p += 3 * o_ptr->pval;
	}

	if (f1 & TR1_SPEED)
	{
		p += 7 * o_ptr->pval;
	}

	ADD_POWER("sustain STR",	 5, TR2_SUST_STR, 2, sustains++);
	ADD_POWER("sustain INT",	 2, TR2_SUST_INT, 2, sustains++);
	ADD_POWER("sustain WIS",	 2, TR2_SUST_WIS, 2, sustains++);
	ADD_POWER("sustain DEX",	 4, TR2_SUST_DEX, 2, sustains++);
	ADD_POWER("sustain CON",	 3, TR2_SUST_CON, 2, sustains++);
	ADD_POWER("sustain CHR",	 2, TR2_SUST_CHR, 2, sustains++);

	/* Add bonus for sustains getting 'sustain-lock' */
	if (sustains > 1) p += sustains * sustains / 2;

	ADD_POWER("acid immunity",	17, TR2_IM_ACID, 2, immunities++);
	ADD_POWER("elec immunity",	22, TR2_IM_ELEC, 2, immunities++);
	ADD_POWER("fire immunity",	22, TR2_IM_FIRE, 2, immunities++);
	ADD_POWER("cold immunity",	17, TR2_IM_COLD, 2, immunities++);
	ADD_POWER("poison immunity",	22, TR4_IM_POIS, 4, immunities++);

	if (immunities > 1)
	{
		p += 15;
	}
	if (immunities > 2)
	{
		p += 45;
	}
	if (immunities > 3)
	{
		p += 20000;		/* inhibit */
	}

	ADD_POWER("free action",	 7, TR3_FREE_ACT, 3, high_resists++);
	ADD_POWER("hold life",		 6, TR3_HOLD_LIFE, 3, high_resists++);
	ADD_POWER("feather fall",	 2, TR3_FEATHER, 3,); /* was 2 */

	ADD_POWER("see invisible",	 4, TR3_SEE_INVIS, 3,);
	ADD_POWER("sense orcs",	  	3, TR3_ESP_ORC, 3,);
	ADD_POWER("sense trolls",	3, TR3_ESP_TROLL, 3,);
	ADD_POWER("sense giants",	3, TR3_ESP_GIANT, 3,);
	ADD_POWER("sense demons",	4, TR3_ESP_DEMON, 3,);
	ADD_POWER("sense undead",	5, TR3_ESP_UNDEAD, 3,);
	ADD_POWER("sense dragons",       5, TR3_ESP_DRAGON, 3,);
	ADD_POWER("sense nature",	4, TR3_ESP_NATURE, 3,);
	ADD_POWER("telepathy",	  18, TR3_TELEPATHY, 3,);
	ADD_POWER("slow digestion",	 2, TR3_SLOW_DIGEST, 3,);

	/* Digging moved to general section since it can be on anything now */
	ADD_POWER("tunnelling",	 o_ptr->pval, TR1_TUNNEL, 1,);

	ADD_POWER("resist acid",	 2, TR2_RES_ACID, 2, low_resists++);
	ADD_POWER("resist elec",	 3, TR2_RES_ELEC, 2, low_resists++);
	ADD_POWER("resist fire",	 3, TR2_RES_FIRE, 2, low_resists++);
	ADD_POWER("resist cold",	 3, TR2_RES_COLD, 2, low_resists++);

	/* Add bonus for sustains getting 'low_resists-lock' */
	if (low_resists > 1) p += low_resists * low_resists;

	ADD_POWER("resist poison",	14, TR2_RES_POIS, 2, high_resists++);
	ADD_POWER("resist light",	 3, TR2_RES_LITE, 2, high_resists++);
	ADD_POWER("resist dark",	 8, TR2_RES_DARK, 2, high_resists++);
	ADD_POWER("resist blindness",	 8, TR2_RES_BLIND, 2, high_resists++);
	ADD_POWER("resist confusion",	12, TR2_RES_CONFU, 2, high_resists++);
	ADD_POWER("resist sound",	 7, TR2_RES_SOUND, 2, high_resists++);
	ADD_POWER("resist shards",	 4, TR2_RES_SHARD, 2, high_resists++);
	ADD_POWER("resist nexus",	 5, TR2_RES_NEXUS, 2, high_resists++);
	ADD_POWER("resist nether",	10, TR2_RES_NETHR, 2, high_resists++);
	ADD_POWER("resist chaos",	10, TR2_RES_CHAOS, 2, high_resists++);
	ADD_POWER("resist disenchantment", 10, TR2_RES_DISEN, 2, high_resists++);
	ADD_POWER("resist disease", 10, TR4_RES_DISEASE, 4, high_resists++);
	ADD_POWER("resist water",	5, TR4_RES_WATER, 4, high_resists++);

	/* Add bonus for sustains getting 'high_resists-lock' */
	if (high_resists > 1) p += high_resists * high_resists / 2;

	ADD_POWER("random and uncontrolled activation",	 -40, TR3_UNCONTROLLED, 3,);
	ADD_POWER("drain experience",	 -20, TR3_DRAIN_EXP, 3,);

	ADD_POWER("drain health",	 -20, TR3_DRAIN_HP, 3,);
	ADD_POWER("drain mana",	 	 -10, TR3_DRAIN_MANA, 3,);
	ADD_POWER("aggravation",	 -15, TR3_AGGRAVATE, 3,);
	ADD_POWER("light curse",	 -1,  TR3_LIGHT_CURSE, 3,);
	ADD_POWER("heavy curse",	 -4, TR3_HEAVY_CURSE, 3,);
/*	ADD_POWER("permanent curse",	 -40, TR3_PERMA_CURSE, 3,);*/
	ADD_POWER("light vulnerability", -30, TR4_HURT_LITE, 4,);
	ADD_POWER("water vulnerability", -30, TR4_HURT_WATER, 4,);
	ADD_POWER("hunger",	 	 -15, TR3_HUNGER, 3,);
	ADD_POWER("anchor",	 	 -4, TR4_ANCHOR, 4,);
	ADD_POWER("silent",	 	 -20, TR4_SILENT, 4,);
	ADD_POWER("static",	 	 -15, TR4_STATIC, 4,);
	ADD_POWER("windy",	 	 -15, TR4_WINDY, 4,);
	ADD_POWER("animal",	 	 -5, TR4_ANIMAL, 4,);
	ADD_POWER("evil",	 	 -5, TR4_EVIL, 4,);
	ADD_POWER("undead",	 	 -10, TR4_UNDEAD, 4,);
	ADD_POWER("demon",	 	 -10, TR4_DEMON, 4,);
	ADD_POWER("orc",	 	 -3, TR4_ORC, 4,);
	ADD_POWER("troll",	 	 -3, TR4_TROLL, 4,);
	ADD_POWER("giant",	 	 -5, TR4_GIANT, 4,);
	ADD_POWER("dragon",	 	 -10, TR4_DRAGON, 4,);
	ADD_POWER("man",	 	 -3, TR4_MAN, 4,);
	ADD_POWER("dwarf",	 	 -3, TR4_DWARF, 4,);
	ADD_POWER("elf",	 	 -3, TR4_ELF, 4,);
	ADD_POWER("poison vulnerability", -50, TR4_HURT_POIS, 4,);
	ADD_POWER("acid vulnerability",	  -30, TR4_HURT_ACID, 4,);
	ADD_POWER("lightning vulnerability", -40, TR4_HURT_ELEC, 4,);
	ADD_POWER("fire vulnerability",	 -40, TR4_HURT_FIRE, 4,);
	ADD_POWER("cold vulnerability",	 -40, TR4_HURT_COLD, 4,);


	/* Evaluate weight discount. */
	switch (o_ptr->tval)
	{
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
	/*
	 * If a worn item weighs more than 5 pounds, we discount its power by up to 50%.
	 *
	 * This figure is from 30 lb weight limit for mages divided by 6 slots.
	 */
			if (o_ptr->weight >= 50)
			{
				if (p > o_ptr->weight / 25)
					p -= o_ptr->weight / 50;
				else if (p > 0)
					p = (p + 1) / 2;
			}

			break;
		}
		case TV_SWORD:
		case TV_DIGGING:
		case TV_STAFF:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			/* These are breaks for 1-handed, 2-handed, max 4 blows for warriors,
				and max 3 blows for others */
			if (p > 0)
			{
				if ((o_ptr->weight > 960) && (p > 7))
				{
					p -= 1; 
				}

				if ((o_ptr->weight > 240) && (p > 5))
				{
					p -= 1; 
				}

				if ((o_ptr->weight >= 200) && (p > 3))
				{
					p -= 1; 
				}

				if ((o_ptr->weight >= 150) && (p > 1))
				{
					p -= 1; 
				}
			}
			break;
		}
		case TV_BOW:
		case TV_INSTRUMENT:
		case TV_LITE:
		case TV_RING:
		case TV_AMULET:
		{
	/*
	 * If a worn item weighs more than 15 pounds, we discount its power by up to 50%.
	 *
	 * This if for acting as a swap item. Note that super heavy weapons get less of a
	 * discount than other super heavy swap items, because of the increased criticals
	 * and charge bonus.
	 */
			if (o_ptr->weight >= 150)
			{
				if (p > o_ptr->weight / 75)
					p -= o_ptr->weight / 150;
				else if (p > 0)
					p = (p + 1) / 2;
			}
			break;
		}
	}


	return (p);
}


/* Describe the type of feature */
static void describe_feature_type(const feature_type *f_ptr)
{
	if (f_ptr->flags1 & FF1_ENTER)			text_out(" shop entrance");
	else if (f_ptr->flags1 & FF1_DOOR)	text_out(" door");
	else if (f_ptr->flags3 & FF3_CHEST)	text_out(" chest");
	else if (f_ptr->flags1 & FF1_TRAP)	text_out(" trap");
	else if (f_ptr->flags1 & FF1_STAIRS)	text_out(" staircase");
	else if (f_ptr->flags1 & FF1_GLYPH)	text_out(" glyph");
	else if (f_ptr->flags1 & FF1_FLOOR)	text_out(" floor");
	else if (f_ptr->flags1 & FF1_WALL)	text_out(" wall");
	else if (f_ptr->flags3 & FF3_TREE)	text_out(" tree");
	else if (f_ptr->flags1 & FF3_GROUND)	text_out(" ground");

	/*Default*/
	else text_out(" feature");
}



static void describe_feature_basic(int f_idx)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int n, vn;
	cptr vp[128];

	text_out("This is a");

	if (f_ptr->flags2 & FF2_SHALLOW) 	text_out(" shallow");
	if (f_ptr->flags2 & FF2_DEEP) 	text_out(" deep");

	if (f_ptr->flags2 & FF2_GLOW)
	{
	     if (f_ptr->flags1 & FF1_ENTER)	text_out(" well-lit");
		else	text_out(" glowing");
	}

	if (f_ptr->flags2 & FF2_LAVA)
	{
		if (f_ptr->flags2 & FF2_WATER) text_out(" boiling");
		else text_out(" lava");
	}
	if (f_ptr->flags2 & FF2_ICE) 	text_out(" icy");
	if (f_ptr->flags2 & FF2_ACID) 	text_out(" acid");
	if (f_ptr->flags2 & FF2_OIL) 	text_out(" fuel");

	if (f_ptr->flags2 & FF2_WATER)
	{
		if (f_ptr->flags2 & FF2_CAN_DIG) text_out(" mud");
	 	text_out(" water");
	}

	/*Describe the feature type*/
	describe_feature_type(f_ptr);

	/* Describe location */
	if (f_ptr->flags1 & FF1_ENTER)
	{
		text_out(" that is found in the town");
	}
	else if (f_ptr->flags1 & FF1_GLYPH)
	{
		text_out(" that is set by the player");
	}
	else
	{
		text_out(" that");

		if (f_ptr->rarity >= 4) text_out(" rarely");
		else if (f_ptr->rarity >= 2) text_out(" occasionally");
		else text_out(" commonly");

		if (f_ptr->level == 0)
		{
			text_out(" appears in both the town and dungeon");
		}
		else if (f_ptr->level == 1)
		{
			text_out(" appears throughout the dungeon");
		}
		else
		{
			text_out(" appears");

			if (depth_in_feet)
			{
				text_out(format(" at depths of %d feet and below",
			                            f_ptr->level * 50));
			}
			else
			{
				text_out(format(" on dungeon level %d and below",
			                            f_ptr->level));
			}
		}
	}

	/* Allocation */
	vn = 0;

	if (f_ptr->flags3 & (FF3_ALLOC)) vp[vn++] = "as a specially placed item";
	if (f_ptr->flags3 & (FF3_CHEST)) vp[vn++] = "as a item carried by monsters";
	if (f_ptr->flags2 & (FF2_LAKE)) vp[vn++] = "as a 'lake' of terrain";
	if (f_ptr->flags2 & (FF2_RIVER)) vp[vn++] = "as a 'river' of terrain";
	if (f_ptr->flags1 & (FF1_STREAMER)) vp[vn++] = "as a 'streams' of terrain through the dungeon walls";
	if (f_ptr->flags1 & (FF1_DOOR)) vp[vn++] = "placed in a doorway";
	if (f_ptr->flags1 & (FF1_INNER)) vp[vn++] = "as part of the inner wall of a room";
	if (f_ptr->flags1 & (FF1_OUTER)) vp[vn++] = "as part of the outer wall of a room";
	if (f_ptr->flags1 & (FF1_SOLID)) vp[vn++] = "next to the entrance of a corridor in a room";

	/* Describe innate attacks */
	if (vn)
	{
		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (!n) text_out(" ");
			else if ((n) && (n < vn-1)) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out(vp[n]);
		}
	}

	if (f_ptr->flags1 & (FF1_TRAP)) text_out(" with a hidden trap");

	/* End this sentence */
	text_out(".  ");

}




/*
 * Describe the player ability to move, see and cast on or through the feature.
 */
static void describe_feature_player_moves(int f_idx)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int n, vn;
	cptr vp[128];

	bool intro = FALSE;
	bool effect = FALSE;
	bool impede = TRUE;

	/* Collect sight and movement */
	vn = 0;

	if (!(f_ptr->flags1 & (FF1_FLOOR)) && !(f_ptr->flags3 & (FF3_GROUND)))
	{
		if (f_ptr->flags1 & (FF1_LOS)) vp[vn++] = "see";
		if (f_ptr->flags1 & (FF1_PROJECT)) vp[vn++] = "cast spells";
		if (f_ptr->flags1 & (FF1_PROJECT)) vp[vn++] = "fire missiles";

		if (vn) impede = FALSE;
	}

	if (f_ptr->flags1 & (FF1_MOVE)) vp[vn++] = "walk";
	if (f_ptr->flags1 & (FF1_RUN)) vp[vn++] = "run";
	if (f_ptr->flags3 & (FF3_EASY_CLIMB)) vp[vn++] = "climb";

	/* Describe sight and movement */
	if (vn)
	{
		/* Intro */
		text_out("You");

		intro = TRUE;

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		if ((f_ptr->flags1 & (FF1_FLOOR)) || (f_ptr->flags3 & (FF3_GROUND))) text_out(" on");
		else text_out(" through");

		text_out(" this");

		describe_feature_type(f_ptr);
	}

	/* Have to climb in or out of the grid */
	if ((intro) && (f_ptr->flags3 & (FF3_EASY_CLIMB | FF3_MUST_CLIMB)))
	{
		text_out(" taking an extra turn to ");
		if (f_ptr->flags3 & (FF3_EASY_CLIMB))
		{
			text_out("enter ");

			if (f_ptr->flags3 & (FF3_MUST_CLIMB)) text_out("and ");
		}

		if (f_ptr->flags3 & (FF3_MUST_CLIMB))
		{
			text_out("leave ");
		}

		text_out("the grid");
		effect = TRUE;
		impede = TRUE;
	}

	if ((intro) && (f_ptr->flags2 & (FF2_SHALLOW | FF2_DEEP | FF2_FILLED)))
	{
		if (effect) text_out(" and");
		text_out(" making your equipment");
		
		if (f_ptr->flags2 & (FF2_FILLED)) text_out(" significantly");
		else if (!(f_ptr->flags2 & (FF2_DEEP))) text_out(" slightly");
		text_out(" heavier");
		impede = TRUE;
	}

	if ((f_ptr->flags1 & (FF1_FLOOR)) || (f_ptr->flags3 & (FF3_GROUND)))
	{
		vn = 0;

		if (f_ptr->flags1 & (FF1_LOS)) vp[vn++] = "see";
		if (f_ptr->flags1 & (FF1_PROJECT)) vp[vn++] = "cast spells";
		if (f_ptr->flags1 & (FF1_PROJECT)) vp[vn++] = "fire missiles";

		/* Describe sight and movement */
		if (vn)
		{
			/* Intro */
			if (!intro) text_out("You");
			else text_out(" and");

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				/* Intro */
				if (n == 0) text_out(" can ");
				else if (n < vn-1) text_out(", ");
				else text_out(" and ");

				/* Dump */
				text_out(vp[n]);
			}

			if (!intro)
			{
				text_out(" through this");

				describe_feature_type(f_ptr);

			}
			else
			{
				text_out(" through it");
			}

			intro = TRUE;
			impede = FALSE;
		}
	}

	if (!impede) text_out(" without impediment");


	/* Collect innate attacks */
	vn = 0;
	effect = FALSE;
	impede = FALSE;

	if (!(f_ptr->flags1 & (FF1_MOVE)))
	{
		if (!(f_ptr->flags3 & (FF3_EASY_CLIMB)))
		{
			vp[vn++] = "you from moving through it";
			impede = TRUE;
			if (!(f_ptr->flags2 & (FF2_CAN_PASS))) { vp[vn++] = "pass through walls"; effect = TRUE; }
			if (f_ptr->flags1 & (FF1_PERMANENT)) { vp[vn++] = "bore through walls"; effect = TRUE; }
			if (!(f_ptr->flags2 & (FF2_CAN_CLIMB))) { vp[vn++] = "climb"; effect = TRUE; }
		}
	}
	else
	{
		if ((f_ptr->flags2 & (FF2_COVERED)) && !(f_ptr->flags1 & (FF1_BASH))) { vp[vn++] = "lie underneath from surfacing"; effect = TRUE; }
		else if (f_ptr->flags2 & (FF2_COVERED)) { vp[vn++] = "lie underneath from surfacing without bashing through"; effect = TRUE; }
		if (!(f_ptr->flags2 & (FF2_CAN_FLY))) { vp[vn++] = "must fly"; effect = TRUE; }
		if (!(f_ptr->flags2 & (FF2_CAN_SWIM))) { vp[vn++] = "must swim"; effect = TRUE; }
	}
	if (!(f_ptr->flags1 & (FF1_LOS))) vp[vn++] = "line of sight";
	if (!(f_ptr->flags1 & (FF1_PROJECT))) vp[vn++] = "casting spells";
	if (!(f_ptr->flags1 & (FF1_PROJECT))) vp[vn++] = "firing missiles";

	/* Describe innate attacks */
	if (vn)
	{
		/* Intro */
		if (!intro)
		{
			text_out("This ");
			describe_feature_type(f_ptr);
		}
		else text_out(" but it");

		intro = TRUE;

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (!n) text_out(" blocks ");
			else if ((n == 1) && (impede)) text_out(" and ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			if ((effect) && ((n == 1) || (!impede)))
			{
				effect = FALSE;
				text_out("monsters that ");
			}


			/* Dump */
			text_out(vp[n]);
		}
	}

	/* End sentence */
	if (intro) text_out(".  ");

	/* Collect innate attacks */
	vn = 0;
	intro = FALSE;


}



/*
 * Describe the monster ability to move and hide on or through the feature.
 */
static void describe_feature_monster_moves(int f_idx)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int n, vn;
	cptr vp[128];

	bool intro = FALSE;

	vn = 0;

	if (!(f_ptr->flags1 & (FF1_MOVE)) || (f_ptr->blow.method) || (f_ptr->spell))
	{
		if ((f_ptr->flags2 & (FF2_CAN_FLY))) vp[vn++] = "fly"; 
		if ((f_ptr->flags2 & (FF2_CAN_SWIM))) vp[vn++] = "swim";
		if ((f_ptr->flags2 & (FF2_CAN_CLIMB))) vp[vn++] = "climb";
		if ((f_ptr->flags2 & (FF2_CAN_DIG))) vp[vn++] = "dig";
		if ((f_ptr->flags2 & (FF2_CAN_OOZE))) vp[vn++] = "ooze";
		if ((f_ptr->flags2 & (FF2_CAN_PASS))) vp[vn++] = "pass through walls";
		if (!(f_ptr->flags1 & (FF1_PERMANENT))) vp[vn++] = "bore through walls";
	}

	/* Describe monster moves */
	if (vn)
	{
		/* Intro */
		if (!intro)
		{
			text_out("This");
			describe_feature_type(f_ptr);
		}

		intro = TRUE;

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (!n) text_out(" lets monsters that can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		text_out(" pass ");
		if ((f_ptr->flags1 & (FF1_FLOOR)) || (f_ptr->flags3 & (FF3_GROUND))) text_out("over");
		else text_out("through");

		text_out(" without impediment");
	}

	if ((vn) && (f_ptr->blow.method))
	{
		text_out(" provided they resist");
	}

	/* Collect innate attacks */
	vn = 0;

	if ((f_ptr->flags2 & (FF2_HIDE_SNEAK))) vp[vn++] = "sneak"; 
	if ((f_ptr->flags2 & (FF2_HIDE_SWIM))) vp[vn++] = "swim";
	if ((f_ptr->flags2 & (FF2_HIDE_DIG))) vp[vn++] = "dig";
	if ((f_ptr->flags3 & (FF3_EASY_HIDE))) vp[vn++] = "stay still";
	if ((f_ptr->flags2 & (FF2_HIDE_SWIM))) vp[vn++] = "survive without breathing";
	if ((f_ptr->flags2 & (FF2_CAN_PASS))) vp[vn++] = "pass through walls";

	/* Describe innate attacks */
	if (vn)
	{
		/* Intro */
		if (!intro)
		{
			text_out("This");
			describe_feature_type(f_ptr);
		}
		else text_out(" and");

		intro = TRUE;

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (!n) text_out(" hides monsters that can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}
	}
	/* End sentence */
	if (intro) text_out(".  ");
}


/*
 * Describe the player miscellaneous notes on the feature.
 */
static void describe_feature_misc(int f_idx)
{
	const feature_type *f_ptr = &f_info[f_idx];

	/* Other player actions */
	if (f_ptr->flags1 & (FF1_DROP))
	{
		text_out("You can drop objects here");
		if (f_ptr->flags2 & (FF2_HIDE_ITEM))
		{
			text_out(" but they will disappear from view");
		}
		text_out(".  ");
	}
	
	/* Other player actions */
	if (f_ptr->flags1 & (FF1_REMEMBER))
	{
		text_out("You will remember this");
		describe_feature_type(f_ptr);
		text_out(" on your overhead map.  ");
	}

	/* Other player actions */
	if (f_ptr->flags1 & (FF1_NOTICE))
	{
		text_out("You stop running next to this");
		describe_feature_type(f_ptr);

		if (f_ptr->flags1 & (FF1_STAIRS))
		{
			text_out(" unless you have the ignore_stairs option on");
		}
		text_out(".  ");
	}

	/* Other player actions */
	if (!(f_ptr->flags1 & (FF1_MOVE)))
	{
		text_out("You can get stuck inside this");
		describe_feature_type(f_ptr);
		text_out(" should it appear around you.  ");
	}

	/* Other player actions */
	if (f_ptr->flags2 & (FF2_FILLED))
	{
		text_out("You can't breath in this.  ");
	}

	/* Take damage if trap */
	if (!(f_ptr->flags1 & (FF1_HIT_TRAP)))
	{
		bool effect = FALSE;

		if (f_ptr->blow.method)
		{
			effect = TRUE;

			text_out("Whilst in this");
			describe_feature_type(f_ptr);

			/* Hack -- should really describe blow */
			text_out(format(" you take %dd%d damage", f_ptr->blow.d_dice, f_ptr->blow.d_side));
		}

		if (f_ptr->spell)
		{
			if (effect) text_out(" and");
			else
			{
				text_out("Whilst in this");
				describe_feature_type(f_ptr);
			}

			effect = TRUE;

			/* Hack -- should really describe spell */
			text_out(" suffer its effects");
		}

		if (effect) text_out(" continuously.  ");
	}

	if (f_ptr->flags3 & (FF3_EASY_CLIMB))
	{
		text_out("You can melee huge monsters more easily from here.  ");
	}
}



/*
 * Return true if player can do this action to the feature.
 */
static bool is_player_action_valid(int f_idx, int action)
{
	const feature_type *f_ptr = &f_info[f_idx];

	/* Feature flag 1 actions */
	if (action < 32)
	{
		if ((f_ptr->flags1 & (1L << action)) &&
			((1L << action) & (FF1_SECRET | FF1_OPEN | FF1_CLOSE | FF1_BASH |
				FF1_DISARM | FF1_SPIKE | FF1_ENTER | FF1_TUNNEL | FF1_FLOOR | FF1_HIT_TRAP)))
				return (TRUE);
		/* Hack -- bashable features are opened 50% of the time */
		else if ((f_ptr->flags1 & (FF1_BASH)) &&
			((1L << action) & (FF1_OPEN)))
				return (TRUE);

		/* Hack -- glyphs can be set on a floor */
		else if ((f_ptr->flags1 & (FF1_FLOOR)) &&
			((1L << action) & (FF1_GLYPH)))
				return (TRUE);

		return (FALSE);
	}

	/* Feature flag 2 actions */
	else if (action < 64)
	{
		if ((f_ptr->flags2 & (1L << (action - 32))) &&
			((1L << (action - 32)) & (FF2_HURT_ROCK | FF2_HURT_FIRE | FF2_HURT_COLD | FF2_HURT_ACID | 
				FF2_KILL_HUGE | FF2_KILL_MOVE)))
				return (TRUE);

		return (FALSE);
	}

	/* Feature flag 3 actions */
	else if (action < 96)
	{
		if((f_ptr->flags3 & (1L << (action - 64))) &&
			((1L << (action - 64)) & (FF3_HURT_POIS | FF3_HURT_ELEC | FF3_HURT_WATER | 
				FF3_HURT_BWATER | FF3_USE_FEAT | FF3_GET_FEAT | FF3_NEED_TREE)))
				return (TRUE);

		return (FALSE);
	}

	/* Player action not valid */
	return (FALSE);
}

/*
 * Describe the player initiated transitions for this feature, and the resulting features.
 */
static void describe_feature_actions(int f_idx)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int n, vn;
	cptr vp[128];

	int i;

	bool intro = FALSE;

	/* Permanent stuff never gets changed */
	if (f_ptr->flags1 & FF1_PERMANENT) return;

	/* Check all the actions */
	for (i=0; i<96;i++)
	{
		/* Do we have a known transition */
		if (is_player_action_valid(f_idx, i))
		{
			int newfeat = feat_state(f_idx, i);

			vn = 0;

			switch (i)
			{
				case FS_SECRET: vp[vn++] = "search"; break;
				case FS_OPEN: if (f_ptr->flags1 & (FF1_OPEN)) vp[vn++] = "open";
					      if (f_ptr->flags1 & (FF1_BASH)) vp[vn++] = "bash"; break;    
				case FS_CLOSE: vp[vn++] = "close"; break;
				case FS_BASH: vp[vn++] = "bash"; break;
				case FS_DISARM: vp[vn++] ="disarm"; break;
				case FS_SPIKE: vp[vn++] ="spike"; break;
				case FS_ENTER: vp[vn++] ="enter"; break;
				case FS_TUNNEL: if (f_ptr->flags2 & (FF2_CAN_DIG)) vp[vn++] ="dig";
						else vp[vn++] ="tunnel"; break;
				case FS_FLOOR: vp[vn++] ="set traps on"; break;
				case FS_GLYPH: vp[vn++] ="create glyphs on"; break;
				case FS_HIT_TRAP: vp[vn++] = "stumble on"; break;
				case FS_HURT_ROCK: vp[vn++] = "magically remove rock from"; break;
				case FS_HURT_FIRE: vp[vn++] = "burn"; break;
				case FS_HURT_COLD: vp[vn++] = "freeze"; break;
				case FS_HURT_ACID: vp[vn++] = "melt"; break;
				case FS_KILL_HUGE: vp[vn++] = "magically destroy"; break;
				case FS_KILL_MOVE: vp[vn++] ="disturb"; break;
				case FS_HURT_POIS: vp[vn++] = "poison"; break;
				case FS_HURT_ELEC: vp[vn++] = "electrify"; break;
				case FS_HURT_WATER: vp[vn++] = "flood"; break;
				case FS_HURT_BWATER: vp[vn++] = "boil"; vp[vn++] = "steam"; break;
				case FS_USE_FEAT: vp[vn++] = "use"; break;
				case FS_GET_FEAT: vp[vn++] = "gather"; break;
				case FS_NEED_TREE: vp[vn++] = "cut down"; break;
				default: break;
			}

			/* Hack -- handle some transitions */
			if (i == FS_GLYPH) newfeat = FEAT_GLYPH;
			else if (i == FS_FLOOR) newfeat = FEAT_INVIS;
			else if (i == FS_ENTER) newfeat = f_idx;

			/* Describe transitions */
			if (vn)
			{
				bool effect = FALSE;

				/* Intro */
				if (!intro)
				{
					text_out("You");
				}

				/* Note */
				intro = TRUE;

				/* Scan */
				for (n = 0; n < vn; n++)
				{
					/* Intro */
					if (n == 0) text_out(" can ");
					else if (n < vn-1) text_out(", ");
					else text_out(" or ");

					/* Dump */
					text_out(vp[n]);
				}

				text_out(" it to");

				/* Take damage if trap */
				if (((f_ptr->flags1 & (FF1_HIT_TRAP)) != 0) && ((i == FS_OPEN) || (i == FS_CLOSE) || (i == FS_BASH) || (i == FS_TUNNEL) || (i == FS_HIT_TRAP)))
				{
					if (f_ptr->blow.method)
					{
						if (effect) text_out(" and");
						effect = TRUE;

						/* Hack -- should really describe blow */
						text_out(format(" take %dd%d damage", f_ptr->blow.d_dice, f_ptr->blow.d_side));
					}

					if (f_ptr->spell)
					{
						if (effect) text_out(" and");
						effect = TRUE;

						/* Hack -- should really describe spell */
						text_out(" suffer its effects");
					}

					if ((f_ptr->flags3 & (FF3_PICK_TRAP | FF3_PICK_DOOR)) != 0)
					{
						newfeat = f_idx;
					}
				}

				/* Describe new feature */
				if ((newfeat != f_idx) || ((f_info[newfeat].flags3 & (FF3_PICK_TRAP | FF3_PICK_DOOR)) != 0))
				{
					if (effect) text_out(" and");
					effect = TRUE;

					if (i == FS_SECRET) text_out(" find ");
					else text_out(" make it ");

					if (f_info[newfeat].flags3 & (FF3_PICK_TRAP)) text_out("a random trap");
					else if (f_info[newfeat].flags3 & (FF3_PICK_DOOR)) text_out("a random door");
					else text_out(f_name + f_info[newfeat].name);

					if (cheat_xtra) text_out(format(" (%d)", newfeat));

					/* Side effects -- stop glow */
					if (((f_ptr->flags2 & (FF2_GLOW)) != 0)
						&& ((f_info[newfeat].flags2 & (FF2_GLOW)) == 0))
					{
						text_out(" and darken the surrounding grids");
					}

					/* Side effects -- start glow */
					if (((f_ptr->flags2 & (FF2_GLOW)) == 0)
						&& ((f_info[newfeat].flags2 & (FF2_GLOW)) != 0))
					{
						text_out(" and light up the surrounding grids");
					}

					/* Side effects -- remove branches */
					if (((f_ptr->flags3 & (FF3_TREE)) == 0)
						&& ((f_info[newfeat].flags3 & (FF3_TREE)) != 0))
					{
						text_out(" and remove the surrounding branches");
					}

					/* Side effects -- remove branches */
					if (((f_ptr->flags3 & (FF3_TREE)) != 0)
						&& ((f_info[newfeat].flags3 & (FF3_TREE)) == 0))
					{
						text_out(" and cover the surrounding grids with branches");
					}

					/* Side effects -- remove outside */
					if (((f_ptr->flags3 & (FF3_OUTSIDE)) != 0)
						&& ((f_info[newfeat].flags3 & (FF3_OUTSIDE)) == 0))
					{
						text_out(" and hide the surrounding grids from the sun");
					}

					/* Side effects -- remove outside */
					if (((f_ptr->flags3 & (FF3_OUTSIDE)) == 0)
						&& ((f_info[newfeat].flags3 & (FF3_OUTSIDE)) != 0))
					{
						text_out(" and expose the surrounding grids to daylight");
					}
				}

				/* Side effects -- drop / use / get object */
				if ((((f_ptr->flags1 & (FF1_HAS_GOLD | FF1_HAS_ITEM)) != 0)
					&& ((f_info[newfeat].flags1 & (FF1_HAS_GOLD | FF1_HAS_ITEM)) == 0))
					|| (f_ptr->k_idx && ((i == FS_USE_FEAT) || (i == FS_GET_FEAT) || (i == FS_DISARM))))
				{
					int count = 0;

					if (effect) text_out(" and ");
					else text_out(" ");

					effect = TRUE;

					if (f_ptr->flags3 & (FF3_DROP_1D2)) count += 2;
					if (f_ptr->flags3 & (FF3_DROP_1D3)) count += 3;

					if (i == FS_USE_FEAT)
					{
						switch(k_info[f_ptr->k_idx].tval)
						{
							case TV_RUNESTONE:
								text_out("apply, "); /* Fall through */
							case TV_SONG_BOOK:
							case TV_MAGIC_BOOK:
							case TV_PRAYER_BOOK:
								text_out("study or cast from "); break;
							case TV_INSTRUMENT:
								text_out("play "); break;
							case TV_FLASK:
							case TV_POTION:
								text_out("fill a bottle or flask to get "); break;
							default: text_out("use "); break;
						}
					}

					if (((f_ptr->flags1 & (FF1_HAS_GOLD | FF1_HAS_ITEM)) != 0) || (i == FS_DISARM) || (i == FS_GET_FEAT))
					{
						if (i == FS_USE_FEAT) text_out("or ");
						text_out("find ");
					}

					if ((f_ptr->k_idx) && ((i == FS_USE_FEAT) || (i == FS_GET_FEAT) || (i == FS_DISARM)))
					{
						object_type object_type_body;
						char o_name[80];

						object_type *o_ptr = &object_type_body;

						object_prep(o_ptr, f_ptr->k_idx);

						/* Set it to stored to prevent revealing flavours */
						o_ptr->ident |= (IDENT_STORE);

						if (count)
						{
							text_out("up to ");
							o_ptr->number = count;
						}
						else o_ptr->number = 1;

						object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

						text_out(o_name);
					}
					else if (count)
					{
						text_out(format("up to %d ", count));
					}

					if (f_ptr->flags1 & (FF1_HAS_ITEM))
					{
						if (!count) text_out("an ");
						text_out(format("object%s", count > 1 ? "s" : ""));
					}

					if (f_ptr->flags1 & (FF1_HAS_GOLD))
					{
						if (f_ptr->flags1 & (FF1_HAS_ITEM)) text_out(" or ");
						else if (!count) text_out("a ");
						text_out(format("treasure%s", count > 1 ? "s" : ""));
					}
				}

				/* Side effects -- set traps or make feature dangerous */
				if ((i == FS_FLOOR) || (!(f_ptr->blow.method) && (f_info[newfeat].blow.method)))
				{
					if (effect) text_out(" and");
					effect = TRUE;

					text_out(" potentially harm monsters");
				}

				/* Side effects -- enter shop */
				if (i == FS_ENTER)
				{
					if (effect) text_out(" and");
					effect = TRUE;
					text_out(" find useful items");
				}

				/* No effect */
				if (!effect)
				{
					text_out(" no effect");
				}

				/* End sentence */
				text_out(".  ");

				/* Need intro */
				intro = FALSE;
			}
		}
	}
}


/*
 * Return true if feature does this action to itself when placed or dynamically
 */
static bool is_feature_action_valid(int f_idx, int action)
{
	const feature_type *f_ptr = &f_info[f_idx];

	/* Feature flag 1 actions that always occur */
	if (action < 32)
	{
		if (((1L << action) & (FF1_TUNNEL)) != 0)
				return (TRUE);

		return (FALSE);
	}

	/* Feature flag 2 actions */
	else if (action < 64)
	{
		if (((f_ptr->flags2 & (1L << (action - 32))) != 0) &&
			(((1L << action) & (FF2_HURT_FIRE)) != 0))
				return (TRUE);

		/* Feature flag 2 actions that always occur */
		else if (((1L << (action - 32)) & (FF2_CHASM)) != 0) 
				return (TRUE);

		return (FALSE);
	}

	/* Feature flag 3 actions */
	else if (action < 96)
	{
		if (((f_ptr->flags3 & (1L << (action - 64))) != 0) &&
			(((1L << (action - 64)) & (FF3_PICK_TRAP | FF3_PICK_DOOR | FF3_NEED_TREE | FF3_INSTANT |
				FF3_ADJACENT | FF3_TIMED | FF3_ERUPT | FF3_STRIKE | FF3_SPREAD)) != 0))
				return (TRUE);

		/* Feature flag 3 actions that always occur*/
		else if (((1L << (action - 64)) & (FF3_TREE)) != 0)
				return (TRUE);

		return (FALSE);
	}

	/* Feature action not valid */
	return (FALSE);
}

/*
 * Describe the feature initiated transitions and resulting features.
 */
static void describe_feature_transitions(int f_idx)
{
	int n, vn;
	cptr vp[128];

	int i;

	bool intro = FALSE;

	/* Permanent stuff never gets changed */
	if (f_info[f_idx].flags1 & FF1_PERMANENT) return;

	/* Get the new feature */
	for (i=0;i<MAX_FEAT_STATES;i++)
	{
		/* Do we have a known transition */
		if (is_feature_action_valid(f_idx, i))
		{
			int newfeat = feat_state(f_idx, i);

			vn = 0;

			switch (i)
			{
				case FS_TUNNEL: vp[vn++] ="tunnelled through"; break;
				case FS_BRIDGE: vp[vn++] = "on the safe path"; break;
				case FS_HURT_FIRE: vp[vn++] = "on destroyed levels"; break;
				case FS_TREE: vp[vn++] = "near a tree"; break;
				case FS_NEED_TREE: vp[vn++] = "not near a tree"; break;
				case FS_INSTANT: case FS_ADJACENT: vp[vn++] = "a moment passes"; break;
				case FS_TIMED: vp[vn++] = "time passes"; break;
				case FS_SPREAD: vp[vn++] = "it spreads"; break;
				case FS_STRIKE: case FS_ERUPT: vp[vn++] = "chance dictates"; break;
				default: break;
			}

			/* Describe transitions */
			if (vn)
			{
				bool effect = FALSE;

				/* Intro */
				if (!intro)
				{
					text_out("When ");
				}

				/* Note */
				intro = TRUE;

				/* Scan */
				for (n = 0; n < vn; n++)
				{
					/* Intro */
					if ((n) && (n < vn-1)) text_out(", ");
					else text_out(" or ");

					/* Dump */
					text_out(vp[n]);
				}

				/* Describe new feature */
				if ((newfeat != f_idx) || (f_info[newfeat].flags3 & (FF3_PICK_TRAP | FF3_PICK_DOOR)))
				{
					text_out(" it becomes ");

					if (f_info[newfeat].flags3 & (FF3_PICK_TRAP)) text_out("a random trap");
					else if (f_info[newfeat].flags3 & (FF3_PICK_DOOR)) text_out("a random door");
					else text_out(f_name + f_info[newfeat].name);

					if (cheat_xtra) text_out(format(" (%d)", newfeat));
				}

				/* Side effects -- dynamic */
				if (i == FS_ADJACENT)
				{
					if (effect) text_out(" and");
					effect = TRUE;

					text_out(" it affects all adjacent grids");
				}

				/* Side effects -- erupts */
				if (i == FS_ERUPT)
				{
					if (effect) text_out(" and");
					effect = TRUE;

					text_out(" it erupts in a radius 2 ball");
				}

				/* Side effects -- erupts */
				if (i == FS_STRIKE)
				{
					if (effect) text_out(" and");
					effect = TRUE;

					text_out(" strikes a random grid nearby");
				}

				/* No effect */
				if (!effect)
				{
					text_out(" it remains unchanged");
				}

				/* End sentence */
				text_out(".  ");

				/* Need intro */
				intro = FALSE;
			}
		}
	}
}




/*
 * Hack -- display feature information using "roff()"
 *
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen or line, on a cleared line, in which the output is
 * to take place.  One extra blank line is left after the recall.
 */
void describe_feature(int f_idx)
{
	/* Describe the movement and level of the monster */
	describe_feature_basic(f_idx);

	/* Describe the movement, LOS, and projection for player and monsters */
	describe_feature_player_moves(f_idx);

	/* Describe the movement, LOS, and projection for player and monsters */
	describe_feature_monster_moves(f_idx);

	/* Describe the movement, LOS, and projection for player and monsters */
	describe_feature_misc(f_idx);

	/* Describe feature actions */
	describe_feature_actions(f_idx);

	/* Describe feature transitions */
	describe_feature_transitions(f_idx);

	/* All done */
	text_out("\n");
}





/*
 * Hack -- Display the "name" and "attr/chars" of a feature
 */
void feature_roff_top(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	byte a1, a2;
	char c1, c2;

	/* Get the chars */
	c1 = f_ptr->d_char;
	c2 = f_ptr->x_char;

	/* Get the attrs */
	a1 = f_ptr->d_attr;
	a2 = f_ptr->x_attr;

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, format("%^s",f_name + f_ptr->name));


	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	if (!(use_trptile) && !(use_dbltile))
	{
		/* Append the "optional" attr/char info */
		Term_addstr(-1, TERM_WHITE, "/('");
		Term_addch(a2, c2);
		if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
		Term_addstr(-1, TERM_WHITE, "'):");
	}
}



/*
 * Hack -- describe the given feature at the top of the screen
 */
void screen_feature_roff(int f_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall feature */
	describe_feature(f_idx);

	/* Describe feature */
	feature_roff_top(f_idx);

}

/*
 * Hack -- describe the given feature in the current "term" window
 */
void display_feature_roff(int f_idx)
{
	int y;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall feature */
	describe_feature(f_idx);

	/* Describe feature  */
	feature_roff_top(f_idx);
}

