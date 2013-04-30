/* File: info.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.6
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

/*
 * Modes of list_object_flags()
 */
#define LIST_FLAGS_CAN   1 /* Target selected normally */
#define LIST_FLAGS_MAY     2 /* Always targets self */
#define LIST_FLAGS_NOT    3 /* Always targets aimed target */


/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_kind *k_ptr;

	if (mode != OBJECT_FLAGS_FULL)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = 0L;

		if (mode != OBJECT_FLAGS_RANDOM)
		{
			/* Add flags object is known to have */
			*f1 |= o_ptr->can_flags1;
			*f2 |= o_ptr->can_flags2;
			*f3 |= o_ptr->can_flags3;
			return;
		}

		/* Must be identified */
		if (!object_known_p(o_ptr)) return;
	}

	if (mode != OBJECT_FLAGS_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		if (mode == OBJECT_FLAGS_FULL)
		{
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

		}

		/* Full knowledge for *identified* objects */
		if (!(o_ptr->ident & IDENT_MENTAL)) return;
	}

	/* Rune powers */
	if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_RUNES)
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
			}
		}
	}
	/* Extra powers */
	else
	{
		if (object_xtra_what[o_ptr->xtra1] == 1)
		{
			(*f1) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
		}
		else if (object_xtra_what[o_ptr->xtra1] == 2)
		{
			(*f2) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
		}
		else if (object_xtra_what[o_ptr->xtra1] == 3)
		{
			(*f3) |= (object_xtra_base[o_ptr->xtra1] << o_ptr->xtra2);
		}
	}
}




/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f1, f2, f3);
}


/*
 * Set obvious flags for average items 
 */
void object_obvious_flags(object_type *o_ptr)
{
        if (o_ptr->ident & (IDENT_MENTAL))
        {
                u32b f1,f2,f3;

                /* Spoil the object */
                object_flags(o_ptr, &f1, &f2, &f3);

                object_can_flags(o_ptr, f1, f2, f3);

                object_not_flags(o_ptr, ~(f1), ~(f2), ~(f3));
        }

        else if (object_known_p(o_ptr) && !o_ptr->name1)
        {
                /* Abilities of base item are always known */
                o_ptr->can_flags1 |= k_info[o_ptr->k_idx].flags1;
                o_ptr->can_flags2 |= k_info[o_ptr->k_idx].flags2;
                o_ptr->can_flags3 |= k_info[o_ptr->k_idx].flags3;

                /* Non-runed average item have no more hidden ability */
                if (!o_ptr->name2 && !o_ptr->xtra1 && wield_slot(o_ptr) >= INVEN_WIELD)
                        object_not_flags(o_ptr, ~(o_ptr->can_flags1), 
                                         ~(o_ptr->can_flags2), 
                                         ~(o_ptr->can_flags3));
        }
}


/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
        /* Set obvious flags for an average item */
        object_obvious_flags(o_ptr);

	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f1, f2, f3);
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


/*
 * Hack -- Get spell description
 */
bool spell_desc(const spell_type *s_ptr, const cptr intro, int level, bool detail, int target)
{
	int vn;

	int m,n,r;
	cptr vp[64];

	cptr p, q, s, t, u;

	bool introduced = FALSE;

	u32b id_flags = s_ptr->flags1;

	/* Collect detects */
	vn = 0;
	if (s_ptr->flags1 & (SF1_DETECT_DOORS))	vp[vn++] = "doors";
	if (s_ptr->flags1 & (SF1_DETECT_TRAPS))	vp[vn++] = "traps";
	if (s_ptr->flags1 & (SF1_DETECT_STAIRS))	vp[vn++] = "stairs";
	if (s_ptr->flags1 & (SF1_DETECT_WATER))	vp[vn++] = "running water";
	if (s_ptr->flags1 & (SF1_DETECT_GOLD))	vp[vn++] = "gold, including hidden treasures";
	if (s_ptr->flags1 & (SF1_DETECT_OBJECT))	vp[vn++] = "objects, including hidden objects";
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC))	vp[vn++] = "magic objects, and senses their power";
	if (s_ptr->flags1 & (SF1_DETECT_CURSE))	vp[vn++] = "cursed objects, and senses their power";
	if (s_ptr->flags1 & (SF1_DETECT_MONSTER))	vp[vn++] = "visible monsters";
	if (s_ptr->flags1 & (SF1_DETECT_EVIL))	vp[vn++] = "evil monsters";
	if (s_ptr->flags1 & (SF1_DETECT_INVIS))	vp[vn++] = "invisible monsters";
	if (s_ptr->flags1 & (SF1_DETECT_ANIMAL))	vp[vn++] = "natural creatures";
	if (s_ptr->flags1 & (SF1_DETECT_UNDEAD))	vp[vn++] = "undead";
	if (s_ptr->flags1 & (SF1_DETECT_DEMON))	vp[vn++] = "demons";

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
		text_out(" on the current panel");
	}

	/* Some identifies assume earlier IDs */
	if (s_ptr->flags1 & (SF1_IDENT_FULLY)) id_flags |= SF1_IDENT;
	if (s_ptr->flags1 & (SF1_IDENT_PACK)) id_flags |= SF1_IDENT;
	if (s_ptr->flags1 & (SF1_DETECT_CURSE)) id_flags |= SF1_IDENT_PACK;
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC)) id_flags |= SF1_IDENT_PACK;
	if (s_ptr->flags1 & (SF1_DETECT_CURSE)) id_flags |= SF1_IDENT_SENSE;
	if (s_ptr->flags1 & (SF1_DETECT_MAGIC)) id_flags |= SF1_IDENT_SENSE;
	if (s_ptr->flags1 & (SF1_FORGET)) id_flags |= SF1_IDENT_PACK;

	/* Collect identifies */
	vn = 0;
	if (id_flags & (SF1_IDENT_SENSE)) vp[vn++]="the general power level";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="the bonuses to hit, damage and armour class";
	if (id_flags & (SF1_IDENT_BONUS)) vp[vn++]="the number of charges";
	if ((id_flags & (SF1_IDENT)) || (s_ptr->type == SPELL_IDENT_TVAL)) vp[vn++]="the kind, ego-item and artifact names";
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
	if (id_flags & (SF1_IDENT)) vp[vn++]="unknown item";
	if (id_flags & (SF1_IDENT_RUMOR | SF1_IDENT_FULLY | SF1_FORGET)) vp[vn++]="known item";
	if (id_flags & (SF1_DETECT_CURSE)) vp[vn++]="cursed item";
	if (id_flags & (SF1_DETECT_MAGIC)) vp[vn++]="magic item";

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
		|| (s_ptr->type == SPELL_BRAND_WEAPON)) vp[vn++]="piece of armor";
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
	    (s_ptr->type == SPELL_BRAND_AMMO))
	{
		vp[vn++]=inscrip_text[INSCRIP_MIN_HIDDEN-INSCRIP_NULL+s_ptr->param];
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
	if (s_ptr->flags2 & (SF2_INVULN)) vp[vn++]="makes you invulnerible to damage";
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
	if (s_ptr->type ==SPELL_INVEN_BELT) vp[vn++]="creates a magical belt";


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
	else if ((s_ptr->l_dice) && (s_ptr->l_side))
	{
		/* End */
		text_out(format(" for %dd%d turns",s_ptr->l_dice,s_ptr->l_side));
	}
	else if (s_ptr->l_plus)
	{
		/* End */
		text_out(format(" for %d turns",s_ptr->l_plus));
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
	if (s_ptr->flags3 & (SF3_SLOW_POIS)) vp[vn++]="poison";
	if (s_ptr->flags3 & (SF3_SLOW_CUTS)) vp[vn++]="cuts";

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

	/* Collect stat gain effects */
	vn = 0;

	if (s_ptr->flags3 & (SF3_INC_STR)) vp[vn++]="strength";
	if (s_ptr->flags3 & (SF3_INC_INT)) vp[vn++]="intelligence";
	if (s_ptr->flags3 & (SF3_INC_WIS)) vp[vn++]="wisdom";
	if (s_ptr->flags3 & (SF3_INC_DEX)) vp[vn++]="dexterity";
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

	/* Collect restore effects */
	vn = 0;
	if (s_ptr->flags3 & (SF3_CURE_STR)) vp[vn++]="strength";
	if (s_ptr->flags3 & (SF3_CURE_INT)) vp[vn++]="intelligence";
	if (s_ptr->flags3 & (SF3_CURE_WIS)) vp[vn++]="wisdom";
	if (s_ptr->flags3 & (SF3_CURE_DEX)) vp[vn++]="dexterity";
	if (s_ptr->flags3 & (SF3_CURE_CON)) vp[vn++]="constitution";
	if (s_ptr->flags3 & (SF3_CURE_CHR)) vp[vn++]="charisma";
	if (s_ptr->flags3 & (SF3_CURE_EXP)) vp[vn++]="experience";
	if (s_ptr->flags3 & (SF3_CURE_MANA)) vp[vn++]="mana";

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

	if (s_ptr->flags3 & (SF3_SLOW_MANA)) vp[vn++] = "partially restores your mana";
	if (s_ptr->flags3 & (SF3_DEC_FOOD)) vp[vn++] = "makes you weak from hunger";
	if (s_ptr->flags2 & (SF2_CURSE_WEAPON)) vp[vn++] = "curses your weapon";
	if (s_ptr->flags2 & (SF2_CURSE_ARMOR)) vp[vn++] = "curses your armor";
	if (s_ptr->flags2 & (SF2_AGGRAVATE)) vp[vn++] = "wakes up nearby monsters and hastes those in line of sight";
	if (s_ptr->type == SPELL_SUMMON) vp[vn++] = "summons monsters";
	if (s_ptr->type == SPELL_SUMMON_RACE) vp[vn++] = format("summons %s%s",
		is_a_vowel((r_name+r_info[s_ptr->param].name)[0])?"an ":"a ",
		r_name+r_info[s_ptr->param].name);
	if (s_ptr->type == SPELL_CREATE_RACE) vp[vn++] = format("creates %s%s",
		is_a_vowel((r_name+r_info[s_ptr->param].name)[0])?"an ":"a ",
		r_name+r_info[s_ptr->param].name);
	if (s_ptr->type == SPELL_CREATE_KIND) vp[vn++] = "creates gold";
	if (s_ptr->flags2 & (SF2_CREATE_STAIR)) vp[vn++] = "creates a staircase under you";
	if (s_ptr->type == SPELL_WARD_GLYPH) vp[vn++] = "creates a glyph of warding under you";
	if (s_ptr->type == SPELL_WARD_TRAP) vp[vn++] = format("creates %s%s next to you",
		is_a_vowel((f_name+f_info[s_ptr->param].name)[0])?"an ":"a ",
		f_name+f_info[s_ptr->param].name);
	if (s_ptr->flags1 & (SF1_STAR_ACQUIREMENT)) vp[vn++] = "creates several excellent, superb or special items";
	else if (s_ptr->flags1 & (SF1_ACQUIREMENT)) vp[vn++] = "creates an excellent, superb or special item";
	if (s_ptr->flags2 & (SF2_TELE_LEVEL)) vp[vn++] = "teleports you to an adjacent level";
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
		}

		/* Get the method */
		switch (method)
		{
			case RBM_AURA: p = "surrounds you with an aura";  t = "your enemies"; rad = 2; break;
			case RBM_SELF: t = "you";break;
			case RBM_ADJACENT: t = "all enemies adjacent to you"; break;
			case RBM_HANDS: t = "an adjacent target"; if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_MISSILE: t = "your enemies"; if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_BOLT_10: p = "creates a bolt"; t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BOLT: p = "creates a powerful bolt";  t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BEAM: p = "creates a beam"; t = "your enemies";if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BLAST: p = "creates an adjacent blast"; t = "your enemies"; d3 += level;break;
			case RBM_WALL: p = "creates a wall"; t = "your enemies"; if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BALL: p = "creates a ball"; t = "your enemies"; rad = 2; break;
			case RBM_CLOUD: p = "creates a cloud"; t = "your enemies"; rad = 2; d3 += level/2; break;
			case RBM_STORM: p = "creates a storm"; t = "your enemies"; rad = 3; break;
			case RBM_BREATH: p = "breathes";  t = "your enemies"; break;
			case RBM_AREA: p = "surrounds you with magic"; rad = (level/10)+2; break;
			case RBM_LOS: t = "all your enemies in line of sight"; break;
			case RBM_LINE: t = "one direction"; break;
			case RBM_AIM: t = "one target"; break;
			case RBM_ORB: p = "creates an orb"; t = "your enemies"; rad = (level < 30 ? 2 : 3); d3 += level/2; break;
			case RBM_CROSS: p = "surrounds you with a cross"; t = "your enemies"; break;
			case RBM_STRIKE: p = "strikes"; t = "an enemy"; if ((level > 5) && (d2)) d1+= (level-1)/5; break;
			case RBM_STAR: p = "surrounds you with a star"; t = "your enemies"; break;
			case RBM_SPHERE: p = "creates a sphere";  t = "your enemies";  rad = (level/10)+2;break;
			case RBM_PANEL: t = "the current panel"; break;
			case RBM_LEVEL: t = "the current level"; break;
		}


		/* Default technique */
		q = NULL;

		/* Default flavor */
		u = NULL;

		/* Get the effect */
		switch (effect)
		{
			case GF_NOTHING: q = "do"; u = "nothing"; break;
			case GF_ARROW: q= "hurt"; u = "with arrows"; break;
			case GF_MISSILE: q="blast"; u = "with magic missiles";break;
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
			case GF_AWAY_UNDEAD: q = "teleport"; u="away if undead";break;
			case GF_AWAY_EVIL: q = "teleport"; u="away if evil";break;
			case GF_AWAY_ALL: q = "teleport"; u = "away";break;
			case GF_TURN_UNDEAD: q = "turn"; u="if undead"; break;
			case GF_TURN_EVIL: q = "turn"; u="if evil"; break;
			case GF_TURN_ALL: q = "turn";break;
			case GF_DISP_UNDEAD: q = "dispel"; u="if undead"; break;
			case GF_DISP_EVIL: q = "dispel"; u="if evil"; break;
			case GF_DISP_ALL: q = "dispel";break;
			case GF_OLD_CLONE: q = "clone";break;
			case GF_OLD_POLY: q = "polymorph";break;
			case GF_OLD_HEAL: q = "heal";break;
			case GF_OLD_SPEED: q = "hasten";break;
			case GF_OLD_SLOW: q = "slow";break;
			case GF_OLD_CONF: q = "confuse";break;
			case GF_OLD_SLEEP: q = "send"; u="to sleep";break;
			case GF_OLD_DRAIN: q = "drain"; s="life from";break;
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
			case GF_FALL_SPIKE: q = "drop"; u="into a spiked pit";break;
			case GF_FALL_POIS: q = "drop"; u="into a poison spiked pit";break;
			case GF_BLIND:  q = "blind"; break;
			case GF_TERRIFY:	q = "terrify"; break;
			case GF_PARALYZE:       q = "paralyze"; break;
			case GF_LOSE_STR:       q = "reduce"; s="strength from"; break;
			case GF_LOSE_INT:       q = "reduce"; s="intelligence from"; break;
			case GF_LOSE_WIS:       q = "reduce"; s="wisdom from"; break;
			case GF_LOSE_DEX:       q = "reduce"; s="dexterity from"; break;
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

			/* Hack -- handle features */
			case GF_FEATURE:
			{

				char buf[80];
				cptr name = f_name + f_info[f_info[d3].mimic].name;

				q = "create";
				s = buf;
				sprintf(buf,"%s%s around",is_a_vowel(name[0])?"a ":"an ",name);
				d1 = 0;
				d2 = 0;
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
			if (rad) text_out (format( " of radius %d",rad));
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
		else if ((d1) && (d2) && (d3))
		{
			/* End */
			text_out(format(" for %dd%d+%d ",d1,d2,d3));
		}
		else if ((d1) && (d2))
		{
			/* End */
			text_out(format(" for %dd%d ",d1,d2));
		}
		else if (d3)
		{
			/* End */
			text_out(format(" for %d ",d3));
		}

		/* Get the effect */
		if ((d1 || d2 || d3) && (detail)) switch (effect)
		{
			case GF_LITE_WEAK: text_out("damage to monsters vulnerible to light"); break;
			case GF_KILL_WALL: text_out("damage to monsters vulnerible to rock remover"); break;
			case GF_HOLY_ORB: text_out("damage, doubled against evil monsters"); break;
			case GF_BLIND: text_out("damage, doubled against eye monsters"); break;
			case GF_TURN_UNDEAD: text_out("power"); break;
			case GF_TURN_EVIL: text_out("power");  break;
			case GF_TURN_ALL: text_out("power"); break;
			case GF_OLD_CLONE: text_out("power"); break;
			case GF_OLD_POLY: text_out("power"); break;
			case GF_OLD_HEAL: text_out("hit points"); break;
			case GF_AWAY_ALL: text_out("distance on average"); break;
			case GF_OLD_SPEED: text_out("power"); break;
			case GF_OLD_SLOW: text_out("power"); break;
			case GF_OLD_CONF: text_out("power"); break;
			case GF_OLD_SLEEP: text_out("power"); break;
			default: text_out("damage"); break;

		}
		r++;
	}

	return (introduced);

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
void spell_info(char *p, int spell, bool use_level)
{
	cptr q;

	spell_type *s_ptr = &s_info[spell];

	int m,n,rad;

	int level = spell_power(spell);

	if (!use_level) level = 0;

	/* Default */
	strcpy(p, "");

	/* Roll out the duration */
	if ((s_ptr->l_dice) && (s_ptr->l_side) && (s_ptr->l_plus))
	{
		/* End */
		strcpy(p,format(" dur %dd%d+%d",s_ptr->l_dice,s_ptr->l_side,s_ptr->l_plus));
	}
	else if ((s_ptr->l_dice) && (s_ptr->l_side))
	{
		/* End */
		strcpy(p,format(" dur %dd%d",s_ptr->l_dice,s_ptr->l_side));
	}
	else if (s_ptr->l_plus)
	{
		/* End */
		strcpy(p,format(" dur %d",s_ptr->l_plus));
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
		if (((s_ptr->l_dice) || (s_ptr->l_side) || (s_ptr->l_plus)) && (effect == GF_OLD_HEAL)) continue;

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
			case RBM_HANDS: if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_MISSILE: if ((level > 5) && (d2)) d1+= (level-1)/5;break;
			case RBM_BOLT_10: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BOLT: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BEAM: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BLAST: d3 += level;break;
			case RBM_WALL: if ((level > 8) && (d2)) d1+= (level-5)/4;break;
			case RBM_BALL: rad = 2; break;
			case RBM_CLOUD: rad = 2; d3 += level/2; break;
			case RBM_STORM: rad = 3; break;
			case RBM_AREA: rad = (level/10)+2; break;
			case RBM_ORB: rad = (level < 30 ? 2 : 3); d3 += level/2; break;
		}

		/* Default */
		q = NULL;

		/* Get the effect */
		if (d1 || d2 || d3) switch (effect)
		{
			case GF_TURN_UNDEAD: q="pow"; break;
			case GF_TURN_EVIL: q="pow";  break;
			case GF_TURN_ALL: q="pow"; break;
			case GF_OLD_CLONE: q="pow"; break;
			case GF_OLD_POLY: q="pow"; break;
			case GF_OLD_HEAL: q="heal"; break;
			case GF_AWAY_ALL: q="range"; break;
			case GF_OLD_SPEED: q="pow"; break;
			case GF_OLD_SLOW: q="pow"; break;
			case GF_OLD_CONF: q="pow"; break;
			case GF_OLD_SLEEP: q="pow"; break;
			default: q="dam"; break;
		}

		/* Display the damage */
		/* Roll out the damage */
		if ((d1) && (d2) && (d3))
		{
			/* End */
			strcpy(p,format(" %s %dd%d+%d ",q,d1,d2,d3));
		}
		else if ((d1) && (d2))
		{
			/* End */
			strcpy(p,format(" %s %dd%d ",q,d1,d2));
		}
		else if (d3)
		{
			/* End */
			strcpy(p,format(" %s %d ",q,d3));
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
static const o_flag_desc stat_flags_desc[A_MAX] =
{
	{ TR1_STR,	"strength" },
	{ TR1_INT,	"intelligence" },
	{ TR1_WIS,	"wisdom" },
	{ TR1_DEX,	"dexterity" },
	{ TR1_CON,	"constitution" },
	{ TR1_CHR,	"charisma" }
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */
static const o_flag_desc pval_flags1_desc[] =
{
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
 * Slays for weapons
 */
static const o_flag_desc slay_flags1_desc[] =
{
	{ TR1_SLAY_NATURAL,	"natural creatures" },
	{ TR1_SLAY_EVIL,	"evil" },
	{ TR1_SLAY_UNDEAD,	"undead" },
	{ TR1_SLAY_DEMON,	"demons" },
	{ TR1_SLAY_ORC,		"orcs" },
	{ TR1_SLAY_TROLL,	"trolls" },
	{ TR1_SLAY_GIANT,	"giants" },
	{ TR1_SLAY_DRAGON,	"dragons" }
};

/*
 * Executes for weapons
 */
static const o_flag_desc kill_flags1_desc[] =
{
	{ TR1_KILL_UNDEAD,	"undead" },
	{ TR1_KILL_DEMON,	"demons" },
	{ TR1_KILL_DRAGON,	"dragons" }
};


/*
 * Brands for weapons
 */
static const o_flag_desc brand_flags1_desc[] =
{
	{ TR1_BRAND_POIS,       "poison" },
	{ TR1_BRAND_ACID,       "acid" },
	{ TR1_BRAND_ELEC,       "electricity" },
	{ TR1_BRAND_FIRE,       "fire" },
	{ TR1_BRAND_COLD,       "cold" }
};

/*
 * Brands for weapons
 */
static const o_flag_desc brand_flags3_desc[] =
{
	{ TR3_IMPACT,   "earthquakes" }
};



/*
 * Sustain stats -  these are given their "own" line in the
 * spoiler file, mainly for simplicity
 */
static const o_flag_desc sustain_flags_desc[] =
{
	{ TR2_SUST_STR,   "strength" },
	{ TR2_SUST_INT,   "intelligence" },
	{ TR2_SUST_WIS,   "wisdom" },
	{ TR2_SUST_DEX,   "dexterity" },
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
 * Immunities
 */
static const o_flag_desc immune_flags2_desc[] =
{
	{ TR2_IM_ACID,  "acid" },
	{ TR2_IM_ELEC,  "electricity" },
	{ TR2_IM_FIRE,  "fire" },
	{ TR2_IM_COLD,  "cold" },
	{ TR2_RES_BLIND,	"blindness" },
	{ TR2_RES_SOUND,	"stunning" },
	{ TR2_RES_SHARD,	"cuts" },
	{ TR2_RES_CHAOS,	"hallucination" },
	{ TR2_RES_POIS,	 "the effects of poison" },
	{ TR2_RES_CONFU,	"the effects of confusion" }
};

/*
 * Immunities
 */
static const o_flag_desc immune_flags3_desc[] =
{
	{ TR3_FREE_ACT, "paralysis" },
	{ TR3_FREE_ACT, "magical slowness" }
};

/*
 * Miscellaneous magic given by an object's "flags3" field
 */
static const o_flag_desc misc_flags3_desc[] =
{
	{ TR3_SLOW_DIGEST,	"slow digestion" },
	{ TR3_FEATHER,		"feather falling" },
	{ TR3_LITE,	     "brighter light" },
	{ TR3_REGEN,		"regeneration" },
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
	{ TR3_TELEPORT,		"random teleportation" },
	{ TR3_AGGRAVATE,	"aggravation" },
	{ TR3_DRAIN_HP,		"health drain" },
	{ TR3_DRAIN_MANA,       "mana drain" },
	{ TR3_DRAIN_EXP,	"experience drain" }
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
static void obj_top(const object_type *o_ptr, bool real)
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
void screen_object(object_type *o_ptr, bool real)
{
	/* Flush messages */
	message_flush();

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Begin recall */
	Term_gotoxy(0, 1);

        /* Set obvious flags for an average item */
        object_obvious_flags(o_ptr);

	/* Actually display the item */
        if (o_ptr->ident & (IDENT_MENTAL)) list_object(o_ptr, OBJECT_FLAGS_FULL);
        else list_object(o_ptr, OBJECT_FLAGS_KNOWN);

	/* Display item name */
	obj_top(o_ptr, real);
}


/*
 * This function displays lists of properties
 */
static bool outlist(cptr header, const cptr *list, byte attr)
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

	/* End the current list */
	text_out_c(attr, ".  ");

	/* Something was printed */
	return (TRUE);
}

/* 
 * Create a spoiler file entry for an artifact.
 * We use this to list the flags.
 */
bool list_object_flags(u32b f1, u32b f2, u32b f3, int mode)
{
	const u32b all_stats = (TR1_STR | TR1_INT | TR1_WIS |
							TR1_DEX | TR1_CON | TR1_CHR);
	const u32b all_sustains = (TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS |
							   TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHR);

	bool anything = FALSE; /* Printed anything at all */

	cptr list[40];
	cptr *list_ptr;

	/* Brands */
	if (f1 || f3)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, brand_flags1_desc, list_ptr, N_ELEMENTS(brand_flags1_desc));
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
				anything |= outlist("It does not do extra damage from", list, TERM_SLATE);
				break;
		} 

	}

	/* Slays */
	if (f1)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, slay_flags1_desc, list_ptr, N_ELEMENTS(slay_flags1_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It is especially deadly against", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may be deadly against", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does no extra damage against", list, TERM_SLATE);
				break;
		} 
	}

	/* Execute */
	if (f1)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f1, kill_flags1_desc, list_ptr, N_ELEMENTS(kill_flags1_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It is a great bane of", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may be a great bane of", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It is not a great bane of", list, TERM_SLATE);
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

		/* Terminate the description list */
		*list_ptr = NULL;

		/* Print the Pval */
		if (!(*list == NULL))
		{
			byte attr = TERM_WHITE;
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				text_out_c(TERM_WHITE,"It modifies ");
				break;
			case LIST_FLAGS_MAY:
				text_out_c(TERM_L_WHITE,"It may modify ");
				attr= TERM_L_WHITE;
				break;
			case LIST_FLAGS_NOT:
				text_out_c(TERM_SLATE,"It does not modify ");
				attr = TERM_SLATE;
				break;
		} 
			anything |= outlist(NULL, list,attr);
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
	if ((f2) || (f3))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, resist_flags2_desc, list_ptr, N_ELEMENTS(resist_flags2_desc));
		list_ptr = spoiler_flag_aux(f3, resist_flags3_desc, list_ptr, N_ELEMENTS(resist_flags3_desc));

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
	if ((f2) || (f3))
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, immune_flags2_desc, list_ptr, N_ELEMENTS(immune_flags2_desc));
		list_ptr = spoiler_flag_aux(f3, immune_flags3_desc, list_ptr, N_ELEMENTS(immune_flags3_desc));

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


	/* Negative effects */
	if (f3)
	{
		list_ptr = list;
				
		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, bad_flags3_desc, list_ptr, N_ELEMENTS(bad_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		switch (mode)
		{
			case LIST_FLAGS_CAN:
				anything |= outlist("It burdens you with", list, TERM_WHITE);
				break;
			case LIST_FLAGS_MAY:
				anything |= outlist("It may burden you with", list, TERM_L_WHITE);
				break;
			case LIST_FLAGS_NOT:
				anything |= outlist("It does not burdern you with", list, TERM_SLATE);
				break;
		} 
	}

	return (anything);

}

/* 
 * Create a spoiler file entry for an artifact 
 */
void list_object(const object_type *o_ptr, int mode)
{
	int i;

	u32b f1, f2, f3;

	bool anything = FALSE;
	bool charge = FALSE;
	bool detail = FALSE;
	bool powers = FALSE;

	cptr p = NULL;

	s16b book[26];

	int num;

	bool random = (mode == OBJECT_FLAGS_RANDOM) ? TRUE : FALSE;
	bool spoil = (mode == OBJECT_FLAGS_FULL) ? TRUE : FALSE;

	/* Basic stats */
	if (!random) for (i = 0; object_group_tval[i]; i++)
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

	/* Extract the flags */
	object_flags_aux(mode, o_ptr, &f1, &f2, &f3);

	/* Display the flags */
	anything |= list_object_flags(f1, f2, f3, 1); 

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
		else if (object_known_p(o_ptr) || (o_ptr->discount == INSCRIP_CURSED))
		{
			text_out_c(TERM_RED, "It is cursed.  ");
			anything = TRUE;
		}
	}

	/* Extra powers */
	if (!random && ((object_aware_p(o_ptr)) || (spoil))
		&& (o_ptr->tval !=TV_MAGIC_BOOK) && (o_ptr->tval != TV_PRAYER_BOOK)
		&& (o_ptr->tval !=TV_SONG_BOOK) && (o_ptr->tval != TV_RUNESTONE))
	{
		int target = SPELL_TARGET_NORMAL;

		switch (o_ptr->tval)
		{
			case TV_POTION:
				p = "When quaffed, it ";
				target = SPELL_TARGET_SELF;
				break;
			case TV_SCROLL:
				p = "When read, it ";
				break;
			case TV_FOOD:
				p = "When eaten, it ";
				target = SPELL_TARGET_SELF;
				break;
			case TV_ROD:
				p = "When zapped, it ";
				charge = (k_info[o_ptr->k_idx].used > o_ptr->pval) || (o_ptr->ident & (IDENT_MENTAL)) || (spoil);
				break;
			case TV_STAFF:
				p = "When used, it ";
				break;
			case TV_WAND:
				p = "When aimed, it ";
				break;
			case TV_FLASK:
			case TV_LITE:
				p = "When thrown, it ";
				target = SPELL_TARGET_AIMED;
				break;
			case TV_ARROW:
			case TV_BOLT:
			case TV_SHOT:
				p = "When fired, it ";
				target = SPELL_TARGET_AIMED;
				break;
		}

		if ((f3 & TR3_ACTIVATE) && !(p))
		{
			p = "When activated, it ";
			charge = (k_info[o_ptr->k_idx].used > o_ptr->pval) || (o_ptr->ident & (IDENT_MENTAL)) || (spoil);
		}

		/* Artifacts */
		if ((o_ptr->name1) && ((object_known_p(o_ptr)) || (spoil)) && (f3 & TR3_ACTIVATE))
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			bool art_charge = (a_ptr->activated > a_ptr->randtime) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));
			bool art_detail = (a_ptr->activated > 8 * a_ptr->level) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));

			if (a_ptr->activation)
			{
				if (spell_desc(&s_info[a_ptr->activation],"When activated, it ",0,art_detail, 1))
				{
					anything = TRUE;

					if (art_charge) text_out(format(", recharging in %d + d%d turns.  ",a_ptr->time,a_ptr->randtime));
					else text_out(".  ");
				}

				p = NULL;
			}
		}

		if (p)
		{
			/* Fill the book with spells */
			fill_book(o_ptr,book,&num);

			/* Detailled explaination */
			detail = (k_info[o_ptr->k_idx].used > 4 * k_info[o_ptr->k_idx].level * num) || (spoil) || (o_ptr->ident & (IDENT_MENTAL));

			for (i = 0; i < num; i++)
			{
				powers |= spell_desc(&s_info[book[i]],(i==0)?p:", or ",0,detail, target);
			}

			if ((charge) && (powers)) text_out(format(", recharging in d%d turns.  ",o_ptr->pval));
			else if (powers) text_out(".  ");

			anything |= powers;
		}
	}

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (!spoil && !random && object_known_p(o_ptr) && ((o_ptr->xtra1) || artifact_p(o_ptr)) )
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
		anything |= list_object_flags(o_ptr->may_flags1, o_ptr->may_flags2, o_ptr->may_flags3, 2); 

                /* Equipment only */
                if (wield_slot(o_ptr) >= INVEN_WIELD)
                        /* Display the flags */
                        anything |= list_object_flags(o_ptr->not_flags1, o_ptr->not_flags2, o_ptr->not_flags3, 3); 
	}

        /* *Identified* object */
	else if (spoil) text_out("You know everything about this item.  ");


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
		spell_info(info, spell, FALSE);

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
void print_spells(const s16b *book, int num, int y, int x)
{
	int i, ii, spell, level;

	bool legible;

	cptr comment;

	char info[80];

	char out_val[160];

	byte line_attr;

	spell_type *s_ptr;

	spell_cast *sc_ptr;

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		/* Set casting details to null */
		sc_ptr = &(s_info[0].cast[0]);
		
		/* Get the spell index */
		spell = book[i];

		/* Get the spell info */
		s_ptr = &s_info[spell];

		legible = FALSE;

		for (ii=0;ii<MAX_SPELL_CASTERS;ii++)
		{
			if (s_ptr->cast[ii].class == p_ptr->pclass)
			{
				legible = TRUE;
				sc_ptr=&(s_ptr->cast[ii]);
			}
		}
			

		/* Skip illegible spells */
		if (!legible)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), s_name+s_info[0].name);
			c_prt(TERM_L_DARK, out_val, y + i + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, spell, TRUE);

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
				comment = " unknown";
				line_attr = TERM_L_BLUE;
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
			spell_level(spell), sc_ptr->mana, spell_chance(spell), comment);
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

	/* Display item name */
	obj_top(o_ptr, term_obj_real);

	/* Warriors are illiterate */
	if (c_info[p_ptr->pclass].spell_first > PY_MAX_LEVEL) return;

	browse = FALSE;

	for (i=0;i<z_info->s_max;i++)
	{
		s_ptr=&s_info[i];

		for (ii=0;ii<MAX_SPELL_APPEARS;ii++)
		{
			if ((s_ptr->appears[ii].tval == o_ptr->tval) &&
			    (s_ptr->appears[ii].sval == o_ptr->sval))
			{
				for (iii=0;iii<MAX_SPELL_CASTERS;iii++)
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

	/* Variant? */
	if (!variant_guess_id) return;

	/* Do not guess identified items */
	if (object_known_p(o_ptr)) return;

	/* Do not guess aware items */
	if (object_aware_p(o_ptr)) return;

	/* Check the ego item list */
	/* Hack -- exclude artifacts */
	if (!(o_ptr->discount == INSCRIP_SPECIAL) &&
	       !(o_ptr->discount == INSCRIP_TERRIBLE) &&
	       !(o_ptr->discount == INSCRIP_UNBREAKABLE))
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

		/* Must not have excepted powers */
		if (o_ptr->can_flags1 & n_ptr->not_flags1) continue;
		if (o_ptr->can_flags2 & n_ptr->not_flags2) continue;
		if (o_ptr->can_flags3 & n_ptr->not_flags3) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((o_ptr->can_flags1 & (1L<<ii)) && (e_ptr->flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (e_ptr->flags1 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((o_ptr->can_flags2 & (1L<<ii)) && (e_ptr->flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (e_ptr->flags2 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((cheat_lore) || !(e_ptr->xtra))
			{
				if ((o_ptr->can_flags3 & (1L<<ii)) && (e_ptr->flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (e_ptr->flags3 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=1;
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
	/* Hack -- exclude artifacts */
	if (!(o_ptr->discount == INSCRIP_SPECIAL) &&
	       !(o_ptr->discount == INSCRIP_TERRIBLE) &&
	       !(o_ptr->discount == INSCRIP_UNBREAKABLE))
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

		/* Must not have excepted powers */
		if (o_ptr->can_flags1 & ~(k_ptr->flags1)) continue;
		if (o_ptr->can_flags2 & ~(k_ptr->flags2)) continue;
		if (o_ptr->can_flags3 & ~(k_ptr->flags3)) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((o_ptr->can_flags1 & (1L<<ii)) && (k_ptr->flags1 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags1 & (1L<<ii)) && (k_ptr->flags1 & (1L<<ii))) score +=1;
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((o_ptr->can_flags2 & (1L<<ii)) && (k_ptr->flags2 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags2 & (1L<<ii)) && (k_ptr->flags2 & (1L<<ii))) score +=1;
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if ((o_ptr->can_flags3 & (1L<<ii)) && (k_ptr->flags3 & (1L<<ii))) score +=3;
			if ((o_ptr->may_flags3 & (1L<<ii)) && (k_ptr->flags3 & (1L<<ii))) score +=1;
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
	if (!(o_ptr->discount == INSCRIP_EXCELLENT) &&
	       !(o_ptr->discount == INSCRIP_SUPERB) &&
	       !(o_ptr->discount == INSCRIP_WORTHLESS))
		for (i = 1; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		object_lore *n_ptr = &a_list[i];

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

		/* Must not have excepted powers */
		if (o_ptr->can_flags1 & n_ptr->not_flags1) continue;
		if (o_ptr->can_flags2 & n_ptr->not_flags2) continue;
		if (o_ptr->can_flags3 & n_ptr->not_flags3) continue;

		/* Reset score */
		score = 0;

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((o_ptr->can_flags1 & (1L<<ii)) && (a_ptr->flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (a_ptr->flags1 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags1 & (1L<<ii)) && (n_ptr->can_flags1 & (1L<<ii))) score +=1;
			}
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((o_ptr->can_flags2 & (1L<<ii)) && (a_ptr->flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (a_ptr->flags2 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags2 & (1L<<ii)) && (n_ptr->can_flags2 & (1L<<ii))) score +=1;
			}					      
		}

		/* Award points on matching powers: 3 for have, 1 for may */
		for (ii=0;ii<32;ii++)
		{
			if (cheat_lore)
			{
				if ((o_ptr->can_flags3 & (1L<<ii)) && (a_ptr->flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (a_ptr->flags3 & (1L<<ii))) score +=1;
			}
			else
			{
				if ((o_ptr->can_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=3;
				if ((o_ptr->may_flags3 & (1L<<ii)) && (n_ptr->can_flags3 & (1L<<ii))) score +=1;
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
 */
void object_can_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3)
{
	u32b xf1 = 0, xf2 = 0, xf3 = 0;
	int i;

	/* Variant? */
	if (!variant_learn_id) return;

	/* Clear not flags */
	o_ptr->not_flags1 &= ~(f1);
	o_ptr->not_flags2 &= ~(f2);
	o_ptr->not_flags3 &= ~(f3);

	/* Clear may flags on all kit - include inventory */
	for (i = 0; i < INVEN_TOTAL+1; i++)
	{
		u32b if1 = o_ptr->may_flags1 & (f1);
		u32b if2 = o_ptr->may_flags2 & (f2);
		u32b if3 = o_ptr->may_flags3 & (f3);

		object_type *i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear may flags */
		i_ptr->may_flags1 &= ~(if1);
		i_ptr->may_flags2 &= ~(if2);
		i_ptr->may_flags3 &= ~(if3);
	}

	/* Mark can flags */
	o_ptr->can_flags1 |= (f1);
	o_ptr->can_flags2 |= (f2);
	o_ptr->can_flags3 |= (f3);

	/* Must be identified to continue */
	if (!object_known_p(o_ptr))
	{
		object_guess_name(o_ptr);

		return;
	}

	/* Hack -- Remove kind flags */
	/* This prevents Blades of Chaos 'tainting' ego items etc. */
	f1 &= ~(k_info[o_ptr->k_idx].flags1);
	f2 &= ~(k_info[o_ptr->k_idx].flags2);
	f3 &= ~(k_info[o_ptr->k_idx].flags3);

	/* Hack -- Remove 'user' enchanted hidden flags */
	/* This prevents runes and enchantment spells 'tainting' ego items */
	if (o_ptr->xtra1 >= OBJECT_XTRA_MIN_RUNES)
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
	}


	/* Artifact */
	if (o_ptr->name1)
	{
		object_lore *n_ptr = &a_list[o_ptr->name1];

		n_ptr->not_flags1 &= ~(f1);
		n_ptr->not_flags2 &= ~(f2);
		n_ptr->not_flags3 &= ~(f3);

		/* Fixed flags */
		n_ptr->can_flags1 |= (f1);
		n_ptr->can_flags2 |= (f2);
		n_ptr->can_flags3 |= (f3);

		/* Extra flags */
		n_ptr->may_flags1 |= xf1;
		n_ptr->may_flags2 |= xf2;
		n_ptr->may_flags3 |= xf3;

		/* Exclude fixed flags */
		n_ptr->may_flags1 &= ~(n_ptr->can_flags1);
		n_ptr->may_flags2 &= ~(n_ptr->can_flags2);
		n_ptr->may_flags3 &= ~(n_ptr->can_flags3);
	}

	/* Ego item */
	else if (o_ptr->name2)
	{
		object_lore *n_ptr = &e_list[o_ptr->name2];

		n_ptr->not_flags1 &= ~(f1);
		n_ptr->not_flags2 &= ~(f2);
		n_ptr->not_flags3 &= ~(f3);

		/* Fixed flags */
		n_ptr->can_flags1 |= (f1);
		n_ptr->can_flags2 |= (f2);
		n_ptr->can_flags3 |= (f3);

		/* Extra flags */
		n_ptr->may_flags1 |= xf1;
		n_ptr->may_flags2 |= xf2;
		n_ptr->may_flags3 |= xf3;

		/* Exclude fixed flags */
		n_ptr->may_flags1 &= ~(n_ptr->can_flags1);
		n_ptr->may_flags2 &= ~(n_ptr->can_flags2);
		n_ptr->may_flags3 &= ~(n_ptr->can_flags3);
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

	u32b nf1 = 0x0L;
	u32b nf2 = 0x0L;
	u32b nf3 = 0x0L;

	object_type *i_ptr;

	/* Check inventory may flags*/
	for (i = 0; i < INVEN_TOTAL+1; i++)
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
	}

	/* Check inventory may flags*/
	for (i = 0; i < INVEN_TOTAL+1; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		if ((f1 & (i_ptr->may_flags1)) || (f2 & (i_ptr->may_flags2))
			|| (f3 & (i_ptr->may_flags3)))
			update_slot_flags(i,f1 & (i_ptr->may_flags1),f2 & (i_ptr->may_flags2),f3 & (i_ptr->may_flags3));
	}
}

/*
 * Check if equipment has only 1 item that may flags
 */
static void equip_may_flags(u32b f1, u32b f2, u32b f3)
{
	int i, j;

	u32b if1 = 0x0L;
	u32b if2 = 0x0L;
	u32b if3 = 0x0L;

	u32b nf1 = 0x0L;
	u32b nf2 = 0x0L;
	u32b nf3 = 0x0L;

	object_type *i_ptr;

	/* Check inventory may flags*/
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
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
	}

	/* Only check passed flags */
	if1 &= (f1);
	if2 &= (f2);
	if3 &= (f3);

	if (!(if1) && !(if2) && !(if3)) return;

	/* Check equipment may flags*/
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		if ((if1 & (i_ptr->may_flags1)) || (if2 & (i_ptr->may_flags2))
			|| (if3 & (i_ptr->may_flags3)))
			update_slot_flags(i,if1 & (i_ptr->may_flags1),if2 & (i_ptr->may_flags2),if3 & (i_ptr->may_flags3));
	}
}


/*
 * Object does not have flags.
 */
void object_not_flags(object_type *o_ptr, u32b f1, u32b f2, u32b f3)
{
	/* Variant? */
	if (!variant_learn_id) return;

	/* No change */
	if (!(f1 & ~(o_ptr->not_flags1)) && !(f2 & ~(o_ptr->not_flags2)) && !(f3 & ~(o_ptr->not_flags3))) return;
	
	/* Mark not flags */
	o_ptr->not_flags1 |= (f1);
	o_ptr->not_flags2 |= (f2);
	o_ptr->not_flags3 |= (f3);

	/* Clear may flags */
	o_ptr->may_flags1 &= ~(f1);
	o_ptr->may_flags2 &= ~(f2);
	o_ptr->may_flags3 &= ~(f3);

	/* Clear can flags */
	o_ptr->can_flags1 &= ~(f1);
	o_ptr->can_flags2 &= ~(f2);
	o_ptr->can_flags3 &= ~(f3);

	/* Check inventory */
	inven_may_flags();

	/* Must be identified to continue */
	if (!object_known_p(o_ptr))
	{
		object_guess_name(o_ptr);

		return;
	}

	/* Artifact */
	if (o_ptr->name1) 
	{
		object_lore *n_ptr = &a_list[o_ptr->name1];

		n_ptr->not_flags1 |= f1;
		n_ptr->not_flags2 |= f2;
		n_ptr->not_flags3 |= f3;

		n_ptr->can_flags1 &= ~(f1);
		n_ptr->can_flags2 &= ~(f2);
		n_ptr->can_flags3 &= ~(f3);

	}

	/* Ego item */
	else if (o_ptr->name2)
	{
		object_lore *n_ptr = &e_list[o_ptr->name2];

		n_ptr->not_flags1 |= f1;
		n_ptr->not_flags2 |= f2;
		n_ptr->not_flags3 |= f3;

		n_ptr->can_flags1 &= ~(f1);
		n_ptr->can_flags2 &= ~(f2);
		n_ptr->can_flags3 &= ~(f3);
	}
}

/*
 * Object may have these flags. If only object in equipment
 * to do so, will have these flags. Use for object absorbtion.
 */
void object_may_flags(object_type *o_ptr, u32b f1,u32b f2,u32b f3)
{
	/* Variant? */
	if (!variant_learn_id) return;

	/* Clear bits with not flags */
	f1 &= ~(o_ptr->not_flags1);
	f2 &= ~(o_ptr->not_flags2);
	f3 &= ~(o_ptr->not_flags3);

	/* Clear bits with can flags */
	f1 &= ~(o_ptr->can_flags1);
	f2 &= ~(o_ptr->can_flags2);
	f3 &= ~(o_ptr->can_flags3);

	/* Mark may flags */
	o_ptr->may_flags1 |= (f1);
	o_ptr->may_flags2 |= (f2);
	o_ptr->may_flags3 |= (f3);

	/* Check the inventory */
	inven_may_flags();

	/* Must be identified to continue */
	if (!object_known_p(o_ptr))
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

	/* Clear may flags */
	o_ptr->may_flags1 = 0L;
	o_ptr->may_flags2 = 0L;
	o_ptr->may_flags3 = 0L;

	/* Clear may flags */
	o_ptr->not_flags1 = 0L;
	o_ptr->not_flags2 = 0L;
	o_ptr->not_flags3 = 0L;

	return;	

}

/*
 * Usage count for an object
 */
void object_usage(int slot)
{
	object_type *o_ptr;

	char o_name[80];

	/* Variant? */
	if (!variant_usage_id) return;

	if (slot >=0) o_ptr =&inventory[slot];
	else o_ptr=&o_list[0-slot];

	if (!o_ptr->k_idx) return;

	if ((o_ptr->usage)<MAX_SHORT) o_ptr->usage++;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Don't identify if fully known */
	if (o_ptr->ident & (IDENT_MENTAL)) return;

	/* Describe the object */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Hack --- know everything */
	if ((o_ptr->usage) == MAX_SHORT)
	{
		/* Describe what we know */
		msg_format("You feel you know everything about the %s you are %s.",o_name,describe_use(slot));

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		return;
	}

	/* Hack --- know most things */
	if (!object_known_p(o_ptr) && ((o_ptr->usage) > 5000))
	{

		/* Describe what we know */
		msg_format("You feel you recognize the %s you are %s.",o_name,describe_use(slot));

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

		return;

	}

	if (!object_bonus_p(o_ptr) &&  ((o_ptr->usage) > 500) && (((o_ptr->usage) % 100) == 0)
		&& (rand_int(100) <30))
	{

		/* Describe what we know */
		msg_format("You feel you know more about the %s you are %s.",o_name,describe_use(slot));

		/* Identify the kind */
		object_aware(o_ptr);

		/* Mark the item as partially known */
		object_bonus(o_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	}

}

/*
 * Slot holds an object with these flags. Inform the player.
 */
void update_slot_flags(int slot, u32b f1, u32b f2, u32b f3)
{
	char o_name[80];

	object_type *i_ptr;

	/* Variant? */
	if (!variant_learn_id) return;

	/* Get the item */
	if (slot >=0) i_ptr = &inventory[slot];
	else i_ptr= &o_list[0-slot];

	/* Update the object */
	object_can_flags(i_ptr,f1,f2,f3);

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
	list_object_flags(f1, f2, f3, 1);

	(void)inkey();
	
	/* Load screen */
	screen_load();

}

/*
 * Equipment must have these flags.
 * Note with ALLOW_OBJECT_INFO_MORE, this is
 * an efficiency bottleneck.
 */
void equip_can_flags(u32b f1,u32b f2,u32b f3)
{
	u32b nf1;
	u32b nf2;
	u32b nf3;

	int i;

	object_type *i_ptr;

	/* Variant? */
	if (!variant_learn_id) return;

	/* Hack --- exclude player flags */
	player_flags(&nf1,&nf2,&nf3);

	f1 &= ~(nf1);
	f2 &= ~(nf2);
	f3 &= ~(nf3);

	/* Hack --- exclude temporary effect flags */
	if (p_ptr->tim_infra) f1 &= ~(TR1_INFRA);
	if (p_ptr->hero || p_ptr->shero) f2 &= ~(TR2_RES_FEAR);
	if (p_ptr->tim_invis) f3 &= ~(TR3_SEE_INVIS);
	if (p_ptr->blessed) f3 &= ~(TR3_HOLD_LIFE);

	/* Exclude known flags */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear bits with can flags */
		f1 &= ~(i_ptr->can_flags1);
		f2 &= ~(i_ptr->can_flags2);
		f3 &= ~(i_ptr->can_flags3);

	}

	/* Nothing unknown */
	if (!f1 && !f2 && !f3) return;

	/* Check for flags */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		u32b if1 = f1;
		u32b if2 = f2;
		u32b if3 = f3;

		bool guess = FALSE;

		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		/* Clear bits with not flags */
		if1 &= ~(i_ptr->not_flags1);
		if2 &= ~(i_ptr->not_flags2);
		if3 &= ~(i_ptr->not_flags3);

		/* Do we guess again ? */
		guess |= (if1 & ~(i_ptr->may_flags1)) || (if2 & ~(i_ptr->may_flags2)) || (if3 & ~(i_ptr->may_flags3));

		/* Mark may flags */
		i_ptr->may_flags1 |= (if1);
		i_ptr->may_flags2 |= (if2);
		i_ptr->may_flags3 |= (if3);

		/* Must be identified to continue */
		if ((guess) && (!object_known_p(i_ptr)))
		{
			object_guess_name(i_ptr);
		}
	}

	/* Check inventory */
	equip_may_flags(f1, f2, f3);
}

/*
 * Equipment does not have these flags
 */
void equip_not_flags(u32b f1,u32b f2,u32b f3)
{
	int i;

	object_type *i_ptr;

	/* Variant? */
	if (!variant_learn_id) return;

	/* Mark equipment with not flags*/
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		object_not_flags(i_ptr,f1,f2,f3);
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

	object_type *i_ptr;

	if (!(f1) && !(f2) && !(f3)) return;

	/* Clear equipment may flags*/
	for (i = 0; i < INVEN_TOTAL+1; i++)
	{
		i_ptr = &inventory[i];

		/* Skip non-objects */
		if (!i_ptr->k_idx) continue;

		i_ptr->may_flags1 &= ~(f1);
		i_ptr->may_flags2 &= ~(f2);
		i_ptr->may_flags3 &= ~(f3);
	}
}

