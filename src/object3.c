/* File: object3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Hack -- determine if an item is "wearable" 
 */
bool wearable_p(const object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_CLOAK:
		case TV_BODY_ARMOR:
		case TV_DRAG_ARMOR:
		{
			if (p_ptr->shape == SHAPE_HARPY) return FALSE;
			else if (p_ptr->shape == SHAPE_ANGEL) return FALSE;
			else return TRUE;
		}
		case TV_BOW:
		case TV_DIGGING:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_RING:
		case TV_GLOVES:
		case TV_SHIELD:
		{
			if (p_ptr->shape == SHAPE_HARPY) return FALSE;
			else return TRUE;
		}
		case TV_BOOTS:
		{
			if (p_ptr->shape == SHAPE_HARPY) return FALSE;
			else if (p_ptr->shape == SHAPE_NAGA) return FALSE;
			else if (p_ptr->shape == SHAPE_FAUN) return FALSE;
			else return TRUE;
		}
		case TV_HEADGEAR:
		{
			if (p_ptr->shape == SHAPE_FAUN) return FALSE;
			else return TRUE;
		}
		case TV_LITE:
		case TV_LITE_SPECIAL:
		case TV_AMULET:
		case TV_MUSIC:
		{
			return TRUE;
		}
	}

	/* Nope */
	return FALSE;
}

/*
 * Hack -- determine if an item is a "weapon" 
 */
bool weapon_p(const object_type *o_ptr)
{
	/* Valid "tval" codes */
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_BLUNT:
		case TV_POLEARM:
		{
			return TRUE;
		}
	}

	/* Nope */
	return FALSE;
}

/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_BLUNT:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return INVEN_WIELD;
		}

		case TV_BOW:
		{
			return INVEN_BOW;
		}

		case TV_RING:
		{
			int slot;

			cptr q, s;
			
			object_type *i_ptr = &inventory[INVEN_RIGHT];
			object_type *j_ptr = &inventory[INVEN_LEFT];
	
			/* Right hand is free - pick it first */
			if (!i_ptr->k_idx) return INVEN_RIGHT;

			/* Left hand is free - pick it */
			if (!j_ptr->k_idx) return INVEN_LEFT;

			/* 
			 * Both hands are full - time to see if we can decide where to put it ourselves 
			 * If not obvious, prefer left hand to right hand for swapping.
			 */

			/* Both rings are cursed, choose arbitrarily (will fail later anyway) */
			if (cursed_p(i_ptr) && cursed_p(j_ptr)) return INVEN_LEFT;

			/* Left ring is cursed, but right one isn't */
			if (!cursed_p(i_ptr) && cursed_p(j_ptr)) return INVEN_RIGHT;

			/* Right ring is cursed, but left one isn't */
			if (cursed_p(i_ptr) && !cursed_p(j_ptr)) return INVEN_LEFT;

			/* Rings are of the same type, choice might be easy */
			if ((i_ptr->sval == j_ptr->sval) && object_known_p(i_ptr) && object_known_p(j_ptr))
			{
				switch (i_ptr->sval)
				{
					case SV_RING_PROTECTION:
					case SV_RING_LIGHTNING:
					case SV_RING_ACID:
					case SV_RING_FLAMES:
					case SV_RING_ICE:
					{
						/* Prefer the ring with lower ac bonus */
						if (i_ptr->to_a < j_ptr->to_a) return INVEN_RIGHT;
						else return INVEN_LEFT;
					}
					case SV_RING_ACCURACY:
					{
						{
							/* Prefer the ring with lower bonuses */
							if (i_ptr->to_h < j_ptr->to_h)
								return INVEN_RIGHT;
							else return INVEN_LEFT;
						}
					}
					default:
					{
						/* 
						 * For most cases, just prefer the ring with lower pval
						 * Note that this works fine for rings with no pval
						 */
						if (i_ptr->pval < j_ptr->pval) return INVEN_RIGHT;
						else return INVEN_LEFT;
					}
				}
			}
			
			/* Since we haven't chosen automatically, choose interactively */

			/* Restrict the choices */
			item_tester_tval = TV_RING;

			/* Choose a ring from the equipment only */
			q = "Replace which ring? ";
			s = "Oops.";
			if (!get_item(&slot, q, s, USE_EQUIP)) return 0;
			
			return slot;
		}

		case TV_AMULET:
		{
			return INVEN_NECK;
		}

		case TV_LITE:
		case TV_LITE_SPECIAL:
		{
			return INVEN_LITE;
		}

		case TV_DRAG_ARMOR:
		case TV_BODY_ARMOR:
		{
			return INVEN_BODY;
		}

		case TV_CLOAK:
		{
			return INVEN_OUTER;
		}

		case TV_SHIELD:
		{
			return INVEN_ARM;
		}

		case TV_HEADGEAR:
		{
			return INVEN_HEAD;
		}

		case TV_GLOVES:
		{
			return INVEN_HANDS;
		}

		case TV_BOOTS:
		{
			return INVEN_FEET;
		}
	}

	/* No slot available */
	return 0;
}

/*
 * Obtain an object's color
 */
byte object_attr(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	
	/* Flavored objects */
	if (k_ptr->flavor) return misc_to_attr[k_ptr->flavor];

	/* Prefixes */
	if (allow_prefix_colors)
	{
		if (weapon_p(o_ptr))
		{
			if (o_ptr->pfx_idx && (wpx_info[o_ptr->pfx_idx].d_attr)) 
			{
				return wpx_info[o_ptr->pfx_idx].d_attr;
			}
		}

		if ((o_ptr->tval == TV_BODY_ARMOR))
		{	
			if (o_ptr->pfx_idx && (apx_info[o_ptr->pfx_idx].d_attr))
			{
				return apx_info[o_ptr->pfx_idx].d_attr;
			}
		}
	}

	/* Base object color */
	return k_ptr->x_attr;
}

/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	bool mental = (o_ptr->ident & IDENT_MENTAL);

	object_kind *k_ptr;

	/* Check artifact knowledge status */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		if (artifact_known_p(a_ptr)) mental = TRUE;
	}

	if (mode != OBJECT_INFO_FULL)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = 0L;

		/* Must be identified */
		if (!object_known_p(o_ptr)) 
		{
			/* Hack - know the light radius of lite items */
			if (o_ptr->a_idx) (*f3) = (a_info[o_ptr->a_idx].flags3 & (TR3_LITE_MASK));
			else (*f3) = (k_info[o_ptr->k_idx].flags3 & (TR3_LITE_MASK)); 

			return;
		}
	}

	if (mode != OBJECT_INFO_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		if (mode == OBJECT_INFO_FULL)
		{
			/* Artifact */
			if (o_ptr->a_idx)
			{
				artifact_type *a_ptr = &a_info[o_ptr->a_idx];

				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
			}
		}

		/* Ego-item */
		if (o_ptr->e_idx)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

			(*f1) |= e_ptr->flags1;
			(*f2) |= e_ptr->flags2;
			(*f3) |= e_ptr->flags3;
		}

		if (mode == OBJECT_INFO_KNOWN)
		{
			if (o_ptr->e_idx)
			{
				ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

				/* Obvious flags (pval) */
				(*f1) = (e_ptr->flags1);
			}

			/* Obvious artifact flags */
			if (o_ptr->a_idx)
			{
				artifact_type *a_ptr = &a_info[o_ptr->a_idx];

				/* Obvious flags (pval, elemental ignores) */
				(*f1) = (a_ptr->flags1 & (TR1_PVAL_MASK));

				(*f3) = (a_ptr->flags3 & (TR3_IGNORE_ELEM | TR3_IGNORE_NON_ELEM));

				/* Always know the radius of light sources */
				(*f3) |= (a_ptr->flags3 & (TR3_LITE_MASK));

			}
		}
	}

	if (mode != OBJECT_INFO_FULL)
	{
		/* Need full knowledge or spoilers */
		if (!mental) return;

		/* Artifact */
		if (o_ptr->a_idx)
		{
			artifact_type *a_ptr = &a_info[o_ptr->a_idx];

			(*f1) = a_ptr->flags1;
			(*f2) = a_ptr->flags2;
			(*f3) = a_ptr->flags3;

			if (mode == OBJECT_INFO_RANDOM)
			{
				/* Hack - remove ALL flags, until random artifacts return */
				(*f1) = 0;
				(*f2) = 0;
				(*f3) = 0;
			}
		}
	}

	/* Extra powers */
	switch (o_ptr->xtra1)
	{
		case OBJECT_XTRA_TYPE_SUSTAIN:
		{
			(*f1) |= (OBJECT_XTRA_BASE_SUSTAIN << o_ptr->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			(*f3) |= (OBJECT_XTRA_BASE_POWER << o_ptr->xtra2);
			break;
		}
	}
}

/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_INFO_FULL, o_ptr, f1, f2, f3);
}

/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_INFO_KNOWN, o_ptr, f1, f2, f3);
}

/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_random(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_INFO_RANDOM, o_ptr, f1, f2, f3);
}

/*
 * Obtain the resistances from an item 
 */
byte object_resist(const object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

	int i = 0;

	/* Random abilities for ego items */
	if (o_ptr->e_idx)
	{
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_HIGH_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			i = 25;
	}

	return (k_ptr->res[res_type] + a_ptr->res[res_type] + e_ptr->res[res_type] + i);
}

/*
 * Obtain the resistances from an item 
 */
byte object_resist_known(const object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

	int res = 0;

	if (object_known_p(o_ptr)) 
	{
		res = k_ptr->res[res_type] + e_ptr->res[res_type];
		if (artifact_known_p(a_ptr)) res += a_ptr->res[res_type];
		/* Known random ego-item resists */
		if (o_ptr->e_idx && (o_ptr->ident & IDENT_MENTAL))
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_HIGH_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			res += 25;
	}

	return (res);
}

/*
 * Obtain the slays from an item 
 *
 * Slays from ego items, prefixes, and base item types are additively cumulative.
 */
void weapon_slays(const object_type *o_ptr, byte *slays)
{
	int j, counter;
	byte slay;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];
	weapon_prefix_type *wpx_ptr = &wpx_info[o_ptr->pfx_idx];

	/* Extract slays */
	for (j = 0 ; j < SL_MAX ; j++)
	{
		/* Reset counter */
		counter = 0;

		slay = k_ptr->slays[j];
		if (slay) counter++;

		if (o_ptr->a_idx && a_ptr->slays[j]) 
		{
			slay += a_ptr->slays[j];
			counter++;
		}

		if (o_ptr->e_idx && e_ptr->slays[j]) 
		{
			slay += e_ptr->slays[j];
			counter++;
		}
		
		if (weapon_p(o_ptr) && wpx_ptr->slays[j])
		{
			slay += wpx_ptr->slays[j];
			counter++;
		}

		if (counter > 1) slay -= 10 * (counter - 1);
		slays[j] = slay;
	}
}

/*
 * Obtain the slays from an item 
 *
 * Slays from ego items, prefixes, and base item types are additively cumulative.
 */
void weapon_slays_known(const object_type *o_ptr, byte *slays)
{
	int j, counter;
	byte slay;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];
	weapon_prefix_type *wpx_ptr = &wpx_info[o_ptr->pfx_idx];

	/* Extract slays */
	for (j = 0 ; j < SL_MAX ; j++)
	{
		/* Reset counter */
		counter = 0;

		slay = k_ptr->slays[j];
		if (slay) counter++;

		if (weapon_p(o_ptr) && wpx_ptr->slays[j])
		{
			slay += wpx_ptr->slays[j];
			counter++;
		}

		if (object_known_p(o_ptr))
		{
			if (o_ptr->a_idx && artifact_known_p(a_ptr) && a_ptr->slays[j]) 
			{
				slay += a_ptr->slays[j];
				counter++;
			}

			if (o_ptr->e_idx && e_ptr->slays[j]) 
			{
				slay += e_ptr->slays[j];
				counter++;
			}
		}

		if (counter > 1) slay -= 10 * (counter - 1);
		slays[j] = slay;
	}
}

/* 
 * Actual weight (including prefixes)
 */
s16b object_weight(const object_type *o_ptr)
{
	int wgt;

	if (o_ptr->a_idx) wgt = a_info[o_ptr->a_idx].weight;
	else wgt = k_info[o_ptr->k_idx].weight;

	if (weapon_p(o_ptr))
	{
		if (o_ptr->pfx_idx) 
		{
			return ((wgt * wpx_info[o_ptr->pfx_idx].weight) / 100);
		}
	}
	if ((o_ptr->tval == TV_BODY_ARMOR))
	{
		if (o_ptr->pfx_idx) 
		{
			return ((wgt * apx_info[o_ptr->pfx_idx].weight) / 100);
		}
	}
	return wgt;
}

/* 
 * Actual to-hit (including prefixes)
 */
s16b object_to_h(const object_type *o_ptr)
{
	if (weapon_p(o_ptr))
	{
		if (o_ptr->pfx_idx) 
		{
			return (o_ptr->to_h + wpx_info[o_ptr->pfx_idx].to_h);
		}
	}

	return o_ptr->to_h;
}

/*
 * Hack - Increase damage dice according to a percentile increase in average damage.
 *
 * This function goes through all the possible damage dice that achieve the correct 
 * average, and chooses the one that most closely matches the maximum damage of the
 * original weapon (appropriately increased). That gives as close an approximation to
 * the original damage distribution as possible.
 */
static byte damage_dice_aux(const object_type *o_ptr, bool sides)
{
	int perc = 100;
	int i, j, k;
	int dd, ds;
	int dam_avg, new_avg;
	int ideal_max;
	int diff;

	/* Handle artifacts */
	if (o_ptr->a_idx)
	{
		dd = a_info[o_ptr->a_idx].dd;
		ds = a_info[o_ptr->a_idx].ds;
	}
	else
	{
		dd = k_info[o_ptr->k_idx].dd;
		ds = k_info[o_ptr->k_idx].ds;

		/* Non-artifact prefix, handle percentile */
		if (o_ptr->pfx_idx) perc = wpx_info[o_ptr->pfx_idx].d_perc;

		/* Ego item, may modify percentile */
		if (o_ptr->e_idx && (e_info[o_ptr->e_idx].d_perc)) 
			perc += e_info[o_ptr->e_idx].d_perc - 100;
	}

	/* Need to modify according to percentile */
	if (perc != 100)
	{
		dam_avg = ((ds + 1) * 5 * dd);
		new_avg = (dam_avg * perc) / 100;
		ideal_max = (10 * dd * ds * perc) / 100;

		/* Handle low percentages - never require less than 1d1 */
		if (new_avg < 10) new_avg = 10;
		if (ideal_max < 10) ideal_max = 10;

		diff = ideal_max;

		/* Find all damage dice which give (approximately) the right average */
		for (i = 1; i < new_avg + 1; i++)
		{
			j = ((new_avg * 2) / i); 
			
			/* Round j */
			if ((j % 10) >= 5) j = (j / 10) + 1;
			else j /= 10;

			j -= 1;

			k = i * j * 10;

			/* Now, try to get as close to the ideal new max as possible */
			if (k == ideal_max)
			{
				/* We found it */
				dd = i;
				ds = j;

				break;
			}

			if ((k > ideal_max) && ((k - ideal_max) < diff))
			{
				diff = k - ideal_max;

				dd = i;
				ds = j;
			}

			if ((k < ideal_max) && ((ideal_max - k) < diff))
			{
				diff = ideal_max - k;

				dd = i;
				ds = j;
			}

			/* No 0-sided dice */
			if (j < 1) break;
		}
	}

	if (sides) return ds;
	else return dd;
}

/* 
 * Actual damage dice (including prefixes)
 */
byte object_dd(const object_type *o_ptr)
{
	byte dd;

	/* Hack - blasted items */
	if ((o_ptr->e_idx == EGO_BLASTED) || (o_ptr->e_idx == EGO_SHATTERED)) return 0;

	if (o_ptr->a_idx) dd = a_info[o_ptr->a_idx].dd;
	else dd = k_info[o_ptr->k_idx].dd;

	if (weapon_p(o_ptr)) dd = damage_dice_aux(o_ptr, FALSE);

	return dd;
}

/* 
 * Actual damage sides (including prefixes)
 */
byte object_ds(const object_type *o_ptr)
{
	byte ds;

	/* Hack - blasted items */
	if ((o_ptr->e_idx == EGO_BLASTED) || (o_ptr->e_idx == EGO_SHATTERED)) return 0;

	if (o_ptr->a_idx) ds = a_info[o_ptr->a_idx].ds;
	else ds = k_info[o_ptr->k_idx].ds;

	if (weapon_p(o_ptr)) ds = damage_dice_aux(o_ptr, TRUE);

	return ds;
}

/* 
 * Actual armor class (including prefixes)
 */
s16b object_ac(const object_type *o_ptr)
{
	int ac;

	/* Hack - blasted items */
	if ((o_ptr->e_idx == EGO_BLASTED) || (o_ptr->e_idx == EGO_SHATTERED)) return 0;

	if (o_ptr->a_idx) ac = a_info[o_ptr->a_idx].ac;
	else ac = k_info[o_ptr->k_idx].ac;

	if (o_ptr->tval == TV_BODY_ARMOR)
	{
		if (o_ptr->pfx_idx) 
		{
			ac += apx_info[o_ptr->pfx_idx].ac;

			return ((ac >= 0) ? ac : 0);
		}
	}

	return ac;
}

/* 
 * Actual armor class (including prefixes)
 */
byte object_material(const object_type *o_ptr)
{
	if (weapon_p(o_ptr))
	{
		if ((o_ptr->pfx_idx) && (wpx_info[o_ptr->pfx_idx].actual_material))
		{
			return wpx_info[o_ptr->pfx_idx].actual_material;
		}
	}
	if ((o_ptr->tval == TV_BODY_ARMOR))
	{
		if ((o_ptr->pfx_idx) && (apx_info[o_ptr->pfx_idx].actual_material))
		{
			return apx_info[o_ptr->pfx_idx].actual_material;
		}
	}

	return k_info[o_ptr->k_idx].material;;
}

/*
 * Get the might of a bow
 */
byte bow_might(const object_type *o_ptr)
{
	/* Use the "damage dice" from the bow as base range XXX XXX */
	int might = k_info[o_ptr->k_idx].dd;

	u32b f1, f2, f3;

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (f1 & TR1_MIGHT) might += (o_ptr->pval);

	/* Archery proficiency gives bonuses to might */
	might += p_ptr->archery;

	/* Boundary check */
	if (might < 1) might = 1;

	return (byte)might;
}

/*
 * Get the range of a bow
 */
byte bow_range(const object_type *o_ptr)
{
	/* Use the "damage sides" from the bow as base range XXX XXX */
	int range = k_info[o_ptr->k_idx].ds;

	u32b f1, f2, f3;

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (f1 & TR1_RANGE) range += (o_ptr->pval);

	/* Boundary check */
	if (range < 1) range = 1;

	return (byte)range;
}

/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
bool object_hates_acid(const object_type *o_ptr)
{
	/* Analyze the type */
	switch (object_material(o_ptr))
	{
		/* Stuff that is always damaged by acid */
		case MATERIAL_PAPER:
		case MATERIAL_CLOTH:
		case MATERIAL_BONE:
		case MATERIAL_LEATHER:
		case MATERIAL_STEEL:
		case MATERIAL_METAL_OTHER:
		case MATERIAL_STONE:
		case MATERIAL_S_WOOD:
		{
			return (TRUE);
		}

		/* Other metals, very rarely */
		case MATERIAL_METAL_CHARGED:
		{
			if (rand_int(10) == 0) return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate electricity?
 */
bool object_hates_elec(const object_type *o_ptr)
{
	switch (object_material(o_ptr))
	{
		/* Various metals (not steel)*/
		case MATERIAL_METAL_CHARGED:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate fire?
 */
bool object_hates_fire(const object_type *o_ptr)
{
	switch (object_material(o_ptr))
	{
		/* Stuff that is always damaged by fire */
		case MATERIAL_PAPER:
		case MATERIAL_CLOTH:
		case MATERIAL_S_WOOD:
		case MATERIAL_H_WOOD:
		case MATERIAL_BONE:
		case MATERIAL_FOOD:
		{
			return (TRUE);
		}

		/* Leather, rarely */
		case MATERIAL_LEATHER:
		{
			if (rand_int(3) == 0) return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate cold?
 */
bool object_hates_cold(const object_type *o_ptr)
{
	switch (object_material(o_ptr))
	{
		/* Liquids freeze */
		case MATERIAL_LIQUID:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate rust?
 */
bool object_hates_rust(const object_type *o_ptr)
{
	switch (object_material(o_ptr))
	{
		/* Steel rusts */
		case MATERIAL_STEEL:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Does a given object (usually) hate rust?
 */
bool object_hates_rot(const object_type *o_ptr)
{
	switch (object_material(o_ptr))
	{
		/* Wood and food */
		case MATERIAL_S_WOOD:
		case MATERIAL_H_WOOD:
		case MATERIAL_FOOD:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}
