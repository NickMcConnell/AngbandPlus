/* File: obj_kind.c */


#include "angband.h"


bool object_is_food(object_type *o_ptr)
{
	if (o_ptr->tval == TV_FOOD) return TRUE;

	/* Assume not */
	return FALSE;
}

bool object_is_potion(object_type *o_ptr)
{
	return (k_info[o_ptr->k_idx].tval == TV_POTION);
}

bool object_is_readable(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_TAROT) || (o_ptr->tval == TV_SCRATCH_CARD) || (o_ptr->tval == TV_SCROLL) || (o_ptr->name1 == ART_FIRECREST)) return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Hook to determine if an object is activatable
 */
bool object_is_activate(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	/* Not known */
	if (!object_is_known(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Check activation flag */
	if (have_flag(flgs, TR_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/*
 * Hook to determine if an object is useable
 */
bool object_is_useable(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	/* Ammo */
	if (o_ptr->tval == p_ptr->tval_ammo)
		return (TRUE);

	/* Useable object */
	switch (o_ptr->tval)
	{
		case TV_SPIKE:
		case TV_STAFF:
		case TV_WAND:
		case TV_ROD:
		case TV_SCROLL:
		case TV_POTION:
		case TV_FOOD:
		{
			return (TRUE);
		}

		default:
		{
			int i;

			/* Not known */
			if (!object_is_known(o_ptr)) return (FALSE);

			/* HACK - only items from the equipment can be activated */
			for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
			{
				if (&inventory[i] == o_ptr)
				{
					/* Extract the flags */
					object_flags(o_ptr, flgs);

					/* Check activation flag */
					if (have_flag(flgs, TR_ACTIVATE)) return (TRUE);
				}
			}
		}
	}

	/* Assume not */
	return (FALSE);
}


bool object_is_shoukinkubi(object_type *o_ptr)
{
	int i;

	if (astral_mode) return FALSE;
	if (p_ptr->today_mon > 0 && (streq(r_name + r_info[o_ptr->pval].name, r_name + r_info[today_mon].name))) return TRUE;
	if (o_ptr->pval == MON_ZEBRA) return TRUE;
	for (i = 0; i < MAX_KUBI; i++)
		if (o_ptr->pval == kubi_r_idx[i]) break;
	if (i < MAX_KUBI) return TRUE;
	return FALSE;
}

/*
 * Check if an object is artifact
 */
bool object_is_artifact(object_type *o_ptr)
{
	if (object_is_fixed_artifact(o_ptr) || o_ptr->art_name) return TRUE;

	return FALSE;
}


bool object_is_runeweapon(object_type *o_ptr)
{
	if (!o_ptr->k_idx) return FALSE;
	return (k_info[o_ptr->k_idx].gen_flags & TRG_RUNEWEAPON) ? TRUE : FALSE;
}

bool object_is_snapdragon_runeweapon(object_type *o_ptr)
{
	if (object_is_runeweapon(o_ptr) && o_ptr->art_name)
	{
		if (o_ptr->xtra3 && (o_ptr->xtra3 <= runeweapon_num)) return TRUE;
		else if (p_ptr->is_dead & DEATH_SNAP_DRAGON)
		{
			if (o_ptr == &runeweapon_list[0].weapon) return TRUE;
		}
	}

	return FALSE;
}

bool object_is_astral_runeweapon(object_type *o_ptr)
{
	return (astral_mode && object_is_runeweapon(o_ptr) && o_ptr->art_name && (o_ptr->xtra3 == 1));
}

/*
 * Equipment including all wearable objects and ammo
 */
bool object_is_equipment(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_STONE: case TV_TAROT: case TV_SCRATCH_CARD:
	case TV_BULLET: case TV_ROUND: case TV_SHELL: case TV_ROCKET:
	case TV_ARROW: case TV_BOLT: case TV_BOW:
	case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD: 
	case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
	case TV_SHIELD: case TV_CLOAK:
	case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
	case TV_LITE: case TV_AMULET: case TV_RING: case TV_CARD: case TV_TRUMP:
		return TRUE;

	default:
		return FALSE;
	}
}

/*
 * Rare weapons/aromors
 * including Blade of Chaos, Dragon armors, etc.
 */
bool object_is_rare(object_type *o_ptr)
{
	switch(o_ptr->tval)
	{
	case TV_TAROT:
		return TRUE;

	case TV_BOW:
		if (o_ptr->sval == SV_SNIPER_RIFLE ||
			o_ptr->sval == SV_SHOTGUN ||
			o_ptr->sval == SV_ROCKET_LAUNCHER) return TRUE;
		break;

	case TV_HAFTED:
		if (o_ptr->sval == SV_MACE_OF_DISRUPTION ||
		    o_ptr->sval == SV_WIZSTAFF) return TRUE;
		break;

	case TV_POLEARM:
		if (o_ptr->sval == SV_DEATH_SCYTHE) return TRUE;
		break;

	case TV_SWORD:
		if (o_ptr->sval == SV_YOUTOU ||
		    o_ptr->sval == SV_DARK_SWORD ||
		    o_ptr->sval == SV_DIAMOND_EDGE ||
		    o_ptr->sval == SV_MITHRIL_SWORD) return TRUE;
		break;

	case TV_SHIELD:
		if (o_ptr->sval == SV_DRAGON_SHIELD ||
		    o_ptr->sval == SV_MIRROR_SHIELD) return TRUE;
		break;

	case TV_HELM:
		if (o_ptr->sval == SV_DRAGON_HELM) return TRUE;
		break;

	case TV_BOOTS:
		if (o_ptr->sval == SV_PAIR_OF_DRAGON_GREAVE) return TRUE;
		break;

	case TV_CLOAK:
		if (o_ptr->sval == SV_BOLMARKAN_CLOAK ||
		    o_ptr->sval == SV_CLOAK_OF_IVORY_TOWER ||
		    o_ptr->sval == SV_SHADOW_CLOAK ||
		    o_ptr->sval == SV_SIR_COAT) return TRUE;
		break;

	case TV_GLOVES:
		if (o_ptr->sval == SV_SET_OF_DRAGON_GLOVES) return TRUE;
		break;

	case TV_SOFT_ARMOR:
		if (o_ptr->sval == SV_DRAGON_LEATHER_ARMOR ||
		    o_ptr->sval == SV_KUROSHOUZOKU ||
		    o_ptr->sval == SV_ROBE_DECOLLETE ||
		    o_ptr->sval == SV_ROBE_MONTANTE ||
		    o_ptr->sval == SV_SWALLOW_TAILED_COAT ||
		    o_ptr->sval == SV_FROCK_COAT) return TRUE;
		break;

	case TV_HARD_ARMOR:
		if (o_ptr->sval == SV_DRAGON_SCALE_MAIL) return TRUE;
		break;

	case TV_AMULET:
		if (o_ptr->sval == SV_AMULET_ALIGNMENT ||
		    o_ptr->sval == SV_AMULET_THE_MAGI ||
		    o_ptr->sval == SV_AMULET_REFLECTION ||
		    o_ptr->sval == SV_AMULET_RESISTANCE ||
		    o_ptr->sval == SV_AMULET_TELEPATHY ||
		    o_ptr->sval == SV_AMULET_FOL ||
		    o_ptr->sval == SV_AMULET_OHN ||
		    o_ptr->sval == SV_AMULET_SOL ||
		    o_ptr->sval == SV_AMULET_VAN) return TRUE;
		break;

	case TV_RING:
		if (o_ptr->sval == SV_RING_SHOTS ||
		    o_ptr->sval == SV_RING_SUSTAIN ||
		    o_ptr->sval == SV_RING_SPEED ||
		    o_ptr->sval == SV_RING_DEC_MANA ||
		    o_ptr->sval == SV_RING_RES_DISENCHANT ||
		    o_ptr->sval == SV_RING_LORDLY ||
		    o_ptr->sval == SV_RING_ATTACKS) return TRUE;
		break;

	case TV_CARD:
		return TRUE;

	case TV_TRUMP:
		return TRUE;

	case TV_FOOD:
		if (o_ptr->sval == SV_FOOD_INC_STR ||
		    o_ptr->sval == SV_FOOD_INC_INT ||
		    o_ptr->sval == SV_FOOD_INC_WIS ||
		    o_ptr->sval == SV_FOOD_INC_DEX ||
		    o_ptr->sval == SV_FOOD_INC_CON ||
		    o_ptr->sval == SV_FOOD_INC_CHR ||
		    o_ptr->sval == SV_FOOD_AUGMENTATION) return TRUE;
		break;

	default:
		break;
	}

	/* Any others are not "rare" objects. */
	return FALSE;
}


/*
 * Check if an object is weapon or armour (but not arrow, bolt, or shot)
 */
bool object_is_weapon_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "weapon"
 */
bool object_is_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_BOW:
		case TV_BOLT:
		case TV_ARROW:
		case TV_ROCKET:
		case TV_SHELL:
		case TV_ROUND:
		case TV_BULLET:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


bool object_is_melee_weapon(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "armour"
 */
bool object_is_armour(object_type *o_ptr)
{
	if (o_ptr->name2 == EGO_NO_ELEM) return (FALSE);
	switch (o_ptr->tval)
	{
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_BOOTS:
		case TV_GLOVES:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


bool object_is_body_armour(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_CLOAK:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


bool object_is_corpse(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_CORPSE)) return TRUE;

	/* Assume not */
	return FALSE;
}


/*
 * Hook to specify "ammo"
 */
bool object_is_ammo(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_ARROW:
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "bow", except guns
 */
bool object_is_bow(object_type *o_ptr)
{
	if (o_ptr->tval == TV_BOW)
	{
		switch (o_ptr->sval)
		{
		case SV_SHORT_BOW:
		case SV_LONG_BOW:
		case SV_BOWGUN:
		case SV_CROSSBOW:
		case SV_RUNEBOW:
			return TRUE;
		}
	}

	return FALSE;
}

bool object_is_metal(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	object_flags(o_ptr, flgs);
	if (have_flag(flgs, TR_BALDAR)) return TRUE;

	switch (o_ptr->tval)
	{
	case TV_CHUNK:
	case TV_SPIKE:
	case TV_BULLET:
	case TV_ROUND:
	case TV_SHELL:
	case TV_ROCKET:
	case TV_DIGGING:
	case TV_POLEARM:
	case TV_SWORD:
		return TRUE;

	case TV_HARD_ARMOR:
		switch (o_ptr->sval)
		{
		case SV_METAL_SCALE_MAIL:
		case SV_CHAIN_MAIL:
		case SV_DOUBLE_CHAIN_MAIL:
		case SV_SPLINT_MAIL:
		case SV_FULL_PLATE_ARMOUR:
		case SV_MITHRIL_SCALE_MAIL:
		case SV_MITHRIL_CHAIN_MAIL:
		case SV_MITHRIL_PLATE_MAIL:
			return TRUE;
		}
		break;

	case TV_CHEST:
		switch (o_ptr->sval)
		{
		case 2: /* Small iron chest */
		case 3: /* Small steel chest */
		case 6: /* Large iron chest */
		case 7: /* Large steel chest */
			return TRUE;
		}
		break;

	case TV_STATUE:
		switch (o_ptr->sval)
		{
		case SV_IRON_STATUE:
		case SV_COPPER_STATUE:
		case SV_SILVER_STATUE:
		case SV_GOLDEN_STATUE:
			return TRUE;
		}
		break;

	case TV_ARROW:
		switch (o_ptr->sval)
		{
		case 2: /* Seeker Arrow */
			return TRUE;
		}
		break;

	case TV_BOLT:
		if (o_ptr->k_idx > 80) return TRUE;
		break;

	case TV_BOW:
		switch (o_ptr->sval)
		{
		case SV_PISTOL:
		case SV_ASSAULT_RIFLE:
		case SV_SNIPER_RIFLE:
		case SV_SHOTGUN:
		case SV_ROCKET_LAUNCHER:
		case SV_RUNEBOW:
		case SV_RUNEGUN:
			return TRUE;
		}
		break;

	case TV_HAFTED:
		switch (o_ptr->sval)
		{
		case SV_HALT_HAMMER:
		case SV_PAUA_HAMMER:
		case SV_FLAIL:
		case SV_LEAD_FILLED_MACE:
		case SV_FAN:
		case SV_GREAT_HAMMER:
		case SV_MACE_OF_DISRUPTION:
		case SV_GROND:
		case SV_RUNEHAMMER:
		case SV_RUNEWHIP:
		case SV_RUNESTAFF:
		case SV_RUNEFAN:
			return TRUE;
		}
		break;

	case TV_BOOTS:
		switch (o_ptr->sval)
		{
		case SV_PAIR_OF_STEEL_BOOTS:
			return TRUE;
		}
		break;

	case TV_GLOVES:
		switch (o_ptr->sval)
		{
		case SV_SET_OF_CESTI:
		case SV_SET_OF_POWER_GLOVES:
		case SV_SET_OF_MITHRIL_GLOVES:
			return TRUE;
		}
		break;

	case TV_HELM:
		switch (o_ptr->sval)
		{
		case SV_METAL_CAP:
		case SV_IRON_HELM:
		case SV_STEEL_HELM:
			return TRUE;
		}
		break;

	case TV_CROWN:
		switch (o_ptr->sval)
		{
		case SV_IRON_CROWN:
		case SV_GOLDEN_CROWN:
		case SV_KING:
			return TRUE;
		}
		break;

	case TV_SHIELD:
		switch (o_ptr->sval)
		{
		case SV_TOWER_SHIELD:
		case SV_KITE_SHIELD:
		case SV_KNIGHT_SHIELD:
		case SV_MIRROR_SHIELD:
			return TRUE;
		}
		break;

	case TV_LITE:
		switch (o_ptr->sval)
		{
		case SV_LITE_WATCH:
			return TRUE;
		}
		break;

	default:
		break;
	}

	/* Assume not */
	return FALSE;
}

bool object_is_gun(object_type *o_ptr)
{
	if (o_ptr->tval == TV_BOW)
	{
		switch (o_ptr->sval)
		{
		case SV_PISTOL:
		case SV_ASSAULT_RIFLE:
		case SV_SNIPER_RIFLE:
		case SV_SHOTGUN:
		case SV_ROCKET_LAUNCHER:
		case SV_RUNEGUN:
			return TRUE;
		}
	}

	return FALSE;
}


/*
 * Hook to determine if an object is contertible in an arrow/bolt
 */
bool object_is_convertible(object_type *o_ptr)
{
	if((o_ptr->tval==TV_JUNK) || (o_ptr->tval==TV_SKELETON)) return TRUE;

	if ((o_ptr->tval == TV_CORPSE) && (o_ptr->sval == SV_SKELETON)) return TRUE;
	/* Assume not */
	return (FALSE);
}


/*
 * Hook for "get_item()".  Determine if something is rechargable.
 */
bool object_is_recharge(object_type *o_ptr)
{
	/* Recharge staffs */
	if (o_ptr->tval == TV_STAFF) return TRUE;

	/* Recharge wands */
	if (o_ptr->tval == TV_WAND) return TRUE;

	/* Hack -- Recharge rods */
	if (o_ptr->tval == TV_ROD) return TRUE;

	/* Nope */
	return FALSE;
}

bool object_is_ego_creatable(object_type *o_ptr)
{
	if (object_is_runeweapon(o_ptr)) return FALSE;

	/* Analyze type */
	switch (o_ptr->tval)
	{
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
	case TV_AMULET:
	case TV_RING:
	case TV_LITE:
		if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DIAMOND_EDGE)) return FALSE;
		if ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_DEATH_SCYTHE)) return FALSE;
		if ((o_ptr->tval == TV_AMULET) && (o_ptr->sval == SV_AMULET_EMPTY)) return FALSE;
		if ((o_ptr->tval == TV_RING) && (o_ptr->sval == SV_RING_EMPTY)) return FALSE;
		if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_EMPTY)) return FALSE;

		if (object_is_known(o_ptr) && (object_is_artifact(o_ptr) || object_is_ego(o_ptr) || o_ptr->xtra3))
			return FALSE;
		else return TRUE;
	}

	return FALSE;
}


