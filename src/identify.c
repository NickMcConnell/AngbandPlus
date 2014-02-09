/*
 * File: identify.c
 * Purpose: Object identification and knowledge routines
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2009 Brian Bull, Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "game-event.h"


/** Time last item was wielded */
s32b object_last_wield;




/*** Knowledge accessor functions ***/


/**
 * \returns whether an object counts as "known" due to EASY_KNOW status
 */
bool easy_know(const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	if (k_ptr->aware && (k_ptr->k_flags3 & TR3_EASY_KNOW))
		return TRUE;
	else
		return FALSE;
}

/**
 * \returns whether an object should be treated as fully known (e.g. ID'd)
 */
bool object_is_known(const object_type *o_ptr)
{
	return (o_ptr->ident & IDENT_KNOWN) || easy_know(o_ptr) ||
			(o_ptr->ident & IDENT_STORE);
}

/**
 * \returns whether the object is known to be an artifact
 */
bool object_is_known_artifact(const object_type *o_ptr)
{
	return (o_ptr->ident & IDENT_INDESTRUCT) ||
			(artifact_p(o_ptr) && object_was_sensed(o_ptr));
}


/**
 * \returns whether the object has been sensed with pseudo-ID
 */
bool object_was_sensed(const object_type *o_ptr)
{
	return o_ptr->ident & IDENT_SENSE ? TRUE : FALSE;
}

/**
 * \returns whether the player is aware of the object's flavour
 */
bool object_flavor_is_aware(const object_type *o_ptr)
{
	return k_info[o_ptr->k_idx].aware;
}



/* Flags on an object that do not affect the player, belongs in some .h file */
#define TR2_AFFECTS_OBJ_ONLY (TR2_INSTA_ART | TR2_EASY_KNOW | TR2_HIDE_TYPE | TR2_SHOW_MODS | TR2_IGNORE_ACID | TR2_IGNORE_ELEC | TR2_IGNORE_FIRE | TR2_IGNORE_COLD)


/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
int value_check_aux1(const object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Good "armor" bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD_STRONG);

	/* Good "weapon" bonus */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD_STRONG);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static int value_check_aux2(const object_type *o_ptr)
{
	/* Cursed items (all of them) */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items (all of them) */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Artifacts -- except cursed/broken ones */
	if (artifact_p(o_ptr)) return (INSCRIP_GOOD_WEAK);

	/* Ego-Items -- except cursed/broken ones */
	if (ego_item_p(o_ptr)) return (INSCRIP_GOOD_WEAK);

	/* Good armor bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD_WEAK);

	/* Good weapon bonuses */
	if (o_ptr->to_h + o_ptr->to_d > 0) return (INSCRIP_GOOD_WEAK);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}
/*
 * Returns TRUE if this object can be pseudo-ided.
 */
bool can_be_pseudo_ided(const object_type *o_ptr)
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
		case TV_DRAG_SHIELD:
      	{
			return (TRUE);
      	}
    	case TV_LIGHT:
      	{
      		if (game_mode == GAME_NPPMORIA) return (FALSE);

			if (o_ptr->sval == SV_LIGHT_LANTERN)
	  		return (TRUE);
			break;
      	}
  	}
  	return (FALSE);
}



/*
 * Sense the inventory
 */
void sense_inventory(void)
{
	int i;

	int plev = p_ptr->lev;

	bool heavy = ((cp_ptr->flags & CF_PSEUDO_ID_HEAVY) ? TRUE : FALSE);

	int feel;

	object_type *o_ptr;

	char o_name[80];


	/*** Check for "sensing" ***/

	/* No sensing when confused */
	if (p_ptr->timed[TMD_CONFUSED]) return;

	if (cp_ptr->flags & CF_PSEUDO_ID_IMPROV)
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev * plev + cp_ptr->sense_div)))
			return;
	}
	else
	{
		if (0 != rand_int(cp_ptr->sense_base / (plev + cp_ptr->sense_div)))
			return;
	}


	/*** Sense everything ***/

	/* Check everything */
	for (i = 0; i < ALL_INVEN_TOTAL; i++)
	{

		int squelch = SQUELCH_NO;

		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Sensing the swap weapon is kind of cheating */
		if (adult_swap_weapons && (i == INVEN_SWAP_WEAPON))	continue;

		/* Skip non-sense machines */
		if (!can_be_pseudo_ided(o_ptr)) continue;

		/* It already has a discount or special inscription */
		if ((o_ptr->discount > 0) &&
		    (o_ptr->discount != INSCRIP_INDESTRUCTIBLE)) continue;

		/* It has already been sensed, do not sense it again */
		if (o_ptr->ident & (IDENT_SENSE)) continue;

		/* It is known, no information needed */
		if (object_known_p(o_ptr)) continue;

		/* 80% failure on inventory items */
		if ((i < INVEN_WIELD) && (0 != rand_int(5))) continue;

		/* Indestructible objects are either excellent or terrible */
		if (o_ptr->discount == INSCRIP_INDESTRUCTIBLE)
			heavy = TRUE;

		/* Check for a feeling */
		feel = (heavy ? value_check_aux1(o_ptr) : value_check_aux2(o_ptr));

		/* Skip non-feelings */
		if (!feel) continue;

		/* Squelch it? */
		if (i < INVEN_WIELD)
		{
			squelch = squelch_itemp(o_ptr, feel, FALSE);
		}

		/* Stop everything */
		disturb(0, 0);

		/* Get an object description */
		object_desc(o_name, sizeof(o_name), o_ptr, ODESC_FULL);

		/* Message (equipment) */

		sound(MSG_PSEUDOID);
		if (i >= INVEN_WIELD)
		{
			msg_format("You feel the %s (%c) you are %s %s %s...",
			           o_name, index_to_label(i), describe_use(i),
			           ((o_ptr->number == 1) ? "is" : "are"),
			           inscrip_text[feel - INSCRIP_NULL]);
		}

		/* Message (inventory) */
		else
		{
			msg_format("You feel the %s (%c) in your pack %s %s...  %s",
			           o_name, index_to_label(i),
			           ((o_ptr->number == 1) ? "is" : "are"),
			           inscrip_text[feel - INSCRIP_NULL], squelch_to_label(squelch));
		}

		/* Sense the object */
		o_ptr->discount = feel;

		if (feel == INSCRIP_AVERAGE)
		{
			/* Identify it fully */
			object_aware(o_ptr);
			object_known(o_ptr);
		}

		else
		{
			/* The object has been "sensed" */
			o_ptr->ident |= (IDENT_SENSE);
		}

		/* Squelch it if necessary */
		do_squelch_item(squelch, i, o_ptr);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

		/* Redraw stuff */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);
	}
}


