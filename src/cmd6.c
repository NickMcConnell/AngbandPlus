/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007 Andrew Sidwell
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
#include "cmds.h"

/*
 * This file includes code for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of wands, staffs,
 * and rods.  Note the overly paranoid warning about potential pack
 * overflow, which allows the player to use and drop a stacked item.
 *
 * In all "unstacking" scenarios, the "used" object is "carried" as if
 * the player had just picked it up.  In particular, this means that if
 * the use of an item induces pack overflow, that item will be dropped.
 *
 * For simplicity, these routines induce a full "pack reorganization"
 * which not only combines similar items, but also reorganizes various
 * items to obey the current "sorting" method.  This may require about
 * 400 item comparisons, but only occasionally.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */

/** standard chance of using an item or device */
int
player_type::item_chance(int lev) const
{
	/* Base chance of success */
	int chance = p_ptr->skills[SKILL_DEVICE];

	/* Confusion hurts skill */
	if (p_ptr->timed[TMD_CONFUSED]) chance /= 2;

	/* High level objects are harder */
	chance -= MIN(50,lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	return chance;
}
