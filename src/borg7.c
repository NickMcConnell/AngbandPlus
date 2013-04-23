/* File: borg7.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


/* Verbosity is good (sometimes) -RML */
#define EXTRA_SHOP_MESSAGES


/*
 * This file handles various high level inventory related goals.
 *
 * Problems:
 *   Use "time stamps" (and not "random" checks) for several routines,
 *   including "kill junk" and "wear stuff", and maybe even "free space".
 *   But be careful with the "free space" routine, wear stuff first.
 *   Make sure nothing is "destroyed" if we do not do them every turn.
 *   Consider some special routines in stores (and in the home).
 *
 * Hack -- We should perhaps consider wearing "harmless" items into empty
 * slots when in the dungeon, to allow rings/amulets to be brought back up
 * to town to be sold.
 *
 * We should take account of possible combinations of equipment.  This may
 * be a potentially expensive computation, but could be done occasionally.
 * It is important to use a "state-less" formula to allow the exchange to
 * be spread over multiple turns.
 *
 * Hack -- We should attempt to only collect non-discounted items, at least
 * for the "expensive" slots, such as books, since we do not want to lose
 * value due to stacking.  We seem to sell the non-discounted items first,
 * and to buy the discounted items first, since they are cheap.  Oh well,
 * we may just be stuck with using discounted books.  Unless we actually
 * do correct "combining" in the simulations, and reward free slots.  Ick!
 *
 * We really need a better "twitchy" function.  XXX XXX XXX
 *
 * We need a better "flee this level" function.  XXX XXX XXX
 *
 * We need to stockpile possible useful items at home.  XXX XXX XXX
 *
 * Perhaps we could simply maintain a list of abilities that we might need
 * at some point, such as the ability to identify, and simply allow the Borg
 * to "sell" items to the home which satisfy this desire for "abilities".
 * XXX XXX XXX
 *
 * Also, we should probably attempt to track the "best" item in the home for
 * each equipment slot, using some form of heuristic, and reward that item
 * based on its power, so that the Borg would always have a "backup" item
 * in case of disenchantment.  XXX XXX XXX
 *
 * Also, we could reward equipment based on possible enchantment, up to the
 * maximal amount available in the home, which would induce item switching
 * when the item could be enchanted sufficiently.  XXX XXX XXX
 *
 * Fleeing from fast spell-casters is probably not a very smart idea, nor is
 * fleeing from fast monsters, nor is attempting to clear a room full of fast
 * moving breeding monsters, such as lice.
 *
 * Resting near monsters which drain mana or multiply is a bad idea.
 */



/*
 * Hack -- importance of the various "level feelings"
 * Try to explore the level for at least this many turns
 */
static s16b value_feeling[] =
{
	500,
	8000,
	8000,
	6000,
	4000,
	2000,
	1000,
	800,
	600,
	400,
	200,
	0
};




/*
 * Determine if an item is "probably" worthless
 *
 * This (very heuristic) function is a total hack, designed only to prevent
 * a very specific annoying situation described below.
 *
 * Note that a "cautious" priest (or low level mage/ranger) will leave town
 * with a few identify scrolls, wander around dungeon level 1 for a few turns,
 * and use all of the scrolls on leather gloves and broken daggers, and must
 * then return to town for more scrolls.  This may repeat indefinitely.
 *
 * The problem is that some characters (priests, mages, rangers) never get an
 * "average" feeling about items, and have no way to keep track of how long
 * they have been holding a given item for, so they cannot even attempt to
 * gain knowledge from the lack of "good" or "cursed" feelings.  But they
 * cannot afford to just identify everything they find by using scrolls of
 * identify, because, in general, some items are, on average, "icky", and
 * not even worth the price of a new scroll of identify.
 *
 * Even worse, the current algorithm refuses to sell un-identified items, so
 * the poor character will throw out all his good stuff to make room for crap.
 *
 * This function simply examines the item and assumes that certain items are
 * "icky", which is probably a total hack.  Perhaps we could do something like
 * compare the item to the item we are currently wearing, or perhaps we could
 * analyze the expected value of the item, or guess at the likelihood that the
 * item might be a blessed, or something.
 *
 * Currently, only characters who do not get "average" feelings are allowed
 * to decide that something is "icky", others must wait for an "average"
 * feeling.
 */
bool borg_item_icky(auto_item *item)
{
	/* Mega-Hack -- allow "icky" items */
	if ((b_ptr->pclass != CLASS_PRIEST) &&
	    (b_ptr->pclass != CLASS_RANGER) &&
	    (b_ptr->pclass != CLASS_MAGE)) return (FALSE);

	/* Broken dagger/sword, Filthy rag */
	if (((item->tval == TV_SWORD) && (item->sval == SV_BROKEN_DAGGER)) ||
	    ((item->tval == TV_SWORD) && (item->sval == SV_BROKEN_SWORD)) ||
	    ((item->tval == TV_SOFT_ARMOR) && (item->sval == SV_FILTHY_RAG)))
	{
		return (TRUE);
	}

	/* Dagger, Sling */
	if (((item->tval == TV_SWORD) && (item->sval == SV_DAGGER)) ||
	    ((item->tval == TV_BOW) && (item->sval == SV_SLING)))
	{
		return (TRUE);
	}

	/* Cloak, Robe */
	if (((item->tval == TV_CLOAK) && (item->sval == SV_CLOAK)) ||
	    ((item->tval == TV_SOFT_ARMOR) && (item->sval == SV_ROBE)))
	{
		return (TRUE);
	}

	/* Leather Gloves */
	if ((item->tval == TV_GLOVES) &&
	    (item->sval == SV_SET_OF_LEATHER_GLOVES))
	{
		return (TRUE);
	}

	/* Hack -- Diggers */
	if (item->tval == TV_DIGGING) return (TRUE);

	/* Assume okay */
	return (FALSE);
}





/*
 * Use things in a useful, but non-essential, manner
 */
bool borg_use_things(void)
{
	int i;


	/* Experience drained */
	if (borg_base_fix_exp)
	{
		/* Attempt to restore */
		if (borg_prayer(6, 4) ||
		    borg_quaff_potion(SV_POTION_RESTORE_EXP))
		{
			return (TRUE);
		}
	}


	/* Quaff potions of "increase" stat if "needed" */
    if ((!borg_stat_maxed[A_STR] &&
	     borg_quaff_potion(SV_POTION_INC_STR)) ||
        (!borg_stat_maxed[A_INT] &&
	     borg_quaff_potion(SV_POTION_INC_INT)) ||
        (!borg_stat_maxed[A_WIS] &&
	     borg_quaff_potion(SV_POTION_INC_WIS)) ||
        (!borg_stat_maxed[A_DEX] &&
	     borg_quaff_potion(SV_POTION_INC_DEX)) ||
        (!borg_stat_maxed[A_CON] &&
	     borg_quaff_potion(SV_POTION_INC_CON)) ||
        (!borg_stat_maxed[A_CHR] &&
	     borg_quaff_potion(SV_POTION_INC_CHR)))
	{
		return (TRUE);
	}

	/* Strength drained */
	if (borg_base_fix_stat[A_STR])
	{
		/* Attempt to restore strength */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_STR))
		{
			return (TRUE);
		}
	}

	/* Intelligence drained */
	if (borg_base_fix_stat[A_INT])
	{
		/* Attempt to restore intelligence */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_INT))
		{
			return (TRUE);
		}
	}

	/* Wisdom drained */
	if (borg_base_fix_stat[A_WIS])
	{
		/* Attempt to restore wisdom */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_WIS))
		{
			return (TRUE);
		}
	}

	/* Dexterity drained */
	if (borg_base_fix_stat[A_DEX])
	{
		/* Attempt to restore dexterity */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_DEX))
		{
			return (TRUE);
		}
	}

	/* Constitution drained */
	if (borg_base_fix_stat[A_CON])
	{
		/* Attempt to restore constitution */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_CON))
		{
			return (TRUE);
		}
	}

	/* Chrisma drained */
	if (borg_base_fix_stat[A_CHR])
	{
		/* Attempt to restore charisma */
		if (borg_prayer(6, 3) ||
		    borg_quaff_potion(SV_POTION_RES_CHR))
		{
			return (TRUE);
		}
	}


	/* Use some items right away */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Process "force" items */
		switch (item->tval)
		{
			case TV_POTION:
			{
				/* Check the scroll */
				switch (item->sval)
				{
					case SV_POTION_ENLIGHTENMENT:
					{
						/* Never quaff in town */
						if (!b_ptr->depth) break;
					}

					case SV_POTION_AUGMENTATION:
					case SV_POTION_EXPERIENCE:
					case SV_POTION_INC_STR:
					case SV_POTION_INC_INT:
					case SV_POTION_INC_WIS:
					case SV_POTION_INC_DEX:
					case SV_POTION_INC_CON:
					case SV_POTION_INC_CHR:
					{
						/* Try quaffing the potion */
						if (borg_quaff_potion(item->sval)) return (TRUE);
					}
				}

				break;
			}

			case TV_SCROLL:
			{
				/* Hack -- check Blind/Confused */
				if (borg_base_is_blind || borg_base_is_confused) break;

				/* Handle darkness XXX XXX XXX */

				/* Check the scroll */
				switch (item->sval)
				{
					case SV_SCROLL_MAPPING:
					case SV_SCROLL_DETECT_TRAP:
					case SV_SCROLL_DETECT_DOOR:
					case SV_SCROLL_ACQUIREMENT:
					case SV_SCROLL_STAR_ACQUIREMENT:
					{
						/* Never read these in town */
						if (!b_ptr->depth) break;

						/* Try reading the scroll */
						if (borg_read_scroll(item->sval)) return (TRUE);
					}
				}

				break;
			}
		}
	}


	/* Eat food */
	if (borg_base_is_hungry)
	{
		/* Attempt to satisfy hunger */
		if (borg_spell(2, 0) ||
		    borg_prayer(1, 5) ||
		    borg_eat_food(SV_FOOD_RATION))
		{
			return (TRUE);
		}
	}


	/* Nothing to do */
	return (FALSE);
}



/*
 * Refuel, call lite, detect traps/doors/walls, etc
 *
 * Note that we refuel whenever our lite starts to get low.
 *
 * Note that we detect traps/doors/walls at least once in each
 * panel, as soon as possible after entering a new panel.
 *
 * Note that we call lite whenever the current grid is dark, and
 * all the grids touching the current grid diagonally are known
 * floors, which catches all rooms, including "checkerboard" rooms,
 * and only occasionally calls lite in corridors, and then only once.
 *
 * Note that we also sometimes call lite whenever we are using a
 * lantern or artifact lite, and when all of the grids in the box
 * of grids containing the maximal torch-lit region (the 5x5 or 7x7
 * region centered at the player) are non-glowing floor grids, and
 * when at least one of them is known to be "dark".  This catches
 * most of the "probable rooms" before the standard check succeeds.
 *
 * We use the special "SELF" messages to "borg_react()" to delay the
 * processing of "detection" and "call lite" until we know if it has
 * worked or not.
 */
bool borg_check_lite(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int y, x, i, r;

	int qy, qx;

	bool do_lite;

	bool do_trap;
	bool do_door;
	bool do_wall;

	bool do_lite_aux = FALSE;


	/* Never in town */
	if (!b_ptr->depth) return (FALSE);

	/* Never when blind or confused or hallucinating */
	if (borg_base_is_blind || borg_base_is_confused || borg_base_is_image) return (FALSE);

	/* Handle darkness XXX XXX XXX */


	/* Extract the panel */
	qy = b_ptr->wy / 11;
	qx = b_ptr->wx / 33;


	/* Start */
	do_trap = FALSE;

	/* Determine if we need to detect traps */
	if (!borg_detect_trap[qy+0][qx+0]) do_trap = TRUE;
	if (!borg_detect_trap[qy+0][qx+1]) do_trap = TRUE;
	if (!borg_detect_trap[qy+1][qx+0]) do_trap = TRUE;
	if (!borg_detect_trap[qy+1][qx+1]) do_trap = TRUE;

	/* Hack -- check traps every few turns anyway */
	if (!borg_when_detect_traps || (borg_time - borg_when_detect_traps >= 183)) do_trap = TRUE;


	/* Start */
	do_door = FALSE;

	/* Determine if we need to detect doors */
	if (!borg_detect_door[qy+0][qx+0]) do_door = TRUE;
	if (!borg_detect_door[qy+0][qx+1]) do_door = TRUE;
	if (!borg_detect_door[qy+1][qx+0]) do_door = TRUE;
	if (!borg_detect_door[qy+1][qx+1]) do_door = TRUE;

	/* Hack -- check doors every few turns anyway */
	if (!borg_when_detect_doors || (borg_time - borg_when_detect_doors >= 731)) do_door = TRUE;


	/* Start */
	do_wall = FALSE;

	/* Determine if we need to detect walls */
	if (!borg_detect_wall[qy+0][qx+0]) do_wall = TRUE;
	if (!borg_detect_wall[qy+0][qx+1]) do_wall = TRUE;
	if (!borg_detect_wall[qy+1][qx+0]) do_wall = TRUE;
	if (!borg_detect_wall[qy+1][qx+1]) do_wall = TRUE;

	/* Hack -- check walls every few turns anyway */
	if (!borg_when_detect_walls || (borg_time - borg_when_detect_walls >= 937)) do_wall = TRUE;


	/* Hack -- find traps and doors */
	if ((do_trap || do_door) &&
	    ((!borg_when_detect_traps || (borg_time - borg_when_detect_traps >= 5)) ||
	     (!borg_when_detect_doors || (borg_time - borg_when_detect_doors >= 5))))
	{
		/* Check for traps and doors */
		if (borg_zap_rod(SV_ROD_DETECTION) ||
		    borg_spell(0, 7) ||
		    borg_prayer(5, 1))
		{
			borg_note("# Checking for traps and doors.");

			borg_react("SELF:both", "SELF:both");

			borg_when_detect_traps = borg_time;
			borg_when_detect_doors = borg_time;

			return (TRUE);
		}
	}
	/* Hack -- Mages should also check for monsters */
	if (do_detect_monsters && borg_spell(0,1))
	{
		borg_note("# Checking for monsters.");
		do_detect_monsters = FALSE;
		return (TRUE);
	}
			
	/* Hack -- find traps */
	if (do_trap &&
	    (!borg_when_detect_traps || (borg_time - borg_when_detect_traps >= 7)))
	{
		/* Check for traps */
		if (borg_read_scroll(SV_SCROLL_DETECT_TRAP) ||
		    borg_use_staff(SV_STAFF_DETECT_TRAP) ||
		    borg_zap_rod(SV_ROD_DETECT_TRAP) ||
		    borg_prayer(0, 5))
		{
			borg_note("# Checking for traps.");

			borg_react("SELF:trap", "SELF:trap");

			borg_when_detect_traps = borg_time;

			return (TRUE);
		}
	}


	/* Hack -- find doors */
	if (do_door &&
	    (!borg_when_detect_doors || (borg_time - borg_when_detect_doors >= 9)))
	{
		/* Check for traps */
		if (borg_read_scroll(SV_SCROLL_DETECT_DOOR) ||
		    borg_use_staff(SV_STAFF_DETECT_DOOR) ||
		    borg_zap_rod(SV_ROD_DETECT_DOOR) ||
		    borg_prayer(0, 6))
		{
			borg_note("# Checking for doors.");

			borg_react("SELF:door", "SELF:door");

			borg_when_detect_doors = borg_time;

			return (TRUE);
		}
	}


	/* Hack -- find walls */
	if (do_wall &&
	    (!borg_when_detect_walls || (borg_time - borg_when_detect_walls >= 15)))
	{
		/* Check for walls */
		if (borg_activate_artifact(ART_ELENDIL) ||
		    borg_read_scroll(SV_SCROLL_MAPPING) ||
		    borg_use_staff(SV_STAFF_MAPPING) ||
		    borg_prayer(2, 6))
		{
			borg_note("# Checking for walls.");

			borg_react("SELF:wall", "SELF:wall");

			borg_when_detect_walls = borg_time;

			return (TRUE);
		}
	}


	/* Start */
	do_lite = FALSE;

	/* Dark central grid */
	if (borg_cave_info[py][px] & (CAVE_DARK))
	{
		/* Assume okay */
		do_lite = TRUE;

		/* Scan diagonal neighbors */
		for (i = 4; i < 8; i++)
		{
			/* Get location */
			x = px + ddx_ddd[i];
			y = py + ddy_ddd[i];

			/* Location must be known */
			if (borg_cave_feat[y][x] == FEAT_NONE) do_lite = FALSE;

			/* Location must not be a wall/door */
			if (!borg_cave_floor_bold(y, x)) do_lite = FALSE;
		}
	}

	/* Lite radius */
	r = b_ptr->cur_lite;

	/* Hack */
	if (!do_lite && (r >= 2) &&
	    (px >= r) && (px < DUNGEON_WID - r) &&
	    (py >= r) && (py < DUNGEON_HGT - r) &&
	    (rand_int(100) < 90))
	{
		bool f1 = TRUE;
		bool f2 = FALSE;

		/* Scan the "local" grids (5x5 or 7x7) */
		for (y = py - r; y <= py + r; y++)
		{
			/* Scan the "local" grids (5x5 or 7x7) */
			for (x = px - r; x <= px + r; x++)
			{
				/* Location must be a floor */
				if (!borg_cave_floor_bold(y, x)) f1 = FALSE;

				/* Location must not be glowing */
				if (borg_cave_info[y][x] & (CAVE_GLOW)) f1 = FALSE;

				/* Notice any known dark grids */
				if (borg_cave_info[y][x] & (CAVE_DARK)) f2 = TRUE;
			}
		}

		/* Sounds good XXX XXX XXX */
		if (f1 && f2) do_lite = do_lite_aux = TRUE;
	}


	/* Hack -- call lite */
	if (do_lite &&
	    (!borg_when_call_lite || (borg_time - borg_when_call_lite >= 7)))
	{
		/* Call light */
		if (borg_read_scroll(SV_SCROLL_LIGHT) ||
			borg_activate_artifact(ART_GALADRIEL) ||
		    borg_zap_rod(SV_ROD_ILLUMINATION) ||
		    borg_use_staff(SV_STAFF_LITE) ||
		    borg_spell(0, 3) ||
		    borg_prayer(0, 4))
		{
			if (do_lite_aux) borg_note("# Gratuitous illumination");

			borg_note("# Illuminating the room");

			borg_react("SELF:lite", "SELF:lite");

			borg_when_call_lite = borg_time;

			return (TRUE);
		}
	}


	/* Hack -- Wizard Lite */
	if (TRUE &&
	    (!borg_when_wizard_lite || (borg_time - borg_when_wizard_lite >= 1000)))
	{
		/* Wizard lite */
		if (borg_activate_artifact(ART_THRAIN) ||
		    borg_prayer(5, 4))
		{
			borg_note("# Illuminating the dungeon");

			/* borg_react("SELF:wizard lite", "SELF:wizard lite"); */

			borg_when_wizard_lite = borg_time;

			return (TRUE);
		}
	}


	/* Oops */
	return (FALSE);
}

/*
 * Remove Curse on swap weapon
 * apw
 */
static bool borg_remove_curse(void)
{
    /* Nothing to decurse */
	if (!borg_cursed) return (FALSE);

	borg_note("# removing curse");

    if ((borg_slot(TV_SCROLL,SV_SCROLL_STAR_REMOVE_CURSE) < 0) &&
        (borg_prayer_fail(7,2, TRUE) > 35))
	{
		return (FALSE);
	}

    /* remove the curse */
    if (borg_read_scroll(SV_SCROLL_STAR_REMOVE_CURSE) ||
        borg_prayer(7,2) )
	{
		borg_cursed = FALSE;
		return (TRUE);
	}

    /* Nothing to do */
    return (FALSE);
}


/*
 * Enchant armor
 */
static bool borg_enchant_to_a(void)
{
	int i, b_i = -1;
	int a, b_a = 99;


	/* Nothing to enchant */
	if (!xb_ptr->need_enchant_to_a) return (FALSE);

	/* Need "enchantment" ability */
	if (!amt_enchant_to_a) return (FALSE);


	/* Look for armor that needs enchanting */
	for (i = INVEN_BODY; i <= INVEN_FEET; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Obtain the bonus */
		a = item->to_a;

		/* Skip "boring" items */
		if (a >= xb_ptr->max_enchant_armor) continue;

		/* Find the least enchanted item */
		if ((b_i >= 0) && (b_a < a)) continue;

		/* Save the info */
		b_i = i; b_a = a;
	}
	if (b_i >= 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 4) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_ARMOR) ||
			borg_read_scroll(SV_SCROLL_ENCHANT_ARMOR))
		{
			/* Send item index */
			borg_send_item_index(b_i);

			/* Notice to prevent buying another one */
			borg_notice(TRUE);

			/* Success */
			return (TRUE);
		}	
	}

	if (borg_armour_swap > 1)
    {
		for (i = borg_armour_swap; i <= borg_armour_swap; i++)
		{
			auto_item *item = &borg_items[borg_armour_swap];

			if ((item->tval == TV_RING)  ||
				(item->tval == TV_AMULET)) continue;

			/* Obtain the bonus */
			a = item->to_a;

			/* Skip "boring" items */
			if (a >= xb_ptr->max_enchant_armor) continue;

	        /* Save the info */
			b_i = borg_armour_swap; b_a = a;
		}
    }

	/* If we found equip that needs enchanting, enchant it first */
	if (b_i > 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 4) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_ARMOR) ||
		    borg_read_scroll(SV_SCROLL_ENCHANT_ARMOR))
		{
			/* Send item index */
			borg_note(format("# enchanting swap armour b_i = '%d'",b_i));
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}

	/* Nothing to do */
	return (FALSE);
}



/*
 * Enchant weapons to hit
 */
static bool borg_enchant_to_h(void)
{
	int i, b_i = -1;
	int a, b_a = 99;


	/* Nothing to enchant */
	if (!xb_ptr->need_enchant_to_h) return (FALSE);

	/* Need "enchantment" ability */
	if ((!amt_enchant_to_h) && (!amt_enchant_weapon)) return (FALSE);


	/* Look for a weapon that needs enchanting */
	for (i = INVEN_WIELD; i <= INVEN_BOW; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Obtain the bonus */
		a = item->to_h;

		/* Skip "boring" items */
		if (a >= xb_ptr->max_enchant_weapon) continue;

		/* Find the least enchanted item */
		if ((b_i >= 0) && (b_a < a)) continue;

		/* Save the info */
		b_i = i; b_a = a;
	}

	/* If we found equip that needs enchanting, enchant it first */
	if (b_i >= 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 3) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
		    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_HIT))
		{
			/* Send item index */
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}

	if (borg_weapon_swap > 1)
    {
		for (i = borg_weapon_swap; i <= borg_weapon_swap; i++)
		{
			auto_item *item = &borg_items[borg_weapon_swap];

			/* Obtain the bonus */
			a = item->to_h;

			/* Skip "boring" items */
			if (a >= xb_ptr->max_enchant_weapon) continue;

	        /* Save the info */
			b_i = borg_weapon_swap; b_a = a;
		}
    }

	/* If we found equip that needs enchanting, enchant it first */
	if (b_i >= 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 3) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
		    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_HIT))
		{
			/* Send item index */
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}

	/* Look for missiles that need enchanting */
	for (i = 0; i < INVEN_WIELD; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Skip non-missiles */
		if (item->tval != b_ptr->ammo_tval) continue;

		/* Skip cursed ammo */
		if (item->value <= 0) continue;

		/* Only enchant in batches of at least 20 */
		if (item->iqty < 20) continue;

		/* Obtain the bonus */
		a = item->to_h;

		/* Skip "boring" items */
        if (a >= xb_ptr->max_enchant_weapon_bolt) continue;

		/* Find the least enchanted item */
		if ((b_i >= 0) && (b_a < a)) continue;

		/* Save the info */
		b_i = i; b_a = a;
	}

	/* Nothing */
	if (b_i < 0) return (FALSE);

	/* Enchant it */
	if (borg_prayer(7, 3) ||
		borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
	    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_HIT))
	{
		/* Send item index */
		borg_send_item_index(b_i);

		/* Success */
		return (TRUE);
	}

	/* Nothing to do */
	return (FALSE);
}


/*
 * Enchant weapons
 */
static bool borg_enchant_to_d(void)
{
	int i, b_i = -1;
	int a, b_a = 99;


	/* Nothing to enchant */
	if (!xb_ptr->need_enchant_to_d) return (FALSE);

	/* Need "enchantment" ability */
	if ((!amt_enchant_to_d) && (!amt_enchant_weapon)) return (FALSE);


	/* Look for a weapon that needs enchanting */
	for (i = INVEN_WIELD; i <= INVEN_BOW; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Obtain the bonus */
		a = item->to_d;

		/* Skip "boring" items */
		if (a >= xb_ptr->max_enchant_weapon) continue;

		/* Find the least enchanted item */
		if ((b_i >= 0) && (b_a < a)) continue;

		/* Save the info */
		b_i = i; b_a = a;
	}

	/* If we found equip that needs enchanting, enchant it first */
	if (b_i >= 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 3) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
		    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_DAM))
		{
			/* Send item index */
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}

		if (borg_weapon_swap > 1)
    {
		for (i = borg_weapon_swap; i <= borg_weapon_swap; i++)
		{
			auto_item *item = &borg_items[borg_weapon_swap];

			/* Obtain the bonus */
			a = item->to_d;

			/* Skip "boring" items */
			if (a >= xb_ptr->max_enchant_weapon) continue;

	        /* Save the info */
			b_i = borg_weapon_swap; b_a = a;
		}
    }

	/* If we found equip that needs enchanting, enchant it first */
	if (b_i >= 0)
	{
		/* Enchant it */
		if (borg_prayer(7, 3) ||
			borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
		    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_DAM))
		{
			/* Send item index */
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}

	/* Look for missiles that need enchanting */
	for (i = 0; i < INVEN_WIELD; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Skip non-missiles */
		if (item->tval != b_ptr->ammo_tval) continue;

		/* Skip cursed ammo */
		if (item->value <= 0) continue;

		/* Only enchant in batches of at least 20 */
		if (item->iqty < 20) continue;

		/* Obtain the bonus */
		a = item->to_d;

		/* Skip "boring" items */
        if (a >= xb_ptr->max_enchant_weapon_bolt) continue;

		/* Find the least enchanted item */
		if ((b_i >= 0) && (b_a < a)) continue;

		/* Save the info */
		b_i = i; b_a = a;
	}

	/* Nothing */
	if (b_i < 0) return (FALSE);

	/* Enchant it */
	if (borg_prayer(7, 3) ||
		borg_read_scroll(SV_SCROLL_STAR_ENCHANT_WEAPON) ||
	    borg_read_scroll(SV_SCROLL_ENCHANT_WEAPON_TO_DAM))
	{
		/* Send item index */
		borg_send_item_index(b_i);

		/* Success */
		return (TRUE);
	}

	/* Nothing to do */
	return (FALSE);
}


/*
 * Enchant bolts
 */
static bool borg_enchant_brand(void)
{
	int i, b_i = -1;
	/* int a, b_a = 99; DvE */


	/* Nothing to enchant */
    if (!xb_ptr->need_enchant_brand) return (FALSE);


	/* Look for missiles that need enchanting */
	for (i = 0; i < INVEN_WIELD; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Skip non-missiles */
		if (item->tval != b_ptr->ammo_tval) continue;

		/* Skip cursed ammo */
		if (item->value <= 0) continue;

		/* Only enchant in batches of at least 20 */
		if (item->iqty < 20) continue;

        /* Skip already enchanted items */
        if (item->name1 || item->name2) continue;

		/* Save the info */
		b_i = i; /* b_a = a; */
	}

	/* Nothing */
	if (b_i < 0) return (FALSE);

	/* Enchant it */
    if (borg_activate_artifact(ART_CUBRAGOL))
	{
#if 0
		/* Send item index */
		borg_send_item_index(b_i);
#endif

		/* Success */
		return (TRUE);
	}

	/* Nothing to do */
	return (FALSE);
}


/*
 * Enchant things
 */
bool borg_enchanting(void)
{
	/* Forbid blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* Handle darkness XXX XXX XXX */

	/* What were my swaps again?? */
	borg_notice(TRUE);

	/* Hack -- Reset counter */
	time_this_panel = 0;

	/* Enchant things */
	if (borg_enchant_to_d()) return (TRUE);
	if (borg_enchant_to_a()) return (TRUE);
	if (borg_enchant_to_h()) return (TRUE);

    if (borg_enchant_brand()) return (TRUE);

	if (borg_remove_curse()) return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Recharge things
 *
 * Prioritize available items.  XXX XXX XXX
 */
bool borg_recharging(void)
{
	int i, b_i = -1;


	/* Forbid blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* Handle darkness XXX XXX XXX */


	/* Look for an item to recharge */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip non-identified items */
		if (!item->able) continue;

		/* Skip non wands/staffs */
		if ((item->tval != TV_STAFF) && (item->tval != TV_WAND)) continue;

		/* Skip charged items */
		if (item->pval) continue;

		/* Save the info */
		b_i = i;
	}

	/* Nope */
	if (b_i < 0) return (FALSE);

	/* Attempt to recharge */
	if (borg_spell(6, 2) ||
	    borg_spell(3, 1) ||
	    borg_spell(2, 1) ||
	    borg_prayer(7, 1) ||
	    borg_read_scroll(SV_SCROLL_RECHARGING))
	{
		auto_item *item = &borg_items[b_i];

		/* Message */
		borg_note(format("Recharging %s", item->desc));

		/* Send item index */
		borg_send_item_index(b_i);

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Attempt to consume an item
 */
static bool borg_consume(int i)
{
	auto_item *item = &borg_items[i];


	/* Special destruction */
	switch (item->tval)
	{
		case TV_POTION:
		{
			/* Check the potion */
			switch (item->sval)
			{
				case SV_POTION_WATER:
				case SV_POTION_APPLE_JUICE:
				case SV_POTION_SLIME_MOLD:
				case SV_POTION_CURE_LIGHT:
				case SV_POTION_CURE_SERIOUS:
				case SV_POTION_CURE_CRITICAL:
				case SV_POTION_RES_STR:
				case SV_POTION_RES_INT:
				case SV_POTION_RES_WIS:
				case SV_POTION_RES_DEX:
				case SV_POTION_RES_CON:
				case SV_POTION_RES_CHR:
				case SV_POTION_RESTORE_EXP:
				case SV_POTION_HEROISM:
				case SV_POTION_BESERK_STRENGTH:
				case SV_POTION_RESIST_HEAT:
				case SV_POTION_RESIST_COLD:
				case SV_POTION_INFRAVISION:
				case SV_POTION_DETECT_INVIS:
				case SV_POTION_SLOW_POISON:
				case SV_POTION_CURE_POISON:
				case SV_POTION_SPEED:
				{
					/* Try quaffing the potion */
					if (borg_quaff_potion(item->sval)) return (TRUE);
				}
			}

			break;
		}

		case TV_SCROLL:
		{
			/* Check the scroll */
			switch (item->sval)
			{
				case SV_SCROLL_REMOVE_CURSE:
				case SV_SCROLL_LIGHT:
				case SV_SCROLL_MONSTER_CONFUSION:
				case SV_SCROLL_RUNE_OF_PROTECTION:
				case SV_SCROLL_STAR_REMOVE_CURSE:
				case SV_SCROLL_DETECT_GOLD:
				case SV_SCROLL_DETECT_ITEM:
				case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
				case SV_SCROLL_SATISFY_HUNGER:
				case SV_SCROLL_DISPEL_UNDEAD:
				case SV_SCROLL_BLESSING:
				case SV_SCROLL_HOLY_CHANT:
				case SV_SCROLL_HOLY_PRAYER:
				{
					/* Try reading the scroll */
					if (borg_read_scroll(item->sval)) return (TRUE);
				}
			}

			break;
		}

		case TV_FOOD:
		{
			/* Check the scroll */
			switch (item->sval)
			{
				case SV_FOOD_CURE_POISON:
				case SV_FOOD_CURE_BLINDNESS:
				case SV_FOOD_CURE_PARANOIA:
				case SV_FOOD_CURE_CONFUSION:
				case SV_FOOD_CURE_SERIOUS:
				case SV_FOOD_RESTORE_STR:
				case SV_FOOD_RESTORE_CON:
				case SV_FOOD_RESTORING:
				case SV_FOOD_BISCUIT:
				case SV_FOOD_JERKY:
				case SV_FOOD_RATION:
				case SV_FOOD_SLIME_MOLD:
				case SV_FOOD_WAYBREAD:
				case SV_FOOD_PINT_OF_ALE:
				case SV_FOOD_PINT_OF_WINE:
				{
					/* Try eating the food (unless Bloated) */
					if (!borg_base_is_full && borg_eat_food(item->sval)) return (TRUE);
				}
			}

			break;
		}
	}


	/* Nope */
	return (FALSE);
}




/*
 * Destroy "junk" items
 */
bool borg_crush_junk(void)
{
	int i;


	/* Hack -- no need */
	if (!borg_do_crush_junk) return (FALSE);


	/* Destroy actual "junk" items */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- dont crush the swap weapon */
		if (i != 0) 
		{
			if (i == borg_weapon_swap) continue;
			if (i == borg_armour_swap) continue;
		}

		/* Skip non "worthless" items */
		if ((item->tval > TV_CHEST) && (item->value > 0)) continue;

		/* Never junk certain potions */
		if ((item->tval == TV_POTION) &&
			((item->sval == SV_POTION_HEALING) ||
			 (item->sval == SV_POTION_STAR_HEALING) ||
			 (item->sval == SV_POTION_LIFE) ||
			 (item->sval == SV_POTION_RESTORE_MANA)))
		{
			continue;
		}



#if 0
		/* Hack -- Skip artifacts */
		if (item->name1) continue;
		if (item->special) continue;
#endif

		/* Message */
		borg_note(format("# Junking junk (junk)"));

		/* Message */
		borg_note(format("# Destroying %s.", item->desc));

		/* Artifacts require special methods */
		if (item->name1 || item->special)
		{
			/* Send count (all) */
			borg_keypresses("099");

			/* Send action (drop) */
			borg_keypress('d');

			/* Send item index */
			borg_send_item_index(i);

			/* Remember this location, in order to never pick it up again */
			borg_bad_location_y = b_ptr->py;
			borg_bad_location_x = b_ptr->px;
		}
		else
		{
			/* Send count (all) */
			borg_keypresses("099");

			/* Send action (destroy) */
			borg_keypress('k');

			/* Send item index */
			borg_send_item_index(i);

			/* No verification needed XXX XXX XXX */
		}

		/* Success */
		return (TRUE);
	}


	/* Hack -- no need */
	borg_do_crush_junk = FALSE;


	/* Nothing to destroy */
	return (FALSE);
}



/*
 * Destroy something to make a free inventory slot.
 *
 * This function evaluates the possible worlds that result from
 * the destruction of each inventory slot, and attempts to destroy
 * that slot which causes the best possible resulting world.
 *
 * We attempt to avoid destroying unknown items by "rewarding" the
 * presence of unknown items by a massively heuristic value.
 *
 * If the Borg cannot find something to destroy, which should only
 * happen if he fills up with artifacts, then he will probably act
 * rather twitchy for a while.
 *
 * This function does not have to be very efficient.
 */
bool borg_crush_hole(void)
{
	int i, b_i = -1;
	s32b p, b_p = 0L;

	bool fix = FALSE;


	/* Hack -- no need */
	if (!borg_do_crush_hole) return (FALSE);


	/* Do not destroy items unless we need the space */
	if (!borg_items[INVEN_PACK-1].iqty) return (FALSE);


	/* Scan the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

#if 0
        /* dont crush the swap weapon */
		if ( i != 0)
		{
			if (i == borg_weapon_swap) continue;
			if (i == borg_armour_swap) continue;
		}
#endif
		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- skip "artifacts" */
		if (item->name1) continue;
		if (item->special) continue;
		/* Never junk certain potions */
		if ((item->tval == TV_POTION) &&
			((item->sval == SV_POTION_HEALING) ||
			 (item->sval == SV_POTION_STAR_HEALING) ||
			 (item->sval == SV_POTION_LIFE) ||
			 (item->sval == SV_POTION_RESTORE_MANA)))
		{
			continue;
		}

		/* Save the item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Destroy the item */
		WIPE(&borg_items[i], auto_item);

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory */
		borg_notice(TRUE);

		/* Evaluate the inventory */
		p = borg_power();

		/* Restore the item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Penalize loss of "gold" */
		p -= (item->iqty * item->value);

		/* Hack -- try not to destroy "unaware" items */
		if (!item->kind && (item->value > 0))
		{
			/* Hack -- Reward "unaware" items */
			switch (item->tval)
			{
				case TV_RING:
				case TV_AMULET:
				{
					p -= (b_ptr->max_depth * 5000L);
					break;
				}

				case TV_ROD:
				{
					p -= (b_ptr->max_depth * 3000L);
					break;
				}

				case TV_STAFF:
				case TV_WAND:
				{
					p -= (b_ptr->max_depth * 2000L);
					break;
				}

				case TV_SCROLL:
				{
				if ((item->sval != SV_SCROLL_PROTECTION_FROM_EVIL) ||
					(item->sval != SV_SCROLL_RUNE_OF_PROTECTION))
                        p -=(b_ptr->max_depth * (30000L));
                else
                		p -= (b_ptr->max_depth * 500L);
					break;
				}
				case TV_POTION:
				{
					/* BIG HACK! don't crush heal/mana potions.  It could be */
                    /* that we are in town and are collecting them. */
                    if ((item->sval != SV_POTION_HEALING) ||
                        (item->sval != SV_POTION_STAR_HEALING) ||
                        (item->sval != SV_POTION_LIFE) ||
                        (item->sval != SV_POTION_RESTORE_MANA))
                        p -= 5000000L;
                    else
                        p -= (b_ptr->max_depth * 500L);
					break;
				}
				
				case TV_FOOD:
				{
					p -= (b_ptr->max_depth * 10L);
					break;
				}
			}
		}

		/* Hack -- try not to destroy non-cheap, non-known, non-average items (unless "icky") */
		if (!item->able && !item->average &&
		    (item->value > 0) && !borg_item_icky(item))
		{
			/* Reward "unknown" items */
			switch (item->tval)
			{
				case TV_SHOT:
				case TV_ARROW:
				case TV_BOLT:
				{
					p -= 100L;
					break;
				}

				case TV_BOW:
				{
					p -= 20000L;
					break;
				}

				case TV_DIGGING:
				{
					p -= 10L;
					break;
				}

				case TV_HAFTED:
				case TV_POLEARM:
				case TV_SWORD:
				{
					p -= 10000L;
					break;
				}

				case TV_BOOTS:
				case TV_GLOVES:
				case TV_HELM:
				case TV_CROWN:
				case TV_SHIELD:
				case TV_CLOAK:
				{
					p -= 15000L;
					break;
				}

				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:
				{
					p -= 15000L;
					break;
				}

				case TV_AMULET:
				case TV_RING:
				{
					p -= 5000L;
					break;
				}

				case TV_STAFF:
				case TV_WAND:
				{
					p -= 1000L;
					break;
				}
			}
		}

		/* Ignore "bad" swaps */
		if ((b_i >= 0) && (p < b_p)) continue;

		/* Maintain the "best" */
		b_i = i; b_p = p;
	}

	/* Examine the inventory */
	if (fix) borg_notice(TRUE);

	/* Attempt to destroy it */
	if (b_i >= 0)
	{
		auto_item *item = &borg_items[b_i];

		/* Debug */
		borg_note(format("# Junking %ld gold (full)", borg_base_power - b_p));

		/* Try to consume the junk */
		if (borg_consume(b_i)) return (TRUE);

		/* Message */
		borg_note(format("# Destroying %s.", item->desc));

		/* Send count (all) */
		borg_keypresses("099");

		/* Send action (destroy) */
		borg_keypress('k');

		/* Send item index */
		borg_send_item_index(b_i);

		/* No verification needed XXX XXX XXX */

		/* Success */
		return (TRUE);
	}


	/* Hack -- no need */
	borg_do_crush_hole = FALSE;


	/* Paranoia */
	return (FALSE);
}



/*
 * Destroy "junk" when slow (in the dungeon).
 *
 * We penalize the loss of both power and monetary value, and reward
 * the loss of weight that may be slowing us down.  The weight loss
 * is worth one gold per tenth of a pound.  This causes things like
 * lanterns and chests and spikes to be considered "annoying".
 */
bool borg_crush_slow(void)
{
	int i, b_i = -1;
	s32b p, b_p = 0L;

	s32b temp;

	s32b greed;

	bool fix = FALSE;


	/* Hack -- no need */
	if (!borg_do_crush_slow) return (FALSE);


	/* Hack -- never in town */
	if (!b_ptr->depth) return (FALSE);


	/* Do not crush items unless we are slow */
	if (b_ptr->pspeed >= 110) return (FALSE);

	/* Calculate "greed" factor */
	/* not to greedy, stay in the dungeon */
	greed = (b_ptr->au / 100L) + (10L * (110 - b_ptr->pspeed));

	/* Minimal greed */
    if (greed < 1000L) greed = 1000L;
    if (greed > 25000L) greed = 25000L;


	/* Scan for junk */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;
#if 0
        /* dont crush the swap weapon */
		if (i != 0)
		{
			if (i == borg_weapon_swap) continue;
			if (i == borg_armour_swap) continue;
		}
#endif
		/* Skip non-cheap, non-known, non-average items (unless "icky") */
		if (!item->able && !item->average &&
		    (item->value > 0) && !borg_item_icky(item)) continue;

		/* Hack -- Skip artifacts */
		if (item->name1) continue;

		/* Save the item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Destroy one of the items */
		borg_items[i].iqty--;

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory */
		borg_notice(TRUE);

		/* Evaluate the inventory */
		p = borg_power();

		/* Restore the item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Obtain the base price */
		temp = ((item->value < 30000L) ? item->value : 30000L);

		/* Hack -- ignore very cheap items */
		if (temp < greed) temp = 0L;

		/* Penalize */
		p -= temp;

		/* Obtain the base weight */
		temp = item->weight;

		/* Reward */
		p += temp;

		/* Ignore "bad" swaps */
		if ((b_i >= 0) && (p < b_p)) continue;

		/* Maintain the "best" */
		b_i = i; b_p = p;
	}

	/* Examine the inventory */
	if (fix) borg_notice(TRUE);

	/* Destroy "useless" things */
	if ((b_i >= 0) && (b_p >= borg_base_power))
	{
		auto_item *item = &borg_items[b_i];

		/* Message */
		borg_note(format("# Junking %ld gold (slow)", borg_base_power - b_p));

		/* Attempt to consume it */
		if (borg_consume(b_i)) return (TRUE);

		/* Message */
		borg_note(format("# Destroying %s.", item->desc));

		/* Send count (one) */
		borg_keypresses("01");

		/* Send action (destroy) */
		borg_keypress('k');

		/* Send item index */
		borg_send_item_index(b_i);

		/* No verification needed XXX XXX XXX */

		/* Success */
		return (TRUE);
	}


	/* Hack -- no need */
	borg_do_crush_slow = FALSE;


	/* Nothing to destroy */
	return (FALSE);
}




/*
 * Use "identify"
 *
 * Note that "borg_parse()" will "cancel" the identification if it
 * detects a "You failed..." message.  This is VERY important!!!
 * Otherwise the "identify" might induce bizarre actions by sending
 * the "index" of an item as a command.
 *
 * We always identify unknown equipment, even if it is average, for
 * various reasons, including the fact that we are not allowed to
 * enchant equipment if it is average.
 *
 * We instantly identify items known to be blessed.
 *
 * We instantly identify items known to be "terrible".
 *
 * We identify most un-aware items as soon as possible.
 *
 * We identify most un-known items as soon as possible.
 *
 * We play games with items that get "feelings" to try and wait for
 * "sensing" to take place if possible.
 *
 * Make sure not to sell "non-aware" items, unless we are really sure
 * we want to lose them.  For example, we should wait for feelings on
 * (non-icky) wearable items or else make sure that we identify them
 * before we try and sell them.  Otherwise we might accidentally sell
 * a really good object.
 *
 * Mega-Hack -- the whole "sometimes identify things" code is a total
 * hack.  Slightly less bizarre would be some form of "occasionally,
 * pick a random item and identify it if necessary", which might lower
 * the preference for identifying items that appear early in the pack.
 * Also, preventing inventory motion would allow proper time-stamping.
 *
 * We should identify things in a much better order, for example, after
 * getting a flail from farmer maggot, and a small shield from a kobold,
 * we should definitely identify the flail first.
 */
bool borg_test_stuff(void)
{
	int i, b_i = -1;
	s32b v, b_v = -1;


	/* Look for an item to identify (equipment) */
	for (i = INVEN_WIELD; i <= INVEN_FEET; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- full_ident means able */
//		if (item->full_ident) continue;

		/* Skip known items */
		if (item->able) continue;

		/* Get the value XXX XXX */
		v = item->value + 100000L;

		/* Track the best */
		if (v < b_v) continue;

		/* Track it */
		b_i = i; b_v = v;
	}

	/* Look for an item to identify (inventory) */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Hack -- full_ident means able */
//		if (item->full_ident) continue;

		/* Skip known items */
		if (item->able) continue;

		/* Skip average items */
		if (item->average) continue;

		/* Hack -- assume no value */
		v = 0;

		/* Hack -- Alwas id blessed stuff */
		if (item->blessed || item->special) v += 10000L;

		/* Hack -- reward "unaware" items */
		if (!item->kind)
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_RING:
				case TV_AMULET:
				{
					/* Hack -- reward depth */
					v += (b_ptr->max_depth * 5000L);

					break;
				}

				case TV_ROD:
				{
					/* Hack -- reward depth */
					v += (b_ptr->max_depth * 3000L);

					break;
				}

				case TV_WAND:
				case TV_STAFF:
				{
					/* Hack -- reward depth */
					v += (b_ptr->max_depth * 2000L);

					break;
				}

				case TV_POTION:
				case TV_SCROLL:
				{
					/* Hack -- boring levels */
					if (b_ptr->max_depth < 5) break;

					/* Hack -- reward depth */
					v += (b_ptr->max_depth * 500L);

					break;
				}

				case TV_FOOD:
				{
					/* Hack -- reward depth */
					v += (b_ptr->max_depth * 10L);

					break;
				}
			}
		}

		/* Analyze the type */
		switch (item->tval)
		{
			case TV_CHEST:
			{
				/* Hack -- Always identify chests */
				v = item->value;
				break;
			}

			case TV_WAND:
			case TV_STAFF:
			{
				/* Hack -- Always identify (get charges) */
				v = item->value;
				break;
			}

			case TV_RING:
			case TV_AMULET:
			{
				/* Hack -- Always identify (get information) */
				v = item->value;
				break;
			}

			case TV_LITE:
			{
				/* Hack -- Always identify (get artifact info) */
				v = item->value;
				break;
			}

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
			{
				/* Mega-Hack -- use identify spell/prayer */
				if (borg_spell_legal(2, 4) || borg_prayer_legal(5, 2))
				{
					v = item->value;
				}

				/* Mega-Hack -- mages get bored */
				if ((b_ptr->pclass == CLASS_MAGE) && (rand_int(1000) < b_ptr->lev))
				{

					/* Mega-Hack -- ignore "icky" items */
					if (!borg_item_icky(item)) v = item->value;
				}

				/* Mega-Hack -- rangers get bored */
				if ((b_ptr->pclass == CLASS_RANGER) && (rand_int(3000) < b_ptr->lev))
				{

					/* Mega-Hack -- ignore "icky" items */
					if (!borg_item_icky(item)) v = item->value;
				}

				/* Mega-Hack -- priests get bored */
				if ((b_ptr->pclass == CLASS_PRIEST) && (rand_int(5000) < b_ptr->lev))
				{

					/* Mega-Hack -- ignore "icky" items */
					if (!borg_item_icky(item)) v = item->value;
				}

				break;
			}
		}

		/* Ignore */
		if (!v) continue;

		/* Track the best */
		if (v < b_v) continue;

		/* Track it */
		b_i = i; b_v = v;
	}


	/* Found something */
	if (b_i >= 0)
	{
		auto_item *item = &borg_items[b_i];

		/* Use some form of identification */
		if (borg_use_staff(SV_STAFF_IDENTIFY) ||
			borg_read_scroll(SV_SCROLL_IDENTIFY) ||
			borg_activate_artifact(ART_ERIRIL) ||
		    borg_zap_rod(SV_ROD_IDENTIFY) ||
		    borg_spell(2, 4) ||
		    borg_prayer(5, 2)
		    )
		{
			/* Log -- may be cancelled */
			borg_note(format("# Identifying %s.", item->desc));

			/* Send item index */
			borg_send_item_index(b_i);

			/* Success */
			return (TRUE);
		}
	}


	/* Nothing to do */
	return (FALSE);
}


/* 
 * Use "*identify*" scrolls on special objects
 *
 * Hack -- we inscribe items with a special inscription before
 * using *identify* on them so we will know which items to try
 * to examine.
 */
bool borg_star_stuff(void)
{
	int j;

	int i, b_i = -1;
	s32b v, b_v = -1;


	/* See "borg_read_scroll()" XXX XXX XXX */
	
	/* Blind or Confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* Look for the appropriate scroll */
	j = borg_slot(TV_SCROLL, SV_SCROLL_STAR_IDENTIFY);

	/* None available */
	if (j < 0) return (FALSE);


	/* Look for an item to *identify* */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Check flag */
		if (!item->do_star) continue;

        /* Don't *ID* ego-items in the dungeon */
        // if (!item->name1 && b_ptr->depth) continue;

        /* Don't *ID* ego-items when there are artifacts */
        // if (!item->name1 && borg_need_star_ident()) continue;

		/* Prioritize XXX XXX XXX */
        if (item->name1)
            v = 100;
        else
            v = 0;

		/* Track the best */
		if (v < b_v) continue;

		/* Track it */
		b_i = i; b_v = v;
	}


	/* Found something */
	if (b_i >= 0)
	{
		auto_item *item = &borg_items[b_i];

		/* Log action (may be cancelled) */
		borg_note(format("# Using '*identify*' on %s.", item->desc));

		/* Send first action (special inscription) */
		borg_send_inscribe_item(b_i, "BORG_*");

		/* mark item *id*ed */
		item->full_ident = TRUE;

		/* Send second action (read scroll) */
		(void)borg_read_scroll(SV_SCROLL_STAR_IDENTIFY);

		/* Send item index */
		borg_send_item_index(b_i);
		
		/* Send escape keys XXX XXX XXX */
		borg_keypress(ESCAPE);
		borg_keypress(ESCAPE);
		borg_keypress(ESCAPE);
		borg_keypress(ESCAPE);

		/* Success */
		return (TRUE);
	}


	/* Nothing to do */
	return (FALSE);
}


/* 
 * Use "examine" on special objects XXX XXX XXX
 */
bool borg_exam_stuff(void)
{
	int i, b_i = -1;
	s32b v, b_v = -1;


	/* Look for an item to "examine" */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Check flag */
		if (!item->do_exam) continue;

		/* Prioritize XXX XXX XXX */
		v = 0;

		/* Track the best */
		if (v < b_v) continue;

		/* Track it */
		b_i = i; b_v = v;
	}


	/* Found something */
	if (b_i >= 0)
	{
		auto_item *item = &borg_items[b_i];

        if (item->name1)
        {
            /* Log */
            borg_note(format("# Marking artifact %s.", item->desc));

            /* XXX XXX XXX Artifact - just inscribe it */
            borg_send_inscribe_item(b_i, "BORG_ART");
            return (TRUE);
        }

		/* Log -- may be cancelled */
		borg_note(format("# Examining %s.", item->desc));

		/* Inscribe it */
		borg_send_inscribe_item(b_i, "BORG_?");

		/* Send action (examine) */
		borg_keypress('I');

		/* Send item index */
		borg_send_item_index(b_i);

		/* Hack -- memorize index */
		borg_exam_item = b_i;
		strcpy(borg_exam_note, "");

		/* Success */
		return (TRUE);
	}


	/* Nothing to do */
	return (FALSE);
}




/*
 * Attempt to take off useless equipment
 */
bool borg_takeoff_stuff(void)
{
	int hole = INVEN_PACK - 1;

	int i, b_i = -1;

	s32b v, b_v = 0L;

	bool fix = FALSE;

	/* Require an empty slot */
	if (borg_items[hole].iqty) return (FALSE);

	/* Hack -- Prevent infinite loops */
	if (time_this_panel > 500) return (FALSE);


	/* Scan equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		borg_note(format("# Testing takeing of %s, weight: %d",item->desc, item->weight));
		
		/* Skip empty slots */
		if (!item->iqty) continue;

		/* Save the hole */
		COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

		/* Save the item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Take off the item */
		COPY(&borg_items[hole], &borg_safe_items[i], auto_item);

		/* Erase the item */
		WIPE(&borg_items[i], auto_item);

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory was FALSE*/
		borg_notice(TRUE);

		/* Evaluate the inventory */
		v = borg_power();
		borg_note(format("# new power: %d",v));

		/* Restore the item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Restore the hole */
		COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

		/* Track best */
		if ((b_i >= 0) && (b_v > v)) continue;

		/* Track best */
		b_i = i; b_v = v;
	}

	/* Examine the inventory */
	if (fix) borg_notice(TRUE);

	/* Prepare to "swap" if needed */
	if ((b_i >= 0) && (b_v > borg_base_power))
	{
		auto_item *item = &borg_items[b_i];

		/* Log */
		borg_note(format("# Taking off %s.", item->desc));
		borg_note(format("# power from %d to %d",borg_base_power, b_v));

		/* Send action (take off) */
		borg_keypress('t');

		/* Send item index XXX XXX XXX */
		borg_keypress((char)I2A(b_i - INVEN_WIELD));

		/* Update the counter */
		time_this_panel++;

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}



/*
 * This function is responsible for making sure that, if possible,
 * the "best" ring we have is always on the "right" (tight) finger,
 * so that the other functions, such as "borg_best_stuff()", do not
 * have to think about the "tight" ring, but instead, can just assume
 * that the "right" ring is "good for us" and should never be removed.
 *
 * In general, this will mean that our "best" ring of speed will end
 * up on the "right" finger, if we have one, or a ring of free action,
 * or a ring of see invisible, or some other "useful" ring.
 *
 * This routine is only called in shops, to allow us to "safely" play
 * the ring shuffling game, since this routine may take several steps,
 * and we must be able to do them all without being attacked.
 */
bool borg_swap_rings(void)
{
	int hole = INVEN_PACK - 1;
	int icky = INVEN_PACK - 2;

	s32b v1, v2;

	bool fix = FALSE;
	
	/* Hack -- Prevent loops */
	if (borg_base_is_hungry) return (FALSE);

	/*** Check conditions ***/

	/* Require an empty slot */
	if (borg_items[hole].iqty) return (FALSE);


	/*** Remove naked "loose" rings ***/

	/* Remove any naked loose ring */
	if (borg_items[INVEN_LEFT].iqty &&
	    !borg_items[INVEN_RIGHT].iqty)
	{
		/* Log */
		borg_note("# Taking off naked loose ring.");

		/* Send action (take off) */
		borg_keypress('t');

		/* Send item index XXX XXX XXX */
		borg_keypress(I2A(INVEN_LEFT - INVEN_WIELD));

		/* Success */
		return (TRUE);
	}


	/*** Check conditions ***/

	/* Require another empty slot */
	if (borg_items[icky].iqty) return (FALSE);

	/* Require "tight" ring */
	if (!borg_items[INVEN_RIGHT].iqty) return (FALSE);


	/*** Remove nasty "tight" rings ***/

	/* Save the hole */
	COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

	/* Save the ring */
	COPY(&borg_safe_items[INVEN_LEFT], &borg_items[INVEN_LEFT], auto_item);

	/* Take off the ring */
	COPY(&borg_items[hole], &borg_items[INVEN_LEFT], auto_item);

	/* Erase left ring */
	WIPE(&borg_items[INVEN_LEFT], auto_item);

	/* Fix later */
	fix = TRUE;

	/* Examine the inventory was FALSE*/
	borg_notice(TRUE);

	/* Evaluate the inventory */
	v1 = borg_power();

	/* Restore the ring */
	COPY(&borg_items[INVEN_LEFT], &borg_safe_items[INVEN_LEFT], auto_item);

	/* Restore the hole */
	COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);


	/*** Consider taking off the "right" ring ***/

	/* Save the hole */
	COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

	/* Save the ring */
	COPY(&borg_safe_items[INVEN_RIGHT], &borg_items[INVEN_RIGHT], auto_item);

	/* Take off the ring */
	COPY(&borg_items[hole], &borg_items[INVEN_RIGHT], auto_item);

	/* Erase left ring */
	WIPE(&borg_items[INVEN_RIGHT], auto_item);

	/* Fix later */
	fix = TRUE;

	/* Examine the inventory was FALSE */
	borg_notice(TRUE);

	/* Evaluate the inventory */
	v2 = borg_power();

	/* Restore the ring */
	COPY(&borg_items[INVEN_RIGHT], &borg_safe_items[INVEN_RIGHT], auto_item);

	/* Restore the hole */
	COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);


	/*** Swap rings if necessary ***/

	/* Examine the inventory */
	if (fix) borg_notice(TRUE);

	/* Remove "useless" ring */
	if (v2 > v1)
	{
		/* Log */
		borg_note("# Taking off nasty tight ring.");

		/* Send action (take off) */
		borg_keypress('t');

		/* Send item index XXX XXX XXX */
		borg_keypress(I2A(INVEN_RIGHT - INVEN_WIELD));

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}



/*
 * Place our "best" ring on the "tight" finger if needed
 *
 * This function is adopted from "borg_wear_stuff()"
 *
 * Basically, we evaluate the world in which each ring is added
 * to the current set of equipment, and we wear the ring, if any,
 * that gives us the most "power".
 *
 * The "borg_swap_rings()" code above occasionally allows us to remove
 * both rings, at which point this function will place the "best" ring
 * on the "tight" finger, and then the "borg_best_stuff()" function will
 * allow us to put on our second "best" ring on the "loose" finger.
 *
 * This function should only be used when a ring finger is empty.
 */
bool borg_wear_rings(void)
{
	int slot;

	s32b p, b_p = 0L;

	int i, b_i = -1;

	auto_item *item;

	bool fix = FALSE;


	/* Require no rings */
	if (borg_items[INVEN_LEFT].iqty) return (FALSE);
	if (borg_items[INVEN_RIGHT].iqty) return (FALSE);


	/* Scan inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];


		/* Skip empty items */
		if (!item->iqty) continue;


		/* Require "aware" */
		if (!item->kind) continue;

		/* Require known/average */
		if (!item->able && !item->average) continue;

		/* Hack -- ignore "worthless" items */
		if (!item->value) continue;


		/* Where does it go */
		slot = borg_wield_slot(item);

		/* Only process "rings" */
		if (slot != INVEN_LEFT) continue;

		/* Save the old item (empty) */
		COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

		/* Save the new item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Wear new item */
		COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

		/* Only a single item */
		borg_items[slot].iqty = 1;

		/* Reduce the inventory quantity by one */
		borg_items[i].iqty--;

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory was FALSE */
		borg_notice(TRUE);

		/* Evaluate the inventory */
		p = borg_power();

		/* Restore the old item (empty) */
		COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

		/* Restore the new item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Ignore "bad" swaps */
		if ((b_i >= 0) && (p < b_p)) continue;

		/* Maintain the "best" */
		b_i = i; b_p = p;
	}

	/* Restore bonuses */
	if (fix) borg_notice(TRUE);

	/* No item */
	if ((b_i >= 0) && (b_p > borg_base_power))
	{
		/* Get the item */
		item = &borg_items[b_i];

		/* Log */
		borg_note("# Putting on best tight ring.");

		/* Log */
		borg_note(format("# Wearing %s.", item->desc));

		/* Send action (wear) */
		borg_keypress('w');

		/* Send item index */
		borg_send_item_index(b_i);

		/* Did something */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}

/*
 * Place our "swap" if needed.   We check both the armour and the weapon
 * then wear the one that give the best result (lowest danger).
 * This function is adopted from "borg_wear_stuff()" and borg_wear_rings
 *
 * Basically, we evaluate the world in which the swap is added
 * to the current set of equipment, and we use weapon,
 * that gives the largest drop in danger---based mostly on resists.
 *
 */
bool borg_backup_swap(int p)
{
    int slot;
    int swap;

    s32b b_p = 0L;
    s32b b_p1 = 0L;
    s32b b_p2 = 0L;

    int i;

    auto_item *item;

    bool fix = FALSE;

    /* hack prevent the swap till you drop loop */
    if (borg_base_is_hungry || borg_base_is_weak) return (FALSE);

    /*apw Forbid if been sitting on level forever */
    /*    Just come back and work through the loop later */
    if (time_this_panel > 500) return (FALSE);

    /* make sure we have an appropriate swap */
    if (borg_armour_swap < 1 && borg_weapon_swap < 1) return (FALSE);

    /* Check the items, first armour then weapon */
    /* get the item */
    i = borg_armour_swap;
    item = &borg_items[i];

    /* Where does it go */
    slot = borg_wield_slot(item);

    /* Save the old item (empty) */
    COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

    /* Save the new item */
    COPY(&borg_safe_items[i], &borg_items[i], auto_item);

    /* Wear new item */
    COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

    /* Only a single item */
    borg_items[slot].iqty = 1;

    /* Reduce the inventory quantity by one */
    borg_items[i].iqty--;

    /* Fix later */
    fix = TRUE;

    /* Examine the benefits of the swap item */
    borg_notice(FALSE);

    /* Evaluate the power with the new item worn */
    b_p1 = borg_danger(b_ptr->py,b_ptr->px,1);

    /* Restore the old item (empty) */
    COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

    /* Restore the new item */
    COPY(&borg_items[i], &borg_safe_items[i], auto_item);

    /* Restore bonuses */
    if (fix) borg_notice(TRUE);

    /* skip it if it has not been decursed */
    if ((item->flags3 & TR3_LIGHT_CURSE) ||
        (item->flags3 & TR3_HEAVY_CURSE)) b_p1 = 9999;

    /* Now we check the weapon */
    /* get the item */
    i = borg_weapon_swap;
    item = &borg_items[i];

    /* Where does it go */
    slot = INVEN_WIELD;

    /* Save the old item (empty) */
    COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

    /* Save the new item */
    COPY(&borg_safe_items[i], &borg_items[i], auto_item);

    /* Wear new item */
    COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

    /* Only a single item */
    borg_items[slot].iqty = 1;

    /* Reduce the inventory quantity by one */
    borg_items[i].iqty--;

    /* Fix later */
    fix = TRUE;

    /* Examine the inventory */
    borg_notice(FALSE);

    /* Evaluate the power with the new item worn */
    b_p2 = borg_danger(b_ptr->py,b_ptr->px,1);

    /* Restore the old item (empty) */
    COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

    /* Restore the new item */
    COPY(&borg_items[i], &borg_safe_items[i], auto_item);

    /* Restore bonuses */
    if (fix) borg_notice(TRUE);

	/* skip it if it has not been decursed */
    if ((item->flags3 & TR3_LIGHT_CURSE) ||
        (item->flags3 & TR3_HEAVY_CURSE)) b_p2 = 9999;

    /* Pass on the swap which yields the best result */
    if (b_p1 <= b_p2)
    {
        b_p = b_p1;
        swap = borg_armour_swap;
    }
    else
    {
        b_p = b_p2;
        swap = borg_weapon_swap;
    }

    /* good swap.  Make sure it helps a significant amount */
    if (p > b_p &&
        b_p <= (borg_avoid / borg_avoid_factor))
    {
        /* Log */
        borg_note(format("# Swapping backup.  (%d < %d).", b_p, p));

        /* Wear it */
        borg_keypress('w');
        borg_keypress((char)I2A(swap));

        /* Did something */
        return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * Wear useful equipment.
 *
 * Look through the inventory for equipment that is better than
 * the current equipment, and wear it, in an optimal order.
 *
 * Basically, we evaluate the world both with the current set of
 * equipment, and in the alternate world in which various items
 * are used instead of the items they would replace, and we take
 * one step towards the world in which we have the most "power".
 *
 * Although a player can actually wear two rings, we pretend that only
 * the "loose" ring can be removed, which is the semantics induced by
 * the "wear" command.
 *
 * The "borg_swap_rings()" code above occasionally allows us to remove
 * both rings, at which point this function will replace the "best" ring
 * on the "tight" finger, and the second "best" ring on the "loose" finger.
 */
bool borg_wear_stuff(void)
{
	int hole = INVEN_PACK - 1;

	int slot;

	s32b p, b_p = 0L, cp;

	int i, b_i = -1;

	auto_item *item;

	bool fix = FALSE;


    /* Hack -- not if "besting" -RML */
    /* XXX Allow if level <= 3 */
    if (!b_ptr->depth && borg_town_need_check_best &&
        b_ptr->lev > 3) return (FALSE);

	/* Hack not if we're wielding something that is cursed */
	if (borg_cursed) return (FALSE);

	/* Require an empty slot */
	if (borg_items[hole].iqty) return (FALSE);

	/* Notice inventory */
	borg_notice(TRUE);
	cp = borg_power();


	/* Scan inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		item = &borg_items[i];


		/* Skip empty items */
		if (!item->iqty) continue;


		/* Require "aware" */
		if (!item->kind) continue;

		/* Require known/average/blessed */
		if (!item->able && !item->average && !item->blessed) continue;

		/* Hack -- ignore "worthless" items */
		if (!item->value) continue;


		/* Where does it go */
		slot = borg_wield_slot(item);

		/* Cannot wear this item */
		if (slot < 0) continue;

		/* Save the old item */
		COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

		/* Save the new item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

		/* Save the hole */
		COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

		/* Take off old item */
		COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

		/* Wear new item */
		COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

		/* Only a single item */
		borg_items[slot].iqty = 1;

		/* Reduce the inventory quantity by one */
		borg_items[i].iqty--;

		/* Fix later */
		fix = TRUE;

		/* Examine the inventory */
		borg_notice(TRUE);

		/* Evaluate the inventory */
		p = borg_power();

		/* Restore the old item */
		COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

		/* Restore the new item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Restore the hole */
		COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

		/* Ignore "bad" swaps */
		if ((b_i >= 0) && (p < b_p)) continue;

		/* Consider if slot is empty XXX XXX XXX */

		/* Hack -- Ignore "equal" swaps */
		if ((b_i >= 0) && (p == b_p)) continue;

		/* Maintain the "best" */
		b_i = i; b_p = p;
	}

	/* Restore bonuses */
	if (fix) borg_notice(TRUE);

	/* No item */
	if ((b_i >= 0) && (b_p > borg_base_power))
	{
		/* Get the item */
		item = &borg_items[b_i];

		/* Log */
		borg_note(format("# Wearing %s.", item->desc));
		borg_note(format("# power from %d(%d) to %d",borg_base_power, cp, b_p));

		/* Send action (wear) */
		borg_keypress('w');

		/* Send item index */
		borg_send_item_index(b_i);

		/* Did something */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}



/*
 * Hack -- order of the slots
 *
 * Note that we ignore the "tight" ring, and we assume that we will always
 * be wearing our "best" ring on our "right" (tight) finger, and if we are
 * not, then the "borg_swap_rings()" function will remove both the rings,
 * which will induce the "borg_best_stuff()" function to put the rings back
 * on in the "optimal" order.  XXX XXX XXX
 */
static byte borg_best_stuff_order[] =
{
	INVEN_BOW,
	INVEN_WIELD,
	INVEN_BODY,
	INVEN_OUTER,
	INVEN_ARM,
	INVEN_HEAD,
	INVEN_HANDS,
	INVEN_FEET,
	INVEN_LITE,
	INVEN_NECK,
	INVEN_LEFT,
	255
};


/*
 * Helper function (see below)
 */
static void borg_best_stuff_aux(int n, byte *test, byte *best, s32b *vp)
{
	int i;

	int slot;


	/* Extract the slot */
	slot = borg_best_stuff_order[n];


	/* All done */
	if (slot == 255)
	{
		s32b p;

		/* Examine was FALSE*/
		borg_notice(TRUE);

		/* Evaluate */
		p = borg_power();

		/* Track best */
		if (p > *vp)
		{
			/* Save the results */
			for (i = 0; i < n; i++) best[i] = test[i];

			/* Use it */
			*vp = p;
		}

		/* Success */
		return;
	}


	/* Note the attempt */
    test[n] = 255;

	/* Evaluate the default item */
    borg_best_stuff_aux(n + 1, test, best, vp);


	/* Try other possible objects */
    for (i = 0; i < INVEN_PACK; i++)
	{
        auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Require "aware" */
		if (!item->kind) continue;

		/* Require known/average/blessed */
		if (!item->able && !item->average && !item->blessed) continue;

		/* Hack -- ignore "worthless" items */
		if (!item->value) continue;


		/* Make sure it goes in this slot */
		if (slot != borg_wield_slot(item)) continue;

		/* Take off old item */
		COPY(&borg_items[i], &borg_safe_items[slot], auto_item);

		/* Wear the new item */
		COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

		/* Note the attempt */
		test[n] = i;

		/* Evaluate the possible item */
        borg_best_stuff_aux(n + 1, test, best, vp);

		/* Restore equipment */
        COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

		/* Restore inventory */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);
	}

	/* Try other possible objects */
        for (i = 0; i < STORE_INVEN_MAX; i++)
        {
            auto_item *item = &borg_shops[7].ware[i];
    
            /* Skip empty items */
            if (!item->iqty) continue;
    
            /* Require "aware" */
            if (!item->kind) continue;
    
            /* Require known/average/blessed */
            if (!item->able && !item->average && !item->blessed) continue;
    
            /* Hack -- ignore "worthless" items */
            if (!item->value) continue;
    
    
            /* Make sure it goes in this slot */
            if (slot != borg_wield_slot(item)) continue;
    
            /* Take off old item */
            COPY(&borg_shops[7].ware[i], &borg_safe_items[slot], auto_item);
    
            /* Wear the new item */
            COPY(&borg_items[slot], &borg_safe_shops[7].ware[i], auto_item);
    
            /* Note the attempt */
            test[n] = i + INVEN_PACK;
    
            /* Evaluate the possible item */
            borg_best_stuff_aux(n + 1, test, best, vp);
    
            /* Restore equipment */
            COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);
    
            /* Restore inventory */
            COPY(&borg_shops[7].ware[i], &borg_safe_shops[7].ware[i], auto_item);
        }
}



/*
 * Attempt to instantiate the *best* possible equipment.
 */
bool borg_best_stuff(bool in_store)
{
	int hole = INVEN_PACK - 1;

	int slot;

	int k;

	int n = 0;

	s32b value;

	int i, b_i = -1;
	s32b p, b_p = 0L;

	byte test[12];
	byte best[12];

	auto_item *item;

	bool fix = FALSE;

    /* Require complete information -RML */
    if (!borg_shops[7].when) return (FALSE);

	/* Require an empty slot */
	if (borg_items[hole].iqty) return (FALSE);

	/* Hack -- Prevent infinite loops */
	if (time_this_panel > 500) return (FALSE);

	/* Hack -- Don't try this if we're wielding a cursed item (Calris) */
	if (borg_cursed) return (FALSE);

    /* Once maximized, stop -RML */
    /* XXX Use simpler borg_wear_stuff until level > 3 */
    if (!borg_town_need_check_best || b_ptr->lev <= 3) return (FALSE);


	/* Hack -- Initialize */
	for (k = 0; k < 12; k++)
	{
		/* Initialize */
		best[k] = test[k] = 255;
	}

	/* Hack -- Copy all the slots */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		/* Save the item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);
	}

    for (i = 0; i < STORE_INVEN_MAX; i++)
    {
        /* Save the item */
        COPY(&borg_safe_shops[7].ware[i], &borg_shops[7].ware[i], auto_item);
    }

	/* Evaluate the inventory */
	value = borg_base_power;
	
	/* Determine the best possible equipment */
    (void)borg_best_stuff_aux(0, test, best, &value);

	/* Restore bonuses was FALSE */
	borg_notice(TRUE);


	/* Find "easiest" step */
	for (k = 0; k < 12; k++)
	{
        slot = borg_best_stuff_order[k];

		/* Get choice */
		i = best[k];

        /* Ignore non-changes */
        if (i >= INVEN_PACK + STORE_INVEN_MAX) continue;

		/* Count changes */
		n++;

        if (i < INVEN_PACK)
        {
            /* Doing a pack change */
            if (!in_store) continue;

            /* Access the item */
            item = &borg_items[i];

            /* Access the slot */
            if (slot != borg_wield_slot(item))
            {
                borg_oops("slot mismatch in borg_best_stuff()");
                return (TRUE);
            }

            /* Save the old item */
            COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

            /* Save the new item */
            COPY(&borg_safe_items[i], &borg_items[i], auto_item);

            /* Save the hole */
            COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

            /* Take off old item */
            COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

            /* Wear new item */
            COPY(&borg_items[slot], &borg_safe_items[i], auto_item);

            /* Only a single item */
            borg_items[slot].iqty = 1;

            /* Reduce the inventory quantity by one */
            borg_items[i].iqty--;

            /* Fix later */
            fix = TRUE;

            /* Examine the inventory was FALSE */
            borg_notice(TRUE);

            /* Evaluate the inventory */
            p = borg_power();

            /* Restore the old item */
            COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

            /* Restore the new item */
            COPY(&borg_items[i], &borg_safe_items[i], auto_item);

            /* Restore the hole */
            COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

            /* Track the "best" change */
            if ((b_i >= 0) && (p < b_p))
            {
#ifdef EXTRA_SHOP_MESSAGES
                borg_note(format(
                    "# No improvement from besting '%s' in slot %i now",
                     borg_safe_items[i].desc, slot));
#endif

                continue;
            }


#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# Planning to best '%s' (weight: %d) in slot %i now",
                             borg_safe_items[i].desc, borg_safe_items[i].weight, slot));
			borg_note(format("# old power %d, new power %d",b_p,p));
#endif

			/* Maintain the "best" */
            b_i = i; b_p = p;
        }
        else
        {
            /* Doing a home change */
            if (in_store /* && borg_base_shop != 7 */ ) continue;

            /* Require two empty slots */
            if (borg_items[INVEN_PACK-1].iqty) continue;
            if (borg_items[INVEN_PACK-2].iqty) continue;

            /* Access the item */
            item = &borg_shops[7].ware[i-INVEN_PACK];

            /* Access the slot */
            if (slot != borg_wield_slot(item))
            {
                borg_oops("slot mismatch in borg_best_stuff()");
                return (TRUE);
            }


            /* Save the old item */
            COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

            /* Save the hole */
            COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

            /* Take off old item */
            COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

            /* Wear new item */
            COPY(&borg_items[slot], &borg_safe_shops[7].ware[i-INVEN_PACK], auto_item);

            /* Only a single item */
            borg_items[slot].iqty = 1;

            /* Fix later */
            fix = TRUE;

            /* Examine the inventory was FALSE*/
            borg_notice(TRUE);

            /* Evaluate the inventory */
            p = borg_power();

            /* Restore the old item */
            COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);

            /* Restore the hole */
            COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

            /* Track the "best" change */
            if ((b_i >= 0) && (p < b_p))
            {
#ifdef EXTRA_SHOP_MESSAGES
                borg_note(format(
                    "# No improvement from besting '%s' in slot %i now",
                    borg_safe_shops[7].ware[i-INVEN_PACK].desc, slot));
#endif

                continue;
            }

            /* Maintain the "best" */
            b_i = i; b_p = p;

#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# Planning to best '%s' (weight = %d) in slot %i now",
                             borg_safe_shops[7].ware[i-INVEN_PACK].desc,
							 borg_safe_shops[7].ware[i-INVEN_PACK].weight,
                             slot));
#endif
        }
    }

	/* Restore bonuses */
	if (fix) borg_notice(TRUE);


    /* If no changes, we're maximized -RML */
    if (n == 0)
    {
        borg_town_need_check_best = FALSE;
        borg_note("# Done thinking of besting equipment");
    }

	/* Start changing */
	if (b_i >= 0)
	{
        if (b_i < INVEN_PACK)
        {
            /* Get the item */
            auto_item *item = &borg_items[b_i];

            /* Log */
            borg_note(format("# Besting %s (1/%d).", item->desc, n));

            /* Send action (wear) */
            borg_keypress('w');

            /* Send item index */
            borg_send_item_index(b_i);

            /* Did something */
            return (TRUE);
        }
        else
        {
            if (in_store)
            {
                /* Get the item */
                auto_item *item = &borg_shops[7].ware[b_i-INVEN_PACK];

                /* XXX XXX XXX */
                return (FALSE);

                /* Go to the correct page */
                if (((b_i-INVEN_PACK) / 12) != borg_shops[7].page)
                {
                    /* Send next page key */
                    borg_keypress(' ');
                }

                /* Log */
                borg_note(format("# Grabbing %s for besting (in home).",
                                 item->desc));

#ifdef EXTRA_SHOP_MESSAGES
                borg_note(format("# b_i = %i, b_p = %d", b_i, b_p));
#endif

                /* Send count (one) */
                borg_keypresses("01");

                /* Send action (get) */
                borg_keypress('g');

                /* Send item index XXX XXX XXX */
                borg_keypress((char)I2A(b_i-INVEN_PACK));

                /* Hack -- Send price acceptance keys */
                borg_keypress('\n');
                borg_keypress('\n');
                borg_keypress('\n');
                borg_keypress('\n');

                /* Got an item */
                return (TRUE);
            }
            else
            {
                /* Get the item */
                auto_item *item = &borg_shops[7].ware[b_i-INVEN_PACK];

                /* Go to the home */
                borg_goal_shop = 7;
                borg_goal_ware = b_i - INVEN_PACK;

                /* Log */
                borg_note(format("# Grabbing %s for besting.", item->desc));

                return (TRUE);
            }
        }
	}


	/* Nope */
	return (FALSE);
}





/*
 * Study spells/prayers
 */
bool borg_study_magic(bool bored)
{
	int book, what;

	int i, r;

	int b_book = -1;
	int b_what = -1;
	int b_r = -1;


	/* Hack -- must use magic or prayers */
	if (!mb_ptr->spell_book) return (FALSE);


	/* Hack -- blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* Handle darkness XXX XXX XXX */

	/* Mages should not learn spells on level 2, 4, 6 and 8 */
	if ((b_ptr->pclass == CLASS_MAGE) &&
		((b_ptr->lev == 2) ||
		 (b_ptr->lev == 4) ||
		 (b_ptr->lev == 6) ||
		 (b_ptr->lev == 8)))
	{
		return (FALSE);
	}


	/* Check each book (backwards) */
	for (book = 9 - 1; book >= 0; book--)
	{
		/* Look for the book */
		i = borg_base_book[book];

		/* No such book */
		if (i < 0) continue;

		/* Check each spells */
		for (what = 9 - 1; what >= 0; what--)
		{
			auto_magic *as = &borg_magics[book][what];

			/* Require "learnable" status */
			if (as->status != BORG_MAGIC_OKAY) continue;

			/* Obtain "rating" */
			r = as->rating;

			/* Skip "boring" spells/prayers */
			if (!bored && (r <= 50)) continue;

			/* Skip "icky" spells/prayers */
			if (r <= 0) continue;

			/* Skip "worse" spells/prayers */
			if (r <= b_r) continue;

			/* Track it */
			b_book = book;
			b_what = what;
			b_r = r;
		}
	}

	/* Study */
	if (borg_base_is_study && (b_r > 0))
	{
		auto_magic *as = &borg_magics[b_book][b_what];

		/* Debugging Info */
		borg_note(format("# Studying spell/prayer %s.", as->name));

		/* Look for the book */
		i = borg_base_book[b_book];

		/* Send action (study) */
		borg_keypress('G');

		/* Send item index */
		borg_send_item_index(i);

		/* Send spell index (not prayer index) */
		if (mb_ptr->spell_book == TV_MAGIC_BOOK)
		{
			/* Send spell index */
			borg_keypress((char)I2A(b_what));
		}

		/* Success */
		return (TRUE);
	}


	/* Nope */
	return (FALSE);
}


/*
 * Try out spells/prayers for the experience
 */
bool borg_play_magic(void)
{
	int book, what;

	int i;


	/* Hack -- only in town */
	if (b_ptr->depth) return (FALSE);


	/* Hack -- must use magic or prayers */
	if (!mb_ptr->spell_book) return (FALSE);


	/* Hack -- blind/confused */
	if (borg_base_is_blind || borg_base_is_confused) return (FALSE);

	/* Handle darkness XXX XXX XXX */


	/* Check each book (backwards) */
	for (book = 9 - 1; book >= 0; book--)
	{
		/* Look for the book */
		i = borg_base_book[book];

		/* No such book */
		if (i < 0) continue;

		/* Check every spell (backwards) */
		for (what = 9 - 1; what >= 0; what--)
		{
			auto_magic *as = &borg_magics[book][what];

			/* Only try "untried" spells/prayers */
			if (as->status != BORG_MAGIC_TEST) continue;

			/* Ignore "bizarre" spells/prayers */
			if (as->method == BORG_MAGIC_OBJ) continue;
			if (as->method == BORG_MAGIC_WHO) continue;

			/* Can we cast it DvE */
			if (!borg_spell_okay(book, what)) continue;

			/* Note */
			borg_note("# Testing untried spell/prayer");

			/* Hack -- Use spell or prayer */
			if (borg_spell(book, what) ||
			    borg_prayer(book, what))
			{
				/* Hack -- Allow attack spells */
				if (as->method == BORG_MAGIC_AIM)
				{
					/* Send action (target) */
					borg_keypress('*');

					/* Send target self keys */
					borg_keypress('p');
					borg_keypress('t');
				}

				/* Success */
				return (TRUE);
			}
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Count the number of items worth "selling"
 *
 * This determines the choice of stairs.
 *
 * Total hack, by the way...  XXX XXX XXX
 */
static int borg_count_sell(void)
{
	int i, k = 0;

	s32b price;
	s32b greed;


	/* Calculate "greed" factor */
	/* not to greedy, stay in the dungeon */
	greed = (b_ptr->au / 100L) + 100L;

	/* Minimal greed */
    if (greed < 1000L) greed = 1000L;
    if (greed > 25000L) greed = 25000L;

	/* When there is a vault, we go for it */
	if (vault_on_level) greed = 25000L;

	/* Count "sellable" items */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Skip "crappy" items */
		if (item->value <= 0) continue;

        /* dont crush the swap weapon */
		if (i != 0) 
		{
			if (i == borg_weapon_swap) continue;
			if (i == borg_armour_swap) continue;
		}

		/* Obtain the base price */
		price = ((item->value < 30000L) ? item->value : 30000L);

		/* Skip cheap known/average items */
		if ((price * item->iqty < greed) &&
		    (item->able || item->average)) continue;

		/* Count remaining items */
		k++;
	}

	/* Result */
	return (k);
}


/*
 * Number of times we need to complete the given level
 * Depending on the class DvE
 */
int borg_happy_required(int depth)
{
#if 0
	/* Mages start very, very slow */
	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 10))
	{
		return 15;
	}

	if ((b_ptr->pclass == CLASS_MAGE) && (b_ptr->lev < 20))
	{
		return 10;
	}
	
	/* Move fast until stat gain */
	if (depth < 30) return 5;

	/* Very slow for a while */
	if (depth < 40) return 10;

	/* Faster for a while */
	if (depth < 60) return 7;

	/* Very fast */
	if (depth < 100) return 4;
#endif

	/* Default */
	return 1;
}

/*
 * Leave the level if necessary (or bored)
 */
bool borg_leave_level(bool bored)
{
	int k, g = 0;

    u32b min_wait;


	/* Hack -- waiting for "recall" */
	if (borg_recalling) return (FALSE);

	/* Don't leave if there are still uniques left */
	if (borg_unique_level) return (FALSE);

	/* don't leave if there's a vault on this level */
	/* this might cause a problem */
	if (vault_on_level) return (FALSE);

	/* Note level completion */
	if (bored && !borg_completed)
	{
		/* Note */
		borg_note(format("# Completed depth %d", b_ptr->depth));
		
		/* Level is boring */
		borg_completed = TRUE;
		
		/* Track happiness */
		if (b_ptr->depth >= borg_happy_depth - 1)
		{
			/* Track happiness */
			if (++borg_happy_count >= borg_happy_required(borg_happy_depth))
			{
				/* Don't update the happy level if we're not prepared DvE */
				if (borg_prepared(borg_happy_depth + 1))
				{

					/* Reset happiness */
					borg_happy_count = 0;
	
					/* Dive deeper */
					borg_happy_depth++;

					/* Note */
					borg_note(format("# Happy about depth %d", borg_happy_depth));
				}
			}
		}
		else
		{
			/* Check for max depth much deeper than happy depth */
			if (b_ptr->max_depth >= borg_happy_depth + 3)
			{
				/* Reset happiness */
				borg_happy_count = 0;

				/* Dive deeper */
				borg_happy_depth++;
			}
		}
	}


	/* Town */
	if (!b_ptr->depth)
	{
		/* Cancel rising */
		borg_rising = FALSE;

		/* Wait until bored */
		if (!bored) return (FALSE);

		/* Hack -- Recall into dungeon */
 		if ((b_ptr->max_depth >= 5) &&
		    (amt_recall > 2) &&
 		    borg_recall())
		{
			/* Note */
			borg_note("# Recalling into dungeon (for fun)...");

			/* Give it a shot */
			return (TRUE);
		}

		/* Take next stairs */
		borg_stair_more = TRUE;

		/* Attempt to use those stairs */
		if (borg_flow_stair_more(GOAL_BORE)) return (TRUE);

		/* Oops */
		return (FALSE);
	}


	/* Count sellable items */
	k = borg_count_sell();


	/* Dive when bored */
	if (bored) g = 1;

	/* Hack -- Power-dive downwards when able */
	if (b_ptr->lev > b_ptr->depth * 2 + 2) g = 1;


	/* Do not enter dangerous depths */
	if (g && !borg_prepared(b_ptr->depth + 1)) g = 0;

	/* Do not dive when "full" of items */
	if (g && (k >= 18)) g = 0;

	/* Do not dive when drained */
	if (g && borg_base_fix_exp) g = 0;


	/* Hack -- Power-dive upwards when needed */
	if (!borg_prepared(b_ptr->depth)) g = -1;

	/* Hack -- Stay on each level for a minimal amount of time */
    /* Changed to require less time for shallower levels -RML */
    if (b_ptr->depth > 0)
    {
        min_wait = value_feeling[borg_feeling] * b_ptr->depth /
                   b_ptr->max_depth;
    }
    else
        min_wait = 100;
    if (min_wait < (u32b)10 * extract_energy[b_ptr->pspeed])
        min_wait = 10 * extract_energy[b_ptr->pspeed];
    if (g && ((u32b)(borg_time - borg_when_began) < min_wait)) g = 0;


	/* Return to town when bored and unable to dive */
	if (bored && !borg_prepared(b_ptr->depth + 1)) borg_rising = TRUE;

	/* Return to town to sell stuff */
	if (bored && (k >= 18)) borg_rising = TRUE;

	/* Return to town when level drained */
	if (borg_base_fix_lev) borg_rising = TRUE;

	/* Return to town to restore experience */
	if (bored && borg_base_fix_exp) borg_rising = TRUE;


	/* Return to town */
	if (borg_rising) g = -1;

	/* Mega-Hack -- spend time on the first level to rotate shops */
	if ((b_ptr->depth == 1) && (borg_time - borg_when_began < 100) && (g < 0)) g = 0;


	/* Use random stairs when really bored */
	if (bored && (borg_time - borg_when_began >= 5000))
	{
		/* Note */
		borg_note("# Choosing random stairs.");

		/* Use random stairs */
		g = ((rand_int(100) < 50) ? -1 : 1);
	}


	/* Go Up */
	if (g < 0)
	{
		/* Take next stairs */
		borg_stair_less = TRUE;

		/* Let's not Recall when we're bored. DvE */
		/* Hack -- recall */
		/*
		if (borg_rising &&
		    (b_ptr->depth >= 5) &&
		    (amt_recall > 3) &&
		    borg_recall())
		{
			borg_note("# Recalling to town (for fun)");
			return (TRUE);
		}
		*/

		/* Attempt to use stairs */
		if (borg_flow_stair_less(GOAL_BORE)) return (TRUE);

		/* Cannot find any stairs */
		if (borg_rising && bored)
		{
			if (borg_recall())
			{
				borg_note("# Recalling to town (no stairs)");
				return (TRUE);
			}
		}
	}


	/* Go Down */
	if (g > 0)
	{
		/* Take next stairs */
		borg_stair_more = TRUE;

		/* Attempt to use those stairs */
		if (borg_flow_stair_more(GOAL_BORE)) return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * This file handles the highest level goals, and store interaction.
 *
 * Store interaction strategy
 *
 *   (1) Sell items to the home (for later use)
 *       We sell anything we may need later (see step 4)
 *
 *   (2) Sell items to the shops (for money)
 *       We sell anything we do not actually need
 *
 *   (3) Buy items from the shops (for the player)
 *       We buy things that we actually need
 *
 *   (4) Buy items from the home (for the player)
 *       We buy things that we actually need (see step 1)
 *
 *   (5) Buy items from the shops (for the home)
 *       We buy things we may need later (see step 1)
 *
 *   (6) Buy items from the home (for the stores)
 *       We buy things we no longer need (see step 2)
 *
 *   The basic principle is that we should always act to improve our
 *   "status", and we should sometimes act to "maintain" our status,
 *   especially if there is a monetary reward.  But first we should
 *   attempt to use the home as a "stockpile", even though that is
 *   not worth any money, since it may save us money eventually.
 */


static auto_item shop_save_item;

/*
 * Helper function -- find if an item would be removed from
 * the home immediately  -RML
 */
static bool borg_bad_home_sell_aux(int t_n)
{
	int icky = STORE_INVEN_MAX - 1;

	int n, b_n = -1;
	s32b s, b_s = 0L;
    s32b t_s = 0;


	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	b_s = borg_power_home();

    /* We need to take *something* */
    b_s = 0L;

	/* Scan the home */
	for (n = 0; n < STORE_INVEN_MAX; n++)
	{
		auto_item *item = &borg_shops[7].ware[n];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Save shop item */
        COPY(&shop_save_item, &borg_shops[7].ware[n], auto_item);

        /* Remove whole stack from shop */
        borg_shops[7].ware[n].iqty = 0;

		/* Examine the home */
		borg_notice_home();

		/* Evaluate the home */
		s = borg_power_home();

		/* Restore shop item */
        COPY(&borg_shops[7].ware[n], &shop_save_item, auto_item);

        /* Save the value of item being checked */
        if (n == t_n)
            t_s = s;

		/* Ignore "bad" sales */
        if (s <= b_s) continue;

		/* Maintain the "best" */
		b_n = n; b_s = s;
	}

	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	s = borg_power_home();

    /* Is it the item being checked (or something of equal use)? */
    if (b_n >= 0 && b_s == t_s)
        return (TRUE);

	/* Assume not */
	return (FALSE);
}

/*
 * Find the mininum amount of some item to buy/sell. For most
 * items this is 1, but for certain items (such as ammunition)
 * it may be higher.
 */
static int borg_min_item_quantity(auto_item *item)
{
    /* Only trade in bunches if sufficient cash */
    if (b_ptr->au < 250) return (1);

    /* Don't trade expensive items in bunches */
    if (item->cost > 5) return (1);

    /* Don't trade non-known, non-average items in bunches */
    if (!item->able && !item->average) return (1);

    /* Only allow some types */
    switch (item->tval)
    {
    case TV_SPIKE:
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        /* Maximum number of items */
        if (item->iqty < 5)
            return (item->iqty);

        return (5);

    default:
        return (1);
    }
}

/*
 * Determine if a item similar to some item is already in the home.
 *
 * Created by -RML
 */
static int borg_home_item_similar(auto_item *item)
{
    int n;
    int iqty;

    /* Save the item quantity */
    iqty = item->iqty;

    /* Only one item */
    item->iqty = borg_min_item_quantity(item);

    /* Scan the wares */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *test = &borg_shops[7].ware[n];

		/* Skip empty items */
        if (!test->iqty) continue;

		/* skip full slots */
		if (test->iqty == 99) continue;

		/* Skip un-aware items */
        if (!test->kind) continue;

        if (borg_object_similar(test, item))
        {
            /* Restore the item quantity */
            item->iqty = iqty;

            return (n);
        }
    }

    /* Restore the item quantity */
    item->iqty = iqty;

    return (-1);
}

/*
 * Step 1 -- sell "useful" things to the home (for later)
 */
static bool borg_think_home_sell_aux(void)
{
	int icky = STORE_INVEN_MAX - 1;
    int hole;

	int i, b_i = -1;
	s32b p, b_p = 0L;
	s32b s, b_s = 0L;

	bool fix = FALSE;

    bool bad_item;


	/* Hack -- prevent loops */
	if (borg_base_is_hungry) return (FALSE);

	/* Hack -- the home is full */
	if (borg_shops[7].ware[icky].iqty) return (FALSE);


	/* Evaluate the inventory */
	b_p = borg_base_power;


	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	b_s = borg_power_home();


	/* Sell stuff */
	for (i = 0; i < INVEN_PACK; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;
#if 0
        /* skip our swap weapon */
		if (i != 0)
		{
			if (i == borg_weapon_swap) continue;
			if (i == borg_armour_swap) continue;
		}
#endif
		/* Save the item */
		COPY(&borg_safe_items[i], &borg_items[i], auto_item);

        /* Find the place to put it in the store */        
        hole = borg_home_item_similar(item);

        /* XXX XXX XXX Describe */
        if (hole >= 0)
        {
#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# Planning to combine '%s' (in inven) ...",
                             item->desc));
            borg_note(format("# ... with '%s' (in home slot %i).",
                             borg_shops[7].ware[hole].desc, hole));
#endif
        }

        if (hole < 0)
        {
            /* Find a hole */
            for (hole = 0; hole < icky; hole++)
            {
                if (!borg_shops[7].ware[hole].iqty) break;
            }

            /* Save the store hole */
            COPY(&borg_safe_shops[7].ware[hole], &borg_shops[7].ware[hole], auto_item);

            /* Copy the item to the shop */
            COPY(&borg_shops[7].ware[hole], &borg_safe_items[i], auto_item);

            borg_shops[7].ware[hole].iqty = 0;
        }
        else
        {
            /* Save the store hole */
            COPY(&borg_safe_shops[7].ware[hole], &borg_shops[7].ware[hole], auto_item);
        }

        /* Give a single item (sometimes) */
        borg_shops[7].ware[hole].iqty += borg_min_item_quantity(item);
		if (borg_shops[7].ware[hole].tval == TV_WAND)
		{
			/* spread the charges */
			borg_shops[7].ware[hole].pval = borg_shops[7].ware[hole].pval * 
				(borg_shops[7].ware[hole].iqty / borg_items[i].iqty);			
		}

        /* Lose a single item (sometimes) */
        borg_items[i].iqty -= borg_min_item_quantity(item);
		if (borg_items[i].tval == TV_WAND)
		{
			/* spread the charges */
			borg_items[i].pval = borg_items[i].pval - borg_shops[7].ware[hole].pval;
		}

        /* Note: '*item' now changed */

		/* Fix later */
		fix = TRUE;

        /* Check for bad sales -RML */
        bad_item = FALSE;
        if (borg_shops[7].ware[icky].iqty &&
            borg_bad_home_sell_aux(hole))
        {
            bad_item = TRUE;
        }

		/* Examine the inventory */
		borg_notice(TRUE);
		borg_notice_home();

		/* Evaluate the inventory */
		p = borg_power();

		/* Examine the home */
		/* borg_notice_home(); */

		/* Evaluate the home */
		s = borg_power_home();

        /* Restore the store hole */
        COPY(&borg_shops[7].ware[hole], &borg_safe_shops[7].ware[hole], auto_item);

		/* Restore the item */
		COPY(&borg_items[i], &borg_safe_items[i], auto_item);

		/* Ignore "bad" sales */
        if (p < b_p)
        {
#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# Can't put '%s' in home (in use)",
                             item->desc));
#endif

            continue;
        }

        /* Ignore "bad" sales -RML */
        if (bad_item)
        {
#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# No room for '%s' in home",
                             item->desc));

            borg_note(format("# hole = %i, item->kind = %i",
                             borg_home_item_similar(item),
                             item->kind));
#endif

            continue;
        }

		/* Ignore "silly" sales */
        /* if ((p == b_p) && (s <= b_s)) continue; */

		/* hack -- Don't save empty wands and staffs in the home */
		if (((item->tval == TV_WAND) || (item->tval == TV_STAFF)) && 
			 (item->pval == 0))
		{
			borg_note(format("# No sense putting '%s' in home (no charges)",
							  item->desc));
			continue;
		}

        if (s <= b_s)
        {
#ifdef EXTRA_SHOP_MESSAGES
            borg_note(format("# No improvement from dropping '%s' in home",
                             item->desc));

            borg_note(format("# hole = %i, item->kind = %i",
                             borg_home_item_similar(item),
                             item->kind));
#endif

            continue;
        }

		/* Maintain the "best" */
        /* b_i = i; b_p = p; b_s = s; */

		b_i = i; b_s = s;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Planning to drop '%s' in home",
                         item->desc));

        borg_note(format("# hole = %i, item->kind = %i",
                         borg_home_item_similar(item),
                         item->kind));
#endif

	}

	/* Examine the player */
	if (fix) borg_notice(TRUE);
	

	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	s = borg_power_home();

	/* Stockpile */
	if (b_i >= 0)
	{
		/* Visit the home */
		borg_goal_shop = 7;

		/* Sell that item */
		borg_goal_item = b_i;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from inven to home, home power %i to %i",
		                 borg_items[borg_goal_item].desc,
                         s, b_s));

        borg_note(format("# hole = %i, item->kind = %i",
                         borg_home_item_similar(&borg_items[b_i]),
                         borg_items[b_i].kind));
#endif

		/* Success */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Determine if an item can be sold in the given store
 *
 * Never sell anything to the black market.
 */
static bool borg_good_sell(auto_item *item, int who)
{
	/* Never sell worthless items */
	if (item->value <= 0) return (FALSE);

    /* Hack -- never sell un-*ID*'d random artifacts */
//    if (item->name1 && item->do_star) return (FALSE);

	/* Analyze the type */
	switch (item->tval)
	{
		case TV_POTION:
		case TV_SCROLL:
		{
			/* Never sell if not "known" and interesting */
			if (!item->able && (b_ptr->max_depth > 5)) return (FALSE);
			break;
		}

		case TV_FOOD:
		case TV_ROD:
		case TV_WAND:
		case TV_STAFF:
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			/* Never sell if not "known" */
			if (!item->able) return (FALSE);
			break;
		}

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
		{
			/* Do not sell non-known, non-average, non-icky items */
			if (!item->able && !item->average &&
			    !borg_item_icky(item)) return (FALSE);

			break;
		}
	}


	/* Switch on the store */
	switch (who + 1)
	{
		/* General Store */
		case 1:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_DIGGING:
				case TV_CLOAK:
				case TV_FOOD:
				case TV_FLASK:
				case TV_LITE:
				case TV_SPIKE:
				{
					return (TRUE);
				}
			}
			break;
		}

		/* Armoury */
		case 2:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_BOOTS:
				case TV_GLOVES:
#if 0
				case TV_CLOAK:
#endif
				case TV_HELM:
				case TV_CROWN:
				case TV_SHIELD:
				case TV_SOFT_ARMOR:
				case TV_HARD_ARMOR:
				case TV_DRAG_ARMOR:
				{
					return (TRUE);
				}
			}
			break;
		}

		/* Weapon Shop */
		case 3:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_SHOT:
				case TV_BOLT:
				case TV_ARROW:
				case TV_BOW:
#if 0
				case TV_DIGGING:
				case TV_HAFTED:
#endif
				case TV_POLEARM:
				case TV_SWORD:
				{
					return (TRUE);
				}
			}
			break;
		}

		/* Temple */
		case 4:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_HAFTED:
#if 0
				case TV_SCROLL:
				case TV_POTION:
#endif
				case TV_PRAYER_BOOK:
				{
					return (TRUE);
				}
			}
			break;
		}

		/* Alchemist */
		case 5:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_SCROLL:
				case TV_POTION:
				{
					return (TRUE);
				}
			}
			break;
		}

		/* Magic Shop */
		case 6:
		{
			/* Analyze the type */
			switch (item->tval)
			{
				case TV_MAGIC_BOOK:
				case TV_AMULET:
				case TV_RING:
#if 0
				case TV_SCROLL:
				case TV_POTION:
#endif
				case TV_STAFF:
				case TV_WAND:
				case TV_ROD:
				{
					return (TRUE);
				}
			}
			break;
		}
	}

	/* Assume not */
	return (FALSE);
}



/*
 * Step 2 -- sell "useless" items to a shop (for cash)
 */
static bool borg_think_shop_sell_aux(void)
{
	int icky = STORE_INVEN_MAX - 1;

    int qty;

	int k, b_k = -1;
	int i, b_i = -1;
	s32b p, b_p = 0L;
	s32b c, b_c = 0L;

	bool fix = FALSE;


	/* Evaluate */
	borg_notice(TRUE);
	borg_notice_home();
	/* b_p = borg_base_power; */
	b_p = borg_power();

	borg_note("# Selling stuff");

	/* Check each shop */
	for (k = 0; k < 7; k++)
	{
		/* Hack -- Skip "full" shops */
		if (borg_shops[k].ware[icky].iqty) continue;

		/* Save the store hole */
		COPY(&borg_safe_shops[k].ware[icky], &borg_shops[k].ware[icky], auto_item);

		/* Sell stuff */
		for (i = 0; i < INVEN_PACK; i++)
		{
			auto_item *item = &borg_items[i];

			/* Skip empty items */
			if (!item->iqty) continue;
#if 0
			/* skip our swap weapon */
			if (i !=0 )
			{
				if (i == borg_weapon_swap) continue;
				if ((i == borg_armour_swap) && (item->iqty < 2)) continue;
			}
#endif

			/* Skip "bad" sales */
			if (!borg_good_sell(item, k)) continue;

			/* Save the item */
			COPY(&borg_safe_items[i], &borg_items[i], auto_item);

			/* Give the item to the shop */
			COPY(&borg_shops[k].ware[icky], &borg_safe_items[i], auto_item);

            /* Save the number */
            qty = borg_min_item_quantity(item);

            /* Give a single item (sometimes) */
            borg_shops[k].ware[icky].iqty = qty;

            /* Lose a single item (sometimes) */
            borg_items[i].iqty -= qty;

			if (borg_items[i].tval == TV_WAND)
			{
				/* spread the charges */
				borg_items[i].pval = borg_items[i].pval * qty/(qty +borg_items[i].iqty);
				borg_shops[k].ware[icky].pval -= borg_items[i].pval;
			}


            /* Note: '*item' now changed */

			/* Fix later */
			fix = TRUE;

			/* Examine the inventory */
			borg_notice(TRUE);
			borg_notice_home();

			/* Evaluate the inventory */
			p = borg_power();

			/* Restore the item */
			COPY(&borg_items[i], &borg_safe_items[i], auto_item);

			/* Ignore "bad" sales */
			if (p < b_p) continue;

			/* Extract the "price" */
            c = ((item->value < 30000L) ? item->value : 30000L) * qty;

			/* Ignore "cheaper" items */
			if ((p == b_p) && (c <= b_c)) continue;

			/* Maintain the "best" */
			b_k = k; b_i = i; b_p = p; b_c = c;
		}

		/* Restore the store hole */
		COPY(&borg_shops[k].ware[icky], &borg_safe_shops[k].ware[icky], auto_item);
	}

	/* Examine the inventory */
	if (fix)
	{
		borg_notice(TRUE);
		borg_notice_home();
	}

	/* Sell something (if useless) */
	if ((b_k >= 0) && (b_i >= 0))
	{
		auto_item *item = &borg_items[b_i];
		/* Hack -- Calris can't be sold */
		if (item->name1 == ART_CALRIS)
		{
			borg_note("# Dropping Calris");
			item->value = 0;
			borg_do_crush_junk = TRUE;
		}
		else 
		{
			/* Visit that shop */
			borg_goal_shop = b_k;
	
			/* Sell that item */
			borg_goal_item = b_i;

#ifdef EXTRA_SHOP_MESSAGES
	        borg_note(format("# Move '%s' from inven to store, power %i to %i",
			                 borg_items[borg_goal_item].desc,
			                 borg_base_power, b_p));
#endif
		}

		/* Success */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}



/*
 * Help decide if an item should be bought from a real store
 *
 * We prevent the purchase of enchanted (or expensive) ammo,
 * so we do not spend all our money on temporary power.
 *
 * We prevent the purchase of items from the black market
 * which are often available at normal stores, currently,
 * this includes low level books, and all wands and staffs.
 */
static bool borg_good_buy(auto_item *item, int who)
{
	/* Check the object */
	switch (item->tval)
	{
		case TV_DIGGING:
		{
			return (FALSE);
		}

		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			if (item->to_h) return (FALSE);
			if (item->to_d) return (FALSE);
			if (who == 6) return (FALSE);
			break;
		}

		case TV_MAGIC_BOOK:
		case TV_PRAYER_BOOK:
		{
			if (item->sval >= 4) break;
			if (who == 6) return (FALSE);
			break;
		}

		case TV_WAND:
		case TV_STAFF:
		{
			if (who == 6) return (FALSE);
			break;
		}
	}


	/* Okay */
	return (TRUE);
}



/*
 * Step 3 -- buy "useful" things from a shop (to be used)
 */
static bool borg_think_shop_buy_aux(void)
{
	int hole = INVEN_PACK - 1;

	int slot;

    int qty;

	int k, b_k = -1;
	int n, b_n = -1;
	s32b p, b_p = 0L;
	s32b c, b_c = 0L;

	bool fix = FALSE;

	/* Require one empty slot */
	if (borg_items[hole].iqty) return (FALSE);


	/* Extract the "power" */
	borg_notice(TRUE);
	borg_notice_home();
	b_p = borg_power();


	/* Check the shops */
	for (k = 0; k < 7; k++)
	{
		/* Hack -- Don't buy from shop 2, 3 or 7 when */
		/* we're wielding a cursed item */
		if (borg_cursed) continue;

		/* Scan the wares */
		for (n = 0; n < STORE_INVEN_MAX; n++)
		{
			auto_item *item = &borg_shops[k].ware[n];

			/* Skip empty items */
			if (!item->iqty) continue;

			/* Skip "bad" buys */
			if (!borg_good_buy(item, k)) continue;

			/* Hack -- Require "sufficient" cash */
			if (b_ptr->au < item->cost * 12 / 10) continue;

			/* Save shop item */
			COPY(&borg_safe_shops[k].ware[n], &borg_shops[k].ware[n], auto_item);

			/* Save hole */
			COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

            /* Save the number to trade */
            qty = borg_min_item_quantity(item);

            /* Remove one item from shop (sometimes) */
            borg_shops[k].ware[n].iqty -= qty;

            /* Note: '*item' is now changed */

			/* Obtain "slot" */
			slot = borg_wield_slot(item);

			/* Consider new equipment */
			if (slot >= 0)
			{
				/* Save old item */
				COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

				/* Move equipment into inventory */
				COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

				/* Move new item into equipment */
				COPY(&borg_items[slot], &borg_safe_shops[k].ware[n], auto_item);

                /* Only a single item (always) */
                borg_items[slot].iqty = qty;

				/* Fix later */
				fix = TRUE;

				/* Examine the inventory */
				borg_notice(TRUE);
				borg_notice_home();

				/* Evaluate the inventory */
				p = borg_power();

				/* Restore old item */
				COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);
			}

			/* Consider new inventory */
			else
			{
				/* Move new item into inventory */
				COPY(&borg_items[hole], &borg_safe_shops[k].ware[n], auto_item);

                /* Only a single item (sometimes) */
                borg_items[hole].iqty = qty;
				if (borg_items[hole].tval == TV_WAND)
				{	
					/* spread the charges */
					borg_items[hole].pval = borg_items[hole].pval * qty/(qty +borg_shops[k].ware[n].iqty);
					borg_shops[k].ware[n].pval -= borg_items[hole].pval;
				}

				/* Fix later */
				fix = TRUE;

				/* Examine the inventory */
				borg_notice(TRUE);
				borg_notice_home();

				/* Evaluate the equipment */
				p = borg_power();
			}

			/* Restore hole */
			COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

			/* Restore shop item */
			COPY(&borg_shops[k].ware[n], &borg_safe_shops[k].ware[n], auto_item);

			/* Obtain the "cost" of the item */
            c = item->cost * qty;

			/* Penalize the cost of expensive items */
			if (c > b_ptr->au / 10) p -= c;

			/* Ignore "bad" purchases */
			if (p <= b_p) continue;

			/* Ignore "expensive" purchases */
			if ((p == b_p) && (c >= b_c)) continue;

			/* Save the item and cost */
			b_k = k; b_n = n; b_p = p; b_c = c;
		}
	}

	/* Examine the inventory */
	if (fix) 
	{
		borg_notice(TRUE);
		borg_notice_home();
	}

	/* Buy something */
	if ((b_k >= 0) && (b_n >= 0))
	{
		/* Visit that shop */
		borg_goal_shop = b_k;

		/* Buy that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from store to inven, power %i to %i",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         borg_base_power, b_p));
#endif

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Step 4 -- buy "useful" things from the home (to be used)
 */
static bool borg_think_home_buy_aux(void)
{
	int hole = INVEN_PACK - 1;

    int qty;

	int slot;

	int n, b_n = -1;
	s32b p, b_p = 0L;

	bool fix = FALSE;


	/* Require one empty slot */
	if (borg_items[hole].iqty) return (FALSE);

	/* Hack not if we're wielding something that is cursed */
	if (borg_cursed) return (FALSE);


	/* Extract the "power" */
	b_p = borg_base_power;


	/* Scan the home */
	for (n = 0; n < STORE_INVEN_MAX; n++)
	{
		auto_item *item = &borg_shops[7].ware[n];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Save shop item */
		COPY(&borg_safe_shops[7].ware[n], &borg_shops[7].ware[n], auto_item);

		/* Save hole */
		COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

        /* Save the number */
        qty = borg_min_item_quantity(item);

        /* Remove one item from shop (sometimes) */
        borg_shops[7].ware[n].iqty -= qty;

		/* Obtain "slot" */
		slot = borg_wield_slot(item);

		/* Consider new equipment */
		if (slot >= 0)
		{
			/* Save old item */
			COPY(&borg_safe_items[slot], &borg_items[slot], auto_item);

			/* Move equipment into inventory */
			COPY(&borg_items[hole], &borg_safe_items[slot], auto_item);

			/* Move new item into equipment */
			COPY(&borg_items[slot], &borg_safe_shops[7].ware[n], auto_item);

            /* Only a single item (always) */
            borg_items[slot].iqty = qty;

			/* Fix later */
			fix = TRUE;

			/* Examine the inventory */
			borg_notice(TRUE);
			borg_notice_home();

			/* Evaluate the inventory */
			p = borg_power();

			/* Restore old item */
			COPY(&borg_items[slot], &borg_safe_items[slot], auto_item);
		}

		/* Consider new inventory */
		else
		{
			/* Move new item into inventory */
			COPY(&borg_items[hole], &borg_safe_shops[7].ware[n], auto_item);

            /* Only a single item (sometimes) */
            borg_items[hole].iqty = qty;
			if (borg_items[hole].tval == TV_WAND)
			{
				/* spread the charges */
				borg_items[hole].pval = borg_items[hole].pval * qty/(qty +borg_shops[7].ware[n].iqty);
				borg_shops[7].ware[n].pval -= borg_items[hole].pval;
			}

			/* Fix later */
			fix = TRUE;

			/* Examine the inventory */
			borg_notice(TRUE);
			borg_notice_home();

			/* Evaluate the equipment */
			p = borg_power();
		}

		/* Restore hole */
		COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

		/* Restore shop item */
		COPY(&borg_shops[7].ware[n], &borg_safe_shops[7].ware[n], auto_item);

		/* Ignore "silly" purchases */
		if (p <= b_p) continue;

		/* Save the item and cost */
		b_n = n; b_p = p;
	}

	/* Examine the inventory */
	if (fix) 
	{
		borg_notice(TRUE);
		borg_notice_home();
	}

	/* Buy something */
	if ((b_n >= 0) && (b_p > p))
	{
		/* Go to the home */
		borg_goal_shop = 7;

		/* Buy that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from home to inven, power %i to %i",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         borg_base_power, b_p));
#endif

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}



/*
 * Step 5 -- buy "interesting" things from a shop (to be used later)
 */
static bool borg_think_shop_grab_aux(void)
{
	int icky = STORE_INVEN_MAX - 1;
    int hole;

    int qty;

	int k, b_k = -1;
	int n, b_n = -1;
	s32b s, b_s = 0L;
    s32b c, b_c = -1L;

    bool bad_buy;


	/* Hack -- the home is full */
	if (borg_shops[7].ware[icky].iqty) return (FALSE);


	/* Require two empty slots */
	if (borg_items[INVEN_PACK-1].iqty) return (FALSE);
	if (borg_items[INVEN_PACK-2].iqty) return (FALSE);


	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	b_s = borg_power_home();


	/* Check the shops */
	for (k = 0; k < 7; k++)
	{
		/* Scan the wares */
		for (n = 0; n < STORE_INVEN_MAX; n++)
		{
			auto_item *item = &borg_shops[k].ware[n];

			/* Skip empty items */
			if (!item->iqty) continue;

			/* Skip "bad" buys */
			if (!borg_good_buy(item, k)) continue;

			/* Hack -- Require some "extra" cash */
			if (b_ptr->au < 1000L + item->cost * 5) continue;

			/* Save shop item */
			COPY(&borg_safe_shops[k].ware[n], &borg_shops[k].ware[n], auto_item);

            /* Save number */
            qty = borg_min_item_quantity(item);

            /* Find the place to put it in the store */        
            hole = borg_home_item_similar(item);

            /* XXX XXX XXX Describe */
            if (hole >= 0)
            {
#ifdef EXTRA_SHOP_MESSAGES
                borg_note(format("# Planning to combine '%s' (in shop) ...",
                                 item->desc));
                borg_note(format("# ... with '%s' (in home slot %i).",
                                 borg_shops[7].ware[hole].desc, hole));
#endif
            }

            if (hole < 0)
            {
                /* Find a hole */
                for (hole = 0; hole < icky; hole++)
                {
                    if (!borg_shops[7].ware[hole].iqty) break;
                }
    
                /* Save the store hole */
                COPY(&borg_safe_shops[7].ware[hole], &borg_shops[7].ware[hole], auto_item);
    
                /* Copy the item to the shop */
                COPY(&borg_shops[7].ware[hole], &borg_safe_shops[k].ware[n], auto_item);
    
                borg_shops[7].ware[hole].iqty = 0;
            }
            else
            {
                /* Save the store hole */
                COPY(&borg_safe_shops[7].ware[hole], &borg_shops[7].ware[hole], auto_item);
            }

            /* Give a single item (sometimes) */
            borg_shops[7].ware[hole].iqty += qty;

            /* Remove one item from shop (sometimes) */
            borg_shops[k].ware[n].iqty -= qty;
			if (borg_shops[7].ware[hole].tval == TV_WAND)
			{
				/* spread the charges */
				borg_shops[7].ware[hole].pval = borg_shops[7].ware[hole].pval * qty/(qty + borg_shops[k].ware[n].iqty);
				borg_shops[k].ware[n].pval -= borg_shops[7].ware[hole].pval;
			}

            /* Check for immediate rebuy -RML */
            bad_buy = FALSE;
            if (borg_shops[7].ware[icky].iqty &&
                borg_bad_home_sell_aux(hole))
            {
                bad_buy = TRUE;
            }

			/* Examine the home */
			borg_notice_home();

			/* Evaluate the home */
			s = borg_power_home();

			/* Restore shop item */
			COPY(&borg_shops[k].ware[n], &borg_safe_shops[k].ware[n], auto_item);

            /* Restore the store hole */
            COPY(&borg_shops[7].ware[hole], &borg_safe_shops[7].ware[hole], auto_item);

            /* Ignore "bad" sales */
            if (bad_buy) continue;

			/* Obtain the "cost" of the item */
            c = item->cost * qty;

			/* Penalize expensive items */
			if (c > b_ptr->au / 10) s -= c;

			/* Ignore "bad" sales */
            if (s < b_s) continue;

			/* Ignore "expensive" purchases */
			if ((s == b_s) && (c >= b_c)) continue;

			/* Save the item and cost */
			b_k = k; b_n = n; b_s = s; b_c = c;
		}
	}

	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	s = borg_power_home();

	/* Buy something */
	if ((b_k >= 0) && (b_n >= 0))
	{
		/* Visit that shop */
		borg_goal_shop = b_k;

		/* Buy that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from store to inven, home power %i to %i",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         s, b_s));

        borg_note(format("# hole = %i, item->kind = %i",
                         borg_home_item_similar(&borg_shops[b_k].ware[b_n]),
                         borg_shops[b_k].ware[b_n].kind));
#endif

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Step 6 -- take "useless" things from the home (to be sold)
 */
static bool borg_think_home_grab_aux(void)
{
	int n, b_n = -1;
	s32b s, b_s = 0L;

    int qty;


	/* Require two empty slots */
	if (borg_items[INVEN_PACK-1].iqty) return (FALSE);
	if (borg_items[INVEN_PACK-2].iqty) return (FALSE);


	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	b_s = borg_power_home();

	/* Hack -- do nothing if home-power = 0 */
	if (b_s == 0) return (FALSE);


	/* Scan the home */
	for (n = 0; n < STORE_INVEN_MAX; n++)
	{
		auto_item *item = &borg_shops[7].ware[n];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Save shop item */
		COPY(&borg_safe_shops[7].ware[n], &borg_shops[7].ware[n], auto_item);

        /* Save quantity */
        qty = borg_min_item_quantity(item);

        /* Remove one item from shop (sometimes) */
        borg_shops[7].ware[n].iqty -= qty;
		if (borg_shops[7].ware[n].tval == TV_WAND)
		{
			/* spread the charges */
			/* Hack, wands sell by 1 */
			borg_shops[7].ware[n].pval /= borg_safe_shops[7].ware[n].iqty;
		}

		/* Examine the home */
		borg_notice_home();

		/* Evaluate the home */
		s = borg_power_home();

		/* Restore shop item */
		COPY(&borg_shops[7].ware[n], &borg_safe_shops[7].ware[n], auto_item);

		/* Ignore "bad" sales */
		if (s < b_s) continue;

		/* Maintain the "best" */
		b_n = n; b_s = s;
	}

	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	s = borg_power_home();

	/* Stockpile */
	if (b_n >= 0)
	{
		/* Visit the home */
		borg_goal_shop = 7;

		/* Grab that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from home to inven, home power %i to %i",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         s, b_s));
#endif

		/* Success */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Step 7 -- take something from the home (to make room) -RML
 */
static bool borg_think_home_clear_aux(void)
{
	int icky = STORE_INVEN_MAX - 1;

	int n, b_n = -1;
	s32b s, b_s = 0L;


    /* Only if the home is full */
    if (!borg_shops[7].ware[icky].iqty) return (FALSE);


	/* Require two empty slots */
	if (borg_items[INVEN_PACK-1].iqty) return (FALSE);
	if (borg_items[INVEN_PACK-2].iqty) return (FALSE);


	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	b_s = borg_power_home();

    /* We need to take *something* */
    b_s = 0L;

	/* Scan the home */
	for (n = 0; n < STORE_INVEN_MAX; n++)
	{
		auto_item *item = &borg_shops[7].ware[n];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Save shop item */
		COPY(&borg_safe_shops[7].ware[n], &borg_shops[7].ware[n], auto_item);

        /* Remove whole stack from shop */
        borg_shops[7].ware[n].iqty = 0;

		/* Examine the home */
		borg_notice_home();

		/* Evaluate the home */
		s = borg_power_home();

		/* Restore shop item */
		COPY(&borg_shops[7].ware[n], &borg_safe_shops[7].ware[n], auto_item);

		/* Ignore "bad" sales */
		if (s < b_s) continue;

		/* Maintain the "best" */
		b_n = n; b_s = s;
	}

	/* Examine the home */
	borg_notice_home();

	/* Evaluate the home */
	s = borg_power_home();

	/* Stockpile */
	if (b_n >= 0)
	{
		/* Visit the home */
		borg_goal_shop = 7;

		/* Grab that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from home to inven, home power %i to %i",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         s, b_s));
#endif

		/* Success */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}

/*
 * Grab a random artifact from the home to *Identify*.
 */
static bool borg_think_home_grab_ident(void)
{
	int n, b_n = -1;
	s32b s, b_s = 0L;


	/* Require two empty slots */
	if (borg_items[INVEN_PACK-1].iqty) return (FALSE);
	if (borg_items[INVEN_PACK-2].iqty) return (FALSE);

    /* Require a scroll of *Identify* */
    if (borg_slot(TV_SCROLL, SV_SCROLL_STAR_IDENTIFY) < 0)
        return (FALSE);


	/* Examine the home */
	borg_notice_home();

	/* Scan the home */
	for (n = 0; n < STORE_INVEN_MAX; n++)
	{
		auto_item *item = &borg_shops[7].ware[n];

		/* Skip empty items */
		if (!item->iqty) continue;

        /* Skip non-artifacts */
        if (!item->name1) continue;

        /* Skip nonrandom or already-*ID*'d items */
        if (!item->do_star) continue;

        /* XXX XXX XXX Prioritize */
        s = 100;

        /* Ignore "bad" picks */
        if (s < b_s) continue;

		/* Maintain the "best" */
		b_n = n; b_s = s;
	}

	/* Stockpile */
	if (b_n >= 0)
	{
		/* Visit the home */
		borg_goal_shop = 7;

		/* Grab that item */
		borg_goal_ware = b_n;

#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# Move '%s' from home to inven",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
#endif

		/* Success */
		return (TRUE);
	}

	/* Assume not */
	return (FALSE);
}

/*
 * Step 7A -- buy "useful" weapons from the home (to be used as a swap)
 */
static bool borg_think_home_buy_swap_weapon(void)
{
    int hole;

    int slot;
    int old_weapon_swap;
    s32b old_weapon_swap_value;
    int old_armour_swap;
    s32b old_armour_swap_value;
    int n, b_n = -1;
    s32b p, b_p = 0L;

    bool fix = FALSE;

    /* save the current values */
    old_weapon_swap = borg_weapon_swap;
    old_weapon_swap_value =  borg_weapon_swap_value;
    old_armour_swap = borg_armour_swap;
    old_armour_swap_value =  borg_armour_swap_value;

    if (borg_weapon_swap <= 0 || borg_weapon_swap_value <=0)
    {
        hole = INVEN_PACK - 1;
        borg_weapon_swap_value = -1L;
    }
    else
    {
        hole = borg_weapon_swap;
    }

    /* Require one empty slot */
    if (borg_items[hole].iqty) return (FALSE);


    /* Extract the "power" */
    b_p = borg_weapon_swap_value;

    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &borg_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Obtain "slot" make sure its a weapon */
        slot = borg_wield_slot(item);
        if (slot != INVEN_WIELD) continue;
        
        /* Save shop item */
        COPY(&borg_safe_shops[7].ware[n], &borg_shops[7].ware[n], auto_item);

        /* Save hole */
        COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

        /* Remove one item from shop */
        borg_shops[7].ware[n].iqty--;


        /* Consider new equipment */
        if (slot == INVEN_WIELD)
        {
            /* Move new item into inventory */
            COPY(&borg_items[hole], &borg_safe_shops[7].ware[n], auto_item);

            /* Only a single item */
            borg_items[hole].iqty = 1;

            /* Fix later */
            fix = TRUE;

            /* Examine the iventory and swap value*/
            borg_notice(TRUE);

            /* Evaluate the new equipment */
            p = borg_weapon_swap_value;
        }

        /* Restore hole */
        COPY(&borg_items[hole], &borg_items[hole], auto_item);

        /* Restore shop item */
        COPY(&borg_shops[7].ware[n], &borg_safe_shops[7].ware[n], auto_item);

        /* Ignore "silly" purchases */
        if (p <= b_p) continue;

        /* Save the item and value */
        b_n = n; b_p = p;
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_n >= 0) && (b_p > borg_weapon_swap_value))
    {
        /* Go to the home */
	    borg_goal_shop = 7;

        /* Buy that item */
        borg_goal_ware = b_n;

        /* Restore the values */
        borg_weapon_swap = old_weapon_swap;
        borg_weapon_swap_value =  old_weapon_swap_value;
        borg_armour_swap = old_armour_swap;
        borg_armour_swap_value =  old_armour_swap_value;

        /* Success */
        return (TRUE);
    }

    /* Restore the values */
        borg_weapon_swap = old_weapon_swap;
        borg_weapon_swap_value =  old_weapon_swap_value;
        borg_armour_swap = old_armour_swap;
        borg_armour_swap_value =  old_armour_swap_value;

    /* Nope */
    return (FALSE);
}

/*
 * Step 7B -- buy "useful" armour from the home (to be used as a swap)
 */
static bool borg_think_home_buy_swap_armour(void)
{
    int hole;

    int slot;

    int n, b_n = -1;
    s32b p, b_p = 0L;
    bool fix = FALSE;
    int old_weapon_swap;
    s32b old_weapon_swap_value;
    int old_armour_swap;
    s32b old_armour_swap_value;

    /* save the current values */
    old_weapon_swap = borg_weapon_swap;
    old_weapon_swap_value =  borg_weapon_swap_value;
    old_armour_swap = borg_armour_swap;
    old_armour_swap_value =  borg_armour_swap_value;

    if (borg_armour_swap <= 1 || borg_armour_swap_value <=0 )
    {
        hole = INVEN_PACK - 1;
        borg_armour_swap_value = -1L;
    }
    else
    {
        hole = borg_armour_swap;
    }


    /* Require one empty slot */
   if (borg_items[hole].iqty) return (FALSE);


    /* Extract the "power" */
    b_p = borg_armour_swap_value;


    /* Scan the home */
    for (n = 0; n < STORE_INVEN_MAX; n++)
    {
        auto_item *item = &borg_shops[7].ware[n];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Obtain "slot" make sure its an armour */
        slot = borg_wield_slot(item);
        if (slot <= INVEN_LITE) continue;

        /* Save shop item */
        COPY(&borg_safe_shops[7].ware[n], &borg_shops[7].ware[n], auto_item);

        /* Save hole */
        COPY(&borg_safe_items[hole], &borg_items[hole], auto_item);

        /* Remove one item from shop */
        borg_shops[7].ware[n].iqty--;

        /* Consider new equipment */
        if (slot >= INVEN_BODY)
        {
            /* Move new item into inventory */
            COPY(&borg_items[hole], &borg_shops[7].ware[n], auto_item);

            /* Only a single item */
            borg_items[hole].iqty = 1;

            /* Fix later */
            fix = TRUE;

            /* Examine the inventory (false)*/
            borg_notice(TRUE);

            /* Evaluate the new equipment */
            p = borg_armour_swap_value;
        }

        /* Restore hole */
        COPY(&borg_items[hole], &borg_safe_items[hole], auto_item);

        /* Restore shop item */
        COPY(&borg_shops[7].ware[n], &borg_safe_shops[7].ware[n], auto_item);

        /* Ignore "silly" purchases */
        if (p <= b_p) continue;

        /* Save the item and value */
        b_n = n; b_p = p;
    }

    /* Examine the inventory */
    if (fix) borg_notice(TRUE);

    /* Buy something */
    if ((b_n >= 0) && (b_p > borg_armour_swap_value))
    {
        /* Go to the home */
        borg_goal_shop = 7;

        /* Buy that item */
        borg_goal_ware = b_n;

        /* Restore the values */
        borg_weapon_swap = old_weapon_swap;
        borg_weapon_swap_value = old_weapon_swap_value;
        borg_armour_swap = old_armour_swap;
        borg_armour_swap_value = old_armour_swap_value;

        /* Success */
        return (TRUE);
    }
    /* Restore the values */
    borg_weapon_swap = old_weapon_swap;
    borg_weapon_swap_value = old_weapon_swap_value;
    borg_armour_swap = old_armour_swap;
    borg_armour_swap_value = old_armour_swap_value;

    /* Nope */
    return (FALSE);
}        

/*
 * Choose a shop to visit (see above)
 *
 */
static bool borg_choose_shop(void)
{
	int i;


	/* Must be in town */
	if (b_ptr->depth) return (FALSE);

	/*apw Forbid if been sitting on level forever */
    /*    Just come back and work through the loop later */
    if (time_this_panel > 1000) return (FALSE);

	/* Assume no important shop */
	borg_goal_shop = borg_goal_ware = borg_goal_item = -1;

	/* Need Restore STR? */
	if (borg_base_fix_stat[A_STR]) borg_goal_shop = 4;

	/* If poisoned or bleeding -- flow to temple */
    if (borg_base_is_cut || borg_base_is_poisoned) borg_goal_shop = 3;

    /* If Starving  -- flow to general store */
    if (borg_base_is_weak || (!b_ptr->cur_lite && b_ptr->lev >= 2)) borg_goal_shop = 0;

	/* If poisoned or bleeding -- Buy items from temple */
    if (!b_ptr->cur_lite || 
		 borg_base_is_weak || 
		 borg_base_is_cut || 
		 borg_base_is_poisoned ||
		 borg_base_fix_stat[A_STR])
    {
       if (borg_think_shop_buy_aux())
       {
            /* Message */
            borg_note(format("# Buying '%s' at '%s' (for player)",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
                         (f_name + f_info[0x08+borg_goal_shop].name)));

            /* Success */
            return (TRUE);
        }

        /* if temple is out of healing stuff, try the house */
        if (borg_think_home_buy_aux())
        {
            /* Message */
            borg_note(format("# Buying '%s' from the home",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));

            /* Success */
            return (TRUE);
        }

    }

	/* Must have complete information */
	for (i = 0; i < BORG_MAX_SHOP; i++)
	{
		auto_shop *shop = &borg_shops[i];

		/* Skip "visited" shops */
		if (!shop->when)
		{
			borg_note("# Shops not visited, aborting shopping");
			return (FALSE);
		}
	}


	/* Assume no important shop */
	borg_goal_shop = borg_goal_ware = borg_goal_item = -1;


    /* Step 0 -- Try to *Identify* artifacts in the home */
    if (borg_think_home_grab_ident())
	{
		/* Message */
        borg_note(format("# Grabbing '%s' from the home (to *Identify*)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}

    /* XXX Stop if we need to *Identify* something */
    if (borg_need_star_ident() &&
        borg_slot(TV_SCROLL, SV_SCROLL_STAR_IDENTIFY) >= 0)
    {
        return (FALSE);
    }


    /* Step 1 -- Grab items from the home (for the shops) */
	if (borg_think_home_grab_aux())
	{
		/* Message */
        borg_note(format("# Grabbing '%s' from the home (to sell)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 2 -- Grab items from the home (to make room) */
    if (borg_think_home_clear_aux())
	{
		/* Message */
        borg_note(format("# Grabbing '%s' from the home (full)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 2 1/2 -- Go for best possible equipment */
    if (borg_best_stuff(FALSE))
    {
        /* Message */
        borg_note(format("# Grabbing '%s' from the home (besting)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 3 -- Sell items to the home */
	if (borg_think_home_sell_aux())
	{
		/* Message */
        borg_note(format("# Selling '%s' to the home (to keep)",
    	                 borg_items[borg_goal_item].desc));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_item = %i",
                         borg_goal_shop, borg_goal_item));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 4 -- Sell items to the shops */
	if (borg_think_shop_sell_aux())
	{
		/* Message */
        borg_note(format("# Selling '%s' at '%s' (for cash)",
		                 borg_items[borg_goal_item].desc,
		                 (f_name + f_info[0x08+borg_goal_shop].name)));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_item = %i",
                         borg_goal_shop, borg_goal_item));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 5 -- Buy items from the shops (for the player) */
	if (borg_think_shop_buy_aux())
	{
		/* Message */
        borg_note(format("# Buying '%s' at '%s' (for player)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
		                 (f_name + f_info[0x08+borg_goal_shop].name)));
		
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 6 -- Buy items from the home (for the player) */
	if (borg_think_home_buy_aux())
	{
		/* Message */
        borg_note(format("# Buying '%s' from the home (for player)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));
		
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}


    /* Step 7 -- Buy items from the shops (for the home) */
	if (borg_think_shop_grab_aux())
	{
		/* Message */
        borg_note(format("# Grabbing '%s' at '%s' (for home)",
		                 borg_shops[borg_goal_shop].ware[borg_goal_ware].desc,
		                 (f_name + f_info[0x08+borg_goal_shop].name)));
#ifdef EXTRA_SHOP_MESSAGES
        borg_note(format("# borg_goal_shop = %i, borg_goal_ware = %i",
                         borg_goal_shop, borg_goal_ware));
#endif

		/* Success */
		return (TRUE);
	}
#if 0 
	/* Step 7A -- Buy weapons from the home (as a backup item) */
    if (borg_think_home_buy_swap_weapon())
    {
        /* Message */
        borg_note(format("# Buying '%s' from the home as a backup",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));

        /* Success */
        return (TRUE);
    }
    /* Step 7B -- Buy armour from the home (as a backup item) */
    if (borg_think_home_buy_swap_armour())
    {
        /* Message */
        borg_note(format("# Buying '%s' from the home as a backup",
                         borg_shops[borg_goal_shop].ware[borg_goal_ware].desc));

        /* Success */
        return (TRUE);
    }
#endif

	/* Failure */
	return (FALSE);
}




/*
 * Sell items to the current shop, if desired
 */
static bool borg_think_shop_sell(void)
{
	/* Sell something if requested */
	if ((borg_goal_shop == borg_base_shop) && (borg_goal_item >= 0))
	{
		auto_item *item = &borg_items[borg_goal_item];

		/* Log */
		borg_note(format("# Selling %s", item->desc));

        /* Send count */
        borg_keypresses(format("0%i", borg_min_item_quantity(item)));

		/* Send action (drop) */
		borg_keypress('d');

		/* Send item index XXX XXX XXX */
		borg_keypress((char)I2A(borg_goal_item));

		/* Hack -- Send price acceptance keys */
		borg_keypress('\n');
		borg_keypress('\n');
		borg_keypress('\n');
		borg_keypress('\n');

		/* The purchase is complete */
		borg_goal_shop = borg_goal_item = -1;

		/* Success */
		return (TRUE);
	}

	/* Nope */
	return (FALSE);
}


/*
 * Buy items from the current shop, if desired
 */
static bool borg_think_shop_buy(void)
{
	/* Increment our clock to avoid loops */
    time_this_panel ++;

	/* Buy something if requested */
	if ((borg_goal_shop == borg_base_shop) && (borg_goal_ware >= 0))
	{
		auto_shop *shop = &borg_shops[borg_goal_shop];

		auto_item *item = &shop->ware[borg_goal_ware];

		/* Go to the correct page */
		if ((borg_goal_ware / 12) != shop->page)
		{
			/* Send next page key */
			borg_keypress(' ');
		}

		/* Log */
		borg_note(format("# Buying %s.", item->desc));

        /* Send count */
        borg_keypresses(format("0%d", borg_min_item_quantity(item)));

		/* Send action (get) */
		borg_keypress('g');

		/* Send item index XXX XXX XXX */
		borg_keypress((char)I2A(borg_goal_ware));

		/* Hack -- Send price acceptance keys */
		borg_keypress('\n');
		borg_keypress('\n');
		borg_keypress('\n');
		borg_keypress('\n');

		/* The purchase is complete */
		borg_goal_shop = borg_goal_ware = -1;

		/* Success */
		return (TRUE);
	}

	/* Nothing to buy */
	return (FALSE);
}


/*
 * Deal with being in a store
 */
bool borg_think_store(void)
{
	/* Remove "backwards" rings */
	if (borg_swap_rings()) return (TRUE);

	/* Repair "backwards" rings */
	if (borg_wear_rings()) return (TRUE);

	/* Wear "optimal" equipment */
    if (borg_best_stuff(TRUE)) return (TRUE);

    /* Wear "useful" equipment */
    if (borg_wear_stuff()) return (TRUE);

	/* Remove "useless" equipment */
//	if (borg_takeoff_stuff()) return (TRUE);

	/* Choose a shop to visit */
	if (borg_choose_shop())
	{
		/* Try to sell stuff */
		if (borg_think_shop_sell()) return (TRUE);

		/* Try to buy stuff */
		if (borg_think_shop_buy()) return (TRUE);
	}

	/* Stamp the shop with a time stamp */
	borg_shops[borg_base_shop].when = borg_time;

	/* No shop */
	borg_base_shop = -1;

	/* Send leave store key */
	borg_keypress(ESCAPE);

	/* Done */
	return (TRUE);
}



/*
 * Hack -- perform an action in the dungeon under boosted bravery
 *
 * This function is a sub-set of the standard dungeon goals, and is
 * only executed when all of the standard dungeon goals fail, because
 * of excessive danger, or because the level is "bizarre".
 */
static bool borg_think_dungeon_brave(void)
{
	/*** Try teleport ***/

	/* With scroll */
	if (borg_read_scroll(SV_SCROLL_TELEPORT)) return (TRUE);
	if (borg_read_scroll(SV_SCROLL_PHASE_DOOR)) return (TRUE);

	/* With staff */
	if (borg_use_staff(SV_STAFF_TELEPORTATION)) return (TRUE);

	/* With spell */
	if (borg_prayer(1, 1) || borg_spell(1, 5)) return (TRUE);
	if (borg_prayer(4, 0) || borg_spell(0, 2)) return (TRUE);

	/*** Local stuff ***/

	/* Attack monsters */
	if (borg_attack()) return (TRUE);

	/* Continue flowing towards monsters */
	if (borg_flow_old(GOAL_KILL)) return (TRUE);

	/* Find a (viewable) monster */
	if (borg_flow_kill(TRUE)) return (TRUE);

	/* Continue flowing towards objects */
	if (borg_flow_old(GOAL_TAKE)) return (TRUE);

	/* Find a (viewable) object */
	if (borg_flow_take(TRUE)) return (TRUE);

    /* Find an (onscreen) object */
    if (borg_flow_take_near(FALSE)) return (TRUE);


	/*** Flee (or leave) the level ***/

	/* Flee the level */
	if (borg_fleeing || borg_leaving)
	{
		/* Hack -- Take the next stairs */
		borg_stair_less = borg_stair_more = TRUE;

		/* Continue fleeing the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);
	}


	/*** Exploration ***/

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_MISC)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_DARK)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_XTRA)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_BORE)) return (TRUE);


	/*** Explore the dungeon ***/

	/* Explore interesting grids */
	if (borg_flow_dark(TRUE)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(FALSE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(FALSE)) return (TRUE);


	/*** Track down old stuff ***/

	/* Chase old objects */
	if (borg_flow_take(FALSE)) return (TRUE);

	/* Chase old monsters */
	if (borg_flow_kill(FALSE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(TRUE)) return (TRUE);


	/* Nothing */
	return (FALSE);
}


/*
 * Perform an action in the dungeon
 *
 * Return TRUE if a "meaningful" action was performed
 * Otherwise, return FALSE so we will be called again
 *
 * Strategy:
 *   Make sure we are happy with our "status" (see above)
 *   Attack and kill visible monsters, if near enough
 *   Open doors, disarm traps, tunnel through rubble
 *   Pick up (or tunnel to) gold and useful objects
 *   Explore "interesting" grids, to expand the map
 *   Explore the dungeon and revisit old grids
 *
 * Fleeing:
 *   Use word of recall when level is "scary"
 *   Flee to stairs when there is a chance of death
 *   Avoid "stair bouncing" if at all possible
 *
 * Note that the various "flow" actions allow the Borg to flow
 * "through" closed doors, which will be opened when he attempts
 * to pass through them, so we do not have to pursue terrain until
 * all monsters and objects have been dealt with.
 *
 * The poor Borg often kills a nasty monster, and then takes a nap to recover
 * from damage, but gets yanked back to town before he can collect his reward.
 * XXX XXX XXX
 *
 * The "save game" command would be much more useful if the Borg's internal
 * state was saved along with the savefile.  We save the random number seed
 * just for convenience, but it may not be meaningful without the rest of
 * the internal state.  XXX XXX XXX
 */
bool borg_think_dungeon(void)
{
	int py = b_ptr->py;
	int px = b_ptr->px;

	int i, j, k;


	/* Save game */
	if (borg_need_save)
	{
		/* Clear flag */
		borg_need_save = FALSE;

		/* Save the game */
		(void)borg_save_game();

		/* Reset the seed XXX XXX XXX */
		Rand_value = rand_int(0x10000000);

		/* Done */
		return (TRUE);
	}


	/* Hack -- prevent clock wrapping */
	if (borg_time - borg_when_began >= 30000)
	{
		/* Panic */
		borg_oops("clock overflow");

		/* Oops */
		return (TRUE);
	}


	/* Prevent clock overflow */
	if (borg_time - borg_when_began >= 10000)
	{
		/* Start leaving */
		if (!borg_leaving)
		{
			/* Note */
			borg_note("# Leaving (boredom)");

			/* Start leaving */
			borg_leaving = TRUE;
		}

		/* Start fleeing */
		if (!borg_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (boredom)");

			/* Start fleeing */
			borg_fleeing = TRUE;
		}
	}

    /* Prevent a "bouncing Borg" bug. Where borg with telepathy
     * will sit in a narrow area bouncing between 2 or 3 places
     * tracking and flowing to a bouncing monster behind a wall.
     */
    if (b_ptr->depth && (time_this_panel >= 500))
    {
		borg_note(format("# Bouncing!! Time on panel = %d",time_this_panel));

        /* Start leaving */
        if (!borg_leaving)
        {
            /* Note */
            borg_note("# Leaving (bouncing-borg)");

            /* Start leaving */
            borg_leaving = TRUE;
        }

        /* Start fleeing */
        if (!borg_fleeing)
        {
            /* Note */
            borg_note("# Fleeing (bouncing-borg)");

            /* Start fleeing */
            borg_fleeing = TRUE;
        }

    }

	/* Avoid annoying farming */
	if (borg_time - borg_when_began >= 5000)
	{
		/* Ignore monsters from boredom */
		if (!borg_ignoring)
		{
			/* Flee */
			borg_note("# Ignoring breeders (boredom)");

			/* Ignore multipliers */
			borg_ignoring = TRUE;
		}
	}

	/* Count the awake breeders */
	for (j = 0, i = 1; i < borg_kills_nxt; i++)
	{
		auto_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Skip sleeping monsters */
		if (!kill->awake) continue;

		/* Count the monsters which are "breeders" */
		if (r_info[kill->r_idx].flags2 & RF2_MULTIPLY) j++;
	}
	
	if (j >=4 && b_ptr->lev <=30)
	{
		/* set the flag to close doors */
		borg_close_doors = TRUE;
	}

	/* Fear multipliers when lacking recall */
	if ((j >= 3) && (amt_recall <= 0))
	{
		/* Ignore monsters from caution */
		if (!borg_ignoring)
		{
			/* Flee */
			borg_note("# Ignoring breeders (no recall)");

			/* Ignore multipliers */
			borg_ignoring = TRUE;
		}

		/* Start leaving */
		if (!borg_leaving)
		{
			/* Note */
			borg_note("# Leaving (no recall)");

			/* Start leaving */
			borg_leaving = TRUE;
		}

		/* Start fleeing */
		if (!borg_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (no recall)");

			/* Start fleeing */
			borg_fleeing = TRUE;
		}
	}


	/* Maximal avoidance */
	k = MAX(b_ptr->chp, borg_boost);

	/* Hack -- Mages are not that brave early on DvE */
	if (b_ptr->pclass == CLASS_MAGE) k /= 2;

	/* Uniques deserve extra attention */
	if ((borg_unique_near || vault_on_level) && (k == b_ptr->chp)) k = k * 2;

	/* Update avoidance */
	if (borg_avoid != k)
	{
		/* Reset avoidance */
		borg_avoid = k;

		/* Forget task */
		borg_task = 0;
	}


	/*** Crucial goals ***/
	/* require light-- */
    if (b_ptr->cur_lite <= 0 && b_ptr->depth >= 1)
    {
        if (borg_recalling)
        {
            /* just wait */
            borg_keypress('R');
            borg_keypress('9');
            borg_keypress('\n');
            return (TRUE);
        }

        /* attempt to refuel */
        if (borg_refuel_torch() || borg_refuel_lantern()) return (TRUE);

        /* wear stuff and see if it glows */
        if (borg_wear_stuff()) return (TRUE);

        /* Can I recall out with a rod */
        if (!borg_recalling && borg_zap_rod(SV_ROD_RECALL)) return (TRUE);

        /* Test for stairs */
        borg_keypress('<');

        /* Try to flow to upstairs if on level one */
        if (borg_flow_stair_less(GOAL_FLEE))
        {
            /* Take the stairs */
            borg_keypress('<');
            return (TRUE);
        }

        /* Try to flow to a lite */
        if (borg_flow_light(GOAL_FLEE))
        {
            /* Can I recall out with a spell */
            if (!borg_recalling && borg_recall()) return (TRUE);
            return (TRUE);
        }

        /* panic */
        borg_oops("out of light");

        /* oops */
        return (TRUE);
    }

	/*** Important goals ***/

    /* Try not to die (and wear some items) */
	if (borg_caution()) return (TRUE);

	/* Study useful magic immediately */
	if (borg_study_magic(FALSE)) return (TRUE);

	/* Attack monsters */
	if (borg_attack()) return (TRUE);

	/* Recover from damage */
	if (borg_recover()) return (TRUE);

	/* Wear things that need to be worn */
	if (borg_wear_stuff()) return (TRUE);

	/* Continue flowing towards monsters */
	if (borg_flow_old(GOAL_KILL)) return (TRUE);

	/* Find a (viewable) monster */
	if (borg_flow_kill(TRUE)) return (TRUE);


	/*** Deal with inventory objects ***/

	/* Check the light */
	if (borg_check_lite()) return (TRUE);

	/* Use things */
	if (borg_use_things()) return (TRUE);

	/* Use "identify" */
	if (borg_test_stuff()) return (TRUE);

	/* Use "*identify*" */
	if (borg_star_stuff()) return (TRUE);

	/* Use "examine" */
	if (borg_exam_stuff()) return (TRUE);

	/* Recharge things */
	if (borg_recharging()) return (TRUE);

	/* Destroy junk */
	if (borg_crush_junk()) return (TRUE);

	/* Destroy items to make space */
	if (borg_crush_hole()) return (TRUE);

	/* Destroy items if we are slow */
	if (borg_crush_slow()) return (TRUE);


	/*** Flee the level XXX XXX XXX ***/

	/* Flee the level */
	if (borg_fleeing)
	{
		/* Hack -- Take the next stairs */
		borg_stair_less = borg_stair_more = TRUE;

		/* Continue fleeing the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);
	}


	/*** Flow towards objects ***/

	/* Continue flowing towards objects */
	if (borg_flow_old(GOAL_TAKE)) return (TRUE);

	/* Find a (viewable) object */
	if (borg_flow_take(TRUE)) return (TRUE);

    /* Find an (onscreen) object */
    if (borg_flow_take_near(FALSE)) return (TRUE);


	/*** Leave the level XXX XXX XXX ***/

	/* Leave the level */
	if (borg_leaving)
	{
		/* Hack -- Take the next stairs */
		borg_stair_less = borg_stair_more = TRUE;

		/* Continue leaving the level */
		if (borg_flow_old(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs up */
		if (borg_flow_stair_less(GOAL_FLEE)) return (TRUE);

		/* Try to find some stairs down */
		if (borg_flow_stair_more(GOAL_FLEE)) return (TRUE);
	}


	/*** Exploration ***/

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_MISC)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_DARK)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_XTRA)) return (TRUE);

	/* Continue flowing (see below) */
	if (borg_flow_old(GOAL_BORE)) return (TRUE);


	/*** Explore the dungeon ***/

	/* Chase old monsters */
	if (borg_flow_kill(FALSE)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(TRUE)) return (TRUE);

	/* Chase old objects */
	if (borg_flow_take(FALSE)) return (TRUE);

	/* Leave the level (if needed) */
	if (borg_leave_level(FALSE)) return (TRUE);

	/* Explore interesting grids */
	if (borg_flow_dark(FALSE)) return (TRUE);


	/*** Deal with shops ***/

	/* Hack -- visit all the shops */
	if (borg_flow_shop_visit()) return (TRUE);

	/* Hack -- Visit the shops */
	if (borg_choose_shop())
	{
		/* Try and visit a shop, if so desired */
		if (borg_flow_shop_entry(borg_goal_shop)) return (TRUE);
	}

	/* Enchant things */
	if (borg_enchanting()) return (TRUE);


	/*** Leave the Level ***/

	/* Study boring spells/prayers */
	if (borg_study_magic(TRUE)) return (TRUE);

	/* Use spells/prayers for experience */
	if (borg_play_magic()) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(FALSE)) return (TRUE);

	/* Leave the level (if possible) */
	if (borg_leave_level(TRUE)) return (TRUE);

	/* Search for secret doors */
	if (borg_flow_spastic(TRUE)) return (TRUE);


	/*** Wait for recall ***/

	/* Wait for recall, unless in danger */
	if (borg_recalling && (borg_danger(py, px, 1) <= 0))
	{
		/* Take note */
		borg_note("# Waiting for Recall...");

		/* Send action (rest until done) */
		borg_keypress('R');
		borg_keypress('&');
		borg_keypress('\n');

		/* Done */
		return (TRUE);
	}


	/*** Nothing to do ***/

	/* Start leaving */
	if (!borg_leaving)
	{
		/* Note */
		borg_note("# Leaving (twitchy)");

		/* Start leaving */
		borg_leaving = TRUE;
	}

	/* Start fleeing */
	if (!borg_fleeing)
	{
		/* Note */
		borg_note("# Fleeing (twitchy)");

		/* Start fleeing */
		borg_fleeing = TRUE;
	}

	/* Recall to town */
	/* Let's not do this DvE */
	/*	if ((b_ptr->depth > 1) && (borg_recall())) */
	/* { */
		/* Note */
	/*	borg_note("# Recalling (twitchy)"); */

		/* Success */
	/*	return (TRUE); */
	/* } */
	

	/* Boost avoidance */
	if (borg_boost < b_ptr->chp)
	{
		/* Note */
		borg_note(format("# Boosting bravery (1) from %d to %d!",
		                 borg_boost, b_ptr->chp));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = (b_ptr->chp);

		/* Try something brave */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
		
	}

	/* Boost avoidance */
	if (borg_boost < b_ptr->mhp)
	{
		/* Note */
		borg_note(format("# Boosting bravery (2) from %d to %d!",
		                 borg_boost, b_ptr->mhp));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = (b_ptr->mhp);

		/* Try anything */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
	}

	/* Boost avoidance */
	if (borg_boost < b_ptr->mhp * 2)
	{
		/* Note */
		borg_note(format("# Boosting bravery (3) from %d to %d!",
		                 borg_boost, b_ptr->mhp * 2));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = (b_ptr->mhp * 2);

		/* Try anything */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
	}

	/* Boost avoidance */
	if (borg_boost < b_ptr->mhp * 4)
	{
		/* Note */
		borg_note(format("# Boosting bravery (4) from %d to %d!",
		                 borg_boost, b_ptr->mhp * 4));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = (b_ptr->mhp * 4);

		/* Try anything */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
	}

	/* Boost avoidance */
	if (borg_boost < b_ptr->mhp * 8)
	{
		/* Note */
		borg_note(format("# Boosting bravery (5) from %d to %d!",
		                 borg_boost, b_ptr->mhp * 8));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = (b_ptr->mhp * 8);

		/* Try anything */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
	}

	/* Boost avoidance */
	if (borg_boost < 30000)
	{
		/* Note */
		borg_note(format("# Boosting bravery (6) from %d to %d!",
		                 borg_boost, 30000));

		/* Hack -- ignore some danger */
		borg_avoid = borg_boost = 30000;

		/* Try anything */
		if (borg_think_dungeon_brave())
		{
			/* Reset bravery */
			borg_boost = 0;
			borg_avoid = b_ptr->chp;

			/* Success */
			return (TRUE);
		}
	}

	/* Twitch around */
	if (borg_twitchy()) return (TRUE);

	/* Oops */
	return (FALSE);
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

