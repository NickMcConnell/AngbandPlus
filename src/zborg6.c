/* File: zborg6.c */
/* Purpose: Medium level stuff for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"
#include "zborg5.h"
#include "zborg6.h"
#include "zbmagic.h"


/* Helper function to cure poison when not full/gorged */
bool borg_eat_cure_poison(void)
{
	/* Only when poisoned */
	if (!bp_ptr->status.poisoned) return (FALSE);

	/* Only allow when not too full or really low hp */
	if ((bp_ptr->status.full ||
		bp_ptr->status.gorged) &&
		bp_ptr->chp > 3) return (FALSE);

	return (borg_eat_food(SV_FOOD_CURE_POISON) ||
			borg_eat_food(SV_FOOD_WAYBREAD));
}

/*
 * Attempt to recover from damage and such after a battle
 *
 * Note that resting while in danger is counter-productive, unless
 * the danger is low, in which case it may induce "farming".
 *
 * Note that resting while recall is active will often cause you
 * to lose hard-won treasure from nasty monsters, so we disable
 * resting when waiting for recall in the dungeon near objects.
 *
 * First we try spells/prayers, which are "free", and then we
 * try food, potions, scrolls, staffs, rods, artifacts, etc.
 *
 * XXX XXX XXX
 * Currently, we use healing spells as if they were "free", but really,
 * this is only true if the "danger" is less than the "reward" of doing
 * the healing spell, and if there are no monsters which might soon step
 * around the corner and attack.
 */
bool borg_recover(void)
{
	int p = 0;
	int q;

	map_block *mb_ptr = map_loc(c_x, c_y);
	list_item *l_ptr = look_up_equip_slot(EQUIP_LITE);

	/*** Handle annoying situations ***/

	/* If the borg has a torch or lantern */
	if (l_ptr &&
		l_ptr->tval == TV_LITE &&
		(k_info[l_ptr->k_idx].sval == SV_LITE_LANTERN ||
		k_info[l_ptr->k_idx].sval == SV_LITE_TORCH))
	{
		/* Try to refuel torch or lantern */
		if (borg_refuel()) return (TRUE);

		/* If it is not everburning and low on fuel */
		if (!bp_ptr->britelite &&
			l_ptr->timeout < 1000)
		{
			/* Take note */
			borg_note("# Need to refuel but can't!", p);

			/* Go to town */
			goal_rising = TRUE;

			/* Allow Pets to Roam so we dont hit them in the dark. */
			p_ptr->pet_follow_distance = PET_STAY_AWAY;
		}
	}
	
	/*** Do not recover when in danger ***/

	/* Look around for danger */
	p = borg_danger(c_x, c_y, 1, TRUE);

	/* Never recover in dangerous situations */
	if (p > avoidance / 4) return (FALSE);


	/*** Roll for "paranoia" ***/

	/* Base roll */
	q = randint0(100);

	/* Half dead */
	if (bp_ptr->chp < bp_ptr->mhp / 2) q = q - 10;

	/* Almost dead */
	if (bp_ptr->chp < bp_ptr->mhp / 4) q = q - 10;

	/* Minimize q a bit more */
	q = MIN(q, bp_ptr->chp);

	/*** Use "cheap" cures ***/

	/* Hack -- cure stun */
	if (bp_ptr->status.stun && (q < 75))
	{
		if (borg_activate(BORG_ACT_HEAL_SERIOUS) ||
			borg_spell(REALM_LIFE, 0, 1) ||
			borg_spell(REALM_LIFE, 0, 6) ||
			borg_spell(REALM_ARCANE, 0, 7))

		{
			/* Take note */
			borg_note("# Cure Stun", p);

			return (TRUE);
		}
	}

	/* Hack -- cure stun */
	if (bp_ptr->status.heavy_stun)
	{
		if (borg_activate(BORG_ACT_HEAL_SERIOUS) ||
			borg_spell(REALM_LIFE, 1, 2) ||
			borg_racial(RACE_AMBERITE_POWER2))
		{
			/* Take note */
			borg_note("# Cure Heavy Stun", p);

			return (TRUE);
		}
	}

	/* Hack -- cure cuts */
	if (bp_ptr->status.cut && (q < 75))
	{
		if (borg_activate(BORG_ACT_HEAL_SERIOUS) ||
			borg_spell(REALM_LIFE, 1, 2) ||
			borg_spell(REALM_NATURE, 0, 7) ||
			borg_spell(REALM_LIFE, 0, 6) ||
			borg_racial(RACE_AMBERITE_POWER2))
		{
			/* Take note */
			borg_note("# Cure Cuts", p);

			return (TRUE);
		}
	}

	/* Hack -- cure poison */
	if (bp_ptr->status.poisoned && (q < 75))
	{
		if (borg_activate(BORG_ACT_CURE_POISON) ||
			borg_spell(REALM_ARCANE, 1, 7) ||
			borg_spell(REALM_NATURE, 0, 7) ||
			borg_spell(REALM_LIFE, 1, 2) ||
			borg_racial(RACE_AMBERITE_POWER2))
		 {
			/* Take note */
			borg_note("# Cure poison", p);

			return (TRUE);
		}
	}

	/* Hack -- cure fear */
	if (bp_ptr->status.afraid && (q < 75))
	{
		if (borg_activate(BORG_ACT_REMOVE_FEAR) ||
			borg_spell(REALM_LIFE, 0, 3))
		{
			/* Take note */
			borg_note("# Cure fear", p);

			return (TRUE);
		}
	}

	/* Hack -- satisfy hunger */
	if ((bp_ptr->status.hungry || bp_ptr->status.weak) && (q < 75))
	{
		if (borg_spell_fail(REALM_LIFE, 0, 7, 65) ||
			borg_spell_fail(REALM_ARCANE, 2, 6, 65) ||
			borg_spell_fail(REALM_NATURE, 0, 3, 65) ||
			borg_racial(RACE_HOBBIT) ||
			borg_read_scroll(SV_SCROLL_SATISFY_HUNGER))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal damage */
	if ((bp_ptr->chp < bp_ptr->mhp / 2) && (q < 75) && p == 0
		&& (bp_ptr->csp > bp_ptr->msp / 4))
	{
		if (borg_activate(BORG_ACT_HEAL_BIG) ||
			borg_spell(REALM_LIFE, 1, 6) ||
			borg_spell(REALM_NATURE, 1, 7))
		{
			/* Take note */
			borg_note("# heal damage (recovering)");

			return (TRUE);
		}
	}

	/* cure experience loss with prayer */
	if (bp_ptr->status.fixexp &&
		(borg_activate(BORG_ACT_RESTORE_LIFE) ||
		 borg_spell(REALM_LIFE, 3, 3) ||
		 borg_spell(REALM_DEATH, 1, 7) ||
		 borg_racial(RACE_AMBERITE_POWER2) ||
		 borg_racial(RACE_SKELETON) ||
		 borg_racial(RACE_ZOMBIE)))
	{
		return (TRUE);
	}

	/* cure stat drain with prayer */
	if ((bp_ptr->status.fixstat[A_STR] ||
		 bp_ptr->status.fixstat[A_INT] ||
		 bp_ptr->status.fixstat[A_WIS] ||
		 bp_ptr->status.fixstat[A_DEX] ||
		 bp_ptr->status.fixstat[A_CON] ||
		 bp_ptr->status.fixstat[A_CHR]) &&
		(borg_spell(REALM_LIFE, 3, 3) ||
		 borg_zap_rod(SV_ROD_RESTORATION) ||
		 borg_activate(BORG_ACT_RESTORATION) ||
		 borg_racial(RACE_AMBERITE_POWER2) ||
		 borg_eat_food(SV_FOOD_RESTORING)))
	{
		return (TRUE);
	}

	/*** Use "expensive" cures ***/

	/* Hack -- cure stun */
	if (bp_ptr->status.stun && (q < 25))
	{
		if (borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING) ||
			borg_zap_rod(SV_ROD_HEALING) ||
			borg_activate(BORG_ACT_HEAL_BIG) ||
			borg_quaff_crit(FALSE))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure heavy stun */
	if (bp_ptr->status.heavy_stun && (q < 95))
	{
		if (borg_quaff_crit(TRUE) ||
			borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING) ||
			borg_zap_rod(SV_ROD_HEALING) ||
			borg_activate(BORG_ACT_HEAL_BIG))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure cuts */
	if (bp_ptr->status.cut && (q < 25))
	{
		if (borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING) ||
			borg_zap_rod(SV_ROD_HEALING) ||
			borg_activate(BORG_ACT_HEAL_BIG) ||
			borg_quaff_crit((bool) (bp_ptr->chp < 10)))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure poison */
	if (bp_ptr->status.poisoned && (q < 25))
	{
		if (borg_quaff_potion(SV_POTION_CURE_POISON) ||
			borg_quaff_potion(SV_POTION_SLOW_POISON) ||
			borg_eat_cure_poison() ||
			borg_quaff_crit((bool) (bp_ptr->chp < 10)) ||
			borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING) ||
			borg_activate(BORG_ACT_CURE_POISON) ||
			borg_racial(RACE_AMBERITE_POWER2))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure blindness */
	if (bp_ptr->status.blind && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_BLINDNESS) ||
			borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			borg_quaff_crit(FALSE) ||
			borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING) ||
			borg_racial(RACE_AMBERITE_POWER2))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure confusion */
	if (bp_ptr->status.confused && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			borg_quaff_crit(FALSE) ||
			borg_use_staff_fail(SV_STAFF_CURING) ||
			borg_zap_rod(SV_ROD_CURING))
		{
			return (TRUE);
		}
	}

	/* Hack -- cure fear */
	if (bp_ptr->status.afraid && (q < 25))
	{
		if (borg_eat_food(SV_FOOD_CURE_PARANOIA) ||
			borg_quaff_potion(SV_POTION_BOLDNESS) ||
			borg_quaff_potion(SV_POTION_HEROISM) ||
			borg_quaff_potion(SV_POTION_BERSERK_STRENGTH) ||
			borg_activate(BORG_ACT_REMOVE_FEAR) ||
			borg_activate(BORG_ACT_HEROISM) ||
			borg_activate(BORG_ACT_BERSERKER) ||
			borg_mutation(MUT1_BERSERK) ||
			borg_racial(RACE_HALF_ORC) ||
			borg_racial(RACE_HALF_TROLL) ||
			borg_racial(RACE_AMBERITE_POWER2))
		{
			return (TRUE);
		}
	}

	/* Hack -- satisfy hunger */
	if ((bp_ptr->status.hungry || bp_ptr->status.weak) && (q < 25))
	{
		if (borg_read_scroll(SV_SCROLL_SATISFY_HUNGER))
		{
			return (TRUE);
		}
	}

	/* Hack -- heal damage */
	if ((bp_ptr->chp < bp_ptr->mhp / 2) && (q < 25))
	{
		if (borg_zap_rod(SV_ROD_HEALING) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			borg_quaff_crit(FALSE) ||
			borg_activate(BORG_ACT_HEAL_SERIOUS))
		{
			return (TRUE);
		}
	}

	/* If Fleeing, then do not rest */
	if (goal_fleeing) return (FALSE);

	/* Step 1.  Recharge just 1 rod. */
	if ((borg_slot(TV_ROD, SV_ROD_HEALING) &&
		 borg_slot(TV_ROD, SV_ROD_HEALING)->timeout) ||
		(borg_slot(TV_ROD, SV_ROD_RECALL) &&
		 borg_slot(TV_ROD, SV_ROD_RECALL)->timeout))
	{
		/* Rest until at least one recharges */
		if (!bp_ptr->status.weak && !bp_ptr->status.cut &&
			!bp_ptr->status.hungry && !bp_ptr->status.poisoned &&
			borg_check_rest() && borg_on_safe_feat(map_loc(c_x, c_y)->feat))
		{
			/* Take note */
			borg_note("# Resting to recharge a rod...");

			/* Rest until done */
			borg_keypress('R');
			borg_keypress('1');
			borg_keypress('0');
			borg_keypress('0');
			borg_keypress('\n');

			/* Done */
			return (TRUE);
		}
	}

	/*** Just Rest ***/

	/* Hack -- rest until healed */
	if ((bp_ptr->status.blind ||
		 bp_ptr->status.confused ||
		 bp_ptr->status.image ||
		 bp_ptr->status.afraid ||
		 bp_ptr->status.stun ||
		 bp_ptr->status.heavy_stun ||
		 bp_ptr->chp < bp_ptr->mhp ||
		 bp_ptr->csp < bp_ptr->msp * 6 / 10) &&
		(!borg_takes_cnt || !goal_recalling) &&
		!borg_goi &&
		!borg_shield &&
		!scaryguy_on_level &&
		borg_check_rest() &&
		p <= mb_ptr->fear &&
		!goal_fleeing &&
		borg_on_safe_feat(map_loc(c_x, c_y)->feat))
	{
		/* XXX XXX XXX */
		if (!bp_ptr->status.weak && !bp_ptr->status.cut &&
			!bp_ptr->status.hungry && !bp_ptr->status.poisoned)
		{
			/* Take note */
			borg_note("# Resting (danger %d)...", p);

			/* Rest until done */
			borg_keypress('R');
			borg_keypress('\n');

			/* Done */
			return (TRUE);
		}
	}

	/* Hack to recharge mana if a low level mage or priest */
	if (bp_ptr->msp && bp_ptr->lev < 25 && bp_ptr->csp < bp_ptr->msp && p == 0)
	{
		if (!bp_ptr->status.weak && !bp_ptr->status.cut &&
			!bp_ptr->status.hungry && !bp_ptr->status.poisoned &&
			borg_on_safe_feat(map_loc(c_x, c_y)->feat))
		{
			/* Take note */
			borg_note("# Resting to gain Mana. (danger %d)...", p);

			/* Rest until done */
			borg_keypress('R');
			borg_keypress('\n');

			/* Done */
			return (TRUE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Given a "source" and "target" locations, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * Note that we use "diagonal" motion whenever possible.
 *
 * We return "5" if no motion is needed.
 */
int borg_extract_dir(int x1, int y1, int x2, int y2)
{
	/* No movement required */
	if ((y1 == y2) && (x1 == x2)) return (5);

	/* South or North */
	if (x1 == x2) return ((y1 < y2) ? 2 : 8);

	/* East or West */
	if (y1 == y2) return ((x1 < x2) ? 6 : 4);

	/* South-east or South-west */
	if (y1 < y2) return ((x1 < x2) ? 3 : 1);

	/* North-east or North-west */
	if (y1 > y2) return ((x1 < x2) ? 9 : 7);

	/* Paranoia */
	return (5);
}


/*
 * Clear the "flow" information
 *
 * This function was once a major bottleneck, so we now use several
 * slightly bizarre, but highly optimized, memory copying methods.
 */
static void borg_flow_clear(void)
{
	map_block *mb_ptr;

	/* Iterate over the map */
	MAP_ITT_START (mb_ptr)
	{
		mb_ptr->cost = 255;

		if (borg_danger_wipe)
		{
			/* Clear the "icky" + "know" flags */
			mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);
		}
	}
	MAP_ITT_END;

	/* Wipe complete */
	borg_danger_wipe = FALSE;

	/* Start over */
	flow_head = 0;
	flow_tail = 0;
}


/*
 * Take one "step" towards the given location, return TRUE if possible
 */
static bool borg_play_step(int y2, int x2)
{
	map_block *mb_ptr = NULL;

	int dir, x, y, ox, oy, i;

	int o_y = 0, o_x = 0, door_found = 0;

	/* Breeder levels, close all doors */
	if (breeder_level)
	{
		/* Scan the adjacent grids */
		for (ox = -1; ox <= 1; ox++)
		{
			for (oy = -1; oy <= 1; oy++)
			{
				/* Skip our own spot */
				if ((oy + c_y == c_y) && (ox + c_x == c_x)) continue;

				/* Skip our orignal goal */
				if ((oy + c_y == y2) && (ox + c_x == x2)) continue;

				/* Bounds checking */
				if (!map_in_bounds(ox + c_x, oy + c_y)) continue;

				/* Acquire location */
				mb_ptr = map_loc(ox + c_x, oy + c_y);

				/* Skip non open doors */
				if (mb_ptr->feat != FEAT_OPEN) continue;

				/* Skip monster on door */
				if (mb_ptr->monster) continue;

				/* Skip repeatedly closed doors */
				if (track_door_num >= 255) continue;

				/* Save this spot */
				o_y = oy;
				o_x = ox;
				door_found = 1;
			}
		}

		/* Is there a door to close? */
		if (door_found)
		{
			/* Get a direction, if possible */
			dir = borg_goto_dir(c_x, c_y, c_x + o_x, c_y + o_y);

			/* Obtain the destination */
			x = c_x + ddx[dir];
			y = c_y + ddy[dir];

			/* Hack -- set goal */
			g_x = x;
			g_y = y;

			/* If the borg is in the dark and just failed to close the door */
			if (!bp_ptr->cur_lite &&
				borg_close_door_failed)
			{
				/* Hack the door closed */
				mb_ptr->feat = FEAT_CLOSED;

				borg_close_door_failed = FALSE;
			}
			else
			{
				/* Close */
				borg_note("# Closing a door");
				borg_keypress('c');
				borg_keypress(I2D(dir));

				/* Check for an existing flag */
				for (i = 0; i < track_door_num; i++)
				{
					/* Stop if we already knew about this door */
					if ((track_door_x[i] == x) &&
						(track_door_y[i] == y)) return (TRUE);
				}

				/* Track the newly closed door */
				if (i == track_door_num && i < track_door_size)
				{

					borg_note("# Noting the closing of a door.");
					track_door_num++;
					track_door_x[i] = x;
					track_door_y[i] = y;
				}
				return (TRUE);
			}
		}
	}

	/* Get a direction, if possible */
	dir = borg_goto_dir(c_x, c_y, x2, y2);

	/* We have arrived */
	if (dir == 5) return (FALSE);

	/* Obtain the destination */
	x = c_x + ddx[dir];
	y = c_y + ddy[dir];

	/* Access the grid we are stepping on (Assume this is in bounds) */
	mb_ptr = map_loc(x, y);

	/* Hack -- set goal */
	g_x = x;
	g_y = y;

	/* Monsters -- Attack */
	if (mb_ptr->kill)
	{
		/* Can't attack someone if afraid! */
		if (bp_ptr->status.afraid)
			return (FALSE);

		/* Hack -- ignore Maggot until later.  */
		if ((FLAG(&r_info[mb_ptr->monster], RF_UNIQUE)) &&
			bp_ptr->depth == 0 && bp_ptr->lev < 5)
			return (FALSE);

		/* Quick hack to prevent the walking into player bug */
		if (!mb_ptr->monster) return (FALSE);

		/* Message */
		borg_note("# Walking into a '%s' at (%d,%d)",
					  r_name + r_info[mb_ptr->monster].name, x, y);

		/* Walk into it */
		if (my_no_alter)
		{
			borg_keypress(';');
			my_no_alter = FALSE;
		}
		else
		{
			borg_keypress('+');
		}
		borg_keypress(I2D(dir));
		return (TRUE);
	}


	/* Objects -- Take */
	if (mb_ptr->object)
	{
		/*** Handle other takes ***/
		/* Message */
		borg_note("# Walking onto a '%s' at (%d,%d)",
					  k_name + k_info[mb_ptr->object].name, x, y);

		/* Walk onto it */
		borg_keypress(I2D(dir));
		return (TRUE);
	}


	/* Traps -- disarm -- */
	if (bp_ptr->cur_lite && !bp_ptr->status.blind &&
		!bp_ptr->status.confused && !scaryguy_on_level &&
		mb_ptr->trap)
	{

		/*
		 * NOTE: If a scary guy is on the level,
		 * we allow the borg to run over the
		 * trap in order to escape this level.
		 */

		/* Cast a disarm with direction */
		if (borg_spell(REALM_ARCANE, 0, 6) ||
			borg_zap_rod(SV_ROD_DISARMING) ||
			borg_aim_wand(SV_WAND_DISARMING) ||
			borg_aim_wand(SV_WAND_TRAP_DOOR_DEST))
		{
			/* Make sure there is no target */
			borg_keypress('*');
			borg_keypress(ESCAPE);

			borg_note("# Unbarring ways");
			borg_keypress(I2D(dir));
			mb_ptr->trap = FT_NONE;
			return (TRUE);
		}
		/* allow "destroy doors" */
		if (borg_spell(REALM_CHAOS, 0, 1) ||
			borg_activate(BORG_ACT_DISARM))
		{
			borg_note("# Unbarring ways");
			mb_ptr->trap = FT_NONE;
			return (TRUE);
		}

		/* Disarm */
		borg_note("# Disarming a trap");
		borg_keypress('D');
		borg_keypress(I2D(dir));

		/* We are not sure if the trap will get 'untrapped'. pretend it will */
		mb_ptr->trap = FT_NONE;
		return (TRUE);
	}

	/* Closed Doors -- Open */
	if (mb_ptr->feat == FEAT_CLOSED)
	{
		/* Paranoia XXX XXX XXX */
		if (one_in_(100)) return (FALSE);

		/* If the borg is in the dark and just failed to open the door */
		if (!bp_ptr->cur_lite &&
			borg_open_door_failed)
		{
			/* Hack the door open */
			mb_ptr->feat = FEAT_OPEN;

			borg_open_door_failed = FALSE;
		}
		else
		{
			/* Open */
			if (my_need_alter)
			{
				borg_keypress('+');
				my_need_alter = FALSE;
			}
			else
			{
				borg_note("# Opening a door");
				borg_keypress('0');
				borg_keypress('9');
				borg_keypress('o');
			}
			borg_keypress(I2D(dir));

			return (TRUE);
		}
	}

	/* Rubble, Treasure, Seams, Walls -- Tunnel or Melt */
	if (mb_ptr->feat >= FEAT_PILLAR && mb_ptr->feat <= FEAT_WALL_SOLID)
	{

		/* Mega-Hack -- prevent infinite loops */
		if (randint0(100) < 10) return (FALSE);

		/* Not if hungry */
		if (bp_ptr->status.weak) return (FALSE);

		/* Mega-Hack -- allow "stone to mud" */
		if (mb_ptr->feat != FEAT_RUBBLE &&
			(borg_spell_okay_fail(REALM_ARCANE, 2, 4, 60) ||
			borg_spell_okay_fail(REALM_NATURE, 1, 0, 60) ||
			borg_spell_okay_fail(REALM_CHAOS, 2, 3, 60) ||
			borg_mutation_check(MUT1_EAT_ROCK, TRUE) ||
			borg_racial_check(RACE_HALF_GIANT, TRUE)))
		{
			/* Lose old target */
			borg_keypress('*');
			borg_keypress(ESCAPE);

			/* Mega-Hack -- allow "stone to mud" */
			if (borg_spell(REALM_ARCANE, 2, 4) ||
				borg_spell(REALM_NATURE, 1, 0) ||
				borg_spell(REALM_CHAOS, 2, 3) ||
				borg_mutation(MUT1_EAT_ROCK) ||
				borg_racial(RACE_HALF_GIANT))
			{
				borg_note("# Melting a wall (%c)", I2D(dir));
				borg_keypress(I2D(dir));
				return (TRUE);
			}
		}

		/* No tunneling if in danger */
		if (borg_danger(c_x, c_y, 1, TRUE) >= bp_ptr->chp / 4) return (FALSE);

		/* Tunnel */
		borg_note("# Digging through wall/etc");
		borg_keypress('0');
		borg_keypress('9');
		borg_keypress('9');

		borg_keypress('T');
		borg_keypress(I2D(dir));
		return (TRUE);
	}

	/* Perhaps the borg could search for traps as he walks around level one. */
	if ((bp_ptr->max_lev <= 3) && bp_ptr->depth &&
		!bp_ptr->status.search && borg_needs_searching)
	{
		borg_keypress('S');
	}

	/* Turn off the searching if needed */
	if (!borg_needs_searching && bp_ptr->status.search)
	{
		borg_keypress('S');
	}

	/* Don't step on scary floors */
	if (goal != GOAL_FEAT && !borg_on_safe_feat(mb_ptr->feat))
	{
		/* Don't let the borg step on a painful floor */
		borg_flow_clear();
		return (FALSE);
	}

	/* Walk in that direction */
	if (my_need_alter)
	{
		borg_keypress('+');
		my_need_alter = FALSE;
	}

	borg_keypress(I2D(dir));

	/* Stand stairs up */
	if (goal_less)
	{
		/* Up stairs */
		if (mb_ptr->feat == FEAT_LESS)
		{
			/* Stand on stairs */
			goal_less = FALSE;

			/* Success */
			return (TRUE);
		}
	}

	/* Did something */
	return (TRUE);
}


/*
 * Act twitchy
 */
bool borg_twitchy(void)
{
	int dir;

	/* This is a bad thing */
	borg_note("# Twitchy!");

	/* try to phase out of it */
	if (bp_ptr->able.phase && borg_caution_phase(15, 2) &&
		(borg_spell_fail(REALM_ARCANE, 0, 4, 40) ||
		 borg_spell_fail(REALM_SORCERY, 0, 1, 40) ||
		 borg_spell_fail(REALM_TRUMP, 0, 0, 40) ||
		 borg_activate(BORG_ACT_PHASE_DOOR) ||
		 borg_activate(BORG_ACT_TELEPORT) ||
		 borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
	{
		/* We did something */
		return (TRUE);
	}
	/* Pick a random direction */
	dir = randint1(9);

	/* Hack -- set goal */
	g_x = c_x + ddx[dir];
	g_y = c_y + ddy[dir];

	/* Maybe alter */
	if (randint0(100) < 10 && dir != 5)
	{
		/* Send action (alter) */
		borg_keypress('+');

		/* Send direction */
		borg_keypress(I2D(dir));
	}

	/* Normally move */
	else
	{
		/* Send direction */
		borg_keypress(I2D(dir));
	}

	/* We did something */
	return (TRUE);
}


/* short hand options for borg_flow_spread */
#define BORG_FLOW_SIMPLE		0x00000000L
#define BORG_FLOW_OPTIMIZE		0x00000001L
#define BORG_FLOW_AVOID 		0x00000002L
#define BORG_FLOW_OPTI_AVOID    0x00000003L
#define BORG_FLOW_TUNNELING		0x00000004L
#define BORG_FLOW_FEAT_HURT		0x00000008L

/*
 * Spread a "flow" from the "destination" grids outwards
 *
 * We fill in the "cost" field of every grid that the player can
 * "reach" with the number of steps needed to reach that grid,
 * if the grid is "reachable", and otherwise, with "255", which
 * is the largest possible value that can be stored in a byte.
 *
 * Thus, certain grids which are actually "reachable" but only by
 * a path which is at least 255 steps in length will thus appear
 * to be "unreachable", but this is not a major concern.
 *
 * We use the "flow" array as a "circular queue", and thus we must
 * be careful not to allow the "queue" to "overflow".  This could
 * only happen with a large number of distinct destination points,
 * each several units away from every other destination point, and
 * in a dungeon with no walls and no dangerous monsters.  But this
 * is technically possible, so we must check for it just in case.
 *
 * We do not need a "priority queue" because the cost from grid to
 * grid is always "one" and we process them in order.  If we did
 * use a priority queue, this function might become unusably slow,
 * unless we reactivated the "room building" code.
 *
 * We handle both "walls" and "danger" by marking every grid which
 * is "impassible", due to either walls, or danger, as "ICKY", and
 * marking every grid which has been "checked" as "KNOW", allowing
 * us to only check the wall/danger status of any grid once.  This
 * provides some important optimization, since many "flows" can be
 * done before the "ICKY" and "KNOW" flags must be reset.
 *
 * Note that the "borg_enqueue_grid()" function should refuse to
 * enqueue "dangeous" destination grids, but does not need to set
 * the "KNOW" or "ICKY" flags, since having a "cost" field of zero
 * means that these grids will never be queued again.  In fact,
 * the "borg_enqueue_grid()" function can be used to enqueue grids
 * which are "walls", such as "doors" or "rubble".
 *
 * This function is extremely expensive, and is a major bottleneck
 * in the code, due more to internal processing than to the use of
 * the "borg_danger()" function, especially now that the use of the
 * "borg_danger()" function has been optimized several times.
 *
 * The "optimize" flag allows this function to stop as soon as it
 * finds any path which reaches the player, since in general we are
 * looking for paths to destination grids which the player can take,
 * and we can stop this function as soon as we find any usable path,
 * since it will always be as short a path as possible.
 *
 * We queue the "children" in reverse order, to allow any "diagonal"
 * neighbors to be processed first, since this may boost efficiency.
 *
 * Note that we should recalculate "danger", and reset all "flows"
 * if we notice that a wall has disappeared, and if one appears, we
 * must give it a maximal cost, and mark it as "icky", in case it
 * was currently included in any flow.
 *
 * If a "depth" is given, then the flow will only be spread to that
 * depth, note that the maximum legal value of "depth" is 250.
 */
static void borg_flow_spread(int depth, u32b flow_how)
/*							 bool optimize, bool avoid,
                             bool tunneling, bool feat_hurt)*/
{
	int i;
	int n, o = 0;
	int x1, y1;
	int x, y;

	map_block *mb_ptr = map_loc(c_x, c_y);

	int player_cost = mb_ptr->cost;

	bool optimize	= (flow_how & BORG_FLOW_OPTIMIZE) ? TRUE : FALSE;
	bool avoid		= (flow_how & BORG_FLOW_AVOID) ? TRUE : FALSE;
	bool tunneling  = (flow_how & BORG_FLOW_TUNNELING) ? TRUE : FALSE;
	bool feat_hurt  = (flow_how & BORG_FLOW_FEAT_HURT) ? TRUE : FALSE;

	/* Now process the queue */
	while (flow_head != flow_tail)
	{
		/* Extract the next entry */
		x1 = borg_flow_x[flow_tail];
		y1 = borg_flow_y[flow_tail];

		/* Circular queue -- dequeue the next entry */
		if (++flow_tail == BORG_FLOW_MAX) flow_tail = 0;

		/* Bounds checking */
		if (!map_in_bounds(x1, y1)) continue;

		mb_ptr = map_loc(x1, y1);

		/* Cost (one per movement grid) */
		n = mb_ptr->cost + 1;

		/* New depth */
		if (n > o)
		{
			/* Optimize (if requested) */
			if (optimize && (n > player_cost)) break;

			/* Limit depth */
			if (n > depth) break;

			/* Save */
			o = n;
		}

		/* Queue the "children" */
		for (i = 0; i < 8; i++)
		{
			int old_head;

			map_block *mb_ptr;

			/* Neighbor grid */
			x = x1 + ddx_ddd[i];
			y = y1 + ddy_ddd[i];

			/* Only on legal grids */
			if (!map_in_bounds(x, y)) continue;

			/* Access the grid */
			mb_ptr = map_loc(x, y);

			/* Skip "reached" grids */
			if (mb_ptr->cost <= n) continue;

			/* If the borg is not ready to tunnel */
			if (!tunneling)
			{
				/* Avoid walls */
				if (mb_ptr->feat >= FEAT_SECRET &&
					mb_ptr->feat <= FEAT_WALL_SOLID) continue;

				/* Avoid pillars */
				if (mb_ptr->feat == FEAT_PILLAR) continue;

				/* Avoid Jungle */
				if (mb_ptr->feat >= FEAT_FENCE &&
					mb_ptr->feat <= FEAT_JUNGLE) continue;
			}

			/* Avoid "perma-wall" grids */
			if (mb_ptr->feat >= FEAT_PERM_EXTRA &&
				mb_ptr->feat <= FEAT_PERM_SOLID) continue;
			
			/* Sometimes allows painfull feats to be included */
			if (!feat_hurt && !borg_on_safe_feat(mb_ptr->feat)) continue;

			/* Avoid unknown grids (if requested or retreating) */
			if ((avoid || borg_desperate) && !mb_ptr->feat) continue;

			/* Avoid Monsters if Desprerate */
			if (borg_desperate && (mb_ptr->monster)) continue;


			/* Avoid Traps if low level-- unless brave or scaryguy. */
			if (mb_ptr->trap && avoidance <= bp_ptr->chp && 
				!scaryguy_on_level)
			{
				/* Do not disarm when you could end up dead */
				if (bp_ptr->chp < 60) continue;

				/* Do not disarm when clumsy */
				if ((bp_ptr->skill_dis < 30) && (bp_ptr->lev < 20)) continue;
				if ((bp_ptr->skill_dis < 45) && (bp_ptr->lev < 10)) continue;
			}


			/* Ignore "icky" grids */
			if (mb_ptr->info & BORG_MAP_ICKY) continue;


			/* Analyze every grid once */
			if (!(mb_ptr->info & BORG_MAP_KNOW))
			{
				int p;

				/* Mark as known */
				mb_ptr->info |= BORG_MAP_KNOW;

				if (!borg_desperate)
				{
					/* Get the danger */
					p = borg_danger(x, y, 1, TRUE);

					/* Dangerous grid */
					if (p > avoidance / 3)
					{
						/* Mark as icky */
						mb_ptr->info |= BORG_MAP_ICKY;

						/* Ignore this grid */
						continue;
					}
				}
			}


			/* Save the flow cost */
			mb_ptr->cost = n;

			/* Enqueue that entry */
			borg_flow_x[flow_head] = x;
			borg_flow_y[flow_head] = y;

			/* Circular queue -- memorize head */
			old_head = flow_head;

			/* Circular queue -- insert with wrap */
			if (++flow_head == BORG_FLOW_MAX)
				flow_head = 0;

			/* Circular queue -- handle overflow (badly) */
			if (flow_head == flow_tail)
				flow_head = old_head;
		}
	}

	/* Forget the flow info */
	flow_head = flow_tail = 0;
}


/*
 * Enqueue a fresh (legal) starting grid, if it is safe
 */
static void borg_flow_enqueue_grid(int x, int y)
{
	int old_head;

	map_block *mb_ptr;

	/* Bounds checking */
	if (!map_in_bounds(x, y)) return;

	mb_ptr = map_loc(x, y);

	/* Avoid icky grids */
	if (mb_ptr->info & BORG_MAP_ICKY) return;

	/* Unknown */
	if (!(mb_ptr->info & BORG_MAP_KNOW))
	{
		/* Mark as known */
		mb_ptr->info |= BORG_MAP_KNOW;

		/* Mark dangerous grids as icky */
		if ((borg_danger(x, y, 1, TRUE) > avoidance / 3) && !borg_desperate)
		{
			/* Icky */
			mb_ptr->info |= BORG_MAP_ICKY;

			/* Avoid */
			return;
		}
	}


	/* Only enqueue a grid once */
	if (mb_ptr->cost == 1) return;

	/* Save the flow cost (zero) */
	mb_ptr->cost = 1;

	/* Enqueue that entry */
	borg_flow_y[flow_head] = y;
	borg_flow_x[flow_head] = x;


	/* Circular queue -- memorize head */
	old_head = flow_head;

	/* Circular queue -- insert with wrap */
	if (++flow_head == BORG_FLOW_MAX) flow_head = 0;

	/* Circular queue -- handle overflow */
	if (flow_head == flow_tail) flow_head = old_head;
}



/*
 * Do a "reverse" flow from the player outwards
 */
static void borg_flow_reverse(void)
{
	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the player's grid */
	borg_flow_enqueue_grid(c_x, c_y);

	/* Spread, but do NOT optimize */
	borg_flow_spread(250, BORG_FLOW_SIMPLE);
}

/*
 * Commit the current "flow"
 */
static bool borg_flow_commit(cptr who, int why)
{
	int cost;

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Cost of current grid */
	cost = mb_ptr->cost;

	/* Verify the total "cost" */
	if (cost >= 250) return (FALSE);

	/* Message */
	if (who) borg_note("# Flowing toward %s at cost %d", who, cost);

	/* Iterate over all grids */
	MAP_ITT_START (mb_ptr)
	{
		/* Obtain the "flow" information */
		mb_ptr->flow = mb_ptr->cost;
	}
	MAP_ITT_END;

	/* Save the goal type */
	goal = why;

	/* Success */
	return (TRUE);
}



/*
 * Attempt to take an optimal step towards the current goal location
 *
 * Note that the "borg_update()" routine notices new monsters and objects,
 * and movement of monsters and objects, and cancels any flow in progress.
 *
 * Note that the "borg_update()" routine notices when a grid which was
 * not thought to block motion is discovered to in fact be a grid which
 * blocks motion, and removes that grid from any flow in progress.
 *
 * When given multiple alternative steps, this function attempts to choose
 * the "safest" path, by penalizing grids containing embedded gold, monsters,
 * rubble, doors, traps, store doors, and even floors.  This allows the Borg
 * to "step around" dangerous grids, even if this means extending the path by
 * a step or two, and encourages him to prefer grids such as objects and stairs
 * which are not only interesting but which are known not to be invisible traps.
 *
 * XXX XXX XXX XXX This function needs some work.  It should attempt to
 * analyze the "local region" around the player and determine the optimal
 * choice of locations based on some useful computations.
 *
 * If it works, return TRUE, otherwise, cancel the goal and return FALSE.
 */
bool borg_flow_old(int why)
{
	int x, y;

	map_block *mb_ptr;

	/* Continue */
	if (goal == why)
	{
		int b_n = 0;

		int i, b_i = -1;

		int c, b_c;

		mb_ptr = map_loc(c_x, c_y);

		/* Flow cost of current grid */
		b_c = mb_ptr->flow * 10;

		/* Prevent loops */
		b_c = b_c - 5;


		/* Look around */
		for (i = 0; i < 8; i++)
		{
			/* Grid in that direction */
			x = c_x + ddx_ddd[i];
			y = c_y + ddy_ddd[i];

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			/* Access the grid */
			mb_ptr = map_loc(x, y);

			/* Flow cost at that grid */
			c = mb_ptr->flow * 10;

			/* Never backtrack */
			if (c > b_c) continue;

			/* Notice new best value */
			if (c < b_c) b_n = 0;

			/* Apply the randomizer to equivalent values */
			if ((++b_n >= 2) && (randint0(b_n)))
			{
				continue;
			}
			/* Track it */
			b_i = i;
			b_c = c;
		}


		/* Try it */
		if (b_i >= 0)
		{
			/* Access the location */
			x = c_x + ddx_ddd[b_i];
			y = c_y + ddy_ddd[b_i];

			/* Attempt motion */
			if (borg_play_step(y, x)) return (TRUE);
		}

		/* Cancel goal */
		goal = GOAL_NONE;
	}

	/* Nothing to do */
	return (FALSE);
}

/*
 * Flow closer to a set of coordinates.  This proc aims to get right on those
 * coords.  That is closer than needed for a word of recall, but it is an easy
 * way to make sure that the borg doesn't go down the wrong dungeon.
 */
static bool borg_flow_block(int x, int y, cptr reason, int why)
{
	/* Don't leave the map */
	if (x < 0 || x > BORG_MAX_WILD_SIZE) return (FALSE);
 
	/* Don't leave the map */
	if (y < 0 || y > BORG_MAX_WILD_SIZE) return (FALSE);

	/* Flow a block east */
	if (c_x < x)
		x = MIN(c_x + 16, x);

	/* Flow a block west */
	else if (c_x > x)
		x = MAX(c_x - 16, x);
	else
		x = c_x;

	/* Flow a block south */
	if (c_y < y)
		y = MIN(c_y + 16, y);

	/* Flow a block north */
	else if (c_y > y)
		y = MAX(c_y - 16, y);
	else
		y = c_y;

	/* No need to go where you already are */
	if (x == c_x && y == c_y) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the grid */
	borg_flow_enqueue_grid(x, y);

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTI_AVOID);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit(reason, why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards a specific shop entry
 */
bool borg_flow_shop_entry(int i)
{
	int x, y;

	/* Must be in town */
	if (bp_ptr->depth) return (FALSE);

	/* Is it a valid shop? */
	if (i < 0 || i > borg_shop_num) return (FALSE);

	/* Pick up the new target, but not too far */
	if (c_x < borg_shops[i].x)
	{
		/* Flow a block east */
		x = MIN(c_x + 16, borg_shops[i].x);
	}
	else if (c_x > borg_shops[i].x)
	{
		/* Flow a block west */
		x = MAX(c_x - 16, borg_shops[i].x);
	}
	else x = c_x;

	if (c_y < borg_shops[i].y)
	{
		/* Flow a block south */
		y = MIN(c_y + 16, borg_shops[i].y);
	}
	else if (c_y > borg_shops[i].y)
	{
		/* Flow a block north */
		y = MAX(c_y - 16, borg_shops[i].y);
	}
	else y = c_y;

	/* Hack -- re-enter a shop if needed */
	if (x == c_x && y == c_y)
	{
		/* Note */
		borg_note("# Re-entering a shop");

		/* Enter the store */
		borg_keypress('5');

		/* Success */
		return (TRUE);
	}

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the grid */
	borg_flow_enqueue_grid(x, y);

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("shop", GOAL_SHOP)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_SHOP)) return (FALSE);

	/* Success */
	return (TRUE);
}


/* Choose a shop to visit */
bool borg_choose_shop(void)
{
	int i;
	s32b dist, b_d = BORG_SMALL_DISTANCE;

	/* Must be in town */
	if (bp_ptr->depth) return (FALSE);

	/* Only try to find a shop when there is no goal */
	if (goal && goal != GOAL_SHOP) return (FALSE);

	/* If we are already flowing toward a shop do not check again... */
	if (goal_shop != -1)
	{
		borg_note("# Using previous goal shop: %d", goal_shop);
		return (TRUE);
	}
	
	/* Find 'best' shop to go to */
	for (i = 0; i < borg_shop_num; i++)
	{
		/* Do not revisit shops */
		if (borg_shops[i].visit) continue;

		/* Get distance */
		dist = distance(c_x, c_y, borg_shops[i].x, borg_shops[i].y);

		/* Visit only the shops in one town */
		if (dist > BORG_SMALL_DISTANCE) continue;

		/* Only closer shops */
		if (dist >= b_d) continue;

		goal_shop = i;
	}

	/* All shops have been visited */
	if (goal_shop == -1) return (FALSE);

	/* We want to shop */
	borg_note("# Goal shop: %d, dist: %d", goal_shop, b_d);

	/* Success */
	return (TRUE);
}


/* Find the closest shop from the closest town */
bool borg_find_shop(void)
{
	if (borg_choose_shop())
	{
		/* Try and visit a shop, if so desired */
		if (borg_flow_shop_entry(goal_shop)) return (TRUE);

		/* Let us know what the value is when we fail */
		borg_note("# Failed to get good shop!");
	}

	goal = GOAL_NONE;
	goal_shop = -1;
	shop_num = -1;

	/* No shop */
	return (FALSE);
}


/* Find a town to blow some money */
bool borg_find_town(void)
{
	int i, b_i = -1;
	int d, b_d = BORG_MAX_DISTANCE;

	/* Only on the surface */
	if (bp_ptr->depth && !vanilla_town) return (FALSE);

	/* No known town */
	if (!borg_town_num) return (FALSE);

	/* Not enough money */
	if (borg_gold < 20) return (FALSE);

	/* Buying doesn't really help you now, unless you have loads */
	if (borg_prepared_depth() > 20 && borg_gold < 100000) return (FALSE);

	/* Leave if there is a goal already */
	if (goal && goal != GOAL_TOWN) return (FALSE);

	/* Remember previous effort */
	if (goal == GOAL_TOWN)
	{
		b_i = goal_town;

		/* Get the distance to this place */
		b_d = distance(c_x, c_y, borg_towns[b_i].x, borg_towns[b_i].y);
	}
	/* Find the closest, non-visited town */
	else
	{
		/* Loop through the towns */
		for (i = 0; i < borg_town_num; i++)
		{
			/* Get the distance to this place */
			d = distance(c_x, c_y, borg_towns[i].x, borg_towns[i].y);

			/* Was it visited recently? */
			if (borg_towns[i].visit) continue;

			/* Is it closer? */
			if (d >= b_d) continue;

			/* Remember this one */
			b_i = i;
			b_d = d;
		}
	}

	/* All towns were visited since last dungeon crawl */
	if (b_i == -1) return (FALSE);

	/* More or less in town */
	if (b_d < BORG_SMALL_DISTANCE)
	{
		/* Mark this place to prevent looping */
		borg_towns[b_i].visit = TRUE;

		/* No need to go here anymore */
		goal_town = -1;

		/* Go shopping */
		if (borg_find_shop()) return (TRUE);

		/* Oh well */
		return (FALSE);
	}

	/* Go closer */
	if (borg_flow_block(borg_towns[b_i].x,
						borg_towns[b_i].y,
						borg_towns[b_i].name,
						GOAL_TOWN))
	{
		/* Happy */
		return (TRUE);
	}
	else
	{
		borg_note("Flow block failed because the target was in deep water");
	}

	goal = GOAL_NONE;
	goal_town = -1;

	/* The borg is in that town */
	return (FALSE);
}


/* Find a new dungeon */
bool borg_find_dungeon(void)
{
	int i, b_i = -1;
	int d, b_d = BORG_MAX_DISTANCE;
	int p;

	/* Do this only on the surface */
	if (bp_ptr->depth) return (FALSE);

	/* Find the target depth */
	p = borg_prepared_depth();

	/* A bit of trickery for the vanilla_town */
	if (vanilla_town)
	{
		/* Tell the borg to enter the only existing dungeon */
		goal = GOAL_CAVE;
		goal_dungeon = 0;
	}
	else
	{
		/* Not when the borg knows only a few towns */
		if ((p > 20) &&
		    ((bp_ptr->lev > 20 && borg_town_num < 5) ||
		     (bp_ptr->lev > 30 && borg_town_num < 13) ||
		     (bp_ptr->lev > 40 && borg_town_num < 19))) return (FALSE);
	}

	/* Not when the borg is exploring the wilderness */
	if (goal && goal != GOAL_CAVE) return (FALSE);

	/* Remember previous effort */
	if (goal == GOAL_CAVE)
	{
		b_i = goal_dungeon;
		b_d = distance(c_x, c_y, borg_dungeons[b_i].x, borg_dungeons[b_i].y);
	}
	/* Find the closest non-visited dungeon */
	else
	{
		/* Loop through the dungeons */
		for (i = 0; i < borg_dungeon_num; i++)
		{
			/* This dungeon starts too deep */
			if (borg_dungeons[i].min_depth > p) continue;

			/* This dungeon ends too shallow */
			if (borg_dungeons[i].max_depth < p &&
				borg_dungeons[i].bottom) continue;

			/* How far away is this? */
			d = distance(c_x, c_y, borg_dungeons[i].x, borg_dungeons[i].y);

			/* Ignore dungeons far away */
			if (d > b_d) continue;

			/* Remember this one */
			b_i = i;
			b_d = d;
		}
	}

	/* No good dungeon found */
	if (b_i == -1) return (FALSE);

	/* If the borg is right there at the dungeon */
	if (b_d == 0)
	{
		/* Reached the place */
		goal_dungeon = -1;

		/* If the borg leaves the surface */
		if (!bp_ptr->depth) borg_leave_surface();

			/* If the dungeon was visited and the target depth is not shallow */
		if (p >= borg_dungeons[b_i].min_depth + 4 &&
			borg_dungeons[b_i].min_depth != 0 &&
			borg_dungeons[b_i].min_depth < 5 + borg_dungeons[b_i].max_depth != 0 &&
			bp_ptr->recall >= 4 && borg_recall())
		{
			/* Note */
			borg_note("# Recalling into dungeon.");
		}
		else
		{
			/* Take those stairs */
			borg_keypress('>');
		}

		return (TRUE);
	}

	/* Go closer to that dungeon */
	if (borg_flow_block(borg_dungeons[b_i].x,
						borg_dungeons[b_i].y,
						"my dungeon",
						GOAL_CAVE))
	{
		goal_dungeon = b_i;

		/* Happy */
		return (TRUE);
	}

	goal_dungeon = -1;
	goal = GOAL_NONE;

	return (FALSE);
}


/*
 * Prepare to flee the level via stairs
 */
bool borg_flow_stair_both(int why)
{
	int i;

	/* None to flow to */
	if (!track_less_num && !track_more_num) return (FALSE);

	/* dont go down if hungry or low on food, unless fleeing a scary town */
	if ((!goal_fleeing && !bp_ptr->depth) &&
		(!bp_ptr->cur_lite || bp_ptr->status.weak ||
		 bp_ptr->status.hungry || (bp_ptr->food < 2)))
		return (FALSE);

	/* clear the possible searching flag */
	borg_needs_searching = FALSE;

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < track_less_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(track_less_x[i], track_less_y[i]);
	}

	/* Enqueue useful grids */
	for (i = 0; i < track_more_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(track_more_x[i], track_more_y[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to flow towards "up" stairs
 */
bool borg_flow_stair_less(int why)
{
	int i;

	/* None to flow to */
	if (!track_less_num) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* clear the possible searching flag */
	borg_needs_searching = FALSE;

	/* Enqueue useful grids */
	for (i = 0; i < track_less_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(track_less_x[i], track_less_y[i]);
	}

	if ((bp_ptr->lev > 35) || !bp_ptr->cur_lite)
	{
		/* Spread the flow */
		borg_flow_spread(250, BORG_FLOW_OPTIMIZE);
	}
	else
	{
		/* Spread the flow, No Optimize, Avoid */
		borg_flow_spread(250, (borg_desperate) ? BORG_FLOW_SIMPLE
                                               : BORG_FLOW_AVOID);
	}

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("up-stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to flow towards "down" stairs
 */
bool borg_flow_stair_more(int why)
{
	int i;

	/* None to flow to */
	if (!track_more_num) return (FALSE);

	/* if not fleeing do not go down unless safe */
	if (!goal_fleeing && borg_prepared(bp_ptr->depth + 1))
		return (FALSE);

	/* dont go down if hungry or low on food, unless fleeing a scary town */
	if (bp_ptr->depth &&
		(bp_ptr->status.weak || bp_ptr->status.hungry || (bp_ptr->food < 2)))
		return (FALSE);

	/* No diving if no light */
	if (!bp_ptr->cur_lite) return (FALSE);

	/* don't head for the stairs if you are recalling,  */
	/* even if you are fleeing. */
	if (goal_recalling)	return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < track_more_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(track_more_x[i], track_more_y[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("down-stairs", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Prepare to flow towards glyph of warding
 */
bool borg_flow_glyph(int why)
{
	int i;

	/* None to flow to */
	if (!track_glyph_num) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < track_glyph_num; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(track_glyph_x[i], track_glyph_y[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("glyph of warding", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to flow towards light
 */
bool borg_flow_light(int why)
{
	int y, x, i;

	map_block *mb_ptr;

	/* reset counters */
	borg_temp_n = 0;
	i = 0;

	/* build the glow array */
	/* Scan map */
	MAP_ITT_START (mb_ptr)
	{
		/* Not a perma-lit, and not our spot. */
		if (!(mb_ptr->flags & MAP_GLOW)) continue;

		/* Get location */
		MAP_GET_LOC(x, y);

		/* keep count */
		borg_temp_y[borg_temp_n] = y;
		borg_temp_x[borg_temp_n] = x;
		borg_temp_n++;
	}
	MAP_ITT_END;

	/* None to flow to */
	if (!borg_temp_n) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_temp_x[i], borg_temp_y[i]);
	}

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("a lighted area", why)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(why)) return (FALSE);

	/* Success */
	return (TRUE);
}


/* When it is dark and the borg is outside, find an inn */
bool borg_waits_daylight(void)
{
	int i, b_i = -1;
	int d, b_d = BORG_MAX_DISTANCE;

	/* Is there a wilderness? */
	if (vanilla_town) return (FALSE);

	/* Is the borg at the surface? */
	if (bp_ptr->depth) return (FALSE);

	/* Is it dark at all? */
	if (bp_ptr->hour > 5 && bp_ptr->hour < 18) return (FALSE);

	/* Find the closest inn */
	for (i = 0; i < borg_shop_num; i++)
	{
		if (borg_shops[i].type != BUILD_INN) continue;

		/* How far away is this? */
		d = distance(c_x, c_y, borg_dungeons[i].x, borg_dungeons[i].y);

		/* Ignore inns far away */
		if (d > b_d) continue;

		/* Remember this one */
		b_i = i;
		b_d = d;
	}

	goal_shop = b_i;

	/* Go to that inn */
	if (b_i != -1 && borg_flow_shop_entry(b_i) && borg_gold > 25)
	{
		borg_note("# Looking for a place to spend the night.");

		return (TRUE);
	}

	/* Don't rest when it hurts */
	if (!borg_on_safe_feat(map_loc(c_x, c_y)->feat)) return (FALSE);

	/* Wait out the night */
	borg_keypress(ESCAPE);
	borg_keypress('0');
	borg_keypress('2');
	borg_keypress('9');
	borg_keypress('9');
	borg_keypress('R');

	borg_note("# Wait out the night.");

	return (TRUE);
}


/*
 * This proc returns TRUE if from (x, y) there is a monster to target, or the
 * spot next to a monster.  It fails if the new position is next to a monster.
 * The method to find out if there is a monster is taken from borg_temp_fill.
 */
static bool borg_aim_ball(int x, int y)
{
	int i, dx, dy;
	int x1, y1;
	bool found_target = FALSE;

	/* Loop through the close, known monsters */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Fail if the new position is next to a monster */
		if (distance(x, y, borg_temp_x[i], borg_temp_y[i]) == 1) return (FALSE);

		/* Don't bother if a monster has been found already */
		if (!found_target)
		{
			for (dx = -1; dx <= 1; dx++)
			{
				for (dy = -1; dy <= 1; dy++)
				{
					/* Get the coords of the target grid */
					x1 = borg_temp_x[i] + dx;
					y1 = borg_temp_y[i] + dy;

					/* Bounds checking */
					if (!map_in_bounds(x1, y1)) continue;

					/* Is this grid out of range? */
					if (distance(x, y, x1, y1) > MAX_RANGE) continue;

					/* If the borg has no ESP
					 * or the borg has ESP and has just hit his target
					 * assume there is no wall in the way
					 * OR
					 * If the borg has ESP and has just missed his target
					 * assume there is a wall in the way
					 */
					if (((!FLAG(bp_ptr, TR_TELEPATHY) ||
						(FLAG(bp_ptr, TR_TELEPATHY) && successful_target)) &&
						borg_los(x, y, x1, y1))
						||
						(FLAG(bp_ptr, TR_TELEPATHY) && !successful_target &&
						borg_los_pure(x, y, x1, y1)))
					{
						/* If it is not a wall it is OK for a ball */
						if (!borg_cave_wall_grid(map_loc(x, y)))
						{
							/* Found a targettable monster */
							found_target = TRUE;
						}
					}
				}
			}
		}
	}

	/* return result */
	return (found_target);
}

/*
 * Take a couple of steps to line up a shot
 *
 */
bool borg_flow_kill_aim(bool viewable)
{
	int o_y, o_x;

	/* Consider each adjacent spot */
	for (o_x = c_x - 2; o_x <= c_x + 2; o_x++)
	{
		for (o_y = c_y - 2; o_y <= c_y + 2; o_y++)
		{
			/* borg_attack would have already checked
			   for a shot from where I currently am */
			if (o_x == c_x && o_y == c_y) continue;

			/* avoid screen edgeds */
			if (!map_in_bounds(o_x, o_y)) continue;

			/* Is there a possible target? */
			if (borg_aim_ball(o_x, o_y))
			{
				/* Clear the flow codes */
				borg_flow_clear();

				/* Enqueue the grid */
				borg_flow_enqueue_grid(o_x, o_y);

				/* Spread the flow */
				borg_flow_spread(5, (viewable) ? BORG_FLOW_OPTIMIZE
											   : BORG_FLOW_OPTI_AVOID);

				/* Attempt to Commit the flow */
				if (!borg_flow_commit("targetable position", GOAL_KILL))
					return (FALSE);

				/* Take one step */
				if (!borg_flow_old(GOAL_KILL)) return (FALSE);

				/* success */
				return (TRUE);
			}
		}
	}

	/* No new flow */
	return (FALSE);
}

/*
 * Dig an anti-summon corridor
 *
 *            ############## We want the borg to not dig #1
 *            #............# but to dig #2, and hopefully shoot from the
 *      #######............# last #2 and try to avoid standing on #3.
 *      #222223............# This is great for offset ball attacks but
 *      #2#####..s.........# not for melee.  Warriors need to dig a wall
 * ######2###########+###### adjacent to the monsters so he can swing on them.
 * #            1     #
 * # ################ #
 *   #              # #
 * ###              # #
 *
 */
bool borg_flow_kill_corridor(bool viewable)
{
#if 0
	int o_y, o_x;
	int m_x, m_y;
	int f_y, f_x;
	int floors = 0;
	int b_y = 0, b_x = 0;
	int perma_grids = 0;

	borg_kill *kill;

#endif /* 0 */

	/* Hack - ignore usused parameter */
	(void)viewable;

	/* Efficiency -- Nothing to kill */
	if (!borg_kills_cnt) return (FALSE);

	/* Need to do this properly */
#if 0

	/* Only do this to summoners when they are close */
	if (borg_kills_summoner == -1) return (FALSE);

	/* Do not dig when weak. It takes too long */
	if (my_stat_ind[A_STR] < 17) return (FALSE);

	/* Do not dig when confused */
	if (bp_ptr->status.confused) return (FALSE);

	/* Not when darkened */
	if (!bp_ptr->cur_lite) return (FALSE);

	/* get the summoning monster */
	kill = &borg_kills[borg_kills_summoner];

	/* Consider each adjacent spot to monster */
	for (o_x = -1; o_x <= 1; o_x++)
	{
		for (o_y = -1; o_y <= 1; o_y++)
		{
			map_block *mb_ptr;

			/* Check grids near monster */
			m_x = kill->x + o_x;
			m_y = kill->y + o_y;

			/* avoid screen edges */
			if (!map_in_bounds(m_x + 2, m_y + 2)) continue;
			if (!map_in_bounds(m_x - 2, m_y - 2)) continue;

			/* Bounds checking */
			if (!map_in_bounds(m_x, m_y)) continue;

			/* get the grid */
			mb_ptr = map_loc(m_x, m_y);

			/* Can't tunnel a non wall or permawall */
			if (mb_ptr->feat >= FEAT_FLOOR &&
				mb_ptr->feat < FEAT_MAGMA) continue;
			if (mb_ptr->feat >= FEAT_PERM_EXTRA &&
				mb_ptr->feat <= FEAT_PERM_SOLID)
			{
				perma_grids++;
				continue;
			}

			/* Do not dig unless we appear strong enough to succeed or we have a digger */
			if (borg_spell_legal(REALM_ARCANE, 2, 4) ||
				borg_spell_legal(REALM_NATURE, 1, 0) ||
				borg_spell_legal(REALM_CHAOS, 2, 3) ||
				borg_mutation_check(MUT1_EAT_ROCK, TRUE) ||
				borg_racial_check(RACE_HALF_GIANT, TRUE) ||
				(bp_ptr->skill_dig > (bp_ptr->depth > 80 ? 30 : 40)))
			{
				/* digging ought to work */
			}
			else
			{
				/* do not try digging */
				continue;
			}

			/* reset floors counter */
			floors = 0;

			/* That grid must not have too many floors adjacent */
			for (f_x = -1; f_x <= 1; f_x++)
			{
				for (f_y = -1; f_y <= 1; f_y++)
				{
					/* Bounds checking */
					if (!map_in_bounds(m_x + f_x, m_y + f_y)) continue;

					/* grid the grid */
					mb_ptr = map_loc(m_x + f_x, m_y + f_y);

					/* check if this neighbor is a floor */
					if (borg_cave_floor_grid(mb_ptr)) floors++;
				}
			}

			/* Do not dig if too many floors near. */
			if (floors >= 5) continue;

			/* Track the good location */
			b_y = m_y;
			b_x = m_x;
		}
	}
	/* NOTE: Perma_grids count the number of grids which contain permawalls.
	 * The borg may try to flow to an unknown grid but may get stuck on a perma
	 * wall.  This will keep him from flowing to a summoner if the summoner is
	 * near a perma grid.  The real fix out to be in the flow_spread so that
	 * he will not flow through perma_grids.  I will work on that next.
	 */
	if (b_y != 0 && b_x != 0 && perma_grids == 0)
	{
		/* Clear the flow codes */
		borg_flow_clear();

		/* Enqueue the grid */
		borg_flow_enqueue_grid(m_x, m_y);

		/* Spread the flow */
		borg_flow_spread(15, BORG_FLOW_OPTIMIZE | BORG_FLOW_TUNNELING);

		/* Attempt to Commit the flow */
		if (!borg_flow_commit("anti-summon corridor", GOAL_KILL))
			return (FALSE);

		/* Take one step */
		if (!borg_flow_old(GOAL_KILL)) return (FALSE);

		return (TRUE);
	}
#endif /* 0 */
	return FALSE;
}


/* Check to see if the borg is standing on a nasty grid.
 * Lava can hurt the borg unless he is IFire.
 * Water can hurt if it is deep/ocean and encumbered.
 * Acid can hurt the borg unless he is IAcid.
 * Swamp can hurt the borg unless he is IPoison.
 * Levitation item can reduce the effect of nasty grids.
 */
bool borg_on_safe_feat(byte feat)
{
	/* Water */
	if (feat == FEAT_DEEP_WATER ||
	 	 feat == FEAT_OCEAN_WATER)
	{
		/* Levitation helps */
		if (FLAG(bp_ptr, TR_FEATHER)) return (TRUE);

		/* Being non-encumbered helps */
		if (!bp_ptr->encumber) return (TRUE);

		/* Everything else hurts */
		return (FALSE);
	}

	/* Nothing hurts when Invulnerable */
	if (borg_goi) return (TRUE);

	/* Lava */
	if (feat == FEAT_DEEP_LAVA)
	{
		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_FIRE)) return (TRUE);

		/* Everything else hurts */
		return (FALSE);
	}

	if (feat == FEAT_SHAL_LAVA)
	{
		/* Levitation helps */
		if (FLAG(bp_ptr, TR_FEATHER)) return (TRUE);

		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_FIRE)) return (TRUE);

		/* Everything else hurts */
		return (FALSE);
	}

	/* Swamp */
	if (feat == FEAT_DEEP_SWAMP)
	{
		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_POIS)) return (TRUE);

		return (FALSE);
	}

	if (feat == FEAT_SHAL_SWAMP)
	{
		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_POIS)) return (TRUE);

		/* (temp) Resistance helps */
		if (FLAG(bp_ptr, TR_WILD_WALK)) return (TRUE);

		/* Levitation helps */
		if (FLAG(bp_ptr, TR_FEATHER)) return (TRUE);

		/* Shallow swamp does hurt */
		return (FALSE);
	}

	/* Acid */
	if (feat == FEAT_DEEP_ACID)
	{
		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_ACID)) return (TRUE);

		/* Everything else hurts */
		return (FALSE);
	}

	if (feat == FEAT_SHAL_ACID)
	{
		/* Immunity helps */
		if (FLAG(bp_ptr, TR_IM_ACID)) return (TRUE);

		/* Levitation helps */
		if (FLAG(bp_ptr, TR_FEATHER)) return (TRUE);

		/* Everything else hurts */
		return (FALSE);
	}
	/* Generally ok */
	return (TRUE);
}


/* If the borg stands on something painfull he'd better move */
bool borg_flow_non_hurt(void)
{
	int x, y;
	int c, b_c = 255;
	map_block *mb_ptr = map_loc(c_x, c_y);

	/* This doesn't hurt */
	if (borg_on_safe_feat(mb_ptr->feat)) return (FALSE);

	/* Search outwards */
	borg_flow_clear();

	/* Enqueue the player's grid */
	borg_flow_enqueue_grid(c_x, c_y);

	/* Spread, but do NOT optimize and allow painful feats */
	borg_flow_spread(10, BORG_FLOW_FEAT_HURT);

	/* Scan the entire map */
	MAP_ITT_START (mb_ptr)
	{
		/* Skip unknown grids */
		if (!mb_ptr->feat) continue;

		/* Skip walls/doors */
		if (borg_cave_wall_grid(mb_ptr)) continue;

		/* Skip painfull grids */
		if (!borg_on_safe_feat(mb_ptr->feat)) continue;

		/* Acquire the cost */
		c = mb_ptr->cost;

		/* Skip "unreachable" grids */
		if (c >= b_c) continue;

		/* Remember this cost */
		b_c = c;

		/* Remember this location */
		MAP_GET_LOC(x, y);
	}
	MAP_ITT_END;

	/* Found no safe feat */
	if (b_c == 255) return (FALSE);

	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue the grid */
	borg_flow_enqueue_grid(x, y);

	/* Spread the flow */
	borg_flow_spread(30, BORG_FLOW_OPTI_AVOID | BORG_FLOW_FEAT_HURT);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("to a safe feat", GOAL_FEAT)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_FEAT)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards monsters to "kill"
 * But in a few phases, viewable, near and far.
 * Note that monsters under the player are always deleted
 */
bool borg_flow_kill(bool viewable, int nearness)
{
	int i, x, y, p, j, b_j = -1;
	int b_stair = -1;

	bool borg_in_hall = FALSE;
	int hall_y, hall_x, hall_walls = 0;
	bool skip_monster = FALSE;

	map_block *mb_ptr;

	/* Efficiency -- Nothing to kill */
	if (!borg_kills_cnt) return (FALSE);

	/* Don't chase down town monsters when you are just starting out */
	if (bp_ptr->depth == 0 && bp_ptr->lev < 7) return (FALSE);

	/* YOU ARE NOT A WARRIOR!! DON'T ACT LIKE ONE!! */
	if ((borg_class == CLASS_MAGE ||
		borg_class == CLASS_HIGH_MAGE ||
		borg_class == CLASS_MINDCRAFTER) &&
		bp_ptr->lev < (bp_ptr->depth ? 35 : 5)) return (FALSE);


	/* Nothing found */
	borg_temp_n = 0;

	/* check to see if in a hall, used later */
	for (hall_x = -1; hall_x <= 1; hall_x++)
	{
		for (hall_y = -1; hall_y <= 1; hall_y++)
		{
			/* Acquire location */
			x = hall_x + c_x;
			y = hall_y + c_y;

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			mb_ptr = map_loc(x, y);

			/* track walls */
			if (				/* (mb_ptr->feat == FEAT_GLYPH) ||
								   (mb_ptr->feat == FEAT_MINOR_GLYPH) || */
				   ((mb_ptr->feat >= FEAT_MAGMA) &&
					(mb_ptr->feat <= FEAT_PERM_SOLID)))
			{
				hall_walls++;
			}

			/* addem up */
			if (hall_walls >= 5) borg_in_hall = TRUE;
		}
	}


	/* Check distance away from stairs, used later */

	/* Check for an existing "up stairs" */
	for (i = 0; i < track_less_num; i++)
	{
		x = track_less_x[i];
		y = track_less_y[i];

		/* How far is the nearest up stairs */
		j = distance(c_y, c_x, y, x);

		/* skip the closer ones */
		if (b_j >= j) continue;

		/* track it */
		b_j = j;
		b_stair = i;
	}

	/* Scan the monster list */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		borg_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Ignore multiplying monsters */
		if (goal_ignoring && !bp_ptr->status.afraid &&
			(FLAG(&r_info[kill->r_idx], RF_MULTIPLY))) continue;

		/* Avoid fighting if a scary guy is on the level */
		if (scaryguy_on_level) continue;

		/* Don't chase our friends or pets */
		if (kill->m_flags & (MONST_FRIEND | MONST_PET)) continue;

		/* Avoid multiplying monsters when low level */
		if (bp_ptr->lev < 10 &&
			(FLAG(&r_info[kill->r_idx], RF_MULTIPLY))) continue;

		/* Hack -- ignore Maggot until later.  Player will chase Maggot
		 * down all accross the screen waking up all the monsters.  Then
		 * he is stuck in a comprimised situation.
		 */
		if ((FLAG(&r_info[kill->r_idx], RF_UNIQUE)) &&
			bp_ptr->depth == 0 && bp_ptr->lev < 5) continue;

		/* Access the location */
		x = kill->x;
		y = kill->y;

		/* Bounds checking */
		if (!map_in_bounds(x, y)) continue;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Require line of sight if requested */
		if (viewable && !(mb_ptr->info & BORG_MAP_VIEW)) continue;

		/* Calculate danger */
		borg_full_damage = FALSE;
		p = borg_danger_aux(x, y, 1, i, TRUE);
		borg_full_damage = FALSE;


		/* Hack -- Skip "deadly" monsters unless uniques */
		if (bp_ptr->lev > 15 && (!FLAG(&r_info[kill->r_idx], RF_UNIQUE)) &&
			p > avoidance / 2) continue;
		if (bp_ptr->lev <= 15 && p > avoidance / 3) continue;

		/* Skip ones that make me wander too far */
		if ((b_stair != -1) && (bp_ptr->lev < 10))
		{
			/* Check the distance of this monster to the stair */
			j = distance(track_less_y[b_stair], track_less_x[b_stair], y, x);
			/* skip far away monsters while I am close to stair */
			if (b_j <= bp_ptr->lev * 5 + 9 &&
				j >= bp_ptr->lev * 5 + 9) continue;
		}

		/* Hack -- Avoid getting surrounded */
		if (borg_in_hall && (FLAG(&r_info[kill->r_idx], RF_FRIENDS)))
		{
			/* check to see if monster is in a hall, */
			for (hall_x = -1; hall_x <= 1; hall_x++)
			{
				for (hall_y = -1; hall_y <= 1; hall_y++)
				{
					/* Bounds checking */
					if (!map_in_bounds(hall_x + x, hall_y + y)) continue;

					mb_ptr = map_loc(hall_x + x, hall_y + y);

					/* track walls */
					if (		/* (mb_ptr->feat == FEAT_GLYPH) ||
								   (mb_ptr->feat == FEAT_MINOR_GLYPH) || */
						   ((mb_ptr->feat >= FEAT_MAGMA) &&
							(mb_ptr->feat <= FEAT_PERM_SOLID)))
					{
						hall_walls++;
					}

					/* we want the monster to be in a hall also
					 *
					 *  ########################
					 *  ############      S  ###
					 *  #         @'     SSS ###
					 *  # ##########       SS###
					 *  # #        #       Ss###
					 *  # #        ###### ######
					 *  # #             # #
					 * Currently, we would like the borg to avoid
					 * flowing to a situation like the one above.
					 * We would like him to stay in the hall and
					 * attack from a distance.  One problem is the
					 * lower case 's' in the corner, He will show
					 * up as being in a corner, and the borg may
					 * flow to it.  Let's hope that is a rare case.
					 *
					 * The borg might flow to the 'dark' south exit
					 * of the room.  This would be dangerous for
					 * him as well.
					 */
					/* add 'em up */
					if (hall_walls < 4)
					{
						/* This monster is not in a hallway.
						 * It may not be safe to fight.
						 */
						skip_monster = TRUE;
					}
				}
			}
		}
		/* skip certain ones */
		if (skip_monster) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing to kill */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Look for something to kill */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_temp_x[i], borg_temp_y[i]);
	}

	/* Spread the flow */
	/* if we are not flowing toward monsters that we can see, make sure they */
	/* are at least easily reachable.  The second flag is whether or not */
	/* to avoid unknown squares.  This was for performance when we have ESP. */
	borg_flow_spread(nearness, (viewable) ? BORG_FLOW_OPTIMIZE
										  : BORG_FLOW_OPTI_AVOID);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit("kill", GOAL_KILL)) return (FALSE);


	/* Take one step */
	if (!borg_flow_old(GOAL_KILL)) return (FALSE);


	/* Success */
	return (TRUE);
}



/*
 * Prepare to "flow" towards objects to "take"
 *
 * Note that objects under the player are always deleted
 */
bool borg_flow_take(bool viewable, int nearness)
{
	int i, x, y;
	int b_stair = -1, j, b_j = -1;

	map_block *mb_ptr;

	/* Efficiency -- Nothing to take */
	if (!borg_takes_cnt) return (FALSE);

	/* Require one empty slot */
	if (inven_num >= INVEN_PACK) return (FALSE);

	/* Nothing yet */
	borg_temp_n = 0;

	/* Set the searching flag for low level borgs */
	borg_needs_searching = TRUE;

	/* Check distance away from stairs, used later */
	/* Check for an existing "up stairs" */
	for (i = 0; i < track_less_num; i++)
	{
		x = track_less_x[i];
		y = track_less_y[i];

		/* How far is the nearest up stairs */
		j = distance(c_y, c_x, y, x);

		/* skip the closer ones */
		if (b_j >= j) continue;

		/* track it */
		b_j = j;
		b_stair = i;
	}

	/* Scan the object list */
	for (i = 1; i < borg_takes_nxt; i++)
	{
		borg_take *take = &borg_takes[i];

		int a;
		bool item_bad;

		/* Skip dead objects */
		if (!take->k_idx) continue;

		/* Access the location */
		x = take->x;
		y = take->y;

		/* Skip ones that make me wander too far */
		if ((b_stair != -1) && (bp_ptr->lev < 10))
		{
			/* Check the distance of this 'take' to the stair */
			j = distance(track_less_y[b_stair], track_less_x[b_stair], y, x);

			/* skip far away takes while I am close to stair */
			if (b_j <= bp_ptr->lev * 5 + 9 &&
				j >= bp_ptr->lev * 5 + 9) continue;
		}

		/* look to see if this is on the bad items list */
		item_bad = FALSE;
		for (a = 0; a < bad_obj_n; a++)
		{
			if (x == bad_obj_x[a] && y == bad_obj_y[a])
				item_bad = TRUE;
		}

		/* it is a bad item, do not track it */
		if (item_bad) continue;

		/* Bounds checking */
		if (!map_in_bounds(x, y)) continue;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Require line of sight if requested */
		if (viewable && !(mb_ptr->info & BORG_MAP_VIEW)) continue;

		/* Require the item to be on a safe feat */
		if (!borg_on_safe_feat(mb_ptr->feat)) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing to take */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Look for something to take */
	for (i = 0; i < borg_temp_n; i++)
	{
		/* Enqueue the grid */
		borg_flow_enqueue_grid(borg_temp_x[i], borg_temp_y[i]);
	}

	/* Spread the flow */
	/* if we are not flowing toward items that we can see, make sure they */
	/* are at least easily reachable.  The second flag is weather or not  */
	/* to avoid unkown squares.  This was for performance. */
	borg_flow_spread(nearness, (viewable) ? BORG_FLOW_OPTIMIZE
										  : BORG_FLOW_OPTI_AVOID);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("item", GOAL_TAKE)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_TAKE)) return (FALSE);

	/* Success */
	return (TRUE);
}



/*
 * Determine if a grid is "interesting" (and should be explored)
 *
 * A grid is "interesting" if it is a closed door, rubble, hidden treasure,
 * or a visible trap, or an "unknown" grid.
 * or a non-perma-wall adjacent to a perma-wall. (GCV)
 *
 * b_stair is the index to the closest upstairs.
 */
static bool borg_flow_dark_interesting(int x, int y, int b_stair)
{
	int oy;
	int ox, i;

	map_block *mb_ptr;

	/* Hack ignore parameter */
	(void)b_stair;

	/* Have the borg so some Searching */
	borg_needs_searching = TRUE;

#if 0
	/* Skip ones that make me wander too far */
	if ((b_stair != -1) && (bp_ptr->lev < 10))
	{
		/* Check the distance of this grid to the stair */
		j = distance(track_less_y[b_stair], track_less_x[b_stair], y, x);
		/* Distance of me to the stairs */
		b_j = distance(c_y, c_x, track_less_y[b_stair], track_less_x[b_stair]);

		/* skip far away grids while I am close to stair */
		if (b_j <= bp_ptr->lev * 5 + 9 &&
			j >= bp_ptr->lev * 5 + 9) return (FALSE);
	}
#endif /* 0 */

	/* Bounds checking */
	if (!map_in_bounds(x, y)) return (TRUE);

	/* Get the grid */
	mb_ptr = map_loc(x, y);

	/* Explore unknown grids */
	if (!mb_ptr->feat) return (TRUE);

	/* Efficiency -- Ignore "boring" grids */
	if (mb_ptr->feat < FEAT_CLOSED) return (FALSE);

	/* Explore "known treasure" */
	if ((mb_ptr->feat == FEAT_MAGMA_K) || (mb_ptr->feat == FEAT_QUARTZ_K))
	{
		/* Do not disarm when confused */
		if (bp_ptr->status.confused) return (FALSE);

		/* Not when darkened */
		if (!bp_ptr->cur_lite) return (FALSE);

		/* Allow "stone to mud" ability */
		if (borg_spell_legal(REALM_ARCANE, 2, 4) ||
			borg_spell_legal(REALM_NATURE, 1, 0) ||
			borg_spell_legal(REALM_CHAOS, 2, 3) ||
			borg_mutation_check(MUT1_EAT_ROCK, TRUE) ||
			borg_racial_check(RACE_HALF_GIANT, TRUE)) return (TRUE);

		/*
		 * Do not dig unless we appear strong
		 * enough to succeed or we have a digger
		 */
		if (bp_ptr->skill_dig > 40)
		{
			/* digging ought to work */
		}
		else
		{
			return (FALSE);
		}

		/* Okay */
		return (TRUE);
	}

#if 0
	/* "Vaults" Explore non perma-walls adjacent to a perma wall */
	if (mb_ptr->feat == FEAT_WALL_EXTRA || mb_ptr->feat == FEAT_MAGMA ||
		mb_ptr->feat == FEAT_QUARTZ)
	{
		/* Do not attempt when confused */
		if (bp_ptr->status.confused) return (FALSE);

		/* hack and cheat.  No vaults  on this level */
		if (!vault_on_level) return (FALSE);

		/* AJG Do not attempt on the edge */
		if (map_in_bounds(x, y))
		{
			/* scan the adjacent grids */
			for (ox = -1; ox <= 1; ox++)
			{
				for (oy = -1; oy <= 1; oy++)
				{
					/* Bounds checking */
					if (!map_in_bounds(ox + x, oy + y)) continue;

					/* Acquire location */
					mb_ptr = map_loc(ox + x, oy + y);

					/* skip non perma grids wall */
					if (mb_ptr->feat != FEAT_PERM_INNER) continue;

					/* Allow "stone to mud" ability */
					if (borg_spell_legal(REALM_ARCANE, 2, 4) ||
						borg_spell_legal(REALM_NATURE, 1, 0) ||
						borg_spell_legal(REALM_CHAOS, 0, 6) ||
						borg_mutation_check(MUT1_EAT_ROCK) ||
						borg_racial_check(RACE_HALF_GIANT)) return (TRUE);

					/*
					 * Do not dig unless we appear strong
					 * enough to succeed or we have a digger
					 */
					if (bp_ptr->skill_dig > 40)
					{
						/* digging ought to work, proceed */
					}
					else
					{
						continue;
					}
					if (bp_ptr->skill_dig < 40) return (FALSE);

					/* Glove up and dig in */
					return (TRUE);
				}
			}
		}

		/* not adjacent to a GCV,  Restore Grid */
		mb_ptr = map_loc(x, y);
	}
#endif /* 0 */

	/* Explore "rubble" */
	if (mb_ptr->feat == FEAT_RUBBLE)
	{
		return (TRUE);
	}

	/* Explore "Trees" somewhat */
	if (mb_ptr->feat == FEAT_TREES)
	{
		/* Scan near trees for unknown grids */

		/* AJG Do not attempt on the edge */
		if (map_in_bounds(x, y))
		{
			/* scan the adjacent grids */
			for (ox = -1; ox <= 1; ox++)
			{
				for (oy = -1; oy <= 1; oy++)
				{
					/* Bounds checking */
					if (!map_in_bounds(ox + x, oy + y)) continue;

					/* Acquire location */
					mb_ptr = map_loc(ox + x, oy + y);

					/* look for Unknown grid */
					if (!mb_ptr->feat) return (TRUE);
				}
			}
		}

		/* this forest is already explored */
		return (FALSE);
	}


	/* Explore "closed doors" */
	if (mb_ptr->feat == FEAT_CLOSED)
	{
		/* some closed doors leave alone */
		if (breeder_level)
		{
			/* Did I close this one */
			for (i = 0; i < track_door_num; i++)
			{
				/* mark as icky if I closed this one */
				if ((track_door_x[i] == x) && (track_door_y[i] == y))
				{
					/* not interesting */
					return (FALSE);
				}
			}

		}
		/* this door should be ok to open */
		return (TRUE);
	}

	/* Explore "visible traps" */
	if (mb_ptr->trap)
	{
		/* Do not disarm when blind */
		if (bp_ptr->status.blind) return (FALSE);

		/* Do not disarm when confused */
		if (bp_ptr->status.confused) return (FALSE);

		/* Do not disarm when hallucinating */
		if (bp_ptr->status.image) return (FALSE);

		/* Do not flow without lite */
		if (!bp_ptr->cur_lite) return (FALSE);

		/* Do not disarm trap doors on level 99 */
		if (bp_ptr->depth == 99 &&
			mb_ptr->trap == FT_TRAP_DOOR) return (FALSE);

		/* Do not disarm when you could end up dead */
		if (bp_ptr->chp < 60) return (FALSE);

		/* Do not disarm when clumsy */
		if ((bp_ptr->skill_dis < 30) && (bp_ptr->lev < 20)) return (FALSE);
		if ((bp_ptr->skill_dis < 45) && (bp_ptr->lev < 10)) return (FALSE);

		/* NOTE: the flow code allows a borg to flow through a trap and so he may
		 * still try to disarm one on his way to the other interesting grid.  If mods
		 * are made to the above criteria for disarming traps, then mods must also be
		 * made to borg_flow_spread()
		 */

		/* Okay */
		return (TRUE);
	}

	/* Ignore other grids */
	return (FALSE);
}


/*
 * Determine if a grid is "reachable" (and can be explored)
 */
static bool borg_flow_dark_reachable(int x, int y)
{
	int j;

	map_block *mb_ptr;

	/* Scan neighbors */
	for (j = 0; j < 8; j++)
	{
		int y2 = y + ddy_ddd[j];
		int x2 = x + ddx_ddd[j];

		/* Bounds checking */
		if (!map_in_bounds(x2, y2)) continue;

		/* Get the grid */
		mb_ptr = map_loc(x2, y2);


		/* Skip unknown grids (important) */
		if (!mb_ptr->feat) continue;

		/* Accept known floor grids */
		if (borg_cave_floor_grid(mb_ptr)) return (TRUE);

		/* Accept Trees too */
		if (mb_ptr->feat == FEAT_TREES) return (TRUE);
		
		if (borg_on_safe_feat(mb_ptr->feat)) return (TRUE);

		/* I can push pass friendly monsters */
		if (mb_ptr->kill &&
			(borg_kills[mb_ptr->kill].m_flags & (MONST_FRIEND | MONST_PET)))
		{
			return (TRUE);
		}
	}

	/* Failure */
	return (FALSE);
}


/*
 * Place a "direct path" into the flow array, checking danger
 *
 * Modify the "cost" array in such a way that from any point on
 * one "direct" path from the player to the given grid, as long
 * as the rest of the path is "safe" and "clear", the Borg will
 * walk along the path to the given grid.
 *
 * This is used to move around town without looking like a drunk.
 */
void borg_flow_direct(int x, int y)
{
	int n = 0;

	int x1, y1, x2, y2;

	int ay, ax;

	int shift;

	map_block *mb_ptr;

	/* Bounds checking */
	if (!map_in_bounds(x, y)) return;

	mb_ptr = map_loc(x, y);

	/* Avoid icky grids */
	if (mb_ptr->info & BORG_MAP_ICKY) return;

	/* Unknown */
	if (!(mb_ptr->info & BORG_MAP_KNOW))
	{
		/* Mark as known */
		mb_ptr->info |= BORG_MAP_KNOW;

		/* Mark dangerous grids as icky */
		if (borg_danger(x, y, 1, TRUE) > avoidance / 3)
		{
			/* Icky */
			mb_ptr->info |= BORG_MAP_ICKY;


			/* Avoid */
			return;
		}
	}


	/* Save the flow cost (zero) */
	mb_ptr->cost = 1;


	/* Save "origin" */
	y1 = y;
	x1 = x;

	/* Save "destination" */
	y2 = c_y;
	x2 = c_x;

	/* Calculate distance components */
	ay = (y2 < y1) ? (y1 - y2) : (y2 - y1);
	ax = (x2 < x1) ? (x1 - x2) : (x2 - x1);

	/* Path */
	while (1)
	{
		/* Check for arrival at player */
		if ((x == x2) && (y == y2)) return;

		/* Next */
		n++;

		/* Move mostly vertically */
		if (ay > ax)
		{
			/* Extract a shift factor XXX */
			shift = (n * ax + (ay - 1) / 2) / ay;

			/* Sometimes move along the minor axis */
			x = (x2 < x1) ? (x1 - shift) : (x1 + shift);

			/* Always move along major axis */
			y = (y2 < y1) ? (y1 - n) : (y1 + n);
		}

		/* Move mostly horizontally */
		else
		{
			/* Extract a shift factor XXX */
			shift = (n * ay + (ax - 1) / 2) / ax;

			/* Sometimes move along the minor axis */
			y = (y2 < y1) ? (y1 - shift) : (y1 + shift);

			/* Always move along major axis */
			x = (x2 < x1) ? (x1 - n) : (x1 + n);
		}

		/* Bounds checking */
		if (!map_in_bounds(x, y)) return;

		/* Access the grid */
		mb_ptr = map_loc(x, y);

		if (borg_cave_wall_grid(mb_ptr))
		{
			/* Only like 'diggable' things */
			if (!((mb_ptr->feat >= FEAT_CLOSED) &&
				  (mb_ptr->feat <= FEAT_QUARTZ))) return;
		}

		/* Ignore certain "non-wall" grids */
		if (!borg_on_safe_feat(mb_ptr->feat)) return;

		/* Abort at "icky" grids */
		if (mb_ptr->info & BORG_MAP_ICKY) return;

		/* Analyze every grid once */
		if (!(mb_ptr->info & BORG_MAP_KNOW))
		{
			/* Mark as known */
			mb_ptr->info |= BORG_MAP_KNOW;

			/* Avoid dangerous grids (forever) */
			if (borg_danger(x, y, 1, TRUE) > avoidance / 3)
			{
				/* Mark as icky */
				mb_ptr->info |= BORG_MAP_ICKY;

				/* Abort */
				return;
			}
		}

		/* Abort "pointless" paths if possible */
		if (mb_ptr->cost <= n) break;

		/* Save the new flow cost */
		mb_ptr->cost = n;
	}

}

/*
 * Hack -- mark off the edges of a rectangle as "avoid" or "clear"
 */
static void borg_flow_border(int x1, int y1, int x2, int y2, bool stop)
{
	int x, y;

	map_block *mb_ptr;

	if (stop)
	{
		/* Scan west/east edges */
		for (y = y1; y <= y2; y++)
		{
			/* Avoid/Clear west edge */
			if (!map_in_bounds(x1, y)) continue;
			mb_ptr = map_loc(x1, y);
			mb_ptr->info |= (BORG_MAP_ICKY | BORG_MAP_KNOW);

			/* Avoid/Clear east edge */
			if (!map_in_bounds(x2, y)) continue;
			mb_ptr = map_loc(x2, y);
			mb_ptr->info |= (BORG_MAP_ICKY | BORG_MAP_KNOW);
		}

		/* Scan north/south edges */
		for (x = x1; x <= x2; x++)
		{
			/* Avoid/Clear north edge */
			if (!map_in_bounds(x, y1)) continue;
			mb_ptr = map_loc(x, y1);
			mb_ptr->info |= (BORG_MAP_ICKY | BORG_MAP_KNOW);

			/* Avoid/Clear south edge */
			if (!map_in_bounds(x, y2)) continue;
			mb_ptr = map_loc(x, y2);
			mb_ptr->info |= (BORG_MAP_ICKY | BORG_MAP_KNOW);
		}
	}
	else
	{
		/* Scan west/east edges */
		for (y = y1; y <= y2; y++)
		{
			/* Avoid/Clear west edge */
			if (!map_in_bounds(x1, y)) continue;
			mb_ptr = map_loc(x1, y);
			mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);

			/* Avoid/Clear east edge */
			if (!map_in_bounds(x2, y)) continue;
			mb_ptr = map_loc(x2, y);
			mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);
		}

		/* Scan north/south edges */
		for (x = x1; x <= x2; x++)
		{
			/* Avoid/Clear north edge */
			if (!map_in_bounds(x, y1)) continue;
			mb_ptr = map_loc(x, y1);
			mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);

			/* Avoid/Clear south edge */
			if (!map_in_bounds(x, y2)) continue;
			mb_ptr = map_loc(x, y2);
			mb_ptr->info &= ~(BORG_MAP_ICKY | BORG_MAP_KNOW);
		}
	}
}


/*
 * Prepare to "flow" towards "interesting" grids (method 2)
 *
 * This function is only used when the player is at least 4 grids away
 * from the outer dungeon wall, to prevent any nasty memory errors.
 *
 * This function examines the grids just outside the torch-lit grids
 * for "unknown" grids, and flows directly towards them (one step).
 */
static bool borg_flow_dark_2(void)
{
	int i, r;

	int x, y;

	map_block *mb_ptr;

	/* Hack -- not in town */
	if (!bp_ptr->depth) return (FALSE);

	/* Set the searching flag for low level borgs */
	borg_needs_searching = TRUE;

	/* Maximal radius */
	r = bp_ptr->cur_lite + 1;


	/* Reset */
	borg_temp_n = 0;

	/* Four directions */
	for (i = 0; i < 4; i++)
	{
		y = c_y + ddy_ddd[i] * r;
		x = c_x + ddx_ddd[i] * r;

		/* Check legality */
		if (!map_in_bounds(x, y)) continue;

		/* Acquire grid */
		mb_ptr = map_loc(x, y);

		/* Require unknown */
		if (mb_ptr->feat) continue;

		/* Require viewable */
		if (!(mb_ptr->info & BORG_MAP_VIEW)) continue;

		/* if it makes me wander, skip it */

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}

	/* Nothing */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Create paths to useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];
#if 0
		/* Create a path */
		borg_flow_direct(x, y);
#endif /* 0 */
		borg_flow_enqueue_grid(x, y);
	}

	/* Spread the flow */
	borg_flow_spread(5, BORG_FLOW_OPTIMIZE);


	/* Attempt to Commit the flow */
	/* Note was NULL */
	if (!borg_flow_commit("dark-2", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Forget goal */
	goal = GOAL_NONE;

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 3)
 *
 * Note the use of a limit on the "depth" of the flow, and of the flag
 * which avoids "unknown" grids when calculating the flow, both of which
 * help optimize this function to only handle "easily reachable" grids.
 *
 * The "borg_temp" array is much larger than any "local region".
 */
static bool borg_flow_dark_3(int b_stair)
{
	int i;

	int x, y;

	int x1, y1, x2, y2;


	/* Hack -- not in town */
	if (!bp_ptr->depth) return (FALSE);


	/* Local region */
	y1 = c_y - 4;
	x1 = c_x - 4;
	y2 = c_y + 4;
	x2 = c_x + 4;

	/* Reset */
	borg_temp_n = 0;

	/* Examine the region */
	for (y = y1; y <= y2; y++)
	{
		/* Examine the region */
		for (x = x1; x <= x2; x++)
		{
			if (!map_in_bounds(x, y)) continue;

			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(x, y, b_stair)) continue;

			/* Skip "unreachable" grids */
			if (!borg_flow_dark_reachable(x, y)) continue;

			/* Careful -- Remember it */
			borg_temp_x[borg_temp_n] = x;
			borg_temp_y[borg_temp_n] = y;
			borg_temp_n++;
		}
	}

	/* Nothing interesting */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(x, y);
	}

	/* Spread the flow (limit depth) */
	borg_flow_spread(5, BORG_FLOW_OPTI_AVOID);


	/* Attempt to Commit the flow */
	/* Note was NULL */
	if (!borg_flow_commit("dark-3", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 4)
 *
 * Note that we avoid grids close to the edge of the panel, since they
 * induce panel scrolling, which is "expensive" in terms of CPU usage,
 * and because this allows us to "expand" the border by several grids
 * to lay down the "avoidance" border in known legal grids.
 *
 * We avoid paths that would take us into different panels by setting
 * the "icky" flag for the "border" grids to prevent path construction,
 * and then clearing them when done, to prevent confusion elsewhere.
 *
 * The "borg_temp" array is large enough to hold one panel full of grids.
 */
static bool borg_flow_dark_4(int b_stair)
{
	int i, x, y;

	int x1, y1, x2, y2;


	/* Hack -- not in town */
	if (!bp_ptr->depth) return (FALSE);


	/* Local region */
	y1 = c_y - 11;
	x1 = c_x - 11;
	y2 = c_y + 11;
	x2 = c_x + 11;

	/* Nothing yet */
	borg_temp_n = 0;

	/* Examine the panel */
	for (y = y1; y <= y2; y++)
	{
		/* Examine the panel */
		for (x = x1; x <= x2; x++)
		{
			if (!map_in_bounds(x, y)) continue;

			/* Skip "boring" grids */
			if (!borg_flow_dark_interesting(x, y, b_stair)) continue;

			/* Skip "unreachable" grids */
			if (!borg_flow_dark_reachable(x, y)) continue;

			/* Careful -- Remember it */
			borg_temp_x[borg_temp_n] = x;
			borg_temp_y[borg_temp_n] = y;
			borg_temp_n++;
		}
	}

	/* Nothing useful */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(x, y);
	}


	/* Expand borders */
	y1--;
	x1--;
	y2++;
	x2++;

	/* Avoid the edges */
	borg_flow_border(x1, y1, x2, y2, TRUE);

	/* Spread the flow (limit depth) */
	borg_flow_spread(32, BORG_FLOW_OPTI_AVOID);

	/* Clear the edges */
	borg_flow_border(x1, y1, x2, y2, FALSE);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit("dark-4", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Prepare to "flow" towards "interesting" grids (method 5)
 */
static bool borg_flow_dark_5(int b_stair)
{
	int i, x, y;

	map_block *mb_ptr;

	/* Hack -- not in town */
	if (!bp_ptr->depth) return (FALSE);

	/* Nothing yet */
	borg_temp_n = 0;

	/* Examine every "legal" grid */
	MAP_ITT_START (mb_ptr)
	{
		/* Paranoia -- Check for overflow */
		if (borg_temp_n == BORG_TEMP_MAX) continue;

		/* Get location */
		MAP_GET_LOC(x, y);

		/* Skip "boring" grids */
		if (!borg_flow_dark_interesting(x, y, b_stair)) continue;

		/* Skip "unreachable" grids */
		if (!borg_flow_dark_reachable(x, y)) continue;

		/* Careful -- Remember it */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;
	}
	MAP_ITT_END;

	/* Nothing useful */
	if (!borg_temp_n) return (FALSE);


	/* Clear the flow codes */
	borg_flow_clear();

	/* Enqueue useful grids */
	for (i = 0; i < borg_temp_n; i++)
	{
		y = borg_temp_y[i];
		x = borg_temp_x[i];

		/* Enqueue the grid */
		borg_flow_enqueue_grid(x, y);
	}

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTI_AVOID);


	/* Attempt to Commit the flow */
	if (!borg_flow_commit("dark-5", GOAL_DARK)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_DARK)) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Is this an acceptaple place to flow to?
 * Should be expanded so the borg can avoid nasty quests and dungeons
 */
static bool borg_flow_wild_check(int x, int y)
{
	/* Is the x in bounds? */
	if (x < 0 || x > max_wild - 2) return (FALSE);

	/* Is the y in bounds? */
	if (y < 0 || y > max_wild - 2) return (FALSE);

	/* Hack!  Check this location */
	return (!(wild[y][x].done.info & WILD_INFO_SEEN));
}


/* Prepare to flow somewhere in the wilderness */
bool borg_flow_dark_wild(void)
{
	int x, y, side = 0;
	int loop_min, loop_max;

	int base_x = c_x / WILD_BLOCK_SIZE;
	int base_y = c_y / WILD_BLOCK_SIZE;

	bool found = FALSE;

	/* Is the borg in the wilderness? */
	if (bp_ptr->depth || vanilla_town) return (FALSE);

	/* No exploring in the dark */
	if (bp_ptr->hour < 6 || bp_ptr->hour > 17) return (FALSE);

	/* Does the borg already know where to go but it isn't there? */
	if (goal == GOAL_DARK)
	{
		x = goal_explore_x;
		y = goal_explore_y;
	}
	else
	{
		/*
		 * Try to find a dark spot on the overhead map.
		 * The method is to draw an everwidening square around the current location
		 * and check the locations on the sides of the square if they are explored.
		 * To keep this search as quick as possible there is some trickery to
		 * ensure that every spot on the map in checked only once.
		 */
		while (!found)
		{
			/* Enlarge the box */
			side += 1;

			/* Has the borg explored the whole map? */
			if (base_x - side < 0 &&
				base_y - side < 0 &&
				base_x + side > max_wild - 2 &&
				base_y + side > max_wild - 2) return (FALSE);

			/* The upper side has a constant y */
			y = base_y - side;

			/* If the y is on the map */
			if (y >= 0)
			{
				/* Don't go out of bounds */
				loop_min = MAX(0, base_x - side);
				loop_max = MIN(base_x + side, max_wild - 2);

				/* Check the upper side from left to right */
				for (x = loop_min; x < loop_max && !found; x++)
				{
					found = borg_flow_wild_check(x, y);
					if (found) break;
				}
			}

			/* Step out */
			if (found) break;

			/* The right side has a constant x */
			x = base_x + side;

			/* If the x is on the map */
			if (x < max_wild - 1)
			{
				/* Don't go out of bounds */
				loop_min = MAX(0, base_y - side);
				loop_max = MIN(base_y + side, max_wild - 2);

				/* Check the right side from up to down */
				for (y = loop_min; y < loop_max && !found; y++)
				{
					found = borg_flow_wild_check(x, y);
					if (found) break;
				}
			}

			/* Step out */
			if (found) break;

			/* The lower side has a constant y */
			y = base_y + side;

			/* If the y is on the map */
			if (y < max_wild - 1)
			{
				/* Don't go out of bounds */
				loop_min = MAX(0, base_x - side);
				loop_max = MIN(base_x + side, max_wild - 2);

				/* Check the lower side from right to left */
				for (x = loop_max; x > loop_min && !found; x--)
				{
					found = borg_flow_wild_check(x, y);
					if (found) break;
				}
			}

			/* Step out */
			if (found) break;

			/* The left side has a constant x */
			x = base_x - side;

			/* If the x is on the map */
			if (x >= 0)
			{
				/* Don't go out of bounds */
				loop_min = MAX(0, base_y - side);
				loop_max = MIN(base_y + side, max_wild - 2);

				/* Check the left side from down to up */
				for (y = loop_max; y > loop_min && !found; y--)
				{
					found = borg_flow_wild_check(x, y);
					if (found) break;
				}
			}
		}

		/* Failure */
		if (!found) return (FALSE);

		/* Close by? */
		if (ABS(base_x - x) < 5)
		{
			/* keep this c_x */
			x = c_x;
		}
		/* Not close */
		else
		{
			/* West? */
			if (x < base_x)
			{
				/* Stay at a distance from the known/unknown edge */
				x = x + 4;

				/* One foot into the new block */
				x = WILD_BLOCK_SIZE * (x + 1) - 1;
			}
			/* East */
			else
			{
				/* Stay at a distance from the known/unknown edge */
				x = x - 4;

				/* One foot into the new block */
				x = WILD_BLOCK_SIZE * x;
			}
		}

		/* Close by? */
		if (ABS(base_y - y) < 5)
		{
			/* keep this c_y */
			y = c_y;
		}
		/* Not close */
		else
		{
			/* North? */
			if (y < base_y)
			{
				/* Stay at a distance from the known/unknown edge */
				y = y + 4;

				/* One foot into the new block */
				y = WILD_BLOCK_SIZE * (y + 1) - 1;
			}
			/* South */
			else
			{
				/* Stay at a distance from the known/unknown edge */
				y = y - 4;

				/* One foot into the new block */
				y = WILD_BLOCK_SIZE * y;
			}
		}
	}

	/* Enqueue the grid */
	if (borg_flow_block(x, y, "unexplored wilderness", GOAL_DARK))
	{
		/* Flag the global */
		goal_explore_x = x;
		goal_explore_y = y;

		return (TRUE);
	}

	goal = GOAL_NONE;
	goal_explore_x = -1;
	goal_explore_y = -1;

	/* This flow is not possible */
	return (FALSE);
}


/* Reset the flow in the wilderness, based on a goal */
void borg_flow_goal_wild(void)
{
	int x, y;
	cptr reason;

	if (bp_ptr->depth) return;

	switch(goal)
	{
		case GOAL_SHOP:
		{
			x = borg_shops[goal_shop].x;
			y = borg_shops[goal_shop].y;

			reason = "reflowing to a shop";

			break;
		}
		case GOAL_DARK:
		{
			x = goal_explore_x;
			y = goal_explore_y;

			reason = "reflowing the dark";

			break;
		}
		case GOAL_TOWN:
		{
			x = borg_towns[goal_town].x;
			y = borg_towns[goal_town].y;

			reason = "reflowing to a town";

			break;
		}
		case GOAL_CAVE:
		{
			x = borg_dungeons[goal_dungeon].x;
			y = borg_dungeons[goal_dungeon].y;

			reason = "reflowing to a dungeon";

			break;
		}
		default: return;
	}

	(void)borg_flow_block(x, y, reason, goal);
}


/*
 * Prepare to "flow" towards "interesting" grids
 *
 * The "exploration" routines are broken into "near" and "far"
 * exploration, and each set is chosen via the flag below.
 */
bool borg_flow_dark(bool close)
{
	int i;
	int x, y, j, b_j = -1;
	int b_stair = -1;

	/* Paranoia */
	if (borg_flow_dark_interesting(c_x, c_y, -1))
	{
		return (FALSE);
	}

	/* Check distance away from stairs, used later */
	/* Check for an existing "up stairs" */
	for (i = 0; i < track_less_num; i++)
	{
		x = track_less_x[i];
		y = track_less_y[i];

		/* How far is the nearest up stairs */
		j = distance(c_y, c_x, y, x);

		/* skip the closer ones */
		if (b_j >= j) continue;

		/* track it */
		b_j = j;
		b_stair = i;
	}

	/* Near */
	if (close)
	{
		/* Method 2 */
		if (borg_flow_dark_2()) return (TRUE);

		/* Method 3 */
		if (borg_flow_dark_3(b_stair)) return (TRUE);
	}

	/* Far */
	else
	{
		/* Method 4 */
		if (borg_flow_dark_4(b_stair)) return (TRUE);

		/* Method 5 */
		if (borg_flow_dark_5(b_stair)) return (TRUE);
	}

	/* Fail */
	return (FALSE);
}


/*
 * Hack -- spastic searching
 */

static s16b spastic_x;
static s16b spastic_y;



/*
 * Search carefully for secret doors and such
 */
bool borg_flow_spastic(bool bored)
{
	int cost;

	int i, x, y, v;

	int b_x = c_x;
	int b_y = c_y;
	int b_v = -1;
	int j, b_j = 1;
	int b_stair = -1;

	map_block *mb_ptr;

	/* Hack -- not in town */
	if (!bp_ptr->depth) return (FALSE);


	/* Not bored */
	if (!bored)
	{
		/* Look around for danger */
		int p = borg_danger(c_x, c_y, 1, TRUE);

		/* Avoid searching when in danger */
		if (p > avoidance / 4) return (FALSE);
	}

	/* Check distance away from stairs, used later */
	/* Check for an existing "up stairs" */
	for (i = 0; i < track_less_num; i++)
	{
		x = track_less_x[i];
		y = track_less_y[i];

		/* How far is the nearest up stairs */
		j = distance(c_y, c_x, y, x);

		/* skip the closer ones */
		if (b_j >= j) continue;

		/* track it */
		b_j = j;
		b_stair = i;
	}

	/* We have arrived */
	if ((spastic_x == c_x) && (spastic_y == c_y))
	{
		/* Cancel */
		spastic_x = 0;
		spastic_y = 0;

		/* Take note */
		borg_note("# Spastic Searching at (%d,%d)...", c_x, c_y);

		/* Count searching */
		for (i = 0; i < 9; i++)
		{
			/* Extract the location */
			int xx = c_x + ddx_ddd[i];
			int yy = c_y + ddy_ddd[i];

			/* Bounds checking */
			if (!map_in_bounds(xx, yy)) continue;

			/* Current grid */
			mb_ptr = map_loc(xx, yy);

			/* Tweak -- Remember the search */
			if (mb_ptr->xtra < 100) mb_ptr->xtra += 5;
		}

		/* Tweak -- Search a little */
		borg_keypress('0');
		borg_keypress('5');
		borg_keypress('s');

		/* Success */
		return (TRUE);
	}


	/* Reverse flow */
	borg_flow_reverse();

	/* Scan the entire map */
	MAP_ITT_START (mb_ptr)
	{
		map_block *mb_array[8];

		int wall = 0;
		int supp = 0;
		int diag = 0;

		byte xtra_val;

		/* Skip unknown grids */
		if (!mb_ptr->feat) continue;

		/* Skip walls/doors */
		if (borg_cave_wall_grid(mb_ptr)) continue;

		/* Acquire the cost */
		cost = mb_ptr->cost;

		/* Skip "unreachable" grids */
		if (cost >= 250) continue;

		xtra_val = mb_ptr->xtra;

		/* Tweak -- Limit total searches */
		if (xtra_val >= 50) continue;
		if (xtra_val >= bp_ptr->lev * 5) continue;

		/* Limit initial searches until bored */
		if (!bored && (xtra_val > 5)) continue;

		/* Acquire the location */
		MAP_GET_LOC(x, y);

		/* Avoid searching detected sectors */
		if (mb_ptr->detect & BORG_DETECT_DOOR) continue;

		/* Skip ones that make me wander too far */
		if ((b_stair != -1) && (bp_ptr->lev < 10))
		{
			/* Check the distance of this grid to the stair */
			j = distance(track_less_y[b_stair], track_less_x[b_stair], y, x);
			/* Distance of me to the stairs */
			b_j =
				distance(c_y, c_x, track_less_y[b_stair],
						 track_less_x[b_stair]);

			/* skip far away grids while I am close to stair */
			if (b_j <= bp_ptr->lev * 5 + 9 &&
				j >= bp_ptr->lev * 5 + 9) continue;
		}

		/* Extract adjacent locations */
		for (i = 0; i < 8; i++)
		{
			/* Extract the location */
			int xx = x + ddx_ddd[i];
			int yy = y + ddy_ddd[i];

			/* Bounds checking */
			if (map_in_bounds(xx, yy))
			{
				/* Get the grid contents */
				mb_array[i] = map_loc(xx, yy);
			}
			else
			{
				mb_array[i] = NULL;
			}
		}


		/* Count possible door locations */
		for (i = 0; i < 4; i++)
		{
			mb_ptr = mb_array[i];
			if (mb_ptr && mb_ptr->feat >= FEAT_WALL_EXTRA) wall++;
		}

		/* No possible secret doors */
		if (wall < 1) continue;


		/* Count supporting evidence for secret doors */
		for (i = 0; i < 4; i++)
		{
			mb_ptr = mb_array[i];

			/* Rubble */
			if (!mb_ptr || mb_ptr->feat == FEAT_RUBBLE) continue;

			/* Walls, Doors */
			if (((mb_ptr->feat >= FEAT_SECRET) &&
				 (mb_ptr->feat <= FEAT_PERM_SOLID)) ||
				((mb_ptr->feat == FEAT_OPEN) ||
				 (mb_ptr->feat == FEAT_BROKEN)) ||
				(mb_ptr->feat == FEAT_CLOSED))
			{
				supp++;
			}
		}

		/* Count supporting evidence for secret doors */
		for (i = 4; i < 8; i++)
		{
			mb_ptr = mb_array[i];

			/* Rubble */
			if (!mb_ptr || mb_ptr->feat == FEAT_RUBBLE) continue;

			/* Walls */
			if (mb_ptr->feat >= FEAT_SECRET)
			{
				diag++;
			}
		}

		/* No possible secret doors */
		if (diag < 2) continue;

		/* Tweak -- Reward walls, punish visitation and distance */
		v = (supp * 500) + (diag * 100) - (xtra_val * 20) - (cost * 1);

		/* The grid is not searchable */
		if (v <= 0) continue;


		/* Tweak -- Minimal interest until bored */
		if (!bored && (v < 1500)) continue;


		/* Track "best" grid */
		if ((b_v >= 0) && (v < b_v)) continue;

		/* Save the data */
		b_v = v;
		b_x = x;
		b_y = y;

	}
	MAP_ITT_END;

	/* Clear the flow codes */
	borg_flow_clear();

	/* Hack -- Nothing found */
	if (b_v < 0) return (FALSE);

	/* Memorize */
	spastic_x = b_x;
	spastic_y = b_y;


	/* Enqueue the grid */
	borg_flow_enqueue_grid(b_x, b_y);

	/* Spread the flow */
	borg_flow_spread(250, BORG_FLOW_OPTIMIZE);

	/* Attempt to Commit the flow */
	if (!borg_flow_commit("spastic", GOAL_XTRA)) return (FALSE);

	/* Take one step */
	if (!borg_flow_old(GOAL_XTRA)) return (FALSE);

	/* Success */
	return (TRUE);
}


/* Clear some bools that are used in the wilderness */
void borg_leave_surface(void)
{
	int i;

	/* Only valid on the surface */
	if (bp_ptr->depth) return;

	/* Clear town visits */
	for (i = 0; i < borg_town_num; i++) borg_towns[i].visit = FALSE;

	/* Clear shop visits */
	for (i = 0; i < borg_shop_num; i++) borg_shops[i].visit = FALSE;

	/* Into which dungeon? */
	borg_dungeon_remember(TRUE);
}




/*
 * Initialize this file
 */
void borg_init_6(void)
{
	/* Nothing */
}

#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
