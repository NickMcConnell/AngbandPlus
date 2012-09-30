/* File: zbmagic1.c */
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

bool borg_desperate = FALSE;



/*
 * This file is responsible for the low level dungeon goals.
 *
 * This includes calculating the danger from monsters, determining
 * how and when to attack monsters, and calculating "flow" paths
 * from place to place for various reasons.
 *
 * Notes:
 *   We assume that invisible/offscreen monsters are dangerous
 *   We consider physical attacks, missile attacks, spell attacks,
 *     wand attacks, etc, as variations on a single theme.
 *   We take account of monster resistances and susceptibilities
 *   We try not to wake up sleeping monsters by throwing things
 *
 *
 * Bugs:
 *   Currently the "twitchy()" function is not very smart
 *   We get "twitchy" when we are afraid of the monsters
 *   Annoyance and Danger are very different things (!)
 */



/*
 * Given a "source" and "target" locations, extract a "direction",
 * which will move one step from the "source" towards the "target".
 *
 * We prefer "non-diagonal" motion, which allows us to save the
 * "diagonal" moves for avoiding pillars and other obstacles.
 *
 * If no "obvious" path is available, we use "borg_extract_dir()".
 *
 * We return "5" if no motion is needed.
 */
int borg_goto_dir(int x1, int y1, int x2, int y2)
{
	int d, e;

	int ay = (y2 > y1) ? (y2 - y1) : (y1 - y2);
	int ax = (x2 > x1) ? (x2 - x1) : (x1 - x2);

	map_block *mb_ptr;


	/* Default direction */
	e = borg_extract_dir(x1, y1, x2, y2);


	/* Adjacent location, use default */
	if ((ay <= 1) && (ay <= 1)) return (e);


	/* Try south/north (primary) */
	if (ay > ax)
	{
		d = (y1 < y2) ? 2 : 8;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}

	/* Try east/west (primary) */
	if (ay < ax)
	{
		d = (x1 < x2) ? 6 : 4;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}


	/* Try diagonal */
	d = borg_extract_dir(x1, y1, x2, y2);

	/* Check for walls */

	/* Bounds checking */
	if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
	{
		mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

		if (borg_cave_floor_grid(mb_ptr)) return (d);
	}

	/* Try south/north (secondary) */
	if (ay <= ax)
	{
		d = (y1 < y2) ? 2 : 8;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}

	/* Try east/west (secondary) */
	if (ay >= ax)
	{
		d = (x1 < x2) ? 6 : 4;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}


	/* Circle obstacles */
	if (!ay)
	{
		/* Circle to the south */
		d = (x1 < x2) ? 3 : 1;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}

		/* Circle to the north */
		d = (x1 < x2) ? 9 : 7;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}

	/* Circle obstacles */
	if (!ax)
	{
		/* Circle to the east */
		d = (y1 < y2) ? 3 : 9;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}

		/* Circle to the west */
		d = (y1 < y2) ? 1 : 7;

		/* Bounds checking */
		if (map_in_bounds(x1 + ddx[d], y1 + ddy[d]))
		{
			mb_ptr = map_loc(x1 + ddx[d], y1 + ddy[d]);

			if (borg_cave_floor_grid(mb_ptr)) return (d);
		}
	}


	/* Oops */
	return (e);
}


/*
 * Attempt to induce "word of recall"
 * artifact activations added throughout this code
 */
bool borg_recall(void)
{
	int wid, hgt;

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* Is the borg somewhere in the wilderness? */
	if (borg_term_text_comp(wid - T_NAME_LEN, hgt - 1, "Wilderness"))
		return (FALSE);

	/* Multiple "recall" fails */
	if (!goal_recalling)
	{
		/* Press an ESC to try to avoid the take-off loop */
		borg_keypress(ESCAPE);

		/* Try to "recall" */
		if (borg_zap_rod(SV_ROD_RECALL) ||
			borg_activate(BORG_ACT_WORD_OF_RECALL) ||
			borg_spell_fail(REALM_SORCERY, 2, 7, 60) ||
			borg_spell_fail(REALM_ARCANE, 3, 6, 60) ||
			borg_spell_fail(REALM_TRUMP, 1, 6, 60) ||
			borg_mutation(MUT1_RECALL) ||
			borg_read_scroll(SV_SCROLL_WORD_OF_RECALL))
		{
			/* Always try to cancel the reset recall. */
			borg_keypress(ESCAPE);

			/* Success */
			return (TRUE);
		}
	}

	/* Nothing */
	return (FALSE);
}



/*
 * Prevent starvation by any means possible
 */
bool borg_eat_food_any(void)
{
	int i;

	list_item *l_ptr;
	object_kind *k_ptr;

	/* No help for some */
	if (FLAG(bp_ptr, TR_CANT_EAT)) return (FALSE);

	/* Scan the inventory for "normal" food */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Skip empty / unknown items */
		if (!l_ptr->k_idx) continue;

		/* Skip non-food */
		if (l_ptr->tval != TV_FOOD) continue;

		/* Get kind */
		k_ptr = &k_info[l_ptr->k_idx];

		/* Skip "flavored" food */
		if (k_ptr->sval < SV_FOOD_MIN_FOOD) continue;

		/* Eat something of that type */
		if (borg_eat_food(k_ptr->sval)) return (TRUE);
	}

	/* Scan the inventory for "okay" food */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Skip empty / unknown items */
		if (!l_ptr->k_idx) continue;

		/* Skip non-food */
		if (l_ptr->tval != TV_FOOD) continue;

		/* Get kind */
		k_ptr = &k_info[l_ptr->k_idx];

		/* Skip "icky" food */
		if (k_ptr->sval < SV_FOOD_MIN_FOOD) continue;

		/* Eat something of that type */
		if (borg_eat_food(k_ptr->sval)) return (TRUE);
	}

	/* Scan the inventory for "potions" food */
	for (i = 0; i < inven_num; i++)
	{
		l_ptr = &inventory[i];

		/* Skip empty / unknown items */
		if (!l_ptr->k_idx) continue;

		/* Skip non-potion */
		if (l_ptr->tval != TV_POTION) continue;

		/* Consume in order, when hurting */
		if (bp_ptr->chp < 4 &&
			(borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			 borg_use_staff(SV_STAFF_CURE_LIGHT) ||
			 borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			 borg_quaff_crit(TRUE) ||
			 borg_use_staff(SV_STAFF_CURING) ||
			 borg_quaff_potion(SV_POTION_RESTORE_MANA) ||
			 borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_LIFE)))
		{
			return (TRUE);
		}
	}

	/* Nothing */
	return (FALSE);
}

/*
 * Hack -- evaluate the likelihood of the borg getting surrounded
 * by a bunch of monsters.  This is called from borg_danger() when
 * he looking for a strategic retreat.  It is hopeful that the borg
 * will see that several monsters are approaching him and he may
 * become surrouned then die.  This routine looks at near by monsters
 * as determines the likelyhood of him getting surrouned.
 */
bool borg_surrounded(void)
{
	borg_kill *kill;
	monster_race *r_ptr;

	int safe_grids = 8;
	int non_safe_grids = 0;
	int monsters = 0;
	int adjacent_monsters = 0;

	int x9, y9, ax, ay, d;
	int i;

	/* Evaluate the local monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];
		r_ptr = &r_info[kill->r_idx];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		x9 = kill->x;
		y9 = kill->y;

		/* Distance components */
		ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
		ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

		/* Distance */
		d = MAX(ax, ay);

		/* if the monster is too far then skip it. */
		if (d > 3) continue;

		/* if he cant see me then forget it. */
		if (!borg_los(c_x, c_y, x9, y9)) continue;

		/* if asleep, don't consider this one */
		if (kill->m_flags & MONST_ASLEEP) continue;

		/* Monsters with Pass Wall are dangerous, no escape from them */
		if (FLAG(r_ptr, RF_PASS_WALL)) continue;
		if (FLAG(r_ptr, RF_KILL_WALL)) continue;

		/* Monsters who never move cant surround */
		if (FLAG(r_ptr, RF_NEVER_MOVE)) continue;

		/* keep track of monsters touching me */
		if (d == 1) adjacent_monsters++;

		/* Add them up. */
		monsters++;

	}

	/* Evaluate the Non Safe Grids, (walls, closed doors, traps, monsters) */
	for (i = 0; i < 8; i++)
	{
		int x = c_x + ddx_ddd[i];
		int y = c_y + ddy_ddd[i];

		map_block *mb_ptr;

		if (!map_in_bounds(x, y)) continue;

		/* Access the grid */
		mb_ptr = map_loc(x, y);

		/* Non Safe grids */
		if (borg_cave_wall_grid(mb_ptr)) non_safe_grids++;

		/* Skip unknown grids */
		if (!mb_ptr->feat) non_safe_grids++;

		/* Skip monster grids */
		if (mb_ptr->monster) non_safe_grids++;

		/* MT - Skip trap grids */
		if (mb_ptr->trap) non_safe_grids++;
	}

	/* Safe grids are decreased */
	safe_grids = safe_grids - non_safe_grids;

	/* Am I in hallway? If so don't worry about it */
	if (safe_grids == 1 && adjacent_monsters == 1) return (FALSE);

	/* I am likely to get surrouned */
	if (monsters > safe_grids)
	{
		borg_note("# Possibility of being surrounded (%d/%d)",
					  monsters, safe_grids);

		/* The borg can get trapped by breeders by continueing to flee
		 * into a dead-end.  So he needs to be able to trump this
		 * routine.
		 */
		if (goal_ignoring) return (FALSE);
		else
			return (TRUE);
	}

	/* Probably will not be surrouned */
	return (FALSE);
}

/*
 * Mega-Hack -- evaluate the "freedom" of the given location
 *
 * The theory is that often, two grids will have equal "danger",
 * but one will be "safer" than the other, perhaps because it
 * is closer to stairs, or because it is in a corridor, or has
 * some other characteristic that makes it "safer".
 *
 * Then, if the Borg is in danger, say, from a normal speed monster
 * which is standing next to him, he will know that walking away from
 * the monster is "pointless", because the monster will follow him,
 * but if the resulting location is "safer" for some reason, then
 * he will consider it.  This will allow him to flee towards stairs
 * in the town, and perhaps towards corridors in the dungeon.
 *
 * This method is used in town to chase the stairs.
 *
 * XXX XXX XXX We should attempt to walk "around" buildings.
 */
int borg_freedom(int x, int y)
{
	int d, f = 0;

	/* Hack -- chase down stairs in town */
	if (!bp_ptr->depth && track_more_num)
	{
		/* Love the stairs! */
		d = double_distance(y, x, track_more_y[0], track_more_x[0]);

		/* Proximity is good */
		f += (1000 - d);

		/* Close proximity is great */
		if (d < 4) f += (2000 - (d * 500));
	}

	/* Hack -- chase Up Stairs in dungeon */
	if (bp_ptr->depth && track_less_num)
	{
		/* Love the stairs! */
		d = double_distance(y, x, track_less_y[0], track_less_x[0]);

		/* Proximity is good */
		f += (1000 - d);

		/* Close proximity is great */
		if (d < 4) f += (2000 - (d * 500));
	}

	/* Freedom */
	return (f);
}


/*
 * Check a floor grid for "happy" status
 *
 * These grids are floor grids which contain stairs, or which
 * are non-corners in corridors, or which are directly adjacent
 * to pillars, or grids which we have stepped on before.
 *  Stairs are good because they can be used to leave
 * the level.  Corridors are good because you can back into them
 * to avoid groups of monsters and because they can be used for
 * escaping.  Pillars are good because while standing next to a
 * pillar, you can walk "around" it in two different directions,
 * allowing you to retreat from a single normal monster forever.
 * Stepped on grids are good because they likely stem from an area
 * which has been cleared of monsters.
 */
bool borg_happy_grid_bold(int x, int y)
{
	int i;

	map_block *mb_ptr;

	/* Bounds checking */
	if (!map_in_bounds(x, y)) return (FALSE);

	mb_ptr = map_loc(x, y);


	/* Accept stairs */
	if (mb_ptr->feat == FEAT_LESS) return (TRUE);
	if (mb_ptr->feat == FEAT_MORE) return (TRUE);

	/* Accept Glyphs */
	if (mb_ptr->m_effect) return (TRUE);

	/* Hack -- weak/dark is very unhappy */
	if (bp_ptr->status.weak || !bp_ptr->cur_lite) return (FALSE);

	/* Apply a control effect so that he does not get stuck in a loop */
	if ((borg_t - borg_began) >= 2000) return (FALSE);

	/* Case 1a: north-south corridor */
	if (borg_cave_floor_bold(y - 1, x) && borg_cave_floor_bold(y + 1, x) &&
		!borg_cave_floor_bold(y, x - 1) && !borg_cave_floor_bold(y, x + 1) &&
		!borg_cave_floor_bold(y + 1, x - 1) &&
		!borg_cave_floor_bold(y + 1, x + 1) &&
		!borg_cave_floor_bold(y - 1, x - 1) &&
		!borg_cave_floor_bold(y - 1, x + 1))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 1b: east-west corridor */
	if (borg_cave_floor_bold(y, x - 1) && borg_cave_floor_bold(y, x + 1) &&
		!borg_cave_floor_bold(y - 1, x) && !borg_cave_floor_bold(y + 1, x) &&
		!borg_cave_floor_bold(y + 1, x - 1) &&
		!borg_cave_floor_bold(y + 1, x + 1) &&
		!borg_cave_floor_bold(y - 1, x - 1) &&
		!borg_cave_floor_bold(y - 1, x + 1))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 1aa: north-south doorway */
	if (borg_cave_floor_bold(y - 1, x) && borg_cave_floor_bold(y + 1, x) &&
		!borg_cave_floor_bold(y, x - 1) && !borg_cave_floor_bold(y, x + 1))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 1ba: east-west doorway */
	if (borg_cave_floor_bold(y, x - 1) && borg_cave_floor_bold(y, x + 1) &&
		!borg_cave_floor_bold(y - 1, x) && !borg_cave_floor_bold(y + 1, x))
	{
		/* Happy */
		return (TRUE);
	}


	/* Case 2a: north pillar */
	if (!borg_cave_floor_bold(y - 1, x) &&
		borg_cave_floor_bold(y - 1, x - 1) &&
		borg_cave_floor_bold(y - 1, x + 1) && borg_cave_floor_bold(y - 2, x))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2b: south pillar */
	if (!borg_cave_floor_bold(y + 1, x) &&
		borg_cave_floor_bold(y + 1, x - 1) &&
		borg_cave_floor_bold(y + 1, x + 1) && borg_cave_floor_bold(y + 2, x))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2c: east pillar */
	if (!borg_cave_floor_bold(y, x + 1) &&
		borg_cave_floor_bold(y - 1, x + 1) &&
		borg_cave_floor_bold(y + 1, x + 1) && borg_cave_floor_bold(y, x + 2))
	{
		/* Happy */
		return (TRUE);
	}

	/* Case 2d: west pillar */
	if (!borg_cave_floor_bold(y, x - 1) &&
		borg_cave_floor_bold(y - 1, x - 1) &&
		borg_cave_floor_bold(y + 1, x - 1) && borg_cave_floor_bold(y, x - 2))
	{
		/* Happy */
		return (TRUE);
	}

	/* check for grids that have been stepped on before */
	for (i = 0; i < track_step_num; i++)
	{
		/* Enqueue the grid */
		if ((track_step_y[i] == y) && (track_step_x[i] == x))
		{
			/* Recent step is good */
			if (i < 25)
			{
				return (TRUE);
			}
		}
	}

	/* Not happy */
	return (FALSE);
}


/*
 * Target a location.  Can be used alone or at "Direction?" prompt.
 *
 * Warning -- This will only work for locations on the current panel.
 * So before you call this be sure there was a call to map_in_bounds.
 */
void borg_target(int x, int y)
{
	int x1, y1, x2, y2;
	map_block *mb_ptr;

	/* Bounds checking */
	if (!map_in_bounds(x, y))
	{
		borg_oops("Untargettable location (%d, %d)", x, y);
		return;
	}

	/* Get the grid */
	mb_ptr = map_loc(x, y);

	/* Report a little bit */
	if (mb_ptr->monster)
	{
		borg_note("# Targeting %s, from (%d, %d) to (%d, %d).",
					  mon_race_name(&r_info[mb_ptr->monster]), c_x, c_y, x, y);
	}
	else
	{
		borg_note("# Targetting location from (%d, %d) to (%d,%d)",
					  c_x, c_y, x, y);
	}

	/* Target mode */
	borg_keypress('*');

	/* Target a location */
	borg_keypress('p');

	/* Determine "path" */
	x1 = c_x;
	y1 = c_y;
	x2 = x;
	y2 = y;

	/* Move to the location (diagonals) */
	for (; (y1 < y2) && (x1 < x2); y1++, x1++) borg_keypress('3');
	for (; (y1 < y2) && (x1 > x2); y1++, x1--) borg_keypress('1');
	for (; (y1 > y2) && (x1 < x2); y1--, x1++) borg_keypress('9');
	for (; (y1 > y2) && (x1 > x2); y1--, x1--) borg_keypress('7');

	/* Move to the location */
	for (; y1 < y2; y1++) borg_keypress('2');
	for (; y1 > y2; y1--) borg_keypress('8');
	for (; x1 < x2; x1++) borg_keypress('6');
	for (; x1 > x2; x1--) borg_keypress('4');

	/* Select the target */
	borg_keypress('5');

	/* Success */
	return;
}


static bool test_borg_lite_beam(byte dir, byte radius)
{
	int x = c_x, y = c_y;
	int dx = 0, dy = 0;
	int i;

	map_block *mb_ptr;

	switch (dir)
	{
		case 8:
		{
			/* North */
			dx = 0;
			dy = -1;

			break;
		}

		case 6:
		{
			/* East */
			dx = 1;
			dy = 0;

			break;
		}

		case 2:
		{
			/* South */
			dx = 0;
			dy = 1;

			break;
		}

		case 4:
		{
			/* West */
			dx = -1;
			dy = 0;

			break;
		}
	}

	for (i = 0; i < radius; i++)
	{
		x += dx;
		y += dy;

		/* No need to light beyond the map */
		if (!map_in_bounds(x, y)) return (FALSE);

		mb_ptr = map_loc(x, y);

		/* Walls aren't interesting to light */
		if (borg_cave_wall_grid(mb_ptr)) return (FALSE);

		/* Unlit square just out of los? */
		if (!mb_ptr->feat) return (TRUE);
	}

	/* Nothing interesting just out of los */
	return (FALSE);
}

/*
 * This will look down a hallway and possibly light it up using
 * the Light Beam mage spell.  This spell is mostly used when
 * the borg is moving through the dungeon under boosted bravery.
 * This will allow him to "see" if anyone is there.
 *
 * It might also come in handy if he's in a hallway and gets shot, or
 * if resting in a hallway.  He may want to cast it to make
 * sure no previously unknown monsters are in the hall.
 * NOTE:  ESP will alter the value of this spell.
 *
 * Borg has a problem when not on map centering mode and casting the beam
 * repeatedly, down or up when at the edge of a panel.
 */
bool borg_lite_beam(bool simulation, int *dir)
{
	if (simulation)
	{
		/* Hack -- weak/dark is very unhappy */
		if (bp_ptr->status.weak || !bp_ptr->cur_lite) return (FALSE);

		/* Apply a control effect so that he does not get stuck in a loop */
		if ((borg_t - borg_began) >= 2000) return (FALSE);

		/* Require the ability */
		if (!borg_spell_okay_fail(REALM_NATURE, 1, 4, 20) &&
			!borg_spell_okay_fail(REALM_ARCANE, 2, 4, 20) &&
			!borg_equips_wand_fail(SV_WAND_LITE) &&
			!borg_equips_rod_fail(SV_ROD_LITE))
			return (FALSE);

		/* Set the direction to nowhere */
		*dir = 5;

		/* North */
		if (test_borg_lite_beam(8, MAX_RANGE)) *dir = 8;

		/* East */
		else if (test_borg_lite_beam(6, MAX_RANGE)) *dir = 6;

		/* West */
		else if (test_borg_lite_beam(4, MAX_RANGE)) *dir = 4;

		/* South */
		else if (test_borg_lite_beam(2, MAX_RANGE)) *dir = 2;

		/* Failure? */
		if (*dir == 5) return (FALSE);

		/* simulation */
		return (TRUE);
	}

	/* Drop old target */
	borg_keypress('*');
	borg_keypress(ESCAPE);

	/* cast the light beam */
	if (borg_spell(REALM_NATURE, 1, 4) ||
		borg_spell(REALM_ARCANE, 2, 5) ||
		borg_zap_rod(SV_ROD_LITE) ||
		borg_aim_wand(SV_WAND_LITE))
	{	
		/* apply the direction */
		borg_keypress(I2D(*dir));

		/* Tell what you do */
		borg_note("# Illuminating this hallway, dir = %d", *dir);

		/* Leave */
		return (TRUE);
	}

	/* Huh?  One of the three spells must be available! */
	borg_oops("Not supposed to happen!");

	/* can't do it */
	return (FALSE);
}

/*
 * Scan the monster lists for certain types of monster that we
 * should be concerned over.
 * This only works for monsters we know about.  If one of the
 * monsters around is misidentified then it may be a unique
 * and we wouldn't know.  Special consideration is given to The Serpent
 */
void borg_near_monster_type(int dist)
{
	borg_kill *kill;
	monster_race *r_ptr;

	int x9, y9, ax, ay, d;
	int i;

	/* reset the borg flags */
	borg_fighting_unique = 0;
	borg_fighting_evil_unique = FALSE;

	/* Scan the monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		kill = &borg_kills[i];
		r_ptr = &r_info[kill->r_idx];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;


		x9 = kill->x;
		y9 = kill->y;

		/* Distance components */
		ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
		ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

		/* Distance */
		d = MAX(ax, ay);

		/* if the guy is too far then skip it. */
		if (d > dist) continue;


		/*** Scan for Uniques ***/

		/* this is a unique. */
		if (FLAG(r_ptr, RF_UNIQUE))
		{
			/* Set a flag for use with certain types of spells */
			unique_on_level = TRUE;

			/* Remember which unique */
			unique_r_idx = kill->r_idx;

			/* return 1 if not Serpent, +101 if it is Serpent or Oberon */
			if (FLAG(r_ptr, RF_QUESTOR))
			{
				/* A Questor adds a BORG_QUESTOR + 1 value */
				borg_fighting_unique += BORG_QUESTOR;
			}

			/* regular unique */
			borg_fighting_unique++;

			/* Is there an evil unique */
			if (FLAG(r_ptr, RF_EVIL)) borg_fighting_evil_unique = TRUE;
		}
	}
}

/*
 * Help determine if "phase door" seems like a good idea
 */
bool borg_caution_phase(int emergency, int turns)
{
	int n, k, i, d, x, y, p;

	int dis = 10;
	int min;

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Define minimal distance */
	min = dis / 2;

	/* Simulate 100 attempts */
	for (n = k = 0; k < 100; k++)
	{
		/* Pick a location */
		for (i = 0; i < 100; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(c_y, dis);
				x = rand_spread(c_x, dis);
				d = distance(c_y, c_x, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			if (!map_in_bounds(x, y)) continue;

			/* Access */
			mb_ptr = map_loc(x, y);

			/* If low level, unknown squares are scary */
			if (!mb_ptr->feat && bp_ptr->mhp < 30) break;

			/* Skip unknown grids */
			if (!mb_ptr->feat) continue;

			/* Skip walls */
			if (borg_cave_wall_grid(mb_ptr)) continue;

			/* Skip monsters */
			if (mb_ptr->monster) continue;

			/* Stop looking */
			break;
		}

		/* If low level, unknown squares are scary */
		if (!mb_ptr->feat && bp_ptr->mhp < 30)
		{
			n++;
			continue;
		}

		/* No location */
		/* in the real code it would keep trying but here we should */
		/* assume that there is unknown spots that you would be able */
		/* to go but may be dangerious. */
		if (i >= 100)
		{
			n++;
			continue;
		}

		/* Examine */
		p = borg_danger(x, y, turns, TRUE);

		/* if *very* scary, do not allow jumps at all */
		if (p > bp_ptr->chp) n++;

	}

	/* Too much danger */
	/* in an emergency try with extra danger allowed */
	if (n > emergency)
	{
		borg_note("# No Phase. scary squares: %d", n);
		return (FALSE);
	}
	else
		borg_note("# Safe to Phase. scary squares: %d", n);

	/* Okay */
	return (TRUE);
}

/*
 * Help determine if "dimension door" seems like a good idea
 */
static bool borg_dim_door(int emergency, int p1)
{
	int x, y, p;

	int b_y = -1, b_x = -1, b_p = p1;
	int dis = bp_ptr->lev + 2;

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Scan every grid in landing zone */
	for (y = c_y - dis; y < c_y + dis; y++)
	{
		/* Pick a location */
		for (x = c_x - dis; x < c_x + dis; x++)
		{
			if (!map_in_bounds(x, y)) continue;

			if ((x == c_x) && (y == c_y)) continue;

			/* Access */
			mb_ptr = map_loc(x, y);

			/* Verify distance again */
			if (distance(y, x, c_y, c_x) > bp_ptr->lev + 2) continue;

			/* Skip unknown grids */
			if (!mb_ptr->feat) continue;

			/* Skip walls, trees, water, lava */
			if (borg_cave_wall_grid(mb_ptr)) continue;

			/* Skip monsters */
			if (mb_ptr->monster) continue;

			/* Examine */
			p = borg_danger(x, y, 1, TRUE);

			/* if *very* scary, do not allow jumps at all */
			if (!emergency && p > bp_ptr->chp) continue;

			/* Track the grid with the least danger */
			if (p > b_p) continue;

			/* note good landing zones */
			b_p = p;
			b_y = y;
			b_x = x;
		}
	}


	/* Dimension Door report */
	borg_note
		("# Dim Door: Safest grid: (%d, %d) with %d Danger", b_y, b_x, b_p);
	dim_door_y = b_y;
	dim_door_x = b_x;

	/* No good landing zone */
	if (b_p >= p1) return (FALSE);

	/* Okay */
	return (TRUE);
}


/* Just in case the key changes again */
void borg_press_faint_accept(void)
{
	borg_keypress('y');
}


/*
 * Try to phase door or teleport
 * b_q is the danger of the least dangerious square around us.
 */
bool borg_escape(int b_q)
{
	/* Risky borgs are more likely to stay in a fight */
	int risky_boost = 3;

	bool amt_dim_door = FALSE;

	/* only escape with spell if fail is low */
	int allow_fail = 25;
	int sv_mana;

	/* Not if locked down */
	if (FLAG(bp_ptr, TR_NO_TELE)) return (FALSE);

	/* if we have Dim Door spell */
	amt_dim_door = (borg_activate_fail(BORG_ACT_DIM_DOOR) ||
					borg_spell_okay_fail(REALM_SORCERY, 2, 3, allow_fail) ||
					borg_spell_okay_fail(REALM_TRUMP, 0, 5, allow_fail) ||
					borg_mindcr_okay_fail(MIND_MINOR_DISP, 40, allow_fail));

	/* if very healthy, allow extra fail */
	if (((bp_ptr->chp * 100) / bp_ptr->mhp) > 70)
		allow_fail = 10;

	/* comprimised, get out of the fight */
	if (bp_ptr->status.heavy_stun)
		allow_fail = 35;

	/* for emergencies */
	sv_mana = bp_ptr->csp;

	/* Borgs who are bleeding to death or dying of poison may sometimes
	 * phase around the last two hit points right before they enter a
	 * shop.  He knows to make a bee-line for the temple but the danger
	 * trips this routine.  So we must bypass this routine for some
	 * particular circumstances.
	 */
	if (!bp_ptr->depth &&
		(bp_ptr->status.poisoned || bp_ptr->status.weak ||
		 bp_ptr->status.cut)) return (FALSE);

	/* Borgs with GOI should not escape until the GOI falls */
	if (borg_goi) return (FALSE);

	/* 1. really scary, I'm about to die */
	/* Try an emergency teleport, or phase door as last resort */
	if (bp_ptr->status.heavy_stun ||
		(b_q >= avoidance * (45 + risky_boost) / 10) ||
		((b_q >= avoidance * (40 + risky_boost) / 10) &&
		 borg_fighting_unique >= BORG_QUESTOR && bp_ptr->depth == 100 &&
		 bp_ptr->chp < 600) ||
		((b_q >= avoidance * (30 + risky_boost) / 10) &&
		 borg_fighting_unique >= BORG_QUESTOR && bp_ptr->depth == 99 &&
		 bp_ptr->chp < 600) ||
		((b_q >= avoidance * (25 + risky_boost) / 10) &&
		 borg_fighting_unique >= 1 && borg_fighting_unique <= 8 &&
		 bp_ptr->depth >= 95 && bp_ptr->chp < 550) ||
		((b_q >= avoidance * (17 + risky_boost) / 10) &&
		 borg_fighting_unique >= 1 && borg_fighting_unique <= 8 &&
		 bp_ptr->depth < 95) ||
		((b_q >= avoidance * (15 + risky_boost) / 10) && !borg_fighting_unique))
	{

		int allow_fail = 11;

		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail - 10) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail - 10) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail - 10) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail - 10) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail - 10) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			/* revisit spells, increased fail rate */
			borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail + 9) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail + 9) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail + 9) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail + 9) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail + 9) ||
			borg_racial(RACE_GNOME) ||
			borg_mutation(MUT1_VTELEPORT) ||
			/* Attempt Teleport Level */
			(bp_ptr->depth &&
			(borg_activate(BORG_ACT_TELEPORT_LEVEL) ||
			borg_spell_fail(REALM_SORCERY, 2, 6, allow_fail + 9) ||
			borg_spell_fail(REALM_TRUMP, 1, 5, allow_fail + 9) ||
			borg_spell_fail(REALM_ARCANE, 3, 1, allow_fail + 9) ||
			borg_racial(RACE_AMBERITE) ||
			borg_read_scroll(SV_SCROLL_TELEPORT_LEVEL))) ||
			/* try Dimension Door */
			(amt_dim_door && borg_dim_door(TRUE, b_q) &&
			  (borg_activate(BORG_ACT_DIM_DOOR) ||
			   borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			   borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			   borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))) ||
			/* try phase at least */
			borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
			borg_activate(BORG_ACT_PHASE_DOOR) ||
			borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail))
		{
			/* Flee! */
			borg_note("# Danger Level 1.");
			return (TRUE);
		}

		/* try to teleport, get far away from here */
		if (borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_read_scroll(SV_SCROLL_TELEPORT))
		{
			/* Flee! */
			borg_note("# Danger Level 1.1  Critical Attempt");
			return (TRUE);
		}

		bp_ptr->csp = bp_ptr->msp;

		if (borg_spell(REALM_ARCANE, 2, 3) ||
			borg_spell(REALM_TRUMP, 0, 4) ||
			borg_spell(REALM_CHAOS, 0, 7) ||
			borg_spell(REALM_SORCERY, 0, 5))
		{
			/* verify use of spell */
			borg_press_faint_accept();

			/* Flee! */
			borg_note("# Danger Level 1.1  Critical Attempt");
			return (TRUE);
		}


		/* emergency phase spell */
		if ((bp_ptr->able.phase && borg_caution_phase(80, 5) &&
			(borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
			 borg_activate(BORG_ACT_PHASE_DOOR))) ||
			borg_activate(BORG_ACT_TELEPORT_LEVEL) ||
			borg_read_scroll(SV_SCROLL_TELEPORT_LEVEL))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 1.2  Critical Phase");
			return (TRUE);
		}

		/* emergency phase spell */
		if (borg_caution_phase(80, 5) &&
			(borg_spell_fail(REALM_ARCANE, 0, 4, 15) ||
			 borg_spell_fail(REALM_SORCERY, 0, 1, 15) ||
			 borg_spell_fail(REALM_TRUMP, 0, 0, 15)))
		{
			/* verify use of spell */
			borg_press_faint_accept();

			/* Flee! */
			borg_note("# Danger Level 1.3  Critical Attempt");
			return (TRUE);
		}
		bp_ptr->csp = sv_mana;
	}

	/* If fighting a unique and at the end of the game try to stay and
	 * finish the fight.  Only bail out in extreme danger as above.
	 */
	if ((b_q < avoidance * (25 + risky_boost) / 10 && borg_fighting_unique >= 1
		 && borg_fighting_unique <= 3 && bp_ptr->depth >= 97) ||
		bp_ptr->chp > 550) return (FALSE);


	/* 2 - a bit more scary */
	/* Attempt to teleport (usually) */
	/* do not escape from uniques so quick */
	if (bp_ptr->status.heavy_stun ||
		((b_q >= avoidance * (15 + risky_boost) / 10) &&
		 borg_fighting_unique >= 1 && borg_fighting_unique <= 8 &&
		 bp_ptr->depth != 99) ||
		((b_q >= avoidance * (13 + risky_boost) / 10) && !borg_fighting_unique))
	{

		/* Try teleportation */
		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail - 10) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail - 10) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail - 10) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail - 10) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail - 10) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail) ||
			borg_racial(RACE_GNOME) ||
			borg_mutation(MUT1_VTELEPORT) ||
			/* try Dimension Door */
			(amt_dim_door && borg_dim_door(TRUE, b_q) &&
			 (borg_activate(BORG_ACT_DIM_DOOR) ||
			  borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))))
		{
			/* Flee! */
			borg_note("# Danger Level 2.1");

			/* Success */
			return (TRUE);
		}
		/* Phase door, if useful */
		if (bp_ptr->able.phase && borg_caution_phase(50, 2) &&
			(borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
			 borg_spell(REALM_ARCANE, 0, 4) ||
			 borg_spell(REALM_SORCERY, 0, 1) ||
			 borg_spell(REALM_TRUMP, 0, 0) ||
			 borg_mindcr(MIND_MINOR_DISP, 3) ||
			 borg_activate(BORG_ACT_PHASE_DOOR) ||
			 borg_activate(BORG_ACT_TELEPORT)))
		{
			/* Flee! */
			borg_note("# Danger Level 2.2");
			/* Success */
			return (TRUE);
		}

	}

	/* 3- not too bad */
	/* also run if stunned or it is scary here */
	if (bp_ptr->status.heavy_stun ||
		((b_q >= avoidance * (13 + risky_boost) / 10) &&
		 borg_fighting_unique >= 2 && borg_fighting_unique <= 8) ||
		((b_q >= avoidance * (10 + risky_boost) / 10) && !borg_fighting_unique)
		|| ((b_q >= avoidance * (10 + risky_boost) / 10) &&
			bp_ptr->status.afraid && !bp_ptr->able.missile &&
			(borg_class == CLASS_WARRIOR)))
	{
		/* try Dimension Door */
		if ((amt_dim_door && borg_dim_door(TRUE, b_q) &&
			 (borg_activate(BORG_ACT_DIM_DOOR) ||
			  borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))) ||
			/* Phase door, if useful */
			(bp_ptr->able.phase && borg_caution_phase(25, 2) &&
			 (borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			  borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			  borg_activate(BORG_ACT_PHASE_DOOR) ||
			  borg_activate(BORG_ACT_TELEPORT) ||
			  borg_read_scroll(SV_SCROLL_PHASE_DOOR))))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 3.1");

			/* Success */
			return (TRUE);
		}

		/* Teleport via spell */
		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_mutation(MUT1_VTELEPORT) ||
			borg_racial(RACE_GNOME))
		{
			/* Flee! */
			borg_note("# Danger Level 3.2");

			/* Success */
			return (TRUE);
		}
		/* Phase door, if useful */
		if (bp_ptr->able.phase && borg_caution_phase(65, 2) &&
			(borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			 borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			 borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			 borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			 borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			 borg_activate(BORG_ACT_PHASE_DOOR) ||
			 borg_activate(BORG_ACT_TELEPORT) ||
			 borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 3.3");

			/* Success */
			return (TRUE);
		}

		/* if we got this far we tried to escape but couldn't... */
		/* time to flee */
		if (!goal_fleeing &&
			(!borg_fighting_unique || bp_ptr->lev < 35) && !vault_on_level)
		{
			/* Note */
			borg_note("# Fleeing (failed to teleport)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}

		/* Flee now */
		if (!goal_leaving &&
			(!borg_fighting_unique || bp_ptr->lev < 35) && !vault_on_level)
		{
			/* Flee! */
			borg_note("# Leaving (failed to teleport)");

			/* Start leaving */
			goal_leaving = TRUE;
		}

	}
	/* 4- not too scary but I'm comprimized */
	if ((b_q >= avoidance * (8 + risky_boost) / 10 &&
		 (bp_ptr->lev < 35 ||
		  bp_ptr->chp <= bp_ptr->mhp / 3)) ||
		((b_q >= avoidance * (12 + risky_boost) / 10) &&
		 borg_fighting_unique >= 1 && borg_fighting_unique <= 8 &&
		 (bp_ptr->lev < 35 ||
		  bp_ptr->chp <= bp_ptr->mhp / 3)) ||
		((b_q >= avoidance * (6 + risky_boost) / 10) &&
		 bp_ptr->lev <= 20 && !borg_fighting_unique) ||
		((b_q >= avoidance * (6 + risky_boost) / 10) && borg_class == CLASS_MAGE
		 && bp_ptr->lev <= 35))
	{
		/* Phase door, if useful */
		if ((amt_dim_door && borg_dim_door(TRUE, b_q) &&
			 (borg_activate(BORG_ACT_DIM_DOOR) ||
			  borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))) ||
			(bp_ptr->able.phase && borg_caution_phase(20, 2) &&
			 (borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			  borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			  borg_activate(BORG_ACT_PHASE_DOOR) ||
			  borg_activate(BORG_ACT_TELEPORT) ||
			  borg_read_scroll(SV_SCROLL_PHASE_DOOR))))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 4.1");
			/* Success */
			return (TRUE);
		}

		/* Teleport via spell */
		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_mutation(MUT1_VTELEPORT) ||
			borg_racial(RACE_GNOME))
		{
			/* Flee! */
			borg_note("# Danger Level 4.2");

			/* Success */
			return (TRUE);
		}

		/* if we got this far we tried to escape but couldn't... */
		/* time to flee */
		if (!goal_fleeing && !borg_fighting_unique && bp_ptr->lev < 25
			&& !vault_on_level)
		{
			/* Note */
			borg_note("# Fleeing (failed to teleport)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}

		/* Flee now */
		if (!goal_leaving && !borg_fighting_unique && !vault_on_level)
		{
			/* Flee! */
			borg_note("# Leaving (failed to teleport)");

			/* Start leaving */
			goal_leaving = TRUE;
		}
		/* Emergency Phase door if a weak mage */
		if ((borg_class == CLASS_MAGE && bp_ptr->lev <= 35) &&
			bp_ptr->able.phase && borg_caution_phase(65, 2) &&
			(borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			 borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			 borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			 borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			 borg_activate(BORG_ACT_PHASE_DOOR) ||
			 borg_activate(BORG_ACT_TELEPORT) ||
			 borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 4.3");
			/* Success */
			return (TRUE);
		}

	}

	/* 5- not too scary but I'm very low level  */
	if (bp_ptr->lev < 10 &&
		(b_q >= avoidance * (6 + risky_boost) / 10 ||
		 (b_q >= avoidance * (8 + risky_boost) / 10 && borg_fighting_unique >= 1
		  && borg_fighting_unique <= 8)))
	{
		/* Dimension Door, if useful */
		if ((amt_dim_door && borg_dim_door(TRUE, b_q) &&
			 (borg_activate(BORG_ACT_DIM_DOOR) ||
			  borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))) ||
			/* Phase Door */
			(bp_ptr->able.phase && borg_caution_phase(20, 2) &&
			 (borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			  borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			  borg_activate(BORG_ACT_PHASE_DOOR) ||
			  borg_activate(BORG_ACT_TELEPORT) ||
			  borg_read_scroll(SV_SCROLL_PHASE_DOOR))))
		{
			/* Flee! */
			borg_note("# Danger Level 5.1");
			/* Success */
			return (TRUE);
		}

		/* Teleport via spell */
		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_mutation(MUT1_VTELEPORT) ||
			borg_racial(RACE_GNOME))
		{
			/* Flee! */
			borg_note("# Danger Level 5.2");

			/* Success */
			return (TRUE);
		}

		/* if we got this far we tried to escape but couldn't... */
		/* time to flee */
		if (!goal_fleeing && !borg_fighting_unique)
		{
			/* Note */
			borg_note("# Fleeing (failed to teleport)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}

		/* Flee now */
		if (!goal_leaving && !borg_fighting_unique)
		{
			/* Flee! */
			borg_note("# Leaving (failed to teleport)");

			/* Start leaving */
			goal_leaving = TRUE;
		}
		/* Emergency Phase door if a weak mage */
		if ((borg_class == CLASS_MAGE && bp_ptr->lev <= 8) &&
			bp_ptr->able.phase && borg_caution_phase(65, 2) &&
			(borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			 borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			 borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			 borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			 borg_activate(BORG_ACT_PHASE_DOOR) ||
			 borg_activate(BORG_ACT_TELEPORT) ||
			 borg_read_scroll(SV_SCROLL_PHASE_DOOR)))
		{
			/* Flee! */
			borg_escapes--;		/* a phase isn't really an escape */
			borg_note("# Danger Level 5.3");
			/* Success */
			return (TRUE);
		}

	}

	/* 6- not too scary but I'm out of mana  */
	if ((borg_class == CLASS_MAGE || borg_class == CLASS_HIGH_MAGE ||
		borg_class == CLASS_PRIEST || borg_class == CLASS_MINDCRAFTER) &&
		(b_q >= avoidance * (6 + risky_boost) / 10 ||
		 (b_q >= avoidance * (8 + risky_boost) / 10 && borg_fighting_unique >= 1
		  && borg_fighting_unique <= 8)) &&
		(bp_ptr->csp <= (bp_ptr->msp * 1 / 10) && bp_ptr->msp >= 100))
	{
		/* Dimension Door, if useful */
		if ((amt_dim_door && borg_dim_door(TRUE, b_q) &&
			 (borg_activate(BORG_ACT_DIM_DOOR) ||
			  borg_spell_fail(REALM_SORCERY, 2, 3, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 5, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 40, allow_fail))) ||
			/* Phase Door */
			(bp_ptr->able.phase && borg_caution_phase(20, 2) &&
			 (borg_spell_fail(REALM_ARCANE, 0, 4, allow_fail) ||
			  borg_spell_fail(REALM_SORCERY, 0, 1, allow_fail) ||
			  borg_spell_fail(REALM_TRUMP, 0, 0, allow_fail) ||
			  borg_mindcr_fail(MIND_MINOR_DISP, 3, allow_fail) ||
			  borg_activate(BORG_ACT_PHASE_DOOR) ||
			  borg_activate(BORG_ACT_TELEPORT) ||
			  borg_read_scroll(SV_SCROLL_PHASE_DOOR))))
		{
			/* Flee! */
			borg_note("# Danger Level 6.1");
			/* Success */
			return (TRUE);
		}

		/* Teleport via spell */
		if (borg_spell_fail(REALM_ARCANE, 2, 3, allow_fail) ||
			borg_spell_fail(REALM_TRUMP, 0, 4, allow_fail) ||
			borg_spell_fail(REALM_CHAOS, 0, 7, allow_fail) ||
			borg_spell_fail(REALM_SORCERY, 0, 5, allow_fail) ||
			borg_mindcr_fail(MIND_MAJOR_DISP, 7, allow_fail) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_read_scroll(SV_SCROLL_TELEPORT) ||
			borg_use_staff_fail(SV_STAFF_TELEPORTATION) ||
			borg_mutation(MUT1_VTELEPORT) ||
			borg_racial(RACE_GNOME))
		{
			/* Flee! */
			borg_note("# Danger Level 6.2");

			/* Success */
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * ** Try healing **
 * this function tries to heal the borg before trying to flee.
 * The ez_heal items (*Heal* and Life) are reserved for The Serpent.
 * In severe emergencies the borg can drink an ez_heal item but that is
 * checked in borg_caution().  He should bail out of the fight before
 * using an ez_heal.
 */
bool borg_heal(int danger)
{
	int hp_down;
	int allow_fail = 20;
	int chance;

	int stats_needing_fix = 0;
	int i;

	map_block *mb_ptr = map_loc(c_x, c_y);

	hp_down = bp_ptr->mhp - bp_ptr->chp;



	/*
	 * When fighting The Serpent, we want the borg to use Life potion to fix his
	 * stats.  So we need to add up the ones that are dropped.
	 */
	for (i = 0; i < A_MAX; i++)
	{
		if (bp_ptr->status.fixstat[i]) stats_needing_fix++;
	}

	/* Special cases get a second vote */
	if ((borg_class == CLASS_MAGE || borg_class == CLASS_HIGH_MAGE) &&
		bp_ptr->status.fixstat[A_INT]) stats_needing_fix++;

	if ((borg_class == CLASS_PRIEST || borg_class == CLASS_MINDCRAFTER) &&
		bp_ptr->status.fixstat[A_WIS]) stats_needing_fix++;

	if (!borg_has_realm(REALM_LIFE) &&
		bp_ptr->status.fixstat[A_CON]) stats_needing_fix++;

	if (bp_ptr->mhp <= 850 && bp_ptr->status.fixstat[A_CON])
	{
		stats_needing_fix++;
	}
	if (bp_ptr->mhp <= 700 && bp_ptr->status.fixstat[A_CON])
	{
		stats_needing_fix += 3;
	}
	if ((borg_class == CLASS_PRIEST || borg_class == CLASS_MINDCRAFTER) &&
		bp_ptr->msp < 100 &&
		bp_ptr->status.fixstat[A_WIS])
		stats_needing_fix += 5;

	if ((borg_class == CLASS_MAGE || borg_class == CLASS_HIGH_MAGE) &&
		bp_ptr->msp < 100 &&
		bp_ptr->status.fixstat[A_INT])
		stats_needing_fix += 5;


	/*
	 * Hack -- heal when confused. This is deadly.
	 *
	 * This is checked twice, once, here, to see if he is in low danger
	 * and again at the end of borg_caution, when all other avenues have failed
	 */
	if (bp_ptr->status.confused && (randint0(100) < 85))
	{
		if ((hp_down >= 300) && danger - 300 < bp_ptr->chp &&
			borg_quaff_potion(SV_POTION_HEALING))
		{
			borg_note("# Fixing Confusion. Level 1");
			return (TRUE);
		}
		if (danger - 20 < bp_ptr->chp &&
			(borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
			 borg_activate(BORG_ACT_HEAL_BIG) ||
			 borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			 borg_quaff_crit(FALSE) ||
			 borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_use_staff_fail(SV_STAFF_HEALING) ||
			 borg_use_staff_fail(SV_STAFF_CURING)))
		{
			borg_note("# Fixing Confusion. Level 2");
			return (TRUE);
		}

		/*
		 * If my ability to use a teleport staff is really
		 * bad, then I should heal up then use the staff.
		 */
		/* Check for a charged teleport staff */
		if (borg_equips_staff_fail(SV_STAFF_TELEPORTATION))
		{
			/* Check my skill, drink a potion */
			if ((bp_ptr->skill_dev -
				 borg_get_kind(TV_STAFF, SV_STAFF_TELEPORTATION)->
				 level > 7) && (danger < (avoidance + 35) * 15 / 10) &&
				(borg_quaff_crit(FALSE) ||
				 borg_quaff_potion(SV_POTION_HEALING)))
			{
				borg_note("# Fixing Confusion. Level 3");
				return (TRUE);
			}
			/*
			 * However, if I am in really big trouble and there is
			 * no way I am going to be able to
			 * survive another round, take my chances on the staff.
			 */
			else if (danger >= avoidance * 15 / 10)
			{
				borg_note("# Too scary to fix Confusion. Level 4");
				return (FALSE);
			}

		}

	}
	/*  Hack -- heal when blind. This is deadly. */
	if (bp_ptr->status.blind && (randint0(100) < 85))
	{
		/*
		 * If in extreme danger, use teleport then fix the
		 * blindness later.
		 */
		if (danger > avoidance * 25 / 10)
		{
			/* Check for a charged teleport staff */
			if (borg_equips_staff_fail(SV_STAFF_TELEPORTATION)) return (0);
		}
		if ((hp_down >= 300) && borg_quaff_potion(SV_POTION_HEALING))
		{
			return (TRUE);
		}
		/* Warriors with ESP won't need it so quickly */
		if (!(borg_class == CLASS_WARRIOR &&
			  bp_ptr->chp > bp_ptr->mhp / 4 &&
			  FLAG(bp_ptr, TR_TELEPATHY)))
		{
			if (borg_eat_food(SV_FOOD_CURE_BLINDNESS) ||
				borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
				borg_quaff_crit(TRUE) ||
				borg_quaff_potion(SV_POTION_HEALING) ||
				borg_use_staff_fail(SV_STAFF_HEALING) ||
				borg_use_staff_fail(SV_STAFF_CURING))
			{
				borg_note("# Fixing Blindness.");
				return (TRUE);
			}
		}
	}


	/* We generally try to conserve ez-heal pots */
	if ((bp_ptr->status.blind || bp_ptr->status.confused) &&
		((hp_down >= 400) ||
		 (danger > bp_ptr->chp * 5 && hp_down > 100)) &&
		borg_quaff_potion(SV_POTION_STAR_HEALING))
	{
		borg_note("# Fixing Confusion/Blind.");
		return (TRUE);
	}

	/*  Hack -- rest until healed */
	if ((!bp_ptr->status.blind && !bp_ptr->status.poisoned &&
		 !bp_ptr->status.cut && !borg_goi && !borg_see_inv && !borg_shield &&
		 !bp_ptr->status.weak && !bp_ptr->status.hungry &&
		 (bp_ptr->status.confused ||
		  bp_ptr->status.image ||
		  bp_ptr->status.afraid ||
		  bp_ptr->status.stun ||
		  bp_ptr->status.heavy_stun ||
		  (bp_ptr->chp < bp_ptr->mhp) ||
		  (bp_ptr->csp < bp_ptr->msp * 6 / 10)) &&
		 (danger < avoidance / 5)) && borg_check_rest() && !scaryguy_on_level &&
		(danger <= mb_ptr->fear) && !goal_fleeing &&
		borg_on_safe_feat(map_loc(c_x, c_y)->feat))
	{
		/* check for then call lite in dark room before resting */
		if (!borg_check_lite_only())
		{
			/* Take note */
			borg_note("# Resting to restore HP/SP...");

			/* Rest until done */
			borg_keypress('R');
			borg_keypress('\n');

			/* reset the inviso clock to avoid loops */
			need_see_inviso = borg_t - 50;

			/* Done */
			return (TRUE);
		}
		else
		{
			/* Must have been a dark room */
			borg_note("# Lighted the darkened room instead of resting.");
			return (TRUE);
		}
	}


	/* Healing and fighting The Serpent. */
	if (borg_fighting_unique >= BORG_QUESTOR)
	{
		if ((bp_ptr->chp <= 625) &&
			(((bp_ptr->chp > 250) &&
			  borg_spell_fail(REALM_LIFE, 2, 6, 14)) ||
			 borg_use_staff_fail(SV_STAFF_HOLINESS) ||
			 ((stats_needing_fix >= 5) && borg_quaff_potion(SV_POTION_LIFE)) ||
			 ((hp_down > 500) && !borg_slot(TV_POTION, SV_POTION_STAR_HEALING)
			  && borg_quaff_potion(SV_POTION_LIFE)) ||
			 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_spell_fail(REALM_LIFE, 2, 6, 17) ||
			 borg_spell_fail(REALM_LIFE, 3, 4, 15) ||
			 borg_spell_fail(REALM_NATURE, 1, 7, allow_fail + 9) ||
			 borg_spell_fail(REALM_LIFE, 1, 7, 15) ||
			 borg_quaff_potion(SV_POTION_LIFE)))
		{
			borg_note("# Healing in Questor Combat.");
			return (TRUE);
		}
	}

	/* restore Mana */
	/* note, blow the staff charges easy because the staff will not last. */
	if (bp_ptr->csp < (bp_ptr->msp / 5) && (randint0(100) < 50))
	{
		if (borg_use_staff_fail(SV_STAFF_THE_MAGI))
		{
			borg_note("# Use Magi Staff");
			return (TRUE);
		}
	}
	/* blowing potions is harder */
	/* NOTE: must have enough mana to keep up GOI or do a HEAL */
	if (bp_ptr->csp < (bp_ptr->msp / 10) ||
		((bp_ptr->csp < 70 && bp_ptr->msp > 200) &&
		 (borg_goi <= borg_game_ratio * 3)))
	{
		/*  use the potion if battling a unique and not too dangerous */
		if (borg_fighting_unique >= BORG_QUESTOR ||
			(borg_fighting_unique && danger < avoidance * 2) ||
			(!bp_ptr->able.teleport && danger > avoidance))
		{
			if (borg_use_staff_fail(SV_STAFF_THE_MAGI) ||
				borg_quaff_potion(SV_POTION_RESTORE_MANA))
			{
				borg_note("# Restored My Mana");
				return (TRUE);
			}
		}
	}

	/* if unhurt no healing needed */
	if (hp_down == 0)
		return FALSE;

	/* Don't bother healing if not in danger */
	if (danger == 0 && !bp_ptr->status.poisoned && !bp_ptr->status.cut)
		return (FALSE);

	/* Restoring while fighting The Serpent */
	if (stats_needing_fix >= 5 && borg_fighting_unique >= BORG_QUESTOR &&
		bp_ptr->chp > 650 && borg_eat_food(SV_FOOD_RESTORING))
	{
		borg_note("# Trying to fix stats in combat.");
		return (TRUE);
	}

	/* No further Healing considerations if fighting Questors */
	if (borg_fighting_unique >= BORG_QUESTOR)
	{
		/* No further healing considerations right now */
		return (FALSE);
	}


	/* Hack -- heal when wounded a percent of the time */
	/* down 4/5 hp 0%                      */
	/* 3/4 hp 2%                           */
	/* 2/3 hp 20%                          */
	/* 1/2 hp 50%                          */
	/* 1/3 hp 75%                          */
	/* 1/4 hp 100%                         */

	chance = randint0(100);

	/* if we are fighting a unique increase the odds of healing */
	if (borg_fighting_unique) chance -= 10;

	/* if danger is close to the hp and healing will help, do it */
	if (danger >= bp_ptr->chp && danger < bp_ptr->mhp)
		chance -= 75;
	else
	{
		if (!borg_has_realm(REALM_LIFE) && !borg_has_realm(REALM_NATURE))
			chance -= 25;
	}


	if (!
		(((bp_ptr->chp <= ((bp_ptr->mhp * 4) / 5)) &&
		  (chance < 0)) ||
		 ((bp_ptr->chp <= ((bp_ptr->mhp * 3) / 4)) &&
		  (chance < 2)) ||
		 ((bp_ptr->chp <= ((bp_ptr->mhp * 2) / 3)) &&
		  (chance < 20)) ||
		 ((bp_ptr->chp <= (bp_ptr->mhp / 2)) && (chance < 50))
		 || ((bp_ptr->chp <= (bp_ptr->mhp / 3)) &&
			 (chance < 75)) ||
		 (bp_ptr->chp <= (bp_ptr->mhp / 4)) ||
		 bp_ptr->status.heavy_stun || bp_ptr->status.stun ||
		 bp_ptr->status.poisoned || bp_ptr->status.cut))
		return FALSE;


	/* Cure light Wounds (50) */
	if (hp_down < 50 &&
		danger < bp_ptr->chp + 50 &&
		danger > bp_ptr->chp &&
		(borg_spell_fail(REALM_LIFE, 0, 1, allow_fail) ||
		 borg_spell_fail(REALM_ARCANE, 0, 7, allow_fail) ||
		 borg_spell_fail(REALM_NATURE, 0, 1, allow_fail) ||
		 borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
		 borg_use_staff(SV_STAFF_CURE_LIGHT)))
	{
		borg_note("# Healing Level 1.");
		return (TRUE);
	}
	/* Cure Serious Wounds (75) */
	if (hp_down < 75 &&
		danger < bp_ptr->chp + 75 &&
		danger > bp_ptr->chp &&
		(borg_spell_fail(REALM_LIFE, 0, 6, allow_fail) ||
		 borg_spell_fail(REALM_ARCANE, 2, 2, allow_fail) ||
		 borg_quaff_potion(SV_POTION_CURE_SERIOUS)))
	{
		borg_note("# Healing Level 2.");
		return (TRUE);
	}

	/* Cure Critical Wounds (150) */
	if (hp_down < 150 &&
		danger < bp_ptr->chp + 150 &&
		danger > bp_ptr->chp &&
		(borg_spell_fail(REALM_LIFE, 1, 2, allow_fail) ||
		 borg_quaff_crit(FALSE)))
	{
		borg_note("# Healing Level 3.");
		return (TRUE);
	}

	/* If in danger try  one more Cure Critical if it will help */
	if (danger >= bp_ptr->chp &&
		danger < bp_ptr->mhp &&
		bp_ptr->chp < 20 && danger < 30 && borg_quaff_crit(TRUE))
	{
		borg_note("# Healing Level 5.");
		return (TRUE);
	}



	/* Generally continue to heal.  But if we are preparing for the end
	 * game uniques, then bail out here in order to save our heal pots.
	 * (unless The Serpent is dead)
	 * Priests wont need to bail, they have good heal spells.
	 */
	if (bp_ptr->max_depth >= 98 &&
		!bp_ptr->winner &&
		!borg_fighting_unique &&
		!borg_has_realm(REALM_LIFE) &&
		!borg_has_realm(REALM_NATURE))
	{
		/* Bail out to save the heal pots for The Serpent */
		return (FALSE);
	}

	/* Heal step one (200hp) */
	if (hp_down < 250 &&
		danger / 2 < bp_ptr->chp + 200 &&
		(((!bp_ptr->able.teleport ||
		   (bp_ptr->skill_dev -
			borg_get_kind(TV_ROD, SV_ROD_HEALING)->level > 7)) &&
		  borg_zap_rod(SV_ROD_HEALING)) ||
		 borg_activate(BORG_ACT_HEAL_BIG) ||
		 borg_use_staff_fail(SV_STAFF_HEALING) ||
		 borg_spell_fail(REALM_LIFE, 1, 6, allow_fail) ||
		 borg_quaff_potion(SV_POTION_HEALING)))
	{
		borg_note("# Healing Level 6.");
		return (TRUE);
	}

	/* Heal step two (300hp) */
	if (hp_down < 350 && danger / 2 < bp_ptr->chp + 300 &&
		(borg_use_staff_fail(SV_STAFF_HEALING) ||
		 (borg_fighting_evil_unique &&
		  borg_spell_fail(REALM_LIFE, 2, 6, allow_fail)) ||
		 borg_use_staff_fail(SV_STAFF_HOLINESS) ||
		 borg_spell_fail(REALM_LIFE, 1, 6, allow_fail) ||
		 ((!bp_ptr->able.teleport ||
		   (bp_ptr->skill_dev - borg_get_kind(TV_ROD,
											  SV_ROD_HEALING)->level > 7)) &&
		  borg_zap_rod(SV_ROD_HEALING)) || borg_zap_rod(SV_ROD_HEALING) ||
		 borg_quaff_potion(SV_POTION_HEALING)))
	{
		borg_note("# Healing Level 7.");
		return (TRUE);
	}

	/* Healing step three (300hp).  */
	if (hp_down < 650 && danger / 2 < bp_ptr->chp + 300 &&
		((borg_fighting_evil_unique &&
		  borg_spell_fail(REALM_LIFE, 2, 6, allow_fail)) ||
		 ((!bp_ptr->able.teleport ||
		   (bp_ptr->skill_dev - borg_get_kind(TV_ROD,
											  SV_ROD_HEALING)->level > 7)) &&
		  borg_zap_rod(SV_ROD_HEALING)) ||
		 borg_spell_fail(REALM_LIFE, 1, 6, allow_fail) ||
		 borg_spell_fail(REALM_NATURE, 1, 7, allow_fail) ||
		 borg_use_staff_fail(SV_STAFF_HOLINESS) ||
		 borg_use_staff_fail(SV_STAFF_HEALING) ||
		 borg_quaff_potion(SV_POTION_HEALING) ||
		 borg_activate(BORG_ACT_HEAL_BIG)))
	{
		borg_note("# Healing Level 8.");
		return (TRUE);
	}

	/* Healing final check.  Note that *heal* and Life potions are not
	 * wasted.  They are saved for The Serpent and emergencies.  The
	 * Emergency check is at the end of borg_caution().
	 */
	if (hp_down >= 650 && (danger / 2 < bp_ptr->chp + 500) &&
		((borg_fighting_evil_unique &&
		  borg_spell_fail(REALM_LIFE, 2, 6, allow_fail)) ||
		 borg_spell_fail(REALM_LIFE, 3, 4, allow_fail) ||
		 borg_spell_fail(REALM_NATURE, 1, 7, allow_fail) ||
		 borg_use_staff_fail(SV_STAFF_HOLINESS) ||
		 borg_use_staff_fail(SV_STAFF_HEALING) ||
		 ((!bp_ptr->able.teleport ||
		   (bp_ptr->skill_dev - borg_get_kind(TV_ROD,
											  SV_ROD_HEALING)->level > 7)) &&
		  borg_zap_rod(SV_ROD_HEALING)) ||
		  borg_quaff_potion(SV_POTION_HEALING) ||
		 borg_activate(BORG_ACT_HEAL_BIG) ||
		 (borg_fighting_unique && (borg_quaff_potion(SV_POTION_HEALING) ||
								   borg_quaff_potion(SV_POTION_LIFE)))))
	{
		borg_note("# Healing Level 9.");
		return (TRUE);
	}

	/*** Cures ***/

	/* Dont do these in the middle of a fight, teleport out then try it */
	if (danger > avoidance * 2 / 10) return (FALSE);

	/* Hack -- cure poison when poisoned
	 * This was moved from borg_caution.
	 */
	if (bp_ptr->status.poisoned && (bp_ptr->chp < bp_ptr->mhp / 2))
	{
		if (borg_spell_fail(REALM_LIFE, 1, 1, 60) ||
			borg_spell_fail(REALM_ARCANE, 1, 5, 60) ||
			borg_spell_fail(REALM_NATURE, 0, 7, 60) ||
			borg_quaff_potion(SV_POTION_CURE_POISON) ||
			borg_activate(BORG_ACT_CURE_POISON) ||
			borg_use_staff(SV_STAFF_CURING) ||
			borg_eat_cure_poison() ||
			borg_racial(RACE_AMBERITE_POWER2) ||
			/* buy time */
			borg_quaff_crit(TRUE) ||
			borg_spell_fail(REALM_LIFE, 0, 6, 40) ||
			borg_spell_fail(REALM_LIFE, 0, 1, 40) ||
			borg_spell_fail(REALM_ARCANE, 0, 7, 40) ||
			borg_spell_fail(REALM_NATURE, 0, 1, 40) ||
			borg_use_staff_fail(SV_STAFF_HEALING))
		{
			borg_note("# Curing.");
			return (TRUE);
		}

		/* attempt to fix mana then poison on next round */
		if ((borg_spell_legal(REALM_LIFE, 1, 1) ||
			 borg_spell_legal(REALM_ARCANE, 1, 5) ||
			 borg_spell_legal(REALM_NATURE, 0, 7)) &&
			(borg_quaff_potion(SV_POTION_RESTORE_MANA)))
		{
			borg_note("# Curing next round.");
			return (TRUE);
		}
	}


	/* Hack -- cure poison when poisoned CRITICAL CHECK
	 */
	if (bp_ptr->status.poisoned &&
		(bp_ptr->chp < 2 || bp_ptr->chp < bp_ptr->mhp / 20))
	{
		int sv_mana = bp_ptr->csp;

		bp_ptr->csp = bp_ptr->msp;

		if (borg_spell(REALM_LIFE, 1, 1) ||
			borg_spell(REALM_ARCANE, 1, 5) ||
			borg_spell(REALM_NATURE, 0, 7))
		{
			/* verify use of spell */
			borg_press_faint_accept();

			/* Flee! */
			borg_note("# Emergency Cure Poison! Gasp!!!....");

			return (TRUE);
		}
		bp_ptr->csp = sv_mana;

		/* Quaff healing pots to buy some time- in this emergency.  */
		if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			borg_use_staff(SV_STAFF_CURE_LIGHT) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS)) return (TRUE);

		/* Try to Restore Mana */
		if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

		/* Emergency check on healing.  Borg_heal has already been checked but
		 * but we did not use our ez_heal potions.  All other attempts to save
		 * ourself have failed.  Use the ez_heal if I have it.
		 */
		if (bp_ptr->chp < bp_ptr->mhp / 20 &&
			(borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_LIFE) ||
			 borg_quaff_potion(SV_POTION_HEALING)))
		{
			return (TRUE);
		}

		/* Quaff unknown potions in this emergency.  We might get luck */
		if (borg_quaff_unknown()) return (TRUE);

		/* Eat unknown mushroom in this emergency.  We might get luck */
		if (borg_eat_unknown()) return (TRUE);

		/* Use unknown Staff in this emergency.  We might get luck */
		if (borg_use_unknown()) return (TRUE);

	}

	/* Hack -- cure wounds when bleeding, also critical check */
	if (bp_ptr->status.cut &&
		(bp_ptr->chp < bp_ptr->mhp / 3 || randint0(100) < 20))
	{
		if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			borg_use_staff(SV_STAFF_CURE_LIGHT) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			borg_quaff_crit((bool) (bp_ptr->chp < 10)) ||
			borg_spell_fail(REALM_LIFE, 1, 1, 100) ||
			borg_spell_fail(REALM_LIFE, 0, 6, 100) ||
			borg_spell_fail(REALM_LIFE, 0, 1, 100) ||
			borg_spell_fail(REALM_ARCANE, 2, 2, 100) ||
			borg_spell_fail(REALM_ARCANE, 0, 7, 100) ||
			borg_spell_fail(REALM_NATURE, 0, 7, 100) ||
			borg_spell_fail(REALM_NATURE, 0, 1, 100))
		{
			return (TRUE);
		}
	}
	/* bleeding and about to die CRITICAL CHECK */
	if (bp_ptr->status.cut &&
		((bp_ptr->chp < 2) || bp_ptr->chp < bp_ptr->mhp / 20))
	{
		int sv_mana = bp_ptr->csp;


		/* Quaff healing pots to buy some time- in this emergency.  */
		if (borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			borg_use_staff(SV_STAFF_CURE_LIGHT) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS)) return (TRUE);

		/* Try to Restore Mana */
		if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

		/* Emergency check on healing.  Borg_heal has already been checked but
		 * but we did not use our ez_heal potions.  All other attempts to save
		 * ourself have failed.  Use the ez_heal if I have it.
		 */
		if (bp_ptr->chp < bp_ptr->mhp / 20 &&
			(borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_LIFE)))
		{
			return (TRUE);
		}

		bp_ptr->csp = bp_ptr->msp;

		/* Emergency use of spell */
		if (borg_spell_fail(REALM_LIFE, 1, 1, 100) ||
			borg_spell_fail(REALM_LIFE, 0, 6, 100) ||
			borg_spell_fail(REALM_LIFE, 0, 1, 100) ||
			borg_spell_fail(REALM_ARCANE, 2, 2, 100) ||
			borg_spell_fail(REALM_ARCANE, 0, 7, 100) ||
			borg_spell_fail(REALM_NATURE, 0, 7, 100) ||
			borg_spell_fail(REALM_NATURE, 0, 1, 100))

		{
			/* verify use of spell */
			borg_press_faint_accept();

			/* Flee! */
			borg_note("# Emergency Wound Patch! Gasp!!!....");

			return (TRUE);
		}
		bp_ptr->csp = sv_mana;

		/* Quaff unknown potions in this emergency.  We might get luck */
		if (borg_quaff_unknown()) return (TRUE);

		/* Eat unknown mushroom in this emergency.  We might get luck */
		if (borg_eat_unknown()) return (TRUE);

		/* Use unknown Staff in this emergency.  We might get luck */
		if (borg_use_unknown()) return (TRUE);
	}

	/* nothing to do */
	return (FALSE);

}

/*
 * Be "cautious" and attempt to prevent death or dishonor.
 *
 * Strategy:
 *
 *   (1) Caution
 *   (1a) Analyze the situation
 *   (1a1) try to heal
 *   (1a2) try a defence
 *   (1b) Teleport from danger
 *   (1c) Handle critical stuff
 *   (1d) Retreat to happy grids
 *   (1e) Back away from danger
 *   (1f) Heal various conditions
 *
 *   (2) Attack
 *   (2a) Simulate possible attacks
 *   (2b) Perform optimal attack
 *
 *   (3) Recover
 *   (3a) Recover by spells/prayers
 *   (3b) Recover by items/etc
 *   (3c) Recover by resting
 *
 * XXX XXX XXX
 * In certain situations, the "proper" course of action is to simply
 * attack a nearby monster, since often most of the danger is due to
 * a single monster which can sometimes be killed in a single blow.
 *
 * Actually, both "borg_caution()" and "borg_recover()" need to
 * be more intelligent, and should probably take into account
 * such things as nearby monsters, and/or the relative advantage
 * of simply pummeling nearby monsters instead of recovering.
 *
 * Note that invisible/offscreen monsters contribute to the danger
 * of an extended "region" surrounding the observation, so we will
 * no longer rest near invisible monsters if they are dangerous.
 *
 * XXX XXX XXX
 * We should perhaps reduce the "fear" values of each region over
 * time, to take account of obsolete invisible monsters.
 *
 * Note that walking away from a fast monster is counter-productive,
 * since the monster will often just follow us, so we use a special
 * method which allows us to factor in the speed of the monster and
 * predict the state of the world after we move one step.  Of course,
 * walking away from a spell casting monster is even worse, since the
 * monster will just get to use the spell attack multiple times.  But,
 * if we are trying to get to known safety, then fleeing in such a way
 * might make sense.  Actually, this has been done too well, note that
 * it makes sense to flee some monsters, if they "stumble", or if we
 * are trying to get to stairs.  XXX XXX XXX
 *
 * Note that the "flow" routines attempt to avoid entering into
 * situations that are dangerous, but sometimes we do not see the
 * danger coming, and then we must attempt to survive by any means.
 *
 * We will attempt to "teleport" if the danger in the current situation,
 * as well as that resulting from attempting to "back away" from danger,
 * are sufficient to kill us in one or two blows.  This allows us to
 * avoid teleportation in situations where simply backing away is the
 * proper course of action, for example, when standing next to a nasty
 * stationary monster, but also to teleport when backing away will not
 * reduce the danger sufficiently.
 *
 * But note that in "nasty" situations (when we are running out of light,
 * or when we are starving, blind, confused, or hallucinating), we will
 * ignore the possibility of "backing away" from danger, when considering
 * the possibility of using "teleport" to escape.  But if the teleport
 * fails, we will still attempt to "retreat" or "back away" if possible.
 *
 * XXX XXX XXX Note that it should be possible to do some kind of nasty
 * "flow" algorithm which would use a priority queue, or some reasonably
 * efficient normal queue stuff, to determine the path which incurs the
 * smallest "cumulative danger", and minimizes the total path length.
 * It may even be sufficient to treat each step as having a cost equal
 * to the danger of the destination grid, plus one for the actual step.
 * This would allow the Borg to prefer a ten step path passing through
 * one grid with danger 10, to a five step path, where each step has
 * danger 9.  Currently, he often chooses paths of constant danger over
 * paths with small amounts of high danger.  However, the current method
 * is very fast, which is certainly a point in its favor...
 *
 * When in danger, attempt to "flee" by "teleport" or "recall", and if
 * this is not possible, attempt to "heal" damage, if needed, and else
 * attempt to "flee" by "running".
 *
 * XXX XXX XXX Both "borg_caution()" and "borg_recover()" should only
 * perform the "healing" tasks if they will cure more "damage"/"stuff"
 * than may be re-applied in the next turn, this should prevent using
 * wimpy healing spells next to dangerous monsters, and resting to regain
 * mana near a mana-drainer.
 *
 * Whenever we are in a situation in which, even when fully healed, we
 * could die in a single round, we set the "goal_fleeing" flag, and if
 * we could die in two rounds, we set the "goal_leaving" flag.
 *
 * In town, whenever we could die in two rounds if we were to stay still,
 * we set the "goal_leaving" flag.  In combination with the "retreat" and
 * the "back away" code, this should allow us to leave town before getting
 * into situations which might be fatal.
 *
 * Flag "goal_fleeing" means get off this level right now, using recall
 * if possible when we get a chance, and otherwise, take stairs, even if
 * it is very dangerous to do so.
 *
 * Flag "goal_leaving" means get off this level when possible, using
 * stairs if possible when we get a chance.
 *
 * We will also take stairs if we happen to be standing on them, and we
 * could die in two rounds.  This is often "safer" than teleportation,
 * and allows the "retreat" code to retreat towards stairs, knowing that
 * once there, we will leave the level.
 *
 * If we can, we should try to hit a monster with an offset  spell.
 * A Druj can not move but they are really dangerous.  So we should retreat
 * to a happy grid (meaning we have los and it does not), we should target
 * one space away from the bad guy then blast away with ball spells.
 */
bool borg_caution(void)
{
	int j, p;
	bool borg_surround = FALSE;
	bool nasty = FALSE;

	map_block *mb_ptr = map_loc(c_x, c_y);

	/*** Notice "nasty" situations ***/

	/* About to run out of light is extremely nasty */
	if (!bp_ptr->britelite && equipment[EQUIP_LITE].timeout < 250) nasty = TRUE;

	/* Starvation is nasty */
	if (bp_ptr->status.weak) nasty = TRUE;

	/* Blind-ness is nasty */
	if (bp_ptr->status.blind) nasty = TRUE;

	/* Confusion is nasty */
	if (bp_ptr->status.confused) nasty = TRUE;

	/* Hallucination is nasty */
	if (bp_ptr->status.image) nasty = TRUE;


	/*** Evaluate local danger ***/

	/* am I fighting a unique or a summoner, or scaryguy? */
	borg_near_monster_type(bp_ptr->max_lev < 15 ? MAX_SIGHT : 12);
	borg_surround = borg_surrounded();


	/*
	 * Only allow three 'escapes' per level
	 * unless fighting a unique, then allow 7.
	 */
	if ((borg_escapes > 3 && !unique_on_level) || borg_escapes > 7)
	{
		/* No leaving if going after questors */
		if (bp_ptr->depth <= 98)
		{
			/* Start leaving */
			if (!goal_leaving)
			{
				/* Note */
				borg_note("# Leaving (Too many escapes)");

				/* Start leaving */
				goal_leaving = TRUE;
			}

			/* Start fleeing */
			if (!goal_fleeing && borg_escapes > 3)
			{
				/* Note */
				borg_note("# Fleeing (Too many escapes)");

				/* Start fleeing */
				goal_fleeing = TRUE;
			}
		}
	}

	/* No hanging around if nasty here. */
	if (scaryguy_on_level)
	{
		/* Start leaving */
		if (!goal_leaving)
		{
			/* Note */
			borg_note("# Leaving (Scary guy on level)");

			/* Start leaving */
			goal_leaving = TRUE;
		}

		/* Start fleeing */
		if (!goal_fleeing)
		{
			/* Note */
			borg_note("# Fleeing (Scary guy on level)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}
	}

	/* Look around */
	p = borg_danger(c_x, c_y, 1, TRUE);

	/* Describe (briefly) the current situation */
	/* Danger (ignore stupid "fear" danger) */
	if (borg_goi || (p > avoidance / 10) || (p > mb_ptr->fear))
	{
		/* Describe (briefly) the current situation */
		borg_note
			("# Loc:%d,%d Dep:%d Lev:%d HP:%d/%d SP:%d/%d Danger:p=%d",
			 c_x, c_y, bp_ptr->depth, bp_ptr->lev,
			 bp_ptr->chp, bp_ptr->mhp, bp_ptr->csp, bp_ptr->msp, p);
		if (borg_goi)
		{
			borg_note
				("# Protected by GOI (borg turns:%d; game turns:%d)",
				 borg_goi / borg_game_ratio, p_ptr->tim.invuln);
		}
		if (borg_shield)
		{
			borg_note("# Protected by Mystic Shield");
		}
		if (borg_prot_from_evil)
		{
			borg_note("# Protected by PFE");
		}
	}
	/* Comment on glyph */
	if (track_glyph_num)
	{
		int i;
		for (i = 0; i < track_glyph_num; i++)
		{
			/* Enqueue the grid */
			if ((track_glyph_y[i] == c_y) && (track_glyph_x[i] == c_x))
			{
				/* if standing on one */
				borg_note("# Standing on Glyph");
			}
		}
	}
	/* Comment on stair */
	if (track_less_num)
	{
		int i;
		for (i = 0; i < track_less_num; i++)
		{
			/* Enqueue the grid */
			if ((track_less_y[i] == c_y) && (track_less_x[i] == c_x))
			{
				/* if standing on one */
				borg_note("# Standing on up-stairs");
			}
		}
	}
	/* Comment on stair */
	if (track_more_num)
	{
		int i;
		for (i = 0; i < track_more_num; i++)
		{
			/* Enqueue the grid */
			if ((track_more_y[i] == c_y) && (track_more_x[i] == c_x))
			{
				/* if standing on one */
				borg_note("# Standing on dn-stairs, (%d, %d)", c_x, c_y);
			}
		}
	}

	/* If the borg has healing spells */
	if (borg_has_realm(REALM_LIFE) || borg_has_realm(REALM_NATURE))
	{
		/* try healing before running away */
		if (borg_heal(p))
			return (TRUE);

		/* do some defence before running away! */
		if (borg_defend(p))
			return (TRUE);
	}
	else
	{
		/* do some defence before running away */
		if (borg_defend(p))
			return (TRUE);

		/* try healing before running away */
		if (borg_heal(p))
			return (TRUE);
	}

	/* If I am waiting for recall,  & safe, then stay put. */
	if (goal_recalling && borg_check_rest() &&
		bp_ptr->depth && borg_on_safe_feat(map_loc(c_x, c_y)->feat))
	{
		/* note the resting */
		borg_note("# Resting here, waiting for Recall.");

		/* rest here until lift off */
		borg_keypress('R');
		borg_keypress('\n');

		return (TRUE);
	}

	/* If I am waiting for recall in town */
	if (goal_recalling && goal_recalling <= (borg_game_ratio * 2) &&
		!bp_ptr->depth)
	{
		/* Cast GOI just before returning to dungeon */
		if (!borg_goi &&
			(borg_spell_fail(REALM_LIFE, 3, 7, 15) ||
			 borg_spell_fail(REALM_SORCERY, 3, 7, 15)))
		{
			borg_note("# Casting GOI before Recall activates.");
			return (TRUE);
		}

		/* Cast PFE just before returning to dungeon */
		if (!borg_prot_from_evil &&
			borg_spell_fail(REALM_LIFE, 1, 5, 15))
		{
			borg_note("# Casting PFE before Recall activates.");
			return (TRUE);
		}

		/* Cast other good prep things */
		if ((!borg_speed && borg_spell_fail(REALM_SORCERY, 1, 5, 15)) ||
			(my_oppose_fire + my_oppose_cold + my_oppose_acid +
			 my_oppose_elec + my_oppose_pois < 3 &&
			 (borg_spell_fail(REALM_NATURE, 2, 3, 15) ||
			  borg_spell_fail(REALM_NATURE, 0, 6, 15))) ||
			(my_oppose_fire + my_oppose_cold + my_oppose_elec < 2 &&
			 borg_spell_fail(REALM_NATURE, 0, 6, 15)) ||
			(!borg_shield && !borg_goi &&
			 borg_spell_fail(REALM_NATURE, 2, 2, 15)) || (!borg_hero &&
														  borg_spell_fail
														  (REALM_SORCERY, 7, 0,
														   15)) ||
			(!borg_berserk && borg_spell_fail(REALM_DEATH, 2, 0, 15)) ||
			(!borg_bless && borg_spell_fail(REALM_LIFE, 0, 2, 15)) ||
			(!borg_speed && borg_mindcr_fail(MIND_ADRENALINE, 35, 15)) ||
			(!borg_hero && borg_mindcr_fail(MIND_ADRENALINE, 35, 15)))
		{
			borg_note("# Casting preparatory spell before Recall activates.");
			return (TRUE);
		}

	}

	/*** Danger ***/

	/* Impending doom */
	/* Don't take off in the middle of a fight */
	/* just to restock and it is useless to restock */
	/* if you have just left town. */
	if (borg_restock(bp_ptr->depth) &&
		!borg_fighting_unique && (borg_time_town + (borg_t - borg_began)) > 200)
	{
		/* Start leaving */
		if (!goal_leaving)
		{
			/* Note */
			borg_note
				("# Leaving (restock) %s", borg_restock(bp_ptr->depth));

			/* Start leaving */
			goal_leaving = TRUE;
		}
		/* Start fleeing */
		if (!goal_fleeing && (bp_ptr->able.ccw < 2))
		{
			/* Flee */
			borg_note
				("# Fleeing (restock) %s", borg_restock(bp_ptr->depth));

			/* Start fleeing */
			goal_fleeing = TRUE;
		}
	}
	/* Excessive danger */
	else if (p > (bp_ptr->chp * 2))
	{
		/* Start fleeing */
		if (!goal_fleeing && !borg_fighting_unique &&
			(bp_ptr->lev < 50) && !vault_on_level && (bp_ptr->depth < 100))
		{
			/* Note */
			borg_note("# Fleeing (excessive danger)");

			/* Start fleeing */
			goal_fleeing = TRUE;
		}
	}
	/* Potential danger (near death) in town */
	else if (!bp_ptr->depth && (p > bp_ptr->chp) && (bp_ptr->lev < 50))
	{
		/* Flee now */
		if (!goal_leaving)
		{
			/* Flee! */
			borg_note("# Leaving (potential danger)");

			/* Start leaving */
			goal_leaving = TRUE;
		}
	}


	/*** Stairs ***/

	/* Leaving or Fleeing, take stairs */
	if (goal_leaving || goal_fleeing || scaryguy_on_level)
	{
		/* Take next stairs */
		stair_less = goal_fleeing;

		if (scaryguy_on_level) stair_less = TRUE;

		/*
		 * Only go down if fleeing or prepared,
		 * but not when starving, or lacking food
		 */
		stair_more = goal_fleeing;

		if (borg_prepared_depth() > bp_ptr->depth) stair_more = TRUE;

		/* Its ok to go one level deep if evading scary guy */
		if (scaryguy_on_level) stair_more = TRUE;

		if (!bp_ptr->cur_lite || bp_ptr->status.hungry ||
			bp_ptr->status.weak || (bp_ptr->food < 2))
			stair_more = FALSE;

		/* if fleeing town, then dive */
		if (!bp_ptr->depth) stair_more = TRUE;


	}

	/* Take stairs up */
	if (stair_less)
	{
		/* Current grid */
		map_block *mb_ptr = map_loc(c_x, c_y);

		/* Usable stairs */
		if (mb_ptr->feat == FEAT_LESS)
		{
			borg_keypress('<');

			/* Success */
			return (TRUE);
		}
	}

	/* Take stairs down */
	if (stair_more && !goal_recalling)
	{
		/* Current grid */
		map_block *mb_ptr = map_loc(c_x, c_y);

		/* Usable stairs */
		if (mb_ptr->feat == FEAT_MORE)
		{
			/* Cast GOI just before returning to dungeon */
			if (bp_ptr->csp > bp_ptr->msp * 6 / 10 &&
				!borg_goi &&
				(borg_spell_fail(REALM_LIFE, 3, 7, 15) ||
				 borg_spell_fail(REALM_SORCERY, 3, 7, 15)))
			{
				borg_note("# Casting GOI before taking stairs.");
				return (TRUE);
			}

			/* Cast PFE just before returning to dungeon */
			if (bp_ptr->csp > bp_ptr->msp * 6 / 10 &&
				!borg_prot_from_evil && borg_spell_fail(REALM_LIFE, 1, 5, 15))
			{
				borg_note("# Casting PFE before taking stairs.");
				return (TRUE);
			}

			/* Cast other good prep things */
			if ((bp_ptr->csp > bp_ptr->msp * 6 / 10) &&
				((!borg_speed && borg_spell_fail(REALM_SORCERY, 1, 5, 15)) ||
				 (my_oppose_fire + my_oppose_cold + my_oppose_acid +
				  my_oppose_elec + my_oppose_pois < 3 &&
				  (borg_spell_fail(REALM_NATURE, 2, 3, 15) ||
				   borg_spell_fail(REALM_NATURE, 0, 6, 15))) ||
				 (my_oppose_fire + my_oppose_cold + my_oppose_elec < 2 &&
				  borg_spell_fail(REALM_NATURE, 0, 6, 15)) ||
				 (!borg_shield && !borg_goi &&
				  borg_spell_fail(REALM_NATURE, 2, 2, 15)) || (!borg_hero &&
															   borg_spell_fail
															   (REALM_SORCERY,
																7, 0, 15)) ||
				 (!borg_berserk && borg_spell_fail(REALM_DEATH, 2, 0, 15)) ||
				 (!borg_bless && borg_spell_fail(REALM_LIFE, 0, 2, 15)) ||
				 (!borg_speed && borg_mindcr_fail(MIND_ADRENALINE, 35, 15)) ||
				 (!borg_hero && borg_mindcr_fail(MIND_ADRENALINE, 35, 15))))

			{
				borg_note("# Casting preparatory spell before taking stairs.");
				return (TRUE);
			}

			/* Take the stairs */
			borg_keypress('>');

			/* If the borg leaves the wilderness */
			if (!bp_ptr->depth) borg_leave_surface();

			/* Success */
			return (TRUE);
		}
	}


	/*** Deal with critical situations ***/

	/* Hack -- require light */
	if (!bp_ptr->britelite)
	{
		list_item *l_ptr = look_up_equip_slot(EQUIP_LITE);

		/* If the borg manages to refuel */
		if (borg_refuel()) return (TRUE);

		/* Flee for fuel */
		if (bp_ptr->depth && (!l_ptr || l_ptr->timeout < 1000))
		{
			/* Start leaving */
			if (!goal_leaving)
			{
				/* Flee */
				borg_note("# Leaving (need fuel)");

				/* Start leaving */
				goal_leaving = TRUE;
			}
		}
	}

	/* Hack -- prevent starvation */
	if (bp_ptr->status.weak)
	{
		/* Attempt to satisfy hunger */
		if (borg_eat_food_any() ||
			borg_spell_fail(REALM_LIFE, 0, 7, 45) ||
			borg_spell_fail(REALM_ARCANE, 2, 6, 45) ||
			borg_spell_fail(REALM_NATURE, 0, 3, 45))
		{
			/* Success */
			return (TRUE);
		}

		/* Try to restore mana then cast the spell next round */
		if (borg_quaff_potion(SV_POTION_RESTORE_MANA)) return (TRUE);

		/* Flee for food */
		if (bp_ptr->depth)
		{
			/* Start leaving */
			if (!goal_leaving)
			{
				/* Flee */
				borg_note("# Leaving (need food)");

				/* Start leaving */
				goal_leaving = TRUE;
			}

			/* Start fleeing */
			if (!goal_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (need food)");

				/* Start fleeing */
				goal_fleeing = TRUE;
			}
		}
	}

	/* Prevent breeder explosions when low level */
	if (breeder_level && bp_ptr->lev < 15)
	{
		/* Start leaving */
		if (!goal_leaving)
		{
			/* Flee */
			borg_note("# Leaving (breeder level)");

			/* Start leaving */
			goal_leaving = TRUE;
		}

	}

	/*** Flee on foot ***/

	/* Desperation Head for stairs */
	/* If you are low level and near the stairs and you can */
	/* hop onto them in very few steps, try to head to them */
	/* out of desperation */
	if (track_less_num && (goal_fleeing || (p > avoidance && bp_ptr->lev < 35)))
	{
		int y, x, i;
		int b_j = -1;

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
		}
		/*
		 * If you are within a few (3) steps of the stairs
		 * and you can take some damage to get there
		 * go for it
		 */
		if ((b_j < 3) && (p < bp_ptr->chp))
		{
			borg_desperate = TRUE;
			if (borg_flow_stair_less(GOAL_FLEE))
			{
				/* Note */
				borg_note("# Desperate for Stairs (one)");

				borg_desperate = FALSE;
				return (TRUE);
			}
			borg_desperate = FALSE;
		}

		/* If you are next to steps of the stairs go for it */
		if (b_j <= 2)
		{
			borg_desperate = TRUE;
			if (borg_flow_stair_less(GOAL_FLEE))
			{
				/* Note */
				borg_note("# Desperate for Stairs (two)");

				borg_desperate = FALSE;
				return (TRUE);
			}
			borg_desperate = FALSE;
		}

		/* Low level guys tend to waste money reading the recall scrolls */
		if (b_j < 15 && scaryguy_on_level && bp_ptr->lev < 20)
		{
			borg_desperate = TRUE;
			if (borg_flow_stair_less(GOAL_FLEE))
			{
				/* Note */
				borg_note("# Desperate for Stairs (three)");

				borg_desperate = FALSE;
				return (TRUE);
			}
			borg_desperate = FALSE;
		}
	}


	/* Strategic retreat */
	/* Do not retreat if */
	/* 1) we are icky (poisoned, blind, confused etc */
	/* 2) we are boosting our avoidance because we are stuck */
	if ((p > avoidance / 3 && !nasty && !borg_no_retreat) ||
		(borg_surround && p != 0))
	{
		int d, b_d = -1;
		int r, b_r = -1;

		int b_x = c_x;
		int b_y = c_y;

		/* Scan the useful viewable grids */
		for (j = 1; j < borg_view_n; j++)
		{
			int x1 = c_x;
			int y1 = c_y;

			int x2 = borg_view_x[j];
			int y2 = borg_view_y[j];

			/* Cant if confused: no way to predict motion */
			if (bp_ptr->status.confused) continue;

			/* Require "floor" grids */
			if (!borg_cave_floor_bold(y2, x2)) continue;

			/* XXX -- Borgs in an unexplored hall (& with only a torch
			 * will always return FALSE for Happy Grids:
			 *
			 *  222222      Where 2 = unknown grid.  Borg has a torch.
			 *  2221.#      Borg will consider both the . and the 1
			 *     #@#      for a retreat from the C. But the . will be
			 *     #C#      false d/t adjacent wall to the east.  1 will
			 *     #'#      will be false d/t unknown grid to the west.
			 *              So he makes no attempt to retreat.
			 * However, the next function (backing away), allows him
			 * to back up to 1 safely.
			 *
			 * To play safer, the borg should not retreat to grids where
			 * he has not previously been.  This tends to run him into
			 * more monsters.  It is better for him to retreat to grids
			 * previously travelled, where the monsters are most likely
			 * dead, and the path is clear.  However, there is not (yet)
			 * tag for those grids.  Something like BORG_BEEN would work.
			 */

			/* Require "happy" grids (most of the time) */
			if (!borg_happy_grid_bold(x2, y2)) continue;

			/* Track "nearest" grid */
			if (b_r >= 0)
			{
				int ay = ((y2 > y1) ? (y2 - y1) : (y1 - y2));
				int ax = ((x2 > x1) ? (x2 - x1) : (x1 - x2));

				/* Ignore "distant" locations */
				if ((ax > b_r) || (ay > b_r)) continue;
			}

			/* Reset */
			r = 0;

			/* Simulate movement */
			while (1)
			{
				map_block *mb_ptr;

				/* Obtain direction */
				d = borg_goto_dir(x1, y1, x2, y2);

				/* Verify direction */
				if ((d == 0) || (d == 5)) break;

				/* Track distance */
				r++;

				/* Simulate the step */
				y1 += ddy[d];
				x1 += ddx[d];

				/* Bounds checking */
				if (!map_in_bounds(x1, y1)) break;

				/* Obtain the grid */
				mb_ptr = map_loc(x1, y1);

				/* Require floor */
				if (borg_cave_wall_grid(mb_ptr)) break;

				/* Require line of sight */
				if (!borg_los(x1, y1, x2, y2)) break;

				/* Check danger of that spot (over time) */
				if (!borg_surround &&
					borg_danger(x1, y1, r + 1, TRUE) >= p) break;

				/* make sure it is not dangerous to take the first step; unless surrounded. */
				if (r == 1)
				{
					/* Not surrounded */
					if (!borg_surround)
					{
						if (borg_danger(x2, y2, 1, TRUE) >= avoidance * 6 / 10)
							break;
					}
					else
						/* Surrounded, try to back-up */
					{
						if (borg_danger(x2, y2, 1, TRUE) >=
							(b_r <= 3 ? avoidance * 15 / 10 : avoidance))
							break;
					}
				}

				/* Skip monsters */
				if (mb_ptr->monster) break;

				/* Skip traps */
				if (mb_ptr->trap) break;
				
				/* Safe arrival */
				if ((x1 == x2) && (y1 == y2))
				{
					/* Save distance */
					b_r = r;

					/* Save location */
					b_x = x2;
					b_y = y2;

					/* Done */
					break;
				}
			}
		}

		/* Retreat */
		if (b_r >= 0)
		{
			/* Save direction */
			b_d = borg_goto_dir(c_x, c_y, b_x, b_y);

			/* Hack -- set goal */
			g_x = c_x + ddx[b_d];
			g_y = c_y + ddy[b_d];

			/* Note */
			borg_note
				("# Retreating to %d,%d (distance %d) via %d,%d (%d > %d)",
				 b_y, b_x, b_r, g_y, g_x, p, borg_danger(g_x, g_y, 2, TRUE));

			/* Strategic retreat */
			borg_keypress(I2D(b_d));

			/* Success */
			return (TRUE);
		}
	}

	/*** Escape if possible ***/

	/* Attempt to escape via spells */
	if (borg_escape(p))
	{
		/* increment the escapes this level counter */
		borg_escapes++;

		/* Success */
		return (TRUE);
	}

	/*** Back away ***/
	/* Do not back up if */
	/* 1) we are icky (poisoned, blind, confused etc */
	/* 2) we are boosting our avoidance because we are stuck */
	if ((p > avoidance / 3 && !nasty && !borg_no_retreat) ||
		(borg_surround && p != 0))
	{
		int i = -1, b_i = -1;
		int k = -1, b_k = -1;
		int f = -1, b_f = -1;

		/* Current danger */
		b_k = p;

		/* Fake the danger down if surounded so that he can move. */
		if (borg_surround) b_k = (b_k * 6 / 10);

		/* Check the freedom */
		b_f = borg_freedom(c_x, c_y);

		/* Attempt to find a better grid */
		for (i = 0; i < 8; i++)
		{
			int x = c_x + ddx_ddd[i];
			int y = c_y + ddy_ddd[i];

			map_block *mb_ptr;

			/* Bounds checking */
			if (!map_in_bounds(x, y)) continue;

			/* Access the grid */
			mb_ptr = map_loc(x, y);

			/* Cant if confused: no way to predict motion */
			if (bp_ptr->status.confused) continue;

			/* Skip walls/doors */
			if (borg_cave_wall_grid(mb_ptr)) continue;

			/* Skip monster grids */
			if (mb_ptr->monster) continue;

			/* MT - skip traps */
			if (mb_ptr->trap) continue;

			/* Extract the danger there */
			k = borg_danger(x, y, 2, TRUE);

			/* Skip higher danger */
			/* note: if surrounded, then b_k has been lowered. */
			if (b_k < k) continue;

			/* Check the freedom there */
			f = borg_freedom(x, y);

			/* Danger is the same */
			if (b_k == k)
			{
				/* If I am low level, reward backing-up if safe */
				if (bp_ptr->lev <= 3 &&
					(bp_ptr->chp < bp_ptr->mhp || bp_ptr->csp < bp_ptr->msp))
				{
					/* do consider the retreat */
				}
				/* Freedom of my grid is better than the next grid
				 * so stay put and fight.
				 */
				else if (b_f > f) continue;
				else
					continue;
			}

			/* Save the info */
			b_i = i;
			b_k = k;
			b_f = f;
		}

		/* Back away */
		if (b_i >= 0)
		{
			/* Hack -- set goal */
			g_x = c_x + ddx_ddd[b_i];
			g_y = c_y + ddy_ddd[b_i];

			/* Note */
			borg_note("# Backing up to %d,%d (%d > %d)",
						  g_x, g_y, p, borg_danger(g_x, g_y, 2, TRUE));

			/* Back away from danger */
			borg_keypress(I2D(ddd[b_i]));

			/* Success */
			return (TRUE);
		}

	}


	/*** Cures ***/

	/* cure confusion, second check, first (slightly different) in borg_heal */
	if (bp_ptr->status.confused)
	{
		if (bp_ptr->mhp - bp_ptr->chp >= 300 &&
			(borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_LIFE)))
		{
			return (TRUE);
		}
		if (borg_eat_food(SV_FOOD_CURE_CONFUSION) ||
			borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			borg_quaff_crit(FALSE) ||
			borg_quaff_potion(SV_POTION_HEALING) ||
			borg_use_staff_fail(SV_STAFF_HEALING))
		{
			return (TRUE);
		}
	}

	/* Cure hallucination as soon as possible! */
	if (bp_ptr->status.image &&
		(borg_quaff_potion(SV_POTION_CURING) ||
		 borg_use_staff(SV_STAFF_CURING) ||
		 borg_zap_rod(SV_ROD_CURING)))
	{
		/* Tried to stop the visions */
		return (TRUE);
	}

	/* Hack -- cure fear when afraid */
	if (bp_ptr->status.afraid &&
		(randint0(100) < 70 ||
		 ((borg_class == CLASS_WARRIOR) && !bp_ptr->able.missile)))
	{
		if (borg_spell_fail(REALM_LIFE, 0, 3, 100) ||
			borg_mindcr_fail(MIND_ADRENALINE, 23, 100) ||
			borg_quaff_potion(SV_POTION_BOLDNESS) ||
			borg_quaff_potion(SV_POTION_HEROISM) ||
			borg_quaff_potion(SV_POTION_BERSERK_STRENGTH) ||
			borg_activate(BORG_ACT_REMOVE_FEAR) ||
			borg_activate(BORG_ACT_HEROISM) ||
			borg_activate(BORG_ACT_BERSERKER) ||
			borg_mutation(MUT1_BERSERK) ||
			borg_racial(RACE_HALF_ORC) ||
			borg_racial(RACE_HALF_TROLL))
		{
			return (TRUE);
		}
	}


	/*** Note impending death XXX XXX XXX ***/

	/* Flee from low hit-points */
	if (((bp_ptr->chp < bp_ptr->mhp / 3) ||
		 ((bp_ptr->chp < bp_ptr->mhp / 2) &&
		  (bp_ptr->chp < (bp_ptr->lev * 3)))) &&
		(bp_ptr->able.ccw < 3) && !bp_ptr->able.heal)
	{
		/* Flee from low hit-points */
		if (bp_ptr->depth && (randint0(100) < 25))
		{
			/* Start leaving */
			if (!goal_leaving)
			{
				/* Flee */
				borg_note("# Leaving (low hit-points)");

				/* Start leaving */
				goal_leaving = TRUE;

			}
			/* Start fleeing */
			if (!goal_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (low hit-points)");

				/* Start fleeing */
				goal_fleeing = TRUE;
			}

		}
	}

	/* Flee from bleeding wounds or poison and no heals */
	if ((bp_ptr->status.cut || bp_ptr->status.poisoned) &&
		(bp_ptr->chp < bp_ptr->mhp / 2))
	{
		/* Flee from bleeding wounds */
		if (bp_ptr->depth && (randint0(100) < 25))
		{
			/* Start leaving */
			if (!goal_leaving)
			{
				/* Flee */
				borg_note("# Leaving (bleeding/posion)");

				/* Start leaving */
				goal_leaving = TRUE;
			}

			/* Start fleeing */
			if (!goal_fleeing)
			{
				/* Flee */
				borg_note("# Fleeing (bleeding/poison)");

				/* Start fleeing */
				goal_fleeing = TRUE;
			}
		}
	}

	/*
	 * Emergency check on healing.  Borg_heal has already been checked but
	 * but we did not use our ez_heal potions.  All other attempts to save
	 * ourself have failed.  Use the ez_heal if I have it.
	 */
	if (((bp_ptr->chp < bp_ptr->mhp / 10) ||
		 (!bp_ptr->able.teleport && !bp_ptr->able.escape &&
		  (bp_ptr->chp < bp_ptr->mhp / 4))) &&
		((p > bp_ptr->chp * 2) ||
		 ((p > bp_ptr->chp) && (bp_ptr->able.easy_heal > 5)) ||
		 ((p > bp_ptr->chp * 12 / 10) && (bp_ptr->mhp - bp_ptr->chp >= 400) &&
		  borg_fighting_unique && (bp_ptr->depth >= 85))) &&
		(borg_quaff_potion(SV_POTION_HEALING) ||
		 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
		 borg_quaff_potion(SV_POTION_LIFE)))
	{
		borg_note("# Using reserve EZ_Heal.");
		return (TRUE);
	}

	/* Hack -- use "recall" to flee if possible */
	if (goal_fleeing && bp_ptr->depth && (borg_recall()))
	{
		/* Note */
		borg_note("# Fleeing the level (recall)");

		/* Success */
		return (TRUE);
	}

	/* If I am waiting for recall,and in danger, buy time with
	 * phase and cure_anythings.
	 */
	if (goal_recalling && (p > avoidance * 2))
	{
		if (!bp_ptr->status.confused && !bp_ptr->status.blind &&
			bp_ptr->msp > 60 &&
			bp_ptr->csp < (bp_ptr->msp / 4) &&
			borg_quaff_potion(SV_POTION_RESTORE_MANA))
		{
			borg_note("# Buying time waiting for Recall.  Step 1.");
			return (TRUE);
		}

		if (borg_read_scroll(SV_SCROLL_PHASE_DOOR) ||
			borg_spell_fail(REALM_ARCANE, 0, 4, 25) ||
			borg_spell_fail(REALM_SORCERY, 0, 1, 25) ||
			borg_spell_fail(REALM_TRUMP, 0, 0, 25) ||
			borg_mindcr_fail(MIND_MINOR_DISP, 3, 35) ||
			borg_activate(BORG_ACT_PHASE_DOOR) ||
			borg_activate(BORG_ACT_TELEPORT) ||
			borg_zap_rod(SV_ROD_HEALING))
		{
			borg_note("# Buying time waiting for Recall.  Step 2.");
			return (TRUE);
		}

		if ((bp_ptr->mhp - bp_ptr->chp < 100) &&
			(borg_quaff_potion(SV_POTION_CURE_LIGHT) ||
			 borg_use_staff(SV_STAFF_CURE_LIGHT) ||
			 borg_quaff_potion(SV_POTION_CURE_SERIOUS) ||
			 borg_quaff_crit(FALSE)))
		{
			borg_note("# Buying time waiting for Recall.  Step 3.");
			return (TRUE);
		}

		if ((bp_ptr->mhp - bp_ptr->chp < 300) &&
			(borg_quaff_crit(FALSE) ||
			 borg_use_staff(SV_STAFF_CURING)))
		{
			borg_note("# Buying time waiting for Recall.  Step 4.");
			return (TRUE);
		}

		if ((bp_ptr->mhp - bp_ptr->chp >= 300) &&
			(borg_quaff_potion(SV_POTION_HEALING) ||
			 borg_quaff_potion(SV_POTION_STAR_HEALING) ||
			 borg_quaff_potion(SV_POTION_LIFE) ||
			 borg_quaff_crit(FALSE)))
		{
			borg_note("# Buying time waiting for Recall.  Step 5.");
			return (TRUE);
		}

	}



	/* if I am gonna die next round, and I have no way to escape
	 * use the unknown stuff (if I am low level).
	 */
	if (p > (bp_ptr->chp * 4) && bp_ptr->lev < 20 && !bp_ptr->msp)
	{
		if (borg_use_unknown()) return (TRUE);
		if (borg_quaff_unknown()) return (TRUE);
		if (borg_read_unknown()) return (TRUE);
		if (borg_eat_unknown()) return (TRUE);

	}



	/* Nothing */
	return (FALSE);
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
