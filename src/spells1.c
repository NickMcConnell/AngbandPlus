
/* File: spells1.c */

/*
 * Get colors and symbols for spell projections.  Projection effects on
 * terrain features, monsters, objects, and the character.  The projection
 * code.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Draw some projections in multi-hued colors.
 * -TY-, -EB-
 */
static byte mh_attr(void)
{
	switch (randint(9))
	{
		case 1:  return (TERM_RED);
		case 2:  return (TERM_GREEN);
		case 3:  return (TERM_BLUE);
		case 4:  return (TERM_YELLOW);
		case 5:  return (TERM_ORANGE);
		case 6:  return (TERM_VIOLET);
		case 7:  return (TERM_L_RED);
		case 8:  return (TERM_L_GREEN);
		case 9:  return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte plasma_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte hellfire_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_RED);
		case 2: return (TERM_L_RED);
		case 3: return (TERM_L_DARK);
	}

	return (TERM_WHITE);
}

static byte ice_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte lite_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_YELLOW);
		case 3: return (TERM_ORANGE);
	}

	return (TERM_WHITE);
}

static byte confu_color(void)
{
	switch (rand_int(5))
	{
		case 0: case 1: case 2: return (TERM_L_UMBER);
		case 3: return (TERM_UMBER);
		case 4: return (TERM_WHITE);
	}

	return (TERM_WHITE);
}

static byte grav_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_DARK);
		case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_WHITE);
}

static byte storm_color(void)
{
	switch (rand_int(7))
	{
		case 0: case 1: case 2: return (TERM_SLATE);
		case 3: return (TERM_BLUE);
		case 4: return (TERM_L_BLUE);
		case 5: return (TERM_YELLOW);
		case 6: return (TERM_WHITE);
	}

	return (TERM_WHITE);
}

static byte meteor_color(void)
{
	switch (rand_int(6))
	{
		case 0: case 1: return (TERM_L_DARK);
		case 2: return (TERM_WHITE);
		case 3: return (TERM_RED);
		case 4: return (TERM_ORANGE);
		case 5: return (TERM_YELLOW);
	}

	return (TERM_WHITE);
}

static byte orb_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_L_DARK);
		case 3: return (TERM_SLATE);
	}

	return (TERM_L_DARK);
}

static byte death_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: case 2: return (TERM_L_DARK);
		case 3: return (TERM_VIOLET);
	}

	return (TERM_L_DARK);
}

static byte mana_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_VIOLET);
		case 2: return (TERM_BLUE);
		case 3: return (TERM_RED);
	}

	return (TERM_VIOLET);
}


/*
 * Return a color to use for the bolt/ball spells
 */
byte spell_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_ROCK:         return (TERM_SLATE);
		case GF_SHOT:         return (TERM_SLATE);
		case GF_ARROW:        return (TERM_L_UMBER);
		case GF_MISSILE:      return (TERM_UMBER);
		case GF_PMISSILE:     return (TERM_GREEN);
		case GF_WHIP:         return (TERM_UMBER);
		case GF_HURT:         return (TERM_WHITE);

		case GF_ACID:         return (TERM_SLATE);
		case GF_ELEC:         return (TERM_BLUE);
		case GF_FIRE:         return (TERM_RED);
		case GF_COLD:         return (TERM_WHITE);
		case GF_POIS:         return (TERM_GREEN);

		case GF_PLASMA:       return (plasma_color());
		case GF_HELLFIRE:     return (hellfire_color());
		case GF_ICE:          return (ice_color());

		case GF_LITE_WEAK:    return (TERM_YELLOW);
		case GF_LITE:         return (lite_color());
		case GF_LITE_EXTRA:   return (lite_color());
		case GF_DARK_WEAK:    return (TERM_L_DARK);
		case GF_DARK:         return (TERM_L_DARK);
		case GF_MORGUL_DARK:  return (TERM_L_DARK);

		case GF_CONFUSION:    return (confu_color());
		case GF_SOUND:        return (TERM_YELLOW);
		case GF_SHARD:        return (TERM_UMBER);
		case GF_INERTIA:      return (TERM_L_WHITE);
		case GF_GRAVITY:      return (grav_color());
		case GF_FORCE:        return (TERM_UMBER);
		case GF_WATER:        return (TERM_SLATE);
		case GF_WIND:         return (TERM_WHITE);
		case GF_STORM:        return (storm_color());

		case GF_NEXUS:        return (TERM_L_RED);
		case GF_NETHER:       return (TERM_L_GREEN);
		case GF_CHAOS:        return (mh_attr());
		case GF_DISENCHANT:   return (TERM_VIOLET);
		case GF_TIME:         return (TERM_L_BLUE);
		case GF_MANA:         return (mana_color());

		case GF_METEOR:       return (meteor_color());
		case GF_HOLY_ORB:     return (orb_color());
		case GF_BLACK_ORB:    return (TERM_L_DARK);
		case GF_DEATH:        return (death_color());
		case GF_VAMP_DRAIN:   return (TERM_VIOLET);
		case GF_DEATH_CLOUD:  return (death_color());

		case GF_CURSE:        return (TERM_L_DARK);

		case GF_ENLIGHTENMENT:return (lite_color());
		case GF_GAIN_LEVEL:   return (TERM_L_GREEN);
		case GF_PROTECTION:   return (TERM_L_WHITE);

		case GF_MAKE_WATER:   return (TERM_BLUE);
		case GF_MAKE_TREES:   return (TERM_GREEN);
		case GF_MAKE_LAVA:    return (TERM_RED);
	}

	/* Standard "color" */
	return (TERM_WHITE);
}


/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
static u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
	int base;

	byte k;

	byte a;
	char c;

	/* No motion (*) */
	if ((ny == y) && (nx == x)) base = 0x30;

	/* Vertical (|) */
	else if (nx == x) base = 0x40;

	/* Horizontal (-) */
	else if (ny == y) base = 0x50;

	/* Diagonal (/) */
	else if ((ny - y) == (x - nx)) base = 0x60;

	/* Diagonal (\) */
	else if ((ny - y) == (nx - x)) base = 0x70;

	/* Weird (*) */
	else
		base = 0x30;

	/* Basic spell color */
	k = spell_color(typ);

	/* Obtain attr/char */
	a = misc_to_attr[base + k];
	c = misc_to_char[base + k];

	/* Create pict */
	return (PICT(a, c));
}


/*
 * Mega-Hack -- track "affected" monsters (see "project()" comments)
 */
static int project_m_n;
static int project_m_x;
static int project_m_y;

/*
 * Mega-Hack -- count number of monsters killed out of sight
 */
static int death_count;

/*
 * We are called from "project()" to alter terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * Hack -- We also "see" grids which are "memorized".
 *
 * Perhaps we should affect doors and/or walls.
 *
 * Handle special terrain.  Some terrain types that change the damage
 * done by various projections are marked (using the CAVE_TEMP flag) for
 * later processing.  This prevents a fire breath, for example, changing
 * floor to lava and then getting the damage bonuses that accrue to fire
 * spells on lava.  We use "dist" to keep terrain alteration under control.
 */
static bool project_f(int who, int y, int x, int dist, int dam, int typ)
{
	bool obvious = FALSE;

	/* Analyze the type */
	switch (typ)
	{
		/* Can eat at walls.  See "project_t()". */
		case GF_ACID:
		{
			/* Mark most rocky grids for (possible) later alteration. */
			if ((cave_feat[y][x] >= FEAT_RUBBLE) &&
			    (cave_feat[y][x] <= FEAT_WALL_SOLID) && (dist <= 4))
			{
				cave_temp_mark(y, x, FALSE);
			}
			break;
		}

		/* Can solidify lava.  See "project_t()". */
		case GF_COLD:
		case GF_ICE:
		{
			/* Mark the lava grid for (possible) later alteration. */
			if ((cave_feat[y][x] == FEAT_LAVA) && (dist <= 1))
				cave_temp_mark(y, x, FALSE);
			break;
		}

		/* Can burn, evaporate, and even make lava.  See "project_t()". */
		case GF_FIRE:
		case GF_HELLFIRE:
		case GF_PLASMA:
		{
			if (dist <= 1)
			{
				/* Mark the grid for (possible) later alteration. */
				cave_temp_mark(y, x, FALSE);
			}
			break;
		}

		/* Light up the grid */
		case GF_LITE_WEAK:
		case GF_LITE:
		case GF_LITE_EXTRA:
		{
			/* Turn on the light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Must not be blind */
				if (!p_ptr->blind)
				{
					/* Observe */
					obvious = TRUE;
				}

				/* Update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Darken the grid */
		case GF_DARK_WEAK:
		case GF_DARK:
		case GF_MORGUL_DARK:
		{
			/* Turn off the light */
			cave_info[y][x] &= ~(CAVE_GLOW);

			/* Forget */
			cave_info[y][x] &= ~(CAVE_MARK);

			/* Re-learn sometimes */
			note_spot(y, x);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Can make pools and solidify lava.  See "project_t()". */
		case GF_WATER:
		case GF_STORM:
		{
			if (dist <= 1)
			{
				/* Mark the floor grid for (possible) later alteration. */
				if ((cave_feat[y][x] == FEAT_FLOOR) ||
				    (cave_feat[y][x] == FEAT_LAVA))
				{
					cave_temp_mark(y, x, FALSE);
				}
			}
			break;
		}


		/* Chaos can create chaos tiles (rarely) */
		case GF_CHAOS:
		{
			if ((cave_floor_bold(y, x)) && (p_ptr->depth > 40) &&
			    (rand_range(200, 4200) < dam))
			{
				/* Mark the grid for later summoning. */
				cave_temp_mark(y, x, FALSE);
			}
			break;
		}

		/* Chaos can create mana flies (rarely) */
		case GF_MANA:
		{
			/* The character can control his mana */
			if (who <= 0)
			{
				if ((cave_floor_bold(y, x)) && (p_ptr->depth > 30) &&
					 (rand_range(200, 4200) < dam))
				{
					/* Mark the grid for later summoning. */
					cave_temp_mark(y, x, FALSE);
				}
			}
			break;
		}

		/* Destroy Traps (and Locks) */
		case GF_KILL_TRAP:
		{
			/* Attempt to disarm traps (95% chance of success) */
			if (cave_info[y][x] & (CAVE_TRAP))
			{
				if (magic_disarm(y, x, 95)) obvious = TRUE;
			}

			/* Secret / Locked doors are (always) found and unlocked */
			if ((cave_feat[y][x] == FEAT_SECRET) ||
				 ((cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x01) &&
				  (cave_feat[y][x] <= FEAT_DOOR_HEAD + 0x07)))
			{
				/* Unlock the door */
				place_unlocked_door(y, x);

				/* Check line of sound */
				if (player_has_los_bold(y, x))
				{
					msg_print("Click!");
					obvious = TRUE;
				}
			}

			break;
		}

		/* Destroy Doors */
		case GF_KILL_DOOR:
		{
			int feat = cave_feat[y][x];

			/* Destroy all doors.  Traps are not affected */
			if ((feat == FEAT_OPEN) || (feat == FEAT_BROKEN) ||
			    (cave_closed_door(y, x)))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("There is a bright flash of light!");
					obvious = TRUE;

					/* Visibility change */
					if (cave_closed_door(y, x))
					{
						/* Update the visuals */
						p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
					}
				}

				/* Forget the door */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			break;
		}

		/* Destroy Doors */
		case GF_FORCE_DOOR:
		{
			/* Affect closed doors */
			if (cave_closed_door(y, x))
			{
				int feat = cave_feat[y][x];
				bool closed = FALSE;
				if (cave_closed_door(y, x)) closed = TRUE;

				/* Door is secret -- instantiate it */
				if (feat == FEAT_SECRET) place_closed_door(y, x);

				/* Get door again */
				feat = cave_feat[y][x];

				/* Door is closed, not locked or jammed */
				if (feat == FEAT_DOOR_HEAD)
				{
					if (player_has_los_bold(y, x))
					{
						/* Message */
						msg_print("The door is jammed shut.");
						obvious = TRUE;
					}

					/* Jam the door */
					cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + randint(3));
				}

				/* Door is locked or jammed */
				else
				{
					/* Get door power XXX XXX  */
					int power = (feat - FEAT_DOOR_HEAD) % 8;

					/* Attempt to unbar the door */
					if (randint(power) <= 3)
					{
						/* Forget the door */
						cave_info[y][x] &= ~(CAVE_MARK);

						/* Door may become closed, open, or broken */
						if ((feat < FEAT_DOOR_HEAD + 0x08) && (one_in_(2)))
						{
							if (distance(y, x, p_ptr->py, p_ptr->px) <= 3)
							{
								msg_print("You hear a click.");
								if (player_has_los_bold(y, x)) obvious = TRUE;
							}
							cave_set_feat(y, x, FEAT_DOOR_HEAD);
						}
						else if (!one_in_(power))
						{
							if (feat < FEAT_DOOR_HEAD + 0x08)
							{
								if (player_has_los_bold(y, x))
								{
									msg_print("The door swings open.");
									obvious = TRUE;
								}
							}
							else
							{
								if (player_has_los_bold(y, x))
								{
									msg_print("The door crashes open!");
									obvious = TRUE;
								}
							}
							cave_set_feat(y, x, FEAT_OPEN);
						}
						else
						{
							if (player_has_los_bold(y, x))
							{
								msg_print("The door is smashed off its hinges!");
								obvious = TRUE;
							}
							cave_set_feat(y, x, FEAT_BROKEN);
						}
					}

					/* Visibility change */
					if (closed)
					{
						/* Update the visuals */
						p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
					}
				}
			}

			/* Close and jam open doors */
			else if (cave_feat[y][x] == FEAT_OPEN)
			{
				if ((cave_m_idx[y][x]) || (cave_o_idx[y][x]))
				{
					/* Failure */
					msg_print("Something is in the way!");
					break;
				}

				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("The door slams shut!");
					obvious = TRUE;

					/* Update the visuals */
					p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
				}

				/* Close and jam the door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + randint(3));
			}

			break;
		}

		/* Magical door jamming */
		case GF_JAM_DOOR:
		{
			/* Require closed door (not secret XXX) */
			if ((cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
				(cave_feat[y][x] <= FEAT_DOOR_TAIL))
			{
				/* Check line of sight */
				if (player_has_los_bold(y, x))
				{
					/* Message */
					msg_print("Magical spikes slam into the door.");
					obvious = TRUE;
				}
				do_cmd_spike_aux(y, x);
			}
			break;
		}

		/* Destroy walls, rubble, and doors */
		case GF_KILL_WALL:
		case GF_DISINTEGRATE:
		{
			/* Non-walls (etc) */
			if (cave_floor_bold(y, x)) break;

			/* Trees are unaffected.  XXX */
			if (cave_feat[y][x] == FEAT_TREE) break;

			/* Permanent walls and stores are immune. */
			if (cave_feat[y][x] >= FEAT_PERM_EXTRA) break;

			/* Granite */
			if (cave_feat[y][x] >= FEAT_WALL_EXTRA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					if (typ != GF_DISINTEGRATE)
						msg_print("The wall turns into mud.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Quartz / Magma with treasure */
			else if (cave_feat[y][x] >= FEAT_MAGMA_H)
			{
				/* Message */
				if ((cave_info[y][x] & (CAVE_MARK)) &&
			       (typ != GF_DISINTEGRATE))
				{
					msg_print("The vein turns into mud.");
					msg_print("You have found something!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Place some gold */
				place_gold(y, x);
			}

			/* Quartz / Magma */
			else if (cave_feat[y][x] >= FEAT_MAGMA)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					if (typ != GF_DISINTEGRATE)
						msg_print("The vein turns into mud.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Rubble */
			else if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					if (typ != GF_DISINTEGRATE)
						msg_print("The rubble turns into mud.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the rubble */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Hack -- place an object. */
				if ((typ != GF_DISINTEGRATE) && (one_in_(100)))
				{
					/* Found something */
					if (player_can_see_bold(y, x))
					{
						msg_print("There was something buried in the rubble!");
						obvious = TRUE;
					}

					/* Place object */
					place_object(y, x, FALSE, FALSE, FALSE);
				}
			}

			/* Destroy doors (and secret doors) */
			else if (cave_feat[y][x] >= FEAT_DOOR_HEAD)
			{
				/* Hack -- special message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					if (typ != GF_DISINTEGRATE)
						msg_print("The door turns into mud!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the feature */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			break;
		}

		/* Make doors */
		case GF_MAKE_DOOR:
		{
			/* XXX - Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) break;

			/* Create closed door */
			cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);

			/* Observe */
			if (cave_info[y][x] & (CAVE_MARK)) obvious = TRUE;

			/* Update the visuals */
			p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

			break;
		}

		/* Make traps */
		case GF_MAKE_TRAP:
		{
			/* Require a passable floor grid */
			if (!cave_passable_bold(y, x)) break;

			/* Require that grid be free of "nasty" traps */
			if (nasty_traps(y, x, 0)) break;

			/* Place a trap (a nasty one, of course!) */
			place_trap(y, x, -1, p_ptr->depth);

			break;
		}

		/* Learn about a lot of things */
		case GF_ENLIGHTENMENT:
		{
			/* Learn about traps and loose rocks */
			if (cave_invisible_trap(y, x))
			{
				(void)reveal_trap(y, x, 50, TRUE, FALSE);
			}

			/* Learn about secret doors */
			if (cave_feat[y][x] == FEAT_SECRET) place_closed_door(y, x);

			/* Perma Light */
			cave_info[y][x] |= (CAVE_GLOW);

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
			}

			/* No damage */
			dam = 0;

			break;
		}

		/* Create glyphs of warding (rarely) */
		case GF_PROTECTION:
		{
			/* Must be a passable grid */
			if (cave_passable_bold(y, x))
			{
				/* Limit damage */
				if (dam > 200) dam = 200;

				/* Must not have too many glyphs on the level XXX */
				if (num_glyph_on_level < MAX_GLYPHS)
				{
					/* Must pass a (fairly stringent) rarity check */
					if (dam > rand_range(50, 600))
					{
						/* Create a glyph */
						place_trap(y, x, TRAP_GLYPH, 0);
					}
				}
			}

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Make water */
		case GF_MAKE_WATER:
		{
			/* Change to water */
			cave_set_feat(y, x, FEAT_WATER);
			obvious = TRUE;

			break;
		}

		/* Make water */
		case GF_MAKE_TREES:
		{
			/* Change to trees */
			cave_set_feat(y, x, FEAT_TREE);
			obvious = TRUE;

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
			}

			break;
		}

		/* Make lava */
		case GF_MAKE_LAVA:
		{
			/* Change to lava */
			cave_set_feat(y, x, FEAT_LAVA);
			obvious = TRUE;

			break;
		}

		/* Ignore most effects */
		default:
		{
			break;
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * We are called from "project()" to "damage" objects
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * Hack -- We also "see" objects which are "memorized".
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int y, int x, int dam, int typ)
{
	s16b this_o_idx, next_o_idx = 0;

	bool obvious = FALSE;

	char o_name[120];

	u32b f1, f2, f3;


	/* "Who" is currently unused */
	who = 0;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		bool is_art = FALSE;
		bool ignore = FALSE;
		bool plural = FALSE;

		bool do_move = FALSE;
		bool do_kill = FALSE;
		cptr note_kill = NULL;
		bool do_change = FALSE;
		cptr note_change = NULL;

		int ny, nx;


		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Note multiple objects */
		if (o_ptr->number > 1) plural = TRUE;

		/* Check for artifact */
		if (artifact_p(o_ptr)) is_art = TRUE;

		/* Analyze the type */
		switch (typ)
		{
			/* Acid -- Lots of things */
			case GF_ACID:
			{
				if (hates_acid(o_ptr) && dam > rand_int(50))
				{
					do_kill = TRUE;
					note_kill = (plural ? " melt!" : " melts!");
					if (f2 & (TR2_IGNORE_ACID)) ignore = TRUE;
				}
				break;
			}

			/* Elec -- Rings, Wands, Rods */
			case GF_ELEC:
			{
				if (hates_elec(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f2 & (TR2_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire -- Flammable objects */
			case GF_FIRE:
			{
				if (hates_fire(o_ptr) && dam > rand_int(40))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
				}
				break;
			}

			/* Cold -- potions and flasks */
			case GF_COLD:
			{
				if (hates_cold(o_ptr) && dam > rand_int(40))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
					if (f2 & (TR2_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Elec */
			case GF_PLASMA:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(40)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
				}
				if (hates_elec(o_ptr) && (dam > rand_int(40)))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
					if (f2 & (TR2_IGNORE_ELEC)) ignore = TRUE;
				}
				break;
			}

			/* Fire + Cold */
			case GF_METEOR:
			{
				if (hates_fire(o_ptr) && (dam > rand_int(80)))
				{
					do_kill = TRUE;
					note_kill = (plural ? " burn up!" : " burns up!");
					if (f2 & (TR2_IGNORE_FIRE)) ignore = TRUE;
				}
				if (hates_cold(o_ptr) && (dam > rand_int(80)))
				{
					ignore = FALSE;
					do_kill = TRUE;
					note_kill = (plural ? " shatter!" : " shatters!");
					if (f2 & (TR2_IGNORE_COLD)) ignore = TRUE;
				}
				break;
			}

			/* Hack -- break potions and such */
			case GF_ICE:
			case GF_SHARD:
			case GF_FORCE:
			case GF_SOUND:
			{
				if (hates_cold(o_ptr) && (dam > rand_int(40)))
				{
					note_kill = (plural ? " shatter!" : " shatters!");
					do_kill = TRUE;
				}
				break;
			}

			/* Wind blows things around */
			case GF_WIND:
			{
				if (o_ptr->weight <= dam)
				{
					nx = 0;
					ny = 0;

					scatter(&ny, &nx, y, x, 1 + (dam - o_ptr->weight) / 33, 0);
					if (ny != y || nx != x) do_move = TRUE;
				}
				break;
			}

			/* Chaos destroys and changes objects */
			case GF_CHAOS:
			{
				if (dam > rand_range(20, 150))
				{
					if (one_in_(2))
					{
						do_change = TRUE;
						note_change = (plural ? " change!" : " changes!");
					}
					else
					{
						do_kill = TRUE;
						note_kill = (plural ? " are destroyed!" : " is destroyed!");
					}
				}
				break;
			}


			/* It wouldn't be fun if disenchantment got too cute. */


			/* Mana -- activates scrolls and magical devices */
			case GF_MANA:
			{
				/* Note:  we never learn about devices this way */

				/* Require a certain amount of power */
				if ((dam > 10) && ((dam + 100) > rand_int(400)))
				{
					if (o_ptr->tval == TV_SCROLL)
					{
						/* Do not use up the scroll */
						(void)scroll_read_effect(0, y, x, o_ptr);
					}
					else if (is_magical_device(o_ptr))
					{
						(void)device_use_effect(0, rsqrt(dam * 10), y, x, o_ptr);
					}
				}
				break;
			}

			/* Holy Orb -- destroys cursed non-artifacts */
			case GF_HOLY_ORB:
			{
				if (cursed_p(o_ptr))
				{
					do_kill = TRUE;
					note_kill = (plural ? " are destroyed!" : " is destroyed!");
				}
				break;
			}

			/* Unlock chests */
			case GF_KILL_TRAP:
			{
				/* Chests are noticed only if trapped or locked */
				if (o_ptr->tval == TV_CHEST)
				{
					/* Disarm/Unlock traps */
					if (o_ptr->pval > 0)
					{
						/* Disarm or Unlock */
						o_ptr->pval = (0 - o_ptr->pval);

						/* Identify */
						object_known(o_ptr);

						/* Notice */
						if (o_ptr->marked)
						{
							msg_print("Click!");
							obvious = TRUE;
						}
					}
				}

				break;
			}

			/* Grab the top object in the grid */
			case GF_FETCH_OBJ:
			{
				s16b o_idx;

				/* Hack -- Ignore character's own grid */
				if ((y == p_ptr->py) && (x == p_ptr->px)) break;

				/* Get object index */
				o_idx = cave_o_idx[y][x];

				/* Require an object */
				if (!o_idx) break;

				/* Get the object */
				o_ptr = &o_list[o_idx];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

				/* Pick up gold */
				if (o_ptr->tval == TV_GOLD)
				{
					/* Message -- regular gold */
					if ((o_ptr->sval == SV_COPPER) ||
					    (o_ptr->sval == SV_SILVER) ||
						 (o_ptr->sval == SV_GOLD) ||
						 (o_ptr->sval == SV_PLATINUM) ||
						 (o_ptr->sval == SV_MITHRIL) ||
						 (o_ptr->sval == SV_ADAMANTITE))
					{
						msg_format("You have found %ld gold pieces worth of %s.",
							(long)o_ptr->pval, o_name);
					}

					/* Message -- regular gems */
					else if (o_ptr->sval < SV_SPECIAL_GOLD_MIN)
					{
						msg_format("You have found %s worth %ld gold pieces.",
							o_name, (long)o_ptr->pval);
					}

					/* Message -- special treasures */
					else
					{
						msg_format("You have found %s worth %ld gold pieces!",
							o_name, (long)o_ptr->pval);
					}

					/* Collect the gold */
					p_ptr->au += o_ptr->pval;

					/* Redraw gold */
					p_ptr->redraw |= (PR_GOLD);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

					/* Delete the gold */
					delete_object_idx(o_idx);
				}

				/* Ignore essences */
				else if (o_ptr->tval == TV_ESSENCE)
				{
					/* Do nothing */
				}

				/* Normal objects */
				else
				{
					/* Check the weight */
					if (o_ptr->weight * o_ptr->number <= dam)
					{
						object_type *i_ptr;
						object_type forge;

						/* Hack -- try to put ammo directly in the quiver */
						if (quiver_carry(o_ptr, this_o_idx)) break;

						/* Get local object */
						i_ptr = &forge;

						/* Wipe the new object */
						object_wipe(i_ptr);

						/* Make a copy */
						object_copy(i_ptr, o_ptr);

						/* Give the new object to the player */
						give_object(i_ptr);

						/* Delete the original object */
						delete_object_idx(o_idx);
					}
					else
					{
						msg_format("The %s %s too heavy to move.", o_name,
							(o_ptr->number > 1 ? "are" : "is"));
					}
				}
				break;
			}

			/* Learn about objects sometimes */
			case GF_ENLIGHTENMENT:
			{
				/* Allow any degree of knowledge */
				if (!one_in_(5))
				{
					object_aware(o_ptr);

					/* Never allow a perfect chance */
					if (dam >= rand_range(30, 30 + dam))
					{
						object_known(o_ptr);

						if (dam >= rand_range(80, 80 + dam))
						{
							object_mental(o_ptr);
						}
					}
				}

				break;
			}

			/* Al other kinds of projections have no effect */
			default:
			{
				break;
			}
		}


		/* Note that the object may not exist now  XXX XXX */

		/* Attempt to destroy the object (hack -- unless essence) */
		if ((do_kill) && (o_ptr->tval != TV_ESSENCE))
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Some objects resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					object_desc(o_name, o_ptr, FALSE, 0);
					msg_format("The %s %s unaffected!",
						   o_name, (plural ? "are" : "is"));
				}
			}

			/* Kill it */
			else
			{
				/* Describe if needed */
				if (o_ptr->marked && note_kill)
				{
					object_desc(o_name, o_ptr, FALSE, 0);
					msg_format("The %s%s", o_name, note_kill);
				}

				/* Potions produce effects when 'shattered' */
				if (o_ptr->tval == TV_POTION)
				{
					/* Do not learn anything  XXX XXX */
					(void)potion_smash_effect(0, y, x, o_ptr);
				}

				/* Delete the object */
				delete_object_idx(this_o_idx);

				/* Redraw */
				lite_spot(y, x);
			}
		}

		/* Attempt to change the object */
		else if (do_change)
		{
			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Some objects resist */
			if (is_art || ignore)
			{
				/* Observe the resist */
				if (o_ptr->marked)
				{
					object_desc(o_name, o_ptr, FALSE, 0);
					msg_format("The %s %s unaffected!",
						   o_name, (plural ? "are" : "is"));
				}
			}

			/* Change it (hack -- unless gold or essences) */
			else if ((o_ptr->tval != TV_GOLD) && (o_ptr->tval != TV_ESSENCE))
			{
				object_type *i_ptr;
				object_type forge;

				/* Get local object */
				i_ptr = &forge;

				/* Clear the record */
				WIPE(i_ptr, object_type);

				/* Require the same tval */
				required_tval = o_ptr->tval;

				/* Make a new object */
				make_object(i_ptr, FALSE, FALSE, TRUE);

				/* Cancel tval forcing */
				required_tval = 0;

				/* Require a valid new object */
				if (i_ptr->k_idx)
				{
					/* Describe original object if needed */
					if (o_ptr->marked && note_change)
					{
						object_desc(o_name, o_ptr, FALSE, 0);
						msg_format("The %s%s", o_name, note_change);
					}

					/* Delete the original object */
					delete_object_idx(this_o_idx);

					/* Drop the new object nearby */
					drop_near(i_ptr, -1, y, x);

					/* Redraw */
					lite_spot(y, x);
				}
			}
		}

		/* Move the object (hack -- unless essence) */
		if ((do_move) && (o_ptr->tval != TV_ESSENCE))
		{
			object_type *i_ptr;
			object_type forge;

			/* Get local object */
			i_ptr = &forge;

			/* Wipe the new object */
			object_wipe(i_ptr);

			object_copy(i_ptr, o_ptr);

			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Delete the object in its old location */
			delete_object_idx(this_o_idx);

			/* Redraw */
			lite_spot(y, x);

			/* Drop it near the new location */
			drop_near(i_ptr, -1, ny, nx);
		}
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a projection causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a location, via
 * integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.
 *
 * Certain terrain types affect spells.  -LM-
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.   If can breath nether, then it resists it as well.
 *
 * XXX XXX - For monsters, Morgul-dark is the same as darkness.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
static bool project_m(int who, int y, int x, int dam, int typ, u32b flg)
{
	int tmp;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	cptr name;

	int old_sleep;

	/* Adjustment to damage caused by terrain, if applicable. */
	int terrain_adjustment = 0;

	/* Is the monster "seen"? */
	bool seen = FALSE;
	bool fully_seen = FALSE;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;


	/* Polymorph setting (true or false) */
	int do_poly = 0;

	/* Slow setting (amount to slow) */
	int do_slow = 0;

	/* Confusion setting (amount to confuse) */
	int do_conf = 0;

	/* Stunning setting (amount to stun) */
	int do_stun = 0;

	/* Fear amount (amount to fear) */
	int do_fear = 0;

	/* Sleep amount (amount to sleep) */
	int do_sleep = 0;


	/* Hold the monster name */
	char m_name[80];

	/* Assume no note */
	cptr note = NULL;

	cptr note_dies;

	bool fear = FALSE;


	/* No monster here */
	if (!(cave_m_idx[y][x] > 0)) return (FALSE);

	/* Never affect projector */
	if (cave_m_idx[y][x] == who) return (FALSE);

	/* Obtain monster info */
	m_ptr = &m_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	name = (r_name + r_ptr->name);

	/* Paranoia -- This monster is already dead */
	if (m_ptr->hp < 0) return (FALSE);

	/* Hack -- Monster race can be immune */
	if (project_immune)
	{
		/* Skip monsters with the given racial index */
		if (project_immune == m_ptr->r_idx) return (FALSE);
	}

	/* Optionally - Only affect monsters with a specified flag */
	if (p_ptr->proj_mon_flags)
	{
		int i;
		bool hit = FALSE;

		/* Scan the flag set */
		for (i = 0; i < 32; i++)
		{
			/* We are looking for monsters with this flag */
			if (p_ptr->proj_mon_flags & (1L << i))
			{
				/* Monster has this flag */
				if (r_ptr->flags3 & (1L << i))
				{
					if (mon_fully_visible(m_ptr))
					{
						l_ptr->flags3 |= (1L << i);
					}

					hit = TRUE;
				}
			}
		}

		/* Monster has none of the flags for which we are looking */
		if (!hit) return (FALSE);
	}


	/* Get visibility */
	if (m_ptr->ml)
	{
		seen = TRUE;
		if (mon_fully_visible(m_ptr)) fully_seen = TRUE;
	}

	/* Some monsters are great at dodging  -EZ- */
	if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
	    (!m_ptr->confused))
	{
		/* Area-effect and jumping spells cannot be dodged */
		if (!(flg & (PROJECT_ARC | PROJECT_STAR | PROJECT_JUMP |
		             PROJECT_BOOM)))
		{
			/* Allow dodging */
			if (rand_int(5 + m_ptr->cdis) >= (2 + m_ptr->stunned / 10))
			{
				if (fully_seen)
				{
					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					msg_format("%^s dodges!", name);

					/* Learn that monster can dodge */
					l_ptr->flags2 |= (RF2_EVASIVE);
				}

				/* Missed! */
				return (TRUE);
			}
		}
	}


	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Most monsters "die" */
	note_dies = " dies.";

	/* Some monsters get "destroyed" */
	if (monster_nonliving(r_ptr))
	{
		note_dies = " is destroyed.";
	}


	/* Determine if terrain is capable of adjusting physical damage. */
	switch (cave_feat[y][x])
	{
		/* Monsters can duck behind rubble, or take only partial damage. */
		case FEAT_RUBBLE:
		{
			if ((!m_ptr->csleep) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
			    (one_in_(4)))
			{
				msg_format("%^s ducks behind a boulder!", m_name);
				return (FALSE);
			}
			else
			{
				terrain_adjustment -= dam / 4;
			}
			break;
		}

		/* Fire-based spells suffer, but water spells come into their own. */
		case FEAT_WATER:
		{
			if (typ == GF_FIRE)
			{
				terrain_adjustment -= dam / 2;
			}
			else if ((typ == GF_HELLFIRE) || (typ == GF_PLASMA))
			{
				terrain_adjustment -= dam / 4;
			}
			else if ((typ == GF_WATER) || (GF_STORM))
			{
				terrain_adjustment = dam / 3;
			}
			break;
		}

		/* Cold and water-based spells suffer, and fire-based spells benefit. */
		case FEAT_LAVA:
		{
			if ((typ == GF_COLD) || (typ == GF_ICE) ||
			    (typ == GF_WATER) || (typ == GF_STORM))
			{
				terrain_adjustment -= dam / 3;
			}
			else if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
			         (typ == GF_PLASMA))
			{
				terrain_adjustment = dam / 5;
			}
			break;
		}

		/* Monsters can duck, or take only partial damage. */
		case FEAT_TREE:
		{
			if ((!m_ptr->csleep) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
			    (one_in_(4)))
			{
				msg_format("%^s hides behind a tree!", m_name);
				return (FALSE);
			}
			else
			{
				terrain_adjustment -= dam / 6;
			}
			break;
		}
	}


	/* Remember old sleep */
	old_sleep = m_ptr->csleep;

	/* Monster wakes up */
	m_ptr->csleep = 0;

	/* Monster goes active */
	m_ptr->mflag |= (MFLAG_ACTV);


	/* Analyze the damage type, determine effects */
	switch (typ)
	{
		/* Boulders -- damage, possibly stunning.  Can miss. */
		case GF_ROCK:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* XXX - Crude formula to determine hit. */
			if (rand_int(200) < r_ptr->ac)
			{
				msg_print("The boulder misses.");
				dam = 0;
			}

			/* Can stun monsters. */
			if ((dam > 15) && one_in_(2))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			if (seen) obvious = TRUE;
			break;
		}

		/* Arrows and Missiles -- XXX: damage only.  Can miss. */
		case GF_SHOT:
		case GF_ARROW:
		case GF_MISSILE:
		case GF_PMISSILE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* XXX - Crude formula to determine hit. */
			if (rand_int(200) < r_ptr->ac)
			{
				msg_print("The missile misses.");
				dam = 0;
			}

			if (seen) obvious = TRUE;
			break;
		}

		/* Pure hurt */
		case GF_HURT:
		{
			/* Not affected by terrain  XXX */

			/* No resists, adjusts, etc. */

			break;
		}

		/* Acid */
		case GF_ACID:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ACID))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_ACID);
			}
			break;
		}

		/* Electricity */
		case GF_ELEC:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
			}
			/* Can stun, if enough damage is done. */
			else if ((dam > 10) && (one_in_(2)))
				do_stun = randint(dam > 240 ? 32 : dam / 8);

			break;
		}

		/* Fire */
		case GF_FIRE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
				dam += (dam / 4);
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_FIRE);
			}
			break;
		}

		/* Cold */
		case GF_COLD:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				dam += (dam / 4);
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_COLD);
			}
			break;
		}

		/* Poison */
		case GF_POIS:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_POIS))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_POIS);
			}
			break;
		}

		/* Ice -- Cold + Stun */
		case GF_ICE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_IM_COLD))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_COLD);
			}
			else
			{
				if (r_ptr->flags3 & (RF3_HURT_COLD))
				{
					dam += (dam / 4);
					if (fully_seen) l_ptr->flags3 |= (RF3_HURT_COLD);
				}

				do_stun = randint(dam > 240 ? 20 : dam / 12);
			}
			break;
		}

		/* Plasma  */
		case GF_PLASMA:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if ((r_ptr->flags4 & (RF4_BRTH_PLAS)) ||
			    (r_ptr->flags3 & (RF3_RES_PLAS)))
			{
				note = " resists a lot.";
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_PLAS);
			}

			else if ((r_ptr->flags3 & (RF3_IM_ELEC)) ||
			         (r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				if ((r_ptr->flags3 & (RF3_IM_ELEC)) &&
				    (r_ptr->flags3 & (RF3_IM_FIRE)))
				{
					note = " resists.";
					dam = div_round(dam, 4);

					if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
					if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
				}
				else
				{
					note = " resists somewhat.";
					dam = div_round(dam, 2);

					if (fully_seen)
					{
						if ((r_ptr->flags3 & (RF3_IM_FIRE)) && (one_in_(2)))
							l_ptr->flags3 |= (RF3_IM_FIRE);
						if ((r_ptr->flags3 & (RF3_IM_ELEC)) && (one_in_(2)))
							l_ptr->flags3 |= (RF3_IM_ELEC);
					}
				}
			}
			break;
		}

		/* Hellfire -- fire, plus nastiness to non-evil */
		case GF_HELLFIRE:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;


			/* Demons are healed */
			if (r_ptr->flags3 & (RF3_DEMON))
			{
				/* Message */
				note = " basks in the flames of Hell!";

				/* Heal */
				m_ptr->hp += dam;

				/* No overflow */
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == cave_m_idx[y][x])
					p_ptr->redraw |= (PR_HEALTH);

				/* No damage */
				dam = 0;
			}

			/* Less effective on creatures that wield darkness */
			else if ((r_ptr->flags4 & (RF4_BRTH_DARK)) ||
			         (r_ptr->flags2 & (RF2_MORGUL_MAGIC)))
			{
				note = " resists the dark flames.";
				dam /= 2;
			}

			/* Can resist fire */
			else if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				note = " resists the evil fire.";
				dam /= 2;
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
			}

			/* Evil creatures resist a little */
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				note = " resists somewhat.";
				dam = 2 * dam / 3;
			}

			/* If the creature did not resist, it can have nasty done to it */
			else if (randint(dam) > (2 * r_ptr->level / 3 + 30))
			{
				int choice = randint(4);

				/* Effect 1 -- panic */
				if ((choice <= 3) && (!(r_ptr->flags3 & (RF3_NO_FEAR))))
				{
					/* Get frightened later */
					do_fear = rand_spread(2 * dam / 3, dam / 3);
				}

				/* Effect 2 -- confusion */
				if ((choice == 4) && (!(r_ptr->flags3 & (RF3_NO_CONF))))
				{
					/* Get confused later */
					do_conf = rand_range(15, 15 + (dam > 200 ? 20 : dam / 10));
				}
			}

			break;
		}

		/* Light, but only hurts susceptible creatures */
		case GF_LITE_WEAK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Hurt by light */
			if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				/* Obvious effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_LITE);

				/* Special effect */
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
			}

			/* Normally no damage */
			else
			{
				/* No damage */
				dam = 0;
			}

			break;
		}

		/* Light -- opposite of Dark */
		case GF_LITE:
		case GF_LITE_EXTRA:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_LITE))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_LITE);
				note = " cringes from the light!";
				note_dies = " shrivels away in the light!";
				dam = 3 * dam / 2;

				/* Allow extra effects */
				if (typ == GF_LITE_EXTRA)
				{
					/* Does nasty on spiders */
					if (strchr("S", r_ptr->d_char))
					{
						/* Especially Shelob */
						if (r_ptr->flags1 & (RF1_UNIQUE))
						{
							dam *= 3;
						}
						else
						{
							dam = 3 * dam / 2;
						}

						/* Attempt to frighten the monster.  Ignore level. */
						if (!(r_ptr->flags3 & (RF3_NO_FEAR)))
						{
							if (one_in_(2))
							{
								do_fear = rand_range(dam / 2, dam);
							}
						}
					}
				}
			}
			break;
		}

		/* Darkness -- opposite of Light */
		case GF_DARK:
		case GF_MORGUL_DARK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_DARK))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}

			/* Creatures that use Morgul-magic are resistant to darkness. */
			else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
			{
				note = " resists somewhat.";
				dam = div_round(dam, 3);
			}

			/* Orcs partially resist darkness. */
			else if (r_ptr->flags3 & (RF3_ORC))
			{
				note = " resists somewhat.";
				dam = div_round(dam, 2);
			}
			break;
		}

		/* Confusion */
		case GF_CONFUSION:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_CONFU))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				note = " resists somewhat.";
				dam = div_round(dam, 2);
			}
			else
			{
				do_conf = rand_range(20, 20 + (dam > 300 ? 30 : dam / 10));
			}
			break;
		}

		/* Sound -- Sound breathers resist, others may be stunned. */
		case GF_SOUND:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;

			do_stun = randint(dam > 300 ? 30 : dam / 10);

			if (r_ptr->flags4 & (RF4_BRTH_SOUND))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			break;
		}

		/* Shards -- Shard breathers resist */
		case GF_SHARD:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_SHARD))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			break;
		}

		/* Inertia -- breathers resist */
		case GF_INERTIA:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_INER))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			break;
		}

		/* Gravity -- breathers resist */
		case GF_GRAVITY:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_GRAV))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				break;
			}

			/* Some monsters can resist being moved about */
			if (r_ptr->flags3 & (RF3_RES_TPORT))
			{
				if (one_in_(3))
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_RES_TPORT);
					note = " stays in place!";
					break;
				}
			}

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Force.  Can stun. */
		case GF_FORCE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;

			if (r_ptr->flags4 & (RF4_BRTH_FORCE))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				break;
			}

			/* Can stun */
			if (one_in_(2)) do_stun = randint(dam > 240 ? 20 : dam / 12);

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Water damage -- Water spirits/elementals are immune */
		case GF_WATER:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_RES_WATER)) || (prefix(name, "Water")))
			{
				note = " is immune.";
				dam = 0;
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_WATER);
			}

			/* Can stun */
			if (one_in_(2)) do_stun = randint(dam > 240 ? 20 : dam / 12);

			break;
		}

		/* Wind -- Hurts most monsters */
		case GF_WIND:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;

			/* Wind-breathers and "air" monsters love wind */
			if ((r_ptr->flags4 & (RF4_BRTH_WIND)) || prefix(name, "Air"))
			{
				note = " is unaffected!";
				dam = 0;
				break;
			}

			/* Wind doesn't affect ghosts very much */
			if (strchr("G", r_ptr->d_char))
			{
				note = " resists.";
				dam = div_round(dam, 3);
				break;
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Storm damage -- Various immunities, resistances, & effects */
		case GF_STORM:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_RES_WATER)) || (prefix(name, "Water")))
			{
				note = " is immune.";
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_WATER);
				dam = 0;
				break;
			}

			/* Electricity resistance. */
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				note = " resists.";
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
				dam /= 2;
			}
			else if ((dam) && one_in_(6))
			{
				/* Lightning strike. */
				note = " is struck by lightning!";

				dam += dam / 2;
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 50) && (one_in_(2)))
				do_stun = randint(dam > 240 ? 20 : dam / 12);

			/* Can confuse, if monster can be confused. */
			if ((dam > 20) && one_in_(3))
			{
				/* Get confused later */
				do_conf = rand_range(15, 15 + (dam > 200 ? 20 : dam / 10));
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nexus -- Breathers and Nexus beings resist */
		case GF_NEXUS:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_RES_NEXUS)) ||
			    (r_ptr->flags4 & (RF4_BRTH_NEXUS)))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_NEXUS);

				break;
			}

			/* Mark grid for later processing, if not resisted. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nether -- see above */
		case GF_NETHER:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_UNDEAD))
			{
				note = " is immune.";
				dam = 0;
				if (fully_seen) l_ptr->flags3 |= (RF3_UNDEAD);
			}
			else if ((r_ptr->flags3 & (RF3_RES_NETHR)) ||
			         (r_ptr->flags4 & (RF4_BRTH_NETHR)))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_NETHR);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam = 2 * dam / 3;
				note = " resists somewhat.";
				if (fully_seen) l_ptr->flags3 |= (RF3_EVIL);
			}
			break;
		}

		/* Chaos -- Chaos breathers resist */
		case GF_CHAOS:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;

			/* Allow resistance */
			if ((r_ptr->flags4 & (RF4_BRTH_CHAOS)) ||
			    (r_ptr->flags3 & (RF3_RES_CHAOS)))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_CHAOS);
			}

			/* Sometimes polymorph and confuse monsters */
			else if (dam > rand_int(dam + 200))
			{
				if (one_in_(2)) do_poly = TRUE;
				else do_conf = rand_range(15, 15 + (dam > 600 ? 60 : dam / 10));
			}

			break;
		}

		/* Disenchantment -- Breathers and Disenchanters resist */
		case GF_DISENCHANT:
		{
			if (seen) obvious = TRUE;
			if ((r_ptr->flags3 & (RF3_RES_DISEN)) ||
			    (r_ptr->flags4 & (RF4_BRTH_DISEN)))
			{
				note = " resists.";
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_DISEN);
			}
			break;
		}

		/* Time -- breathers resist */
		case GF_TIME:
		{
			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_TIME))
			{
				note = " resists.";
				dam = div_round(dam, 6);
			}
			break;
		}

		/* Magic energy */
		case GF_MANA:
		{
			/* Affected slightly by terrain. */
			dam += terrain_adjustment / 2;

			/* "mana" monsters and breathers are immune */
			if (strstr(name, "mana") || strstr(name, "Mana") ||
			    (r_ptr->flags4 & (RF4_BRTH_MANA)))
			{
				note = " is immune!";
				dam = 0;
			}

			if (seen) obvious = TRUE;
			break;
		}

		/* Meteor and orb -- powerful magic missile */
		case GF_METEOR:
		case GF_BLACK_ORB:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 20;
			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = r_ptr->level + 15;
			else tmp = r_ptr->level + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists!";

			/* If it fails, become stunned. */
			else do_stun = rand_spread(dam / 10, dam / 5);

			if (seen) obvious = TRUE;
			break;
		}

		/* Holy Orb -- hurts Evil */
		case GF_HOLY_ORB:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags3 & (RF3_EVIL))
			{
				note = " is hit hard.";
				dam = 3 * dam / 2;
				if (fully_seen) l_ptr->flags3 |= (RF3_EVIL);
			}
			else if (r_ptr->d_char == 'A')
			{
				note = "is immune!";
				dam = 0;
			}
			break;
		}

		/* Kill Living and Drain Life */
		case GF_DEATH:
		case GF_VAMP_DRAIN:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;

			/* No affect on non-living creatures */
			if (monster_nonliving(r_ptr))
			{
				if (r_ptr->flags3 & (RF3_UNDEAD))
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_UNDEAD);
				}
				if (r_ptr->flags3 & (RF3_DEMON))
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_DEMON);
				}

				note = " is unaffected!";
				obvious = FALSE;
				dam = 0;
			}

			/* Normal draining ends here */
			if (typ == GF_DEATH) break;

			/* Do not allow wimpy monsters to yield much profit */
			if (m_ptr->hp + 1 < dam) dam = m_ptr->hp + 1;

			/* Character has cast the spell */
			if (who < 0)
			{
				/* Spell is damaging, and has hit a warm-blooded creature. */
				if ((dam > 0) && (!(r_ptr->flags2 & (RF2_COLD_BLOOD))))
				{
					msg_print("You suck in some life force.");

					/* Heal caster */
					hp_player(randint(dam * 3 - 1));

					/* Feed caster -- protect against bloating */
					if (p_ptr->food + dam * 8 < p_ptr->food_bloated)
						set_food(p_ptr->food + dam * 8);
				}
			}

			break;
		}

		/* Death cloud -- requires special-case code */
		case GF_DEATH_CLOUD:
		{
			if (seen) obvious = TRUE;

			/* Not affected by terrain. */

			/* No effect on non-living monsters -- no learning */
			if (monster_nonliving(r_ptr)) dam = 0;

			break;
		}

		/* Curse a monster, attempt to slow, daze, confuse, frighten. */
		case GF_CURSE:
		{
			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 20;
			if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = r_ptr->level + 10;
			else tmp = r_ptr->level + 5;

			/* Attempt a saving throw. */
			if (tmp > randint(dam))
			{
				note = " resists the curse!";
				obvious = FALSE;
				dam = 0;

				break;
			}

			/* Allow at least one, and possibly several effects */
			while (TRUE)
			{
				int curse = rand_int(5);

				/* Effect 0 -- slow */
				if (curse == 0)
				{
					do_slow = TRUE;
				}

				/* Effect 1 -- confusion */
				if ((curse == 1) && (!(r_ptr->flags3 & (RF3_NO_CONF))))
				{
					/* Get confused later */
					do_conf = rand_range(15, 15 + (dam > 200 ? 20 : dam / 10));
				}

				/* Effect 2 -- panic */
				if ((curse == 2) && (!(r_ptr->flags3 & (RF3_NO_FEAR))))
				{
					/* Get frightened later */
					do_fear = rand_spread(2 * dam / 3, dam / 3);
				}

				/* Effect 3 -- stun */
				if (curse == 3)
				{
					/* Get stunned later */
					do_stun = rand_spread(2 * dam / 3, dam / 3);
				}

				/* Strip away bonuses */
				if (curse == 4)
				{
					if (m_ptr->hp > m_ptr->maxhp)
					{
						m_ptr->hp = m_ptr->maxhp;
						note = " is cut down to size!";
					}
					else if (m_ptr->mspeed > r_ptr->speed)
					{
						m_ptr->mspeed = r_ptr->speed;
						note = " is no longer hasted.";
					}
				}

				/* Usually end the curse */
				if (!one_in_(3)) break;
			}

			/* No "real" damage */
			dam = 0;

			break;
		}

		/* Dispel monster */
		case GF_SMITE:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";

			/* Determine monster's power to resist  (damage is high) */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 50;
			else tmp = r_ptr->level + 25;


			/* Slow */
			if (tmp <= randint(dam))
			{
				do_slow = TRUE;
			}

			/* Confusion */
			if (tmp <= randint(dam))
			{
				/* Get confused later */
				do_conf = rand_range(20, 20 + (dam > 200 ? 20 : dam / 10));
			}

			/* Stun */
			if (tmp <= randint(dam))
			{
				/* Get stunned later */
				do_stun = rand_range(dam / 8, dam / 4);
			}

			break;
		}

		/* Dispel monster */
		case GF_DISPEL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			note = " shudders.";
			note_dies = " dissolves!";

			break;
		}

		/* Dispel only weak monsters. */
		case GF_DISPEL_SMALL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Only affect the weakest creatures. */
			if (m_ptr->hp >= dam)
			{
				dam = 0;
				note = " is unaffected.";
			}
			else
			{
				tmp = rand_int(40);

				/* Colorful messages */
				if (tmp <= 34) note_dies = " collapses.";
				if (tmp == 35) note_dies = " falls suddenly silent!";
				if (tmp == 36) note_dies = " lies stiff and still!";
				if (tmp == 37) note_dies = " dies in a fit of agony!";
				if (tmp == 38) note_dies = " squeals and topples over!";
				if (tmp == 39) note_dies = " is snuffed out!";
				if (tmp == 40) note_dies = " shrieks in mortal pain!";
			}

			break;
		}

		/* Mental attacks -- From PsiAngband */
		case GF_PSI:
		{
			int psi_resists = 0;

			note_dies = " collapses, a mindless husk.";


			/* Vortexes are completely immune */
			if (r_ptr->d_char == 'v')
			{
				skipped = TRUE;
				break;
			}

			/* Get character resistance to backlashing */
			if (p_ptr->mental_barrier) psi_resists += 2;
			if (p_ptr->holy) psi_resists++;
			if (p_ptr->skill_sav > rand_int(100)) psi_resists++;
			if ((p_ptr->berserk) && (p_ptr->skill_sav < randint(100)))
				 psi_resists--;
			if (p_ptr->confused) psi_resists--;
			if (p_ptr->image) psi_resists--;
			if (p_ptr->stun) psi_resists--;
			if ((p_ptr->afraid) && (p_ptr->skill_sav < randint(100)))
				 psi_resists--;


			/* Attempt to backlash the character */
			if (r_ptr->flags3 & (RF3_UNDEAD | RF3_DEMON))
			{
				/* Check for backlash */
				if (r_ptr->level / 2 > rand_int(dam + psi_resists * 20))
				{
					u32b flg2 = PROJECT_JUMP | PROJECT_HIDE | PROJECT_KILL;

					msg_format("%^s%s corrupted mind backlashes your attack!",
								  m_name, (seen ? "'s" : "s"));

					/* Project the psi attack right back */
					(void)project(cave_m_idx[y][x], 0, p_ptr->py, p_ptr->px,
					              p_ptr->py, p_ptr->px, dam / 2,
					              GF_PSI, flg2, 0, 0);

					/* Learn something */
					if (r_ptr->flags3 & (RF3_UNDEAD))
					{
						if (fully_seen) l_ptr->flags3 |= (RF3_UNDEAD);
					}
					if (r_ptr->flags3 & (RF3_DEMON))
					{
						if (fully_seen) l_ptr->flags3 |= (RF3_DEMON);
					}

					/* No effect on monster */
					skipped = TRUE;
					break;
				}
			}

			/* Check for mental combat */
			else if (r_ptr->flags6 & (RF6_MIND_BLAST))
			{
				/* Opponent's level and your mental power meet... */
				int mon_lev = r_ptr->level;
				int mind_strength = 2 * p_ptr->skill_sav / 3 +
				                    psi_resists * 20;

				/* Monster wins */
				if (rand_int(mon_lev) > rand_int(mind_strength))
				{
					msg_format("%^s stares right back at you!", m_name);

					tmp = randint(5);

					if (tmp == 1) set_blind(p_ptr->blind + rand_range(5, 7), NULL);
					if (tmp == 2) set_confused(p_ptr->confused + rand_range(3, 5));
					if (tmp == 3) set_afraid(p_ptr->afraid + rand_range(12, 24));
					if (tmp == 4) set_paralyzed(p_ptr->paralyzed + randint(2));
					if (tmp == 5) take_hit(dam / 2, 0, "Your mind is smashed!",
						                  format("mind-smashed by %s", m_name));
				}

				/* You win */
				else
				{
					dam += 20;
				}

				/* Learn about the spells the hard way */
				if (fully_seen)
				{
					l_ptr->flags6 |= (RF6_MIND_BLAST);
				}
			}

			/* Get monster resistance level */
			tmp = r_ptr->level;
			if (r_ptr->flags1 & (RF1_UNIQUE))      tmp += 25;
			if (r_ptr->flags2 & (RF2_SMART))       tmp += 10;
			if (r_ptr->flags2 & (RF2_STUPID))      tmp -= 20;

			if (monster_nonliving(r_ptr))          tmp += 10;

			/* Note that EMPTY_MIND is deliberately ignored */

			/* Roll for resistance */
			if (rand_int(dam) < tmp)
			{
				dam = div_round(dam, 3);
				note = " resists.";
			}

			/* Smart monsters often win, but suffer if they lose */
			else if (r_ptr->flags2 & (RF2_SMART))
			{
				dam += dam / 2;
				if (dam >= p_ptr->chp / 2) note = " is vulnerable!";
			}

			/* Stupid monsters often lose, but it doesn't matter much */
			else if (r_ptr->flags2 & (RF2_STUPID))
			{
				dam = div_round(dam, 3);
				if (dam <= p_ptr->chp / 5) note = " stupidly resists the attack.";
			}

			/* All mental attacks ignore ordinary resists */

			/* Try to drive the monster insane (doesn't work on everything) */
			if (((20 * dam / tmp) > rand_int(100)) &&
			    (!strchr("gvQ", r_ptr->d_char)))
			{
				m_ptr->mflag |= (MFLAG_MADD);
				note = " goes insane!";
			}

			/* Try to confuse the monster */
			if ((30 * dam / tmp) > rand_int(100))
			{
				do_conf = rand_range(20, 20 + (dam > 200 ? 20 : dam / 10));
			}

			/* Try to slow the monster down */
			if ((40 * dam / tmp) > rand_int(100))
			{
				do_slow = TRUE;
			}

			/* Try to daze the monster */
			if ((50 * dam / tmp) > rand_int(100))
			{
				do_stun = rand_range(15, 15 + dam / 5);
			}

			/* Try to rob the monster of energy */
			if ((50 * dam / tmp) > rand_int(100))
			{
				m_ptr->energy -= randint(m_ptr->energy);
				note = " stumbles.";
			}

			/* Try to frighten the monster */
			if ((60 * dam / tmp) > rand_int(100))
			{
				do_fear = rand_range(25, 25 + dam / 3);
			}

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Madness */
		case GF_MADNESS:
		{
			/* Determine resistance */
			int resist = r_ptr->level / 3 + 5;
			if (r_ptr->flags1 & (RF1_UNIQUE)) resist *= 2;
			if (r_ptr->flags3 & (RF3_UNDEAD)) resist += 15;
			if (r_ptr->flags2 & (RF2_STUPID)) resist += 10;

			/* Golems, Vortexes, and Q's are completely immune */
			if (strchr("gvQ", r_ptr->d_char))
			{
				skipped = TRUE;
				break;
			}

			/* Try to drive the monster insane */
			if (rand_int(dam) > resist)
			{
				m_ptr->mflag |= (MFLAG_MADD);
				note = " goes insane!";
			}

			/* No physical damage */
			dam = 0;
			break;
		}

		/* Frighten monsters (Use "dam" as "power"). */
		case GF_DO_FEAR:
		case GF_DO_FEAR_PRIEST:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Sometimes super-charge the spell */
			if (one_in_(5)) dam += dam / 2;

			/* Priests do a number on undead and demons */
			if (typ == GF_DO_FEAR_PRIEST)
			{
				if (r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD))
					dam += dam / 3;
			}


			/* Monsters can be immune to fear */
			if (r_ptr->flags3 & (RF3_NO_FEAR))
			{
				/* Undead or demonic creatures can still be vulnerable */
				if ((r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD)) &&
				    (p_ptr->proj_mon_flags & (RF3_DEMON | RF3_UNDEAD)))
				{
					/* Continue */
				}

				/* Otherwise, this flag provides immunity */
				else
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_NO_FEAR);
					note = " knows no fear!";
					dam = 0;
					break;
				}
			}

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level/2 + 20;
			else tmp = r_ptr->level/2 + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists!";

			/* If it fails, panic. */
			else do_fear = rand_range(dam, dam * 2);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Slow Monster (Use "dam" as "power") */
		case GF_DO_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Sometimes super-charge the spell. */
			if (one_in_(5)) dam += dam / 2;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level / 2 + 20;
			else tmp = r_ptr->level / 2 + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists!";

			/* If it fails, slow down */
			else do_slow = TRUE;

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Confusion (Use "dam" as "power"). */
		case GF_DO_CONF:
		{
			if (seen) obvious = TRUE;

			/* Sometimes super-charge the spell. */
			if (one_in_(5)) dam += dam / 2;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE))
				tmp = r_ptr->level / 2 + 20;
			else if (r_ptr->flags3 & (RF3_UNDEAD))
				tmp = r_ptr->level / 2 + 15;
			else
				tmp = r_ptr->level / 2 + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists!";

			/* If it fails, become confused. */
			else do_conf = rand_range(20, 20 + (dam > 80 ? 20 : dam / 4));

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Sleep (Use "dam" as "power"). */
		case GF_DO_SLEEP:
		{
			if (seen) obvious = TRUE;

			/* Hack -- Monster didn't wake up */
			m_ptr->csleep = old_sleep;

			/* Sometimes super-charge the spell. */
			if (one_in_(5)) dam += dam / 2;

			/* Allow resistance */
			if (r_ptr->flags3 & (RF3_NO_SLEEP))
			{
				note = " cannot be lulled!";
				obvious = FALSE;

				/* Note resistance */
				if (fully_seen) l_ptr->flags3 |= (RF3_NO_SLEEP);
			}
			else
			{
				/* Determine monster's power to resist. */
				if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level/2 + 20;
				else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = r_ptr->level/2 + 15;
				else tmp = r_ptr->level/2 + 5;

				/* Attempt a saving throw */
				if (tmp > dam)               note = " is unaffected!";
				else if (tmp > randint(dam)) note = " resists!";

				/* If it fails, (usually) hit the hay. */
				else do_sleep = rand_range(30, 50);
			}

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Stun (Use "dam" as "power"). */
		case GF_DO_STUN:
		{
			if (seen) obvious = TRUE;

			/* Sometimes super-charge the spell. */
			if (one_in_(5)) dam += dam / 2;

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level/2 + 20;
			else if (r_ptr->flags3 & (RF3_UNDEAD)) tmp = r_ptr->level/2 + 15;
			else tmp = r_ptr->level/2 + 5;

			/* Attempt a saving throw */
			if (tmp > dam)               note = " is unaffected!";
			else if (tmp > randint(dam)) note = " resists!";

			/* If it fails, become stunned. */
			else do_stun = rand_spread(dam / 2, dam / 4);

			/* Notice resistance only if character is the caster */
			if (who >= 0) note = NULL;

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Clone monsters (Ignore "dam") */
		case GF_DO_CLONE:
		{
			if (seen) obvious = TRUE;

			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Attempt to clone. */
			if (multiply_monster(cave_m_idx[y][x]))
			{
				note = " spawns!";
			}

			/* No "real" damage */
			dam = 0;

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_DO_POLY:
		{
			if (seen) obvious = TRUE;

			/* Attempt to polymorph (see below) */
			do_poly = TRUE;

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Heal Monster (use "dam" as amount of healing) */
		case GF_DO_HEAL:
		{
			if (seen) obvious = TRUE;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x])
				p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			note = " looks healthier.";

			/* No physical damage */
			dam = 0;
			break;
		}

		/* Speed Monster (Ignore "dam") */
		case GF_DO_SPEED:
		{
			if (seen) obvious = TRUE;

			/* Speed up */
			if (m_ptr->mspeed < r_ptr->speed + 10) m_ptr->mspeed += 10;
			note = " starts moving faster.";

			/* No physical damage */
			dam = 0;
			break;
		}


		/* Teleport monsters and player (Use "dam" as "power") */
		case GF_COME_HITHER:
		case GF_AWAY:
		{
			/* No physical damage */
			dam = 0;

			/* Some monsters resist teleportation 2/3rds of the time */
			if (r_ptr->flags3 & (RF3_RES_TPORT))
			{
				if (!one_in_(3))
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_RES_TPORT);
					note = " resists!";
					break;
				}
			}

			/* Some monsters resist teleportation 1/3rds of the time */
			if ((r_ptr->flags3 & (RF3_RES_NEXUS)) ||
			    (r_ptr->flags4 & (RF4_BRTH_NEXUS)))
			{
				if (one_in_(3))
				{
					if (fully_seen)
					{
						if (r_ptr->flags3 & (RF3_RES_NEXUS))
						    l_ptr->flags3 |= (RF3_RES_NEXUS);
						if (r_ptr->flags4 & (RF4_BRTH_NEXUS))
						    l_ptr->flags4 |= (RF4_BRTH_NEXUS);
					}
					note = " resists!";
					break;
				}
			}

			/* GF_COME_HITHER doesn't always work */
			if (typ == GF_COME_HITHER)
			{
				/* High-level monsters are harder to summon */
				if (rand_range(r_ptr->level, 4 * r_ptr->level / 3) >
				    p_ptr->power + rand_int(20))
				{
					note = " resists!";
					break;
				}
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Stone to Mud */
		case GF_KILL_WALL:
		case GF_DISINTEGRATE:
		{
			/* Hurt by rock remover */
			if (r_ptr->flags3 & (RF3_HURT_ROCK))
			{
				/* Notice effect */
				if (seen) obvious = TRUE;

				/* Memorize the effects */
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_ROCK);

				/* Cute little message */
				note = " loses some skin!";
				note_dies = " dissolves!";
			}

			/* Usually, ignore the effects */
			else
			{
				/* Hack -- Monster didn't wake up */
				m_ptr->csleep = old_sleep;

				/* No damage */
				dam = 0;
			}

			break;
		}

		/*
		 * Monsters learn about the character
		 * (for lack of anything else to learn)
		 */
		case GF_ENLIGHTENMENT:
		{
			int i;
			int m_idx = cave_m_idx[y][x];

			/* Learn lots of things */
			for (i = 0; i < LRN_MAX; i++)
			{
				if (one_in_(2))
					update_smart_learn(m_idx, i);
			}

			if (seen) obvious = TRUE;

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Become more powerful */
		case GF_GAIN_LEVEL:
		{
			/* Gain *maximum* hps */
			long new_hps = ((long)m_ptr->maxhp * 5 / 4);
			if (new_hps <= 32000L) m_ptr->maxhp = (s16b)new_hps;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			note = " looks more powerful!";

			if (seen) obvious = TRUE;

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Default */
		default:
		{
			/* Irrelevant */
			skipped = TRUE;

			/* No damage */
			dam = 0;

			break;
		}
	}


	/* Absolutely no effect */
	if (skipped) return (FALSE);


	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}


	/* Track it */
	project_m_n++;
	project_m_x = x;
	project_m_y = y;


	/* Hack -- Sound attacks are extremely noisy. */
	if (typ == GF_SOUND) add_wakeup_chance = 10000;

	/*
	 * Otherwise, if this is the first monster hit, the spell was capable
	 * of causing damage, and the player was the source of the spell,
	 * make noise. -LM-
	 */
	else if ((project_m_n == 1) && (who <= 0) && (dam))
	{
		add_wakeup_chance += p_ptr->base_wakeup_chance / 2 + 1000;
	}



	/*
	 * Mega-Hack -- "Unique" and special quest monsters may only be killed
	 * by the player.
	 */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
	    (m_ptr->r_idx == q_info[quest_num(p_ptr->depth)].r_idx))
	{
		if ((who > 0) && (dam > m_ptr->hp)) dam = m_ptr->hp;
	}


	/* Hurt monster if damage is greater than zero */
	if (dam > 0)
	{
		bool can_hear = FALSE;

		/* Different (or no) death messages when not seen */
		if (!seen)
		{
			/* No death message */
			note_dies = "";

			/* Not in line of sight, too far away to hear through walls */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) ||
			    (player_has_los_bold(y, x)) ||
			    (distance(p_ptr->py, p_ptr->px, y, x) <= 10))
			{
				/* Can hear */
				can_hear = TRUE;
			}
		}

		/* Hurt the monster, check for fear and death */
		if (mon_take_hit(cave_m_idx[y][x], who, dam, &fear, note_dies))
		{
			/* Note death */
			if (can_hear) death_count++;

			/* Return "Anything seen?" */
			return (obvious);
		}
	}


	/* Handle polymorph */
	if (do_poly)
	{
		/* Default -- assume no polymorph */
		if (!dam && !note)
		{
			note = " cannot be polymorphed!";
			obvious = TRUE;
		}

		/* Uniques can't be polymorphed */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			do_poly = FALSE;
			obvious = TRUE;
		}

		/* Chaos breathers and resisters can't be polymorphed */
		else if ((r_ptr->flags3 & (RF3_RES_CHAOS)) ||
		         (r_ptr->flags4 & (RF4_BRTH_CHAOS)))
		{
			do_poly = FALSE;
			obvious = TRUE;

			/* Take note */
			if (fully_seen) l_ptr->flags3 |= (RF3_RES_CHAOS);
		}

		/* Allow a saving throw */
		if (randint(150) < r_ptr->level + 50)
		{
			if (!dam && !note) note = " resists!";
			do_poly = FALSE;
			obvious = FALSE;
		}

		/* Polymorph the monster */
		if (do_poly)
		{
			/* Note how wounded the old monster was */
			long perc = 100 - (100 * m_ptr->hp / m_ptr->maxhp);

			/* Introduce a little variance */
			perc = rand_range(2 * perc / 3, 4 * perc / 3);

			/* New monster cannot be too badly hurt */
			if (perc >= 75) perc = 75;

			/* Pick a new monster race */
			tmp = poly_r_idx(m_ptr->r_idx);

			/* Handle polymorph */
			if (tmp != m_ptr->r_idx)
			{
				/* Obvious */
				if (seen)
				{
					obvious = TRUE;

					/* Hack -- Note polymorph, ignore other messages */
					msg_format("%^s changes!", m_name);
				}

				/* Delete the "old" monster */
				delete_monster_idx(cave_m_idx[y][x]);

				/* Create a new monster (no groups) */
				(void)place_monster_aux(y, x, tmp, FALSE, FALSE);

				/* Hack -- Get new monster */
				m_ptr = &m_list[cave_m_idx[y][x]];

				/* New monster's wounds are much like those of the old monster */
				m_ptr->hp -= (perc * m_ptr->maxhp / 100);

				/* New monster is sometimes slightly confused and/or stunned */
				if (one_in_(2)) m_ptr->confused = 5;
				if (one_in_(2)) m_ptr->stunned  = 5;

				/* Cancel any other effects */
				return (obvious);
			}
		}
	}

	/* Handle slowing -- no cumulative slowing */
	if ((do_slow) && (m_ptr->mspeed >= r_ptr->speed - 5))
	{
		/* Inertia hounds and time elementals can't be slowed */
		if ((r_ptr->flags4 & (RF4_BRTH_INER)) ||
		    (r_ptr->flags4 & (RF4_BRTH_TIME)) ||
		    (strstr(name, "Time ")))
		{
			if (!dam) note = " cannot be slowed!";
		}

		/* Slow the monster down */
		else
		{
			m_ptr->mspeed -= 10;
			if (!note) note = " starts moving slower.";
		}
	}


	/* Handle stunning -- usually no cumulative stunning */
	if ((do_stun) && ((!m_ptr->stunned) || (one_in_(2))))
	{
		/* Sound and Impact breathers never stun */
		if ((r_ptr->flags4 & (RF4_BRTH_SOUND)) ||
		    (r_ptr->flags4 & (RF4_BRTH_FORCE)))
		{
			if (!dam) note = " is immune to stunning!";
		}

		/* Non-living monsters are harder to stun */
		else if ((monster_nonliving(r_ptr)) && (one_in_(2)))
		{
			if (!dam && !note && !m_ptr->stunned)
			{
				note = " is dazed, but quickly recovers.";
			}
		}

		/* Stun the monster */
		else
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Get confused */
			if (m_ptr->stunned)
			{
				if (!note) note = " is more dazed.";
				tmp = m_ptr->stunned + (do_stun / 3);
			}
			else
			{
				if (!note) note = " is dazed.";
				tmp = do_stun;
			}

			/* Apply stun */
			m_ptr->stunned = (tmp < 200) ? tmp : 200;
		}
	}

	/* Handle confusion -- no cumulative confusion */
	if ((do_conf) && (!m_ptr->confused))
	{
		/* Allow resistance */
		if ((r_ptr->flags3 & (RF3_NO_CONF)) ||
		    (r_ptr->flags4 & (RF4_BRTH_CONFU)) ||
		    (r_ptr->flags4 & (RF4_BRTH_CHAOS)))
		{
			if (!dam) note = " cannot be confused!";

			/* Note resistance */
			if (fully_seen) l_ptr->flags3 |= (RF3_NO_CONF);
		}

		/* Confuse the monster */
		else
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Already confused  (not currently possible) */
			if (m_ptr->confused)
			{
				if (!note) note = " looks more confused.";
				tmp = m_ptr->confused + (do_conf / 3);
			}

			/* Was not confused */
			else
			{
				if (!note) note = " looks confused.";
				tmp = do_conf;
			}

			/* Apply confusion */
			m_ptr->confused = (tmp < 200) ? tmp : 200;
		}
	}

	/* Handle fear -- resistances handled earlier  XXX */
	if (do_fear)
	{
		/* Note new fear */
		if ((!note) && (!m_ptr->monfear)) note = " panics!";

		/* Increase fear (not fully cumulative) */
		if (m_ptr->monfear) do_fear /= 2;
		tmp = MIN(200, m_ptr->monfear + do_fear);

		/* Panic the monster */
		set_mon_fear(m_ptr, tmp, TRUE);
	}

	/* Handle sleeping -- no cumulative sleeping */
	if ((do_sleep) && (!m_ptr->csleep))
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Message */
		note = " falls asleep!";

		/* Apply sleeping */
		m_ptr->csleep = do_sleep;

		/* Monster will /not/ go inactive */
	}


	/* Give detailed messages if visible */
	if (note)
	{
		if (seen)
		{
			msg_format("%^s%s", m_name, note);

			/* Character notices monster */
			m_ptr->mflag &= ~(MFLAG_MIME);
		}
	}

	/* Standard pain messages */
	else
	{
		/* Monster is hurt, and is fairly close  XXX XXX */
		if ((dam > 0) && (distance(p_ptr->py, p_ptr->px, y, x) <= 12))
		{
			message_pain(cave_m_idx[y][x], dam);
		}
	}

	/* Fear message */
	if ((fear) && (m_ptr->ml))
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.
 * Actually, for historical reasons, we just assume that the effects were
 * obvious.  XXX XXX XXX
 */
static bool project_p(int who, int y, int x, int dam, int typ)
{
	int k = 0;

	/* Adjustment to damage caused by terrain, if any. */
	int terrain_adjustment = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = FALSE;

	/* Source monster and its race */
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Monster name (for attacks) */
	char m_name[80];

	/* Monster name (for damage) */
	char killer[80];

	/* Hack -- messages */
	cptr act = NULL;
	cptr msg = NULL;


	/* No player here */
	if (!(cave_m_idx[y][x] < 0)) return (FALSE);

	/* Player cannot harm himself  XXX XXX */
	if (who < 0) return (FALSE);


	/* Some attack types are affected by skills */
	if (typ == GF_LITE)
	{
		s32b temp;
		int factor;

		/* Decrease damage up to 33% if character is very pious */
		factor = 100 - get_skill(S_PIETY, 0, 33);

		/* Calculate and store adjusted damage */
		temp = (s32b)dam * factor / 100;
		dam = (s16b)temp;
	}
	else if ((typ == GF_DARK) || (typ == GF_MORGUL_DARK))
	{
		s32b temp;
		int factor;

		/* Decrease damage up to 33% if character is attuned to darkness */
		factor = 100 - get_skill(S_DOMINION, 0, 33);

		/* Calculate and store adjusted damage */
		temp = (s32b)dam * factor / 100;
		dam = (s16b)temp;
	}


	/* Limit maximum damage XXX XXX XXX */
	if (dam > 1600) dam = 1600;


	/* Determine if terrain is capable of adjusting physical damage. */
	switch (cave_feat[y][x])
	{
		/* A player behind rubble takes less damage. */
		case FEAT_RUBBLE:
		{
			if (one_in_(10))
			{
				msg_print("You duck behind a boulder!");
				return (FALSE);
			}
			else terrain_adjustment -= dam / 6;
			break;
		}

		/*
		 * Fire-based spells suffer, but other spells benefit slightly
		 * (player is easier to hit).  Water spells come into their own.
		 */
		case FEAT_WATER:
		{
			if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
				(typ == GF_PLASMA)) terrain_adjustment -= dam / 4;
			else if ((typ == GF_WATER) || (GF_STORM))
				terrain_adjustment = dam / 2;
			else terrain_adjustment = dam / 10;
			break;
		}

		/* Cold and water-based spells suffer, and fire-based spells benefit. */
		case FEAT_LAVA:
		{
			if ((typ == GF_COLD) || (typ == GF_ICE) ||
				(typ == GF_WATER) || (typ == GF_STORM))
				terrain_adjustment -= dam / 4;
			else if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
				(typ == GF_PLASMA)) terrain_adjustment = dam / 4;
			break;
		}

		/* Characters skilled in nature lore can hide behind trees. */
		case FEAT_TREE:
		{
			int chance = 16 - get_skill(S_NATURE, 0, 10);

			if (one_in_(chance))
			{
				msg_print("You dodge behind a tree!");
				return (FALSE);
			}
			else terrain_adjustment -= dam / 6;
			break;
		}
	}

	/* If the player is blind, be more descriptive */
	if (blind) fuzzy = TRUE;

	/* Get the source monster */
	m_ptr = &m_list[who];

	/* Get the monster race. */
	r_ptr = &r_info[m_ptr->r_idx];

	/* A real monster */
	if (who > 0)
	{
		/* Get the monster name */
		monster_desc(m_name, m_ptr, 0);

		/* Get the monster's real name */
		monster_desc(killer, m_ptr, 0x88);
	}

	/* An explosion  XXX */
	else
	{
		strcpy(m_name, "something");
		strcpy(killer, "an explosion");
	}

	/* Analyze the damage */
	switch (typ)
	{
		/* Boulders -- Can be dodged.  Crushing.  Armour protects a little. */
		case GF_ROCK:
		{
			/* Do we dodge the boulder? */
			if (dodging_ability(30) > 5 + rand_int(r_ptr->level))
			{
				msg_print("You nimbly dodge aside.");
				dam = 0;
			}

			/* We've been hit - check for damage, crushing. */
			if (dam)
			{
				/* Affected by terrain. */
				dam += terrain_adjustment;

				/* Player armor reduces total damage (a little) */
				dam = dam * 150 / (150 + p_ptr->ac + p_ptr->to_a);

				/* Player can be crushed. */
				if (one_in_(3))
				{
					if (fuzzy) msg = "You are crushed by a boulder!";
					else msg = "You are crushed!";

					/* Be careful not to knock out the player immediately. */
					(void)set_stun(p_ptr->stun + ((dam / 2 > 40) ? 40 : dam / 2));
				}
				else
				{
					if (fuzzy) msg = "You are hit by a boulder.";
					else msg = "You are hit.";
				}

				/* Take the damage. */
				take_hit(dam, 0, msg, killer);

				/* Make a boulder sometimes */
				if (one_in_(3))
				{
					make_boulder(y, x, (p_ptr->depth + r_ptr->level) / 2);
				}
			}

			break;
		}

		/* Sling shot -- Stunning, wounding.  Heavy armour protects well.  */
		case GF_SHOT:
		{
			/* Test for deflection - Only base armour counts here. */
			if (p_ptr->ac > 10 + rand_int(50 + r_ptr->level))
			{
				if (fuzzy) msg_print("A missile glances off your armour.");

				else msg_print("The missile glances off your armour.");

				/* No damage. */
				dam = 0;
			}

			/* Test for a deflection. */
			else if ((inventory[INVEN_ARM].k_idx) &&
			         (inventory[INVEN_ARM].tval == TV_SHIELD) &&
			         (!p_ptr->shield_on_back) &&
			         (inventory[INVEN_ARM].ac > rand_int(MAX_SHIELD_BASE_AC * 4)))
			{
				/* No damage. */
				msg_print("The missile ricochets off your shield.");
				dam = 0;
			}

			/* Reduce damage slightly if missile did not get deflected. */
			else dam = dam * 150 / (150 + p_ptr->ac + p_ptr->to_a);

			/* We've been hit - check for stunning, wounding. */
			if (dam)
			{
				if (fuzzy) msg = "You are hit by a sling shot.";
				else msg = "You are hit.";

				/* Affected by terrain. */
				dam += terrain_adjustment;

				/* Take the damage. */
				take_hit(dam, 0, msg, killer);

				/* Player can be stunned. */
				if (one_in_(4))
				{
					/* Be careful not to knock out the player immediately. */
					(void)set_stun(p_ptr->stun + ((dam / 3 > 30) ? 30 : dam / 3));
				}

				/* Player can be wounded. */
				if (one_in_(4))
				{
					/* Wound the player. */
					(void)set_cut(p_ptr->cut + dam / 3);
				}
			}

			break;
		}

		/* Edged physical missiles -- Frequent wounding.  Armour protects some. */
		case GF_ARROW:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Test for deflection - Only base armour counts here. */
			if (p_ptr->ac > 10 + rand_int(50 + r_ptr->level))
			{
				msg_print("The missile glances off your armour.");

				/* No damage. */
				dam = 0;
			}

			/* Test for a deflection. */
			else if ((inventory[INVEN_ARM].k_idx) &&
			         (inventory[INVEN_ARM].tval == TV_SHIELD) &&
			         (!p_ptr->shield_on_back) &&
			         (inventory[INVEN_ARM].ac > rand_int(MAX_SHIELD_BASE_AC * 4)))
			{
				/* No damage. */
				msg_print("The missile ricochets off your shield.");
				dam = 0;
			}

			/* Reduce damage (slightly) if missile did not get deflected. */
			else dam = dam * 150 / (150 + p_ptr->ac + p_ptr->to_a);

			if (dam)
			{
				/* Hit the player */
				if (fuzzy) msg = "You are hit by an arrow!";
				else msg = "You are hit.";

				take_hit(dam, 0, msg, killer);

				/* Player can be wounded. */
				if (one_in_(2))
				{
					/* Wound the player. */
					(void)set_cut(p_ptr->cut + dam / 2);
				}
			}

			break;
		}

		/* Miscellaneous physical missiles.  Can dodge.   Armour reduces damage. */
		/* Also venomous missiles.  These get nasty with Morgul-magic. */
		/* Ringwraiths and Sauron are very dangerous. */
		case GF_MISSILE:
		case GF_PMISSILE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Do we dodge the missile (not an easy thing to do)? */
			if (randint(50 + dodging_ability(150)) > 50 + r_ptr->level)
			{
				msg_print("You nimbly dodge aside.");
				dam = 0;
			}

			/* Test for a deflection (ordinary missiles only). */
			else if ((typ == GF_MISSILE) &&
			         (inventory[INVEN_ARM].k_idx) &&
			         (inventory[INVEN_ARM].tval == TV_SHIELD) &&
			         (!p_ptr->shield_on_back) &&
			         (inventory[INVEN_ARM].ac > rand_int(MAX_SHIELD_BASE_AC * 4)))
			{
				/* No damage. */
				msg_print("The missile ricochets off your shield.");
				dam = 0;
			}

			/* Hit the player with a missile. */
			if (dam)
			{
				/* A poisonous missile hits the player. */
				if (typ == GF_PMISSILE)
				{
					/* Monster has Morgul-magic. */
					if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
					{
						/* Hack - cannot rapid-fire morgul missiles. */
						if ((r_ptr->flags2 & (RF2_ARCHER)) && (m_ptr->cdis > 1) &&
						    (randint(100) > r_ptr->freq_ranged))
						{
							k = 1;
						}

						/* Hack - The Ringwraiths and Sauron are very dangerous. */
						else if ((prefix(m_name, "Sauron, the Sorcerer")) ||
							((r_ptr->d_char == 'W') && (r_ptr->flags1 & (RF1_UNIQUE))))
						{
							/* 40% chance of Black Breath. */
							k = randint(5);
						}

						/* Other monsters with Morgul-magic. */
						else
						{
							/* 17% chance of Black Breath. */
							k = randint(2);
							if ((r_ptr->level > 50) && (one_in_(3)))
								k += 2;
						}
					}

					/* Standard poisonous missile. */
					else k = 1;
				}

				/* Ordinary missile. */
				else k = 0;

				/* Hit the player */
				if (fuzzy) msg = "You are hit by a missile.";
				else msg = "You are hit.";

				/* Armour reduces damage (if not a morgul-missile) */
				if (k < 4)
				{
					dam = dam * 150 / (150 + p_ptr->ac + p_ptr->to_a);
				}

				/* Ordinary missile. */
				if (k == 0)
				{
					/* No special damage. */
					take_hit(dam, 0, msg, killer);
				}

				/* Poisonous missiles */
				else
				{
					int temp = 0;

					/* First the raw damage, */
					take_hit(dam, 0, msg, killer);

					/* Then a cute message, */
					if (k == 4) msg_print("Foul magics assault body and mind!");

					/* Then the poison, */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						if (!p_ptr->poisoned)
							temp = 3 + rand_range(dam / 2, 3 * dam / 2);
						else
							temp = randint((dam + 2) / 3);
					}
					else if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
					{
						if (!p_ptr->poisoned)
							temp = randint((dam + 2) / 3);
						else
							temp = 0;
					}
					(void)set_poisoned(p_ptr->poisoned + temp);

					/* Then the life draining, */
					if (k >= 2)
					{
						if (p_ptr->hold_life && (one_in_(4)))
						{
							msg_print("You feel your life slipping away!");
							lose_exp((r_ptr->level) + (calc_spent_exp() / 1000) *
								MON_DRAIN_LIFE, FALSE);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp((r_ptr->level * 2) + (calc_spent_exp() / 100) *
								MON_DRAIN_LIFE, FALSE);
						}
					}

					/* Then the stat loss, */
					if (k >= 3)
					{
						/* Reduce all unsustained stats by 1. */
						for (k = 0; k < A_MAX; k++)
						{
							(void)do_dec_stat(k, 1, FALSE, NULL, NULL);
						}
						p_ptr->update |= (PU_BONUS);
					}

					/* Then the Black Breath. */
					if (k == 4)
					{
						if (p_ptr->black_breath == FALSE)
						{
							/* Messages. */
							msg_print("Your foe calls upon your soul!");
							message_flush();
							msg_print("You feel the Black Breath slowly draining you of life...");
						}
						p_ptr->black_breath = TRUE;
					}
				}
			}

			break;
		}

		/* Whip or spitting attack -- pure damage */
		case GF_WHIP:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (r_ptr->flags3 & (RF3_ANIMAL))
			{
				if (fuzzy) msg_print("You have been spat upon.");

				/* Ordinary spit doesn't do any damage. */
				dam = 0;
			}
			else
			{
				if (fuzzy) msg_print("You are struck by a whip!");
				take_hit(dam, 0, NULL, killer);
			}
			break;
		}

		/* Pure hurt */
		case GF_HURT:
		{
			/* Not affected by terrain  XXX */

			/* No resists, adjusts, etc. */
			take_hit(dam, 0, NULL, killer);

			break;
		}


		/* Standard damage -- hurts inventory */
		case GF_ACID:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by acid!");
			acid_dam(dam, 0, NULL, killer);
			break;
		}

		/* Standard damage -- hurts inventory, can stun. */
		case GF_ELEC:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by lightning!");
			elec_dam(dam, 0, NULL, killer);
			break;
		}

		/* Standard damage -- hurts inventory */
		case GF_FIRE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by fire!");
			fire_dam(dam, 0, NULL, killer);
			break;
		}

		/* Standard damage -- hurts inventory */
		/* Morgul-cold can be dangerous, if strong and not well-resisted. */
		case GF_COLD:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by cold!");
			cold_dam(dam, 0, NULL, killer);

			/* Strong Morgul-cold can have extra side effects. */

			/* 100% of the time if no resistance, 33% if only one resistance. */
			if ((r_ptr->flags2 & (RF2_MORGUL_MAGIC)) && (!p_ptr->immune_cold) &&
				(((!p_ptr->resist_cold) && (!p_ptr->oppose_cold)) ||
				(((!p_ptr->resist_cold) || (!p_ptr->oppose_cold)) &&
				(one_in_(3)))))
			{
				k = randint(3);

				if ((k == 1) && (dam >= 150) && (!p_ptr->sustain_con))
				{
					(void)do_dec_stat(A_CON, 1, FALSE,
						"The cold seeps into your bones.", NULL);
				}
				if ((k == 2) && (dam >= 250) && (!p_ptr->hold_life))
				{
					msg_print("A deadly chill withers your lifeforce.");
					lose_exp((r_ptr->level * 2) + (p_ptr->exp / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
				if ((k == 3) && (dam >= 400))
				{
					bool hurt = FALSE;

					if (!p_ptr->free_act)
					{
						(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 3));
						hurt = TRUE;
					}
					if (!p_ptr->resist_fear)
					{
						(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
						hurt = TRUE;
					}
					if (!p_ptr->hold_life)
					{
						/* Serious, but temporary, loss of exp. */
						lose_exp((r_ptr->level * 10) + (calc_spent_exp() / 50) *
							MON_DRAIN_LIFE, FALSE);
						hurt = TRUE;
					}

					if (hurt) msg_print("A deadly chill drives daggers into your soul!");
				}
			}

			break;
		}

		/* Standard damage -- also poisons player */
		/* Monsters with Morgul-magic have nasty poison. */
		case GF_POIS:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by poison!");

			if (p_ptr->resist_pois) dam = div_round(dam, 3);
			if (p_ptr->oppose_pois) dam = div_round(dam, 3);

			take_hit(dam, 0, NULL, killer);

			/* Poison the player. */
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				(void)set_poisoned(p_ptr->poisoned + randint(dam));
			}
			else if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
			{
				(void)set_poisoned(p_ptr->poisoned + randint(dam / 3));
			}

			/*
			 * Some nasty possible side-effects of Morgul-poison.  Poison
			 * resistance reduces the damage counted when determining effects.
			 */
			if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
			{
				/* Paralyzation. */
				if (rand_int(dam / 2 + 20) > p_ptr->skill_sav)
				{
					msg_print("The deadly vapor overwhelms you, and you faint away!");
					if (p_ptr->free_act)
						(void)set_paralyzed(p_ptr->paralyzed + 1);
					else
						(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 4));
				}

				else if ((!p_ptr->resist_blind) &&
				    (rand_int(dam + 20) > p_ptr->skill_sav))
				{
					(void)set_blind(p_ptr->blind + rand_range(8, 16),
						"The deadly vapor blinds you!");
				}
			}

			break;
		}

		/* Plasma -- Combines fire and electricity. */
		case GF_PLASMA:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by plasma!");
			elec_dam((dam + 1) / 2, 0, NULL, killer);
			fire_dam((dam + 1) / 2, 0, NULL, killer);
			break;
		}

		/* Hellfire (Udun-fire) is fire and darkness, plus nastiness. */
		case GF_HELLFIRE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by hellfire!");
			fire_dam(2 * dam / 3, 0, NULL, killer);

			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 3);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(4, 8),
					"You are blinded!");
			}
			take_hit(div_round(dam, 3), 0, NULL, killer);

			/* Test player's saving throw. */
			if (randint(5 * r_ptr->level / 4) > p_ptr->skill_sav)
			{
				if ((!p_ptr->resist_fear) || (!p_ptr->resist_chaos) ||
				    (!p_ptr->resist_confu))
				{
					msg_print("Visions of hell invade your mind!");
				}

				/* Possible fear, hallucination and confusion. */
				if (!p_ptr->resist_fear)
				{
					(void)set_afraid(p_ptr->afraid + rand_range(10, 20));
				}
				if (!p_ptr->resist_chaos)
				{
					(void)set_image(p_ptr->image + rand_range(40, 80));
				}
				if (!p_ptr->resist_confu)
				{
					(void)set_confused(p_ptr->confused + rand_range(15, 30));
				}
			}

			break;
		}

		/* Ice -- cold plus stun plus cuts */
		case GF_ICE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by something sharp!");
			cold_dam(dam, 0, NULL, killer);

			if ((!p_ptr->resist_shard)&& (one_in_(2)))
			{
				(void)set_cut(p_ptr->cut + damroll(5, 8));
			}
			if ((!p_ptr->resist_sound) && (one_in_(3)))
			{
				(void)set_stun(p_ptr->stun + randint(5 + dam / 50));
			}

			break;
		}

		/* Lite -- blinding */
		case GF_LITE:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something!");
			if ((p_ptr->resist_lite) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(dam / 8, dam / 4),
					"You are blinded by the flash!");
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something!");
			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(dam / 8, dam / 4),
					"Everying goes black!");
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Morgul-dark -- very dangerous if sufficiently powerful. */
		case GF_MORGUL_DARK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You feel a deadly blackness surround you!");

			/* Adjust damage for darkness resistance, or blind the player. */
			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + rand_range(dam / 4, dam / 2),
					"Darkness blots out your vision!");
			}
			take_hit(dam, 0, NULL, killer);

			/* Determine power of attack - usually between 25 and 350. */
			k = dam * r_ptr->level / 100;

			/* Hack - The Ringwraiths and Sauron are very dangerous. */
			if ((prefix(m_name, "Sauron, the Sorcerer")) ||
				((r_ptr->d_char == 'W') && (r_ptr->flags1 & (RF1_UNIQUE))))
			{
				if (k < 175) k = 175;
			}

			/* Various effects, depending on power. */
			if (rand_int(k) > 20)
			{
				/* Extremely frightening. */
				if (!p_ptr->resist_fear)
				{
					/* Paralyze.  If has free action, max of 1 turn. */
					if ((!p_ptr->free_act) || one_in_(3))
					{
						(void)set_paralyzed(p_ptr->paralyzed +
							(p_ptr->free_act ? 1 : rand_range(2, 4)));

						msg_print("You are paralyzed with fear!");
					}
					(void)set_afraid(p_ptr->afraid + rand_int(k));
				}

				/* Use up some of the power. */
				k = 2 * k / 3;
			}

			if (rand_int(k) > 40)
			{
				/* Poisoning */
				if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
					msg_print("You smell a hideous corpse-scent.");
					(void)set_poisoned(p_ptr->poisoned + 10 + randint(dam));
				}
				else if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
				{
					msg_print("You smell a nasty corpse-scent.");
					(void)set_poisoned(p_ptr->poisoned + 5 + randint(dam/3));
				}

				/* Use up some of the power. */
				k = 2 * k / 3;
			}

			if (rand_int(k) > 80)
			{
				/* Reduce experience. */
				if (p_ptr->hold_life)
				{
					if (one_in_(2))
					{
						msg_print("You feel your life slipping away!");
						lose_exp((r_ptr->level * 2) + (calc_spent_exp() / 1000) *
							MON_DRAIN_LIFE, FALSE);
					}
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp((r_ptr->level * 4) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
				/* Use up some of the power. */
				k = 2 * k / 3;
			}

			if (rand_int(k) > 100)
			{
				/* Disenchantment. */
				if (!p_ptr->resist_disen)
				{
					msg_print("You feel a force attacking the magic around you.");
					(void)apply_disenchant(0);
				}

				/* Use up some of the power. */
				k = 2 * k / 3;
			}

			if (rand_int(k) > 120)
			{
				/* Loss of memory. */
				if (rand_int(k) > p_ptr->skill_sav)
				{
					(void)lose_all_info("The blackness invades your mind; your memories fade away.");
				}
				/* Use up some of the power. */
				k = 2 * k / 3;
			}

			if (rand_int(k) > 160)
			{
				/* Dagger bearing the Black Breath (rare). */
				msg_print("Out of the uttermost shadow leaps a perilous blade!");

				if (p_ptr->black_breath == FALSE)
				{
					/* Message. */
					msg_print("You feel the Black Breath slowly draining you of life...");
					p_ptr->black_breath = TRUE;
				}
				else
				{
					msg_print("You feel the Black Breath sucking away your lifeforce!");
					lose_exp(calc_spent_exp() / 20, FALSE);
					check_experience();
				}
			}

			break;
		}

		/* Pure confusion */
		case GF_CONFUSION:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by something confusing!");
			if (p_ptr->resist_confu)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_confused(p_ptr->confused + rand_range(dam / 4, dam / 2));
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Sound -- mostly stunning and confusing, can paralyze */
		case GF_SOUND:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are blasted by sound.");


			/* Resistance reduces damage, negates all side effects. */
			if (p_ptr->resist_sound)
			{
				dam = div_round(dam, 2);
			}

			/* Side effects of powerful sound attacks. */
			else if (dam > rand_int(300))
			{
				/* Confuse the player (a little). */
				if (!p_ptr->resist_confu)
				{
					k = (randint((dam > 400) ? 21 : (1 + dam / 20)));
					(void)set_confused(p_ptr->confused + k);
				}

				/* Stun the player. */
				k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);

				/* Sometimes, paralyze the player briefly. */
				if (rand_int(dam) > p_ptr->skill_sav)
				{
					/* Warning */
					msg_print("The noise shatters your wits, and you struggle to recover.");

					/* Hack - directly reduce player energy. */
					p_ptr->energy -= (s16b)rand_int(dam / 2);
					if (p_ptr->energy < 0) p_ptr->energy = 0;
				}
			}
			take_hit(dam, 0, NULL, killer);

			/* Resistance to sound - much less inventory destruction. */
			if (p_ptr->resist_sound) k = dam / 3;
			else                     k = dam;

			/* Blow up flasks and potions sometimes. */
			if (k > 12)
			{
				inven_damage(set_cold_destroy,
					((k / 13 > 30) ? 30 : k / 13));
			}

			break;
		}

		/* Shards -- mostly cutting.  Shields may offer some protection. */
		case GF_SHARD:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Test for partial shield protection. */
			if ((inventory[INVEN_ARM].k_idx) &&
				(inventory[INVEN_ARM].tval == TV_SHIELD) &&
				(!p_ptr->shield_on_back) &&
				(inventory[INVEN_ARM].ac >
				rand_int(MAX_SHIELD_BASE_AC * 2)))
			{
				dam = 2 * dam / 3;
			}

			if (fuzzy) msg_print("You are hit by something sharp!");
			if (p_ptr->resist_shard)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_cut(p_ptr->cut + dam);
			}

			/* Resistance to shards - much less inventory destruction. */
			if (p_ptr->resist_shard) k = dam / 3;
			else k = dam;

			/* Blow up flasks and potions on rare occasions. */
			if (k > 19) inven_damage(set_cold_destroy,
				((k / 20 > 20) ? 20 : k / 20));

			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			(void)set_slow(p_ptr->slow + 2 + rand_int(dam / 15));
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Gravity -- stunning and slowness. */
		case GF_GRAVITY:
		{
			(void)set_slow(p_ptr->slow + randint(dam / 20));
			if (!p_ptr->resist_sound)
			{
				k = randint((dam > 200) ? 20 : (dam / 10));
				(void)set_stun(p_ptr->stun + k);
			}
			take_hit(dam, 0, NULL, killer);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Force -- mostly stun */
		case GF_FORCE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by a shock wave!");
			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint(dam / 10 + 5));
			}
			take_hit(dam, 0, NULL, killer);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Water -- stun/confuse */
		case GF_WATER:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Message */
			if (fuzzy) msg_print("You are hit by water!");

			if (dam > randint(200))
			{
				if ((!p_ptr->resist_sound) && (one_in_(2)))
				{
					(void)set_stun(p_ptr->stun + randint(5 + dam / 10));
				}
				if ((!p_ptr->resist_confu) && (one_in_(2)))
				{
					(void)set_confused(p_ptr->confused + rand_int(4));
				}
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Wind -- confusion, but rarely if res_confu */
		case GF_WIND:
		{
			if (fuzzy) msg_print("You are buffeted by winds!");
			if (dam > randint(200))
			{
				if ((!p_ptr->resist_confu) || (one_in_(6)))
				{
					msg_print("You are spun until dizzy!");
					(void)set_confused(p_ptr->confused + rand_range(2, 3));
				}
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			/* Take damage */
			take_hit(dam, 0, NULL, killer);

			break;
		}

		/* Storm -- Electricity, also acid (acidic water) and cold. */
		/* Inventory damage, stunning, confusing */
		case GF_STORM:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Message */
			if (fuzzy) msg_print("You are enveloped in a storm!");

			/* Pure (wind-driven water + flying objects) damage. */
			take_hit(dam / 2, 0, NULL, killer);

			/* Electrical damage. */
			if (one_in_(3))
			{
				/* Lightning strikes. */
				elec_dam(dam, 0, "You are struck by lightning!", killer);
			}

			/* Lightning didn't strike - at least not directly. */
			else elec_dam(dam / 4, 0, NULL, killer);

			/* Possibly cold and/or acid damage. */
			if (one_in_(2))
			{
				if (!one_in_(3)) msg = "You are blasted by freezing winds.";
				else msg = "You are bombarded with hail.";

				cold_dam(dam / 4, 0, msg, killer);
			}
			if (one_in_(2))
			{
				acid_dam(dam / 4, 0, "You are drenched by acidic rain.", killer);
			}

			/* Sometimes, confuse the player. */
			if ((one_in_(2)) && (!p_ptr->resist_confu))
			{
				(void)set_confused(p_ptr->confused + rand_int(6));
			}

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nexus -- Effects processed later, in "project_t()" */
		case GF_NEXUS:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_nexus)
			{
				dam = div_round(dam, 2);
			}
			take_hit(dam, 0, NULL, killer);

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by nether!");
			if (p_ptr->resist_nethr)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				if (p_ptr->hold_life && (!one_in_(4)))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp((r_ptr->level) + (calc_spent_exp() / 1000) *
						MON_DRAIN_LIFE, FALSE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp((r_ptr->level * 2) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Chaos -- many effects.  */
		case GF_CHAOS:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by chaos!");
			if (p_ptr->resist_chaos)
			{
				dam = div_round(dam, 2);
			}
			if (!p_ptr->resist_chaos)
			{
				if (!p_ptr->resist_confu)
				{
					(void)set_confused(p_ptr->confused + rand_range(dam / 4, dam / 2));
				}
				(void)set_image(p_ptr->image + rand_range(dam / 4, dam / 2));
			}

			/* Can lose exp */
			if (one_in_(2))
			{
				if (!p_ptr->resist_nethr && !p_ptr->resist_chaos)
				{
					if (p_ptr->hold_life && (!one_in_(4)))
					{
						msg_print("You keep hold of your life force!");
					}
					else if (p_ptr->hold_life)
					{
						msg_print("You feel your life slipping away!");
						lose_exp((r_ptr->level * 1) + (calc_spent_exp() / 1000) *
							MON_DRAIN_LIFE, FALSE);
					}
					else
					{
						msg_print("You feel your life draining away!");
						lose_exp((r_ptr->level * 5) + (calc_spent_exp() / 100) *
							MON_DRAIN_LIFE, FALSE);
					}
				}
			}

			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (p_ptr->resist_disen)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)apply_disenchant(dam);
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Time */
		case GF_TIME:
		{
			if (fuzzy) msg_print("You are hit by something strange!");

			switch (randint(10))
			{
				case 1: case 2: case 3: case 4: case 5:
				{
					msg_print("You feel life has clocked back.");
					lose_exp((r_ptr->level * 2) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
					break;
				}

				case 6: case 7: case 8: case 9:
				{
					switch (randint(A_MAX))
					{
						case 1: k = A_STR; act = "strong"; break;
						case 2: k = A_INT; act = "bright"; break;
						case 3: k = A_WIS; act = "wise"; break;
						case 4: k = A_DEX; act = "agile"; break;
						case 5: k = A_CON; act = "hale"; break;
						case 6: k = A_CHR; act = "beautiful"; break;
					}

					msg_format("You're not as %s as you used to be...", act);

					p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
					if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					p_ptr->update |= (PU_BONUS);
					break;
				}

				case 10:
				{
					msg_print("You're not as powerful as you used to be...");

					/* Drain all stats */
					for (k = 0; k < A_MAX; k++)
					{
						p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
						if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
					}
					p_ptr->update |= (PU_BONUS);
					break;

				}
			}
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by something!");
			take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Holy Orb -- Adjust damage by various factors */
		case GF_HOLY_ORB:
		{
			int factor = 100;

			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Piety reduces damage */
			factor -= get_skill(S_PIETY, 0, 100);

			/* Students of necromancy get hit hard */
			if (p_ptr->realm == NECRO) factor += get_skill(S_MAGIC, 0, 50);

			if (fuzzy) msg_print("You are hit by something!");

			take_hit(dam * factor / 100, 0, NULL, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		case GF_BLACK_ORB:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by a doomspell!");
			take_hit(dam, 0, NULL, killer);

			set_stun(p_ptr->stun + rand_range(10, HVY_STUN));

			break;
		}

		/* Drain Life */
		case GF_DEATH:
		{
			int resist = 0;

			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are hit by life-draining energies!");

			/* Allow resistance */
			if (p_ptr->resist_nethr) resist++;
			if (p_ptr->hold_life)    resist++;

			/* Apply resistance */
			dam /= (1 + resist);

			/* Life draining */
			if (rand_int(100) < (resist * 25))
			{
				msg_print("You keep hold of your life force!");
			}
			else if (resist)
			{
				msg_print("You feel your life slipping away!");
				lose_exp((dam) + (calc_spent_exp() / 500) *
					MON_DRAIN_LIFE, FALSE);
			}
			else
			{
				msg_print("You feel your life draining away!");
				lose_exp((dam * 3) + (calc_spent_exp() / 50) *
					MON_DRAIN_LIFE, FALSE);
			}

			take_hit(dam, 0, NULL, killer);

			break;
		}

		/* Spores - poison, cause disease */
		case GF_SPORE:
		{
			int power = (who > 0 ? r_ptr->level : p_ptr->depth);

			if (fuzzy) msg_print("You feel spores all around you...");

			/* Affected slightly by terrain. */
			dam += terrain_adjustment / 2;

			take_hit(dam, 0, NULL, killer);

			/* Poison */
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				set_poisoned(p_ptr->poisoned + randint(dam * 2));
			}

			/* Disease */
			if (randint(power) >= 15)
			{
				int dummy = randint(dam * 2);
				disease(&dummy);
			}

			break;
		}

		/* Curse a character, attempt to slow, daze, confuse, frighten. */
		case GF_CURSE:
		{
			int curse = rand_int(4);

			/* Attempt a saving throw */
			if (get_skill(S_SAVE, 0, 100) > rand_int(100))
			{
				msg_print("You resist the curse!");
				break;
			}

			/* Allow at least one, and possibly several effects */
			while (TRUE)
			{
				/* Effect 0 -- slow */
				if ((curse == 0) && (!p_ptr->free_act))
				{
					(void)set_slow(p_ptr->slow + rand_range(5, 5 + dam / 3));
				}

				/* Effect 1 -- confusion */
				if ((curse == 1) && (!p_ptr->resist_confu))
				{
					(void)set_confused(p_ptr->confused + rand_range(5, 5 + dam / 3));
				}

				/* Effect 2 -- panic */
				if ((curse == 2) && (!p_ptr->resist_fear))
				{
					(void)set_afraid(p_ptr->afraid + rand_range(5, 5 + dam / 3));
				}

				/* Effect 3 -- stun */
				if ((curse == 3) && (!p_ptr->resist_sound))
				{
					int tmp = 3;
					if (p_ptr->resist_sound) tmp = 10;

					(void)set_stun(p_ptr->stun + rand_range(5, 5 + dam / tmp));
				}

				/* Roll for next curse */
				curse = rand_int(16);

				/* End of curses */
				if (curse >= 4) break;
			}

			break;
		}

		/* Mental attacks -- From PsiAngband */
		case GF_PSI:
		{
			int psi_resists = 0;
			int tmp;

			bool conf = FALSE;
			bool do_blind = FALSE;
			bool fear = FALSE;
			bool paralyze = FALSE;
			bool slow = FALSE;
			bool stun = FALSE;
			bool energy = FALSE;

			/* Hack -- limit damages */
			if (dam > 150) dam = 150;


			if (fuzzy) msg_print("Your mind is hit by mental energy!");

			/* Get character resistance to mental attacks */
			if (p_ptr->mental_barrier) psi_resists += 5;
			if (p_ptr->holy) psi_resists += 2;
			if (p_ptr->skill_sav < randint(100)) psi_resists += 2;
			if ((p_ptr->berserk) && (p_ptr->skill_sav < randint(100)))
				 psi_resists--;
			if (p_ptr->confused) psi_resists--;
			if (p_ptr->image) psi_resists--;
			if (p_ptr->stun) psi_resists--;
			if ((p_ptr->afraid) && (p_ptr->skill_sav < randint(100)))
				 psi_resists--;

			/* Get effect factor (dam / resist) */
			if (psi_resists < -2) psi_resists = -2;
			tmp = 2 * dam / (psi_resists + 3);


			/* Mental attacks ignore most resists */

			/* Can be confused */
			if (tmp > rand_int(p_ptr->skill_sav)) conf = TRUE;

			/* Can be blinded */
			if (tmp > 5 + rand_int(p_ptr->skill_sav * 2)) do_blind = TRUE;

			/* Can be frightened */
			if (tmp > 2 + rand_int(p_ptr->skill_sav)) fear = TRUE;


			/* Can be paralyzed, but only without free action */
			if (!p_ptr->free_act)
			{
				if (tmp > 5 + rand_int(p_ptr->skill_sav)) paralyze = TRUE;
			}

			/* Those with free action can only be slowed */
			else if (tmp > 10 + rand_int(p_ptr->skill_sav)) slow = TRUE;

			/* Can be stunned */
			if (tmp > 5 + rand_int(p_ptr->skill_sav)) stun = TRUE;

			/* Can be robbed of energy */
			if (tmp > 5 + rand_int(p_ptr->skill_sav)) energy = TRUE;


			/* Something happened */
			if (conf || do_blind || fear || paralyze || slow || stun || energy)
			{
				if (!fuzzy) msg_print("Your mind is blasted by mental energy.");

				if (conf) set_confused(p_ptr->confused + 2 +
					rand_range(dam / 30, dam / 10));
				if (do_blind) set_blind(p_ptr->blind + 2 +
					rand_range(dam / 30, dam / 10), "You are blinded!");
				if (fear) set_afraid(p_ptr->afraid + 2 +
					rand_range(dam / 15, dam / 5));
				if (paralyze) set_paralyzed(p_ptr->paralyzed + 2 +
					rand_range(dam / 30, dam / 10));
				if (slow) set_slow(p_ptr->slow + 2 +
					rand_range(dam / 30, dam / 10));
				if (stun) set_stun(p_ptr->stun + 5 +
					rand_range(dam / 20, dam / 10));
				if (energy)
				{
					msg_print("You lose control of your body for a moment!");
					p_ptr->energy -= randint(MIN(p_ptr->energy, 25));
				}
			}
			else
			{
				dam /= 3;
				if (p_ptr->chp > dam)
					msg_print("You resist the mental attack!");
			}

			/* Take some damage */
			take_hit(dam, 0, NULL, killer);

			break;
		}

		/* Speed */
		case GF_DO_SPEED:
		{
			/* Speed up */
			(void)set_fast(p_ptr->fast + 5);

			/* No damage */
			dam = 0;

			break;
		}

		/* Slow */
		case GF_DO_SLOW:
		{
			/* Slow down */
			(void)set_slow(p_ptr->slow + 10);

			/* No damage */
			dam = 0;

			break;
		}

		/* Healing */
		case GF_DO_HEAL:
		{
			/* Heal */
			(void)hp_player(dam);

			/* No damage */
			dam = 0;

			break;
		}

		/* Enlightenment */
		case GF_ENLIGHTENMENT:
		{
			/* Learn about self -- sometimes */
			if (dam > randint(400)) self_knowledge(TRUE);

			/* No damage */
			dam = 0;

			break;
		}

		/* Gain level */
		case GF_GAIN_LEVEL:
		{
			/* Raise skills, see if character noticed */
			if (raise_skills(dam * 100))
			{
				obvious = TRUE;
			}

			/* No damage */
			dam = 0;

			break;
		}

		/* Default */
		default:
		{
			/* No damage */
			dam = 0;

			break;
		}
	}

	/* Disturb */
	disturb(1, 0);

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Helper function for "project()" below.
 *
 * Handle movement of monsters and the player.  Handle the alteration of
 * grids that affect damage.  -LM-
 *
 * This function only checks grids marked with the CAVE_TEMP flag.  To
 * help creatures get out of each other's way, this function processes
 * from outside in.
 *
 * This accomplishes three things:  A creature now cannot be damaged/blinked
 * more than once in a single projection, if all teleport functions also
 * clear the CAVE_TEMP flag.  Also, terrain now affects damage taken, and
 * only then gets altered.  Also, any summoned creatures don't get hurt
 * by the magics that gave them birth.
 *
 * XXX XXX -- Hack -- because the CAVE_TEMP flag may be erased by certain
 * updates, we must be careful not to allow any of the teleport functions
 * called by this function to ask for one.  This work well in practice, but
 * is a definite hack.
 *
 * This function assumes that most messages have already been shown.
 */
static bool project_t(int who, int y, int x, int dam, int typ, u32b flg)
{
	monster_type *m_ptr = NULL;
	monster_race *r_ptr = NULL;

	char m_name[80];

	int k, d;

	bool seen = FALSE;
	bool obvious = FALSE;

	bool affect_player = FALSE;
	bool affect_monster = FALSE;

	int do_dist = 0;

	/* Assume no note */
	cptr note = NULL;

	/* Only process marked grids. */
	if (!cave_info[y][x] & (CAVE_TEMP)) return (FALSE);

	/* Clear the cave_temp flag.  (this is paranoid) */
	cave_info[y][x] &= ~(CAVE_TEMP);


	/* Projection will be affecting a player. */
	if ((flg & (PROJECT_PLAY)) && (cave_m_idx[y][x] < 0))
		affect_player = TRUE;

	/* Projection will be affecting a monster. */
	if ((flg & (PROJECT_KILL)) && (cave_m_idx[y][x] > 0))
	{
		affect_monster = TRUE;
		m_ptr = &m_list[cave_m_idx[y][x]];
		r_ptr = &r_info[m_ptr->r_idx];
	}

	if (affect_player)
	{
		obvious = TRUE;
	}

	if (affect_monster)
	{
		/* Sight check. */
		if (m_ptr->ml) seen = TRUE;

		/* Get the monster name (before teleporting) */
		monster_desc(m_name, m_ptr, 0);
	}

	/* Analyze the type */
	switch (typ)
	{
		/* Reasonably strong acid can dissolve rock. */
		case GF_ACID:
		{
			if (dam > rand_range(200, 400))
			{
				/* Forget the previous feature */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Granite becomes magma or quartz */
				if ((cave_feat[y][x] >= FEAT_WALL_EXTRA) &&
				    (cave_feat[y][x] <= FEAT_WALL_SOLID))
				{
					if (one_in_(2)) cave_set_feat(y, x, FEAT_MAGMA);
					else            cave_set_feat(y, x, FEAT_QUARTZ);
				}

				/* Non-granite rock become rubble */
				else if ((cave_feat[y][x] >= FEAT_MAGMA) &&
				         (cave_feat[y][x] <= FEAT_QUARTZ_K))
				{
					/* Treasure seam -- make gold */
					if ((cave_feat[y][x] >= FEAT_MAGMA) &&
					    (cave_feat[y][x] <= FEAT_QUARTZ_K))
					{
						object_type *i_ptr;
						object_type forge;

						/* Get local object */
						i_ptr = &forge;

						/* Make some gold */
						if (make_gold(i_ptr))
						{
							/* Drop it in the dungeon */
							drop_near(i_ptr, -1, y, x);
						}
					}

					cave_set_feat(y, x, FEAT_RUBBLE);
				}

				/* Rubble becomes floor */
				else if (cave_feat[y][x] == FEAT_RUBBLE)
					cave_set_feat(y, x, FEAT_FLOOR);
			}
			break;
		}

		/* Sufficiently intense cold can solidify lava. */
		case GF_COLD:
		case GF_ICE:
		{
			if (cave_feat[y][x] == FEAT_LAVA)
			{
				if (dam > rand_range(300, 1200))
				{
					/* Forget the lava */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Destroy the lava */
					if (!one_in_(3)) cave_set_feat(y, x, FEAT_FLOOR);
					else             cave_set_feat(y, x, FEAT_RUBBLE);
				}
			}

			break;
		}

		/* Fire and plasma can create lava, evaporate water, and burn trees. */
		case GF_FIRE:
		case GF_HELLFIRE:
		case GF_PLASMA:
		{
			/* Can create lava if extremely powerful. */
			if (dam > rand_range(600, 2400))
			{
				if ((cave_feat[y][x] == FEAT_FLOOR) ||
					(cave_feat[y][x] == FEAT_RUBBLE))
				{
					/* Forget the floor or rubble. */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Make lava. */
					cave_set_feat(y, x, FEAT_LAVA);
				}
			}

			/* Can boil water if very strong. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				k = 0;

				/* Look around for nearby water. */
				for (d = 0; d < 8; d++)
				{
					/* Extract adjacent (legal) location */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Count the water grids. */
					if (cave_feat[yy][xx] == FEAT_WATER) k++;
				}

				/* Is the fire strong enough? Large ponds are difficult
				 * to evaporate, as Smaug found out the hard way.
				 */
				if (dam > randint(600 + k * 300) + 200)
				{
					/* Forget the water */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Destroy the water */
					cave_set_feat(y, x, FEAT_FLOOR);
				}
			}

			/* Can burn trees if strong. */
			if ((cave_feat[y][x] == FEAT_TREE) && (dam > rand_range(100, 400)))
			{
				/* Forget the tree */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the tree */
				cave_set_feat(y, x, FEAT_FLOOR);
			}

			break;
		}

		/* Gravity -- totally random blink */
		case GF_GRAVITY:
		{
			if (affect_player)
			{
				if (((p_ptr->resist_nexus) || (p_ptr->ffall)) &&
				    (!one_in_(3)))
				{
					msg_print("You barely hold your ground.");
				}
				else
				{
					msg_print("Gravity warps around you.");
					teleport_player(6, FALSE);
				}
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 4 + div_round(dam, 25);

				/* Resist even when affected */
				if (r_ptr->flags4 & (RF4_BRTH_GRAV)) do_dist = 0;
				else if (r_ptr->flags4 & (RF4_BRTH_NEXUS)) do_dist /= 4;
				else if (r_ptr->flags3 & (RF3_RES_NEXUS)) do_dist /= 2;
				else if (r_ptr->flags3 & (RF3_RES_TPORT))
					do_dist = 2 * do_dist / 3;

				/* Big, heavy monsters */
				if (strchr("DGP#", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) do_dist /= 2;

				if (seen) obvious = TRUE;
			}

			break;
		}

		/* Force -- thrust target away from caster */
		case GF_FORCE:
		{
			if (affect_monster)
			{
				/* Force breathers are immune */
				if (r_ptr->flags4 & (RF4_BRTH_FORCE)) break;

				/* Big, heavy monsters */
				if (strchr("DGP#", r_ptr->d_char)) dam /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) dam /= 2;
			}

			if ((affect_monster) || (affect_player))
			{
				/* Thrust monster or player away. */
				thrust_away(who, y, x, 2 + div_round(dam, 15));

				/* Hack -- get new location */
				if (affect_monster)
				{
					y = m_ptr->fy;
					x = m_ptr->fx;
				}
			}

			break;
		}

		/* Wind can move monsters and the player about */
		case GF_WIND:
		{
			if (affect_player)
			{
				/* Throw distance depends on weight and strength */
				int dist = div_round(25 * dam, p_ptr->wt);
				if (dist > 10) dist = 10;

				/* Feather fall greatly reduces the effect of wind */
				if (p_ptr->ffall) dist = (dist + 2) / 3;

				/* Messages */
				if (dist >= 6)
					msg_print("The wind grabs you, and whirls you around!");
				else if (dist >= 2)
					msg_print("The wind buffets you about.");

				/* Throw the player around unsafely. */
				teleport_player(dist, FALSE);
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 3 + div_round(dam, 25);

				/* Big, heavy monsters (or ghosts) */
				if (strchr("DGP#G", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) do_dist /= 2;
			}

			break;
		}

		/* Water/storm can make pools.  Water nearby makes it easier. */
		case GF_WATER:
		case GF_STORM:
		{
			if ((typ == GF_STORM) && (affect_player))
			{
				/* Throw distance depends on weight and strength */
				int dist = div_round(10 * dam, p_ptr->wt);
				if (dist > 6) dist = 6;

				/* Feather fall greatly reduces the effect of wind */
				if (p_ptr->ffall) dist = (dist + 2) / 3;

				/* Messages */
				if (dist >= 6)
					msg_print("The wind grabs you, and whirls you around!");
				else if (dist >= 2)
					msg_print("The wind buffets you about.");

				/* Throw the player around unsafely. */
				teleport_player(dist, FALSE);
			}

			if ((typ == GF_STORM) && (affect_monster))
			{
				/* Damage-variable throw distance */
				do_dist = 3 + div_round(dam, 25);

				/* Big, heavy monsters */
				if (strchr("DGP#", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) do_dist /= 2;
			}

			/* Require strong attack.  Require floor. */
			if ((dam >= 60) && (cave_feat[y][x] == FEAT_FLOOR))
			{
				k = 0;

				/* Look around for nearby water. */
				for (d = 0; d < 8; d++)
				{
					/* Extract adjacent (legal) location */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Count the water grids. */
					if (cave_feat[yy][xx] == FEAT_WATER) k++;
				}

				/* If enough water available, make pool. */
				if ((dam + (k * 20)) > rand_range(100, 500))
				{
					/* Forget the floor */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Create water */
					cave_set_feat(y, x, FEAT_WATER);
				}
			}

			/* Solidify lava */
			if (cave_feat[y][x] == FEAT_LAVA)
			{
				if (dam > rand_range(200, 700))
				{
					/* Forget the lava */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Destroy the lava */
					if (!one_in_(3)) cave_set_feat(y, x, FEAT_FLOOR);
					else             cave_set_feat(y, x, FEAT_RUBBLE);
				}
			}
			break;
		}

		/* Nexus - movement */
		case GF_NEXUS:
		{
			if (affect_player)
			{
				if (!p_ptr->resist_nexus)
				{
					/* Get caster */
					monster_type *n_ptr = &m_list[who];

					/* Various effects. */
					apply_nexus(n_ptr->fy, n_ptr->fx, dam);
				}
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 4 + div_round(dam, 10);

				/* Resist even when affected */
				if      (r_ptr->flags4 & (RF4_BRTH_NEXUS)) do_dist = 0;
				else if (r_ptr->flags3 & (RF3_RES_NEXUS)) do_dist /= 4;
				else if (r_ptr->flags3 & (RF3_RES_TPORT))
					do_dist = 2 * do_dist / 3;
			}
			break;
		}

		/* Chaos - summoning */
		case GF_CHAOS:
		{
			/* Require chaos tiles. */
			summon_index_type = MON_CHAOS_TILE;

			/* Attempt to create a chaos tile */
			if (summon_specific(y, x, FALSE, 100, SUMMON_INDEX))
			{
				if (player_can_see_bold(y, x))
					msg_print("The floor is corrupted by chaos!");
			}
			break;
		}

		/* Mana - summoning */
		case GF_MANA:
		{
			/* Require mana flies or mana vortexes. */
			summon_index_type = MON_MANA_FLY;

			if ((p_ptr->depth > 55) && (one_in_(2)))
				summon_index_type = MON_MANA_VORTEX;

			/* Attempt to create a mana fly or mana vortex */
			if (summon_specific(y, x, FALSE, 100, SUMMON_INDEX))
			{
				if (player_can_see_bold(y, x))
					msg_print("The raw magic coalesces into a creature of pure mana!");
			}
			break;
		}

		/* Teleport to */
		case GF_COME_HITHER:
		{
			if (affect_player)
			{
				/* Get caster */
				monster_type *n_ptr = &m_list[who];

				/* Teleport character to monster */
				teleport_player_to(n_ptr->fy, n_ptr->fx, 0, FALSE);
			}

			if (affect_monster)
			{
				u16b grid[8];
				int grids = 0;
				int i;

				/* Scan the adjacent grids */
				for (i = 0; i < 8; i++)
				{
					y = p_ptr->py + ddy_ddd[i];
					x = p_ptr->px + ddx_ddd[i];

					/* Must be fully in bounds */
					if (!in_bounds_fully(y, x)) continue;

					/* This grid is OK for this monster */
					if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE))
					{
						/* Save this grid */
						grid[grids++] = GRID(y, x);
					}
				}

				/* If we have at least one legal grid, choose one at random */
				if (grids) i = rand_int(grids);

				/* Cancel on failure */
				else break;

				/* Get the location of this grid */
				y = GRID_Y(grid[i]);
				x = GRID_X(grid[i]);

				/* Teleport the monster there */
				monster_swap(m_ptr->fy, m_ptr->fx, y, x);

				/* Monster loses a turn  XXX */
				i = extract_energy[m_ptr->mspeed];
				if (m_ptr->energy < i) m_ptr->energy = 0;
				else                   m_ptr->energy -= i;

				/* Remember the monster index  XXX */
				p_ptr->came_hither = cave_m_idx[y][x];
			}
			break;
		}

		/* Teleport away - movement */
		case GF_AWAY:
		{
			if (affect_player)
			{
				teleport_player(dam, FALSE);
			}

			if (affect_monster)
			{
				/* Obvious */
				if (seen) obvious = TRUE;

				/* Prepare to teleport */
				do_dist = dam;

				/* Resist even when affected */
				if (r_ptr->flags3 & (RF3_RES_TPORT)) do_dist /= 3;
				else if (r_ptr->flags4 & (RF4_BRTH_NEXUS)) do_dist /= 2;
				else if (r_ptr->flags3 & (RF3_RES_NEXUS)) do_dist /= 2;
			}
			break;
		}

		/* All other projection types have no effect. */
		default:
		{
			return (FALSE);
		}
	}

	/* Handle teleportation of monster */
	if (do_dist)
	{
		/* Obvious */
		if (seen) obvious = TRUE;

		/* Teleport */
		teleport_away(cave_m_idx[y][x], do_dist);

		/* No movement */
		if ((y == m_ptr->fy) && (x == m_ptr->fx))
		{
			/* No message */
		}
		/* Visible (after teleport) */
		else if (m_ptr->ml)
		{
			/* No message */
		}
		else
		{
			/* Message */
			note = " disappears!";
		}

		/* Hack -- get new location */
		if (affect_monster)
		{
			y = m_ptr->fy;
			x = m_ptr->fx;
		}
	}

	if (affect_monster)
	{
		/* Give detailed messages if visible */
		if (note && seen)
		{
			msg_format("%^s%s", m_name, note);
		}

		/* Update the monster */
		(void)update_mon(cave_m_idx[y][x], FALSE, FALSE);

		/* Update monster recall window */
		if (p_ptr->monster_race_idx == m_ptr->r_idx)
		{
			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);
		}
	}

	return (obvious);
}


/*
 * Calculate and store the arcs used to make starbursts.
 */
static void calc_starburst(int height, int width, byte *arc_first,
	byte *arc_dist, int *arc_num)
{
	int i;
	int size, dist, vert_factor;
	int degree_first, center_of_arc;


	/* Note the "size" */
	size = 2 + div_round(width + height, 22);

	/* Ask for a reasonable number of arcs. */
	*arc_num = 8 + (height * width / 80);
	*arc_num = rand_spread(*arc_num, 3);
	if (*arc_num < 8)  *arc_num = 8;
	if (*arc_num > 45) *arc_num = 45;

	/* Determine the start degrees and expansion distance for each arc. */
	for (degree_first = 0, i = 0; i < *arc_num; i++)
	{
		/* Get the first degree for this arc (using 180-degree circles). */
		arc_first[i] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(180, *arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 180 * (i+1) / *arc_num)
		    degree_first = 180 * (i+1) / *arc_num;
		if (degree_first > (180 + *arc_num) * (i+1) / *arc_num)
		    degree_first = (180 + *arc_num) * (i+1) / *arc_num;


		/* Get the center of the arc (convert from 180 to 360 circle). */
		center_of_arc = degree_first + arc_first[i];

		/* Get arc distance from the horizontal (0 and 180 degrees) */
		if      (center_of_arc <=  90) vert_factor = center_of_arc;
		else if (center_of_arc >= 270) vert_factor = ABS(center_of_arc - 360);
		else                           vert_factor = ABS(center_of_arc - 180);

		/*
		 * Usual case -- Calculate distance to expand outwards.  Pay more
		 * attention to width near the horizontal, more attention to height
		 * near the vertical.
		 */
		dist = ((height * vert_factor) + (width * (90 - vert_factor))) / 90;

		/* Randomize distance (should never be greater than radius) */
		arc_dist[i] = rand_range(dist / 4, dist / 2);

		/* Keep variability under control (except in special cases). */
		if ((dist != 0) && (i != 0))
		{
			int diff = arc_dist[i] - arc_dist[i-1];

			if (ABS(diff) > size)
			{
				if (diff > 0)
					arc_dist[i] = arc_dist[i-1] + size;
				else
					arc_dist[i] = arc_dist[i-1] - size;
			}
		}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (TRUE)
	{
		int diff = arc_dist[*arc_num - 1] - arc_dist[0];

		if (ABS(diff) > size)
		{
			if (diff > 0)
				arc_dist[*arc_num - 1] = arc_dist[0] + size;
			else
				arc_dist[*arc_num - 1] = arc_dist[0] - size;
		}
	}




}


/*
 * Generic "beam"/"bolt"/"ball" projection routine.
 *   -BEN-, -LM-
 *
 * Input:
 *   who:             Index of "source" monster (negative for the character)
 *   rad:             Radius of explosion, or length of beam, or maximum
 *                    length of arc from the source.
 *   y0, x0:          Source location (Location to travel from)
 *   y1, x1:          Target location (Location to travel towards)
 *   dam:             Base damage to apply to monsters, terrain, objects,
 *                    or player
 *   typ:             Type of projection (fire, frost, dispel demons etc.)
 *   flg:             Bit flags that control projection behavior
 *   degrees:         How wide an arc spell is (in degrees).
 *   source_diameter: how wide the source diameter is.
 *
 * Return:
 *   TRUE if any effects of the projection were observed, else FALSE
 *
 *
 * At present, there are five major types of projections:
 *
 * Point-effect projection:  (no PROJECT_BEAM flag, radius of zero, and either
 *   jumps directly to target or has a single source and target grid)
 * A point-effect projection has no line of projection, and only affects one
 *   grid.  It is used for most area-effect spells (like dispel evil) and
 *   pinpoint strikes.
 *
 * Bolt:  (no PROJECT_BEAM flag, radius of zero, has to travel from source to
 *   target)
 * A bolt travels from source to target and affects only the final grid in its
 *   projection path.  If given the PROJECT_STOP flag, it is stopped by any
 *   monster or character in its path (at present, all bolts use this flag).
 *
 * Beam:  (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through
 *   with full damage.  It is never stopped by monsters in its path.  Beams
 *   may never be combined with any other projection type.
 *
 * Ball:  (positive radius, unless the PROJECT_ARC flag is set)
 * A ball travels from source towards the target, and always explodes.  Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in
 *   its path.  If given a target, it will explode on that target.  If a
 *   wall is in the way, it will explode against the wall.  If a ball reaches
 *   MAX_RANGE without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc:  (positive radius, with the PROJECT_ARC flag set)
 * An arc is a portion of a source-centered ball that explodes outwards
 *   towards the target grid.  Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area.  The width of arc spells is con-
 *   trolled by degrees.
 * An arc is created by rejecting all grids that form the endpoints of lines
 *   whose angular difference (in degrees) from the centerline of the arc is
 *   greater than one-half the input "degrees".  See the table "get_
 *   angle_to_grid" in "util.c" for more information.
 * Note:  An arc with a value for degrees of zero is actually a beam of
 *   defined length.
 *
 * Projections that affect all monsters in LOS are handled through the use
 *   of "project_los()", which applies a single-grid projection to individual
 *   monsters.  Projections that light up rooms or affect all monsters on the
 *   level are more efficiently handled through special functions.
 *
 *
 * Variations:
 *
 * PROJECT_STOP forces a path of projection to stop at the first occupied
 *   grid it hits.  This is used with bolts, and also by ball spells
 *   travelling in a specific direction rather than towards a target.
 *
 * PROJECT_THRU allows a path of projection towards a target to continue
 *   past that target.
 *
 * PROJECT_JUMP allows a projection to immediately set the source of the pro-
 *   jection to the target.  This is used for all area effect spells (like
 *   dispel evil), and can also be used for bombardments.
 *
 * PROJECT_WALL allows a projection, not just to affect one layer of any
 *   passable wall (rubble, trees), but to affect the surface of any wall.
 *   Certain projection types always have this flag.
 *
 * PROJECT_PASS allows projections to ignore walls completely.
 *   Certain projection types always have this flag.
 *
 * PROJECT_HIDE erases all graphical effects, making the projection
 *   invisible.
 *
 * PROJECT_GRID allows projections to affect terrain features.
 *
 * PROJECT_ITEM allows projections to affect objects on the ground.
 *
 * PROJECT_KILL allows projections to affect monsters.
 *
 * PROJECT_PLAY allows projections to affect the player.
 *
 * degrees controls the width of arc spells.  With a value for
 *   degrees of zero, arcs act like beams of defined length.
 *
 * source_diameter controls how quickly explosions lose strength with dis-
 *   tance from the target.  Most ball spells have a source diameter of 10,
 *   which means that they do 1/2 damage at range 1, 1/3 damage at range 2,
 *   and so on.   Caster-centered balls usually have a source diameter of 20,
 *   which allows them to do full damage to all adjacent grids.   Arcs have
 *   source diameters ranging up to 20, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *   It is expected, but not required, that wide arcs lose strength more
 *   quickly over distance.
 *
 *
 * Implementation notes:
 *
 * If the source grid is not the same as the target, we project along the path
 *   between them.  Bolts stop if they hit anything, beams stop if they hit a
 *   wall, and balls and arcs may exhibit either behavior.  When they reach
 *   the final grid in the path, balls and arcs explode.  We do not allow beams
 *   to be combined with explosions.
 * Balls affect all floor grids in LOS (optionally, also wall grids adjacent
 *   to a grid in LOS) within their radius.  Arcs do the same, but only within
 *   their cone of projection.
 * Because affected grids are only scanned once, and it is really helpful to
 *   have explosions that travel outwards from the source, they are sorted by
 *   distance.  For each distance, an adjusted damage is calculated.
 * In successive passes, the code then displays explosion graphics, erases
 *   these graphics, marks terrain for possible later changes, affects
 *   objects, monsters, the character, and finally changes features and
 *   teleports monsters and characters in marked grids.
 *
 *
 * Usage and graphics notes:
 *
 * If the option "fresh_before" is on, or the delay factor is anything other
 * than zero, bolt and explosion pictures will be momentarily shown on screen.
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * radius of standard ball attacks to nine units (diameter nineteen).  Arcs
 * can have larger radii; an arc capable of going out to range 20 should not
 * be wider than 70 degrees.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters on
 * both sides of a wall.
 *
 * Note that for consistency, we pretend that the bolt actually takes time
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 *
 * Hack -- when only a single monster is affected, we automatically track
 * (and recall) that monster, unless "PROJECT_JUMP" is used.
 *
 * Note that we must call "handle_stuff()" after affecting terrain features
 * in the blast radius, in case the illumination of the grid was changed,
 * and "update_view()" and "update_monsters()" need to be called.
 */
bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
             u32b flg, int degrees, byte source_diameter)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, j, k, dist;

	u32b dam_temp;
	int centerline = 0;

	int y = y0;
	int x = x0;
	int n1y = 0;
	int n1x = 0;
	int y2, x2;

	int msec = op_ptr->delay_factor * op_ptr->delay_factor;

	/* Assume the player sees nothing */
	bool notice = FALSE;

	/* Assume the player has seen nothing */
	bool visual = FALSE;

	/* Assume the player has seen no blast grids */
	bool drawn = FALSE;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Number of grids in the "path" */
	int path_n = 0;

	/* Actual grids in the "path" */
	u16b path_g[512];

	/* Number of grids in the "blast area" (including the "beam" path) */
	int grids = 0;

	/* Coordinates of the affected grids */
	byte gx[256], gy[256];

	/* Distance to each of the affected grids. */
	byte gd[256];

	/* Precalculated damage values for each distance. */
	int dam_at_dist[MAX_RANGE+1];

	/*
	 * Starburst projections only --
	 * Holds first degree of arc, maximum effect distance in arc.
	 */
	byte arc_first[45];
	byte arc_dist[45];

	/* Number (max 45) of arcs. */
	int arc_num = 0;

	int degree, max_dist;



	/* Hack -- Flush any pending output */
	handle_stuff();

	/* Make certain that the radius is not too large */
	if (rad > MAX_SIGHT) rad = MAX_SIGHT;

	/* Handle effects to the caster of using a specific projection type. */
	if ((typ == GF_PROTECTION) && (who < 0))
	{
		if ((dam > 5) && (one_in_(2)))
		{
			set_shield(p_ptr->shield + randint(dam / 2), NULL);
		}
	}


	/* Some projection types always PROJECT_WALL. */
	if ((typ == GF_KILL_WALL) || (typ == GF_KILL_DOOR) ||
		 (typ == GF_FORCE_DOOR))
	{
		flg |= (PROJECT_WALL);
	}


	/* Hack -- Jump to target, but require a valid target */
	if ((flg & (PROJECT_JUMP)) && (y1) && (x1))
	{
		y0 = y1;
		x0 = x1;

		/* Clear the flag */
		flg &= ~(PROJECT_JUMP);
	}

	/* If a single grid is both source and destination, store it. */
	if ((x1 == x0) && (y1 == y0))
	{
		gy[grids] = y0;
		gx[grids] = x0;
		gd[grids++] = 0;
	}

	/* Otherwise, unless an arc or a star, travel along the projection path. */
	else if (!(flg & (PROJECT_ARC | PROJECT_STAR)))
	{
		/* Determine maximum length of projection path */
		if (flg & (PROJECT_BOOM)) dist = MAX_RANGE;
		else if (rad <= 0)        dist = MAX_RANGE;
		else                      dist = rad;

		/* Monster is directing a projection at the character */
		if ((who > 0) && (flg & (PROJECT_PLAY)) && (y1 == py) && (x1 == px))
		{
			/* Projection is a bolt or beam that doesn't explode */
			if ((flg & (PROJECT_BEAM)) || !(flg & (PROJECT_BOOM)))
			{
				/* Get the source monster */
				monster_type *m_ptr = &m_list[who];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Take reaction time into account */
				int diff = 300 / (m_ptr->cdis + 2);
				if (!m_ptr->ml) diff += 100;

				/* Hack -- characters can sometimes dodge */
				if (dodging_ability(60) > 5 + rand_int(diff + r_ptr->level))
				{
					/* Missile whizzes right past the character */
					y1 += (y1 - y0) * MAX_SIGHT;
					x1 += (x1 - x0) * MAX_SIGHT;
					dist = MAX_SIGHT;

					flg &= ~(PROJECT_PLAY | PROJECT_STOP);

					msg_print("You nimbly dodge aside.");
				}
			}
		}

		/* Calculate the projection path */
		path_n = project_path(path_g, dist, y0, x0, &y1, &x1, flg);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int oy = y;
			int ox = x;

			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);


			/* Hack -- Balls explode before reaching walls. */
			if ((flg & (PROJECT_BOOM)) && (!cave_floor_bold(ny, nx)))
			{
				break;
			}

			/* Advance */
			y = ny;
			x = nx;

			/* If a beam, collect all grids in the path. */
			if (flg & (PROJECT_BEAM))
			{
				gy[grids] = y;
				gx[grids] = x;
				gd[grids++] = 0;
			}

			/* Otherwise, collect only the final grid in the path. */
			else if (i == path_n - 1)
			{
				gy[grids] = y;
				gx[grids] = x;
				gd[grids++] = 0;
			}

			/* Only do visuals if requested */
			if (!blind && !(flg & (PROJECT_HIDE)))
			{
				/* Only do visuals if the player can "see" the projection */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					u16b p;

					byte a;
					char c;

					/* Obtain the bolt pict */
					p = bolt_pict(oy, ox, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Display the visual effects */
					print_rel(c, a, y, x);
					move_cursor_relative(y, x);
					if (op_ptr->delay_factor) Term_fresh();

					/* Delay */
					Term_xtra(TERM_XTRA_DELAY, msec);

					/* Erase the visual effects */
					lite_spot(y, x);
					if (op_ptr->delay_factor) Term_fresh();

					/* Re-display the beam  XXX */
					if (flg & (PROJECT_BEAM))
					{
						/* Obtain the explosion pict */
						p = bolt_pict(y, x, y, x, typ);

						/* Extract attr/char */
						a = PICT_A(p);
						c = PICT_C(p);

						/* Visual effects */
						print_rel(c, a, y, x);
					}

					/* Hack -- Activate delay */
					visual = TRUE;
				}

				/* Hack -- Always delay for consistency */
				else if (visual)
				{
					/* Delay for consistency */
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
		}
	}

	/* Save the "blast epicenter" */
	y2 = y;
	x2 = x;

	/* Beams have already stored all the grids they will affect. */
	if (flg & (PROJECT_BEAM))
	{
		/* No special actions */
	}

	/* Handle explosions */
	else if (flg & (PROJECT_BOOM))
	{
		/* Some projection types always PROJECT_WALL. */
		if (typ == GF_ACID)
		{
			/* Note that acid only affects monsters if it melts the wall. */
			flg |= (PROJECT_WALL);
		}

		/* Some projection types always PROJECT_PASS. */
		if (typ == GF_DISINTEGRATE)
		{
			flg |= (PROJECT_PASS);
		}


		/* Pre-calculate some things for starbursts. */
		if (flg & (PROJECT_STAR))
		{
			calc_starburst(1 + rad * 2, 1 + rad * 2, arc_first, arc_dist,
				&arc_num);

			/* Mark the area nearby -- limit range, ignore rooms */
			spread_cave_temp(y0, x0, rad, FALSE);
		}

		/* Pre-calculate some things for arcs. */
		if (flg & (PROJECT_ARC))
		{
			/* The radius of arcs cannot be more than 20 */
			if (rad > 20) rad = 20;

			/* Reorient the grid forming the end of the arc's centerline. */
			n1y = y1 - y0 + 20;
			n1x = x1 - x0 + 20;

			/* Correct overly large or small values */
			if (n1y > 40) n1y = 40;
			if (n1x > 40) n1x = 40;
			if (n1y <  0) n1y =  0;
			if (n1x <  0) n1x =  0;

			/* Get the angle of the arc's centerline */
			centerline = 90 - get_angle_to_grid[n1y][n1x];
		}

		/*
		 * If the center of the explosion hasn't been
		 * saved already, save it now.
		 */
		if (grids == 0)
		{
			gy[grids] = y2;
			gx[grids] = x2;
			gd[grids++] = 0;
		}

		/*
		 * Scan every grid that might possibly
		 * be in the blast radius.
		 */
		for (y = y2 - rad; y <= y2 + rad; y++)
		{
			for (x = x2 - rad; x <= x2 + rad; x++)
			{
				/* Center grid has already been stored. */
				if ((y == y2) && (x == x2)) continue;

				/* Precaution: Stay within area limit. */
				if (grids >= 255) break;

				/* Ignore "illegal" locations */
				if (!in_bounds(y, x)) continue;

				/* This is a wall grid (whether passable or not). */
				if (!cave_floor_bold(y, x))
				{
					/* Spell with PROJECT_PASS ignore walls */
					if (!(flg & (PROJECT_PASS)))
					{
						/* This grid is passable, or PROJECT_WALL is active */
						if ((flg & (PROJECT_WALL)) || (cave_passable_bold(y, x)))
						{
							/* Allow grids next to grids in LOS of explosion center */
							for (i = 0, k = 0; i < 8; i++)
							{
								int yy = y + ddy_ddd[i];
								int xx = x + ddx_ddd[i];

								/* Stay within dungeon */
								if (!in_bounds(yy, xx)) continue;

								if (los(y2, x2, yy, xx))
								{
									k++;
									break;
								}
							}

							/* Require at least one adjacent grid in LOS */
							if (!k) continue;
						}

						/* We can't affect this non-passable wall */
						else continue;
					}
				}

				/* Must be within maximum distance. */
				dist = (distance(y2, x2, y, x));
				if (dist > rad) continue;


				/* Projection is a starburst */
				if (flg & (PROJECT_STAR))
				{
					/* Grid is within effect range */
					if (cave_info[y][x] & (CAVE_TEMP))
					{
						/* Reorient current grid for table access. */
						int ny = y - y2 + 20;
						int nx = x - x2 + 20;

						/* Illegal table access is bad. */
						if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
							continue;

						/* Get angle to current grid. */
						degree = get_angle_to_grid[ny][nx];

						/* Scan arcs to find the one that applies here. */
						for (i = arc_num - 1; i >= 0; i--)
						{
							if (arc_first[i] <= degree)
							{
								max_dist = arc_dist[i];

								/* Must be within effect range. */
								if (max_dist >= dist)
								{
									gy[grids] = y;
									gx[grids] = x;
									gd[grids] = 0;
									grids++;
								}

								/* Arc found.  End search */
								break;
							}
						}
					}
				}

				/* Use angle comparison to delineate an arc. */
				else if (flg & (PROJECT_ARC))
				{
					int n2y, n2x, tmp, diff;

					/* Reorient current grid for table access. */
					n2y = y - y2 + 20;
					n2x = x - x2 + 20;

					/*
					 * Find the angular difference (/2) between
					 * the lines to the end of the arc's center-
					 * line and to the current grid.
					 */
					tmp = ABS(get_angle_to_grid[n2y][n2x] + centerline) % 180;
					diff = ABS(90 - tmp);

					/*
					 * If difference is not greater then that
					 * allowed, and the grid is in LOS, accept it.
					 */
					if (diff < (degrees + 6) / 4)
					{
						if (los(y2, x2, y, x))
						{
							gy[grids] = y;
							gx[grids] = x;
							gd[grids] = dist;
							grids++;
						}
					}
				}

				/* Standard ball spell -- accept all grids in LOS. */
				else
				{
					if (flg & (PROJECT_PASS) || los(y2, x2, y, x))
					{
						gy[grids] = y;
						gx[grids] = x;
						gd[grids] = dist;
						grids++;
					}
				}
			}
		}
	}

	/* Clear the "temp" array  XXX */
	clear_temp_array();

	/* Calculate and store the actual damage at each distance. */
	for (i = 0; i <= MAX_RANGE; i++)
	{
		/* No damage outside the radius. */
		if (i > rad) dam_temp = 0;

		/* Standard damage calc. for 10' source diameters, or at origin. */
		else if ((!source_diameter) || (i == 0))
		{
			dam_temp = (dam + i) / (i + 1);
		}

		/* If a particular diameter for the source of the explosion's
		 * energy is given, calculate an adjusted damage.
		 */
		else
		{
			dam_temp = (source_diameter * dam) / ((i + 1) * 10);
			if (dam_temp > (u32b)dam) dam_temp = dam;
		}

		/* Store it. */
		dam_at_dist[i] = dam_temp;
	}


	/* Sort the blast grids by distance, starting at the origin. */
	for (i = 0, k = 0; i < rad; i++)
	{
		int tmp_y, tmp_x, tmp_d;

		/* Collect all the grids of a given distance together. */
		for (j = k; j < grids; j++)
		{
			if (gd[j] == i)
			{
				tmp_y = gy[k];
				tmp_x = gx[k];
				tmp_d = gd[k];

				gy[k] = gy[j];
				gx[k] = gx[j];
				gd[k] = gd[j];

				gy[j] = tmp_y;
				gx[j] = tmp_x;
				gd[j] = tmp_d;

				/* Write to next slot */
				k++;
			}
		}
	}

	/* Display the blast area if allowed. */
	if (!blind && !(flg & (PROJECT_HIDE)))
	{
		/* Do the blast from inside out */
		for (i = 0; i < grids; i++)
		{
			/* Extract the location */
			y = gy[i];
			x = gx[i];

			/* Only do visuals if the player can "see" the blast */
			if (panel_contains(y, x) && player_has_los_bold(y, x))
			{
				u16b p;

				byte a;
				char c;

				drawn = TRUE;

				/* Obtain the explosion pict */
				p = bolt_pict(y, x, y, x, typ);

				/* Extract attr/char */
				a = PICT_A(p);
				c = PICT_C(p);

				/* Visual effects -- Display */
				print_rel(c, a, y, x);
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* New radius is about to be drawn */
			if ((i == grids - 1) || ((i < grids - 1) && (gd[i + 1] > gd[i])))
			{
				/* Flush each radius separately */
				if (op_ptr->delay_factor) Term_fresh();

				/* Delay (efficiently) */
				if (visual || drawn)
				{
					Term_xtra(TERM_XTRA_DELAY, msec);
				}
			}
		}

		/* Delay for a while if there are pretty graphics to show */
		if ((grids > 1) && (visual || drawn))
		{
			if (!op_ptr->delay_factor) Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, 50 + msec);
		}

		/* Flush the erasing -- except if we specify lingering graphics */
		if ((drawn) && (!(flg & (PROJECT_NO_REDRAW))))
		{
			/* Erase the explosion drawn above */
			for (i = 0; i < grids; i++)
			{
				/* Extract the location */
				y = gy[i];
				x = gx[i];

				/* Hack -- Erase if needed */
				if (panel_contains(y, x) && player_has_los_bold(y, x))
				{
					lite_spot(y, x);
				}
			}

			/* Hack -- center the cursor */
			move_cursor_relative(y2, x2);

			/* Flush the explosion */
			if (op_ptr->delay_factor) Term_fresh();
		}
	}

	/* Check features */
	if (flg & (PROJECT_GRID))
	{
		/* Scan for features */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the feature in that grid */
			if (project_f(who, y, x, gd[i], dam_at_dist[gd[i]], typ))
				notice = TRUE;
		}
	}

	/* Check objects */
	if (flg & (PROJECT_ITEM))
	{
		/* Scan for objects */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the object in the grid */
			if (project_o(who, y, x, dam_at_dist[gd[i]], typ))
				notice = TRUE;
		}
	}

	/* Check monsters */
	if (flg & (PROJECT_KILL))
	{
		/* Mega-Hack */
		project_m_n = 0;
		project_m_x = 0;
		project_m_y = 0;
		death_count = 0;

		/* Scan for monsters */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Affect the monster in the grid */
			if (project_m(who, y, x, dam_at_dist[gd[i]], typ, flg))
				notice = TRUE;
		}

		/* Player affected one monster (without "jumping") */
		if ((who < 0) && (project_m_n == 1) && !(flg & (PROJECT_JUMP)))
		{
			/* Location */
			x = project_m_x;
			y = project_m_y;

			/* Track if possible */
			if (cave_m_idx[y][x] > 0)
			{
				monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

				/* Hack -- auto-recall */
				if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

				/* Hack - auto-track */
				if (m_ptr->ml) health_track(cave_m_idx[y][x]);
			}
		}

		/* Hack -- Moria-style death messages for non-visible monsters */
		if (death_count)
		{
			/* One monster */
			if (death_count == 1)
			{
				msg_print("You hear a scream of agony!");
			}

			/* Several monsters */
			else
			{
				msg_print("You hear several screams of agony!");
			}

			/* Reset */
			death_count = 0;
		}
	}

	/* Check player */
	if (flg & (PROJECT_PLAY))
	{
		/* Scan for player */
		for (i = 0; i < grids; i++)
		{
			/* Get the grid location */
			y = gy[i];
			x = gx[i];

			/* Player is in this grid */
			if (cave_m_idx[y][x] < 0)
			{
				/* Affect the player */
				if (project_p(who, y, x, dam_at_dist[gd[i]], typ))
					notice = TRUE;
			}
		}
	}

	/* Teleport monsters and player around, alter certain features. */
	for (i = 0; i < grids; i++)
	{
		/* Get the grid location */
		y = gy[i];
		x = gx[i];

		/* Grid must be marked. */
		if (!(cave_info[y][x] & (CAVE_TEMP))) continue;

		/* Affect marked grid */
		if (project_t(who, y, x, dam_at_dist[gd[i]], typ, flg)) notice = TRUE;
	}


	/* Clear the "temp" array  (paranoia is good) */
	clear_temp_array();

	/* Clear required flags */
	p_ptr->proj_mon_flags = 0L;

	/* Clear racial immunity */
	project_immune = 0;

	/* Allow potion-smashing and scroll activation */
	allow_activate = TRUE;

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

	/* Return "something was noticed" */
	return (notice);
}
