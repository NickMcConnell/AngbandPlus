/* File: spells1.c */

/*
 * Get colors and symbols for spell projections.  Projection effects on
 * terrain features, monsters, objects, and the character.  The projection
 * code.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
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
		case 6:  return (TERM_PURPLE);
		case 7:  return (TERM_L_RED);
		case 8:  return (TERM_L_GREEN);
		case 9:  return (TERM_L_BLUE);
	}

	return (TERM_WHITE);
}

static byte acid_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_SLATE);
		case 2: return (TERM_L_DARK);
	}
	return (TERM_WHITE);
}

static byte elec_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_BLUE);
		case 2: return (TERM_L_BLUE);
	}
	return (TERM_WHITE);
}

static byte fire_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_RED);
		case 2: return (TERM_L_RED);
	}
	return (TERM_WHITE);
}

static byte cold_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_WHITE);
		case 2: return (TERM_L_WHITE);
	}
	return (TERM_WHITE);
}

static byte pois_color(void)
{
	switch (rand_int(3))
	{
		case 0: case 1: return (TERM_GREEN);
		case 2: return (TERM_L_GREEN);
	}
	return (TERM_WHITE);
}

static byte plasma_color(void)
{
	switch (rand_int(4))
	{
		case 0: return (TERM_WHITE);
		case 1: return (TERM_L_BLUE);
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
	switch (rand_int(2))
	{
		case 0: return (TERM_L_DARK);
		case 1: return (TERM_SLATE);
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
		case 3: return (TERM_PURPLE);
	}

	return (TERM_L_DARK);
}

static byte mana_color(void)
{
	switch (rand_int(4))
	{
		case 0: case 1: return (TERM_PURPLE);
		case 2: return (TERM_BLUE);
		case 3: return (TERM_RED);
	}

	return (TERM_PURPLE);
}


/*
 * Return a color to use for spells without user-specified colors.
 *
 * Applies to text-mode only.
 */
byte spell_color(int type)
{
	/* Analyze */
	switch (type)
	{
		case GF_ROCK:         return (TERM_SLATE);
		case GF_SHOT:         return (TERM_SLATE);
		case GF_ARROW:        return (TERM_L_UMBER);
		case GF_MISSILE:      return (TERM_L_WHITE);
		case GF_PMISSILE:     return (TERM_GREEN);
		case GF_WHIP:         return (TERM_UMBER);
		case GF_HURT:         return (TERM_WHITE);

		case GF_ACID:         return (acid_color());
		case GF_ELEC:         return (elec_color());
		case GF_FIRE:         return (fire_color());
		case GF_COLD:         return (cold_color());
		case GF_POIS:         return (pois_color());

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
		case GF_WATER:
		{
			if (max_system_colors > 16) return (TERM_TEAL);
			else                        return (TERM_SLATE);
		}
		case GF_WIND:         return (TERM_WHITE);
		case GF_STORM:        return (storm_color());

		case GF_NEXUS:        return (TERM_L_RED);
		case GF_NETHER:       return (TERM_L_GREEN);
		case GF_CHAOS:        return (mh_attr());
		case GF_DISENCHANT:   return (TERM_PURPLE);
		case GF_TIME:         return (TERM_L_BLUE);
		case GF_MANA:         return (mana_color());

		case GF_METEOR:       return (meteor_color());
		case GF_HOLY_ORB:     return (orb_color());
		case GF_BLACK_ORB:    return (TERM_L_DARK);
		case GF_DEATH:        return (death_color());
		case GF_VAMP_DRAIN:   return (TERM_PURPLE);
		case GF_DEATH_CLOUD:  return (death_color());
		case GF_SPORE:        return (TERM_MUD);
		case GF_FORGET:       return (TERM_VIOLET);

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
 * Spell character is always specified in pref files.  Spell color
 * may or may not be.
 */
u16b bolt_pict(int y, int x, int ny, int nx, int typ)
{
	byte a;
	char c;

	/* No motion (*) */
	if ((ny == y) && (nx == x))
	{
		a = proj_graphics[typ].attr_ball ?
		    proj_graphics[typ].attr_ball : spell_color(typ);
		c = proj_graphics[typ].char_ball;
	}

	/* Vertical (|) */
	else if (nx == x)
	{
		a = proj_graphics[typ].attr_vert ?
		    proj_graphics[typ].attr_vert : spell_color(typ);
		c = proj_graphics[typ].char_vert;
	}

	/* Horizontal (-) */
	else if (ny == y)
	{
		a = proj_graphics[typ].attr_horiz ?
		    proj_graphics[typ].attr_horiz : spell_color(typ);
		c = proj_graphics[typ].char_horiz;
	}

	/* Diagonal (/) */
	else if ((ny - y) == (x - nx))
	{
		a = proj_graphics[typ].attr_rdiag ?
		    proj_graphics[typ].attr_rdiag : spell_color(typ);
		c = proj_graphics[typ].char_rdiag;
	}

	/* Diagonal (\) */
	else if ((ny - y) == (nx - x))
	{
		a = proj_graphics[typ].attr_ldiag ?
		    proj_graphics[typ].attr_ldiag : spell_color(typ);
		c = proj_graphics[typ].char_ldiag;
	}

	/* Weird (*) */
	else
	{
		a = proj_graphics[typ].attr_ball ?
		    proj_graphics[typ].attr_ball : spell_color(typ);
		c = proj_graphics[typ].char_ball;
	}

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
			/* Mark rocky grids for (possible) later alteration. */
			if ((cave_rock_bold(y, x)) && (dist <= 4))
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

			/* Grid is in line of sight */
			if (player_has_los_bold(y, x))
			{
				/* Observe */
				obvious = TRUE;

				/* Fully update the visuals */
				p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);
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
				if ((cave_floor_bold(y, x)) ||
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
			if ((cave_passable_bold(y, x)) && (p_ptr->depth > 40) &&
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
				if ((cave_passable_bold(y, x)) && (p_ptr->depth > 30) &&
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
			if (magic_disarm(y, x, 95)) obvious = TRUE;

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
			/* Destroy all doors.  Traps are not affected */
			if (cave_any_door(y, x))
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

				/* Copy any nearby floor-type terrain. */
				cave_set_feat(y, x, get_nearby_floor(y, x));
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
					cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + randint(4));
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
				/* Note changes */
				if (player_has_los_bold(y, x))
				{
					if ((cave_m_idx[y][x]) || (cave_o_idx[y][x]))
					{
						/* Failure */
						if (dist <= 2) msg_print("Something is in the way!");
						break;
					}

					/* Message */
					msg_print("The door slams shut!");
					obvious = TRUE;

					/* Update the visuals */
					p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
				}

				/* Close and jam the door */
				cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + randint(4));
			}

			break;
		}

		/* Magical door jamming */
		case GF_JAM_DOOR:
		{
			/* Require known closed door (not secret XXX) */
			if (cave_known_closed_door(y, x))
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
			/* Kill-wall only affects rock and doors */
			if ((typ == GF_KILL_WALL) && (!cave_rock_bold(y, x)) &&
			    (!cave_any_door(y, x))) break;

			/* Permanent walls and stores are immune */
			if (cave_perma_bold(y, x)) break;

			/* Floors are unaffected */
			if (cave_floor_bold(y, x)) break;


			/* Quartz / Magma */
			if ((cave_feat[y][x] == FEAT_MAGMA) ||
			    (cave_feat[y][x] == FEAT_QUARTZ))
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
				cave_set_feat(y, x, get_nearby_floor(y, x));
			}

			/* Quartz / Magma with treasure */
			else if ((cave_feat[y][x] == FEAT_MAGMA_H) ||
			         (cave_feat[y][x] == FEAT_QUARTZ_H) ||
			         (cave_feat[y][x] == FEAT_MAGMA_K) ||
			         (cave_feat[y][x] == FEAT_QUARTZ_K))
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
				cave_set_feat(y, x, get_nearby_floor(y, x));

				/* Place some gold */
				if (typ != GF_DISINTEGRATE) place_gold(y, x);
			}

			/* Destroy doors (and secret doors) */
			else if (cave_any_door(y, x))
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
				cave_set_feat(y, x, get_nearby_floor(y, x));
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
				cave_set_feat(y, x, get_nearby_floor(y, x));

				/* There is a hidden object here */
				if (cave_loose_rock(y, x))
				{
					/* Remove the loose rock */
					remove_trap_kind(y, x, TRAP_LOOSE_ROCK);

					/* The spell is not disintegration */
					if (typ != GF_DISINTEGRATE)
					{
						/* Found something */
						if (player_can_see_or_infra_bold(y, x))
						{
							msg_print("There was something buried in the rubble!");
							obvious = TRUE;
						}

						/* Place object */
						place_object(y, x, FALSE, FALSE, FALSE);
					}
				}
			}

			/* Pillar */
			else if ((cave_feat[y][x] == FEAT_PILLAR) ||
			         (cave_feat[y][x] == FEAT_PILLAR_GOLD))
			{
				bool gold = (cave_feat[y][x] == FEAT_PILLAR_GOLD);

				/* Disintegration spells ruin gold too */
				if (typ == GF_DISINTEGRATE) gold = FALSE;

				/* Message */
				if ((cave_info[y][x] & (CAVE_MARK)) &&
			       (typ != GF_DISINTEGRATE))
				{
					msg_print("The pillar turns into mud.");
					if (gold) msg_print("Gold glitters on the ground!");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, get_nearby_floor(y, x));

				/* Place lots of gold  */
				if (gold)
				{
					int i;

					coin_type = SV_GOLD;
					for (i = 0; i < 1 + p_ptr->depth / 14; i++) place_gold(y, x);
					coin_type = 0;
				}
			}

			/* Generic "wall" */
			else
			{
				/* Message */
				if (cave_info[y][x] & (CAVE_MARK))
				{
					if ((typ != GF_DISINTEGRATE) && (cave_wall_bold(y, x)))
						msg_print("The wall turns into mud.");
					obvious = TRUE;
				}

				/* Forget the wall */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the wall */
				cave_set_feat(y, x, get_nearby_floor(y, x));
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
			/* Require a passable grid */
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
			/* Assume obvious  XXX */
			if (player_has_los_bold(y, x)) obvious = TRUE;

			/* Limit total glyphs on level */
			if (num_glyph_on_level >= MAX_GLYPHS) break;

			/* Require passable grid */
			if (!cave_passable_bold(y, x)) break;

			/* Cannot place glyphs on glyphs */
			if (cave_glyph(y, x)) break;

			/* Limit damage */
			if (dam > 250) dam = 250;

			/* Must pass a rarity check */
			if (dam > rand_range(50, 500))
			{
				/* Create a glyph */
				place_trap(y, x, TRAP_GLYPH, 0);

				/* Cool */
				if (player_can_see_or_infra_bold(y, x))
					message(TERM_L_BLUE, 50, "A glyph of sanctuary materializes!");
			}

			break;
		}

		/* Make water */
		case GF_MAKE_WATER:
		{
			/* Do not alter stairs in town  XXX */
			if ((cave_feat[y][x] == FEAT_MORE) && (p_ptr->depth == 0))
				break;

			/* Ignore permanent terrain */
			if (cave_perma_bold(y, x)) break;

			/* Change to water */
			cave_set_feat(y, x, FEAT_WATER);
			obvious = TRUE;

			break;
		}

		/* Make trees */
		case GF_MAKE_TREES:
		{
			/* Do not alter stairs in town  XXX */
			if ((cave_feat[y][x] == FEAT_MORE) && (p_ptr->depth == 0))
				break;

			/* Ignore permanent terrain */
			if (cave_perma_bold(y, x)) break;

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
			/* Do not alter stairs in town  XXX */
			if ((cave_feat[y][x] == FEAT_MORE) && (p_ptr->depth == 0))
				break;

			/* Ignore permanent terrain */
			if (cave_perma_bold(y, x)) break;

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
 * We are called from "project()" to affect objects
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_o(int who, int y, int x, int dam, int typ)
{
	s16b this_o_idx, next_o_idx = 0;
	int i;

	int o_idx_list[MAX_FLOOR_STACK];
	int o_idx_count = 0;

	object_type *o_ptr;
	object_type *i_ptr;
	object_type forge;

	bool obvious = FALSE;

	char o_name[DESC_LEN];

	u32b f1, f2, f3;

	/* "Who" is currently unused */
	(void)who;


	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Store this object index, increase list size */
		o_idx_list[o_idx_count++] = this_o_idx;

		/* We've run out of room */
		if (o_idx_count >= MAX_FLOOR_STACK) break;

		/* Get the next object */
		next_o_idx = o_list[this_o_idx].next_o_idx;
	}

	/* No objects */
	if (!o_idx_count) return (FALSE);


	/* Scan all saved objects */
	for (i = 0; i < o_idx_count; i++)
	{
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
		o_ptr = &o_list[o_idx_list[i]];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Note multiple objects */
		if (o_ptr->number > 1) plural = TRUE;

		/* Hack -- note "collective plural" treasure objects */
		else if (o_ptr->tval == TV_GOLD)
		{
			object_kind *k_ptr = &k_info[o_ptr->k_idx];
			cptr s;
			cptr base_name = (k_name + k_ptr->name);

			/* Scan the object */
			for (s = base_name; *s; s++)
			{
				/* Note approaching end */
				if (!*(s+1))
				{
					/* Note plurality (simple test) */
					if (*s == 's') plural = TRUE;
				}
			}
		}

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

				/* Force also moves objects */
				if ((typ != GF_FORCE) || (do_kill)) break;
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
				/* Object is not resistant to chaos */
				if (!(f2 & (TR2_RES_CHAOS)))
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

			/* Grab -- only the top object in the grid */
			case GF_FETCH_OBJ:
			{
				/* Hack -- Ignore character's own grid */
				if ((y == p_ptr->py) && (x == p_ptr->px)) break;

				/* Ignore all objects except the first */
				if (i != 0) break;

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

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
					delete_object_idx(o_idx_list[i]);
				}

				/* Ignore essences */
				else if (o_ptr->tval == TV_ESSENCE)
				{
					/* Do nothing */
				}

				/* Normal objects that are not too heavy */
				else if (o_ptr->weight * o_ptr->number <= dam)
				{
					/* Hack -- try to put ammo directly in the quiver */
					if (quiver_carry(o_ptr, o_idx_list[i])) break;

					/* Get local object */
					i_ptr = &forge;

					/* Wipe the new object */
					object_wipe(i_ptr);

					/* Make a copy */
					object_copy(i_ptr, o_ptr);

					/* Give the new object to the character */
					give_object(i_ptr, FALSE);

					/* Delete the original object */
					delete_object_idx(o_idx_list[i]);
				}

				/* Over-heavy objects can't be fetched */
				else
				{
					msg_format("The %s %s too heavy to move.", o_name,
						(o_ptr->number > 1 ? "are" : "is"));
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
					if (dam >= rand_range(20, 20 + dam))
					{
						object_known(o_ptr);

						if (dam >= rand_range(60, 60 + dam))
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
					object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
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
					object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
					message_format(MSG_DESTROY, 0, "The %s%s", o_name, note_kill);
				}

				/* Potions produce effects when 'shattered' */
				if (o_ptr->tval == TV_POTION)
				{
					/* Do not learn anything  XXX XXX */
					(void)potion_smash_effect(0, y, x, o_ptr);
				}

				/* Delete the object */
				delete_object_idx(o_idx_list[i]);

				/* Redraw */
				lite_spot(y, x);
			}

			continue;
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
					object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
					msg_format("The %s %s unaffected!",
						   o_name, (plural ? "are" : "is"));
				}
			}

			/* Change it (hack -- unless gold or essences) */
			else if ((o_ptr->tval != TV_GOLD) && (o_ptr->tval != TV_ESSENCE))
			{
				/* Get local object */
				i_ptr = &forge;

				/* Wipe the new object */
				object_wipe(i_ptr);

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
						object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
						msg_format("The %s%s", o_name, note_change);
					}

					/* Delete the original object */
					delete_object_idx(o_idx_list[i]);

					/* Drop the new object nearby */
					drop_near(i_ptr, 0, y, x, DROP_HERE);

					/* Redraw */
					lite_spot(y, x);
				}
			}

			continue;
		}

		/* Move the object (hack -- unless essence) */
		else if ((do_move) && (o_ptr->tval != TV_ESSENCE))
		{
			/* Get local object */
			i_ptr = &forge;

			/* Wipe the new object */
			object_wipe(i_ptr);

			/* Make a copy */
			object_copy(i_ptr, o_ptr);

			/* Effect "observed" */
			if (o_ptr->marked)
			{
				obvious = TRUE;
			}

			/* Delete the object in its old location */
			delete_object_idx(o_idx_list[i]);

			/* Drop it near the new location */
			drop_near(i_ptr, 0, ny, nx, DROP_HERE);

			/* Redraw */
			lite_spot(y, x);

			continue;
		}

		/* Go to next object */
	}

	/* Return "Anything seen?" */
	return (obvious);
}


/*
 * Types of hindrance attacks recognized by the "can_hinder" function.
 */
#define DO_SLOW         1
#define DO_CONF         2
#define DO_STUN         3
#define DO_FEAR         4
#define DO_TURN         5
#define DO_SLEEP        6
#define DO_POLY         7


/*
 * Modes of operation for the "can_hinder" function.
 */
#define LEARN          1   /* Allow learning monster lore */
#define NOTES          2   /* Display detailed messages */


/*
 * Given a type of hindrance and an attack power, determine whether the
 * given monster will be affected and what (if any) resistance messages
 * will display.  -LM-
 *
 * A power of zero means the monster will always be affected unless
 * entirely immune.
 *
 * Stunning, fear, and confusion decrease by
 * "randint(3 + div_round(r_ptr->level, 20))" every 10 game turns.
 * Monsters never get a chance to instantly recover except by casting
 * cure-self spells.
 *
 * In order for the character to learn something, he has to observe a
 * non-ambiguous spell.
 *
 * We stay pretty quiet about resistances unless asked to be verbose.
 * We are normally only verbose when pure magic (as opposed to ranged
 * damage with side-effects) is being used.
 *
 * We are often specific about the type of hindrance used, even when it
 * seem unnecessary, because many magics have random effects.  Players
 * need to know what special effects their spells may have.
 */
static bool can_hinder(int type, monster_type *m_ptr, int power,
	char *note, size_t note_len, byte mode)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	cptr name = (r_name + r_ptr->name);

	int resist;
	int lev = 5 + (2 * r_ptr->level / 3);

	byte learn = mode & (LEARN);
	byte notes = mode & (NOTES);


	/* Slowing */
	if (type == DO_SLOW)
	{
		/* Monster is already slowed */
		if (m_ptr->slowed) return (FALSE);

		/* Some monsters are immune */
		if ((m_ptr->r_idx == MON_MAGIC_MUSHROOM) ||
		    (r_ptr->flags4 & (RF4_BRTH_INER | RF4_BRTH_TIME)) ||
			 (strstr(name, "Time")))
		{
			if (notes) (void)strnfmt(note, note_len, " cannot be slowed!");
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			if (r_ptr->flags1 & (RF1_UNIQUE)) resist = lev + 10;
			else                              resist = lev;

			/* No chance of success */
			if (resist > power)
			{
				if (notes) (void)strnfmt(note, note_len, " is unaffected by the slowing spell!");
				return (FALSE);
			}

			/* Monster resisted */
			else if (resist > randint(power))
			{
				if (notes) (void)strnfmt(note, note_len, " resists the slowing spell.");
				return (FALSE);
			}
		}
	}

	/* Confusion */
	else if (type == DO_CONF)
	{
		/* Some monsters are immune */
		if ((r_ptr->flags3 & (RF3_NO_CONF)) ||
		    (r_ptr->flags4 & (RF4_BRTH_CONFU)) ||
		    (r_ptr->flags4 & (RF4_BRTH_CHAOS)))
		{
			if (notes) (void)strnfmt(note, note_len, " cannot be confused!");
			if (learn) l_ptr->flags3 |= (RF3_NO_CONF);
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			if (r_ptr->flags1 & (RF1_UNIQUE))  resist = lev + 10;
			else                               resist = lev;

			if (r_ptr->flags3 & (RF3_UNDEAD))  resist += 10;
			else if (monster_nonliving(r_ptr)) resist += 5;

			/* No chance of success */
			if (resist > power)
			{
				if (notes) (void)strnfmt(note, note_len, " refuses to be confused!");
				return (FALSE);
			}

			/* Monster resisted */
			else if (resist > randint(power))
			{
				if (notes) (void)strnfmt(note, note_len, " shakes off the confusion.");
				return (FALSE);
			}
		}
	}

	/* Stunning */
	else if (type == DO_STUN)
	{
		/* Some monsters are immune */
		if ((r_ptr->flags3 & (RF3_NO_STUN)) ||
		    (r_ptr->flags4 & (RF4_BRTH_SOUND | RF4_BRTH_FORCE)))
		{
			if (notes) (void)strnfmt(note, note_len, " is immune to stunning!");
			if (learn) l_ptr->flags3 |= (RF3_NO_STUN);
			return (FALSE);
		}

		/* Non-living monsters are harder to stun */
		if ((monster_nonliving(r_ptr)) && (one_in_(2)))
		{
			if (!m_ptr->stunned)
			{
				if (notes) (void)strnfmt(note, note_len, " is dazed, but quickly recovers.");
			}
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			if (r_ptr->flags1 & (RF1_UNIQUE))  resist = lev + 10;
			else                               resist = lev;

			if (r_ptr->flags3 & (RF3_UNDEAD))  resist += 5;

			/* No chance of success */
			if (resist > power)
			{
				if (notes) (void)strnfmt(note, note_len, " refuses to be stunned!");
				return (FALSE);
			}

			/* Monster resisted */
			else if (resist > randint(power))
			{
				if (notes) (void)strnfmt(note, note_len, " is dazed, but quickly recovers.");
				return (FALSE);
			}
		}
	}

	/* Frightening and turning */
	else if ((type == DO_FEAR) || (type == DO_TURN))
	{
		/* Frightened monsters have no resistance to fear */
		if (m_ptr->monfear) return (TRUE);

		/* Some monsters are immune to fear (not turning) */
		if ((r_ptr->flags3 & (RF3_NO_FEAR)) && (type != DO_TURN))
		{
			if (notes) (void)strnfmt(note, note_len, " knows no fear!");
			if (learn) l_ptr->flags3 |= (RF3_NO_FEAR);
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			if (r_ptr->flags1 & (RF1_UNIQUE))  resist = lev + 10;
			else                               resist = lev;

			/* No chance of success */
			if (resist > power)
			{
				if (notes)
				{
					if (type == DO_TURN)
						(void)strnfmt(note, note_len, " refuses to be turned!");
					else
						(void)strnfmt(note, note_len, " refuses to be frightened!");
				}
				return (FALSE);
			}

			/* Monster resisted */
			else if (resist > randint(power))
			{
				if (notes)
				{
					if (type == DO_TURN)
						(void)strnfmt(note, note_len, " turns to flee, but recovers.");
					else
						(void)strnfmt(note, note_len, " looks frightened, but recovers.");
				}
				return (FALSE);
			}
		}
	}

	/* Sleeping */
	else if (type == DO_SLEEP)
	{
		/* No notes for already sleeping monsters */
		if (m_ptr->csleep) notes = learn = FALSE;

		/* Some monsters are immune */
		if (r_ptr->flags3 & (RF3_NO_SLEEP))
		{
			if (notes) (void)strnfmt(note, note_len, " cannot be lulled!");
			if (learn) l_ptr->flags3 |= (RF3_NO_SLEEP);
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			if (r_ptr->flags1 & (RF1_UNIQUE))  resist = lev + 10;
			else                               resist = lev;

			if (monster_nonliving(r_ptr)) resist += 5;

			/* No chance of success */
			if (resist > power)
			{
				if (notes) (void)strnfmt(note, note_len, " is too vigilant to lull!");
				return (FALSE);
			}

			/* Monster resisted */
			else if (resist > randint(power))
			{
				if (notes) (void)strnfmt(note, note_len, " looks sleepy, but recovers.");
				return (FALSE);
			}
		}
	}

	/* Polymorphing */
	else if (type == DO_POLY)
	{
		/* Some monsters are immune */
		if ((r_ptr->flags3 & (RF3_RES_CHAOS)) ||
		    (r_ptr->flags4 & (RF4_BRTH_CHAOS)))
		{
			if (notes) (void)strnfmt(note, note_len, " cannot be polymorphed!");
			if (learn) l_ptr->flags3 |= (RF3_RES_CHAOS);
			return (FALSE);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			if (notes) (void)strnfmt(note, note_len, " cannot be polymorphed!");
			return (FALSE);
		}

		/* Roll for resistance */
		if (power > 0)
		{
			/* Determine resistance level */
			resist = lev + 5;

			/* Monster resisted */
			if (resist > randint(power))
			{
				if (notes) (void)strnfmt(note, note_len, " resists alteration!");
				return (FALSE);
			}
		}
	}

	/* Unknown hindrance type */
	else
	{
		return (FALSE);
	}

	/* Hindrance practices a skill */
	if (skill_being_used != S_NOSKILL)
	{
		/* Hindrance trains around 5% of base skill */
		long new_exp = div_round(monster_exp(r_ptr), 20);

		/* Practice */
		practice_skill(new_exp, skill_being_used);
	}

	/* Monster failed to resist */
	return (TRUE);
}


/*
 * Helper function for "project()" below.
 *
 * Handle a projection causing damage to a monster.
 *
 * Certain terrain types affect spells.  -LM-
 *
 *
 * Resistances:
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune; you
 * must actually be "made" of that substance, or "breathe" it.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt
 * undead, and hurts evil less.   If can breath nether, then it resists
 * it as well.
 *
 * XXX XXX - For monsters, Morgul-dark is the same as darkness.
 *
 * Non-visible and limited-visible monsters take less damage if at
 * ground zero.  Note that characters in darkness get similar treatment.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string (if any) is appended to the monster name.
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

	/* Adjustment to damage caused by terrain, if applicable. */
	int terrain_adjustment = 0;

	/* Is the monster "seen"? */
	bool seen = FALSE;
	bool fully_seen = FALSE;

	/* Were the effects "obvious" (if seen)? */
	bool obvious = FALSE;

	/* Were the effects "irrelevant"? */
	bool skipped = FALSE;


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

	/* Polymorph setting (true or false) */
	bool do_poly = FALSE;


	char m_name[DESC_LEN];
	char note[DESC_LEN];

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
		if ((mon_fully_visible(m_ptr)) && (!p_ptr->image)) fully_seen = TRUE;
	}

	/* Non-visible and partially-visible monsters are somewhat protected */
	if      (!seen)       dam /= 2;
	else if (!fully_seen) dam = 2 * dam / 3;


	/* Some monsters are great at dodging  -EZ- */
	if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->csleep) &&
	    (!m_ptr->confused) && ((!m_ptr->stunned) || (one_in_(2))))
	{
		/* Area-effect and jumping spells cannot be dodged */
		if (!(flg & (PROJECT_ARC | PROJECT_STAR | PROJECT_JUMP |
		             PROJECT_BOOM)))
		{
			/* Allow dodging */
			if (rand_int(4 + m_ptr->cdis) >= (2 + m_ptr->stunned / 10))
			{
				if (fully_seen)
				{
					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					msg_format("%^s dodges!", m_name);

					/* Learn that monster can dodge */
					l_ptr->flags2 |= (RF2_EVASIVE);
				}

				/* No effect, no observation */
				return (FALSE);
			}
		}
	}


	/* Get the monster name, or "something" */
	monster_desc(m_name, m_ptr, 0x04);

	/* Handle hallucination */
	if (p_ptr->image) strcpy(m_name, "something");

	/* Describe the death */
	note_dies = death_string(r_ptr);

	/* Initialize note */
	strcpy(note, "");


	/* Determine if terrain is capable of adjusting physical damage. */
	switch (cave_feat[y][x])
	{
		/* Monsters can duck behind rubble, or take only partial damage. */
		case FEAT_RUBBLE:
		{
			if ((!m_ptr->csleep) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
			    (one_in_(4)))
			{
				if (fully_seen) msg_format("%^s ducks behind a boulder!", m_name);
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
			else if ((typ == GF_WATER) || (typ == GF_STORM))
			{
				terrain_adjustment = dam / 3;
			}
			break;
		}

		/* Cold and water-based spells suffer, fire-based spells benefit. */
		case FEAT_LAVA:
		{
			if ((typ == GF_COLD) || (typ == GF_ICE) ||
			    (typ == GF_WATER) || (typ == GF_STORM))
			{
				terrain_adjustment -= dam / 2;
			}
			else if ((typ == GF_FIRE) || (typ == GF_HELLFIRE) ||
			         (typ == GF_PLASMA))
			{
				terrain_adjustment = dam / 4;
			}
			break;
		}

		/* Monsters can duck, or take only partial damage. */
		case FEAT_TREE:
		{
			/* Nature skill reduces protection */
			int skill = get_skill(S_NATURE, 0, 100);

			if ((!m_ptr->csleep) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
			    (skill < 75) && (one_in_(4 + skill / 5)))
			{
				if (fully_seen) msg_format("%^s hides behind a tree!", m_name);
				return (FALSE);
			}
			else if (skill < 60)
			{
				terrain_adjustment -= dam / (5 + skill / 10);
			}
			break;
		}
	}

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
			else if ((dam >= 16) && (one_in_(2)))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 8));
				}
			}
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
				strcpy(note, " resists a lot.");
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
				strcpy(note, " resists a lot.");
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
			}
			/* Can stun, if enough damage is done. */
			else if ((dam >= 16) && (one_in_(2)))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 8));
				}
			}
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
				strcpy(note, " resists a lot.");
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
			}
			else if (r_ptr->flags3 & (RF3_HURT_FIRE))
			{
				strcpy(note, " is hit hard.");
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
				strcpy(note, " resists a lot.");
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_COLD);
			}
			else if (r_ptr->flags3 & (RF3_HURT_COLD))
			{
				strcpy(note, " is hit hard.");
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
				strcpy(note, " resists a lot.");
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
				strcpy(note, " resists a lot.");
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

				/* Can stun */
				if ((can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0)) && (one_in_(2)))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 8));
				}
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
				strcpy(note, " resists a lot.");
				dam = div_round(dam, 9);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_PLAS);
			}

			else if ((r_ptr->flags3 & (RF3_IM_ELEC)) ||
			         (r_ptr->flags3 & (RF3_IM_FIRE)))
			{
				if ((r_ptr->flags3 & (RF3_IM_ELEC)) &&
				    (r_ptr->flags3 & (RF3_IM_FIRE)))
				{
					strcpy(note, " resists.");
					dam = div_round(dam, 4);

					if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
					if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
				}
				else
				{
					strcpy(note, " resists somewhat.");
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
				strcpy(note, " basks in the flames of Hell!");

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
				strcpy(note, " resists the dark flames.");
				dam /= 2;
			}

			/* Can resist fire */
			else if (r_ptr->flags3 & (RF3_IM_FIRE))
			{
				strcpy(note, " resists the evil fire.");
				dam /= 2;
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_FIRE);
			}

			/* Evil creatures resist a little */
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				strcpy(note, " resists somewhat.");
				dam = 2 * dam / 3;
			}

			/* If the creature did not resist, it can have nasty done to it */
			else if (randint(dam) > (2 * r_ptr->level / 3 + 30))
			{
				int choice = randint(4);

				/* Effect 1 -- panic */
				if (choice <= 3)
				{
					if (can_hinder(DO_FEAR, m_ptr, 0, note, sizeof(note), 0)) do_fear = 40;
				}

				/* Effect 2 -- confusion */
				if (choice == 4)
				{
					/* Get confused later */
					if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note), 0)) do_conf = 20;
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
				strcpy(note, " cringes from the light!");
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

			/* Light-breathers and Angels resist */
			if ((r_ptr->flags4 & (RF4_BRTH_LITE)) || (r_ptr->d_char == 'A'))
			{
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
			}

			/* Creatures can be susceptible */
			else if (r_ptr->flags3 & (RF3_HURT_LITE))
			{
				if (fully_seen) l_ptr->flags3 |= (RF3_HURT_LITE);
				strcpy(note, " cringes from the light!");
				note_dies = " shrivels away in the light!";
				dam = 3 * dam / 2;

				/* Allow extra effects */
				if (typ == GF_LITE_EXTRA)
				{
					/* Does nasty on spiders */
					if (strchr("S", r_ptr->d_char))
					{
						/* Especially Shelob and Ungoliant */
						if (r_ptr->flags1 & (RF1_UNIQUE))
						{
							dam *= 3;
						}
						else
						{
							dam = 3 * dam / 2;
						}

						/* Attempt to frighten the monster.  Ignore level. */
						if (one_in_(2))
						{
							if (can_hinder(DO_FEAR, m_ptr, 0, note, sizeof(note), 0))
								do_fear = 50;
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
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
			}

			/* Creatures that use Morgul-magic are resistant to darkness. */
			else if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
			{
				strcpy(note, " resists somewhat.");
				dam = div_round(dam, 3);
			}

			/* Orcs partially resist darkness. */
			else if (r_ptr->flags3 & (RF3_ORC))
			{
				strcpy(note, " resists somewhat.");
				dam = div_round(dam, 2);
			}
			break;
		}

		/* Confusion -- usually confuses */
		case GF_CONFUSION:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;
			if (r_ptr->flags4 & (RF4_BRTH_CONFU))
			{
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
			}
			else if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				strcpy(note, " resists somewhat.");
				dam = div_round(dam, 2);
			}
			else if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note),
				(who < 0 ? LEARN | NOTES : 0)))
			{
				/* Confuse from 10 to 30 */
				do_conf = 10 + rand_int(MIN(21, dam / 8));
			}
			break;
		}

		/* Sound -- Sound breathers resist, others may be stunned. */
		case GF_SOUND:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (seen) obvious = TRUE;

			if (r_ptr->flags4 & (RF4_BRTH_SOUND))
			{
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
			}
			else if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note),
				(who < 0 ? LEARN | NOTES : 0)))
			{
				/* Stun from 10 to 30 */
				do_stun = 10 + rand_int(MIN(21, dam / 8));
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
				strcpy(note, " resists.");
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
				strcpy(note, " resists.");
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
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
				break;
			}

			/* Some monsters can resist being moved about */
			if (r_ptr->flags3 & (RF3_RES_TPORT))
			{
				if (one_in_(3))
				{
					if (fully_seen) l_ptr->flags3 |= (RF3_RES_TPORT);
					strcpy(note, " stays in place!");
					break;
				}
			}

			/* Monster was affected -- Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Force.  Can (sometimes) stun. */
		case GF_FORCE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (seen) obvious = TRUE;

			if (r_ptr->flags4 & (RF4_BRTH_FORCE))
			{
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
				break;
			}

			/* Can stun */
			if (one_in_(2))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 10));
				}
			}

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
				strcpy(note, " is immune.");
				dam = 0;
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_WATER);
			}

			/* Can stun */
			if (one_in_(2))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 10));
				}
			}

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
				strcpy(note, " is unaffected!");
				dam = 0;
				break;
			}

			/* Wind doesn't affect ghosts very much */
			if (strchr("G", r_ptr->d_char))
			{
				strcpy(note, " resists.");
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
				strcpy(note, " is immune.");
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_WATER);
				dam = 0;
				break;
			}

			/* Electricity resistance. */
			if (r_ptr->flags3 & (RF3_IM_ELEC))
			{
				strcpy(note, " resists.");
				if (fully_seen) l_ptr->flags3 |= (RF3_IM_ELEC);
				dam /= 2;
			}
			else if ((dam) && one_in_(6))
			{
				/* Lightning strike. */
				strcpy(note, " is struck by lightning!");

				dam += dam / 2;
			}

			/* Can stun, if enough damage is done. */
			if ((dam > 20) && (one_in_(2)))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Stun from 10 to 30 */
					do_stun = 10 + rand_int(MIN(21, dam / 10));
				}
			}

			/* Can confuse, if enough damage is done. */
			if ((dam > 30) && (one_in_(2)))
			{
				if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Confuse from 10 to 30 */
					do_conf = 10 + rand_int(MIN(21, dam / 10));
				}
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
				strcpy(note, " resists.");
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
				strcpy(note, " is immune.");
				dam = 0;
				if (fully_seen) l_ptr->flags3 |= (RF3_UNDEAD);
			}
			else if ((r_ptr->flags3 & (RF3_RES_NETHR)) ||
			         (r_ptr->flags4 & (RF4_BRTH_NETHR)))
			{
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_NETHR);
			}
			else if (r_ptr->flags3 & (RF3_EVIL))
			{
				dam = 2 * dam / 3;
				strcpy(note, " resists somewhat.");
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
				strcpy(note, " resists.");
				dam = div_round(dam, 6);
				if (fully_seen) l_ptr->flags3 |= (RF3_RES_CHAOS);
			}

			/* Sometimes polymorph and confuse monsters */
			else if (dam > rand_int(dam + 200))
			{
				if (one_in_(2))
				{
					do_poly = TRUE;
				}
				else if ((m_ptr->confused) && (one_in_(2)))
				{
					/* No additional effect */
				}
				else if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note), 0))
				{
					/* Confuse from 10 to 30 */
					do_conf = 10 + rand_int(MIN(21, dam / 15));
				}
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
				strcpy(note, " resists.");
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
				strcpy(note, " resists.");
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
				strcpy(note, " is immune!");
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

			/* Can stun monsters */
			if ((can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0)) && (one_in_(2)))
			{
				/* Stun from 10 to 30 */
				do_stun = 10 + rand_int(MIN(21, dam / 8));
			}

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
				strcpy(note, " is hit hard.");
				dam = 3 * dam / 2;
				if (fully_seen) l_ptr->flags3 |= (RF3_EVIL);
			}
			else if (r_ptr->d_char == 'A')
			{
				strcpy(note, " is immune!");
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

				strcpy(note, " is unaffected!");
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

			/* No effect on non-living monsters */
			if (monster_nonliving(r_ptr)) dam = 0;

			break;
		}

		/* Forget -- forget character resists */
		case GF_FORGET:
		{
			if (m_ptr->smart)
			{
				/* Erase monster memory of player */
				m_ptr->smart = 0L;

				/* Notice -- if fully visible */
				if (mon_fully_visible(m_ptr))
				{
					char m_poss[32];

					/* Get monster pronoun */
					monster_desc(m_poss, m_ptr, 0x22);

					obvious = TRUE;
					msg_format("%^s forgets all %s knows about you!", m_name, m_poss);
				}
			}

			break;
		}


		/* Curse a monster, attempt to slow, daze, confuse, frighten. */
		case GF_CURSE:
		{
			int curse = rand_int(5);

			/* Determine monster's power to resist. */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 20;
			else                              tmp = r_ptr->level + 5;

			if (r_ptr->flags3 & (RF3_UNDEAD)) tmp += 10;

			/* Attempt a saving throw. */
			if (tmp > randint(dam))
			{
				strcpy(note, " resists the curse!");
				obvious = FALSE;
				dam = 0;

				break;
			}

			/* Effect 0 -- slow */
			if (curse == 0)
			{
				if (can_hinder(DO_SLOW, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_slow = get_skill(S_DOMINION, 10, 30);
				}
			}

			/* Effect 1 -- confusion */
			if (curse == 1)
			{
				if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_conf = get_skill(S_DOMINION, 15, 30);
				}
			}

			/* Effect 2 -- panic */
			if (curse == 2)
			{
				if (can_hinder(DO_FEAR, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_fear = get_skill(S_DOMINION, 20, 50);
				}
			}

			/* Effect 3 -- stun */
			if (curse == 3)
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_stun = get_skill(S_DOMINION, 15, 30);
				}
			}

			/* Strip away bonuses */
			if (curse == 4)
			{
				if (m_ptr->hp > m_ptr->maxhp)
				{
					m_ptr->hp = m_ptr->maxhp;
					strcpy(note, " is cut down to size!");
				}
				else if (m_ptr->hasted)
				{
					/* No longer hasted */
					m_ptr->hasted = 0;

					strcpy(note, " is no longer hasted.");
				}
			}

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Dispel monster */
		case GF_SMITE:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			strcpy(note, " shudders.");
			note_dies = " dissolves!";

			/* Determine monster's power to resist (damage is high) */
			if (r_ptr->flags1 & (RF1_UNIQUE)) tmp = r_ptr->level + 50;
			else tmp = r_ptr->level + 25;


			/* Slow */
			if (tmp <= randint(dam))
			{
				if (can_hinder(DO_SLOW, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_slow = get_skill(S_PIETY, 15, 25);
				}
			}

			/* Confusion */
			if (tmp <= randint(dam))
			{
				if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_conf = get_skill(S_PIETY, 15, 25);
				}
			}

			/* Stun */
			if (tmp <= randint(dam))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
				{
					do_stun = get_skill(S_PIETY, 15, 25);
				}
			}

			break;
		}

		/* Dispel monster */
		case GF_DISPEL:
		{
			/* Obvious */
			if (seen) obvious = TRUE;

			/* Message */
			strcpy(note, " shudders.");
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
				strcpy(note, " is unaffected.");
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

			/* Get character resistance to backlashing, etc. */
			if (p_ptr->mental_barrier)                   psi_resists += 2;
			if (p_ptr->holy)                             psi_resists++;
			if (check_save(100))                         psi_resists++;
			if ((p_ptr->berserk) && (!check_save(100)))  psi_resists--;
			if (p_ptr->confused)                         psi_resists--;
			if (p_ptr->image)                            psi_resists--;
			if (p_ptr->stun)                             psi_resists--;
			if ((p_ptr->afraid) && (!check_save(100)))   psi_resists--;


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
					if (tmp == 5) (void)take_hit(dam / 2, 0, "Your mind is smashed!",
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

			/* Note that EMPTY_MIND has nothing to do with intelligence */

			/* Always have some resistance */
			if (tmp < 5) tmp = 5;

			/* Roll for resistance */
			if (rand_int(dam) < tmp)
			{
				dam = div_round(dam, 3);
				strcpy(note, " resists.");
			}

			/* Smart monsters often win, but suffer if they lose */
			else if (r_ptr->flags2 & (RF2_SMART))
			{
				dam += dam / 2;
				if (dam >= m_ptr->hp / 2) strcpy(note, " is vulnerable!");
			}

			/* Stupid monsters often lose, but it doesn't matter much */
			else if (r_ptr->flags2 & (RF2_STUPID))
			{
				dam = div_round(dam, 3);
				if (dam <= m_ptr->hp / 5)
					strcpy(note, " stupidly resists the attack.");
			}

			/* All mental attacks ignore ordinary resists */

			/* Try to drive the monster insane (doesn't work on everything) */
			if (((20 * dam / tmp) > rand_int(100)) &&
			    (!strchr("gvQ", r_ptr->d_char)))
			{
				m_ptr->mflag |= (MFLAG_MADD);

				if (r_ptr->flags2 & (RF2_SMART))
					strcpy(note, " goes insane!");
				else if (r_ptr->flags2 & (RF2_STUPID))
					strcpy(note, " goes berserk!");
				else
					strcpy(note, " goes mad!");

				/* No more effects */
				dam = 0;
			}

			/* Try to confuse the monster */
			if ((30 * dam / tmp) > rand_int(100))
			{
				if (can_hinder(DO_CONF, m_ptr, 0, note, sizeof(note), 0))
					do_conf = 30;
			}

			/* Try to slow the monster down */
			if ((40 * dam / tmp) > rand_int(100))
			{
				if (can_hinder(DO_SLOW, m_ptr, 0, note, sizeof(note), 0))
					do_slow = 15;
			}

			/* Try to daze the monster */
			if ((50 * dam / tmp) > rand_int(100))
			{
				if (can_hinder(DO_STUN, m_ptr, 0, note, sizeof(note), 0))
					do_stun = 30;
			}

			/* Try to rob the monster of energy */
			if ((50 * dam / tmp) > rand_int(100))
			{
				m_ptr->energy -= randint(m_ptr->energy);
				strcpy(note, " stumbles.");
			}

			/* Try to frighten the monster */
			if ((60 * dam / tmp) > rand_int(100))
			{
				if (can_hinder(DO_FEAR, m_ptr, 0, note, sizeof(note), 0))
					do_fear = 40;
			}

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Madness  (Use "dam" as "power"). */
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

				if (r_ptr->flags2 & (RF2_SMART))
					strcpy(note, " goes insane!");
				else if (r_ptr->flags2 & (RF2_STUPID))
					strcpy(note, " goes berserk!");
				else
					strcpy(note, " goes mad!");
			}

			/* No physical damage */
			dam = 0;
			break;
		}

		/* Frighten monsters (Use "dam" as "power"). */
		case GF_DO_FEAR:
		case GF_DO_TURN:
		{
			int type = DO_FEAR;

			/* Obvious */
			if (seen) obvious = TRUE;

			/* Turn spells ignore NO_FEAR flags */
			if (typ == GF_DO_TURN) type = DO_TURN;

			/* Try to frighten or turn the monster. */
			if (can_hinder(type, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_fear = 25 + p_ptr->power / 5;
				if (typ == GF_DO_TURN) strcpy(note, " runs frantically!");
			}

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Slow Monster (Use "dam" as "power") */
		case GF_DO_SLOW:
		{
			if (seen) obvious = TRUE;

			/* Try to slow the monster down */
			if (can_hinder(DO_SLOW, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_slow = 15 + p_ptr->power / 10;
			}

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Confusion (Use "dam" as "power"). */
		case GF_DO_CONF:
		{
			if (seen) obvious = TRUE;

			/* Try to confuse the monster. */
			if (can_hinder(DO_CONF, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_conf = 20 + p_ptr->power / 10;
			}

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Sleep (Use "dam" as "power"). */
		case GF_DO_SLEEP:
		{
			if (seen) obvious = TRUE;

			/* Try to sleep the monster. */
			if (can_hinder(DO_SLEEP, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_sleep = get_skill(S_STEALTH, 40, 70) - r_ptr->level / 4;
			}

			/* No physical damage. */
			dam = 0;
			break;
		}

		/* Stun (Use "dam" as "power"). */
		case GF_DO_STUN:
		{
			if (seen) obvious = TRUE;

			/* Try to stun the monster. */
			if (can_hinder(DO_STUN, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_stun = 30;
			}

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
				strcpy(note, " spawns!");
			}

			/* No physical damage */
			dam = 0;

			break;
		}

		/* Polymorph monster (Use "dam" as "power") */
		case GF_DO_POLY:
		{
			if (seen) obvious = TRUE;

			/* Try to polymorph the monster. */
			if (can_hinder(DO_POLY, m_ptr, dam, note, sizeof(note),
				    (who < 0 ? LEARN | NOTES : 0)))
			{
				do_poly = TRUE;
			}

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
			strcpy(note, " looks healthier.");

			/* No physical damage */
			dam = 0;
			break;
		}

		/* Speed Monster (use "dam" as duration) */
		case GF_DO_SPEED:
		{
			if (seen) obvious = TRUE;

			/* Speed up */
			if (m_ptr->hasted < MIN(dam, 200))
			{
				/* Note the hasting */
				if (!m_ptr->hasted)
				{
					strcpy(note, " starts moving faster.");
				}
				m_ptr->hasted = (byte)MIN(dam, 200);
			}

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
					strcpy(note, " resists!");
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
					strcpy(note, " resists!");
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
					strcpy(note, " resists!");
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
				strcpy(note, " loses some skin!");
				note_dies = " dissolves!";
			}

			/* Usually ignore the effects */
			else
			{
				/* No damage */
				dam = 0;
				skipped = TRUE;
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
			if ((new_hps <= 32000L) && (new_hps >= m_ptr->maxhp))
				m_ptr->maxhp = (s16b)new_hps;

			/* Heal */
			m_ptr->hp += dam;

			/* No overflow */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Redraw (later) if needed */
			if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

			/* Message */
			strcpy(note, " looks more powerful!");

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

	/* Not using a sleep spell, have not put the monster to sleep */
	if ((typ != GF_DO_SLEEP) && (!do_sleep))
	{
		/* Monster wakes up */
		m_ptr->csleep = 0;

		/* Monster goes active */
		m_ptr->mflag |= (MFLAG_ACTV);
	}


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


	/*
	 * If this is the first monster hit, the spell was capable
	 * of causing damage, and the player was the source of the spell,
	 * make noise. -LM-
	 */
	if ((project_m_n == 1) && (who <= 0) && (dam))
	{
		add_wakeup_chance += p_ptr->base_wakeup_chance / 2 + 500;
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

			/* In line of sight, or close enough to hear through walls */
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
			r_ptr = &r_info[m_ptr->r_idx];


			/* New monster's wounds are much like those of the old monster */
			m_ptr->hp -= (perc * m_ptr->maxhp / 100);

			/* New monster is sometimes slightly confused and/or stunned */
			if (one_in_(2)) m_ptr->confused = 10;
			if (one_in_(2)) m_ptr->stunned  = 10;

			/* Cancel any other effects */
			return (obvious);
		}
	}

	/* Handle slowing -- not cumulative */
	if ((do_slow) && (!m_ptr->slowed))
	{
		if (seen) obvious = TRUE;

		m_ptr->slowed = do_slow;

		if (!strlen(note)) strcpy(note, " starts moving slower.");
	}


	/* Handle stunning */
	if (do_stun)
	{
		if (seen) obvious = TRUE;

		/* Already stunned (not fully cumulative) */
		if (m_ptr->stunned)
		{
			tmp = m_ptr->stunned + (do_stun / 3);
		}

		/* Was not stunned */
		else
		{
			if (!strlen(note)) strcpy(note, " is dazed.");
			tmp = do_stun;
		}

		/* Apply stun (never more than 200) */
		m_ptr->stunned = MIN(tmp, 200);
	}

	/* Handle confusion  */
	if (do_conf)
	{
		if (seen) obvious = TRUE;

		/* Already confused (not fully cumulative) */
		if (m_ptr->confused)
		{
			tmp = m_ptr->confused + (do_conf / 3);
		}

		/* Was not confused */
		else
		{
			if (!strlen(note)) strcpy(note, " looks confused.");
			tmp = do_conf;
		}

		/* Apply confusion (never more than 200) */
		m_ptr->confused = MIN(tmp, 200);
	}

	/* Handle fear */
	if (do_fear)
	{
		if (seen) obvious = TRUE;

		/* Already frightened (not fully cumulative) */
		if (m_ptr->monfear)
		{
			tmp = m_ptr->monfear + (do_fear / 3);
		}

		/* Was not frightened */
		else
		{
			if (!strlen(note)) strcpy(note, " panics!");
			tmp = do_fear;
		}

		/* Panic the monster (never more than 200) */
		set_mon_fear(m_ptr, MIN(tmp, 200), TRUE);
	}

	/* Handle sleeping */
	if (do_sleep)
	{
		if (seen) obvious = TRUE;

		/* Already sleeping (not fully cumulative) */
		if (m_ptr->csleep)
		{
			tmp = m_ptr->csleep + (do_sleep / 3);
		}

		/* Was not sleeping */
		else
		{
			if (!strlen(note)) strcpy(note, " falls asleep.");
			tmp = do_sleep;
		}

		/* Apply sleeping (never more than 200) */
		m_ptr->csleep = MIN(tmp, 200);

		/* Deliberate -- Monster does not go inactive */
	}


	/* Give detailed messages if visible */
	if (strlen(note))
	{
		/* If you get a note, you must be noticing something */
		obvious = TRUE;

		if (fully_seen)
		{
			/* Note - include fear */
			if (fear)
			{
				char *n;
				cptr conj = "and";

				/* Scan to the end of the note */
				for (n = note; (*n != '\0'); n++);

				/* Strip away the last character  XXX */
				n--;    *n = '\0';

				/* Determine conjunction -- look for resistance */
				if ((strstr(note, "resist")) ||
				    (strstr(note, "cannot")) ||
				    (strstr(note, "immune")))
				{
					conj = "but";
				}


				/* Add a fear indicator to the unpunctuated note */
				msg_format("%^s%s - %s flees!", m_name, note, conj);
			}

			/* Standard note */
			else msg_format("%^s%s", m_name, note);

			/* Character notices monster */
			m_ptr->mflag &= ~(MFLAG_MIME);
		}
	}

	/* Standard pain messages */
	else
	{
		/* Monster is hurt, and can either be seen or is fairly close */
		if ((dam > 0) && ((m_ptr->ml) ||
		                  (distance(p_ptr->py, p_ptr->px, y, x) <= 7)))
		{
			/* No special hit message */
			strcpy(note, "");

			/* Message */
			message_pain(cave_m_idx[y][x], dam, fear, note);
		}
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
	int power;
	long tmp_dam;

	/* Adjustment to damage caused by terrain, if any. */
	int terrain_adjustment = 0;

	/* Hack -- assume obvious */
	bool obvious = TRUE;

	/* Player blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Player needs a "description" (he is blind) */
	bool fuzzy = (p_ptr->blind ? TRUE : FALSE);

	/* Source monster and its race */
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Monster name (for damage) */
	char killer[DESC_LEN];

	/* Hack -- messages */
	cptr act = NULL;
	cptr msg = NULL;


	/* No player here */
	if (!(cave_m_idx[y][x] < 0)) return (FALSE);


	/* Adjust "who" to avoid errors  XXX XXX */
	if (who < 0) who = 0;


	/* Get the source monster */
	m_ptr = &m_list[who];

	/* Get the monster race */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Get the attack "power" for miscellaneous effects */
	power = (who > 0 ? r_ptr->level : p_ptr->depth);


	/* A real monster */
	if (who > 0)
	{
		/* Get the killed by name */
		monster_desc(killer, m_ptr, 0x88);
	}

	/* An explosion  XXX */
	else
	{
		strcpy(killer, "an explosion");
	}


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

	/* Characters take up to 33% less damage when in darkness */
	if ((no_light()) && (who > 0))
		dam -= (dam * darkness_ratio(1) / 300);

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
			else terrain_adjustment -= dam / 10;
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
			else if ((typ == GF_WATER) || (typ == GF_STORM))
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
			int odds = 20 - get_skill(S_NATURE, 0, 14);

			if (one_in_(odds))
			{
				msg_print("You duck behind a tree!");
				return (FALSE);
			}
			else terrain_adjustment -= dam / 10;
			break;
		}
	}


	/* Analyze the damage */
	switch (typ)
	{
		/* Boulders -- Crushing.  Armor protects a little. */
		case GF_ROCK:
		{
			/* We've been hit - check for damage, crushing. */
			if (dam)
			{
				/* Affected by terrain. */
				dam += terrain_adjustment;

				/* Character armor reduces total damage (a little) */
				tmp_dam = (long)dam * 180 / (180 + p_ptr->ac + p_ptr->to_a);
				dam = (int)tmp_dam;

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
				(void)take_hit(dam, 0, msg, killer);

				/* Make a boulder sometimes */
				if (one_in_(3))
				{
					make_boulder(y, x, (p_ptr->depth + power) / 2);
				}
			}

			break;
		}

		/* Sling shot -- Stunning, wounding.  Heavy armor protects well.  */
		case GF_SHOT:
		{
			/* Test for deflection - Only base armor counts here. */
			if (p_ptr->ac > 10 + rand_int(50 + power))
			{
				if (!fuzzy) msg_print("A sling shot glances off your armor.");

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
				if (!fuzzy) msg_print("A sling shot ricochets off your shield.");
				dam = 0;
			}

			/* Reduce damage slightly if missile did not get deflected. */
			else
			{
				tmp_dam = (long)dam * 180 / (180 + p_ptr->ac + p_ptr->to_a);
				dam = (int)tmp_dam;
			}

			/* We've been hit - check for stunning, wounding. */
			if (dam)
			{
				if (fuzzy) msg = "You are hit by a sling shot.";
				else msg = "You are hit.";

				/* Affected by terrain. */
				dam += terrain_adjustment;

				/* Take damage */
				if (take_hit(dam, 0, msg, killer)) break;

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

		/* Edged physical missiles -- Frequent wounding.  Armor protects some. */
		case GF_ARROW:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Test for deflection - Only base armor counts here. */
			if (p_ptr->ac > 10 + rand_int(50 + power))
			{
				if (!fuzzy) msg_print("A missile glances off your armor.");

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
				if (!fuzzy) msg_print("A missile ricochets off your shield.");
				dam = 0;
			}

			/* Reduce damage (slightly) if missile did not get deflected. */
			else
			{
				tmp_dam = (long)dam * 180 / (180 + p_ptr->ac + p_ptr->to_a);
				dam = (int)tmp_dam;
			}

			if (dam)
			{
				/* Hit the player */
				if (fuzzy) msg = "You are hit by an arrow!";
				else msg = "You are hit.";

				/* Take damage */
				if (take_hit(dam, 0, msg, killer)) break;

				/* Player can be wounded. */
				if (one_in_(3))
				{
					/* Wound the player. */
					(void)set_cut(p_ptr->cut + dam / 2);
				}
			}

			break;
		}

		/*
		 * Miscellaneous physical missiles.  Armor reduces
		 * damage.
		 * Also venomous missiles.  These get nasty with Morgul-magic.
		 * Ringwraiths and Sauron are very dangerous.
		 */
		case GF_MISSILE:
		case GF_PMISSILE:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Test for a deflection (ordinary missiles only). */
			if ((typ == GF_MISSILE) &&
			    (inventory[INVEN_ARM].k_idx) &&
			    (inventory[INVEN_ARM].tval == TV_SHIELD) &&
			    (!p_ptr->shield_on_back) &&
			    (inventory[INVEN_ARM].ac > rand_int(MAX_SHIELD_BASE_AC * 3)))
			{
				/* No damage. */
				if (!fuzzy) msg_print("A missile ricochets off your shield.");
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
						else if ((prefix(killer, "Sauron, the Sorcerer")) ||
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
							if ((power > 50) && (one_in_(3)))
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

				/* Armor reduces damage (if not a morgul-missile) */
				if (k < 4)
				{
					tmp_dam = (long)dam * 180 /
						(180 + p_ptr->ac + p_ptr->to_a);
					dam = (int)tmp_dam;
				}

				/* Ordinary missile. */
				if (k == 0)
				{
					/* No special damage. */
					(void)take_hit(dam, 0, msg, killer);
				}

				/* Poisonous missiles */
				else
				{
					int temp = 0;

					/* Take damage */
					if (take_hit(dam, 0, msg, killer)) break;

					/* Then a cute message, */
					if (k >= 4) msg_print("Foul magics assault body and mind!");

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
					if (temp) (void)set_poisoned(p_ptr->poisoned + temp);

					/* Then the life draining, */
					if (k >= 2)
					{
						if (p_ptr->hold_life && (one_in_(4)))
						{
							msg_print("You feel your life slipping away!");
							lose_exp((power) + (calc_spent_exp() / 1000) *
								MON_DRAIN_LIFE, FALSE);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp((power * 2) + (calc_spent_exp() / 100) *
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
					if (k >= 4)
					{
						if (p_ptr->black_breath == FALSE)
						{
							/* Messages. */
							msg_print("The missile bears a terrible curse!");
							message_flush();
							message(MSG_L_RED, 200, "You feel the Black Breath slowly draining you of life...");

							/* Inflict the Black Breath */
							p_ptr->black_breath = TRUE;
						}
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
				(void)take_hit(dam, 0, NULL, killer);
			}
			break;
		}

		/* Pure hurt */
		case GF_HURT:
		{
			/* Not affected by terrain  XXX */

			/* No resists, adjusts, etc. */
			(void)take_hit(dam, 0, NULL, killer);

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
			if (p_ptr->leaving) break;

			/* Strong Morgul-cold can have extra side effects. */

			/* 100% of the time if no resistance, 33% if only one resistance. */
			if ((r_ptr->flags2 & (RF2_MORGUL_MAGIC)) && (!p_ptr->immune_cold) &&
				(((!p_ptr->resist_cold) && (!p_ptr->oppose_cold)) ||
				(((!p_ptr->resist_cold) || (!p_ptr->oppose_cold)) &&
				(one_in_(3)))))
			{
				k = randint(3);

				if ((k == 1) && (dam >= 50) && (!p_ptr->sustain_con))
				{
					(void)do_dec_stat(A_CON, 1, FALSE,
						"A deadly chill seeps into your bones.", NULL);
				}
				else if ((k == 2) && (dam >= 150) && (!p_ptr->hold_life))
				{
					msg_print("A deadly chill withers your lifeforce.");
					lose_exp((power * 2) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
				else if ((k == 3) && (dam >= 200))
				{
					bool c_paralyze = FALSE, c_panic = FALSE, c_drain = FALSE;

					if (!p_ptr->free_act) c_paralyze = TRUE;
					if (!p_ptr->resist_fear) c_panic = TRUE;
					if (!p_ptr->hold_life) c_drain = TRUE;

					/* Messages and effects */
					if (c_paralyze || c_panic || c_drain)
					{
						msg_print("A deadly chill drives daggers into your soul!");
						if (c_paralyze)
							(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 3));
						if (c_panic)
							(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
						if (c_drain)
						{
							/* Serious, but temporary, loss of exp. */
							lose_exp((power * 10) + (calc_spent_exp() / 50) *
								MON_DRAIN_LIFE, FALSE);
						}
					}
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

			/* Handle vulnerability */
			if (p_ptr->vuln_pois) dam += dam / 2;

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Poison the player */
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				power = (p_ptr->poisoned ?
				         rand_int(dam / 2) : 5 + rand_range(dam / 3, dam));
				(void)set_poisoned(p_ptr->poisoned + power);
			}

			/* Poison the player through resistances, if very strong */
			else if ((!p_ptr->resist_pois || !p_ptr->oppose_pois) &&
			         (dam > 100) && (!p_ptr->poisoned))
			{
				msg_print("The venom breaks through your resistance!");
				(void)set_poisoned(p_ptr->poisoned + 10);
			}

			/*
			 * Some nasty possible side-effects of Morgul-poison.  Poison
			 * resistance reduces the damage counted when determining effects.
			 */
			if (r_ptr->flags2 & (RF2_MORGUL_MAGIC))
			{
				/* Paralyzation. */
				if (!check_save(dam / 2 + 20))
				{
					msg_print("The deadly vapor overwhelms you, and you faint away!");
					if (p_ptr->free_act)
						(void)set_paralyzed(p_ptr->paralyzed + 1);
					else
						(void)set_paralyzed(p_ptr->paralyzed + rand_range(2, 4));
				}

				else if ((!p_ptr->resist_blind) && (!check_save(dam + 20)))
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

			/* Take damage */
			if (take_hit(div_round(dam, 3), 0, NULL, killer)) break;

			/* Test player's saving throw. */
			if (!check_save(5 * power / 4))
			{
				/* Note lack of resistance */
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

			/* Handle vulnerability */
			if (p_ptr->vuln_lite) dam += dam / 2;

			if ((p_ptr->resist_lite) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + rand_range(dam / 8, dam / 4),
					"You are blinded by the flash!");
			}
			(void)take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Dark -- blinding */
		case GF_DARK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Handle vulnerability */
			if (p_ptr->vuln_dark) dam += dam / 2;

			if (fuzzy) msg_print("You are hit by something!");
			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + 3 + rand_range(dam / 8, dam / 4),
					"Everything goes black!");
			}
			(void)take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Morgul-dark -- very dangerous if sufficiently powerful. */
		case GF_MORGUL_DARK:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You feel a deadly blackness surround you!");

			/* Handle vulnerability */
			if (p_ptr->vuln_dark) dam += dam / 2;

			/* Adjust damage for darkness resistance, or blind the player. */
			if ((p_ptr->resist_dark) || (p_ptr->oppose_ethereal))
			{
				dam = div_round(dam, 2);
			}
			else if (!blind && !p_ptr->resist_blind)
			{
				(void)set_blind(p_ptr->blind + 5 + rand_range(dam / 4, dam / 2),
					"Darkness blots out your vision!");
			}

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Determine power of attack - usually between 25 and 350. */
			k = dam * power / 100;

			/* Hack - The Ringwraiths and Sauron are very dangerous. */
			if ((prefix(killer, "Sauron, the Sorcerer")) ||
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
					msg_print("A hideous corpse-scent pervades the air...");
					(void)set_poisoned(p_ptr->poisoned + 10 + randint(dam));
				}
				else if (!p_ptr->resist_pois || !p_ptr->oppose_pois)
				{
					msg_print("A nasty corpse-scent pervades the air...");
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
						lose_exp((power * 2) + (calc_spent_exp() / 1000) *
							MON_DRAIN_LIFE, FALSE);
					}
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp((power * 4) + (calc_spent_exp() / 100) *
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
				if (!check_save(k))
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
					message(MSG_L_RED, 200, "You feel the Black Breath slowly draining you of life...");
					p_ptr->black_breath = TRUE;
				}
				else
				{
					message(MSG_L_RED, 200, "You feel the Black Breath sucking away your lifeforce!");
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

			/* Handle vulnerability */
			if (p_ptr->vuln_confu) dam += dam / 2;

			if (p_ptr->resist_confu)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)set_confused(p_ptr->confused + 2 +
					rand_range(dam / 4, dam / 2));
			}
			(void)take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Sound -- mostly stunning and confusing, can paralyze */
		case GF_SOUND:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			if (fuzzy) msg_print("You are blasted by sound.");

			/* Handle vulnerability */
			if (p_ptr->vuln_sound) dam += dam / 2;


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
					k = (randint((dam > 400) ? 21 : (dam / 20)));
					(void)set_confused(p_ptr->confused + k);
				}

				/* Stun the player. */
				k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
				(void)set_stun(p_ptr->stun + k);

				/* Sometimes, paralyze the player briefly. */
				if (!check_save(dam))
				{
					/* Warning */
					msg_print("The noise shatters your wits, and you struggle to recover.");

					/* Hack - directly reduce player energy. */
					p_ptr->energy -= (s16b)rand_int(dam / 2);
					if (p_ptr->energy < 0) p_ptr->energy = 0;
				}
			}

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Resistance to sound - much less inventory destruction. */
			if (p_ptr->resist_sound) k = dam / 3;
			else                     k = dam;

			/* Blow up flasks and potions sometimes. */
			(void)inven_damage(set_cold_destroy,
			                   MIN(40, div_round(k, 10)), k);

			break;
		}

		/* Shards -- mostly cutting.  Shields may offer some protection. */
		case GF_SHARD:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			/* Handle vulnerability */
			if (p_ptr->vuln_shard) dam += dam / 2;

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

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Resistance to shards - much less inventory destruction. */
			if (p_ptr->resist_shard) k = dam / 3;
			else k = dam;

			/* Blow up flasks and potions on rare occasions. */
			(void)inven_damage(set_cold_destroy,
				                MIN(30, div_round(k, 15)), k);

			break;
		}

		/* Inertia -- slowness */
		case GF_INERTIA:
		{
			if (fuzzy) msg_print("You are hit by something strange!");
			if (take_hit(dam, 0, NULL, killer)) break;
			(void)set_slow(p_ptr->slow + 2 + rand_int(dam / 15));
			break;
		}

		/* Gravity -- stunning and slowness. */
		case GF_GRAVITY:
		{
			if (fuzzy) msg_print("Gravity buckles around you!");

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			if (randint(dam) > 20)
				(void)set_slow(p_ptr->slow + rand_range(2, 4));

			if (!p_ptr->resist_sound)
			{
				k = randint((dam > 200) ? 20 : (dam / 10));
				(void)set_stun(p_ptr->stun + k);
			}

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

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			if (!p_ptr->resist_sound)
			{
				(void)set_stun(p_ptr->stun + randint(dam / 10 + 5));
			}

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

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Can confuse or stun the character */
			if (dam > randint(200))
			{
				if ((!p_ptr->resist_sound) && (one_in_(2)))
				{
					(void)set_stun(p_ptr->stun + randint(5 + dam / 10));
				}
				if ((!p_ptr->resist_confu) && (one_in_(3)))
				{
					(void)set_confused(p_ptr->confused + 2 + dam / 25);
				}
			}
			break;
		}

		/* Wind -- confusion, but rarely if res_confu */
		case GF_WIND:
		{
			if (fuzzy) msg_print("You are buffeted by winds!");

			/* Take damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Can confuse the character */
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
			if (take_hit(dam / 2, 0, NULL, killer)) break;

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
			if (!p_ptr->leaving) cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nexus -- Effects processed later, in "project_t()" */
		case GF_NEXUS:
		{
			if (fuzzy) msg_print("You are hit by something strange!");

			/* Handle vulnerability */
			if (p_ptr->vuln_nexus) dam += dam / 2;

			if (p_ptr->resist_nexus)
			{
				dam = div_round(dam, 2);
			}
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Mark grid for later processing. */
			cave_temp_mark(y, x, FALSE);

			break;
		}

		/* Nether -- drain experience */
		case GF_NETHER:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Handle vulnerability */
			if (p_ptr->vuln_nethr) dam += dam / 2;

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
					lose_exp((power) + (calc_spent_exp() / 1000) *
						MON_DRAIN_LIFE, FALSE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp((power * 2) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
			}
			(void)take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Chaos -- many effects.  */
		case GF_CHAOS:
		{
			/* Slightly affected by terrain. */
			dam += terrain_adjustment / 2;

			/* Handle vulnerability */
			if (p_ptr->vuln_chaos) dam += dam / 2;

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


			/* Take some damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Can lose exp */
			if ((!p_ptr->resist_chaos) && (one_in_(2)))
			{
				if (p_ptr->hold_life && (!one_in_(4)))
				{
					msg_print("You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					msg_print("You feel your life slipping away!");
					lose_exp((power * 1) + (calc_spent_exp() / 1000) *
						MON_DRAIN_LIFE, FALSE);
				}
				else
				{
					msg_print("You feel your life draining away!");
					lose_exp((power * 5) + (calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
				}
			}

			break;
		}

		/* Disenchantment -- see above */
		case GF_DISENCHANT:
		{
			if (fuzzy) msg_print("You are hit by something strange!");

			/* Handle vulnerability */
			if (p_ptr->vuln_disen) dam += dam / 2;

			if (p_ptr->resist_disen)
			{
				dam = div_round(dam, 2);
			}
			else
			{
				(void)apply_disenchant(dam);
			}
			(void)take_hit(dam, 0, NULL, killer);
			break;
		}

		/* Time */
		case GF_TIME:
		{
			if (fuzzy) msg_print("You are hit by something strange!");

			/* Take some damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			switch (randint(10))
			{
				case 1: case 2: case 3: case 4: case 5:
				{
					msg_print("You feel life has clocked back.");
					lose_exp((power * 2) + (calc_spent_exp() / 100) *
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
			break;
		}

		/* Pure damage */
		case GF_MANA:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by something!");
			(void)take_hit(dam, 0, NULL, killer);
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

			(void)take_hit(dam * factor / 100, 0, NULL, killer);
			break;
		}

		/* Pure damage */
		case GF_METEOR:
		case GF_BLACK_ORB:
		{
			/* Affected by terrain. */
			dam += terrain_adjustment;

			if (fuzzy) msg_print("You are hit by a doomspell!");
			if (take_hit(dam, 0, NULL, killer)) break;

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
			if (p_ptr->resist_nethr && !p_ptr->vuln_nethr) resist++;
			if (p_ptr->hold_life)    resist++;

			/* Apply resistance */
			dam /= (1 + resist);

			/* Take some damage */
			if (take_hit(dam, 0, NULL, killer)) break;

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

			break;
		}

		/* Spores - poison, cause disease */
		case GF_SPORE:
		{
			if (fuzzy) msg_print("You feel spores all around you...");

			/* Affected slightly by terrain. */
			dam += terrain_adjustment / 2;

			/* Take some damage */
			if (take_hit(dam, 0, NULL, killer)) break;

			/* Handle vulnerability */
			if (p_ptr->vuln_pois) dam += dam / 2;

			/* Poison */
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				set_poisoned(p_ptr->poisoned + randint(dam * 2));
			}

			/* Disease */
			if (randint(power) >= 10)
			{
				int pow = rand_range(dam * 2, dam * 3);
				disease(&pow);
			}

			break;
		}

		/* Forget -- forget memories, lose experience */
		case GF_FORGET:
		{
			if (!check_save(100))
			{
				(void)lose_all_info("Your memories fade away.");
			}
			if (!check_save(100))
			{
				/* Lose some experience (hold life doesn't help) */
				msg_print("You forget some of your past experiences.");
				lose_exp((calc_spent_exp() / 100) *
						MON_DRAIN_LIFE, FALSE);
			}

			break;
		}


		/* Curse a character, attempt to slow, daze, confuse, frighten. */
		case GF_CURSE:
		{
			int curse = rand_int(4);

			/* Attempt a saving throw */
			if (check_save(100))
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
					(void)set_slow(p_ptr->slow + 5 + rand_int(dam / 3));
				}

				/* Effect 1 -- confusion */
				if ((curse == 1) && (!p_ptr->resist_confu))
				{
					(void)set_confused(p_ptr->confused + 5 + rand_int(dam / 3));
				}

				/* Effect 2 -- panic */
				if ((curse == 2) && (!p_ptr->resist_fear))
				{
					(void)set_afraid(p_ptr->afraid + 8 + rand_int(dam / 2));
				}

				/* Effect 3 -- stun */
				if ((curse == 3) && (!p_ptr->resist_sound))
				{
					(void)set_stun(p_ptr->stun + 5 + rand_int(dam / 3));
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
			int psi_resists = 3;
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
			if (p_ptr->mental_barrier)                  psi_resists += 5;
			if (p_ptr->holy)                            psi_resists += 2;
			if (check_save(100))                        psi_resists += 2;
			if ((p_ptr->berserk) && (!check_save(100))) psi_resists--;
			if (p_ptr->confused)                        psi_resists--;
			if (p_ptr->image)                           psi_resists--;
			if ((p_ptr->stun) && (!check_save(100)))    psi_resists--;
			if ((p_ptr->afraid) && (!check_save(100)))  psi_resists--;

			/* Get effect factor (dam / resist) */
			if (psi_resists < 1) psi_resists = 1;
			tmp = 2 * dam / psi_resists;


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
					rand_range(dam / 30, dam / 20));
				if (do_blind) set_blind(p_ptr->blind + 3 +
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
			(void)take_hit(dam, 0, NULL, killer);

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
			if (dam > randint(400))
			{
				set_self_knowledge(p_ptr->self_knowledge + MIN(10, 1 + dam / 20),
					"You begin to know yourself a little better...");
			}

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

	char m_name[DESC_LEN];

	int k, d;

	bool seen = FALSE;
	bool obvious = FALSE;

	bool affect_player = FALSE;
	bool affect_monster = FALSE;

	int do_dist = 0;
	bool do_dist_los = FALSE;

	/* Assume no note */
	cptr note = NULL;

	/* Only process marked grids. */
	if (!(cave_info[y][x] & (CAVE_TEMP))) return (FALSE);

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
		monster_desc(m_name, m_ptr, 0x40);
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
					if ((cave_feat[y][x] >= FEAT_MAGMA_K) &&
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
							drop_near(i_ptr, 0, y, x, DROP_HERE);
						}
					}

					cave_set_feat(y, x, FEAT_RUBBLE);
				}

				/* Rubble becomes floor */
				else if (cave_feat[y][x] == FEAT_RUBBLE)
				{
					cave_set_feat(y, x, get_nearby_floor(y, x));
					if (cave_m_idx[y][x] < 0)
						msg_print("The rubble you are standing on dissolves!");
				}
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
					if (!one_in_(3)) cave_set_feat(y, x, get_nearby_floor(y, x));
					else             cave_set_feat(y, x, FEAT_RUBBLE);

					if (cave_m_idx[y][x] < 0)
						msg_print("The lava you are standing on freezes solid!");
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
			if (dam > rand_range(600, 3600))
			{
				if ((cave_floor_bold(y, x)) ||
					(cave_feat[y][x] == FEAT_RUBBLE))
				{
					/* Forget the floor or rubble. */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Make lava. */
					cave_set_feat(y, x, FEAT_LAVA);

					if (cave_m_idx[y][x] < 0)
						msg_print("The ground you are standing on turns to lava!");
				}
			}

			/* Can boil water if very strong. */
			if (cave_feat[y][x] == FEAT_WATER)
			{
				/* Start with some water */
				k = 2;

				/* Look around for nearby water. */
				for (d = 0; d < 8; d++)
				{
					/* Extract adjacent (legal) location */
					int yy = y + ddy_ddd[d];
					int xx = x + ddx_ddd[d];

					/* Add water grids */
					if (cave_feat[yy][xx] == FEAT_WATER) k++;
				}

				/*
				 * Is the fire strong enough? Large ponds are difficult
				 * to evaporate, as Smaug found out the hard way.
				 */
				if (dam > rand_range(k * 100, k * 300))
				{
					/* Forget the water */
					cave_info[y][x] &= ~(CAVE_MARK);

					/* Destroy the water */
					cave_set_feat(y, x, get_nearby_floor(y, x));

					if (cave_m_idx[y][x] < 0)
						msg_print("The water you are in turns to steam!");
				}
			}

			/* Can burn trees if strong. */
			if ((cave_feat[y][x] == FEAT_TREE) && (dam > rand_range(100, 400)))
			{
				/* Forget the tree */
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the tree */
				cave_set_feat(y, x, get_nearby_floor(y, x));

				if (cave_m_idx[y][x] < 0)
					msg_print("The trees around you explode in fire!");
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
					teleport_player(6, FALSE, TRUE);
				}
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 4 + div_round(dam, 25);
				do_dist_los = TRUE;

				/* Resist even when affected */
				if (r_ptr->flags4 & (RF4_BRTH_GRAV)) do_dist = 0;
				else if (r_ptr->flags4 & (RF4_BRTH_NEXUS)) do_dist /= 4;
				else if (r_ptr->flags3 & (RF3_RES_NEXUS)) do_dist /= 2;
				else if (r_ptr->flags3 & (RF3_RES_TPORT))
					do_dist = 2 * do_dist / 3;

				/* Big, heavy monsters */
				if (strchr("DGP#X", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgv&", r_ptr->d_char)) do_dist /= 2;

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
				thrust_away(who, y, x, 1 + div_round(dam, 15));

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
				int dist = div_round(20 * dam, p_ptr->wt);
				if (dist > 8) dist = 8;

				/* Feather fall greatly reduces the effect of wind */
				if (p_ptr->ffall) dist = (dist + 2) / 3;

				/* Messages */
				if (dist >= 6)
					msg_print("The wind grabs you, and whirls you around!");
				else if (dist >= 1)
					msg_print("The wind buffets you about.");

				/* Throw the player around unsafely. */
				teleport_player(dist, FALSE, TRUE);
			}

			if (affect_monster)
			{
				/* Damage-variable throw distance */
				do_dist = 1 + div_round(dam, 25);
				if (do_dist > 8) do_dist = 8;
				do_dist_los = TRUE;

				/* Big, heavy monsters (or ghosts) */
				if (strchr("DGP#", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgvq", r_ptr->d_char)) do_dist /= 2;
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
				teleport_player(dist, FALSE, TRUE);
			}

			if ((typ == GF_STORM) && (affect_monster))
			{
				/* Damage-variable throw distance */
				do_dist = div_round(dam, 30);
				if (do_dist > 8) do_dist = 8;
				do_dist_los = TRUE;

				/* Big, heavy monsters */
				if (strchr("DGP#", r_ptr->d_char)) do_dist /= 3;
				else if (strchr("OTdgv", r_ptr->d_char)) do_dist /= 2;
			}

			/* Require strong attack.  Require floor. */
			if ((dam >= 60) && (cave_floor_bold(y, x)))
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

					if (cave_m_idx[y][x] < 0)
						msg_print("You are standing in a pool of water!");
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
					if (!one_in_(3)) cave_set_feat(y, x, get_nearby_floor(y, x));
					else             cave_set_feat(y, x, FEAT_RUBBLE);

					if (cave_m_idx[y][x] < 0)
						msg_print("The lava you are standing becomes solid rock!");
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
			if (summon_specific(y, x, FALSE, 100, SUMMON_INDEX, 1))
			{
				if (player_can_see_or_infra_bold(y, x))
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
			if (summon_specific(y, x, FALSE, 100, SUMMON_INDEX, 1))
			{
				if (player_can_see_or_infra_bold(y, x))
					msg_print("The raw magic coalesces!");
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
				teleport_player_to(n_ptr->fy, n_ptr->fx, 0, FALSE, 0);
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

				/* Monster loses a turn */
				m_ptr->mflag |= (MFLAG_TURN);

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
				teleport_player(dam, FALSE, FALSE);
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
		teleport_away(cave_m_idx[y][x], do_dist, do_dist_los);

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
		(void)update_mon(cave_m_idx[y][x], FALSE);

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
 *
 * Note:  We use 240-degree circles.
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
		/* Get the first degree for this arc */
		arc_first[i] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(240, *arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 240 * (i+1) / *arc_num)
		    degree_first = 240 * (i+1) / *arc_num;
		if (degree_first > (240 + *arc_num) * (i+1) / *arc_num)
		    degree_first = (240 + *arc_num) * (i+1) / *arc_num;


		/* Get the center of the arc */
		center_of_arc = (degree_first + arc_first[i]) / 2;

		/* Get arc distance from the horizontal (0 and 120 degrees) */
		if      (center_of_arc <  60) vert_factor = center_of_arc;
		else if (center_of_arc > 180) vert_factor = 240 - center_of_arc;
		else                          vert_factor = ABS(center_of_arc - 120);


		/*
		 * Usual case -- Calculate distance to expand outwards.  Pay more
		 * attention to width near the horizontal, more attention to height
		 * near the vertical.
		 */
		dist = ((height * vert_factor) + (width * (60 - vert_factor))) / 60;

		/* Randomize distance (should never be greater than radius) */
		arc_dist[i] = rand_range(dist / 4, dist / 2);

		/* Keep variability under control (except in special cases). */
		if ((dist != 0) && (i != 0))
		{
			int diff = arc_dist[i] - arc_dist[i-1];

			if (ABS(diff) > size)
			{
				if (diff > 0) arc_dist[i] = arc_dist[i-1] + size;
				else          arc_dist[i] = arc_dist[i-1] - size;
			}
		}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (TRUE)
	{
		int diff = arc_dist[*arc_num - 1] - arc_dist[0];

		if (ABS(diff) > size)
		{
			if (diff > 0) arc_dist[*arc_num - 1] = arc_dist[0] + size;
			else          arc_dist[*arc_num - 1] = arc_dist[0] - size;
		}
	}
}


/*
 * Character can sometimes dodge bolts.
 */
static bool dodge_bolt_player(int who, int y0, int x0, int *y1, int *x1)
{
	/* Get the source monster */
	monster_type *m_ptr = &m_list[who];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Take reaction time into account */
	int diff = 300 / (m_ptr->cdis + 1);
	if (!m_ptr->ml) diff += 100;

	/* Characters can sometimes dodge */
	if (dodging_ability(70) > 5 + rand_int(diff + r_ptr->level))
	{
		/* Missile whizzes right past the character */
		*y1 += (*y1 - y0);
		*x1 += (*x1 - x0);

		/* Dodge */
		msg_print("You nimbly dodge aside.");
		return (TRUE);
	}

	/* Don't dodge */
	return (FALSE);
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
 * A bolt travels from source to target and affects only the last grid it
 *   enters.  If given the PROJECT_STOP flag, it is stopped by any monster
 *   or character in its path (at present, all bolts use this flag).
 *
 * Beam:  (PROJECT_BEAM)
 * A beam travels from source to target, affecting all grids passed through
 *   with full damage.  It is never stopped by monsters in its path.  Beams
 *   may never be combined with any other projection type.
 *
 * Ball:  (positive radius, no PROJECT_ARC flag)
 * A ball travels from source towards the target, and always explodes.  Unless
 *   specified, it does not affect wall grids, but otherwise affects any grids
 *   in LOS from the center of the explosion.
 * If used with a direction, a ball will explode on the first occupied grid in
 *   its path.  If given a target, it will explode on that target.  If a
 *   wall is in the way, it will explode against the wall.  If a ball reaches
 *   MAX_RANGE without hitting anything or reaching its target, it will
 *   explode at that point.
 *
 * Arc:  (positive radius, with PROJECT_ARC flag)
 * An arc is a portion of a source-centered ball that explodes outwards
 *   towards the target grid.  Like a ball, it affects all non-wall grids in
 *   LOS of the source in the explosion area.  The width of arc spells is
 *   controlled by the variable "degrees".
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
 *   past that target.  This is appropriate for physical missiles (crossbow
 *   bolts, arrows, etc.)
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
 *   source diameters ranging up to 200, which allows the spell designer to
 *   fine-tune how quickly a breath loses strength outwards from the breather.
 *   It is expected, but not required, that wide arcs lose strength more
 *   quickly over distance (see the math in the "fire_arc()" function).
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
 * If the delay factor is anything other than zero, bolt and explosion
 * pictures will be momentarily shown on screen.
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
bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam,
		int typ, u32b flg, int degrees, byte source_diameter)
{
	int i, j, k, m, dist = 0;

	u32b dam_temp;
	int rotate = 0;

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


	/* Apply and cancel flags */
	if ((p_ptr->proj_temp_flags) || (p_ptr->proj_temp_flags_cancel))
	{
		flg |= (p_ptr->proj_temp_flags);
		p_ptr->proj_temp_flags = 0L;
		flg &= ~(p_ptr->proj_temp_flags_cancel);
		p_ptr->proj_temp_flags_cancel = 0L;
	}

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


	/* Some projection types are always PROJECT_WALL. */
	if ((typ == GF_KILL_WALL) || (typ == GF_KILL_DOOR) ||
		 (typ == GF_FORCE_DOOR))
	{
		flg |= (PROJECT_WALL);
	}

	/* Some projection types are always PROJECT_HIDE */
	if (typ == GF_HURT) flg |= PROJECT_HIDE;

	/* Hack -- Sound attacks are extremely noisy. */
	if (typ == GF_SOUND) add_wakeup_chance = MAX(add_wakeup_chance, 10000 + dam * 200);


	/* Hack -- Jump to target (if valid) */
	if ((flg & (PROJECT_JUMP)) && (y1) && (x1))
	{
		y = y0 = y1;
		x = x0 = x1;
	}

	/* If a single grid is both source and destination, store it. */
	if ((x1 == x0) && (y1 == y0))
	{
		gy[grids] = y0;
		gx[grids] = x0;
		gd[grids++] = 0;
	}

	/* Otherwise, travel along the projection path (unless arc). */
	else if (!(flg & (PROJECT_ARC)))
	{
		/* Determine maximum length of projection path  XXX */
		if (flg & (PROJECT_BOOM)) dist = MAX_RANGE;
		else if (rad <= 0)        dist = MAX_RANGE;
		else                      dist = rad;


		/* Calculate the projection path */
		(void)project_path(dist, y0, x0, &y1, &x1, flg);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int oy = y;
			int ox = x;

			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);

			/* Hack -- Shorten delay if path is long */
			m = (path_n <= 4 ? msec : (path_n <= 8 ? 2*msec/3 : msec/2));


			/* Hack -- Balls explode before reaching a wall.  XXX */
			/* Reconsider this rule  XXX XXX XXX DEBUG */
			if ((flg & (PROJECT_BOOM)) && (cave_wall_bold(ny, nx)))
			{
				break;
			}

			/* Advance */
			y = ny;
			x = nx;


			/* Character is in the way */
			if (cave_m_idx[y][x] < 0)
			{
				/* Monster is directing a bolt or beam at the character */
				if ((who > 0) && (flg & (PROJECT_PLAY)) &&
					 ((flg & (PROJECT_BEAM)) || !(flg & (PROJECT_BOOM))))
				{
					/* Attempt to dodge the bolt or beam */
					if (dodge_bolt_player(who, y0, x0, &y1, &x1))
					{
						/* No longer hurts the character */
						flg &= ~(PROJECT_PLAY);

						/* This projection stops if it hits anything */
						if (flg & (PROJECT_STOP))
						{
							/* Hide the character  XXX */
							int char_tmp = cave_m_idx[y][x];
							cave_m_idx[y][x] = 0;

							/* Recalculate the projection path */
							(void)project_path(dist, y0, x0, &y1, &x1, flg);

							/* Make the character reappear XXX */
							cave_m_idx[y][x] = char_tmp;
						}
					}
				}
			}


			/* Grid is in projection path */
			if (path_gx[i] < PATH_G_NONE)
			{
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
			}

			/* Only do visuals if requested */
			if (!blind && !(flg & (PROJECT_HIDE | PROJECT_NO_TRAIL)))
			{
				/* Only do visuals if the player can "see" the projection */
				if (panel_contains(y, x) && player_has_los_bold(y, x) &&
				    (path_gx[i] < PATH_G_NONE))
				{
					u16b p;

					byte a;
					char c;

					/* Obtain the bolt or explosion pict */
					if (flg & (PROJECT_BEAM)) p = bolt_pict(y, x, y, x, typ);
					else                      p = bolt_pict(oy, ox, y, x, typ);

					/* Extract attr/char */
					a = PICT_A(p);
					c = PICT_C(p);

					/* Display the visual effects */
					print_rel(c, a, y, x);
					move_cursor_relative(y, x);
					if (op_ptr->delay_factor) (void)Term_fresh();

					/* Extra delay if monster in way, if PROJECT_STOP */
					if ((flg & (PROJECT_STOP)) && (cave_m_idx[y][x] != 0))
					{
						m = 10 + msec;
					}

					/* Delay */
					pause_for(m);

					/* Erase the visual effects, unless a beam */
					if (!(flg & (PROJECT_BEAM)))
					{
						lite_spot(y, x);
						if (op_ptr->delay_factor) (void)Term_fresh();
					}

					/* If a beam, erase later */
					else
					{
						drawn = TRUE;
					}

					/* Hack -- Activate delay */
					visual = TRUE;
				}

				/* Hack -- Always delay for consistency */
				else if (visual)
				{
					/* Delay for consistency */
					pause_for(m);
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
			spread_cave_temp(y2, x2, rad, FALSE);
		}

		/* Pre-calculate some things for arcs. */
		if (flg & (PROJECT_ARC))
		{
			/*
			 * A weakness of this system of calculating arcs is that the closest
			 * grids may be skipped if the arc is narrow enough.  Avoid this
			 * horrid fate.  XXX XXX
			 */
			if (degrees <= 55)
			{
				k = MIN(rad / 2, 60 / (degrees + 5));

				/* Calculate the first part of the projection path */
				(void)project_path(20, y0, x0, &y1, &x1, flg);

				/* Store the grids */
				for (i = 0; i < MIN(k, path_n); i++)
				{
					/* Grid is not skipped, and is not a wall */
					if ((path_gx[i] < PATH_G_NONE) &&
						(path_gx[i] != PATH_G_WALL))
					{
						/* Save the grid */
						gy[grids] = GRID_Y(path_g[i]);
						gx[grids] = GRID_X(path_g[i]);
						gd[grids++] = i;

						/* Mark the grid (it will not be used again) */
						cave_temp_mark(gy[grids-1], gx[grids-1], FALSE);
					}
				}
			}


			/* The radius of arcs cannot be more than 20 */
			if (rad > 20) rad = 20;

			/* Reorient the grid forming the end of the arc's centerline */
			n1y = y1 - y0 + 20;
			n1x = x1 - x0 + 20;

			/* Correct overly large or small values */
			if (n1y > 40) n1y = 40;
			if (n1x > 40) n1x = 40;
			if (n1y <  0) n1y =  0;
			if (n1x <  0) n1x =  0;

			/* Get rotation value for central angle (needed for comparison) */
			rotate = 120 - get_angle_to_grid[n1y][n1x];
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

				/* This is a non-projectable grid (whether passable or not). */
				if (!cave_project_bold(y, x))
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

						/* Get (non-normalized) angle to current grid */
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
					int n2y, n2x, val, diff;

					/* Skip already marked grids */
					if (cave_info[y][x] & (CAVE_TEMP)) continue;

					/* Reorient current grid for table access. */
					n2y = y - y2 + 20;
					n2x = x - x2 + 20;

					/* Get angle to this grid and rotate it */
					val = ABS(get_angle_to_grid[n2y][n2x] + rotate) % 240;

					/* Get unsigned difference between this angle and centerline */
					diff = ABS(120 - val);

					/* Accept if less than limit */
					if (diff < (degrees + 4) / 3)
					{
						/* And in LOS */
						if (los(y2, x2, y, x))
						{
							gy[grids] = y;
							gx[grids] = x;
							gd[grids] = dist;
							grids++;
						}
					}
				}

				/* Standard ball spell */
				else
				{
					/* Accept all grids in LOS, or all if project_pass */
					if ((flg & (PROJECT_PASS)) || (los(y2, x2, y, x)))
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

		/*
		 * If a particular diameter for the source of the explosion's
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

	/* Display the blast area if allowed (unless a bolt) */
	if (!blind && !(flg & (PROJECT_HIDE)) && ((grids > 1) || (dist == 0) || (flg & (PROJECT_BOOM))))
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
				if (op_ptr->delay_factor) (void)Term_fresh();

				/* Delay (efficiently) */
				if (visual || drawn)
				{
					/* Shorter delay for large explosions */
					m = (rad <= 4 ? msec : (rad <= 8 ? 2*msec/3 : msec/2));
					pause_for(m);
				}
			}
		}

		/* Delay for a while if there are pretty graphics to show */
		if ((grids > 1) && (drawn))
		{
			if (!op_ptr->delay_factor) (void)Term_fresh();
			pause_for(who <= 0 ? msec*2 : msec);
		}
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
		if (op_ptr->delay_factor) (void)Term_fresh();
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

	/* Update stuff if needed */
	if (p_ptr->update) update_stuff();

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
				if (m_ptr->ml >= ML_FULL) monster_race_track(m_ptr->r_idx);

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
