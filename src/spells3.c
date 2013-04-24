/* File: spells3.c */

/* Purpose: Spell code (part 3) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Smash a potion, observe effects
 *
 * Potions smash at full strength if "who" is not zero.  Otherwise their
 * strength is cut by two-thirds.  This prevents a floor with potions
 * becoming quite so much of a minefield.
 *
 * Note the "allow_activate" variable, which prevents the projections of
 * smashed potions smashing other potions.  In steam, chain reactions are possible
 * XXX XXX
 * If "who" is -1, use throwing skill for some things.
 *
 * Return TRUE if we noticed anything.
 *
 * This function needs some heavy editing - will be fixed in version .4
 */
bool item_smash_effect(int who, int y, int x, object_type *o_ptr)
{
	bool do_fire_star = FALSE;

	int rad = 1;
	int typ = 0;
	int dam = 0;
	u32b flg;

	int skill = 0;
	int sides = 0;
	int dice = 0;

	/* various throwing skills */
	int advthrow, masterthrow, powerthrow, accthrow, tohitthrow, critthrow;

	/* combine to produce an item smashing bonus */
	int throwbonus = 0;

	/* Paranoia */
	advthrow = masterthrow = powerthrow = accthrow = tohitthrow = critthrow = 0;
	
	if (p_ptr->skills[SK_TOHIT_THROWING].skill_max > 0)
		tohitthrow = p_ptr->skills[SK_TOHIT_THROWING].skill_rank;
	if (p_ptr->skills[SK_ADV_THROWING].skill_max > 0)
		advthrow = p_ptr->skills[SK_ADV_THROWING].skill_rank;
	if (p_ptr->skills[SK_MASTER_THROWING].skill_max > 0)
		masterthrow = p_ptr->skills[SK_MASTER_THROWING].skill_rank;
	if (p_ptr->skills[SK_POWER_THROW].skill_max > 0)
		powerthrow = p_ptr->skills[SK_POWER_THROW].skill_rank;
	if (p_ptr->skills[SK_CRIT_THROW].skill_max > 0)
		critthrow = p_ptr->skills[SK_CRIT_THROW].skill_rank;
	if (p_ptr->skills[SK_ACC_THROW].skill_max > 0)
		accthrow = p_ptr->skills[SK_ACC_THROW].skill_rank;

	throwbonus = (tohitthrow + advthrow + masterthrow) / 6;
	throwbonus += (powerthrow / 4);
	throwbonus += (critthrow / 2);
	throwbonus += (accthrow / 2);
	
	/* paranoia */
	if (throwbonus < 0) throwbonus = 0;
	if (throwbonus > 35) throwbonus = 35;
		
	/* Analyze the item */
	switch (o_ptr->tval)
	{
		/* ammo and bullets blow up! */
		case TV_AMMO:
		case TV_BULLET:
		{
			/* Only non-magical bullets blow up */
			if ((o_ptr->to_h <= 0)	|| (o_ptr->to_d <= 0))
			{
				/* maybe??? */
				typ = GF_FIRE;
				dam = damroll(o_ptr->dd, o_ptr->ds) / 2;
				rad = 1 + div_round(dam, 100);
			}
			break;
		}
		case TV_FLASK:
		{
			typ = GF_FIRE;
			dam = damroll(o_ptr->dd, o_ptr->ds) / 2;
			rad = 1 + div_round(dam, 10);
			break;
		}		
		case TV_TONIC:
		{
			switch (o_ptr->sval)
			{
				case SV_TONIC_WATER:
				case SV_TONIC_APPLE_JUICE:
				case SV_TONIC_SLIME_MOLD:
				case SV_TONIC_SALT_WATER:
				{
					typ = GF_EMP;
					if (who < 0) dam = 10 + throwbonus;
					else dam = 10;
					break;
				}			
				case SV_TONIC_SLOWNESS:
				{
						typ = GF_SLOW;
						if (who < 0) dam = 50 + throwbonus * 2;
						else dam = 50;
						break;
				}
				case SV_TONIC_POISON:
				{
					typ = GF_POISON;
					if (who < 0)
					{
						 dam = damroll( 7 + (throwbonus / 10), 5 + (throwbonus / 5));
					}
					dam = damroll(7, 5);
					break;
				}
				/* case SV_TONIC_MUTAGEN: */
				case SV_TONIC_BLINDNESS:
				case SV_TONIC_CONFUSION:				
				{
					typ = GF_CONFUSION;
					if (who < 0) dam = 50 + throwbonus * 2;
					else dam = 50;
					break;
				}
				case SV_TONIC_SLEEP:
				{
					typ = GF_SLEEP;
					if (who < 0) dam = 50 + throwbonus * 2;
					else dam = 50;
					break;
				}
				case SV_TONIC_LOSE_MEMORIES:
				{
						typ = GF_FORGET;
						dam = damroll(10, 8);
						break;
				}
				case SV_TONIC_SKILL:
				{
						typ = GF_GAIN_LEVEL;
						dam = 40;
						break;
				}
				case SV_TONIC_DEC_MUS:
				case SV_TONIC_DEC_AGI:
				case SV_TONIC_DEC_VIG:
				case SV_TONIC_DEC_SCH:
				case SV_TONIC_DEC_EGO:
				case SV_TONIC_DEC_CHR:
				{
						typ = GF_CURSE;
						dam = 40;
						break;
				}	
				case SV_TONIC_INFRAVISION:
				case SV_TONIC_DETECT_INVIS:
				case SV_TONIC_SLOW_POISON:
				case SV_TONIC_CURE_POISON:
				case SV_TONIC_BOLDNESS:
				{
						/* Nothing */
						break;
				}
				case SV_TONIC_SPEED:
				{
						typ = GF_SPEED;
						break;
				}	
				case SV_TONIC_HEROISM:
				{
						typ = GF_GAIN_LEVEL;
						dam = 10;
						break;
				}
				case SV_TONIC_BERSERK_STRENGTH:
				{
						typ = GF_GAIN_LEVEL;
						dam = 30;
						break;
				}
				case SV_TONIC_XXX1:
				{
						/* Nothing (!) */
						break;
				}
				case SV_TONIC_CURE_LIGHT_HP:
      	{
      			typ = GF_HEAL;
      			dam = 100;
      			break;
      	}
				case SV_TONIC_CURE_SERIOUS_HP:
      	{
      			typ = GF_HEAL;
      			dam = 200;
      			break;
      	}
				case SV_TONIC_CURE_CRITICAL_HP:
      	{
      			typ = GF_HEAL;
      			dam = 400;
      			break;
      	}
				case SV_TONIC_CURE_MORTAL_HP:
      	{
      			typ = GF_HEAL;
      			dam = 800;
      			break;
      	}
				case SV_TONIC_HP_HEALING:
      	{
      			typ = GF_HEAL;
      			dam = 1000;
      			break;
      	}
				case SV_TONIC_HP_STAR_HEALING:
      	{
      			typ = GF_HEAL;
      			dam = 10000;
      			break;
      	}
					
				case SV_TONIC_CURE_MINOR_SP:
				case SV_TONIC_CURE_LIGHT_SP:
				case SV_TONIC_CURE_SERIOUS_SP:
				case SV_TONIC_CURE_CRITICAL_SP:
				case SV_TONIC_CURE_MORTAL_SP:
				case SV_TONIC_SP_HEALING:
				case SV_TONIC_SP_STAR_HEALING:
				{
						/* Nothing (!) */
						break;
				}
				case SV_TONIC_LIFE:
				{
						typ = GF_GAIN_LEVEL;
						dam = 1000;
						break;
				}
				case SV_TONIC_RESTORE_EXP:
				{
						typ = GF_GAIN_LEVEL;
						dam = 1000;
						break;
				}
				case SV_TONIC_RES_MUS:
				case SV_TONIC_RES_AGI:
				case SV_TONIC_RES_VIG:
				case SV_TONIC_RES_SCH:
				case SV_TONIC_RES_EGO:
				case SV_TONIC_RES_CHR:
				{
						typ = GF_GAIN_LEVEL;
						dam = 5;
						break;
				}
				case SV_TONIC_INC_MUS:
				case SV_TONIC_INC_AGI:
				case SV_TONIC_INC_VIG:
				case SV_TONIC_INC_SCH:
				case SV_TONIC_INC_EGO:
				case SV_TONIC_INC_CHR:
				{
						typ = GF_GAIN_LEVEL;
						dam = 100;
						break;
				}
				case SV_TONIC_AUGMENTATION:
				{
						typ = GF_GAIN_LEVEL;
						dam = 500;
						break;
				}
				case SV_TONIC_ENLIGHTENMENT:
      	{
      			typ = GF_ENLIGHTENMENT;
      			rad = 10;
      			do_fire_star = TRUE;
      			break;
      	}
				case SV_TONIC_STAR_ENLIGHTENMENT:
				{
						typ = GF_ENLIGHTENMENT;
						rad = 18;
						do_fire_star = TRUE;
						break;
				}
				case SV_TONIC_SELF_KNOWLEDGE:
				case SV_TONIC_EXPERIENCE:
				{
						typ = GF_GAIN_LEVEL;
						dam = 500;
						break;
				}
				default:
				{
					/* No effect, no identify */
					return (FALSE);
				}
				break;
			}
			break;
		}
		default:
		{
			/* No effect, no identify MAIN*/
			return (FALSE);
		}
	}	
		
	/* Require a projection type */
	if (!typ) return (FALSE);

	/* Do not smash more potions */
	/* allow_activate = FALSE; */

	/* Jump to target, affect everything */
	flg = PROJECT_BOOM | PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM |
	      PROJECT_KILL | PROJECT_PLAY;

	/* Optionally, fire a starburst */
	if (do_fire_star) flg |= (PROJECT_STAR);


	/* Hack -- potions are especially powerful when used in traps */
	/* if (who == -2) dam += dam / 3; */

	/* Special case -- tone down accidental destruction of potions */
	if (!who) dam /= 3;

	/* Hack -- Allow differing "characters" */
	if (who < -1) who = -1;

	if (p_ptr->wizard )msg_format("rad is %d y is %d x is %d dam is %d", rad, y, x, dam);

	/* Cast the projection, notice effects, reset "allow_activate" */
	return (project(who, rad, y, x, y, x, dam, typ, flg, 0, 0));
}

/* future tool/mechanism non-player device activate function. */

/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
void teleport_away(int m_idx, int dis)
{	
	int ny, nx, oy, ox, d, i, min;

	bool look = TRUE;

	monster_type *m_ptr = &m_list[m_idx];

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return;
	
	/* Paranoia */
	if (!m_ptr->r_idx) return;

	/* Save the old location */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				ny = rand_spread(oy, dis);
				nx = rand_spread(ox, dis);
				d = distance(oy, ox, ny, nx);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(ny, nx)) continue;

			/* Require "empty" floor space */
			if (!cave_empty_bold(ny, nx)) continue;

			/* Hack -- no teleport onto glyph of warding */
			if (cave_feat[ny][nx] == FEAT_GLYPH) continue;

			/* No teleporting into vaults and such */
			if (cave_info[ny][nx] & (CAVE_ICKY)) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
	}

	/* Sound */
	sound(MSG_TPOTHER);

	/* Swap the monsters */
	monster_swap(oy, ox, ny, nx);
}

/*
 * Thrust the player or a monster away from the source of a projection.
 * Used for GF_FORCE only (GF_STORM and GF_GRAVITY blink the player in
 * a random direction).  Also used for impact blows.  -LM-
 *
 * Monsters and players can be pushed past monsters or players weaker than
 * they are.
 */
void thrust_away(int who, int t_y, int t_x, int grids_away)
{
	int y, x, yy, xx;
	int i, d, first_d;
	int angle;

	int c_y, c_x;

	/* Assume a default death */
	cptr note_dies = " dies.";

	/*** Find a suitable endpoint for testing. ***/

	/* Get location of caster (assumes index of caster is not zero) */
	if (who > 0)
	{
		c_y = m_list[who].fy;
		c_x = m_list[who].fx;
	}
	else
	{
		c_y = p_ptr->py;
		c_x = p_ptr->px;
	}

	/* Determine where target is in relation to caster. */
	y = t_y - c_y + 20;
	x = t_x - c_x + 20;

	/* Find the angle (/2) of the line from caster to target. */
	angle = get_angle_to_grid[y][x];

	/* Start at the target grid. */
	y = t_y;
	x = t_x;

	/*
	 * Up to the number of grids requested, force the target away from the
	 * source of the projection, until it hits something it can't travel
	 * around.
	 */
	for (i = 0; i < grids_away; i++)
	{
		/* Randomize initial direction. */
		first_d = rand_int(8);

		/* Look around. */
		for (d = first_d; d < 8 + first_d; d++)
		{
			/* Reject angles more than 44 degrees from line. */
			if (d % 8 == 0)	/* 135 */
			{
				if ((angle > 157) || (angle < 114)) continue;
			}
			if (d % 8 == 1)	/* 45 */
			{
				if ((angle > 66) || (angle < 23)) continue;
			}
			if (d % 8 == 2)	/* 0 */
			{
				if ((angle > 21) && (angle < 159)) continue;
			}
			if (d % 8 == 3)	/* 90 */
			{
				if ((angle > 112) || (angle < 68)) continue;
			}
			if (d % 8 == 4)	/* 158 */
			{
				if ((angle > 179) || (angle < 136)) continue;
			}
			if (d % 8 == 5)	/* 113 */
			{
				if ((angle > 134) || (angle < 91)) continue;
			}
			if (d % 8 == 6)	/* 22 */
			{
				if ((angle > 44) || (angle < 1)) continue;
			}
			if (d % 8 == 7)	/* 67 */
			{
				if ((angle > 89) || (angle < 46)) continue;
			}

			/* Extract adjacent location */
			yy = y + ddy_ddd[d % 8];
			xx = x + ddx_ddd[d % 8];

			/* Cannot switch places with stronger monsters. */
			if (cave_m_idx[yy][xx] != 0)
			{
				/* A monster is trying to pass. */
				if (cave_m_idx[y][x] > 0)
				{
					monster_type *m_ptr = &m_list[cave_m_idx[y][x]];

					/* Monsters cannot pass by stronger monsters. */
					if (cave_m_idx[yy][xx] > 0)
					{
						monster_type *n_ptr = &m_list[cave_m_idx[yy][xx]];

						if (r_info[n_ptr->r_idx].mexp > r_info[m_ptr->r_idx].mexp)
							continue;
					}

					/* Monsters cannot pass by stronger characters. */
					else
					{
						if (p_ptr->lev > r_info[m_ptr->r_idx].level)
							continue;
					}
				}

				/* The player is trying to pass. */
				if (cave_m_idx[y][x] < 0)
				{
					/* Players cannot pass by stronger monsters. */
					if (cave_m_idx[yy][xx] > 0)
					{
						monster_type *n_ptr = &m_list[cave_m_idx[yy][xx]];

						if (r_info[n_ptr->r_idx].level > p_ptr->lev)
							continue;
					}
				}
			}

			/* Check for obstruction. */
			if (!cave_floor_bold(yy, xx))
			{
				/* Some features allow entrance, but not exit. */
				if (cave_passable_bold(yy, xx))
				{
					/* Travel down the path. */
					monster_swap(y, x, yy, xx);

					/* Jump to new location. */
					y = yy;
					x = xx;

					/* We can't travel any more. */
					i = grids_away;

					/* Stop looking. */
					break;
				}

				/* If there are walls everywhere, stop here. */
				else if (d == (8 + first_d - 1))
				{
					/* Message for player. */
					if (cave_m_idx[y][x] < 0)
						msg_print("You come to rest next to a wall.");
					i = grids_away;
				}
			}
			else
			{
				/* Travel down the path. */
				monster_swap(y, x, yy, xx);

				/* Jump to new location. */
				y = yy;
				x = xx;

				/* Stop looking at previous location. */
				break;
			}
		}
	}
#if 0
	/* Some special messages or effects for player. */
	if (cave_m_idx[y][x] < 0)
	{
		if (cave_feat[y][x] == FEAT_TREE)
			msg_print("You come to rest in some trees.");
		if (cave_feat[y][x] == FEAT_RUBBLE)
			msg_print("You come to rest in some rubble.");
		if (cave_feat[y][x] == FEAT_WATER)
			msg_print("You come to rest in a pool of water.");
		if (cave_feat[y][x] == FEAT_LAVA)
		{
			fire_dam(damroll(4, 100), 0, "You are thrown into molten lava!",
				"burnt up in molten lava");
		}
	}

	/* Some monsters don't like lava or water. */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		if (cave_feat[y][x] == FEAT_WATER)
		{
			if (!cave_exist_mon(r_ptr, y, x, TRUE, TRUE))
			{
				note_dies = " is drowned.";

				/* Hurt the monster.  No fear. */
				mon_take_hit(cave_m_idx[y][x], 0,
					damroll(2, 18 + m_ptr->maxhp / 12), FALSE, note_dies);

				/* XXX - If still alive, monster escapes. */
				teleport_away(cave_m_idx[y][x], 3);
			}
		}
		if (cave_feat[y][x] == FEAT_LAVA)
		{
			if (!cave_exist_mon(r_ptr, y, x, TRUE, TRUE))
			{
				note_dies = " is burnt up.";

				/* Hurt the monster.  No fear. */
				mon_take_hit(cave_m_idx[y][x], 0,
					damroll(2, 18 + m_ptr->maxhp / 12), FALSE, note_dies);

				/* XXX - If still alive, monster escapes. */
				teleport_away(cave_m_idx[y][x], 3);
			}
		}
	}
#endif
	/* Encourage the player to blow away monsters */
	if ((who < 0) && (distance(t_y, t_x, y, x) > 5))
	{
		/* Require initial visibility and living monster */
		if ((player_can_see_bold(t_y, t_x)) && (cave_m_idx[y][x] > 0))
		{
			/* Require that monster not be visible now */
			if (!m_list[cave_m_idx[y][x]].ml)
			{
				msg_print("You hear screams fading off into the distance.");
			}
		}
	}

	/* Clear the cave_temp flag (the "project()" code may have set it). */
	cave_info[y][x] &= ~(CAVE_TEMP);
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 */
void teleport_player(int dis)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int d, i, min, y, x;

	bool look = TRUE;

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return;

	/* Sanity check */
	if (dis < 1) dis = 1;

	/* Initialize */
	y = py;
	x = px;

	/* Minimum distance */
	min = dis / 2;

	/* Look until done */
	while (look)
	{
		/* Verify max distance */
		if (dis > 200) dis = 200;

		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "naked" floor space */
			if (!cave_naked_bold(y, x)) continue;

			/* No teleporting into vaults and such */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}

/*
 * Teleport monster to a grid near the given location.  This function is
 * used in the monster spell "TELE_SELF_TO", to allow monsters both to
 * suddenly jump near the character, and to make them "dance" around the
 * character.
 *
 * Usually, monster will teleport to a grid that is not more than 4
 * squares away from the given location, and not adjacent to the given
 * location.  These restrictions are relaxed if necessary.
 *
 * This function allows teleporting into vaults.
 */
void teleport_towards(int oy, int ox, int ny, int nx, bool charge)
{
	int y, x;

	int dist;
	int ctr = 0;
	int min = 2, max = 4;

	/* teleport field only affects teleporting */
	if (!charge)
	{
			/* Anti-teleport field */
			if (p_ptr->no_teleport) return;
	}
	/* Find a usable location */
	while (TRUE)
	{
		/* Pick a nearby legal location */
		while (TRUE)
		{
			y = rand_spread(ny, max);
			x = rand_spread(nx, max);
			if (in_bounds_fully(y, x)) break;
		}

		/* Consider all unoccupied, passable grids  XXX */
		if ((cave_passable_bold(y, x)) && (cave_m_idx[y][x] == 0))
		{
			/* Calculate distance between target and current grid */
			dist = distance(ny, nx, y, x);

			/* If we're charging, teleport right next to the player */
			if (charge) 
			{
				/* Accept grids that are the right distance away */
				if (dist < 2) break;
			}
			else
			{
 				/* Accept grids that are the right distance away. */
				if ((dist >= min) && (dist <= max)) break;
			}
		}

		/* Occasionally relax the constraints */
		if ((++ctr > 15) && (!charge))
		{
			ctr = 0;

			max++;
			if (max > 5) min = 0;
		}
	}

	/* Sound (assumes monster is moving) */
	if (!charge) sound(SOUND_TPOTHER);

	/* Move monster */
	monster_swap(oy, ox, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}


/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(int ny, int nx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;

	int dis = 0, ctr = 0;

	/* Initialize */
	y = py;
	x = px;

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return;

	/* Find a usable location */
	while (1)
	{
		/* Pick a nearby legal location */
		while (1)
		{
			y = rand_spread(ny, dis);
			x = rand_spread(nx, dis);
			if (in_bounds_fully(y, x)) break;
		}

		/* Accept "naked" floor grids */
		if (cave_naked_bold(y, x)) break;

		/* Occasionally advance the distance */
		if (++ctr > (4 * dis * dis + 4 * dis + 1))
		{
			ctr = 0;
			dis++;
		}
	}

	/* Sound */
	sound(MSG_TELEPORT);

	/* Move player */
	monster_swap(py, px, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}



/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(bool voluntary)
{
	byte kind_of_quest = quest_check(p_ptr->depth);

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return;

	if (adult_ironman)
	{
		msg_print("Nothing happens.");
		return;
	}


	if (!p_ptr->depth)
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth++;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else if ((kind_of_quest == QUEST_FIXED) ||
			 (kind_of_quest == QUEST_FIXED_U) || 
			 (p_ptr->depth >= MAX_DEPTH-1) || 
			 (p_ptr->wonderland))
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	/*Not fair to fail the quest if the monster teleports player off*/
	else if ((!(voluntary)) &&
		((kind_of_quest == QUEST_GUILD) ||
		 (kind_of_quest == QUEST_UNIQUE) ||
		 (kind_of_quest == QUEST_VAULT)))
	{
		/*de-activate the quest*/
		p_ptr->cur_quest = 0;

		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else if (rand_int(100) < 50)
	{
		message(MSG_TPLEVEL, 0, "You sink through the floor.");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else
	{
		message(MSG_TPLEVEL, 0, "You rise up through the ceiling.");

		/* New depth */
		p_ptr->depth++;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
}


/*
 * Switch position with a monster.
 */
bool teleport_swap(int dir)
{
	int tx, ty;

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return (FALSE);

	if ((dir == 5) && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;
	}
	else
	{
		tx = p_ptr->px + ddx[dir];
		ty = p_ptr->py + ddy[dir];
	}

	if (cave_m_idx[ty][tx] < 1)
	{
		msg_print("You can't trade places with that!");
		/* Failure */
		return FALSE;
	}

	/* Need something better than player level */
	if ((cave_info[ty][tx] & (CAVE_ICKY)) || (distance(ty, tx, p_ptr->py, p_ptr->px) > p_ptr->lev * 3 / 2 + 10))
	{
		msg_print("Failed to swap.");
		/* Failure */
		return FALSE;
	}
	
	monster_swap(p_ptr->py, p_ptr->px, ty, tx);

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	/* Success */
	return TRUE;
}

/*
 * Teleport monster to a grid VERY near the given location.  This function is
 * used to let the monsters jump very near the player - it is only used by 
 * the player to call a monster near his or her location.
 * 
 *
 * Usually, monster will teleport to a grid that is not more than 1
 * squares away from the given location, and is adjacent to the given
 * location.  These restrictions are relaxed if necessary.
 *
 * This function allows teleporting into vaults.
 */
void teleport_to_player(int oy, int ox, int ny, int nx)
{
	int y, x;

	int dist;
	int ctr = 0;
	int min = 0, max = 1;

	/* Anti-teleport field */
	if (p_ptr->no_teleport) return;

	/* Find a usable location */
	while (TRUE)
	{
		/* Pick a nearby legal location */
		while (TRUE)
		{
			y = rand_spread(ny, max);
			x = rand_spread(nx, max);
			if (in_bounds_fully(y, x)) break;
		}

		/* Consider all unoccupied, passable grids  XXX */
		if ((cave_passable_bold(y, x)) && (cave_m_idx[y][x] == 0))
		{
			/* Calculate distance between target and current grid */
			dist = distance(ny, nx, y, x);

			/* Accept grids that are the right distance away. */
			if ((dist >= min) && (dist <= max)) break;
		}

		/* Occasionally relax the constraints */
		if (++ctr > 15)
		{
			ctr = 0;

			max++;
			if (max > 5) min = 0;
		}
	}

	/* Sound (assumes monster is moving) */
	sound(SOUND_TPOTHER);

	/* Move monster */
	monster_swap(oy, ox, y, x);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();
}

/* Combine with teleport player level XCCCX */
void steam_mecha_drill_level(void)

{
	byte kind_of_quest = quest_check(p_ptr->depth);

	if (adult_ironman)
	{
		msg_print("Nothing happens.");
		return;
	}


	if (!p_ptr->depth)
	{
		message(MSG_TPLEVEL, 0, "You drill up through the ceiling.");

		/* New depth */
		p_ptr->depth++;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else if ((kind_of_quest == QUEST_FIXED) ||
			 (kind_of_quest == QUEST_FIXED_U) ||
			 (p_ptr->depth >= MAX_DEPTH-1) || (p_ptr->wonderland))
	{
		message(MSG_TPLEVEL, 0, "You drill down through the floor!");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else if (rand_int(100) < 10)
	{
		message(MSG_TPLEVEL, 0, "You drill down through the floor!");

		/* New depth */
		p_ptr->depth--;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}

	else
	{
		message(MSG_TPLEVEL, 0, "You drill up through the ceiling.");

		/* New depth */
		p_ptr->depth++;

		/* Leaving */
		p_ptr->leaving = TRUE;
	}
}
/*
 * Increase a stat by one randomized level
 *
 * Most code will "restore" a stat before calling this function,
 * in particular, stat tonics will always restore the stat and
 * then increase the fully restored value.
 */
bool inc_stat(int stat)
{
	int value, gain;
	int raceside, classside; 
 	int totalside;
 	bool topout = FALSE;

	/* Then augment the current/max stat */
	value = p_ptr->stat_max[stat];

	if (p_ptr->stat_cur[stat] == p_ptr->stat_max[stat]) topout = TRUE;
	
	if (value < 700)
	{
	 	/* assign the first number(racial dice sides) to raceside */
	 	raceside = rp_ptr->r_adj[stat];
	 		
	 	/* assign the second number (class die sides) to classside */
	 	classside = cp_ptr->c_adj[stat];
	 		
	 	/* total them */
	 	totalside = raceside + classside;	

		if ((p_ptr->skills[SK_FIRE_LORE].skill_rank > 1) && stat == A_MUS)
			totalside += (p_ptr->skills[SK_FIRE_LORE].skill_rank / 4);
		if ((p_ptr->skills[SK_WIND_LORE].skill_rank > 1) && stat == A_AGI)
			totalside += (p_ptr->skills[SK_WIND_LORE].skill_rank / 4);
		if ((p_ptr->skills[SK_EARTH_LORE].skill_rank > 1) && stat == A_VIG)
			totalside += (p_ptr->skills[SK_EARTH_LORE].skill_rank / 4);
		if ((p_ptr->skills[SK_WATER_LORE].skill_rank > 1) && stat < 3)
			totalside += (p_ptr->skills[SK_WATER_LORE].skill_rank / 5);
		if ((p_ptr->skills[SK_DRAGON_HEART].skill_rank > 1) && stat < 3)
			totalside += (p_ptr->skills[SK_DRAGON_HEART].skill_rank / 4);
		
		/* assign a random value of the sides to gain */
		/* possibly give chance for higher bonuses (tied in with a skill?)*/
		gain = (randint(totalside) + randint(totalside))  / 2;
		
		/* raise the value by that much */
		value += gain;
		
		if (value > 700)
		{
			value = 700;
		}	
		
		/* Save the new value */
		p_ptr->stat_max[stat] = value;
	
		/* Bring up the maximum too */
		if (topout)
		{
			p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];
		}
	
	
		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	
		/* Success */
		return (TRUE);	
	}
	
	/* Nothing to gain */
	return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
 /* also have to make sure that I correct this for the new system -CCC */
bool dec_stat(int stat, int amount, int permanent)
{
	int cur, max, same, res = FALSE;
	int statdam;


	/* Get the current value */
	cur = p_ptr->stat_cur[stat];
	max = p_ptr->stat_max[stat];

	/* Note when the values are identical */
	same = (cur == max);

	/* Damage "current" value */
	if (!permanent)
	{
		statdam = randint(amount);
		cur -= statdam;
		
		if (cur < 1) cur = 1;
		
		/* Something happened */
		if (cur != p_ptr->stat_cur[stat]) res = TRUE;
	}

	/* Damage "max" value */
	if (permanent)
	{
		statdam = randint(amount);
		max -= statdam;
		if (max < 1) max = 1;
		
		/* Hack -- keep it clean */
		if (same || (max < cur)) max = cur;

		/* Something happened */
		if (max != p_ptr->stat_max[stat]) res = TRUE;
	}

	/* Apply changes */
	if (res)
	{
		/* Actually set the stat to its new value. */
		p_ptr->stat_cur[stat] = cur;
		p_ptr->stat_max[stat] = max;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	}

	/* Done */
	return (res);
}


/*
 * Restore a stat.  Return TRUE only if this actually makes a difference.
 */
bool res_stat(int stat)
{
	/* Restore if needed */
	if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
	{
		/* Restore */
		p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Success */
		return (TRUE);
	}

	/* Nothing to restore */
	return (FALSE);
}

/*
 * Turns an object into gold, gain some of its value in a shop
 */
bool alchemy(void)
{
	int item, amt = 1;
	int old_number;
	long price;
	bool force = FALSE;
	object_type *o_ptr;
	char o_name[256];
	char out_val[512];

	cptr q, s;

	/* Hack -- force destruction */
	if (p_ptr->command_arg > 0) force = TRUE;

	/* Get an item */
	q = "Turn which item to gold? ";
	s = "You have nothing to turn to gold.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return FALSE;
	}


	/* Describe the object */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	object_desc(o_name, o_ptr, TRUE, 3);
	o_ptr->number = old_number;

	/* Verify unless quantity given */
	if (!force)
	{
		if (object_value(o_ptr) < 1)
		{
			/* Make a verification */
			sprintf(out_val, "Really turn %s to gold? ", o_name);
			if (!get_check(out_val)) return FALSE;
		}
	}
	
	/* Artifacts cannot be converted to gold */
	if (artifact_p(o_ptr))
	{
		/* Message */
		msg_format("You cannot turn %s into gold!", o_name);

		/* Don't mark id'ed objects */
		if (object_known_p(o_ptr)) return (FALSE);

		/* It has already been sensed */
		if (o_ptr->ident & (IDENT_SENSE))
		{
			/* Already sensed objects always get improved feelings */
			if (cursed_p(o_ptr))
				o_ptr->discount = INSCRIP_TWISTED;
			else if (broken_p(o_ptr))
				o_ptr->discount = INSCRIP_SHATTERED;
			else
				o_ptr->discount = INSCRIP_SPECIAL;
		}
		else
		{
			/* Mark the object as indestructible */
			o_ptr->discount = INSCRIP_INDESTRUCTIBLE;
		}

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return (FALSE);
	}

	price = object_value(o_ptr);

	if (price <= 0)
	{
		/* Message */
		msg_format("You turn %s to fool's gold.", o_name);
	}
	else
	{
		price /= 3;

		if (amt > 1) price *= amt;

		if (price > 30000) price = 30000;
		msg_format("You turn %s to %ld coins worth of gold.", o_name, price);
		p_ptr->au += price;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}

	/* Eliminate the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -amt);
		inven_item_describe(item);
		inven_item_optimize(item);
	}

	/* Eliminate the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -amt);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	return TRUE;
}

/* have your wounds change. . . Possibly for the worse */

void do_poly_wounds(void)
{
	s16b wounds = p_ptr->cut;
	s16b hit_p = (p_ptr->mhp - p_ptr->chp);
	s16b change = damroll(p_ptr->depth, 5);
	bool Nasty_effect = (randint(5)==1);

	if (!(wounds || hit_p || Nasty_effect)) return;

	if (Nasty_effect)
	{
		msg_print("A new wound was created!");
		take_hit(change, "a polymorphed wound", TRUE);
		set_cut(change);
	}
	else
	{
		msg_print("Your wounds are polymorphed into less serious ones.");
		hp_player(change);
		set_cut((p_ptr->cut)-(change/2));
	}
}

/*
 * Fetch an item (teleport it right underneath the caster)
 * I'll have to figure this out later, at least now it's
 * not 'undeclared' Fixed. This cave info stuff sucks!
 * I can't figure out why it's different in v from _all_
 * the other variants -ccc
 */
void fetch(int dir, int wgt, bool require_los)
{
	int             ty, tx, i;
 	bool            flag;
 	object_type     *o_ptr;
 	char            o_name[80];
 	int py = 			p_ptr->py;
 	int px = 			p_ptr->px;
 
	/* Check to see if you're on an empty floor square */
	if(!cave_clean_bold(py, px))
	{
		msg_print("You need an empty space on the floor to fetch something.");
		return;
	}

	/* Use a target */
	if(dir==5 && target_okay())
	{
		tx = p_ptr->target_col;
		ty = p_ptr->target_row;

		if(distance(py, px, ty, tx)>MAX_RANGE)
		{
			msg_print("You can't fetch something that far away!");
			return;
		}

		if (cave_o_idx[ty][tx] == 0)
		{
			msg_print("There's nothing there to fetch!");
			return;
		}

		if (require_los && (!player_has_los_bold(ty,tx)))
		{
			msg_print("You have no direct line of sight to that location.");
			return;
		}

		if (cave_info[ty][tx] & (CAVE_ICKY))
		{
			msg_print("The object refuses to budge!");
			return;
		}
	}
	else
	{
		/* Use a direction */
		ty = py; /* Where to drop the item */
		tx = px;
		flag = FALSE;

		do
		{
			ty += ddy[dir];
			tx += ddx[dir];

			if ((distance(py, px, ty, tx)> MAX_RANGE)
			    || !cave_floor_bold(ty, tx)) return;
		}
		while(cave_o_idx[py][px] == 0);
	}

	o_ptr = &o_list[(cave_o_idx[ty][tx])];

	if (o_ptr->weight > wgt)
	{
		/* Too heavy to 'fetch' */
		msg_print("The object is too heavy.");
		return;
	}

	i = cave_o_idx[ty][tx];
	cave_o_idx[ty][tx] = 0;
	cave_o_idx[py][px] = i; /* 'move' it */
	o_ptr->iy = py;
	o_ptr->ix = px;

	object_desc(o_name, o_ptr, TRUE, 0);
	msg_format("%^s appears at your feet.", o_name);

	note_spot(py,px);
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_OVERHEAD);

}

void do_poly_self(void)
{
	int effects = randint(2);
	int tmp = 0;
	int more_effects = TRUE;

	msg_print("You feel a change coming over you...");

	while (effects-- && more_effects)
	{
		switch (randint(15))
		{
			case 1: case 2:
				do_cmd_rerate();
				break;
			case 3: /* Lose all mutations -- Gumby */
#if 0
				if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3 ||
					p_ptr->muta4 || p_ptr->muta5 || p_ptr->muta6)
				{
					msg_print("All of your lovely mutations go away!");
					p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
					p_ptr->muta4 = p_ptr->muta5 = p_ptr->muta6 = 0;
					p_ptr->update |= PU_BONUS;
					handle_stuff();
				}
				else
				{
					msg_print("You feel as if you almost lost something...");
				}
#endif
				break;
			case 4: case 5:
				do_poly_wounds();
				break;
			case 6: case 7: case 8:
				(void) gain_random_mutation(0);
				break;
			case 9: case 10: case 11:
				lose_mutation(0);
				break;
			case 12: /* Purposedly "leaks" into default */
				msg_print("You polymorph into an abomination!");
				while (tmp < 6)
				{
					(void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
					tmp++;
				}

				if (randint(6)==1)
				{
					msg_print("You find living difficult in your present form!");
					take_hit(damroll(randint(p_ptr->lev),p_ptr->lev), "a lethal mutation", TRUE);
				}
				/* No break; here! */
			default:
			{
				/* nothing */
			#if 0
				mutate_player();
			#endif
			}		
		}
	}
}

void mutate_player(void)
{
	
#if 0
	int max1, cur1, max2, cur2, ii, jj;

	/* Pick a pair of stats */
	ii = rand_int(6);
	for (jj = ii; jj == ii; jj = rand_int(6)) /* loop */;

	max1 = p_ptr->stat_max[ii];
	cur1 = p_ptr->stat_cur[ii];
	max2 = p_ptr->stat_max[jj];
	cur2 = p_ptr->stat_cur[jj];

	p_ptr->stat_max[ii] = max2;
	p_ptr->stat_cur[ii] = cur2;
	p_ptr->stat_max[jj] = max1;
	p_ptr->stat_cur[jj] = cur1;

	p_ptr->update |= (PU_BONUS);
	#endif
}

void do_cmd_rerate(void)
{
	int min_value, max_value, i, percent;

	min_value = (PY_MAX_LEVEL * 3 * (p_ptr->hitdie - 1)) / 8;
	min_value += PY_MAX_LEVEL;

	max_value = (PY_MAX_LEVEL * 5 * (p_ptr->hitdie - 1)) / 8;
	max_value += PY_MAX_LEVEL;

	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Rerate */
	while (1)
	{
		/* Collect values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] += p_ptr->player_hp[i - 1];
		}

		/* Legal values */
		if ((p_ptr->player_hp[PY_MAX_LEVEL - 1] >= min_value) &&
		    (p_ptr->player_hp[PY_MAX_LEVEL - 1] <= max_value)) break;
	}

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
	                (p_ptr->hitdie + ((PY_MAX_LEVEL - 1) * p_ptr->hitdie)));

	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	msg_format("You feel your life force changing!");
}

/*
 * Enchant some (non-magical) bolts
 */
void brand_bolts(void)
{
	int item;
	object_type *o_ptr;
	cptr q, s;


	/* Restrict choices to bolts */
	item_tester_tval = TV_SHOT;

	/* Get an item */
	q = "Enchant which bolts? ";
	s = "You have no bolts to brand.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/*
	 * Don't enchant artifacts, ego-items, cursed or broken items
	 */
	if (artifact_p(o_ptr) || ego_item_p(o_ptr) ||
	    cursed_p(o_ptr) || broken_p(o_ptr))
	{
		/* Flush */
		if (flush_failure) flush();

		/* Fail */
		msg_print("The fiery enchantment failed.");

	}

	/* Message */
	msg_print("Your bolts are covered in a fiery aura!");

	/* Ego-item, set's to evil, not to fire, must correct later.*/
	o_ptr->name2 = 402;

	/* Enchant */
	enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

}

/*
 * Brand the current weapon
 */
void brand_weapon(void)
{
	object_type *o_ptr;
	int i = randint(100);

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
	    (!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act;

		char o_name[80];

		if (i < 20)
		{
			act = "is covered in a fiery shield!";
			/* Note the hardcoding due to bad design. I should fix */
			/* this as some point */
			if ((o_ptr->tval == TV_SWORD) ||
				((o_ptr->tval == TV_DAGGER)) ||
				((o_ptr->tval == TV_AXES))) o_ptr->name2 = 159;
			else o_ptr->name2 = 203;
		}
		else if (i < 40)
		{
			act = "is covered in dripping acid!";
			/* Note the hardcoding due to bad design. I should fix */
			/* this as some point */
			if ((o_ptr->tval == TV_SWORD) ||
				((o_ptr->tval == TV_DAGGER)) ||
				((o_ptr->tval == TV_AXES))) o_ptr->name2 = 157;
			else o_ptr->name2 = 208;
		}
		else if (i < 60)
		{
			act = "starts crackling with electricty!";
			/* Note the hardcoding due to bad design. I should fix */
			/* this as some point */
			if ((o_ptr->tval == TV_SWORD) ||
				((o_ptr->tval == TV_DAGGER)) ||
				((o_ptr->tval == TV_AXES))) o_ptr->name2 = 158;
			else o_ptr->name2 = 204;
		}
		else if (i < 80)
		{
			act = "begins dripping poison!";
			/* Note the hardcoding due to bad design. I should fix */
			/* this as some point */
			if ((o_ptr->tval == TV_SWORD) ||
				((o_ptr->tval == TV_DAGGER)) ||
				((o_ptr->tval == TV_AXES))) o_ptr->name2 = 161;
			else o_ptr->name2 = 206;
		}
		else
		{
			act = "glows deep, icy blue!";
			/* Note the hardcoding due to bad design. I should fix */
			/* this as some point */
			if ((o_ptr->tval == TV_SWORD) ||
				((o_ptr->tval == TV_DAGGER)) ||
				((o_ptr->tval == TV_AXES))) o_ptr->name2 = 160;
			else o_ptr->name2 = 207;
		}

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	}
}

/*
 * Controlled teleportation.  
 * Idea from PsiAngband, through Zangband, Oangband and finally Eyangband to here.
 *
 * I should really expand this function to be optionally limited to los.
 * Should also allow the function call to pass the message text for different paradigms.
 */
void dimen_door(int dis, int fail)
{
	int ny, nx;
	bool okay;
	bool old_expand_look = expand_look;
	s16b old_target_set = p_ptr->target_set;
	s16b old_target_who = p_ptr->target_who;
	s16b old_target_row = p_ptr->target_row; 
	s16b old_target_col = p_ptr->target_col;

	expand_look = TRUE;
	okay = target_set_interactive(TARGET_FREE);
	expand_look = old_expand_look;
	if (!okay) return;

	/* grab the target coords. */
	ny = p_ptr->target_row;
	nx = p_ptr->target_col;

	/* Hack - return target to old values */
	p_ptr->target_set = old_target_set;
	p_ptr->target_col = old_target_col;
	p_ptr->target_row = old_target_row;
	p_ptr->target_who = old_target_who;

	/* Test for empty floor, forbid vaults or too large a
	 * distance, and insure that this spell is never certain.
	 */
	if (!cave_empty_bold(ny,nx) || (cave_info[ny][nx] & CAVE_ICKY) ||
		(distance(ny,nx,p_ptr->py,p_ptr->px) > dis) || 
		(rand_int(100) < fail))
	{
		message(MSG_GENERIC, 0, "You fail to exit the astral plane correctly!");
		p_ptr->energy -= 50;
		if (dis > 5) teleport_player(dis - 5);
	}

	/* Controlled teleport. */
	else teleport_player_to(ny,nx);
}

