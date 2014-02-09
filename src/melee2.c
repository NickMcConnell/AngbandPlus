/* File: melee2.c */

/*
 * Copyright (c) 2001 Leon Marrick & Bahman Rabii, Ben Harrison,
 * James E. Wilson, Robert A. Koeneke, Jeff Greene, Diego Gonzalez
 *
 * Additional code and concepts by David Reeve Sward, Keldon Jones,
 * and others.
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Terrified monsters will turn to fight if they are slower than the
 * character, and closer to him than this distance.
 */
#define TURN_RANGE      3



/*
 * Calculate minimum and desired combat ranges.  -BR-
 */
static void find_range(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

	/* All "afraid" monsters will run away */
	if ((m_ptr->m_timed[MON_TMD_FEAR]) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0))
	{
		m_ptr->min_range = FLEE_RANGE;
	}

	/* Some monsters run when low on mana */
	else if ((r_ptr->flags2 & (RF2_LOW_MANA_RUN)) &&
	    (m_ptr->mana < r_ptr->mana / 6)) m_ptr->min_range = FLEE_RANGE;

	/* Hack -- townsmen go about their business */
	else if (m_ptr->mflag & (MFLAG_TOWN)) m_ptr->min_range = 1;

	/*stupid monsters always charge*/
	else if (r_ptr->flags2 & (RF2_STUPID)) m_ptr->min_range = 1;

	/* Breeders cannot be terrified */
	else if (r_ptr->flags2 & (RF2_MULTIPLY)) m_ptr->min_range = 1;

	/* Allow monster terror */
	else
	{
		/* Minimum distance - stay at least this far if possible */
		m_ptr->min_range = 1;

		/* Examine player power  */
		p_lev = p_ptr->lev;

		/* Examine monster power (level plus morale) */
		m_lev = r_ptr->level +
			(cave_m_idx[m_ptr->fy][m_ptr->fx] % 10) + 25;

		/* Optimize extreme cases below */
		if (m_lev < p_lev + 4) m_ptr->min_range = FLEE_RANGE;
		else if (m_lev + 3 < p_lev)
		{
			/* Examine player health */
			p_chp = p_ptr->chp;
			p_mhp = p_ptr->mhp;

			/* Examine monster health */
			m_chp = m_ptr->hp;
			m_mhp = m_ptr->maxhp;

			/* Prepare to optimize the calculation */
			p_val = (p_lev * p_mhp) + (p_chp << 2);	/* div p_mhp */
			m_val = (m_lev * m_mhp) + (m_chp << 2);	/* div m_mhp */

			/* Strong players scare strong monsters */
			if (p_val * m_mhp > m_val * p_mhp)
				m_ptr->min_range = FLEE_RANGE;
		}
	}

	/* Handle range greater than FLEE Range */
	if (m_ptr->min_range > FLEE_RANGE) m_ptr->min_range = FLEE_RANGE;

	/* Nearby monsters that cannot run away will stand and fight */
	if ((m_ptr->cdis < TURN_RANGE) && (m_ptr->m_speed < p_ptr->state.p_speed))
		m_ptr->min_range = 1;

	/* Now find preferred range */
	m_ptr->best_range = m_ptr->min_range;

	if (r_ptr->freq_ranged > 24)
	{
		/* Heavy spell casters will sit back and cast */
		if (m_ptr->mana > r_ptr->mana / 5) m_ptr->best_range = 6;

		/* Creatures that don't move never like to get too close */
		else if (r_ptr->flags1 & (RF1_NEVER_MOVE)) m_ptr->best_range = 6;

		/* Spellcasters that don't strike never like to get too close */
		else if (r_ptr->flags1 & (RF1_NEVER_BLOW)) m_ptr->best_range = 8;

		/*Monsters who have had unfair attacks happen to them charge or cast */
		else if (m_ptr->mflag & (MFLAG_ATTACKED_BAD))
		{
			m_ptr->min_range = 1;
		}

		/* Breathers like point blank range */
		else if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
		     (r_ptr->flags5 & (RF5_BREATH_MASK)) ||
		     (r_ptr->flags6 & (RF6_BREATH_MASK)) ||
		     (r_ptr->flags7 & (RF7_BREATH_MASK))) &&
		    (m_ptr->best_range < 6) &&
		    (m_ptr->hp > m_ptr->maxhp / 2))
		{
			m_ptr->best_range = 7;
		}
	}

}

static void find_best_flow(monster_type *m_ptr)
{
	int lowest_cost = BASE_FLOW_MAX;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int y = m_ptr->fy;
	int x = m_ptr->fx;

	bool allow_doors = TRUE;

	/* Remember if the monster cannot handle doors */
	if (MONSTER_HATES_DOORS(r_ptr)) allow_doors = FALSE;

	/*Can the monster pass through walls?*/
	if (r_ptr->flags2 & (RF2_KILL_WALL | RF2_PASS_WALL))
	{
		/*Is there a flow here?*/
		if (cave_cost[FLOW_PASS_WALLS][y][x])
		{
			/*
			 * Is it the best route?
			 */
			if (cave_cost[FLOW_PASS_WALLS][y][x] <= lowest_cost)
			{

				lowest_cost = cave_cost[FLOW_PASS_WALLS][y][x];

				/*Mark which flow we are using */
				m_ptr->using_flow = FLOW_PASS_WALLS;

			}
		}

		/*Why didn't we use this flow?*/
		if(m_ptr->using_flow != FLOW_PASS_WALLS)
		{
			/*The flow is not active*/
			if (cost_at_center[FLOW_PASS_WALLS] == 0)
			{
				/* Full update of the flows, which should activate the FLOW_PASS_WALLS */
				p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
			}
		}
	}

	/*Can the monster fly?*/
	if (r_ptr->flags3 & (RF3_FLYING))
	{
		int flow;

		/* Pick the proper version of the FLYING flow */
		if (allow_doors) flow = FLOW_FLYING;
		else flow = FLOW_FLYING_NO_DOORS;

		/*Is there a flow here?*/
		if (cave_cost[flow][y][x])
		{
			/*
			 * Is it the best route?
			 */
			if (cave_cost[flow][y][x] <= lowest_cost)
			{
				lowest_cost = cave_cost[flow][y][x];

				/*Mark which flow we are using */
				m_ptr->using_flow = flow;

			}
		}

		/* Why wouldn't a flying creature use this flow, unless they are using the FLOW_PASS_WALLS flow. */
		if((m_ptr->using_flow != flow) && (m_ptr->using_flow != FLOW_PASS_WALLS))
		{
			/*The flow is not active*/
			if (cost_at_center[flow] == 0)
			{
				/* Full update of the flows, which should activate the FLOW_FLYING */
				p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
			}
		}

	}

	/*
	 * Can the monster pass doors?
	 */
	if (r_ptr->flags2 & (RF2_OPEN_DOOR | RF2_BASH_DOOR))
	{
		/*Is the best flow*/
		if (cave_cost[FLOW_PASS_DOORS][y][x])
		{
			if (cave_cost[FLOW_PASS_DOORS][y][x] < lowest_cost)
			{

				lowest_cost = cave_cost[FLOW_PASS_DOORS][y][x];

				/*Mark which flow we are using */
				m_ptr->using_flow = FLOW_PASS_DOORS;
			}
		}
	}

	/*FLOW_NO_DOORS will never be quicker than FLOW_PASS_DOORS.*/
	else if (cave_cost[FLOW_NO_DOORS][y][x])
	{
		/*Is it the best route?*/
		if (cave_cost[FLOW_NO_DOORS][y][x] <= lowest_cost)
		{
			lowest_cost = cave_cost[FLOW_NO_DOORS][y][x];

			/*Mark which flow we are using */
			m_ptr->using_flow = FLOW_NO_DOORS;
		}
	}

	/*Is the monster native to any terrains?*/
	if (r_ptr->r_native & (TERRAIN_MASK))
	{
		int j;

		u32b which_elem_flow = 1;

		int base, tail;

		/* Pick the proper versions of the elemental flows */
		if (allow_doors)
		{
			base = ELEM_FLOW_BASE;
			tail = ELEM_FLOW_TAIL;
		}
		/* Doorless flows */
		else
		{
			base = ELEM_FLOW_BASE_NO_DOORS;
			tail = ELEM_FLOW_TAIL_NO_DOORS;
		}

		for (j = base; j <= tail; j++, which_elem_flow <<= 1)
		{
			/*Is the monster native to this terrain*/
			if (r_ptr->r_native & which_elem_flow)
			{
				/*Is there a flow here?*/
				if (cave_cost[j][y][x])
				{
					/*Is it the best route?*/
					if (cave_cost[j][y][x] < lowest_cost)
					{
						/*Mark which flow we are using */
						m_ptr->using_flow = j;

						lowest_cost = cave_cost[j][y][x];
					}
				}
			}
		}
	}

	/*Monster is flying*/
	if ((m_ptr->using_flow == FLOW_FLYING) ||
		(m_ptr->using_flow == FLOW_FLYING_NO_DOORS)) m_ptr->mflag |= (MFLAG_FLYING);

	/* Flying monster is standing on dangerous terrain. Make it fly */
	else if ((r_ptr->flags3 & (RF3_FLYING)) &&
				!cave_no_dam_for_mon(y, x, r_ptr)) m_ptr->mflag |= (MFLAG_FLYING);

	/*Not flying*/
	else m_ptr->mflag &= ~(MFLAG_FLYING);
}

/*
 * Check the effect of the Rogue's monster trap.  Certain traps may be avoided by
 * certain monsters.  Traps may be disarmed by smart monsters.
 * If the trap works, the effects vary depending on trap type. -BR-
 *
 * "death" tells the calling function if the monster is killed by the trap.
 *
 * Note that the variables y and x can only be used for a real trap.
 * In MODE_DESCRIBE - monster type will be undefined
 */
void apply_monster_trap(int f_idx, int y, int x, byte mode)
{
	monster_type *m_ptr = NULL;
	monster_race *r_ptr = NULL;
	monster_lore *l_ptr = NULL;
	feature_lore *f_l_ptr = &f_l_list[f_idx];

	int dis_chance = 0, trap_skill = 0;

	/* Assume monster not frightened by trap */
	bool fear = FALSE;

	/*extra damage if monster fails to disarm trap*/
	bool fail_disarm = FALSE;

	/* Assume the trap works */
	bool trap_hit = TRUE;

	/* Assume trap is not destroyed */
	bool trap_destroyed = FALSE;

	char m_name[80];

	int m_idx = 0;

	/* Sanity check */
	if (!cave_monster_trap_bold(y,x) && (mode == MODE_ACTION)) return;

	if (mode == MODE_ACTION)
	{

		m_ptr = &mon_list[cave_m_idx[y][x]];
		r_ptr = &r_info[m_ptr->r_idx];
		l_ptr = &l_list[m_ptr->r_idx];
		m_idx = get_mon_idx(m_ptr);
	}

	/*Count in the feature lore the number of times set off*/
	if ((mode == MODE_ACTION) && (f_l_ptr->f_l_power < MAX_UCHAR))
	{
		f_l_ptr->f_l_power++;
	}
	/* Don't describe if not set off*/
	if ((mode == MODE_DESCRIBE) && (!f_l_ptr->f_l_power))
	{
		text_out("  The effects of this trap are unknown.");
		return;
	}

	/*Skip all this unless we are setting off a trap*/
	if (mode == MODE_ACTION)
	{

		/* Get "the monster" or "it" */
		monster_desc(m_name, sizeof(m_name),m_ptr, 0);

		/* Evasive monsters can usually avoid traps entirely. */
		if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!one_in_(3)))
		{
			if (m_ptr->ml)
			{
				/* Message */
				msg_format("%^s dodges your trap.", m_name);

				/* Note that monster is evasive */
				l_ptr->r_l_flags2 |= (RF2_EVASIVE);
			}

			return;
		}

		/* Lightning traps affect all but ghosts */
		if (f_idx == FEAT_MTRAP_ELEC)
		{
			if (r_ptr->flags2 & (RF2_PASS_WALL))
			{
				if (m_ptr->ml) msg_format("%^s passes through your trap.", m_name);
				trap_hit = FALSE;
			}
		}

		/* Other traps seldom affect ghosts. */
		else if ((r_ptr->flags2 & (RF2_PASS_WALL)) &&
	          (rand_int(4) != 1) && (mode == MODE_ACTION))
		{
			if (m_ptr->ml) msg_format("%^s passes through your trap.", m_name);
			trap_hit = FALSE;
		}

		/* Find the monsters base skill at disarming */
		dis_chance = 40 + (2 * r_ptr->level);

		/*Hack - these traps are harder to disarm*/
		if (f_idx == FEAT_MTRAP_PORTAL) dis_chance /= 2;

		/*wary or smart creatures much more likely to disarm the trap*/
		if (r_ptr->flags2 & (RF2_SMART)) dis_chance *= 2;
		if (m_ptr->mflag & (MFLAG_WARY)) dis_chance *= 2;

		trap_skill = (p_ptr->state.skills[SKILL_DISARM] + p_ptr->lev - 15) / 2;
	}

	/*In decribe mode, we aren't hitting any trap*/
	if (mode == MODE_DESCRIBE) trap_hit = FALSE;

	/* Monsters may attempts to disarm traps which would affect them,
	 * but occasionally set it off
	 */
	if (trap_hit)
	{
		if ((r_ptr->flags2 & (RF2_SMART)) && (randint(dis_chance) > trap_skill))
		{
			/*accidentally blows it up*/
			if (one_in_(dis_chance / 15))
			{
				if (m_ptr->ml)
				{
					msg_format("%^s tries to disarm your trap, but sets it off!", m_name);
				}

				/* worked */
				trap_hit = TRUE;

				/*monster will get extra damage because trap was right in their face.*/
				fail_disarm = TRUE;

			}

  			/*succeeds in disarming*/
			else
			{
				if (m_ptr->ml)
				{
					msg_format("%^s finds your trap and disarms it.", m_name);
					}

				/* Trap is gone */
				trap_destroyed = TRUE;

				/* Didn't work */
				trap_hit = FALSE;

			}
		}

		/* Monsters can be wary of traps */
		else if ((m_ptr->mflag & (MFLAG_WARY)) || (r_ptr->flags2 & (RF2_SMART)))
		{
			/* Check for avoidance */
			if (randint(dis_chance) > (trap_skill))
			{
				if (m_ptr->ml)
				{
					msg_format("%^s avoids your trap.", m_name);
				}

				/* Didn't work */
				trap_hit = FALSE;
			}
		}
	}

	/* I thought traps only affected players!  Unfair! */
	if ((trap_hit) || (mode == MODE_DESCRIBE))
	{
		/* Assume a default death */
		cptr note_dies = " dies.";

		int n, trap_power = 0;

		int sturdy_break = 9;
		int reg_break = 3;

		if (trap_hit)
		{

			/* Some monsters get "destroyed" */
			if ((r_ptr->flags3 & (RF3_DEMON)) ||
		    	(r_ptr->flags3 & (RF3_UNDEAD)) ||
		    	(r_ptr->flags2 & (RF2_STUPID)) ||
		    	(strchr("Evg", r_ptr->d_char)))
			{
				/* Special note at death */
				note_dies = " is destroyed.";
			}

			/* Players sees the monster, butfailed to disarm got it's own message */
			if ((m_ptr->ml) && (!(fail_disarm))) msg_format("%^s sets off your cunning trap!", m_name);

			/* Not seen but in line of sight */
			else if ((player_has_los_bold(y, x)) && (!(fail_disarm)))
				msg_print("Something sets off your cunning trap!");

			/* Monster is not seen or in LOS */
			else
			{
				/* HACK - no message for non-damaging traps */
				if ((!(f_idx == FEAT_MTRAP_CONFUSION)) && (!(f_idx == FEAT_MTRAP_SLOWING)) &&
					(!(f_idx == FEAT_MTRAP_PORTAL)))
				{
		    		msg_print("You hear anguished yells in the distance.");
				}
			}

			/* Explosion traps are always destroyed. */
			if (f_idx == FEAT_MTRAP_EXPLOSIVE)
			{
				trap_destroyed = TRUE;
			}

			/* Some traps are rarely destroyed */
			else if (f_idx == FEAT_MTRAP_STURDY)
			{
				if (one_in_(sturdy_break)) trap_destroyed = TRUE;
			}

			/* Most traps are destroyed 1 time in 3 */
			else if (one_in_(reg_break)) trap_destroyed = TRUE;

			/* Find the 'power' of the trap effect */
			n = p_ptr->lev + ((p_ptr->lev * p_ptr->lev)/ 12);
			trap_power = 3 + randint(n) + n;

			/*the monster who fails to disarm gets a full blast at point-blank range*/
			if (fail_disarm) trap_power *=3;

			/* Monsters can be wary of traps */
			else if (m_ptr->mflag & (MFLAG_WARY)) trap_power /= 3;

			/* Trap 'critical' based on disarming skill (if not wary) */
			else if (randint(trap_skill) > (randint(dis_chance + (r_ptr->level * 2))))
			{
				trap_power += trap_power / 3;
			}
		}

		/* Affect the monster, or describe the trap. */
		switch (f_idx)
		{
			/* Sturdy trap gives 33% of damage (normal traps give half) */
			case FEAT_MTRAP_STURDY:
			{
				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will cause a moderate amount of damage any creature who walks into it.");
					text_out(format("  This trap is %d times more sturdy than all other monster traps.", (sturdy_break / reg_break)));
					break;
				}

				if (mode == MODE_ACTION)
				{

					mon_take_hit(cave_m_idx[y][x], (trap_power / 3), &fear, note_dies, SOURCE_PLAYER);

				}
				break;
			}

			/* Confusion trap */
			case  FEAT_MTRAP_CONFUSION:
			{
				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will attempt to confuse any creature who walks into it.");
					break;
				}

				if (mode == MODE_ACTION)
				{
					int tmp = rand_int((3 * trap_power) / 2) - r_ptr->level - 10;

					if (tmp < 0)
					{
						if (m_ptr->ml) msg_format("%^s is unaffected!", m_name);
					}
					else mon_inc_timed(m_idx, MON_TMD_CONF, 4 + tmp, MON_TMD_FLG_NOTIFY);
				}

				break;
			}

			/* Slow the monster */
			case FEAT_MTRAP_SLOWING:
			{

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will attempt to slow any creature who walks into it.");
					break;
				}

				if (mode == MODE_ACTION)
				{
					int tmp = rand_int((3 * trap_power) / 2) - r_ptr->level - 10;

					if (tmp < 0)
					{
						if (m_ptr->ml) msg_format("%^s is unaffected!", m_name);
					}

					/* set or add to slow counter */
					else mon_inc_timed(m_idx, MON_TMD_SLOW, 4 + tmp, MON_TMD_FLG_NOTIFY);
				}

				break;
			}

			/* Slow the monster */
			case FEAT_MTRAP_DRAIN_LIFE:
			{
				byte rad = 3;

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will set off an explosion that will damage");
					text_out(format(" any living creature within %d squares that is in line of sight of the blast.", rad));
					break;
				}

				if (mode == MODE_ACTION)
				{


					/*ball of drain life*/
					(void)explosion(SOURCE_PLAYER, rad, y, x, (3 * trap_power) / 4, GF_LIFE_DRAIN, PROJECT_KILL);

					break;
				}
			}

			case FEAT_MTRAP_POISON:
			{
				int rad = 3;

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will set off an explosion of poison gas that will affect");
					text_out(format(" any creature within %d squares that is in line of sight of the blast.", rad));
					break;
				}

				if (mode == MODE_ACTION)
				{


					/*ball of poison*/
					(void)explosion(SOURCE_PLAYER, rad, y, x, (4 * trap_power) / 3, GF_POIS, (PROJECT_KILL | PROJECT_PLAY));

				}

				break;
			}

			case FEAT_MTRAP_ELEC:
			{
				int rad = 3;

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will set off an explosion of electricity that will affect");
					text_out(format(" any creature within %d squares that is in line of sight of the blast.", rad));
					break;
				}

				if (mode == MODE_ACTION)
				{

					/*ball of electricity*/

					(void)explosion(SOURCE_PLAYER, rad, y, x, (7 * trap_power) /8 , GF_ELEC, PROJECT_KILL);
				}

				break;
			}

			case FEAT_MTRAP_EXPLOSIVE:
			{
				int rad = 3;

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will set off an explosion of plamsa, followed by an");
					text_out(" explosion of shards, that will affect ");
					text_out(format(" any creature within %d squares that is in line of sight of the blast.", rad));
					break;
				}

				if (mode == MODE_ACTION)
				{

					/*explosion of fire*/
					(void)explosion(SOURCE_PLAYER, 3, y, x, trap_power, GF_PLASMA, PROJECT_KILL);

					/*followed by shards*/
					(void)explosion(SOURCE_PLAYER, 3, y, x, trap_power, GF_SHARD, PROJECT_KILL);

				}

				break;
			}

			case FEAT_MTRAP_PORTAL:
			{
				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will teleport any creature who walks into it.");
					break;
				}

				if (mode == MODE_ACTION)
				{
					/*teleport the monster*/
					if (teleport_away(cave_m_idx[y][x], 5 + (trap_power / 10)))
					{
						/*give message if in LOS*/
						if (m_ptr->ml) msg_format("%^s is teleported.", m_name);
					}
				}

				break;
			}

			/* Dispel Monsters Trap */
			case FEAT_MTRAP_DISPEL_M:
			{

				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will damage any creature within");
					text_out(" line of sight when the trap is set off.");
					break;
				}

				if (mode == MODE_ACTION)
				{

					/*100% - 200% damage of trap power to all creatures within LOS of trap*/
					int dam = (trap_power + randint(trap_power) + randint(trap_power / 2));

					/* Damage the target monster */
					(void)project_los(y, x, dam, GF_DISP_ALL);
				}

				break;
			}

			/* Default to the basic trap - half damage */
			default:
			{
				if (mode == MODE_DESCRIBE)
				{
					text_out("  This monster trap will damage any creature who walks into it.");
					break;
				}

				if (mode == MODE_ACTION)
				{

					(void)(mon_take_hit(cave_m_idx[y][x], trap_power / 2, &fear,
								note_dies, SOURCE_PLAYER));
				}

				break;
			}
		}

		if (mode == MODE_DESCRIBE)
		{
			/* HACK - no message for non-damaging traps */
			if ((!(f_idx == FEAT_MTRAP_CONFUSION)) && (!(f_idx == FEAT_MTRAP_SLOWING)) &&
					(!(f_idx == FEAT_MTRAP_PORTAL)))
			{
				text_out("  The amount of damage caused by this trap increases as the player gains levels.");
			}
			return;
		}

		/*make the monsters who saw wary*/
		(void)project_los(y, x, 0, GF_MAKE_WARY);

	}

	if (trap_destroyed)
	{
		msg_format("The trap has been destroyed.");

		/* Destroy the trap */
		delete_effect_idx(cave_x_idx[y][x]);

		/* Redraw the spot */
		light_spot(y, x);

		/*one less trap on level*/
		num_trap_on_level--;

		/* Stop resting */
		disturb(FALSE, 0);
	}

	/* Return */
	return;
}



/*
 * Given a central direction at position [dir #][0], return a series
 * of directions radiating out on both sides from the central direction
 * all the way back to its rear.
 *
 * Side directions come in pairs; for example, directions '1' and '3'
 * flank direction '2'.  The code should know which side to consider
 * first.  If the left, it must add 10 to the central direction to
 * access the second part of the table.
 */
static byte side_dirs[20][8] =
{
	{ 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias right */
	{ 1, 4, 2, 7, 3, 8, 6, 9 },
	{ 2, 1, 3, 4, 6, 7, 9, 8 },
	{ 3, 2, 6, 1, 9, 4, 8, 7 },
	{ 4, 7, 1, 8, 2, 9, 3, 6 },
	{ 5, 5, 5, 5, 5, 5, 5, 5 },
	{ 6, 3, 9, 2, 8, 1, 7, 4 },
	{ 7, 8, 4, 9, 1, 6, 2, 3 },
	{ 8, 9, 7, 6, 4, 3, 1, 2 },
	{ 9, 6, 8, 3, 7, 2, 4, 1 },

	{ 0, 0, 0, 0, 0, 0, 0, 0 },	/* bias left */
	{ 1, 2, 4, 3, 7, 6, 8, 9 },
	{ 2, 3, 1, 6, 4, 9, 7, 8 },
	{ 3, 6, 2, 9, 1, 8, 4, 7 },
	{ 4, 1, 7, 2, 8, 3, 9, 6 },
	{ 5, 5, 5, 5, 5, 5, 5, 5 },
	{ 6, 9, 3, 8, 2, 7, 1, 4 },
	{ 7, 4, 8, 1, 9, 2, 6, 3 },
	{ 8, 7, 9, 4, 6, 1, 3, 2 },
	{ 9, 8, 6, 7, 3, 4, 2, 1 }
};

#ifdef MONSTER_SMELL
/*
 * Get and return the strength (age) of scent in a given grid.
 *
 * Return "-1" if no scent exists in the grid.
 */
int get_scent(int y, int x)
{
	int age;
	int scent;

	/* Check Bounds */
	if (!(in_bounds(y, x))) return (-1);

	/* Sent trace? */
	scent = cave_when[y][x];

	/* No scent at all */
	if (!scent) return (-1);

	/* Get age of scent */
	age = scent - scent_when;

	/*Hack - sound is recorded in multiples of 100 now*/
	age *= 100;

	/* Return the age of the scent */
	return (age);
}


/*
 * Can the monster catch a whiff of the character?
 *
 * Many more monsters can smell, but they find it hard to smell and
 * track down something at great range.
 */
static bool monster_can_smell(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int age;

	/* Get the age of the scent here */
	age = get_scent(m_ptr->fy, m_ptr->fx);

	/* No scent */
	if (age == -1) return (FALSE);

	/* Scent is too old */
	if (age > SMELL_STRENGTH) return (FALSE);

	/* Canines and Zephyr Hounds are amazing trackers */
	if (strchr("CZ", r_ptr->d_char))
	{
		/* I smell a character! */
		return (TRUE);
	}

	/* So are the Nazgul */
	else if ((strchr("W", r_ptr->d_char)) &&
	         (r_ptr->flags1 & (RF1_UNIQUE)))
	{
		/* Bloodscent! */
		return (TRUE);
	}

	/* Other monsters can sometimes make good use of scent */
	else if (strchr("fkoqyHORTY", r_ptr->d_char))
	{
		if (age <= SMELL_STRENGTH - 10)
		{
			/* Something's in the air... */
			return (TRUE);
		}
	}


	/* You're imagining things. */
	return (FALSE);
}

#endif /*MONSTER_SMELL*/

/*
 * Determine if there is a space near the the selected spot in which
 * a summoned creature can appear
 */
static int summon_possible(int y1, int x1)
{
	int y, x;
	int num_clear=0;

	/* Start at the location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x) > 2) continue;

			/* Hack: no summon on glyph of warding */
			if (f_info[cave_feat[y][x]].f_flags1 & (FF1_GLYPH)) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y, x) && los(y1, x1, y, x))
			{
				num_clear++;
			}
		}
	}

	return (num_clear);
}

struct gf_type_match_flags
{
	int gf_type;	/* The GF type */
	u32b gf_spell;		/* The monster flag */
	byte flag_set;	/* Which monster flag set */
};


/*
 * Events triggered by the various flags.
 */
static const struct gf_type_match_flags gf_and_flags[] =
{
	/* Ball spells */
	{GF_ACID, 		RF5_BALL_ACID, 		5},
	{GF_ELEC, 		RF5_BALL_ELEC, 		5},
	{GF_FIRE, 		RF5_BALL_FIRE, 		5},
	{GF_COLD, 		RF5_BALL_COLD, 		5},
	{GF_POIS, 		RF5_BALL_POIS, 		5},
	{GF_LIGHT, 		RF5_BALL_LIGHT, 	5},
	{GF_DARK, 		RF5_BALL_DARK, 		5},
	{GF_CONFUSION, 	RF5_BALL_CONFU, 	5},
	{GF_SOUND, 		RF5_BALL_SOUND, 	5},
	{GF_SHARD, 		RF5_BALL_SHARD, 	5},
	{GF_WATER, 		RF5_BALL_STORM, 	5},
	{GF_NETHER, 	RF5_BALL_NETHR, 	5},
	{GF_CHAOS, 		RF5_BALL_CHAOS, 	5},
	{GF_MANA, 		RF5_BALL_MANA, 		5},
	{GF_WATER, 		RF5_BALL_WATER, 	5},

	{GF_ACID, 		RF4_BRTH_ACID, 		4},
	{GF_ELEC, 		RF4_BRTH_ELEC, 		4},
	{GF_FIRE, 		RF4_BRTH_FIRE, 		4},
	{GF_COLD, 		RF4_BRTH_COLD, 		4},
	{GF_POIS, 		RF4_BRTH_POIS, 		4},
	{GF_LIGHT, 		RF4_BRTH_LIGHT, 	4},
	{GF_DARK, 		RF4_BRTH_DARK , 	4},
	{GF_CONFUSION, 	RF4_BRTH_CONFU, 	4},
	{GF_SOUND, 		RF4_BRTH_SOUND, 	4},
	{GF_SHARD, 		RF4_BRTH_SHARD, 	4},
	{GF_NETHER, 	RF4_BRTH_NETHR, 	4},
	{GF_CHAOS, 		RF4_BRTH_CHAOS, 	4},
	{GF_MANA, 		RF4_BRTH_MANA, 		4},
	{GF_DISENCHANT,	RF4_BRTH_DISEN, 	4},
	{GF_NEXUS, 		RF4_BRTH_NEXUS, 	4},
	{GF_TIME, 		RF4_BRTH_TIME, 		4},
	{GF_INERTIA,	RF4_BRTH_INER, 		4},
	{GF_GRAVITY,	RF4_BRTH_GRAV, 		4},
	{GF_SHARD, 		RF4_BRTH_SHARD, 	4},
	{GF_PLASMA,		RF4_BRTH_PLAS, 		4},
	{GF_FORCE, 		RF4_BRTH_FORCE, 	4},
	{GF_MANA, 		RF4_BRTH_MANA, 		4},

};


/*
 * Determines if the monster breathes the element, either by
 * a ball spell, or by a breath spell.
 */
bool race_breathes_element(const monster_race *r_ptr, int gf_type)
{
	u16b i;

	/* Search through the list for breaths that match the right GF*/
	for (i = 0; i < N_ELEMENTS(gf_and_flags); i++)
	{
		const struct gf_type_match_flags *gff = &gf_and_flags[i];

		/* Find the right GF_TYPE */
		if (gf_type != gf_and_flags->gf_type) continue;

		/* Return true if the monster race has the right flag */
		if ((gff->flag_set == 4) &&
			(r_ptr->flags4 & (gf_and_flags->gf_spell))) return (TRUE);
		if ((gff->flag_set == 5) &&
			(r_ptr->flags5 & (gf_and_flags->gf_spell))) return (TRUE);
		if ((gff->flag_set == 6) &&
			(r_ptr->flags6 & (gf_and_flags->gf_spell))) return (TRUE);
		if ((gff->flag_set == 7) &&
			(r_ptr->flags7 & (gf_and_flags->gf_spell))) return (TRUE);
	}

	return FALSE;
}

/*
 * Return true if monster race 2 breathes all of the breaths
 * and ball spells that monster race 1 breathes.
 */
bool race_similar_breaths(const monster_race *r_ptr, const monster_race *r2_ptr)
{
	u32b f4 = r_ptr->flags4;
	u32b f5 = r_ptr->flags5;
	u32b f6 = r_ptr->flags6;
	u32b f7 = r_ptr->flags7;
	u32b f4_2 = r2_ptr->flags4;
	u32b f5_2 = r2_ptr->flags5;
	u32b f6_2 = r2_ptr->flags6;
	u32b f7_2 = r2_ptr->flags7;

	/* Limit to the breath and ball masks for each race.*/
	f4   &= (RF4_BREATH_MASK | RF4_BALL_MASK);
	f5   &= (RF5_BREATH_MASK | RF5_BALL_MASK);
	f6   &= (RF6_BREATH_MASK | RF6_BALL_MASK);
	f7   &= (RF7_BREATH_MASK | RF7_BALL_MASK);
	f4_2 &= (RF4_BREATH_MASK | RF4_BALL_MASK);
	f5_2 &= (RF5_BREATH_MASK | RF5_BALL_MASK);
	f6_2 &= (RF6_BREATH_MASK | RF6_BALL_MASK);
	f7_2 &= (RF7_BREATH_MASK | RF7_BALL_MASK);

	/* One of the monsters doesn't have any ball or breath spells. */
	if ((!f4)   && (!f5)   && (!f6)   && (!f7))   return (FALSE);
	if ((!f4_2) && (!f5_2) && (!f6_2) && (!f7_2)) return (FALSE);

	/* Filter second race breaths and ball spells to only what the first race casts */
	f4_2 &= (f4);
	f5_2 &= (f5);
	f6_2 &= (f6);
	f7_2 &= (f7);


	/* Return false if any of the 4 flag sets don't match */
	if (f4 != f4_2) return (FALSE);
	if (f5 != f5_2) return (FALSE);
	if (f6 != f6_2) return (FALSE);
	if (f7 != f7_2) return (FALSE);

	/* The second monster has all of the breaths/spells of the first */
	return (TRUE);
}

/* States if monsters on two separate coordinates are similar or not*/
bool race_similar_monsters(int m_idx, int m2y, int m2x)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_type *m2_ptr;
	monster_race *r2_ptr;

	/* First check if there are monsters on the target coordinates. */
	if (!(cave_m_idx[m2y][m2x] > 0)) return(FALSE);

	/* Access monster 2*/
	m2_ptr = &mon_list[cave_m_idx[m2y][m2x]];
	r2_ptr = &r_info[m2_ptr->r_idx];

	/* the same character */
	if (r_ptr->d_char == r2_ptr->d_char) return (TRUE);

	/*
	 * Same race (we are not checking orcs, giants, or
	 * trolls because that would be true at
	 * the symbol check
	 */
	if ((r_ptr->flags3 & (RF3_DRAGON)) && (r2_ptr->flags3 & (RF3_DRAGON))) return(TRUE);
	if ((r_ptr->flags3 & (RF3_DEMON)) && (r2_ptr->flags3 & (RF3_DEMON))) return(TRUE);
	if ((r_ptr->flags3 & (RF3_UNDEAD)) && (r2_ptr->flags3 & (RF3_UNDEAD))) return(TRUE);

	/*We are not checking for animal*/

	/*Not the same*/
	return(FALSE);
}


/*
 * Fully update monster's knowledge of the player.
 * Used by player ghost (and all monsters with smart_cheat).
 */
static void update_smart_cheat(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];

	/* Know weirdness */
	if (p_ptr->state.free_act) m_ptr->smart |= (SM_IMM_FREE);
	if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
	if (p_ptr->state.skills[SKILL_SAVE] >= 75) m_ptr->smart |= (SM_GOOD_SAVE);
	if (p_ptr->state.skills[SKILL_SAVE] >= 100) m_ptr->smart |= (SM_PERF_SAVE);

	/* Know immunities */
	if (p_ptr->state.immune_acid) m_ptr->smart |= (SM_IMM_ACID);
	if (p_ptr->state.immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
	if (p_ptr->state.immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
	if (p_ptr->state.immune_cold) m_ptr->smart |= (SM_IMM_COLD);
	if (p_ptr->state.immune_pois) m_ptr->smart |= (SM_IMM_POIS);

	/* Know oppositions */
	if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
	if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
	if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
	if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
	if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);

	/* Know resistances */
	if (p_ptr->state.resist_acid) m_ptr->smart |= (SM_RES_ACID);
	if (p_ptr->state.resist_elec) m_ptr->smart |= (SM_RES_ELEC);
	if (p_ptr->state.resist_fire) m_ptr->smart |= (SM_RES_FIRE);
	if (p_ptr->state.resist_cold) m_ptr->smart |= (SM_RES_COLD);
	if (p_ptr->state.resist_pois) m_ptr->smart |= (SM_RES_POIS);
	if (p_ptr->state.resist_fear) m_ptr->smart |= (SM_RES_FEAR);
	if (p_ptr->state.resist_light) m_ptr->smart |= (SM_RES_LIGHT);
	if (p_ptr->state.resist_dark) m_ptr->smart |= (SM_RES_DARK);
	if (p_ptr->state.resist_blind) m_ptr->smart |= (SM_RES_BLIND);
	if (p_ptr->state.resist_confu) m_ptr->smart |= (SM_RES_CONFU);
	if (p_ptr->state.resist_sound) m_ptr->smart |= (SM_RES_SOUND);
	if (p_ptr->state.resist_shard) m_ptr->smart |= (SM_RES_SHARD);
	if (p_ptr->state.resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
	if (p_ptr->state.resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
	if (p_ptr->state.resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
	if (p_ptr->state.resist_disen) m_ptr->smart |= (SM_RES_DISEN);

	return;
}

/*
 * Used to determine the player's known level of resistance to a
 * particular spell.
 *
 * The LRN_xxx constant determines what type of resistance is
 * applicable.  The monster's SM_xxx flags, as well as player
 * conditions, are referred to as needed.
 * -BR-
 */
static int find_resist(int m_idx, int spell_lrn)
{
	monster_type *m_ptr = &mon_list[m_idx];

	int a;
	u32b smart;

	/* Nothing Known */
	if (!m_ptr->smart) return (0);

	/* get smart flags */
	smart = m_ptr->smart;

	/* Which spell */
	switch (spell_lrn)
	{
		/* Spells 'resisted' by AC, Dex, etc.
		 * Currently no assessment is made */
		case LRN_ARCH:
		{
			return (0);
		}
		/* As above, but poisonous. */
		case LRN_PARCH:
		{
			if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (10);
			else return (0);
		}
		/* Acid Spells */
		case LRN_ACID:
		{
			if (smart & (SM_IMM_ACID)) return (100);
			else if ((smart & (SM_OPP_ACID)) && (smart & (SM_RES_ACID))) return (70);
			else if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID))) return (40);
			else return (0);
		}
		/* Lightning Spells */
		case LRN_ELEC:
		{
			if (smart & (SM_IMM_ELEC)) return (100);
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) return (70);
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) return (40);
			else return (0);
		}
		/* Fire Spells */
		case LRN_FIRE:
		{
			if (smart & (SM_IMM_FIRE)) return (100);
			else if ((smart & (SM_OPP_FIRE)) && (smart & (SM_RES_FIRE))) return (70);
			else if ((smart & (SM_OPP_FIRE)) || (smart & (SM_RES_FIRE))) return (40);
			else return (0);
		}
		/* Cold Spells */
		case LRN_COLD:
		{
			if (smart & (SM_IMM_COLD)) return (100);
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) return (70);
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) return (40);
			else return (0);
		}
		/* Ice Spells */
		case LRN_ICE:
		{
			if (smart & (SM_IMM_COLD)) a=90;
			else if ((smart & (SM_OPP_COLD)) && (smart & (SM_RES_COLD))) a = 60;
			else if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD))) a = 30;
			else a = 0;
			if (smart & (SM_RES_SOUND)) a += 5;
			if (smart & (SM_RES_SHARD)) a += 5;
			return (a);
		}
		/* Poison Spells */
		case LRN_POIS:
		{
			if (smart & (SM_IMM_POIS)) return (100);
			else if ((smart & (SM_OPP_POIS)) && (smart & (SM_RES_POIS))) return (80);
			else if ((smart & (SM_OPP_POIS)) || (smart & (SM_RES_POIS))) return (55);
			else return (0);
		}
		/* Plasma Spells */
		case LRN_PLAS:
		{
			if (smart & (SM_RES_SHARD)) return (15);
			else return (0);
		}
		/* Light Spells */
		case LRN_LIGHT:
		{
			if (smart & (SM_RES_LIGHT)) return (30);
			else return (0);
		}
		/* Darkness Spells */
		case LRN_DARK:
		{
			if (smart & (SM_RES_DARK)) return (30);
			else return (0);
		}
		/* Confusion Spells, damage dealing */
		case LRN_CONFU:
		{
			if (smart & (SM_RES_CONFU)) return (30);
			else return (0);
		}
		/* Sound Spells */
		case LRN_SOUND:
		{
			a=0;
			if (smart & (SM_RES_SOUND)) a += 30;
			if (smart & (SM_RES_CONFU)) a += 10;
			if (smart & (SM_PERF_SAVE)) a += 10;
			else if (smart & (SM_GOOD_SAVE)) a += 5;
			return (a);
		}
		/* Irresistible, but sound prevents stun */
		case LRN_SOUND2:
		{
			if (smart & (SM_RES_SOUND)) return (5);
			else return (0);
		}
		/* Shards Spells */
		case LRN_SHARD:
		{
			if (smart & (SM_RES_SHARD)) return (30);
			else return (0);
		}
		/* Nexus Spells */
		case LRN_NEXUS:
		{
			if (smart & (SM_RES_NEXUS)) return (30);
			else return (0);
		}
		/* Nether Spells */
		case LRN_NETHR:
		{
			if (smart & (SM_RES_NETHR)) return (30);
			else return (0);
		}
		/* Nether Spells */
		case LRN_LAVA:
		{
			if (smart & (SM_NAT_LAVA)) return (70);
			else return (0);
		}
		/* Chaos Spells */
		case LRN_CHAOS:
		{
			a = 0;
			if (smart & (SM_RES_CHAOS)) return(30);
			if (smart & (SM_RES_NETHR))  a += 10;
			if (smart & (SM_RES_CONFU))  a += 10;
			return (a);
		}
		/* Disenchantment Spells */
		case LRN_DISEN:
		{
			if (smart & (SM_RES_DISEN)) return (30);
			else return (0);
		}
		/* Storm Spells */
		case LRN_STORM:
		{
			a=0;
			if (smart & (SM_IMM_ELEC)) a += 15;
			else if ((smart & (SM_OPP_ELEC)) && (smart & (SM_RES_ELEC))) a += 10;
			else if ((smart & (SM_OPP_ELEC)) || (smart & (SM_RES_ELEC))) a += 5;
			if ((smart & (SM_OPP_COLD)) || (smart & (SM_RES_COLD)) ||(smart & (SM_IMM_COLD))) a += 5;
			if ((smart & (SM_OPP_ACID)) || (smart & (SM_RES_ACID)) ||(smart & (SM_IMM_ACID))) a += 5;
			if (smart & (SM_RES_CONFU)) a += 10;
			return (a);
		}
		/* Water Spells */
		case LRN_WATER:
		{
			a=0;
			if (smart & (SM_RES_CONFU)) a += 10;
			if (smart & (SM_RES_SOUND)) a += 5;
			return (a);
		}
		/* Spells that attack player mana */
		case LRN_MANA:
		{
			if (smart & (SM_IMM_MANA)) return (100);
			else return (0);
		}
		/* Spells Requiring Save or Resist Nexus */
		case LRN_NEXUS_SAVE:
		{
			if (smart & (SM_RES_NEXUS)) return (100);
			else if (smart & (SM_PERF_SAVE)) return (100);
			else if (smart & (SM_GOOD_SAVE)) return (30);
			else return (0);
		}
		/* Spells Requiring Save or Resist Fear */
		case LRN_FEAR_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_FEAR)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->timed[TMD_AFRAID]) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Blindness */
		case LRN_BLIND_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_BLIND)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->timed[TMD_BLIND]) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Resist Confusion */
		case LRN_CONFU_SAVE:
		{
			a = 0;
			if (smart & (SM_RES_CONFU)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->timed[TMD_CONFUSED]) a += 50;
			}
			return (a);
		}
		/* Spells Requiring Save or Free Action */
		case LRN_FREE_SAVE:
		{
			a = 0;
			if (smart & (SM_IMM_FREE)) a = 100;
			else if (smart & (SM_PERF_SAVE)) a = 100;
			else if (p_ptr->timed[TMD_PARALYZED]) a = 80;
			else
			{
				if (smart & (SM_GOOD_SAVE)) a += 30;
				if (p_ptr->timed[TMD_SLOW]) a += 50;
			}
			return (a);
		}

		/* Spells Requiring Save  */
		case LRN_SAVE:
		{
			if (smart & (SM_PERF_SAVE)) return (100);
			else if (smart & (SM_GOOD_SAVE)) return (30);
			else return (0);
		}

		/* Spells Requiring Darkness/Save */
		case LRN_DARK_SAVE:
		{
			a = 0;

			if (smart & (SM_RES_DARK)) a += 25;

			if (smart & (SM_PERF_SAVE)) a += 25;
			else if (smart & (SM_GOOD_SAVE)) a += 15;
			return (a);
		}

		/* Anything else */
		default:
		{
			return (0);
		}
	}
}


/*
 * Used to exclude spells which are too expensive for the
 * monster to cast.  Excludes all spells that cost more than the
 * current available mana.
 *
 * Smart monsters may also exclude spells that use a lot of mana,
 * even if they have enough.
 *
 * -BR-
 */
static void remove_expensive_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, max_cost;

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Determine maximum amount of mana to be spent */
	/* Smart monsters will usually not blow all their mana on one spell.
	 */
	if (r_ptr->flags2 & (RF2_SMART))
		max_cost = (m_ptr->mana * (rand_range(4, 6))) / 6;

	/* Otherwise spend up to the full current mana */
	else max_cost = m_ptr->mana;

	/* check innate spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (spell_info_RF4[i][COL_SPELL_MANA_COST] > max_cost) f4 &= ~(0x00000001 << i);
	}

	/* check normal spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (spell_info_RF5[i][COL_SPELL_MANA_COST] > max_cost) f5 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (spell_info_RF6[i][COL_SPELL_MANA_COST] > max_cost) f6 &= ~(0x00000001 << i);
	}

	/* check other spells for mana available */
	for (i = 0; i < 32; i++)
	{
		if (spell_info_RF7[i][COL_SPELL_MANA_COST] > max_cost) f7 &= ~(0x00000001 << i);
	}

	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;

}

/*
 * Intelligent monsters use this function to filter away spells
 * which have no benefit.
 */
static void remove_useless_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool require_los)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Don't regain mana if full */
	if (m_ptr->mana >= r_ptr->mana) f6 &= ~(RF6_ADD_MANA);

	/* Don't heal if full */
	if (m_ptr->hp >= m_ptr->maxhp) f6 &= ~(RF6_HEAL);

	/* Don't Haste if Hasted */
	if (m_ptr->m_timed[MON_TMD_FAST] > 10) f6 &= ~(RF6_HASTE);

	/* Don't cure if not needed */
	if (!((m_ptr->m_timed[MON_TMD_STUN]) ||(m_ptr->m_timed[MON_TMD_FEAR]) ||
	      (m_ptr->m_timed[MON_TMD_SLOW])))	f6 &= ~(RF6_CURE);

	/* Don't jump in already close, or don't want to be close */
	if (!(m_ptr->cdis > m_ptr->best_range) && require_los)
		f6 &= ~(RF6_TELE_SELF_TO);

	if (m_ptr->min_range > 5) f6 &= ~(RF6_TELE_SELF_TO);

	/* Rarely teleport to if too far or close */
	if ((m_ptr->cdis == 1) && (!one_in_(3))) f6 &= ~(RF6_TELE_TO);

	/* Modify the spell list. */
	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*f7p) = f7;
}

/*
 * Count the number of castable spells.
 *
 * If exactly 1 spell is available cast it.  If more than more is
 * available, and the random bit is set, pick one.
 *
 * Used as a short cut in 'choose_attack_spell' to circumvent AI
 * when there is only 1 choice. (random=FALSE)
 *
 * Also used in 'choose_attack_spell' to circumvent AI when
 * casting randomly (random=TRUE), as with dumb monsters.
 */
static int choose_attack_spell_fast(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *f7p, bool do_random)
{
	int i, num=0;
	byte spells[128];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b f7 = (*f7p);

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i)) spells[num++] = i + 32 * 3;
	}

	/* Extract the "attack" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i)) spells[num++] = i + 32 * 4;
	}

	/* Extract the "miscellaneous" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i)) spells[num++] = i + 32 * 5;
	}

	/* Extract the "summon" spells */
	for (i = 0; i < 32; i++)
	{
		if (f7 & (1L << i)) spells[num++] = i + 32 * 6;
	}

	/* Paranoia */
	if (num == 0) return (0);

	/* Go quick if possible */
	if (num == 1)
	{
		/* Hack - Don't cast if known to be immune, unless
		 * casting randomly anyway.  */
		if (!(do_random))
		{
			if (spells[0] < 128)
			{
				if (find_resist(m_idx, spell_desire_RF4[spells[0]-96][D_RES]) == 100) return (0);
			}
			else if (spells[0] < 160)
			{
				if (find_resist(m_idx, spell_desire_RF5[spells[0]-128][D_RES]) == 100) return (0);
			}
			else if (spells[0] < 192)
			{
				if (find_resist(m_idx, spell_desire_RF6[spells[0]-160][D_RES]) == 100) return (0);
			}
			else
			{
				if (find_resist(m_idx, spell_desire_RF7[spells[0]-192][D_RES]) == 100) return (0);
			}
		}

		/* Otherwise cast the one spell */
		else return (spells[0]);
	}

	/*
	 * If we aren't allowed to choose at random
	 * and we have multiple spells left, give up on quick
	 * selection
	 */
	if (!(do_random)) return (0);

	/* Pick at random */
	return (spells[rand_int(num)]);
}

/*
 * Have a monster choose a spell.
 *
 * Monster at m_idx uses this function to select a legal attack spell.
 * Spell casting AI is based here.
 *
 * First the code will try to save time by seeing if
 * choose_attack_spell_fast is helpful.  Otherwise, various AI
 * parameters are used to calculate a 'desirability' for each spell.
 * There is some randomness.  The most desirable spell is cast.
 *
 * archery_only can be used to restrict us to arrow/boulder type attacks.
 *
 * Returns the spell number, of '0' if no spell is selected.
 *
 *-BR-
 */
static int choose_ranged_attack(int m_idx, int *tar_y, int *tar_x)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	byte *spell_desire;

	u32b f4, f5, f6, f7;

	byte spell_range;

	bool do_random = FALSE;

	bool require_los = TRUE;
	bool monster_blocking = FALSE;
	bool is_breath = FALSE;

	int i;
	int breath_hp, breath_maxhp, path, spaces;

	int want_hps=0, want_escape=0, want_mana=0, want_summon=0;
	int want_tactic=0, cur_range=0;

	int best_spell=0, best_spell_rating=0;
	int cur_spell_rating;

	char m_name[80];
	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	f7 = r_ptr->flags7;

	/*default: target the player*/
	*tar_y = p_ptr->py;
	*tar_x = p_ptr->px;

	/*hack - some spells are pointless or unfair on arena levels*/
	if (p_ptr->dungeon_type == DUNGEON_TYPE_ARENA)
	{
		f6 &= ~(RF6_TELE_LEVEL | RF6_TELE_AWAY);
		f7 &= ~(dungeon_summon_mask_f7);
	}
	/*hack - some spells are pointless or unfair on labyrinth levels*/
	else if (p_ptr->dungeon_type == DUNGEON_TYPE_LABYRINTH)
	{
		f6 &= ~(RF6_TELE_LEVEL);
		f7 &= ~(dungeon_summon_mask_f7);
	}
	/*hack - some spells are unfair on themed levels */
	else if (p_ptr->dungeon_type == DUNGEON_TYPE_THEMED_LEVEL)
	{
		f6 &= ~(RF6_TELE_TO | RF6_TELE_LEVEL | RF6_TELE_AWAY);
		f7 &= ~(dungeon_summon_mask_f7);
	}
	/*hack - some spells are unfair on wilderness levels */
	else if (p_ptr->dungeon_type == DUNGEON_TYPE_WILDERNESS)
	{
		f6 &= ~(RF6_TELE_LEVEL);
		f7 &= ~(dungeon_summon_mask_f7);
	}
	/*hack - some spells are unfair on greater_vault levels*/
	else if (p_ptr->dungeon_type >= DUNGEON_TYPE_GREATER_VAULT)
	{
		f6 &= ~(RF6_TELE_LEVEL);
		f7 &= ~(dungeon_summon_mask_f7);
	}


	/* Check what kinds of spells can hit player */
	path = projectable(fy, fx, p_ptr->py, p_ptr->px, PROJECT_CHCK);

	/* do we have the player in sight at all? */
	if (path == PROJECT_NO)
	{
		bool clear_ball_spell = TRUE;

		/* Note if LOS is blocked by a monster instead of a wall */
		if (projectable(fy, fx, p_ptr->py, p_ptr->px, PROJECT_NONE))
		{
			clear_ball_spell = FALSE;
		monster_blocking = TRUE;
		}

		/*are we in range (and not stupid), and have access to ball spells?*/
		else if ((m_ptr->cdis < MAX_RANGE) && (!(r_ptr->flags2 & (RF2_STUPID))) &&
			 ((r_ptr->flags4 & (RF4_BALL_MASK)) ||
			  (r_ptr->flags5 & (RF5_BALL_MASK)) ||
			  (r_ptr->flags6 & (RF6_BALL_MASK)) ||
			  (r_ptr->flags7 & (RF7_BALL_MASK))))
		{

			int alt_y, alt_x, alt_path, best_y, best_x, best_path;

			/*start with no alternate shot*/
			best_y =  best_x = best_path  = 0;

			/* Check for impassable terrain */
			for (i = 0; i < 8; i++)
			{
				alt_y = p_ptr->py + ddy_ddd[i];
				alt_x = p_ptr->px + ddx_ddd[i];

				alt_path = projectable(m_ptr->fy, m_ptr->fx, alt_y, alt_x, PROJECT_CHCK);

				if (alt_path == PROJECT_NO) continue;

				if (alt_path == PROJECT_NOT_CLEAR)
				{
					/*we already have a NOT_CLEAR path*/
					if ((best_path == PROJECT_NOT_CLEAR) && (one_in_(2))) continue;
				}

				/*
			 	 * PROJECT_CLEAR, or monster has an
			 	 * empty square or a square with a safe monster
			 	 *  to lob a ball spell at player
			  	 */
				best_y = alt_y;
				best_x = alt_x;
				best_path = alt_path;
				/*we want to keep ball spells*/
				clear_ball_spell = FALSE;

				if (best_path == PROJECT_CLEAR) break;
			}

			if (best_y + best_x > 0)
			{
				/*default: target the player*/
				*tar_y = best_y;
				*tar_x = best_x;
			}
		}

		/* Don't allow breathing if player is not in a projectable path */
		if (!monster_blocking)
		{
			if (game_mode != GAME_NPPMORIA)
			{
				f4 &= ~(RF4_BREATH_MASK);
				f5 &= ~(RF5_BREATH_MASK);
				f6 &= ~(RF6_BREATH_MASK);
				f7 &= ~(RF7_BREATH_MASK);
			}
			require_los = FALSE;
		}

		/*We don't have a reason to try a ball spell*/
		if (clear_ball_spell)
		{
			f4 &= ~(RF4_BALL_MASK);
			f5 &= ~(RF5_BALL_MASK);
			f6 &= ~(RF6_BALL_MASK);
			f7 &= ~(RF7_BALL_MASK);
		}
	}

	/* Remove spells the 'no-brainers'*/
	/* Spells that require LOS */
	if ((!require_los) || (m_ptr->cdis > MAX_RANGE))
	{
		/*(ball spells would have been filtered out above if not usable*/
		f4 &= (RF4_NO_PLAYER_MASK | RF4_BALL_MASK);
		f5 &= (RF5_NO_PLAYER_MASK | RF5_BALL_MASK);
		f6 &= (RF6_NO_PLAYER_MASK | RF6_BALL_MASK);
		f7 &= (RF7_NO_PLAYER_MASK | RF7_BALL_MASK);
	}

	/*remove bolts and archery shots*/
	else if ((path == PROJECT_NOT_CLEAR) || (monster_blocking))
	{
		f4 &= ~(RF4_BOLT_MASK);
		f4 &= ~(RF4_ARCHERY_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f5 &= ~(RF5_ARCHERY_MASK);
		f6 &= ~(RF6_BOLT_MASK);
		f6 &= ~(RF6_ARCHERY_MASK);
		f7 &= ~(RF7_BOLT_MASK);
		f7 &= ~(RF7_ARCHERY_MASK);
	}

	/*
	 * Flat out 75% chance of not casting if the player is not in sight
	 * In addition, most spells don't work without a player around
	 */
	if ((path == PROJECT_NO) && (!monster_blocking))
	{
		if (!one_in_(4)) return (0);
	}

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Spells we can not afford */
	remove_expensive_spells(m_idx, &f4, &f5, &f6, &f7);

	/* Don't lash if too far or close */
	if ((m_ptr->cdis > 3) || (m_ptr->cdis < 2)) f4 &= ~(RF4_LASH);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Stupid monsters choose at random. */
	if (r_ptr->flags2 & (RF2_STUPID)) return (choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, TRUE));

	/* Remove spells that have no benefit
	 * Does not include the effects of player resists/immunities */
	remove_useless_spells(m_idx, &f4, &f5, &f6, &f7, require_los);

	/* No spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Sometimes non-dumb monsters cast randomly (though from the
	 * restricted list)
	 */
	if ((r_ptr->flags2 & (RF2_SMART)) && (one_in_(10))) do_random = TRUE;
	if ((!(r_ptr->flags2 & (RF2_SMART))) && (one_in_(5))) do_random = TRUE;

	/* Try 'fast' selection first.
	 * If there is only one spell, choose that spell.
	 * If there are multiple spells, choose one randomly if the 'random' flag is set.
	 * Otherwise fail, and let the AI choose.
	 */
	best_spell = choose_attack_spell_fast(m_idx, &f4, &f5, &f6, &f7, do_random);
	if (best_spell) return (best_spell);

	/* If we get this far, we are using the full-up AI.  Calculate
	   some parameters. */

	/* Figure out if we are hurt */
	if (m_ptr->hp < m_ptr->maxhp/8) want_hps += 5;
	else if (m_ptr->hp < m_ptr->maxhp/5) want_hps += 3;
	else if (m_ptr->hp < m_ptr->maxhp/4) want_hps += 2;
	else if (m_ptr->hp < m_ptr->maxhp/2) want_hps++;
	else if (m_ptr->hp == m_ptr->maxhp) f6 &= ~(RF6_HEAL);

	/* Figure out if we want mana */
	if (m_ptr->mana < r_ptr->mana/4) want_mana +=2;
	else if (m_ptr->mana < r_ptr->mana/2) want_mana++;
	else if (m_ptr->mana == m_ptr->mana) f6 &= ~(RF6_ADD_MANA);

	/* Figure out if we want to scram */
	if (want_hps) want_escape = want_hps - 1;
	if (m_ptr->min_range == FLEE_RANGE) want_escape++;

	/* Desire to keep minimum distance */
	if (m_ptr->cdis < m_ptr->min_range)
		want_tactic += (m_ptr->min_range - m_ptr->cdis + 1) / 2;
	if (want_tactic > 3) want_tactic=3;

	/* Check terrain for purposes of summoning spells */
	spaces = summon_possible(m_ptr->fy, m_ptr->fx);
	if (spaces > 10) want_summon=3;
	else if (spaces > 3) want_summon=2;
	else if (spaces > 0) want_summon=1;
	else /*no spaces to summon*/
	{
		f4 &= ~(RF4_SUMMON_MASK);
		f5 &= ~(RF5_SUMMON_MASK);
		f6 &= ~(RF6_SUMMON_MASK);
		f7 &= ~(RF7_SUMMON_MASK);

	}

	/* Check if no spells left */
	if (!f4 && !f5 && !f6 && !f7) return (0);

	/* Find monster properties; Add an offset so that things are OK near zero */
	breath_hp = (m_ptr->hp > 2000 ? m_ptr->hp : 2000);
	breath_maxhp = (m_ptr->maxhp > 2000 ? m_ptr->maxhp : 2000);

	/* Cheat if requested, or if a player ghost. */
	if ((smart_cheat) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
	{
		update_smart_cheat(m_idx);
	}

	/* The conditionals are written for speed rather than readability
	 * They should probably stay that way. */
	for (i = 0; i < 128; i++)
	{
		/* Do we even have this spell? */
		if (i < 32)
		{
			if (!(f4 &(1L <<  i    ))) continue;
			spell_desire=&spell_desire_RF4[i][0];
			spell_range = spell_info_RF4[i][COL_SPELL_BEST_RANGE];
			if (RF4_BREATH_MASK &(1L << (i   ))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else if (i < 64)
		{
			if (!(f5 &(1L << (i-32)))) continue;
			spell_desire=&spell_desire_RF5[i-32][0];
			spell_range=spell_info_RF5[i-32][COL_SPELL_BEST_RANGE];
			if (RF5_BREATH_MASK &(1L << (i-32))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else if (i < 96)
		{
			if (!(f6 &(1L << (i-64)))) continue;
			spell_desire=&spell_desire_RF6[i-64][0];
			spell_range=spell_info_RF6[i-64][COL_SPELL_BEST_RANGE];
			if (RF6_BREATH_MASK &(1L << (i-64))) is_breath=TRUE;
			else is_breath=FALSE;
		}
		else
		{
			if (!(f7 &(1L << (i-96)))) continue;
			spell_desire=&spell_desire_RF7[i-96][0];
			spell_range=spell_info_RF7[i-96][COL_SPELL_BEST_RANGE];
			if (RF7_BREATH_MASK &(1L << (i-96))) is_breath=TRUE;
			else is_breath=FALSE;
		}

		/* Base Desirability*/
		cur_spell_rating = spell_desire[D_BASE];

		/* modified for breath weapons */
		if (is_breath) cur_spell_rating = (cur_spell_rating * breath_hp) / breath_maxhp;

		/* Bonus if want summon and this spell is helpful */
		if (spell_desire[D_SUMM] && want_summon) cur_spell_rating +=
						      want_summon * spell_desire[D_SUMM];

		/* Bonus if wounded and this spell is helpful */
		if (spell_desire[D_HURT] && want_hps) cur_spell_rating +=
							want_hps * spell_desire[D_HURT];

		/* Bonus if low on mana and this spell is helpful */
		if (spell_desire[D_MANA] && want_mana) cur_spell_rating +=
							 want_mana * spell_desire[D_MANA];

		/* Bonus if want to flee and this spell is helpful */
		if (spell_desire[D_ESC] && want_escape) cur_spell_rating +=
							  want_escape * spell_desire[D_ESC];

		/* Bonus if want a tactical move and this spell is helpful */
		if (spell_desire[D_TACT] && want_tactic) cur_spell_rating +=
							   want_tactic * spell_desire[D_TACT];

		/* Penalty if this spell is resisted */
		if (spell_desire[D_RES])
		      cur_spell_rating = (cur_spell_rating * (100 - find_resist(m_idx, spell_desire[D_RES])))/100;

		/* Penalty for range if attack drops off in power */
		if (spell_range)
		{
			cur_range = m_ptr->cdis;
			while (cur_range-- > spell_range)
				cur_spell_rating = (cur_spell_rating * spell_desire[D_RANGE])/100;
		}

		/* Random factor; less random for smart monsters */
		if (r_ptr->flags2 & (RF2_SMART)) cur_spell_rating *= 16 + rand_int(100);
		else cur_spell_rating *= 12 + rand_int(50);

		/* Deflate for testing purposes */
		cur_spell_rating /= 20;

		/* Is this the best spell yet?, or alternate between equal spells*/
		if ((cur_spell_rating > best_spell_rating) ||
			((cur_spell_rating == best_spell_rating) && one_in_(2)))
		{
			best_spell_rating = cur_spell_rating;
			best_spell = i + 96;
		}
	}

	if (p_ptr->wizard)
	{
		msg_format("Spell rating: %i.", best_spell_rating);
	}

	/* Return Best Spell */
	return (best_spell);
}


/*
 * Can the monster exist in this grid?
 *
 * Because this function is designed for use in monster placement and
 * generation as well as movement, it cannot accept monster-specific
 * data, but must rely solely on racial information.
 */
bool cave_exist_mon(const monster_race *r_ptr, int y, int x,
	bool occupied_ok, bool damage_ok, bool can_dig)
{
	feature_type *f_ptr;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* Check location */
	f_ptr = &f_info[cave_feat[y][x]];

	/* The grid is already occupied. */
	if (cave_m_idx[y][x] != 0)
	{
		if (!occupied_ok) return (FALSE);
	}

	/* Glyphs -- must break first */
	if (cave_player_glyph_bold(y, x)) return (FALSE);

	/* Permanent walls are never OK */
	if (_feat_ff1_match(f_ptr, FF1_MOVE | FF1_PERMANENT) ==
		(FF1_PERMANENT)) return (FALSE);

	/*** Check passability of various features. ***/

	/* Feature is a wall */
	if (!cave_passable_bold(y, x))
 	{
		/* Monster isn't allowed to enter */
		if (!can_dig) return (FALSE);

		/* Handle creatures who can go through walls */
		if ((r_ptr->flags2 & (RF2_KILL_WALL)) ||
			(r_ptr->flags2 & (RF2_PASS_WALL)))
		{
			/* Monster is not going there by choice */
			if (damage_ok) return (TRUE);

			/* Check to see if monster wants to go there */
			if (cave_no_dam_for_mon(y, x, r_ptr)) return (TRUE);

			else return (FALSE);

		}
 		else return (FALSE);
 	}

	/* Monster is not going there by choice */
	if (damage_ok) return (TRUE);

	/* Check to see if monster wants to go there */
	if (cave_no_dam_for_mon(y, x, r_ptr)) return (TRUE);

	/* Flying monsters can pass through dangerous terrain */
	if (r_ptr->flags3 & (RF3_FLYING)) return (TRUE);

	/*Monster will be damaged going there*/
	return (FALSE);

}


/*
 * Can the monster enter this grid?  How easy is it for them to do so?
 *
 * The code that uses this function sometimes assumes that it will never
 * return a value greater than 100.
 *
 * The usage of exp to determine whether one monster can kill another is
 * a kludge.  Maybe use HPs, plus a big bonus for acidic monsters
 * against monsters that don't like acid.
 *
 * The usage of exp to determine whether one monster can push past
 * another is also a tad iffy, but ensures that black orcs can always
 * push past other black orcs.
 */
static int cave_passable_mon(monster_type *m_ptr, int y, int x, bool *bash)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Assume nothing in the grid other than the terrain hinders movement */
	int move_chance = 100;

	u16b feat;

	bool is_native;

	int unlock_chance = 0;
	int bash_chance = 0;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (0);

	/* Check nativity */
	is_native = is_monster_native(y, x, r_ptr);

	/* Check location */
	feat = cave_feat[y][x];

	/*
	 * Don't move through permanent walls.
	 * Passable permanent features are allowed (stairs)
	 */
	if (feat_ff1_match(feat, FF1_PERMANENT | FF1_MOVE) == (FF1_PERMANENT))
	{
		return (0);
	}

	/* The grid is occupied by the player. */
	if (cave_m_idx[y][x] < 0)
	{
		/* Monster has no melee blows - character's grid is off-limits. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (0);

		/* Any monster with melee blows can attack the character. */
		else move_chance = 100;
	}

	/* The grid is occupied by a monster. */
	else if ((cave_m_idx[y][x] > 0) && (is_native))
	{
		monster_type *n_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *nr_ptr = &r_info[n_ptr->r_idx];

		/* Move weaker monsters */
		if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
		    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
		    (r_ptr->mexp > nr_ptr->mexp))
		{
			move_chance = 100;
		}

		/* Push past hidden monsters */
		else if ((m_ptr->mflag & (MFLAG_HIDE)) ||
		    (n_ptr->mflag & (MFLAG_HIDE)))
		{
			move_chance = 80;
		}

		/* Push past weaker or similar monsters */
		else if (r_ptr->mexp >= nr_ptr->mexp)
		{
			/* It's easier to push past weaker monsters */
			if (r_ptr->mexp == nr_ptr->mexp) move_chance = 40;
			else move_chance = 80;
		}

		/* Cannot do anything to clear away the other monster */
		else return (0);
	}

	/* Glyphs */
	else if (cave_player_glyph_bold(y, x))
	{
		int glyph_chance = 100 * r_ptr->level / BREAK_GLYPH;

		/* Glyphs are hard to break */
		if (move_chance > glyph_chance)
		{
			 move_chance = glyph_chance;

		}
	}

	/* Monster is flying*/
	else if (MONSTER_CAN_FLY(m_ptr, feat))
	{
		move_chance = 100;
	}

	/* Is the monster native to that square */
	else if (!is_native)
	{
		/* confused monsters or stunned monsters don't know any better */
		if (m_ptr->m_timed[MON_TMD_CONF]) move_chance = 100;
		else if ((m_ptr->m_timed[MON_TMD_STUN]) && (one_in_(10))) move_chance = 100;

		/* Will monster be significantly damaged by going there? */
		else if ((f_info[feat].dam_non_native) > (m_ptr->hp / 15)) return (0);

		/*desirability of the move is based on energy to go in square*/
		else move_chance = 10000 / f_info[feat].non_native_energy_move;

		/*Never more than 100*/
		if (move_chance > 100) move_chance = 100;

	}

	/*Monster is native - movement based on 100 energy average*/
	else
	{
		move_chance = 10000 / f_info[feat].native_energy_move;

		/*Never more than 100*/
		if (move_chance > 100) move_chance = 100;
	}

	/*no further analysis for monsters who pass or kill walls*/
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
	    (r_ptr->flags2 & (RF2_KILL_WALL)))
	{
		return (move_chance);
	}


	/*** Check passability of various features. ***/

	/* Feature is passable */
	if (cave_passable_bold(y, x))
	{
		/*
		 * Any monster can handle floors, except glyphs,
		 * which are handled above
		 */
		return (move_chance);
	}

	/* An effect is disabling movement */
	else if (feat_ff1_match(feat, FF1_MOVE))
	{
		/* Reject grid */
		return (0);
	}

	/* Feature is a wall but the monster can fly over it */
	else if (MONSTER_CAN_FLY(m_ptr, feat))
	{
		return (move_chance);
	}

	/* Glyphs */
	if (cave_player_glyph_bold(y, x))
	{
		/* Glyphs are hard to break */
		return (move_chance);
	}

	/* Monster can open doors and the door isn't jammed */
	if (feat_ff1_match(feat, FF1_SECRET | FF1_DOOR) ==
			(FF1_SECRET | FF1_DOOR))
	{
		/* Discover the secret (temporarily) */
		feat = feat_state(feat, FS_SECRET);
	}

	/* Monster can open doors */
	if ((r_ptr->flags2 & (RF2_OPEN_DOOR)) &&
		feat_ff1_match(feat, FF1_CAN_OPEN) &&
		!feat_ff3_match(feat, FF3_DOOR_JAMMED))
	{
		int open_power = feat_state_power(feat, FS_OPEN);

		/* Secret doors and easily opened stuff */
		if (open_power == 0)
		{
			/*
			 * Note:  This section will have to be rewritten if
			 * secret doors can be jammed or locked as well.
			 */

			/*
			 * It usually takes two turns to open a door
			 * and move into the doorway.
			 */
			return (MIN(50, move_chance));
		}

		/*
		 * Locked doors (not jammed).  Monsters know how hard
		 * doors in their neighborhood are to unlock.
		 */
		else
		{
			int lock_power, ability;

			/* Door power (from 35 to 245) */
			lock_power = 35 * open_power;

			/* Calculate unlocking ability (usu. 11 to 200) */
			ability = r_ptr->level + 10;
			if (r_ptr->flags2 & (RF2_SMART)) ability *= 2;
			if (strchr("ph", r_ptr->d_char))
				ability = 3 * ability / 2;

			/*
			 * Chance varies from 5% to over 100%.  XXX XXX --
			 * we ignore the fact that it takes extra time to
			 * open the door and walk into the entranceway.
			 */
			unlock_chance = (MAX(5, (100 * ability / lock_power)));
		}
	}

	/* Monster can bash doors */
	if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&
		feat_ff1_match(feat, FF1_CAN_BASH))
	{
		int door_power, bashing_power;

		/* Door power (from 60 to 420) */
		/*
		 * XXX - just because a door is difficult to unlock
		 * shouldn't mean that it's hard to bash.  Until the
		 * character door bashing code is changed, however,
		 * we'll stick with this.
		 */
		door_power = 60 + 60 * feat_state_power(feat, FS_BASH);

		/*
		 * Calculate bashing ability (usu. 21 to 300).  Note:
		 * This formula assumes Oangband-style HPs.
		 */
		bashing_power = 20 + r_ptr->level + m_ptr->hp / 15;

		if ((r_ptr->flags3 & (RF3_GIANT)) || (r_ptr->flags3 & (RF3_TROLL)))
			bashing_power = 3 * bashing_power / 2;

		/*
		 * Chance varies from 2% to over 100%.  Note that
		 * monsters "fall" into the entranceway in the same
		 * turn that they bash the door down.
		 */
		bash_chance = (MAX(2, (100 * bashing_power / door_power)));
	}

	/*
	 * A monster cannot both bash and unlock a door in the same
	 * turn.  It needs to pick one of the two methods to use.
	 */
	if (unlock_chance > bash_chance) *bash = FALSE;
	else *bash = TRUE;

	return MIN(move_chance, (MAX(unlock_chance, bash_chance)));

}


/*
 * Get a target for a monster using the special "townsman" AI.
 */
static void get_town_target(monster_type *m_ptr)
{
	int i;
	int y, x;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Clear target */
	m_ptr->target_y = 0;
	m_ptr->target_x = 0;

	/* Hack -- Usually choose a random store, but not cats and dogs */
	if ((r_ptr->d_char != 'f') && (r_ptr->d_char != 'C') && (!one_in_(3)))
	{
		/* shop base - note that shop power is teh shop -1*/
		i = rand_int(MAX_STORES);

		/* Try to find the store XXX XXX */
		for (y =1; y < p_ptr->cur_map_hgt - 1; y++)
		{
			for (x = 1; x < p_ptr->cur_map_wid - 1; x++)
			{
				if (cave_shop_bold(y, x))
				{

					/* Is our store */
					if (f_info[cave_feat[y][x]].f_power == i)
					{
						m_ptr->target_y = y;
						m_ptr->target_x = x;
						break;
					}
				}
			}

			/* Store found */
			if (m_ptr->target_y) return;
		}
	}

	/* No store chosen */
	if (!m_ptr->target_y)
	{
		for (i = 0;; i++)
		{
			/* Pick a grid on the edge of the map (simple test) */
			if (i < 100)
			{
				if (one_in_(2))
				{
					/* Pick a random location along the N/S walls */
					y = rand_range(1, p_ptr->cur_map_hgt - 1);

					if (one_in_(2)) x = p_ptr->cur_map_wid - 2 ;
					else            x = 2 ;

				}
				else
				{
					/* Pick a random location along the E/W walls */
					x = rand_range(1, p_ptr->cur_map_wid -1);

					if (one_in_(2)) y = p_ptr->cur_map_hgt -2 ;
					else            y = 2 ;
				}
			}
			else
			{
				y = rand_range(1, p_ptr->cur_map_hgt - 2);
				x = rand_range(1, p_ptr->cur_map_wid - 2);
			}

			/* Require "empty" floor grids */
			if (cave_empty_bold(y, x))
			{
				m_ptr->target_y = y;
				m_ptr->target_x = x;
				break;
			}
		}
	}
}

/*Returns true if the monster is holding a quest artifact, false if they aren't.*/
static bool holding_quest_artifact(const monster_type *m_ptr)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Search items being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/*Monster is holding a quest item*/
		if(o_ptr->ident & (IDENT_QUEST)) return (TRUE);

	}

	/*didn't find one*/
	return (FALSE);
}

/*
 * Primary function for monsters that want to advance toward the character.
 * Assumes that the monster isn't frightened.
 *
 * Ghosts and rock-eaters do not use flow information, because they
 * can - in general - move directly towards the character.  We could make
 * them look for a grid at their preferred range, but the character
 * would then be able to avoid them better (it might also be a little
 * hard on those poor warriors...).
 *
 * Other monsters will use target information, then their ears, then their
 * noses (if they can), and advance blindly if nothing else works.
 *
 * When flowing, monsters prefer non-diagonal directions.  Monsters also
 * prefer squares that don't have monsters in them.
 *
 */
static void get_move_advance(monster_type *m_ptr, int *ty, int *tx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i;

	byte y, x, y1, x1;

	byte flow_used;

	bool wants_shot = FALSE;

	/* Start with the player's current location*/
	int lowest_cost = BASE_FLOW_MAX;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/*
	 * Monster can go through rocks should always try to head for the player, unless they
	 * are obstructed.
     */
	if ((((r_ptr->flags2 & (RF2_PASS_WALL)) ||
	    (r_ptr->flags2 & (RF2_KILL_WALL)))) &&
		(!(m_ptr->mflag & (MFLAG_NEED_PASS_WALL_FLOW))))
	{
		bool dummy;
		bool passable = FALSE;

		u32b distance_squared_now = (GET_SQUARE((py - y1)) + GET_SQUARE((px - x1)));

		/* Check for impassable terrain */
		for (i = 0; i < 8; i++)
		{
			u32b distance_squared_new;

			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			distance_squared_new =  (GET_SQUARE((py - y)) + GET_SQUARE((px - x)));

			/*This is further away from the player*/
			if (distance_squared_new > distance_squared_now) continue;

			if (cave_passable_mon(m_ptr, y, x, &dummy) >= 75)
			{
				passable = TRUE;
				break;
			}
		}

		/* Usually head straight for character */
		if (passable)
		{
			*ty = py;
			*tx = px;
			return;

		}

		else
		{
			/*Do we need to activate or update this flow?*/
			if ((ABS(py - p_ptr->flow_center_y) > 1) || (ABS(px - p_ptr->flow_center_x) > 1))
			{
				/* Full update of the flows, which should activate the FLOW_PASS_WALLS */
				p_ptr->update |= (PU_FLOW_DOORS | PU_FLOW_NO_DOORS);
			}

			/*This monster needs to advance by flow from now on*/
			m_ptr->mflag |= (MFLAG_NEED_PASS_WALL_FLOW);
		}
	}

	/* Use target information if available */
	if ((m_ptr->target_y) && (m_ptr->target_x))
	{

		*ty = m_ptr->target_y;
		*tx = m_ptr->target_x;
		return;
	}


	/* Find the best and quickest route for the creature */
#ifdef MONSTER_SMELL
	/*
	 * Can the monster use a scent trail? This would need to be re-written, because
	 * the smell and flow data are not on the same scale anymore. -JG
	 */
	if (monster_can_smell(m_ptr))
	{
		/* Using flow information.  Check nearby grids first. */
		for (i = 7; i >= 0; i--)
		{
			int age;


			/* Get the location */
			y = y1 + ddy_ddd[i];
			x = x1 + ddx_ddd[i];

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			age = get_scent(y, x);
			if (age == -1) continue;

			/* Accept younger scent */
			if (lowest_cost < age) continue;
			lowest_cost = age;
		}
	}

#endif /*MONSTER_SMELL*/

	if (m_ptr->using_flow == NEED_FLOW)
	{
		find_best_flow(m_ptr);
	}

	/*
	 * Sometimes there is no good flow, because the player is standing in lava
	 * Just try to head towards the player.
	 */
	if (m_ptr->using_flow == NEED_FLOW)
	{

		*ty = p_ptr->py;
		*tx = p_ptr->px;
		return;
	}


	/*For efficiency, get the flow now*/
	flow_used = m_ptr->using_flow;

	/*Do we prefer a clear path to the player*/
	if ((r_ptr->flags4 & (RF4_BOLT_MASK)) ||
		(r_ptr->flags4 & (RF4_ARCHERY_MASK)) ||
		(r_ptr->flags5 & (RF5_BOLT_MASK)) ||
		(r_ptr->flags5 & (RF5_ARCHERY_MASK)) ||
		(r_ptr->flags6 & (RF6_BOLT_MASK)) ||
		(r_ptr->flags6 & (RF6_ARCHERY_MASK)) ||
		(r_ptr->flags7 & (RF7_BOLT_MASK)) ||
		(r_ptr->flags7 & (RF7_ARCHERY_MASK)))
	{
		/*Is monster wounded?*/
		if ((m_ptr->hp > (m_ptr->maxhp / 2)) || (m_ptr->hp > 750))
		{
				wants_shot = TRUE;
 		}
	}


	/*We don't want to move further away from the player if we don't have to*/
	if (cave_cost[flow_used][m_ptr->fy][m_ptr->fx])
	{
		lowest_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx] + 1;

		*ty = m_ptr->fy;
		*tx = m_ptr->fx;
	}

	/*
	 * Find the best square....  Check nearby grids, diagonals last.
	 */
	for (i = 0; i < 8; i++)
	{

		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Check Bounds */
		if (!in_bounds(y, x)) continue;

		/*Is there a flow here?*/
		if (cave_cost[flow_used][y][x])
		{
			int this_cost = cave_cost[flow_used][y][x];

			/*
			 * Is there a monster here?  If so, encourange a different path
			 * to the player.  Try to surround the player quickly.
			 * Strongest monsters get the quickest path.
			 */
			if (cave_m_idx[y][x] > 0)
			{
				monster_type *m2_ptr = &mon_list[cave_m_idx[y][x]];
				monster_race *r2_ptr = &r_info[m_ptr->r_idx];

				/*
				 * Add a premium for advancing to this square depending on
				 * the strength of the monsters.
				 * Note: Avoid a divide by zero bug if the monster has zero hp.
				 */
				u32b cost_add = (200 * (m2_ptr->hp + 1)) / (m_ptr->hp + 1);

				/*
			 	 * Cap it at a 199 energy penalty, so the monster doesn't prefer a step
			 	 * backwards during normal movement
			 	 */
				if (cost_add > 199) cost_add = 199;

				/* Death molds can be permanent barriers */
				if (r2_ptr->flags1 & (RF1_NEVER_MOVE)) cost_add /= 2;


				/*Apply the penalty*/
				this_cost += cost_add;
			}
			/*avoid glyphs if possible*/
			else if (cave_player_glyph_bold(y, x))
			{
				if (is_monster_native(y, x, r_ptr))
				{
					this_cost += (f_info[cave_feat[y][x]].native_energy_move);
				}
				else this_cost += (f_info[cave_feat[y][x]].non_native_energy_move);
			}
			/*Is it the best route?*/
			if (this_cost < lowest_cost)
			{
				/*Save the new closest distance*/
				lowest_cost = this_cost;

				/* Save the location */
				*ty = y;
				*tx = x;
				continue;
			}

			/*
			 * Go in a straight path if energy is equal
			 */
			else if (this_cost == lowest_cost)
			{

				bool better_spot = FALSE;

				/*
				 * Prefer the new spot if the distance squared is less than the current
				 * distance (see Pythagorean's theory for an explanation of the math)
				 * while it is extremely slow to get the square root of a number
				 * without using math.c (which uses floating point math), we can simply
				 * compare the squared numbers to find out which route is more direct to
				 * the player, thus making the monsters prefer the straight route
				 * vs diaganols.  Since distance in Angband is measured in a square pattern
				 * outward rather than a circular pattern, the monster could wobble
				 * back and forth diagonally while advancing towards the player.
				 * We can make the monster advance in a straighter path based on the
				 * comparing the squares of the distance (A2 + B2) and not bother finding the square root.
				 */

				u32b distance_squared_best = (GET_SQUARE((py - *ty)) + GET_SQUARE((px - *tx)));
				u32b distance_squared_new =  (GET_SQUARE((py - y)) + GET_SQUARE((px - x)));

				/* Have the player angle for a clear shot at the player if the monster wants it. */
				if (wants_shot)
				{
					int best_path = projectable(*ty, *tx, py, px, PROJECT_CHCK);
					int new_path = projectable(y, x, py, px, PROJECT_CHCK);

					/*
					 * This is a clearer path, or if equally clear sometimes pick one
					 * The way the #defines are ordered, the higher the result, the clearer the shot
					 */
					if (new_path > best_path)
					{
						/*Save the best square*/
						lowest_cost = this_cost;

						/* Save the location */
						*ty = y;
						*tx = x;

					 	continue;
					}

					/*
					 * If equally clear almost always pick the further route because that will
					 * most likely help the monster find a clearer path on the next turn.
					 * The one_in_two check will help the creature wobble a bit to try to find an open shot.
					 */
					if ((new_path == best_path) && (best_path < PROJECT_CLEAR))
					{
						if ((distance_squared_new > distance_squared_best) || one_in_(2))

						/*Save the best square*/
						lowest_cost = this_cost;

						/* Save the location */
						*ty = y;
						*tx = x;

					 	continue;
					}
				}

				/*Don't "wobble" towards player*/
				if (distance_squared_new > distance_squared_best) continue;

				/*50-50 shot, for pillar dancers*/
				if (distance_squared_new == distance_squared_best)
				{
					if (one_in_(2)) better_spot = TRUE;
					else continue;
				}

				/*
				 * At this point, distance_squared_new is always lower than distance_squared_cur.
				 * No need to check due to previous if statements eliminating the other possibilities.
				 * The square under consideration is slightly closer,
				 * but we want to gradually make diagonal moves towards the player.
				 */
				if (!better_spot)
				{
					int new_y_diff = ABS(p_ptr->py - y);
					int new_x_diff = ABS(p_ptr->px - x);

					int shorter_axis_diff = 1;

					/*Base it on the shorter axis*/
					if (new_y_diff > new_x_diff) shorter_axis_diff = new_y_diff - new_x_diff;
					else if (new_x_diff > new_y_diff) shorter_axis_diff = new_x_diff - new_y_diff;
					else shorter_axis_diff = 1; /*Hack - straight diagonal path to the player, always take it*/

					if (one_in_(shorter_axis_diff)) better_spot = TRUE;

				}

				if (better_spot)
				{
					/*Save the best square*/
					lowest_cost = this_cost;

					/* Save the location */
					*ty = y;
					*tx = x;
					 continue;
				}
			}
		}
		/*No flow here, continue*/
	}
}



/*
 * "Do not be seen."
 *
 * Monsters in LOS that want to retreat are primarily interested in
 * finding a nearby place that the character can't see into.
 * Search for such a place with the lowest cost to get to up to 15
 * grids away.
 *
 * Look outward from the monster's current position in a square-
 * shaped search pattern.  Calculate the approximate cost in monster
 * turns to get to each passable grid, using a crude route finder.  Penal-
 * ize grids close to or approaching the character.  Ignore hiding places
 * with no safe exit.  Once a passable grid is found that the character
 * can't see, the code will continue to search a little while longer,
 * depending on how pricey the first option seemed to be.
 *
 * If the search is successful, the monster will target that grid,
 * and (barring various special cases) run for it until it gets there.
 *
 * We use a limited waypoint system (see function "get_route_to_target()"
 * to reduce the likelihood that monsters will get stuck at a wall between
 * them and their target (which is kinda embarrassing...).
 *
 * This function does not yield perfect results; it is known to fail
 * in cases where the previous code worked just fine.  The reason why
 * it is used is because its failures are less common and (usually)
 * less embarrassing than was the case before.  In particular, it makes
 * monsters great at not being seen.
 *
 * This function is fairly expensive.  Call it only when necessary.
 */
static bool find_safety(monster_type *m_ptr, int *ty, int *tx)
{
	int i, j, d;

	/* Scanning range for hiding place search. */
	byte scan_range = 15;

	/*
	 * Allocate and initialize a table of movement costs.
	 * Both axis must be (2 * scan_range + 1).
	 */
	u16b safe_cost[31][31];

	int y, x, yy, xx;

	int countdown = scan_range;

	int least_cost = BASE_FLOW_MAX;
	int least_cost_y = 0;
	int least_cost_x = 0;
	int chance, cost, parent_cost;
	bool dummy;

	/* Factors for converting table to actual dungeon grids */
	int conv_y, conv_x;

	for (i = 0; i < 31; i++)
	{
		for (j = 0; j < 31; j++)
		{
			safe_cost[i][j] = 0;
		}
	}

	conv_y = scan_range - m_ptr->fy;
	conv_x = scan_range - m_ptr->fx;

	/* Mark the origin */
	safe_cost[scan_range][scan_range] = 1;

	/* If the character's grid is in range, mark it as being off-limits */
	if ((ABS(m_ptr->fy - p_ptr->py) <= scan_range) &&
	    (ABS(m_ptr->fx - p_ptr->px) <= scan_range))
	{
		safe_cost[p_ptr->py + conv_y][p_ptr->px + conv_x] = BASE_FLOW_MAX;
	}

	/* Work outward from the monster's current position */
	for (d = 0; d < scan_range; d++)
	{
		for (y = scan_range - d; y <= scan_range + d; y++)
		{
			for (x = scan_range - d; x <= scan_range + d;)
			{
				int x_tmp;

				/*
				 * Scan all grids of top and bottom rows, just
				 * outline other rows.
				 */
				if ((y != scan_range - d) && (y != scan_range + d))
				{
					if (x == scan_range + d) x_tmp = 999;
					else x_tmp = scan_range + d;
				}
				else x_tmp = x + 1;

				/* Grid and adjacent grids must be legal */
				if (!in_bounds_fully(y - conv_y, x - conv_x))
				{
					x = x_tmp;
					continue;
				}

				/* Grid is inaccessible (or at least very difficult to enter) */
				if ((safe_cost[y][x] == 0) || (safe_cost[y][x] >= 10000))
				{
					x = x_tmp;
					continue;
				}

				/* Get the accumulated cost to enter this grid */
				parent_cost = safe_cost[y][x];

				/* Scan all adjacent grids */
				for (i = 0; i < 8; i++)
				{
					bool can_hide = FALSE;

					yy = y + ddy_ddd[i];
					xx = x + ddx_ddd[i];

					/* check bounds */
					if ((yy < 0) || (yy > 30) || (xx < 0) || (xx > 30)) continue;

					/*
					 * Handle grids with empty cost and passable grids
					 * with costs we have a chance of beating.
					 */
					if ((safe_cost[yy][xx] == 0) ||
					      ((safe_cost[yy][xx] > parent_cost + 1) &&
					       (safe_cost[yy][xx] < 100)))
					{
						/* The monster can hide here - best case scenario. */
						if ((m_ptr->mflag & (MFLAG_HIDE)) &&
							(cave_ff2_match(yy - conv_y, xx - conv_x, FF2_COVERED)))
						{
							can_hide = TRUE;

							chance = 200;
						}

						/* Get the cost to enter this grid */
						else chance = cave_passable_mon(m_ptr, yy - conv_y,
						         xx - conv_x, &dummy);

						/* Impassable */
						if (!chance)
						{
							/* Cannot enter this grid */
							safe_cost[yy][xx] = 100;
							continue;
						}

						/* Calculate approximate cost (in monster turns) */
						cost = 100 / chance;

						/* Next to character */
						if (distance(yy - conv_y, xx - conv_x,
						    p_ptr->py, p_ptr->px) <= 1)
						{
							/* Don't want to maneuver next to the character */
							cost += 3;
						}

						/* Mark this grid with a cost value */
						safe_cost[yy][xx] = parent_cost + cost;

						/* Character can't see this grid */
						if (!player_can_see_bold(yy - conv_y, xx - conv_x))
						{
							int this_cost = safe_cost[yy][xx];

							/* Penalize grids that approach character */
							if (ABS(p_ptr->py - (yy - conv_y)) <
							    ABS(m_ptr->fy - (yy - conv_y)))
							{
								 this_cost *= 2;
							}
							if (ABS(p_ptr->px - (xx - conv_x)) <
							    ABS(m_ptr->fx - (xx - conv_x)))
							{
								 this_cost *= 2;
							}

							/* Accept lower-cost, sometimes accept same-cost options */
							if ((least_cost > this_cost) ||
							    (least_cost == this_cost && one_in_(2)) || can_hide)
							{
								bool has_escape = FALSE;

								/* Scan all adjacent grids for escape routes */
								for (j = 0; j < 8; j++)
								{
									/* Calculate real adjacent grids */
									int yyy = yy - conv_y + ddy_ddd[i];
									int xxx = xx - conv_x + ddx_ddd[i];

									/* Check bounds */
									if (!in_bounds(yyy, xxx)) continue;

									/* Look for any passable grid that isn't in LOS */
									if (((!player_can_see_bold(yyy, xxx)) &&
									    (cave_passable_mon(m_ptr, yyy, xxx, &dummy))) ||
										((m_ptr->mflag & (MFLAG_HIDE)) &&
										(cave_ff2_match(yyy, xxx, FF2_COVERED))))
									{
										/* Not a one-grid cul-de-sac */
										has_escape = TRUE;
										break;
									}
								}

								/* Ignore cul-de-sacs */
								if (has_escape == FALSE) continue;

								least_cost = this_cost;
								least_cost_y = yy;
								least_cost_x = xx;

								/*
								 * Look hard for alternative hiding places if
								 * this one seems pricey.
								 */
								countdown = 1 + least_cost - d;
							}
						}
					}
				}

				/* Adjust x as instructed */
				x = x_tmp;
			}
		}

		/*
		 * We found a good place a while ago, and haven't done better
		 * since, so we're probably done.
		 */
		if (countdown-- == 0) break;
	}

	/* We found a place that can be reached in reasonable time */
	if (least_cost < 50)
	{
		/* Convert to actual dungeon grid. */
		y = least_cost_y - conv_y;
		x = least_cost_x - conv_x;

		/* Move towards the hiding place */
		*ty = y;
		*tx = x;

		/* Target the hiding place */
		m_ptr->target_y = y;
		m_ptr->target_x = x;

		return (TRUE);
	}


	/* No good place found */
	return (FALSE);
}


/*
 * Helper function for monsters that want to retreat from the character.
 * Used for any monster that is terrified, frightened, is looking for a
 * temporary hiding spot, or just wants to open up some space between it
 * and the character.
 *
 * If the monster is well away from danger, let it relax.
 * If the monster's current target is not in LOS, use it (+).
 * If the monster is not in LOS, and cannot pass through walls, try to
 * use flow (noise) information.
 * If the monster is in LOS, even if it can pass through walls,
 * search for a hiding place (helper function "find_safety()"):
 * search for a hiding place (helper function "find_safety()").
 * If no hiding place is found, and there seems no way out, go down
 * fighting.
 *
 * If none of the above solves the problem, run away blindly.
 *
 * (+) There is one exception to the automatic usage of a target.  If the
 * target is only out of LOS because of "knight's move" rules (distance
 * along one axis is 2, and along the other, 1), then the monster will try
 * to find another adjacent grid that is out of sight.  What all this boils
 * down to is that monsters can now run around corners properly!
 *
 * Return TRUE if the monster did actually want to do anything.
 */
static bool get_move_retreat(monster_type *m_ptr, int *ty, int *tx)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i;
	int y, x;

	bool done = FALSE;
	bool dummy;

	/* If the monster is well away from danger, let it relax. */
	if (m_ptr->cdis >= FLEE_RANGE)
	{
		return (FALSE);
	}

	/*Monster is hiding, relax*/
	if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

	/* Monster has a target */
	if ((m_ptr->target_y) && (m_ptr->target_x))
	{
		/* It's out of LOS; keep using it, except in "knight's move" cases */
		if (!player_has_los_bold(m_ptr->target_y, m_ptr->target_x))
		{
			/* Get axis distance from character to current target */
			int dist_y = ABS(p_ptr->py - m_ptr->target_y);
			int dist_x = ABS(p_ptr->px - m_ptr->target_x);

			/* It's only out of LOS because of "knight's move" rules */
			if (((dist_y == 2) && (dist_x == 1)) ||
			    ((dist_y == 1) && (dist_x == 2)))
			{
				/*
				 * If there is another grid adjacent to the monster that
				 * the character cannot see into, and it isn't any harder
				 * to enter, use it instead.  Prefer diagonals.
				 */
				for (i = 7; i >= 0; i--)
				{
					y = m_ptr->fy + ddy_ddd[i];
					x = m_ptr->fx + ddx_ddd[i];

					/* Check Bounds */
					if (!in_bounds(y, x)) continue;

					if (player_has_los_bold(y, x)) continue;

					if ((y == m_ptr->target_y) && (x == m_ptr->target_x)) continue;

					if (cave_passable_mon(m_ptr, m_ptr->target_y, m_ptr->target_x, &dummy) >
					    cave_passable_mon(m_ptr, y, x, &dummy)) continue;

					m_ptr->target_y = y;
					m_ptr->target_x = x;
					break;
				}
			}

			/* Move towards the target */
			*ty = m_ptr->target_y;
			*tx = m_ptr->target_x;
			return (TRUE);
		}

		/* It's in LOS; cancel it. */
		else
		{
			m_ptr->target_y = 0;
			m_ptr->target_x = 0;
		}
	}

	/*
	 * The  monster has no flow, and no target (as checked above)
	 * First off, the game will crash if using flow == NEEDS flow and the game enters one of the "else" statements below
	 * so this line of "if" and "else" statements must account for this.
	 * In general, the monster will only need this if the player is standing in a dangerous terrain.
	 */
	else if (m_ptr->using_flow == NEED_FLOW)
	{
		if (find_safety(m_ptr, ty, tx) == TRUE) return (TRUE);
	}

	/* The monster is not in LOS, but thinks it's still too close. */
	else if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
	{
		/* Monster cannot pass through walls */
		if (!((r_ptr->flags2 & (RF2_PASS_WALL)) ||
		      (r_ptr->flags2 & (RF2_KILL_WALL))))
		{
			byte flow_used = get_monster_flow(m_ptr);

			/* Run away from noise */
			if (cave_cost[flow_used][m_ptr->fy][m_ptr->fx])
			{
				int start_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx];

				/* Look at adjacent grids, diagonals first */
				for (i = 7; i >= 0; i--)
				{
					y = m_ptr->fy + ddy_ddd[i];
					x = m_ptr->fx + ddx_ddd[i];

					/* Check bounds */
					if (!in_bounds(y, x)) continue;

					/* Accept the first non-visible grid with a higher cost */
					if (cave_cost[flow_used][y][x] > start_cost)
					{
						if (!player_has_los_bold(y, x))
						{
							*ty = y;  *tx = x;
							done = TRUE;
							break;
						}
					}
				}

				/* Return if successful */
				if (done) return (TRUE);
			}
		}

		/* No flow info, or don't need it -- see bottom of function */
	}

	/* The monster is in line of sight. */
	else
	{
		byte flow_used = get_monster_flow(m_ptr);

		int prev_cost = cave_cost[flow_used][m_ptr->fy][m_ptr->fx];
		int start = rand_int(8);

		/* Look for adjacent hiding places */
		for (i = start; i < 8 + start; i++)
		{
			y = m_ptr->fy + ddy_ddd[i % 8];
			x = m_ptr->fx + ddx_ddd[i % 8];

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* No grids in LOS */
			if (player_has_los_bold(y, x)) continue;

			/* Grid must be pretty easy to enter */
			if (cave_passable_mon(m_ptr, y, x, &dummy) < 50) continue;

			/* Accept any grid that doesn't have a lower flow (noise) cost. */
			if (cave_cost[flow_used][y][x] >= prev_cost)
			{
				*ty = y;
				*tx = x;
				prev_cost = cave_cost[flow_used][y][x];

				/* Success */
				return (TRUE);
			}
		}

		/* Find a nearby grid not in LOS of the character. */
		if (find_safety(m_ptr, ty, tx) == TRUE) return (TRUE);

		/*
		 * No safe place found.  If monster is in LOS and close,
		 * it will turn to fight.
		 */
		if ((player_has_los_bold(m_ptr->fy, m_ptr->fx)) &&
			((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0) &&
		    ((m_ptr->cdis < TURN_RANGE) || (m_ptr->m_speed < p_ptr->state.p_speed)))
		{
			if (m_ptr->m_timed[MON_TMD_FEAR])
			{

				char m_name[80];

				m_ptr->mflag |= (MFLAG_DESPERATE);

				/* Forget target */
				m_ptr->target_y = 0;    m_ptr->target_x = 0;

				/* Charge!  XXX XXX */
				m_ptr->min_range = 1;  m_ptr->best_range = 1;

				/* Get the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				msg_format("%^s fights on desperately!", m_name);
			}

			/* Charge! */
			*ty = p_ptr->py;
			*tx = p_ptr->px;
			return (TRUE);
		}
	}

	/* Move directly away from character. */
	*ty = -(p_ptr->py - m_ptr->fy);
	*tx = -(p_ptr->px - m_ptr->fx);

	/* We want to run away */
	return (TRUE);
}

static void calc_vulnerability(void)
{
	byte i;
	int y, x;

	/*already calculated*/
	if (p_ptr->vulnerability) return;

	/* Attack disabled or aggravating player -EB- */
	if (p_ptr->timed[TMD_BLIND] || p_ptr->timed[TMD_IMAGE] || p_ptr->timed[TMD_CONFUSED] ||
		 p_ptr->timed[TMD_AFRAID] || p_ptr->timed[TMD_PARALYZED] || p_ptr->state.aggravate)
	{
		p_ptr->vulnerability = 10;
	}

	/*always more vulnerable if in a room,
	 * and this removes the "monsters run if
	 * player is in corner of a room" bug
	 */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		p_ptr->vulnerability += 8;
	}

	/*
	 * For use in non-rooms (corridors)
	 * Count passable grids next to player
	 */
	else for (i = 0; i < 8; i++)
	{
		y = p_ptr->py + ddy_ddd[i];
		x = p_ptr->px + ddx_ddd[i];

		/* Check Bounds */
		if (!in_bounds(y, x)) continue;

		/* Count floor grids (generic passable) */
		if (cave_passable_bold(y, x))
		{
			p_ptr->vulnerability++;
		}
	}

	/*
	 * Take character weakness into account (this
	 * always adds at least one)
	 */
	p_ptr->vulnerability += (p_ptr->mhp / MAX(1, p_ptr->chp));

	/* Is seriously weakened -- go berserk */
	if ((p_ptr->chp < 2 * p_ptr->mhp / 5) ||
	/*or, very low on mana for spellcasters*/
		(p_ptr->csp < 2 * p_ptr->msp / 25))
	{
		p_ptr->vulnerability = 100;
	}
	/* Sometimes go berserk at random  XXX
	 *
	 */
	else if (p_ptr->vulnerability > 4)
	{

		bool berserk = FALSE;

		if (p_ptr->vulnerability >= 15)
		{
			if (randint(25) < p_ptr->vulnerability) berserk = TRUE;
		}
		else
		{
			int chance = 15 - p_ptr->vulnerability;
			if one_in_(chance) berserk = TRUE;
		}

		if (berserk) p_ptr->vulnerability = 100;
	}
}


/*
 * Choose the probable best direction for a monster to move in.  This
 * is done by choosing a target grid and then finding the direction that
 * best approaches it.
 *
 * Monsters that cannot move always attack if possible.
 * Frightened monsters retreat.
 * Monsters adjacent to the character attack if possible.
 *
 * Monster packs lure the character into open ground and then leap
 * upon him.  Monster groups try to surround the character.  -KJ-
 *
 * Monsters not in LOS always advance (this avoids player frustration).
 * Monsters in LOS will advance to the character, up to their standard
 * combat range, to a grid that allows them to target the character, or
 * just stay still if they are happy where they are, depending on the
 * tactical situation and the monster's preferred and minimum combat
 * ranges.
 * NOTE:  Here is an area that would benefit from more development work.
 *
 * Non-trivial movement calculations are performed by the helper
 * functions "get_move_advance" and "get_move_retreat", which keeps
 * this function relatively simple.
 *
 * The variable "must_use_target" is used for monsters that can't
 * currently perceive the character, but have a known target to move
 * towards.  With a bit more work, this will lead to semi-realistic
 * "hunting" behavior.
 *
 * Return FALSE if monster doesn't want to move or can't.
 */
static bool get_move(monster_type *m_ptr, int *ty, int *tx, bool *fear,
                     bool must_use_target)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Assume no movement */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;

	/*
	 * Monsters that cannot move will attack the character if he is
	 * adjacent.  Otherwise, they cannot move unless they are
	 * caught in non-native terrain that is hurting them.
	 */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Hack -- memorize lack of moves after a while. */
		if (!(l_ptr->r_l_flags1 & (RF1_NEVER_MOVE)))
		{
			if ((m_ptr->ml) && (one_in_(20)))
			{
				l_ptr->r_l_flags1 |= (RF1_NEVER_MOVE);
			}
		}

		/* Is character in range? */
		if (m_ptr->cdis <= 1)
		{

			/* Monster can't melee either (pathetic little creature) */
			if (r_ptr->flags1 & (RF1_NEVER_BLOW))
			{
				/* Hack -- memorize lack of attacks after a while */
				if (!(l_ptr->r_l_flags1 & (RF1_NEVER_BLOW)))
				{
					if ((m_ptr->ml) && (one_in_(10)))
					{
						l_ptr->r_l_flags1 |= (RF1_NEVER_BLOW);
					}
				}
			}

			/* Can attack */
			else
			{
				/* Kill. */
				*fear = FALSE;
				*ty = py;
				*tx = px;
				return (TRUE);
			}
		}

		/*Are we taking damage where we are?*/
		if (!cave_no_dam_for_mon(m_ptr->fy, m_ptr->fx, r_ptr) &&
                       !MONSTER_CAN_FLY(m_ptr, cave_feat[m_ptr->fy][m_ptr->fx]))
		{
			/*Move towards the player*/
			*fear = FALSE;
			get_move_advance(m_ptr, ty, tx);
			return (TRUE);
		}

		/* If we can't hit anything, do not move */
		else return (FALSE);
	}

	/*
	 * Monster is only allowed to use targeting information.
	 */
	if (must_use_target)
	{
		*ty = m_ptr->target_y;
		*tx = m_ptr->target_x;
		return (TRUE);
	}

	/*Stop using the flow if we don't need it*/
	if (m_ptr->mflag & (MFLAG_NEED_PASS_WALL_FLOW))
	{
		/*Must have clear line of sight*/
		if (player_can_fire_bold(m_ptr->fy, m_ptr->fx))
		{
			/*Paranoia - The flow must be is active*/
			if (cost_at_center[FLOW_PASS_WALLS] > 0)
			{
				/*Check if we have a straight and for the most part unobstructed path to the player*/
				if (m_ptr->cdis <= ((cave_cost[FLOW_PASS_WALLS][m_ptr->fy][m_ptr->fx] -
				             cost_at_center[FLOW_PASS_WALLS]) / BASE_ENERGY_MOVE))
				{
					/*We might not need the flow anymore*/
					m_ptr->mflag &= ~(MFLAG_NEED_PASS_WALL_FLOW);
				}
			}
		}
	}

	/*** Handle monster fear -- only for monsters that can move ***/

	/* Is the monster scared? */
	if ((m_ptr->min_range >= FLEE_RANGE) || (m_ptr->m_timed[MON_TMD_FEAR])) *fear = TRUE;
	else *fear = FALSE;

	/* Monster is frightened or terrified. */
	if ((*fear) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0))
	{
		/* The character is too close to avoid, and faster than we are */
		if ((!m_ptr->m_timed[MON_TMD_FEAR]) && (m_ptr->cdis < TURN_RANGE) &&
		     (p_ptr->state.p_speed > m_ptr->m_speed))
		{
			/* Recalculate range */
			find_range(m_ptr);

			/* Note changes in monster attitude */
			if (m_ptr->min_range < m_ptr->cdis)
			{
				/* Cancel fear */
				*fear = FALSE;

				/* No message -- too annoying */
				m_ptr->mflag |= (MFLAG_DESPERATE);

				/* Advance, ... */
				get_move_advance(m_ptr, ty, tx);

				return (TRUE);
			}
		}

		/* The monster is within 25 grids of the character */
		else if (m_ptr->cdis < FLEE_RANGE)
		{
			/* Find and move towards a hidey-hole */
			get_move_retreat(m_ptr, ty, tx);

			return (TRUE);
		}

		/* Monster is well away from danger */
		else
		{
			/* No need to move */
			return (FALSE);
		}
	}

	/* If the character is adjacent, attack or back off.  */
	if ((!*fear) && (m_ptr->cdis <= 1))
	{
		/* Monsters that cannot attack back off. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW))
		{
			/* Hack -- memorize lack of attacks after a while */
			if (!(l_ptr->r_l_flags1 & (RF1_NEVER_BLOW)))
			{
				if ((m_ptr->ml) && (one_in_(10)))
				{
					l_ptr->r_l_flags1 |= (RF1_NEVER_BLOW);
				}
			}

			/* Back away */
			*fear = TRUE;
		}

		else
		{
			/* All other monsters attack. */
			get_move_advance(m_ptr, ty, tx);
			return (TRUE);
		}
	}

	/* Animal packs try to lure the character into the open. */
	if ((!*fear) && ((r_ptr->flags1 & (RF1_FRIENDS)) || (r_ptr->flags1 & (RF1_FRIEND))) &&
	    (r_ptr->flags3 & (RF3_ANIMAL)) &&
	    (!(r_ptr->flags2 & (RF2_PASS_WALL | RF2_KILL_WALL))))
	{

		/* Animal has to be willing to melee */
		if (m_ptr->min_range == 1)
		{
			/*
			 * Make sure player vurnerability is up to date
			 */
			calc_vulnerability();

			/*
			 * RE_DO - figure out how vulnerable player is in this territory
			 * player is more vulnerable in non-native territory
			 */
			if (!is_player_native(p_ptr->py,p_ptr->px))
			{
				p_ptr->vulnerability = p_ptr->vulnerability * 3 / 2;
			}

			/*
			 * Character is insufficiently vulnerable
			 * and monster hasn't been hit by an attack that sets off alarms
			 */
			if ((p_ptr->vulnerability <= 4) && (!(m_ptr->mflag & (MFLAG_ATTACKED_BAD))))
			{
				/* If we're in sight, find a hiding place */
				if (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_FIRE | CAVE_SEEN))
				{
					/* Find a safe spot to lurk in */
					if (get_move_retreat(m_ptr, ty, tx))
					{
						*fear = TRUE;
					}
					else
					{
						/* Cancel fear */
						*fear = FALSE;

						/* No message -- too annoying */

						/* Advance, ... */
						get_move_advance(m_ptr, ty, tx);
					}
				}

				/* Otherwise, we advance cautiously */
				else
				{
					/* Advance, ... */
					get_move_advance(m_ptr, ty, tx);

					/* ... but make sure we stay hidden. */
					if (m_ptr->cdis > 1) *fear = TRUE;
				}

				/* done */
				return (TRUE);
			}
		}
	}

	/* Monster can go through rocks - head straight for character */
	if ((!*fear) && ((r_ptr->flags2 & (RF2_PASS_WALL)) ||
	                 (r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		/* Advance, ... */
		get_move_advance(m_ptr, ty, tx);
	}


	/* No special moves made -- use standard movement */

	/* Not frightened */
	if (!*fear)
	{
		/*
		 * The monster is advancing.
		 */
		get_move_advance(m_ptr, ty, tx);
	}

	/* Monster is frightened */
	else
	{
		/* Back away -- try to be smart about it */
		get_move_retreat(m_ptr, ty, tx);
	}

	/* We do not want to move */
	if ((*ty == m_ptr->fy) && (*tx == m_ptr->fx)) return (FALSE);

	/* We want to move */
	return (TRUE);
}




/*
 * A simple method to help fleeing monsters who are having trouble getting
 * to their target.  It's very stupid, but works fairly well in the
 * situations it is called upon to resolve.  XXX XXX
 *
 * If this function claims success, ty and tx must be set to a grid
 * adjacent to the monster.
 *
 * Return TRUE if this function actually did any good.
 */
static bool get_route_to_target(monster_type *m_ptr, int *ty, int *tx)
{
	int i, j;
	int y, x, yy, xx;
	int tar_y, tar_x, dist_y, dist_x;

	bool dummy;
	bool below = FALSE;
	bool right = FALSE;

	tar_y = 0;
	tar_x = 0;

	/* Is the target further away vertically or horizontally? */
	dist_y = ABS(m_ptr->target_y - m_ptr->fy);
	dist_x = ABS(m_ptr->target_x - m_ptr->fx);

	/* Target is further away vertically than horizontally */
	if (dist_y > dist_x)
	{
		/* Find out if the target is below the monster */
		if (m_ptr->target_y - m_ptr->fy > 0) below = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Grid is not passable */
			if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

			/* Grid will take me further away */
			if ((( below) && (y < m_ptr->fy)) ||
			    ((!below) && (y > m_ptr->fy)))
			{
				continue;
			}

			/* Grid will not take me closer or further */
			else if (y == m_ptr->fy)
			{
				/* See if it leads to better things */
				for (j = 0; j < 8; j++)
				{
					yy = y + ddy_ddd[j];
					xx = x + ddx_ddd[j];

					/* Grid does lead to better things */
					if ((( below) && (yy > m_ptr->fy)) ||
					    ((!below) && (yy < m_ptr->fy)))
					{
						/* But it is not passable */
						if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

						/*
						 * Accept (original) grid, but don't immediately claim
						 * success
						 */
						tar_y = y;
						tar_x = x;
					}
				}
			}

			/* Grid will take me closer */
			else
			{
				/* Don't look this gift horse in the mouth. */
				*ty = y;
				*tx = x;
				return (TRUE);
			}
		}
	}

	/* Target is further away horizontally than vertically */
	else if (dist_x > dist_y)
	{
		/* Find out if the target is right of the monster */
		if (m_ptr->target_x - m_ptr->fx > 0) right = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			/* Check bounds */
			if (!in_bounds_fully(y, x)) continue;

			/* Grid is not passable */
			if (!cave_passable_mon(m_ptr, y, x, &dummy)) continue;

			/* Grid will take me further away */
			if ((( right) && (x < m_ptr->fx)) ||
			    ((!right) && (x > m_ptr->fx)))
			{
				continue;
			}

			/* Grid will not take me closer or further */
			else if (x == m_ptr->fx)
			{
				/* See if it leads to better things */
				for (j = 0; j < 8; j++)
				{
					yy = y + ddy_ddd[j];
					xx = x + ddx_ddd[j];

					/* Grid does lead to better things */
					if ((( right) && (xx > m_ptr->fx)) ||
					    ((!right) && (xx < m_ptr->fx)))
					{
						/* But it is not passable */
						if (!cave_passable_mon(m_ptr, yy, xx, &dummy)) continue;

						/* Accept (original) grid, but don't immediately claim success */
						tar_y = y;
						tar_x = x;
					}
				}
			}

			/* Grid will take me closer */
			else
			{
				/* Don't look this gift horse in the mouth. */
				*ty = y;
				*tx = x;
				return (TRUE);
			}
		}
	}

	/* Target is the same distance away along both axes. */
	else
	{
		/* XXX XXX - code something later to fill this hole. */
		return (FALSE);
	}

	/* If we found a solution, claim success */
	if ((tar_y) && (tar_x))
	{
		*ty = tar_y;
		*tx = tar_x;
		return (TRUE);
	}

	/* No luck */
	return (FALSE);
}


/*
 * Confused monsters bang into walls and doors, and wander into lava or
 * water.  This function assumes that the monster does not belong in this
 * grid, and therefore should suffer for trying to enter it.
 */
static void make_confused_move(monster_type *m_ptr, int y, int x)
{
	char m_name[80];

	bool seen = FALSE;
	bool fear = FALSE;
	bool death = TRUE;

	bool confused = m_ptr->m_timed[MON_TMD_CONF];

	/* Check Bounds (fully) */
	if (!in_bounds_fully(y, x)) return;

	/* Check visibility */
	if ((m_ptr->ml) && (cave_info[y][x] & (CAVE_SEEN))) seen = TRUE;

	/* Get the monster name/poss */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Feature can't be passed */
	if (!cave_passable_bold(y, x))
	{
		/* Feature is a (known) door */
		if ((f_info[cave_feat[y][x]].f_flags1 & (FF1_DOOR)) &&
			(!(f_info[cave_feat[y][x]].f_flags1 & (FF1_SECRET))))
		{
			if (seen && confused)
				msg_format("%^s bangs into a door.", m_name);
		}

		/* Otherwise, we assume that the feature is a "wall".  XXX  */
		else
		{
			if (seen && confused)
				msg_format("%^s bashes into a wall.", m_name);
		}

		/* Sometimes stun the monster, but only lightly */
		if (one_in_(3))
		{
			mon_inc_timed(get_mon_idx(m_ptr), MON_TMD_STUN, 3, MON_TMD_FLG_NOTIFY);
		}

		/*possibly update the monster health bar*/
		if ((p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])  || (m_ptr->sidebar))
					p_ptr->redraw |= (PR_HEALTH);
	}

	/* Feature is not a wall */
	else
	{
		/* No changes */
	}


	/* Monster is frightened */
	if ((!death) && (fear) && (seen))
	{
		msg_format("%^s panics!", m_name);
	}
}


/*
 * Given a target grid, calculate the grid the monster will actually
 * attempt to move into.
 *
 * The simplest case is when the target grid is adjacent to us and
 * able to be entered easily.  Usually, however, one or both of these
 * conditions don't hold, and we must pick an initial direction, than
 * look at several directions to find that most likely to be the best
 * choice.  If so, the monster needs to know the order in which to try
 * other directions on either side.  If there is no good logical reason
 * to prioritize one side over the other, the monster will act on the
 * "spur of the moment", using current turn as a randomizer.
 *
 * The monster then attempts to move into the grid.  If it fails, this
 * function returns FALSE and the monster ends its turn.
 *
 * The variable "fear" is used to invoke any special rules for monsters
 * wanting to retreat rather than advance.  For example, such monsters
 * will not leave an non-viewable grid for a viewable one and will try
 * to avoid the character.
 *
 * The variable "bash" remembers whether a monster had to bash a door
 * or not.  This has to be remembered because the choice to bash is
 * made in a different function than the actual bash move.  XXX XXX  If
 * the number of such variables becomes greater, a structure to hold them
 * would look better than passing them around from function to function.
 */
static bool make_move(monster_type *m_ptr, int *ty, int *tx, bool fear,
	bool *bash)
{
	int i, j;

	/* Start direction, current direction */
	int dir0, dir;

	/* Deltas, absolute axis distances from monster to target grid */
	int dy, ay, dx, ax;

	/* Existing monster location, proposed new location */
	int oy, ox, ny, nx;

	bool avoid = FALSE;
	bool passable = FALSE;
	bool look_again = FALSE;

	int chance;

	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the change in position needed to get to the target */
	dy = *ty - oy;
	dx = *tx - ox;

	/* Monster is frightened, but is obliged to fight. */
	if ((fear) && (cave_m_idx[*ty][*tx] < 0))
	{
		/* Message if seen */
		if ((m_ptr->ml) && (m_ptr->m_timed[MON_TMD_FEAR]) &&
				  ((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0))
		{
			char m_name[80];

			m_ptr->mflag |= MFLAG_DESPERATE;

			/* Get the monster name */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);

			/* Dump a message */
			msg_format("%^s fights on desperately!", m_name);
		}

		/* Turn and fight */
		fear = FALSE;

		/* Forget target */
		m_ptr->target_y = 0;    m_ptr->target_x = 0;

		/* Charge!  XXX XXX */
		m_ptr->min_range = 1;  m_ptr->best_range = 1;

		/* can't return yet; monster might not be adjacent to player */
	}

	/* Is the target grid adjacent to the current monster's position? */
	if ((!fear) && (dy >= -1) && (dy <= 1) && (dx >= -1) && (dx <= 1))
	{
		/* If it is, try the shortcut of simply moving into the grid */

		/* Get the probability of entering this grid */
		chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

		/* Grid must be pretty easy to enter, or monster must be confused */
		if ((m_ptr->m_timed[MON_TMD_CONF]) || (chance >= 50))
		{
			/*
			 * Amusing messages and effects for confused monsters trying
			 * to enter terrain forbidden to them.
			 */
			if ((m_ptr->m_timed[MON_TMD_CONF]) && (chance <= 25))
			{
				/* Sometimes hurt the poor little critter */
				if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

				/* Do not actually move */
				if (!chance) return (FALSE);
			}

			/* We can enter this grid */
			if ((chance >= 100) || (chance > rand_int(100)))
			{
				return (TRUE);
			}

			/* Failure to enter grid.  Cancel move */
			else
			{
				return (FALSE);
			}
		}
	}

	/* Calculate vertical and horizontal distances */
	ay = ABS(dy);
	ax = ABS(dx);

	/* We mostly want to move vertically */
	if (ay > (ax * 2))
	{
		/* Choose between directions '8' and '2' */
		if (dy < 0)
		{
			/* We're heading up */
			dir0 = 8;
			if ((dx < 0) || (dx == 0 && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading down */
			dir0 = 2;
			if ((dx > 0) || (dx == 0 && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We mostly want to move horizontally */
	else if (ax > (ay * 2))
	{
		/* Choose between directions '4' and '6' */
		if (dx < 0)
		{
			/* We're heading left */
			dir0 = 4;
			if ((dy > 0) || (dy == 0 && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading right */
			dir0 = 6;
			if ((dy < 0) || (dy == 0 && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We want to move up and sideways */
	else if (dy < 0)
	{
		/* Choose between directions '7' and '9' */
		if (dx < 0)
		{
			/* We're heading up and left */
			dir0 = 7;
			if ((ay < ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading up and right */
			dir0 = 9;
			if ((ay > ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
	}

	/* We want to move down and sideways */
	else
	{
		/* Choose between directions '1' and '3' */
		if (dx < 0)
		{
			/* We're heading down and left */
			dir0 = 1;
			if ((ay > ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
		else
		{
			/* We're heading down and right */
			dir0 = 3;
			if ((ay < ax) || (ay == ax && (turn % 2 == 0))) dir0 += 10;
		}
	}


	/*
	 * Now that we have an initial direction, we must determine which
	 * grid to actually move into.
	 */
	if (TRUE)
	{
		/* Build a structure to hold movement data */
		typedef struct move_data move_data;
		struct move_data
		{
			int move_chance;
			bool move_bash;
		};
		move_data moves_data[8];


		/*
		 * Scan each of the eight possible directions, in the order of
		 * priority given by the table "side_dirs", choosing the one that
		 * looks like it will get the monster to the character - or away
		 * from him - most effectively.
		 */
		for (i = 0; i <= 8; i++)
		{
			/* Out of options */
			if (i == 8) break;

			/* Get the actual direction */
			dir = side_dirs[dir0][i];

			/* Get the grid in our chosen direction */
			ny = oy + ddy[dir];
			nx = ox + ddx[dir];

			/* Check Bounds */
			if (!in_bounds(ny, nx)) continue;

			/* Store this grid's movement data. */
			moves_data[i].move_chance =
				cave_passable_mon(m_ptr, ny, nx, bash);
			moves_data[i].move_bash = *bash;


			/* Confused monsters must choose the first grid */
			if (m_ptr->m_timed[MON_TMD_CONF]) break;

			/* If this grid is totally impassable, skip it */
			if (moves_data[i].move_chance == 0) continue;

			/* Frightened monsters work hard not to be seen. */
			if (fear)
			{
				/* Monster is having trouble navigating to its target. */
				if ((m_ptr->target_y) && (m_ptr->target_x) && (i >= 2) &&
				    (distance(m_ptr->fy, m_ptr->fx, m_ptr->target_y, m_ptr->target_x) > 1))
				{
					/* Look for an adjacent grid leading to the target */
					if (get_route_to_target(m_ptr, ty, tx))
					{
						/* Calculate the chance to enter the grid */
						chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

						/* Try to move into the grid */
						if (randint(100) > chance)
						{
							/* Can't move here */
							continue;
						}

						/* Can move */
						return (TRUE);
					}

					/* No good route found */
					else if (i >= 3)
					{
						/*
						 * We can't get to our hiding place.  We're in line of fire.
						 * The only thing left to do is go down fighting.  XXX XXX
						 */
						 if ((m_ptr->ml) && (player_can_fire_bold(oy, ox)) &&
								 ((m_ptr->mflag & (MFLAG_JUST_SCARED | MFLAG_DESPERATE)) == 0))
						 {
							char m_name[80];

							m_ptr->mflag |= (MFLAG_DESPERATE);

							/* Forget target */
							m_ptr->target_y = 0;    m_ptr->target_x = 0;

							/* Charge!  XXX XXX */
							m_ptr->min_range = 1;  m_ptr->best_range = 1;

							/* Get the monster name */
							monster_desc(m_name, sizeof(m_name), m_ptr, 0);

							/* Dump a message if they weren't just scared */
							if (!(m_ptr->mflag & (MFLAG_JUST_SCARED))) msg_format("%^s turns to fight!", m_name);

							/* Hack -- lose some time  XXX XXX */
							return (FALSE);
						}
					}
				}

				/* Attacking the character as a first choice? */
				if ((i == 0) && (ny == p_ptr->py) && (nx == p_ptr->px))
				{
					/* Need to rethink some plans XXX XXX XXX */
					m_ptr->target_y = 0;
					m_ptr->target_x = 0;
				}

				/* Monster is visible */
				if (m_ptr->ml)
				{
					/* And is in LOS */
					if (player_has_los_bold(oy, ox))
					{
						/* Accept any easily passable grid out of LOS */
						if ((!player_has_los_bold(ny, nx)) &&
							(moves_data[i].move_chance > 40))
						{
							break;
						}
					}
					else
					{
						/* Do not enter a grid in LOS */
						if (player_has_los_bold(ny, nx))
						{
							moves_data[i].move_chance = 0;
							continue;
						}
					}
				}

				/* Monster can't be seen, and is not in a "seen" grid. */
				if ((!m_ptr->ml) && (!player_can_see_bold(oy, ox)))
				{
					/* Do not enter a "seen" grid */
					if (player_can_see_bold(ny, nx))
					{
						moves_data[i].move_chance = 0;
						continue;
					}
				}
			}

			/* XXX XXX -- Sometimes attempt to break glyphs. */
			if (cave_player_glyph_bold(ny, nx) && !fear && one_in_(5))
			{
				break;
			}

			/* Initial direction is almost certainly the best one */
			if ((i == 0) && (moves_data[i].move_chance >= 80))
			{
				/*
				 * If backing away and close, try not to walk next
				 * to the character, or get stuck fighting him.
				 */
				if ((fear) && (m_ptr->cdis <= 2) &&
					(distance(p_ptr->py, p_ptr->px, ny, nx) <= 1))
				{
					avoid = TRUE;
				}

				else break;
			}

			/* Either of the first two side directions looks good */
			else if (((i == 1) || (i == 2)) &&
			         (moves_data[i].move_chance >= 50))
			{
				/* Accept the central direction if at least as good */
				if ((moves_data[0].move_chance >=
				     moves_data[i].move_chance))
				{
					if (avoid)
					{
						/* Frightened monsters try to avoid the character */
						if (distance(p_ptr->py, p_ptr->px, ny, nx) == 0)
						{
							i = 0;
						}
					}
					else
					{
						i = 0;
					}
				}

				/* Accept this direction */
				break;
			}

			/* This is the first passable direction */
			if (!passable)
			{
				/* Note passable */
				passable = TRUE;

				/* All the best directions are blocked. */
				if (i >= 3)
				{
					/* Settle for "good enough" */
					break;
				}
			}

			/* We haven't made a decision yet; look again. */
			if (i == 7) look_again = TRUE;
		}


		/* We've exhausted all the easy answers. */
		if (look_again)
		{
			/* There are no passable directions. */
			if (!passable)
			{
				return (FALSE);
			}

			/* We can move. */
			for (j = 0; j < 8; j++)
			{
				/* Accept the first option, however poor.  XXX */
				if (moves_data[j].move_chance)
				{
					i = j;
					break;
				}
			}
		}

		/* If no direction was acceptable, end turn */
		if (i >= 8)
		{
			return (FALSE);
		}

		/* Get movement information (again) */
		dir = side_dirs[dir0][i];
		*bash = moves_data[i].move_bash;

		/* No good moves, so we just sit still and wait. */
		if ((dir == 5) || (dir == 0))
		{
			return (FALSE);
		}

		/* Get grid to move into */
		*ty = oy + ddy[dir];
		*tx = ox + ddx[dir];

		/*
		 * Amusing messages and effects for confused monsters trying
		 * to enter terrain forbidden to them.
		 */
		if ((m_ptr->m_timed[MON_TMD_CONF]) && (moves_data[i].move_chance <= 25))
		{
			/* Sometimes hurt the poor little critter */
			if (one_in_(5)) make_confused_move(m_ptr, *ty, *tx);

			/* Do not actually move */
			if (!moves_data[i].move_chance) return (FALSE);
		}

		/* Try to move in the chosen direction.  If we fail, end turn. */
		if ((moves_data[i].move_chance < 100) &&
		    (randint(100) > moves_data[i].move_chance))
		{
			return (FALSE);
		}
	}

	/* We can move. */
	return (TRUE);
}

/*
 * If one monster moves into another monster's grid, they will
 * normally swap places.  If the second monster cannot exist in the
 * grid the first monster left, this can't happen.  In such cases,
 * the first monster tries to push the second out of the way.
 */
static bool push_aside(monster_type *m_ptr, monster_type *n_ptr)
{
	/* Get racial information about the second monster */
	monster_race *nr_ptr = &r_info[n_ptr->r_idx];

	int y, x, i;
	int dir = 0;

	/*
	 * Translate the difference between the locations of the two
	 * monsters into a direction of travel.
	 */
	for (i = 0; i < 10; i++)
	{
		/* Require correct difference along the y-axis */
		if ((n_ptr->fy - m_ptr->fy) != ddy[i]) continue;

		/* Require correct difference along the x-axis */
		if ((n_ptr->fx - m_ptr->fx) != ddx[i]) continue;

		/* Found the direction */
		dir = i;
		break;
	}

	/* Favor either the left or right side on the "spur of the moment". */
	if (one_in_(2)) dir += 10;

	/* Check all directions radiating out from the initial direction. */
	for (i = 0; i < 7; i++)
	{
		int side_dir = side_dirs[dir][i];

		y = n_ptr->fy + ddy[side_dir];
		x = n_ptr->fx + ddx[side_dir];

		/* Illegal grid */
		if (!in_bounds_fully(y, x)) continue;

		/* Grid is not occupied, and the 2nd monster can exist in it. */
		if (cave_exist_mon(nr_ptr, y, x, FALSE, TRUE, TRUE))
		{
			/* Push the 2nd monster into the empty grid. */
			monster_swap(n_ptr->fy, n_ptr->fx, y, x);
			return (TRUE);
		}
	}

	/* We didn't find any empty, legal grids */
	return (FALSE);
}

/*
 * Process a monster's move.
 *
 * All the plotting and planning has been done, and all this function
 * has to do is move the monster into the chosen grid.
 *
 * This may involve attacking the character, breaking a glyph of
 * warding, bashing down a door, etc..  Once in the grid, monsters may
 * stumble into monster traps, hit a scent trail, pick up or destroy
 * objects, and so forth.
 *
 * A monster's move may disturb the character, depending on which
 * disturbance options are set.
 */
static s16b process_move(monster_type *m_ptr, int ty, int tx, bool bash)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	u16b feat;

	/* Existing monster location, proposed new location */
	int oy, ox, ny, nx;

	/* Default move, default lack of view */
	bool do_move = TRUE;
	bool do_view = FALSE;

	/* Assume nothing */
	bool did_open_door = FALSE;
	bool did_bash_door = FALSE;
	bool did_take_item = FALSE;
	bool did_kill_item = FALSE;
	bool did_kill_body = FALSE;
	bool did_pass_wall = FALSE;
	bool did_kill_wall = FALSE;


	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the destination */
	ny = ty;
	nx = tx;

	/* Check Bounds */
	if (!in_bounds(ny, nx)) return(BASE_ENERGY_MOVE);

	/* The monster is hidden in terrain, trying to attack the player.*/
	if (do_move && (m_ptr->mflag & (MFLAG_HIDE)) && (cave_m_idx[ny][nx] < 0))
	{
		monster_unhide(m_ptr);
	}

	/* The grid is occupied by the player. */
	if (cave_m_idx[ny][nx] < 0)
	{
		/* Attack if possible */
		if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			/* Player is standing on a glyph of warding */
			if (cave_player_glyph_bold(ny, nx))
			{
				/* Break glyphs of warding before attacking the player if possible */
				if (randint(BREAK_GLYPH) < r_ptr->level)
				{
					/* Describe observable breakage */
					if (cave_info[ny][nx] & (CAVE_MARK))
					{
						msg_print("The rune of protection is broken!");
					}
					/* Destroy the rune */
					delete_effect_idx(cave_x_idx[ny][nx]);
				}
			}


			(void)make_attack_normal(m_ptr);
		}

		/* End move */
		do_move = FALSE;
	}

	/* Can still move */
	if (do_move)
	{

		/* Get the feature in the grid that the monster is trying to enter. */
		feat = cave_feat[ny][nx];

		/*
		 * Monster doesn't want to hide in the feature.
		 * Discover secret doors
		 */
		if (feat_ff1_match(feat, FF1_SECRET | FF1_DOOR) == (FF1_SECRET | FF1_DOOR))
		{
			/* Discover secret */
			cave_alter_feat(ny, nx, FS_SECRET);

			/* Rescan the feature */
			feat = cave_feat[ny][nx];

			/* Update visuals */
			if (player_can_see_bold(ny, nx)) do_view = TRUE;
		}

		/* Entering a wall */
		if (!cave_ff1_match(ny, nx, FF1_MOVE))
		{
			/* Monster passes through walls (and doors) */
			if (r_ptr->flags2 & (RF2_PASS_WALL))
			{
				/* Monster went through a wall */
				did_pass_wall = TRUE;
			}

			/* Monster flies over suitable terrain */
			else if (MONSTER_CAN_FLY(m_ptr, feat))
			{
				/* Blank test */
			}

			/* Monster destroys walls (and doors) */
			else if (r_ptr->flags2 & (RF2_KILL_WALL))
			{
				/* Noise distance depends on monster "dangerousness"  XXX */
				int noise_dist = 3 + MIN(8, m_ptr->hp / p_ptr->lev);

				/* Forget the wall */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Note that the monster killed the wall */
				if (player_can_see_bold(ny, nx))
				{
					do_view = TRUE;

					did_kill_wall = TRUE;

					/* Stop everything */
					disturb(0, 0);
				}

				/* Output warning messages if the racket gets too loud */
				else if (m_ptr->cdis <= noise_dist)
				{
					/* Grid is currently a door */
					if (cave_closed_door(ny, nx))
					{
						msg_print("You hear a door being smashed open.");
					}

					/* Grid is anything else */
					else
					{
						msg_print("You hear grinding noises.");
					}

					/* Stop everything if necessary */
					disturb(0, 0);
				}

				if (feat_ff1_match(feat, FF1_CAN_BASH))
				{
					/* Bash through the feature */
					/* Example: closed doors, trees */
 					cave_alter_feat(ny, nx, FS_BASH);
 				}
				else
				{
					/* Tunnel trough the walls */
					cave_alter_feat(ny, nx, FS_TUNNEL);
				}
			}

			/* Monster bashes the door down */
			else if (bash && feat_ff1_match(feat, FF1_CAN_BASH))
			{
				/* Note that the monster bashed the feature (if visible) */
				if ((player_can_see_bold(ny, nx)) || (m_ptr->ml))
				{
					do_view = TRUE;

					did_bash_door = TRUE;

					/* Stop everything */
					disturb(0, 0);
				}

				/* Character is not too far away */
				else if (m_ptr->cdis <= MAX_SIGHT)
				{
					/* Grid is currently a door */
					if (cave_closed_door(ny, nx))
					{
						msg_print("You hear a door burst open!");
					}
					/* Grid is anything else */
					else
					{
						msg_print("You hear a very loud noise.");
					}

					/* Stop everything if necessary */
					disturb(0, 0);
				}

				/* Just open the door sometimes */
				if (feat_ff1_match(feat, FF1_CAN_OPEN) && one_in_(2))
				{
					cave_alter_feat(ny, nx, FS_OPEN);
				}
				/* Or break through the feature */
				else
				{
					cave_alter_feat(ny, nx, FS_BASH);
				}
			}

			else if (feat_ff1_match(feat, FF1_CAN_OPEN))
			{
				/* Locked doors */
				if (feat_ff3_match(feat, FF3_DOOR_LOCKED))
 				{
 					do_move = FALSE;
				}
				/* Ordinary doors */
				else if (feat_ff3_match(feat, FF3_DOOR_CLOSED))
				{
					if (one_in_(5)) do_move = FALSE;
				}

				/* Handle doors in sight */
				if (player_can_see_bold(ny, nx))
				{
					/* Do not disturb automatically */
					do_view = TRUE;
				}
				/* Sometimes monsters are too noisy */
				else if ((m_ptr->cdis <= MAX_SIGHT) && one_in_(3))
				{
					char name[80];

					/* Get the feature name */
					feature_desc(name, sizeof(name), feat, TRUE, TRUE);

					/* Show a message */
					msg_format("You hear %s being opened.", name);

					/* Stop everything if necessary */
					disturb(0, 0);
				}

				/* Unlock the door */
				cave_alter_feat(ny, nx, FS_OPEN);

			}

			/* Paranoia -- Ignore all features not added to this code */
			else return (BASE_ENERGY_MOVE);
		}

		/* An effect is forbidding movement */
		else if (!cave_passable_bold(ny, nx))
		{
			/* Ghosts can pass */
			if (r_ptr->flags2 & (RF2_PASS_WALL))
			{
				did_pass_wall = TRUE;
			}

			/* Diggers can pass. Remove the effect */
			else if (r_ptr->flags2 & (RF2_KILL_WALL))
			{
				/* Get the index of the first effect */
				s16b x_idx = cave_x_idx[ny][nx];

				/* Traverse the effect list of the grid */
				while (x_idx)
				{
					/* Get the effect */
					effect_type *x_ptr = &x_list[x_idx];

					/* Get the associated feature */
					u16b x_feat = x_ptr->x_f_idx;

					/* Point to the next effect */
					x_idx = x_ptr->next_x_idx;

					/* Ignore effects that don't affect movement */
					if (!x_feat || feat_ff1_match(x_feat, FF1_MOVE)) continue;

					/* Remove the effect */
					delete_effect_idx((s16b)(x_ptr - x_list));

					/* Notify the player */
					if (player_has_los_bold(ny, nx))
					{
						char name[80];

						/* Get the name of the effect */
						feature_desc(name, sizeof(name), x_feat, FALSE, TRUE);

						/* Show a message */
						msg_format("The %s was destroyed!", name);
					}
				}

				did_kill_wall = TRUE;
			}

			/* Monster can't pass */
			else return (BASE_ENERGY_MOVE);
		}

		/* Glyphs */
		else if (cave_player_glyph_bold(ny, nx))
		{
			/* Describe observable breakage */
			if (cave_info[ny][nx] & (CAVE_MARK))
			{
				msg_print("The rune of protection is broken!");
			}

			/* Destroy the rune */
			delete_effect_idx(cave_x_idx[ny][nx]);
		}

		/* Monsters tunnel through impassable terrain */
		else if ((r_ptr->flags2 & (RF2_KILL_WALL)) &&
			     (f_info[feat].f_flags1 & (FF1_CAN_TUNNEL)))
		{
			/* Tunnel through the wall */
			cave_alter_feat(ny, nx, FS_TUNNEL);

			/* Did kill wall */
			did_kill_wall = TRUE;
		}
	}

	/* Monster is allowed to move */
	if (do_move)
	{
		/* The grid is occupied by a monster. */
		if (cave_m_idx[ny][nx] > 0)
		{
			monster_type *n_ptr = &mon_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[n_ptr->r_idx];

			/* XXX - Kill (much) weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
				(!holding_quest_artifact(n_ptr)) &&
				(!(n_ptr->mflag & (MFLAG_QUEST))) &&
			    (r_ptr->mexp > nr_ptr->mexp * 2))
			{
				/* Note that the monster killed another monster (if visible) */
				did_kill_body = TRUE;

				/* Kill the monster */
				delete_monster(ny, nx);
			}

			/* Swap with or push aside the other monster */
			else
			{
				/* The other monster cannot switch places */
				if (!cave_exist_mon(nr_ptr, m_ptr->fy, m_ptr->fx, TRUE, TRUE, TRUE))
				{
					/* Try to push it aside */
					if (!push_aside(m_ptr, n_ptr))
					{
						/* Cancel move on failure */
						do_move = FALSE;
					}
				}
			}
		}
	}


	/* Monster can (still) move */
	if (do_move)
	{
		/* Move the monster */
		monster_swap(oy, ox, ny, nx);

		/*Mark the movement in the terrain lore, if the player is in a state to do so*/
		if (player_can_observe() && (m_ptr->ml) && (!(m_ptr->mflag & (MFLAG_FLYING))))
		{

			feature_lore *f_l_ptr = &f_l_list[cave_feat[m_ptr->fy][m_ptr->fx]];

			/*Check if the monster is native*/
			if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_native_moves < MAX_UCHAR) f_l_ptr->f_l_native_moves ++;
			}
			else
			{
				/*Mark the lore*/
				if (f_l_ptr->f_l_non_native_moves < MAX_UCHAR) f_l_ptr->f_l_non_native_moves ++;
			}
		}

		/* Cancel target when reached */
		if ((m_ptr->target_y == ny) && (m_ptr->target_x == nx))
		{
			m_ptr->target_y = 0;
			m_ptr->target_x = 0;
		}

		/* Check for monster trap */
		if cave_monster_trap_bold(ny,nx)
		{
			/* Apply trap */
			apply_monster_trap(x_list[cave_x_idx[ny][nx]].x_f_idx, ny, nx, MODE_ACTION);

			/* Return if dead */
			if (!(m_ptr->r_idx)) return(BASE_ENERGY_MOVE);
		}

		/*Did a new monster get pushed into the old space?*/
		if (cave_m_idx[oy][ox] > 0)
		{
			/*Is there a trap there?*/
			if cave_monster_trap_bold(oy,ox)
			{
				/* Apply trap */
				apply_monster_trap(x_list[cave_x_idx[ny][nx]].x_f_idx, oy, ox, MODE_ACTION);
			}
		}

		/* If he carries a light, update lights */
		if (r_ptr->flags2 & (RF2_HAS_LIGHT)) do_view = TRUE;

#ifdef MONSTER_SMELL

		/*
		 * If a member of a monster group capable of smelling hits a
		 * scent trail while out of LOS of the character, it will
		 * communicate this to similar monsters.
		 */
		if ((!player_has_los_bold(ny, nx)) && (r_ptr->flags1 & (RF1_FRIENDS)) &&
		    (monster_can_smell(m_ptr)) && (get_scent(oy, ox) == -1) &&
		    (!m_ptr->target_y) && (!m_ptr->target_x))
		{
			int i;
			monster_type *n_ptr;
			monster_race *nr_ptr;

			/* Scan all other monsters */
			for (i = mon_max - 1; i >= 1; i--)
			{
				/* Access the monster */
				n_ptr = &mon_list[i];
				nr_ptr = &r_info[n_ptr->r_idx];

				/* Ignore dead monsters */
				if (!n_ptr->r_idx) continue;

				/* Ignore monsters with the wrong symbol */
				if (r_ptr->d_char != nr_ptr->d_char) continue;

				/* Ignore monsters with specific orders */
				if ((n_ptr->target_x) || (n_ptr->target_y)) continue;

				/* Ignore monsters picking up a good scent */
				if (get_scent(n_ptr->fy, n_ptr->fx) < SMELL_STRENGTH - 10)
					continue;

				/* Ignore monsters not in LOS */
				if (!los(m_ptr->fy, m_ptr->fx, n_ptr->fy, n_ptr->fx))
					continue;

				/* Activate all other monsters and give directions */
				n_ptr->csleep = 0;
				n_ptr->mflag |= (MFLAG_ACTV);
				n_ptr->target_y = ny;   n_ptr->target_x = nx;
			}
		}

#endif /*MONSTER_SMELL*/

		/* Monster is visible and not cloaked */
		if (m_ptr->ml)
		{

			/* Player will always be disturbed if monster moves adjacent */
			if (m_ptr->cdis == 1) disturb(1, 0);

			/* Hack -- ignore townspeople if strong enough  -clefs- */
			else if ((m_ptr->mflag & (MFLAG_TOWN)) && (p_ptr->lev >= 10))
			{
				/* Ignore */
			}

			/* Option -- be disturbed by all other monster movement */
			else if (disturb_move) disturb(0, 0);

			/* Option -- be disturbed by monster movement in LOS */
			else if ((m_ptr->ml) && (disturb_near))
			{

				if ((m_ptr->project) ||
						(((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL))) &&
								(m_ptr->cdis < 3)))
				{
					disturb(0, 0);
				}
			}
		}

		/* Take or kill objects on the floor */
		if (((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
			 (r_ptr->flags2 & (RF2_KILL_ITEM))) &&
		    (f_info[cave_feat[ny][nx]].f_flags1 & (FF1_DROP)))
		{
			u32b f1, f2, f3, fn;

			u32b flg3 = 0L;

			char m_name[80];
			char o_name[120];

			s16b this_o_idx, next_o_idx = 0;

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx;
			     this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;


				/* Skip gold */
				if (o_ptr->tval == TV_GOLD)
				{
					continue;
				}

				/* Extract some flags */
				object_flags(o_ptr, &f1, &f2, &f3, &fn);

				/* React to objects that hurt the monster */
				if (f1 & (TR1_SLAY_DRAGON))  flg3 |= (RF3_DRAGON);
				if (f1 & (TR1_KILL_DRAGON))  flg3 |= (RF3_DRAGON);
				if (f1 & (TR1_SLAY_TROLL))   flg3 |= (RF3_TROLL);
				if (f1 & (TR1_SLAY_GIANT))   flg3 |= (RF3_GIANT);
				if (f1 & (TR1_SLAY_ORC))     flg3 |= (RF3_ORC);
				if (f1 & (TR1_SLAY_DEMON))   flg3 |= (RF3_DEMON);
				if (f1 & (TR1_SLAY_UNDEAD))  flg3 |= (RF3_UNDEAD);
				if (f1 & (TR1_SLAY_ANIMAL))  flg3 |= (RF3_ANIMAL);
				if (f1 & (TR1_SLAY_EVIL))    flg3 |= (RF3_EVIL);

				/* The object (or quest related mimic) cannot be picked up by the monster */
				if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3) ||
 				   (f3 & (TR3_NEVER_PICKUP)))
				{
					/* Only give a message for "take_item" */
					if (r_ptr->flags2 & (RF2_TAKE_ITEM))
					{
						/* Take note */
						did_take_item = TRUE;

						/* Describe observable situations */
						if (m_ptr->ml && player_has_los_bold(ny, nx))
						{
							/* Get the object name */
							object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

							/* Get the monster name */
							monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

							/* Dump a message */
							msg_format("%^s tries to pick up %s, but fails.",
								   m_name, o_name);
						}
					}
				}

				/* Pick up the item */
				else if (r_ptr->flags2 & (RF2_TAKE_ITEM))
				{
					object_type *i_ptr;
					object_type object_type_body;

					/* Take note */
					did_take_item = TRUE;

					/* Describe observable situations */
					if (player_has_los_bold(ny, nx) && m_ptr->ml)
					{
						/* Get the object name */
						object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

						/* Get the monster name */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

						/* Dump a message */
						msg_format("%^s picks up %s.", m_name, o_name);
					}

					/* Get local object */
					i_ptr = &object_type_body;

					/* Obtain local object */
					object_copy(i_ptr, o_ptr);

					/* Delete the object */
					delete_object_idx(this_o_idx);

					/* Carry the object */
					(void)monster_carry(cave_m_idx[m_ptr->fy][m_ptr->fx], i_ptr);
				}

				/* Destroy the item */
				else
				{
					/* Take note */
					did_kill_item = TRUE;

					/* Describe observable situations */
					if (player_has_los_bold(ny, nx))
					{
						/* Get the object name */
						object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

						/* Get the monster name */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);

						/* Dump a message */
						message_format(MSG_DESTROY, 0, "%^s crushes %s.", m_name, o_name);
					}

					/* Delete the object */
					delete_object_idx(this_o_idx);
				}
			}
		}
	}             /* End of monster's move */



	/* Notice changes in view */
	if (do_view)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Learn things from observable monster */
	if ((m_ptr->ml) && player_can_observe())
	{
		const feature_type *f_ptr = &f_info[cave_feat[m_ptr->fy][m_ptr->fx]];
		feature_lore *f_l_ptr = &f_l_list[cave_feat[m_ptr->fy][m_ptr->fx]];

		/*Mark the monster lore*/
		if (do_move)
		{
			u32b native = f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3;
			native &= r_ptr->r_native;
			l_ptr->r_l_native |= native;
		}

		/* Monster is flying*/
		if (m_ptr->mflag & (MFLAG_FLYING))
		{
			/* Monster and terrain lore*/
			l_ptr->r_l_flags3 |= (RF3_FLYING);
			if ((f_ptr->f_flags2 & (FF2_CAN_FLY)) && (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_SEEN)))
			{
				f_l_ptr->f_l_flags2 |= FF2_CAN_FLY;
			}
		}

		/* Monster opened a door */
		if (did_open_door) l_ptr->r_l_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->r_l_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->r_l_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->r_l_flags2 |= (RF2_KILL_ITEM);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->r_l_flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->r_l_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->r_l_flags2 |= (RF2_KILL_WALL);
	}

	/*Monster is flying*/
	if (m_ptr->mflag & (MFLAG_FLYING)) return (BASE_ENERGY_MOVE);

	if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
	{
		return(f_info[cave_feat[m_ptr->fy][m_ptr->fx]].native_energy_move);
	}
	else return(f_info[cave_feat[m_ptr->fy][m_ptr->fx]].non_native_energy_move);
}

/*alert others in pack about something using the m_flag, and wake them up*/
static void tell_allies(int y, int x, u32b flag)
{
	monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i;

	/* Scan all other monsters */
	for (i = mon_max - 1; i >= 1; i--)
	{

		/* Access the monster */
		monster_type *n_ptr = &mon_list[i];
		monster_race *nr_ptr = &r_info[n_ptr->r_idx];

		/* Access the monster */
		n_ptr = &mon_list[i];
		nr_ptr = &r_info[n_ptr->r_idx];

		/* Ignore dead monsters */
		if (!n_ptr->r_idx) continue;

		/* Ignore monsters with the wrong symbol */
		if (r_ptr->d_char != nr_ptr->d_char) continue;

		/* Ignore monsters not in LOS */
		if (!los(y, x, n_ptr->fy, n_ptr->fx)) continue;

		/* Activate all other monsters and communicate to them */
		n_ptr->mflag |= (MFLAG_ACTV | flag | MFLAG_DESPERATE);

	}
}


/*
 * Monster takes its turn.
 */
static s16b process_monster(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, k, y, x;
	int ty, tx;
	int chance = 0;
	int choice = 0;
	int dir;

	char m_name[80];

	bool fear = FALSE;

	bool bash = FALSE;

	/* Assume the monster doesn't have a target */
	bool must_use_target = FALSE;

	/* Will the monster move randomly? */
	bool random_move = FALSE;

	/* Calculate the monster's preferred combat range when needed */
	if (m_ptr->min_range == 0) find_range(m_ptr);

	/* Monster is in active mode. */
	if (m_ptr->mflag & (MFLAG_ACTV))
	{
		/*
		 * Character is outside of scanning range and well outside
		 * of sighting range.  Monster does not have a target.
		 */
		if ((m_ptr->cdis >= FLEE_RANGE) && (m_ptr->cdis > r_ptr->aaf) &&
		    (!m_ptr->target_y) && (!m_ptr->target_x))
		{
#ifdef MONSTER_SMELL
			/* Monster cannot smell the character */
			if (!cave_when[m_ptr->fy][m_ptr->fx]) m_ptr->mflag &= ~(MFLAG_ACTV);
			else if (!monster_can_smell(m_ptr))   m_ptr->mflag &= ~(MFLAG_ACTV);
#else /*MONSTER_SMELL*/

		/*Monster is no longer active*/
		m_ptr->mflag &= ~(MFLAG_ACTV | MFLAG_NEED_PASS_WALL_FLOW );
#endif /*MONSTER_SMELL*/
		}
	}


	/* Monster is in passive mode. */
	else
	{
		/* Character is inside scanning range */
		if (m_ptr->cdis <= r_ptr->aaf) m_ptr->mflag |= (MFLAG_ACTV);

		/* Monster has a target */
		else if ((m_ptr->target_y) && (m_ptr->target_x)) m_ptr->mflag |= (MFLAG_ACTV);

#ifdef MONSTER_SMELL

		/* The monster is catching too much of a whiff to ignore */
		else if (cave_when[m_ptr->fy][m_ptr->fx])
		{
			if (monster_can_smell(m_ptr)) m_ptr->mflag |= (MFLAG_ACTV);
		}

#endif /*MONSTER_SMELL*/
	}

	/*
	 * Special handling if the first turn a monster has after
	 * being attacked by the player, but the player is out of sight
	 */
	if (m_ptr->mflag & (MFLAG_HIT_BY_RANGED))
	{
		/*Monster will be very upset if it can't see the player*/
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			m_ptr->mflag |= (MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);

			/*if part of a pack, let them know*/
			if ((r_ptr->flags1 & (RF1_FRIENDS)) ||
				(r_ptr->flags1 & (RF1_FRIEND)) ||
				(r_ptr->flags1 & (RF1_ESCORT)) ||
				(r_ptr->flags1 & (RF1_ESCORTS)))
			{
				tell_allies(m_ptr->fy, m_ptr->fx, MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);
			}

			/*Monsters with ranged attacks will try to cast a spell*/
			if (r_ptr->freq_ranged) m_ptr->mflag |= (MFLAG_ALWAYS_CAST);

			/*Tweak the monster speed*/
			m_ptr->mflag &= ~(MFLAG_SLOWER | MFLAG_FASTER);
			if (one_in_(3))	m_ptr->mflag |= (MFLAG_SLOWER);
			else if (one_in_(2)) m_ptr->mflag |= (MFLAG_FASTER);
			calc_monster_speed(m_ptr->fy, m_ptr->fx);
		}

		/*clear the flag*/
		m_ptr->mflag &= ~(MFLAG_HIT_BY_RANGED);

	}

	/*This if the first turn a monster has after being attacked by the player*/
	if (m_ptr->mflag & (MFLAG_HIT_BY_MELEE))
	{
		/*
		 * Monster will be very upset if it isn't next to the
		 * player (pillar dance, hack-n-back, etc)
		 */
		if (m_ptr->cdis > 1)
		{
			m_ptr->mflag |= (MFLAG_AGGRESSIVE | MFLAG_ATTACKED_BAD);

			/*if part of a pack, let them know*/
			if ((r_ptr->flags1 & (RF1_FRIENDS)) ||
				(r_ptr->flags1 & (RF1_FRIEND)) ||
				(r_ptr->flags1 & (RF1_ESCORT)) ||
				(r_ptr->flags1 & (RF1_ESCORTS)))
			{
				tell_allies(m_ptr->fy, m_ptr->fx, MFLAG_AGGRESSIVE);
			}

			/*Monsters with ranged attacks will try to cast a spell*/
			if (r_ptr->freq_ranged) m_ptr->mflag |= (MFLAG_ALWAYS_CAST);

			/*Tweak the monster speed*/
			m_ptr->mflag &= ~(MFLAG_SLOWER | MFLAG_FASTER);
			if (one_in_(3))	m_ptr->mflag |= (MFLAG_SLOWER);
			else if (one_in_(2)) m_ptr->mflag |= (MFLAG_FASTER);
			calc_monster_speed(m_ptr->fy, m_ptr->fx);
		}

		/*clear the flag*/
		m_ptr->mflag &= ~(MFLAG_HIT_BY_MELEE);

	}

	/* A monster in passive mode will end its turn at this point. */
	if (!(m_ptr->mflag & (MFLAG_ACTV))) return (BASE_ENERGY_MOVE);

	/*Paranoia - Find the best flow for a monster*/
	if (m_ptr->using_flow == NEED_FLOW) find_best_flow(m_ptr);

	/* Hack -- Always redraw the current target monster health bar */
	if ((p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx])  || (m_ptr->sidebar))
		p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

	/* Attempt to multiply if able to and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) &&
	    (!m_ptr->m_timed[MON_TMD_CONF]) && (!m_ptr->m_timed[MON_TMD_FEAR]) &&
	    (mon_cnt < z_info->m_max - 50) && (!(m_ptr->mflag & (MFLAG_STERILE))))
	{
		/* Count the adjacent monsters */
		for (k = 0, y = m_ptr->fy - 1; y <= m_ptr->fy + 1; y++)
		{
			for (x = m_ptr->fx - 1; x <= m_ptr->fx + 1; x++)
			{
				/* Check Bounds */
				if (!in_bounds(y, x)) continue;

				/* Count monsters */
				if (cave_m_idx[y][x] > 0) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(cave_m_idx[m_ptr->fy][m_ptr->fx], FALSE))
			{
				/* Make a sound */
				sound(MSG_MULTIPLY);

				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->r_l_flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return (BASE_ENERGY_MOVE);
			}
		}
	}

	/* Make sure messages such as wakes up, etc. come before attack messages */
	if (size_mon_msg) flush_monster_messages();

	/*** Ranged attacks ***/

	/*Monster can cast spells*/
	if (r_ptr->freq_ranged)
	{
		int tar_y, tar_x;

		/* Extract the ranged attack probability. */
		chance = r_ptr->freq_ranged;

		/*certain conditions always cause a monster to always cast*/
		if (m_ptr->mflag & (MFLAG_ALWAYS_CAST)) chance = 100;

		/* Heavy spell casters will sit back and cast */
		else if (r_ptr->freq_ranged > 20)
		{
			if ((m_ptr->best_range >= m_ptr->cdis +2) &&
				((m_ptr->best_range / 2) <= m_ptr->cdis))
			{

				/* Heavy spell casters will sit back and cast */
				if (m_ptr->mana > r_ptr->mana / 5)
				{
					chance += (100 - chance) / 5;
				}

				/* Creatures that don't move never like to get too close */
				if (r_ptr->flags1 & (RF1_NEVER_MOVE)) chance += (100 - chance) / 3;

				/* Spellcasters that don't strike never like to get too close */
				else if (r_ptr->flags1 & (RF1_NEVER_BLOW)) chance += (100 - chance) / 3;

				/*Monsters who have had dangerous attacks happen to them are more extreme*/
				else if (m_ptr->mflag & (MFLAG_ATTACKED_BAD))
				{
					chance += (100 - chance) / 3;
				}
			}

			/* Breathers like point blank range */
			else if (((r_ptr->flags4 & (RF4_BREATH_MASK)) ||
		     (r_ptr->flags5 & (RF5_BREATH_MASK)) ||
		     (r_ptr->flags6 & (RF6_BREATH_MASK)) ||
		     (r_ptr->flags7 & (RF7_BREATH_MASK))) &&
		     (m_ptr->cdis < 6) &&
		     (m_ptr->hp > m_ptr->maxhp / 2))
			{
				chance += (100 - chance) / 10;
			}

		}

		/*Monsters marked as aggressive or Desperatedo the same*/
		else if (m_ptr->mflag & (MFLAG_AGGRESSIVE | MFLAG_DESPERATE))
		{
			chance += ((100 - chance) / 10);
		}

		/* Cannot use ranged attacks when confused. */
		if (m_ptr->m_timed[MON_TMD_CONF]) chance = 0;

		/* Stunned monsters use ranged attacks half as often. */
		else if ((chance) && (m_ptr->m_timed[MON_TMD_STUN])) chance /= 2;

		/* Hidden creatures love ranged attacks */
		else if (m_ptr->mflag & (MFLAG_HIDE)) chance += ((100 - chance) / 3);

		/*
		 * Monster does not have a path toward the player.  Cast much more often.
		 */
		if ((m_ptr->using_flow == NEED_FLOW) && !(m_ptr->m_timed[MON_TMD_STUN]) && !(m_ptr->m_timed[MON_TMD_CONF]))
		{
			chance += ((100 - chance) * 3 / 4);
		}

		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* Monster can use ranged attacks */
		if (rand_int(100) < chance)
		{
			/* Pick a ranged attack */
			choice = choose_ranged_attack(cave_m_idx[m_ptr->fy][m_ptr->fx], &tar_y, &tar_x);
		}

		/* Selected a ranged attack? */
		if (choice != 0)
		{
			/* The monster is hidden */
			if (m_ptr->mflag & (MFLAG_HIDE)) monster_unhide(m_ptr);

			/* Execute said attack */
			make_attack_ranged(m_ptr, choice, tar_y, tar_x);

			/* End turn */
			return(BASE_ENERGY_MOVE);
		}
	}

	/*** Movement ***/

	/* Assume no movement */
	ty = 0;
	tx = 0;

	/*
	 * Innate semi-random movement.  Monsters adjacent to the character
	 * can always strike accurately at him (monster isn't confused).
	 */
	if ((r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25)) && (m_ptr->cdis > 1))
	{
		chance = 0;

		/* RAND_25 and RAND_50 are cumulative */
		if (r_ptr->flags1 & (RF1_RAND_25))
		{
			chance += 25;
			if (m_ptr->ml) l_ptr->r_l_flags1 |= (RF1_RAND_25);
		}
		if (r_ptr->flags1 & (RF1_RAND_50))
		{
			chance += 50;
			if (m_ptr->ml) l_ptr->r_l_flags1 |= (RF1_RAND_50);
		}

		/* Chance of moving randomly */
		if (rand_int(100) < chance) random_move = TRUE;
	}

	/* Monster isnt' moving randomly, isnt' running away
	 * doesn't hear or smell the character
	 */
	if (!random_move)
	{
		/*
	     * First, monsters who can't cast, are aggressive, and
		 * are not afraid just want to charge
		 */
		if (!m_ptr->m_timed[MON_TMD_FEAR])
		{

			if (m_ptr->mflag & (MFLAG_AGGRESSIVE | MFLAG_DESPERATE) && (!r_ptr->freq_ranged))
			{
				m_ptr->target_y = 0;
				m_ptr->target_x = 0;
			}

			/*Monster can see the player*/
			if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
			{
				m_ptr->target_y = 0;
				m_ptr->target_x = 0;
			}
		}

		/*
		 * Monster has a known target, but not only if the target is because
		 * there is not a good flow
		 */
		if ((m_ptr->target_y) && (m_ptr->target_x) && !(m_ptr->using_flow == NEED_FLOW)) must_use_target = TRUE;
	}

	/* Monster is using the special "townsman" AI */
	else if (m_ptr->mflag & (MFLAG_TOWN))
	{
		/* Always have somewhere to go */
		if ((!m_ptr->target_y) || (!m_ptr->target_x) ||
		    (cave_shop_bold(m_ptr->fy, m_ptr->fx)))
		{
			/* Get a new target */
			get_town_target(m_ptr);
		}

		else if ((m_ptr->target_y == m_ptr->fy) &&
				 (m_ptr->target_x == m_ptr->fx))
		{
			/* go to a new target */
			get_town_target(m_ptr);
		}

		/* Not interested in the character */
		must_use_target = TRUE;
	}

	/*** Find a target to move to ***/

	/* Monster is genuinely confused */
	if ((m_ptr->m_timed[MON_TMD_CONF]) && (!(r_ptr->flags1 & (RF1_NEVER_MOVE))))
	{
		/* Choose any direction except five and zero */
		dir = rand_int(8);

		/* Monster can try to wander into /anything/... */
		ty = m_ptr->fy + ddy_ddd[dir];
		tx = m_ptr->fx + ddx_ddd[dir];
	}

	/* Monster isn't confused, just moving semi-randomly */
	else if (random_move)
	{

		int start = rand_int(8);
		bool dummy;

		/* Is the monster scared? */
		if ((!(r_ptr->flags1 & (RF1_NEVER_MOVE))) && ((m_ptr->mflag & (MFLAG_DESPERATE)) == 0) &&
		    ((m_ptr->min_range >= FLEE_RANGE) ||
		     (m_ptr->m_timed[MON_TMD_FEAR])))
		{
			fear = TRUE;
		}

		/* Look at adjacent grids, starting at random. */
		for (i = start; i < 8 + start; i++)
		{
			y = m_ptr->fy + ddy_ddd[i % 8];
			x = m_ptr->fx + ddx_ddd[i % 8];

			/* Accept first passable grid. */
			if (cave_passable_mon(m_ptr, y, x, &dummy) != 0)
			{
				ty = y;
				tx = x;
				break;
			}
		}

		/* No passable grids found */
		if ((ty == 0) && (tx == 0)) return (BASE_ENERGY_MOVE);

		/* Cannot move, target grid does not contain the character */
		if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) &&
		    (cave_m_idx[ty][tx] >= 0))
		{
			/* Cannot move */
			return(BASE_ENERGY_MOVE);
		}
	}

	/* Normal movement */
	else
	{
		/* Choose a pair of target grids, or cancel the move. */
		if (!get_move(m_ptr, &ty, &tx, &fear, must_use_target))
			return(BASE_ENERGY_MOVE);

	}

	/* Calculate the actual move.  Cancel move on failure to enter grid. */
	if (!make_move(m_ptr, &ty, &tx, fear, &bash)) return (BASE_ENERGY_MOVE);


	/* Change terrain, move the monster, handle secondary effects, end turn. */
	return (process_move(m_ptr, ty, tx, bash));

}




/*
 * Monster regeneration of recovery from all temporary
 * conditions.
 *
 * This function is called a lot, and is therefore fairly expensive.
 */
static void recover_monster(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];
	bool visible = FALSE;

	/* Visible monsters must be both seen and noticed */
	if (m_ptr->ml)
	{
		visible = TRUE;
	}

	/* Handle any area-effects of the monster - only if active */
	if ((r_ptr->flags2 & (RF2_CLOUD_SURROUND)) &&
	    (m_ptr->mflag & (MFLAG_ACTV)))
	{
		/* Assume no affect */
		bool affect = FALSE;

		int typ, dam, rad;

	  	rad = (r_ptr->flags2 & (RF2_POWERFUL) ? 2 : 1);

		/*just a minimal of damage*/
		dam = MAX(1, m_ptr->hp / 100);

		/* The Nazgul sometimes drain light */
		if (r_ptr->d_char == 'W')
		{
			if (one_in_(2)) affect = TRUE;
		}

		/* Silver jellies drain light only if their grid is lit */
		else if ((r_ptr->d_char == 'j') &&
		         (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_GLOW)))
		{
			affect = TRUE;
		}

		/* Other monsters are more likely to emit a cloud when they are closer, but
		 * must always be line of sight */
		else if ((one_in_(m_ptr->cdis)) &&
				 (player_can_fire_bold(m_ptr->fy, m_ptr->fx))) affect = TRUE;

		/* Affect surroundings if appropriate */
		if (affect)
		{
			/* Get information */
			cloud_surround(m_ptr->r_idx, &typ, &dam, &rad);

			/* Learn about monster (before visibility changes) */
			if ((m_ptr->ml) && (r_ptr->flags2 & (RF2_CLOUD_SURROUND)))
			{
				l_ptr->r_l_flags2 |= (RF2_CLOUD_SURROUND);
			}

			/* Release of cloud (can affect visibility) */
			if (typ) mon_cloud(cave_m_idx[m_ptr->fy][m_ptr->fx], typ,
			                   dam, rad);
		}
	}

	/* Monster is sleeping, but character is within detection range */
	if ((m_ptr->m_timed[MON_TMD_SLEEP]) && (m_ptr->cdis <= r_ptr->aaf))
	{
		/* Aggravated by the player */
		if (p_ptr->state.aggravate)
		{
			/* Reset sleep counter */
			mon_clear_timed(m_idx, MON_TMD_SLEEP , MON_TMD_FLG_NOMESSAGE);

			/* Notice the "waking up" */
			if (visible)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);

				add_monster_message(m_name, m_idx, MON_MSG_WAKES_UP);
			}
		}

		/* Standard noise */
		else
		{
			int divisor, d;

			/* Monsters are disturbed less if far away */
			divisor = 300 + m_ptr->cdis * 200;

			/* Monsters are disturbed more if in LOS */
			if (player_has_los_bold(m_ptr->fy, m_ptr->fx)) divisor /= 2;

			/* Get disturbance (additional noise counts double) */
			d = div_round(total_wakeup_chance + add_wakeup_chance, divisor);

			/* Still asleep */

			if (m_ptr->m_timed[MON_TMD_SLEEP] > d)
			{
				/* Monster's sleep is disturbed */
				mon_dec_timed(m_idx, MON_TMD_SLEEP, d , MON_TMD_FLG_NOMESSAGE);

				/* Notice the "not waking up" */
				if (visible)
				{

					/* Hack -- Count the ignores */
					if (l_ptr->ignore < MAX_UCHAR)
					{
						l_ptr->ignore++;
					}

					/* We are making a substantial amount of extra noise */
					if (add_wakeup_chance >= 1000)
					{
						char m_name[80];

						/* Acquire the monster name */
						monster_desc(m_name, sizeof(m_name), m_ptr, 0);

						/* Warning */
						add_monster_message(m_name, m_idx, MON_MSG_STIRS);
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Monster's sleep is disturbed */
				mon_clear_timed(m_idx, MON_TMD_SLEEP, MON_TMD_FLG_NOMESSAGE);

				/* Notice the "waking up" */
				if (visible)
				{
					char m_name[80];

					/* Acquire the monster name */
					monster_desc(m_name, sizeof(m_name), m_ptr, 0);

					add_monster_message(m_name, m_idx, MON_MSG_WAKES_UP);

					/* Hack -- Count the wakings */
					if (l_ptr->wake < MAX_UCHAR)
					{
						l_ptr->wake++;
					}
				}
			}
		}
	}

	/* Recover from stuns */
	if (m_ptr->m_timed[MON_TMD_STUN])
	{
		if (m_ptr->m_timed[MON_TMD_STUN] == 1)
		{
			mon_clear_timed(m_idx, MON_TMD_STUN, MON_TMD_FLG_NOTIFY);
		}

		else mon_dec_timed(m_idx, MON_TMD_STUN, 1 , MON_TMD_FLG_NOMESSAGE);

	}

	/* Recover from confusion */
	if (m_ptr->m_timed[MON_TMD_CONF])
	{
		if (m_ptr->m_timed[MON_TMD_CONF] == 1)
		{
			mon_clear_timed(m_idx, MON_TMD_CONF, MON_TMD_FLG_NOTIFY);
		}

		else mon_dec_timed(m_idx, MON_TMD_CONF, 1 , MON_TMD_FLG_NOMESSAGE);

	}

	/* Recover courage */
	if (m_ptr->m_timed[MON_TMD_FEAR] > 0)
	{
		if (m_ptr->m_timed[MON_TMD_FEAR] == 1)
		{
			mon_clear_timed(m_idx, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY);
			/*re-calculate minimum range */
			find_range(m_ptr);
		}

		else mon_dec_timed(m_idx, MON_TMD_FEAR, 1 , MON_TMD_FLG_NOMESSAGE);
	}

	/*
	 * Handle haste counter
	 */
	if (m_ptr->m_timed[MON_TMD_FAST])
	{
		if (m_ptr->m_timed[MON_TMD_FAST] == 1)
		{
			mon_clear_timed(m_idx, MON_TMD_FAST, MON_TMD_FLG_NOTIFY);
		}

		else mon_dec_timed(m_idx, MON_TMD_FAST, 1 , MON_TMD_FLG_NOMESSAGE);

	}

	/*
	 * Handle slow counter
	 */
	if (m_ptr->m_timed[MON_TMD_SLOW])
	{
		if (m_ptr->m_timed[MON_TMD_SLOW] == 1)
		{
			mon_clear_timed(m_idx, MON_TMD_SLOW, MON_TMD_FLG_NOTIFY);
		}

		else mon_dec_timed(m_idx, MON_TMD_SLOW, 1 , MON_TMD_FLG_NOMESSAGE);
	}

	/* Hack -- Update the health and mana bar (always) */
	if ((p_ptr->health_who == m_idx)  || (m_ptr->sidebar))
		p_ptr->redraw |= (PR_HEALTH | PR_MON_MANA);

}


/*
  * Sorting hook -- comp function -- array of movement moments.
  */
static bool ang_sort_comp_hook_moment(const void *u, const void *v, int a, int b)
{
	move_moment_type *mm = (move_moment_type*)(u);

	/* Unused parameter */
	(void)v;

	/* Sort by moment in increasing order */
	return (mm[a].moment <= mm[b].moment);
}


/*
 * Sorting hook -- swap function -- array of movement moments.

 */
static void ang_sort_swap_hook_moment(void *u, void *v, int a, int b)
{
	move_moment_type *mm = (move_moment_type*)(u);

	move_moment_type temp_moment;

	/* Unused parameter */
	(void)v;

	/* Swap records */
	COPY(&temp_moment, &mm[a], move_moment_type);
	COPY(&mm[a], &mm[b], move_moment_type);
	COPY(&mm[b], &temp_moment, move_moment_type);
}


/*
  * Process monsters, the character, and other entities.  -LM-
  *
  * Give the character energy.  If the character has >= 100 energy,
  * store character index for later movement.
  *
  * Every ten game turns, allow monsters to recover from temporary con-
  * ditions.  Every 100 game turns, regenerate monsters.  Give energy to
  * each monster, store monster index of all monster with >= energy for
  * later movement.
  *
  * All entities that move this turn are sorted by "movement moment",
  * the exact instant within the course of a game turn in which the
  * entity has exactly 100 energy, and may move.  Lower movement moments
  * take priority.
  */
void process_entities(void)
{
	int i;
	int energy_per_turn, old_energy, moment;
	int idx;

	u16b dummy = 0;

	monster_type *m_ptr;

	/* Clear the moment array */
	move_moment_num = 0;

	/* Give the character some energy (unless leaving) */
	if (!p_ptr->leaving)
	{
		byte energy_gain = calc_energy_gain(p_ptr->state.p_speed);

		/* Give character energy */
		p_ptr->p_energy += energy_gain;

		/* Can the character move? */
		if (p_ptr->p_energy >= ENERGY_TO_MOVE)
		{
			/* Determine how much energy the character gets per turn */
			energy_per_turn = energy_gain;

			/* Note how much energy the character had last turn */
			old_energy = p_ptr->p_energy - energy_per_turn;

			/* Calculate movement moment - Hugo Kornelis - */
			moment = 100 * (ENERGY_TO_MOVE - old_energy) / (energy_per_turn);

			/* Insert character into movement table */
			mon_moment_info[move_moment_num].m_idx = -1;
			mon_moment_info[move_moment_num++].moment = moment;

		}
	}

	/* Process the monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		energy_per_turn = calc_energy_gain(m_ptr->m_speed);

		/* Give this monster some energy */
		m_ptr->m_energy += energy_per_turn;

		/* Ignore monsters with less than 100 energy */
		if (m_ptr->m_energy < ENERGY_TO_MOVE) continue;

		/* Handle temporary monster attributes and regeneration */
		recover_monster(m_ptr);

		/* Insert monster into the movement moment table */
		mon_moment_info[move_moment_num].m_idx = i;

		/* Note how much energy the monster had last turn */
		old_energy = m_ptr->m_energy - energy_per_turn;

		/* Calculate movement moment - Hugo Kornelis - */
		moment = 100 * (ENERGY_TO_MOVE - old_energy) / (energy_per_turn);

		/* Save it, go to next slot */
		mon_moment_info[move_moment_num++].moment = moment;

	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook_moment;
	ang_sort_swap = ang_sort_swap_hook_moment;

	/* Sort the movement table by decreasing movement moment*/
	ang_sort(mon_moment_info, &dummy, move_moment_num);

	/* Process monsters and the character, in order of priority */
	for (i = 0; i < move_moment_num; i++)
	{
		/* Get next entity index*/
		idx = mon_moment_info[i].m_idx;

		/* This is a monster */
		if (idx > 0)
		{
			/* Character is dead or leaving the current level */
			if (p_ptr->leaving) continue;

			/* Get the monster race */
			m_ptr = &mon_list[idx];

			/* Paranoia - Ignore dead monsters */
			if (!m_ptr->r_idx) continue;

			/*sleeping monsters don't get a move*/
			if (m_ptr->m_timed[MON_TMD_SLEEP])
			{

				/*Burn some energy and continue*/
				if (m_ptr->m_energy >= ENERGY_TO_MOVE) m_ptr->m_energy -= BASE_ENERGY_MOVE;

				continue;
			}

			/* Require that monster still have at least 100 energy */
			if (m_ptr->m_energy >= ENERGY_TO_MOVE)
			{
				/* Monster takes a turn */
				m_ptr->m_energy -= process_monster(m_ptr);
			}

		}

		/* This is the character */
		else if (idx < 0)
		{
			/* Can the character move? */
			while (p_ptr->p_energy >= ENERGY_TO_MOVE && !p_ptr->leaving)
			{
				/* Let the character take a turn */
				process_player();
			}
		}
	}

}


