/* File: melee1.c */

/*
 * Cofxright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this cofxright and statement
 * are included in all such copies.  Other cofxrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam)
{
	int max = 0;
	int total = dice * sides;

	/* Must do at least 95% of perfect */
	if (dam < total * 19 / 20) return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (rand_int(100) >= dam)) return (0);

	/* Perfect damage */
	if (dam == total) max++;

	/* Super-charge */
	if (dam >= 20)
	{
		while (rand_int(100) < 2) max++;
	}

	/* Critical damage */
	if (dam > 45) return (6 + max);
	if (dam > 33) return (5 + max);
	if (dam > 25) return (4 + max);
	if (dam > 18) return (3 + max);
	if (dam > 11) return (2 + max);
	return (1 + max);
}





/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level)
{
	int i, k, ac;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Some rooms make the player vulnerible */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if(room_info[dun_room[by][bx]].flags & (ROOM_CURSED))
		{
			/* Halve the effective armor class */
			ac /=2;
		}
	}							    


	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ((ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
	"insults you!",
	"insults your mother!",
	"gives you the finger!",
	"humiliates you!",
	"defiles you!",
	"dances around you!",
	"makes obscene gestures!",
	"moons you!!!"
};



/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[] =
{
	"seems sad about something.",
	"asks if you have seen his dogs.",
	"tells you to get off his land.",
	"mumbles something about mushrooms."
};


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int tmp, ac, rlev;
	int do_cut, do_stun;

	char m_name[80];

	char ddesc[80];

	bool blinked;


	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);


	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a goblin") */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;


		/* Hack -- no more attacks */
		if (!method) break;


		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Skip 'tricky' attacks */
		if ((method == RBM_SHOOT) ||
		       (method == RBM_TRAP) ||
			(method == RBM_AURA)) continue;

		/* Extract the attack "power" */
		switch (effect)
		{
			case GF_HURT: power = 60; break;
			case GF_POIS:	power =  5; break;
			case GF_UN_BONUS:	power = 20; break;
			case GF_UN_POWER:	power = 15; break;
			case GF_EAT_GOLD:	power =  5; break;
			case GF_EAT_ITEM:	power =  5; break;
			case GF_EAT_FOOD:	power =  5; break;
			case GF_EAT_LITE:	power =  5; break;
			case GF_ACID:		power =  0; break;
			case GF_ELEC:		power = 10; break;
			case GF_FIRE:		power = 10; break;
			case GF_COLD:		power = 10; break;
			case GF_BLIND:		power =  2; break;
			case GF_CONFUSION:	power = 10; break;
			case GF_TERRIFY:	power = 10; break;
			case GF_PARALYZE:	power =  2; break;
			case GF_LOSE_STR:	power =  0; break;
			case GF_LOSE_DEX:	power =  0; break;
			case GF_LOSE_CON:	power =  0; break;
			case GF_LOSE_INT:	power =  0; break;
			case GF_LOSE_WIS:	power =  0; break;
			case GF_LOSE_CHR:	power =  0; break;
			case GF_LOSE_ALL:	power =  2; break;
			case GF_SHATTER:	power = 60; break;
			case GF_EXP_10:	power =  5; break;
			case GF_EXP_20:	power =  5; break;
			case GF_EXP_40:	power =  5; break;
			case GF_EXP_80:	power =  5; break;
			case GF_HALLU:     power = 10; break;

			/* Need to add extra flavours in here */
		}

		/* Monster hits player */
		if (!effect || check_hit(power, rlev))
		{
			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}


			/* Assume no cut or stun */
			do_cut = do_stun = 0;

			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
					act = "hits you.";
					do_cut = do_stun = 1;
					break;
				}

				case RBM_TOUCH:
				{
					act = "touches you.";
					break;
				}

				case RBM_PUNCH:
				{
					act = "punches you.";
					do_stun = 1;
					break;
				}

				case RBM_KICK:
				{
					act = "kicks you.";
					do_stun = 1;
					break;
				}

				case RBM_CLAW:
				{
					act = "claws you.";
					do_cut = 1;
					break;
				}

				case RBM_BITE:
				{
					act = "bites you.";
					do_cut = 1;
					break;
				}

				case RBM_STING:
				{
					act = "stings you.";
					break;
				}

				case RBM_VOMIT:
				{
					act = "vomits on you.";
					break;
				}

				case RBM_BUTT:
				{
					act = "butts you.";
					do_stun = 1;
					break;
				}

				case RBM_CRUSH:
				{
					act = "crushes you.";
					do_stun = 1;
					break;
				}

				case RBM_ENGULF:
				{
					act = "engulfs you.";
					break;
				}

				case RBM_XXX2:
				{
					act = "XXX2's you.";
					break;
				}

				case RBM_CRAWL:
				{
					act = "crawls on you.";
					break;
				}

				case RBM_DROOL:
				{
					act = "drools on you.";
					break;
				}

				case RBM_SPIT:
				{
					act = "spits on you.";
					break;
				}

				case RBM_XXX3:
				{
					act = "XXX3's on you.";
					break;
				}

				case RBM_GAZE:
				{
					act = "gazes at you.";
					break;
				}

				case RBM_WAIL:
				{
					act = "wails at you.";
					break;
				}

				case RBM_SPORE:
				{
					act = "releases spores at you.";
					break;
				}

				case RBM_XXX4:
				{
					act = "projects XXX4's at you.";
					break;
				}

				case RBM_BEG:
				{
					act = "begs you for money.";
					break;
				}

				case RBM_INSULT:
				{
					act = desc_insult[rand_int(8)];
					break;
				}

				case RBM_MOAN:
				{
					act = desc_moan[rand_int(4)];
					break;
				}

				case RBM_XXX5:
				{
					act = "XXX5's you.";
					break;
				}
			}

			/* Message */
			if (act) msg_format("%^s %s", m_name, act);


			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

			/* Check for usage */
			if (rand_int(100)<damage)
			{
				int slot;

				/* Pick a (possibly empty) inventory slot */
				switch (randint(6))
				{
					case 1: slot = INVEN_BODY; break;
					case 2: slot = INVEN_ARM; break;
					case 3: slot = INVEN_OUTER; break;
					case 4: slot = INVEN_HANDS; break;
					case 5: slot = INVEN_HEAD; break;
					case 6: slot = INVEN_FEET; break;
				}

				/* Object used? */
				object_usage(INVEN_WIELD);
			}
			/* New result routine */
			project_p(m_idx,0,p_ptr->py,p_ptr->px,damage,effect);

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (rand_int(100) < 50)
				{
					do_cut = 0;
				}

				/* Cancel stun */
				else
				{
					do_stun = 0;
				}
			}

			/* Handle cut */
			if (do_cut)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(5) + 5; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(50) + 50; break;
					case 5: k = randint(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)set_cut(p_ptr->cut + k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(10) + 10; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(30) + 30; break;
					case 5: k = randint(40) + 40; break;
					case 6: k = 100; break;
					default: k = 200; break;
				}

				/* Apply the stun */
				if (k) (void)set_stun(p_ptr->stun + k);
			}
		}

		/* Monster missed player */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
				case RBM_HIT:
				case RBM_TOUCH:
				case RBM_PUNCH:
				case RBM_KICK:
				case RBM_CLAW:
				case RBM_BITE:
				case RBM_STING:
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:
				case RBM_XXX2:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1, 0);

					/* Message */
					msg_format("%^s misses you.", m_name);
				}

				break;
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}
	}


	/* Blink away */
	if (blinked)
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}


	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}

#if 0

/*
 * The running algorithm  -CJS-
 *
 * Modified for monsters. This lets them navigate corridors 'quickly'
 * and correctly turn corners. We use the monster run algorithm to
 * move now.
 *
 * We should be careful not to to let a monster run past the player
 * unless they are fleeing.
 *
 * We should be careful to 'disturb' the monster if they are in a room
 * and the player becomes visible or moves while visible.
 *
 * Because monsters are effectively running, we should be careful to
 * update the run parameters if they are 'staggering', and not let
 * them run while confused.
 *
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #		  #
 *       #x#		 @x#
 *       @p.		  p
 */
static void mon_run_init(int dir)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;

	/* Save the direction */
	m_ptr->run_cur_dir = dir;

	/* Assume running straight */
	m_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	m_ptr->mflag |= MFLAG_RUN_OPEN_AREA;

	/* Assume not looking for breaks */
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_RIGHT);
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_LEFT);

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = fy + ddy[dir];
	col = fx + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		deepright = TRUE;
	}

	/* Looking for a break */
	if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)) && (m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
	{
		/* Not looking for open area */
		m_ptr->mflag &= ~(MFLAG_RUN_OPEN_AREA);

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				m_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				m_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				m_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				m_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max;
	int option, option2;

	int feat;

	bool notice;

	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = m_ptr->run_old_dir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = fy + ddy[new_dir];
		col = fx + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			return (TRUE);
		}

		/* Analyze unknown grids and floors */
		if (cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				if (i < 0)
				{
					/* Break to the right */
					m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
				}

				else if (i > 0)
				{
					/* Break to the left */
					m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
				}
			}
		}
	}


	/* Looking for open area */
	if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA)run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))) )
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}


			/* Obstacle */
			else
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))))
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* No other options */
			m_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			m_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = fy + ddy[option];
			col = fx + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					m_ptr->run_cur_dir = option;
					m_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				m_ptr->run_cur_dir = option2;
				m_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				m_ptr->run_cur_dir = option;
				m_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(m_ptr->run_cur_dir, fy, fx))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void mon_run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Done */
			return;
		}
	}

}


#endif
