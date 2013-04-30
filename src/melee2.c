/* File: melee2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * UnAngband (c) 2001 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeve Sward).
 *
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.  This will require
 * much less space, if I am not mistaken.  Thus, each monster gets a
 * set of 32 bit flags, "smart", build from the various "SM_*" flags.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart" field is always up to date,
 * while "smart_learn" means that the "smart" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 */



/*
 * Monsters will run up to 25 grids away
 */
#define FLEE_RANGE      MAX_SIGHT + 5

/*
 * Terrified monsters will turn to fight if they are slower than the
 * character, and closer to him than this distance.
 */
#define TURN_RANGE      3



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
	if (m_ptr->monfear) m_ptr->min_range = FLEE_RANGE;

	else
	{

		/* Minimum distance - stay at least this far if possible */
		m_ptr->min_range=1;

		/* Examine player power (level) */
		p_lev = p_ptr->lev;

		/* Examine monster power (level plus morale) */
		m_lev = r_ptr->level + (cave_m_idx[m_ptr->fy][m_ptr->fx] & 0x08) + 25;

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
			if (p_val * m_mhp > m_val * p_mhp) m_ptr->min_range = FLEE_RANGE;
		}
	}

	if (m_ptr->min_range < FLEE_RANGE)
	{
		/* Creatures that don't move never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_MOVE)) m_ptr->min_range += 3;

		/* Spellcasters that don't stike never like to get too close */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) m_ptr->min_range += 3;
	}

	/* Maximum range to flee to (reduced elsewhere for themed levels */
	if (!(m_ptr->min_range < FLEE_RANGE)) m_ptr->min_range = FLEE_RANGE;

	/* Nearby monsters that cannot run away will not become run unless
	 * completely afraid */
	else if ((m_ptr->cdis < TURN_RANGE) && (m_ptr->mspeed < p_ptr->pspeed))
		m_ptr->min_range = 1;

	/* Now find prefered range */
	m_ptr->best_range = m_ptr->min_range;

	if (r_ptr->freq_spell + r_ptr->freq_innate > 49)
	{
		m_ptr->best_range += 3;
	}
}


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

	/* Canines and Zephyer Hounds are amazing trackers */
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
	/* Now include ancient dragons */
	else if (strchr("AfkoQyHORTY", r_ptr->d_char))
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


#ifdef MONSTER_AI

/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
static bool summon_possible(int y1, int x1)
{
	int y, x;

	/* Start at the player's location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x) > 2) continue;

			/* Hack: no summon on glyph of warding */
			if (f_info[cave_feat[y][x]].flags1 & (FF1_GLYPH)) continue;

			/* Require empty floor grid in line of sight */
			if (cave_empty_bold(y, x) && los(y1, x1, y, x))
			{
				return (TRUE);
			}
		}
	}

	return FALSE;
}



/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 *
 * Then we should perhaps instead supply a flag to "projectable()".  XXX XXX
 */
static bool clean_shot(int y1, int x1, int y2, int x2)
{
	int y, x;

	int grid_n;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, PROJECT_STOP);

	/* Source and target the same */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!cave_floor_bold(y, x)) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	/* Assume okay */
	return (TRUE);
}

#endif /* MONSTER_AI */




/*
 * Offsets for the spell indices
 */
#define RF4_OFFSET 32 * 3
#define RF5_OFFSET 32 * 4
#define RF6_OFFSET 32 * 5


/*
 * Used by following routine to speed up results
 */
static bool rand_int_hack(int *roll_p, int less)
{
	int roll = (*roll_p);
	if (roll < less) return (TRUE);
	else
	{
		roll = (roll-less) * 100/(100-less);
		*roll_p = roll;
		return (FALSE);
	}
	
	
}

/*
 * Have a monster choose a spell to cast.
 *
 * Note that the monster's spell list has already had "useless" spells
 * (bolts that won't hit the player, summons without room, etc.) removed.
 * Perhaps that should be done by this function.
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * This function could be an efficiency bottleneck.
 */
static int choose_attack_spell(int m_idx, u32b f4, u32b f5, u32b f6)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4_mask = 0L;
	u32b f5_mask = 0L;
	u32b f6_mask = 0L;

	int num = 0;
	byte spells[96];

	int i, py = p_ptr->py, px = p_ptr->px;


#ifdef MONSTER_AI

	bool has_escape, has_attack, has_summon, has_tactic;
	bool has_annoy, has_haste, has_heal;


	/* Smart monsters restrict their spell choices. */
	if (smart_monsters && !(r_ptr->flags2 & (RF2_STUPID)))
	{

		int roll = rand_int(100);

		/* What have we got? */
		has_escape = ((f4 & (RF4_ESCAPE_MASK)) ||
			      (f5 & (RF5_ESCAPE_MASK)) ||
			      (f6 & (RF6_ESCAPE_MASK)));
		has_attack = ((f4 & (RF4_ATTACK_MASK)) ||
			      (f5 & (RF5_ATTACK_MASK)) ||
			      (f6 & (RF6_ATTACK_MASK)));
		has_summon = ((f4 & (RF4_SUMMON_MASK)) ||
			      (f5 & (RF5_SUMMON_MASK)) ||
			      (f6 & (RF6_SUMMON_MASK)));
		has_tactic = ((f4 & (RF4_TACTIC_MASK)) ||
			      (f5 & (RF5_TACTIC_MASK)) ||
			      (f6 & (RF6_TACTIC_MASK)));
		has_annoy = ((f4 & (RF4_ANNOY_MASK)) ||
			     (f5 & (RF5_ANNOY_MASK)) ||
			     (f6 & (RF6_ANNOY_MASK)));
		has_haste = ((f4 & (RF4_HASTE_MASK)) ||
			     (f5 & (RF5_HASTE_MASK)) ||
			     (f6 & (RF6_HASTE_MASK)));
		has_heal = ((f4 & (RF4_HEAL_MASK)) ||
			    (f5 & (RF5_HEAL_MASK)) ||
			    (f6 & (RF6_HEAL_MASK)));

		/*** Try to pick an appropriate spell type ***/

		/* Hurt badly or afraid, in line of sight, attempt to flee */
		if (has_escape && ((m_ptr->hp < m_ptr->maxhp / 4) || m_ptr->monfear) &&
			player_can_see_bold(m_ptr->fy,m_ptr->fx))
		{
			/* Choose escape spell */
			f4_mask = (RF4_ESCAPE_MASK);
			f5_mask = (RF5_ESCAPE_MASK);
			f6_mask = (RF6_ESCAPE_MASK);
		}

		/* Still hurt badly, couldn't flee, attempt to heal */
		else if (has_heal && m_ptr->hp < m_ptr->maxhp / 4)
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
		}

		/* Player is close and we have attack spells, blink away */
		else if (has_tactic && (distance(py, px, m_ptr->fy, m_ptr->fx) < 4) &&
			 has_attack && (rand_int_hack(&roll,75)))
		{
			/* Choose tactical spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
		}

		/* We're hurt (not badly), try to heal */
		else if (has_heal && (m_ptr->hp < m_ptr->maxhp * 3 / 4) &&
			 (!player_can_see_bold(m_ptr->fy,m_ptr->fx) || (rand_int_hack(&roll,60))))
		{
			/* Choose heal spell */
			f4_mask = (RF4_HEAL_MASK);
			f5_mask = (RF5_HEAL_MASK);
			f6_mask = (RF6_HEAL_MASK);
		}

		/* Summon if possible (sometimes) */
		else if (has_summon && (rand_int_hack(&roll,50)))
		{
			/* Choose summon spell */
			f4_mask = (RF4_SUMMON_MASK);
			f5_mask = (RF5_SUMMON_MASK);
			f6_mask = (RF6_SUMMON_MASK);
		}

		/* Attack spell (most of the time) */
		else if (has_attack && (rand_int_hack(&roll,85)))
		{
			/* Choose attack spell */
			f4_mask = (RF4_ATTACK_MASK);
			f5_mask = (RF5_ATTACK_MASK);
			f6_mask = (RF6_ATTACK_MASK);
		}

		/* Try another tactical spell (sometimes) */
		else if (has_tactic && (rand_int_hack(&roll,50)) &&
			!player_can_see_bold(m_ptr->fy,m_ptr->fx))
		{
			/* Choose tactic spell */
			f4_mask = (RF4_TACTIC_MASK);
			f5_mask = (RF5_TACTIC_MASK);
			f6_mask = (RF6_TACTIC_MASK);
		}

		/* Haste self if we aren't already somewhat hasted (rarely) */
		else if (has_haste && (rand_int_hack(&roll,(20 + r_ptr->speed - m_ptr->mspeed))))
		{
			/* Choose haste spell */
			f4_mask = (RF4_HASTE_MASK);
			f5_mask = (RF5_HASTE_MASK);
			f6_mask = (RF6_HASTE_MASK);
		}

		/* Annoy player (most of the time) */
		else if (has_annoy && (rand_int_hack(&roll,85)))
		{
			/* Choose annoyance spell */
			f4_mask = (RF4_ANNOY_MASK);
			f5_mask = (RF5_ANNOY_MASK);
			f6_mask = (RF6_ANNOY_MASK);
		}

		/* Else choose no spell (The masks default to this.) */

		/* Keep only the interesting spells */
		f4 &= f4_mask;
		f5 &= f5_mask;
		f6 &= f6_mask;

		/* Anything left? */
		if (!(f4 || f5 || f6)) return (0);
	}

#endif /* MONSTER_AI */

	/* Extract the "innate" spells */
	for (i = 0; i < 32; i++)
	{
		if (f4 & (1L << i)) spells[num++] = i + RF4_OFFSET;
	}

	/* Extract the "normal" spells */
	for (i = 0; i < 32; i++)
	{
		if (f5 & (1L << i)) spells[num++] = i + RF5_OFFSET;
	}

	/* Extract the "bizarre" spells */
	for (i = 0; i < 32; i++)
	{
		if (f6 & (1L << i)) spells[num++] = i + RF6_OFFSET;
	}

	/* Paranoia */
	if (num == 0) return 0;

	/* The monster is hidden */
	if (m_ptr->mflag & (MFLAG_HIDE))
	{

		/* We can't get out of hiding */
		if ((f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_HIDE_DEEP)) &&
			(place_monster_here(m_ptr->fy,m_ptr->fx,m_ptr->r_idx) == MM_WALK))
		{
			return (0);
		}

		/* Monster is under COVER. Bash up or fail to cast spell. */
		else if (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_COVERED))
		{

			if ((r_ptr->flags2 & (RF2_BASH_DOOR)) && (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags1 & (FF1_BASH)))
			{
				/* Bash up */
				cave_alter_feat(m_ptr->fy,m_ptr->fx,FS_BASH);

			}
			else
			{

				return (0);
			}

		}

		/* Reveal the monster */
		m_ptr->mflag &= ~(MFLAG_HIDE);

		/* And update */
		update_mon(m_idx,FALSE);	

		/* Hack --- tell the player if something unhides */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			msg_format("%^s emerges from %s%s.",m_name,
				((f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_FILLED))?"":"the "),
				f_name+f_info[cave_feat[m_ptr->fy][m_ptr->fx]].name);
		}

		/* Disturb on "move" */
		if (m_ptr->ml &&
		    (disturb_move ||
		     ((m_ptr->mflag & (MFLAG_VIEW)) &&
		      disturb_near)))
		{
			/* Disturb */
			disturb(0, 0);
		}

	}


	/* Pick at random */
	return (spells[rand_int(num)]);
}

/* We handle monsters casting spells at player, at each other,
   and being cast by traps.  Need to do so, much in the same way that
   we handle project functions */

bool make_attack_spell_aux(int who, int y, int x, int spell)
{

	monster_type *m_ptr,*n_ptr;
	monster_race *r_ptr,*s_ptr;
	monster_lore *l_ptr;

	char m_name[80];
	char m_poss[80];

	char t_name[80];
	char t_poss[80];

	char ddesc[80];

	int target = cave_m_idx[y][x];

	int flg,rad;

	int rlev;

	int i,k,count;

	bool blind;
	bool seen; /* Source monster seen */
	bool known; /* Either monster seen */
	bool normal;
	bool direct;

	/* Hack -- don't summon on surface */
	bool surface = p_ptr->depth == min_depth(p_ptr->dungeon);

	/* Reset */
	count = 0;

	if (target > 0)
	{
		n_ptr = &m_list[cave_m_idx[y][x]];
		s_ptr = &r_info[n_ptr->r_idx];

		/* Get the monster name (or "it") */
		monster_desc(t_name, n_ptr, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(t_poss, n_ptr, 0x22);
	}
	else if (target < 0)
	{
		n_ptr = &m_list[0];
		s_ptr = &r_info[0];

		strcpy(t_name,"you");
		strcpy(t_poss,"your");
	}
	else
	{
		n_ptr = &m_list[0];
		s_ptr = &r_info[0];

		strcpy(t_name,"it");
		strcpy(t_poss,"its");
	}
		
	if (who <= 0)
	{
		m_ptr = &m_list[0];
		l_ptr = &l_list[0];
		r_ptr = &r_info[0];

		strcpy(m_name,"it");
		strcpy(m_poss,"its");
		strcpy(ddesc,"a trap");

		/* Extract the blind-ness */
		blind = (p_ptr->blind ? TRUE : FALSE);

		seen = FALSE;

		/* Assume "normal" target */
		normal = (target < 0);

		/* Assume "projectable" */
		direct = TRUE;

		/* Check if known */
		known = player_can_see_bold(y,x);

		/* Fake the monster level */
		rlev = f_info[cave_feat[y][x]].power;
	}
	else
	{
		m_ptr = &m_list[who];
		l_ptr = &l_list[m_ptr->r_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Get the monster name (or "it") */
		monster_desc(m_name, m_ptr, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, m_ptr, 0x22);

		/* Hack -- Get the "died from" name */
		monster_desc(ddesc, m_ptr, 0x88);

		/* Extract the monster level */
		rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

		/* Extract the blind-ness */
		blind = (p_ptr->blind ? TRUE : FALSE);

		/* Extract the "see-able-ness" */
		seen = (!blind && m_ptr->ml);

		if (target > 0)
		{
			known = ((m_ptr->ml || n_ptr->ml));

			/* Not "normal" target */
			normal = FALSE;

			/* Check "projectable" */
			direct = (projectable(m_ptr->fy, m_ptr->fx, y, x));

		}
		else if (target < 0)
		{
			/* Always known if target */
			known = TRUE;

			/* Assume "normal" target */
			normal = TRUE;

			/* Check "projectable" */
			direct = (projectable(m_ptr->fy,m_ptr->fx,y,x));
		}
		else
		{
			known = (m_ptr->ml && player_can_see_bold(y,x));


			/* Always known if target */
			known = TRUE;

			/* Assume "normal" target */
			normal = TRUE;

			/* Check "projectable" */
			direct = (projectable(m_ptr->fy,m_ptr->fx,y,x));

		}
	}

	/* Cast the spell. */
	switch (spell)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			if (!direct) break;
			disturb(1, 0);
			msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(who);
			break;
		}

		/* RF4_SPORE */
		case 96+1:
		{
			if (!variant_dis_attacks) break;
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				bool obvious;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{

					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not spores */
					if (r_ptr->blow[i].method != (RBM_SPORE)) continue;

					/* Message */
					if (known) msg_format("%^s releases spores.", m_name);

					flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

					/* Hit with radiate attack */
					obvious = project(who, 2, m_ptr->fy, m_ptr->fx, damroll(r_ptr->blow[i].d_side, r_ptr->blow[i].d_dice),
						 r_ptr->blow[i].effect, flg);

					/* Analyze "visible" monsters only */
					if (seen)
					{			
						/* Count "obvious" attacks */
						if (obvious || (l_ptr->blows[i] > 10))
						{
							/* Count attacks of this type */
							if (l_ptr->blows[i] < MAX_UCHAR)
							{
								l_ptr->blows[i]++;
							}
						}
					}
				}
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Message */
				if (known) msg_format("%^s releases spores.", m_name);

				/* Hit with radiate attack */
				(void)project(who, 2, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side, f_info[cave_feat[y][x]].blow.d_dice),
					 f_info[cave_feat[y][x]].blow.effect, flg);
			}
			break;
		}

		/* RF4_GAZE */
		case 96+2:
		{
			if (!direct) break;
			if (!variant_dis_attacks) break;

			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				bool obvious;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{

					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not gaze */
					if (r_ptr->blow[i].method != (RBM_GAZE)) continue;

					/* Message */
					if (seen) msg_format("%^s gazes at %s.", m_name,t_name);

					if (target < 0)
					{

						/* Target the player */
						obvious = project_p(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);

						/* Analyze "visible" monsters only */
						if (seen)
						{
				
							/* Count "obvious" attacks */
							if (obvious || (l_ptr->blows[i] > 10))
							{
								/* Count attacks of this type */
								if (l_ptr->blows[i] < MAX_UCHAR)
								{
									l_ptr->blows[i]++;
								}
							}				
						}
					}
					else if (target > 0)
					{
						/* Target the monster */
						project_m(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							 r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);
					}
				}
			}
			else
			{
				/* Message */
				if (seen) msg_format("%^s gazes at %s.", m_name,t_name);

				if (target < 0)
				{

					/* Target the player */
					project_p(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect);
				}
				else if (target > 0)
				{
					/* Target the monster */
					project_m(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect);

				}				       
			}
			break;
		}

		/* RF4_WAIL */
		case 96+3:
		{
			if (!variant_dis_attacks) break;
			if (target < 0) disturb(1,0);
			

			if (who > 0)
			{
				bool obvious;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{
					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not spores */
					if (r_ptr->blow[i].method != (RBM_WAIL)) continue;

					/* Message */
					if (known) msg_format("%^s wails at %s.", m_name,t_name);

					if (target < 0)
					{

						/* Target the player */
						obvious = project_p(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);

						/* Analyze "visible" monsters only */
						if (seen)
						{
				
							/* Count "obvious" attacks */
							if (obvious || (l_ptr->blows[i] > 10))
							{
								/* Count attacks of this type */
								if (l_ptr->blows[i] < MAX_UCHAR)
								{
									l_ptr->blows[i]++;
								}
							}
				
						}
					}
					else if (target > 0)
					{
						/* Target the monster */
						project_m(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							 r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);
					}

				}
			}
			else
			{
				/* Message */
				if (known) msg_format("%^s wails at %s.", m_name,t_name);

				if (target < 0)
				{

					/* Target the player */
					project_p(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect);
				}
				else if (target > 0)
				{
					/* Target the monster */
					project_m(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect);

				}				       
					
			}
			break;
		}

		/* RF4_SPIT */
		case 96+4:
		{
			if (!variant_dis_attacks) break;

			if (target < 0) disturb(1,0);
			

			if (who > 0)
			{
				bool obvious;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{
					flg = PROJECT_STOP | PROJECT_KILL;

					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not spores */
					if (r_ptr->blow[i].method != (RBM_SPIT)) continue;

					/* Message */
					if (known) msg_format("%^s spits at %s.", m_name,t_name);

					/* Target the player with a bolt attack */
					obvious = project(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect, flg);

					/* Analyze "visible" monsters only */
					if (seen)
					{
			
						/* Count "obvious" attacks */
						if (obvious || (l_ptr->blows[i] > 10))
						{
							/* Count attacks of this type */
							if (l_ptr->blows[i] < MAX_UCHAR)
							{
								l_ptr->blows[i]++;
							}
						}			
					}			
				}
			}
			else
			{
				flg = PROJECT_STOP | PROJECT_KILL;

				/* Message */
				if (known) msg_format("%^s spits at %s.", m_name,t_name);

				/* Target the player with a bolt attack */
				(void)project(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect, flg);
					
			}
			break;
		}

		/* RF4_SHOOT */
		case 96+5:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				bool obvious;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{
					flg = PROJECT_STOP | PROJECT_KILL;


					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not shoots */
					if (r_ptr->blow[i].method != (RBM_SHOOT)) continue;

					/* Message */
					if ((blind) && (known)) msg_format("%^s makes a strange noise.", m_name);
					else if (target < 0) msg_format("%^s fires an arrow!", m_name);
					else if (known) msg_format("%^s fires an arrow at %s!", m_name, t_name);

					/* Target the player with a bolt attack */
					obvious = project(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect, flg);

					/* Analyze "visible" monsters only */
					if (seen)
					{
			
						/* Count "obvious" attacks */
						if (obvious || (l_ptr->blows[i] > 10))
						{
							/* Count attacks of this type */
							if (l_ptr->blows[i] < MAX_UCHAR)
							{
								l_ptr->blows[i]++;
							}
						}
			
					}
				}
			}
			else
			{
				flg = PROJECT_STOP | PROJECT_KILL;

				/* Message */
				if ((blind) && (known)) msg_format("%^s makes a strange noise.", m_name);
				else if (target < 0) msg_format("%^s fires an arrow!", m_name);
				else if (known) msg_format("%^s fires an arrow at %s!", m_name, t_name);		

				/* Target the player with a bolt attack */
				(void)project(who, 0, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side,
						f_info[cave_feat[y][x]].blow.d_dice),f_info[cave_feat[y][x]].blow.effect, flg);
			}
			break;
		}

		/* RF4_EXPLODE --- used by chests */
		case 96+6:
		{
			if (target < 0) disturb(1,0);

			if (known) msg_format("%^s explodes.", m_name);

			flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

			rad = 2;

			/* Centre on caster */
			if (who > 0)
			{
				y = m_ptr->fy;
				x = m_ptr->fx;
			}

			/* Target everyone (including caster) with a ball attack */
			(void)project(0, rad, y, x, damroll(5,8), GF_EXPLODE, flg);

			break;
		}

		/* RF4_AURA */
		case 96+7:
		{
			if (!variant_dis_attacks) break;
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				bool obvious;

				int rad;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1;

				/* Scan through all four blows */
				for (i = 0; i < 4; i++)
				{

					/* End of attacks */
					if (!(r_ptr->blow[i].method)) break;

					/* Skip if not spores */
					if (r_ptr->blow[i].method != (RBM_AURA)) continue;

					flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

					/* Hit with radiate attack */
					obvious = project(who, rad, m_ptr->fy, m_ptr->fx, damroll(r_ptr->blow[i].d_side, r_ptr->blow[i].d_dice),
						 r_ptr->blow[i].effect, flg);

					/* Analyze "visible" monsters only */
					if (seen)
					{
			
						/* Count "obvious" attacks */
						if (obvious || (l_ptr->blows[i] > 10))
						{
							/* Count attacks of this type */
							if (l_ptr->blows[i] < MAX_UCHAR)
							{
								l_ptr->blows[i]++;
							}
						}
					}
				}
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Hit with radiate attack */
				(void)project(who, 1, y, x, damroll(f_info[cave_feat[y][x]].blow.d_side, f_info[cave_feat[y][x]].blow.d_dice),
					 f_info[cave_feat[y][x]].blow.effect, flg);
					
			}
			break;
		}

		/* RF4_BR_ACID */
		case 96+8:
		{

			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;



				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes acid.", m_name);
				else if (known) msg_format("%^s breathes acid at %s.", m_name, t_name);


				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 1600 ? 1600
						: (m_ptr->hp / 3)), GF_ACID, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_ACID);
						
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 1600 ? 1600
						: (rlev * 20 / 3)), GF_ACID, flg);

			}
			break;
		}

		/* RF4_BR_ELEC */
		case 96+9:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{

				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes lightening.", m_name);
				else if (known) msg_format("%^s breathes lightening at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 1600 ? 1600
						: (m_ptr->hp / 3)), GF_ELEC, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_ELEC);
						
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 1600 ? 1600
						: (rlev * 20 / 3)), GF_ELEC, flg);

			}
			break;
		}

		/* RF4_BR_FIRE */
		case 96+10:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes fire.", m_name);
				else if (known) msg_format("%^s breathes fire at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 1600 ? 1600
						: (m_ptr->hp / 3)), GF_FIRE, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_FIRE);
						
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 1600 ? 1600
						: (rlev * 20 / 3)), GF_FIRE, flg);

			}
			break;
		}

		/* RF4_BR_COLD */
		case 96+11:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes frost.", m_name);
				else if (known) msg_format("%^s breathes frost at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 1600 ? 1600
						: (m_ptr->hp / 3)), GF_COLD, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_COLD);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 1600 ? 1600
						: (rlev * 20 / 3)), GF_COLD, flg);

			}
			break;
		}


		/* RF4_BR_POIS */
		case 96+12:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes gas.", m_name);
				else if (known) msg_format("%^s breathes gas at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 800 ? 800
						: (m_ptr->hp / 3)), GF_POIS, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_POIS);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 800 ? 800
						: (rlev * 20 / 3)), GF_POIS, flg);

			}
			break;
		}

		/* RF4_BR_NETH */
		case 96+13:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes nether.", m_name);
				else if (known) msg_format("%^s breathes nether at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 550 ? 550
						: (m_ptr->hp / 6)), GF_ACID, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_NETHR);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 550 ? 550
						: (rlev * 20 / 6)), GF_NETHER, flg);

			}
			break;
		}

		/* RF4_BR_LITE */
		case 96+14:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes light.", m_name);
				else if (known) msg_format("%^s breathes light at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 400 ? 400
						: (m_ptr->hp / 6)), GF_LITE, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_LITE);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 400 ? 400
						: (rlev * 20 / 6)), GF_LITE, flg);

			}
			break;
		}

		/* RF4_BR_DARK */
		case 96+15:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes darkness.", m_name);
				else if (known) msg_format("%^s breathes darkness at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 400 ? 400
						: (m_ptr->hp / 6)), GF_DARK, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_DARK);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 400 ? 400
						: (rlev * 20 / 6)), GF_DARK, flg);

			}
			break;

		}

		/* RF4_BR_CONF */
		case 96+16:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes confusion.", m_name);
				else if (known) msg_format("%^s breathes confusion at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 400 ? 400
						: (m_ptr->hp / 6)), GF_CONFUSION, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_CONFU);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 400 ? 400
						: (rlev * 20 / 6)), GF_CONFUSION, flg);

			}
			break;

		}

		/* RF4_BR_SOUN */
		case 96+17:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes sound.", m_name);
				else if (known) msg_format("%^s breathes sound at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 400 ? 400
						: (m_ptr->hp / 6)), GF_SOUND, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_SOUND);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 400 ? 400
						: (rlev * 20 / 6)), GF_SOUND, flg);

			}
			break;
		}

		/* RF4_BR_CHAO */
		case 96+18:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes chaos.", m_name);
				else if (known) msg_format("%^s breathes chaos at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 600 ? 600
						: (m_ptr->hp / 6)), GF_CHAOS, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_CHAOS);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 600 ? 600
						: (rlev * 20 / 6)), GF_CHAOS, flg);

			}
			break;
		}

		/* RF4_BR_DISE */
		case 96+19:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes disenchantment.", m_name);
				else if (known) msg_format("%^s breathes disenchantment at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 500 ? 500
						: (m_ptr->hp / 6)), GF_DISENCHANT, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_DISEN);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 500 ? 500
						: (rlev * 20 / 6)), GF_DISENCHANT, flg);

			}
			break;
		}

		/* RF4_BR_NEXU */
		case 96+20:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes nexus.", m_name);
				else if (known) msg_format("%^s breathes nexus at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 250 ? 250
						: (m_ptr->hp / 3)), GF_NEXUS, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_NEXUS);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 250 ? 250
						: (rlev * 20 / 3)), GF_NEXUS, flg);

			}
			break;
		}

		/* RF4_BR_TIME */
		case 96+21:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes time.", m_name);
				else if (known) msg_format("%^s breathes time at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 150 ? 150
						: (m_ptr->hp / 3)), GF_TIME, flg);
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 150 ? 150
						: (rlev * 20 / 3)), GF_TIME, flg);

			}
			break;
		}

		/* RF4_BR_INER */
		case 96+22:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes inertia.", m_name);
				else if (known) msg_format("%^s breathes inertia at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 200 ? 200
						: (m_ptr->hp / 6)), GF_INERTIA, flg);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 200 ? 200
						: (rlev * 20 / 6)), GF_INERTIA, flg);

			}
			break;
		}

		/* RF4_BR_GRAV */
		case 96+23:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes gravity.", m_name);
				else if (known) msg_format("%^s breathes gravity at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 3) > 200 ? 200
						: (m_ptr->hp / 3)), GF_GRAVITY, flg);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 3) > 200 ? 200
						: (rlev * 20 / 3)), GF_GRAVITY, flg);

			}
			break;
		}

		/* RF4_BR_SHAR */
		case 96+24:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes shards.", m_name);
				else if (known) msg_format("%^s breathes shards at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 400 ? 400
						: (m_ptr->hp / 6)), GF_SHARD, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_SHARD);					
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 400 ? 400
						: (rlev * 20 / 6)), GF_SHARD, flg);

			}
			break;

		}

		/* RF4_BR_PLAS */
		case 96+25:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes plasma.", m_name);
				else if (known) msg_format("%^s breathes plasma at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 150 ? 150
						: (m_ptr->hp / 6)), GF_PLASMA, flg);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 150 ? 150
						: (rlev * 20 / 6)), GF_PLASMA, flg);

			}
			break;

		}

		/* RF4_BR_WALL */
		case 96+26:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s breathes at %s.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s breathes force.", m_name);
				else if (known) msg_format("%^s breathes force at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((m_ptr->hp / 6) > 200 ? 200
						: (m_ptr->hp / 6)), GF_FORCE, flg);
						
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, ((rlev * 20 / 6) > 200 ? 200
						: (rlev * 20 / 6)), GF_FORCE, flg);

			}
			break;
		}

		/* RF4_BR_MANA */
		case 96+27:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF4_XXX5X4 */
		case 96+28:
		{
			break;
		}

		/* RF4_XXX6X4 */
		case 96+29:
		{
			break;
		}

		/* RF4_XXX7X4 */
		case 96+30:
		{
			break;
		}

		/* RF4_BOULDER */
		case 96+31:
		{
			if (target < 0) disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s grunts with exertion.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s hurls a boulder.", m_name);
				else if (known) msg_format("%^s hurls a boulder at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(1 + rlev / 7, 12) , GF_ARROW, flg);
			}
			else
			{
				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(1 + rlev / 7, 12), GF_ARROW, flg);
			}			break;
		}


		/* RF5_BA_ACID */
		case 128+0:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts an acid ball.", m_name);
				else if (known) msg_format("%^s casts an acid ball at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 / 2) + 8, GF_ACID, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_ACID);  
			}
			else
			{
				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 /2) + 8, GF_ACID, flg);

			}
			break;

		}

		/* RF5_BA_ELEC */
		case 128+1:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a lightening ball.", m_name);
				else if (known) msg_format("%^s casts a lightening ball at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 / 2) + 8, GF_ELEC, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_ELEC);  
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 /2) + 8, GF_ELEC, flg);

			}
			break;

		}

		/* RF5_BA_FIRE */
		case 128+2:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a fire ball.", m_name);
				else if (known) msg_format("%^s casts a fire ball at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 7 / 2) + 10, GF_FIRE, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_FIRE);  
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 7 /2) + 10, GF_FIRE, flg);

			}
			break;

		}

		/* RF5_BA_COLD */
		case 128+3:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a frost ball.", m_name);
				else if (known) msg_format("%^s casts a frost ball at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 / 2) + 10, GF_COLD, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_COLD);  
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 3 / 2) + 10, GF_COLD, flg);

			}
			break;

		}

		/* RF5_BA_POIS */
		case 128+4:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a stinking cloud.", m_name);
				else if (known) msg_format("%^s casts a stinking cloud at %s.", m_name, t_name);

				flg = PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, damroll(12,2), GF_POIS, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_POIS);  
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, damroll(12,2), GF_POIS, flg);

			}
			break;
		}

		/* RF5_BA_NETH */
		case 128+5:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a nether ball.", m_name);
				else if (known) msg_format("%^s casts a nether ball at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (50 + damroll(10,10) + rlev), GF_NETHER, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_NETHR);	
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (50 + damroll(10,10) + rlev), GF_NETHER, flg);

			}
			break;
		}

		/* RF5_BA_WATE */
		case 128+6:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s gestures fluidly.", m_name);
				else if (known) msg_format("%^s gestures fluidly at %s.", m_name, t_name);
				else if (target < 0) msg_print("You are engulfed in a whirlpool.");

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 5 / 2) + 50, GF_WATER, flg);      
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, randint(rlev * 5 / 2) + 50, GF_WATER, flg);

			}
			break;
		}

		/* RF5_BA_MANA */
		case 128+7:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles powerfully.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s invokes a mana storm.", m_name);
				else if (known) msg_format("%^s invokes a mana storm at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (rlev * 5) + damroll(10,10), GF_MANA, flg);      
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (rlev * 5) + damroll(10,10), GF_MANA, flg);

			}
			break;
		}

		/* RF5_BA_DARK */
		case 128+8:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles powerfully.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s invokes a darkness storm.", m_name);
				else if (known) msg_format("%^s invokes a darkness storm at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (rlev * 5) + damroll(10,10), GF_DARK, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_DARK);
			}
			else
			{
				

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

				/* Determine the radius of the blast */
				rad = 2;

				/* Target the player with a ball attack */
				(void)project(who, rad, y, x, (rlev * 5) + damroll(10,10), GF_DARK, flg);

			}
			break;
		}

		/* RF5_DRAIN_MANA */
		case 128+9:
		{
			if (!direct) break;

			if (target >= 0) break;

			if (p_ptr->csp)
			{
				int r1;

				/* Disturb if legal */
				disturb(1, 0);

				/* Basic message */
				msg_format("%^s draws psychic energy from you!", m_name);

				/* Attack power */
				r1 = (randint(rlev) / 2) + 1;

				/* Full drain */
				if (r1 >= p_ptr->csp)
				{
					r1 = p_ptr->csp;
					p_ptr->csp = 0;
					p_ptr->csp_frac = 0;
				}

				/* Partial drain */
				else
				{
					p_ptr->csp -= r1;
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

				/* Heal the monster */
				if ((who > 0) && (m_ptr->hp < m_ptr->maxhp))
				{
					/* Heal */
					m_ptr->hp += (6 * r1);
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			if (who > 0) update_smart_learn(who, DRS_MANA);
			break;
		}

		/* RF5_MIND_BLAST */
		case 128+10:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if ((who > 0) && (!seen))
			{
				if (target < 0) msg_print("You feel something focusing on your mind.");
			}
			else if (who > 0)
			{
				msg_format("%^s gazes deep into %s eyes.", m_name, t_poss);
			}


			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					msg_print("Your mind is blasted by psionic energy.");
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_int(4) + 4);
					}
					take_hit(damroll(8, 8), ddesc);
				}
			}
			else if (target > 0)
			{

				if (!(r_ptr->flags2 & (RF2_EMPTY_MIND)))
				{
					if (known) msg_format ("&^s mind is blasted by psionic energy.",t_poss);

					/* Hack --- Use GF_CONFUSION */
					project_m(who, 0, y, x, damroll(8,8), GF_CONFUSION);
				}
			}

			break;
		}

		/* RF5_BRAIN_SMASH */
		case 128+11:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if ((who > 0) && (!seen))
			{
				if (target < 0) msg_print("You feel something focusing on your mind.");
			}
			else if (who > 0) 
			{
				msg_format("%^s looks deep into %s eyes.", m_name, t_poss);
			}

			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					msg_print("Your mind is blasted by psionic energy.");
					take_hit(damroll(12, 15), ddesc);
					if (!p_ptr->resist_blind)
					{
						(void)set_blind(p_ptr->blind + 8 + rand_int(8));

						/* Always notice */
						equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);
					}
					else
					{
						/* Always notice */
						equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
					}
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_int(4) + 4);

						/* Always notice */
						equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);
					}
					else
					{
						/* Always notice */
						equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
					}
					if (!p_ptr->free_act)
					{
						(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);

						/* Always notice */
						equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
					}
					else
					{
						/* Always notice */
						equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
					}
					(void)set_slow(p_ptr->slow + rand_int(4) + 4);
				}
				break;
			}
			else if (target > 0)
			{
				if (!(r_ptr->flags2 & (RF2_EMPTY_MIND)))
				{
					if (known) msg_format ("&^s mind is blasted by psionic energy.",t_poss);

					/* Hack --- Use GF_CONFUSION */
					project_m(who, 0, y, x, damroll(12,15), GF_CONFUSION);
				}

			}

		}
		/* RF5_CAUSE_1 */
		case 128+12:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s points at %s and curses.", m_name, t_name);
			}
			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					take_hit(damroll(3, 8), ddesc);
				}
			}
			else if (target > 0)
			{
					/* Hack --- Use GF_OLD_DRAIN */
					project_m(who, 0, y, x, damroll(3,8), GF_OLD_DRAIN);    
			}
			break;
		}

		/* RF5_CAUSE_2 */
		case 128+13:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s points at %s and curses horribly.", m_name, t_name);
			}
			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					take_hit(damroll(8, 8), ddesc);
				}
			}
			else if (target > 0)
			{
					/* Hack --- Use GF_OLD_DRAIN */
					project_m(who, 0, y, x, damroll(8,8), GF_OLD_DRAIN);    
			}
			break;
		}

		/* RF5_CAUSE_3 */
		case 128+14:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s points at %s, incanting terribly.", m_name, t_name);
			}
			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					take_hit(damroll(10, 15), ddesc);
				}
			}
			else if (target > 0)
			{
					/* Hack --- Use GF_OLD_DRAIN */
					project_m(who, 0, y, x, damroll(10,15), GF_OLD_DRAIN);  
			}
			break;
		}

		/* RF5_CAUSE_4 */
		case 128+15:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s points at %s, screaming the word DIE!.", m_name, t_name);
			}
			if (target < 0)
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				(void)set_cut(p_ptr->cut + damroll(10, 10));
				}
				else
				{
					take_hit(damroll(15, 15), ddesc);
				}
			}
			else if (target > 0)
			{
					/* Hack --- Use GF_OLD_DRAIN */
					project_m(who, 0, y, x, damroll(15,15), GF_OLD_DRAIN);  
			}
			break;
		}

		/* RF5_BO_ACID */
		case 128+16:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts an acid bolt.", m_name);
				else if (known) msg_format("%^s casts an acid bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(7, 8) + (rlev /3 ), GF_ACID, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_ACID);  
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(7, 8) + (rlev /3 ), GF_ACID, flg);
			}
			break;
		}

		/* RF5_BO_ELEC */
		case 128+17:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a lightening bolt.", m_name);
				else if (known) msg_format("%^s casts a lightening bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(4, 8) + (rlev /3 ), GF_ELEC, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_ELEC);  
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(4, 8) + (rlev /3 ), GF_ELEC, flg);
			}
			break;
		}

		/* RF5_BO_FIRE */
		case 128+18:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a fire bolt.", m_name);
				else if (known) msg_format("%^s casts a fire bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(9, 8) + (rlev /3 ), GF_FIRE, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_FIRE);  
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(9, 8) + (rlev /3 ), GF_FIRE, flg);
			}
			break;

		}

		/* RF5_BO_COLD */
		case 128+19:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a frost bolt.", m_name);
				else if (known) msg_format("%^s casts a frost bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(6, 8) + (rlev /3 ), GF_COLD, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_COLD);  
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(6, 8) + (rlev /3 ), GF_COLD, flg);
			}
		}

		/* RF5_BO_POIS */
		case 128+20:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF5_BO_NETH */
		case 128+21:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a nether bolt.", m_name);
				else if (known) msg_format("%^s casts a nether bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, 30 + damroll(5, 5) + (rlev * 3 /2), GF_NETHER, flg);

				if (target < 0) update_smart_learn(who,DRS_RES_NETHR);	
			}
			else
			{
				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, 30 + damroll(5, 5) + (rlev * 3 /2), GF_NETHER, flg);
			}
			break;
		}

		/* RF5_BO_WATE */
		case 128+22:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a water bolt.", m_name);
				else if (known) msg_format("%^s casts a water bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(10, 10) + (rlev), GF_WATER, flg);
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, 30 + damroll(10, 10) + (rlev), GF_WATER, flg);
			}
			break;
		}

		/* RF5_BO_MANA */
		case 128+23:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a mana bolt.", m_name);
				else if (known) msg_format("%^s casts a mana bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, (rlev * 7 / 2) + 50, GF_MANA, flg);
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, (rlev * 7 / 2) + 50, GF_MANA, flg);
			}
			break;
		}

		/* RF5_BO_PLAS */
		case 128+24:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a plasma bolt.", m_name);
				else if (known) msg_format("%^s casts a plasma bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, 10 + damroll(8,7) + (rlev), GF_PLASMA, flg);
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, 10 + damroll(8,7) + (rlev), GF_PLASMA, flg);
			}
			break;
		}

		/* RF5_BO_ICEE */
		case 128+25:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a plasma bolt.", m_name);
				else if (known) msg_format("%^s casts a plasma bolt at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(6, 6) + (rlev), GF_PLASMA, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_COLD);
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(6, 6) + (rlev), GF_PLASMA, flg);
			}
			break;
		}

		/* RF5_MISSILE */
		case 128+26:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a magic missile.", m_name);
				else if (known) msg_format("%^s casts a magic missile at %s.", m_name, t_name);

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(2, 6) + (rlev/3), GF_MISSILE, flg);

				if (target < 0) update_smart_learn(who, DRS_RES_COLD);
			}
			else
			{
				

				flg = PROJECT_STOP | PROJECT_KILL;

				/* Target with a bolt attack */
				(void)project(who, 0, y, x, damroll(2, 6) + (rlev/3), GF_MISSILE, flg);
			}
			break;
		}

		/* RF5_SCARE */
		case 128+27:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and you hear scary noises.", m_name);
				else if ((blind) && (known)) msg_format("%^s mumbles.",m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a fearful illusion.", m_name);
				else if (known) msg_format("%^s casts a fearful illusion at %s.",m_name,t_name);
			}

			if (target < 0)
			{
				if (p_ptr->resist_fear)
				{
					msg_print("You refuse to be frightened.");

					/* Sometimes notice */
					if (rand_int(100) < 30) equip_can_flags(0x0L,TR2_RES_FEAR,0x0L);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You refuse to be frightened.");
				}
				else
				{
					(void)set_afraid(p_ptr->afraid + rand_int(4) + 4);

					/* Always notice */
					equip_not_flags(0x0L,TR2_RES_FEAR,0x0L);
				}
				if (who > 0) update_smart_learn(who, DRS_RES_FEAR);
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_TERRIFY */
				project_m(who, 0, y, x, rand_int(4)+4, GF_TERRIFY);
			}
			break;
		}

		/* RF5_BLIND */
		case 128+28:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s casts a spell, burning %s eyes.", m_name, t_poss);
			}

			if (target < 0)
			{
				if (p_ptr->resist_blind)
				{
					msg_print("You are unaffected!");

					/* Always notice */
					if (rand_int(100)<30) equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_blind(12 + rand_int(4));

					/* Always notice */
					equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);
				}
				if (who > 0) update_smart_learn(who, DRS_RES_BLIND);
	
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_OLD_CONF and monster level / feature power*/
				project_m(who, 0, y, x, 12+rlev, GF_OLD_CONF);
			}
			break;
		}

		/* RF5_CONF */
		case 128+29:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
				else if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a mesmerising illusion.", m_name);
				else if (known) msg_format("%^s creates a mesmerising illusion for %s.", m_name, t_poss);
			}

			if (target < 0)
			{
				if (p_ptr->resist_confu)
				{
					msg_print("You disbelieve the feeble spell.");

					/* Sometimes notice */
					if (rand_int(100)<30) equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You disbelieve the feeble spell.");
				}
				else
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);

					/* Always notice */
					equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);
				}
				if (who > 0) update_smart_learn(who, DRS_RES_CONFU);
	
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_OLD_CONF + monster level / feature power*/
				project_m(who, 0, y, x, rlev, GF_OLD_CONF);
			}
			break;
		}

		/* RF5_SLOW */
		case 128+30:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s drains power from your muscles.", m_name);
				else if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
				else if (known) msg_format("%^s drains power from %s muscles.", m_name, t_poss);
			}

			if (target < 0)
			{
				if (p_ptr->free_act)
				{
					msg_print("You are unaffected!");

					/* Always notice */
					equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_slow(p_ptr->slow + rand_int(4) + 4);

					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
				}
				if (who > 0) update_smart_learn(who, DRS_FREE);
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_OLD_SLOW */
				project_m(who, 0, y, x, rlev, GF_OLD_SLOW);			     
			}
			break;
		}

		/* RF5_HOLD */
		case 128+31:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
				else if (known) msg_format("%^s stares deeply into %s muscles.", m_name, t_poss);
			}

			if (target < 0)
			{
				if (p_ptr->free_act)
				{
					msg_print("You are unaffected!");

					/* Always notice */
					equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);

					/* Always notice */
					equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
				}
				if (who > 0) update_smart_learn(who, DRS_FREE);
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_OLD_SLEEP */
				project_m(who, 0, y, x, rlev, GF_OLD_SLEEP);			    
			}
			break;
		}



		/* RF6_HASTE */
		case 160+0:
		{
			if (target <= 0) break;

			disturb(1, 0);
			if ((blind) && (known))
			{
				msg_format("%^s mumbles.", m_name);
			}
			else if (known)
			{
				msg_format("%^s concentrates on %s body.", m_name, t_poss);
			}

			/* Allow quick speed increases to base+10 */
			if (n_ptr->mspeed < s_ptr->speed + 10)
			{
				if (known) msg_format("%^s starts moving faster.", t_name);
				n_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base+20 */
			else if (n_ptr->mspeed < s_ptr->speed + 20)
			{
				if (known) msg_format("%^s starts moving faster.", t_name);
				n_ptr->mspeed += 2;
			}

			break;
		}

		/* RF6_XXX1X6 */
		case 160+1:
		{
			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			if (target <= 0) break;

			if (known) disturb(1, 0);

			/* Message */
			if ((blind) && (known))
			{
				msg_format("%^s mumbles.", m_name);
			}
			else if (known)
			{
				msg_format("%^s concentrates on %s wounds.", m_name, t_poss);
			}

			/* Heal some */
			n_ptr->hp += (rlev * 3);

			/* Fully healed */
			if (n_ptr->hp >= n_ptr->maxhp)
			{
				/* Fully healed */
				n_ptr->hp = n_ptr->maxhp;

				/* Message */
				if (seen)
				{
					msg_format("%^s looks REALLY healthy!", t_name);
				}
				else if (known)
				{
					msg_format("%^s sounds REALLY healthy!", t_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (seen)
				{
					msg_format("%^s looks healthier.", t_name);
				}
				else if (known)
				{
					msg_format("%^s sounds healthier.", t_name);
				}
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (n_ptr->monfear)
			{
				/* Cancel fear */
				n_ptr->monfear = 0;

				/* Message */
				if (known) msg_format("%^s recovers %s courage.", t_name, t_poss);
			}

			break;
		}

		/* RF6_XXX2X6 */
		case 160+3:
		{
			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			if (target > 0)
			{
				if ((known) || (direct)) disturb(1, 0);
				if (direct) msg_format("%^s blinks away.", m_name);
				else if (known) msg_format("%^s blinks.", m_name);

				teleport_away(target, 10);
			}
			else if (target < 0)
			{
				teleport_player(10);
			}
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			if (target > 0)
			{
				if ((known) || (direct)) disturb(1, 0);
				if (direct) msg_format("%^s teleports away.", m_name);
				else if (known) msg_format("%^s teleports.", m_name);

				teleport_away(target, MAX_SIGHT * 2 + 5);
			}
			else if (target < 0)
			{
				teleport_player(100);
			}
			break;
		}

		/* RF6_XXX3X6 */
		case 160+6:
		{
			break;
		}

		/* RF6_XXX4X6 */
		case 160+7:
		{
			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			if ((who > 0) && (target < 0))
			{
				if (!direct) break;
				disturb(1, 0);
				msg_format("%^s commands you to return.", m_name);
				teleport_player_to(m_ptr->fy, m_ptr->fx);
			}
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			if (target < 0)
			{
				if (!direct) break;
				disturb(1, 0);
				msg_format("%^s teleports you away.", m_name);
				teleport_player(100);
			}
			else if (target > 0)
			{
				disturb(1, 0);
				msg_format("%^s teleports %s away.", m_name, t_name);
				teleport_away(target, MAX_SIGHT * 2 + 5);
			}
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			if (target < 0)
			{
				if (!direct) break;
				disturb(1, 0);
				if (who > 0)
				{
					if ((blind) && (known)) msg_format("%^s mumbles strangely.", m_name);
					else if (known) msg_format("%^s gestures at your feet.", m_name);
				}
				if (p_ptr->resist_nexus)
				{
					msg_print("You are unaffected!");

					/* Always notice */
					equip_can_flags(0x0L,TR2_RES_NEXUS,0x0L);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					teleport_player_level();

					/* Always notice */
					equip_not_flags(0x0L,TR2_RES_NEXUS,0x0L);
				}
				update_smart_learn(who, DRS_RES_NEXUS);
			}
			break;
		}

		/* RF6_XXX5 */
		case 160+11:
		{
			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s gestures in shadow.", m_name);
			}

			flg = PROJECT_GRID | PROJECT_KILL;

			/* Hack -- Message */
			if (!((blind) && (known)) && (target < 0))
			{
				msg_print("Darkness surrounds you.");
			}

			/* Hook into the "project()" function */
			(void)project(-1, 3, y, x, 0, GF_DARK_WEAK, flg);

			/* Lite up the room */
			unlite_room(y, x);

			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

			if (who > 0)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and then cackles evilly.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a spell and cackles evilly.", m_name);
				else if (known) msg_format("%^s casts a spell at %s and cackles evilly.",m_name,t_name);
			}

			(void)project(-1, 1, y, x, 0, GF_MAKE_TRAP, flg);

			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			if (!direct) break;
			if (target >=0) break;
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (rand_int(100) < p_ptr->skill_sav)
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				msg_print("Your memories fade away.");
			}
			break;
		}

		/* RF6_XXX6X6 */
		case 160+15:
		{
			break;
		}

		/* RF6_S_KIN */
		case 160+16:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons %s %s.", m_name, m_poss,
						((r_ptr->flags1) & RF1_UNIQUE ?
						 "minions" : "kin"));
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_KIN);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_HI_DEMON */
		case 160+17:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons greater demons!", m_name);
				else msg_print("You hear loud infernal chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_DEMON);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}

		/* RF6_S_MONSTER */
		case 160+18:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons help!", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_MONSTERS */
		case 160+19:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons monsters.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, 0);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_ANIMAL */
		case 160+20:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons animals.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_ANIMAL);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_SPIDER */
		case 160+21:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons spiders.", m_name);
				else msg_print("You hear distant chittering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_SPIDER);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_HOUND */
		case 160+22:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons hounds.", m_name);
				else msg_print("You hear distant howling.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HOUND);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_HYDRA */
		case 160+23:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons hydras.", m_name);
				else msg_print("You hear distant hissing.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; (k < 6)&& !((k>1) && (surface)); k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HYDRA);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF6_S_ANGEL */
		case 160+24:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons an angel!", m_name);
				else msg_print("You hear an angelic chorus.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_ANGEL);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_DEMON */
		case 160+25:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons a hellish adversary!", m_name);
				else msg_print("You hear infernal chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_DEMON);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_UNDEAD */
		case 160+26:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons an undead adversary!", m_name);
				else msg_print("You hear distant whispering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_UNDEAD);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_DRAGON */
		case 160+27:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons a dragon!", m_name);
				else msg_print("You hear loud chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_DRAGON);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear something appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_UNDEAD */
		case 160+28:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons a greater undead!", m_name);
				else msg_print("You hear loud whispering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_HI_DRAGON */
		case 160+29:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons ancient dragons!", m_name);
				else msg_print("You hear cacophonous chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}

		/* RF6_S_WRAITH */
		case 160+30:
		{
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons mighty undead opponents!", m_name);
				else msg_print("You hear thunderous, echoing whispers.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_WRAITH);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many creepy things appear nearby.");
			}
			break;
		}

		/* RF6_S_UNIQUE */
		case 160+31:
		{
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons special opponents!", m_name);
				else msg_print("You hear powerful, invocative chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_UNIQUE);
			}
			for (k = 0; k < 8; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
			}
			if (blind && count && (target < 0))
			{
				msg_print("You hear many powerful things appear nearby.");
			}
			break;
		}
	}
	return (TRUE);
}



/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.  See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 *
 * Note the special "MFLAG_NICE" flag, which prevents a monster from using
 * any spell attacks until the player has had a single chance to move.
 */
bool make_attack_spell(int m_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int thrown_spell, rlev;

#ifdef MONSTER_AI
	int failrate;
#endif /* MONSTER_AI */

	u32b f4, f5, f6;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	bool allow_innate = FALSE;
	bool allow_spell = FALSE;

	/* Target player */
	int x = px;
	int y = py;


	/* Extract the blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Extract the "see-able-ness" */
	bool seen = (!blind && m_ptr->ml);

	/* Assume "normal" target */
	bool normal = TRUE;

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & (MFLAG_NICE)) return (FALSE);

	/* Cannot cast spells */
	if (!r_ptr->freq_spell && !r_ptr->freq_innate) return (FALSE);

	/* Sometimes allow spells) */
	if (rand_int(100) < r_ptr->freq_spell) allow_spell = TRUE;

	/* Sometimes allow innate attacks (breaths/archery) */
	if (rand_int(100) < r_ptr->freq_innate) allow_innate = TRUE;

	/* Cannot use abilities */
	if (!allow_spell && !allow_innate) return (FALSE);

	/* Hack -- require in range player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);
	}

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;

	/* Forbid innate attacks sometimes */
	if (!allow_innate)
	{
		f4 &= ~(RF4_INNATE_MASK);
		f5 &= ~(RF5_INNATE_MASK);
		f6 &= ~(RF6_INNATE_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Forbid non-innate attacks sometimes */
	if (!allow_spell)
	{
		f4 &= (RF4_INNATE_MASK);
		f5 &= (RF5_INNATE_MASK);
		f6 &= (RF6_INNATE_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Forbid summoning sometimes */
	if (m_ptr->summoned)
	{
		f4 &= ~(RF4_SUMMON_MASK);
		f5 &= ~(RF5_SUMMON_MASK);
		f6 &= ~(RF6_SUMMON_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags2 & (RF2_SMART)) &&
	    (m_ptr->hp < m_ptr->maxhp / 10) &&
	    (rand_int(100) < 50))
	{
		/* Require intelligent spells */
		f4 &= (RF4_INT_MASK);
		f5 &= (RF5_INT_MASK);
		f6 &= (RF6_INT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

	if (!player_can_see_bold(m_ptr->fy,m_ptr->fx))
	{
		/* Set self as target */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Remove spells that attack player directly */
		f4 &= ~(RF4_ATTACK_MASK);
		f5 &= ~(RF5_ATTACK_MASK);
		f6 &= ~(RF6_ATTACK_MASK);

		/* Remove escape spells if required */
		if (!variant_oos_escapes)
		{
			f4 &= ~(RF4_ESCAPE_MASK);
			f5 &= ~(RF5_ESCAPE_MASK);
			f6 &= ~(RF6_ESCAPE_MASK);
		}

		/* Remove summon spells if required */
		if (!variant_oos_summons)
		{
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}

		/* Remove healing spells if required */
		if (!variant_oos_heals)
		{
			f4 &= ~(RF4_HEAL_MASK);
			f5 &= ~(RF5_HEAL_MASK);
			f6 &= ~(RF6_HEAL_MASK);
		}

		/* Remove 'extra' spells if required */
		if (!variant_oos_xtra)
		{
			f4 &= ~(RF4_HASTE_MASK | RF6_ANNOY_MASK);
			f5 &= ~(RF5_HASTE_MASK | RF6_ANNOY_MASK);
			f6 &= ~(RF6_HASTE_MASK | RF6_ANNOY_MASK);
		}

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}

#ifdef MONSTER_AI

	/* Check whether summons and bolts are worth it. */
	if (smart_monsters && !(r_ptr->flags2 & (RF2_STUPID)))
	{
		/* Check for a clean bolt shot */
		if ((f4 & (RF4_BOLT_MASK) ||
			 f5 & (RF5_BOLT_MASK) ||
			 f6 & (RF6_BOLT_MASK)) &&
			!clean_shot(m_ptr->fy, m_ptr->fx, py, px))
		{
			/* Remove spells that will only hurt friends */
			f4 &= ~(RF4_BOLT_MASK);
			f5 &= ~(RF5_BOLT_MASK);
			f6 &= ~(RF6_BOLT_MASK);
		}

		/* Check for a possible summon */
		if (!(summon_possible(py,px)))
		{
			/* Remove summoning spells */
			f4 &= ~(RF4_SUMMON_MASK);
			f5 &= ~(RF5_SUMMON_MASK);
			f6 &= ~(RF6_SUMMON_MASK);
		}

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);
	}
#endif /* MONSTER_AI */

	/* Cannot cast spells when confused */
	if (m_ptr->confused)
	{
		f4 &= (RF4_INNATE_MASK);
		f5 &= (RF5_INNATE_MASK);
		f6 &= (RF6_INNATE_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6) return (FALSE);

		/* Hack --- handle confusion XXX XXX */
		do
		{
			y = m_ptr->fy + randint(11) - 6;
			x = m_ptr->fx + randint(11) - 6;
		} while (!in_bounds_fully(y,x));

	}

	/* Handle "leaving" */
	if (p_ptr->leaving) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Choose a spell to cast */
	thrown_spell = choose_attack_spell(m_idx, f4, f5, f6);

	/* Abort if no spell was chosen */
	if (!thrown_spell) return (FALSE);

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (!smart_monsters || r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

	/* Check for spell failure (innate attacks never fail) */
	if ((thrown_spell >= RF5_OFFSET) && (rand_int(100) < failrate))
	{
		/* Message */
		if (m_ptr->ml) msg_format("%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}

	switch (thrown_spell)
	{
		/* RF6_HASTE */
		case 160+0:

		/* RF6_XXX1X6 */
		case 160+1:

		/* RF6_HEAL */
		case 160+2:

		/* RF6_XXX2X6 */
		case 160+3:

		/* RF6_BLINK */
		case 160+4:

		/* RF6_TPORT */
		case 160+5:
		{
			make_attack_spell_aux(m_idx, m_ptr->fy, m_ptr->fx, thrown_spell);
			break;
		}
		default:
		{
			make_attack_spell_aux(m_idx, y, x, thrown_spell);
			break;
		}
	}

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (thrown_spell < 32*4)
		{
			l_ptr->flags4 |= (1L << (thrown_spell - 32*3));
			if (l_ptr->cast_innate < MAX_UCHAR) l_ptr->cast_innate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32*5)
		{
			l_ptr->flags5 |= (1L << (thrown_spell - 32*4));
			if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32*6)
		{
			l_ptr->flags6 |= (1L << (thrown_spell - 32*5));
			if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
		}
	}

	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


	/* A spell was cast */
	return (TRUE);
}



/*
 * Can the monster exist in this grid?
 *
 * Because this function is designed for use in monster placement and
 * generation as well as movement, it cannot accept monster-specific
 * data, but must rely solely on racial information.
 */
bool cave_exist_mon(int r_idx, int y, int x, bool occupied_ok)
{
	/* Check Bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* The grid is already occupied. */
	if (cave_m_idx[y][x] != 0)
	{
		if (!occupied_ok) return (FALSE);
	}

	/*** Check passability of various features. ***/
	if (place_monster_here(y,x,r_idx)) return (TRUE);

	/* Catch weirdness */
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

	/* Hack -- fly (almost) anywhere on surface */
	bool surface = p_ptr->depth == min_depth(p_ptr->dungeon);

	/* Assume nothing in the grid other than the terrain hinders movement */
	int move_chance = 100;

	int feat;

	int mmove;

	/* Check Bounds */
	if (!in_bounds(y, x)) return (FALSE);

	/* Check location */
	feat = cave_feat[y][x];

	/* The grid is occupied by the player. */
	if (cave_m_idx[y][x] < 0)
	{
		/* Monster has no melee blows - character's grid is off-limits. */
		if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (0);

		/* Any monster with melee blows can attack the character. */
		else move_chance = 100;
	}

	/* The grid is occupied by a monster. */
	else if (cave_m_idx[y][x] > 0)
	{
	       	monster_type *n_ptr = &m_list[cave_m_idx[y][x]];
       		monster_race *nr_ptr = &r_info[n_ptr->r_idx];

		/* Kill weaker monsters */
		if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
		    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
		    (r_ptr->mexp > nr_ptr->mexp))
		{
			move_chance = 100;
		}

		/* Push past weaker or similar monsters */
		else if ((r_ptr->flags2 & (RF2_MOVE_BODY)) && 
		    (r_ptr->mexp >= nr_ptr->mexp))
		{
			move_chance = 80;
		}

		/* Push past flying monsters or if flying */
		else if ((m_ptr->mflag & (MFLAG_OVER)) || 
		    (n_ptr->mflag & (MFLAG_OVER)))
		{
			move_chance = 80;
		}

		/* Push past hidden monsters */
		else if ((m_ptr->mflag & (MFLAG_HIDE)) ||
		    (n_ptr->mflag & (MFLAG_HIDE)))
		{
			move_chance = 80;
		}

		/* Push past if fleeing, or target fleeing, but not both */
		else if (((m_ptr->monfear) || 
		    (n_ptr->monfear)) && !(m_ptr->monfear && (n_ptr->monfear)))
		{
			move_chance = 80;
		}


		/* Cannot do anything to clear away the other monster */
		else return (0);
	}

	/* Paranoia -- move_chance must not be more than 100 */
	if (move_chance > 100) move_chance = 100;

	/* Check how we move */
	mmove = place_monster_here(y,x,m_ptr->r_idx);

	/*** Check passability of various features. ***/

	/* The monster is under covered terrain, moving to uncovered terrain. */
	if ((m_ptr->mflag & (MFLAG_HIDE)) && (f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_COVERED)) &&
		!(f_info[cave_feat[y][x]].flags2 & (FF2_COVERED)) && (mmove != MM_SWIM) && (mmove != MM_DIG))
	{

		if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[y][x]].flags1 & (FF1_BASH)))
		{
			*bash = TRUE;
		}
		else
		{
			move_chance =0;
		}

		return (move_chance);

	}

	/* Feature is passable */
	else if (mmove != MM_FAIL)
	{

		/* Anything else that's not a wall we assume to be passable. */
		return (move_chance);
	}

	/* Feature is a wall */
	else
	{
		int unlock_chance = 0;
		int bash_chance = 0;

		/* Glyphs */
		if (f_info[feat].flags1 & (FF1_GLYPH))
		{
			/* Glyphs are hard to break */
			return (MIN(100 * r_ptr->level / BREAK_GLYPH, move_chance));
		}

		/* Monsters outside can fly/climb over buildings */
		if ((surface) && (m_ptr->mflag & (MFLAG_OVER)) && (feat != FEAT_PERM_SOLID))
		{
			return (100);
		}

		/* Monster can open doors */
		if (f_info[feat].flags1 & (FF1_SECRET))
		{
				/* Discover the secret (temporarily) */
				feat = feat_state(feat,FS_SECRET);
		}

		/* Monster can open doors */
		if ((r_ptr->flags2 & (RF2_OPEN_DOOR)) && (f_info[feat].flags1 & (FF1_OPEN)))
		{
			/* Secret doors and easily opened stuff */
			if (f_info[feat].power == 0)
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
				lock_power = 35 * f_info[feat].power;

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
		if ((r_ptr->flags2 & (RF2_BASH_DOOR)) && (f_info[feat].flags1 & (FF1_BASH)))
		{
			int door_power, bashing_power;

			/* Door power (from 60 to 420) */
			/* 
			 * XXX - just because a door is difficult to unlock 
			 * shouldn't mean that it's hard to bash.  Until the 
			 * character door bashing code is changed, however, 
			 * we'll stick with this.
			 */
			door_power = 60 + 60 * f_info[feat].power;

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

	/* Any wall grid that isn't explicitly made passible is impassible. */
	return (0);
}


/*
 * Helper function for monsters that want to advance toward the character.
 * Assumes that the monster isn't frightened, and is not in LOS of the 
 * character.
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
 * When flowing, monsters prefer non-diagonal directions.
 *
 * XXX - At present, this function does not handle difficult terrain 
 * intelligently.  Monsters using flow may bang right into a door that 
 * they can't handle.  Fixing this may require code to set monster 
 * paths.
 */
static void get_move_advance(monster_type *m_ptr, int *ty, int *tx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1;

	int lowest_cost = 250;

	bool use_sound = FALSE;
	bool use_scent = FALSE;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster can go through rocks - head straight for character */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) || 
	   (r_ptr->flags2 & (RF2_KILL_WALL)))
	{
		*ty = py;
		*tx = px;
		return;
	}

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* Use target information if available */
	if ((m_ptr->ty) && (m_ptr->tx))
	{
		*ty = m_ptr->ty;
		*tx = m_ptr->tx;
		return;
	}

	/* If we can hear noises, advance towards them */
	if (cave_cost[y1][x1])
	{
		use_sound = TRUE;
	}

	/* Otherwise, try to follow a scent trail */
	else if (monster_can_smell(m_ptr))
	{
		use_scent = TRUE;
	}

	/* Otherwise, advance blindly */
	if ((!use_sound) && (!use_scent))
	{
		*ty = py;
		*tx = px;
		return;
	}

	/* Using flow information.  Check nearby grids, diagonals first. */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Check Bounds */
		if (!in_bounds(y, x)) continue;

		/* We're following a scent trail */
		if (use_scent)
		{
			int age = get_scent(y, x);
			if (age == -1) continue;

			/* Accept younger scent */
			if (lowest_cost < age) continue;
			lowest_cost = age;
		}

		/* We're using sound */
		else
		{
			int cost = cave_cost[y][x];

			/* Accept louder sounds */
			if ((cost == 0) || (lowest_cost < cost)) continue;
			lowest_cost = cost;
		}

		/* Save the location */
		*ty = y;
		*tx = x;
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

	int y, x, yy, xx;

	int countdown = scan_range;

	int least_cost = 100;
	int least_cost_y = 0;
	int least_cost_x = 0;
	int chance, cost, parent_cost;
	bool dummy;

	/* Factors for converting table to actual dungeon grids */
	int conv_y, conv_x;

	/*
	 * Allocate and initialize a table of movement costs.
	 * Both axis must be (2 * scan_range + 1).
	 */
	byte safe_cost[31][31];

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
		safe_cost[p_ptr->py + conv_y][p_ptr->px + conv_x] = 100;
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

				/* Grid is inaccessable (or at least very difficult to enter) */
				if ((safe_cost[y][x] == 0) || (safe_cost[y][x] >= 100))
				{
					x = x_tmp;
					continue;
				}

				/* Get the accumulated cost to enter this grid */
				parent_cost = safe_cost[y][x];

				/* Scan all adjacent grids */
				for (i = 0; i < 8; i++)
				{
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
						/* Get the cost to enter this grid */
						chance = cave_passable_mon(m_ptr, yy - conv_y, 
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
							    (least_cost == this_cost && rand_int(2) == 0))
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
									if ((!player_can_see_bold(yyy, xxx)) &&
									    (cave_passable_mon(m_ptr, yyy, xxx, &dummy)))
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
								 * Look hard for alternative
								 * hiding places if this one
								 * seems pricey.
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
		m_ptr->ty = y;
		m_ptr->tx = x;

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

	/* Monster has a target */
	if ((m_ptr->ty) && (m_ptr->tx))
	{
		/* It's out of LOS; keep using it, except in "knight's move" cases */
		if (!player_has_los_bold(m_ptr->ty, m_ptr->tx))
		{
			/* Get axis distance from character to current target */
			int dist_y = ABS(p_ptr->py - m_ptr->ty);
			int dist_x = ABS(p_ptr->px - m_ptr->tx);

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

					if ((y == m_ptr->ty) && (x == m_ptr->tx)) continue;

					if (cave_passable_mon(m_ptr, m_ptr->ty, m_ptr->tx, &dummy) > 
					    cave_passable_mon(m_ptr, y, x, &dummy)) continue;

					m_ptr->ty = y;
					m_ptr->tx = x;
					break;
				}
			}

			/* Move towards the target */
			*ty = m_ptr->ty;
			*tx = m_ptr->tx;
			return (TRUE);
		}

		/* It's in LOS; cancel it. */
		else
		{
			m_ptr->ty = 0;
			m_ptr->tx = 0;
		}
	}

	/* The monster is not in LOS, but thinks it's still too close. */
	if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
	{
		/* Monster cannot pass through walls */
		if (!((r_ptr->flags2 & (RF2_PASS_WALL)) || 
	      	 (r_ptr->flags2 & (RF2_KILL_WALL))))
		{
			/* Run away from noise */
			if (cave_cost[m_ptr->fy][m_ptr->fx])
			{
				int start_cost = cave_cost[m_ptr->fy][m_ptr->fx];

				/* Look at adjacent grids, diagonals first */
				for (i = 7; i >= 0; i--)
				{
					y = m_ptr->fy + ddy_ddd[i];
					x = m_ptr->fx + ddx_ddd[i];

					/* Check Bounds */
					if (!in_bounds(y, x)) continue;

					/* Accept the first non-visible grid with a higher cost */
					if (cave_cost[y][x] > start_cost)
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
		int prev_cost = cave_cost[m_ptr->fy][m_ptr->fx];
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
			if (cave_cost[y][x] >= prev_cost)
			{
				*ty = y;
				*tx = x;
				prev_cost = cave_cost[y][x];

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
		    (m_ptr->cdis < TURN_RANGE))
		{
			/* Turn and fight */
			m_ptr->monfear = 0;

			/* Recalculate combat range (later) */
			m_ptr->min_range = 0;

			/* Visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s turns to fight!", m_name);
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

	int i, start;
	int y, x;

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Assume no movement */
	*ty = m_ptr->fy;
	*tx = m_ptr->fx;


	/*
	 * Monster is only allowed to use targetting information.
	 */
	if (must_use_target)
	{
		*ty = m_ptr->ty;
		*tx = m_ptr->tx;
		return (TRUE);
	}


	/*
	 * Monsters that cannot move will attack the character if he is 
	 * adjacent.
	 */
	if (r_ptr->flags1 & (RF1_NEVER_MOVE))
	{
		/* Hack -- memorize lack of moves after a while. */
		if (!(l_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			if ((m_ptr->ml) && (randint(20) == 1)) 
				l_ptr->flags1 |= (RF1_NEVER_MOVE);
		}

		/* Is character in range? */
		if (m_ptr->cdis <= 1)
		{
			/* Monster can't melee either (pathetic little creature) */
			if (r_ptr->flags1 & (RF1_NEVER_BLOW))
			{
				/* Hack -- memorize lack of attacks after a while */
				if (!(l_ptr->flags1 & (RF1_NEVER_BLOW)))
				{
					if ((m_ptr->ml) && (randint(10) == 1))
						l_ptr->flags1 |= (RF1_NEVER_BLOW);
				}
				return (FALSE);
			}

			/* Kill. */
			*fear = FALSE;
			*ty = py;
			*tx = px;
			return (TRUE);
		}

		/* If we can't hit anything, do not move */
		else
		{
			return (FALSE);
		}
	}


	/*** Handle monster fear -- only for monsters that can move ***/

	/* Is the monster scared? */
	if ((m_ptr->min_range == FLEE_RANGE) || (m_ptr->monfear)) *fear = TRUE;
	else *fear = FALSE;

	/* Monster is frightened or terrified. */
	if (*fear)
	{
		/* The character is too close to avoid, and faster than we are */
		if ((!m_ptr->monfear) && (m_ptr->cdis < TURN_RANGE) &&
		     (p_ptr->pspeed > m_ptr->mspeed))
		{
			/* Recalculate range */
			find_range(m_ptr);

			/* Note changes in monster attitude */
			if (m_ptr->min_range < m_ptr->cdis)
			{
				/* Cancel fear */
				*fear = FALSE;

				/* No message -- too annoying */

				/* Charge! */
				*ty = py;
				*tx = px;

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
			if (!(l_ptr->flags1 & (RF1_NEVER_BLOW)))
			{
				if ((m_ptr->ml) && (randint(10) == 1))
					l_ptr->flags1 |= (RF1_NEVER_BLOW);
			}

			/* Back away */
			*fear = TRUE;
		}

		else
		{
			/* All other monsters attack. */
			*ty = py;
			*tx = px;
			return (TRUE);
		}
	}


	/* Animal packs try to lure the character into the open. */
	if ((!*fear) && (r_ptr->flags1 & (RF1_FRIENDS)) && 
			(r_ptr->flags3 & (RF3_ANIMAL))  && 
		      (!((r_ptr->flags2 & (RF2_PASS_WALL)) || 
		      (r_ptr->flags2 & (RF2_KILL_WALL)))))
	{
		/* Animal has to be willing to melee */
		if (m_ptr->min_range == 1)
		{
			/*
			 * If character vulnerability has not yet been 
			 * calculated this turn, calculate it now.
			 */
			if (p_ptr->vulnerability == 0)
			{
				/* Count passable grids next to player */
				for (i = 0; i < 8; i++)
				{
					y = py + ddy_ddd[i];
					x = px + ddx_ddd[i];

					/* Check Bounds */
					if (!in_bounds(y, x)) continue;

					/* Count floor grids (generic passable) */
					if (cave_floor_bold(y, x))
					{
						p_ptr->vulnerability++;
					}
				}

				/*
				 * Take character weakness into account (this 
				 * always adds at least one)
				 */
				if (p_ptr->chp <= 3) p_ptr->vulnerability = 100;
				else p_ptr->vulnerability += (p_ptr->mhp / p_ptr->chp);
			}

			/* Character is insufficiently vulnerable */
			if (p_ptr->vulnerability <= 4)
			{
				/* If we're in sight, find a hiding place */
				if (cave_info[m_ptr->fy][m_ptr->fx] & (CAVE_SEEN))
				{
					/* Find a safe spot to lurk in */
					if (get_move_retreat(m_ptr, ty, tx))
					{
						*fear = TRUE;
					}
					else
					{
						/* No safe spot -- charge */
						*ty = py;
						*tx = px;
					}
				}

				/* If we're not viewable, we advance cautiously */
				else
				{
					/* Advance, ... */
					get_move_advance(m_ptr, ty, tx);

					/* ... but make sure we stay hidden. */
					*fear = TRUE;
				}

				/* done */
				return (TRUE);
			}
		}
	}

	/* Monster groups try to surround the character. */
	if ((!*fear) && (r_ptr->flags1 & (RF1_FRIENDS)) && (m_ptr->cdis <= 3))
	{
		start = rand_int(8);

		/* Find a random empty square next to the player to head for */
		for (i = start; i < 8 + start; i++)
		{
			/* Pick squares near player */
			y = py + ddy_ddd[i % 8];
			x = px + ddx_ddd[i % 8];

			/* Check Bounds */
			if (!in_bounds(y, x)) continue;

			/* Ignore occupied grids */
			if (cave_m_idx[y][x] != 0) continue;

			/* Ignore grids that monster can't enter immediately */
			if (!cave_exist_mon(m_ptr->r_idx, y, x, FALSE)) continue;

			/* Accept */
			*ty = y;
			*tx = x;
			return (TRUE);
		}
	}

	/* Monster can go through rocks - head straight for character */
	if ((!*fear) && ((r_ptr->flags2 & (RF2_PASS_WALL)) || 
			 (r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		*ty = py;
		*tx = px;
		return (TRUE);
	}


	/* No special moves made -- use standard movement */

	/* Not frightened */
	if (!*fear)
	{
		/*
		 * XXX XXX -- The monster cannot see the character.  Make it 
		 * advance, so the player can have fun ambushing it.
		 */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Advance */
			get_move_advance(m_ptr, ty, tx);
		}

		/* Monster can see the character */
		else
		{
			/* Always reset the monster's target */
			m_ptr->ty = py;
			m_ptr->tx = px;

			/* Monsters too far away will advance. */
			if (m_ptr->cdis > m_ptr->best_range)
			{
				*ty = py;
				*tx = px;
			}

			/* Monsters not too close will often advance */
			else if ((m_ptr->cdis > m_ptr->min_range)  && (rand_int(2) == 0))
			{
				*ty = py;
				*tx = px;
			}			

			/* Monsters that can't target the character will advance. */
			else if (!projectable(m_ptr->fy, m_ptr->fx, 
				 py, px))
			{
				*ty = py;
				*tx = px;
			}

			/* Otherwise they will stay still or move randomly. */
			else
			{
				/*
				 * It would be odd if monsters that move randomly 
				 * were to stay still.
				 */
				if (r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25))
				{
					/* pick a random grid next to the monster */
					int i = rand_int(8);

					*ty = m_ptr->fy + ddy_ddd[i];
					*tx = m_ptr->fx + ddx_ddd[i];
				}

				/* Monsters could look for better terrain... */
			}
		}
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
 * to their target.  It's very limited, but works fairly well in the 
 * situations it is called upon to resolve.  XXX
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
	int target_y, target_x, dist_y, dist_x;

	bool dummy;
	bool below = FALSE;
	bool right = FALSE;

	target_y = 0;
	target_x = 0;

	/* Is the target further away vertically or horizontally? */
	dist_y = ABS(m_ptr->ty - m_ptr->fy);
	dist_x = ABS(m_ptr->tx - m_ptr->fx);

	/* Target is further away vertically than horizontally */
	if (dist_y > dist_x)
	{
		/* Find out if the target is below the monster */
		if (m_ptr->ty - m_ptr->fy > 0) below = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

			/* Check Bounds (fully) */
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

						/* Accept (original) grid, but don't immediately claim success */
						*ty = y;
						*tx = x;
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
		if (m_ptr->tx - m_ptr->fx > 0) right = TRUE;

		/* Search adjacent grids */
		for (i = 0; i < 8; i++)
		{
			y = m_ptr->fy + ddy_ddd[i];
			x = m_ptr->fx + ddx_ddd[i];

       			/* Check Bounds (fully) */
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
						target_y = y;
						target_x = x;
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
	if ((target_y) && (target_x))
	{
		*ty = target_y;
		*tx = target_x;
		return (TRUE);
	}

	/* No luck */
	return (FALSE);
}


/*
 * If one monster moves into another monster's grid, they will 
 * normally swap places.  If the second monster cannot exist in the 
 * grid the first monster left, this can't happen.  In such cases, 
 * the first monster tries to push the second out of the way.
 */
static bool push_aside(monster_type *m_ptr, monster_type *n_ptr)
{

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
	if (turn % 2 == 0) dir += 10;

	/* Check all directions radiating out from the initial direction. */
	for (i = 0; i < 7; i++)
	{
		int side_dir = side_dirs[dir][i];

		y = n_ptr->fy + ddy[side_dir];
		x = n_ptr->fx + ddx[side_dir];

		/* Illegal grid */
		if (!in_bounds_fully(y, x)) continue;

		/* Grid is not occupied, and the 2nd monster can exist in it. */
		if (cave_exist_mon(n_ptr->r_idx, y, x, FALSE))
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
 * Handle monster hitting a real trap.
 */
void mon_hit_trap(int m_idx, int y, int x)
{
	int dam;

	feature_type *f_ptr;
	monster_type *m_ptr = &m_list[m_idx];

	int feat = cave_feat[y][x];

	/* Option */
	if (!variant_hit_traps) return;

	/* Hack --- don't activate unknown invisible traps */
	if (cave_feat[y][x] == FEAT_INVIS) return;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];

	/* Hack --- trapped doors */
	/* XXX XXX Dangerous */
	while (!(f_ptr->spell) && !(f_ptr->blow.method) && (f_ptr->flags1 & (FF1_TRAP)))
	{
		pick_trap(y,x);

		/* Error */
		if (cave_feat[y][x] == feat) break;

		feat = cave_feat[y][x];

		/* Get feature */
		f_ptr = &f_info[feat];

	}

	/* Use covered or bridged if necessary */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		f_ptr = &f_info[f_ptr->mimic];
	}

	/* Paranoia */
	if (!(f_ptr->spell) && !(f_ptr->blow.method)) return;

	/* Hack -- monster falls onto trap */
	if ((m_ptr->fy!=y)|| (m_ptr->fx !=x))
	{
		/* Move monster */
		monster_swap(m_ptr->fy, m_ptr->fx, y, x);
	}


	/* Apply the spell */
	if (f_ptr->spell)
	{
	      make_attack_spell_aux(0,y,x,f_ptr->spell);
	}

	/* Apply the attack */
	else if (f_ptr->blow.method)
	{
		dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice);
			   
		/* Apply the blow */
		project_m(0,
			  0,
			  y,
			  x,
			  dam,
			  f_info[cave_feat[y][x]].blow.effect);

	}

	if (f_ptr->flags1 & (FF1_HIT_TRAP))
	{
		/* Modify the location hit by the trap */
		cave_alter_feat(y,x,FS_HIT_TRAP);
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
static bool make_move(monster_type *m_ptr, int *ty, int *tx, bool fear, bool *bash)
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
	dy = oy - *ty;
	dx = ox - *tx;

	/* Is the target grid adjacent to the current monster's position? */
	if ((!fear) && (dy >= -1) && (dy <= 1) && (dx >= -1) && (dx <= 1))
	{
		/* If it is, try the shortcut of simply moving into the grid */

		/* Get the probability of entering this grid */
		chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

		/* Grid must be pretty easy to enter, or monster must be confused */
		if ((m_ptr->confused) || (chance >= 50))
		{
			/*
			 * Amusing messages and effects for confused monsters trying 
			 * to enter terrain forbidden to them.
			 */
			if (chance == 0)
			{
				/* Do not actually move */
				return (FALSE);
			}

			/* We can enter this grid */
			if ((chance == 100) || (chance > rand_int(100)))
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
		if (dy > 0)
		{
			/* We're heading up */
			dir0 = 8;
			if ((dx > 0) || (dx == 0 && turn % 2 == 0)) dir0 += 10;
		}
		else
		{
			/* We're heading down */
			dir0 = 2;
			if ((dx < 0) || (dx == 0 && turn % 2 == 0)) dir0 += 10;
		}
	}

	/* We mostly want to move horizontally */
	else if (ax > (ay * 2))
	{
		/* Choose between directions '4' and '6' */
		if (dx > 0)
		{
			/* We're heading left */
			dir0 = 4;
			if ((dy < 0) || (dy == 0 && turn % 2 == 0)) dir0 += 10;
		}
		else
		{
			/* We're heading right */
			dir0 = 6;
			if ((dy > 0) || (dy == 0 && turn % 2 == 0)) dir0 += 10;
		}
	}

	/* We want to move up and sideways */
	else if (dy > 0)
	{
		/* Choose between directions '7' and '9' */
		if (dx > 0)
		{
			/* We're heading up and left */
			dir0 = 7;
			if ((ay < ax) || (ay == ax && turn % 2 == 0)) dir0 += 10;
		}
		else
		{
			/* We're heading up and right */
			dir0 = 9;
			if ((ay > ax) || (ay == ax && turn % 2 == 0)) dir0 += 10;
		}
	}

	/* We want to move down and sideways */
	else
	{
		/* Choose between directions '1' and '3' */
		if (dx > 0)
		{
			/* We're heading down and left */
			dir0 = 1;
			if ((ay > ax) || (ay == ax && turn % 2 == 0)) dir0 += 10;
		}
		else
		{
			/* We're heading down and right */
			dir0 = 3;
			if ((ay < ax) || (ay == ax && turn % 2 == 0)) dir0 += 10;
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
			if (m_ptr->confused) break;

			/* If this grid is totally impassable, skip it */
			if (moves_data[i].move_chance == 0) continue;

			/* Frightened monsters work hard not to be seen. */
			if (fear)
			{
				/* Monster is having trouble navigating to its target. */
				if ((m_ptr->ty) && (m_ptr->tx) && (i >= 2))
				{
					/* Look for an adjacent grid leading to the target */
					if (get_route_to_target(m_ptr, ty, tx))
					{
						int chance;

						/* Calculate the chance to enter the grid */
						chance = cave_passable_mon(m_ptr, *ty, *tx, bash);

						/* Try to move into the grid */
						if ((chance < 100) && (randint(100) > chance))
						{
							/* Can't move */
							return (FALSE);
						}

						/* Can move */
						return (TRUE);
					}
				}

				/* Attacking the character as a first choice? */
				if ((i == 0) && (ny == p_ptr->py) && (nx == p_ptr->px))
				{
					/* Need to rethink some plans XXX XXX XXX */
					m_ptr->ty = 0;
					m_ptr->tx = 0;
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
				if ((!m_ptr->ml) && (!(cave_info[oy][ox] & (CAVE_SEEN))))
				{
					/* Do not enter a "seen" grid */
					if (cave_info[ny][nx] & (CAVE_SEEN))
					{
						moves_data[i].move_chance = 0;
						continue;
					}
				}
			}

			/* XXX XXX -- Sometimes attempt to break glyphs. */
			if ((cave_feat[ny][nx] == FEAT_GLYPH) && (!fear) && 
			    (rand_int(5) == 0)) 
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
		if ((m_ptr->confused) && (moves_data[i].move_chance == 0))
		{
			/* Do not actually move */
			return (FALSE);
		}

		/* Try to move in the chosen direction.  If we fail, end turn. */
		if ((moves_data[i].move_chance < 100) && 
		    (randint(100) > moves_data[i].move_chance))
		{
			return (FALSE);
		}
	}


	/* Monster is frightened, and is obliged to fight. */
	if ((fear) && (cave_m_idx[*ty][*tx] < 0))
	{
		/* Turn and fight */
		m_ptr->monfear = 0;
		fear = FALSE;

		/* Recalculate combat range (later) */
		m_ptr->min_range = 0;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);
		}
	}

	/* We can move. */
	return (TRUE);
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
static void process_move(int m_idx, int ty, int tx, bool bash)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int mmove;

	/* Existing monster location, proposed new location */
	int oy, ox, ny, nx;

	s16b this_o_idx, next_o_idx = 0;

	int feat;

	/* Default move, default lack of view */
	bool do_move = TRUE;
	bool do_view = FALSE;

	/* Assume nothing */
	bool did_open_door = FALSE;
	bool did_bash_door = FALSE;
	bool did_take_item = FALSE;
	bool did_kill_item = FALSE;
	bool did_move_body = FALSE;
	bool did_kill_body = FALSE;
	bool did_pass_wall = FALSE;
	bool did_kill_wall = FALSE;
	bool did_smart = FALSE;
	bool did_sneak = FALSE;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));
	bool outside = (f_info[cave_feat[p_ptr->py][p_ptr->px]].flags3 & (FF3_OUTSIDE)) && (surface);

	/* Remember where monster is */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get the destination */
	ny = ty;
	nx = tx;

	/* Check Bounds */
	if (!in_bounds(ny, nx)) return;


	/* The monster is flying around above the player while they are in a building */
	if (do_move && (m_ptr->mflag & (MFLAG_OVER)) && (surface) && (!outside) && cave_m_idx[ny][nx]<0)
	{
		return;
	}
	/* The monster is hidden in terrain, trying to attack the player.*/
	else if (do_move && (m_ptr->mflag & (MFLAG_HIDE)) && (cave_m_idx[ny][nx] < 0))
	{
		/* We can't get out of hiding */
		if ((f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED)) ||
			(m_ptr->mflag & (MFLAG_OVER)))
		{
			if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[ny][nx]].flags1 & (FF1_BASH)))
			{
				/* Don't move*/
				do_move = FALSE;

				/* Bash through the floor */
				cave_alter_feat(ny, nx, FS_BASH);

				/* Unhide the monster */
				m_ptr->mflag &= ~(MFLAG_HIDE);

				/* And reveal */
				update_mon(m_idx,FALSE);

				/* Hack --- tell the player if something unhides */
				if ((m_ptr->mflag & (MFLAG_HIDE)) && (m_ptr->ml))
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					msg_format("%^s emerges from %s%s.",m_name,
						((f_info[cave_feat[oy][ox]].flags2 & (FF2_FILLED))?"":"the "),
						f_name+f_info[cave_feat[oy][ox]].name);
				}

				/* We saw it, maybe */
				did_bash_door = TRUE;

				/* Disturb on "move" */
				if (m_ptr->ml &&
				    (disturb_move ||
				     ((m_ptr->mflag & (MFLAG_VIEW)) &&
				      disturb_near)))
				{
					/* Disturb */
					disturb(0, 0);
				}

			}

			/* Can't get out, can't attack */
			else
			{

				return;
			}
		}
		else
		{
			/* Unhide the monster */
			m_ptr->mflag &= ~(MFLAG_HIDE);

			/* And reveal */
			update_mon(m_idx,FALSE);

			/* Hack --- tell the player if something unhides */
			if ((m_ptr->mflag & (MFLAG_HIDE)) && (m_ptr->ml))
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				msg_format("%^s emerges from %s%s.",m_name,
					((f_info[cave_feat[oy][ox]].flags2 & (FF2_FILLED))?"":"the "),
					f_name+f_info[cave_feat[oy][ox]].name);
			}

			/* Disturb on "move" */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}

		}

	}

	/* The grid is occupied by the player. */
	if (cave_m_idx[ny][nx] < 0)
	{
		/* Attack if possible */
		if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			(void)make_attack_normal(m_idx);
		}

		/* End move */
		do_move = FALSE;
	}


	/* Get the feature in the grid that the monster is trying to enter. */
	feat = cave_feat[ny][nx];

	/* Get the move */
	mmove = place_monster_here(ny,nx,m_ptr->r_idx);

	/* The monster is stuck in terrain */
	if (!(m_ptr->mflag & (MFLAG_OVER)) && !(f_info[cave_feat[oy][ox]].flags1 & (FF1_MOVE)) &&
		!place_monster_here(oy,ox,m_ptr->r_idx) && (mmove != MM_PASS))
	{
		if (((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[oy][ox]].flags1 & (FF1_BASH)))
				|| (r_ptr->flags2 & (RF2_KILL_WALL)))
		{
			/* Bash through the terrain */
			cave_alter_feat(oy, ox, FS_BASH);

			/* Unhide the monster */
			m_ptr->mflag &= ~(MFLAG_HIDE);

			/* And reveal */
			update_mon(m_idx,FALSE);

			/* Hack --- tell the player if something unhides */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				msg_format("%^s emerges from %s%s.",m_name,
				((f_info[cave_feat[oy][ox]].flags2 & (FF2_FILLED))?"":"the "),
				f_name+f_info[cave_feat[oy][ox]].name);
			}

			/* We saw it, maybe */
			if (r_ptr->flags2 & (RF2_KILL_WALL)) did_kill_wall = TRUE;
				else did_bash_door = TRUE;

			/* Disturb on "move" */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}

			do_move = FALSE;
		}
			else
			{
				do_move = FALSE;
			}
			
	}

	/* The monster is under covered terrain, moving to uncovered terrain. */
	else if ((m_ptr->mflag & (MFLAG_HIDE)) && (f_info[cave_feat[oy][ox]].flags2 & (FF2_COVERED)) &&
		!(f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED)) && (mmove != MM_FAIL))
	{
		if ((mmove == MM_SWIM) || (mmove == MM_DIG) || (mmove == MM_PASS))
		{
				/* Move harmlessly */
		}

		else if (((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[oy][ox]].flags1 & (FF1_BASH)))
				|| (r_ptr->flags2 & (RF2_KILL_WALL)))
		{
			/* Bash through the floor */
			cave_alter_feat(oy, ox, FS_BASH);

			/* Unhide the monster */
			m_ptr->mflag &= ~(MFLAG_HIDE);

			/* And reveal */
			update_mon(m_idx,FALSE);

			/* Hack --- tell the player if something unhides */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				msg_format("%^s emerges from %s%s.",m_name,
				((f_info[cave_feat[oy][ox]].flags2 & (FF2_FILLED))?"":"the "),
				f_name+f_info[cave_feat[oy][ox]].name);
			}

			/* We saw it, maybe */
			if (r_ptr->flags2 & (RF2_KILL_WALL)) did_kill_wall = TRUE;
				else did_bash_door = TRUE;

			/* Disturb on "move" */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}

			do_move = FALSE;
		}
	}

	/* Monster is on covered terrain, moving to covered terrain */
	else if (!(m_ptr->mflag & (MFLAG_HIDE)) && (f_info[cave_feat[oy][ox]].flags2 & (FF2_COVERED)) &&
		(f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED)) &&
			((mmove == MM_SWIM) || (mmove == MM_DIG)))
	{

		if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[ny][nx]].flags1 & (FF1_BASH)))
		{
			/* Bash through the floor */
			cave_alter_feat(ny, nx, FS_BASH);

			/* We saw it, maybe */
			did_bash_door = TRUE;

			/* Disturb on "move" */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}

			mmove = MM_WALK;
		}

		else if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[oy][ox]].flags1 & (FF1_BASH)))
		{
			/* Bash through the floor */
			cave_alter_feat(oy, ox, FS_BASH);

			/* We saw it, maybe */
			did_bash_door = TRUE;

			/* Disturb on "move" */
			if (m_ptr->ml &&
			    (disturb_move ||
			     ((m_ptr->mflag & (MFLAG_VIEW)) &&
			      disturb_near)))
			{
				/* Disturb */
				disturb(0, 0);
			}

			do_move = FALSE;
		}

		else if ((r_ptr->flags2 & (RF2_CAN_FLY)) &&
			 (f_info[cave_feat[ny][nx]].flags2 & (FF2_CAN_FLY)))
		{
			mmove = MM_FLY;
		}

		else if (!(r_ptr->flags2 & (RF2_MUST_SWIM)) &&
			(mon_resist_feat(f_info[cave_feat[ny][nx]].mimic,m_ptr->r_idx)))
		{
			mmove = MM_WALK;
		}
			else
			{
				do_move = FALSE;
			}
	 
	}

	else if (mmove == MM_FAIL)
	{
		/* Glyphs */
		if (f_info[feat].flags1 & (FF1_GLYPH))
		{
			/* Describe observable breakage */
			if (cave_info[ny][nx] & (CAVE_MARK))
			{
				msg_print("The rune of protection is broken!");
			}
	
			/* Forget the rune */
			cave_info[ny][nx] &= ~(CAVE_MARK);
	
			/* Break the rune */
			cave_alter_feat(ny, nx, FS_GLYPH);
	
		}

		/* Doors */
		if ((f_info[feat].flags1 & (FF1_BASH)) || (f_info[feat].flags1 & (FF1_OPEN)) ||
			  (f_info[feat].flags1 & (FF1_SECRET)))
		{

			/* Hack --- monsters find secrets */
			if (f_info[feat].flags1 & (FF1_SECRET)) cave_alter_feat(ny,nx,FS_SECRET);

			/* Monster bashes the door down */
			if ((bash) & (f_info[feat].flags1 & (FF1_BASH)))
			{
				/* Character is not too far away */
				if (m_ptr->cdis < 30)
				{
					/* Message */
					msg_print("You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor) disturb(0, 0);
				}

				/* The door was bashed open */
				did_bash_door = TRUE;

				/* Break down the door */
				if (rand_int(100) < 50) cave_alter_feat(ny, nx, FS_OPEN);
				else cave_alter_feat(ny, nx, FS_OPEN);

				/* Handle viewable doors */
				if (cave_info[ny][nx] & (CAVE_SEEN)) 
				{
					/* Always disturb */
					disturb(0, 0);

					do_view = TRUE;
				}

				/* Optional disturb for non-viewable doors */
				else if (disturb_minor) disturb(0, 0);
			}

			/* Monster opens the door */
			else if (f_info[feat].flags1 & (FF1_OPEN))
			{
					/* Unlock the door */
					cave_alter_feat(ny, nx, FS_OPEN);

					/* Do not move */
					do_move = FALSE;
			}
		}

		/* Hack --- smart monsters try to disarm traps */
		else if ((f_info[cave_feat[ny][nx]].flags1 & (FF1_TRAP)) &&
			(r_ptr->flags2 & (RF2_SMART)))
		{
			/* Break the ward */
			if (randint(r_ptr->level) > f_info[cave_feat[ny][nx]].power)
			{
				/* Describe hidden trap breakage */
				if ((cave_feat[ny][nx] == FEAT_INVIS) || (cave_feat[ny][nx] == FEAT_DOOR_INVIS))
				{

					/* Pick a trap */
					pick_trap(ny,nx);

					/* Describe observable breakage */
					if ((cave_info[ny][nx] & (CAVE_MARK)) && (m_ptr->ml))
					{
						char m_name[80];

						/* Get the monster name */
						monster_desc(m_name, m_ptr, 0);

						msg_format("%^s disarms the hidden %s.",m_name,f_name+f_info[cave_feat[ny][nx]].name);
					}

				}

				/* Describe observable breakage */
				else if (cave_info[ny][nx] & (CAVE_MARK))
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					msg_format("%^s disarms the %s.",m_name,f_name+f_info[cave_feat[ny][nx]].name);
				}

				/* Forget the rune */
				cave_info[ny][nx] &= ~(CAVE_MARK);

				/* Break the rune */
				cave_alter_feat(ny, nx, FS_DISARM);

				/* Use up time */
				do_move = FALSE;

				/* Did smart stuff */
				did_smart = TRUE;
			}
			/* Don't set off the ward */
			else if (randint(r_ptr->level) > f_info[cave_feat[ny][nx]].power)
			{
				do_move = FALSE;	
			}
		}

		else if (!(f_info[feat].flags1 & (FF1_MOVE))) return;

	}

	/* Monster is allowed to move */
	if (do_move)
	{
		/* The grid is occupied by a monster. */
		if (cave_m_idx[ny][nx] > 0)
		{
			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];
			monster_race *nr_ptr = &r_info[n_ptr->r_idx];

			/* XXX - Kill weaker monsters */
			if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (!(nr_ptr->flags1 & (RF1_UNIQUE))) &&
			    (r_ptr->mexp > nr_ptr->mexp))
			{
				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Generate treasure, etc */
				monster_death(cave_m_idx[ny][nx]);

				/* Delete the monster */
				delete_monster_idx(cave_m_idx[ny][nx]);

			}

			/* Attack if confused and not fleeing */
			/* XXX XXX Should use seperate routine */
			else if (m_ptr->confused)
			{
				int ap_cnt;

				do_move = FALSE;

				if (!(m_ptr->monfear))
				{
					/* Scan through all four blows */
					for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
					{
						int damage = 0;

						/* Extract the attack infomation */
						int effect = r_ptr->blow[ap_cnt].effect;
						int method = r_ptr->blow[ap_cnt].method;
						int d_dice = r_ptr->blow[ap_cnt].d_dice;
						int d_side = r_ptr->blow[ap_cnt].d_side;


						/* Hack -- no more attacks */
						if (!method) break;

						/* Hack --- always hit, never display message XXX XXX XXX */

						/* Roll out the damage */
						damage = damroll(d_dice, d_side);

						/* New result routine */
						project_p(m_idx,0,ny,nx,damage,effect);
					}
				}
			}

			/* Swap with or push aside the other monster */
			else
			{
				/* The other monster cannot switch places */
				if (!cave_exist_mon(n_ptr->r_idx, m_ptr->fy, m_ptr->fx, TRUE))
				{
					/* Try to push it aside */
					if (!push_aside(m_ptr, n_ptr))
					{
						/* Cancel move on failure */
						do_move = FALSE;
					}
				}

				/* Note */
				if (do_move) did_move_body = TRUE;
			}
		}
	}

	/* Monster can (still) move */
	if (do_move)
	{
		/* Hidden ? */
		bool hidden = ((m_ptr->mflag & (MFLAG_HIDE))!=0);

		/* Hide monster if allowed */
		monster_hide(ny,nx,mmove,m_ptr);

		/* Hack --- tell the player if something hides */
		if (!(hidden) && (m_ptr->mflag & (MFLAG_HIDE)) && (m_ptr->ml))
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			msg_format("%^s hides in %s%s.",m_name,
			((f_info[cave_feat[ny][nx]].flags2 & (FF2_FILLED))?"":"the "),
			f_name+f_info[cave_feat[ny][nx]].name);
		}

		/* Move the monster */
		monster_swap(oy, ox, ny, nx);

		/* Cancel target when reached */
		if ((m_ptr->ty == ny) && (m_ptr->tx == nx))
		{
			m_ptr->ty = 0;
			m_ptr->tx = 0;
		}

		/* Hack --- tell the player if something unhides */
		if ((hidden) && !(m_ptr->mflag & (MFLAG_HIDE)) && (m_ptr->ml))
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			msg_format("%^s emerges from %s%s.",m_name,
			((f_info[cave_feat[oy][ox]].flags2 & (FF2_FILLED))?"":"the "),
			f_name+f_info[cave_feat[oy][ox]].name);
		}

		/* Possible disturb */
		if (m_ptr->ml &&
		    (disturb_move ||
		     ((m_ptr->mflag & (MFLAG_VIEW)) &&
		      disturb_near)))
		{
			/* Disturb */
			disturb(0, 0);
		}

		/* Hit traps */
		if (f_info[cave_feat[ny][nx]].flags1 & (FF1_HIT_TRAP) &&
			!(m_ptr->mflag & (MFLAG_OVER)))
		{
			mon_hit_trap(m_idx,ny,nx);
		}
		/* Hit other terrain */
		else if ((!mon_resist_feat(cave_feat[ny][nx],m_ptr->r_idx))&&
			!(m_ptr->mflag & (MFLAG_OVER)))
		{
			mon_hit_trap(m_idx,ny,nx);
		}

		/* XXX XXX Note we don't hit the old monster with traps/terrain */

		/* Leave tracks */
		if (f_info[cave_feat[ny][nx]].flags2 & (FF2_KILL_MOVE))
		{
			if (!(m_ptr->mflag & (MFLAG_OVER))) cave_alter_feat(ny, nx, FS_KILL_MOVE);
		}
		else if (f_info[cave_feat[oy][ox]].flags1 & (FF1_FLOOR))
		{
			if ((r_ptr->flags7 & (RF7_HAS_BLOOD)) && (m_ptr->hp < m_ptr->maxhp/3) && (rand_int(100)<30))
				cave_set_feat(oy, ox, FEAT_FLOOR_BLOOD_T);
			else if (r_ptr->flags7 & (RF7_HAS_SLIME))
				cave_set_feat(oy, ox, FEAT_FLOOR_SLIME_T);
			else if (r_ptr->flags2 & (RF2_HAS_WEB))
				cave_set_feat(oy, ox, FEAT_FLOOR_WEB);
		}
                else if (f_info[cave_feat[oy][ox]].flags2 & (FF2_CHASM))
		{
			if (r_ptr->flags2 & (RF2_HAS_WEB))
				cave_set_feat(oy, ox, FEAT_CHASM_WEB);
		}

		/*
		 * If a member of a monster group capable of smelling hits a 
		 * scent trail while out of LOS of the character, it will 
		 * communicate this to similar monsters.
		 */
		if ((!player_has_los_bold(ny, nx)) && (r_ptr->flags1 & (RF1_FRIENDS)) && 
		    (monster_can_smell(m_ptr)) && (get_scent(oy, ox) == -1) && 
		    (!m_ptr->ty) && (!m_ptr->tx))
		{
			int i;
			monster_type *n_ptr;
			monster_race *nr_ptr;

			/* Scan all other monsters */
			for (i = m_max - 1; i >= 1; i--)
			{
				/* Access the monster */
				n_ptr = &m_list[i];
				nr_ptr = &r_info[n_ptr->r_idx];

				/* Ignore dead monsters */
				if (!n_ptr->r_idx) continue;

				/* Ignore monsters with the wrong symbol */
				if (r_ptr->d_char != nr_ptr->d_char) continue;

				/* Ignore monsters with specific orders */
				if ((n_ptr->ty) || (n_ptr->ty)) continue;

				/* Ignore monsters picking up a good scent */
				if (get_scent(n_ptr->fy, n_ptr->fx) < SMELL_STRENGTH - 10)
					continue;

				/* Ignore monsters not in LOS */
				if (!los(m_ptr->fy, m_ptr->fx, n_ptr->fy, n_ptr->fx))
					continue;

				/* Activate all other monsters and give directions */
				n_ptr->csleep = 0;
				n_ptr->mflag |= (MFLAG_ACTV);
				n_ptr->ty = ny;   n_ptr->tx = nx;
			}
		}


		/* Player will always be disturbed if a monster is adjacent */
		if (m_ptr->cdis == 1)
		{
			disturb(1, 0);
		}

		/* Possible disturb */
		else if (m_ptr->ml && (disturb_move || 
			(m_ptr->mflag & (MFLAG_VIEW) && disturb_near)))
		{
			/* Disturb */
			disturb(0, 0);
		}


		/* Scan all objects in the grid */

		/* Can we get the objects */
		if ((f_info[cave_feat[ny][nx]].flags1 & (FF1_DROP)) &&
			!(m_ptr->mflag & (MFLAG_OVER)))
		{
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;
	
				/* Acquire object */
				o_ptr = &o_list[this_o_idx];
	
				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;
	
				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;
	
				/* Sneaky monsters hide behind big objects */
				if ((o_ptr->weight > 1500)
					&& (r_ptr->flags2 & (RF2_SNEAKY))
					&& !(m_ptr->mflag & (MFLAG_HIDE)))
				{
					char m_name[80];
					char o_name[80];
	
					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);
	
					/* Get the object name */
					object_desc(o_name, o_ptr, TRUE, 3);
	
					msg_format("%^s hides behind %s.",m_name, o_name);
	
					m_ptr->mflag |=(MFLAG_HIDE);
	
					did_sneak = TRUE;
	
				}
	
				/* Take or kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					u32b f1, f2, f3;
	
					u32b flg3 = 0L;
	
					char m_name[80];
					char o_name[120];
	
					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);
	
					/* Acquire the object name */
					object_desc(o_name, o_ptr, TRUE, 3);
	
					/* Acquire the monster name */
					monster_desc(m_name, m_ptr, 0x04);
	
	
	
					/* React to objects that hurt the monster */
					if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
					if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);
					if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_NATURAL)) flg3 |= (RF3_ANIMAL | RF3_PLANT | RF3_INSECT);
					if (f1 & (TR1_SLAY_EVIL)) flg3 |= (RF3_EVIL);
	
					/* The object cannot be picked up by the monster */
					if (artifact_p(o_ptr) || (r_ptr->flags3 & flg3))
					{
						/* Only give a message for "take_item" */
						if (r_ptr->flags2 & (RF2_TAKE_ITEM))
						{
							/* Take note */
							did_take_item = TRUE;
	
							/* Describe observable situations */
							if (m_ptr->ml && player_has_los_bold(ny, nx))
							{
								/* Dump a message */
								msg_format("%^s tries to pick up %s, but fails.",
									   m_name, o_name);
								/* Mark object as ungettable? */
								if ((o_ptr->discount == 0) &&
									!(o_ptr->ident & (IDENT_SENSE))
									&& !(object_known_p(o_ptr)))
								{
	
									/* Sense the object */
									o_ptr->discount = INSCRIP_UNGETTABLE;
	
									/* The object has been "sensed" */
									o_ptr->ident |= (IDENT_SENSE);
								}
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
						if (player_has_los_bold(ny, nx))
						{
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
							/* Dump a message */
							msg_format("%^s crushes %s.", m_name, o_name);
						}
	
						/* Delete the object */
						delete_object_idx(this_o_idx);
					}
				}
			}
		}
	}	     /* End of monster's move */

	/* Notice changes in view */
	if (do_view)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}

	/* Learn things from observable monster */
	if (m_ptr->ml)
	{
		/* Monster opened a door */
		if (did_open_door) l_ptr->flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		/* XXX Fleeing monsters will always push past others, so need to check */
		if ((did_move_body) && (r_ptr->flags2 & RF2_MOVE_BODY)) l_ptr->flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->flags2 |= (RF2_KILL_WALL);

		/* Monster disarmed a trap */
		if (did_smart) l_ptr->flags2 |= (RF2_SMART);

		/* Monster hide behind an object */
		if (did_sneak) l_ptr->flags2 |= (RF2_SNEAKY);

		/* Monster is climbing */
		/* XXX Rubble, trees, webs will always result in climbing, so need to check */
		if ((mmove == MM_CLIMB) && (r_ptr->flags2 & RF2_CAN_CLIMB)) l_ptr->flags2 |= (RF2_CAN_CLIMB);

		/* Monster is flying */
		if (mmove == MM_FLY) l_ptr->flags2 |= (RF2_CAN_FLY);

		/* Monster must swim */
		if ((mmove == MM_FLY) && (r_ptr->flags2 & (RF2_MUST_FLY))) l_ptr->flags2 |= (RF2_MUST_FLY);

		/* Monster is swimming */
		if (mmove == MM_SWIM) l_ptr->flags2 |= (RF2_CAN_SWIM);

		/* Monster must swim */
		if ((mmove == MM_SWIM) && (r_ptr->flags2 & (RF2_MUST_SWIM))) l_ptr->flags2 |= (RF2_MUST_SWIM);

		/* Monster is digging */
		if (mmove == MM_DIG) l_ptr->flags2 |= (RF2_CAN_DIG);

		/* Monster is oozing */
		if (mmove == MM_OOZE) l_ptr->flags3 |= (RF3_OOZE);

		/* Monster is passing */
		if (mmove == MM_PASS) l_ptr->flags2 |= (RF2_PASS_WALL);

	}
}


/*
 * Monster takes its turn.
 */
static void process_monster(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, k, y, x;
	int ty, tx;
	int dir;
	bool fear;

	bool bash;

	/* Assume the monster is able to perceive the player. */
	bool aware = TRUE;
	bool must_use_target = FALSE;

	/* Will the monster move randomly? */
	bool random = FALSE;


	/* If monster is sleeping, or in stasis, it loses its turn. */
	if (m_ptr->csleep) return;

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
		    (!m_ptr->ty) && (!m_ptr->tx))
		{
			/* Monster cannot smell the character */
			if (!cave_when[m_ptr->fy][m_ptr->fx]) m_ptr->mflag &= ~(MFLAG_ACTV);
			else if (!monster_can_smell(m_ptr))   m_ptr->mflag &= ~(MFLAG_ACTV);
		}
	}

	/* Monster is in passive mode. */
	else
	{
		/* Character is inside scanning range */
		if (m_ptr->cdis <= r_ptr->aaf) m_ptr->mflag |= (MFLAG_ACTV);

		/* Monster has a target */
		else if ((m_ptr->ty) && (m_ptr->tx)) m_ptr->mflag |= (MFLAG_ACTV);

		/* The monster is catching too much of a whiff to ignore */
		else if (cave_when[m_ptr->fy][m_ptr->fx])
		{
			if (monster_can_smell(m_ptr)) m_ptr->mflag |= (MFLAG_ACTV);
		}
	}


	/* A monster in passive mode will end its turn at this point. */
	if (!(m_ptr->mflag & (MFLAG_ACTV))) return;


	/* Hack -- Always redraw the current target monster health bar */
	if (p_ptr->health_who == cave_m_idx[m_ptr->fy][m_ptr->fx]) 
		p_ptr->redraw |= (PR_HEALTH);


	/* Attempt to multiply if able to and allowed */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
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
			if (multiply_monster(cave_m_idx[m_ptr->fy][m_ptr->fx]))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}


	/*** Ranged attacks ***/

	if (make_attack_spell(m_idx)) return;

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
		int chance = 0;

		/* RAND_25 and RAND_50 are cumulative */
		if (r_ptr->flags1 & (RF1_RAND_25))
		{
			chance += 25;
			if (m_ptr->ml) l_ptr->flags1 |= (RF1_RAND_25);
		}
		if (r_ptr->flags1 & (RF1_RAND_50))
		{
			chance += 50;
			if (m_ptr->ml) l_ptr->flags1 |= (RF1_RAND_50);
		}

		/* Chance of moving randomly */
		if (rand_int(100) < chance) random = TRUE;
	}


	/* Monster cannot perceive the character */
	if ((!aware) && (!random))
	{
		/* Monster has a known target */
		if ((m_ptr->ty) && (m_ptr->tx)) must_use_target = TRUE;

		/* Monster is just going to have to search at random */
		else random = TRUE;
	}


	/*** Find a target to move to ***/

	/* Monster is genuinely confused */
	if (m_ptr->confused)
	{
		/* Choose any direction except five and zero */
		dir = rand_int(8);

		/* Monster can try to wander into /anything/... */
		ty = m_ptr->fy + ddy_ddd[dir];
		tx = m_ptr->fx + ddx_ddd[dir];
	}

	/* Monster isn't confused, just moving semi-randomly */
	else if (random)
	{
		int start = rand_int(8);
		bool dummy;

		/* Is the monster scared? */
		if ((!(r_ptr->flags1 & (RF1_NEVER_MOVE))) &&
		    ((m_ptr->min_range == FLEE_RANGE) ||
		     (m_ptr->monfear)))
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
		if ((ty == 0) && (tx == 0)) return;
	}

	/* Normal movement */
	else
	{
		/* Choose a pair of target grids, or cancel the move. */
		if (!get_move(m_ptr, &ty, &tx, &fear, must_use_target))
			return;
	}

	/* Calculate the actual move.  Cancel move on failure to enter grid. */
	if (!make_move(m_ptr, &ty, &tx, fear, &bash)) return;

	/* Change terrain, move the monster, handle secondary effects. */
	process_move(m_idx, ty, tx, bash);

	/* End turn */
	return;
}


/*
 * Monster regeneration of HPs and mana, and recovery from all temporary 
 * conditions.
 *
 * This function is called a lot, and is therefore fairly expensive.
 */
static void recover_monster(int m_idx, bool regen)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int frac;

	/* Get the origin */
	int y = m_ptr->fy;
	int x = m_ptr->fx;

	/* Handle "summoned" */
	if (m_ptr->summoned)
	{
		m_ptr->summoned--;

		if (!m_ptr->summoned)
		{
			/* Delete monster summoned by player */
		}
	}

	/* Get hit by terrain continuously */
	if (!(place_monster_here(y,x,m_ptr->r_idx)))
	{

		bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

		bool daytime = ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

		bool hurt_lite = r_ptr->flags3 & (RF3_HURT_LITE);

		bool outside = (f_info[cave_feat[y][x]].flags3 & (FF3_OUTSIDE));

		/* Hack -- silently wake monster */
		m_ptr->csleep = 0;

		if (surface && daytime && hurt_lite && outside)
		{
			/* Burn the monster */
			project_m(0, 0, y, x, damroll(4,6), GF_LITE);

		}
		else if ((f_info[cave_feat[y][x]].blow.method) && !(f_info[cave_feat[y][x]].flags1 & (FF1_HIT_TRAP)))
		{
			mon_hit_trap(m_idx,y,x);
		}

		/* Is the monster hidden?*/
		if (m_ptr->mflag & (MFLAG_HIDE))
		{

			/* Unhide the monster */
			m_ptr->mflag &= ~(MFLAG_HIDE);

			/* And reveal */
			update_mon(m_idx,FALSE);

			/* Hack --- tell the player if something unhides */
			if (m_ptr->ml)
			{

				char m_name[80];

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				msg_format("%^s emerges from %s%s.",m_name,
					((f_info[cave_feat[m_ptr->fy][m_ptr->fx]].flags2 & (FF2_FILLED))?"":"the "),
					f_name+f_info[cave_feat[m_ptr->fy][m_ptr->fx]].name);
			}
		}
	}

	/* Every 100 game turns, regenerate monsters */
	if (regen)
	{

		if (m_ptr->hp < m_ptr->maxhp)
		{
			/* Base regeneration */
			frac = m_ptr->maxhp / 100;

			/* Minimal regeneration rate */
			if (!frac) frac = 1;

			/* Some monsters regenerate quickly */
			if (r_ptr->flags2 & (RF2_REGENERATE)) frac *= 2;

			/* Regenerate */
			m_ptr->hp += frac;

			/* Do not over-regenerate */
			if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

			/* Fully healed -> flag minimum range for recalculation */
			if (m_ptr->hp == m_ptr->maxhp) m_ptr->min_range = 0;
		}
	}


	/* Monster is sleeping, but character is within detection range */
	if ((m_ptr->csleep) && (m_ptr->cdis <= r_ptr->aaf))
	{
		u32b notice;

		/* Anti-stealth */
		notice = rand_int(1024);

		/* Aggravated by the player */
		if (p_ptr->aggravate)
		{
			/* Reset sleep counter */
			m_ptr->csleep = 0;

			/* Notice the "waking up" */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s wakes up.", m_name);
			}
		}

		/* Hack -- See if monster "notices" player */
		else if ((notice * notice * notice) <= p_ptr->noise)
		{
			int d = 1;

			/* Wake up faster near the player */
			if (m_ptr->cdis < 50) d = (100 / m_ptr->cdis);

			/* Still asleep */
			if (m_ptr->csleep > d)
			{
				/* Monster wakes up "a little bit" */
				m_ptr->csleep -= d;

				/* Notice the "not waking up" */
				if (m_ptr->ml)
				{
					/* Hack -- Count the ignores */
					if (l_ptr->ignore < MAX_UCHAR)
					{
						l_ptr->ignore++;
					}
				}
			}

			/* Just woke up */
			else
			{
				/* Reset sleep counter */
				m_ptr->csleep = 0;

				/* Notice the "waking up" */
				if (m_ptr->ml)
				{
					char m_name[80];

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0);

					/* Dump a message */
					msg_format("%^s wakes up.", m_name);

					/* Hack -- Update the health bar */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Hack -- Count the wakings */
					if (l_ptr->wake < MAX_UCHAR)
					{
						l_ptr->wake++;
					}
				}
			}
		}
	}

	/* Some monsters radiate damage when awake */
	if (!(m_ptr->csleep) && (r_ptr->flags2 & (RF2_HAS_AURA)))
	{
		(void)make_attack_spell_aux(m_idx,m_ptr->fy,m_ptr->fx,96+7);
	}


	/* Recover from stuns */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun. */
		if (rand_int(330) < r_ptr->level + 10)
		{
			/* Recover fully */
			d = m_ptr->stunned;
		}

		/* Hack -- Recover from stun */
		if (m_ptr->stunned > d)
		{
			/* Recover somewhat */
			m_ptr->stunned -= d;
		}

		/* Fully recover */
		else
		{
			/* Recover fully */
			m_ptr->stunned = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);
			}
		}
	}


	/* Recover from confusion */
	if (m_ptr->confused)
	{
		int d = randint(r_ptr->level / 10 + 1);

		/* Still confused */
		if (m_ptr->confused > d)
		{
			/* Reduce the confusion */
			m_ptr->confused -= d;
		}

		/* Recovered */
		else
		{
			/* No longer confused */
			m_ptr->confused = 0;

			/* Message if visible */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);
			}
		}
	}


	/* Recover courage */
	if (m_ptr->monfear)
	{
		/* Random recovery from fear */
		int d = randint(r_ptr->level / 20 + 1);

		/* Still afraid */
		if (m_ptr->monfear > d)
		{
			/* Reduce the fear */
			m_ptr->monfear -= d;
		}

		/* Recover from fear, take note if seen */
		else
		{
			/* No longer afraid */
			m_ptr->monfear = 0;

			/* Recalculate minimum range immediately */
			find_range(m_ptr);

			/* Visual note - only if monster isn't terrified */
			if ((m_ptr->ml) && (m_ptr->min_range != FLEE_RANGE))
			{
				char m_name[80];
				char m_poss[80];

				/* Acquire the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);
			}
		}
	}


	/*
	 * Handle significantly hasted or slowed creatures.  Random variations 
	 * are not large enough to activate this code.
	 */
	if ((m_ptr->mspeed > r_ptr->speed + 4) || (m_ptr->mspeed < r_ptr->speed - 4))
	{
		/* 1.5% chance that slowed monsters will return to normal speed. */
		if ((m_ptr->mspeed < r_ptr->speed) && (rand_int(67) == 0))
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%s is no longer slowed.", m_name);
			}
		}

		/* 1% chance that hasted monsters will return to normal speed. */
		else if ((m_ptr->mspeed > r_ptr->speed) && (rand_int(100) == 0)) 
		{
			m_ptr->mspeed = r_ptr->speed;

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Acquire the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Message. */
				msg_format("%s is no longer hasted.", m_name);
			}
		}
	}


	/* Hack -- Update the health bar (always) */
	if (p_ptr->health_who == m_idx) 
		p_ptr->redraw |= (PR_HEALTH);
}


/*
 * Process all living monsters, once per game turn.
 *
 * Scan through the list of all living monsters, (backwards, so we can 
 * excise any "freshly dead" monsters).
 *
 * Every ten game turns, allow monsters to recover from temporary con-
 * ditions.  Every 100 game turns, regenerate monsters.  Give energy to 
 * each monster, and allow fully energized monsters to take their turns.
 *
 * This function and its children are responsible for at least a third of 
 * the processor time in normal situations.  If the character is resting, 
 * this may rise substantially.
 */
void process_monsters(byte minimum_energy)
{
	int i;
	monster_type *m_ptr;

	/* Only process some things every so often */
	bool recover = FALSE;
	bool regen = FALSE;

	/* Time out temporary conditions every ten game turns */
	if (turn % 10 == 0) 
	{
		recover = TRUE;

		/* Regenerate hitpoints and mana every 100 game turns */
		if (turn % 100 == 0) regen = TRUE;
	}

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Player is dead or leaving the current level */
		if (p_ptr->leaving) break;

		/* Access the monster */
		m_ptr = &m_list[i];

		/* Ignore dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Ignore monsters that have already been handled */
		if (m_ptr->moved) continue;

		/* Leave monsters without enough energy for later */
		if (m_ptr->energy < minimum_energy) continue;

		/* Prevent reprocessing */
		m_ptr->moved = TRUE;

		/* Handle temporary monster attributes every ten game turns */
		if (recover) recover_monster(i, regen);

		/* Give the monsters some energy */
		m_ptr->energy += extract_energy[m_ptr->mspeed];

		/* End the turn of monsters without enough energy to move */
		if (m_ptr->energy < 100) continue;

		/* Use up some energy */
		m_ptr->energy -= 100;

		/* Let the monster take its turn */
		process_monster(i);
	}
}


/*
 * Clear 'moved' status from all monsters.
 */
void reset_monsters(void)
{
	int i;
	monster_type *m_ptr;

	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

		/* Monster is ready to go again */
		m_ptr->moved = FALSE;
	}

}
		
