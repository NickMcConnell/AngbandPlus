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
                                        (void)project(who, 2, m_ptr->fy, m_ptr->fx, damroll(r_ptr->blow[i].d_side, r_ptr->blow[i].d_dice),
                                                 r_ptr->blow[i].effect, flg);

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
						project_p(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);
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
						project_p(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect);
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
					(void)project(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect, flg);

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
					(void)project(who, 0, y, x, damroll(r_ptr->blow[i].d_side,
							r_ptr->blow[i].d_dice),r_ptr->blow[i].effect, flg);

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

		/* RF4_XXX1X4 */
		case 96+6:
		{
			break;
		}

		/* RF4_XXX1X4 */
		case 96+7:
		{
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

		/* RF4_XXX8X4 */
		case 96+31:
		{
			break;
		}



		/* RF5_BA_ACID */
		case 128+0:
		{
			if (target < 0) disturb(1,0);

			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a stinking cloud.", m_name);
				else if (known) msg_format("%^s casts a stinking cloud at %s.", m_name, t_name);

				flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles powerfully.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles powerfully.", m_name, t_name);
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
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
      			                        equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
					}
					else
					{
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
                                                equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
					}
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
      			                        equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
					}
					else
					{
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
                                                equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
					}
					if (!p_ptr->free_act)
					{
						(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
      			                        equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
					}
					else
					{
#ifdef ALLOW_OBJECT_INFO
     		                           	/* Always notice */
                                                equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name, t_name);
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Sometimes notice */
                                        if (rand_int(100) < 30) equip_can_flags(0x0L,TR2_RES_FEAR,0x0L);
#endif

				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You refuse to be frightened.");
				}
				else
				{
					(void)set_afraid(p_ptr->afraid + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,TR2_RES_FEAR,0x0L);
#endif
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
                                        if (rand_int(100)<30) equip_can_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_blind(12 + rand_int(4));
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,TR2_RES_BLIND,0x0L);
#endif
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Sometimes notice */
                                        if (rand_int(100)<30) equip_can_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You disbelieve the feeble spell.");
				}
				else
				{
					(void)set_confused(p_ptr->confused + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,TR2_RES_CONFU,0x0L);
#endif
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
                                        equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_slow(p_ptr->slow + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
                                        equip_can_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,0x0L,TR3_FREE_ACT);
#endif
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
			n_ptr->hp += (rlev * 6);

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
				disturb(1, 0);
				msg_format("%^s blinks away.", m_name);
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
				disturb(1, 0);
				msg_format("%^s teleports away.", m_name);
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
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
                                        equip_can_flags(0x0L,TR2_RES_NEXUS,0x0L);
#endif
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
				}
				else
				{
					teleport_player_level();
#ifdef ALLOW_OBJECT_INFO
   		                        /* Always notice */
      			                equip_not_flags(0x0L,TR2_RES_NEXUS,0x0L);
#endif
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

		/* RF6_S_ANT */
		case 160+20:
		{
			if (surface) break;
			disturb(1, 0);
			if (who > 0)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s magically summons ants.", m_name);
				else msg_print("You hear distant chittering.");

                                /* Hack -- prevent summoning for a short while */
                                m_ptr->summoned = 20;
			}
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, rlev, SUMMON_ANT);
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

	int chance, thrown_spell, rlev;

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

	bool no_innate = FALSE;

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

	/* Hack -- Extract the spell probability */
	chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

	/* Not allowed to cast spells */
	if (!chance) return (FALSE);

#ifdef MONSTER_AI

	if (!smart_monsters)
	{
		/* Only do spells occasionally */
		if (rand_int(100) >= chance) return (FALSE);
	}
	else
	{
		/* Do spells more often, because they can fail */
		if (rand_int(100) >= 2 * chance) return (FALSE);

		/* Sometimes forbid innate attacks (breaths) */
		if (rand_int(100) >= chance) no_innate = TRUE;
	}

#else /* MONSTER_AI */

	/* Only do spells occasionally */
	if (rand_int(100) >= chance) return (FALSE);

#endif /* MONSTER_AI */



	/* Hack -- require projectable player */
	if (normal)
	{
		/* Check range */
		if (m_ptr->cdis > MAX_RANGE) return (FALSE);

		/* Check path */
		/*if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) return (FALSE);*/
	}


	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;


#ifdef MONSTER_AI

	/* Forbid innate attacks sometimes */
	if (no_innate)
	{
		f4 &= ~(RF4_INNATE_MASK);
		f5 &= ~(RF5_INNATE_MASK);
		f6 &= ~(RF6_INNATE_MASK);
	}

#endif /* MONSTER_AI */

        /* Forbid summoning sometimes */
        if (m_ptr->summoned)
	{
                f4 &= ~(RF4_SUMMON_MASK);
                f5 &= ~(RF5_SUMMON_MASK);
                f6 &= ~(RF6_SUMMON_MASK);
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

#ifdef DRS_SMART_OPTIONS

	/* Remove the "ineffective" spells */
        /*remove_bad_spells(m_idx, &f4, &f5, &f6);*/

	/* No spells left */
	if (!f4 && !f5 && !f6) return (FALSE);

#endif /* DRS_SMART_OPTIONS */


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

#ifdef MONSTER_AI
	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (!smart_monsters || r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

	/* Check for spell failure (innate attacks never fail) */
	if ((thrown_spell >= RF5_OFFSET) && (rand_int(100) < failrate))
	{
		/* Message */
		msg_format("%^s tries to cast a spell, but fails.", m_name);

		return (TRUE);
	}
#endif /* MONSTER_AI */


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
			l_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
			if (l_ptr->r_cast_inate < MAX_UCHAR) l_ptr->r_cast_inate++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32*5)
		{
			l_ptr->r_flags5 |= (1L << (thrown_spell - 32*4));
			if (l_ptr->r_cast_spell < MAX_UCHAR) l_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32*6)
		{
			l_ptr->r_flags6 |= (1L << (thrown_spell - 32*5));
			if (l_ptr->r_cast_spell < MAX_UCHAR) l_ptr->r_cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->r_deaths < MAX_SHORT))
	{
		l_ptr->r_deaths++;
	}


	/* A spell was cast */
	return (TRUE);
}



/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
static int mon_will_run(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

#ifdef ALLOW_TERROR

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b p_lev, m_lev;
	u16b p_chp, p_mhp;
	u16b m_chp, m_mhp;
	u32b p_val, m_val;

#endif /* ALLOW_TERROR */

	/* Keep monsters from running too far away */
	if (m_ptr->cdis > MAX_SIGHT + 5) return (FALSE);

	/* All "afraid" monsters will run away */
	if (m_ptr->monfear) return (TRUE);

#ifdef ALLOW_TERROR

	/* Nearby monsters will not become terrified */
	if (m_ptr->cdis <= 5) return (FALSE);

	/* Examine player power (level) */
	p_lev = p_ptr->lev;

	/* Examine monster power (level plus morale) */
	m_lev = r_ptr->level + (m_idx & 0x08) + 25;

	/* Optimize extreme cases below */
	if (m_lev > p_lev + 4) return (FALSE);
	if (m_lev + 4 <= p_lev) return (TRUE);

	/* Examine player health */
	p_chp = p_ptr->chp;
	p_mhp = p_ptr->mhp;

	/* Examine monster health */
	m_chp = m_ptr->hp;
	m_mhp = m_ptr->maxhp;

	/* Prepare to optimize the calculation */
	p_val = (p_lev * p_mhp) + (p_chp << 2); /* div p_mhp */
	m_val = (m_lev * m_mhp) + (m_chp << 2); /* div m_mhp */

	/* Strong players scare strong monsters */
	if (p_val * m_mhp > m_val * p_mhp) return (TRUE);

#endif /* ALLOW_TERROR */

	/* Assume no terror */
	return (FALSE);
}




#ifdef MONSTER_FLOW

/*
 * Choose the "best" direction for "flowing"
 *
 * Note that ghosts and rock-eaters are never allowed to "flow",
 * since they should move directly towards the player.
 *
 * Prefer "non-diagonal" directions, but twiddle them a little
 * to angle slightly towards the player's actual location.
 *
 * Allow very perceptive monsters to track old "spoor" left by
 * previous locations occupied by the player.  This will tend
 * to have monsters end up either near the player or on a grid
 * recently occupied by the player (and left via "teleport").
 *
 * Note that if "smell" is turned on, all monsters get vicious.
 *
 * Also note that teleporting away from a location will cause
 * the monsters who were chasing you to converge on that location
 * as long as you are still near enough to "annoy" them without
 * being close enough to chase directly.  I have no idea what will
 * happen if you combine "smell" with low "aaf" values.
 */
static bool get_moves_aux(int m_idx, int *yp, int *xp)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, y1, x1;

	int when = 0;
	int cost = 999;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!flow_by_sound) return (FALSE);

	/* Monster can go through rocks */
	if (r_ptr->flags2 & (RF2_PASS_WALL)) return (FALSE);
	if (r_ptr->flags2 & (RF2_KILL_WALL)) return (FALSE);

	/* Monster location */
	y1 = m_ptr->fy;
	x1 = m_ptr->fx;

	/* The player is not currently near the monster grid */
	if (cave_when[y1][x1] < cave_when[py][px])
	{
		/* The player has never been near the monster grid */
		if (cave_when[y1][x1] == 0) return (FALSE);

		/* The monster is not allowed to track the player */
		if (!flow_by_smell) return (FALSE);
	}

	/* Monster is too far away to notice the player */
	if (cave_cost[y1][x1] > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave_cost[y1][x1] > r_ptr->aaf) return (FALSE);

	/* Hack -- Player can see us, run towards him */
	if (player_has_los_bold(y1, x1)) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		/* Get the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when) continue;

		/* Ignore distant locations */
		if (cave_cost[y][x] > cost) continue;

		/* Save the cost and time */
		when = cave_when[y][x];
		cost = cave_cost[y][x];

		/* Hack -- Save the "twiddled" location */
		(*yp) = py + 16 * ddy_ddd[i];
		(*xp) = px + 16 * ddx_ddd[i];
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

	/* Success */
	return (TRUE);
}

#ifdef MONSTER_AI

/*
 * Provide a location to flee to, but give the player a wide berth.
 *
 * A monster may wish to flee to a location that is behind the player,
 * but instead of heading directly for it, the monster should "swerve"
 * around the player so that he has a smaller chance of getting hit.
 */
static bool get_fear_moves_aux(int m_idx, int *yp, int *xp)
{
	int y, x, y1, x1, fy, fx, py, px, gy = 0, gx = 0;
	int when = 0, score = -1;
	int i;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Monster flowing disabled */
	if (!flow_by_sound) return (FALSE);

	/* Player location */
	py = p_ptr->py;
	px = p_ptr->px;

	/* Monster location */
	fy = m_ptr->fy;
	fx = m_ptr->fx;

	/* Desired destination */
	y1 = fy - (*yp);
	x1 = fx - (*xp);

	/* The player is not currently near the monster grid */
	if (cave_when[fy][fx] < cave_when[py][px])
	{
		/* No reason to attempt flowing */
		return (FALSE);
	}

	/* Monster is too far away to use flow information */
	if (cave_cost[fy][fx] > MONSTER_FLOW_DEPTH) return (FALSE);
	if (cave_cost[fy][fx] > r_ptr->aaf) return (FALSE);

	/* Check nearby grids, diagonals first */
	for (i = 7; i >= 0; i--)
	{
		int dis, s;

		/* Get the location */
		y = fy + ddy_ddd[i];
		x = fx + ddx_ddd[i];

		/* Ignore illegal locations */
		if (cave_when[y][x] == 0) continue;

		/* Ignore ancient locations */
		if (cave_when[y][x] < when) continue;

		/* Calculate distance of this grid from our destination */
		dis = distance(y, x, y1, x1);

		/* Score this grid */
		s = 5000 / (dis + 3) - 500 / (cave_cost[y][x] + 1);

		/* No negative scores */
		if (s < 0) s = 0;

		/* Ignore lower scores */
		if (s < score) continue;

		/* Save the score and time */
		when = cave_when[y][x];
		score = s;

		/* Save the location */
		gy = y;
		gx = x;
	}

	/* No legal move (?) */
	if (!when) return (FALSE);

	/* Find deltas */
	(*yp) = fy - gy;
	(*xp) = fx - gx;

	/* Success */
	return (TRUE);
}

#endif /* MONSTER_AI */

#endif /* MONSTER_FLOW */


#ifdef MONSTER_AI

/*
 * Hack -- Precompute a bunch of calls to distance() in find_safety() and
 * find_hiding().
 *
 * The pair of arrays dist_offsets_y[n] and dist_offsets_x[n] contain the
 * offsets of all the locations with a distance of n from a central point,
 * with an offset of (0,0) indicating no more offsets at this distance.
 *
 * This is, of course, fairly unreadable, but it eliminates multiple loops
 * from the previous version.
 *
 * It is probably better to replace these arrays with code to compute
 * the relevant arrays, even if the storage is pre-allocated in hard
 * coded sizes.  At the very least, code should be included which is
 * able to generate and dump these arrays (ala "los()").  XXX XXX XXX
 *
 * Also, the storage needs could be reduced by using char.  XXX XXX XXX
 *
 * These arrays could be combined into two big arrays, using sub-arrays
 * to hold the offsets and lengths of each portion of the sub-arrays, and
 * this could perhaps also be used somehow in the "look" code.  XXX XXX XXX
 */


static sint d_off_y_0[] =
{ 0 };

static sint d_off_x_0[] =
{ 0 };


static sint d_off_y_1[] =
{ -1, -1, -1, 0, 0, 1, 1, 1, 0 };

static sint d_off_x_1[] =
{ -1, 0, 1, -1, 1, -1, 0, 1, 0 };


static sint d_off_y_2[] =
{ -1, -1, -2, -2, -2, 0, 0, 1, 1, 2, 2, 2, 0 };

static sint d_off_x_2[] =
{ -2, 2, -1, 0, 1, -2, 2, -2, 2, -1, 0, 1, 0 };


static sint d_off_y_3[] =
{ -1, -1, -2, -2, -3, -3, -3, 0, 0, 1, 1, 2, 2,
  3, 3, 3, 0 };

static sint d_off_x_3[] =
{ -3, 3, -2, 2, -1, 0, 1, -3, 3, -3, 3, -2, 2,
  -1, 0, 1, 0 };


static sint d_off_y_4[] =
{ -1, -1, -2, -2, -3, -3, -3, -3, -4, -4, -4, 0,
  0, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 0 };

static sint d_off_x_4[] =
{ -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, -4, 4,
  -4, 4, -3, 3, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_5[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -4, -4, -5, -5,
  -5, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 4, 4, 5, 5,
  5, 0 };

static sint d_off_x_5[] =
{ -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1, 0, 1,
  -5, 5, -5, 5, -4, 4, -4, 4, -2, -3, 2, 3, -1,
  0, 1, 0 };


static sint d_off_y_6[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
  5, 5, 6, 6, 6, 0 };

static sint d_off_x_6[] =
{ -6, 6, -5, 5, -5, 5, -4, 4, -2, -3, 2, 3, -1,
  0, 1, -6, 6, -6, 6, -5, 5, -5, 5, -4, 4, -2,
  -3, 2, 3, -1, 0, 1, 0 };


static sint d_off_y_7[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -5, -5,
  -6, -6, -6, -6, -7, -7, -7, 0, 0, 1, 1, 2, 2, 3,
  3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 0 };

static sint d_off_x_7[] =
{ -7, 7, -6, 6, -6, 6, -5, 5, -4, -5, 4, 5, -2,
  -3, 2, 3, -1, 0, 1, -7, 7, -7, 7, -6, 6, -6,
  6, -5, 5, -4, -5, 4, 5, -2, -3, 2, 3, -1, 0,
  1, 0 };


static sint d_off_y_8[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -6, -6, -7, -7, -7, -7, -8, -8, -8, 0, 0, 1, 1,
  2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7,
  8, 8, 8, 0 };

static sint d_off_x_8[] =
{ -8, 8, -7, 7, -7, 7, -6, 6, -6, 6, -4, -5, 4,
  5, -2, -3, 2, 3, -1, 0, 1, -8, 8, -8, 8, -7,
  7, -7, 7, -6, 6, -6, 6, -4, -5, 4, 5, -2, -3,
  2, 3, -1, 0, 1, 0 };


static sint d_off_y_9[] =
{ -1, -1, -2, -2, -3, -3, -4, -4, -5, -5, -6, -6,
  -7, -7, -7, -7, -8, -8, -8, -8, -9, -9, -9, 0,
  0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 7,
  7, 8, 8, 8, 8, 9, 9, 9, 0 };

static sint d_off_x_9[] =
{ -9, 9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4,
  -5, 4, 5, -2, -3, 2, 3, -1, 0, 1, -9, 9, -9,
  9, -8, 8, -8, 8, -7, 7, -7, 7, -6, 6, -4, -5,
  4, 5, -2, -3, 2, 3, -1, 0, 1, 0 };


static sint *dist_offsets_y[10] =
{
	d_off_y_0, d_off_y_1, d_off_y_2, d_off_y_3, d_off_y_4,
	d_off_y_5, d_off_y_6, d_off_y_7, d_off_y_8, d_off_y_9
};

static sint *dist_offsets_x[10] =
{
	d_off_x_0, d_off_x_1, d_off_x_2, d_off_x_3, d_off_x_4,
	d_off_x_5, d_off_x_6, d_off_x_7, d_off_x_8, d_off_x_9
};

#endif /* MONSTER_AI */


/*
 * Choose a "safe" location near a monster for it to run toward.
 *
 * A location is "safe" if it can be reached quickly and the player
 * is not able to fire into it (it isn't a "clean shot").  So, this will
 * cause monsters to "duck" behind walls.  Hopefully, monsters will also
 * try to run towards corridor openings if they are in a room.
 *
 * This function may take lots of CPU time if lots of monsters are fleeing.
 *
 * Return TRUE if a safe location is available.
 */
static bool find_safety(int m_idx, int *yp, int *xp)
{

#ifdef MONSTER_AI
#ifdef MONSTER_FLOW

	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 0;

	sint *y_offsets;
	sint *x_offsets;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Skip locations in a wall */
			if (!cave_floor_bold(y, x)) continue;

			/* Check for "availability" (if monsters can flow) */
			if (flow_by_sound)
			{
				/* Ignore grids very far from the player */
				if (cave_when[y][x] < cave_when[py][px]) continue;

				/* Ignore too-distant grids */
				if (cave_cost[y][x] > cave_cost[fy][fx] + 2 * d) continue;
			}

			/* Check for absence of shot (more or less) */
			if (!player_has_los_bold(y,x))
			{
				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if further than previous */
				if (dis > gdis)
				{
					gy = y;
					gx = x;
					gdis = dis;
				}
			}
		}

		/* Check for success */
		if (gdis > 0)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found safe place */
			return (TRUE);
		}
	}

#endif /* MONSTER_FLOW */
#endif /* MONSTER_AI */

	/* No safe place */
	return (FALSE);
}


#ifdef MONSTER_AI

/*
 * Choose a good hiding place near a monster for it to run toward.
 *
 * Pack monsters will use this to "ambush" the player and lure him out
 * of corridors into open space so they can swarm him.
 *
 * Return TRUE if a good location is available.
 */
static bool find_hiding(int m_idx, int *yp, int *xp)
{
	monster_type *m_ptr = &m_list[m_idx];

	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, y, x, dy, dx, d, dis;
	int gy = 0, gx = 0, gdis = 999, min;

	sint *y_offsets, *x_offsets;

	/* Closest distance to get */
	min = distance(py, px, fy, fx) * 3 / 4 + 2;

	/* Start with adjacent locations, spread further */
	for (d = 1; d < 10; d++)
	{
		/* Get the lists of points with a distance d from (fx, fy) */
		y_offsets = dist_offsets_y[d];
		x_offsets = dist_offsets_x[d];

		/* Check the locations */
		for (i = 0, dx = x_offsets[0], dy = y_offsets[0];
		     dx != 0 || dy != 0;
		     i++, dx = x_offsets[i], dy = y_offsets[i])
		{
			y = fy + dy;
			x = fx + dx;

			/* Skip illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Skip occupied locations */
			if (!cave_empty_bold(y, x)) continue;

			/* Check for hidden, available grid */
			if (!player_has_los_bold(y, x) && (clean_shot(fy, fx, y, x)))
			{
				/* Calculate distance from player */
				dis = distance(y, x, py, px);

				/* Remember if closer than previous */
				if (dis < gdis && dis >= min)
				{
					gy = y;
					gx = x;
					gdis = dis;
				}
			}
		}

		/* Check for success */
		if (gdis < 999)
		{
			/* Good location */
			(*yp) = fy - gy;
			(*xp) = fx - gx;

			/* Found good place */
			return (TRUE);
		}
	}

	/* No good place */
	return (FALSE);
}

#endif /* MONSTER_AI */


/*
 * Choose "logical" directions for monster movement
 *
 * We store the directions in a special "mm" array
 */
static bool get_moves(int m_idx, int mm[5])
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int y, ay, x, ax;

	int move_val = 0;

	int y2 = py;
	int x2 = px;

	bool done = FALSE;

#ifdef MONSTER_FLOW

	/* Flow towards the player */
	if (flow_by_sound)
	{
		/* Flow towards the player */
		(void)get_moves_aux(m_idx, &y2, &x2);
	}

#endif /* MONSTER_FLOW */

	/* Extract the "pseudo-direction" */
	y = m_ptr->fy - y2;
	x = m_ptr->fx - x2;


#ifdef MONSTER_AI

	/* Normal animal packs try to get the player out of corridors. */
	if (smart_packs &&
	    (r_ptr->flags1 & RF1_FRIENDS) && (r_ptr->flags3 & RF3_ANIMAL) &&
	    !((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_KILL_WALL))))
	{
		int i, room = 0;

		/* Count room grids next to player */
		for (i = 0; i < 8; i++)
		{
			/* Check grid */
			if (cave_info[py + ddy_ddd[i]][px + ddx_ddd[i]] & (CAVE_ROOM))
			{
				/* One more room grid */
				room++;
			}
		}

		/* Not in a room and strong player */
		if ((room < 8) && (p_ptr->chp > p_ptr->mhp / 2))
		{
			/* Find hiding place */
			if (find_hiding(m_idx, &y, &x)) done = TRUE;
		}
	}

#endif /* MONSTER_AI */

	/* Apply fear */
	if (!done && mon_will_run(m_idx))
	{
		/* Try to find safe place */
		if (!(smart_monsters && find_safety(m_idx, &y, &x)))
		{
			/* This is not a very "smart" method XXX XXX */
			y = (-y);
			x = (-x);
		}

#ifdef MONSTER_AI
#ifdef MONSTER_FLOW

		else
		{
			/* Attempt to avoid the player */
			if (flow_by_sound)
			{
				/* Adjust movement */
				if (get_fear_moves_aux(m_idx, &y, &x)) done = TRUE;
			}
		}

#endif /* MONSTER_FLOW */
#endif /* MONSTER_AI */

	}


#ifdef MONSTER_AI

	/* Monster groups try to surround the player */
	if (!done && smart_packs && (r_ptr->flags1 & RF1_FRIENDS))
	{
		int i;

		/* Find an empty square near the player to fill */
		for (i = 0; i < 8; i++)
		{
			/* Pick squares near player (semi-randomly) */
			y2 = py + ddy_ddd[(m_idx + i) & 7];
			x2 = px + ddx_ddd[(m_idx + i) & 7];

			/* Already there? */
			if ((m_ptr->fy == y2) && (m_ptr->fx == x2))
			{
				/* Attack the player */
				y2 = py;
				x2 = px;

				break;
			}

			/* Ignore filled grids */
			if (!cave_empty_bold(y2, x2)) continue;

			/* Try to fill this hole */
			break;
		}

		/* Extract the new "pseudo-direction" */
		y = m_ptr->fy - y2;
		x = m_ptr->fx - x2;

		/* Done */
		done = TRUE;
	}

#endif /* MONSTER_AI */


	/* Check for no move */
	if (!x && !y) return (FALSE);

	/* Extract the "absolute distances" */
	ax = ABS(x);
	ay = ABS(y);

	/* Do something weird */
	if (y < 0) move_val += 8;
	if (x > 0) move_val += 4;

	/* Prevent the diamond maneuvre */
	if (ay > (ax << 1))
	{
		move_val++;
		move_val++;
	}
	else if (ax > (ay << 1))
	{
		move_val++;
	}

	/* Analyze */
	switch (move_val)
	{
		case 0:
		{
			mm[0] = 9;
			if (ay > ax)
			{
				mm[1] = 8;
				mm[2] = 6;
				mm[3] = 7;
				mm[4] = 3;
			}
			else
			{
				mm[1] = 6;
				mm[2] = 8;
				mm[3] = 3;
				mm[4] = 7;
			}
			break;
		}

		case 1:
		case 9:
		{
			mm[0] = 6;
			if (y < 0)
			{
				mm[1] = 3;
				mm[2] = 9;
				mm[3] = 2;
				mm[4] = 8;
			}
			else
			{
				mm[1] = 9;
				mm[2] = 3;
				mm[3] = 8;
				mm[4] = 2;
			}
			break;
		}

		case 2:
		case 6:
		{
			mm[0] = 8;
			if (x < 0)
			{
				mm[1] = 9;
				mm[2] = 7;
				mm[3] = 6;
				mm[4] = 4;
			}
			else
			{
				mm[1] = 7;
				mm[2] = 9;
				mm[3] = 4;
				mm[4] = 6;
			}
			break;
		}

		case 4:
		{
			mm[0] = 7;
			if (ay > ax)
			{
				mm[1] = 8;
				mm[2] = 4;
				mm[3] = 9;
				mm[4] = 1;
			}
			else
			{
				mm[1] = 4;
				mm[2] = 8;
				mm[3] = 1;
				mm[4] = 9;
			}
			break;
		}

		case 5:
		case 13:
		{
			mm[0] = 4;
			if (y < 0)
			{
				mm[1] = 1;
				mm[2] = 7;
				mm[3] = 2;
				mm[4] = 8;
			}
			else
			{
				mm[1] = 7;
				mm[2] = 1;
				mm[3] = 8;
				mm[4] = 2;
			}
			break;
		}

		case 8:
		{
			mm[0] = 3;
			if (ay > ax)
			{
				mm[1] = 2;
				mm[2] = 6;
				mm[3] = 1;
				mm[4] = 9;
			}
			else
			{
				mm[1] = 6;
				mm[2] = 2;
				mm[3] = 9;
				mm[4] = 1;
			}
			break;
		}

		case 10:
		case 14:
		{
			mm[0] = 2;
			if (x < 0)
			{
				mm[1] = 3;
				mm[2] = 1;
				mm[3] = 6;
				mm[4] = 4;
			}
			else
			{
				mm[1] = 1;
				mm[2] = 3;
				mm[3] = 4;
				mm[4] = 6;
			}
			break;
		}

		default: /* case 12: */
		{
			mm[0] = 1;
			if (ay > ax)
			{
				mm[1] = 2;
				mm[2] = 4;
				mm[3] = 3;
				mm[4] = 7;
			}
			else
			{
				mm[1] = 4;
				mm[2] = 2;
				mm[3] = 7;
				mm[4] = 3;
			}
			break;
		}
	}

	/* Want to move */
	return (TRUE);
}



/*
 * Hack -- compare the "strength" of two monsters XXX XXX XXX
 */
static int compare_monsters(monster_type *m_ptr, monster_type *n_ptr)
{
	monster_race *r_ptr;

	u32b mexp1, mexp2;

	/* Race 1 */
	r_ptr = &r_info[m_ptr->r_idx];

	/* Extract mexp */
	mexp1 = r_ptr->mexp;

	/* Race 2 */
	r_ptr = &r_info[n_ptr->r_idx];

	/* Extract mexp */
	mexp2 = r_ptr->mexp;

	/* Compare */
	if (mexp1 < mexp2) return (-1);
	if (mexp1 > mexp2) return (1);

	/* Assume equal */
	return (0);
}


/*
 * Handle monster hitting a real trap.
 */
void mon_hit_trap(int m_idx, int y, int x)
{
	int dam;

	feature_type *f_ptr;
        monster_type *m_ptr = &m_list[m_idx];

	/* Option */
	if (!variant_hit_traps) return;

        /* Hack --- don't activate unknown invisible traps */
        if ((cave_feat[y][x] == FEAT_INVIS) && !(cave_info[y][x] & (CAVE_MARK))) return;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];

        /* Hack --- trapped doors */
        /* XXX XXX Dangerous */
        while (!(f_ptr->spell) && !(f_ptr->blow.method) && (f_ptr->flags1 & (FF1_TRAP)))
        {
                pick_trap(y,x);

                /* Get feature (again) */
                f_ptr = &f_info[cave_feat[y][x]];

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
 * Process a monster
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * In addition, monsters which *cannot* open or bash down a door
 * will still stand there trying to open it...  XXX XXX XXX
 *
 * Technically, need to check for monster in the way combined
 * with that monster being in a wall (or door?) XXX
 */
static void process_monster(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int i, d, oy, ox, ny, nx;

	int mm[5];

	bool stagger;

	bool do_turn;
	bool do_move;

	bool did_open_door;
	bool did_bash_door;
	bool did_take_item;
	bool did_kill_item;
	bool did_move_body;
	bool did_kill_body;
	bool did_pass_wall;
	bool did_kill_wall;
        bool did_smart;

	bool surface = (p_ptr->depth == min_depth(p_ptr->dungeon));

	bool daytime = ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2));

	bool hurt_lite = r_ptr->flags3 & (RF3_HURT_LITE);

	int mmove;

	bool desperate; /* Stuck in terrain we can't get out of */

        /* Handle "summoned" */
        if (m_ptr->summoned)
	{
                m_ptr->summoned--;

                if (!m_ptr->summoned)
                {
                        /* Delete monster summoned by player */
                }
        }

	/* Handle "sleep" */
	if (m_ptr->csleep)
	{
		u32b notice;

		/* Aggravation */
		if (p_ptr->aggravate)
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
			}

			/* Efficiency XXX XXX */
			return;
		}

		/* Anti-stealth */
		notice = rand_int(1024);

		/* Hack -- See if monster "notices" player */
		if ((notice * notice * notice) <= p_ptr->noise)
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
					if (l_ptr->r_ignore < MAX_UCHAR)
					{
						l_ptr->r_ignore++;
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
					if (l_ptr->r_wake < MAX_UCHAR)
					{
						l_ptr->r_wake++;
					}
				}
			}
		}

		/* Still sleeping */
		if (m_ptr->csleep) return;
	}


	/* Handle "stun" */
	if (m_ptr->stunned)
	{
		int d = 1;

		/* Make a "saving throw" against stun */
		if (rand_int(5000) <= r_ptr->level * r_ptr->level)
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

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer stunned.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Still stunned */
		if (m_ptr->stunned) return;
	}


	/* Handle confusion */
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

				/* Get the monster name */
				monster_desc(m_name, m_ptr, 0);

				/* Dump a message */
				msg_format("%^s is no longer confused.", m_name);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* Handle "fear" */
	if (m_ptr->monfear)
	{
		/* Amount of "boldness" */
		int d = randint(r_ptr->level / 10 + 1);

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

			/* Visual note */
			if (m_ptr->ml)
			{
				char m_name[80];
				char m_poss[80];

				/* Get the monster name/poss */
				monster_desc(m_name, m_ptr, 0);
				monster_desc(m_poss, m_ptr, 0x22);

				/* Dump a message */
				msg_format("%^s recovers %s courage.", m_name, m_poss);

				/* Hack -- Update the health bar */
				if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			}
		}
	}


	/* Reset */
	stagger = FALSE;
	desperate=FALSE;

	/* Get the origin */
	oy = m_ptr->fy;
	ox = m_ptr->fx;

	/* Get hit by terrain continuously, but not traps */
	if (((f_info[cave_feat[oy][ox]].blow.method) &&
	    !(f_info[cave_feat[oy][ox]].flags1 & (FF1_HIT_TRAP)) &&
             !(place_monster_here(oy,ox,m_ptr->r_idx)) &&
             !(m_ptr->mflag & (MFLAG_OVER))) || (surface && daytime && hurt_lite))
	{

                if (surface && daytime && hurt_lite)
		{
			/* Burn the monster */
			project_m(0,
				  0,
				  oy,
				  ox,
                                  damroll(4,6),
				  GF_LITE);

		}
		else mon_hit_trap(m_idx,oy,ox);

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

		/* Start feeling desperate */
		desperate = TRUE;

                /* Get moves */
                (void)get_moves(m_idx, mm);

		/* Process moves */
		for (i = 0; i < 5; i++)
		{
			/* Get the direction */
			d = (mm[i]);

			/* Get the destination */
			ny = oy + ddy[d];
			nx = ox + ddx[d];

			/* Hack --- require safe floor to not be desperate XXX XXX*/
                        if ((f_info[cave_feat[ny][nx]].flags1 & (FF1_MOVE)) && 
                                (place_monster_here(ny,nx,m_ptr->r_idx)))
			{
					desperate = FALSE;
			}

		}

		if (desperate) {

			/* Consider any direction */
			stagger = TRUE;

			/* Try again in any direction*/
			for (i = 0; i < 8; i++)
			{
                                d = ddd[i];

				/* Get the destination */
                                ny = oy + ddy[d];
                                nx = ox + ddx[d];

				/* Hack --- require safe floor not to be desperate XXX XXX*/
                                if ((f_info[cave_feat[ny][nx]].flags1 & (FF1_MOVE)) && 
                                        (place_monster_here(ny,nx,m_ptr->r_idx)))
				{
						desperate = FALSE;
				}
			}
		}
	}

	/* Attempt to "mutiply" if able and allowed */
	else if ((r_ptr->flags2 & (RF2_MULTIPLY)) && (num_repro < MAX_REPRO))
	{
		int k, y, x;

		/* Count the adjacent monsters */
		for (k = 0, y = oy - 1; y <= oy + 1; y++)
		{
			for (x = ox - 1; x <= ox + 1; x++)
			{
				/* Count monsters */
				if (cave_m_idx[y][x] > 0) k++;
			}
		}

		/* Hack -- multiply slower in crowded areas */
		if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
		{
			/* Try to multiply */
			if (multiply_monster(m_idx))
			{
				/* Take note if visible */
				if (m_ptr->ml)
				{
					l_ptr->r_flags2 |= (RF2_MULTIPLY);
				}

				/* Multiplying takes energy */
				return;
			}
		}
	}

	/* Attempt to cast a spell */
	if (make_attack_spell(m_idx)) return;


	/* Confused */
	if (m_ptr->confused)
	{
		/* Stagger */
		stagger = TRUE;
	}

	/* Random movement */
	else if (r_ptr->flags1 & (RF1_RAND_50 | RF1_RAND_25))
	{
		/* Random movement (25%) */
		if (!(r_ptr->flags1 & (RF1_RAND_50)))
		{
			/* Random */
			if (rand_int(100) < 25)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->r_flags1 |= (RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (50%) */
		else if (!(r_ptr->flags1 & (RF1_RAND_25)))
		{
			/* Random */
			if (rand_int(100) < 50)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->r_flags1 |= (RF1_RAND_50);

				/* Stagger */
				stagger = TRUE;
			}
		}

		/* Random movement (75%) */
		else
		{
			/* Random */
			if (rand_int(100) < 75)
			{
				/* Memorize flags */
				if (m_ptr->ml) l_ptr->r_flags1 |= (RF1_RAND_50 | RF1_RAND_25);

				/* Stagger */
				stagger = TRUE;
			}
		}
	}

	/* Normal movement */
	if (!stagger)
	{
		/* Logical moves, may do nothing */
		if (!get_moves(m_idx, mm)) return;
	}


	/* Assume nothing */
	do_turn = FALSE;
	do_move = FALSE;

	/* Assume nothing */
	did_open_door = FALSE;
	did_bash_door = FALSE;
	did_take_item = FALSE;
	did_kill_item = FALSE;
	did_move_body = FALSE;
	did_kill_body = FALSE;
	did_pass_wall = FALSE;
	did_kill_wall = FALSE;
        did_smart = FALSE;

	/* Not moving funny */
	mmove = MM_FAIL;

	/* Process moves */
	for (i = 0; i < 5; i++)
	{
		/* Get the direction (or stagger) */
		d = (stagger ? ddd[rand_int(8)] : mm[i]);

		/* Get the destination */
		ny = oy + ddy[d];
		nx = ox + ddx[d];

		mmove = place_monster_here(ny,nx,m_ptr->r_idx);

		/* The player is in the way.  Attack him. */
		if (cave_m_idx[ny][nx] < 0)
		{

			do_move = TRUE;

		}
		/* The monster is under covered terrain, moving to uncovered terrain. */
		else if ((m_ptr->mflag & (MFLAG_HIDE)) && (f_info[cave_feat[oy][ox]].flags2 & (FF2_COVERED)) &&
			!(f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED)) && (mmove != MM_FAIL))
		{


			if ((mmove == MM_SWIM) || (mmove == MM_DIG))
			{
				do_move = TRUE;
			}

			else if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[oy][ox]].flags1 & (FF1_BASH)))
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

				do_turn = TRUE;
			}
		}

		/* Monster is on covered terrain, moving to covered terrain */
		else if (!(m_ptr->mflag & (MFLAG_HIDE)) && (f_info[cave_feat[oy][ox]].flags2 & (FF2_COVERED)) &&
			(f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED)) && ((mmove == MM_SWIM) || (mmove == MM_DIG)))
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

				do_move = TRUE;

				mmove = MM_WALK;

				do_turn = TRUE;
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

				do_turn = TRUE;
			}

			else if ((r_ptr->flags2 & (RF2_CAN_FLY)) &&
				 (f_info[cave_feat[ny][nx]].flags2 & (FF2_CAN_FLY)))
			{
				do_move = TRUE;

				mmove = MM_FLY;
			}

			else if (!(r_ptr->flags2 & (RF2_MUST_SWIM)) &&
				(mon_resist_feat(f_info[cave_feat[ny][nx]].mimic,m_ptr->r_idx)))
			{
				do_move = TRUE;

				mmove = MM_WALK;
			}
			
		}
		/* Floor is open? */
		else if (f_info[cave_feat[ny][nx]].flags1 & (FF1_MOVE))
		{

			/* Hack -- check to see if can stand on terrain */
			if ((m_ptr->confused) || (desperate) ||
				(mmove))
			{

				/* Go ahead and move */
				do_move = TRUE;
			}
	
		}

		/* Permanent wall */
		else if ((cave_feat[ny][nx] >= FEAT_PERM_EXTRA) &&
			 (cave_feat[ny][nx] < FEAT_UPPER_EXTRA))
		{
			/* Nothing */
		}

		/* Monster moves climbing or flying */
		else if (((mmove == MM_CLIMB) || (mmove == MM_FLY)) &&
			  (f_info[cave_feat[ny][nx]].flags2 & (FF2_CAN_FLY)))
		{

			/* Pass through abyss */
			do_move = TRUE;
		}


		/* Monster moves swimming */
		else if ((mmove == MM_SWIM) &&
			  (f_info[cave_feat[ny][nx]].flags2 & (FF2_CAN_SWIM)))
		{
			/* Pass through underwater */
			do_move = TRUE;
		}

		/* Monster moves digging */
		else if ((mmove == MM_DIG) &&
			  (f_info[cave_feat[ny][nx]].flags2 & (FF2_CAN_DIG)))
		{
			/* Pass through earth/rubble */
			do_move = TRUE;
		}

		/* Monster moves through walls (and doors) */
		else if (r_ptr->flags2 & (RF2_PASS_WALL))
		{
			/* Pass through walls/doors/rubble */
			do_move = TRUE;

			/* Monster went through a wall */
			did_pass_wall = TRUE;
		}

		/* Monster destroys walls (and doors) */
		else if ((r_ptr->flags2 & (RF2_KILL_WALL)) &&
			 (mon_resist_feat(f_info[cave_feat[ny][nx]].mimic,m_ptr->r_idx)))
		{
			/* Eat through walls/doors/rubble */
			do_move = TRUE;

			/* Monster destroyed a wall */
			did_kill_wall = TRUE;

			/* Forget the wall */
			cave_info[ny][nx] &= ~(CAVE_MARK);

			/* Notice */
			cave_alter_feat(ny, nx, FS_TUNNEL);
		}

                /* Handle doors/trapped doors */
                else if (mon_resist_feat(cave_feat[ny][nx],m_ptr->r_idx) &&
                        (f_info[cave_feat[ny][nx]].flags1 & (FF1_DOOR)))
		{
                        /* May bash ? */
                        bool may_bash = TRUE;

                        /* Hack --- delay hitting traps */
                        if (f_info[cave_feat[ny][nx]].flags1 & (FF1_HIT_TRAP))
                        {
                                /* Do move */
                                do_move = TRUE;

                                /* Don't bash */
                                may_bash = FALSE;
                        }
			/* Creature can open doors. */
                        if (r_ptr->flags2 & (RF2_OPEN_DOOR))
			{
				/* Locked doors (not jammed) */
				if (f_info[cave_feat[ny][nx]].flags1 & (FF1_OPEN))
				{                                       

					int k;

					/* Take a turn */
					do_turn = TRUE;


					/* Door power */
					k = f_info[cave_feat[ny][nx]].power;
#if 0
					/* XXX XXX XXX Old test (pval 10 to 20) */
					if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
					    40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif

					/* Try to unlock it XXX XXX XXX */
					if ((!k) || (rand_int(m_ptr->hp / 10) > k))
					{
						/* Unlock the door */
						cave_alter_feat(ny, nx, FS_OPEN);

						/* Do not bash the door */
						may_bash = FALSE;
					}
				}
			}

			/* Stuck doors -- attempt to bash them down if allowed */
			if (may_bash && (r_ptr->flags2 & (RF2_BASH_DOOR)))
			{
				int k;


				/* Take a turn */
				do_turn = TRUE;

				/* Door power */
				k = f_info[cave_feat[ny][nx]].power;

#if 0
				/* XXX XXX XXX Old test (pval 10 to 20) */
				if (randint((m_ptr->hp + 1) * (50 + o_ptr->pval)) <
				    40 * (m_ptr->hp - 10 - o_ptr->pval));
#endif

				/* Attempt to Bash XXX XXX XXX */
				if (rand_int(m_ptr->hp / 10) > k)
				{
					/* Unlock the door */
					cave_alter_feat(ny, nx, FS_BASH);

					/* Message */
					msg_print("You hear a door burst open!");

					/* Disturb (sometimes) */
					if (disturb_minor) disturb(0, 0);

					/* The door was bashed open */
					did_bash_door = TRUE;

					/* Hack -- fall into doorway */
					do_move = TRUE;

				}
			}
		}

		/* Hack -- check for Glyph of Warding */
		if (do_move && (f_info[cave_feat[ny][nx]].flags1 & (FF1_GLYPH)))
		{
			/* Assume no move allowed */
			do_move = FALSE;

			/* Break the ward */
			if (randint(BREAK_GLYPH) < r_ptr->level)
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

				/* Allow movement */
				do_move = TRUE;
			}
		}



		/* Some monsters never attack */
		if (do_move && (cave_m_idx[ny][nx] < 0) &&
		    (r_ptr->flags1 & (RF1_NEVER_BLOW)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) l_ptr->r_flags1 |= (RF1_NEVER_BLOW);

			/* Do not move */
			do_move = FALSE;
		}


		/* The monster is hidden in terrain, trying to attack the player.*/
		if (do_move && (m_ptr->mflag & (MFLAG_HIDE)) && (cave_m_idx[ny][nx] < 0))
		{
			/* We can't get out of hiding */
			if ((f_info[cave_feat[oy][ox]].flags2 & (FF2_HIDE_DEEP)) &&
				(place_monster_here(ny,nx,m_ptr->r_idx) == MM_WALK))
			{
				do_move = FALSE;
			}
                        else if (f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED))
			{
				if ((r_ptr->flags2 & (RF2_BASH_DOOR)) &&  (f_info[cave_feat[ny][nx]].flags1 & (FF1_BASH)))
				{
					/* Don't move*/
					do_move = FALSE;

					/* Do use energy */
					do_turn = TRUE;

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
				else
				{

					do_move = FALSE;
				}

			}

		}


		/* The player is in the way.  Attack him. */
		if (do_move && (cave_m_idx[ny][nx] < 0))
		{

			/* If was hidden, reveal - ANDY */
			if (m_ptr->mflag & (MFLAG_HIDE))
			{
				/* Unhide the monster */
				m_ptr->mflag &= ~(MFLAG_HIDE);

				/* Check if it is visible */
				update_mon(m_idx,FALSE);

                                /* Hack --- tell the player if something unhides */
                                if (m_ptr->ml)
                                {
                                        char m_name[80];

                                        /* Get the monster name */
                                        monster_desc(m_name, m_ptr, 0);

                                        msg_format("%^s emerges from %s%s.",m_name,
                                                ((f_info[cave_feat[ny][nx]].flags2 & (FF2_FILLED))?"":"the "),
                                                f_name+f_info[cave_feat[ny][nx]].name);
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


			/* Do the attack */
			(void)make_attack_normal(m_idx);

			/* Do not move */
			do_move = FALSE;

			/* Took a turn */
			do_turn = TRUE;
		}


		/* Some monsters never move */
		if (do_move && (r_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			/* Hack -- memorize lack of attacks */
			if (m_ptr->ml) l_ptr->r_flags1 |= (RF1_NEVER_MOVE);

			/* Do not move */
			do_move = FALSE;

		}

		/* A monster is in the way */
		if (do_move && (cave_m_idx[ny][nx] > 0))
		{

			monster_type *n_ptr = &m_list[cave_m_idx[ny][nx]];

			/* Assume no movement */
			do_move = FALSE;

			/* Push past hidden monsters */
			if ((n_ptr->mflag & (MFLAG_HIDE)) && 
			    (place_monster_here(m_ptr->fy,m_ptr->fx,n_ptr->r_idx)))
			{

				do_move = TRUE;
			}

			/* Push past flying monsters */
			else if ((n_ptr->mflag & (MFLAG_OVER)) &&
			    (place_monster_here(m_ptr->fy,m_ptr->fx,n_ptr->r_idx) == MM_FLY))
			{

				do_move = TRUE;
			}

			/* Kill weaker monsters (unless fleeing)*/
			else if ((r_ptr->flags2 & (RF2_KILL_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0) &&
			    !(m_ptr->monfear))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster ate another monster */
				did_kill_body = TRUE;

				/* Message XXX XXX XXX */

				/* Kill the monster */
				delete_monster(ny, nx);

				/* Don't reveal hidden monsters to scare the player even more - ANDY*/

			}

			/* Push past weaker monsters (unless leaving a grid that other monster cannot occupy) */
			else if ((r_ptr->flags2 & (RF2_MOVE_BODY)) &&
			    (compare_monsters(m_ptr, n_ptr) > 0) &&
			    (place_monster_here(m_ptr->fy,m_ptr->fx,n_ptr->r_idx)))
			{
				/* Allow movement */
				do_move = TRUE;

				/* Monster pushed past another monster */
				did_move_body = TRUE;

			}

			/* Attack if confused and not fleeing */
			/* XXX XXX Should use seperate routine */
			else if (m_ptr->confused)
			{
				int ap_cnt;

				do_turn = TRUE;

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

			/* Push past fleeing monsters or if fleeing (not both) (unless leaving a grid other monster cannot occupy) */
			else if (((m_ptr->monfear) || (n_ptr->monfear)) &&
			    !((m_ptr->monfear) && (n_ptr->monfear)) &&
			    (place_monster_here(m_ptr->fy,m_ptr->fx,n_ptr->r_idx)))
			{
				/* Allow movement */
				do_move = TRUE;
			}

		}

                /* Hack --- smart monsters sometimes disarm traps */
                if ((f_info[cave_feat[ny][nx]].flags1 & (FF1_TRAP)) &&
                        (r_ptr->flags2 & (RF2_SMART)) && 
                        !(do_move) && (rand_int(100)<30))
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
                                do_turn = TRUE;

                                /* Did smart stuff */
                                did_smart = TRUE;
                        }
                }


		/* Creature has been allowed move */
		if (do_move)
		{
			s16b this_o_idx, next_o_idx = 0;

                        /* Hidden ? */
                        bool hidden = ((m_ptr->mflag & (MFLAG_HIDE))!=0);

			/* Take a turn */
			do_turn = TRUE;

			/* Update flags */
                        if ((mmove == MM_FLY) || (mmove == MM_CLIMB))
			{
				m_ptr->mflag |= (MFLAG_OVER);
			}
                        else 
			{
				m_ptr->mflag &= ~(MFLAG_OVER);
			}

			/* Set hide flag if passing through floor/ceiling (phasing) */
                        if (mmove == MM_PASS)
			{
				m_ptr->mflag |=(MFLAG_HIDE);
			}

			/* Set hide flag if digging and HIDE_DIG */
                        else if ((f_info[cave_feat[ny][nx]].flags2 & (FF2_HIDE_DIG)) && (mmove == MM_DIG))
			{
				m_ptr->mflag |=(MFLAG_HIDE);
			}
			/* Set hide flag if swimming and HIDE_SWIM */
			else if ((f_info[cave_feat[ny][nx]].flags2 & (FF2_HIDE_SWIM)) && (mmove == MM_SWIM))
			{
				m_ptr->mflag |=(MFLAG_HIDE);
			}

			/* Set hide flag if HIDE_DEEP and resistant, with conditions */
			else if ((f_info[cave_feat[ny][nx]].flags2 & (FF2_HIDE_DEEP))
				&& (mon_resist_feat(cave_feat[ny][nx],m_ptr->r_idx)))
			{
				if (f_info[cave_feat[ny][nx]].flags2 & (FF2_COVERED))
				{
					if (f_info[cave_feat[oy][ox]].flags2 & (FF2_COVERED))
					{
						/* No change */
					}
					/* Walking under cover if can't pop-up to it */
					else if ((m_ptr->mflag & (MFLAG_HIDE)) && (place_monster_here(oy,ox,m_ptr->r_idx) == MM_WALK))
					{
						m_ptr->mflag |=(MFLAG_HIDE);
					}
					else
					{
						m_ptr->mflag &= ~(MFLAG_HIDE);
					}

				}
				/* Covered/bridged features are special */
				else if (f_info[cave_feat[ny][nx]].flags2 & (FF2_BRIDGED))
				{

					/* Walking under bridges if can't pop-up to it */
					if ((m_ptr->mflag & (MFLAG_HIDE)) && (place_monster_here(oy,ox,m_ptr->r_idx) == MM_WALK))
					{
						m_ptr->mflag |=(MFLAG_HIDE);
					}
					else
					{
						m_ptr->mflag &= ~(MFLAG_HIDE);
					}
				}
				else
				{
						m_ptr->mflag |=(MFLAG_HIDE);
				}

			}
			else
			{
				m_ptr->mflag &= ~(MFLAG_HIDE);
			}

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

                        /* Hack --- tell the player if something unhides */
                        if ((hidden) && !(m_ptr->mflag & (MFLAG_HIDE)) && (m_ptr->ml))
                        {
                                        char m_name[80];

                                        /* Get the monster name */
                                        monster_desc(m_name, m_ptr, 0);

                                        msg_format("%^s emerges from %s%s.",m_name,
                                        ((f_info[cave_feat[ny][nx]].flags2 & (FF2_FILLED))?"":"the "),
                                        f_name+f_info[cave_feat[ny][nx]].name);
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
				cave_alter_feat(ny, nx, FS_KILL_MOVE);
			}

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[ny][nx]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;
				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Skip gold */
				if (o_ptr->tval == TV_GOLD) continue;

				/* Take or Kill objects on the floor */
				if ((r_ptr->flags2 & (RF2_TAKE_ITEM)) ||
				    (r_ptr->flags2 & (RF2_KILL_ITEM)))
				{
					u32b f1, f2, f3;

					u32b flg3 = 0L;

					char m_name[80];
					char o_name[80];

					/* Extract some flags */
					object_flags(o_ptr, &f1, &f2, &f3);

					/* Get the object name */
					object_desc(o_name, o_ptr, TRUE, 3);

					/* Get the monster name */
					monster_desc(m_name, m_ptr, 0x04);

					/* React to objects that hurt the monster */
					if (f1 & (TR1_KILL_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_DRAGON)) flg3 |= (RF3_DRAGON);
					if (f1 & (TR1_SLAY_TROLL)) flg3 |= (RF3_TROLL);
					if (f1 & (TR1_SLAY_GIANT)) flg3 |= (RF3_GIANT);
					if (f1 & (TR1_SLAY_ORC)) flg3 |= (RF3_ORC);
					if (f1 & (TR1_SLAY_DEMON)) flg3 |= (RF3_DEMON);
					if (f1 & (TR1_SLAY_UNDEAD)) flg3 |= (RF3_UNDEAD);
					if (f1 & (TR1_SLAY_ANIMAL)) flg3 |= (RF3_ANIMAL);
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

								/* XXX Flag object with correct flag? */
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
						(void)monster_carry(m_idx, i_ptr);
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

		/* Stop when done */
		if (do_turn) break;
	}


	/* If we haven't done anything, try casting a spell again */
	if (smart_monsters && !do_turn && !do_move)
	{
		/* Cast spell */
		if (make_attack_spell(m_idx)) return;
	}

	/* Learn things from observable monster */
	if (m_ptr->ml)
	{
		/* Monster opened a door */
		if (did_open_door) l_ptr->r_flags2 |= (RF2_OPEN_DOOR);

		/* Monster bashed a door */
		if (did_bash_door) l_ptr->r_flags2 |= (RF2_BASH_DOOR);

		/* Monster tried to pick something up */
		if (did_take_item) l_ptr->r_flags2 |= (RF2_TAKE_ITEM);

		/* Monster tried to crush something */
		if (did_kill_item) l_ptr->r_flags2 |= (RF2_KILL_ITEM);

		/* Monster pushed past another monster */
		if (did_move_body) l_ptr->r_flags2 |= (RF2_MOVE_BODY);

		/* Monster ate another monster */
		if (did_kill_body) l_ptr->r_flags2 |= (RF2_KILL_BODY);

		/* Monster passed through a wall */
		if (did_pass_wall) l_ptr->r_flags2 |= (RF2_PASS_WALL);

		/* Monster destroyed a wall */
		if (did_kill_wall) l_ptr->r_flags2 |= (RF2_KILL_WALL);

                /* Monster disarmed a trap */
                if (did_smart) l_ptr->r_flags2 |= (RF2_SMART);

		/* Monster is climbing */
		if (mmove == MM_CLIMB) l_ptr->r_flags2 |= (RF2_CAN_CLIMB);

		/* Monster is flying */
		if (mmove == MM_FLY) l_ptr->r_flags2 |= (RF2_CAN_FLY);

                /* Monster must swim */
                if ((mmove == MM_FLY) && (r_ptr->flags2 & (RF2_MUST_FLY))) l_ptr->r_flags2 |= (RF2_MUST_FLY);

		/* Monster is swimming */
		if (mmove == MM_SWIM) l_ptr->r_flags2 |= (RF2_CAN_SWIM);

                /* Monster must swim */
                if ((mmove == MM_SWIM) && (r_ptr->flags2 & (RF2_MUST_SWIM))) l_ptr->r_flags2 |= (RF2_MUST_SWIM);

		/* Monster is digging */
		if (mmove == MM_DIG) l_ptr->r_flags2 |= (RF2_CAN_DIG);

		/* Monster is passing */
		if (mmove == MM_PASS) l_ptr->r_flags2 |= (RF2_PASS_WALL);


	}


	/* Hack -- get "bold" if out of options */
	if (!do_turn && !do_move && m_ptr->monfear)
	{
		/* No longer afraid */
		m_ptr->monfear = 0;

		/* Message if seen */
		if (m_ptr->ml)
		{
			char m_name[80];

			/* Get the monster name */
			monster_desc(m_name, m_ptr, 0);

			/* Dump a message */
			msg_format("%^s turns to fight!", m_name);
		}

		/* XXX XXX XXX Actually do something now (?) */
	}
}


/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "lite_spot()",
 * especially when the player is running.
 *
 * Note the special "MFLAG_BORN" flag, which prevents monsters from doing
 * anything during the game turn in which they are created.  This flag is
 * optimized via the "repair_mflag_born" flag.
 *
 * Note the special "MFLAG_NICE" flag, which prevents "nasty" monsters from
 * using any of their spell attacks until the player gets a turn.  This flag
 * is optimized via the "repair_mflag_nice" flag.
 */
void process_monsters(byte minimum_energy)
{
	int i;
	int fy, fx;

	monster_type *m_ptr;
	monster_race *r_ptr;


	/* Repair "born" flags */
	if (repair_mflag_born)
	{
		/* Clear flag */
		repair_mflag_born = FALSE;

		/* Process the monsters */
		for (i = 1; i < m_max; i++)
		{
			/* Get the monster */
			m_ptr = &m_list[i];

			/* Ignore "dead" monsters */
			/* if (!m_ptr->r_idx) continue; */

			/* Clear "born" flag */
			m_ptr->mflag &= ~(MFLAG_BORN);
		}
	}


	/* Process the monsters (backwards) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Get the monster */
		m_ptr = &m_list[i];


		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;


		/* Ignore "born" monsters XXX XXX */
		if (m_ptr->mflag & (MFLAG_BORN)) continue;


		/* Not enough energy to move */
		if (m_ptr->energy < minimum_energy) continue;

		/* Use up "some" energy */
		m_ptr->energy -= 100;


		/* Heal monster? XXX XXX XXX */


		/* Get the race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Monsters can "sense" the player */
		if (m_ptr->cdis <= r_ptr->aaf)
		{
			/* Process the monster */
			process_monster(i);

			/* Continue */
			continue;
		}


		/* Get the location */
		fx = m_ptr->fx;
		fy = m_ptr->fy;

		/* Monsters can "see" the player (backwards) XXX XXX */
		if (player_has_los_bold(fy, fx))
		{
			/* Process the monster */
			process_monster(i);

			/* Continue */
			continue;
		}

#ifdef MONSTER_FLOW

		/* Hack -- Monsters can "smell" the player from far away */
		if (flow_by_sound)
		{
			int py = p_ptr->py;
			int px = p_ptr->px;

			/* Check the flow (normal aaf is about 20) */
			if ((cave_when[fy][fx] == cave_when[py][px]) &&
			    (cave_cost[fy][fx] < MONSTER_FLOW_DEPTH) &&
			    (cave_cost[fy][fx] < r_ptr->aaf))
			{
				/* Process the monster */
				process_monster(i);

				/* Continue */
				continue;
			}
		}

#endif /* MONSTER_FLOW */

	}
}
