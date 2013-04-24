/* File: monattk.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"





#define MAX_DESC_INSULT   8
#define MAX_DESC_MOAN 4
#define MAX_DESC_SPEAK 3


/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[MAX_DESC_INSULT] =
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
static cptr desc_moan[MAX_DESC_MOAN] =
{
	"seems sad about something",
	"asks you \"Do you know how I got here?\"",
	"becomes wild-eyed and lunges towards you",
	"shrieks \"Get away from me! I can't control myself!\""
};

/*
 * Hack -- possible "insult" messages
 */
static cptr desc_speak[MAX_DESC_SPEAK] =
{
	"Oh dear! Oh dear! I shall be late!",
	"Oh my ears and whiskers, how late it's getting!",
	"I'm late, I'm late, I'm really, really late!"	
};

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int m_idx, int typ, int dam)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY;

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Cast a beam at the player, sometimes with limited range.
 * Do not stop if we hit a monster
 * Affect grids, monsters, and the player
 */
static void beam(int m_idx, int typ, int dam, int range)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
	           PROJECT_PLAY;

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with a beam attack */
	(void)project(m_idx, range, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Cast a ball spell at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void ball(int m_idx, int typ, int dam, int rad, bool jump)
{
	monster_type *m_ptr = &m_list[m_idx];
	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL |
	           PROJECT_PLAY | PROJECT_WALL;

	/* Optionally, allow the attack to "jump" to the player */
	if (jump) flg |= (PROJECT_JUMP);

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, fy, fx, py, px, dam, typ, flg, 0, 0);
}

/*
 * Release a cloud, which is a ball centered on the monster that does not
 * affect other monsters (mostly to avoid annoying messages).
 *
 * Consider being less graphics-intensive.
 */
void cloud(int m_idx, int typ, int dam, int rad)
{
	monster_type *m_ptr = &m_list[m_idx];
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY;

	/* Hack -- unlight surrounding area */
	if (typ == GF_WEAK_RAD) flg |= (PROJECT_HIDE);
	else if (typ == GF_STRONG_RAD) flg |= (PROJECT_HIDE);
	else if (typ == GF_DARK) flg |= (PROJECT_HIDE);
	else if (typ == GF_TENEBROUS) flg |= (PROJECT_HIDE);
	
	/* if ((typ == GF_DARK) || (GF_TENEBROUS)) flg |= (PROJECT_HIDE); */

	/* Surround the monster with a cloud */
	(void)project(m_idx, rad, fy, fx, fy, fx, dam, typ, flg, 0, 0);
}

/*
 * Breathe or cast an arc-shaped spell at the player.
 * Use an arc spell of specified range and width.
 * Optionally, do not harm monsters with the same r_idx.
 * Affect grids, objects, monsters, and (specifically) the player.
 *
 * Monster breaths do not lose strength with distance at the same rate
 * that normal arc spells do.  If the monster is "powerful", they lose
 * less strength; otherwise, they lose more.
 */
static void arc(int m_idx, int typ, int dam, int rad, int degrees_of_arc)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int py = p_ptr->py;
	int px = p_ptr->px;
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	u32b flg = PROJECT_ARC | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM |
	           PROJECT_KILL | PROJECT_PLAY;


	/* Diameter of source of energy is at least 20. */
	int diameter_of_source = 20;

	/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
	int degree_factor = (r_ptr->flags2 & (RF2_POWERFUL)) ? 120 : 60;

	/* Narrow arcs lose relatively little energy over distance. */
	if (degrees_of_arc < degree_factor)
	{
		if (degrees_of_arc <= 6) diameter_of_source = rad * 10;
		else diameter_of_source = diameter_of_source * degree_factor /
			degrees_of_arc;
	}


	/* Can optionally ignore monsters with the same r_idx. */
	/* if (noharm) project_immune = m_ptr->r_idx; */

	/* Radius of zero means no fixed limit. */
	if (rad == 0) rad = MAX_SIGHT;

	/* Maddened monsters may adjust their target */
	/* if (m_ptr->mflag & (MFLAG_MADD)) mad_mon_retarget(fy, fx, &py, &px); */

	/* Target the player with an arc-shaped attack. */
	(void)project(m_idx, rad, fy, fx, py, px, dam, typ, flg, degrees_of_arc,
		(byte)diameter_of_source);
}

/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level, int m_idx)
{
	int i, k, ac;

	monster_type *m_ptr = &m_list[m_idx];

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	i = (power + (m_ptr->stunned ? level * 4 : level * 6));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}

static int check_hit_monst_v_monst(int power, int level, int ac)
{
	int i, k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ac)) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Adjust damage for character armour.
 *
 * Use proper rounding.  -LM-
 */
static void ac_dam(int *dam, int ac)
{
	/* The effect of a point of damage decreases as the total rises */
	int d = 120 + ABS(ac);

	/* Adjust damage (whole) */
	*dam -= ((*dam) * ac) / d;

	/* Adjust damage (fractional) */
	if ((((*dam) * ABS(ac)) % d) > rand_int(d))
	{
		*dam -= ((ac > 0) ? 1 : -1);
	}
}

/*
 * Using an input value for average damage, and another that controls
 * variability, return the actual base damage of a monster's attack
 * spell.  The larger the value for "control", the less likely the damage
 * will vary greatly.
 */
s16b get_dam(int av_dam, int control)
{
	int dam = 0;
	int spread;

	/* Damage may never differ by more than 50% from the average */
	if (control < 4) control = 4;

	/*
	 * Get the allowable spread (two standard deviations, or 100,
	 * whichever is less).
	 */
	spread = MIN(100, av_dam * 2 / control);

	/* Loop until damage is within the allowable spread */
	while (TRUE)
	{
		/* Randomize damage (average, standard deviation) */
		dam = Rand_normal(av_dam, div_round(av_dam, control));

		/* Forbid too great a variation */
		if (dam > av_dam + spread) continue;
		if (dam < av_dam - spread) continue;

		/* Accept */
		break;
	}

	/* Return randomized damage */
	return (dam);
}

/*
 * Critical blows by monsters can inflict cuts and stuns.
 */
static int monster_critical(int dice, int sides, int dam, int effect)
{
	int max = 0;
	int bonus;
	int total = dice * sides;


	/* Special case -- wounding/battering attack */
	if (effect == RBE_SHATTER)
	{
		/* Must do at least 70% of perfect */
		if (dam < total * 7 / 10) return (0);

		max = 1;
	}

	/* Standard attack */
	else 
	{  
		/* Weak blows rarely work */
		if ((rand_int(20) >= dam) || (!one_in_(3))) return (0);

		/* Must do at least 90% of perfect */
		if (dam < total * 9 / 10) return (0);
	}

	/* Perfect damage */
	if (dam == total) max++;

	/* Get bonus to critical damage (never greater than 6) */
	bonus = MIN(6, div_round(dam, 8));

	/* Critical damage  (never greater than 6 + max) */
	return (randint(bonus) + max);
}

/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will return FALSE if a monster
 * is in the way.
 *
 * Then we should perhaps instead supply a flag to "projectable()".  XXX XXX
 */
bool clean_shot(int y1, int x1, int y2, int x2, bool friend)
{
	int y, x, i;
	
	int grid_n;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, &y2, &x2, PROJECT_STOP);

	/* Source and target the same */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in a wall grid */
	if (!cave_floor_bold(y, x)) return (FALSE);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);
	
	for (i = 0; i < grid_n; i++)
	{
		y = GRID_Y(grid_g[i]);
		x = GRID_X(grid_g[i]);

		if ((cave_m_idx[y][x] > 0) && !((y == y2) && (x == x2)))
		{
			monster_type *m_ptr = &m_list[cave_m_idx[y][x]];
			if (friend == is_pet(m_ptr))
			{
				return (FALSE);
			}
		}
		/* Pets may not shoot through the character - TNB */
		if ((y == p_ptr->py) && (x == p_ptr->px))
		{
			if (friend) return (FALSE);
		}
	}


	/* Assume okay */
	return (TRUE);
}


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(monster_type *m_ptr)
{
	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	object_type *j_ptr = &inventory[INVEN_WIELD];
	object_type *o_ptr;

	int ap_cnt;

	int i, k, j, tmp, ac, rlev;
	int do_cut, do_stun;
	int save;

	bool alive = TRUE;
	bool fear = FALSE;
	/* Attack must be touching and player must be hit */
	bool touched = FALSE;
	bool struck = FALSE;

	s32b gold;

	char o_name[120];

	char m_name[80];

	char ddesc[80];

	bool blinked;
	bool martial;
	
	bool notice = FALSE;
	
	save = p_ptr->skill_sav;

	martial = FALSE;
	
	/* Find out if we're using martial arts */
	if (!j_ptr->k_idx) martial = TRUE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Total armour */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level (at least 1) */
	rlev = MAX(r_ptr->level, 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a chicken") */
	monster_desc(ddesc, m_ptr, 0x88);
	
	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		bool no_effect = FALSE;
		bool no_punc = FALSE;

		int power = 0;
		int dam = 0;

		cptr act = NULL;

		/* Extract the attack information */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;


		/* Hack -- no more attacks */
		if (!method) break;

		/* Stop if monster is dead or gone */
		if (!alive) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;


		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Assume no cut, stun */
		do_cut = do_stun = 0;

		/* Extract the attack "power".  Elemental attacks upgraded. */
		switch (effect)
		{
			/* Lowered power of hurt b/c of new regen rates */
			/* Greater emphasis on monster level (*4 if not stunned) */
			case RBE_HURT:      power = 60; break;
			case RBE_POISON:	power =  5; break;
			case RBE_UN_BONUS:	power = 20; break;
			case RBE_UN_POWER:	power = 15; break;
			case RBE_EAT_GOLD:	power =  5; break;
			case RBE_EAT_ITEM:	power =  5; break;
			case RBE_EAT_FOOD:	power =  5; break;
			case RBE_EAT_LITE:	power =  5; break;
			case RBE_FIRE:		power = 10; break;
			case RBE_ACID:		power =  0; break;
			case RBE_ELEC:		power = 10; break;
			case RBE_COLD:		power = 10; break;
			case RBE_STEAM:		power = 10; break;
			/* Nether and ether are non-functioning */
			case RBE_NETHER:	power = 10; break;
			case RBE_ETHER:		power = 10; break;
			case RBE_BLIND:		power =  2; break;
			case RBE_CONFUSE:	power = 10; break;
			case RBE_TERRIFY:	power = 10; break;
			case RBE_PARALYZE:	power =  2; break;
			case RBE_LOSE_MUS:	power =  0; break;
			case RBE_LOSE_AGI:	power =  0; break;
			case RBE_LOSE_VIG:	power =  0; break;
			case RBE_LOSE_SCH:	power =  0; break;
			case RBE_LOSE_EGO:	power =  0; break;
			case RBE_LOSE_CHR:	power =  0; break;
			case RBE_LOSE_ALL:	power =  2; break;
			case RBE_SHATTER:	power = 60; break;
			case RBE_EXP_10:	power =  5; break;
			case RBE_EXP_20:	power =  5; break;
			case RBE_EXP_40:	power =  5; break;
			case RBE_EXP_80:	power =  5; break;
		}

		/* Roll out the damage */
		dam = damroll(d_dice, d_side);

			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
					act = "hits you";
					do_cut = do_stun = 1;
					touched = TRUE;
					break;
				}

				case RBM_TOUCH:
				{
					act = "touches you";
					touched = TRUE;
					break;
				}

				case RBM_PUNCH:
				{
					act = "punches you";
					do_stun = 1;
					touched = TRUE;
					break;
				}

				case RBM_KICK:
				{
					act = "kicks you";
					do_stun = 1;
					touched = TRUE;
					break;
				}

				case RBM_SLASH:
				{
					act = "slashes you";
					do_cut = 1;
					touched = TRUE;
					break;
				}
	
				case RBM_PIERCE:
				{
					act = "pierces you";
					do_cut = 1;
					touched = TRUE;
					break;
				}
				
				case RBM_BLUNT:
				{
					act = "bashes you";
					do_stun = 1;
					touched = TRUE;
					break;
				}

				case RBM_LURE:
				{
					act = "lures you";
					break;
				}
				case RBM_XXX2:
				{
					act = "XXX2s you";
					break;
				}
				case RBM_HOWL:
				{
					act = "howls at you";
					break;
				}

				case RBM_CLAW:
				{
					act = "claws you";
					do_cut = 1;
					touched = TRUE;
					break;
				}

				case RBM_KISS:
				{
					act = "kisses you";
					touched = TRUE;
					break;
				}

				case RBM_GRAB:
				{
					act = "grabs you";
					touched = TRUE;
					break;
				}

				case RBM_BITE:
				{
					act = "bites you";
					do_cut = 1;
					touched = TRUE;
					break;
				}

				case RBM_STING:
				{
					act = "stings you";
					touched = TRUE;
					break;
				}

				case RBM_BUTT:
				{
					act = "butts you";
					do_stun = 1;
					touched = TRUE;
					break;
				}

				case RBM_CRUSH:
				{
					act = "crushes you";
					do_stun = 1;
					touched = TRUE;
					break;
				}
				
				case RBM_ENGULF:
				{
					act = "engulfs you";
					touched = TRUE;
					break;
				}

				case RBM_CRAWL:
				{
					act = "crawls on you";
					touched = TRUE;
					break;
				}

				case RBM_DROOL:
				{
					act = "drools on you";
					touched = TRUE;
					break;
				}

				case RBM_SPIT:
				{
					act = "spits on you";
					touched = TRUE;
					break;
				}

				case RBM_XXX4:
				{
					act = "XXX4's on you";
					touched = TRUE;
					break;
				}

				case RBM_XXX5:
				{
					act = "XXX5's on you";
					touched = TRUE;
					break;
				}

				case RBM_GAZE:
				{
					act = "gazes at you";
					break;
				}

				case RBM_WAIL:
				{
					act = "wails at you";
					break;
				}

				case RBM_SPORE:
				{
					act = "releases spores at you";
					break;
				}

				case RBM_XXX6:
				{
					act = "XXX6's on you";
					touched = TRUE;
					break;
				}

				case RBM_XXX7:
				{
					act = "XXX7's on you";
					touched = TRUE;
					break;
				}

				case RBM_ZAP:
				{
					act = "zaps you";
					touched = TRUE;
					break;
				}
				

				case RBM_PECK:
				{
					act = "pecks you";
					touched = TRUE;
					break;
				}

				/* Need a better (more general way) to handle speech */
				case RBM_SPEAK:
				{	
					act = desc_moan[rand_int(MAX_DESC_MOAN)];
					no_punc = TRUE;
					break;
				}

				case RBM_BEG:
				{
					act = "begs you for money";
					break;
				}

				case RBM_SEDUCE:
				{
					act = "caresses you";
					touched = TRUE;
					break;
				}

				case RBM_XXX10:
				{
					act = "XXX10's on you";
					touched = TRUE;
					break;
				}

			}

		/* No effect */
		if (no_effect) continue;


		/* Monster hits player */
		/* need to move all these hacks into a seperate 'handle */
		/* special protection function. */
		if (!effect || check_hit(power, rlev, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);

			struck = TRUE;
			
			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil) && (r_ptr->flags3 & (RF3_EVIL)) &&
				/* Keep an eye on this. . . */
			    ((rand_int(100)) > r_ptr->level))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->r_flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Message */
			if (act)
			{
				if (no_punc)
					msg_format("%^s %s", m_name, act);
				else if (dam > p_ptr->chp / 3)
					msg_format("%^s %s!", m_name, act);
				else
					msg_format("%^s %s.", m_name, act);
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Apply appropriate damage */
			switch (effect)
			{
				/* No effect */
				case 0:
				{
					/* Hack -- Assume obvious */
					obvious = TRUE;

					/* Hack -- No damage */
					dam = 0;

					break;
				}

				case RBE_HURT:
				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					break;
				}

				case RBE_POISON:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage (special) - Perhaps vary the GF_FIRE */
					/* based off monster something? */
					notice = poison_dam(dam, GF_POISON, ddesc);

					/* Message, only in case there is actual damage */
					if (notice)
					msg_print("You are hit with poison!");

					break;
				}

				case RBE_UN_BONUS:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Apply disenchantment */
					/* Currently Unresistable!!! */
					if (apply_disenchant(0)) obvious = TRUE;

					break;
				}

				case RBE_UN_POWER:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staffs */
						if (((o_ptr->tval == TV_TOOL) ||
						     (o_ptr->tval == TV_RAY)) &&
						    (o_ptr->pval > 0))
						{
							/* Message */
							msg_print("Energy drains from your pack!");

							/* Obvious */
							obvious = TRUE;

							/* Heal */
							j = rlev;
							m_ptr->hp += j * o_ptr->pval * o_ptr->number;
							if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

							/* Uncharge */
							o_ptr->pval = 0;

							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE | PN_REORDER);

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

							/* Done */
							break;
						}
					}

					break;
				}

				case RBE_EAT_GOLD:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex and level */
					/* an agility of 560 makes you immune */
					if (!p_ptr->paralyzed &&
					    (rand_int(140) < (p_ptr->stat_use[A_AGI] / 4)))
					{
						/* Saving throw message */
						msg_print("You quickly protect your money pouch!");

						/* Occasional blink anyway */
						if (rand_int(4)) blinked = TRUE;
					}

					/* Eat gold */
					else
					{
						gold = (p_ptr->au / 10) + randint(25);
						if (gold < 2) gold = 2;
						if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
						if (gold > p_ptr->au) gold = p_ptr->au;
						p_ptr->au -= gold;
						if (gold <= 0)
						{
							msg_print("Nothing was stolen.");
						}
						else if (p_ptr->au)
						{
							msg_print("Your purse feels lighter.");
							msg_format("%ld coins were stolen!", (long)gold);
						}
						else
						{
							msg_print("Your purse feels lighter.");
							msg_print("All of your coins were stolen!");
						}

						/* Redraw gold */
						p_ptr->redraw |= (PR_GOLD);

						/* Window stuff */
						p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

						/* Blink away */
						blinked = TRUE;
					}

					break;
				}

				case RBE_EAT_ITEM:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Saving throw (unless paralyzed) based on dex and level */
					/* an agility of 560 makes you immune */
					if (!p_ptr->paralyzed &&
					    (rand_int(140) < (p_ptr->stat_use[A_AGI] / 4)))
					{
						/* Saving throw message */
						msg_print("You grab hold of your backpack!");

						/* Occasional "blink" anyway */
						blinked = TRUE;

						/* Obvious */
						obvious = TRUE;

						/* Done */
						break;
					}

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Pick an item */
						i = rand_int(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip artifacts */
						if (artifact_p(o_ptr)) continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 3);

						/* Message */
						msg_format("%sour %s (%c) was stolen!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain local object */
						object_copy(i_ptr, o_ptr);

						/* Modify number */
						i_ptr->number = 1;

						/* Carry the object */
						(void)monster_carry(m_idx, i_ptr);

						/* Steal the items */
						inven_item_increase(i, -1);
						inven_item_optimize(i);

						/* Obvious */
						obvious = TRUE;

						/* Blink away */
						blinked = TRUE;

						/* Done */
						break;
					}

					break;
				}

				case RBE_EAT_FOOD:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Steal some food */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item from the pack */
						i = rand_int(INVEN_PACK);

						/* Get the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip non-food objects */
						if (o_ptr->tval != TV_FOOD) continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
						msg_format("%sour %s (%c) was eaten!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));

						/* Steal the items */
						inven_item_increase(i, -1);
						inven_item_optimize(i);

						/* Obvious */
						obvious = TRUE;

						/* Done */
						break;
					}

					break;
				}

				case RBE_EAT_LITE:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Get the lite */
					o_ptr = &inventory[INVEN_LITE];

					/* Drain fuel */
					if ((o_ptr->pval > 0) && (!artifact_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->pval -= (250 + randint(250));
						if (o_ptr->pval < 1) o_ptr->pval = 1;

						/* Notice */
						if (!p_ptr->blind)
						{
							msg_print("Your light dims.");
							obvious = TRUE;
						}

						/* Window stuff */
						p_ptr->window |= (PW_EQUIP);
					}

					break;
				}

				case RBE_FIRE:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage (special) - Perhaps vary the GF_FIRE */
					/* based off monster something? */
					notice = fire_dam(dam, GF_FIRE, ddesc);

					/* Message */
					if (notice)
					msg_print("You are enveloped in flames!");

					break;
				}

				case RBE_ACID:
				{
					/* Obvious */
					obvious = TRUE;

					/* Special damage */
					notice = acid_dam(dam, GF_ACID, ddesc);

					/* Message */
					if (notice)
					msg_print("You are covered in acid!");

					break;
				}

				case RBE_ELEC:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage (special) */
					notice = elec_dam(dam, GF_ELEC, ddesc);

					/* Message */
					if (notice)
					msg_print("You are struck by electricity!");

					break;
				}

				case RBE_COLD:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage (special) */
					ice_dam(dam, GF_ICE, ddesc);

					/* Message */
					if (notice)
					msg_print("You are covered with frost!");

					break;
				}

				case RBE_STEAM:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage (special) */
					notice = water_dam(dam, GF_STEAM, ddesc);

					/* Message */
					if (notice)
					msg_print("You are blasted with steam!");

					break;
				}

				case RBE_NETHER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are assailed by dark energies!");

					/* Take damage (special) - nothing special */
					take_hit(dam, ddesc, TRUE);
					break;
				}

				case RBE_ETHER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Message */
					msg_print("You are pounded by etheric forces!");

					/* Take damage (special) - Note this doesn't do */
					/* anything special */
					take_hit(dam, ddesc, TRUE);
					break;
				}


				case RBE_BLIND:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Increase "blind" */
					if (!p_ptr->resist_blind)
					{
						if (set_blind(p_ptr->blind + 10 + randint(rlev * 2)))
						{
							obvious = TRUE;
						}
					}
					break;
				}

				case RBE_CONFUSE:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Increase "confused" */
					if (!p_ptr->resist_confu)
					{
						if (set_confused(p_ptr->confused + 3 + randint(rlev * 2)))
						{
							obvious = TRUE;
						}
					}
					break;
				}

				case RBE_TERRIFY:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Increase "afraid" */
					if (p_ptr->resist_fear)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else if (rand_int(100) < save)
					{
						msg_print("You stand your ground!");
						obvious = TRUE;
					}
					else
					{
						if (set_afraid(p_ptr->afraid + 3 + randint(rlev * 2)))
						{
							obvious = TRUE;
						}
					}
					break;
				}

				case RBE_PARALYZE:
				{
					/* Hack -- Prevent perma-paralysis via damage */
					if (p_ptr->paralyzed && (dam < 1)) dam = 1;

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Increase "paralyzed" */
					if (p_ptr->free_act)
					{
						msg_print("You are unaffected!");
						obvious = TRUE;
					}
					else if (rand_int(100) < save)
					{
						msg_print("You resist the effects!");
						obvious = TRUE;
					}
					else
					{
						if (set_paralyzed(p_ptr->paralyzed + 3 + randint(rlev * 2)))
						{
							obvious = TRUE;
						}
					}
					break;
				}

				case RBE_LOSE_MUS:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_MUS)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_SCH:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_SCH)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_EGO:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_EGO)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_AGI:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_AGI)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_VIG:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_VIG)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stat) */
					if (do_dec_stat(A_CHR)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					/* Damage (stats) */
					if (do_dec_stat(A_MUS)) obvious = TRUE;
					if (do_dec_stat(A_AGI)) obvious = TRUE;
					if (do_dec_stat(A_VIG)) obvious = TRUE;
					if (do_dec_stat(A_SCH)) obvious = TRUE;
					if (do_dec_stat(A_EGO)) obvious = TRUE;
					if (do_dec_stat(A_CHR)) obvious = TRUE;

					break;
				}

				case RBE_SHATTER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Radius 6 earthquake centered on the monster */
					if (dam > rand_int(60))
						earthquake(m_ptr->fy, m_ptr->fx, 6);

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					break;
				}

				case RBE_EXP_10:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					if (p_ptr->hold_life && (rand_int(100) < 95))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(10, 6) + (p_ptr->exp/100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d/10);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_20:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					if (p_ptr->hold_life && (rand_int(100) < 90))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_40:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					if (p_ptr->hold_life && (rand_int(100) < 75))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				case RBE_EXP_80:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					if (p_ptr->hold_life && (rand_int(100) < 50))
					{
						msg_print("You keep hold of your life force!");
					}
					else
					{
						s32b d = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;

						if (p_ptr->hold_life)
						{
							msg_print("You feel your life slipping away!");
							lose_exp(d / 10);
						}
						else
						{
							msg_print("You feel your life draining away!");
							lose_exp(d);
						}
					}
					break;
				}

				/* Undefined hit */
				default:
				{
					/* Obvious */
					obvious = TRUE;

					/* Player armour reduces total damage */
					ac_dam(&dam, ac);

					/* Take damage */
					take_hit(dam, ddesc, TRUE);

					break;
				}
			}
			
			/* Handle monster death  XXX */
			if ((m_ptr->hp < 0) || (!m_ptr->maxhp)) return (TRUE);

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
				tmp = monster_critical(d_dice, d_side, dam, effect);

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
				tmp = monster_critical(d_dice, d_side, dam, effect);

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
				case RBM_SLASH:
				case RBM_PIERCE:
				case RBM_BLUNT:
				case RBM_CLAW:
				case RBM_KISS:
				case RBM_GRAB:
				case RBM_BITE:
				case RBM_STING:
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:
				case RBM_CRAWL:
				case RBM_DROOL:
				case RBM_SPIT:
				case RBM_ZAP:
				case RBM_PECK:
				case RBM_SEDUCE:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1, 0);

					/* Message */
					msg_format("%^s misses you.", m_name);
					break;
				}
				default: break;
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || dam || (l_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->r_blows[ap_cnt]++;
				}
			}
		}
	}


	if (touched && struck)
	{
		/* I need to replace this code */
		if (p_ptr->sh_fire && !(p_ptr->leaving) && alive)
		{
			if (!(r_ptr->flags3 & RF3_IM_FIRE))
			{
				msg_format("%^s is suddenly very hot!", m_name);
				if (mon_take_hit(m_idx, damroll(4,5), &fear,
				" turns into a pile of ash."))
				{
					blinked = FALSE;
					alive = FALSE;
				}
			}
			else
			{
				if (m_ptr->ml)
				{
					r_ptr->flags3 |= RF3_IM_FIRE;
				}
			}
		}
		if (p_ptr->sh_elec && !(p_ptr->leaving) && alive)
		{
			if (!(r_ptr->flags3 & RF3_IM_ELEC))
			{
				msg_format("%^s gets zapped!", m_name);
				if (mon_take_hit(m_idx, damroll(4,5), &fear,
				" turns into a pile of cinder."))
				{
					blinked = FALSE;
					alive = FALSE;
				}
			}
			else
			{
				if (m_ptr->ml)
				{
					r_ptr->flags3 |= RF3_IM_ELEC;
				}
			}
		}
		if (p_ptr->sh_spine && !(p_ptr->leaving) && alive)
		{
			if (!(r_ptr->flags3 & RF3_NO_STUN))
			{
				msg_format("%^s is pierced!", m_name);
				if (mon_take_hit(m_idx, damroll(4,5), &fear,
				" crumples to the ground."))
				{
					blinked = FALSE;
					alive = FALSE;
				}
			}
			else
			{
				if (m_ptr->ml)
				{
					r_ptr->flags3 |= RF3_NO_STUN;
				}
			}
		}

		if (p_ptr->counter && alive && !p_ptr->is_dead && m_ptr->ml && (p_ptr->csp > 7))
		{
			char m_name[80];
			monster_desc(m_name, m_ptr, 0);
	
			p_ptr->csp -= 7;
			msg_format("You counterattack %s!", m_name);
			py_attack(m_ptr->fy, m_ptr->fx, 0);
			fear = FALSE;
		}
		if ((p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_max > 0) && 
			!(p_ptr->leaving) && alive && martial)
		{
			if (rand_int(41) < p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_rank)
			{
				char m_name[80];
				monster_desc(m_name, m_ptr, 0);					
				msg_format("You counterattack %s!", m_name);
				py_attack(m_ptr->fy, m_ptr->fx, 0);
				fear = FALSE;
			}
		}
		/* reset touched */
		touched = FALSE;
	}

	/* Blink away */
	if (blinked)
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->r_deaths < MAX_SHORT))
	{
		l_ptr->r_deaths++;
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Assume we attacked */
	return (TRUE);
}


/*
 * Offsets for the spell indices
 */
#define RF4_OFFSET 32 * 3
#define RF5_OFFSET 32 * 4
#define RF6_OFFSET 32 * 5
#define RF7_OFFSET 32 * 6


/*
 * Monster attempts to make a ranged (non-melee) attack.
 *
 * Determine if monster can attack at range, then see if it will.  Use
 * the helper function "choose_attack_spell()" to pick a physical ranged
 * attack, magic spell, or summon.  Execute the attack chosen.  Process
 * its effects, and update character knowledge of the monster.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 */
bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px)
{
	int i, k, rlev, spower, rad, manacost;

	int m_idx = cave_m_idx[m_ptr->fy][m_ptr->fx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];
	char m_poss[80];

	char ddesc[80];

	/* Target player */
	int x = px;
	int y = py;

	/* Summon count */
	int count = 0;

	/* Summon level */
	int summon_lev;

	/* Is the player blind? */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Can the player see the monster casting the spell? */
	bool seen = (!blind && m_ptr->ml);
	
	/* Is the monster organic? */
	bool automata = (monster_automata(r_ptr));
	bool elemental = (monster_elemental(r_ptr));
  bool nonliving = (monster_nonliving(r_ptr));
	
	int save;
	
	save = p_ptr->skill_sav;
	
	/* Determine mana cost */
	if (attack >= 224) return (FALSE);
	else if (attack >= 192) manacost = mana_cost_RF7[attack-192];
	else if (attack >= 160) manacost = mana_cost_RF6[attack-160];
	else if (attack >= 128) manacost = mana_cost_RF5[attack-128];
	else if (attack >=  96) manacost = mana_cost_RF4[attack- 96];
	else return (FALSE);

	/* Spend mana */
	m_ptr->mana -= manacost;


	/*** Get some info. ***/

	/* Extract the monster level.  Must be at least 1. */
	rlev = MAX(1, r_ptr->level);

	/* Extract the monster's spell power.  Must be at least 1. */
	spower = MAX(1, r_ptr->spell_power);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, 0x22);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Get the summon level */
	if (r_ptr->d_char == 'v') summon_lev = r_ptr->level + 3;
	else                      summon_lev = r_ptr->level - 1;



	/*** Execute the ranged attack chosen. ***/
	switch (attack)
	{
		/* RF4_SHRIEK */
		case RF4_OFFSET+0:
		{
			disturb(1, 0);
			if (r_ptr->flags2 & (RF2_SMART))
				msg_format("%^s shouts for help.", m_name);
			else
				msg_format("%^s makes a high pitched shriek.", m_name);
			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_LASH */
		/* The sang source does some _crazy_ stuff with this */
		/* for right now, I'm keeping it simple. */
		case RF4_OFFSET+1:
		{
			/* Crack the whip, or spit - range 3 */
			beam(m_idx, GF_WHIP, get_dam(spower * 2, 4), 3);
			break;
		}

		/* RF4_ARROW */
		case RF4_OFFSET+2:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind) msg_print("You hear a soft twang.");
				else msg_format("%^s fires a small arrow.", m_name);
			}
			else if (spower < 15)
			{
				if (blind) msg_print("You hear a twang.");
				else msg_format("%^s fires an arrow.", m_name);
			}
			else
			{
				if (blind) msg_print("You hear a loud thwang.");
				else msg_format("%^s fires a seeker arrow.", m_name);
			}

			bolt(m_idx, GF_ARROW, get_dam(spower * 2, 6));
			break;
		}

		/* RF4_GUN */
		case RF4_OFFSET+3:
		{
			disturb(1, 0);
			if (spower < 8)
			{
				if (blind) msg_print("You hear a pop!.");
				else msg_format ("%^s fires a small pistol.", m_name);
			}

			else
			{
				if (blind) msg_print("You hear a loud bang!.");
				else msg_format ("%^s fires a pistol.", m_name);
			}
			bolt(m_idx, GF_BULLET, get_dam(spower * 3, 4));
			break;
		}

		/* RF4_RIFLE */
		case RF4_OFFSET+4:
		{
			disturb(1, 0);
			if (blind) msg_print("You hear a rifle shot.");
			else msg_format("%^s fires a rifle.", m_name);
			bolt(m_idx, GF_BULLET, get_dam(spower * 4, 4));
			break;
		}

		/* RF4_SHOTGUN */
		case RF4_OFFSET+5:
		{
			disturb(1, 0);
			if (blind) msg_format("You hear a shotgun blast.", m_name);
			else msg_format("%^s fires a shotgun!", m_name);
			bolt(m_idx, GF_SHOT, get_dam(spower * 6, 4));
			break;
		}

		/* RF4_ROCKET */
		case RF4_OFFSET+6:
		{
			disturb(1, 0);
			if (blind) msg_format("You hear a loud roar.", m_name);
			else msg_format("%^s fires a rocket!", m_name);
			ball(m_idx, GF_ROCKET, get_dam(spower * 8, 4), 2, FALSE);
			break;
		}

		/* RF4_MISSILE */
		case RF4_OFFSET+7:
		{
			disturb(1, 0);
			if (blind) msg_format("You hear a loud roaring whine.", m_name);
			else msg_format("%^s fires a guided missile!", m_name);
			ball(m_idx, GF_MISSILE, get_dam(spower * 10, 4), 3, FALSE);
			break;
		}

		/* Basic resists (max 500 dam, excepting steam) */
		/* RF4_BR_FIRE */
		case RF4_OFFSET+8:
		{
			int typ;
			if (spower < 5) typ = GF_HEAT;
			else if (spower > 14) typ = GF_PLASMA;
			else typ = GF_FIRE;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes fire.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 400),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_EARTH */
		case RF4_OFFSET+9:
		{
			int typ;
			if (spower < 5) typ = GF_ROCK;
			else if (spower > 14) typ = GF_SHARDS;
			else typ = GF_EARTH;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes earth.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 400),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_AIR */
		case RF4_OFFSET+10:
		{
			int typ;
			if (spower < 5) typ = GF_GUST;
			else if (spower > 14) typ = GF_GALE;
			else typ = GF_WIND;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes wind.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 400),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_WATER */
		/* Special few features due to theme and name */
		case RF4_OFFSET+11:
		{
			int typ;
			if (spower < 5) typ = GF_RUST;
			else if (spower > 14) typ = GF_STORM;
			else typ = GF_STEAM;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else if (typ == GF_RUST) msg_format("%^s breathes rust.", m_name);
			else if (typ == GF_STEAM) msg_format("%^s breathes steam.", m_name);
			else if (typ == GF_STORM) msg_format("%^s breathes storm.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 600),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}
		
		/* Advanced resists (max damage 700) */
		/* RF4_BR_ELEC */
		case RF4_OFFSET+12:
		{
			int typ;
			if (spower < 5) typ = GF_SHOCK;
			else if (spower > 14) typ = GF_VOLT;
			else typ = GF_ELEC;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes lightning.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 600),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_ICE */
		case RF4_OFFSET+13:
		{
			int typ;
			if (spower < 5) typ = GF_CHILL;
			else if (spower > 14) typ = GF_GLACIAL;
			else typ = GF_ICE;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes ice.", m_name);
			arc(m_idx, typ, 
			       MIN(m_ptr->hp / 2, 600),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_ACID */
		case RF4_OFFSET+14:
		{
			int typ;
			if (spower < 5) typ = GF_CORROSIVE;
			else if (spower > 14) typ = GF_LIQUESCE;
			else typ = GF_ACID;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes acid.", m_name);
			arc(m_idx, typ, 
			       MIN(m_ptr->hp / 2, 600),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_POISON */
		case RF4_OFFSET+15:
		{
			int typ;
			if (spower < 5) typ = GF_CAUSTIC;
			else if (spower > 14) typ = GF_CONTAGION;
			else typ = GF_POISON;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes poison.", m_name);
			arc(m_idx, typ, 
			       MIN(m_ptr->hp / 2, 600),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* High Resists - max damage 900 */
		/* RF4_BR_TIME */
		case RF4_OFFSET+16:
		{
			int typ;
			if (spower < 5) typ = GF_AGE;
			else if (spower > 14) typ = GF_CHRONOS;
			else typ = GF_TIME;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes temporal forces.", m_name);
			arc(m_idx, typ, 
			       MIN(m_ptr->hp / 2, 800),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_ETHER */
		case RF4_OFFSET+17:
		{
			int typ;
			if (spower < 5) typ = GF_VAPOR;
			else if (spower > 14) typ = GF_NEXUS;
			else typ = GF_ETHER;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes etheric forces.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 800),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_SOUND */
		case RF4_OFFSET+18:
		{
			int typ;
			if (spower < 5) typ = GF_VIBE;
			else if (spower > 14) typ = GF_SONIC;
			else typ = GF_SOUND;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes sonic force.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 800),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_NETHER */
		case RF4_OFFSET+19:
		{
			int typ;
			if (spower < 5) typ = GF_UNHOLY;
			else if (spower > 14) typ = GF_ABYSS;
			else typ = GF_NETHER;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes dark energies.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 800),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* Special breaths - variable maxes */
		/* RF4_BR_GRAVITY */
		case RF4_OFFSET+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes gravity.", m_name);
			arc(m_idx, GF_GRAVITY,
			       MIN(m_ptr->hp / 2, 350),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_RAD */
		case RF4_OFFSET+21:
		{
			int typ;
			if (spower < 5) typ = GF_WEAK_RAD;
			else typ = GF_STRONG_RAD;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes radiation.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 250),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_LIGHT */
		case RF4_OFFSET+22:
		{
			int typ;
			if (spower < 5) typ = GF_GLOW;
			else if (spower > 14) typ = GF_BRILLIANCE;
			else typ = GF_LIGHT;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes photons.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 150),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_BR_DARK */
		case RF4_OFFSET+23:
		{
			int typ;
			if (spower < 5) typ = GF_DIM;
			else if (spower > 14) typ = GF_TENEBROUS;
			else typ = GF_DARK;
			disturb(1, 0);
			if (blind) msg_format("%^s breathes.", m_name);
			else msg_format("%^s breathes anti-photons.", m_name);
			arc(m_idx, typ,
			       MIN(m_ptr->hp / 2, 200),
			       0, (r_ptr->flags2 & (RF2_POWERFUL) ? 40 : 20));
			break;
		}

		/* RF4_CLOUD_RAD */
		case RF4_OFFSET+24:
		{
			int typ, rad;
			if (spower < 10) typ = GF_WEAK_RAD;
			else typ = GF_STRONG_RAD;
			if (spower < 10) rad = 2;
			else rad = 3;
			disturb(1, 0);
			cloud(m_idx, typ, (get_dam(1 * spower, 8)), rad);
			break;
		}

		/* RF4_CLOUD_POISON */
		case RF4_OFFSET+25:
		{	
			int typ, rad;
			if (spower < 5) typ = GF_CAUSTIC;
			else if (spower > 14) typ = GF_CONTAGION;
			else typ = GF_POISON;
			if (spower < 5) rad = 2;
			else if (spower > 14) rad = 4;
			else rad = 3;
			disturb(1, 0);
			cloud(m_idx, typ, (get_dam(5 * spower, 4)), rad);			
			break;
		}

		/* RF4_XXX3 */
		case RF4_OFFSET+26:
		{
			break;
		}

		/* RF4_XXX4 */
		case RF4_OFFSET+27:
		{
			break;
		}

		/* RF4_XXX5 */
		case RF4_OFFSET+28:
		{
			break;
		}

		/* RF4_XXX6 */
		case RF4_OFFSET+29:
		{
			break;
		}

		/* RF4_XXX7 */
		case RF4_OFFSET+30:
		{
			break;
		}

		/* RF4_XXX8 */
		case RF4_OFFSET+31:
		{
			break;
		}

		/* Beginning of RF5 */
		/* Basic resists - all ball spowers mutiplied by '5' for average damage*/
		/* RF5_BA_FIRE */
		case RF5_OFFSET+0:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a gout of fire.", m_name);
				else msg_format("%^s casts a fireball.", m_name);
				rad = 0;
				typ = GF_HEAT;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				/* Bomb throwing ornithopter only. */
				else if (automata) msg_format("%^s throws a bomb.", m_name);
				else msg_format("%^s casts a firesphere.", m_name);
				rad = 2;
				typ = GF_FIRE;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a storm of super-heated plasma.", m_name);
				else msg_format("%^s invokes a plasma-storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_PLASMA;
			}
			ball(m_idx, typ, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_EARTH */
		case RF5_OFFSET+1:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges rocks and earth.", m_name);
				else msg_format("%^s casts a rockball.", m_name);
				rad = 0;
				typ = GF_ROCK;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a sphere of rocks and dirt.", m_name);
				else msg_format("%^s casts an earthsphere.", m_name);
				rad = 2;
				typ = GF_EARTH;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a storm of deadly shards.", m_name);
				else msg_format("%^s invokes a shard-storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_SHARDS;
			}
			ball(m_idx, typ, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_AIR */
		case RF5_OFFSET+2:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a blast of wind.", m_name);
				else msg_format("%^s casts an windball.", m_name);
				rad = 0;
				typ = GF_GUST;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a swirling winds.", m_name);
				else msg_format("%^s casts an windsphere.", m_name);
				rad = 2;
				typ = GF_WIND;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a storm of high powered gales of wind.", m_name);
				else msg_format("%^s invokes a gale-storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_GALE;
			}
			ball(m_idx, typ, get_dam(5 * spower, 6), rad, FALSE);
			break;
		}

		/* Steam does slightly more damage on average */
		/* RF5_BA_WATER */
		case RF5_OFFSET+3:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a gout of steam.", m_name);
				else msg_format("%^s casts an steamball.", m_name);
				rad = 0;
				typ = GF_RUST;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a gouts of steam.", m_name);
				else msg_format("%^s casts an steamsphere.", m_name);
				rad = 2;
				typ = GF_STEAM;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a storm of super-heated water.", m_name);
				else msg_format("%^s invokes a storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_STORM;
			}
			ball(m_idx, typ, get_dam(6 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_ELEC */
		case RF5_OFFSET+4:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a spark.", m_name);
				else msg_format("%^s casts an lightning ball.", m_name);
				rad = 0;
				typ = GF_SHOCK;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a voltage shower.", m_name);
				else msg_format("%^s casts an electricty ball.", m_name);
				rad = 2;
				typ = GF_ELEC;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a storm of electricty.", m_name);
				else msg_format("%^s invokes a lightning storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_VOLT;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_ICE */
		case RF5_OFFSET+5:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges an icicle.", m_name);
				else msg_format("%^s casts an snowball.", m_name);
				rad = 0;
				typ = GF_CHILL;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a sphere of thermal stasis.", m_name);
				else msg_format("%^s casts an ice ball.", m_name);
				rad = 2;
				typ = GF_ICE;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges sphere of total entropic thermal statis.", m_name);
				else msg_format("%^s invokes a glacial storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_GLACIAL;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_ACID */
		case RF5_OFFSET+6:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a splash of acid.", m_name);
				else msg_format("%^s casts an acid splash.", m_name);
				rad = 0;
				typ = GF_CORROSIVE;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a gout of corrosive.", m_name);
				else msg_format("%^s casts an acid ball.", m_name);
				rad = 2;
				typ = GF_ACID;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a shower of molecular bond destoryer.", m_name);
				else msg_format("%^s invokes a acid storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_LIQUESCE;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_POISON */
		case RF5_OFFSET+7:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges an organic toxin.", m_name);
				else msg_format("%^s casts an poison splash.", m_name);
				rad = 0;
				typ = GF_CAUSTIC;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a cloud of organic toxin.", m_name);
				else msg_format("%^s casts an poison gas cloud.", m_name);
				rad = 2;
				typ = GF_POISON;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a giant cloud of toxins.", m_name);
				else msg_format("%^s invokes a poison storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_CONTAGION;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_TIME */
		case RF5_OFFSET+8:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a taychon beam.", m_name);
				else msg_format("%^s manipulates chronos.", m_name);
				rad = 0;
				typ = GF_AGE;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a sphere of taychon particles.", m_name);
				else msg_format("%^s casts a time field.", m_name);
				rad = 2;
				typ = GF_TIME;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges an intense storm of taychon energy.", m_name);
				else msg_format("%^s invokes a time storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_CHRONOS;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_ETHER */
		case RF5_OFFSET+9:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges etheric forces.", m_name);
				else msg_format("%^s manipulates etheric forces.", m_name);
				rad = 0;
				typ = GF_VAPOR;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a sphere of etheric energy.", m_name);
				else msg_format("%^s casts an etheric field.", m_name);
				rad = 2;
				typ = GF_ETHER;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s warps etheric energies.", m_name);
				else msg_format("%^s invokes a ether storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_NEXUS;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_SOUND */
		case RF5_OFFSET+10:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s discharges a focused blast of sound.", m_name);
				else msg_format("%^s casts a thunderclap.", m_name);
				rad = 0;
				typ = GF_VIBE;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				else if (automata) msg_format("%^s discharges a sound-sphere.", m_name);
				else msg_format("%^s casts a sonic field.", m_name);
				rad = 2;
				typ = GF_SOUND;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s discharges a sonic blast.", m_name);
				else msg_format("%^s invokes a sonic storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_SONIC;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_NETHER */
		case RF5_OFFSET+11:
		{
			int typ;
			disturb (1, 0);
			if (spower < 20)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				/* Non-living things shouldn't use nether */
				/* else if (automata) msg_format("%^s discharges a taychon beam", m_name); */
				else msg_format("%^s draws on unholy forces.", m_name);
				rad = 0;
				typ = GF_UNHOLY;
			}
			else if (spower < 60)
			{
				if (blind) msg_format("%^s makes deep noises.", m_name);
				/* Non-living things shouldn't use nether */
				/* else if (automata) msg_format("%^s discharges a taychon beam", m_name); */
				else msg_format("%^s casts a ball of dark forces.", m_name);
				rad = 2;
				typ = GF_NETHER;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				/* Non-living things shouldn't use nether */
				/* else if (automata) msg_format("%^s discharges a taychon beam", m_name); */
				else msg_format("%^s invokes a nether storm.", m_name);
				if (spower < 120) rad = 3;
				else rad = 4;
				typ = GF_ABYSS;
			}
			ball(m_idx, typ, get_dam(7 * spower, 6), rad, FALSE);
			break;
		}

		/* RF5_BA_GRAVITY */
		case RF5_OFFSET+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			else if (automata) msg_format("%^s creates a gravity wave.", m_name);
			ball(m_idx, GF_GRAVITY, get_dam(8 * spower, 5), 3, FALSE);
			break;
		}

		/* RF5_BA_EMP */
		case RF5_OFFSET+13:
		{
			disturb(1, 0);
			/* Caster of emp must be organic */
			if (blind) msg_format("%^s makes noise.", m_name);
			/* No non-living */
			else msg_format("%^s sets off an emp blast!", m_name);
			/* Watch out! This attack is strong! */
			ball(m_idx, GF_EMP, get_dam(10 * spower, 4), 5, FALSE);
			break;
		}

		/* RF5_BA_RAD */
		case RF5_OFFSET+14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			else if (automata) msg_format("%^s emits radiation!", m_name);
			/* Should only be cthuloid */
			else msg_format("%^s draws on dark forces of chaos and mutation!", m_name);
			if (spower < 60)
				ball(m_idx, GF_WEAK_RAD, get_dam(8 * spower, 4), 3, FALSE);
			else ball(m_idx, GF_STRONG_RAD, get_dam(9 * spower, 4), 4, FALSE);
			break;
		}

		/* RF5_XXX1 */
		case RF5_OFFSET+15:
		{
			break;
		}

		/* RF5_BO_FIRE */
		case RF5_OFFSET+16:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of fire.", m_name);
				else msg_format("%^s casts a fire bolt.", m_name);
				typ = GF_FIRE;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of super-heated plasma.", m_name);
				else msg_format("%^s casts a bolt of plasma.", m_name);
				typ = GF_PLASMA;
			}
			bolt(m_idx, typ, get_dam(4 * spower, 6));
			break;
		}

		/* RF5_BO_EARTH */
		case RF5_OFFSET+17:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots rocks.", m_name);
				else msg_format("%^s casts an earth bolt.", m_name);
				typ = GF_EARTH;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots shards.", m_name);
				else msg_format("%^s casts a bolt of shards.", m_name);
				typ = GF_SHARDS;
			}
			bolt(m_idx, typ, get_dam(4 * spower, 6));
			break;
		}

		/* RF5_BO_AIR */
		case RF5_OFFSET+18:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s blows high-pressured air.", m_name);
				else msg_format("%^s casts a gust of wind.", m_name);
				typ = GF_WIND;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s blows gale winds at you.", m_name);
				else msg_format("%^s casts a cyclone at you.", m_name);
				typ = GF_GALE;
			}
			bolt(m_idx, typ, get_dam(4 * spower, 6));
			break;
		}

		/* RF5_BO_WATER */
		case RF5_OFFSET+19:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a blast of steam.", m_name);
				else if (strchr("q", r_ptr->d_char)) msg_format("%^s sprays a blast of water.", m_name);
				else msg_format("%^s casts a steam bolt.", m_name);
				typ = GF_STEAM;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of super-heated steam.", m_name);
				else msg_format("%^s casts a storming bolt.", m_name);
				typ = GF_STORM;
			}
			bolt(m_idx, typ, get_dam(4 * spower, 6));
			break;
		}

		/* RF5_BO_ELEC */
		case RF5_OFFSET+20:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of lightning.", m_name);
				else msg_format("%^s casts a lightning bolt.", m_name);
				typ = GF_ELEC;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of high-voltage lightning.", m_name);
				else msg_format("%^s casts a powerful lightning bolt.", m_name);
				typ = GF_VOLT;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_ICE */
		case RF5_OFFSET+21:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of ice.", m_name);
				else msg_format("%^s casts an ice bolt.", m_name);
				typ = GF_ICE;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of freezing cold.", m_name);
				else msg_format("%^s casts a glacial ice bolt.", m_name);
				typ = GF_GLACIAL;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_ACID */
		case RF5_OFFSET+22:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of acid.", m_name);
				else msg_format("%^s casts an acid bolt.", m_name);
				typ = GF_ACID;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of high-powered corrosive.", m_name);
				else msg_format("%^s casts a corrosive bolt.", m_name);
				typ = GF_LIQUESCE;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_POISON */
		case RF5_OFFSET+23:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of toxin.", m_name);
				else msg_format("%^s casts a toxic bolt.", m_name);
				typ = GF_POISON;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of poison.", m_name);
				else msg_format("%^s casts a poison bolt.", m_name);
				typ = GF_CONTAGION;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_TIME */
		case RF5_OFFSET+24:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of taychons.", m_name);
				else msg_format("%^s casts a time bolt.", m_name);
				typ = GF_TIME;
			}
			else
			{
				/* bleah, need better visual/written cues */
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of taychons.", m_name);
				else msg_format("%^s casts a time bolt.", m_name);
				typ = GF_CHRONOS;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_ETHER */
		case RF5_OFFSET+25:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of etheric forces.", m_name);
				else msg_format("%^s casts an ether bolt.", m_name);
				typ = GF_ETHER;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of etheric forces.", m_name);
				else msg_format("%^s casts a nexus bolt.", m_name);
				typ = GF_NEXUS;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_SOUND */
		case RF5_OFFSET+26:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots a blast of sound.", m_name);
				else msg_format("%^s casts a sound bolt.", m_name);
				typ = GF_SOUND;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots a bolt of sonic power.", m_name);
				else msg_format("%^s casts a sonic bolt.", m_name);
				typ = GF_SONIC;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_NETHER */
		case RF5_OFFSET+27:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				/* NOPE- not if you're a machine, how are you getting in touch with */
				/* Dark forces anyway? */
				/* else if (automata) msg_format("%^s shoots a bolt of.", m_name); */
				else msg_format("%^s casts a nether bolt.", m_name);
				typ = GF_NETHER;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				/* NOPE- not if you're a machine, how are you getting in touch with */
				/* Dark forces anyway? */
				/* else if (automata) msg_format("%^s shoots a bolt of.", m_name); */
				else msg_format("%^s casts a bolt of nefarious dark energies.", m_name);
				typ = GF_ABYSS;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_BO_GRAVITY */
		case RF5_OFFSET+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes a lot of noise.", m_name);
			else if (automata) msg_format("%^s shoots a bolt of graviton waves.", m_name);
			else msg_format("%^s casts a gravity bolt.", m_name);
			bolt(m_idx, GF_GRAVITY, get_dam(6 * spower, 6));
			break;
		}

		/* RF5_BO_DARK */
		case RF5_OFFSET+29:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s shoots beam of anti-photons.", m_name);
				else msg_format("%^s casts a bolt of darkness.", m_name);
				typ = GF_DARK;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				else if (automata) msg_format("%^s shoots beam of photic annihilators.", m_name);
				else msg_format("%^s casts a bolt of tenebrous nightdark.", m_name);
				typ = GF_TENEBROUS;
			}
			bolt(m_idx, typ, get_dam(5 * spower, 6));
			break;
		}

		/* RF5_XXX3 */
		case RF5_OFFSET+30:
		{
			break;
		}

		/* RF5_XXX4 */
		case RF5_OFFSET+31:
		{
			break;
		}

		/* RF6_HASTE */
		case RF6_OFFSET+0:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			else if (automata) msg_format("%^s spends a moment processing.", m_name);
			else msg_format("%^s concentrates on %s body.", m_name, m_poss);

			/* Allow quick speed increases to base+10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				msg_format("%^s starts moving faster.", m_name);
				m_ptr->mspeed += 10;
			}

			/* Allow small speed increases to base+20 */
			else if (m_ptr->mspeed < r_ptr->speed + 20)
			{
				msg_format("%^s starts moving slightly faster.", m_name);
				m_ptr->mspeed += 2;
			}
			break;
		}

		/* RF6_CURE */
		case RF6_OFFSET+1:
		{
			if (m_ptr->ml)
				msg_format("%^s concentrates on %s ailments.", m_name, m_poss);

			/* Cancel stunning */
			if (m_ptr->stunned)
			{
				/* Cancel stunning */
				m_ptr->stunned = 0;

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s is no longer stunned.", m_name);
			}

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				set_mon_fear(m_ptr, 0, FALSE);

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s recovers %s courage.", m_name, m_poss);

			}

			/* Cancel (major) slowing */
			if (m_ptr->mspeed < r_ptr->speed - 5)
			{
				/* Cancel slowing */
				m_ptr->mspeed = r_ptr->speed;

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s is no longer slowed.", m_name);
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			break;
		}

		/* RF6_HEAL */
		case RF6_OFFSET+2:
		{
			int gain, cost;
			disturb(1, 0);
			/* Message */
			if (m_ptr->ml)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s spends a moment processing.", m_name);
				else msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
			}

			/* We regain lost hitpoints (up to spower * 5) */
			gain = MIN(m_ptr->maxhp - m_ptr->hp, spower * 5);

			/* We do not gain more than mana * 20 HPs at a time */
			gain = MIN(gain, m_ptr->mana * 20);

			/* Regain some hitpoints */
			m_ptr->hp += gain;

			/* Lose some mana (high-level monsters are more efficient) */
			cost = 1 + gain / (5 + 4 * r_ptr->level / 5);

			/* Reduce mana (do not go negetive) */
			m_ptr->mana -= MIN(cost, m_ptr->mana);

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (m_ptr->ml)
				{
					if (seen && automata) msg_format("%^s looks fully repaired!",  m_name);
					else if (seen) msg_format("%^s looks very healthy!",  m_name);
					else      msg_format("%^s sounds very healthy!", m_name);
				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (m_ptr->ml)
				{
					if (seen && automata) msg_format("%^s looks repaired.",  m_name);
					else if (seen) msg_format("%^s looks healthier.",  m_name);
					else      msg_format("%^s sounds healthier.", m_name);
				}
			}


			/* Redraw (later) if needed */
			if ((p_ptr->health_who == m_idx) && (m_ptr->ml))
				p_ptr->redraw |= (PR_HEALTH);

			/* Cancel fear */
			if (m_ptr->monfear)
			{
				/* Cancel fear */
				set_mon_fear(m_ptr, 0, FALSE);

				/* Message */
				if (m_ptr->ml)
					msg_format("%^s recovers %s courage.", m_name, m_poss);
			}

			/* Recalculate combat range later */
			m_ptr->min_range = 0;
			break;
		}

		/* RF6_ADD_MANA */
		case RF6_OFFSET+3:
		{
			if (m_ptr->ml) disturb(1, 0);
			if (m_ptr->ml)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				else if (automata) msg_format("%^s spends a moment charging up.", m_name);
				else msg_format("%^s gathers %s power.", m_name, m_poss);
			}
			/* Increase current mana.  Do not exceed maximum. */
			m_ptr->mana += (spower / 20) + 5;
			if (m_ptr->mana > r_ptr->mana) m_ptr->mana = r_ptr->mana;
			break;
		}

		/* RF6_BLINK */
		case RF6_OFFSET+4:
		{
			if (m_ptr->ml) disturb(1, 0);
			teleport_away(m_idx, 10);

			/* If it comes into view from around a corner (unlikely) */
			/* give a message and learn about the casting */
			if (!seen && m_ptr->ml)
			{
				seen = TRUE;
				if (automata) msg_format("Space warps in front of you and %^s appears.", m_name);
				else msg_format("%^s blinks into view.", m_name);
			}
			
			/* Normal message */
			else
			{
				if (automata && seen) msg_format("Space warps in front of you and %^s dissapears.", m_name);
				else if (seen) msg_format("%^s blinks away.", m_name);
			}
			break;
		}

		/* RF6_TPORT */
		case RF6_OFFSET+5:
		{
			disturb(1, 0);
			if (m_ptr->ml) msg_format("%^s teleports away.", m_name);
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);
			break;
		}

		/* RF6_XXX1 */
		case RF6_OFFSET+6:
		{
			break;
		}

		/* RF6_XXX2 */
		case RF6_OFFSET+7:
		{
			break;
		}

		/* RF6_TELE_TO */
		case RF6_OFFSET+8:
		{
			disturb(1, 0);
			msg_format("%^s commands you to return.", m_name);
			if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				teleport_player_to(m_ptr->fy, m_ptr->fx);
			}
			seen = TRUE;
			break;
		}

		/* RF6_TELE_AWAY */
		case RF6_OFFSET+9:
		{
			disturb(1, 0);
			msg_format("%^s teleports you away.", m_name);
			if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				teleport_player(100);
			}
			break;
		}

		/* RF6_TELE_LEVEL */
		case RF6_OFFSET+10:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles strangely.", m_name);
			else msg_format("%^s gestures at your feet.", m_name);
			if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				teleport_player_level(FALSE);
			}
			break;
		}

		/* RF6_TELE_SELF_TO */
		case RF6_OFFSET+11:
		{
			int old_cdis = m_ptr->cdis;

			/* Move monster near player (also updates "m_ptr->ml"). */
			teleport_towards(m_ptr->fy, m_ptr->fx, py, px, FALSE);

			/* Monster is now visible, but wasn't before. */
			if ((!seen) && (m_ptr->ml))
			{
				/* Get the name (using "A"/"An") again. */
				monster_desc(ddesc, m_ptr, 0x08);

				/* Message */
				msg_format("%^s suddenly appears.", ddesc);
			}

			/* Monster was visible before, but isn't now. */
			else if ((seen) && (!m_ptr->ml))
			{
				/* Message */
				msg_format("%^s blinks away.", m_name);
			}

			/* Monster is visible both before and after. */
			else if ((seen) && (m_ptr->ml))
			{
				if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) <
				    old_cdis - 1)
				{
					msg_format("%^s blinks toward you.", m_name);
				}
				else
				{
					msg_format("%^s blinks.", m_name);
				}
			}

			/* Have we seen them at any point?  If so, we will learn about the spell. */
			if (m_ptr->ml) seen = TRUE;

			break;
		}

		/* RF6_DARKNESS */
		case RF6_OFFSET+12:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			else if (automata) msg_format("%^s absorbs photons.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
			(void)unlite_area(0, 3);
			break;
		}

		/* RF6_TRAPS */
		case RF6_OFFSET+13:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			if (automata) msg_format("%^s makes strange whirring and clacking noises.", m_name);
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
			if (automata) (void)trap_creation(m_ptr->fy, m_ptr->fx);
			else (void)trap_creation(py, px);
			break;
		}

		/* RF6_FORGET */
		case RF6_OFFSET+14:
		{
			disturb(1, 0);
			if (automata) msg_format("%^s creates a strobic light display.", m_name);
			else msg_format("%^s tries to blank your mind.", m_name);
			if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
			}
			else if (lose_all_info())
			{
				msg_print("Your memories fade away.");
			}
			break;
		}

		/* RF6_FEAR */
		/* This for consistancy's sake should probably be handled with project. */
		case RF6_OFFSET+15:
		{
			disturb(1, 0);
			/* Being blind makes you immune to fear from automata and other non-living */
			if (blind && automata) break;
			else if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else if (automata) msg_format("%^s projects a horrifying hologram.", m_name);
			else msg_format("%^s casts a fearful illusion.", m_name);
			ball(m_idx, GF_FEAR, get_dam(spower, 8), 0, TRUE);
			break;
		}

		/* RF6_PSI ne:mind blast NO AUTOMATA/NON-LIVING */
		/* NOT WORKING XXX */
		case RF6_OFFSET+16:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s gazes deep into your eyes.", m_name);
			}

			/* Project a mental attack */
			ball(m_idx, GF_PSI, get_dam(spower, 8), 0, TRUE);

			break;
		}

		/* RF6_DOMINATION ne: brainsmash NO AUTOMATA/NON-LIVING */
		/* NOT WORKING XXX */
		case RF6_OFFSET+17:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel something focusing on your mind.");
			}
			else
			{
				msg_format("%^s gazes deep into your eyes.", m_name);
			}

			/* Project a mental attack */
			ball(m_idx, GF_DOMINATION, get_dam(spower, 8), 0, TRUE);
			break;
		}

		/* RF6_STUN */
		case RF6_OFFSET+18:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel invisible force smash into you.");
			}
			else if (elemental)
			{
				msg_format("%^s blasts you with rocks!", m_name);
			}
			else
			{
				msg_format("%^s gazes intently at you.", m_name);
			}

			/* Project a mental attack */
			ball(m_idx, GF_STUN, get_dam(spower, 8), 0, TRUE);

			break;
		}

		/* RF6_TK */
		case RF6_OFFSET+19:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel invisible force smash into you.");
			}
			/* These need to be fixed, the Air elementals aren't gazing XCCCX */
			else if (elemental)
			{
				msg_format("%^s blasts you with rocks!", m_name);
			}
			else
			{
				msg_format("%^s gazes intently at you.", m_name);
			}

			/* Project a mental attack */
			ball(m_idx, GF_TK, get_dam(spower, 8), 0, TRUE);

			break;
		}

		/* RF6_FORCE */
		case RF6_OFFSET+20:
		{
			disturb(1, 0);
			if (!seen)
			{
				msg_print("You feel invisible force smash into you.");
			}
			else if (elemental)
			{
				msg_format("%^s blasts you with rocks!", m_name);
			}
			else
			{
				msg_format("%^s gazes intently at you.", m_name);
			}

			/* Project a mental attack */
			ball(m_idx, GF_FORCE, get_dam(spower, 8), 0, TRUE);

			break;
		}

		/* RF6_CONFUSION */
		case RF6_OFFSET+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s creates a mesmerising illusion.", m_name);
			/* Project a mental attack */
			ball(m_idx, GF_CONFUSION, get_dam(spower, 8), 0, TRUE);

			break;
		}

		/* RF6_SPIRIT */
		case RF6_OFFSET+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s summons spirits to assail you.", m_name);
			/* Project a mental attack */
			ball(m_idx, GF_SPIRIT, get_dam(2 * spower, 8), 0, TRUE);
			break;
		}

		/* RF6_ECTOPLASM */
		case RF6_OFFSET+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else msg_format("%^s blasts you with the raw essence of spirit.", m_name);

			/* Project a mental attack */
			ball(m_idx, GF_ECTOPLASM, get_dam(3 * spower, 8), 0, TRUE);
			break;
		}

		/* RF6_BLIND */
		case RF6_OFFSET+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			/* Must not already be blind */
			else if (!p_ptr->blind)
			{
				if (automata) msg_format("%^s emits a blinding flash of light!", m_name);
				else msg_format("%^s casts a spell, burning your eyes!", m_name);
				if (p_ptr->resist_blind)
				{
					msg_print("You are unaffected!");
				}
				else if (rand_int(100) < save)
				{
					msg_print("You blink, and your vision clears.");
				}
				else
				{
					i = div_round(r_ptr->level, 2);
					(void)set_blind(p_ptr->blind + i + rand_range(5, 10));
				}
			}
			break;
		}

		/* RF6_SLOW */
		case RF6_OFFSET+25:
		{
			disturb(1, 0);
			if (automata) msg_format("%^s focuses entropy acceleration panels at you!", m_name);
			else msg_format("%^s drains power from your muscles!", m_name);
			if (p_ptr->free_act)
			{
				msg_print("You are unaffected!");
			}
			else if (rand_int(100) < save)
			{
				msg_print("You resist the effects!");
			}
			else
			{
				i = div_round((r_ptr->level * 2), 25);
				(void)set_slow(p_ptr->slow + i + rand_range(5, 10));
			}
			break;
		}

		/* RF6_HOLD */
		case RF6_OFFSET+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s makes noise.", m_name);
			else if (automata) msg_format("%^s fires a pinpoint nerve disruption beam!", m_name);
			else msg_format("%^s stares deep into your eyes!", m_name);
			if (p_ptr->free_act)
			{
				if (!p_ptr->paralyzed) msg_print("You are unaffected!");
			}
			else if (rand_int(100) < save)
			{
				if (!p_ptr->paralyzed) msg_print("You stare back unafraid!");
			}
			/* Must not already be paralyzed */
			else if (!p_ptr->paralyzed)
			{
				(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 8));
			}
			break;
		}

		/* RF6_DRAIN_MANA */
		case RF6_OFFSET+27:
		{
			if (p_ptr->csp)
			{
				int r1;
				int tmpsave;
				int resist;
				
				/* this is some HACK ish jimmy rigging I've done to make */
				/* the sang code work. */
				tmpsave = save / 4;
				if (tmpsave < 2) tmpsave = 2;
				if (tmpsave > 5) tmpsave = 5;
				resist = tmpsave;
				
				/* Attack power */
				r1 = (rand_spread(spower, spower / 2) / resist) + 2;

				/* Allow saving throw for wizard-protected characters */
				/* if ((p_ptr->wiz_prot) && (!one_in_(3))) r1 = 0; */
				/* Standard save */
				if (randint(100) > save) r1 = 0;

				/* Disturb if legal */
				disturb(1, 0);

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

				/* Note if anything happened */
				if (r1)
				{
					msg_format("%^s draws psychic energy from you!", m_name);
				}

				/* Note immunity */
				else if (p_ptr->csp)
				{
					msg_format("%^s tries to draw psychic energy from you, but fails!", m_name);
					break;
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

				/* Replenish monster mana */
				if (m_ptr->mana < r_ptr->mana)
				{
					if ( r1 > r_ptr->mana - m_ptr->mana)
					{
						 r1 -= r_ptr->mana - m_ptr->mana;
						 m_ptr->mana = r_ptr->mana;
					}
					else
					{
						 m_ptr->mana += r1;
						 r1 = 0;
					}
				}

				/* Heal the monster with remaining energy */
				if ((m_ptr->hp < m_ptr->maxhp) && (r1))
				{
					/* Heal */
					m_ptr->hp += (30 * (r1 + 1));
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

					/* Special message */
					if (seen)
					{
						msg_format("%^s appears healthier.", m_name);
					}
				}
			}
			break;
		}

		/* RF6_CHARGE */
		case RF6_OFFSET+28:
		{
			int old_cdis = m_ptr->cdis;

			/* Move monster near player (also updates "m_ptr->ml"). */
			teleport_towards(m_ptr->fy, m_ptr->fx, py, px, TRUE);

			/* Monster is now visible, but wasn't before. */
			if ((!seen) && (m_ptr->ml))
			{
				/* Get the name (using "A"/"An") again. */
				monster_desc(ddesc, m_ptr, 0x08);

				/* Message */
				msg_format("%^s charges you.", ddesc);
			}

			/* Monster was visible before, but isn't now. */
			else if ((seen) && (!m_ptr->ml))
			{
				/* Message */
				msg_format("%^s charges you and vanishes.", m_name);
			}

			/* Monster is visible both before and after. */
			else if ((seen) && (m_ptr->ml))
			{
				if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) <
				    old_cdis - 1)
				{
					msg_format("%^s charges toward you.", m_name);
				}
				else
				{
					msg_format("%^s charges.", m_name);
				}
			}

			/* Have we seen them at any point?  If so, we will learn about the spell. */
			if (m_ptr->ml) seen = TRUE;
			
			/* HACK - this is the only other place monster attacks are called */
			if (m_ptr->cdis < 2) 
			{
						/* Prevent this attack (paranoia) if it is a pet */
						/* Attack if possible */
						if (!(r_ptr->flags1 & (RF1_NEVER_BLOW)) && !(is_pet(m_ptr)))
						{
								(void)make_attack_normal(m_ptr);
						}

			}
			break;
		}


		/* RF6_XXX4 */
		case RF6_OFFSET+29:
		{
			break;
		}

		/* RF6_XXX5 */
		case RF6_OFFSET+30:
		{
			break;
		}

		/* RF6_MIRROR_IMAGE */
		case RF6_OFFSET+31:
		{
			if (blind) msg_format("%^s makes noise.", m_name);
			else msg_format("%^s tries to cast mirror image on %s body.", m_name, m_poss);
			
			/* Heal fully */
			m_ptr->hp = m_ptr->maxhp;

			/* Attempt to clone. */
			/* Should perhaps allow pets to be cloned? */
			if (multiply_monster(cave_m_idx[m_ptr->fy][m_ptr->fx], FALSE, TRUE))
			{
				msg_format("%^s spawns a replica!", m_name);
			}
			/* Clone self stuffs goes here */
			break;
		}
	
		/* RF7_BE_FIRE */
		case RF7_OFFSET+0:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				if (automata) msg_format("%^s fires a heat ray at you.", m_name);
				else msg_format("%^s casts an beam of fire.", m_name);
				typ = GF_FIRE;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				if (automata) msg_format("%^s fires a plasma ray at you.", m_name);
				else msg_format("%^s casts an beam of plasma.", m_name);
				typ = GF_PLASMA;
			}
			beam(m_idx, typ, get_dam(5 * spower, 6), 12);
			break;
		}
		
		/* RF7_BE_ELEC */
		case RF7_OFFSET+1:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				if (automata) msg_format("%^s fires a lightning ray at you.", m_name);
				else msg_format("%^s casts an lightning bolt.", m_name);
				typ = GF_ELEC;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				if (automata) msg_format("%^s fires a voltage ray at you.", m_name);
				else msg_format("%^s casts an high voltage electric bolt.", m_name);
				typ = GF_VOLT;
			}
			beam(m_idx, typ, get_dam(5 * spower, 6), 12);
			break;
		}
		
		/* RF7_BE_WATER */
		case RF7_OFFSET+2:
		{
			int typ;
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) msg_format("%^s makes noise.", m_name);
				if (automata) msg_format("%^s fires a spray of water at you.", m_name);
				else msg_format("%^s casts an geyser.", m_name);
				typ = GF_RUST;
			}
			else
			{
				if (blind) msg_format("%^s makes a lot of noise.", m_name);
				if (automata) msg_format("%^s fires a scalding spray at you.", m_name);
				else msg_format("%^s casts an volcanic geyser.", m_name);
				typ = GF_STEAM;
			}
			beam(m_idx, typ, get_dam(5 * spower, 6), 12);
			break;
		}
		/* RF7_XXX7X4 */
		case RF7_OFFSET+3:
		{
			break;
		}
		/* RF7_XXX7X5 */
		case RF7_OFFSET+4:
		{
			break;
		}
		/* RF7_XXX7X6 */
		case RF7_OFFSET+5:
		{
			break;
		}
		/* RF7_XXX7X7 */
		case RF7_OFFSET+6:
		{
			break;
		}
		/* RF7_XXX7X8 */
		case RF7_OFFSET+7:
		{
			break;
		}
		/* RF7_XXX7X9 */
		case RF7_OFFSET+8:
		{
			break;
		}
		/* RF7_XXX7X10 */
		case RF7_OFFSET+9:
		{
			break;
		}
		/* RF7_XXX7X11 */
		case RF7_OFFSET+10:
		{
			break;
		}
		/* RF7_XXX7X12 */
		case RF7_OFFSET+11:
		{
			break;
		}
		/* RF7_XXX7X13 */
		case RF7_OFFSET+12:
		{
			break;
		}
		/* RF7_XXX7X14 */
		case RF7_OFFSET+13:
		{
			break;
		}
		/* RF7_S_CUTTENCLIP */
		case RF7_OFFSET+14:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s makes Cuttenclip troops!", m_name);
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_CUTTENCLIP, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_BEASTMEN */
		case RF7_OFFSET+15:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons beastmen!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_BEASTMAN, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_PLANTS */
		case RF7_OFFSET+16:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s grows some plants!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev,
					SUMMON_PLANT, FALSE);
			}
			if (blind && count)
			{
				msg_print("You hear many growing things appear nearby.");
			}
			break;
		}

		/* RF7_S_KIN */
		case RF7_OFFSET+17:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons %s %s.", m_name,
				m_poss, ((r_ptr->flags1) & RF1_UNIQUE ?
				"minions" : "kin"));

			/* Hack -- Set the letter of the monsters to summon */
			summon_kin_type = r_ptr->d_char;
			for (k = 0; k < (rlev > 20 ? 3 : 2); k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx,
					summon_lev, SUMMON_KIN, FALSE);
			}

			if (blind && count)
			{
				msg_print("You hear many things appear nearby.");
			}
			break;
		}

		/* RF7_S_HI_DEMON */
		case RF7_OFFSET+18:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons greater demons!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev,
					SUMMON_HI_DEMON, FALSE);
			}
			if (blind && count)
			{
				msg_print("You hear many evil things appear nearby.");
			}
			break;
		}
		/* RF7_S_MONSTER */
		case RF7_OFFSET+19:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons help!", m_name);
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, FALSE);
			}
			if (blind && count) msg_print("You hear something appear nearby.");
			break;
		}
		/* RF7_S_MONSTERS */
		case RF7_OFFSET+20:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monsters!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, 0, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_AUTOMATA */
		case RF7_OFFSET+21:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons automata!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_AUTOMATA, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_SPIDER */
		case RF7_OFFSET+22:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons spiders!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_SPIDER, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_HOUND */
		case RF7_OFFSET+23:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons hounds!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_HOUND, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_MONKEY */
		case RF7_OFFSET+24:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons monkeys!", m_name);
			for (k = 0; k < 5; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_MONKEY, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_ALIEN */
		case RF7_OFFSET+25:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons aliens!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_ALIEN, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_DEMON */
		case RF7_OFFSET+26:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons demons!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_DEMON, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_UNDEAD */
		case RF7_OFFSET+27:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons undead!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_UNDEAD, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_ELEMENTAL */
		case RF7_OFFSET+28:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons elementals!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_ELEMENTAL, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_HI_UNDEAD */
		case RF7_OFFSET+29:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons major undead!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_HI_UNDEAD, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_HI_ELEMENTAL */
		case RF7_OFFSET+30:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);
			else msg_format("%^s magically summons major elementals!", m_name);
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summon_lev, SUMMON_HI_ELEMENTAL, FALSE);
			}
			if (blind && count) msg_print("You hear many things appear nearby.");
			break;
		}
		/* RF7_S_UNIQUE */
		case RF7_OFFSET+31:
		{
			disturb(1, 0);
			if (blind) msg_format("%^s mumbles.", m_name);

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, 
					summon_lev, SUMMON_UNIQUE, FALSE);
			}
			if (count)
			{
				if (blind) msg_print("You've got a bad feeling about this...");
				else       msg_format("%^s magically summons legendary opponents!", m_name);
			}
			else
			{
				if (!blind)
					msg_format("%^s gestures imperiously ... and looks puzzled for a moment.",
						m_name);
			}
			break;
		}		
		/* Paranoia */
		default:
		{
			msg_print("A monster tried to cast a spell that has not yet been defined.");
		}
	}

	/* Learn Player Resists */
	if (attack < 128)
	{
		  update_smart_learn(m_idx, spell_desire_RF4[attack-96][D_RES]);
	}
	else if (attack < 160)
	{
		  update_smart_learn(m_idx, spell_desire_RF5[attack-128][D_RES]);
	}
	else if (attack < 192)
	{
		  update_smart_learn(m_idx, spell_desire_RF6[attack-160][D_RES]);
	}
	else if (attack < 224)
	{
		  update_smart_learn(m_idx, spell_desire_RF7[attack-192][D_RES]);
	}

	/* Mark minimum desired range for recalculation */
	m_ptr->min_range = 0;

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Innate spell */
		if (attack < 32*4)
		{
		l_ptr->r_flags4 |= (1L << (attack - 32*3));
		if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
		}

		/* Bolt or Ball */
		else if (attack < 32*5)
		{
		l_ptr->r_flags5 |= (1L << (attack - 32*4));
		if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
		}

		/* Special spell */
		else if (attack < 32*6)
		{
		l_ptr->r_flags6 |= (1L << (attack - 32*5));
		if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
		}

		/* Beam spells or Summon spell */
		else if (attack < 32*7)
		{
		l_ptr->r_flags7 |= (1L << (attack - 32*6));
		if (l_ptr->r_ranged < MAX_UCHAR) l_ptr->r_ranged++;
		}

		/* Remember special flags */
		if (r_ptr->flags2 & (RF2_ARCHER)) l_ptr->r_flags2 |= RF2_ARCHER;
	}

	if (seen && p_ptr->wizard)
		msg_format("%^s has %i mana remaining.", m_name, m_ptr->mana);

	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (l_ptr->r_deaths < MAX_SHORT))
	{
		l_ptr->r_deaths++;
	}


	/* A spell was cast */
	return (TRUE);
}


/*
 * Hack, based on mon_take_hit... perhaps all monster attacks on
 * other monsters should use this? mon_take_hit is in xtra2.c
 */
void mon_take_hit_mon(int m_idx, int dam, cptr note)
{
	monster_type	*m_ptr = &m_list[m_idx];

	monster_race	*r_ptr = &r_info[m_ptr->r_idx];
	
	char m_name[160];

	bool seen = m_ptr->ml;

	/* Can the player be aware of this attack? */
	bool known = (m_ptr->cdis <= MAX_SIGHT);

	/* Extract monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Redraw (later) if needed */
	if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

	/* Wake it up */
	m_ptr->csleep = 0;

	/* Hurt it */
	m_ptr->hp -= dam;

	/* It is dead now... or is it? */
	if (m_ptr->hp < 0)
	{
		if ((r_ptr->flags1 & RF1_UNIQUE) ||
			(r_ptr->flags1 & RF1_QUESTOR))
		{
			m_ptr->hp = 1;
		}
		else
		{
			/* Make a sound */
				sound(SOUND_KILL);
			
			if (known)
			{
				/* Death by special attack */
				if (note)
				{
					msg_format("%^s%s", m_name, note);
				}
				/* Unseen death by normal attack */
				else if (!seen)
				{
					/* Do nothing */
				}
				/* Death by normal attack -- nonliving monster */
				else if (!monster_living(r_ptr))
				{
					msg_format("%^s is destroyed.", m_name);
				}
				/* Death by normal attack -- living monster */
				else
				{
					msg_format("%^s is killed.", m_name);
				}
			}

			/* Generate treasure */
			monster_death(m_idx);

			/* Delete the monster */
			delete_monster_idx(m_idx);

			/* Monster is dead */
			return;
		}
	}

/* NOT USED IN MONSTER TO MONSTER FUNCTION */
#if 0

	/* Mega-Hack -- Pain cancels fear */
	if (m_ptr->monfear && (dam > 0))
	{
		int tmp = randint(dam);

		/* Cure a little fear */
		if (tmp < m_ptr->monfear)
		{
			/* Reduce fear */
			m_ptr->monfear -= tmp;
		}

		/* Cure all the fear */
		else
		{
			/* Cure fear */
			m_ptr->monfear = 0;

			/* No more fear */
			(*fear) = FALSE;
		}
	}

	/* Sometimes a monster gets scared by damage */
	if (!m_ptr->monfear && !(r_ptr->flags3 & RF3_NO_FEAR))
	{
		int		percentage;

		/* Percentage of fully healthy */
		percentage = (100L * m_ptr->hp) / m_ptr->maxhp;

		/*
		* Run (sometimes) if at 10% or less of max hit points,
		* or (usually) when hit for half its current hit points
		*/
		if (((percentage <= 10) && (rand_int(10) < percentage)) ||
			((dam >= m_ptr->hp) && (rand_int(100) < 80)))
		{
			/* Hack -- note fear */
			(*fear) = TRUE;

			/* XXX XXX XXX Hack -- Add some timed fear */
			m_ptr->monfear = (randint(10) +
				(((dam >= m_ptr->hp) && (percentage > 7)) ?
				20 : ((11 - percentage) * 5)));
		}
	}

#endif /* ALLOW_FEAR */

	/* Not dead yet */
	return;
}

/* Monster attacks monster */
bool monst_attack_monst(int m_idx, int t_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_type *t_ptr = &m_list[t_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_race *tr_ptr = &r_info[t_ptr->r_idx];

	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	int             ap_cnt;
	int             ac, rlev, pt;
  int							blow_selection = 0;
  int             blowcount = 0;
	char            m_name[80], t_name[80];
	char            ddesc[80], temp[80];
	bool            blinked = FALSE, touched = FALSE;
	bool			heal_effect = FALSE;
	byte            y_saver = t_ptr->fy;
	byte            x_saver = t_ptr->fx;

	bool see_m = m_ptr->ml;
	bool see_t = t_ptr->ml;
	bool see_either = see_m || see_t;

	/* Can the player be aware of this attack? */
	bool known = (m_ptr->cdis <= MAX_SIGHT) || (t_ptr->cdis <= MAX_SIGHT);

	/* Cannot attack self */
	if (m_idx == t_idx) return FALSE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & RF1_NEVER_BLOW) return FALSE;

	/* Wake it up */
	t_ptr->csleep = 0;

	/* Total armor */
	ac = tr_ptr->ac;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the monster name (or "it") */
	/* This is what happens right before the program crashes. :-/ */
	/* The data for the t_ptr is getting corrupted */
	monster_desc(t_name, t_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	if (!see_either && known)
	{
		msg_print("You hear noise.");
	}

	/* Scan through all the blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
			/* If there is no blow method, break out of the loop */
			if (!r_ptr->blow[ap_cnt].method) break;
				
			/* Increase the count of the number of blows */	
			blowcount++;		
	}
	
	/* Randomly select an attack to use */
	blow_selection = rand_int(blowcount);

	/* paranoia */
	if ((blow_selection < 0) || (blow_selection > 3)) 
	{
		return FALSE;
	}
	/* We don't attack four times to avoid message glut */
	else
	{
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[blow_selection].effect;
		int method = r_ptr->blow[blow_selection].method;
		int d_dice = r_ptr->blow[blow_selection].d_dice;
		int d_side = r_ptr->blow[blow_selection].d_side;

		/* This is redundant */
		/* Stop attacking if the target dies! */
		if (t_ptr->fx != x_saver || t_ptr->fy != y_saver) return FALSE;

		/* This is redundant */
		if (blinked) /* Stop! */
		{
			 return FALSE;
		}

		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:          power = 60; break;
			case RBE_POISON:        power =  5; break;
			case RBE_UN_BONUS:      power = 20; break;
			case RBE_UN_POWER:      power = 15; break;
			case RBE_EAT_GOLD:      power =  5; break;
			case RBE_EAT_ITEM:      power =  5; break;
			case RBE_EAT_FOOD:      power =  5; break;
			case RBE_EAT_LITE:      power =  5; break;
			case RBE_FIRE:			power = 10; break;
			case RBE_ACID:			power =  0; break;
			case RBE_ELEC:			power = 10; break;
			case RBE_COLD:			power = 10; break;
			case RBE_STEAM:			power = 10; break;
			case RBE_NETHER:		power = 10; break;
			case RBE_ETHER:			power = 10; break;
			case RBE_BLIND:         power =  2; break;
			case RBE_CONFUSE:       power = 10; break;
			case RBE_TERRIFY:       power = 10; break;
			case RBE_PARALYZE:      power =  2; break;
			case RBE_LOSE_MUS:      power =  0; break;
			case RBE_LOSE_AGI:      power =  0; break;
			case RBE_LOSE_VIG:      power =  0; break;
			case RBE_LOSE_SCH:      power =  0; break;
			case RBE_LOSE_EGO:      power =  0; break;
			case RBE_LOSE_CHR:      power =  0; break;
			case RBE_LOSE_ALL:      power =  2; break;
			case RBE_SHATTER:       power = 60; break;
			case RBE_EXP_10:        power =  5; break;
			case RBE_EXP_20:        power =  5; break;
			case RBE_EXP_40:        power =  5; break;
			case RBE_EXP_80:        power =  5; break;
			default:				power =  0; break;
		}


		/* Monster hits*/
		if (!effect || check_hit_monst_v_monst(power, rlev, ac))
		{
			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
					act = "hits %s.";
					touched = TRUE;
					break;
				}

				case RBM_TOUCH:
				{
					act = "touches %s.";
					touched = TRUE;
					break;
				}

				case RBM_PUNCH:
				{
					act = "punches %s.";
					touched = TRUE;
					break;
				}

				case RBM_KICK:
				{
					act = "kicks %s.";
					touched = TRUE;
					break;
				}

				case RBM_SLASH:
				{
					act = "slashes %s.";
					touched = TRUE;
					break;
				}
	
				case RBM_PIERCE:
				{
					act = "pierces %s.";
					touched = TRUE;
					break;
				}
				
				case RBM_BLUNT:
				{
					act = "bashes %s.";
					touched = TRUE;
					break;
				}

				case RBM_LURE:
				{
					act = "lures %s.";
					break;
				}
				case RBM_XXX2:
				{
					act = "XXX2s %s.";
					break;
				}
				case RBM_HOWL:
				{
					act = "howls at %s.";
					break;
				}

				case RBM_CLAW:
				{
					act = "claws %s.";
					touched = TRUE;
					break;
				}

				case RBM_KISS:
				{
					act = "kisses %s.";
					touched = TRUE;
					break;
				}

				case RBM_GRAB:
				{
					act = "grabs %s.";
					touched = TRUE;
					break;
				}

				case RBM_BITE:
				{
					act = "bites %s.";
					touched = TRUE;
					break;
				}

				case RBM_STING:
				{
					act = "stings %s.";
					touched = TRUE;
					break;
				}

				case RBM_BUTT:
				{
					act = "butts %s.";
					touched = TRUE;
					break;
				}

				case RBM_CRUSH:
				{
					act = "crushes %s.";
					touched = TRUE;
					break;
				}
				
				case RBM_ENGULF:
				{
					act = "engulfs %s.";
					touched = TRUE;
					break;
				}

				case RBM_CRAWL:
				{
					act = "crawls on %s.";
					touched = TRUE;
					break;
				}

				case RBM_DROOL:
				{
					act = "drools on %s.";
					touched = TRUE;
					break;
				}

				case RBM_SPIT:
				{
					act = "spits on %s.";
					touched = TRUE;
					break;
				}

				case RBM_XXX4:
				{
					act = "XXX4's on %s.";
					touched = TRUE;
					break;
				}

				case RBM_XXX5:
				{
					act = "XXX5's on %s.";
					touched = TRUE;
					break;
				}

				case RBM_GAZE:
				{
					act = "gazes at %s.";
					break;
				}

				case RBM_WAIL:
				{
					act = "wails at %s.";
					break;
				}

				case RBM_SPORE:
				{
					act = "releases spores at %s.";
					break;
				}

				case RBM_XXX6:
				{
					act = "XXX6's on %s.";
					touched = TRUE;
					break;
				}

				case RBM_XXX7:
				{
					act = "XXX7's on %s.";
					touched = TRUE;
					break;
				}

				case RBM_ZAP:
				{
					act = "zaps %s.";
					touched = TRUE;
					break;
				}
				

				case RBM_PECK:
				{
					act = "pecks %s.";
					touched = TRUE;
					break;
				}

				/* Need a better (more general way) to handle speech */
				case RBM_SPEAK:
				{	
					act = desc_speak[rand_int(MAX_DESC_SPEAK)];
					break;
				}

				case RBM_BEG:
				{
					act = "begs %s for money.";
					break;
				}

				case RBM_SEDUCE:
				{
					act = "caresses %s.";
					touched = TRUE;
					break;
				}

				case RBM_XXX10:
				{
					act = "XXX10's on %s.";
					touched = TRUE;
					break;
				}
				default:
				{
					act = "Undefined at %s.";
					touched = FALSE;
					break;
				}
			}

			/* Message */
			if (act && see_either)
			{
				strfmt(temp, act, t_name);
				msg_format("%^s %s", m_name, temp);
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Triple the damage */
			/* this makes pets and monster fighting faster and more effective */
			damage = (3 * damroll(d_dice, d_side));
		
			
			/* Assume no healing effect */
			heal_effect = FALSE;

			pt = GF_HURT;

			/* Apply appropriate damage */
			switch (effect)
			{
			case 0:
				{
					damage = 0;
					pt  = 0;
					break;
				}

			case RBE_HURT:
				{
					damage -= (damage * ((ac < 150) ? ac : 150) / 250);
					if (damage < 1) damage = 1;
					break;
				}

			case RBE_POISON:
				{
					pt = GF_POISON;
					break;
				}

			case RBE_UN_BONUS:
			case RBE_UN_POWER:
				{
					pt = GF_HURT;
					break;
				}

			case RBE_EAT_FOOD:
			case RBE_EAT_LITE:
				{
					pt = damage = 0;
					break;
				}

			case RBE_EAT_ITEM:
			case RBE_EAT_GOLD:
				{
					pt = damage = 0;
					if (randint(2) == 1) blinked = TRUE;
					break;
				}

			case RBE_FIRE:
				{
					pt = GF_FIRE;
					break;
				}

			case RBE_ACID:
				{
					pt = GF_ACID;
					break;
				}

			case RBE_ELEC:
				{
					pt = GF_ELEC;
					break;
				}

			case RBE_COLD:
				{
					pt = GF_ICE;
					break;
				}

			case RBE_STEAM:
				{
					pt = GF_STEAM;
					break;
				}

			case RBE_NETHER:
				{
					pt = GF_NETHER;
					break;
				}

			case RBE_ETHER:
				{
					pt = GF_ETHER;
					break;
				}

			case RBE_BLIND:
				{
					break;
				}

			case RBE_CONFUSE:
				{
					pt = GF_CONFUSION;
					break;
				}

			case RBE_TERRIFY:
				{
					pt = GF_FEAR;
					break;
				}

			case RBE_PARALYZE:
				{
					pt = GF_SLEEP; /* sort of close... */
					break;
				}

			case RBE_LOSE_MUS:
			case RBE_LOSE_AGI:
			case RBE_LOSE_VIG:
			case RBE_LOSE_SCH:
			case RBE_LOSE_EGO:
			case RBE_LOSE_CHR:
			case RBE_LOSE_ALL:
				{
					break;
				}
			case RBE_SHATTER:
				{
					if (damage > 23)
					{
						earthquake(m_ptr->fy, m_ptr->fx, 8);
					}
					break;
				}
			case RBE_EXP_10:
			case RBE_EXP_20:
			case RBE_EXP_40:
			case RBE_EXP_80:
				{
					pt = GF_SPIRIT;
					break;
				}
			default:
				{
					pt = 0;
					break;
				}
			}

			if (pt)
			{
				u32b flg = PROJECT_KILL | PROJECT_STOP;

				/* Sanity Check */
				if (damage < 0) damage = 0;
				
				/* Use the monster hit function if no special effects */
				if (pt == GF_HURT) 
				{
						mon_take_hit_mon(t_idx, damage, NULL);
				}
				/* Do damage if not exploding */				
				else
				{	
						project(m_idx, 0, m_ptr->fy, m_ptr->fx, t_ptr->fy, t_ptr->fx,
						(pt == GF_SLEEP ? r_ptr->level : damage), pt, flg, 0, 0);
				}
				
				if (heal_effect)
				{
					if ((monster_living(tr_ptr)) && (damage > 2))
					{
						bool did_heal = FALSE;

						if (m_ptr->hp < m_ptr->maxhp) did_heal = TRUE;

						/* Heal */
						m_ptr->hp += damroll(4, damage / 6);
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

						/* Special message */
						if (see_m && did_heal)
						{
							msg_format("%^s appears healthier.", m_name);
						}
					}
				}
			}
		}

		/* Monster missed monster */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
				case RBM_HIT:
				case RBM_TOUCH:
				case RBM_PUNCH:
				case RBM_KICK:
				case RBM_SLASH:
				case RBM_PIERCE:
				case RBM_BLUNT:
				case RBM_CLAW:
				case RBM_KISS:
				case RBM_GRAB:
				case RBM_BITE:
				case RBM_STING:
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:
				case RBM_CRAWL:
				case RBM_DROOL:
				case RBM_SPIT:
				case RBM_ZAP:
				case RBM_PECK:
				case RBM_SEDUCE:
				
				{
					/* Visible monsters */
					if (see_m)
					{
						/* Message */
						msg_format("%^s misses %s.", m_name, t_name);
					}

					break;
				}
				default:
				{
					break;
				}
			}
		}


		/* Analyze "visible" monsters only */
		if (see_m)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->r_blows[ap_cnt]++;
				}
			}
		}
	}


	/* Blink away */
	if (blinked)
	{
		if (see_m)
		{
			msg_print("The thief flees laughing!");
		}
		else if (known)
		{
			msg_print("You hear laughter!");
		}

		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}

	return TRUE;
}


/*
 * Some monsters are surrounded by gas, terrible heat, loud noises, spores,
 * etc.  Process any such affects.
 *
 * The Nazgul surround themselves with darkness, so they all have IS_LIT.
 */
void cloud_surround(int r_idx, int *typ, int *dam, int *rad)
{
	monster_race *r_ptr = &r_info[r_idx];

	*typ = 0;
	*dam = randint(r_ptr->level * 8);
	*rad = 1 + r_ptr->level / 20;

	/* Unique monsters have especially strong effects */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		*rad += 1;
		*dam *= 2;
	}

	/*** Determine the kind of cloud we're supposed to be giving off ***/
	/* If breaths and attrs match, the choice is clear. */
	if (r_ptr->flags5)
	{
		if ((r_ptr->flags5 & (RF5_BA_FIRE)) &&
				((r_ptr->d_attr == TERM_RED) ||
				 (r_ptr->d_attr == TERM_L_RED))) *typ = GF_FIRE;
		else if ((r_ptr->flags5 & (RF5_BA_EARTH)) &&
				((r_ptr->d_attr == TERM_UMBER) ||
				 (r_ptr->d_attr == TERM_L_UMBER))) *typ = GF_EARTH;
		else if ((r_ptr->flags5 & (RF5_BA_AIR)) &&
				((r_ptr->d_attr == TERM_WHITE) ||
				 (r_ptr->d_attr == TERM_BLUE) ||
				 (r_ptr->d_attr == TERM_L_BLUE))) *typ = GF_WIND;
		else if ((r_ptr->flags5 & (RF5_BA_WATER)) &&
				((r_ptr->d_attr == TERM_BLUE) ||
				 (r_ptr->d_attr == TERM_L_BLUE))) *typ = GF_STEAM;
		else if ((r_ptr->flags5 & (RF5_BA_ICE)) &&
				((r_ptr->d_attr == TERM_WHITE) ||
				 (r_ptr->d_attr == TERM_BLUE) ||
				 (r_ptr->d_attr == TERM_L_BLUE))) *typ = GF_ICE;
		else if ((r_ptr->flags5 & (RF5_BA_ACID)) &&
				((r_ptr->d_attr == TERM_SLATE) ||
				 (r_ptr->d_attr == TERM_L_DARK))) *typ = GF_ACID;
		else if ((r_ptr->flags5 & (RF5_BA_POISON)) &&
				((r_ptr->d_attr == TERM_GREEN))) *typ = GF_POISON;	
	}
#if 0

	/* Molds release spores */
	if ((!typ) && (r_ptr->d_char == 'm'))
	{
		*typ = GF_SPORE;
		*dam *= 4;
	}

	/* The Nazgul darken everything nearby (so do silver jellies) */
	if ((!typ) && ((r_ptr->d_char == 'W') || (r_ptr->d_char == 'j')))
	{
		*typ = GF_DARK_WEAK;
		*rad = 4;
	}
#endif
	/* Leave the rest blank until we make monsters that need it. */
}

