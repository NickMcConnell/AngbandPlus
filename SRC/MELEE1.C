/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
 * Hit the player with a physical attack
 */


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

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

	/* Get the "died from" information (i.e. "a kobold") */
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

                /* Skip 'tricky' attacks */
                if ((method == RBM_SHOOT) ||
                       (method == RBM_TRAP) ||
                       (method == RBM_AURA)) continue;


		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

        /* Extract visibility from carrying lite */
        if (r_ptr->flags2 & RF2_HAS_LITE) visible = TRUE;

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
					r_ptr->r_flags3 |= (RF3_EVIL);
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

				case RBM_XXX1:
				{
					act = "XXX1's you.";
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

			}

			/* Message */
			if (act) msg_format("%^s %s", m_name, act);


			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);

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
				case RBM_XXX1:
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
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
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
	if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}


