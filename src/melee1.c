/* File: melee1.c */

/* Purpose: Monster attacks */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, s32b dam)
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
        "mumbles something about mushrooms.",

        /* Mathilde's sentence */
        "giggles at you.",
        "asks you if you want to giggle with her.",
        "says she is always happy."
};


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx, byte divis)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int ap_cnt;

	int i, j, k, tmp, ac, rlev, flg;
	int do_cut, do_stun;
        int mon_att_bonus = 0;

	s32b gold;

	object_type *o_ptr;
	object_type *j_ptr;

	char o_name[80];

	char m_name[80];

	char ddesc[80];
	char ch;


	bool blinked;
	bool touched = FALSE, fear = FALSE, alive = TRUE;
	bool explode = FALSE;

        u32b f1, f2, f3, f4;
	u32b ff1, ff2, ff3, ff4;

        o_ptr = &inventory[INVEN_WIELD];
	j_ptr = &inventory[INVEN_WIELD+1];

        object_flags(o_ptr, &f1, &f2, &f3, &f4);
	object_flags(o_ptr, &ff1, &ff2, &ff3, &ff4);

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* ...nor if friendly */
	if (is_pet(m_ptr))  return FALSE;

        /* If the arms are mutilated...no attacks, right? Right. */
        if (m_ptr->abilities & (MUTILATE_ARMS)) return FALSE;

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x88);

	/* Assume no blink */
	blinked = FALSE;

	/* Attack until we have no more blows! */
	for (i = 0; i < r_ptr->attacks; i++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;
		bool nothurt = FALSE;

		int power = 0;
		int numattacks = 0;
		int chosen = 0;
                s32b damage = 0;

		cptr act = NULL;

		/* Stop if player is dead or gone */
		if (!alive || death) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Make sure the monster is still alive */
		if (!m_ptr) break;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* We can have up to 20!! different attacks. */
		/* Let's choose one! */
		j = 0;
		while (r_ptr->attack[j].type > 0 && numattacks < 20) 
		{
			numattacks++;
			j++;
		}

		/* Now, choose an attack! */
		chosen = randint(numattacks) - 1;

		/* Learn that the monster can do that. */
		r_ptr->r_blows[chosen] = 1;
                
		/* What type of attack did the monster do? */
		switch (r_ptr->attack[chosen].type)
		{
			/* Normal attack */
			case 1:
			{
				/* Monk's grappling throw! */
                		if (p_ptr->abilities[(CLASS_MONK * 10) + 3] >= 1 && unarmed())
                		{
                        		char askstring[100];
                        		sprintf(askstring, "Attempt to grab and throw %s? [y/n]", m_name); 
                        		get_com(askstring, &ch);
                        		if (ch == 'y' || ch == 'Y')
                        		{
                                		if ((p_ptr->stat_ind[A_STR] * 100) >= r_ptr->weight)
                                		{
							int hit = 0;
							call_lua("player_hit_monster", "(Md)", "d", m_ptr, ((p_ptr->abilities[(CLASS_MONK * 10) + 3] - 1) * 10), &hit);
                                        		if (hit == 1)
                                        		{
                                                		monk_throw_counter(m_ptr);
                                                		return (FALSE);
                                        		}
                                        		else
                                        		{
                                                		msg_print("You failed to grab the monster...");
                                                		mon_att_bonus = m_ptr->hitrate * 2;
                                        		}
                                		}
                                		else
                                		{
                                        		msg_print("The monster is too heavy.");
                                        		mon_att_bonus = m_ptr->hitrate * 2;
                                		}
                        		}

                		}

                		/* Zelar's arms crush */
                		if (p_ptr->abilities[(CLASS_ZELAR * 10) + 2] >= 1 && unarmed())
                		{
					int hit;
                        		int bonus = p_ptr->abilities[(CLASS_ZELAR * 10) + 2] * 20;

					call_lua("player_hit_monster", "(Md)", "d", m_ptr, bonus, &hit);
                        		if ((hit == 1) && m_ptr->boss < 1 && !(r_ptr->flags1 & (RF1_UNIQUE)))
                        		{
                                		s32b dam = monk_damages();
						dam += ((dam * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 2] - 1) * 5)) / 100);
                                		msg_print("You grab your foes's arms(or means of attack), and crush them!");
                                		m_ptr->abilities |= (MUTILATE_ARMS);
                                		(void)project(0, 0, m_ptr->fy, m_ptr->fx, dam, GF_PHYSICAL, PROJECT_GRID | PROJECT_KILL);
                                		return (FALSE);
                        		}
                		}

				if (monster_hit_player(m_ptr, mon_att_bonus))
				{
					damage = damroll(r_ptr->attack[chosen].ddice, r_ptr->attack[chosen].dside);
					damage *= (m_ptr->skill_attack + 1);
					damage += ((damage * ((m_ptr->str - 5) * 5)) / 100);
					damage += ((damage * m_ptr->str) / 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;
					
					if (seduction(m_ptr) == TRUE) damage += (damage / 2);
					if (strstr(r_ptr->attack[chosen].name, "!")) msg_format("%^s %s you!", m_name, r_ptr->attack[chosen].act);
					else msg_format("%^s %s you with %s!", m_name, r_ptr->attack[chosen].act, r_ptr->attack[chosen].name);
					
					/* Oh! We have been hit, but the monster was cursed! */
                        		if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
                        		{
                                		m_ptr->hp -= damage * p_ptr->abilities[(CLASS_MAGE * 10) + 6];
                        		}

                        		/* Defender's Iron Skin applies BEFORE resistance */
                        		if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 1] >= 1)
                        		{
                                		damage -= p_ptr->abilities[(CLASS_DEFENDER * 10) + 1] * 100;
                                		if (damage < 0) damage = 0;
                        		}

                        		/* Justice Warrior's protection from evil! */
                        		if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8] >= 1 && r_ptr->flags3 & (RF3_EVIL))
                        		{
                                		int reduction = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8];
                                		if (reduction > 75) reduction = 75;
                                		damage -= ((damage * reduction) / 100);
                                		damage -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8] * 5);
                        		}

                        		/* Physical resistance...and it apply AFTER the Damages Curse! :) */
                        		if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = (damage * p_ptr->pres) / 100;
                                		damage -= damagepercent; 
                        		}

                        		else if (unarmed() && p_ptr->skill[18] >= 40 && !heavy_armor())
                        		{
                                		int blockchance = 0;

                                		/* Block chance is 25%...or is it? */
                                		blockchance = 25 + p_ptr->abilities[(CLASS_ZELAR * 10) + 9];

                                		/* Max is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You avoid the hit!");
							nothurt = TRUE;
						}
                        		}                

                        		else if (polearm_has() && p_ptr->skill[14] >= 25 && !shield_has())
                        		{
                                		int blockchance = 0;

                                		/* Block chance is 20%. */
                                		blockchance = 20;

                                		/* If a Polearm has PARRY, add the bonus! */
                                		if (f4 & (TR4_PARRY)) blockchance = blockchance + (10 + (o_ptr->pval * 2));

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{ 
							msg_print("You parry!");
							nothurt = TRUE;
						}
                        		}                

                        		/* Check for a shield... */
                        		else if (shield_has() || p_ptr->skill[12] >= 10)
                        		{
                                		int blockchance = 0;
                                		o_ptr = &inventory[INVEN_ARM];

                                		if (o_ptr)
                                		{
                                        		/* Calculate the blocking chances */
                                        		blockchance = ((o_ptr->sval * 10) / 2);

                                        		/* Magic Shields are better at blocking... */
                                        		blockchance += (o_ptr->pval * 2);

                                        		/* Defender is the master of shields! */
                                        		if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] >= 1) blockchance += (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] * 2);
                                		}
                                		if (sword_has() && p_ptr->skill[12] >= 10) blockchance += 10;
						
                                		/* Maximum blocking chance is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You block!");
							nothurt = TRUE;
						}
                        		}
					else if (two_weapon_wield() && p_ptr->skill[8] >= 70)
					{
						int blockchance = 20;                                                                 
                                                
						if (f4 & (TR4_PARRY))
						{                                                                     
                                			/* Block chance is 10% + pval * 2. */                                                
                                			blockchance += 10 + (o_ptr->pval * 2);
						}

						/* Two weapons with parry makes it better! */
						if (ff4 & (TR4_PARRY))
						{
							blockchance += 10 + (j_ptr->pval * 2);
						}                                                
                                                                                                                     
                                		/* Maximum blocking chance is 75% */                                                 
                                		if (blockchance > 75) blockchance = 75;                                              
                                                                                                                       
                                		/* Now, try to block */                                                              
                                		if (randint(100) < blockchance)
						{
							msg_print("You dual block!");
							nothurt = TRUE;
						}
					}
                        		/* Last ressort, the PARRY flag! */
                        		else if (f4 & (TR4_PARRY))                                                   
                        		{                                                                                           
                                		int blockchance = 0;                                                                 
                                                                                                                     
                                		/* Block chance is 10% + pval * 2. */                                                
                                		blockchance = 10 + (o_ptr->pval * 2);

						/* Two weapons with parry makes it better! */
						if (ff4 & (TR4_PARRY))
						{
							blockchance += 10 + (j_ptr->pval * 2);
						}                                                
                                                                                                                     
                                		/* Maximum blocking chance is 75% */                                                 
                                		if (blockchance > 75) blockchance = 75;                                              
                                                                                                                       
                                		/* Now, try to block */                                                              
                                		if (randint(100) < blockchance)
						{
							msg_print("You parry!");
							nothurt = TRUE;
						}
                        		}

					if (!nothurt)
					{
						flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                				no_magic_return = TRUE;
						monster_physical = TRUE;
                				(void)project(m_idx, 0, py, px, damage, r_ptr->attack[chosen].element, flg);
						monster_physical = FALSE;
                				no_magic_return = FALSE;
						/* Some Elites/Bosses can cause weird effects on the player... */
                        			if ((m_ptr->abilities & (BOSS_CURSED_HITS)) && randint(100) >= 50 && damage > 0)
                        			{
                                			msg_print("The monster cursed you!");
                                			set_confused(10);
                                			set_afraid(10);
                                			set_blind(10);
                        			}
						/* Some monsters have some special side effects. */
						
						/* Drain stats. */
						if (r_ptr->attack[chosen].special1 >= 1)
						{
							switch (r_ptr->attack[chosen].special1)
							{
								case 1:
								{
									if (!(p_ptr->sustain_str))
									{
										msg_print("Your strength has been reduced!");
										dec_stat(A_STR, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 2:
								{
									if (!(p_ptr->sustain_int))
									{
										msg_print("Your intelligence has been reduced!");
										dec_stat(A_INT, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 3:
								{
									if (!(p_ptr->sustain_wis))
									{
										msg_print("Your wisdom has been reduced!");
										dec_stat(A_WIS, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 4:
								{
									if (!(p_ptr->sustain_dex))
									{
										msg_print("Your dexterity has been reduced!");
										dec_stat(A_DEX, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 5:
								{
									if (!(p_ptr->sustain_con))
									{
										msg_print("Your constitution has been reduced!");
										dec_stat(A_CON, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 6:
								{
									if (!(p_ptr->sustain_chr))
									{
										msg_print("Your charisma has been reduced!");
										dec_stat(A_CHR, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 7:
								{
									if (!(p_ptr->sustain_str))
									{
										msg_print("Your strength has been reduced!");
										dec_stat(A_STR, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									if (!(p_ptr->sustain_int))
									{
										msg_print("Your intelligence has been reduced!");
										dec_stat(A_INT, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									if (!(p_ptr->sustain_wis))
									{
										msg_print("Your wisdom has been reduced!");
										dec_stat(A_WIS, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									if (!(p_ptr->sustain_dex))
									{
										msg_print("Your dexterity has been reduced!");
										dec_stat(A_DEX, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									if (!(p_ptr->sustain_con))
									{
										msg_print("Your constitution has been reduced!");
										dec_stat(A_CON, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									if (!(p_ptr->sustain_chr))
									{
										msg_print("Your charisma has been reduced!");
										dec_stat(A_CHR, r_ptr->attack[chosen].special2, STAT_DEC_NORMAL);
										update_and_handle();
									}
									break;
								}
								case 8:
								{
									if (!(p_ptr->hold_life))
									{
										msg_print("Your experience has been reduced!");
										lose_exp(r_ptr->attack[chosen].special2);
										update_and_handle();
									}
									break;
								}
							}
						}
					}
				}
				else msg_format("%^s misses you.", m_name);

				/* Still alive? We may be able to counter attack! */
				if (p_ptr->chp >= 0)
				{
					if (p_ptr->abilities[(CLASS_WARRIOR * 10) + 7] >= 1) counter_attack(m_ptr);
				}

				/* Applies after everything! */
                		if (p_ptr->elem_shield > 0)
                		{
                        		s32b dam;
					int spellstat;

					spellstat = (p_ptr->stat_ind[A_INT] - 5);
					if (spellstat < 0) spellstat = 0;
                        		dam = (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 2] * 30) * spellstat;
                        		no_magic_return = TRUE;
                        		(void)project(0, 0, m_ptr->fy, m_ptr->fx, dam, p_ptr->elemlord, PROJECT_GRID | PROJECT_KILL);
                        		no_magic_return = FALSE;
                		}
				break;
			}
			/* Animated monster damages */
			case 2:
			{
				/* Monk's grappling throw! */
                		if (p_ptr->abilities[(CLASS_MONK * 10) + 3] >= 1 && unarmed())
                		{
                        		char askstring[100];
                        		sprintf(askstring, "Attempt to grab and throw %s? [y/n]", m_name); 
                        		get_com(askstring, &ch);
                        		if (ch == 'y' || ch == 'Y')
                        		{
                                		if ((p_ptr->stat_ind[A_STR] * 100) >= r_ptr->weight)
                                		{
							int hit;
							call_lua("player_hit_monster", "(Md)", "d", m_ptr, ((p_ptr->abilities[(CLASS_MONK * 10) + 3] - 1) * 10), &hit);
                                        		if (hit == 1)
                                        		{
                                                		monk_throw_counter(m_ptr);
                                                		return (FALSE);
                                        		}
                                        		else
                                        		{
                                                		msg_print("You failed to grab the monster...");
                                                		mon_att_bonus = m_ptr->hitrate * 2;
                                        		}
                                		}
                                		else
                                		{
                                        		msg_print("The monster is too heavy.");
                                        		mon_att_bonus = m_ptr->hitrate * 2;
                                		}
                        		}

                		}

                		/* Zelar's arms crush */
                		if (p_ptr->abilities[(CLASS_ZELAR * 10) + 2] >= 1 && unarmed())
                		{
					int hit;
                        		int bonus = p_ptr->abilities[(CLASS_ZELAR * 10) + 2] * 20;

					call_lua("player_hit_monster", "(Md)", "d", m_ptr, bonus, &hit);
                        		if ((hit == 1) && m_ptr->boss < 1 && !(r_ptr->flags1 & (RF1_UNIQUE)))
                        		{
                                		s32b dam = (p_ptr->to_d / 4) * ((p_ptr->abilities[(CLASS_ZELAR * 10) + 2] / 2)+1);
                                		msg_print("You grab your foes's arms(or means of attack), and crush them!");
                                		m_ptr->abilities |= (MUTILATE_ARMS);
                                		(void)project(0, 0, m_ptr->fy, m_ptr->fx, dam, GF_PHYSICAL, PROJECT_GRID | PROJECT_KILL);
                                		return (FALSE);
                        		}
                		}
				if (monster_hit_player(m_ptr, mon_att_bonus))
				{
					damage = damroll(m_ptr->animdam_d, m_ptr->animdam_s);
					damage *= (m_ptr->skill_attack + 1);
					damage += ((damage * ((m_ptr->str - 5) * 5)) / 100);
					damage += ((damage * m_ptr->str) / 100);
					/* Bosses may get higher damages! */
					if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) damage *= 2;
                        		if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) damage -= damage / 2;
                        		else if (m_ptr->abilities & (CURSE_LOWER_POWER)) damage -= damage / 4;

					if (seduction(m_ptr) == TRUE) damage += (damage / 2);
					
					/* Oh! We have been hit, but the monster was cursed! */
                        		if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
                        		{
                                		m_ptr->hp -= damage * p_ptr->abilities[(CLASS_MAGE * 10) + 6];
                        		}

                        		/* Defender's Iron Skin applies BEFORE resistance */
                        		if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 1] >= 1)
                        		{
                                		damage -= p_ptr->abilities[(CLASS_DEFENDER * 10) + 1] * 100;
                                		if (damage < 0) damage = 0;
                        		}

                        		/* Justice Warrior's protection from evil! */
                        		if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8] >= 1 && r_ptr->flags3 & (RF3_EVIL))
                        		{
                                		int reduction = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8];
                                		if (reduction > 75) reduction = 75;
                                		damage -= ((damage * reduction) / 100);
                                		damage -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 8] * 5);
                        		}

                        		/* Physical resistance...and it apply AFTER the Damages Curse! :) */
                        		if (p_ptr->pres_dur > 0)
                        		{
                                		s32b damagepercent;
                                		damagepercent = (damage * p_ptr->pres) / 100;
                                		damage -= damagepercent; 
                        		}

                        		else if (unarmed() && p_ptr->skill[18] >= 40 && !heavy_armor())
                        		{
                                		int blockchance = 0;

                                		/* Block chance is 25%...or is it? */
                                		blockchance = 25 + p_ptr->abilities[(CLASS_ZELAR * 10) + 9];

                                		/* Max is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You avoid the hit!");
							nothurt = TRUE;
						}
                        		}                

                        		else if (polearm_has() && p_ptr->skill[14] >= 25 && !shield_has())
                        		{
                                		int blockchance = 0;

                                		/* Block chance is 20%. */
                                		blockchance = 20;

                                		/* If a Polearm has PARRY, add the bonus! */
                                		if (f4 & (TR4_PARRY)) blockchance = blockchance + (10 + (o_ptr->pval * 2));

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{ 
							msg_print("You parry!");
							nothurt = TRUE;
						}
                        		}                

                        		/* Check for a shield... */
                        		else if (shield_has() || p_ptr->skill[12] >= 10)
                        		{
                                		int blockchance = 0;
                                		o_ptr = &inventory[INVEN_ARM];

                                		if (o_ptr)
                                		{
                                        		/* Calculate the blocking chances */
                                        		blockchance = ((o_ptr->sval * 10) / 2);

                                        		/* Magic Shields are better at blocking... */
                                        		blockchance += (o_ptr->pval * 2);

                                        		/* Defender is the master of shields! */
                                        		if (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] >= 1) blockchance += (p_ptr->abilities[(CLASS_DEFENDER * 10) + 3] * 2);
                                		}
                                		if (sword_has() && p_ptr->skill[12] >= 10) blockchance += 10;
						
                                		/* Maximum blocking chance is 75% */
                                		if (blockchance > 75) blockchance = 75;

                                		/* Now, try to block */
                                		if (randint(100) < blockchance)
						{
							msg_print("You block!");
							nothurt = TRUE;
						}
                        		}
                        		else if (two_weapon_wield() && p_ptr->skill[8] >= 70)
					{
						int blockchance = 20;                                                                 
                                                
						if (f4 & (TR4_PARRY))
						{                                                                     
                                			/* Block chance is 10% + pval * 2. */                                                
                                			blockchance += 10 + (o_ptr->pval * 2);
						}

						/* Two weapons with parry makes it better! */
						if (ff4 & (TR4_PARRY))
						{
							blockchance += 10 + (j_ptr->pval * 2);
						}                                                
                                                                                                                     
                                		/* Maximum blocking chance is 75% */                                                 
                                		if (blockchance > 75) blockchance = 75;                                              
                                                                                                                       
                                		/* Now, try to block */                                                              
                                		if (randint(100) < blockchance)
						{
							msg_print("You dual block!");
							nothurt = TRUE;
						}
					}
                        		/* Last ressort, the PARRY flag! */
                        		else if (f4 & (TR4_PARRY))                                                   
                        		{                                                                                           
                                		int blockchance = 0;                                                                 
                                                                                                                     
                                		/* Block chance is 10% + pval * 2. */                                                
                                		blockchance = 10 + (o_ptr->pval * 2);

						/* Two weapons with parry makes it better! */
						if (ff4 & (TR4_PARRY))
						{
							blockchance += 10 + (j_ptr->pval * 2);
						}                                                
                                                                                                                     
                                		/* Maximum blocking chance is 75% */                                                 
                                		if (blockchance > 75) blockchance = 75;                                              
                                                                                                                       
                                		/* Now, try to block */                                                              
                                		if (randint(100) < blockchance)
						{
							msg_print("You parry!");
							nothurt = TRUE;
						}
                        		}

					if (!nothurt)
					{
						msg_print("You are hit!");
						flg = PROJECT_JUMP | PROJECT_GRID | PROJECT_KILL;
                				no_magic_return = TRUE;
						monster_physical = TRUE;
                				(void)project(m_idx, 0, py, px, damage, r_ptr->attack[chosen].element, flg);
						monster_physical = FALSE;
                				no_magic_return = FALSE;
						/* Some Elites/Bosses can cause weird effects on the player... */
                        			if ((m_ptr->abilities & (BOSS_CURSED_HITS)) && randint(100) >= 50 && damage > 0)
                        			{
                                			msg_print("The monster cursed you!");
                                			set_confused(10);
                                			set_afraid(10);
                                			set_blind(10);
                        			}
					}
				}
				else msg_format("%^s misses you.", m_name);
				/* Still alive? We may be able to counter attack! */
				if (p_ptr->chp >= 0)
				{
					if (p_ptr->abilities[(CLASS_WARRIOR * 10) + 7] >= 1) counter_attack(m_ptr);
				}

				/* Applies after everything! */
                		if (p_ptr->elem_shield > 0)
                		{
                        		s32b dam;
					int spellstat;

					spellstat = (p_ptr->stat_ind[A_INT] - 5);
					if (spellstat < 0) spellstat = 0;
                        		dam = (p_ptr->abilities[(CLASS_ELEM_LORD * 10) + 2] * 30) * spellstat;
                        		no_magic_return = TRUE;
                        		(void)project(0, 0, m_ptr->fy, m_ptr->fx, dam, p_ptr->elemlord, PROJECT_GRID | PROJECT_KILL);
                        		no_magic_return = FALSE;
                		}
				break;
			}
			case 999:
			{
				call_lua(r_ptr->attack[chosen].name, "(d)", "", m_idx);
				break;
			}
		}        
	}                                          

	/* Always notice cause of death */
	if (death && (r_ptr->r_deaths < MAX_SHORT))
	{
		r_ptr->r_deaths++;
	}

	/* Assume we attacked */
	return (TRUE);
}

/* Check if the player has a shield. */
/* You can only block with Shields, not arm bands... */
bool shield_has()
{
        object_type *o_ptr;
        o_ptr = &inventory[INVEN_ARM];

        if (o_ptr->tval == TV_SHIELD) return (TRUE);
        else return (FALSE);
}

/* Check if the player has a sword. */
bool sword_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_SWORD && o_ptr->tval != TV_SWORD_DEVASTATION) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_SWORD) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a hafted weapon. */
bool hafted_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if ((o_ptr->tval != TV_HAFTED) && (o_ptr->tval != TV_MSTAFF)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if ((o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_MSTAFF)) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a polearm. */
bool polearm_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_POLEARM) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_POLEARM) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a rod. */
bool rod_has()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_ROD) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_ROD) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

/* Check if the player has a weapon equipped. */
bool unarmed()
{
        object_type *o_ptr;
        o_ptr = &inventory[INVEN_WIELD];

        if (o_ptr->tval == 0)
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == 0) return (TRUE);
	}

        return (FALSE);
}

/* Check if the player has a heavy armor. */
bool heavy_armor()
{
        object_type *o_ptr;
        o_ptr = &inventory[INVEN_BODY];

        if (o_ptr->tval != TV_HARD_ARMOR && o_ptr->tval != TV_DRAG_ARMOR) return (TRUE);
        else return (FALSE);
}