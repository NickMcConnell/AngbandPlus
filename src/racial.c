/* File: racial.c */

/* Purpose: Racial powers (and mutations) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Returns the chance to activate a racial power/mutation
 */
static int racial_chance(s16b min_level, int use_stat, int difficulty)
{
	int i;
	int val;
	int sum = 0;
	int stat = p_ptr->stat_cur[use_stat];

	/* No chance for success */
	if ((p_ptr->lev < min_level) || p_ptr->confused)
	{
		return (0);
	}

	/* Calculate difficulty */
	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty == 0)
		return (100);
	else
		return (((sum * 100) / difficulty) / stat);
}


/* Note: return value indicates that we have succesfully used the power */

bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
	bool use_hp = FALSE;

	/* Not enough mana - use hp */
	if (p_ptr->csp < cost) use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->lev < min_level)
	{
		msg_format("You need to attain level %d to use this power.", min_level);
		p_ptr->energy_use = 0;
		return FALSE;
	}

	/* Too confused */
	else if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		p_ptr->energy_use = 0;
		return FALSE;
	}

	/* Risk death? */
	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!get_check("Really use the power in your weakened state? "))
		{
			p_ptr->energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->stun)
	{
		difficulty += p_ptr->stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	p_ptr->energy_use = 100;

	if (use_hp)
	{
		take_hit((cost / 2) + randint1(cost / 2),
			"concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2) + randint1(cost / 2);
	}


	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);

	/* Success? */
	if (randint1(p_ptr->stat_cur[use_stat]) >=
	    ((difficulty / 2) + randint1(difficulty / 2)))
	{
		return TRUE;
	}

	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}


static void cmd_racial_power_aux(s32b command)
{
	s16b        plev = p_ptr->lev;
	int         dir = 0;


	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			if (racial_aux(5, 5, A_WIS, 12))
			{
				msg_print("You examine your surroundings.");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_HOBBIT:
			if (racial_aux(15, 10, A_INT, 10))
			{
				object_type *q_ptr;
				object_type forge;

				/* Get local object */
				q_ptr = &forge;

				/* Create the food ration */
				object_prep(q_ptr, 21);

#ifdef USE_SCRIPT
				q_ptr->python = object_create_callback(q_ptr);
#endif /* USE_SCRIPT */

				/* Drop the object from heaven */
				(void)drop_near(q_ptr, -1, p_ptr->py, p_ptr->px);
				msg_print("You cook some food.");
			}
			break;

		case RACE_GNOME:
			if (racial_aux(5, (5 + (plev / 5)), A_INT, 12))
			{
				msg_print("Blink!");
				teleport_player(10 + plev);
			}
			break;

		case RACE_HALF_ORC:
			if (racial_aux(3, 5, A_WIS,
			    (p_ptr->pclass == CLASS_WARRIOR ? 5 : 10)))
			{
				msg_print("You play tough.");
				(void)set_afraid(0);
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
			    (p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("RAAAGH!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint1(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_AMBERITE:
			if (command == -2)
			{
				if (racial_aux(40, 75, A_WIS, 50))
				{
					msg_print("You picture the Pattern in your mind and walk it...");
					(void)set_poisoned(0);
					(void)set_image(0);
					(void)set_stun(0);
					(void)set_cut(0);
					(void)set_blind(0);
					(void)set_afraid(0);
					(void)do_res_stat(A_STR);
					(void)do_res_stat(A_INT);
					(void)do_res_stat(A_WIS);
					(void)do_res_stat(A_DEX);
					(void)do_res_stat(A_CON);
					(void)do_res_stat(A_CHR);
					(void)restore_level();
				}
			}

			else if (command == -1)
			{
				if (racial_aux(30, 50, A_INT, 50))
				{
					/* No effect in arena or quest */
					if (p_ptr->inside_quest)
					{
						msg_print("There is no effect.");
					}
					else
					{
						msg_print("You start walking around. Your surroundings change.");

						if (autosave_l) do_cmd_save_game(TRUE);

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS, (p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("Raaagh!");
				(void)set_afraid(0);

				(void)set_shero(p_ptr->shero + 10 + randint1(plev));
				(void)hp_player(30);
			}
			break;

		case RACE_HALF_OGRE:
			if (racial_aux(25, 35, A_INT, 15))
			{
				msg_print("You carefully set an explosive rune...");
				explosive_rune();
			}
			break;

		case RACE_HALF_GIANT:
			if (racial_aux(20, 10, A_STR, 12))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You bash at a stone wall.");
				(void)wall_to_mud(dir);
			}
			break;

		case RACE_HALF_TITAN:
			if (racial_aux(35, 20, A_INT, 12))
			{
				msg_print("You examine your foes...");
				probing();
			}
			break;

		case RACE_CYCLOPS:
			if (racial_aux(20, 15, A_STR, 12))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You throw a huge boulder.");
				fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
			}
			break;

		case RACE_YEEK:
			if (racial_aux(15, 15, A_WIS, 10))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You make a horrible scream!");
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You spit acid.");
				if (plev < 25)
					fire_bolt(GF_ACID, dir, plev);
				else
					fire_ball(GF_ACID, dir, plev, 2);
			}
			break;

		case RACE_KOBOLD:
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev);
			}
			break;

		case RACE_NIBELUNG:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You examine your surroundings.");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}
			break;

		case RACE_DARK_ELF:
			if (racial_aux(2, 2, A_INT, 9))
			{
				if (!get_aim_dir(&dir)) break;
				msg_print("You cast a magic missile.");
				fire_bolt_or_beam(10, GF_MISSILE, dir,
				    damroll(3 + ((plev - 1) / 5), 4));
			}
			break;

		case RACE_DRACONIAN:
			if (racial_aux(1, plev, A_CON, 12))
			{
				int  Type = ((randint1(3) == 1) ? GF_COLD : GF_FIRE);
				cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");

				if (randint1(100) < plev)
				{
					switch (p_ptr->pclass)
					{
						case CLASS_WARRIOR:
						case CLASS_RANGER:
							if (randint1(3) == 1)
							{
								Type = GF_MISSILE;
								Type_desc = "the elements";
							}
							else
							{
								Type = GF_SHARDS;
								Type_desc = "shards";
							}
							break;
						case CLASS_MAGE:
						case CLASS_WARRIOR_MAGE:
						case CLASS_HIGH_MAGE:
							if (randint1(3) == 1)
							{
								Type = GF_MANA;
								Type_desc = "mana";
							}
							else
							{
								Type = GF_DISENCHANT;
								Type_desc = "disenchantment";
							}
							break;
						case CLASS_CHAOS_WARRIOR:
							if (randint1(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_CHAOS;
								Type_desc = "chaos";
							}
							break;
						case CLASS_MONK:
							if (randint1(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_SOUND;
								Type_desc = "sound";
							}
							break;
						case CLASS_MINDCRAFTER:
							if (randint1(3) != 1)
							{
								Type = GF_CONFUSION;
								Type_desc = "confusion";
							}
							else
							{
								Type = GF_PSI;
								Type_desc = "mental energy";
							}
							break;
						case CLASS_PRIEST:
						case CLASS_PALADIN:
							if (randint1(3) == 1)
							{
								Type = GF_HELL_FIRE;
								Type_desc = "hellfire";
							}
							else
							{
								Type = GF_HOLY_FIRE;
								Type_desc = "holy fire";
							}
							break;
						case CLASS_ROGUE:
							if (randint1(3) == 1)
							{
								Type = GF_DARK;
								Type_desc = "darkness";
							}
							else
							{
								Type = GF_POIS;
								Type_desc = "poison";
							}
							break;
					}
				}

				if (!get_aim_dir(&dir)) break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, plev * 2,
				    (plev / 15) + 1);
			}
			break;

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msg_print("You concentrate and your eyes glow red...");
					fire_bolt(GF_PSI, dir, plev);
				}
			}
			break;

		case RACE_IMP:
			if (racial_aux(9, 15, A_WIS, 15))
			{
				if (!get_aim_dir(&dir)) break;
				if (plev >= 30)
				{
					msg_print("You cast a ball of fire.");
					fire_ball(GF_FIRE, dir, plev, 2);
				}
				else
				{
					msg_print("You cast a bolt of fire.");
					fire_bolt(GF_FIRE, dir, plev);
				}
			}
			break;

		case RACE_GOLEM:
			if (racial_aux(20, 15, A_CON, 8))
			{
				(void)set_shield(p_ptr->shield + randint1(20) + 30);
			}
			break;

		case RACE_SKELETON:
		case RACE_ZOMBIE:
			if (racial_aux(30, 30, A_WIS, 18))
			{
				msg_print("You attempt to restore your lost energies.");
				(void)restore_level();
			}
			break;

		case RACE_VAMPIRE:
			if (racial_aux(2, (1 + (plev / 3)), A_CON, 9))
			{
				int y, x, dummy;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir,FALSE)) break;   /* was get_aim_dir */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				/* Paranoia */
				if (!in_bounds2(y, x)) break;

				c_ptr = area(y, x);

				if (!c_ptr->m_idx)
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + randint1(plev) * MAX(1, plev / 10);   /* Dmg */
				if (drain_gain_life(dir, dummy))
				{
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
						(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case RACE_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir)) break;
				(void)fear_monster(dir, plev);
			}
			break;

		case RACE_SPRITE:
			if (racial_aux(12, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (plev < 25)
					sleep_monsters_touch();
				else
					(void)sleep_monsters();
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			p_ptr->energy_use = 0;
	}

	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);
}


typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
	char name[40];
	int  level;
	int  cost;
	int  fail;
	int  number;
};


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	power_desc_type power_desc[36];
	int             num, ask, i = 0;
	int             lvl = p_ptr->lev;
	bool            flag, redraw;
	bool            warrior = ((p_ptr->pclass == CLASS_WARRIOR) ? TRUE : FALSE);
	bool            has_racial = FALSE;
	char            choice;
	char            out_val[160];


	for (num = 0; num < 36; num++)
	{
		strcpy(power_desc[num].name, "");
		power_desc[num].number = 0;
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		p_ptr->energy_use = 0;
		return;
	}

	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			strcpy(power_desc[0].name, "detect doors+traps");
			power_desc[0].level = 5;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(5, A_WIS, 12);
			has_racial = TRUE;
			break;
		case RACE_NIBELUNG:
			strcpy(power_desc[0].name, "detect doors+traps");
			power_desc[0].level = 10;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(10, A_WIS, 10);
			has_racial = TRUE;
			break;
		case RACE_HOBBIT:
			strcpy(power_desc[0].name, "create food");
			power_desc[0].level = 15;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(15, A_INT, 10);
			has_racial = TRUE;
			break;
		case RACE_GNOME:
			sprintf(power_desc[0].name, "teleport (range %d)", 10 + lvl);
			power_desc[0].level = 5;
			power_desc[0].cost = 5 + (lvl / 5);
			power_desc[0].fail = 100 - racial_chance(5, A_INT, 12);
			has_racial = TRUE;
			break;
		case RACE_HALF_ORC:
			strcpy(power_desc[0].name, "remove fear");
			power_desc[0].level = 3;
			power_desc[0].cost = 5;
			power_desc[0].fail = 100 - racial_chance(3, A_WIS, (warrior ? 5 : 10));
			has_racial = TRUE;
			break;
		case RACE_HALF_TROLL:
			strcpy(power_desc[0].name, "berserk");
			power_desc[0].level = 10;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(10, A_WIS, (warrior ? 6 : 12));
			has_racial = TRUE;
			break;
		case RACE_BARBARIAN:
			strcpy(power_desc[0].name, "berserk");
			power_desc[0].level = 8;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(8, A_WIS, (warrior ? 6 : 12));
			has_racial = TRUE;
			break;
		case RACE_AMBERITE:
			strcpy(power_desc[0].name, "Shadow Shifting");
			power_desc[0].level = 30;
			power_desc[0].cost = 50;
			power_desc[0].fail = 100 - racial_chance(30, A_INT, 50);
			strcpy(power_desc[1].name, "Pattern Mindwalking");
			power_desc[1].level = 40;
			power_desc[1].cost = 75;
			power_desc[1].fail = 100 - racial_chance(40, A_WIS, 50);
			power_desc[1].number = -2;
			num++;
			has_racial = TRUE;
			break;
		case RACE_HALF_OGRE:
			strcpy(power_desc[0].name, "explosive rune");
			power_desc[0].level = 25;
			power_desc[0].cost = 35;
			power_desc[0].fail = 100 - racial_chance(25, A_INT, 15);
			has_racial = TRUE;
			break;
		case RACE_HALF_GIANT:
			strcpy(power_desc[0].name, "stone to mud");
			power_desc[0].level = 20;
			power_desc[0].cost = 10;
			power_desc[0].fail = 100 - racial_chance(20, A_STR, 12);
			has_racial = TRUE;
			break;
		case RACE_HALF_TITAN:
			strcpy(power_desc[0].name, "probing");
			power_desc[0].level = 35;
			power_desc[0].cost = 20;
			power_desc[0].fail = 100 - racial_chance(35, A_INT, 12);
			has_racial = TRUE;
			break;
		case RACE_CYCLOPS:
			sprintf(power_desc[0].name, "throw boulder (dam %d)", (3 * lvl) / 2);
			power_desc[0].level = 20;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(20, A_STR, 12);
			has_racial = TRUE;
			break;
		case RACE_YEEK:
			strcpy(power_desc[0].name, "scare monster");
			power_desc[0].level = 15;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(15, A_WIS, 10);
			has_racial = TRUE;
			break;
		case RACE_SPECTRE:
			strcpy(power_desc[0].name, "scare monster");
			power_desc[0].level = 4;
			power_desc[0].cost = 6;
			power_desc[0].fail = 100 - racial_chance(4, A_INT, 3);
			has_racial = TRUE;
			break;
		case RACE_KLACKON:
			sprintf(power_desc[0].name, "spit acid (dam %d)", lvl);
			power_desc[0].level = 9;
			power_desc[0].cost = 9;
			power_desc[0].fail = 100 - racial_chance(9, A_DEX, 14);
			has_racial = TRUE;
			break;
		case RACE_KOBOLD:
			sprintf(power_desc[0].name, "poison dart (dam %d)", lvl);
			power_desc[0].level = 12;
			power_desc[0].cost = 8;
			power_desc[0].fail = 100 - racial_chance(12, A_DEX, 14);
			has_racial = TRUE;
			break;
		case RACE_DARK_ELF:
			sprintf(power_desc[0].name, "magic missile (dm %dd%d)", 3 + ((lvl - 1) / 5), 4);
			power_desc[0].level = 2;
			power_desc[0].cost = 2;
			power_desc[0].fail = 100 - racial_chance(2, A_INT, 9);
			has_racial = TRUE;
			break;
		case RACE_DRACONIAN:
			sprintf(power_desc[0].name, "breath weapon (dam %d)", lvl * 2);
			power_desc[0].level = 1;
			power_desc[0].cost = lvl;
			power_desc[0].fail = 100 - racial_chance(1, A_CON, 12);
			has_racial = TRUE;
			break;
		case RACE_MIND_FLAYER:
			sprintf(power_desc[0].name, "mind blast (dam %d)", lvl);
			power_desc[0].level = 15;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(15, A_INT, 14);
			has_racial = TRUE;
			break;
		case RACE_IMP:
			sprintf(power_desc[0].name, "fire bolt/ball (dam %d)", lvl);
			power_desc[0].level = 9;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(9, A_WIS, 15);
			has_racial = TRUE;
			break;
		case RACE_GOLEM:
			strcpy(power_desc[0].name, "stone skin (dur 1d20+30)");
			power_desc[0].level = 20;
			power_desc[0].cost = 15;
			power_desc[0].fail = 100 - racial_chance(20, A_CON, 8);
			has_racial = TRUE;
			break;
		case RACE_SKELETON:
		case RACE_ZOMBIE:
			strcpy(power_desc[0].name, "restore life");
			power_desc[0].level = 30;
			power_desc[0].cost = 30;
			power_desc[0].fail = 100 - racial_chance(30, A_WIS, 18);
			has_racial = TRUE;
			break;
		case RACE_VAMPIRE:
			strcpy(power_desc[0].name, "drain life");
			power_desc[0].level = 2;
			power_desc[0].cost = 1 + (lvl / 3);
			power_desc[0].fail = 100 - racial_chance(2, A_CON, 9);
			has_racial = TRUE;
			break;
		case RACE_SPRITE:
			strcpy(power_desc[0].name, "sleeping dust");
			power_desc[0].level = 12;
			power_desc[0].cost = 12;
			power_desc[0].fail = 100 - racial_chance(12, A_INT, 15);
			has_racial = TRUE;
			break;
		default:
			strcpy(power_desc[0].name, "(none)");
	}

	if (!has_racial && !p_ptr->muta1)
	{
		msg_print("You have no powers to activate.");
		p_ptr->energy_use = 0;
		return;
	}

	if (has_racial)
	{
		power_desc[0].number = -1;
		num++;
	}

	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
			strcpy(power_desc[num].name, "spit acid");
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_DEX, 15);
			power_desc[num++].number = MUT1_SPIT_ACID;
		}

		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
			strcpy(power_desc[num].name, "fire breath");
			power_desc[num].level = 20;
			power_desc[num].cost = lvl;
			power_desc[num].fail = 100 - racial_chance(20, A_CON, 18);
			power_desc[num++].number = MUT1_BR_FIRE;
		}

		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
			strcpy(power_desc[num].name, "hypnotic gaze");
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_CHR, 18);
			power_desc[num++].number = MUT1_HYPN_GAZE;
		}

		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
			strcpy(power_desc[num].name, "telekinesis");
			power_desc[num].level = 9;
			power_desc[num].cost = 9;
			power_desc[num].fail = 100 - racial_chance(9, A_WIS, 14);
			power_desc[num++].number = MUT1_TELEKINES;
		}

		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
			strcpy(power_desc[num].name, "teleport");
			power_desc[num].level = 7;
			power_desc[num].cost = 7;
			power_desc[num].fail = 100 - racial_chance(7, A_WIS, 15);
			power_desc[num++].number = MUT1_VTELEPORT;
		}

		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
			strcpy(power_desc[num].name, "mind blast");
			power_desc[num].level = 5;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(5, A_WIS, 15);
			power_desc[num++].number = MUT1_MIND_BLST;
		}

		if (p_ptr->muta1 & MUT1_RADIATION)
		{
			strcpy(power_desc[num].name, "emit radiation");
			power_desc[num].level = 15;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(15, A_CON, 14);
			power_desc[num++].number = MUT1_RADIATION;
		}

		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
			strcpy(power_desc[num].name, "vampiric drain");
			power_desc[num].level = 2;
			power_desc[num].cost = (1 + (lvl / 3));
			power_desc[num].fail = 100 - racial_chance(2, A_CON, 9);
			power_desc[num++].number = MUT1_VAMPIRISM;
		}

		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
			strcpy(power_desc[num].name, "smell metal");
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_INT, 12);
			power_desc[num++].number = MUT1_SMELL_MET;
		}

		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
			strcpy(power_desc[num].name, "smell monsters");
			power_desc[num].level = 5;
			power_desc[num].cost = 4;
			power_desc[num].fail = 100 - racial_chance(5, A_INT, 15);
			power_desc[num++].number = MUT1_SMELL_MON;
		}

		if (p_ptr->muta1 & MUT1_BLINK)
		{
			strcpy(power_desc[num].name, "blink");
			power_desc[num].level = 3;
			power_desc[num].cost = 3;
			power_desc[num].fail = 100 - racial_chance(3, A_WIS, 12);
			power_desc[num++].number = MUT1_BLINK;
		}

		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
			strcpy(power_desc[num].name, "eat rock");
			power_desc[num].level = 8;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(8, A_CON, 18);
			power_desc[num++].number = MUT1_EAT_ROCK;
		}

		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
			strcpy(power_desc[num].name, "swap position");
			power_desc[num].level = 15;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(15, A_DEX, 16);
			power_desc[num++].number = MUT1_SWAP_POS;
		}

		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
			strcpy(power_desc[num].name, "shriek");
			power_desc[num].level = 20;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(20, A_CON, 16);
			power_desc[num++].number = MUT1_SHRIEK;
		}

		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
			strcpy(power_desc[num].name, "illuminate");
			power_desc[num].level = 3;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(3, A_INT, 10);
			power_desc[num++].number = MUT1_ILLUMINE;
		}

		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
			strcpy(power_desc[num].name, "detect curses");
			power_desc[num].level = 7;
			power_desc[num].cost = 14;
			power_desc[num].fail = 100 - racial_chance(7, A_WIS, 14);
			power_desc[num++].number = MUT1_DET_CURSE;
		}

		if (p_ptr->muta1 & MUT1_BERSERK)
		{
			strcpy(power_desc[num].name, "berserk");
			power_desc[num].level = 8;
			power_desc[num].cost = 8;
			power_desc[num].fail = 100 - racial_chance(8, A_STR, 14);
			power_desc[num++].number = MUT1_BERSERK;
		}

		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
			strcpy(power_desc[num].name, "polymorph");
			power_desc[num].level = 18;
			power_desc[num].cost = 20;
			power_desc[num].fail = 100 - racial_chance(18, A_CON, 18);
			power_desc[num++].number = MUT1_POLYMORPH;
		}

		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
			strcpy(power_desc[num].name, "midas touch");
			power_desc[num].level = 10;
			power_desc[num].cost = 5;
			power_desc[num].fail = 100 - racial_chance(10, A_INT, 12);
			power_desc[num++].number = MUT1_MIDAS_TCH;
		}

		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
			strcpy(power_desc[num].name, "grow mold");
			power_desc[num].level = 1;
			power_desc[num].cost = 6;
			power_desc[num].fail = 100 - racial_chance(1, A_CON, 14);
			power_desc[num++].number = MUT1_GROW_MOLD;
		}

		if (p_ptr->muta1 & MUT1_RESIST)
		{
			strcpy(power_desc[num].name, "resist elements");
			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(10, A_CON, 12);
			power_desc[num++].number = MUT1_RESIST;
		}

		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
			strcpy(power_desc[num].name, "earthquake");
			power_desc[num].level = 12;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(12, A_STR, 16);
			power_desc[num++].number = MUT1_EARTHQUAKE;
		}

		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
			strcpy(power_desc[num].name, "eat magic");
			power_desc[num].level = 17;
			power_desc[num].cost = 1;
			power_desc[num].fail = 100 - racial_chance(17, A_WIS, 15);
			power_desc[num++].number = MUT1_EAT_MAGIC;
		}

		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
			strcpy(power_desc[num].name, "weigh magic");
			power_desc[num].level = 6;
			power_desc[num].cost = 6;
			power_desc[num].fail = 100 - racial_chance(6, A_INT, 10);
			power_desc[num++].number = MUT1_WEIGH_MAG;
		}

		if (p_ptr->muta1 & MUT1_STERILITY)
		{
			strcpy(power_desc[num].name, "sterilize");
			power_desc[num].level = 12;
			power_desc[num].cost = 23;
			power_desc[num].fail = 100 - racial_chance(12, A_CHR, 15);
			power_desc[num++].number = MUT1_STERILITY;
		}

		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
			strcpy(power_desc[num].name, "panic hit");
			power_desc[num].level = 10;
			power_desc[num].cost = 12;
			power_desc[num].fail = 100 - racial_chance(10, A_DEX, 14);
			power_desc[num++].number = MUT1_PANIC_HIT;
		}

		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
			strcpy(power_desc[num].name, "dazzle");
			power_desc[num].level = 7;
			power_desc[num].cost = 15;
			power_desc[num].fail = 100 - racial_chance(7, A_CHR, 8);
			power_desc[num++].number = MUT1_DAZZLE;
		}

		if (p_ptr->muta1 & MUT1_LASER_EYE)
		{
			strcpy(power_desc[num].name, "laser eye");
			power_desc[num].level = 7;
			power_desc[num].cost = 10;
			power_desc[num].fail = 100 - racial_chance(7, A_WIS, 9);
			power_desc[num++].number = MUT1_LASER_EYE;
		}

		if (p_ptr->muta1 & MUT1_RECALL)
		{
			strcpy(power_desc[num].name, "recall");
			power_desc[num].level = 17;
			power_desc[num].cost = 50;
			power_desc[num].fail = 100 - racial_chance(17, A_INT, 16);
			power_desc[num++].number = MUT1_RECALL;
		}

		if (p_ptr->muta1 & MUT1_BANISH)
		{
			strcpy(power_desc[num].name, "banish evil");
			power_desc[num].level = 25;
			power_desc[num].cost = 25;
			power_desc[num].fail = 100 - racial_chance(25, A_WIS, 18);
			power_desc[num++].number = MUT1_BANISH;
		}

		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
			strcpy(power_desc[num].name, "cold touch");
			power_desc[num].level = 2;
			power_desc[num].cost = 2;
			power_desc[num].fail = 100 - racial_chance(2, A_CON, 11);
			power_desc[num++].number = MUT1_COLD_TOUCH;
		}

		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
			strcpy(power_desc[num].name, "throw object");
			power_desc[num].level = 1;
			power_desc[num].cost = lvl;
			power_desc[num].fail = 100 - racial_chance(1, A_STR, 6);
			/* XXX_XXX_XXX Hack! MUT1_LAUNCHER counts as negative... */
			power_desc[num++].number = 3;
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt */
	(void)strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
		I2A(0), (num <= 26) ? I2A(num - 1) : '0' + num - 27);

if (!repeat_pull(&i) || i<0 || i>=num) {
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];
				char letter;
				int x1, y1;

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Print header(s) */
				if (num < 17)
					prt("                            Lv Cost Fail", y++, x);
				else
					prt("                            Lv Cost Fail                            Lv Cost Fail", y++, x);

				/* Print list */
				while (ctr < num)
				{
					/* letter/number for power selection */
					if (ctr < 26)
						letter = I2A(ctr);
					else
						letter = '0' + ctr - 26;
					x1 = ((ctr < 17) ? x : x + 40);
					y1 = ((ctr < 17) ? y + ctr : y + ctr - 17);

					sprintf(dummy, " %c) %-23.23s %2d %4d %3d%%", letter, power_desc[ctr].name, power_desc[ctr].level, power_desc[ctr].cost, power_desc[ctr].fail);
					prt(dummy, y1, x1);
					ctr++;
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Abort if needed */
	if (!flag)
	{
		p_ptr->energy_use = 0;
		return;
	}

        repeat_push(i);
        } /*if (!repeat_pull(&i) || ...)*/

	if (power_desc[i].number < 0)
	{
		cmd_racial_power_aux(power_desc[i].number);
	}
	else
	{
		mutation_power_aux(power_desc[i].number);
	}

	/* Success */
	return;
}
