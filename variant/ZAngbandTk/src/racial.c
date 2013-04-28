/* CVS: Last edit by $Author: rr9 $ on $Date: 2000/01/04 17:42:30 $ */
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


#include "tnb.h" /* TNB */


/*
 * Returns the chance to activate a racial power/mutation
 */
/* static -- TNB */ int racial_chance(s16b min_level, int use_stat,
	int difficulty)
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
		if (lev_adj > 10)
			lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5)
		difficulty = 5;

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
	if (p_ptr->csp < cost)
		use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->lev < min_level)
	{
		sound(SNDGRP_EVENT, SND_ACTION_FAILED, 0); /* ALLOW_SOUND */
		msg_format("You need to attain level %d to use this power.",
			min_level);
		energy_use = 0;
		return FALSE;
	}

	/* Too confused */
	else if (p_ptr->confused)
	{
		sound(SNDGRP_EVENT, SND_ACTION_FAILED, 0); /* ALLOW_SOUND */
		msg_print("You are too confused to use this power.");
		energy_use = 0;
		return FALSE;
	}

	/* Risk death? */
	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!get_check("Really use the power in your weakened state? "))
		{
			energy_use = 0;
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
		if (lev_adj > 10)
			lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5)
		difficulty = 5;

	/* take time and pay the price */
	energy_use = 100;

	if (use_hp)
	{
		take_hit((cost / 2) + randint(cost / 2), "concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (cost / 2) + randint(cost / 2);
	}


	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);

	/* Success? */
	if (randint(p_ptr->stat_cur[use_stat]) >=
		((difficulty / 2) + randint(difficulty / 2)))
	{
		return TRUE;
	}

	sound(SNDGRP_EVENT, SND_ACTION_FAILED, 0); /* ALLOW_SOUND */
	msg_print("You've failed to concentrate hard enough.");
	return FALSE;
}


#if 0 /* TNB */

static void cmd_racial_power_aux(s32b command)
{
	s16b plev = p_ptr->lev;
	int dir = 0;


	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			if (racial_aux(5, 5, A_WIS, 12))
			{
				msg_print("You examine your surroundings.");
				(void) detect_traps();
				(void) detect_doors();
				(void) detect_stairs();
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
				(void) drop_near(q_ptr, -1, py, px);
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
				(void) set_afraid(0);
			}
			break;

		case RACE_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
					(p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("RAAAGH!");
				(void) set_afraid(0);

				(void) set_shero(p_ptr->shero + 10 + randint(plev));
				(void) hp_player(30);
			}
			break;

		case RACE_AMBERITE:
			if (command == -2)
			{
				if (racial_aux(40, 75, A_WIS, 50))
				{
					msg_print
						("You picture the Pattern in your mind and walk it...");
					(void) set_poisoned(0);
					(void) set_image(0);
					(void) set_stun(0);
					(void) set_cut(0);
					(void) set_blind(0);
					(void) set_afraid(0);
					(void) do_res_stat(A_STR);
					(void) do_res_stat(A_INT);
					(void) do_res_stat(A_WIS);
					(void) do_res_stat(A_DEX);
					(void) do_res_stat(A_CON);
					(void) do_res_stat(A_CHR);
					(void) restore_level();
				}
			}

			else if (command == -1)
			{
				if (racial_aux(30, 50, A_INT, 50))
				{
					/* No effect in arena or quest */
					if (p_ptr->inside_arena || p_ptr->inside_quest)
					{
						msg_print("There is no effect.");
					}
					else
					{
						msg_print
							("You start walking around. Your surroundings change.");

						if (autosave_l)
							do_cmd_save_game(TRUE);

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
			}
			break;

		case RACE_BARBARIAN:
			if (racial_aux(8, 10, A_WIS,
					(p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("Raaagh!");
				(void) set_afraid(0);

				(void) set_shero(p_ptr->shero + 10 + randint(plev));
				(void) hp_player(30);
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
				if (!get_aim_dir(&dir))
					break;
				msg_print("You bash at a stone wall.");
				(void) wall_to_mud(dir);
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
				if (!get_aim_dir(&dir))
					break;
				msg_print("You throw a huge boulder.");
				fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
			}
			break;

		case RACE_YEEK:
			if (racial_aux(15, 15, A_WIS, 10))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You make a horrible scream!");
				(void) fear_monster(dir, plev);
			}
			break;

		case RACE_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
			{
				if (!get_aim_dir(&dir))
					break;
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
				if (!get_aim_dir(&dir))
					break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev);
			}
			break;

		case RACE_NIBELUNG:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You examine your surroundings.");
				(void) detect_traps();
				(void) detect_doors();
				(void) detect_stairs();
			}
			break;

		case RACE_DARK_ELF:
			if (racial_aux(2, 2, A_INT, 9))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You cast a magic missile.");
				fire_bolt_or_beam(10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			break;

		case RACE_DRACONIAN:
			if (racial_aux(1, plev, A_CON, 12))
			{
				int Type = ((randint(3) == 1) ? GF_COLD : GF_FIRE);
				cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");

				if (randint(100) < plev)
				{
					switch (p_ptr->pclass)
					{
						case CLASS_WARRIOR:
						case CLASS_RANGER:
							if (randint(3) == 1)
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
							if (randint(3) == 1)
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
							if (randint(3) != 1)
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
							if (randint(3) != 1)
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
							if (randint(3) != 1)
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
							if (randint(3) == 1)
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
							if (randint(3) == 1)
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

				if (!get_aim_dir(&dir))
					break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, plev * 2, (plev / 15) + 1);
			}
			break;

		case RACE_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir))
					break;
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
				if (!get_aim_dir(&dir))
					break;
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
				(void) set_shield(p_ptr->shield + randint(20) + 30);
			}
			break;

		case RACE_SKELETON:
		case RACE_ZOMBIE:
			if (racial_aux(30, 30, A_WIS, 18))
			{
				msg_print("You attempt to restore your lost energies.");
				(void) restore_level();
			}
			break;

		case RACE_VAMPIRE:
			if (racial_aux(2, (1 + (plev / 3)), A_CON, 9))
			{
				int y, x, dummy = 0;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir))
					break; /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + randint(plev) * MAX(1, plev / 10); /* Dmg */
				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void) hp_player(dummy);
					else
						msg_print("You were not hungry.");
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX) /* Not gorged already */
						(void) set_food(dummy >=
							PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case RACE_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir))
					break;
				(void) fear_monster(dir, plev);
			}
			break;

		case RACE_SPRITE:
			if (racial_aux(12, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (plev < 25)
					sleep_monsters_touch();
				else
					(void) sleep_monsters();
			}
			break;

		default:
			msg_print("This race has no bonus power.");
			energy_use = 0;
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
	int level;
	int cost;
	int fail;
	int number;
};


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	power_desc_type power_desc[36];
	int num, ask, i = 0;
	int lvl = p_ptr->lev;
	bool flag, redraw;
	bool warrior = ((p_ptr->pclass == CLASS_WARRIOR) ? TRUE : FALSE);
	bool has_racial = FALSE;
	char choice;
	char out_val[160];


	for (num = 0; num < 36; num++)
	{
		strcpy(power_desc[num].name, "");
		power_desc[num].number = 0;
	}

	num = 0;

	if (p_ptr->confused)
	{
		msg_print("You are too confused to use any powers!");
		energy_use = 0;
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
			power_desc[0].fail =
				100 - racial_chance(3, A_WIS, (warrior ? 5 : 10));
			has_racial = TRUE;
			break;
		case RACE_HALF_TROLL:
			strcpy(power_desc[0].name, "berserk");
			power_desc[0].level = 10;
			power_desc[0].cost = 12;
			power_desc[0].fail =
				100 - racial_chance(10, A_WIS, (warrior ? 6 : 12));
			has_racial = TRUE;
			break;
		case RACE_BARBARIAN:
			strcpy(power_desc[0].name, "berserk");
			power_desc[0].level = 8;
			power_desc[0].cost = 10;
			power_desc[0].fail =
				100 - racial_chance(8, A_WIS, (warrior ? 6 : 12));
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
			sprintf(power_desc[0].name, "throw boulder (dam %d)",
				(3 * lvl) / 2);
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
			sprintf(power_desc[0].name, "magic missile (dm %dd%d)",
				3 + ((lvl - 1) / 5), 4);
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
		energy_use = 0;
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
	(void) strnfmt(out_val, 78,
		"(Powers %c-%c, *=List, ESC=exit) Use which power? ", I2A(0),
		(num <= 26) ? I2A(num - 1) : '0' + num - 27);

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
					prt("                            Lv Cost Fail", y++,
						x);
				else
					prt
						("                            Lv Cost Fail                            Lv Cost Fail",
						y++, x);

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

					sprintf(dummy, " %c) %-23.23s %2d %4d %3d%%", letter,
						power_desc[ctr].name, power_desc[ctr].level,
						power_desc[ctr].cost, power_desc[ctr].fail);
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
			if (ask)
				choice = tolower(choice);

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
			(void) strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);

			/* Belay that order */
			if (!get_check(tmp_val))
				continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
		screen_load();

	/* Abort if needed */
	if (!flag)
	{
		energy_use = 0;
		return;
	}

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

#else /* TNB */

static bool get_power(int *pn)
{
	int i;

	int power = -1;
	int num;
	int ask;

	int powers[MAX_POWER];

	bool flag, redraw, okay;
	char choice;

	char out_val[160];


	/* Assume no powers available */
	(*pn) = -2;

	/* Get a list of POWER_XXX indexes */
	num = get_powers(powers);

	/* No "okay" powers */
	if (!num)
		return (FALSE);

#ifdef ALLOW_REPEAT	/* TNB */

	/* Get the power, if available */
	if (repeat_pull(pn))
	{

		/* Check each "okay" power */
		for (i = 0; i < num; i++)
		{
			/* Verify the power */
			if (powers[i] == (*pn))
			{
				/* Success */
				return (TRUE);
			}
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Assume cancelled */
	*pn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all powers) */
	strnfmt(out_val, 78,
		"(Powers %c-%c, *=List, ESC=exit) Use which power? ", I2A(0),
		(num <= 26) ? I2A(num - 1) : ('0' + (num - 27)));

	Bind_Choose(KEYWORD_CHOOSE_POWER + 1, 0, TRUE);

	/* Get a power from the user */
	while (!flag)
	{
		inkey_flags = (INKEY_POWER);

		okay = get_com(out_val, &choice);

		inkey_flags = 0;
		if (!okay)
			break;

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Display a list of powers */
				angtk_eval("angband_display", "power", "show", NULL);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Hide list */
				angtk_eval("angband_display", "power", "hide", NULL);
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
			ask = isupper(choice);

			/* Lowercase */
			if (ask)
				choice = tolower(choice);

			/* Extract request */
			i = A2I(choice);
		}
		else
		{
			/* Can't uppercase digits */
			ask = FALSE;

			/* Extract request */
			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the POWER_XXX index */
		power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[power]);

			/* Belay that order */
			if (!get_check(tmp_val))
				continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	Bind_Choose(KEYWORD_CHOOSE_POWER + 1, 0, FALSE);

	/* Hide the list */
	if (redraw)
		angtk_eval("angband_display", "power", "hide", NULL);

	/* Abort if needed */
	if (!flag)
		return (FALSE);

	/* Save the choice */
	(*pn) = power;

#ifdef ALLOW_REPEAT	/* TNB */

	repeat_push(*pn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}

/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	int power;
	int dir = 0;
	s16b plev = p_ptr->lev;
	cptr q, s;

	if (p_ptr->confused)
	{
#if 1 /* TNB */
		if (flush_failure)
			flush();
#endif /* TNB */
		sound(SNDGRP_EVENT, SND_ACTION_FAILED, 0); /* ALLOW_SOUND */
		msg_print("You are too confused to use any powers!");
		return;
	}

	if (!get_power(&power))
	{
		if (power == -2)
		{
			sound(SNDGRP_EVENT, SND_ACTION_FAILED, 0); /* ALLOW_SOUND */
			msg_print("You have no powers to activate.");
		}
		return;
	}

	/* Powers. */
	switch (power)
	{
		case POWER_DWARF:
			if (racial_aux(5, 5, A_WIS, 12))
			{
				msg_print("You examine your surroundings.");
				(void) detect_traps();
				(void) detect_doors();
				(void) detect_stairs();
			}
			break;

		case POWER_HOBBIT:
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
				drop_near(q_ptr, -1, py, px);
				msg_print("You cook some food.");
			}
			break;

		case POWER_GNOME:
			if (racial_aux(5, (5 + (plev / 5)), A_INT, 12))
			{
				msg_print("Blink!");
				teleport_player(10 + plev);
			}
			break;

		case POWER_HALF_ORC:
			if (racial_aux(3, 5, A_WIS,
					(p_ptr->pclass == CLASS_WARRIOR ? 5 : 10)))
			{
				msg_print("You play tough.");
				(void) set_afraid(0);
			}
			break;

		case POWER_HALF_TROLL:
			if (racial_aux(10, 12, A_WIS,
					(p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("RAAAGH!");
				(void) set_afraid(0);

				(void) set_shero(p_ptr->shero + 10 + randint(plev));
				(void) hp_player(30);
			}
			break;

		case POWER_AMBERITE_A:
			if (racial_aux(40, 75, A_WIS, 50))
			{
				msg_print
					("You picture the Pattern in your mind and walk it...");
				(void) set_poisoned(0);
				(void) set_image(0);
				(void) set_stun(0);
				(void) set_cut(0);
				(void) set_blind(0);
				(void) set_afraid(0);
				(void) do_res_stat(A_STR);
				(void) do_res_stat(A_INT);
				(void) do_res_stat(A_WIS);
				(void) do_res_stat(A_DEX);
				(void) do_res_stat(A_CON);
				(void) do_res_stat(A_CHR);
				(void) restore_level();
			}
			break;

		case POWER_AMBERITE_B:
			if (racial_aux(30, 50, A_INT, 50))
			{
				/* No effect in arena or quest */
				if (p_ptr->inside_arena || p_ptr->inside_quest)
				{
					msg_print("There is no effect.");
				}
				else
				{
					msg_print
						("You start walking around. Your surroundings change.");

					if (autosave_l)
						do_cmd_save_game(TRUE);

					/* Leaving */
					p_ptr->leaving = TRUE;
				}
			}
			break;

		case POWER_BARBARIAN:
			if (racial_aux(8, 10, A_WIS,
					(p_ptr->pclass == CLASS_WARRIOR ? 6 : 12)))
			{
				msg_print("Raaagh!");
				(void) set_afraid(0);

				(void) set_shero(p_ptr->shero + 10 + randint(plev));
				(void) hp_player(30);
			}
			break;

		case POWER_HALF_OGRE:
			if (racial_aux(25, 35, A_INT, 15))
			{
				msg_print("You carefully set an explosive rune...");
				explosive_rune();
			}
			break;

		case POWER_HALF_GIANT:
			if (racial_aux(20, 10, A_STR, 12))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You bash at a stone wall.");
				(void) wall_to_mud(dir);
			}
			break;

		case POWER_HALF_TITAN:
			if (racial_aux(35, 20, A_INT, 12))
			{
				msg_print("You examine your foes...");
				probing();
			}
			break;

		case POWER_CYCLOPS:
			if (racial_aux(20, 15, A_STR, 12))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You throw a huge boulder.");
				fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
			}
			break;

		case POWER_YEEK:
			if (racial_aux(15, 15, A_WIS, 10))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You make a horrible scream!");
				(void) fear_monster(dir, plev);
			}
			break;

		case POWER_KLACKON:
			if (racial_aux(9, 9, A_DEX, 14))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You spit acid.");
				if (plev < 25)
					fire_bolt(GF_ACID, dir, plev);
				else
					fire_ball(GF_ACID, dir, plev, 2);
			}
			break;

		case POWER_KOBOLD:
			if (racial_aux(12, 8, A_DEX, 14))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You throw a dart of poison.");
				fire_bolt(GF_POIS, dir, plev);
			}
			break;

		case POWER_NIBELUNG:
			if (racial_aux(10, 5, A_WIS, 10))
			{
				msg_print("You examine your surroundings.");
				(void) detect_traps();
				(void) detect_doors();
				(void) detect_stairs();
			}
			break;

		case POWER_DARK_ELF:
			if (racial_aux(2, 2, A_INT, 9))
			{
				if (!get_aim_dir(&dir))
					break;
				msg_print("You cast a magic missile.");
				fire_bolt_or_beam(10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			break;

		case POWER_DRACONIAN:
			if (racial_aux(1, plev, A_CON, 12))
			{
				int Type = ((randint(3) == 1) ? GF_COLD : GF_FIRE);
				cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");

				if (randint(100) < plev)
				{
					switch (p_ptr->pclass)
					{
						case CLASS_WARRIOR:
						case CLASS_RANGER:
							if (randint(3) == 1)
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
							if (randint(3) == 1)
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
							if (randint(3) != 1)
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
							if (randint(3) != 1)
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
							if (randint(3) != 1)
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
							if (randint(3) == 1)
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
							if (randint(3) == 1)
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

				if (!get_aim_dir(&dir))
					break;
				msg_format("You breathe %s.", Type_desc);
				fire_ball(Type, dir, plev * 2, (plev / 15) + 1);
			}
			break;

		case POWER_MIND_FLAYER:
			if (racial_aux(15, 12, A_INT, 14))
			{
				if (!get_aim_dir(&dir))
					break;
				else
				{
					msg_print("You concentrate and your eyes glow red...");
					fire_bolt(GF_PSI, dir, plev);
				}
			}
			break;

		case POWER_IMP:
			if (racial_aux(9, 15, A_WIS, 15))
			{
				if (!get_aim_dir(&dir))
					break;
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

		case POWER_GOLEM:
			if (racial_aux(20, 15, A_CON, 8))
			{
				(void) set_shield(p_ptr->shield + randint(20) + 30);
			}
			break;

		case POWER_SKELETON:
		case POWER_ZOMBIE:
			if (racial_aux(30, 30, A_WIS, 18))
			{
				msg_print("You attempt to restore your lost energies.");
				(void) restore_level();
			}
			break;

		case POWER_VAMPIRE:
			if (racial_aux(2, (1 + (plev / 3)), A_CON, 9))
			{
				int y, x, dummy = 0;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir))
					break; /* was get_aim_dir */
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");
				dummy = plev + randint(plev) * MAX(1, plev / 10); /* Dmg */
				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void) hp_player(dummy);
					else
						msg_print("You were not hungry.");
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX) /* Not gorged already */
						(void) set_food(dummy >=
							PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case POWER_SPECTRE:
			if (racial_aux(4, 6, A_INT, 3))
			{
				msg_print("You emit an eldritch howl!");
				if (!get_aim_dir(&dir))
					break;
				(void) fear_monster(dir, plev);
			}
			break;

		case POWER_SPRITE:
			if (racial_aux(12, 12, A_INT, 15))
			{
				msg_print("You throw some magic dust...");
				if (plev < 25)
					sleep_monsters_touch();
				else
					(void) sleep_monsters();
			}
			break;

			/* Mutation power */
		case POWER_SPIT_ACID:
			if (racial_aux(9, 9, A_DEX, 15))
			{
				msg_print("You spit acid...");
				if (get_aim_dir(&dir))
					fire_ball(GF_ACID, dir, plev, 1 + (plev / 30));
			}
			break;

		case POWER_BR_FIRE:
			if (racial_aux(20, plev, A_CON, 18))
			{
				msg_print("You breathe fire...");
				if (get_aim_dir(&dir))
					fire_ball(GF_FIRE, dir, plev * 2, 1 + (plev / 20));
			}
			break;

		case POWER_HYPN_GAZE:
			if (racial_aux(12, 12, A_CHR, 18))
			{
				msg_print("Your eyes look mesmerizing...");
				if (get_aim_dir(&dir))
					(void) charm_monster(dir, plev);
			}
			break;

		case POWER_TELEKINES:
			if (racial_aux(9, 9, A_WIS, 14))
			{
				msg_print("You concentrate...");
				if (get_aim_dir(&dir))
					fetch(dir, plev * 10, TRUE);
			}
			break;

		case POWER_VTELEPORT:
			if (racial_aux(7, 7, A_WIS, 15))
			{
				msg_print("You concentrate...");
				teleport_player(10 + 4 * plev);
			}
			break;

		case POWER_MIND_BLST:
			if (racial_aux(5, 3, A_WIS, 15))
			{
				msg_print("You concentrate...");
				if (!get_aim_dir(&dir))
					return;
				fire_bolt(GF_PSI, dir, damroll(3 + ((plev - 1) / 5), 3));
			}
			break;

		case POWER_RADIATION:
			if (racial_aux(15, 15, A_CON, 14))
			{
				msg_print("Radiation flows from your body!");
				fire_ball(GF_NUKE, 0, (plev * 2), 3 + (plev / 20));
			}
			break;

		case POWER_VAMPIRISM:
			if (racial_aux(2, (1 + (plev / 3)), A_CON, 9))
			{
				int x, y, dummy;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir))
					break;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!(c_ptr->m_idx))
				{
					msg_print("You bite into thin air!");
					break;
				}

				msg_print("You grin and bare your fangs...");

				dummy = plev * 2;

				if (drain_life(dir, dummy))
				{
					if (p_ptr->food < PY_FOOD_FULL)
						/* No heal if we are "full" */
						(void) hp_player(dummy);
					else
						msg_print("You were not hungry.");
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX) /* Not gorged already */
						(void) set_food(dummy >=
							PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msg_print("Yechh. That tastes foul.");
			}
			break;

		case POWER_SMELL_MET:
			if (racial_aux(3, 2, A_INT, 12))
			{
				(void) detect_treasure();
			}
			break;

		case POWER_SMELL_MON:
			if (racial_aux(5, 4, A_INT, 15))
			{
				(void) detect_monsters_normal();
			}
			break;

		case POWER_BLINK:
			if (racial_aux(3, 3, A_WIS, 12))
			{
				teleport_player(10);
			}
			break;

		case POWER_EAT_ROCK:
			if (racial_aux(8, 12, A_CON, 18))
			{
				int x, y /* , ox, oy -- TNB */;
				cave_type *c_ptr;

				if (!get_rep_dir(&dir))
					break;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];
				if (cave_floor_bold(y, x))
				{
					msg_print("You bite into thin air!");
					break;
				}
				else if (((c_ptr->feat >= FEAT_PERM_EXTRA) &&
						(c_ptr->feat <= FEAT_PERM_SOLID)) ||
					(c_ptr->feat == FEAT_MOUNTAIN))
				{
					msg_print
						("Ouch!  This wall is harder than your teeth!");
					break;
				}
				else if (c_ptr->m_idx)
				{
					msg_print("There's something in the way!");
					break;
				}
				else if (c_ptr->feat == FEAT_TREES)
				{
					msg_print("You don't like the woody taste!");
					break;
				}
				else
				{
					if ((c_ptr->feat >= FEAT_DOOR_HEAD) &&
						(c_ptr->feat <= FEAT_RUBBLE))
					{
						(void) set_food(p_ptr->food + 3000);
					}
					else if ((c_ptr->feat >= FEAT_MAGMA) &&
						(c_ptr->feat <= FEAT_QUARTZ_K))
					{
						(void) set_food(p_ptr->food + 5000);
					}
					else
					{
						msg_print("This granite is very filling!");
						(void) set_food(p_ptr->food + 10000);
					}
				}
				(void) wall_to_mud(dir);

#if 1 /* TNB */

				/* ALLOW_SOUND */
				sound(SNDGRP_EVENT, SND_WALK, 0x01);
				sound_flush();

				monster_swap(py, px, y, x);

#else /* not 1 -- TNB */

				oy = py;
				ox = px;

				py = y;
				px = x;

				lite_spot(py, px);
				lite_spot(oy, ox);

				verify_panel();

				p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
				p_ptr->update |= (PU_DISTANCE);
				p_ptr->window |= (PW_OVERHEAD);

#endif /* not 1 -- TNB */
			}
			break;

		case POWER_SWAP_POS:
			if (racial_aux(15, 12, A_DEX, 16))
			{
				if (get_aim_dir(&dir))
					(void) teleport_swap(dir);
			}
			break;

		case POWER_SHRIEK:
			if (racial_aux(20, 14, A_CON, 16))
			{
				(void) fire_ball(GF_SOUND, 0, 2 * plev, 8);
				(void) aggravate_monsters(0);
			}
			break;

		case POWER_ILLUMINE:
			if (racial_aux(3, 2, A_INT, 10))
			{
				(void) lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			}
			break;

		case POWER_DET_CURSE:
			if (racial_aux(7, 14, A_WIS, 14))
			{
				int i;

				for (i = 0; i < INVEN_TOTAL; i++)
				{
					object_type *o_ptr = &inventory[i];

					if (!o_ptr->k_idx)
						continue;
					if (!cursed_p(o_ptr))
						continue;

					o_ptr->feeling = FEEL_CURSED;
				}
			}
			break;

		case POWER_BERSERK:
			if (racial_aux(8, 8, A_STR, 14))
			{
				(void) set_shero(p_ptr->shero + randint(25) + 25);
				(void) hp_player(30);
				(void) set_afraid(0);
			}
			break;

		case POWER_POLYMORPH:
			if (racial_aux(18, 20, A_CON, 18))
			{
				do_poly_self();
			}
			break;

		case POWER_MIDAS_TCH:
			if (racial_aux(10, 5, A_INT, 12))
			{
				(void) alchemy();
			}
			break;

			/* Summon pet molds around the player */
		case POWER_GROW_MOLD:
			if (racial_aux(1, 6, A_CON, 14))
			{
				int i;
				for (i = 0; i < 8; i++)
				{
					summon_specific(-1, py, px, plev, SUMMON_BIZARRE1, FALSE,
						TRUE, TRUE);
				}
			}
			break;

		case POWER_RESIST:
			if (racial_aux(10, 12, A_CON, 12))
			{
				int num = plev / 10;
				int dur = randint(20) + 20;

				if (rand_int(5) < num)
				{
					(void) set_oppose_acid(p_ptr->oppose_acid + dur);
					num--;
				}
				if (rand_int(4) < num)
				{
					(void) set_oppose_elec(p_ptr->oppose_elec + dur);
					num--;
				}
				if (rand_int(3) < num)
				{
					(void) set_oppose_fire(p_ptr->oppose_fire + dur);
					num--;
				}
				if (rand_int(2) < num)
				{
					(void) set_oppose_cold(p_ptr->oppose_cold + dur);
					num--;
				}
				if (num)
				{
					(void) set_oppose_pois(p_ptr->oppose_pois + dur);
					num--;
				}
			}
			break;

		case POWER_EARTHQUAKE:
			if (racial_aux(12, 12, A_STR, 16))
			{
				sound(SNDGRP_EVENT, SND_SPELL_EARTHQUAKE, 0); /* ALLOW_SOUND */
				earthquake(py, px, 10);
			}
			break;

		case POWER_EAT_MAGIC:
			if (racial_aux(17, 1, A_WIS, 15))
			{
				object_type *o_ptr;
				int lev, item;

				item_tester_hook = item_tester_hook_recharge;

				/* Get an item */
				q = "Drain which item? ";
				s = "You have nothing to drain.";
				if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
					break;

				if (item >= 0)
				{
					o_ptr = &inventory[item];
				}
				else
				{
					o_ptr = &o_list[0 - item];
				}

				lev = get_object_level(o_ptr);

				if (o_ptr->tval == TV_ROD)
				{
					if (o_ptr->pval > 0)
					{
						msg_print
							("You can't absorb energy from a discharged rod.");
					}
					else
					{
						p_ptr->csp += 2 * lev;
						o_ptr->pval = 500;
					}
				}
				else
				{
					if (o_ptr->pval > 0)
					{
						p_ptr->csp += o_ptr->pval * lev;
						o_ptr->pval = 0;
					}
					else
					{
						msg_print("There's no energy there to absorb!");
					}
					o_ptr->ident |= IDENT_EMPTY;
				}

				if (p_ptr->csp > p_ptr->msp)
				{
					p_ptr->csp = p_ptr->msp;
				}

				p_ptr->notice |= (PN_COMBINE | PN_REORDER);
				p_ptr->window |= (PW_INVEN);
			}
			break;

		case POWER_WEIGH_MAG:
			if (racial_aux(6, 6, A_INT, 10))
			{
				report_magics();
			}
			break;

		case POWER_STERILITY:
			if (racial_aux(20, 40, A_CHR, 18))
			{
				/* Fake a population explosion. */
				msg_print("You suddenly have a headache!");
				take_hit(randint(30) + 30,
					"the strain of forcing abstinence");
				num_repro += MAX_REPRO;
			}
			break;

		case POWER_PANIC_HIT:
			if (racial_aux(10, 12, A_DEX, 14))
			{
				int x, y;

				if (!get_rep_dir(&dir))
					return;
				y = py + ddy[dir];
				x = px + ddx[dir];
				if (cave[y][x].m_idx)
				{
					py_attack(y, x);
					teleport_player(30);
				}
				else
				{
					msg_print
						("You don't see any monster in this direction");
					msg_print(NULL);
				}
			}
			break;

		case POWER_DAZZLE:
			if (racial_aux(7, 15, A_CHR, 8))
			{
				stun_monsters(plev * 4);
				confuse_monsters(plev * 4);
				turn_monsters(plev * 4);
			}
			break;

		case POWER_LASER_EYE:
			if (racial_aux(7, 10, A_WIS, 9))
			{
				if (get_aim_dir(&dir))
					fire_beam(GF_LITE, dir, 2 * plev);
			}
			break;

		case POWER_RECALL:
			if (racial_aux(17, 50, A_INT, 16))
			{
				if (ironman_downward)
				{
					msg_print("Your skill fails.");
				}
				else
				{
					if (dun_level && (p_ptr->max_dlv > dun_level))
					{
						if (get_check("Reset recall depth? "))
							p_ptr->max_dlv = dun_level;
					}
					if (!p_ptr->word_recall)
					{
#ifdef TNB_BACKUP
						do_cmd_backup_game();
#endif /* TNB_BACKUP */
						p_ptr->word_recall = rand_int(21) + 15;
						msg_print("The air about you becomes charged...");
						p_ptr->redraw |= (PR_STATUS);
					}
					else
					{
						p_ptr->word_recall = 0;
						msg_print
							("A tension leaves the air around you...");
						p_ptr->redraw |= (PR_STATUS);
					}
					redraw_add(PR_RECALL); /* ALLOW_STATUS_EXTRA */
				}
			}
			break;

		case POWER_BANISH:
			if (racial_aux(25, 25, A_WIS, 18))
			{
				int x, y;
				cave_type *c_ptr;
				monster_type *m_ptr;
				monster_race *r_ptr;

				if (!get_rep_dir(&dir))
					return;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
					msg_print("You sense no evil there!");
					break;
				}

				m_ptr = &m_list[c_ptr->m_idx];
				r_ptr = &r_info[m_ptr->r_idx];

				if ((r_ptr->flags3 & RF3_EVIL) &&
					!(r_ptr->flags1 & RF1_QUESTOR) &&
					!(r_ptr->flags1 & RF1_UNIQUE))
				{
					/* Delete the monster, rather than killing it. */
					delete_monster_idx(c_ptr->m_idx);
					msg_print
						("The evil creature vanishes in a puff of sulfurous smoke!");
				}
				else
				{
					msg_print("Your invocation is ineffectual!");
				}
			}
			break;

		case POWER_COLD_TOUCH:
			if (racial_aux(2, 2, A_CON, 11))
			{
				int x, y;
				cave_type *c_ptr;

				if (!get_rep_dir(&dir))
					return;
				y = py + ddy[dir];
				x = px + ddx[dir];
				c_ptr = &cave[y][x];

				if (!c_ptr->m_idx)
				{
					msg_print("You wave your hands in the air.");
					break;
				}
				fire_bolt(GF_COLD, dir, 2 * plev);
			}
			break;

		case POWER_LAUNCHER:
			if (racial_aux(1, plev, A_STR, 6))
			{
				/* Gives a multiplier of 2 at first, up to 5 at 48th */
				do_cmd_throw_aux(2 + plev / 30);
			}
			break;

		default:
			msg_format("Power %d is not implemented.", power);
			energy_use = 0;
			break;
	}
}

#endif /* TNB */
