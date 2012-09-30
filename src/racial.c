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
    int stat = p_ptr->stat[use_stat].cur;

    if (stat <= 180)
        stat /= 10;
    else
        stat += 18-180;

	/* No chance for success */
	if ((p_ptr->lev < min_level) || p_ptr->tim.confused)
	{
		return (0);
	}

	/* Calculate difficulty */
	if (p_ptr->tim.stun)
	{
		difficulty += p_ptr->tim.stun;
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

/* 
 * Helper function for ghouls.
 * I realize it is somewhat illogical to have this as a "power" rather
 * than an extension of the "eat" command, but I could not think of
 * a handy solution to the conceptual/UI problem of having food objects AND
 * an edible corpse in the same square...
 * Eating corpses should probably take more than 1 turn (realistically). 
 * (OTOH, you can swap your full-plate armour for a dragonscalemail in
 * 1 turn *shrug*) 
 */
static void eat_corpse(void)
{
	field_type *f_ptr;

	/* While there are fields in the linked list */
	FLD_ITT_START (area(p_ptr->px, p_ptr->py)->fld_idx, f_ptr)
	{
		/* Want a corpse / skeleton */
		if ((f_ptr->t_idx == FT_CORPSE || f_ptr->t_idx == FT_SKELETON))
		{
			if (f_ptr->t_idx == FT_CORPSE)
			{
				msgf("The corpse tastes delicious!");
				(void)set_food(p_ptr->food + 2000);
			}
			else
			{
				msgf("The bones taste delicious!");
				(void)set_food(p_ptr->food + 1000);
			}

			/* Sound */
			sound(SOUND_EAT);

			delete_field_ptr(f_ptr);

			/* Done */
			return;
		}
	}
	FLD_ITT_END;

	/* Nothing to eat */
	msgf("There is no fresh skeleton or corpse here!");
	p_ptr->state.energy_use = 0;

	/* Done */
	return;
}


/*
 * Note: return value indicates that we have succesfully used the power
 */
bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
    bool use_hp = FALSE;
    int stat = p_ptr->stat[use_stat].cur;

    if (stat <= 180)
        stat /= 10;
    else
        stat += 18-180;

	/* Not enough mana - use hp */
	if (p_ptr->csp < cost) use_hp = TRUE;

	/* Power is not available yet */
	if (p_ptr->lev < min_level)
	{
		msgf("You need to attain level %d to use this power.", min_level);
		p_ptr->state.energy_use = 0;
		return FALSE;
	}

	/* Too confused */
	else if (p_ptr->tim.confused)
	{
		msgf("You are too confused to use this power.");
		p_ptr->state.energy_use = 0;
		return FALSE;
	}

	/* Risk death? */
	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!get_check("Really use the power in your weakened state? "))
		{
			p_ptr->state.energy_use = 0;
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	if (p_ptr->tim.stun)
	{
		difficulty += p_ptr->tim.stun;
	}
	else if (p_ptr->lev > min_level)
	{
		int lev_adj = ((p_ptr->lev - min_level) / 3);
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* take time and pay the price */
	p_ptr->state.energy_use = 100;

	if (use_hp)
	{
		take_hit(rand_range(cost / 2, cost), "concentrating too hard");
	}
	else
	{
		p_ptr->csp -= (s16b)rand_range(cost / 2, cost);
	}


	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);

	/* Success? */
	if (randint1(stat) >=
		rand_range(difficulty / 2, difficulty))
	{
		return TRUE;
	}

	msgf("You've failed to concentrate hard enough.");
	return FALSE;
}


static void cmd_racial_power_aux(const mutation_type *mut_ptr)
{
	s16b plev = p_ptr->lev;
	int dir = 0;

	if (racial_aux(mut_ptr->level, mut_ptr->cost, mut_ptr->stat, mut_ptr->diff))
	{
		switch (p_ptr->rp.prace)
		{
			case RACE_DWARF:
			{
				msgf("You examine your surroundings.");
				(void)detect_traps(TRUE);
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case RACE_HOBBIT:
			{
				create_food();
				msgf("You cook some food.");

				break;
			}

			case RACE_GNOME:
			{
				msgf("Blink!");
				teleport_player(10 + plev);
				break;
			}

			case RACE_HALF_ORC:
			{
				msgf("You play tough.");
				(void)clear_afraid();
				break;
			}

			case RACE_HALF_TROLL:
			{
				msgf("RAAAGH!");
				if (!p_ptr->tim.shero)
				{
					(void)hp_player(30);
				}
				(void)clear_afraid();
				(void)inc_shero(10 + randint1(plev));

				break;
			}

			case RACE_AMBERITE:
			{
				/* Hack - use levels to choose ability */
				if (mut_ptr->level == 40)
				{
					msgf
						("You picture the Pattern in your mind and walk it...");
					(void)clear_poisoned();
					(void)clear_image();
					(void)clear_stun();
					(void)clear_cut();
					(void)clear_blind();
					(void)clear_afraid();
					(void)do_res_stat(A_STR);
					(void)do_res_stat(A_INT);
					(void)do_res_stat(A_WIS);
					(void)do_res_stat(A_DEX);
					(void)do_res_stat(A_CON);
					(void)do_res_stat(A_CHR);
					(void)restore_level();
				}

				else if (mut_ptr->level == 30)
				{
					msgf("You start walking around. Your surroundings change.");

					/* Leaving */
					p_ptr->state.leaving = TRUE;
				}
				break;
			}

			case RACE_BARBARIAN:
			{
				msgf("Raaagh!");
				if (!p_ptr->tim.shero)
				{
					(void)hp_player(30);
				}

				(void)clear_afraid();
				(void)inc_shero(10 + randint1(plev));
				break;
			}

			case RACE_HALF_OGRE:
			{
				msgf("You carefully set an explosive rune...");
				(void)explosive_rune();
				break;
			}

			case RACE_HALF_GIANT:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You bash at a stone wall.");
				(void)wall_to_mud(dir);
				break;
			}

			case RACE_HALF_TITAN:
			{
				msgf("You examine your foes...");
				(void)probing();
				break;
			}

			case RACE_CYCLOPS:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You throw a huge boulder.");
				(void)fire_bolt(GF_MISSILE, dir, (3 * plev) / 2);
				break;
			}

			case RACE_YEEK:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You make a horrible scream!");
				(void)fear_monster(dir, plev);
				break;
			}

			case RACE_KLACKON:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You spit acid.");
				if (plev < 25)
					(void)fire_bolt(GF_ACID, dir, plev);
				else
					(void)fire_ball(GF_ACID, dir, plev, 2);
				break;
			}

			case RACE_KOBOLD:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You throw a dart of poison.");
				(void)fire_bolt(GF_POIS, dir, plev);
				break;
			}

			case RACE_NIBELUNG:
			{
				msgf("You examine your surroundings.");
				(void)detect_traps(TRUE);
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case RACE_DARK_ELF:
			{
				if (!get_aim_dir(&dir)) break;
				msgf("You cast a magic missile.");
				(void)fire_bolt_or_beam(10, GF_MISSILE, dir,
										damroll(3 + ((plev - 1) / 5), 4));
				break;
			}

			case RACE_DRACONIAN:
			{
				int Type = (one_in_(3) ? GF_COLD : GF_FIRE);
				cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");

				if (randint1(100) < plev)
				{
					switch (p_ptr->rp.pclass)
					{
						case CLASS_WARRIOR:
						case CLASS_RANGER:
							if (one_in_(3))
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
							if (one_in_(3))
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
							if (!one_in_(3))
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
							if (!one_in_(3))
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
							if (!one_in_(3))
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
							if (one_in_(3))
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
							if (one_in_(3))
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
				msgf("You breathe %s.", Type_desc);
				(void)fire_ball(Type, dir, plev * 2, (plev / 15) + 1);
				break;
			}

			case RACE_MIND_FLAYER:
			{
				if (!get_aim_dir(&dir)) break;
				else
				{
					msgf("You concentrate and your eyes glow red...");
					(void)fire_bolt(GF_PSI, dir, plev);
				}

				break;
			}

			case RACE_IMP:
			{
				if (!get_aim_dir(&dir)) break;
				if (plev >= 30)
				{
					msgf("You cast a ball of fire.");
					(void)fire_ball(GF_FIRE, dir, plev, 2);
				}
				else
				{
					msgf("You cast a bolt of fire.");
					(void)fire_bolt(GF_FIRE, dir, plev);
				}
				break;
			}

			case RACE_GOLEM:
			{
				(void)inc_shield(rand_range(30, 50));
				break;
			}

			case RACE_SKELETON:
			case RACE_ZOMBIE:
			{
				msgf("You attempt to restore your lost energies.");
				(void)restore_level();
				break;
			}

			case RACE_VAMPIRE:
			{
				int y, x, dummy;
				cave_type *c_ptr;

				/* Only works on adjacent monsters */
				if (!get_rep_dir(&dir)) break;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				/* Paranoia */
				if (!in_bounds2(x, y)) break;

				c_ptr = area(x, y);

				if (!c_ptr->m_idx)
				{
					msgf("You bite into thin air!");
					break;
				}

				msgf("You grin and bare your fangs...");
				dummy = plev + randint1(plev) * MAX(1, plev / 10);	/* Dmg */
				if (drain_gain_life(dir, dummy))
				{
					/* Gain nutritional sustenance: 150/hp drained */
					/* A Food ration gives 5000 food points (by contrast) */
					/* Don't ever get more than "Full" this way */
					/* But if we ARE Gorged,  it won't cure us */
					dummy = p_ptr->food + MIN(5000, 100 * dummy);
					if (p_ptr->food < PY_FOOD_MAX)	/* Not gorged already */
						(void)set_food(dummy >=
									   PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
				}
				else
					msgf("Yechh. That tastes foul.");
				break;
			}

			case RACE_SPECTRE:
			{
				msgf("You emit an eldritch howl!");
				if (!get_aim_dir(&dir)) break;
				(void)fear_monster(dir, plev);
				break;
			}

			case RACE_SPRITE:
			{
				msgf("You throw some magic dust...");
				if (plev < 25)
					(void)sleep_monsters_touch();
				else
					(void)sleep_monsters();
				break;
			}
			case RACE_GHOUL:
			{
				if (mut_ptr->level == 30)
				{
					/* Sense living */
					(void)detect_monsters_living();
				}
				else
				{
					eat_corpse();
				}
				break;
			}
			default:
				msgf("This race has no bonus power.");
				p_ptr->state.energy_use = 0;
		}
	}

	/* Redraw mana and hp */
	p_ptr->redraw |= (PR_HP | PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER | PW_SPELL);
}


/*
 * Header for racial menu
 */
static int display_racial_header(int num)
{
	/* Print header(s) */
	if (num < 18)
		prtf(0, 2, "                            Lv Cost Fail");
	else
		prtf(0, 2, "                            Lv Cost Fail                            Lv Cost Fail");
		
	
	/* Move the options down one row */
	return (1);
}



typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
	cptr name;
	int level;
	int cost;
	int fail;
	int number;
	const mutation_type *power;
};

static power_desc_type power_desc[36];

/*
 * Access the power and use it.
 */
static bool do_cmd_power_aux(int num)
{
	if (power_desc[num].number == -1)
	{
		/* A racial power */
		cmd_racial_power_aux(power_desc[num].power);
	}
	else
	{
		/* A mutation */
		mutation_power_aux(power_desc[num].power);
	}

	/* Exit the menu */
	return (TRUE);
}


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
	menu_type racial_menu[37];
	int num = 0, i = 0;
	
	char buf[1024];

	const mutation_type *mut_ptr;

	/* Not when we're confused */
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused to use any powers!");
		p_ptr->state.energy_use = 0;
		return;
	}

	/* Look for racial powers */
	for (i = 0; i < MAX_RACE_POWERS; i++)
	{
		mut_ptr = &race_powers[i];

		if (mut_ptr->which == p_ptr->rp.prace)
		{
			power_desc[num].name = mut_ptr->name;
			power_desc[num].level = mut_ptr->level;
			power_desc[num].cost = mut_ptr->cost;
			power_desc[num].fail = 100 -
				racial_chance(mut_ptr->level, mut_ptr->stat, mut_ptr->diff);
			power_desc[num].number = -1;
			power_desc[num].power = mut_ptr;
			num++;
		}
	}

	/* Look for appropriate mutations */
	for (i = 0; i < MUT_PER_SET; i++)
	{
		mut_ptr = &mutations[i];

		if (p_ptr->muta1 & mut_ptr->which)
		{
			power_desc[num].name = mut_ptr->name;
			power_desc[num].level = mut_ptr->level;
			power_desc[num].cost = mut_ptr->cost;
			power_desc[num].fail = 100 -
				racial_chance(mut_ptr->level, mut_ptr->stat, mut_ptr->diff);
			power_desc[num].number = mut_ptr->which;
			power_desc[num].power = mut_ptr;
			num++;
		}
	}
	
	/* Not if we don't have any */
	if (num == 0)
	{
		msgf("You have no powers to activate.");
		p_ptr->state.energy_use = 0;
		return;
	}
	
	/* Initialise the options for the menu */
	for (i = 0; i < num; i++)
	{
		strnfmt(buf, 1024, "%-23.23s %2d %4d %3d%%",
				power_desc[i].name,
				power_desc[i].level,
				power_desc[i].cost, power_desc[i].fail);
		
		/* Add option to menu */
		racial_menu[i].text = string_make(buf);
		racial_menu[i].help = NULL;
		racial_menu[i].action = do_cmd_power_aux;
		racial_menu[i].flags = MN_ACTIVE | MN_CLEAR;
	}
	
	/* Make sure the menu is terminated */
	racial_menu[num].text = NULL;
	racial_menu[num].help = NULL;
	racial_menu[num].action = NULL;
	racial_menu[num].flags = 0x00;
	
	
	if (!display_menu(racial_menu, -1, FALSE, display_racial_header, "Use which power?"))
	{
		/* We aborted */
		p_ptr->state.energy_use = 0;
	}
	
	/* Free the allocated strings */
	for (i = 0; i < num; i++)
	{
		string_free(racial_menu[i].text);
	}
}
