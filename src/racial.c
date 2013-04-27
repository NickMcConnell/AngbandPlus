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

    if (stat <= 180)
        stat /= 10;
    else
        stat += 18-180;

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
	s16b fld_idx;
	field_type *f_ptr;

	fld_idx = area(p_ptr->px, p_ptr->py)->fld_idx;

	/* While there are fields in the linked list */
	while (fld_idx)
	{
		f_ptr = &fld_list[fld_idx];

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

			delete_field_idx(fld_idx);

			/* Done */
			return;
		}

		/* Get next field in list */
		fld_idx = f_ptr->next_f_idx;
	}

	/* Nothing to eat */
	msgf("There is no fresh skeleton or corpse here!");
	p_ptr->energy_use = 0;

	/* Done */
	return;
}


/*
 * Note: return value indicates that we have succesfully used the power
 */
bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
    bool use_hp = FALSE;
    int stat = p_ptr->stat_cur[use_stat];

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
		p_ptr->energy_use = 0;
		return FALSE;
	}

	/* Too confused */
	else if (p_ptr->confused)
	{
		msgf("You are too confused to use this power.");
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
	if (p_ptr->confused)
	{
		msgf("You are too confused to use any powers!");
		p_ptr->energy_use = 0;
		return;
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
		p_ptr->energy_use = 0;
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
		p_ptr->energy_use = 0;
	}
	
	/* Free the allocated strings */
	for (i = 0; i < num; i++)
	{
		string_free(racial_menu[i].text);
	}
}
