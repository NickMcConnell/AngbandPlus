/* File: reaper.c
 * Purpose: abilities for the Reaper class
 */

/* Reapers are bookless priests. Their abilities are as follows:
 * a) Manaburst (lv 5, 3 sp): conjures 1+(plev)/6 bolts of mana for
 *  10+(plev) damage each.
 * b) Berserk (lv 10, 6 sp): grants speed and berserk strength.
 * c) Darksight (lv 15, 9 sp): maps the surrounding area and detects
 *  monsters.
 * d) Indominable (lv 20, 12 sp): Grants resistance to confusion,
 *  poison, and the elements.
 *
 * Reapers gain a few HP and SP for every foe slain.
 */

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define REAPER_MANABURST	0
#define REAPER_BERSERK		1
#define REAPER_DARKSIGHT	2
#define REAPER_INDOMINABLE	3

struct reaper_infoholder {
	char *name;
	int level;
	int cost;
	int fail;
	char *desc;
} reaper_spell_info[] = {
	"Manaburst", 5, 3, 10, "Conjures a barrage of mana bolts.",
	"Berserk", 10, 6, 20, "Grants increased speed and strength.",
	"Darksight", 15, 9, 30, "Allows you to see through walls.",
	"Indominable", 20, 12, 40, "Protects you from physical and psychic harm."
};

bool cast_reaper_spell(int spell)
{
	int plev = p_ptr->lev;
	int dir;
	int i;

	switch(spell)
	{
		case REAPER_MANABURST:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (i = 0; i < 1 + (plev / 6); i++)
			{
				fire_bolt(GF_MANA, dir, 10 + plev);
			}
			break;
		}
		case REAPER_BERSERK:
		{
			(void)inc_timed(TMD_SHERO, randint1(25) + plev, TRUE);
			(void)inc_timed(TMD_FAST, randint1(25) + plev, TRUE);
			break;
		}
		case REAPER_DARKSIGHT:
		{
			(void)map_area();
			(void)detect_monsters_normal(TRUE);
			(void)detect_monsters_invis(TRUE);
			break;
		}
		case REAPER_INDOMINABLE:
		{
			(void)inc_timed(TMD_OPP_FIRE, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_OPP_COLD, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_OPP_ACID, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_OPP_ELEC, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_OPP_CONF, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_OPP_POIS, randint1(50) + plev, TRUE);
			break;
		}
	}

	return TRUE;
}

int get_reaper_fail(int spell)
{
	int minfail;
	int plev = p_ptr->lev;
	int chance = reaper_spell_info[spell].fail;

	/* Level adjustment */
	chance -= 3 * (plev - reaper_spell_info[spell].level);

	/* Stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->state.stat_ind[cp_ptr->spell_stat]] - 1);

	/* Get the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[cp_ptr->spell_stat]];
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Always leave a 5% chance of working */
	if (chance > 95) chance = 95;

	return chance;
}

void print_reaper_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	prt(" Name    Lv    Mana    Fail    Desc ", y, x);
	for (i = 0; i <= REAPER_INDOMINABLE; i++)
	{
		if(reaper_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %16s %4d %4d %4d%% %s",
				I2A(i),
				reaper_spell_info[i].name,
				reaper_spell_info[i].level,
				reaper_spell_info[i].cost,
				get_reaper_fail(i),
				reaper_spell_info[i].desc), y + i + 1, x);

		}
	}

	return;
}

void print_reaper_stat(int spell)
{
	int plev = p_ptr->lev;
	
	switch(spell)
	{
		case REAPER_MANABURST:
			msg_format("dam %d*%d", 10 + plev, 1 + plev / 6);
			break;
		case REAPER_BERSERK:
			msg_format("dur %d+d25", plev);
			break;
		case REAPER_INDOMINABLE:
			msg_format("dur %d+d50", plev);
			break;
		default:
			break;
	}
}

void do_cmd_reaper(void)
{
	char choice;

	screen_save();
	print_reaper_menu();
	
	while(get_com("Cast which spell? (Esc to exit, Shift+letter for stats)", &choice))
	{
		if (!choice)
		{
			screen_load();
			return;
		}
		
		if (reaper_spell_info[A2I(tolower(choice))].level > p_ptr->lev)
			continue;
		
		if (isupper(choice) && A2I(tolower(choice)) <= REAPER_INDOMINABLE)
		{
			print_reaper_stat(A2I(tolower(choice)));
			continue;
		}
		
		else if (A2I(choice) < 0 || A2I(choice) > REAPER_INDOMINABLE)
			continue;
		
		else if (p_ptr->csp < reaper_spell_info[A2I(choice)].cost)
		{
			msg_print("Not enough mana to cast this spell.");
			continue;
		}
		
		screen_load();
		
		if (randint1(100) > get_reaper_fail(A2I(choice)))
		{
			if (cast_reaper_spell(A2I(choice)))
			{
				p_ptr->csp -= reaper_spell_info[A2I(choice)].cost;
				p_ptr->redraw |= (PR_MANA);
			}
		}
		else
			msg_print("You failed to get the spell off!");
		
		p_ptr->energy_use = 100;
		return;
	}
	
	screen_load();
	return;
}
