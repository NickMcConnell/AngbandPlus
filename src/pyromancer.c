/* File: pyromancer.c
 * Purpose: spells for the Pyromancer class
 */

/* Pyromancers are bookless spellcasters. Their spells are as follows:
a) Fire bolt (lv 1, 2 sp): conjures a bolt of fire for 4d(plev) damage.
b) Sense heat (lv 4, 3 sp): detects normal (visible) monsters.
c) Fire beam: (lv 8, 8 sp) conjures a beam of fire for 6d(plev) damage.
d) Flicker: (lv 14, 6 sp) teleport up to 5 + (plev)/2 spaces.
e) Fire ball: (lv 22, 12 sp) conjures a ball of fire for 8*(plev) damage.
f) Phoenix fire: (lv 28, 20 sp) heals 500 and conjures a fireball for 4*(plev)
*/

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define PYRO_FIRE_BOLT	0
#define PYRO_SENSE_HEAT	1
#define PYRO_FIRE_BEAM	2
#define PYRO_FLICKER	3
#define PYRO_FIRE_BALL	4
#define PYRO_PHOENIX	5

struct pyro_infoholder {
	char *name;
	int level;
	int cost;
	int fail;
	char *desc;
} pyro_spell_info[] = {
	"Fire bolt", 1, 1, 5, "Conjures a bolt of fire.",
	"Sense heat", 4, 3, 10, "Detects monsters in your vicinity.",
	"Fire beam", 8, 8, 15, "Conjures a beam of fire.",
	"Flicker", 14,  6, 15, "Teleports you a short distance.",
	"Fire ball", 22, 12, 20, "Conjures a powerful ball of fire.",
	"Phoenix fire", 28, 20, 25, "Heals you and burns your enemies."
};

bool cast_pyro_spell(int spell)
{
	int plev = p_ptr->lev;
	int dir;
	
	switch(spell)
	{
		case PYRO_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(4, plev));
			break;
		}
		case PYRO_SENSE_HEAT:
		{
			(void)detect_monsters_normal(TRUE);
			break;
		}
		case PYRO_FIRE_BEAM:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			fire_beam(GF_FIRE, dir, damroll(6, plev));
			break;
		}
		case PYRO_FLICKER:
		{
			teleport_player(5 + plev / 2);
			break;
		}
		case PYRO_FIRE_BALL:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 8 * plev, 4);
			break;
		}
		case PYRO_PHOENIX:
		{
			(void)hp_player(500);
			fire_ball(GF_FIRE, 0, 4 * plev, 24);
			break;
		}
	}

	return TRUE;
}

int get_pyro_fail(int spell)
{
	int minfail;
	int plev = p_ptr->lev;
	int chance = pyro_spell_info[spell].fail;
	
	/* Level adjustment */
	chance -= 3 * (plev - pyro_spell_info[spell].level);
	
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

void print_pyro_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	
	prt(" Lv    Mana    Fail    Info    Desc ", y, x);
	for (i = 0; i <= PYRO_PHOENIX; i++)
	{
		if (pyro_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %s    %d    %d    %d%%    %s",
				I2A(i),
				pyro_spell_info[i].name,
				pyro_spell_info[i].level,
				pyro_spell_info[i].cost,
				get_pyro_fail(i),
				pyro_spell_info[i].desc), y + i + 1, x);
		}
	}
	
	return;
}

/* Print a spell's current stats to the screen */
void print_pyro_stat(int spell)
{
	int plev = p_ptr->lev;

	switch(spell)
	{
		case PYRO_FIRE_BOLT:
		{
			msg_format("damage 4d%d", plev);
			break;
		}
		case PYRO_FIRE_BEAM:
		{
			msg_format("damage 6d%d", plev);
			break;
		}
		case PYRO_FLICKER:
		{
			msg_format("range %d", 5 + plev / 2);
			break;
		}
		case PYRO_FIRE_BALL:
		{
			msg_format("radius 4 damage %d", 8 * plev);
			break; 
		}
		case PYRO_PHOENIX:
		{
			msg_format("heal 500 radius 24 damage %d", 4 * plev);
			break;
		}
		default:
		{
			break;
		}
	}
}

void do_cmd_pyro()
{
	char choice;

	screen_save();
	print_pyro_menu();
	while (get_com("Cast which spell? (Esc to exit, Shift+letter for stats)", &choice))
	{
		if (!choice)
		{
			screen_load();
			return;
		}
		
		if (isupper(choice) && A2I(tolower(choice)) <= PYRO_PHOENIX)
		{
			print_pyro_stat(A2I(tolower(choice)));
			continue;
		}
		
		else if (A2I(choice) < 0 || A2I(choice) > PYRO_PHOENIX)
		{
			continue;
		}
		
		if (pyro_spell_info[A2I(choice)].level > p_ptr->lev)
		{
			continue;
		}
		
		else if (p_ptr->csp < pyro_spell_info[A2I(choice)].cost)
		{
			msg_print("Not enough mana to cast this spell.");
			continue;
		}

		screen_load();
		if (randint1(100) > get_pyro_fail(A2I(choice)))
		{
			if (cast_pyro_spell(A2I(choice)))
			{
				p_ptr->csp -= pyro_spell_info[A2I(choice)].cost;
				p_ptr->redraw |= (PR_MANA);
				p_ptr->energy_use = 100;
			}
		}
		else
		{
			msg_print("You failed to get the spell off!");
		}
		
		return;
	}

	screen_load();
	return;
}
