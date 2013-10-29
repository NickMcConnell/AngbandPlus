/* File: pyromancer.c
 * Purpose: spells for the Pyromancer class
 */

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define PYRO_FIRE_BOLT	0
#define PYRO_SENSE_HEAT	1
#define PYRO_PLAS_BOLT	2
#define PYRO_FLICKER	3
#define PYRO_FIRE_BALL	4
#define PYRO_HEAT_RAYS	5
#define PYRO_KNOWLEDGE	6
#define PYRO_MANA_BOLT	7

struct spellholder pyro_spell_info[] = {
	/* Name		level	cost	fail		description */
	{ "Fire bolt", 1, 1, 5, "Conjures a bolt of fire." },
	{ "Sense heat", 4, 3, 10, "Detects monsters in your vicinity." },
	{ "Plasma Bolt", 8, 8, 15, "Conjures a bolt of plasma." },
	{ "Flicker", 14,  6, 15, "Teleports you a short distance." },
	{ "Fire ball", 22, 12, 20, "Conjures a powerful ball of fire." },
	{ "Heat Rays", 28, 20, 25, "Burns all enemies in your sight." },
	{ "Knowledge", 32, 18, 30, "Lights and maps the whole level." },
	{ "Mana Bolt", 36, 24, 35, "Summons a bolt of pure energy." },
};

/* Invokes spell effects. Returns true if succeeded, false if aborted by player. */
bool cast_pyro_spell(int spell)
{
	int plev = p_ptr->lev;
	int dir;
	
	switch(spell)
	{
		case PYRO_FIRE_BOLT:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(4, plev));
			break;
		case PYRO_SENSE_HEAT:
			(void)detect_monsters_normal(TRUE);
			break;
		case PYRO_PLAS_BOLT:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_PLASMA, dir, damroll(6, plev));
			break;
		case PYRO_FLICKER:
			teleport_player(5 + plev / 2);
			break;
		case PYRO_FIRE_BALL:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 8 * plev, 4);
			break;
		case PYRO_HEAT_RAYS:
			(void)project_los(GF_PLASMA, 4 * plev, FALSE);
			break;
		case PYRO_KNOWLEDGE:
			wiz_light();
			break;
		case PYRO_MANA_BOLT:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_MANA, dir, damroll(10, plev));
			break;
	}

	return TRUE;
}

/* Calculates the current failure rate of a spell. */
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

/* Prints the text menu for choosing a spell. */
void print_pyro_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	
	prt("Name    Lv    Mana    Fail    Desc ", y, x);
	for (i = 0; i <= PYRO_MANA_BOLT; i++)
	{
		if (pyro_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %16s %4d %4d %4d%% %s",
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

/* Prints a spell's current stats in the line above the text menu. */
void print_pyro_stat(int spell)
{
	int plev = p_ptr->lev;

	switch(spell)
	{
		case PYRO_FIRE_BOLT:
			msg_format("damage 4d%d", plev);
			break;
		case PYRO_PLAS_BOLT:
			msg_format("damage 6d%d", plev);
			break;
		case PYRO_FLICKER:
			msg_format("range %d", 5 + plev / 2);
			break;
		case PYRO_FIRE_BALL:
			msg_format("radius 4 damage %d", 8 * plev);
			break; 
		case PYRO_HEAT_RAYS:
			msg_format("damage %d", 4 * plev);
			break;
		case PYRO_MANA_BOLT:
			msg_format("damage 10d%d", plev);
		default:
			break;
	}
}

/* Ties all the above functionality together, and checks for failure, etc. on casting. */
void do_cmd_pyro()
{
	char choice; /* Whichever character the player enters */
	bool casting; /* Whether the player is casting the spell or aborting */

	/* Check for blindness and confusion, which prevent casting */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are blind!");
		return;
	}

	screen_save(); /* Screen state must be saved first to prevent display corruption */
	print_pyro_menu(); /* Display the text menu */
	while (casting = get_com("Cast which spell? (Esc to exit, Shift+letter for stats)", &choice))
	{
		if (pyro_spell_info[A2I(tolower(choice))].level > p_ptr->lev)
			continue; /* Spell is not yet allowed */

		if (A2I(tolower(choice)) < 0 || A2I(tolower(choice)) > PYRO_MANA_BOLT)
			continue; /* Spell does not exist */
			
		if (isupper(choice))
		{
			print_pyro_stat(A2I(tolower(choice)));
			continue;
		}
		
		else if (p_ptr->csp < pyro_spell_info[A2I(choice)].cost)
		{
			/* Can't cast without enough SP */
			msg_print("Not enough mana to cast this spell.");
			continue;
		}

		break;
	}

	screen_load(); /* Reload the screen state, so we can show the spell effect */
	if (casting) /* Player did not abort */
	{
		/* Possibly fail the spell */
		if (randint1(100) <= get_pyro_fail(A2I(choice)))
			msg_print("You failed to get the spell off!");
		else if (!cast_pyro_spell(A2I(choice)))
			return; /* Allow the player to abort before wasting a turn */
		p_ptr->csp -= pyro_spell_info[A2I(choice)].cost; /* Use some SP */
		p_ptr->redraw |= (PR_MANA); /* Redraw SP counter */
		p_ptr->energy_use = 100; /* Use a turn */
	}
	return;
}
