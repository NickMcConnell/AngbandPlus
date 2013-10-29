/* File: assassin.c
 * Purpose: abilities for the Assassin class
 */

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define ASSASSIN_POIS_DART	0
#define ASSASSIN_FUME_CLOUD	1
#define ASSASSIN_SHADOW_PORT	2
#define ASSASSIN_CORROSION	3
#define ASSASSIN_WITHERING	4
#define ASSASSIN_ASTRAL_VIS	5

struct spellholder assassin_spell_info[] = {
	{ "Poison Dart", 1, 1, 15, "Hurls a bolt of poison." },
	{ "Fume Cloud", 4, 4, 25, "Conjures a cloud of poisonous fumes." },
	{ "Shadow Portal", 8, 8, 35, "Teleports you out of enemies' sight." },
	{ "Corrosion", 12, 12, 45, "Conjures a beam of acidic vapor." },
	{ "Withering", 16, 16, 55, "Conjures a small, powerful ball of nether." },
	{ "Astral Vision", 20, 20, 65, "Lights and maps the entire level." },
};

bool cast_assassin_spell(int spell)
{
	int plev = p_ptr->lev;
	int dir;

	switch(spell)
	{
		case ASSASSIN_POIS_DART:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_POIS, dir, 5 + plev);
			break;
		case ASSASSIN_FUME_CLOUD:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 10 + (plev * 2), 2);
			break;
		case ASSASSIN_SHADOW_PORT:
			(void)teleport_player(25 + plev);
			break;
		case ASSASSIN_CORROSION:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_beam(GF_ACID, dir, plev * 7);
			break;
		case ASSASSIN_WITHERING:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_NETHER, dir, plev * plev / 2, 1);
			break;
		case ASSASSIN_ASTRAL_VIS:
			wiz_light();
			(void)detect_traps(TRUE);
			(void)detect_monsters_normal(TRUE);
			(void)detect_monsters_invis(TRUE);
			break;
	}

	return TRUE;
}

int get_assassin_fail(int spell)
{
	int minfail;
	int plev = p_ptr->lev;
	int chance = assassin_spell_info[spell].fail;

	/* Level adjustment */
	chance -= 3 * (plev - assassin_spell_info[spell].level);

	/* Stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->state.stat_ind[cp_ptr->spell_stat]] - 1);

	/* Get the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[cp_ptr->spell_stat]];
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;

	/* Poisoning makes spells harder */
	if (p_ptr->timed[TMD_POISONED]) chance += 15;

	/*Always leave a 5% chance of working */
	if (chance > 95) chance = 95;

	return chance;
}

void print_assassin_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	prt(" Name    Lv    Mana    Fail    Desc ", y, x);
	for (i = 0; i <= ASSASSIN_ASTRAL_VIS; i++)
	{
		if (assassin_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %16s %4d %4d %4d%% %s",
				I2A(i),
				assassin_spell_info[i].name,
				assassin_spell_info[i].level,
				assassin_spell_info[i].cost,
				get_assassin_fail(i),
				assassin_spell_info[i].desc), y + i + 1, x);
		}
	}

	return;
}

void print_assassin_stat(int spell)
{
	int plev = p_ptr->lev;

	switch(spell)
	{
		case ASSASSIN_POIS_DART:
			msg_format("dam %d", 5 + plev);
			break;
		case ASSASSIN_FUME_CLOUD:
			msg_format("dam %d rad 2", 10 + (plev * 2));
			break;
		case ASSASSIN_SHADOW_PORT:
			msg_format("range %d", 25 + plev);
			break;
		case ASSASSIN_CORROSION:
			msg_format("dam %d", plev * 7);
			break;
		case ASSASSIN_WITHERING:
			msg_format("dam %d rad 1", plev * plev / 2);
			break;
		default:
			break;
	}
}

void do_cmd_assassin(void)
{
	char choice;
	bool casting;

	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}

	screen_save();
	print_assassin_menu();

	while (casting = get_com("Cast which spell? (Esc to exit, Shift+letter for stats)", &choice))
	{
		if (assassin_spell_info[A2I(tolower(choice))].level > p_ptr->lev)
			continue; /* Spell is not yet known */

		if (A2I(tolower(choice)) < 0 || A2I(tolower(choice)) > ASSASSIN_ASTRAL_VIS)
			continue; /* Spell does not exist */

		if (isupper(choice))
		{
			print_assassin_stat(A2I(tolower(choice)));
			continue;
		}

		else if (p_ptr->csp < assassin_spell_info[A2I(choice)].cost)
		{
			msg_print("Not enough mana to cast this spell.");
			continue;
		}

		break;
	}

	screen_load();
	if (casting)
	{
		if (randint1(100) <= get_assassin_fail(A2I(choice)))
		{
			msg_print("You failed to get the spell off!");
			if (randint0(1) == 1) /* 50% chance of poisoning */
			{
				msg_print("The botched spell contaminates you!");
				(void)set_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] + 5 + randint1(25), TRUE);
			}
		}
		else if (!cast_assassin_spell(A2I(choice)))
			return;
		p_ptr->csp -= assassin_spell_info[A2I(choice)].cost;
		p_ptr->redraw |= (PR_MANA);
		p_ptr->energy_use = 100;
	}

	return;
}
