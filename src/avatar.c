/* File: avatar.c
 * Purpose: spells for the Avatar class
 */

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define AV_HOLY_LIGHT	0
#define AV_SENSE_EVIL	1
#define AV_PHASING	2
#define AV_WEIGH_MAGIC	3
#define AV_DIVINE_FURY	4
#define AV_RETRIBUTION	5
#define AV_EXORCISM	6
#define AV_DELUGE	7

struct spellholder avatar_spell_info[] = {
	{ "Holy light", 1, 1, 0, "Grants Heroism and protection from evil." },
	{ "Sense evil", 3, 2, 0, "Detects evil monsters." },
	{ "Phasing", 6, 3, 0, "Teleports you a good distance." },
	{ "Weigh magic", 9, 4, 0, "Identifies an object." },
	{ "Divine fury", 12, 5, 0, "Grants speed and berserk strength." },
	{ "Retribution", 15, 6, 0, "Invokes a beam of holy fire." },
	{ "Mass Exorcism", 18, 7, 0, "Dispels undead and evil monsters." },
	{ "Deluge", 21, 8, 0, "Invokes a terrible flood." },
};

bool cast_avatar_spell(int spell)
{
	int plev = p_ptr->lev;
	int dir;

	switch(spell)
	{
		case AV_HOLY_LIGHT:
			(void)inc_timed(TMD_HERO, randint1(25) + plev, TRUE);
			(void)inc_timed(TMD_PROTEVIL, randint1(25) + plev, TRUE);
			break;
		case AV_SENSE_EVIL:
			(void)detect_monsters_evil(TRUE);
			break;
		case AV_PHASING:
			teleport_player(3 * plev);
			break;
		case AV_WEIGH_MAGIC:
			if (!ident_spell()) return FALSE;
			break;
		case AV_DIVINE_FURY:
			(void)inc_timed(TMD_SHERO, randint1(50) + plev, TRUE);
			(void)inc_timed(TMD_FAST, randint1(50) + plev, TRUE);
			break;
		case AV_RETRIBUTION:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_beam(GF_HOLY_ORB, dir, 6 * plev);
			break;
		case AV_EXORCISM:
			(void)dispel_undead(3 * plev);
			(void)dispel_evil(3 * plev);
			break;
		case AV_DELUGE:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_WATER, dir, 9 * plev, 14);
			break;
	}

	return TRUE;
}

void print_avatar_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	prt("Name    Lv    Mana    Desc ", y, x);
	for (i = 0; i <= AV_DELUGE; i++)
	{
		if (avatar_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %16s %4d %4d %s",
				I2A(i),
				avatar_spell_info[i].name,
				avatar_spell_info[i].level,
				avatar_spell_info[i].cost,
				avatar_spell_info[i].desc), y + i + 1, x);
		}
	}

	return;
}

void print_avatar_stat(int spell)
{
	int plev = p_ptr->lev;

	switch(spell)
	{
		case AV_HOLY_LIGHT:
			msg_format("dur %d+d25", plev);
			break;
		case AV_PHASING:
			msg_format("range %d", 3 * plev);
			break;
		case AV_DIVINE_FURY:
			msg_format("dur %d+d50", plev);
			break;
		case AV_RETRIBUTION:
			msg_format("damage %d", 6 * plev);
			break;
		case AV_EXORCISM:
			msg_format("damage %d * 2", 3 * plev);
			break;
		case AV_DELUGE:
			msg_format("damage %d radius 14", 9 * plev);
			break;
		default:
			break;
	}
}

void do_cmd_avatar()
{
	char choice;
	bool casting;

	/* Not while confused */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}
	/* Nor while scared */
	if (p_ptr->timed[TMD_AFRAID])
	{
		msg_print("You are too scared!");
		return;
	}
	/* Nor while blind */
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are blind!");
		return;
	}

	/* Must save the screen state before invoking the menu */
	screen_save();
	print_avatar_menu();
	while (casting = get_com("Recite which prayer? (Esc to exit, Shift+letter for stats)", &choice))
	{
		if (avatar_spell_info[A2I(tolower(choice))].level > p_ptr->lev)
			continue; /* Spell is not yet allowed */

		if (A2I(tolower(choice)) < 0 || A2I(tolower(choice)) > AV_DELUGE)
			continue; /* Spell does not exist */

		/* Uppercase input gets spell info */
		if (isupper(choice))
		{
			print_avatar_stat(A2I(tolower(choice)));
			continue;
		}

		/* Must have enough mana! */
		else if (p_ptr->csp < avatar_spell_info[A2I(choice)].cost)
		{
			msg_print("Not enough mana to recite this prayer.");
			continue;
		}

		break; /* Player has made a spell choice */
	}

	/* Reload the screen so we can see the spell effect */
	screen_load();
	if (casting)
	{
		if (cast_avatar_spell(A2I(choice)))
		{
			p_ptr->csp -= avatar_spell_info[A2I(choice)].cost;
			p_ptr->redraw |= (PR_MANA);
			p_ptr->energy_use = 100;
		}
	}
	return;
}
