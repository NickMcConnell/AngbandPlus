/* File: avatar.c */

/*
* Purpose: Enable an Ultima IV style "avatar" game where you try to
* achieve perfection in various virtues.
*
* Topi Ylinen 1998
*
*/

/*
* Copyright (c) 1989 James E. Wilson, Christopher J. Stuart
*
* This software may be copied and distributed for educational, research, and
* not for profit purposes provided that this copyright and statement are
* included in all such copies.
*/

#include "angband.h"


/* The names of the virtues */
cptr virtue[MAX_VIRTUE] =
{
	"Compassion",
	"Honour",
	"Justice",
	"Sacrifice",
	"Knowledge",
	"Faith",
	"Enlightenment",
	"Mysticism",
	"Chance",
	"Nature",
	"Harmony",
	"Vitality",
	"Unlife",
	"Patience",
	"Temperance",
	"Diligence",
	"Valour",
	"Individualism",
};


/* Aux function */
static bool exist_virtue(int type)
{
	int i;

	/* Search */
	for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
	{
		if (p_ptr->vir_types[i] == type) return TRUE;
	}

	/* No match */
	return FALSE;
}


/* Aux function */
static void get_random_virtue(int which)
{
	int type = 0;

	/* Randomly choose a type */
	while (!type || exist_virtue(type))
	{
		switch (randint1(29))
		{
			case 1:  case 2:  case 3:
				type = V_SACRIFICE;
				break;
			case 4:  case 5:  case 6:
				type = V_COMPASSION;
				break;
			case 7:  case 8:  case 9:  case 10:  case 11:  case 12:
				type = V_VALOUR;
				break;
			case 13:  case 14:  case 15:  case 16:  case 17:
				type = V_HONOUR;
				break;
			case 18:  case 19:  case 20:  case 21:
				type = V_JUSTICE;
				break;
			case 22:  case 23:
				type = V_TEMPERANCE;
				break;
			case 24:  case 25:
				type = V_HARMONY;
				break;
			case 26:  case 27:  case 28:
				type = V_PATIENCE;
				break;
			default:
				type = V_DILIGENCE;
		}
	}

	/* Chosen */
	p_ptr->vir_types[which] = type;
}


/*
* Select virtues & reset values for a new character
*/
void get_virtues(void)
{
	int i, j;

	/* Reset */
	for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
	{
		p_ptr->virtues[i] = 0;
		p_ptr->vir_types[i] = 0;
	}

	i = 0;

	/* Get pre-defined types */
	/* One or more virtues based on class */
	switch (p_ptr->rp.pclass)
	{
		case CLASS_WARRIOR:
			p_ptr->vir_types[i++] = V_VALOUR;
			p_ptr->vir_types[i++] = V_HONOUR;
			break;
		case CLASS_MAGE:
			p_ptr->vir_types[i++] = V_KNOWLEDGE;
			p_ptr->vir_types[i++] = V_ENCHANT;
			break;
		case CLASS_PRIEST:
			p_ptr->vir_types[i++] = V_FAITH;
			p_ptr->vir_types[i++] = V_TEMPERANCE;
			break;
		case CLASS_ROGUE:
			p_ptr->vir_types[i++] = V_HONOUR;
			break;
		case CLASS_RANGER:
			p_ptr->vir_types[i++] = V_NATURE;
			p_ptr->vir_types[i++] = V_TEMPERANCE;
			break;
		case CLASS_PALADIN:
			p_ptr->vir_types[i++] = V_JUSTICE;
			p_ptr->vir_types[i++] = V_VALOUR;
			p_ptr->vir_types[i++] = V_HONOUR;
			p_ptr->vir_types[i++] = V_FAITH;
			break;
		case CLASS_WARRIOR_MAGE:
			p_ptr->vir_types[i++] = V_ENCHANT;
			p_ptr->vir_types[i++] = V_VALOUR;
			break;
		case CLASS_CHAOS_WARRIOR:
			p_ptr->vir_types[i++] = V_CHANCE;
			p_ptr->vir_types[i++] = V_INDIVIDUALISM;
			break;
		case CLASS_MONK:
			p_ptr->vir_types[i++] = V_FAITH;
			p_ptr->vir_types[i++] = V_HARMONY;
			p_ptr->vir_types[i++] = V_TEMPERANCE;
			p_ptr->vir_types[i++] = V_PATIENCE;
			break;
		case CLASS_MINDCRAFTER:
			p_ptr->vir_types[i++] = V_HARMONY;
			p_ptr->vir_types[i++] = V_ENLIGHTEN;
			p_ptr->vir_types[i++] = V_PATIENCE;
			break;
		case CLASS_HIGH_MAGE:
			p_ptr->vir_types[i++] = V_ENLIGHTEN;
			p_ptr->vir_types[i++] = V_ENCHANT;
			p_ptr->vir_types[i++] = V_KNOWLEDGE;
			break;
	};


	/* Get one virtue based on race */
	switch (p_ptr->rp.prace)
	{
		case RACE_HUMAN:  case RACE_HALF_ELF:
			p_ptr->vir_types[i++] = V_INDIVIDUALISM;
			break;
		case RACE_ELF:  case RACE_SPRITE:
			p_ptr->vir_types[i++] = V_NATURE;
			break;
		case RACE_HOBBIT:  case RACE_HALF_OGRE:
			p_ptr->vir_types[i++] = V_TEMPERANCE;
			break;
		case RACE_DWARF:  case RACE_KLACKON:
			p_ptr->vir_types[i++] = V_DILIGENCE;
			break;
		case RACE_GNOME:  case RACE_CYCLOPS:
			p_ptr->vir_types[i++] = V_KNOWLEDGE;
			break;
		case RACE_HALF_ORC:  case RACE_AMBERITE:  case RACE_KOBOLD:
			p_ptr->vir_types[i++] = V_HONOUR;
			break;
		case RACE_HALF_TROLL:  case RACE_BARBARIAN:
			p_ptr->vir_types[i++] = V_VALOUR;
			break;
		case RACE_HIGH_ELF:
			p_ptr->vir_types[i++] = V_VITALITY;
			break;
		case RACE_HALF_GIANT:  case RACE_GOLEM:
			p_ptr->vir_types[i++] = V_JUSTICE;
			break;
		case RACE_HALF_TITAN:
			p_ptr->vir_types[i++] = V_HARMONY;
			break;
		case RACE_YEEK:
			p_ptr->vir_types[i++] = V_SACRIFICE;
			break;
		case RACE_MIND_FLAYER:
			p_ptr->vir_types[i++] = V_ENLIGHTEN;
			break;
		case RACE_DARK_ELF:  case RACE_DRACONIAN:
			p_ptr->vir_types[i++] = V_ENCHANT;
			break;
		case RACE_NIBELUNG:
			p_ptr->vir_types[i++] = V_PATIENCE;
			break;
		case RACE_IMP:
			p_ptr->vir_types[i++] = V_FAITH;
			break;
		case RACE_ZOMBIE:  case RACE_SKELETON:  case RACE_VAMPIRE:
		case RACE_SPECTRE:  case RACE_GHOUL:
			p_ptr->vir_types[i++] = V_UNLIFE;
			break;
		case RACE_BEASTMAN:
			p_ptr->vir_types[i++] = V_CHANCE;
			break;
	}

	/* Get a virtue for r[0].realm */
	if (p_ptr->spell.r[0].realm)
	{
		switch (p_ptr->spell.r[0].realm)
		{
			case 1:
				if (exist_virtue(V_VITALITY))
					p_ptr->vir_types[i++] = V_COMPASSION;
				else
					p_ptr->vir_types[i++] = V_VITALITY;
				break;
			case 2:
				if (exist_virtue(V_ENCHANT))
					p_ptr->vir_types[i++] = V_KNOWLEDGE;
				else
					p_ptr->vir_types[i++] = V_ENCHANT;
				break;
			case 3:
				if (exist_virtue(V_NATURE))
					p_ptr->vir_types[i++] = V_HARMONY;
				else
					p_ptr->vir_types[i++] = V_NATURE;
				break;
			case 4:
				if (exist_virtue(V_CHANCE))
					p_ptr->vir_types[i++] = V_INDIVIDUALISM;
				else
					p_ptr->vir_types[i++] = V_CHANCE;
				break;
			case 5:
				p_ptr->vir_types[i++] = V_UNLIFE;
				break;
			case 6:
				p_ptr->vir_types[i++] = V_KNOWLEDGE;
				break;
			case 7:
				break;
		};
	}

	/* Get a virtue for r[1].realm */
	if (p_ptr->spell.r[1].realm)
	{
		switch (p_ptr->spell.r[1].realm)
		{
			case 1:
				if (exist_virtue(V_VITALITY))
					p_ptr->vir_types[i++] = V_COMPASSION;
				else
					p_ptr->vir_types[i++] = V_VITALITY;
				break;
			case 2:
				if (exist_virtue(V_ENCHANT))
					p_ptr->vir_types[i++] = V_KNOWLEDGE;
				else
					p_ptr->vir_types[i++] = V_ENCHANT;
				break;
			case 3:
				if (exist_virtue(V_NATURE))
					p_ptr->vir_types[i++] = V_HARMONY;
				else
					p_ptr->vir_types[i++] = V_NATURE;
				break;
			case 4:
				if (exist_virtue(V_CHANCE))
					p_ptr->vir_types[i++] = V_INDIVIDUALISM;
				else
					p_ptr->vir_types[i++] = V_CHANCE;
				break;
			case 5:
				p_ptr->vir_types[i++] = V_UNLIFE;
				break;
			case 6:
				p_ptr->vir_types[i++] = V_KNOWLEDGE;
				break;
			case 7:
				break;
		};
	}

	/* Eliminate doubles */
	for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
	{
		for (j = i + 1; j < MAX_PLAYER_VIRTUES; j++)
		{
			if ((p_ptr->vir_types[j] != 0) &&
				(p_ptr->vir_types[j] == p_ptr->vir_types[i]))
				p_ptr->vir_types[j] = 0;
		}
	}

	/* Fill in the blanks */
	for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
	{
		if (p_ptr->vir_types[i] == 0) get_random_virtue(i);
	}
}


void chg_virtue(int virtue, int amount)
{
	int i;

	for (i = 0; i < MAX_PLAYER_VIRTUES; i++)
	{
		if (p_ptr->vir_types[i] == virtue)
		{
			if (amount > 0)
			{
				if (amount + p_ptr->virtues[i] > 125)
					p_ptr->virtues[i] = 125;
				else
					p_ptr->virtues[i] = p_ptr->virtues[i] + amount;
			}
			else
			{
				if (amount + p_ptr->virtues[i] < -125)
					p_ptr->virtues[i] = -125;
				else
					p_ptr->virtues[i] = p_ptr->virtues[i] + amount;
			}
			return;
		}
	}
}


void dump_virtues(FILE *OutFile)
{
	int v_nr;

	if (!OutFile) return;

	for (v_nr = 0; v_nr < MAX_PLAYER_VIRTUES; v_nr++)
	{
		char virt_name[20];

		int tester = p_ptr->virtues[v_nr];

		strcpy(virt_name, virtue[(p_ptr->vir_types[v_nr]) - 1]);

		if ((p_ptr->vir_types[v_nr] == 0) ||
			(p_ptr->vir_types[v_nr] > MAX_VIRTUE))
			froff(OutFile, "Oops. No info about %s.", virt_name);
		else if (tester < -100)
			froff(OutFile, "You are the polar opposite of %s.", virt_name);
		else if (tester < -80)
			froff(OutFile, "You are an arch-enemy of %s.", virt_name);
		else if (tester < -60)
			froff(OutFile, "You are a bitter enemy of %s.", virt_name);
		else if (tester < -40)
			froff(OutFile, "You are an enemy of %s.", virt_name);
		else if (tester < -20)
			froff(OutFile, "You have sinned against %s.", virt_name);
		else if (tester < 0)
			froff(OutFile, "You have strayed from the path of %s.",
					virt_name);
		else if (tester == 0)
			froff(OutFile, "You are neutral to %s.", virt_name);
		else if (tester < 20)
			froff(OutFile, "You are somewhat virtuous in %s.", virt_name);
		else if (tester < 40)
			froff(OutFile, "You are virtuous in %s.", virt_name);
		else if (tester < 60)
			froff(OutFile, "You are very virtuous in %s.", virt_name);
		else if (tester < 80)
			froff(OutFile, "You are a champion of %s.", virt_name);
		else if (tester < 100)
			froff(OutFile, "You are a great champion of %s.", virt_name);
		else
			froff(OutFile, "You are the living embodiment of %s.", virt_name);

		froff(OutFile, "\n");
	}

	if (p_ptr->state.wizard)
		froff(OutFile, "Your overall alignment is %ld.\n", p_ptr->align);
}
