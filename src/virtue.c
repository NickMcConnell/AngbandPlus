/* Virtue System from Hengband, but re-written for clarity */

#include "angband.h"

static cptr _names[VIRTUE_MAX] =
{
    "Unknown",
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

static cptr _good_msg[VIRTUE_MAX] = 
{
	"Bug: Invalid Virtue",
	"That was very compassionate.",
	"That was rather honorable.",
	"Your sense of justice is impressive.",
	"What a noble sacrifice!",
	"You feel more knowledgeable.",
	"What an act of faith!",
	"That was quite enlightening.",
	"You are a true mystic!",
	"A bold risk indeed!",
	"You feel more in tune with the world around you.",
	"That was very harmonious.",
	"How vigorous!",
	"You commune with the undead.",
	"Your patience is astounding!",
	"Moderation in all things.",
	"That was very diligent.",
	"How heroic!",
	"You walk your own road in life.",
};

static cptr _bad_msg[VIRTUE_MAX] = 
{
	"Bug: Invalid Virtue",
	"Stop, you brute!",
	"That was rather dishonorable.",
	"How unfair!",
	"You are straying from the path of sacrifice.",
	"You feel less knowledgeable.",
	"That shows a complete lack of faith on your part.",
	"That was not very enlightening.",
	"You are not very mystical.",
	"Perhaps you are too cautious?",
	"You feel out of touch with natural world.",
	"Your actions are disharmonious.",
	"That was rather lethargic.",
	"You are straying from the path of unlife.",
	"How impatient!",
	"Gluttony is very unbecoming.",
	"How lazy!",
	"You dastardly villain!",
	"You are straying from the path of individualism.",
};

static bool _is_valid_index(int idx)
{
	if (0 <= idx && idx < 8)
		return TRUE;
	return FALSE;
}

static bool _is_valid_virtue(int which)
{
	if (0 < which && which < VIRTUE_MAX)
		return TRUE;
	return FALSE;
}

static s16b _random_virtue(void)
{
	int result = VIRTUE_NONE;

	while (!_is_valid_virtue(result) || virtue_present(result))
	{
		switch (randint1(29))
		{
		case 1: case 2: case 3:
			result = VIRTUE_SACRIFICE;
			break;
		case 4: case 5: case 6:
			result = VIRTUE_COMPASSION;
			break;
		case 7: case 8: case 9: case 10: case 11: case 12:
			result = VIRTUE_VALOUR;
			break;
		case 13: case 14: case 15: case 16: case 17:
			result = VIRTUE_HONOUR;
			break;
		case 18: case 19: case 20: case 21:
			result = VIRTUE_JUSTICE;
			break;
		case 22: case 23:
			result = VIRTUE_TEMPERANCE;
			break;
		case 24: case 25:
			result = VIRTUE_HARMONY;
			break;
		case 26: case 27: case 28:
			result = VIRTUE_PATIENCE;
			break;
		default:
			result = VIRTUE_DILIGENCE;
			break;
		}
	}
	return result;
}

static s16b _realm_virtue(int realm)
{
	switch (realm)
	{
	case REALM_LIFE:
		if (virtue_present(VIRTUE_VITALITY)) return VIRTUE_TEMPERANCE;
		else return VIRTUE_VITALITY;
	case REALM_SORCERY:
		if (virtue_present(VIRTUE_KNOWLEDGE)) return VIRTUE_ENCHANTMENT;
		else return VIRTUE_KNOWLEDGE;
	case REALM_NATURE:
		if (virtue_present(VIRTUE_NATURE)) return VIRTUE_HARMONY;
		else return VIRTUE_NATURE;
	case REALM_CHAOS:
		if (virtue_present(VIRTUE_CHANCE)) return VIRTUE_INDIVIDUALISM;
		else return VIRTUE_CHANCE;
	case REALM_DEATH: case REALM_NECROMANCY:
		return VIRTUE_UNLIFE;
	case REALM_TRUMP:
		return VIRTUE_KNOWLEDGE;
	case REALM_ARCANE:
		return VIRTUE_NONE;
	case REALM_CRAFT:
		if (virtue_present(VIRTUE_ENCHANTMENT)) return VIRTUE_INDIVIDUALISM;
		else return VIRTUE_ENCHANTMENT;
	case REALM_DAEMON:
		if (virtue_present(VIRTUE_JUSTICE)) return VIRTUE_FAITH;
		else return VIRTUE_JUSTICE;
	case REALM_CRUSADE:
		if (virtue_present(VIRTUE_JUSTICE)) return VIRTUE_HONOUR;
		else return VIRTUE_JUSTICE;
	case REALM_HEX:
		if (virtue_present(VIRTUE_COMPASSION)) return VIRTUE_JUSTICE;
		else return VIRTUE_COMPASSION;
	}

	return VIRTUE_NONE;
}

cptr virtue_name(int which)
{
	if (which > 0 && which < VIRTUE_MAX)
		return _names[which];
	return _names[VIRTUE_NONE];
}

int virtue_find(int which)
{
	int i;
	for (i = 0; i < 8; i++)
		if (p_ptr->vir_types[i] == which) return i;

	return -1;
}

bool virtue_present(int which)
{
	return _is_valid_index(virtue_find(which));
}

int virtue_current(int which)
{
	if (enable_virtues)
	{
		int idx = virtue_find(which);
		if (_is_valid_index(idx))
			return p_ptr->virtues[idx];
	}
	return 0;
}

void virtue_init(void)
{
	int i = 0;

	/* Reset */
	for (i = 0; i < 8; i++)
	{
		p_ptr->virtues[i] = 0;
		p_ptr->vir_types[i] = VIRTUE_NONE;
	}

	i = 0;

	/* Get pre-defined types */
	/* 1 or more virtues based on class */
	switch (p_ptr->pclass)
	{
	case CLASS_WARRIOR:
	case CLASS_SAMURAI:
	case CLASS_MAULER:
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		p_ptr->vir_types[i++] = VIRTUE_HONOUR;
		break;
	case CLASS_MAGE:
	case CLASS_BLOOD_MAGE:
	case CLASS_NECROMANCER:
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		p_ptr->vir_types[i++] = VIRTUE_ENCHANTMENT;
		break;
	case CLASS_PRIEST:
		p_ptr->vir_types[i++] = VIRTUE_FAITH;
		p_ptr->vir_types[i++] = VIRTUE_TEMPERANCE;
		break;
	case CLASS_ROGUE:
	case CLASS_SNIPER:
	case CLASS_SCOUT:
		p_ptr->vir_types[i++] = VIRTUE_HONOUR;
		break;
	case CLASS_RANGER:
	case CLASS_ARCHER:
		p_ptr->vir_types[i++] = VIRTUE_NATURE;
		p_ptr->vir_types[i++] = VIRTUE_TEMPERANCE;
		break;
	case CLASS_PALADIN:
		p_ptr->vir_types[i++] = VIRTUE_JUSTICE;
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		p_ptr->vir_types[i++] = VIRTUE_HONOUR;
		p_ptr->vir_types[i++] = VIRTUE_FAITH;
		break;
	case CLASS_WARRIOR_MAGE:
	case CLASS_RED_MAGE:
		p_ptr->vir_types[i++] = VIRTUE_ENCHANTMENT;
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		break;
	case CLASS_CHAOS_WARRIOR:
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		p_ptr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
		break;
	case CLASS_MONK:
	case CLASS_FORCETRAINER:
		p_ptr->vir_types[i++] = VIRTUE_FAITH;
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		p_ptr->vir_types[i++] = VIRTUE_TEMPERANCE;
		p_ptr->vir_types[i++] = VIRTUE_PATIENCE;
		break;
	case CLASS_MYSTIC:
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		p_ptr->vir_types[i++] = VIRTUE_TEMPERANCE;
		p_ptr->vir_types[i++] = VIRTUE_PATIENCE;
		break;
	case CLASS_MINDCRAFTER:
	case CLASS_MIRROR_MASTER:
	case CLASS_PSION:
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		p_ptr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
		p_ptr->vir_types[i++] = VIRTUE_PATIENCE;
		break;
	case CLASS_HIGH_MAGE:
	case CLASS_SORCERER:
		p_ptr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
		p_ptr->vir_types[i++] = VIRTUE_ENCHANTMENT;
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		break;
	case CLASS_TOURIST:
		p_ptr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		break;
	case CLASS_IMITATOR:
	case CLASS_WILD_TALENT:
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		break;
	case CLASS_BLUE_MAGE:
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		break;
	case CLASS_BEASTMASTER:
		p_ptr->vir_types[i++] = VIRTUE_NATURE;
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		p_ptr->vir_types[i++] = VIRTUE_VITALITY;
		break;
	case CLASS_MAGIC_EATER:
		p_ptr->vir_types[i++] = VIRTUE_ENCHANTMENT;
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		break;
	case CLASS_BARD:
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		p_ptr->vir_types[i++] = VIRTUE_COMPASSION;
		break;
	case CLASS_CAVALRY:
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		break;
	case CLASS_BERSERKER:
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		p_ptr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
		break;
	case CLASS_WEAPONSMITH:
		p_ptr->vir_types[i++] = VIRTUE_HONOUR;
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		break;
	case CLASS_NINJA:
		p_ptr->vir_types[i++] = VIRTUE_PATIENCE;
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		p_ptr->vir_types[i++] = VIRTUE_FAITH;
		p_ptr->vir_types[i++] = VIRTUE_UNLIFE;
		break;
	};

	/* Get one virtue based on race */
	switch (p_ptr->prace)
	{
	case RACE_HUMAN: case RACE_DEMIGOD: case RACE_TONBERRY: case RACE_DUNADAN:
		p_ptr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
		break;
	case RACE_SPRITE: case RACE_ENT:
		p_ptr->vir_types[i++] = VIRTUE_NATURE;
		break;
	case RACE_HOBBIT: case RACE_HALF_OGRE:
		p_ptr->vir_types[i++] = VIRTUE_TEMPERANCE;
		break;
	case RACE_DWARF: case RACE_KLACKON: case RACE_ANDROID:
		p_ptr->vir_types[i++] = VIRTUE_DILIGENCE;
		break;
	case RACE_GNOME: case RACE_CYCLOPS:
		p_ptr->vir_types[i++] = VIRTUE_KNOWLEDGE;
		break;
	case RACE_SNOTLING: case RACE_AMBERITE: case RACE_KOBOLD:
		p_ptr->vir_types[i++] = VIRTUE_HONOUR;
		break;
	case RACE_HALF_TROLL: case RACE_BARBARIAN:
		p_ptr->vir_types[i++] = VIRTUE_VALOUR;
		break;
	case RACE_HIGH_ELF: case RACE_KUTAR:
		p_ptr->vir_types[i++] = VIRTUE_VITALITY;
		break;
	case RACE_HALF_GIANT: case RACE_GOLEM: case RACE_ARCHON: case RACE_BALROG:
		p_ptr->vir_types[i++] = VIRTUE_JUSTICE;
		break;
	case RACE_HALF_TITAN:
		p_ptr->vir_types[i++] = VIRTUE_HARMONY;
		break;
	case RACE_YEEK:
		p_ptr->vir_types[i++] = VIRTUE_SACRIFICE;
		break;
	case RACE_MIND_FLAYER:
		p_ptr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
		break;
	case RACE_DARK_ELF: case RACE_DRACONIAN: case RACE_SHADOW_FAIRY:
		p_ptr->vir_types[i++] = VIRTUE_ENCHANTMENT;
		break;
	case RACE_NIBELUNG:
		p_ptr->vir_types[i++] = VIRTUE_PATIENCE;
		break;
	case RACE_IMP:
		p_ptr->vir_types[i++] = VIRTUE_FAITH;
		break;
	case RACE_ZOMBIE: case RACE_SKELETON:
	case RACE_VAMPIRE: case RACE_SPECTRE:
		p_ptr->vir_types[i++] = VIRTUE_UNLIFE;
		break;
	case RACE_BEASTMAN:
		p_ptr->vir_types[i++] = VIRTUE_CHANCE;
		break;
	}

	/* Get a virtue for realms */
	if (p_ptr->realm1)
	{
		s16b v = _realm_virtue(p_ptr->realm1);
		if (_is_valid_virtue(v)) 
			p_ptr->vir_types[i++] = v;
	}
	if (p_ptr->realm2)
	{
		s16b v = _realm_virtue(p_ptr->realm2);
		if (_is_valid_virtue(v)) 
			p_ptr->vir_types[i++] = v;
	}

	/* Eliminate doubles */
	for (i = 0; i < 8; i++)
	{
		if (_is_valid_virtue(p_ptr->vir_types[i]))
		{
			int j;
			for (j = i + 1; j < 8; j++)
			{
				if ( _is_valid_virtue(p_ptr->vir_types[j])
				  && p_ptr->vir_types[j] == p_ptr->vir_types[i] )
				{
					p_ptr->vir_types[j] = 0;
				}
			}
		}
	}

	/* Fill in the blanks */
	for (i = 0; i < 8; i++)
	{
		if (!_is_valid_virtue(p_ptr->vir_types[i])) 
			p_ptr->vir_types[i] = _random_virtue();
	}
}


void virtue_add(int which, int amount)
{
	int idx;
	
	if (!enable_virtues) 
		return;
	
	idx = virtue_find(which);
	if (!_is_valid_index(idx)) 
		return;
	
	if (amount > 0)
	{
		p_ptr->update |= PU_BONUS;
		if (disturb_minor)
			msg_print(_good_msg[which]);

		if (amount + p_ptr->virtues[idx] > 50 && one_in_(2))
		{
			p_ptr->virtues[idx] = MAX(p_ptr->virtues[idx], 50);
			return;
		}
		if (amount + p_ptr->virtues[idx] > 80 && one_in_(2))
		{
			p_ptr->virtues[idx] = MAX(p_ptr->virtues[idx], 80);
			return;
		}
		if (amount + p_ptr->virtues[idx] > 100 && one_in_(2))
		{
			p_ptr->virtues[idx] = MAX(p_ptr->virtues[idx], 100);
			return;
		}
		if (amount + p_ptr->virtues[idx] > 125)
			p_ptr->virtues[idx] = 125;
		else
			p_ptr->virtues[idx] = p_ptr->virtues[idx] + amount;
	}
	else
	{
		if (disturb_minor)
			msg_print(_bad_msg[which]);

		if (amount + p_ptr->virtues[idx] < -50 && one_in_(2))
		{
			p_ptr->virtues[idx] = MIN(p_ptr->virtues[idx], -50);
			return;
		}
		if (amount + p_ptr->virtues[idx] < -80 && one_in_(2))
		{
			p_ptr->virtues[idx] = MIN(p_ptr->virtues[idx], -80);
			return;
		}
		if (amount + p_ptr->virtues[idx] < -100 && one_in_(2))
		{
			p_ptr->virtues[idx] = MIN(p_ptr->virtues[idx], -100);
			return;
		}
		if (amount + p_ptr->virtues[idx] < -125)
			p_ptr->virtues[idx] = -125;
		else
			p_ptr->virtues[idx] = p_ptr->virtues[idx] + amount;
	}
}

void virtue_dump(FILE *file)
{
	int idx = 0;

	if (!file) return;
	if (!enable_virtues) return;

	for (idx = 0; idx < 8; idx++)
	{
		if (!_is_valid_virtue(p_ptr->vir_types[idx]))
		{
		}
		else
		{
			char name[255];
			int tester = p_ptr->virtues[idx];

			strcpy(name, virtue_name(p_ptr->vir_types[idx]));

			if (tester < -100)
				fprintf(file, "You are the polar opposite of %s.\n", name);
			else if (tester < -80)
				fprintf(file, "You are an arch-enemy of %s.\n", name);
			else if (tester < -60)
				fprintf(file, "You are a bitter enemy of %s.\n", name);
			else if (tester < -40)
				fprintf(file, "You are an enemy of %s.\n", name);
			else if (tester < -20)
				fprintf(file, "You have sinned against %s.\n", name);
			else if (tester < 0)
				fprintf(file, "You have strayed from the path of %s.\n", name);
			else if (tester == 0)
				fprintf(file,"You are neutral to %s.\n", name);
			else if (tester < 20)
				fprintf(file,"You are somewhat virtuous in %s.\n", name);
			else if (tester < 40)
				fprintf(file,"You are virtuous in %s.\n", name);
			else if (tester < 60)
				fprintf(file,"You are very virtuous in %s.\n", name);
			else if (tester < 80)
				fprintf(file,"You are a champion of %s.\n", name);
			else if (tester < 100)
				fprintf(file,"You are a great champion of %s.\n",  name);
			else
				fprintf(file,"You are the living embodiment of %s.\n", name);
		}
	}
}
