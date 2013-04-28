/* File: xtra1.c */

/*
 * Character titles and fame text.  Left and bottom panel displays.  Show
 * some things in sub-windows.  Calculate spells, mana, hitpoints, torch
 * radius, and regeneration rate.  Apply bonuses and attributes to the
 * character from stats, shapechanges, equipment, and temporary conditions.
 * Calculate blows, shots, armor, and so on.  The game updating code.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Character title tables.  Feel free to use any that you like.
 */
static cptr fighter_general[10] =
{
	"Fighter",
	"Soldier",
	"Mercenary",
	"Warrior",
	"Veteran",
	"Champion",
	"Hero/Heroine",
	"Baron/Amazon",
	"Iron Duke/Iron Lady",
	"#Lord of Battle/#Queen of Victory"
};

static cptr swordfighter[10] =
{
	"Rookie",
	"Fencer",
	"Duelist",
	"Swordsman",
	"Swashbuckler",
	"Freesword",
	"Bloodblade",
	"Deathsword",
	"#Blade of Gold",
	"#Lord of the Blade/#Lady of the Blade"
};

static cptr spearfighter[10] =
{
	"Rookie",
	"Spearsman",
	"Pikeman",
	"Phalangist",
	"Halberdier",
	"Varangian",
	"Dread-Axe",
	"Spear-Master/Spear-Lady",
	"#Lance of Battle",
	"Lord of the Lance/Lady of the Lance"
};

static cptr macefighter[10] =
{
	"Rookie",
	"Clubber",
	"Flailer",
	"Maceman",
	"Crusher",
	"Foe-smasher",
	"#Ball of Steel",
	"#Hammer Implacable",
	"#Death-Wrecker",
	"#Mace of Victory"
};

static cptr missilefighter[10] =
{
	"Runner",
	"Hunter",
	"Scout",
	"Courser",
	"Tracker",
	"Guide",
	"Pathfinder",
	"Low Ranger",
	"High Ranger",
	"Ranger Lord"
};

static cptr crossbowfighter[9] =
{
	"Bow-cocker",
	"Boltshooter",
	"Crossbowsman",
	"Missilier",
	"Sniper",
	"Sharpshooter",
	"Thunderbolt",
	"Quarrel-driver",
	"#Hail of Steel"
};

static cptr bowfighter[9] =
{
	"Hunter",
	"Archer",
	"Bowsman",
	"Longbowsman",
	"Marksman",
	"Sharpshooter",
	"Bowmaster",
	"Shaft-driver",
	"#Lord of the Bow/#Lady of the Bow"
};

static cptr slingfighter[8] =
{
	"Rock-whipper",
	"Slinger",
	"Missilier",
	"Sling-Warrior",
	"Master Slinger",
	"Stormslinger",
	"Lead-driver",
	"#Hail of Lead"
};

static cptr throwerfighter[8] =
{
	"Lobber",
	"Thrower",
	"Skirmisher",
	"Striker",
	"Bushfighter",
	"Hurler",
	"Catapulter",
	"Unerring Eye"
};

static cptr karatefighter[9] =
{
	"Initiate",
	"Boxer",
	"Striker",
	"Master",
	"Karateka",
	"Iron Fist",
	"Black Belt",
	"Lightning Blow",
	"#Hands of Death"
};

static cptr wrestlingfighter[8] =
{
	"Tussler",
	"Wrestler",
	"Clincher",
	"Crusher",
	"Pugilist",
	"Prizewinner",
	"#Bear of Battle",
	"#Gripping Death"
};

static cptr pure_wizard[11] =
{
	"Novice",
	"Apprentice",
	"Trickster",
	"Visionist",
	"Illusionist",
	"Spellbinder",
	"Magician/Witch",
	"Conjurer/High Witch",
	"Archimage/Enchantress",
	"Sorcerer/Sorceress",
	"#Lord of Wizardry/#Lady of Enchantment"
};

static cptr h_wizard[10] =
{
	"Novice",
	"Apprentice",
	"Charmer",
	"Evoker",
	"Enchanter",
	"Incantationist",
	"Thaumaturge",
	"Hero-Mage/Heroine-Mage",
	"Master Mage",
	"Mage-King/Mage-Queen"
};

static cptr pure_priest[11] =
{
	"Believer",
	"Acolyte",
	"Adept",
	"Curate",
	"Theurgist",
	"Immaculate",
	"Inquisitor",
	"Lord Abbot/Lady Abbot",
	"Patriarch/Matriarch",
	"High Priest/High Priestess",
	"#Shining Saint"
};

static cptr h_priest[12] =
{
	"Gallant",
	"Keeper",
	"Protector",
	"Defender",
	"Warder",
	"Knight",
	"Guardian",
	"Chevalier",
	"Crusader",
	"Justiciar",
	"Paladin",
	"Paladin Lord/Holy Amazon"
};

static cptr pure_druid[10] =
{
	"Wanderer",
	"Tamer",
	"Nurturer",
	"Creator",
	"Mystic",
	"Earthwarder",
	"Windrider",
	"Stormwielder",
	"High Mystic",
	"Mystic Lord/Mystic Queen"
};

static cptr h_druid[10] =
{
	"Tracker",
	"Defender",
	"Vitalist",
	"Lion Warrior",
	"Beast-Hunter",
	"Nature Guardian",
	"Battle-Shaman",
	"War-Druid",
	"Mystic Knight",
	"Ranger Lord"
};

static cptr pure_necro[10] =
{
	"Acolyte",
	"Curser",
	"Dark Student",
	"Initiate",
	"Slavemaster",
	"Summoner",
	"Controller",
	"Commander",
	"Dark Master/Dark Mistress",
	"Night Lord/Lady of Blood"
};

static cptr h_necro[10] =
{
	"Dacoit",
	"Acolyte",
	"Dark Soldier",
	"Nightfighter",
	"Undead-Hunter",
	"Death-Warrior",
	"Vampire-Slayer",
	"Dark Knight",
	"#Fell Enforcer",
	"#Shadow of Death"
};

static cptr pure_burglar[11] =
{
	"Vagabond",
	"Pilferer",
	"Cutpurse",
	"Robber",
	"Scofflaw",
	"Thief",
	"Burglar",
	"Rogue",
	"Master Rogue",
	"Guildmaster",
	"Lord of Misrule/Queen of Misrule"
};

static cptr hand_burglar[12] =
{
	"Thug",
	"Bouncer",
	"Enforcer",
	"Outlaw",
	"Waylayer",
	"Backstabber",
	"Shadowboxer",
	"Renegade",
	"Executioner",
	"Guildgripper",
	"Lord Executioner",
	"#Sudden Death"
};

static cptr fighting_burglar[11] =
{
	"Trainee",
	"Myrmidon",
	"Stalker",
	"Knifer",
	"Bladesman",
	"Hashishin",
	"Shadowstrike",
	"Assassin",
	"High Assassin",
	"Guild Assassin",
	"Death Lord/#Lady Death"
};

static cptr mage_burglar[11] =
{
	"Dodger",
	"Apprentice",
	"Trickster",
	"Illusionist",
	"Mystifier",
	"Incantationist",
	"Magic Thief",
	"Burglar-Mage",
	"Shadow Wizard",
	"Sorcerer-Rogue",
	"Sorceror of Misrule/Sorceress of Misrule"
};

static cptr priest_burglar[11] =
{
	"Wanderer",
	"White-hat",
	"Adept",
	"Keensman",
	"Gentle of Shadow",
	"Holy Burglar",
	"Shadow Paladin",
	"Divine Keensman",
	"Divine Guildmaster/Divine Guildmaster",
	"Shadow Patriarch/Shadow Matriarch",
	"#Saint-in-Shadow"
};

static cptr druid_burglar[10] =
{
	"Wanderer",
	"Forester",
	"Shadowwarder",
	"Panther-Thief",
	"Green Burglar",
	"Hidden Shaman",
	"Shaman-Rogue",
	"Elemental Rogue",
	"Mystic Guildmaster",
	"Mystic of Misrule"
};

static cptr necro_burglar[11] =
{
	"Vagabond",
	"Pilferer",
	"Cutpurse",
	"Dark Robber",
	"#Robber of the Ritual",
	"#Robber of the Rune",
	"Blood Thief",
	"Burglar of Blood",
	"Death Rogue",
	"#Fell Enforcer",
	"Lord of Misrule/Queen of Misrule"
};


static cptr device_user[10] =
{
	"Zapper",
	"Charm-user",
	"Empiric",
	"Wand-wielder",
	"Demonstrator",
	"Student of Magic",
	"Mage-Technician",
	"Practical Magician",
	"Magic Engineer",
	"#Guildmaster of Devices"
};

static cptr alchemist[10] =
{
	"Dabbler",
	"Student",
	"Experimentalist",
	"Catalyst",
	"Alchemist",
	"Ring-Binder",
	"Infuser",
	"Creator",
	"Alchemist Lord",
	"Philosopher-Magus"
};

static cptr infuser[10] =
{
	"Charmspeaker",
	"Infuser",
	"Creator",
	"Patternweaver",
	"Powerweaver",
	"#Binder of Hexes",
	"#Master of the Flux/#Mistress of the Flux",
	"#Master of the Form/#Mistress of the Form",
	"#Master of the Ideal/#Mistress of the Ideal",
	"#Lord of Infusion"
};

static cptr forger[10] =
{
	"Tinker",
	"Cobbler",
	"Forger",
	"Metalworker",
	"Blacksmith",
	"Hammersmith",
	"Loresmith",
	"Runemaster",
	"Artificer",
	"#Divine Artificer"
};



/*
 * Given a level of expertise and a table to draw from, choose a title.
 *
 * We sometimes have differing titles for males and females.  We sometimes
 * have to shorten the title to fit a given space.
 */
static cptr title_aux(int level, cptr *list, int array_elements, bool quotes, u16b max_len)
{
	int i;
	char buf[DESC_LEN];
	char *s, *t;

	/* Paranoia -- require that level be in bounds */
	if (level > 100) level = 100;
	if (level <   0) level =   0;

	/* Pick an array element (round slightly down) */
	i = (((array_elements-1) * level) + 25) / 100;

	/* Copy the raw title */
	strcpy(buf, list[i]);

	/* Scan title for a '/', which separates one gender's title from another */
	for (s = buf; *s; s++)
	{
		/* We have found a '/' */
		if (*s == '/')
		{
			/* Male characters use the title before the '/' */
			if (p_ptr->psex == SEX_MALE)
			{
				/* Truncate the title at the dividing point */
				*s = '\0';
			}

			/* Female characters use the title after the '/' */
			else
			{
				/* Delete all characters up to the dividing point */
				for (s = buf; *s != '/'; s++) *s = '\0';

				/* Delete the divider */
				*s++ = '\0';

				/* Copy the remaining text back into storage */
				for (i = 0; *s; s++, i++) buf[i] = *s;

				/* End the string */
				buf[i] = '\0';
			}

			/* Done */
			break;
		}
	}

	/* If title has a quotes marker, and we will remove it, allow more space */
	if ((buf[0] == '#') && (!quotes)) max_len++;


	/* Length of title exceeds our maximum */
	if (strlen(buf) > max_len)
	{
		char tmp[DESC_LEN];

		/* Check for the word " of " */
		t = strstr(buf, " of ");

		/* Word exists */
		if (t)
		{
			/* Until we reach this word, copy characters */
			for (i = 0, s = buf; s < t; s++, i++) tmp[i] = *s;

			/* Insert a '-' */
			tmp[i++] = '-';

			/* Determine whether the title contains " of the " */
			if (strstr(buf, " of the "))
			{
				/* Skip over this 8-character string */
				s = t + 8;
			}
			else
			{
				/* Skip over the 4 characters of " of " */
				s = t + 4;
			}

			/* Copy the remainder of the title */
			for (; *s; s++, i++) tmp[i] = *s;

			/* End the string */
			tmp[i] = '\0';

			/* Copy our truncated string back into "buf" */
			for (i = 0; i < (int)sizeof(buf) && tmp[i]; i++) buf[i] = tmp[i];

			/* End the string */
			buf[i] = '\0';
		}
	}


	/* Point to the title */
	s = buf;

	/* Add quotes, or remove quotes marker */
	if (buf[0] == '#')
	{
		if (quotes)
		{
			buf[0] = '\"';
			strcat(buf, "\"");
		}
		else s++;
	}

	/* Return a pointer to the title */
	return (format(s));
}


/*
 * Character titles vary depending on relative skill levels.  -LM-
 *
 * Store character specialty.
 *
 * If "wizard" is TRUE, we allow "winner" and "wizard" titles.
 */
cptr get_title(int len, bool wizard, bool quotes)
{
	/* Store all the skills that affect titles for handy reference */
	int s_sword = get_skill(S_SWORD, 0, 100);
	int s_hafted = get_skill(S_HAFTED, 0, 100);
	int s_polearm = get_skill(S_POLEARM, 0, 100);
	int s_crossbow = get_skill(S_CROSSBOW, 0, 100);
	int s_bow = get_skill(S_BOW, 0, 100);
	int s_sling = get_skill(S_SLING, 0, 100);
	int s_throwing = get_skill(S_THROWING, 0, 100);
	int s_wrestling = get_skill(S_WRESTLING, 0, 100);
	int s_karate = get_skill(S_KARATE, 0, 100);

	int s_magic = get_skill(S_MAGIC, 0, 100);
	int s_wizardry = get_skill(S_WIZARDRY, 0, 100);
	int s_piety = get_skill(S_PIETY, 0, 100);
	int s_nature = get_skill(S_NATURE, 0, 100);
	int s_dominion = get_skill(S_DOMINION, 0, 100);

	int s_device = get_skill(S_DEVICE, 0, 100);
	int s_burglary = get_skill(S_BURGLARY, 0, 100);
	int s_forge_weapon = get_skill(S_FORGE_WEAPON, 0, 100);
	int s_forge_armor = get_skill(S_FORGE_ARMOR, 0, 100);
	int s_alchemy = get_skill(S_ALCHEMY, 0, 100);
	int s_infusion = get_skill(S_INFUSION, 0, 100);

	/* Groups of skills */
	int fighting_skill, weapon_skill, missile_skill, unarmed_skill,
		magic_combat_skill, forging_skill, creation_skill;

	int s;


	/* Get the highest skill in each of various categories */
	weapon_skill = MAX(s_sword, s_polearm);
	weapon_skill = MAX(s_hafted, weapon_skill);

	missile_skill = MAX(s_crossbow, s_bow);
	missile_skill = MAX(s_sling, missile_skill);

	unarmed_skill = MAX(s_wrestling, s_karate);

	fighting_skill = MAX(weapon_skill, missile_skill);
	fighting_skill = MAX(unarmed_skill, fighting_skill);
	fighting_skill = MAX(s_throwing, fighting_skill);

	magic_combat_skill = MAX(s_wizardry, s_piety);
	magic_combat_skill = MAX(s_nature, magic_combat_skill);
	magic_combat_skill = MAX(s_dominion, magic_combat_skill);

	forging_skill = MAX(s_forge_armor, s_forge_weapon);

	creation_skill = MAX(s_alchemy, forging_skill);
	creation_skill = MAX(s_infusion, creation_skill);


	/* Wizard */
	if (wizard)
	{
		/* Wizard */
		if (p_ptr->wizard)
		{
			return ("[=-WIZARD-=]");
		}

		/* Winner */
		if (p_ptr->total_winner)
		{
			return ("***WINNER***");
		}
	}


	/* No experience yet */
	if (!calc_spent_exp())
	{
		p_ptr->specialty = SPECIALTY_NONE;
		return ("Adventurer");
	}



	/* Check for fighter */
	if (fighting_skill)
	{
		bool fighter = TRUE;

		/* Oath of Iron or no magic realm */
		if ((p_ptr->oath & (OATH_OF_IRON)) || (!p_ptr->realm))
		{
			/* Accept combat skills unless lower */
			if      (fighting_skill < creation_skill) fighter = FALSE;
			else if (fighting_skill < s_device) fighter = FALSE;
			else if (fighting_skill < s_burglary) fighter = FALSE;
		}

		/* Any other Oath (including Burglary) */
		else if (p_ptr->oath)
		{
			/* Require great concentration on combat skills */
			if      (fighting_skill < 3 * creation_skill / 2) fighter = FALSE;
			else if (fighting_skill < 3 * s_device / 2) fighter = FALSE;
			else if (fighting_skill < s_magic * 2) fighter = FALSE;
			else if (fighting_skill < 5 * s_burglary / 4) fighter = FALSE;
		}

		/* Magic realm, but no Oath */
		else
		{
			/* Require some concentration on combat skills */
			if      (fighting_skill < 5 * creation_skill / 4) fighter = FALSE;
			else if (fighting_skill < 5 * s_device / 4) fighter = FALSE;
			else if (fighting_skill < 5 * s_magic / 4) fighter = FALSE;
			else if (fighting_skill < 5 * s_burglary / 4) fighter = FALSE;
		}

		/* We're a fighter */
		if (fighter)
		{
			/* Investment in burglary */
			if ((s_burglary >= 2 * fighting_skill / 3) && (s_burglary > s_magic))
			{
				s = (s_burglary + p_ptr->power) / 2;

				/* Concentration on hand-to-hand combat */
				if (unarmed_skill == fighting_skill)
				{
					p_ptr->specialty = SPECIALTY_HAND_BURGLAR;
					return (title_aux(s, hand_burglar,
					                  N_ELEMENTS(hand_burglar), quotes, len));
				}

				/* Concentration on melee or throwing */
				p_ptr->specialty = SPECIALTY_FIGHT_BURGLAR;
				return (title_aux(s, fighting_burglar,
				                  N_ELEMENTS(fighting_burglar), quotes, len));
			}

			/* Investment in the specialist realm skills */
			if ((magic_combat_skill >= 3 * fighting_skill / 2) ||
			    (p_ptr->realm && (magic_combat_skill >= fighting_skill / 2)))
			{
				s = (fighting_skill + p_ptr->power) / 2;

				/* Wizardry */
				if ((s_wizardry == magic_combat_skill) &&
				    (p_ptr->realm == MAGE) &&
				    (s_wizardry >= 3 * fighting_skill / 4))
				{
					p_ptr->specialty = SPECIALTY_H_WIZARD;
					return (title_aux(s, h_wizard, N_ELEMENTS(h_wizard), quotes, len));
				}

				/* Piety */
				if ((s_piety == magic_combat_skill) ||
				    (p_ptr->realm == PRIEST &&
				     s_piety >= 2 * magic_combat_skill / 3))
				{
					p_ptr->specialty = SPECIALTY_H_PRIEST;
					return (title_aux(s, h_priest, N_ELEMENTS(h_priest), quotes, len));
				}

				/* Nature */
				if ((s_nature == magic_combat_skill) ||
				    (p_ptr->realm == DRUID &&
				     s_nature >= 2 * magic_combat_skill / 3))
				{
					/* Require actual magic at higher levels */
					if ((p_ptr->power <= 50) || (p_ptr->realm == DRUID))
					{
						p_ptr->specialty = SPECIALTY_H_DRUID;
						return (title_aux(s, h_druid, N_ELEMENTS(h_druid), quotes, len));
					}
				}

				/* Blood dominion */
				if ((s_dominion == magic_combat_skill) ||
				    (p_ptr->realm == NECRO &&
				     s_dominion >= 2 * magic_combat_skill / 3))
				{
					p_ptr->specialty = SPECIALTY_H_NECRO;
					return (title_aux(s, h_necro, N_ELEMENTS(h_necro), quotes, len));
				}
			}

			/* Concentration on weapon-using melee */
			if ((weapon_skill == fighting_skill) &&
			    (weapon_skill > 4 * MAX(missile_skill, unarmed_skill) / 3))
			{
				/* Concentration on swords */
				if ((s_sword >= 5 * s_polearm / 3) && (s_sword >= s_hafted * 2))
				{
					p_ptr->specialty = SPECIALTY_SWORDS;
					s = (s_sword + p_ptr->power) / 2;
					return (title_aux(s, swordfighter, N_ELEMENTS(swordfighter),
						quotes, len));
				}

				/* Concentration on polearms */
				if ((s_polearm >= 5 * s_sword / 3) && (s_polearm >= s_hafted * 2))
				{
					p_ptr->specialty = SPECIALTY_POLEARMS;
					s = (s_polearm + p_ptr->power) / 2;
					return (title_aux(s, spearfighter, N_ELEMENTS(spearfighter),
						quotes, len));
				}

				/* Concentration on hafted weapons */
				if ((s_hafted >= s_polearm * 2) && (s_hafted >= s_sword * 2))
				{
					p_ptr->specialty = SPECIALTY_HAFTED;
					s = (s_hafted + p_ptr->power) / 2;
					return (title_aux(s, macefighter, N_ELEMENTS(macefighter),
						quotes, len));
				}
			}

			/* Concentration on missile warfare */
			if ((missile_skill == fighting_skill) &&
			    (missile_skill > 4 * MAX(weapon_skill, unarmed_skill) / 3))
			{
				s = (missile_skill + p_ptr->power) / 2;

				/* Generalist (requires some magical ability) */
				if ((p_ptr->realm) && (s_bow > 2 * missile_skill / 3) &&
				    (s_crossbow > 2 * missile_skill / 3))
				{
					p_ptr->specialty = SPECIALTY_MISSILE_MAGIC;
					return (title_aux(s, missilefighter,
					                  N_ELEMENTS(missilefighter), quotes, len));
				}

				/* Specialist in crossbows */
				if (s_crossbow == missile_skill)
				{
					p_ptr->specialty = SPECIALTY_CROSSBOW;
					return (title_aux(s, crossbowfighter,
					                  N_ELEMENTS(crossbowfighter), quotes, len));
				}

				/* Specialist with bows */
				if (s_bow == missile_skill)
				{
					p_ptr->specialty = SPECIALTY_BOW;
					return (title_aux(s, bowfighter, N_ELEMENTS(bowfighter),
						quotes, len));
				}

				/* Specialist with slings */
				if (s_sling == missile_skill)
				{
					p_ptr->specialty = SPECIALTY_SLING;
					return (title_aux(s, slingfighter, N_ELEMENTS(slingfighter),
						quotes, len));
				}
			}

			/* Concentration on unarmed combat */
			if ((unarmed_skill == fighting_skill) &&
			    (unarmed_skill > 5 * MAX(weapon_skill, missile_skill) / 4))
			{
				if (s_karate >= s_wrestling) p_ptr->specialty = SPECIALTY_KARATE;
				else                         p_ptr->specialty = SPECIALTY_WRESTLING;
				s = (MAX(s_karate, s_wrestling) + p_ptr->power) / 2;
				return (title_aux(s,
					(s_karate >= s_wrestling ? karatefighter : wrestlingfighter),
					N_ELEMENTS((s_karate >= s_wrestling ? karatefighter :
						wrestlingfighter)), quotes, len));
			}

			/* Concentration on throwing */
			if ((s_throwing == fighting_skill) &&
			    (s_throwing > 4 * MAX(weapon_skill, missile_skill) / 3))
			{
				p_ptr->specialty = SPECIALTY_THROWING;
				s = (s_throwing + p_ptr->power) / 2;
				return (title_aux(s, throwerfighter, N_ELEMENTS(throwerfighter),
						quotes, len));
			}

			/* Generalist */
			if (TRUE)
			{
				p_ptr->specialty = SPECIALTY_FIGHTER;
				s = (fighting_skill + p_ptr->power) / 2;
				return (title_aux(s, fighter_general,
				                  N_ELEMENTS(fighter_general), quotes, len));
			}
		}
	}


	/* Check for burglary */
	if (s_burglary)
	{
		bool burglary_flag = FALSE;

		/* We're a Guild burglar */
		if (p_ptr->oath & (BURGLARS_GUILD))
		{
			/* Require some burglary skill */
			if ((s_burglary > 3 * s_magic / 5) &&
			    (s_burglary > 3 * s_device / 4) &&
			    (s_burglary > 3 * creation_skill / 4))
			{
				burglary_flag = TRUE;
			}
		}

		/* We're not a Guild burglar */
		else
		{
			/* Require focus on burglary skill */
			if ((s_burglary > (p_ptr->oath ? s_magic : 2 * s_magic / 3)) &&
			    (s_burglary > 5 * s_device / 4) &&
			    (s_burglary > 5 * creation_skill / 4))
			{
				burglary_flag = TRUE;
			}
		}

		/* We're some sort of Burglar */
		if (burglary_flag)
		{
			/* Check for investment in magic (always, if priest) */
			if ((p_ptr->realm) &&
			    ((s_magic > 2 * s_burglary / 3) || (p_ptr->realm == PRIEST)))
			{
				s = p_ptr->power;

				/* Wizardry */
				if (p_ptr->realm == MAGE)
				{
					p_ptr->specialty = SPECIALTY_MAGE_BURGLAR;
					return (title_aux(s, mage_burglar,
						N_ELEMENTS(mage_burglar), quotes, len));
				}

				/* Piety */
				if (p_ptr->realm == PRIEST)
				{
					p_ptr->specialty = SPECIALTY_PRIEST_BURGLAR;
					return (title_aux(s, priest_burglar,
						N_ELEMENTS(priest_burglar), quotes, len));
				}

				/* Nature */
				if (p_ptr->realm == DRUID)
				{
					p_ptr->specialty = SPECIALTY_DRUID_BURGLAR;
					return (title_aux(s, druid_burglar,
						N_ELEMENTS(druid_burglar), quotes, len));
				}

				/* Blood dominion */
				if (p_ptr->realm == NECRO)
				{
					p_ptr->specialty = SPECIALTY_NECRO_BURGLAR;
					return (title_aux(s, necro_burglar,
						N_ELEMENTS(necro_burglar), quotes, len));
				}
			}

			/* Otherwise, we are considered a "pure" burglar */
			p_ptr->specialty = SPECIALTY_PURE_BURGLAR;

			s = (s_burglary + p_ptr->power) / 2;
			return (title_aux(s, pure_burglar,
				N_ELEMENTS(pure_burglar), quotes, len));
		}
	}


	/* We're a magic-user */
	if (p_ptr->realm)
	{
		bool specialist = FALSE;

		/* Get the maximum of all remaining skills */
		int misc_skills = MAX(s_device, creation_skill);

		/* We've taken an oath */
		if (p_ptr->oath)
		{
			/* Bias for spellcasting */
			if (3 * misc_skills / 4 < s_magic) specialist = TRUE;
		}

		/* We haven't */
		else
		{
			/* Bias against spellcasting */
			if (5 * misc_skills / 4 < s_magic) specialist = TRUE;
		}

		/* We're a specialist magic-user */
		if (specialist)
		{
			/* We're a wizard */
			if (p_ptr->realm == MAGE)
			{
				p_ptr->specialty = SPECIALTY_P_WIZARD;
				s = (s_magic + s_wizardry + p_ptr->power) / 3;
				return (title_aux(s, pure_wizard, N_ELEMENTS(pure_wizard), quotes, len));
			}

			/* We're a priest */
			if (p_ptr->realm == PRIEST)
			{
				p_ptr->specialty = SPECIALTY_P_PRIEST;
				s = (s_magic + s_piety + p_ptr->power) / 3;
				return (title_aux(s, pure_priest, N_ELEMENTS(pure_priest), quotes, len));
			}

			/* We're a druid */
			if (p_ptr->realm == DRUID)
			{
				p_ptr->specialty = SPECIALTY_P_DRUID;
				s = (s_magic + s_nature + p_ptr->power) / 3;
				return (title_aux(s, pure_druid, N_ELEMENTS(pure_druid), quotes, len));
			}

			/* We're a necromancer */
			if (p_ptr->realm == NECRO)
			{
				p_ptr->specialty = SPECIALTY_P_NECRO;
				s = (s_magic + s_dominion + p_ptr->power) / 3;
				return (title_aux(s, pure_necro, N_ELEMENTS(pure_necro), quotes, len));
			}
		}
	}

	/* We're a specialist in magical devices */
	if (s_device > creation_skill)
	{
		p_ptr->specialty = SPECIALTY_DEVICE;
		s = (s_device + p_ptr->power) / 2;
		return (title_aux(s, device_user, N_ELEMENTS(device_user), quotes, len));
	}

	/* We have some creation skill */
	if (creation_skill)
	{
		/* We're an object-forger */
		if (forging_skill == creation_skill)
		{
			p_ptr->specialty = SPECIALTY_FORGING;
			s = (forging_skill + p_ptr->power) / 2;
			return (title_aux(s, forger, N_ELEMENTS(forger), quotes, len));
		}

		/* We're an alchemist */
		if (s_alchemy == creation_skill)
		{
			p_ptr->specialty = SPECIALTY_ALCHEMY;
			s = (s_alchemy + p_ptr->power) / 2;
			return (title_aux(s, alchemist, N_ELEMENTS(alchemist), quotes, len));
		}

		/* We're an infuser */
		if (s_infusion == creation_skill)
		{
			p_ptr->specialty = SPECIALTY_INFUSION;
			s = (s_infusion + p_ptr->power) / 2;
			return (title_aux(s, infuser, N_ELEMENTS(infuser), quotes, len));
		}
	}

	/* Can't seem to find anything that matches */
	p_ptr->specialty = SPECIALTY_NONE;
	return ("Adventurer");
}


/*
 * Get a color and description of the character's fame
 */
void get_fame_desc(int *attr, char *fame_desc)
{
	int f = p_ptr->fame;

	if (f < -20)
	{
		*attr = TERM_RED;
		strcpy(fame_desc, "Reviled");
	}
	else if (f < -10)
	{
		*attr = TERM_ORANGE;
		strcpy(fame_desc, "Despised");
	}
	else if (f < 0)
	{
		*attr = TERM_YELLOW;
		strcpy(fame_desc, "Scorned");
	}
	else if (f == 0)
	{
		*attr = TERM_SLATE;
		strcpy(fame_desc, "Unknown");
	}
	else if (f < 10)
	{
		*attr = TERM_L_WHITE;
		strcpy(fame_desc, "Mentioned");
	}
	else if (f < 20)
	{
		*attr = TERM_WHITE;
		strcpy(fame_desc, "Spoken of");
	}
	else if (f < 30)
	{
		*attr = TERM_L_GREEN;
		strcpy(fame_desc, "Respected");
	}
	else if (f < 40)
	{
		*attr = TERM_L_GREEN;
		strcpy(fame_desc, "Praised");
	}
	else if (f < 52)
	{
		*attr = TERM_GREEN;
		strcpy(fame_desc, "Well-known");
	}
	else if (f < 64)
	{
		*attr = TERM_GREEN;
		strcpy(fame_desc, "Honored");
	}
	else if (f < 76)
	{
		*attr = TERM_L_BLUE;
		strcpy(fame_desc, "Storied");
	}
	else if (f < 88)
	{
		*attr = TERM_L_BLUE;
		strcpy(fame_desc, "Far-famed");
	}
	else if (f < 100)
	{
		*attr = TERM_BLUE;
		strcpy(fame_desc, "Legendary");
	}
	else
	{
		*attr = TERM_PURPLE;
		strcpy(fame_desc, "Immortal");
	}
}



/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(char *out_val, size_t size, int val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 220)
		{
			(void)strnfmt(out_val, size, "18/%3s", "***");
		}
		else if (bonus >= 100)
		{
			(void)strnfmt(out_val, size, "18/%03d", bonus);
		}
		else
		{
			(void)strnfmt(out_val, size, " 18/%02d", bonus);
		}
	}

	/* From 3 to 18 */
	else
	{
		(void)strnfmt(out_val, size, "    %2d", val);
	}
}



/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat(int value, int amount)
{
	int i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18) value++;

			/* Ten "points" at a time */
			else value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18 + 10) value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18) value = 18;

			/* One point at a time */
			else if (value > 3) value--;
		}
	}

	/* Return new value */
	return (value);
}


/*
 * Display short name of character
 */
static void prt_short_name(void)
{
	int i;

	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", ROW_NAME, COL_NAME);

	for (i = 0; i < 12; i++)
	{
		if (op_ptr->full_name[i] == '\0') break;
		if ((op_ptr->full_name[i] == ' ') && (i >= 7)) break;

		(void)Term_putch(COL_NAME + i, ROW_NAME, TERM_L_GREEN, op_ptr->full_name[i]);
	}
}


/*
 * Prints character titles.
 */
static void prt_title(void)
{
	int i;
	cptr title = get_title(13, TRUE, FALSE);

	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", ROW_TITLE, COL_TITLE);

	for (i = 0; i < 12; i++)
	{
		if (title[i] == '\0') break;

		(void)Term_putch(COL_TITLE + i, ROW_TITLE, TERM_L_BLUE, title[i]);
	}
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
	char out_val[32];
	bool skdrain = FALSE;
	int i;

	for (i = 0; i < NUM_SK_USED && !skdrain; i++)
	{
		if (p_ptr->pskills[i].cur < p_ptr->pskills[i].max) skdrain = TRUE;
	}

	(void)strnfmt(out_val, sizeof(out_val), "%8ld", (long)p_ptr->exp);

	/* Skills have not been drained */
	if (!skdrain)
	{
		put_str("EXP ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
	}

	/* One or more skills have been drained */
	else
	{
		put_str("Exp ", ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	char tmp[32];

	put_str("AU ", ROW_GOLD, COL_GOLD);
	(void)strnfmt(tmp, sizeof(tmp), "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}

/*
 * Equippy chars
 */
static void prt_equippy(void)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;

	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		/* Object */
		o_ptr = &inventory[i];

		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* No object -- clear with spaces */
		if (!o_ptr->k_idx)
		{
			c = ' ';
			a = TERM_DARK;
		}

		/* Special case -- diggers need prominent notice */
		else if (o_ptr->tval == TV_DIGGING)
		{
			a = TERM_L_PURPLE;
			c = '#';
		}

		/* Dump */
		(void)Term_putch(COL_EQUIPPY + i - INVEN_WIELD, ROW_EQUIPPY, a, c);
	}

	/* We are using bare-handed combat */
	if (p_ptr->barehanded)
	{
		/* Using Karate */
		if (p_ptr->barehand == S_KARATE) c = 'K';

		/* Using Wrestling */
		else if (p_ptr->barehand == S_WRESTLING) c = 'W';

		/* Unknown bare-handed combat method */
		else c = '*';

		/* Dump an indicator */
		(void)Term_putch(COL_EQUIPPY, ROW_EQUIPPY, TERM_SLATE, c);
	}
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		cnv_stat(tmp, sizeof(tmp), p_ptr->stat_use[stat]);
		c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
		cnv_stat(tmp, sizeof(tmp), p_ptr->stat_use[stat]);
		c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18 + 100)
	{
		put_str("!", ROW_STAT + stat, 3);
	}
}


/*
 * Prints current shape, if not normal.  -LM-
 */
static void prt_shape(void)
{
	cptr shapedesc = "          ";

	switch (p_ptr->schange)
	{
		case SHAPE_GOAT:
			shapedesc = "Goat      ";
			break;
		case SHAPE_BEAR:
			shapedesc = "Bear      ";
			break;
		case SHAPE_MOUSE:
			shapedesc = "Mouse     ";
			break;
		case SHAPE_HOUND:
			shapedesc = "Hound     ";
			break;
		case SHAPE_CHEETAH:
			shapedesc = "Cheetah   ";
			break;
		case SHAPE_LION:
			shapedesc = "Lion      ";
			break;
		case SHAPE_DRAGON:
			shapedesc = "Dragon    ";
			break;
		case SHAPE_ENT:
			shapedesc = "Ent       ";
			break;
		case SHAPE_TROLL:
			shapedesc = "Troll     ";
			break;
		case SHAPE_BAT:
			shapedesc = "Bat       ";
			break;
		case SHAPE_LICH:
			shapedesc = "Lich      ";
			break;
		case SHAPE_VAMPIRE:
			shapedesc = "Vampire   ";
			break;
		case SHAPE_WEREWOLF:
			shapedesc = "Werewolf  ";
			break;
		case SHAPE_SERPENT:
			shapedesc = "Serpent   ";
			break;
		case SHAPE_ANGEL:
			shapedesc = "Angel     ";
			break;
		case SHAPE_VORTEX:
			shapedesc = "Fire Vortx";
			break;
		case SHAPE_GOLEM:
			shapedesc = "Golem     ";
			break;
		case SHAPE_EAGLE:
			shapedesc = "Golem     ";
			break;

		default:
			shapedesc = "          ";
			break;
	}

	/* Display (or write over) the shapechange with pretty colors. */
	if (p_ptr->realm == DRUID) c_put_str(TERM_GREEN, shapedesc,
		ROW_SHAPE, COL_SHAPE);
	else if (p_ptr->realm == NECRO) c_put_str(TERM_PURPLE, shapedesc,
		ROW_SHAPE, COL_SHAPE);
	else c_put_str(TERM_RED, shapedesc, ROW_SHAPE, COL_SHAPE);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

	put_str("Cur AC ", ROW_AC, COL_AC);
	(void)strnfmt(tmp, sizeof(tmp), "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
}


/*
 * Print character's max/cur hitpoints
 */
static void prt_hp(void)
{
	char tmp[32];
	byte a;

	int warn = op_ptr->hitpoint_warn;

	/* Display title */
	put_str("HP ", ROW_HP, COL_HP);

	/* Color the current hitpoints according to damage and HP warning */
	if (p_ptr->chp < p_ptr->mhp * (warn / 2) / 10) a = TERM_L_RED;
	else if (p_ptr->chp < p_ptr->mhp * warn / 10)  a = TERM_ORANGE;
	else if (p_ptr->chp < p_ptr->mhp)              a = TERM_YELLOW;
	else if (p_ptr->chp == p_ptr->mhp)             a = TERM_L_GREEN;
	else                                           a = TERM_GREEN;

	/* Display current hitpoints */
	(void)strnfmt(tmp, sizeof(tmp), "%4d", p_ptr->chp);
	c_put_str(a, tmp, ROW_HP, COL_HP + 3);

	/* Divider */
	put_str("/", ROW_HP, COL_HP + 7);

	/* Format and display maximum hitpoints */
	(void)strnfmt(tmp, sizeof(tmp), "%4d", p_ptr->mhp);
	c_put_str(TERM_L_GREEN, tmp, ROW_HP, COL_HP + 8);
}


/*
 * Print character's max/cur spell points
 */
static void prt_sp(void)
{
	char tmp[32];
	byte a;

	int warn = op_ptr->hitpoint_warn;


	/* Do not show mana unless it matters */
	if (!p_ptr->msp && !mp_ptr->spell_book) return;


	/* Display title */
	put_str("SP ", ROW_SP, COL_SP);

	/* Color the current spell points according to damage and HP warning */
	if (p_ptr->csp < p_ptr->msp * (warn / 2) / 10) a = TERM_L_RED;
	else if (p_ptr->csp < p_ptr->msp * warn / 10)  a = TERM_ORANGE;
	else if (p_ptr->csp < p_ptr->msp)              a = TERM_YELLOW;
	else if (p_ptr->csp == p_ptr->msp)             a = TERM_L_GREEN;
	else                                           a = TERM_GREEN;

	/* Display current spell points */
	(void)strnfmt(tmp, sizeof(tmp), "%4d", p_ptr->csp);
	c_put_str(a, tmp, ROW_SP, COL_SP + 3);

	/* Divider */
	put_str("/", ROW_SP, COL_SP + 7);

	/* Format and display maximum spell points */
	(void)strnfmt(tmp, sizeof(tmp), "%4d", p_ptr->msp);
	c_put_str(TERM_L_GREEN, tmp, ROW_SP, COL_SP + 8);
}


/*
 * Redraw the monster health bar.
 */
void health_redraw_aux(monster_type *m_ptr, int row, int col)
{
	int path, attr2;

	/* Tracking an unseen monster */
	if (!m_ptr->ml)
	{
		/* Indicate that the monster health is "unknown" */
		(void)Term_putstr(col, row, 12, TERM_SLATE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		(void)Term_putstr(col, row, 12, TERM_PURPLE, "[??????????]");
	}

	/* Tracking a dead monster (?) */
	else if (!m_ptr->hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		(void)Term_putstr(col, row, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = TERM_L_RED;

		/* Wounded */
		if (pct >= 25) attr = TERM_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60) attr = TERM_YELLOW;

		/* Healthy */
		if (pct == 100) attr = TERM_L_GREEN;

		/* Super-powerful */
		if (pct > 100) attr = TERM_GREEN;

		/* Afraid */
		if (m_ptr->monfear) attr = TERM_PURPLE;

		/* Stunned */
		if (m_ptr->stunned) attr = TERM_L_BLUE;

		/* Confused */
		if (m_ptr->confused) attr = TERM_UMBER;

		/* Asleep */
		if (m_ptr->csleep) attr = TERM_BLUE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		(void)Term_putstr(col + 1, row, 12, TERM_WHITE, "          ");

		/* Dump the current "health" (use '*' symbols) */
		if (mon_fully_visible(m_ptr))
		{
			char sym = '*';
			if ((m_ptr->hasted) && (!m_ptr->slowed)) sym = '+';
			if ((m_ptr->slowed) && (!m_ptr->hasted)) sym = '-';

			(void)Term_putstr(col + 1, row, len, attr,
				format("%c%c%c%c%c%c%c%c%c%c",
				sym, sym, sym, sym, sym, sym, sym, sym, sym, sym));
		}
		else
		{
			(void)Term_putstr(col + 1, row, 12, TERM_L_DARK, "..........");
		}

		/* Check line of fire */
		path = projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px,
			PROJECT_CHCK);

		/* We have a clear, direct line of fire */
		if (path == PROJECT_CLEAR) attr2 = TERM_L_GREEN;

		/* We have an obstructed line of fire */
		else if (path == PROJECT_NOT_CLEAR) attr2 = TERM_WHITE;

		/* We have no line of fire */
		else if (path == PROJECT_NO) attr2 = TERM_SLATE;

		/* We have any other line of fire */
		else attr2 = TERM_L_DARK;

		/* Display brackets */
		(void)Term_putstr(col, row, 1, attr2, "[");
		(void)Term_putstr(col + 11, row, 1, attr2, "]");
	}
}

/*
 * Redraw the "monster health bar"
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targeting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.  When nothing
 * is being tracked, we clear the health bar.  If the monster being
 * tracked is not currently visible, a special health bar is shown.
 */
static void health_redraw(void)
{
	monster_type *m_ptr;

	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		(void)Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking a monster */
	else
	{
		/* Get the monster */
		m_ptr = &m_list[p_ptr->health_who];

		/* Print the health bar */
		health_redraw_aux(m_ptr, ROW_INFO, COL_INFO);
	}
}



/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < p_ptr->food_fainting)
	{
		c_put_str(TERM_RED, "Weak! ", ROW_STATUS, COL_HUNGRY);
	}

	/* Weak */
	else if (p_ptr->food < p_ptr->food_weak)
	{
		c_put_str(TERM_ORANGE, "Weak  ", ROW_STATUS, COL_HUNGRY);
	}

	/* Hungry */
	else if (p_ptr->food < p_ptr->food_hungry)
	{
		c_put_str(TERM_YELLOW, "Hungry", ROW_STATUS, COL_HUNGRY);
	}

	/* Normal */
	else if (p_ptr->food < p_ptr->food_full)
	{
		c_put_str(TERM_L_GREEN, "      ", ROW_STATUS, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < p_ptr->food_bloated)
	{
		c_put_str(TERM_L_GREEN, "Full  ", ROW_STATUS, COL_HUNGRY);
	}

	/* Gorged */
	else
	{
		c_put_str(TERM_GREEN, "Gorged", ROW_STATUS, COL_HUNGRY);
	}
}




/*
 * Insert a string, being careful not to go beyond the right margin.
 */
static errr c_roff_insert(byte a, cptr str, byte r_margin)
{
	int y, x;

	/* Locate the cursor */
	(void)Term_locate(&x, &y);

	/* Cursor is too far right to place this string */
	if (x + strlen(str) >= r_margin) return (1);

	/* Refuse to print to the message line  XXX XXX */
	if (y == 0) return (1);

	/* Display the string (plus a space) */
	c_roff(a, format("%s%s", str, " "), 0, r_margin);

	/* Success */
	return (0);
}


/*
 * Print cuts
 */
static bool prt_cut(byte r_margin)
{
	if (p_ptr->cut)
	{
		int c = p_ptr->cut;

		if (c > WOUND_MORTAL)
		{
			if (c_roff_insert(TERM_L_RED, "Mortal wound", r_margin)) return (1);
		}
		else if (c > 200)
		{
			if (c_roff_insert(TERM_RED, "Deep gash", r_margin)) return (1);
		}
		else if (c > 100)
		{
			if (c_roff_insert(TERM_RED, "Severe cut", r_margin)) return (1);
		}
		else if (c > 50)
		{
			if (c_roff_insert(TERM_ORANGE, "Nasty cut", r_margin)) return (1);
		}
		else if (c > 25)
		{
			if (c_roff_insert(TERM_ORANGE, "Bad cut", r_margin)) return (1);
		}
		else if (c > 10)
		{
			if (c_roff_insert(TERM_YELLOW, "Light cut", r_margin)) return (1);
		}
		else if (c)
		{
			if (c_roff_insert(TERM_YELLOW, "Graze", r_margin)) return (1);
		}
	}

	/* Success */
	return (0);
}

/*
 * Print stuns
 */
static bool prt_stun(byte r_margin)
{
	if (p_ptr->stun)
	{
		int s = p_ptr->stun;

		if (s >= KNOCKED_OUT)
		{
			if (c_roff_insert(TERM_RED, "Knocked out!", r_margin)) return (1);
		}
		else if (s >= HVY_STUN)
		{
			if (c_roff_insert(TERM_ORANGE, "Heavy stun", r_margin)) return (1);
		}
		else if (s)
		{
			if (c_roff_insert(TERM_YELLOW, "Stun", r_margin)) return (1);
		}
	}

	/* Success */
	return (0);
}


/*
 * Print poisoned
 */
static bool prt_poisoned(byte r_margin)
{
	if (p_ptr->poisoned)
	{
		int p = p_ptr->poisoned;
		int a;

		if      (p >= 200) a = TERM_RED;
		else if (p >=  50) a = TERM_ORANGE;
		else               a = TERM_YELLOW;
		if (c_roff_insert(a, "Poisoned", r_margin)) return (1);
	}

	/* Success */
	return (0);
}
/*
 * Print diseased
 */
static bool prt_diseased(byte r_margin)
{
	if (p_ptr->diseased)
	{
		int p = p_ptr->diseased;
		int a;

		if      (p >= 140) a = TERM_RED;
		else if (p >=  40) a = TERM_ORANGE;
		else               a = TERM_YELLOW;
		if (c_roff_insert(a, "Diseased", r_margin)) return (1);
	}

	/* Success */
	return (0);
}


/*
 * Print invisibility
 */
static bool prt_invisible(byte r_margin)
{
	if (p_ptr->invisible > 0)
	{
		int i = p_ptr->invisible;
		int a;

		if      (i >= 55) a = TERM_PURPLE;
		else if (i >= 45) a = TERM_BLUE;
		else if (i >= 35) a = TERM_L_BLUE;
		else if (i >= 25) a = TERM_WHITE;
		else if (i >= 15) a = TERM_L_WHITE;
		else              a = TERM_SLATE;
		if (c_roff_insert(a, "Invisible", r_margin)) return (1);
	}

	/* Success */
	return (0);
}

/*
 * Print oppositions
 */
static bool prt_oppose(byte r_margin)
{
	/* Show oppositions.  Hack -- check space manually */
	if ((p_ptr->oppose_acid) || (p_ptr->oppose_elec) ||
	    (p_ptr->oppose_fire) || (p_ptr->oppose_cold) ||
	    (p_ptr->oppose_pois) || (p_ptr->oppose_ethereal))
	{
		int y, x;

		/* Too little space  XXX */
		if (c_roff_insert(TERM_DARK, "A E F C P L", r_margin)) return (1);

		/* Locate the cursor */
		(void)Term_locate(&x, &y);

		/* Print the oppositions */
		if (p_ptr->oppose_acid)
			(void)Term_putch(x - 11, y, TERM_SLATE,  'A');
		if (p_ptr->oppose_elec)
			(void)Term_putch(x -  9, y, TERM_BLUE,   'E');
		if (p_ptr->oppose_fire)
			(void)Term_putch(x -  7, y, TERM_RED,    'F');
		if (p_ptr->oppose_cold)
			(void)Term_putch(x -  5, y, TERM_WHITE,  'C');
		if (p_ptr->oppose_pois)
			(void)Term_putch(x -  3, y, TERM_GREEN,  'P');
		if (p_ptr->oppose_ethereal)
			(void)Term_putch(x -  1, y, TERM_YELLOW, 'L');
	}

	/* Success */
	return (0);
}



/*
 * Display an item in the customized portion of the left panel.  -LM-
 */
static void left_panel_display_aux(byte item, byte row, int tmp)
{
	/* Main screen must be active */
	if (main_screen_inactive) return;

	/* Clear the row */
	put_str("             ", row, 0);

	/* Display the item */
	switch (item)
	{
		case DISPLAY_HEALTH:
		{
			int ty, tx;

			/* This is the nth health bar, so find the nth closest monster */
			get_closest_los_monster(tmp, p_ptr->py, p_ptr->px,
			   &ty, &tx, TRUE);

			/* We have a monster */
			if (ty && tx)
			{
				s32b perc;
				int attr = TERM_RED;
				int attr2;
				int path;
				char m_name[DESC_LEN];

				/* Get the monster index */
				int m_idx = cave_m_idx[ty][tx];

				/* Get the monster */
				monster_type *m_ptr = &m_list[m_idx];

				/* Get the monster race */
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Paranoia - Must be a valid monster */
				if (!m_idx) break;

				/* Find out how hurt the monster is */
				perc = 100L * m_ptr->hp / m_ptr->maxhp;

				/* Get the monster name */
				if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
				{
					/* Get the ghost name. */
					strcpy(m_name, ghost_name);

					/* Build the ghost name. */
					strcat(m_name, ", the ");
					strcat(m_name, r_name + r_ptr->name);
				}
				else
				{
					strcpy(m_name, r_name + r_ptr->name);
				}

				/* Badly wounded */
				if (perc >= 10) attr = TERM_L_RED;

				/* Wounded */
				if (perc >= 25) attr = TERM_ORANGE;

				/* Somewhat Wounded */
				if (perc >= 60) attr = TERM_YELLOW;

				/* Healthy */
				if (perc >= 100) attr = TERM_L_GREEN;

				/* Note asleep */
				if (m_ptr->csleep) attr = TERM_BLUE;


				/* Special case -- limited visibility or hallucination */
				if ((m_ptr->ml < ML_FULL) || (p_ptr->image))
				{
					strcpy(m_name, "??????????");
					attr = TERM_WHITE;
				}

				/* Check line of fire */
				path = projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px,
					PROJECT_CHCK);

				/* We have a clear, direct line of fire */
				if (path == PROJECT_CLEAR) attr2 = TERM_L_GREEN;

				/* We have an obstructed line of fire */
				else if (path == PROJECT_NOT_CLEAR) attr2 = TERM_WHITE;

				/* We have no line of fire */
				else if (path == PROJECT_NO) attr2 = TERM_SLATE;

				/* We have any other line of fire */
				else attr2 = TERM_L_DARK;


				/* Display this monster, colored according to health */
				c_put_str(attr2, "[", row, 0);
				c_put_str(attr, format("%-10.10s", m_name), row, 1);
				c_put_str(attr2, "]", row, 11);
			}
			break;
		}
		case DISPLAY_TOTAL_KILLS:
		{
			s32b k = p_ptr->total_kills;

			put_str("Kills", row, 0);

			if (k > 99999L) c_put_str(TERM_L_GREEN, "*****", row, 7);
			else c_put_str(TERM_L_GREEN, format("%5ld", k), row, 7);
			break;
		}
		case DISPLAY_SCORE:
		{
			s32b tp = total_points();

			put_str("Score", row, 0);

			if (ABS(tp) > 99999L) c_put_str(TERM_WHITE, "*****", row, 7);
			else c_put_str(TERM_WHITE, format("%5ld", tp), row, 7);
			break;
		}
		case DISPLAY_FAME:
		{
			int a;
			char fame_desc[DESC_LEN];

			/* Get and print a color and description of fame */
			get_fame_desc(&a, fame_desc);
			c_put_str(a, fame_desc, row, 0);
			break;
		}

		/* Display time elapsed  -clefs- */
		case DISPLAY_TIME:
		{
			s32b len = 10L * TOWN_DAWN;
			s32b tick = turn % len;

			s32b day = turn / len;
			s32b hour = (24L * tick / len) % 24;

			/*
			 * Note the lack of a minutes indicator.  The passage of time
			 * should be "fuzzy".
			 */
			c_put_str(TERM_WHITE,
					format("%3ld day%s %2ldh", day, (day == 1) ? "" : "s", hour),
					row, 0);

			break;
		}
		case DISPLAY_LUCK:
		{
			int l = p_ptr->luck;

			if      (l <  40) c_put_str(TERM_RED,     "Luck ", row, 0);
			else if (l <  60) c_put_str(TERM_L_RED,   "Luck ", row, 0);
			else if (l <  80) c_put_str(TERM_ORANGE,  "Luck ", row, 0);
			else if (l < 100) c_put_str(TERM_YELLOW,  "Luck ", row, 0);
			else              c_put_str(TERM_L_GREEN, "Luck ", row, 0);
			break;
		}
		case DISPLAY_REGEN:
		{
			int rh = p_ptr->life_recovery_value;
			int rm = p_ptr->mana_recovery_value;
			int attr;

			/* Assume normal if character updates are silenced (beginning of game) */
			if (!rh && !rm && character_silent) rh = rm = PY_REGEN_NORMAL;

			put_str("Regen: ", row, 0);

			/* Always print HP recovery */
			if (TRUE)
			{
				if (rh == 0) attr = TERM_RED;
				else if (rh < PY_REGEN_NORMAL / 2)      attr = TERM_ORANGE;
				else if (rh < PY_REGEN_NORMAL)          attr = TERM_YELLOW;
				else if (rh == PY_REGEN_NORMAL)         attr = TERM_WHITE;
				else if (rh <= 3 * PY_REGEN_NORMAL / 2) attr = TERM_L_GREEN;
				else if (rh <= PY_REGEN_NORMAL * 2)     attr = TERM_GREEN;
				else if (rh <= PY_REGEN_NORMAL * 3)     attr = TERM_L_BLUE;
				else if (rh <= PY_REGEN_NORMAL * 4)     attr = TERM_BLUE;
				else                                    attr = TERM_PURPLE;

				c_put_str(attr, "HP", row, 7);
			}

			/* Sometimes print mana recovery too */
			if (p_ptr->msp)
			{
				if (rm == 0) attr = TERM_RED;
				else if (rm < PY_REGEN_NORMAL / 2)      attr = TERM_ORANGE;
				else if (rm < PY_REGEN_NORMAL)          attr = TERM_YELLOW;
				else if (rm == PY_REGEN_NORMAL)         attr = TERM_WHITE;
				else if (rm <= 3 * PY_REGEN_NORMAL / 2) attr = TERM_L_GREEN;
				else if (rm <= PY_REGEN_NORMAL * 2)     attr = TERM_GREEN;
				else if (rm <= PY_REGEN_NORMAL * 3)     attr = TERM_L_BLUE;
				else if (rm <= PY_REGEN_NORMAL * 4)     attr = TERM_BLUE;
				else                                    attr = TERM_PURPLE;

				c_put_str(TERM_WHITE, "/", row, 9);
				c_put_str(attr, "MP", row, 10);
			}

			break;
		}
		/* Display active quest  -clefs- */
		case DISPLAY_QUEST:
		{
			/* Check if you're on a quest */
			if (p_ptr->cur_quest > 0)
			{
				/* Get quest information */
				int q_idx = quest_num(p_ptr->cur_quest);
				quest_type *q_ptr = &q_info[q_idx];

				/* Completed quest */
				if (!q_ptr->active_level)
				{
					c_put_str(TERM_L_BLUE, "Complete", row, 0);
				}

				/* Not completed */
				else
				{
					/* Get number of quest monsters remaining */
					int num = q_ptr->max_num - q_ptr->cur_num;
					monster_race *r_ptr = &r_info[q_ptr->r_idx];

					byte attr = TERM_WHITE;

					/* Hack -- Special colours for active quest */
					if (q_ptr->started)
					{
						attr = (p_ptr->depth == p_ptr->cur_quest) ?
							TERM_WHITE : TERM_SLATE;
					}

					/* Quest */
					c_put_str(TERM_WHITE, "Quest", row, 0);

					/* Display remaining opponents */
					c_put_str(attr, format("%2d [ ]", num), row, 6);

					/* Print monster symbol */
					(void)Term_gotoxy(10, row);
					(void)Term_addch(r_ptr->x_attr, r_ptr->x_char);
				}
			}

			/* No quest */
			else
			{
				c_put_str(TERM_L_DARK, "(no quest)", row, 0);
			}

			break;
		}
		case DISPLAY_TARGET:
		{
			/* No target */
			if (!p_ptr->target_set)
			{
				/* Note lack of target */
				c_put_str(TERM_L_DARK,  "(no target)", row, 0);
				break;
			}

			/* Check "monster" targets */
			if (p_ptr->target_who > 0)
			{
				monster_type *m_ptr = &m_list[p_ptr->target_who];

				/* We can't see the monster we are targeting */
				if (!m_ptr->ml)
				{
					/* Give no information */
					c_put_str(TERM_SLATE,  "(not visible)", row, 0);
					break;
				}

				/* Otherwise, update the target */
				p_ptr->target_row = m_ptr->fy;
				p_ptr->target_col = m_ptr->fx;
			}

			/* We have a target */
			if (TRUE)
			{
				int a = TERM_SLATE;

				int y = p_ptr->target_row;
				int x = p_ptr->target_col;
				int py = p_ptr->py;
				int px = p_ptr->px;

				int dy, dx, ay, ax;

				cptr compass = "*";
				int dist;

				/* Check LOF */
				if (player_can_fire_bold(y, x))
				{
					a = TERM_WHITE;

					/* Check line of fire for obstructions */
					if (projectable(py, px, y, x, PROJECT_CHCK) ==
					    PROJECT_CLEAR)
					{
						a = TERM_L_GREEN;
					}
				}

				/* Get the change in position needed to get to the target */
				dy = y - py;
				dx = x - px;

				/* Target is not under us */
				if ((dy != 0) || (dx != 0))
				{
					/* Calculate vertical and horizontal distances */
					ay = ABS(dy);
					ax = ABS(dx);

					/* We mostly want to move vertically */
					if (ay > (ax * 2))
					{
						if (dy < 0) compass = "N";
						else        compass = "S";
					}

					/* We mostly want to move horizontally */
					else if (ax > (ay * 2))
					{
						if (dx < 0) compass = "W";
						else        compass = "E";
					}

					/* We want to move up and sideways */
					else if (dy < 0)
					{
						if (dx < 0) compass = "NW";
						else        compass = "NE";
					}

					/* We want to move down and sideways */
					else
					{
						if (dx < 0) compass = "SW";
						else        compass = "SE";
					}
				}

				/* Get distance to target */
				dist = distance(py, px, y, x);

				/* Print the direction */
				c_put_str(a, format("(%s %d)", compass, dist), row, 0);
			}

			break;
		}

		/* Display various conditions usually gained by spells */
		case DISPLAY_REALM_COND:
		{
			/* Start at column 0 */
			int col = 0;

			/* Wizardly conditions */
			if (p_ptr->blink_away)
				c_put_str(TERM_L_BLUE, "B ", row, col +=2);
			if (p_ptr->evasion)
				c_put_str(TERM_L_BLUE, "E ", row, col +=2);
			if (p_ptr->dancing_feet)
				c_put_str(TERM_L_BLUE, "D ", row, col +=2);
			if (p_ptr->phasing_foes)
				c_put_str(TERM_L_BLUE, "P ", row, col +=2);
			if (p_ptr->aura_fire)
				c_put_str(TERM_RED, "AF ", row, col +=3);
			if (p_ptr->aura_cold)
				c_put_str(TERM_WHITE, "AC ", row, col +=3);
			if (p_ptr->wiz_prot)
				c_put_str(TERM_PURPLE, "W ", row, col +=2);
			if (p_ptr->forbid_summoning)
				c_put_str(TERM_L_BLUE, "S ", row, col +=2);
			if (p_ptr->shield)
				c_put_str(TERM_L_UMBER, "S ", row, col +=2);

			/* Priestly conditions */
			if (p_ptr->holy)
				c_put_str(TERM_BLUE, "H ", row, col +=2);
			else if (p_ptr->blessed)
			{
				if (p_ptr->realm == PRIEST)
					c_put_str(TERM_L_BLUE, "B ", row, col +=2);
				else if (p_ptr->realm == NECRO)
					c_put_str(TERM_PURPLE, "B ", row, col +=2);
			}
			if ((p_ptr->bold) && (p_ptr->realm == PRIEST))
				c_put_str(TERM_WHITE, "B ", row, col +=2);
			if (p_ptr->detect_inv)
				c_put_str(TERM_WHITE, "S ", row, col +=2);
			if (p_ptr->protevil)
			{
				if (p_ptr->realm == PRIEST)
					c_put_str(TERM_L_BLUE, "P ", row, col +=2);
				else if (p_ptr->realm == NECRO)
					c_put_str(TERM_PURPLE, "P ", row, col +=2);
			}

			/* Necromantic conditions */
			if (p_ptr->tim_infra)
				c_put_str(TERM_L_WHITE, "I ", row, col +=2);
			if (p_ptr->wraithform)
				c_put_str(TERM_PURPLE, "W ", row, col +=2);
			if (p_ptr->necro_rage)
				c_put_str(TERM_PURPLE, "R ", row, col +=2);
			if (p_ptr->mental_barrier)
				c_put_str(TERM_PURPLE, "M ", row, col +=2);

			break;
		}
		case DISPLAY_SPECIAL_ATTACK:
		{
			int col = 0;

			if (p_ptr->acid_attack)
				(void)Term_putch(col++, row, TERM_SLATE, 'A');
			if (p_ptr->elec_attack)
				(void)Term_putch(col++, row, TERM_BLUE, 'E');
			if (p_ptr->fire_attack)
				(void)Term_putch(col++, row, TERM_RED, 'F');
			if (p_ptr->cold_attack)
				(void)Term_putch(col++, row, TERM_WHITE, 'C');
			if (p_ptr->pois_attack)
				(void)Term_putch(col++, row, TERM_GREEN, 'P');

			if (p_ptr->special_attack & (ATTACK_CONFUSE))
					(void)Term_putch(col++, row, TERM_L_UMBER, '?');
			if (p_ptr->special_attack & (ATTACK_BLKBRTH))
					(void)Term_putch(col++, row, TERM_PURPLE, '!');
			if (p_ptr->special_attack & (ATTACK_FLEE))
					(void)Term_putch(col++, row, TERM_WHITE, '*');
			if (p_ptr->special_attack & (ATTACK_HOLY))
					(void)Term_putch(col++, row, TERM_YELLOW, 'H');
			if (p_ptr->special_attack & (ATTACK_VORPAL))
					(void)Term_putch(col++, row, TERM_L_UMBER, 'V');

			if (p_ptr->special_attack & (ATTACK_PIERCING))
					(void)Term_putch(col++, row, TERM_PURPLE, '^');
			if (p_ptr->special_attack & (ATTACK_DEADLY))
					(void)Term_putch(col++, row, TERM_ORANGE, '^');
			if (p_ptr->special_attack & (ATTACK_IMPACT))
					(void)Term_putch(col++, row, TERM_L_UMBER, '^');
			if (p_ptr->special_attack & (ATTACK_ACCURATE))
					(void)Term_putch(col++, row, TERM_L_BLUE, '^');
			if (p_ptr->special_attack & (ATTACK_SHOT_FIRE))
					(void)Term_putch(col++, row, TERM_RED, '^');
			if (p_ptr->special_attack & (ATTACK_SHOT_COLD))
					(void)Term_putch(col++, row, TERM_WHITE, '^');
			if (p_ptr->special_attack & (ATTACK_BARD))
					(void)Term_putch(col++, row, TERM_L_BLUE, 'B');

			break;
		}
		case DISPLAY_OPPOSE:
		{
			move_cursor(row, 0);
			prt_oppose(13);
			break;
		}
		case DISPLAY_FEAR:
		{
			if (p_ptr->afraid)
				c_put_str(TERM_ORANGE, "Afraid", row, 0);
			else if ((p_ptr->berserk) && (p_ptr->hero))
				c_put_str(TERM_PURPLE, "Berserk/Hero", row, 0);
			else if (p_ptr->berserk)
				c_put_str(TERM_PURPLE, "Berserk", row, 0);
			else if (p_ptr->hero)
				c_put_str(TERM_ORANGE, "Heroic", row, 0);
			else if ((p_ptr->bold) && ((p_ptr->realm != PRIEST) ||
			          (!left_panel_display(DISPLAY_REALM_COND, 1))))
				c_put_str(TERM_WHITE, "Bold", row, 0);

			break;
		}
		case DISPLAY_PROT_BLESS:
		{
			/* Start at column 0 */
			int col = 0;

			/* Must not be showing blessing in another display */
			if (((p_ptr->realm != PRIEST) && (p_ptr->realm != NECRO)) ||
			    (!left_panel_display(DISPLAY_REALM_COND, 1)))
			{
				if (p_ptr->holy)
					c_put_str(TERM_BLUE, "H ", row, col +=2);
				else if (p_ptr->blessed)
				{
					if (p_ptr->realm == NECRO)
						c_put_str(TERM_PURPLE, "B ", row, col +=2);
					else if (p_ptr->realm == PRIEST)
						c_put_str(TERM_L_BLUE, "B ", row, col +=2);
					else
						c_put_str(TERM_L_WHITE, "B ", row, col +=2);
				}
				if (p_ptr->protevil)
					c_put_str(TERM_L_WHITE, "P ", row, col +=3);
			}

			if ((p_ptr->shield) && (!left_panel_display(DISPLAY_REALM_COND, 1)))
				c_put_str(TERM_L_UMBER, "S ", row, col +=2);
			if (p_ptr->steelskin)
				c_put_str(TERM_SLATE, "S ", row, col +=2);
			if (p_ptr->res_dam)
				c_put_str(TERM_UMBER, "D ", row, col +=2);

			break;
		}
		case DISPLAY_WEATHER:
		{
			byte attr_humid[15] =
				{'y', 'u', 'U', 'w', 'W', 's', 'D',
				 'D',
				 'D', 's', 'W', 'w', 'G', 'B', 'b'};
			byte attr_wind[15]  =
				{'v', 'b', 'B', 'w', 'W', 's', 'D',
				 'D',
				 'D', 's', 'W', 'w', 'B', 'b', 'v',};
			byte attr_temp[15]  =
				{'b', 'b', 'B', 'w', 'W', 's', 'D',
				 'D',
				 'D', 's', 'w', 'y', 'o', 'R', 'r',};

			cptr str;
			byte attr;

			/* Weather only matters for druids */
			if (p_ptr->realm != DRUID) break;

			/* Determine string and attr for humidity */
			if      (p_ptr->humid_forecast == -7) str = "*D*";
			else if (p_ptr->humid_forecast <= -5) str = "d++";
			else if (p_ptr->humid_forecast <= -3) str = " d+";
			else if (p_ptr->humid_forecast <= -1) str = " d ";
			else if (p_ptr->humid_forecast ==  7) str = "*W*";
			else if (p_ptr->humid_forecast >=  5) str = "w++";
			else if (p_ptr->humid_forecast >=  3) str = " w+";
			else if (p_ptr->humid_forecast >=  1) str = " w ";
			else                                  str = " - ";

			attr = color_char_to_attr(attr_humid[p_ptr->humid_forecast + 7]);
			c_put_str(attr, format("%.3s", str), row, 0);

			/* Determine string and attr for wind */
			if      (p_ptr->wind_forecast == -7) str = "*S*";
			else if (p_ptr->wind_forecast <= -5) str = "s++";
			else if (p_ptr->wind_forecast <= -3) str = " s+";
			else if (p_ptr->wind_forecast <= -1) str = " s ";
			else if (p_ptr->wind_forecast ==  7) str = "*B*";
			else if (p_ptr->wind_forecast >=  5) str = "b++";
			else if (p_ptr->wind_forecast >=  3) str = " b+";
			else if (p_ptr->wind_forecast >=  1) str = " b ";
			else                                str = " - ";

			attr = color_char_to_attr(attr_wind[p_ptr->wind_forecast + 7]);
			c_put_str(attr, format("%.3s", str), row, 4);

			/* Determine string and attr for temperature */
			if      (p_ptr->temp_forecast == -7) str = "*C*";
			else if (p_ptr->temp_forecast <= -5) str = "c++";
			else if (p_ptr->temp_forecast <= -3) str = " c+";
			else if (p_ptr->temp_forecast <= -1) str = " c ";
			else if (p_ptr->temp_forecast ==  7) str = "*H*";
			else if (p_ptr->temp_forecast >=  5) str = "h++";
			else if (p_ptr->temp_forecast >=  3) str = " h+";
			else if (p_ptr->temp_forecast >=  1) str = " h ";
			else                                 str = " - ";

			attr = color_char_to_attr(attr_temp[p_ptr->temp_forecast + 7]);
			c_put_str(attr, format("%.3s", str), row, 8);

			break;
		}
		case DISPLAY_NOISE:
		{
			int n = total_wakeup_chance;

			if (p_ptr->aggravate) c_put_str(TERM_L_RED, "Aggravating", row, 0);

			else if (n <   70) c_put_str(TERM_L_DARK,  "shadowsilent", row, 0);
			else if (n <  125) c_put_str(TERM_L_DARK,  "silent", row, 0);
			else if (n <  250) c_put_str(TERM_L_DARK,  "whisper", row, 0);
			else if (n <  450) c_put_str(TERM_SLATE,   "very quiet", row, 0);
			else if (n <  750) c_put_str(TERM_SLATE,   "quiet", row, 0);
			else if (n < 1100) c_put_str(TERM_SLATE,   "noise", row, 0);
			else if (n < 1700) c_put_str(TERM_L_WHITE, "noise", row, 0);
			else if (n < 2400) c_put_str(TERM_WHITE,   "noise", row, 0);
			else if (n < 3100) c_put_str(TERM_YELLOW,  "noise", row, 0);
			else if (n < 4000) c_put_str(TERM_ORANGE,  "noise", row, 0);
			else if (n < 5000) c_put_str(TERM_L_RED,   "noise", row, 0);
			else if (n <10000) c_put_str(TERM_RED,     "Noise!", row, 0);
			else               c_put_str(TERM_RED,     "*Noise*", row, 0);

			break;
		}
		case DISPLAY_INVISIBILITY:
		{
			move_cursor(row, 0);
			prt_invisible(13);
			break;
		}

		default:
		{
			break;
		}
	}
}


/*
 * Display the customized portion of the left panel.  -LM-
 */
static void print_left_panel(bool health_bars_only)
{
	int i, row;
	int health_bars = 0;

	/* Refresh all the customized displays */
	for (i = 0; i < term_main->rows - ROW_CUSTOM - 1; i++)
	{
		int item = custom_display[i];

		/* We want to display something */
		if (item)
		{
			/* Option to display only health bars */
			if ((health_bars_only) && (item != DISPLAY_HEALTH)) continue;

			/* Calculate display row */
			row = ROW_CUSTOM + i + 1;

			/* Hack -- Count health bars */
			if (item == DISPLAY_HEALTH) health_bars++;

			/* Display the item, count health bars */
			left_panel_display_aux(item, row, health_bars);
		}
	}
}


/*
 * Given a display item, determine if it has been chosen for display
 * and, if requested, display it in its proper place on the left panel.
 *
 * Mode 0 - display this item
 * Mode 1 - note if we do display this item
 */
bool left_panel_display(byte item, byte mode)
{
	int i, row;

	/* Do we want to display this item?  If so, where? */
	for (i = 0; i < term_main->rows - ROW_CUSTOM - 1; i++)
	{
		/* Accept matches */
		if (custom_display[i] == item) break;
	}

	/* No match */
	if (i >= term_main->rows - ROW_CUSTOM - 1) return (FALSE);

	/* Just note whether we display the item or not */
	if (mode == 1) return (TRUE);


	/* Special case -- monster health bars */
	if (item == DISPLAY_HEALTH)
	{
		print_left_panel(TRUE);
		return (FALSE);
	}

	/* Calculate display row */
	row = ROW_CUSTOM + i + 1;

	/* Display the item */
	left_panel_display_aux(item, row, 0);

	/* Return */
	return (TRUE);
}



/*
 * Display a priority-ordered queue of conditions.  -LM-
 */
static void prt_conditions(void)
{
	/* Get right margin (leave a blank space) */
	byte r_margin = COL_STATE - 2;

	char buf[256];

	int length = COL_STATE - COL_CONDITIONS;

	/* Get a blank string the length of the conditions display */
	center_string(buf, sizeof(buf), "", length);

	/* Clear the conditions display */
	c_put_str(TERM_WHITE, buf, ROW_STATUS, COL_CONDITIONS);


	/* Move the cursor */
	move_cursor(ROW_STATUS, COL_CONDITIONS);

	/* Show Black Breath */
	if (p_ptr->black_breath)
	{
		if (c_roff_insert(TERM_RED, "Black Breath", r_margin)) return;
	}

	/* Show self knowledge */
	if (p_ptr->self_knowledge)
	{
		if (c_roff_insert(TERM_L_BLUE, "Self Knowledge", r_margin)) return;
	}

	/* Show wounds */
	if (p_ptr->cut)
	{
		if (prt_cut(r_margin)) return;
	}

	/* Show stunning */
	if (p_ptr->stun)
	{
		if (prt_stun(r_margin)) return;
	}

	/* Show blindness */
	if (p_ptr->blind)
	{
		if (c_roff_insert(TERM_ORANGE, "Blind", r_margin)) return;
	}

	/* Show confusion */
	if (p_ptr->confused)
	{
		if (c_roff_insert(TERM_ORANGE, "Confused", r_margin)) return;
	}

	/* Show poison */
	if (p_ptr->poisoned)
	{
		if (prt_poisoned(r_margin)) return;
	}

	/* Show disease */
	if (p_ptr->diseased)
	{
		if (prt_diseased(r_margin)) return;
	}

	/* Show fear.  Otherwise show necro rage, berserk, heroism, or boldness  */
	if ((p_ptr->afraid) && (!left_panel_display(DISPLAY_FEAR, 1)))
	{
		if (c_roff_insert(TERM_ORANGE, "Afraid", r_margin)) return;
	}
	else if ((p_ptr->necro_rage) && (!left_panel_display(DISPLAY_REALM_COND, 1)))
	{
		if (c_roff_insert(TERM_PURPLE, "Raging", r_margin)) return;
	}
	else if ((p_ptr->berserk) && (!left_panel_display(DISPLAY_FEAR, 1)))
	{
		if (c_roff_insert(TERM_PURPLE, "Berserk", r_margin)) return;
	}
	else if ((p_ptr->hero) && (!left_panel_display(DISPLAY_FEAR, 1)))
	{
		if (c_roff_insert(TERM_L_GREEN, "Heroic", r_margin)) return;
	}
	else if ((p_ptr->bold) && (!left_panel_display(DISPLAY_FEAR, 1)))
	{
		if (c_roff_insert(TERM_WHITE, "Bold", r_margin)) return;
	}

	/* Show recall */
	if (p_ptr->word_recall)
	{
		if (c_roff_insert(TERM_WHITE, "Recall", r_margin)) return;
	}

	/* Show invisibility */
	if ((p_ptr->invisible > 0) && (!left_panel_display(DISPLAY_INVISIBILITY, 1)))
	{
		prt_invisible(r_margin);
	}

	/* Show mania */
	if (p_ptr->mania)
	{
		if (c_roff_insert(TERM_L_RED, "Mania", r_margin)) return;
	}

	/* Show blessed/holy */
	if ((p_ptr->blessed) || (p_ptr->holy))
	{
		/*
		 * Don't show if bless displayed, or if realm is displayed by a
		 * priest or necromancer.
		 */
		if ((!left_panel_display(DISPLAY_PROT_BLESS, 1)) &&
		    ((!left_panel_display(DISPLAY_REALM_COND, 1)) ||
		     ((p_ptr->realm != PRIEST) && (p_ptr->realm != NECRO))))
		{
			if (p_ptr->holy)
			{
				if (c_roff_insert(TERM_L_BLUE, "Holy", r_margin)) return;
			}
			else if (p_ptr->blessed)
			{
				if (c_roff_insert(TERM_WHITE, "Blessed", r_margin)) return;
			}
		}
	}

	if (num_trap_on_level >= PLAYER_ALLOWED_TRAPS)
	{
		c_roff_insert(TERM_YELLOW, "Traps", r_margin);
	}

	/* Show oppositions */
	if (!left_panel_display(DISPLAY_OPPOSE, 1)) prt_oppose(r_margin);
}



/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
	byte attr = TERM_WHITE;

	char text[16];


	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		attr = TERM_RED;

		strcpy(text, "Paralyzed!");
	}

	/* Resting */
	else if (p_ptr->resting)
	{
		int i;
		int n = p_ptr->resting;

		/* Start with "Rest" */
		strcpy(text, "Rest      ");

		/* Extensive (timed) rest */
		if (n >= 1000)
		{
			i = n / 100;
			text[9] = '0';
			text[8] = '0';
			text[7] = I2D(i % 10);
			if (i >= 10)
			{
				i = i / 10;
				text[6] = I2D(i % 10);
				if (i >= 10)
				{
					text[5] = I2D(i / 10);
				}
			}
		}

		/* Long (timed) rest */
		else if (n >= 100)
		{
			i = n;
			text[9] = I2D(i % 10);
			i = i / 10;
			text[8] = I2D(i % 10);
			text[7] = I2D(i / 10);
		}

		/* Medium (timed) rest */
		else if (n >= 10)
		{
			i = n;
			text[9] = I2D(i % 10);
			text[8] = I2D(i / 10);
		}

		/* Short (timed) rest */
		else if (n > 0)
		{
			i = n;
			text[9] = I2D(i);
		}

		/* Rest until healed */
		else if (n == -1)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '*';
		}

		/* Rest until done */
		else if (n == -2)
		{
			text[5] = text[6] = text[7] = text[8] = text[9] = '&';
		}
	}

	/* Repeating */
	else if (p_ptr->command_rep)
	{
		if (p_ptr->command_rep > 999)
		{
			(void)strnfmt(text, sizeof(text), "Rep. %3d00", p_ptr->command_rep / 100);
		}
		else
		{
			(void)strnfmt(text, sizeof(text), "Repeat %3d", p_ptr->command_rep);
		}
	}

	/* Crossing */
	else if (p_ptr->crossing_dir)
	{
		if (p_ptr->crossing_moves >= 10)
			strcpy(text, "Crossing +");
		else
			(void)strnfmt(text, sizeof(text), "Crossing %d", p_ptr->crossing_moves);
	}

	/* Sneaking */
	else if (p_ptr->sneaking)
	{
		strcpy(text, "Sneaking  ");
	}

	/* Nothing interesting */
	else
	{
		strcpy(text, "          ");
	}

	/* Display the info (or blanks) */
	c_put_str(attr, text, ROW_STATUS, COL_STATE);

	/* Hack -- If character color changes with damage taken, redraw */
	if (colored_hurt_char) lite_spot(p_ptr->py, p_ptr->px);
}


/*
 * Prints the speed of a character.
 */
static void prt_speed(void)
{
	int s = p_ptr->pspeed;
	int base = p_ptr->pspeed;

	byte attr = TERM_WHITE;
	char buf[32] = "";

	/* Adjust base for temporary slowing and hasting */
	if (p_ptr->fast) base -= 10;
	if (p_ptr->slow) base += 10;

	/* Fast */
	if (s > 110)
	{
		if      (s > base) attr = TERM_GREEN;
		else if (s < base) attr = TERM_YELLOW;
		else               attr = TERM_L_GREEN;
		(void)strnfmt(buf, sizeof(buf), "Fast (%+d)", (s - 110));
	}

	/* Slow */
	else if (s < 110)
	{
		if      (s > base) attr = TERM_L_WHITE;
		else if (s < base) attr = TERM_ORANGE;
		else               attr = TERM_L_UMBER;
		(void)strnfmt(buf, sizeof(buf), "Slow (%+d)", (s - 110));
	}

	/* Normal speed */
	else
	{
		if (p_ptr->slow) attr = TERM_YELLOW;
	}

	/* Display the speed */
	c_put_str(attr, format("%-11s", buf), ROW_STATUS, COL_SPEED);
}

/*
 * Prints whether the character can cast new spells
 */
static void prt_uncast(void)
{
	if (p_ptr->uncast_spells)
	{
		if (p_ptr->uncast_spells < 10)
		{
			put_str(format("Uncast: %d", p_ptr->uncast_spells),
				ROW_STATUS, COL_UNCAST);
		}
		else
		{
			put_str("Uncast: *", ROW_STATUS, COL_UNCAST);
		}
	}
	else
	{
		put_str("         ", ROW_STATUS, COL_UNCAST);
	}
}

/*
 * Prints depth
 */
static void prt_depth(void)
{
	char depths[32];
	int attr = TERM_WHITE;

	if (!p_ptr->depth)
	{
		strcpy(depths, "Town");
	}
	else if (depth_in_feet)
	{
		if (use_metric) (void)strnfmt(depths, sizeof(depths), "%d m", p_ptr->depth * 15);
		else (void)strnfmt(depths, sizeof(depths), "%d ft", p_ptr->depth * 50);
	}
	else
	{
		(void)strnfmt(depths, sizeof(depths), "Lev %d", p_ptr->depth);
	}

	/* Get color of level based on feeling  -JSV- */
	if ((p_ptr->depth) && (!no_feeling_yet))
	{
		if (feeling ==  1) attr = TERM_L_PURPLE;
		if (feeling ==  2) attr = TERM_RED;
		if (feeling ==  3) attr = TERM_L_RED;
		if (feeling ==  4) attr = TERM_ORANGE;
		if (feeling ==  5) attr = TERM_ORANGE;
		if (feeling ==  6) attr = TERM_YELLOW;
		if (feeling ==  7) attr = TERM_YELLOW;
		if (feeling ==  8) attr = TERM_WHITE;
		if (feeling ==  9) attr = TERM_WHITE;
		if (feeling == 10) attr = TERM_L_WHITE;
	}

	/* Right-Adjust the "depth", and clear old values */
	c_prt(attr, format("%7s", depths), ROW_STATUS, COL_DEPTH);
}


/*
 * Display basic info (left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Short name */
	prt_short_name();

	/* Title */
	prt_title();

	/* Experience */
	prt_exp();

	/* Gold */
	prt_gold();

	/* Equippy chars */
	prt_equippy();

	/* Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i);

	/* Shape, if not normal */
	prt_shape();

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();

	/* Spellpoints */
	prt_sp();

	/* Special */
	health_redraw();

	/* Custom left panel displays */
	print_left_panel(FALSE);
}


/*
 * Display extra info (below map)
 */
static void prt_frame_extra(void)
{
	/* Food */
	prt_hunger();

	/* Various conditions */
	prt_conditions();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

	/* Uncast spells */
	prt_uncast();

	/* Current depth */
	prt_depth();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_INVEN))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display inventory */
		display_inven();

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}

/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_EQUIP))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display equipment */
		display_equip();

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}

/*
 * Hack -- display player in sub-windows (mode 0)
 */
static void fix_player_0(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_0))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display player */
		display_player(0, FALSE);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}



/*
 * Hack -- display player in sub-windows (mode 2)
 */
static void fix_player_1(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_PLAYER_1))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display flags */
		display_player(2, FALSE);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display recent messages in sub-windows
 *
 * Adjust for width and split messages.  XXX XXX XXX
 */
static void fix_message(void)
{
	int j, i;
	int w, h;
	int x, y;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MESSAGE))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Get size */
		(void)Term_get_size(&w, &h);

		/* Dump messages */
		for (i = 0; i < h; i++)
		{
			byte color = message_color((s16b)i);

			/* All white messages except the newest become off-white */
			if ((i != 0) && (color == TERM_WHITE)) color = TERM_L_WHITE;

			/* Dump the message on the appropriate line */
			(void)Term_putstr(0, (h - 1) - i, -1, color, message_str((s16b)i));

			/* Cursor */
			(void)Term_locate(&x, &y);

			/* Clear to end of line */
			(void)Term_erase(x, y, 255);
		}

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display overhead view in sub-windows.
 *
 * The "display_map()" function handles NULL arguments in a special manner.
 */
static void fix_overhead(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Hack -- Hide player XXX XXX XXX */
		cave_m_idx[py][px] = 0;

		/* Redraw map */
		display_map(NULL, NULL);

		/* Hack -- Show player XXX XXX XXX */
		cave_m_idx[py][px] = -1;

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_MONSTER))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->monster_race_idx) display_roff(p_ptr->monster_race_idx);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
	int j;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_OBJECT))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display object kind info */
		if (p_ptr->object_kind_idx) display_koff(p_ptr->object_kind_idx);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display monsters in sub-windows
 */
static void fix_m_list(void)
{
	int j;

	term *old = Term;

	/* Skip updating monster list while running */
	if (p_ptr->running) return;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_M_LIST))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display visible monsters (and then objects if space exists) */
		display_m_list(0, 0, TRUE);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Hack -- display objects in sub-windows
 */
static void fix_o_list(void)
{
	int j;

	term *old = Term;

	/* Skip updating object list while running */
	if (p_ptr->running) return;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_O_LIST))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Display nearby objects (and then monsters if space exists) */
		display_nearby_objects(0, 0, TRUE);

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}


/*
 * Display a chunk of text on screen.
 */
static void roff_chunk(cptr str, int l_margin, int r_margin)
{
	int y, x, h, w;

	/* Get length of string */
	int len = strlen(str);

	/* Get cursor position */
	(void)Term_locate(&x, &y);

	/* Get size of window */
	(void)Term_get_size(&w, &h);

	/* Notice that string will cross the right edge */
	if ((x > 0) && (x + len >= w))
	{
		/* Go to the next line */
		move_cursor(y + 1, l_margin);
	}

	/* Display the text */
	roff(str, l_margin, r_margin);
}


/*
 * Display list of commands in sub-windows
 */
static void fix_cmdlist(void)
{
	int j, w, h;

	term *old = Term;

	/* Scan sub-windows */
	for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* No relevant flags */
		if (!(op_ptr->window_flag[j] & (PW_CMDLIST))) continue;

		/* Activate */
		(void)Term_activate(angband_term[j]);

		/* Clear the window */
		(void)Term_clear();

		/* Position the cursor */
		(void)Term_gotoxy(0, 0);

		/* Get size of window */
		(void)Term_get_size(&w, &h);


		/* Display list of commands -- original keyset */
		if (!rogue_like_commands)
		{
			roff("   Walk    7  8  9      Run     7  8  9      Alter    7  8  9\n", 0, w);
			roff("           4     6      Shift + 4     6      Ctrl  +  4     6\n", 0, w);
			roff("           1  2  3              1  2  3               1  2  3\n", 0, w);
			if (h >= 18) roff_chunk("\n", 0, w);

			roff_chunk(" C  Character screen     ", 0, w);
			roff_chunk(" e  Equipment list       ", 0, w);
			roff_chunk(" i  Inventory list       ", 0, w);
			roff_chunk(" I  Inspect an item      ", 0, w);
			roff_chunk(" l  Look around          ", 0, w);
			roff_chunk(" s  Search               ", 0, w);
			roff_chunk(" ~  Knowledge            ", 0, w);
			roff_chunk(" ?  Help                 ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 19) roff_chunk("\n", 0, w);

			roff_chunk(" a  Aim a wand           ", 0, w);
			roff_chunk(" A  Activate an artifact ", 0, w);
			roff_chunk(" d  Drop an item         ", 0, w);
			roff_chunk(" E  Eat some food        ", 0, w);
			roff_chunk(" f  Fire an item         ", 0, w);
			roff_chunk(" F  Fuel your light      ", 0, w);
			roff_chunk(" g  Pick up objects      ", 0, w);
			roff_chunk(" q  Quaff a potion       ", 0, w);
			roff_chunk(" r  Read a scroll        ", 0, w);
			roff_chunk(" t  Take off             ", 0, w);
			roff_chunk(" u  Use a staff          ", 0, w);
			roff_chunk(" v  Throw an item        ", 0, w);
			roff_chunk(" w  Wear/wield           ", 0, w);
			roff_chunk(" z  Zap a rod            ", 0, w);
			roff_chunk(" {  Inscribe an item     ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 20) roff_chunk("\n", 0, w);

			roff_chunk(" B  Bash                 ", 0, w);
			roff_chunk(" c  Close a door         ", 0, w);
			roff_chunk(" D  Disarm a trap        ", 0, w);
			roff_chunk(" o  Open a door          ", 0, w);
			roff_chunk(" +  Alter grid           ", 0, w);
			roff_chunk(" *  Target monster       ", 0, w);
			roff_chunk(" $  Advance skills       ", 0, w);
			roff_chunk(" [  Use talents          ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 22) roff_chunk("\n", 0, w);

			/* Some commands are only valid if the character knows magic */
			if (p_ptr->realm)
			{
				roff_chunk(" b  Browse a book        ", 0, w);
				if (p_ptr->realm == PRIEST)
					roff_chunk(" p  Pray a prayer        ", 0, w);
				else if (p_ptr->realm == DRUID)
					roff_chunk(" p  Druidic technique    ", 0, w);
				else if (p_ptr->realm == NECRO)
					roff_chunk(" m  Perform a ritual     ", 0, w);
				else
					roff_chunk(" m  Cast a spell         ", 0, w);

				roff_chunk("\n", 0, w);
				if (h >= 21) roff_chunk("\n", 0, w);
			}

			roff_chunk(" R, Tab   Rest           ", 0, w);
			roff_chunk(" =  Set options          ", 0, w);
			roff_chunk("^S  Save        ", 0, w);
			roff_chunk("^X  Exit   \n", 0, w);
		}

		/* Display list of commands -- roguelike keyset */
		else
		{
			roff("   Walk    y  k  u       Run    Y  K  U      Alter    y  k  u\n", 0, w);
			roff("           h     l              H     L      Ctrl  +  h     l\n", 0, w);
			roff("           b  j  n              B  J  N               b  j  n\n", 0, w);
			if (h >= 18) roff_chunk("\n", 0, w);

			roff_chunk(" C  Character screen     ", 0, w);
			roff_chunk(" e  Equipment list       ", 0, w);
			roff_chunk(" i  Inventory list       ", 0, w);
			roff_chunk(" I  Inspect object       ", 0, w);
			roff_chunk(" s  Search               ", 0, w);
			roff_chunk(" x  Examine area         ", 0, w);
			roff_chunk(" ~  Knowledge            ", 0, w);
			roff_chunk(" ?  Help                 ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 19) roff_chunk("\n", 0, w);

			roff_chunk(" a  Aim a rod            ", 0, w);
			roff_chunk(" A  Activate an artifact ", 0, w);
			roff_chunk(" d  Drop an item         ", 0, w);
			roff_chunk(" E  Eat some food        ", 0, w);
			roff_chunk(" F  Fuel your light      ", 0, w);
			roff_chunk(" g  Pick up objects      ", 0, w);
			roff_chunk(" q  Quaff a potion       ", 0, w);
			roff_chunk(" r  Read a scroll        ", 0, w);
			roff_chunk(" t  Fire an item         ", 0, w);
			roff_chunk(" T  Take off             ", 0, w);
			roff_chunk(" v  Throw an item        ", 0, w);
			roff_chunk(" w  Wear/wield           ", 0, w);
			roff_chunk(" z  Zap a wand           ", 0, w);
			roff_chunk(" Z  Use a staff          ", 0, w);
			roff_chunk(" {  Inscribe an item     ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 20) roff_chunk("\n", 0, w);

			roff_chunk(" c  Close a door         ", 0, w);
			roff_chunk(" D  Disarm a trap        ", 0, w);
			roff_chunk(" f  Force a door         ", 0, w);
			roff_chunk(" o  Open a door          ", 0, w);
			roff_chunk(" +  Alter grid           ", 0, w);
			roff_chunk(" *  Target monster       ", 0, w);
			roff_chunk(" $  Advance skills       ", 0, w);
			roff_chunk(" [  Use talents          ", 0, w);
			roff_chunk("\n", 0, w);
			if (h >= 22) roff_chunk("\n", 0, w);

			/* Some commands are only valid if the character knows magic */
			if (p_ptr->realm)
			{
				roff_chunk(" P  Peruse a book        ", 0, w);
				if (p_ptr->realm == PRIEST)
					roff_chunk(" p  Pray a prayer        ", 0, w);
				else if (p_ptr->realm == DRUID)
					roff_chunk(" p  Druidic technique    ", 0, w);
				else if (p_ptr->realm == NECRO)
					roff_chunk(" m  Perform a ritual     ", 0, w);
				else
					roff_chunk(" m  Cast a spell         ", 0, w);

				roff_chunk("\n", 0, w);
				if (h >= 21) roff_chunk("\n", 0, w);
			}

			roff_chunk(" R, Tab   Rest           ", 0, w);
			roff_chunk(" =  Set options          ", 0, w);
			roff_chunk("^S  Save        ", 0, w);
			roff_chunk("^X  Exit   \n", 0, w);
		}

		/* Fresh */
		(void)Term_fresh();
	}

	/* Restore */
	(void)Term_activate(old);
}



/*
 * Calculate the character's spell level, and forget or remember, spells as
 * needed.  We only "forget" spells that we have already used.
 *
 * Note that this function induces various "status" messages,
 * which must be bypassed until the character is created.
 */
static void calc_spells(void)
{
	int i;

	const magic_type *s_ptr;

	cptr p;

	/* Clear uncast spells */
	p_ptr->uncast_spells = 0;

	/* Clear the spell level */
	p_ptr->spell_level = 0;

	/* Hack -- must be literate */
	if (p_ptr->realm == NONE) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Get name of spells */
	p = spell_type();


	/* Save our new spell level */
	p_ptr->spell_level = get_skill(S_MAGIC, 0, 100);


	/* Forget and remember spells */
	for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
	{
		/* Get the spell */
		s_ptr = &mp_ptr->info[i];

		/* We are allowed to know this spell */
		if (s_ptr->slevel <= p_ptr->spell_level)
		{
			/* Spell has been previously forgotten */
			if (p_ptr->spell_flags[i] & (PY_SPELL_FORGOTTEN))
			{
				/* Message */
				msg_format("You have remembered the %s of %s.", p, s_ptr->sname);

				/* No longer forgotten */
				p_ptr->spell_flags[i] &= ~(PY_SPELL_FORGOTTEN);
			}

			continue;
		}

		/* Has this spell been used and is not already forgotten? */
		if ((p_ptr->spell_flags[i] & (PY_SPELL_WORKED)) &&
		    (!(p_ptr->spell_flags[i] & (PY_SPELL_FORGOTTEN))))
		{
			/* Message */
			msg_format("You have forgotten the %s of %s.", p, s_ptr->sname);

			/* Forget it */
			p_ptr->spell_flags[i] |= (PY_SPELL_FORGOTTEN);
		}
	}


	/* Calculate uncast spells */
	if (TRUE)
	{
		/* Get the spellbook type */
		int tval = mp_ptr->spell_book;
		int sval, i;

		object_kind *k_ptr;

		/* Scan books until no more spells are found */
		for (sval = 1; sval <= 10; sval++)
		{
			/* This book is empty -- stop now */
			if (!mp_ptr->book_start_index[sval] -
			     mp_ptr->book_start_index[sval-1]) break;

			/* Get the book */
			k_ptr = &k_info[lookup_kind(tval, sval)];

			/* Ignore empty */
			if (!k_ptr->name) continue;

			/* This book has been seen at some point */
			if (k_ptr->special & (SPECIAL_EVER_SEEN))
			{
				/* Scan all the spells */
				for (i = mp_ptr->book_start_index[sval-1]; i < mp_ptr->book_start_index[sval]; i++)
				{
					/* Get the spell */
					s_ptr = &mp_ptr->info[i];

					/* This spell is legal and as yet unused */
					if ((s_ptr->slevel <= p_ptr->spell_level) &&
					    (!(p_ptr->spell_flags[i] & (PY_SPELL_WORKED))))
					{
						/* Count it */
						p_ptr->uncast_spells++;
					}
				}
			}
		}
	}


	/* Redraw Study Status */
	p_ptr->redraw |= (PR_UNCAST);
}


/*
 * Weigh the player's armor
 */
static int armor_weight(void)
{
	int cur_wgt = 0;

	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	if (!(p_ptr->twoweap)) cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;
	return (cur_wgt);
}

/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor, and
 * by a shapeshift.
 *
 * Mana is (almost) halved if character has not taken an Oath.
 *
 * This function induces status messages.
 */
void calc_mana(void)
{
	int msp, cur_wgt, max_wgt;
	int skill, power;
	int spell_stat_index = p_ptr->stat_ind[mp_ptr->spell_stat];

	bool old_cumber_armor = p_ptr->cumber_armor;

	/* Hack -- Must know a realm of magic */
	if (p_ptr->realm == NONE)
	{
		p_ptr->msp = 0;
		return;
	}

	skill = get_skill(S_MPOWER, 0, 100);

	/* Get mana level (~60% for non-Oath spellcasters) (10x inflation) */
	if (oath_caster)
	{
		power = get_skill(S_MPOWER, 0, 1000);
	}

	/* Give medium mana at low levels */
	else if (skill < 20)
	{
		power = get_skill(S_MPOWER, 0, 800);
	}
	else
	{
		power = 50 + get_skill(S_MPOWER, 0, 550);
	}

	/* Calculate total mana, using spell stat and character power */
	msp = 4 + ((adj_mag_mana[spell_stat_index] * power) + 100) / 200;

	/* Take mana bonus into account */
	msp += p_ptr->mana_bonus;


	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = armor_weight();

	/* Determine the weight allowance */
	if (p_ptr->realm)
	{
		max_wgt = mp_ptr->spell_weight;

		/* A high dodging skill relaxes armor restrictions */
		max_wgt += get_skill(S_DODGING, 0, 100);

		/* Heavy armor penalizes mana by a percentage. */
		if (((cur_wgt - max_wgt) / 10) > 0)
		{
			/* Encumbered */
			p_ptr->cumber_armor = TRUE;

			/*
			 * Pure spellcasters lose half their mana if their armor
			 * is twice as heavy as max_wgt (ranges between 50 and 80
			 * pounds)
			 */
			if (oath_caster)
			{
				msp -= msp * (cur_wgt - max_wgt) / (max_wgt * 2);
			}

			/*
			 * Non-specialist spellcasters lose half their mana if their
			 * armor is four times as heavy as max_wgt (ranges between
			 * 100 and 160 pounds)
			 */
			else
			{
				msp -= msp * (cur_wgt - max_wgt) / (max_wgt * 6);
			}
		}
	}

	/* Any non-humanoid shape penalizes mana. */
	if (p_ptr->schange)
	{
		/* Take a third of the mana away. */
		msp = 2 * msp / 3;
	}


	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
		int old_msp = p_ptr->msp;

		/* Save new limit */
		p_ptr->msp = msp;

		/* Scale old mana correctly  - JM */
		if (!old_msp) p_ptr->csp = p_ptr->msp;
		else p_ptr->csp = div_round(p_ptr->csp * p_ptr->msp, old_msp);

		/* Enforce new limit */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}


	/* Hack -- optionally silence messages */
	if (character_silent) return;


	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			msg_print("The weight of your armor encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}
	}
}



/*
 * Calculate the character's (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
void calc_hitpoints(void)
{
	int bonus, mhp;


	/* Pure warriors get a large bonus (up to 200) */
	if (p_ptr->oath & (OATH_OF_IRON)) bonus = 2 * p_ptr->power;

	/* Guild-Burglars get a relatively small bonus (up to 133) */
	else if (p_ptr->oath & (BURGLARS_GUILD)) bonus = 4 * p_ptr->power / 3;

	/* Everyone else starts off with a medium-sized bonus (up to 150) */
	else bonus = 3 * p_ptr->power / 2;


	/* Pure spellcasters eventually lose all their bonus */
	if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
	    (p_ptr->oath & (COVENANT_OF_FAITH)) ||
	    (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
	    (p_ptr->oath & (BLACK_MYSTERY)))
	{
		bonus -= MIN(150, 3 * p_ptr->pskills[S_MAGIC].cur / 2);
		if (bonus < 0) bonus = 0;
	}

	/* Half-spellcasters eventually lose half their bonus */
	else if (p_ptr->realm)
	{
		bonus -= MIN(75, 3 * p_ptr->pskills[S_MAGIC].cur / 4);
		if (bonus < 0) bonus = 0;
	}

	/* Un-inflate and apply Con bonus */
	bonus += ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128) *
		p_ptr->power / 4;


	/* Calculate hitpoints */
	mhp = p_ptr->player_hp[p_ptr->power - 1] + bonus;


	/* Factor in heroism and berserk strength  XXX */
	if (p_ptr->hero)    mhp += 10;
	if (p_ptr->berserk) mhp += 30;

	/* Wrestlers get +1 HP per skill level above 50 */
	if (get_skill(S_WRESTLING, 0, 100) >= 50)
	{
		mhp += get_skill(S_WRESTLING, -50, 50);
	}


	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		int old_mhp = p_ptr->mhp;

		/* Add in extra hit points from heroism and such  XXX */
		if (p_ptr->extrahp)
		{
			p_ptr->chp += p_ptr->extrahp;
			p_ptr->extrahp = 0;
		}

		/* Scale hitpoints correctly */
		if (!old_mhp) p_ptr->chp = p_ptr->mhp;
		else p_ptr->chp = div_round(p_ptr->chp * mhp, old_mhp);

		/* Save new limit */
		p_ptr->mhp = mhp;

		/* Enforce new limit */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}

/*
 * Calculate and set the current light radius.
 *
 * In Sangband, a glowing character increases any light radius, as do
 * various special conditions.  The brightest wielded object counts as
 * the light source; they are not cumulative.  The maximum light radius
 * is 5 (see cave.c).
 *
 * This function does not allow light sources of radius 0.
 */
static void calc_torch(void)
{
	int i, light;
	int reduce_light = 0;

	/* Save old light */
	s16b old_lite = p_ptr->cur_lite;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Character is glowing */
	if (p_ptr->glowing) p_ptr->cur_lite += 1;

	/* Character has a very high piety */
	if (get_skill(S_PIETY, 0, 100) >= LEV_REQ_XTRA_LIGHT)
	{
		p_ptr->cur_lite += 1;
	}

	/* Character has an aura of holiness (not cumulative with piety) */
	else if (p_ptr->holy)
	{
		p_ptr->cur_lite += 1;
	}

	/* Character has reduced light */
	if (p_ptr->drain_light)
	{
		p_ptr->cur_lite -= 1;
	}

	/* Examine all wielded objects, use the brightest */
	for (light = 0, i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		int tmp = get_object_pval(o_ptr, TR_PVAL_LIGHT);

		u32b f1, f2, f3;

		/* Get object flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Handle light sources (unless negative) */
		if ((o_ptr->tval == TV_LITE) && (tmp >= 0))
		{
			/* Ignore if doused */
			if (!(o_ptr->flags3 & (TR3_IS_LIT))) continue;

			/* Object needs fuel */
			else if (!(f3 & (TR3_NOFUEL)))
			{
				/* No fuel -- ignore */
				if (o_ptr->pval <= 0) continue;

				/* Torches with low fuel burn more dimly */
				if (o_ptr->sval == SV_LITE_TORCH)
				{
					if ((o_ptr->pval < TORCH_LOW) && (tmp > 1)) tmp -= 1;
				}
			}
		}

		/* Negative light radius -- reduce light radius later */
		if (tmp < 0) reduce_light -= tmp;

		/* Remember best */
		if (tmp > light) light = tmp;
	}

	/* Adjust light */
	p_ptr->cur_lite += (light - reduce_light);

	/* Maximum light radius is 5 */
	if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

	/* Minimum is 0 */
	if (p_ptr->cur_lite < 0) p_ptr->cur_lite = 0;

	/* Notice changes in the light radius */
	if (old_lite != p_ptr->cur_lite)
	{
		/* Update the visuals */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	}
}


/*
 * Calculate base regeneration
 */
static int calc_regen_aux(void)
{
	int regen_amount = PY_REGEN_NORMAL;

	/* Very hungry */
	if (p_ptr->food < p_ptr->food_starving)
	{
		regen_amount = 0;
	}
	else if (p_ptr->food < p_ptr->food_fainting)
	{
		regen_amount = PY_REGEN_FAINT;
	}
	else if (p_ptr->food < p_ptr->food_weak)
	{
		regen_amount = PY_REGEN_WEAK;
	}

	/* Resting */
	if (p_ptr->resting)
	{
		regen_amount = regen_amount * 2;
	}

	/* Return */
	return (regen_amount);
}

/*
 * Calculate hitpoint regeneration
 */
s16b calc_hp_regen(void)
{
	/* Get basic regeneration */
	int base = calc_regen_aux();
	int regen_amount = base;

	/* Regeneration ability -- +100% */
	if (p_ptr->regenerate) regen_amount += base;

	/* Trolls regenerate hitpoints quickly */
	if (p_ptr->schange == SHAPE_TROLL) regen_amount += base;
	else if (p_ptr->prace == RACE_HALF_TROLL) regen_amount += base / 2;

	/* Necromantic rage -- +200% */
	if (p_ptr->necro_rage > NECRO_WEAKNESS_LENGTH) regen_amount += base * 2;

	/* Berserk rage -- +100% */
	else if (p_ptr->berserk) regen_amount += base;

	/* Timed HP regen or vitality -- +100% */
	if ((p_ptr->regen_hp) || (p_ptr->vitality)) regen_amount += base;

	/* Various things interfere with healing */
	if ((!p_ptr->vitality) && (!p_ptr->necro_rage))
	{
		if (p_ptr->paralyzed) regen_amount = 0;
		if (p_ptr->poisoned) regen_amount = 0;
		if (p_ptr->diseased) regen_amount = 0;
		if (p_ptr->cut) regen_amount = 0;

		if (p_ptr->stun >= HVY_STUN) regen_amount = 0;
		else if (p_ptr->stun)
		{
			regen_amount -= regen_amount * p_ptr->stun / HVY_STUN;
		}

		if (p_ptr->soulsteal && p_ptr->soul_reserve == 0) regen_amount = 0;
	}

	/* The Black Breath always hurts recovery */
	if (p_ptr->black_breath) regen_amount /= 2;

	/* Save this value */
	p_ptr->life_recovery_value = regen_amount;

	/* Return */
	return (regen_amount);
}

/*
 * Calculate mana regeneration
 */
s16b calc_mana_regen(void)
{
	/* Get basic regeneration -- MP should regen slightly faster than HP */
	int base = calc_regen_aux();
	int regen_amount = base;

	/* Regeneration ability -- +50% */
	if (p_ptr->regenerate) regen_amount += base / 2;

	/* Timed mana regen -- +100% */
	if (p_ptr->regen_mana) regen_amount += base;

	/* Necromantic rage -- no mana recovery */
	if (p_ptr->necro_rage > NECRO_WEAKNESS_LENGTH) regen_amount = 0;

	/* Berserk rage -- little mana recovery */
	else if (p_ptr->berserk) regen_amount /= 2;

	/* Save this value */
	p_ptr->mana_recovery_value = regen_amount;

	/* Return */
	return (regen_amount);
}


/*
 * Check for barehanded combat (assume nothing has been updated).
 */
static bool check_barehanded(void)
{
	/* Get the primary melee weapon */
	object_type *o_ptr = &inventory[INVEN_WIELD];

	/* Examine the shield or secondary melee weapon */
	object_type *i_ptr = &inventory[INVEN_ARM];

	/* Are we holding any weapons? */
	if (is_melee_weapon(o_ptr) || is_melee_weapon(i_ptr))
	{
		return (FALSE);
	}

	/* No weapons */
	return (TRUE);
}


/*
 * Calculate special adjustments to melee Skill.  -LM-
 */
static int add_special_melee_skill(void)
{
	int add_skill = 0, weight = 0;

	object_type *o_ptr = &inventory[INVEN_WIELD];
	object_type *i_ptr = &inventory[INVEN_ARM];


	/* Add weight of primary weapon */
	if (is_melee_weapon(o_ptr)) weight = o_ptr->weight;

	/* Add weight of secondary weapon, if appropriate */
	if (is_melee_weapon(i_ptr)) weight += i_ptr->weight;


	/*** Handle weapon weight adjustments ***/
	if (o_ptr->k_idx)
	{
		/* Warriors can handle heavy weapons unusually well */
		if (p_ptr->oath & (OATH_OF_IRON))
		{
			add_skill = get_skill(sweapon(o_ptr->tval), 25, 125) - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
		}

		/* Pious characters are middling */
		else if (p_ptr->realm == PRIEST)
		{
			/* Priests do better than the other specialist spellcasters */
			if (p_ptr->oath & (COVENANT_OF_FAITH))
			{
				add_skill = get_skill(sweapon(o_ptr->tval), 30, 55) - (weight / 4);
				if (add_skill > 0) add_skill = 0;
				if (add_skill < -20) add_skill = -20;
			}

			/* Holy warriors are good with heavy weapons */
			else
			{
				add_skill = get_skill(sweapon(o_ptr->tval), 25, 75) - (weight / 6);
				if (add_skill > 0) add_skill = 0;
				if (add_skill < -10) add_skill = -10;
			}
		}

		/* The specialist magic-users do poorly */
		else if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
		         (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
		         (p_ptr->oath & (BLACK_MYSTERY)))
		{
			add_skill = get_skill(sweapon(o_ptr->tval), 20, 90) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
		}

		/* Non-specialists are OK with heavy weapons */
		else
		{
			add_skill = get_skill(sweapon(o_ptr->tval), 25, 75) - (weight / 5);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -15) add_skill = -15;
		}

		/*
		 * Burglars (who can be of any realm or none) are unusually bad
		 * with heavy weapons and are the only characters to get a bonus
		 * with light ones.
		 */
		if (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_BURGLE)
			{
			/* Skilled and powerful burglars can handle heavier weapons */
			int level = ((p_ptr->power + 50) +  get_skill(S_BURGLARY, 50, 150)) / 2;

			/* Apply a bonus for relatively light weapons */
			if (weight < level)
			{
				add_skill += (level - weight) / 3;

				/* Only skilled burglars get big bonuses */
				if (add_skill > get_skill(S_BURGLARY, 5, 35))
					add_skill = get_skill(S_BURGLARY, 5, 35);
			}

			/* Otherwise, increase any penalty */
			else if (add_skill < 0)
			{
				long temp;

				/* Those who concentrate on Burglary get larger penalties */
				temp = add_skill * get_skill(S_BURGLARY, 130, 200) / 100;
				add_skill = (int)temp;
			}
		}
	}

	return (add_skill);
}


/*
 * Obtain the non-pval flags for the player as if he were an item.
 *
 * Use "modify" to actually make changes to the character.
 *
 * All temporary attributes that can be expressed in object bitflag form
 * should go here.
 *
 * (coding shapechanges)
 * The effects of shapechanges go in four places:  this function handles
 * most effects of shapechanges that cannot be expressed in pval form,
 * "player_flags_pval()" handles all changes that can be expressed in
 * pval form, "player_flags_cancel()" handles any flags that are cancelled
 * because of a shapechange, and various timed states must be hacked into
 * "process_world()".
 */
void player_flags(u32b *f1, u32b *f2, u32b *f3, bool shape, bool modify)
{
	int to_a = 0;
	int to_h = 0;
	int to_d = 0;
	int to_s = 0;
	int skill;

	int weapon_skill;

	/* Set to the racial flags */
	*f1 = rp_ptr->flags1;
	*f2 = rp_ptr->flags2;
	*f3 = rp_ptr->flags3;


	/* Various special attacks add brands */
	if (p_ptr->special_attack & (ATTACK_HOLY)) *f1 |= (TR1_SLAY_EVIL);
	if (p_ptr->acid_attack) *f1 |= (TR1_BRAND_ACID);
	if (p_ptr->elec_attack) *f1 |= (TR1_BRAND_ELEC);
	if (p_ptr->fire_attack) *f1 |= (TR1_BRAND_FIRE);
	if (p_ptr->cold_attack) *f1 |= (TR1_BRAND_COLD);
	if (p_ptr->pois_attack) *f1 |= (TR1_BRAND_POIS);


	/* Get non-magical combat skill */
	weapon_skill = sweapon(inventory[INVEN_WIELD].tval);

	/*
	 * Special immunities for high-level martial arts experts
	 * No longer requires character to be fighting with karate or wrestling -JM-
	 */

	/* Karate bonuses */
	skill = get_skill(S_KARATE, 0, 100);
	if (skill >= LEV_REQ_MARTIAL_FA) *f3 |= (TR3_FREE_ACT);
	if (skill >= LEV_REQ_MARTIAL_RESIST) *f2 |= (TR2_RES_CONFU);

	/* Wrestling bonuses */
	skill = get_skill(S_WRESTLING, 0, 100);
	if (skill >= LEV_REQ_MARTIAL_FA) *f3 |= (TR3_FREE_ACT);
	if (skill >= LEV_REQ_MARTIAL_RESIST) *f2 |= (TR2_RES_SOUND);


	/* Wizardly protection */
	if (p_ptr->wiz_prot)
	{
		*f1 |= (TR1_SUST_INT);
		*f2 |= (TR2_RES_DISEN);
	}

	/* Holy aura */
	if (p_ptr->holy)
	{
		*f3 |= (TR3_HOLD_LIFE);
	}

	/* Temporary resist fear from various things */
	if ((p_ptr->bold) || (p_ptr->hero) || (p_ptr->berserk > BERSERK_WEAKNESS_LENGTH))
	{
		*f2 |= (TR2_RES_FEAR);
	}

	/* Temporary see invisible */
	if (p_ptr->tim_invis)
	{
		*f3 |= (TR3_SEE_INVIS);
	}

	/* Temporary esp */
	if (p_ptr->tim_esp)
	{
		*f3 |= (TR3_TELEPATHY);
	}


	/* Optionally ignore the effects of shapechanges */
	if (!shape) return;


	/* Handle shapechanges */
	switch (p_ptr->schange)
	{
		case SHAPE_NORMAL:
		{
			break;
		}
		case SHAPE_GOAT:
		{
			(*f3) |= (TR3_SLOW_DIGEST);
			to_a += 5;
			break;
		}
		case SHAPE_BEAR:
		{
			to_a += 5;
			if (check_barehanded()) to_h += 10;

			break;
		}
		case SHAPE_MOUSE:
		{
			to_a -= 5;
			to_h -= 15;
			to_d -= 25;

			break;
		}
		case SHAPE_HOUND:
		{
			(*f3) |= (TR3_TELEPATHY);
			break;
		}
		case SHAPE_LION:
		{
			(*f2) |= (TR2_RES_FEAR);
			to_a += 5;
			if (check_barehanded()) to_h += 20;

			break;
		}
		case SHAPE_CHEETAH:
		{
			break;
		}
		case SHAPE_DRAGON:
		{
			to_a += 10;
			if (check_barehanded()) to_h += 10;
			break;
		}
		case SHAPE_ENT:
		{
			(*f2) |= (TR2_RES_COLD);
			(*f2) |= (TR2_RES_POIS);
			(*f2) |= (TR2_RES_FEAR);
			(*f3) |= (TR3_SEE_INVIS);
			(*f3) |= (TR3_FREE_ACT);

			if (check_barehanded()) to_h += 10;
			break;
		}
		case SHAPE_TROLL:
		{
			to_a += 10;
			if (check_barehanded()) to_h += 10;
			break;
		}
		case SHAPE_BAT:
		{
			(*f2) |= (TR2_RES_BLIND);
			(*f3) |= (TR3_FEATHER);
			to_h -= 15;
			to_d -= 15;
			break;
		}
		case SHAPE_LICH:
		{
			/*
			 * Lich abilities are temporary haste, resistance to cold, poison  -- they are handled elsewhere
			 * See shapechange(), oppose_cold, opppose_pois, set_fast
			 */
			(*f3) |= TR3_SEE_INVIS;
			break;
		}
		case SHAPE_VAMPIRE:
		{
			(*f2) |= (TR2_RES_DARK);
			(*f3) |= (TR3_HOLD_LIFE);
			break;
		}
		case SHAPE_WEREWOLF:
		{
			(*f2) |= (TR2_RES_FEAR);
			(*f3) |= (TR3_AGGRAVATE);
			if (check_barehanded()) to_h += 10;
			break;
		}
		case SHAPE_SERPENT:
		{
			if (get_skill(S_NATURE, 0, 100) > 70)
				(*f2) |= (TR2_RES_POIS);

			if (check_barehanded() && p_ptr->barehand == S_WRESTLING) to_h += 20;
			break;
		}
		case SHAPE_ANGEL:
		{
			(*f3) |= TR3_LITE;
			(*f2) |= TR2_RES_LITE;
			(*f3) |= TR3_FEATHER;
			break;
		}
		case SHAPE_VORTEX:
		{
			if (get_skill(S_WIZARDRY,0,100) >= 80)
				*(f2) |= TR2_IM_FIRE;
			else
				*(f2) |= TR2_RES_FIRE;
			*(f3) |= TR3_LITE;
			*(f1) |= TR1_BRAND_FIRE;

			to_s += -10;
			break;
		}
		case SHAPE_GOLEM:
		{
			to_a += 30;
			*(f3) |= TR3_NOMAGIC;
			*(f3) |= TR3_HOLD_LIFE;
			*(f2) |= TR2_RES_CONFU;
			break;
		}
		case SHAPE_EAGLE:
		{
			*(f3) |= TR3_FEATHER;
			to_h -= 10;
			to_s -= 10;
			break;
		}
	}

	/* Modify */
	if (modify)
	{
		p_ptr->to_a += to_a;
		p_ptr->dis_to_a += to_a;

		/* Only melee skill is affected */
		p_ptr->skill_thn += to_h * BTH_PLUS_ADJ;
		p_ptr->skill_thn2 += to_h * BTH_PLUS_ADJ;

		/* Affect missile skill separately */
		p_ptr->skill_thb += to_s * BTH_PLUS_ADJ;

		p_ptr->to_d += to_d;
		p_ptr->dis_to_d += to_d;
	}
}

/*
 * Handle racial and shapechange vulnerabilities
 */
void player_flags_vulnerable(u32b *f1, u32b *f2, u32b *f3, bool shape)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Ents are vulnerable to fire */
	if (p_ptr->prace == RACE_ENT) *f2 |= TR2_RES_FIRE;

	if (!shape) return;

	if (p_ptr->schange == SHAPE_LICH) *f2 |= TR2_RES_FIRE;
	if (p_ptr->schange == SHAPE_VAMPIRE) *f2 |= TR2_RES_LITE;
	if (p_ptr->schange == SHAPE_ENT) *f2 |= TR2_RES_FIRE;
}


/*
 * Handle racial and shapechange cancellations of various flags.
 *
 * Optionally, ignore shapechanges.
 */
void player_flags_cancel(u32b *f1, u32b *f2, u32b *f3, bool shape)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;


	/* Optionally ignore the effects of shapechanges */
	if (!shape) return;


	/* Shapechange, if any. */
	switch (p_ptr->schange)
	{
		case SHAPE_NORMAL:
		{
			break;
		}
		case SHAPE_MOUSE:
		{
			(*f3) |= (TR3_AGGRAVATE);
			break;
		}
		case SHAPE_ENT:
		{
			(*f2) |= (TR2_IM_FIRE);
			(*f3) |= (TR3_FEATHER);
			break;
		}
		case SHAPE_TROLL:
		{
			(*f3) |= (TR3_FEATHER);
			break;
		}
	}
}


/*
 * Calculate the innate bonus to extra shots and extra might.
 *
 * Each bonus to shots increases shots by 0.5 per turn.  Each bonus to
 * might increases missile weapon multiplier by 1.
 */
int missile_bonus(u32b flag_pval, int skill)
{
	/* Skilled characters are quicker with slings and bows */
	if (flag_pval == TR_PVAL_SHOTS)
	{
		int extra_shots = 0;

		/* The specialist magic-users do poorly */
		if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
		    (p_ptr->oath & (COVENANT_OF_FAITH)) ||
		    (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
		    (p_ptr->oath & (BLACK_MYSTERY)))
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				if (skill >= 75) extra_shots++;
			}

			else if (p_ptr->ammo_tval == TV_ARROW)
			{
				if (skill >= 85) extra_shots++;
			}
		}

		/* Pure warrior/archers are very good */
		else if (p_ptr->oath & (OATH_OF_IRON))
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				if (skill >= 40) extra_shots++;
				if (skill >= 90) extra_shots++;
				if (skill >= 100) extra_shots++;
			}
			else if (p_ptr->ammo_tval == TV_ARROW)
			{
				if (skill >= 50) extra_shots++;
				if (skill >= 80) extra_shots++;
				if (skill >= 100) extra_shots++;
			}
		}

		/* Everyone else gets the normal bonuses */
		else
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				if (skill >= 60) extra_shots++;
				if (skill >= 100) extra_shots++;
			}
			else if (p_ptr->ammo_tval == TV_ARROW)
			{
				if (skill >= 65) extra_shots++;
				if (skill >= 100) extra_shots++;
			}
		}

		return (extra_shots);
	}

	/* Skilled characters are more deadly with slings and crossbows */
	else if (flag_pval == TR_PVAL_MIGHT)
	{
		int extra_might = 0;

		/* The specialist magic-users do poorly */
		if ((p_ptr->oath & (OATH_OF_SORCERY)) ||
		    (p_ptr->oath & (COVENANT_OF_FAITH)) ||
		    (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) ||
		    (p_ptr->oath & (BLACK_MYSTERY)))
		{
			/* no bonuses at all */
		}

		/* Pure warrior/archers are very good */
		else if (p_ptr->oath & (OATH_OF_IRON))
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				if (skill >= 60) extra_might++;
			}
			else if (p_ptr->ammo_tval == TV_BOLT)
			{
				if (skill >= 60) extra_might++;
				if (skill >= 100) extra_might++;
			}
		}

		/* Everyone else gets the normal bonuses */
		else
		{
			if (p_ptr->ammo_tval == TV_SHOT)
			{
				if (skill >= 90) extra_might++;
			}

			if (p_ptr->ammo_tval == TV_BOLT)
			{
				if (skill >= 95) extra_might++;
			}
		}

		return (extra_might);
	}

	/* Oops */
	return (0);
}


/*
 * Calculate the character's modifiers to pval-dependant qualities.
 *
 * This function handles modifiers to blows, shots, and extra might.  It
 * does not handle racial modifiers to the utility skills (they are grouped
 * with other utility skill modifiers).
 *
 * This function should be used for all modifiers to stats that adjust the
 * maximum value (maximize mode).  At present, character races behave this
 * way, as do most other permanent conditions.
 *
 * Every change made in the function (shapechanges excepted) show up on
 * the character screen.
 *
 * If "shapechange" is FALSE, we ignore the effects of shapechanges.
 */
int player_flags_pval(u32b flag_pval, bool shape)
{
	/* Assume no change */
	int pval = 0;

	/* Optionally ignore the effects of shapechanges */
	if (shape)
	{
		/* Awareness is reduced in all forms */
		if (flag_pval == TR_PVAL_AWARE && p_ptr->schange) pval -= 2;

		/* Handle shapechanges */
		switch (p_ptr->schange)
		{
			case SHAPE_NORMAL:
			{
				break;
			}
			case SHAPE_GOAT:
			{
				if (flag_pval == TR_PVAL_INFRA)   pval += 1;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_BEAR:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 2;
				if (flag_pval == TR_PVAL_INT)     pval -= 1;
				if (flag_pval == TR_PVAL_CON)     pval += 2;
				if (flag_pval == TR_PVAL_CHR)     pval -= 3;
				if (flag_pval == TR_PVAL_INFRA)   pval += 1;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_MOUSE:
			{
				if (flag_pval == TR_PVAL_STR)     pval -= 2;
				if (flag_pval == TR_PVAL_INT)     pval -= 7;
				if (flag_pval == TR_PVAL_CON)     pval -= 1;
				if (flag_pval == TR_PVAL_CHR)     pval -= 5;

				if (flag_pval == TR_PVAL_INFRA)   pval += 2;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 15;
				if (flag_pval == TR_PVAL_STEALTH) pval += 10;
				if (flag_pval == TR_PVAL_AWARE)   pval += 5;
				break;
			}
			case SHAPE_HOUND:
			{
				if (flag_pval == TR_PVAL_INT)     pval -= 2;
				if (flag_pval == TR_PVAL_CON)     pval += 2;
				if (flag_pval == TR_PVAL_CHR)     pval -= 2;

				if (flag_pval == TR_PVAL_AWARE)   pval += 3;
				if (flag_pval == TR_PVAL_INFRA)   pval += 3;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_CHEETAH:
			{
				if (flag_pval == TR_PVAL_DEX)     pval += 2;
				if (flag_pval == TR_PVAL_INFRA)   pval += 2;
				if (flag_pval == TR_PVAL_SPEED)   pval += get_skill(S_NATURE, 1, 7);
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_LION:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 3;
				if (flag_pval == TR_PVAL_INT)     pval -= 1;
				if (flag_pval == TR_PVAL_WIS)     pval -= 1;
				if (flag_pval == TR_PVAL_CHR)     pval -= 4;
				if (flag_pval == TR_PVAL_SPEED)   pval += 1;
				if (flag_pval == TR_PVAL_INFRA)   pval += 2;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_DRAGON:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 3;
				if (flag_pval == TR_PVAL_CON)     pval += 2;
				if (flag_pval == TR_PVAL_INFRA)   pval += 3;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 20;
				break;
			}
			case SHAPE_ENT:
			{
				if (flag_pval == TR_PVAL_WIS)     pval += 1;
				if (flag_pval == TR_PVAL_DEX)     pval -= 5;
				if (flag_pval == TR_PVAL_STR)     pval += 4;
				if (flag_pval == TR_PVAL_CON)     pval += 4;
				if (flag_pval == TR_PVAL_TUNNEL)  pval += 8;

				break;
			}
			case SHAPE_TROLL:
			{
				if (flag_pval == TR_PVAL_INT)     pval -= 2;
				if (flag_pval == TR_PVAL_DEX)     pval -= 2;
				if (flag_pval == TR_PVAL_STR)     pval += 3;
				if (flag_pval == TR_PVAL_CON)     pval += 1;
				if (flag_pval == TR_PVAL_INFRA)   pval += 2;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 40;
				break;
			}
			case SHAPE_BAT:
			{
				if (flag_pval == TR_PVAL_STR)     pval -= 1;
				if (flag_pval == TR_PVAL_WIS)     pval -= 2;
				if (flag_pval == TR_PVAL_INT)     pval -= 2;
				if (flag_pval == TR_PVAL_CHR)     pval -= 2;
				if (flag_pval == TR_PVAL_INFRA)   pval += 6;
				if (flag_pval == TR_PVAL_SPEED)   pval += 5;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 15;
				break;
			}
			case SHAPE_LICH:
			{
				if (flag_pval == TR_PVAL_INVIS)   pval += 6;
				break;
			}
			case SHAPE_VAMPIRE:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 2;
				if (flag_pval == TR_PVAL_INT)     pval += 2;
				if (flag_pval == TR_PVAL_CHR)     pval += 2;
				if (flag_pval == TR_PVAL_SPEED)   pval += 3;
				break;
			}
			case SHAPE_WEREWOLF:
			{
				if (flag_pval == TR_PVAL_STR)     pval += get_skill(S_DOMINION, 1, 4);
				if (flag_pval == TR_PVAL_CON)     pval += get_skill(S_DOMINION, 1, 4);
				if (flag_pval == TR_PVAL_CHR)     pval -= 4;
				if (flag_pval == TR_PVAL_INT)     pval -= 2;
				if (flag_pval == TR_PVAL_WIS)     pval -= 2;
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 15;
				if (flag_pval == TR_PVAL_INFRA)   pval += 3;
				if (flag_pval == TR_PVAL_STEALTH) pval -= 3;  /* It's those constant howls */
				break;
			}
			case SHAPE_SERPENT:
			{
				if (flag_pval == TR_PVAL_STR)     pval += get_skill(S_NATURE, 1, 4);
				if (flag_pval == TR_PVAL_DEX)     pval += get_skill(S_NATURE, 1, 4);
				if (flag_pval == TR_PVAL_INT)     pval -= 2;
				if (flag_pval == TR_PVAL_WIS)     pval -= 2;
				if (flag_pval == TR_PVAL_STEALTH) pval += get_skill(S_NATURE, 1, 4);
				break;
			}
			case SHAPE_ANGEL:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 2;
				if (flag_pval == TR_PVAL_WIS)     pval += 2;
				if (flag_pval == TR_PVAL_CON)     pval += 2;
				if (flag_pval == TR_PVAL_CHR)     pval += 3;
				if (flag_pval == TR_PVAL_INVIS)   pval -= 2;
				break;
			}
			case SHAPE_VORTEX:
			{
				if (flag_pval == TR_PVAL_DEX)     pval += 2;
				break;
			}
			case SHAPE_GOLEM:
			{
				if (flag_pval == TR_PVAL_STR)     pval += 2;
				if (flag_pval == TR_PVAL_DEX)     pval -= 2;
				if (flag_pval == TR_PVAL_SAVE)    pval += get_skill(S_WIZARDRY, 0, 5);
				if (flag_pval == TR_PVAL_DEVICE)  pval -= p_ptr->skill_dev / 10;
				break;
			}
			case SHAPE_EAGLE:
			{
				if (flag_pval == TR_PVAL_STR)      pval -= 2;
				if (flag_pval == TR_PVAL_AWARE)    pval += 4;
				if (flag_pval == TR_PVAL_STEALTH)  pval += get_skill(S_SHAPECHANGE, 0, 6);
				if (flag_pval == TR_PVAL_SPEED)    pval += get_skill(S_SHAPECHANGE, 0, 6);
				if (flag_pval == TR_PVAL_DEVICE)   pval -= p_ptr->skill_dev / 15;
				break;
			}
		}

		/* Decrease penalties for high-level shapechangers */
		/* Should trigger at 3/4 of the way to 100 from where player got the skill */
		if (get_skill(p_ptr->schange_skill, 0, 100) >= (300 + p_ptr->schange_min_skill) / 4)
		{
			if (pval < 0) pval++;
		}

	}


	/* Handle racial modifiers to stats */
	if (flag_pval == TR_PVAL_STR) pval += rp_ptr->r_adj[A_STR];
	if (flag_pval == TR_PVAL_INT) pval += rp_ptr->r_adj[A_INT];
	if (flag_pval == TR_PVAL_WIS) pval += rp_ptr->r_adj[A_WIS];
	if (flag_pval == TR_PVAL_DEX) pval += rp_ptr->r_adj[A_DEX];
	if (flag_pval == TR_PVAL_CON) pval += rp_ptr->r_adj[A_CON];
	if (flag_pval == TR_PVAL_CHR) pval += rp_ptr->r_adj[A_CHR];


	/* Handle racial modifiers to infravision */
	if (flag_pval == TR_PVAL_INFRA) pval += rp_ptr->infra;

	/* Increases in perception increase infravision */
	if (flag_pval == TR_PVAL_INFRA)
	{
		int skill = get_skill(S_PERCEPTION, 0, 100);

		if(skill >= LEV_REQ_PERCEPTION_INFRA1) pval++;
		if(skill >= LEV_REQ_PERCEPTION_INFRA2) pval++;
		if(skill >= LEV_REQ_PERCEPTION_INFRA3) pval++;
		if(skill >= LEV_REQ_PERCEPTION_INFRA4) pval++;

	}



	/* Giants and Ents are great at tunneling */
	if (flag_pval == TR_PVAL_TUNNEL)
	{
		if (p_ptr->prace == RACE_GIANT ||
			p_ptr->prace == RACE_ENT) pval += 1 + p_ptr->power / 20;
	}


	/* Handle modifiers for taking Oaths */
	if (flag_pval == TR_PVAL_STR)
	{
		/* Bonus for taking the Oath of Iron */
		if (p_ptr->oath & (OATH_OF_IRON)) pval += 2 + (p_ptr->power / 60);
	}
	if (flag_pval == TR_PVAL_INT)
	{
		/* Bonus for knowing sorcerous or necromantic spells */
		if ((p_ptr->realm == MAGE) || (p_ptr->realm == NECRO)) pval += 2;

		/* Another bonus for Oath of Sorcery or Black Mystery */
		if      (p_ptr->oath & (OATH_OF_SORCERY)) pval += 2;
		else if (p_ptr->oath & (BLACK_MYSTERY))   pval += 2;
	}
	if (flag_pval == TR_PVAL_WIS)
	{
		/* Bonus for knowing holy prayers or druidic spells */
		if ((p_ptr->realm == PRIEST) || (p_ptr->realm == DRUID)) pval += 2;

		/* Another bonus for Covenant or Faith or Yavanna's Fellowship */
		if      (p_ptr->oath & (COVENANT_OF_FAITH))   pval += 2;
		else if (p_ptr->oath & (YAVANNAS_FELLOWSHIP)) pval += 2;

		/* Penalty for lack of sanctity */
		if (p_ptr->unsanctified) pval -= 6;
	}
	if (flag_pval == TR_PVAL_DEX)
	{
		/* Bonus for taking the Oath of Iron */
		if (p_ptr->oath & (OATH_OF_IRON)) pval += 1 + (p_ptr->power / 55);

		/* Bonus for joining the Burglars' Guild */
		if (p_ptr->oath & (BURGLARS_GUILD)) pval += 2;
	}
	if (flag_pval == TR_PVAL_CON)
	{
		/* Bonus for taking the Oath of Iron */
		if (p_ptr->oath & (OATH_OF_IRON)) pval += 1 + (p_ptr->power / 65);
	}
	if (flag_pval == TR_PVAL_CHR)
	{
		/* Penalty for learning how to steal */
		if (get_skill(S_BURGLARY, 0, 100) >= LEV_REQ_BURGLE) pval -= 2;

		/* Penalty for joining the Burglars' Guild */
		if (p_ptr->oath & (BURGLARS_GUILD)) pval -= 2;
	}


	/* Handle martial arts stat bonuses */
	if (flag_pval == TR_PVAL_STR)
	{
		int skill = get_skill(S_WRESTLING, 0, 100);
		if (skill >= LEV_REQ_MARTIAL_STAT1) pval++;
		if (skill >= LEV_REQ_MARTIAL_STAT2) pval++;
		if (skill >= LEV_REQ_MARTIAL_STAT3) pval++;
	}

	if (flag_pval == TR_PVAL_DEX)
	{
		int skill = get_skill(S_KARATE, 0, 100);
		if (skill >= LEV_REQ_MARTIAL_STAT1) pval++;
		if (skill >= LEV_REQ_MARTIAL_STAT2) pval++;
		if (skill >= LEV_REQ_MARTIAL_STAT3) pval++;
	}

	if (flag_pval == TR_PVAL_SPEED)
	{
		/* Karate gives some speed */
		int skill = get_skill(S_KARATE, 0, 100);
		if(skill >= LEV_REQ_KARATE_SPEED1) pval++;
		if(skill >= LEV_REQ_KARATE_SPEED2) pval++;
	}

	/* Handle wrestling bonus to digging */
	if (flag_pval == TR_PVAL_TUNNEL) pval += get_skill(S_WRESTLING, 0, 3);

	/* Handle karate bonus to stealth */
	if (flag_pval == TR_PVAL_STEALTH) pval += get_skill(S_KARATE, 0, 1);


	/* Handle modifiers to blows */
	if (flag_pval == TR_PVAL_BLOWS)
	{
		/* None yet */
	}

	/* Handle modifiers to shots and extra might */
	if ((flag_pval == TR_PVAL_SHOTS) || (flag_pval == TR_PVAL_MIGHT))
	{
		int skill = get_skill(sbow(inventory[INVEN_BOW].tval), 0, 100);
		pval += missile_bonus(flag_pval, skill);
	}






	/* Return */
	return (pval);
}

/*
 * Consolidate weapon blow calculations
 */
int weapon_blows(object_type *o_ptr, bool primary)
{
	int str_index, dex_index, blows, str, dex;
	object_type *i_ptr;
	int weight, effective_weight, hold;

	str = p_ptr->stat_ind[A_STR];
	dex = p_ptr->stat_ind[A_DEX];

	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];

	i_ptr = &inventory[INVEN_WIELD];

	if (primary) weight = o_ptr->weight / 10;
	else weight = (o_ptr->weight + i_ptr->weight) / 5;  /* Half the single-weapon weight */

	/* Handle heavy weapons */
	if (weight > hold) return (primary) ? 1 : 0;

	/* Choose which item to replace in calculation */
	if (primary) 	i_ptr = &inventory[INVEN_WIELD];
	else 			i_ptr = &inventory[INVEN_ARM];

	/* Remove benefits from replaced weapon or shield */
	if (i_ptr)
	{

		str -= get_object_pval(i_ptr, TR_PVAL_STR);
		dex -= get_object_pval(i_ptr, TR_PVAL_DEX);
	}

	/* Add benefits of new weapon */
	str += get_object_pval(o_ptr, TR_PVAL_STR);
	dex += get_object_pval(o_ptr, TR_PVAL_DEX);

	/* Keep within limits */
	str = MIN(MAX(0, str), 37);
	dex = MIN(MAX(0, dex), 37);

	/* Enforce a minimum weight of three pounds */
	effective_weight = (o_ptr->weight < 30 ? 30 : o_ptr->weight);

	/* Compare strength and weapon weight */
	str_index = MIN(11, 6 * adj_str_blow[str] / effective_weight);

	/* Index by dexterity */
	dex_index = MIN(11, (adj_dex_blow[dex]));

	/* Use the blows table */
	blows = blows_table[str_index][dex_index];

	/* Add extra blows */
	blows += get_object_pval(o_ptr, TR_PVAL_BLOWS);

	/* Apply character modifiers to blows */
	blows += player_flags_pval(TR_PVAL_BLOWS, TRUE);

	return blows;
}


/*
 * Analyze wielded melee and missile weapons, check various combat-
 * related things.
 */
static void analyze_weapons(void)
{
	object_type *o_ptr, *i_ptr;

	u32b f1, f2, f3;

	int ammo_multiplier = 0;

	/* Get the maximum weapon weight that can be wielded effectively */
	int hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/*** Analyze missile launcher ***/

	/* Examine the missile launcher */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;


	/* Analyze launcher  XXX XXX */
	if (o_ptr->k_idx)
	{
		/* Choose ammo based on launcher type  XXX */
		if (o_ptr->tval == TV_SLING)    p_ptr->ammo_tval = TV_SHOT;
		if (o_ptr->tval == TV_BOW)      p_ptr->ammo_tval = TV_ARROW;
		if (o_ptr->tval == TV_CROSSBOW) p_ptr->ammo_tval = TV_BOLT;

		/* Get the missile weapon's base ammo multiplier */
		ammo_multiplier += o_ptr->dd;

		/* Get the missile weapon's bonus to ammo multiplier */
		ammo_multiplier += get_object_pval(o_ptr, TR_PVAL_MIGHT);

		/* Get the character's bonus to ammo multiplier */
		ammo_multiplier += player_flags_pval(TR_PVAL_MIGHT, TRUE);

		/* Store ammo multiplier */
		p_ptr->ammo_mult += ammo_multiplier;


		/* It is hard to carry a heavy bow */
		if (hold < o_ptr->weight / 10)
		{
			/* Hard to wield a heavy bow */
			p_ptr->skill_thb += 5 * (hold - o_ptr->weight / 10);

			/* Heavy Bow */
			p_ptr->heavy_shoot = TRUE;
		}

		/*
		 * Shoot one time every turn.
		 *
		 * Note:  Energy taken by each shot is 200 / p_ptr->num_fire.
		 */
		p_ptr->num_fire = 2;

		/* Triple crossbows shoot incredibly quickly */
		if (o_ptr->sval == SV_TRIPLE_XBOW) p_ptr->num_fire = 6;

		/* Allow extra shots if the missile launcher isn't too heavy */
		if (!p_ptr->heavy_shoot)
		{
			/* Extra shots - innate */
			p_ptr->num_fire += player_flags_pval(TR_PVAL_SHOTS, TRUE);

			/* Extra shots - from missile launcher */
			p_ptr->num_fire += get_object_pval(o_ptr, TR_PVAL_SHOTS);
		}

		/* Require at least one shot per two rounds */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}


	/*** Analyze melee weapon(s) ***/

	/* Get the primary melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Examine the shield or secondary melee weapon */
	i_ptr = &inventory[INVEN_ARM];

	/* Are we holding two weapons? */
	if (is_melee_weapon(o_ptr) && is_melee_weapon(i_ptr))
	{
		/* Note that we are using two weapons */
		p_ptr->twoweap = TRUE;

		/* Automatic penalty to melee skill */
		p_ptr->skill_thn -= 10 + (o_ptr->weight + i_ptr->weight) / 10;
		p_ptr->skill_thn2 -= 10 + (o_ptr->weight + i_ptr->weight) / 10;
	}

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* Test for over-heavy weapon */
	if (o_ptr->k_idx)
	{
		/* Weigh the weapon(s) */
		int weapon_weight = o_ptr->weight + (p_ptr->twoweap ? i_ptr->weight : 0);

		/* It is hard to hold a heavy weapon */
		if (hold < weapon_weight / 10)
		{
			p_ptr->skill_thn += 5 * (hold - weapon_weight / 10);
			p_ptr->skill_thn2 += 5 * (hold - weapon_weight / 10);

			/* Note that weapon is heavy */
			p_ptr->heavy_wield = TRUE;

			/* The player gets to swing a heavy weapon only once. */
			p_ptr->num_blow = 1;

			/* Calculate digging bonus */
			p_ptr->skill_dig += (get_object_pval(o_ptr, TR_PVAL_TUNNEL) * 20) + (o_ptr->weight / 25);
		}

		/* Weapon is not too heavy */
		else
		{
			int skill_dig1 = 0, skill_dig2 = 0;

			/* First weapon */
			if (TRUE)
			{
				/* Calculate number of blows */
				p_ptr->num_blow = weapon_blows(o_ptr, TRUE);

				/* Calculate digging bonus */
				skill_dig1 += (get_object_pval(o_ptr, TR_PVAL_TUNNEL) * 20) +
					(o_ptr->weight / 25);
			}


			/* Second weapon */
			if (p_ptr->twoweap)
			{
				p_ptr->num_blow2 = weapon_blows(i_ptr, FALSE);

				/* Calculate digging bonus */
				skill_dig2 += (get_object_pval(i_ptr, TR_PVAL_TUNNEL) * 20) +
					(i_ptr->weight / 25);
			}

			/* Apply the better of the two digging bonuses */
			p_ptr->skill_dig += MAX(skill_dig1, skill_dig2);
		}

		/* Note that we are not bare-handed */
		p_ptr->barehanded = FALSE;
	}

	/* Using bare-handed combat */
	else
	{
		int skill;
		p_ptr->num_blow = 2;

		/* Martial arts gains blows at fixed skill levels -JM */

		if (p_ptr->barehand == S_KARATE)
		{
			skill = get_skill(S_KARATE, 0, 100);
			if (skill >= LEV_REQ_KARATE_BLOW1) p_ptr->num_blow++;
			if (skill >= LEV_REQ_KARATE_BLOW2) p_ptr->num_blow++;
			if (skill >= LEV_REQ_KARATE_BLOW3) p_ptr->num_blow++;
			if (skill >= LEV_REQ_KARATE_BLOW4) p_ptr->num_blow++;
		}
		else if (p_ptr->barehand == S_WRESTLING)
		{
			skill = get_skill(S_WRESTLING, 0, 100);
			if (skill >= LEV_REQ_WREST_BLOW1) p_ptr->num_blow++;
			if (skill >= LEV_REQ_WREST_BLOW2) p_ptr->num_blow++;
		}



		/* Note that we are bare-handed */
		p_ptr->barehanded = TRUE;
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;

	/* Priest weapon penalty for non-blessed edged weapons */
	if ((p_ptr->realm == PRIEST) && (!p_ptr->bless_blade) &&
	    ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		/* Reduce the melee combat ability (not anything else) */
		p_ptr->skill_thn /= 2;
		p_ptr->skill_thn2 /= 2;

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;

		/* Less light */
		p_ptr->drain_light = TRUE;
	}

	/* Defensive weapon yields better AC if skilled  -clefs- */
	if ((o_ptr->k_idx) && (o_ptr->ac) && (!p_ptr->twoweap) &&
	    (!p_ptr->icky_wield) && (!p_ptr->heavy_wield))
	{
		/* Bonus rises exponentially */
		int skill = get_skill(sweapon(o_ptr->tval), 0, 40);

		/* Total AC bonus can be 5x (4x, plus original) of normal */
		int bonus = o_ptr->ac * get_skill(sweapon(o_ptr->tval), 0, skill) / 10;

		/* Must be a bonus, not a penalty */
		if (bonus > 0)
		{
			p_ptr->ac += bonus;
			p_ptr->dis_ac += bonus;
		}
	}


	/*** Update the shield on back variable ***/

	/* Assume that we don't have to carry our shield on our back */
	p_ptr->shield_on_back = FALSE;

	/* Get primary weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Wielding a melee weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Primary melee weapon needs two hands */
		if (needs_two_hands(f1, o_ptr->weight))
		{
			p_ptr->shield_on_back = TRUE;
		}
	}

	/* Get secondary weapon (this shouldn't be necessary, but...) */
	o_ptr = &inventory[INVEN_ARM];

	/* Wielding a melee weapon */
	if (is_melee_weapon(o_ptr))
	{
		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Secondary melee weapon needs two hands */
		if (needs_two_hands(f1, o_ptr->weight))
		{
			p_ptr->shield_on_back = TRUE;
		}
	}

	/* A shield worn on the back has no effective base AC */
	if (p_ptr->shield_on_back)
	{
		o_ptr = &inventory[INVEN_ARM];

		/* Player is using a shield */
		if ((o_ptr->k_idx) && (o_ptr->tval == TV_SHIELD))
		{
			/* Hack -- undo base ac */
			p_ptr->ac     -= o_ptr->ac;
			p_ptr->dis_ac -= o_ptr->ac;
		}
	}
}


/*
 * Calculate the character's abilities, bonuses, and attributes.
 *
 * 1.  Calculate basic abilities.
 * 2.  Apply intrinsics (including those from race and shapechanges).
 * 3.  Apply adjustments to and calculate vital statistics, determine
 *     their effects.
 * 4.  Apply the non-stat effects of temporary conditions.
 * 5.  Determine combat speed, note over-heavy weapons.
 * 6.  Notice changes, output messages (unless suppressed).
 *
 * See also calc_mana() and calc_hitpoints().
 */
static void calc_bonuses(void)
{
	int i;

	int old_speed;

	int old_telepathy;
	int old_see_inv;
	int old_invisibility;

	int old_dis_ac;
	int old_dis_to_a;

	int weapon_skill, bow_skill;

	int old_mana_bonus;

	bool old_twoweap;
	int old_stealth;

	int hit_bonus = 0;

	int old_stat_use[A_MAX];
	int old_stat_ind[A_MAX];

	bool old_heavy_shoot;
	bool old_heavy_wield;
	bool old_icky_wield;
	bool old_shield_on_back;
	bool old_being_crushed;
	bool old_cumber_glove;

	object_type *o_ptr;
	u32b f1, f2, f3;

	int tmp;

	/*** Memorize ***/

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old stealth */
	old_stealth = p_ptr->skill_stl;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;
	old_invisibility = p_ptr->invisible;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old two-weapon status */
	old_twoweap = p_ptr->twoweap;


	/* Save the old stats and their indexes */
	for (i = 0; i < A_MAX; i++)
	{
		old_stat_use[i] = p_ptr->stat_use[i];
		old_stat_ind[i] = p_ptr->stat_ind[i];
	}

	/* Save the old weapon penalties */
	old_heavy_shoot = p_ptr->heavy_shoot;
	old_heavy_wield = p_ptr->heavy_wield;
	old_icky_wield = p_ptr->icky_wield;
	old_shield_on_back = p_ptr->shield_on_back;
	old_being_crushed = p_ptr->being_crushed;

	/* Save the old mana bonus */
	old_mana_bonus = p_ptr->mana_bonus;

	/* Save the old gloves encumbrance */
	old_cumber_glove = p_ptr->cumber_glove;


	/*** Reset ***/

	/* Reset player speed */
	p_ptr->pspeed = 110;
	p_ptr->pspeed += player_flags_pval(TR_PVAL_SPEED, TRUE);


	/* Reset "blow" info */
	p_ptr->num_blow = 1;
	p_ptr->num_blow2 = 0;

	/* Reset "fire" info */
	p_ptr->num_fire = 0;
	p_ptr->ammo_mult = 0;
	p_ptr->ammo_tval = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;


	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->drain_exp = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->glowing = FALSE;
	p_ptr->invisible = 0;
	p_ptr->soulsteal = FALSE;
	p_ptr->nomagic = FALSE;
	p_ptr->drain_light = FALSE;
	p_ptr->twoweap = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;
	p_ptr->resist_acid = FALSE;
	p_ptr->resist_elec = FALSE;
	p_ptr->resist_fire = FALSE;
	p_ptr->resist_cold = FALSE;
	p_ptr->resist_pois = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_confu = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_nethr = FALSE;
	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;



	/*** Calculate various abilities ***/

	/* Stealth (usually between 0 and 10, can be much more) */
	p_ptr->skill_stl  = 1 + rp_ptr->r_stl;

	if (p_ptr->sneaking) p_ptr->skill_stl += get_skill_race(S_STEALTH, 2, 12);
	else                 p_ptr->skill_stl += get_skill_race(S_STEALTH, 0,  4);
	p_ptr->skill_stl += player_flags_pval(TR_PVAL_STEALTH, TRUE);

	/* Extra bonuses early on if character focuses enough on stealth */
	if (get_skill(S_STEALTH, 0, 100) > p_ptr->max_depth)
		p_ptr->skill_stl += 1;
	if (get_skill(S_STEALTH, 0,  85) > p_ptr->max_depth)
		p_ptr->skill_stl += 1;
	if (get_skill(S_STEALTH, 0,  70) > p_ptr->max_depth)
		p_ptr->skill_stl += 1;


	/* Perception ability (ranges from ~0 to ~125) */
	p_ptr->skill_srh  = 5 + rp_ptr->r_srh;

	/* Searching ability goes up with perception skill (can reach 100) */
	p_ptr->skill_srh +=
		get_skill_race(S_PERCEPTION, 0, get_skill(S_PERCEPTION, 30, 100));

	/* Awareness enhances searching too */
	p_ptr->skill_srh += player_flags_pval(TR_PVAL_AWARE, TRUE) * 5;

	/* Awareness ability (usually zero, can be enhanced) */
	p_ptr->skill_awr = player_flags_pval(TR_PVAL_AWARE, TRUE) * 5;


	/* Digging (ranges from 0 to >100) */
	p_ptr->skill_dig  = 0;
	p_ptr->skill_dig += player_flags_pval(TR_PVAL_TUNNEL, TRUE) * 20;


	/* Invisibility */
	p_ptr->invisible = 0;
	p_ptr->invisible += player_flags_pval(TR_PVAL_INVIS, TRUE) * 5;


	/* Disarming (ranges from ~5 to ~180) */
	p_ptr->skill_dis  = rp_ptr->r_dis;
	p_ptr->skill_dis += get_skill_race(S_DISARM, 10, 160);
	p_ptr->skill_dis += player_flags_pval(TR_PVAL_DISARM, TRUE) * 10;


	/* Magic devices (ranges from -4 to >125) */
	p_ptr->skill_dev  = rp_ptr->r_dev;

	if (get_skill(S_DEVICE, 0, 100))
		p_ptr->skill_dev += get_skill_race(S_DEVICE, 3, 103);

	p_ptr->skill_dev += player_flags_pval(TR_PVAL_DEVICE, TRUE) * 5;


	/* Saving throw (ranges from ~7 to ~100, plus bonuses) */
	p_ptr->skill_sav  = rp_ptr->r_sav;       /* -4 to 10 */
	p_ptr->skill_sav += p_ptr->power / 4;    /* 0 to 25 guarantee a minimum saving throw */
	p_ptr->skill_sav += get_skill(S_SAVE, 10, 55); /* 10 to 55 from skill (see below for more from skill)*/
	p_ptr->skill_sav += player_flags_pval(TR_PVAL_SAVE, TRUE) * 5;
											 /* -9 to 19 more from wisdom added later using adj_wis_sav[] */

	/* A saving throw skill above 80 is doubly effective */
	if (get_skill(S_SAVE, 0, 100) > 80)
		p_ptr->skill_sav += get_skill(S_SAVE, -80, 20);

	/* Bad luck reduces saving throw somewhat */
	p_ptr->skill_sav += (p_ptr->luck - 100) / 4;

	/* Mana bonus */
	p_ptr->mana_bonus = 0;
	p_ptr->mana_bonus += player_flags_pval(TR_PVAL_MANA, TRUE) * 20;

	/* Infravision */
	p_ptr->see_infra = 0;
	p_ptr->see_infra += player_flags_pval(TR_PVAL_INFRA, TRUE);


	/* Determine which weapon and bow skills are currently applicable */
	bow_skill = sbow(inventory[INVEN_BOW].tval);

	/* Melee Combat (ranges from ~12 to ~140 (~185 with martial arts)) */
	p_ptr->skill_thn  = 10 + rp_ptr->r_thn;
	p_ptr->skill_thn2  = 10 + rp_ptr->r_thn;

	tmp = add_special_melee_skill();
	p_ptr->skill_thn += tmp;
	p_ptr->skill_thn2 += tmp;

	/* Main weapon */
	o_ptr = &inventory[INVEN_WIELD];
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_BLESSED) weapon_skill = best_melee_skill();  	/* Hack -- blessed weapons are used as if the best weapon type */
	else weapon_skill = sweapon(o_ptr->tval);
	if (get_skill(weapon_skill, 0, 100)) p_ptr->skill_thn += get_skill_race(weapon_skill, 5, 115);

	/* Offhand weapon */
	o_ptr = &inventory[INVEN_ARM];
	object_flags(o_ptr, &f1, &f2, &f3);
	if (f3 & TR3_BLESSED) weapon_skill = best_melee_skill();  	/* Hack -- blessed weapons are used as if the best weapon type */
	else weapon_skill = sweapon(o_ptr->tval);
	if (get_skill(weapon_skill, 0, 100)) p_ptr->skill_thn2 += get_skill_race(weapon_skill, 5, 115);


	/* Hack -- special bonus for high-level martial arts experts */
	if ((weapon_skill == S_KARATE) || (weapon_skill == S_WRESTLING))
	{
		int ma_bonus = get_skill(weapon_skill, -45, 15);
		if (ma_bonus > 0)
		{
			/* Bonus is quadratic -- goes up to 45 */
			p_ptr->skill_thn += ma_bonus * (15 + ma_bonus) / 10;
		}
	}

	/* Ranged Combat (ranges from ~11 (with skill of 1) to ~140) */
	p_ptr->skill_thb = 10 + rp_ptr->r_thb;

	if (get_skill(bow_skill, 0, 100))
		p_ptr->skill_thb += get_skill_race(bow_skill, 5, 115);


	/* Throwing (ranges from ~15 (with skill of 1) to ~140) */
	p_ptr->skill_tht = 10 + rp_ptr->r_tht;

	if (get_skill(S_THROWING, 0, 100))
		p_ptr->skill_tht += get_skill_race(S_THROWING, 5, 115);



	/*** Analyze the character ***/

	/* Extract the player flags, apply any special modifiers */
	player_flags(&f1, &f2, &f3, TRUE, TRUE);

	/* Sustain flags */
	if (f1 & (TR1_SUST_STR))       p_ptr->sustain_str   = TRUE;
	if (f1 & (TR1_SUST_INT))       p_ptr->sustain_int   = TRUE;
	if (f1 & (TR1_SUST_WIS))       p_ptr->sustain_wis   = TRUE;
	if (f1 & (TR1_SUST_DEX))       p_ptr->sustain_dex   = TRUE;
	if (f1 & (TR1_SUST_CON))       p_ptr->sustain_con   = TRUE;
	if (f1 & (TR1_SUST_CHR))       p_ptr->sustain_chr   = TRUE;

	/* Immunity flags */
	if (f2 & (TR2_IM_ACID))        p_ptr->immune_acid   = TRUE;
	if (f2 & (TR2_IM_ELEC))        p_ptr->immune_elec   = TRUE;
	if (f2 & (TR2_IM_FIRE))        p_ptr->immune_fire   = TRUE;
	if (f2 & (TR2_IM_COLD))        p_ptr->immune_cold   = TRUE;

	/* Resistance flags */
	if (f2 & (TR2_RES_ACID))       p_ptr->resist_acid   = TRUE;
	if (f2 & (TR2_RES_ELEC))       p_ptr->resist_elec   = TRUE;
	if (f2 & (TR2_RES_FIRE))       p_ptr->resist_fire   = TRUE;
	if (f2 & (TR2_RES_COLD))       p_ptr->resist_cold   = TRUE;
	if (f2 & (TR2_RES_POIS))       p_ptr->resist_pois   = TRUE;
	if (f2 & (TR2_RES_LITE))       p_ptr->resist_lite   = TRUE;
	if (f2 & (TR2_RES_DARK))       p_ptr->resist_dark   = TRUE;
	if (f2 & (TR2_RES_FEAR))       p_ptr->resist_fear   = TRUE;
	if (f2 & (TR2_RES_BLIND))      p_ptr->resist_blind  = TRUE;
	if (f2 & (TR2_RES_CONFU))      p_ptr->resist_confu  = TRUE;
	if (f2 & (TR2_RES_SOUND))      p_ptr->resist_sound  = TRUE;
	if (f2 & (TR2_RES_SHARD))      p_ptr->resist_shard  = TRUE;
	if (f2 & (TR2_RES_NEXUS))      p_ptr->resist_nexus  = TRUE;
	if (f2 & (TR2_RES_NETHR))      p_ptr->resist_nethr  = TRUE;
	if (f2 & (TR2_RES_CHAOS))      p_ptr->resist_chaos  = TRUE;
	if (f2 & (TR2_RES_DISEN))      p_ptr->resist_disen  = TRUE;

	/* Miscellaneous flags */
	if (f3 & (TR3_SLOW_DIGEST))    p_ptr->slow_digest   = TRUE;
	if (f3 & (TR3_FEATHER))        p_ptr->ffall         = TRUE;
	if (f3 & (TR3_LITE))           p_ptr->glowing       = TRUE;
	if (f3 & (TR3_REGEN))          p_ptr->regenerate    = TRUE;
	if (f3 & (TR3_TELEPATHY))      p_ptr->telepathy     = TRUE;
	if (f3 & (TR3_SEE_INVIS))      p_ptr->see_inv       = TRUE;
	if (f3 & (TR3_FREE_ACT))       p_ptr->free_act      = TRUE;
	if (f3 & (TR3_HOLD_LIFE))      p_ptr->hold_life     = TRUE;
	if (f3 & (TR3_BLESSED))        p_ptr->bless_blade   = TRUE;

	/* Bad flags */
	if (f3 & (TR3_NOMAGIC))        p_ptr->nomagic       = TRUE;
	if (f3 & (TR3_TELEPORT))       p_ptr->teleport      = TRUE;
	if (f3 & (TR3_AGGRAVATE))      p_ptr->aggravate     = TRUE;
	if (f3 & (TR3_DRAIN_EXP))      p_ptr->drain_exp     = TRUE;
	if (f3 & (TR3_DRAIN_HP))       p_ptr->being_crushed = TRUE;



	/*** Adjust stats ***/

	/* Scan the equipment (ignore quiver) */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Affect stats */
		p_ptr->stat_add[A_STR] += get_object_pval(o_ptr, TR_PVAL_STR);
		p_ptr->stat_add[A_INT] += get_object_pval(o_ptr, TR_PVAL_INT);
		p_ptr->stat_add[A_WIS] += get_object_pval(o_ptr, TR_PVAL_WIS);
		p_ptr->stat_add[A_DEX] += get_object_pval(o_ptr, TR_PVAL_DEX);
		p_ptr->stat_add[A_CON] += get_object_pval(o_ptr, TR_PVAL_CON);
		p_ptr->stat_add[A_CHR] += get_object_pval(o_ptr, TR_PVAL_CHR);
	}

	/* Temporary Berserk */
	if (p_ptr->berserk)
	{
		/* In the "rage" portion of the berserker fit */
		if (p_ptr->berserk > BERSERK_WEAKNESS_LENGTH)
		{
			/* Extreme strength */
			p_ptr->stat_add[A_STR] += 6;

			/* Some extra dexterity */
			p_ptr->stat_add[A_STR] += 2;

			/* Poor wisdom, intelligence and charisma */
			p_ptr->stat_add[A_WIS] -= 4;
			p_ptr->stat_add[A_INT] -= 4;
			p_ptr->stat_add[A_CHR] -= 4;
		}

		/* In the "crash" portion of the berserker fit */
		else
		{
			/* Wisdom, intelligence and charisma slowly recover */
			int tmp = MIN(4, 5 * p_ptr->berserk / BERSERK_WEAKNESS_LENGTH);

			p_ptr->stat_add[A_WIS] -= tmp;
			p_ptr->stat_add[A_INT] -= tmp;
			p_ptr->stat_add[A_CHR] -= tmp;

			/* Extreme weakness, slowly recovering */
			p_ptr->stat_add[A_STR] -= tmp;
		}
	}

	/* Temporary Necromantic rage */
	if (p_ptr->necro_rage)
	{
		/* In the "power" portion of the necromancer rage */
		if (p_ptr->necro_rage > NECRO_WEAKNESS_LENGTH)
		{
			/* Great strength */
			p_ptr->stat_add[A_STR] += 3;
		}

		/* In the "crash" portion of the necromantic rage */
		else
		{
			int tmp = MIN(4, 4 * p_ptr->necro_rage / NECRO_WEAKNESS_LENGTH);

			/* Extreme weakness, slowly recovering */
			p_ptr->stat_add[A_STR] -= tmp;
		}
	}


	/*** Handle stats ***/

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
	{
		int use, ind;
		u32b flag_pval = 0L;

		/* Translate stats into flags */
		if (i == A_STR) flag_pval = TR_PVAL_STR;
		if (i == A_INT) flag_pval = TR_PVAL_INT;
		if (i == A_WIS) flag_pval = TR_PVAL_WIS;
		if (i == A_DEX) flag_pval = TR_PVAL_DEX;
		if (i == A_CON) flag_pval = TR_PVAL_CON;
		if (i == A_CHR) flag_pval = TR_PVAL_CHR;

		/* Adjust modifier according to race, intrinsics, etc. */
		p_ptr->stat_add[i] += player_flags_pval(flag_pval, TRUE);

		/* Calculate the current stat value (intrinsic + adjustments) */
		p_ptr->stat_use[i] = use =
			modify_stat(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		/* Calculate the stat index value for: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Index value for: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Index value for: 18/220+ */
		else ind = (37);

		/* Save the new index */
		p_ptr->stat_ind[i] = ind;
	}


	/*** Analyze weight ***/

	/* Get burden percentage */
	i = p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]];

	/* An over-heavy pack slows you down */
	if (i >= 60)
	{
		/* -1 spd per 10% of weight above 50%, maximum of -30 */
		p_ptr->pspeed -= MIN(30, ((i - 50) / 10));
	}

	/* Bloating slows the player down */
	if (p_ptr->food >= p_ptr->food_bloated) p_ptr->pspeed -= 10;

	/* Sneaking slows the player down a little */
	if (p_ptr->sneaking) p_ptr->pspeed -= 5;

	/* Sanity check on extreme speeds */
	if (p_ptr->pspeed < 0) p_ptr->pspeed = 0;
	if (p_ptr->pspeed > 199) p_ptr->pspeed = 199;


	/*** Apply modifier bonuses ***/

	/* Apply stat modifiers to Skill, Deadliness, and armor class */
	hit_bonus       += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

	/* Deadliness for Karate is based on dexterity */
	if (p_ptr->barehand == S_KARATE && p_ptr->barehanded)
	{
		p_ptr->to_d     += ((int)(adj_str_td[p_ptr->stat_ind[A_DEX]]) - 128);
		p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_DEX]]) - 128);
	}
	else
	{
		p_ptr->to_d     += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
		p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	}

	p_ptr->to_a     += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);

	/* Apply stat modifiers to skills */
	p_ptr->skill_dis += ((int)(adj_dis[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->skill_dis += ((int)(adj_dis[p_ptr->stat_ind[A_INT]]) - 128);
	p_ptr->skill_dev += ((int)(adj_int_dev[p_ptr->stat_ind[A_INT]]) - 128);
	p_ptr->skill_sav += ((int)(adj_wis_sav[p_ptr->stat_ind[A_WIS]]) - 128);
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];




	/*** Analyze equipment ***/


	/* Scan the equipment (ignore quiver) */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stealth */
		p_ptr->skill_stl += get_object_pval(o_ptr, TR_PVAL_STEALTH);

		/* Affect perception and awareness (factor of five) */
		p_ptr->skill_srh += get_object_pval(o_ptr, TR_PVAL_AWARE) * 5;
		p_ptr->skill_awr += get_object_pval(o_ptr, TR_PVAL_AWARE) * 5;

		/* Affect infravision */
		p_ptr->see_infra += get_object_pval(o_ptr, TR_PVAL_INFRA);

		/* Affect digging (factor of 20) -- melee weapons are handled later */
		if (!is_melee_weapon(o_ptr))
		{
			p_ptr->skill_dig += get_object_pval(o_ptr, TR_PVAL_TUNNEL) * 20;
		}

		/* Affect speed */
		p_ptr->pspeed += get_object_pval(o_ptr, TR_PVAL_SPEED);

		/* Affect invisibility (factor of 5) */
		p_ptr->invisible += get_object_pval(o_ptr, TR_PVAL_INVIS) * 5;

		/* Affect disarming skill (factor of 10) */
		p_ptr->skill_dis += get_object_pval(o_ptr, TR_PVAL_DISARM) * 10;

		/* Affect magical device skill (factor of 5) */
		p_ptr->skill_dev += get_object_pval(o_ptr, TR_PVAL_DEVICE) * 5;

		/* Affect saving throw (factor of 5) */
		p_ptr->skill_sav += get_object_pval(o_ptr, TR_PVAL_SAVE) * 5;

		/* Affect mana (factor of 15) */
		p_ptr->mana_bonus += get_object_pval(o_ptr, TR_PVAL_MANA) * 15;


		/*
		 * Note:  Only weapons can affect blows, shots, and extra might.
		 *
		 * Do not change this unless you know *exactly* what problems
		 * you are creating.
		 */


		/* Sustain flags */
		if (f1 & (TR1_SUST_STR))    p_ptr->sustain_str   = TRUE;
		if (f1 & (TR1_SUST_INT))    p_ptr->sustain_int   = TRUE;
		if (f1 & (TR1_SUST_WIS))    p_ptr->sustain_wis   = TRUE;
		if (f1 & (TR1_SUST_DEX))    p_ptr->sustain_dex   = TRUE;
		if (f1 & (TR1_SUST_CON))    p_ptr->sustain_con   = TRUE;
		if (f1 & (TR1_SUST_CHR))    p_ptr->sustain_chr   = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_ACID))     p_ptr->immune_acid   = TRUE;
		if (f2 & (TR2_IM_ELEC))     p_ptr->immune_elec   = TRUE;
		if (f2 & (TR2_IM_FIRE))     p_ptr->immune_fire   = TRUE;
		if (f2 & (TR2_IM_COLD))     p_ptr->immune_cold   = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID))    p_ptr->resist_acid   = TRUE;
		if (f2 & (TR2_RES_ELEC))    p_ptr->resist_elec   = TRUE;
		if (f2 & (TR2_RES_FIRE))    p_ptr->resist_fire   = TRUE;
		if (f2 & (TR2_RES_COLD))    p_ptr->resist_cold   = TRUE;
		if (f2 & (TR2_RES_POIS))    p_ptr->resist_pois   = TRUE;
		if (f2 & (TR2_RES_LITE))    p_ptr->resist_lite   = TRUE;
		if (f2 & (TR2_RES_DARK))    p_ptr->resist_dark   = TRUE;
		if (f2 & (TR2_RES_FEAR))    p_ptr->resist_fear   = TRUE;
		if (f2 & (TR2_RES_BLIND))   p_ptr->resist_blind  = TRUE;
		if (f2 & (TR2_RES_CONFU))   p_ptr->resist_confu  = TRUE;
		if (f2 & (TR2_RES_SOUND))   p_ptr->resist_sound  = TRUE;
		if (f2 & (TR2_RES_SHARD))   p_ptr->resist_shard  = TRUE;
		if (f2 & (TR2_RES_NEXUS))   p_ptr->resist_nexus  = TRUE;
		if (f2 & (TR2_RES_NETHR))   p_ptr->resist_nethr  = TRUE;
		if (f2 & (TR2_RES_CHAOS))   p_ptr->resist_chaos  = TRUE;
		if (f2 & (TR2_RES_DISEN))   p_ptr->resist_disen  = TRUE;

		/* Miscellaneous flags */
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest   = TRUE;
		if (f3 & (TR3_FEATHER))     p_ptr->ffall         = TRUE;
		if (f3 & (TR3_LITE))        p_ptr->glowing       = TRUE;
		if (f3 & (TR3_REGEN))       p_ptr->regenerate    = TRUE;
		if (f3 & (TR3_TELEPATHY))   p_ptr->telepathy     = TRUE;
		if (f3 & (TR3_SEE_INVIS))   p_ptr->see_inv       = TRUE;
		if (f3 & (TR3_FREE_ACT))    p_ptr->free_act      = TRUE;
		if (f3 & (TR3_HOLD_LIFE))   p_ptr->hold_life     = TRUE;
		if (f3 & (TR3_BLESSED))     p_ptr->bless_blade   = TRUE;

		/* Bad flags */
		if (f3 & (TR3_NOMAGIC))     p_ptr->nomagic       = TRUE;
		if (f3 & (TR3_TELEPORT))    p_ptr->teleport      = TRUE;
		if (f3 & (TR3_AGGRAVATE))   p_ptr->aggravate     = TRUE;
		if (f3 & (TR3_DRAIN_EXP))   p_ptr->drain_exp     = TRUE;
		if (f3 & (TR3_DRAIN_HP))    p_ptr->being_crushed = TRUE;
		if (f3 & (TR3_SOULSTEAL))   p_ptr->soulsteal     = TRUE;


		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply bonuses to AC */
		p_ptr->to_a += o_ptr->to_a;
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Weapon bonuses to Skill and Deadliness are applied in battle */
		if (is_any_weapon(o_ptr)) continue;

		/* Skill bonuses apply to both weapons and martial arts */
		hit_bonus += o_ptr->to_h;

		/* Deadliness bonuses apply to weapons and martial arts */
		p_ptr->to_d += o_ptr->to_d;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}

	/* Apply bonuses to the non-magical combat skills */
	p_ptr->skill_thn  += hit_bonus * BTH_PLUS_ADJ;
	p_ptr->skill_thn2 += hit_bonus * BTH_PLUS_ADJ;
	p_ptr->skill_thb  += hit_bonus * BTH_PLUS_ADJ;
	p_ptr->skill_tht  += hit_bonus * BTH_PLUS_ADJ;


	/* Sorcerers and necromancers are hindered by most gloves */
	if (p_ptr->realm == MAGE || p_ptr->realm == NECRO)
	{
		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Character is wearing gloves */
		if (o_ptr->k_idx)
		{
			/* Get object attributes */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* Normal gloves hurt spells */
			if (!(f3 & (TR3_FREE_ACT)) &&
				 (get_object_pval(o_ptr, TR_PVAL_DEVICE) <= 0) &&
				 (get_object_pval(o_ptr, TR_PVAL_DEX) <= 0))
			{
				/* Encumbered */
				p_ptr->cumber_glove = TRUE;
			}
		}
	}

	/* Take note when "glove state" changes */
	if ((old_cumber_glove != p_ptr->cumber_glove) && (!character_silent))
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msg_print("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}
	}


	/*** Temporary conditions ***/

	/*
	 * Anything that can be expressed in pval or flag form should go in
	 * "player_flags()" or "player_flags_pval()".
	 */


	/* Apply stunning  */
	if (p_ptr->stun >= HVY_STUN)
	{
		p_ptr->skill_thn  /= 2;
		p_ptr->skill_thn2 /= 2;
		p_ptr->skill_thb  /= 2;
		p_ptr->skill_tht  /= 2;

		p_ptr->to_d -= 10;
		p_ptr->dis_to_d -= 10;
	}
	else if (p_ptr->stun)
	{
		p_ptr->skill_thn  -= p_ptr->skill_thn / 3;
		p_ptr->skill_thn2 -= p_ptr->skill_thn2 / 3;
		p_ptr->skill_thb  -= p_ptr->skill_thb / 3;
		p_ptr->skill_tht  -= p_ptr->skill_tht / 3;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}

	/* Apply fear */
	if (p_ptr->afraid)
	{
		p_ptr->skill_thb -= p_ptr->skill_thb / 3;
		p_ptr->skill_tht -= p_ptr->skill_tht / 3;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}

	/* Wizardly protection */
	if (p_ptr->wiz_prot)
	{
		if (p_ptr->skill_sav <= 100)
			p_ptr->skill_sav += (100 - p_ptr->skill_sav) / 2;

		p_ptr->resist_mana_drain = TRUE;
	}

	/* Temporary holy aura (replaces blessing) */
	if (p_ptr->holy)
	{
		p_ptr->to_a += 20;
		p_ptr->dis_to_a += 20;
		p_ptr->skill_thn += 5;
		p_ptr->skill_thn2 += 5;
		p_ptr->skill_thb += 5;
		p_ptr->skill_tht += 5;
	}
	/* Temporary blessing */
	else if (p_ptr->blessed)
	{
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
		p_ptr->skill_thn += 5;
		p_ptr->skill_thn2 += 5;
		p_ptr->skill_thb += 5;
		p_ptr->skill_tht += 5;
	}

	/* Temporary shield (not cumulative) */
	if ((p_ptr->steelskin) || (p_ptr->shield))
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->skill_thn += 20;
		p_ptr->skill_thn2 += 20;
		p_ptr->skill_thb += 20;
		p_ptr->skill_tht += 20;
	}

	/* Temporary Berserk */
	if (p_ptr->berserk)
	{
		/* In the "rage" portion of the berserker fit */
		if (p_ptr->berserk > BERSERK_WEAKNESS_LENGTH)
		{
			int tmp;

			/* Berserkers are great in melee, but not in ranged combat */
			p_ptr->skill_thn += 36;
			p_ptr->skill_thn2 += 36;
			p_ptr->skill_thb -= 36;
			p_ptr->skill_tht -= 36;

			/* Bonus to Deadliness */
			tmp = (3 + p_ptr->to_d / 6);
			p_ptr->to_d     += tmp;
			p_ptr->dis_to_d += tmp;

			/* Berserkers care nothing for their safety */
			p_ptr->to_a -= 15;
			p_ptr->dis_to_a -= 15;
		}

		/* In the "crash" portion of the berserker fit */
		else
		{
			/* No non-stat effects */
		}
	}

	/* Temporary Necromantic rage */
	if (p_ptr->necro_rage)
	{
		/* In the "power" portion of the necromancer rage */
		if (p_ptr->necro_rage > NECRO_WEAKNESS_LENGTH)
		{
			/* Terribly fast (not cumulative) */
			if (!p_ptr->fast) p_ptr->pspeed += 10;
		}

		/* In the "crash" portion of the necromantic rage */
		else
		{
			/* Great lassitude */
			if (!p_ptr->slow) p_ptr->pspeed -= 10;
		}
	}

	/* Temporary "mental_barrier" */
	if (p_ptr->mental_barrier)
	{
		int diff = 100 - p_ptr->skill_sav;
		if (diff > 20) diff = 20;
		if (diff <  0) diff =  0;

		p_ptr->skill_sav += diff;
	}


	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra += 5;
	}

	if (p_ptr->see_infra < 0) p_ptr->see_infra = 0;

	/* Temporary invisibility */
	if (p_ptr->tim_invis)
	{
		p_ptr->invisible += p_ptr->tim_inv_pow;
	}

	/* Dodging yields a minimum base armor class */
	if (p_ptr->ac < get_skill(S_DODGING, 0, 50))
	{
		p_ptr->dis_ac = p_ptr->ac = get_skill(S_DODGING, 0, 50);
	}

	/* Special case -- Half-Trolls have very tough skin */
	if (p_ptr->prace == RACE_HALF_TROLL)
	{
		p_ptr->ac     += p_ptr->power / 3;
		p_ptr->dis_ac += p_ptr->power / 3;
	}

	/* Special case -- Ents have *very* tough skin */
	if (p_ptr->prace == RACE_ENT)
	{
		p_ptr->ac     += 10 + 2 * p_ptr->power / 5;
		p_ptr->dis_ac += 10 + 2 * p_ptr->power / 5;
	}

	/* Handle lack of sanctity -- frowned upon by the Divine */
	if (p_ptr->unsanctified)
	{
		p_ptr->to_a -= 25;
		p_ptr->dis_to_a -= 25;
	}


	/*** Analyze weapons ***/
	analyze_weapons();



	/*** Cancellation of flags ***/

	/* Extract the player cancellation flags (including shapechanges) */
	player_flags_cancel(&f1, &f2, &f3, TRUE);

	/* Sustain flags */
	if (f1 & (TR1_SUST_STR))       p_ptr->sustain_str   = FALSE;
	if (f1 & (TR1_SUST_INT))       p_ptr->sustain_int   = FALSE;
	if (f1 & (TR1_SUST_WIS))       p_ptr->sustain_wis   = FALSE;
	if (f1 & (TR1_SUST_DEX))       p_ptr->sustain_dex   = FALSE;
	if (f1 & (TR1_SUST_CON))       p_ptr->sustain_con   = FALSE;
	if (f1 & (TR1_SUST_CHR))       p_ptr->sustain_chr   = FALSE;

	/* Immunity flags */
	if (f2 & (TR2_IM_ACID))        p_ptr->immune_acid   = FALSE;
	if (f2 & (TR2_IM_ELEC))        p_ptr->immune_elec   = FALSE;
	if (f2 & (TR2_IM_FIRE))        p_ptr->immune_fire   = FALSE;
	if (f2 & (TR2_IM_COLD))        p_ptr->immune_cold   = FALSE;

	/* Resistance flags */
	if (f2 & (TR2_RES_ACID))       p_ptr->resist_acid   = FALSE;
	if (f2 & (TR2_RES_ELEC))       p_ptr->resist_elec   = FALSE;
	if (f2 & (TR2_RES_FIRE))       p_ptr->resist_fire   = FALSE;
	if (f2 & (TR2_RES_COLD))       p_ptr->resist_cold   = FALSE;
	if (f2 & (TR2_RES_POIS))       p_ptr->resist_pois   = FALSE;
	if (f2 & (TR2_RES_LITE))       p_ptr->resist_lite   = FALSE;
	if (f2 & (TR2_RES_DARK))       p_ptr->resist_dark   = FALSE;
	if (f2 & (TR2_RES_FEAR))       p_ptr->resist_fear   = FALSE;
	if (f2 & (TR2_RES_BLIND))      p_ptr->resist_blind  = FALSE;
	if (f2 & (TR2_RES_CONFU))      p_ptr->resist_confu  = FALSE;
	if (f2 & (TR2_RES_SOUND))      p_ptr->resist_sound  = FALSE;
	if (f2 & (TR2_RES_SHARD))      p_ptr->resist_shard  = FALSE;
	if (f2 & (TR2_RES_NEXUS))      p_ptr->resist_nexus  = FALSE;
	if (f2 & (TR2_RES_NETHR))      p_ptr->resist_nethr  = FALSE;
	if (f2 & (TR2_RES_CHAOS))      p_ptr->resist_chaos  = FALSE;
	if (f2 & (TR2_RES_DISEN))      p_ptr->resist_disen  = FALSE;

	/* Miscellaneous flags */
	if (f3 & (TR3_SLOW_DIGEST))    p_ptr->slow_digest   = FALSE;
	if (f3 & (TR3_FEATHER))        p_ptr->ffall         = FALSE;
	if (f3 & (TR3_LITE))           p_ptr->glowing       = FALSE;
	if (f3 & (TR3_REGEN))          p_ptr->regenerate    = FALSE;
	if (f3 & (TR3_TELEPATHY))      p_ptr->telepathy     = FALSE;
	if (f3 & (TR3_SEE_INVIS))      p_ptr->see_inv       = FALSE;
	if (f3 & (TR3_FREE_ACT))       p_ptr->free_act      = FALSE;
	if (f3 & (TR3_HOLD_LIFE))      p_ptr->hold_life     = FALSE;
	if (f3 & (TR3_BLESSED))        p_ptr->bless_blade   = FALSE;

	/* Bad flags */
	if (f3 & (TR3_NOMAGIC))        p_ptr->nomagic       = FALSE;
	if (f3 & (TR3_TELEPORT))       p_ptr->teleport      = FALSE;
	if (f3 & (TR3_AGGRAVATE))      p_ptr->aggravate     = FALSE;
	if (f3 & (TR3_DRAIN_EXP))      p_ptr->drain_exp     = FALSE;
	if (f3 & (TR3_DRAIN_HP))       p_ptr->being_crushed = FALSE;

	/* Handle vulnerabilities */
	player_flags_vulnerable(&f1, &f2, &f3, TRUE);

	/* Vunlerability flags */
	if (f2 & (TR2_RES_ACID))       p_ptr->vuln_acid   = FALSE;
	if (f2 & (TR2_RES_ELEC))       p_ptr->vuln_elec   = FALSE;
	if (f2 & (TR2_RES_FIRE))       p_ptr->vuln_fire   = FALSE;
	if (f2 & (TR2_RES_COLD))       p_ptr->vuln_cold   = FALSE;
	if (f2 & (TR2_RES_POIS))       p_ptr->vuln_pois   = FALSE;
	if (f2 & (TR2_RES_LITE))       p_ptr->vuln_lite   = FALSE;
	if (f2 & (TR2_RES_DARK))       p_ptr->vuln_dark   = FALSE;
	if (f2 & (TR2_RES_CONFU))      p_ptr->vuln_confu  = FALSE;
	if (f2 & (TR2_RES_SOUND))      p_ptr->vuln_sound  = FALSE;
	if (f2 & (TR2_RES_SHARD))      p_ptr->vuln_shard  = FALSE;
	if (f2 & (TR2_RES_NEXUS))      p_ptr->vuln_nexus  = FALSE;
	if (f2 & (TR2_RES_NETHR))      p_ptr->vuln_nethr  = FALSE;
	if (f2 & (TR2_RES_CHAOS))      p_ptr->vuln_chaos  = FALSE;
	if (f2 & (TR2_RES_DISEN))      p_ptr->vuln_disen  = FALSE;


	/*** Notice changes ***/

	/* Check stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Notice changes */
		if (p_ptr->stat_use[i] != old_stat_use[i])
		{
			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
		}

		/* Notice changes */
		if (p_ptr->stat_ind[i] != old_stat_ind[i])
		{
			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in spell stat affects Mana/Spells */
			else if ((i == mp_ptr->spell_stat) && (p_ptr->realm != NONE))
			{
				p_ptr->update |= (PU_MANA | PU_SPELLS);
			}
		}
	}

	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		/* Update monster visibility */
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Tweak down very high invisibility (game-balancing) */
	if (p_ptr->invisible > 40)
		p_ptr->invisible -= (p_ptr->invisible - 40) / 2;

	/* Hack -- Invisibility change */
	if (p_ptr->invisible != old_invisibility)
	{
		/* Redraw conditions status */
		p_ptr->redraw |= (PR_CONDITIONS);

		/* Print "invisibility" */
		left_panel_display(DISPLAY_INVISIBILITY, 0);
	}

	/* Redraw speed (if needed) */
	if (p_ptr->pspeed != old_speed)
	{
		/* Redraw speed */
		p_ptr->redraw |= (PR_SPEED);
	}

	/* Recalculate mana if needed */
	if (p_ptr->mana_bonus != old_mana_bonus)
	{
		p_ptr->update |= (PU_MANA);
	}

	/* Recalculate stealth when needed */
	if ((p_ptr->skill_stl != old_stealth) || (!p_ptr->skill_stl))
	{
		/* Assume character is extremely noisy. */
		p_ptr->base_wakeup_chance = 100 * WAKEUP_ADJ;

		/* For every increase in stealth past 0, multiply wakeup chance by 0.8. */
		for (i = 0; i < p_ptr->skill_stl; i++)
		{
			p_ptr->base_wakeup_chance = 4 * p_ptr->base_wakeup_chance / 5;

			/* Always make at least some innate noise */
			if (p_ptr->base_wakeup_chance < 25)
			{
				p_ptr->base_wakeup_chance = 25;
				break;
			}
		}
	}

	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}



	/* Hack -- optionally silence messages */
	if (character_silent) return;


	/* Let the player know whether their secondary is any use. */
	if (old_twoweap != p_ptr->twoweap)
	{
		if ((p_ptr->twoweap) && (p_ptr->num_blow2 == 0))
		{
			msg_print("You are unable to use your secondary weapon effectively.");
		}
	}

	/* Take note when "heavy bow" changes */
	if (old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			msg_print("You have no trouble wielding your bow.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}
	}

	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}
	}

	/* Take note when "illegal weapon" changes */
	if (old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			msg_print("You feel that you are wielding a weapon of impiety...");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You grip a sanctified weapon, and feel the Almighty smile upon you.");
		}
		else
		{
			msg_print("You unhand your impious weapon, and feel The Almighty smile upon you.");
		}
	}

	/* Take note when player moves his shield on and off his back. */
	if (old_shield_on_back != p_ptr->shield_on_back)
	{
		o_ptr = &inventory[INVEN_ARM];

		/* Player is using a shield */
		if ((o_ptr->k_idx) && (o_ptr->tval == TV_SHIELD))
		{
			/* Status change messages */
			if (p_ptr->shield_on_back)
			{
				msg_print("You are carrying your shield on your back.");
			}
			else
			{
				msg_print("You are carrying your shield in your hand.");
			}
		}

		/* No message for players no longer carrying a shield. */
	}

	/* Print "regen" (unless dead or in a special display) */
	if ((!p_ptr->is_dead) && (!main_screen_inactive))
		left_panel_display(DISPLAY_REGEN, 0);

}



/*
 * Allow messages to be sent to a window only if we're pretty sure the window is
 * requested and available.  Otherwise, this option makes a lovely pistol.
 */
static void verify_message_to_window(void)
{
	int j;

	/* Assume that messages cannot be automatically sent to a window */
	message_to_window_active = FALSE;

	/* Verify that requested windows include one for messages */
	if (message_to_window)
	{
		/* Scan sub-windows */
		for (j = TERM_SUBWINDOW; j < TERM_MAX; j++)
		{
			/* No term, or term is unavailable */
			if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

			/* No relevant flags */
			if (!(op_ptr->window_flag[j] & (PW_MESSAGE))) continue;

			/* Turn on the option */
			message_to_window_active = TRUE;

			/* All done */
			return;
		}
	}
}


/*
 * Handle "p_ptr->notice".  Do not set it to 0.
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
		(void)process_quiver(0, NULL);
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		(void)reorder_pack(-1, -1, TRUE);
	}
}


/*
 * Handle "p_ptr->update", setting it to 0 when complete.
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_noise(TRUE);
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		calc_hitpoints();
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		calc_mana();
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* Main screen is inactive, no screen updates */
	if (main_screen_inactive) return;


	if (p_ptr->update & (PU_FORGET_VIEW))
	{
		p_ptr->update &= ~(PU_FORGET_VIEW);
		forget_view();
	}

	if (p_ptr->update & (PU_UPDATE_VIEW))
	{
		p_ptr->update &= ~(PU_UPDATE_VIEW);
		update_view();
	}

	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MONSTERS))
	{
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(FALSE);
	}

	if (p_ptr->update & (PU_PANEL))
	{
		p_ptr->update &= ~(PU_PANEL);
		verify_panel(p_ptr->move_dir, FALSE);
	}

	if (p_ptr->update & (PU_SCORE))
	{
		p_ptr->update &= ~(PU_SCORE);
		(void)total_points();
		left_panel_display(DISPLAY_SCORE, 0);
	}

	/* No further updates */
	p_ptr->update = 0L;
}


/*
 * Handle "p_ptr->redraw", setting it to 0 when complete.
 */
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* No screen updates if main screen is inactive */
	if (main_screen_inactive) return;


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}

	if (p_ptr->redraw & (PR_BASIC))
	{
		p_ptr->redraw &= ~(PR_BASIC);
		p_ptr->redraw &= ~(PR_NAME | PR_RACE | PR_TITLE);
		p_ptr->redraw &= ~(PR_EXP | PR_GOLD | PR_EQUIPPY | PR_STATS);
		p_ptr->redraw &= ~(PR_SHAPE);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_HEALTH);
		prt_frame_basic();
	}

	if (p_ptr->redraw & (PR_HEALTH_EXTRA))
	{
		p_ptr->redraw &= ~(PR_HEALTH_EXTRA);
		print_left_panel(TRUE);
	}

	if (p_ptr->redraw & (PR_NAME))
	{
		p_ptr->redraw &= ~(PR_NAME);
		prt_short_name();
	}

	/* No effect (at least for now) */
	if (p_ptr->redraw & (PR_RACE))
	{
		p_ptr->redraw &= ~(PR_RACE);
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp();
	}

	if (p_ptr->redraw & (PR_GOLD))
	{
		p_ptr->redraw &= ~(PR_GOLD);
		prt_gold();
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		prt_equippy();
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		int i;

		p_ptr->redraw &= ~(PR_STATS);
		for (i = 0; i < A_MAX; i++) prt_stat(i);
	}

	if (p_ptr->redraw & (PR_SHAPE))
	{
		p_ptr->redraw &= ~(PR_SHAPE);
		prt_shape();
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac();
	}

	if (p_ptr->redraw & (PR_HP))
	{
		p_ptr->redraw &= ~(PR_HP);
		prt_hp();
	}

	if (p_ptr->redraw & (PR_MANA))
	{
		p_ptr->redraw &= ~(PR_MANA);
		prt_sp();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_HUNGER | PR_CONDITIONS);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_UNCAST | PR_DEPTH);
		prt_frame_extra();
	}

	if (p_ptr->redraw & (PR_HUNGER))
	{
		p_ptr->redraw &= ~(PR_HUNGER);
		prt_hunger();
	}

	if (p_ptr->redraw & (PR_CONDITIONS))
	{
		p_ptr->redraw &= ~(PR_CONDITIONS);
		prt_conditions();
	}

	if (p_ptr->redraw & (PR_STATE))
	{
		p_ptr->redraw &= ~(PR_STATE);
		prt_state();
	}

	if (p_ptr->redraw & (PR_SPEED))
	{
		p_ptr->redraw &= ~(PR_SPEED);
		prt_speed();
	}

	if (p_ptr->redraw & (PR_UNCAST))
	{
		p_ptr->redraw &= ~(PR_UNCAST);
		prt_uncast();
	}

	if (p_ptr->redraw & (PR_DEPTH))
	{
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_depth();
	}

	/* No further redraws */
	p_ptr->update = 0L;
}


/*
 * Handle "p_ptr->window", setting it to 0 when complete.
 */
void window_stuff(void)
{
	int j;
	u32b mask = 0L;

	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (j = 0; j < TERM_MAX; j++)
	{
		/* Save usable flags */
		if (angband_term[j])
		{
			/* Build the mask */
			mask |= (op_ptr->window_flag[j]);
		}
	}

	/* Allow messages to be sent to a window only if safe */
	verify_message_to_window();


	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Display inventory */
	if (p_ptr->window & (PW_INVEN))
	{
		p_ptr->window &= ~(PW_INVEN);
		fix_inven();
	}

	/* Display equipment */
	if (p_ptr->window & (PW_EQUIP))
	{
		p_ptr->window &= ~(PW_EQUIP);
		fix_equip();
	}

	/* Display player (mode 0) */
	if (p_ptr->window & (PW_PLAYER_0))
	{
		p_ptr->window &= ~(PW_PLAYER_0);
		fix_player_0();
	}

	/* Display player (mode 1) */
	if (p_ptr->window & (PW_PLAYER_1))
	{
		p_ptr->window &= ~(PW_PLAYER_1);
		fix_player_1();
	}

	/* Display messages */
	if (p_ptr->window & (PW_MESSAGE))
	{
		p_ptr->window &= ~(PW_MESSAGE);
		fix_message();
	}

	/* Display nearby monster and object lists */
	if (p_ptr->window & (PW_M_LIST | PW_O_LIST))
	{
		p_ptr->window &= ~(PW_M_LIST);
		fix_m_list();

		p_ptr->window &= ~(PW_O_LIST);
		fix_o_list();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_OVERHEAD))
	{
		p_ptr->window &= ~(PW_OVERHEAD);
		fix_overhead();
	}

	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
	}

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}

	/* Display command list */
	if (p_ptr->window & (PW_CMDLIST))
	{
		p_ptr->window &= ~(PW_CMDLIST);
		fix_cmdlist();
	}

	/* No further sub-window refreshes */
	p_ptr->window = 0L;
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

	/* Window stuff */
	if (p_ptr->window) window_stuff();
}
