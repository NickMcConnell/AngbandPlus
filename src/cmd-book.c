/* File: cmd-book.c */
/* 
 * Purpose: Handle player spellcasting from arcane, divine, and device 
 * "spellbooks"			
 */

/*
 * cmd-book.c is your dark path to the characters ability to manipulate
 * dark forces. First a function that determines mana cost by class 
 * handicap, then one to determine the chance of failure. This
 * is followed by a function that contains information about whether or
 * not a player can cast a specific spell. Then come a slew of mysterious
 * functions for getting, browsing, printing and counting spells. Rounding
 * out the above functions are the actual means by which the pla- I mean
 * the character interacts with neferious dark arcane demonic forces, 
 * holy mystic divine energies, or strange eldrich devices. 
 *
 * The majority of the code below is taken directly from Eyangband  
 * We do not need literate() function because all classes can use books. 
 * We do not need spellcaster() because all classes can cast spells  
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Returns mana cost for a spell, Mana cost should not be affected 
 * by skills
 */
static s16b spell_mana(int book, int spell)
{
	int mana, handicap;

	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Extract the base spell mana */
	mana = s_ptr->smana;

	handicap = (cp_ptr->spell_handicap[book] - 1);

	/* Modify for handicap */
	/* every six points of handicap doubles the cost of the spell */
	mana += ((mana / 6) * handicap);

	return mana;
}


/*
 * Returns chance of failure for a spell
 *
 * Magelore is a combination of all your occult skills,
 * Occult, Advanced Occult, and Cthulhu Mythos.
 *
 * Praylore is a combination of your prayer skills,
 * Spirituality, Prayer, and Devotion.
 * 
 * Gear is your gadgeteering score.
 *
 * Known is value for the highest level spell you can
 * cast, it is a total of Latin + Praylore, or Latin +
 * Magelore (depending on the book), or Gear + your skill
 * in that specific device.
 * 
 * Magic books rely on Ego, Prayer books rely on Ego/Sch,
 * and devices rely on Sch for reduction of fail rates.
 */
static s16b spell_chance(int book, int spell)
{
	int chance, minfail, mana;
	int handicap;
	int spell_stat1, spell_stat2, magelore, praylore, gear;

	int stat_factor;
	magic_type *s_ptr;
	int known;
	
	known = 0;
	magelore = 1;
	praylore = 1;
	gear = 1;
	spell_stat1 = spell_stat2 = 10;
	
	if (p_ptr->skills[SK_OCCULT].skill_max > 0)
	{
		/* Add Occult to magelore */
		magelore += p_ptr->skills[SK_OCCULT].skill_rank;
	}
	if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
	{
		/* Add Advanced Occult to magelore */
		magelore += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
	}
	if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
	{
		/* Add Cthulhu Mythos to magelore */
		magelore += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
	}
	if (p_ptr->skills[SK_SPIRITUALITY].skill_max > 0)
	{
		/* Add Spirituality to praylore */
		praylore += p_ptr->skills[SK_SPIRITUALITY].skill_rank;
	}
	if (p_ptr->skills[SK_PRAYER].skill_max > 0)
	{
		/* Add prayer to praylore */
		praylore += p_ptr->skills[SK_PRAYER].skill_rank;
	}
	if (p_ptr->skills[SK_DEVOTION].skill_max > 0)
	{
		/* Add Devotion to praylore */
		praylore += p_ptr->skills[SK_DEVOTION].skill_rank;
	}
	if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
	{
		/* Add Gadgeteer to Gear */
		gear += p_ptr->skills[SK_GADGETEER].skill_rank;
	}
	
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			if (p_ptr->skills[SK_OCCULT].skill_max > 0)
			{
				/* Normal Max of 40 */
				known += p_ptr->skills[SK_OCCULT].skill_rank;
			}
			if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
			{
				/* Normal Max of 60 */
				known += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
			}
			if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
			{
				/* Normal Max of 80 */
				known += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
			}
			break;
		}
		case SBF_PRAYER:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			if (p_ptr->skills[SK_SPIRITUALITY].skill_max > 0)
			{
				/* Normal Max of 40 */
				known += p_ptr->skills[SK_SPIRITUALITY].skill_rank;
			}
			if (p_ptr->skills[SK_PRAYER].skill_max > 0)
			{
				/* Normal Max of 60 */
				known += p_ptr->skills[SK_PRAYER].skill_rank;
			}
			if (p_ptr->skills[SK_DEVOTION].skill_max > 0)
			{
				/* Normal Max of 80 */
				known += p_ptr->skills[SK_DEVOTION].skill_rank;
			}
			break;
		}
		/* each device has a known total of 40 */
		case SBF_DEVICE:
		{
			switch(book)
			{
				case SV_BOOK_DEVICE1:
				{
					known += p_ptr->skills[SK_UTILITY_BANDOLIER].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE2:
				{
					known += p_ptr->skills[SK_DETECTIVES_KIT].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE3:
				{
					known += p_ptr->skills[SK_CLOCKWORK_CHASSIS].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE4:
				{
					known += p_ptr->skills[SK_CLOCKWORK_CARBINE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE5:
				{
					known += p_ptr->skills[SK_VELOCIPEDE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE6:
				{
					known += p_ptr->skills[SK_ANALYTIC_ENGINE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE7:
				{
					break;
				}
				case SV_BOOK_DEVICE8:
				{
					break;
				}
				case SV_BOOK_DEVICE9:
				{
					break;
				}
				case SV_BOOK_DEVICE10:
				{
					break;
				}
				default:
				{
					break;
				}
			}
		}	
		default:
		{
			break;
		}
	}

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;
	
	/* Extract handicap */
	handicap = cp_ptr->spell_handicap[book] - 1;
	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 2 * (known - (s_ptr->slevel + handicap));
	
	if ((books[book].flags & SBF_TYPE_MASK) == SBF_MAGIC)
	{
		spell_stat1 = p_ptr->stat_use[A_EGO];
		spell_stat2 = p_ptr->stat_use[A_EGO];
	}

	if ((books[book].flags & SBF_TYPE_MASK) == SBF_PRAYER)
	{
		spell_stat1 = p_ptr->stat_use[A_SCH];
		spell_stat2 = p_ptr->stat_use[A_EGO];
	}

	if ((books[book].flags & SBF_TYPE_MASK) == SBF_DEVICE)
	{
		spell_stat1 = p_ptr->stat_use[A_SCH];
		spell_stat2 = p_ptr->stat_use[A_SCH];
	}

	/* Reduce failure rate by stat adjustment */
	stat_factor = ((spell_stat1 + spell_stat2) / 2);
	chance -= 2 * (stat_factor / 50);

	mana = spell_mana(book, spell);

	/* Not enough mana to cast */
	if (mana > p_ptr->csp)
	{
		chance += 5 * (mana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			minfail = 15 + (s_ptr->slevel + handicap) - (stat_factor / 50) - magelore;
			if (p_ptr->anti_magic) minfail = 100;
			break;
		}
		case SBF_PRAYER:
		{
			minfail = 15 + (s_ptr->slevel + handicap) - (stat_factor / 50) - praylore;
			if (p_ptr->anti_magic) minfail = 100;
			break;
		}
		case SBF_DEVICE:
		{
			minfail = 20 - (stat_factor / 50) - gear;
			break;
		}
		default:
		{
			minfail = 15;
			break;
		}
	}
	if (minfail < 0) minfail = 0;

	/* Non mage/priest/mystic characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Spell disruption (after minfail)*/
	if (p_ptr->spell_disrupt) 
	{
		chance = 3 * (chance + 3);
	}	
	
	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	if (p_ptr->confused)	
	{
		/* Confusion makes spells harder */
		chance += 20;
	}

	/* Always a 5 percent chance of working */
	if (p_ptr->anti_magic) 
	{
		/* Nothing */
	}
	else if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}


/*
 * Determine if a spell is "okay" for the player to cast.
 * What determines if a spell is "okay" is simply their skill levels
 * and whether or not they have access to the book or device.
 *
 * Known is an interger value to determine weither or not 
 * We possess enough skill and knowledge to cast the spell
 * It is checked against the 'level' of the spell.
 */

static bool spell_okay(int book, int spell)
{
	magic_type *s_ptr;
	int known;
	
	known = 0;
	
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			if (p_ptr->skills[SK_OCCULT].skill_max > 0)
			{
				/* Max of 40 */
				known += p_ptr->skills[SK_OCCULT].skill_rank;
			}
			if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
			{
				/* Max of 60 */
				known += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
			}
			if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
			{
				/* Max of 80 */
				known += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
			}
			break;
		}
		case SBF_PRAYER:
		{
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			if (p_ptr->skills[SK_SPIRITUALITY].skill_max > 0)
			{
				known += p_ptr->skills[SK_SPIRITUALITY].skill_rank;
			}
			if (p_ptr->skills[SK_PRAYER].skill_max > 0)
			{
				known += p_ptr->skills[SK_PRAYER].skill_rank;
			}
			if (p_ptr->skills[SK_DEVOTION].skill_max > 0)
			{
				known += p_ptr->skills[SK_DEVOTION].skill_rank;
			}
			break;
		}
		/* each device has a known total of 40 */
		case SBF_DEVICE:
		{
			switch(book)
			{
				case SV_BOOK_DEVICE1:
				{
					known += p_ptr->skills[SK_UTILITY_BANDOLIER].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE2:
				{
					known += p_ptr->skills[SK_DETECTIVES_KIT].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE3:
				{
					known += p_ptr->skills[SK_CLOCKWORK_CHASSIS].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE4:
				{
					known += p_ptr->skills[SK_CLOCKWORK_CARBINE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE5:
				{
					known += p_ptr->skills[SK_VELOCIPEDE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE6:
				{
					known += p_ptr->skills[SK_ANALYTIC_ENGINE].skill_rank;
					if (p_ptr->skills[SK_GADGETEER].skill_rank)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE7:
				{
					break;
				}
				case SV_BOOK_DEVICE8:
				{
					break;
				}
				case SV_BOOK_DEVICE9:
				{
					break;
				}
				case SV_BOOK_DEVICE10:
				{
					break;
				}
				default:
				{
					break;
				}
			}
		}	
		default:
		{
			break;
		}
	}
	
	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Spell is illegal */
	if ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1)) > known) return (FALSE);

	/* Not sure if this is what it needs to return */
	return (TRUE);
}



/*
 * Extra information on a spell		-DRS-
 *
 * We can use up to 20 characters of the buffer 'p'
 *
 * Originally taken from Oangband -LM-
 */
static void spell_info(char *p, int spell_index)
{
	/* add, and pass the skill values */
	int magepower = p_ptr->skills[SK_RITUAL_MAGIC].skill_rank;
	int willpower = 1;
	int gearhead = 1;
	
	/* Various class flags influence things */
	/* Should probably not make beam based off level */
	int	beam = ((cp_ptr->flags & CF_BEAM) ? ((p_ptr->lev - 10) * 2) : (p_ptr->lev - 10));
	
	/* Mage power goes from 1-40, willpower from 1-60 */
	if (p_ptr->skills[SK_THELMA].skill_max > 0)
	{
		magepower += p_ptr->skills[SK_THELMA].skill_rank;
	}
	if (p_ptr->skills[SK_TMPR_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_TMPR_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_HARD_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_HARD_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_IRON_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_IRON_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
	{
		gearhead = p_ptr->skills[SK_GADGETEER].skill_rank;
	}

	/* Some more calculations for beam chance */
	if (beam < 0) beam = 0;

	/* Default */
	strcpy(p, "");

	/* XXX XXX Analyze the spell */
	switch (spell_index)
	{
		case POW_AIM_OF_THE_WILL:
			sprintf(p, " ice dmg %dd%d", 3 + ((magepower) / 4), (4 + (willpower / 5))); break;
		case POW_SENSE_WILL:
			strcpy(p, " sense life"); break;
		case POW_ASTRAL_GATE:
			sprintf(p, " range %d", 10 + willpower / 2); break;
		case POW_INNER_RAD:
			sprintf(p, " damage 2d%d", magepower); break;
		case POW_DEMONIC_WILL:
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_LO_OBJECT:
			strcpy(p, " locate object"); break;
		case POW_LO_TRAPS: 
			strcpy(p, " locate traps"); break;
		case POW_PLAGUE_WILL:
			sprintf(p, " damage %dd%d", 4 + (magepower / 4), 6 + (willpower / 20)); break;
		case POW_RECHARGE:
			sprintf(p, " %d charges", magepower); break;
		case POW_SUPPRESSION_WILL: 
			strcpy(p, " sleep"); break;
		case POW_IDENTIFY: 
			strcpy(p, " identify"); break;
		case POW_FROST_BOLT:
			sprintf(p, " damage %dd%d", 6+(magepower/3), 6 + (willpower / 8)); break;
		case POW_FIRE_BOLT:
			sprintf(p, " damage %dd%d", 6+(magepower/3), 6 + (willpower / 8)); break;
		case POW_HASTE: 
			strcpy(p, " haste"); break;
		case POW_TELEPORT_OTHER_I: 
			strcpy(p, " teleport other"); break;
		case POW_FIREBALL:
			sprintf(p, " damage %dd4", 5 + ((magepower + willpower) / 2)); break;
		case POW_GENOCIDE: 
			strcpy(p, " genocide"); break;
		case POW_ACID_BOLT:
			sprintf(p, " damage %dd12", (4 + ((magepower + willpower) / 3))); break;
		case POW_LIGHTNING_BOLT:
			sprintf(p, " damage %dd8", (6 + ((magepower + willpower) / 2))); break;
		case POW_CLOUD_KILL:
			sprintf(p, " damage %dd%d", 2 + (magepower / 4),  1 + (willpower / 30)); break;
		case POW_ICE_STORM:
			sprintf(p, " damage %dd3", (1 + (((magepower / 5) + (willpower / 10)) / 2))); break;
		case POW_FIRE_STORM:
			sprintf(p, " damage %dd4", (10 + (magepower / 2 + willpower / 2)/ 2)); break;
		case POW_METEOR_STORM:
			sprintf(p, " %d * damage %dd2", (magepower + willpower) / 10, 25 + ((magepower + willpower)/ 3)); break;
		case POW_ELEMENTAL_BALL:
			sprintf(p, " damage 4 * %dd2", 33 + ((magepower + willpower)/ 2)); break;
		case POW_SOUL_STORM:
			sprintf(p, " damage ~%d+%dd%d", willpower * 3 / 2, 1 + (magepower / 2), magepower / 3); break;
		case POW_RECHARGE_II: 
			strcpy(p, " recharge"); break;
		case POW_EARTHQUAKE_I: 
			strcpy(p, " earthquake"); break;
		case POW_WORD_OF_RECALL_I: 
			strcpy(p, " recall"); break;
		case POW_MIND_OF_WORM: 
			strcpy(p, " wormsense"); break;
		case POW_MASS_GENOCIDE: 
			strcpy(p, " mass genocide"); break;
		case POW_SUMMON_DEMON:
			strcpy(p, " summon demon"); break;
		case POW_VOORISH_SIGN:
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_BYAKHEE_WINDS:
			sprintf(p, " damage %dd%d", 2+(magepower/10), 6 + (willpower / 8)); break;
		case POW_FAUGN_BARRIER:
			strcpy(p, " wave of fear"); break;
		case POW_BANISH_DEMON:
			strcpy(p, " eliminate demon"); break;
		case POW_SUMMON_DEMON_II:
			strcpy(p, " summon demon"); break;
		case POW_LAMP_ALHAZRED:
			strcpy(p, " enlightenment"); break;
		case POW_CONTACT_YITHIAN:	
			strcpy(p, " detect magic"); break;
		case POW_INCANT_MI_GO:
			sprintf(p, " damage %d", (40 + willpower)); break;
		case POW_CONSUME_FLESH:
			sprintf(p, " causehp/healsp %d", (willpower * 2)); break;
		case POW_MELT_STONE:
			strcpy(p, " stone to mud"); break;
		case POW_CHIME_TEZCH:
			strcpy(p, " force barrier"); break;
		case POW_MIRROR_ATEP:
			strcpy(p, " non-detection"); break;
		case POW_GATE:
			sprintf(p, " range %d", (magepower + willpower)); break;
		case POW_EFFIGY_HATE:
			strcpy(p, " violence"); break;
		case POW_CONTACT_NYAR:
			strcpy(p, " *identify*"); break;
		case POW_DEMON_COURAGE: 
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_DEMON_FURY: 
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_DEMONIC_VIGOR: 
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_DEMON_SHIELD: 
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_STYGIAN_WAR:
			sprintf(p, " duration %d", (willpower * ((magepower / 10) + 1))); break;
		case POW_CHANT: 
			strcpy(p, " blessing"); break;
		case POW_SANCTUARY: 
			strcpy(p, " barrier"); break;
		case POW_HEALING_I:
			sprintf(p, " heal 8d12 + %d", willpower); break;
		case POW_PROTECTION_FROM_EVIL: 
			strcpy(p, " prot. evil"); break;
		case POW_HEALING_II:			
			sprintf(p, " heal 8d10 + %d", willpower); break;
		case POW_DESTROY_MACHINE:
			sprintf(p, " dam %dd3", 1 + willpower / 4); break;
		case POW_HOLY_BOLT:
			sprintf(p, " damn %dd%d", 2 + (willpower / 2), 4 + (willpower / 4)); break;
		case POW_REMOVE_CURSE: 
			strcpy(p, " remove curse"); break;
		case POW_TURN_UNDEAD: 
			strcpy(p, " fear undead"); break;
		case POW_HEALING_III:			
			sprintf(p, " cure cut/poison"); break;
		case POW_PORTAL:			
			sprintf(p, " range %d", willpower * 3); break;
		case POW_SENSE_INVISIBLE: 
			strcpy(p, " sense invis"); break;
		case POW_SENSE_SURROUNDINGS: 
			strcpy(p, " sense area"); break;
		case POW_SATISFY_HUNGER:
			strcpy(p, " hunger"); break;
		case POW_PRAYER: 
			strcpy(p, " heroism"); break;
		case POW_DISPEL_EVIL: 
			strcpy(p, " dispel evil"); break;
		case POW_IDENTIFY_II: 
			strcpy(p, " identify"); break;
		case POW_HOLY_WORD: 
			strcpy(p, " holy world"); break;
		case POW_HEALING_IV: 
			strcpy(p, " heal 400-800/10-20"); break;
		case POW_RESTORATION: 
			strcpy(p, " restore stats"); break;
		case POW_DISPEL_CURSE: 
			strcpy(p, " remove curse"); break;
		case POW_BANISHMENT: 
			strcpy(p, " banishment"); break;
		case POW_RECHARGE_III: 
			strcpy(p, " recharge"); break;
		case POW_DISPEL_EVIL_II: 
			strcpy(p, " dispel evil"); break;
		case POW_WORD_OF_RECALL_II: 
			strcpy(p, " recall"); break;
		case POW_RESISTANCE: 
			strcpy(p, " resistance"); break;
		case POW_HEAL_CUTS:
			strcpy(p, " heal cuts"); break;
		case POW_MINOR_RESISTANCE:
			sprintf(p, " lw rst, ~dur %d", willpower * 2); break;
		case POW_REMOVE_FEAR_II:
			strcpy(p, " remove fear"); break;
		case POW_MUSCLE_BUFF:
			sprintf(p, " inc. mus, %d-%d", willpower, willpower+48); break;
		case POW_SHATTER:
			sprintf(p, " sonic cone dam 4d%d", 6 + willpower/2); break;
		case POW_NO_TELEPORT:
			sprintf(p, " no teleport, %d-%d", willpower*6, (willpower*6)+200); break;
		case POW_VIGOR_BUFF:
			sprintf(p, " inc. vigor, %d-%d", willpower, willpower+48); break;
		case POW_STONE_MELD:
			sprintf(p, " wraithform, %d-%d", willpower/3, willpower/3+6); break;
		case POW_FREE_ACT:
			sprintf(p, " dur %d-%d", willpower*2, 128 + willpower*2); break;
		case POW_DIVINE_FAVOR:
			sprintf(p, " dur %d-%d", willpower/6, 20 + willpower/6); break;
		case POW_FLAMING_WRATH:
			sprintf(p, " dam %dd5", (20 + (willpower / 3))); break;
		case POW_ANTI_MAGIC:
			strcpy(p, " dur 1-12"); break;
		case POW_HOLY_STRIKE:
			sprintf(p, " fire/holy dam %dd8", willpower); break;
		case POW_DANCING_LIGHTS:
			strcpy(p, " moving lights"); break;
		case POW_SACRED_SERMON:
			strcpy(p, " charms monsters"); break;
		case POW_MANIFEST_GOD:
			strcpy(p, " divine intervention"); break;
		case POW_THOUGHT_SENSE:
			strcpy(p, " detect minds"); break;
		case POW_KEY:
			strcpy(p, " unlock the universe"); break;
		case POW_CALL_LIGHT: 
			strcpy(p, " light"); break;
		case POW_REMOVE_FEAR: 
			strcpy(p, " remove fear"); break;
		case POW_HEALING_VI: 
			sprintf(p, " heal %dd10+d", 2 + gearhead, gearhead); break;
		case POW_DETECT_DOOR_STAIR: 
			strcpy(p, " detect stair"); break;
		case POW_DETECT_TRAPS: 
			strcpy(p, " detect traps"); break;
		case POW_SLOW_POISON: 
			strcpy(p, " slow poison"); break;
		case POW_SPRING_BLADE:			
			sprintf(p, " damage %dd3", 3 + (gearhead / 6)); break;
		case POW_NOURISHMENT: 
			strcpy(p, " food"); break;
		case POW_OBJECT_ANALYSIS: 
			strcpy(p, " identify"); break;
		case POW_DETECT_HOSTILE: 
			strcpy(p, " detect monster"); break;
		case POW_DETECT_TRAPS_DOORS: 
			strcpy(p, " detect traps"); break;
		case POW_TREASURE_DETECTION: 
			strcpy(p, " detect gold"); break;
		case POW_DETECT_ENCHANTMENT: 
			strcpy(p, " detect enchantment"); break;
		case POW_DETECTION: 
			strcpy(p, " detect all"); break;
		case POW_PERCEPTION: 
			strcpy(p, " ident"); break;
		case POW_PROBEING: 
			strcpy(p, " probe"); break;
		case POW_CLARIVOYANCE: 
			strcpy(p, " wiz light"); break;
		case POW_IDENTIFY_III:
			strcpy(p, " full ident"); break;
		case POW_SPEAR_OF_LIGHT: 
			strcpy(p, " light spear"); break;
		case POW_ETHERIC_JUMP: 
			strcpy(p, " teleport"); break;
		case POW_DEFENSIVE_ARRAY: 
			strcpy(p, " defense"); break;
		case POW_NEUTRALIZE_POISON: 
			strcpy(p, " cure poison"); break;
		case POW_GUNS:
			sprintf(p, " damage %dd20", (4 + (gearhead / 3))); break;
		case POW_HEALING_VII: 
			sprintf(p, " heal %dd10+%d", 6 + gearhead, gearhead); break;
		case POW_TURN_STONE_TO_MUD: 
			strcpy(p, " stone to mud"); break;
		case POW_EARTHQUAKE_II: 
			strcpy(p, " earthquake"); break;
		case POW_MISSILE:
			sprintf(p, " damage %dd%d", 140, 4); break;
		case POW_EMP: 
			strcpy(p, " EM blast"); break;			
		case POW_LEAD_SLUGS:
			sprintf(p, " damage %dd%d", 3 + (gearhead / 4), 8); break;
		case POW_LIGHTNING_RAY:
			sprintf(p, " damage %dd%d", 3 + gearhead, 12); break;
		case POW_FROST_RAY:
			sprintf(p, " damage %dd%d", 5 + (gearhead / 5), 10); break;
		case POW_HEAT_RAY:
			sprintf(p, " damage %dd%d", 8 + gearhead, 12); break;
		case POW_GRAVITY_RAY:
			sprintf(p, " damage %dd%d", 10 + gearhead, 10); break;
		case POW_TELEPORT_OTHER_II: 
			strcpy(p, " teleport other"); break;
		case POW_BLINK: 
			strcpy(p, " portal"); break;
		case POW_TELEPORT_SELF: 
			strcpy(p, " teleport"); break;
		case POW_TELEPORT_OTHER_III: 
			strcpy(p, " teleport other"); break;
		case POW_TELEPORT_LEVEL: 
			strcpy(p, " teleport level"); break;
		case POW_WORD_OF_RECALL_III: 
			strcpy(p, " recall"); break;
		case POW_HEALING_VIII: 
			sprintf(p, " heal %dd10+%d", 8+ gearhead, gearhead); break;
		case POW_BIOLOGICAL_ENHANCE: 
			strcpy(p, " shielding"); break;
		case POW_POLYMORPH_OTHER: 
			strcpy(p, " poly other"); break;
		case POW_RECHARGE_IV: 
			strcpy(p, " recharging"); break;
		case POW_DOOR_CREATION: 
			strcpy(p, " create doors"); break;
		case POW_STAIR_CREATION: 
			strcpy(p, " create stairs"); break;
		case POW_BIOLOGICAL_ENHANCE_II: 
			strcpy(p, " bio enhance"); break;
		case POW_HEALING_IX: 
			strcpy(p, " heal 2000"); break;
		case POW_ALTER_REALITY: 
			strcpy(p, " alter reality"); break;

	}
}

/*
 * Print out a list of available spells for any spellbook given.
 * Revised by -LM-
 *
 * Input y controls lines from top for list, and input x controls columns 
 * from left. 
 *
 */
void print_spells(int book, int y, int x)
{
	int i, left_justi;
	int j = 0;
	int handicap;
	int mana;
	int protect, backlash;
	
	magic_type *s_ptr;

	byte attr_book, attr_name;

	char comment1[20];
	char comment2[20];
	char info[80];
	char out_val[160];

	object_kind *k_ptr;
	cptr basenm;
	
	int	known = 0;
	
	attr_name = TERM_WHITE;
	
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			/* Max of 40 */
			if (p_ptr->skills[SK_OCCULT].skill_rank > 0)
			{
				known += p_ptr->skills[SK_OCCULT].skill_rank;
			}
			/* Max of 60 */
			if (p_ptr->skills[SK_ADV_OCCULT].skill_rank > 0)
			{
				known += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
			}
			/* Max of 80 */
			if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank > 0)
			{
				known += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
			}
			break;
		}
		case SBF_PRAYER:
		{
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			if ((p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0) &&
				(p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1))
			{
				known += (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank * 2);
			}
			if (p_ptr->skills[SK_SPIRITUALITY].skill_rank > 0)
			{
				known += p_ptr->skills[SK_SPIRITUALITY].skill_rank;
			}
			if (p_ptr->skills[SK_PRAYER].skill_rank > 0)
			{
				known += p_ptr->skills[SK_PRAYER].skill_rank;
			}
			if (p_ptr->skills[SK_DEVOTION].skill_rank > 0)
			{
				known += p_ptr->skills[SK_DEVOTION].skill_rank;
			}
			break;
		}
		/* each device has a known total of 40 */
		case SBF_DEVICE:
		{
			switch(book)
			{
				case SV_BOOK_DEVICE1:
				{
					
					if (p_ptr->skills[SK_UTILITY_BANDOLIER].skill_max)
					{
						known += p_ptr->skills[SK_UTILITY_BANDOLIER].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE2:
				{
					if (p_ptr->skills[SK_DETECTIVES_KIT].skill_max)
					{
						known += p_ptr->skills[SK_DETECTIVES_KIT].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE3:
				{
					if (p_ptr->skills[SK_CLOCKWORK_CHASSIS].skill_max)
					{
						known += p_ptr->skills[SK_CLOCKWORK_CHASSIS].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE4:
				{
					if (p_ptr->skills[SK_CLOCKWORK_CARBINE].skill_max)
					{
						known += p_ptr->skills[SK_CLOCKWORK_CARBINE].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE5:
				{
					if (p_ptr->skills[SK_VELOCIPEDE].skill_max)
					{
						known += p_ptr->skills[SK_VELOCIPEDE].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE6:
				{
					if (p_ptr->skills[SK_ANALYTIC_ENGINE].skill_max)
					{
						known += p_ptr->skills[SK_ANALYTIC_ENGINE].skill_rank;
					}
					if (p_ptr->skills[SK_GADGETEER].skill_max)
					{
						known += p_ptr->skills[SK_GADGETEER].skill_rank;
					}
					break;
				}
				case SV_BOOK_DEVICE7:
				{
					break;
				}
				case SV_BOOK_DEVICE8:
				{
					break;
				}
				case SV_BOOK_DEVICE9:
				{
					break;
				}
				case SV_BOOK_DEVICE10:
				{
					break;
				}
				default:
				{
					break;
				}
			}
		}	
		default:
		{
			break;
		}
	}


	k_ptr = &k_info[lookup_kind(TV_MAGIC_BOOK, book)];
	basenm = k_name + k_ptr->name;

	/* Choose appropriate spellbook color. */
	attr_book = k_ptr->d_attr;

	/* Choose a left margin for the spellbook name. */
	left_justi = ((80 - x) - strlen(basenm)) / 2;

	/* Center the spellbook name */
	prt("", y, x);
	c_put_str(attr_book, format("%s", basenm), y, x + left_justi);

	/* Title the list */
	prt("", y + 1, x);
	put_str("Name", y + 1, x + 5);
	put_str("Lv Mana Fail Info", y + 1, x + 45);

	/* Calculate handicap */
	handicap = cp_ptr->spell_handicap[book]-1;

	/* Dump the spells in the book. */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Get the spell */
		s_ptr = &books[book].contents[i];

		if (s_ptr->index == 0) continue;

		/* Increment the current line */
		j++;

		/* Skip illegible spells. */
		/* 80 is the maximum skill total that one can have */
		if ((s_ptr->slevel + handicap) > MAX_SPELL_SKILL)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + j + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, s_ptr->index);

		/* Use that info */
		strcpy (comment1, "");
		strcpy (comment2, info);

		mana = spell_mana(book, i);

		switch (books[book].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				protect = p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank + p_ptr->skills[SK_ADV_THAUMIC_ENERGY].skill_rank;
				if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) protect += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;
				if (protect > 0) protect += (protect / 2);
	
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;

				/* Vivid color for known, safe spells */
				if (backlash < 4) attr_name = attr_book;
				
				/* Danger color for mild backlash */
				else if (backlash < 9) attr_name = TERM_YELLOW;
		
				/* Alarm color for heavy backlash */
				else attr_name = TERM_VIOLET;
				break;
			}
			case SBF_PRAYER:
			{
				protect = p_ptr->skills[SK_LESSER_WARD].skill_rank + p_ptr->skills[SK_GREATER_WARD].skill_rank;
				if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) protect += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;
				if (protect > 0) protect += protect;
	
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;		

				/* Vivid color for known, safe spells */
				if (backlash < 6) attr_name = attr_book;
				
				/* Danger color for mild backlash */
				else if (backlash < 11) attr_name = TERM_YELLOW;
		
				/* Alarm color for heavy backlash */
				else attr_name = TERM_VIOLET;
				break;
			}		
			case SBF_DEVICE:
			{
				attr_name = attr_book;
				break;
			}
		}

		if ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1)) > known)
		{
			strcpy (comment1, "");
			strcpy (comment2, " too high");
			attr_name = TERM_L_DARK;
		}

		/* Clear line */
		prt("", y + j + 1, x);

		/* Print out (colored) information about a single spell. */
		put_str(format("  %c) ", I2A(i)), y + j + 1, x);
		c_put_str(attr_name, format("%-40s", s_ptr->sname), 
			y + j + 1, x + 5);
		if (mana > 0) 
			c_put_str(attr_name, format("%2d %4d %3d%%", (s_ptr->slevel+handicap), 
			mana, spell_chance(book, i)), y + j + 1, x + 45);
			
		c_put_str(attr_name, format("%s%s", comment1, comment2), y + j + 1, x + 57);
	}

	/* Clear the bottom line */
	prt("", y + j + 2, x);
}

/* Count how many spells in spell book */
byte count_spells(int book)
{
	int j, count;
	magic_type *s_ptr;

	count = 0;

	for (j=0; j < MAX_BOOK_SPELLS; j++)
	{
		s_ptr = &books[book].contents[j];
		if (s_ptr->index > 0) count++;
	}

	return count;
}

/*
 * Get a spell out of a book/instrument
 */
static int get_spell(int *sn, cptr prompt, int book, bool allow_all)
{
	int i;

	int spell = -1;

	bool flag, redraw, okay;
	char choice;

	char out_val[160];

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (spell_okay(book, *sn)) 
		{
			/* Success */
			return (TRUE);
		}
	}

	if (!allow_all)
	{
		okay = FALSE;

		/* Assume no spells available */
		(*sn) = -2;

		/* Check for "okay" spells */
		for (i = 0; i < MAX_BOOK_SPELLS; i++)
		{
			/* Look for "okay" spells */
			if (spell_okay(book, i)) okay = TRUE;
		}

		/* No "okay" spells */
		if (!okay) return (FALSE);
	}

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) %^s which effect? ",
		I2A(0), I2A(count_spells(book)-1), prompt);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		okay = TRUE;

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();

				/* Hide list */
				redraw = FALSE;
			}

			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;

				/* Save screen */
				screen_save();

				/* Display a list of spells */
				print_spells(book, 1, 2);
			}

			/* Ask again */
			continue;
		}

		/* Lowercase */
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= MAX_BOOK_SPELLS))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		spell = i;

		/* Require "okay" spells */
		if (!allow_all && !spell_okay(book, spell))
		{
			okay = FALSE;			
		}
		else if (allow_all)
		{
			/* Even if all spells are allowed, you need to account for illegible spells */
			magic_type *s_ptr = &books[book].contents[spell];

			/* Spell is illegal */
			/* This code is screwy - don't know what it's doing -ccc */
			/* if ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1)) > PY_MAX_LEVEL) okay = FALSE; */
		}

		if (!okay)
		{
			bell("Illegal spell choice!");
			message_format(MSG_GENERIC, 0, "You may not %s that spell.", prompt);
			continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;
	
	repeat_push(*sn);

	/* Success */
	return (TRUE);
}

/*
 *  Actually browse a spellbook
 */
static void do_browse_book(int book)
{
	int spell, lines, j;
	bool redraw = FALSE;

	magic_type *s_ptr;

	/* Display the spells */
	print_spells(book, 1, 2);

	/* 
	 * Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_spells(book);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell,  "browse", book, TRUE))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No spells to browse     ", 0, 0);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Maybe we need to redraw */
		if (redraw)
		{
			/* Redraw everyting */
			screen_load();

			/* Remember the screen again */
			screen_save();

			print_spells(book, 1, 2);

			redraw = FALSE;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 3, 255);

		/* Access the spell */
		s_ptr = &books[book].contents[spell];

		for (j = (POW_MAX - 1); j > 0; j--) 
		{
			if (power_info[j].index == s_ptr->index) break;
		}

		/* Output to the screen */
		text_out_hook = text_out_to_screen;
		
		/* Display that spell's information. */
		if (power_info[j].desc != NULL)
			c_put_str(TERM_L_BLUE, format("%^s.",power_info[j].desc), lines + 3, 8);

	}
}

/*
 * Hook to specify "book"
 */
static bool item_tester_hook_book(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_MAGIC_BOOK:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

/*
 * Peruse the spells/prayers in a Book/Instrument
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 *
 * Some code taken from Oangband 0.4.1 
 *
 */
void do_cmd_browse(void)
{
	int item;

	object_type *o_ptr;

	/* hmmm. . . */
	item_tester_hook = item_tester_hook_book;

	/* Get an item */

	/* Can read books, can't use instruments */
	/* need something that checks the spellbook flag, or a reworking for devices */
	if (!get_item(&item, "Examine which? ", "You have nothing that you can examine.", 
	(USE_INVEN | USE_EQUIP))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	
	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();
	
	do_browse_book(o_ptr->sval);

	/* Load screen */
	screen_load();

	/* Hack -- Process "Escape" */
	if (p_ptr->command_new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->command_new = 0;
	}
}

/* 
 * This function is called from do_cast, and pre-
 * tabulates all the information that do_power needs,
 * and then calls it. If this function fails for any reason
 * do_cast is canceled.
 */
static bool aux_spell_cast(int book, int index)
{
	/* add, and pass the skill values */
	int magepower = p_ptr->skills[SK_RITUAL_MAGIC].skill_rank;
	int willpower = 1;
	int gearhead = 1;
	int mageprot, prayprot;
	
	/* Various class flags influence things */
	/* Should probably not make beam based off level */
	int	beam = ((cp_ptr->flags & CF_BEAM) ? ((p_ptr->lev - 10) * 2) : (p_ptr->lev - 10));
	bool ignore_me;
	
	/* Mage power goes from 1-40, willpower from 1-60 */
	if (p_ptr->skills[SK_THELMA].skill_max > 0)
	{
		magepower += p_ptr->skills[SK_THELMA].skill_rank;
	}
	if (p_ptr->skills[SK_TMPR_WILL].skill_max > 0)
	{
		willpower = p_ptr->skills[SK_TMPR_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_HARD_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_HARD_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_IRON_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_IRON_WILL].skill_rank;
	}
	if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
	{
		gearhead = p_ptr->skills[SK_GADGETEER].skill_rank;
	}
	
	mageprot = p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank + p_ptr->skills[SK_ADV_THAUMIC_ENERGY].skill_rank;
	if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) mageprot += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;
	if (mageprot < 0) mageprot = 0;
	if (mageprot > 0) mageprot += (mageprot/2);
	prayprot = p_ptr->skills[SK_LESSER_WARD].skill_rank + p_ptr->skills[SK_GREATER_WARD].skill_rank;
	if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) prayprot += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;
	if (prayprot < 0) prayprot = 0;
	if (prayprot > 0) prayprot += prayprot;
	/* A spell was cast */
	return do_power(book, index, 0, beam, &ignore_me, magepower, willpower, gearhead, mageprot, prayprot);
}

/*
 * Cast a spell or pray a prayer.
 */
static void do_cast(int book)
{
	int spell;
	int chance;
	int mana;
	int num, y, x, i;
	int fastcast = 0;
	int peril_sorc = 0;
		
	
	magic_type *s_ptr;
	num = 1 + randint(2);
	y = p_ptr->py;
	x = p_ptr->px;

	if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_max > 0)
	{
		if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 1)
		{
			peril_sorc = p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank;
		}
	}
	/* Ask for a spell */
	if (!get_spell(&spell,"cast", book, FALSE))
	{
		if (spell == -2) 
		{
			message(MSG_GENERIC, 0, "You don't know anything about that!");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &books[book].contents[spell];


	mana = spell_mana(book, spell);

	/* Verify "dangerous" spells */
	if (mana > p_ptr->csp)
	{
		/* Warning */
		message(MSG_GENERIC, 0, "You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell_chance(book, spell);

	if (p_ptr->no_magic)
	{
		switch (books[book].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				message(MSG_GENERIC, 0, "Your equipment prevents you!");
				return;
			}
			case SBF_PRAYER:
			{
				message(MSG_GENERIC, 0, "Your equipment prevents you!");
				return;
			}
			default:
			{
				/* nothing */
			}
		}
	}		
		
	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		switch (books[book].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				int backlash, protect, horror;

				message(MSG_GENERIC, 0, "You fail to correctly incant the spell!");

				/* HACK!! - If the player lacks these skills, protect becomes a negative value */
				/* If they haven't put any points into the first skill, the total is still neg */
				/* this means that not knowing anything about energy hurts your saftey */
				protect = p_ptr->skills[SK_THAUMIC_ENERGY].skill_rank + p_ptr->skills[SK_ADV_THAUMIC_ENERGY].skill_rank;
				if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) protect += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;
			

				/* increse protect  by 50%, can't mutiply for screwing negative values */
				if (protect > 0) protect += (protect / 2);

				/* The most backlash can be is around 90, the least is around, or under 0 */
				/* for the most powerful spells, backlash is equal to a positive value */
				/* for a spell with a diff of 80, backlash is 20 ( and the spell can't be */
				/* used unless all of your skill's are maxed). */
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;
				
				/* How badly did I screw up the spell? */
				horror = randint(backlash) + peril_sorc;
				
				switch(randint(backlash) + peril_sorc/2)
				{
					case 1:
					case 2:
					case 3:
					{
						/* Safe */
						break;
					}
					case 4: case 5: case 6:
					{
						msg_print("Magical Thaumic energy goes wild!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy", TRUE);
						break;
					}
					case 7:  
					{
						/* Bypasses sustain! */
						msg_print("You feel power drain from your muscles!");
						dec_stat(A_MUS, horror, FALSE);
						break;
					}
					case 8:
					{
						/* Bypasses sustain! */
						msg_print("You feel strength drain from your body!");
						dec_stat(A_VIG, horror, FALSE);
						break;
					}
					case 9:
					{
						/* Bypasses sustain! */
						msg_print("You feel slow and clumsy!");
						dec_stat(A_AGI, horror, FALSE);
						(void)set_slow(p_ptr->slow + rand_int(horror) + horror);
						break;
					}
					case 10: 
					{
						/* Bypasses sustain! */
						msg_print("Your mind is assailed!");
						dec_stat(A_SCH, horror, FALSE);
						dec_stat(A_EGO, horror, FALSE);
						(void)set_image(p_ptr->image + rand_int(horror) + horror);					
						break;
					}
					case 11:
					{
						msg_print("The dark forces take their due!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy", TRUE);
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);
						break;	
					}
					case 12:
					{
						msg_print("The dark forces take their due!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy", TRUE);
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);	
						(void)set_cut(p_ptr->cut + rand_int(horror) + horror);
						break;
					}
					case 13:
					{

						msg_print("The dark forces take their due!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy", TRUE);
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);	
						(void)set_cut(p_ptr->cut + rand_int(horror) + horror);
						(void)set_stun(p_ptr->stun + rand_int(horror) + horror);	
					}
					case 14:
					{
						/* Bypasses sustain! */
						msg_print("You body is wracked by pain!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy", TRUE);
						dec_stat(A_MUS, horror * 3, FALSE);
						dec_stat(A_VIG, horror * 3, FALSE);
						dec_stat(A_AGI, horror * 3, FALSE);
						break;
					}
					case 15:
					{
						/* Bypasses sustain! */
						msg_print("They are trying to drive you mad!");
						dec_stat(A_SCH, horror * 3, FALSE);
						dec_stat(A_EGO, horror * 3, FALSE);
						dec_stat(A_CHR, horror * 3, FALSE);
						(void)set_confused(p_ptr->confused + rand_int(horror) + horror);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						break;	
					}
					case 16:
					{
						/* Bypasses sustain! */
						msg_print("The fabric of reality tears!");
						dec_stat(A_EGO, horror * 3, FALSE);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						for (i = 0; i < num; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
						break;	
					}
					case 17:
					{
						/* Bypasses sustain! */
						msg_print("Oh God! They're coming!");
						dec_stat(A_EGO, horror * 3, FALSE);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						for (i = 0; i < num; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
						break;	
					}
					case 18:
					{
						/* Bypasses sustain! */
						msg_print("If you knew what I know, y-you'd kill yourself!");
						dec_stat(A_SCH, horror * 5, FALSE);
						dec_stat(A_EGO, horror * 5, FALSE);
						dec_stat(A_CHR, horror * 5, FALSE);
						(void)set_confused(p_ptr->confused + rand_int(horror) + horror);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						break;	
					}
					case 19:
					{
						/* Bypasses sustain! */
						msg_print("OH GOD! THE PAIN!!! AAAAAARRRGGGGGULKKK-");
						take_hit(damroll(horror, (horror * 2)), "thaumic energy", TRUE);
						dec_stat(A_MUS, horror * 5, FALSE);
						dec_stat(A_VIG, horror * 5, FALSE);
						dec_stat(A_AGI, horror * 5, FALSE);
						(void)set_confused(p_ptr->confused + rand_int(horror) + horror);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						break;	
					}
					case 20:
					{
						/* Bypasses sustain! */
						msg_print("A dark infinte universe. . crushing eternity-THE END IS NEIGH!");
						dec_stat(A_EGO, horror * 3, FALSE);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
						break;	
					}
					default:
					{
						msg_print("i have seen our doom. THERE IS NO ESCAPE!!!");
						take_hit(damroll(horror, (horror * 3)), "thaumic energy", TRUE);
						dec_stat(A_MUS, horror * 5, FALSE);
						dec_stat(A_VIG, horror * 5, FALSE);
						dec_stat(A_AGI, horror * 5, FALSE);
						dec_stat(A_SCH, horror * 5, FALSE);
						dec_stat(A_EGO, horror * 5, FALSE);
						dec_stat(A_CHR, horror * 5, FALSE);
						(void)set_confused(p_ptr->confused + rand_int(horror) + horror);
						(void)set_afraid(p_ptr->afraid + rand_int(horror) + horror);
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_HI_DEMON, FALSE);
						}
						break;	
					}
				}
				break;
			}
			case SBF_PRAYER:
			{
				int backlash, protect, horror;

				message(MSG_GENERIC, 0, "Your prayer goes unanswered!");

				/* HACK!! - If the player lacks these skills, protect becomes a negative value */
				/* If they haven't put any points into the first skill, the total is still neg */
				/* this means that not knowing anything about energy hurts your saftey */
				protect = p_ptr->skills[SK_LESSER_WARD].skill_rank + p_ptr->skills[SK_GREATER_WARD].skill_rank;
				if (p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank > 2) protect += p_ptr->skills[SK_PERILOUS_SORCERY].skill_rank / 3;

				/* doubles protect, can't mutiply for screwing negative values */
				if (protect > 0) protect += protect;

				/* Prayers can be totally safe */
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;
				
				/* How badly did I screw up the spell? */
				horror = randint(backlash) + peril_sorc;
				
				switch(randint(backlash) + (peril_sorc/2))
				{
					case 1: case 2: case 3: case 4: case 5:
					{
						/* prayers are safer than spells */
						break;
					}
					case 6: case 7: case 8: case 9: case 10:
					{
						msg_print("Your prayer has attracted the wrong kind of attention!");
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
						break;
					}
					case 11: case 12: case 13: case 14: 
					{
						msg_print("Dark and nefarious forces intercept your plea!");
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
						break;
					}
					case 15: case 16: case 17: case 18: 
					{
						msg_print("Your chants have disturbed elder-beasts!");
						break;
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE);
						}
					}
					case 19: case 20: case 21: case 22:
					{
						msg_print("The desperateness of your plea draws a great evil towards you!");
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_HI_DEMON, FALSE);
						}
						break;
					}
					default:
					{
						msg_print("Your unpenitent plea has angered God!");
						take_hit(damroll(horror, (horror * 3)), "divine will", TRUE);
						lose_exp(200 + (p_ptr->exp/25));
						break;
					}
					
				}
				break;
			}
			case SBF_DEVICE:
			{
				message(MSG_GENERIC, 0, "Your fail to correctly manipulate the device!");
				break;
			}
			/* good! */
			case SBF_XXX1:
			{
				message(MSG_GENERIC, 0, 
					"Your mind is overwhelmed by the magnitude of ancient mystery!");
				/* Lose your spell-casting stats */
				/* Lose your memories */
				lose_all_info();
				break;
			}
			case SBF_XXX2:
			{
				message(MSG_GENERIC, 0, 
					"You lost your grasp on the evil powers that you had sought to control!");
				/* Summon some horrors */
				/* summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth+10, SUMMON_HORROR); */
				/* Darkness */
				if (!p_ptr->blind)
				{
					set_blind(p_ptr->blind + 3 + randint(5));
				}
				unlite_area(10, 3);
				/* Lose EXP */
				if (p_ptr->hold_life && (rand_int(100) < (100-chance)))
				{
					message(MSG_GENERIC, 0, "You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					message(MSG_GENERIC, 0, "You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/250));
				}
				else
				{
					message(MSG_GENERIC, 0, "You feel your life draining away!");
					lose_exp(200 + (p_ptr->exp/25));
				}				
				break;
			}
		}
	}

	/* Process spell */
	else
	{
		/* Allow cancelling directional spells */
		if (!aux_spell_cast(book, spell)) return;
	}

	/* Take a turn */
	if (p_ptr->skills[SK_FAST_CAST].skill_rank > 0)
	{
		fastcast = p_ptr->skills[SK_FAST_CAST].skill_rank;
	}

	p_ptr->energy_use = 100 - fastcast;
	
	/* Sufficient mana */
	if (mana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= mana;
	}

	/* Over-exert the player */
	else
	{
		int oops = mana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		message(MSG_GENERIC, 0, "You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			message(MSG_GENERIC, 0, "You have damaged your health!");

			/* Reduce constitution */
			dec_stat(A_VIG, 50 + randint(80), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Cast a spell or pray a prayer.
 */
void do_cmd_magic(void)
{
	int item;

	cptr q, s;

	/* Can use instruments, can't use books */
	int flg;

	object_type *o_ptr;
	
	/* o_ptr is first set to weilded item to check if it's a spellbook */
	/* If it is a spellbook, it automatically displays the open equipment window */
	o_ptr = &inventory[INVEN_WIELD];
	
	
	if (p_ptr->confused)
	{
		message(MSG_GENERIC, 0, "You are too confused!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		message(MSG_GENERIC, 0, "You cannot see!");
		return;
	}
	
	/* Get an item */
	item_tester_hook = item_tester_hook_book;

	if (o_ptr->tval == TV_MAGIC_BOOK && spell_book_select) 
	{
		/* p_ptr->command_see = TRUE; */
		p_ptr->command_wrk = (USE_EQUIP);
	}
	else p_ptr->command_wrk = (USE_INVEN);	

	/* Can read books, can't use instruments */
	q = "Use which? ";
	s = "You have nothing that you can use.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return;


	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
	

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);
	
	/* Hack -- Handle stuff */
	handle_stuff();

	do_cast(o_ptr->sval);
}


/*
 * Actually use a power
 *
 * beam is the base chance of beaming. It is calculated based off
 * player level in aux_spell_cast. Perhaps this should be based off
 * something other than player level? XCCCX
 *
 * obvious determines if the effect of the spell can be seen by the player.
 *
 * Mage power is the total of your ritual magic skill ranks, and your themla. 
 * Note that it requires a _lot_ of skill points to even get access to thelma.
 * for non mages assume mage power is never > 20, though it has a range of 1-40
 * values above 20 for magepower should really be supercharged
 *
 * willpower is the force of your will. It ranges from 1-60. It
 * increases the power/damage/range of your spells.
 *
 * NOTE: spells have negative effects. Some are elminatable - 
 * some are not. Welcome to the wonderful world of dark mystic energy.
 * It is generally possible to eliminate damage from a spell (due
 * to the fact that magiclash/praylash can be 0), BUT it is _not_
 * possible to eliminate stat drain, being that randmlash, and randplash
 * are both randint functions and always return at least 1.
 *
 * Priest spells are in general safer, have fewer damageing effects, and 
 * rarely if at all cause damage or statloss. 
 *
 * Devices do not use willpower. 
 */
bool do_power(int book, int idx, int dir, int beam, bool *obvious, int magepower, int willpower, int gearhead, int mageprot, int prayprot)
{
	int k, ty, tx, b;
	int backlash;
	int magiclash, praylash;
	int randmlash, randplash;
	int py, px;
	magic_type *s_ptr;
	s_ptr = &books[book].contents[idx];
	backlash = s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1);
	
	py = p_ptr->py;
	px = p_ptr->px;
	
	magiclash = backlash - mageprot;
	if (magiclash < 0) magiclash = 0;
	
	praylash = backlash - prayprot;
	if (praylash < 0) praylash = 0;
	
	randmlash = randint(magiclash);
	randplash = randint(praylash);
	
	/* We haven't seen anything yet */
	*obvious = FALSE;

	switch (s_ptr->index)
	{
		/* Liber AL vel Legis (sval 0) */
		/* Aim of the Will */
		case POW_AIM_OF_THE_WILL:
		{
			int typ;
			if (magepower > 10) typ = GF_ICE;
			else typ = GF_CHILL;
			if (!get_aim_dir(&dir)) return (FALSE);
			/* Max damage 13d16, 208 */
			fire_bolt(typ, dir, damroll(3 + ((magepower) / 4), (4 + (willpower / 5))));
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Sense Will */
		case POW_SENSE_WILL:
		{
			(void)detect_life(FALSE);
			break;
		}
		/* Astral Gate */
		case POW_ASTRAL_GATE:
		{
			teleport_player(10 + willpower / 2);
			dec_stat(A_VIG, randmlash, FALSE);
			break;
		}
		/* Inner Radiance */
		case POW_INNER_RAD:
		{
			(void)lite_area(damroll(2, (magepower)), (magepower / 8) + 1);
			(void)set_tim_light((magepower * ((willpower / 10) + 1)));
			break;
		}
		/* Demonic Will */
		case POW_DEMONIC_WILL:
		{
			/* Max duration 60*5 = 300 */
			(void)set_tim_demonspell(willpower * ((magepower / 10) + 1));
			(void)set_cut(p_ptr->cut + randmlash);
			(void)heal_player_sp(100, 0);
			dec_stat(A_VIG, randmlash + 10, FALSE);
			dec_stat(A_MUS, randmlash + 10, FALSE);
			break;
		}
		/* Object/Metal Detection */
		case POW_LO_OBJECT:
		{
			(void)detect_treasure(FALSE);
			(void)detect_objects_gold(FALSE);
			(void)detect_objects_normal(FALSE);
			/* dec_stat(A_SCH, randmlash, FALSE); */
			break;
		}
		/* Safe Passage of the Will */
		case POW_LO_TRAPS:
		{
			(void)detect_traps(FALSE);
			(void)detect_doors(FALSE);
			(void)detect_stairs(FALSE);
			/* dec_stat(A_EGO, randmlash, FALSE); */
			break;
		}
		/* Plaguing the Will */
		case POW_PLAGUE_WILL:
		{
			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of poison */
			if (magepower < 30) x_list[k].type = GF_POISON;
			else x_list[k].type = GF_CONTAGION;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 20;

			/* Does damage, has a large radius, */
			/* Max damage 14d9 */
			x_list[k].power =  damroll(4 + (magepower / 4), 6 + (willpower / 20));
			/* max radius 7 */
			x_list[k].power2 = 1 + (willpower / 10);

			/* And lasts for about 21 max attacks */
			x_list[k].lifespan = (1 + (magepower / 2));

			dec_stat(A_VIG, randmlash, FALSE);
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Supression of the Will */
		case POW_SUPPRESSION_WILL:
		{
			(void)sleep_monsters_touch();
			break;
		}
		/* Knowledge of Secular Objects */
		case POW_IDENTIFY:
		{
			(void)ident_spell();
			/* dec_stat(A_VIG, randmlash, FALSE); */
			break;
		}
		/* Transfering Will to Objects */
		case POW_RECHARGE:
		{
			if (!recharge(magepower)) return(FALSE);
			dec_stat(A_VIG, randmlash + 30, FALSE);
			break;
		}
		/* Spectacle of Ice */
		case POW_FROST_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* Max damage 6+13d6+7, or 19d13 max 247 */
			fire_beam(GF_ICE, dir, damroll(6+(magepower/3), 6 + willpower / 8));
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Spectacle of Fire */
		case POW_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* Max damage 6+13d6+7, or 19d13 max 247 */
			fire_arc(GF_FIRE, dir, damroll(6+(magepower/3), 6 + willpower / 8), 5, 30);
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Focus of the Will */
		case POW_HASTE:
		{
			if (!p_ptr->fast) (void)set_fast(randint(20) + magepower + willpower);
			else (void)set_fast(p_ptr->fast + randint(5));
			dec_stat(A_SCH, randmlash + 10, FALSE);
			dec_stat(A_EGO, randmlash + 10, FALSE);
			break;
		}
		/* Movement of the Will of Others */
		case POW_TELEPORT_OTHER_I:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)teleport_monster(dir);
			dec_stat(A_VIG, randmlash + 20, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			dec_stat(A_MUS, randmlash, FALSE);
			break;
		}
		/* Sphere of Flame */
		case POW_FIREBALL:
		{
			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_SPHERE;

			/* Of poison */
			if (magepower < 30) x_list[k].type = GF_FIRE;
			else x_list[k].type = GF_PLASMA;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 5;

			/* Does damage, has a large radius, */
			/* Max damage of 5 + 20 + 30d4 or 55d4 Max dam 440 */
			x_list[k].power =  damroll(5 + ((magepower + willpower) / 2), 4);
			/* Radius of 1, 2, or 3 */
			x_list[k].power2 = 1 + (willpower / 30);

			/* And lasts for about 3 attacks */
			x_list[k].lifespan = 3;

			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		
		/* Solitude of the Will */
		case POW_GENOCIDE:
		{
			(void)genocide();
			dec_stat(A_MUS, randmlash, FALSE);
			dec_stat(A_VIG, randmlash + 50, FALSE);
			dec_stat(A_AGI, randmlash, FALSE);
			dec_stat(A_SCH, randmlash, FALSE);
			dec_stat(A_EGO, randmlash + 30, FALSE);
			dec_stat(A_CHR, randmlash, FALSE);
			break;
		}
		
		/* Spectacle of acid */
		case POW_ACID_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* Max damage is 4 + 40 + 60 / 3d12 or 34d12 max damage 408 */
			fire_bolt_or_beam(beam, GF_ACID, dir,
								damroll(4+((magepower + willpower) /3), 12));
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}

		/* Spectacle of Lightning */
		case POW_LIGHTNING_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* Max damage is 106/2d8, or 53d8 max damage 424 */
			fire_beam(GF_ELEC, dir, (damroll(6+((magepower + willpower) /2), 8)));
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Purge the Toxins of the Will */
		case POW_CLOUD_KILL:
		{
			bool cloud_exists = FALSE;

			/* Try to find an existing cloud of death */
			for (k = 0; k < z_info->x_max; k++)
			{
				/* Require a cloud of death */
				if (x_list[k].index == EFFECT_DEATH_CLOUD)
				{
					msg_print("You purge more filth from your soul.");
					cloud_exists = TRUE;
					break;
				}
			}

			/* Get a new effect index if necessary */
			if (!cloud_exists)
			{
				k = effect_prep();

				/* Note failure XXX */
				if (k < 0) break;

				/* Message */
				msg_print("You purge your soul of toxins.");
			}

			/* We want a cloud, */
			x_list[k].index = EFFECT_DEATH_CLOUD;

			/* Of death */
			x_list[k].type = GF_POISON;

			/* That starts at the character location. */
			x_list[k].y0 = py;
			x_list[k].x0 = px;

			/* Moves every game turn, */
			x_list[k].time_delay = 2;

			/* Does damage, affects a fairly large area */
			/* 12d3 */
			x_list[k].power =  damroll(2 + (magepower / 4), 1 + (willpower / 30));
			/* 1-5 rad */
			x_list[k].power2 = 1 + magepower / 10;

			/* And lasts for a certain period of time. */
			x_list[k].lifespan = (magepower);

			/* Allow clouds to be renewed */
			if (cloud_exists)
			{
				/* Renew cloud, but never allow age to go negative */
				x_list[k].age -= MIN(x_list[k].age, (magepower / 4));
			}
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		
		/* Storm of Ice */
		case POW_ICE_STORM:
		{
			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;

			 for (b = 0; b < (willpower / 5); b++)
			 {
				 /* Get a new effect index */
				 k = effect_prep();

				 /* Note failure XXX */
				 if (k < 0) break;

				 /* We want a spirit, */
				 x_list[k].index = EFFECT_SEEKER_VORTEX;

				 /* Of fire */
				 x_list[k].type = GF_ICE;

				/* Use the given direction */
				ty = py + ddy[dir];
				tx = px + ddx[dir];
				
				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
				}
				 /* That starts at the character location. */
				 x_list[k].y0 = ty;
				 x_list[k].x0 = tx;

				 /* Moves with a speed that depends on the wind, */
				 x_list[k].time_delay = 3;

				 /* Does damage, 1 + 9 + 6 / 2 d 3 or 8d3 */
				 x_list[k].power = damroll(1 + (((magepower / 5) + (willpower / 10)) / 2), 3);

				 /* And lasts for a certain period of time. */
				 x_list[k].lifespan = magepower;
			}

			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of ICE */
			x_list[k].type = GF_GLACIAL;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 5;

			/* Does damage, has a large radius, */
			/* 13d11 == 143 dam */
			x_list[k].power = damroll(magepower / 3, ((willpower / 6) + 1));
			x_list[k].power2 = 10;

			/* And lasts for about 10 attacks */
			x_list[k].lifespan = (magepower * 2) / 3;
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Storm of Fire */
		case POW_FIRE_STORM:
		{
			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;
			for (b = 0; b < (willpower / 8); b++)
			{
				/* Get a new effect index */
				k = effect_prep();
	
				/* Note failure XXX */
				if (k < 0) break;
	
				/* Use the given direction */
				ty = py + ddy[dir];
				tx = px + ddx[dir];
				
				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
				}
	
				/* We want an lingering cloud, */
				x_list[k].index = EFFECT_SPHERE;
	
				/* Of poison */
				if (magepower < 30) x_list[k].type = GF_FIRE;
				else x_list[k].type = GF_PLASMA;
	
				/* That starts at the monster location. */
				x_list[k].y0 = x_list[k].y1 = ty + rand_range(-2, 2);
				x_list[k].x0 = x_list[k].x1 = tx + rand_range(-2, 2);
	
				/* It attacks every 8 -> 5 game turns, */
				x_list[k].time_delay = 10;
	
				/* Does damage, has a large radius, */
				/* 10 + 20 + 30 d 4 60d4, 240 */
				x_list[k].power =  damroll(10 + (magepower / 2 + willpower / 2), 4);
				/* rad 3 */
				x_list[k].power2 = 1 + (willpower / 30);
	
				/* And lasts for about 3 attacks */
				x_list[k].lifespan = 3;
			}
						k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of ICE */
			x_list[k].type = GF_FIRE;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 5;

			/* Does damage, has a large radius, */
			x_list[k].power = damroll(3, 4);
			x_list[k].power2 = 10;

			/* And lasts for about 10 attacks */
			x_list[k].lifespan = 6;

			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		case POW_METEOR_STORM:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* 25 + (40 + 60)/3 d 2 58d2, 116 max, 10 max meteor's, radius of 1-2 */
			fire_barrage(GF_EARTH, dir, 25 + ((magepower + willpower)/ 3), 2, 
			           (magepower + willpower) / 10, 2, (magepower + willpower) / 40);
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Sphere of the Elements */
		case POW_ELEMENTAL_BALL:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_FIRE, dir,
						damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_ICE, dir,
						damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_ELEC, dir,
						damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_ACID, dir,
						damroll(33 + ((magepower + willpower)/ 2), 2), 2);          
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Soulstorm */
		case POW_SOUL_STORM:
		{
			 for (b = 0; b < (magepower / 2); b++)
			 {
				 /* Get a new effect index */
				 k = effect_prep();

				 /* Note failure XXX */
				 if (k < 0) break;

				 /* We want a spirit, */
				 x_list[k].index = EFFECT_SEEKER_VORTEX;

				 /* Of fire */
				 x_list[k].type = GF_SPIRIT;

				 /* That starts at the character location. */
				 x_list[k].y0 = p_ptr->py + ddy_cdd[(b) % 8];
				 x_list[k].x0 = p_ptr->px + ddx_cdd[(b) % 8];

				 /* Moves with a speed that depends on the wind, */
				 x_list[k].time_delay = 2;

				 /* Does damage, 60-120 + 41d13 */
				 x_list[k].power = rand_range(willpower, willpower * 2) + 
						damroll(1 + (magepower / 2), magepower / 3);

				 /* And lasts for a certain period of time. */
				 x_list[k].lifespan = magepower / 2;
			}
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}	
		/* Path of the Worm */	
		case POW_WORD_OF_RECALL_I:
		{
			set_recall();
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			break;
		}
		/* Power of the Worm */
		case POW_RECHARGE_II:
		{
			if(!recharge(40)) return(FALSE);
			dec_stat(A_VIG, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_SCH, randmlash * 2, FALSE);
			dec_stat(A_EGO, randmlash * 2, FALSE);

			break;
		}
		/* Force of the Worm */
		case POW_EARTHQUAKE_I:
		{
			earthquake(py, px, 10);
			dec_stat(A_CHR, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_VIG, randmlash * 2, FALSE);
			dec_stat(A_AGI, randmlash * 2, FALSE);

			break;
			
		}
		/* Mind of the Worm */
		case POW_MIND_OF_WORM:
		{
			/* Increase senses */
			(void)set_tim_wormsense(willpower * ((magepower / 10) + 1));
			dec_stat(A_SCH, randmlash + 5, FALSE);
			dec_stat(A_EGO, randmlash + 5, FALSE);
			dec_stat(A_CHR, randmlash * 2 + 30, FALSE);
			dec_stat(A_VIG, randmlash, FALSE);
			break;
		}
		/* Death of the Worm */
		case POW_MASS_GENOCIDE:
		{
			(void)mass_genocide();
			dec_stat(A_SCH, randmlash * 2, FALSE);
			dec_stat(A_EGO, randmlash * 2, FALSE);
			dec_stat(A_CHR, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_AGI, randmlash * 2, FALSE);
			dec_stat(A_VIG, randmlash * 2 + 40, FALSE);
			break;
		}
		/* Hellspawn */
		case POW_SUMMON_DEMON:
		{
			summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_LO_DEMON, TRUE);
			dec_stat(A_AGI, randmlash + 20, FALSE);
			break;
		}
		/* Voorish Sign */
		case POW_VOORISH_SIGN:
		{
			(void)set_tim_voorish(willpower * ((magepower / 10) + 1));
			(void)heal_player_sp(100, 0);
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_MUS, randmlash, FALSE);
			break;
		}
		/* Winds of Byakhee */
		case POW_BYAKHEE_WINDS:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			/* Damage 6d13 */ 
			fire_arc(GF_GALE, dir, damroll(2+(magepower/10), 6 + willpower / 8), 10, 35);
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Barier of Chaugnar Faugn */
		case POW_FAUGN_BARRIER:
		{
			int ty, tx;
			for (b = 0; b < 3; b++)
			{
				/* Get a new effect index */
				k = effect_prep();

				/* Note failure XXX */
				if (k < 0) break;

				/* Get a direction */
				if (!get_aim_dir(&dir)) return (FALSE);

				/* Use the given direction */
				ty = py + 20 * ddy[dir];
				tx = px + 20 * ddx[dir];

				/* Hack -- Use an actual "target" */
				if ((dir == 5) && target_okay())
				{
					ty = p_ptr->target_row;
					tx = p_ptr->target_col;
				}

				/* We want an advancing wall, */
				x_list[k].index = EFFECT_WALL;

				/* Of fire */
				x_list[k].type = GF_FEAR;

				/* That starts at the character location. */
				x_list[k].y0 = py;
				x_list[k].x0 = px;

				/* It advances one grid every two or three game turns, */
				x_list[k].time_delay = b + 2;

				/* Heads for the target grid, */
				x_list[k].y1 = ty;
				x_list[k].x1 = tx;

				/* Does damage, */
				x_list[k].power = 30;

				/* And lasts for a certain period of time. */
				x_list[k].lifespan = willpower / 2;
			}
			break;
		}
		/* Expatriate Demon */
		case POW_BANISH_DEMON:
		{
			if (!get_hack_dir(&dir)) return(FALSE);
			fire_ball(GF_DISP_DEMON, dir, 10000, 0);
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Hell Lord */
		case POW_SUMMON_DEMON_II:
		{
			summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth, SUMMON_HI_DEMON, TRUE);
			dec_stat(A_AGI, randmlash + 50, FALSE);
			dec_stat(A_MUS, randmlash + 10, FALSE);
			dec_stat(A_VIG, randmlash + 10, FALSE);
			break;
		}	
		/* Lamp of Alhazred */
		case POW_LAMP_ALHAZRED:
		{
			(void)set_image(p_ptr->image + rand_int(6));
			dec_stat(A_CHR, randmlash * 2 + 30, FALSE);
			dec_stat(A_EGO, randmlash * 2 + 10, FALSE);
			dec_stat(A_SCH, randmlash * 2, FALSE);
			wiz_lite();
			break;
		}
		/* Contact Yithian */
		case POW_CONTACT_YITHIAN:	
		{
			detect_objects_magic(TRUE);
			dec_stat(A_EGO, randmlash, FALSE);
			break;
		}
		/* Incantation of Mi-go */
		case POW_INCANT_MI_GO:
		{
			for (b = 0; b < 9; b++)
			{
				int y1, x1;
				y1 = p_ptr->py + ddy_cdd[(b)];
				x1 = p_ptr->px + ddx_cdd[(b)];
							
				/* Cast a beam */
				project_beam(-1, 2, p_ptr->py, p_ptr->px, y1, x1, 40 + willpower, GF_DRAIN, 0L);
			}
			take_hit(magiclash, "Mystic energy", FALSE);
			break;
		}
		/* Counsume Flesh */
		case POW_CONSUME_FLESH:
		{
			/* no safety */
			take_hit(willpower * 2, "Consumed flesh", FALSE);
			sp_player(willpower * 2, NULL);
			break;
		}
		/* Melt Stone */
		case POW_MELT_STONE:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			break;
		}
		/* Chime of Tezchaptl */
		case POW_CHIME_TEZCH:
		{
			for (b = 0; b < 4; b++)
			{
				/* Get a new effect index */
				k = effect_prep();

				/* Note failure XXX */
				if (k < 0) break;

				/* Use the given direction */
				ty = py + ddy_cdd[(b * 2) % 8];
				tx = px + ddx_cdd[(b * 2) % 8];

				/* We want an advancing wall, */
				x_list[k].index = EFFECT_WALL;

				/* Of fire */
				x_list[k].type = GF_TK;

				/* That starts at the character location. */
				x_list[k].y0 = 	py;
				x_list[k].x0 =  px;

				/* It advances one grid every two or three game turns, */
				x_list[k].time_delay = 0;

				/* Heads for the target grid, */
				x_list[k].y1 = ty;
				x_list[k].x1 = tx;

				/* Does damage, */
				x_list[k].power = willpower / 2;

				/* And lasts for a certain period of time. */
				x_list[k].lifespan = 2;
			}
			dec_stat(A_MUS, randmlash * 2 + 5, FALSE);
			break;
		}
		/* Mirror of Tarkhun Atep */
		case POW_MIRROR_ATEP:
		{
			if (p_ptr->tim_invisiblity) set_tim_invisiblity(p_ptr->tim_invisiblity + 1);
			else set_tim_invisiblity(p_ptr->tim_invisiblity + willpower / 6);
			dec_stat(A_EGO, randmlash * 2 + 30, FALSE);
			break;
		}
		/* Gate */
		case POW_GATE:
		{
			msg_print("Choose a location to teleport to.");
			message_flush();
			dimen_door(willpower + magepower, magiclash);
			dec_stat(A_VIG, randmlash * 2 + 10, FALSE);
			break;
		}
		/* Effigy of Hate */
		case POW_EFFIGY_HATE:
		{
			/* This doesn't work right, find something that works. */
			project_ball(-1, 2, py, px, py, px, 20, GF_FEAR, 0L, 10 + 2 * 10);
			project_ball(-1, 1, py, px, py, px, 30, GF_FEAR, 0L, 10 + 1 * 10);
			project_ball(-1, 5, py, px, py, px, 100, GF_KILL_WALL, 0L, 10 + 5 * 10);
			project_ball(-1, 4, py, px, py, px, magepower + willpower, GF_FORCE, 0L, 10 + 4 * 10);
			dec_stat(A_SCH, randmlash * 2 + 40, FALSE);
			dec_stat(A_EGO, randmlash * 2 + 40, FALSE);
			break;
		}
		/* *identify* */
		case POW_CONTACT_NYAR:
		{
			if (!identify_fully()) return(FALSE);
			dec_stat(A_EGO, randmlash * 2 + 60, FALSE);
			dec_stat(A_AGI, randmlash * 2 + 60, FALSE);
			break;
		}
		case POW_DEMON_COURAGE:
		{
			if (!p_ptr->hero) (void)set_hero(willpower * ((magepower / 10) + 1));
			else (void)set_hero(p_ptr->hero + 1);
			break;
		}
		case POW_DEMON_FURY:
		{
			if (!p_ptr->shero) (void)set_shero(willpower * ((magepower / 10) + 1));
			else (void)set_shero(p_ptr->shero + 1);
			dec_stat(A_SCH, randmlash + 10, FALSE);
		}
		case POW_DEMONIC_VIGOR:
		{
			if (!p_ptr->tim_demonhealth) (void)set_tim_demonhealth(willpower * ((magepower / 10) + 1));
			else (void)set_tim_demonhealth(p_ptr->tim_demonhealth + 1);
			(void)heal_player(100, 0);
			dec_stat(A_EGO, randmlash + 30, FALSE);
			dec_stat(A_SCH, randmlash + 30, FALSE);
			break;
		}
		case POW_DEMON_SHIELD:
		{
			if (!p_ptr->shield) (void)set_shield(willpower * ((magepower / 10) + 1));
			else (void)set_shield(p_ptr->shield + 1);
			dec_stat(A_VIG, randmlash + 40, FALSE);
			dec_stat(A_MUS, randmlash + 40, FALSE);
			break;
		}
		case POW_STYGIAN_WAR:
		{
			(void)set_tim_stygian(willpower * ((magepower / 10) + 1));
			(void)heal_player(100, 0);
			dec_stat(A_EGO, randmlash + 50, FALSE);
			dec_stat(A_SCH, randmlash + 50, FALSE);
			break;
		}
		case POW_CHANT:
		{
			(void)set_blessed(p_ptr->blessed + randint(24) + willpower);
			break;
		}
		case POW_SANCTUARY:
		{
			for (b = 0; b < 4; b++)
			{
				/* Get a new effect index */
				k = effect_prep();

				/* Note failure XXX */
				if (k < 0) break;

				/* Use the given direction */
				ty = py + ddy_cdd[(b * 2) % 8];
				tx = px + ddx_cdd[(b * 2) % 8];

				/* We want an advancing wall, */
				x_list[k].index = EFFECT_STATIC_WALL;

				/* Of fire */
				if (willpower < 20) x_list[k].type = GF_TK;
				else x_list[k].type = GF_FORCE;
				
				/* That starts at the character location. */
				x_list[k].y0 = 	py;
				x_list[k].x0 =  px;

				/* It advances one grid every two or three game turns, */
				x_list[k].time_delay = 5;

				/* Heads for the target grid, */
				x_list[k].y1 = ty;
				x_list[k].x1 = tx;

				/* Does damage, */
				x_list[k].power = 2 + willpower/2;

				/* And lasts for a certain period of time. */
				x_list[k].lifespan = 10 + willpower / 10;
			}
			break;
		}
		case POW_HEALING_I:
		{
			(void)hp_player(damroll(8, 12) + willpower);
			break;
		}
		case POW_PROTECTION_FROM_EVIL:
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * willpower);
			break;
		}
		case POW_HEALING_II:
		{
			(void)hp_player(damroll(8, 10) + willpower);
			(void)wp_player(1);
			(void)set_stun(0);
			break;
		}
		case POW_REMOVE_CURSE:
		{
			msg_print("You feel as if someone is watching over you.");
			remove_curse();
			break;
		}
		case POW_TURN_UNDEAD:
		{
			(void)turn_undead(willpower * 2);
			break;
		}
		case POW_HEALING_III:
		{
			(void)set_cut(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			break;
		}
		case POW_DESTROY_MACHINE:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt(GF_EMP, dir, damroll(1 + willpower / 4, 3));
			break;
		}
		case POW_HOLY_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt(GF_DISP_EVIL, dir, damroll(2 + (willpower / 2), (4 + willpower / 4)));
			break;
		}
		case POW_PORTAL:
		{
			teleport_player(willpower * 3);
			break;
		}
		case POW_SENSE_INVISIBLE:
		{
			(void)set_tim_invis(p_ptr->tim_invis + randint(24) + (willpower * 3));
			break;
		}
		case POW_SENSE_SURROUNDINGS:
		{
			map_area();
			break;
		}
		case POW_SATISFY_HUNGER:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case POW_PRAYER:
		{
			(void)set_hero(p_ptr->hero + randint(48) + willpower);
			break;
		}
		case POW_DISPEL_EVIL:
		{
			(void)dispel_evil(randint(willpower * 3));
			break;
		}
		case POW_IDENTIFY_II:
		{
			if(!ident_spell()) return(FALSE);
			break;
		}
		case POW_HEALING_IV:
		{
			(void)hp_player(rand_range(400, 800));
			(void)wp_player(rand_range(10, 20));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case POW_HOLY_WORD:
		{
			(void)dispel_evil(randint(willpower * 4));
			(void)hp_player(300);
			(void)wp_player(rand_range(2, 4));
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case POW_RESTORATION:
		{
			(void)do_res_stat(A_MUS);
			(void)do_res_stat(A_AGI);
			(void)do_res_stat(A_VIG);
			(void)do_res_stat(A_SCH);
			(void)do_res_stat(A_EGO);
			(void)do_res_stat(A_CHR);
			break;
		}
		case POW_DISPEL_CURSE:
		{
			(void)remove_all_curse();
			break;
		}
		case POW_BANISHMENT:
		{
			if (banish_evil(100 + willpower))
			{
				msg_print("The power of god banishes evil!");
			}
			break;
		}
		case POW_RECHARGE_III:
		{
			if (!recharge(40 + willpower)) return(FALSE);
			break;
		}
		case POW_DISPEL_EVIL_II:
		{
			(void)dispel_evil(willpower * 6);
			break;
		}
		case POW_WORD_OF_RECALL_II:
		{
			set_recall();
			break;
		}
		case POW_RESISTANCE:
		{
			int time = randint(20) + (willpower * 2);
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
			(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
			(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
			(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
			(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
			(void)set_tim_res(RS_TIM, p_ptr->tim_res[RS_TIM] + time);
			(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
			(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
			(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);
			break;
		}
		case POW_HEAL_CUTS:
		{
			(void)set_cut(0);
			break;
		}
		case POW_REMOVE_FEAR_II:
		{
			(void)set_afraid(0);
			break;
		}
		case POW_MINOR_RESISTANCE:
		{
			int time = randint(20) + (willpower * 2);
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
			break;
		}
		case POW_MUSCLE_BUFF:
		{
			(void)set_tim_muscle(p_ptr->tim_muscle + randint(48) + willpower);
			break;
		}
		case POW_SHATTER:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
				fire_arc(GF_SONIC, dir, damroll(4, 6 + willpower/2), 10, 35);
			break;
		}
		case POW_NO_TELEPORT:
		{
			(void)set_tim_no_tele(p_ptr->tim_no_tele + randint(200) + willpower*6);
			break;
		}
		case POW_VIGOR_BUFF:
		{
			(void)set_tim_vigor(p_ptr->tim_vigor + randint(48) + willpower);
			break;
		}
		case POW_STONE_MELD:
		{
			(void)set_shadow(p_ptr->tim_wraith + randint(6) + willpower/3);
			break;
		}
		case POW_FREE_ACT:
		{
			(void)set_tim_free_act(p_ptr->tim_free_act + randint(128) + (willpower * 2));
			break;
		}
		case POW_DIVINE_FAVOR:
		{
			(void)set_shero(p_ptr->shero + randint(20) + (willpower / 6));
			break;
		}
		case POW_FLAMING_WRATH:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_orb(GF_FIRE, dir, damroll(20 + (willpower / 3), 5), 3);
			break;
		}
		case POW_ANTI_MAGIC:
		{
			(void)set_tim_anti_magic(p_ptr->tim_anti_magic + randint(12));
			break;
		}
		case POW_HOLY_STRIKE:
		{
			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_SPHERE;

			/* Of poison */
			x_list[k].type = GF_HOLY_FIRE;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 5;

			/* Does damage, has a large radius, */
			/* Max damage of 60d8 Max dam 480 * 3 = 1440  */
			x_list[k].power =  damroll((willpower), 8);
			/* Radius of 0, 1, or 2 */
			x_list[k].power2 = 0 + (willpower / 30);

			/* And lasts for about 3 attacks */
			x_list[k].lifespan = 3;
			break;
		}
		case POW_DANCING_LIGHTS:
		{
			for (b = 0; b < ((willpower / 5) + 1); b++)
			{
				/* Get a new effect index */
				k = effect_prep();

				/* Note failure XXX */
				if (k < 0) break;

				/* We want a spirit, */
				x_list[k].index = EFFECT_SEEKER_VORTEX;

				/* Of fire */
				x_list[k].type = GF_BRILLIANCE;

				/* That starts at the character location. */
				x_list[k].y0 = py;
				x_list[k].x0 = px;

				/* Moves with a speed that depends on the wind, */
				x_list[k].time_delay = 1;

				/* Does damage, 1 + 9 + 6 / 2 d 3 or 8d3 */
				x_list[k].power = damroll((6 + (willpower / 5)), 3);

				/* And lasts for a certain period of time. */
				x_list[k].lifespan = willpower + 10;
			}
			break;
		}
		case POW_SACRED_SERMON:
		{
			charm_monsters(willpower);
			break;
		}
		case POW_MANIFEST_GOD:
		{
			manifest_god();
			break;
		}
		case POW_THOUGHT_SENSE:
		{
			(void)detect_life(TRUE);
			(void)set_tim_esp(p_ptr->tim_esp + (willpower * 3));
			break;
		}
		case POW_KEY:
		{
			(void)do_res_stat(A_MUS);
			(void)do_res_stat(A_AGI);
			(void)do_res_stat(A_VIG);
			(void)do_res_stat(A_SCH);
			(void)do_res_stat(A_EGO);
			(void)do_res_stat(A_CHR);
			(void)hp_player(2000);
			(void)wp_player(rand_range(100, 200));
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			
			msg_print("The world falls away and reforms around you!");
			p_ptr->leaving = TRUE;
			break;
		}
		/* Utility Bandolier */
		case POW_CALL_LIGHT:
		{
			(void)lite_area(damroll(2, (gearhead / 2)), (gearhead /10) +1);
			break;
		}
		case POW_REMOVE_FEAR:
		{
			(void)set_afraid(0);
			break;
		}
		case POW_SPRING_BLADE:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
								damroll(3 + (gearhead / 6), 3));
			break;
		}
		case POW_HEALING_VI:
		{
			(void)hp_player(damroll(2 + gearhead ,10) + gearhead);
			(void)set_cut(p_ptr->cut - 10);
			break;
		}
		case POW_DETECT_DOOR_STAIR:
		{
			(void)detect_doors(FALSE);
			(void)detect_stairs(FALSE);
			break;
		}
		case POW_DETECT_TRAPS:
		{
			(void)detect_traps(FALSE);
			break;
		}
		case POW_SLOW_POISON:
		{
			(void)set_poisoned(p_ptr->poisoned / 2);
			break;
		}
		case POW_NOURISHMENT:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case POW_OBJECT_ANALYSIS:
		{
			if (!ident_spell()) return(FALSE);
			break;
		}
		case POW_FETCH:
		{
			if (!get_hack_dir(&dir)) return (FALSE);
			fetch(dir, gearhead * 20, TRUE);
			break;
		}
		/* Detectives Kit */
		case POW_DETECT_HOSTILE:
		{
			(void)detect_monsters_normal(FALSE);
			break;
		}
			
		case POW_DETECT_TRAPS_DOORS:
		{
			(void)detect_traps(FALSE);
			(void)detect_doors(FALSE);
			(void)detect_stairs(FALSE);
			break;
		}
		
		case POW_TREASURE_DETECTION:
		{
			(void)detect_treasure(FALSE);
			(void)detect_objects_gold(FALSE);
			break;
		}
		
		case POW_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic(FALSE);
			break;
		}
		
		case POW_DETECTION:
		{
			(void)detect_all();
			break;
		}

		case POW_PERCEPTION:
		{
			if (!ident_spell()) return(FALSE);
			break;
		}

		case POW_PROBEING:
		{
			if (!probing()) return(FALSE);
			break;
		}

		case POW_CLARIVOYANCE:
		{
			(void)wiz_lite();
			break;
		}
		case POW_IDENTIFY_III:
		{
			if (!identify_fully()) return(FALSE);
			break;
		}
		/* Clockwork Chassis */
		case POW_SPEAR_OF_LIGHT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			msg_print("A line of blue shimmering light appears.");
			lite_line(dir);
			break;
		}
		case POW_ETHERIC_JUMP:
		{
			/* hack */
			teleport_player((gearhead * 6) + 3);
			break;
		}
		case POW_DEFENSIVE_ARRAY:
		{
			(void)set_afraid(0);
			(void)set_blessed(p_ptr->blessed + randint(24) +24);
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + randint(10) + 10);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + randint(10) + 10);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + randint(10) + 10);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + randint(10) + 10);
			(void)set_hero(p_ptr->hero + randint(25) + 25);
			break;
		}
		case POW_NEUTRALIZE_POISON:
		{
			(void)set_poisoned(0);
			break;
		}
		case POW_GUNS:
		{
		if (!get_aim_dir(&dir)) return(FALSE);
			fire_blast(GF_BULLET, dir, (4 + (gearhead / 3)), 
								20, 1 + gearhead / 5, 2, FALSE);
			break;
		}
		case POW_HEALING_VII:
		{
			(void)hp_player(damroll(6 + gearhead, 10) + gearhead);
			(void)wp_player(rand_range(1, 3));
			(void)set_cut(0);
			break;
		}
		case POW_TURN_STONE_TO_MUD:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)wall_to_mud(dir);
			break;
		}
		case POW_EARTHQUAKE_II:
		{
			earthquake(py, px, 10);
			break;
		}
		case POW_MISSILE:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_MISSILE, dir,
							damroll(140, 4), 2);
			break;
		}
		case POW_EMP:
		{
			project_ball(-1, 4, py, px, py, px, 300, GF_EMP, 0L, 0);
			break;
		}
		case POW_LEAD_SLUGS:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_blast(GF_BULLET, dir, (3 + (gearhead / 4)), 
								8, rand_range(1, 1 + gearhead / 5), 2, FALSE);
			break;
		}
		case POW_LIGHTNING_RAY:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_beam(GF_ELEC, dir, damroll(3 + gearhead, 12));
			break;
		}
		case POW_FROST_RAY:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_blast(GF_ICE, dir, (5 + (gearhead / 5)), 10, 
						rand_range(1, 1 + (gearhead / 5)), 1, TRUE);
			break;
		}
		case POW_HEAT_RAY:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_arc(GF_FIRE, dir,
								damroll((8+gearhead), 12), 3 + gearhead / 4, 30);
			break;
		}
		case POW_GRAVITY_RAY:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_beam(GF_GRAVITY, dir,
								damroll(10+(gearhead), 10));
			break;
		}
		case POW_TELEPORT_OTHER_II:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)teleport_monster(dir);
			break;
		
		}
		/* Velocipede */
		case POW_BLINK:
		{
			teleport_player(10);
			break;
		}

		case POW_TELEPORT_SELF:
		{
			teleport_player(gearhead * 12);
			break;
		}

		case POW_TELEPORT_OTHER_III:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)teleport_monster(dir);
			break;
		}

		case POW_TELEPORT_LEVEL:
		{
			(void)teleport_player_level(TRUE);
			break;
		}

		case POW_WORD_OF_RECALL_III:
		{
			set_recall();
			break;
		}
		/* The Analytic Engine */
		case POW_HEALING_VIII:
		{
			(void)hp_player(damroll(8 + gearhead, 10) + gearhead);
			(void)wp_player(rand_range(8, 12));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
	 
		case POW_BIOLOGICAL_ENHANCE:
		{
			int time = randint(20) + 20 + gearhead;
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
			(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
			(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
			(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
			(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
			(void)set_shield(p_ptr->shield + randint(20) + 30);
			break;
		}
		case POW_POLYMORPH_OTHER:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)poly_monster(dir);
			break;
		}
		case POW_RECHARGE_IV:
		{
			if(!recharge(80)) return(FALSE);
			break;
		}
		case POW_DOOR_CREATION:
		{
			(void)door_creation();
			break;
		}
		case POW_STAIR_CREATION:
		{
			(void)stair_creation();
			break;
		}
		case POW_BIOLOGICAL_ENHANCE_II:
		{
			int time = randint(30) + 30 + gearhead;
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
			(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
			(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
			(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
			(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
			(void)set_shield(p_ptr->shield + time);
			(void)hp_player(30);
			(void)set_shero(p_ptr->shero + time);
			(void)set_afraid(0);
			if (!p_ptr->fast) (void)set_fast(time);
			else (void)set_fast(p_ptr->fast + randint(10));
			break;
		}
		case POW_HEALING_IX:
		{
			(void)hp_player(2000);
			(void)wp_player(200);
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case POW_ALTER_REALITY:
		{
			msg_print("The world changes!");

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}		
	}
	return (TRUE);
}


info_entry power_info[POW_MAX] = 
{
	{0, NULL},
	/* Liber AL vel Legis (sval 0) */
	{POW_AIM_OF_THE_WILL,	"uses your will to directly chill monsters (HP damage)"},
	{POW_SENSE_WILL,		"detects the will of nearby creatures"},
	{POW_ASTRAL_GATE,		"transports your physical self a short distance (Vigor damage)"},
	{POW_INNER_RAD,			"lights rooms with the radiance of your soul"},
	{POW_DEMONIC_WILL,		"Raises your spellcasting skill (Vigor, Muscle damage, cuts)"},	
	{POW_LO_OBJECT,			"detects objects nearby"},	
	{POW_LO_TRAPS,			"detects nearby traps, doors, and stairs"},	
	{POW_PLAGUE_WILL,		"attempts to poison another's soul (HP, Vigor damage)"},	
	/* The Zohar (Book of Splendor) (sval 1) */
	{POW_RECHARGE, 			"transmutes will into energy for devices (Vigor damage)"},
	{POW_SUPPRESSION_WILL, 	"puts a monster you touch to sleep"},
	{POW_IDENTIFY, 			"determines information about an object"},
	{POW_FIRE_BOLT, 		"creates a arc of flame to burn your enemies (HP damage)"},
	{POW_FROST_BOLT, 		"creates a beam of frost to freeze your enemies (HP damage)"},
	{POW_TELEPORT_OTHER_I, 	"removes a monster from your presence (Vigor, Muscle, and Ego damage)"},
	{POW_HASTE, 			"temporarly increases your speed (Schooling and Ego damage)"},
	{POW_FIREBALL, 			"creates a huge sphere of flame (HP damage)"},
	{POW_GENOCIDE, 			"removes a certain type of creature from a level (HP + All stat damage)"},
	/* Sepher Yetzirah (Book of Formation) (sval 2) */
	{POW_ACID_BOLT, 		"creates a bolt of acid to corrode your enemies (HP damage)"},
	{POW_LIGHTNING_BOLT, 	"creates a bolt of lightning to shock your enemies (HP damage)"},
	{POW_CLOUD_KILL, 		"creates a cloud of poison to damage your enemies (HP damage)"},
	{POW_ICE_STORM, 		"creates a storm of ice to damage your enemies (HP damage)"},
	{POW_FIRE_STORM, 		"creates a storm of fire to damage your enemies (HP damage)"},
	{POW_METEOR_STORM, 		"fires a barrage of meteors to damage your enemies (HP damage)"},
	{POW_ELEMENTAL_BALL, 	"fires a ball of the 4 elements at your enemies (HP damage)"},
	{POW_SOUL_STORM, 		"creates a storm of the dead! (HP damage)"},
	/* De Vermis Mysteriis (Mysteries of the Worm) (sval 3) */
	{POW_RECHARGE_II, 		"transmutes will into energy for devices (Vigor, Muscle, Schooling, & Ego damage)"},
	{POW_EARTHQUAKE_I, 		"a great worm to shake the earth! (Charm, Muscle, Vigor, & Agility damage)"},
	{POW_WORD_OF_RECALL_I, 	"transports you quickly back to the surface (Vigor and Ego damage)"},
	{POW_MIND_OF_WORM, 		"gain the powerful worm's mighty senses! (Schooling, Ego, Charm, and Vigor damage)"},
	{POW_MASS_GENOCIDE, 	"removes all creatures from your presence! (HP + All stat damage)"},
	/* Cthaat Aquadingen (sval 4) */
	{POW_SUMMON_DEMON, 	"summons a minor demon to serve you! (Agility damage)"},
	{POW_VOORISH_SIGN, "increases the power of your spells (Vigor, Muscle damage)"},
	{POW_BYAKHEE_WINDS, "fires a cone of air (HP damage)"},
	{POW_FAUGN_BARRIER, "sends out a wave of fear (HP damage)"},
	{POW_BANISH_DEMON, "banishes a single demon (HP damage"},
	{POW_SUMMON_DEMON_II, 	"summons a major demon servant! (Agility, Muscle, & Vigor damage)"},
	{POW_LAMP_ALHAZRED, "enlightens you. (Charm, Ego & Schooling damage"},	
	/* Othuum Omnicia  (sval 5) */
	{POW_CONTACT_YITHIAN, "detects magic items (Ego damage)"},
	{POW_INCANT_MI_GO, "creates a windmill of drain (HP damage)" },
	{POW_CONSUME_FLESH, "use your flesh to refresh your spellpoints (Consumes HP)"},
	{POW_MELT_STONE, "eliminates walls"},
	{POW_CHIME_TEZCH, "surrounding protective wall forces monsters back (Muscle Damage)"},
	{POW_MIRROR_ATEP, "turns you invisible (Ego damage)"},
	{POW_GATE, "instant controlled transport (Vigor damage)"},
	{POW_EFFIGY_HATE, "rage of hell (Schooling & Ego damage)"},
	{POW_CONTACT_NYAR, "gain all information about an item (Ego & Agility Damage)"},
	/* Revelations of Glaaki (sval 6) */
	{POW_DEMON_COURAGE, "gives you demonic courage"},
	{POW_DEMON_FURY, 	"allows you to fight with demonic fury (Schooling damage)"},
	{POW_DEMONIC_VIGOR, "gives you the toughness of a demon (Schooling and Ego damage)"},
	{POW_DEMON_SHIELD, 	"shields you with infernal power (Vigor and Muscle damage)"},
	{POW_STYGIAN_WAR, 	"gives you demonic combat ability (Schooling and Ego damage)"},
	/* Book name (sval 7) */
	/* Book name (sval 8) */
	/* Book name (sval 9) */
	/* Treatise on the Resurrection (sval 10) */
	{POW_CHANT, 			"the power of god blesses you! (+ to ac, and to hit)"},
	{POW_SANCTUARY, 		"surrounds you with a protective wall"},
	{POW_HEALING_I, 		"heals a small amount of damage"},
	{POW_PROTECTION_FROM_EVIL, "spiritual power will protect you from evil creatures"},
	{POW_HEALING_II, 		"heals some damage and wounds"},
	{POW_REMOVE_CURSE, 		"removes most curses from your equipment"},
	{POW_TURN_UNDEAD, 		"causes nearby undead to flee"},
	{POW_HEALING_III, 		"cures poison + cuts"},
	{POW_DESTROY_MACHINE, 	"blasts machines"},
	{POW_HOLY_BOLT, "strikes at the heart of evil"},
	/* Odes of Solomon (sval 11) */
	{POW_PORTAL, 			"moves you a short distance"},
	{POW_SENSE_INVISIBLE,   "allows you to detect invisable creatures for a short time"},
	{POW_SENSE_SURROUNDINGS, "detects the dungeon around you"},
	{POW_SATISFY_HUNGER, 	"fills your stomach"},
	{POW_PRAYER, 			"calls upon the power of god to protect you (+ to hit)"},
	{POW_DISPEL_EVIL, 		"attempts to destory evil creatures in your sight"},
	{POW_IDENTIFY_II, 		"provides knowledge about an item"},
	{POW_HOLY_WORD, 		"heals you a great deal, and dispels evil creatures"},
	{POW_HEALING_IV, 		"greatly heals your hit points and wounds"},
	{POW_RESTORATION, 		"causes all your stats to be restored"},
	/* The Pnakotic Manuscripts (sval 12) */
	{POW_DISPEL_CURSE, 		"removes almost all curses"},
	{POW_BANISHMENT, 		"banishes evil from your sight"},
	{POW_RECHARGE_III, 		"recharges objects"},
	{POW_DISPEL_EVIL_II,	"dispells a great amount of evil"},
	{POW_WORD_OF_RECALL_II, "draws you back to the surface"},
	{POW_RESISTANCE, 		"protects you from the basic elements"},
	/* Khorda Avesta (sval 13) */
	{POW_HEAL_CUTS,			"heals you of all cuts"},
	{POW_MINOR_RESISTANCE,	"gives you low resistances"},
	{POW_REMOVE_FEAR_II,	"removes fear"},
	{POW_MUSCLE_BUFF,		"increases your muscle temporarily"},
	{POW_SHATTER,			"blasts out a cone of vibration"},
	{POW_NO_TELEPORT,		"temporarily prevents teleportation"},
	{POW_VIGOR_BUFF,		"increases your vigor temporarily"},
	{POW_STONE_MELD,		"allows you to meld with stone for a short time"},
	/* Trimorphic Protennoia (Nag Hammadi) (sval 14) */
	{POW_FREE_ACT, 			"gives you temporary free action"},
	{POW_DIVINE_FAVOR, 		"gives your fighting ability the favor of your god"},
	{POW_FLAMING_WRATH, 	"calls down a column of fire"},
	{POW_ANTI_MAGIC, 		"prevents the casting of magical spells"},
	{POW_HOLY_STRIKE,  "Calls down holy fire to deal damage to enemies"},
	/* The Corpus Hermeticum (sval 15) */
	{POW_DANCING_LIGHTS, 	"surrounds you with dancing lights"},
	{POW_SACRED_SERMON,		"attempts to convert all monsters around you"},
	{POW_MANIFEST_GOD, 		"all are whisked away from you"},
	{POW_THOUGHT_SENSE, 	"detect life and the minds of creatures around you"},
	{POW_KEY, 				"heals you and changes the world around you."},
	/* Book name (sval 16) */
	/* Book name (sval 17) */
	/* Utility knife (sval 18) */
	{POW_CALL_LIGHT, 		"lights a room"},
	{POW_REMOVE_FEAR,		"uses harmonics to prevent fear"},
	{POW_SPRING_BLADE, 		"shoots a small projectile towards enemies"},
	{POW_HEALING_VI, 		"heals a very small amount"},
	{POW_DETECT_DOOR_STAIR, "detects doors and stairs"},
	{POW_DETECT_TRAPS, 		"detects traps"},
	{POW_SLOW_POISON, 		"removes some poison damage"},
	{POW_NOURISHMENT, 		"provides a small pill satisfying your body's needs"},
	{POW_OBJECT_ANALYSIS, 	"analyzes objects and tells you their properties"},
	{POW_FETCH, 			"shoots out a small grappling hook to pull items towards you"},
	/* Detective's Kit  (sval 19) */
	{POW_DETECT_HOSTILE, 	"detects all hostile creatures"},
	{POW_DETECT_TRAPS_DOORS, "detects traps, doors, and stairs"},
	{POW_TREASURE_DETECTION, "detects gold"},
	{POW_DETECT_ENCHANTMENT, "detects magical objects"},
	{POW_DETECTION, 		"detects all"},
	{POW_PERCEPTION, 		"provides information about an item"},
	{POW_PROBEING, 			"returns information about a monster"},
	{POW_CLARIVOYANCE, 		"gives information about your surroundings"},
	{POW_IDENTIFY_III, 		"fully describes all features of an object"},
	/* Clockwork Chassis (sval 20)*/
	{POW_SPEAR_OF_LIGHT, 	"shoots a weak laser beam"},
	{POW_ETHERIC_JUMP, 		"moves you a short distance through space"},
	{POW_DEFENSIVE_ARRAY, 	"increases to hit, armor class, removes fear, resists fire and cold"},
	{POW_NEUTRALIZE_POISON, "eliminates poison damage"},
	{POW_GUNS, 				"fires cannons"},
	{POW_HEALING_VII, 		"heals some HP, and cures all cuts"},
	{POW_TURN_STONE_TO_MUD, "turns a wall to mud, damages machines"},
	{POW_EARTHQUAKE_II, 	"devastates the surrounding area"},
	{POW_MISSILE, 			"fires an explosive missile"},
	{POW_EMP, 				"emits an electromagnetic pulse"},
	/* Clockwork carbine (sval 21) */
	{POW_LEAD_SLUGS, 		"activates vulcan cannons"},
	{POW_LIGHTNING_RAY, 	"emits a lightning ray"},
	{POW_FROST_RAY, 		"shoots out beams of freezing cold"},
	{POW_HEAT_RAY, 			"creates a blistering ray of heat"},
	{POW_GRAVITY_RAY, 		"causes a linear gravity distortion"},
	{POW_TELEPORT_OTHER_II, "fires an etheric ray that transports others away"},
	/* Velocipede (sval 22) */
	{POW_BLINK, 			"transports self a short distance through the ether"},
	{POW_TELEPORT_SELF, 	"transports self a long distance through the ether"},
	{POW_TELEPORT_OTHER_III, "transports others away through the ether"},
	{POW_TELEPORT_LEVEL, 	 "changes your locality"},
	{POW_WORD_OF_RECALL_III, "returns you back to town"},
	/* The Analytic Engine (sval 23) */
	{POW_HEALING_VIII, 		"fixes moderate damage to your body. Heals stunning and cuts"},
	{POW_BIOLOGICAL_ENHANCE, "opposes all elements, and raises AC"},
	{POW_POLYMORPH_OTHER, "transforms your opponents into different monsters"},
	{POW_RECHARGE_IV, 		"recharges power cells"},
	{POW_DOOR_CREATION, 	"creates doors"},
	{POW_STAIR_CREATION, 	"creates stairs"},
	{POW_BIOLOGICAL_ENHANCE_II, "as biological enhance, but also + to hit, and speed"},
	{POW_HEALING_IX, 		"powerful full healing"},
	{POW_ALTER_REALITY, 	"recreates reality"},
	/* Book name (sval 24) */
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
	{0, NULL},
};


