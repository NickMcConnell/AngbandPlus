/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* All the code below is going to be rewritten for the new spell system. -CCC */


/* Using Ey functions and code */
/* Do not need literate() function because all classes can use books. */
/* Do not need spellcaster() because all classes can cast spells */
/* Without skills they will suck at them anyway. */


/*
 * Returns mana cost for a spell 
 */
/* Mana cost should not be affected by skills */
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
 */
static s16b spell_chance(int book, int spell)
{
	int chance, minfail, mana;
	int handicap;
	int spell_stat1, spell_stat2, magelore, praylore, gear;

	byte stat_factor;
	magic_type *s_ptr;
	int known;
	
	known = 0;
	magelore = 1;
	praylore = 1;
	gear = 1;
	
	if (p_ptr->skills[SK_OCCULT].skill_max > 0)
	{
		magelore += p_ptr->skills[SK_OCCULT].skill_rank;
	}
	if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
	{
		magelore += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
	}
	if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
	{
		magelore += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
	}
	if (p_ptr->skills[SK_SPIRITUALITY].skill_max > 0)
	{
		praylore += p_ptr->skills[SK_SPIRITUALITY].skill_rank;
	}
	if (p_ptr->skills[SK_PRAYER].skill_max > 0)
	{
		praylore += p_ptr->skills[SK_PRAYER].skill_rank;
	}
	if (p_ptr->skills[SK_DEVOTION].skill_max > 0)
	{
		praylore += p_ptr->skills[SK_DEVOTION].skill_rank;
	}
	if (p_ptr->skills[SK_GADGETEER].skill_max > 0)
	{
		gear += p_ptr->skills[SK_GADGETEER].skill_rank;
	}
	
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
			/* Max of 40 */
			if (p_ptr->skills[SK_OCCULT].skill_max > 0)
			{
				known += p_ptr->skills[SK_OCCULT].skill_rank;
			}
			/* Max of 60 */
			if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
			{
				known += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
			}
			/* Max of 80 */
			if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
			{
				known += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
			}
			break;
		}
		case SBF_PRAYER:
		{
			known += p_ptr->skills[SK_LATIN].skill_rank;
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
	/* This needs to be replaced by a skill for plev */
	chance -= 3 * (known - (s_ptr->slevel + handicap));
	
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
	chance -= 3 * (stat_factor / 50);

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
			minfail = 25 - (stat_factor / 50) - magelore;
			break;
		}
		case SBF_PRAYER:
		{
			minfail = 25 - (stat_factor / 50) - praylore;
			break;
		}
		case SBF_DEVICE:
		{
			minfail = 25 - (stat_factor / 50) - gear;
			break;
		}
		default:
		{
			minfail = 25;
			break;
		}
	}
	if (minfail < 0) minfail = 0;

	/* Non mage/priest/mystic characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	/* removed */
	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Spell disruption (after minfail)*/
	/* removed */	
	/* Gloves (after minfail)*/
	/* removed */
	/* Heavy armor */
	/* removed */
	
	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	if (p_ptr->confused)	
	{
		/* Confusion makes spells harder */
		chance += 20;
	}

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}


/*
 * Determine if a spell is "okay" for the player to cast.
 * What determines if a spell is "okay" is simply their skill levels
 * and whether or not they have access to the book or device.
 * Known is an interger value to determine weither or not 
 * We possess enough skill and knowledge to cast the spell
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
			
			/* Max of 40 */
			if (p_ptr->skills[SK_OCCULT].skill_max > 0)
			{
				known += p_ptr->skills[SK_OCCULT].skill_rank;
			}
			/* Max of 60 */
			if (p_ptr->skills[SK_ADV_OCCULT].skill_max > 0)
			{
				known += p_ptr->skills[SK_ADV_OCCULT].skill_rank;
			}
			/* Max of 80 */
			if (p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 0)
			{
				known += p_ptr->skills[SK_CTHULHU_MYTHOS].skill_rank;
			}
			break;
		}
		case SBF_PRAYER:
		{
			known += p_ptr->skills[SK_LATIN].skill_rank;
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
	/* the p_ptr->lev, again needs to be a skill */
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
	if (p_ptr->skills[SK_HARD_WILL].skill_max > 0)
	{
		willpower += p_ptr->skills[SK_TMPR_WILL].skill_rank;
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
			sprintf(p, " damage %dd%d", 3 + ((magepower) / 4), (4 + (willpower / 6))); break;
		case POW_SENSE_WILL:
			strcpy(p, " sense life"); break;
		case POW_TRANSPORT_WILL:
			sprintf(p, " range %d", 10 + willpower); break;
		case POW_INNER_RAD:
			sprintf(p, " damage 2d%d", magepower); break;
		case POW_HEALING_I:
			sprintf(p, " heal %dd%d", 2 + (willpower / 10), magepower); break;
		case POW_LO_OBJECT:
			strcpy(p, " locate object"); break;
		case POW_LO_TRAPS: 
			strcpy(p, " locate traps"); break;
		case POW_PLAGUE_WILL:
			sprintf(p, " damage %dd%d", 5 + (magepower / 2), 4 + (willpower / 5)); break;
		case POW_RECHARGE:
			sprintf(p, " %d charges", magepower); break;
		case POW_SUPPRESSION_WILL: 
			strcpy(p, " sleep"); break;
		case POW_IDENTIFY: 
			strcpy(p, " identify"); break;
		case POW_FIRE_BOLT:
			sprintf(p, " damage %dd%d", 6+(magepower/2), 6 + (willpower / 4)); break;
		case POW_FROST_BOLT:
			sprintf(p, " damage %dd%d", 6+(magepower/2), 6 + (willpower / 4)); break;
		case POW_TELEPORT_OTHER_I: 
			strcpy(p, " teleport other"); break;
		case POW_HASTE: 
			strcpy(p, " haste"); break;
		case POW_FIREBALL:
			sprintf(p, " damage %dd4", 25 + (magepower + willpower)); break;
		case POW_GENOCIDE: 
			strcpy(p, " genocide"); break;
		case POW_ACID_BOLT:
			sprintf(p, " damage %dd8", (6 + ((magepower + willpower) / 2))); break;
		case POW_LIGHTNING_BOLT:
			sprintf(p, " damage %dd8", (6 + ((magepower + willpower) / 2))); break;
		case POW_CLOUD_KILL:
			sprintf(p, " damage %dd%d", 10 + magepower,  (6 + willpower / 4)); break;
		case POW_ICE_STORM:
			sprintf(p, " damage %dd3", 70 + ((magepower + willpower)/ 2)); break;
		case POW_FIRE_STORM:
			sprintf(p, " damage %dd3", 70 + ((magepower + willpower)/ 2)); break;
		case POW_METEOR_STORM:
			sprintf(p, " %d * damage %dd2", (magepower + willpower) / 10, 75 + ((magepower + willpower)/ 2)); break;
		case POW_ELEMENTAL_BALL:
			sprintf(p, " damage 4 * %dd2", 33 + ((magepower + willpower)/ 2)); break;
		case POW_SOUL_STORM:
			sprintf(p, " damage %dd%d", 300 + (magepower + willpower), 3 + (willpower / 6)); break;
		case POW_RECHARGE_II: 
			strcpy(p, " recharge"); break;
		case POW_EARTHQUAKE_I: 
			strcpy(p, " earthquake"); break;
		case POW_WORD_OF_RECALL_I: 
			strcpy(p, " recall"); break;
		case POW_MIND_OF_WORM: 
			strcpy(p, " enlightenment"); break;
		case POW_MASS_GENOCIDE: 
			strcpy(p, " mass genocide"); break;
		case POW_CHANT: 
			strcpy(p, " blessing"); break;
		case POW_SANCTUARY: 
			strcpy(p, " sleep"); break;
		case POW_HOLY_BOLT: 
			sprintf(p, " dam 2d10 + %d", willpower); break;
		case POW_HEALING_II:
			sprintf(p, " heal 6d10 + %d", willpower); break;
		case POW_PROTECTION_FROM_EVIL: 
			strcpy(p, " prot. evil"); break;
		case POW_HEALING_III:			
			sprintf(p, " heal 8d10 + %d", willpower); break;
		case POW_DESTROY_MACHINE:
			sprintf(p, " dam %dd10", 1 + willpower); break;
		case POW_REMOVE_CURSE: 
			strcpy(p, " remove curse"); break;
		case POW_TURN_UNDEAD: 
			strcpy(p, " fear undead"); break;
		case POW_HEALING_IV:			
			sprintf(p, " heal 300 + %d", willpower); break;
		case POW_PORTAL:			
			sprintf(p, " range %d", willpower * 3); break;
		case POW_SENSE_INVISIBLE: 
			strcpy(p, " sense invis"); break;
		case POW_SENSE_SURROUNDINGS: 
			strcpy(p, " sense area"); break;
		case POW_SATISFY_HUNGER:
			strcpy(p, " hunger"); break;
		case POW_PRAYER: 
			strcpy(p, " blessing"); break;
		case POW_DISPEL_EVIL: 
			strcpy(p, " dispel evil"); break;
		case POW_IDENTIFY_II: 
			strcpy(p, " identify"); break;
		case POW_HOLY_WORD: 
			strcpy(p, " holy world"); break;
		case POW_HEALING_V: 
			strcpy(p, " heal 2000"); break;
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
		case POW_CALL_LIGHT: 
			strcpy(p, " light"); break;
		case POW_REMOVE_FEAR: 
			strcpy(p, " remove fear"); break;
		case POW_HEALING_VI: 
			sprintf(p, " heal 2d10+%d", gearhead); break;
		case POW_DETECT_DOOR_STAIR: 
			strcpy(p, " detect stair"); break;
		case POW_DETECT_TRAPS: 
			strcpy(p, " detect traps"); break;
		case POW_SLOW_POISON: 
			strcpy(p, " slow poison"); break;
		case POW_SPRING_BLADE:			
			sprintf(p, " damage %dd3", 3 + (willpower / 6)); break;
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
			sprintf(p, " damage %dd15",3 + gearhead); break;
		case POW_HEALING_VII: 
			sprintf(p, " heal 6d10+%d", gearhead); break;
		case POW_TURN_STONE_TO_MUD: 
			strcpy(p, " stone to mud"); break;
		case POW_EARTHQUAKE_II: 
			strcpy(p, " earthquake"); break;
		case POW_MISSILE:
			sprintf(p, " damage %dd%d", 60 + (gearhead * 2), 2); break;
		case POW_LEAD_SLUGS:
			sprintf(p, " damage %dd%d", 3 + gearhead, 15); break;
		case POW_LIGHTNING_RAY:
			sprintf(p, " damage %dd%d", 3 + gearhead, 10); break;
		case POW_FROST_RAY:
			sprintf(p, " damage %dd%d", 5 + gearhead, 10); break;
		case POW_HEAT_RAY:
			sprintf(p, " damage %dd%d", 8 + gearhead, 10); break;
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
			sprintf(p, " heal 8d10+%d", gearhead); break;
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

	magic_type *s_ptr;

	byte attr_book, attr_name;

	char comment1[20];
	char comment2[20];
	char info[80];
	char out_val[160];

	object_kind *k_ptr;
	cptr basenm;
	
	int	known = 0;
	
	switch (books[book].flags & SBF_TYPE_MASK) 
	{
		case SBF_MAGIC:
		{
			/* Max of 20 */
			known += p_ptr->skills[SK_LATIN].skill_rank;
			
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

		/* Vivid color for known, cast spells */
		attr_name = attr_book;

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
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) %^s which spell? ",
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
			if ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1)) > PY_MAX_LEVEL) okay = FALSE;
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
	(USE_INVEN | USE_FLOOR))) return;

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
	if (mageprot < 0) mageprot = 0;
	mageprot *= 2;
	prayprot = p_ptr->skills[SK_LESSER_WARD].skill_rank + p_ptr->skills[SK_GREATER_WARD].skill_rank;
	if (prayprot < 0) prayprot = 0;
	prayprot *= 2;
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
	
	magic_type *s_ptr;
	num = 1 + randint(2);
	y = p_ptr->py;
	x = p_ptr->px;

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

				/* increse protect  by 50%, can't mutiply for screwing negative values */
				protect += (protect / 2);

				/* The most backlash can be is around 90, the least is around, or under 0 */
				/* for the most powerful spells, backlash is equal to a positive value */
				/* for a spell with a diff of 80, backlash is 20 ( and the spell can't be */
				/* used unless all of your skill's are maxed). */
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;
				
				/* How badly did I screw up the spell? */
				horror = randint(backlash);
				
				switch(randint(backlash))
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
						take_hit(damroll(1, (horror * 2)), "thaumic energy");
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
						take_hit(damroll(1, (horror * 2)), "thaumic energy");
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);
						break;	
					}
					case 12:
					{
						msg_print("The dark forces take their due!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy");
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);	
						(void)set_cut(p_ptr->cut + rand_int(horror) + horror);
						break;
					}
					case 13:
					{

						msg_print("The dark forces take their due!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy");
						(void)set_poisoned(p_ptr->poisoned + rand_int(horror) + horror);	
						(void)set_cut(p_ptr->cut + rand_int(horror) + horror);
						(void)set_stun(p_ptr->stun + rand_int(horror) + horror);	
					}
					case 14:
					{
						/* Bypasses sustain! */
						msg_print("You body is wracked by pain!");
						take_hit(damroll(1, (horror * 2)), "thaumic energy");
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
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
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
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
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
						take_hit(damroll(horror, (horror * 2)), "thaumic energy");
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
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
						}
						break;	
					}
					default:
					{
						msg_print("i have seen our doom. THERE IS NO ESCAPE!!!");
						take_hit(damroll(horror, (horror * 3)), "thaumic energy");
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
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_HI_DEMON, FALSE, FALSE);
						}
						break;	
					}
				}
				break;
			}
			case SBF_PRAYER:
			{
				int backlash, protect, horror;

				message(MSG_GENERIC, 0, "You prayer goes unanswered!");

				/* HACK!! - If the player lacks these skills, protect becomes a negative value */
				/* If they haven't put any points into the first skill, the total is still neg */
				/* this means that not knowing anything about energy hurts your saftey */
				protect = p_ptr->skills[SK_LESSER_WARD].skill_rank + p_ptr->skills[SK_GREATER_WARD].skill_rank;

				/* doubles protect, can't mutiply for screwing negative values */
				protect += protect;

				/* Prayers can be totally safe */
				backlash = ((s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1) - (protect)));
				if (backlash < 1) backlash = 1;
				
				/* How badly did I screw up the spell? */
				horror = randint(backlash);
				
				switch(randint(backlash))
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
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
						}
						break;
					}
					case 11: case 12: case 13: case 14: 
					{
						msg_print("Dark and nefarious forces intercept your plea!");
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
						}
						break;
					}
					case 15: case 16: case 17: case 18: 
					{
						msg_print("Your chants have disturbed elder-beasts!");
						break;
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_DEMON, FALSE, FALSE);
						}
					}
					case 19: case 20: case 21: case 22:
					{
						msg_print("The desperateness of your plea draws a great evil towards you!");
						for (i = 0; i < horror; i++)
						{
							(void)summon_specific(y, x, p_ptr->depth, SUMMON_HI_DEMON, FALSE, FALSE);
						}
						break;
					}
					default:
					{
						msg_print("Your unpenitent plea has angered God!");
						take_hit(damroll(horror, (horror * 3)), "divine will");
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
		if (!aux_spell_cast(book, s_ptr->index)) return;
	}

	/* Take a turn */
	if (p_ptr->skills[SK_FAST_CAST].skill_rank > 0)
	{
		fastcast = p_ptr->skills[SK_FAST_CAST].skill_rank;
		/* This makes fastcast a number from 4-80. */
		fastcast *= 4;
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
	/* Can use instruments, can't use books */
	int flg;

	object_type *o_ptr;

	if (p_ptr->confused)
	{
		message(MSG_GENERIC, 0, "You are too confused!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		message(MSG_GENERIC, 0, "You cannot see!");
	}
	
	/* Get an item */
	item_tester_hook = item_tester_hook_book;


	flg = (USE_INVEN | USE_FLOOR);

	/* Can read books, can't use instruments */
	if (!get_item(&item, "Use which? ", "You have nothing that you can use.", 
	flg)) return;


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
 * beam is the base chance of beaming, obvious determines if the 
 * effect of the spell can be seen by the player.
 * Mage power is the total of your ritual magic skill ranks, and your themla. 
 * Note that it requires a _lot_ of skill points to even get access to thelma.
 * for non mages assume mage power is never > 20, though it has a range of 1-40
 * values above 20 should be supercharged
 * willpower is the force of your will. Only the spellcasting classes have access
 * to will - it ranges from 1-60. Only 'spellcasting' classes have access to will
 * NOTE: spells have negative effects. Some are elminatable - some are not. Welcome to the wonderful
 * world of dark mystic energy.
 */
bool do_power(int book, int idx, int dir, int beam, bool *obvious, int magepower, int willpower, int gearhead, int mageprot, int prayprot)
{
	int backlash;
	int magiclash, praylash;
	int randmlash, randplash;
	int py, px;
	magic_type *s_ptr;
	s_ptr = &books[book].contents[idx];
	backlash = s_ptr->slevel + (cp_ptr->spell_handicap[book] - 1);
	
	py = p_ptr->py;
	px = p_ptr->px;
	
	magiclash = backlash -  mageprot;
	if (magiclash < 0) 
	{
	magiclash = 0;
	}
	praylash = backlash - prayprot;
	if (praylash < 0) 
	{
	praylash = 0;
	}
	
	randmlash = rand_int(magiclash);
	randplash = rand_int(praylash);
	
	/* We haven't seen anything yet */
	*obvious = FALSE;

	switch (idx)
	{
		/* Liber AL vel Legis (sval 0) */
		case POW_AIM_OF_THE_WILL:
		{
			
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
			                  damroll(3 + ((magepower) / 4), (4 + (willpower / 5))));
			take_hit(magiclash, "Mystic energy");
			break;
		}

		case POW_SENSE_WILL:
		{
			(void)detect_monsters_normal();
			break;
		}

		case POW_TRANSPORT_WILL:
		{
			teleport_player(10 + willpower);
			dec_stat(A_VIG, randmlash, FALSE);
			break;
		}

		case POW_INNER_RAD:
		{
			(void)lite_area(damroll(2, (magepower)), (magepower / 5) + 1);
			break;
		}

		case POW_HEALING_I:
		{
			(void)hp_player(damroll(2 + (willpower / 10), magepower));
			(void)set_cut(p_ptr->cut - magepower);
			dec_stat(A_VIG, randmlash, FALSE);
			break;
		}

		case POW_LO_OBJECT:
		{
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			dec_stat(A_SCH, randmlash, FALSE);
			break;
		}

		case POW_LO_TRAPS:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			dec_stat(A_EGO, randmlash, FALSE);
			break;
		}

		case POW_PLAGUE_WILL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-10, GF_POIS, dir,
			         damroll(5 + (magepower / 2), 4 + (willpower / 5)));
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_RECHARGE:
		{
			(void)recharge(magepower);
			dec_stat(A_VIG, randmlash, FALSE);
			break;
		}
		case POW_SUPPRESSION_WILL:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case POW_IDENTIFY:
		{
			(void)ident_spell();
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			break;
		}
		case POW_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(6+(magepower/2), 6 + willpower / 4));
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_FROST_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_COLD, dir,
			                  damroll(6+(magepower/2), 6 + willpower / 4));
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_TELEPORT_OTHER_I:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			(void)teleport_monster(dir);
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			dec_stat(A_MUS, randmlash, FALSE);
			break;
		}
		case POW_HASTE:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + magepower + willpower);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
			dec_stat(A_SCH, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);

			break;
		}
		case POW_FIREBALL:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_FIRE, dir,
			          damroll(25 + (magepower + willpower), 4), 4);
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_GENOCIDE:
		{
			(void)genocide();
			dec_stat(A_MUS, randmlash, FALSE);
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_AGI, randmlash, FALSE);
			dec_stat(A_SCH, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			dec_stat(A_CHR, randmlash, FALSE);
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_ACID_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_ACID, dir,
			                  damroll(6+((magepower + willpower) /2), 8));
			take_hit(magiclash, "Mystic energy");
			break;
		}

		case POW_LIGHTNING_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_ELEC, dir,
			                  damroll(6+((magepower + willpower) /2), 8));
			take_hit(magiclash, "Mystic energy");
			break;
		}

		case POW_CLOUD_KILL:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_POIS, dir,
			          damroll(10 + magepower, 6 + (willpower / 4)), 5);
			take_hit(magiclash, "Mystic energy");
			break;
		}

		case POW_ICE_STORM:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_COLD, dir,
			          damroll(70 + ((magepower + willpower)/ 2), 3), 4);
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_FIRE_STORM:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_FIRE, dir,
			          damroll(70 + ((magepower + willpower)/ 2), 3), 4);
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_METEOR_STORM:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_barrage(GF_METEOR, dir,
			          75 + ((magepower + willpower)/ 2), 2, (magepower + willpower) / 10, 2, (magepower + willpower) / 20);
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_ELEMENTAL_BALL:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_ball(GF_FIRE, dir,
			          damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_COLD, dir,
			          damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_ELEC, dir,
			          damroll(33 + ((magepower + willpower)/ 2), 2), 2);
			fire_ball(GF_ACID, dir,
			          damroll(33 + ((magepower + willpower)/ 2), 2), 2);          
			take_hit(magiclash, "Mystic energy");
			break;
		}
		case POW_SOUL_STORM:
		{
			fire_ball(GF_MANA, 0,
			          damroll(300 + (magepower + willpower), 3 + (willpower / 6)), willpower / 8);
			take_hit(magiclash, "Mystic energy");
			break;
		}		
		case POW_RECHARGE_II:
		{
			(void)recharge(40);
			dec_stat(A_VIG, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_SCH, randmlash * 2, FALSE);
			dec_stat(A_EGO, randmlash * 2, FALSE);

			break;
		}
		case POW_EARTHQUAKE_I:
		{
			earthquake(py, px, 10);
			dec_stat(A_CHR, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_VIG, randmlash * 2, FALSE);
			dec_stat(A_AGI, randmlash * 2, FALSE);

	 		break;
	 		
		}
		case POW_WORD_OF_RECALL_I:
		{
			set_recall();
			dec_stat(A_VIG, randmlash, FALSE);
			dec_stat(A_EGO, randmlash, FALSE);
			break;
		}
		case POW_MIND_OF_WORM:
		{
			wiz_lite();
			dec_stat(A_SCH, randmlash * 2, FALSE);
			dec_stat(A_EGO, randmlash * 2, FALSE);
			dec_stat(A_CHR, randmlash * 2, FALSE);
			dec_stat(A_VIG, randmlash * 2, FALSE);
	 		break;
		}
		case POW_MASS_GENOCIDE:
		{
			(void)mass_genocide();
			dec_stat(A_SCH, randmlash * 2, FALSE);
			dec_stat(A_EGO, randmlash * 2, FALSE);
			dec_stat(A_CHR, randmlash * 2, FALSE);
			dec_stat(A_MUS, randmlash * 2, FALSE);
			dec_stat(A_AGI, randmlash * 2, FALSE);
			dec_stat(A_VIG, randmlash * 2, FALSE);
			break;
		}
		case POW_CHANT:
		{
			(void)set_blessed(p_ptr->blessed + randint(24) + willpower);
			break;
		}
		case POW_SANCTUARY:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case POW_HOLY_BOLT:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam - 20, GF_HOLY_ORB, dir, damroll(2,10) + willpower);
			break;
		}
		case POW_HEALING_II:
		{
			(void)hp_player(damroll(6, 10) + willpower);
	 		(void)set_cut(0);
	 		break;
		}
		case POW_PROTECTION_FROM_EVIL:
		{
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * willpower);
	 		break;
		}
		case POW_HEALING_III:
		{
			(void)hp_player(damroll(8, 10) + willpower);
	 		(void)set_stun(0);
	 		(void)set_cut(0);
	 		break;
		}
		case POW_REMOVE_CURSE:
		{
			remove_curse();
			break;
		}
		case POW_TURN_UNDEAD:
		{
			(void)turn_undead();
	 		break;
		}
		case POW_HEALING_IV:
		{
			(void)hp_player(300 + willpower);
	 		(void)set_stun(0);
	 		(void)set_cut(0);
	 		break;
		}
		case POW_DESTROY_MACHINE:
		{
			if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt(GF_KILL_WALL, dir, damroll(1 + willpower, 10));
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
	    	(void)set_blessed(p_ptr->blessed + randint(48) + willpower);
	 		break;
	    }
	    case POW_DISPEL_EVIL:
	    {
	    	(void)dispel_evil(randint(willpower * 3));
	 		break;
	    }
	    case POW_IDENTIFY_II:
	    {
	    	(void)ident_spell();
			break;
	    }
	    case POW_HOLY_WORD:
	    {
	    	(void)dispel_evil(randint(willpower * 4));
	 		(void)hp_player(1000);
	 		(void)set_afraid(0);
	 		(void)set_poisoned(0);
	 		(void)set_stun(0);
	 		(void)set_cut(0);
	 		break;
	    }
	    case POW_HEALING_V:
	    {
	    	(void)hp_player(2000);
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
			(void)recharge(40 + willpower);
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
			(void)set_oppose_acid(p_ptr->oppose_acid + time);
			(void)set_oppose_elec(p_ptr->oppose_elec + time);
			(void)set_oppose_fire(p_ptr->oppose_fire + time);
			(void)set_oppose_cold(p_ptr->oppose_cold + time);
			(void)set_oppose_pois(p_ptr->oppose_pois + time);
			break;
		}
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
	 	case POW_HEALING_VI:
	 	{
	 		(void)hp_player(damroll(2,10) + gearhead);
	 		(void)set_cut(p_ptr->cut - 10);
	 		break;
	 	}
	 	case POW_DETECT_DOOR_STAIR:
	 	{
	 		(void)detect_doors();
	 		(void)detect_stairs();
	 		break;
	 	}
	 	case POW_DETECT_TRAPS:
	 	{
	 		(void)detect_traps();
	 		break;
	 	}
	 	case POW_SLOW_POISON:
	 	{
	 		(void)set_poisoned(p_ptr->poisoned / 2);
	 		break;
	 	}
	 	case POW_SPRING_BLADE:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
			                  damroll(3 + (gearhead / 6), 3));
			break;
		}
		case POW_NOURISHMENT:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case POW_OBJECT_ANALYSIS:
		{
			(void)ident_spell();
			break;
		}
		case POW_DETECT_HOSTILE:
		{
			(void)detect_monsters_normal();
			break;
		}
			
		case POW_DETECT_TRAPS_DOORS:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		
		case POW_TREASURE_DETECTION:
		{
			(void)detect_treasure();
			(void)detect_objects_gold();
			break;
		}
		
		case POW_DETECT_ENCHANTMENT:
		{
			(void)detect_objects_magic();
			break;
		}
		
		case POW_DETECTION:
		{
			(void)detect_all();
			break;
		}

		case POW_PERCEPTION:
		{
			(void)ident_spell();
			break;
		}

		case POW_PROBEING:
		{
			(void)probing();
			break;
		}

		case POW_CLARIVOYANCE:
		{
			(void)wiz_lite();
			break;
		}
		case POW_IDENTIFY_III:
		{
			(void)identify_fully();
			break;
		}
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
	 		(void)set_blessed(p_ptr->blessed + randint(24) +24);
	 		(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
	 		(void)set_hero(p_ptr->hero + randint(25) + 25);
			(void)set_afraid(0);
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
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
			                  damroll(3 + gearhead, 15));
			break;
	 	}
	 	case POW_HEALING_VII:
	 	{
	 		(void)hp_player(damroll(6, 10) + gearhead);
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
			fire_ball(GF_FIRE, dir,
			          damroll(60 + (gearhead * 2), 2), 2);
			break;
	 	}
	 	case POW_EMP:
	 	{
	 		int flg = PROJECT_KILL | PROJECT_STOP;
	 		project(-1, 10, p_ptr->py, p_ptr->px, 1200, GF_EMP, flg);
	 		break;
	 	}
	 	case POW_LEAD_SLUGS:
	 	{
		 	if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
			                  damroll(3 + gearhead, 15));
			break;
	 	}
	 	case POW_LIGHTNING_RAY:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam-10, GF_ELEC, dir,
			                  damroll(3 + gearhead, 10));
			break;
	 	}
	 	case POW_FROST_RAY:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
			                  damroll(5+(gearhead), 10));
			break;
	 	}
	 	case POW_HEAT_RAY:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir,
			                  damroll(8+(gearhead), 10));
			break;
	 	}
	 	case POW_GRAVITY_RAY:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			fire_bolt_or_beam(beam, GF_GRAVITY, dir,
			                  damroll(10+(gearhead), 10));
			break;
	 	}
	 	case POW_TELEPORT_OTHER_II:
	 	{
	 		if (!get_aim_dir(&dir)) return(FALSE);
			(void)teleport_monster(dir);
			break;
	 	
	 	}
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
			(void)teleport_player_level();
			break;
		}

		case POW_WORD_OF_RECALL_III:
		{
			set_recall();
			break;
		}
	 	case POW_HEALING_VIII:
	 	{
	 		(void)hp_player(damroll(8, 10) + gearhead);
	 		(void)set_stun(0);
	 		(void)set_cut(0);
	 		break;
	 	}
	 
		case POW_BIOLOGICAL_ENHANCE:
		{
			int time = randint(20) + 20 + gearhead;
			(void)set_oppose_acid(p_ptr->oppose_acid + time);
			(void)set_oppose_elec(p_ptr->oppose_elec + time);
			(void)set_oppose_fire(p_ptr->oppose_fire + time);
			(void)set_oppose_cold(p_ptr->oppose_cold + time);
			(void)set_oppose_pois(p_ptr->oppose_pois + time);
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
			recharge(80);
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
			(void)set_oppose_acid(p_ptr->oppose_acid + time);
			(void)set_oppose_elec(p_ptr->oppose_elec + time);
			(void)set_oppose_fire(p_ptr->oppose_fire + time);
			(void)set_oppose_cold(p_ptr->oppose_cold + time);
			(void)set_oppose_pois(p_ptr->oppose_pois + time);
			(void)set_shield(p_ptr->shield + time);
			(void)hp_player(30);
			(void)set_shero(p_ptr->shero + time);
			(void)set_afraid(0);
			if (!p_ptr->fast)
			{
				(void)set_fast(time);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(10));
			}
			break;
		}
			case POW_HEALING_IX:
			{
				(void)hp_player(2000);
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
	{POW_AIM_OF_THE_WILL,	"uses your will to directly damage monsters. (HP damage) "},
	{POW_SENSE_WILL,		"detects the will of nearby creatures"},
	{POW_TRANSPORT_WILL,	"transports your physical self a short distance (Vigor damage)"},
	{POW_INNER_RAD,			"lights rooms with the radiance of your soul"},
	{POW_HEALING_I,			"heals some current damage (Vigor damage)"},	
	{POW_LO_OBJECT,			"detects objects nearby (Schooling damage)"},	
	{POW_LO_TRAPS,			"detects nearby traps, doors, and stairs (Ego damage)"},	
	{POW_PLAGUE_WILL,		"attempts to poison anothers soul (HP damage)"},	
	/* The Zohar (Book of Splendor) (sval 1) */
	{POW_RECHARGE, 			"transmutes will into energy for devices (Vigor damage)"},
	{POW_SUPPRESSION_WILL, 	"puts a monster you touch to sleep"},
	{POW_IDENTIFY, 			"determines information about an object (Vigor and Ego damage)"},
	{POW_FIRE_BOLT, 		"creates a bolt of flame to burn your enemies (HP damage)"},
	{POW_FROST_BOLT, 		"creates a bolt of frost to freeze your enemies (HP damage)"},
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
	{POW_RECHARGE_II, 		"transmutes will into energy for devices (Vig, Mus, Sch, & Ego damage)"},
	{POW_EARTHQUAKE_I, 		"calls upon the great worm to rock the earth! (Charm, Mus, Vig, & Agi damage)"},
	{POW_WORD_OF_RECALL_I, 	"transports you quickly back to the surface (Vigor and Ego damage)"},
	{POW_MIND_OF_WORM, 		"gain the powerful worm's mighty senses! (Schooling, Ego, charm, and Vigor damage)"},
	{POW_MASS_GENOCIDE, 	"removes all creatures from your presence! (HP + All stat damage)"},
	/* Book name (sval 4) */
	/* Book name (sval 5) */
	/* Book name (sval 6) */
	/* Book name (sval 7) */
	/* Book name (sval 8) */
	/* Book name (sval 9) */
	/* Treatise on the Resurrection (sval 10) */
	{POW_CHANT, 			"the power of god blesses you! (+ to ac, and to hit)"},
	{POW_SANCTUARY, 		"puts any monster you touch to sleep"},
	{POW_HOLY_BOLT, 			"fires a bolt of holy energy"},
	{POW_HEALING_II, 		"heals a small amount of damage"},
	{POW_PROTECTION_FROM_EVIL, "spiritual power will protect you from evil creatures!"},
	{POW_HEALING_III, 		"heals some damage in addition to curing cuts and stunning"},
	{POW_REMOVE_CURSE, 		"removes most curses from your equipment"},
	{POW_TURN_UNDEAD, 		"causes nearby undead to flee"},
	{POW_HEALING_IV, 		"heals a great amount of damage, and eliminates cuts and stunning"},
	{POW_DESTROY_MACHINE, 	"destroys machines"},
	/* Odes of Solomon (sval 11) */
	{POW_PORTAL, 			"moves you a short distance"},
	{POW_SENSE_INVISIBLE,   "allows you to detect invisable creatures for a short time"},
	{POW_SENSE_SURROUNDINGS, "detects the dungeon around you"},
	{POW_SATISFY_HUNGER, 	"fills your stomach"},
	{POW_PRAYER, 			"calls upon the power of god to protect you (+ to ac, and to hit)"},
	{POW_DISPEL_EVIL, 		"attempts to destory evil creatures in your sight"},
	{POW_IDENTIFY_II, 		"provides knowledge about an item"},
	{POW_HOLY_WORD, 		"heals you a great deal, and dispels evil creatures."},
	{POW_HEALING_V, 		"fully heals your hit points"},
	{POW_RESTORATION, 		"causes all your stats to be restored."},
	/* The Pnakotic Manuscripts (sval 12) */
	{POW_DISPEL_CURSE, 		"removes almost all curses"},
	{POW_BANISHMENT, 		"banishes evil from your sight"},
	{POW_RECHARGE_III, 		"recharges objects"},
	{POW_DISPEL_EVIL_II,	"dispells a great amount of evil"},
	{POW_WORD_OF_RECALL_II, "draws you back to the surface"},
	{POW_RESISTANCE, 		"protects you from the basic elements"},
	/* Book name (sval 13) */
	/* Book name (sval 14) */
	/* Book name (sval 15) */
	/* Book name (sval 16) */
	/* Book name (sval 17) */
	/* Utility knife (sval 18) */
	{POW_CALL_LIGHT, 		"lights a room"},
	{POW_REMOVE_FEAR,		"uses harmoics to prevent fear"},
	{POW_HEALING_VI, 		"heals a very small amount"},
	{POW_DETECT_DOOR_STAIR, "detects doors and stairs"},
	{POW_DETECT_TRAPS, 		"detects traps"},
	{POW_SLOW_POISON, 		"removes some poison damage"},
	{POW_SPRING_BLADE, 		"shoots a small projectile towards enemies"},
	{POW_NOURISHMENT, 		"provides a small pill satisfying your bodies needs"},
	{POW_OBJECT_ANALYSIS, 	"analyzes objects, and tells you their properties"},
	{POW_FETCH, 			"shoots out a small grappeling hook to pull items towards you"},
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
	{POW_EARTHQUAKE_II, 	"devestates the surrounding area"},
	{POW_MISSILE, 			"fires an explosive missile"},
	{POW_EMP, 				"emits an electromagnetic pulse"},
	/* Clockwork carbine (sval 21) */
	{POW_LEAD_SLUGS, 		"activates vulcan cannons"},
	{POW_LIGHTNING_RAY, 	"emits a lightning ray"},
	{POW_FROST_RAY, 		"shoots out a beam of cold"},
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
	{POW_HEALING_VIII, 		"fixes moderate damage to your body. Heals stunning and cuts."},
	{POW_BIOLOGICAL_ENHANCE, "opposes all elements, and raises AC"},
	{POW_POLYMORPH_OTHER, "transforms your opponents into different monsters"},
	{POW_RECHARGE_IV, 		"recharges power cells"},
	{POW_DOOR_CREATION, 	"creates doors"},
	{POW_STAIR_CREATION, 	"creates stairs"},
	{POW_BIOLOGICAL_ENHANCE_II, "as biological enhance, but also + to hit, and speed"},
	{POW_HEALING_IX, 		"powerful full healing."},
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
	{POW_DETECT_EVIL,		"detects all evil monsters, even invisible ones"},
	{POW_DETECT_INVIS,		"detects all invisible monsters on the current panel"},
	{POW_DETECT_TRAP,		"detects all traps on the current panel"},
	{POW_DETECT_TREASURE,	"detects all treasure on the current panel"},
	{POW_DETECT_DOOR_STAIR,	"detects all doors and stairs on the current panel"},
	{POW_DETECT_TRAP_DOOR,	"detects hidden traps, stairs and doors on the current screen"},
	{POW_DETECT_ITEM,		"detects all objects on the current panel"},
	{POW_DETECT_ENCHANT,	"detects magical objects on the current panel"},
	{POW_DETECT_ALL,		"detects everything of interest on the panel"},
	{POW_ABSORB_HIT,		"temporarily reverses the effect of damage"},
	{POW_BLESS_1,			"provides a short-duration bonus to fighting ability and ac"},
	{POW_BLESS_2,			"provides a medium-duration bonus to fighting ability and ac"},
	{POW_BLESS_3,			"provides a long-duration bonus to fighting ability and ac"},
	{POW_HEROISM,			"temporarily raises fighting skill and makes you immune to fear"},
	{POW_STABILITY,			"temporarily makes you immune to confusion and stunning"},
	{POW_RAGE_1,			"causes temporary berserk rage"},
	{POW_RAGE_2,			"causes temporary berserk rage"},
	{POW_RAGE_BLESS_RESIST,	"causes temporary berserk rage, blessing, and resistance"},
	{POW_SHIELD,			"temporarily increases armour class"},
	{POW_INVIS_1,			"temporarily turns you invisible"},
	{POW_INVIS_2,			"temporarily turns you invisible"},
	{POW_RESILIENCE,		"temporarily raises your AC by 50 and reduces all damage by 66%"},
	{POW_INFRAVISION,		"temporarily increases the range of your infravision"},
	{POW_STEALTH,			"temporarily increases your stealth"},
	{POW_SEE_INVIS,			"provides temporary see invisible"},
	{POW_PROT_EVIL,			"provides temporary protection from lesser evil creatures"},
	{POW_HASTE_SELF_1,		"temporarily hastes you"},
	{POW_HASTE_SELF_2,		"hastes you for a long duration"},
	{POW_HASTE_SELF_3,		"hastes you for a very long duration"},
	{POW_DISARM,			"disarms a trap"},
	{POW_DEST_TRAP_DOOR_1,	"destroys a line of traps and doors"},
	{POW_DEST_TRAP_DOOR_2,	"destroys all doors and traps next to you"},
	{POW_STONE_TO_MUD,		"melts a wall square to floor"},
	{POW_CREATE_DOOR,		"creates a barrier of doors around you"},
	{POW_CREATE_WALL,		"creates a barrier of walls around you"},
	{POW_CREATE_STAIR,		"creates a randomly oriented staircase nearby"},
	{POW_CREATE_TRAP,		"creates traps around you"},
	{POW_MAGIC_LOCK,		"magically locks all nearby closed doors"},
	{POW_ACQUIRE_1,			"creates a great item"},
	{POW_ACQUIRE_2,			"creates several great items"},
	{POW_AGGRAVATE,			"aggravates nearby monsters"},
	{POW_AGGRAVATE_SAFE,	"aggravates nearby monsters"},
	{POW_CONFUSE_MONSTER,	"attempts to confuse one monster"},
	{POW_CONFUSE_ALL,		"attempts to confuse all monsters in line of sight"},
	{POW_SLEEP_MONSTER,		"attempts to put a monster to sleep"},
	{POW_SLEEP_ADJACENT,	"attempts to put all adjacent monsters to sleep"},
	{POW_SLEEP_ALL,			"attempts to put all monsters in line of sight to sleep"},
	{POW_SLOW_MONSTER,		"attempts to slow a monster down"},
	{POW_SLOW_ALL,			"attempts to slow all monsters in line of sight"},
	{POW_CALM_MONSTER,		"attempts to calm a monster"},
	{POW_CALM_ANIMALS,		"attempts to calm all natural creatures in line of sight"},
	{POW_CALM_NON_EVIL,		"attempts to calm all non-evil creatures in line of sight"},
	{POW_CALM_NON_CHAOS,	"attempts to calm all non-chaotic creatures in line of sight"},
	{POW_CALM_ALL,			"attempts to calm all creatures in line of sight"},
	{POW_BLIND_MONSTER,		"attempts to blind a monster"},
	{POW_SCARE_MONSTER,		"attempts to frighten one monster"},
	{POW_SCARE_UNDEAD,		"attempts to make all undead monsters in line of sight flee"},
	{POW_SCARE_ALL,			"attempts to make all monsters in line of sight flee"},
	{POW_CALL_MONSTER,		"attempts to teleport a monster closer to you"},
	{POW_POLY_MONSTER,		"attempts to change a monster"},
	{POW_HEAL_MONSTER,		"attempts to heal a monster"},
	{POW_HASTE_MONSTER,		"attempts to haste a monster"},
	{POW_CLONE_MONSTER,		"attempts to clone a monster"},
	{POW_HOLY_BOLT,			"cause holy flame to burn your enemies"},
	{POW_RECHARGE_1,		"recharges a staff, wand, rod or talisman"},
	{POW_RECHARGE_2,		"recharges a staff, wand, rod or talisman"},
	{POW_RECHARGE_3,		"powerfully recharges a staff, wand, rod or talisman"},
	{POW_RECHARGE_4,		"powerfully recharges a staff, wand, rod or talisman"},
	{POW_IDENTIFY,			"identifies an object"},
	{POW_IDENTIFY_PACK,		"identifies everything being carried"},
	{POW_IDENTIFY_FULL,		"reveals all information about a specific object"},
	{POW_RES_ACID,			"temporarily protects from acid"},
	{POW_RES_ELEC,			"temporarily protects from electricity"},
	{POW_RES_FIRE,			"temporarily protects from fire"},
	{POW_RES_COLD,			"temporarily protects from cold"},
	{POW_RES_FIRE_COLD,		"temporarily protects from fire & frost"},
	{POW_RES_ACID_ELEC,		"temporarily protects from acid & electricity"},
	{POW_RES_LITE_DARK,		"temporarily protects from light & darkness"},
	{POW_RES_CHAOS_NEXUS,	"temporarily protects from chaos & nexus"},
	{POW_RES_POISON,		"temporarily protects from poison"},
	{POW_RES_DISEASE,		"temporarily protects from disease"},
	{POW_RES_SOUND,			"temporarily protects from sound"},
	{POW_RES_ELEMENTS,		"temporarily protects from all four elements"},
	{POW_RES_GREATER,		"temporarily protects from many things"},
	{POW_RESISTANCE,		"temporarily protects from all elements & poison"},
	{POW_GLYPH_WARDING,		"puts a strong glyph on the floor that monsters cannot pass over"},
	{POW_GLYPH_LESSER,		"puts a glyph on the floor that monsters cannot pass over"},
	{POW_GLYPH_HOLY,		"puts a sigil that blocks undead & demons and prevents summoning"},
	{POW_REMOVE_CURSE_1,	"removes standard curses"},
	{POW_REMOVE_CURSE_2,	"removes both normal and heavy curses"},
	{POW_MAP_1,				"maps the local area"},
	{POW_MAP_2,				"permanently lights and detects objects on the entire level"},
	{POW_MAP_3,				"permanently lights and detects all on the entire level"},
	{POW_PROBE_MONSTER,		"teaches about the attributes of a monster"},
	{POW_PROBE_ALL,			"teaches about the attributes of all visible monsters"},
	{POW_KNOW_ALL,			"reveals the entire map, IDs your pack, and raises your stats"},
	{POW_ENCHANT_WEAPON_HIT,"adds a plus to hit to weapons"},
	{POW_ENCHANT_WEAPON_DAM,"adds a plus to damage to weapons"},
	{POW_ENCHANT_WEAPON,	"adds plusses to hit and damage to weapons"},
	{POW_ENCHANT_ARMOR_1,	"increases armour's bonus to armour class"},
	{POW_ENCHANT_ARMOR_2,	"powerfully increases armour's bonus to armour class"},
	{POW_BRAND_WEAPON_ELMNT,"imbues weapons with elemental power"},
	{POW_BRAND_ARROW_ANML,	"makes arrows extra powerful against animals"},
	{POW_BRAND_ARROW_WOUND,	"makes arrows sharper and more powerful"},
	{POW_BRAND_ARROW_ELMNT,	"imbues arrows with elemental power"},
	{POW_BRAND_BOLT_FIRE,	"imbues bolts with the power of fire"},
	{POW_BRAND_BOLT_LITE,	"imbues bolts with the power of light"},
	{POW_BRAND_SHOT_POIS,	"makes your shots hit with a poisonous strike"},
	{POW_BRAND_SHOT_HOLY,	"makes your shots powerful against evil creatures"},
	{POW_BIZZARE,			"causes powerful, random, effects"},
	{POW_CURSE_EQUIP_1,		"curses some of your equipment"},
	{POW_CURSE_EQUIP_2,		"curses all of your equipment"},
	{POW_SUM_MONSTER,		"summons monsters to fight against you"},
	{POW_SUM_UNDEAD,		"summons undead creatures to fight against you"},
	{POW_SUM_DRAGON,		"summons dragons to fight against you"},
	{POW_NAUSEA,			"induces vomiting"},
	{POW_POISON_SELF,		"poisons you"},
	{POW_BLIND_SELF,		"blinds you"},
	{POW_CONFUSE_SELF,		"confuses you"},
	{POW_SCARE_SELF,		"scares you"},
	{POW_SLOW_SELF,			"slows you"},
	{POW_PARALYZE,			"paralyzes you"},
	{POW_HALLUCINATE,		"causes you to hallucinate"},
	{POW_DISEASE,			"infects you with a disease"},
	{POW_DEFORM,			"deforms you and swaps your stats"},
	{POW_LOSE_STR,			"lowers your strength"},
	{POW_LOSE_INT,			"lowers your intelligence"},
	{POW_LOSE_WIS,			"lowers your wisdom"},		
	{POW_LOSE_DEX,			"lowers your dexterity"},		
	{POW_LOSE_CON,			"lowers your constitution"},		
	{POW_LOSE_CHR,			"lowers your charisma"},		
	{POW_LOSE_EXP,			"lowers your experience"},
	{POW_RUINATION,			"lowers your stats permanently and damages you"},
	{POW_DETONATE,			"damages you"},
	{POW_KILL_SELF,			"kills you"},
	{POW_DRAGON_BLACK,		"breathes acid (125+)"},
	{POW_DRAGON_BLUE,		"breathes lightning (125+)"},
	{POW_DRAGON_WHITE,		"breathes frost (125+)"},
	{POW_DRAGON_RED,		"breathes fire (125+)"},
	{POW_DRAGON_GREEN,		"breathes poison gas (150+)"},
	{POW_DRAGON_BRONZE,		"breathes confusion (100+)"},
	{POW_DRAGON_GOLD,		"breathes sound (100+)"},
	{POW_DRAGON_SILVER,		"breathes shards (100+)"},
	{POW_DRAGON_MH,			"breathes multi-hued (250+)"},
	{POW_DRAGON_SPIRIT,		"breathes force (250+)"},
	{POW_DRAGON_SHADOW,		"breathes nether (250+)"},
	{POW_DRAGON_ETHER,		"breathes light/darkness/confusion (250+)"},
	{POW_DRAGON_CHAOS,		"breathes chaos/disenchant/plasma/sound (350+)"},
	{POW_DRAGON_TIME,		"breathes time/inertia/nexus/nether (350+)"},
	{POW_DRAGON_POWER,		"breathes the elements (400+)"},
	{POW_RISK_HACK,			"either kills or rewards you"},
	{POW_WONDER_HACK,		"creates a random effect"},
};


