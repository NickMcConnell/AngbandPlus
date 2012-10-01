/* File: book.c */

/*
 * Tables containing Detailed info on spells. Spell failure chance, if is OK to cast, 
 * extra info shown in books, and print spells of a given spellbook.
 *
 * Originally based on Oangband's info.c
 *
 * Copyright (c) 1999 Leon Marrick, Ben Harrison, James E. Wilson, 
 * Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Prints "User's tips" for various spells.  User's tips only appear after 
 * a learnt spell is browsed. -LM-
 */
cptr spell_tips[SPELLS_TOTAL] = 
{
/* Sorcery Spells */

	NULL,
	/* Magic for Beginners (sval 0) */
	"Fires a bolt of mana.",
	"Random minor displacement.",
	"Permanently light up the nearby area.",
	"Detects monsters on the current screen that are not invisible.",
	"Fires a ball of poison.",
	"Attempts to confuse one monster.",
	"Detects hidden traps and doors on the current screen.",
	/* Conjurings and Tricks (sval 1) */
	"The next time you are hurt you heals instead of taking damage.",
	"Fires a bolt or beam of lightning.",	
	"Destroys all doors and traps next to you.",
	"Attempts to put a monster to sleep.",
	"Fires a bolt or beam of cold.",
	"Random major displacement.",
	"Fires a line of light.  Effects light-hating creatures.",
	"Melts a wall square to floor.",
	/* Incantations and Illusions (sval 2) */
	"Attempts to put all monster in line of sight to sleep.",
	"Fully feeds you.",
	"Minor recharging.",
	"Fires a bolt or beam of acid.",
	"Standard identification of an object.",
	"Creates a barrier of doors around you.",
	"Detects magical objects on the current panel.",
	"Fires a bolt or beam of fire.",
	"Attempts to slow a monster down.",
	/* Sorcery and Evocations (sval 3) */
	"Fires a ball of frost.",
	"Medium-strength recharging spell.",
	"Teleports a line of opponents away.",
	"Fires a ball of fire.",	
	"Temporarily hasten yourself.",
	"Destroys objects and monsters, and banishes uniques.",
	"Removes all monsters of the symbol you choose from the level.",
	/* Resistance of Scarabtarices (sval 4) */
	"Opposition to fire.  Cumulative with equipment.",
	"Opposition to cold.  Cumulative with equipment.",
	"Opposition to acid & electricity.  Cumulative with equipment.",
	"Opposition to poison.  Cumulative with equipment.",
	"Opposition to all elements + poison.  Cumulative with equipment.",
	/* Mordenkainen's Escapes (sval 5) */
	"Creates a randomly oriented staircase nearby.",
	"Immediately takes you to the next level up or down.",	
	"Controlled displacement. Press 'p' when you enter targeting mode.",
	"Shakes the nearby dungeon, randomly swapping walls and floors.",
	"Recalls you to the town, or back into the dungeon.",
	/* Kelek's Grimoire of Power (sval 6) */
	"Detects all evil monsters, even invisible ones.",
	"Attempts to change a monster.",	
	"Powerful recharging spell.",		
		/*Repeat Genocide*/
	"Removes nearby monsters except uniques.",
	/* Tenser's transformations... (sval 7) */
	"Temporary heroism.",
	"Temporarily increases armour class by 50.",
	"Temporarily turns you invisible.",
	"Long-duration haste spell.",
	"Temporarily raises your AC by 100 and reduces all damage by 33%.",
	/* Raal's Tome of Destruction (sval 8) */
	"Fires a bolt or beam of pure mana.",
	"Fires a large poison ball.",
	"Fires a large acid ball.",
	"Fires a large frost ball.",
	"Fires a large, unresistable ball of magic.",
	"Fires a large, very powerful mana ball.",

/* Piety Spells */
	
	/* Novice's Handbook (sval 10) */
		/*repeat Detect Evil */
	"Reduces cuts and heals you a little.",
	"Short-duration bonus to fighting ability and armour class.",
	"Removes any fear you currently feel.",
		/*repeat Light Area*/
	"Detects all traps on the current panel.",
	"Detects all doors and stairs on the current panel.",
	"Reduces the amount of poison in your system.",
	/* Words of Wisdom (sval 11) */
	"Attempts to frighten one monster.",
	"Random medium-range displacement.",
    "Reduces cuts and heals you a moderate amount.",
	"Medium-duration bonus to fighting ability and armour class.",
	"Attempts to put all adjacent monsters to sleep.",
		/*repeat Satisfy Hunger */
	"Removes standard curses.",
	"Opposition to fire and frost.  Cumulative with equipment.",
	/* Chants and Blessings (sval 12) */
	"Removes all poison from your body.",
	"Fires an orb of holy force.",
	"Reduces cuts and heals you a large amount.",
	"Temporary see invisible.",
	"Temporary protection from lesser evil creatures.",
		/*repeat Earthquake */
	"Maps the local area.",
	"Eliminates cuts and heals you a very large amount.",
	"Attempts to make all undead monsters in line of sight flee.",
	/* Exorcism and Dispelling (sval 13) */
	"Long-duration bonus to fighting ability and armour class.",
	"Dispels all undead in line of sight.",
	"A large amount of healing, eliminates cuts and stunning.",	
	"Dispels all evil monsters in line of sight.",
	"Places a glyph on the floor that monsters cannot pass over.",
	"Strong dispel evil and healing.",
	/* Ethereal openings (sval 14) */
		/*repeat Phase Door */
	"Long-range random displacement.",
		/*repeat Teleport Other */
		/*repeat Teleport Level */
		/*repeat Word of Recall */
	"Regenerates the dungeon level.",
	/* Godly Insights... (sval 15) */
		/*repeat Detect Monsters */
	"Detects everything of interest on the panel.",
		/*repeat Identify */
	"Learns about a monster's attributes and resistances.",
	"Permanently light and detect objects on the entire level.",
	/* Purifications and Healing (sval 16) */
    "Eliminates cuts and heals you a moderate amount.",
		/*repeast Cure Mortal Wounds */
	"An extremely strong healing spell.  Removes cuts and stuns.",
	"Restores all stats.",
	"Restores experience level.",
	/* Holy Infusions (sval 17) */
		/*repeat destroy doors */
	"Medium strength recharging spell.",
	"Removes both normal and heavy curses.",
	"Adds plusses to Skill and Deadliness to weapons.",
	"Adds plusses to armour class to armour.",
	"Makes your shots powerful against evil creatures.",
	/* Wrath of God (sval 18) */
	"Dispels all undead in line of sight.",
	"Dispels all evil monsters in line of sight.",
	"Teleports away all evil monsters in line of sight.",
		/*repeat Word of Destruction */
	"A powerful drain life attack.",

/* Other Spells */
	/* The Codex of Ultimate Wisdom (sval 22)*/
	"Reveals all the magics that affect you.",
	"Reveals all information about a specific object.",
	/* The Necronomicon (sval 23)*/
	"Dispels all non-evil monsters in line of sight.",
	"Fires an ultra-powerful beam of nether. Does not affect undead.",

/* Mystic Spells */
	/* The seven steps (sval 21) */
	"Rids your mind of confusion and fear, cures blindness.",
		/*repeat Dimension door */
	"Opposition to all elements. Cumulative with equipment.",
		/*repeat Detection */
	"Restores all stats, cures poison, and fully feeds you.",
	"Resist all except disenchantment. Cumulative with resist spells.",
		/*repeat globe of resilence */

/* Ranger Spells */
	/* The Hunter's Lore (sval 9)*/
	"Makes arrows and bolts extra powerful against animals",
	"Strengthens arrows and bolts",
		/* repeat haster self */
	"Imbues arrows and bolts with elemental power"

};

/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(int book, int spell)
{
	int chance, minfail, stat_factor;

	magic_type *s_ptr;

	/* Paranoia -- must be literate */
	if (!literate()) return (100);

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - (s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)));
	
	/* Reduce failure rate by INT/WIS adjustment */
	stat_factor = (p_ptr->stat_ind[cp_ptr->spell_stat1] + p_ptr->stat_ind[cp_ptr->spell_stat2])/2;
	chance -= 3 * (adj_mag_stat[stat_factor] - 1);

	/* Not enough mana to cast */
	if (spell_mana(book, spell) > p_ptr->csp)
	{
		chance += 5 * (spell_mana(book, spell) - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[stat_factor];

	/* Non mage/priest/mystic characters never get better than 5 percent */
	if (!cp_ptr->flags & CF_ZERO_FAIL)
	{
		if (minfail < (cp_ptr->spell_handicap[book]+2)) minfail = (cp_ptr->spell_handicap[book]+2); 
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (p_ptr->icky_wield))
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder (after minfail) */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Returns mana cost for a spell
 */
s16b spell_mana(int book, int spell)
{
	int mana;

	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Extract the base spell failure rate */
	mana = s_ptr->smana;

	/* Calculate exact mana */

	mana += ((s_ptr->slevel/6) * (cp_ptr->spell_handicap[book] - 1));

	return mana;
}

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int book, int spell, bool known)
{
	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &books[book].contents[spell];

	/* Spell is illegal */
	if ((s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)) > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_forgotten[book] & (1L << spell))
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_learned[book] & (1L << spell))
		/* Okay to cast, not to study */
	{
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}

/*
 * Extra information on a spell		-DRS-
 *
 * We can use up to 20 characters of the buffer 'p'
 *
 * The strings in this function were extracted from the code in the
 * functions "do_cmd_cast()" and "do_cmd_pray()" and are up to date 
 * (as of 0.4.0). -LM-
 */
void spell_info(char *p, int spell_index)
{
	int plev = ((cp_ptr->flags & CF_POWER) 
		? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);

	int beam = ((cp_ptr->flags & CF_BEAM) 
		? plev : (plev / 2));
	int beam_low = (beam - 10 > 0 ? beam - 10 : 0);

	/* Default */
	strcpy(p, "");

	/* Analyze the spell */
	switch (spell_index)
	{
		/* Sorcery */

		case   1: sprintf(p, " dam %dd4, beam %d%%", 3+((plev-1)/5), beam_low); break;
		case   2: strcpy(p, " range 10"); break;
		case   3: sprintf(p, " dam 2d%d, rad %d", 
			(plev/2), ( plev / 10) + 1); break;
		case   5: sprintf(p, " dam %d, rad 2", 10 + (plev / 2)); break;
		case   8: sprintf(p, " dur %d+d25", plev); break;
		case   9: sprintf(p, " dam %dd8, beam %d%%", (3+((plev-5)/4)), beam_low); break;
		case  12: sprintf(p, " dam %dd8, beam %d%%", (5+((plev-5)/4)), beam_low); break;
		case  13: sprintf(p, " range %d", plev * 5); break;
		case  14: strcpy(p, " dam 9d8"); break;
		case  19: sprintf(p, " dam %dd8, beam %d%%", (6+((plev-5)/4)), beam); break;
		case  23: sprintf(p, " dam %dd8, beam %d%%", (8+((plev-5)/4)), beam); break;
		case  25: sprintf(p, " dam %d, rad 2", 30 + plev); break;
		case  28: sprintf(p, " dam %d, rad 2", 55 + plev); break;
		case  29: sprintf(p, " dur %d+d20", plev); break;
		case  30: strcpy(p, " rad 15"); break;
		case  31: strcpy(p, " hurt 1d4 per kill"); break;
		case  32: strcpy(p, " dur 20+d20"); break;
		case  33: strcpy(p, " dur 20+d20"); break;
		case  34: strcpy(p, " dur 20+d20"); break;
		case  35: strcpy(p, " dur 20+d20"); break;
		case  36: strcpy(p, " dur 20+d20"); break;
		case  39: strcpy(p, " range 20"); break;
		case  40: strcpy(p, " radius 10"); break;
		case  45: strcpy(p, " hurt 1d3 per kill"); break;
		case  46: strcpy(p, " dur 25+d25"); break;
		case  47: strcpy(p, " dur 30+d20"); break;
		case  48: strcpy(p, " dur 25+d25"); break;
		case  49: sprintf(p, " dur %d+d30", 30+plev); break;
		case  50: strcpy(p, " dur 8+d8"); break;
		case  51: sprintf(p, " dam %dd8, beam %d%%", (6+((plev-5)/4)), beam); break;
		case  52: sprintf(p, " dam %d, rad 3", 20 + (plev/2)); break;
		case  53: sprintf(p, " dam %d, rad 3", 40 + (plev)); break;
		case  54: sprintf(p, " dam %d, rad 3", 70 + (plev)); break;
		case  55: sprintf(p, " dam %d, rad 3", 65 + (plev)); break;
		case  56: sprintf(p, " dam %d, rad 3", 300 + (plev * 2)); break;

		/* Piety */
		
		case  57: strcpy(p, " heal 2d10"); break;
		case  58: strcpy(p, " dur 12+d12"); break;
		case  62: strcpy(p, " halve poison"); break;
		case  64: sprintf(p, " range %d", 3 * plev); break;
		case  65: strcpy(p, " heal 4d10"); break;
		case  66: strcpy(p, " dur 24+d24"); break;
		case  69: strcpy(p, " dur 10+d10"); break;
		case  71: sprintf(p, " dam %d+3d6, rad %d", plev + 
			(plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4)), 
			((plev >= 30) && (cp_ptr->flags & CF_BLESS_WEAPON)) ? 3 : 2); break;
		case  72: strcpy(p, " heal 6d10"); break;
		case  73: strcpy(p, " dur 24+d24"); break;
		case  74: sprintf(p, " dur %d+d25", 3*plev); break;
		case  76: strcpy(p, " heal 8d10, any cut"); break;
		case  78: strcpy(p, " dur 48+d48"); break;
		case  79: sprintf(p, " dam d%d", 3 * plev); break;
		case  80: strcpy(p, " heal 300, any cut"); break;
		case  81: sprintf(p, " dam d%d", 3 * plev); break;
		case  83: sprintf(p, " dam d%d, heal 1000", plev * 4); break;
		case  84: sprintf(p, " range %d", 8 * plev); break;
		case  89: strcpy(p, " heal 4d10, any cut"); break;
		case  90: strcpy(p, " heal 2000, any cut"); break;
		case  98: sprintf(p, " dam d%d", 4 * plev); break;
		case  99: sprintf(p, " dam d%d", 4 * plev); break;
		case 101: strcpy(p, " dam 200"); break;

		/* Artifact Spells */

		case 104: sprintf(p, " dam d%d", 5 * plev); break;
		case 105: sprintf(p, " dam %dd4", 8 * plev); break;

		/* Mystic Spells */
		case 107: sprintf(p, " dur %d+d%d", plev/2, plev/2); break;
		case 109: sprintf(p, " dur %d+d%d", plev/3, plev/3); break;

	}
}


/*
 * Print out a list of available spells for any spellbook given.
 * Revised by -LM-
 *
 * Input y controls lines from top for list, and input x controls columns 
 * from left. 
 */
void print_spells(int book, int y, int x)
{
	int i, left_justi;
	int j = 0;

	magic_type *s_ptr;

	byte attr_book, attr_name;

	cptr comment;
	char info[80];
	char out_val[160];

	object_kind *k_ptr = &k_info[lookup_kind(TV_MAGIC_BOOK, book)];

	cptr basenm = (k_name + k_ptr->name);

	/* Choose appropriate spellbook color. */
	attr_book = k_ptr->x_attr;

	/* Choose a left margin for the spellbook name. */
	left_justi = ((80 - x) - strlen(basenm)) / 2;

	/* Center the spellbook name */
	prt("", y, x);
	c_put_str(attr_book, format("%s", basenm), y, x + left_justi);

	/* Title the list */
	prt("", y + 1, x);
	put_str("Name", y + 1, x + 5);
	put_str("Lv Mana Fail Info", y + 1, x + 35);

	/* Dump the spells in the book. */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Get the spell */
		s_ptr = &books[book].contents[i];

		if (s_ptr->index == 0) continue;

		/* Increment the current line */
		j++;

		/* Skip illegible spells. */
		if ((s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)) >= 51)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + j + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, s_ptr->index);

		/* Use that info */
		comment = info;

		/* Analyze the spell */
		if (p_ptr->spell_forgotten[book] & (1L << i))
		{
			comment = " forgotten";
			attr_name = TERM_L_WHITE;
		}
		else if (!(p_ptr->spell_learned[book] & (1L << i)))
		{
			if ((s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)) <= p_ptr->lev)
			{
				comment = " unknown";
				attr_name = TERM_SLATE;
			}
			else
			{
				comment = " too high";
				attr_name = TERM_L_DARK;
			}
		}
		else if (!(p_ptr->spell_worked[book] & (1L << i)))
		{
			comment = " untried";
			attr_name = TERM_WHITE;
		}
		else 
		{
			/* Vivid color for known, cast spells */
			attr_name = attr_book;
		}

		/* Clear line */
		prt("", y + j + 1, x);

		/* Print out (colored) information about a single spell. */
		put_str(format("  %c) ", I2A(i)), y + j + 1, x);
		c_put_str(attr_name, format("%-30s", s_ptr->sname), 
			y + j + 1, x + 5);
		c_put_str(attr_name, format("%2d %4d %3d%%", (s_ptr->slevel+(cp_ptr->spell_handicap[book]-1)), 
			spell_mana(book, i), spell_chance(book, i)), y + j + 1, x + 35);
		c_put_str(attr_name, format("%s", comment), y + j + 1, x + 47);
	}

	/* Clear the bottom line */
	prt("", y + j + 2, x);
}

/* Checks to see if the character has access to any spell realms */

bool literate (void)
{
	int j;

	for (j = 0; j < SV_MAX_BOOKS; j++)
	{
		if (cp_ptr->spell_book[j]) return TRUE;  
	}
	return FALSE;
}

/* Count how many spells in spell book */
int count_spells (int book)
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

