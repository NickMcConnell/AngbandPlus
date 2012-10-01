/* File: cmd-book.c */

/*
 * Commands and routines that have to do with spellbooks. Combined from old cmd5.c and book.c
 *
 * Some of the code originally based on Oangband's info.c
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
 * Checks to see if the character has access to any spell books 
 */
bool literate(void)
{
	int j;

	for (j = 0; j < SV_BOOK_MAX; j++)
	{
		if (cp_ptr->spell_book[j]) return TRUE;  
	}
	return FALSE;
}

/* 
 * Checks to see if the character can cast spells
 */
bool spellcaster(void)
{
	if ((cp_ptr->flags & CF_MUSIC) || literate()) return TRUE;

	return FALSE;
}

/*
 * Returns mana cost for a spell
 */
static s16b spell_mana(int book, int spell, bool music)
{
	int mana, handicap;

	magic_type *s_ptr;

	/* Get the spell */
	if (!music) s_ptr = &books[book].contents[spell];
	else s_ptr = &instruments[book].contents[spell];

	/* Extract the base spell mana */
	mana = s_ptr->smana;

	/* Extract the base handicap */
	if (music) handicap = 0;
	else handicap = (cp_ptr->spell_handicap[book] - 1);

	/* Modify for handicap */
	mana += ((s_ptr->slevel/6) * handicap);

	return mana;
}

/*
 * Returns chance of failure for a spell
 */
static s16b spell_chance(int book, int spell, bool music)
{
	int chance, minfail;
	int handicap;
	byte stat_factor;

	magic_type *s_ptr;

	/* Paranoia -- must be able to cast spells */
	if (!spellcaster()) return (100);

	/* Get the spell */
	if (!music) s_ptr = &books[book].contents[spell];
	else s_ptr = &instruments[book].contents[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Extract handicap */
	if (!music) handicap = cp_ptr->spell_handicap[book]-1;
	else handicap = 0;
	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - (s_ptr->slevel+handicap));
	
	/* Reduce failure rate by stat adjustment */
	stat_factor = (p_stat(cp_ptr->spell_stat1) + p_stat(cp_ptr->spell_stat2))/2;
	chance -= 3 * (adj_mag_stat[stat_factor] - 1);

	/* Not enough mana to cast */
	if (spell_mana(book, spell, music) > p_ptr->csp)
	{
		chance += 5 * (spell_mana(book, spell, music) - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[stat_factor];

	/* Non mage/priest/mystic characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if ((cp_ptr->flags & CF_BLESS_WEAPON) && (p_ptr->icky_wield))
	{
		chance += 25;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Spell disruption (after minfail)*/
	if (p_ptr->disrupt) 
	{
		if (chance < 15) chance = 25;
		else chance += 10;
	}

	/* Gloves (after minfail)*/
	if (p_ptr->cumber_glove) 
	{
		if (chance < 10) chance = 25;
		else chance += 15;
	}

	/* Heavy armor */
	if (p_ptr->cumber_armor) 
	{
		if (p_ptr->cumber_armor < 60) chance += 1;
		if ((p_ptr->cumber_armor >= 60) && (p_ptr->cumber_armor < 90)) chance += 2;
		if ((p_ptr->cumber_armor >= 90) && (p_ptr->cumber_armor < 105)) chance += 3;
		if ((p_ptr->cumber_armor >= 105) && (p_ptr->cumber_armor < 120)) chance += 4;
		if (p_ptr->cumber_armor >= 120) chance += 5 + ((p_ptr->cumber_armor - 120) / 10);
	}

	/* Stunning makes spells harder */
	if (p_ptr->stun > PY_STUN_HEAVY) chance += 25;
	else if (p_ptr->stun) chance += 15;

	if (p_ptr->confused)	
	{
		/* Confusion makes spells harder */
		if (!(books[book].flags & SBF_MYSTIC) || !(cp_ptr->flags & CF_MYSTIC_CAST)) 
			chance += 20;
	}

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
static bool spell_okay(int book, int spell, bool known)
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
 * Determine if a tune is "okay" for the player to cast.
 */
static bool tune_okay(int instrument, int lev, int tune)
{
	magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &instruments[instrument].contents[tune];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	if (s_ptr->slevel > (lev * 10)) return (FALSE);

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
	/* Various class flags influence things */
	int	beam = ((cp_ptr->flags & CF_BEAM) ? p_ptr->lev : (p_ptr->lev / 2));
	int damlev = ((cp_ptr->flags & CF_POWER) ? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);
	int durlev = ((cp_ptr->flags & CF_POWER) ? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);
	bool holy = ((cp_ptr->flags & CF_BLESS_WEAPON) ? TRUE : FALSE);

	int beam_low = (beam - 10 > 0 ? beam - 10 : 0);

	/* Default */
	strcpy(p, "");

	/* XXX XXX Analyze the spell */
	switch (spell_index)
	{
		case POW_HEAL_1:
			strcpy(p, " heal 5%"); break;
		case POW_HEAL_2:
			strcpy(p, " heal 15%"); break;
		case POW_HEAL_3:
			strcpy(p, " heal 30%"); break;
		case POW_HEAL_4:
			strcpy(p, " heal 60%, any cut"); break;
		case POW_HEAL_5:
			strcpy(p, " heal 90%, any cut"); break;
		case POW_CURE_POISON_1:
			strcpy(p, " halve poison"); break;
		case POW_TELE_10: 
			strcpy(p, " range 10"); break;
		case POW_TELE_MINOR:
			sprintf(p, " range %d", 3 * damlev); break;
		case POW_TELE_CONTROL: 
			strcpy(p, " range 20"); break;
		case POW_TELE_MAJOR: 
			sprintf(p, " range %d", damlev * 5); break;
		case POW_BOLT_MISSILE_2: 
			sprintf(p, " dam %dd4", (3+((damlev-1)/5))); break;
		case POW_BOLT_ELEC: 
			sprintf(p, " dam %dd8, beam %d%%", (3 + ((damlev-5) / 4)), beam_low); break;
		case POW_BOLT_FROST_1: 
			sprintf(p, " dam %dd8, beam %d%%", (5 + ((damlev-5) / 4)), beam_low); break;
		case POW_BOLT_ACID_2: 
			sprintf(p, " dam %dd9, beam %d%%", (6 + ((damlev-5) / 4)), beam); break;
		case POW_BOLT_FIRE_2: 
			sprintf(p, " dam %dd9, beam %d%%", (7 + ((damlev-5) / 3)), beam); break;
		case POW_BOLT_SOUND: 
			sprintf(p, " dam %dd4, beam %d%%", (3 + ((damlev-1) / 5)), beam_low); break;
		case POW_BOLT_FORCE_1: 
			sprintf(p, " dam %dd8, beam %d%%", (2 + ((damlev-5) / 4)), beam); break;
		case POW_BOLT_MANA: 
			sprintf(p, " dam %dd8, beam %d%%", (6 + ((damlev-5) / 4)), beam); break;
		case POW_BEAM_WEAK_LITE: 
			strcpy(p, " dam 9d8"); break;
		case POW_BEAM_NETHER:
			sprintf(p, " dam %dd4", damlev * 8); break;
		case POW_BALL_POISON_1:
			sprintf(p, " dam %d, rad 2", 10 + (damlev / 2)); break;
		case POW_BALL_POISON_2: 
			sprintf(p, " dam %d, rad 3", 20 + damlev); break;
		case POW_BALL_FIRE_1: 
			sprintf(p, " dam %d, rad %d", 60 + damlev, (p_ptr->lev < 40) ? 2 : 3); break;
		case POW_BALL_FIRE_2: 
			sprintf(p, " dam %d, rad %d", 100 + (damlev * 2), (p_ptr->lev < 40) ? 3 : 4); break;
		case POW_BALL_FROST_1: 
			sprintf(p, " dam %d, rad %d", 35 + damlev, (p_ptr->lev < 35) ? 2 : 3); break;
		case POW_BALL_FROST_2: 
			sprintf(p, " dam %d, rad %d", 60 + (damlev * 2), (p_ptr->lev < 35) ? 3 : 4); break;
		case POW_BALL_SOUND: 
			sprintf(p, " dam %d, rad 2", 30 + damlev); break;
		case POW_BALL_MANA: 
			sprintf(p, " dam %d, rad 3", 300 + (damlev * 2)); break;
		case POW_BALL_HOLY:
			{
				int x = (p_ptr->lev + (p_ptr->lev / ((holy) ? 2 : 4)));
				int y = (((p_ptr->lev >= 30) && (holy)) ? 3 : 2);
				sprintf(p, " dam %d+3d6, rad %d", x, y);
				break;
			}
		case POW_BURST_ASTRAL: 
			sprintf(p, " dam 25%%"); break;
		case POW_DRAIN_LIFE_3:
			strcpy(p, " dam 200"); break;
		case POW_BLIGHT: 
			sprintf(p, " dam %d", damlev * 8); break;
		case POW_DISPEL_UNDEAD_1:
			sprintf(p, " dam d%d", 3 * damlev); break;
		case POW_DISPEL_UNDEAD_2:
			sprintf(p, " dam d%d", 4 * damlev); break;
		case POW_DISPEL_NON_EVIL:
			sprintf(p, " dam d%d", 5 * damlev); break;
		case POW_DISPEL_EVIL_3:
			sprintf(p, " dam d%d", 3 * damlev); break;
		case POW_DISPEL_EVIL_4:
			sprintf(p, " dam d%d", 4 * damlev); break;
		case POW_HOLY_2:
			sprintf(p, " dam d%d, heal 1000", damlev * 4); break;
		case POW_GENOCIDE: 
			strcpy(p, " hurt 1d4 per kill"); break;
		case POW_MASS_GENOCIDE: 
			strcpy(p, " hurt 1d3 per kill"); break;
		case POW_EARTHQUAKE: 
			strcpy(p, " rad 10"); break;
		case POW_DESTRUCTION: 
			strcpy(p, " rad 15"); break;
		case POW_LIGHT_AREA:
			sprintf(p, " dam 2d%d, rad %d", (damlev / 2), (damlev / 10) + 1); break;
		case POW_DARK_AREA:
			sprintf(p, " dam 2d%d, rad %d", (damlev / 2), (damlev / 10) + 1); break;
		case POW_ABSORB_HIT: 
			sprintf(p, " dur %d+d32", durlev * 2); break;
		case POW_BLESS_1:
			strcpy(p, " dur 12+d12"); break;
		case POW_BLESS_2:
			strcpy(p, " dur 24+d24"); break;
		case POW_BLESS_3:
			strcpy(p, " dur 48+d48"); break;
		case POW_HEROISM: 
			strcpy(p, " dur 25+d25"); break;
		case POW_RAGE_1: 
			strcpy(p, " dur 25+d25"); break;
		case POW_SHIELD: 
			strcpy(p, " dur 30+d20"); break;
		case POW_INVIS_2: 
			strcpy(p, " dur 25+d25"); break;
		case POW_RESILIENCE: 
			strcpy(p, " dur 8+d8"); break;
		case POW_SEE_INVIS:
			strcpy(p, " dur 24+d24"); break;
		case POW_PROT_EVIL:
			sprintf(p, " dur %d+d25", durlev * 3); break;
		case POW_MAGIC_LOCK: 
			strcpy(p, " rad 3"); break;
		case POW_HASTE_SELF_1: 
			sprintf(p, " dur %d+d20", durlev); break;
		case POW_HASTE_SELF_2	: 
			sprintf(p, " dur %d+d30", durlev + 30); break;
		case POW_RES_FIRE: 
			strcpy(p, " dur 20+d20"); break;
		case POW_RES_COLD: 
			strcpy(p, " dur 20+d20"); break;
		case POW_RES_FIRE_COLD:
			strcpy(p, " dur 10+d10"); break;
		case POW_RES_ACID_ELEC: 
			strcpy(p, " dur 20+d20"); break;
		case POW_RES_POISON: 
			strcpy(p, " dur 20+d20"); break;
		case POW_RES_DISEASE: 
			strcpy(p, " dur 20+d20"); break;
		case POW_RES_SOUND: 
			strcpy(p, " dur 40+d40"); break;
		case POW_RES_ELEMENTS:
			sprintf(p, " dur %d+d%d", durlev / 2, durlev / 2); break;
		case POW_RES_GREATER:
			sprintf(p, " dur %d+d%d", durlev / 3, durlev / 3); break;
		case POW_RESISTANCE: 
			strcpy(p, " dur 20+d20"); break;
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
void print_spells(int book, bool music, int lev, int y, int x)
{
	int i, left_justi;
	int j = 0;
	int handicap;

	magic_type *s_ptr;

	byte attr_book, attr_name;

	cptr comment;
	char info[80];
	char out_val[160];

	object_kind *k_ptr;
	cptr basenm;

	if (!music)
	{
		k_ptr = &k_info[lookup_kind(TV_MAGIC_BOOK, book)];
		basenm = k_name + k_ptr->name;
	}
	else
	{
		k_ptr = &k_info[lookup_kind(TV_MUSIC, book)];
	}

	/* Choose appropriate spellbook color. */
	attr_book = k_ptr->d_attr;

	if (!music)
	{
		/* Choose a left margin for the spellbook name. */
		left_justi = ((80 - x) - strlen(basenm)) / 2;

		/* Center the spellbook name */
		prt("", y, x);
		c_put_str(attr_book, format("%s", basenm), y, x + left_justi);
	}

	/* Title the list */
	prt("", y + 1, x);
	put_str("Name", y + 1, x + 5);
	put_str("Lv Mana Fail Info", y + 1, x + 35);

	/* Calculate handicap */
	if (!music) handicap = cp_ptr->spell_handicap[book]-1;
	else handicap = 0;

	/* Dump the spells in the book. */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Get the spell */
		if (!music) s_ptr = &books[book].contents[i];
		else s_ptr = &instruments[book].contents[i];

		if (s_ptr->index == 0) continue;

		/* Skip tunes if higher than the instrument level */
		if ((music) && (s_ptr->slevel > (lev * 10)))	continue;

		/* Increment the current line */
		j++;

		/* Skip illegible spells. */
		if ((s_ptr->slevel + handicap) >= 51)
		{
			sprintf(out_val, "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, y + j + 1, x);
			continue;
		}

		/* Get extra info */
		spell_info(info, s_ptr->index);

		/* Use that info */
		comment = info;

		/* Vivid color for known, cast spells */
		attr_name = attr_book;

		/* Analyze the spell */
		if (!music)
		{
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
		}
		else
		{
			if (s_ptr->slevel > p_ptr->lev)
			{
				comment = " too high";
				attr_name = TERM_L_DARK;
			}
		}

		/* Clear line */
		prt("", y + j + 1, x);

		/* Print out (colored) information about a single spell. */
		put_str(format("  %c) ", I2A(i)), y + j + 1, x);
		c_put_str(attr_name, format("%-30s", s_ptr->sname), 
			y + j + 1, x + 5);
		c_put_str(attr_name, format("%2d %4d %3d%%", (s_ptr->slevel+handicap), 
			spell_mana(book, i, music), spell_chance(book, i, music)), y + j + 1, x + 35);
		c_put_str(attr_name, format("%s", comment), y + j + 1, x + 47);
	}

	/* Clear the bottom line */
	prt("", y + j + 2, x);
}

/* Count how many spells in spell book */
static byte count_tunes(int instrument, int lev)
{
	int j, count;
	magic_type *s_ptr;

	count = 0;

	for (j=0; j < MAX_BOOK_SPELLS; j++)
	{
		s_ptr = &instruments[instrument].contents[j];
		if ((s_ptr->index > 0) && (s_ptr->slevel <= lev * 10)) count++;
	}

	return count;
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
static int get_spell(int *sn, cptr prompt, int book, bool known)
{
	int i;

	int spell = -1;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (spell_okay(book, *sn, known)) 
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(book, i, known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

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
				print_spells(book, FALSE, 0, 1, 14);
			}

			/* Ask again */
			continue;
		}

		/* Note verify */
		ver = (isupper(choice));

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
		if (!spell_okay(book, spell, known))
		{
			bell("Illegal spell choice!");
			message_format(MSG_FAIL, 0, "You may not %s that spell.", prompt);
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &books[book].contents[i];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, s_ptr->sname,
			        spell_mana(book, spell, FALSE), spell_chance(book, spell, FALSE));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
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

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}

/*
 * Get a spell out of a book/instrument
 */
static int get_tune(int *sn, cptr prompt, int instrument, int lev)
{
	int i;

	int tune = -1;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Verify the spell is okay */
		if (tune_okay(instrument, lev, *sn)) 
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT */

	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < MAX_BOOK_SPELLS; i++)
	{
		/* Look for "okay" spells */
		if (tune_okay(instrument, lev, i)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(spells %c-%c, *=List, ESC=exit) %^s which tune? ",
		I2A(0), I2A(count_tunes(instrument, lev)-1), prompt);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
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
				print_spells(instrument, TRUE, lev, 1, 14);
			}

			/* Ask again */
			continue;
		}

		/* Note verify */
		ver = (isupper(choice));

		/* Lowercase */
		choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= MAX_BOOK_SPELLS))
		{
			bell("Illegal tune choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		tune = i;

		if (!tune_okay(instrument, lev, tune))
		{
			bell("Illegal tune choice!");
			message_format(MSG_FAIL, 0, "You may not %s that tune.", prompt);
			continue;
		}

		/* Verify it */
		if (ver)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &instruments[instrument].contents[i];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ", prompt, s_ptr->sname,
			        spell_mana(instrument, tune, TRUE), spell_chance(instrument, tune, TRUE));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
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
	(*sn) = tune;

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);
#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}

/*
 *  Actually browse a spellbook
 */
static void do_browse_instrument(int instrument, int lev)
{
	int tune, lines, j;

	magic_type *s_ptr;

	/* Display the spells */
	print_spells(instrument, TRUE, lev, 1, 14);

	/* Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_tunes(instrument, lev);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_tune(&tune, "browse", instrument, lev))
		{
			/* If cancelled, leave immediately. */
			if (tune == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No tunes to browse     ", 0, 0);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 3, 255);

		/* Access the spell */
		s_ptr = &instruments[instrument].contents[tune];

		for (j = (POW_MAX - 1); j > 0; j--) 
		{
			if (power_info[j].index == s_ptr->index) break;
		}

		/* Output to the screen */
		text_out_hook = text_out_to_screen;

		/* Display that spell's information. */
		if (power_info[j].desc != NULL) text_out_c(TERM_L_BLUE, format("%^s.",power_info[j].desc));
	}
}

/*
 *  Actually browse a spellbook
 */
static void do_browse_book(int book)
{
	int spell, lines, j;

	magic_type *s_ptr;

	/* Display the spells */
	print_spells(book, FALSE, 0, 1, 14);

	/* 
	 * Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = count_spells(book);

	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", book, TRUE))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No spells to browse     ", 0, 0);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
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
		if (power_info[j].desc != NULL) text_out_c(TERM_L_BLUE, format("%^s.",power_info[j].desc));
	}
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

	if (!spellcaster())
	{
		message(MSG_FAIL, 0, "You know no magic!");
		return;
	}

	item_tester_hook = item_tester_hook_bookmusic;

	/* Get an item */

	/* Can read books, can't use instruments */
	if (literate() && (!(cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which book? ", "You have no books that you can browse.", 
		(USE_INVEN | USE_FLOOR))) return;

	/* Can use both instruments and books */
	if (literate() && ((cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which book or musical instrument? ", 
			"You have no books or musical instruments that you can browse.", 
			(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

	/* Can use instruments, can't use books */
	if (!literate() && ((cp_ptr->flags & CF_MUSIC))) 
		if (!get_item(&item, "Browse which musical instrument? ", 
			"You have no musical instruments that you can browse.", 
			(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

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
	object_actual_track(o_ptr);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Save screen */
	screen_save();
	
	if (o_ptr->tval == TV_MAGIC_BOOK) do_browse_book(o_ptr->sval);
	else do_browse_instrument(o_ptr->sval, o_ptr->pval);

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
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;

	magic_type *s_ptr;

	int spell = -1;

	object_type *o_ptr;

	/* Forbid illiterates to read spellbooks. */
	if (!literate())
	{
		message(MSG_FAIL, 0, "You cannot learn magic!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		message(MSG_FAIL, 0, "You cannot see!");
		return;
	}

	if (p_ptr->confused)
	{
		message(MSG_FAIL, 0, "You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		message(MSG_FAIL, 0, "You cannot learn any new spells.");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_hook = item_tester_hook_spellbooks;

	/* Get an item */
	if (!get_item(&item, "Study which book? ", "You have no books that you can read."
		, (USE_INVEN | USE_FLOOR))) return;

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
	object_actual_track(o_ptr);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* All but Priests -- Learn a selected spell */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr->sval, FALSE) && (spell == -1)) return;
	}
	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Pick an legal, unknown prayer at random. */
		for (spell = 0; spell < MAX_BOOK_SPELLS; spell++)
		{
			/* Skip non "okay" prayers */
			if (!spell_okay(o_ptr->sval, spell, FALSE)) continue;

			/* Apply the randomizer */
			if ((++k > 1) && (rand_int(k) != 0)) continue;

			/* Track it */
			gift = spell;
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		message(MSG_FAIL, 0, "You cannot learn any spells in that book.");

		/* Abort */
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Learn the spell */
	p_ptr->spell_learned[o_ptr->sval] |= (1L << spell);

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < (SV_BOOK_MAX * MAX_BOOK_SPELLS); i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i][1] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i][0] = o_ptr->sval;
	p_ptr->spell_order[i][1] = spell;

	/* Access the spell */
	s_ptr = &books[o_ptr->sval].contents[spell];

	/* Mention the result */
	message_format(MSG_STUDY, TRUE, "You have learned the spell of %s.",
	           s_ptr->sname);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		message_format(MSG_STUDY, FALSE, "You can learn %d more spell%s.", p_ptr->new_spells, 
			(p_ptr->new_spells != 1)  ? "s" : "");
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}

/*
 * Actual spell effect 
 */
static bool aux_spell_cast(int index)
{
	/* Various class flags influence things */
	int	beam = ((cp_ptr->flags & CF_BEAM) ? p_ptr->lev : (p_ptr->lev / 2));
	int damlev = ((cp_ptr->flags & CF_POWER) ? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);
	int durlev = ((cp_ptr->flags & CF_POWER) ? p_ptr->lev + (p_ptr->lev / 2) : p_ptr->lev);
	int inflev = ((cp_ptr->flags & CF_INFLUENCE) ? p_ptr->lev + (p_ptr->lev / 2) :
					((cp_ptr->flags & CF_POWER) ? p_ptr->lev + (p_ptr->lev / 4) : p_ptr->lev));
	bool ignore_me;

	/* A spell was cast */
	return do_power(index, 0, beam, damlev, durlev, inflev, &ignore_me);
}

/*
 * Cast a spell or pray a prayer.
 */
static void do_cast_or_pray(int book)
{
	int spell;
	int chance;

	magic_type *s_ptr;

	/* Ask for a spell */
	if (!get_spell(&spell, "cast", book, TRUE))
	{
		if (spell == -2) 
		{
			message(MSG_FAIL, 0, "You don't know any spells in that books.");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &books[book].contents[spell];

	/* Verify "dangerous" spells */
	if (spell_mana(book, spell, FALSE) > p_ptr->csp)
	{
		/* Warning */
		message(MSG_GENERIC, 0, "You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell_chance(book, spell, FALSE);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		switch (books[book].flags & SBF_TYPE_MASK) 
		{
			case SBF_MAGIC:
			{
				message(MSG_SPELL_FAIL, 0, "You fail to tap onto the necessary magical forces!");
				break;
			}
			case SBF_PRAYER:
			{
				message(MSG_SPELL_FAIL, 0, "Your prayer was left unanswered!");
				break;
			}
			case SBF_MYSTIC:
			{
				message(MSG_SPELL_FAIL, 0, "You lose your concentration!");
				break;
			}
			case SBF_CODEX:
			{
				message(MSG_SPELL_FAIL, 0, 
					"Your mind is overwhelmed by the magnitude of ancient mystery!");
				/* Lose your spell-casting stats */
				if (rand_int(100) < chance) 
				{ 
					if (rand_int(2) == 0) 
						do_dec_stat(cp_ptr->spell_stat1, randint(2) + 1, TRUE, FALSE);
					else do_dec_stat(cp_ptr->spell_stat2, randint(2) + 1, TRUE, FALSE);
				}
				/* Lose your memories */
				lose_all_info();
				break;
			}
			case SBF_NECRONOM:
			{
				message(MSG_SPELL_FAIL, 0, 
					"You lost your grasp on the evil powers that you had sought to control!");
				/* Summon some horrors */
				summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth+10, SUMMON_HORROR);
				/* Darkness */
				if (!p_ptr->no_blind)
				{
					set_blind(p_ptr->blind + 3 + randint(5));
				}
				unlite_area(10, 3);
				/* Lose EXP */
				if (p_ptr->hold_life && (rand_int(100) < (100-chance)))
				{
					message(MSG_RESIST, 0, "You keep hold of your life force!");
				}
				else if (p_ptr->hold_life)
				{
					message(MSG_EFFECT, 0, "You feel your life slipping away!");
					lose_exp(200 + (p_ptr->exp/250));
				}
				else
				{
					message(MSG_EFFECT, 0, "You feel your life draining away!");
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
		if (!aux_spell_cast(s_ptr->index)) return;

		/* A spell was cast or a prayer prayed */
		if (!(p_ptr->spell_worked[book] & (1L << spell)))
		{
			int e = s_ptr->sexp;

			/* The spell or prayer worked */
			p_ptr->spell_worked[book] |= (1L << spell);

			/* Gain experience */
			gain_exp(e * (s_ptr->slevel + (cp_ptr->spell_handicap[book]-1)));
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell_mana(book, spell, FALSE) <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(book, spell, FALSE);
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_mana(book, spell, FALSE) - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		message(MSG_EFFECT, 0, "You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			message(MSG_EFFECT, 0, "You have damaged your health!");

			/* Reduce constitution */
			(void)do_dec_stat(A_CON, randint(2), perm, FALSE);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

/*
 * Play a tune
 */
static void do_play(int instrument, int lev)
{
	int tune;
	int chance;

	magic_type *s_ptr;

	/* Ask for a spell */
	if (!get_tune(&tune, "play", instrument, lev))
	{
		if (tune == -2) 
		{
			message(MSG_FAIL, 0, "You can't play any tunes with this instrument.");
		}
		return;
	}

	/* Access the spell */
	s_ptr = &instruments[instrument].contents[tune];

	/* Verify "dangerous" spells */
	if (spell_mana(instrument, tune, TRUE) > p_ptr->csp)
	{
		/* Warning */
		message(MSG_GENERIC, 0, "You do not have enough mana to play this tune.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell_chance(instrument, tune, TRUE);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		message(MSG_SPELL_FAIL, 0, "Your tune falls flat!");
	}

	/* Process spell */
	else
	{
		/* Allow cancelling directional spells */
		if (!aux_spell_cast(s_ptr->index)) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell_mana(instrument, tune, TRUE) <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell_mana(instrument, tune, TRUE);
	}

	/* Over-exert the player */
	else
	{
		int oops = spell_mana(instrument, tune, TRUE) - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		message(MSG_EFFECT, 0, "You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage DEX (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			message(MSG_EFFECT, 0, "You overtax your muscles!");

			/* Reduce constitution */
			(void)do_dec_stat(A_DEX, randint(2), perm, FALSE);
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

	object_type *o_ptr;

	if (!spellcaster())
	{
		message(MSG_FAIL, 0, "You know no magic!");
		return;
	}

	if (p_ptr->confused > PY_CONF_CONFUSE)
	{
		if (!(cp_ptr->flags & CF_MYSTIC_CAST))
		{
			message(MSG_FAIL, 0, "You are too confused!");
			return;
		}
	}

	if (p_ptr->blind || no_lite())
	{
		if ((!(cp_ptr->flags & CF_MYSTIC_CAST)) && (!(cp_ptr->flags & CF_MUSIC)))
		{
			message(MSG_FAIL, 0, "You cannot see!");
			return;
		}
	}

	/* Handle player fear */
	if (p_ptr->afraid > PY_FEAR_PANIC)
	{
		/* Message */
		message(MSG_FAIL, 0, "You are too afraid!");

		/* Done */
		return;
	}
	
	/* Get an item */
	item_tester_hook = item_tester_hook_bookmusic;

	/* Can use instruments, can't use books */
	if (!literate() && ((cp_ptr->flags & CF_MUSIC))) 
	{
		o_ptr = &inventory[INVEN_MUSIC];
		if (!o_ptr->tval)
		{
			message(MSG_FAIL, 0, "You have nothing to play tunes with.");
			return;
		}

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;
	}
	else
	{
		/* Can read books, can't use instruments */
		if (literate() && (!(cp_ptr->flags & CF_MUSIC))) 
			if (!get_item(&item, "Use which book? ", "You have no books that you can use.", 
			(USE_INVEN | USE_FLOOR))) return;

		/* Can use both instruments and books */
		if (literate() && ((cp_ptr->flags & CF_MUSIC))) 
			if (!get_item(&item, "Use which book or musical instrument? ", 
				"You have no books or musical instruments that you can use.", 
				(USE_INVEN | USE_FLOOR | USE_EQUIP))) return;

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
	}

	/* Track the object kind */
	object_actual_track(o_ptr);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (o_ptr->tval == TV_MAGIC_BOOK) do_cast_or_pray(o_ptr->sval);
	else do_play(o_ptr->sval, o_ptr->pval);
}
