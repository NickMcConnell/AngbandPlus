/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Returns chance of failure for a spell
 */
s16b spell_chance(int spell)
{
	int chance, minfail;

	const magic_type *s_ptr;


	/* Paranoia -- must be literate */
	if (!cp_ptr->spell_book) return (100);

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - s_ptr->slevel);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[SPELL_STAT_SLOT];

	/* Not enough mana to cast */
	if (s_ptr->smana > p_ptr->csp)
	{
		chance += 5 * (s_ptr->smana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[SPELL_STAT_SLOT];

	/* Non mage/priest characters never get better than 5 percent */
	if (!(cp_ptr->flags & CF_ZERO_FAIL))
	{
		if (minfail < 5) minfail = 5;
	}

	/* Priest prayer penalty for "edged" weapons (before minfail) */
	if (p_ptr->icky_wield)
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
 * Determine if a spell is "okay" for the player to cast or study
 * The spell must be legible, not forgotten, and also, to cast,
 * it must be known, and to study, it must not be known.
 */
bool spell_okay(int spell, bool known)
{
	const magic_type *s_ptr;

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Spell is illegal */
	if (s_ptr->slevel > p_ptr->lev) return (FALSE);

	/* Spell is forgotten */
	if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is ironman */
	if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
	{
		/* Never okay */
		return (FALSE);
	}

	/* Spell is learned */
	if (p_ptr->spell_flags[spell] & PY_SPELL_LEARNED)
	{
		/* Okay to cast, not to study */
		return (known);
	}

	/* Okay to study, not to cast */
	return (!known);
}

/*
 * Return the player realm for the spell_list table.  Assumes player is a spellcaster.
 * We don't return any error because this value is going to be looked up in a table,
 * & would cause the game to crash
 */
int get_player_spell_realm(void)
{
	/* Mage or priest spells? */
	if (cp_ptr->spell_book == TV_MAGIC_BOOK) 	return (MAGE_REALM);
	if (cp_ptr->spell_book == TV_PRAYER_BOOK)	return (PRIEST_REALM);
	/*Druid Book*/								return (DRUID_REALM);
}


static int get_spell_index(const object_type *o_ptr, int index)
{

	/* Get the item's sval */
	int sval = o_ptr->sval;

	int realm;

	/* Check bounds */
	if ((index < 0) || (index >= SPELLS_PER_BOOK)) return (-1);
	if ((sval < 0) || (sval >= BOOKS_PER_REALM)) return (-1);

	/*Get the right spell realm*/
	realm = get_player_spell_realm();

	return spell_list[realm][sval][index];
}


/*
 * Print a list of spells (for browsing or casting or viewing).
 */
void print_spells(const byte *spells, int num, int row, int col)
{
	int i, spell;

	const magic_type *s_ptr;

	cptr comment;

	char out_val[500];

	byte line_attr;

	int length;

	int wrap = Term->wid - col;

	/* The length of the spell name, level, and mana info*/
	int indent = 48;

	/* Title the list */
	prt("", row, col);
	put_str("Name", row, col + 5);
	put_str("Lv Mana Fail Info", row, col + 35);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		int use_indent = 0;

		/* Get the spell index */
		spell = spells[i];

		/* Get the spell info */
		s_ptr = &mp_ptr->info[spell];

		/* Skip illegible spells */
		if (s_ptr->slevel >= 99)
		{
			strnfmt(out_val, sizeof(out_val), "  %c) %-30s", I2A(i), "(illegible)");
			c_prt(TERM_L_DARK, out_val, row + i + 1, col);
			continue;
		}

		/* Get extra info */
		switch (cp_ptr->spell_book)
		{
			case TV_MAGIC_BOOK:
			{
				comment = do_mage_spell(MODE_SPELL_DESC, spell);
				break;
			}
			case TV_PRAYER_BOOK:
			{
				comment = do_priest_prayer(MODE_SPELL_DESC, spell);
				break;
			}
			case TV_DRUID_BOOK:
			{
				comment = do_druid_spell(MODE_SPELL_DESC, spell);
				break;
			}
			/*Oops*/
			default: comment = "Error!";
		}

		/* Assume spell is known and tried */
		line_attr = TERM_WHITE;

		/* Analyze the spell */
		if (p_ptr->spell_flags[spell] & PY_SPELL_IRONMAN)
		{
			comment = "Ironman Spell";
			line_attr = TERM_L_RED;
		}
		/* Analyze the spell */
		else if (p_ptr->spell_flags[spell] & PY_SPELL_FORGOTTEN)
		{
			comment = "forgotten";
			line_attr = TERM_YELLOW;
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_LEARNED))
		{
			if (s_ptr->slevel <= p_ptr->lev)
			{
				comment = "unknown";
				line_attr = TERM_L_BLUE;
			}
			else
			{
				comment = "difficult";
				line_attr = TERM_RED;
			}
		}
		else if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			comment = "untried";
			line_attr = TERM_L_GREEN;
		}

		/* Dump the spell --(-- */
		strnfmt(out_val, sizeof(out_val), "  %c) %-30s%2d %4d %3d%% %s",
		        I2A(i), get_spell_name(cp_ptr->spell_book, spell),
		        s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);

		length = strlen(out_val);

		/* Break up long notes */
		if ((length > wrap) && (indent < wrap))
		{
			int startpoint = 0;
			int endpoint, n, x;

			while (TRUE)
			{
				char partial_out_val[200];

				/* Don't print more than the set linewrap amount */
				endpoint = startpoint + wrap - use_indent - 1;

				/* The remaining portion of the string fits in the screen */
				if (endpoint > (length - 1))
				{
					/* Print all */
				       	endpoint = (length - 1);
				}
				/* We are in the middle of a word */
				else if (out_val[endpoint + 1] != ' ')
				{
					/* Find a breaking point */
					while ((endpoint > startpoint) &&
						(out_val[endpoint] != ' ') &&
						(out_val[endpoint] != '-'))
					{
						--endpoint;
					}

					/* No spaces in the line, so break in the middle of text */
					if (endpoint == startpoint)
					{
						endpoint = startpoint + wrap - use_indent - 1;
					}
				}

				/* Write that line to file */
				for (x = 0; x < use_indent; x++)
				{
					/* Insert Blank Space */
					partial_out_val[x] = ' ';
				}

				/* Write that line to file */
				for (n = startpoint; n <= endpoint; x++, n++)
				{
					/* Ensure the character is printable */
					partial_out_val[x] = (isprint(out_val[n]) ? out_val[n] : ' ');

				}

				/* Terminate */
				partial_out_val[x] = '\0';

				c_prt(line_attr, partial_out_val, row + i + 1, col);

				/* Prepare for the next line */
				startpoint = endpoint + 1;

				/* Trim whitespace */
				while (out_val[startpoint] == ' ') ++startpoint;

				/* We reached the end of the string so this spell is done */
				if (!out_val[startpoint]) break;

				/* Indent from now on */
				use_indent = indent;

				/* Prepare for the next line */
				++row;
			}

		}

 	 	/* Add note to buffer */
 	 	else
		{
			c_prt(line_attr, out_val, row + i + 1, col);
		}

	}

	/* Clear the bottom line */
	prt("", row + i + 1, col);
}


/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readible books
 */
void display_koff(int k_idx)
{
	int y;

	object_type *i_ptr;
	object_type object_type_body;

	char o_name[80];


	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!k_idx) return;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare the object */
	object_wipe(i_ptr);

	/* Prepare the object */
	object_prep(i_ptr, k_idx);


	/* Describe */
	object_desc_spoil(o_name, sizeof(o_name), i_ptr, FALSE, 0);

	/* Mention the object name */
	Term_putstr(0, 0, -1, TERM_WHITE, o_name);


	/* Warriors are illiterate */
	if (!cp_ptr->spell_book) return;

	/* Display spells in readible books */
	if (i_ptr->tval == cp_ptr->spell_book)
	{
		int i;
		int spell;
		int num = 0;

		byte spells[PY_MAX_SPELLS];


		/* Extract spells */
		for (i = 0; i < SPELLS_PER_BOOK; i++)
		{
			spell = get_spell_index(i_ptr, i);

			/* Collect this spell */
			if (spell >= 0) spells[num++] = spell;
		}

		/* Print spells */
		print_spells(spells, num, 2, 0);
	}
}

cptr get_spell_name(int tval, int spell)
{
	if (tval == TV_MAGIC_BOOK)
		return do_mage_spell(MODE_SPELL_NAME, spell);
	else if (tval == TV_PRAYER_BOOK)
		return do_priest_prayer(MODE_SPELL_NAME, spell);
	/*TV_DRUID_BOOK*/
	else return do_druid_spell(MODE_SPELL_NAME, spell);
}



/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * Returns -1 if the user hits escape.
 * Returns -2 if there are no legal choices.
 * Returns a valid spell otherwise.
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int get_spell(const object_type *o_ptr, cptr prompt, bool known)
{
	int i;

	int spell;
	int num = 0;

	byte spells[PY_MAX_SPELLS];

	bool verify;

	bool flag, redraw, okay;
	char choice;

	const magic_type *s_ptr;

	char out_val[160];

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

	int result;

	/* Get the spell, if available */
	if (repeat_pull(&result))
	{
		/* Verify the spell */
		if (spell_okay(result, known))
		{
			/* Success */
			return (result);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		spell = get_spell_index(o_ptr, i);

		/* Collect this spell */
		if (spell != -1) spells[num++] = spell;
	}

	/* Assume no usable spells */
	okay = FALSE;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(spells[i], known)) okay = TRUE;
	}

	/* No available spells */
	if (!okay) return (-2);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(num - 1), prompt, p);

	/* Option -- automatically show lists */
	if (auto_display_lists)
	{
		/* Show list */
		redraw = TRUE;

		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(spells, num, 2, 2);
	}

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
				print_spells(spells, num, 2, 2);
			}

			/* Ask again */
			continue;
		}


		/* Note verify */
		verify = (isupper((unsigned char)choice) ? TRUE : FALSE);

		/* Lowercase */
		choice = tolower((unsigned char)choice);

		/* Extract request */
		i = (islower((unsigned char)choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Save the spell index */
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known))
		{
			bell("Illegal spell choice!");
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (verify)
		{
			char tmp_val[160];

			/* Get the spell */
			s_ptr = &mp_ptr->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, get_spell_name(cp_ptr->spell_book, spell),
			        s_ptr->smana, spell_chance(spell));

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
	if (!flag) return (-1);



	repeat_push(spell);



	/* Success */
	return (spell);
}


static int beam_chance(void)
{
	int plev = p_ptr->lev;
	return ((cp_ptr->flags & CF_BEAM) ? plev : (plev / 2));
}


/*
 * Cast a spell, or output spell info, description.
 */
cptr do_druid_spell(int mode, int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Casting variables */
	int dir;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1;
	int dice, sides;
	int rad;

	/* Function modes */
	bool cast = (mode == MODE_SPELL_CAST);
	bool name = (mode == MODE_SPELL_NAME);
	bool desc = (mode == MODE_SPELL_DESC);

	/* Spells. */
	switch (spell)
	{
		case DRUID_ACID_BOLT:
		{
			dice = 3 + ((plev - 1) / 5);
			sides = 5;

			if (name) return ("Acid bolt");
			if (desc) return (format("Fires a bolt of acid for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt(GF_ACID, dir, damroll(dice, sides));
			}

			break;
		}

		case DRUID_CURE_LIGHT_WOUNDS:
		{
			dice = 2;
			sides = 10;

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				set_cut(p_ptr->cut - 15);
			}
			break;
		}

		case DRUID_DETECT_LIFE:
		{
			if (name) return ("Detect Life");
			if (desc) return ("Detects nearby living monsters.");
			if (cast)
			{
				(void)detect_monsters_living();
			}

			break;
		}

		case DRUID_CALL_LIGHT:
		{
			dice = 2;
			sides = plev / 2;
			rad = (plev / 10) + 1;

			if (name) return ("Call Light");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (cast)
			{
				(void)lite_area(damroll(dice, sides), rad);
			}

			break;
		}

		case DRUID_FIND_TRAPS_DOORS:
		{
			if (name) return ("Find Hidden Traps/Doors");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (cast)
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}

			break;
		}

		case DRUID_SLOW_POISON:
		{
			if (name) return ("Slow Poison");
			if (desc) return ("Reduces the level of the player's poison.");
			if (cast)
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
			}

			break;
		}

		case DRUID_POISON_CLOUD:
		{
			rad = 4;

			/*
			 * Note the damage of the final poison cloud is 15% of the
			 * damage listed below, according to terrain.txt
			 */
			dam = 30 + (plev * 2);
			dam1 = (dam * f_info[FEAT_POISON_CLOUD].x_damage) / 100;

			if (name) return ("Poison Cloud");
			if (desc) return (format("Creates a radius %d cloud of poison that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_POIS, dir, dam, rad);
			}

			break;
		}

		case DRUID_NATURAL_ESCAPE:
		{
			if (name) return ("Natural Escape");
			if (desc) return ("Random displacement from/to native terrain.");
			if (cast)
			{
				/* Get the distance */
				/*int dam = MAX(plev, 20);*/

				int dam = plev * 8;

				/* Teleport */
				if (!native_teleport_player(dam)) return (NULL);
			}

			break;

		}

		case DRUID_BARKSKIN:
		{

			dur = 25 + p_ptr->lev;

			if (name) return ("Bark Skin");
			if (desc) return (format("Temporarily increases armour class by 50 for %d turns.", dur));
			if (cast)
			{
				(void)set_shield(p_ptr->shield + dur);
			}

			break;
		}

		case DRUID_NOURISHMENT:
		{
			if (name) return ("Nourishment");
			if (desc) return ("Magically renders the player well-fed.");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case DRUID_TURN_STONE_TO_MUD:
		{
			dam = 20 + randint(30);

			if (name) return ("Turn Stone to Mud");
			if (desc) return ("Removes one section of a normal wall to floor.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)wall_to_mud(dir, dam);
			}

			break;
		}

		case DRUID_FROST_BEAM:
		{
			dice = 6 + ((plev - 3) / 4);
			sides = 8;

			if (name) return ("Frost Beam");
			if (desc) return (format("Fires a beam of frost for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);

				fire_beam(GF_COLD, dir, damroll(dice, sides), 0L);
			}

			break;
		}

		case DRUID_CURE_POISON:
		{
			if (name) return ("Cure Poison");
			if (desc) return ("Cures the player of poison.");
			if (cast)
			{
				(void)set_poisoned(0);
			}

			break;
		}

		case DRUID_TRAP_DOOR_DESTRUCTION:
		{
			if (name) return ("Trap/Door Destruction");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (cast)
			{
				if (!get_aim_dir(&dir, TRUE)) return (NULL);
				destroy_door(dir);
			}

			break;
		}

		case DRUID_RESIST_HEAT_COLD:
		{
			dur = 20;


			if (name) return ("Resist Heat and Cold");
			if (desc) return (format("Temporary opposition to fire and frost for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				dur1 = randint(dur);

				(void)set_oppose_fire(p_ptr->oppose_fire + dur1 + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur1 + dur);
			}

			break;
		}

		case DRUID_SPEAR_OF_LIGHT:
		{
			dice = 6;
			sides = 8;

			if (name) return ("Spear of Light");
			if (desc) return (format("Fires a line of weak light.  %dd%d hp damage for light-hating creatures.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				msg_print("A line of blue shimmering light appears.");
				(void)lite_line(dir, damroll(dice, sides));
			}

			break;
		}

		case DRUID_FIRE_BEAM:
		{
			dice = 7 + (plev-3) / 4;
			sides = 7;

			if (name) return ("Fire Beam");
			if (desc) return (format("Fires a beam of fire for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)fire_beam(GF_FIRE, dir, damroll(dice, sides), 0L);
			}

			break;
		}

		case DRUID_STERILIZE:
		{

			dam = 1;
			rad = 8;

			if (name) return ("Sterilize");
			if (desc) return (format("Fires a radius %d ball that prevents monsters from multiplying.", rad));

			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_STERILIZE, dir, dam, rad);
			}

			break;
		}

		case DRUID_EXTINGUISH:
		{

			if (name) return ("Extinguish");
			if (desc) return ("Attempts to extinguish nearby fires.");
			if (cast)
			{
				u32b flg = PROJECT_BOOM | PROJECT_WALL | PROJECT_GRID | PROJECT_EFCT;

				/* Try to put out fires*/
				project(SOURCE_PLAYER, 2, py, px, py, px, (plev * 75), GF_EXTINGUISH, flg, 0,0);
			}

			break;
		}

		case DRUID_CLEAR_AREA:
		{
			dam = 50 + randint(30);
			rad = 4 + randint(3);

			if (name) return ("Clear Area");
			if (desc) return ("Clears an area of all normal walls.");
			if (cast)
			{
				fire_star(GF_KILL_WALL, dam, rad, PROJECT_PASS);
			}

			break;
		}

		case DRUID_CURE_CRITICAL_WOUNDS:
		{
			dice = 8;
			sides = 10;

			if (name) return ("Cure Critical Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case DRUID_IDENTIFY:
		{
			if (name) return ("Identify");
			if (desc) return ("Identifies an object.");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;

		}

		case DRUID_CLEAR_AIR:
		{

			if (name) return ("Clear Air");
			if (desc) return ("Clears the air of visible effects.");
			if (cast)
			{
				u32b flg = PROJECT_BOOM | PROJECT_WALL | PROJECT_EFCT;

				/* Clear the air of effects*/
				project(SOURCE_PLAYER, 3, py, px, py, px, 1, GF_CLEAR_AIR, flg, 0,0);
			}

			break;
		}

		case DRUID_DETECT_TERRAIN:
		{
			if (name)
			{
			       	return ("Detect Terrain");
			}
			if (desc)
			{
				return ("Maps all natural features on the entire dungeon level."
					"  At level 30 also reveals what natural beings can see.");
			}
			if (cast)
			{
				detect_terrain();
				if (plev >= 30) read_minds();
			}
			break;

		}

		case DRUID_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (cast)
			{
				earthquake(py, px, rad);
			}

			break;
		}

		case DRUID_LIFE_DRAIN_BURST:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 30% of the
			 * damage listed below, according to terrain.txt
			 */
			dam = 400 + (plev * 6);  /*120 + plev times 2 damage*/
			dam1 = (dam * f_info[FEAT_LIFE_DRAIN].x_damage) / 100;

			if (name) return ("Life draining bursts");
			if (desc) return (format("Creates a radius %d orb of time released life draining bursts that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_LIFE_DRAIN, dir, dam, rad);
			}

			break;
		}

		case DRUID_ELEMENTAL_BRAND:
		{

			dur1 = 5;
			dur = 10 + p_ptr->lev / 2;

			if (name) return ("Elemental Weapon");
			if (desc) return (format("Temporarily makes your weapon glow with elemental brands for %d turns, or %d more turns if already glowing.",
		   								dur, dur1));
			if (cast)
			{
				if (!p_ptr->slay_elements)
				{
					(void)set_slay_elements(p_ptr->slay_elements + dur);
				}
				else
				{
					(void)set_slay_elements(p_ptr->slay_elements + dur1);
				}
			}

			break;
		}


		case DRUID_FROST_BALL:
		{
			dam = 25 + 7 * plev / 2;
			rad = 2;

			if (name) return ("Frost Ball");
			if (desc) return (format("Fires a radius %d ball of frost for %d hp damage.", rad, dam));

			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_COLD, dir, dam, rad);
			}

			break;
		}


		case DRUID_HEAL:
		{
			/*Not as powerful for Rangers*/
			if (cp_ptr->flags & (CF_ZERO_FAIL)) dam = 300;
			else dam = 175;

			if (name) return ("Heal");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case DRUID_DISPEL_LIFE:
		{
			dice = 1;
			sides = plev * 3;

			if (name) return ("Dispel Life");
			if (desc) return (format("Does %dd%d damage to all living creatures in line of sight.", dice, sides));
			if (cast)
			{
				/* Calculate damage */
				if (dice > 1) dam = damroll(dice, sides);

				else dam = randint(sides);

				project_los(p_ptr->py, p_ptr->px, dam, GF_LIFE_DRAIN);
			}

			break;
		}

		case DRUID_FIRE_BALL:
		{
			dam = 50 + plev * 3;
			dam1 = plev * 2;
			rad = 2;

			if (name) return ("Fire Ball");
			if (desc) return (format("Fires a radius %d ball of fire for %d+1d%d hp damage.", rad, dam, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				dam += randint(dam1);
				fire_ball(GF_FIRE, dir, dam, rad);
			}

			break;
		}

		case DRUID_DRAIN_LIFE_ARC:
		{
			dam = 125;
			dice = 5;
			sides = plev;

			if (name) return ("Life Draining Arc");
			if (desc) return (format("Fires an arc of life draining for %d+%dd%d hp damage.", dam, dice, sides));
			if (cast)
			{
				/*figure the damage*/
				dam += damroll(dice, sides);

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_LIFE_DRAIN, dir, dam, 0, 30);
			}

			break;
		}

		case DRUID_MASS_IDENTIFY:
		{
			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (cast)
			{
				mass_identify(3);
			}

			break;
		}


		case DRUID_RESIST_ELEC:
		{
			dur = 40;

			if (name) return ("Resist Electricity");
			if (desc) return (format("Temporary opposition to electricity for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_elec(p_ptr->oppose_elec +  randint(dur) + dur);
			}

			break;
		}

		case DRUID_RESIST_ACID:
		{
			dur = 40;

			if (name) return ("Resist Acid");
			if (desc) return (format("Temporary opposition to acid for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(dur) + dur);
			}

			break;
		}

		case DRUID_RESIST_POISON:
		{
			dur = 40;

			if (name) return ("Resist Poison");
			if (desc) return (format("Temporary resist to poison for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(dur) + dur);
			}

			break;
		}

		case DRUID_RESISTANCE:
		{
			dur = 30;

			if (name) return ("Resistance");
			if (desc) return (format("Temporary resist to the 5 elements for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				int time_2 = randint(dur) + dur;
				(void)set_oppose_acid(p_ptr->oppose_acid + time_2);
				(void)set_oppose_elec(p_ptr->oppose_elec + time_2);
				(void)set_oppose_fire(p_ptr->oppose_fire + time_2);
				(void)set_oppose_cold(p_ptr->oppose_cold + time_2);
				(void)set_oppose_pois(p_ptr->oppose_pois + time_2);
			}

			break;
		}

		case DRUID_HASTE_SELF:
		{
			dur1 = 5;
			dur = 20;

			if (name) return ("Haste Self");
			if (desc) return (format("Temporarily hasten yourself for %d+1d%d turns, or %d more turns if already hasted.",
											plev, dur, dur1));
			if (cast)
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(dur) + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(dur1));
				}
			}

			break;
		}

		case DRUID_GLACIER:
		{
			/* Get the feature */
			feature_type *f_ptr = &f_info[FEAT_GLACIER];
			dam = f_ptr->x_timeout_set;
			dam1 = f_ptr->x_timeout_rand;

			if (name) return ("Glacier");
			if (desc) return (format("Creates a transparent and impenetrable ice barrier that lasts %d+d%d turns.", dam, dam1));
			if (cast)
			{
				if (!create_glacier()) return (NULL);
			}

			break;
		}

		case DRUID_FLICKER:
		{
			dam = 10;

			if (name) return ("Flicker");
			if (desc) return (format("A range %d random minor displacement.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;
		}


		case DRUID_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (cast)
			{
				set_recall();
			}

			break;
		}

		case DRUID_HEALING:
		{
			dam = 750;

			if (name) return ("Healing");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case DRUID_RESTORATION:
		{

			if (name) return ("Restoration");
			if (desc) return ("Restores all stats to their maximum.");
			if (cast)
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
			}

			break;
		}

		case DRUID_REMEMBRANCE:
		{

			if (name) return ("Remembrance");
			if (desc) return ("Restores experience to maximum.");
			if (cast)
			{
				(void)restore_level();
			}

			break;
		}

		case DRUID_SANDSTORM:
		{
			dam = plev * 3;
			dice = 6;
			sides = plev;

			if (name) return ("Sand storm");
			if (desc) return (format("Fires an arc of sand for %d+%dd%d hp damage",
				dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				dam += damroll(dice, sides); 
				fire_arc(GF_SAND, dir, dam, 0, 16);
			}

			break;
		}

		case DRUID_NATIVE_SAND:
		{

			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Sand Nativity");
			if (desc) return (format("Temporary nativity to sand for %d turns, or %d more turns if already temporarily native to sand.",
											dur, dur1));
			if (cast)
			{
				if (p_ptr->temp_native_sand) dur = dur1;

				(void)set_temp_native_sand(p_ptr->temp_native_sand + dur);
			}

			break;
		}

		case DRUID_NATIVE_MUD:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Mud Nativity");
			if (desc) return (format("Temporary nativity to mud for %d turns, or %d more turns if already temporarily native to mud.",
											dur, dur1));
			if (cast)
			{
				if (p_ptr->temp_native_mud) dur = dur1;

				(void)set_temp_native_mud(p_ptr->temp_native_mud + dur);
			}

			break;
		}

		case DRUID_NATIVE_WATER:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Water Nativity");
			if (desc) return (format("Temporary nativity to water for %d turns, or %d more turns if already temporarily native to water.",
											dur, dur1));
			if (cast)
			{
				if (p_ptr->temp_native_water) dur = dur1;

				(void)set_temp_native_water(p_ptr->temp_native_water + dur);
			}

			break;
		}

		case DRUID_NATIVE_OIL:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Oil Nativity");
			if (desc) return (format("Temporary nativity to oil for %d turns, or %d more turns if already temporarily native to oil.",
											dur, dur1));
			if (cast)
			{
				if (p_ptr->temp_native_oil) dur = dur1;

				(void)set_temp_native_oil(p_ptr->temp_native_oil + dur);
			}

			break;
		}

		case DRUID_NATIVE_LAVA:
		{
			dur = 50 + p_ptr->lev;
			dur1 = 10;

			if (name) return ("Lava Nativity");
			if (desc) return (format("Temporary nativity to lava for %d turns, or %d more turns if already temporarily native to lava.",
											dur, dur1));
			if (cast)
			{
				if (p_ptr->temp_native_lava) dur = dur1;

				(void)set_temp_native_lava(p_ptr->temp_native_lava + dur);
			}

			break;
		}

		case DRUID_CHANNEL_LIGHTNING:
		{
			dice = 4 + plev / 3;
			sides = 10 + plev / 2;

			if (name) return ("Channel Lightning");
			if (desc) return (format("Fires a powerful bolt of lightning for %dd%d hp damage.",
				dice, sides));

			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);

				(void)fire_bolt_beam_special(GF_ELEC, dir, damroll(dice, sides),
					MAX_RANGE, PROJECT_NO_EFCT);
			}

			break;
		}

		case DRUID_DISPEL_CURSE:
		{
			if (name) return ("Dispel Curse");
			if (desc) return ("Removes standard and heavy curses.");
			if (cast)
			{
				remove_curse(TRUE);
			}

			break;
		}

		case DRUID_RECHARGE_ITEM:
		{
			if (name) return ("Greater Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (cast)
			{
				(void)recharge(50 + plev, FALSE);
			}

			break;
		}

		case DRUID_BRAND_AMMUNITION:
		{
			if (name) return ("Elemental Ammunition Brand");
			if (desc) return ("Attempts to brand one set of ammunition.");
			if (cast)
			{
				(void)brand_ammo(TRUE);
			}

			break;
		}

		case DRUID_ENCHANT_ARMOUR:
		{
			if (name) return ("Enchant Armour");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
			}

			break;

		}

		case DRUID_BRAND_WEAPON:
		{
			if (name) return ("Elemental Weapon Brand");
			if (desc) return ("Attempts to brand one weapon.");
			if (cast)
			{
				(void)brand_weapon(TRUE);
			}

			break;
		}

		case DRUID_WATER_CHAIN:
		{
			dam = 100;
			dice = 3 + plev / 10;
			sides = plev; 
 
			if (name) return ("Ulmo's Wrath");
			if (desc) return (format("Fires chained beams of water for %d+%dd%d hp damage.",
				dam, dice, sides));
			if (cast)
			{
				int max_hits = 5;
				int decrement = 10;

				if (one_in_(5))
				{
					max_hits = 7;
					decrement = 0;
				}

				dam += damroll(dice, sides);

				if (!beam_chain(GF_WATER, dam, max_hits, decrement)) return (NULL);
			}

			break;
		}

		case DRUID_CALL_HUORNS:
		{
			dam = plev;
			dam1 = plev / 10;

			if (name) return ("Call Huorns");
			if (desc) return (format("Make nearby trees attack foes for %d player turns, or %d turns more if already casted.",
				dam, dam1));
			if (cast)
			{
				/* Spell already active */
				if (p_ptr->temp_call_huorns > 0)
				{
					set_temp_call_huorns(p_ptr->temp_call_huorns + dam1);
				}
				/* Full time */
				else
				{
					set_temp_call_huorns(dam);
				}
			}

			break;
		}

		case DRUID_MASTER_ELEMENTS:
		{
			dam = p_ptr->lev * ((28 * p_ptr->lev) / 100);

			if (name) return ("Master Elements");
			if (desc) return (format("Cast a powerful ball of elements for %d hp damage.", dam));
			if (cast)
			{
				if (!master_elements(dam)) return (NULL);
			}

			break;
		}

		case DRUID_STEAL_POWERS:
		{
			if (name) return ("Steal Powers");
			if (desc) return (format("Cast attack spells like nearby animals and vortices."));
			if (cast)
			{
				if (!steal_powers()) return (NULL);
			}

			break;
		}

		default: break;
	}

	/* success  */
	return ("success");
}

/*
 * Helper function for casting the "Prismatic Spray" spell.
 */
static void cast_prismatic_spray(int dir, int dam)
{
	int gftype;

	switch (randint(p_ptr->lev + 25) / 5)
	{
		/*
		 * Should only be possible for level 50 and even then odds
		 * should be 1/75 of this hapening. An easter egg type of
		 * effect. :) -AR
		 */
		case (15):
		{
			gftype = GF_CHAOS;
			dam *= 12;
			msg_print("You conjure forth a massive torrent of raw chaos!");
			break;
		}
		case (14):
		{
			gftype = GF_INERTIA;
			dam *= 2;
			msg_print("You conjure forth a torrent of inertia.");
			break;
		}
		case (13):
		{
			gftype = GF_GRAVITY;
			dam *= 2;
			msg_print("You conjure forth a torrent of gravity.");
			break;
		}
		case (12):
		{
			gftype = GF_CONFUSION;
			dam *= 3;
			msg_print("You conjure forth a torrent of confusion.");
			break;
		}
		case (11):
		{
			gftype = GF_FORCE;
			dam *= 3;
			msg_print("You conjure forth a torrent of pure force.");
			break;
		}
		case (10):
		{
			gftype = GF_TIME;
			dam *= 3;
			msg_print("You conjure forth a torrent of time.");
			break;
		}
		case (9):
		{
			gftype = GF_STATIC;
			dam *= 4;
			msg_print("You conjure forth a torrent of anti-magic static.");
			break;
		}
		case (8):
		{
			gftype = GF_NEXUS;
			dam *= 4;
			msg_print("You conjure forth a torrent of dimensional energy.");
			break;
		}
		case (7):
		{
			gftype = GF_DISENCHANT;
			dam *= 4;
			msg_print("You conjure forth a torrent of disenchantment.");
			break;
		}
		case (6):
		{
			gftype = GF_SHARD;
			dam *= 4;
			msg_print("You conjure forth a torrent of shrapnel.");
			break;
		}
		case (5):
		{
			gftype = GF_NETHER;
			dam *= 5;
			msg_print("You conjure forth a torrent of nether.");
			break;
		}
		case (4):
		{
			gftype = GF_COLD;
			dam *= 5;
			msg_print("You conjure forth a torrent of frost.");
			break;
		}
		case (3):
		{
			gftype = GF_FIRE;
			dam *= 5;
			msg_print("You conjure forth a torrent of flames.");
			break;
		}
		case (2):
		{
			gftype = GF_ELEC;
			dam *= 5;
			msg_print("You conjure forth a torrent of electricity.");
			break;
		}
		case (1):
		{
			gftype = GF_ACID;
			dam *= 5;
			msg_print("You conjure forth a torrent of acid.");
			break;
		}
		default:
		{
			gftype = GF_POIS;
			dam *= 5;
			msg_print("You conjure forth a torrent of poison.");
			break;
		}
	}

	fire_arc(gftype, dir, dam, 0, 60);
}

/*
 * Cast a spell, or output spell info, description.
 */
cptr do_mage_spell(int mode, int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int plev = p_ptr->lev;

	/* Casting variables */
	int dir;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1, dur2;
	int dice, sides;
	int rad;

	/* Function modes */
	bool cast = FALSE;
	bool name = FALSE;
	bool desc = FALSE;

	/* Function modes */
	if (mode == MODE_SPELL_CAST) cast = TRUE;
	if (mode == MODE_SPELL_NAME) name = TRUE;
	if (mode == MODE_SPELL_DESC) desc = TRUE;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = beam_chance();

	/* Spells. */
	switch (spell)
	{
		case SPELL_MAGIC_MISSILE:
		{

			dice = 3 + ((plev - 1) / 5);
			sides = 4;

			if (name) return ("Magic Missile");
			if (desc) return (format("Fires a magic missile for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_DETECT_MONSTERS:
		{
			if (name) return ("Detect Monsters");
			if (desc) return ("Detects nearby monsters that are not invisible.");
			if (cast)
			{
				(void)detect_monsters_normal();
			}

			break;
		}

		case SPELL_PHASE_DOOR:
		{
			dam = 10;

			if (name) return ("Phase Door");
			if (desc) return (format("A range %d random minor displacement.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;
		}

		case SPELL_LIGHT_AREA:
		{
			dice = 2;
			sides = (plev / 2);
			rad = (plev / 10) + 1;

			if (name) return ("Light Area");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (cast)
			{
				(void)lite_area(damroll(dice, sides), rad);
			}

			break;
		}

		case SPELL_TREASURE_DETECTION:
		{
			if (name) return ("Detect Treasure");
			if (desc) return ("Detects nearby treasure.");
			if (cast)
			{
				(void)detect_treasure();
				(void)detect_objects_gold();
			}

			break;
		}

		case SPELL_CURE_LIGHT_WOUNDS:
		{
			dice = 2;
			sides = 10;

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(p_ptr->cut - 15);
			}
			break;
		}

		case SPELL_OBJECT_DETECTION:
		{
			if (name) return ("Detect Objects");
			if (desc) return ("Detects nearby objects.");
			if (cast)
			{
				(void)detect_objects_normal();
			}

			break;
		}

		case SPELL_FIND_TRAPS_DOORS:
		{
			if (name) return ("Find Hidden Traps/Doors");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (cast)
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}

			break;
		}

		case SPELL_STINKING_CLOUD:
		{
			rad = 2;
			dam = 10 + (plev / 2);

			if (name) return ("Stinking Cloud");
			if (desc) return (format("Fires a radius %d cloud of poison for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_POIS, dir, dam, rad);
			}

			break;
		}

		case SPELL_CONFUSE_MONSTER:
		{
			if (name) return ("Confuse Monster");
			if (desc) return ("Attempts to confuse one monster.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)confuse_monster(dir, plev);
			}

			break;
		}

		/*
		This spell is meant to be very strong at point blank range but then weaken fast as the range extends.
		As such, it might make sense to boost the damage and broaden the arc to even wider than 60 degrees to
		achieve the desired result. Considering how sound behaves in real life it might be fun to make the arc
	   270 degrees and call it something like "Wail of the Banshee" or "War Cry" or something. -AR
		*/

		case SPELL_SHOCK_WAVE:
		{
			dam = 20;
			dice = 1 + ((plev - 1 ) / 5); /*Reaches max damage (20+10d11, average 80) at plev 46. */
			sides = 11;

			if (name) return ("Shock Wave");
			if (desc) return (format("Fires an arc of sonic energy for %d+%dd%d hp damage.", dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_SOUND, dir, dam + damroll(dice, sides), 0, 60);
			}

			break;
		}

		case SPELL_TRAP_DOOR_DESTRUCTION:
		{
			if (name) return ("Trap/Door Destruction");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (cast)
			{
				if (!get_aim_dir(&dir, TRUE)) return (NULL);
				destroy_door(dir);
			}

			break;
		}

		case SPELL_SLEEP_MONSTER:
		{
			if (name) return ("Sleep Monster");
			if (desc) return ("Attempts to put a monster to sleep.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)sleep_monster(dir);
			}

			break;
		}

		case SPELL_CURE_POISON:
		{
			if (name) return ("Cure Poison");
			if (desc) return ("Cures the player of poison.");
			if (cast)
			{
				(void)set_poisoned(0);
			}

			break;
		}

		case SPELL_TELEPORT_SELF:
		{
			dam = (plev * 5);

			if (name) return ("Teleport Self");
			if (desc) return (format("Random major displacement up to %d squares.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;
		}

		case SPELL_SPEAR_OF_LIGHT:
		{
			dice = 6;
			sides = 8;

			if (name) return ("Spear of Light");
			if (desc) return (format("Fires a line of weak light.  %dd%d hp damage for light-hating creatures.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				msg_print("A line of blue shimmering light appears.");
				(void)lite_line(dir, damroll(dice, sides));
			}

			break;
		}

		/*
		Magic Missile is average of 30 damage for 1 at level 50.

		I'm powering up the Mage's bolt spells significantly, since I see them as a key element
		for Mages due to the BEAM flag and the Druid's almost total reliance on non-bolt projections.
		Non- Magic Missile bolts will follow a guide line of 20 damage per mana average on plev 50,
		which is slightly less than Orb of Drainings average of 23 damage per mana at plev 50. This is
		done to lessen the reliance on Magic Missile. The uniformity of damage to mana ratios needs to
		be counteracted somehow in order to minimize the amount of spells that become obsolete as the
		game progresses. The HURT_FIRE, HURT_COLD, RES_WATER, RES_PLAS, EVIL, RES_NETHR, RES_COLD
		(partial resistance to ice) and RES_FIRE (partial resistance to plasma) could be used for that.
		In many cases, by accident they already are. For example HURT_COLD keeps ICE bolt useful even
		after Water Bolt has achieved 0% fail rate by offering a better damage per mana output against
		Fire based mosters that are cold sensitive, most notably red dragons.

		Overall I hope to make Mages better than Druids at confonting single monsters while Druid
		out perform Mages at crowd control.

		The fact that Orb does only 11.5 damage on average to non-evil is trivial due to the high % of
	    evil in the dungeon. (As it should be, since it is Morgoth's dungeon, after all) -AR
		*/

		case SPELL_ICE_BOLT:
		{
			dice = 5 + ((plev - 3) / 3);
			sides = 9;

			if (name) return ("Ice Bolt");
			if (desc) return (format("Fires a bolt or beam of ice for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
					fire_bolt_or_beam(beam, GF_ICE, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_TURN_STONE_TO_MUD:
		{
			dam = 20 + randint(30);

			if (name) return ("Turn Stone to Mud");
			if (desc) return ("Removes one section of a normal wall to floor.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)wall_to_mud(dir, dam);
			}

			break;
		}

		case SPELL_SATISFY_HUNGER:
		{
			if (name) return ("Satisfy Hunger");
			if (desc) return ("Magically renders the player well-fed.");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case SPELL_RECHARGE_ITEM_I:
		{
			if (name) return ("Lesser Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (cast)
			{
				(void)recharge(2 + plev / 5, FALSE);
			}

			break;
		}

		case SPELL_PRISMATIC_SPRAY: /* Replaces Wonder with something more consistently usefull. -AR*/
		{
			dam = 25 + 2 * plev;

			if (name) return ("Prismatic Spray");
			if (desc) return (format("Invokes a cone of a random element with a "
				"damage between %d and %d, depending on the element.", 2 * dam, 5 * dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				cast_prismatic_spray(dir, dam);
			}

			break;
		}

		case SPELL_POLYMORPH_OTHER:
		{
			if (name) return ("Polymorph Other");
			if (desc) return ("Attempts to change a monster into a different monster race.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)poly_monster(dir);
			}

			break;
		}

		case SPELL_IDENTIFY:
		{
			if (name) return ("Identify");
			if (desc) return ("Identifies an object.");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;

		}

		case SPELL_MASS_SLEEP:
		{
			if (name) return ("Mass Sleep");
			if (desc) return ("Attempts to sleep all monsters in LOS.");
			if (cast)
			{
				(void)sleep_monsters(damroll (2, p_ptr->lev));
			}

			break;
		}

		case SPELL_SHARD_STORM:
		{
			rad = 4;

			/*
			* Note the damage of the final shard cloud is 15% of the
			* damage listed below, according to terrain.txt
			*
			* I gave this a weird damage progression to in an attempt to
			* make it more usefull for levels 30+ while keeping it at
			* roughly the same powerlevel in the early game.
			*/

			dam = 20 + (plev * 6);

			if (plev>30)
			   dam+=(plev-30)*9;


			dam1 = (dam * f_info[FEAT_SHARD].x_damage) / 100;

			if (name) return ("Shard Storm");
			if (desc) return (format("Creates a radius %d cloud of shards that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_SHARD, dir, dam, rad);
			}


			break;
		}

		case SPELL_SLOW_MONSTER:
		{
			if (name) return ("Slow Monster");
			if (desc) return ("Attempts to slow a monster.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)slow_monster(dir);
			}

			break;
		}

		case SPELL_CALL_LIGHTNING:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 10% of the
			 * damage listed below, according to terrain.txt
			 * Final damage is double the damage od Drain Life Bursts
			 * at double the mana cost, a big bang for bick bucks theme
			 * I have for Mages. Maybe cut it down to 150% damage for 150%
			 * mana later if it is too fast a damage dealing rate. -AR
			 */
			dam = 2400 + (plev * 40);  /*240 + plev times 4 damage*/
			dam1 = (dam * f_info[FEAT_SPARKS].x_damage) / 100;

			if (name) return ("Call Lightning");
			if (desc) return (format("Creates a radius %d orb of time released lightning strikes that cause %d hp damage each.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_ELEC_BURST, dir, dam, rad);
			}

			break;
		}

		case SPELL_RECHARGE_ITEM_II:
		{
			if (name) return ("Greater Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (cast)
			{
				(void)recharge(50 + plev, FALSE);
			}

			break;
		}

		case SPELL_TELEPORT_OTHER:
		{
			if (name) return ("Teleport Other");
			if (desc) return ("Attempts to teleport a monster away.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)teleport_monster(dir);
			}

			break;
		}

		case SPELL_BEDLAM:
		{
			rad = 4;

			/*
			* Note the damage of the final confusion cloud is 15% of the
			* damage listed below, according to terrain.txt
			*
			* Bedlam is now a cloud of confusion -AR
			*/
			dam = 120 + (plev * 8);
			dam1 = (dam * f_info[FEAT_CONFUSION].x_damage) / 100;

			if (name) return ("Bedlam");
			if (desc) return (format("Creates a radius %d cloud of confusion that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_CONFUSION, dir, dam, rad);
			}


			break;
		}

		case SPELL_WATER_BOLT: /*The culmination of a mages pre-Raal's arsenal -AR*/
		{
			dice =  3 + (plev + 3) / 3;
			sides = 19;

			if (name) return ("Water Bolt");
			if (desc) return (format("Fires a bolt or beam of water for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
					fire_bolt_or_beam(beam, GF_WATER, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_WORD_OF_DESTRUCTION:
		{
			rad = 15;

			if (name) return ("Word of Destruction");
			if (desc) return (format("Creates a radius %d earthquake.  Deletes all non-quest monsters.", rad));
			if (cast)
			{
				destroy_area(py, px, rad, TRUE);
			}

			break;
		}

		case SPELL_BANISHMENT:
		{
			if (name) return ("Banishment");
			if (desc) return ("Banishes one type of monster.  Uniques and quest monsters are unaffected");
			if (cast)
			{
				(void)banishment();
			}

			break;
		}

		case SPELL_DOOR_CREATION:
		{
			if (name) return ("Door Creation");
			if (desc) return ("Creates a barrier of doors around you.");
			if (cast)
			{
				(void)door_creation();
			}

			break;
		}

		case SPELL_STAIR_CREATION:
		{
			if (name) return ("Stair Creation");
			if (desc) return ("Creates a staircase nearby.  Random choice between up or down, except on quest levels.");
			if (cast)
			{
				(void)stair_creation();
			}

			break;
		}

		case SPELL_TELEPORT_LEVEL:
		{
			if (name) return ("Teleport Level");
			if (desc) return ("Immediately takes you to the next level up or down.");
			if (cast)
			{
				teleport_player_level(SOURCE_PLAYER);
			}

			break;
		}

		case SPELL_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (cast)
			{
				earthquake(py, px, rad);
			}

			break;
		}

		case SPELL_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (cast)
			{
				set_recall();
			}

			break;
		}

		case SPELL_HURRICANE:
		{
			dam = 50 + (plev * 4);
			rad = 2;
			/*Big damage for big mana. Final damage will be 12.5 points per mana,
		    * a bit better than Druid Fire Ball, Druid Frost Ball is almost
			* exactly 12.9. -AR
		    */


			if (name) return ("Hurricane");
			if (desc) return (format("Conjures forth a radius %d storm of wind and water for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_WATER, dir, dam, rad);
			}

			break;
		}

		case SPELL_CLOUD_KILL:
		{
			rad = 4;

			/*
			* Note the damage of the final Nether cloud is 30% of the
			* damage listed below, according to terrain.txt
			*
			* Cloudkill is now a cloud of nether, double damage and cost
			* of Shard Storm. It now reminds it's D&D 3.5 counterpart. -AR
			*/
			dam = 60 + (plev * 4);

			if (plev > 40) dam += (plev - 40) * 24;

			dam1 = (dam * f_info[FEAT_NETHER].x_damage) / 100;

			if (name) return ("Cloudkill");
			if (desc) return (format("Creates a radius %d cloud of nether that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_NETHER, dir, dam, rad);
			}


			break;
		}

		case SPELL_ICE_STORM: /* Moved down to make room for more powerful spells. Final damage ratio is 12 damage for 1 mana. -AR*/
		{
			dam = 160 + (plev * 4);
			rad = 3;

			if (name) return ("Ice Storm");
			if (desc) return (format("Fires a radius %d ball of ice for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_ICE, dir, dam, rad);
			}

			break;
		}

		case SPELL_PLASMA_BOLT: /*Most powerful bolt for pre-Kelek's Mages. -AR */
		{
			dice =  15 + (plev / 2);
			sides = 19;

			if (name) return ("Plasma Bolt");
			if (desc) return (format("Fires a bolt or beam of plasma for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
					fire_bolt_or_beam(beam, GF_PLASMA, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_METEOR_STORM:
		{
			rad = 5;

			/*
			 * Note the damage of the final static is 30% of the
			 * damage listed below, according to terrain.txt
			 * Final damage is 990 hp per hit! A monumental mana cost is
			 * required of course, at least 60 or 75 mana. -AR
			 */
			dam = 1800 + (plev * 30);  /*540 + plev times 9 damage for each meteor*/
			dam1 = (dam * f_info[FEAT_METEOR_BURST].x_damage) / 100;

			if (name) return ("Meteor Storm");
			if (desc) return (format("Creates a radius %d meteor strike that causes %d hp damage.", rad, dam1));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_effect_orb(GF_METEOR, dir, dam, rad);
			}

			break;
		}

		case SPELL_MANA_STORM:
		{
			dam = 160 + (plev * 10);
			rad = 3;

			if (name) return ("Mana Storm");
			if (desc) return (format("Fire a radius %d storm of mana for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_MANA, dir, dam, rad);
			}

			break;
		}
		case SPELL_DETECT_INVISIBLE:
		{
			if (name) return ("Detect Invisible");
			if (desc) return ("Detects invisible monsters.");
			if (cast)
			{
				(void)detect_monsters_invis();
			}

			break;

		}

		case SPELL_DETECT_ENCHANTMENT:
		{
			if (name) return ("Detect Enchantment");
			if (desc) return ("Detects nearby enchanted objects.");
			if (cast)
			{
				(void)detect_objects_magic();
			}

			break;
		}

		case SPELL_NOVA:
			{
			/* A ball of GF_LITE allows a Mage to light up a distant area and hit
		    * vulnerable creatures for good damage. 8/1 final damage/mana for
			* normals, 20/1 for vulnerable creatures. Might be out of flavour for
			* mages though, so considering GF_PLASMA or possibly something else
			* with a different spell name. -AR
			*/
			dam = 60 + ((plev * 6) / 5);
			rad = 2;

			if (name) return ("Nova");
			if (desc) return (format("Fires a radius %d explosion of powerful light for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_LITE, dir, dam, rad);
			}

			break;
		}

		case SPELL_REND_SOUL:
		{
			dice =  10 + ((plev * 2) / 5);
			sides = 19;

			/*Has greater than 20/1 final damage ratio, 25/1 but
			* balanced by the fact that EVIL monsters resist. Take
		    * that Hydras! ;) Might tone down damage and mana costs
			* both. -AR*/

			if (name) return ("Rend Soul");
			if (desc) return (format("Fires a bolt or beam of nether for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
					fire_bolt_or_beam(beam, GF_NETHER, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_MASS_BANISHMENT:
		{
			if (name) return ("Mass Banishment");
			if (desc) return ("Banishes nearby monsters.  Uniques and quest monsters are unaffected.");
			if (cast)
			{
				(void)mass_banishment();
			}

			break;
		}

		case SPELL_RESIST_FIRE:
		{
			dur = 20;

			if (name) return ("Resist Fire");
			if (desc) return (format("Temporary opposition to fire for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(dur) + dur);
			}

			break;
		}

		case SPELL_RESIST_COLD:
		{
			dur = 20;

			if (name) return ("Resist Cold");
			if (desc) return (format("Temporary opposition to cold for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(dur) + dur);
			}

			break;
		}

		case SPELL_ELEMENTAL_BRAND: /* elemental brand */
		{
			if (name) return ("Elemental Brand");
			if (desc) return ("Attempts to brand one set of ammunition.");
			if (cast)
			{
				(void)brand_ammo(TRUE);
			}

			break;
		}

		case SPELL_RESIST_POISON:
		{
			dur = 20;

			if (name) return ("Resist Poison");
			if (desc) return (format("Temporary resist to poison for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(dur) + dur);
			}

			break;
		}

		case SPELL_RESISTANCE:
		{
			dur = 20;

			if (name) return ("Resistance");
			if (desc) return (format("Temporary resist to the 5 elements for %d+%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{
				int time_2 = randint(dur) + dur;
				(void)set_oppose_acid(p_ptr->oppose_acid + time_2);
				(void)set_oppose_elec(p_ptr->oppose_elec + time_2);
				(void)set_oppose_fire(p_ptr->oppose_fire + time_2);
				(void)set_oppose_cold(p_ptr->oppose_cold + time_2);
				(void)set_oppose_pois(p_ptr->oppose_pois + time_2);
			}

			break;
		}

		case SPELL_FLIGHT:
		{
			dur = 25 + plev / 2;
			dur1 = 10;

			if (name) return ("Flight");
			if (desc) return (format("Causes the player to fly for %d turns, or %d more turns if already flying.",
											dur, dur1));
			if (cast)
			{
				if (!p_ptr->flying)
				{
					(void)set_flying(dur, TRUE);
				}
				else
				{
					(void)set_flying(p_ptr->flying + dur1, TRUE);
				}
			}

			break;
		}

		case SPELL_SHIELD:
		{
			dur1 = 20;
			dur2 = 30;

			if (name) return ("Shield");
			if (desc) return (format("Temporarily increases armour class by 50 for %d+1d%d turns.", dur2, dur1));
			if (cast)
			{
				(void)set_shield(p_ptr->shield + randint(dur1) + dur2);
			}

			break;
		}

		case SPELL_BERSERKER:
		{
			dur = 25;
			dam = 30;

			if (name) return ("Berserker");
			if (desc) return (format("Heals player a little.  Removes fear.  Makes player berzerk for %d+1d%d turns.", dur, dur));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_shero(p_ptr->shero + randint(dur) + dur);
				(void)set_afraid(0);
			}

			break;
		}

		case SPELL_HASTE_SELF:
		{
			dur1 = 5;
			dur = 20;

			if (name) return ("Haste Self");
			if (desc) return (format("Temporarily hasten yourself for %d+1d%d turns, or %d more turns if already hasted.",
										plev, dur, dur1));
			if (cast)
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(dur) + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + dur1);
				}
			}

			break;
		}

		case SPELL_RIFT:
		{
			dam1 = 40;
			dice = plev;
			sides = 7;

			if (name) return ("Rift");
			if (desc) return (format("Fires a beam of gravity for %d+%dd%d hp damage.", dam1, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_beam(GF_GRAVITY, dir,	dam1 + damroll(dice, sides), 0L);
			}

			break;
		}

		case SPELL_DARKNESS_STORM:
		{
			/*
			* A Storm type spell while waiting for Mana Storm, picked GF_DARK
			* because I wanted to mimic monster spells. 12 for 1 final ratio.
			* -AR
			*/

			dam = 280 + (plev * 4);
			rad = 3;

			if (name) return ("Darness Storm");
			if (desc) return (format("Fires a radius %d explosion of powerful darkness for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_DARK, dir, dam, rad);
			}

			break;
		}

		case SPELL_MANA_BOLT:
		{
			dice = 20 + (plev * 2);
			sides = 9;

			if (name) return ("Mana Bolt");
			if (desc) return (format("Fires a bolt or beam of pure mana for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam, GF_MANA, dir, damroll(dice, sides));
			}

			break;
		}

		case SPELL_RUNE_OF_PROTECTION:
		{
			if (name) return ("Rune of Protection");
			if (desc) return ("Creates a rune of protection beneath you.");
			if (cast)
			{
				(void)warding_glyph();
			}

			break;
		}

		case SPELL_ENCHANT_ARMOR: /* enchant armor */
		{
			if (name) return ("Enchant Armor");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + plev / 20);
			}

			break;
		}

		case SPELL_ENCHANT_WEAPON: /* enchant weapon */
		{
			if (name) return ("Enchant Weapon");
			if (desc) return ("Attempts to enchant one weapon.");
			if (cast)
			{
				(void)enchant_spell(rand_int(4) + plev / 20,
			                     rand_int(4) + plev / 20, 0);
			}

			break;
		}

		case SPELL_MASS_IDENTIFY:
		{
			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (cast)
			{
				mass_identify(3);
			}

			break;
		}

 		case SPELL_WAIL_OF_THE_BANSHEE:
		{
			if (name) return ("Wail of the Banshee");
			if (desc) return ("Creates a wide cone of fear.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_TURN_ALL, dir, plev, 0, 60);
			}

			break;
		}


	}

	/* Success */
	return ("Success!");
}




cptr do_priest_prayer(int mode, int spell)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;

	/* Spell-specific variables */
	int dam, dam1;
	int dur, dur1;
	int dice, sides;
	int rad;

	cptr extra = "";

	/* Function modes */
	bool cast = (mode == MODE_SPELL_CAST);
	bool name = (mode == MODE_SPELL_NAME);
	bool desc = (mode == MODE_SPELL_DESC);

	switch (spell)
	{
		case PRAYER_DETECT_EVIL:
		{
			if (name) return ("Detect Evil");
			if (desc) return ("Detects all nearby evil monsters, even invisible ones.");
			if (cast)
			{
				(void)detect_monsters_evil();
			}

			break;
		}

		case PRAYER_CURE_LIGHT_WOUNDS:
		{
			dice = 3;
			sides = 8;

			if (name) return ("Cure Light Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(p_ptr->cut - 10);
			}

			break;
		}

		case PRAYER_BLESS:
		{
			dur = 12;
			if (p_ptr->blessed)
			{
				extra = " extra";
				dur = 5;
			}


			if (name) return ("Bless");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (cast)
			{
				(void)set_blessed(p_ptr->blessed + randint(dur) + dur);
			}

			break;
		}

		case PRAYER_REMOVE_FEAR:
		{

			if (name) return ("Remove Fear");
			if (desc) return ("Removes fear.");
			if (cast)
			{
				(void)set_afraid(0);
			}

			break;
		}

		case PRAYER_CALL_LIGHT:
		{
			dice = 2;
			sides = plev / 2;
			rad = (plev / 10) + 1;

			if (name) return ("Call Light");
			if (desc) return (format("Permanently lights up a room or a radius %d area.", rad));
			if (cast)
			{
				(void)lite_area(damroll(dice, sides), rad);
			}

			break;
		}

		case PRAYER_FIND_TRAPS_DOORS_STAIRS:
		{
			if (name) return ("Find Doors/Stairs/Traps");
			if (desc) return ("Detects nearby traps, doors and stairs.");
			if (cast)
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
			}

			break;
		}

		case PRAYER_SHOCK_BOLT:
		{
			dice = 6;
			sides = 4;
			dam = (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 3));

			if (name) return ("Shock Bolt");
			if (desc) return (format("Fires a bolt of solid light for %d+%dd%d damage.", dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt(GF_LITE, dir, (damroll(dice, sides) + dam));
			}

			break;
		}

		case PRAYER_SLOW_POISON:
		{
			if (name) return ("Slow Poison");
			if (desc) return ("Reduces the level of the player's poison.");
			if (cast)
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
			}

			break;
		}

		case PRAYER_SCARE_MONSTER:
		{

			if (name) return ("Scare Monster");
			if (desc) return ("Attempts to scare one monster.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)fear_monster(dir, plev);
			}

			break;

		}

		case PRAYER_PORTAL:
		{
			dam = plev * 3;

			if (name) return ("Portal");
			if (desc) return (format("Random minor displacement up to %d squares.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;
		}

		case PRAYER_CURE_SERIOUS_WOUNDS:
		{
			dice = 5;
			sides = 10;

			if (name) return ("Cure Serious Wounds");
			if (desc) return (format("Reduces cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut((p_ptr->cut / 2) - 20);
			}


			break;
		}

		case PRAYER_CHANT:
		{
			dur = 24;

			if (p_ptr->blessed)
			{
				extra = " extra";
				dur = 5;
			}

			if (name) return ("Chant");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (cast)
			{
				(void)set_blessed(p_ptr->blessed + randint(dur) + dur);
			}

			break;
		}

		case PRAYER_SANCTUARY:
		{

			if (name) return ("Sanctuary");
			if (desc) return ("Attempts to sleep all adjacent monsters.");
			if (cast)
			{
				(void)sleep_monsters_touch();
			}

			break;
		}

		case PRAYER_SATISFY_HUNGER:
		{
			if (name) return ("Satisfy Hunger");
			if (desc) return ("Magically renders the player well-fed.");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case PRAYER_REMOVE_CURSE:
		{
			if (name) return ("Remove Curse");
			if (desc) return ("Removes standard curses.");
			if (cast)
			{
				remove_curse(FALSE);
			}

			break;
		}

		case PRAYER_RESIST_HEAT_COLD:
		{
			dur = 10;

			if (name) return ("Resist Heat and Cold");
			if (desc) return (format("Temporary opposition to fire and frost for %d+d%d turns.  Cumulative with equipment resistances.", dur, dur));
			if (cast)
			{

				dur1 = randint(dur);

				(void)set_oppose_fire(p_ptr->oppose_fire + dur1 + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur1 + dur);

			}

			break;
		}

		case PRAYER_NEUTRALIZE_POISON:
		{
			if (name) return ("Neutralize Poison");
			if (desc) return ("Cures the player of poison.");
			if (cast)
			{
				(void)set_poisoned(0);
			}

			break;
		}
		case PRAYER_ORB_OF_DRAINING:
		{
			dice = 10;
			sides = 5;
			dam = plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 1 : 2);
			rad = 2;

			if (name) return ("Orb of Draining");
			if (desc) return (format("Fires a radius %d orb of holy force that does %d+%dd%d damage.", rad, dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_orb(GF_HOLY_ORB, dir, (damroll(dice, sides) + dam), rad);
			}

			break;
		}

		case PRAYER_CURE_CRITICAL_WOUNDS:
		{
			dice = 8;
			sides = 10;

			if (name) return ("Cure Critical Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_SENSE_INVISIBLE:
		{
			dur = 24;
			if (p_ptr->tim_invis)
			{
				extra = " extra";
				dur = 10;
			}

			if (name) return ("Sense Invisible");
			if (desc) return (format("Allows player to see invisible monsters for %d%s turns.", dur, extra));
			if (cast)
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(dur) + dur);
			}

			break;
		}

		case PRAYER_PROTECTION_FROM_EVIL:
		{
			dur = 25;
			dur1 = 3 * p_ptr->lev;
			if (p_ptr->protevil)
			{
				extra = " additional";
				dur = dur / 10;
				dur1 = dur1 / 10;
			}

			if (name) return ("Protection from Evil");
			if (desc) return (format("Temporary protection from evil creatures for %dd%d%s turns.", dur1, dur, extra));
			if (cast)
			{
				(void)set_protevil(p_ptr->protevil + randint(dur) + dur1);
			}

			break;
		}

		case PRAYER_EARTHQUAKE:
		{
			rad = 10;

			if (name) return ("Earthquake");
			if (desc) return (format("Creates a radius %d earthquake centered on the player.", rad));
			if (cast)
			{
				earthquake(py, px, rad);
			}

			break;
		}

		case PRAYER_SENSE_SURROUNDINGS:
		{
			if (name) return ("Sense Surroundings");
			if (desc) return ("Maps the local area, reveals doors and stairs.");
			if (cast)
			{
				map_area();
			}
			break;

		}

		case PRAYER_CURE_MORTAL_WOUNDS:
		{
			dice = 12;
			sides = 10;

			if (name) return ("Cure Mortal Wounds");
			if (desc) return (format("Eliminates stunning and cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_TURN_UNDEAD:
		{
			if (name) return ("Turn Undead");
			if (desc) return ("Repels all undead monsters in line of sight.");
			if (cast)
			{
				(void)turn_undead(p_ptr->lev);
			}
			break;

		}

		case PRAYER_PRAYER:
		{
			dur = 48;
			if (p_ptr->blessed)
			{
				extra = " extra";

				dur = 10;
			}

			if (name) return ("Prayer");
			if (desc) return (format("Bonus to fighting ability and armour class for %d+d%d%s turns.", dur, dur, extra));
			if (cast)
			{
				(void)set_blessed(p_ptr->blessed + randint(dur) + dur);
			}

			break;
		}

		case PRAYER_SUN_BEAM:
		{
			/* Reaches max damage (85+10d10, average 140) at plev 46. */
			dam = 85;
			dice = 1 + (plev - 1) / 5;
			sides = 10;

			if (name) return ("Sun Beam");
			if (desc) return (format("Fires an narrow arc of intense light energy"
				" for %d+%dd%d hp damage.", dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_LITE, dir, dam + damroll(dice, sides), 0, 30);
			}

			break;
		}

		case PRAYER_HEAL:
		{
			dam = 325;

			if (name) return ("Heal");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_DISPEL_EVIL:
		{
			dam = plev * 3;

			if (name) return ("Dispel Evil");
			if (desc) return (format("Does 1d%d damage to all evil creatures in line of sight.", dam));
			if (cast)
			{
				(void)dispel_evil(randint(dam));
			}

			break;
		}

		case PRAYER_GLYPH_OF_WARDING:
		{
			if (name) return ("Glyph of Warding");
			if (desc) return ("Creates a glyph of warding beneath you.");
			if (cast)
			{
				(void)warding_glyph();
			}

			break;
		}

		case PRAYER_HOLY_WORD:
		{
			dam = 150;

			if (name) return ("Holy Word");
			if (desc) return (format("Dispels evil with %d hp damage."
				"  Eliminates stunning, fear, poison and cuts and heals %d hp.", dam, dam));
			{
				(void)dispel_evil(dam);
				(void)hp_player(dam);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_DETECT_MONSTERS:
		{
			if (name) return ("Detect Monsters");
			if (desc) return ("Detects all nearby creatures.");
			if (cast)
			{
				(void)detect_monsters_normal();
			}

			break;
		}

		case PRAYER_DETECTION:
		{
			if (name) return ("Detection");
			if (desc) return ("Detects all nearby traps, doors, stairs, treasure, objects, and creatures.");
			if (cast)
			{
				(void)detect_all();
			}

			break;
		}

		case PRAYER_PERCEPTION:
		{
			if (name) return ("Perception");
			if (desc) return ("Identifies an object.");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;
		}

		case PRAYER_PROBING:
		{
			if (name) return ("Probing");
			if (desc) return ("Learns many attributes of a monster or feature in sight.");
			if (cast)
			{
				(void)probing();
			}

			break;
		}

		case PRAYER_CLAIRVOYANCE:
		{
			if (name) return ("Clairvoyance");
			if (desc) return ("Lights up the entire dungeon level and shows all objects on the dungeon floor.");
			if (cast)
			{
				wiz_lite();
			}

			break;
		}

		case PRAYER_CURE_SERIOUS_WOUNDS2:
		{
			dice = 5;
			sides = 15;

			if (name) return ("Cure Serious Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}

		case PRAYER_CURE_MORTAL_WOUNDS2:
		{
			dice = 12;
			sides = 15;

			if (name) return ("Cure Mortal Wounds");
			if (desc) return (format("Eliminates stunning and cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_HEALING:
		{
			dam = 2000;

			if (name) return ("Healing");
			if (desc) return (format("Eliminates stunning and cuts and heals %d hp.", dam));
			if (cast)
			{
				(void)hp_player(dam);
				(void)set_cut(0);
				(void)set_stun(0);
			}

			break;
		}

		case PRAYER_RESTORATION:
		{
			if (name) return ("Restoration");
			if (desc) return ("Restores all stats to their maximum.");
			if (cast)
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
			}

			break;
		}

		case PRAYER_REMEMBRANCE:
		{
			if (name) return ("Remembrance");
			if (desc) return ("Restores experience to maximum.");
			if (cast)
			{
				(void)restore_level();
			}

			break;
		}

		case PRAYER_SUN_BURST:
		{
			dice = 25 + plev / 2;
			sides = 7;
			rad = 5;

			if (name) return ("Sun Burst");
			if (desc) return (format("You cast a radius %d orb of hard"
				" light that deals %dd%d damage.", rad, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_orb(GF_LITE, dir, damroll(dice, sides), rad);
			}

			break;
		}

		case PRAYER_DISPEL_EVIL2:
		{
			dice = 2;
			sides = plev * 3;

			if (name) return ("Dispel Evil");
			if (desc) return (format("Does %dd%d damage to all evil creatures in line of sight.", dice, sides));
			if (cast)
			{
				(void)dispel_evil(damroll(dice, sides));
			}

			break;
		}

		case PRAYER_BANISH_EVIL:
		{
			if (name) return ("Repel Evil");
			if (desc) return ("Attempts to teleport away all evil monsters in line of sight.");
			if (cast)
			{
				if (banish_evil(150))
				{
					msg_print("The power of your god banishes evil!");
				}
			}

			break;
		}

		case PRAYER_WORD_OF_DESTRUCTION:
		{
			rad = 15;

			if (name) return ("Word of Destruction");
			if (desc) return (format("Creates a radius %d earthquake.  Deletes all non-quest monsters.", rad));
			if (cast)
			{
				destroy_area(py, px, rad, TRUE);
			}

			break;
		}

		case PRAYER_JUDGEMENT_OF_MANDOS:
		{
			rad = 9;
			dice = 20;
			sides = 10;
			dam1 = 5 * (plev - 30);

			if (name) return ("Judgement of Mandos");
			if (desc) return (format("You release a massive starburst of holy energy."
				" Affected creatures suffer %d+%dd%d hp damage or twice as much if evil.",
				dam1, dice, sides));
			if (cast)
			{
				dam = dam1 + damroll(dice, sides);
				fire_star(GF_HOLY_ORB, dam, 9, 0L);
			}
			break;

		}

		case PRAYER_UNBARRING_WAYS:
		{
			if (name) return ("Unbarring Ways");
			if (desc) return ("Fires a beam that disarms traps and chests and destroys doors.");
			if (cast)
			{
				if (!get_aim_dir(&dir, TRUE)) return (NULL);
				destroy_door(dir);
			}

			break;
		}

		case PRAYER_RECHARGING:
		{
			if (name) return ("Recharging");
			if (desc) return ("Attempts to recharge a wand, staff, or rod.");
			if (cast)
			{
				(void)recharge(15, FALSE);
			}

			break;
		}

		case PRAYER_DISPEL_CURSE:
		{
			if (name) return ("Dispel Curse");
			if (desc) return ("Removes standard and heavy curses.");
			if (cast)
			{
				remove_curse(TRUE);
			}

			break;
		}

		case PRAYER_ENCHANT_WEAPON:
		{
			if (name) return ("Enchant Weapon");
			if (desc) return ("Attempts to enchant one weapon.");
			if (cast)
			{
				(void) enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
			}

			break;
		}

		case PRAYER_ENCHANT_ARMOUR:
		{
			if (name) return ("Enchant Armour");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
			}

			break;

		}

		case PRAYER_ELEMENTAL_BRAND:
		{
			if (name) return ("Elemental Brand");
			if (desc) return ("Attempts to brand one weapon.");
			if (cast)
			{
				(void)brand_weapon(TRUE);
			}

			break;
		}

		case PRAYER_BLINK:
		{
			dam = 10;

			if (name) return ("Blink");
			if (desc) return (format("Random minor displacement up to %d squares.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;

		}

		case PRAYER_TELEPORT_SELF:
		{
			dam = (plev * 8);

			if (name) return ("Teleport Self");
			if (desc) return (format("Random major displacement up to %d squares.", dam));
			if (cast)
			{
				teleport_player(dam);
			}

			break;
		}

		case PRAYER_TELEPORT_OTHER:
		{
			if (name) return ("Teleport Other");
			if (desc) return ("Attempts to teleport a monster away.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)teleport_monster(dir);
			}

			break;
		}

		case PRAYER_TELEPORT_LEVEL:
		{
			if (name) return ("Teleport Level");
			if (desc) return ("Immediately takes you to the next level up or down.");
			if (cast)
			{
				teleport_player_level(SOURCE_PLAYER);
			}

			break;

		}

		case PRAYER_WORD_OF_RECALL:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (cast)
			{
				set_recall();
			}

			break;
		}

		case PRAYER_ALTER_REALITY:
		{
			if (name) return ("Alter Reality");
			if (desc) return ("Redraws the current level.");
			if (cast)
			{
				/* Ironman */
				if (adult_ironman && !p_ptr->total_winner)
				{
					msg_print("Nothing happens.");
				}

				else
				{
					msg_print("The world changes!");

					/* Leaving */
					p_ptr->leaving = TRUE;
				}

			}


			break;
		}

		case PRAYER_MASS_IDENTIFY:
		{
			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (cast)
			{
				mass_identify(3);
			}

			break;
		}
	}

	/* success  */
	return ("Success!");
}


static cptr cast_spell(int mode, int tval, int index)
{
	if (tval == TV_MAGIC_BOOK)
	{
		return do_mage_spell(mode, index);
	}

	else if (tval == TV_DRUID_BOOK)
	{
		return do_druid_spell(mode, index);
	}
	else /*Priest*/
	{
		return do_priest_prayer(mode, index);
	}
}

void do_cmd_browse_aux(const object_type *o_ptr)
{
	int i;
	int spell;
	int num = 0;
	cptr spell_type;

	byte spells[SPELLS_PER_BOOK];

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (cp_ptr->spell_book == TV_PRAYER_BOOK) spell_type = "prayer";
	else spell_type = "spell";

	/* Extract spells */
	for (i = 0; i < SPELLS_PER_BOOK; i++)
	{
		spell = get_spell_index(o_ptr, i);

		/* Collect this spell */
		if (spell != -1) spells[num++] = spell;
	}

	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 2, 2);

	/* Note */
	prt("Hit any key to continue...", 0, 0);

	/* Wait for the player to press any key*/
	inkey();

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
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item;

	object_type *o_ptr;

	cptr q, s;


	/* Warriors are illiterate */
	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

#if 0

	/* No lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

#endif

	/* Restrict choices to "useful" books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

	/* Browse the book */
	do_cmd_browse_aux(o_ptr);
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;

	int spell;

	cptr p = ((cp_ptr->spell_book == TV_PRAYER_BOOK) ? "prayer" : "spell");

	cptr q, s;

	object_type *o_ptr;


	if (!cp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	if (!(p_ptr->new_spells))
	{
		msg_format("You cannot learn any new %ss!", p);
		return;
	}


	/* Restrict choices to "useful" books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

	/* Mage -- Learn a selected spell */
	if (cp_ptr->flags & CF_CHOOSE_SPELLS)
	{
		/* Ask for a spell */
		spell = get_spell(o_ptr, "study", FALSE);

		/* Allow cancel */
		if (spell == -1) return;
	}
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (i = 0; i < SPELLS_PER_BOOK; i++)
		{
			spell = get_spell_index(o_ptr, i);

			/* Skip empty spells */
			if (spell == -1) continue;

			/* Skip non "okay" prayers */
			if (!spell_okay(spell, FALSE)) continue;

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
		msg_format("You cannot learn any %ss in that book.", p);

		/* Abort */
		return;
	}


	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Learn the spell */
	p_ptr->spell_flags[spell] |= PY_SPELL_LEARNED;

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < PY_MAX_SPELLS; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i] = spell;

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, get_spell_name(cp_ptr->spell_book, spell));

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.",
		           p_ptr->new_spells, p,
		           (p_ptr->new_spells != 1) ? "s" : "");
	}

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

	/* Redraw object recall */
	p_ptr->window |= (PW_OBJECT);
}



/*
 * Cast a spell
 */
void do_cmd_cast(bool pray)
{
	int item, spell;
	int chance;

	object_type *o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	bool prayer = cp_ptr->spell_book == TV_PRAYER_BOOK ? TRUE : FALSE;

	/* Require spell ability */
	if (cp_ptr->spell_book == 0)
	{
		if (pray) msg_print("Pray hard enough and your prayers may be answered.");
		else msg_print("You cannot cast spells!");
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices to spell books */
	item_tester_tval = cp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	if (prayer) s = "You have no prayer books!";
	else s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

	/* Ask for a spell */
	spell = get_spell(o_ptr, "cast", TRUE);

	if (spell < 0)
	{
		if (spell == -2)
		{
			if (prayer) msg_print("You don't know any prayers in that book.");
			else msg_print("You don't know any spells in that book.");
		}
		return;
	}

	/* Get the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		if (prayer) msg_print("You do not have enough mana to cast this prayer.");
		else msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		sound(MSG_SPELL);
		if (cast_spell(MODE_SPELL_CAST, cp_ptr->spell_book, spell) == NULL) return;

		/* A spell was cast */
		if (!(p_ptr->spell_flags[spell] & PY_SPELL_WORKED))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			p_ptr->spell_flags[spell] |= PY_SPELL_WORKED;

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			/* Redraw object recall */
			p_ptr->window |= (PW_OBJECT);
		}
	}

	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Sufficient mana */
	if (s_ptr->smana <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= s_ptr->smana;
	}

	/* Over-exert the player */
	else
	{
		int oops = s_ptr->smana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

