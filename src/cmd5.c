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
	return 1; /* always OK now */
}

/*
 * Return the player realm for the spell_list table.  Assumes player is a spellcaster.
 * We don't return any error because this value is going to be looked up in a table,
 * & would cause the game to crash
 */
int get_player_spell_realm(void)
{
	return (MAGE_REALM);
}


static int get_spell_index(const object_type *o_ptr, int index)
{

	/* Get the item's sval */
	int sval = o_ptr->sval;

	return sval*6+index;
}


/*
 * Print a list of spells (for browsing or casting or viewing).
 */

void print_spells(const byte *spells, int num, int row, int col)
{
	int i, spell;

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
	put_str("Mana Fail Info", row, col + 32);

	/* Dump the spells */
	for (i = 0; i < num; i++)
	{
		int use_indent = 0;

		/* Get the spell index */
		spell = spells[i];

		/* Get extra info */
		comment = do_mage_spell(MODE_SPELL_DESC, spell);

		line_attr = TERM_WHITE;

		/* Dump the spell --(-- */
		if (get_mana_cost(spell) >= 1){
			strnfmt(out_val, sizeof(out_val), "  %c) %-27s%2d  %3d%% %s",
		        I2A(i), get_spell_name(cp_ptr->spell_book, spell),
		        get_mana_cost(spell), 100-get_success_prob(spell), comment);
		} else {	
			strnfmt(out_val, sizeof(out_val), "  %c) %-27s%3s  %3d%% %s",
		        I2A(i), get_spell_name(cp_ptr->spell_book, spell),
		        nice_mana_cost(get_mana_cost(spell)), 100-get_success_prob(spell), comment);
		}

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
	return newspells[spell].name;
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

	cptr p = "spell";

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
			if (get_mana_cost(spell) >= 1){
				strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
			        prompt, get_spell_name(cp_ptr->spell_book, spell),
			        get_mana_cost(spell), 100-get_success_prob(spell));
			} else {
				strnfmt(tmp_val, 78, "%^s %s (%s mana, %d%% fail)? ",
			        prompt, get_spell_name(cp_ptr->spell_book, spell),
			        nice_mana_cost(get_mana_cost(spell)), 100-get_success_prob(spell));
			}

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
	return plev;
}


static void spell_wonder(int dir)
{
/* This spell should become more useful (more
   controlled) as the player gains experience levels.
   Thus, add 1/5 of the player's level to the die roll.
   This eliminates the worst effects later on, while
   keeping the results quite random.  It also allows
   some potent effects only at high level. */

	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die = randint(100) + plev / 5;
	int beam = beam_chance();

	if (die > 100)
		msg_print("You feel a surge of power!");
	if (die < 8) clone_monster(dir);
	else if (die < 14) speed_monster(dir);
	else if (die < 26) heal_monster(dir, damroll(4, 6));
	else if (die < 31) poly_monster(dir);
	else if (die < 36)
		fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
		                  damroll(3 + ((plev - 1) / 5), 4));
	else if (die < 41) confuse_monster(dir, plev);
	else if (die < 46) fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
	else if (die < 51) lite_line(dir, damroll(6, 8));
	else if (die < 56)
		fire_beam(GF_ELEC, dir, damroll(3+((plev-5)/6), 6), PROJECT_GRID);
	else if (die < 61)
		fire_bolt_or_beam(beam-10, GF_COLD, dir,
		                  damroll(5+((plev-5)/4), 8));
	else if (die < 66)
		fire_bolt_or_beam(beam, GF_ACID, dir,
		                  damroll(6+((plev-5)/4), 8));
	else if (die < 71)
		fire_bolt_or_beam(beam, GF_FIRE, dir,
		                  damroll(8+((plev-5)/4), 8));
	else if (die < 76) drain_life(dir, 75);
	else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
	else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
	else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
	else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
	else if (die < 101) drain_life(dir, 100 + plev);
	else if (die < 104) earthquake(py, px, 12);
	else if (die < 106) destroy_area(py, px, 15, TRUE);
	else if (die < 108) banishment();
	else if (die < 110) dispel_monsters(120);
	else /* RARE */
	{
		dispel_monsters(150);
		slow_monsters(damroll (2, p_ptr->lev));
		sleep_monsters(damroll (2, p_ptr->lev));
		hp_player(300);
	}
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
	int rad, mult;

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

		case DRUID_LIGHTNING_BEAM:
		{
			dice = 3 + ((plev - 1) / 6);
			sides = 6;

			if (name) return ("Lightning Beam");
			if (desc) return (format("Fires a beam of lightning for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_beam(GF_ELEC, dir, damroll(dice, sides), 0L);
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
			if (name) return ("Detect Terrain");
			if (desc) return ("Maps all natural features on the entire dungeon level.");
			if (cast)
			{
				detect_terrain();
			}
			break;

		}

		case DRUID_CURE_MORTAL_WOUNDS:
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

		case DRUID_BEAM_LIGHT:
		{
			dice = 6 + (plev-5) / 4;
			sides = 8;

			if (name) return ("Beam of Light");
			if (desc) return (format("Fires a beam of strong light for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				msg_print("A line of orange shimmering light appears.");
				(void)fire_beam(GF_LITE, dir, damroll(dice, sides), PROJECT_GRID);
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
			dam = 30 + (plev * 5 / 2);
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
			dam = 55 +  (plev * 5 / 2);
			rad = 2;

			if (name) return ("Fire Ball");
			if (desc) return (format("Fires a radius %d ball of fire for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_FIRE, dir, dam, rad);
			}

			break;
		}

		case DRUID_DRAIN_LIFE_ARC:
		{
			dam = 125;
			dice = 2;
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
				(void)do_res_stat(A_AGI);
				(void)do_res_stat(A_STE);
				(void)do_res_stat(A_PER);
				(void)do_res_stat(A_LUC);
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

		case DRUID_WATER_BALL:
		{
			dam = 15;
			mult = 12;
			rad = 2;

			if (name) return ("Water Ball");
			if (desc) return (format("Fires a radius %d ball of water for %d hp damage.  Damage is multiplied by %d if caster is standing in water.", rad, dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in water (boiling water included)*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, ELEMENT_WATER))
				{
					dam *= mult;
					rad = 3;
				}

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_WATER, dir, dam, rad);
			}

			break;
		}


		case DRUID_SANDSTORM:
		{
			dam = 20;
			mult = 10;

			if (name) return ("Sandstorm");
			if (desc) return (format("Fires a sandstorm arc for %d hp damage.  Damage is multiplied by %d if caster is standing in sand.", dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in sand*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_SAND)) dam *= mult;

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_SAND, dir, dam, 0, 30);
			}

			break;
		}

		case DRUID_ICICLE:
		{
			dam = 30;
			mult = 9;

			if (name) return ("Icicles");
			if (desc) return (format("Fires a beam of icicles for %d hp damage.  Damage is multiplied by %d if caster is standing in ice.", dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in ice*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_ICE)) dam *= mult;

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_beam(GF_ICE, dir, dam, 0L);
			}

			break;
		}

		case DRUID_GEYSER:
		{
			dam = 35;
			mult = 12;

			if (name) return ("Geyser");
			if (desc) return (format("Fires an arc of boiling water for %d hp damage.  Damage is multiplied by %d if caster is standing in boiling water.", dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in boiling water*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, TERRAIN_MASK) == ELEMENT_BWATER) dam *= mult;

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_BWATER, dir, dam, 0, 30);
			}

			break;
		}

		case DRUID_BMUDBALL:
		{
			dam = 40;
			mult = 15;
			rad = 2;

			if (name) return ("Boiling Mud");
			if (desc) return (format("Fires a radius %d ball of boiling mud for %d hp damage.  Damage is multiplied by %d if caster is standing in boiling mud.", rad, dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in boiling mud*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, TERRAIN_MASK) == ELEMENT_BMUD)
				{
					dam *= mult;
					rad = 3;
				}

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_BMUD, dir, dam, rad);
			}

			break;
		}

		case DRUID_LAVA_BALL:
		{
			dam = 30;
			mult = 25;
			rad = 2;

			if (name) return ("Ball of Lava");
			if (desc) return (format("Fires a radius %d ball of lava for %d hp damage.  Damage is multiplied by %d if caster is standing in lava.", rad, dam, mult));
			if (cast)
			{
				/*boiling water and boiling mud are excluded*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, TERRAIN_MASK) == ELEMENT_LAVA)
				{
					dam *= mult;
					rad = 3;
				}

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_LAVA, dir, dam, rad);
			}

			break;
		}



		default: break;
	}

	/* success  */
	return ("success");
}

void create_object(int tval, int sval)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type object_type_body;
	object_type *i_ptr;

	/* Get local object */
	i_ptr = &object_type_body;

	/* make object */
	object_prep(i_ptr, lookup_kind(tval, sval));

	/* Apply magic (no messages, no artifacts) */
	apply_magic(i_ptr, danger(p_ptr->depth), FALSE, FALSE, FALSE, FALSE);

	object_aware(i_ptr);
	object_known(i_ptr);

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, py, px);
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
	int rad, mult;
	int drop_type;
	cptr extra = "";
	int i,j,k;

	/* Function modes */
	bool cast = FALSE;
	bool name = FALSE;
	bool desc = FALSE;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam;
	beam = beam_chance();

	/* Function modes */
	if (mode == MODE_SPELL_CAST) cast = TRUE;
	if (mode == MODE_SPELL_NAME) name = TRUE;
	if (mode == MODE_SPELL_DESC) desc = TRUE;

	/* Spells. */
	switch (spell)
	{

		case NEWSPELL_MAGIC_MISSILE:
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

		case NEWSPELL_DETECT_ANIMALS:
		{
			if (name) return ("Detect Animals");
			if (desc) return ("Detects nearby animals.");
			if (cast)
			{
				(void)detect_monsters_animal();
			}

			break;
		}

		case NEWSPELL_DETECT_MONSTERS:
		{
			if (name) return ("Detect Monsters");
			if (desc) return ("Detects nearby monsters that are not invisible.");
			if (cast)
			{
				(void)detect_monsters_normal();
			}

			break;
		}

		case NEWSPELL_CREATE_FOOD:
		{
			if (name) return ("Create Food");
			if (desc) return ("Produces some nutritious food for you to eat.");
			if (cast)
			{
				(void)create_object(TV_FOOD,SV_FOOD_RATION);
			}

			break;
		}

		case NEWSPELL_MAGELIGHT:
		{
			if (name) return ("Magelight");
			if (desc) return ("Produces a magical torch to light the dungeon.");
			if (cast)
			{
				(void)create_object(TV_LITE,SV_LITE_MAGELIGHT);
			}

			break;
		}

		case NEWSPELL_BARKSKIN:
		{

			dur = 25 + p_ptr->lev;

			if (name) return ("Bark Skin");
			if (desc) return (format("Temporarily increases armour class by 30 for %d turns.", dur));
			if (cast)
			{
				(void)set_shield(p_ptr->shield + dur);
			}

			break;
		}
		
		case NEWSPELL_GLOBE_OF_INVULNERABILITY:
		{

			dur = 10 + p_ptr->lev/2;

			if (name) return ("Globe of Invulnerability");
			if (desc) return (format("Increases armour class and saving throw for %d turns.", dur));
			if (cast)
			{
				(void)set_megashield(p_ptr->megashield + dur);
			}

			break;
		}
		
		case NEWSPELL_PHASE_DOOR:
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

		case NEWSPELL_LIGHT:
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

		case NEWSPELL_DETECT_TREASURE:
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

		case NEWSPELL_CLW:
		{
			dice = 3;
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

		case NEWSPELL_DETECT_OBJECTS:
		{
			if (name) return ("Detect Objects");
			if (desc) return ("Detects nearby objects.");
			if (cast)
			{
				(void)detect_objects_normal();
			}

			break;
		}

		case NEWSPELL_DETECT_TRAPS_AND_DOORS:
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

		case -3 /* was SPELL_STINKING_CLOUD */:
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

		case NEWSPELL_CONFUSION:
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

		case NEWSPELL_MANA_BOLT:
		{
			dice = 5 + ((plev - 5) / 4);
			sides = 10;

			if (name) return ("Mana Bolt");
			if (desc) return (format("Fires a bolt or beam of raw mana for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam-10, GF_MANA, dir, damroll(dice, sides));
			}

			break;
		}

		case -4 /* was SPELL_LIGHTNING_BOLT */ :
		{
			dice = 3 + ((plev - 5) / 6);
			sides = 6;

			if (name) return ("Lightning Bolt");
			if (desc) return (format("Fires a beam of lightning for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_beam(GF_ELEC, dir, damroll(dice, sides), 0L);
			}

			break;
		}

		case -5 /* was SPELL_TRAP_DOOR_DESTRUCTION */:
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

		case NEWSPELL_ENCHANT_ARMOR:
		{
			if (name) return ("Enchant Armour");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
			}

			break;

		}
		
		case NEWSPELL_SLEEP:
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

		case -6 /* was SPELL_CURE_POISON */:
		{
			if (name) return ("Cure Poison");
			if (desc) return ("Cures the player of poison.");
			if (cast)
			{
				(void)set_poisoned(0);
			}

			break;
		}

		case NEWSPELL_TELEPORTATION:
		{
			dam = (plev * 5);

			if (name) return ("Teleport Self");
			if (desc) return (format("Delayed random displacement up to %d squares.", dam));
			if (cast)
			{
				msg_print("Magical forces begin to gather around you...");
				delayed_teleport_player(dam);
			}

			break;
		}

		case -7 /* was SPELL_SPEAR_OF_LIGHT */:
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

		case NEWSPELL_FROST_BOLT /* was SPELL_FROST_BOLT */:
		{
			dice = 5 + ((plev - 5) / 4);
			sides = 12;

			if (name) return ("Frost Bolt");
			if (desc) return (format("Fires a bolt or beam of frost for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);

				fire_bolt_or_beam(beam-10, GF_COLD, dir, damroll(dice, sides));
			}

			break;
		}

		case NEWSPELL_NATIVITY:
		{

			dur = 50 + p_ptr->lev;

			if (name) return ("Nativity");
			if (desc) return (format("Temporary nativity to sand, mud, oil, water and lava."));
			if (cast)
			{
				(void)set_temp_native_lava(p_ptr->temp_native_lava + dur);
				(void)set_temp_native_oil(p_ptr->temp_native_oil + dur);
				(void)set_temp_native_water(p_ptr->temp_native_water + dur);
				(void)set_temp_native_mud(p_ptr->temp_native_mud + dur);
				(void)set_temp_native_sand(p_ptr->temp_native_sand + dur);
			}

			break;
		}
		
		case NEWSPELL_CREATE_MISSILES:
		{

			if (name) return ("Create Missiles");
			if (desc) return ("Creates arrows, shot or bolts depending on your equipped missile weapon.");
			if (cast)
			{
				if (p_ptr->ammo_tval == TV_SHOT){	
					drop_type = DROP_TYPE_SHOT;
				} else if (p_ptr->ammo_tval == TV_BOLT){
					drop_type = DROP_TYPE_BOLTS;
				} else if (p_ptr->ammo_tval == TV_ARROW){
					drop_type = DROP_TYPE_ARROWS;
				} else {
					if (one_in_(3)){
						drop_type = DROP_TYPE_SHOT;
					} else if (one_in_(2)){
						drop_type = DROP_TYPE_BOLTS;
					} else {
						drop_type = DROP_TYPE_ARROWS;
					}
				}
				/* Try up to 11 spots looking for empty space */
				for (i = 0; i < 11; ++i)
				{
					/* Pick a random location */
					while (1)
					{
						j = rand_spread(py, 1);
						k = rand_spread(px, 1);
						if (!in_bounds(j, k)) continue;
						break;
					}

					/* Require "clean" floor space */
					if (!cave_clean_bold(j, k)) continue;

					place_object(j, k, one_in_((40 - p_ptr->lev)/10+1), 0, drop_type);

					/* Placement accomplished */
					break;
				}
			
			}

			break;
		}

		case NEWSPELL_STONE_TO_MUD:
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

		case -10 /* was SPELL_SATISFY_HUNGER */:
		{
			if (name) return ("Satisfy Hunger");
			if (desc) return ("Magically renders the player well-fed.");
			if (cast)
			{
				(void)set_food(PY_FOOD_MAX - 50);
			}

			break;
		}

		case -18 /* was SPELL_WONDER */: /* wonder */
		{
			if (name) return ("Wonder");
			if (desc) return ("Invokes strange and random magic.");
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)spell_wonder(dir);
			}

			break;
		}

		case -11 /* was SPELL_POLYMORPH_OTHER */:
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

		case NEWSPELL_IDENTIFY:
		{
			if (name) return ("Identify");
			if (desc) return ("Identifies an object.");
			if (cast)
			{
				if (!ident_spell()) return (NULL);
			}

			break;

		}

		case NEWSPELL_STARIDENTIFYSTAR:
		{
			if (name) return ("*Identify*");
			if (desc) return ("Reveals all normal and hidden powers of an object.");
			if (cast)
			{
				if (!identify_fully()) return (NULL);
			}

			break;

		}

		case -12 /* was SPELL_MASS_SLEEP */:
		{
			if (name) return ("Mass Sleep");
			if (desc) return ("Attempts to sleep all monsters in LOS.");
			if (cast)
			{
				(void)sleep_monsters(damroll (2, p_ptr->lev));
			}

			break;
		}

		case NEWSPELL_FIRE_BOLT:
		{
			dice = 5 + ((plev - 5) / 4);
			sides = 12;

			if (name) return ("Fire Bolt");
			if (desc) return (format("Fires a bolt or beam of fire for %dd%d hp damage", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				(void)fire_bolt_or_beam(beam + 5, GF_FIRE, dir, damroll(dice, sides));
			}

			break;
		}

		case NEWSPELL_SLOW:
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

		case -14 /* was SPELL_FROST_BALL */:
		{
			dam = 30 + (plev);
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

		case NEWSPELL_TELEPORT_AWAY:
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

		case -15 /* was SPELL_BEDLAM */:
		{
			dice = 2;
			sides = plev;
			rad = 4;

			if (name) return ("Bedlam");
			if (desc) return (format("Fires a radius %d ball of confusion for %dd%d hp damage", rad, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_OLD_CONF, dir, damroll(dice, sides), rad);
			}

			break;
		}

		case NEWSPELL_FIREBALL:
		{
			dam = 50 + 2*plev;
			rad = 3;

			if (name) return ("Fire Ball");
			if (desc) return (format("Fires a radius %d ball of fire for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_FIRE, dir, dam, rad);
			}

			break;
		}

		case -16 /* SPELL_WORD_OF_DESTRUCTION */:
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

		case NEWSPELL_SANDSTORM:
		{
			dam = 20;
			mult = 10;

			if (name) return ("Sandstorm");
			if (desc) return (format("Fires a sandstorm arc for %d hp damage.  Damage is multiplied by %d if caster is standing in sand.", dam, mult));
			if (cast)
			{
				/*re-calc the damage if the player is in sand*/
				if (cave_ff3_match(p_ptr->py, p_ptr->px, FF3_SAND)) dam *= mult;

				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_arc(GF_SAND, dir, dam, 0, 30);
			}

			break;
		}
		
		case -17 /* SPELL_BANISHMENT */:
		{
			if (name) return ("Banishment");
			if (desc) return ("Banishes one type of monster.  Uniques and quest monsters are unaffected");
			if (cast)
			{
				(void)banishment();
			}

			break;
		}

		case -33 /* SPELL_DOOR_CREATION */:
		{
			if (name) return ("Door Creation");
			if (desc) return ("Creates a barrier of doors around you.");
			if (cast)
			{
				(void)door_creation();
			}

			break;
		}

		case NEWSPELL_CREATE_STAIRS:
		{
			if (name) return ("Stair Creation");
			if (desc) return ("Creates a staircase nearby.");
			if (cast)
			{
				(void)stair_creation();
			}

			break;
		}

		case -34 /* was SPELL_TELEPORT_LEVEL */:
		{
			if (name) return ("Teleport Level");
			if (desc) return ("Immediately takes you to the next level up or down.");
			if (cast)
			{
				teleport_player_level(SOURCE_PLAYER);
			}

			break;
		}

		case -20 /* was SPELL_EARTHQUAKE */:
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

		case -21 /* was SPELL_WORD_OF_RECALL */:
		{
			if (name) return ("Word of Recall");
			if (desc) return ("Recalls you to the town, or to the recall level in the dungeon.");
			if (cast)
			{
				set_recall();
			}

			break;
		}

		case -22 /* was SPELL_ACID_BOLT */:
		{
			dice = 8 + ((plev - 5) / 4);
			sides = 8;

			if (name) return ("Acid bolt");
			if (desc) return (format("Fires a beam of acid for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam, GF_ACID, dir, damroll(dice, sides));
			}

			break;
		}

		case -23 /* SPELL_CLOUD_KILL */:
		{
			dam = 40 + (plev / 2);
			rad = 3;

			if (name) return ("Cloudkill");
			if (desc) return (format("Fires a radius %d ball of poison for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_POIS, dir, dam, rad);
			}

			break;
		}

		case NEWSPELL_ACID_BALL:
		{
			dam = 40 + 2*plev;
			rad = 4;

			if (name) return ("Acid ball");
			if (desc) return (format("Fires a radius %d ball of acid for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_ACID, dir, dam, rad);
			}

			break;
		}

		case -24 /* SPELL_ICE_STORM */:
		{
			dam = 50 + (plev * 2);
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

		case -25 /* SPELL_METEOR_SWARM */:
		{
			dice = 2 + plev / 20;

			dam = 30 + plev / 2;

			if (name) return ("Meteor Swarm");
			if (desc) return (format("Fires %d meteors, each causing %d hp damage.", dice, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_swarm(dice, GF_METEOR, dir, dam, 1);
			}

			break;
		}

		case NEWSPELL_MANA_STORM:
		{
			dam = 300 + (plev * 2);
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
		case NEWSPELL_SEE_INVISIBLE:
		{
			if (name) return ("Detect Invisible");
			if (desc) return ("Detects invisible monsters.");
			if (cast)
			{
				(void)detect_monsters_invis();
			}

			break;

		}

		case -27 /* was SPELL_DETECT_ENCHANTMENT */:
		{
			if (name) return ("Detect Enchantment");
			if (desc) return ("Detects nearby enchanted objects.");
			if (cast)
			{
				(void)detect_objects_magic();
			}

			break;
		}

		case -28 /* was SPELL_SHOCK_WAVE */:
		{
			dam = 10 + plev;
			rad = 2;

			if (name) return ("Shock Wave");
			if (desc) return (format("Fires a radius %d ball of sound for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_SOUND, dir, dam, rad);
			}

			break;
		}

		case -29 /* was SPELL_EXPLOSION */:
		{
			dam = 20 + (plev * 2);
			rad = 2;

			if (name) return ("Explosion");
			if (desc) return (format("Fires a radius %d ball of shards for %d hp damage.", rad, dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_ball(GF_SHARD, dir, dam, rad);
			}

			break;
		}

		case -30 /* was SPELL_MASS_BANISHMENT */:
		{
			if (name) return ("Mass Banishment");
			if (desc) return ("Banishes nearby monsters.  Uniques and quest monsters are unaffected.");
			if (cast)
			{
				(void)mass_banishment();
			}

			break;
		}

		case -31 /* was SPELL_RESIST_FIRE */:
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

		case -32 /* was SPELL_RESIST_COLD */:
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

		case -36 /* SPELL_ELEMENTAL_BRAND */:
		{
			if (name) return ("Elemental Brand");
			if (desc) return ("Attempts to brand one set of ammunition.");
			if (cast)
			{
				(void)brand_ammo(TRUE);
			}

			break;
		}

		case NEWSPELL_RESIST_POISON:
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

		case NEWSPELL_RESISTANCE:
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

		case -35 /* SPELL_FLIGHT */:
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

		case -43 /* SPELL_SHIELD */:
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

		case -37 /* SPELL_BERSERKER */:
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

		case NEWSPELL_HASTE_SELF:
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

		case NEWSPELL_RIFT:
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

		case -38 /* SPELL_REND_SOUL */:
		{
			dice = 11;
			sides = plev;

			if (name) return ("Rend Soul");
			if (desc) return (format("Fires a bolt or beam of nether for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam / 4, GF_NETHER, dir, damroll(dice, sides));
			}

			break;
		}

		case -39 /* SPELL_CHAOS_STRIKE */:
		{
			dice = 13;
			sides = plev;

			if (name) return ("Chaos Strike");
			if (desc) return (format("Fires a bolt or beam of chaos for %dd%d hp damage.", dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(dice, sides));
			}

			break;
		}

		case -40 /* SPELL_RUNE_OF_PROTECTION */:
		{
			if (name) return ("Rune of Protection");
			if (desc) return ("Creates a rune of protection beneath you.");
			if (cast)
			{
				(void)warding_glyph();
			}

			break;
		}

		case -41 /* SPELL_ENCHANT_ARMOR */:
		{
			if (name) return ("Enchant Armor");
			if (desc) return ("Attempts to enchant one piece of armor.");
			if (cast)
			{
				(void)enchant_spell(0, 0, rand_int(3) + plev / 20);
			}

			break;
		}

		case -42 /* SPELL_ENCHANT_WEAPON */:
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

		case NEWSPELL_MASS_IDENTIFY:
		{
			if (name) return ("Mass Identify");
			if (desc) return ("Identifies all nearby objects, including player equipment and inventory.");
			if (cast)
			{
				mass_identify(3);
			}

			break;
		}

		case NEWSPELL_BLESS:
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

		case NEWSPELL_REMOVE_FEAR:
		{

			if (name) return ("Remove Fear");
			if (desc) return ("Removes fear.");
			if (cast)
			{
				(void)set_afraid(0);
			}

			break;
		}

		case NEWSPELL_PORTAL:
		{
			dam = plev * 3;

			if (name) return ("Portal");
			if (desc) return (format("Delayed random displacement up to %d squares.", dam));
			if (cast)
			{
				msg_print("Mystic forces begin to gather around you...");
				delayed_teleport_player(dam);
			}

			break;
		}

		case NEWSPELL_REMOVE_CURSE:
		{
			if (name) return ("Remove Curse");
			if (desc) return ("Removes standard curses.");
			if (cast)
			{
				remove_curse(FALSE);
			}

			break;
		}

		case NEWSPELL_CCW:
		{
			dice = 12;
			sides = 12;

			if (name) return ("Cure Critical Wounds");
			if (desc) return (format("Eliminates cuts and heals %dd%d hp.", dice, sides));
			if (cast)
			{
				(void)hp_player(damroll(dice, sides));
				(void)set_cut(0);
			}

			break;
		}
		
		case NEWSPELL_RESIST_HEAT_AND_COLD:
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

		case NEWSPELL_PROTECTION_FROM_EVIL:
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

		case NEWSPELL_MAGIC_MAPPING:
		{
			if (name) return ("Magic Mapping");
			if (desc) return ("Maps the local area, reveals doors and stairs.");
			if (cast)
			{
				map_area();
			}
			break;

		}

		case NEWSPELL_DETECTION:
		{
			if (name) return ("Detection");
			if (desc) return ("Detects all nearby traps, doors, stairs, treasure, objects, and creatures.");
			if (cast)
			{
				(void)detect_all();
			}

			break;
		}
		
		case NEWSPELL_RESTORATION:
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
				(void)do_res_stat(A_AGI);
				(void)do_res_stat(A_STE);
				(void)do_res_stat(A_PER);
				(void)do_res_stat(A_LUC);
			}

			break;
		}

		case NEWSPELL_RESTORE_LIFE_LEVELS:
		{
			if (name) return ("Remembrance");
			if (desc) return ("Restores experience to maximum.");
			if (cast)
			{
				(void)restore_level();
			}

			break;
		}
		
		case NEWSPELL_STARREMOVE_CURSESTAR:
		{
			if (name) return ("Dispel Curse");
			if (desc) return ("Removes standard and heavy curses.");
			if (cast)
			{
				remove_curse(TRUE);
			}

			break;
		}
		
		case NEWSPELL_CSW:
		{
			dice = 8;
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

		case NEWSPELL_NEUTRALIZE_POISON:
		{
			if (name) return ("Neutralize Poison");
			if (desc) return ("Cures the player of poison.");
			if (cast)
			{
				(void)set_poisoned(0);
			}

			break;
		}
		
		case NEWSPELL_HEALING:
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

		case NEWSPELL_DETECT_EVIL:
		{
			if (name) return ("Detect Evil");
			if (desc) return ("Detects all nearby evil monsters, even invisible ones.");
			if (cast)
			{
				(void)detect_monsters_evil();
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

		case PRAYER_BOLT_OF_DRAINING:
		{
			dice = 2;
			sides = 4;
			dam = (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 3));

			if (name) return ("Bolt of Draining");
			if (desc) return (format("Fires a holy beam for %d+%dd%d damage.", dam, dice, sides));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				fire_bolt(GF_HOLY_ORB, dir, (damroll(dice, sides) + dam));
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
			if (desc) return (format("Delayed random displacement up to %d squares.", dam));
			if (cast)
			{
				msg_print("Mystic forces begin to gather around you...");
				delayed_teleport_player(dam);
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
			dice = 3;
			sides = 6;
			dam = plev + (plev / ((cp_ptr->flags & CF_BLESS_WEAPON) ? 2 : 4));
			rad = (plev < 30) ? 2 : 3;

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
			if (desc) return ("Attempts to scare all undead monsters in line of sight.");
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

		case PRAYER_DISPEL_UNDEAD:
		{
			dam = plev * 3;

			if (name) return ("Dispel Undead");
			if (desc) return (format("Does 1d%d damage to all undead creatures in line of sight.", dam));
			if (cast)
			{
				(void)dispel_undead(randint(dam));
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
			dam = 1500;
			dam1 = (plev * 4);

			if (name) return ("Holy Word");
			if (desc) return (format("Dispels evil with 1d%d hp damage.  Eliminates stunning, fear, poison and cuts and heals %d hp.", dam1, dam));
			if (cast)
			{
				(void)dispel_evil(randint(dam1));
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
			if (desc) return ("Permanently lights up the entire dungeon level.");
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
				(void)do_res_stat(A_AGI);
				(void)do_res_stat(A_STE);
				(void)do_res_stat(A_PER);
				(void)do_res_stat(A_LUC);
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

		case PRAYER_DISPEL_UNDEAD2:
		{
			dice = 2;
			sides = plev * 3;

			if (name) return ("Dispel Undead");
			if (desc) return (format("Does %dd%d damage to all undead creatures in line of sight.", dice, sides));
			if (cast)
			{
				(void)dispel_undead(damroll(dice, sides));
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
			if (name) return ("Banish Evil");
			if (desc) return ("Attempts to scare all evil monsters in line of sight.");
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

		case PRAYER_ANNIHILATION:
		{
			dam = 200;

			if (name) return ("Annihilation");
			if (desc) return (format("Strikes at the soul of a living monster for %d hp damage.", dam));
			if (cast)
			{
				if (!get_aim_dir(&dir, FALSE)) return (NULL);
				drain_life(dir, dam);
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
			if (desc) return (format("Delayed displacement up to %d squares.", dam));
			if (cast)
			{
				msg_print("Mystic forces begin to gather around you...");
				delayed_teleport_player(dam);
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
			if (desc) return ("Transport to the next level up or down.");
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
	do_mage_spell(mode, index);
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

	spell_type = "spell";

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


	item_tester_tval = TV_MAGIC_BOOK;

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

int get_mana_cost(int spell_code) /* does not include wand bonus */
{
	int result, school1_talisman=0, school1_other=0, school2_talisman=0, school2_other=0;
	object_type *o_ptr;
	int i;

	/* Scan through the slots backwards */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		if (o_ptr->tval==TV_TALISMAN){
			if (newspells[spell_code].school1==o_ptr->sval){
				school1_talisman = 1;
			}
			if (newspells[spell_code].school2==o_ptr->sval){
				school2_talisman = 1;
			}
		}
	}
	for (i = INVEN_WIELD; i < END_EQUIPMENT; ++i)
	{
		/* Object */
		o_ptr = &inventory[i];

		/* Skip empty objects */
		if (!o_ptr->k_idx) continue;

		if (o_ptr->ego_num==53 || o_ptr->ego_num==71){ /* digger of Dwarvenkind, helm of seeing */
			if (newspells[spell_code].school1==SCHOOL_DETECTION){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_DETECTION){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==68 || o_ptr->ego_num==76 || (o_ptr->tval==TV_AMULET && o_ptr->sval==SV_AMULET_THE_MAGI) || (o_ptr->tval==TV_CROWN && o_ptr->sval==SV_WIZARD_HAT)){ /* * of the Magi, wizard hat */
			if (newspells[spell_code].school1==SCHOOL_IDENTIFICATION){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_IDENTIFICATION){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==57 || (o_ptr->tval==TV_AMULET && o_ptr->sval==SV_AMULET_REGEN)){ /* * of Regeneration */
			if (newspells[spell_code].school1==SCHOOL_HEALING){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_HEALING){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==21 || (o_ptr->tval==TV_HAFTED && o_ptr->sval==SV_MACE_OF_DISRUPTION) || (o_ptr->tval==TV_AMULET && o_ptr->sval==SV_AMULET_DEVOTION)){ /* Amulet of Devotion, Mace of Disruption, Law DSM */
			if (newspells[spell_code].school1==SCHOOL_BLESSINGS){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_BLESSINGS){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==2 || o_ptr->ego_num==11 || o_ptr->ego_num==75 || (o_ptr->tval==TV_AMULET && o_ptr->sval==SV_AMULET_ADORNMENT)){ /* Amulet of Adornment, Shield or Armor of Elvenkind, * of Aman */
			if (newspells[spell_code].school1==SCHOOL_ENCHANTMENT){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_ENCHANTMENT){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==22 || (o_ptr->tval==TV_SWORD && o_ptr->sval==SV_WARLOCK_SWORD)){ /* Chaos DSM, Warlock Sword */
			if (newspells[spell_code].school1==SCHOOL_BATTLE){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_BATTLE){
				school2_other = 1;
			}
		}

		if ((o_ptr->tval==TV_CLOAK && o_ptr->sval==SV_DISPLACER_CLOAK) || (o_ptr->tval==TV_RING && o_ptr->sval==SV_RING_TELEPORTATION) || (o_ptr->tval==TV_AMULET && o_ptr->sval==SV_AMULET_TELEPORT)){ /* Ring or Amulet of Teleportation, Displacer Cloak */
			if (newspells[spell_code].school1==SCHOOL_TELEPORTATION){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_TELEPORTATION){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==8 || (o_ptr->tval==TV_SHIELD && o_ptr->sval==SV_SHIELD_OF_DEFLECTION)){ /* Shield of Deflection, Dwarven Armor */
			if (newspells[spell_code].school1==SCHOOL_RESISTANCE){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_RESISTANCE){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==28 || (o_ptr->tval==TV_RING && o_ptr->sval==SV_RING_FLAMES)){ /* Ring of Flames, Red dragon armor/shield */
			if (newspells[spell_code].school1==SCHOOL_FIRE_AND_LIGHT){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_FIRE_AND_LIGHT){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==18){ /* PDSM, good luck */
			if (newspells[spell_code].school1==SCHOOL_FORCE){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_FORCE){
				school2_other = 1;
			}
		}
		
		if (o_ptr->ego_num==2 || o_ptr->ego_num==11){ /* Shield or Armor of Elvenkind */
			if (newspells[spell_code].school1==SCHOOL_NATURE){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_NATURE){
				school2_other = 1;
			}
		}

		if (o_ptr->ego_num==75 || o_ptr->ego_num==20){ /* Balance DSM, Cloak of Aman */
			if (newspells[spell_code].school1==SCHOOL_TIME){
				school1_other = 1;
			}
			if (newspells[spell_code].school2==SCHOOL_TIME){
				school2_other = 1;
			}
		}

	
}

	result = newspells[spell_code].cost;
	if (school1_talisman){
		if (result >= 5){
			result = ((result-1)*2)/3 + 1;
		} else {
			result = result - 1;
		}
	}
	if (school1_other){
		if (result >= 4){
			result = ((result-1)*2)/3 + 1;
		} else {
			result = result - 1;
		}
	}
	if (school2_talisman){
		if (result >= 4){
			result = ((result-1)*2)/3 + 1;
		} else {
			result = result - 1;
		}
	}
	if (school2_other){
		if (result >= 4){
			result = ((result-1)*2)/3 + 1;
		} else {
			result = result - 1;
		}
	}
	return result;
}

cptr nice_mana_cost(int raw_cost)
{
	if (raw_cost==0){
		return "1/2";
	} else if (raw_cost==-1){
		return "1/4";
	} else if (raw_cost==-2){
		return "1/8";
	} else {
		return "***";
	}
}


int get_success_prob(int spell_code)
{
	int spell_level, caster_level, chance;

	spell_level = newspells[spell_code].level;
	caster_level = p_ptr->lev + adj_int_success[p_ptr->stat_ind[A_INT]];
	if (spell_level >= 10){
		spell_level = spell_level - 1;
	} else if (spell_level >= 20){
		spell_level = spell_level - 2;
	} else if (spell_level >= 40){
		spell_level = spell_level - 3;
	}
	if (caster_level >= spell_level+5) chance = 100;
	else if (caster_level >= spell_level + 2) chance = 99;
	else if (caster_level >= spell_level)     chance = 98;
	else if (caster_level == spell_level - 1) chance = 95;
	else if (caster_level == spell_level - 2) chance = 90;
	else if (caster_level == spell_level - 3) chance = 80;
	else if (caster_level == spell_level - 4) chance = 50;
	else if (caster_level == spell_level - 5) chance = 20;
	else if (caster_level == spell_level - 6) chance = 10;
	else if (caster_level == spell_level - 7) chance = 5;
	else if (caster_level == spell_level - 8) chance = 1;
	else chance = 0;

	if (chance > 100-adj_int_fail_rate[p_ptr->stat_ind[A_INT]]){
		chance = 100-adj_int_fail_rate[p_ptr->stat_ind[A_INT]];
	}

	if (p_ptr->stun > 50) chance -= 25;
	else if (p_ptr->stun) chance -= 15;

	if (chance<0) chance = 0;
	return chance;
}

/*
 * Cast a spell
 */
void do_cmd_cast(bool pray)
{
	int item, spell;
	int chance;
	int mana_cost;

	object_type *o_ptr;

	cptr q, s;

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
	item_tester_tval = TV_MAGIC_BOOK;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
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

	if (spell < 0) return;

	mana_cost = get_mana_cost(spell); 
	chance = get_success_prob(spell);

	/* Disallow "dangerous" spells */
	if (mana_cost > p_ptr->csp || p_ptr->csp<=0)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Flush input */
		flush();

		return;
	}

	/* Failed spell */
	if (randint(100) > chance)
	{
		if (flush_failure) flush();
		msg_print("You failed to get the spell off!");
	}

	/* Process spell */
	else
	{
		/* Cast the spell */
		sound(MSG_SPELL);
		if (do_mage_spell(MODE_SPELL_CAST, spell) == NULL) return;

	}

	/* Use some mana */
	if (mana_cost >= 1){
		p_ptr->csp -= mana_cost;
	} else if (mana_cost == 0){
		p_ptr->csp -= one_in_(2);
	} else if (mana_cost == -1){
		p_ptr->csp -= one_in_(4);
	} else if (mana_cost == -2){
		p_ptr->csp -= one_in_(8);
	} else {
		p_ptr->csp -= one_in_(16);
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);
	
	/* Take a turn */
	p_ptr->p_energy_use = BASE_ENERGY_MOVE;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}



