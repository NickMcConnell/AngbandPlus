

/* File: cmd5.c */


/*
 * Copyright (c) 1998 Julian Lighton, Ben Harrison,
 * James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Brand the current weapon
 */
static void
brand_weapon(void)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
	/* you can never modify broken / cursed items */
	if ((o_ptr->k_idx) &&
		(!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
		(!broken_p(o_ptr)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;

		char o_name[80];

		ego_item_type *e_ptr;

		if (rand_int(100) < 25)
		{
			act = "is covered in a fiery shield!";
			o_ptr->name2 = EGO_BRAND_FIRE;
		}

		else
		{
			act = "glows deep, icy blue!";
			o_ptr->name2 = EGO_BRAND_COLD;
		}

		e_ptr = &e_info[o_ptr->name2];
		o_ptr->flags1 |= e_ptr->flags1;
		o_ptr->flags2 |= e_ptr->flags2;
		o_ptr->flags3 |= e_ptr->flags3;

		object_desc(o_name, o_ptr, FALSE, 0);

		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure)
			flush();
		msg_print("The Branding failed.");
	}
}

/* Puts all creatures on a level to sleep */
static void
mass_sleep(int power)
{
	int i;
	monster_type *m_ptr;

	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];
		if (m_ptr->r_idx)
			(void)project(-1, 0, m_ptr->fy, m_ptr->fx, power, GF_MASS_SLEEP,
						  PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE);
	}
}

/*
 * Shapechange code. Most of the work is done by calc_bonuses().
 * We just handle the messages and whatnot.
 */
void
shapechange(s16b shape)
{
	char *shapedesc;

	/* Wonder Twin powers -- Activate! */
	p_ptr->schange = shape;
	p_ptr->update |= PU_BONUS;

	switch (shape)
	{
	case SHAPE_SHEEP:
		shapedesc = "sheep";
		break;
	case SHAPE_GOAT:
		shapedesc = "goat";
		break;
	case SHAPE_WOLVER:
		shapedesc = "wolverine";
		break;
	case SHAPE_BEAR:
		shapedesc = "bear";
		break;
	case SHAPE_LION:
		shapedesc = "lion";
		break;
	case SHAPE_GAZELLE:
		shapedesc = "gazelle";
		break;
	case SHAPE_CHEETAH:
		shapedesc = "cheetah";
		break;
	case SHAPE_DRAGON:
		shapedesc = "dragon";
		break;
	default:
		msg_print("You return to your normal form.");
		return;
	}
	msg_format("You assume the form of a %s.", shapedesc);
	msg_print("Your equipment merges into your body!");
}


/* Calculate the amount of damage a type of attack will do in the
 * current weather */
static s16b
weather_dam(int typ, int base_dam)
{
	int dam;
	s16b humid, wind, temp;

	dam = base_dam;
	humid = get_weather_humid(p_ptr->cur_weath);
	wind = get_weather_wind(p_ptr->cur_weath);
	temp = get_weather_temp(p_ptr->cur_weath);

	switch (typ)
	{
	case GF_WIND:				/* "winds" */
		{
			dam += (dam * ((wind * 8) > 100 ? 100 : wind * 8)) / 100;
			break;
		}
	case GF_FIRE:
		{
			dam += (dam * ((temp * 8) > 100 ? 100 : temp * 8)) / 100;
			break;
		}
	case GF_ELEC:
		{
			dam -= (dam * ((humid * -8) > 100 ? -100 : humid * 8)) / 150;
			dam -= (dam * ((temp * -8) > 100 ? -100 : temp * 8)) / 300;
			break;
		}
	case GF_POIS:
		{
			dam -= (dam * ((temp * -8) > 100 ? -100 : temp * 8)) / 100;
			break;
		}
	}
	return dam;
}


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int
get_spell(int *sn, cptr prompt, int sval, bool known)
{
	int i;

	int spell = -1;
	int num = 0;

	byte spells[64];

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p = spell_type();


	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
			(spell_flags[p_ptr->realm - 1][sval][0] & (1L << spell)) :
			(spell_flags[p_ptr->realm - 1][sval][1] & (1L << (spell - 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;

	/* Check for "okay" spells */
	for (i = 0; i < num; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(spells[i], known))
			okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay)
		return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;
	/* No redraw yet */
	redraw = FALSE;

#if 0
	/* Show the list */
	if (redraw)
	{
		/* Save screen */
		screen_save();

		/* Display a list of spells */
		print_spells(spells, num, 1, 20);
	}

#endif


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
			p, I2A(0), I2A(num - 1), prompt, p);

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
				print_spells(spells, num, 1, 20);
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
		if (ver)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->info[spell];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
					prompt, spell_names[p_ptr->realm - 1][spell],
					s_ptr->smana, spell_chance(spell));

			/* Belay that order */
			if (!get_check(tmp_val))
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

		/* Hack -- forget redraw */
		/* redraw = FALSE; */
	}


	/* Abort if needed */
	if (!flag)
		return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	/* Success */
	return (TRUE);
}

/*
 * An "item_tester_hook" for browsing any spell book.
 */
static bool
item_tester_hook_sb(object_type * o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_MAGIC_BOOK:
	case TV_PRAYER_BOOK:
	case TV_NATURE_BOOK:
	case TV_DARK_BOOK:
		return TRUE;
	default:
		return FALSE;
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
void
do_cmd_browse(void)
{
	int item, sval, realm;

	int spell = -1;
	int num = 0;

	byte spells[64];

	object_type *o_ptr;

	cptr q, s;


#if 1
	/* Warriors are illiterate */
	if (!mp_ptr->spell_book)
	{
		msg_print("You cannot read books!");
		return;
	}
	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

#else
	/* Allow people to see what they're getting before picking a realm */
	if (!mp_ptr->spell_book)
	{
		item_tester_hook = item_tester_hook_sb;
	}
	else
	{
		/* Restrict choices to "useful" books */
		item_tester_tval = mp_ptr->spell_book;
	}
#endif

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

	/* Get an item */
	q = "Browse which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
		return;

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

	/* Save which realm the book belongs to */
	realm = o_ptr->tval + 1 - TV_MAGIC_BOOK;

	/* Access the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < 64; spell++)
	{
		/* Check for this spell */
		if ((spell < 32) ?
			(spell_flags[realm - 1][sval][0] & (1L << spell)) :
			(spell_flags[realm - 1][sval][1] & (1L << (spell - 32))))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20);

	/* Prompt for a command */
	put_str("(Browsing) Command: ", 0, 0);

	/* Hack -- Get a new command */
	p_ptr->command_new = inkey();

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
void
do_cmd_study(void)
{
	int i, item, sval;

	int spell = -1;

	cptr p = spell_type();

	cptr q, s;

	object_type *o_ptr;

	if (!mp_ptr->spell_book)
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
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Study which book? ";
	s = "You have no books that you can read.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
		return;

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

	/* Access the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Priest -- Learn a random prayer */
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 64; spell++)
		{
			/* Check spells in the book */
			if ((spell < 32) ?
				(spell_flags[p_ptr->realm - 1][sval][0] & (1L << spell)) :
			(spell_flags[p_ptr->realm - 1][sval][1] & (1L << (spell - 32))))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE))
					continue;

				/* Apply the randomizer */
				if ((++k > 1) && (rand_int(k) != 0))
					continue;

				/* Track it */
				gift = spell;
			}
		}
		/* Accept gift */
		spell = gift;
	}

	/* Other realms -- Learn a selected spell */
	else
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1))
			return;
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
	p_ptr->energy_use = 100;

	/* Learn the spell */
	if (spell < 32)
	{
		p_ptr->spell_learned1 |= (1L << spell);
	}
	else
	{
		p_ptr->spell_learned2 |= (1L << (spell - 32));
	}

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 64; i++)
	{
		/* Stop at the first empty space */
		if (p_ptr->spell_order[i] == 99)
			break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
			   p, spell_names[p_ptr->realm - 1][spell]);

	/* Sound */
	sound(SOUND_STUDY);

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

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}
/*
 * Cast a spell
 */
void
do_cmd_cast(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, sval, spell, dir;
	int chance, beam;

	int spower;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q, s;


	/* Must have a realm */
	if (!p_ptr->realm)
	{
		msg_print("You cannot use magic!");
		return;
	}

	/* Require lite */
	if (p_ptr->blind || no_lite())
	{
		msg_print("You cannot see!");
		return;
	}

	/* Must not be berserk */
	if (p_ptr->shero)
	{
		msg_print("You are too berserk!");
		return;
	}

	/* Must be able to use magic */
	if (p_ptr->nomagic)
	{
		msg_print("Your magical abilities are being blocked!");
		return;
	}

	/* Can't be shapechanged. */
	if (DRUID_SCHANGE)
	{
		msg_print("You cannot cast spells while shapechanging.");
		return;
	}

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}


	/* Restrict choices to spell books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get an item */
	q = "Use which book? ";
	s = "You have no spell books!";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR)))
		return;

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

	/* Access the item's sval */
	sval = o_ptr->sval;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, "cast", sval, TRUE))
	{
		if (spell == -2)
			msg_print("You don't know any spells in that book.");
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to cast this spell.");

		/* Verify */
		if (!get_check("Attempt it anyway? "))
			return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure)
			flush();
		msg_print("You failed to get the spell off!");
	}
	/* Process spell */
	else
	{
		/* Get spell power */
		spower = get_skill(S_MAGIC) + (get_raw_skill(mp_ptr->spell_skill) / 20) - 5;
		/* Hack -- chance of "beam" instead of "bolt" */
		beam = (get_raw_skill(mp_ptr->spell_skill) - 52) / 4;

		/* Spells.  */
		switch (spell + (p_ptr->realm - 1) * 64)
		{
/*** Mage spells ***/
		case 0:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
								  damroll(3 + ((spower - 1) / 5), 4));
				break;
			}

		case 1:
			{
				(void)detect_monsters_normal();
				break;
			}

		case 2:
			{
				teleport_player(10);
				break;
			}

		case 3:
			{
				(void)lite_area(damroll(2, (spower / 2)), (spower / 10) + 1);
				break;
			}

		case 4:
			{
				(void)detect_objects_normal();
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

		case 5:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

		case 6:
			{
				if (!get_aim_dir(&dir))
					return;
				fetch_obj(dir, spower * 15);
				break;
			}

		case 7:
			{
				if (!get_aim_dir(&dir))
					return;
				jam_door(dir);
				break;
			}

		case 8:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_POIS, dir, 10 + (spower / 2), 2);
				break;
			}

		case 9:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)fear_monster(dir, spower);
				break;
			}

		case 10:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
								  damroll(3 + ((spower - 5) / 4), 8));
				break;
			}

		case 11:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_MANA, dir, 10 + spower / 3, 1);
				break;
			}

		case 12:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)sleep_monster(dir, spower);
				break;
			}

		case 13:
			{
				(void)set_poisoned(0);
				break;
			}

		case 14:
			{
				teleport_player(spower * 5);
				break;
			}
		case 15:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam - 10, GF_COLD, dir,
								  damroll(5 + ((spower - 5) / 4), 8));
				break;
			}

		case 16:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)wall_to_mud(dir);
				break;
			}

		case 17:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

		case 18:
			{
				(void)recharge(5);
				break;
			}

		case 19:
			{
				(void)sleep_monsters(spower);
				break;
			}

		case 20:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)poly_monster(dir, spower);
				break;
			}

		case 21:
			{
				(void)ident_spell();
				break;
			}

		case 22:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)fire_ball(GF_ELEC, dir, 30 + spower / 4, 2);
				break;
			}

		case 23:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
								  damroll(8 + ((spower - 5) / 4), 8));
				break;
			}

		case 24:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)slow_monster(dir, spower);
				break;
			}

		case 25:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_COLD, dir,
						  30 + (spower), 2);
				break;
			}

		case 26:
			{
				(void)recharge(40);
				break;
			}

		case 27:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)teleport_monster(dir);
				break;
			}

		case 28:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + spower);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}

		case 29:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_FIRE, dir, 55 + (spower), 2);
				break;
			}

		case 30:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

		case 31:
			{
				int dtypes[5] =
				{GF_FIRE, GF_ACID, GF_COLD, GF_ELEC, GF_POIS};
				int dams[5] =
				{spower, spower, spower, spower, spower};
				int rads[5] =
				{3, 3, 3, 3, 3};

				if (!get_aim_dir(&dir))
					return;
				multi_ball(dtypes, dir, dams, rads, 5);
				break;
			}

		case 32:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				break;
			}

		case 33:
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}

		case 34:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				break;
			}

		case 35:
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}
		case 36:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}

		case 37:
			{
				(void)door_creation();
				break;
			}

		case 38:
			{
				(void)stair_creation();
				break;
			}

		case 39:
			{
				/* fill in later */
				set_invis((!p_ptr->tim_invis ? spower :
						   p_ptr->tim_invis + 10), 15);
				break;
			}

		case 40:
			{
				teleport_player_level();
				break;
			}

		case 41:
			{
				recall_player();
				break;
			}

		case 42:
			{
				/* fill in later */
				set_regen_mana(!p_ptr->regen_mana ? damroll(2, spower / 2) :
							   p_ptr->regen_mana + randint(spower / 3));
				break;
			}

		case 43:
			{
				detect_evil();
				break;
			}

		case 44:
			{
				(void)detect_objects_magic();
				break;
			}

		case 45:
			{
				recharge(100);
				break;
			}

		case 46:
			{
				(void)genocide();
				break;
			}

		case 47:
			{
				(void)mass_genocide();
				break;
			}
		case 48:
			{
				(void)extra_hp_player(10);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

		case 49:
			{
				(void)set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

		case 50:
			{
				(void)extra_hp_player(30);
				(void)set_shero(p_ptr->shero + randint(25) + 25);
				(void)set_afraid(0);
				break;
			}

		case 51:
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(30) + 30 + spower);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(10));
				}
				break;
			}

		case 52:
			{
				(void)set_res_dam(p_ptr->res_dam + randint(8) + 8);
				break;
			}

		case 53:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
								  damroll(6 + (spower - 5) / 4, 8));
				break;
			}

		case 54:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_beam(GF_MANA, dir, damroll(spower - 5, 8));
				break;
			}

		case 55:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_SOUND, dir, 170 + (spower * 3) / 2, 2);
				break;
			}

		case 56:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_ACID, dir, 190 + (spower * 3) / 2, 3);
				break;
			}

		case 57:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_PLASMA, dir, 210 + (spower * 3) / 2, 3);
				break;
			}

		case 58:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_CHAOS, dir, 230 + (spower * 3) / 2, 3);
				break;
			}

		case 59:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_MANA, dir, 300 + (spower * 3) / 2, 3);
				break;
			}

/*** Priest spells ***/
		case 64:
			{
				detect_evil();
				break;
			}

		case 65:
			{
				(void)hp_player(damroll(3 + spower / 15, 3 + spower / 8));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}

		case 66:
			{
				(void)set_blessed(p_ptr->blessed + randint(12) +
								  (spower / 10) * 12);
				break;
			}

		case 67:
			{
				(void)set_afraid(0);
				break;
			}

		case 68:
			{
				(void)lite_area(damroll(2, (spower / 2)), (spower / 10) + 1);
				break;
			}

		case 69:
			{
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

		case 70:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam - 10, GF_HOLY_ORB, dir,
								  damroll(3 + (spower / 10), 5));
				break;
			}

		case 71:
			{
				(void)set_poisoned(0);
				break;
			}

		case 72:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)sleep_monster(dir, spower);
				break;
			}

		case 73:
			{
				teleport_player(spower * 3);
				break;
			}

		case 74:
			{
				(void)hp_player(damroll(4 + spower / 5, 5 + spower / 8));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

		case 75:
			{
				(void)set_blessed(p_ptr->blessed + randint(24) +
								  (spower / 10) * 24);
				break;
			}

		case 76:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

		case 77:
			{
				(void)remove_curse();
				break;
			}

		case 78:
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
				break;
			}

		case 79:
			{
				set_detect_inv(p_ptr->detect_inv + 24 + randint(24));
				break;
			}

		case 80:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_HOLY_ORB, dir,
						  damroll(4, 4) + (spower * 3) / 2, 2);
				break;
			}

		case 81:
			{
				(void)hp_player(damroll(5 + spower / 5, 5 + spower / 6));
				(void)set_cut(0);
				break;
			}

		case 82:
			{
				/* fill in later */
				if (p_ptr->tim_invis > 0)
					set_invis(p_ptr->tim_invis + 5 + randint(5), 10);
				else
					set_invis(15 + spower + randint(24), 10);
				break;
			}

		case 83:
			{
				(void)set_protevil(p_ptr->protevil + damroll(2, 12) +
								   2 * spower);
				break;
			}

		case 84:
			{
				earthquake(py, px, 10);
				break;
			}

		case 85:
			{
				(void)map_area();
				break;
			}

		case 86:
			{
				(void)hp_player(damroll(10 + spower / 9, 4 + spower / 8));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
		case 87:
			{
				(void)turn_undead(spower);
				break;
			}

		case 88:
			{
				(void)destroy_doors_touch();
				break;
			}

		case 89:
			{
				banishment(RF3_EVIL, spower);
				break;
			}

		case 90:
			{
				(void)dispel_undead(spower * 3);
				break;
			}

		case 91:
			{
				(void)hp_player(200 + spower * 2);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

		case 92:
			{
				(void)dispel_evil(spower * 3);
				break;
			}

		case 93:
			{
				warding_glyph();
				break;
			}

		case 94:
			{
				(void)dispel_evil(spower * 4);
				(void)hp_player(2000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

		case 95:
			{
				teleport_player(10);
				break;
			}

		case 96:
			{
				teleport_player(spower * 8);
				break;
			}

		case 97:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)teleport_monster(dir);
				break;
			}

		case 98:
			{
				recall_player();
				break;
			}

		case 99:
			{
				(void)teleport_player_level();
				break;
			}

		case 100:
			{
				if (p_ptr->depth == -1)
				{
					msg_print("The world refuses to change.");
					break;
				}
				msg_print("The world changes.");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}

		case 101:
			{
				(void)detect_all();
				break;
			}

		case 102:
			{
				(void)ident_spell();
				break;
			}

		case 103:
			{
				(void)probing();
				break;
			}

		case 104:
			{
				wiz_lite();
				break;
			}
		case 105:
			{
				self_knowledge();
				break;
			}

		case 106:
			{
				/* fill in later */
				(void)hp_player(damroll(12 + spower / 8, 4 + spower / 6));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

		case 107:
			{
				/* fill in later */
				set_regen_mana(!p_ptr->regen_mana ? 15 + randint(spower) :
							   p_ptr->regen_mana + randint(10));
				break;
			}

		case 108:
			{
				(void)set_shield(p_ptr->shield + 20 + spower / 3 +
								 randint(spower));
				msg_print("The essence of your god surrounds you!");
				break;
			}

		case 109:
			{
				(void)restore_stats();
				break;
			}

		case 110:
			{
				(void)restore_level();
				break;
			}

		case 111:
			{
				(void)hp_player(1000);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

		case 112:
			{
				(void)recharge(15);
				break;
			}

		case 113:
			{
				(void)remove_all_curse();
				break;
			}

		case 114:
			{
				(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
				break;
			}

		case 115:
			{
				(void)enchant_spell(0, 0, rand_int(3) + 2);
				break;
			}

		case 116:
			{
				brand_weapon();
				break;
			}

		case 117:
			{
				(void)dispel_evil(spower * 4);
				break;
			}

		case 118:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_beam(GF_HOLY_ORB, dir, (spower * 3) / 2 + 150);
				break;
			}

		case 119:
			{
				banishment((RF3_EVIL | RF3_UNDEAD | RF3_DEMON), 2 * spower);
				break;
			}

		case 120:
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}
		case 121:
			{
				if (!get_aim_dir(&dir))
					return;
				drain_life(dir, 300);
				break;
			}

/*** Druid spells ***/
		case 128:
			{
				detect_life();
				break;
			}

		case 129:
			{
				predict_weather(10 + spower * 2);
				break;
			}

		case 130:
			{
				(void)set_blessed(p_ptr->blessed + spower + 10);
				break;
			}

		case 131:
			{
				(void)set_afraid(0);
				break;
			}

		case 132:
			{
				(void)lite_area(damroll(2, (spower / 2)), (spower / 10) + 1);
				break;
			}

		case 133:
			{
				(void)detect_doors();
				(void)detect_traps();
				(void)detect_stairs();
				break;
			}

		case 134:
			{
				(void)hp_player(damroll(3 + spower / 15, 4));
				(void)set_cut(p_ptr->cut - 5);
				break;
			}

		case 135:
			{
				(void)set_poisoned(0);
				break;
			}

		case 136:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)sleep_monster(dir, spower);
				break;
			}

		case 137:
			{
				/* fill in later */
				set_regen_hp(!p_ptr->regen_hp ? spower + randint(spower) :
							 p_ptr->regen_hp + randint(spower / 2));
				break;
			}

		case 138:
			{
				banishment(RF3_ANIMAL, 20 + spower);
				break;
			}

		case 139:
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

		case 140:
			{
				if (!get_aim_dir(&dir))
					return;
				lite_line(dir);
				break;
			}
		case 141:
			{
				if (!get_aim_dir(&dir))
					return;
				passwall(dir, FALSE, FALSE);
				break;
			}

		case 142:
			{
				(void)map_area();
				break;
			}

		case 143:
			{
				/* fill in later */
				hp_player(damroll(5 + spower / 5, 6));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

		case 144:
			{
				shapechange(SHAPE_SHEEP);
				break;
			}

		case 145:
			{
				set_regen_hp(!p_ptr->regen_hp ?
							 spower + damroll(2, spower) :
							 p_ptr->regen_hp + randint(spower));
				set_regen_mana(!p_ptr->regen_mana ?
							   spower + damroll(2, spower) :
							   p_ptr->regen_mana + randint(spower));
				break;
			}

		case 146:
			{
				if (change_weather(0, 0, 0))
					msg_print("The weather seems more moderate.");
				break;
			}

		case 147:
			{
				shapechange(SHAPE_GOAT);
				break;
			}

		case 148:
			{
				(void)set_oppose_acid(p_ptr->oppose_acid +
									  randint(spower) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec +
									  randint(spower) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire +
									  randint(spower) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold +
									  randint(spower) + 20);
				break;
			}

		case 149:
			{
				shapechange(SHAPE_WOLVER);
				break;
			}

		case 150:
			{
				s16b dam;

				dam = weather_dam(GF_POIS, 20 + spower / 4);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_bolt_or_beam(beam - 10, GF_POIS, dir, dam);
				}
				else
				{
					msg_print("The vapors blow away in the wind!");
				}
				break;
			}

		case 151:
			{
				s16b dam;

				dam = weather_dam(GF_ELEC, 25 + spower / 4);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_bolt_or_beam(beam + 5, GF_ELEC, dir, dam);
				}
				else
				{
					msg_print("The bolt dissipates harmlessly!");
				}
				break;
			}

		case 152:
			{
				s16b dam;

				dam = weather_dam(GF_COLD, 30 + spower / 4);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_bolt_or_beam(beam - 20, GF_COLD, dir, dam);
				}
				else
				{
					msg_print("The ice melts in the heat!");
				}
				break;
			}

		case 153:
			{
				s16b dam;

				dam = weather_dam(GF_FIRE, 35 + spower / 4);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_bolt_or_beam(beam, GF_FIRE, dir, dam);
				}
				else
				{
					msg_print("The fire is extinguished!");
				}
				break;
			}

		case 154:
			{
				s16b dam;

				dam = weather_dam(GF_WIND, 40 + spower / 4);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_bolt_or_beam(beam, GF_WIND, dir, dam);
				}
				else
				{
					msg_print("The wind dies out as it leaves your hands!");
				}
				break;
			}

		case 155:
			{
				s16b dam;

				dam = weather_dam(GF_COLD, 45 + spower / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_COLD, dir, dam, 2);
				}
				else
				{
					msg_print("The snow melts!");
				}
				break;
			}

		case 156:
			{
				s16b dam;

				dam = weather_dam(GF_FIRE, 50 + spower / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_FIRE, dir, dam, 2);
				}
				else
				{
					msg_print("The fire is exinguished!");
				}
				break;
			}

		case 157:
			{
				s16b dam;

				dam = weather_dam(GF_ELEC, 55 + spower / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_ELEC, dir, dam, 2);
				}
				else
				{
					msg_print("The bolts arc harmlessly to the ground!");
				}
				break;
			}

		case 158:
			{
				s16b dam;

				dam = weather_dam(GF_POIS, 60 + spower / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_POIS, dir, dam, 2);
				}
				else
				{
					msg_print("The mists blow away!");
				}
				break;
			}

		case 159:
			{
				shapechange(SHAPE_BEAR);
				break;
			}

		case 160:
			{
				shapechange(SHAPE_LION);
				break;
			}

		case 161:
			{
				shapechange(SHAPE_GAZELLE);
				break;
			}

		case 162:
			{
				shapechange(SHAPE_CHEETAH);
				break;
			}

		case 163:
			{
				shapechange(SHAPE_DRAGON);
				break;
			}

		case 164:
			{
				(void)detect_objects_normal();
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

		case 165:
			{
				(void)ident_spell();
				break;
			}

		case 166:
			{
				(void)probing();
				break;
			}

		case 167:
			{
				(void)stair_creation();
				break;
			}
		case 168:
			{
				(void)wiz_lite();
				break;
			}

		case 169:
			{
				/* fill in later */
				set_tim_stealth(p_ptr->tim_stealth + randint(20) + spower);
				break;
			}
		case 170:
			{
				/* fill in later */
				(void)extra_hp_player(30);
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + spower + 50);
				break;
			}

		case 171:
			{
				set_shield(p_ptr->shield + spower + randint(spower) + 10);
				break;
			}

		case 172:
			{
				(void)mass_sleep(spower);
				break;
			}

		case 173:
			{
				fire_ball_player(GF_WIND, 800, 3);
				break;
			}

		case 174:
			{
				/* Yes, this is not an oversight. Recasting does erase  *
				 * previous weather holds. Because it feels right, before  *
				 * you ask. Messing with the weather is hard to predict. */
				set_hold_weather(damroll(3, spower));
				break;
			}
		case 175:
			{
				if (change_weather(-1, 0, 0))
					msg_print("The air seems drier.");
				break;
			}

		case 176:
			{
				if (change_weather(1, 0, 0))
					msg_print("The air seems damper.");
				break;
			}

		case 177:
			{
				if (change_weather(0, 1, 0))
					msg_print("You feel the wind pick up.");
				break;
			}

		case 178:
			{
				if (change_weather(0, -1, 0))
					msg_print("You feel the wind die down.");
				break;
			}

		case 179:
			{
				if (change_weather(0, 0, 1))
					msg_print("The air grows warmer.");
				break;
			}

		case 180:
			{
				if (change_weather(0, 0, -1))
					msg_print("You feel the air grow cooler.");
				break;
			}

		case 181:
			{
				s16b dam;

				dam = weather_dam(GF_FIRE, 200 + (spower * 3) / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_FIRE, dir, dam, 3);
				}
				else
				{
					msg_print("The flames are extinguished!");
				}
				break;
			}

		case 182:
			{
				s16b dam;

				dam = weather_dam(GF_COLD, 210 + (spower * 3) / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_COLD, dir, dam, 3);
				}
				else
				{
					msg_print("The heat cancels your attack!");
				}
				break;
			}

		case 183:
			{
				s16b dam;

				dam = weather_dam(GF_POIS, 220 + (spower * 3) / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_POIS, dir, dam, 3);
				}
				else
				{
					msg_print("The wind wafts the plague away!");
				}
				break;
			}

		case 184:
			{
				s16b dam;

				dam = weather_dam(GF_WIND, 230 + (spower * 3) / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_WIND, dir, dam, 3);
				}
				else
				{
					msg_print("The winds die away almost instantly!");
				}
				break;
			}

		case 185:
			{
				s16b dam;

				dam = weather_dam(GF_ELEC, 240 + (spower * 3) / 2);
				if (dam)
				{
					if (!get_aim_dir(&dir))
						return;
					fire_ball(GF_ELEC, dir, dam, 3);
				}
				else
				{
					msg_print("The bolt arcs away!");
				}
				break;
			}

/*** Necro spells ***/
		case 192:
			{
				detect_undead();
				break;
			}

		case 193:
			{
				/* fill in later */
				set_detect_inv(!p_ptr->detect_inv ?
							   ((spower / 10) + 1) * 12 + randint(12) :
							   p_ptr->detect_inv + randint(12));
				set_tim_infra(!p_ptr->tim_infra ?
							  ((spower / 10) + 1) * 12 + randint(12) :
							  p_ptr->tim_infra + randint(12));
				break;
			}

		case 194:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)confuse_monster(dir, spower);
				break;
			}

		case 195:
			{
				(void)detect_doors();
				(void)detect_traps();
				(void)detect_stairs();
				break;
			}

		case 196:
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

		case 197:
			{
				(void)teleport_player(10);
				break;
			}

		case 198:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)sleep_monster(dir, spower);
				break;
			}

		case 199:
			{
				(void)remove_curse();
				break;
			}

		case 200:
			{
				take_hit(damroll(2, 5), "the dark arts");
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

		case 201:
			{
				(void)set_oppose_pois(!p_ptr->oppose_pois ?
									  randint(20) + spower :
									  p_ptr->oppose_pois + 10);
				(void)set_oppose_cold(!p_ptr->oppose_cold ?
									  randint(20) + spower :
									  p_ptr->oppose_cold + 10);
				break;
			}

		case 202:
			{
				(void)slow_undead(spower);
				break;
			}

		case 203:
			{
				(void)dispel_undead(spower * 3);
				break;
			}

		case 204:
			{
				/* fill in later */
				set_invis((!p_ptr->tim_invis ? damroll(2, 12) + spower :
						   p_ptr->tim_invis + damroll(2, 12)), 15);
				break;
			}

		case 205:
			{
				detect_life();
				break;
			}

		case 206:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_SPIRIT, dir, 20 + spower / 3, 2);
				break;
			}

		case 207:
			{
				/* fill in later */
				take_hit(damroll(4, 5), "the dark arts");
				set_cheat_death(!p_ptr->cheat_death ? damroll(2, spower) :
								p_ptr->cheat_death + randint(10));
				break;
			}

		case 208:
			{
				teleport_player(spower * 3);
				break;
			}

		case 209:
			{
				/* fill in later */
				if (!get_aim_dir(&dir))
					return;
				fire_bolt_or_beam(beam + 10, GF_DARK, dir,
								  damroll(7 + (spower - 5) / 4, 6));
				break;
			}

		case 210:
			{
				recall_player();
				break;
			}

		case 211:
			{
				(void)ident_spell();
				break;
			}

		case 212:
			{
				if (!get_aim_dir(&dir))
					return;
				drain_life(dir, spower / 3 + 35);
				break;
			}

		case 213:
			{
				(void)slow_monsters(spower);
				break;
			}
		case 214:
			{
				if (!get_aim_dir(&dir))
					return;
				(void)teleport_monster(dir);
				break;
			}

		case 215:
			{
				/* fill in later */
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_POIS, dir, spower / 2 + 30, 3);
				break;
			}

		case 216:
			{
				o_ptr = &inventory[INVEN_WIELD];
				if (!o_ptr->k_idx)
					break;
				if (o_ptr->name1 || o_ptr->name2)
				{
					msg_print("Your weapon resists the spell.");
					break;
				}
				if (o_ptr->flags3 & TR3_LIGHT_CURSE)
				{
					msg_print("The weapon has already been cursed.");
					break;
				}
				enchant(o_ptr, 2, ENCH_TOHIT);
				enchant(o_ptr, 2, ENCH_TODAM);
				o_ptr->flags3 |= TR3_LIGHT_CURSE;
				o_ptr->ident |= (IDENT_CURSED);

				msg_print("Your weapon feels deathly cold!");
				break;
			}

		case 217:
			{
				/* fill in later, yes this does override other effects */
				int dur;

				dur = damroll(2, 24) + spower * 2;
				set_invis(dur, 30);
				set_oppose_cold(dur);
				set_oppose_pois(dur);
				set_detect_inv(dur);
				set_tim_stealth(dur);
				break;
			}

		case 218:
			{
				set_shield(p_ptr->shield + randint(20) + 30);
				break;
			}

		case 219:
			{
				fear_monsters(spower);
				break;
			}

		case 220:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_SPIRIT, dir, 80 + (spower * 3) / 2, 2);
				break;
			}
		case 221:
			{
				o_ptr = &inventory[INVEN_WIELD];
				if (!o_ptr->k_idx)
					break;
				if (o_ptr->name1 || o_ptr->name2)
				{
					msg_print("Your weapon resists the spell.");
					break;
				}

				msg_print("A deep purple light surrounds your weapon.");
				o_ptr->name2 = EGO_SLAY_UNDEAD;
				enchant(o_ptr, 1 + spower / 8, ENCH_TOHIT);
				enchant(o_ptr, 1 + spower / 8, ENCH_TODAM);
				o_ptr->flags1 |= TR1_SLAY_UNDEAD;
				o_ptr->flags3 |= (TR3_HOLD_LIFE | TR3_LITE | TR3_SEE_INVIS);
				break;
			}

		case 222:
			{
				extra_hp_player(30);
				set_afraid(0);
				set_shero(p_ptr->shero > 0 ? p_ptr->shero + 10 : spower + 30);
				break;
			}

		case 223:
			{
				take_hit(damroll(6, 5), "the dark arts");
				destroy_area(py, px, 15, TRUE);
				break;
			}

		case 224:
			{
				/* fill in later */
				set_tim_stealth(!p_ptr->tim_stealth ? spower + damroll(3, 12) :
								p_ptr->tim_stealth + damroll(2, 12));
				break;
			}

		case 225:
			{
				turn_undead(spower);
				break;
			}

		case 226:
			{
				banishment(RF3_UNDEAD | RF3_DEMON, spower * 2);
				break;
			}

		case 227:
			{
				(void)dispel_undead(spower * 5);
				break;
			}

		case 228:
			{
				if (!get_aim_dir(&dir))
					return;
				fire_ball(GF_SPIRIT, dir, (spower * 3) / 2 + 180, 3);
				break;
			}

		case 229:
			{
				set_protevil(!p_ptr->protevil ? spower / 2 + randint(10) :
							 p_ptr->protevil + 8);
				break;
			}

		case 230:
			{
				(void)do_res_stat(rand_int(7));
				break;
			}

		case 231:
			{
				(void)restore_level();
				break;
			}

		case 232:
			{
				(void)restore_stats();
				break;
			}

		case 233:
			{
				(void)lite_area(damroll(2, (spower / 2)), (spower / 10) + 1);
				break;
			}

		case 234:
			{
				(void)detect_all();
				break;
			}

		case 235:
			{
				(void)probing();
				break;
			}

		case 236:
			{
				(void)map_area();
				break;
			}

		case 237:
			{
				take_hit(damroll(3, 5), "the dark arts");
				(void)wiz_lite();
				break;
			}

		case 238:
			{
				int sp_diff;

				sp_diff = (p_ptr->msp - p_ptr->csp) + s_ptr->smana;
				/* hack. restore mana including cost of spell. */
				p_ptr->csp = p_ptr->msp + s_ptr->smana;
				p_ptr->csp_frac = 0;
				msg_print("You feel new power rush through your veins.");
				take_hit(sp_diff, "the dark arts");
				break;
			}

		case 239:
			{
				(void)dispel_animal(spower * 4);
				break;
			}

		case 240:
			{
				if (!get_aim_dir(&dir))
					return;
				drain_life(dir, (spower * 3) / 2 + 120);
				break;
			}

		case 241:
			{
				(void)genocide();
				break;
			}

		case 242:
			{
				(void)mass_genocide();
				break;
			}

		case 243:
			{
				set_blind(0);
				set_poisoned(0);
				set_confused(0);
				set_stun(0);
				set_cut(0);
				break;
			}

		case 244:
			{
				/* fill in later */
				set_regen_hp(!p_ptr->regen_hp ? spower * 2 :
							 p_ptr->regen_hp + randint(spower));
				break;
			}
		case 245:
			{
				extra_hp_player(30);
				set_afraid(0);
				set_shero(p_ptr->shero > 0 ? p_ptr->shero + spower + 10 :
						  spower + 50);
				set_fast(p_ptr->fast > 0 ? p_ptr->fast + spower + 10 : spower + 50);
				break;
			}

		case 246:
			{
				int dur;

				dur = spower + damroll(5, 10);
				(void)set_invis(dur, 35);
				(void)set_oppose_cold(dur);
				(void)set_oppose_pois(dur);
				(void)set_cheat_death(dur);
				(void)set_detect_inv(dur);
				(void)set_fast(dur);
				break;
			}

		case 247:
			{
				fire_ball_player(GF_MANA, 3000, 4);
				take_hit(300 + randint(100), "the dark arts");
				break;
			}

		default:
			{
				msg_print("You have successfully cast a bugged spell!");
				msg_print("Better tell someone, right now!");
			}
		}

		/* A spell was cast */
		if (!((spell < 32) ?
			  (p_ptr->spell_worked1 & (1L << spell)) :
			  (p_ptr->spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (spell < 32)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << (spell - 32));
			}

			/* Gain experience */
			gain_exp(e);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

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

/* Stop doing a druid shapechange */
void
do_cmd_unchange()
{
	if (!DRUID_SCHANGE)
	{
		msg_print("You aren't in another form right now.");
		return;
	}
	/* Confirm */
	if (!get_check("Really return to normal? "))
		return;

	shapechange(SHAPE_NORMAL);
	p_ptr->energy_use = 50;
}
