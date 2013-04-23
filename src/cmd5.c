/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2, bool examine)
{
	int         i;
	int         spell;
	int         num = 0;
	int         ask;
	byte        spells[64];
	bool        flag, redraw, okay;
	char        choice;
	const magic_type  *s_ptr;
	char        out_val[160];
	int         use_realm = (realm_2 ? p_ptr->realm2 : p_ptr->realm1);
	cptr        p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? 
		(((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA) || 
		(p_ptr->pclass == CLASS_ASSASSIN)) ? "skill" : "prayer") : "spell");
						
	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (spell_okay(*sn, known, use_realm - 1))
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
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
		if (((spell_okay(spells[i], known, use_realm - 1)) && (!examine)) ||
			(examine)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
		p, I2A(0), I2A(num - 1), prompt, p);

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 20, use_realm - 1);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}


		/* Note verify */
		ask = (isupper(choice));

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = spells[i];
		
		s_ptr = &mp_ptr->info[use_realm - 1][spell];

		/* Require "okay" spells */
		if (((!spell_okay(spell, known, use_realm - 1)) && (!examine)) ||
			((examine) && (s_ptr->slevel > p_ptr->lev + 1)))
		{
			bell();
			msg_format("You may not %s that %s.", prompt, p);
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &mp_ptr->info[use_realm - 1][spell % 32];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[use_realm - 1][spell % 32],
				s_ptr->smana, spell_chance(spell, use_realm - 1));

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}


	/* Restore the screen */
	if (redraw) screen_load();


	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a given book.  Note that we may
 * bypass do_cmd_browse by calling this function directly (as we
 * do from identify_fully_aux() which has the effect of allowing
 * any book to be browsed regardless of the player's realm choice.
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse_aux(const object_type *o_ptr)
{
	int sval;
	int spell;
	int num = 0;
	int increment = 0;
	byte spells[64];

	if (o_ptr->tval == REALM2_BOOK) increment = 32;
	
	/* Access the item's sval */
	sval = o_ptr->sval;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	screen_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20, (o_ptr->tval - TV_BOOKS_MIN));

	/* Clear the top line */
	prt("", 0, 0);

	(void)get_spell(&spell, "examine", sval, FALSE, (bool)(increment ? TRUE : FALSE), TRUE);
	    
	if (spell >= 0)
	{
		prt("", 10, 13);
		prt("", 11, 13);
		prt("", 12, 13);
		put_str(spell_xtra[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell], 11, 13);

		/* Wait for key */
		(void)inkey();
	}

	/* Restore the screen */
	screen_load();
}


/*
 * Peruse the spells/prayers in a book
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item;
	object_type	*o_ptr;
	cptr q, s;


	/* Warriors are illiterate */
	if (!(p_ptr->realm1 || p_ptr->realm2))
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
	item_tester_tval = mp_ptr->spell_book;

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

	/* Print out the spells */
	do_cmd_browse_aux(o_ptr);
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	increment = 0;

	/* Spells of realm2 will have an increment of +32 */
	int	spell = -1;

	cptr        p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? 
		(((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA) || 
		(p_ptr->pclass == CLASS_ASSASSIN)) ? "skill" : "prayer") : "spell");

	object_type *o_ptr;

	cptr q, s;

	if (!p_ptr->realm1)
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

	msg_format("You can learn %d new %s%s.", p_ptr->new_spells, p,
		(p_ptr->new_spells == 1?"":"s"));
	msg_print(NULL);


	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

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

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Mage -- Learn a selected spell */
	if (mp_ptr->spell_book == TV_ORDER_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", sval, FALSE,
		    (bool)(increment ? TRUE : FALSE), FALSE) && (spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	else
	{
		int k = 0;

		int gift = -1;

		/* Extract spells */
		for (spell = 0; spell < 32; spell++)
		{
			/* Check spells in the book */
			if ((fake_spell_flags[sval] & (1L << spell)))
			{
				/* Skip non "okay" prayers */
				if (!spell_okay(spell, FALSE,
					(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)))
					continue;

				/* Hack -- Prepare the randomizer */
				k++;

				/* Hack -- Apply the randomizer */
				if (one_in_(k)) gift = spell;
			}
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
	p_ptr->energy_use = 100;

	if (increment) spell += increment;

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
		if (p_ptr->spell_order[i] == 99) break;
	}

	/* Add the spell to the known list */
	p_ptr->spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[(increment ? p_ptr->realm2 - 1 : p_ptr->realm1 - 1)][spell % 32]);

	if (mp_ptr->spell_book == TV_LIFE_BOOK)
		chg_virtue(V_FAITH, 1);
	else
		chg_virtue(V_KNOWLEDGE, 1);

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

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}


#define MAX_BIZARRE		6


static const int bizarre_num[MAX_BIZARRE] =
{
	SUMMON_BIZARRE1,
	SUMMON_BIZARRE2,
	SUMMON_BIZARRE3,
	SUMMON_BIZARRE4,
	SUMMON_BIZARRE5,
	SUMMON_BIZARRE6,
};


static void wild_magic(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	switch (randint0(spell) + randint0(9))
	{
		case 1:
		case 2:
		case 3:
		{
			teleport_player(10);
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			teleport_player(100);
			break;
		}
		case 7:
		case 8:
		{
			teleport_player(200);
			break;
		}
		case 9:
		case 10:
		case 11:
		{
			(void)unlite_area(10, 3);
			break;
		}
		case 12:
		case 13:
		case 14:
		{
			(void)lite_area(damroll(2, 3), 2);
			break;
		}
		case 15:
		{
			(void)destroy_doors(1);
			break;
		}
		case 16: case 17:
		{
			wall_breaker();
			break;
		}
		case 18:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case 19:
		case 20:
		{
			(void)trap_creation();
			break;
		}
		case 21:
		case 22:
		{
			(void)door_creation();
			break;
		}
		case 23:
		case 24:
		case 25:
		{
			aggravate_monsters(0);
			break;
		}
		case 26:
		{
			(void)earthquake(py, px, 5, 0);
			break;
		}
		case 27:
		case 28:
		{
			(void)gain_mutation(0);
			break;
		}
		case 29:
		case 30:
		{
			(void)apply_disenchant();
			break;
		}
		case 31:
		{
			(void)lose_all_info();
			break;
		}
		case 32:
		{
			(void)fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
			break;
		}
		case 33:
		{
			(void)wall_stone();
			break;
		}
		case 34:
		case 35:
		{
			int i;
			int type = bizarre_num[randint0(6)];

			for (i = 0; i < 8; i++)
			{
				(void)summon_specific(0, py, px, (p_ptr->depth * 3) / 2, type, TRUE, FALSE, FALSE);
			}
			break;
		}
		case 36:
		case 37:
		{
			(void)activate_hi_summon();
			break;
		}
		case 38:
		{
			(void)summon_cyber(-1, py, px);
			break;
		}
		default:
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);

			break;
		}
	}

	return;
}


static bool cast_life_spell(int spell) // Life
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir;
	int	plev = p_ptr->lev;
	int	beam;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_DRUID)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Detect Creatures */
		(void)detect_monsters_normal();
		break;
	case 1: /* Holy Smite */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_HOLY_FIRE, dir, damroll((2+plev/5), 3), 0);
		break;
	case 2: /* Cure Light Wounds */
		(void)hp_player(damroll(2, 10));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 3: /* Bless */
		(void)set_blessed(p_ptr->blessed + rand_range(10 + plev, 10 + beam * 5));
		break;
	case 4: /* Daylight */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
		{
			msg_print("The daylight scorches your flesh!");
			take_hit(damroll(2, 2), "daylight");
		}
		break;
	case 5: /* Grow Food */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 6: /* Remove Fear */
		(void)set_afraid(0);
		break;
	case 7: /* Cure */
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 20);
		(void)set_poisoned(0);
		break;
	case 8: /* Remove Curse */
		(void)remove_curse();
		break;
	case 9: /* Animal Taming */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)charm_animal(dir, plev);
		break;
	case 10: /* Nature's Awareness -- downgraded */
		map_area();
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();
		break;
	case 11: /* Holy Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_HOLY_FIRE, dir, (damroll(4, 6) + plev), 2 + beam / 25);
		break;
	case 12: /* Ray of Sunlight */
		if (!get_aim_dir(&dir)) return FALSE;
		msg_print("A line of sunlight appears.");
		(void)lite_line(dir);
		break;
	case 13: /* Protection from Evil */
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * plev);
		break;
	case 14: /* Healing */
		(void)hp_player(300);
		(void)set_stun(0);
		(void)set_cut(0);
		break;
	case 15: /* Sanctuary */
		(void)warding_glyph();
		(void)fire_ball(GF_GROWTH, 0, (damroll(5, 10) + plev), 5 + beam / 25);
		break;
	case 16: /* Summon Animals */
		if (!(summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE)));
		break;
	case 17: /* Door Building */
		(void)door_creation();
		break;
	case 18: /* Stair Building */
		(void)stair_creation();
		break;
	case 19: /* Resistance True */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 20: /* Animal Friendship */
		(void)charm_animals(plev + beam);
		break;
	case 21: /* Call Sunlight */
		(void)fire_ball(GF_LITE, 0, 150, 8);
		wiz_lite();
		if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
		{
			msg_print("The sunlight scorches your flesh!");
			take_hit(50, "sunlight");
		}
		break;
	case 22: /* Protection from Corrosion */
		return rustproof();
	case 23: /* Nature's Wrath */
		(void)dispel_monsters(plev * 4);
		(void)earthquake(py, px, 20 + (plev / 2), 0);
		(void)project(0, 1 + plev / 12, py, px,
			100 + plev, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 24: /* Exorcism */
		(void)dispel_undead(plev);
		(void)dispel_demons(plev);
		(void)turn_evil(plev);
		break;
	case 25: /* Heroism */
		(void)set_hero(p_ptr->hero + rand_range(30, 60));
		(void)hp_player(20);
		(void)set_afraid(0);
		break;
	case 26:/* Dispel Undead + Demons */
		(void)dispel_undead(beam * 3);
		(void)dispel_demons(beam * 3);
		break;
	case 27: /* Banishment */
		if (banish_evil(100))
		{
			msg_print("The power of your god banishes evil!");
		}
		break;
	case 28: /* Bless Weapon */
		return bless_weapon();
	case 29: /* Mega Heal */
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)hp_player(plev*11);
		break;
	case 30: /* Divine Intervention */
		(void)project(0, 1, py, px, 777, GF_HOLY_FIRE, PROJECT_KILL);
		(void)dispel_monsters(plev * 4);
		(void)stun_monsters(plev * 4);
		(void)confuse_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)stasis_monsters(plev * 4);
		(void)summon_specific(-1, py, px, plev, SUMMON_ANGEL, TRUE, TRUE, TRUE);
		(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
		(void)hp_player(300);
		/* Haste */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}

		(void)set_afraid(0);
		break;
	case 31: /* Holy Invulnerability */
		(void)set_invuln(p_ptr->invuln + rand_range(7, 14));
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * plev);
		break;
	default:
		msg_format("You cast an unknown Life spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}



static bool cast_order_spell(int spell) // Order
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_PRIEST)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;
	
	switch (spell)
	{
	case 0: /* Phase Door */
		dimension_door(0);
		break;
	case 1: /* Justice */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_MISSILE, dir, p_ptr->mhp - p_ptr->chp);
		break;
	case 2: /* Detect Doors and Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 3: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 4: /* Identify */
		return ident_spell();
		break;
	case 5: /* Teleport Self */
		dimension_door(1);
		break;
	case 6: /* Sleep Monster */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)sleep_monster(dir);
		break;
	case 7: /* Recharging */
		return recharge(plev * 4);
	case 8: /* Magic Mapping */
		map_area();
		break;
	case 9: /* Slow Monster */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)slow_monster(dir);
		break;
	case 10:/* Fire + Ice */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_TEMP, dir, plev * 5);
		break;
	case 11: /* Mass Sleep */
		(void)sleep_monsters();
		break;
	case 12: /* Haste Self */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 13: /* Inertia Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_INERTIA, dir, beam * 5);
		break;
	case 14: /* Detection True */
		(void)detect_all();
		break;
	case 15: /* Identify True */
		return identify_fully();
	case 16: /* Detect Objects and Treasure */
		(void)detect_objects_normal();
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 17: /* Detect Enchantment */
		(void)detect_objects_magic();
		break;
	case 18: /* Resist Elements */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 19: /* Sense Minds */
		(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		break;
	case 20: /* Cold Flame */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_TEMP, dir, plev * 10, 3);
		break;
	case 21: /* Self knowledge */
		(void)self_knowledge();
		break;
	case 22: /* Inertia Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_INERTIA, dir, beam * 10, -3);
		break;
	case 23: /* Word of Recall */
		word_of_recall();
		break;
	case 24: /* Stasis */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)stasis_monster(dir);
		break;
	case 25: /* Telekinesis */
		if (!get_aim_dir(&dir)) return FALSE;
		fetch(dir, plev * 15, FALSE);
		break;
	case 26: /* Explosive Rune */
		(void)explosive_rune();
		break;
	case 27: /* Clairvoyance */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		break;
	case 28: /* Enchant Weapon */
		return enchant_spell(randint1(4), randint1(4), 0);
	case 29: /* Enchant Armour */
		return enchant_spell(0, 0, rand_range(2, 5));
	case 30: /* Alchemy */
		return alchemy();
	case 31: /* Pattern Weapon */
		brand_weapon(11);
		break;
	default:
		msg_format("You cast an unknown Order spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_fire_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir, i, beam, pet;
	int	plev = p_ptr->lev;
	cave_type *c_ptr;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_FIRE)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* R Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(plev, plev*beam+plev));
		break;
	case 1: /* Ignite */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll((2+plev/5), 3));
		break;
	case 2: /* Detect Invisible */
		(void)detect_monsters_invis();
		break;
	case 3: /* Globe of flame */
		(void)fire_ball(GF_FIRE, 0, damroll((2+plev/5), 3), 3);
		break;
	case 4: /* Firelight */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 5: /* Blast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll((2+plev/5), 3)+ plev, 1);
		break;
	case 6: /* Destroy Doors */
		(void)destroy_doors(5);
		break;
	case 7: /* Firebolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll(3+(plev-10)/5, 6)+ plev-10);
		break;
	case 8: /* R Cold */
		(void)set_oppose_cold(p_ptr->oppose_fire + rand_range(20, 50));
		break;
	case 9: /* Narmon */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_FIRE, dir, damroll((2+plev/5), 3)+1);
		break;
	case 10: /* Refuel */
		phlogiston();
		break;
	case 11: /* Engulf */
		(void)fire_ball(GF_FIRE, 0, damroll(3+(plev-10)/5, 6)+ plev-10, beam/5);
		break;
	case 12: /* Recharge */
		return recharge(plev * 4);
		break;
	case 13: /* Explode */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(3+(plev-10)/5, 6)+ plev * 2, 2);
		break;
	case 14: /* Warmth */
		(void)hp_player(damroll(2, plev/2));
		break;
	case 15: /* Wild Fire */
		for (i = 0; i <= beam/8; i++)
		{
			wild_blast(py, px, damroll(3+(plev-10)/5, 6)+ plev, GF_FIRE, 3);
		}
		break;
	case 16: /* Phoenix Shield */
		(void)set_sh_fire(p_ptr->sh_fire + rand_range(plev, plev*beam/11));
		break;
	case 17: /* Fire Ball */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll(plev - 20, 10));
		break;
	case 18: /* Flame Animation */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_FIREE, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry elemental appears!");
		}
		break;
	case 19: /* Heat Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(3+(plev-10)/5, 6)+ plev, -5);
		break;
	case 20: /* Phoenix Tear */
		(void)restore_level();
		(void)hp_player(damroll(3, beam));
		break;
	case 21: /* Scorching Beam */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_PLASMA, dir, damroll(plev - 20, 10));
		break;
	case 22: /* Solar Flare */
		(void)detect_all();
		(void)turn_undead();
		(void)dispel_undead(beam);
		break;
	case 23: /* Dragon's Breath :) */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(plev - 20, 10)+ plev, -4);
		break;
	case 24: /* Enchant Weapon */
		return enchant_spell(randint1(4), randint1(4), 0);
		break;
	case 25: /* Lava Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_LAVA, dir, damroll(plev, 6)+ plev*2);
		break;
	case 26: /* Lava Armour */
		(void)set_ac2(p_ptr->ac2 + rand_range(50, 100));
		break;
	case 27: /* Fire Storm */
		(void)rain_effect(GF_FIRE, (plev * 3) / 2, 1000, 2, 10);
		break;
	case 28: /* Melt */
		(void)destroy_doors(8);
		(void)destroy_traps(8);
		(void)remove_wall_stone();
		break;
	case 29: /* Lava Flow */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_LAVA, dir, damroll(plev, 6)+ plev * 2);
		break;
	case 30: /* Fire Brand */
		brand_weapon(5);
		break;
	case 31: /* Volcano */
		(void)fire_ball(GF_LAVA, 0, damroll(plev * 2, 10), beam/10 +5);
		(void)earthquake(py, px, plev/2, 0);
		break;
	default:
		msg_format("You cast an unknown Fire spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_air_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir, i, beam, pet;
	int	plev = p_ptr->lev;
	cave_type *c_ptr;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_AIR)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;
	
	switch (spell)
	{
	case 0: /* Resist Elec */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(plev, plev*beam/10+plev));
		break;
	case 1: /* Zap */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, damroll((2+plev/5), 3));
		break;
	case 2: /* Call Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 3: /* Lesoch */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_FORCE, dir, damroll(plev, 2));
		break;
	case 4: /* Speed */
		set_slow(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(15 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 5: /* Beam of Light */
		if (!get_aim_dir(&dir)) return FALSE;
		msg_print("A line of light appears.");
		(void)lite_line(dir);
		break;
	case 6: /* Air Shift */
		teleport_player(10);
		break;
	case 7: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 8: /* IZYUK */
		(void)set_ffall(p_ptr->ffall + rand_range(plev, beam*5));
		break;		
	case 9: /* Shock Ball */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll((2+plev/5), 3), 3);
		break;
	case 10: /* Free Action */
		(void)set_free_act(p_ptr->free_act + rand_range(plev, beam*5));
		break;	
	case 11: /* Whirlwind */
		(void)fire_ball(GF_FORCE, 0, damroll(3+(plev-10)/5, 6)+ plev, beam/5);
		break;
	case 12: /* Lightning Shift */
		teleport_player(plev * 5);
		break;
	case 13: /* Fork Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll(3+(plev-10)/5, 6)+ plev, -1);
		break;
	case 14: /* Air Detect */
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();
		break;
	case 15: /* Minor Storm */
		for (dir = 1; dir <= 9; dir++)
		{
			if (dir != 5);
			{
			(void)fire_bolt(GF_ELEC, dir, damroll(3+(plev-10)/5, 6)+ plev);
			}
		}
		break;
	case 16: /* Speed + */
		set_slow(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(50 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(plev));
		}
		break;
	case 17: /* Anti Gravity */
		(void)fire_ball(GF_GRAVITY, 0, damroll(3+(plev-10)/5, 6)+ plev, plev/2);
		break;
	case 18: /* Essence of Air */
		set_slow(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(10));
		}
		(void)set_ffall(p_ptr->ffall + rand_range(plev, beam*5));
		(void)set_free_act(p_ptr->free_act + rand_range(plev, beam*5));
		break;
	case 19: /* Sonic Boom */
		msg_print("BOOM!");
		(void)project(0, plev / 10 + 2, py, px, damroll(plev, 10)+ plev*2, 
			GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 20: /* Restore Mind */
		do_res_stat(A_INT);
		do_res_stat(A_WIS);
		break;
	case 21: /* Tornado */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(3+(plev-10)/5, 6)+ plev, 5);
		break;
	case 22: /* Aether Shift */
		word_of_recall();
		break;
	case 23: /* Cyclone */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(plev - 20, 10)+ plev, -3);
		break;
	case 24: /* Storm Shield */
		(void)set_sh_elec(p_ptr->sh_elec + rand_range(plev, plev*beam/11));
		break;
	case 25: /* Ball Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, damroll(plev - 20, 10));
		break;
	case 26: /* Recharge */
		return recharge(plev * 3);
		break;
	case 27: /* Storm */
		for (dir = 1; dir <= 9; dir++)
		{
			(void)fire_ball(GF_ELEC, dir, damroll(3+(plev-10)/5, 6)+ plev, 3);
		}
		break;
	case 28: /* Storm Lore */
		return identify_fully();
		break;
	case 29: /* Chain Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll(plev - 20, 10), -3);
		break;
	case 30: /* Storm Brand */
		brand_weapon(7);
		break;
	case 31: /* Mega Storm */
		(void)fire_ball(GF_FORCE, 0, damroll(plev - 20, 10)+ plev, 6);
		for (dir = 1; dir <= 9; dir++)
		{
			(void)fire_ball(GF_ELEC, dir, damroll(3+(plev-10)/5, 6)+ plev + beam, 3);
		}
		break;
	default:
		msg_format("You cast an unknown Air spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_death_spell(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int	i;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_NECROMANCER)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Detect Undead & Demons -> Unlife */
		(void)detect_monsters_nonliving();
		break;
	case 1: /* Malediction */
		if (!get_aim_dir(&dir)) return FALSE;
		/* A radius-0 ball may (1) be aimed at objects etc.,
		 * and will affect them; (2) may be aimed at ANY
		 * visible monster, unlike a 'bolt' which must travel
		 * to the monster. */

		(void)fire_ball(GF_HELL_FIRE, dir,
			damroll(3 + ((plev - 1) / 5), 3), 0);

		if (one_in_(5))
		{
			/* Special effect first */
			dummy = randint1(1000);
			if (dummy == 666)
				(void)fire_bolt(GF_DEATH_RAY, dir, plev * 50);
			else if (dummy < 500)
				(void)fire_bolt(GF_TURN_ALL, dir, plev);
			else if (dummy < 800)
				(void)fire_bolt(GF_OLD_CONF, dir, plev);
			else
				(void)fire_bolt(GF_STUN, dir, plev);
		}
		break;
	case 2: /* Detect Evil */
		(void)detect_monsters_evil();
		break;
	case 3: /* Stinking Cloud */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
		break;
	case 4: /* Black Sleep */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)sleep_monster(dir);
		break;
	case 5: /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 6: /* Horrify */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fear_monster(dir, plev);
		break;
	case 7: /* Enslave the Undead */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)control_one_undead(dir, plev);
		break;
	case 8: /* Orb of Entropy */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_OLD_DRAIN, dir, (damroll(4, 6) + plev), 2 + beam / 25);
		break;
	case 9: /* Nether Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam, GF_NETHER, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 10: /* Terror */
		(void)turn_monsters(30 + plev);
		break;
	case 11: /* Vampiric Drain */
		if (!get_aim_dir(&dir)) return FALSE;
		dummy = plev + randint1(plev) * MAX(1, plev / 10);   /* Dmg */
		if (drain_gain_life(dir, dummy))
		{
			/*
			 * Hack - this only happens when monster is seen to
			 * be hit.
			 */
			chg_virtue(V_SACRIFICE, -1);
			chg_virtue(V_VITALITY, -1);

			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged, it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
		}
		break;
	case 12: /* Poison Branding */
		brand_weapon(2);
		break;
	case 13: /* Dispel Good */
		(void)dispel_good(plev * 4);
		break;
	case 14: /* Genocide */
		(void)genocide(TRUE);
		break;
	case 15: /* Steal Soul */
		if (!get_aim_dir(&dir)) return FALSE;
		if (steal_soul(dir,  plev * beam))
		{
			(void)restore_level();
		}
		break;
	case 16: /* Berserk */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 17: /* Invoke Spirits */
		{
			int die = randint1(100) + beam / 5;
			if (!get_aim_dir(&dir)) return FALSE;

			msg_print("You call on the power of the dead...");

			if (die < 26)
				chg_virtue(V_CHANCE, 1);

			if (die > 100)
				msg_print("You feel a surge of eldritch force!");

			if (die < 8)
			{
				msg_print("Oh no! Mouldering forms rise from the earth around you!");
				(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_UNDEAD, TRUE, FALSE, FALSE);

				chg_virtue(V_UNLIFE, 1);
			}
			else if (die < 14)
			{
				msg_print("An unnamable evil brushes against your mind...");
				(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
			}
			else if (die < 26)
			{
				msg_print("Your head is invaded by a horde of gibbering spectral voices...");
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			else if (die < 31)
			{
				(void)poly_monster(dir);
			}
			else if (die < 36)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			else if (die < 41)
			{
				(void)confuse_monster (dir, plev);
			}
			else if (die < 46)
			{
				(void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			}
			else if (die < 51)
			{
				(void)lite_line(dir);
			}
			else if (die < 56)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
					damroll(3 + ((plev - 5) / 4), 8));
			}
			else if (die < 61)
			{
				(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
					damroll(5 + ((plev - 5) / 4), 8));
			}
			else if (die < 66)
			{
				(void)fire_bolt_or_beam(beam, GF_ACID, dir,
					damroll(6 + ((plev - 5) / 4), 8));
			}
			else if (die < 71)
			{
				(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
					damroll(8 + ((plev - 5) / 4), 8));
			}
			else if (die < 76)
			{
				(void)drain_life(dir, 75);
			}
			else if (die < 81)
			{
				(void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			}
			else if (die < 86)
			{
				(void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			}
			else if (die < 91)
			{
				(void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			}
			else if (die < 96)
			{
				(void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			}
			else if (die < 101)
			{
				(void)drain_life(dir, 100 + plev);
			}
			else if (die < 104)
			{
				(void)earthquake(py, px, 12, 0);
			}
			else if (die < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (die < 108)
			{
				(void)genocide(TRUE);
			}
			else if (die < 110)
			{
				(void)dispel_monsters(120);
			}
			else
			{ /* RARE */
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}

			if (die < 31)
				msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
			break;
		}
	case 18: /* Dark Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_DARK, dir, damroll(plev - 20, 10));
		break;
	case 19: /* Battle Frenzy */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(rand_range(plev / 2, 20 + plev));
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		break;
	case 20: /* Vampirism True */
		if (!get_aim_dir(&dir)) return FALSE;
		chg_virtue(V_SACRIFICE, -1);
		chg_virtue(V_VITALITY, -1);
		for (dummy = 0; dummy < 3; dummy++)
		{
			(void)drain_gain_life(dir, 100);
		}
		break;
	case 21: /* Vampiric Branding */
		brand_weapon(3);
		break;
	case 22: /* Darkness Storm */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_DARK, dir, 120, beam/10);
		break;
	case 23: /* Mass Genocide */
		(void)mass_genocide(TRUE);
		break;
	case 24: /* Death Ray */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)death_ray(dir, plev);
		break;
	case 25: /* Raise the Dead */
		{
			if (raise_dead(py, px, (bool)(!one_in_(3))))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				chg_virtue(V_UNLIFE, 1);
			}
			else
			{
				msg_print("Nothing happens.");
			}	
			break;
		}
	case 26: /* Esoteria */
		if (randint1(50) > plev)
			return ident_spell();
		else
			return identify_fully();
		break;
	case 27: /* Word of Death */
		(void)dispel_living(plev * 3);
		break;
	case 28: /* Evocation */
		(void)dispel_monsters(plev * 4);
		(void)turn_monsters(plev * 4);
		(void)banish_monsters(plev * 4);
		break;
	case 29: /* Hellfire */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
		take_hit(rand_range(50, 100), "the strain of casting Hellfire");
		break;
	case 30: /* Omnicide */
		p_ptr->csp -= 100;
		
		/* Display doesn't show mana cost (100)
		 * as deleted until the spell has finished. This gives a
		 * false impression of how high your mana is climbing.
		 * Therefore, 'deduct' the cost temporarily before entering the
		 * loop, then add it back at the end so that the rest of the
		 * program can deduct it properly
		 */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- Skip Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

			/* Hack -- Skip Quest Monsters */
			if (r_ptr->flags1 & RF1_QUESTOR) continue;

			/* Notice changes in view */
			if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
			{
				/* Update some things */
				p_ptr->update |= (PU_MON_LITE);
			}
			
			/* Delete the monster */
			delete_monster_idx(i);

			/* Take damage */
			take_hit(randint1(4), "the strain of casting Omnicide");

			/* Absorb power of dead soul - up to twice max. mana */
			if (p_ptr->csp < (p_ptr->msp * 2))
				p_ptr->csp++;

			/* Visual feedback */
			move_cursor_relative(py, px);

			/* Redraw */
			p_ptr->redraw |= (PR_HP | PR_MANA);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
			p_ptr->window |= (PW_SPELL);

			/* Handle */
			handle_stuff();

			/* Fresh */
			Term_fresh();

			/* Delay */
			Term_xtra(TERM_XTRA_DELAY,
				delay_factor * delay_factor * delay_factor);
		}

		/* Restore, ready to be deducted properly */
		p_ptr->csp += 100;

		break;
	case 31: /* Wraithform */
		(void)set_wraith_form(p_ptr->wraith_form + rand_range(plev / 2, plev));
		break;
	default:
		msg_format("You cast an unknown Death spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_chaos_spell(int spell, bool success)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int     dir;
	int     beam;
	int     plev = p_ptr->lev;
	int     dummy = 0;
	bool	no_trump = FALSE;
	char	ppp[80];
	char	tmp_val[160];
	char        Rumor[1024];
	int     i, x, y;
	cave_type *c_ptr;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_SHAMAN)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	
	switch (spell)
	{
		case 0: /* Phase Door */
			if (success)
			{
				teleport_player(10);
			}
			break;
		case 1: /* Crazy bullet */
			if (success)
			{
			int die = ((randint1(plev)) / 5 + 1);
				for (dummy = 0; dummy <= die; dummy++)
				{
				wild_blast(py, px, damroll(1, plev + 5), GF_MANA, 0);
				}
			}
			break;
		case 2: /* Shuffle */
			if (success)
			{
				int die = randint1(3);
				if (die < 2)
			{	msg_print("You shuffle your position");
				teleport_player(plev*4);
				}
				else			
			{
				/* A limited power 'wonder' spell */
				int die = randint1(120);

				if ((p_ptr->pclass == CLASS_SHAMAN) ||
					(p_ptr->pclass == CLASS_HIGH_MAGE))
					die = (randint1(110)) + plev / 5;
				/* Card sharks and high mages get a level bonus */

				msg_print("You shuffle the deck and draw a card...");

				if (die < 30)
					chg_virtue(V_CHANCE, 1);

				if (die < 7)
				{
					msg_print("Oh no! It's Death!");
					for (dummy = 0; dummy < randint1(3); dummy++)
						(void)activate_hi_summon();
				}
				else if (die < 14)
				{
					msg_print("Oh no! It's the Devil!");
					(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_DEMON, TRUE, FALSE, FALSE);
				}
				else if (die < 18)
				{
					int count = 0;

					msg_print("Oh no! It's the Hanged Man.");
					(void)activate_ty_curse(FALSE, &count);
				}
				else if (die < 22)
				{
					msg_print("It's the swords of discord.");
					aggravate_monsters(0);
				}
				else if (die < 26)
				{
					msg_print("It's the Fool.");
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
				else if (die < 30)
				{
					msg_print("It's the picture of a strange monster.");
					if (!(summon_specific(0, py, px, (p_ptr->depth * 3) / 2, rand_range(33, 38), TRUE, FALSE, FALSE)))
						no_trump = TRUE;
				}
				else if (die < 33)
				{
					msg_print("It's the Moon.");
					(void)unlite_area(10, 3);
				}
				else if (die < 38)
				{
					msg_print("It's the Wheel of Fortune.");
					wild_magic(randint0(32));
				}
				else if (die < 40)
				{
					msg_print("It's a teleport trump card.");
					teleport_player(10);
				}
				else if (die < 42)
				{
					msg_print("It's Justice.");
					(void)set_blessed(p_ptr->blessed + p_ptr->lev);
				}
				else if (die < 47)
				{
					msg_print("It's a teleport trump card.");
					teleport_player(100);
				}
				else if (die < 52)
				{
					msg_print("It's a teleport trump card.");
					teleport_player(200);
				}
				else if (die < 60)
				{
					msg_print("It's the Tower.");
					wall_breaker();
				}
				else if (die < 72)
				{
					msg_print("It's Temperance.");
					(void)sleep_monsters_touch();
				}
				else if (die < 80)
				{
					msg_print("It's the Tower.");
					(void)earthquake(py, px, 5, 0);
				}
				else if (die < 82)
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE1, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 84)
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE2, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 86)
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE4, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 88)
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE5, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
				}
				else if (die < 96)
				{
					msg_print("It's the Lovers.");
					if (get_aim_dir(&dir))
						(void)charm_monster(dir, MIN(p_ptr->lev, 20));
				}
				else if (die < 101)
				{
					msg_print("It's the Hermit.");
					(void)wall_stone();
				}
				else if (die < 111)
				{
					msg_print("It's the Judgement.");
					do_cmd_rerate();
					if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
					{
						msg_print("You are cured of all mutations.");
						p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
						p_ptr->update |= PU_BONUS;
						handle_stuff();
					}
				}
				else if (die < 120)
				{
					msg_print("It's the Sun.");
					wiz_lite();
				}
				else
				{
					msg_print("It's the World.");
					if (p_ptr->exp < PY_MAX_EXP)
					{
						s32b ee = (p_ptr->exp / 25) + 1;
						if (ee > 5000) ee = 5000;
						msg_print("You feel more experienced.");
						gain_exp(ee);
					}
				}
				}
			}
			break;
		case 3: /* Flash of Light */
			if (success)
			{
				int i = randint1(8);
				cptr word = "red";
				
				if (i == 1)
				{
					msg_print("There is a dark flash");
					unlite_area(10, 3);
					break;
				}
				else if (i == 2)
				{
					word = "red";
				}
				else if (i == 3)
				{
					word = "yellow";
				}
				else if (i == 4)
				{
					word = "green";
				}
				else if (i == 5)
				{
					word = "blue";
				}
				else if (i == 6)
				{
					word = "indigo";
				}
				else if (i == 7)
				{
					word = "violet";
				}
				else if (i == 8)
				{
					msg_print("There is a bright flash");
					(void)lite_area(damroll(1, plev), (plev / 10) + 2);
					break;
				}
				msg_format("There is a %s flash", word);
				(void)lite_area(damroll(1, plev), (plev / 10) + 1);
				break;
				
			}
			break;
		case 4: /* Minor Trump */
			if (success)
			{
				int die = randint1(10);
				if (die < 2)
				{ msg_print("You draw the flame") ;
				cast_fire_spell(3); }
				else if (die < 3)
				{ msg_print("You draw the stream") ;
				cast_water_spell(7); }
				else if (die < 4)
				{ msg_print("You draw the shield") ;
				cast_earth_spell(0); }
				else if (die < 5)
				{ msg_print("You draw the wind") ;
				cast_air_spell(4); }
				else if (die < 6)
				{ msg_print("You draw the star") ;
				cast_life_spell(3); }
				else if (die < 7)
				{ msg_print("You draw the ghost") ;
				cast_death_spell(6); }
				else if (die < 8)
				{ msg_print("You draw the ?") ;
				cast_order_spell(4); }
				else if (die < 9)
				{ msg_print("You draw the door") ;
				teleport_player(plev*3); }
				else if (die < 10)
				{ msg_print("You draw the missile") ;
				cast_astral_spell(7); }
				else
				{ msg_print("You draw the torch") ;
				cast_wizard_spell(2); }
			}
			break;
		case 5: /* Confusion */
			if (success)
			{
				int die = randint1(2);
				if (die < 2)
				{	msg_print("Your hands start glowing.");
					p_ptr->confusing = TRUE;
					p_ptr->redraw |= (PR_STATUS);
				}
				else
				{	
					if (!get_aim_dir(&dir)) return FALSE;
					(void)fire_bolt_or_beam(beam - 10, GF_CONFUSION, dir, plev * 3);
				}
			}
			break;
		case 6: /* Trump Spying */
			if (success)
			{
				(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			}
			break;
		case 7: /* Hand of Fate */
			if (success)
			{
				/* Mark all (nearby) monsters */
				for (i = 1; i < m_max; i++)
				{
					monster_type *m_ptr = &m_list[i];

					/* Paranoia -- Skip dead monsters */
					if (!m_ptr->r_idx) continue;

					/* Location */
					y = m_ptr->fy;
					x = m_ptr->fx;

					c_ptr = area(y, x);

					/* Require line of sight */
					if (!player_has_los_grid(c_ptr)) continue;
					
					if(one_in_(2)) continue;

					(void)project(0, 0, y, x, damroll(2, (plev * 3)), GF_MISSILE, PROJECT_KILL);
				}
				if(!one_in_(10)) take_hit(rand_range(1, plev), "the Hand of Fate");
			}
			break;
		case 8: /* Chaos Bolt */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt_or_beam(beam, GF_CHAOS, dir, damroll(10 + ((plev - 5) / 4), 8));
			}
			break;
		case 9: /* Trump */
		{
			int die = randint1(8);
				if (die < 2)
				{ msg_print("You draw the bomb") ;
				cast_fire_spell(13); }
				else if (die < 3)
				{ msg_print("You draw the drop") ;
				cast_water_spell(10); }
				else if (die < 4)
				{ msg_print("You draw the eye") ;
				cast_earth_spell(15); }
				else if (die < 5)
				{ msg_print("You draw the whirl") ;
				cast_air_spell(11); }
				else if (die < 6)
				{ msg_print("You draw the cross") ;
				cast_life_spell(13); }
				else if (die < 7)
				{ msg_print("You draw the nether") ;
				cast_death_spell(9); }
				else if (die < 8)
				{ msg_print("You draw the Z's") ;
				cast_order_spell(11); }
				else if (die < 9)
				{ msg_print("You draw the phantom") ;
				if (summon_specific(-1, py, px, (plev * 3) / 2, SUMMON_PHANTOM, FALSE, TRUE, TRUE))
				{
					msg_print("'Your wish, master?'");
				}
				else
				{
					no_trump = TRUE;
				}
				}
				else if (die < 10)
				{ msg_print("You draw the health") ;
				cast_astral_spell(10); }
				else
				{ msg_print("You draw the wall") ;
				cast_wizard_spell(15); }
			break;
		}
		case 10: /* Doom Bolt */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_beam(GF_MANA, dir, damroll(11 + ((plev - 5) / 4), 8));
			}
			break;
		case 11: /* Conjure Elemental */
		{
			bool pet = success; /* was (randint1(6) > 3) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msg_print("You concentrate on the trump of a monster...");

			if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_ELEMENTAL, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry elemental appears!");
			}
			else
			{
				no_trump = TRUE;
			}

			break;
		}
		case 12: /* Tele Other */
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_beam(GF_AWAY_ALL, dir, plev);
			}
			break;
		case 13: /* Teleport Level */
			if (success)
			{
				(void)teleport_player_level();
			}
			break;
		case 14: /* Invoke Logrus */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_CHAOS, dir, plev + 66, plev / 5);
			}
			break;
		case 15: /* Banish */
			if (success)
			{
				(void)banish_monsters(plev * 4);
			}
			break;
		case 16: /* Joker Card */
		{
			bool pet = success; /* was one_in_(2) */
			bool group = (pet ? FALSE : TRUE);

			if (success)
				msg_print("You concentrate on a joker card...");

			switch (randint1(4))
			{
				case 1: dummy = SUMMON_BIZARRE1; break;
				case 2: dummy = SUMMON_BIZARRE2; break;
				case 3: dummy = SUMMON_BIZARRE4; break;
				case 4: dummy = SUMMON_BIZARRE5; break;
			}

			if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, dummy, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
			else
			{
				no_trump = TRUE;
			}
			break;
		}
		case 17: /* Trump Divination */
			if (success)
			{
				(void)detect_all();
			}
			break;
		case 18: /* Trump Brand */
			if (success)
			{
				brand_weapon(4);
			}
			break;
		case 19: /* Trump Lore */
			if (success)
			{
				int die = randint1(2);
				if (die < 2)
				{	return identify_fully();
				}
				else
				{	errr err;
					err = get_rnd_line("rumors.txt", 0, Rumor);
					msg_format("%s", Rumor);
				}
			}
			break;
		case 20: /* Creature Trump */
		{
			bool pet = success; /* (randint1(5) > 2) */
			bool group = (pet ? FALSE : TRUE);
			
			int die = randint1(3);
			if (die < 2)
			{	 msg_print("You draw the spider") ;
				if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_SPIDER, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry spiders appears!");
				}
				else
				{
				no_trump = TRUE;
				}
			}
			else if (die < 3)
			{	 msg_print("You draw the hydra") ;
				if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_HYDRA, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry hydra appears!");
				}
				else
				{
				no_trump = TRUE;
				}
			}
			else
			{	 msg_print("You draw the hound") ;
				if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_HOUND, TRUE, FALSE, pet))
				{
					if (!pet)
					msg_print("Angry barking surrounds you!");
				}
				else
				{
					no_trump = TRUE;
				}
			}

				}
			break;
		case 21: /* Living Trump */
			if (success)
			{
				if (one_in_(8))
					/* Teleport control */
					dummy = 12;
				else
					/* Random teleportation (uncontrolled) */
					dummy = 77;

				/* Gain the mutation */
				if (gain_mutation(dummy))
					msg_print("You have turned into a Living Trump.");
			}
			break;
		case 22: /* Trump Undead */
		{	bool pet = success; /* (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);
			int die = randint1(75) + plev;
			if (die < 100)
			{	msg_print("You concentrate on the trump of an undead creature...");
				if (summon_specific((pet ? -1 : 0), py, px, plev * 2, SUMMON_UNDEAD, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry undead creature appears!");
				}
				else
				{
					no_trump = TRUE;
				}
			}	
			else
			{	bool pet = success; /* was (randint1(10) > 3) */
				bool group = (pet ? FALSE : TRUE);
				int type = (pet ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_HI_UNDEAD);
					msg_print("You concentrate on the trump of a greater undead being...");
				if (summon_specific((pet ? -1 : 0), py, px, plev * 2, type, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry greater undead creature appears!");
				}
				else
				{
					no_trump = TRUE;
			}
		}	
		break;	}	
		
		case 23: /* Trump Dragon */
		{	bool pet = success; /* (randint1(10) > 3) */
			bool group = (pet ? FALSE : TRUE);
			int die = randint1(75) + plev;
			if (die < 100)
			{		msg_print("You concentrate on the trump of a dragon...");
				if (summon_specific((pet ? -1 : 0), py, px, plev * 2, SUMMON_DRAGON, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry dragon appears!");
				}
				else
				{
					no_trump = TRUE;
				}
			}	
			else
			{	bool pet = success; /* was (randint1(10) > 3) */
				bool group = (pet ? FALSE : TRUE);
				int type = (pet ? SUMMON_HI_DRAGON_NO_UNIQUES : SUMMON_HI_DRAGON);
					msg_print("You concentrate on the trump of an ancient dragon...");
				if (summon_specific((pet ? -1 : 0), py, px, plev * 2, type, group, FALSE, pet))
				{
					if (!pet)
						msg_print("An angry ancient dragon appears!");
				}
				else
				{
					no_trump = TRUE;
				}
			}	
		break;	}	
		
		case 24: /* Morph Other */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)poly_monster(dir);
			}
			break;
		case 25: /* Call Chaos */
			if (success)
			{
				call_chaos();
			}
			break;
		case 26: /* Fortean Flicker */
		{
			int die = randint1(4);
				if (die < 3)
				{	
					alter_reality();
				}
				else if (die < 4)
				{	
					(void)fire_ball(GF_CHAOS, 0, 150 + (2 * plev), 5);
				}
				else
				{	
					(void)teleport_player_level();
				}
		}
			break;
		case 27: /* Drink Logrus */
		{	
			do_poly_self();
			break;
		}
		case 28: /* Infinty Bolt */
		{	
			int die = randint1(3);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_MISSILE, dir, damroll(1, 2*plev));
				if (die < 3)
				{
					cast_chaos_spell(28, 100);
				}
			break;
		}
		case 29: /* Breathe Logrus */
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_CHAOS, dir, p_ptr->chp, -2);
			break;
		}
		case 30: /* Chaos Brand */
		{
			brand_weapon(1);
			break;
		}
		case 31: /* Call The Void */
		{
			call_the_();
			break;
		}
		default:
			msg_format("You cast an unknown Chaos spell: %d.", spell);
			msg_print(NULL);
	}

	if (no_trump)
	{
		msg_print("Nobody answers to your Trump call.");
	}

	return TRUE;
}


static bool cast_astral_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int     i, x, y;
	cave_type *c_ptr;


	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_WITCH)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Wizard Lock */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)wizard_lock(dir);
		break;
	case 1: /* Detect Invisibility */
		(void)detect_monsters_invis();
		break;
	case 2: /* Phase Door */
		dimension_door(0);
		break;
	case 3: /* Light Area */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 4: /* Disarm */
		(void)fire_ball(GF_KILL_TRAP, 0, 0, (plev / 10));
		break;
	case 5: /* Detect Life */
		(void)detect_monsters_living();
		break;
	case 6: /* Detect Unlife */
		(void)detect_monsters_nonliving();
		break;
	case 7: /* Magic Missile */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam - 10, GF_MANA, dir,
			damroll(3 + ((plev - 1) / 5), 4));
		break;
	case 8: /* Detect Treasure */
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 9: /* First Aid */
		(void)set_cut(0);
		break;
	case 10: /* Cure Light Wounds */
		(void)hp_player(damroll(3, 10));
		break;
	case 11: /* Blink */
		teleport_player(12);
		break;
	case 12: /* Detect Portals */
		(void)detect_stairs();
		(void)detect_doors();
		break;
	case 13: /* Detect Monsters */
		(void)detect_monsters_normal();
		break;
	case 14: /* Phlogiston */
		phlogiston();
		break;
	case 15: /* Luck (Bless) */
		(void)set_blessed(p_ptr->blessed + rand_range(50, 75));
		break;
	case 16: /* Multi Missile */

		/* Mark all (nearby) monsters */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Location */
			y = m_ptr->fy;
			x = m_ptr->fx;

			c_ptr = area(y, x);

			/* Require line of sight */
			if (!player_has_los_grid(c_ptr)) continue;

			(void)project(0, 0, y, x, damroll(3 + ((plev - 1) / 5), 4), GF_MANA, (PROJECT_STOP | PROJECT_KILL));
		}
		break;
	case 17: /* Resist Cold */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		break;
	case 18: /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		break;
	case 19: /* Teleport */
		teleport_player(plev * 5);
		break;
	case 20: /* Resist Lightning */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		break;
	case 21: /* Resist Acid */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 22: /* Identify */
		return ident_spell();
	case 23: /* Ray of Light */
		if (!get_aim_dir(&dir)) return FALSE;
		msg_print("A line of light appears.");
		(void)lite_line(dir);
		break;
	case 24: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)wall_to_mud(dir);
		break;
	case 25: /* See Invisible */
		(void)set_tim_invis(p_ptr->tim_invis + rand_range(24, 48));
		break;
	case 26: /* Satisfy Hunger */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 27: /* Detect Object */
		(void)detect_objects_normal();
		break;
	case 28: /* Word of Recall */
		word_of_recall();
		break;
	case 29: /* Force Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam, GF_FORCE, dir, damroll(10 + (plev / 10), 8));
		break;
	case 30: /* Aura of Fire */
		(void)set_sh_fire(p_ptr->sh_fire + rand_range(25, beam));
		break;
	case 31: /* SEER */
		wiz_lite();
		if (!p_ptr->telepathy)
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 50));
		}
		break;
	default:
		msg_format("You cast an unknown Astral spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}


static bool cast_water_spell(int spell)
{
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_WATER)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Cold Resist */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(plev, plev*beam/10+plev));
		break;
	case 1: /* Chill */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_COLD, dir, damroll((2+plev/5), 3));
		break;
	case 2: /* Acid Resist */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(plev, plev*beam/10+plev));
		break;
	case 3: /* Acid bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ACID, dir, damroll((2+plev/5), 3));
		break;
	case 4: /* Blink */
		teleport_player(10);
		break;
	case 5: /* Frost */
		(void)fire_ball(GF_COLD, 0, damroll((2+plev/5), 3), 4);
		break;
	case 6: /* Detect Cretures */
		(void)detect_monsters_normal();
		break;
	case 7: /* Hyrdoblast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_WATER, dir, damroll(plev, 2));
		break;
	case 8: /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 50));
		break;
	case 9: /* Ice Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ICE, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 10: /* Dewdrop */
		(void)hp_player(damroll(2, 8));
		break;
	case 11: /* Holy Water */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_HOLY_FIRE, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 12: /* Ice Shield */
		(void)set_ac1(p_ptr->ac1 + rand_range(30, 50));
		break;
	case 13: /* Acid Spray */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ACID, dir, damroll((2+plev/5), 3), -2);
		break;
	case 14: /* Healing Water */
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 20);
		(void)set_poisoned(0);
		break;
	case 15: /* Snow Fall */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_COLD, dir, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
		break;
	case 16: /* Wave Speed */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(plev));
		}
		break;
	case 17: /* Acidic Flow */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_ACID, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 18: /* Hydroflow */
		word_of_recall();
		break;
	case 19: /* Quicksilver */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(plev));
		}
			(void)set_blessed(p_ptr->blessed + randint1(20 + plev) + plev);
		break;
	case 20: /* Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, damroll((2+plev/5), 3), -3);
		break;
	case 21: /* Acid Rain */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ACID, dir, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
		break;
	case 22: /* Acid Brand */
		brand_weapon(8);
		break;
	case 23: /* Tidal Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(plev - 20, 10), 2);
		(void)fire_ball(GF_WATER, dir, damroll(plev - 20, 10), -3);
		break;
	case 24: /* Ice Lore */
		return identify_fully();
	case 25: /* Freeze */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_COLD, dir, damroll(plev - 20, 10));
		break;
	case 26: /* Restore Soul */
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 27: /* Snow Flake */
		(void)fire_beam(GF_ICE, 8, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 2, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 4, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 6, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 7, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 3, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 9, damroll((2+plev/4), 3));
		(void)fire_beam(GF_ICE, 1, damroll((2+plev/4), 3));
		break;
	case 28: /* Frost Brand */
		brand_weapon(6);
		break;
	case 29: /* Freezing Sphere */
		(void)fire_ball(GF_COLD, 0, damroll(plev - 20, 10), 4);
		break;
	case 30: /* Ice Air */
		(void)slow_monsters();
		(void)detect_all();
		break;
	case 31: /* Arctic Blast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ICE, dir, damroll(plev - 20, 10) + plev, 3);
		break;
	default:
		msg_format("You cast an unknown Water spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}

static bool cast_earth_spell(int spell) // Earth
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	int	dir;
	int	i;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	int	x = 0, y = 0;
	cave_type *c_ptr;
	
	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_EARTH)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Shield */
		(void)set_ac1(p_ptr->ac1 + rand_range(1, plev + 5));
		break;
	case 1: /* Slow */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)slow_monster(dir);
		break;
	case 2: /* Detect Gold */
		(void)detect_treasure();
		(void)detect_objects_gold();
		break;
	case 3: /* Rock Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FORCE, dir, damroll((1+plev/5), 3));
		break;
	case 4: /* Sense Invisible */
		(void)detect_monsters_invis();
		break;
	case 5: /* Stun Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_STUN, dir, damroll((2+plev/5), 3));
		break;
	case 6: /* Dig */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)wall_to_mud(dir);
		break;
	case 7: /* Increase Gravity */
		(void)slow_monsters();
		break;
	case 8: /* Resist Lightning */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 50));
		break;
	case 9: /* Gravity Blast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_GRAVITY, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 10: /* Armour */
		(void)set_ac2(p_ptr->ac2 + rand_range(plev, plev*2));
		break;
	case 11: /* Multi-Stun */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_STUN, dir, damroll((2+plev/5), 3), 3);
		break;
	case 12: /* Detect Walls */
		(void)map_area();
		break;
	case 13: /* Gravity Beam */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_GRAVITY, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 14: /* Stunwave */
		(void)fire_ball(GF_STUN, 0, damroll(3+(plev-10)/5, 6)+ plev -10, 1+plev/10);
		break;
	case 15: /* Greater Detection */
		(void)detect_all();
		break;
	case 16: /* Remove Traps */
		(void)destroy_traps(10);
		break;
	case 17: /* Tremor */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_QUAKE, dir, damroll(3+(plev-10)/5, 6)+ plev -10, -2);
		break;
	case 18: /* Tunnel */
		if (!get_aim_dir(&dir)) return FALSE;
		for (dir = 1; dir < beam/10+1; dir++)
		{
			(void)wall_to_mud(dir);
		}
		break;
	case 19: /* Earthquake */
		(void)earthquake(py, px, plev/2 - 10, 0);
		break;
	case 20: /* Passage */
		word_of_recall();
		break;
	case 21: /* Greater Quake */
		if (!tgt_pt(&x, &y)) return FALSE;
	
		p_ptr->energy -= 60 - plev;
	
		/* paranoia */
		if (!in_bounds2(y, x)) return FALSE;
	
		c_ptr = area(y, x);
		if (distance(y, x, py, px) > plev + 2)
		{
			msg_print("You over reach!");
			p_ptr->energy -= 100;
			earthquake(py, px, plev/5, 0);
		}
		else earthquake(y, x, plev/2 - 10, 0);
		break;
	case 22: /* Quake Brand */
		(void)brand_weapon(9);
		break;
	case 23: /* Geoforce */
		(void)set_shield(p_ptr->shield + rand_range(25, 50));
		(void)fire_ball(GF_STUN, 0, damroll(plev - 20, 10), plev/5);
		break;
	case 24: /* Gem Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_SHARDS, dir, damroll(plev - 20, 10));
		break;
	case 25: /* Stone Skin */
		msg_print("Your skin turns to stone! ");
		(void)set_shield(p_ptr->shield + rand_range(25, 50));
		break;
	case 26: /* Crystal Vortex */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_SHARDS, dir, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
		break;
	case 27: /* Improve Armour */
		return enchant_spell(0, 0, rand_range(2, 5));
	case 28: /* Crush */
			(void)fire_ball(GF_GRAVITY, 8, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 2, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 4, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 6, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 7, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 3, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 9, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
			(void)fire_ball(GF_GRAVITY, 1, damroll(3+(plev-10)/5, 6)+ plev -10, 3);
		break;
	case 29: /* Restore Body */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_DEX);
		break;
	case 30: /* Rain of Shards */
		(void)rain_effect(GF_SHARDS, damroll(plev - 20, 10), 1500, 3, 12);
		break;
	case 31: /* Crystal Skin */
		(void)set_invuln(p_ptr->invuln + rand_range(10, 20));
		break;
	default:
		msg_format("You cast an unknown Earth spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}

static bool cast_wizard_spell(int spell) // Wizardry
{
	int px = p_ptr->px;
	int py = p_ptr->py;
	int	dir;
	int	beam;
	int	plev = p_ptr->lev;
	int	dummy = 0;
	bool	no_trump = FALSE;
	bool pet = 100;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_WIZARD) 
		|| (p_ptr->pclass == CLASS_SAGE)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Infravision */
		(void)set_tim_infra(p_ptr->tim_infra + rand_range(10, 50));
		break;
	case 1: /* Mana Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam, GF_MANA, dir, damroll((1+plev/5), 3));
		break;
	case 2: /* Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 3: /* Blink */
		teleport_player(10);
		break;
	case 4: /* Detect Traps+Doors */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 5: /* Remove Fear */
		(void)set_afraid(0);
		break;
	case 6: /* Detect Danger */
		(void)detect_traps();
		(void)detect_monsters_normal();
		(void)detect_monsters_invis();
		do_cmd_feeling();
		break;
	case 7: /* Slow Poison */
		(void)set_poisoned(p_ptr->poisoned / 2);
		break;
	case 8: /* Teleport */
		teleport_player(plev * 4);
		break;
	case 9: /* Haste */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(20 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(plev));
		}
		break;
	case 10: /* See Invisible */
		set_tim_invis(p_ptr->tim_invis + randint1(plev*2));
		break;
	case 11: /* Dispell */
		set_confused(0);
		set_image(0);
		set_slow(0);
		set_stun(0);
		break;
	case 12: /* Slowing Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_OLD_SLOW, dir, damroll((2+plev/5), 4), 3);
		break;
	case 13: /* Magic Report */
		(void)report_magics();
		break;
	case 14: /* Cure Medium Wounds */
		(void)hp_player(damroll(4, 10));
		(void)set_cut((p_ptr->cut / 2) - 50);
		break;
	case 15: /* Mantle */
		(void)set_shield(p_ptr->shield + rand_range(25, 50));
		break;
	case 16: /* Disintegrate */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam, GF_DISINTEGRATE, dir, damroll(3+(plev-10)/5, 6)+ plev -10);
		break;
	case 17: /* Protection From Evil*/
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * plev);
		break;
	case 18: /* Teleport Other */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_AWAY_ALL, dir, damroll(plev , 10));
		break;
	case 19:/* Dimensional Door */
		dimension_door(1);
		break;
	case 20: /* Summon Monster */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_NO_UNIQUES, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry monster appears!");
		}
		else
		{
			no_trump = TRUE;
		}
		break;
	case 21: /* Hold Monster */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_STASIS, dir, damroll(3+(plev-10)/5, 6)+ plev -10, 1);
		break;
	case 22: /* Disintergration Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_DISINTEGRATE, dir, damroll(3+(plev-10)/5, 6)+ plev -10, -(plev/10)-1);
		break;
	case 23: /* Recharge */
		return recharge(plev * 4);
	case 24:  /* Aura of Energy */
		(void)set_sh_elec(p_ptr->sh_elec + rand_range(plev, plev*beam/11));
		break;
	case 25: /* Magic Meteorite */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_METEOR, dir, damroll(plev - 20, 10), 1);
		break;
	case 26: /* Word Of Recall */
		word_of_recall();
		break;
	case 27: /* Greater Identify */
		return identify_fully();
	case 28:/* Meteorite Shower */
		(void)rain_effect(GF_METEOR, damroll(plev - 20, 10), 2000, 1, 15);
		break;
	case 29: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		break;
	case 30: /* Cosmic Wrath */
		(void)dispel_monsters(plev * 5);
		break;
	case 31: /* Star Shield */
		dummy = rand_range(plev, beam*plev);
		(void)set_shield(p_ptr->shield + dummy);
		(void)set_sh_elec(p_ptr->sh_elec + dummy);
		(void)set_sh_fire(p_ptr->sh_fire + dummy);
		break;
	default:
		msg_format("You cast an unknown Wizardry spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}



/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
#ifdef USE_SCRIPT
	use_skill_callback();
#else /* USE_SCRIPT */
	int	item, sval, spell, realm;
	int	chance;
	int	increment = 0;
	int	use_realm;
	bool cast;

	const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? 
		(((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA) || 
		(p_ptr->pclass == CLASS_ASSASSIN)) ? "skill" : "prayer") : "spell");
	cptr        use = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? 
		(((p_ptr->pclass == CLASS_ROGUE) || (p_ptr->pclass == CLASS_NINJA) || 
		(p_ptr->pclass == CLASS_ASSASSIN)) ? "use" : "recite") : "cast");
	
	object_type	*o_ptr;

	const magic_type *s_ptr;

	cptr q, s;

	/* Require spell ability */
	if (!p_ptr->realm1)
	{
		msg_print("You no magic ability!");
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
	item_tester_tval = mp_ptr->spell_book;

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

#ifdef USE_SCRIPT
	if (object_cast_callback(o_ptr)) return;
#endif /* USE_SCRIPT */

	/* Access the item's sval */
	sval = o_ptr->sval;

	if (o_ptr->tval == REALM2_BOOK) increment = 32;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	if (increment) realm = p_ptr->realm2;
	else realm = p_ptr->realm1;

	/* Ask for a spell */
	if (!get_spell(&spell, use, sval, TRUE, (bool)(increment ? TRUE : FALSE), FALSE))
	{
		if (spell == -2)
			msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	use_realm = (increment ? p_ptr->realm2 : p_ptr->realm1);

	s_ptr = &mp_ptr->info[use_realm - 1][spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.", use, prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell, use_realm - 1);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);
		sound(SOUND_FAIL);

		if ((randint1(100) < chance) && (mp_ptr->spell_book == TV_LIFE_BOOK))
			chg_virtue(V_FAITH, -1);
		else if (randint1(100) < chance)
			chg_virtue(V_KNOWLEDGE, -1);


		if ((o_ptr->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
		else if ((o_ptr->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
		{
			if ((sval == 3) && one_in_(2))
			{
				msg_print("Your sanity is shaken by reading the Necronomicon!");
				
				/* Mind blast */
				if (!saving_throw(p_ptr->skill_sav * 100))
				{
					if (!p_ptr->resist_confu)
					{
						(void)set_confused(p_ptr->confused + rand_range(4, 8));
					}
					if (!p_ptr->resist_chaos && one_in_(3))
					{
						(void)set_image(p_ptr->image + rand_range(150, 400));
					}
				}

				/* Lose int & wis */
				else if (!saving_throw(p_ptr->skill_sav * 100))
				{
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
				}
			}
			else
			{
				msg_print("It hurts!");
				take_hit(damroll(o_ptr->sval + 1, 6), "a miscast Death spell");
				if ((spell > 15) && one_in_(6) && !p_ptr->hold_life)
					lose_exp(spell * 250);
			}
		}
	}

	/* Process spell */
	else
	{
		if ((randint1(100) < chance) && (chance < 50))
		{
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
		}

		/* Spells. */
		switch (realm)
		{
		case REALM_LIFE: /* * LIFE * */
			cast = cast_life_spell(spell);
			break;
		case REALM_ORDER: /* * ORDER * */
			cast = cast_order_spell(spell);
			break;
		case REALM_FIRE: /* * FIRE * */
			cast = cast_fire_spell(spell);
			break;
		case REALM_AIR: /* * AIR * */
			cast = cast_air_spell(spell);
			break;
		case REALM_DEATH: /* * DEATH * */
			cast = cast_death_spell(spell);
			break;
		case REALM_CHAOS: /* CHAOS */
			cast = cast_chaos_spell(spell, TRUE);
			break;
		case REALM_ASTRAL: /* ASTRAL */
			cast = cast_astral_spell(spell);
			break;
		case REALM_WATER: /* WATER */
			cast = cast_water_spell(spell);
			break;
		case REALM_EARTH: /* EARTH */
			cast = cast_earth_spell(spell);
			break;
		case REALM_WIZARD: /* WIZARD */
			cast = cast_wizard_spell(spell);
			break;
		default:
			cast = FALSE;
			msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
			msg_print(NULL);
		}

		/* Canceled spells cost neither a turn nor mana */
		if (!cast) return;

		/* A spell was cast */
		if (!(increment ?
		    (p_ptr->spell_worked2 & (1L << spell)) :
		    (p_ptr->spell_worked1 & (1L << spell))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
			if (realm == p_ptr->realm1)
			{
				p_ptr->spell_worked1 |= (1L << spell);
			}
			else
			{
				p_ptr->spell_worked2 |= (1L << spell);
			}

			/* Gain experience */
			gain_exp(e * s_ptr->slevel);

			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				chg_virtue(V_FAITH, 1);
			else
				chg_virtue(V_KNOWLEDGE, 1);
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
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		if (mp_ptr->spell_book == TV_LIFE_BOOK)
			chg_virtue(V_FAITH, -10);
		else
			chg_virtue(V_KNOWLEDGE, -10);

		/* Damage CON (possibly permanently) */
		if (one_in_(2))
		{
			bool perm = one_in_(4);

			/* Message */
			msg_print("You have damaged your health!");

			/* Reduce constitution */
			(void)dec_stat(A_CON, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
#endif /* USE_SCRIPT */
}


/*
 * Pray a prayer -- Unused in Zangband
 */
void do_cmd_pray(void)
{
	msg_format("Praying is not used in %s. Use magic spell casting instead.",
	           VERSION_NAME);
}


/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
	int i = 0;
	int powers[36];
	cptr power_desc[36];
	bool flag, redraw;
	int ask;
	char choice;
	char out_val[160];
	int pets = 0;
	int pet_ctr;
	bool all_pets = FALSE;
	monster_type *m_ptr;
	int mode = 0;
	byte y = 1, x = 0;
	int ctr = 0;
	char buf[160];
	int num = 0;


	/* Calculate pets */
	/* Process the monsters (backwards) */
	for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
	{
		/* Access the monster */
		m_ptr = &m_list[pet_ctr];

		if (is_pet(m_ptr)) pets++;
	}

	if (pets)
	{
		power_desc[num] = "dismiss pets";
		powers[num++] = PET_DISMISS;
	}

	power_desc[num] = "stay close";
	if (p_ptr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
	powers[num++] = PET_STAY_CLOSE;

	power_desc[num] = "follow me";
	if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
	powers[num++] = PET_FOLLOW_ME;

	power_desc[num] = "seek and destroy";
	if (p_ptr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
	powers[num++] = PET_SEEK_AND_DESTROY;

	power_desc[num] = "give me space";
	if (p_ptr->pet_follow_distance == PET_SPACE_DIST) mode = num;
	powers[num++] = PET_ALLOW_SPACE;

	power_desc[num] = "stay away";
	if (p_ptr->pet_follow_distance == PET_AWAY_DIST) mode = num;
	powers[num++] = PET_STAY_AWAY;

	if (p_ptr->pet_open_doors)
	{
		power_desc[num] = "pets may open doors";
	}
	else
	{
		power_desc[num] = "pets may not open doors";
	}
	powers[num++] = PET_OPEN_DOORS;

	if (p_ptr->pet_pickup_items)
	{
		power_desc[num] = "pets may pick up items";
	}
	else
	{
		power_desc[num] = "pets may not pick up items";
	}
	powers[num++] = PET_TAKE_ITEMS;

	/* Nothing chosen yet */
	flag = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
		(void)strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), I2A(num - 1));
	}
	else
	{
		(void)strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
			I2A(0), '0' + num - 27);
	}

	/* Show list */
	redraw = TRUE;

	/* Save the screen */
	Term_save();

	prt("", y++, x);

	while (ctr < num)
	{
		sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
		prt(buf, y + ctr, x);
		ctr++;
	}

	if (ctr < 17)
	{
		prt("", y + ctr, x);
	}
	else
	{
		prt("", y + 17, x);
	}

	/* Get a command from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				y = 1;
				x = 0;
				ctr = 0;

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt("", y++, x);

				while (ctr < num)
				{
					sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
					prt(buf, y + ctr, x);
					ctr++;
				}

				if (ctr < 17)
				{
					prt("", y + ctr, x);
				}
				else
				{
					prt("", y + 17, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Verify it */
		if (ask)
		{
			/* Prompt */
			(void)strnfmt(buf, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(buf)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag)
	{
		p_ptr->energy_use = 0;
		return;
	}

	switch (powers[i])
	{
		case PET_DISMISS: /* Dismiss pets */
		{
			int Dismissed = 0;

			if (get_check("Dismiss all pets? ")) all_pets = TRUE;

			/* Process the monsters (backwards) */
			for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
			{
				/* Access the monster */
				m_ptr = &m_list[pet_ctr];

				if (is_pet(m_ptr))
				{
					bool delete_this = FALSE;

					if (all_pets)
						delete_this = TRUE;
					else
					{
						char friend_name[80], check_friend[80];
						monster_desc(friend_name, m_ptr, 0x80);
						sprintf(check_friend, "Dismiss %s? ", friend_name);

						if (get_check(check_friend))
							delete_this = TRUE;
					}

					if (delete_this)
					{
						/* Notice changes in view */
						if (r_info[m_ptr->r_idx].flags7 &
							 (RF7_LITE_1 | RF7_LITE_2))
						{
							/* Update some things */
							p_ptr->update |= (PU_MON_LITE);
						}
												
						delete_monster_idx(pet_ctr);
						Dismissed++;
					}
				}
			}

			msg_format("You have dismissed %d pet%s.", Dismissed,
				(Dismissed == 1 ? "" : "s"));
			break;
		}
		/* Call pets */
		case PET_STAY_CLOSE:
		{
			p_ptr->pet_follow_distance = PET_CLOSE_DIST;
			break;
		}
		/* "Follow Me" */
		case PET_FOLLOW_ME:
		{
			p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
			break;
		}
		/* "Seek and destoy" */
		case PET_SEEK_AND_DESTROY:
		{
			p_ptr->pet_follow_distance = PET_DESTROY_DIST;
			break;
		}
		/* "Give me space" */
		case PET_ALLOW_SPACE:
		{
			p_ptr->pet_follow_distance = PET_SPACE_DIST;
			break;
		}
		/* "Stay away" */
		case PET_STAY_AWAY:
		{
			p_ptr->pet_follow_distance = PET_AWAY_DIST;
			break;
		}
		/* flag - allow pets to open doors */
		case PET_OPEN_DOORS:
		{
			p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
			break;
		}
		/* flag - allow pets to pickup items */
		case PET_TAKE_ITEMS:
		{
			p_ptr->pet_pickup_items = !p_ptr->pet_pickup_items;

			/* Drop objects being carried by pets */
			if (!p_ptr->pet_pickup_items)
			{
				for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
				{
					/* Access the monster */
					m_ptr = &m_list[pet_ctr];

					if (is_pet(m_ptr))
					{
						monster_drop_carried_objects(m_ptr);
					}
				}
			}

			break;
		}
	}
}

