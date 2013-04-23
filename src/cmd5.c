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

	int	dir, i;
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
		(void)fire_ball(GF_HOLY_FIRE, dir, damroll(MIN(2+plev*2/3, 20), 3), 0);
		break;
	case 2: /* Cure Light Wounds */
		(void)cure_wounds(1);
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 3: /* Bless */
		(void)set_blessed(p_ptr->blessed + rand_range(5 + plev, 5 + beam * 5));
		break;
	case 4: /* Entangle */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_GROWTH, dir, damroll(plev, 3), 2+plev/25);
		break;
	case 5: /* Grow Food */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 6: /* Remove Fear */
		(void)set_afraid(0);
		break;
	case 7: /* Cure Medium Wounds*/
		(void)cure_wounds(2);
		(void)set_cut(p_ptr->cut / 2 - 20);
		(void)set_poisoned(p_ptr->poisoned - 20);
		break;
	case 8: /* Remove Curse */
		(void)remove_curse();
		(void)set_fatigue(p_ptr->fatigue + 1000);
		break;
	case 9: /* Animal Taming */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)charm_animal(dir, plev);
		break;
	case 10: /* Nature's Awareness -- downgraded */
		(void)map_area();
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();
		break;
	case 11: /* Holy Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_HOLY_FIRE, dir, (damroll(6, 6) + plev*2), 1 + beam / 30);
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
		(void)cure_wounds(4);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(p_ptr->poisoned / 2 - 20);
		break;
	case 15: /* Sanctuary */
		(void)warding_glyph();
		(void)fire_ball(GF_GROWTH, 0, damroll(plev-10, 10), 2 + beam / 25);
		(void)cure_wounds(3);
		(void)set_fatigue(p_ptr->fatigue + 200);
		break;
	case 16: /* Animal Friendship */
		(void)charm_animals(plev + beam);
		break;
	case 17: /* Bark Skin */
		(void)set_ac2(p_ptr->ac2 + rand_range(50, 100));
		break;
	case 18: /* Vitality */
		(void)set_vitality(p_ptr->vitality + rand_range(50, 100));
		(void)hp_player(100);
		break;
	case 19: /* Resistance True */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 20: /* Summon Animals */
		if (!(summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE)));
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
	case 22: /* Silvan Song */
		(void)fire_ball(GF_HOLY_FIRE, 0, plev*4+100, 4);
		(void)fire_ball(GF_GROWTH, 0, beam*4+100, 4);
		break;
	case 23: /* Nature's Blessing */
		(void)set_ac1(p_ptr->ac1 + rand_range(50, 100));
		(void)set_ac2(p_ptr->ac2 + rand_range(50, 100));
		(void)set_hero(p_ptr->hero + rand_range(50, 100));
		(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
		(void)hp_player(50);
		if (!p_ptr->fast)	(void)set_fast(rand_range(50, 100));
		else			(void)set_fast(p_ptr->fast + randint1(10));
		(void)set_fatigue(p_ptr->fatigue + 1000);
		break;
	case 24: /* Exorcism */
		(void)dispel_undead(plev);
		(void)dispel_demons(plev);
		(void)turn_evil(plev);
		break;
	case 25: /* Cure Mortal Wounds */
		(void)cure_wounds(5);
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 26: /* Banishment */
		if (banish_evil(100))
		{
			msg_print("The power of your god banishes evil!");
		}
		break;
	case 27: /* Dispel Undead + Demons */
		(void)dispel_undead(beam * 3);
		(void)dispel_demons(beam * 3);
		break;
	case 28: /* Divine Touch */
	{
		char ch;
		int bless = 0;
		
		while (TRUE)
		{
			if (!get_com(" Do you wish for (B)less Weapon or *(R)emove Curse* ? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'B' || ch == 'b')
			{
				bless = 1;
				break;
			}
			if (ch == 'R' || ch == 'r')
			{
				break;
			}
		}
		(void)set_fatigue(p_ptr->fatigue + 2000);
		if (bless == 1)		return bless_weapon();
		else (void)remove_all_curse();
	}
		break;		
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
		(void)cure_wounds(6);
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 30: /* Angelic Shield */
		(void)summon_specific(-1, py, px, plev, SUMMON_ANGEL, TRUE, TRUE, TRUE);
		(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
		(void)set_vitality(p_ptr->vitality + rand_range(50, 100));
		(void)hp_player(100);
		(void)set_afraid(0);
		break;
	case 31: /* Holy Invulnerability */
		(void)set_invuln(p_ptr->invuln + rand_range(7, 14));
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * plev);
		(void)set_fatigue(p_ptr->fatigue + 1000);
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
		(void)dimension_door(0);
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
		(void)set_fatigue(p_ptr->fatigue + 100);
		return ident_spell();
		break;
	case 5: /* Teleport Self */
		(void)dimension_door(1);
		break;
	case 6: /* Slow Monster */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)slow_monster(dir);
		break;
	case 7: /* See Invisible */
		(void)set_tim_invis(p_ptr->tim_invis + rand_range(plev,plev*2));
		break;
	case 8: /* Magic Mapping */
		(void)map_area();
		break;
	case 9: /* Heroism */
		(void)set_hero(p_ptr->hero + rand_range(plev, beam*3));
		(void)hp_player(20);
		(void)set_afraid(0);
		break;
	case 10:/* Fire + Ice */
		(void)project(0, plev / 10 + 2, p_ptr->py, p_ptr->px, plev*3, GF_TEMP, PROJECT_KILL);
		break;
	case 11: /* Mass Identify */
		(void)identify_pack();
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 12: /* Haste Self */
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(25 + plev) + plev);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(5));
		}
		(void)project(0, 1, p_ptr->py, p_ptr->px, 1, GF_OLD_SPEED, PROJECT_KILL);
		break;
	case 13: /* Inertia */
		(void)project(0, beam / 10 + 2, p_ptr->py, p_ptr->px, beam*3, GF_INERTIA, PROJECT_KILL);
		break;
	case 14: /* Boost Self */
		if (!p_ptr->boost_all)
		{
			(void)set_boost_all(p_ptr->boost_all + rand_range(50, 50 + plev));
		}
		else
		{
			(void)set_boost_all(p_ptr->boost_all + randint1(plev));
		}
		break;
	case 15: /* Identify True */
		(void)set_fatigue(p_ptr->fatigue + 1000);
		return identify_fully();
	case 16: /* Divine Wisdom */
		if (!p_ptr->boost_wis)
		{
			(void)set_boost_wis(p_ptr->boost_wis + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_wis(p_ptr->boost_wis + randint1(plev));
		}
		break;
	case 17: /* Detect All */
		(void)detect_all();
		break;
	case 18: /* Resist Elements */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 19: /* Righteousness */
		(void)set_hero(p_ptr->hero + rand_range(plev, beam*2));
		(void)set_blessed(p_ptr->blessed + rand_range(plev, beam*2));
		(void)hp_player(20);
		(void)set_afraid(0);
		break;
	case 20: /* Cold Flame */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_TEMP, dir, plev * 4 + 100, 1+plev/25);
		break;
	case 21: /* Self knowledge */
		(void)self_knowledge();
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 22: /* Inertia Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_INERTIA, dir, beam * 4 + 100, -(1+plev/25));
		break;
	case 23: /* Word of Recall */
		(void)word_of_recall();
		break;
	case 24: /* Stasis */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)stasis_monster(dir);
		break;
	case 25: /* Dimensional Gate */
		(void)dimension_door(3);
		break;
	case 26: /* Warding Circle */
		(void)fire_ball(GF_MAKE_GLYPH, 0, 1, 1);
		break;
	case 27: /* Clairvoyance */
		wiz_lite();
		if (!(p_ptr->telepathy))
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
		}
		break;
	case 28: /* Enchant Weapon */
		(void)set_fatigue(p_ptr->fatigue + 2000);
		return enchant_spell(randint1(4), randint1(4), 0);
	case 29: /* Enchant Armour */
		(void)set_fatigue(p_ptr->fatigue + 2000);
		return enchant_spell(0, 0, rand_range(2, 5));
	case 30: /* Alchemy */
		return alchemy();
	case 31: /* Pattern Weapon */
		(void)set_fatigue(p_ptr->fatigue + 5000);
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

	int	dir, i, beam;
	int	plev = p_ptr->lev;
	bool pet = (!one_in_(3));
	cave_type *c_ptr;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_FIRE)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* R Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(plev, plev*beam/11 + 9));
		break;
	case 1: /* Ignite */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll(MIN(2+plev*2/3, 20), 3));
		break;
	case 2: /* Smoke */
		if (!p_ptr->resist_blind)
		{
			(void)set_blind(p_ptr->blind + rand_range(4, 9));
		}
		unlite_area(10, 3);
		(void)set_invisible(p_ptr->tim_nonvis + rand_range(5, 12));
		break;
	case 3: /* Circle of Cinder */
		(void)fire_ball(GF_FIRE, 0, damroll(MIN(1+plev*2/3, 20), 3), 2+plev/25);
		break;
	case 4: /* Firelight */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 5: /* Blast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(MIN(2+plev*2/3, 20), 3) + plev, 1);
		break;
	case 6: /* Destroy Doors */
		(void)destroy_doors(5);
		break;
	case 7: /* Firebolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 8: /* R Cold */
		(void)set_oppose_cold(p_ptr->oppose_fire + rand_range(20, 50));
		break;
	case 9: /* Flamethrower */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_FIRE, dir, plev + 15);
		break;
	case 10: /* Refuel */
		(void)phlogiston();
		break;
	case 11: /* Recharge */
		(void)set_fatigue(p_ptr->fatigue + 100);
		return recharge(plev * 4);
		break;
	case 12: /* Explode */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6), 1+plev/20);
		break;
	case 13: /* Phoenix Shield */
		(void)set_sh_fire(p_ptr->tim_sh_fire + rand_range(plev, plev*beam/11));
		break;
	case 14: /* Warmth */
		(void)cure_wounds(2);
		break;
	case 15: /* Wild Fire */
		for (i = 0; i <= beam/8; i++)
		{
			wild_blast(py, px, damroll(plev/5, plev), GF_FIRE, 2);
		}
		break;
	case 16: /* Fire Ball */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_FIRE, dir, damroll(MAX((plev*plev-1000)/15, 1), 10));
		break;
	case 17: /* Flame Animation */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_FIREE, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry elemental appears!");
		}
		break;
	case 18: /* Rapid Fire */
	{
		bool cast = TRUE;
		char ch;
		object_type *j_ptr;
		j_ptr = &inventory[INVEN_BOW];
		
		/* Player can multi-cast or multi-shoot if they are using a missile weapon. */
		while (j_ptr->tval == TV_BOW)
		{
			if (!get_com(" Rapid (S)hoot or Rapid (C)ast ? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'S' || ch == 's')
			{
				cast = FALSE;
				break;
			}
			if (ch == 'C' || ch == 'c')
			{
				break;
			}
		}
			
		for ( i=1; i<=plev/10; i++)
		{
			if (cast)
				do_cmd_cast();
			else
				do_cmd_fire();
		}
		(void)set_fatigue(p_ptr->fatigue + 100);
		break;
	}
	case 19: /* Scorching Beam */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_PLASMA, dir, damroll(MAX((plev*plev-1000)/15, 1), 10));
		break;
	case 20: /* Phoenix Tear */
		(void)restore_level();
		(void)cure_wounds(5);
		(void)set_fatigue(p_ptr->fatigue + 200);
		break;
	case 21: /* Incinerate */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_PLASMA, dir, damroll(12, plev+beam), 0);
		break;
	case 22: /* Sun Strike */
		(void)lite_area(damroll(2, plev), (plev / 10) + 1);
		(void)detect_all();
		(void)turn_undead();
		(void)dispel_undead(beam+plev);
		break;
	case 23: /* Dragon's Breath :) */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FIRE, dir, damroll(MAX((plev*plev-1000)/15, 1), 10) + plev, -3);
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 24: /* Enchant Weapon */
		(void)set_fatigue(p_ptr->fatigue + 500);
		return enchant_spell(randint1(4), randint1(4), 0);
		break;
	case 25: /* Lava Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_LAVA, dir, damroll(plev, 6)+ plev*5);
		break;
	case 26: /* Plasma Shift */
		(void)dimension_door(1);
		(void)fire_ball(GF_PLASMA, 0, 100 + plev, 3);
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
		(void)fire_beam(GF_LAVA, dir, damroll(plev, 6)+ plev * 5);
		break;
	case 30: /* Fire Brand */
		(void)set_fatigue(p_ptr->fatigue + 5000);
		brand_weapon(5);
		break;
	case 31: /* Volcano */
		(void)fire_ball(GF_LAVA, 0, damroll(plev * 2, 10), beam/10 +5);
		(void)earthquake(py, px, plev/2, 0);
		(void)set_fatigue(p_ptr->fatigue + 1000);
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

	int	dir, i, beam;
	int	plev = p_ptr->lev;
	bool pet = (!one_in_(3));
	cave_type *c_ptr;

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_AIR)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;
	
	switch (spell)
	{
	case 0: /* Resist Elec */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(plev, plev*beam/11 + 9));
		break;
	case 1: /* Zap */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, damroll(MIN(2+plev*2/3, 20), 3));
		break;
	case 2: /* Call Light */
		(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
		break;
	case 3: /* Lesoch */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_FORCE, dir, damroll(plev, 2));
		break;
	case 4: /* Speed */
		(void)set_slow(0);
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
		(void)teleport_player(10);
		break;
	case 7: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam-10, GF_ELEC, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 8: /* IZYUK */
		(void)set_ffall(p_ptr->ffall + rand_range(plev, beam*5));
		break;		
	case 9: /* Shock Ball */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll(MIN(2+plev*2/3, 20), 3), 1+plev/20);
		break;
	case 10: /* Free Action */
		(void)set_free_act(p_ptr->free_act + rand_range(plev, beam*5));
		break;	
	case 11: /* Whirlwind */
		(void)fire_ball(GF_FORCE, 0, damroll(MAX(MIN(4+(plev-10)*5/3, 50), 2), 6), 2+plev/25);
		break;
	case 12: /* Lightning Shift */
		(void)teleport_player(plev * 4);
		break;
	case 13: /* Fork Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6) + plev, -1);
		break;
	case 14: /* Whispering Winds */
		(void)detect_doors();
		(void)detect_stairs();
		(void)detect_monsters_normal();
		break;
	case 15: /* Minor Storm */
		for (dir = 1; dir <= 9; dir++)
		{
			if (dir != 5);
			{
				(void)fire_bolt(GF_ELEC, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6) + plev);
			}
		}
		(void)set_fatigue(p_ptr->fatigue + 300);
		break;
	case 16: /* Speed + */
		(void)set_slow(0);
		if (!p_ptr->fast)
		{
			(void)set_fast(randint1(50 + plev) + plev*2);
		}
		else
		{
			(void)set_fast(p_ptr->fast + randint1(plev));
		}
		break;
	case 17: /* Light Burst */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_LITE, dir, damroll(5, plev) + plev*3, -(1+plev/25));
		break;
	case 18: /* Invisiblity */
		(void)set_invisible(p_ptr->tim_nonvis + rand_range(50, 100));
		break;
	case 19: /* Sonic Boom */
		msg_print("BOOM!");
		(void)project(0, plev / 10 + 2, py, px, plev*7, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
		break;
	case 20: /* Restore Mind */
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)set_fatigue(p_ptr->fatigue + 2000);
		break;
	case 21: /* Tornado */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(MAX((plev*plev-1000)/15, 1), 10), 1+plev/2);
		break;
	case 22: /* Aether Shift */
		(void)word_of_recall();
		break;
	case 23: /* Cyclone */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(MAX((plev*plev-1000)/15, 1), 10) + plev, -(1+plev/25));
		(void)set_fatigue(p_ptr->fatigue + 1000);
		break;
	case 24: /* Storm Shield */
		(void)set_sh_elec(p_ptr->tim_sh_elec + rand_range(plev, plev*beam/11));
		break;
	case 25: /* Ball Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ELEC, dir, damroll(MAX((plev*plev-1000)/15, 1), 10));
		break;
	case 26: /* Call Air */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_AIRE, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry elemental appears!");
		}
		break;
	case 27: /* Storm */
		for (dir = 1; dir <= 9; dir++)
		{
			(void)fire_ball(GF_ELEC, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6)+ plev, 2);
		}
		break;
	case 28: /* Intelligence */
		if (!p_ptr->boost_all)
		{
			(void)set_boost_int(p_ptr->boost_int + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_int(p_ptr->boost_int + randint1(plev));
		}
		break;
	case 29: /* Chain Lightning */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ELEC, dir, damroll(MAX((plev*plev-1000)/15, 1), 10), -3);
		break;
	case 30: /* Storm Brand */
		(void)set_fatigue(p_ptr->fatigue + 5000);
		(void)brand_weapon(7);
		break;
	case 31: /* Mega Storm */
		(void)fire_ball(GF_FORCE, 0, damroll(MAX((plev*plev-1000)/15, 1), 10), 2+plev/25);
		for (dir = 1; dir <= 9; dir++)
		{
			(void)fire_ball(GF_ELEC, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6)+ plev + beam, -2);
		}
		(void)set_fatigue(p_ptr->fatigue + 1000);
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
			damroll(MIN(2+(plev-1)*2/3, 20), 3), 0);

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
		(void)fire_ball(GF_POIS, dir, 10 + plev * 3 / 2, 2);
		break;
	case 4: /* Scare */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fear_monster(dir, plev);
		break;
	case 5: /* Resist Poison */
		(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		break;
	case 6: /* Nether Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_NETHER, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 7: /* Enslave the Undead */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)control_one_undead(dir, plev);
		break;
	case 8: /* Orb of Entropy */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_OLD_DRAIN, dir, (damroll(6, 6) + plev*2), 1 + beam / 30);
		break;
	case 9: /* Viper Ray */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_POIS, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
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
		(void)set_fatigue(p_ptr->fatigue + 4500);
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
		(void)set_fatigue(p_ptr->fatigue + 1000);
		break;
	case 16: /* Berserk */
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)hp_player(30);
		(void)set_afraid(0);
		break;
	case 17: /* Dread */
		(void)turn_monsters(beam + 3*plev);
		(void)stun_monsters(beam + 2*plev);
	case 18: /* Dark Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_DARK, dir, damroll(MAX((plev*plev-1000)/15, 1), 10));
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
	case 20: /* Hex */
		(void)dispel_monsters(2*plev);
		(void)mindblast_monsters(plev*4);
		break;
	case 21: /* Vampiric Branding */
		(void)set_fatigue(p_ptr->fatigue + 5000);
		brand_weapon(3);
		break;
	case 22: /* Nether Storm */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_NETHER, dir, plev*5, beam/10);
		break;
	case 23: /* Mass Genocide */
		(void)mass_genocide(TRUE);
		(void)set_fatigue(p_ptr->fatigue + 100);
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
	case 26: /* Word of Death */
		(void)dispel_living(plev * 4);
		break;
	case 27: /* Esoteria */
		if (randint1(50) > plev)
			return ident_spell();
		else
		{
			(void)identify_pack();
			if (one_in_(3))
			{
				msg_print("A mass of undead voices enter your head! ");
				(void)set_confused(55 - plev);
			}	
		}
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 28: /* Plague */
		(void)slow_monsters();
		(void)project(0, plev / 10 + 3, py, px, plev*6, GF_POIS, PROJECT_KILL);
		break;
	case 29: /* Hell Gate */
	{
		char ch;
		int fire = 0;
		
		while (TRUE)
		{
			if (!get_com(" Summon (H)ellfire or Summon (D)emon? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'H' || ch == 'h')
			{
				fire = 1;
				break;
			}
			if (ch == 'D' || ch == 'd')
			{
				break;
			}
		}
		if (fire == 1)
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_HELL_FIRE, dir, 666, 3);
			take_hit(rand_range(50, 100), "the strain of casting Hellfire");
			break;
		}
		else
		{
			bool pet = (!one_in_(3));
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_DEMON, group, FALSE, pet))
			{
				msg_print("The area fills with a stench of sulphur and brimstone...");

				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
			break;
		}
	}
		break;			
	case 30: /* Necropotence */
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
			take_hit(randint1(4), "the strain of casting Necropotence");

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
		(void)set_fatigue(p_ptr->fatigue + 1000);
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
		case 0: /* Blink */
			if (success)
			{
				teleport_player(10);
			}
			break;
		case 1: /* Crazy bullet */
			if (success)
			{
				for (dummy = 0; dummy <= ((randint1(plev)) / 5 + 1); dummy++)
				{
					wild_blast(py, px, damroll(1, plev + 10), GF_MANA, 0);
				}
			}
			break;
		case 2: /* Flash of Light */
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
				msg_format("There is a %s flash.", word);
				(void)lite_area(damroll(1, plev), (plev / 10) + 1);
				break;
				
			}
			break;
		case 3: /* Healing Spirits */
			if (success)
			{
				int die = randint1(plev/10+5) - 1;
				
				if (die < 2)
				{
					msg_print("A spirit sreams 'HeAl ThiS!' ");
					take_hit(damroll(2,plev/4), "an angry spirit of chaos");
					break;
				}
				else if (die < 6)
				{
					do_poly_wounds();
					break;
				}
				else if (die < 7)
				{
					cure_wounds(1);
					break;
				}
				else if (die < 8)
				{
					cure_wounds(2);
					break;
				}
				else if (die < 9)
				{
					msg_print("A spirit touches your wounds");
					cure_wounds(3);
					break;
				}
				else if (die < 10)
				{
					msg_print("A spirit flies into your wounds");
					cure_wounds(4);
					break;
				}
				else
				{
					msg_print("A spirit merges with your life!");
					cure_wounds(5);
					break;
				}
			}
		case 4: /* Confusion */
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
		case 5: /* Trump Spying */
			if (success)
			{
				(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			}
			break;
		case 6: /* Hand of Fate */
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
					
					/* Only hit half the monsters */
					if (one_in_(2)) continue;

					(void)project(0, 0, y, x, damroll(2, plev+15), GF_MISSILE, PROJECT_KILL);
				}
				if (one_in_(10)) take_hit(rand_range(1, plev), "the Hand of Fate");
			}
			break;
		case 7: /* Teleport */
			if (success)
			{
				teleport_player(randint1(plev*8));
			}
			break;
		case 8: /* Chaos Bolt */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_CHAOS, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
			}
			break;
		case 9: /* Defensive Trump */
		{		/*
				 * This spell can cause several random effects to happen.
				 * The quality of the effect is improved with levels.
				 */
				int chance = (plev + beam)/10 - 5;
				msg_print(" You shuffle the cards... ");
			do
			{
				if (one_in_(30 + chance))
				{
					msg_print("Oh no! It's the Devil card!");
						(void)activate_hi_summon();
						(void)activate_hi_summon();
						if (!one_in_(5)) break;
				}
				if (one_in_(33 + chance))
				{
					int count = 0;
					msg_print("Oh no! It's Doom card.");
					(void)activate_ty_curse(FALSE, &count);
					if (!one_in_(5)) break;
				}
				if (one_in_(18 + chance))
				{
					msg_print("Oh no! It's the swords of discord.");
					aggravate_monsters(0);
					if (!one_in_(5)) break;
				}
				if (one_in_(28 + chance))
				{
					msg_print("Oh no! It's the Fool card.");
					(void)do_dec_stat(A_INT);
					(void)do_dec_stat(A_WIS);
					if (!one_in_(5)) break;
				}
				if (one_in_(23 + chance))
				{
					msg_print("Oh no! It's the picture of an angry monster.");
					if (!(summon_specific(0, py, px, (p_ptr->depth * 3) / 2, rand_range(33, 38), TRUE, FALSE, FALSE)))
						no_trump = TRUE;
					if (!one_in_(5)) break;
				}
				if (one_in_(24 + chance))
				{
					msg_print("Oh no! It's the Glue card.");
					set_paralyzed(10);
					if (!one_in_(5)) break;
				}
				if (one_in_(21 + chance))
				{
					msg_print("Oh no! It's the Slow card.");
					set_slow(p_ptr->slow + rand_range(10, 30));
					if (!one_in_(5)) break;
				}
				if (one_in_(18 + chance))
				{
					msg_print("Oh no! It's the Insanity card.");
					(void)set_confused(p_ptr->confused + rand_range(10, 40));
					if (!one_in_(5)) break;
				}
				if (one_in_(15 + chance))
				{
					msg_print("Oh no! It's the Wierd card.");
					(void)set_image(p_ptr->image + rand_range(10, 40));
					if (!one_in_(4)) break;
				}
				if (one_in_(11))
				{
					msg_print("It's the Wheel of Fortune card.");
					wild_magic(randint0(32));
					if (!one_in_(5)) break;
				}
				if (one_in_(7))
				{
					msg_print("It's the blink card.");
					teleport_player(10);
					if (!one_in_(4)) break;
				}
				if (one_in_(11))
				{
					msg_print("It's the teleport card.");
					teleport_player(100);
					if (!one_in_(5)) break;
				}
				if (one_in_(13))
				{
					msg_print("It's the Cracked Wall.");
					wall_breaker();
					if (!one_in_(4)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(12 - chance))
				{
					msg_print("It's the Resistance card.");
					(void)set_resist_magic(p_ptr->resist_magic + rand_range(20, 40));
					if (!one_in_(5)) break;
				}
				if (one_in_(8 - chance))
				{
					msg_print("It's the Shield card.");
					(void)set_ac1(p_ptr->ac1 + rand_range(20, 40));
					if (!one_in_(2)) break;
				}
				if (one_in_(14 - chance))
				{
					msg_print("It's the Armor card.");
					(void)set_ac2(p_ptr->ac2 + rand_range(20, 40));
					if (!one_in_(4)) break;
				}
				if (one_in_(19 - chance))
				{
					msg_print("It's the Stoneskin card.");
					(void)set_shield(p_ptr->shield + rand_range(20, 40));
					if (!one_in_(6)) break;
				}
				if (one_in_(9 - chance))
				{
					msg_print("It's the Minor Health card.");
					(void)cure_wounds(1);
					if (!one_in_(2)) break;
				}
				if (one_in_(13 - chance))
				{
					msg_print("It's the Health card.");
					(void)cure_wounds(2);
					if (!one_in_(4)) break;
				}
				if (one_in_(21 - chance))
				{
					msg_print("It's the Greater Health card.");
					(void)cure_wounds(3);
					if (!one_in_(7)) break;
				}
				if (one_in_(14 - chance))
				{
					msg_print("It's the Food card.");
					(void)set_food(PY_FOOD_FULL - 1);
					if (!one_in_(5)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE1, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
					if (!one_in_(5)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE2, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
					if (!one_in_(5)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE4, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
					if (!one_in_(5)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the picture of a friendly monster.");
					if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE5, FALSE, TRUE, TRUE)))
						no_trump = TRUE;
					if (!one_in_(5)) break;
				}
				if (one_in_(11 - chance))
				{
					msg_print("It's the Snooze card.");
					(void)sleep_monsters_touch();
					if (!one_in_(4)) break;
				}
				if (one_in_(12 - chance))
				{
					msg_print("It's the Lovers.");
					if (get_aim_dir(&dir))
						(void)charm_monster(dir, MIN(p_ptr->lev, 20));
					if (!one_in_(5)) break;
				}
				if (one_in_(20 - chance))
				{
					msg_print("It's the Dove card.");
					(void)charm_monsters(plev * 2);
					if (!one_in_(6)) break;
				}
				if (one_in_(17 - chance))
				{
					msg_print("It's the Blank card.");
					(void)set_invisible(p_ptr->tim_nonvis + rand_range(20, 40));
					if (!one_in_(5)) break;
				}
				if (one_in_(15 - chance))
				{
					msg_print("It's the Wisp card.");
					(void)set_ffall(p_ptr->ffall + rand_range(20, 40));
					(void)set_free_act(p_ptr->free_act + rand_range(20, 40));
					if (!one_in_(5)) break;
				}
				if (one_in_(19 - chance))
				{
					msg_print("It's the Wind card.");
					(void)set_fast(p_ptr->fast + rand_range(10, 30));
					if (!one_in_(6)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the Sun card.");
					wiz_lite();
					if (!one_in_(5)) break;
				}
				if (one_in_(16 - chance))
				{
					msg_print("It's the Change card.");
					do_poly_self();
					if (!one_in_(5)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the Mana card.");
					p_ptr->csp += p_ptr->msp/5;
					p_ptr->csp_frac = 0;
					if (!one_in_(7)) break;
				}
				if (one_in_(13 - chance))
				{
					msg_print("It's the Clock card.");
					p_ptr->energy += plev*2;
					if (!one_in_(4)) break;
				}
				if (one_in_(18 - chance))
				{
					msg_print("It's the Bless card.");
					(void)set_blessed(p_ptr->blessed + rand_range(10, 50));
					if (!one_in_(5)) break;
				}
				if (one_in_(23 - chance))
				{
					msg_print("It's the Hero card.");
					(void)set_hero(p_ptr->hero + rand_range(10, 50));
					if (!one_in_(5)) break;
				}
				if (one_in_(19 - chance))
				{
					msg_print("It's the Rage card.");
					(void)set_shero(p_ptr->shero + rand_range(10, 50));
					if (!one_in_(5)) break;
				}
				if (one_in_(15 - chance))
				{
					msg_print("It's the Boost card.");
					(void)set_boost_all(p_ptr->boost_all + rand_range(10, 50));
					if (!one_in_(5)) break;
				}
			}
			while (TRUE);
				
		}
		break;
		
		case 10: /* Offensive Trump */
			{
			/*
			 * This spell should become more useful (more
			 * controlled) as the player gains experience levels.
			 * Thus, add 1/5 of the player's level to the die roll.
			 * This eliminates the worst effect later on, while
			 * keeping the results quite random.  It also allows
			 * some potent effects only at high level.
			 */
				int die = randint1(100) + plev / 5;
	
				if (die < 26)
					chg_virtue(V_CHANCE, 1);
	
				if (!get_aim_dir(&dir)) return FALSE;
				if (die > 100)
					msg_print("You feel a surge of power!");
				if (die < 8) (void)clone_monster(dir);
				else if (die < 14) (void)speed_monster(dir);
				else if (die < 26) (void)heal_monster(dir);
				else if (die < 31) (void)poly_monster(dir);
				else if (die < 36)
					(void)fire_bolt_or_beam(beam - 10, GF_MANA, dir,
					                  damroll(3 + ((plev - 1) / 5), 4));
				else if (die < 41) (void)confuse_monster(dir, plev);
				else if (die < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
				else if (die < 51) (void)lite_line(dir);
				else if (die < 56)
					(void)fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
					                  damroll(3 + ((plev - 5) / 4), 8));
				else if (die < 61)
					(void)fire_bolt_or_beam(beam - 10, GF_COLD, dir,
					                  damroll(5 + ((plev - 5) / 4), 8));
				else if (die < 66)
					(void)fire_bolt_or_beam(beam, GF_ACID, dir,
					                  damroll(6 + ((plev - 5) / 4), 8));
				else if (die < 71)
					(void)fire_bolt_or_beam(beam, GF_FIRE, dir,
					                  damroll(8 + ((plev - 5) / 4), 8));
				else if (die < 76) (void)drain_life(dir, 75);
				else if (die < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev, 2);
				else if (die < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
				else if (die < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
				else if (die < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
				else if (die < 101) (void)drain_life(dir, 100 + plev);
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
				else if (die < 110) (void)dispel_monsters(120);
				else /* RARE */
				{
					(void)dispel_monsters(150);
					(void)slow_monsters();
					(void)sleep_monsters();
					(void)hp_player(300);
				}
				break;
			}
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
				(void)fire_beam(GF_AWAY_ALL, dir, plev*2);
			}
			break;
		case 13: /* Teleport Level */
			if (success)
			{
				(void)teleport_player_level();
				(void)set_fatigue(p_ptr->fatigue + 100);
			}
			break;
		case 14: /* Invoke Logrus */
			if (success)
			{
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_CHAOS, dir, damroll(3,plev*2), plev/10);
			}
			break;
		case 15: /* Greater Confusion */
			if (success)
			{
				(void)confuse_monsters(randint1(10*plev));
				(void)set_fatigue(p_ptr->fatigue + 500);
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
				if (one_in_(4)) detect_traps();
				if (one_in_(4)) detect_doors();
				if (one_in_(4)) detect_stairs();
				if (one_in_(4)) detect_treasure();
				if (one_in_(4)) detect_objects_gold();
				if (one_in_(4)) detect_objects_normal();
				if (one_in_(4)) detect_monsters_invis();
				if (one_in_(4)) detect_monsters_normal();
				if (one_in_(5)) map_area();
			}
			break;
		case 18: /* Trump Brand */
			if (success)
			{
				(void)set_fatigue(p_ptr->fatigue + 5000);
				brand_weapon(4);
			}
			break;
		case 19: /* Trump Lore */
			if (success)
			{
				int die = randint1(4);
				if (die == 4)	return identify_fully();
				else if ((die == 3) || (die == 2)) return ident_spell();
				else
				{	errr err;
					err = get_rnd_line("rumors.txt", 0, Rumor);
					msg_format("%s", Rumor);
				}
				(void)set_fatigue(p_ptr->fatigue + 500);
			}
			break;
		case 20: /* Creature Trump */
			{
				bool pet = success; /* (randint1(5) > 2) */
				bool group = (pet ? FALSE : TRUE);
				
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
		case 22: /* Mega Trump */
		{
			int die = randint1(110);
			int cards = randint1((plev-41)/3); // One to three cards.
			
			if (die == 110)
			{
				msg_print("You throw all the cards into the air! ");
				cards = 5;
				die = randint1(110);
			}
			else msg_print("You shuffle the cards....");
			
			if (die < 26)	chg_virtue(V_CHANCE, 1);

			for (i=1; i<=cards; i++)
			{
				if (die < 8)
				{
					msg_print("Oh No! You draw the Clone card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 5); dummy++)
					wild_blast(py, px, 0, GF_OLD_CLONE, 0);
				}
				else if (die < 14)
				{
					msg_print("Oh No! You draw a picture of a blurred monster. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 5); dummy++)
					wild_blast(py, px, randint1(plev), GF_OLD_SPEED, 0);
				}
				else if (die < 26)
				{
					msg_print("Oh No! You draw a picture of a healed monster. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 5); dummy++)
					wild_blast(py, px, randint1(plev), GF_OLD_HEAL, 0);
				}
				else if (die < 31)
				{
					msg_print("You draw a picture of a polymorphed monster. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 5); dummy++)
					wild_blast(py, px, randint1(plev), GF_OLD_POLY, 0);
				}
				else if (die < 36)
				{
					msg_print("You draw the Teleport Storm card. ");
					fire_ball(GF_AWAY_ALL, 0, plev*3, 4);
					teleport_player(randint1(plev*8));
				}
				else if (die < 41)
				{
					msg_print("You draw the Confusion card. ");
					(void)confuse_monsters(plev*3);
				}	
				else if (die < 46)
				{
					msg_print("You draw the Healing card. ");
					cure_wounds(4);
					do_poly_wounds();
				}
				else if (die < 51)
				{
					msg_print("You draw the Bright card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_LITE, 2);
					}
				}
				else if (die < 56)
				{
					msg_print("You draw the Lightning card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_ELEC, 2);
					}
				}
				else if (die < 61)
				{
					msg_print("You draw the Cold card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_COLD, 2);
					}
				}
				else if (die < 66)
				{
					msg_print("You draw the Acid card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_ACID, 2);
					}
				}
				else if (die < 71)
				{
					msg_print("You draw the Fire card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_FIRE, 2);
					}
				}
				else if (die < 76)
				{
					msg_print("You draw the Shadow card. ");
					if (!tgt_pt(&x, &y)) return FALSE;
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(y, x, damroll(3, plev), GF_DARK, 2);
					}
				}
				else if (die < 81)
				{
					msg_print("You draw the Storm card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_ELEC, 3);
					}
				}
				else if (die < 86)
				{
					msg_print("You draw the Volcano card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_LAVA, 3);
					}
				}
				else if (die < 91)
				{
					msg_print("You draw the Blizzard card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_ICE, 3);
					}
				}
				else if (die < 96)
				{
					msg_print("You draw the Earthquake card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_QUAKE, 3);
					}
				}
				else if (die < 101)
				{
					msg_print("You draw the Vampire card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_NEW_DRAIN, 3);
					}
				}
				else if (die < 104)
				{
					msg_print("You draw the Holy Wrath card. ");
					for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
					{
						wild_blast(py, px, damroll(3, plev+beam), GF_HOLY_FIRE, 3);
					}
				}
				else if (die < 106)
				{
					msg_print("You draw the Defence card. ");
					(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(10, 50));
					(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(10, 50));
					(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(10, 50));
					(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(10, 50));
					(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(10, 50));
					(void)set_invisible(p_ptr->tim_nonvis + rand_range(10, 50));
					(void)set_resist_magic(p_ptr->resist_magic + rand_range(10, 50));
					
				}
				else if (die < 108)
				{
					msg_print("You draw the Life card. ");
					(void)set_vitality(p_ptr->vitality + rand_range(10, 50));
					cure_wounds(5);
					(void)restore_level();
					(void)set_stun(0);
					(void)set_cut(0);
					(void)set_poisoned(0);
				}
				else if (die < 110)
				{
					msg_print("You draw the Aura card. ");
					(void)set_sh_fire(p_ptr->tim_sh_fire + rand_range(25, beam));
					(void)set_sh_elec(p_ptr->tim_sh_elec + rand_range(25, beam));
					(void)set_sh_cold(p_ptr->tim_sh_cold + rand_range(25, beam));
					(void)set_sh_acid(p_ptr->tim_sh_acid + rand_range(25, beam));
					(void)set_invuln(p_ptr->invuln + rand_range(7, 14));
				}
				else /* RARE */
				{
					msg_print("You draw the Annihilation card. ");
					wall_breaker();
					(void)deathray_monsters();
					(void)destroy_area(py, px, 15);
				}
				
				die = randint1(110);
			}
			break;
		}
		case 23: /* Trump Dragon */
			{	
				bool pet = success; /* (randint1(10) > 3) */
				bool group = (pet ? FALSE : TRUE);
				
				msg_print("You concentrate on the trump of a dragon...");
				
				if (summon_specific((pet ? -1 : 0), py, px, plev * 2, SUMMON_DRAGON, group, FALSE, pet))
				{
					if (!pet)
					msg_print("An angry dragon appears!");
				}
				else
				{
					no_trump = TRUE;
				}
			(void)set_fatigue(p_ptr->fatigue + 1000);
			}	
			break;
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
		case 26: /* Alter Reality */
		{
			alter_reality();
			break;
		}
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
				if (die < 3) /* Loop */
				{
					cast_chaos_spell(28, TRUE);
				}
			break;
		}
		case 29: /* Breathe Logrus */
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_CHAOS, dir, damroll(2, 10*plev), -3);
			break;
		}
		case 30: /* Chaos Brand */
		{
			(void)set_fatigue(p_ptr->fatigue + 5000);
			brand_weapon(1);
			break;
		}
		case 31: /* Pandemonium */
		{
			for (dummy = 0; dummy <= ((randint1(plev)) / 3); dummy++)
			{
				wild_blast(py, px, damroll(2, plev), GF_CHAOS, 2);
			}
			project_hack(GF_OLD_POLY, beam*3);
			(void)confuse_monsters(damroll(10,beam+plev));
			(void)banish_monsters(damroll(10,beam+plev));
			(void)set_fatigue(p_ptr->fatigue + 1000);
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
		cure_wounds(1);
		break;
	case 11: /* Blink */
		teleport_player(10);
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
	case 15: /* Luck */
		if (!p_ptr->boost_all)
		{
			(void)set_boost_all(p_ptr->boost_all + rand_range(50, 75));
		}
		else
		{
			(void)set_boost_all(p_ptr->boost_all + randint1(plev));
		}	
		break;
	case 16: /* Multi Missile */

		/* Mark all (nearby) monsters */
		for (i = 1; i < m_max; i++)
		{			
			monster_type *m_ptr = &m_list[i];
			
			if (dummy >= (beam + plev) / 8)
				break;

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			/* Location */
			y = m_ptr->fy;
			x = m_ptr->fx;

			c_ptr = area(y, x);

			/* Require line of sight */
			if (!player_has_los_grid(c_ptr)) continue;

			(void)project(0, 0, y, x, damroll(3 + ((plev - 1) / 5), 4), GF_MANA, (PROJECT_STOP | PROJECT_KILL));
			dummy++;
		}
		(void)set_fatigue(p_ptr->fatigue + 100);
		break;
	case 17: /* Resist Cold */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		break;
	case 18: /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		break;
	case 19: /* Teleport */
		teleport_player(plev * 4);
		break;
	case 20: /* Resist Lightning */
		(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		break;
	case 21: /* Resist Acid */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		break;
	case 22: /* Identify */
		(void)set_fatigue(p_ptr->fatigue + 200);
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
		(void)set_sh_fire(p_ptr->tim_sh_fire + rand_range(25, beam));
		break;
	case 31: /* SEER */
		wiz_lite();
		if (!p_ptr->telepathy)
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 50));
		}
		(void)set_fatigue(p_ptr->fatigue + 600);
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
	int px = p_ptr->px;
	int py = p_ptr->py;
	bool pet = (!one_in_(3));

	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_WATER)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Cold Resist */
		(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(plev, plev*beam/11 + 9));
		break;
	case 1: /* Chill */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_COLD, dir, damroll(MIN(2+plev*2/3, 20), 3));
		break;
	case 2: /* Acid Resist */
		(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(plev, plev*beam/11 + 9));
		break;
	case 3: /* Acid bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ACID, dir, damroll(MIN(2+plev*2/3, 20), 3));
		break;
	case 4: /* Ice Shield */
		(void)set_ac1(p_ptr->ac1 + rand_range(30, 50));
		break;
	case 5: /* Frost */
		(void)fire_ball(GF_COLD, 0, damroll(MIN(2+plev*2/3, 20), 3), 2+plev/25);
		break;
	case 6: /* Detect Cretures */
		(void)detect_monsters_normal();
		break;
	case 7: /* Hydroblast */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_WATER, dir, damroll(plev, 2));
		break;
	case 8: /* Resist Fire */
		(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 50));
		break;
	case 9: /* Ice Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_ICE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 10: /* Hydroflow */
		teleport_player(plev);
		break;
	case 11: /* Holy Water */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_DISP_UNDEAD, dir, damroll(4, 10) + 2*plev, 1+plev/25);
		break;
	case 12: /* Reflect Self */
		(void)self_knowledge();
		break;
	case 13: /* Acid Spray */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ACID, dir, damroll(MIN(2+plev*2/3, 20), 3), -(1+plev/25));
		break;
	case 14: /* Healing Water */
		cure_wounds(1);
		(void)set_poisoned(0);
		break;
	case 15: /* Orb of Winter */
		(void)fire_ball(GF_COLD, 0, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6), 2+plev/20);
		(void)set_fatigue(p_ptr->fatigue + 300);
		break;
	case 16: /* Fluid Motion */
		if (!p_ptr->boost_dex)
		{
			(void)set_boost_dex(p_ptr->boost_dex + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_dex(p_ptr->boost_dex + randint1(plev));
		}	
		break;
	case 17: /* Acidic Flow */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_ACID, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 18: /* Whirlpool */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6) + plev, 1+plev/25);
		break;
	case 19: /* Aura of Aqua */
	if (randint1(beam+plev) <= 75)
	{
		char ch;
		int water = 0;
		
		while (TRUE)
		{
			if (!get_com(" Aura of (C)old or (A)cid? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'C' || ch == 'c')
			{
				water = 1;
				break;
			}
			else if (ch == 'A' || ch == 'a')
			{
				break;
			}
		}
		if (water == 0)
		{
			(void)set_sh_cold(p_ptr->tim_sh_cold + rand_range(plev, plev*beam/11));
			break;
		}
		else
		{
			(void)set_sh_acid(p_ptr->tim_sh_acid + rand_range(plev, plev*beam/11));
			break;
		}
	}
	else
	{
		(void)set_sh_cold(p_ptr->tim_sh_cold + rand_range(plev, plev*beam/11));
		(void)set_sh_acid(p_ptr->tim_sh_acid + rand_range(plev, plev*beam/11));
	}
		break;
	case 20: /* Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_WATER, dir, damroll(MAX(MIN(15+(plev-10)*5/3, 50), 2), 6), -(1+plev/25));
		break;
	case 21: /* Acid Rain */
		(void)fire_ball(GF_ACID, 0, damroll(MAX(MIN(15+(plev-10)*5/3, 50), 2), 6) + 2*plev, beam/20);
		break;
	case 22: /* Hydro Brand */
	{
		char ch;
		int water = 0;
		
		while (TRUE)
		{
			if (!get_com(" (R)ustproof, (F)rostbrand or (A)cidbrand? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'R' || ch == 'r')
			{
				water = 1;
				break;
			}
			if (ch == 'F' || ch == 'f')
			{
				water = 2;
				break;
			}
			if (ch == 'A' || ch == 'a')
			{
				break;
			}
		}
		if (water == 1)
		{
			(void)set_fatigue(p_ptr->fatigue + 5000);
			return rustproof();
		}
		else if (water == 2)
		{
			(void)set_fatigue(p_ptr->fatigue + 5000);
			brand_weapon(6);
			break;
		}
		else
		{
			(void)set_fatigue(p_ptr->fatigue + 5000);
			brand_weapon(8);
			break;
		}
	}
	case 23: /* Tidal Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_FORCE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6), 2);
		(void)fire_ball(GF_WATER, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6), -3);
		break;
	case 24: /* Gaurdian of Water */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_WATERE, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry elemental appears!");
		}
		break;
	case 25: /* Freeze */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_COLD, dir, damroll(MAX((plev*plev-1000)/15, 1), 12));
		break;
	case 26: /* Restore Soul */
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)set_fatigue(p_ptr->fatigue + 4000);
		break;
	case 27: /* Razor Icicle */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_ICE, dir, damroll((2*beam), 9));
		break;
	case 28: /* Frozen Armour */
		(void)set_shield(p_ptr->shield + rand_range(50, 100));
		break;
	case 29: /* Ice Storm */
		(void)rain_effect(GF_ICE, damroll(MAX(MIN(15+(plev-10)*5/3, 50), 2), 6), 20*beam, 2, 10);
		break;
	case 30: /* Ice Vapour */
		(void)slow_monsters();
		(void)detect_all();
		break;
	case 31: /* Freezing Sphere */
		(void)fire_ball(GF_ICE, 0, damroll(MAX((plev*plev-1000)/15, 1), 10) + plev, 5);
		(void)set_fatigue(p_ptr->fatigue + 1000);
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
	bool pet = (!one_in_(3));
	
	if ((p_ptr->pclass == CLASS_HIGH_MAGE) || (p_ptr->pclass == CLASS_MAGE_EARTH)) beam = plev * 3 / 2;
	else beam = plev * 2 / 3;

	switch (spell)
	{
	case 0: /* Shield */
		(void)set_ac1(p_ptr->ac1 + rand_range(5, plev + 5));
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
		(void)fire_bolt(GF_MISSILE, dir, damroll(MIN(1+plev*2/3, 20), 3));
		break;
	case 4: /* Sense Invisible */
		(void)detect_monsters_invis();
		break;
	case 5: /* Stun Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_STUN, dir, beam+15);
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
		(void)fire_bolt(GF_GRAVITY, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 10: /* Armour */
		(void)set_ac2(p_ptr->ac2 + rand_range(plev, plev*2));
		break;
	case 11: /* Multi-Stun */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_STUN, dir, 3*beam, plev/11);
		break;
	case 12: /* Detect Walls */
		(void)map_area();
		break;
	case 13: /* Gravity Beam */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_GRAVITY, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 14: /* Stunwave */
		(void)fire_ball(GF_STUN, 0, 2*beam + 100, 1+plev/10);
		break;
	case 15: /* Greater Detection */
		(void)detect_all();
		break;
	case 16: /* Remove Traps */
		(void)destroy_traps(10);
		break;
	case 17: /* Tremor */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_QUAKE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6) + plev, -1);
		break;
	case 18: /* Terraform */
	{
		char ch;
		int choice = 0;

		while (TRUE) // ?
		{
			if (!get_com(" Do you wish for (T)unnel, (B)ridge or (S)tonewall? ", &ch))
			{
				return FALSE;
			}
			if (ch == 'T' || ch == 't')
			{
				choice = 1;
				break;
			}
			if (ch == 'B' || ch == 'b')
			{
				choice = 2;
				break;
			}
			if (ch == 'S' || ch == 's')
			{
				break;
			}
		}
		switch (choice)
		{
			case 0:
			{
				return wall_stone();
				(void)wall_to_mud(1);
			}
			case 1:
			{
				if (!get_aim_dir(&dir)) return FALSE;
				for (i=1; i<=5; i++)	(void)wall_to_mud(dir);
				break;
			}
			case 2: return bridge();
		}
		(void)set_fatigue(p_ptr->fatigue + 600);
	}
		break;		
	case 19: /* Earthquake */
		(void)earthquake(py, px, plev/3 - 6, 0);
		(void)set_fatigue(p_ptr->fatigue + 100);
		break;
	case 20: /* Strength */
		if (!p_ptr->boost_str)
		{
			(void)set_boost_str(p_ptr->boost_str + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_str(p_ptr->boost_str + randint1(plev));
		}
		break;
	case 21: /* Toughness */
		if (!p_ptr->boost_con)
		{
			(void)set_boost_con(p_ptr->boost_con + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_con(p_ptr->boost_con + randint1(plev));
		}
		break;
	case 22: /* Quake Brand */
		(void)set_fatigue(p_ptr->fatigue + 5000);
		(void)brand_weapon(9);
		break;
	case 23: /* Greater Quake */
		if (!tgt_pt(&x, &y)) return FALSE;
	
		p_ptr->energy -= 60 - plev;
	
		/* paranoia */
		if (!in_bounds2(y, x)) return FALSE;
	
		c_ptr = area(y, x);
		if (distance(y, x, py, px) > plev + 2)
		{
			msg_print("You over reach!");
			p_ptr->energy -= 100;
			earthquake(py, px, plev/10, 0);
		}
		else earthquake(y, x, plev/3 - 6, 0);
		(void)set_fatigue(p_ptr->fatigue + 1000);
		break;
	case 24: /* Gem Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_SHARDS, dir, damroll(MAX((plev*plev-1000)/15, 1), 10));
		break;
	case 25: /* Stone Skin */
		msg_print("Your skin turns to stone! ");
		(void)set_shield(p_ptr->shield + rand_range(25, 50));
		break;
	case 26: /* Enchant Rock */
		if (summon_specific((pet ? -1 : 0), py, px, plev, SUMMON_EARTHE, TRUE, FALSE, pet))
		{
			if (!pet)
			msg_print("An angry elemental appears!");
		}
		break;
	case 27: /* Improve Armour */
		(void)set_fatigue(p_ptr->fatigue + 1000);
		return enchant_spell(0, 0, rand_range(2, 5));
	case 28: /* Crush */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_GRAVITY, dir, damroll(MAX((plev*plev-1000)/15, 1), 10), 1+plev/25);
		break;
	case 29: /* Restore Body */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_DEX);
		(void)set_fatigue(p_ptr->fatigue + 3000);
		break;
	case 30: /* Rain of Shards */
		(void)rain_effect(GF_SHARDS, damroll(MAX((plev*plev-1000)/15, 1), 10), 1500, 2, 12);
		break;
	case 31: /* Crystal Skin */
		(void)set_invuln(p_ptr->invuln + rand_range(10, 20));
		(void)set_fatigue(p_ptr->fatigue + 1000);
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
	bool pet = (!one_in_(3));

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
		(void)fire_bolt_or_beam(beam, GF_MANA, dir, damroll(MIN(2+plev*2/3, 20), 3));
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
	case 11: /* Resist Magic */
		(void)set_resist_magic(p_ptr->resist_magic + rand_range(plev, 50+beam));
		break;
	case 12: /* Slowing Orb */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_OLD_SLOW, dir, 3*beam, 1+plev/20);
		break;
	case 13: /* Magic Report */
		(void)report_magics();
		break;
	case 14: /* Cure Medium Wounds */
		cure_wounds(2);
		(void)set_cut((p_ptr->cut / 2) - 50);
		break;
	case 15: /* Mantle */
		(void)set_shield(p_ptr->shield + rand_range(25, 50));
		break;
	case 16: /* Disintegrate */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt_or_beam(beam, GF_DISINTEGRATE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6));
		break;
	case 17: /* Protection From Evil*/
		(void)set_protevil(p_ptr->protevil + randint1(25) + 3 * plev);
		break;
	case 18: /* Invisiblity */
		(void)set_invisible(p_ptr->tim_nonvis + rand_range(25, 50));
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
		(void)fire_ball(GF_STASIS, dir, 3*plev, 1);
		break;
	case 22: /* Disintergration Wave */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_DISINTEGRATE, dir, damroll(MAX(MIN(5+(plev-10)*5/3, 50), 2), 6), -(plev/10)+2);
		break;
	case 23: /* Recharge */
		(void)set_fatigue(p_ptr->fatigue + 200);
		return recharge(plev * 4);
	case 24: /* Aura of Energy */
		(void)set_sh_elec(p_ptr->tim_sh_elec + rand_range(plev, plev*beam/11));
		break;
	case 25: /* Magic Meteorite */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_METEOR, dir, damroll(MAX((plev*plev-1000)/15, 1), 10), 1);
		break;
	case 26: /* Word of Recall */
		word_of_recall();
		break;
	case 27: /* Greater Identify */
		(void)set_fatigue(p_ptr->fatigue + 4000);
		return identify_fully();
	case 28:/* Meteorite Shower */
		(void)rain_effect(GF_METEOR, damroll(MAX((plev*plev-1000)/15, 1), 10), 2000, 1, 15);
		break;
	case 29: /* Restoration */
		(void)do_res_stat(A_STR);
		(void)do_res_stat(A_INT);
		(void)do_res_stat(A_WIS);
		(void)do_res_stat(A_DEX);
		(void)do_res_stat(A_CON);
		(void)do_res_stat(A_CHR);
		(void)restore_level();
		(void)set_fatigue(p_ptr->fatigue + 5000);
		break;
	case 30: /* Cosmic Wrath */
		(void)dispel_monsters(plev * 5);
		(void)set_fatigue(p_ptr->fatigue + 500);
		break;
	case 31: /* Star Shield */
		dummy = rand_range(plev, beam*plev/10);
		(void)set_shield(p_ptr->shield + dummy);
		(void)set_sh_elec(p_ptr->tim_sh_elec + dummy);
		(void)set_sh_fire(p_ptr->tim_sh_fire + dummy);
		(void)set_sh_cold(p_ptr->tim_sh_cold + dummy);
		(void)set_sh_acid(p_ptr->tim_sh_acid + dummy);
		(void)set_boost_all(p_ptr->boost_all + dummy);
		(void)set_fatigue(p_ptr->fatigue + 1000);
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

	/* Require spell casting ability */
	if (!p_ptr->realm1)
	{
		msg_print("You have no spell casting ability!");
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

